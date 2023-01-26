(in-package :postgresql)

(defvar *client* nil)

(define-condition missing-password (simple-error)
  ()
  (:default-initargs
   :format-control "Missing password for authentication."))

(define-condition unsupported-authentication-scheme (error)
  ((name
    :type string
    :initarg :name
    :reader unsupported-authentication-scheme-name))
  (:report
   (lambda (condition stream)
     (with-slots (name) condition
       (format stream "Unsupported PostgreSQL authentication scheme ~A."
               name)))))

(define-condition unexpected-message (error)
  ((message
    :initarg :message
    :reader unexpected-message-message))
  (:report
   (lambda (condition stream)
     (with-slots (message) condition
       (format stream "Unexpected PostgreSQL message:~%~%~S~%" message)))))

(defclass client ()
  ((stream
    :type (or system:network-stream null)
    :initarg :stream
    :reader client-stream)
   (backend-process-id
    :type (or integer null)
    :initform nil
    :accessor client-backend-process-id)
   (backend-secret-key
    :type (or integer null)
    :initform nil
    :accessor client-backend-secret-key)))

(defmethod print-object ((client client) stream)
  (print-unreadable-object (client stream :type t)
    (let ((address (system:network-stream-address (client-stream client))))
      (system:format-socket-address address stream))))

(defun make-client (&key (host "localhost") (port 5432)
                         user password
                         database
                         application-name)
  (declare (type system:host host)
           (type system:port-number port)
           (type (or string null) user password database application-name))
  ;; It may seems strange that the host is not a mandatory parameter, but in
  ;; the future we would like to support UNIX sockets.
  (let ((stream (system:make-tcp-client host port))
        (client nil))
    (core:abort-protect
        (let ((parameters nil))
          (when user
            (push (cons "user" user) parameters))
          (when database
            (push (cons "database" database) parameters))
          (when application-name
            (push (cons "application_name" application-name) parameters))
          (write-startup-message 3 0 parameters stream)
          (setf client (make-instance 'client :stream stream))
          (authenticate user password client)
          (finish-startup client)
          client)
      (if client
          (close-client client)
          (close stream)))))

(defun close-client (client)
  (declare (type client client))
  (with-slots (stream) client
    (when stream
      (ignore-errors
       (write-termination-message stream)
       (close stream))
      (setf stream nil)
      t)))

(defmacro with-client ((&rest options) &body body)
  `(let ((*client* (make-client ,@options)))
     (unwind-protect
          (progn
            ,@body)
       (close-client *client*))))

(defmacro read-message-case ((message client) &rest forms)
  (let ((fields (gensym "FIELDS-")))
    `(let* ((,message (read-message (client-stream ,client)))
            (,fields (cdr ,message)))
       (declare (ignorable ,fields))
       (case (car ,message)
         (:error-response
          (backend-error (car ,fields)))
         (:notice-response
          nil)
         ,@(mapcar
            (lambda (form)
              (destructuring-bind (type args &body body) form
                `(,type
                  ,(if (null args)
                       `(progn ,@body)
                       `(let* (,@(mapcar (lambda (arg)
                                           `(,arg (pop ,fields)))
                                         args))
                          ,@body)))))
            forms)
         (t
          (error 'unexpected-message :message ,message))))))

(defun authenticate (user password client)
  (declare (type (or string null) user password)
           (type client client))
  (with-slots (stream) client
    (loop
      (read-message-case (message client)
        (:authentication-ok ()
          (return))
        (:authentication-cleartext-password ()
          (unless password
            (error 'missing-password))
          (write-password-message password stream))
        (:authentication-md5-password (salt)
          (unless password
            (error 'missing-password))
          (let ((hash (compute-password-md5-hash user password salt)))
            (write-password-message hash stream)))
        (:authentication-gss ()
          (error 'unsupported-authentication-scheme :name "GSS"))
        (:authentication-kerberos-v5 ()
          (error 'unsupported-authentication-scheme :name "Kerberos V5"))
        (:authentication-scm-credential ()
          (error 'unsupported-authentication-scheme :name "SCM"))
        (:authentication-sspi ()
          (error 'unsupported-authentication-scheme :name "SSPI"))
        (:authentication-sasl (mechanisms)
          (unless (member "SCRAM-SHA-256" mechanisms :test #'string=)
            (error 'unsupported-authentication-scheme
                   :name (format nil "SASL (~{~A~^, ~})" mechanisms)))
          (authenticate/scram-sha-256 user password client))))))

(defun authenticate/scram-sha-256 (user password client)
  (declare (type (or string null) user password)
           (type client client)
           (ignore user))
  ;; See https://www.postgresql.org/docs/current/sasl-authentication.html for
  ;; more information. We only support the SCRAM-SHA-256 mechanism.
  ;;
  ;; It might seem strange to always loop even though we are waiting for one
  ;; specific message at each step. However we need to be able to handle
  ;; messages such as NoticeResponse which can be sent at any moment.
  (unless password
    (error 'missing-password))
  (with-slots (stream) client
    ;; Send a SASLInitialResponse message containing a client-first-message
    ;; SCRAM payload.
    (let* ((nonce (generate-scram-nonce))
           (client-first-message (scram-client-first-message nil nonce)))
      (write-sasl-initial-response-message "SCRAM-SHA-256"
                                           client-first-message stream)
      ;; Wait for a AuthenticationSASLContinue message containing a
      ;; server-first-message SCRAM payload.
      (loop
        (read-message-case (message client)
          (:authentication-sasl-continue (server-first-message)
            ;; Compute the proof and send a SASLResponse message containing a
            ;; client-final-message SCRAM payload.
            (multiple-value-bind
                  (client-final-message salted-password auth-message)
                (scram-client-final-message client-first-message
                                            server-first-message
                                            password nonce)
              (write-sasl-response-message client-final-message stream)
              ;; Wait for an AuthenticationSASLFinal message containing a
              ;; server-final-message SCRAM payload.
              (loop
                (read-message-case (message client)
                  (:authentication-sasl-final (server-final-message)
                    ;; Check the server signature
                    (check-scram-server-final-message
                     server-final-message salted-password auth-message)
                    (return-from authenticate/scram-sha-256)))))))))))

(defun finish-startup (client)
  (declare (type client client))
  ;; See
  ;; https://www.postgresql.org/docs/current/protocol-flow.html#id-1.10.6.7.3
  ;; for more information about the startup process.
  (with-slots (stream) client
    (loop
      (read-message-case (message client)
        (:parameter-status (name value)
          (declare (ignore name value))
          nil)
        (:backend-key-data (process-id secret-key)
          (setf (client-backend-process-id client) process-id
                (client-backend-secret-key client) secret-key))
        (:ready-for-query ()
          (return))))))

(defun query/simple (query &key (client *client*))
  "Execute QUERY as a simple query and returns two values. The first one is a
list of command results. Each result is a list containing a vector of column
names, a list of row vectors and the number of rows affected by the query.
Columns are always returned as strings. The second value is the transaction
status after execution of the query."
  (declare (type string query)
           (type client client))
  (multiple-value-bind (results transaction-status)
      (send-simple-query query :client client)
    (labels ((column-name (column)
               (cdr (assoc :name column)))
             (decode-row (row)
               (dotimes (i (length row))
                 (setf (aref row i) (text:decode-string (aref row i)))))
             (make-result (result)
               (destructuring-bind (columns rows command-tag)
                   result
                 (let* ((nb-columns (length columns))
                        (column-names
                          (map-into
                           (make-array nb-columns :element-type 'string
                                                  :initial-element "")
                           #'column-name columns))
                        (nb-affected-rows (cdr command-tag)))
                   (mapc #'decode-row rows)
                   (list rows column-names nb-affected-rows)))))
      (values (mapcar #'make-result results) transaction-status))))

(defun query (query &optional parameters &key (client *client*))
  "Execute QUERY with PARAMETERS as an extended query and returns four values: a
vector of column names, a list of row vectors, the number of rows affected by
the query and the transaction status after execution of the query. always
returned as strings."
  (declare (type string query)
           (type list parameters)
           (type client client)
           #+sbcl (sb-ext:muffle-conditions style-warning))
  (multiple-value-bind (columns rows command-tag transaction-status)
      (send-extended-query query parameters :client client)
    (labels ((column-name (column)
               (cdr (assoc :name column)))
             (decode-row (row)
               (dotimes (i (length row))
                 (let* ((octets (aref row i))
                        (column (aref columns i))
                        (oid (cdr (assoc :type-oid column))))
                   (setf (aref row i) (decode-value octets oid))))))
      (let ((column-names (map 'vector #'column-name columns))
            (nb-affected-rows (cdr command-tag)))
        (mapc #'decode-row rows)
        (values rows column-names nb-affected-rows transaction-status)))))

(defun send-simple-query (query &key (client *client*))
  (declare (type string query)
           (type client client))
  (with-slots (stream) client
    (write-query-message query stream)
    (do ((columns nil)
         (results nil)
         (rows nil)
         (transaction-status nil))
        (transaction-status
         (values (nreverse results) transaction-status))
      (read-message-case (message client)
        (:empty-query-response ()
          nil)
        (:row-description (description)
          (setf columns description))
        (:data-row (row)
          (push row rows))
        (:command-complete (tag)
          (push (list columns (nreverse rows) tag) results)
          (setf columns nil
                rows nil))
        (:ready-for-query (status)
          (setf transaction-status status))))))

(defun send-extended-query (query parameters &key (client *client*))
  (declare (type string query)
           (type list parameters)
           (type client client))
  (let ((encoded-parameters (mapcar 'encode-value parameters))
        (parameter-oids nil)
        (parameter-values nil))
    (dolist (parameter encoded-parameters)
      (push (car parameter) parameter-oids)
      (push (cdr parameter) parameter-values))
    (with-slots (stream) client
      (write-parse-message nil query (nreverse parameter-oids) stream)
      (write-bind-message nil nil (list :binary) (nreverse parameter-values)
                          (list :binary) stream)
      (write-describe-message :portal nil stream)
      (write-execute-message nil nil stream)
      (write-sync-message stream)
      (do ((transaction-status nil)
           (columns nil)
           (rows nil)
           (command-tag nil))
          (transaction-status
           (values columns (nreverse rows) command-tag transaction-status))
        ;; TODO :NO-DATA
        (read-message-case (message client)
          (:empty-query-response ()
            nil)
          (:parse-complete ()
            nil)
          (:bind-complete ()
            nil)
          (:row-description (description)
            (setf columns description))
          (:data-row (row)
            (push row rows))
          (:command-complete (tag)
            (setf command-tag tag))
          (:ready-for-query (status)
            (setf transaction-status status)))))))
