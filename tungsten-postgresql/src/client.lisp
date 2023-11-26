(in-package :postgresql)

(defvar *client* nil)

(defparameter *connection-acquisition-timeout* 10.0)

(define-condition missing-password (simple-error)
  ()
  (:default-initargs
   :format-control "missing password for authentication"))

(define-condition tls-not-supported (error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "PostgreSQL server does not support TLS connections"))))

(define-condition unsupported-authentication-scheme (error)
  ((name
    :type string
    :initarg :name
    :reader unsupported-authentication-scheme-name))
  (:report
   (lambda (condition stream)
     (format stream "unsupported PostgreSQL authentication scheme ~A"
             (unsupported-authentication-scheme-name condition)))))

(define-condition unexpected-message (error)
  ((message
    :initarg :message
    :reader unexpected-message-message))
  (:report
   (lambda (condition stream)
     (format stream "Unexpected PostgreSQL message: ~S"
             (unexpected-message-message condition)))))

(define-condition no-available-connection (error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "no available PostgreSQL connection"))))

(defclass client ()
  ((host
    :type system:host
    :initarg :host
    :reader client-host)
   (port
    :type system:port-number
    :initarg :port
    :reader client-port)
   (user
    :type (or string null)
    :initarg :user
    :initform nil
    :reader client-user)
   (password
    :type (or string null)
    :initarg :password
    :initform nil
    :reader client-password)
   (tls
    :type boolean
    :initarg :tls
    :initform nil
    :reader client-tls)
   (tls-ca-certificate-paths
    :type list
    :initarg :tls-ca-certificate-paths
    :initform nil
    :reader client-tls-ca-certificate-paths)
   (tls-certificate-path
    :type (or pathname string null)
    :initarg :tls-certificate-path
    :initform nil
    :reader client-tls-certificate-path)
   (tls-private-key-path
    :type (or pathname string null)
    :initarg :tls-private-key-path
    :initform nil
    :reader client-tls-private-key-path)
   (tls-server-name
    :type (or string null)
    :initarg :tls-server-name
    :initform nil
    :reader client-tls-server-name)
   (database
    :type (or string null)
    :initarg :database
    :initform nil
    :reader client-database)
   (application-name
    :type (or string null)
    :initarg :application-name
    :reader client-application-name)
   (max-connections
    :type (integer 1)
    :initarg :max-connections
    :reader client-max-connections)
   (mutex
    :type system:mutex
    :initform (system:make-mutex :name "postgresql-client"))
   (condition-variable
    :type system:condition-variable
    :initform (system:make-condition-variable :name "postgresql-client"))
   (used-connections
    :type list
    :initform nil)
   (idle-connections
    :type list
    :initform nil)))

(defclass connection ()
  ((stream
    :type (or system:network-stream null)
    :initarg :stream
    :reader connection-stream)
   (backend-process-id
    :type (or integer null)
    :initform nil
    :accessor connection-backend-process-id)
   (backend-secret-key
    :type (or integer null)
    :initform nil
    :accessor connection-backend-secret-key)
   (codecs
    :type hash-table
    :initform (make-default-codec-table)
    :reader connection-codecs)))

(defmethod print-object ((connection connection) stream)
  (print-unreadable-object (connection stream :type t)
    (let ((address
            (system:network-stream-address (connection-stream connection))))
      (system:format-socket-address address stream))))

(defmethod print-object ((client client) stream)
  (with-slots (host port max-connections
               mutex used-connections idle-connections)
      client
    (system:with-mutex (mutex)
      (print-unreadable-object (client stream :type t)
        (system:format-host-and-port host port stream)
        (format stream " ~D/~D"
                (+ (length used-connections) (length idle-connections))
                max-connections)))))

(defun make-client (&key (host "localhost") (port 5432)
                         user password
                         database application-name
                         (max-connections 10)
                         tls
                         tls-ca-certificate-paths
                         tls-certificate-path tls-private-key-path
                         tls-server-name)
  (declare (type system:host host)
           (type system:port-number port)
           (type (or string null) user password database application-name)
           (type (integer 1) max-connections)
           (type boolean tls)
           (type list tls-ca-certificate-paths)
           (type (or pathname string null)
                 tls-certificate-path tls-private-key-path)
           (type (or string null) tls-server-name))
  ;; It may seems strange that the host is not a mandatory parameter, but in
  ;; the future we would like to support UNIX sockets.
  (let ((client
          (make-instance 'client
                         :host host :port port
                         :user user :password password
                         :database database
                         :application-name application-name
                         :max-connections max-connections
                         :tls tls
                         :tls-ca-certificate-paths tls-ca-certificate-paths
                         :tls-certificate-path tls-certificate-path
                         :tls-private-key-path tls-private-key-path
                         :tls-server-name tls-server-name)))
    (push (make-client-connection client)
          (slot-value client 'idle-connections))
    client))

(defun make-client-connection (client)
  (declare (type client client))
  (with-slots (host port user password database application-name
               tls tls-ca-certificate-paths
               tls-certificate-path tls-private-key-path tls-server-name)
      client
    (make-connection :host (client-host client)
                     :port (client-port client)
                     :user (client-user client)
                     :password (client-password client)
                     :database (client-database client)
                     :application-name (client-application-name client)
                     :tls tls
                     :tls-ca-certificate-paths tls-ca-certificate-paths
                     :tls-certificate-path tls-certificate-path
                     :tls-private-key-path tls-private-key-path
                     :tls-server-name tls-server-name)))

(defun close-client-connections (client)
  (declare (type client client))
  (with-slots (mutex used-connections idle-connections) client
    (system:with-mutex (mutex)
      (mapc 'close-connection used-connections)
      (setf used-connections nil)
      (mapc 'close-connection idle-connections)
      (setf idle-connections nil))
    t))

(defmacro with-client ((&rest options) &body body)
  `(let ((*client* (make-client ,@options)))
     (unwind-protect
          (progn
            ,@body)
       (close-client-connections *client*))))

(defun acquire-connection (&key (client *client*)
                                (timeout *connection-acquisition-timeout*))
  (declare (type client client)
           (type (or real null) timeout))
  (with-slots (max-connections
               mutex condition-variable used-connections idle-connections)
      client
    (system:with-mutex (mutex)
      (let ((connection (pop idle-connections)))
        (cond
          (connection
           (push connection used-connections)
           connection)
          ((< (length used-connections) max-connections)
           (let ((connection (make-client-connection client)))
             (push connection used-connections)
             connection))
          (t
           (do ()
               ((and timeout (< timeout 0.0))
                (error 'no-available-connection))
             (let ((start (time:current-timestamp)))
               (system:wait-condition-variable condition-variable mutex
                                               :timeout timeout)
               (when idle-connections
                 (return (pop idle-connections)))
               (when timeout
                 (decf timeout (time:time-since start)))))))))))

(defun release-connection (connection &key (client *client*))
  (declare (type connection connection)
           (type client client))
  (with-slots (mutex condition-variable used-connections idle-connections)
      client
    (system:with-mutex (mutex)
      (setf used-connections (delete connection used-connections))
      (push connection idle-connections)
      (system:signal-condition-variable condition-variable)
      nil)))

(defmacro with-connection ((connection &key (client *client*) timeout)
                           &body body)
  (let ((client-var (gensym "CLIENT-")))
    `(let* ((,client-var ,client)
            (,connection
              (acquire-connection
               :client ,client-var
               :timeout (or ,timeout *connection-acquisition-timeout*))))
       (unwind-protect
            (progn
              ,@body)
         (release-connection ,connection :client ,client-var)))))

(defun make-connection (&key (host "localhost") (port 5432)
                             user password
                             database
                             application-name
                             tls
                             tls-ca-certificate-paths
                             tls-certificate-path tls-private-key-path
                             tls-server-name)
  (declare (type system:host host)
           (type system:port-number port)
           (type (or string null) user password database application-name)
           (type boolean tls)
           (type list tls-ca-certificate-paths)
           (type (or pathname string null)
                 tls-certificate-path tls-private-key-path)
           (type (or string null) tls-server-name))
  (let ((stream (system:make-tcp-client host port))
        (connection nil))
    (core:abort-protect
        (progn
          (when tls
            (write-ssl-request-message stream)
            (let ((response (read-byte stream)))
              (case response
                (#.(char-code #\S)
                 (setf stream (openssl:init-tls-client
                               stream
                               :ca-certificate-paths tls-ca-certificate-paths
                               :certificate-path tls-certificate-path
                               :private-key-path tls-private-key-path
                               :server-name tls-server-name)))
                (#.(char-code #\N)
                 (error 'tls-not-supported))
                (t
                 (protocol-error
                  "Invalid byte ~S received after SSL request message."
                  response)))))
          (let ((parameters nil))
            (when user
              (push (cons "user" user) parameters))
            (when database
              (push (cons "database" database) parameters))
            (when application-name
              (push (cons "application_name" application-name) parameters))
            (write-startup-message 3 0 parameters stream))
          (setf connection (make-instance 'connection :stream stream))
          (authenticate user password connection)
          (finish-startup connection)
          connection)
      (if connection
          (close-connection connection)
          (close stream)))))

(defun close-connection (connection)
  (declare (type connection connection))
  (with-slots (stream) connection
    (when stream
      (ignore-errors
       (write-termination-message stream)
       (close stream))
      (setf stream nil)
      t)))

(defmacro read-message-case ((message connection) &rest forms)
  (let ((fields (gensym "FIELDS-")))
    `(let* ((,message (read-message (connection-stream ,connection)))
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

(defun authenticate (user password connection)
  (declare (type (or string null) user password)
           (type connection connection))
  (with-slots (stream) connection
    (loop
      (read-message-case (message connection)
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
          (authenticate/scram-sha-256 user password connection))))))

(defun authenticate/scram-sha-256 (user password connection)
  (declare (type (or string null) user password)
           (type connection connection)
           (ignore user))
  ;; See https://www.postgresql.org/docs/current/sasl-authentication.html for
  ;; more information. We only support the SCRAM-SHA-256 mechanism.
  ;;
  ;; It might seem strange to always loop even though we are waiting for one
  ;; specific message at each step. However we need to be able to handle
  ;; messages such as NoticeResponse which can be sent at any moment.
  (unless password
    (error 'missing-password))
  (with-slots (stream) connection
    ;; Send a SASLInitialResponse message containing a client-first-message
    ;; SCRAM payload.
    (let* ((nonce (generate-scram-nonce))
           (client-first-message (scram-client-first-message nil nonce)))
      (write-sasl-initial-response-message "SCRAM-SHA-256"
                                           client-first-message stream)
      ;; Wait for a AuthenticationSASLContinue message containing a
      ;; server-first-message SCRAM payload.
      (loop
        (read-message-case (message connection)
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
                (read-message-case (message connection)
                  (:authentication-sasl-final (server-final-message)
                    ;; Check the server signature
                    (check-scram-server-final-message
                     server-final-message salted-password auth-message)
                    (return-from authenticate/scram-sha-256)))))))))))

(defun finish-startup (connection)
  (declare (type connection connection))
  ;; See
  ;; https://www.postgresql.org/docs/current/protocol-flow.html#id-1.10.6.7.3
  ;; for more information about the startup process.
  (with-slots (stream) connection
    (loop
      (read-message-case (message connection)
        (:parameter-status (name value)
          (declare (ignore name value))
          nil)
        (:backend-key-data (process-id secret-key)
          (setf (connection-backend-process-id connection) process-id
                (connection-backend-secret-key connection) secret-key))
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
  (with-connection (connection :client client)
    (multiple-value-bind (results transaction-status)
        (send-simple-query query connection)
      (when results
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
          (values (mapcar #'make-result results) transaction-status))))))

(defun query (query &optional parameters &key (client *client*))
  "Execute QUERY with PARAMETERS as an extended query and returns four values: a
vector of column names, a list of row vectors, the number of rows affected by
the query and the transaction status after execution of the query. always
returned as strings."
  (declare (type string query)
           (type list parameters)
           (type client client)
           #+sbcl (sb-ext:muffle-conditions style-warning))
  (with-connection (connection :client client)
    (multiple-value-bind (columns rows command-tag transaction-status)
        (send-extended-query query parameters connection)
      (when columns
        (labels ((column-name (column)
                   (cdr (assoc :name column)))
                 (decode-row (row)
                   (dotimes (i (length row))
                     (let* ((octets (aref row i))
                            (column (aref columns i))
                            (oid (cdr (assoc :type-oid column))))
                       (setf (aref row i)
                             (decode-value octets oid
                                           (connection-codecs connection)))))))
          (let ((column-names (map 'vector #'column-name columns))
                (nb-affected-rows (cdr command-tag)))
            (mapc #'decode-row rows)
            (values rows column-names nb-affected-rows transaction-status)))))))

(defun send-simple-query (query connection)
  (declare (type string query)
           (type connection connection))
  (with-slots (stream) connection
    (write-query-message query stream)
    (do ((columns nil)
         (results nil)
         (rows nil)
         (transaction-status nil))
        (transaction-status
         (values (nreverse results) transaction-status))
      (read-message-case (message connection)
        (:no-data ()
          nil)
        (:row-description (description)
          (setf columns description))
        (:data-row (row)
          (push row rows))
        (:command-complete (tag)
          (push (list columns (nreverse rows) tag) results)
          (setf columns nil
                rows nil))
        (:empty-query-response ()
          (return-from send-simple-query nil))
        (:ready-for-query (status)
          (setf transaction-status status))))))

(defun send-extended-query (query parameters connection)
  (declare (type string query)
           (type list parameters)
           (type connection connection))
  (let ((encoded-parameters
          (mapcar (lambda (parameter)
                    (encode-value parameter (connection-codecs connection)))
                  parameters))
        (parameter-oids nil)
        (parameter-values nil))
    (dolist (parameter encoded-parameters)
      (push (car parameter) parameter-oids)
      (push (cdr parameter) parameter-values))
    (with-slots (stream) connection
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
        (read-message-case (message connection)
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
          (:no-data ()
            nil)
          (:empty-query-response ()
            (return-from send-extended-query nil))
          (:ready-for-query (status)
            (setf transaction-status status)))))))
