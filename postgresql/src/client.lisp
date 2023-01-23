(in-package :postgresql)

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
    :reader client-stream)))

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
  (let ((stream (system:make-tcp-client host port)))
    (core:abort-protect
        (let ((parameters nil))
          (when user
            (push (cons "user" user) parameters))
          (when database
            (push (cons "database" database) parameters))
          (when application-name
            (push (cons "application_name" application-name) parameters))
          (write-startup-message 3 0 parameters stream)
          (authenticate user password stream)
          (make-instance 'client :stream stream))
      (close stream))))

(defun close-client (client)
  (declare (type client client))
  (with-slots (stream) client
    (when stream
      (ignore-errors
       (write-termination-message stream)
       (close stream))
      (setf stream nil)
      t)))

(defmacro with-client ((client &rest options) &body body)
  `(let ((,client (make-client ,@options)))
     (unwind-protect
          (progn
            ,@body)
       (close-client client))))

(defmacro read-message-case ((message stream) &rest forms)
  `(let ((,message (read-message ,stream)))
     (case (car ,message)
       (:error-response
        (backend-error (cdr ,message)))
       (:notice-response
        nil)
       ,@forms
       (t
        (error 'unexpected-message :message ,message)))))

(defun authenticate (user password stream)
  (declare (type (or string null) user password)
           (type stream stream))
  (do ()
      (nil)
    (read-message-case (message stream)
      (:authentication-ok
        (return))
      (:authentication-cleartext-password
        (unless password
          (error 'missing-password))
        (write-password-message password stream))
      (:authentication-md5-password
        (unless password
          (error 'missing-password))
        (let* ((salt (cadr message))
               (hash (compute-password-md5-hash user password salt)))
          (write-password-message hash stream)))
      (:authentication-gss
        (error 'unsupported-authentication-scheme :name "GSS"))
      (:authentication-kerberos-v5
        (error 'unsupported-authentication-scheme :name "Kerberos V5"))
      (:authentication-scm-credential
        (error 'unsupported-authentication-scheme :name "SCM"))
      (:authentication-sspi
        (error 'unsupported-authentication-scheme :name "SSPI"))
      (:authentication-sasl-credential
        (error 'unsupported-authentication-scheme :name "SASL")))))
