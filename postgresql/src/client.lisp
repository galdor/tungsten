(in-package :postgresql)

(define-condition authentication-error (simple-error)
  ())

(defun authentication-error (format &rest arguments)
  (error 'authentication-error :format-control format
                               :format-arguments arguments))

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

(defun authenticate (user password stream)
  (declare (type (or string null) user password)
           (type stream stream))
  (do ()
      (nil)
    (let ((message (read-message stream)))
      (case (car message)
        (:error-response
         (backend-error (cdr message)))
        (:notice-response
         nil)
        (:authentication-ok
         (return))
        (:authentication-cleartext-password
         (unless password
           (authentication-error
            "Missing password for clear-text authentication"))
         (write-password-message password stream))
        (:authentication-md5-password
         (unless password
           (authentication-error "Missing password for MD5 authentication."))
         (let* ((salt (cadr message))
                (hash (compute-password-md5-hash user password salt)))
           (write-password-message hash stream)))
        (:authentication-gss
         (authentication-error "Unsupported GSS authentication scheme."))
        (:authentication-kerberos-v5
         (authentication-error
          "Unsupported Kerberos V5 authentication scheme."))
        (:authentication-scm-credential
         (authentication-error "Unsupported SCM authentication scheme."))
        (:authentication-sspi-credential
         (authentication-error "Unsupported SSPI authentication scheme."))
        (:authentication-sasl-credential
         (authentication-error "Unsupported SASL authentication scheme."))
        (t
         (authentication-error "Unexpected message ~S" message))))))
