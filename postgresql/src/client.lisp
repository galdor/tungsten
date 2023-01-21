(in-package :postgresql)

(defclass client ()
  ((stream
    :type system:network-stream
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
           (type (or string null) user password database application-name)
           (ignore password))
  ;; It may seems strange that the host is not a mandatory parameter, but in
  ;; the future we would like to support UNIX sockets.
  (let ((stream (system:make-tcp-client host port)))
    (core:abort-protect
        (let ((parameters `(("user" . ,user)
                            ("database" . ,database)
                            ,(when application-name
                               `("application_name" . ,application-name)))))
          (write-startup-message 3 0 parameters stream)
          (finish-output stream)
          stream)
      (close stream))))

(defun close-client (client)
  (declare (type client client))
  (with-slots (stream) client
    (when stream
      (ignore-errors (close stream))
      (setf stream nil)
      t)))
