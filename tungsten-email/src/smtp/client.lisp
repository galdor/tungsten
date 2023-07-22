(in-package :smtp)

(defclass client ()
  ((host
    :type system:host
    :initarg :host
    :reader client-host)
   (port
    :type system:port-number
    :initarg :port
    :reader client-port)
   (tls
    :type boolean
    :initarg :tls
    :reader client-tls-p)
   (stream
    :type (or system:network-stream null)
    :initarg :stream
    :reader client-stream)
   (domains
    :type list
    :initform nil
    :reader client-domains)))

(defmethod print-object ((client client) stream)
  (print-unreadable-object (client stream :type t)
    (let* ((client-stream (client-stream client))
           (address (system:network-stream-address client-stream)))
      (system:format-socket-address address stream)
      (when (client-tls-p client)
        (write-string " TLS" stream)))))

(defun make-client (host port &key tls)
  (declare (type system:host host)
           (type system:port-number port)
           (type boolean tls))
  (let* ((external-format '(:ascii :eol-style :crlf))
         (stream
           (cond
             (tls
              (openssl:make-tls-client host port
                                       :external-format external-format))
             (t
              (system:make-tcp-client host port
                                      :external-format external-format)))))
    (core:abort-protect
        (let ((client (make-instance 'client :host host :port port :tls tls
                                             :stream stream)))
          (read-greeting-response client)
          client)
      (close stream))))

(defun disconnect-client (client)
  (declare (type client client))
  (with-slots (stream) client
    (close stream)
    (setf stream nil))
  nil)
