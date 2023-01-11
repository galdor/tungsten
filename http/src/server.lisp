(in-package :http)

(defclass server ()
  ((mutex
    :type system:mutex
    :initform (system:make-mutex :name "http-server")
    :reader server-mutex)
   (tcp-server
    :type (or system:tcp-server null)
    :initform nil)))

(defun start-server (host port)
  (let ((server (make-instance 'server)))
    (setf (slot-value server 'tcp-server)
          (system:start-tcp-server host port
                                   (lambda (stream) (close stream))))
    server))

(defun stop-server (server)
  (declare (type server server))
  (with-slots (tcp-server) server
    (when tcp-server
      (system:stop-tcp-server tcp-server)
      (setf tcp-server nil)
      t)))
