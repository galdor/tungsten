(in-package :http)

(defclass server ()
  ((mutex
    :type system:mutex
    :initform (system:make-mutex :name "http-server")
    :reader server-mutex)
   (tcp-server
    :type (or system:tcp-server null)
    :initform nil
    :accessor server-tcp-server)
   (connection-handlers
    :type list
    :initform nil
    :accessor server-connection-handlers)))

(defun start-server (host port &key (nb-threads 1))
  (let ((server (make-instance 'server))
        (connection-handlers nil))
    (core:abort-protect
        (progn
          (dotimes (i nb-threads)
            (push (system:make-thread "http-connection-handler"
                                      (lambda ()
                                        ;; TODO
                                        nil))
                  connection-handlers))
          (setf (server-connection-handlers server) connection-handlers)
          (setf (server-tcp-server server)
                (system:start-tcp-server
                 host port
                 (lambda (stream)
                   ;; TODO
                   (close stream))))
          server)
      (mapc 'system:join-thread connection-handlers))))

(defun stop-server (server)
  (declare (type server server))
  (with-slots (tcp-server connection-handlers) server
    (mapc 'system:join-thread connection-handlers)
    (setf connection-handlers nil)
    (when tcp-server
      (system:stop-tcp-server tcp-server)
      (setf tcp-server nil)
      t)))
