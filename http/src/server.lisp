(in-package :http)

(defclass server ()
  ((mutex
    :type system:mutex
    :initform (system:make-mutex :name "http-server")
    :reader server-mutex)
   (closingp
    :type boolean
    :initform nil)
   (tcp-server
    :type (or system:tcp-server null)
    :initform nil)
   (connections
    :type list
    :initform nil)
   (connections-condition-variable
    :type system:condition-variable
    :initform (system:make-condition-variable :name "http-server-connections"))
   (connection-handlers
    :type list
    :initform nil)))

(defclass connection ()
  ((stream
    :type system:network-stream
    :initarg :stream
    :reader connection-stream)))

(defun start-server (host port &key (nb-threads 1))
  (let ((server (make-instance 'server))
        (connection-handlers nil))
    (core:abort-protect
        (flet ((handle-connection (stream)
                 (let ((connection (make-instance 'connection :stream stream)))
                   (server-handle-connection server connection))))
          (dotimes (i nb-threads)
            (push (system:make-thread "http-connection-handler"
                                      (lambda ()
                                        (connection-handler server)))
                  connection-handlers))
          (setf (slot-value server 'connection-handlers) connection-handlers)
          (setf (slot-value server 'tcp-server)
                (system:start-tcp-server host port #'handle-connection))
          server)
      (mapc 'system:join-thread connection-handlers))))

(defun stop-server (server)
  (declare (type server server))
  (with-slots (mutex closingp tcp-server connection-handlers connections)
      server
    (system:with-mutex (mutex)
      (setf closingp t))
    (mapc 'system:join-thread connection-handlers)
    (setf connection-handlers nil)
    (mapc 'close-connection connections)
    (when tcp-server
      (system:stop-tcp-server tcp-server)
      (setf tcp-server nil)
      t)))

(defun server-handle-connection (server connection)
  (declare (type server server)
           (type connection connection))
  (with-slots (stream) connection
    (core:abort-protect
        (progn
          (setf (system:io-stream-external-format stream)
                '(:ascii :eol-style :crlf))
          (setf (system:network-stream-read-timeout stream) 30000
                (system:network-stream-write-timeout stream) 30000)
          (server-enqueue-connection server connection))
      (close-connection connection))))

(defun server-enqueue-connection (server connection)
  (declare (type server server)
           (type connection connection))
  (with-slots (mutex connections connections-condition-variable) server
    (system:with-mutex (mutex)
      (push connection connections)
      (when (null (cdr connections))
        (system:signal-condition-variable connections-condition-variable)))))

(defun server-pop-connection (server)
  (declare (type server server))
  (with-slots (mutex connections connections-condition-variable) server
    (system:with-mutex (mutex)
      (system:wait-condition-variable connections-condition-variable mutex
                                      :timeout 1.0)
      (pop connections))))

(defun connection-handler (server)
  (declare (type server server))
  (do ((mutex (server-mutex server)))
      ((system:with-mutex (mutex) (slot-value server 'closingp))
       nil)
    (let ((connection (server-pop-connection server)))
      (when connection
        (core:abort-protect
            (handle-connection server connection)
          (close-connection connection))))))

(defun handle-connection (server connection)
  (declare (type server server)
           (type connection connection))
  (let* ((stream (connection-stream connection))
         (request (read-request stream)))
    (handle-request server connection request))
  (close-connection connection) ; TODO See Connection header field
  ;; (server-enqueue-connection server connection)
  nil)

(defun handle-request (server connection request)
  (declare (type server server)
           (type connection connection)
           (type request request)
           (ignore server connection))
  ;; TODO
  (format t "processing request ~S~%" request))

(defun close-connection (connection)
  (declare (type connection connection))
  (with-slots (stream) connection
    (ignore-errors (close stream))))
