(in-package :system)

(defclass tcp-server ()
  ((thread
    :type (or thread null))
   (mutex
    :type mutex
    :initform (make-mutex :name "tcp-server")
    :reader tcp-server-mutex)
   (closingp
    :type boolean
    :initform nil)
   (io-base
    :type (or io-base null)
    :initarg :io-base
    :reader tcp-server-io-base)
   (sockets
    :type list
    :initform nil
    :accessor tcp-server-sockets)
   (connection-handler
    :type (or symbol function)
    :initarg :connection-handler
    :accessor tcp-server-connection-handler)))

(defun start-tcp-server (host port connection-handler)
  (declare (type host host)
           (type port-number port)
           (type (or symbol function) connection-handler))
  (let ((io-base nil)
        (server nil))
    (core:abort-protect
        (let ((addresses (resolve-net-service host port)))
          (unless addresses
            (error "no socket address found for ~S"
                   (format-host-and-port host port)))
          (setf io-base (make-io-base))
          (setf server (make-instance 'tcp-server
                                      :io-base io-base
                                      :connection-handler connection-handler))
          (dolist (address addresses)
            (let ((socket (tcp-listen address)))
              (push socket (tcp-server-sockets server))
              (fcntl-setfl-add-remove-flags socket '(:o-nonblock) nil)
              (watch-fd io-base socket '(:read)
                        (lambda (events)
                          (declare (ignore events))
                          (accept-tcp-connection server socket)))))
          (make-thread "tcp-server"
                       (lambda ()
                         (setf (slot-value server 'thread) (current-thread))
                         (run-tcp-server server)))
          server)
      (when server
        (close-tcp-server server)))))

(defun stop-tcp-server (server)
  (declare (type tcp-server server))
  (with-slots (thread mutex closingp) server
    (with-mutex (mutex)
      (when closingp
        (return-from stop-tcp-server nil))
      (setf closingp t))
    (join-thread thread)
    (setf thread nil)
    t))

(defun run-tcp-server (server)
  (declare (type tcp-server server))
  (unwind-protect
       (do ((base (tcp-server-io-base server)))
           ((tcp-server-closingp server)
            nil)
         (run-io-base base :timeout 1.0))
    (close-tcp-server server)))

(defun tcp-server-closingp (server)
  (declare (type tcp-server server))
  (with-mutex ((tcp-server-mutex server))
    (slot-value server 'closingp)))

(defun close-tcp-server (server)
  (declare (type tcp-server server))
  (with-slots (io-base sockets) server
    (ignore-errors (mapc #'close-fd sockets))
    (setf sockets nil)
    (when io-base
      (ignore-errors (close-io-base io-base))
      (setf io-base nil)
      t)))

(defun accept-tcp-connection (server socket)
  (declare (type tcp-server server)
           (type (integer 0) socket))
  (multiple-value-bind (connection-socket address)
      (accept socket)
    (with-slots (connection-handler) server
      (core:abort-protect
          (let ((stream (make-instance 'tcp-stream :fd connection-socket
                                                   :address address)))
            (funcall connection-handler stream))
        (close-fd connection-socket)))))

(defun tcp-listen (address)
  (declare (type socket-address address))
  (let* ((sockaddr-type
           (etypecase address
             (ipv4-socket-address 'sockaddr-in)
             (ipv6-socket-address 'sockaddr-in6)))
         (sockaddr-length (ffi:foreign-type-size sockaddr-type))
         (address-family
           (etypecase address
             (ipv4-socket-address :af-inet)
             (ipv6-socket-address :af-inet6)))
         (socket
           (socket address-family :sock-stream :ipproto-tcp)))
    (core:abort-protect
        (progn
          (ffi:with-foreign-value (%option :int)
            (setsockopt socket :sol-socket :so-reuseaddr
                        %option (ffi:foreign-type-size :int)))
          (ffi:with-foreign-value (%address sockaddr-type)
            (ffi:clear-foreign-memory %address sockaddr-length)
            (initialize-sockaddr-from-socket-address %address address)
            (bind socket %address sockaddr-length))
          (socket-listen socket 32)
          socket)
        (close-fd socket))))
