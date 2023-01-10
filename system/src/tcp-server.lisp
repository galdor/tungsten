(in-package :system)

(defclass tcp-server ()
  ((io-base
    :type (or io-base null)
    :initarg :io-base
    :reader tcp-server-io-base)
   (sockets
    :type list
    :initform nil
    :accessor tcp-server-sockets)))

(defun make-tcp-server (host port)
  (declare (type host host)
           (type port-number port))
  (let ((io-base nil)
        (server nil))
    (core:abort-protect
        (let ((addresses (resolve-net-service host port)))
          (unless addresses
            (error "no socket address found for ~S"
                   (format-host-and-port host port)))
          (setf io-base (make-io-base))
          (setf server (make-instance 'tcp-server :io-base io-base))
          (dolist (address addresses)
            (let ((socket (tcp-listen address)))
              (push socket (tcp-server-sockets server))
              (fcntl-setfl-add-flags socket '(:o-nonblock))
              (watch-fd io-base socket '(:read)
                        (lambda (events)
                          (declare (ignore events))
                          (accept-tcp-connection server socket)))))
          server)
      (when server
        (close-tcp-server server)))))

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
           (type (integer 0) socket)
           (ignore server))
  (let ((connection-socket (accept socket)))
    ;; TODO
    (close-fd connection-socket)))

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
