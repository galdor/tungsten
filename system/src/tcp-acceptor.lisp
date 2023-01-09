(in-package :system)

(defclass tcp-acceptor ()
  ((socket
    :type (or (integer 0) null)
    :initarg :socket
    :reader tcp-acceptor-socket)
   (address
    :type socket-address
    :initarg :address
    :reader tcp-acceptor-address)))

(defmethod print-object ((acceptor tcp-acceptor) stream)
  (print-unreadable-object (acceptor stream :type t)
    (with-slots (address) acceptor
      (write-string (format-socket-address address) stream))))

(defun make-tcp-acceptor (address)
  (declare (type socket-address address))
  (let ((socket (tcp-listen address)))
    (core:abort-protect
        (make-instance 'tcp-acceptor :socket socket :address address)
      (close-fd socket))))

(defun close-tcp-acceptor (acceptor)
  (declare (type tcp-acceptor acceptor))
  (with-slots (socket) acceptor
    (when socket
      (close-fd socket)
      (setf socket nil)
      t)))

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

(defun accept-tcp-connection (acceptor)
  (declare (type tcp-acceptor acceptor))
  (accept (tcp-acceptor-socket acceptor)))
