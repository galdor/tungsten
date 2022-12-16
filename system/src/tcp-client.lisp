(in-package :system)

(deftype host ()
  '(or string ip-address))

(defclass tcp-client (streams:fundamental-binary-input-stream
                      streams:fundamental-binary-output-stream)
  ((host
    :type host
    :initarg :host
    :reader tcp-client-host)
   (port
    :type port-number
    :initarg :port
    :reader tcp-client-port)
   (address
    :type socket-address
    :initarg :address
    :reader tcp-client-address)
   (socket
    :type (or (integer 0) null)
    :initarg :socket
    :initform nil
    :reader tcp-client-socket)))

(defmethod print-object ((client tcp-client) stream)
  (print-unreadable-object (client stream :type t)
    (with-slots (address) client
      (write-string (format-socket-address address) stream))))

(defun make-tcp-client (host port)
  "Create and return a TCP client connected to HOST and PORT."
  (declare (type host host)
           (type port-number port))
  (multiple-value-bind (socket address)
      (tcp-connect host port)
    (make-instance 'tcp-client :host host :port port :address address
                               :socket socket)))

(defun tcp-connect (host port)
  "Establish a TCP connection to HOST and PORT. If HOST is an IP address, use it
as it is. If HOST is a hostname, resolve it and try to connect to each
resulting address until a connection succeeds. Return both the socket
associated with the connection and the socket address used.

If HOST is a hostname, unsuccessful connection attempts are logged to
*ERROR-OUTPUT*."
  (declare (type host host)
           (type port-number port))
  (etypecase host
    (string
     (let ((addresses (resolve-net-service host port)))
       (dolist (address (core:nshuffle addresses))
         (handler-case
             (return-from tcp-connect
               (values (tcp-connect-to-address address) address))
           (error (c)
             (format *error-output* "cannot connect to ~S: ~A" address c))))
       (error "cannot connect to ~A port ~D" host port)))
    (ip-address
     (let ((address (make-ip-socket-address host port)))
       (values (tcp-connect-to-address address) address)))))

(defun tcp-connect-to-address (address)
  "Establish a TCP connection to ADDRESS and return the socket associated to
it."
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
           (socket address-family :sock-stream :ipproto-tcp))
         (success nil))
    (unwind-protect
         (ffi:with-foreign-value (%addr sockaddr-type)
           (ffi:clear-foreign-memory %addr sockaddr-length)
           (initialize-sockaddr-from-socket-address %addr address)
           (connect socket %addr sockaddr-length)
           (setf success t)
           socket)
      (unless success
        (close-fd socket)))))

(defmethod close ((client tcp-client) &key abort)
  (declare (ignore abort))
  (with-slots (socket) client
    (when socket
      (close-fd (tcp-client-socket client))
      (setf socket nil)
      t)))

(defmethod open-stream-p ((client tcp-client))
  (not (null (tcp-client-socket client))))
