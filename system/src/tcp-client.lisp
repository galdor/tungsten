(in-package :system)

(deftype host ()
  '(or string ip-address))

(defclass tcp-client (tcp-stream)
  ((host
    :type host
    :initarg :host
    :reader tcp-client-host)
   (port
    :type port-number
    :initarg :port
    :reader tcp-client-port)))

(defmethod print-object ((client tcp-client) stream)
  (print-unreadable-object (client stream :type t)
    (with-slots (address) client
      (write-string (format-socket-address address) stream))))

(defun make-tcp-client (host port
                        &key (external-format text:*default-external-format*))
  "Create and return a TCP client connected to HOST and PORT."
  (declare (type host host)
           (type port-number port))
  (let ((success nil))
    (multiple-value-bind (socket address) (tcp-connect host port)
      (unwind-protect
           (prog1
               (make-instance 'tcp-client :socket socket :address address
                                          :external-format external-format
                                          :host host :port port)
             (setf success t))
        (unless success
          (when socket
            (close-fd socket)))))))

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
             (format *error-output* "cannot connect to ~A: ~A~%" address c))))
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
