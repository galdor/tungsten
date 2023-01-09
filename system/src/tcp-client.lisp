(in-package :system)

(define-condition tcp-connection-failure ()
  ((host
    :type host
    :initarg :host
    :reader tcp-connection-failure-host)
   (port
    :type port-number
    :initarg :port
    :reader tcp-connection-failure-port)
   (address-errors
    :type list
    :initarg :errors
    :reader tcp-connection-failure-address-errors))
  (:report
   (lambda (condition stream)
     (with-slots (host port address-errors) condition
       (format stream "Cannot connect to ~A.~%"
               (format-host-and-port host port))
       (dolist (address-error address-errors)
         (terpri stream)
         (destructuring-bind (address . error) address-error
           (format stream "~A~%~A~%"
                   (format-socket-address address) error)))))))

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
                        &key (external-format text:*default-external-format*)
                             read-timeout write-timeout)
  "Create and return a TCP client connected to HOST and PORT."
  (declare (type host host)
           (type port-number port)
           (type (or (integer 0) null) read-timeout write-timeout))
  (multiple-value-bind (socket address)
      (tcp-connect host port)
    (core:abort-protect
        (make-instance 'tcp-client :fd socket
                                   :address address
                                   :external-format external-format
                                   :host host :port port
                                   :read-timeout read-timeout
                                   :write-timeout write-timeout)
      (when socket
        (close-fd socket)))))

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
     (let ((addresses (resolve-net-service host port))
           (errors nil))
       (dolist (address (core:nshuffle addresses))
         (handler-case
             (return-from tcp-connect
               (values (tcp-connect-to-address address) address))
           (error (condition)
             (push (cons address condition) errors))))
       (error 'tcp-connection-failure :host host :port port
                                      :errors (nreverse errors))))
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
           (socket address-family :sock-stream :ipproto-tcp)))
    (core:abort-protect
        (ffi:with-foreign-value (%addr sockaddr-type)
          (ffi:clear-foreign-memory %addr sockaddr-length)
          (initialize-sockaddr-from-socket-address %addr address)
          (connect socket %addr sockaddr-length)
          socket)
      (close-fd socket))))
