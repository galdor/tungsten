(in-package :system)

(deftype host ()
  '(or string ip-address))

(deftype port-number ()
  '(unsigned-byte 16))

(defun format-host-and-port (host port &optional stream)
  (declare (type host host)
           (type port-number port))
  (if (position #\: host)
      (format stream "[~A]:~D" host port)
      (format stream "~A:~D" host port)))

(defclass socket-address ()
  ())

(defclass ip-socket-address (socket-address)
  ((port
    :type port-number
    :initarg :port
    :initform 0
    :accessor ip-socket-address-port)))

(defclass ipv4-socket-address (ip-socket-address)
  ((address
    :type ipv4-address
    :initarg :address
    :accessor ipv4-socket-address-address)))

(defclass ipv6-socket-address (ip-socket-address)
  ((address
    :type ipv6-address
    :initarg :address
    :accessor ipv6-socket-address-address)
   (scope-id
    :type (or null (unsigned-byte 32))
    :initarg :scope-id
    :initform nil
    :accessor ipv6-socket-address-scope-id)))

(defmethod print-object ((address socket-address) stream)
  (print-unreadable-object (address stream :type t)
    (princ (format-socket-address address) stream)))

(defun make-ip-socket-address (ip-address port)
  (let ((class (etypecase ip-address
                 (ipv4-address 'ipv4-socket-address)
                 (ipv6-address 'ipv6-socket-address))))
    (make-instance class :address ip-address :port port)))

(defgeneric format-socket-address (address &optional stream)
  (:documentation "Return the textual representation of an IP address.")
  (:method ((address ipv4-socket-address) &optional stream)
    (with-slots (address port) address
      (format stream "~A:~D" (format-ip-address address) port)))
  (:method ((address ipv6-socket-address) &optional stream)
    (with-slots (address port) address
      (format stream "[~A]:~D" (format-ip-address address) port))))

(defgeneric socket-address-equal (address1 address2)
  (:method ((address1 ipv4-socket-address) (address2 ipv4-socket-address))
    (and (= (ip-socket-address-port address1)
            (ip-socket-address-port address2))
         (equalp (ipv4-socket-address-address address1)
                 (ipv4-socket-address-address address2))))
  (:method ((address1 ipv6-socket-address) (address2 ipv6-socket-address))
    (and (= (ip-socket-address-port address1)
            (ip-socket-address-port address2))
         (equalp (ipv6-socket-address-address address1)
                 (ipv6-socket-address-address address2))
         (= (ipv6-socket-address-scope-id address1)
            (ipv6-socket-address-scope-id address2))))
  (:method ((address1 ip-socket-address) (address2 ip-socket-address))
    (and (eq (class-of address1) (class-of address2))
         (= (ip-socket-address-port address1)
            (ip-socket-address-port address2))))
  (:method ((address1 socket-address) (address2 socket-address))
    nil))

(defgeneric initialize-socket-address-from-sockaddr (address %addr)
  (:method ((address ipv4-socket-address) %addr)
    (let* ((%in (ffi:struct-member-pointer %addr 'sockaddr-in :sin-addr))
           (addr (ffi:struct-member %in 'in-addr :s-addr))
           (port (ffi:struct-member %addr 'sockaddr-in :sin-port)))
      (setf (ipv4-socket-address-address address)
            (ip-address (vector (ldb (byte 8 0) addr)
                                (ldb (byte 8 8) addr)
                                (ldb (byte 8 16) addr)
                                (ldb (byte 8 24) addr))))
      (setf (ip-socket-address-port address)
            (logior (ash (ldb (byte 8 0) port) 8)
                    (ldb (byte 8 8) port))))
    address)
  (:method ((address ipv6-socket-address) %addr)
    ;; It would be nice to handle the :sin6-flowinfo member. According to RFC
    ;; 2553 3.3, it is supposed to contain the traffic class (8 bit) and the
    ;; flow label (20 bit). However there does not seem to be any information
    ;; on the way these two values are combined in the 32 bit member.
    ;;
    ;; Calling getaddrinfo() always return flowinfo fields set to zero anyway.
    (ffi:with-struct-members (((port :sin6-port)
                               (scope-id :sin6-scope-id))
                              %addr 'sockaddr-in6)
      (let ((%in6 (ffi:struct-member-pointer
                   %addr 'sockaddr-in6 :sin6-addr))
            (ipv6-address (make-array 8 :element-type '(unsigned-byte 16))))
        (dotimes (i 8)
          (let ((hi (ffi:struct-member %in6 'in6-addr :s6-addr (* i 2)))
                (lo (ffi:struct-member %in6 'in6-addr :s6-addr (1+ (* i 2)))))
            (setf (aref ipv6-address i)
                  (logior (ash hi 8) lo))))
        (setf (ipv6-socket-address-address address) (ip-address ipv6-address)))
      (setf (ipv6-socket-address-scope-id address) scope-id)
      (setf (ip-socket-address-port address)
            (logior (ash (ldb (byte 8 0) port) 8)
                    (ldb (byte 8 8) port))))
    address))

(defgeneric initialize-sockaddr-from-socket-address (%addr address)
  (:method (%addr (address ipv4-socket-address))
    (with-slots (address port) address
      (setf (ffi:struct-member %addr 'sockaddr-in :sin-family)
            (ffi:encode-foreign-value :af-inet 'address-family))
      (setf (ffi:struct-member %addr 'sockaddr-in :sin-port)
            (logior (ash (ldb (byte 8 0) port) 8)
                    (ldb (byte 8 8) port)))
      (let ((%in (ffi:struct-member-pointer %addr 'sockaddr-in :sin-addr)))
        (setf (ffi:struct-member %in 'in-addr :s-addr)
              (logior (aref address 0)
                      (ash (aref address 1) 8)
                      (ash (aref address 2) 16)
                      (ash (aref address 3) 24)))))
    %addr)
  (:method (%addr (address ipv6-socket-address))
    (with-slots (address scope-id port) address
      (setf (ffi:struct-member %addr 'sockaddr-in6 :sin6-family)
            (ffi:encode-foreign-value :af-inet6 'address-family))
      (setf (ffi:struct-member %addr 'sockaddr-in6 :sin6-port)
            (logior (ash (ldb (byte 8 0) port) 8)
                    (ldb (byte 8 8) port)))
      (let ((%in6 (ffi:struct-member-pointer %addr 'sockaddr-in6 :sin6-addr)))
        (dotimes (i 8)
          (let ((offset (* i 2))
                (group (aref address i)))
            (setf (ffi:struct-member %in6 'in6-addr :s6-addr offset)
                  (ldb (byte 8 8) group))
            (setf (ffi:struct-member %in6 'in6-addr :s6-addr (1+ offset))
                  (ldb (byte 8 0) group)))))
      (setf (ffi:struct-member %addr 'sockaddr-in6 :sin6-scope-id)
            (or scope-id 0)))
    %addr))

(defun resolve-net-service (host service-or-port)
  "Resolve a host and service and return a list of matching socket addresses.

HOST is a string containing either a hostname or a numeric address.
SERVICE-OR-PORT is either a service name as defined by RFC 6335 or a port
number.

This function is used to obtain socket addresses which can then be used for
network clients and servers. It is not a generic DNS client and should not
be used as such."
  (declare (type string host)
           (type (or string port-number) service-or-port))
  (let ((service (if (stringp service-or-port)
                     service-or-port
                     (princ-to-string service-or-port)))
        (addresses nil))
    (ffi:with-foreign-value (%hints 'addrinfo)
      (ffi:clear-foreign-memory
       %hints (ffi:foreign-type-size 'addrinfo))
      (setf (ffi:struct-member %hints 'addrinfo :ai-flags) '(:ai-addrconfig))
      (with-getaddrinfo (%info host service :%hints %hints)
        (ffi:with-struct-members (((family :ai-family)
                                   (%addr :ai-addr))
                                  %info 'addrinfo)
          (let ((address-class (case family
                                 (:af-inet 'ipv4-socket-address)
                                 (:af-inet6 'ipv6-socket-address))))
            (when address-class
              (let ((address (make-instance address-class)))
                (initialize-socket-address-from-sockaddr address %addr)
                (pushnew address addresses :test 'socket-address-equal)))))))
    (nreverse addresses)))

(defun socket-address-net-service (address &key numeric-host
                                                numeric-service)
  "Return the name and service associated with a socket address."
  (declare (type socket-address address))
  (let* ((sockaddr-type (etypecase address
                          (ipv4-socket-address 'sockaddr-in)
                          (ipv6-socket-address 'sockaddr-in6)))
         (sockaddr-length (ffi:foreign-type-size sockaddr-type))
         (flags nil))
    (when numeric-host
      (push :ni-numerichost flags))
    (when numeric-service
      (push :ni-numericserv flags))
    (ffi:with-foreign-value (%addr sockaddr-type)
      (ffi:clear-foreign-memory %addr sockaddr-length)
      (initialize-sockaddr-from-socket-address %addr address)
      (getnameinfo %addr sockaddr-length flags))))
