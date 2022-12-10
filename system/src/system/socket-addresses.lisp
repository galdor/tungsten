(in-package :system)

(deftype port-number ()
  '(unsigned-byte 16))

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
    :accessor ipv6-socket-address-scope-id)
   (flow-label
    :type (or null (unsigned-byte 20))
    :initarg :flow-label
    :initform nil
    :accessor ipv6-socket-address-flow-label)))

(defmethod print-object ((address socket-address) stream)
  (print-unreadable-object (address stream :type t)
    (princ (format-socket-address address) stream)))

(defgeneric format-socket-address (address)
  (:documentation "Return the textual representation of an IP address.")
  (:method ((address ipv4-socket-address))
    (with-slots (address port) address
      (format nil "~A:~D" (format-ip-address address) port)))
  (:method ((address ipv6-socket-address))
    (with-slots (address port) address
      (format nil "[~A]:~D" (format-ip-address address) port))))
