(in-package :uri)

(deftype port-number ()
  '(integer 1 65535))

(defclass uri ()
  ((scheme
    :type (or null string)
    :initarg :scheme
    :accessor uri-scheme)
   (username
    :type (or null string)
    :initarg :username
    :accessor uri-username)
   (password
    :type (or null string)
    :initarg :password
    :accessor uri-password)
   (host
    :type (or null string)
    :initarg :host
    :accessor uri-host)
   (port
    :type (or null port-number)
    :initarg :port
    :accessor uri-port)
   (path
    :type (or null string)
    :initarg :path
    :accessor uri-path)
   (query
    :type (or null list)
    :initarg :query
    :accessor uri-query)
   (fragment
    :type (or null string)
    :initarg :fragment
    :accessor uri-fragment)))

(defmethod print-object ((uri uri) stream)
  (print-unreadable-object (uri stream :type t)
    (write-string (serialize uri) stream)))

(defun make-uri (&key scheme username password host port path query fragment)
  (make-instance 'uri :scheme scheme
                      :username username
                      :password password
                      :host host
                      :port port
                      :path path
                      :query query
                      :fragment fragment))
