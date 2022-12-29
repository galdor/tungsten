(in-package :http)

(defclass request ()
  ((method
     :type request-method
     :initform :get
     :accessor request-method)
   (target
    :type request-target
    :initform (uri:make-uri :path "/")
    :accessor request-target)
   (version
    :type protocol-version
    :initform :http-1.1
    :accessor request-version)
   (header
    :type header
    :initform nil
    :accessor request-header)
   (body
    :type (or body null)
    :initform nil
    :accessor request-body)))

(defun request-header-field (request name)
  (declare (type request request)
           (type header-field-name name))
  (header-field (request-header request) name))

(defun (setf request-header-field) (value request name)
  (declare (type request request)
           (type header-field-name name))
  (setf (header-field (request-header request) name) value))
