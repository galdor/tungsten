(in-package :http)

(defclass response ()
  ((status
     :type response-status
     :initarg :status
     :accessor response-status)
   (reason
    :type string
    :initarg :reason
    :accessor response-reason)
   (version
    :type protocol-version
    :initarg :version
    :initform :http-1.1
    :accessor response-version)
   (header
    :type header
    :initarg :header
    :initform nil
    :accessor response-header)
   (body
    :type (or body null)
    :initarg :body
    :initform nil
    :accessor response-body)))
