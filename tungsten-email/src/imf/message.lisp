(in-package :imf)

(deftype header ()
  'list)

(deftype body ()
  '(or null string))

(defclass message ()
  ((header
    :type header
    :initarg :header
    :initform nil
    :accessor message-header)
   (body
    :type body
    :initarg :body
    :initform nil
    :accessor message-body)))

(defun make-message (&key header body)
  (declare (type header header)
           (type body body))
  (make-instance 'message :header header :body body))
