(in-package :mime)

(defclass media-type ()
  ((type
    :type string
    :initarg :type
    :accessor media-type-type)
   (subtype
    :type string
    :initarg :subtype
    :accessor media-type-subtype)
   (parameters
    :type list
    :initarg :parameters
    :initform nil
    :accessor media-type-parameters)))

(defmethod print-object ((media-type media-type) stream)
  (print-unreadable-object (media-type stream :type t)
    (prin1 (serialize-media-type media-type) stream)))

(defun make-media-type (type subtype &key parameters)
  (declare (type string type subtype)
           (type list parameters))
  (make-instance 'media-type :type type :subtype subtype
                             :parameters parameters))

(defun media-type-parameter (media-type name)
  (declare (type media-type media-type))
  (cdr (assoc name (media-type-parameters media-type) :test #'equalp)))

(defun (setf media-type-parameter) (value media-type name)
  (declare (type media-type media-type))
  (with-slots (parameters) media-type
    (let ((parameter (assoc name parameters :test #'equalp)))
      (if parameter
          (rplacd parameter value)
          (push (cons name value) parameters)))))

(defun serialize-media-type (media-type)
  (declare (type media-type media-type))
  (with-output-to-string (stream)
    (with-slots (type subtype parameters) media-type
      (write-string type stream)
      (write-char #\/ stream)
      (write-string subtype stream)
      (dolist (parameter parameters)
        (write-char #\; stream)
        (write-string (car parameter) stream)
        (write-char #\= stream)
        (write-string (cdr parameter) stream)))))
