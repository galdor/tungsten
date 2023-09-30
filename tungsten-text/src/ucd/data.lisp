(in-package :ucd)

(defvar *characters* nil)

(defclass character-data ()
  ((code-point
    :type unicode:code-point
    :initarg :code-point
    :reader character-data-code-point)
   (name
    :type (or string null)
    :initarg :name
    :initform nil
    :reader character-data-name)
   (general-category
    :type string
    :initarg :general-category
    :reader character-data-general-category)))

(defmethod print-object ((c character-data) stream)
  (print-unreadable-object (c stream :type t)
    (with-slots (code-point name) c
      (format stream "U+~4,'0X~@[ ~A~]" code-point name))))

(defun find-character-data (code)
  (declare (type unicode:code-point code))
  (when (<= 0 code (length *characters*))
    (aref *characters* code)))

(defun load-character-data ()
  (let ((characters (make-array 0 :element-type '(or character-data null)
                                  :adjustable t :fill-pointer 0)))
    (mapc-unicode-data-file
     (lambda (code-point
              name
              general-category
              canonical-combining-class
              bidi-class
              decomposition-string
              numeric-decimal-string
              numeric-digit-string
              numeric-numeric-string
              bidi-mirrored
              unicode-1-name
              iso-comment
              simple-uppercase-mapping
              simple-lowercase-mapping
              simple-titlecase-mapping)
       (declare (ignore canonical-combining-class
                        bidi-class
                        decomposition-string
                        numeric-decimal-string
                        numeric-digit-string
                        numeric-numeric-string
                        bidi-mirrored
                        unicode-1-name
                        iso-comment
                        simple-uppercase-mapping
                        simple-lowercase-mapping
                        simple-titlecase-mapping))
       (let* ((character-data
                (make-instance 'character-data
                               :code-point code-point
                               :name name
                               :general-category general-category)))
         (do ((i (length characters) (1+ i)))
             ((>= i code-point)
              nil)
           (vector-push-extend nil characters))
         (vector-push-extend character-data characters))))
    (setf *characters* characters)
    t))

(defmacro do-characters ((code-point character-data &optional result) &body body)
  `(dotimes (,code-point (length *characters*) ,result)
     (let ((,character-data (aref *characters* ,code-point)))
       (when ,character-data
         ,@body))))
