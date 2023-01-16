(in-package :log)

(defclass message ()
  ((time
    :type (integer 0)
    :initform (get-universal-time)
    :reader message-time)
   (domain
    :type (or domain null)
    :initarg :domain
    :initform nil
    :accessor message-domain)
   (level
    :type level
    :initarg :level
    :accessor message-level)
   (text
    :type string
    :initarg :text
    :accessor message-text)
   (data
    :type list
    :initarg :data
    :initform nil
    :accessor message-data)))

(defun normalize-message-data (message)
  (declare (type message message))
  (with-slots (data) message
    (dolist (datum data)
      (let ((key (car datum))
            (value (cdr datum)))
        (etypecase key
          (string
           nil)
          (symbol
           (rplaca datum (symbol-name key))))
        (typecase value
          (string
           nil)
          (t
           (rplacd datum (princ-to-string value))))))
    (setf data (sort data #'string< :key #'car)))
  message)
