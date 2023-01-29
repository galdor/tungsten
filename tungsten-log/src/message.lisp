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
   (text-format
    :type string
    :initarg :text-format
    :accessor message-text-format)
   (text-arguments
    :type list
    :initarg :text-arguments
    :accessor message-text-arguments)
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
           (rplaca datum (string-downcase (symbol-name key)))))
        (typecase value
          (string
           nil)
          (t
           (rplacd datum (write-to-string value :case :downcase
                                                :escape nil
                                                :pretty nil))))))
    (setf data (sort data #'string< :key #'car)))
  message)
