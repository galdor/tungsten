(in-package :unicode)

(define-condition unknown-code-point ()
  ((code-point
    :type code-point
    :initarg :code-point
    :reader unknown-code-point-code-point))
  (:report
   (lambda (c stream)
     (format stream "unknown code point U+~4,'0X"
             (unknown-code-point-code-point c)))))

(defvar *character-general-categories*)
(defvar *character-general-categories-ascii*)
(defvar *general-categories*)

(declaim (inline code-point))
(defun code-point (code)
  (declare (type (or code-point character) code))
  (etypecase code
    (code-point code)
    (character (char-code code))))

(defun general-category (code-point)
  (declare (type (or code-point character) code-point))
  (let ((code-point (code-point code-point)))
    (cond
      ((< code-point 128)
       (aref *character-general-categories-ascii* code-point))
      (t
       (flet ((predicate (value)
                (declare (type (unsigned-byte 64) value))
                (let ((start (ldb (byte 24 40) value))
                      (end (ldb (byte 24 16) value)))
                  (cond
                    ((< code-point start) -1)
                    ((> code-point end) 1)
                    (t 0)))))
         (let ((value (binary-search #'predicate
                                     *character-general-categories*)))
           (unless value
             (error 'unknown-code-point :code-point code-point))
           (when value
             (let ((category-id (ldb (byte 16 0) value)))
               (aref *general-categories* category-id)))))))))

(defun binary-search (function vector &key key)
  (declare (type (or symbol function) function)
           (type vector vector)
           (type (or symbol function null) key))
  (do ((low 0)
       (high (1- (length vector))))
      ((> low high)
       nil)
    (declare (type (integer 0) low high))
    (let* ((i (floor (+ high low) 2))
           (value (aref vector i))
           (result (funcall function (if key (funcall key value) value))))
      (cond
        ((zerop result)
         (return (values value i)))
        ((< result 0)
         (setf high (1- i)))
        ((> result 0)
         (setf low (1+ i)))))))
