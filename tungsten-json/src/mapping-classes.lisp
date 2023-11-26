(in-package :json)

(defvar *mapping-classes* (make-hash-table))

(define-condition unknown-mapping-class (error)
  ((name
    :type symbol
    :initarg :name
    :reader unknown-mapping-class-name))
  (:report
   (lambda (condition stream)
     (format stream "unknown JSON mapping class ~S"
             (unknown-mapping-class-name condition)))))

(defun register-mapping-class (name class)
  (setf (gethash name *mapping-classes*) class))

(defun delete-mapping-class (name)
  (remhash name *mapping-classes*))

(defun find-mapping-class (name)
  (or (gethash name *mapping-classes*)
      (error 'unknown-mapping-class :name name)))
