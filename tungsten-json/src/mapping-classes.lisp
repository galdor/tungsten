(in-package :json)

(defvar *mapping-classes* (make-hash-table))

(define-condition unknown-mapping-class (error)
  ((name
    :type symbol
    :initarg :name))
  (:report
   (lambda (condition stream)
     (with-slots (name) condition
       (format stream "Unknown JSON mapping class ~S." name)))))

(defun register-mapping-class (name class)
  (setf (gethash name *mapping-classes*) class))

(defun delete-mapping-class (name)
  (remhash name *mapping-classes*))

(defun find-mapping-class (name)
  (or (gethash name *mapping-classes*)
      (error 'unknown-mapping-class :name name)))
