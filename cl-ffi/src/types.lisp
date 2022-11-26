(in-package :ffi)

(defvar *foreign-types* (make-hash-table :test #'eq))

(defun define-foreign-type (type base-type)
  (declare (type symbol type base-type))
  (setf (gethash type *foreign-types*) base-type))

(defun resolve-foreign-type (type)
  (or (gethash type *foreign-types*)
      (error "unknown foreign type ~A" type)))
