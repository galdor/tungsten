(in-package :ffi)

(defun foreign-type-size (type)
  (%foreign-type-size type))
