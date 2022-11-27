(defpackage :ffi
  (:use :cl)
  (:export
   :use-foreign-library
   :use-asdf-shared-library

   :define-foreign-type
   :foreign-type-size

   :null-pointer
   :null-pointer-p
   :allocate-foreign-memory
   :free-foreign-memory
   :with-foreign-value
   :with-foreign-values
   :with-foreign-memory
   :foreign-value-ref

   :*default-string-encoding*
   :allocate-foreign-string
   :with-foreign-string
   :with-foreign-strings
   :foreign-string-length
   :decode-foreign-string

   :define-enum

   :foreign-funcall

   :errno))
