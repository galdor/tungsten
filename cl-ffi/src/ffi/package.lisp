(defpackage :ffi
  (:use :cl)
  (:export
   :use-foreign-library
   :use-asdf-shared-library

   :define-foreign-type
   :foreign-type-size
   :define-type-alias

   :null-pointer
   :null-pointer-p
   :allocate-foreign-memory
   :free-foreign-memory
   :with-foreign-value
   :with-foreign-values
   :with-foreign-memory
   :foreign-value

   :*default-string-encoding*
   :allocate-foreign-string
   :with-foreign-string
   :with-foreign-strings
   :foreign-string-length
   :decode-foreign-string

   :define-enum

   :define-struct
   :struct-member
   :struct-member-pointer

   :foreign-funcall

   :errno))
