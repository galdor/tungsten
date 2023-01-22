(defpackage :ffi
  (:use :cl)
  (:export
   #:use-foreign-library
   #:use-asdf-shared-library

   #:define-foreign-type
   #:foreign-type
   #:foreign-type-size
   #:encode-foreign-value
   #:decode-foreign-value
   #:define-type-alias

   #:pointer
   #:pointer+
   #:null-pointer
   #:null-pointer-p
   #:allocate-foreign-memory
   #:free-foreign-memory
   #:with-foreign-value
   #:with-foreign-values
   #:foreign-value
   #:read-foreign-memory
   #:clear-foreign-memory
   #:with-pinned-vector-data

   #:*default-string-encoding*
   #:allocate-foreign-string
   #:with-foreign-string
   #:with-foreign-strings
   #:foreign-string-length
   #:decode-foreign-string

   #:define-enum

   #:define-bitset

   #:define-struct
   #:struct-member
   #:struct-member-pointer
   #:with-struct-members

   #:define-foreign-union
   #:foreign-union-member
   #:with-foreign-union-members

   #:defcallback
   #:callback-pointer

   #:foreign-funcall

   #:errno))
