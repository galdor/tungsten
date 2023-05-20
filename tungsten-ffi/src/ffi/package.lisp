(defpackage :ffi
  (:use :cl)
  (:export
   #:use-foreign-library
   #:use-asdf-shared-library

   #:foreign-type
   #:foreign-type-size
   #:foreign-base-type
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

   #:define-foreign-enumeration

   #:define-foreign-bitset

   #:define-foreign-structure
   #:foreign-structure-member
   #:foreign-structure-member-pointer
   #:with-foreign-structure-members

   #:define-foreign-union
   #:foreign-union-member
   #:with-foreign-union-members

   #:defcallback
   #:callback-pointer

   #:foreign-funcall

   #:errno))
