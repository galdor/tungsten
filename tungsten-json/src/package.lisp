(defpackage :json
  (:use :cl)
  (:export
   #:duplicate-key-handling
   #:*duplicate-key-handling*
   #:*max-depth*
   #:json-parse-error
   #:parse

   #:large-integer-representation
   #:*large-integer-representation*
   #:unserializable-value
   #:serialize

   #:pointer
   #:pointer-parse-error
   #:invalid-pointer
   #:parse-pointer
   #:serialize-pointer
   #:pointer-equal
   #:pointer-ref))
