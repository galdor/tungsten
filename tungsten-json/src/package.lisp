(defpackage :json
  (:use :cl)
  (:export
   #:pointer
   #:child-pointer
   #:pointer*
   #:pointer-parse-error
   #:invalid-pointer
   #:parse-pointer
   #:serialize-pointer
   #:make-pointer
   #:parent-pointer
   #:pointer-equal
   #:pointer-ref

   #:unknown-mapping-class
   #:register-mapping-class
   #:delete-mapping-class
   #:find-mapping-class

   #:unknown-mapping
   #:register-mapping
   #:define-mapping
   #:delete-mapping
   #:find-mapping
   #:mapping
   #:mapping-error
   #:mapping-error-value
   #:mapping-error-pointer
   #:mapping-error-description
   #:add-mapping-error
   #:invalid-value
   #:invalid-value-value
   #:invalid-value-mapping-errors
   #:validate-value
   #:generate-value
   #:validate-child
   #:generate-child
   #:validate
   #:generate

   #:duplicate-key-handling
   #:*duplicate-key-handling*
   #:*max-depth*
   #:json-parse-error
   #:parse

   #:large-integer-representation
   #:*large-integer-representation*
   #:unserializable-value
   #:serialize))
