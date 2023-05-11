(defpackage :json
  (:use :cl)
  (:export
   #:pointer
   #:child-pointer
   #:pointer*
   #:pointer-parse-error
   #:pointer-parse-error-format-control
   #:pointer-parse-error-format-arguments
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
   #:delete-package-mappings
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

   #:any-mapping
   #:boolean-mapping
   #:number-mapping
   #:integer-mapping
   #:string-mapping
   #:array-mapping
   #:object-mapping
   #:or-mapping

   #:duplicate-key-handling
   #:*duplicate-key-handling*
   #:*max-depth*
   #:json-parse-error
   #:parse

   #:large-integer-representation
   #:*large-integer-representation*
   #:unserializable-value
   #:serialize))
