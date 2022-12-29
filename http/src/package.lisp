(defpackage :http
  (:use :cl)
  (:export
   #:request-method
   #:request-target
   #:protocol-version
   #:body
   #:header
   #:header-field-name
   #:header-field

   #:request
   #:request-method
   #:request-version
   #:request-header
   #:request-body
   #:request-header-field

   #:client
   #:make-client
   #:client-connections
   #:disconnect-client))
