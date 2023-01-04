(defpackage :http
  (:use :cl)
  (:export
   #:request-method
   #:response-status
   #:protocol-version
   #:body
   #:header
   #:missing-request-target-host
   #:header-field
   #:header-field-value
   #:header-field-values
   #:header-field-tokens

   #:request
   #:request-method
   #:request-version
   #:request-header
   #:request-body
   #:request-header-field

   #:response
   #:response-status
   #:response-reason
   #:response-version
   #:response-header
   #:response-body

   #:client
   #:make-client
   #:client-connections
   #:disconnect-client
   #:*client*
   #:send-request))
