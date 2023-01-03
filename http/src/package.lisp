(defpackage :http
  (:use :cl)
  (:export
   #:request-method
   #:request-target
   #:response-status
   #:protocol-version
   #:body
   #:header
   #:missing-request-target-host
   #:header-field-name
   #:header-field-value
   #:header-field

   #:request
   #:request-method
   #:request-version
   #:request-header
   #:request-body
   #:make-request
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
