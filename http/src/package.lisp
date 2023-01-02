(defpackage :http
  (:use :cl)
  (:export
   #:request-method
   #:request-target
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

   #:client
   #:make-client
   #:client-connections
   #:disconnect-client
   #:*client*
   #:send-request))
