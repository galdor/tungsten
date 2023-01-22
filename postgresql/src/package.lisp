(defpackage :postgresql
  (:use :cl)
  (:export
   #:protocol-error

   #:authentication-error
   #:client
   #:client-stream
   #:make-client
   #:close-client))
