(defpackage :http
  (:use :cl)
  (:export
   #:client
   #:make-client
   #:client-connections
   #:disconnect-client))
