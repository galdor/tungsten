(defpackage :postgresql
  (:use :cl)
  (:export
   #:client
   #:client-stream
   #:make-client
   #:close-client))
