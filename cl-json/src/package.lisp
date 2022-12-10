(defpackage :json
  (:use :cl)
  (:export
   #:duplicate-key-handling
   #:*duplicate-key-handling*
   #:*max-depth*
   #:json-parse-error
   #:parse))
