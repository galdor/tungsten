(defpackage :uuid
  (:use :cl)
  (:export
   #:version
   #:uuid
   #:uuid-octets
   #:uuid-equal
   #:generate
   #:invalid-format
   #:parse
   #:serialize))
