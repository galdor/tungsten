(defpackage :core
  (:use :cl)
  (:export
   #:shuffle
   #:nshuffle

   #:octet
   #:octet-vector
   #:octet-vector*

   #:buffer
   #:buffer-data
   #:buffer-start
   #:buffer-end
   #:make-buffer
   #:buffer-empty-p
   #:buffer-append-octet
   #:buffer-append-octets
   #:buffer-reset
   #:buffer-skip))
