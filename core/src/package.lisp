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
   #:buffer-capacity
   #:buffer-length
   #:buffer-content
   #:buffer-empty-p
   #:buffer-append-octet
   #:buffer-append-octets
   #:buffer-reserve
   #:buffer-reserve-start
   #:buffer-reset
   #:buffer-skip
   #:buffer-skip-to

   #:prompt-eval

   #:unsupported-feature
   #:unsupported-feature-name
   #:abort-protect))
