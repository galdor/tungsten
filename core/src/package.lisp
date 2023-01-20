(defpackage :core
  (:use :cl)
  (:export
   #:*interactive*

   #:shuffle
   #:nshuffle

   #:alist-to-hash-table

   #:octet
   #:make-octet-vector
   #:octet-vector
   #:octet-vector*

   #:unknown-binary-type
   #:out-of-bounds-binary-access
   #:binref
   #:binref-type-size

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
