(defpackage :core
  (:use :cl)
  (:export
   #:unsupported-feature
   #:unsupported-feature-name
   #:endianness
   #:*endianness*
   #:*interactive*

   #:float-parse-error
   #:float-parse-error-string
   #:float-parse-error-description
   #:parse-float

   #:iota

   #:shuffle
   #:nshuffle

   #:alist-to-hash-table
   #:hash-table-keys
   #:hash-table-values
   #:hash-table-entries

   #:string-case

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

   #:abort-protect

   #:*backtrace-depth*
   #:*include-backtrace-source-files*
   #:frame
   #:frame-number
   #:frame-name
   #:frame-source-file
   #:format-frame
   #:format-backtrace
   #:backtrace))
