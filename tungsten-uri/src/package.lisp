(defpackage :uri
  (:use :cl)
  (:export
   #:percent-decoding-error
   #:truncated-percent-sequence
   #:invalid-percent-sequence-hex-digit
   #:percent-decode
   #:percent-encode

   #:uri
   #:uri-scheme
   #:uri-username
   #:uri-password
   #:uri-host
   #:uri-port
   #:uri-path
   #:uri-query
   #:uri-fragment
   #:make-uri
   #:copy-uri
   #:uri-absolute-path-p
   #:absolute-path-p
   #:uri-path-segments
   #:path-segments

   #:serialize

   #:uri-parse-error
   #:truncated-host
   #:invalid-host
   #:invalid-port
   #:parse

   #:missing-base-uri-scheme
   #:resolve-reference))
