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

   #:serialize

   #:uri-parse-error
   #:truncated-host
   #:invalid-host
   #:invalid-port
   #:parse

   #:missing-base-uri-scheme
   #:resolve-reference))
