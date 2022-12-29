(defpackage :text
  (:use :cl)
  (:export
   #:*default-encoding*
   #:unencodable-character
   #:decoding-error
   #:encoding
   #:encoding-name
   #:encoding-encoding-function
   #:encoding-decoding-function
   #:register-encoding
   #:unregister-encoding
   #:define-encoding

   #:encoded-string-length
   #:encode-string
   #:decoded-string-length
   #:decode-string

   #:invalid-ascii-octet

   #:invalid-utf8-leading-octet
   #:invalid-utf8-continuation-octet
   #:truncated-utf8-sequence
   #:overlong-utf8-sequence
   #:invalid-utf8-sequence))
