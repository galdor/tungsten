(defpackage :text
  (:use :cl)
  (:export
   #:*default-encoding*
   #:unencodable-character
   #:invalid-octet
   #:decoding-error
   #:encoding
   #:encoding-name
   #:encoding-encoding-function
   #:encoding-decoding-function
   #:register-encoding
   #:unregister-encoding
   #:define-encoding

   #:encoded-character-length
   #:encoded-string-length
   #:encode-string
   #:decoded-string-length
   #:decode-character
   #:decode-string

   #:*default-eol-style*
   #:*default-external-format*
   #:eol-style
   #:external-format
   #:external-format-encoding
   #:external-format-eol-style
   #:eol-octets

   #:invalid-utf8-leading-octet
   #:invalid-utf8-continuation-octet
   #:truncated-utf8-sequence
   #:overlong-utf8-sequence
   #:invalid-utf8-sequence))
