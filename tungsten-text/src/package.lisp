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
   #:encodings

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
   #:eol-string

   #:invalid-utf8-leading-octet
   #:invalid-utf8-continuation-octet
   #:truncated-utf8-sequence
   #:overlong-utf8-sequence
   #:invalid-utf8-sequence

   #:truncated-utf16-character
   #:truncated-utf16-surrogate-pair

   #:invalid-hex-digit
   #:invalid-hex-digit-character
   #:invalid-hex-string
   #:invalid-hex-string-string
   #:encode-hex-string
   #:decode-hex-string

   #:invalid-base64-character
   #:invalid-base64-character-character
   #:encode-base64
   #:decode-base64))
