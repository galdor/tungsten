(defpackage :text
  (:use :cl)
  (:export
   :encoding
   :encoding-name
   :encoding-encoding-function
   :encoding-decoding-function
   :register-encoding
   :unregister-encoding
   :define-encoding

   :encode-string
   :decode-string

   :utf8-decoding-error
   :invalid-utf8-leading-byte
   :invalid-utf8-continuation-byte
   :truncated-utf8-sequence
   :overlong-utf8-sequence
   :invalid-utf8-sequence))
