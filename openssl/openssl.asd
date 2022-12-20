(defsystem "openssl"
  :description "An interface for the OpenSSL library."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :defsystem-depends-on ("ffi")
  :depends-on ("system")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   ("ffi-extractor:manifest"
    "ffi-manifest"
    :package :openssl)
   (:file "ffi")
   (:file "tls-stream")
   (:file "tls-client"))
  :in-order-to ((test-op (test-op "openssl/test"))))

(defsystem "openssl/test"
  :description "Tests for the openssl system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("check" "openssl")
  :pathname "t"
  :serial t
  :components
  ((:file "package"))
  :perform (test-op (op system)
                    (symbol-call :check :run :package :openssl-test)))
