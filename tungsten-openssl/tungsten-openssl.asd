(defsystem "tungsten-openssl"
  :description "An interface for the OpenSSL library."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :defsystem-depends-on
  ("tungsten-ffi")
  :depends-on
  ("tungsten-core"
   "tungsten-system"
   "tungsten-text")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   ("ffi-extractor:manifest"
    "ffi-manifest"
    :package :openssl)
   (:file "ffi")
   (:file "utils")
   (:file "tls-stream")
   (:file "tls-client")
   (:file "parameters")
   (:file "random")
   (:file "digests")
   (:file "hmacs")
   (:file "pbkdf2"))
  :in-order-to ((test-op (test-op "tungsten-openssl/test"))))

(defsystem "tungsten-openssl/test"
  :description "Tests for the openssl system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-test"
   "tungsten-openssl")
  :pathname "t"
  :serial t
  :components
  ((:file "package")
   (:file "digests")
   (:file "hmacs")
   (:file "pbkdf2"))
  :perform (test-op (op system)
                    (symbol-call :test :run :package :openssl-test)))
