(defsystem "system"
  :description "System programming utilities."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :defsystem-depends-on ("ffi")
  :depends-on ("core" "ffi" "text" "streams")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   ("ffi-extractor:manifest"
    "ffi-manifest"
    :package :system)
   (:file "errors")
   (:file "ffi")
   (:file "concurrency-ccl" :if-feature :ccl)
   (:file "concurrency-sbcl" :if-feature :sbcl)
   (:file "concurrency")
   (:file "io-multiplexing")
   (:file "io-multiplexing-epoll" :if-feature :linux)
   (:file "filesystem")
   (:file "ip-addresses")
   (:file "socket-addresses")
   (:file "io-stream")
   (:file "network-stream")
   (:file "tcp-stream")
   (:file "tcp-client")
   (:file "tcp-server"))
  :in-order-to ((test-op (test-op "system/test"))))

(defsystem "system/test"
  :description "Tests for the system system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("check" "system")
  :pathname "t"
  :serial t
  :components
  ((:file "package")
   (:file "concurrency"))
  :perform (test-op (op system)
                    (symbol-call :check :run :package :system-test)))
