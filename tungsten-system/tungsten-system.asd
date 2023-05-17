(defsystem "tungsten-system"
  :description "System programming utilities."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :defsystem-depends-on
  ("tungsten-ffi")
  :depends-on
  ("tungsten-core"
   "tungsten-ffi"
   "tungsten-text"
   "tungsten-streams")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   ("ffi-extractor:manifest"
    "ffi-base-manifest"
    :package :system)
   ("ffi-extractor:manifest"
    "ffi-manifest"
    :package :system)
   (:file "errors")
   (:file "ffi")
   (:file "concurrency-ccl" :if-feature :ccl)
   (:file "concurrency-sbcl" :if-feature :sbcl)
   (:file "concurrency")
   (:file "io-multiplexing")
   (:file "io-multiplexing-kqueue" :if-feature :bsd)
   (:file "io-multiplexing-epoll" :if-feature :linux)
   (:file "filesystem")
   (:file "ip-addresses")
   (:file "socket-addresses")
   (:file "io-streams")
   (:file "pipes")
   (:file "network-stream")
   (:file "tcp-stream")
   (:file "tcp-client")
   (:file "tcp-server")
   (:file "environment-linux" :if-feature :linux)
   (:file "environment"))
  :in-order-to ((test-op (test-op "tungsten-system/test"))))

(defsystem "tungsten-system/test"
  :description "Tests for the system system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-test"
   "tungsten-system")
  :pathname "t"
  :serial t
  :components
  ((:file "package")
   (:file "concurrency"))
  :perform (test-op (op system)
                    (symbol-call :test :run :package :system-test)))
