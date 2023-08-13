(defsystem "tungsten-libssh"
  :description "An interface for the Libssh library."
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
    :package :libssh
    :cflags (#+freebsd "-I/usr/local/include"))
   (:file "ffi")
   (:file "sessions")
   (:file "client"))
  :in-order-to ((test-op (test-op "tungsten-libssh/test"))))

(defsystem "tungsten-libssh/test"
  :description "Tests for the libssh system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-test"
   "tungsten-libssh")
  :pathname "t"
  :serial t
  :components
  ((:file "package"))
  :perform (test-op (op system)
                    (symbol-call :test :run :package :libssh-test)))
