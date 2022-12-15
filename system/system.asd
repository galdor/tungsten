(defsystem "system"
  :description "System programming utilities."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :defsystem-depends-on ("ffi")
  :depends-on ("core" "ffi")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   ("ffi-extractor:manifest"
    "ffi-manifest"
    :package :system)
   (:file "errors")
   (:file "ffi")
   (:file "ip-addresses")
   (:file "socket-addresses")
   (:file "tcp-client"))
  :in-order-to ((test-op (test-op "system/test"))))

(defsystem "system/test"
  :description "Tests for the system system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("check" "system")
  :pathname "t"
  :serial t
  :components
  ((:file "package"))
  :perform (test-op (op system)
                    (symbol-call :check :run :package :system-test)))
