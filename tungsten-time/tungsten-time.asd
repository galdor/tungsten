(defsystem "tungsten-time"
  :description "Time utilities."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :defsystem-depends-on
  ("tungsten-ffi")
  :depends-on
  ("tungsten-core"
   "tungsten-system")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   ("ffi-extractor:manifest"
    "ffi-base-manifest"
    :package :time)
   ("ffi-extractor:manifest"
    "ffi-manifest"
    :package :time)
   (:file "ffi")
   (:file "clocks")
   (:file "timestamp"))
  :in-order-to ((test-op (test-op "tungsten-time/test"))))

(defsystem "tungsten-time/test"
  :description "Tests for the time system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-test"
   "tungsten-time")
  :pathname "t"
  :serial t
  :components
  ((:file "package"))
  :perform (test-op (op system)
                    (symbol-call :test :run :package :time-test)))
