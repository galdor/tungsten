(defsystem "tungsten-sqlite"
  :description "An interface for the SQLite embedded database library."
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
    "ffi-manifest"
    :package :sqlite)
   (:file "ffi")
   (:file "sqlite"))
  :in-order-to ((test-op (test-op "tungsten-sqlite/test"))))

(defsystem "tungsten-sqlite/test"
  :description "Tests for the sqlite system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-test"
   "tungsten-sqlite")
  :pathname "t"
  :serial t
  :components
  ((:file "package"))
  :perform (test-op (op system)
                    (symbol-call :test :run :package :sqlite-test)))
