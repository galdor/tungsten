(defsystem "tungsten-sqlite"
  :description "An interface for the SQLite embedded database library."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :defsystem-depends-on
  ("tungsten-asdf-utils"
   "tungsten-ffi")
  :depends-on
  ("tungsten-core")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   ("asdf-utils:shared-library"
    "sqlite"
    :source-files ("sqlite3.c")
    :header-files ("sqlite3.h")
    :cflags ("-DSQLITE_ENABLE_FTS5"
             "-DSQLITE_ENABLE_MATH_FUNCTIONS"))
   ("ffi-extractor:manifest"
    "ffi-manifest"
    :package :sqlite
    :disable-default-cflags t
    :cflags ())
   (:file "ffi"))
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
