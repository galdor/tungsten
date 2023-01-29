(defsystem "tungsten-postgresql"
  :description "A client for the PostgreSQL database."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-core"
   "tungsten-text"
   "tungsten-system"
   "tungsten-streams"
   "tungsten-openssl")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "protocol")
   (:file "codecs")
   (:file "scram")
   (:file "client"))
  :in-order-to ((test-op (test-op "tungsten-postgresql/test"))))

(defsystem "tungsten-postgresql/test"
  :description "Tests for the postgresql system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-test"
   "tungsten-postgresql")
  :pathname "t"
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "scram")
   (:file "codecs"))
  :perform (test-op (op system)
                    (symbol-call :test :run :package :postgresql-test)))
