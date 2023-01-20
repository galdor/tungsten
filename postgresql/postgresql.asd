(defsystem "postgresql"
  :description "A client for the PostgreSQL database."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :pathname "src"
  :serial t
  :components
  ((:file "package"))
  :in-order-to ((test-op (test-op "postgresql/test"))))

(defsystem "postgresql/test"
  :description "Tests for the postgresql system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("check" "postgresql")
  :pathname "t"
  :serial t
  :components
  ((:file "package"))
  :perform (test-op (op system)
                    (symbol-call :check :run :package :postgresql-test)))
