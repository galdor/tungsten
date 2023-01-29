(defsystem "postgresql"
  :description "A client for the PostgreSQL database."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("core" "text" "system" "streams" "openssl")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "protocol")
   (:file "codecs")
   (:file "scram")
   (:file "client"))
  :in-order-to ((test-op (test-op "postgresql/test"))))

(defsystem "postgresql/test"
  :description "Tests for the postgresql system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("test" "postgresql")
  :pathname "t"
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "scram")
   (:file "codecs"))
  :perform (test-op (op system)
                    (symbol-call :test :run :package :postgresql-test)))
