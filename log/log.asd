(defsystem "log"
  :description "Utilities for flexible logging."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :pathname "src"
  :serial t
  :components
  ((:file "package"))
  :in-order-to ((test-op (test-op "log/test"))))

(defsystem "log/test"
  :description "Tests for the log system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("check" "log")
  :pathname "t"
  :serial t
  :components
  ((:file "package"))
  :perform (test-op (op system)
                    (symbol-call :check :run :package :log-test)))
