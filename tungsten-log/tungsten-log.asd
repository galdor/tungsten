(defsystem "tungsten-log"
  :description "Utilities for flexible logging."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-core"
   "tungsten-system")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "logging")
   (:file "message")
   (:file "sink")
   (:file "terminal-sink")
   (:file "logger"))
  :in-order-to ((test-op (test-op "tungsten-log/test"))))

(defsystem "tungsten-log/test"
  :description "Tests for the log system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-test"
   "tungsten-log")
  :pathname "t"
  :serial t
  :components
  ((:file "package"))
  :perform (test-op (op system)
                    (symbol-call :test :run :package :log-test)))
