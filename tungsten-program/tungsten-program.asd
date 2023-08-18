(defsystem "tungsten-program"
  :description "Utilities to write standalone programs."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :pathname "src"
  :depends-on
  ("tungsten-core")
  :serial t
  :components
  ((:file "package")
   (:file "command-line")
   (:file "program"))
  :in-order-to ((test-op (test-op "tungsten-program/test"))))

(defsystem "tungsten-program/test"
  :description "Tests for the program system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-test"
   "tungsten-program")
  :pathname "t"
  :serial t
  :components
  ((:file "package"))
  :perform (test-op (op system)
                    (symbol-call :test :run :package :program-test)))
