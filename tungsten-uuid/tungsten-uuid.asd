(defsystem "tungsten-uuid"
  :description "Utilities for UUID unique identifiers."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-core")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "uuid"))
  :in-order-to ((test-op (test-op "tungsten-uuid/test"))))

(defsystem "tungsten-uuid/test"
  :description "Tests for the tungsten-uuid system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-test"
   "tungsten-uuid")
  :pathname "t"
  :serial t
  :components
  ((:file "package")
   (:file "uuid"))
  :perform (test-op (op system)
                    (symbol-call :test :run :package :uuid-test)))
