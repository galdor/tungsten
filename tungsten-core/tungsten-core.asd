(defsystem "tungsten-core"
  :description "Utilities used by various systems."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "environment")
   (:file "floats")
   (:file "maths")
   (:file "sequences")
   (:file "hash-tables")
   (:file "strings")
   (:file "octets")
   (:file "binary")
   (:file "buffer")
   (:file "random")
   (:file "input")
   (:file "conditions")
   (:file "backtrace")
   (:file "macros"))
  :in-order-to ((test-op (test-op "tungsten-core/test"))))

(defsystem "tungsten-core/test"
  :description "Tests for the tungsten-core system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-test"
   "tungsten-core")
  :pathname "t"
  :serial t
  :components
  ((:file "package")
   (:file "floats")
   (:file "sequences")
   (:file "binary"))
  :perform (test-op (op system)
                    (symbol-call :test :run :package :core-test)))
