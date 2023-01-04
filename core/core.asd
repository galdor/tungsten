(defsystem "core"
  :description "Utilities used by various systems."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "sequences")
   (:file "binary")
   (:file "buffer")
   (:file "input")
   (:file "conditions"))
  :in-order-to ((test-op (test-op "core/test"))))

(defsystem "core/test"
  :description "Tests for the core system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("check" "core")
  :pathname "t"
  :serial t
  :components
  ((:file "package")
   (:file "sequences"))
  :perform (test-op (op system)
                    (symbol-call :check :run :package :core-test)))
