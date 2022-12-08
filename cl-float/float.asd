(defsystem "float"
  :description "Utilities to manipulate IEEE.754 floating point numbers."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :pathname "src"
  :serial t
  :components
  ((:file "package"))
  :in-order-to ((test-op (test-op "float/test"))))

(defsystem "float/test"
  :description "Tests for the float system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("check" "float")
  :pathname "t"
  :serial t
  :components
  ((:file "package"))
  :perform (test-op (op system)
                    (symbol-call :check :run :package :float-test)))
