(defsystem "tungsten-html"
  :description "Utilities for the HTML markup language."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-core")
  :pathname "src"
  :serial t
  :components
  ((:file "package"))
  :in-order-to
  ((test-op (test-op "tungsten-html/test"))))

(defsystem "tungsten-html/test"
  :description "Tests for the tungsten-html system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-test"
    "tungsten-html")
  :pathname "t"
  :serial t
  :components
  ((:file "package"))
  :perform
  (test-op (op system) (symbol-call :test :run :package :html-test)))
