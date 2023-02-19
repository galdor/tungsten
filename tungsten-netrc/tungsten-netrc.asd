(defsystem "tungsten-netrc"
  :description "Parser for netrc credential files."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-core")
  :pathname "src"
  :serial t
  :components
  ((:file "package"))
  :in-order-to
  ((test-op (test-op "tungsten-netrc/test"))))

(defsystem "tungsten-netrc/test"
  :description "Tests for the tungsten-netrc system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-test"
   "tungsten-netrc")
  :pathname "t"
  :serial t
  :components
  ((:file "package"))
  :perform
  (test-op (op system) (symbol-call :test :run :package :netrc-test)))
