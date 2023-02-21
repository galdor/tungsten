(defsystem "tungsten-netrc"
  :description "Parser for netrc credential files."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-core"
   "tungsten-system")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "entries")
   (:file "parser")
   (:file "netrc"))
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
  ((:file "package")
   (:file "parser"))
  :perform
  (test-op (op system) (symbol-call :test :run :package :netrc-test)))
