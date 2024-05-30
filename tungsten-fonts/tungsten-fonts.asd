(defsystem "tungsten-fonts"
  :description "Utilities to manipulate font files."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :pathname "src"
  :depends-on
  ("tungsten-core"
   "tungsten-streams"
   "tungsten-system"
   "tungsten-text")
  :serial t
  :components
  ((:file "package")
   (:file "sfnt")
   (:file "font"))
  :in-order-to ((test-op (test-op "tungsten-fonts/test"))))

(defsystem "tungsten-fonts/test"
  :description "Tests for the fonts system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-test"
   "tungsten-fonts")
  :pathname "t"
  :serial t
  :components
  ((:file "package"))
  :perform (test-op (op system)
                    (symbol-call :test :run :package :fonts-test)))
