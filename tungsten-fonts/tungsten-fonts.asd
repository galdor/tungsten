(defsystem "tungsten-fonts"
  :description "Utilities to manipulate font files."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :pathname "src"
  :depends-on
  ("tungsten-core"
   "tungsten-system")
  :serial t
  :components
  ((:file "package")
   (:file "opentype")
   (:file "parser")
   (:file "parser-hhea")
   (:file "parser-name")
   (:file "parser-cmap"))
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
