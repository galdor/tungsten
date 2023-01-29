(defsystem "tungsten-uri"
  :description "Utilities to manipulate Universal Resource Identifiers."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-text")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "encoding")
   (:file "uri")
   (:file "serialization")
   (:file "parser")
   (:file "references"))
  :in-order-to ((test-op (test-op "tungsten-uri/test"))))

(defsystem "tungsten-uri/test"
  :description "Tests for the uri system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-test"
   "tungsten-uri")
  :pathname "t"
  :serial t
  :components
  ((:file "package")
   (:file "encoding")
   (:file "serialization")
   (:file "parser")
   (:file "references"))
  :perform (test-op (op system)
                    (symbol-call :test :run :package :uri-test)))
