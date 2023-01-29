(defsystem "tungsten-json"
  :description "An implementation of the JSON serialization format."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-core")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "parser")
   (:file "serialization"))
  :in-order-to ((test-op (test-op "tungsten-json/test"))))

(defsystem "tungsten-json/test"
  :description "Tests for the json system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-test"
   "tungsten-json")
  :pathname "t"
  :serial t
  :components
  ((:file "package")
   (:file "parser"))
  :perform (test-op (op system)
                    (symbol-call :test :run :package :json-test)))
