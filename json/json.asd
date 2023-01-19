(defsystem "json"
  :description "An implementation of the JSON serialization format"
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("float")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "parser")
   (:file "serialization"))
  :in-order-to ((test-op (test-op "json/test"))))

(defsystem "json/test"
  :description "Tests for the json system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("check" "json")
  :pathname "t"
  :serial t
  :components
  ((:file "package")
   (:file "parser"))
  :perform (test-op (op system)
                    (symbol-call :check :run :package :json-test)))
