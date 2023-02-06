(defsystem "tungsten-openapi"
  :description "Utilities for the OpenAPI interface definition language."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-core"
   "tungsten-json")
  :pathname "src"
  :serial t
  :components
  ((:file "package"))
  :in-order-to ((test-op (test-op "tungsten-openapi/test"))))

(defsystem "tungsten-openapi/test"
  :description "Tests for the openapi system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-test")
  :pathname "t"
  :serial t
  :components
  ((:file "package"))
  :perform (test-op (op system)
                    (symbol-call :test :run :package :openapi-test)))
