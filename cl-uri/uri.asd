(defsystem "uri"
  :description "Utilities to manipulate Universal Resource Identifiers."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "uri"))
  :in-order-to ((test-op (test-op "uri/test"))))

(defsystem "uri/test"
  :description "Tests for the uri system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("check" "uri")
  :pathname "t"
  :serial t
  :components
  ((:file "package"))
  :perform (test-op (op system)
                    (symbol-call :check :run :package :uri-test)))
