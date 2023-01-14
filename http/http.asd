(defsystem "http"
  :description "An implementation of the HTTP 1.1 protocol"
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("system" "openssl" "uri")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "time")
   (:file "http")
   (:file "statuses")
   (:file "request")
   (:file "response")
   (:file "client")
   (:file "server"))
  :in-order-to ((test-op (test-op "http/test"))))

(defsystem "http/test"
  :description "Tests for the http system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("check" "http")
  :pathname "t"
  :serial t
  :components
  ((:file "package")
   (:file "http"))
  :perform (test-op (op system)
                    (symbol-call :check :run :package :http-test)))
