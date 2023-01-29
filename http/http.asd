(defsystem "http"
  :description "An implementation of the HTTP 1.1 protocol"
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("core" "text" "system" "openssl" "uri")
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
   (:file "server")
   (:file "routes")
   (:file "router"))
  :in-order-to ((test-op (test-op "http/test"))))

(defsystem "http/test"
  :description "Tests for the http system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("test" "http")
  :pathname "t"
  :serial t
  :components
  ((:file "package")
   (:file "http")
   (:file "routes"))
  :perform (test-op (op system)
                    (symbol-call :test :run :package :http-test)))

(defsystem "http/example"
  :description "Examples for the http system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("system" "http")
  :pathname "example"
  :serial t
  :components
  ((:file "package")
   (:file "message-board")))
