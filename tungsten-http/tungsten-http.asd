(defsystem "tungsten-http"
  :description "An implementation of the HTTP 1.1 protocol"
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-core"
   "tungsten-time"
   "tungsten-text"
   "tungsten-system"
   "tungsten-openssl"
   "tungsten-uri")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "http")
   (:file "statuses")
   (:file "request")
   (:file "response")
   (:file "client")
   (:file "server")
   (:file "routes")
   (:file "router"))
  :in-order-to ((test-op (test-op "tungsten-http/test"))))

(defsystem "tungsten-http/test"
  :description "Tests for the http system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-test"
   "tungsten-http")
  :pathname "t"
  :serial t
  :components
  ((:file "package")
   (:file "http")
   (:file "routes"))
  :perform (test-op (op system)
                    (symbol-call :test :run :package :http-test)))

(defsystem "tungsten-http/example"
  :description "Examples for the http system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-system"
   "tungsten-http")
  :pathname "example"
  :serial t
  :components
  ((:file "package")
   (:file "message-board")))
