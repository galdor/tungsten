(defsystem "tungsten-streams"
  :description "Stream utilities."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-core"
   "tungsten-text")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "fundamental-stream-definitions")
   (:file "fundamental-streams")
   (:file "octet-output-stream"))
  :in-order-to ((test-op (test-op "tungsten-streams/test"))))

(defsystem "tungsten-streams/test"
  :description "Tests for the streams system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-test"
   "tungsten-streams")
  :pathname "t"
  :serial t
  :components
  ((:file "package"))
  :perform (test-op (op system)
                    (symbol-call :test :run :package :streams-test)))
