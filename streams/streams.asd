(defsystem "streams"
  :description "Stream utilities."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("core" "text")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "fundamental-stream-definitions")
   (:file "fundamental-streams")
   (:file "octet-output-stream"))
  :in-order-to ((test-op (test-op "streams/test"))))

(defsystem "streams/test"
  :description "Tests for the streams system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("test" "streams")
  :pathname "t"
  :serial t
  :components
  ((:file "package"))
  :perform (test-op (op system)
                    (symbol-call :test :run :package :streams-test)))
