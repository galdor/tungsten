(defsystem "streams"
  :description "Stream utilities."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "fundamental-streams"))
  :in-order-to ((test-op (test-op "streams/test"))))

(defsystem "streams/test"
  :description "Tests for the streams system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("check" "streams")
  :pathname "t"
  :serial t
  :components
  ((:file "package"))
  :perform (test-op (op system)
                    (symbol-call :check :run :package :streams-test)))
