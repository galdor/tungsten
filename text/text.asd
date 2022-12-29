(defsystem "text"
  :description "Textual data manipulation."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :pathname "src"
  :depends-on ("core")
  :serial t
  :components
  ((:file "package")
   (:file "base")
   (:file "encodings")
   (:file "strings")
   (:file "ascii")
   (:file "utf-8"))
  :in-order-to ((test-op (test-op "text/test"))))

(defsystem "text/test"
  :description "Tests for the text system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("check" "text")
  :pathname "t"
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "ascii")
   (:file "utf-8"))
  :perform (test-op (op system)
                    (symbol-call :check :run :package :text-test)))
