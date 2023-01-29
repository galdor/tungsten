(defsystem "test"
  :description "A simple test framework."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "test")
   (:file "tests")
   (:file "reporter")
   (:file "execution")
   (:file "text-reporter")
   (:file "assertions")
   (:file "systems"))
  :in-order-to ((test-op (test-op "test/test"))))

(defsystem "test/test"
  :description "Tests for the test system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("test")
  :pathname "t"
  :serial t
  :components
  ((:file "package")
   (:file "assertions"))
  :perform (test-op (op system)
                    (symbol-call :test :run :package :test-test)))
