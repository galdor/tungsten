(defsystem "tungsten-email"
  :description "Utilities to work with various email protocols."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-core"
   "tungsten-text"
   "tungsten-system")
  :pathname "src"
  :serial t
  :components
  ((:module
    "imf"
    :serial t
    :components
    ((:file "package")
     (:file "coding")
     (:file "addresses")
     (:file "message"))))
  :in-order-to ((test-op (test-op "tungsten-email/test"))))

(defsystem "tungsten-email/test"
  :description "Tests for the tungsten-email system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-test"
   "tungsten-email")
  :pathname "t"
  :serial t
  :components
  ((:file "package"))
  :perform (test-op (op system)
                    (symbol-call :test :run :package :email-test)))
