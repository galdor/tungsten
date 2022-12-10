(defsystem "system"
  :description "System programming utilities."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :defsystem-depends-on ("ffi")
  :depends-on ("ffi")
  :pathname "src"
  :serial t
  :components
  ((:module
     "system-ffi"
     :serial t
     :components
     ((:file "package")
      ("ffi-extractor:manifest"
       "ffi-manifest"
       :package :system-ffi)
      (:file "errors")))
   (:module
    "system"
    :serial t
    :components
    ((:file "package")
     (:file "ip-addresses"))))
  :in-order-to ((test-op (test-op "system/test"))))

(defsystem "system/test"
  :description "Tests for the system system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("check" "system")
  :pathname "t"
  :serial t
  :components
  ((:file "package"))
  :perform (test-op (op system)
                    (symbol-call :check :run :package :system-test)))
