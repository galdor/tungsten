(defsystem "tungsten-mime"
  :description "Utilities for Multipurpose Internet Mail Extensions."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-core"
   "tungsten-json")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "media-types")
   (:file "media-ranges")
   (:file "json"))
  :in-order-to ((test-op (test-op "tungsten-mime/test"))))

(defsystem "tungsten-mime/test"
  :description "Tests for the tungsten-mime system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-test"
   "tungsten-mime")
  :pathname "t"
  :serial t
  :components
  ((:file "package")
   (:file "media-types")
   (:file "media-ranges"))
  :perform
  (test-op (op system) (symbol-call :test :run :package :mime-test)))
