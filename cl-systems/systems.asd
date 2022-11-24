(defsystem "systems"
  :description "Utilities to work with ASDF systems."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("ffi")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "systems")
   (:file "shared-library")))
