(defsystem "ffi"
  :description "A foreign function interface."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("text")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "ffi-ccl" :if-feature :ccl)
   (:file "ffi-sbcl" :if-feature :sbcl)
   (:file "libraries")
   (:file "ffi")))
