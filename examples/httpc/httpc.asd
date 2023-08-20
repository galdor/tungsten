(defsystem "httpc"
  :description "An HTTP client."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :pathname "src"
  :depends-on
  ("tungsten-http"
   "tungsten-program")
  :serial t
  :components
  ((:file "package")
   (:file "httpc")))
