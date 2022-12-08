(defpackage :uri
  (:use :cl)
  (:export
   :uri
   :uri-scheme
   :uri-username
   :uri-password
   :uri-host
   :uri-port
   :uri-path
   :uri-query
   :uri-fragment
   :make-uri
   :serialize))
