(defpackage :html
  (:use :cl)
  (:export
   #:*html-output*
   #:invalid-generation-data
   #:with-html
   #:html
   #:escape-text-element
   #:escape-attribute))
