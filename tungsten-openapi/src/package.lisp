(defpackage :openapi
  (:use :cl)
  (:export
   #:document
   #:document-openapi-version
   #:document-title
   #:document-version
   #:document-servers
   #:document-operations
   #:document-operation
   #:parse-document))
