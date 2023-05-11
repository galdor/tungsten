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
   #:parse-document

   #:*server-uri*
   #:invalid-path-template
   #:missing-parameter-value
   #:missing-request-body
   #:unexpected-response-status
   #:unexpected-response-content-type
   #:execute-operation))
