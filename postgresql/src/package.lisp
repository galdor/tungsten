(defpackage :postgresql
  (:use :cl)
  (:export
   #:protocol-error
   #:backend-error
   #:backend-error-l10n-severity
   #:backend-error-severity
   #:backend-error-code
   #:backend-error-message
   #:backend-error-detail
   #:backend-error-position
   #:backend-error-internal-position
   #:backend-error-internal-query
   #:backend-error-where
   #:backend-error-schema
   #:backend-error-table
   #:backend-error-column
   #:backend-error-data-type
   #:backend-error-constraint
   #:backend-error-file
   #:backend-error-line
   #:backend-error-routine

   #:missing-password
   #:unsupported-authentication-scheme
   #:unsupported-authentication-scheme-name
   #:unexpected-message
   #:unexpected-message-message
   #:client
   #:client-stream
   #:make-client
   #:close-client
   #:with-client))
