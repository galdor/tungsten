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

   #:codec-type
   #:oid
   #:unknown-codec
   #:unknown-codec-type
   #:unknown-codec-oid
   #:unencodable-value
   #:unencodable-value-value
   #:value-decoding-error
   #:value-decoding-error-octets
   #:codec
   #:codec-type
   #:codec-oid
   #:codec-encoding-function
   #:codec-decoding-function
   #:find-codec
   #:register-codec
   #:delete-codec
   #:encode-value
   #:decode-value

   #:*client*
   #:missing-password
   #:unsupported-authentication-scheme
   #:unsupported-authentication-scheme-name
   #:unexpected-message
   #:unexpected-message-message
   #:client
   #:client-backend-process-id
   #:client-backend-secret-key
   #:client-stream
   #:make-client
   #:close-client
   #:with-client
   #:query/simple))
