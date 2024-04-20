(defpackage :imf
  (:use :cl)
  (:export
   #:mailbox
   #:mailbox-display-name
   #:mailbox-local-part
   #:mailbox-domain
   #:group
   #:group-display-name
   #:group-mailboxes
   #:address
   #:make-mailbox
   #:make-group
   #:serialize-address
   #:serialize-mailbox

   #:header
   #:header-field
   #:header-fields
   #:body
   #:message
   #:message-header
   #:message-body
   #:message-field
   #:message-fields
   #:make-message
   #:write-message))
