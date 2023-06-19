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
   #:serialize-group

   #:header
   #:body
   #:message
   #:message-header
   #:message-body
   #:make-message))
