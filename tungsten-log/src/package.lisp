(defpackage :log
  (:use :cl)
  (:export
   #:level
   #:domain-part
   #:domain
   #:message
   #:message-time
   #:message-domain
   #:message-level
   #:message-text
   #:message-data
   #:message-writer

   #:sink
   #:write-message
   #:default-sink

   #:make-terminal-sink

   #:*logger*
   #:logger
   #:logger-domain
   #:logger-data
   #:logger-message-writer
   #:logger-stream
   #:make-logger
   #:with-logger
   #:log-message
   #:log-debug
   #:log-debug-data
   #:log-info
   #:log-info-data
   #:log-error
   #:log-error-data))
