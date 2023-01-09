(defpackage :system
  (:use :cl)
  (:export
   #:system-error
   #:system-error-function
   #:system-error-value
   #:system-error-description

   #:size-t
   #:ssize-t

   #:mutex
   #:make-mutex
   #:acquire-mutex
   #:maybe-acquire-mutex
   #:release-mutex
   #:with-mutex

   #:thread
   #:current-thread
   #:list-threads
   #:make-thread
   #:join-thread

   #:ipv4-address
   #:ipv6-address
   #:ip-address
   #:format-ip-address

   #:host
   #:port-number
   #:format-host-and-port
   #:socket-address
   #:ip-socket-address
   #:ip-socket-address-port
   #:ipv4-socket-address
   #:ipv4-socket-address-address
   #:ipv6-socket-address
   #:ipv6-socket-address-address
   #:ipv6-socket-address-scope-id
   #:format-socket-address
   #:socket-address-equal

   #:write-io-stream
   #:read-io-stream
   #:io-stream
   #:io-stream-file-descriptor
   #:io-stream-address
   #:io-stream-read-buffer
   #:io-stream-write-buffer
   #:io-stream-external-format
   #:io-stream-read-more

   #:network-stream
   #:network-stream-address
   #:network-stream-read-timeout
   #:network-stream-write-timeout

   #:tcp-connection-failure
   #:tcp-connection-failure-host
   #:tcp-connection-failure-port
   #:tcp-connection-failure-address-errors
   #:tcp-client
   #:tcp-client-host
   #:tcp-client-port
   #:make-tcp-client

   #:tcp-acceptor
   #:make-tcp-acceptor
   #:close-tcp-acceptor))
