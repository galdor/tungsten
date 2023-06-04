(defpackage :system
  (:use :cl)
  (:export
   #:system-error
   #:system-error-function
   #:system-error-value
   #:system-error-description
   #:system-funcall

   #:size-t
   #:ssize-t
   #:intptr-t
   #:uintptr-t

   #:mutex
   #:make-mutex
   #:acquire-mutex
   #:maybe-acquire-mutex
   #:release-mutex
   #:with-mutex
   #:semaphore
   #:make-semaphore
   #:signal-semaphore
   #:wait-semaphore
   #:condition-variable
   #:make-condition-variable
   #:wait-condition-variable
   #:signal-condition-variable
   #:broadcast-condition-variable
   #:thread
   #:current-thread
   #:list-threads
   #:make-thread
   #:join-thread

   #:io-event
   #:io-base
   #:io-watcher
   #:io-watcher-fd
   #:io-watcher-registeredp
   #:io-watcher-events
   #:io-watcher-handler
   #:close-io-base
   #:make-io-base
   #:watch-fd
   #:unwatch-fd

   #:directory-path
   #:read-file

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

   #:io-stream-fd
   #:io-stream-address
   #:io-stream-read-buffer
   #:io-stream-write-buffer
   #:io-stream-external-format
   #:input-io-stream
   #:output-io-stream
   #:read-io-stream
   #:write-io-stream
   #:io-stream-read-more

   #:make-pipe

   #:network-stream
   #:network-stream-address
   #:network-stream-read-timeout
   #:network-stream-write-timeout

   #:read-event-required
   #:write-event-required
   #:tcp-timeout
   #:tcp-timeout-stream
   #:tcp-read-timeout
   #:tcp-write-timeout
   #:tcp-stream
   #:tcp-stream-non-blocking

   #:*default-tcp-client-read-timeout*
   #:*default-tcp-client-write-timeout*
   #:tcp-connection-failure
   #:tcp-connection-failure-host
   #:tcp-connection-failure-port
   #:tcp-connection-failure-address-errors
   #:tcp-client
   #:tcp-client-host
   #:tcp-client-port
   #:make-tcp-client

   #:tcp-server
   #:tcp-server-io-base
   #:start-tcp-server
   #:stop-tcp-server

   #:hostname
   #:count-file-descriptors
   #:file-descriptor-limit
   #:page-size
   #:memory-usage
   #:environment-variable
   #:home-directory-path))
