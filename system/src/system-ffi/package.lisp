(defpackage :system-ffi
  (:use :cl)
  (:export
   #:size-t
   #:ssize-t
   #:errno
   #:socklen-t
   #:sa-family-t
   #:in-addr-t
   #:in-port-t
   #:socket-type
   #:socket-protocol
   #:address-family
   #:shutdown-type
   #:in-addr
   #:sockaddr-in
   #:in6-addr
   #:sockaddr-in6
   #:ai-error
   #:ai-flags
   #:ni-flags
   #:addrinfo
   #:time-t
   #:suseconds-t
   #:timeval

   #:system-error
   #:system-funcall

   #:close-fd
   #:read-fd
   #:write-fd
   #:socket
   #:shutdown
   #:with-getaddrinfo
   #:getnameinfo))
