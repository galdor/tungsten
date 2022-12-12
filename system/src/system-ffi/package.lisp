(defpackage :system-ffi
  (:use :cl)
  (:export
   #:size-t
   #:ssize-t
   #:errno
   #:time-t
   #:suseconds-t
   #:timeval

   #:system-error
   #:system-funcall

   #:clear-memory

   #:close-fd
   #:read-fd
   #:write-fd
   #:socket
   #:shutdown))
