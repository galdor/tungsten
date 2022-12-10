(defpackage :system-ffi
  (:use :cl)
  (:export
   #:size-t
   #:ssize-t

   #:errno
   #:system-error
   #:system-funcall

   #:time-t
   #:suseconds-t
   #:timeval))
