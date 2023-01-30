(defpackage :time
  (:use :cl)
  (:export
   #:clock
   #:current-clock-timestamp
   #:wall-clock
   #:monotonic-clock
   #:*clock*
   #:with-clock

   #:timestamp
   #:current-timestamp
   #:timestamp-delta
   #:time-since))
