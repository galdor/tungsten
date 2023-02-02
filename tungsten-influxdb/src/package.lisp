(defpackage :influxdb
  (:use :cl)
  (:export
   #:request-failure

   #:*client*
   #:client
   #:start-client
   #:stop-client
   #:enqueue-point
   #:enqueue-points))
