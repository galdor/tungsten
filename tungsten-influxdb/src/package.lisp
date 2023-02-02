(defpackage :influxdb
  (:use :cl)
  (:export
   #:request-failure

   #:*builtin-metrics-collection-interval*

   #:*client*
   #:client
   #:start-client
   #:stop-client
   #:enqueue-point
   #:enqueue-points))
