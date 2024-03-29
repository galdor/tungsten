(defsystem "tungsten-influxdb"
  :description "A client for the InfluxDB time serie database."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-core"
   "tungsten-http"
   "tungsten-log"
   "tungsten-system"
   "tungsten-text"
   "tungsten-time")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "influxdb")
   (:file "line-protocol")
   (:file "transport")
   (:file "builtin-metrics")
   (:file "client"))
  :in-order-to ((test-op (test-op "tungsten-influxdb/test"))))

(defsystem "tungsten-influxdb/test"
  :description "Tests for the influxdb system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-test"
   "tungsten-influxdb")
  :pathname "t"
  :serial t
  :components
  ((:file "package"))
  :perform (test-op (op system)
                    (symbol-call :test :run :package :influxdb-test)))
