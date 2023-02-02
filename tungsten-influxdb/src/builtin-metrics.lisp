(in-package :influxdb)

(defparameter *builtin-metrics-collection-interval* 10.0
  "The time between collections of builtin metrics in seconds.")

(defun collect-builtin-metrics ()
  (let ((nb-threads (length (system:list-threads))))
    (list (make-point "tungsten.lisp.nb_threads"
                      :fields `(("count" . ,nb-threads))))))
