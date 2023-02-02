(in-package :influxdb)

(defparameter *builtin-metrics-collection-interval* 10.0
  "The time between collections of builtin metrics in seconds.")

(defun collect-builtin-metrics ()
  (append (collect-builtin-metrics/threads)
          (collect-builtin-metrics/memory)
          (collect-builtin-metrics/file-descriptors)))

(defun collect-builtin-metrics/threads ()
  (let ((nb-threads (length (system:list-threads))))
    (list (make-point "tungsten.lisp.nb_threads"
                      :fields `(("count" . ,nb-threads))))))

(defun collect-builtin-metrics/memory ()
  (multiple-value-bind (virtual resident shared)
      (system:memory-usage)
    (list (make-point "tungsten.system.memory"
                      :fields `(("virtual" . ,virtual)
                                ("resident" . ,resident)
                                ("shared" . ,shared))))))

(defun collect-builtin-metrics/file-descriptors ()
  (let ((count (system:count-file-descriptors))
        (limit (system:file-descriptor-limit)))
    (list (make-point "tungsten.system.file_descriptors"
                      :fields `(("count" . ,count)
                                ("limit" . ,limit))))))
