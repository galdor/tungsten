(in-package :influxdb)

(defparameter *builtin-metrics-collection-interval* 10.0
  "The time between collections of builtin metrics in seconds.")

(defun collect-builtin-metrics ()
  (append (collect-builtin-metrics/threads)
          #+sbcl (collect-builtin-metrics/sbcl-memory)))

(defun collect-builtin-metrics/threads ()
  (let ((nb-threads (length (system:list-threads))))
    (list (make-point "tungsten.lisp.nb_threads"
                      :fields `(("count" . ,nb-threads))))))

#+sbcl
(defun collect-builtin-metrics/sbcl-memory ()
  (let ((spaces '((:dynamic . "dynamic")
                  (:immobile . "immobile")
                  (:read-only . "read_only")
                  (:static . "static")))
        (points nil))
    (dolist (space spaces points)
      (let ((entries (sb-vm::type-breakdown (car space)))
            (total-size 0)
            (nb-objects 0))
        (dolist (entry entries)
          (incf total-size (first entry))
          (incf nb-objects (second entry)))
        (push (make-point "tungsten.lisp.sbcl.memory"
                          :tags `(("space" . ,(cdr space)))
                          :fields `(("size" . ,total-size)
                                    ("nb_objects" . ,nb-objects)))
              points)))))
