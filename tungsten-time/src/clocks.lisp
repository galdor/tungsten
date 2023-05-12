(in-package :time)

(defclass clock ()
  ())

(defgeneric current-clock-timestamp (clock))

(defclass wall-clock (clock)
  ())

(defmethod current-clock-timestamp ((clock wall-clock))
  (multiple-value-bind (seconds nanoseconds)
      (clock-gettime :clock-realtime)
    (make-timestamp :seconds seconds :nanoseconds nanoseconds)))

(defclass monotonic-clock (clock)
  ())

(defmethod current-clock-timestamp ((clock monotonic-clock))
  (multiple-value-bind (seconds nanoseconds)
      (clock-gettime #+linux :clock-monotonic-raw
                     #-linux :clock-monotonic)
    (make-timestamp :seconds seconds :nanoseconds nanoseconds)))

(defvar *clock* (make-instance 'wall-clock))

(defmacro with-clock ((clock) &body body)
  `(let ((*clock* ,clock))
     ,@body))

(defmacro with-wall-clock (&body body)
  `(with-clock ((make-instance 'wall-clock))
     ,@body))

(defmacro with-monotonic-clock (&body body)
  `(with-clock ((make-instance 'monotonic-clock))
     ,@body))
