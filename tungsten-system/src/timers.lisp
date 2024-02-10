(in-package :system)

;;; Ticks are always represented in microseconds and are based on the system
;;; monotonic clock.

(defclass timer ()
  ((handler
    :type (or symbol function)
    :initarg :handler
    :reader timer-handler)
   (period
    :type (or float null)
    :initarg :period
    :reader timer-period)
   (next-tick
    :type integer
    :initarg :next-tick
    :accessor timer-next-tick)))

(defun timer-type (timer)
  (declare (type timer timer))
  (if (timer-period timer)
      :periodic
      :one-shot))

(defmethod print-object ((timer timer) stream)
  (print-unreadable-object (timer stream :type t)
    (with-slots (period) timer
      (format stream "~A~@[ ~As~]" (timer-type timer) period))))

(defun make-timer (duration handler &key period)
  (declare (type float duration)
           (type (or float null) period)
           (type (or symbol function) handler))
  (let* ((current-time (current-time))
         (next-tick (+ current-time (round (* duration 1e6)))))
    (make-instance 'timer :handler handler
                          :period period
                          :next-tick next-tick)))

(defun update-periodic-timer-next-tick (timer)
  (declare (type timer timer))
  (with-slots (period next-tick) timer
    (incf next-tick (round (* period 1e6)))))

(defun current-time ()
  (ffi:with-foreign-value (%timespec 'timespec)
    (system-funcall
     ("clock_gettime" ((clock-type :pointer) :int) :clock-monotonic %timespec))
    (ffi:with-foreign-structure-members (((seconds :tv-sec)
                                          (nanoseconds :tv-nsec))
                                         %timespec 'timespec)
      (+ (* seconds 1000000) (round nanoseconds 1000)))))
