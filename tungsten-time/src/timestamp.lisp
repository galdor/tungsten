(in-package :time)

(defstruct timestamp
  (seconds 0 :type integer :read-only t)
  (nanoseconds 0 :type integer :read-only t))

(defmethod print-object ((timestamp timestamp) stream)
  (print-unreadable-object (timestamp stream :type t)
    (with-slots (seconds nanoseconds) timestamp
      (format stream "~D ~D" seconds nanoseconds))))

(defun timestamp-equal (timestamp1 timestamp2)
  "Return T if TIMESTAMP1 and TIMESTAMP2 are equal or NIL else."
  (declare (type timestamp timestamp1 timestamp2))
  (with-slots ((s1 seconds) (ns1 nanoseconds)) timestamp1
    (with-slots ((s2 seconds) (ns2 nanoseconds)) timestamp2
      (and (= s1 s2) (= ns1 ns2)))))

(defun current-timestamp ()
  (current-clock-timestamp *clock*))

(defun timestamp-delta (t1 t2)
  "Return the time delta between two timestamps T1 and T2 as a floating point
number in seconds."
  (declare (type timestamp t1 t2))
  (let ((ds (- (timestamp-seconds t2) (timestamp-seconds t1)))
        (dns (- (timestamp-nanoseconds t2) (timestamp-nanoseconds t1))))
    (+ (float ds 1.0d0) (* dns 1d-9))))

(defun time-since (timestamp)
  "Return the time delta before TIMESTAMP and the current timestamp as a
floating point number in seconds."
  (declare (type timestamp timestamp))
  (timestamp-delta timestamp (current-timestamp)))
