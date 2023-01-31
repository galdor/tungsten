(in-package :time)

(defconstant unix-epoch -11017
  "The UNIX epoch as a number of days between 2000-03-01 and 1970-01-01.")

(defparameter *month-day-offsets*
  (map-into (make-array 13 :element-type '(integer 0 365))
              (let ((sum 0)) (lambda (n) (incf sum n)))
              #(0 31 30 31 30 31 31 30 31 30 31 31 28))
  "The offset in days of each beginning of the month starting from March 1th.
Includes a thirteenth entry because we need an upper boundary during
calculations.")

(defstruct (datetime
            (:constructor nil))
  "A date in the Gregorian calendar represented as a number of days, seconds and
nanoseconds since 2000-03-01."
  (days 0 :type integer)
  (seconds 0 :type (integer 0 86399))
  (nanoseconds 0 :type (integer 0 999999999)))

(defmethod print-object ((datetime datetime) stream)
  (print-unreadable-object (datetime stream :type t)
    (write-string (format-datetime datetime :rfc3339) stream)))

(defun make-datetime (year month day
                      &optional (hours 0) (minutes 0) (seconds 0)
                                (nanoseconds 0))
  (make-datetime* (encode-datetime-days year month day)
                  (encode-datetime-seconds hours minutes seconds)
                  nanoseconds))

(defun make-datetime* (days seconds nanoseconds)
  (let ((datetime (make-instance 'datetime)))
    (setf (slot-value datetime 'days) days
          (slot-value datetime 'seconds) seconds
          (slot-value datetime 'nanoseconds) nanoseconds)
    datetime))

(defun make-datetime-from-unix-timestamp (timestamp &key (unit :second))
  "Create a datetime from a UNIX timestamp. The precision of the timestamp is
specified with the UNIX parameter; supported units are :SECOND, :MILLISECOND,
:MICROSECOND and :NANOSECOND. The default unit is :SECOND."
  (declare (type integer timestamp)
           (type (member :second :millisecond :microsecond :nanosecond) unit))
  (multiple-value-bind (seconds rest)
      (floor timestamp (ecase unit
                         (:second               1)
                         (:millisecond       1000)
                         (:microsecond    1000000)
                         (:nanosecond  1000000000)))
    (let ((nanoseconds (case unit
                         (:millisecond (setf rest (* rest 1000000)))
                         (:microsecond (setf rest (* rest    1000)))
                         (t rest))))
      (multiple-value-bind (days seconds) (floor seconds 86400)
        (make-datetime* (+ days unix-epoch) seconds nanoseconds)))))

(defun datetime-unix-timestamp (datetime &key (unit :second))
  "Return DATETIME represented as a UNIX timestamp. The precision of the
timestamp depends on the UNIT parameter; supported units are :SECOND,
:MILLISECOND, :MICROSECOND and :NANOSECOND. The default unit is :SECOND."
  (declare (type datetime datetime)
           (type (member :second :millisecond :microsecond :nanosecond) unit))
  (with-slots (days seconds nanoseconds) datetime
    (let ((timestamp (+ (* (- days unix-epoch) 86400) seconds)))
      (ecase unit
        (:second            timestamp)
        (:millisecond (+ (* timestamp       1000) (floor nanoseconds 1000000)))
        (:microsecond (+ (* timestamp    1000000) (floor nanoseconds    1000)))
        (:nanosecond  (+ (* timestamp 1000000000)        nanoseconds))))))

(defun decode-datetime (datetime)
  "Return the year, month, day, hours, minutes, seconds and nanoseconds parts of
DATETIME."
  (declare (type datetime datetime))
  (with-slots (days seconds nanoseconds) datetime
    (multiple-value-bind (year month day)
        (decode-datetime-days days)
      (multiple-value-bind (hours minutes seconds)
          (decode-datetime-seconds seconds)
        (values year month day hours minutes seconds nanoseconds)))))

(defun decode-datetime-days (days)
  (declare (type integer days))
  ;; The point of using 2000-03-01 as the zero value for the number of days is
  ;; that it marks the start of a 400 years cycle, which simplifies
  ;; calculations. See "The Long, Painful History of Time" (Naggum 1999) for
  ;; more information.
  ;;
  ;; Remember, years are leap years if they are multiple of 4, but not if they
  ;; are multiple of 100, unless they are multiple of 400.
  (let ((days-in-400-years 146097)      ; 303 normal years, 97 leap years
        (days-in-100-years  36524)      ;  76 normal years, 24 leap years
        (days-in-4-years     1461)      ;   3 normal years,  1 leap year
        (days-in-1-year       365))
    (multiple-value-bind (nb-400-year-cycles days)
        (floor days days-in-400-years)
      (let* ((nb-100-year-cycles (min (floor days days-in-100-years) 3))
             (days (- days (* nb-100-year-cycles days-in-100-years))))
        (multiple-value-bind (nb-4-year-cycles days)
            (floor days days-in-4-years)
          (let* ((nb-years (min (floor days days-in-1-year) 3))
                 (year (+ (* nb-400-year-cycles 400)
                          (* nb-100-year-cycles 100)
                          (* nb-4-year-cycles 4)
                          nb-years))
                 (days (- days (* nb-years days-in-1-year))))
            ;; At this point DAYS is the number of days remaining after
            ;; removing all full years, with zero being March 1st.
            (let* ((month
                     (or (position days *month-day-offsets* :test #'<) 12))
                   (day (1+ (- days (aref *month-day-offsets* (1- month))))))
              (if (< month 11)
                  (values (+ 2000 year) (+ month 2) day)
                  (values (+ 2001 year) (- month 10) day)))))))))

(defun encode-datetime-days (year month day)
  (declare (type year year)
           (type month month)
           (type day day))
  (let* ((year (- year (if (< month 3) 2001 2000)))
         (year-days (+ (floor year 400)
                       (- (floor year 100))
                       (floor year 4)
                       (* year 365)))
         (month (if (< month 3) (+ month 9) (- month 3)))
         (month-days (aref *month-day-offsets* month)))
    (+ year-days month-days (1- day))))

(defun decode-datetime-seconds (seconds)
  (declare (type (integer 0 86399) seconds))
  (multiple-value-bind (hours rest) (floor seconds 3600)
    (multiple-value-bind (minutes seconds) (floor rest 60)
      (values hours minutes seconds))))

(defun encode-datetime-seconds (hours minutes seconds)
  (declare (type (integer 0 23) hours)
           (type (integer 0 59) minutes)
           (type (integer 0 59) seconds))
  (+ (* hours 3600)
     (* minutes 60)
     seconds))

(defun current-datetime ()
  "Return the current datetime reported by the system clock."
  (multiple-value-bind (seconds nanoseconds)
      (clock-gettime :clock-realtime)
    (multiple-value-bind (day seconds)
        (floor seconds 86400)
      (make-datetime* (+ unix-epoch day) seconds nanoseconds))))
