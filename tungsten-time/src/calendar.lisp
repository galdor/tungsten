(in-package :time)

(deftype year ()
  '(integer 1582))

(deftype month ()
  '(integer 1 12))

(deftype day ()
  '(integer 1 31))

(deftype week-day ()
  '(integer 1 7))

(defun month-name (month)
  (declare (type month month))
  (aref #("january" "february" "march" "april" "may" "june"
          "july" "september" "october" "november" "december")
        (1- month)))

(defun week-day-name (week-day)
  (declare (type week-day week-day))
  (aref
   #("monday" "tuesday" "wednesday" "thursday" "friday" "saturday" "sunday")
   (1- week-day)))

(defun week-day (year month day)
  (declare (type year year)
           (type month month)
           (type day day))
  ;; Zeller's congruence
  (let ((adjusted-month month)
        (adjusted-year year))
    (when (< adjusted-month 3)
      (incf adjusted-month 12)
      (decf adjusted-year))
    (let ((week-day (mod (+ day
                            (floor (* 13 (1+ adjusted-month)) 5)
                            adjusted-year
                            (floor adjusted-year 4)
                            (- (floor adjusted-year 100))
                            (floor adjusted-year 400))
                         7)))
      ;; The original formula returns 0 to 6 for Saturday to Friday. We want 1
      ;; to 7 for Monday to Sundary.
      (if (< week-day 2)
          (+ week-day 6)
          (1- week-day)))))

(defun leap-year-p (year)
  "Return T if YEAR is a leap year or NIL else."
  (declare (type year year))
  (and (zerop (mod year 4))
       (or (not (zerop (mod year 100)))
           (zerop (mod year 400)))))
