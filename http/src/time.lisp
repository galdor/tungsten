(in-package :http)

(defun format-rfc7231-date (time)
  (declare (type (integer 0) time))
  (multiple-value-bind
        (second minute hour day month year week-day daylightp zone)
      (decode-universal-time time 0)
    (declare (ignore daylightp zone))
    (format nil "~A, ~2,'0D ~A ~4,'0D ~2,'0D:~2,'0D:~2,'0D GMT"
            (rfc7231-week-day-name week-day)
            day (rfc7231-month-name month) year
            hour minute second)))

(defun rfc7231-week-day-name (day)
  (declare (type (integer 0 6) day))
  (ecase day
    (0 "Mon")
    (1 "Tue")
    (2 "Wed")
    (3 "Thu")
    (4 "Fri")
    (5 "Sat")
    (6 "Sun")))

(defun rfc7231-month-name (month)
  (declare (type (integer 1 12) month))
  (ecase month
    (1 "Jan")
    (2 "Feb")
    (3 "Mar")
    (4 "Apr")
    (5 "May")
    (6 "Jun")
    (7 "Jul")
    (8 "Aug")
    (9 "Sep")
    (10 "Oct")
    (11 "Nov")
    (12 "Dec")))
