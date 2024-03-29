(in-package :time)

(define-condition unknown-datetime-format (error)
  ((format
    :type symbol
    :initarg :format
    :reader unknown-datetime-format-format))
  (:report
   (lambda (condition stream)
     (format stream "unknown datetime format ~S"
             (unknown-datetime-format-format condition)))))

(defvar *datetime-formats* (make-hash-table :test #'eq))

(defun register-datetime-format (name format)
  (declare (type symbol name)
           (type list format))
  (setf (gethash name *datetime-formats*) format))

(defun delete-datetime-format (name)
  (declare (type symbol name))
  (remhash name *datetime-formats*))

(defmacro define-datetime-format (name format)
  `(register-datetime-format ,name ',format))

(define-datetime-format :rfc3339
    ;; Example: 1994-11-06T08:49:37.123456789Z
    ("~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~9,2$Z"
     :year :month :day :hour :minute :second-real))

(define-datetime-format :rfc3339-no-fraction
    ;; Example: 1994-11-06T08:49:37Z
    ("~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
     :year :month :day :hour :minute :second))

(define-datetime-format :rfc3339-date
    ;; Example: 1994-11-06
    ("~4,'0D-~2,'0D-~2,'0D"
     :year :month :day))

(define-datetime-format :rfc7231
    ;; Example: Sun, 06 Nov 1994 08:49:37 GMT
    ("~:(~A~), ~2,'0D ~:(~A~) ~4,'0D ~2,'0D:~2,'0D:~2,'0D GMT"
     :short-week-day-name :day :short-month-name :year :hour :minute :second))

(defun format-datetime (datetime format)
  (declare (type datetime datetime)
           (type (or symbol list) format))
  (let* ((format-list
          (if (listp format)
              format
              (or (gethash format *datetime-formats*)
                  (error 'unknown-datetime-format :format format))))
         (control (car format-list))
         (arguments (cdr format-list)))
    (multiple-value-bind (year month day hours minutes seconds nanoseconds)
        (decode-datetime datetime)
      (apply #'format nil control
             (mapcar
              (lambda (argument)
                (ecase argument
                  (:year
                   year)
                  (:month
                   month)
                  (:month-name
                   (month-name month))
                  (:short-month-name
                   (subseq (month-name month) 0 3))
                  (:day
                   day)
                  (:hour
                   hours)
                  (:minute
                   minutes)
                  (:second
                   seconds)
                  (:second-real
                   (+ (float seconds 1.0d0) (* nanoseconds 1.0d-9)))
                  (:week-day
                   (week-day year month day))
                  (:week-day-name
                   (let ((week-day (week-day year month day)))
                     (week-day-name week-day)))
                  (:short-week-day-name
                   (let ((week-day (week-day year month day)))
                     (subseq (week-day-name week-day) 0 3)))))
              arguments)))))
