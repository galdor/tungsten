(in-package :time)

(define-condition datetime-parse-error (parse-error)
  ((format-control
    :type string
    :initarg :format-control)
   (format-arguments
    :type list
    :initarg :format-arguments))
  (:report
   (lambda (condition stream)
     (with-slots (format-control format-arguments) condition
       (format stream "Invalid datetime: ~?."
               format-control format-arguments)))))

(defun datetime-parse-error (format &rest arguments)
  (error 'datetime-parse-error :format-control format
                               :format-arguments arguments))

;; Quick and dirty parsing for the most common format (RFC 3339). In the
;; future we want something similar to strptime().

(defun parse-rfc3339-datetime (string &key (start 0) (end (length string)))
  (declare (type string string)
           (type integer start end))
  (labels ((date-time-separator-p (c)
             (declare (type character c))
             (or (char= c #\t) (char= c #\T)))
           (timezone-separator-p (c)
             (declare (type character c))
             (or (char= c #\Z) (char= c #\z) (char= c #\+) (char= c #\-)))
           (split (string c start end)
             (declare (type string string)
                      (type character c)
                      (type integer start end))
             (do ((parts nil)
                  (i start))
                 ((>= i end)
                  (nreverse parts))
               (let* ((c-position (position c string :start i :end end))
                      (part-end (or c-position end)))
                 (push (subseq string i part-end) parts)
                 (setf i (1+ part-end)))))
           (parse-integer-part (string name)
             (declare (type string string name))
             (unless (and (not (zerop (length string)))
                          (every #'digit-char-p string))
               (datetime-parse-error "invalid ~A" name))
             (parse-integer string))
           (parse-seconds (string)
             (declare (type string string))
             (let ((dot (position #\. string)))
               (cond
                 (dot
                  (let ((seconds (parse-integer-part (subseq string 0 dot)
                                                     "seconds"))
                        (fractional-seconds
                          (parse-integer-part (subseq string (1+ dot))
                                              "fractional seconds"))
                        (fractional-seconds-length (- (length string) dot 1)))
                    (values seconds
                            (ceiling
                             (/ fractional-seconds
                                   (coerce (expt 10 fractional-seconds-length)
                                           'double-float))
                             1.0d-9))))
                 (t
                  (values (parse-integer-part string "seconds") 0))))))
    (let ((timezone-start (position-if #'timezone-separator-p string
                                       :start start :end end :from-end t)))
      (unless timezone-start
        (datetime-parse-error "missing timezone indicator"))
      (unless (or (string= string "Z" :start1 timezone-start :end1 end)
                  (string= string "z" :start1 timezone-start :end1 end))
        (datetime-parse-error "unsupported non-GMT timezone indicator"))
      (let ((date-time-separator (position-if #'date-time-separator-p string
                                              :start start
                                              :end timezone-start)))
        (unless date-time-separator
          (datetime-parse-error "missing date/time separator"))
        (let ((date-parts
                (split string #\- start date-time-separator))
              (time-parts
                (split string #\: (1+ date-time-separator) timezone-start)))
          (unless (= (length date-parts) 3)
            (datetime-parse-error "invalid date format"))
          (unless (= (length time-parts) 3)
            (datetime-parse-error "invalid time format"))
          (let ((year (parse-integer-part (first date-parts) "year"))
                (month (parse-integer-part (second date-parts) "month"))
                (day (parse-integer-part (third date-parts) "day"))
                (hours (parse-integer-part (first time-parts) "hours"))
                (minutes (parse-integer-part (second time-parts) "minutes")))
            (multiple-value-bind (seconds nanoseconds)
                (parse-seconds (third time-parts))
              (funcall 'make-datetime
                       year month day
                       hours minutes seconds
                       nanoseconds))))))))
