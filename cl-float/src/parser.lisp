(in-package :float)

(define-condition float-parse-error (parse-error)
  ((string
    :type string
    :initarg :string)
   (description
    :type string
    :initarg :description))
  (:report
   (lambda (c stream)
     (with-slots (string description) c
       (format stream "Invalid floating point number ~S: ~A."
               string description)))))

(defun float-parse-error (string format &rest args)
  (error (make-instance 'float-parse-error
                        :string string
                        :description (apply #'format nil format args))))

(defun parse (string &key (start 0) (end (length string))
                          (type 'double-float))
  "Parse a floating point number.

Return both the floating point value and the number of characters read."
  (declare (type string string)
           (type (integer 0) start end))
  (let ((i start)
        (sign 1)
        (integer-part 0)
        (fractional-part 0)
        (nb-fractional-digits 0)
        (exponent-sign 1)
        (exponent 0))
    (declare (type (integer 0) i)
             (type (integer -1 1) sign exponent-sign)
             (type integer
                   integer-part
                   fractional-part nb-fractional-digits
                   exponent))
    (tagbody
     sign
       (when (< i end)
         (let ((c (char string i)))
           (cond
             ((char= c #\-)
              (setf sign -1)
              (incf i))
             ((char= c #\+)
              (incf i)))))
     integer-part
       (cond
         ((>= i end)
          (float-parse-error (subseq string start end)
                             "missing integer part"))
         ((digit-char-p (char string i))
          (let ((part-end (or (position-if-not #'digit-char-p string
                                               :start (1+ i) :end end)
                              end)))
            (setf integer-part
                  (parse-integer string :start i :end part-end))
            (setf i part-end)))
         (t
          (float-parse-error (subseq string start end)
                             "invalid digit ~S" (char string i))))
     fractional-part
       (when (and (< i end) (char= (char string i) #\.))
         (incf i)
         (cond
           ((= i end)
            (float-parse-error (subseq string start end)
                               "missing fractional part"))
           ((digit-char-p (char string i))
            (let* ((part-end (or (position-if-not #'digit-char-p string
                                                  :start (1+ i) :end end)
                                 end)))
              (setf fractional-part
                    (parse-integer string :start i :end part-end))
              (setf nb-fractional-digits (- part-end i))
              (setf i part-end)))
           (t
            (float-parse-error (subseq string start end)
                               "invalid digit ~S" (char string i)))))
     exponent
       (when (>= i end)
         (go end))
       (let ((c (char string i)))
         (when (and (char/= c #\E) (char/= c #\e))
           (float-parse-error (subseq string start end)
                              "invalid exponent character ~S" c)))
       (incf i)
     exponent-sign
       (when (< i end)
         (let ((c (char string i)))
           (cond
             ((char= c #\-)
              (setf exponent-sign -1)
              (incf i))
             ((char= c #\+)
              (incf i)))))
     exponent-part
       (cond
         ((>= i end)
          (float-parse-error (subseq string start end) "missing exponent"))
         ((digit-char-p (char string i))
          (let ((part-end (or (position-if-not #'digit-char-p string
                                               :start (1+ i) :end end)
                              end)))
            (setf exponent (parse-integer string :start i :end part-end))
            (setf i part-end)))
         (t
          (float-parse-error (subseq string start end)
                             "invalid digit ~S" (char string i))))
     end)
    (let* ((mantissa (* sign
                        (+ integer-part
                           (/ fractional-part
                              (coerce (expt 10 nb-fractional-digits) type)))))
           (number (if (> exponent-sign 0)
                       (* mantissa (expt 10 exponent))
                       (/ mantissa (expt 10 exponent)))))
      (values number (- i start)))))
