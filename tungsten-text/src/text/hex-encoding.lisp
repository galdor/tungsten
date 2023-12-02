(in-package :text)

(define-condition invalid-hex-digit (error)
  ((character
    :type character
    :initarg :character
    :reader invalid-hex-digit-character))
  (:report
   (lambda (condition stream)
     (format stream "invalid hex digit ~S"
             (invalid-hex-digit-character condition)))))

(define-condition invalid-hex-string (error)
  ((string
    :type string
    :initarg :string
    :reader invalid-hex-string-string))
  (:report
   (lambda (condition stream)
     (format stream "invalid hex-encoded string ~S"
             (invalid-hex-string-string condition)))))

(defun encode-hex-string (octets &key (start 0) end)
  (declare (type core:octet-vector octets)
           (type (integer 0) start)
           (type (or (integer 0) null) end))
  (flet ((hex-digit (i)
           (declare (type (unsigned-byte 4) i))
           (cond
             ((<= i 9)
              (code-char (+ #.(char-code #\0) i)))
             ((<= i 15)
              (code-char (+ #.(char-code #\a) (- i 10)))))))
    (do* ((end (or end (length octets)))
          (string (make-array (* 2 (- end start)) :element-type 'standard-char))
          (i start (1+ i))
          (j 0 (+ j 2)))
         ((>= i end)
          string)
      (let ((octet (aref octets i)))
        (setf (aref string j) (hex-digit (ldb (byte 4 4) octet))
              (aref string (1+ j)) (hex-digit (ldb (byte 4 0) octet)))))))

(defun decode-hex-string (string &key (start 0) end)
  (declare (type string string)
           (type (integer 0) start)
           (type (or (integer 0) null) end))
  (flet ((hex-digit-value (character)
           (declare (type character character))
           (cond
             ((char<= #\0 character #\9)
              (- (char-code character) #.(char-code #\0)))
             ((char<= #\a character #\f)
              (+ 10 (- (char-code character) #.(char-code #\a))))
             ((char<= #\A character #\F)
              (+ 10 (- (char-code character) #.(char-code #\A))))
             (t
              (error 'invalid-hex-digit :character character)))))
    (let* ((end (or end (length string)))
           (nb-characters (- end start)))
      (unless (zerop (mod nb-characters 2))
        (error 'invalid-hex-string :string string))
      (do ((octets (core:make-octet-vector (/ nb-characters 2)))
           (i start (+ i 2))
           (j 0 (1+ j)))
          ((>= i end)
           octets)
        (let ((digit1 (aref string i))
              (digit2 (aref string (1+ i))))
          (setf (ldb (byte 4 4) (aref octets j)) (hex-digit-value digit1)
                (ldb (byte 4 0) (aref octets j)) (hex-digit-value digit2)))))))
