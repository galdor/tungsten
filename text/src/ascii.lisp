(in-package :text)

(define-condition invalid-ascii-octet (decoding-error)
  ((octet
    :type core:octet
    :initarg :octet))
  (:report
   (lambda (condition stream)
     (with-slots (octet) condition
       (format stream "Invalid ASCII octet ~S." octet)))))

(defun encoded-string-length/ascii (string start end)
  (declare (type simple-string string)
           (type (or index null) start end))
  (- (or end (length string)) (or start 0)))

(defun encode-string/ascii (string start end octets offset)
  (declare (type simple-string string)
           (type core:octet-vector octets)
           (type (or index null) start end offset))
  (do ((max-index (1- (or end (length string))))
       (i (or start 0) (1+ i))
       (j offset (1+ j)))
      ((> i max-index)
       octets)
    (let* ((character (schar string i))
           (code (char-code character)))
      (cond
        ((< code #x80)
         (setf (aref octets j) code))
        (t
         (error 'unencodable-character :character character))))))

(defun decoded-string-length/ascii (octets start end)
  (declare (type core:octet-vector octets)
           (type (or index null) start end))
  (- (or end (length octets)) (or start 0)))

(defun decode-string/ascii (octets start end string offset)
  (declare (type core:octet-vector octets)
           (type string string)
           (type (or index null) start end offset))
  (do ((max-index (1- (or end (length octets))))
       (i (or start 0) (1+ i))
       (j offset (1+ j)))
      ((> i max-index)
       string)
    (let ((octet (aref octets i)))
      (cond
        ((< octet #x80)
         (setf (schar string j) (code-char octet)))
        (t
         (error 'invalid-ascii-octet :octets octets :offset i
                                     :octet octet))))))

(define-encoding :ascii ()
  :name "ASCII"
  :encoded-string-length-function #'encoded-string-length/ascii
  :encoding-function #'encode-string/ascii
  :decoded-string-length-function #'decoded-string-length/ascii
  :decoding-function #'decode-string/ascii)
