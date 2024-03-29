(in-package :text)

(defun encoded-character-length/ascii (character)
  (declare (type character character))
  (let ((code (char-code character)))
    (cond
      ((< code #x80)
       1)
      (t
       (error 'unencodable-character :character character
                                     :encoding :ascii)))))

(defun encoded-string-length/ascii (string start end)
  (declare (type string string)
           (type (or index null) start end))
  (- (or end (length string)) (or start 0)))

(defun encode-character/ascii (character octets offset)
  (declare (type character character)
           (type core:octet-vector octets)
           (type (or index null) offset))
  (let ((code (char-code character)))
    (cond
      ((< code #x80)
       (setf (aref octets offset) code)
       1)
      (t
       (error 'unencodable-character :character character
                                     :encoding :ascii)))))

(defun decoded-string-length/ascii (octets start end)
  (declare (type core:octet-vector octets)
           (type (or index null) start end))
  (- (or end (length octets)) (or start 0)))

(defun decode-character/ascii (octets start end)
  (declare (type core:octet-vector octets)
           (type (or index null) start end))
  (let ((start (or start 0))
        (end (or end (length octets))))
    (cond
      ((>= start end)
       (values nil 0))
      (t
       (let ((octet (aref octets start)))
         (cond
           ((< octet #x80)
            (values (code-char octet) 1))
           (t
            (error 'invalid-octet :octets octets :offset start
                                  :octet octet :encoding :ascii))))))))

(define-encoding :ascii ()
  :name "ASCII"
  :encoded-character-length-function #'encoded-character-length/ascii
  :encoded-string-length-function #'encoded-string-length/ascii
  :character-encoding-function #'encode-character/ascii
  :decoded-string-length-function #'decoded-string-length/ascii
  :character-decoding-function #'decode-character/ascii)
