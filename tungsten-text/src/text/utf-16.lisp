(in-package :text)

(define-condition truncated-utf16-character (decoding-error)
  ())

(define-condition truncated-utf16-surrogate-pair (decoding-error)
  ())

(defun encoded-character-length/utf16 (character)
  (declare (type character character))
  (let ((code (char-code character)))
    (cond
      ((<= #xd800 code #xdfff)
       (error 'unencodable-character :character character :encoding :utf-16))
      ((< code #xffff) 2)
      (t 4))))

(defun encoded-string-length/utf16 (string start end)
  (declare (type string string)
           (type (or index null) start end))
  (do ((max-index (1- (or end (length string))))
       (length 0)
       (i (or start 0) (1+ i)))
      ((> i max-index)
       length)
    (incf length (encoded-character-length/utf16 (schar string i)))))

(defun encode-character/utf16be (character octets offset)
  (declare (type character character)
           (type core:octet-vector octets)
           (type (or index null) offset))
  (let ((code (char-code character)))
    (macrolet ((char-octets (i)
                 `(aref octets (+ offset ,i))))
      (cond
        ((<= #xd800 code #xdfff)
         (error 'unencodable-character :character character
                                       :encoding :utf-16be))
        ((< code #xffff)
         (setf (char-octets 0) (ldb (byte 8 8) code))
         (setf (char-octets 1) (ldb (byte 8 0) code))
         2)
        (t
         (let* ((n (- code #x10000))
                (w1 (+ #xd800 (ldb (byte 10 10) n)))
                (w2 (+ #xdc00 (ldb (byte 10  0) n))))
           (setf (char-octets 0) (ldb (byte 8 8) w1))
           (setf (char-octets 1) (ldb (byte 8 0) w1))
           (setf (char-octets 2) (ldb (byte 8 8) w2))
           (setf (char-octets 3) (ldb (byte 8 0) w2)))
         4)))))

(defun decoded-string-length/utf16be (octets start end)
  (declare (type core:octet-vector octets)
           (type (or index null) start end))
  (do ((max-index (1- (or end (length octets))))
       (length 0 (1+ length))
       (i (or start 0)))
      ((> i max-index)
       length)
    (cond
      ;; Surrogate pair
      ((<= #xd8 (aref octets i) #xdb)
       (when (> (+ i 3) max-index)
         (error 'truncated-utf16-surrogate-pair :octets octets :offset i))
       (incf i 4))
      ;; Character
      (t
       (when (> (+ i 1) max-index)
         (error 'truncated-utf16-character :octets octets :offset i))
       (incf i 2)))))

(defun decode-character/utf16be (octets start end)
  (declare (type core:octet-vector octets)
           (type (or index null) start end))
  (let* ((start (or start 0))
         (end (or end (length octets)))
         (nb-octets (- end start)))
    (flet ((check-length (n)
             (when (< nb-octets n)
               (return-from decode-character/utf16be (values nil 0)))))
      (check-length 1)
      (cond
        ;; Surrogate pair
        ((<= #xd8 (aref octets start) #xdb)
         (check-length 4)
         (let* ((b1 (aref octets start))
                (b2 (aref octets (+ start 1)))
                (b3 (aref octets (+ start 2)))
                (b4 (aref octets (+ start 3)))
                (w1 (logior (ash b1 8) b2))
                (w2 (logior (ash b3 8) b4))
                (code (+ #x10000
                         (ash (- w1 #xd800) 10)
                         (- w2 #xdc00))))
           (values (code-char code) 4)))
        ;; Character
        (t
         (let* ((b1 (aref octets start))
                (b2 (aref octets (1+ start)))
                (code (logior (ash b1 8) b2)))
           (values (code-char code) 2)))))))

(define-encoding :utf-16be ()
  :name "UTF-16BE"
  :encoded-character-length-function #'encoded-character-length/utf16
  :encoded-string-length-function #'encoded-string-length/utf16
  :character-encoding-function #'encode-character/utf16be
  :decoded-string-length-function #'decoded-string-length/utf16be
  :character-decoding-function #'decode-character/utf16be)
