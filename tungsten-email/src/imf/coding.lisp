(in-package :imf)

(defun dot-atom-text-p (string)
  (declare (type string string))
  ;; Not 100% accurate according to RFC 5322 3.2.3., we would need to check
  ;; that there is at least one "atext" character between each pair of dots.
  (and (> (length string) 0)
       (atext-char-p (char string 0))
       (every (lambda (c)
                (or (atext-char-p c)
                    (char= c #\.)))
              string)))

(defun atext-char-p (c)
  (declare (type character c))
  (or (char<= #\A c #\Z)
      (char<= #\a c #\z)
      (char<= #\0 c #\9)
      (member c '#.(coerce "!#$%&'*+-/=?^_`{|}~" 'list) :test #'char=)))

(defun printable-ascii-char-p (c)
  (declare (type character c))
  (char<= #\! c #\~))

(defun qtext-char-p (c)
  (declare (type character c))
  (and (printable-ascii-char-p c)
       (char/= c #\")
       (char/= c #\\)))

(defun quote-string (string)
  (declare (type string string))
  ;; In the context of this package, QUOTE-STRING refers to the quoted-string
  ;; syntax rule of RFC 5322 extended by RFC 6532 (Internationalized Email
  ;; Headers).
  (let* ((length (do ((length 0)
                      (i 0 (1+ i))
                      (end (length string)))
                     ((>= i end)
                      (+ length 2))
                   (let ((c (char string i)))
                     (incf length (if (qtext-char-p c) 1 2)))))
         (quoted-string (make-array length :element-type 'character)))
    (setf (aref quoted-string 0) #\"
          (aref quoted-string (1- length)) #\")
    (do ((i 0 (1+ i))
         (j 1)
         (end (length string)))
        ((>= i end)
         quoted-string)
      (let ((c (char string i)))
        (cond
          ((qtext-char-p c)
           (setf (aref quoted-string j) c)
           (incf j))
          (t
           (setf (aref quoted-string j) #\\
                 (aref quoted-string (1+ j)) c)
           (incf j 2)))))))
