(in-package :uri)

(defun percent-decode (string &key (start 0) (end (length string))
                                   decode-plus)
  (declare (type string string)
           (type integer start end))
  (let ((octets (make-array (length string) :element-type '(unsigned-byte 8)
                                            :fill-pointer 0)))
    (do ((i start))
        ((>= i end)
         (text:decode-string octets))
      (let ((c (char string i)))
        (cond
          ((char= c #\%)
           (unless (>= (- end i) 3)
             (error "truncated percent sequence"))
           (let ((hi (hex-digit-to-integer (char string (+ i 1))))
                 (lo (hex-digit-to-integer (char string (+ i 2)))))
             (vector-push-extend (logior (ash hi 4) lo) octets))
           (incf i 3))
          ((and (char= c #\+) decode-plus)
           (vector-push-extend #.(char-code #\Space) octets)
           (incf i))
          (t
           (vector-push-extend (char-code c) octets)
           (incf i)))))))

(defun hex-digit-to-integer (digit)
  (declare (type character digit))
  (cond
    ((char<= #\0 digit #\9)
     (- (char-code digit) #.(char-code #\0)))
    ((char<= #\A digit #\F)
     (+ 10 (- (char-code digit) #.(char-code #\A))))
    ((char<= #\a digit #\f)
     (+ 10 (- (char-code digit) #.(char-code #\a))))
    (t
     (error "invalid hex digit ~S" digit))))

(defun percent-encode (string valid-char-p)
  (declare (type string string)
           (type (function (character) boolean) valid-char-p))
  (with-output-to-string (stream)
    (dotimes (i (length string))
      (let ((c (char string i)))
        (cond
          ((funcall valid-char-p c)
           (write-char c stream))
          (t
           (let ((bytes (text:encode-string (string c))))
             (dotimes (i (length bytes))
               (format stream "%~2,'0X" (aref bytes i))))))))))

(defun unreserved-char-p (c)
  (declare (type character c))
  (or (char<= #\a c #\z)
      (char<= #\A c #\Z)
      (char<= #\0 c #\9)
      (char= c #\-)
      (char= c #\.)
      (char= c #\_)
      (char= c #\~)))

(defun sub-delim-char-p (c)
  (declare (type character c))
  (or (char= c #\!)
      (char= c #\$)
      (char= c #\&)
      (char= c #\')
      (char= c #\()
      (char= c #\))
      (char= c #\*)
      (char= c #\+)
      (char= c #\,)
      (char= c #\;)
      (char= c #\=)))

(defun pcharp (c)
  (declare (type character c))
  (or (char= c #\:)
      (char= c #\@)
      (unreserved-char-p c)
      (sub-delim-char-p c)))

(defun valid-username-char-p (c)
  (declare (type character c))
  (pcharp c))

(defun encode-username (username)
  (percent-encode username #'valid-username-char-p))

(defun valid-password-char-p (c)
  (declare (type character c))
  (or (pcharp c)
      (char= c #\:)))

(defun encode-password (password)
  (percent-encode password #'valid-password-char-p))

(defun valid-path-char-p (c)
  (declare (type character c))
  (or (pcharp c)
      (char= c #\/)))

(defun encode-path (path)
  (percent-encode path #'valid-path-char-p))

(defun valid-query-value-char-p (c)
  (declare (type character c))
  ;; While the grammar in RFC 3986 defines valid query characters as pchars,
  ;; '/' or '?', we actually want to know if the character can be represented
  ;; without being encoded as an hexadecimal sequence. The only characters
  ;; which can be included in a query value without being encoded are
  ;; characters of the unreserved set.
  (unreserved-char-p c))

(defun encode-query-value (value)
  (percent-encode value #'valid-query-value-char-p))

(defun encode-query (query)
  (flet ((encode-parameter (parameter)
           (concatenate 'string
                        (encode-query-value (car parameter))
                        "="
                        (encode-query-value (cdr parameter)))))
    (format nil "~{~A~^&~}" (mapcar #'encode-parameter query))))

(defun valid-fragment-char-p (c)
  (declare (type character c))
  (or (pcharp c)
      (char= c #\/)
      (char= c #\?)))

(defun encode-fragment (fragment)
  (percent-encode fragment #'valid-fragment-char-p))
