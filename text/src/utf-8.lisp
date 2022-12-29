(in-package :text)

(define-condition utf8-decoding-error (error)
  ((octets
    :type core:octet-vector
    :initarg :octets)
   (offset
    :type index
    :initarg :offset)))

(define-condition invalid-utf8-leading-octet (utf8-decoding-error)
  ((octet
    :type core:octet
    :initarg :octet)))

(define-condition invalid-utf8-continuation-octet (utf8-decoding-error)
  ((octet
    :type core:octet
    :initarg :octet)))

(define-condition truncated-utf8-sequence (utf8-decoding-error)
  ())

(define-condition overlong-utf8-sequence (utf8-decoding-error)
  ())

(define-condition invalid-utf8-sequence (utf8-decoding-error)
  ())

(defun encoded-string-length/utf8 (string start end)
  (declare (type simple-string string)
           (type (or index null) start end))
  (do ((max-index (1- (or end (length string))))
       (length (the fixnum 0))
       (i (or start 0) (1+ i)))
      ((> i max-index)
       length)
    (let* ((code (the fixnum (char-code (schar string i))))
           (code-length (cond
                          ((< code #x80) 1)
                          ((< code #x800) 2)
                          ((< code #x10000) 3)
                          (t 4))))
      (setf length (the fixnum (+ length code-length))))))

(defun encode-string/utf8 (string start end octets offset)
  (declare (type simple-string string)
           (type core:octet-vector octets)
           (type (or index null) start end offset))
  (do ((max-index (1- (or end (length string))))
       (i (or start 0) (1+ i))
       (j offset))
      ((> i max-index)
       octets)
    (let ((code (the fixnum (char-code (schar string i)))))
      (cond
        ((< code #x80)
         (setf (aref octets j) code)
         (incf j 1))
        ((< code #x800)
         (setf (aref octets j) (logior #xc0 (ash code -6)))
         (setf (aref octets (+ j 1)) (logior #x80 (logand code #x3f)))
         (incf j 2))
        ((< code #x10000)
         (setf (aref octets j) (logior #xe0 (ash code -12)))
         (setf (aref octets (+ j 1)) (logior #x80 (logand (ash code -6) #x3f)))
         (setf (aref octets (+ j 2)) (logior #x80 (logand code #x3f)))
         (incf j 3))
        (t
         (setf (aref octets j) (logior #xf0 (ash code -18)))
         (setf (aref octets (+ j 1)) (logior #x80 (logand (ash code -12) #x3f)))
         (setf (aref octets (+ j 2)) (logior #x80 (logand (ash code -6) #x3f)))
         (setf (aref octets (+ j 3)) (logior #x80 (logand code #x3f)))
         (incf j 4))))))

(defun decoded-string-length/utf8 (octets start end)
  (declare (type core:octet-vector octets)
           (type (or index null) start end))
  (do ((max-index (1- (or end (length octets))))
       (length 0 (the fixnum (1+ length)))
       (i (or start 0)))
      ((> i max-index)
       length)
    (let ((b1 (aref octets i)))
      (cond
        ;; 1 byte character: 0xxxxxxx
        ((<= b1 #x7f)
         (incf i 1))
        ;; 2 byte character: 110xxxxx 10xxxxxx
        ((<= #xc0 b1 #xdf)
         (when (> (+ i 1) max-index)
           (error 'truncated-utf8-sequence :octets octets :offset i))
         (incf i 2))
        ;; 3 byte character: 1110xxxx 10xxxxxx 10xxxxxx
        ((<= #xe0 b1 #xef)
         (when (> (+ i 2) max-index)
           (error 'truncated-utf8-sequence :octets octets :offset i))
         (incf i 3))
        ;; 4 byte character: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
        ((<= #xf0 b1 #xf4)
         (when (> (+ i 3) max-index)
           (error 'truncated-utf8-sequence :octets octets :offset i))
         (incf i 4))
        (t
         (error 'invalid-utf8-leading-octet
                :octets octets :offset i :octet b1))))))

(defun decode-string/utf8 (octets start end string offset)
  (declare (type core:octet-vector octets)
           (type string string)
           (type (or index null) start end offset))
  (flet ((check-continuation-octet (b i)
           (unless (<= #x80 b #xbf)
             (error 'invalid-utf8-continuation-octet
                    :octets octets :offset i :octet b))))
    (do ((max-index (1- (or end (length octets))))
         (i (or start 0))
         (j offset (the fixnum (1+ j))))
        ((> i max-index)
         string)
      (let ((b1 (aref octets i)))
        (cond
          ;; 1 byte character: 0xxxxxxx
          ((<= b1 #x7f)
           (setf (schar string j) (code-char b1))
           (incf i 1))
          ;; 2 byte character: 110xxxxx 10xxxxxx
          ((<= #xc0 b1 #xdf)
           (when (<= b1 #xc1)
             (error 'overlong-utf8-sequence :octets octets :offset i))
           (let ((b2 (aref octets (+ i 1))))
             (check-continuation-octet b2 (+ i 1))
             (let ((code (logior (ash (logand b1 #x1f) 6)
                                 (logand b2 #x3f))))
               (setf (schar string j) (code-char code))))
           (incf i 2))
          ;; 3 byte character: 1110xxxx 10xxxxxx 10xxxxxx
          ((<= #xe0 b1 #xef)
           (let ((b2 (aref octets (+ i 1)))
                 (b3 (aref octets (+ i 2))))
             (check-continuation-octet b2 (+ i 1))
             (check-continuation-octet b3 (+ i 2))
             (when (and (= b1 #xe0) (< b2 #x9f))
               (error 'overlong-utf8-sequence :octets octets :offset i))
             (let ((code (logior (ash (logand b1 #x0f) 12)
                                 (ash (logand b2 #x3f) 6)
                                 (logand b3 #x3f))))
               ;; UTF-16 surrogates
               (when (<= #xd800 code #xdfff)
                 (error 'invalid-utf8-sequence :octets octets :offset i))
               ;; Dangerous characters and non-characters
               (when (or (<= #xfdd0 code #xfdef)
                         (<= #xfffe code #xffff))
                 (error 'invalid-utf8-sequence :octets octets :offset i))
               (setf (schar string j) (code-char code))))
           (incf i 3))
          ;; 4 byte character: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
          ((<= #xf0 b1 #xf4)
           (let ((b2 (aref octets (+ i 1)))
                 (b3 (aref octets (+ i 2)))
                 (b4 (aref octets (+ i 3))))
             (when (and (= b1 #xf0) (<= b2 #x8f))
               (error 'overlong-utf8-sequence :octets octets :offset i))
             (check-continuation-octet b2 (+ i 1))
             (check-continuation-octet b3 (+ i 2))
             (check-continuation-octet b4 (+ i 3))
             (let ((code (logior (ash (logand b1 #x07) 18)
                                 (ash (logand b2 #x3f) 12)
                                 (ash (logand b3 #x3f) 6)
                                 (logand b4 #x3f))))
               (setf (schar string j) (code-char code))))
           (incf i 4)))))))

(define-encoding :utf-8 ()
  :name "UTF-8"
  :encoded-string-length-function #'encoded-string-length/utf8
  :encoding-function #'encode-string/utf8
  :decoded-string-length-function #'decoded-string-length/utf8
  :decoding-function #'decode-string/utf8)
