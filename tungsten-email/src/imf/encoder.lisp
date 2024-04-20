(in-package :imf)

(defvar *encoder* nil)

(defclass encoder ()
  ((stream
    :type stream
    :initarg :stream
    :reader encoder-stream)
   (line-length
    :type (integer 0)
    :initform 0)
   (max-line-length
    :type (or (integer 1) null)
    :initarg :max-line-length)
   (smtp
    :type boolean
    :initarg :smtp
    :initform nil)))

(defun make-encoder (stream &key max-line-length smtp)
  (declare (type stream stream)
           (type (or (integer 1) null) max-line-length)
           (type boolean smtp))
  (make-instance 'encoder :stream stream
                          :max-line-length max-line-length
                          :smtp smtp))

(defmacro with-encoder ((stream &key max-line-length smtp) &body body)
  `(let ((*encoder* (make-encoder ,stream :max-line-length ,max-line-length
                                          :smtp ,smtp)))
     ,@body))

(defmacro with-encoder-to-string ((&key max-line-length) &body body)
  (let ((stream (gensym "STREAM-")))
    `(with-output-to-string (,stream)
       (with-encoder (,stream :max-line-length ,max-line-length)
         ,@body))))

(defun encode-eol ()
  (with-slots (stream line-length) *encoder*
    (write-string (text:eol-string :crlf) stream)
    (setf line-length 0)))

(defun encode-character (c)
  (declare (type character c))
  (encode-string (string c)))

(defun encode-string (string)
  (declare (type string string))
  (with-slots (stream line-length max-line-length smtp) *encoder*
    (let ((nb-bytes (text:encoded-string-length string :encoding :utf-8)))
      (when (and max-line-length (> (+ line-length nb-bytes) max-line-length))
        (write-string (text:eol-string :crlf) stream)
        (write-char #\Space stream)
        (setf line-length 1))
      (when (and smtp (zerop line-length) (char= (char string 0) #\.))
        (write-char #\. stream)
        (incf line-length))
      (write-string string stream)
      (incf line-length nb-bytes)))
  nil)

(defun encode-atom (atom)
  (declare (type string atom))
  (encode-string atom))

(defun encode-dot-atom (dot-atom)
  (declare (type string dot-atom))
  (encode-string dot-atom))

(defun encode-quoted-string (string)
  (declare (type string string))
  (encode-character #\")
  (dotimes (i (length string))
    (let ((c (char string i)))
      (cond
        ((and (< (char-code c) 32) (char/= #\Tab))
         (error "unencodable control character ~S" c))
        ((or (char= c #\") (char= c #\\))
         ;; Write the entire escape sequence as a single string to make sure it
         ;; cannot be split during folding.
         (encode-string (concatenate 'string "\\" (string c))))
        (t
         (encode-character c)))))
  (encode-character #\"))

(defun encode-atom-or-quoted-string (string)
  (declare (type string string))
  (if (atom-p string)
      (encode-atom string)
      (encode-quoted-string string)))

(defun encode-dot-atom-or-quoted-string (string)
  (declare (type string string))
  (if (dot-atom-p string)
      (encode-dot-atom string)
      (encode-quoted-string string)))

(defun encode-phrase (phrase)
  (declare (type string phrase))
  (encode-quoted-string phrase))

(defun encode-phrases (phrases)
  (declare (type list phrases))
  (do ((phrases phrases (cdr phrases)))
      ((null phrases)
       nil)
    (encode-phrase (car phrases))
    (unless (null (cdr phrases))
      (encode-string ", "))))

(defun encode-unstructured (string)
  (declare (type string string))
  (dotimes (i (length string))
    (encode-character (char string i))))

(defun encode-datetime (datetime)
  (declare (type time:datetime datetime))
  ;; Tungsten-time does not support local dates yet, so we only represent GMT
  ;; datetime strings.
  (encode-string (time:format-datetime datetime
                                       '("~:(~A~)" :short-week-day-name)))
  (encode-string ", ")
  (encode-string (time:format-datetime datetime
                                       '("~2,'0D ~:(~A~) ~4,'0D"
                                         :day :short-month-name :year)))
  (encode-character #\Space)
  (encode-string (time:format-datetime datetime
                                       '("~2,'0D:~2,'0D:~2,'0D"
                                         :hour :minute :second)))
  (encode-character #\Space)
  (encode-string "GMT"))
