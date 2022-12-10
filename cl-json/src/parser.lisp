(in-package :json)

(deftype duplicate-key-handling ()
  '(member :keep :first :last :error))

(defparameter *duplicate-key-handling* :keep)
(defparameter *max-depth* 100)

(define-condition json-parse-error (parse-error)
  ((row
    :type (integer 1)
    :initarg :row)
   (column
    :type (integer 1)
    :initarg :column)
   (description
    :type string
    :initarg :description))
  (:report
   (lambda (c stream)
     (with-slots (row column description) c
       (format stream "Invalid JSON string: ~D:~D: ~A."
               row column description)))))

(defclass parser ()
  ((string
    :type string
    :initarg :string)
   (start
    :type (integer 0)
    :initarg :start)
   (end
    :type (integer 0)
    :initarg :end)
   (i
    :type (integer 0))
   (depth
    :type (integer 0)
    :initform 0)
   (row
    :type (integer 1)
    :initform 1)
   (column
    :type (integer 1)
    :initform 1)))

(defmethod initialize-instance :after ((parser parser) &key &allow-other-keys)
  (with-slots (string start end i) parser
    (unless (slot-boundp parser 'end)
      (setf end (length string)))
    (setf i start)))

(defun parser-error (parser format &rest args)
  (declare (type parser parser))
  (with-slots (row column) parser
    (let ((description (apply #'format nil format args)))
      (error 'json-parse-error :row row :column column
                               :description description))))

(defun parser-endp (parser)
  (declare (type parser parser))
  (with-slots (i end) parser
    (>= i end)))

(defun parser-skip (parser n)
  (declare (type parser parser)
           (type integer n))
  (with-slots (i column) parser
    (incf i n)
    (incf column n)))

(defun parser-skip-whitespaces (parser)
  (declare (type parser parser))
  (with-slots (string i end row column) parser
    (do ((j i (1+ j)))
        ((>= j end)
         (setf i j))
      (let ((c (char string j)))
        (cond
          ((char= c #\Newline)
           (incf row)
           (setf column 1))
          ((or (char= c #\Space)
               (char= c #\Tab)
               (char= c #\Return))
           (incf column))
          (t
           (setf i j)
           (return-from parser-skip-whitespaces)))))))

(defun parse (string &key (start 0) (end (length string)))
  (let ((parser
          (make-instance 'parser :string string
                                 :start start
                                 :end end)))
    (let ((value (parse-value parser)))
      (parser-skip-whitespaces parser)
      (with-slots (i end) parser
        (when (< i end)
          (parser-error parser "invalid trailing data after value")))
      value)))

(defun parse-value (parser)
  (declare (type parser parser))
  (parser-skip-whitespaces parser)
  (when (parser-endp parser)
    (parser-error parser "missing value"))
  (with-slots (string i) parser
    (let ((c (char string i)))
      (cond
        ((char= c #\n)
         (parse-constant parser :null "null" 4)
         )
        ((char= c #\t)
         (parse-constant parser :true "true" 4)
         )
        ((char= c #\f)
         (parse-constant parser :false "false" 5)
         )
        ((char= c #\")
         (parse-string parser)
         )
        ((char= c #\[)
         (parse-array parser)
         )
        ((char= c #\{)
         (parse-object parser)
         )
        ((or (char= c #\-) (char<= #\0 c #\9))
         (parse-number parser))
        (t
         (parser-error parser "invalid character ~S" c))))))

(defun parse-constant (parser value value-string value-length)
  (with-slots (string i end) parser
    (cond
      ((and (>= (- end i) value-length)
            (string= string value-string
                     :start1 i :end1 (+ i value-length)))
       (parser-skip parser value-length)
       value)
      (t
       (parser-error parser "invalid value")))))

(defun parse-string (parser)
  (parser-skip parser 1)                ; #\"
  (with-slots (string i end) parser
    (do ((decoded-string (make-array 0 :element-type 'character
                                       :adjustable t :fill-pointer 0)))
        ((and (< i end) (char= (char string i) #\"))
         (parser-skip parser 1)
         decoded-string)
      (when (parser-endp parser)
        (parser-error parser "truncated string"))
      (let ((c (char string i)))
        (cond
          ((and (>= (- end i) 2)
                (char= c #\\)
                (or (char= (char string (1+ i)) #\u)
                    (char= (char string (1+ i)) #\U)))
           ;; Unicode escape sequence
           (parser-skip parser 2)
           (when (< (- end i) 4)
             (parser-error parser "truncated unicode escape sequence"))
           (let ((code (parse-integer string :start i :end (+ i 4)
                                             :radix 16)))
             (cond
               ;; UTF-16 surrogate pair
               ((and (<= #xd800 code #xdbff))
                (parser-skip parser 4)
                (when (<= (- end i) 6)
                  (parser-error parser "truncated utf-16 surrogate pair"))
                (unless (and (char= (char string i) #\\)
                             (or (char= (char string (1+ i)) #\u)
                                 (char= (char string (1+ i)) #\U)))
                  (parser-error parser "invalid utf-16 surrogate pair"))
                (parser-skip parser 2)
                (let* ((hi code)
                       (lo (parse-integer string :start i :end (+ i 4)
                                                 :radix 16))
                       (code (+ #x10000
                                (ash (- hi #xd800) 10)
                                (- lo #xdc00))))
                  (vector-push-extend (code-char code) decoded-string))
                (parser-skip parser 4))
               ;; Single codepoint
               (t
                (vector-push-extend (code-char code) decoded-string)
                (parser-skip parser 4)))))
          ((char= c #\\)
           ;; Simple escape sequence
           (parser-skip parser 1)
           (when (parser-endp parser)
             (parser-error parser "truncated escape sequence"))
           (case (char string i)
             ((#\" #\\ #\/)
              (vector-push-extend (char string i) decoded-string))
             (#\b
              (vector-push-extend #\Backspace decoded-string))
             (#\f
              (vector-push-extend #\Page decoded-string))
             (#\r
              (vector-push-extend #\Return decoded-string))
             (#\n
              (vector-push-extend #\Newline decoded-string))
             (#\t
              (vector-push-extend #\Tab decoded-string))
             (t
              (parser-error parser "invalid escape sequence ~S"
                            (subseq string (1- i) (1+ i)))))
           (parser-skip parser 1))
          ((char/= c #\")
           (vector-push-extend c decoded-string)
           (parser-skip parser 1)))))))

(defun parse-array (parser)
  (with-slots (depth string i end) parser
    (when (> (incf depth) *max-depth*)
      (parser-error parser "maximum recursion depth reached"))
    (parser-skip parser 1)              ; #\[
    (parser-skip-whitespaces parser)
    (do ((elements (make-array 0 :adjustable t :fill-pointer 0)))
        ((and (< i end) (char= (char string i) #\]))
         (parser-skip parser 1)
         (decf depth)
         elements)
      ;; Value
      (when (parser-endp parser)
        (parser-error parser "truncated array"))
      (vector-push-extend (parse-value parser) elements)
      ;; Separator
      (parser-skip-whitespaces parser)
      (when (parser-endp parser)
        (parser-error parser "truncated array"))
      (let ((c (char string i)))
        (cond
          ((char= c #\,)
           (parser-skip parser 1))
          ((char/= c #\])
           (parser-error parser
                         "invalid character ~S after array element" c)))))))

(defun parse-object (parser)
  (with-slots (depth string i end) parser
    (when (> (incf depth) *max-depth*)
      (parser-error parser "maximum recursion depth reached"))
    (parser-skip parser 1)              ; #\{
    (parser-skip-whitespaces parser)
    (do ((members nil))
        ((and (< i end) (char= (char string i) #\}))
         (parser-skip parser 1)
         (decf depth)
         (nreverse members))
      (let ((member (cons nil nil)))
        ;; Key
        (parser-skip-whitespaces parser)
        (when (parser-endp parser)
          (parser-error parser "truncated object"))
        (let ((key (parse-value parser)))
          (unless (stringp key)
            (parser-error parser "invalid non-string object key"))
          ;; TODO duplicate-key handling
          (setf (car member) key))
        ;; Colon
        (parser-skip-whitespaces parser)
        (when (parser-endp parser)
          (parser-error parser "truncated object"))
        (let ((c (char string i)))
          (cond
            ((char= c #\:)
             (parser-skip parser 1))
            (t
             (parser-error parser "invalid character ~S after object key" c))))
        ;; Value
        (parser-skip-whitespaces parser)
        (when (parser-endp parser)
          (parser-error parser "truncated object"))
        (setf (cdr member) (parse-value parser))
        ;; Member
        (push member members))
      ;; Separator
      (parser-skip-whitespaces parser)
      (when (parser-endp parser)
        (parser-error parser "truncated object"))
      (let ((c (char string i)))
        (cond
          ((char= c #\,)
           (parser-skip parser 1))
          ((char/= c #\})
           (parser-error parser
                         "invalid character ~S after object member" c)))))))

(defun parse-number (parser)
  (with-slots (string i end) parser
    (let* ((digits-start (if (char= (char string i) #\-) (1+ i) i))
           (digits-end (or (position-if-not #'digit-char-p
                                            string :start digits-start
                                            :end end)
                           end))
           (is-float (and (< digits-end end)
                          (or (char= (char string digits-end) #\.)
                              (char= (char string digits-end) #\e)
                              (char= (char string digits-end) #\E)))))
      (cond
        (is-float
         (handler-case
             (multiple-value-bind (float n)
                 (float:parse string :start i :end end)
               (parser-skip parser n)
               float)
           (float:float-parse-error (c)
             (parser-error parser "invalid floating point number: ~A" c))))
        (t
         (let ((integer (parse-integer string :start i :end digits-end)))
           (parser-skip parser (- digits-end i))
           integer))))))
