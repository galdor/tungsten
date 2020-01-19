;;; Copyright (c) 2019,2020 Nicolas Martyanoff <khaelin@gmail.com>
;;;
;;; Permission to use, copy, modify, and distribute this software for any
;;; purpose with or without fee is hereby granted, provided that the above
;;; copyright notice and this permission notice appear in all copies.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :tungsten.json)

(defparameter *duplicate-object-keys-handling* :keep
  "The way entries with identical keys are handled in objects. Valid values
  are:

- :KEEP: keep all entries.
- :FIRST: only keep the first entry.
- :LAST: only keep the last entry.
- :ERROR: trigger a parsing error")

(defparameter *depth-limit* 1024
  "The maximum nesting depth of a JSON value. The parser will signal a
  DEPTH-LIMIT-EXCEEDED condition when trying to parse a document which exceeds
  this limit.")

(defvar *depth-level* nil
  "The current depth level for the value being parsed.")

(define-condition parsing-error (error)
  ((position
    :type (or null (integer 0))
    :initarg :position
    :initform nil
    :reader parsing-error-position
    :documentation "The position in the source string where the error
    occurred.")
   (message
    :type string
    :initarg :message
    :reader parsing-error-message
    :documentation "A message describing the error."))
  (:documentation "An error signaled when parsing fails.")
  (:report (lambda (condition stream)
             (with-slots (message) condition
               (format stream "parsing error: ~A" message)))))

(defun parsing-error (position format &rest args)
  "Signal a parsing error."
  (let ((message (format nil "~?" format args)))
    (error 'parsing-error :position position :message message)))

(define-condition depth-limit-exceeded (parsing-error)
  ((depth-limit
     :type (integer 0)
     :initarg :depth-limit
     :initform *depth-limit*
     :reader depth-limit-exceeded-depth-limit
     :documentation "The current depth limit."))
  (:documentation "An error signaled when a document depth exceeds the current
  depth limit."))

(defun depth-limit-exceeded (position)
  (error 'depth-limit-exceeded :position position
                               :message "depth limit exceeded"))

(defun skip-spaces (string &optional start end)
  "Return the index of the first character which is not a white space in
STRING, or END if all characters are white spaces. START and END are optional
bounding index designators."
  (or (position-if-not #'char-space-p string :start start :end end) end))

(defun parse (string &key (start 0) (end (length string)))
  "Parse a string as a single JSON value. START and END are optional bounding
index designators. Signal an error if trailing characters are found after the
value. Return both the value which was parsed and the number of characters
read."
  (let ((i start))
    (multiple-value-bind (value length)
        (parse-value string :start i :end end)
      (setf i (skip-spaces string (+ i length) end))
      (when (< i end)
        (parsing-error i "unexpected trailing character ~S" (char string i)))
      (values value length))))

(defun parse-value (string &key (start 0) (end (length string)))
  "Parse a single JSON value from a string, ignoring any trailing
character. START and END are optional bounding index designators. Return both
the value which was parsed and the number of characters read."
  (let ((*depth-level* (if *depth-level* *depth-level* 0))
        (i (skip-spaces string start end)))
    (when (>= i end)
      (parsing-error nil "empty string"))
    (let ((c (char string i)))
      (multiple-value-bind (value length)
          (cond
            ((char= c #\n)
             (parse-null string :start i :end end))
            ((char= c #\t)
             (parse-true string :start i :end end))
            ((char= c #\f)
             (parse-false string :start i :end end))
            ((char= c #\")
             (parse-string string :start i :end end))
            ((char= c #\[)
             (parse-array string :start i :end end))
            ((char= c #\{)
             (parse-object string :start i :end end))
            ((or (char= c #\-) (char<= #\0 c #\9))
             (parse-number string :start i :end end))
            (t
             (parsing-error start "unexpected character ~S" c)))
        (values value (- (+ i length) start))))))

(defun parse-null (string &key (start 0) (end (length string)))
  (let ((element-end (if (<= (+ start 4) end) (+ start 4) end)))
    (unless (string= string "null" :start1 start :end1 element-end)
      (parsing-error start "invalid element ~S"
                     (subseq string start element-end)))
    (values :null 4)))

(defun parse-true (string &key (start 0) (end (length string)))
  (let ((element-end (if (<= (+ start 4) end) (+ start 4) end)))
    (unless (string= string "true" :start1 start :end1 element-end)
      (parsing-error start "invalid element ~S"
                     (subseq string start element-end)))
    (values :true 4)))

(defun parse-false (string &key (start 0) (end (length string)))
  (let ((element-end (if (<= (+ start 5) end) (+ start 5) end)))
    (unless (string= string "false" :start1 start :end1 element-end)
      (parsing-error start "invalid element ~S"
                     (subseq string start element-end)))
    (values :false 5)))

(defun parse-string (string &key (start 0) (end (length string)))
  (when (>= (1+ start) end)
    (parsing-error start "truncated string"))
  (do ((i (1+ start))
       (decoded-string (make-array 0 :element-type 'character
                                     :adjustable t :fill-pointer 0)))
      ((and (< i end) (char= (char string i) #\"))
       (values decoded-string (- (1+ i) start)))
    (cond
      ((>= i end)
       (parsing-error (1- i) "truncated string"))
      ;; End
      ((char= (char string i) #\")
       nil)
      ;; Unicode escape sequence
      ((and (> (- end i) 2)
            (string-equal string "\\u" :start1 i :end1 (+ i 2)))
       (when (<= (- end i) 6)
         (parsing-error i "truncated unicode escape sequence"))
       (incf i 2)
       (let ((code (parse-integer string :start i :end (+ i 4) :radix 16)))
         (cond
           ;; UTF-16 surrogate pair
           ((and (<= #xd800 code #xdbff))
            (incf i 4)
            (when (<= (- end i) 6)
              (parsing-error i "truncated utf-16 surrogate pair"))
            (unless (string-equal string "\\u" :start1 i :end1 (+ i 2))
              (parsing-error i "invalid utf-16 surrogate pair"))
            (incf i 2)
            (let* ((hi code)
                   (lo (parse-integer string :start i :end (+ i 4) :radix 16))
                   (code (+ #x10000 (ash (- hi #xd800) 10) (- lo #xdc00))))
              (vector-push-extend (code-char code) decoded-string))
            (incf i 4))
           ;; Single codepoint
           (t
            (vector-push-extend (code-char code) decoded-string)
            (incf i 4)))))
      ;; Short escape sequence
      ((char= (char string i) #\\)
       (when (>= (1+ i) end)
         (parsing-error i "truncated escape sequence"))
       (incf i)
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
          (parsing-error (1- i) "invalid escape sequence ~A"
                         (subseq string (1- i) (1+ i)))))
       (incf i))
      ;; Unescaped character
      (t
       (vector-push-extend (char string i) decoded-string)
       (incf i)))))

(defun parse-array (string &key (start 0) (end (length string)))
  (when (> (incf *depth-level*) *depth-limit*)
    (depth-limit-exceeded start))
  (do ((i (1+ start))
       (values (make-array 0 :adjustable t :fill-pointer 0)))
      ((and (< i end) (char= (char string i) #\]))
       (values values (- (1+ i) start)))
    (when (>= i end)
      (parsing-error (1- i) "truncated array"))
    ;; Value
    (setf i (skip-spaces string i end))
    (unless (char= (char string i) #\])
      (multiple-value-bind (value value-length)
          (parse-value string :start i :end end)
        (vector-push-extend value values)
        (incf i value-length))
      (setf i (skip-spaces string i end))
      (when (>= i end)
        (parsing-error (1- i) "truncated array")))
    ;; Separator
    (cond
      ((char= (char string i) #\,)
       (incf i))
      ((char/= (char string i) #\])
       (parsing-error i "unexpected character ~S" (char string i))))))

(defun object-append (object entry i)
  (cond
    ((eq *duplicate-object-keys-handling* :keep)
     (push entry object))
    ((assoc (car entry) object :test #'string=)
     (ccase *duplicate-object-keys-handling*
       (:first
        object)
       (:last
        (cons entry (delete (car entry) object :key #'car :test #'string=)))
       (:error
        (parsing-error i "duplicate object entry"))))
    (t
     (push entry object))))

(defun parse-object (string &key (start 0) (end (length string)))
  (when (> (incf *depth-level*) *depth-limit*)
    (depth-limit-exceeded start))
  (do ((i (1+ start))
       (pairs nil))
      ((and (< i end) (char= (char string i) #\}))
       (values (reverse pairs) (- (1+ i) start)))
    (setf i (skip-spaces string i end))
    (when (>= i end)
      (parsing-error (1- i) "truncated object"))
    ;; Entry
    (unless (char= (char string i) #\})
      (let ((entry (cons nil nil))
            (entry-start i))
        ;; Key
        (multiple-value-bind (value value-length)
            (parse-value string :start i :end end)
          (unless (stringp value)
            (parsing-error i "invalid object key"))
          (setf (car entry) value)
          (incf i value-length))
        ;; Colon
        (setf i (skip-spaces string i end))
        (when (>= i end)
          (parsing-error (1- i) "truncated object"))
        (cond
          ((char= (char string i) #\:)
           (incf i))
          (t
           (parsing-error i "unexpected character ~S" (char string i))))
        ;; Value
        (setf i (skip-spaces string i end))
        (multiple-value-bind (value value-length)
            (parse-value string :start i :end end)
          (setf (cdr entry) value)
          (incf i value-length))
        ;; Entry
        (setf pairs (object-append pairs entry entry-start))))
    ;; Separator
    (setf i (skip-spaces string i end))
    (when (>= i end)
      (parsing-error (1- i) "truncated object"))
    (cond
      ((char= (char string i) #\,)
       (incf i))
      ((char/= (char string i) #\})
       (parsing-error i "unexpected character ~S" (char string i))))))

(defun parse-number (string &key (start 0) (end (length string)))
  (let* ((i start)
         (integer-part 0)
         (sign 1)
         (fractional-part nil)
         (exponent nil)
         (exponent-sign 1))
    ;; Integer part
    (when (char= (char string i) #\-)
      (setf sign -1)
      (when (>= (1+ i) end)
        (parsing-error i "truncated number"))
      (incf i))
    (multiple-value-bind (value length)
        (parse-simple-integer string :start i :end end)
      (setf integer-part value)
      (incf i length))
    ;; Fractional part
    (when (and (< i end)
               (char= (char string i) #\.))
      (when (>= (1+ i) end)
        (parsing-error i "truncated number"))
      (incf i)
      (multiple-value-bind (value length)
          (parse-simple-integer string :start i :end end)
        (setf fractional-part (float (* value (expt 10 (- length))) 1.0d0))
        (incf i length)))
    ;; Exponent
    (when (and (< i end)
               (char-equal (char string i) #\e))
      (when (>= (1+ i) end)
        (parsing-error i "truncated number"))
      (incf i)
      (when (member (char string i) (list #\- #\+) :test #'char=)
        (setf exponent-sign (if (char= (char string i) #\-) -1 1))
        (when (>= (1+ i) end)
          (parsing-error i "truncated number"))
        (incf i))
      (multiple-value-bind (value length)
          (parse-simple-integer string :start i :end end)
        (setf exponent value)
        (incf i length)))
    ;; Result
    (values (if (or fractional-part exponent)
                (* (+ integer-part (or fractional-part 0.0d0))
                   sign
                   (expt 10 (* (or exponent 0) exponent-sign)))
                (* integer-part sign))
            (- i start))))

(defun parse-simple-integer (string &key (start 0) (end (length string)))
  (do ((i start)
       (integer 0))
      ((or (= i end)
           (not (char-digit-p (char string i))))
       (values integer (- i start)))
    (setf integer (+ (* integer 10)
                     (- (char-code (char string i))
                        (char-code #\0))))
    (incf i)))

(defun char-space-p (c)
  (member c '(#\Space #\Tab #\Newline #\Return) :test #'char=))

(defun char-digit-p (c)
  (char<= #\0 c #\9))
