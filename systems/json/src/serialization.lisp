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

;;; Mapping Common Lisp values to JSON values is hard. There is no perfect
;;; solution, only preferences. For this serializer, the following choices
;;; were made:
;;;
;;; - Integers lower than 2^53-1 are serialized to JSON integers. Integers
;;;   larger or equal to 2^53 are designated as "large integers". Their JSON
;;;   representation depends of the value of the
;;;   *LARGE-INTEGER-REPRESENTATION* parameter; it can either be an integer
;;;   integer, a floating point approximation, or a character string.
;;;
;;; - Floating point numbers are always serialized to JSON floating point
;;;   values.
;;;
;;; - Characters, strings and pathnames are serialized to JSON strings. For
;;;   pathnames, the full form returned by NAMESTRING is used.
;;;
;;; - Association lists whose keys are either characters, strings, pathnames
;;;   or symbols are serialized to JSON objects.
;;;
;;; - Other lists are serialized to JSON arrays, even if they are not proper.
;;;
;;; - Vectors and multidimensional arrays are serialized to nested JSON
;;;   arrays. Values are read in row-major order.
;;;
;;; - Hash tables are serialized to JSON objects. An error is raised if a key
;;;   cannot be serialized to a JSON string.
;;;
;;; - NIL is serialized to an empty JSON array.
;;;
;;; - T, :TRUE and :FALSE are serialized to a JSON boolean.
;;;
;;; - :NULL is serialized to the JSON null value.
;;;
;;; - Other symbols and keywords are serialized to strings.
;;;
;;; - Complex numbers and ratios cannot be serialized to JSON.
;;;
;;; - Objects are not supported.

(in-package :tungsten.json)

(defparameter *large-integer-representation* :integer
  "The way integers greater than 2^53-1 are serialized. Valid values are:

- :INTEGER: the value is serialized to a JSON integer.
- :FLOAT: the value is serialized to a JSON floating point number.
- :STRING: the value is serialized to a JSON string.")

(defun serialize-to-string (value)
  "Serialize a value to a string."
  (with-output-to-string (stream)
    (serialize value stream)))

(defun serialize (value &optional (stream *standard-output*))
  "Serialize a value to a stream."
  (cond
    ((integerp value)
     (serialize-integer value stream))
    ((floatp value)
     (serialize-float value stream))
    ((stringp value)
     (serialize-string value stream))
    ((characterp value)
     (serialize-string (string value) stream))
    ((pathnamep value)
     (serialize-string (namestring value) stream))
    ((arrayp value)
     (serialize-array value stream))
    ((and (listp value)
          (every (lambda (element)
                   (and (consp element)
                        (typep (car element)
                               '(or string character pathname symbol))
                        (cdr element)))
                 value))
     (serialize-alist value stream))
    ((listp value)
     (serialize-list value stream))
    ((hash-table-p value)
     (serialize-hash-table value stream))
    ((or (eq value t)
         (eq value :true))
     (write-string "true" stream))
    ((eq value :false)
     (write-string "false" stream))
    ((eq value :null)
     (write-string "null" stream))
    ((symbolp value)
     (serialize-string (symbol-name value) stream))
    (t
     (error "cannot serialize value ~S of type ~A" value (type-of value))))
  nil)

(defun serialize-integer (i stream)
  (cond
    ((or (< i #.(1- (expt 2 53)))
         (eq *large-integer-representation* :integer))
     (princ i stream))
    ((eq *large-integer-representation* :float)
     (serialize-float i stream))
    ((eq *large-integer-representation* :string)
     (serialize-string (princ-to-string i) stream))
    (t
     (error "invalid value ~S for *LARGE-INTEGER-REPRESENTATION*"
            *large-integer-representation*))))

(defun serialize-float (f stream)
  (format stream "~F" f))

(defun serialize-string (string stream)
  (write-char #\" stream)
  (do ((i 0))
      ((>= i (length string)))
    (let ((idx (position-if #'char-escape-p string :start i)))
      (cond
        (idx
         (write-string string stream :start i :end idx)
         (let ((c (char string idx)))
           (cond
             ((char= c #\")
              (write-string "\\\"" stream))
             ((char= c #\\)
              (write-string "\\\\" stream))
             ((char= c #\Backspace)
              (write-string "\\b" stream))
             ((char= c #\Page)
              (write-string "\\f" stream))
             ((char= c #\Newline)
              (write-string "\\n" stream))
             ((char= c #\Return)
              (write-string "\\r" stream))
             ((char= c #\Tab)
              (write-string "\\t" stream))
             (t
              (format stream "\\u~4,'0x" (char-code c)))))
         (setf i (1+ idx)))
        (t
         (write-string string stream :start i)
         (setf i (length string))))))
  (write-char #\" stream))

(defun char-escape-p (c)
  (or (<= #x00 (char-code c) #x1f)
      (char= c #\")
      (char= c #\\)))

(defun serialize-array (array stream)
  (let ((offset 0))
    (labels ((aux (dimensions)
               (cond
                 ((= (length dimensions) 1)
                  (write-char #\[ stream)
                  (dotimes (i (car dimensions))
                    (when (> i 0)
                      (write-char #\, stream))
                    (serialize (row-major-aref array offset) stream)
                    (incf offset))
                  (write-char #\] stream)
                  array)
                 (t
                  (write-char #\[ stream)
                  (dotimes (i (car dimensions))
                    (when (> i 0)
                      (write-char #\, stream))
                    (aux (cdr dimensions)))
                  (write-char #\] stream)))))
      (aux (array-dimensions array)))))

(defun serialize-list (list stream)
  (write-char #\[ stream)
  (let ((i 0))
    (labels ((aux (value)
               (cond
                 ((and (listp value) (endp value))
                  nil)
                 (t
                  (let ((element (if (listp value) (car value) value)))
                    (when (> i 0)
                      (write-char #\, stream))
                    (serialize element stream)
                    (incf i))
                  (when (listp value)
                    (aux (cdr value)))))))
      (aux list)))
  (write-char #\] stream))

(defun serialize-key (value stream)
  ;; Force the conversion to string here so that :TRUE, :FALSE and :NULL
  ;; are serialized to JSON strings and not booleans or null values.
  (serialize (if (symbolp value)
                 (symbol-name value)
                 value)
             stream))

(defun serialize-alist (list stream)
  (write-char #\{ stream)
  (let ((i 0))
    (dolist (entry list)
      (when (> i 0)
        (write-char #\, stream))
      (serialize-key (car entry) stream)
      (write-char #\: stream)
      (serialize (cdr entry) stream)
      (incf i)))
  (write-char #\} stream))

(defun serialize-hash-table (table stream)
  (write-char #\{ stream)
  (let ((i 0))
    (maphash (lambda (key value)
               (when (> i 0)
                 (write-char #\, stream))
               (serialize-key key stream)
               (write-char #\: stream)
               (serialize value stream)
               (incf i))
             table))
  (write-char #\} stream))
