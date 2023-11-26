(in-package :json)

(deftype large-integer-representation ()
  '(member :integer :float :string))

(defparameter *large-integer-representation* :integer)

(define-condition unserializable-value (error)
  ((value
    :initarg :value
    :reader unserializable-value-value))
  (:report
   (lambda (condition stream)
     (format stream "value ~S cannot be represented in JSON"
             (unserializable-value-value condition)))))

(defun serialize (value &key stream mapping)
  (declare (type (or stream null) stream))
  (let ((encoded-value (if mapping
                           (generate value mapping)
                           value)))
    (if stream
        (serialize* encoded-value stream)
        (with-output-to-string (stream)
          (serialize* encoded-value stream)))))

(defun serialize* (value stream)
  (declare (type stream stream))
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
     (error 'unserializable-value :value value)))
  nil)

(defun serialize-integer (i stream)
  (if (< i #.(1- (expt 2 53)))
      (princ i stream)
      (ecase *large-integer-representation*
        (:integer
         (princ i stream))
        (:float
         (serialize-float i stream))
        (:string
         (serialize-string (princ-to-string i) stream)))))

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
              (format stream "\\u~4,'0X" (char-code c)))))
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
                    (serialize* (row-major-aref array offset) stream)
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
                    (serialize* element stream)
                    (incf i))
                  (when (listp value)
                    (aux (cdr value)))))))
      (aux list)))
  (write-char #\] stream))

(defun serialize-key (value stream)
  ;; Force the conversion to string here so that :TRUE, :FALSE and :NULL
  ;; are serialized to JSON strings and not booleans or null values.
  (serialize* (if (symbolp value)
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
      (serialize* (cdr entry) stream)
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
               (serialize* value stream)
               (incf i))
             table))
  (write-char #\} stream))
