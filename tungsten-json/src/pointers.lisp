(in-package :json)

(deftype pointer ()
  'list)

(deftype child-pointer ()
  '(or list string (integer 0)))

(define-condition pointer-parse-error (parse-error)
  ((format-control
    :type string
    :initarg :format-control
    :reader pointer-parse-error-format-control)
   (format-arguments
    :type list
    :initarg :format-arguments
    :reader pointer-parse-error-format-arguments))
  (:report
   (lambda (condition stream)
     (format stream "invalid JSON pointer: ~?"
             (pointer-parse-error-format-control condition)
             (pointer-parse-error-format-arguments condition)))))

(define-condition invalid-pointer (error)
  ((pointer
    :type pointer
    :initarg :pointer
    :reader invalid-pointer-pointer))
  (:report
   (lambda (condition stream)
     (format stream "JSON pointer ~S does not match the structure of the data"
             (serialize-pointer (invalid-pointer-pointer condition))))))

(defun pointer-parse-error (format &rest arguments)
  (error 'pointer-parse-error :format-control format
                              :format-arguments arguments))

(defun parse-pointer (string &key (start 0) end)
  (declare (type string string)
           (type (integer 0) start)
           (type (or (integer 0) null) end))
  (let ((end (or end (length string))))
    (when (>= start end)
      (return-from parse-pointer nil))
    (unless (char= (char string start) #\/)
      (pointer-parse-error "token does not start with a ~S character" #\/))
    (incf start)
    (do ((tokens nil))
        ((> start end)
         (nreverse tokens))
      (let ((token-end (or (position #\/ string :start start :end end) end)))
        (push (parse-token string start token-end) tokens)
        (setf start (1+ token-end))))))

(defun parse-token (string start end)
  (declare (type string)
           (type (integer 0) start end))
  (with-output-to-string (stream)
    (do ()
        ((>= start end)
         nil)
      (let* ((position (position #\~ string :start start :end end))
             (part-end (or position end)))
        (write-string string stream :start start :end part-end)
        (when position
          (when (>= (1+ position) end)
            (pointer-parse-error "truncated escape sequence"))
          (case (char string (1+ position))
            (#\0
             (write-char #\~ stream)
             (incf start))
            (#\1
             (write-char #\/ stream)
             (incf start))
            (t
             (pointer-parse-error "invalid escape sequence ~S"
                                  (subseq string position (+ position 2))))))
        (setf start (+ part-end 2))))))

(defun serialize-pointer (pointer)
  (declare (type pointer pointer))
  (with-output-to-string (stream)
    (dolist (token pointer)
      (write-char #\/ stream)
      (write-pointer-token token stream))))

(defun write-pointer-token (token stream)
  (declare (type string token)
           (type stream stream))
  (flet ((special-character-p (character)
           (or (char= character #\/)
               (char= character #\~))))
    (do ((start 0)
         (end (length token)))
        ((>= start end)
         nil)
      (let* ((position (position-if #'special-character-p token :start start))
             (part-end (or position end)))
        (write-string token stream :start start :end part-end)
        (when position
          (ecase (char token position)
            (#\/ (write-string "~1" stream))
            (#\~ (write-string "~0" stream))))
        (setf start (1+ part-end))))))

(defun pointer (pointer)
  (declare (type (or pointer string) pointer))
  (etypecase pointer
    (pointer
     pointer)
    (string
     (parse-pointer pointer))))

(defun pointer* (&rest pointers)
  (declare (type list pointers))
  (let ((pointer nil))
    (dolist (element pointers (nreverse pointer))
      (etypecase element
        ((or string (integer 0))
         (push element pointer))
        (list
         (setf pointer (append (reverse element) pointer)))))))

(defun make-pointer ()
  (list))

(defun parent-pointer (pointer)
  (declare (type pointer pointer))
  (butlast pointer))

(defun child-pointer (child-pointer pointer)
  (declare (type child-pointer child-pointer)
           (type pointer pointer))
  (let ((child-pointer (etypecase child-pointer
                         (pointer child-pointer)
                         (string (list child-pointer))
                         (integer (list (princ-to-string child-pointer))))))
    (concatenate 'list pointer child-pointer)))

(defun pointer-equal (pointer1 pointer2)
  (declare (type pointer pointer1 pointer2))
  (do ((p1 pointer1 (cdr p1))
       (p2 pointer2 (cdr p2)))
      ((or (null p1) (null p2))
       (and (null p1) (null p2)))
    (unless (string= (car p1) (car p2))
      (return nil))))

(defun pointer-ref (pointer data)
  (declare (type (or pointer string) pointer))
  (let ((pointer (pointer pointer)))
    (labels ((ref (subpointer data)
               (cond
                 ((null subpointer)
                  data)
                 ((listp data)
                  (let* ((key (car subpointer))
                         (member (assoc key data :test #'string=)))
                    (unless member
                      (error 'invalid-pointer :pointer pointer))
                    (ref (cdr subpointer) (cdr member))))
                 ((vectorp data)
                  (let ((index (parse-integer (car subpointer))))
                    (unless (<= 0 index (1- (length data)))
                      (error 'invalid-pointer :pointer pointer))
                    (ref (cdr subpointer) (aref data index))))
                 (t
                  (error 'invalid-pointer :pointer pointer)))))
      (ref pointer data))))
