(in-package :json)

(deftype pointer ()
  'list)

(define-condition pointer-parse-error (parse-error)
  ((format-control
    :type string
    :initarg :format-control)
   (format-arguments
    :type list
    :initarg :format-arguments))
  (:report
   (lambda (condition stream)
     (with-slots (format-control format-arguments) condition
       (format stream "Invalid JSON pointer: ~?."
               format-control format-arguments)))))

(defun pointer-parse-error (format &rest arguments)
  (error 'pointer-parse-error :format-control format
                              :format-arguments arguments))

(defun parse-pointer (string &key (start 0) (end (length string)))
  (declare (type string string)
           (type (integer 0) start end))
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
      (setf start (1+ token-end)))))

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

(defun pointer-equal (pointer1 pointer2)
  (declare (type pointer pointer1 pointer2))
  (do ((p1 pointer1 (cdr p1))
       (p2 pointer2 (cdr p2)))
      ((or (null p1) (null p2))
       (and (null p1) (null p2)))
    (unless (string= (car p1) (car p2))
      (return nil))))
