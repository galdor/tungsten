(in-package :core)

(defmacro string-case (string &rest clauses)
  (let ((string-var (gensym "STRING-")))
    `(let ((,string-var ,string))
       (declare (ignorable ,string-var))
       (cond
         ,@(mapcar (lambda (clause)
                     (destructuring-bind (keys &rest forms) clause
                       (cond
                         ((member keys '(t otherwise))
                          `(,keys
                            ,@forms))
                         ((listp keys)
                          `((or ,@(mapcar (lambda (key)
                                            `(string= ,string-var ,key))
                                          keys))
                            ,@forms))
                         (t
                          `((string= ,string-var ,keys)
                            ,@forms)))))
                   clauses)))))

(defun split-string (string separator &key (start 0) (end (length string)))
  (declare (type string string)
           (type (or string character) separator)
           (type (integer 0) start end))
  (etypecase separator
    (character
     (split-string/character string separator :start start :end end))
    (string
     (split-string/string string separator :start start :end end))))

(defun split-string/character (string separator
                               &key (start 0) (end (length string)))
  (declare (type string string)
           (type character separator)
           (type (integer 0) start end))
  (do ((parts nil)
       (i start))
      ((> i end)
       (nreverse parts))
    (let* ((s (position separator string :start i :end end))
           (part-end (or s end)))
      (push (subseq string i part-end) parts)
      (setf i (1+ part-end)))))

(defun split-string/string (string separator
                            &key (start 0) (end (length string)))
  (declare (type string string)
           (type string separator)
           (type (integer 0) start end))
  (do ((parts nil)
       (i start))
      ((> i end)
       (nreverse parts))
    (let* ((s (search separator string :test #'string= :start2 i :end2 end))
           (part-end (or s end)))
      (push (subseq string i part-end) parts)
      (setf i (+ part-end (length separator))))))
