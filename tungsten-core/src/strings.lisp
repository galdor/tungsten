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
