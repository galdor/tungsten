(in-package :float-test)

(defun double-float= (a b)
  (declare (type double-float a b))
  (< (- a b) double-float-epsilon))

(defmacro check-parse (&rest checks)
  `(progn
     ,@(mapcar (lambda (check)
                 `(check-true
                   (double-float= ,(car check)
                                  (float:parse ,(cadr check) ,@(cddr check)))))
               checks)))

(deftest parse ()
  (check-parse
   (-0.0d0 "-0.0")
   (0.0d0 "+0.0")
   (123d0 "123.0")
   (123d0 "123e0")
   (1230d0 "123e1")
   (123000d0 "123e3")
   (123.4d0 "123.4e0")
   (1234d0 "123.4e1")
   (123400d0 "123.4e3")
   (-123.4d0 "-123.4e-0")
   (-12.34d0 "-123.4e-1")
   (-0.1234d0 "-123.4e-3")
   (0.00000000123456d0 "123.456e-11")
   (-12345600000000d0 "-123.456e11")))
