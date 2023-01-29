(in-package :text-test)

(defmacro check-encode-hex-string (&rest checks)
  `(progn
     ,@(mapcar (lambda (check)
                 `(check-string=
                   ,(car check)
                   (text:encode-hex-string (core:octet-vector,(cadr check))
                                           ,@(cddr check))))
               checks)))

(deftest encode-hex-string ()
  (check-encode-hex-string
   ("" #())
   ("00010203" #(0 1 2 3))
   ("0a0f107f80ff" #(10 15 16 127 128 255))
   ("040506" #(1 2 3 4 5 6 7 8) :start 3 :end 6)))
