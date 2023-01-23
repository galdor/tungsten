(in-package :text-test)

(defmacro check-encode-base64 (&rest checks)
  `(progn
     ,@(mapcar (lambda (check)
                 `(check:check-string=
                   ,(car check)
                   (text:encode-base64
                    (text:encode-string ,(cadr check)) ,@(cddr check))))
               checks)))

(deftest encode-base64 ()
  ;; See RFC 4648 10. Test Vectors
  (check-encode-base64
   ("" "")
   ("Zg==" "f")
   ("Zm8=" "fo")
   ("Zm9v" "foo")
   ("Zm9vYg==" "foob")
   ("Zm9vYmE=" "fooba")
   ("Zm9vYmFy" "foobar")
   ("Zm9vYmFy" "ABCfoobarABC" :start 3 :end 9)))

(defmacro check-decode-base64 (&rest checks)
  `(progn
     ,@(mapcar (lambda (check)
                 `(check:check-string=
                   ,(car check)
                   (text:decode-string
                    (text:decode-base64 ,(cadr check) ,@(cddr check)))))
               checks)))

(deftest decode-base64 ()
  ;; See RFC 4648 10. Test Vectors
  (check-decode-base64
   ("" "")
   ("f" "Zg==")
   ("fo" "Zm8=")
   ("foo" "Zm9v")
   ("foob" "Zm9vYg==")
   ("fooba" "Zm9vYmE=")
   ("foobar" "Zm9vYmFy")
   ("foobar" "[[[Zm9vYmFy]]]" :start 3 :end 11)))
