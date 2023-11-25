(in-package :core-test)

(defmacro check-split-string (&rest checks)
  `(progn
     ,@(mapcar
        (lambda (check)
          `(check-equalp ',(first check)
                         (core:split-string ,(second check) ,(third check))))
        checks)))

(deftest split-string/character ()
  (check-split-string
   (("") "" #\,)
   (("foo") "foo" #\,)
   (("foo" "bar") "foo,bar" #\,)
   (("foo" "bar" "baz") "foo,bar,baz" #\,)
   (("a" "b" "c") "a,b,c" #\,)
   (("" "") "," #\,)
   (("" "" "") ",," #\,)
   (("" "foo") ",foo" #\,)
   (("" "" "foo") ",,foo" #\,)
   (("foo" "") "foo," #\,)
   (("foo" "" "") "foo,," #\,)))

(deftest split-string/string ()
  (check-split-string
   (("") "" "[]")
   (("foo") "foo" "[]")
   (("foo" "bar") "foo[]bar" "[]")
   (("foo" "bar" "baz") "foo[]bar[]baz" "[]")
   (("a" "b" "c") "a[]b[]c" "[]")
   (("" "") "[]" "[]")
   (("" "" "") "[][]" "[]")
   (("" "foo") "[]foo" "[]")
   (("" "" "foo") "[][]foo" "[]")
   (("foo" "") "foo[]" "[]")
   (("foo" "" "") "foo[][]" "[]")))
