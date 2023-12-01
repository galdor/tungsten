(in-package :core-test)

(defmacro check-string-starts-with (&rest checks)
  `(progn
     ,@(mapcar
        (lambda (check)
          `(check-eq ,(first check)
                     (core:string-starts-with ,(second check) ,(third check)
                                              ,@(cdddr check))))
        checks)))

(deftest string-starts-with ()
  (check-string-starts-with
   (t "" "")
   (t "a" "a")
   (t "foo" "foo")
   (t "foo" "fo")
   (t "foo" "f")
   (t "foo" "")
   (t "foobar" "abarc" :start1 3 :start2 1 :end2 3)
   (nil "foo" "x")
   (nil "foo" "fox")
   (nil "foo" "foobar")
   (nil "foobar" "oob" :start1 3 :end1 5)))

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
