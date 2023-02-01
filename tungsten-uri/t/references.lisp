(in-package :uri-test)

(defmacro check-resolve-reference (&rest tests)
  `(progn
     ,@(mapcar
        (lambda (test)
          (destructuring-bind (expected (reference base)) test
            `(check-string= ,expected
                            (uri:serialize
                             (uri:resolve-reference ,reference ,base)))))
        tests)))

(deftest resolve-reference ()
  ;; See RFC 3986 5.4.1 and 5.4.2.
  (check-resolve-reference
   ("g:h"
    ("g:h" "http://a/b/c/d;p?q"))
   ("http://a/b/c/g"
    ("g" "http://a/b/c/d;p?q"))
   ("http://a/b/c/g"
    ("./g" "http://a/b/c/d;p?q"))
   ("http://a/b/c/g/"
    ("g/" "http://a/b/c/d;p?q"))
   ("http://a/g"
    ("/g" "http://a/b/c/d;p?q"))
   ("http://g"
    ("//g" "http://a/b/c/d;p?q"))
   ("http://a/b/c/d;p?y="
    ("?y" "http://a/b/c/d;p?q"))
   ("http://a/b/c/g?y="
    ("g?y" "http://a/b/c/d;p?q"))
   ("http://a/b/c/d;p?q=#s"
    ("#s" "http://a/b/c/d;p?q"))
   ("http://a/b/c/g#s"
    ("g#s" "http://a/b/c/d;p?q"))
   ("http://a/b/c/g?y=#s"
    ("g?y#s" "http://a/b/c/d;p?q"))
   ("http://a/b/c/;x"
    (";x" "http://a/b/c/d;p?q"))
   ("http://a/b/c/g;x"
    ("g;x" "http://a/b/c/d;p?q"))
   ("http://a/b/c/g;x?y=#s"
    ("g;x?y#s" "http://a/b/c/d;p?q"))
   ("http://a/b/c/d;p?q="
    ("" "http://a/b/c/d;p?q"))
   ("http://a/b/c/"
    ("." "http://a/b/c/d;p?q"))
   ("http://a/b/c/"
    ("./" "http://a/b/c/d;p?q"))
   ("http://a/b/"
    (".." "http://a/b/c/d;p?q"))
   ("http://a/b/"
    ("../" "http://a/b/c/d;p?q"))
   ("http://a/b/g"
    ("../g" "http://a/b/c/d;p?q"))
   ("http://a/"
    ("../.." "http://a/b/c/d;p?q"))
   ("http://a/"
    ("../../" "http://a/b/c/d;p?q"))
   ("http://a/g"
    ("../../g" "http://a/b/c/d;p?q"))
   ("http://a/g"
    ("../../../g" "http://a/b/c/d;p?q"))
   ("http://a/g"
    ("../../../../g" "http://a/b/c/d;p?q"))
   ("http://a/g"
    ("/./g" "http://a/b/c/d;p?q"))
   ("http://a/g"
    ("/../g" "http://a/b/c/d;p?q"))
   ("http://a/b/c/g."
    ("g." "http://a/b/c/d;p?q"))
   ("http://a/b/c/.g"
    (".g" "http://a/b/c/d;p?q"))
   ("http://a/b/c/g.."
    ("g.." "http://a/b/c/d;p?q"))
   ("http://a/b/c/..g"
    ("..g" "http://a/b/c/d;p?q"))
   ("http://a/b/g"
    ("./../g" "http://a/b/c/d;p?q"))
   ("http://a/b/c/g/"
    ("./g/." "http://a/b/c/d;p?q"))
   ("http://a/b/c/g/h"
    ("g/./h" "http://a/b/c/d;p?q"))
   ("http://a/b/c/h"
    ("g/../h" "http://a/b/c/d;p?q"))
   ("http://a/b/c/g;x=1/y"
    ("g;x=1/./y" "http://a/b/c/d;p?q"))
   ("http://a/b/c/y"
    ("g;x=1/../y" "http://a/b/c/d;p?q"))
   ("http://a/b/c/g?y%2F.%2Fx="
    ("g?y/./x" "http://a/b/c/d;p?q"))
   ("http://a/b/c/g?y%2F..%2Fx="
    ("g?y/../x" "http://a/b/c/d;p?q"))
   ("http://a/b/c/g#s/./x"
    ("g#s/./x" "http://a/b/c/d;p?q"))
   ("http://a/b/c/g#s/../x"
    ("g#s/../x" "http://a/b/c/d;p?q"))
   ("http:g"
    ("http:g" "http://a/b/c/d;p?q"))))
