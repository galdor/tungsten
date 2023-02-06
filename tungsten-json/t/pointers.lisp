(in-package :json-test)

(deftest parse-pointer ()
  (macrolet ((check-pointer (expected-pointer string)
               (let ((pointer (gensym "POINTER-")))
                 `(let ((,pointer (json:parse-pointer ,string)))
                    (unless (json:pointer-equal ,pointer ,expected-pointer)
                      (fail "~S was parsed as ~S which is not POINTER-EQUAL ~
                             to ~S" ,string ,pointer ,expected-pointer))))))
    (check-pointer '() "")
    (check-pointer '("foo") "/foo")
    (check-pointer '("foo" "bar") "/foo/bar")
    (check-pointer '("x" "y" "z") "/x/y/z")
    (check-pointer '("" "abc" "" "") "//abc//")
    (check-pointer '("~foo~" "/a/b/c/") "/~0foo~0/~1a~1b~1c~1")
    (check-signals json:pointer-parse-error (json:parse-pointer "foo"))
    (check-signals json:pointer-parse-error (json:parse-pointer "/fo~"))
    (check-signals json:pointer-parse-error (json:parse-pointer "/~fo"))))

(deftest serialize-pointer ()
  (macrolet ((check-pointer (string pointer)
               `(check-string= ,string (json:serialize-pointer ,pointer))))
    (check-pointer "" '())
    (check-pointer "/foo" '("foo"))
    (check-pointer "/foo/bar" '("foo" "bar"))
    (check-pointer "/x/y/z" '("x" "y" "z"))
    (check-pointer "//abc//" '("" "abc" "" ""))
    (check-pointer "/~0foo~0/~1a~1b~1c~1" '("~foo~" "/a/b/c/"))))

(deftest pointer-equal ()
  (check-true (json:pointer-equal '() '()))
  (check-true (json:pointer-equal '("a") '("a")))
  (check-true (json:pointer-equal '("a" "b") '("a" "b")))
  (check-false (json:pointer-equal '("a") '("b")))
  (check-false (json:pointer-equal '("a" "b") '("a" "c")))
  (check-false (json:pointer-equal '("a") '("a" "c")))
  (check-false (json:pointer-equal '() '("a"))))
