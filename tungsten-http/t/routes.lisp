(in-package :http-test)

(deftest route-path-equal ()
  (check-true (http:route-path-equal '() '()))
  (check-true (http:route-path-equal '("a") '("a")))
  (check-true (http:route-path-equal '("a" :b *) '("a" :b *)))
  (check-false (http:route-path-equal '() '("a")))
  (check-false (http:route-path-equal '(:a) '("a")))
  (check-false (http:route-path-equal '("a") '("a" "b"))))

(deftest parse-router-path ()
  (macrolet ((check-path (path string)
               `(check-is 'http:route-path-equal
                          ,path (http:parse-route-path ,string))))
    (check-signals error (http:parse-route-path ""))
    (check-signals error (http:parse-route-path "foo/bar"))
    (check-signals error (http:parse-route-path "/*/foo"))
    (check-signals error (http:parse-route-path "/foo/*/bar"))
    (check-signals error (http:parse-route-path "/foo/:*"))
    (check-path '() "/")
    (check-path '("foo") "/foo")
    (check-path '("foo" "bar" "baz") "/foo/bar/baz")
    (check-path '("foo" "bar" "baz") "/foo/bar/baz/")
    (check-path '("a" :*) "/a/*")
    (check-path '(:*) "/*/")
    (check-path '(:a "b" :b) "/:a/b/:b")
    (check-path '(:a :*) "/:a/*")))

(deftest match-route-path ()
  (macrolet ((check-match (expected-variables target-string route-path-string)
               (let ((route-path (gensym "ROUTE-PATH-"))
                     (match (gensym "MATCH-"))
                     (variables (gensym "VARIABLES-")))
                 `(let ((,route-path
                          (http:parse-route-path ,route-path-string)))
                    (multiple-value-bind (,match ,variables)
                        (http::match-route-path
                         (make-instance 'http:route :path ,route-path)
                         (uri:parse ,target-string))
                      (unless ,match
                        (fail "target ~S does not match route path ~S"
                              ,target-string ,route-path-string))
                      (unless (equal ,expected-variables ,variables)
                        (fail "matching target ~S to route path ~S ~
                                     yielded variables ~S instead of ~S"
                              ,target-string ,route-path-string
                              ,variables ,expected-variables))))))
             (check-no-match (target-string route-path-string)
               (let ((route-path (gensym "ROUTE-PATH-"))
                     (match (gensym "MATCH-")))
                 `(let* ((,route-path
                           (http:parse-route-path ,route-path-string))
                         (,match (http::match-route-path
                                  (make-instance 'http:route :path ,route-path)
                                  (uri:parse ,target-string))))
                    (when ,match
                      (fail "target ~S matches route path ~S"
                            ,target-string ,route-path-string))))))
    (check-match nil "/" "/")
    (check-match nil "/a" "/a")
    (check-match nil "/a/" "/a")
    (check-match nil "/a" "/a/")
    (check-match nil "/a/b/c" "/a/b/c")
    (check-match nil "/a/b/c/" "/a/b/c")
    (check-match nil "/a/b/c" "/a/b/c/")
    (check-no-match "/a" "/")
    (check-no-match "/a/" "/")
    (check-no-match "/a/b/c" "/a/b")
    (check-no-match "/a/b/c/" "/a/b")
    (check-match '((:* . "a"))  "/a" "/*")
    (check-match '((:* . "a/b/c"))  "/a/b/c" "/*")
    (check-match '((:* . "b/c"))  "/a/b/c" "/a/*")
    (check-match '((:* . "c"))  "/a/b/c" "/a/b/*")
    (check-match '((:* . "c"))  "/a/b/c/" "/a/b/*")
    (check-match '((:* . "c"))  "/a/b/c" "/a/b/*/")
    (check-no-match "/b/c" "/a/b/*")
    (check-no-match "/a/b" "/a/b/*")
    (check-match '((:x . "a")) "/a" "/:x")
    (check-match '((:x . "a") (:y . "c")) "/a/b/c" "/:x/b/:y")
    (check-no-match "/a/b/c" "/a/:x/d")
    (check-match '((:x . "a") (:* . "b/c")) "/a/b/c" "/:x/*")))