(in-package :uri-test)

(defmacro check-parser (&rest tests)
  `(progn
     ,@(mapcar
        (lambda (test)
          (destructuring-bind (values uri-string &rest parse-args) test
            (let ((uri (gensym "URI-"))
                  (label (format nil "URI ~S" uri-string)))
              `(let ((,uri (uri:parse ,uri-string ,@parse-args)))
                 ,@(mapcar
                    (lambda (spec)
                      (destructuring-bind (check keyword accessor)
                          spec
                        (if (getf values keyword)
                            `(,check ,(getf values keyword) (,accessor ,uri)
                                     :label ,label)
                            `(check-null (,accessor ,uri) :label ,label))))
                    '((check-string= :scheme uri:uri-scheme)
                      (check-string= :username uri:uri-username)
                      (check-string= :password uri:uri-password)
                      (check-string= :host uri:uri-host)
                      (check-equal :port uri:uri-port)
                      (check-string= :path uri:uri-path)
                      (check-equal :query uri:uri-query)
                      (check-string= :fragment uri:uri-fragment)))))))
        tests)))

(deftest parser/no-authority ()
  (check-parser
   ((:scheme "file")
    "file:")
   ((:scheme "file+foo-1.1")
    "file+foo-1.1:")
   ((:scheme "file" :path "/")
    "file:/")
   ((:scheme "file" :path "/foo/bar")
    "file:/foo/bar")
   ((:scheme "file" :path "foo/bar")
    "file:foo/bar")
   ((:scheme "file" :path ".")
    "file:.")
   ((:scheme "file" :path "/foo" :query '(("a" . "1")))
    "file:/foo?a=1")
   ((:scheme "file" :path "/foo" :fragment "bye")
    "file:/foo#bye")
   ((:scheme "file" :query '(("a" . "1")))
    "file:?a=1")
   ((:scheme "file" :fragment "bye")
    "file:#bye")
   ((:scheme "file" :query '(("a" . "b")) :fragment "bye")
    "file:?a=b#bye")))

(deftest parser/no-userinfo ()
  (check-parser
   ((:scheme "http" :host "example.com")
    "http://example.com")
   ((:scheme "http" :host "example.com" :port 80)
    "http://example.com:80")
   ((:scheme "http" :host "example.com" :query '(("a" . "1")))
    "http://example.com?a=1")
   ((:scheme "http" :host "example.com" :fragment "bye")
    "http://example.com#bye")
   ((:scheme "http" :host "example.com" :query '(("a" . "1")) :fragment "bye")
    "http://example.com?a=1#bye")
   ((:scheme "http" :host "example.com" :path "/" :query '(("a" . "1")))
    "http://example.com/?a=1")
   ((:scheme "http" :host "example.com" :path "/" :fragment "bye")
    "http://example.com/#bye")
   ((:scheme "http" :host "example.com" :path "/a/b/c"
     :query '(("a" . "b")) :fragment "bye")
    "http://example.com/a/b/c?a=b#bye")))

(deftest parser/userinfo ()
  (check-parser
   ((:scheme "http" :username "bob" :host "example.com")
    "http://bob@example.com")
   ((:scheme "http" :username "bob" :host "example.com" :port 80)
    "http://bob@example.com:80")
   ((:scheme "http" :username "bob" :password "foo" :host "example.com")
    "http://bob:foo@example.com")
   ((:scheme "http" :username "bob" :password "" :host "example.com")
    "http://bob:@example.com")
   ((:scheme "http" :username "" :password "foo" :host "example.com")
    "http://:foo@example.com")
   ((:scheme "http" :username "" :password "" :host "example.com")
    "http://:@example.com")
   ((:scheme "http" :username "bob" :password "foo:bar" :host "example.com")
    "http://bob:foo:bar@example.com")
   ((:scheme "http" :username "mónica" :password "𠜎𠜱" :host "example.com")
    "http://m%C3%B3nica:%F0%A0%9C%8E%F0%A0%9C%B1@example.com")))

(deftest parser/host-port ()
  (check-parser
   ((:scheme "http" :host "10.0.150.3")
    "http://10.0.150.3")
   ((:scheme "http" :host "::1")
    "http://[::1]")
   ((:scheme "http" :host "fc00:0001:0002:0003:0004:0005")
    "http://[fc00:0001:0002:0003:0004:0005]")
   ((:scheme "http" :host "v1.fc00::1%2542")
    "http://[v1.fc00::1%2542]")
   ((:scheme "http" :host "foo.bar.example.com")
    "http://foo.bar.example.com")
   ((:scheme "http" :host "example.com.")
    "http://example.com.")
   ((:scheme "http" :host "net.")
    "http://net.")
   ((:scheme "http" :host ".")
    "http://.")
   ((:scheme "http" :host "example.com" :port 80)
    "http://example.com:80")
   ((:scheme "http" :host "example.com")
    "http://example.com:")))

(deftest parser/path ()
  (check-parser
   ((:scheme "file" :host "")
    "file://")
   ((:scheme "file" :host "" :path "/")
    "file:///")
   ((:scheme "file" :host "" :path "/a/b/c")
    "file:///a/b/c")
   ((:scheme "file" :host "" :path "/a/b/c/")
    "file:///a/b/c/")
   ((:scheme "file" :host "" :path "/./etc/passwd")
    "file:///./etc/passwd")
   ((:scheme "file" :host "" :path "/foo//bar///")
    "file:///foo//bar///")
   ((:scheme "file" :path "foo")
    "file:foo")
   ((:scheme "file" :path "foo/")
    "file:foo/")
   ((:scheme "file" :path "/dir;v=1.0/file")
    "file:/dir;v=1.0/file")
   ((:scheme "file" :path "/:/@")
    "file:/:/@")
   ((:scheme "file" :path ":")
    "file::")))

(deftest parser/query ()
  (check-parser
   ((:scheme "http" :query '())
    "http:?")
   ((:scheme "http" :host "" :query '())
    "http://?")
   ((:scheme "http" :host "" :query '(("a" . "1")))
    "http://?a=1")
   ((:scheme "http" :host "" :query '(("foo" . "/   /") ("bar" . "?")))
    "http://?foo=/+%20+/&bar=?")
   ((:scheme "http" :host "" :query '(("a" . "(b") ("c)@d" . "")))
    "http://?a=(b&c)@d")
   ((:scheme "http" :host "" :query '(("foo" . "") ("" . "bar") ("" . "")))
    "http://?foo=&=bar&=")
   ((:scheme "http" :host "" :query '(("=foo&" . "&bar=baz&")))
    "http://?%3Dfoo%26=%26bar%3Dbaz%26")
   ((:scheme "http" :host "" :query '(("a" . "b c d") ("été" . "à")))
    "http://?a=b%20c%20d&%C3%A9t%C3%A9=%C3%A0")))

(deftest parser/fragment ()
  (check-parser
   ((:scheme "http" :fragment "")
    "http:#")
   ((:scheme "http" :host "" :fragment "")
    "http://#")
   ((:scheme "http" :host "" :fragment "foo")
    "http://#foo")
   ((:scheme "http" :host "" :fragment "foo bar")
    "http://#foo%20bar")
   ((:scheme "http" :host "" :fragment "foo/bar")
    "http://#foo/bar")
   ((:scheme "http" :host "" :fragment "?a/b/c")
    "http://#?a/b/c")
   ((:scheme "http" :host "" :fragment "#")
    "http://#%23")))

(deftest parser/relative-reference ()
  (check-parser
   (()
    "")
   ((:path "foo")
    "foo")
   ((:path "./")
    "./")
   ((:path "/foo/bar")
    "/foo/bar")
   ((:host "example.com")
    "//example.com")
   ((:username "bob" :password "foo" :host "example.com")
    "//bob:foo@example.com")
   ((:path "foo" :query '(("a" . "b")) :fragment "bar")
    "foo?a=b#bar")))

(deftest parser/boundaries ()
  (check-parser
   ((:scheme "http")
    "abhttp:" :start 2)
   ((:host "ex")
    "//example.com" :end 4)
   ((:path "/a")
    "file:///a/b/c" :start 7 :end 9)))
