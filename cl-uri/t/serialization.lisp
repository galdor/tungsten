(in-package :uri-test)

(defmacro check-serialization (&rest tests)
  `(progn
     ,@(mapcar (lambda (test)
                 `(check-string= ,(car test)
                                 (uri:serialize
                                  (uri:make-uri ,@(cadr test)))))
               tests)))

(deftest serialize ()
  (check-serialization
   ("//example.com"
    (:host "example.com"))
   ("http:"
    (:scheme "http"))
   ("http://example.com"
    (:scheme "http" :host "example.com"))
   ("http://example.com:80"
    (:scheme "http" :host "example.com" :port 80))
   ("http://127.0.0.1:80"
    (:scheme "http" :host "127.0.0.1" :port 80))
   ("http://bob@example.com"
    (:scheme "http" :host "example.com" :username "bob"))
   ("http://:foo@example.com"
    (:scheme "http" :host "example.com" :password "foo"))
   ("http://john%20doe:foo%20bar@example.com"
    (:scheme "http" :host "example.com"
     :username "john doe" :password "foo bar"))
   ("http://m%C3%B3nica:%F0%A0%9C%8E%F0%A0%9C%B1@example.com"
    (:scheme "http" :host "example.com"
     :username "mónica" :password "𠜎𠜱"))
   ("http://example.com/foo/bar%20baz"
    (:scheme "http" :host "example.com" :path "/foo/bar baz"))
   ("http://example.com/"
    (:scheme "http" :host "example.com" :path "/"))
   ("http://example.com?foo=%C3%A9t%C3%A9&x%20y=&=hello"
    (:scheme "http" :host "example.com"
     :query '(("foo" . "été") ("x y" . "") ("" . "hello"))))
   ("http://example.com"
    (:scheme "http" :host "example.com" :query '()))
   ("http://example.com#foo"
    (:scheme "http" :host "example.com" :fragment "foo"))
   ("http://example.com"
    (:scheme "http" :host "example.com" :fragment ""))
   ("http://example.com?a=b#foo"
    (:scheme "http" :host "example.com"
     :query '(("a" . "b"))  :fragment "foo"))
   ("http://example.com/#%C3%A9t%C3%A9"
    (:scheme "http" :host "example.com" :path "/" :fragment "été"))
   ("http://example.com#/foo%20bar?"
    (:scheme "http" :host "example.com" :fragment "/foo bar?"))
   ("http://example.com?a%3Db=c%3Dd"
    (:scheme "http" :host "example.com" :query '(("a=b" . "c=d"))))))
