(in-package :uri-test)

(deftest uri-relative-path-p ()
  (check-true (uri:uri-relative-reference-p
               (uri:make-uri :path "foo")))
  (check-true (uri:uri-relative-reference-p
               (uri:make-uri :host "example.com")))
  (check-false (uri:uri-relative-reference-p
                (uri:make-uri :scheme "https" :host "example.com")))
  (check-false (uri:uri-relative-reference-p
                (uri:make-uri :scheme "https" :path "foo"))))
