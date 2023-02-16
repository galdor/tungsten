(in-package :mime-test)

(deftest serialize-media-type ()
  (check-string=
   "text/plain"
   (mime:serialize-media-type
    (mime:make-media-type "text" "plain")))
  (check-string=
   "text/html;charset=UTF-8"
   (mime:serialize-media-type
    (mime:make-media-type "text" "html"
                          :parameters '(("charset" . "UTF-8")))))
  (check-string=
   "x-tungsten/test;foo=a;bar=b"
   (mime:serialize-media-type
    (mime:make-media-type "x-tungsten" "test"
                          :parameters '(("foo" . "a")
                                        ("bar" . "b"))))))
