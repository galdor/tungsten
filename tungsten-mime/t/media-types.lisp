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

(deftest parse-media-type ()
  (macrolet ((check-media-type (expected string)
               `(check-string= ,expected (mime:serialize-media-type
                                          (mime:parse-media-type ,string)))))
    (check-media-type "text/plain" "text/plain")
    (check-media-type "text/plain" " text	/	 plain  ")
    (check-media-type "text/plain;charset=UTF-8"
                      "text/plain ; charset	= UTF-8		")
    (check-media-type "x-tungsten/test;a=1;b=2;c=3"
                      "x-tungsten/test;a = 1 ;b=	2 ; c =3 ")))

(deftest normalize-media-type ()
  (check-string= "text/plain;encoding=UTF-8"
                 (mime:serialize-media-type
                  (mime:normalize-media-type
                   (mime:parse-media-type "tExT/PlAiN; EnCoding=UTF-8")))))
