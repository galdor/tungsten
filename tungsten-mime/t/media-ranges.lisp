(in-package :mime-test)

(deftest serialize-media-range ()
  (check-string=
   "text/plain"
   (mime:serialize-media-range
    (mime:make-media-range "text" "plain")))
  (check-string=
   "*/*"
   (mime:serialize-media-range
    (mime:make-media-range '* '*)))
  (check-string=
   "text/*;charset=UTF-8"
   (mime:serialize-media-range
    (mime:make-media-range "text" '*
                           :parameters '(("charset" . "UTF-8"))))))

(deftest parse-media-range ()
  (macrolet ((check-media-range (expected string)
               `(check-string= ,expected (mime:serialize-media-range
                                          (mime:parse-media-range ,string))))
             (check-invalid-media-range (string)
               `(check-signals mime:invalid-media-range
                               (mime:parse-media-range ,string))))
    (check-media-range "text/*" "text/*")
    (check-media-range "*/*" " *	/	 *  ")
    (check-media-range "text/*;charset=UTF-8"
                       "text/* ; charset	= UTF-8		")
    (check-invalid-media-range "text")
    (check-invalid-media-range "text/")
    (check-invalid-media-range "/plain")
    (check-invalid-media-range "*/plain")))

(deftest normalize-media-range ()
  (check-string= "text/*;encoding=UTF-8"
                 (mime:serialize-media-range
                  (mime:normalize-media-range
                   (mime:parse-media-range "tExT/*; EnCoding=UTF-8")))))
