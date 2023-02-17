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

(deftest match-media-range ()
  (macrolet ((check-match (expected range type)
               `(check-eq ,expected
                          (mime:match-media-range (mime:media-range ,range)
                                                  (mime:media-type ,type)))))
    (check-match :exact "text/plain" "text/plain")
    (check-match :exact "text/plain" "text/plain;charset=UTF-8")
    (check-match :exact "text/plain;q=1.0" "text/plain")
    (check-match :partial "text/*" "text/plain")
    (check-match :wildcard "*/*" "text/plain")
    (check-match nil "text/plain" "text/html")
    (check-match nil "text/*" "application/json")))
