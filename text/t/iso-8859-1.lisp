(in-package :text-test)

(deftest encode-string/iso-8859-1 ()
  (check-encode-string
   :iso-8859-1
   (#() "")
   (#(102 111 111) "foo")
   (#(108 105 110 101 10) (format nil "line~C" (code-char 10)))
   (#(233 116 233) "été")))

(deftest encode-string/iso-8859-1/unencodable-characters ()
  (check-encode-string-error
   :iso-8859-1
   (text:unencodable-character "œ")
   (text:unencodable-character "abc🙂")))

(deftest decode-string/iso-8859-1 ()
  (check-decode-string
   :iso-8859-1
   ("" #())
   ("foo" #(102 111 111))
   ((format nil "line~C" (code-char 10)) #(108 105 110 101 10))
   ("été" #(233 116 233))))
