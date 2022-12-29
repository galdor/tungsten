(in-package :text-test)

(deftest encode-string/ascii ()
  (check-encode-string
   :ascii
   (#() "")
   (#(102 111 111) "foo")
   (#(98 97 114) "foobar" :start 3)
   (#(102 111 111) "foobar" :end 3)
   (#() "foobar" :start 3 :end 3)
   (#(98) "foobar" :start 3 :end 4)))

(deftest encode-string/ascii/unencodable-characters ()
  (check-encode-string-error
   :ascii
   (text:unencodable-character "é")
   (text:unencodable-character "abc🙂")))

(deftest decode-string/ascii ()
  (check-decode-string
   :ascii
   ("" #())
   ("foo" #(102 111 111))
   ("bar" #(102 111 111 98 97 114) :start 3)
   ("foo" #(102 111 111 98 97 114) :end 3)
   ("" #(102 111 111 98 97 114) :start 3 :end 3)
   ("b" #(102 111 111 98 97 114) :start 3 :end 4)))

(deftest decode-string/ascii/invalid-octets ()
  (check-decode-string-error
   :ascii
   (text:invalid-ascii-octet #(128))
   (text:invalid-ascii-octet #(97 98 99 255))))
