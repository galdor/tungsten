(in-package :text-test)

(deftest encode-string/utf16be ()
  (check-encode-string
   :utf-16be
   (#() "")
   (#(0 102 0 111 0 111) "foo")
   (#(0 98 0 97 0 114) "foobar" :start 3)
   (#(0 102 0 111 0 111) "foobar" :end 3)
   (#() "foobar" :start 3 :end 3)
   (#(0 98) "foobar" :start 3 :end 4)
   (#(0 233 0 116 0 233) "été")
   (#(0 233) "été" :start 2)
   (#(3 196 3 177 31 80 3 196 31 112) "ταὐτὰ")
   (#(216 61 222 66 216 61 222 67) "🙂🙃")
   (#(216 61 222 66) "🙂🙃" :end 1)))

(deftest decode-string/utf16be ()
  (check-decode-string
   :utf-16be
   ("" #())
   ("foo" #(0 102 0 111 0 111))
   ("bar" #(0 102 0 111 0 111 0 98 0 97 0 114) :start 6)
   ("foo" #(0 102 0 111 0 111 0 98 0 97 0 114) :end 6)
   ("" #(0 102 0 111 0 111 0 98 0 97 0 114) :start 6 :end 6)
   ("b" #(0 102 0 111 0 111 0 98 0 97 0 114) :start 6 :end 8)
   ("été" #(0 233 0 116 0 233))
   ("té" #(0 233 0 116 0 233) :start 2)
   ("ταὐτὰ" #(3 196 3 177 31 80 3 196 31 112))
   ("🙂🙃" #(216 61 222 66 216 61 222 67))
   ("🙂" #(216 61 222 66 216 61 222 67) :end 4)))

(deftest decode-string/utf16be/truncated-characters ()
  (check-decode-string-error
   :utf-16be
   (text:truncated-utf16-character #(0))
   (text:truncated-utf16-character #(0 102 0) :start 2)
   (text:truncated-utf16-character #(0 102 0) :end 1)))

(deftest decode-string/utf16be/truncated-surrogate-pairs ()
  (check-decode-string-error
   :utf-16be
   (text:truncated-utf16-surrogate-pair #(216))
   (text:truncated-utf16-surrogate-pair #(216 61 222))
   (text:truncated-utf16-surrogate-pair #(216 61 222 66) :end 2)))
