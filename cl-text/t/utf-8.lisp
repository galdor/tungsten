(in-package :text-test)

(deftest encode-string/utf8 ()
  (check-encode-string
   :utf-8
   (#() "")
   (#(102 111 111) "foo")
   (#(98 97 114) "foobar" :start 3)
   (#(102 111 111) "foobar" :end 3)
   (#() "foobar" :start 3 :end 3)
   (#(98) "foobar" :start 3 :end 4)
   (#(195 169 116 195 169) "été")
   (#(195 169) "été" :start 2)
   (#(207 132 206 177 225 189 144 207 132 225 189 176) "ταὐτὰ")
   (#(240 159 153 130 240 159 153 131) "🙂🙃")
   (#(240 159 153 130) "🙂🙃" :end 1)))

(deftest decode-string/utf8 ()
  (check-decode-string
   :utf-8
   ;; Base
   ("" #())
   ("foo" #(102 111 111))
   ("bar" #(102 111 111 98 97 114) :start 3)
   ("foo" #(102 111 111 98 97 114) :end 3)
   ("" #(102 111 111 98 97 114) :start 3 :end 3)
   ("b" #(102 111 111 98 97 114) :start 3 :end 4)
   ("été" #(195 169 116 195 169))
   ("té" #(195 169 116 195 169) :start 2)
   ("ταὐτὰ" #(207 132 206 177 225 189 144 207 132 225 189 176))
   ("🙂🙃" #(240 159 153 130 240 159 153 131))
   ("🙂" #(240 159 153 130 240 159 153 131) :end 4)))

(deftest decode-string/utf8/boundary-conditions ()
  ;; Adapted from "UTF-8 decoder capability and stress test"
  ;;
  ;; Markus Kuhn <http://www.cl.cam.ac.uk/~mgk25/> - 2015-08-28 - CC BY 4.0
  (check-decode-string
   :utf-8
   ;; First possible sequences of a certain length
   ((codepoint-string #x0) #(0))
   ((codepoint-string #x80) #(194 128))
   ((codepoint-string #x800) #(224 160 128))
   ((codepoint-string #x10000) #(240 144 128 128))
   ;; Last possible sequence of a certain length (excluding (U+FFFE and
   ;; U+FFFF) which trigger INVALID-UTF8-SEQUENCE errors).
   ((codepoint-string #x7f) #(127))
   ((codepoint-string #x7ff) #(223 191))
   ((codepoint-string #xfffd) #(239 191 189))
   ;; Other boundary conditions
   ((codepoint-string #xd7ff) #(237 159 191))
   ((codepoint-string #xe000) #(238 128 128))
   ((codepoint-string #xfffd) #(239 191 189))
   ((codepoint-string #x10ffff) #(244 143 191 191))))

(deftest decode-string/utf8/invalid-leading-bytes ()
  (check-decode-string-error
   :utf-8
   (text:invalid-utf8-leading-byte #(128))
   (text:invalid-utf8-leading-byte #(191))
   (text:invalid-utf8-leading-byte #(245))
   (text:invalid-utf8-leading-byte #(255))))

(deftest decode-string/utf8/invalid-continuation-bytes ()
  (check-decode-string-error
   :utf-8
   (text:invalid-utf8-continuation-byte #(194 0))
   (text:invalid-utf8-continuation-byte #(226 130 127))
   (text:invalid-utf8-continuation-byte #(240 159 152 92))))

(deftest decode-string/utf8/truncated-sequences ()
  (check-decode-string-error
   :utf-8
   (text:truncated-utf8-sequence #(194))
   (text:truncated-utf8-sequence #(226 130))
   (text:truncated-utf8-sequence #(240 159 152))))

(deftest decode-string/utf8/overlong-sequences ()
  ;; Adapted from "UTF-8 decoder capability and stress test"
  ;;
  ;; Markus Kuhn <http://www.cl.cam.ac.uk/~mgk25/> - 2015-08-28 - CC BY 4.0
  (check-decode-string-error
   :utf-8
   (text:overlong-utf8-sequence #(192 175))
   (text:overlong-utf8-sequence #(224 128 175))
   (text:overlong-utf8-sequence #(240 128 128 175))))

(deftest decode-string/utf8/invalid-sequences ()
  ;; Adapted from "UTF-8 decoder capability and stress test"
  ;;
  ;; Markus Kuhn <http://www.cl.cam.ac.uk/~mgk25/> - 2015-08-28 - CC BY 4.0
  (check-decode-string-error
   :utf-8
   ;; UTF-16 surrogates
   (text:invalid-utf8-sequence #(237 160 128)) ; U+D800
   (text:invalid-utf8-sequence #(237 174 128)) ; U+DB80
   (text:invalid-utf8-sequence #(237 191 191)) ; U+DFFF
   ;; Dangerous codepoints
   (text:invalid-utf8-sequence #(239 191 190)) ; U+FFFE
   (text:invalid-utf8-sequence #(239 191 191)) ; U+FFFF
   ;; Other non-characters
   (text:invalid-utf8-sequence #(239 183 144))   ; U+FDD0
   (text:invalid-utf8-sequence #(239 183 175)))) ; U+FDEF
