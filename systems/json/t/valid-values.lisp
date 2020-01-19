;;; Copyright (c) 2019,2020 Nicolas Martyanoff <khaelin@gmail.com>
;;;
;;; Permission to use, copy, modify, and distribute this software for any
;;; purpose with or without fee is hereby granted, provided that the above
;;; copyright notice and this permission notice appear in all copies.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :tungsten.json-test)

(defmacro check-valid-json (expected-value string)
  `(check-equalp ,expected-value (json:parse ,string)))

;;; Null
(deftest null ()
  (check-valid-json :null "null"))

;;; Booleans
(deftest booleans ()
  (check-valid-json :true "true")
  (check-valid-json :false "false"))

;;; Numbers
(deftest integers ()
  (check-valid-json 0 "0")
  (check-valid-json -1 "-1")
  (check-valid-json 42 "42")
  (check-valid-json 18446744073709551616 "18446744073709551616")
  (check-valid-json -18446744073709551616 "-18446744073709551616"))

(deftest integers-with-exponent ()
  (check-valid-json 0.0d0 "0e0")
  (check-valid-json 0.0d0 "0e1")
  (check-valid-json 1000.d0 "1e3")
  (check-valid-json 1000.d0 "1e+3")
  (check-valid-json -100.d0 "-1e2")
  (check-valid-json -0.01d0 "-1e-2"))

(deftest floats ()
  (check-valid-json 0.0d0 "0.0")
  (check-valid-json 0.1d0 "0.1")
  (check-valid-json -3.25d0 "-3.25")
  (check-valid-json 123.456d0 "123.456")
  (check-valid-json -123.0d0 "-123.0")
  (check-valid-json -0.00000000000000001d0 "-0.00000000000000001"))

(deftest floats-with-exponent ()
  (check-valid-json 0.0d0 "0.0e+0")
  (check-valid-json 125.0d0 "1.25e2")
  (check-valid-json -3000.0d0 "-0.3e4")
  (check-valid-json -1.23456d0 "-123.456e-2"))

;;; Strings
(deftest strings ()
  (check-valid-json "" "\"\"")
  (check-valid-json "hello" "\"hello\"")
  (check-valid-json " abc def " "\" abc def \"")
  (check-valid-json "élément" "\"élément\""))

(deftest strings-with-escaped-characters ()
  (check-valid-json "\"foo\"end" "\"\\\"foo\\\"end\"")
  (check-valid-json "/\\" "\"\\/\\\\\"")
  (check-valid-json (concatenate 'string (list #\Backspace #\Page #\Newline
                                            #\Return #\Tab))
                 "\"\\b\\f\\n\\r\\t\""))

(deftest strings-with-unicode-sequences ()
  (check-valid-json "\\" "\"\\u005c\"")
  (check-valid-json "élément" "\"\\u00E9l\\U00e9ment\""))

(deftest strings-with-unicode-surrogate-pairs ()
  (check-valid-json "foo 𝄞 bar" "\"foo \\uD834\\uDD1E bar\""))

;;; Arrays
(deftest arrays ()
  (check-valid-json #() "[]")
  (check-valid-json #() "[ 	 ]")
  (check-valid-json #(1 2 3) "[	1 ,2, 3]")
  (check-valid-json #("foo" :null :true) "[\"foo\" , null,true]"))

(deftest nested-arrays ()
  (check-valid-json #(#()) "[[]]")
  (check-valid-json #(#(#()) #(#())) "[[ [  ]], [[] ]]")
  (check-valid-json #(1 #(#(2 3)) #(4)) "[1, [[2, 3]], [4]]"))

;;; Objects
(deftest objects ()
  (check-valid-json '() "{}")
  (check-valid-json '(("" . "")) "{\"\": \"\"}")
  (check-valid-json '(("a" . 1)) "{\"a\":1}")
  (check-valid-json '(("abc" . 2) ("def" . "ghi"))
                 "{ \"abc\"	:2 , \"def\":  \"ghi\"}"))

(deftest nested-objects ()
  (check-valid-json '(("a") ("b" ("c" . 1)))
                 "{\"a\": {}, \"b\": {\"c\": 1}}"))

(deftest duplicate-object-keys ()
  (check-valid-json '(("a" . 1) ("b" . 2) ("a" . 3))
                 "{\"a\": 1, \"b\": 2, \"a\": 3}")
  (let ((json:*duplicate-object-keys-handling* :keep))
    (check-valid-json '(("a" . 1) ("b" . 2) ("a" . 3))
                   "{\"a\": 1, \"b\": 2, \"a\": 3}"))
  (let ((json:*duplicate-object-keys-handling* :first))
    (check-valid-json '(("a" . 1) ("b" . 2))
                   "{\"a\": 1, \"b\": 2, \"a\": 3}"))
  (let ((json:*duplicate-object-keys-handling* :last))
    (check-valid-json '(("b" . 2) ("a" . 3))
                   "{\"a\": 1, \"b\": 2, \"a\": 3}")))

;;; Trailing spaces
(deftest trailing-spaces ()
  (check-valid-json 42 "42 ")
  (check-valid-json :false "false		"))
