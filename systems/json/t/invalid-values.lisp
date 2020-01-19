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

(defmacro check-invalid-json (string)
  `(check-signals json:parsing-error (json:parse ,string)))

(deftest no-value ()
  (check-invalid-json "")
  (check-invalid-json "	  ")
  (check-invalid-json "foo")
  (check-invalid-json "nul"))

(deftest invalid-trailing-characters ()
  (check-invalid-json "42foo")
  (check-invalid-json " \"foo\"\"")
  (check-invalid-json "true	."))

;;; Numbers
;;; TODO

;;; Strings
;;; TODO

;;; Arrays
;;; TODO

;;; Objects
(deftest invalid-object-keys ()
  (check-invalid-json "{1: 2}")
  (check-invalid-json "{null: true}"))

(deftest invalid-duplicate-object-keys ()
  (let ((json:*duplicate-object-keys-handling* :error))
    (check-invalid-json "{\"a\": 1, \"b\": 2, \"a\": 3}")))
