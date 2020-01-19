;;; Copyright (c) 2020 Nicolas Martyanoff <khaelin@gmail.com>
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

(defsystem "tungsten"
    :description "Common Lisp tools and systems."
    :author "Nicolas Martyanoff <khaelin@gmail.com>"
    :licence "ISC"
    :depends-on ("tungsten.check"
                 "tungsten.json")
    :in-order-to ((test-op (test-op "tungsten/test"))))

(defsystem "tungsten/test"
  :description "Tests for the tungsten system."
  :author "Nicolas Martyanoff <khaelin@gmail.com>"
  :licence "ISC"
  :depends-on ("tungsten.check/test"
               "tungsten.json/test")
  :perform (test-op (op system)
                    (symbol-call :tungsten.check :run)))
