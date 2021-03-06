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

(defsystem "tungsten.check"
  :description "A simple test framework."
  :author "Nicolas Martyanoff <khaelin@gmail.com>"
  :licence "ISC"
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "test")
               (:file "tests")
               (:file "reporter")
               (:file "text-reporter")
               (:file "execution")
               (:file "assertions")
               (:file "systems"))
  :in-order-to ((test-op (test-op "tungsten.check/test"))))

(defsystem "tungsten.check/test"
  :description "Tests for the tungsten.check system."
  :author "Nicolas Martyanoff <khaelin@gmail.com>"
  :licence "ISC"
  :depends-on ("tungsten.check")
  :serial t
  :pathname "t"
  :components ((:file "package")
               (:file "assertions"))
  :perform (test-op (op system)
                    (symbol-call :tungsten.check :run
                                 :package :tungsten.check-test)))
