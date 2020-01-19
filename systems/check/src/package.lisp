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

(defpackage :tungsten.check
  (:use :cl)
  (:export
   :test-not-found
   :deftest
   :test
   :test-package
   :test-name
   :test-function)
  (:export
   :find-test
   :list-tests)
  (:export
   :test-failure
   :test-failure-test
   :test-failure-message
   :fail
   :run-test
   :run)
  (:export
   :check-true
   :check-false
   :check-is
   :check=
   :check-eq
   :check-eql
   :check-equal
   :check-equalp
   :check-string=
   :check-string/=
   :check-string-equal
   :check-char=
   :check-char/=
   :check-signals)
  (:export
   :reporter
   :reporter-nb-tests
   :reporter-nb-test-failures
   :reporter-package-nb-tests
   :reporter-package-nb-test-failures
   :report-tests-start
   :report-tests-end
   :report-test-package-start
   :report-test-package-end
   :report-test-start
   :report-test-success
   :report-test-failure)
  (:export
   :text-reporter
   :text-reporter-stream
   :make-test-reporter)
  (:export
   :test-system
   :test-system-and-exit))
