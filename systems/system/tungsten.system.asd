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

(defsystem "tungsten.system"
  :description "Tools to interact with the operating system."
  :author "Nicolas Martyanoff <khaelin@gmail.com>"
  :licence "ISC"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi")
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:cffi-grovel-file "grovel"))
  :in-order-to ((test-op (test-op "tungsten.system/test"))))

(defsystem "tungsten.system/test"
  :description "Tests for the tungsten.system system."
  :author "Nicolas Martyanoff <khaelin@gmail.com>"
  :licence "ISC"
  :depends-on ("tungsten.check")
  :pathname "t"
  :serial t
  :components ((:file "package"))
  :perform (test-op (op system)
                    (symbol-call :tungsten.check :run
                                 :package :tungsten.system-test)))
