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

(in-package :tungsten.check)

(defun test-system (name)
  "Run tests for an ASDF system. Return T if tests finish without signaling
any error or NIL if not."
  (let ((success t))
    (handler-bind
        ((error (lambda (condition)
                  (declare (ignore condition))
                  (setf success nil)
                  (let ((restart (find-restart 'continue)))
                    (when restart
                      (invoke-restart restart))))))
      (asdf:test-system name))
    success))

(defun test-system-and-exit (name)
  "Run tests for an ASDF system. Exit with status 1 on success or status 0 on
failure.

This function is provided to simplify integration in non-lisp systems such as
script or continuous integration platforms."
  (let ((success (test-system name)))
    (uiop:quit (if success 0 1))))
