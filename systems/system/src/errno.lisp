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

(in-package :tungsten.system)

(defun %errno ()
  "Return the current value of errno. The errno value is defined as a lvalue,
and cannot always be accessed using a foreign variable. Common Lisp
implementations usually have a way to safely access it. Note that some
implementations can return the negative version of error codes."
  #+sbcl
  (sb-alien:get-errno)
  #+ccl
  (abs (ccl::%get-errno)) ; CCL:GET-ERRNO available but not released yet
  #-(or sbcl ccl)
  (error "unimplemented function"))

;; POSIX defines three functions that return a description of an errno value:
;; strerror(), strerror_r(), and strerror_l(). Infortunately strerror() is not
;; thread safe, and strerror_r() has two versions (GNU and POSIX) that cannot
;; be differenciated at runtime
;; (http://austingroupbugs.net/view.php?id=655). Therefore we only define and
;; use strerror_l().
(cffi:defcfun (%strerror-l "strerror_l") :string
  (number :int)
  (locale locale-t))

(defun errno-value (errno)
  "Return the integer associated with an errno value."
  (etypecase errno
    (integer
     errno)
    (keyword
     (cffi:foreign-enum-value 'errno errno))))

(defun errno-description (errno)
  "Return a string describing an errno value."
  (let ((locale (let ((ret (%uselocale 0)))
                  (when (zerop ret)
                    (error "cannot obtain current locale: ~A" (%errno)))
                  ret)))
    (let ((ret (%strerror-l (errno-value errno) locale)))
      (when (null ret)
        (lambda (value)
          (format nil "cannot obtain error string: ~A" (%errno))))
      ret)))
