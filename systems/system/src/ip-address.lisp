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

(deftype ipv4-address ()
  `(vector (unsigned-byte 8) 4))

(deftype ipv6-address ()
  `(vector (unsigned-byte 16) 8))

(deftype ip-address ()
  `(or ipv4-address ipv6-address))

(defun ip-address (value)
  "Create a new IP address. If VALUE is an IP address, return a copy. If VALUE
is a simple vector, create a new IP address based on its content."
  (etypecase value
    (ipv4-address
     (copy-seq value))
    ((simple-vector 4)
     (make-array 4 :element-type '(unsigned-byte 8) :initial-contents value))
    (ipv6-address
     (copy-seq value))
    ((simple-vector 8)
     (make-array 8 :element-type '(unsigned-byte 16) :initial-contents value))))

(defun ip-address-to-string (address)
  "Return the textual representation of ADDRESS as a string."
  (etypecase address
    (ipv4-address
     (format nil "~D.~D.~D.~D"
             (aref address 0)
             (aref address 1)
             (aref address 2)
             (aref address 3)))
    (ipv6-address
     (multiple-value-bind (start end)
         (do ((i 0)
              (start nil)
              (max-length 0))
             ((>= i (length address))
              (if (zerop max-length)
                  (values nil nil)
                  (values start (+ start max-length))))
           (cond
             ((zerop (aref address i))
              (let* ((end (or (position-if-not #'zerop address :start (1+ i))
                              (length address)))
                     (length (- end i)))
                (when (> length max-length)
                  (setf start i)
                  (setf max-length length))
                (setf i end)))
             (t
              (incf i))))
       (with-output-to-string (stream)
         (cond
           ((and (eql start 0)
                 (eql end (length address)))
            (write-string "::" stream))
           (t
            (do ((i 0))
                ((>= i (length address)))
              (unless (zerop i)
                (write-char #\: stream))
              (cond
                ((eql i start)
                 (when (or (eql start 0)
                           (eql end (length address)))
                   (write-char #\: stream))
                 (setf i end))
                (t
                 (format stream "~(~X~)" (aref address i))
                 (incf i)))))))))))
