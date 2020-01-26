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

(in-package :tungsten.system-test)

(deftest ipv4-addresses ()
  (flet ((check-ipv4-address (value string)
           (let ((label (prin1-to-string string))
                 (address (system:ip-address value)))
             (check-true (typep address 'system:ipv4-address) :label label)
             (check-string= string (system:print-ip-address-to-string address)
                            :label label))))
    (check-ipv4-address #(10 0 42 234)
                        "10.0.42.234")
    (check-ipv4-address #(255 255 255 0)
                        "255.255.255.0")
    (check-ipv4-address #(0 0 0 0)
                        "0.0.0.0")))

(deftest ipv6-addresses ()
  (flet ((check-ipv6-address (value string)
           (let ((label (prin1-to-string string))
                 (address (system:ip-address value)))
             (check-true (typep address 'system:ipv6-address) :label label)
             (check-string= string (system:print-ip-address-to-string address)
                            :label label))))
    (check-ipv6-address #(0 0 0 0 0 0 0 0)
                        "::")
    (check-ipv6-address #(0 0 0 0 0 0 0 1)
                        "::1")
    (check-ipv6-address #(#xff01 0 0 0 0 0 0 #x43)
                        "ff01::43")
    (check-ipv6-address #(#x1080 0 0 0 #x8 #x800 #x200c #x417a)
                        "1080::8:800:200c:417a")))
