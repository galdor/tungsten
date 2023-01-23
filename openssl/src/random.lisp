(in-package :openssl)

(defun random-octets (n)
  (rand-bytes n))
