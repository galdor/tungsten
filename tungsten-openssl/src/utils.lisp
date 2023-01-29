(in-package :openssl)

(defun constant-time-equal (octets1 octets2)
  (declare (type core:octet-vector octets1 octets2))
  (= (crypto-memcmp octets1 octets2) 0))
