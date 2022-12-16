(in-package :core)

(deftype octet ()
  '(unsigned-byte 8))

(deftype octet-vector (&optional length)
  `(array octet (,length)))
