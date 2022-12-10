(in-package :text)

(deftype octet ()
  '(unsigned-byte 8))

(deftype octet-vector (&optional length)
  `(array octet (,length)))

(deftype index ()
  'fixnum)

(deftype vector-length ()
  'fixnum)
