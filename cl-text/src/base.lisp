(in-package :text)

(deftype octet ()
  '(unsigned-byte 8))

(deftype octet-vector (&optional length)
  `(simple-array octet (,length)))

(deftype index ()
  'fixnum)

(deftype vector-length ()
  'fixnum)
