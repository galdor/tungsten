(in-package :unicode)

(defconstant highest-code-point #x10ffff)

(deftype code-point ()
  `(integer 0 ,highest-code-point))
