(in-package :core)

(defun shuffle (sequence)
  "Return a randomly reordered copy of SEQUENCE."
  (nshuffle (copy-seq sequence)))

(defun nshuffle (sequence)
  "Randomly reorder SEQUENCE in a destructive way and return it."
  (etypecase sequence
    (array (nshuffle-array sequence))
    (list (nshuffle-list sequence))))

(defun nshuffle-list (list)
  (declare (type list list))
  (coerce (nshuffle-array (coerce list 'vector)) 'list))

(defun nshuffle-array (array)
  (declare (type array array))
  (do ((i (1- (length array)) (1- i)))
      ((< i 1)
       array)
    (rotatef (aref array i) (aref array (random i)))))
