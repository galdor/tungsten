(in-package :core)

(defun foldl (function value sequence)
  (declare (type (or function symbol) function)
           (type sequence sequence))
  (etypecase sequence
    (list (foldl/list function value sequence))
    (vector (foldl/vector function value sequence))))

(defun foldl/list (function value list)
  (declare (type (or function symbol) function)
           (type list list))
  (if list
      (foldl/list function (funcall function value (car list)) (cdr list))
      value))

(defun foldl/vector (function value vector)
  (declare (type (or function symbol) function)
           (type vector vector))
  (do ((i 0 (1+ i))
       (accumulator value))
      ((>= i (length vector))
       accumulator)
    (setf accumulator (funcall function accumulator (aref vector i)))))

(defun shuffle (sequence)
  "Return a randomly reordered copy of SEQUENCE."
  (declare (type sequence sequence))
  (nshuffle (copy-seq sequence)))

(defun nshuffle (sequence)
  "Randomly reorder SEQUENCE in a destructive way and return it."
  (declare (type sequence sequence))
  (etypecase sequence
    (array (nshuffle/array sequence))
    (list (nshuffle/list sequence))))

(defun nshuffle/list (list)
  (declare (type list list))
  (coerce (nshuffle/array (coerce list 'vector)) 'list))

(defun nshuffle/array (array)
  (declare (type array array))
  (do ((i (1- (length array)) (1- i)))
      ((< i 1)
       array)
    (rotatef (aref array i) (aref array (random i)))))
