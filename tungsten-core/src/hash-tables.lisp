(in-package :core)

(defun alist-to-hash-table (alist &key (test #'eql))
  "Convert a list of cons cells to a hash table."
  (let ((table (make-hash-table :test test)))
    (dolist (entry alist table)
      (setf (gethash (car entry) table) (cdr entry)))))
