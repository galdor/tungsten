(in-package :core)

(defun alist-to-hash-table (alist &key (test #'eql))
  "Convert a list of cons cells to a hash table."
  (let ((table (make-hash-table :test test)))
    (dolist (entry alist table)
      (setf (gethash (car entry) table) (cdr entry)))))

(defun hash-table-keys (table)
  "Return a list containing all keys in TABLE."
  (let ((keys nil))
    (maphash (lambda (key value)
               (declare (ignore value))
               (push key keys))
             table)
    keys))

(defun hash-table-values (table)
  "Return a list containing all values in TABLE."
  (let ((values nil))
    (maphash (lambda (key value)
               (declare (ignore key))
               (push value values))
             table)
    values))

(defun hash-table-entries (table)
  "Return a list containing all entries in TABLE. Each entry is a cons containing the key and the value of the entry."
  (let ((entries nil))
    (maphash (lambda (key value)
               (push (cons key value) entries))
             table)
    entries))
