(in-package :core)

(defun list-to-hash-table (list key-function value-function &key (test #'eql))
  "Build a hash table whose entries are constructed from the elements contained
by LIST. For each element, the key and value are extracted using KEY-FUNCTION
and VALUE-FUNCTION respectively."
  (declare (type list list)
           (type (or symbol function) key-function value-function test))
  (let ((table (make-hash-table :test test)))
    (dolist (element list table)
      (setf (gethash (funcall key-function element) table)
            (funcall value-function element)))))

(defun alist-to-hash-table (alist &key (test #'eql))
  "Convert a list of cons cells to a hash table."
  (declare (type list alist)
           (type (or symbol function) test))
  (list-to-hash-table alist #'car #'cdr :test test))

(defun hash-table-keys (table)
  "Return a list containing all keys in TABLE."
  (declare (type hash-table table))
  (let ((keys nil))
    (maphash (lambda (key value)
               (declare (ignore value))
               (push key keys))
             table)
    keys))

(defun hash-table-values (table)
  "Return a list containing all values in TABLE."
  (declare (type hash-table table))
  (let ((values nil))
    (maphash (lambda (key value)
               (declare (ignore key))
               (push value values))
             table)
    values))

(defun hash-table-entries (table)
  "Return a list containing all entries in TABLE. Each entry is a cons containing the key and the value of the entry."
  (declare (type hash-table table))
  (let ((entries nil))
    (maphash (lambda (key value)
               (push (cons key value) entries))
             table)
    entries))
