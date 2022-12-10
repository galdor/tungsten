(in-package :ffi-extractor)

(defun load-manifest (path &key (package (find-package :ffi-extractor)))
  "Load and return a manifest from a file at PATH."
  (with-open-file (file path)
    (let ((*read-eval* nil)
          (*package* package)
          (ffi-extractor-package (find-package :ffi-extractor)))
      (do ((forms nil))
          ((eq (car forms) 'eof)
           (nreverse (cdr forms)))
        (let ((form (read file nil 'eof)))
          (unless (eq form 'eof)
            (setf (car form)
                  (intern (symbol-name (car form)) ffi-extractor-package)))
          (push form forms))))))

(defun manifest-header-files (manifest)
  "Return the list of header files listed in the manifest."
  (let ((files nil))
    (dolist (form manifest (nreverse files))
      (when (eq (car form) 'include)
        (push (second form) files)))))
