(in-package :ffi-extractor)

(defun load-manifest (path)
  "Load and return a manifest from a file at PATH."
  (with-open-file (file path)
    (let ((*read-eval* nil)
          (*package* (find-package :ffi-extractor)))
      (do ((forms nil))
          ((eq (car forms) 'eof)
           (nreverse (cdr forms)))
        (push (read file nil 'eof) forms)))))

(defun manifest-header-files (manifest)
  "Return the list of header files listed in the manifest."
  (let ((files nil))
    (dolist (form manifest (nreverse files))
      (when (eq (car form) 'include)
        (push (second form) files)))))
