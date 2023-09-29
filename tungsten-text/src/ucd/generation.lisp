(in-package :ucd)

(defun generate-cl-source (package)
  (declare (type (or keyword string) package))
  (with-standard-io-syntax
    (let ((*package* (find-package package)))
      (print `(in-package ,package))))
  nil)
