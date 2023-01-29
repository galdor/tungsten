(in-package :asdf-utils)

(defun list-systems ()
  "Return a list of all available ASDF systems sorted by name.

ASDF:REGISTERED-SYSTEMS returns a list of loaded systems, so we need to
inspect the source registry to find all available systems. Once done, we can
use ASDF:REGISTERED-SYSTEMS which will also contains secondary systems (e.g.
\"foo/test\")."
  (let ((main-system-names nil)
        (systems nil))
    (maphash (lambda (name system-definition-pathname)
               (declare (ignore system-definition-pathname))
               (let ((system (handler-bind ((warning #'muffle-warning))
                               (asdf:find-system name))))
                 (unless (typep system 'asdf:require-system)
                   (push name main-system-names))))
             asdf/source-registry:*source-registry*)
    (mapc (lambda (name)
            (let ((system (asdf:find-system name)))
              (unless (typep system 'asdf:require-system)
                (push (asdf:find-system name) systems))))
          (asdf:registered-systems))
    (sort systems #'string<= :key #'asdf:component-name)))
