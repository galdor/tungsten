(in-package :systems)

(defun list-systems ()
  "Return a list of all available ASDF systems sorted by name."
  (let ((systems nil))
    (maphash (lambda (name system-definition-pathname)
               (declare (ignore system-definition-pathname))
               (let ((system (handler-bind ((warning #'muffle-warning))
                               (asdf:find-system name))))
                 (unless (typep system 'asdf:require-system)
                   (push system systems))))
             asdf/source-registry:*source-registry*)
    (sort systems #'string<= :key #'asdf:component-name)))
