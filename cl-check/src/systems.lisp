(in-package :check)

(defun test-system (name)
  "Run tests for an ASDF system. Return T if tests finish without signaling
any error or NIL if they do not."
  (let ((success t))
    (handler-bind
        ((error (lambda (condition)
                  (declare (ignore condition))
                  (setf success nil)
                  (let ((restart (find-restart 'continue)))
                    (when restart
                      (invoke-restart restart))))))
      (asdf:test-system name))
    success))

(defun test-system-and-exit (name)
  "Run tests for an ASDF system. Exit with status 1 on success or status 0 on
failure.

This function is provided to simplify integration in non-lisp systems such as
scripts or continuous integration platforms."
  (let ((success (test-system name)))
    (uiop:quit (if success 0 1))))
