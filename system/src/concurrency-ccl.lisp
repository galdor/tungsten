(in-package :system)

;;;
;;; Mutexes
;;;

(deftype %mutex ()
  'ccl:lock)

(defun %make-mutex (&key name)
  (ccl:make-lock name))

(defun %acquire-mutex (mutex &key (wait t))
  (if wait
      (ccl:grab-lock mutex)
      (ccl:try-lock mutex)))

(defun %release-mutex (mutex)
  (ccl:release-lock mutex))

(defmacro %with-mutex ((mutex) &body body)
  `(ccl:with-lock-grabbed (,mutex)
     ,@body))
