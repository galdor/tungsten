(in-package :system)

;;;
;;; Mutexes
;;;

(deftype %mutex ()
  'sb-thread:mutex)

(defun %make-mutex (&key name)
  (sb-thread:make-mutex :name name))

(defun %acquire-mutex (mutex &key (wait t))
  (sb-thread:grab-mutex mutex :waitp wait))

(defun %release-mutex (mutex)
  (sb-thread:release-mutex mutex))

(defmacro %with-mutex ((mutex) &body body)
  `(sb-thread:with-mutex (,mutex)
     ,@body))
