(in-package :system)

;;;
;;; Mutexes
;;;

(deftype mutex ()
  '%mutex)

(declaim (inline make-mutex))
(defun make-mutex (&key name)
  (declare (type (or string null) name))
  (%make-mutex :name name))

(declaim (inline acquire-mutex))
(defun acquire-mutex (mutex &key (wait t))
  (%acquire-mutex mutex :wait wait))

(declaim (inline release-mutex))
(defun release-mutex (mutex)
  (%release-mutex mutex))

(defmacro with-mutex ((mutex) &body body)
  `(%with-mutex (,mutex)
     ,@body))
