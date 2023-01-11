(in-package :system)

;;;
;;; Mutexes
;;;

(deftype mutex ()
  '%mutex)

(defun make-mutex (&key name)
  (declare (type (or string null) name))
  (%make-mutex :name name))

(defun acquire-mutex (mutex)
  (declare (type mutex mutex))
  (%acquire-mutex mutex))

(defun maybe-acquire-mutex (mutex)
  (declare (type mutex mutex))
  (%maybe-acquire-mutex mutex))

(defun release-mutex (mutex)
  (declare (type mutex mutex))
  (%release-mutex mutex))

(defmacro with-mutex ((mutex) &body body)
  `(%with-mutex (,mutex)
     ,@body))

;;;
;;; Threads
;;;

(deftype thread ()
  '%thread)

(defun current-thread ()
  (%current-thread))

(defun list-threads ()
  (%list-threads))

(defun make-thread (name function)
  (declare (type string name)
           (type function function))
  (%make-thread name function))

(defun join-thread (thread)
  (declare (type thread thread))
  (%join-thread thread))
