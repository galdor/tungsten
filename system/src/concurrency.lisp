(in-package :system)

;;;
;;; Mutexes
;;;

(deftype mutex ()
  '%mutex)

(defun make-mutex (&key name)
  (declare (type (or string null) name))
  (%make-mutex :name name))

(defun acquire-mutex (mutex &key (wait t))
  (%acquire-mutex mutex :wait wait))

(defun release-mutex (mutex)
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
  (%make-thread name function))

(defun join-thread (thread)
  (%join-thread thread))
