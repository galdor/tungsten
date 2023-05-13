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
;;; Semaphores
;;;

(deftype semaphore ()
  '%semaphore)

(defun make-semaphore (&key name (count 0))
  (declare (type (or string null) name)
           (type (integer 0) count))
  (%make-semaphore :name name :count count))

(defun signal-semaphore (semaphore &optional (n 1))
  (declare (type semaphore semaphore)
           (type (integer 1) n))
  (%signal-semaphore semaphore n))

(defun wait-semaphore (semaphore &key timeout)
  (declare (type semaphore semaphore)
           (type (or real null) timeout))
  (%wait-semaphore semaphore :timeout timeout))

;;;
;;; Condition variables
;;;

(deftype condition-variable ()
  '%condition-variable)

(defun make-condition-variable (&key name)
  (declare (type (or string null) name))
  (%make-condition-variable :name name))

(defun wait-condition-variable (variable mutex &key timeout)
  (declare (type condition-variable variable)
           (type mutex mutex)
           (type (or real null) timeout))
  (%wait-condition-variable variable mutex :timeout timeout))

(defun signal-condition-variable (variable)
  (declare (type condition-variable variable))
  (%signal-condition-variable variable))

(defun broadcast-condition-variable (variable)
  (declare (type condition-variable variable))
  (%broadcast-condition-variable variable))

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
           (type (or symbol function) function))
  (%make-thread name function))

(defun join-thread (thread)
  (declare (type thread thread))
  (%join-thread thread))
