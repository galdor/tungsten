(in-package :system)

;;;
;;; Mutexes
;;;

(deftype %mutex ()
  'sb-thread:mutex)

(defun %make-mutex (&key name)
  (sb-thread:make-mutex :name name))

(defun %acquire-mutex (mutex)
  (sb-thread:grab-mutex mutex :waitp t))

(defun %maybe-acquire-mutex (mutex)
  (sb-thread:grab-mutex mutex :waitp nil))

(defun %release-mutex (mutex)
  (sb-thread:release-mutex mutex))

(defmacro %with-mutex ((mutex) &body body)
  `(sb-thread:with-mutex (,mutex)
     ,@body))

;;;
;;; Threads
;;;

(deftype %thread ()
  'sb-thread:thread)

(defun %current-thread ()
  sb-thread:*current-thread*)

(defun %list-threads ()
  (sb-thread:list-all-threads))

(defun %make-thread (name function)
  (sb-thread:make-thread function :name name))

(defun %join-thread (thread)
  (sb-thread:join-thread thread))
