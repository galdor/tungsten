(in-package :system)

;;;
;;; Mutexes
;;;

(deftype %mutex ()
  'ccl:lock)

(defun %make-mutex (&key name)
  (ccl:make-lock name))

(defun %acquire-mutex (mutex)
  (ccl:grab-lock mutex))

(defun %maybe-acquire-mutex (mutex)
  (ccl:try-lock mutex))

(defun %release-mutex (mutex)
  (ccl:release-lock mutex))

(defmacro %with-mutex ((mutex) &body body)
  `(ccl:with-lock-grabbed (,mutex)
     ,@body))

;;;
;;; Threads
;;;

(deftype %thread ()
  'ccl:process)

(defun %current-thread ()
  ccl:*current-process*)

(defun %list-threads ()
  (ccl:all-processes))

(defun %make-thread (name function)
  (ccl:process-run-function name function))

(defun %join-thread (thread)
  (ccl:join-process thread))
