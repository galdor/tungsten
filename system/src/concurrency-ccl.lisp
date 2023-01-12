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
;;; Semaphores
;;;

(deftype %semaphore ()
  'ccl:semaphore)

(defun %make-semaphore (&key name (count 0))
  (declare (ignore name))
  (ccl:make-semaphore :count count))

(defun %signal-semaphore (semaphore n)
  (dotimes (i n)
    (ccl:signal-semaphore semaphore)))

(defun %wait-semaphore (semaphore &key timeout)
  (if timeout
      (ccl:timed-wait-on-semaphore semaphore timeout)
      (ccl:wait-on-semaphore semaphore)))

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
