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
;;; Condition variables
;;;

;; CCL does not provide condition variables. Normally I would not bother with
;; supporting an implementation without built-in condition variables, but
;; there are not a lot of open source implementations left, and testing with
;; CCL is convenient.
;;
;; Condition variables are implemented with semaphores as described in
;; "Implementing Condition Variables with Semaphores" by Andrew D. Birrell. We
;; use the simple but inefficient implementation based on two semaphores because
;; the queue-based implementation requires wrapping threads to assign a
;; semaphore to each one of them, something we would like to avoid (especially
;; since it would be only for CCL).

(defstruct (%condition-variable
            (:constructor %make-condition-variable)
            (:copier nil))
  (name nil :type (or string null))
  (lock (ccl:make-lock) :type ccl:lock :read-only t)
  (semaphore (ccl:make-semaphore) :type ccl:semaphore :read-only t)
  (handshake (ccl:make-semaphore) :type ccl:semaphore :read-only t)
  (nb-waiters 0 :type (integer 0)))

(defun %wait-condition-variable (variable mutex &key timeout)
  ;; The way timeouts are implemented is wrong: the timeout duration should
  ;; cover both the time to acquire the lock and the time to wait on the
  ;; semaphore. We cannot do it the right way because CCL does not support
  ;; timeouts for lock acquisition. Not much we can do about it.
  (with-slots (lock semaphore handshake nb-waiters) variable
    (ccl:with-lock-grabbed (lock)
      (incf nb-waiters))
    (ccl:release-lock mutex)
    (unwind-protect
         (prog1
             (if timeout
                 (ccl:timed-wait-on-semaphore semaphore timeout)
                 (ccl:wait-on-semaphore semaphore))
           (ccl:signal-semaphore handshake))
      (ccl:grab-lock mutex))))

(defun %signal-condition-variable (variable)
  (with-slots (lock semaphore handshake nb-waiters) variable
    (ccl:with-lock-grabbed (lock)
      (when (> nb-waiters 0)
        (ccl:signal-semaphore semaphore)
        (ccl:wait-on-semaphore handshake)))))

(defun %broadcast-condition-variable (variable)
  (with-slots (lock semaphore handshake nb-waiters) variable
    (ccl:with-lock-grabbed (lock)
      (dotimes (i nb-waiters)
        (ccl:signal-semaphore semaphore))
      (dotimes (i nb-waiters)
        (ccl:signal-semaphore handshake))
      (setf nb-waiters 0))))

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
