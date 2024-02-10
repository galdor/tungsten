(in-package :system)

(deftype io-event ()
  '(member :read :write :hangup))

(defclass io-base ()
  ((mutex
    :type mutex
    :initform (make-mutex :name "io-base"))
   (fd-watchers
    :type hash-table
    :initform (make-hash-table))
   (timers
    :type core:binary-heap
    :initform (core:make-binary-heap #'< :element-type 'timer
                                         :key 'timer-next-tick))))

(defclass io-watcher ()
  ((fd
    :type (integer 0)
    :initarg :fd
    :reader io-watcher-fd)
   (registered
    :type boolean
    :initform nil
    :accessor io-watcher-registered)
   (events
    :type list
    :initarg :events
    :accessor io-watcher-events)
   (handler
    :type (or symbol function)
    :initarg :handler
    :accessor io-watcher-handler)))

(defgeneric close-io-base (base))

(defgeneric add-io-watcher (base watcher))
(defgeneric update-io-watcher (base watcher events))
(defgeneric remove-io-watcher (base watcher))

(defgeneric read-and-dispatch-io-events (base &key timeout))

(defgeneric set-timer (base next-tick))
(defgeneric cancel-timer (base))

(defun make-io-base ()
  (make-instance #+bsd 'bsd-io-base
                 #+linux 'linux-io-base
                 #-(or bsd linux)
                 (core:unsupported-feature "io multiplexing")))

(defmacro with-io-base ((base) &body body)
  `(let ((,base (make-io-base)))
     (unwind-protect
          (progn
            ,@body)
       (close-io-base ,base))))

(defun run-io-base (base &key timeout)
  (declare (type io-base base)
           (type (or float null) timeout))
  (read-and-dispatch-io-events base :timeout timeout))

(defun watch-fd (base fd events handler)
  (declare (type io-base base)
           (type (integer 0) fd)
           (type list events)
           (type (or symbol function) handler))
  (with-slots (fd-watchers mutex) base
    (with-mutex (mutex)
      (let ((watcher (or (gethash fd fd-watchers)
                         (make-instance 'io-watcher :fd fd :events events
                                        :handler handler))))
        (cond
          ((io-watcher-registered watcher)
           (update-io-watcher base watcher events)
           (setf (io-watcher-events watcher) events)
           (when handler
             (setf (io-watcher-handler watcher) handler)))
          (t
           (add-io-watcher base watcher)
           (setf (io-watcher-registered watcher) t)
           (setf (gethash fd fd-watchers) watcher))))))
  handler)

(defun unwatch-fd (base fd)
  (declare (type io-base base)
           (type (integer 0) fd))
  (with-slots (fd-watchers mutex) base
    (with-mutex (mutex)
      (let ((watcher (gethash fd fd-watchers)))
        (when watcher
          (remove-io-watcher base watcher)
          (remhash fd fd-watchers)
          (setf (io-watcher-registered watcher) nil)
          t)))))

(defun start-timer (base duration handler &key period)
  (declare (type io-base base)
           (type float duration)
           (type (or symbol function) handler)
           (type (or float null) period))
  (with-slots (mutex timers) base
    (with-mutex (mutex)
      (let ((timer (make-timer duration handler :period period))
            (next-timer (core:binary-heap-peek timers)))
        (when (or (null next-timer)
                  (< (timer-next-tick timer) (timer-next-tick next-timer)))
          (set-timer base (timer-next-tick timer)))
        (core:binary-heap-add timers timer)
        timer))))

(defun stop-timer (base timer)
  (declare (type io-base base)
           (type timer timer))
  (with-slots (mutex timers) base
    (with-mutex (mutex)
      (let ((was-next (eq timer (core:binary-heap-peek timers))))
        (core:binary-heap-remove timers timer)
        (when was-next
          (let ((next-timer (core:binary-heap-peek timers)))
            (if next-timer
                (set-timer base (timer-next-tick next-timer))
                (cancel-timer base))))))))

(defun process-fd-event (base fd events)
  (declare (type io-base base)
           (type (integer 0) fd)
           (type list events))
  (with-slots (mutex fd-watchers) base
    (let ((handler (with-mutex (mutex)
                     (let ((watcher (gethash fd fd-watchers)))
                       (when (and watcher (io-watcher-registered watcher))
                         (io-watcher-handler watcher))))))
      (call-handler handler events))))

(defun process-timer-event (base)
  (declare (type io-base base))
  (with-slots (mutex timers) base
    (let (timer)
      (let ((handler (with-mutex (mutex)
                       (when (setf timer (core:binary-heap-pop timers))
                         (timer-handler timer)))))
        (unwind-protect
             (call-handler handler)
          (with-mutex (mutex)
            (when timer
              (let ((period (timer-period timer)))
                (when period
                  (update-periodic-timer-next-tick timer)
                  (core:binary-heap-add timers timer))))
            (let ((next-timer (core:binary-heap-peek timers)))
              (if next-timer
                  (set-timer base (timer-next-tick next-timer))
                  (cancel-timer base)))))))))

(defun call-handler (function &rest arguments)
  (declare (type (or symbol function) function)
           (type list arguments))
  (restart-case
      (apply function arguments)
    (continue ()
      :report "Ignore the error and continue."
      nil)))
