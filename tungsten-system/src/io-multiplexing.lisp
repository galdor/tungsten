(in-package :system)

(deftype io-event ()
  '(member :read :write :hangup))

(defclass io-base ()
  ((fd-watchers
    :type hash-table
    :initform (make-hash-table))
   (mutex
    :type mutex
    :initform (make-mutex :name "io-base"))))

(defclass io-watcher ()
  ((fd
    :type (integer 0)
    :initarg :fd
    :reader io-watcher-fd)
   (registeredp
    :type boolean
    :initform nil
    :accessor io-watcher-registeredp)
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

(defun make-io-base ()
  (make-instance #+bsd 'kqueue-io-base
                 #+linux 'epoll-io-base
                 #-(or bsd linux)
                 (core:unsupported-feature "io multiplexing")))

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
          ((io-watcher-registeredp watcher)
           (update-io-watcher base watcher events)
           (setf (io-watcher-events watcher) events)
           (when handler
             (setf (io-watcher-handler watcher) handler)))
          (t
           (add-io-watcher base watcher)
           (setf (io-watcher-registeredp watcher) t)
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
          (setf (io-watcher-registeredp watcher) nil)
          t)))))

(defun dispatch-fd-event (base fd events)
  (declare (type io-base base)
           (type (integer 0) fd)
           (type list events))
  (with-slots (fd-watchers mutex) base
    (let ((watcher (gethash fd fd-watchers)))
      (when (and watcher (io-watcher-registeredp watcher))
        (restart-case
            (funcall (io-watcher-handler watcher) events)
          (continue ()
            :report "Ignore the error and continue."
            nil))))))
