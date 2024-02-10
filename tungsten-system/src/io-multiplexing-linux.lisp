(in-package :system)

(defclass linux-io-base (io-base)
  ((epoll-fd
    :type (or (integer 0) null)
    :initarg :epoll-fd
    :reader linux-io-base-epoll-fd)
   (timer-fd
    :type (or (integer 0) null)
    :initform nil
    :reader linux-io-base-timer-fd)))

(defmacro with-epoll-event ((%event events fd) &body body)
  `(ffi:with-foreign-value (,%event 'epoll-event)
     (ffi:clear-foreign-memory ,%event (ffi:foreign-type-size 'epoll-event))
     (setf (ffi:foreign-structure-member ,%event 'epoll-event :events)
           (io-events-to-epoll-events ,events))
     (setf (ffi:foreign-union-member
            (ffi:foreign-structure-member-pointer %event 'epoll-event :data)
            'epoll-data :fd)
           ,fd)
     ,@body))

(defmethod initialize-instance :after ((base linux-io-base)
                                       &key &allow-other-keys)
  (let (epoll-fd timer-fd)
    (core:abort-protect
        (progn
          (setf epoll-fd (epoll-create1 '(:epoll-cloexec)))
          (setf timer-fd
                (timerfd-create :clock-monotonic '(:tfd-cloexec :tfd-nonblock)))
          (with-epoll-event (%event '(:read) timer-fd)
            (epoll-ctl epoll-fd :epoll-ctl-add timer-fd %event))
          (setf (slot-value base 'epoll-fd) epoll-fd)
          (setf (slot-value base 'timer-fd) timer-fd))
        (close-fd timer-fd)
        (close-fd epoll-fd))))

(defmethod close-io-base ((base linux-io-base))
  (with-slots (epoll-fd timer-fd) base
    (when timer-fd
      (close-fd timer-fd)
      (setf timer-fd nil))
    (when epoll-fd
      (close-fd epoll-fd)
      (setf epoll-fd nil)
      t)))

(defmethod add-io-watcher ((base linux-io-base) (watcher io-watcher))
  (with-slots (fd events) watcher
    (with-epoll-event (%event events fd)
      (epoll-ctl (linux-io-base-epoll-fd base) :epoll-ctl-add fd %event))))

(defmethod update-io-watcher ((base linux-io-base) (watcher io-watcher) events)
  (with-slots (fd) watcher
    (with-epoll-event (%event events fd)
      (epoll-ctl (linux-io-base-epoll-fd base) :epoll-ctl-mod fd %event))))

(defmethod remove-io-watcher ((base linux-io-base) (watcher io-watcher))
  (with-slots (fd) watcher
    (epoll-ctl (linux-io-base-epoll-fd base) :epoll-ctl-del fd
               (ffi:null-pointer))))

(defmethod read-and-dispatch-io-events ((base linux-io-base) &key timeout)
  (declare (type (or float null) timeout))
  (with-slots (epoll-fd timer-fd) base
    (let ((timeout-ms (if timeout (round (* timeout 1e3)) -1)))
      (with-epoll-wait (%event epoll-fd 32 timeout-ms)
        (let* ((events (epoll-events-to-io-events
                        (ffi:foreign-structure-member %event 'epoll-event
                                                      :events)))
               (%data (ffi:foreign-structure-member-pointer %event 'epoll-event
                                                            :data))
               (fd (ffi:foreign-union-member %data 'epoll-data :fd)))
          (cond
            ((eql fd timer-fd)
             (ffi:with-foreign-value (%nb-expirations :uint64)
               (read-fd timer-fd %nb-expirations 8)
               (process-timer-event base)))
            (t
             (process-fd-event base fd events))))))))

(defun io-events-to-epoll-events (events)
  (declare (type list events))
  (let ((epoll-events nil))
    (dolist (event events epoll-events)
      (push (ecase event
              (:read :epollin)
              (:write :epollout)
              (:hangup :epollhup))
            epoll-events))))

(defun epoll-events-to-io-events (events)
  (declare (type list events))
  (let ((io-events nil))
    (dolist (event events io-events)
      (case event
        (:epollin (push :read io-events))
        (:epollout (push :write io-events))
        (:epollhup (push :hangup io-events))))))

(defmethod set-timer ((base linux-io-base) tick)
  (declare (type integer tick))
  (timerfd-settime (linux-io-base-timer-fd base) '(:tfd-timer-abstime) 0 tick))

(defmethod cancel-timer ((base linux-io-base))
  (set-timer base 0))
