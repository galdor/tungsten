(in-package :system)

(defclass epoll-io-base (io-base)
  ((fd
    :type (or (integer 0) null)
    :initarg :fd
    :reader epoll-io-base-fd)))

(defmethod initialize-instance :after ((base epoll-io-base)
                                       &key &allow-other-keys)
  (with-slots (fd) base
    (setf fd (epoll-create1 '(:epoll-cloexec)))))

(defmethod close-io-base ((base epoll-io-base))
  (with-slots (fd) base
    (when fd
      (close-fd fd)
      (setf fd nil)
      t)))

(defmethod add-io-watcher ((base epoll-io-base) (watcher io-watcher))
  (with-slots (fd events) watcher
    (ffi:with-foreign-value (%event 'epoll-event)
      (setf (ffi:struct-member %event 'epoll-event :events)
            (io-events-to-epoll-events events))
      (let ((%data (ffi:struct-member-pointer %event 'epoll-event :data)))
        (setf (ffi:foreign-union-member %data 'epoll-data :fd) fd))
      (epoll-ctl (epoll-io-base-fd base) :epoll-ctl-add fd %event))))

(defmethod update-io-watcher ((base epoll-io-base) (watcher io-watcher) events)
  (with-slots (fd) watcher
    (ffi:with-foreign-value (%event 'epoll-event)
      (setf (ffi:struct-member %event 'epoll-event :events)
            (io-events-to-epoll-events events))
      (let ((%data (ffi:struct-member-pointer %event 'epoll-event :data)))
        (setf (ffi:foreign-union-member %data 'epoll-data :fd) fd))
      (epoll-ctl (epoll-io-base-fd base) :epoll-ctl-mod fd %event))))

(defmethod remove-io-watcher ((base epoll-io-base) (watcher io-watcher))
  (with-slots (fd) watcher
    (epoll-ctl (epoll-io-base-fd base) :epoll-ctl-del fd (ffi:null-pointer))))

(defmethod read-and-dispatch-io-events ((base epoll-io-base) &key timeout)
  (declare (type (or (integer 0) null) timeout))
  (with-slots ((epoll-fd fd)) base
    (with-epoll-wait (%event epoll-fd 32 (or timeout -1))
      (let* ((events (epoll-events-to-io-events
                      (ffi:struct-member %event 'epoll-event :events)))
             (%data (ffi:struct-member-pointer %event 'epoll-event :data))
             (fd (ffi:foreign-union-member %data 'epoll-data :fd)))
        (dispatch-fd-event base fd events)))))

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
