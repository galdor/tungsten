(in-package :system)

(defclass kqueue-io-base (io-base)
  ((fd
    :type (or (integer 0) null)
    :initarg :fd
    :reader kqueue-io-base-fd)))

(defmethod initialize-instance :after ((base kqueue-io-base)
                                       &key &allow-other-keys)
  (with-slots (fd) base
    (setf fd (kqueue))))

(defmethod close-io-base ((base kqueue-io-base))
  (with-slots (fd) base
    (when fd
      (close-fd fd)
      (setf fd nil)
      t)))

(defmacro with-kevents ((%kevent count) &body body)
  `(ffi:with-foreign-value (,%kevent 'kevent :count ,count)
     (ffi:clear-foreign-memory ,%kevent
                               (* ,count (ffi:foreign-type-size 'kevent)))
     ,@body))

(defun init-kevent (%kevent ident filter flags)
  (declare (type ffi:pointer %kevent)
           (type (or fd ffi:pointer) ident)
           (type list flags)
           (type keyword filter))
  (setf (ffi:foreign-structure-member %kevent 'kevent :ident) ident)
  (setf (ffi:foreign-structure-member %kevent 'kevent :filter) filter)
  (setf (ffi:foreign-structure-member %kevent 'kevent :flags) flags))

(defmethod add-io-watcher ((base kqueue-io-base) (watcher io-watcher))
  (with-slots (fd events) watcher
    (assert (<= (length events) 2))     ; (:read :write)
    (with-kevents (%kevents 2)
      (let ((idx 0))
        (dolist (event events)
          (init-kevent (ffi:pointer+ %kevents
                                     (* idx (ffi:foreign-type-size 'kevent)))
                       fd (io-event-to-kqueue-filter event)
                       '(:ev-add))
          (incf idx))
        (kevent (kqueue-io-base-fd base)
                %kevents idx (ffi:null-pointer) 0 (ffi:null-pointer))))))

(defmethod update-io-watcher ((base kqueue-io-base) (watcher io-watcher)
                              events)
  (let ((read-flag (if (member :read events) :ev-add :ev-delete))
        (write-flag (if (member :write events) :ev-add :ev-delete)))
    (with-kevents (%kevents 2)
      (init-kevent %kevents
                   (io-watcher-fd watcher)
                   (io-event-to-kqueue-filter :evfilt-read)
                   (list read-flag))
      (init-kevent (ffi:pointer+ %kevents (ffi:foreign-type-size 'kevent))
                   (io-watcher-fd watcher)
                   (io-event-to-kqueue-filter :evfilt-write)
                   (list write-flag))
      (kevent (kqueue-io-base-fd base)
              %kevents 2 (ffi:null-pointer) 0 (ffi:null-pointer)))))

(defmethod remove-io-watcher ((base kqueue-io-base) (watcher io-watcher))
  (with-kevents (%kevents 2)
    (init-kevent %kevents (io-watcher-fd watcher) :evfilt-read '(:ev-delete))
    (init-kevent (ffi:pointer+ %kevents (ffi:foreign-type-size 'kevent))
                 (io-watcher-fd watcher) :evfilt-write '(:ev-delete))
    (kevent (kqueue-io-base-fd base)
            %kevents 2 (ffi:null-pointer) 0 (ffi:null-pointer))))

(defmethod read-and-dispatch-io-events ((base kqueue-io-base) &key timeout)
  (declare (type (or (integer 0) null) timeout))
  (with-kevents (%kevents 32)
    (ffi:with-foreign-value (%timeout 'timespec)
      (when timeout
        (multiple-value-bind (seconds milliseconds) (floor timeout 1000)
          (setf (ffi:foreign-structure-member %timeout 'timespec :tv-sec)
                seconds)
          (setf (ffi:foreign-structure-member %timeout 'timespec :tv-nsec)
                (* milliseconds 1000000))))
      (let ((nb-events (kevent (kqueue-io-base-fd base)
                               (ffi:null-pointer) 0 %kevents 32
                               (if timeout %timeout (ffi:null-pointer)))))
        (dotimes (i nb-events)
          (let* ((%kevent
                   (ffi:pointer+ %kevents
                                 (* i (ffi:foreign-type-size 'kevent))))
                 (fd (ffi:foreign-structure-member %kevent 'kevent :ident))
                 (filter
                   (ffi:foreign-structure-member %kevent 'kevent :filter))
                 (event (kqueue-filter-to-io-event filter)))
            (when event
              (dispatch-fd-event base fd (list event)))))))))

(defun kqueue-filter-to-io-event (filter)
  (case filter
    (:evfilt-read :read)
    (:evfilt-write :write)))

(defun io-event-to-kqueue-filter (event)
  (declare (type io-event event))
  (ecase event
    (:read :evfilt-read)
    (:write :evfilt-write)))
