(in-package :libssh)

(defclass channel ()
  ((%channel
    :type ffi:pointer
    :initarg :%channel
    :reader channel-%channel)
   (%session
    :type ffi:pointer
    :initarg :%session
    :reader channel-%session)))

(defun open-channel (session)
  (declare (type session session))
  (with-slots (%session) session
    (let ((%channel (ssh-channel-new %session)))
      (core:abort-protect
          (progn
            (ssh-channel-open-session %channel %session)
            (make-instance 'channel :%channel %channel
                                    :%session %session))
        (unwind-protect
             (ssh-channel-close %channel %session)
          (ssh-channel-free %channel))))))

(defun close-channel (channel)
  (declare (type channel channel))
  (with-slots (%channel %session) channel
    (unwind-protect
         (ssh-channel-close %channel %session)
      (ssh-channel-free %channel))))

(defmacro with-channel ((channel session &rest args) &body body)
  `(let ((,channel (open-channel ,session ,@args)))
     (unwind-protect
          (progn
            ,@body)
       (close-channel ,channel))))
