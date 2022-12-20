(in-package :system)

(defclass socket ()
  ((file-descriptor
    :type (or (integer 0) null)
    :initarg :file-descriptor
    :initform nil
    :reader socket-file-descriptor)
   (address
    :type socket-address
    :initarg :address
    :reader socket-address)))

(defmethod close ((socket socket) &key abort)
  (declare (ignore abort))
  (with-slots (file-descriptor) socket
    (when file-descriptor
      (close-fd file-descriptor)
      (setf file-descriptor nil)
      t)))

(defmethod open-stream-p ((socket socket))
  (not (null (socket-file-descriptor socket))))
