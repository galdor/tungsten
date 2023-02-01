(in-package :influxdb)

(defvar *client* nil)

(defclass client ()
  ((uri
    :type uri:uri
    :initarg :uri)
   (tags
    :type list
    :initarg :tags)
   (mutex
    :type system:mutex
    :initform (system:make-mutex :name "influxdb-client"))
   (thread
    :type (or system:thread null))
   (closingp
    :type boolean
    :initform nil)
   (points
    :type list
    :initform nil
    :accessor client-points)))

(defun start-client (uri &key tags)
  (declare (type (or uri:uri string) uri)
           (type list tags))
  (let* ((uri (if (stringp uri) (uri:parse uri) uri))
         (client (make-instance 'client :uri uri :tags tags)))
    (setf (slot-value client 'thread)
          (system:make-thread "influxdb-client"
                              (lambda () (run-client client))))
    client))

(defun stop-client (client)
  (declare (type client client))
  (with-slots (mutex thread closingp) client
    (system:with-mutex (mutex)
      (setf closingp t))
    (system:join-thread thread)
    (setf thread nil)
    t))

(defun run-client (client)
  (declare (type client client))
  (with-slots (mutex closingp) client
    (do ((points nil (system:with-mutex (mutex)
                       (let ((points (client-points client)))
                         (setf (client-points client) nil)
                         points))))
        ((system:with-mutex (mutex) closingp)
         nil)
      (when points
        (restart-case
            (process-points points client)
          (continue ()
            :report "Continue processing points.")))
      (sleep 1.0))))

(defun process-points (points client)
  (declare (type list points)
           (type client client)
           (ignore client))
  (format t "Processing points:~%~S~%" points))

(defun enqueue-point (point &key (client *client*))
  (declare (type point point)
           (type client client))
  (with-slots (mutex points) client
    (system:with-mutex (mutex)
      (push point points))))
