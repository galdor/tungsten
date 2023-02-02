(in-package :influxdb)

(defvar *client* nil)

(defclass client ()
  ((write-uri
    :type uri:uri
    :initarg :write-uri
    :reader client-write-uri)
   (tags
    :type list
    :initarg :tags
    :reader client-tags)
   (mutex
    :type system:mutex
    :initform (system:make-mutex :name "influxdb-client")
    :reader client-mutex)
   (thread
    :type (or system:thread null))
   (closingp
    :type boolean
    :initform nil)
   (points
    :type list
    :initform nil
    :accessor client-points)))

(defun start-client (base-uri bucket &key org tags)
  (declare (type (or uri:uri string) base-uri)
           (type list tags))
  (let* ((write-uri (write-uri base-uri bucket :org org))
         (client (make-instance 'client :write-uri write-uri :tags tags)))
    (push (cons "bucket" bucket) (uri:uri-query write-uri))
    (when org
      (push (cons "org" org) (uri:uri-query write-uri)))
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
    (do ((last-builtin-metrics-collection (time:current-timestamp)))
        ((system:with-mutex (mutex) closingp)
         nil)
      (restart-case
          (handler-bind
              ((error
                 (lambda (condition)
                   (unless core:*interactive*
                     (log:log-condition condition)
                     (invoke-restart 'continue)))))
            (let ((now (time:current-timestamp)))
              (when (> (time:timestamp-delta last-builtin-metrics-collection
                                             now)
                       *builtin-metrics-collection-interval*)
                (enqueue-points (collect-builtin-metrics))
                (setf last-builtin-metrics-collection now))
              (process-points client)))
        (continue ()
          :report "Continue processing points."))
      (sleep 1.0))))

(defun process-points (client)
  (let (points)
    (system:with-mutex ((client-mutex client))
      (setf points (client-points client))
      (setf (client-points client) nil))
    (when points
      (finalize-and-send-points points client))))

(defun finalize-and-send-points (points client)
  (declare (type list points)
           (type client client))
  (dolist (point points)
    (finalize-point point client))
  (send-points points (client-write-uri client)))

(defun finalize-point (point client)
  ;; We currently do not support adding tags to the client after startup so we
  ;; are free to access them without any lock.
  (dolist (tag (client-tags client))
    (push tag (point-tags point))))

(defun enqueue-point (point &key (client *client*))
  (declare (type point point)
           (type client client))
  (when client
    (with-slots (mutex points) client
      (system:with-mutex (mutex)
        (push point points))
      t)))

(defun enqueue-points (points &key (client *client*))
  (declare (type list points)
           (type client client))
  (when client
    (with-slots (mutex (client-points points)) client
      (system:with-mutex (mutex)
        (setf client-points (append points client-points)))
      t)))
