(in-package :influxdb)

(defvar *client* nil)

(define-condition request-failure (error)
  ((method
    :type http:request-method
    :initarg :method
    :reader request-failure-method)
   (uri
    :type uri:uri
    :initarg :uri
    :reader request-failure-uri)
   (status
    :type http:response-status
    :initarg :status
    :reader request-failure-status)
   (message
    :type string
    :initarg :message
    :reader request-failure-message))
  (:report
   (lambda (condition stream)
     (with-slots (method uri status message) condition
       (format stream "InfluxDB request ~A ~A failed with status ~D.~%"
               method (uri:serialize uri) status)
       (terpri stream)
       (write-string message stream)
       (terpri stream)))))

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

(defun write-uri (base-uri bucket &key org)
  (declare (type (or uri:uri string) base-uri)
           (type string bucket)
           (type (or string null) org))
  (let ((uri (uri:resolve-reference "/api/v2/write" base-uri)))
    (push (cons "bucket" bucket) (uri:uri-query uri))
    (when org
      (push (cons "org" org) (uri:uri-query uri)))
    uri))

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
    (do ()
        ((system:with-mutex (mutex) closingp)
         nil)
      (restart-case
          (handler-bind
              ((error
                 (lambda (condition)
                   (declare (ignore condition))
                   (unless core:*interactive*
                     ;; TODO logging
                     (invoke-restart 'continue)))))
              (process-points client))
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
  (let* ((uri (client-write-uri client))
         (body (with-output-to-string (stream)
                 (write-points points stream)))
         (response (http:send-request :post uri :body body))
         (status (http:response-status response)))
    (unless (<= 200 status 299)
      (let ((message (response-error-message response)))
        (error 'request-failure :method :post :uri uri
                                :status status :message message)))))

(defun response-error-message (response)
  (declare (type http:response response))
  ;; We truncate the body to a reasonable length because InfluxDB sometimes
  ;; dumps the entire request payload in the body. Note that we have to
  ;; convert the body to a string before truncating it to make sure we do not
  ;; cut in the middle of a multi-byte sequence.
  (let ((body (text:decode-string (http:response-body response)
                                  :encoding :utf-8))
        (max-length 200))
    (if (> (length body) max-length)
        (subseq body 0 max-length)
        body)))

(defun finalize-point (point client)
  ;; We currently do not support adding tags to the client after startup so we
  ;; are free to access them without any lock.
  (dolist (tag (client-tags client))
    (push tag (point-tags point))))

(defun enqueue-point (point &key (client *client*))
  (declare (type point point)
           (type client client))
  (with-slots (mutex points) client
    (system:with-mutex (mutex)
      (push point points))
    t))
