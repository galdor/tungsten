(in-package :influxdb)

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

(defun send-points (points uri)
  (let* ((body (with-output-to-string (stream)
                 (write-points points stream)))
         (response (http:send-request :post uri :body body))
         (status (http:response-status response)))
    (unless (<= 200 status 299)
      (let ((message (response-error-message response)))
        (error 'request-failure :method :post :uri uri
                                :status status :message message)))))

(defun write-uri (base-uri bucket &key org)
  (declare (type (or uri:uri string) base-uri)
           (type string bucket)
           (type (or string null) org))
  (let ((uri (uri:resolve-reference "/api/v2/write" base-uri)))
    (push (cons "bucket" bucket) (uri:uri-query uri))
    (when org
      (push (cons "org" org) (uri:uri-query uri)))
    uri))

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
