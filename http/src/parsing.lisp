(in-package :http)

(defparameter *max-status-line-length* 256)
(defparameter *max-header-length* 8192)

(define-condition http-parse-error (parse-error)
  ((description
     :type string
     :initarg :description))
  (:report
   (lambda (condition stream)
     (with-slots (description) condition
       (format stream "HTTP parse error: ~A." description)))))

(defun http-parse-error (format &rest args)
  (let ((description (apply #'format nil format args)))
    (error 'http-parse-error :description description)))

(define-condition status-line-too-long (http-parse-error)
  ((data
    :type core:octet-vector
    :initarg :data))
  (:default-initargs
   :description "status line too long"))

(define-condition header-too-large (http-parse-error)
  ((data
    :type core:octet-vector
    :initarg :data))
  (:default-initargs
   :description "header too large"))

(declaim (inline htab-octet-p))
(defun htab-octet-p (octet)
  (or (= octet #.(char-code #\Space))
      (= octet #.(char-code #\Tab))))

(declaim (inline digit-octet-p))
(defun digit-octet-p (octet)
  (<= #.(char-code #\0) octet #.(char-code #\9)))

(defun parse-protocol-version (data start end)
  (declare (type core:octet-vector data)
           (type (integer 0) start end))
  (let ((string (text:decode-string data :encoding :ascii
                                         :start start :end end)))
    (cond
      ((string= string "")
       (http-parse-error "empty version"))
      ((equalp string "http/1.1")
       :http-1.1)
      ((equalp string "http/1.0")
       :http-1.0)
      (t
       string))))

(defun parse-response-status (data start end)
  (declare (type core:octet-vector data)
           (type (integer 0) start end))
  (when (>= start end)
    (http-parse-error "empty status code"))
  (do ((i start (1+ i)))
      ((>= i end)
       (parse-integer (text:decode-string data :encoding :ascii
                                               :start start :end end)))
    (unless (digit-octet-p (aref data i))
      (http-parse-error "invalid status code"))))

(defun parse-response-reason (data start end)
  (declare (type core:octet-vector data)
           (type (integer 0) start end))
  (when (>= start end)
    (http-parse-error "empty reason string"))
  (text:decode-string data :start start :end end))

(defun parse-header-field (data start end)
  (declare (type core:octet-vector data)
           (type (integer 0) start end))
  (when (>= start end)
    (http-parse-error "empty header field"))
  (let ((colon (position #.(char-code #\:) data :start start :end end)))
    (unless colon
      (http-parse-error "truncated header field"))
    (let ((name-end
            (position-if-not 'htab-octet-p data :from-end t
                                                :start start :end colon))
          (value-start
            (position-if-not 'htab-octet-p data :start (1+ colon) :end end)))
      (values (text:decode-string data :start start :end (1+ name-end))
              (text:decode-string data :start value-start :end end)))))
