(in-package :http)

(defparameter *max-status-line-length* 256)
(defparameter *max-header-length* 8192)

(define-condition http-error (error)
  ())

(define-condition connection-closed (http-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "HTTP connection closed."))))

(define-condition invalid-redirection-location (http-error)
  ((location
    :type string
    :initarg :location))
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Invalid Location header field in HTTP response."))))

(define-condition too-many-redirections (http-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Too many HTTP redirections."))))

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

(deftype request-method ()
  '(or symbol string))

(deftype response-status ()
  '(integer 0 999))

(deftype protocol-version ()
  '(or (member :http-1.0 :http-1.1) string))

(deftype body ()
  '(or core:octet-vector string))

(deftype header ()
  'list)

(deftype header-field-value ()
  '(or core:octet-vector string))

(deftype header-field ()
  '(cons string header-field-value))

(define-condition missing-request-target-host ()
  ((target
    :type uri:uri
    :initarg :target))
  (:report
   (lambda (c stream)
     (with-slots (target) c
       (format stream "Missing host in HTTP request target ~S." target)))))

(defun request-method-string (method)
  (declare (type request-method method))
  (etypecase method
    (symbol
     (symbol-name method))
    (string
     method)))

(defun make-request-target (uri)
  (uri:make-uri :path (or (uri:uri-path uri) "/")
                :query (uri:uri-query uri)
                :fragment (uri:uri-fragment uri)))

(defun host-header-field (uri)
  (declare (type uri:uri uri))
  ;; We take care not to include the port in the value if this is the default
  ;; port for the transport used; infortunately, some servers will respond
  ;; with a Location header field based on the value of the Host header field
  ;; without any host/port analysis. For example, GitHub will redirect a
  ;; request for http://github.com with a Host header field set to
  ;; "github.com:80" to "https://github.com:80/".
  (let* ((scheme (uri:uri-scheme uri))
         (host (let ((host (uri:uri-host uri)))
                 (if (find #\: host)
                     (concatenate 'nil "[" host "]")
                     host)))
         (port (let ((port (uri:uri-port uri)))
                 (if (or (null port)
                         (and (= port 80) (equalp scheme "http"))
                         (and (= port 443) (equalp scheme "https")))
                     nil
                     port))))
    (format nil "~A~@[:~D~]" host port)))

(defun protocol-version-string (version)
  (declare (type protocol-version version))
  (cond
    ((eq version :http-1.0) "HTTP/1.0")
    ((eq version :http-1.1) "HTTP/1.1")
    ((typep version 'string) version)))

(defun header-field (header name)
  (declare (type header header)
           (type string name))
  (cdr (assoc name header :test #'equalp)))

(defun header-field-values (header name)
  (declare (type header header)
           (type string name))
  (let ((values nil))
    (dolist (field header (nreverse values))
      (when (equalp (car field) name)
        (push (cdr field) values)))))

(defun header-field-tokens (header name)
  (declare (type header header)
           (type string name))
  (let ((tokens nil))
    (dolist (value (header-field-values header name) (nreverse tokens))
      (dolist (token (parse-tokens value))
        (push token tokens)))))

(defun delete-header-fields (header names)
  (delete-if (lambda (field)
               (member (car field) names :test #'string=))
             header))

(defun header-content-length (header)
  (let* ((string (header-field header "Content-Length"))
         (content-length
           (progn
             (unless string
               (http-parse-error "missing Content-Length header field"))
             (handler-case
                 (parse-integer string)
               (error ()
                 (http-parse-error
                  "invalid Content-Length header field ~S" string))))))
    (when (< content-length 0)
      (http-parse-error "invalid negative Content-Length header field"))
    content-length))

(declaim (inline htab-octet-p))
(defun htab-octet-p (octet)
  (or (= octet #.(char-code #\Space))
      (= octet #.(char-code #\Tab))))

(declaim (inline htab-char-p))
(defun htab-char-p (char)
  (or (char= char #\Space)
      (char= char #\Tab)))

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

(defun parse-tokens (string)
  (declare (type string string))
  (do ((start 0)
       (end (length string))
       (tokens nil))
      ((> start end)
       (nreverse tokens))
    (setf start (or (position-if-not #'htab-char-p string
                                     :start start :end end)
                    end))
    (let* ((comma (or (position #\, string :start start :end end) end))
           (token (subseq string start comma)))
      (unless (string= token "")
        (push token tokens))
      (setf start (1+ comma)))))
