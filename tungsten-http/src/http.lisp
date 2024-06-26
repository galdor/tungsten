(in-package :http)

(defparameter *max-request-line-length* 256)
(defparameter *max-status-line-length* 256)
(defparameter *max-header-length* 8192)

(define-condition http-error (error)
  ())

(define-condition connection-closed (http-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "HTTP connection closed"))))

(define-condition invalid-redirection-location (http-error)
  ((location
    :type string
    :initarg :location))
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "invalid Location header field in HTTP response"))))

(define-condition too-many-redirections (http-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "too many HTTP redirections"))))

(define-condition http-parse-error (parse-error)
  ((description
     :type string
     :initarg :description
     :reader http-parse-error-description))
  (:report
   (lambda (condition stream)
     (format stream "HTTP parse error: ~A"
             (http-parse-error-description condition)))))

(defun http-parse-error (format &rest args)
  (let ((description (apply #'format nil format args)))
    (error 'http-parse-error :description description)))

(define-condition status-line-too-long (http-parse-error)
  ((data
    :type core:octet-vector
    :initarg :data))
  (:default-initargs
   :description "status line too long"))

(define-condition request-line-too-long (http-parse-error)
  ((data
    :type core:octet-vector
    :initarg :data))
  (:default-initargs
   :description "request line too long"))

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

(define-condition missing-request-target-host (error)
  ((target
    :type uri:uri
    :initarg :target
    :reader missing-request-target-host-target))
  (:report
   (lambda (condition stream)
     (format stream "missing host in HTTP request target ~S"
             (missing-request-target-host-target condition)))))

(defun request-method-equal (method1 method2)
  (cond
    ((and (symbolp method1) (symbolp method2))
     (eq method1 method2))
    ((and (stringp method1) (stringp method2))
     (string= method1 method2))
    (t
     (equalp (string method1) (string method2)))))

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
  (let ((string (header-field header "Content-Length")))
    (when string
      (let ((content-length
              (handler-case
                  (parse-integer string)
                (error ()
                  (http-parse-error
                   "invalid Content-Length header field ~S" string)))))
        (when (< content-length 0)
          (http-parse-error "invalid negative Content-Length header field"))
        content-length))))

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

(defun parse-request-method (data start end)
  (declare (type core:octet-vector data)
           (type (integer 0) start end))
  (let ((string (text:decode-string data :encoding :ascii
                                         :start start :end end)))
    (cond
      ((string= string "")
       (http-parse-error "empty method"))
      ((equalp string "get")
       :get)
      ((equalp string "head")
       :head)
      ((equalp string "post")
       :post)
      ((equalp string "put")
       :put)
      ((equalp string "delete")
       :delete)
      ((equalp string "connect")
       :connect)
      ((equalp string "options")
       :options)
      ((equalp string "trace")
       :trace)
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
           (token-end (position-if-not #'htab-char-p string
                                       :from-end t
                                       :start start :end comma))
           (token (subseq string start (if token-end (1+ token-end) start))))
      (unless (string= token "")
        (push token tokens))
      (setf start (1+ comma)))))

(defun body-chunked-p (header)
  ;; RFC 9112 6.3.
  ;;
  ;; If a Transfer-Encoding header field is present in a request and the
  ;; chunked transfer coding is not the final encoding, the message body
  ;; length cannot be determined reliably; the server MUST respond with the
  ;; 400 (Bad Request) status code and then close the connection.
  (mapl (lambda (list)
          (when (equalp (car list) "chunked")
            (if (null (cdr list))
                (return-from body-chunked-p t)
                (http-parse-error "invalid intermediary chunked encoding"))))
        (header-field-tokens header "Transfer-Encoding"))
  nil)

(defun read-header (stream)
  (declare (type stream stream))
  (do ((buffer (system:io-stream-read-buffer stream))
       (eol-octets (text:eol-octets :crlf))
       (eol nil)
       (header nil))
      ((and eol (= eol (core:buffer-start buffer)))
       (core:buffer-skip buffer (length eol-octets))
       (nreverse header))
    (when (>= (core:buffer-length buffer) *max-header-length*)
      (error 'header-too-large :data (core:buffer-content buffer)))
    (setf eol (search eol-octets (core:buffer-data buffer)
                      :start2 (core:buffer-start buffer)
                      :end2 (core:buffer-end buffer)))
    (cond
      ((and eol (> eol (core:buffer-start buffer)))
       (cond
         ((htab-octet-p
           (aref (core:buffer-data buffer) (core:buffer-start buffer)))
          ;; Header field continuation
          (when (null header)
            (http-parse-error "invalid header field continuation"))
          (let ((start (position-if-not 'htab-octet-p (core:buffer-data buffer)
                                        :start (core:buffer-start buffer)
                                        :end eol)))
            (rplacd (car header)
                    (concatenate 'vector (cdar header)
                                 (core:octet-vector* #.(char-code #\Space))
                                 (subseq (core:buffer-data buffer)
                                         start eol)))))
         (t
          ;; Header field
          (multiple-value-bind (name value)
              (parse-header-field (core:buffer-data buffer)
                                  (core:buffer-start buffer) eol)
            (push (cons name value) header))))
       (core:buffer-skip-to buffer (+ eol (length eol-octets))))
      ((null eol)
       (when (zerop (system:io-stream-read-more stream))
         (error 'connection-closed))))))

(defun read-trailer (stream)
  (read-header stream))

(defun read-body-and-trailer (header stream)
  (declare (type header header)
           (type stream stream))
  (let ((content-length (header-content-length header))
        (chunked (body-chunked-p header)))
    (when (and chunked content-length)
      (http-parse-error "Content-Length set with chunked body"))
    (cond
      (chunked
       (do ((body (make-array 0 :element-type 'core:octet :adjustable t))
            (body-length 0)
            (chunk-length nil))
           ((eql chunk-length 0)
            (values
             (adjust-array body body-length)
             (read-trailer stream)))
         (setf chunk-length (read-chunk-header stream))
         (when (> chunk-length 0)
           (setf body (adjust-array body (+ body-length chunk-length)))
           (when (< (read-sequence body stream :start body-length)
                    chunk-length)
             (http-parse-error "truncated chunk"))
           (incf body-length chunk-length)
           (unless (and (= (read-byte stream) #.(char-code #\Return))
                        (= (read-byte stream) #.(char-code #\Newline)))
             (http-parse-error "missing chunk end-of-line sequence")))))
      (content-length
       (let* ((body (make-array content-length :element-type 'core:octet))
              (nb-read (read-sequence body stream)))
         (when (< nb-read content-length)
           (http-parse-error "truncated body"))
         (values body nil)))
      (t
       (values nil nil)))))

(defun read-chunk-header (stream)
  (declare (type stream stream))
  (let* ((line (read-line stream))
         (end (or (position #\; line) (length line))))
    (handler-case
        (let ((length (parse-integer line :end end :radix 16)))
          (unless (>= length 0)
            (http-parse-error "invalid negative chunk length ~S" line))
          length)
      (error ()
        (http-parse-error "invalid chunk header ~S" line)))))
