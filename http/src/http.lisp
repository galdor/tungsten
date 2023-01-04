(in-package :http)

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

(deftype header-field-name ()
  '(or symbol string))

(deftype header-field-value ()
  '(or core:octet-vector string))

(deftype header-field ()
  '(cons header-field-name header-field-value))

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

(defun header-field-name-string (name)
  (declare (type header-field-name name))
  (etypecase name
    (symbol
     (symbol-name name))
    (string
     name)))

(defun header-field (header name)
  (declare (type header header)
           (type header-field-name name))
  (let ((name-string (header-field-name-string name)))
    (cdr (assoc name-string header :key 'header-field-name-string
                                   :test #'equalp))))

(defun delete-header-fields (header names)
  (delete-if (lambda (field)
               (member (car field) names :test #'string=))
             header))
