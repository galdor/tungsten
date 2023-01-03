(in-package :http)

(deftype request-method ()
  '(or symbol string))

(deftype request-target ()
  '(or uri:uri string))

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
    :type request-target
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

(defun request-target-string (target)
  (declare (type request-target target))
  (etypecase target
    (uri:uri
     (uri:serialize target))
    (string
     target)))

(defun request-target-uri (target)
  (declare (type request-target target))
  (etypecase target
    (uri:uri
     target)
    (string
     (uri:parse target))))

(defun normalize-request-target (target)
  (uri:make-uri :path (or (uri:uri-path target) "/")
                :query (uri:uri-query target)
                :fragment (uri:uri-fragment target)))

(defun request-target-host-header-field (target)
  ;; We assume that TARGET has already been normalized
  (declare (type uri:uri target))
  ;; We take care not to include the port in the value if this is the default
  ;; port for the transport used; infortunately, some servers will respond
  ;; with a Location header field based on the value of the Host header field
  ;; without any host/port analysis. For example, GitHub will redirect a
  ;; request for http://github.com with a Host header field set to
  ;; "github.com:80" to "https://github.com:80/".
  (let* ((scheme (uri:uri-scheme target))
         (host (let ((host (uri:uri-host target)))
                 (if (find #\: host)
                     (concatenate 'nil "[" host "]")
                     host)))
         (port (let ((port (uri:uri-port target)))
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
