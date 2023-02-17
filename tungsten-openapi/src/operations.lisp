(in-package :openapi)

(define-condition invalid-path-template (error)
  ((template
    :type string
    :initarg :template)
   (format-control
    :type string
    :initarg :format-control)
   (format-arguments
    :type list
    :initarg :format-arguments))
  (:report
   (lambda (condition stream)
     (with-slots (template format-control format-arguments) condition
       (format stream "Invalid path template ~S: ~?."
               template format-control format-arguments)))))

(defun invalid-path-template (template format &rest arguments)
  (error 'invalid-path-template :template template
                                :format-control format
                                :format-arguments arguments))

(define-condition missing-parameter-value (error)
  ((location
    :type parameter-location
    :initarg :location)
   (name
    :type string
    :initarg :name))
  (:report
   (lambda (condition stream)
     (with-slots (location name) condition
       (format stream "Missing value for ~A parameter ~S." location name)))))

(defun execute-operation (document name &key parameters body)
  (declare (type document document)
           (type string name)
           (type list parameters)
           (ignore body))
  (let* ((operation (document-operation document name))
         (method (operation-method operation))
         (uri (operation-uri operation document :parameters parameters)))
    ;; TODO Add header fields based on parameters with location :HEADER
    ;;
    ;; TODO Encode BODY according to the request body of OPERATION and pass it
    ;; to HTTP:SEND-REQUEST.
    (let* ((response (http:send-request method uri))
           (status (http:response-status response))
           (content-type (http:response-header-field response "Content-Type"))
           (media-type
             (operation-response-media-type operation status content-type))
           (mapping (media-type-json-mapping media-type))
           (body (when (http:response-body response)
                   (json:parse (text:decode-string
                                (http:response-body response))
                               :mapping mapping))))
      (values body response))))

(defun operation-uri (operation document &key parameters)
  ;; According to OpenAPI specifications, "the path is appended (no relative
  ;; URL resolution) to the expanded URL from the Server Object’s url field in
  ;; order to construct the full URL"; thus we use simple path concatenation
  ;; instead of URI:RESOLVE-REFERENCE.
  ;;
  ;; It is not clear what we are supposed to do when there are multiple
  ;; servers specified in the document. Let us kick the subject down the road.
  (declare (type document document)
           (type operation operation)
           (type list parameters))
  (let* ((server-uri (car (document-servers document)))
         (path-template (operation-path-template operation))
         (path (expand-path-template path-template parameters
                                     (operation-parameters operation)))
         (uri (uri:copy-uri server-uri)))
    (setf (uri:uri-path uri)
          (concatenate 'string (uri:uri-path server-uri) path))
    uri))

(defun operation-parameter (operation location name)
  (declare (type operation operation)
           (type parameter-location location)
           (type string name))
  (find-if (lambda (parameter)
             (and (eq (parameter-location parameter) location)
                  (string= (parameter-name parameter) name)))
           (operation-parameters operation)))

(defun operation-response-media-type (operation status content-type)
  (declare (type operation operation)
           (type http:response-status status)
           (type string content-type))
  (let ((response (or (cdr (assoc status (operation-responses operation)))
                      (operation-default-response operation))))
    (when response
      (let* ((matches
               (mime:match-media-ranges (response-content response)
                                        (mime:media-type content-type)
                                        :key #'car))
             (first-match (car matches)))
        (cdr first-match)))))

(defun expand-path-template (template parameter-values parameters)
  (declare (type string template)
           (type list parameter-values parameters))
  (with-output-to-string (stream)
    (do ((start 0)
         (end (length template)))
        ((>= start end)
         nil)
      (let ((expression-start (position #\{ template :start start :end end)))
        (write-string (subseq template start (or expression-start end)) stream)
        (cond
          (expression-start
           (let ((expression-end
                   (position #\} template :start expression-start :end end)))
             (unless expression-end
               (invalid-path-template template
                                      "truncated template expression"))
             (let* ((expression
                      (subseq template (1+ expression-start) expression-end))
                    (value (expand-path-template-expression
                            template expression parameter-values parameters)))
               (write-string value stream))
             (setf start (1+ expression-end))))
          (t
           (setf start end)))))))

(defun expand-path-template-expression (template expression parameter-values
                                        parameters)
  (declare (type string template expression)
           (type list parameter-values parameters))
  (let* ((parameter (find-if
                     (lambda (parameter)
                       (and (eq (parameter-location parameter) :path)
                            (string= (parameter-name parameter) expression)))
                     parameters))
         (value (third (find-if (lambda (value)
                                  (and (eq (first value) :path)
                                       (string= (second value) expression)))
                                parameter-values))))
    (unless parameter
      (invalid-path-template template "unknown parameter ~S" expression))
    (unless value
      (error 'missing-parameter-value :location :path :name expression))
    (encode-parameter-value value parameter)))

(defun encode-parameter-value (value parameter)
  (declare (type parameter parameter)
           (ignore parameter))
  ;; TODO encoding
  ;;
  ;; See https://spec.openapis.org/oas/v3.1.0.html#style-values
  value)
