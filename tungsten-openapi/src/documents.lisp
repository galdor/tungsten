(in-package :openapi)

(define-condition unsupported-elements ()
  ((names
    :type string
    :initarg :name
    :reader unsupported-elements-names))
  (:report
   (lambda (condition stream)
     (with-slots (names) condition
       (format stream "~@(~A~) are not currently supported." names)))))

(defun unsupported-elements (names)
  (error 'unsupported-elements :names names))

(define-condition invalid-reference (error)
  ((reference
    :type list
    :initarg :reference)
   (format-control
    :type string
    :initarg :format-control)
   (format-arguments
    :type string
    :initarg :format-arguments))
  (:report
   (lambda (condition stream)
     (with-slots (reference format-control format-arguments) condition
       (let* ((pointer (json:pointer* "components" "schemas" (cadr reference)))
              (uri (uri:make-uri :fragment (json:serialize-pointer pointer))))
         (format stream "Invalid reference ~S: ~?."
                 (uri:serialize uri) format-control format-arguments))))))

(defun invalid-reference (reference format &rest arguments)
  (error 'invalid-reference :reference reference
                            :format-control format
                            :format-arguments arguments))

(define-condition unknown-operation (error)
  ((name
    :initarg :name))
  (:report
   (lambda (condition stream)
     (with-slots (name) condition
       (format stream "Unknown OpenAPI operation ~S." name)))))

(defclass document ()
  ((openapi-version
    :type (or string null)
    :initform nil
    :accessor document-openapi-version)
   (title
    :type (or string null)
    :initform nil
    :accessor document-title)
   (version
    :type (or string null)
    :initform nil
    :accessor document-version)
   (servers
    :type list
    :initform nil
    :accessor document-servers)
   (operations
    :type hash-table
    :initform (make-hash-table :test #'equal)
    :accessor document-operations)))

(defmethod print-object ((document document) stream)
  (print-unreadable-object (document stream :type t)
    (when (slot-boundp document 'title)
      (prin1 (document-title document) stream))
    (when (slot-boundp document 'version)
      (write-char #\Space stream)
      (write-string (document-version document) stream))))

(defclass operation ()
  ((id
    :type string
    :accessor operation-id)
   (method
    :type keyword
    :accessor operation-method)
   (path-template
    :type string
    :accessor operation-path-template)
   (summary
    :type (or string null)
    :initform nil
    :accessor operation-summary)
   (description
    :type (or string null)
    :initform nil
    :accessor operation-description)
   (servers
    :type list
    :initform nil
    :accessor operation-servers)
   (deprecated
    :type boolean
    :initform nil
    :accessor operation-deprecated)
   (parameters
    :type list
    :initform nil
    :accessor operation-parameters)
   (request-body
    :type (or request-body null)
    :initform nil
    :accessor operation-request-body)
   (default-response
    :type (or response null)
    :initform nil
    :accessor operation-default-response)
   (responses
    :type list
    :initform nil
    :accessor operation-responses)))

(defmethod print-object ((operation operation) stream)
  (print-unreadable-object (operation stream :type t)
    (prin1 (operation-id operation) stream)))

(deftype parameter-location ()
  '(member :query :header :path :cookie))

(deftype encoding-style ()
  '(member
    :matrix :label :form :simple :space-delimited :pipe-delimited
    :deep-object))

(defclass parameter ()
  ((name
    :type string
    :accessor parameter-name)
   (description
    :type (or string null)
    :initform nil
    :accessor parameter-description)
   (location
    :type parameter-location
    :accessor parameter-location)
   (required
    :type boolean
    :accessor parameter-required)
   (deprecated
    :type boolean
    :initform nil
    :accessor parameter-deprecated)
   (style
    :type encoding-style
    :accessor parameter-style)
   (explode
    :type boolean
    :accessor parameter-explode)
   (schema
    :accessor parameter-schema)
   (json-mapping
    :accessor parameter-json-mapping)))

(defmethod print-object ((parameter parameter) stream)
  (print-unreadable-object (parameter stream :type t)
    (when (slot-boundp parameter 'location)
      (princ (slot-value parameter 'location) stream))
    (when (slot-boundp parameter 'name)
      (write-char #\Space stream)
      (prin1 (slot-value parameter 'name) stream))))

(defclass request-body ()
  ((description
    :type (or string null)
    :initform nil
    :accessor request-body-description)
   (required
    :type boolean
    :initform nil
    :accessor request-body-required)
   (content
    :type list
    :initform nil
    :accessor request-body-content)))

(defclass response ()
  ((description
    :type (or string null)
    :initform nil
    :accessor response-description)
   (headers
    :type list
    :initform nil
    :accessor response-headers)
   (content
    :type list
    :initform nil
    :accessor response-content)))

(defclass header ()
  ((description
    :type (or string null)
    :initform nil
    :accessor header-description)
   (required
    :type boolean
    :accessor header-required)
   (deprecated
    :type boolean
    :initform nil
    :accessor header-deprecated)
   (style
    :type encoding-style
    :accessor header-style)
   (explode
    :type boolean
    :accessor header-explode)
   (schema
    :accessor header-schema)
   (json-mapping
    :accessor header-json-mapping)))

(defclass media-type ()
  ((schema
    :accessor media-type-schema)
   (json-mapping
    :accessor media-type-json-mapping)
   (property-encodings
    :type list
    :initform nil
    :accessor media-type-property-encodings)))

(defclass property-encoding ()
  ((content-type
    :type string
    :accessor property-encoding-content-type)
   (headers
    :type list
    :initform nil
    :accessor property-encoding-headers)
   (style
    :type encoding-style
    :accessor property-encoding-style)
   (explode
    :type boolean
    :accessor property-encoding-explode)))

(defun parse-document (string)
  (declare (type string string))
  (build-document (json:parse string :mapping 'document)))

(defun build-document (document-value)
  (let ((document (make-instance 'document)))
    (dolist (member document-value)
      (case (car member)
        (openapi
         (setf (document-openapi-version document) (cdr member)))
        (info
         (build-document/info (cdr member) document))
        (servers
         (build-document/servers (cdr member) document))
        (paths
         (build-document/paths (cdr member) document-value document))))
    document))

(defun build-document/info (value document)
  (declare (type list value)
           (type document document))
  (dolist (member value)
    (case (car member)
      (title
       (setf (document-title document) (cdr member)))
      (version
       (setf (document-version document) (cdr member))))))

(defun build-document/servers (value document)
  (declare (type vector value)
           (type document document))
  (dotimes (i (length value))
    (let ((value (aref value i)))
      (when (assoc 'variables value)
        (unsupported-elements "server variables"))
      (push (cdr (assoc 'url value)) (document-servers document)))))

(defun build-document/paths (paths-value document-value document)
  (declare (type list paths-value document-value)
           (type document document))
  (dolist (path-member paths-value)
    (let* ((path-template (car path-member))
           (path-value (cdr path-member))
           (parameter-values (cdr (assoc 'parameters path-value)))
           (path-parameters nil))
      (when parameter-values
        (dotimes (i (length parameter-values))
          (let* ((value (aref parameter-values i))
                 (parameter (build-parameter value document-value))
                 (name (parameter-name parameter))
                 (member (assoc name path-parameters)))
            (if member
                (rplacd member parameter)
                (push (cons name parameter) path-parameters)))))
      (dolist (member path-value)
        (case (car member)
          ((get put post delete options head patch trace)
           (let ((method (intern (symbol-name (car member)) :keyword)))
             (build-document/operation method path-template
                                       (cdr member) path-parameters
                                       document-value document))))))))

(defun build-document/operation (method path-template
                                 operation-value path-parameters
                                 document-value document)
  (declare (type keyword method)
           (type list operation-value path-parameters)
           (type document document))
  (let ((components-value (cdr (assoc 'components document-value)))
        (operation (make-instance 'operation)))
    (setf (operation-method operation) method)
    (setf (operation-path-template operation) path-template)
    (dolist (member operation-value)
      (case (car member)
        (operation-id
         (setf (operation-id operation) (cdr member)))
        (summary
         (setf (operation-summary operation) (cdr member)))
        (description
         (setf (operation-description operation) (cdr member)))
        (deprecated
         (setf (operation-deprecated operation) (cdr member)))
        (parameters
         (let ((parameter-values (cdr member))
               (parameters (copy-seq path-parameters)))
           (dotimes (i (length parameter-values))
             (let* ((value (resolve-component-value (aref parameter-values i)
                                                    components-value))
                    (parameter
                      (build-parameter value document-value))
                    (name (parameter-name parameter))
                    (member (assoc name parameters)))
               (if member
                   (rplacd member parameter)
                   (push (cons name parameter) parameters))))
           (setf (operation-parameters operation)
                 (mapcar #'cdr parameters))))
        (request-body
         (setf (operation-request-body operation)
               (build-request-body (cdr member) document-value)))
        (responses
         (dolist (member (cdr member))
           (let* ((code (car member))
                  (value
                    (resolve-component-value (cdr member) components-value))
                  (response (build-response value document-value)))
             (if (eq code :default)
                 (setf (operation-default-response operation) response)
                 (push (cons code response)
                       (operation-responses operation))))))))
    (setf (gethash (operation-id operation) (document-operations document))
          operation)))

(defun build-parameter (parameter-value document-value)
  (declare (type list parameter-value document-value))
  (let ((components-value (cdr (assoc 'components document-value)))
        (parameter (make-instance 'parameter)))
    (dolist (member parameter-value)
      (case (car member)
        (name
         (setf (parameter-name parameter) (cdr member)))
        (in
         (setf (parameter-location parameter) (cdr member)))
        (description
         (setf (parameter-description parameter) (cdr member)))
        (required
         (setf (parameter-required parameter) (cdr member)))
        (deprecated
         (setf (parameter-deprecated parameter) (cdr member)))
        (style
         (setf (parameter-style parameter) (cdr member)))
        (explode
         (setf (parameter-explode parameter) (cdr member)))
        (schema
         (setf (parameter-schema parameter)
               (resolve-schema-value (cdr member) components-value))
         (setf (parameter-json-mapping parameter)
               (build-schema-json-mapping (parameter-schema parameter))))))
    (unless (slot-boundp parameter 'required)
      (setf (parameter-required parameter)
            (eq (parameter-location parameter) :path)))
    (unless (slot-boundp parameter 'style)
      (setf (parameter-style parameter)
            (ecase (parameter-location parameter)
              (:query :form)
              (:path :simple)
              (:header :simple)
              (:cookie :form))))
    (unless (slot-boundp parameter 'explode)
      (setf (parameter-explode parameter)
            (eq (parameter-style parameter) :form)))
    (unless (slot-boundp parameter 'schema)
      (setf (parameter-schema parameter) nil))
    parameter))

(defun build-request-body (body-value document-value)
  (declare (type list body-value document-value))
  (let ((body (make-instance 'request-body)))
    (dolist (member body-value)
      (case (car member)
        (description
         (setf (request-body-description body) (cdr member)))
        (content
         (setf (request-body-content body)
               (build-content (cdr member) document-value)))))
    body))

(defun build-content (content-value document-value)
  (declare (type list content-value document-value))
  (let ((components-value (cdr (assoc 'components document-value)))
        (content nil))
    (dolist (member content-value)
      (let* ((media-range (car member))
             (media-type-value (cdr member))
             (media-type (make-instance 'media-type)))
        (dolist (member media-type-value)
          (case (car member)
            (schema
             (setf (media-type-schema media-type)
                   (resolve-schema-value (cdr member) components-value))
             (setf (media-type-json-mapping media-type)
                   (build-schema-json-mapping (media-type-schema media-type))))
            (encoding
             (let ((encodings nil))
               (dolist (member (cdr member))
                 (let* ((name (car member))
                        (encoding-data (cdr member))
                        (encoding
                          (build-property-encoding encoding-data
                                                   document-value)))
                   (push (cons name encoding) encodings)))
               (setf (media-type-property-encodings media-type)
                     encodings)))))
        (push (cons media-range media-type) content)))
    content))

(defun build-response (response-value document-value)
  (declare (type list response-value document-value))
  (let ((components-value (cdr (assoc 'components document-value)))
        (response (make-instance 'response)))
    (dolist (member response-value)
      (case (car member)
        (description
         (setf (response-description response) (cdr member)))
        (headers
         (let ((headers nil))
           (dolist (member (cdr member))
             (let* ((name (car member))
                    (header-data
                      (resolve-component-value (cdr member) components-value))
                    (header (build-header header-data document-value)))
               (push (cons name header) headers)))
           (setf (response-headers response) headers)))
        (content
         (setf (response-content response)
               (build-content (cdr member) document-value)))))
    response))

(defun build-property-encoding (encoding-data document-value)
  (declare (type list encoding-data document-value))
  (let ((components-value (cdr (assoc 'components document-value)))
        (encoding (make-instance 'property-encoding)))
    (dolist (member encoding-data)
      (case (car member)
        (content-type
         (setf (property-encoding-content-type encoding) (cdr member)))
        (headers
         (let ((headers nil))
           (dolist (member (cdr member))
             (let* ((name (car member))
                    (header-data
                      (resolve-component-value (cdr member)
                                               components-value))
                    (header (build-header header-data document-value)))
               (push (cons name header) headers)))
           (setf (property-encoding-headers encoding) headers)))
        (style
         (setf (property-encoding-style encoding) (cdr member)))
        (explode
         (setf (property-encoding-explode encoding) (cdr member))))
      (unless (slot-boundp encoding 'explode)
        (setf (property-encoding-explode encoding)
              (eq (property-encoding-style encoding) :form)))
      encoding)))

(defun build-header (header-value document-value)
  (declare (type list header-value document-value))
  (let ((components-value (cdr (assoc 'components document-value)))
        (header (make-instance 'header)))
    (dolist (member header-value)
      (case (car member)
        (description
         (setf (header-description header) (cdr member)))
        (required
         (setf (header-required header) (cdr member)))
        (deprecated
         (setf (header-deprecated header) (cdr member)))
        (style
         (setf (header-style header) (cdr member)))
        (explode
         (setf (header-explode header) (cdr member)))
        (schema
         (setf (header-schema header)
               (resolve-schema-value (cdr member) components-value))
         (setf (header-json-mapping header)
               (build-schema-json-mapping (header-schema header))))))
    (unless (slot-boundp header 'required)
      (setf (header-required header) t))
    (unless (slot-boundp header 'style)
      (setf (header-style header) :simple))
    (unless (slot-boundp header 'explode)
      (setf (header-explode header)
            (eq (header-style header) :form)))
    (unless (slot-boundp header 'schema)
      (setf (header-schema header) nil))
    header))

(defun resolve-component-value (object-value components-value)
  (declare (type list object-value components-value))
  (let ((visited-references nil))
    (labels ((component-group (ref-type)
               (ecase ref-type
                 (callback 'callbacks)
                 (example 'examples)
                 (header 'headers)
                 (link 'links)
                 (parameter 'parameters)
                 (path-item 'path-items)
                 (request-body 'request-bodies)
                 (response 'responses)
                 (schema 'schemas)
                 (security-scheme 'security-schemes)))
             (resolve (object-value)
               (let* ((reference (cdr (assoc 'ref object-value))))
                 (cond
                   (reference
                    (let* ((name (cadr reference)))
                      (when (member reference visited-references :test #'equal)
                        (push reference visited-references)
                        (invalid-reference reference
                                           "circular reference: ~{~S~^ -> ~}"
                                           (mapcar #'cadr
                                                   (nreverse
                                                    visited-references))))
                      (push reference visited-references)
                      (let* ((ref-type (car reference))
                             (group-name (component-group ref-type))
                             (group (cdr (assoc group-name components-value)))
                             (component-value
                               (cdr (assoc name group :test #'string=))))
                        (unless component-value
                          (invalid-reference reference
                                             "missing target component"))
                        (resolve component-value))))
                   (t
                    object-value)))))
      (resolve object-value))))

(defun resolve-schema-value (schema-value components-value)
  (flet ((resolve (value)
           (resolve-schema-value value components-value)))
    (let ((schema-value
            (resolve-component-value schema-value components-value)))
      (dolist (member schema-value)
        (case (car member)
          (all-of
           (rplacd member (map 'vector #'resolve (cdr member))))
          (one-of
           (rplacd member (map 'vector #'resolve (cdr member))))
          (any-of
           (rplacd member (map 'vector #'resolve (cdr member))))
          (not
           (rplacd member (resolve (cdr member))))
          (items
           (rplacd member (resolve (cdr member))))
          (properties
           (dolist (property (cdr member))
             (rplacd property (resolve (cdr property)))))
          (additional-properties
           (unless (typep (cdr member) 'boolean)
             (rplacd member (resolve (cdr member)))))))
      schema-value)))

(defun document-operation (document name)
  (declare (type document document)
           (type string name))
  (or (gethash name (document-operations document))
      (error 'unknown-operation :name name)))
