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
  ;; TODO requestBody
  ;; TODO responses
  ((id
    :type string
    :accessor operation-id)
   (method
    :type keyword
    :accessor operation-method)
   (path-pattern
    :type string
    :accessor operation-path-pattern)
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
    :accessor operation-parameters)))

(defmethod print-object ((operation operation) stream)
  (print-unreadable-object (operation stream :type t)
    (prin1 (operation-id operation) stream)))

(deftype parameter-location ()
  '(member :query :header :path :cookie))

(deftype parameter-style ()
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
    :type parameter-style
    :accessor parameter-style)
   (explode
    :type boolean
    :accessor parameter-explode)
   (schema
    :accessor parameter-schema)))

(defmethod print-object ((parameter parameter) stream)
  (print-unreadable-object (parameter stream :type t)
    (when (slot-boundp parameter 'location)
      (princ (slot-value parameter 'location) stream))
    (when (slot-boundp parameter 'name)
      (write-char #\Space stream)
      (prin1 (slot-value parameter 'name) stream))))

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
    (let* ((path-pattern (car path-member))
           (path-value (cdr path-member))
           (parameter-values (cdr (assoc 'parameters path-value)))
           (path-parameters nil))
      (when parameter-values
        (dotimes (i (length parameter-values))
          (let* ((value (aref parameter-values i))
                 (parameter (build-document/parameter value document-value))
                 (name (parameter-name parameter))
                 (member (assoc name path-parameters)))
            (if member
                (rplacd member parameter)
                (push (cons name parameter) path-parameters)))))
      (dolist (member path-value)
        (case (car member)
          ((get put post delete options head patch trace)
           (let ((method (intern (symbol-name (car member)) :keyword)))
             (build-document/operation method path-pattern
                                       (cdr member) path-parameters
                                       document-value document))))))))

(defun build-document/operation (method path-pattern
                                 operation-value path-parameters
                                 document-value document)
  (declare (type keyword method)
           (type list operation-value path-parameters)
           (type document document))
  (let ((operation (make-instance 'operation)))
    (setf (operation-method operation) method)
    (setf (operation-path-pattern operation) path-pattern)
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
             (let* ((value (aref parameter-values i))
                    (parameter
                      (build-document/parameter value document-value))
                    (name (parameter-name parameter))
                    (member (assoc name parameters)))
               (if member
                   (rplacd member parameter)
                   (push (cons name parameter) parameters))))
           (setf (operation-parameters operation)
                 (mapcar #'cdr parameters))))
        (request-body
         ;; TODO
         nil)
        (responses
         ;; TODO
         nil)))
    (setf (gethash (operation-id operation) (document-operations document))
          operation)))

(defun build-document/parameter (parameter-value document-value)
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
         (let ((schema-value
                 (resolve-component-value (cdr member) components-value)))
           (setf (parameter-schema parameter) schema-value)))))
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
      (setf (parameter-schema parameter) (build-schema nil)))
    parameter))

(defun resolve-component-value (object-value components-value)
  (declare (type list object-value components-value))
  (let ((visited-references nil))
    (labels ((resolve (object-value)
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
                      (let ((component-value
                              (cdr
                               (assoc name components-value :test #'string=))))
                        (unless component-value
                          (invalid-reference reference
                                             "missing target component"))
                        (resolve component-value))))
                   (t
                    object-value)))))
      (resolve object-value))))
