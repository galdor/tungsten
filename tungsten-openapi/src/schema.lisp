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

(defclass schema ()
  ((openapi-version
    :type (or string null)
    :initform nil
    :accessor schema-openapi-version)
   (title
    :type (or string null)
    :initform nil
    :accessor schema-title)
   (version
    :type (or string null)
    :initform nil
    :accessor schema-version)
   (servers
    :type list
    :initform nil
    :accessor schema-servers)
   (operations
    :type hash-table
    :initform (make-hash-table :test #'equal)
    :accessor schema-operations)))

(defmethod print-object ((schema schema) stream)
  (print-unreadable-object (schema stream :type t)
    (when (slot-boundp schema 'title)
      (prin1 (schema-title schema) stream))
    (when (slot-boundp schema 'version)
      (write-char #\Space stream)
      (write-string (schema-version schema) stream))))

(defclass operation ()
  ;; TODO parameters
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
   (deprecatedp
    :type boolean
    :initform nil
    :accessor operation-deprecatedp)))

(defmethod print-object ((operation operation) stream)
  (print-unreadable-object (operation stream :type t)
    (prin1 (operation-id operation) stream)))

(defun parse-schema (string)
  (declare (type string string))
  (build-schema (json:parse string :mapping 'openapi)))

(defun build-schema (schema-value)
  (let ((schema (make-instance 'schema)))
    (dolist (member schema-value)
      (case (car member)
        (openapi
         (setf (schema-openapi-version schema) (cdr member)))
        (info
         (build-schema/info (cdr member) schema))
        (servers
         (build-schema/servers (cdr member) schema))
        (paths
         (build-schema/paths (cdr member) schema-value schema))))
    schema))

(defun build-schema/info (value schema)
  (declare (type list value)
           (type schema schema))
  (dolist (member value)
    (case (car member)
      (title
       (setf (schema-title schema) (cdr member)))
      (version
       (setf (schema-version schema) (cdr member))))))

(defun build-schema/servers (value schema)
  (declare (type vector value)
           (type schema schema))
  (dotimes (i (length value))
    (let ((value (aref value i)))
      (when (assoc 'variables value)
        (unsupported-elements "server variables"))
      (push (cdr (assoc 'url value)) (schema-servers schema)))))

(defun build-schema/paths (paths-value schema-value schema)
  (declare (type list paths-value schema-value)
           (type schema schema))
  (dolist (path-member paths-value)
    (let ((path-pattern (car path-member))
          (path-value (cdr path-member)))
      (dolist (member path-value)
        (case (car member)
          ((get put post delete options head patch trace)
           (let ((method (intern (symbol-name (car member)) :keyword)))
             (build-schema/operation method path-pattern
                                     (cdr member) path-value schema-value
                                     schema))))))))

(defun build-schema/operation (method path-pattern
                               operation-value path-value schema-value
                               schema)
  (declare (type keyword method)
           (type list operation-value path-value schema-value)
           (type schema schema)
           (ignore path-value schema-value))
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
         (setf (operation-deprecatedp operation) (cdr member)))))
    (setf (gethash (operation-id operation) (schema-operations schema))
          operation)))
