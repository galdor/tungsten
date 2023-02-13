(in-package :openapi)

(defclass schema ()
  ((openapi-version
    :type string
    :initarg :openapi-version
    :initform "3.1.0"
    :accessor schema-openapi-version)
   (title
    :type string
    :initarg :title
    :accessor schema-title)
   (version
    :type string
    :initarg :version
    :accessor schema-version)))

(defmethod print-object ((schema schema) stream)
  (print-unreadable-object (schema stream :type t)
    (when (slot-boundp schema 'title)
      (prin1 (schema-title schema) stream))
    (when (slot-boundp schema 'version)
      (write-char #\Space stream)
      (write-string (schema-version schema) stream))))

(defun parse-schema (string)
  (declare (type string string))
  (parse-schema-value (json:parse string :mapping 'openapi)))

(defun parse-schema-value (value)
  (let ((schema (make-instance 'schema)))
    (dolist (member value)
      (case (car member)
        (openapi
         (setf (schema-openapi-version schema) (cdr member)))
        (info
         (parse-schema-value/info (cdr member) schema))))
    schema))

(defun parse-schema-value/info (value schema)
  (declare (type schema schema))
  (dolist (member value)
    (case (car member)
      (title
       (setf (schema-title schema) (cdr member)))
      (version
       (setf (schema-version schema) (cdr member))))))
