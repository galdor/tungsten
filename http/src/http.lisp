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

(defun header-field-name-string (name)
  (declare (type header-field-name name))
  (etypecase name
    (symbol
     (symbol-name name)
     (string
      name))))

(defun header-field (header name)
  (declare (type header header)
           (type header-field-name name))
  (let ((name-string (header-field-name-string name)))
    (cdr (assoc name-string header :key 'header-field-name-string
                                   :test #'equalp))))

(defun (setf header-field) (value header name)
  (declare (type header header)
           (type header-field-name name))
  (let* ((name-string (header-field-name-string name))
         (pair (assoc name-string header :key 'header-field-name-string
                                         :test #'equalp)))
    (when pair
      (rplacd pair value)))
  value)
