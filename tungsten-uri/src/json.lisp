(in-package :uri)

(json:register-mapping-class 'uri 'uri-json-mapping)

(defclass uri-json-mapping (json:mapping)
  ((scheme
    :type (or string list)
    :initarg :scheme
    :initform nil))
  (:default-initargs
   :base-types '(:string)))

(defmethod json:validate-value (value (mapping uri-json-mapping))
  (handler-case
      (with-slots (scheme) mapping
        (let ((uri (parse value)))
          (when scheme
            (let ((scheme-valid-p
                    (if (listp scheme)
                        (member (uri-scheme uri) scheme :test #'string=)
                        (string= (uri-scheme uri) scheme))))
              (unless scheme-valid-p
                (if (listp scheme)
                    (json:add-mapping-error value "URI scheme must be one of ~
                                                   the following: ~{~S~^, ~}"
                                             scheme)
                    (json:add-mapping-error value "URI scheme must be ~S"
                                            scheme)))))
          uri))
    (uri-parse-error (condition)
      (json:add-mapping-error value "string is not a valid URI: ~A"
                              condition)
      value)))

(defmethod json:generate-value (value (mapping uri-json-mapping))
  (declare (ignore mapping))
  (uri:serialize value))