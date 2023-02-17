(in-package :mime)

(json:register-mapping-class 'media-type 'media-type-json-mapping)
(json:register-mapping-class 'media-range 'media-range-json-mapping)

(defclass media-type-json-mapping (json:mapping)
  ()
  (:default-initargs
   :base-types '(:string)))

(defmethod json:validate-value (value (mapping media-type-json-mapping))
  (handler-case
      (parse-media-type value)
    (invalid-media-type (condition)
      (json:add-mapping-error value "string is not a valid MIME media type: ~A"
                              (invalid-media-type-description condition))
      value)))

(defmethod json:generate-value (value (mapping media-type-json-mapping))
  (declare (ignore mapping))
  (serialize-media-type value))

(defclass media-range-json-mapping (json:mapping)
  ()
  (:default-initargs
   :base-types '(:string)))

(defmethod json:validate-value (value (mapping media-range-json-mapping))
  (handler-case
      (parse-media-range value)
    (invalid-media-range (condition)
      (json:add-mapping-error value
                              "string is not a valid MIME media range: ~A"
                              (invalid-media-range-description condition))
      value)))

(defmethod json:generate-value (value (mapping media-range-json-mapping))
  (declare (ignore mapping))
  (serialize-media-range value))
