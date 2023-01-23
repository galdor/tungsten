(in-package :openssl)

(defun initialize-parameter (%parameter %key type %value value-length)
  (declare (type ffi:pointer %parameter %key %value)
           (type keyword type)
           (type (integer 0) value-length))
  (ffi:with-struct-members (((key :key)
                             (data-type :data-type)
                             (data :data)
                             (data-size :data-size)
                             (return-size :return-size))
                            %parameter 'ossl-param)
    (setf key %key)
    (setf data-type type)
    (setf data %value)
    (setf data-size value-length)
    ;; We should extract the value of OSSL_PARAM_UNMODIFIED but it is of type
    ;; size_t and the extractor only support base types. In practice the value
    ;; is (size_t)-1.
    (let ((type-size (ffi:foreign-type-size 'system:size-t)))
      (setf return-size (ldb (byte (* type-size 8) 0) -1)))))

(defun initialize-last-parameter (%parameter)
  (declare (type ffi:pointer %parameter))
  (ffi:clear-foreign-memory %parameter (ffi:foreign-type-size 'ossl-param)))
