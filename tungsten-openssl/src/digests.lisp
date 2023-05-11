(in-package :openssl)

;;; In this file, we use %DIGEST to refer to the digest context. This is not
;;; obvious because the FFI layer (ffi.lisp) clearly separate %DIGEST (the
;;; digest algorithm object) and %CONTEXT (the digest context). However we do
;;; not expect the user to understand the difference, and they should only
;;; have to manipulate digests.

(define-condition unsupported-digest-algorithm (error)
  ((algorith
    :type symbol
    :initarg :algorithm
    :reader unsupported-digest-algorithm-algorithm))
  (:report
   (lambda (condition stream)
     (with-slots (algorithm) condition
       (format stream "Unsupported digest algorithm ~S." algorithm)))))

(defun digest-algorithm-name (algorithm)
  (cond
    ((member algorithm '(:md5
                         :sha1
                         :sha224 :sha256 :sha384 :sha512
                         :sha3-224 :sha3-256 :sha3-384 :sha3-512))
     (string-downcase (symbol-name algorithm)))
    (t
     (error 'unsupported-digest-algorithm :name algorithm))))

(defun make-digest (algorithm)
  (declare (type symbol algorithm))
  (let ((%context (evp-md-ctx-new)))
    (core:abort-protect
        (let ((%digest (evp-get-digest-by-name
                        (digest-algorithm-name algorithm))))
          (when (ffi:null-pointer-p %digest)
            (error 'unsupported-digest-algorithm :name algorithm))
          (evp-digest-init-ex2 %context %digest (ffi:null-pointer))
          %context)
      (evp-md-ctx-free %context))))

(defun update-digest (%digest data)
  (declare (type ffi:pointer %digest)
           (type (or string core:octet-vector) data))
  (evp-digest-update %digest (etypecase data
                               (string (text:encode-string data))
                               (core:octet-vector data))))

(defmacro with-digest ((%digest algorithm) &body body)
  `(let ((,%digest (make-digest ,algorithm)))
     (unwind-protect
          (progn
            ,@body
            (evp-digest-final-ex %digest))
       (evp-md-ctx-free %digest))))

(defun compute-digest (data algorithm)
  (declare (type (or string core:octet-vector) data)
           (type symbol algorithm))
  (with-digest (%digest algorithm)
    (update-digest %digest data)))

(defun compute-hex-digest (data algorithm)
  (declare (type (or string core:octet-vector) data)
           (type symbol algorithm))
  (text:encode-hex-string (compute-digest data algorithm)))
