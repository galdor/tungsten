(in-package :tls)

;;;
;;; Library
;;;

(ffi:use-foreign-library 'ssl "libssl.so.3")

(defun library-version ()
  "Return a string containing the version number of the OpenSSL library."
  (let ((major (ffi:foreign-funcall
                "OPENSSL_version_major" (() :unsigned-int)))
        (minor (ffi:foreign-funcall
                "OPENSSL_version_minor" (() :unsigned-int)))
        (patch (ffi:foreign-funcall
                "OPENSSL_version_patch" (() :unsigned-int)))
        (pre-release (ffi:decode-foreign-string
                      (ffi:foreign-funcall
                       "OPENSSL_version_pre_release" (() :pointer)))))
    (format nil "~D.~D.~D~A" major minor patch pre-release)))
