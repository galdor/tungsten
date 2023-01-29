(in-package :openssl)

;;; It would be nice to provide an incremental API the same way as the one for
;;; digests. This is possible through the EVP_MAC OpenSSL API. However it is
;;; very complex due to the insanity of OSSP_PARAM structures.
;;;
;;; Therefore we only provide a simple API based on the HMAC() function, at
;;; least for the time being.
;;;
;;; Note that we may one day have to provide a higher level API supporting all
;;; MACs (e.g. CMACs).

(defun compute-hmac (key data digest-algorithm)
  (declare (type core:octet-vector key data)
           (type symbol digest-algorithm))
  (let* ((digest-name (digest-algorithm-name digest-algorithm))
         (%digest (evp-get-digest-by-name digest-name)))
    (hmac %digest key data)))

