(in-package :openssl)

(defun pbkdf2 (password salt digest-algorithm nb-iterations key-length)
  (declare (type core:octet-vector password salt)
           (type symbol digest-algorithm)
           (type (integer 1) nb-iterations))
  (let* ((digest-name (digest-algorithm-name digest-algorithm))
         (%kdf (evp-kdf-fetch (ffi:null-pointer) "PBKDF2" nil))
         (%context (unwind-protect (evp-kdf-ctx-new %kdf)
                     ;; The context keeps a reference to the key derivation
                     ;; function object, we can (and must) free it no matter
                     ;; what.
                     (evp-kdf-free %kdf))))
    (core:abort-protect
        (ffi:with-pinned-vector-data (%password password)
          (ffi:with-pinned-vector-data (%salt salt)
            (ffi:with-foreign-strings ((%digest digest-name)
                                       (%pass-key "pass")
                                       (%salt-key "salt")
                                       (%iter-key "iter")
                                       (%digest-key "digest"))
              (ffi:with-foreign-values ((%nb-iterations :uint64)
                                        (%parameters 'ossl-param :count 5))
                (setf (ffi:foreign-value %nb-iterations :uint64) nb-iterations)
                (macrolet ((%parameter (i)
                             `(ffi:pointer+
                               %parameters
                               ,(* i (ffi:foreign-type-size 'ossl-param)))))
                  (initialize-parameter (%parameter 0) %digest-key
                                        :ossl-param-utf8-string
                                        %digest 0)
                  (initialize-parameter (%parameter 1) %pass-key
                                        :ossl-param-octet-string
                                        %password (length password))
                  (initialize-parameter (%parameter 2) %salt-key
                                        :ossl-param-octet-string
                                        %salt (length salt))
                  (initialize-parameter (%parameter 3) %iter-key
                                        :ossl-param-unsigned-integer
                                        %nb-iterations
                                        (ffi:foreign-type-size :uint64))
                  (initialize-last-parameter (%parameter 4)))
                (evp-kdf-derive %context key-length %parameters)))))
      (evp-kdf-ctx-free %context))))
