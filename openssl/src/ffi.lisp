(in-package :openssl)

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

;;;
;;; Errors
;;;

(defclass openssl-error ()
  ((value
    :type integer
    :initarg :value
    :reader openssl-error-value)
   (reason
    :type (or keyword integer)
    :initarg :reason
    :reader openssl-error-reason)
   (description
    :type string
    :initarg :description
    :reader openssl-error-description)
   (file
    :type (or string null)
    :initarg :file
    :initform nil
    :reader openssl-error-file)
   (line
    :type (or integer null)
    :initarg :line
    :initform nil
    :reader openssl-error-line)
   (function
    :type (or string null)
    :initarg :function
    :initform nil
    :reader openssl-error-function)
   (data
    :type (or string null)
    :initarg :data
    :initform nil
    :reader openssl-error-data)
   (flags
    :type (or integer null)
    :initarg :flags
    :initform nil
    :reader openssl-error-flags)))

(define-condition openssl-error-stack (error)
  ((function
    :type string
    :initarg :function
    :reader openssl-error-stack-function)
   (errors
    :type list
    :initarg :errors
    :reader openssl-error-stack-errors))
  (:report
   (lambda (c stream)
     (with-slots (function errors) c
       (case (length errors)
         (0
          (format stream "OpenSSL function ~S failed." function))
         (1
          (with-slots (value description) (car errors)
            (format stream "OpenSSL function ~S failed with error ~D: ~A."
                    function value description)))
         (t
          (format stream "OpenSSL function ~S failed with multiple errors:~%"
                  function)
          (dolist (error errors)
            (with-slots (value description) error
              (format stream "- Error ~D: ~A~%" value description)))))))))

(defun err-error-string (value &aux (buffer-size 1024))
  (ffi:with-foreign-value (%buffer :char :count buffer-size)
    (ffi:foreign-funcall "ERR_error_string_n"
                         ((:unsigned-int :pointer system:size-t) :void)
                         value %buffer buffer-size)
    (ffi:decode-foreign-string %buffer)))

(defun err-get-reason (value)
  (cond
    ((> (logand value err-system-flag) 0)
     (logand value err-system-mask))
    (t
     (logand value err-reason-mask))))

(defun err-get-error ()
  (ffi:with-foreign-values ((%file :pointer)
                            (%line :int)
                            (%function :pointer)
                            (%data :pointer)
                            (%flags :int))
    (setf (ffi:foreign-value %file :pointer) (ffi:null-pointer)
          (ffi:foreign-value %line :int) 0
          (ffi:foreign-value %function :pointer) (ffi:null-pointer)
          (ffi:foreign-value %data :pointer) (ffi:null-pointer)
          (ffi:foreign-value %flags :int) 0)
    (let ((value (ffi:foreign-funcall "ERR_get_error_all"
                                      ((:pointer :pointer :pointer :pointer
                                        :pointer)
                                       :unsigned-int)
                                      %file %line %function %data %flags)))
      (unless (zerop value)
        (let ((reason
                (ffi:decode-foreign-value (err-get-reason value) 'ssl-reason))
              (description (err-error-string value))
              (file
                (let ((%pointer (ffi:foreign-value %file :pointer)))
                  (unless (ffi:null-pointer-p %pointer)
                    (ffi:decode-foreign-string %pointer))))
              (line (ffi:foreign-value %line :int))
              (function
                (let ((%pointer (ffi:foreign-value %function :pointer)))
                  (unless (ffi:null-pointer-p %pointer)
                    (ffi:decode-foreign-string %pointer))))
              (data
                (let ((%pointer (ffi:foreign-value %data :pointer)))
                  (unless (ffi:null-pointer-p %pointer)
                    (ffi:decode-foreign-string %pointer))))
              (flags (ffi:foreign-value %flags :int)))
          (make-instance 'openssl-error
                         :value value
                         :reason reason
                         :description description
                         :file (unless (string= file "") file)
                         :line (unless (zerop line) line)
                         :function (unless (string= function "") function)
                         :data (unless (string= data "") data)
                         :flags (unless (zerop flags) flags)))))))

(defun err-get-errors ()
  (let ((errors nil))
    (loop
      (let ((error (err-get-error)))
        (unless error
          (return-from err-get-errors (nreverse errors)))
        (push error errors)))))

(defmacro openssl-funcall ((name signature &rest args)
                           &key (errorp
                                 (case (car (last signature))
                                   (:void nil)
                                   (:pointer ''ffi:null-pointer-p)
                                   (:int ''<=0))))
  (let ((errorp-var (gensym "ERRORP-VAR-"))
        (value (gensym "VALUE-")))
    `(progn
       (ffi:foreign-funcall "ERR_clear_error" (() :void))
       (let ((,errorp-var ,errorp)
             (,value (ffi:foreign-funcall ,name ,signature ,@args)))
         (when (and ,errorp-var (funcall ,errorp-var ,value))
           (error 'openssl-error-stack :function ,name :errors (err-get-errors)))
         ,value))))

(declaim (inline <=0))
(defun <=0 (n)
  (<= n 0))

;;;
;;; Utils
;;;

(defun crypto-memcmp (octets1 octets2)
  (assert (= (length octets1) (length octets2)))
  (ffi:with-pinned-vector-data (%octets1 octets1)
    (ffi:with-pinned-vector-data (%octets2 octets2)
      (openssl-funcall
       ("CRYPTO_memcmp" ((:pointer :pointer system:size-t) :int)
                        %octets1 %octets2 (length octets1))
       :errorp nil))))

;;;
;;; Contexts
;;;

(defun ssl-ctx-new (%method)
  (openssl-funcall ("SSL_CTX_new" ((:pointer) :pointer) %method)))

(defun ssl-ctx-free (%context)
  (openssl-funcall ("SSL_CTX_free" ((:pointer) :void) %context)))

(defun tls-client-method ()
  (openssl-funcall ("TLS_client_method" (() :pointer))))

(defun ssl-ctx-set-options (%context options)
  (openssl-funcall ("SSL_CTX_set_options" ((:pointer ctx-options) :uint64)
                                          %context options)))

(defun ssl-ctx-ctrl (%context command long %pointer)
  (openssl-funcall
   ("SSL_CTX_ctrl" ((:pointer ctrl-command :long :pointer) :long)
                   %context command long %pointer)))

(defun ssl-ctx-set-min-proto-version (%context version)
  (ssl-ctx-ctrl %context :ssl-ctrl-set-min-proto-version
                (ffi:encode-foreign-value version 'ssl-version)
                (ffi:null-pointer)))

(defun ssl-ctx-set-cipher-list (%context ciphers)
  (ffi:with-foreign-string (%ciphers (format nil "~{~^:~A~}" ciphers))
    (openssl-funcall ("SSL_CTX_set_cipher_list" ((:pointer :pointer) :int)
                                                %context %ciphers))))

(defun ssl-ctx-set-verify (%context mode callback)
  (let ((%callback (if callback
                       (ffi:callback-pointer callback)
                       (ffi:null-pointer))))
    (openssl-funcall
     ("SSL_CTX_set_verify" ((:pointer verification-mode :pointer) :void)
                           %context mode %callback))))

(defun ssl-ctx-set-verify-depth (%context depth)
  (openssl-funcall ("SSL_CTX_set_verify_depth" ((:pointer :int) :void)
                                               %context depth)))

(defun ssl-ctx-load-verify-dir (%context path)
  (ffi:with-foreign-string (%path (namestring path))
    (openssl-funcall ("SSL_CTX_load_verify_dir" ((:pointer :pointer) :int)
                                                %context %path))))

(defun ssl-ctx-load-verify-file (%context path)
  (ffi:with-foreign-string (%path (namestring path))
    (openssl-funcall ("SSL_CTX_load_verify_file" ((:pointer :pointer) :int)
                                                 %context %path))))

(defun ssl-ctx-use-certificate-file (%context path type)
  (ffi:with-foreign-string (%path (namestring path))
    (openssl-funcall
     ("SSL_CTX_use_certificate_file" ((:pointer :pointer filetype) :int)
                                     %context %path type))))

(defun ssl-ctx-use-private-key-file (%context path type)
  (ffi:with-foreign-string (%path (namestring path))
    (openssl-funcall
     ("SSL_CTX_use_PrivateKey_file" ((:pointer :pointer filetype) :int)
                                    %context %path type))))

;;;
;;; Connection data
;;;

(defun ssl-new (%context)
  (openssl-funcall ("SSL_new" ((:pointer) :pointer) %context)))

(defun ssl-free (%ssl)
  (openssl-funcall ("SSL_free" ((:pointer) :void) %ssl)))

(defun ssl-set-fd (%ssl fd)
  (openssl-funcall ("SSL_set_fd" ((:pointer :int) :pointer) %ssl fd)))

(defun ssl-connect (%ssl)
  (openssl-funcall ("SSL_connect" ((:pointer) :int) %ssl)))

(defun ssl-shutdown (%ssl)
  (openssl-funcall ("SSL_shutdown" ((:pointer) :int) %ssl)))

(defun ssl-read (%ssl %data size)
  (openssl-funcall ("SSL_read" ((:pointer :pointer :int) :int)
                                %ssl %data size)))

(defun ssl-write (%ssl %data size)
  (openssl-funcall ("SSL_write" ((:pointer :pointer :int) :int)
                                %ssl %data size)))

;;;
;;; Random
;;;

(defun rand-bytes (n)
  (let ((octets (core:make-octet-vector n)))
    (ffi:with-pinned-vector-data (%octets octets)
      (openssl-funcall ("RAND_bytes" ((:pointer :int) :int) %octets n)))
    octets))

;;;
;;; Digests
;;;

(defun evp-get-digest-by-name (name)
  (ffi:with-foreign-string (%name name)
    (openssl-funcall ("EVP_get_digestbyname" ((:pointer) :pointer) %name))))

(defun evp-md-ctx-new ()
  (openssl-funcall ("EVP_MD_CTX_new" (() :pointer))))

(defun evp-md-ctx-free (%context)
  (openssl-funcall ("EVP_MD_CTX_free" ((:pointer) :void) %context)))

(defun evp-md-ctx-get0-md (%context)
  (openssl-funcall ("EVP_MD_CTX_get0_md" ((:pointer) :pointer) %context)))

(defun evp-md-get-size (%digest)
  (openssl-funcall ("EVP_MD_get_size" ((:pointer) :int) %digest)))

(defun evp-digest-init-ex2 (%context %digest %parameters)
  (openssl-funcall ("EVP_DigestInit_ex2" ((:pointer :pointer :pointer) :int)
                                         %context %digest %parameters)))

(defun evp-digest-update (%context octets)
  (ffi:with-pinned-vector-data (%octets octets)
    (openssl-funcall
     ("EVP_DigestUpdate" ((:pointer :pointer system:size-t) :int)
                         %context %octets (length octets)))))

(defun evp-digest-final-ex (%context)
  (let* ((size (evp-md-get-size (evp-md-ctx-get0-md %context)))
         (octets (core:make-octet-vector size)))
    (ffi:with-pinned-vector-data (%octets octets)
      (ffi:with-foreign-value (%size :unsigned-int)
        (setf (ffi:foreign-value %size :unsigned-int) size)
        (openssl-funcall
         ("EVP_DigestFinal_ex" ((:pointer :pointer :pointer) :int)
                               %context %octets %size))))
    octets))

;;;
;;; HMACs
;;;

(defun hmac (%digest key data)
  (let* ((mac-length (evp-md-get-size %digest))
         (mac (core:make-octet-vector mac-length)))
    (ffi:with-pinned-vector-data (%mac mac)
      (ffi:with-pinned-vector-data (%key key)
        (ffi:with-pinned-vector-data (%data data)
          (ffi:with-foreign-value (%mac-length :unsigned-int)
            (setf (ffi:foreign-value %mac-length :unsigned-int) mac-length)
            (openssl-funcall
             ("HMAC" ((:pointer :pointer :int :pointer system:size-t
                                :pointer :pointer)
                      :pointer)
                     %digest %key (length key) %data (length data)
                     %mac %mac-length)))))
      mac)))

;;;
;;; Key derivation
;;;

(defun evp-kdf-ctx-new (%kdf)
  (openssl-funcall ("EVP_KDF_CTX_new" ((:pointer) :pointer) %kdf)))

(defun evp-kdf-ctx-free (%context)
  (openssl-funcall ("EVP_KDF_CTX_free" ((:pointer) :void) %context)))

(defun evp-kdf-derive (%context key-length %parameters)
  (let ((key (core:make-octet-vector key-length)))
    (ffi:with-pinned-vector-data (%key key)
      (openssl-funcall
       ("EVP_KDF_derive" ((:pointer :pointer system:size-t :pointer) :int)
                         %context %key key-length %parameters))
      key)))

(defun evp-kdf-fetch (%library-context name properties)
  (ffi:with-foreign-strings ((%name name)
                             (%properties properties))
    (openssl-funcall
     ("EVP_KDF_fetch" ((:pointer :pointer :pointer) :pointer)
                      %library-context %name %properties))))

(defun evp-kdf-free (%kdf)
  (openssl-funcall ("EVP_KDF_free" ((:pointer) :void) %kdf)))
