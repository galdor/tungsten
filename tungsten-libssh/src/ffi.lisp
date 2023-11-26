(in-package :libssh)

;;;
;;; Library
;;;

(ffi:use-foreign-library 'ssh "libssh.so.4")

(defun library-version ()
  "Return a string containing the version number of the libssh library."
  ;; The version string is formatted as:
  ;;
  ;; version-string = version { "/" feature }
  (let* ((string (ffi:decode-foreign-string
                  (ffi:foreign-funcall "ssh_version" ((:int) :pointer) 0)))
         (version-end (or (position #\/ string) (length string))))
    (subseq string 0 version-end)))

;;;
;;; Errors
;;;

(define-condition libssh-error (error)
  ((function
    :type string
    :initarg :function
    :reader libssh-error-function)
   (code
    :type (or keyword integer null)
    :initarg :code
    :initform nil
    :reader libssh-error-code)
   (description
    :type string
    :initarg :description
    :initform nil
    :reader libssh-error-description))
  (:report
   (lambda (condition stream)
     (format stream "libssh function ~S failed~@[ with error ~A~]: ~A"
             (libssh-error-function condition)
             (libssh-error-code condition)
             (libssh-error-description condition)))))

(defun libssh-error (function-name error-source)
  (declare (type string function-name)
           (type (or ffi:pointer null) error-source))
  (let (code description)
    (when error-source
      (setf code
            (ffi:foreign-funcall "ssh_get_error_code"
                                 ((:pointer) :int) error-source))
      (setf description
            (ffi:decode-foreign-string
             (ffi:foreign-funcall "ssh_get_error"
                                  ((:pointer) :pointer) error-source))))
    (error 'libssh-error :function function-name
                         :code code
                         :description (or description "unknown error"))))

(defmacro libssh-funcall ((function-name signature &rest args)
                          &key error-source)
  (let ((result (gensym "RESULT-"))
        (result-type (second signature)))
    `(let ((,result (ffi:foreign-funcall ,function-name ,signature ,@args)))
       (when ,(case result-type
                (:pointer
                 `(ffi:null-pointer-p ,result))
                (:int
                 `(< ,result 0))
                (ssh-error
                 `(not (eql ,result :ssh-ok)))
                (ssh-known-hosts-status
                 `(eql ,result :ssh-known-hosts-error))
                (ssh-auth-status
                 `(eql ,result :ssh-auth-error)))
         (libssh-error ,function-name ,error-source))
       ,result)))

;;;
;;; Sessions
;;;

(defun ssh-new ()
  (libssh-funcall ("ssh_new" (() :pointer))))

(defun ssh-free (%session)
  (libssh-funcall ("ssh_free" ((:pointer) :void) %session)))

(defun ssh-connect (%session)
  (libssh-funcall ("ssh_connect" ((:pointer) ssh-error) %session)
                  :error-source %session))

(defun ssh-disconnect (%session)
  (libssh-funcall ("ssh_disconnect" ((:pointer) :void) %session)
                  :error-source %session))

(defun ssh-options-set (%session option %value)
  (declare (type ffi:pointer %session %value)
           (type keyword option))
  (libssh-funcall ("ssh_options_set" ((:pointer ssh-option :pointer) :int)
                                     %session option %value)
                  :error-source %session))

(defun ssh-options-set/string (%session option value)
  (declare (type ffi:pointer %session)
           (type keyword option)
           (type string value))
  (ffi:with-foreign-string (%value value)
    (ssh-options-set %session option %value)))

(defun ssh-options-set/int (%session option value)
  (declare (type ffi:pointer %session)
           (type keyword option)
           (type integer value))
  (ffi:with-foreign-value (%value :int)
    (setf (ffi:foreign-value %value :int) value)
    (ssh-options-set %session option %value)))

(defun ssh-options-set/unsigned-int (%session option value)
  (declare (type ffi:pointer %session)
           (type keyword option)
           (type (integer 0) value))
  (ffi:with-foreign-value (%value :unsigned-int)
    (setf (ffi:foreign-value %value :unsigned-int) value)
    (ssh-options-set %session option %value)))

(defun ssh-get-server-publickey (%session)
  (declare (type ffi:pointer %session))
  (ffi:with-foreign-value (%key :pointer)
    (libssh-funcall ("ssh_get_server_publickey" ((:pointer :pointer) :int)
                                                %session %key)
                    :error-source %session)
    (ffi:foreign-value %key :pointer)))

(defun ssh-session-is-known-server (%session)
  (declare (type ffi:pointer %session))
  (libssh-funcall ("ssh_session_is_known_server"
                   ((:pointer) ssh-known-hosts-status) %session)
                  :error-source %session))

(defun ssh-session-update-known-hosts (%session)
  (declare (type ffi:pointer %session))
  (libssh-funcall ("ssh_session_update_known_hosts" ((:pointer) :int) %session)
                  :error-source %session))

(defun ssh-userauth-none (%session)
  (libssh-funcall ("ssh_userauth_none" ((:pointer :pointer) :int)
                                       %session (ffi:null-pointer))
                  :error-source %session))

(defun ssh-userauth-list (%session)
  (libssh-funcall ("ssh_userauth_list" ((:pointer :pointer) ssh-auth-method)
                                       %session (ffi:null-pointer))
                  :error-source %session))

(defun ssh-userauth-publickey-auto (%session password)
  (ffi:with-foreign-string (%password password)
    (libssh-funcall ("ssh_userauth_publickey_auto"
                     ((:pointer :pointer :pointer) ssh-auth-status)
                     %session (ffi:null-pointer) %password)
                    :error-source %session)))

(defun ssh-userauth-try-publickey (%session %key)
  (declare (type ffi:pointer %session %key))
  (libssh-funcall ("ssh_userauth_try_publickey"
                   ((:pointer :pointer :pointer) ssh-auth-status)
                   %session (ffi:null-pointer) %key)
                  :error-source %session))

(defun ssh-userauth-publickey (%session %key)
  (declare (type ffi:pointer %session %key))
  (libssh-funcall ("ssh_userauth_publickey"
                   ((:pointer :pointer :pointer) ssh-auth-status)
                   %session (ffi:null-pointer) %key)
                  :error-source %session))

;;;
;;; Channels
;;;

(defun ssh-channel-new (%session)
  (declare (type ffi:pointer %session))
  (libssh-funcall ("ssh_channel_new" ((:pointer) :pointer) %session)))

(defun ssh-channel-free (%channel)
  (declare (type ffi:pointer %channel))
  (libssh-funcall ("ssh_channel_free" ((:pointer) :void) %channel)))

(defun ssh-channel-open-session (%channel %session)
  (declare (type ffi:pointer %channel %session))
  (libssh-funcall ("ssh_channel_open_session"
                   ((:pointer) ssh-error) %channel)
                  :error-source %session))

(defun ssh-channel-send-eof (%channel %session)
  (declare (type ffi:pointer %channel %session))
  (libssh-funcall ("ssh_channel_send_eof" ((:pointer) ssh-error) %channel)
                  :error-source %session))

(defun ssh-channel-is-eof (%channel %session)
  (declare (type ffi:pointer %channel %session))
  (libssh-funcall ("ssh_channel_is_eof" ((:pointer) ssh-error) %channel)
                  :error-source %session))

(defun ssh-channel-close (%channel %session)
  (declare (type ffi:pointer %channel %session))
  (libssh-funcall ("ssh_channel_close" ((:pointer) ssh-error) %channel)
                  :error-source %session))

(defun ssh-channel-request-exec (%channel %session command)
  (declare (type ffi:pointer %channel %session)
           (type string command))
  (ffi:with-foreign-string (%command command)
    (libssh-funcall ("ssh_channel_request_exec"
                     ((:pointer :pointer) ssh-error) %channel %command)
                    :error-source %session)))

(defun ssh-channel-read (%channel %session %buffer size stderr)
  (declare (type ffi:pointer %channel %session %buffer)
           (type integer size)
           (type boolean stderr))
  (let ((value (libssh-funcall ("ssh_channel_read"
                                ((:pointer :pointer :uint32 :int) :int)
                                %channel %buffer size (if stderr 1 0)))))
    (unless (>= value 0)
      (libssh-error "ssh_channel_read" %session))
    value))

(defun ssh-channel-get-exit-status (%channel %session)
  (declare (type ffi:pointer %channel %session))
  (let ((value (libssh-funcall ("ssh_channel_get_exit_status"
                                ((:pointer) :int) %channel)
                               :error-source %session)))
    (unless (>= value 0)
      (libssh-error "ssh_channel_get_exit_status" %session))
    value))

;;;
;;; Keys
;;;

(defun ssh-key-free (%key)
  (declare (type ffi:pointer %key))
  (libssh-funcall ("ssh_key_free" ((:pointer) :void) %key)))

(defun ssh-get-publickey-hash (%key hash-type)
  (declare (type ffi:pointer %key)
           (type keyword hash-type))
  (ffi:with-foreign-values ((%hash :pointer)
                            (%hash-size 'system:size-t))
    (libssh-funcall ("ssh_get_publickey_hash"
                     ((:pointer ssh-publickey-hash :pointer :pointer)
                      ssh-error)
                     %key hash-type %hash %hash-size))
    (unwind-protect
         (let ((hash-size (ffi:foreign-value %hash-size 'system:size-t)))
           (ffi:read-foreign-memory (ffi:foreign-value %hash :pointer)
                                    hash-size))
      (libssh-funcall ("ssh_clean_pubkey_hash" ((:pointer) :void) %hash)))))

(defun ssh-pki-import-privkey-base64 (data passphrase)
  (declare (type string data)
           (type (or string null) passphrase))
  (ffi:with-foreign-value (%key :pointer)
    (ffi:with-foreign-strings ((%data data)
                               (%passphrase passphrase))
      (libssh-funcall ("ssh_pki_import_privkey_base64"
                       ((:pointer :pointer :pointer :pointer :pointer)
                        ssh-error)
                       %data %passphrase (ffi:null-pointer) (ffi:null-pointer)
                       %key)))
    (ffi:foreign-value %key :pointer)))

(defun ssh-pki-export-privkey-to-pubkey (%private-key)
  (declare (type ffi:pointer %private-key))
  (ffi:with-foreign-value (%public-key :pointer)
    (libssh-funcall
     ("ssh_pki_export_privkey_to_pubkey" ((:pointer :pointer) ssh-error)
                                         %private-key %public-key))
    (ffi:foreign-value %public-key :pointer)))
