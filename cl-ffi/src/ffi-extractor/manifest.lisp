(in-package :ffi-extractor)

;;; A manifest lists the C elements we want to extract. A manifest file
;;; is represented as a list of forms.
;;;
;;; For example:
;;;
;;; ((include "curl.h")
;;;
;;;  (type 'curl-socket "curl_socket_t")
;;;
;;;  (enum 'curl-code "CURLcode"
;;;        ((:ok "CURLE_OK")
;;;         (:failed-init "CURLE_FAILED_INIT")))
;;;
;;;  (struct 'curl-sockaddr "struct curl_sockaddr"
;;;          ((:family :int "family")
;;;           (:socktype :int "socktype")
;;;           (:protocol :int "protocol")
;;;           (:addrlen :unsigned-int "addrlen"))))

(defun load-manifest (path)
  "Load and return a manifest from a file at PATH."
  (with-open-file (file path)
    (let ((*read-eval* nil)
          (*package* (find-package :ffi-extractor)))
      (do ((forms nil))
          ((eq (car forms) 'eof)
           (nreverse (cdr forms)))
        (push (read file nil 'eof) forms)))))

(defun manifest-header-files (manifest)
  "Return the list of header files listed in the manifest."
  (let ((files nil))
    (dolist (form manifest (nreverse files))
      (when (eq (car form) 'include)
        (push (second form) files)))))
