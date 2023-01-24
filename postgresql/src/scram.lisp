(in-package :postgresql)

(define-condition scram-error (simple-error)
  ()
  (:report
   (lambda (condition stream)
     (with-slots (format-control format-arguments) condition
       (format stream "SCRAM error: ~?." format-control format-arguments)))))

(defun scram-error (format &rest arguments)
  (error 'scram-error :format-control format :format-arguments arguments))

(defun parse-scram-attributes (string &key (start 0) (end (length string)))
  (declare (type string string))
  ;; RFC 5802 7. Formal Syntax
  (do ((attributes nil)
       (i start))
      ((>= i end)
       (nreverse attributes))
    (let ((equal (position #\= string :start i :end end)))
      (unless equal
        (scram-error "truncated attribute"))
      (when (= equal i)
        (scram-error "empty attribute name"))
      (unless (= equal (1+ i))
        (scram-error "attribute name ~S is not a one character string"
                     (subseq string i equal)))
      (let* ((comma (position #\, string :start (1+ equal) :end end))
             (value-end (or comma end))
             (name (char string i))
             (value (subseq string (1+ equal) value-end))
             (attribute
               (case name
                 (#\i
                  (handler-case
                      (let ((count (parse-integer value )))
                        (unless (> count 0)
                          (error "invalid negative value"))
                        (cons :iteration-count count))
                    (error ()
                      (scram-error "invalid iteration count ~S" value))))
                 (#\r
                  (cons :nonce value))
                 (#\s
                  (cons :salt (text:decode-base64 value)))
                 (#\v
                  (cons :verifier value))
                 (t
                  (cons name value)))))
        (push attribute attributes)
        (setf i (1+ value-end))))))

(defun scram-client-first-message (username nonce)
  (declare (type (or string null) username)
           (type string nonce))
  ;; RFC 5802 7. Formal Syntax
  ;;
  ;; client-first-message-bare ::= [reserved-mext ","]
  ;;                               username "," nonce ["," extensions]
  ;; client-first-message ::= gs2-header client-first-message-bare
  ;;
  ;; Notes:
  ;;
  ;; - gs2-header is "n,," because we not use channel binding.
  ;;
  ;; - The username can be empty; this is the case with PostgreSQL which uses
  ;;   the username provided in initial connection parameters.
  (with-output-to-string (stream)
    (write-string "n,,n=" stream)
    (when username
      (scram-encode-sasl-name username stream))
    (write-string ",r=" stream)
    (write-string nonce stream)))

(defun scram-client-final-message (client-first-message server-first-message
                                   password client-nonce)
  (declare (type string client-first-message server-first-message)
           (type string password client-nonce))
  (let (server-nonce salt nb-iterations)
    (dolist (attribute (parse-scram-attributes server-first-message))
      (case (car attribute)
        (:nonce
         (setf server-nonce (cdr attribute)))
        (:salt
         (setf salt (cdr attribute)))
        (:iteration-count
         (setf nb-iterations (cdr attribute)))))
    (unless server-nonce
      (scram-error "missing nonce in server-first-message data"))
    (unless salt
      (scram-error "missing salt in server-first-message data"))
    (unless nb-iterations
      (scram-error "missing iteration count in server-first-message data"))
    (unless (and (> (length server-nonce) (length client-nonce))
                 (string= server-nonce client-nonce
                          :end1 (length client-nonce)))
      (scram-error "the server nonce does not start with the client nonce"))
    ;; RFC 5802 7. Formal Syntax

    ;; client-final-message-without-proof ::= channel-binding "," nonce [","
    ;;                                        extensions]
    ;; client-final-message ::= client-final-message-without-proof "," proof
    ;;
    ;; Notes:
    ;;
    ;; - channel-binding is just the base64-encoded GS2 header we sent in the
    ;;   first message (always "n,," since we do not use channel binding).
    ;;
    ;; - nonce is the server nonce.
    (let* ((client-final-message-without-proof
             (format nil "c=~A,r=~A"
                     (text:encode-base64 (text:encode-string "n,,"))
                     server-nonce))
           (salted-password
             (compute-scram-salted-password password salt nb-iterations))
           (auth-message
             (scram-auth-message client-first-message
                                 server-first-message
                                 client-final-message-without-proof))
           (proof (compute-scram-proof auth-message salted-password))
           (client-final-message (format nil "~A,p=~A"
                                         client-final-message-without-proof
                                         (text:encode-base64 proof))))
      (values client-final-message salted-password auth-message))))

(defun scram-auth-message (client-first-message
                           server-first-message
                           client-final-message-without-proof)
  (declare (type string
                 client-first-message server-first-message
                 client-final-message-without-proof))
  ;; RFC 5802 3. SCRAM Algorithm Overview
  ;;
  ;; AuthMessage ::= client-first-message-bare + "," +
  ;;                 server-first-message + "," +
  ;;                 client-final-message-without-proof
  (streams:with-output-to-octet-vector (stream :external-format :ascii)
    ;; client-first-message-bare is client-first-message without the GS2
    ;; header.
    (assert (and (> (length client-first-message) (length "n,,"))
                 (string= client-first-message "n,," :end1 3)))
    (write-string client-first-message stream :start 3)
    (write-char #\, stream)
    (write-string server-first-message stream)
    (write-char #\, stream)
    (write-string client-final-message-without-proof stream)))

(defun compute-scram-proof (auth-message salted-password)
  (declare (type core:octet-vector auth-message salted-password))
  ;; RFC 5802 3. SCRAM Algorithm Overview
  (let* ((client-key (openssl:compute-hmac salted-password
                                           (text:encode-string "Client Key")
                                           :sha256))
         (stored-key (openssl:compute-digest client-key :sha256))
         (client-signature
           (openssl:compute-hmac stored-key auth-message :sha256)))
    (core:octet-vector
     (map 'vector #'logxor client-key client-signature))))

(defun compute-scram-salted-password (password salt nb-iterations)
  (declare (type string password)
           (type core:octet-vector salt)
           (type (integer 1) nb-iterations))
  ;; RFC 5802 2.2. Notation
  ;;
  ;; The function which computes the salted password is named "Hi".
  (openssl:pbkdf2 (text:encode-string password)
                  salt :sha256 nb-iterations 32))

(defun check-scram-server-final-message (server-final-message
                                         salted-password auth-message)
  (let (verifier)
    (dolist (attribute (parse-scram-attributes server-final-message))
      (case (car attribute)
        (:verifier
         (setf verifier (cdr attribute)))))
    (unless verifier
      (scram-error "missing verifier in server-final-message data"))
    (let* ((signature (text:decode-base64 verifier))
           (server-key (openssl:compute-hmac salted-password
                                             (text:encode-string "Server Key")
                                             :sha256))
           (expected-signature
             (openssl:compute-hmac server-key auth-message :sha256)))
      (unless (openssl:constant-time-equal expected-signature signature)
        (scram-error "server signature mismatch")))))

(defun scram-encode-sasl-name (name stream)
  (declare (type string name)
           (type stream stream))
  (do ((i 0 (1+ i))
       (end (length name)))
      ((>= i end)
       nil)
    (let ((character (char name i)))
      (case character
        (#\,
         (write-string "=2C" stream))
        (#\=
         (write-string "=3D" stream))
        (t
         (write-char character stream))))))

(defun generate-scram-nonce ()
  (let* ((characters
           "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
         (length 24)
         (random-octets (openssl:random-octets length))
         (nonce (make-string length)))
    (dotimes (i length nonce)
      (setf (aref nonce i)
            (aref characters
                  (mod (aref random-octets i) (length characters)))))))
