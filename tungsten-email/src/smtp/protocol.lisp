(in-package :smtp)

(define-condition smtp-error (error)
  ())

(define-condition smtp-parse-error (smtp-error parse-error)
  ((description
    :type string
    :initarg :description))
  (:report
   (lambda (condition stream)
     (with-slots (description) condition
       (format stream "SMTP parse error: ~A." description)))))

(defun smtp-parse-error (format &rest args)
  (let ((description (apply #'format nil format args)))
    (error 'smtp-parse-error :description description)))

(define-condition unexpected-smtp-response (smtp-error)
  ((code
    :type integer
    :initarg :code
    :reader unexpected-smtp-response-code)
   (text
    :type string
    :initarg :text
    :reader unexpected-smtp-response-text))
  (:report
   (lambda (condition stream)
     (with-slots (code) condition
       (format stream "unexpected SMTP response ~D" code)))))

(defun write-command (command text client)
  (declare (type client client))
  (with-slots (stream) client
    (let ((line (concatenate 'string command " " text)))
      (write-line line stream))
    (finish-output stream)))

(defun parse-response-line (line)
  (declare (type string line))
  (let (code separator text)
    (when (< (length line) 4)
      (smtp-parse-error "truncated response line"))
    (unless (and (digit-char-p (char line 0))
                 (digit-char-p (char line 1))
                 (digit-char-p (char line 2)))
      (smtp-parse-error "invalid response code ~S" (subseq line 0 3)))
    (setf code (parse-integer line :start 0 :end 3))
    (setf separator (char line 3))
    (unless (or (char= separator #\Space)
                (char= separator #\-))
      (smtp-parse-error "invalid separator ~S" (subseq line 3 4)))
    (when (> (length line) 4)
      (setf text (subseq line 4)))
    (list code separator text)))

(defun read-response (client)
  (declare (type client client))
  (do ((lines nil))
      ((and lines (eq (second (car lines)) #\Space))
       (nreverse lines))
    (let ((line (read-line (client-stream client))))
      (push (parse-response-line line) lines))))

(defmacro do-response-lines ((code text client) &rest forms)
  (let ((client-var (gensym "CLIENT-"))
        (lines (gensym "LINES-"))
        (line (gensym "LINE-")))
    `(let* ((,client-var ,client)
            (,lines (read-response ,client-var)))
       (dolist (,line ,lines)
         (let ((,code (first ,line))
               (,text (third ,line)))
           (cond
             ,@(mapcar
                (lambda (form)
                  (etypecase (car form)
                    ;; (CODE FORM)
                    (integer
                     `((= ,code ,(car form))
                       ,@(cdr form)))
                    ;; ((MIN-CODE . MAX-CODE) FORM)
                    ((cons integer integer)
                     `((<= ,(caar form) ,code ,(cdar form))
                       ,@(cdr form)))))
                forms)
             (t
              (error 'unexpected-smtp-response :code ,code :text ,text))))))))

(defun read-greeting-response (client)
  (declare (type client client))
  (with-slots (server-domains) client
    (do-response-lines (code text client)
      (220
       (let ((space (position #\Space text)))
         (unless space
           (smtp-parse-error "invalid greeting format ~S" text))
         (push (subseq text 0 space) server-domains))))))

(defun send-ehlo-command (host client)
  (declare (type system:host host)
           (type client client))
  (with-slots (keywords) client
    (write-command "EHLO" (format-host host) client)
    (let ((first-line t))
      (do-response-lines (code text client)
        (250
         (cond
           (first-line
            (setf first-line nil))
           (t
            (push (parse-ehlo-keyword text) keywords))))))))

(defun parse-ehlo-keyword (string)
  (declare (type string string))
  (do ((start 0)
       (end (length string))
       (words nil))
      ((>= start end)
       (let* ((words (nreverse words))
              (name (car words))
              (parameters (cdr words)))
         (cond
           ((equalp name "8BITMIME")
            '(:8bitmime))
           ((equalp name "AUTH")
            (parse-ehlo-keyword-parameters/auth parameters))
           ((equalp name "CHUNKING")
            '(:chunking))
           ((equalp name "ENHANCEDSTATUSCODES")
            '(:enhancedstatuscodes))
           ((equalp name "PIPELINING")
            '(:pipelining))
           ((equalp name "SIZE")
            (parse-ehlo-keyword-parameters/size parameters))
           ((equalp name "SMTPUTF8")
            '(:smtputf8))
           (t
            words))))
    (let* ((space (position #\Space string :start start))
           (word-end (or space end)))
      (push (subseq string start word-end) words)
      (setf start (1+ word-end)))))

(defun parse-ehlo-keyword-parameters/auth (strings)
  (declare (type list strings))
  ;; RFC 4954 SMTP Service Extension for Authentication
  (flet ((parse-mechanism (string)
           (if (member string '("CRAM-MD5" "GSSAPI" "DIGEST-MD5" "LOGIN" "MD5"
                                "OAUTH10A" "OAUTHBEARER" "PLAIN"
                                "PLAIN-CLIENTTOKEN" "XOAUTH" "XOAUTH2")
                       :test #'equalp)
               (list (intern string :keyword))
               string)))
    (cons :auth (mapcar #'parse-mechanism strings))))

(defun parse-ehlo-keyword-parameters/size (strings)
  (declare (type list strings))
  ;; RFC 1870 SMTP Service Extension for Message Size Declaration
  (unless (= (length strings) 1)
    (smtp-parse-error "invalid parameters for SIZE EHLO keyword"))
  (let ((string (car strings)))
    (unless (every #'digit-char-p string)
      (smtp-parse-error "invalid parameter ~S for SIZE EHLO keyword"))
    (list :size (parse-integer string))))

(defun format-host (host)
  (declare (type system:host host))
  (etypecase host
    (string
     host)
    (system:ipv4-address
     (system:format-ip-address host))
    (system:ipv6-address
     ;; RFC 5321 4.1.3. Address Literals
     (concatenate 'string "IPv6:" (system:format-ip-address host)))))
