(in-package :netrc)

(define-condition invalid-data (error)
  ())

(define-condition invalid-token (invalid-data)
  ((token
     :type string
     :initarg :token
     :reader invalid-token-token))
  (:report
   (lambda (condition stream)
     (format stream "invalid netrc token ~S"
             (invalid-token-token condition)))))

(define-condition invalid-port-number (invalid-data)
  ((value
    :type (or string number)
    :initarg :value
    :reader invalid-port-number-value))
  (:report
   (lambda (condition stream)
     (format stream "invalid port number ~S"
             (invalid-port-number-value condition)))))

(define-condition missing-token (invalid-data)
  ((token
    :type string
    :initarg :token
    :reader missing-token-token))
  (:report
   (lambda (condition stream)
     (format stream "missing netrc token after ~S"
             (missing-token-token condition)))))

(define-condition orphaned-token (invalid-data)
  ((token
     :type string
     :initarg :token
     :reader orphaned-token-token))
  (:report
   (lambda (condition stream)
     (format stream "invalid netrc token ~S out of a \"default\" or ~
                     \"machine\" block"
             (orphaned-token-token condition)))))

(defun parse-entries (string &key (start 0) (end (length string)))
  (declare (type string string)
           (type (integer 0) start end))
  (flet ((spacep (character)
           (or (char= character #\Space)
               (char= character #\Tab)
               (char= character #\Newline))))
    (do ((start (or (position-if-not #'spacep string :start start :end end)
                    end))
         (entries nil)
         (entry nil))
        ((>= start end)
         (when entry
           (push entry entries))
         (nreverse entries))
      (labels ((read-token ()
                 (let* ((space (position-if #'spacep string
                                            :start start :end end))
                        (token-end (or space end)))
                   (prog1 (when (> token-end start)
                            (subseq string start token-end))
                     (setf start (or (position-if-not #'spacep string
                                                      :start token-end
                                                      :end end)
                                     end)))))
               (read-value (token slot &optional (parser #'identity))
                 (let ((value (read-token)))
                   (unless value
                     (error 'missing-token :token token))
                   (unless entry
                     (error 'orphaned-token :token token))
                   (setf (slot-value entry slot) (funcall parser value)))))
        (let ((token (read-token)))
          (core:string-case token
            ("default"
              (when entry
                (push entry entries))
              (setf entry (make-instance 'entry :machine :default)))
            ("machine"
              (when entry
                (push entry entries))
              (let ((machine (read-token)))
                (unless machine
                  (error 'missing-token :token token))
                (setf entry (make-instance 'entry :machine machine))))
            ("login"
              (read-value token 'login))
            ("password"
              (read-value token 'password))
            ("account"
              (read-value token 'account))
            ("port"
              (read-value token 'port #'parse-port-number))
            (t
              (error 'invalid-token :token token))))))))

(defun parse-port-number (string)
  (unless (every #'digit-char-p string)
    (error 'invalid-port-number :value string))
  (let ((number (parse-integer string)))
    (cond
      ((< 0 number 65536)
       number)
      (t
       (error 'invalid-port-number :value number)))))
