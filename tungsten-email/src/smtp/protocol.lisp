(in-package :smtp)

(define-condition smtp-parse-error (parse-error)
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

(defun read-greeting-response (client)
  (declare (type client client))
  (with-slots (domains) client
    (let ((lines (read-response client)))
      (dolist (line lines nil)
        (let* ((text (third line))
               (space (position #\Space text)))
          (unless space
            (smtp-parse-error "invalid greeting format ~S" text))
          (push (subseq text 0 space) domains))))))
