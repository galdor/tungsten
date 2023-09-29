(in-package :text)

(defmacro define-character-mapping ((name) table)
  (let ((character-table-var (intern (format nil "*~A-CHARACTERS*" name)))
        (code-table-var (intern (format nil "*~A-CODES*" name))))
    `(progn
       (defparameter ,character-table-var
         (make-array ,(length table) :element-type '(or character integer null)
                                     :initial-contents ,table))
       (dotimes (i (length ,character-table-var))
         (let ((value (svref ,character-table-var i)))
           (when (integerp value)
             (setf (svref ,character-table-var i) (code-char value)))))
       (defparameter ,code-table-var
         (make-hash-table))
       (dotimes (code (length ,character-table-var))
         (setf (gethash (svref ,character-table-var code) ,code-table-var)
               code))
       (defun ,(intern (format nil "CODE-TO-CHARACTER/~A" name)) (code)
         (declare (type (integer 0) code))
         (when (<= 0 code ,(1- (length table)))
           (svref ,character-table-var code)))
       (defun ,(intern (format nil "CHARACTER-TO-CODE/~A" name)) (character)
         (declare (type character character))
         (gethash character ,code-table-var)))))

(defmacro define-character-mapping-encoding (id name mapping)
  (flet ((function-name (&rest parts)
           (intern (apply #'concatenate 'string (mapcar #'string parts)))))
    `(progn
       (defun ,(function-name "ENCODED-CHARACTER-LENGTH/" id) (c)
         (declare (type character c))
         (let ((code (,(function-name "CHARACTER-TO-CODE/" mapping) c)))
           (unless code
             (error 'unencodable-character :character c :encoding ,id))
           1))
       (defun ,(function-name "ENCODED-STRING-LENGTH/" id) (string start end)
         (declare (type string string)
                  (type (or index null) start end))
         (do ((max-index (1- (or end (length string))))
              (length 0)
              (i (or start 0) (1+ i)))
             ((> i max-index)
              length)
           (incf length (,(function-name "ENCODED-CHARACTER-LENGTH/" id)
                         (schar string i)))))
       (defun ,(function-name "ENCODE-CHARACTER/" id) (c octets offset)
         (declare (type character c)
                  (type core:octet-vector octets)
                  (type (or index null) offset))
         (let ((code (,(function-name "CHARACTER-TO-CODE/" mapping) c)))
           (unless code
             (error 'unencodable-character :character c :encoding ,id))
           (setf (aref octets offset) code)
           1))
       (defun ,(function-name "DECODED-STRING-LENGTH/" id) (octets start end)
         (declare (type core:octet-vector octets)
                  (type (or index null) start end))
         (- (or end (length octets)) (or start 0)))
       (defun ,(function-name "DECODE-CHARACTER/" id) (octets start end)
         (declare (type core:octet-vector octets)
                  (type (or index null) start end))
         (let ((start (or start 0))
               (end (or end (length octets))))
           (cond
             ((>= start end)
              (values nil 0))
             (t
              (let* ((octet (aref octets start))
                     (c (,(function-name "CODE-TO-CHARACTER/" mapping) octet)))
                (unless c
                  (error 'invalid-octet :octets octets :offset start
                                        :octet octet :encoding ,id))
                (values c 1))))))
       (define-encoding ,id ()
         :name ,name
         :encoded-character-length-function
         #',(function-name "ENCODED-CHARACTER-LENGTH/" id)
         :encoded-string-length-function
         #',(function-name "ENCODED-STRING-LENGTH/" id)
         :character-encoding-function
         #',(function-name "ENCODE-CHARACTER/" id)
         :decoded-string-length-function
         #',(function-name "DECODED-STRING-LENGTH/" id)
         :character-decoding-function
         #',(function-name "DECODE-CHARACTER/" id)))))
