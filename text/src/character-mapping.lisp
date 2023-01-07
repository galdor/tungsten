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
