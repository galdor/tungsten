(in-package :imf)

(defstruct (message-id
            (:constructor nil))
  (left "" :type string)
  (right "" :type string))

(defun make-message-id (left right)
  (declare (type string left right))
  (let ((id (make-instance 'message-id)))
    (setf (message-id-left id) left)
    (setf (message-id-right id) right)
    id))

(defun generate-message-id ()
  (let ((left (uuid:serialize (uuid:generate :v7)))
        (right (system:hostname)))
    (make-message-id left right)))

(defun encode-message-id (id)
  (declare (type message-id id))
  (with-slots (left right) id
    (encode-character #\<)
    (encode-dot-atom-or-quoted-string left)
    (encode-character #\@)
    (encode-domain right)
    (encode-character #\>)))

(defun encode-message-ids (ids)
  (declare (type list ids))
  (do ((ids ids (cdr ids)))
      ((null ids)
       nil)
    (encode-message-id (car ids))
    (unless (null (cdr ids))
      (encode-character #\Space))))
