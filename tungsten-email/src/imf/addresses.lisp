(in-package :imf)

(defclass mailbox ()
  ((display-name
    :type (or string null)
    :initarg :display-name
    :initform nil
    :accessor mailbox-display-name)
   (local-part
    :type string
    :initarg :local-part
    :accessor mailbox-local-part)
   (domain
    :type string
    :initarg :domain
    :accessor mailbox-domain)))

(defclass group ()
  ((display-name
    :type string
    :initarg :display-name
    :accessor group-display-name)
   (mailboxes
    :type list
    :initarg :mailboxes
    :initform nil
    :accessor group-mailboxes)))

(deftype address ()
  '(or mailbox group))

(defun make-mailbox (local-part domain &key display-name)
  (declare (type string local-part domain)
           (type (or string null) display-name))
  (make-instance 'mailbox :local-part local-part
                          :domain domain
                          :display-name display-name))

(defun make-group (display-name mailboxes)
  (declare (type string display-name)
           (type list mailboxes))
  (make-instance 'group :display-name display-name
                        :mailboxes mailboxes))

(defmethod print-object ((mailbox mailbox) stream)
  (print-unreadable-object (mailbox stream :type t)
    (write-string (serialize-mailbox mailbox) stream)))

(defmethod print-object ((group group) stream)
  (print-unreadable-object (group stream :type t)
    (write-string (group-display-name group) stream)))

(defmethod write-tokens ((mailbox mailbox))
  (with-slots (display-name local-part domain) mailbox
    (flet ((write-address-specification-tokens ()
             (write-token local-part)
             (write-token #\@)
             (write-token domain)))
      (cond
        (display-name
         (write-token display-name)
         (write-token :space)
         (write-token #\<)
         (write-address-specification-tokens)
         (write-token #\>))
        (t
         (write-address-specification-tokens))))))

(defmethod write-tokens ((group group))
  (with-slots (display-name mailboxes) group
    (write-token display-name)
    (write-token #\:)
    (write-token :space)
    (do ((mailboxes mailboxes (cdr mailboxes))
         (i 0 (1+ i)))
        ((null mailboxes))
      (write-tokens (car mailboxes))
      (unless (null (cdr mailboxes))
        (write-token #\,)))
    (write-token #\;)))

(defun serialize-address (address)
  (declare (type address address))
  (with-line-writer (nil)
    (write-tokens address)))

(defun serialize-mailbox (mailbox)
  (declare (type mailbox mailbox))
  (with-line-writer (nil)
    (write-tokens mailbox)))
