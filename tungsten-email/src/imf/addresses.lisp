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
    (write-string (serialize-address mailbox) stream)))

(defmethod print-object ((group group) stream)
  (print-unreadable-object (group stream :type t)
    (prin1 (group-display-name group) stream)))

(defun serialize-address (address)
  (declare (type address address))
  (with-encoder-to-string ()
    (encode-address address)))

(defun encode-domain-literal (domain)
  (declare (type string domain))
  (encode-string domain))

(defun encode-domain (domain)
  (declare (type string domain))
  (cond
    ((dot-atom-p domain)
     (encode-dot-atom domain))
    (t
     (encode-domain-literal domain))))

(defun encode-specific-address (local-part domain)
  (declare (type string local-part domain))
  (encode-dot-atom-or-quoted-string local-part)
  (encode-character #\@)
  (encode-domain domain))

(defun encode-mailbox (mailbox)
  (declare (type mailbox mailbox))
  (with-slots (display-name local-part domain) mailbox
    (cond
      (display-name
       (encode-quoted-string display-name)
       (encode-character #\Space)
       (encode-character #\<)
       (encode-specific-address local-part domain)
       (encode-character #\>))
      (t
       (encode-specific-address local-part domain)))))

(defun encode-group (group)
  (declare (type group group))
  (with-slots (display-name mailboxes) group
    (encode-phrase display-name)
    (encode-string ": ")
    (do ((mailboxes mailboxes (cdr mailboxes)))
        ((null mailboxes)
         nil)
      (encode-mailbox (car mailboxes))
      (unless (null (cdr mailboxes))
        (encode-string ", ")))
    (encode-character #\;)))

(defun encode-address (address)
  (declare (type address address))
  (etypecase address
    (mailbox
     (encode-mailbox address))
    (group
     (encode-group address))))

(defun encode-addresses (addresses)
  (declare (type list addresses))
  (do ((addresses addresses (cdr addresses)))
      ((null addresses)
       nil)
    (encode-address (car addresses))
    (unless (null (cdr addresses))
      (encode-string ", "))))
