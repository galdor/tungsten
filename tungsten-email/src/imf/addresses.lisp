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

(defun serialize-address (address)
  (declare (type address address))
  (etypecase address
    (mailbox
     (serialize-mailbox address))
    (group
     (serialize-group address))))

(defun serialize-mailbox (mailbox)
  (declare (type mailbox mailbox))
  (with-line-writer/string ()
    (write-mailbox-tokens mailbox)))

(defun write-mailbox-tokens (mailbox)
  (declare (type mailbox mailbox))
  (with-slots (display-name local-part domain) mailbox
    (cond
      (display-name
       (write-token display-name)
       (write-token :space)
       (write-token #\<)
       (write-address-specification-tokens local-part domain)
       (write-token #\>))
      (t
       (write-address-specification-tokens local-part domain)))))

(defun serialize-group (group)
  (declare (type group group))
  (with-line-writer/string ()
    (write-group-tokens group)))

(defun write-group-tokens (group)
  (declare (type group group))
  (with-slots (display-name mailboxes) group
    (write-token display-name)
    (write-token #\:)
    (write-token :space)
    (dolist (mailbox mailboxes)
      (write-mailbox-tokens mailbox))
    (write-token #\;)))

(defun serialize-address-specification (local-part domain)
  (declare (type string local-part domain))
  (with-line-writer/string ()
    (write-address-specification-tokens local-part domain)))

(defun write-address-specification-tokens (local-part domain)
  (declare (type string local-part domain))
  (write-token local-part)
  (write-token #\@)
  (write-token domain))
