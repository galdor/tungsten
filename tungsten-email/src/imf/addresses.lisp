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
