(in-package :system)

(defclass tcp-server ()
  ((acceptors
    :type list
    :initarg :acceptors
    :reader tcp-server-acceptors)))

(defun make-tcp-server (host port)
  (declare (type host host)
           (type port-number port))
  (let ((addresses (resolve-net-service host port))
        (acceptors nil))
    (unless addresses
      (error "no socket address found for ~S"
             (format-host-and-port host port)))
    (core:abort-protect
        (progn
          (dolist (address addresses)
            (push (make-tcp-acceptor address) acceptors))
          ;; TODO IO base
          (make-instance 'tcp-server :acceptors acceptors))
      (dolist (acceptor acceptors)
        (ignore-errors (close-tcp-acceptor acceptor))))))
