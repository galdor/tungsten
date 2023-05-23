(in-package :sqlite)

(deftype transaction-type ()
  '(member :deferred :immediate :exclusive))

(defmacro with-transaction ((&key (type :deferred)) &body body)
  `(progn
     (query (concatenate 'string "BEGIN " (symbol-name ,type)))
     (core:abort-protect
         (progn
           ,@body
           (query "COMMIT"))
       (query "ROLLBACK"))))
