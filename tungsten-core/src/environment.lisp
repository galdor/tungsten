(in-package :core)

;;; See CLtL2 8.5. Environments.
;;;
;;; VARIABLE-INFORMATION and FUNCTION-INFORMATION return NIL for unknown
;;; variables and functions. We are keeping this behaviour to stay conformant.
;;; Functions based on this interface, e.g. FUNCTION-TYPE, signal errors for
;;; unknown bindings: this allows us to distinguish between an unknown binding
;;; and missing information about this binding.

(deftype variable-binding ()
  '(member
    nil :special :lexical :symbol-macro :constant
    #+sbcl :global
    #+sbcl :alien))

(deftype function-binding ()
  '(member
    nil :function :macro :special-form))

(declaim (ftype (function (symbol &optional t)
                          (values variable-binding boolean list))
                variable-information))
(defun variable-information (variable &optional env)
  #+sbcl (sb-cltl2:variable-information variable env)
  #+ccl  (ccl:variable-information variable env))

(declaim (ftype (function (symbol &optional t)
                          (values function-binding boolean list))
                function-information))
(defun function-information (function &optional env)
  #+sbcl (sb-cltl2:function-information function env)
  #+ccl  (ccl:function-information function env))

(declaim (ftype (function (symbol &optional t) t) variable-type))
(defun variable-type (variable &optional env)
  (multiple-value-bind (binding local info)
      (variable-information variable env)
    (declare (ignore local))
    (unless binding
      (error 'unbound-variable :name variable))
    (or (cdr (assoc 'type info)) t)))

(declaim (ftype (function (symbol &optional t) t) function-type))
(defun function-type (function &optional env)
  (multiple-value-bind (binding local info)
      (function-information function env)
    (declare (ignore local))
    (unless binding
      (error 'undefined-function :name function))
    (or (cdr (assoc 'ftype info)) 'function)))
