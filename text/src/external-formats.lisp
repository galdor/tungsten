(in-package :text)

(defparameter *default-eol-style* :lf)
(defparameter *default-external-format* *default-encoding*)

(deftype eol-style ()
  '(member :cr :lf :crlf))

(deftype external-format ()
  '(or symbol (cons symbol list)))

(defun external-format-encoding (format)
  (declare (type external-format format))
  (etypecase format
    (symbol
     (if (eq format :default)
         *default-encoding*
         format))
    (list
     (car format))))

(defun external-format-eol-style (format)
  (declare (type external-format format))
  (etypecase format
    (symbol
     *default-eol-style*)
    (list
     (getf (cdr format) :eol-style *default-eol-style*))))

(declaim (inline eol-octets))
(defun eol-octets (style)
  (declare (type eol-style style))
  (ecase style
    (:cr #.(core:octet-vector* 13))
    (:lf #.(core:octet-vector* 10))
    (:crlf #.(core:octet-vector* 13 10))))
