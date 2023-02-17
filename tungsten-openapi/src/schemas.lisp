(in-package :openapi)

;;; JSON schema (which is a subset of the schema system used by OpenAPI) is a
;;; large and complex way of describing data. Implementing complete validation
;;; would required a ton of work. Fortunately we only need to understand the
;;; subset of JSON schema required to map JSON data to Common Lisp data
;;; structures.
;;;
;;; The tungsten-openapi system only supports clients, and there is no point
;;; in trying to precisely validate what the server returns. As long as we
;;; have enough information to encode request data and decode response data,
;;; we are good.

;;; TODO all-of, one-of, any-of, not
;;; TODO default
;;; TODO nullable

(defun build-schema-json-mapping (schema)
  (declare (type list schema))
  (let ((type (cdr (assoc 'type schema))))
    (case type
      (:boolean
       (build-schema-json-mapping/boolean schema))
      (:integer
       (build-schema-json-mapping/integer schema))
      (:number
       (build-schema-json-mapping/number schema))
      (:string
       (build-schema-json-mapping/string schema))
      (:array
       (build-schema-json-mapping/array schema))
      (:object
       (build-schema-json-mapping/object schema))
      (t
       '(:any)))))

(defun build-schema-json-mapping/boolean (schema)
  (declare (type list schema)
           (ignore schema))
  '(:boolean))

(defun build-schema-json-mapping/integer (schema)
  (declare (type list schema))
  (let ((properties nil))
    (dolist (member schema)
      (case (car member)
        (minimum
         (let ((exclusive (cdr (assoc 'exclusive-minimum schema)))
               (n (cdr member)))
           (push `(:min ,(if exclusive (1+ n) n)) properties)))
        (maximum
         (let ((exclusive (cdr (assoc 'exclusive-maximum schema)))
               (n (cdr member)))
           (push `(:max ,(if exclusive (1- n) n)) properties)))))
    `(:integer ,@(apply #'append properties))))

(defun build-schema-json-mapping/number (schema)
  (declare (type list schema))
  (let ((properties nil))
    (dolist (member schema)
      (case (car member)
        (minimum
         (let ((exclusive (cdr (assoc 'exclusive-minimum schema)))
               (n (cdr member)))
           (push `(:min ,(if exclusive (1+ n) n)) properties)))
        (maximum
         (let ((exclusive (cdr (assoc 'exclusive-maximum schema)))
               (n (cdr member)))
           (push `(:max ,(if exclusive (1- n) n)) properties)))))
    `(:number ,@(apply #'append properties))))

(defun build-schema-json-mapping/string (schema)
  (declare (type list schema))
  (let ((properties nil))
    (dolist (member schema)
      (case (car member)
        (min-length
         (let ((exclusive (cdr (assoc 'exclusive-minimum schema)))
               (n (cdr member)))
           (push `(:min-length ,(if exclusive (1+ n) n)) properties)))
        (max-length
         (let ((exclusive (cdr (assoc 'exclusive-maximum schema)))
               (n (cdr member)))
           (push `(:max-length ,(if exclusive (1- n) n)) properties)))
        (enum
         (let ((values nil))
           (dotimes (i (length (cdr member)))
             (let ((value (aref (cdr member) i)))
               (when (stringp value)
                 ;; TODO convert to symbol and push (VALUE SYMBOL)
                 (push value values))))
           (when values
             (push `(:value ,(nreverse values)) properties))))))
    `(:string ,@(apply #'append properties))))

(defun build-schema-json-mapping/array (schema)
  (declare (type list schema))
  (let ((properties nil))
    (dolist (member schema)
      (case (car member)
        (min-items
         (let ((exclusive (cdr (assoc 'exclusive-minimum schema)))
               (n (cdr member)))
           (push `(:min-length ,(if exclusive (1+ n) n)) properties)))
        (max-items
         (let ((exclusive (cdr (assoc 'exclusive-maximum schema)))
               (n (cdr member)))
           (push `(:max-length ,(if exclusive (1- n) n)) properties)))
        (items
         (let ((element-mapping (build-schema-json-mapping (cdr member))))
           (push `(:element ,element-mapping) properties)))))
    `(:array ,@(apply #'append properties))))

(defun build-schema-json-mapping/object (schema)
  (declare (type list schema))
  (let ((properties nil))
    (dolist (member schema)
      (case (car member)
        (properties
         (let ((member-mappings
                 (mapcar (lambda (member)
                           (list (car member)
                                 (car member) ; TODO convert to symbol
                                 (build-schema-json-mapping (cdr member))))
                         (cdr member))))
           (push `(:members ,member-mappings) properties)))
        (additional-properties
         (let ((value-schema (cdr member)))
           (unless (typep value-schema 'boolean)
             (let ((value-mapping (build-schema-json-mapping value-schema)))
               (push `(:value ,value-mapping) properties)))))
        (required
         (let ((names (cdr member)))
           (push `(:required ,(coerce names 'list)) properties)))))
    `(:object ,@(apply #'append properties))))
