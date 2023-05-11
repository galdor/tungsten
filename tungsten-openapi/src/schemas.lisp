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
;;;
;;; We have to work around JSON schema limitations. JSON schema does not
;;; describe what data are: it simply lists what data may or may not look
;;; like. Multiple valid JSON schemas are useless to decide how to represent
;;; data. An obvious example would be {"not": {"type": "integer"}}.
;;; Problematic constructs include:
;;;
;;; - Any schema without "type".
;;;
;;; - Any schema with a combinator ("allOf", "oneOf", "anyOf", "not") with
;;;   other properties.
;;;
;;; - Any schema with multiple combinators.
;;;
;;; It is possible to expand combinators into manageable forms (e.g.
;;; transforming {"minimum": 1, "oneOf": [{"type": "integer"}, {"type":
;;; "boolean"}]} into {"oneOf": [{"type": "integer", "minimum": 1}, {"type":
;;; "boolean", "minimum": 1}]}. But we should be able to avoid the extra work
;;; since schemas tend to keep things simple.

(defun build-schema-json-mapping (schema)
  (declare (type list schema))
  (let* ((combinator (find-if (lambda (property)
                                (member property '(all-of one-of any-of not)))
                              schema :key #'car))
         (type (cdr (assoc 'type schema)))
         (nullable (cdr (assoc 'nullable schema)))
         (schema (cond
                   (combinator
                    (case (car combinator)
                      (one-of
                       `(:or :mappings ,(map 'list 'build-schema-json-mapping
                                             (cdr combinator))))
                      (any-of
                       `(:or :mappings ,(map 'list 'build-schema-json-mapping
                                             (cdr combinator))))
                      (t
                       '(:any))))
                   (t
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
                       '(:any)))))))
    (cond
      ((and nullable (eq (car schema) :or))
       `(:or :mappings ((:null) ,@(getf (cdr schema) :mappings))))
      (nullable
       `(:or :mappings ((:null) ,schema)))
      (t
       schema))))

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
                 (push (list value (json-name-to-symbol value)) values))))
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
                                 (json-name-to-symbol (car member))
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

(defun json-name-to-symbol (name)
  (declare (type string name))
  (intern (json-name-to-symbol-name name)))

(defun json-name-to-symbol-name (name)
  (declare (type string name))
  (with-output-to-string (stream)
    (do* ((start 0)
          (end (length name))
          (i start)
          (lower-case-p (when (> end start)
                          (lower-case-p (char name start)))))
         ((>= i end)
          nil)
      (let ((character (char name i)))
        (cond
          ((char= character #\_)
           (write-char #\- stream))
          ((and lower-case-p (upper-case-p character))
           (write-char #\- stream)
           (write-char character stream)
           (setf lower-case-p nil))
          ((and (not lower-case-p) (lower-case-p character))
           (write-char (char-upcase character) stream)
           (setf lower-case-p t))
          ((upper-case-p character)
           (when (and (> i start) (< (1+ i) end)
                      (lower-case-p (char name (1+ i))))
             (write-char #\- stream))
           (write-char character stream))
          (t
           (write-char (char-upcase character) stream)))
        (incf i)))))
