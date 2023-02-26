(in-package :html)

(defvar *html-output* nil)

(define-condition invalid-generation-data ()
  ((format-control
    :type string
    :initarg :format-control)
   (format-arguments
    :type list
    :initarg :format-arguments))
  (:report
   (lambda (condition stream)
     (with-slots (format-control format-arguments) condition
       (format stream "Invalid HTML generation data: ~?."
               format-control format-arguments)))))

(defun invalid-generation-data (format &rest arguments)
  (error 'invalid-generation-data :format-control format
                                  :format-arguments arguments))

(defmacro with-html ((&key stream) &rest values)
  (if stream
      `(let ((*html-output* ,stream))
         ,@(mapcar (lambda (value) `(generate ,value)) values)
         nil)
      `(with-output-to-string (*html-output*)
         ,@(mapcar (lambda (value) `(generate ,value)) values))))

(defmacro generate (value)
  (cond
    ;; ((TAG &REST ATTRIBUTES) &REST CHILDREN)
    ((and (listp value) (listp (car value)) (typep (caar value) 'keyword))
     `(generate-element ,(caar value) ,(cdar value) ,(cdr value)))
    ;; (TAG &REST CHILDREN)
    ((and (listp value) (typep (car value) 'keyword))
     `(generate-element ,(car value) nil ,(cdr value)))
    ;; (FORMAT &REST ARGUMENTS)
    ((and (listp value) (stringp (car value)))
     `(generate-formatted-text ,(car value) ,(cdr value)))
    ;; STRING
    ((stringp value)
     `(generate-text ,value))
    ;; Any other expression is expected to yield a string
    (t
     `(generate-text ,value))))

(defmacro generate-element (name attributes children)
  (when (and (void-element-p name) children)
    (invalid-generation-data "void element ~S cannot have children" name))
  (let ((name-string (string-downcase (symbol-name name))))
    `(progn
       (write-string ,(concatenate 'string "<" name-string) *html-output*)
       ,@(mapcar
          (lambda (group)
            (destructuring-bind (name . value) group
              (cond
                (value
                 `(progn
                    (write-string
                     ,(concatenate 'string " "
                                   (string-downcase (symbol-name name))
                                   "=\"")
                     *html-output*)
                    (write-string ,value *html-output*)
                    (write-char #\" *html-output*)))
                (t
                 `(write-string
                   ,(concatenate 'string " "
                                 (string-downcase (symbol-name name)))
                   *html-output*)))))
          (group-attributes attributes))
       (write-char #\> *html-output*)
       ,@(mapcar
          (lambda (child)
            `(generate ,child))
          children)
       ,(unless (void-element-p name)
          `(write-string ,(concatenate 'string "</" name-string ">")
                         *html-output*)))))

(defun group-attributes (attributes)
  (do ((groups nil))
      ((null attributes)
       (nreverse groups))
    (let ((name (pop attributes)))
      (if (typep (car attributes) 'keyword)
          (push (cons name nil) groups)
          (push (cons name (pop attributes)) groups)))))

(defmacro generate-formatted-text (format arguments)
  `(format *html-output* ,format ,@arguments))

(defmacro generate-text (value)
  `(write-string ,value *html-output*))

(defun void-element-p (element)
  (declare (type symbol element))
  (member element
          '(:area :base :br :col :embed :hr :img :input :link :meta :source
            :track :wbr)))
