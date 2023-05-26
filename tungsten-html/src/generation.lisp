(in-package :html)

(defvar *html-output* nil)

(define-condition invalid-generation-data (error)
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

(defmacro html (value)
  `(generate ,value))

(defmacro generate (value)
  (cond
    ;; ((TAG &REST ATTRIBUTES) &REST CHILDREN)
    ((and (listp value) (listp (car value)) (typep (caar value) 'keyword))
     `(generate-element ,(caar value) ,(cdar value) ,(cdr value)))
    ;; (TAG &REST CHILDREN) or (SPECIAL-TAG &REST ARGUMENTS)
    ((and (listp value) (typep (car value) 'keyword))
     (case (car value)
       (:doctype
        `(generate-doctype ,(cdr value)))
       (:raw
        `(generate-raw-data (list ,@(cdr value))))
       (:comment
        `(generate-comment (list ,@(cdr value))))
       (t
        `(generate-element ,(car value) nil ,(cdr value)))))
    ;; (FORMAT &REST ARGUMENTS)
    ((and (listp value) (stringp (car value)))
     `(generate-formatted-text ,(car value) ,(cdr value)))
    ;; STRING
    ((stringp value)
     `(generate-text ,value))
    ;; Any other expression is expanded to itself
    (t
     value)))

(defmacro generate-doctype (arguments)
  (let ((doctype (or (car arguments) "HTML")))
    `(format *html-output* "<!DOCTYPE ~A>" ,doctype)))

(defmacro generate-raw-data (arguments)
  (let ((argument (gensym "ELEMENT-")))
    `(dolist (,argument ,arguments)
       (write-string ,argument *html-output*))))

(defmacro generate-comment (arguments)
  (let ((argument (gensym "ELEMENT-")))
    `(progn
       (write-string "<!-- " *html-output*)
       (dolist (,argument ,arguments)
         (write-string ,argument *html-output*))
       (write-string " -->" *html-output*))))

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
                    (write-string (escape-attribute ,value) *html-output*)
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
  `(write-string (escape-text-element ,value) *html-output*))

(defun void-element-p (element)
  (declare (type symbol element))
  (member element
          '(:area :base :br :col :embed :hr :img :input :link :meta :source
            :track :wbr)))

(defun escape-text-element (string)
  (flet ((special-character-p (character)
           (or (char= character #\&)
               (char= character #\ )
               (char= character #\<)
               (char= character #\>))))
    (with-output-to-string (stream)
      (do ((start 0)
           (end (length string)))
          ((>= start end)
           nil)
        (let* ((position (position-if #'special-character-p string
                                      :start start))
               (part-end (or position end)))
          (write-string string stream :start start :end part-end)
          (when position
            (let ((character (char string position)))
              (write-string
               (cond
                 ((char= character #\&) "&amp;")
                 ((char= character #\ ) "&nbsp;")
                 ((char= character #\<) "&lt;")
                 ((char= character #\>) "&gt;")
                 (t ""))
               stream)))
          (setf start (1+ part-end)))))))

(defun escape-attribute (string)
  (flet ((special-character-p (character)
           (or (char= character #\&)
               (char= character #\ )
               (char= character #\"))))
    (with-output-to-string (stream)
      (do ((start 0)
           (end (length string)))
          ((>= start end)
           nil)
        (let* ((position (position-if #'special-character-p string
                                      :start start))
               (part-end (or position end)))
          (write-string string stream :start start :end part-end)
          (when position
            (let ((character (char string position)))
              (write-string
               (cond
                 ((char= character #\&) "&amp;")
                 ((char= character #\ ) "&nbsp;")
                 ((char= character #\") "&quot;")
                 (t ""))
               stream)))
          (setf start (1+ part-end)))))))
