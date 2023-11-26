(in-package :ucd)

;;; See Unicode Standard Annex #44 4.2
;;; (https://www.unicode.org/reports/tr44/#Format_Conventions) for a
;;; description of the file format used in the Unicode Character Database.

(deftype code-point-or-range ()
  '(or unicode:code-point (cons unicode:code-point unicode:code-point)))

(defparameter *ucd-path*
  (asdf:system-relative-pathname "tungsten-text" #p"data/ucd/"))

(defun data-file-path (subpath)
  (declare (type (or pathname string) subpath))
  (merge-pathnames subpath *ucd-path*))

(defmacro with-data-file ((stream subpath) &body body)
  (let ((path (gensym "PATH-")))
    `(let ((,path (data-file-path ,subpath)))
       ;; Let us hope the implementation supports the :UTF-8 external format
       (with-open-file (,stream ,path :external-format :utf-8)
         ,@body))))

(defun mapc-unicode-data-file (function)
  (declare (type (or symbol function) function))
  (with-data-file (stream "UnicodeData.txt")
    (loop
      (let ((line (read-line stream nil nil)))
        (unless line
          (return))
        (destructuring-bind (code-point name &rest properties)
            (parse-unicode-data-fields line)
          ;; Do not include private use planes, we have no need for 130k+
          ;; characters which are not defined anyway.
          (when (>= code-point #xf0000)
            (return))
          (cond
            ((and (consp name) (eq (car name) 'start))
             (let* ((range-name (cdr name))
                    (line-2 (read-line stream nil nil)))
               (unless line-2
                 (error "missing end of range ~S starting at code point ~
                         U+~4,'0X" range-name code-point))
               (destructuring-bind (code-point-2 name-2 &rest properties-2)
                   (parse-unicode-data-fields line-2)
                 (declare (ignore properties-2))
                 (unless (and (consp name-2) (eq (car name-2) 'end))
                   (error "invalid line ~S as end of range ~S starting at ~
                           code point U+~4,'0X" line-2 range-name code-point))
                 (unless (>= code-point-2 code-point)
                   (error "invalid end code point U+~4,'0X for range ~S ~
                           starting at code point U+~4,'0X"
                          code-point-2 range-name code-point))
                 (do ((cp code-point (1+ cp)))
                     ((> cp code-point-2)
                      nil)
                   ;; The name of the characters which are part of a range
                   ;; cannot be derived without information from other data
                   ;; files.
                   (apply function cp nil properties)))))
            (t
             (let ((name (if (string= name "<control>")
                             (nth 8 properties) ; unicode-1-name
                             name)))
               (apply function code-point name properties)))))))))

(defun parse-unicode-data-fields (line)
  (labels ((has-suffix (string suffix)
             (let ((string-length (length string))
                   (suffix-length (length suffix)))
               (and (>= string-length suffix-length)
                    (string= string suffix
                             :start1 (- string-length suffix-length)))))
           (range-info (name marker)
             (let ((suffix (concatenate 'string ", " marker ">")))
               (when (and (char= (char name 0) #\<)
                          (has-suffix name suffix))
                 (subseq name 1 (- (length name) (length suffix)))))))
    (destructuring-bind (code-point-string
                         name-string
                         general-category
                         canonical-combining-class-string
                         bidi-class
                         decomposition-string
                         numeric-decimal-string
                         numeric-digit-string
                         numeric-numeric-string
                         bidi-mirrored-string
                         unicode-1-name
                         iso-comment
                         simple-uppercase-mapping-string
                         simple-lowercase-mapping-string
                         simple-titlecase-mapping-string)
        (parse-fields line)
      (let* ((code-point (parse-code-point code-point-string))
             (name
               (let ((name-start (range-info name-string "First"))
                     (name-end (range-info name-string "Last")))
                 (cond
                   (name-start
                    (cons 'start name-start))
                   (name-end
                    (cons 'end name-end))
                   (t
                    name-string))))
             (canonical-combining-class
               (parse-integer canonical-combining-class-string))
             (bidi-mirrored
               (parse-boolean bidi-mirrored-string))
             (simple-uppercase-mapping
               (when simple-uppercase-mapping-string
                 (parse-code-point simple-uppercase-mapping-string)))
             (simple-lowercase-mapping
               (when simple-lowercase-mapping-string
                 (parse-code-point simple-lowercase-mapping-string)))
             (simple-titlecase-mapping
               (if simple-titlecase-mapping-string
                   (parse-code-point simple-titlecase-mapping-string)
                   simple-uppercase-mapping)))
        (list code-point
              name
              general-category
              canonical-combining-class
              bidi-class
              decomposition-string
              numeric-decimal-string
              numeric-digit-string
              numeric-numeric-string
              bidi-mirrored
              unicode-1-name
              iso-comment
              simple-uppercase-mapping
              simple-lowercase-mapping
              simple-titlecase-mapping)))))

(defun parse-fields (line)
  (declare (type string line))
  (do ((fields nil)
       (start 0)
       (end (or (position #\# line) (length line))))
      ((> start end)
       (nreverse fields))
    (let* ((semicolon (position #\; line :start start))
           (value-end (or semicolon end))
           (string (string-trim '(#\Space #\Tab)
                                (subseq line start value-end))))
      (push (if (zerop (length string)) nil string) fields)
      (setf start (1+ value-end)))))

(defun parse-code-point (string &key (start 0) (end (length string)))
  (declare (type string string)
           (type (integer 0) start end))
  (let ((code-point (parse-integer string :start start :end end :radix 16)))
    (unless (<= 0 code-point unicode:highest-code-point)
      (error "invalid code point ~D" code-point))
    code-point))

(defun parse-code-point-or-range (string)
  (declare (type string string))
  (let ((full-stop (position #\. string)))
    (cond
      (full-stop
       (unless (char= (char string (1+ full-stop)) #\.)
         (error "invalid code point range ~S" string))
       (cons (parse-code-point string :end full-stop)
             (parse-code-point string :start (+ full-stop 2))))
      (t
       (parse-code-point string)))))

(defun parse-boolean (string)
  (declare (type string string))
  (cond
    ((string= string "Y")
     t)
    ((string= string "N")
     nil)
    (t
     (error "invalid boolean ~" string))))
