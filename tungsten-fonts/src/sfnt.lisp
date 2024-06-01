(in-package :fonts)

;;; References:
;;; - https://learn.microsoft.com/en-us/typography/opentype/spec/
;;; - https://developer.apple.com/fonts/TrueType-Reference-Manual/

(deftype tag ()
  '(string 4))

(defclass decoder ()
  ((data
    :type core:octet-vector
    :initarg :data
    :reader decoder-data)
   (start
    :type (integer 0)
    :initarg :start
    :accessor decoder-start)
   (end
    :type (integer 0)
    :initarg :end
    :accessor decoder-end)
   (context
    :type (or string null)
    :initarg :context
    :initform nil
    :reader decoder-context)))

(defvar *decoder* nil)

(defmacro with-decoder ((data &key (start 0) end context) &body body)
  (let ((data-var (gensym "DATA-")))
    `(let* ((,data-var ,data)
            (*decoder*
              (make-instance 'decoder :data ,data-var
                                      :start ,start
                                      :end (or ,end (length ,data-var))
                                      :context ,context)))
       ,@body)))

(define-condition truncated-field (error)
  ((name
    :type string
    :initarg :name
    :reader truncated-field-name)
   (size
    :type (integer 0)
    :initarg :size
    :reader truncated-field-size)
   (context
    :type (or string null)
    :initarg :context
    :initform nil
    :reader truncated-field-context))
  (:report
   (lambda (condition stream)
     (format stream "truncated field ~S (~D octets)"
             (truncated-field-name condition)
             (truncated-field-size condition))
     (let ((context (truncated-field-context condition)))
       (when context
         (format stream " in ~A" context))))))

(defmacro define-field-reader ((type-name type-size) (data start)
                               &body body)
  (let ((function-name
          (concatenate 'string "READ-FIELD/" (symbol-name type-name))))
    `(defun ,(intern function-name) (name)
       (declare (type string name))
       (when (> (+ (decoder-start *decoder*) ,type-size)
                (decoder-end *decoder*))
         (error 'truncated-field :name name :size ,type-size
                                 :context (decoder-context *decoder*)))
       (with-slots ((,data data) (,start start)) *decoder*
         (prog1
             (progn
               ,@body)
           (incf ,start ,type-size))))))

(define-field-reader (#:uint16 2) (data start)
  (core:binref :uint16be data start))

(define-field-reader (#:uint32 4) (data start)
  (core:binref :uint32be data start))

(define-field-reader (#:offset16 2) (data start)
  (core:binref :uint16be data start))

(define-field-reader (#:offset32 4) (data start)
  (core:binref :uint32be data start))

(define-field-reader (#:tag 4) (data start)
  (text:decode-string data :start start :end (+ start 4)))

(defun decode-platform-id (id)
  (declare (type (unsigned-byte 16) id))
  (case id
    (0 :unicode)
    (1 :macintosh)
    (3 :windows)
    (t id)))

(defun decode-encoding-id (id platform-id)
  (declare (type (unsigned-byte 16) id)
           (type (or symbol (unsigned-byte 16)) platform-id))
  (case platform-id
    (:unicode
     (case id
       (0 :unicode-1-0)
       (1 :unicode-1-1)
       (2 :iso-10646)
       (3 :unicode-2-0-bmp)
       (4 :unicode-2-0)
       (t id)))
    (:macintosh
     (case id
       ( 0 :roman)
       ( 1 :japanese)
       ( 2 :traditional-chinese)
       ( 3 :korean)
       ( 4 :arabic)
       ( 5 :hebrew)
       ( 6 :greek)
       ( 7 :russian)
       ( 8 :rsymbol)
       ( 9 :devanagari)
       (10 :gurmukhi)
       (11 :gujarati)
       (12 :oriya)
       (13 :bengali)
       (14 :tamil)
       (15 :telugu)
       (16 :kannada)
       (17 :malayalam)
       (18 :sinhalese)
       (19 :burmese)
       (20 :khmer)
       (21 :thai)
       (22 :laotian)
       (23 :georgian)
       (24 :armenian)
       (25 :simplified-chinese)
       (26 :tibetan)
       (27 :mongolian)
       (28 :geez)
       (29 :slavic)
       (30 :vietnamese)
       (31 :sindhi)
       (32 :uninterpreted)
       (t id)))
    (:windows
     (case id
       ( 0 :symbol)
       ( 1 :unicode-bmp)
       ( 2 :shift-jis)
       ( 3 :prc)
       ( 4 :big5)
       ( 5 :wansung)
       ( 6 :johab)
       ( 7 :reserved)
       ( 8 :reserved)
       ( 9 :reserved)
       (10 :unicode)
       (t
        id)))
    (t
     id)))

(defun decode-name-id (id)
  (declare (type (unsigned-byte 16) id))
  (case id
    ( 0 :copyright-notice)
    ( 1 :font-family-name)
    ( 2 :font-subfamily-name)
    ( 3 :unique-font-identifier)
    ( 4 :full-font-name)
    ( 5 :version-string)
    ( 6 :postscript-name)
    ( 7 :trademark)
    ( 8 :manufacturer-name)
    ( 9 :designer)
    (10 :description)
    (11 :url-vendor)
    (12 :url-designer)
    (13 :license-description)
    (14 :license-info)
    (16 :typographic-family-name)
    (17 :typographic-subfamily-name)
    (18 :compatible-full)
    (19 :sample-text)
    (20 :postscript-cid-findfont-name)
    (21 :wws-family-name)
    (22 :wws-subfamily-name)
    (23 :light-background-palette)
    (24 :dark-background-palette)
    (25 :variations-postscript-name-prefix)
    (t id)))

(defun decode-string (octets platform-id encoding-id &key (start 0) end)
  (declare (type core:octet-vector octets)
           (type (integer 0) start)
           (type (or (integer 0) null) end)
           (type (or symbol (unsigned-byte 16)) platform-id encoding-id))
  (let ((encoding (case platform-id
                    ((:unicode :windows)
                     :utf-16be)
                    (:macintosh
                     (case encoding-id
                       (:roman :macintosh))))))
    (if encoding
        (text:decode-string octets :start start :end end :encoding encoding)
        (subseq octets start end))))
