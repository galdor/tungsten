(in-package :fonts)

(defvar *parser* nil)

(define-condition font-parse-error (error)
  ())

(define-condition truncated-field (font-parse-error)
  ((name
    :type string
    :initarg :name
    :reader truncated-field-name)
   (type
    :type symbol
    :initarg :type
    :reader truncated-field-type))
  (:report
   (lambda (condition stream)
     (with-slots (name type) condition
       (format stream "Truncated field ~A of type ~A." name type)))))

(define-condition invalid-tag-octet (font-parse-error)
  ((octet
    :type uint8
    :initarg :octet
    :reader invalid-tag-octet-octet)
   (offset
    :type (integer 0)
    :initarg :offset
    :reader invalid-tag-octet-offset))
  (:report
   (lambda (condition stream)
     (with-slots (octet offset) condition
       (format stream "Invalid tag octet ~D at offset ~D." octet offset)))))

(defclass parser ()
  ((data
    :type core:octet-vector
    :initarg :data
    :reader parser-data)
   (start
    :type (integer 0)
    :initarg :start
    :reader parser-start)
   (end
    :type (integer 0)
    :initarg :end
    :reader parser-end)
   (table
    :type (or tag null)
    :initarg :table
    :accessor parser-table)))

(defun load-font (path)
  (declare (type (or pathname string) path))
  (parse-font (system:read-file path)))

(defun parse-font (data)
  (declare (type core:octet-vector data))
  (let* ((*parser*
           (make-instance 'parser :data data :start 0 :end (length data)))
         (table-directory (parse-table-directory)))
    (make-instance 'font :table-directory table-directory)))

(defun parse-table-directory ()
  (let* ((version (parse-uint32 "sfntVersion"))
         (nb-tables (parse-uint16 "numTables"))
         (tables (make-hash-table :test #'equal)))
    (parse-uint16 "searchRange")
    (parse-uint16 "entrySelector")
    (parse-uint16 "rangeShift")
    (dotimes (i nb-tables)
      (let* ((tag (parse-tag "tableTag"))
             (checksum (parse-uint32 "checksum"))
             (offset (parse-offset32 "offset"))
             (length (parse-uint32 "length")))
        (declare (ignore checksum))
        (let ((*parser* (with-slots (data start end) *parser*
                          (make-instance 'parser :data data
                                                 :start offset
                                                 :end (+ offset length)
                                                 :table tag)))
              (table-class (core:string-case tag
                             ("hhea" 'hhea-table)
                             ("name" 'name-table))))
          (when table-class
            (let ((table (make-instance table-class :tag tag
                                                    :offset offset
                                                    :length length)))
              (parse-table table)
              (setf (gethash (table-tag table) tables) table))))))
    (make-instance 'table-directory :version version :tables tables)))

(defgeneric parse-table (table))

(defun parse-platform-id (id)
  (declare (type uint16 id))
  (case id
    (0 'unicode)
    (1 'macintosh)
    (3 'windows)
    (t id)))

(defun parse-encoding-id (id platform-id)
  (declare (type uint16 id)
           (type (or symbol uint16) platform-id))
  (case platform-id
    (unicode
     (case id
       (0 'unicode-1-0)
       (1 'unicode-1-1)
       (2 'iso-10646)
       (3 'unicode-2-0-bmp)
       (4 'unicode-2-0)
       (t id)))
    (macintosh
     (case id
       ( 0 'roman)
       ( 1 'japanese)
       ( 2 'traditional-chinese)
       ( 3 'korean)
       ( 4 'arabic)
       ( 5 'hebrew)
       ( 6 'greek)
       ( 7 'russian)
       ( 8 'rsymbol)
       ( 9 'devanagari)
       (10 'gurmukhi)
       (11 'gujarati)
       (12 'oriya)
       (13 'bengali)
       (14 'tamil)
       (15 'telugu)
       (16 'kannada)
       (17 'malayalam)
       (18 'sinhalese)
       (19 'burmese)
       (20 'khmer)
       (21 'thai)
       (22 'laotian)
       (23 'georgian)
       (24 'armenian)
       (25 'simplified-chinese)
       (26 'tibetan)
       (27 'mongolian)
       (28 'geez)
       (29 'slavic)
       (30 'vietnamese)
       (31 'sindhi)
       (32 'uninterpreted)
       (t id)))
    (windows
     (case id
       ( 0 'symbol)
       ( 1 'unicode-bmp)
       ( 2 'shift-jis)
       ( 3 'prc)
       ( 4 'big5)
       ( 5 'wansung)
       ( 6 'johab)
       ( 7 'reserved)
       ( 8 'reserved)
       ( 9 'reserved)
       (10 'unicode)
       (t
        id)))
    (t
     id)))

(defun parse-name-id (id)
  (declare (type uint16 id))
  (case id
    ( 0 'copyright-notice)
    ( 1 'font-family-name)
    ( 2 'font-subfamily-name)
    ( 3 'unique-font-identifier)
    ( 4 'full-font-name)
    ( 5 'version-string)
    ( 6 'postscript-name)
    ( 7 'trademark)
    ( 8 'manufacturer-name)
    ( 9 'designer)
    (10 'description)
    (11 'url-vendor)
    (12 'url-designer)
    (13 'license-description)
    (14 'license-info)
    (16 'typographic-family-name)
    (17 'typographic-subfamily-name)
    (18 'compatible-full)
    (19 'sample-text)
    (20 'postscript-cid-findfont-name)
    (21 'wws-family-name)
    (22 'wws-subfamily-name)
    (23 'light-background-palette)
    (24 'dark-background-palette)
    (25 'variations-postscript-name-prefix)
    (t id)))

(defun parse-uint8 (name)
  (declare (type string name))
  (parse-integer-field name :uint8))

(defun parse-uint16 (name)
  (declare (type string name))
  (parse-integer-field name :uint16be))

(defun parse-int16 (name)
  (declare (type string name))
  (parse-integer-field name :int16be))

(defun parse-uint32 (name)
  (declare (type string name))
  (parse-integer-field name :uint32be))

(defun parse-offset16 (name)
  (declare (type string name))
  (parse-integer-field name :uint16be))

(defun parse-fword (name)
  (declare (type string name))
  (parse-integer-field name :int16be))

(defun parse-ufword (name)
  (declare (type string name))
  (parse-integer-field name :uint16be))

(defun parse-offset32 (name)
  (declare (type string name))
  (parse-integer-field name :uint32be))

(defun parse-integer-field (name type)
  (declare (type string name)
           (type symbol type))
  (with-slots (data start end) *parser*
    (let ((size (core:binref-type-size type)))
      (unless (<= (+ start size) end)
        (error 'truncated-field :name name :type type))
      (prog1 (core:binref type data start)
        (incf start size)))))

(defun parse-tag (name)
  (declare (type string name))
  (with-slots (start end) *parser*
    (unless (< (+ start 4) end)
      (error 'truncated-field :name name :type 'tag))
    (flet ((parse-tag-octet ()
             (let ((octet (parse-uint8 name)))
               (unless (<= #x20 octet #x7e)
                 (error 'invalid-tag-octet :octet octet :offset start))
               octet)))
      (let* ((a (parse-tag-octet))
             (b (parse-tag-octet))
             (c (parse-tag-octet))
             (d (parse-tag-octet))
             (content (vector (code-char a) (code-char b)
                              (code-char c) (code-char d))))
        (make-array 4 :element-type 'base-char :initial-contents content)))))

(defun decode-string (octets platform-id encoding-id
                      &key (start 0) (end (length octets)))
  (declare (type core:octet-vector octets)
           (type (integer 0) start end)
           (type (or uint16 symbol) platform-id encoding-id))
  (let ((encoding (case platform-id
                    ((unicode windows) :utf-16be)
                    (macintosh
                     (case encoding-id
                       (roman :macintosh))))))
    (if encoding
        (text:decode-string octets :start start :end end :encoding encoding)
        (subseq octets start end))))
