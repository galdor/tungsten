(in-package :fonts)

;;; Reference: https://learn.microsoft.com/en-us/typography/opentype/spec/otff

(deftype uint8 ()
  '(unsigned-byte 8))

(deftype int8 ()
  '(signed-byte 8))

(deftype uint16 ()
  '(unsigned-byte 16))

(deftype int16 ()
  '(signed-byte 16))

(deftype uint24 ()
  '(unsigned-byte 24))

(deftype uint32 ()
  '(unsigned-byte 32))

(deftype int32 ()
  '(signed-byte 32))

;; TODO FIXED 16.16

(deftype fword ()
  'int16)

(deftype ufword ()
  'uint16)

;; TODO F2DOT14 2.14

(deftype longdatetime ()
  '(signed-byte 64))

(deftype tag ()
  '(string 4))

(deftype offset16 ()
  'uint16)

(deftype offset24 ()
  'uint24)

(deftype offset32 ()
  'uint32)

(deftype version16dot16 ()
  '(cons uint16 uint16))

(defclass table ()
  ((tag
    :type tag
    :initarg :tag
    :accessor table-tag)
   (offset
    :type offset32
    :initarg :offset
    :accessor table-offset)
   (length
    :type uint32
    :initarg :length
    :accessor table-length)))

(defmethod print-object ((table table) stream)
  (print-unreadable-object (table stream :type t)
    (format stream "~A" (table-tag table))))

(defclass name-table (table)
  ((version
    :type uint16
    :accessor name-table-version)
   (name-records
    :type list
    :accessor name-table-name-records)))

(defclass hhea-table (table)
  ((major-version
    :type uint16
    :accessor hhea-table-major-version)
   (minor-version
    :type uint16
    :accessor hhea-table-minor-version)
   (ascender
    :type fword
    :accessor hhea-table-ascender)
   (descender
    :type fword
    :accessor hhea-table-descender)
   (line-gap
    :type fword
    :accessor hhea-table-line-gap)
   (advance-width-max
    :type ufword
    :accessor hhea-table-advance-width-max)
   (min-left-side-bearing
    :type fword
    :accessor hhea-table-min-left-side-bearing)
   (min-right-side-bearing
    :type fword
    :accessor hhea-table-min-right-side-bearing)
   (x-max-extent
    :type fword
    :accessor hhea-table-x-max-extent)
   (caret-slope-rise
    :type int16
    :accessor hhea-table-caret-slope-rise)
   (caret-slope-run
    :type int16
    :accessor hhea-table-caret-slope-run)
   (caret-offset
    :type int16
    :accessor hhea-table-caret-offset)
   (metric-data-format
    :type int16
    :accessor hhea-table-metric-data-format)
   (number-of-hmetrics
    :type uint16
    :accessor hhea-table-number-of-hmetrics)))

(defclass name-record ()
  ((platform-id
    :type (or symbol uint16)
    :initarg :platform-id
    :accessor name-record-platform-id)
   (encoding-id
    :type (or symbol uint16)
    :initarg :encoding-id
    :accessor name-record-encoding-id)
   (language-id
    :type (or symbol uint16)
    :initarg :language-id
    :accessor name-record-language-id)
   (name-id
    :type (or symbol uint16)
    :initarg :name-id
    :accessor name-record-name-id)
   (value
    :type (or string core:octet-vector)
    :initarg :value
    :accessor name-record-value)))

(defclass cmap-table (table)
  ((version
     :type uint16
     :initarg :version
     :accessor cmap-table-version)
   (encoding-records
    :type list
    :accessor cmap-table-encoding-records)))

(defclass encoding-record ()
  ((platform-id
    :type (or symbol uint16)
    :initarg :platform-id
    :accessor encoding-record-platform-id)
   (encoding-id
    :type (or symbol uint16)
    :initarg :encoding-id
    :accessor encoding-record-encoding-id)
   (subtable-offset
    :type offset32
    :initarg :subtable-offset
    :accessor encoding-record-subtable-offset)))

(defmethod print-object ((record name-record) stream)
  (print-unreadable-object (record stream :type t)
    (with-slots (name-id value) record
      (format stream "~A ~S" name-id value))))

(defclass table-directory ()
  ((version
    :type uint32
    :initarg :version
    :accessor table-directory-version)
   (tables
    :type hash-table
    :initarg :tables
    :initform (make-hash-table :test #'equal)
    :accessor table-directory-tables)))

(defun table-directory-table (table-directory tag)
  (declare (type table-directory table-directory)
           (type tag tag))
  (gethash tag (table-directory-tables table-directory)))

(defclass font ()
  ((table-directory
    :type table-directory
    :initarg :table-directory
    :accessor font-table-directory)))

(defun font-table (font name)
  (declare (type font font)
           (type tag name))
  (table-directory-table (font-table-directory font) name))

(defun font-ascender (font)
  (declare (type font font))
  (hhea-table-ascender (font-table font "hhea")))

(defun font-descender (font)
  (declare (type font font))
  (hhea-table-descender (font-table font "hhea")))

(defun font-line-gap (font)
  (declare (type font font))
  (hhea-table-line-gap (font-table font "hhea")))
