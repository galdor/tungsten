(in-package :fonts)

(define-table (head-table "head") ()
  ((major-version
    :type (unsigned-byte 16)
    :accessor head-table-major-version)
   (minor-version
    :type (unsigned-byte 16)
    :accessor head-table-minor-version)
   (font-revision
    :type short-float
    :accessor head-table-font-revision)
   (flags
    :type (unsigned-byte 16)
    :accessor head-table-flags)
   (units-per-em
    :type (unsigned-byte 16)
    :accessor head-table-units-per-em)
   (created
    :type time:datetime
    :accessor head-table-created)
   (modified
    :type time:datetime
    :accessor head-table-modified)
   (x-min
    :type (signed-byte 16)
    :accessor head-table-x-min)
   (y-min
    :type (signed-byte 16)
    :accessor head-table-y-min)
   (x-max
    :type (signed-byte 16)
    :accessor head-table-x-max)
   (y-max
    :type (signed-byte 16)
    :accessor head-table-y-max)
   (mac-style
    :type (unsigned-byte 16)
    :accessor head-table-mac-style)
   (lowest-rec-ppem
    :type (signed-byte 16)
    :accessor head-table-lowest-rec-ppem)
   (font-direction-hint
    :type (signed-byte 16)
    :accessor head-table-font-direction-hint)
   (index-to-loc-format
    :type (signed-byte 16)
    :accessor head-table-index-to-loc-format)
   (glyph-data-format
    :type (unsigned-byte 16)
    :accessor head-table-glyph-data-format)))

(defmethod decode-table-data ((table head-table) record)
  (declare (type table-record record))
  (setf (head-table-major-version table) (read-field/uint16 "majorVersion"))
  (setf (head-table-minor-version table) (read-field/uint16 "minorVersion"))
  (setf (head-table-font-revision table) (read-field/fixed "fontRevision"))
  (read-field/uint32 "checksumAdjustment")
  (read-field/uint32 "magicNumber")
  (setf (head-table-flags table) (read-field/uint16 "flags"))
  (setf (head-table-units-per-em table) (read-field/uint16 "unitsPerEm"))
  (setf (head-table-created table) (read-field/long-datetime "created"))
  (setf (head-table-modified table) (read-field/long-datetime "modified"))
  (setf (head-table-x-min table) (read-field/int16 "xMin"))
  (setf (head-table-y-min table) (read-field/int16 "yMin"))
  (setf (head-table-x-max table) (read-field/int16 "xMax"))
  (setf (head-table-y-max table) (read-field/int16 "yMax"))
  (setf (head-table-mac-style table) (read-field/uint16 "macStyle"))
  (setf (head-table-lowest-rec-ppem table) (read-field/uint16 "lowestRecPPEM"))
  (setf (head-table-font-direction-hint table)
        (read-field/uint16 "fontDirectionHint"))
  (setf (head-table-index-to-loc-format table)
        (read-field/uint16 "indexToLocFormat"))
  (setf (head-table-glyph-data-format table)
        (read-field/uint16 "glyphDataFormat"))))
