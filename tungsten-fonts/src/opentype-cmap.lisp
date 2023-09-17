(in-package :fonts)

(defclass cmap-table (table)
  ((version
     :type uint16
     :initarg :version
     :accessor cmap-table-version)
   (encoding-records
    :type list
    :accessor cmap-table-encoding-records)))

(defun cmap-table-glyph-id (table code)
  (declare (type cmap-table table)
           (type (signed-byte 32) code))
  ;; At least for the time being we only support Unicode encoding records
  ;; since we only represent characters as Unicode codepoints.
  (let ((record (find-if (lambda (record)
                           (with-slots (platform-id encoding-id) record
                             (or (eq platform-id :unicode)
                                 (and (eq platform-id :windows)
                                      (eq encoding-id :unicode-bmp)))))
                         (cmap-table-encoding-records table))))
    (unless record
      (error "No supported encoding record found."))
    (cmap-subtable-glyph-id (encoding-record-subtable record) code)))

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
    :accessor encoding-record-subtable-offset)
   (subtable
    :type (or cmap-subtable null)
    :initarg :subtable
    :initform nil
    :accessor encoding-record-subtable)))

(defmethod print-object ((record encoding-record) stream)
  (print-unreadable-object (record stream :type t)
    (with-slots (platform-id encoding-id) record
      (format stream "~A ~A" platform-id encoding-id))))

(defclass cmap-subtable ()
  ())

(defgeneric parse-cmap-subtable (subtable))

(defgeneric cmap-subtable-glyph-id (subtable code))

(defmethod parse-table ((table cmap-table))
  (setf (cmap-table-version table) (parse-uint16 "version"))
  (let* ((table-start (table-offset table))
         (nb-encoding-records (parse-uint16 "numTables"))
         (encoding-records nil))
    (setf (cmap-table-encoding-records table)
          (dotimes (i nb-encoding-records (nreverse encoding-records))
            (push (parse-encoding-record table-start) encoding-records)))))

(defun parse-encoding-record (table-start)
  (let* ((platform-id (parse-platform-id (parse-uint16 "platformID")))
         (encoding-id
           (parse-encoding-id (parse-uint16 "encodingID") platform-id))
         (subtable-offset (parse-offset32 "subtableOffset"))
         (*parser* (with-slots (data start end table) *parser*
                     (let ((parser-table
                             (list table (list platform-id encoding-id))))
                       (make-instance 'parser
                                      :data data
                                      :start (+ table-start subtable-offset)
                                      :end end
                                      :table parser-table))))
         (subtable-format (parse-uint16 "format"))
         (subtable-length (parse-uint16 "length"))
         (subtable-class (case subtable-format
                           (4 'segment-mapping-cmap-subtable)
                           (6 'trimmed-mapping-cmap-subtable)))
         (subtable (when subtable-class
                     (let ((subtable (make-instance subtable-class)))
                       (parse-cmap-subtable subtable)
                       subtable))))
    (setf (parser-end *parser*) (+ (parser-start *parser*) -4 subtable-length))
    (make-instance 'encoding-record :platform-id platform-id
                                    :encoding-id encoding-id
                                    :subtable-offset subtable-offset
                                    :subtable subtable)))

(defclass segment-mapping-cmap-subtable (cmap-subtable)
  ((language
    :type uint16
    :accessor segment-mapping-cmap-subtable-language)
   (nb-segments
    :type (integer 0)
    :accessor segment-mapping-cmap-subtable-nb-segments)
   (start-codes
    :type (array uint16)
    :accessor segment-mapping-cmap-subtable-start-codes)
   (end-codes
    :type (array uint16)
    :accessor segment-mapping-cmap-subtable-end-codes)
   (id-deltas
    :type (array int16)
    :accessor segment-mapping-cmap-subtable-id-deltas)
   (id-range-offsets
    :type (array uint16)
    :accessor segment-mapping-cmap-subtable-id-range-offsets)
   (glyph-ids
    :type (array uint16)
    :accessor segment-mapping-cmap-subtable-glyph-ids)))

(defmethod parse-cmap-subtable ((subtable segment-mapping-cmap-subtable))
  (with-slots (language nb-segments
               start-codes end-codes id-deltas id-range-offsets glyph-ids)
      subtable
    (setf language (parse-uint16 "language"))
    (setf nb-segments (floor (parse-uint16 "segCountX2") 2))
    (parse-uint16 "searchRange")
    (parse-uint16 "entrySelector")
    (parse-uint16 "rangeShift")
    (setf end-codes (make-array nb-segments :element-type 'uint16))
    (dotimes (i nb-segments)
      (setf (aref end-codes i) (parse-uint16 "endCode")))
    (parse-uint16 "reservedPad")
    (setf start-codes (make-array nb-segments :element-type 'uint16))
    (dotimes (i nb-segments)
      (setf (aref start-codes i) (parse-uint16 "startCode")))
    (setf id-deltas (make-array nb-segments :element-type 'int16))
    (dotimes (i nb-segments)
      (setf (aref id-deltas i) (parse-int16 "idDelta")))
    (setf id-range-offsets (make-array nb-segments :element-type 'uint16))
    (dotimes (i nb-segments)
      (setf (aref id-range-offsets i) (parse-uint16 "idRangeOffset")))
    (let* ((nb-glyphs
             (floor (- (parser-end *parser*) (parser-start *parser*)) 2)))
      (setf glyph-ids (make-array nb-glyphs :element-type 'uint16))
      (dotimes (i nb-glyphs)
        (setf (aref glyph-ids i) (parse-uint16 "glyphId"))))))

(defmethod cmap-subtable-glyph-id
    ((subtable segment-mapping-cmap-subtable) code)
  (declare (type (unsigned-byte 32) code))
  (with-slots (nb-segments
               start-codes end-codes id-deltas id-range-offsets glyph-ids)
      subtable
    (dotimes (i nb-segments)
      (when (<= code (aref end-codes i))
        (return
          (let ((start-code (aref start-codes i))
                (id-delta (aref id-deltas i))
                (id-range-offset (aref id-range-offsets i)))
            (cond
              ((< code start-code)
               0)
              ((zerop id-range-offset)
               (mod (+ code id-delta) 65536))
              (t
               (let* ((offset (- (+ (/ id-range-offset 2)
                                    (- code start-code)
                                    i)
                                 nb-segments))
                      (index (aref glyph-ids offset)))
                 (mod (+ index id-delta) 65536))))))))))

(defclass trimmed-mapping-cmap-subtable (cmap-subtable)
  ((language
    :type uint16
    :accessor trimmed-mapping-cmap-subtable-language)
   (glyph-ids
    :type (array uint8 (256))
    :initform (make-array 256 :element-type 'uint8 :initial-element 0)
    :accessor trimmed-mapping-cmap-subtable-glyph-ids)))

(defmethod parse-cmap-subtable ((subtable trimmed-mapping-cmap-subtable))
  (with-slots (language glyph-ids) subtable
    (setf language (parse-uint16 "language"))
    (dotimes (code 256)
      (setf (aref glyph-ids code) (parse-uint8 "glyphId")))))

(defmethod cmap-subtable-glyph-id
    ((subtable trimmed-mapping-cmap-subtable) code)
  (declare (type (unsigned-byte 32) code))
  (with-slots (glyph-ids) subtable
    (if (< code 256)
        (aref glyph-ids code)
        0)))
