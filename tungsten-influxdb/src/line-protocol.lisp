(in-package :influxdb)

(defun write-points (points stream)
  (declare (type list points)
           (type stream stream))
  (dolist (point points)
    (write-point point stream)))

(defun write-point (point stream)
  (declare (type point point)
           (type stream stream))
  (with-slots (measurement tags fields timestamp) point
    (write-measurement measurement stream)
    (write-tags tags stream)
    (write-char #\Space stream)
    (write-fields fields stream)
    (when timestamp
      (write-char #\Space stream)
      (write-timestamp timestamp stream))
    (write-char #\Newline stream)
    nil))

(defun write-measurement (measurement stream)
  (declare (type string measurement)
           (type stream stream))
  (write-escaped-string measurement '(#\, #\Space) stream))

(defun write-tags (tags stream)
  (declare (type list tags)
           (type stream stream))
  ;; We sort tags as recommended in the InfluxDB documentation for performance
  ;; reasons.
  (dolist (tag (sort tags #'string< :key #'car))
    (write-char #\, stream)
    (write-key (car tag) stream)
    (write-char #\= stream)
    (write-key (cdr tag) stream)))

(defun write-fields (fields stream)
  (declare (type list fields)
           (type stream stream))
  ;; We also sort fields even though it is not required because it helps
  ;; testing.
  (let ((i 0))
    (dolist (field (sort fields #'string< :key #'car))
      (when (> i 0)
        (write-char #\, stream))
      (write-key (car field) stream)
      (write-char #\= stream)
      (write-field (cdr field) stream)
      (incf i))))

(defun write-field (field stream)
  (declare (type stream stream))
  (typecase field
    (integer
     (princ field stream))
    (real
     (format stream "~F" field))
    (boolean
     (write-string (if field "t" "f") stream))
    (string
     (write-char #\" stream)
     (write-escaped-string field '(#\") stream)
     (write-char #\" stream))
    (t
     (write-field (princ-to-string field) stream))))

(defun write-timestamp (datetime stream)
  (declare (type time:datetime datetime)
           (type stream stream))
  (princ (time:datetime-unix-timestamp datetime :unit :nanosecond) stream))

(defun write-key (key stream)
  (write-escaped-string key '(#\, #\= #\Space) stream))

(defun write-escaped-string (string special-characters stream)
  (flet ((special-character-p (character)
           (member character special-characters :test #'char=)))
    (do ((start 0)
         (end (length string)))
        ((>= start end)
         nil)
      (let* ((position (position-if #'special-character-p string :start start))
             (part-end (or position end)))
        (write-string string stream :start start :end part-end)
        (when position
          (write-char #\\ stream)
          (write-char (char string position) stream))
        (setf start (1+ part-end))))))
