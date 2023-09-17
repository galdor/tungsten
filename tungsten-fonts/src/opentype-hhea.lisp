(in-package :fonts)

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

(defmethod parse-table ((table hhea-table))
  (setf (hhea-table-major-version table)
        (parse-uint16 "majorVersion"))
  (setf (hhea-table-minor-version table)
        (parse-uint16 "minorVersion"))
  (setf (hhea-table-ascender table)
        (parse-fword "ascender"))
  (setf (hhea-table-descender table)
        (parse-fword "descender"))
  (setf (hhea-table-line-gap table)
        (parse-fword "lineGap"))
  (setf (hhea-table-advance-width-max table)
        (parse-ufword "advanceWidthMax"))
  (setf (hhea-table-min-left-side-bearing table)
        (parse-fword "minLeftSideBearing"))
  (setf (hhea-table-min-right-side-bearing table)
        (parse-fword "minRightSideBearing"))
  (setf (hhea-table-x-max-extent table)
        (parse-fword "xMaxExtent"))
  (setf (hhea-table-caret-slope-rise table)
        (parse-int16 "caretSlopeRise"))
  (setf (hhea-table-caret-slope-run table)
        (parse-int16 "caretSlopeRun"))
  (setf (hhea-table-caret-offset table)
        (parse-int16 "caretOffset"))
  (parse-int16 "reserved")
  (parse-int16 "reserved")
  (parse-int16 "reserved")
  (parse-int16 "reserved")
  (setf (hhea-table-metric-data-format table)
        (parse-int16 "metricDataFormat"))
  (setf (hhea-table-number-of-hmetrics table)
        (parse-uint16 "numberOfHMetrics")))
