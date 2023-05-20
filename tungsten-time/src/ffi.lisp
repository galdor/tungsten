(in-package :time)

(defun clock-gettime (clock-id)
  (ffi:with-foreign-value (%timespec 'timespec)
    (system:system-funcall
     ("clock_gettime" ((clock-type :pointer) :int) clock-id %timespec))
    (ffi:with-foreign-structure-members (((seconds :tv-sec)
                                          (nanoseconds :tv-nsec))
                                         %timespec 'timespec)
      (values seconds nanoseconds))))
