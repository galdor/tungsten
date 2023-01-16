(in-package :log)

(deftype ecma48-color ()
  '(member :black :red :green :yellow :blue :magenta :cyan :white))

(defclass terminal-sink (sink)
  ((mutex
    :type system:mutex
    :initform (system:make-mutex :name "terminal-log-sink")
    :reader terminal-sink-mutex)
   (stream
    :type stream
    :initarg :stream
    :initform *error-output*
    :reader terminal-sink-stream)
   (color
    :type boolean
    :initarg :color
    :initform t
    :reader terminal-sink-color)
   (print-length
    :type (or (integer 0) null)
    :initarg :print-length
    :initform 16
    :reader terminal-sink-print-length)))

(defun make-terminal-sink (&key (stream *error-output*)
                                (color t))
  (declare (type stream stream)
           (type boolean color))
  (make-instance 'terminal-sink :stream stream :color color))

(defmethod write-message (message (sink terminal-sink))
  (declare (type message message))
  (normalize-message-data message)
  (with-slots (mutex stream color) sink
    (system:with-mutex (mutex)
      (with-slots (domain level text-format text-arguments data) message
        (let* ((*print-length* (terminal-sink-print-length sink))
               (green (ecma48-foreground-color-sequence :green))
               (blue (ecma48-foreground-color-sequence :blue))
               (reset (ecma48-reset-sequence))
               (domain (let ((*print-case* :downcase))
                         (format nil "~24@<~{~A~^.~}~>" domain))))
          (format stream "~&~5@<~A~>  ~
                          ~:[~*~A~*~;~A~A~A~]  ~
                          ~<~@;~?~:>"
                  (string-downcase (symbol-name level))
                  color green domain reset
                  (list text-format text-arguments))
          (unless (null data)
            (format stream "~%      ")
            (dolist (datum data)
              (format stream " ~:[~*~A:~*~;~A~A:~A~]~A"
                      color blue (car datum) reset
                      (cdr datum))))
          (terpri stream)
          (finish-output stream))))))

(defun ecma48-color-code (color)
  (ecase color
    (:black 0)
    (:red 1)
    (:green 2)
    (:yellow 3)
    (:blue 4)
    (:magenta 5)
    (:cyan 6)
    (:white 7)))

(defun ecma48-foreground-color-sequence (color)
  (declare (type ecma48-color color))
  (format nil "~C[~Dm" (code-char 27) (+ 30 (ecma48-color-code color))))

(defun ecma48-reset-sequence ()
  (format nil "~C[0m" (code-char 27)))
