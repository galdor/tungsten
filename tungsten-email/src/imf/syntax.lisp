(in-package :imf)

(defun atom-p (string)
  (declare (type string string))
  (and (> (length string) 0)
       (every 'atext-char-p string)))

(defun dot-atom-p (string)
  (declare (type string string))
  (every 'atom-p (core:split-string string #\.)))

(defun atext-char-p (c)
  (declare (type character c))
  (or (char<= #\A c #\Z)
      (char<= #\a c #\z)
      (char<= #\0 c #\9)
      (member c '#.(coerce "!#$%&'*+-/=?^_`{|}~" 'list) :test #'char=)))
