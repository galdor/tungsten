(in-package :text-test)

(defmacro check-encode-string (encoding &rest checks)
  `(progn
     ,@(mapcar (lambda (check)
                 `(check:check-equalp
                   ,(car check)
                   (text:encode-string ,(cadr check) :encoding ,encoding
                                       ,@(cddr check))))
               checks)))

(defmacro check-encode-string-error (encoding &rest checks)
  `(progn
     ,@(mapcar (lambda (check)
                 `(check:check-signals
                   ,(car check)
                   (text:encode-string ,(cadr check) :encoding ,encoding
                                       ,@(cddr check))))
               checks)))

(defmacro check-decode-string (encoding &rest checks)
  `(progn
     ,@(mapcar (lambda (check)
                 `(check:check-equalp
                   ,(car check)
                   (text:decode-string
                    ,(make-array (length (cadr check))
                                 :element-type '(unsigned-byte 8)
                                 :initial-contents (cadr check))
                    :encoding ,encoding ,@(cddr check))))
               checks)))

(defmacro check-decode-string-error (encoding &rest checks)
  `(progn
     ,@(mapcar (lambda (check)
                 `(check:check-signals
                   ,(car check)
                   (text:decode-string
                    ,(make-array (length (cadr check))
                                 :element-type '(unsigned-byte 8)
                                 :initial-contents (cadr check))
                    :encoding ,encoding ,@(cddr check))))
               checks)))

(defun codepoint-string (&rest codepoints)
  (make-array (length codepoints)
              :element-type 'character
              :initial-contents (mapcar #'code-char codepoints)))
