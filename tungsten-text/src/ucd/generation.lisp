(in-package :ucd)

;;; Including entire datasets such as *CHARACTERS* in the generated file is
;;; not an option: it would use way too much memory. So we provide useful
;;; information in a way that is both reasonably compact and efficient to
;;; access.
;;;
;;; General categories
;;;
;;; There are currently about 160k characters, but they are ordered in a way
;;; that there are large blocks of characters with the same general category.
;;; There are about 3k of these blocks. Storing start code point, end code
;;; point and general category requires 6 bytes (21 bits for each code point
;;; and 5 bits for the general category since there are currently 29 of them).
;;; Using 8 bytes makes our life easier.
;;;
;;; 64       56       48       40       32       24       16        8        0
;;;  +--------+--------+--------+--------+--------+--------+--------+--------+
;;;  |     START CODE POINT     |      END CODE POINT      |     CATEGORY    |
;;;  +--------+--------+--------+--------+--------+--------+--------+--------+
;;;
;;; Therefore we can represent the general category table with about 24KB.

(defun generate (package)
  (declare (type (or keyword string) package))
  (load-character-data)
  (with-standard-io-syntax
    (let ((*package* (find-package package)))
      (print `(in-package ,package))
      (generate/general-categories))))

(defun generate/general-categories ()
  (multiple-value-bind (table categories)
      (character-general-category-table)
    (print `(setf unicode::*character-general-categories*
                  (make-array ,(length table)
                              :element-type '(unsigned-byte 64)
                              :initial-contents ,table)))
    (let ((ascii-table (mapcar (lambda (code)
                                 (character-data-general-category
                                  (find-character-data code)))
                               (core:iota 128))))
      (print `(setf unicode::*character-general-categories-ascii*
                    (make-array 128 :element-type 'string
                                    :initial-contents ',ascii-table))))
    (print `(setf unicode::*general-categories*
                  (make-array ,(length categories)
                              :element-type 'string
                              :initial-contents ,categories))))
  nil)

(defun character-general-category-table ()
  (let ((table (make-array 0 :element-type '(unsigned-byte 64)
                             :adjustable t :fill-pointer 0))
        (categories (make-array 0 :element-type 'string
                                  :adjustable t :fill-pointer 0)))
    (dolist (block (character-general-category-blocks))
      (destructuring-bind ((start . end) . category) block
        (let ((category-id (or (position category categories :test #'string=)
                               (progn (vector-push-extend category categories)
                                      (1- (length categories)))))
              (entry 0))
          (setf (ldb (byte 24 40) entry) start
                (ldb (byte 24 16) entry) end
                (ldb (byte 16  0) entry) category-id)
          (vector-push-extend entry table))))
    (values (adjust-array table (length table))
            (adjust-array categories (length categories)))))

(defun character-general-category-blocks ()
  (let ((blocks nil)
        (block-start nil)
        (block-end nil)
        (block-category nil))
    (flet ((push-block ()
             (push (cons (cons block-start block-end) block-category)
                   blocks)))
      (dotimes (code-point (length *characters*) (nreverse blocks))
        (let* ((data (aref *characters* code-point))
               (category (when data (character-data-general-category data))))
          (cond
            ((and (null block-start) data)
             (setf block-start code-point
                   block-end code-point
                   block-category category))
            ((and block-start (null data))
             (push-block)
             (setf block-start nil
                   block-end nil
                   block-category nil))
            ((and block-start (eq category block-category))
             (setf block-end code-point)
             (when (eql code-point (1- (length *characters*)))
               (push-block)))
            ((and block-start (not (eq category block-category)))
             (push-block)
             (setf block-start code-point
                   block-end code-point
                   block-category category))))))))
