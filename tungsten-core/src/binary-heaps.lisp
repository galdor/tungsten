(in-package :core)

(defclass binary-heap ()
  ((elements
    :type vector
    :accessor binary-help-elements)
   (element-type
    :initarg :element-type
    :reader binary-heap-element-type)
   (key
    :type (or symbol function)
    :initarg :key
    :reader binary-heap-key)
   (test
    :type (or symbol function)
    :initarg :test
    :reader binary-heap-test)
   (predicate
    :type (or symbol function)
    :initarg :predicate
    :reader binary-heap-predicate)))

(defun make-binary-heap (predicate &key (element-type t) key (test #'eql))
  (let ((heap (make-instance 'binary-heap :predicate predicate
                                          :element-type element-type
                                          :key key :test test)))
    (setf (binary-help-elements heap)
          (make-array 0 :element-type element-type
                        :adjustable t :fill-pointer 0))
    heap))

(defun binary-heap-size (heap)
  (declare (type binary-heap heap))
  (with-slots (elements) heap
    (length elements)))

(defun binary-heap-add (heap element)
  (declare (type binary-heap heap))
  (with-slots (elements) heap
    (vector-push-extend element elements)
    (binary-heap-bubble-up heap (1- (length elements)))))

(defun binary-heap-remove (heap element)
  (declare (type binary-heap heap))
  (with-slots (elements key test) heap
    (let ((nb-elements (length elements))
          (i (position element elements :key key :test test)))
      (cond
        ((null i)
         nil)
        ((zerop i)
         (binary-heap-pop heap)
         t)
        (t
         (when (< i (1- nb-elements))
           (setf (aref elements i) (aref elements (1- nb-elements))))
         (let ((parent-i (floor (1- i) 2)))
           (if (binary-heap-compare heap i parent-i)
               (binary-heap-sink-down heap i)
               (binary-heap-bubble-up heap i)))
         (decf nb-elements)
         (setf (fill-pointer elements) nb-elements)
         (setf elements (adjust-array elements nb-elements))
         t)))))

(defun binary-heap-peek (heap)
  (declare (type binary-heap heap))
  (with-slots (elements) heap
    (when (> (length elements) 0)
      (aref elements 0))))

(defun binary-heap-pop (heap)
  (declare (type binary-heap heap))
  (with-slots (elements) heap
    (let ((nb-elements (length elements)))
      (when (> nb-elements 0)
        (let ((element (aref elements 0)))
          (when (> nb-elements 1)
            (setf (aref elements 0) (aref elements (1- nb-elements))))
          (decf nb-elements)
          (setf (fill-pointer elements) nb-elements)
          (setf elements (adjust-array elements nb-elements))
          (when (> nb-elements 0)
            (binary-heap-sink-down heap 0))
          element)))))

(defun binary-heap-contains (heap element)
  (declare (type binary-heap heap))
  (with-slots (elements key test) heap
    (member element elements :key key :test test)))

(defun binary-heap-element (heap i)
  (declare (type binary-heap heap)
           (type (integer 0) i))
  (with-slots (elements key) heap
    (let ((element (aref elements i)))
      (if key
          (funcall key element)
          element))))

(defun binary-heap-compare (heap i j)
  (declare (type binary-heap heap)
           (type (integer 0) i j))
  (with-slots (elements predicate) heap
    (funcall predicate
             (binary-heap-element heap i)
             (binary-heap-element heap j))))

(defun binary-heap-bubble-up (heap i)
  (declare (type binary-heap heap)
           (type (integer 0) i))
  (with-slots (elements) heap
    (loop
      (when (zerop i)
        (return))
      (let ((parent-i (floor (1- i) 2)))
        (when (binary-heap-compare heap parent-i i)
          (return))
        (rotatef (aref elements i) (aref elements parent-i))
        (setf i parent-i)))))

(defun binary-heap-sink-down (heap i)
  (declare (type binary-heap heap)
           (type (integer 0) i))
  (with-slots (elements) heap
    (loop
      (let ((left-i  (+ (* i 2) 1))
            (right-i (+ (* i 2) 2))
            (min-i i))
        (when (< left-i (length elements))
          (when (binary-heap-compare heap left-i min-i)
            (setf min-i left-i)))
        (when (< right-i (length elements))
          (when (binary-heap-compare heap right-i min-i)
            (setf min-i right-i)))
        (when (= min-i i)
          (return))
        (rotatef (aref elements i) (aref elements min-i))
        (setf i min-i)))))
