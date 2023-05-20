(in-package :ffi-test)

(deftest structures/packed ()
  (ffi:with-foreign-value (ptr 'test-struct-packed)
    (setf (ffi:foreign-structure-member ptr 'test-struct-packed :a) 1
          (ffi:foreign-structure-member ptr 'test-struct-packed :b) 2
          (ffi:foreign-structure-member ptr 'test-struct-packed :c) 3)
    (ffi:foreign-funcall "ffi_test_struct_packed_x2" ((:pointer) :void) ptr)
    (check= 2 (ffi:foreign-structure-member ptr 'test-struct-packed :a))
    (check= 4 (ffi:foreign-structure-member ptr 'test-struct-packed :b))
    (check= 6 (ffi:foreign-structure-member ptr 'test-struct-packed :c))))

(deftest structures/padding ()
  (ffi:with-foreign-value (ptr 'test-struct-padding)
    (setf (ffi:foreign-structure-member ptr 'test-struct-padding :a) 1
          (ffi:foreign-structure-member ptr 'test-struct-padding :b) 2
          (ffi:foreign-structure-member ptr 'test-struct-padding :c) 3)
    (ffi:foreign-funcall "ffi_test_struct_padding_x2" ((:pointer) :void) ptr)
    (check= 2 (ffi:foreign-structure-member ptr 'test-struct-padding :a))
    (check= 4 (ffi:foreign-structure-member ptr 'test-struct-padding :b))
    (check= 6 (ffi:foreign-structure-member ptr 'test-struct-padding :c))))

(deftest structures/arrays ()
  (ffi:with-foreign-value (ptr 'test-struct-arrays)
    (setf (ffi:foreign-structure-member ptr 'test-struct-arrays :a 0) 1
          (ffi:foreign-structure-member ptr 'test-struct-arrays :a 1) 2)
    (setf (ffi:foreign-structure-member ptr 'test-struct-arrays :b 0) 1.5f0
          (ffi:foreign-structure-member ptr 'test-struct-arrays :b 1) 2.5f0
          (ffi:foreign-structure-member ptr 'test-struct-arrays :b 2) 3.5f0)
    (ffi:foreign-funcall "ffi_test_struct_arrays_x2" ((:pointer) :void) ptr)
    (check= 2 (ffi:foreign-structure-member ptr 'test-struct-arrays :a 0))
    (check= 4 (ffi:foreign-structure-member ptr 'test-struct-arrays :a 1))
    (check= 3.0 (ffi:foreign-structure-member ptr 'test-struct-arrays :b 0))
    (check= 5.0 (ffi:foreign-structure-member ptr 'test-struct-arrays :b 1))
    (check= 7.0 (ffi:foreign-structure-member ptr 'test-struct-arrays :b 2))))

(deftest structures/nested ()
  (ffi:with-foreign-values ((ptr 'test-struct-nested)
                            (nptr 'test-struct-packed))
    (let ((sptr
            (ffi:foreign-structure-member-pointer ptr 'test-struct-nested :s)))
      (setf (ffi:foreign-structure-member sptr 'test-struct-packed :a) 1
            (ffi:foreign-structure-member sptr 'test-struct-packed :b) 2
            (ffi:foreign-structure-member sptr 'test-struct-packed :c) 3))
    (let ((s2ptr0
            (ffi:foreign-structure-member-pointer ptr 'test-struct-nested :s2
                                                  0)))
      (setf (ffi:foreign-structure-member s2ptr0 'test-struct-padding :a) 10
            (ffi:foreign-structure-member s2ptr0 'test-struct-padding :b) 20
            (ffi:foreign-structure-member s2ptr0 'test-struct-padding :c) 30))
    (let ((s2ptr1
            (ffi:foreign-structure-member-pointer ptr 'test-struct-nested :s2
                                                  1)))
      (setf (ffi:foreign-structure-member s2ptr1 'test-struct-padding :a) 40
            (ffi:foreign-structure-member s2ptr1 'test-struct-padding :b) 50
            (ffi:foreign-structure-member s2ptr1 'test-struct-padding :c) 60))
    (setf (ffi:foreign-structure-member nptr 'test-struct-packed :a) -1
          (ffi:foreign-structure-member nptr 'test-struct-packed :b) -2
          (ffi:foreign-structure-member nptr 'test-struct-packed :c) -3)
    (setf (ffi:foreign-structure-member ptr 'test-struct-nested :ps) nptr)
    (ffi:foreign-funcall "ffi_test_struct_nested_x2" ((:pointer) :void) ptr)
    (check= -2 (ffi:foreign-structure-member nptr 'test-struct-packed :a))
    (check= -4 (ffi:foreign-structure-member nptr 'test-struct-packed :b))
    (check= -6 (ffi:foreign-structure-member nptr 'test-struct-packed :c))
    (let ((sptr
            (ffi:foreign-structure-member-pointer ptr 'test-struct-nested :s)))
      (check= 2 (ffi:foreign-structure-member sptr 'test-struct-packed :a))
      (check= 4 (ffi:foreign-structure-member sptr 'test-struct-packed :b))
      (check= 6 (ffi:foreign-structure-member sptr 'test-struct-packed :c)))
    (let ((s2ptr0
            (ffi:foreign-structure-member-pointer ptr 'test-struct-nested :s2
                                                  0)))
      (check= 20 (ffi:foreign-structure-member s2ptr0 'test-struct-padding :a))
      (check= 40 (ffi:foreign-structure-member s2ptr0 'test-struct-padding :b))
      (check= 60
              (ffi:foreign-structure-member s2ptr0 'test-struct-padding :c)))
    (let ((s2ptr1
            (ffi:foreign-structure-member-pointer ptr 'test-struct-nested :s2
                                                  1)))
      (check= 80 (ffi:foreign-structure-member s2ptr1 'test-struct-padding :a))
      (check= 100
              (ffi:foreign-structure-member s2ptr1 'test-struct-padding :b))
      (check= 120
              (ffi:foreign-structure-member s2ptr1 'test-struct-padding :c)))))
