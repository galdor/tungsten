(in-package :core-test)

(defmacro check-binref (value type octets &optional (offset 0))
  `(check= ,value (core:binref ,type ,(core:octet-vector octets) ,offset)))

;;;
;;; 8 bit integers
;;;

(deftest binref/uint8 ()
  (check-binref 0 :uint8 #(0))
  (check-binref 127 :uint8 #(127))
  (check-binref 255 :uint8 #(255))
  (check-binref 1 :uint8 #(0 1) 1)
  (let ((data (core:make-octet-vector 3)))
    (setf (core:binref :uint8 data 0) 0)
    (setf (core:binref :uint8 data 1) 127)
    (setf (core:binref :uint8 data 2) 255)
    (check-equalp (core:octet-vector* 0 127 255) data)))

(deftest binref/int8 ()
  (check-binref 0 :int8 #(0))
  (check-binref -128 :int8 #(128))
  (check-binref 127 :int8 #(127))
  (check-binref -1 :int8 #(0 255) 1)
  (let ((data (core:make-octet-vector 3)))
    (setf (core:binref :int8 data 0) 0)
    (setf (core:binref :int8 data 1) -128)
    (setf (core:binref :int8 data 2) 127)
    (check-equalp (core:octet-vector* 0 128 127) data)))

;;;
;;; 16 bit integers
;;;

(deftest binref/uint16be ()
  (check-binref 0 :uint16be #(0 0))
  (check-binref 32767 :uint16be #(127 255))
  (check-binref 32768 :uint16be #(128 0))
  (check-binref 65535 :uint16be #(255 255))
  (check-binref 65280 :uint16be #(255 255 0) 1)
  (let ((data (core:make-octet-vector 8)))
    (setf (core:binref :uint16be data 0) 0)
    (setf (core:binref :uint16be data 2) 32767)
    (setf (core:binref :uint16be data 4) 32768)
    (setf (core:binref :uint16be data 6) 65535)
    (check-equalp (core:octet-vector* 0 0
                                      127 255
                                      128 0
                                      255 255)
                  data)))

(deftest binref/uint16le ()
  (check-binref 0 :uint16le #(0 0))
  (check-binref 32767 :uint16le #(255 127))
  (check-binref 32768 :uint16le #(0 128))
  (check-binref 65535 :uint16le #(255 255))
  (check-binref 65280 :uint16le #(255 0 255) 1)
  (let ((data (core:make-octet-vector 8)))
    (setf (core:binref :uint16le data 0) 0)
    (setf (core:binref :uint16le data 2) 32767)
    (setf (core:binref :uint16le data 4) 32768)
    (setf (core:binref :uint16le data 6) 65535)
    (check-equalp (core:octet-vector* 0 0
                                      255 127
                                      0 128
                                      255 255)
                  data)))

(deftest binref/int16be ()
  (check-binref 0 :int16be #(0 0))
  (check-binref -32768 :int16be #(128 0))
  (check-binref 32767 :int16be #(127 255))
  (check-binref -1 :int16be #(0 255 255) 1)
  (let ((data (core:make-octet-vector 8)))
    (setf (core:binref :int16be data 0) 0)
    (setf (core:binref :int16be data 2) -32768)
    (setf (core:binref :int16be data 4) 32767)
    (setf (core:binref :int16be data 6) -1)
    (check-equalp (core:octet-vector* 0 0
                                      128 0
                                      127 255
                                      255 255)
                  data)))

(deftest binref/int16le ()
  (check-binref 0 :int16le #(0 0))
  (check-binref -32768 :int16le #(0 128))
  (check-binref 32767 :int16le #(255 127))
  (check-binref -1 :int16le #(0 255 255) 1)
  (let ((data (core:make-octet-vector 8)))
    (setf (core:binref :int16le data 0) 0)
    (setf (core:binref :int16le data 2) -32768)
    (setf (core:binref :int16le data 4) 32767)
    (setf (core:binref :int16le data 6) -1)
    (check-equalp (core:octet-vector* 0 0
                                      0 128
                                      255 127
                                      255 255)
                  data)))

;;;
;;; 32 bit integers
;;;

(deftest binref/uint32be ()
  (check-binref 0 :uint32be #(0 0 0 0))
  (check-binref 16909060 :uint32be #(1 2 3 4))
  (check-binref 4294967295 :uint32be #(255 255 255 255))
  (check-binref 4278255360 :uint32be #(0 255 0 255 0) 1)
  (let ((data (core:make-octet-vector 12)))
    (setf (core:binref :uint32be data 0) 0)
    (setf (core:binref :uint32be data 4) 16909060)
    (setf (core:binref :uint32be data 8) 4294967295)
    (check-equalp (core:octet-vector* 0 0 0 0
                                      1 2 3 4
                                      255 255 255 255)
                  data)))

(deftest binref/uint32le ()
  (check-binref 0 :uint32le #(0 0 0 0))
  (check-binref 67305985 :uint32le #(1 2 3 4))
  (check-binref 4294967295 :uint32le #(255 255 255 255))
  (check-binref 16711935 :uint32le #(0 255 0 255 0) 1)
  (let ((data (core:make-octet-vector 12)))
    (setf (core:binref :uint32le data 0) 0)
    (setf (core:binref :uint32le data 4) 67305985)
    (setf (core:binref :uint32le data 8) 4294967295)
    (check-equalp (core:octet-vector* 0 0 0 0
                                      1 2 3 4
                                      255 255 255 255)
                  data)))

(deftest binref/int32be ()
  (check-binref 0 :int32be #(0 0 0 0))
  (check-binref -2147483648 :int32be #(128 0 0 0))
  (check-binref 2147483647 :int32be #(127 255 255 255))
  (check-binref -1 :int32be #(0 255 255 255 255) 1)
  (let ((data (core:make-octet-vector 16)))
    (setf (core:binref :int32be data 0) 0)
    (setf (core:binref :int32be data 4) -2147483648)
    (setf (core:binref :int32be data 8) 2147483647)
    (setf (core:binref :int32be data 12) -1)
    (check-equalp (core:octet-vector* 0 0 0 0
                                      128 0 0 0
                                      127 255 255 255
                                      255 255 255 255)
                  data)))

(deftest binref/int32le ()
  (check-binref 0 :int32le #(0 0 0 0))
  (check-binref -2147483648 :int32le #(0 0 0 128))
  (check-binref 2147483647 :int32le #(255 255 255 127))
  (check-binref -1 :int32le #(0 255 255 255 255) 1)
  (let ((data (core:make-octet-vector 16)))
    (setf (core:binref :int32le data 0) 0)
    (setf (core:binref :int32le data 4) -2147483648)
    (setf (core:binref :int32le data 8) 2147483647)
    (setf (core:binref :int32le data 12) -1)
    (check-equalp (core:octet-vector* 0 0 0 0
                                      0 0 0 128
                                      255 255 255 127
                                      255 255 255 255)
                  data)))

;;;
;;; 64 bit integers
;;;

(deftest binref/uint64be ()
  (check-binref 0 :uint64be #(0 0 0 0 0 0 0 0))
  (check-binref 72623859790382856 :uint64be #(1 2 3 4 5 6 7 8))
  (check-binref 18446744073709551615 :uint64be #(255 255 255 255 255 255 255 255))
  (check-binref 18374966857284222976 :uint64be #(0 255 0 255 0 128 0 128 0) 1)
  (let ((data (core:make-octet-vector 24)))
    (setf (core:binref :uint64be data 0) 0)
    (setf (core:binref :uint64be data 8) 72623859790382856)
    (setf (core:binref :uint64be data 16) 18446744073709551615)
    (check-equalp (core:octet-vector* 0 0 0 0 0 0 0 0
                                      1 2 3 4 5 6 7 8
                                      255 255 255 255 255 255 255 255)
                  data)))

(deftest binref/uint64le ()
  (check-binref 0 :uint64le #(0 0 0 0 0 0 0 0))
  (check-binref 578437695752307201 :uint64le #(1 2 3 4 5 6 7 8))
  (check-binref 18446744073709551615 :uint64le #(255 255 255 255 255 255 255 255))
  (check-binref 36029346791489791 :uint64le #(0 255 0 255 0 128 0 128 0) 1)
  (let ((data (core:make-octet-vector 24)))
    (setf (core:binref :uint64le data 0) 0)
    (setf (core:binref :uint64le data 8) 578437695752307201)
    (setf (core:binref :uint64le data 16) 18446744073709551615)
    (check-equalp (core:octet-vector* 0 0 0 0 0 0 0 0
                                      1 2 3 4 5 6 7 8
                                      255 255 255 255 255 255 255 255)
                  data)))

(deftest binref/int64be ()
  (check-binref 0 :int64be #(0 0 0 0 0 0 0 0))
  (check-binref -9223372036854775808 :int64be #(128 0 0 0 0 0 0 0))
  (check-binref 9223372036854775807 :int64be #(127 255 255 255 255 255 255 255))
  (check-binref -1 :int64be #(0 255 255 255 255 255 255 255 255) 1)
  (let ((data (core:make-octet-vector 24)))
    (setf (core:binref :int64be data 0) 0)
    (setf (core:binref :int64be data 8) -9223372036854775808)
    (setf (core:binref :int64be data 16) 9223372036854775807)
    (check-equalp (core:octet-vector* 0 0 0 0 0 0 0 0
                                      128 0 0 0 0 0 0 0
                                      127 255 255 255 255 255 255 255)
                  data)))

(deftest binref/int64le ()
  (check-binref 0 :int64le #(0 0 0 0 0 0 0 0))
  (check-binref -9223372036854775808 :int64le #(0 0 0 0 0 0 0 128))
  (check-binref 9223372036854775807 :int64le #(255 255 255 255 255 255 255 127))
  (check-binref -1 :int64le #(0 255 255 255 255 255 255 255 255) 1)
  (let ((data (core:make-octet-vector 24)))
    (setf (core:binref :int64le data 0) 0)
    (setf (core:binref :int64le data 8) -9223372036854775808)
    (setf (core:binref :int64le data 16) 9223372036854775807)
    (check-equalp (core:octet-vector* 0 0 0 0 0 0 0 0
                                      0 0 0 0 0 0 0 128
                                      255 255 255 255 255 255 255 127)
                  data)))
