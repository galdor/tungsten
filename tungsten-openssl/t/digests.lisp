(in-package :openssl-test)

(defmacro check-compute-digest (algorithm &rest checks)
  `(progn
     ,@(mapcar (lambda (check)
                 `(check-string=
                   ,(car check)
                   (text:encode-hex-string
                    (openssl:compute-digest ,(cadr check) ,algorithm))))
               checks)))

(deftest compute-digest/md5 ()
  ;; See RFC 1321 A.5 Test suite
  (check-compute-digest
   :md5
   ("d41d8cd98f00b204e9800998ecf8427e" "")
   ("0cc175b9c0f1b6a831c399e269772661" "a")
   ("900150983cd24fb0d6963f7d28e17f72" "abc")
   ("f96b697d7cb7938d525a2f31aaf161d0" "message digest")
   ("c3fcd3d76192e4007dfb496cca67e13b" "abcdefghijklmnopqrstuvwxyz")
   ("d174ab98d277d9f5a5611c2c9f419d9f"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")))

(deftest compute-digest/sha1 ()
  ;; See RFC 3174 7.3 Test Driver
  (check-compute-digest
   :sha1
   ("a9993e364706816aba3e25717850c26c9cd0d89d"
    "abc")
   ("84983e441c3bd26ebaae4aa1f95129e5e54670f1"
    "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
   ("34aa973cd4c4daa4f61eeb2bdbad27316534016f"
    (make-array 1000000 :element-type 'character :initial-element #\a))
   ("dea356a2cddd90c7a7ecedc5ebb563934f460452"
    (apply #'concatenate 'string
           (make-list
            20 :initial-element "01234567012345670123456701234567")))))

(deftest compute-digest/sha256 ()
  ;; See RFC 4634 8.4 The Test driver
  (check-compute-digest
   :sha256
   ("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    "abc")
   ("248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
    "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
   ("cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0"
    (make-array 1000000 :element-type 'character :initial-element #\a))
   ("594847328451bdfa85056225462cc1d867d877fb388df0ce35f25ab5562bfbb5"
    (apply #'concatenate 'string
           (make-list
            20 :initial-element "01234567012345670123456701234567")))))

