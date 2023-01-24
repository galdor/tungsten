(in-package :openssl-test)

(defmacro check-compute-hmac (algorithm &rest checks)
  `(progn
     ,@(mapcar (lambda (check)
                 `(check:check-string=
                   ,(car check)
                   (text:encode-hex-string
                    (openssl:compute-hmac
                     (text:decode-hex-string ,(second check))
                     (text:encode-string ,(third check))
                     ,algorithm))))
               checks)))

(deftest compute-hmac/sha1 ()
  ;; See RFC 2202 3. Test Cases for HMAC-SHA-1
  (check-compute-hmac
   :sha1
   ("b617318655057264e28bc0b6fb378c8ef146be00"
    "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"
    "Hi There")
   ("effcdf6ae5eb2fa2d27416d5f184df9c259a7c79"
    (text:encode-hex-string (text:encode-string "Jefe"))
    "what do ya want for nothing?")
   ("4c1a03424b55e07fe7f27be1d58bb9324a9a5a04"
    "0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c"
    "Test With Truncation")
   ("aa4ae5e15272d00e95705637ce8a3b55ed402112"
    #.(apply #'concatenate 'string (make-list 80 :initial-element "aa"))
    "Test Using Larger Than Block-Size Key - Hash Key First")
   ("e8e99d0f45237d786d6bbaa7965c7808bbff1a91"
    #.(apply #'concatenate 'string (make-list 80 :initial-element "aa"))
    "Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data")
   ("aa4ae5e15272d00e95705637ce8a3b55ed402112"
    #.(apply #'concatenate 'string (make-list 80 :initial-element "aa"))
    "Test Using Larger Than Block-Size Key - Hash Key First")
   ("e8e99d0f45237d786d6bbaa7965c7808bbff1a91"
    #.(apply #'concatenate 'string (make-list 80 :initial-element "aa"))
    #.(concatenate 'string
                   "Test Using Larger Than Block-Size Key and Larger"
                   " Than One Block-Size Data"))))

(deftest compute-hmac/sha256 ()
  ;; See RFC 4231 4. Test Vectors
  (check-compute-hmac
   :sha256
   ("b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"
    "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"
    "Hi There")
   ("5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"
    (text:encode-hex-string (text:encode-string "Jefe"))
    "what do ya want for nothing?")
   ("60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54"
    #.(make-array 262 :element-type 'character :initial-element #\a)
    "Test Using Larger Than Block-Size Key - Hash Key First")))
