(in-package :text)

;; The 0xdb octet was '¤' (U+00A4) before Mac OS 8.5 and is now '€' (U+20AC).
;; If you need to support Mac OS Roman encoded data generated pre-1998, feel
;; free to define your own encoding.

(define-character-mapping (macintosh)
    #( 0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
      16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
      #\  #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\- #\. #\/
      #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\; #\< #\= #\> #\?
      #\@ #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O
      #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\[ #\\ #\] #\^ #\_
      #\` #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o
      #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\{ #\| #\} #\~ 127
      #\Ä #\Å #\Ç #\É #\Ñ #\Ö #\Ü #\á #\à #\â #\ä #\ã #\å #\ç #\é #\è
      #\ê #\ë #\í #\ì #\î #\ï #\ñ #\ó #\ò #\ô #\ö #\õ #\ú #\ù #\û #\ü
      #\† #\° #\¢ #\£ #\§ #\• #\¶ #\ß #\® #\© #\™ #\´ #\¨ #\≠ #\Æ #\Ø
      #\∞ #\± #\≤ #\≥ #\¥ #\µ #\∂ #\∑ #\∏ #\π #\∫ #\ª #\º #\Ω #\æ #\ø
      #\¿ #\¡ #\¬ #\√ #\ƒ #\≈ #\∆ #\« #\» #\… #\  #\À #\Ã #\Õ #\Œ #\œ
      #\– #\— #\“ #\” #\‘ #\’ #\÷ #\◊ #\ÿ #\Ÿ #\⁄ #\€ #\‹ #\› #\ﬁ #\ﬂ
      #\‡ #\· #\‚ #\„ #\‰ #\Â #\Ê #\Á #\Ë #\È #\Í #\Î #\Ï #\Ì #\Ó #\Ô
      240 #\Ò #\Ú #\Û #\Ù #\ı #\ˆ #\˜ #\¯ #\˘ #\˙ #\˚ #\¸ #\˝ #\˛ #\ˇ))

(defun encoded-character-length/macintosh (character)
  (declare (type character character))
  (let ((code (character-to-code/macintosh character)))
    (unless code
      (error 'unencodable-character :character character
                                    :encoding :macintosh))
    1))

(defun encoded-string-length/macintosh (string start end)
  (declare (type string string)
           (type (or index null) start end))
  (do ((max-index (1- (or end (length string))))
       (length 0)
       (i (or start 0) (1+ i)))
      ((> i max-index)
       length)
    (incf length (encoded-character-length/macintosh (schar string i)))))

(defun encode-character/macintosh (character octets offset)
  (declare (type character character)
           (type core:octet-vector octets)
           (type (or index null) offset))
  (let ((code (character-to-code/macintosh character)))
    (unless code
      (error 'unencodable-character :character character
                                    :encoding :macintosh))
    (setf (aref octets offset) code)
    1))

(defun decoded-string-length/macintosh (octets start end)
  (declare (type core:octet-vector octets)
           (type (or index null) start end))
  (- (or end (length octets)) (or start 0)))

(defun decode-character/macintosh (octets start end)
  (declare (type core:octet-vector octets)
           (type (or index null) start end))
  (let ((start (or start 0))
        (end (or end (length octets))))
    (cond
      ((>= start end)
       (values nil 0))
      (t
       (let* ((octet (aref octets start))
              (character (code-to-character/macintosh octet)))
         (unless character
           (error 'invalid-octet :octets octets :offset start
                                 :octet octet :encoding :ascii))
         (values character 1))))))

(define-encoding :macintosh ()
  :name "Mac OS Roman"
  :encoded-character-length-function #'encoded-character-length/macintosh
  :encoded-string-length-function #'encoded-string-length/macintosh
  :character-encoding-function #'encode-character/macintosh
  :decoded-string-length-function #'decoded-string-length/macintosh
  :character-decoding-function #'decode-character/macintosh)
