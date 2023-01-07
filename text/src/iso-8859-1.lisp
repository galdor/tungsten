(in-package :text)

(define-character-mapping (iso-8859-1)
  #(  0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
     16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
    #\  #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\- #\. #\/
    #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\; #\< #\= #\> #\?
    #\@ #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O
    #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\[ #\\ #\] #\^ #\_
    #\` #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o
    #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\{ #\| #\} #\~ 127
    128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143
    144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159
    #\  #\¡ #\¢ #\£ #\¤ #\¥ #\¦ #\§ #\¨ #\© #\ª #\« #\¬ #\­ #\® #\¯
    #\° #\± #\² #\³ #\´ #\µ #\¶ #\· #\¸ #\¹ #\º #\» #\¼ #\½ #\¾ #\¿
    #\À #\Á #\Â #\Ã #\Ä #\Å #\Æ #\Ç #\È #\É #\Ê #\Ë #\Ì #\Í #\Î #\Ï
    #\Ð #\Ñ #\Ò #\Ó #\Ô #\Õ #\Ö #\× #\Ø #\Ù #\Ú #\Û #\Ü #\Ý #\Þ #\ß
    #\à #\á #\â #\ã #\ä #\å #\æ #\ç #\è #\é #\ê #\ë #\ì #\í #\î #\ï
    #\ð #\ñ #\ò #\ó #\ô #\õ #\ö #\÷ #\ø #\ù #\ú #\û #\ü #\ý #\þ #\ÿ))

(defun encoded-character-length/iso-8859-1 (character)
  (declare (type character character))
  (let ((code (character-to-code/iso-8859-1 character)))
    (unless code
      (error 'unencodable-character :character character
                                    :encoding :iso-8859-1))
    1))

(defun encoded-string-length/iso-8859-1 (string start end)
  (declare (type simple-string string)
           (type (or index null) start end))
  (do ((max-index (1- (or end (length string))))
       (length 0)
       (i (or start 0) (1+ i)))
      ((> i max-index)
       length)
    (incf length (encoded-character-length/iso-8859-1 (schar string i)))))

(defun encode-character/iso-8859-1 (character octets offset)
  (declare (type character character)
           (type core:octet-vector octets)
           (type (or index null) offset))
  (let ((code (character-to-code/iso-8859-1 character)))
    (unless code
      (error 'unencodable-character :character character
                                    :encoding :iso-8859-1))
    (setf (aref octets offset) code)
    1))

(defun decoded-string-length/iso-8859-1 (octets start end)
  (declare (type core:octet-vector octets)
           (type (or index null) start end))
  (- (or end (length octets)) (or start 0)))

(defun decode-character/iso-8859-1 (octets start end)
  (declare (type core:octet-vector octets)
           (type (or index null) start end))
  (let ((start (or start 0))
        (end (or end (length octets))))
    (cond
      ((>= start end)
       (values nil 0))
      (t
       (let* ((octet (aref octets start))
              (character (code-to-character/iso-8859-1 octet)))
         (unless character
           (error 'invalid-octet :octets octets :offset start
                                 :octet octet :encoding :ascii))
         (values character 1))))))

(define-encoding :iso-8859-1 ()
  :name "ISO-8859-1"
  :encoded-character-length-function #'encoded-character-length/iso-8859-1
  :encoded-string-length-function #'encoded-string-length/iso-8859-1
  :character-encoding-function #'encode-character/iso-8859-1
  :decoded-string-length-function #'decoded-string-length/iso-8859-1
  :character-decoding-function #'decode-character/iso-8859-1)
