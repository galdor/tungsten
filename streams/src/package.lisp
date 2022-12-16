(defpackage :streams
  (:use :cl)
  (:export
   #:fundamental-stream
   #:fundamental-input-stream
   #:fundamental-output-stream
   #:fundamental-binary-stream
   #:fundamental-binary-input-stream
   #:fundamental-binary-output-stream
   #:fundamental-character-stream
   #:fundamental-character-input-stream
   #:fundamental-character-output-stream

   #:stream-read-char
   #:stream-unread-char
   #:stream-read-char-no-hang
   #:stream-peek-char
   #:stream-listen
   #:stream-read-line
   #:stream-clear-input
   #:stream-read-byte
   #:stream-read-sequence
   #:stream-write-char
   #:stream-line-column
   #:stream-start-line-p
   #:stream-write-string
   #:stream-terpri
   #:stream-fresh-line
   #:stream-finish-output
   #:stream-force-output
   #:stream-clear-output
   #:stream-advance-to-column
   #:stream-write-byte
   #:stream-write-sequence
   #:stream-file-position))
