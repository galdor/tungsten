(in-package :core)

(defun buffer-append-string (buffer string
                             &key (start 0) (end nil)
                                  (encoding text:*default-encoding*))
  (declare (type buffer buffer)
           (type string string))
  (when (> (length string) 0)
    (let* ((end (or end (length string)))
           (nb-octets
             (text:encoded-string-length string :encoding encoding
                                                :start start :end end))
           (offset (core:buffer-reserve buffer nb-octets)))
      (text:encode-string string :encoding encoding
                                 :start start :end end
                                 :octets (buffer-data buffer)
                                 :offset offset)
      (incf (buffer-end buffer) nb-octets))))

(export 'buffer-append-string :core)

;; Directly writing ASCII data to IO stream buffers is used in network servers
;; (e.g. the HTTP server) to bypass Gray streams. It makes sense to optimize
;; this very specific case by avoiding the encoding lookup.
(define-compiler-macro buffer-append-string
    (&whole form
     buffer string
     &key (start 0) end
          (encoding text:*default-encoding*))
  (case encoding
    (:ascii
     (let ((buffer-var (gensym "BUFFER-VAR-"))
           (string-var (gensym "STRING-VAR-"))
           (start-var (gensym "START-VAR-"))
           (end-var (gensym "END-VAR-"))
           (data (gensym "DATA-"))
           (length (gensym "LENGTH-"))
           (i (gensym "I-"))
           (j (gensym "J-")))
       `(let* ((,buffer-var ,buffer)
               (,string-var ,string)
               (,start-var ,start)
               (,end-var (or ,end (length ,string-var)))
               (,data (buffer-data ,buffer-var))
               (,length (- ,end-var ,start-var)))
          (when (> ,length 0)
            (do ((,i ,start (1+ ,i))
                 (,j (core:buffer-reserve ,buffer-var ,length) (1+ ,j)))
                ((>= ,i ,end-var))
              (setf (aref ,data ,j) (char-code (char ,string ,i))))
            (incf (buffer-end ,buffer-var) ,length)))))
    (t
     form)))
