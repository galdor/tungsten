(in-package :uri)

(defun resolve-reference (reference base)
  (cond
    ((uri-scheme reference)
     (let ((uri (copy-uri reference)))
       (setf (uri-path uri) (remove-dot-segments (uri-path reference)))
       uri))
    ((and (uri-host reference) (uri-scheme base))
     (let ((uri (copy-uri reference)))
       (setf (uri-scheme uri) (uri-scheme base))
       (setf (uri-path uri) (remove-dot-segments (uri-path reference)))
       uri))
    ((uri-scheme base)
     (let ((uri (copy-uri base)))
       (when (uri-fragment reference)
         (setf (uri-fragment uri) (uri-fragment reference)))
       (let ((path (or (uri-path reference) "")))
         (cond
           ((string= path "")
            (when (uri-query reference)
              (setf (uri-query uri) (uri-query reference))))
           ((char= (char path 0) #\/)
            (setf (uri-query uri) (uri-query reference))
            (setf (uri-path uri) (remove-dot-segments path)))
           (t
            (setf (uri-query uri) (uri-query reference))
            (let* ((base-path (or (uri-path base) ""))
                   (has-base-authority (not (null (uri-host base)))))
              (setf (uri-path uri)
                    (remove-dot-segments
                     (merge-paths base-path has-base-authority path)))))))
       uri))
    (t
     (error "missing base uri scheme"))))

(defun merge-paths (base-path has-base-authority path)
  (cond
    ((and (string= base-path "") has-base-authority)
     (concatenate 'string "/" path))
    (t
     (concatenate 'string (remove-last-segment base-path) path))))

(defun remove-dot-segments (path)
  (do ((i 0)
       (end (length path))
       (acc nil))
      ((>= i end)
       acc)
    (macrolet ((path-equal (string)
                 `(string= path ,string :start1 i))
               (path-prefixp (prefix)
                 `(and (>= (- end i) ,(length prefix))
                       (string= path ,prefix
                                :start1 i
                                :end1 (+ i ,(length prefix))))))
      (cond
        ((path-prefixp "../")
         (incf i 3))
        ((path-prefixp "./")
         (incf i 2))
        ((path-prefixp "/./")
         (incf i 2))
        ((path-equal "/.")
         (setf acc (concatenate 'string acc "/"))
         (setf i end))
        ((path-prefixp "/../")
         (setf acc (remove-last-segment-and-slash acc))
         (incf i 3))
        ((path-equal "/..")
         (setf acc (concatenate
                    'string (remove-last-segment-and-slash acc) "/"))
         (setf i end))
        ((or (path-equal ".")
             (path-equal ".."))
         (setf i end))
        (t
         (let* ((start (if (char= (char path i) #\/) (1+ i) i))
                (slash (position #\/ path :start start))
                (separator (or slash end)))
           (setf acc (concatenate 'string acc (subseq path i separator)))
           (setf i separator)))))))


(defun remove-last-segment (path)
  (let ((last-slash (position #\/ path :from-end t)))
    (subseq path 0 (if last-slash (1+ last-slash) 0))))

(defun remove-last-segment-and-slash (path)
  (let ((last-slash (position #\/ path :from-end t)))
    (subseq path 0 (or last-slash 0))))
