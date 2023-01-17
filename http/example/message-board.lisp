(in-package :http-example)

(defvar *message-board-server* nil)
(defvar *message-board* nil)

(http:defrouter 'message-board)

(http:in-router 'message-board)

(defclass message-board ()
  ((messages
    :type list
    :initform nil
    :accessor message-board-messages)))

(defun start-message-board ()
  (when *message-board-server*
    (error "message board already started"))
  (let* ((message-board (make-instance 'message-board))
         (request-handler
           (lambda (request)
             (let ((router (http:find-router 'message-board))
                   (*message-board* message-board))
               (http:router-handle-request router request)))))
    (setf *message-board-server*
          (http:start-server "localhost" 8080 request-handler))))

(defun stop-message-board ()
  (when *message-board-server*
    (http:stop-server *message-board-server*)
    (setf *message-board-server* nil
          *message-board* nil)))

(http:defroute list-messages (:get "/messages")
  (let* ((messages (message-board-messages *message-board*))
         (last-messages
           (nreverse (subseq messages 0 (min (length messages) 5))))
         (body (format nil "~{~A~^~%~}" last-messages)))
    (http:make-response 200 :header '(("Content-Type" . "text/plain"))
                            :body body)))

(http:defroute post-message (:post "/messages")
  (let ((body (http:request-body http:*request*)))
    (cond
      ((= (length body) 0)
       (http:make-response 400 :header '(("Content-Type" . "text/plain"))
                               :body "Empty request body."))
      (t
       (let ((message (text:decode-string body)))
         (unless (char= (char message (1- (length message))) #\Newline)
           (setf message (concatenate 'string message (string #\Newline))))
         (push message (message-board-messages *message-board*)))
       (http:make-response 204)))))

(http:defroute delete-messages (:delete "/messages")
  (setf (message-board-messages *message-board*) nil)
  (http:make-response 204))
