(in-package :http)

(defvar *routers* (make-hash-table))

(defvar *router* nil)

(defvar *route* nil)

(defvar *request* nil)

(define-condition unknown-router (error)
  ((name
    :type symbol
    :initarg :name
    :reader unknown-router-name))
  (:report
   (lambda (condition stream)
     (with-slots (name) condition
       (format stream "Unknown HTTP router ~S." name)))))

(defclass router ()
  ((name
    :type symbol
    :initarg :name
    :accessor router-name)
   (mutex
    :type system:mutex
    :initform (system:make-mutex :name "http-router"))
   (routes
    :type list
    :initarg :routes
    :initform nil
    :accessor router-routes)))

(defmethod print-object ((router router) stream)
  (print-unreadable-object (router stream :type t)
    (princ (router-name router) stream)))

(defmacro defrouter (name)
  (let ((name-var (gensym "NAME-"))
        (router (gensym "ROUTER-")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let* ((,name-var ',name)
              (,router (make-instance 'router :name ,name-var)))
         (setf (gethash ,name-var *routers*) ,router)))))

(defmacro in-router (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *router* (find-router ',name))))

(defun find-router (name)
  (etypecase name
    (symbol
     (or (gethash name *routers*)
         (error 'unknown-router :name name)))
    (router
     name)))

(defun delete-router (name)
  (unless (remhash name *routers*)
    (error 'unknown-router :name name)))

(defun router-request-handler (router)
  (lambda (request)
    (router-handle-request router request)))

(defun router-handle-request (router request)
  (let ((route (router-find-matching-route router request)))
    (if route
        (let ((*route* route))
          (funcall (route-request-handler route) request))
        (make-plain-text-response 404 "Route not found."))))

(defun router-find-matching-route (router request)
  (with-slots (mutex routes) router
    (system:with-mutex (mutex)
      (find-if (lambda (route)
                 (match-route route request))
               routes))))

(defmacro defroute (name (method path) &body body)
  (let ((function (gensym "FUNCTION-"))
        (route (gensym "ROUTE-")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let* ((,function
                (lambda (request)
                  (let ((*request* request))
                    ,@body)))
              (,route (make-instance 'route
                                     :name ',name
                                     ,@(when method (list :method method))
                                     :path (parse-route-path ,path)
                                     :request-handler ,function)))
         (with-slots (mutex routes) *router*
           (system:with-mutex (mutex)
             (push ,route routes)))))))

(defun delete-route (name &optional (router *router*))
  (with-slots (mutex routes) (find-router router)
    (system:with-mutex (mutex)
      (setf routes (delete name routes :key 'route-name)))))
