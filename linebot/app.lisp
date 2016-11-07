(in-package #:cl-user)
(defpackage #:linebot/app
  (:use #:cl)
  (:import-from #:linebot
                #:invalid-signature)
  (:import-from #:linebot/handler
                #:webhook-handler
                #:handle
                #:invalid-signature)
  (:import-from #:lack.component
                #:lack-component
                #:call)
  (:import-from #:lack.request
                #:make-request
                #:request-method
                #:request-path-info
                #:request-content
                #:request-headers)
  (:export #:app
           #:*request*))
(in-package #:linebot/app)

(defparameter *request* nil)

(defclass app (lack-component linebot:webhook-handler)
  ((callback :type string
             :initarg :callback
             :reader app-callback)))

(defmethod call ((app app) env)
  (let ((*request* (make-request env)))
    (unless (and (eq (request-method *request*) :post)
                 (string= (request-path-info *request*) (app-callback app)))
      (return-from call '(404 () ("Not Found"))))

    (handler-case
        (progn
          (linebot:handle app
                          (request-content *request*)
                          (gethash "x-line-signature" (request-headers *request*)))
          '(200 () ("ok")))
      (linebot:invalid-signature () '(400 () ("Invalid signature"))))))
