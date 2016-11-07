(let ((asdf:*central-registry*
        (cons (make-pathname :directory (butlast (pathname-directory
                                                  (or *load-pathname* *compile-file-pathname*))))
              asdf:*central-registry*)))
  (ql:quickload '(:lack-request :linebot) :silent t))

(in-package #:cl-user)
(defpackage #:linebot/examples/echo-handler
  (:use #:cl
        #:lack.request))
(in-package #:linebot/examples/echo-handler)

(defclass echo-handler (linebot:webhook-handler) ())
(defvar *handler* (make-instance 'echo-handler))

(defmethod linebot:handle-message ((handler echo-handler) (event linebot:message-event) (message linebot:text-message))
  (linebot:reply-message
   (linebot:event-reply-token event)
   (make-instance 'linebot:text-send-message
                  :text (linebot:message-text message))))

(lambda (env)
  (let ((req (make-request env)))
    (unless (and (eq (request-method req) :post)
                 (string= (request-path-info req) "/callback"))
      (return '(404 () ("Not Found"))))

    (handler-case
        (progn
          (linebot:handle *handler*
                          (request-content req)
                          (gethash "x-line-signature" (request-headers req)))
          '(200 () ("ok")))
      (linebot:invalid-signature () '(400 () ("Invalid signature"))))))
