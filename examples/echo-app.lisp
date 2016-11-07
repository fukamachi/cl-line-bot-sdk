(let ((asdf:*central-registry*
        (cons (make-pathname :directory (butlast (pathname-directory
                                                  (or *load-pathname* *compile-file-pathname*))))
              asdf:*central-registry*)))
  (ql:quickload '(:linebot/app :uiop) :silent t))

(in-package #:cl-user)
(defpackage #:linebot/examples/echo-app
  (:use #:cl))
(in-package #:linebot/examples/echo-app)

(setf linebot:*channel-secret* (uiop:getenv "LINE_CHANNEL_SECRET"))
(setf linebot:*channel-access-token* (uiop:getenv "LINE_CHANNEL_ACCESS_TOKEN"))

(defclass echo-app (linebot/app:app) ())

(defmethod linebot:handle-message-event ((handler echo-app)
                                         (event linebot:message-event)
                                         (message linebot:text-message))
  (linebot:reply-message
   (linebot:event-reply-token event)
   (make-instance 'linebot:text-send-message
                  :text (linebot:message-text message))))

(make-instance 'echo-app :callback "/callback")
