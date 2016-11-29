(in-package #:cl-user)
(defpackage #:linebot/api
  (:use #:cl)
  (:import-from #:linebot/http
                #:request)
  (:import-from #:linebot/models/profile
                #:make-profile)
  (:import-from #:alexandria
                #:ensure-list)
  (:import-from #:jonathan
                #:to-json)
  (:export #:reply-message
           #:push-message
           #:get-message-content
           #:get-profile
           #:leave-room
           #:leave-group))
(in-package #:linebot/api)

(defparameter *reply-token* nil)
(defun reply-message (messages &optional (reply-token *reply-token*))
  (check-type reply-token string)
  (let ((messages (ensure-list messages)))
    (request "message/reply"
             :method :post
             :headers '((:content-type . "application/json"))
             :content (jojo:to-json `(("replyToken" . ,reply-token)
                                      ("messages" . ,messages))
                                    :from :alist))))

(defun push-message (messages to)
  (check-type to string)
  (let ((messages (ensure-list messages)))
    (request "message/push"
             :method :post
             :headers '((:content-type . "application/json"))
             :content (jojo:to-json `(("to" . ,to)
                                      ("messages" . ,messages))
                                    :from :alist))))

(defun get-message-content (message-id)
  (check-type message-id string)
  (request (format nil "message/~A/content" message-id)
           :want-stream t))

(defun get-profile (user-id)
  (check-type user-id string)
  (let ((res (request (format nil "profile/~A" user-id)
                      :method :get)))
    (make-profile (jojo:parse res :as :alist))))

(defun leave-room (room-id)
  (check-type room-id string)
  (request (format nil "room/~A/leave" room-id)
           :method :post))

(defun leave-group (group-id)
  (check-type group-id string)
  (request (format nil "group/~A/leave" group-id)
           :method :post))
