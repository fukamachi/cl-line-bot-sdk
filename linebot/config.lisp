(in-package #:cl-user)
(defpackage #:linebot/config
  (:use #:cl)
  (:export #:*channel-secret*
           #:*channel-access-token*
           #:*message-api-endpoint*))
(in-package #:linebot/config)

(defvar *channel-secret* nil)
(defvar *channel-access-token* nil)

(defvar *message-api-endpoint*
  "https://api.line.me/v2/bot/")
