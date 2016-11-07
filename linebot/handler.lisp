(in-package #:cl-user)
(defpackage #:linebot/handler
  (:use #:cl)
  (:import-from #:linebot/webhook
                #:validate-signature
                #:parse-request)
  (:import-from #:linebot/models/event
                #:event
                #:event-type
                #:event-message
                #:event-postback-data
                #:beacon
                #:event-beacon)
  (:import-from #:linebot/models/message
                #:message)
  (:import-from #:linebot/errors
                #:invalid-signature)
  (:export #:webhook-handler
           #:handle
           #:handle-message
           #:handle-follow
           #:handle-unfollow
           #:handle-join
           #:handle-leave
           #:handle-postback
           #:handle-beacon))
(in-package #:linebot/handler)

(defclass webhook-handler ()
  ())

(defgeneric handle (handler content signature)
  (:method ((handler webhook-handler) content signature)
    (unless (validate-signature content signature)
      (error 'invalid-signature :signature signature))

    (dolist (event (parse-request content))
      (ecase (event-type event)
        (:message  (handle-message handler event (event-message event)))
        (:follow   (handle-follow handler event))
        (:unfollow (handle-unfollow handler event))
        (:join     (handle-join handler event))
        (:leave    (handle-leave handler event))
        (:postback (handle-postback handler event (event-postback-data event)))
        (:beacon   (handle-beacon handler event (event-beacon event)))))))

(defgeneric handle-message (handler event message)
  (:method ((handler webhook-handler) (event event) (message message))))

(defgeneric handle-follow (handler event)
  (:method ((handler webhook-handler) (event event))))

(defgeneric handle-unfollow (handler event)
  (:method ((handler webhook-handler) (event event))))

(defgeneric handle-join (handler event)
  (:method ((handler webhook-handler) (event event))))

(defgeneric handle-leave (handler event)
  (:method ((handler webhook-handler) (event event))))

(defgeneric handle-postback (handler event data)
  (:method ((handler webhook-handler) (event event) data)))

(defgeneric handle-beacon (handler event beacon)
  (:method ((handler webhook-handler) (event event) (beacon beacon))))
