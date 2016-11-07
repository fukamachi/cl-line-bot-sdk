(in-package #:cl-user)
(defpackage #:linebot/handler
  (:use #:cl)
  (:import-from #:linebot/webhook
                #:validate-signature
                #:parse-request)
  (:import-from #:linebot/models/event
                #:event
                #:message-event
                #:follow-event
                #:unfollow-event
                #:join-event
                #:leave-event
                #:postback-event
                #:beacon-event
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
           #:handle-event
           #:handle-message-event
           #:handle-follow-event
           #:handle-unfollow-event
           #:handle-join-event
           #:handle-leave-event
           #:handle-postback-event
           #:handle-beacon-event))
(in-package #:linebot/handler)

(defclass webhook-handler ()
  ())

(defgeneric handle (handler content signature)
  (:method ((handler webhook-handler) content signature)
    (unless (validate-signature content signature)
      (error 'invalid-signature :signature signature))

    (dolist (event (parse-request content))
      (handle-event handler event))))

(defgeneric handle-event (handler event)
  (:method ((handler webhook-handler) (event message-event))
    (handle-message-event handler event (event-message event)))
  (:method ((handler webhook-handler) (event follow-event))
    (handle-follow-event handler event))
  (:method ((handler webhook-handler) (event unfollow-event))
    (handle-unfollow-event handler event))
  (:method ((handler webhook-handler) (event join-event))
    (handle-join-event handler event))
  (:method ((handler webhook-handler) (event leave-event))
    (handle-leave-event handler event))
  (:method ((handler webhook-handler) (event postback-event))
    (handle-postback-event handler event (event-postback-data event)))
  (:method ((handler webhook-handler) (event beacon-event))
    (handle-beacon-event handler event (event-beacon event))))

(defgeneric handle-message-event (handler event message)
  (:method ((handler webhook-handler) (event message-event) (message message))))

(defgeneric handle-follow-event (handler event)
  (:method ((handler webhook-handler) (event follow-event))))

(defgeneric handle-unfollow-event (handler event)
  (:method ((handler webhook-handler) (event unfollow-event))))

(defgeneric handle-join-event (handler event)
  (:method ((handler webhook-handler) (event join-event))))

(defgeneric handle-leave-event (handler event)
  (:method ((handler webhook-handler) (event leave-event))))

(defgeneric handle-postback-event (handler event data)
  (:method ((handler webhook-handler) (event postback-event) data)))

(defgeneric handle-beacon-event (handler event beacon)
  (:method ((handler webhook-handler) (event beacon-event) (beacon beacon))))
