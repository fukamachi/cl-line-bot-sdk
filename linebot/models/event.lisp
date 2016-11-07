(in-package #:cl-user)
(defpackage #:linebot/models/event
  (:use #:cl)
  (:import-from #:linebot/models/message
                #:make-message
                #:text-message
                #:message-text)
  (:import-from #:linebot/models/source
                #:source
                #:make-source)
  (:import-from #:linebot/models/base
                #:json-serializable)
  (:import-from #:linebot/errors
                #:invalid-event-type
                #:invalid-beacon-type)
  (:import-from #:local-time
                #:timestamp
                #:unix-to-timestamp)
  (:import-from #:assoc-utils
                #:aget)
  (:import-from #:alexandria
                #:alist-hash-table)
  (:export #:event
           #:message-event
           #:follow-event
           #:unfollow-event
           #:join-event
           #:leave-event
           #:postback-event
           #:beacon-event
           #:beacon
           #:enter-beacon

           ;;
           ;; Constructor
           #:make-event

           ;;
           ;; Accessors
           #:event-type
           #:event-timestamp
           #:event-source

           ;; Only for replyable-event
           #:event-reply-token

           ;; for message-event
           #:event-message
           #:event-message-text

           ;; for postback-event
           #:event-postback-data

           ;; for beacon-event
           #:event-beacon
           #:beacon-type
           #:beacon-hwid))
(in-package #:linebot/models/event)

(defclass event (json-serializable)
  ((type :accessor event-type)
   (timestamp :type timestamp
              :initarg :timestamp
              :accessor event-timestamp)
   (source :type source
           :initarg :source
           :accessor event-source)))

(defvar *type-to-class*
  (alexandria:alist-hash-table
   '(("message" . message-event)
     ("follow" . follow-event)
     ("unfollow" . unfollow-event)
     ("join" . join-event)
     ("leave" . leave-event)
     ("postback" . postback-event)
     ("beacon" . beacon-event))
   :test 'equal))

(defun type-to-class (type)
  (or (gethash type *type-to-class*)
      (error 'invalid-event-type :type type)))

(defun make-event (alist)
  (make-instance (type-to-class (aget alist "type"))
                 :alist alist))

(defmethod initialize-instance :after ((object event) &key alist &allow-other-keys)
  (setf (event-timestamp object)
        (multiple-value-bind (sec millisec)
            (floor (aget alist "timestamp") 1000)
          (unix-to-timestamp sec :nsec (* millisec 1000 1000))))
  (setf (event-source object)
        (make-source (aget alist "source"))))

(defclass replyable-event ()
  ((reply-token :type string
                :initarg :reply-token
                :accessor event-reply-token)))

(defmethod initialize-instance :after ((object replyable-event) &key alist &allow-other-keys)
  (setf (event-reply-token object)
        (aget alist "replyToken")))

(defclass message-event (event replyable-event)
  ((type :initform :message)
   (message :type message
            :initarg :message
            :accessor event-message)))

(defmethod initialize-instance ((object message-event) &key alist &allow-other-keys)
  (call-next-method object :message (make-message (aget alist "message"))))

(defgeneric event-message-text (event)
  (:method ((event message-event))
    (let ((message (event-message event)))
      (check-type message text-message)
      (message-text message))))

(defclass follow-event (event replyable-event)
  ((type :initform :follow)))

(defclass unfollow-event (event)
  ((type :initform :unfollow)))

(defclass join-event (event replyable-event)
  ((type :initform :join)))

(defclass leave-event (event)
  ((type :initform :leave)))

(defclass postback (json-serializable)
  ((data :type string
         :initarg :data)))

(defclass postback-event (event replyable-event)
  ((type :initform :postback)
   (postback :type postback
             :initarg :postback)))

(defmethod initialize-instance ((object postback-event) &key alist &allow-other-keys)
  (call-next-method object
                    :postback
                    (make-instance 'postback
                                   :data (aget (aget alist "postback") "data"))))

(defgeneric event-postback-data (event)
  (:method ((event postback-event))
    (slot-value (slot-value event 'postback) 'data)))

(defclass beacon (json-serializable)
  ((hwid :type string
         :initarg :hwid
         :accessor beacon-hwid)
   (type :type keyword
         :accessor beacon-type)))

(defclass enter-beacon (beacon)
  ((type :initform :enter)))

(defclass beacon-event (event replyable-event)
  ((type :initform :beacon)
   (beacon :type beacon
           :initarg :beacon
           :accessor event-beacon)))

(defmethod initialize-instance ((object beacon-event) &key alist &allow-other-keys)
  (let ((beacon (aget alist "beacon")))
    (unless (string= (aget beacon "type") "enter")
      (error 'invalid-beacon-type :type (aget beacon "type")))
    (call-next-method object
                      :beacon (make-instance 'enter-beacon
                                             :hwid (aget beacon "hwid")))))
