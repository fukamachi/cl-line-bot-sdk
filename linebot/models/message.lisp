(in-package #:cl-user)
(defpackage #:linebot/models/message
  (:use #:cl)
  (:import-from #:linebot/models/base
                #:json-serializable)
  (:import-from #:linebot/errors
                #:invalid-message-type)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:message
           #:text-message
           #:image-message
           #:video-message
           #:audio-message
           #:location-message
           #:sticker-message

           ;;
           ;; Constructor
           #:make-message

           ;; 
           ;; Accessors
           #:message-id
           #:message-type
           #:message-text

           ;; for location-message
           #:message-location-title
           #:message-location-address
           #:message-location-latitude
           #:message-location-longitude

           ;; for sticker-message
           #:message-sticker-package-id
           #:message-sticker-id))
(in-package #:linebot/models/message)

(defclass message (json-serializable)
  ((id :type (or string null)
       :initarg :id
       :initform nil
       :accessor message-id)
   (type :type keyword
         :accessor message-type)))

(defun type-to-class (type)
  (cond
    ((string= type "text")     'text-message)
    ((string= type "image")    'image-message)
    ((string= type "video")    'video-message)
    ((string= type "audio")    'audio-message)
    ((string= type "location") 'location-message)
    ((string= type "sticker")  'sticker-message)
    (t (error 'invalid-message-type :type type))))

(defun make-message (alist)
  (make-instance (type-to-class (aget alist "type"))
                 :alist alist))

(defmethod initialize-instance :after ((object message) &key alist &allow-other-keys)
  (setf (message-id object) (aget alist "id")))

(defclass text-message (message)
  ((type :initform :text)
   (text :type string
         :initarg :text
         :accessor message-text)))

(defmethod initialize-instance ((object text-message) &key alist &allow-other-keys)
  (call-next-method object :text (aget alist "text")))

(defclass image-message (message)
  ((type :initform :image)))

(defclass video-message (message)
  ((type :initform :video)))

(defclass audio-message (message)
  ((type :initform :audio)))

(defclass location-message (message)
  ((type :initform :location)
   (title :type string
          :initarg :title
          :accessor message-location-title)
   (address :type string
            :initarg :address
            :accessor message-location-address)
   (latitude :type double-float
             :initarg :latitude
             :accessor message-location-latitude)
   (longitude :type double-float
              :initarg :longitude
              :accessor message-location-longitude)))

(defmethod initialize-instance ((object location-message) &key alist &allow-other-keys)
  (call-next-method object
                    :title (aget alist "title") 
                    :address (aget alist "address") 
                    :latitude (aget alist "latitude") 
                    :longitude (aget alist "longitude")))

(defclass sticker-message (message)
  ((type :initform :sticker)
   (package-id :type string
               :initarg :package-id
               :accessor message-sticker-package-id)
   (sticker-id :type string
               :initarg :sticker-id
               :accessor message-sticker-id)))

(defmethod initialize-instance ((object sticker-message) &key alist &allow-other-keys)
  (call-next-method object
                    :package-id (aget alist "packageId")
                    :sticker-id (aget alist "stickerId")))
