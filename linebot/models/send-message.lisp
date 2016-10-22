(in-package #:cl-user)
(defpackage #:linebot/models/send-message
  (:use #:cl)
  (:import-from #:linebot/models/base
                #:json-serializable)
  (:export #:send-message
           #:text-send-message
           #:image-send-message
           #:video-send-message
           #:audio-send-message
           #:location-send-message
           #:sticker-send-message))
(in-package #:linebot/models/send-message)

(defclass send-message (json-serializable)
  ((type :type keyword)))

(defclass text-send-message (send-message)
  ((type :initform :text)
   (text :type string
         :initarg :text)))

(defclass image-send-message (send-message)
  ((type :initform :image)
   (original-content-url :type string
                         :initarg :original-content-url)
   (preview-image-url :type string
                      :initarg :preview-image-url)))

(defclass video-send-message (send-message)
  ((type :initform :video)
   (original-content-url :type string
                         :initarg :original-content-url)
   (preview-image-url :type string
                      :initarg :preview-image-url)))

(defclass audio-send-message (send-message)
  ((type :initform :audio)
   (original-content-url :type string
                         :initarg :original-content-url)
   (duration :type integer ;; milliseconds
             :initarg :duration)))

(defclass location-send-message (send-message)
  ((type :initform :location)
   (title :type string
          :initarg :title)
   (address :type string
            :initarg :title)
   (latitude :type double-float
             :initarg :latitude)
   (longitude :type double-float
              :initarg :longitude)))

(defclass sticker-send-message (send-message)
  ((type :initform :sticker)
   (package-id :type string
               :initarg :package-id)
   (sticker-id :type string
               :initarg :sticker-id)))
