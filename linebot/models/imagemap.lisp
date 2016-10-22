(in-package #:cl-user)
(defpackage #:linebot/models/imagemap
  (:use #:cl)
  (:import-from #:linebot/models/base
                #:json-serializable)
  (:import-from #:linebot/models/send-message
                #:send-message)
  (:export #:imagemap-send-message
           #:imagemap-action
           #:imagemap-uri-action
           #:imagemap-message-action
           #:imagemap-area))
(in-package #:linebot/models/imagemap)

(defclass base-size (json-serializable)
  ((width :type number
          :initarg :width)
   (height :type number
           :initarg :height)))

(defclass imagemap-send-message (send-message)
  ((type :initform :imagemap)
   (base-url :type string
             :initarg :base-url)
   (alt-text :type string
             :initarg :alt-text)
   (base-size :type base-size)
   (actions :type list
            :initarg :actions)))

(defmethod initialize-instance :after ((object imagemap-send-message) &key base-width base-height &allow-other-keys)
  (setf (slot-value object 'base-size)
        (make-instance 'base-size
                       :width base-width
                       :height base-height)))

(defclass imagemap-action (json-serializable)
  ((type :initform :uri)
   (area :type imagemap-area
         :initarg :area)))

(defmethod initialize-instance :after ((action imagemap-action) &key area-x area-y area-width area-height &allow-other-keys)
  (setf (slot-value action 'area)
        (make-instance 'imagemap-area
                       :x area-x
                       :y area-y
                       :width area-width
                       :height area-height)))

(defclass imagemap-uri-action (imagemap-action)
  ((link-uri :type string
             :initarg :link-uri)))

(defclass imagemap-message-action (imagemap-action)
  ((text :type string
         :initarg :text)))

(defclass imagemap-area (json-serializable)
  ((x :type number
      :initarg :x)
   (y :type number
      :initarg :y)
   (width :type number
          :initarg :width)
   (height :type number
           :initarg :height)))
