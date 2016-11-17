(in-package #:cl-user)
(defpackage #:linebot/models/template
  (:use #:cl)
  (:import-from #:linebot/models/base
                #:json-serializable)
  (:import-from #:linebot/models/send-message
                #:send-message)
  (:export #:template-send-message
           #:template
           #:buttons-template
           #:confirm-template
           #:carousel-template
           #:carousel-column

           #:template-action
           #:postback-template-action
           #:message-template-action
           #:uri-template-action))
(in-package #:linebot/models/template)

(defclass template-send-message (send-message)
  ((type :initform :template)
   (alt-text :type string
             :initarg :alt-text)
   (template :type template
             :initarg :template)))

(defclass template (json-serializable)
  ((type :type keyword
         :initarg :type)))

(defclass buttons-template (template)
  ((type :initform :buttons)
   (thumbnail-image-url :type string
                        :initarg :thubmnail-image-url) 
   (title :type string
          :initarg :title)
   (text :type string
         :initarg :text) 
   (actions :type list
            :initarg :actions)))

(defclass confirm-template (template)
  ((type :initform :confirm)
   (text :type string
         :initarg :text)
   (actions :type list
            :initarg :actions)))

(defclass carousel-template (template)
  ((type :initform :carousel)
   (columns :type list
            :initarg :columns)))

(defclass carousel-column (json-serializable)
  ((thumbnail-image-url :type string
                        :initarg :thumbnail-image-url)
   (title :type string
          :initarg :title)
   (text :type string
         :initarg :text)
   (actions :type list
            :initarg :actions)))

(defclass template-action (json-serializable)
  ((type :type keyword
         :initarg :type)
   (label :type string
          :initarg :label)))

(defclass postback-template-action (template-action)
  ((type :initform :postback)
   (data :type string
         :initarg :data) 
   (text :type string
         :initarg :text)))

(defclass message-template-action (template-action)
  ((type :initform :message)
   (text :type string
         :initarg :text)))

(defclass uri-template-action (template-action)
  ((type :initform :uri)
   (label :type string
          :initarg :label)
   (uri :type string
        :initarg :uri)))
