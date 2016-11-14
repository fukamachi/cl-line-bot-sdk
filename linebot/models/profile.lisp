(in-package #:cl-user)
(defpackage #:linebot/models/profile
  (:use #:cl)
  (:import-from #:linebot/models/base
                #:json-serializable)
  (:export #:profile
           #:make-profile
           #:profile-display-name
           #:profile-user-id
           #:profile-picture-url
           #:profile-status-message))
(in-package #:linebot/models/profile)

(defclass profile (json-serializable)
  ((display-name :type string
                 :initarg :display-name
                 :accessor profile-display-name)
   (user-id :type string
            :initarg :user-id
            :accessor profile-user-id)
   (picture-url :type string
                :initarg :picture-url
                :accessor profile-picture-url)
   (status-message :type (or string null)
                   :initarg :status-message
                   :accessor profile-status-message)))

(defun make-profile (alist)
  (make-instance 'profile :alist alist))

(defmethod initialize-instance :after ((object profile) &key alist &allow-other-keys)
  (with-slots (display-name user-id picture-url status-message) object
    (setf display-name (aget alist "displayName")
          user-id (aget alist "userId")
          picture-url (aget alist "pictureUrl")
          status-message (aget alist "statusMessage"))))
