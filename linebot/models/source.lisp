(in-package #:cl-user)
(defpackage #:linebot/models/source
  (:use #:cl)
  (:import-from #:linebot/models/base
                #:json-serializable)
  (:import-from #:linebot/errors
                #:invalid-source-type)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:source
           #:make-source
           #:source-type
           #:source-user
           #:user-id
           #:source-group
           #:group-id
           #:source-room
           #:room-id))
(in-package #:linebot/models/source)

(defclass source (json-serializable)
  ((stype :accessor source-type)))

(defun type-to-class (type)
  (check-type type string)
  (cond
    ((string= type "user")  'source-user)
    ((string= type "group") 'source-group)
    ((string= type "room")  'source-room)
    (t (error 'invalid-source-type :type type))))

(defun make-source (alist)
  (make-instance (type-to-class (aget alist "type"))
                 :alist alist))

(defclass source-user (source)
  ((stype :initform :user)
   (user-id :type string
            :initarg :user-id
            :accessor user-id)))

(defmethod initialize-instance ((object source-user) &key alist &allow-other-keys)
  (call-next-method object :user-id (aget alist "userId")))

(defclass source-group (source)
  ((stype :initform :group)
   (group-id :type string
             :initarg :group-id
             :accessor group-id)))

(defmethod initialize-instance ((object source-group) &key alist &allow-other-keys)
  (call-next-method object :group-id (aget alist "groupId")))

(defclass source-room (source)
  ((stype :initform :room)
   (room-id :type string
            :initarg :room-id
            :accessor room-id)))

(defmethod initialize-instance ((object source-room) &key alist &allow-other-keys)
  (call-next-method object :room-id (aget alist "roomId")))
