(in-package #:cl-user)
(defpackage #:linebot/http
  (:use #:cl)
  (:import-from #:linebot/config
                #:*message-api-endpoint*
                #:*channel-access-token*)
  (:import-from #:dexador)
  (:import-from #:quri
                #:merge-uris
                #:uri)
  (:export #:request))
(in-package #:linebot/http)

(defun message-api (&optional (path "/"))
  (quri:merge-uris (quri:uri path)
                   (quri:uri *message-api-endpoint*)))

(defun request (path &key (method :get) headers content want-stream)
  (dex:request (message-api path)
               :method method
               :headers (append `((:authorization . ,(format nil "Bearer ~A" *channel-access-token*)))
                                headers)
               :content content
               :want-stream want-stream))
