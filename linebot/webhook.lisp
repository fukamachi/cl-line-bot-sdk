(in-package #:cl-user)
(defpackage #:linebot/webhook
  (:use #:cl)
  (:import-from #:linebot/models
                #:make-event)
  (:import-from #:linebot/config
                #:*channel-secret*)
  (:import-from #:ironclad
                #:make-hmac
                #:update-hmac
                #:hmac-digest
                #:ascii-string-to-byte-array)
  (:import-from #:cl-base64
                #:base64-string-to-usb8-array)
  (:import-from #:jonathan
                #:parse)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:validate-signature
           #:parse-request))
(in-package #:linebot/webhook)

(defun validate-signature (content x-line-signature)
  (when (stringp x-line-signature)
    (let ((hmac (ironclad:make-hmac (ascii-string-to-byte-array *channel-secret*) :sha256)))
      (ironclad:update-hmac hmac (if (stringp content)
                                     (ascii-string-to-byte-array content)
                                     content))
      (equalp (ironclad:hmac-digest hmac)
              (base64-string-to-usb8-array x-line-signature)))))

(defun parse-request (content)
  (let ((json (jojo:parse (etypecase content
                            (string content)
                            (simple-vector
                             (babel:octets-to-string content :encoding :utf-8)))
                          :as :alist)))
    (mapcar #'make-event (aget json "events"))))
