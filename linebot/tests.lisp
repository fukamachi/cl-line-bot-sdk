(in-package #:cl-user)
(defpackage #:linebot/tests
  (:use #:cl
        #:re21)
  (:import-from #:linebot/tests/util
                #:random-port
                #:make-random-string)
  (:import-from #:linebot
                #:*message-api-endpoint*)
  (:import-from #:linebot/handler)
  (:import-from #:linebot/app)
  (:import-from #:lack.component
                #:lack-component
                #:call)
  (:import-from #:lack.request
                #:make-request)
  (:import-from #:clack
                #:clackup
                #:stop)
  (:import-from #:jonathan
                #:to-json)
  (:import-from #:babel
                #:string-to-octets)
  (:import-from #:dexador
                #:post)
  (:import-from #:bordeaux-threads
                #:*default-special-bindings*)
  (:import-from #:ironclad
                #:make-hmac
                #:ascii-string-to-byte-array
                #:update-hmac
                #:hmac-digest)
  (:import-from #:cl-base64
                #:usb8-array-to-base64-string)
  (:import-from #:prove
                #:subtest)
  (:import-from #:local-time
                #:timestamp-to-unix
                #:now
                #:timestamp-millisecond)
  (:import-from #:alexandria
                #:with-gensyms
                #:once-only
                #:ensure-list
                #:starts-with-subseq)
  (:export #:emit-webhook
           #:lineapp-requests
           #:subtest-lineapp
           #:dummy-user-id
           #:dummy-reply-token
           #:current-timestamp))
(in-package #:linebot/tests)

(defun make-buffer ()
  (make-array 0 :adjustable t :fill-pointer 0))

(defclass mock-app (lack.component:lack-component)
  ((buffer :initform (make-buffer))
   (channel-secret :initarg :channel-secret)
   (channel-access-token :initarg :channel-access-token)
   (webhook-url :initarg :webhook-url)))

(defvar *mock-app*)
(defun lineapp-requests (&optional (app *mock-app*))
  (prog1 (slot-value app 'buffer)
    (setf (slot-value app 'buffer) (make-buffer))))

(defun emit-webhook (events &optional (app *mock-app*))
  (let* ((hmac (ironclad:make-hmac (ascii-string-to-byte-array (slot-value app 'channel-secret)) :sha256))
         (content (jojo:to-json `(("events" . ,(ensure-list events)))))
         (content (babel:string-to-octets content)))
    (ironclad:update-hmac hmac content)
    (let ((x-line-signature (base64:usb8-array-to-base64-string (ironclad:hmac-digest hmac))))
      (dex:post (slot-value app 'webhook-url)
                :headers `(("Content-Type" . "application/json")
                           ("Content-Length" . ,(length content))
                           ("X-Line-Signature" . ,x-line-signature)
                           ("User-Agent" . "LineBotWebhook/1.0"))
                :content content))))

(defmethod lack.component:call ((app mock-app) env)
  (let ((authorization (gethash "authorization" (getf env :headers))))
    (unless authorization
      (return-from lack.component:call
        '(400 () ("Invalid authorization header"))))

    (destructuring-bind (&optional token)
        (re-groups "^Bearer (.+)$" authorization)
      (unless (equal token (slot-value app 'channel-access-token))
        (return-from lack.component:call
          '(400 () ("Invalid authorization header"))))))

  (vector-push-extend (make-request env) (slot-value app 'buffer))

  ;; Get profile
  (when (and (starts-with-subseq "/profile/" (getf env :path-info))
             (eq (getf env :request-method) :get))
    (return-from lack.component:call
      `(200 (:content-type "application/json")
            (,(jojo:to-json
               `(("displayName" . "Lisp Alien")
                 ("userId" . ,(subseq (getf env :path-info) 9))
                 ("pictureUrl" . nil)
                 ("statusMessage" . nil))
               :from :alist)))))

  '(200 () ("ok")))

(defmacro subtest-lineapp (desc app &body body)
  (with-gensyms (app-port mock-app-port
                 channel-secret channel-access-token
                 callback-path acceptor mock-acceptor)
    (once-only (app)
      `(let* ((,app-port (random-port))
              (,mock-app-port (random-port))
              (,channel-secret (setf (slot-value ,app 'linebot/handler::channel-secret)
                                     (or (slot-value ,app 'linebot/handler::channel-secret)
                                         (make-random-string 32))))
              (,channel-access-token (setf (slot-value ,app 'linebot/handler::channel-access-token)
                                           (or (slot-value ,app 'linebot/handler::channel-access-token)
                                               (make-random-string 128))))
              (,callback-path (slot-value ,app 'linebot/app::callback))
              (*mock-app* (make-instance 'mock-app
                                         :channel-secret ,channel-secret
                                         :channel-access-token ,channel-access-token
                                         :webhook-url
                                         (format nil "http://127.0.0.1:~A~A"
                                                 ,app-port
                                                 ,callback-path)))
              (bt:*default-special-bindings*
                `((linebot:*message-api-endpoint* . ,(format nil "http://127.0.0.1:~A/" ,mock-app-port)))))
         (let ((,acceptor (clack:clackup ,app :port ,app-port :silent t))
               (,mock-acceptor (clack:clackup *mock-app* :port ,mock-app-port :silent t)))
           (unwind-protect (subtest ,desc ,@body)
             (clack:stop ,acceptor)
             (clack:stop ,mock-acceptor)))))))

(defun dummy-user-id ()
  (concatenate 'string '(#\U) (make-random-string 32)))

(defun dummy-reply-token ()
  (make-random-string 30))

(defun current-timestamp ()
  (let ((now (local-time:now)))
    (+ (* (local-time:timestamp-to-unix now) 1000)
       (local-time:timestamp-millisecond now))))
