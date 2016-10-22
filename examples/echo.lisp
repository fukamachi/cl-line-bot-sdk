(let ((asdf:*central-registry*
        (cons (make-pathname :directory (butlast (pathname-directory
                                                  (or *load-pathname* *compile-file-pathname*))))
              asdf:*central-registry*)))
  (ql:quickload '(:clack :lack-request :linebot) :silent t))

(defparameter linebot:*channel-secret* (uiop:getenv "LINE_CHANNEL_SECRET"))
(defparameter linebot:*channel-access-token* (uiop:getenv "LINE_CHANNEL_ACCESS_TOKEN"))

(lambda (env)
  (block nil
    (unless (and (eq (getf env :request-method) :post)
                 (string= (getf env :path-info) "/callback"))
      (return '(404 () ("Not Found"))))

    (let ((req (lack.request:make-request env))
          (headers (getf env :headers)))
      (unless (linebot:validate-signature (lack.request:request-content req)
                                          (gethash "x-line-signature" headers))
        (return '(400 () ("Invalid signature"))))

      (let ((events (linebot:parse-request (lack.request:request-content req))))
        (dolist (event events)
          (when (and (eq (linebot:event-type event) :message)
                     (typep (linebot:event-message event) 'linebot:text-message))
            (linebot:reply-message
             (linebot:event-reply-token event)
             (make-instance 'linebot:text-send-message
                            :text (linebot:event-message-text event))))))
      '(200 () ("ok")))))
