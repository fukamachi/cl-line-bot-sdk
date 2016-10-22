# cl-line-bot-sdk

[![Build Status](https://travis-ci.org/fukamachi/cl-line-bot-sdk.svg?branch=master)](https://travis-ci.org/fukamachi/cl-line-bot-sdk)
[![Coverage Status](https://coveralls.io/repos/github/fukamachi/cl-line-bot-sdk/badge.svg?branch=master)](https://coveralls.io/github/fukamachi/cl-line-bot-sdk?branch=master)

Common Lisp SDK for the [LINE Messaging API](https://devdocs.line.me/en/).

## Usage

```common-lisp
(setf linebot:*channel-secret* "<channel secret>")
(setf linebot:*channel-access-token* "<channel access token>")

(lambda (env)
  (block nil
    (unless (and (eq (getf env :request-method) :post)
                 (eq (getf env :request-path) "/callback"))
      (return '(404 () ("Not Found"))))

    (let ((req (lack.request:make-request env))
          (headers (getf env :headers)))
      (unless (linebot:validate-signature (lack.request:request-content req)
                                          (gethash "x-line-signature" headers))
        (return '(400 () ("Invalid signature"))))

      (let ((events (linebot:parse-request (lack.request:request-content req))))
        (dolist (event events)
          (when (and (eq (linebot:event-type event) :message)
                     (typep (linebot:message-event-message event) 'linebot:text-message))
            (linebot:reply-message
             (linebot:event-reply-token event)
             (make-instance 'linebot:text-send-message
                            :text (linebot:event-message-text event))))))

      '(200 () ("ok")))))
```

```
$ clackup examples/echo.lisp
```

## Installation

```
$ ros install fukamachi/cl-line-bot-sdk
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.
