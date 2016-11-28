# cl-line-bot-sdk

[![Build Status](https://travis-ci.org/fukamachi/cl-line-bot-sdk.svg?branch=master)](https://travis-ci.org/fukamachi/cl-line-bot-sdk)
[![Coverage Status](https://coveralls.io/repos/github/fukamachi/cl-line-bot-sdk/badge.svg?branch=master)](https://coveralls.io/github/fukamachi/cl-line-bot-sdk?branch=master)

Common Lisp SDK for the [LINE Messaging API](https://devdocs.line.me/en/).

## Usage

```common-lisp
(ql:quickload :linebot/app)

(defclass echo-app (linebot/app:app) ())

(defmethod linebot:handle-message-event ((handler echo-app)
                                         (event linebot:message-event)
                                         (message linebot:text-message))
  (linebot:reply-message
   (make-instance 'linebot:text-send-message
                  :text (linebot:message-text message))))

(make-instance 'echo-app
               :channel-secret "<channel secret>"
               :channel-access-token "<channel access token>"
               :callback "/callback")
```

```
$ clackup echo.lisp
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
