(asdf:defsystem #:linebot-tests
  :depends-on (:linebot
               :prove)
  :components
  ((:test-file "tests/webhook"))

  :defsystem-depends-on (:prove-asdf)
  :perform (asdf:test-op :after (op c)
             (funcall (intern #.(string :run-test-system) :prove) c)))
