(in-package #:cl-user)

#-asdf3.1 (error "LINEBOT requires ASDF 3.1")
(asdf:defsystem #:linebot
  :class :package-inferred-system
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on ("linebot/package"
               :uiop)
  :description "SDK for the LINE Messaging API for Common Lisp"
  :in-order-to ((asdf:test-op (asdf:test-op linebot-tests))))
