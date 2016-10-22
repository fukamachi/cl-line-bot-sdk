(in-package #:cl-user)
(defpackage #:linebot/errors
  (:use #:cl)
  (:export #:api-error
           #:invalid-signature
           #:invalid-json-content
           #:invalid-event-type
           #:invalid-message-type
           #:invalid-source-type
           #:invalid-beacon-type))
(in-package #:linebot/errors)

(define-condition api-error (error) ())

(define-condition invalid-signature (api-error)
  ((signature :initarg :signature))
  (:report (lambda (error stream)
             (format stream "Invalid X-Line-Signature header: ~S"
                     (slot-value error 'signature)))))

(define-condition invalid-json-content (api-error)
  ((content :initarg :content))
  (:report (lambda (error stream)
             (format stream "Failed to decode JSON content:~%  ~S"
                     (slot-value error 'content)))))

(define-condition invalid-event-type (api-error)
  ((type :initarg :type))
  (:report (lambda (error stream)
             (format stream "Invalid event type: ~S" (slot-value error 'type)))))

(define-condition invalid-message-type (api-error)
  ((type :initarg :type))
  (:report (lambda (error stream)
             (format stream "Invalid message type: ~S" (slot-value error 'type)))))

(define-condition invalid-source-type (api-error)
  ((type :initarg :type))
  (:report (lambda (error stream)
             (format stream "Invalid source type: ~S" (slot-value error 'type)))))

(define-condition invalid-beacon-type (api-error)
  ((type :initarg :type))
  (:report (lambda (error stream)
             (format stream "Invalid beacon type: ~S" (slot-value error 'type)))))
