(in-package #:cl-user)
(defpackage #:linebot/models/base
  (:use #:cl)
  (:import-from #:closer-mop
                #:class-slots
                #:slot-definition-name)
  (:import-from #:jonathan
                #:%to-json
                #:with-object
                #:write-key-value)
  (:import-from #:local-time
                #:timestamp
                #:timestamp-to-unix
                #:timestamp-millisecond)
  (:import-from #:kebab
                #:to-camel-case)
  (:export #:json-serializable))
(in-package #:linebot/models/base)

(defclass json-serializable () ())

(defmethod jojo:%to-json ((object json-serializable))
  (jojo:with-object
    (loop for slot in (c2mop:class-slots (class-of object))
          for slot-name = (c2mop:slot-definition-name slot)
          when (slot-boundp object slot-name)
            do (let ((slot-value (slot-value object slot-name)))
                 (jojo:write-key-value (kebab:to-camel-case (symbol-name slot-name))
                                       (typecase slot-value
                                         (null :null)
                                         (keyword (string-downcase slot-value))
                                         (timestamp
                                          (+ (* (timestamp-to-unix slot-value) 1000)
                                             (timestamp-millisecond slot-value)))
                                         (otherwise slot-value)))))))
