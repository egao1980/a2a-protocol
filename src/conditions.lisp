(in-package #:a2a-protocol)

(define-condition a2a-error (error)
  ((message :initarg :message :reader a2a-error-message :initform "A2A error")
   (code :initarg :code :reader a2a-error-code :initform -32603)
   (data :initarg :data :reader a2a-error-data :initform nil))
  (:report (lambda (c s)
             (format s "~A~@[ [~A]~]" (a2a-error-message c) (a2a-error-code c)))))
