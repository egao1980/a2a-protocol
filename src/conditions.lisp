(in-package #:a2a-protocol)

(define-condition a2a-error (error)
  ((message :initarg :message :reader a2a-error-message :initform nil))
  (:report (lambda (c s)
             (format s "a2a error~@[: ~a~]" (a2a-error-message c)))))
