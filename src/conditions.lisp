(in-package #:a2a-protocol)

;;; Typed A2A errors + restarts. Wire still uses integer codes; handler-bind
;;; uses A2A-ERROR-REASON or a subclass instead of switching on -32001.

(defparameter *a2a-error-code-reasons*
  '((-32001 . :task-not-found)
    (-32002 . :not-cancelable)
    (-32003 . :push-not-supported)
    (-32004 . :unsupported)
    (-32005 . :content-type-not-supported)
    (-32006 . :invalid-response)
    (-32007 . :extended-card-not-configured)
    (-32008 . :extension-support-required)
    (-32009 . :version-not-supported)))

(defun a2a-code-reason (code)
  (or (cdr (assoc code *a2a-error-code-reasons*)) :unknown))

(defun a2a-reason-code (reason)
  (or (car (rassoc reason *a2a-error-code-reasons*)) -32603))

(define-condition a2a-error (error)
  ((message :initarg :message :reader a2a-error-message :initform "A2A error")
   (code :initarg :code :reader a2a-error-code :initform -32603)
   (data :initarg :data :reader a2a-error-data :initform nil)
   (reason :initarg :reason :reader a2a-error-reason :initform nil)
   (cause :initarg :cause :reader a2a-error-cause :initform nil))
  (:report (lambda (c s)
             (format s "~A~@[ [~A]~]" (a2a-error-message c) (a2a-error-code c)))))

(defmethod initialize-instance :after ((c a2a-error) &key)
  (unless (a2a-error-reason c)
    (setf (slot-value c 'reason) (a2a-code-reason (a2a-error-code c)))))

(define-condition a2a-task-not-found (a2a-error) ()
  (:default-initargs :code -32001 :reason :task-not-found))

(define-condition a2a-task-not-cancelable (a2a-error) ()
  (:default-initargs :code -32002 :reason :not-cancelable))

(define-condition a2a-push-not-supported (a2a-error) ()
  (:default-initargs :code -32003 :reason :push-not-supported))

(define-condition a2a-unsupported (a2a-error) ()
  (:default-initargs :code -32004 :reason :unsupported))

(define-condition a2a-content-type-not-supported (a2a-error) ()
  (:default-initargs :code -32005 :reason :content-type-not-supported))

(define-condition a2a-invalid-response (a2a-error) ()
  (:default-initargs :code -32006 :reason :invalid-response))

(define-condition a2a-extended-card-not-configured (a2a-error) ()
  (:default-initargs :code -32007 :reason :extended-card-not-configured))

(define-condition a2a-extension-support-required (a2a-error) ()
  (:default-initargs :code -32008 :reason :extension-support-required))

(define-condition a2a-version-not-supported (a2a-error) ()
  (:default-initargs :code -32009 :reason :version-not-supported))

(defun a2a-reason-class (reason)
  (case reason
    (:task-not-found 'a2a-task-not-found)
    (:not-cancelable 'a2a-task-not-cancelable)
    (:push-not-supported 'a2a-push-not-supported)
    (:unsupported 'a2a-unsupported)
    (:content-type-not-supported 'a2a-content-type-not-supported)
    (:invalid-response 'a2a-invalid-response)
    (:extended-card-not-configured 'a2a-extended-card-not-configured)
    (:extension-support-required 'a2a-extension-support-required)
    (:version-not-supported 'a2a-version-not-supported)
    (t 'a2a-error)))

(defun signal-a2a-error (&key message code data reason cause)
  (let* ((reason (or reason (and code (a2a-code-reason code)) :unknown))
         (code (or code (a2a-reason-code reason))))
    (error (a2a-reason-class reason)
           :message (or message "A2A error")
           :code code :data data :reason reason :cause cause)))

;;; --- restart helpers -------------------------------------------------------

(defun call-with-a2a-restarts (thunk)
  "Establish RETRY / USE-VALUE around THUNK."
  (tagbody
   :retry
     (return-from call-with-a2a-restarts
       (restart-case (funcall thunk)
         (retry ()
           :report "Retry the A2A operation"
           (go :retry))
         (use-value (value)
           :report "Use a supplied value instead"
           :interactive (lambda ()
                          (format *query-io* "Value to use: ")
                          (force-output *query-io*)
                          (list (read *query-io*)))
           value)))))

(defmacro with-a2a-restarts (&body body)
  `(call-with-a2a-restarts (lambda () ,@body)))

(defun invoke-retry (&optional condition)
  (let ((r (find-restart 'retry condition)))
    (when r (invoke-restart r))))

(defun invoke-use-value (value &optional condition)
  (let ((r (find-restart 'use-value condition)))
    (when r (invoke-restart r value))))

(defun auto-retry (condition)
  (when (find-restart 'retry condition)
    (invoke-retry condition)))

(defmacro with-auto-retry (&body body)
  `(handler-bind ((a2a-error #'auto-retry))
     (with-a2a-restarts ,@body)))
