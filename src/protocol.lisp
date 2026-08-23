(in-package #:a2a-protocol)

(defclass a2a-backend () ())
(defclass agent-card () ())
(defclass a2a-task () ())
(defclass a2a-message () ())
(defclass a2a-artifact () ())

(defvar *a2a-backend* nil)

(defgeneric fetch-agent-card (backend url &key))
(defgeneric serve-agent-card (backend card &key))
(defgeneric send-message (backend message &key task-id blocking))
(defgeneric stream-message (backend message &key on-event))
(defgeneric get-task (backend task-id &key))
(defgeneric cancel-task (backend task-id &key))
(defgeneric resubscribe-task (backend task-id &key on-event))

(defun %ensure-backend (&optional (backend *a2a-backend*))
  (or backend
      (error 'a2a-error :message "*a2a-backend* is nil — load an a2a-backend-*")))
