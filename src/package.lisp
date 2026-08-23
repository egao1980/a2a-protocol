(defpackage #:a2a-protocol
  (:use #:cl)
  (:nicknames #:stack-a2a)
  (:export #:a2a-error
           #:a2a-error-message
           #:a2a-backend
           #:agent-card
           #:a2a-task
           #:a2a-message
           #:a2a-artifact
           #:*a2a-backend*
           #:fetch-agent-card
           #:serve-agent-card
           #:send-message
           #:stream-message
           #:get-task
           #:cancel-task
           #:resubscribe-task))

(in-package #:a2a-protocol)
