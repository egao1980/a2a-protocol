(in-package #:a2a-protocol/tests)

(defun %echo-agent ()
  (a2a-protocol:make-a2a-agent :name "echo"))

(defun %wired ()
  (let* ((agent (%echo-agent))
         (transport (rpc-backend-inprocess:make-inprocess-rpc-transport)))
    (a2a-protocol:serve-a2a agent :transport transport)
    (values agent transport)))

(defun %artifact-text (task)
  (a2a-protocol:a2a-part-text
   (first (a2a-protocol:a2a-artifact-parts
           (first (a2a-protocol:a2a-task-artifacts task))))))

(deftest classes-exist
  (ok (find-class 'a2a-protocol:agent-card))
  (ok (find-class 'a2a-protocol:a2a-task))
  (ok (find-class 'a2a-protocol:a2a-backend))
  (ok (find-class 'a2a-protocol:a2a-agent))
  (ok (equal "1.0" a2a-protocol:+a2a-protocol-version+))
  (ok (equal -32001 a2a-protocol:+a2a-error-task-not-found+))
  (ok (equal -32009 a2a-protocol:+a2a-error-version-not-supported+)))

(deftest-parametrize task-state-roundtrip
    ((lisp wire)
     (:submitted "TASK_STATE_SUBMITTED")
     (:working "TASK_STATE_WORKING")
     (:completed "TASK_STATE_COMPLETED")
     (:failed "TASK_STATE_FAILED")
     (:canceled "TASK_STATE_CANCELED")
     (:rejected "TASK_STATE_REJECTED")
     (:input-required "TASK_STATE_INPUT_REQUIRED")
     (:auth-required "TASK_STATE_AUTH_REQUIRED"))
  (ok (equal wire (a2a-protocol:task-state-to-wire lisp)))
  (ok (eq lisp (a2a-protocol:task-state-from-wire wire))))

(deftest task-state-short-names
  (ok (eq :completed (a2a-protocol:task-state-from-wire "completed")))
  (ok (eq :canceled (a2a-protocol:task-state-from-wire "cancelled")))
  (ok (eq :input-required (a2a-protocol:task-state-from-wire "input_required"))))

(deftest role-roundtrip
  (ok (equal "ROLE_USER" (a2a-protocol:role-to-wire :user)))
  (ok (equal "ROLE_AGENT" (a2a-protocol:role-to-wire :agent)))
  (ok (eq :user (a2a-protocol:role-from-wire "ROLE_USER")))
  (ok (eq :agent (a2a-protocol:role-from-wire "agent"))))

(deftest message-json-roundtrip
  (let* ((msg (a2a-protocol:make-a2a-message
               :role :user :text "hi" :message-id "msg-1"))
         (decoded (a2a-protocol:decode-message (a2a-protocol:encode-message msg))))
    (ok (equal "msg-1" (a2a-protocol:a2a-message-id decoded)))
    (ok (eq :user (a2a-protocol:a2a-message-role decoded)))
    (ok (equal "hi" (a2a-protocol:message-text decoded)))))

(deftest card-json-roundtrip
  (let* ((card (a2a-protocol:make-agent-card
                :name "Recipe"
                :description "cooks"
                :supported-interfaces
                (list (a2a-protocol:make-agent-interface "https://example.com/a2a"
                                                         :protocol-binding "JSONRPC"))
                :skills (list (a2a-protocol:make-agent-skill "echo" :name "Echo"))))
         (decoded (a2a-protocol:decode-agent-card
                   (a2a-protocol:decode-json
                    (a2a-protocol:encode-json (a2a-protocol:encode-agent-card card))))))
    (ok (equal "Recipe" (a2a-protocol:agent-card-name decoded)))
    (ok (equal "https://example.com/a2a"
               (a2a-protocol:agent-interface-url
                (first (a2a-protocol:agent-card-supported-interfaces decoded)))))
    (ok (equal "echo" (a2a-protocol:agent-skill-id
                       (first (a2a-protocol:agent-card-skills decoded)))))))

(deftest local-echo-send
  (let* ((agent (%echo-agent))
         (result (a2a-protocol:send-message
                  agent (a2a-protocol:make-a2a-message :text "pong"))))
    (ok (typep result 'a2a-protocol:a2a-task))
    (ok (eq :completed (a2a-protocol:a2a-task-state result)))
    (ok (equal "pong" (%artifact-text result)))))

(deftest dispatch-send-and-get
  (let* ((agent (%echo-agent))
         (params (a2a-protocol:json-object
                  "message" (a2a-protocol:encode-message
                             (a2a-protocol:make-a2a-message :text "hi"))))
         (sent (a2a-protocol:dispatch-a2a-method agent "SendMessage" params))
         (task (a2a-protocol:decode-send-result sent)))
    (ok (hash-table-p (gethash "task" sent)))
    (ok (eq :completed (a2a-protocol:a2a-task-state task)))
    (ok (equal "hi" (%artifact-text task)))
    (let ((got (a2a-protocol:dispatch-a2a-method
                agent "GetTask"
                (a2a-protocol:json-object "id" (a2a-protocol:a2a-task-id task)))))
      (ok (equal (a2a-protocol:a2a-task-id task) (gethash "id" got)))
      (ok (equal "TASK_STATE_COMPLETED"
                 (gethash "state" (gethash "status" got)))))))

(deftest dispatch-slash-alias
  (let* ((agent (%echo-agent))
         (sent (a2a-protocol:dispatch-a2a-method
                agent "message/send"
                (a2a-protocol:json-object
                 "message" (a2a-protocol:encode-message
                            (a2a-protocol:make-a2a-message :text "alias")))))
         (task (a2a-protocol:decode-send-result sent)))
    (ok (equal "alias" (%artifact-text task)))))

(deftest cancel-and-errors
  (let ((agent (%echo-agent)))
    (ok (signals (a2a-protocol:dispatch-a2a-method
                  agent "GetTask" (a2a-protocol:json-object "id" "missing"))
                 'a2a-protocol:a2a-error))
    (let* ((task (a2a-protocol:send-message
                  agent (a2a-protocol:make-a2a-message :text "x") :blocking nil)))
      (ok (eq :submitted (a2a-protocol:a2a-task-state task)))
      (let ((canceled (a2a-protocol:cancel-task agent (a2a-protocol:a2a-task-id task))))
        (ok (eq :canceled (a2a-protocol:a2a-task-state canceled))))
      (ok (signals (a2a-protocol:cancel-task agent (a2a-protocol:a2a-task-id task))
                   'a2a-protocol:a2a-error)))))

(deftest list-tasks-and-extended-card
  (let ((agent (%echo-agent)))
    (a2a-protocol:send-message agent (a2a-protocol:make-a2a-message :text "a"))
    (a2a-protocol:send-message agent (a2a-protocol:make-a2a-message :text "b"))
    (let ((listed (a2a-protocol:dispatch-a2a-method agent "ListTasks"
                                                    (a2a-protocol:json-object))))
      (ok (eql 2 (gethash "totalSize" listed)))
      (ok (equal "" (gethash "nextPageToken" listed)))
      (ok (null (gethash "artifacts" (elt (gethash "tasks" listed) 0)))))
    (ok (signals (a2a-protocol:dispatch-a2a-method
                  agent "GetExtendedAgentCard" (a2a-protocol:json-object))
                 'a2a-protocol:a2a-error))
    (setf (a2a-protocol:a2a-agent-extended-card agent)
          (a2a-protocol:a2a-agent-card agent))
    (ok (equal "echo"
               (gethash "name"
                        (a2a-protocol:dispatch-a2a-method
                         agent "GetExtendedAgentCard" (a2a-protocol:json-object)))))))

(deftest stream-echo
  (let* ((agent (%echo-agent))
         (events nil)
         (stream (a2a-protocol:stream-message
                  agent (a2a-protocol:make-a2a-message :text "stream")
                  :on-event (lambda (ev) (push ev events)))))
    (ok (typep stream 'a2a-protocol:a2a-stream-result))
    (ok (= 3 (length (a2a-protocol:a2a-stream-events stream))))
    (ok (gethash "task" (first (a2a-protocol:a2a-stream-events stream))))
    (ok (gethash "artifactUpdate" (second (a2a-protocol:a2a-stream-events stream))))
    (ok (gethash "statusUpdate" (third (a2a-protocol:a2a-stream-events stream))))))

(deftest version-not-supported
  (ok (signals (a2a-protocol:dispatch-a2a-method
                (%echo-agent) "SendMessage"
                (a2a-protocol:json-object
                 "protocolVersion" "9.9"
                 "message" (a2a-protocol:encode-message
                            (a2a-protocol:make-a2a-message :text "x"))))
               'a2a-protocol:a2a-error)))

(deftest inprocess-rpc
  (multiple-value-bind (agent transport)
      (%wired)
    (declare (ignore agent))
    (let* ((result (rpc-protocol:rpc-call
                    "SendMessage"
                    (a2a-protocol:json-object
                     "message" (a2a-protocol:encode-message
                                (a2a-protocol:make-a2a-message :text "rpc")))
                    :transport transport))
           (task (a2a-protocol:decode-send-result result)))
      (ok (eq :completed (a2a-protocol:a2a-task-state task)))
      (ok (equal "rpc" (%artifact-text task)))
      (let ((got (rpc-protocol:rpc-call
                  "GetTask"
                  (a2a-protocol:json-object "id" (a2a-protocol:a2a-task-id task))
                  :transport transport)))
        (ok (equal (a2a-protocol:a2a-task-id task) (gethash "id" got)))))))

(deftest unknown-method
  (ok (signals (a2a-protocol:dispatch-a2a-method
                (%echo-agent) "NoSuchMethod" (a2a-protocol:json-object))
               'rpc-protocol:rpc-error)))
