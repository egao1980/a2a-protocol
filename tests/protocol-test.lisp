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
    (handler-case
        (progn
          (a2a-protocol:dispatch-a2a-method
           agent "GetExtendedAgentCard" (a2a-protocol:json-object))
          (fail "expected unsupported extended card"))
      (a2a-protocol:a2a-error (c)
        (ok (eql a2a-protocol:+a2a-error-unsupported-operation+
                 (a2a-protocol:a2a-error-code c)))))
    (setf (a2a-protocol:agent-card-capabilities (a2a-protocol:a2a-agent-card agent))
          '(:streaming t :push-notifications nil :extended-agent-card t)
          (a2a-protocol:a2a-agent-extended-card agent)
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

(deftest card-1.0-shape
  (let ((encoded (a2a-protocol:encode-agent-card
                  (a2a-protocol:a2a-agent-card (%echo-agent)))))
    (ok (null (gethash "protocolVersion" encoded)))
    (ok (null (gethash "url" encoded)))
    (ok (plusp (length (gethash "supportedInterfaces" encoded))))
    (ok (equal "1.0"
               (gethash "protocolVersion"
                        (elt (gethash "supportedInterfaces" encoded) 0))))))

(deftest empty-version-is-0.3
  (ok (hash-table-p
       (a2a-protocol:dispatch-a2a-method
        (%echo-agent) "SendMessage"
        (a2a-protocol:json-object
         "protocolVersion" ""
         "message" (a2a-protocol:encode-message
                    (a2a-protocol:make-a2a-message :text "v03"))))))
  (ok (hash-table-p
       (a2a-protocol:dispatch-a2a-method
        (%echo-agent) "SendMessage"
        (a2a-protocol:json-object
         "A2A-Version" ""
         "message" (a2a-protocol:encode-message
                    (a2a-protocol:make-a2a-message :text "hdr")))))))

(deftest extended-card-declared-but-missing
  (let ((agent (%echo-agent)))
    (setf (a2a-protocol:agent-card-capabilities (a2a-protocol:a2a-agent-card agent))
          '(:streaming t :extended-agent-card t))
    (handler-case
        (progn
          (a2a-protocol:dispatch-a2a-method
           agent "GetExtendedAgentCard" (a2a-protocol:json-object))
          (fail "expected not configured"))
      (a2a-protocol:a2a-error (c)
        (ok (eql a2a-protocol:+a2a-error-extended-card-not-configured+
                 (a2a-protocol:a2a-error-code c)))))))

(deftest list-tasks-paginates-and-sorts
  (let ((agent (%echo-agent)))
    (let ((old (a2a-protocol:send-message
                agent (a2a-protocol:make-a2a-message :text "old")))
          (new (a2a-protocol:send-message
                agent (a2a-protocol:make-a2a-message :text "new"))))
      (setf (a2a-protocol:task-status-timestamp (a2a-protocol:a2a-task-status old))
            "2020-01-01T00:00:00Z"
            (a2a-protocol:task-status-timestamp (a2a-protocol:a2a-task-status new))
            "2024-01-01T00:00:00Z"))
    (let ((page1 (a2a-protocol:dispatch-a2a-method
                  agent "ListTasks"
                  (a2a-protocol:json-object "pageSize" 1))))
      (ok (eql 2 (gethash "totalSize" page1)))
      (ok (equal "1" (gethash "nextPageToken" page1)))
      (ok (eql 1 (length (gethash "tasks" page1))))
      (let* ((first-id (gethash "id" (elt (gethash "tasks" page1) 0)))
             (page2 (a2a-protocol:dispatch-a2a-method
                     agent "ListTasks"
                     (a2a-protocol:json-object "pageSize" 1 "pageToken" "1")))
             (second-id (gethash "id" (elt (gethash "tasks" page2) 0))))
        (ok (not (equal first-id second-id)))
        (ok (equal "" (gethash "nextPageToken" page2)))))))

(deftest context-id-mismatch
  (let* ((agent (%echo-agent))
         (task (a2a-protocol:send-message
                agent (a2a-protocol:make-a2a-message :text "keep") :blocking nil)))
    (handler-case
        (progn
          (a2a-protocol:send-message
           agent
           (a2a-protocol:make-a2a-message
            :text "bad"
            :task-id (a2a-protocol:a2a-task-id task)
            :context-id "other-ctx"))
          (fail "expected context mismatch"))
      (a2a-protocol:a2a-error (c)
        (ok (eql rpc-protocol:+invalid-params+ (a2a-protocol:a2a-error-code c)))))))

(deftest push-is-32003
  (handler-case
      (progn
        (a2a-protocol:dispatch-a2a-method
         (%echo-agent) "CreateTaskPushNotificationConfig"
         (a2a-protocol:json-object))
        (fail "expected push not supported"))
    (a2a-protocol:a2a-error (c)
      (ok (eql a2a-protocol:+a2a-error-push-not-supported+
               (a2a-protocol:a2a-error-code c))))))

(deftest task-not-found-is-typed
  (handler-bind ((a2a-protocol:a2a-task-not-found
                  (lambda (c)
                    (ok (eq :task-not-found (a2a-protocol:a2a-error-reason c)))
                    (ok (eql a2a-protocol:+a2a-error-task-not-found+
                             (a2a-protocol:a2a-error-code c)))
                    (use-value
                     (a2a-protocol:make-a2a-task :id "supplied")
                     c))))
    (let ((task (a2a-protocol:get-task (%echo-agent) "missing")))
      (ok (equal "supplied" (a2a-protocol:a2a-task-id task))))))

(deftest missing-task-reason-without-restart
  (handler-case
      (a2a-protocol:get-task (%echo-agent) "gone")
    (a2a-protocol:a2a-error (c)
      (ok (typep c 'a2a-protocol:a2a-task-not-found))
      (ok (eq :task-not-found (a2a-protocol:a2a-error-reason c))))))
