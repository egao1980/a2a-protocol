(in-package #:a2a-protocol)

;;; A2A 1.0. JSON-RPC via rpc-protocol — do not invent a second codec.
;;; Methods: PascalCase (SendMessage, …). Slash aliases accepted on dispatch.

(defgeneric fetch-agent-card (backend url &key))
(defgeneric serve-agent-card (backend card &key))
(defgeneric send-message (backend message &key task-id blocking))
(defgeneric stream-message (backend message &key on-event))
(defgeneric get-task (backend task-id &key history-length))
(defgeneric list-tasks (backend &key context-id status page-size page-token
                                  history-length include-artifacts
                                  status-timestamp-after))
(defgeneric cancel-task (backend task-id &key))
(defgeneric resubscribe-task (backend task-id &key on-event))

(defmethod fetch-agent-card :around (backend url &rest args)
  (declare (ignore backend url args))
  (with-a2a-restarts (call-next-method)))

(defmethod serve-agent-card :around (backend card &rest args)
  (declare (ignore backend card args))
  (with-a2a-restarts (call-next-method)))

(defmethod send-message :around (backend message &rest args)
  (declare (ignore backend message args))
  (with-a2a-restarts (call-next-method)))

(defmethod stream-message :around (backend message &rest args)
  (declare (ignore backend message args))
  (with-a2a-restarts (call-next-method)))

(defmethod get-task :around (backend task-id &rest args)
  (declare (ignore backend task-id args))
  (with-a2a-restarts (call-next-method)))

(defmethod list-tasks :around (backend &rest args)
  (declare (ignore backend args))
  (with-a2a-restarts (call-next-method)))

(defmethod cancel-task :around (backend task-id &rest args)
  (declare (ignore backend task-id args))
  (with-a2a-restarts (call-next-method)))

(defmethod resubscribe-task :around (backend task-id &rest args)
  (declare (ignore backend task-id args))
  (with-a2a-restarts (call-next-method)))

(defun %ensure-backend (&optional (backend *a2a-backend*))
  (or backend
      (signal-a2a-error :message "*a2a-backend* is nil — load an a2a-backend-*")))

(defun %ensure-params (params)
  (cond
    ((hash-table-p params) params)
    ((null params) (json-object))
    (t (json-object))))

(defun %canonical-method (method)
  (cond
    ((member method '("SendMessage" "message/send") :test #'string=) :send-message)
    ((member method '("SendStreamingMessage" "message/stream") :test #'string=)
     :stream-message)
    ((member method '("GetTask" "tasks/get") :test #'string=) :get-task)
    ((member method '("ListTasks" "tasks/list") :test #'string=) :list-tasks)
    ((member method '("CancelTask" "tasks/cancel") :test #'string=) :cancel-task)
    ((member method '("SubscribeToTask" "tasks/resubscribe" "tasks/subscribe")
             :test #'string=)
     :subscribe)
    ((member method '("GetExtendedAgentCard" "agent/getAuthenticatedExtendedCard")
             :test #'string=)
     :extended-card)
    ((member method '("CreateTaskPushNotificationConfig"
                      "GetTaskPushNotificationConfig"
                      "ListTaskPushNotificationConfig"
                      "DeleteTaskPushNotificationConfig"
                      "tasks/pushNotificationConfig/set"
                      "tasks/pushNotificationConfig/get"
                      "tasks/pushNotificationConfig/list"
                      "tasks/pushNotificationConfig/delete")
             :test #'string=)
     :push)
    (t nil)))

(defun %normalize-a2a-version (ver)
  "Empty / absent A2A-Version MUST be treated as 0.3."
  (if (or (null ver) (and (stringp ver) (zerop (length (string-trim '(#\Space) ver)))))
      "0.3"
      ver))

(defun %check-version (params &optional header-version)
  (let ((ver (%normalize-a2a-version
              (or header-version
                  (param params "A2A-Version")
                  (param params "protocolVersion")))))
    (unless (member ver *supported-protocol-versions* :test #'string=)
      (signal-a2a-error
             :message "Version not supported"
             :code +a2a-error-version-not-supported+
             :data (json-object "supported" (coerce *supported-protocol-versions* 'vector)
                                "requested" ver)))))

(defun %store-task (agent task)
  (setf (gethash (a2a-task-id task) (a2a-agent-tasks agent)) task)
  task)

(defun %find-task (agent task-id)
  (or (gethash task-id (a2a-agent-tasks agent))
      (signal-a2a-error
             :message (format nil "task ~s not found" task-id)
             :code +a2a-error-task-not-found+)))

(defun %require-task-id (params)
  (or (param params "id")
      (signal-a2a-error :message "missing task id"
                        :code rpc-protocol:+invalid-params+)))

(defun default-echo-handler (agent message task &key)
  (declare (ignore agent))
  (let ((text (or (message-text message) "")))
    (setf (a2a-task-state task) :completed
          (a2a-task-artifacts task)
          (list (make-a2a-artifact
                 :name "echo"
                 :parts (list (make-text-part text)))))
    task))

(defun %run-handler (agent message task)
  (let ((fn (or (a2a-agent-handler agent) #'default-echo-handler)))
    (funcall fn agent message task)))

(defun %history-length (params)
  (let ((cfg (param params "configuration")))
    (or (param params "historyLength")
        (param cfg "historyLength"))))

(defun %return-immediately (params blocking)
  (let ((cfg (param params "configuration")))
    (cond
      ((not (eq blocking :default)) (not blocking))
      (cfg (and (param cfg "returnImmediately") t))
      (t nil))))

(defun %prepare-task (agent message)
  (let* ((existing-id (a2a-message-task-id message))
         (task (if existing-id
                   (let ((found (%find-task agent existing-id)))
                     (when (terminal-state-p (a2a-task-state found))
                       (signal-a2a-error
                              :message "task is in a terminal state"
                              :code +a2a-error-unsupported-operation+))
                     (let ((msg-ctx (a2a-message-context-id message))
                           (task-ctx (a2a-task-context-id found)))
                       (when (and msg-ctx task-ctx (not (equal msg-ctx task-ctx)))
                         (signal-a2a-error
                                :message "contextId does not match task"
                                :code rpc-protocol:+invalid-params+)))
                     found)
                   (make-a2a-task
                    :context-id (or (a2a-message-context-id message)
                                    (make-id "ctx"))
                    :state :submitted))))
    (setf (a2a-message-task-id message) (a2a-task-id task)
          (a2a-message-context-id message) (a2a-task-context-id task)
          (a2a-task-history task) (append (a2a-task-history task) (list message)))
    (%store-task agent task)))

(defun %apply-history-length (task history-length)
  (when (and history-length (a2a-task-history task))
    (setf (a2a-task-history task)
          (%slice-history (a2a-task-history task) history-length)))
  task)

;;; --- local agent GFs ------------------------------------------------------

(defmethod fetch-agent-card ((agent a2a-agent) url &key)
  (declare (ignore url))
  (a2a-agent-card agent))

(defmethod serve-agent-card ((agent a2a-agent) card &key)
  (setf (a2a-agent-card agent) card))

(defmethod send-message ((agent a2a-agent) message &key task-id (blocking t))
  (when task-id
    (setf (a2a-message-task-id message) task-id))
  (let ((task (%prepare-task agent message)))
    (if (not blocking)
        (progn
          (setf (a2a-task-state task) :submitted)
          (%store-task agent task))
        (let ((result (%run-handler agent message task)))
          (cond
            ((typep result 'a2a-task)
             (%store-task agent result))
            ((typep result 'a2a-message)
             result)
            (t
             (signal-a2a-error
                    :message "handler must return a task or message"
                    :code +a2a-error-invalid-agent-response+)))))))

(defmethod stream-message ((agent a2a-agent) message &key on-event)
  (unless (a2a-agent-streaming-p agent)
    (signal-a2a-error :message "streaming is not supported"
                      :code +a2a-error-unsupported-operation+))
  (let* ((task (%prepare-task agent message)))
    (setf (a2a-task-state task) :working)
    (%store-task agent task)
    (let ((events (list (encode-stream-event :task task)))
          (result (%run-handler agent message task)))
      (cond
        ((typep result 'a2a-message)
         (let ((ev (encode-stream-event :message result)))
           (when on-event (funcall on-event ev))
           (make-a2a-stream-result (list ev))))
        ((typep result 'a2a-task)
         (%store-task agent result)
         (when (a2a-task-artifacts result)
           (push (encode-stream-event :artifact-update result :last-chunk t) events))
         (push (encode-stream-event :status-update result) events)
         (let ((ordered (nreverse events)))
           (when on-event
             (mapc on-event ordered))
           (make-a2a-stream-result ordered)))
        (t
         (signal-a2a-error
                :message "handler must return a task or message"
                :code +a2a-error-invalid-agent-response+))))))

(defmethod get-task ((agent a2a-agent) task-id &key history-length)
  (%apply-history-length
   (let ((task (%find-task agent task-id)))
     ;; Copy history slice without mutating stored task.
     (make-a2a-task :id (a2a-task-id task)
                    :context-id (a2a-task-context-id task)
                    :status (a2a-task-status task)
                    :artifacts (a2a-task-artifacts task)
                    :history (a2a-task-history task)
                    :metadata (a2a-task-metadata task)))
   history-length))

(defun %task-timestamp (task)
  (or (task-status-timestamp (a2a-task-status task)) ""))

(defmethod list-tasks ((agent a2a-agent) &key context-id status page-size page-token
                                           history-length include-artifacts
                                           status-timestamp-after)
  (declare (ignore include-artifacts))
  (let* ((all (loop for task being the hash-values of (a2a-agent-tasks agent)
                    when (and (or (null context-id)
                                  (equal context-id (a2a-task-context-id task)))
                              (or (null status)
                                  (eq status (a2a-task-state task)))
                              (or (null status-timestamp-after)
                                  (string> (%task-timestamp task) status-timestamp-after)))
                      collect task))
         (all (sort all #'string> :key #'%task-timestamp))
         (size (or page-size 50))
         (size (min 100 (max 1 size)))
         (start (if (and page-token (stringp page-token) (plusp (length page-token)))
                    (or (parse-integer page-token :junk-allowed t) 0)
                    0))
         (start (min (max 0 start) (length all)))
         (rest (nthcdr start all))
         (page (subseq rest 0 (min size (length rest))))
         (next (when (> (length rest) size)
                 (princ-to-string (+ start size)))))
    (list :tasks (mapcar (lambda (task)
                           (%apply-history-length
                            (make-a2a-task :id (a2a-task-id task)
                                           :context-id (a2a-task-context-id task)
                                           :status (a2a-task-status task)
                                           :artifacts (a2a-task-artifacts task)
                                           :history (a2a-task-history task)
                                           :metadata (a2a-task-metadata task))
                            history-length))
                         page)
          :page-size size
          :total-size (length all)
          :next-page-token (or next ""))))

(defmethod cancel-task ((agent a2a-agent) task-id &key)
  (let ((task (%find-task agent task-id)))
    (when (terminal-state-p (a2a-task-state task))
      (signal-a2a-error
             :message "task is not cancelable"
             :code +a2a-error-task-not-cancelable+))
    (setf (a2a-task-state task) :canceled)
    (%store-task agent task)))

(defmethod resubscribe-task ((agent a2a-agent) task-id &key on-event)
  (unless (a2a-agent-streaming-p agent)
    (signal-a2a-error :message "streaming is not supported"
                      :code +a2a-error-unsupported-operation+))
  (let ((task (%find-task agent task-id)))
    (when (terminal-state-p (a2a-task-state task))
      (signal-a2a-error
             :message "cannot subscribe to a terminal task"
             :code +a2a-error-unsupported-operation+))
    (let ((events (list (encode-stream-event :task task))))
      (when on-event
        (mapc on-event events))
      (make-a2a-stream-result events))))

;;; --- dispatch -------------------------------------------------------------

(defun %capability-p (card key)
  (let ((caps (and card (agent-card-capabilities card))))
    (cond
      ((hash-table-p caps)
       (and (param caps (ecase key
                          (:extended-agent-card "extendedAgentCard")
                          (:push-notifications "pushNotifications")
                          (:streaming "streaming")))
            t))
      ((listp caps) (and (getf caps key) t))
      (t nil))))

(defun %encode-list-result (plist include-artifacts history-length)
  (json-object
   "tasks" (map 'vector
                (lambda (task)
                  (encode-task task
                               :history-length history-length
                               :include-artifacts include-artifacts))
                (getf plist :tasks))
   "nextPageToken" (or (getf plist :next-page-token) "")
   "pageSize" (getf plist :page-size)
   "totalSize" (getf plist :total-size)))

(defun dispatch-a2a-method (agent method params &key protocol-version)
  "HANDLER for rpc-serve. METHOD is a string. Returns a JSON-able result
   or an a2a-stream-result for streaming methods."
  (let ((params (%ensure-params params))
        (op (%canonical-method method)))
    (%check-version params protocol-version)
    (unless op
      (error 'rpc-protocol:rpc-error
             :code rpc-protocol:+method-not-found+
             :message (format nil "unknown A2A method ~s" method)))
    (ecase op
      (:send-message
       (let* ((raw (or (param params "message")
                       (signal-a2a-error :message "missing message"
                                         :code rpc-protocol:+invalid-params+)))
              (message (decode-message raw))
              (blocking (not (%return-immediately params :default)))
              (result (send-message agent message :blocking blocking)))
         (encode-send-result result)))
      (:stream-message
       (unless (a2a-agent-streaming-p agent)
         (signal-a2a-error :message "streaming is not supported"
                           :code +a2a-error-unsupported-operation+))
       (let* ((raw (or (param params "message")
                       (signal-a2a-error :message "missing message"
                                         :code rpc-protocol:+invalid-params+)))
              (message (decode-message raw)))
         (stream-message agent message)))
      (:get-task
       (encode-task (%find-task agent (%require-task-id params))
                    :history-length (%history-length params)))
      (:list-tasks
       (let ((status (let ((s (param params "status")))
                       (when s (task-state-from-wire s)))))
         (%encode-list-result
          (list-tasks agent
                      :context-id (param params "contextId")
                      :status status
                      :page-size (param params "pageSize")
                      :page-token (param params "pageToken")
                      :history-length (%history-length params)
                      :include-artifacts (param params "includeArtifacts")
                      :status-timestamp-after (param params "statusTimestampAfter"))
          (and (param params "includeArtifacts") t)
          (%history-length params))))
      (:cancel-task
       (encode-task (cancel-task agent (%require-task-id params))))
      (:subscribe
       (resubscribe-task agent (%require-task-id params)))
      (:push
       (signal-a2a-error
              :message "push notifications are not supported"
              :code +a2a-error-push-not-supported+))
      (:extended-card
       (unless (%capability-p (a2a-agent-card agent) :extended-agent-card)
         (signal-a2a-error
                :message "extended agent card is not supported"
                :code +a2a-error-unsupported-operation+))
       (let ((card (a2a-agent-extended-card agent)))
         (unless card
           (signal-a2a-error
                  :message "extended agent card is not configured"
                  :code +a2a-error-extended-card-not-configured+))
         (encode-agent-card card))))))

(defun serve-a2a (agent &key (transport rpc-protocol:*rpc-transport*))
  (rpc-protocol:rpc-serve
   (lambda (method params)
     (handler-case
         (dispatch-a2a-method agent method params)
       (a2a-error (c)
         (error 'rpc-protocol:rpc-error
                :message (or (a2a-error-message c) "a2a error")
                :code (or (a2a-error-code c) rpc-protocol:+internal-error+)
                :data (a2a-error-data c)))))
   :transport transport))

(defmethod fetch-agent-card ((backend a2a-backend) url &key)
  (declare (ignore url))
  (signal-a2a-error :message "fetch-agent-card not implemented"))

(defmethod serve-agent-card ((backend a2a-backend) card &key)
  (declare (ignore card))
  (signal-a2a-error :message "serve-agent-card not implemented"))

(defmethod send-message ((backend a2a-backend) message &key task-id blocking)
  (declare (ignore message task-id blocking))
  (signal-a2a-error :message "send-message not implemented"))

(defmethod stream-message ((backend a2a-backend) message &key on-event)
  (declare (ignore message on-event))
  (signal-a2a-error :message "stream-message not implemented"))

(defmethod get-task ((backend a2a-backend) task-id &key history-length)
  (declare (ignore task-id history-length))
  (signal-a2a-error :message "get-task not implemented"))

(defmethod list-tasks ((backend a2a-backend) &key context-id status page-size
                                               page-token history-length
                                               include-artifacts
                                               status-timestamp-after)
  (declare (ignore context-id status page-size page-token history-length
                   include-artifacts status-timestamp-after))
  (signal-a2a-error :message "list-tasks not implemented"))

(defmethod cancel-task ((backend a2a-backend) task-id &key)
  (declare (ignore task-id))
  (signal-a2a-error :message "cancel-task not implemented"))

(defmethod resubscribe-task ((backend a2a-backend) task-id &key on-event)
  (declare (ignore task-id on-event))
  (signal-a2a-error :message "resubscribe-task not implemented"))
