(in-package #:a2a-protocol)

(defun json-object (&rest kvs)
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on kvs by #'cddr
          unless (or (null k) (eq v :omit))
            do (setf (gethash k h) v))
    h))

(defun param (obj key &optional default)
  (cond
    ((null obj) default)
    ((hash-table-p obj) (gethash key obj default))
    ((listp obj)
     (let ((cell (assoc key obj :test #'equal)))
       (if cell (cdr cell) default)))
    (t default)))

(defun encode-json (obj)
  (let ((yason:*symbol-encoder* #'yason:encode-symbol-as-lowercase))
    (with-output-to-string (s)
      (yason:encode obj s))))

(defun decode-json (string)
  (yason:parse string :object-as :hash-table :json-arrays-as-vectors t))

(defun %as-list (seq)
  (cond
    ((null seq) nil)
    ((vectorp seq) (coerce seq 'list))
    ((listp seq) seq)
    (t (list seq))))

(defun %as-vector (seq)
  (cond
    ((null seq) :omit)
    ((vectorp seq) seq)
    (t (coerce seq 'vector))))

;;; --- enums ----------------------------------------------------------------

(defparameter *task-states*
  '((:unspecified . "TASK_STATE_UNSPECIFIED")
    (:submitted . "TASK_STATE_SUBMITTED")
    (:working . "TASK_STATE_WORKING")
    (:completed . "TASK_STATE_COMPLETED")
    (:failed . "TASK_STATE_FAILED")
    (:canceled . "TASK_STATE_CANCELED")
    (:cancelled . "TASK_STATE_CANCELED")
    (:input-required . "TASK_STATE_INPUT_REQUIRED")
    (:rejected . "TASK_STATE_REJECTED")
    (:auth-required . "TASK_STATE_AUTH_REQUIRED")))

(defparameter *roles*
  '((:unspecified . "ROLE_UNSPECIFIED")
    (:user . "ROLE_USER")
    (:agent . "ROLE_AGENT")))

(defun %lookup-wire (table key)
  (or (cdr (assoc key table))
      (error 'a2a-error :message (format nil "unknown enum ~s" key)
                        :code rpc-protocol:+invalid-params+)))

(defun task-state-to-wire (state)
  (%lookup-wire *task-states* state))

(defun task-state-from-wire (wire)
  (let ((s (if (stringp wire) wire (princ-to-string wire))))
    (or (car (rassoc s *task-states* :test #'string-equal))
        (let ((kw (intern (string-upcase (substitute #\- #\_ s)) :keyword)))
          (cond
            ((eq kw :cancelled) :canceled)
            ((assoc kw *task-states*) kw)
            (t (error 'a2a-error
                      :message (format nil "unknown task state ~s" wire)
                      :code rpc-protocol:+invalid-params+)))))))

(defun role-to-wire (role)
  (%lookup-wire *roles* role))

(defun role-from-wire (wire)
  (let ((s (if (stringp wire) wire (princ-to-string wire))))
    (or (car (rassoc s *roles* :test #'string-equal))
        (let ((kw (intern (string-upcase (substitute #\- #\_ s)) :keyword)))
          (if (assoc kw *roles*)
              kw
              (error 'a2a-error
                     :message (format nil "unknown role ~s" wire)
                     :code rpc-protocol:+invalid-params+))))))

;;; --- parts / message / artifact / task ------------------------------------

(defun encode-part (part)
  (json-object
   "text" (or (a2a-part-text part) :omit)
   "raw" (or (a2a-part-raw part) :omit)
   "url" (or (a2a-part-url part) :omit)
   "data" (if (slot-boundp part 'data)
              (or (a2a-part-data part) :omit)
              :omit)
   "filename" (or (a2a-part-filename part) :omit)
   "mediaType" (or (a2a-part-media-type part) :omit)
   "metadata" (or (a2a-part-metadata part) :omit)))

(defun decode-part (obj)
  (make-instance 'a2a-part
                 :text (or (param obj "text")
                           (when (member (or (param obj "kind") (param obj "type"))
                                         '("text") :test #'equal)
                             (param obj "text")))
                 :raw (param obj "raw")
                 :url (param obj "url")
                 :data (param obj "data")
                 :filename (param obj "filename")
                 :media-type (or (param obj "mediaType") (param obj "mimeType"))
                 :metadata (param obj "metadata")))

(defun encode-message (message)
  (json-object
   "messageId" (a2a-message-id message)
   "contextId" (or (a2a-message-context-id message) :omit)
   "taskId" (or (a2a-message-task-id message) :omit)
   "role" (role-to-wire (a2a-message-role message))
   "parts" (map 'vector #'encode-part (or (a2a-message-parts message) #()))
   "metadata" (or (a2a-message-metadata message) :omit)
   "extensions" (%as-vector (a2a-message-extensions message))
   "referenceTaskIds" (%as-vector (a2a-message-reference-task-ids message))))

(defun decode-message (obj)
  (make-a2a-message
   :message-id (or (param obj "messageId") (param obj "message_id") (make-id "msg"))
   :context-id (or (param obj "contextId") (param obj "context_id"))
   :task-id (or (param obj "taskId") (param obj "task_id"))
   :role (role-from-wire (or (param obj "role") "ROLE_USER"))
   :parts (mapcar #'decode-part (%as-list (param obj "parts")))
   :metadata (param obj "metadata")
   :extensions (%as-list (param obj "extensions"))
   :reference-task-ids (%as-list (or (param obj "referenceTaskIds")
                                     (param obj "reference_task_ids")))))

(defun encode-artifact (artifact)
  (json-object
   "artifactId" (a2a-artifact-id artifact)
   "name" (or (a2a-artifact-name artifact) :omit)
   "description" (or (a2a-artifact-description artifact) :omit)
   "parts" (map 'vector #'encode-part (or (a2a-artifact-parts artifact) #()))
   "metadata" (or (a2a-artifact-metadata artifact) :omit)
   "extensions" (%as-vector (a2a-artifact-extensions artifact))))

(defun decode-artifact (obj)
  (make-a2a-artifact
   :artifact-id (or (param obj "artifactId") (param obj "artifact_id") (make-id "art"))
   :name (param obj "name")
   :description (param obj "description")
   :parts (mapcar #'decode-part (%as-list (param obj "parts")))
   :metadata (param obj "metadata")
   :extensions (%as-list (param obj "extensions"))))

(defun encode-task-status (status)
  (json-object
   "state" (task-state-to-wire (task-status-state status))
   "message" (if (task-status-message status)
                 (encode-message (task-status-message status))
                 :omit)
   "timestamp" (or (task-status-timestamp status) :omit)))

(defun decode-task-status (obj)
  (make-task-status
   (task-state-from-wire (or (param obj "state") "TASK_STATE_SUBMITTED"))
   :message (let ((m (param obj "message")))
              (when m (decode-message m)))
   :timestamp (param obj "timestamp")))

(defun %slice-history (history history-length)
  (cond
    ((null history-length) history)
    ((zerop history-length) nil)
    ((<= (length history) history-length) history)
    (t (subseq history (- (length history) history-length)))))

(defun encode-task (task &key history-length (include-artifacts t))
  (json-object
   "id" (a2a-task-id task)
   "contextId" (or (a2a-task-context-id task) :omit)
   "status" (encode-task-status (a2a-task-status task))
   "artifacts" (if include-artifacts
                   (map 'vector #'encode-artifact (or (a2a-task-artifacts task) #()))
                   :omit)
   "history" (let ((hist (%slice-history (a2a-task-history task) history-length)))
               (if hist (map 'vector #'encode-message hist) :omit))
   "metadata" (or (a2a-task-metadata task) :omit)))

(defun decode-task (obj)
  (make-a2a-task
   :id (or (param obj "id") (make-id "task"))
   :context-id (or (param obj "contextId") (param obj "context_id"))
   :status (let ((st (param obj "status")))
             (if st
                 (decode-task-status st)
                 (make-task-status :submitted)))
   :artifacts (mapcar #'decode-artifact (%as-list (param obj "artifacts")))
   :history (mapcar #'decode-message (%as-list (param obj "history")))
   :metadata (param obj "metadata")))

(defun encode-send-result (value)
  (cond
    ((typep value 'a2a-task) (json-object "task" (encode-task value)))
    ((typep value 'a2a-message) (json-object "message" (encode-message value)))
    ((hash-table-p value) value)
    (t (error 'a2a-error
              :message "SendMessage must return a task or message"
              :code +a2a-error-invalid-agent-response+))))

(defun decode-send-result (obj)
  (cond
    ((null obj) nil)
    ((param obj "task") (decode-task (param obj "task")))
    ((param obj "message") (decode-message (param obj "message")))
    ((param obj "id") (decode-task obj))
    ((param obj "messageId") (decode-message obj))
    (t (error 'a2a-error
              :message "SendMessage result has neither task nor message"
              :code +a2a-error-invalid-agent-response+
              :data obj))))

(defun encode-stream-event (kind value &key append last-chunk metadata)
  (ecase kind
    (:task (json-object "task" (encode-task value)))
    (:message (json-object "message" (encode-message value)))
    (:status-update
     (json-object
      "statusUpdate"
      (json-object "taskId" (a2a-task-id value)
                   "contextId" (or (a2a-task-context-id value) :omit)
                   "status" (encode-task-status (a2a-task-status value))
                   "metadata" (or metadata :omit))))
    (:artifact-update
     (json-object
      "artifactUpdate"
      (json-object "taskId" (a2a-task-id value)
                   "contextId" (or (a2a-task-context-id value) :omit)
                   "artifact" (encode-artifact (first (a2a-task-artifacts value)))
                   "append" (if append t :omit)
                   "lastChunk" (if last-chunk t :omit)
                   "metadata" (or metadata :omit))))))

;;; --- agent card -----------------------------------------------------------

(defun %encode-capabilities (caps)
  (cond
    ((hash-table-p caps) caps)
    ((listp caps)
     (json-object
      "streaming" (if (getf caps :streaming) t :false)
      "pushNotifications" (if (getf caps :push-notifications) t :false)
      "extendedAgentCard" (if (getf caps :extended-agent-card) t :omit)))
    (t (json-object "streaming" t "pushNotifications" :false))))

(defun %decode-capabilities (obj)
  (if (hash-table-p obj)
      (list :streaming (and (param obj "streaming") t)
            :push-notifications (and (param obj "pushNotifications") t)
            :extended-agent-card (and (param obj "extendedAgentCard") t))
      '(:streaming t :push-notifications nil)))

(defun encode-agent-interface (iface)
  (json-object
   "url" (agent-interface-url iface)
   "protocolBinding" (agent-interface-protocol-binding iface)
   "protocolVersion" (agent-interface-protocol-version iface)
   "tenant" (or (agent-interface-tenant iface) :omit)))

(defun decode-agent-interface (obj)
  (make-agent-interface
   (param obj "url")
   :protocol-binding (or (param obj "protocolBinding") "JSONRPC")
   :protocol-version (or (param obj "protocolVersion") +a2a-protocol-version+)
   :tenant (param obj "tenant")))

(defun encode-agent-skill (skill)
  (json-object
   "id" (agent-skill-id skill)
   "name" (agent-skill-name skill)
   "description" (agent-skill-description skill)
   "tags" (map 'vector #'identity (or (agent-skill-tags skill) #()))
   "examples" (%as-vector (agent-skill-examples skill))
   "inputModes" (%as-vector (agent-skill-input-modes skill))
   "outputModes" (%as-vector (agent-skill-output-modes skill))))

(defun decode-agent-skill (obj)
  (make-agent-skill
   (or (param obj "id") "skill")
   :name (param obj "name")
   :description (param obj "description")
   :tags (%as-list (param obj "tags"))
   :examples (%as-list (param obj "examples"))
   :input-modes (%as-list (param obj "inputModes"))
   :output-modes (%as-list (param obj "outputModes"))))

(defun encode-agent-card (card)
  (json-object
   "name" (agent-card-name card)
   "description" (agent-card-description card)
   "version" (agent-card-version card)
   "supportedInterfaces"
   (map 'vector #'encode-agent-interface
        (or (agent-card-supported-interfaces card) #()))
   "capabilities" (%encode-capabilities (agent-card-capabilities card))
   "defaultInputModes" (map 'vector #'identity
                            (or (agent-card-default-input-modes card) '("text/plain")))
   "defaultOutputModes" (map 'vector #'identity
                             (or (agent-card-default-output-modes card) '("text/plain")))
   "skills" (map 'vector #'encode-agent-skill (or (agent-card-skills card) #()))
   "documentationUrl" (or (agent-card-documentation-url card) :omit)
   "iconUrl" (or (agent-card-icon-url card) :omit)
   "url" (or (agent-card-url card) :omit)
   "protocolVersion" +a2a-protocol-version+))

(defun decode-agent-card (obj)
  (make-agent-card
   :name (param obj "name")
   :description (param obj "description")
   :version (param obj "version")
   :supported-interfaces (mapcar #'decode-agent-interface
                                 (%as-list (param obj "supportedInterfaces")))
   :capabilities (%decode-capabilities (param obj "capabilities"))
   :default-input-modes (%as-list (param obj "defaultInputModes"))
   :default-output-modes (%as-list (param obj "defaultOutputModes"))
   :skills (mapcar #'decode-agent-skill (%as-list (param obj "skills")))
   :documentation-url (param obj "documentationUrl")
   :icon-url (param obj "iconUrl")
   :url (param obj "url")))
