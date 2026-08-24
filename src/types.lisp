(in-package #:a2a-protocol)

;;; Strings are not EQL across reloads — SBCL DEFCONSTANT-UNEQL.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '+a2a-protocol-version+)
    (defconstant +a2a-protocol-version+ "1.0"
      "Preferred A2A protocol version.")))

(defparameter *supported-protocol-versions*
  (list +a2a-protocol-version+ "0.3")
  "Newest-first versions this implementation speaks. Empty A2A-Version ⇒ 0.3 per spec.")

(defconstant +a2a-error-task-not-found+ -32001)
(defconstant +a2a-error-task-not-cancelable+ -32002)
(defconstant +a2a-error-push-not-supported+ -32003)
(defconstant +a2a-error-unsupported-operation+ -32004)
(defconstant +a2a-error-content-type-not-supported+ -32005)
(defconstant +a2a-error-invalid-agent-response+ -32006)
(defconstant +a2a-error-extended-card-not-configured+ -32007)
(defconstant +a2a-error-extension-support-required+ -32008)
(defconstant +a2a-error-version-not-supported+ -32009)

(defvar *a2a-backend* nil)

(defclass a2a-backend ()
  ()
  (:documentation "Transport adapter. Concrete backends live in a2a-backend-*."))

(defun make-id (&optional prefix)
  (format nil "~@[~a-~]~8,'0x~4,'0x~4,'0x"
          prefix
          (random #x100000000)
          (random #x10000)
          (random #x10000)))

;;; --- card -----------------------------------------------------------------

(defclass agent-interface ()
  ((url :initarg :url :accessor agent-interface-url)
   (protocol-binding :initarg :protocol-binding
                     :accessor agent-interface-protocol-binding
                     :initform "JSONRPC")
   (protocol-version :initarg :protocol-version
                     :accessor agent-interface-protocol-version
                     :initform +a2a-protocol-version+)
   (tenant :initarg :tenant :accessor agent-interface-tenant :initform nil)))

(defun make-agent-interface (url &key (protocol-binding "JSONRPC")
                                   (protocol-version +a2a-protocol-version+)
                                   tenant)
  (make-instance 'agent-interface
                 :url url
                 :protocol-binding protocol-binding
                 :protocol-version protocol-version
                 :tenant tenant))

(defclass agent-skill ()
  ((id :initarg :id :accessor agent-skill-id)
   (name :initarg :name :accessor agent-skill-name)
   (description :initarg :description :accessor agent-skill-description :initform "")
   (tags :initarg :tags :accessor agent-skill-tags :initform '("echo"))
   (examples :initarg :examples :accessor agent-skill-examples :initform nil)
   (input-modes :initarg :input-modes :accessor agent-skill-input-modes :initform nil)
   (output-modes :initarg :output-modes :accessor agent-skill-output-modes :initform nil)))

(defun make-agent-skill (id &key name description tags examples input-modes output-modes)
  (make-instance 'agent-skill
                 :id id
                 :name (or name id)
                 :description (or description "")
                 :tags (or tags '("echo"))
                 :examples examples
                 :input-modes input-modes
                 :output-modes output-modes))

(defclass agent-card ()
  ((name :initarg :name :accessor agent-card-name :initform "cl-stack-a2a")
   (description :initarg :description :accessor agent-card-description
                :initform "A2A agent")
   (version :initarg :version :accessor agent-card-version :initform "0.1.0")
   (supported-interfaces :initarg :supported-interfaces
                         :accessor agent-card-supported-interfaces
                         :initform nil)
   (capabilities :initarg :capabilities :accessor agent-card-capabilities
                 :initform '(:streaming t :push-notifications nil))
   (default-input-modes :initarg :default-input-modes
                        :accessor agent-card-default-input-modes
                        :initform '("text/plain"))
   (default-output-modes :initarg :default-output-modes
                         :accessor agent-card-default-output-modes
                         :initform '("text/plain"))
   (skills :initarg :skills :accessor agent-card-skills :initform nil)
   (documentation-url :initarg :documentation-url
                      :accessor agent-card-documentation-url :initform nil)
   (icon-url :initarg :icon-url :accessor agent-card-icon-url :initform nil)
   (provider :initarg :provider :accessor agent-card-provider :initform nil)
   (url :initarg :url :accessor agent-card-url :initform nil)))

(defun make-agent-card (&key name description version supported-interfaces
                          capabilities default-input-modes default-output-modes
                          skills documentation-url icon-url provider url)
  (make-instance 'agent-card
                 :name (or name "cl-stack-a2a")
                 :description (or description "A2A agent")
                 :version (or version "0.1.0")
                 :supported-interfaces supported-interfaces
                 :capabilities (or capabilities '(:streaming t :push-notifications nil))
                 :default-input-modes (or default-input-modes '("text/plain"))
                 :default-output-modes (or default-output-modes '("text/plain"))
                 :skills skills
                 :documentation-url documentation-url
                 :icon-url icon-url
                 :provider provider
                 :url url))

;;; --- message / part / artifact / task -------------------------------------

(defclass a2a-part ()
  ((text :initarg :text :accessor a2a-part-text :initform nil)
   (raw :initarg :raw :accessor a2a-part-raw :initform nil)
   (url :initarg :url :accessor a2a-part-url :initform nil)
   (data :initarg :data :accessor a2a-part-data :initform nil)
   (filename :initarg :filename :accessor a2a-part-filename :initform nil)
   (media-type :initarg :media-type :accessor a2a-part-media-type :initform nil)
   (metadata :initarg :metadata :accessor a2a-part-metadata :initform nil)))

(defun make-text-part (text &key media-type metadata)
  (make-instance 'a2a-part :text (if (stringp text) text (princ-to-string text))
                           :media-type media-type :metadata metadata))

(defun make-data-part (data &key media-type metadata)
  (make-instance 'a2a-part :data data :media-type media-type :metadata metadata))

(defun make-file-part (&key raw url filename media-type metadata)
  (make-instance 'a2a-part :raw raw :url url :filename filename
                           :media-type media-type :metadata metadata))

(defclass a2a-message ()
  ((message-id :initarg :message-id :accessor a2a-message-id :initform nil)
   (context-id :initarg :context-id :accessor a2a-message-context-id :initform nil)
   (task-id :initarg :task-id :accessor a2a-message-task-id :initform nil)
   (role :initarg :role :accessor a2a-message-role :initform :user)
   (parts :initarg :parts :accessor a2a-message-parts :initform nil)
   (metadata :initarg :metadata :accessor a2a-message-metadata :initform nil)
   (extensions :initarg :extensions :accessor a2a-message-extensions :initform nil)
   (reference-task-ids :initarg :reference-task-ids
                       :accessor a2a-message-reference-task-ids :initform nil)))

(defun make-a2a-message (&key message-id context-id task-id (role :user) parts
                           metadata extensions reference-task-ids text)
  (make-instance 'a2a-message
                 :message-id (or message-id (make-id "msg"))
                 :context-id context-id
                 :task-id task-id
                 :role role
                 :parts (or parts (when text (list (make-text-part text))))
                 :metadata metadata
                 :extensions extensions
                 :reference-task-ids reference-task-ids))

(defun message-text (message)
  (loop for part in (a2a-message-parts message)
        for text = (a2a-part-text part)
        when text return text))

(defclass a2a-artifact ()
  ((artifact-id :initarg :artifact-id :accessor a2a-artifact-id :initform nil)
   (name :initarg :name :accessor a2a-artifact-name :initform nil)
   (description :initarg :description :accessor a2a-artifact-description :initform nil)
   (parts :initarg :parts :accessor a2a-artifact-parts :initform nil)
   (metadata :initarg :metadata :accessor a2a-artifact-metadata :initform nil)
   (extensions :initarg :extensions :accessor a2a-artifact-extensions :initform nil)))

(defun make-a2a-artifact (&key artifact-id name description parts metadata extensions)
  (make-instance 'a2a-artifact
                 :artifact-id (or artifact-id (make-id "art"))
                 :name name
                 :description description
                 :parts parts
                 :metadata metadata
                 :extensions extensions))

(defclass task-status ()
  ((state :initarg :state :accessor task-status-state :initform :submitted)
   (message :initarg :message :accessor task-status-message :initform nil)
   (timestamp :initarg :timestamp :accessor task-status-timestamp :initform nil)))

(defun make-task-status (state &key message timestamp)
  (make-instance 'task-status :state state :message message :timestamp timestamp))

(defclass a2a-task ()
  ((id :initarg :id :accessor a2a-task-id)
   (context-id :initarg :context-id :accessor a2a-task-context-id :initform nil)
   (status :initarg :status :accessor a2a-task-status
           :initform (make-task-status :submitted))
   (artifacts :initarg :artifacts :accessor a2a-task-artifacts :initform nil)
   (history :initarg :history :accessor a2a-task-history :initform nil)
   (metadata :initarg :metadata :accessor a2a-task-metadata :initform nil)))

(defun make-a2a-task (&key id context-id status artifacts history metadata state)
  (make-instance 'a2a-task
                 :id (or id (make-id "task"))
                 :context-id (or context-id (make-id "ctx"))
                 :status (or status (make-task-status (or state :submitted)))
                 :artifacts artifacts
                 :history history
                 :metadata metadata))

(defun a2a-task-state (task)
  (task-status-state (a2a-task-status task)))

(defun (setf a2a-task-state) (state task)
  (setf (task-status-state (a2a-task-status task)) state))

(defun terminal-state-p (state)
  (member state '(:completed :failed :canceled :rejected)))

(defclass a2a-stream-result ()
  ((events :initarg :events :accessor a2a-stream-events :initform nil)))

(defun make-a2a-stream-result (events)
  (make-instance 'a2a-stream-result :events events))

;;; --- agent ----------------------------------------------------------------

(defclass a2a-agent (a2a-backend)
  ((name :initarg :name :accessor a2a-agent-name :initform "cl-stack-a2a")
   (card :initarg :card :accessor a2a-agent-card :initform nil)
   (extended-card :initarg :extended-card :accessor a2a-agent-extended-card
                  :initform nil)
   (tasks :initarg :tasks :accessor a2a-agent-tasks
          :initform (make-hash-table :test 'equal))
   (handler :initarg :handler :accessor a2a-agent-handler :initform nil)
   (streaming-p :initarg :streaming-p :accessor a2a-agent-streaming-p
                :initform t)))

(defun make-a2a-agent (&key name card extended-card handler (streaming-p t) url)
  (let* ((name (or name "cl-stack-a2a"))
         (card (or card
                   (make-agent-card
                    :name name
                    :description "Echo A2A agent"
                    :url url
                    :supported-interfaces
                    (when url (list (make-agent-interface url)))
                    :skills (list (make-agent-skill "echo"
                                                    :name "Echo"
                                                    :description "Echoes the first text part"
                                                    :tags '("echo")))))))
    (make-instance 'a2a-agent
                   :name name
                   :card card
                   :extended-card extended-card
                   :handler handler
                   :streaming-p streaming-p)))
