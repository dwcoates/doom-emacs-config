;;; workspace-create-client.el --- Thin client for daemon workspace creation -*- lexical-binding: t; -*-

;;; Commentary:

;; The daemon owns workspace creation end to end: git worktree/branch/tag,
;; session creation, healthy waiting shim, and initial-prompt queueing.  Emacs
;; sends creation intent, then materializes only local perspective/bookkeeping
;; state after the daemon pushes WorkspaceAvailable.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)

(defun agent-repl--workspace-create-log-payload-shape (payload)
  "Return a value-free structural summary of wire PAYLOAD for diagnostics."
  (cond
   ((null payload) "nil")
   ((not (listp payload)) (format "type=%s" (type-of payload)))
   (t
    (let ((cursor payload)
          fields)
      (while (consp cursor)
        (let ((key (car cursor)))
          (push (if (symbolp key)
                    key
                  (format "<%s>" (type-of key)))
                fields))
        (setq cursor (cdr cursor))
        (if (consp cursor)
            (setq cursor (cdr cursor))
          (when cursor
            (push (format "<improper-tail:%s>" (type-of cursor)) fields))
          (setq cursor nil)))
      (format "fields=%S" (nreverse fields))))))

(defun agent-repl--workspace-create-keyword-plist-p (value)
  "Return non-nil when VALUE is an even-length keyword plist."
  (and (listp value)
       (keywordp (car value))
       (condition-case nil
           (zerop (% (length value) 2))
         (error nil))))

(defun agent-repl--workspace-create-required-string (payload key context &optional ws)
  "Return PAYLOAD's non-blank string KEY, or fail loudly for CONTEXT."
  (let ((value
         (condition-case err
             (plist-get payload key)
           (error
            (agent-repl--log
             ws
             "%s: INVALID payload-shape=%s field=%s err-type=%s — aborting before mutation"
             context (agent-repl--workspace-create-log-payload-shape payload)
             key (car err))
            (signal (car err) (cdr err))))))
    (unless (and (stringp value)
                 (not (string-empty-p (string-trim value))))
      (agent-repl--log
       ws
       "%s: INVALID required field=%s value-type=%s value-length=%s %s — aborting before mutation"
       context key (type-of value)
       (and (stringp value) (length value))
       (agent-repl--workspace-create-log-payload-shape payload))
      (error "agent-repl: %s requires non-empty %s" context key))
    value))

(defun agent-repl--workspace-create-send
    (requested-name git-root base-commit source-workspace source-dir
                    initial-prompt &optional priority model)
  "Send daemon-owned workspace creation intent and return its request id.
All inputs are read-only intent.  This function never runs git, creates a
perspective or session, starts a shim, or locally delivers INITIAL-PROMPT."
  (dolist (pair `((requested-name . ,requested-name)
                  (git-root . ,git-root)
                  (base-commit . ,base-commit)
                  (source-workspace . ,source-workspace)
                  (source-dir . ,source-dir)))
    (unless (and (stringp (cdr pair))
                 (not (string-empty-p (string-trim (cdr pair)))))
      (agent-repl--log
       source-workspace
       "workspace-create-send: INVALID %s=%S name=%S root=%S base=%S source=%S dir=%S — aborting"
       (car pair) (cdr pair) requested-name git-root base-commit
       source-workspace source-dir)
      (user-error "agent-repl: workspace creation requires %s" (car pair))))
  (let* ((prompt (and (stringp initial-prompt)
                      (string-trim initial-prompt)))
         ;; The daemon creates the child session, so the source workspace's
         ;; account and the GUI session permission posture must cross the same
         ;; request boundary as the git intent.  Empty configDir deliberately
         ;; means the CLI's own account root.
         (posture (agent-repl--frontend-session-posture source-dir))
         (config-dir (plist-get posture :config-dir))
         (permission-mode (plist-get posture :permission-mode))
         (allow-ungated (plist-get posture :allow-ungated))
         (payload
          (append
           (list :requestedName (string-trim requested-name)
                 :gitRoot (file-name-as-directory (expand-file-name git-root))
                 :baseCommit base-commit
                 :sourceWorkspace source-workspace
                 :sourceDir (file-name-as-directory (expand-file-name source-dir))
                 :configDir (or config-dir "")
                 :permissionMode (or permission-mode ""))
           (when (and prompt (not (string-empty-p prompt)))
             (list :initialPrompt prompt))
           (when priority (list :priority priority))
           (when model (list :model model))
           ;; Consent is call-site state, not a preference; never synthesize it
           ;; merely because a permission-mode string happens to be ungated.
           (when allow-ungated
             (list :allowUngated t))))
         (request-id nil))
    (agent-repl--log
     source-workspace
     "workspace-create-send: READY requested-name=%s git-root=%s base=%s source-dir=%s prompt-present=%S prompt-length=%s priority-present=%S model-present=%S config-dir-present=%S permission-mode-present=%S allow-ungated=%S"
     requested-name git-root base-commit source-dir
     (and prompt (not (string-empty-p prompt)))
     (and prompt (length prompt))
     (not (null priority)) (not (null model))
     (not (null config-dir)) (not (null permission-mode)) allow-ungated)
    (condition-case err
        (progn
          (setq request-id
                (agent-repl--uds-send-command
                 "createWorkspace" payload source-workspace))
          (agent-repl--uds-track-command
           request-id "createWorkspace" source-workspace)
          (agent-repl--log
           source-workspace
           "workspace-create-send: SENT request-id=%s requested-name=%s git-root=%s base=%s source-dir=%s prompt-present=%S prompt-length=%s priority-present=%S model-present=%S config-dir-present=%S permission-mode-present=%S allow-ungated=%S"
           request-id requested-name git-root base-commit source-dir
           (and prompt (not (string-empty-p prompt)))
           (and prompt (length prompt))
           (not (null priority)) (not (null model))
           (not (null config-dir)) (not (null permission-mode)) allow-ungated)
          request-id)
      (error
       (agent-repl--log
        source-workspace
        "workspace-create-send: FAILED request-id=%s requested-name=%s git-root=%s base=%s source-dir=%s prompt-present=%S prompt-length=%s err-type=%s — aborting"
        request-id requested-name git-root base-commit source-dir
        (and prompt (not (string-empty-p prompt)))
        (and prompt (length prompt)) (car err))
       (signal (car err) (cdr err))))))

(defun agent-repl--workspace-create-available-metadata (available)
  "Validate AVAILABLE and return `(WS . METADATA)' without mutating state."
  (let* ((context "WorkspaceAvailable")
         (candidate-ws (plist-get available :finalName))
         (job-id (agent-repl--workspace-create-required-string
                  available :jobId context candidate-ws))
         (ws (agent-repl--workspace-create-required-string
              available :finalName context candidate-ws))
         (raw-path (agent-repl--workspace-create-required-string
                    available :worktreePath context ws))
         (session-id (agent-repl--workspace-create-required-string
                      available :sessionId context ws)))
    (unless (file-name-absolute-p raw-path)
      (agent-repl--log
       ws
       "workspace-available: INVALID non-absolute path=%s job-id=%s session-id=%s — aborting before mutation"
       raw-path job-id session-id)
      (error "agent-repl: WorkspaceAvailable path is not absolute: %s" raw-path))
    (unless (file-directory-p raw-path)
      (agent-repl--log
       ws
       "workspace-available: INVALID missing directory path=%s job-id=%s session-id=%s — aborting before mutation"
       raw-path job-id session-id)
      (error "agent-repl: WorkspaceAvailable directory does not exist: %s"
             raw-path))
    (let ((path (file-name-as-directory (expand-file-name raw-path))))
      (agent-repl--log
       ws
       "workspace-available: METADATA READY job-id=%s path=%s session-id=%s branch-present=%S source-ws-present=%S source-dir-present=%S config-dir-present=%S permission-mode-present=%S allow-ungated=%S prompt-queued=%S"
       job-id path session-id
       (not (null (plist-get available :branch)))
       (not (null (plist-get available :sourceWorkspace)))
       (not (null (plist-get available :sourceDir)))
       (not (null (plist-get available :configDir)))
       (not (null (plist-get available :permissionMode)))
       (and (plist-get available :allowUngated) t)
       (and (plist-get available :initialPromptQueued) t))
      (cons
       ws
       (list :daemon-workspace-job-id job-id
             :project-dir path
             :frontend-session-id session-id
             :branch-name (plist-get available :branch)
             :git-root (plist-get available :gitRoot)
             :base-commit (plist-get available :baseCommit)
             :source-ws-name (plist-get available :sourceWorkspace)
             :source-ws-dir (plist-get available :sourceDir)
             :fork-from (plist-get available :forkFrom)
             :fork-session-id (plist-get available :forkSessionId)
             :priority (plist-get available :priority)
             :model (plist-get available :model)
             :config-dir-override
             (let ((config-dir (plist-get available :configDir)))
               (if (and (stringp config-dir)
                        (not (string-empty-p config-dir)))
                   config-dir
                 :default))
             :permission-mode (plist-get available :permissionMode)
             :allow-ungated (and (plist-get available :allowUngated) t)
             :initial-prompt-queued
             (and (plist-get available :initialPromptQueued) t))))))

(defun agent-repl--workspace-create-ack-materialized (ws job-id result)
  "ACK daemon JOB-ID after WS materialization produced RESULT."
  (let (request-id)
    (condition-case err
        (progn
          (setq request-id
                (agent-repl--uds-send-command
                 "workspaceMaterialized" (list :jobId job-id) ws))
          (agent-repl--uds-track-command
           request-id "workspaceMaterialized" ws)
          (agent-repl--log
           ws
           "workspace-available: ACK SENT request-id=%s job-id=%s materialization=%s"
           request-id job-id result)
          request-id)
      (error
       (agent-repl--log
        ws
        "workspace-available: ACK FAILED request-id=%s job-id=%s materialization=%s err-type=%s"
        request-id job-id result (car err))
       (signal (car err) (cdr err))))))

(defun agent-repl--workspace-create-handle-available (available)
  "Handle daemon `workspaceAvailable' payload AVAILABLE.
Validation completes before local mutation.  On success, creates only the
perspective/bookkeeping state through `workspace.el', then sends the
`workspaceMaterialized' ACK.  Exact replays ACK again without duplicating
local state."
  (let* ((validated
          (agent-repl--workspace-create-available-metadata available))
         (ws (car validated))
         (metadata (cdr validated))
         (job-id (plist-get metadata :daemon-workspace-job-id)))
    (agent-repl--log
     ws
     "workspace-available: VALIDATED ws=%s job-id=%s path=%s session-id=%s branch=%s prompt-queued=%S"
     ws job-id (plist-get metadata :project-dir)
     (plist-get metadata :frontend-session-id)
     (or (plist-get metadata :branch-name) "nil")
     (plist-get metadata :initial-prompt-queued))
    (agent-repl--log
     ws
     "workspace-available: MATERIALIZE BEGIN job-id=%s path=%s session-id=%s"
     job-id (plist-get metadata :project-dir)
     (plist-get metadata :frontend-session-id))
    (let ((result
           (condition-case err
               (agent-repl--ws-materialize-daemon-workspace ws metadata)
             (error
              (agent-repl--log
               ws
               "workspace-available: MATERIALIZE FAILED job-id=%s err-type=%s — ACK not sent"
               job-id (car err))
              (signal (car err) (cdr err))))))
      (agent-repl--log
       ws "workspace-available: MATERIALIZE COMPLETE job-id=%s result=%s"
       job-id result)
      (agent-repl--workspace-create-ack-materialized ws job-id result)
      result)))

(defun agent-repl--handle-workspace-create-failed-command (cmd)
  "Announce a daemon workspace-creation failure carried by CMD.
CMD is the decoded `workspaceCreateFailed' HostAction payload.  The job
already failed durably in the daemon; Emacs' whole job here is to make
sure the user actually finds out, so this both logs and echoes.  It
never signals: a failure notice that fails to display would be NACKed
and redelivered forever, replacing a visible failure with a loop."
  (let ((job-id (or (alist-get 'job_id cmd) "unknown"))
        (name (or (alist-get 'requested_name cmd) "unknown"))
        (text (or (alist-get 'error cmd) "no error text supplied")))
    (agent-repl--log
     name
     "workspace-create: JOB FAILED job-id=%s requested-name=%s error=%s"
     job-id name text)
    (message "agent-repl: workspace creation FAILED for '%s' (job %s): %s"
             name job-id text)
    t))

(defconst agent-repl--host-action-arms
  '((:switchWorkspace agent-repl--handle-switch-command
     ((dir . :dir)))
    (:workspaceCreateFailed agent-repl--handle-workspace-create-failed-command
     ((job_id . :jobId) (requested_name . :requestedName)
      (error . :error)))
    (:setRepositoryFold agent-repl--handle-fold-command
     ((repo_key . :repoKey) (folded . :folded)))
    (:setSidebarView agent-repl--handle-set-view-command
     ((view . :view)))
    (:taskCreate agent-repl--handle-task-create-command nil)
    (:taskToggleDone agent-repl--handle-task-toggle-done-command
     ((id . :id)))
    (:taskOpen agent-repl--handle-task-open-command
     ((id . :id)))
    (:taskAddWorkspace agent-repl--handle-task-add-workspace-command
     ((id . :id))))
  "Inbound HostAction arms mapped to their Emacs UI-only handlers.")

(defconst agent-repl--legacy-host-action-handlers
  '(("prompt" . agent-repl--handle-prompt-command)
    ("finish" . agent-repl--handle-finish-command)
    ("close" . agent-repl--handle-close-command)
    ("open" . agent-repl--handle-open-command)
    ("clipboard" . agent-repl--handle-clipboard-command)
    ("send" . agent-repl--handle-send-command)
    ("merge" . agent-repl--handle-merge-command)
    ("eval" . agent-repl--handle-eval-command))
  "Daemon-owned legacy command types mapped to their Emacs host handlers.
Only the daemon reads and claims `workspace_commands_*.json'; Emacs receives
the UI effect as a durable HostAction and executes it through this table.")

(defconst agent-repl--host-action-success-cache-limit 4096
  "Maximum successful HostAction outcomes retained for duplicate suppression.")

(defvar agent-repl--host-action-outcomes (make-hash-table :test 'equal)
  "Process-local HostAction dedupe state keyed by durable action id.
`defvar' deliberately preserves entries across module hot reload.  A full
Emacs crash loses this cache; a durable action whose completion had not
reached the daemon can then execute again after restart.  Closing that crash
window requires daemon-side transactional side-effect ownership, because
persisting an Emacs success before its non-idempotent UI effect finishes would
create the opposite lost-action failure.")

(defvar agent-repl--host-action-success-order nil
  "Most-recent-first action ids retained in the successful outcome cache.")

(agent-repl--log
 nil
 "host-action dedupe: process-local cache ready entries=%d hot-reload-preserved=yes crash-window=action-may-replay-before-daemon-completion"
 (hash-table-count agent-repl--host-action-outcomes))

(defun agent-repl--workspace-create-wire-object-to-alist (object context &optional ws)
  "Convert protojson Struct OBJECT to a recursively converted alist.
CONTEXT labels loud validation failures.  Protojson decoding produces keyword
plists, while the established host handlers consume symbol-keyed alists."
  (unless (or (null object)
              (agent-repl--workspace-create-keyword-plist-p object))
    (agent-repl--log
     ws "host-action: INVALID %s struct-shape=%s — expected keyword plist"
     context (agent-repl--workspace-create-log-payload-shape object))
    (error "agent-repl: %s must be a structured payload" context))
  (cl-labels
      ((convert (value)
         (cond
          ((and (consp value) (keywordp (car value)))
           (unless (agent-repl--workspace-create-keyword-plist-p value)
             (agent-repl--log
              ws
              "host-action: INVALID %s nested-struct-shape=%s — odd field count"
              context (agent-repl--workspace-create-log-payload-shape value))
             (error "agent-repl: %s contains a malformed object: %S"
                    context value))
           (cl-loop for (key item) on value by #'cddr
                    do (unless (keywordp key)
                         (agent-repl--log
                          ws
                          "host-action: INVALID %s nested-key-type=%s — expected keyword"
                          context (type-of key))
                         (error "agent-repl: %s contains a non-keyword key"
                                context))
                    collect
                    (cons (intern (substring (symbol-name key) 1))
                          (convert item))))
          ((consp value) (mapcar #'convert value))
          (t value))))
    (convert object)))

(defun agent-repl--workspace-create-legacy-host-command (legacy &optional ws)
  "Validate LEGACY and return `(TYPE HANDLER CMD WS)'.
LEGACY is the `legacyCommand' HostAction payload.  Its structured payload is
translated to the alist contract the existing host handlers consume."
  (unless (and (listp legacy) (keywordp (car legacy)))
    (agent-repl--log
     ws "host-action: INVALID legacyCommand-shape=%s — expected object"
     (agent-repl--workspace-create-log-payload-shape legacy))
    (error "agent-repl: HostAction legacyCommand must be an object"))
  (let* ((type (agent-repl--workspace-create-required-string
                legacy :type "HostAction legacyCommand" ws))
         (entry (assoc type agent-repl--legacy-host-action-handlers))
         (_required-payload
          (unless (plist-member legacy :payload)
            (agent-repl--log
             ws "host-action: INVALID legacy type=%s — payload missing"
             type)
            (error "agent-repl: HostAction legacyCommand %s requires payload"
                   type)))
         (payload
          (agent-repl--workspace-create-wire-object-to-alist
           (plist-get legacy :payload)
           (format "HostAction legacyCommand %s payload" type) ws)))
    (unless entry
      (agent-repl--log
       ws
       "host-action: INVALID legacy type=%s known=%S payload-shape=%s — refusing"
       type (mapcar #'car agent-repl--legacy-host-action-handlers)
       (agent-repl--workspace-create-log-payload-shape payload))
      (error "agent-repl: unsupported HostAction legacyCommand type %s" type))
    (list type (cdr entry) payload (alist-get 'workspace payload))))

(defun agent-repl--workspace-create-host-action-command (action &optional ws)
  "Validate ACTION and return `(ACTION-ID TYPE HANDLER CMD WS)'.
Exactly one UI action arm must be present."
  (let* ((action-id (agent-repl--workspace-create-required-string
                     action :actionId "HostAction" ws))
         (present
          (cl-remove-if-not
           (lambda (entry) (plist-member action (car entry)))
           (append agent-repl--host-action-arms
                   '((:legacyCommand nil nil))))))
    (unless (= (length present) 1)
      (agent-repl--log
       ws
       "host-action: INVALID action-id=%s present-arms=%S action-shape=%s"
       action-id (mapcar #'car present)
       (agent-repl--workspace-create-log-payload-shape action))
      (error "agent-repl: HostAction %s must select exactly one UI action"
             action-id))
    (let* ((entry (car present))
           (arm (car entry)))
      (if (eq arm :legacyCommand)
          (pcase-let ((`(,type ,handler ,cmd ,ws)
                       (agent-repl--workspace-create-legacy-host-command
                        (plist-get action arm) ws)))
            (list action-id type handler cmd ws))
        (let* ((handler (cadr entry))
               (mapping (caddr entry))
               (payload (plist-get action arm))
               (cmd
                (mapcar
                 (lambda (pair)
                   (cons (car pair) (plist-get payload (cdr pair))))
                 mapping)))
          (list action-id (substring (symbol-name arm) 1)
                handler cmd nil))))))

(defun agent-repl--workspace-create-send-host-completion
    (action-id ok &optional error-text ws)
  "Send HostActionCompleted for ACTION-ID with OK and ERROR-TEXT."
  (let (request-id)
    (condition-case err
        (progn
          (setq request-id
                (agent-repl--uds-send-command
                 "hostActionCompleted"
                 (append (list :actionId action-id
                               :ok (if ok t json-false))
                         (when error-text (list :error error-text)))))
          (agent-repl--uds-track-command
           request-id "hostActionCompleted" ws)
          (agent-repl--log
           ws
           "host-action: COMPLETION SENT action-id=%s request-id=%s ok=%S error-present=%S error-length=%s"
           action-id request-id ok (not (null error-text))
           (and error-text (length error-text)))
          request-id)
      (error
       (agent-repl--log
        ws
        "host-action: COMPLETION FAILED action-id=%s request-id=%s ok=%S error-present=%S error-length=%s err-type=%s"
        action-id request-id ok (not (null error-text))
        (and error-text (length error-text)) (car err))
       (signal (car err) (cdr err))))))

(defun agent-repl--workspace-create-cache-host-success
    (action-id type handler cmd ws duplicates)
  "Cache ACTION-ID's successful outcome and prune old completed entries."
  (puthash action-id
           (list :state 'succeeded :type type :handler handler :cmd cmd :ws ws
                 :ok t :error nil :duplicates duplicates
                 :completed-at (float-time))
           agent-repl--host-action-outcomes)
  (setq agent-repl--host-action-success-order
        (cons action-id
              (delete action-id agent-repl--host-action-success-order)))
  (while (> (length agent-repl--host-action-success-order)
            agent-repl--host-action-success-cache-limit)
    (let* ((evicted (car (last agent-repl--host-action-success-order)))
           (evicted-outcome
            (gethash evicted agent-repl--host-action-outcomes)))
      (setq agent-repl--host-action-success-order
            (butlast agent-repl--host-action-success-order))
      (when (eq (plist-get evicted-outcome :state)
                'succeeded)
        (remhash evicted agent-repl--host-action-outcomes)
        (agent-repl--log
         (plist-get evicted-outcome :ws)
         "host-action dedupe: EVICT success action-id=%s cache-limit=%d crash-window=unchanged"
         evicted agent-repl--host-action-success-cache-limit)))))

(defun agent-repl--workspace-create-resend-host-outcome
    (action-id outcome reason)
  "Resend cached ACTION-ID OUTCOME without re-running its handler."
  (let ((state (plist-get outcome :state))
        (ws (plist-get outcome :ws)))
    (agent-repl--log
     ws
     "host-action dedupe: RESEND action-id=%s state=%s reason=%s ok=%S error-present=%S error-length=%s handler-not-run=yes"
     action-id state reason (plist-get outcome :ok)
     (not (null (plist-get outcome :error)))
     (and (plist-get outcome :error) (length (plist-get outcome :error))))
    (agent-repl--workspace-create-send-host-completion
     action-id (plist-get outcome :ok) (plist-get outcome :error) ws)
    (when (eq state 'failed-unsent)
      ;; A failed handler is retryable only after its failure completion has
      ;; actually been sent.  Remove it now so the daemon's next redelivery
      ;; executes the handler again rather than suppressing it forever.
      (remhash action-id agent-repl--host-action-outcomes)
      (agent-repl--log
       ws
       "host-action dedupe: FAILURE OUTCOME SENT action-id=%s retryable-on-next-delivery=yes"
       action-id))
    :duplicate))

(defun agent-repl--workspace-create-host-action-ws (action)
  "Return ACTION's directly supplied legacy workspace name, if valid.
This is diagnostic context only; HostAction validation remains authoritative."
  (let* ((legacy (and (listp action) (plist-get action :legacyCommand)))
         (payload (and (listp legacy) (plist-get legacy :payload)))
         (ws (and (listp payload) (plist-get payload :workspace))))
    (and (stringp ws) (not (string-empty-p (string-trim ws))) ws)))

(defun agent-repl--workspace-create-handle-host-action (action)
  "Execute daemon ACTION through its Emacs host handler and complete it.
Once ACTION-ID validates, parsing and handler failures are acknowledged as
`ok=false' before the original error is re-signaled.  Snapshot/live overlap
for the same action id never invokes a non-idempotent handler twice: an
in-flight duplicate is counted for completion replay, and a completed
duplicate resends its cached outcome."
  (let* ((action-ws (agent-repl--workspace-create-host-action-ws action))
         (action-id (agent-repl--workspace-create-required-string
                     action :actionId "HostAction" action-ws))
         (existing (gethash action-id agent-repl--host-action-outcomes))
         (state (plist-get existing :state)))
    (agent-repl--log
     (or (plist-get existing :ws) action-ws)
     "host-action: RECEIVED action-id=%s cached-state=%s action-shape=%s"
     action-id (or state "none")
     (agent-repl--workspace-create-log-payload-shape action))
    (cond
     ((eq state 'in-flight)
      (let ((duplicates (1+ (or (plist-get existing :duplicates) 0))))
        (puthash action-id
                 (plist-put existing :duplicates duplicates)
                 agent-repl--host-action-outcomes)
        (agent-repl--log
         (plist-get existing :ws)
         "host-action dedupe: SUPPRESS action-id=%s state=in-flight duplicates=%d handler-not-run=yes completion-resend=deferred"
         action-id duplicates)
        :duplicate-in-flight))
     ((memq state '(succeeded failed-unsent))
      (agent-repl--workspace-create-resend-host-outcome
       action-id existing "duplicate-delivery"))
     (t
      (puthash action-id
               (list :state 'in-flight :duplicates 0)
               agent-repl--host-action-outcomes)
      (let (type handler cmd ws handler-error)
        (condition-case err
            (pcase-let ((`(,_ ,parsed-type ,parsed-handler ,parsed-cmd ,parsed-ws)
                         (agent-repl--workspace-create-host-action-command
                          action action-ws)))
              (setq type parsed-type
                    handler parsed-handler
                    cmd parsed-cmd
                    ws parsed-ws)
              (puthash action-id
                       (list :state 'in-flight :duplicates 0 :type type
                             :handler handler :cmd cmd :ws ws)
                       agent-repl--host-action-outcomes)
              (agent-repl--log
               ws
               "host-action: DISPATCH action-id=%s type=%s handler=%s cmd-shape=%s"
               action-id type handler
               (agent-repl--workspace-create-log-payload-shape cmd))
              (funcall handler cmd))
          (error (setq handler-error err)))
        (if (null handler-error)
            (let* ((in-flight
                    (gethash action-id agent-repl--host-action-outcomes))
                   (duplicates (or (plist-get in-flight :duplicates) 0)))
              (agent-repl--workspace-create-cache-host-success
               action-id type handler cmd ws duplicates)
              (dotimes (_ (1+ duplicates))
                (agent-repl--workspace-create-send-host-completion
                 action-id t nil ws))
              (agent-repl--log
               ws
               "host-action: COMPLETE action-id=%s type=%s handler=%s duplicate-completions=%d"
               action-id type handler duplicates)
              t)
          (let* ((text (error-message-string handler-error))
                 (in-flight
                  (gethash action-id agent-repl--host-action-outcomes))
                 (duplicates (or (plist-get in-flight :duplicates) 0))
                 (failed
                  (list :state 'failed-unsent :type type :handler handler
                        :cmd cmd :ws ws :ok nil :error text
                        :duplicates duplicates))
                 completion-error)
            (puthash action-id failed agent-repl--host-action-outcomes)
            (condition-case completion-err
                (progn
                  (dotimes (_ (1+ duplicates))
                    (agent-repl--workspace-create-send-host-completion
                     action-id nil text ws))
                  (remhash action-id agent-repl--host-action-outcomes)
                  (agent-repl--log
                   ws
                   "host-action dedupe: FAILURE OUTCOME SENT action-id=%s duplicate-completions=%d retryable-on-next-delivery=yes"
                   action-id duplicates))
              (error (setq completion-error completion-err)))
            (when completion-error
              (agent-repl--log
               ws
               "host-action: FAILURE completion send failed action-id=%s type=%s handler=%s handler-err-type=%s handler-err-length=%d completion-err-type=%s retryable=no-until-completion-resend"
               action-id (or type "unresolved") (or handler "unresolved")
               (car handler-error) (length text) (car completion-error)))
            (agent-repl--log
             ws
             "host-action: FAILED action-id=%s type=%s handler=%s cmd-shape=%s err-type=%s err-length=%d duplicates=%d outcome-retained=%s"
             action-id (or type "unresolved") (or handler "unresolved")
             (agent-repl--workspace-create-log-payload-shape cmd)
             (car handler-error) (length text) duplicates
             (if completion-error "yes" "no"))
            (signal (car handler-error) (cdr handler-error)))))))))

(agent-repl--uds-register-handler
 "workspaceAvailable" #'agent-repl--workspace-create-handle-available)
(agent-repl--uds-register-handler
 "hostAction" #'agent-repl--workspace-create-handle-host-action)

(provide 'workspace-create-client)

;;; workspace-create-client.el ends here
