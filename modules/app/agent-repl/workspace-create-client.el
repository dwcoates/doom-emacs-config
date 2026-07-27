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

(defun agent-repl--workspace-create-required-string (payload key context)
  "Return PAYLOAD's non-blank string KEY, or fail loudly for CONTEXT."
  (let ((value (plist-get payload key)))
    (unless (and (stringp value)
                 (not (string-empty-p (string-trim value))))
      (agent-repl--log
       nil
       "%s: INVALID required field=%s value=%S payload=%S — aborting before mutation"
       context key value payload)
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
         (payload
          (append
           (list :requestedName (string-trim requested-name)
                 :gitRoot (file-name-as-directory (expand-file-name git-root))
                 :baseCommit base-commit
                 :sourceWorkspace source-workspace
                 :sourceDir (file-name-as-directory (expand-file-name source-dir)))
           (when (and prompt (not (string-empty-p prompt)))
             (list :initialPrompt prompt))
           (when priority (list :priority priority))
           (when model (list :model model))))
         (request-id
          (agent-repl--uds-send-command
           "createWorkspace" payload source-workspace)))
    (agent-repl--uds-track-command
     request-id "createWorkspace" source-workspace)
    (agent-repl--log
     source-workspace
     "workspace-create-send: SENT request-id=%s requested-name=%s git-root=%s base=%s source=%s source-dir=%s prompt=%S priority=%s model=%s"
     request-id requested-name git-root base-commit source-workspace source-dir
     (and prompt (not (string-empty-p prompt)))
     (or priority "nil") (or model "nil"))
    request-id))

(defun agent-repl--workspace-create-available-metadata (available)
  "Validate AVAILABLE and return `(WS . METADATA)' without mutating state."
  (let* ((context "WorkspaceAvailable")
         (job-id (agent-repl--workspace-create-required-string
                  available :jobId context))
         (ws (agent-repl--workspace-create-required-string
              available :finalName context))
         (raw-path (agent-repl--workspace-create-required-string
                    available :worktreePath context))
         (session-id (agent-repl--workspace-create-required-string
                      available :sessionId context)))
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
             :initial-prompt-queued
             (and (plist-get available :initialPromptQueued) t))))))

(defun agent-repl--workspace-create-ack-materialized (ws job-id result)
  "ACK daemon JOB-ID after WS materialization produced RESULT."
  (let ((request-id
         (agent-repl--uds-send-command
          "workspaceMaterialized" (list :jobId job-id) ws)))
    (agent-repl--uds-track-command
     request-id "workspaceMaterialized" ws)
    (agent-repl--log
     ws
     "workspace-available: ACK SENT request-id=%s job-id=%s materialization=%s"
     request-id job-id result)
    request-id))

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
    (let ((result
           (agent-repl--ws-materialize-daemon-workspace ws metadata)))
      (agent-repl--workspace-create-ack-materialized ws job-id result)
      result)))

(defconst agent-repl--host-action-arms
  '((:switchWorkspace agent-repl--handle-switch-command
     ((dir . :dir)))
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

(defun agent-repl--workspace-create-host-action-command (action)
  "Validate ACTION and return `(ACTION-ID HANDLER CMD)'.
Exactly one UI action arm must be present."
  (let* ((action-id (agent-repl--workspace-create-required-string
                     action :actionId "HostAction"))
         (present
          (cl-remove-if-not
           (lambda (entry) (plist-member action (car entry)))
           agent-repl--host-action-arms)))
    (unless (= (length present) 1)
      (agent-repl--log
       nil
       "host-action: INVALID action-id=%s present-arms=%S payload=%S"
       action-id (mapcar #'car present) action)
      (error "agent-repl: HostAction %s must select exactly one UI action"
             action-id))
    (let* ((entry (car present))
           (arm (car entry))
           (handler (cadr entry))
           (mapping (caddr entry))
           (payload (plist-get action arm))
           (cmd
            (mapcar
             (lambda (pair)
               (cons (car pair) (plist-get payload (cdr pair))))
             mapping)))
      (list action-id handler cmd))))

(defun agent-repl--workspace-create-send-host-completion
    (action-id ok &optional error-text)
  "Send HostActionCompleted for ACTION-ID with OK and ERROR-TEXT."
  (let ((request-id
         (agent-repl--uds-send-command
          "hostActionCompleted"
          (append (list :actionId action-id
                        :ok (if ok t json-false))
                  (when error-text (list :error error-text))))))
    (agent-repl--uds-track-command
     request-id "hostActionCompleted" nil)
    (agent-repl--log
     nil
     "host-action: COMPLETION SENT action-id=%s request-id=%s ok=%S error=%s"
     action-id request-id ok (or error-text "nil"))
    request-id))

(defun agent-repl--workspace-create-handle-host-action (action)
  "Execute daemon ACTION through the legacy Emacs UI handler and complete it."
  (pcase-let* ((`(,action-id ,handler ,cmd)
                (agent-repl--workspace-create-host-action-command action)))
    (agent-repl--log
     nil "host-action: DISPATCH action-id=%s handler=%s cmd=%S"
     action-id handler cmd)
    (condition-case err
        (progn
          (funcall handler cmd)
          (agent-repl--workspace-create-send-host-completion action-id t)
          t)
      (error
       (let ((text (error-message-string err)))
         (condition-case completion-err
             (agent-repl--workspace-create-send-host-completion
              action-id nil text)
           (error
            (agent-repl--log
             nil
             "host-action: FAILURE completion send failed action-id=%s handler-error=%S completion-error=%S"
             action-id err completion-err)))
         (agent-repl--log
          nil "host-action: FAILED action-id=%s handler=%s err=%S"
          action-id handler err)
         (signal (car err) (cdr err)))))))

(agent-repl--uds-register-handler
 "workspaceAvailable" #'agent-repl--workspace-create-handle-available)
(agent-repl--uds-register-handler
 "hostAction" #'agent-repl--workspace-create-handle-host-action)

(provide 'workspace-create-client)

;;; workspace-create-client.el ends here
