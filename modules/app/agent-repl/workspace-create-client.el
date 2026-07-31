;;; workspace-create-client.el --- Thin client for daemon workspace creation -*- lexical-binding: t; -*-

;;; Commentary:

;; The daemon owns workspace creation end to end: git worktree/branch/tag,
;; session creation, healthy waiting shim, and initial-prompt queueing.
;;
;; There is exactly ONE ingestion point for creation intent — the daemon's
;; inbox of `workspace_commands_<uuid>.json' files under
;; `agent-repl--output-dir'.  Every flavor writes such a file: an Emacs chord,
;; the headless generation skill, an out-of-band agent.  Emacs never runs git,
;; never creates a session, and never invents a workspace of its own.
;;
;; Emacs materializes its perspective/bookkeeping SOLELY from the daemon's
;; `WorkspaceAvailable' announcement, identically for a creation it requested
;; and one it did not.  A requested creation is recognized only by the
;; command-file id Emacs chose, which the daemon carries back as the leading
;; segment of the job id — correlation buys a toast and a jump, nothing else.

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

(defconst agent-repl--workspace-command-prefix "workspace_commands_"
  "Filename prefix the daemon's inbox scanner recognizes.
Mirrors `commandPrefix' in `daemon/internal/workspace/create/inbox.go'.")

(defvar agent-repl--workspace-create-requests (make-hash-table :test 'equal)
  "Command-file ids Emacs itself emitted, mapped to their request plist.
Purely a correlation aid for post-materialization UX (toast, jump): a
creation is materialized identically whether or not it is found here, so
losing this table across a restart costs a jump, never a workspace.
`defvar' keeps in-flight requests across module hot reload.")

(defun agent-repl--workspace-create-command-id ()
  "Return a fresh, unique `workspace_commands_<uuid>' file id."
  (let ((hex (md5 (format "%s-%s-%s-%s"
                          (emacs-pid) (float-time) (random most-positive-fixnum)
                          (recent-keys)))))
    (concat agent-repl--workspace-command-prefix
            (substring hex 0 8) "-" (substring hex 8 12) "-"
            (substring hex 12 16) "-" (substring hex 16 20) "-"
            (substring hex 20 32))))

(defun agent-repl--workspace-create-command-job-file (job-id)
  "Return the command-file id JOB-ID was ingested from, or nil.
The daemon mints `<command-file-id>:<array-index>'; the leading segment
is exactly the id Emacs chose when it wrote the file."
  (when (and (stringp job-id) (string-match "\\`\\([^:]+\\):[0-9]+\\'" job-id))
    (match-string 1 job-id)))

(defun agent-repl--workspace-create-write-command (entry id ws)
  "Atomically write command ENTRY as command file ID for logging owner WS.
ENTRY is one alist-shaped `create' command; it is written as a
single-element top-level ARRAY, which is the only shape the daemon's
inbox parses.  The payload lands under a dot-prefixed temporary name
that the inbox scanner ignores and is then renamed into place, so the
daemon can never claim a half-written file.  Returns the final path."
  (let* ((dir agent-repl--output-dir)
         (final (expand-file-name (concat id ".json") dir))
         (temp (expand-file-name (concat "." id ".json.partial") dir))
         (json (json-encode (vector entry))))
    (make-directory dir t)
    (condition-case err
        (progn
          (with-temp-file temp (insert json))
          (rename-file temp final t))
      (error
       (agent-repl--log
        ws
        "workspace-create-request: WRITE FAILED id=%s temp=%s final=%s err-type=%s — no command reached the daemon"
        id temp final (car err))
       (when (file-exists-p temp) (ignore-errors (delete-file temp)))
       (signal (car err) (cdr err))))
    (agent-repl--log
     ws "workspace-create-request: WROTE id=%s file=%s bytes=%d"
     id final (length json))
    final))

(cl-defun agent-repl--workspace-create-request
    (&key name git-root base-commit source-workspace source-dir
          prompt priority model fork-from jump)
  "Emit one daemon-owned workspace creation command and return its file id.
All inputs are read-only intent.  This function never runs git, creates a
perspective or session, starts a shim, or locally delivers PROMPT — it
writes one `workspace_commands_<uuid>.json' file into the daemon's inbox
and returns the id it chose, which correlates the eventual
`WorkspaceAvailable' back to this request.

NAME and GIT-ROOT are required; BASE-COMMIT is required unless FORK-FROM
is set (the daemon resolves a fork's base from the live source session).
SOURCE-WORKSPACE and SOURCE-DIR nominate the live parent whose account and
permission mode the daemon inherits — deliberately NOT re-derived here, so
Emacs' idea of the parent's posture can never disagree with the session
the daemon actually finds.  JUMP, when non-nil, asks the announcement
handler to switch to the workspace once it arrives."
  (unless (and (stringp name) (not (string-empty-p (string-trim name))))
    (agent-repl--log
     source-workspace
     "workspace-create-request: INVALID name=%S root=%S base=%S — aborting before write"
     name git-root base-commit)
    (user-error "agent-repl: workspace creation requires a name"))
  (unless (and (stringp git-root) (not (string-empty-p (string-trim git-root))))
    (agent-repl--log
     source-workspace
     "workspace-create-request: INVALID git-root=%S name=%S — aborting before write"
     git-root name)
    (user-error "agent-repl: workspace creation requires a git root"))
  (unless (or fork-from
              (and (stringp base-commit)
                   (not (string-empty-p (string-trim base-commit)))))
    (agent-repl--log
     source-workspace
     "workspace-create-request: INVALID base-commit=%S name=%S fork-from=%S — aborting before write"
     base-commit name fork-from)
    (user-error "agent-repl: workspace creation requires a base commit"))
  (let* ((trimmed-prompt (and (stringp prompt) (string-trim prompt)))
         ;; An absent prompt must stay absent: an empty `prompt' field would
         ;; make the daemon submit a blank turn into a fresh session.
         (effective-prompt (and trimmed-prompt
                                (not (string-empty-p trimmed-prompt))
                                trimmed-prompt))
         ;; Consent is call-site state, not a preference; it does not travel
         ;; with the source session's permission mode, so it is the one posture
         ;; field the request still carries.
         (allow-ungated
          (and source-dir
               (plist-get (agent-repl--frontend-session-posture source-dir)
                          :allow-ungated)))
         (id (agent-repl--workspace-create-command-id))
         (entry
          (append
           (list (cons "type" "create")
                 (cons "name" (string-trim name))
                 (cons "git_root"
                       (directory-file-name (expand-file-name git-root))))
           (when base-commit (list (cons "base_commit" base-commit)))
           (when effective-prompt (list (cons "prompt" effective-prompt)))
           (when fork-from (list (cons "fork_from" fork-from)))
           (when source-workspace
             (list (cons "source_workspace" source-workspace)))
           (when source-dir
             (list (cons "source_dir"
                         (directory-file-name (expand-file-name source-dir)))))
           (when priority (list (cons "priority" priority)))
           (when model (list (cons "model" model)))
           (when allow-ungated (list (cons "allow_ungated" t))))))
    (agent-repl--log
     source-workspace
     "workspace-create-request: READY id=%s name=%s git-root=%s base=%s fork-from=%s source=%s source-dir=%s prompt-present=%S prompt-length=%s priority-present=%S model-present=%S allow-ungated=%S jump=%S"
     id name git-root (or base-commit "nil") (or fork-from "nil")
     (or source-workspace "nil") (or source-dir "nil")
     (not (null effective-prompt))
     (and effective-prompt (length effective-prompt))
     (not (null priority)) (not (null model)) (and allow-ungated t)
     (and jump t))
    (agent-repl--workspace-create-write-command entry id source-workspace)
    (puthash id
             (list :requested-name (string-trim name)
                   :source-workspace source-workspace
                   :jump (and jump t)
                   :requested-at (float-time))
             agent-repl--workspace-create-requests)
    (agent-repl--log
     source-workspace
     "workspace-create-request: PENDING id=%s outstanding=%d"
     id (hash-table-count agent-repl--workspace-create-requests))
    id))

(defun agent-repl--workspace-create-take-request (job-id)
  "Claim and return the pending request plist JOB-ID belongs to, or nil."
  (when-let* ((file-id (agent-repl--workspace-create-command-job-file job-id))
              (pending (gethash file-id
                                agent-repl--workspace-create-requests)))
    (remhash file-id agent-repl--workspace-create-requests)
    (append (list :command-file file-id) pending)))

(defun agent-repl--workspace-create-settle-request (ws job-id result)
  "Apply post-materialization UX for WS after JOB-ID materialized as RESULT.
A creation Emacs asked for gets its confirmation and, when the flavor
requested it, a jump to the new tab.  An unsolicited creation — the
generation skill, another agent, a daemon restart replay — is materialized
just as completely but stays silent: the user did not ask for a context
switch they did not initiate."
  (let* ((pending (agent-repl--workspace-create-take-request job-id))
         (jump (and pending (plist-get pending :jump))))
    ;; The two deferred UI drains a worktree workspace has always been born
    ;; with.  They are set OUTSIDE the daemon metadata envelope on purpose:
    ;; they are mutable local flags the drains clear, and the envelope is the
    ;; immutable creation fact a reconnect replay is compared against.
    (when (eq result 'created)
      (agent-repl--ws-put ws :pending-magit t)
      (agent-repl--ws-put ws :pending-initial-buffers t))
    (if (null pending)
        (agent-repl--log
         ws
         "workspace-available: UNSOLICITED ws=%s job-id=%s result=%s — materialized silently"
         ws job-id result)
      (agent-repl--log
       ws
       "workspace-available: CORRELATED ws=%s job-id=%s command-file=%s requested-name=%s result=%s jump=%S"
       ws job-id (plist-get pending :command-file)
       (plist-get pending :requested-name) result jump)
      (agent-repl--info
       (plist-get pending :source-workspace)
       "Workspace '%s' is ready." ws))
    (cond
     (jump (agent-repl--switch-to-workspace ws))
     ;; A workspace nobody is being moved to still gets its panels built
     ;; now, behind a focus-restoring transient switch, so its first real
     ;; activation displays a mounted layout instead of building one.  Only
     ;; a fresh materialization: a reconnect replay of an already-rendered
     ;; workspace has nothing to build.
     ((eq result 'created) (agent-repl--eager-open-panels ws)))
    pending))

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
    ;; Store the announced worktree in the SAME spelling every other
    ;; `:project-dir' producer uses: absolute, no trailing slash
    ;; (`directory-file-name' semantics).  The daemon's own identity for the
    ;; workspace is this clean path, and Emacs echoes `:project-dir' back as
    ;; the `workspace' field of UDS commands — a trailing slash here made
    ;; interrupt/prompt land on an unknown key ("no live session for
    ;; workspace .../color-semantics/").  `expand-file-name' only; NOT
    ;; `file-truename', so the string stays byte-identical to the daemon's.
    (let ((path (directory-file-name (expand-file-name raw-path))))
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
      ;; The ACK is what releases the daemon's initial prompt, so it comes
      ;; first; the toast/jump below is pure local UX and must never be able
      ;; to hold up delivery.
      (agent-repl--workspace-create-settle-request ws job-id result)
      result)))

(defun agent-repl--handle-workspace-create-failed-command (cmd)
  "Announce a daemon workspace-creation failure carried by CMD.
CMD is the decoded `workspaceCreateFailed' HostAction payload.  The job
already failed durably in the daemon; Emacs' whole job here is to make
sure the user actually finds out, so this both logs and echoes.  It
never signals: a failure notice that fails to display would be NACKed
and redelivered forever, replacing a visible failure with a loop."
  (let* ((job-id (or (alist-get 'job_id cmd) "unknown"))
         (name (or (alist-get 'requested_name cmd) "unknown"))
         (text (or (alist-get 'error cmd) "no error text supplied"))
         ;; Claim the correlation entry so a dead request cannot sit in the
         ;; pending table forever waiting for an announcement that will never
         ;; come.  The failure is announced either way — the daemon's report
         ;; IS the collision/validation preflight Emacs no longer runs.
         (pending (agent-repl--workspace-create-take-request job-id)))
    (agent-repl--log
     name
     "workspace-create: JOB FAILED job-id=%s requested-name=%s requested-by-emacs=%S command-file=%s error=%s"
     job-id name (not (null pending))
     (or (plist-get pending :command-file) "nil") text)
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

(defvar agent-repl--host-action-deferral nil
  "Bound by the host-action executor around each handler call.
A handler whose real outcome is NOT known by the time it returns sets this
to its own correlation token, through `agent-repl--host-action-defer'.

WHY IT EXISTS.  The executor's default is to read a handler that returned
without signalling as a SUCCEEDED action, which is right for a handler that
did the work itself.  The daemon-routed merge does not: it sends a
`mergeWorkspace' command and returns immediately, so the daemon was told
`ok=true' while the merge's own `CommandAck' was still in flight — and when
that ack was a rejection, the daemon had already durably recorded the merge
as done.  Deferring is what makes an action's completion mean the effect
happened rather than the dispatch happened.")

(defvar agent-repl--host-action-deferrals (make-hash-table :test 'equal)
  "Deferred host actions keyed by their handler's correlation token.
Maps token -> action id for `agent-repl--host-action-settle'.  `defvar'
preserves entries across module hot reload, exactly as
`agent-repl--host-action-outcomes' does, so a reload mid-merge does not
strand the completion.")

(defun agent-repl--host-action-defer (token)
  "Declare that the running host action's outcome is not yet known.
TOKEN is the handler's own correlation handle (the merge path uses its
`mergeWorkspace' request id), later passed to
`agent-repl--host-action-settle'.  A no-op when called outside a host-action
handler, so the same dispatch function stays callable from an interactive
command that has no action to complete."
  (when (and token (boundp 'agent-repl--host-action-deferral))
    (setq agent-repl--host-action-deferral token))
  token)

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

(defun agent-repl--host-action-succeed (action-id type handler cmd ws)
  "Cache and send ACTION-ID's SUCCESS outcome, replaying suppressed duplicates.
The terminal success path shared by a handler that finished synchronously
and by a deferred handler whose awaited outcome came back OK
\(`agent-repl--host-action-settle'), so neither can drift from the other."
  (let* ((in-flight (gethash action-id agent-repl--host-action-outcomes))
         (duplicates (or (plist-get in-flight :duplicates) 0)))
    (agent-repl--workspace-create-cache-host-success
     action-id type handler cmd ws duplicates)
    (dotimes (_ (1+ duplicates))
      (agent-repl--workspace-create-send-host-completion action-id t nil ws))
    (agent-repl--log
     ws
     "host-action: COMPLETE action-id=%s type=%s handler=%s duplicate-completions=%d"
     action-id type handler duplicates)
    t))

(defun agent-repl--host-action-hold (action-id type handler cmd ws token)
  "Retain ACTION-ID as in-flight until TOKEN's outcome settles it.
No completion is sent: the daemon keeps the action durably outstanding,
which is exactly what an unresolved effect should look like from its side.
A duplicate delivery arriving in this window hits the ordinary in-flight
suppression and is replayed when the outcome lands."
  (puthash action-id
           (list :state 'in-flight
                 :duplicates (or (plist-get
                                  (gethash action-id agent-repl--host-action-outcomes)
                                  :duplicates)
                                 0)
                 :type type :handler handler :cmd cmd :ws ws
                 :deferred-token token)
           agent-repl--host-action-outcomes)
  (puthash token action-id agent-repl--host-action-deferrals)
  (agent-repl--log
   ws
   "host-action: DEFERRED action-id=%s type=%s handler=%s token=%s completion=awaiting-outcome"
   action-id type handler token)
  :deferred)

(defun agent-repl--host-action-settle (token ok error-text)
  "Complete the host action deferred under TOKEN with OK and ERROR-TEXT.
Called from whatever the deferring handler was waiting on — for the
daemon-routed merge, its `mergeWorkspace' CommandAck.  An unknown TOKEN is
logged and ignored: the same dispatch runs for interactive merges that have
no host action behind them, and inventing one would complete an action
nobody is owed."
  (let ((action-id (gethash token agent-repl--host-action-deferrals)))
    (if (null action-id)
        (agent-repl--log
         nil
         "host-action: settle for an UNTRACKED token=%s ok=%S — no deferred action is waiting on it"
         token ok)
      (remhash token agent-repl--host-action-deferrals)
      (let* ((entry (gethash action-id agent-repl--host-action-outcomes))
             (type (plist-get entry :type))
             (handler (plist-get entry :handler))
             (cmd (plist-get entry :cmd))
             (ws (plist-get entry :ws))
             (duplicates (or (plist-get entry :duplicates) 0)))
        (if ok
            (agent-repl--host-action-succeed action-id type handler cmd ws)
          (let ((text (or error-text "the deferred host action failed")))
            (puthash action-id
                     (list :state 'failed-unsent :type type :handler handler
                           :cmd cmd :ws ws :ok nil :error text
                           :duplicates duplicates)
                     agent-repl--host-action-outcomes)
            (dotimes (_ (1+ duplicates))
              (agent-repl--workspace-create-send-host-completion
               action-id nil text ws))
            (remhash action-id agent-repl--host-action-outcomes)
            (agent-repl--log
             ws
             "host-action: DEFERRED FAILURE SENT action-id=%s type=%s handler=%s token=%s err-length=%d duplicate-completions=%d retryable-on-next-delivery=yes"
             action-id type handler token (length text) duplicates)
            nil))))))

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
      (let (type handler cmd ws handler-error deferral)
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
              ;; The handler may declare its outcome UNKNOWN yet by setting
              ;; this (see `agent-repl--host-action-defer'), in which case the
              ;; completion waits for whatever the handler is waiting on.
              (let ((agent-repl--host-action-deferral nil))
                (funcall handler cmd)
                (setq deferral agent-repl--host-action-deferral)))
          (error (setq handler-error err)))
        (if (null handler-error)
            (if deferral
                (agent-repl--host-action-hold action-id type handler cmd ws
                                              deferral)
              (agent-repl--host-action-succeed action-id type handler cmd ws))
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
