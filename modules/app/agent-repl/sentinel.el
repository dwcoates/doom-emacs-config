;;; sentinel.el --- file-notify watcher for Claude Code hooks -*- lexical-binding: t; -*-

;;; Code:

(require 'filenotify)

(declare-function agent-repl--do-log "core")
(declare-function agent-repl--mark-ws-thinking "input")
(declare-function agent-repl--mark-dead "status")
(declare-function agent-repl--frontend-api "frontend-client")
(declare-function agent-repl--state-save "history")

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(defconst agent-repl--sentinel-dir
  (agent-repl--global-state-file "workspace-notifications")
  "Directory where Claude Code hooks write sentinel files.
Lives at `~/.claude-emacs/workspace-notifications' (under
`agent-repl--global-state-dir'); the managed hook scripts compute the
identical path as `$HOME/.claude-emacs/workspace-notifications'.")

(defcustom agent-repl-sentinel-debug-log-filename "hook-debug.log"
  "Filename of the hook debug log to ignore in sentinel file processing."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-sentinel-dispatch-actions '(created changed)
  "File-notify actions that trigger sentinel dispatch."
  :type '(repeat symbol)
  :group 'agent-repl)

(defcustom agent-repl-sentinel-poll-file-regexp "\\`[^.]"
  "Regexp for filtering sentinel directory files during polling.
Only files matching this pattern are enumerated."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-sentinel-watch-events '(change)
  "File-notify event types to watch on the sentinel directory."
  :type '(repeat symbol)
  :group 'agent-repl)

;;; Workspace resolution

;; Workspace event detection via file-notify watcher.
;; Claude Code hooks write the CWD to sentinel files in the sentinel
;; directory; we watch it and dispatch by filename: permission_prompt,
;; stop_*, prompt_submit_*.

(defun agent-repl--ws-for-dir-fast (dir)
  "Fast path for DIR: iterate `agent-repl--workspaces' and match :project-dir.
Returns the workspace name whose canonicalized :project-dir equals the
canonicalized git-root of DIR, or nil.

Unlike the previous dir -> git-root -> md5 -> buffer -> persp-lookup chain,
this function does not touch persp-mode or buffer names.  Bypassing the
persp lookup closes the last known leak path that fed \"none\" into
`agent-repl--workspaces' when persp-mode routed an agent buffer to the
wrong perspective."
  (agent-repl--log-verbose nil "ws-for-dir-fast: ENTER dir=%S" dir)
  (let* ((target-root (and dir (agent-repl--git-root dir)))
         (canonical-target (when target-root
                             (agent-repl--path-canonical target-root)))
         (match nil))
    (agent-repl--log-verbose nil "ws-for-dir-fast: git-root=%S canonical=%S"
                              target-root canonical-target)
    (when canonical-target
      (let ((candidates nil))
        (maphash
         (lambda (ws _plist)
           (let* ((proj (agent-repl--ws-get ws :project-dir))
                  (canonical-proj (when proj
                                    (agent-repl--path-canonical proj))))
             (agent-repl--log-verbose ws "ws-for-dir-fast: check ws=%s proj=%S canonical=%S match=%s"
                                       ws proj canonical-proj
                                       (if (and canonical-proj
                                                (string= canonical-target canonical-proj))
                                           "YES" "no"))
             (when (and canonical-proj
                        (string= canonical-target canonical-proj))
               (push ws candidates))))
         agent-repl--workspaces)
        ;; Multiple entries can share one :project-dir — e.g. a no-name
        ;; `SPC TAB n' stub shadowing the real workspace.  Prefer a
        ;; fully-initialized, non-tombstoned workspace (one that ran
        ;; `initialize-ws-env', so it carries :active-env) over a stub;
        ;; matching the stub makes `active-inst' throw on its missing
        ;; :active-env.  Fall back to any match so a genuine single-entry
        ;; lookup is unchanged.
        (setq match (or (cl-find-if
                         (lambda (ws)
                           (and (agent-repl--ws-get ws :active-env)
                                (not (agent-repl--ws-get ws :nuked-at))))
                         candidates)
                        (car candidates)))
        (when (> (length candidates) 1)
          (agent-repl--log nil "ws-for-dir-fast: %d entries share dir=%S — chose ws=%s over candidates=%S"
                            (length candidates) canonical-target match candidates))))
    (if match
        (agent-repl--log-verbose match "ws-for-dir-fast: HIT dir=%S root=%S ws=%s"
                                  dir canonical-target match)
      (agent-repl--log-verbose nil "ws-for-dir-fast: MISS dir=%S root=%S"
                                dir canonical-target))
    match))

(defun agent-repl--ws-for-dir-container (dir)
  "Try container-path matching for DIR.
Docker sandboxes mount worktrees at /<dirname>, so the sentinel CWD
won't match any host path.  Extract the first path component after /
as the container root name and match against workspace project dirs.
Return the workspace name or nil."
  (agent-repl--log-verbose nil "ws-for-dir-container: ENTER dir=%S persp-mode=%s"
                    dir (if (agent-repl--ws-system-available-p) "yes" "no"))
  (unless (agent-repl--ws-system-available-p)
    (agent-repl--log nil "ws-for-dir-container: persp-mode not bound, aborting"))
  (when (agent-repl--ws-system-available-p)
    (let* ((container-root (car (split-string (substring dir 1) "/")))
           (all-ws (agent-repl--ws-all-names))
           (ws-dirs (mapcar (lambda (ws)
                              (cons ws (agent-repl--ws-get ws :project-dir)))
                            all-ws)))
      (agent-repl--log-verbose nil "ws-for-dir-container: container-root=%S all-ws=%S ws-dirs=%S"
                        container-root all-ws ws-dirs)
      (let ((match (cl-loop for (ws . proj-dir) in ws-dirs
                            for canonical = (when proj-dir
                                              (agent-repl--path-canonical proj-dir))
                            for basename = (when canonical
                                             (file-name-nondirectory canonical))
                            do (agent-repl--log-verbose ws "ws-for-dir-container: checking ws=%s proj-dir=%S canonical=%S basename=%S vs container-root=%S match=%s"
                                                 ws proj-dir canonical basename container-root
                                                 (if (and basename (string= container-root basename)) "YES" "no"))
                            when (and basename (string= container-root basename))
                            return ws)))
        (if match
            (agent-repl--log-verbose match "ws-for-dir-container: HIT dir=%S container-root=%S ws=%s"
                              dir container-root match)
          (agent-repl--log nil "ws-for-dir-container: MISS dir=%S container-root=%S"
                            dir container-root))
        match))))

(defun agent-repl--ws-for-dir (dir)
  "Return the workspace name for an agent session rooted at DIR, or nil.
First tries the fast path: git-root -> hash -> buffer -> workspace.
Falls back to container-path matching for Docker sandbox workspaces."
  (agent-repl--log-verbose nil "ws-for-dir: ENTER dir=%S" dir)
  (let* ((fast (agent-repl--ws-for-dir-fast dir))
         (_ (agent-repl--log-verbose nil "ws-for-dir: fast-path returned %S" fast))
         (container (unless fast (agent-repl--ws-for-dir-container dir)))
         (_ (unless fast (agent-repl--log-verbose nil "ws-for-dir: container-path returned %S" container)))
         (ws (or fast container)))
    (agent-repl--log-verbose nil "ws-for-dir: EXIT dir=%S ws=%S (via %s)"
                      dir ws (cond (fast "fast-path") (container "container-path") (t "NONE")))
    ws))

;;; Sentinel file reading

(defconst agent-repl--sentinel-owned-marker "owned"
  "Line-3 value marking a sentinel as written by a MODULE-LAUNCHED CLI.
The hook scripts emit it when `AGENT_REPL_OWNED' (exported by the
interactive start command — see `agent-repl--assemble-cmd' — and the
daemon's shim spawn) or `DOOM_SANDBOX' is set; the daemon's own
sentinel writer emits it unconditionally.  Its absence means a FOREIGN
claude — e.g. a terminal session run by hand in a workspace's
directory — whose session id must never be adopted onto the workspace
\(see `agent-repl--update-session-id-from-sentinel').")

(defun agent-repl--read-sentinel-file (file)
  "Read sentinel FILE and return a plist, or nil.
The plist shape is (:dir DIR :session-id SID :owned BOOL :source SRC).
The file format is three lines: CWD, session_id, then the ownership
marker (`agent-repl--sentinel-owned-marker' or empty).  Older
two-line files (and single-line CWD-only files) parse as unowned with a
nil session-id respectively — the conservative reading, since an
unmarked writer cannot be proven to be ours.

Line 4, written only by the session-start hook, is the SessionStart
`source' field (\"startup\" / \"resume\" / \"clear\" / \"compact\") —
the origin of the new session id.  Absent or empty parses as a nil
`:source' (every non-session-start sentinel, plus files from older
hook scripts)."
  (let ((fname (file-name-nondirectory file)))
    (agent-repl--log-verbose nil "read-sentinel-file: ENTER file=%s exists=%s readable=%s"
                      fname (file-exists-p file) (file-readable-p file))
    (condition-case err
        (let* ((raw (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string)))
               ;; NOT omit-nulls: an unowned sentinel's line 3 is EMPTY,
               ;; and dropping it would shift nothing here but would make
               ;; the marker's absence indistinguishable from a truncated
               ;; file.  Trailing blank lines are trimmed off `raw' first.
               (lines (split-string (string-trim raw) "\n"))
               (dir (string-trim (or (nth 0 lines) "")))
               (session-id (when (nth 1 lines)
                             (let ((s (string-trim (nth 1 lines))))
                               (unless (string-empty-p s) s))))
               (owned (equal (string-trim (or (nth 2 lines) ""))
                             agent-repl--sentinel-owned-marker))
               (source (when (nth 3 lines)
                         (let ((s (string-trim (nth 3 lines))))
                           (unless (string-empty-p s) s)))))
          (agent-repl--log-verbose nil "read-sentinel-file: file=%s dir=%S session-id=%S owned=%s source=%S"
                            fname dir session-id owned source)
          (if (or (string= dir "null") (string= dir ""))
              (progn
                (agent-repl--log nil "read-sentinel-file: REJECTING file=%s with bogus dir=%S (hook may not have received cwd)"
                                  fname dir)
                nil)
            (list :dir dir :session-id session-id :owned owned :source source)))
      (file-missing
       (agent-repl--log nil "read-sentinel-file: RACE file=%s gone between exists-p and read" fname)
       nil)
      (error
       (agent-repl--warn nil "sentinel file read error for %s: %S" fname err)
       (agent-repl--log nil "read-sentinel-file: ERROR file=%s err=%S" fname err)
       nil))))

;;; Event handlers

(defun agent-repl--update-session-id-from-sentinel (ws session-id &optional owned event source)
  "Update the session ID for workspace WS from sentinel data if non-nil.
Only updates when WS is already registered in `agent-repl--workspaces',
SESSION-ID is a non-empty string that differs from the stored value, and
OWNED is non-nil.
Skipping unregistered workspaces is load-bearing: `agent-repl--active-inst'
auto-creates an instantiation in the hash, which would otherwise cause
sentinel fires for workspaces this module doesn't manage (e.g. the
default persp matched via `workspace-for-buffer') to leak entries into
`agent-repl--workspaces'.

OWNED is the sentinel's ownership marker (see
`agent-repl--sentinel-owned-marker').  The hooks are keyed on CWD, so a
FOREIGN claude — a terminal session the user runs by hand inside a
workspace's directory — fires the same hooks and, before this gate,
stamped ITS session id onto the workspace.  The workspace's durable id
is the resume target for both frontends, so the hijack silently made
`SPC o c' (and every daemon-bounce reattach) resume the wrong
conversation.  An unowned sentinel still drives STATE transitions
\(thinking/done/permission — the user does want to see that an agent is
running in that directory); only the identity write is refused.

EVENT is the dispatching handler's `:name' (e.g. \"handle-stop\") and
SOURCE is the SessionStart hook's `source' field (\"startup\" /
\"resume\" / \"clear\" / \"compact\", session-start sentinels only).
Together they gate WHEN an owned id becomes the durable resume target:

  * A `handle-session-start' sentinel only STAGES the id under
    `:incoming-session-id'.  A freshly started (or /clear-rotated, or
    resume-forked) session has NO transcript until its first user
    message, so adopting it immediately would clobber the last
    resumable id with one that resolves to nothing if the session is
    killed before a message is sent — the exact loss behind the
    \"resume target has no transcript\" hard-fail.

  * Every other owned event (prompt-submit, stop, permission,
    subagent traffic) is evidence that a user message reached the
    session — its transcript exists — so the id is promoted to the
    durable stash via `agent-repl--set-session-id' and any staged
    `:incoming-session-id' is cleared."
  (when (and session-id
             (not (string-empty-p session-id))
             (gethash ws agent-repl--workspaces))
    (let* ((inst (agent-repl--active-inst ws))
           (current (agent-repl-instantiation-session-id inst))
           (staged (agent-repl--ws-get ws :incoming-session-id)))
      (cond
       ((equal current session-id)
        ;; Already durable; a staged id (from a rotation that never got
        ;; a message before something re-adopted CURRENT) is stale.
        (when staged
          (agent-repl--log ws "update-session-id-from-sentinel: ws=%s clearing stale staged id %s (durable already %s)"
                            ws staged current)
          (agent-repl--ws-put ws :incoming-session-id nil)))
       ((not owned)
        (agent-repl--log ws "update-session-id-from-sentinel: REFUSING unowned session %s for ws=%s (foreign CLI in this cwd; keeping %s) event=%s source=%s"
                          session-id ws current (or event "?") (or source "-")))
       ((equal event "handle-session-start")
        (agent-repl--log ws "update-session-id-from-sentinel: ws=%s STAGING incoming session %s (origin=%s; durable stays %s until a user message is seen in the new session)"
                          ws session-id (or source "unknown-origin") current)
        (agent-repl--ws-put ws :incoming-session-id session-id))
       (t
        (agent-repl--log ws "update-session-id-from-sentinel: ws=%s PROMOTING %s -> %s (event=%s is message/turn evidence%s)"
                          ws current session-id (or event "?")
                          (if (equal staged session-id) "; was staged" ""))
        (agent-repl--ws-put ws :incoming-session-id nil)
        (agent-repl--set-session-id ws session-id))))))

(defun agent-repl--delete-sentinel-file (file ws)
  "Delete sentinel FILE, surfacing any failure loudly for workspace WS.
Both the normal dispatch path and the deprecated-prefix drain must remove a
sentinel file after acting on it, otherwise the poll fallback re-detects it
every cycle.  This runs from the file-notify / poll async context, where a
hard error would kill the sentinel watcher, so a failed delete is surfaced
via `agent-repl--warn' rather than rethrown."
  (condition-case err
      (delete-file file)
    (error
     (agent-repl--warn ws "could not delete sentinel file %s: %S"
                       (file-name-nondirectory file) err))))

(defun agent-repl--process-sentinel-file (file handler)
  "Read sentinel FILE, delete it, then dispatch it to HANDLER.
Resolves the file's workspace, then invokes HANDLER's callback.
HANDLER is an entry from `agent-repl--sentinel-dispatch-alist' with keys
:warning, :callback, and :name.

Reads the directory and session-id from FILE via
`agent-repl--read-sentinel-file', then deletes FILE immediately so the
poll fallback cannot race a slow handler and re-dispatch the same file.
Resolves the workspace via `agent-repl--ws-for-dir'.  If the read
failed \(nil return), does nothing further.  If the workspace is
nil, logs the :warning message with the directory interpolated via %s.
Otherwise updates the workspace's session ID (if provided), logs a standard
entry using :name, then calls :callback with two arguments: the workspace
name and the directory."
  (let* ((sentinel-data (agent-repl--read-sentinel-file file))
         (dir (plist-get sentinel-data :dir))
         (session-id (plist-get sentinel-data :session-id))
         (owned (plist-get sentinel-data :owned))
         (source (plist-get sentinel-data :source))
         (ws  (when dir (agent-repl--ws-for-dir dir))))
    ;; Delete the file immediately after reading so the poll fallback can't
    ;; re-dispatch it while a slow handler (e.g. panel setup) is still running.
    (agent-repl--delete-sentinel-file file ws)
    (agent-repl--log ws "process-sentinel-file: handler=%s file=%s dir=%S session-id=%S owned=%s source=%S ws=%s"
                      (plist-get handler :name) (file-name-nondirectory file) dir session-id owned source ws)
    (cond
     ((null dir)
      (agent-repl--log nil "process-sentinel-file: dir is nil (read failed) for %s"
                        (file-name-nondirectory file)))
     ((null ws)
      ;; A null ws on a sentinel from a non-git cwd is expected — the headless
      ;; `claude -p' calls in `prompt-summary.el' / `worktree.el' deliberately
      ;; spawn from `temporary-file-directory' so their hooks don't get
      ;; attributed to the calling workspace.  Demote those to a verbose log;
      ;; reserve the user-visible warning for genuine misattribution (cwd is
      ;; inside a git repo, but the watcher couldn't match it).
      (if (agent-repl--git-root dir)
          ;; A sentinel from a real git repo that matched no workspace is a
          ;; genuine misattribution — surface it on the log channel via
          ;; `--do-log' (file + message), not `message' alone where it is
          ;; lost to *Messages*.
          (agent-repl--do-log nil (plist-get handler :warning) (list dir))
        (agent-repl--log-verbose nil
                                  "process-sentinel-file: skipping non-git cwd dir=%s file=%s"
                                  dir (file-name-nondirectory file))))
     (t
      ;; Run the handler under `condition-case' so a thrown error surfaces on
      ;; the agent-repl log (via `--do-log') instead of being swallowed into
      ;; *Messages* by the file-notify / poll layer.  We deliberately do NOT
      ;; rethrow: this runs from a file-notify callback, where a hard error
      ;; would kill the sentinel watcher.  Surfacing on the agent-repl log
      ;; (file + *Messages*, no modeline flash) is the established channel
      ;; for this async context.
      (condition-case err
          (progn
            (agent-repl--update-session-id-from-sentinel
             ws session-id owned (plist-get handler :name) source)
            (agent-repl--log ws "%s: file=%s dir=%s ws=%s status=%s"
                              (plist-get handler :name)
                              (file-name-nondirectory file) dir ws
                              (agent-repl--ws-get ws :agent-state))
            (funcall (plist-get handler :callback) ws dir))
        (error
         (agent-repl--do-log ws "process-sentinel-file: handler=%s ERRORED file=%s dir=%s ws=%s err=%S"
                              (list (plist-get handler :name)
                                    (file-name-nondirectory file) dir ws err))))))))

(defun agent-repl--on-permission-event (ws _dir)
  "Set :permission status on workspace WS.
Callback for both sentinel sources that signal a permission prompt:

  * `permission_request' — written by the PermissionRequest hook, which
    Claude Code fires the moment the permission dialog appears (before
    the user answers).  This is the real-time signal; it's what flips
    the tab to `:permission' WHILE the agent is waiting on the user.

  * `permission_prompt' — written by the Notification hook for
    `notification_type=permission_prompt'.  That notification can lag
    the dialog (Claude Code dispatches it when sending a notification
    ABOUT the prompt — historically including the 60s-idle \"Claude
    Code needs your attention\" nudge under the same type), so by the
    time it arrives the user may already have answered.  Kept as a
    fallback for older Claude Code versions that don't emit
    PermissionRequest.

Whichever arrives first wins; the second is a no-op thanks to the
`:thinking' gate below.

The Notification hook fires for both real permission prompts and the
60s-idle \"Claude Code needs your attention\" nudge under the same
notification_type=permission_prompt.  We can't distinguish them by
message text alone (Claude Code uses the attention wording for some
real permission prompts too), so we gate on `:agent-state' instead:
mid-turn (`:thinking') means the agent is actively working and a
notification at this point is a real permission prompt; any other
state (`:idle' / `:done' / `:permission' / `:init' / nil) means the
turn is settled or already attention-flagged, so the notification is
either a stale idle nudge or a duplicate, and we leave state alone."
  (let ((before (agent-repl--ws-get ws :agent-state)))
    (agent-repl--log-verbose ws "on-permission-event: ws=%s status-BEFORE=%s" ws before)
    (cond
     ((eq before :thinking)
      (agent-repl--ws-set-agent-state ws :permission)
      (agent-repl--log-verbose ws "on-permission-event: ws=%s status-AFTER=%s" ws (agent-repl--ws-get ws :agent-state)))
     (t
      (agent-repl--log ws "on-permission-event: ws=%s ignoring notification (state=%s, not :thinking)" ws before)))))

(defun agent-repl--on-permission-resolved-event (ws _dir)
  "Flip WS back to `:thinking' after a permission prompt was resolved.
Callback for `permission_resolved_*' sentinels, written by the DAEMON
\(never by a hook): the user answers permission prompts in the webview,
so the daemon's permission-resolved frame is the only resolution
signal Emacs ever receives for it.

Gates on `:permission' exactly as `agent-repl--on-permission-event'
gates on `:thinking' — the gate is what makes the daemon's
turn-end/close auto-cancel resolutions order-independent against the
Stop hook's `:done' (whichever lands second finds a non-`:permission'
state and no-ops)."
  (let ((before (agent-repl--ws-get ws :agent-state)))
    (cond
     ((eq before :permission)
      (agent-repl--log ws "on-permission-resolved-event: ws=%s :permission -> :thinking" ws)
      (agent-repl--mark-ws-thinking ws))
     (t
      (agent-repl--log ws "on-permission-resolved-event: ws=%s ignoring (state=%s, not :permission)" ws before)))))

(defun agent-repl--on-session-dead-event (ws _dir)
  "Record that WS's agent backend died (daemon-written shim death).
Callback for `session_dead_*' sentinels, written by the DAEMON when a
gui session's shim dies abnormally (SIGKILL/crash/fatal_error) — the
gui counterpart of the old vterm process sentinel, and the only death
signal a gui workspace ever gets (the 1Hz poll deliberately never
marks gui workspaces dead).

Delegates to `agent-repl--mark-dead', which owns the guarded `:dead'
transition (idempotent, respects `:merged'/`:merge-failed' precedence
and the `:init' grace)."
  (agent-repl--log ws "on-session-dead-event: ws=%s agent-state=%s" ws (agent-repl--ws-get ws :agent-state))
  (agent-repl--mark-dead ws))

(defun agent-repl--on-account-changed-event (ws _dir)
  "Adopt WS's new account root after a daemon-side switch.
Callback for `account_changed_*' sentinels, written by the DAEMON after
the gui's account menu moved the session onto another canonical config
root.  The sentinel is a POKE, not a payload: the authoritative config
dir is fetched back off the daemon (`GET /sessions/{id}/account'), so
the two sides can never disagree about what the new account is.

Stores the result as `:config-dir-override' — a config-dir string, or
`:default' for the daemon's empty answer (the CLI's own ~/.claude root),
which `agent-repl--compute-config-dir' reads ahead of its path-derived
account — then persists it via `agent-repl--state-save' so the override
survives an Emacs restart.  A fetch failure warns and leaves the
override untouched: acting on a guess could pin the workspace to the
wrong account, which is the exact failure the override exists to
prevent."
  (let ((sid (agent-repl--ws-get ws :frontend-session-id)))
    (if (null sid)
        (agent-repl--warn ws "account-changed sentinel for ws=%s with no :frontend-session-id" ws)
      (condition-case err
          (let* ((resp (agent-repl--frontend-api "GET" (format "/sessions/%s/account" sid)))
                 (dir (alist-get 'config_dir resp))
                 (override (if (or (null dir) (string-empty-p dir)) :default dir)))
            (agent-repl--ws-put ws :config-dir-override override)
            (agent-repl--log ws "on-account-changed-event: ws=%s override=%S email=%S"
                              ws override (alist-get 'email resp))
            (agent-repl--state-save ws))
        (error
         (agent-repl--warn ws "account-changed: config-dir fetch failed for ws=%s: %S" ws err))))))

(defun agent-repl--maybe-finalize-stop (ws)
  "If WS is fully stopped (Stop fired AND no pending subagents), finalize it.
Drives the `:thinking → :done' transition via
`agent-repl--handle-agent-finished' and clears the Stop / SubagentStop
tracking so the next turn starts fresh.  No-op when not fully stopped."
  (when (agent-repl--fully-stopped-p ws)
    (agent-repl--log ws "maybe-finalize-stop: ws=%s fully stopped, finalizing" ws)
    (agent-repl--ws-clear-stop-tracking ws)
    (agent-repl--handle-agent-finished ws)))

(defun agent-repl--on-stop-event (ws dir)
  "Handle a Stop event for workspace WS with directory DIR.
Records that Stop has fired and finalizes the turn iff no subagents
are pending.  When subagents are still in flight, the finalization is
deferred to whichever SubagentStop arrives last (see
`agent-repl--maybe-finalize-stop')."
  (agent-repl--log ws "on-stop-event: ws=%s dir=%S pending-subagents=%d"
                    ws dir (agent-repl--ws-pending-subagents ws))
  (agent-repl--ws-set-stop-received ws t)
  (agent-repl--maybe-finalize-stop ws))

(defun agent-repl--on-subagent-start-event (ws dir)
  "Handle a SubagentStart event for workspace WS with directory DIR.
Increments the pending-subagent counter so the workspace stays in
`:thinking' until every spawned subagent reports back via SubagentStop."
  (agent-repl--log ws "on-subagent-start-event: ws=%s dir=%S" ws dir)
  (agent-repl--ws-incf-pending-subagents ws))

(defun agent-repl--on-subagent-stop-event (ws dir)
  "Handle a SubagentStop event for workspace WS with directory DIR.
Decrements the pending-subagent counter.  If this was the last
outstanding subagent and the Stop hook has already fired, finalize
the turn."
  (agent-repl--log ws "on-subagent-stop-event: ws=%s dir=%S" ws dir)
  (agent-repl--ws-decf-pending-subagents ws)
  (agent-repl--maybe-finalize-stop ws))

(defun agent-repl--on-stop-failure-event (ws dir)
  "Handle a StopFailure event for workspace WS with directory DIR.
Sets `:agent-state' to `:stop-failed' (visually distinct from `:dead'
since the agent session is still alive and re-promptable) and clears
the Stop / SubagentStop tracking so a subsequent successful turn isn't
gated on a stale pending-subagent counter from this aborted turn.

No automatic `try again' retry is fired: the StopFailure hook is not
guaranteed to fire only on genuine API errors (server-returned error
codes) — it also fires on non-error turn endings such as context
compaction — and Claude Code exposes no payload field that lets us
reliably distinguish the two.  Auto-re-prompting on those false
positives double-prompts and interferes with the session, so the
failure is surfaced (the ⚠ `:stop-failed' tab plus the log entry) and
recovery is left to the user."
  (agent-repl--log ws "on-stop-failure-event: ws=%s dir=%S" ws dir)
  (agent-repl--ws-clear-stop-tracking ws)
  (agent-repl--ws-set-agent-state ws :stop-failed))

(defun agent-repl--on-prompt-submit-event (ws _dir)
  "Mark workspace WS as thinking after a prompt submission."
  (let ((before (agent-repl--ws-get ws :agent-state)))
    (agent-repl--log-verbose ws "on-prompt-submit-event: ws=%s status-BEFORE=%s" ws before)
    (agent-repl--mark-ws-thinking ws)
    (agent-repl--log-verbose ws "on-prompt-submit-event: ws=%s status-AFTER=%s" ws (agent-repl--ws-get ws :agent-state))))

(defun agent-repl--on-session-start-event (ws _dir)
  "Handle a session_start event for workspace WS.
This is the agent becoming ready.  Flips `:agent-state' from nil/`:init'
to `:idle' (guarded so a lagging/re-fired session_start never clobbers
an in-flight turn), drains any prompts queued before the session came
up (the workspace-generation dispatch enqueues a new workspace's
initial prompt as `:pending-prompts'; the drain is frontend-aware, so
delivery goes through WS's send path), then fires the frontend-agnostic
after-ready hook and flips the agent-side bit on the ws-fully-loaded
latch.

Hook firing (after-ready and the ws-fully-loaded latch flip) runs
UNCONDITIONALLY on every observed `session_start' for WS — duplicate
or stale events still flip the latch and fire the narrow hook so
observers waiting on them are never stalled by a swallowed duplicate."
  (agent-repl--log ws "on-session-start-event: ENTER ws=%s agent-state=%s"
                    ws (agent-repl--ws-get ws :agent-state))
  (when (memq (agent-repl--ws-get ws :agent-state) '(nil :init))
    (agent-repl--ws-set-agent-state ws :idle))
  ;; Flush prompts queued before the session was up (the
  ;; workspace-generation dispatch enqueues the new workspace's
  ;; initial prompt as `:pending-prompts').  The drain is
  ;; frontend-aware, so delivery goes through the gui send path.
  (agent-repl--drain-pending-prompts ws)
  ;; Fire narrow after-ready hook (every observed session_start).
  (agent-repl--run-after-ready-hooks ws)
  ;; Flip the agent-side bit on the fully-loaded latch.  If
  ;; --on-workspace-switch has also fired, this fires the ws-fully-loaded
  ;; hook; otherwise we just record the bit and wait for switch settle.
  (agent-repl--latch-and-maybe-fire-loaded ws :agent-ready))

(defun agent-repl--run-after-ready-hooks (ws)
  "Run `agent-repl-after-ready-functions' for WS, isolating hook errors.
Each erroring hook is logged and skipped rather than rethrown — this
runs from a file-notify callback where a hard error would kill the
sentinel watcher.  Called from `agent-repl--on-session-start-event'."
  (run-hook-wrapped 'agent-repl-after-ready-functions
                    (lambda (fn ws)
                      (condition-case err
                          (funcall fn ws)
                        (error
                         (agent-repl--log ws
                                           "after-ready-hook fn=%s err=%S"
                                           fn err)))
                      nil)
                    ws))

(defvar agent-repl-after-ready-functions nil
  "Hook run when a `session_start' event is observed for a workspace.
Each function is called with one argument: the workspace name (string)
whose agent process printed `session_start'.

Unlike `agent-repl-ws-fully-loaded-functions', this hook is the
NARROW signal — it fires on every observed `session_start' event,
including duplicates that arrive after WS's `:agent-state' has
already left `:init'.  Use this only when you specifically need the
agent-side ready event; prefer `agent-repl-ws-fully-loaded-functions'
for anything that needs the workspace to also be settled on the
Emacs side.

Handlers run via `run-hook-wrapped' wrapped in `condition-case', so a
broken handler cannot prevent later handlers from completing.")

(defvar agent-repl-ws-fully-loaded-functions nil
  "Hook run when a Agent REPL workspace is fully loaded.
A workspace is fully loaded when BOTH:
  1. `--on-session-start-event' has fired (`:agent-ready' bit set),
  2. `--on-workspace-switch' has completed (`:ws-loaded' bit set).

Each handler is called with two arguments: (WS &optional MARKER).
WS is the workspace name string.  MARKER is `:timed-out' when the
snapshot loader's watchdog fired the hook synthetically because the
ws never reached both ready+settled within the timeout; otherwise
MARKER is nil (happy path).  Handlers that don't care about the
distinction may accept `(ws)' alone — the second arg is optional.

This hook is the LOAD BARRIER.  At fire time, no further
agent-repl-managed automatic code is queued to run for this
workspace's load lifecycle.

HANDLERS MUST BE SYNCHRONOUS AND SHORT-RUNNING.  The barrier guarantee
breaks the moment a handler schedules async work (`run-at-time',
`file-notify-add-watch', a process sentinel, `make-thread', etc.) —
that work will interleave with subsequent workspace loads and
resurrect every race this hook was designed to eliminate.  If you
need async behavior keyed off load completion, drive it from a
separate mechanism with its own state, not from this hook.

Handlers run via `run-hook-wrapped' wrapped in `condition-case', so a
broken handler cannot prevent later handlers from completing.

Fires exactly once per load cycle.  Latch state is cleared after
firing; a subsequent kill/restart that resets the ws plist also
implicitly resets the latch bits.")

(defun agent-repl--latch-and-maybe-fire-loaded (ws key &optional marker)
  "Set KEY to t on WS's plist; fire `ws-fully-loaded-functions' if both bits set.
KEY is `:agent-ready' or `:ws-loaded'.  When both bits are now t,
clears them and runs `agent-repl-ws-fully-loaded-functions' with
WS (and MARKER, when non-nil — typically `:timed-out' from the
watchdog path).  Both bits are explicitly cleared after firing so a
later kill-and-relaunch cycle starts clean (kill clears the whole
plist; explicit clear here covers the case where the ws keeps
running but its load cycle has logically ended)."
  (agent-repl--ws-put ws key t)
  (when (and (agent-repl--ws-get ws :agent-ready)
             (agent-repl--ws-get ws :ws-loaded))
    (agent-repl--ws-put ws :agent-ready nil)
    (agent-repl--ws-put ws :ws-loaded nil)
    (agent-repl--log ws "ws-fully-loaded: ws=%s marker=%s" ws (or marker "nil"))
    (run-hook-wrapped 'agent-repl-ws-fully-loaded-functions
                      (lambda (fn ws marker)
                        (condition-case err
                            (funcall fn ws marker)
                          (error
                           (agent-repl--log ws
                                             "ws-fully-loaded-hook fn=%s err=%S"
                                             fn err)))
                        nil)
                      ws marker)))

;;; Event dispatch

(defconst agent-repl--sentinel-dispatch-alist
  ;; ORDER MATTERS: prefixes are matched via `string-prefix-p' in the
  ;; order they appear here, so a longer entry must come before a shorter
  ;; one whenever the longer is itself prefixed by the shorter.  Concretely:
  ;; `stop_failure_*' filenames are also prefixed by `stop_', so the
  ;; `stop_failure_' entry must appear before the bare `stop_' entry —
  ;; otherwise stop-failure files would dispatch to the regular Stop
  ;; handler.  (`subagent_start_' / `subagent_stop_' do not collide with
  ;; `stop_' since their filenames start with `subagent_'.)
  '(;; ORDER MATTERS for this pair too: `permission_request' is itself
    ;; prefixed by `permission_' just like `permission_prompt' is, but
    ;; since neither prefix is a prefix of the other (they diverge at
    ;; the underscore after "permission_"), order between THEM doesn't
    ;; matter.  `permission_request' is listed first because it's the
    ;; canonical/primary signal (real-time PermissionRequest hook) and
    ;; `permission_prompt' is the lagging fallback (Notification hook).
    ("permission_request" . (:callback agent-repl--on-permission-event
                             :warning  "[agent-repl] WARNING: permission-request dir=%s matched no workspace"
                             :name     "handle-permission-request"))
    ("permission_prompt"  . (:callback agent-repl--on-permission-event
                             :warning  "[agent-repl] WARNING: permission dir=%s matched no workspace"
                             :name     "handle-permission"))
    ;; `permission_resolved' shares the `permission_' stem with the two
    ;; entries above but diverges before either is a prefix of the
    ;; other's filenames, so order among the permission_* trio doesn't
    ;; matter.  Written ONLY by the daemon (permission_resolved_<sid>_<reqid>).
    ("permission_resolved" . (:callback agent-repl--on-permission-resolved-event
                              :warning  "[agent-repl] WARNING: permission-resolved dir=%s matched no workspace"
                              :name     "handle-permission-resolved"))
    ;; `session_dead_' vs `session_start_' diverge at "session_d"/"session_s",
    ;; so neither prefixes the other.  Written ONLY by the daemon
    ;; (session_dead_<sid>) on abnormal shim death.
    ("session_dead_"      . (:callback agent-repl--on-session-dead-event
                             :warning  "[agent-repl] WARNING: session-dead dir=%s matched no workspace"
                             :name     "handle-session-dead"))
    ;; `account_changed_' shares no stem with any other entry.  Written
    ;; ONLY by the daemon (account_changed_<sid>) after an account switch
    ;; relaunches the session under another canonical config root.
    ("account_changed_"   . (:callback agent-repl--on-account-changed-event
                             :warning  "[agent-repl] WARNING: account-changed dir=%s matched no workspace"
                             :name     "handle-account-changed"))
    ;; There is no `login_request_' handler here any more.  The gui login
    ;; used to be relayed to Emacs on this channel because the OAuth flow
    ;; needs a TTY and Emacs was the only TTY host in the system.  That
    ;; premise was wrong: the daemon can open a pty of its own, so it now
    ;; runs the login itself and streams the terminal to the webapp, which
    ;; renders it.  See daemon/internal/login.  A stale daemon binary or
    ;; older shim may still emit `login_request_' files, so the prefix is
    ;; listed in `agent-repl--deprecated-sentinel-prefixes' below and drained
    ;; rather than left to spam the log every poll cycle.
    ("subagent_start_"    . (:callback agent-repl--on-subagent-start-event
                             :warning  "[agent-repl] WARNING: subagent-start sentinel dir=%s matched no workspace"
                             :name     "handle-subagent-start"))
    ("subagent_stop_"     . (:callback agent-repl--on-subagent-stop-event
                             :warning  "[agent-repl] WARNING: subagent-stop sentinel dir=%s matched no workspace"
                             :name     "handle-subagent-stop"))
    ("stop_failure_"      . (:callback agent-repl--on-stop-failure-event
                             :warning  "[agent-repl] WARNING: stop-failure sentinel dir=%s matched no workspace"
                             :name     "handle-stop-failure"))
    ("stop_"              . (:callback agent-repl--on-stop-event
                             :warning  "[agent-repl] WARNING: stop sentinel dir=%s matched no workspace (tab may be stuck red)"
                             :name     "handle-stop"))
    ("prompt_submit_"     . (:callback agent-repl--on-prompt-submit-event
                             :warning  "[agent-repl] WARNING: prompt-submit dir=%s matched no workspace"
                             :name     "handle-prompt-submit"))
    ("session_start_"     . (:callback agent-repl--on-session-start-event
                             :warning  "[agent-repl] WARNING: session-start dir=%s matched no workspace"
                             :name     "handle-session-start")))
  "Alist mapping filename prefixes to handler plists.
Each entry is (PREFIX . PLIST) where PLIST has keys:
  :callback  - function called with (WS DIR) on match
  :warning   - format string logged when no workspace matches
               (interpolates DIR via %s)
  :name      - handler name for debug logging

Order matters: see the leading comment in the source for the prefix
overlap between `stop_' and `stop_failure_'.")

(defconst agent-repl--deprecated-sentinel-prefixes
  '("login_request_")
  "Filename prefixes for sentinel channels Emacs no longer acts on.
A file matching one of these is DRAINED (deleted and dropped) rather than
warned about.  It is deliberately NOT in `agent-repl--sentinel-dispatch-alist':
there is no live handler and no side effect to run, only cleanup.

The reason draining matters: an unrecognized sentinel file is never deleted
by the dispatch path, so `agent-repl--poll-workspace-notifications' re-detects
it every poll cycle and re-warns indefinitely.  A retired channel that a
stale daemon binary or older shim can still emit would therefore spam the
log forever.  Draining deletes the file on first sight so the poll fallback
stops seeing it.

`login_request_' was retired by the commit \"Emacs is out of the login
path\": the daemon now owns its own pty and runs `claude /login' itself,
streaming the terminal to the webapp, so Emacs has no reason to act on the
request.  Truly-unknown prefixes are NOT listed here — they still warn and
are left on disk, since a not-yet-reloaded handler may be able to process
them (forward compatibility).")

(defun agent-repl--dispatch-sentinel-file (file)
  "Dispatch FILE to the appropriate sentinel handler.
Matches the filename against `agent-repl--sentinel-dispatch-alist'.
Returns non-nil if a handler was found and called."
  (let* ((name (file-name-nondirectory file))
         (matched-prefix nil)
         (handler (cl-loop for (prefix . plist) in agent-repl--sentinel-dispatch-alist
                           when (string-prefix-p prefix name)
                           do (setq matched-prefix prefix)
                           and return plist)))
    (agent-repl--log nil "dispatch-sentinel-file: file=%s matched-prefix=%S handler=%s"
                      name matched-prefix (if handler (plist-get handler :name) "NONE"))
    (cond
     (handler
      (agent-repl--process-sentinel-file file handler)
      t)
     ((cl-some (lambda (prefix) (string-prefix-p prefix name))
               agent-repl--deprecated-sentinel-prefixes)
      ;; Retired channel: drain the file so the poll fallback stops re-detecting
      ;; and re-warning about it every cycle.  No handler and no side effect run.
      (agent-repl--delete-sentinel-file file nil)
      (agent-repl--log nil "dispatch-sentinel-file: drained deprecated sentinel file=%s" name)
      t)
     (t
      (agent-repl--warn nil "no handler for sentinel file %s" name)
      (agent-repl--log nil "dispatch-sentinel-file: NO HANDLER for file=%s (tried prefixes: %S)"
                        name (mapcar #'car agent-repl--sentinel-dispatch-alist))
      nil))))

(defun agent-repl--dispatch-sentinel-event (event)
  "Handle file-notify EVENT for workspace notification sentinel files.
Dispatches by filename via `agent-repl--sentinel-dispatch-alist'.
Skips files that no longer exist (file-notify often fires multiple events
for a single file creation; the first handler deletes the file).
Ignores events whose file is nil (e.g. `stopped' events fired when a
watch is removed) and events on the hook debug log, which is pure
noise — same filter as the poll path."
  (let* ((descriptor (nth 0 event))
         (action     (nth 1 event))
         (file       (nth 2 event))
         (fname      (and (stringp file) (file-name-nondirectory file))))
    (cond
     ((not (stringp file))
      (agent-repl--log-verbose nil ">>> SENTINEL EVENT SKIPPED: action=%s no file in event=%S"
                        action event))
     ((string= fname agent-repl-sentinel-debug-log-filename)
      nil)
     (t
      (let ((exists (file-exists-p file)))
        (agent-repl--log nil ">>> SENTINEL EVENT: action=%s file=%s exists=%s descriptor=%S event=%S"
                          action fname exists descriptor event)
        (if (and (memq action agent-repl-sentinel-dispatch-actions) exists)
            (let ((result (agent-repl--dispatch-sentinel-file file)))
              (agent-repl--log nil ">>> SENTINEL EVENT DONE: file=%s dispatched=%s" fname result))
          (agent-repl--log-verbose nil ">>> SENTINEL EVENT SKIPPED: action=%s file=%s exists=%s (need created/changed + exists)"
                            action fname exists)))))))

;;; Polling fallback

(defun agent-repl--poll-workspace-notifications ()
  "Scan the sentinel directory for files that file-notify may have missed.
Called periodically as a fallback; any file still present was not picked up
by the file-notify watcher and needs processing."
  (let* ((dir-exists (file-directory-p agent-repl--sentinel-dir))
         (files (if dir-exists
                    (directory-files agent-repl--sentinel-dir t agent-repl-sentinel-poll-file-regexp t)
                  nil))
         ;; Filter out the debug log itself
         (files (cl-remove-if (lambda (f) (string= (file-name-nondirectory f) agent-repl-sentinel-debug-log-filename)) files)))
    (when files
      (agent-repl--log nil "poll-notifications: found %d orphaned file(s): %s"
                        (length files)
                        (mapconcat #'file-name-nondirectory files ", "))
      (dolist (file files)
        (if (file-exists-p file)
            (progn
              (agent-repl--log nil "poll-notifications: processing orphan file=%s" (file-name-nondirectory file))
              (unless (agent-repl--dispatch-sentinel-file file)
                (agent-repl--log nil "poll-notifications: ignoring unknown file %s"
                                  (file-name-nondirectory file))))
          (agent-repl--log nil "poll-notifications: file disappeared before processing: %s"
                            (file-name-nondirectory file)))))))

;; ---------------------------------------------------------------------------
;; File-notify watcher registration (top-level side effect)
;; ---------------------------------------------------------------------------

(defvar agent-repl--sentinel-watch-descriptor nil
  "File-notify descriptor for the sentinel directory watcher.
Stored so we can remove the old watcher before registering a new one
when sentinel.el is reloaded.")

(defun agent-repl--reap-sentinel-watchers ()
  "Remove every file-notify watcher on `agent-repl--sentinel-dir'.
Returns the count removed.  Iterates `file-notify-descriptors' rather
than relying on `agent-repl--sentinel-watch-descriptor', so it also
reclaims descriptors leaked across module reloads where that variable
lost track of the old descriptor."
  (let ((target (file-truename agent-repl--sentinel-dir))
        (removed 0))
    (maphash
     (lambda (desc watch)
       (let ((watch-dir (cond
                         ((and (fboundp 'file-notify--watch-p)
                               (file-notify--watch-p watch))
                          (file-notify--watch-directory watch))
                         ((consp watch) (car watch)))))
         (when (and watch-dir (string= (file-truename watch-dir) target))
           (file-notify-rm-watch desc)
           (cl-incf removed))))
     file-notify-descriptors)
    removed))

(defun agent-repl-reset-sentinel-watchers ()
  "Remove all file-notify watchers on the sentinel dir and re-register one.
Interactive recovery for the reload-accumulated duplicate-watcher case."
  (interactive)
  (let ((removed (agent-repl--reap-sentinel-watchers)))
    (setq agent-repl--sentinel-watch-descriptor
          (file-notify-add-watch agent-repl--sentinel-dir agent-repl-sentinel-watch-events
                                 #'agent-repl--dispatch-sentinel-event))
    (message "agent-repl: removed %d stale watcher(s); new descriptor=%S"
             removed agent-repl--sentinel-watch-descriptor)))

(defun agent-repl-nuke-sentinel-watchers ()
  "Remove every file-notify watcher on the sentinel dir WITHOUT re-registering.
Intended for testing: after nuking, re-eval sentinel.el (or just the
top-level init block) and confirm that exactly one watcher is created.
Useful to verify the init-time reap logic works without restarting Emacs."
  (interactive)
  (let ((removed (agent-repl--reap-sentinel-watchers)))
    (setq agent-repl--sentinel-watch-descriptor nil)
    (message "agent-repl: nuked %d sentinel watcher(s) — no replacement registered"
             removed)))

(make-directory agent-repl--sentinel-dir t)
(let ((reaped (agent-repl--reap-sentinel-watchers)))
  (when (> reaped 0)
    (agent-repl--log nil "sentinel-init: reaped %d stale watcher(s)" reaped)))
(setq agent-repl--sentinel-watch-descriptor
      (file-notify-add-watch agent-repl--sentinel-dir agent-repl-sentinel-watch-events
                             #'agent-repl--dispatch-sentinel-event))
(agent-repl--log nil "sentinel-init: registered watcher descriptor=%S"
                  agent-repl--sentinel-watch-descriptor)
