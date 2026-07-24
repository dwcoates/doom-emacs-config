;;; frontend-state.el --- Apply daemon-pushed frontend.v1 state -*- lexical-binding: t; -*-

;;; Commentary:

;; The interpretation half of the Emacs dumb renderer (design §5.4, §10).
;; `frontend-uds.el' owns the transport and hands decoded `frontend.v1'
;; frames to handlers registered by oneof field name; this file supplies
;; the three state-bearing handlers and registers them:
;;
;;   - `workspaceState'  -> map the pushed RenderState enum to the existing
;;                          render-state keyword vocabulary and store it as
;;                          the pushed-state source of truth (via workspace.el
;;                          wrappers) that the renderer reads at stitch.
;;   - `snapshot'        -> full resync: apply every WorkspaceState in the
;;                          StateSnapshot on (re)connect.
;;   - `degradedNotice'  -> honest degraded surfacing: echo-area message +
;;                          log.  No fallback behavior, no work-around.
;;
;; The daemon is the SINGLE SOURCE OF TRUTH for render-state (SSM-resolved);
;; Emacs never re-derives it.  Per §10 the local precedence `cond' in
;; `agent-repl--ws-render-status' is replaced by a lookup of the pushed
;; keyword this file stores under the `:pushed-render-state' plist key.
;;
;; No-Silent-Fallbacks (AGENTS.md): an unmappable RenderState (the proto
;; zero value RENDER_STATE_UNSPECIFIED, or any value absent from the closed
;; map), a WorkspaceState missing its `workspace', or a DegradedNotice
;; missing its `component' all fail loudly (log + `error') — never a
;; defaulted value.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; `agent-repl--latch-and-maybe-fire-loaded' lives in sentinel.el; both load
;; as part of the same module, so the symbol is resolved at call time.
(declare-function agent-repl--latch-and-maybe-fire-loaded "sentinel" (ws key &optional marker))
;; `agent-repl--frontend-note-boot-id' lives in frontend-client.el (it owns the
;; reattach give-up state the boot-id change resets); resolved at call time.
(declare-function agent-repl--frontend-note-boot-id "frontend-client" (boot-id))

;;;; ---- State-transition hook -------------------------------------------

(defvar agent-repl-ws-state-transition-functions nil
  "Abnormal hook run each time a daemon-pushed `WorkspaceState' is applied.
Each function is called with three arguments: (WORKSPACE NEW-KEYWORD
PREVIOUS-KEYWORD), where NEW-KEYWORD is the render keyword just stored
under `:pushed-render-state' and PREVIOUS-KEYWORD is what it replaced
\(nil on the first push for a workspace).

This is the single subscription point the agent-shim cutover (design §4.6,
§9.3) re-anchors the merge reactive consequences onto: the daemon now owns
merge STATE (it publishes `:merging'/`:merge-conflict'/`:merged'/… over the
frontend surface), so the magit conflict popup, the merged-teardown, the
close-after-merge, and the parent notification — all previously keyed off
LOCAL `:merge-*' plist flips in worktree.el — key off THIS hook instead
\(see `agent-repl--merge-react-to-pushed-state' in worktree.el).

Handlers run via `run-hook-wrapped' inside `condition-case', so a broken
subscriber cannot prevent state application or later subscribers from
running — the error is loud-logged, never swallowed silently.")

(defun agent-repl--frontend-run-state-transition-hook (workspace new previous)
  "Run `agent-repl-ws-state-transition-functions' for WORKSPACE.
NEW and PREVIOUS are render keywords (PREVIOUS nil on first push).  Each
handler is wrapped so a signal is caught + logged rather than aborting
state application (the log keeps the No-Silent-Fallbacks contract: the
failure is surfaced, not hidden)."
  (run-hook-wrapped 'agent-repl-ws-state-transition-functions
                    (lambda (fn ws n p)
                      (condition-case err
                          (funcall fn ws n p)
                        (error
                         (agent-repl--log ws
                                          "ws-state-transition-hook fn=%s err=%S"
                                          fn err)))
                      nil)
                    workspace new previous))

;;;; ---- RenderState → keyword mapping -----------------------------------

(defconst agent-repl--frontend-render-state-map
  '(("RENDER_STATE_INIT"           . :init)
    ("RENDER_STATE_IDLE"           . :idle)
    ("RENDER_STATE_IDLE_ASYNC"     . :idle-async)
    ("RENDER_STATE_THINKING"       . :thinking)
    ("RENDER_STATE_PERMISSION"     . :permission)
    ("RENDER_STATE_DONE"           . :done)
    ("RENDER_STATE_STOP_FAILED"    . :stop-failed)
    ("RENDER_STATE_MERGING"        . :merging)
    ("RENDER_STATE_MERGE_QUEUED"   . :merge-queued)
    ("RENDER_STATE_MERGE_CONFLICT" . :merge-conflict)
    ("RENDER_STATE_MERGE_FAILED"   . :merge-failed)
    ("RENDER_STATE_MERGED"         . :merged)
    ("RENDER_STATE_DEAD"           . :dead)
    ("RENDER_STATE_DEGRADED"       . :degraded))
  "Map every `RenderState' enum NAME (protojson string) to a render keyword.
The keyword half is the existing closed render-state vocabulary that
renderers look up in `agent-repl-ws-state-icons' (see
`agent-repl--ws-render-status').  Deliberately omits the proto zero
value RENDER_STATE_UNSPECIFIED: receiving it means the daemon pushed an
unresolved state, which is an invariant violation, not a state to
render — `agent-repl--frontend-state->keyword' errors on it.

Note: `:degraded' has no `agent-repl-ws-state-icons' glyph yet; the
stitch phase adds one (see this module's landing report).")

(defun agent-repl--frontend-state->keyword (state)
  "Map RenderState enum NAME STATE (a string) to a render keyword.
Signals `error' (after a loud log) for RENDER_STATE_UNSPECIFIED, a nil
STATE (protojson omits a default-valued enum, i.e. UNSPECIFIED), or any
value outside `agent-repl--frontend-render-state-map' — there is no
fallback keyword (AGENTS.md No-Silent-Fallbacks)."
  (let ((kw (and (stringp state)
                 (cdr (assoc state agent-repl--frontend-render-state-map)))))
    (unless kw
      (agent-repl--log nil
                       "frontend-state->keyword: UNMAPPABLE RenderState=%S (UNSPECIFIED/unknown) — no fallback"
                       state)
      (error "agent-repl frontend: unmappable RenderState %S" state))
    kw))

;;;; ---- WorkspaceState application --------------------------------------

(defun agent-repl--frontend-apply-workspace-state (ws-state)
  "Apply a `WorkspaceState' frame WS-STATE (a plist).
Handler for the `workspaceState' oneof arm.  Maps the pushed RenderState
to a render keyword and stores it under the `:pushed-render-state'
workspace key (via `agent-repl--ws-put') as the pushed-state source of
truth the renderer reads.  The resolution inputs (turn-active, live-task
count, merge phase, cause kind/seq) are stored under
`:pushed-render-state-meta' for debuggability and logged as an old->new
transition.  Returns the applied keyword.

Fails loudly on a missing/blank `workspace' (invariant violation)."
  (let ((workspace (plist-get ws-state :workspace))
        (state (plist-get ws-state :state)))
    (when (or (null workspace) (string-empty-p workspace))
      (agent-repl--log nil
                       "frontend-apply-workspace-state: MISSING workspace in %S — no fallback"
                       ws-state)
      (error "agent-repl frontend: WorkspaceState missing workspace"))
    (let* ((keyword (agent-repl--frontend-state->keyword state))
           (previous (agent-repl--ws-get workspace :pushed-render-state))
           ;; live_task_count / cause_seq / at_ms are proto int64/uint64,
           ;; which protojson encodes as JSON strings — stored verbatim.
           (turn-active (plist-get ws-state :turnActive))
           (live-tasks (plist-get ws-state :liveTaskCount))
           (merge-phase (plist-get ws-state :mergePhase))
           (cause-kind (plist-get ws-state :causeKind))
           (cause-seq (plist-get ws-state :causeSeq)))
      (agent-repl--ws-put workspace :pushed-render-state keyword)
      (agent-repl--ws-put workspace :pushed-render-state-meta
                          (list :turn-active turn-active
                                :live-task-count live-tasks
                                :merge-phase merge-phase
                                :cause-kind cause-kind
                                :cause-seq cause-seq
                                :at-ms (plist-get ws-state :atMs)
                                :session-id (plist-get ws-state :sessionId)))
      (agent-repl--log workspace
                       "frontend-apply-workspace-state: %s -> %s (cause=%s seq=%s turn-active=%S live-tasks=%s merge-phase=%s)"
                       previous keyword cause-kind cause-seq
                       turn-active live-tasks merge-phase)
      ;; Session-ready latch (design §10 cutover gap): the SessionStart
      ;; managed hook that used to set the `:agent-ready' half of the
      ;; ws-fully-loaded latch was deleted in S2, orphaning
      ;; `agent-repl-ws-fully-loaded-functions'.  The daemon now owns
      ;; session-ready reporting, so the FIRST pushed WorkspaceState for a
      ;; workspace (any state) is the ready signal.  One-shot per workspace,
      ;; guarded by `:agent-ready-latched' (cleared when the ws plist resets
      ;; on kill/relaunch, so the next session re-latches).
      (agent-repl--frontend-maybe-latch-agent-ready workspace)
      ;; Re-key point for the merge reactive consequences (design §4.6/§9.3):
      ;; run AFTER the pushed state is stored so subscribers observe it.
      (agent-repl--frontend-run-state-transition-hook workspace keyword previous)
      keyword)))

(defun agent-repl--frontend-maybe-latch-agent-ready (workspace)
  "Set the `:agent-ready' latch bit for WORKSPACE on its FIRST pushed state.
One-shot per workspace: guarded by the `:agent-ready-latched' plist key so
only the first `WorkspaceState' push arms the latch.  Loud-logged.  See
`agent-repl-ws-fully-loaded-functions'."
  (unless (agent-repl--ws-get workspace :agent-ready-latched)
    (agent-repl--ws-put workspace :agent-ready-latched t)
    (agent-repl--log workspace
                     "frontend-latch-agent-ready: first pushed state for ws=%s — setting :agent-ready"
                     workspace)
    (agent-repl--latch-and-maybe-fire-loaded workspace :agent-ready)))

;;;; ---- StateSnapshot resync --------------------------------------------

(defun agent-repl--frontend-apply-snapshot (snapshot)
  "Apply a `StateSnapshot' frame SNAPSHOT (a plist) — full resync.
Handler for the `snapshot' oneof arm.  Applies every `WorkspaceState' in
the snapshot's `:workspaces' list via
`agent-repl--frontend-apply-workspace-state', REBUILDS the SessionView
store wholesale from `:sessions' (the daemon's full roster, terminal
sessions included — a wholesale rebuild drops entries a bounced daemon no
longer knows, unlike an upsert), and applies `:daemon' (DaemonView) for
boot detection.  The `:catalogs' array is the TaskCatalog handler's
responsibility and is logged but not applied here.  Returns the count of
workspace states applied.

On the scoped per-session webapp connection the daemon omits `:daemon'
\(and catalogs); a nil `:daemon' is therefore skipped, not an error.  This
handler runs on the UNSCOPED Emacs connection, which receives all of them."
  (let ((workspaces (plist-get snapshot :workspaces))
        (sessions (plist-get snapshot :sessions))
        (catalogs (plist-get snapshot :catalogs))
        (daemon (plist-get snapshot :daemon)))
    (agent-repl--log nil
                     "frontend-apply-snapshot: resync — %d workspace(s), %d session(s), %d catalog(s), daemon=%S (catalogs deferred to their handler)"
                     (length workspaces) (length sessions) (length catalogs)
                     (and daemon t))
    ;; Rebuild the session roster from scratch: the snapshot is authoritative,
    ;; so a session absent from it (a bounced daemon never heard of) must not
    ;; linger in the store where the orphan/live-p reads would still see it.
    (clrhash agent-repl--frontend-session-views)
    (dolist (view sessions)
      (agent-repl--frontend-apply-session-view view))
    (dolist (ws-state workspaces)
      (agent-repl--frontend-apply-workspace-state ws-state))
    (when daemon
      (agent-repl--frontend-apply-daemon-view daemon))
    (agent-repl--log nil "frontend-apply-snapshot: applied %d workspace state(s), %d session(s)"
                     (length workspaces) (length sessions))
    (length workspaces)))

;;;; ---- DegradedNotice surfacing ----------------------------------------

(defun agent-repl--frontend-apply-degraded-notice (notice)
  "Surface a `DegradedNotice' NOTICE (a plist) honestly.
Handler for the `degradedNotice' oneof arm.  Emits an echo-area message
AND a log line — honest degraded display (design §4.4), never a
work-around or fallback path.  `:recovered' t surfaces the recovery
instead.  Returns the `:recovered' flag.

Fails loudly on a missing/blank `component' (invariant violation)."
  (let ((component (plist-get notice :component))
        (reason (plist-get notice :reason))
        (recovered (plist-get notice :recovered)))
    (when (or (null component) (string-empty-p component))
      (agent-repl--log nil
                       "frontend-apply-degraded-notice: MISSING component in %S — no fallback"
                       notice)
      (error "agent-repl frontend: DegradedNotice missing component"))
    (if recovered
        (progn
          (agent-repl--log nil "frontend-degraded: RECOVERED component=%s reason=%s"
                           component reason)
          (message "agent-repl: %s recovered" component))
      (agent-repl--log nil "frontend-degraded: DEGRADED component=%s reason=%s"
                       component reason)
      (message "agent-repl DEGRADED: %s — %s" component reason))
    recovered))

;;;; ---- SessionView store -----------------------------------------------
;;
;; The daemon pushes a `SessionView' when a session is created/deleted (and
;; carries the full roster — terminal sessions included — in the connect
;; snapshot's `:sessions').  This is the pushed-frame replacement for the
;; Emacs-side GET /sessions poller: the session-CRUD reads that used to hit
;; the daemon (live-p, the create→id correlation, turn-active gating, orphan
;; reap roster) key off THIS store instead.  Keyed by session id; each value
;; is the decoded SessionView plist.

(defvar agent-repl--frontend-session-views (make-hash-table :test 'equal)
  "Hash of session-id -> decoded `SessionView' plist (pushed-frame roster).
Populated by `agent-repl--frontend-apply-session-view' (per-session pushes)
and rebuilt wholesale from the connect snapshot's `:sessions'.  The single
source of truth for daemon session metadata now that Emacs no longer polls
GET /sessions for it.")

(defun agent-repl--frontend-session-view (session-id)
  "Return the stored `SessionView' plist for SESSION-ID, or nil when unknown."
  (and session-id (gethash session-id agent-repl--frontend-session-views)))

(defun agent-repl--frontend-session-views-all ()
  "Return every stored `SessionView' plist (the full known roster)."
  (hash-table-values agent-repl--frontend-session-views))

(defun agent-repl--frontend-live-session-id-for-cwd (cwd)
  "Return the id of a NON-TERMINAL stored SessionView whose workspace is CWD.
The daemon supersedes older sessions on the same transcript, so there is
at most one live session per cwd; nil when none is known yet.  This is how
`createSession' correlates its (ack-receipt-only) command to the id the
daemon delivers on the pushed SessionView."
  (catch 'found
    (maphash (lambda (id view)
               (when (and (equal (plist-get view :workspace) cwd)
                          (not (eq (plist-get view :terminal) t)))
                 (throw 'found id)))
             agent-repl--frontend-session-views)
    nil))

(defun agent-repl--frontend-store-session-view (view)
  "Upsert VIEW (a decoded `SessionView' plist) into the store, keyed by id.
A view with no `:sessionId' is an invariant violation and fails loudly
\(No-Silent-Fallbacks) — the daemon always stamps the id.  Returns the id."
  (let ((id (plist-get view :sessionId)))
    (when (or (null id) (and (stringp id) (string-empty-p id)))
      (agent-repl--log nil
                       "frontend-store-session-view: MISSING sessionId in %S — no fallback"
                       view)
      (error "agent-repl frontend: SessionView missing sessionId"))
    (puthash id view agent-repl--frontend-session-views)
    id))

(defun agent-repl--frontend-apply-session-view (view)
  "Apply a `SessionView' frame VIEW (a plist).  Handler for `sessionView'.
Upserts it into `agent-repl--frontend-session-views' and logs the parity
fields the reattach/orphan/turn-active reads consume.  Returns the id."
  (let ((id (agent-repl--frontend-store-session-view view)))
    (agent-repl--log (plist-get view :workspace)
                     "frontend-apply-session-view: id=%s ws=%s terminal=%S claude-id=%s pending=%s"
                     id (plist-get view :workspace) (plist-get view :terminal)
                     (or (plist-get view :claudeSessionId) "nil")
                     (or (plist-get view :pendingPermissions) "0"))
    id))

;;;; ---- DaemonView (boot/version) ---------------------------------------

(defun agent-repl--frontend-apply-daemon-view (view)
  "Apply a `DaemonView' frame VIEW (a plist).  Handler for `daemonView'.
Routes the daemon `:bootId' into `agent-repl--frontend-note-boot-id' — the
pushed-frame replacement for the boot id the reattach sweep used to read
off the GET /sessions envelope, so a daemon-instance change still resets
the reattach give-ups.  Returns the boot id."
  (let ((boot-id (plist-get view :bootId)))
    (agent-repl--log nil
                     "frontend-apply-daemon-view: boot-id=%s protocol=%s version=%s mtime-ms=%s"
                     (or boot-id "nil") (plist-get view :protocolVersion)
                     (plist-get view :daemonVersion)
                     (or (plist-get view :daemonBinaryMtimeMs) "nil"))
    (agent-repl--frontend-note-boot-id boot-id)
    boot-id))

;;;; ---- Handler registration --------------------------------------------
;;
;; Loaded after `frontend-uds.el' (config.el load order / the test files),
;; so `agent-repl--uds-register-handler' is defined here.

(agent-repl--uds-register-handler "workspaceState"
                                  #'agent-repl--frontend-apply-workspace-state)
(agent-repl--uds-register-handler "snapshot"
                                  #'agent-repl--frontend-apply-snapshot)
(agent-repl--uds-register-handler "degradedNotice"
                                  #'agent-repl--frontend-apply-degraded-notice)
(agent-repl--uds-register-handler "sessionView"
                                  #'agent-repl--frontend-apply-session-view)
(agent-repl--uds-register-handler "daemonView"
                                  #'agent-repl--frontend-apply-daemon-view)

;;;; ---- Module init: open the frontend UDS link -------------------------
;;
;; The agent-shim cutover (design §10, integration item 5) replaces the
;; HTTP status derivation with a daemon push over the frontend UDS: this
;; module-init side effect dials the socket once, after the handlers above
;; are registered, so the daemon's initial StateSnapshot resync lands on a
;; ready dispatcher.  On a failed dial `agent-repl-uds-connect' loud-logs
;; and schedules its own reconnect (design §4.4 honest downtime) — there is
;; no fallback here.  Gated on `agent-repl--frontend-init-inhibited-p' so
;; batch (ert) and the agent sandbox never dial a real socket (the same
;; guard the HTTP reattach sweep uses); this keeps the `agent-repl--uds-connect'
;; external-boundary guard from firing at test load time.

(declare-function agent-repl--frontend-init-inhibited-p "daemon" ())

(unless (agent-repl--frontend-init-inhibited-p)
  (agent-repl-uds-connect))

(provide 'frontend-state)

;;; frontend-state.el ends here
