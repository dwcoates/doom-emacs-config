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
      keyword)))

;;;; ---- StateSnapshot resync --------------------------------------------

(defun agent-repl--frontend-apply-snapshot (snapshot)
  "Apply a `StateSnapshot' frame SNAPSHOT (a plist) — full resync.
Handler for the `snapshot' oneof arm.  Applies every `WorkspaceState' in
the snapshot's `:workspaces' list via
`agent-repl--frontend-apply-workspace-state'.  The `:sessions' and
`:catalogs' arrays are the responsibility of the SessionView / TaskCatalog
handlers (registered by other modules at stitch) and are logged but not
applied here.  Returns the count of workspace states applied."
  (let ((workspaces (plist-get snapshot :workspaces))
        (sessions (plist-get snapshot :sessions))
        (catalogs (plist-get snapshot :catalogs)))
    (agent-repl--log nil
                     "frontend-apply-snapshot: resync — %d workspace(s), %d session(s), %d catalog(s) (states applied here; sessions/catalogs deferred to their handlers)"
                     (length workspaces) (length sessions) (length catalogs))
    (dolist (ws-state workspaces)
      (agent-repl--frontend-apply-workspace-state ws-state))
    (agent-repl--log nil "frontend-apply-snapshot: applied %d workspace state(s)"
                     (length workspaces))
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

(provide 'frontend-state)

;;; frontend-state.el ends here
