;;; caffeinate.el --- Prevent macOS sleep while agent workspaces are working -*- lexical-binding: t; -*-

;;; Commentary:

;; Keeps macOS awake while any registered workspace warrants
;; wakefulness.  Two activity signals are OR'd:
;;
;;   1. `:agent-state' is in `agent-repl-caffeinate-active-states'
;;      (default: `:thinking') — the agent is mid-computation.
;;   2. The workspace has a merge in flight or queued (`:merging t' or
;;      `:repl-state :merge-queued') — the editor needs wakefulness to
;;      detect the workspace-merge sentinel file, run the cherry-pick,
;;      and optionally drive Claude-based conflict resolution.
;;
;; A `caffeinate -i' subprocess is started on the first transition
;; into either active condition and killed once every workspace has
;; resolved out of all of them, so a user-initiated shutdown waits
;; while the agent is computing or a merge is in flight but proceeds
;; immediately once every workspace lands on `:idle' / `:done' /
;; `:permission' AND is not `:merging' / `:merge-queued'.  A merge
;; that lands on `:merged' / `:merge-completed t' (or terminal
;; `:merge-conflict' / `:merge-failed' / `:dead') no longer blocks
;; sleep — the merge work is over, or, in the conflict case, the
;; bottleneck is the user (same exclusion principle as `:permission').
;;
;; This module is a no-op on non-Darwin platforms.  The reconcile
;; trampoline is wired to the central `agent-repl--ws-put' setter
;; (in core.el) via `advice-add' with a key filter, so every plist
;; mutation of one of the relevant keys re-evaluates whether
;; caffeinate should be running.  Advising the central setter rather
;; than the typed wrapper `agent-repl--ws-set-agent-state' is what
;; lets us catch the merge-flow mutations (`:merging',
;; `:merge-completed', `:repl-state :merge-queued') performed
;; directly in worktree.el — those never route through the typed
;; `:agent-state' setter.  Workspace removal is caught through the
;; same setter when `agent-repl--ws-del' writes `:nuked-at', so the
;; reconcile observes the completed tombstone before the canonical
;; `ws-del:' lifecycle record closes the operation.

;;; Code:

(defgroup agent-repl-caffeinate nil
  "macOS sleep-prevention while agent workspaces are active."
  :group 'agent-repl)

(defcustom agent-repl-caffeinate-enabled t
  "Non-nil means manage a `caffeinate' subprocess for macOS sleep prevention.
The subprocess is started when any workspace enters a state in
`agent-repl-caffeinate-active-states', and killed once every workspace
leaves those states.  Ignored on non-Darwin systems."
  :type 'boolean
  :group 'agent-repl-caffeinate)

(defcustom agent-repl-caffeinate-active-states '(:thinking)
  "Agent-state keywords that count as \"actively working\".
While any workspace's `:agent-state' is `memq' of this list, the
caffeinate subprocess is held alive so macOS does not idle-sleep.
`:thinking' is the default; `:permission' is deliberately excluded
because the bottleneck is the user, not the machine.

This is one of two activity signals — see also
`agent-repl--caffeinate-any-merging-p', which keeps caffeinate alive
while any workspace has a merge in flight or queued (independent of
`:agent-state' so a workspace can drop to `:done' and still hold
caffeinate while its sentinel-driven cherry-pick + optional agent
conflict-resolution path runs)."
  :type '(repeat (choice (const :init)
                         (const :idle)
                         (const :thinking)
                         (const :done)
                         (const :permission)
                         keyword))
  :group 'agent-repl-caffeinate)

(defcustom agent-repl-caffeinate-program "caffeinate"
  "Name (or absolute path) of the macOS caffeinate binary."
  :type 'string
  :group 'agent-repl-caffeinate)

(defcustom agent-repl-caffeinate-args '("-i")
  "Arguments passed to `agent-repl-caffeinate-program'.
Default `-i' prevents system idle sleep.  Add `-d' to also prevent
display sleep, `-s' to assert against system sleep on AC power, etc."
  :type '(repeat string)
  :group 'agent-repl-caffeinate)

(defvar agent-repl--caffeinate-process nil
  "Live `caffeinate' subprocess, or nil when not running.")

(defun agent-repl--caffeinate-supported-p ()
  "Return non-nil when caffeinate management is appropriate for this Emacs.
Requires that the feature be enabled, we're on macOS, and the
`caffeinate' binary is on PATH."
  (let* ((enabled agent-repl-caffeinate-enabled)
         (darwin-p (eq system-type 'darwin))
         (program agent-repl-caffeinate-program)
         (program-path (and enabled darwin-p (executable-find program)))
         (supported-p (and enabled darwin-p program-path)))
    (agent-repl--log
     nil
     "caffeinate: support-check enabled=%s system-type=%S darwin-p=%s program=%S program-path=%S supported=%s"
     enabled system-type darwin-p program program-path supported-p)
    supported-p))

(defun agent-repl--caffeinate-any-agent-state-active-p ()
  "Return non-nil when any workspace's `:agent-state' is in active list.
Iterates `agent-repl--workspaces' and checks each plist's
`:agent-state' against `agent-repl-caffeinate-active-states'.  First
of the two activity signals consulted by
`agent-repl--caffeinate-any-active-p'."
  (let ((active nil)
        (workspace-count 0)
        (active-workspaces nil)
        (workspaces-bound-p (boundp 'agent-repl--workspaces)))
    (when (boundp 'agent-repl--workspaces)
      (maphash
       (lambda (ws plist)
         ;; Skip tombstoned entries — a nuked workspace's residual
         ;; `:agent-state' (e.g. `:thinking' captured pre-nuke) must
         ;; not hold caffeinate alive.  Identity-only records are
         ;; intentionally invisible to runtime predicates.
         (let* ((tombstoned-p (not (null (plist-get plist :nuked-at))))
                (agent-state (plist-get plist :agent-state))
                (state-active-p (memq agent-state
                                      agent-repl-caffeinate-active-states))
                (included-p (and (not tombstoned-p) state-active-p)))
           (cl-incf workspace-count)
           (when included-p
             (setq active t)
             (push ws active-workspaces))
           ;; The scan can emit one line per workspace on every watched-state
           ;; update, so retain its branch evidence only in verbose mode.
           (agent-repl--log-verbose
            ws
            "caffeinate: agent-state-scan ws=%s tombstoned=%s agent-state=%S active-states=%S included=%s"
            ws tombstoned-p agent-state agent-repl-caffeinate-active-states
            included-p)))
       agent-repl--workspaces))
    (setq active-workspaces (nreverse active-workspaces))
    (agent-repl--log
     nil
     "caffeinate: agent-state-scan result=%s workspaces-bound=%s workspace-count=%d active-workspaces=%S active-states=%S"
     active workspaces-bound-p workspace-count active-workspaces
     agent-repl-caffeinate-active-states)
    active))

(defun agent-repl--caffeinate-any-merging-p ()
  "Return non-nil when any workspace has a merge in flight or queued.
Workspaces with `:merging t'
\(active cherry-pick in flight) or `:repl-state :merge-queued' (parked
behind another in-flight cherry-pick) are considered active so macOS
cannot idle-sleep between sentinel-driven merge detection and the
follow-up cherry-pick + optional agent-driven conflict resolution.

`:merged' / `:merge-completed t' / `:merge-conflict' / `:merge-failed'
are *not* considered active: a completed merge has no further work, a
conflict awaiting human resolution is bottlenecked on the user (same
exclusion principle as `:permission'), and a failed merge is terminal."
  (let ((active nil)
        (workspace-count 0)
        (active-workspaces nil)
        (workspaces-bound-p (boundp 'agent-repl--workspaces)))
    (when (boundp 'agent-repl--workspaces)
      (maphash
       (lambda (ws plist)
         ;; Skip tombstoned entries — a workspace nuked mid-merge no
         ;; longer needs caffeinate; the sentinel/cherry-pick that the
         ;; `:merging' flag was guarding is moot once the entry is
         ;; tombstoned.
         (let* ((tombstoned-p (not (null (plist-get plist :nuked-at))))
                (merging-p (eq (plist-get plist :merging) t))
                (repl-state (plist-get plist :repl-state))
                (merge-queued-p (eq repl-state :merge-queued))
                (included-p (and (not tombstoned-p)
                                 (or merging-p merge-queued-p))))
           (cl-incf workspace-count)
           (when included-p
             (setq active t)
             (push ws active-workspaces))
           ;; The scan can emit one line per workspace on every watched-state
           ;; update, so retain its branch evidence only in verbose mode.
           (agent-repl--log-verbose
            ws
            "caffeinate: merge-scan ws=%s tombstoned=%s merging=%s repl-state=%S merge-queued=%s included=%s"
            ws tombstoned-p merging-p repl-state merge-queued-p included-p)))
       agent-repl--workspaces))
    (setq active-workspaces (nreverse active-workspaces))
    (agent-repl--log
     nil
     "caffeinate: merge-scan result=%s workspaces-bound=%s workspace-count=%d active-workspaces=%S"
     active workspaces-bound-p workspace-count active-workspaces)
    active))

(defun agent-repl--caffeinate-any-active-p ()
  "Return non-nil when any workspace warrants holding macOS awake.
Logical OR of the two activity signals:
- `agent-repl--caffeinate-any-agent-state-active-p' — a workspace's
  `:agent-state' is in `agent-repl-caffeinate-active-states' (e.g.
  Claude is mid-computation).
- `agent-repl--caffeinate-any-merging-p' — a workspace has a merge in
  flight or queued, so the editor must stay awake to detect the
  workspace-merge sentinel file, run the cherry-pick, and optionally
  drive Claude-based conflict resolution."
  (let ((agent-state-active-p
         (agent-repl--caffeinate-any-agent-state-active-p)))
    (if agent-state-active-p
        (progn
          (agent-repl--log
           nil
           "caffeinate: activity-decision agent-state-active=%s merge-active=skipped result=t"
           agent-state-active-p)
          t)
      (let ((merge-active-p (agent-repl--caffeinate-any-merging-p)))
        (agent-repl--log
         nil
         "caffeinate: activity-decision agent-state-active=%s merge-active=%s result=%s"
         agent-state-active-p merge-active-p merge-active-p)
        merge-active-p))))

(defun agent-repl--caffeinate-running-p ()
  "Return non-nil when the caffeinate subprocess is live."
  (and agent-repl--caffeinate-process
       (process-live-p agent-repl--caffeinate-process)))

(defun agent-repl--caffeinate-start ()
  "Spawn the caffeinate subprocess if it isn't already running.
Idempotent: a second call while the process is live is a no-op."
  (if (agent-repl--caffeinate-running-p)
      (agent-repl--log
       nil "caffeinate: start skipped branch=already-running pid=%s program=%S args=%S"
       (process-id agent-repl--caffeinate-process)
       agent-repl-caffeinate-program agent-repl-caffeinate-args)
    (let* ((proc (apply #'start-process
                        "agent-repl-caffeinate"
                        nil
                        agent-repl-caffeinate-program
                        agent-repl-caffeinate-args)))
      (setq agent-repl--caffeinate-process proc)
      (set-process-query-on-exit-flag proc nil)
      (agent-repl--log nil "caffeinate: started pid=%s program=%S args=%S"
                        (process-id proc) agent-repl-caffeinate-program
                        agent-repl-caffeinate-args)
      proc)))

(defun agent-repl--caffeinate-stop ()
  "Kill the caffeinate subprocess if running, then clear the handle.
Idempotent: a no-op when nothing is live."
  (if (agent-repl--caffeinate-running-p)
      (let ((pid (process-id agent-repl--caffeinate-process)))
        (delete-process agent-repl--caffeinate-process)
        (agent-repl--log nil "caffeinate: stopped pid=%s" pid))
    (agent-repl--log
     nil "caffeinate: stop skipped branch=not-running handle-present=%s"
     (not (null agent-repl--caffeinate-process))))
  (setq agent-repl--caffeinate-process nil))

(defun agent-repl--caffeinate-refresh (&rest _)
  "Reconcile caffeinate process state against current workspace activity.
Starts the subprocess when at least one workspace is in an active
state and none is running; stops it when every workspace has
resolved.  Bails on unsupported platforms."
  (let ((supported-p (agent-repl--caffeinate-supported-p)))
    (if supported-p
        (let* ((active-p (agent-repl--caffeinate-any-active-p))
               (running-p (agent-repl--caffeinate-running-p))
               (action (if active-p 'start 'stop)))
          (agent-repl--log
           nil
           "caffeinate: refresh supported=%s active=%s running-before=%s action=%s"
           supported-p active-p running-p action)
          (if active-p
              (agent-repl--caffeinate-start)
            (agent-repl--caffeinate-stop)))
      (agent-repl--log
       nil "caffeinate: refresh skipped branch=unsupported supported=%s"
       supported-p))))

(defconst agent-repl--caffeinate-watched-keys
  '(:agent-state :merging :merge-completed :repl-state :nuked-at)
  "Plist keys whose mutation can change the caffeinate decision.
- `:agent-state' — drives the `--any-agent-state-active-p' branch.
- `:merging' — set t at cherry-pick start, nil on success/failure.
- `:merge-completed' — flipped to t on successful merge (terminal,
  but watched so the refresh fires the moment the merge exits the
  in-flight bucket alongside the `:merging nil' flip).
- `:repl-state' — `:merge-queued' surfaces here, and terminal merge
  states (`:merged' / `:merge-failed' / `:merge-conflict' / `:dead')
  also flow through this key.
- `:nuked-at' — tombstones a removed workspace, excluding any residual
  active merge marker before `agent-repl--ws-del' emits its final log.

A `--ws-put' call with any other key is a no-op for caffeinate and
deliberately skipped to avoid spurious reconciles on hot paths.")

(defun agent-repl--caffeinate-refresh-on-ws-put (ws key val)
  "Reconcile caffeinate when KEY is one we watch.
Wraps `agent-repl--caffeinate-refresh' for `:after' advice on
`agent-repl--ws-put'.  Only relevant keys (see
`agent-repl--caffeinate-watched-keys') trigger a reconcile."
  (let ((watched-p (memq key agent-repl--caffeinate-watched-keys)))
    (agent-repl--log
     ws
     "caffeinate: ws-put advice ws=%s key=%S val=%S watched=%s action=%s"
     ws key val watched-p (if watched-p 'refresh 'skip))
    (when watched-p
      (agent-repl--caffeinate-refresh))))

;; Reconcile on every plist mutation of a watched key.  Advising
;; `agent-repl--ws-put' (the single central plist setter in core.el)
;; rather than `agent-repl--ws-set-agent-state' (status.el's typed
;; wrapper for `:agent-state' only) ensures we also catch direct
;; mutations of `:merging' / `:merge-completed' / `:repl-state'
;; performed by the merge flow in worktree.el, which never route
;; through the typed setter.
(advice-add 'agent-repl--ws-put
            :after #'agent-repl--caffeinate-refresh-on-ws-put)

;; Hot reloads do not undo advice installed by an earlier module version.
;; Remove the retired post-delete callback explicitly now that `:nuked-at'
;; drives the reconcile from inside `agent-repl--ws-del'.
(let ((legacy-ws-del-advice-p
       (advice-member-p #'agent-repl--caffeinate-refresh-on-ws-del
                        'agent-repl--ws-del)))
  (when legacy-ws-del-advice-p
    (advice-remove 'agent-repl--ws-del
                   #'agent-repl--caffeinate-refresh-on-ws-del))
  (agent-repl--log
   nil
   "caffeinate: legacy ws-del advice present=%s action=%s"
   legacy-ws-del-advice-p
   (if legacy-ws-del-advice-p 'removed 'none)))

;; Never leak a subprocess past Emacs exit.
(add-hook 'kill-emacs-hook #'agent-repl--caffeinate-stop)

(provide 'caffeinate)

;;; caffeinate.el ends here
