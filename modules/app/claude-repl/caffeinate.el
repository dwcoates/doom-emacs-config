;;; caffeinate.el --- Prevent macOS sleep while Claude workspaces are working -*- lexical-binding: t; -*-

;;; Commentary:

;; Keeps macOS awake while any registered workspace warrants
;; wakefulness.  Two activity signals are OR'd:
;;
;;   1. `:claude-state' is in `claude-repl-caffeinate-active-states'
;;      (default: `:thinking') — Claude is mid-computation.
;;   2. The workspace has a merge in flight or queued (`:merging t' or
;;      `:repl-state :merge-queued') — the editor needs wakefulness to
;;      detect the workspace-merge sentinel file, run the cherry-pick,
;;      and optionally drive Claude-based conflict resolution.
;;
;; A `caffeinate -i' subprocess is started on the first transition
;; into either active condition and killed once every workspace has
;; resolved out of all of them, so a user-initiated shutdown waits
;; while Claude is computing or a merge is in flight but proceeds
;; immediately once every workspace lands on `:idle' / `:done' /
;; `:permission' AND is not `:merging' / `:merge-queued'.  A merge
;; that lands on `:merged' / `:merge-completed t' (or terminal
;; `:merge-conflict' / `:merge-failed' / `:dead') no longer blocks
;; sleep — the merge work is over, or, in the conflict case, the
;; bottleneck is the user (same exclusion principle as `:permission').
;;
;; This module is a no-op on non-Darwin platforms.  The reconcile
;; trampoline is wired to the central `claude-repl--ws-put' setter
;; (in core.el) via `advice-add' with a key filter, so every plist
;; mutation of one of the relevant keys re-evaluates whether
;; caffeinate should be running.  Advising the central setter rather
;; than the typed wrapper `claude-repl--ws-set-claude-state' is what
;; lets us catch the merge-flow mutations (`:merging',
;; `:merge-completed', `:repl-state :merge-queued') performed
;; directly in worktree.el — those never route through the typed
;; `:claude-state' setter.  Workspace removal (`claude-repl--ws-del')
;; is also advised so a workspace nuked while still `:thinking' or
;; mid-merge cannot orphan the subprocess.

;;; Code:

(defgroup claude-repl-caffeinate nil
  "macOS sleep-prevention while Claude workspaces are active."
  :group 'claude-repl)

(defcustom claude-repl-caffeinate-enabled t
  "Non-nil means manage a `caffeinate' subprocess for macOS sleep prevention.
The subprocess is started when any workspace enters a state in
`claude-repl-caffeinate-active-states', and killed once every workspace
leaves those states.  Ignored on non-Darwin systems."
  :type 'boolean
  :group 'claude-repl-caffeinate)

(defcustom claude-repl-caffeinate-active-states '(:thinking)
  "Claude-state keywords that count as \"actively working\".
While any workspace's `:claude-state' is `memq' of this list, the
caffeinate subprocess is held alive so macOS does not idle-sleep.
`:thinking' is the default; `:permission' is deliberately excluded
because the bottleneck is the user, not the machine.

This is one of two activity signals — see also
`claude-repl--caffeinate-any-merging-p', which keeps caffeinate alive
while any workspace has a merge in flight or queued (independent of
`:claude-state' so a workspace can drop to `:done' and still hold
caffeinate while its sentinel-driven cherry-pick + optional Claude
conflict-resolution path runs)."
  :type '(repeat (choice (const :init)
                         (const :idle)
                         (const :thinking)
                         (const :done)
                         (const :permission)
                         keyword))
  :group 'claude-repl-caffeinate)

(defcustom claude-repl-caffeinate-program "caffeinate"
  "Name (or absolute path) of the macOS caffeinate binary."
  :type 'string
  :group 'claude-repl-caffeinate)

(defcustom claude-repl-caffeinate-args '("-i")
  "Arguments passed to `claude-repl-caffeinate-program'.
Default `-i' prevents system idle sleep.  Add `-d' to also prevent
display sleep, `-s' to assert against system sleep on AC power, etc."
  :type '(repeat string)
  :group 'claude-repl-caffeinate)

(defvar claude-repl--caffeinate-process nil
  "Live `caffeinate' subprocess, or nil when not running.")

(defun claude-repl--caffeinate-supported-p ()
  "Return non-nil when caffeinate management is appropriate for this Emacs.
Requires that the feature be enabled, we're on macOS, and the
`caffeinate' binary is on PATH."
  (and claude-repl-caffeinate-enabled
       (eq system-type 'darwin)
       (executable-find claude-repl-caffeinate-program)))

(defun claude-repl--caffeinate-any-claude-state-active-p ()
  "Return non-nil when any workspace's `:claude-state' is in active list.
Iterates `claude-repl--workspaces' and checks each plist's
`:claude-state' against `claude-repl-caffeinate-active-states'.  First
of the two activity signals consulted by
`claude-repl--caffeinate-any-active-p'."
  (let ((active nil))
    (when (boundp 'claude-repl--workspaces)
      (maphash
       (lambda (_ws plist)
         ;; Skip tombstoned entries — a nuked workspace's residual
         ;; `:claude-state' (e.g. `:thinking' captured pre-nuke) must
         ;; not hold caffeinate alive.  Identity-only records are
         ;; intentionally invisible to runtime predicates.
         (when (and (null (plist-get plist :nuked-at))
                    (memq (plist-get plist :claude-state)
                          claude-repl-caffeinate-active-states))
           (setq active t)))
       claude-repl--workspaces))
    active))

(defun claude-repl--caffeinate-any-merging-p ()
  "Return non-nil when any workspace has a merge in flight or queued.
Mirrors the drawer's MERGING bucket gate
\(`claude-repl-drawer--workspace-section'): workspaces with `:merging t'
\(active cherry-pick in flight) or `:repl-state :merge-queued' (parked
behind another in-flight cherry-pick) are considered active so macOS
cannot idle-sleep between sentinel-driven merge detection and the
follow-up cherry-pick + optional Claude-driven conflict resolution.

`:merged' / `:merge-completed t' / `:merge-conflict' / `:merge-failed'
are *not* considered active: a completed merge has no further work, a
conflict awaiting human resolution is bottlenecked on the user (same
exclusion principle as `:permission'), and a failed merge is terminal."
  (let ((active nil))
    (when (boundp 'claude-repl--workspaces)
      (maphash
       (lambda (_ws plist)
         ;; Skip tombstoned entries — a workspace nuked mid-merge no
         ;; longer needs caffeinate; the sentinel/cherry-pick that the
         ;; `:merging' flag was guarding is moot once the entry is
         ;; tombstoned.
         (when (and (null (plist-get plist :nuked-at))
                    (or (eq (plist-get plist :merging) t)
                        (eq (plist-get plist :repl-state) :merge-queued)))
           (setq active t)))
       claude-repl--workspaces))
    active))

(defun claude-repl--caffeinate-any-active-p ()
  "Return non-nil when any workspace warrants holding macOS awake.
Logical OR of the two activity signals:
- `claude-repl--caffeinate-any-claude-state-active-p' — a workspace's
  `:claude-state' is in `claude-repl-caffeinate-active-states' (e.g.
  Claude is mid-computation).
- `claude-repl--caffeinate-any-merging-p' — a workspace has a merge in
  flight or queued, so the editor must stay awake to detect the
  workspace-merge sentinel file, run the cherry-pick, and optionally
  drive Claude-based conflict resolution."
  (or (claude-repl--caffeinate-any-claude-state-active-p)
      (claude-repl--caffeinate-any-merging-p)))

(defun claude-repl--caffeinate-running-p ()
  "Return non-nil when the caffeinate subprocess is live."
  (and claude-repl--caffeinate-process
       (process-live-p claude-repl--caffeinate-process)))

(defun claude-repl--caffeinate-start ()
  "Spawn the caffeinate subprocess if it isn't already running.
Idempotent: a second call while the process is live is a no-op."
  (unless (claude-repl--caffeinate-running-p)
    (let* ((proc (apply #'start-process
                        "claude-repl-caffeinate"
                        nil
                        claude-repl-caffeinate-program
                        claude-repl-caffeinate-args)))
      (setq claude-repl--caffeinate-process proc)
      (set-process-query-on-exit-flag proc nil)
      (when (fboundp 'claude-repl--log)
        (claude-repl--log nil "caffeinate: started pid=%s args=%S"
                          (process-id proc) claude-repl-caffeinate-args))
      proc)))

(defun claude-repl--caffeinate-stop ()
  "Kill the caffeinate subprocess if running, then clear the handle.
Idempotent: a no-op when nothing is live."
  (when (claude-repl--caffeinate-running-p)
    (let ((pid (process-id claude-repl--caffeinate-process)))
      (delete-process claude-repl--caffeinate-process)
      (when (fboundp 'claude-repl--log)
        (claude-repl--log nil "caffeinate: stopped pid=%s" pid))))
  (setq claude-repl--caffeinate-process nil))

(defun claude-repl--caffeinate-refresh (&rest _)
  "Reconcile caffeinate process state against current workspace activity.
Starts the subprocess when at least one workspace is in an active
state and none is running; stops it when every workspace has
resolved.  Bails on unsupported platforms."
  (when (claude-repl--caffeinate-supported-p)
    (if (claude-repl--caffeinate-any-active-p)
        (claude-repl--caffeinate-start)
      (claude-repl--caffeinate-stop))))

(defconst claude-repl--caffeinate-watched-keys
  '(:claude-state :merging :merge-completed :repl-state)
  "Plist keys whose mutation can change the caffeinate decision.
- `:claude-state' — drives the `--any-claude-state-active-p' branch.
- `:merging' — set t at cherry-pick start, nil on success/failure.
- `:merge-completed' — flipped to t on successful merge (terminal,
  but watched so the refresh fires the moment the merge exits the
  in-flight bucket alongside the `:merging nil' flip).
- `:repl-state' — `:merge-queued' surfaces here, and terminal merge
  states (`:merged' / `:merge-failed' / `:merge-conflict' / `:dead')
  also flow through this key.

A `--ws-put' call with any other key is a no-op for caffeinate and
deliberately skipped to avoid spurious reconciles on hot paths.")

(defun claude-repl--caffeinate-refresh-on-ws-put (_ws key _val)
  "Reconcile caffeinate when KEY is one we watch.
Wraps `claude-repl--caffeinate-refresh' for `:after' advice on
`claude-repl--ws-put'.  Only relevant keys (see
`claude-repl--caffeinate-watched-keys') trigger a reconcile."
  (when (memq key claude-repl--caffeinate-watched-keys)
    (claude-repl--caffeinate-refresh)))

;; Reconcile on every plist mutation of a watched key.  Advising
;; `claude-repl--ws-put' (the single central plist setter in core.el)
;; rather than `claude-repl--ws-set-claude-state' (status.el's typed
;; wrapper for `:claude-state' only) ensures we also catch direct
;; mutations of `:merging' / `:merge-completed' / `:repl-state'
;; performed by the merge flow in worktree.el, which never route
;; through the typed setter.
(advice-add 'claude-repl--ws-put
            :after #'claude-repl--caffeinate-refresh-on-ws-put)

;; Reconcile on workspace removal too, so nuking a workspace that
;; happened to be `:thinking' or mid-merge doesn't leave caffeinate
;; orphaned.
(advice-add 'claude-repl--ws-del
            :after #'claude-repl--caffeinate-refresh)

;; Never leak a subprocess past Emacs exit.
(add-hook 'kill-emacs-hook #'claude-repl--caffeinate-stop)

(provide 'caffeinate)

;;; caffeinate.el ends here
