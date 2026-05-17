;;; caffeinate.el --- Prevent macOS sleep while Claude workspaces are working -*- lexical-binding: t; -*-

;;; Commentary:

;; Keeps macOS awake while any registered workspace is in an "active"
;; `:claude-state' (default: `:thinking').  A `caffeinate -i'
;; subprocess is started on the first transition into an active state
;; and killed once every workspace has resolved out of those states,
;; so a user-initiated shutdown will wait while Claude is still
;; computing but proceed immediately once every workspace lands on
;; `:idle' / `:done' / `:permission'.
;;
;; This module is a no-op on non-Darwin platforms.  The reconcile
;; trampoline is wired to the central `claude-repl--ws-set-claude-state'
;; setter via `advice-add', so every state mutation re-evaluates
;; whether caffeinate should be running.  Workspace removal
;; (`claude-repl--ws-del') is also advised so a workspace that gets
;; nuked while still `:thinking' cannot orphan the subprocess.

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
because the bottleneck is the user, not the machine."
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

(defun claude-repl--caffeinate-any-active-p ()
  "Return non-nil when any registered workspace is in an active state.
Iterates `claude-repl--workspaces' and checks each plist's
`:claude-state' against `claude-repl-caffeinate-active-states'."
  (let ((active nil))
    (when (boundp 'claude-repl--workspaces)
      (maphash
       (lambda (_ws plist)
         (when (memq (plist-get plist :claude-state)
                     claude-repl-caffeinate-active-states)
           (setq active t)))
       claude-repl--workspaces))
    active))

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

;; Reconcile on every claude-state mutation.  Using :after advice on
;; the single central setter (`claude-repl--ws-set-claude-state') keeps
;; this module decoupled from status.el's internals.
(advice-add 'claude-repl--ws-set-claude-state
            :after #'claude-repl--caffeinate-refresh)

;; Reconcile on workspace removal too, so nuking a workspace that
;; happened to be `:thinking' doesn't leave caffeinate orphaned.
(advice-add 'claude-repl--ws-del
            :after #'claude-repl--caffeinate-refresh)

;; Never leak a subprocess past Emacs exit.
(add-hook 'kill-emacs-hook #'claude-repl--caffeinate-stop)

(provide 'caffeinate)

;;; caffeinate.el ends here
