;;; notifications.el --- desktop notification support for agent-repl -*- lexical-binding: t; -*-

;;; Code:

(defcustom agent-repl-terminal-notifier-executable "terminal-notifier"
  "Name or path of the terminal-notifier binary."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-osascript-executable "osascript"
  "Name or path of the osascript binary."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-notification-sound "default"
  "System sound name used for desktop notifications via osascript."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-notify-process-name "claude-notify"
  "Process name used when spawning notification commands."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-emacsclient-executable
  (expand-file-name "emacsclient" invocation-directory)
  "Path to the emacsclient binary used by clickable notifications.
Clicking a finished-notification runs this binary with `--eval' to focus
Emacs and jump to the originating workspace.  Defaults to the emacsclient
sitting alongside the running Emacs binary (`invocation-directory') so the
click action does not depend on the shell PATH that terminal-notifier
spawns its -execute command under."
  :type 'string
  :group 'agent-repl)

;; Notifications
;;
;; A desktop-notification command can be *invoked* successfully (the
;; `start-process' call returns a live process) yet still fail to post a
;; visible notification — e.g. osascript exits non-zero because the OS
;; suppressed a Script-Editor-attributed notification, or terminal-notifier
;; errors.  Previously the backends fired the command fire-and-forget with
;; output discarded, so that failure was silently swallowed and the log
;; could only prove the notification was *attempted*, never whether it
;; *succeeded*.  `agent-repl--notify-spawn' captures stdout/stderr and the
;; exit status and surfaces a non-zero exit loudly via the log so a failed
;; notification is diagnosable rather than invisible.

(defun agent-repl--notify-process-sentinel (ws backend buffer)
  "Return a process sentinel reporting a notification command's result.
WS is the workspace name (or nil).  BACKEND is a symbol naming the backend
\(for log context).  BUFFER captures the command's stdout/stderr and is
killed once the process terminates.

A zero exit is logged at the gated `agent-repl--log' level; a non-zero
exit (or signal termination) is logged unconditionally via
`agent-repl--do-log' so a failed desktop notification is never silently
swallowed.  The sentinel never signals — it runs from Emacs's process
machinery, where a hard error would simply be dropped."
  (lambda (proc event)
    (when (memq (process-status proc) '(exit signal))
      (let ((status (process-exit-status proc))
            (output (and (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (string-trim (buffer-string))))))
        (if (and (integerp status) (zerop status))
            (agent-repl--log ws "notify-backend=%s ok" backend)
          (agent-repl--do-log
           ws "notify-backend=%s FAILED status=%s event=%s output=%s"
           (list backend status (string-trim (or event "")) (or output ""))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(defun agent-repl--notify-spawn (ws backend program &rest args)
  "Spawn PROGRAM with ARGS for a desktop notification, surfacing failures.
WS is the workspace name (or nil).  BACKEND is a symbol naming the backend
\(for log context).  Unlike a bare fire-and-forget `start-process', the
command's output is captured into a temporary buffer and its exit status
is reported via `agent-repl--notify-process-sentinel', so a notification
tool that exits non-zero is logged rather than silently swallowed.

Returns the spawned process, or nil when `start-process' yields a
non-process value (e.g. a test stub) — in which case the capture buffer is
cleaned up immediately so no orphan buffer leaks."
  (let* ((buffer (generate-new-buffer
                  (format " *%s-output*" agent-repl-notify-process-name)))
         (proc (apply #'start-process agent-repl-notify-process-name
                      buffer program args)))
    (if (processp proc)
        (set-process-sentinel
         proc (agent-repl--notify-process-sentinel ws backend buffer))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))
    proc))

;; Clickable notifications
;;
;; A finished-notification is emitted per-workspace (WS).  Making it
;; clickable means: clicking the banner focuses Emacs and switches to the
;; workspace that finished.  Only terminal-notifier can carry a click
;; action (its -execute flag runs a shell command on click); osascript's
;; `display notification' has no click hook at all.  The click action is a
;; small emacsclient invocation that evaluates
;; `agent-repl--notification-activate', which raises the Emacs frame and
;; jumps to WS.  For that emacsclient call to reach us, a live Emacs server
;; must exist, so `agent-repl--ensure-server' starts one lazily.

(defun agent-repl--ensure-server ()
  "Ensure the Emacs server is running so notification clicks reach Emacs.
Clickable notifications invoke emacsclient, which needs a live server to
evaluate the click action.  No-op under `noninteractive' (batch/ERT),
where no server is useful and starting one would be a side effect."
  (unless noninteractive
    (require 'server)
    (unless (server-running-p)
      (server-start))))

(defun agent-repl--notification-activate (ws)
  "Focus Emacs and switch to workspace WS.
Invoked via emacsclient when a finished-notification is clicked.  Jumps to
WS (so the click lands on the workspace that finished) and then raises and
focuses the selected frame so clicking the banner brings Emacs forward.
Guards against a nil/empty WS so a workspace-free notification click still
focuses Emacs without attempting a bogus jump."
  (agent-repl--log ws "notification-activate ws=%s" ws)
  (when (and ws (stringp ws) (not (string-empty-p ws)))
    (agent-repl-jump-to-workspace ws))
  (select-frame-set-input-focus (selected-frame)))

(defun agent-repl--notification-click-command (ws)
  "Return a shell command string focusing Emacs and switching to WS, or nil.
Used as terminal-notifier's -execute action: on click the shell runs
emacsclient, which evaluates `agent-repl--notification-activate' to raise
Emacs and jump to WS.  Returns nil when WS is nil/empty so a
workspace-free notification stays non-clickable rather than emitting a
malformed action.  Both the executable path and the eval form are
`shell-quote-argument'-escaped so paths with spaces and quotes/parens in
WS survive the shell terminal-notifier spawns the command under."
  (when (and ws (stringp ws) (not (string-empty-p ws)))
    (format "%s --eval %s"
            (shell-quote-argument agent-repl-emacsclient-executable)
            (shell-quote-argument
             (format "(agent-repl--notification-activate %S)" ws)))))

(defun agent-repl--notify-backend-terminal-notifier (ws title message)
  "Send a desktop notification via terminal-notifier for WS.
TITLE and MESSAGE are the notification fields.  When WS is non-nil the
notification is made clickable via terminal-notifier's -execute action
\(see `agent-repl--notification-click-command'): clicking it focuses
Emacs and switches to WS, and the Emacs server is ensured live first so
the click can reach us.  A -sound argument preserves the audible cue that
the osascript backend plays.  The command's exit status is captured and
surfaced to the log via `agent-repl--notify-spawn'."
  (let ((click (agent-repl--notification-click-command ws)))
    (when click
      (agent-repl--ensure-server))
    (apply #'agent-repl--notify-spawn ws 'terminal-notifier
           agent-repl-terminal-notifier-executable
           "-title" title
           "-message" message
           "-sound" agent-repl-notification-sound
           (when click (list "-execute" click)))))

(defun agent-repl--notify-backend-osascript (ws title message)
  "Send a desktop notification via osascript for WS.
TITLE and MESSAGE are the notification fields.  The command's exit status
is captured and surfaced to the log via `agent-repl--notify-spawn', so an
osascript invocation that exits non-zero (or whose notification the OS
suppresses with a diagnostic) is logged instead of swallowed."
  (agent-repl--notify-spawn ws 'osascript
                             agent-repl-osascript-executable "-e"
                             (format "display notification %S with title %S sound name %S"
                                     message title agent-repl-notification-sound)))

(defun agent-repl--select-notification-backend ()
  "Select the best available desktop notification backend.
Prefers terminal-notifier when available: unlike osascript's `display
notification', it supports a click action (-execute), which is what makes
a finished-notification clickable so clicking it focuses Emacs and jumps
to the originating workspace.  Falls back to osascript — which posts a
non-clickable notification — when terminal-notifier is absent.  Signals an
error if no supported notification tool is found."
  (cond
   ((executable-find agent-repl-terminal-notifier-executable)
    (agent-repl--log nil "select-notification-backend: backend=terminal-notifier")
    #'agent-repl--notify-backend-terminal-notifier)
   ((executable-find agent-repl-osascript-executable)
    (agent-repl--log nil "select-notification-backend: backend=osascript")
    #'agent-repl--notify-backend-osascript)
   (t
    (error "agent-repl: no notification backend available (neither terminal-notifier nor osascript found)"))))

(defvar agent-repl--notification-backend (agent-repl--select-notification-backend)
  "Desktop notification backend function, selected at load time based on available platform tools.")

(defun agent-repl--notify (ws title message)
  "Send a desktop notification with TITLE and MESSAGE.
WS is the workspace name string, or nil for workspace-free contexts."
  (agent-repl--log ws "notify title=%s msg=%s" title message)
  (funcall agent-repl--notification-backend ws title message))
