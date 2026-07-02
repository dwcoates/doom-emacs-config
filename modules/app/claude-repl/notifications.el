;;; notifications.el --- desktop notification support for claude-repl -*- lexical-binding: t; -*-

;;; Code:

(defcustom claude-repl-terminal-notifier-executable "terminal-notifier"
  "Name or path of the terminal-notifier binary."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-osascript-executable "osascript"
  "Name or path of the osascript binary."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-notification-sound "default"
  "System sound name used for desktop notifications via osascript."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-notify-process-name "claude-notify"
  "Process name used when spawning notification commands."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-emacsclient-executable
  (expand-file-name "emacsclient" invocation-directory)
  "Path to the emacsclient binary used by clickable notifications.
Clicking a finished-notification runs this binary with `--eval' to focus
Emacs and jump to the originating workspace.  Defaults to the emacsclient
sitting alongside the running Emacs binary (`invocation-directory') so the
click action does not depend on the shell PATH that terminal-notifier
spawns its -execute command under."
  :type 'string
  :group 'claude-repl)

;; Notifications
;;
;; A desktop-notification command can be *invoked* successfully (the
;; `start-process' call returns a live process) yet still fail to post a
;; visible notification — e.g. osascript exits non-zero because the OS
;; suppressed a Script-Editor-attributed notification, or terminal-notifier
;; errors.  Previously the backends fired the command fire-and-forget with
;; output discarded, so that failure was silently swallowed and the log
;; could only prove the notification was *attempted*, never whether it
;; *succeeded*.  `claude-repl--notify-spawn' captures stdout/stderr and the
;; exit status and surfaces a non-zero exit loudly via the log so a failed
;; notification is diagnosable rather than invisible.

(defun claude-repl--notify-process-sentinel (ws backend buffer)
  "Return a process sentinel reporting a notification command's result.
WS is the workspace name (or nil).  BACKEND is a symbol naming the backend
\(for log context).  BUFFER captures the command's stdout/stderr and is
killed once the process terminates.

A zero exit is logged at the gated `claude-repl--log' level; a non-zero
exit (or signal termination) is logged unconditionally via
`claude-repl--do-log' so a failed desktop notification is never silently
swallowed.  The sentinel never signals — it runs from Emacs's process
machinery, where a hard error would simply be dropped."
  (lambda (proc event)
    (when (memq (process-status proc) '(exit signal))
      (let ((status (process-exit-status proc))
            (output (and (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (string-trim (buffer-string))))))
        (if (and (integerp status) (zerop status))
            (claude-repl--log ws "notify-backend=%s ok" backend)
          (claude-repl--do-log
           ws "notify-backend=%s FAILED status=%s event=%s output=%s"
           (list backend status (string-trim (or event "")) (or output ""))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(defun claude-repl--notify-spawn (ws backend program &rest args)
  "Spawn PROGRAM with ARGS for a desktop notification, surfacing failures.
WS is the workspace name (or nil).  BACKEND is a symbol naming the backend
\(for log context).  Unlike a bare fire-and-forget `start-process', the
command's output is captured into a temporary buffer and its exit status
is reported via `claude-repl--notify-process-sentinel', so a notification
tool that exits non-zero is logged rather than silently swallowed.

Returns the spawned process, or nil when `start-process' yields a
non-process value (e.g. a test stub) — in which case the capture buffer is
cleaned up immediately so no orphan buffer leaks."
  (let* ((buffer (generate-new-buffer
                  (format " *%s-output*" claude-repl-notify-process-name)))
         (proc (apply #'start-process claude-repl-notify-process-name
                      buffer program args)))
    (if (processp proc)
        (set-process-sentinel
         proc (claude-repl--notify-process-sentinel ws backend buffer))
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
;; `claude-repl--notification-activate', which raises the Emacs frame and
;; jumps to WS.  For that emacsclient call to reach us, a live Emacs server
;; must exist, so `claude-repl--ensure-server' starts one lazily.

(defun claude-repl--ensure-server ()
  "Ensure the Emacs server is running so notification clicks reach Emacs.
Clickable notifications invoke emacsclient, which needs a live server to
evaluate the click action.  No-op under `noninteractive' (batch/ERT),
where no server is useful and starting one would be a side effect."
  (unless noninteractive
    (require 'server)
    (unless (server-running-p)
      (server-start))))

(defun claude-repl--notification-activate (ws)
  "Focus Emacs and switch to workspace WS.
Invoked via emacsclient when a finished-notification is clicked.  Jumps to
WS (so the click lands on the workspace that finished) and then raises and
focuses the selected frame so clicking the banner brings Emacs forward.
Guards against a nil/empty WS so a workspace-free notification click still
focuses Emacs without attempting a bogus jump."
  (claude-repl--log ws "notification-activate ws=%s" ws)
  (when (and ws (stringp ws) (not (string-empty-p ws)))
    (claude-repl-jump-to-workspace ws))
  (select-frame-set-input-focus (selected-frame)))

(defun claude-repl--notification-click-command (ws)
  "Return a shell command string focusing Emacs and switching to WS, or nil.
Used as terminal-notifier's -execute action: on click the shell runs
emacsclient, which evaluates `claude-repl--notification-activate' to raise
Emacs and jump to WS.  Returns nil when WS is nil/empty so a
workspace-free notification stays non-clickable rather than emitting a
malformed action.  Both the executable path and the eval form are
`shell-quote-argument'-escaped so paths with spaces and quotes/parens in
WS survive the shell terminal-notifier spawns the command under."
  (when (and ws (stringp ws) (not (string-empty-p ws)))
    (format "%s --eval %s"
            (shell-quote-argument claude-repl-emacsclient-executable)
            (shell-quote-argument
             (format "(claude-repl--notification-activate %S)" ws)))))

(defun claude-repl--notify-backend-terminal-notifier (ws title message)
  "Send a desktop notification via terminal-notifier for WS.
TITLE and MESSAGE are the notification fields.  When WS is non-nil the
notification is made clickable via terminal-notifier's -execute action
\(see `claude-repl--notification-click-command'): clicking it focuses
Emacs and switches to WS, and the Emacs server is ensured live first so
the click can reach us.  A -sound argument preserves the audible cue that
the osascript backend plays.  The command's exit status is captured and
surfaced to the log via `claude-repl--notify-spawn'."
  (let ((click (claude-repl--notification-click-command ws)))
    (when click
      (claude-repl--ensure-server))
    (apply #'claude-repl--notify-spawn ws 'terminal-notifier
           claude-repl-terminal-notifier-executable
           "-title" title
           "-message" message
           "-sound" claude-repl-notification-sound
           (when click (list "-execute" click)))))

(defun claude-repl--notify-backend-osascript (ws title message)
  "Send a desktop notification via osascript for WS.
TITLE and MESSAGE are the notification fields.  The command's exit status
is captured and surfaced to the log via `claude-repl--notify-spawn', so an
osascript invocation that exits non-zero (or whose notification the OS
suppresses with a diagnostic) is logged instead of swallowed."
  (claude-repl--notify-spawn ws 'osascript
                             claude-repl-osascript-executable "-e"
                             (format "display notification %S with title %S sound name %S"
                                     message title claude-repl-notification-sound)))

(defun claude-repl--select-notification-backend ()
  "Select the best available desktop notification backend.
Prefers terminal-notifier when available: unlike osascript's `display
notification', it supports a click action (-execute), which is what makes
a finished-notification clickable so clicking it focuses Emacs and jumps
to the originating workspace.  Falls back to osascript — which posts a
non-clickable notification — when terminal-notifier is absent.  Signals an
error if no supported notification tool is found."
  (cond
   ((executable-find claude-repl-terminal-notifier-executable)
    (claude-repl--log nil "select-notification-backend: backend=terminal-notifier")
    #'claude-repl--notify-backend-terminal-notifier)
   ((executable-find claude-repl-osascript-executable)
    (claude-repl--log nil "select-notification-backend: backend=osascript")
    #'claude-repl--notify-backend-osascript)
   (t
    (error "claude-repl: no notification backend available (neither terminal-notifier nor osascript found)"))))

(defvar claude-repl--notification-backend (claude-repl--select-notification-backend)
  "Desktop notification backend function, selected at load time based on available platform tools.")

(defun claude-repl--notify (ws title message)
  "Send a desktop notification with TITLE and MESSAGE.
WS is the workspace name string, or nil for workspace-free contexts."
  (claude-repl--log ws "notify title=%s msg=%s" title message)
  (funcall claude-repl--notification-backend ws title message))
