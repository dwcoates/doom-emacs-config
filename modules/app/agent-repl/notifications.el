;;; notifications.el --- desktop notification support for agent-repl -*- lexical-binding: t; -*-

;;; Code:

(declare-function server-running-p "server")

(defcustom agent-repl-terminal-notifier-executable "terminal-notifier"
  "Name or path of the terminal-notifier binary."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-alerter-executable "alerter"
  "Name or path of the alerter binary.
alerter is the clickable notification backend: it posts through the
UNUserNotificationCenter API current macOS supports and reports a banner
click on stdout, which agent-repl turns into a focus-Emacs-and-switch-to-
workspace action (see `agent-repl--notify-backend-alerter')."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-notification-sender "org.gnu.Emacs"
  "Bundle identifier the alerter backend attributes its banner to.
macOS foregrounds a notification's originating app when the banner is
clicked, so attributing the banner to Emacs makes a click bring Emacs
forward.  Set to the bundle identifier of the running Emacs.app."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-notification-click-timeout-seconds 60
  "Seconds a clickable alerter banner waits for a click before self-dismissing.
alerter blocks until the banner is clicked or dismissed, or until this
many seconds elapse, so this bounds how long a workspace-ready
notification stays clickable from Notification Center."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-osascript-executable "osascript"
  "Name or path of the osascript binary."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-notification-sound "default"
  "System sound name used for desktop notifications via osascript."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-notify-process-name "agent-notify"
  "Process name used when spawning notification commands."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-notify-timeout-seconds 10
  "Seconds a notification command may run before it is treated as hung.
A notification tool is expected to post its banner and exit promptly.  One
that never exits (see `agent-repl--notify-kill-hung') has failed to deliver
and would otherwise linger as an orphan process holding its capture buffer
open forever, with no exit status to report."
  :type 'number
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
;;
;; A third failure mode exists that no exit status can express: the command
;; never terminates at all.  terminal-notifier 2.0.0 does exactly this on
;; macOS 26 — it targets the NSUserNotification API Apple removed, so it
;; blocks forever on a delivery callback that never arrives, posting no
;; banner and yielding no exit status.  Every such spawn leaked an orphan
;; process plus its capture buffer, and the sentinel — which only runs on
;; `exit'/`signal' — never fired, so the log recorded nothing at all.
;; `agent-repl--notify-kill-hung' bounds every notification command by
;; `agent-repl-notify-timeout-seconds' and kills it, converting the silent
;; hang into a loud, sentinel-reported failure.

(defun agent-repl--notify-process-sentinel (ws backend buffer &optional timer-cell on-activate)
  "Return a process sentinel reporting a notification command's result.
WS is the workspace name (or nil).  BACKEND is a symbol naming the backend
\(for log context).  BUFFER captures the command's stdout/stderr and is
killed once the process terminates.  TIMER-CELL, when non-nil, is a
one-element list whose car holds the hang-watchdog timer (see
`agent-repl--notify-spawn'); the timer is cancelled on termination so a
command that exits on its own leaves no pending watchdog behind.

ON-ACTIVATE, when non-nil, is called with the command's trimmed output
string on a zero exit, so a backend that reports a user click via its
stdout (alerter) can act on it in-process.  An error it signals is logged
unconditionally via `agent-repl--do-log' rather than propagated, keeping
the sentinel non-signalling.

A zero exit is logged at the gated `agent-repl--log' level; a non-zero
exit (or signal termination, including the watchdog's kill) is logged
unconditionally via `agent-repl--do-log' so a failed desktop notification
is never silently swallowed.  The sentinel never signals — it runs from
Emacs's process machinery, where a hard error would simply be dropped."
  (lambda (proc event)
    (let ((process-state (process-status proc)))
      (if (not (memq process-state '(exit signal)))
          ;; Process sentinels can receive repeated running-state events.
          (agent-repl--log-verbose
           ws "notify-sentinel backend=%s ignored state=%s event=%s"
           backend process-state (string-trim (or event "")))
      (let ((status (process-exit-status proc))
            (output (and (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (string-trim (buffer-string))))))
        (when (and timer-cell (timerp (car timer-cell)))
          (cancel-timer (car timer-cell))
          (setcar timer-cell nil))
        (if (and (integerp status) (zerop status))
            (progn
              (agent-repl--log ws "notify-backend=%s ok" backend)
              (when on-activate
                (condition-case err
                    (funcall on-activate (or output ""))
                  (error
                   (agent-repl--do-log
                    ws "notify-backend=%s on-activate ERROR: %s"
                    (list backend (error-message-string err)))))))
          (agent-repl--do-log
           ws "notify-backend=%s FAILED status=%s event=%s output=%s"
           (list backend status (string-trim (or event "")) (or output ""))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))))))

(defun agent-repl--notify-kill-hung (proc ws backend)
  "Kill notification process PROC when it has outlived its timeout.
WS is the workspace name (or nil) and BACKEND a symbol naming the backend
\(both for log context).  A notification tool that is still alive after
`agent-repl-notify-timeout-seconds' has failed to deliver its banner, so
the hang is logged unconditionally via `agent-repl--do-log' and PROC is
deleted.
Deleting PROC runs `agent-repl--notify-process-sentinel', which reports the
signal termination and kills the capture buffer, so no orphan process or
buffer survives the hang.  No-op when PROC already terminated."
  (if (and (processp proc) (process-live-p proc))
      (progn
        (agent-repl--do-log
         ws "notify-backend=%s HUNG timeout=%ss killing (no notification delivered)"
         (list backend agent-repl-notify-timeout-seconds))
        (delete-process proc))
    (agent-repl--log-verbose
     ws "notify-backend=%s watchdog ignored process=%s live=%s"
     backend proc (and (processp proc) (process-live-p proc)))))

(defun agent-repl--notify-spawn (ws backend program args &optional watchdog-seconds on-activate)
  "Spawn PROGRAM with ARGS for a desktop notification, surfacing failures.
WS is the workspace name (or nil).  BACKEND is a symbol naming the backend
\(for log context).  ARGS is the list of arguments passed to PROGRAM.
Unlike a bare fire-and-forget `start-process', the command's output is
captured into a temporary buffer and its exit status is reported via
`agent-repl--notify-process-sentinel', so a notification tool that exits
non-zero is logged rather than silently swallowed.

A live process is bounded by a hang watchdog (see
`agent-repl--notify-kill-hung'), so a tool that never exits — and
therefore never delivers a banner nor reaches the sentinel — is killed and
logged instead of leaking forever.  WATCHDOG-SECONDS is how long the
process may run before the watchdog kills it; nil uses
`agent-repl-notify-timeout-seconds'.  A backend whose command is EXPECTED
to stay alive awaiting user interaction (the clickable alerter banner)
passes a value beyond its own self-dismiss timeout so a waiting command is
not mistaken for a hang.  The watchdog timer is handed to the sentinel
through a one-element cell so a normally-exiting command cancels it.

ON-ACTIVATE is threaded to the sentinel and called with the command's
trimmed output on a zero exit, letting a backend that reports a user click
via its stdout (alerter) act on it.

Returns the spawned process, or nil when `start-process' yields a
non-process value (e.g. a test stub) — in which case the capture buffer is
cleaned up immediately so no orphan buffer leaks."
  (let* ((buffer (generate-new-buffer
                  (format " *%s-output*" agent-repl-notify-process-name)))
         (timer-cell (list nil))
         (effective-watchdog (or watchdog-seconds agent-repl-notify-timeout-seconds)))
    (agent-repl--log
     ws "notify-spawn backend=%s program=%s args=%S watchdog=%s activate=%s"
     backend program args effective-watchdog (not (null on-activate)))
    (let ((proc (apply #'start-process agent-repl-notify-process-name
                       buffer program args)))
    (if (processp proc)
        (progn
          (set-process-sentinel
           proc (agent-repl--notify-process-sentinel ws backend buffer timer-cell on-activate))
          (setcar timer-cell
                  (run-at-time effective-watchdog nil
                               #'agent-repl--notify-kill-hung proc ws backend)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (agent-repl--do-log
       ws "notify-spawn backend=%s FAILED non-process-result=%S program=%s args=%S"
       (list backend proc program args)))
      proc)))

;; Clickable notifications
;;
;; A finished-notification is emitted per-workspace (WS).  Making it
;; clickable means: clicking the banner focuses Emacs and switches to the
;; workspace that finished.  Both routes to the click converge on
;; `agent-repl--notification-activate', which jumps to WS and raises Emacs.
;;
;; The primary route is alerter, which delivers on current macOS and
;; reports a click on its stdout.  Emacs owns that process, so its sentinel
;; calls `agent-repl--notification-activate' directly in-process — no
;; emacsclient, no server, no shell command.  The banner is attributed to
;; Emacs via -sender so macOS foregrounds Emacs on the click too.
;;
;; The fallback route is terminal-notifier's -execute flag, which runs a
;; shell command on click (osascript's `display notification' has no click
;; hook at all).  There the click action is a small emacsclient invocation
;; that evaluates `agent-repl--notification-activate', and for that
;; emacsclient call to reach us a live Emacs server must exist, so
;; `agent-repl--ensure-server' starts one lazily.

(defun agent-repl--ensure-server (&optional ws)
  "Ensure the Emacs server is running so notification clicks reach Emacs.
Clickable notifications invoke emacsclient, which needs a live server to
evaluate the click action.  No-op under `noninteractive' (batch/ERT),
where no server is useful and starting one would be a side effect."
  (if noninteractive
      (agent-repl--log-verbose ws "ensure-server skipped noninteractive=t")
    (require 'server)
    (if (server-running-p)
        (agent-repl--log ws "ensure-server running=t")
      (agent-repl--log ws "ensure-server running=nil starting=t")
      (server-start))))

(defun agent-repl--notification-activate (ws)
  "Focus Emacs and switch to workspace WS.
Invoked via emacsclient when a finished-notification is clicked.  Jumps to
WS (so the click lands on the workspace that finished) and then raises and
focuses the selected frame so clicking the banner brings Emacs forward.
Guards against a nil/empty WS so a workspace-free notification click still
focuses Emacs without attempting a bogus jump."
  (agent-repl--log ws "notification-activate ws=%s" ws)
  (let ((navigable (and ws (stringp ws) (not (string-empty-p ws)))))
    (agent-repl--log ws "notification-activate navigable=%s" navigable)
    (when navigable
    (agent-repl--switch-to-workspace ws))
    (select-frame-set-input-focus (selected-frame))))

(defun agent-repl--notification-click-command (ws)
  "Return a shell command string focusing Emacs and switching to WS, or nil.
Used as terminal-notifier's -execute action: on click the shell runs
emacsclient, which evaluates `agent-repl--notification-activate' to raise
Emacs and jump to WS.  Returns nil when WS is nil/empty so a
workspace-free notification stays non-clickable rather than emitting a
malformed action.  Both the executable path and the eval form are
`shell-quote-argument'-escaped so paths with spaces and quotes/parens in
WS survive the shell terminal-notifier spawns the command under."
  (if (and ws (stringp ws) (not (string-empty-p ws)))
      (let ((command
             (format "%s --eval %s"
                     (shell-quote-argument agent-repl-emacsclient-executable)
                     (shell-quote-argument
                      (format "(agent-repl--notification-activate %S)" ws)))))
        (agent-repl--log ws "notification-click-command clickable=t executable=%s command=%s"
                         agent-repl-emacsclient-executable command)
        command)
    (agent-repl--log-verbose ws "notification-click-command clickable=nil ws=%S" ws)
    nil))

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
    (agent-repl--log ws "notify-terminal-notifier clickable=%s title=%s message=%s"
                     (not (null click)) title message)
    (when click
      (agent-repl--ensure-server ws))
    (agent-repl--notify-spawn
     ws 'terminal-notifier agent-repl-terminal-notifier-executable
     (append (list "-title" title
                   "-message" message
                   "-sound" agent-repl-notification-sound)
             (when click (list "-execute" click))))))

(defun agent-repl--notify-backend-osascript (ws title message)
  "Send a desktop notification via osascript for WS.
TITLE and MESSAGE are the notification fields.  The command's exit status
is captured and surfaced to the log via `agent-repl--notify-spawn', so an
osascript invocation that exits non-zero (or whose notification the OS
suppresses with a diagnostic) is logged instead of swallowed."
  (agent-repl--log ws "notify-osascript title=%s message=%s" title message)
  (agent-repl--notify-spawn
   ws
   'osascript agent-repl-osascript-executable
   (list "-e" (format "display notification %S with title %S sound name %S"
                      message title agent-repl-notification-sound))))

(defun agent-repl--alerter-click-p (output &optional ws)
  "Return non-nil when alerter OUTPUT reports the banner was clicked.
alerter prints an @-prefixed activation token on exit: `@CONTENTCLICKED'
when the notification body is clicked and `@ACTIONCLICKED' for an action
button, versus `@TIMEOUT'/`@CLOSED' for a self-dismiss or dismissal.  Only
a click should focus Emacs and switch workspaces, so a dismissal returns
nil and leaves Emacs undisturbed."
  (let* ((token (string-trim (or output "")))
         (clicked (or (string-prefix-p "@CONTENTCLICKED" token)
                      (string-prefix-p "@ACTIONCLICKED" token))))
    (agent-repl--log ws "alerter-activation token=%s clicked=%s" token clicked)
    clicked))

(defun agent-repl--notify-backend-alerter (ws title message)
  "Send a clickable desktop notification via alerter for WS.
TITLE and MESSAGE are the notification fields.  alerter posts through the
UNUserNotificationCenter API current macOS supports, so it delivers a
banner where terminal-notifier only hangs.  Unlike osascript, alerter
blocks until the banner is clicked or dismissed — or self-dismisses after
`agent-repl-notification-click-timeout-seconds' — printing an activation
token to stdout.  The banner is attributed to Emacs via --sender
\(`agent-repl-notification-sender') so a click foregrounds Emacs, and the
on-activate handler runs in-process to focus Emacs and switch to WS when
the token reports a click (see `agent-repl--alerter-click-p' and
`agent-repl--notification-activate').  A --group keyed to WS coalesces
repeat notifications for the same workspace.

Flags are passed GNU-style with a double dash (`--message', `--title', …):
the current alerter is a Swift ArgumentParser CLI that only accepts
double-dash long options and rejects single-dash spellings with exit 64
\(`At least one of --message, --remove, or --list is required'), delivering
no banner.  Double dashes are also accepted by the legacy Go alerter, so
this spelling works across both.

Because alerter is EXPECTED to stay alive awaiting a click, the hang
watchdog is set beyond its self-dismiss timeout so a waiting banner is not
mistaken for a hang."
  (let* ((timeout agent-repl-notification-click-timeout-seconds)
         (keyed (and ws (stringp ws) (not (string-empty-p ws)))))
    (agent-repl--log ws "notify-alerter keyed=%s timeout=%s title=%s message=%s"
                     keyed timeout title message)
    (agent-repl--notify-spawn
     ws 'alerter agent-repl-alerter-executable
     (append (list "--title" title
                   "--message" message
                   "--sound" agent-repl-notification-sound
                   "--sender" agent-repl-notification-sender
                   "--timeout" (number-to-string timeout))
             (when keyed (list "--group" (concat "agent-repl:" ws))))
     (+ timeout agent-repl-notify-timeout-seconds)
     (lambda (output)
       (when (agent-repl--alerter-click-p output ws)
         (agent-repl--notification-activate ws))))))

(defun agent-repl--select-notification-backend ()
  "Select the best available desktop notification backend.
Prefers alerter: it delivers through the UNUserNotificationCenter API
current macOS supports AND carries a click action, so a workspace-ready
banner both appears and, when clicked, focuses Emacs and switches to the
originating workspace (see `agent-repl--notify-backend-alerter').

Falls back to osascript, whose `display notification' still posts a banner
on current macOS but has no click hook at all, so its notifications are
not clickable.

terminal-notifier is the last resort: it alone once carried a click action
via -execute, but terminal-notifier 2.0.0 is built on the NSUserNotification
API that macOS 26 removed — it hangs forever, delivers no banner, and never
exits.  It is kept only for a host lacking both alerter and osascript.

Signals an error if no supported notification tool is found."
  (cond
   ((executable-find agent-repl-alerter-executable)
    (agent-repl--log nil "select-notification-backend: backend=alerter")
    #'agent-repl--notify-backend-alerter)
   ((executable-find agent-repl-osascript-executable)
    (agent-repl--log nil "select-notification-backend: backend=osascript")
    #'agent-repl--notify-backend-osascript)
   ((executable-find agent-repl-terminal-notifier-executable)
    (agent-repl--log nil "select-notification-backend: backend=terminal-notifier")
    #'agent-repl--notify-backend-terminal-notifier)
   (t
    (agent-repl--error
     nil "select-notification-backend FAILED alerter=%s osascript=%s terminal-notifier=%s"
     agent-repl-alerter-executable agent-repl-osascript-executable
     agent-repl-terminal-notifier-executable))))

(defvar agent-repl--notification-backend (agent-repl--select-notification-backend)
  "Desktop notification backend function, selected at load time
based on available platform tools.")

(defun agent-repl--emacs-focused-p (&optional ws)
  "Return non-nil when Emacs is the focused desktop application.
Emacs owns desktop focus when ANY of its live frames holds OS input
focus, so this scans `frame-focus-state' across every frame rather than
only the selected one — a focused-but-not-selected frame still means
Emacs is frontmost.  A frame whose focus is `unknown' counts as focused
too, matching the conservative \"suppress when possibly focused\" stance
the desktop-notification gate relies on (see `agent-repl--notify').
Returns nil under `noninteractive' (batch/ERT), where no window-system
frame can hold focus."
  (if noninteractive
      (progn
        (agent-repl--log-verbose ws "emacs-focused-p noninteractive=t focused=nil")
        nil)
    (let* ((frames (frame-list))
           (focused (seq-some #'frame-focus-state frames)))
      (agent-repl--log-verbose ws "emacs-focused-p noninteractive=nil frame-count=%s focused=%s"
                               (length frames) focused)
      focused)))

(defun agent-repl--notify (ws title message)
  "Send a desktop notification with TITLE and MESSAGE.
WS is the workspace name string, or nil for workspace-free contexts.
A desktop banner is only useful when the user is looking elsewhere, so
the notification is suppressed when Emacs is the focused desktop
application (see `agent-repl--emacs-focused-p').  The focus check runs
here at emit time, so focus regained during `agent-repl-notify-delay'
between scheduling and firing still suppresses the banner."
  (if (agent-repl--emacs-focused-p ws)
      (agent-repl--log ws "notify SKIPPED (emacs focused) title=%s msg=%s" title message)
    (agent-repl--log ws "notify title=%s msg=%s" title message)
    (funcall agent-repl--notification-backend ws title message)))
