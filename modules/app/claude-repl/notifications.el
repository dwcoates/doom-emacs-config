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

(defun claude-repl--notify-backend-terminal-notifier (ws title message)
  "Send a desktop notification via terminal-notifier for WS.
TITLE and MESSAGE are the notification fields.  The command's exit status
is captured and surfaced to the log via `claude-repl--notify-spawn'."
  (claude-repl--notify-spawn ws 'terminal-notifier
                             claude-repl-terminal-notifier-executable
                             "-title" title
                             "-message" message))

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
Signals an error if no supported notification tool is found."
  (cond
   ((executable-find claude-repl-osascript-executable)
    (claude-repl--log nil "select-notification-backend: backend=osascript")
    #'claude-repl--notify-backend-osascript)
   ((executable-find claude-repl-terminal-notifier-executable)
    (claude-repl--log nil "select-notification-backend: backend=terminal-notifier")
    #'claude-repl--notify-backend-terminal-notifier)
   (t
    (error "claude-repl: no notification backend available (neither terminal-notifier nor osascript found)"))))

(defvar claude-repl--notification-backend (claude-repl--select-notification-backend)
  "Desktop notification backend function, selected at load time based on available platform tools.")

(defun claude-repl--notify (ws title message)
  "Send a desktop notification with TITLE and MESSAGE.
WS is the workspace name string, or nil for workspace-free contexts."
  (claude-repl--log ws "notify title=%s msg=%s" title message)
  (funcall claude-repl--notification-backend ws title message))
