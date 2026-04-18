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
(defun claude-repl--notify-backend-terminal-notifier (title message)
  "Send a desktop notification via terminal-notifier."
  (call-process claude-repl-terminal-notifier-executable nil 0 nil
                "-title" title
                "-message" message))

(defun claude-repl--notify-backend-osascript (title message)
  "Send a desktop notification via osascript."
  (start-process claude-repl-notify-process-name nil
                 claude-repl-osascript-executable "-e"
                 (format "display notification %S with title %S sound name %S"
                         message title claude-repl-notification-sound)))

(defun claude-repl--select-notification-backend ()
  "Select the best available desktop notification backend."
  (cond
   ((executable-find claude-repl-terminal-notifier-executable)
    (claude-repl--log nil "select-notification-backend: backend=terminal-notifier")
    #'claude-repl--notify-backend-terminal-notifier)
   (t
    (claude-repl--log nil "select-notification-backend: backend=osascript")
    #'claude-repl--notify-backend-osascript)))

(defvar claude-repl--notification-backend (claude-repl--select-notification-backend)
  "Desktop notification backend function, selected at load time based on available platform tools.")

(defun claude-repl--notify (ws title message)
  "Send a desktop notification with TITLE and MESSAGE.
WS is the workspace name string, or nil for workspace-free contexts."
  (claude-repl--log ws "notify title=%s msg=%s" title message)
  (funcall claude-repl--notification-backend title message))
