;;; notifications.el --- desktop notification support for claude-repl -*- lexical-binding: t; -*-

;;; Code:

;; Notifications
(defun claude-repl--notify-backend-terminal-notifier (title message)
  "Send a desktop notification via terminal-notifier."
  (call-process "terminal-notifier" nil 0 nil
                "-title" title
                "-message" message))

(defun claude-repl--notify-backend-osascript (title message)
  "Send a desktop notification via osascript."
  (start-process "claude-notify" nil
                 "osascript" "-e"
                 (format "display notification %S with title %S sound name \"default\""
                         message title)))

(defun claude-repl--select-notification-backend ()
  "Select the best available desktop notification backend."
  (cond
   ((executable-find "terminal-notifier")
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
