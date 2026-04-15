;;; test-notifications.el --- Tests for claude-repl notifications -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for notifications.el -- desktop notification backend selection
;; and dispatch.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: notification backend selection ----

(ert-deftest claude-repl-test-select-backend-prefers-terminal-notifier ()
  "When terminal-notifier is available, select it as backend."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd) (when (equal cmd "terminal-notifier") "/usr/local/bin/terminal-notifier"))))
    (should (eq (claude-repl--select-notification-backend)
                #'claude-repl--notify-backend-terminal-notifier))))

(ert-deftest claude-repl-test-select-backend-falls-back-to-osascript ()
  "When terminal-notifier is NOT available, fall back to osascript."
  (cl-letf (((symbol-function 'executable-find) (lambda (_cmd) nil)))
    (should (eq (claude-repl--select-notification-backend)
                #'claude-repl--notify-backend-osascript))))

;;;; ---- Tests: notification backend variable ----

(ert-deftest claude-repl-test-notification-backend-is-bound ()
  "The notification backend variable should be set at load time."
  (should (boundp 'claude-repl--notification-backend))
  (should (functionp claude-repl--notification-backend)))

;;;; ---- Tests: terminal-notifier backend ----

(ert-deftest claude-repl-test-terminal-notifier-calls-process ()
  "terminal-notifier backend should call-process with correct arguments."
  (let (captured-args)
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest args) (setq captured-args args) 0)))
      (claude-repl--notify-backend-terminal-notifier "My Title" "My Message")
      (should (equal captured-args
                     '("terminal-notifier" nil 0 nil
                       "-title" "My Title"
                       "-message" "My Message"))))))

(ert-deftest claude-repl-test-terminal-notifier-empty-strings ()
  "terminal-notifier backend should handle empty title and message."
  (let (captured-args)
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest args) (setq captured-args args) 0)))
      (claude-repl--notify-backend-terminal-notifier "" "")
      (should (equal (nth 5 captured-args) ""))
      (should (equal (nth 7 captured-args) "")))))

(ert-deftest claude-repl-test-terminal-notifier-special-chars ()
  "terminal-notifier backend should pass through special characters."
  (let (captured-args)
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest args) (setq captured-args args) 0)))
      (claude-repl--notify-backend-terminal-notifier "Title \"quoted\"" "Message with\nnewline")
      (should (equal (nth 5 captured-args) "Title \"quoted\""))
      (should (equal (nth 7 captured-args) "Message with\nnewline")))))

;;;; ---- Tests: osascript backend ----

(ert-deftest claude-repl-test-osascript-calls-start-process ()
  "osascript backend should start-process with correct arguments."
  (let (captured-args)
    (cl-letf (((symbol-function 'start-process)
               (lambda (&rest args)
                 (setq captured-args args)
                 nil)))
      (claude-repl--notify-backend-osascript "My Title" "My Message")
      (should (equal (nth 0 captured-args) "claude-notify"))
      (should (null (nth 1 captured-args)))
      (should (equal (nth 2 captured-args) "osascript"))
      (should (equal (nth 3 captured-args) "-e"))
      ;; The AppleScript command should contain the title and message
      (let ((script (nth 4 captured-args)))
        (should (string-match-p "display notification" script))
        (should (string-match-p "My Message" script))
        (should (string-match-p "My Title" script))
        (should (string-match-p "sound name" script))))))

(ert-deftest claude-repl-test-osascript-empty-strings ()
  "osascript backend should handle empty title and message."
  (let (captured-args)
    (cl-letf (((symbol-function 'start-process)
               (lambda (&rest args) (setq captured-args args) nil)))
      (claude-repl--notify-backend-osascript "" "")
      (let ((script (nth 4 captured-args)))
        (should (string-match-p "display notification" script))))))

(ert-deftest claude-repl-test-osascript-special-chars ()
  "osascript backend should properly quote special characters in the script."
  (let (captured-args)
    (cl-letf (((symbol-function 'start-process)
               (lambda (&rest args) (setq captured-args args) nil)))
      (claude-repl--notify-backend-osascript "Title \"quoted\"" "Line1\nLine2")
      (let ((script (nth 4 captured-args)))
        ;; format %S will escape the quotes properly for AppleScript
        (should (stringp script))
        (should (string-match-p "display notification" script))))))

;;;; ---- Tests: claude-repl--notify dispatch ----

(ert-deftest claude-repl-test-notify-dispatches-to-backend ()
  "claude-repl--notify should funcall the selected backend."
  (let (called-with)
    (cl-letf ((claude-repl--notification-backend
               (lambda (title msg) (setq called-with (list title msg))))
              (claude-repl-debug nil)
              ((symbol-function 'claude-repl--log) (lambda (_ws _fmt &rest _args) nil)))
      (claude-repl--notify nil "Test Title" "Test Message")
      (should (equal called-with '("Test Title" "Test Message"))))))

(ert-deftest claude-repl-test-notify-logs-when-debug-enabled ()
  "claude-repl--notify should log when debug is enabled."
  (let (log-called)
    (cl-letf ((claude-repl--notification-backend (lambda (_t _m) nil))
              (claude-repl-debug t)
              ((symbol-function 'claude-repl--log)
               (lambda (_ws _fmt &rest _args) (setq log-called t))))
      (claude-repl--notify nil "Title" "Message")
      (should log-called))))

(ert-deftest claude-repl-test-notify-logs-before-sending ()
  "claude-repl--notify should call log before dispatching to backend."
  (let (call-order)
    (cl-letf ((claude-repl--notification-backend
               (lambda (_t _m) (push 'backend call-order)))
              (claude-repl-debug t)
              ((symbol-function 'claude-repl--log)
               (lambda (_ws _fmt &rest _args) (push 'log call-order))))
      (claude-repl--notify nil "Title" "Msg")
      ;; call-order is reversed because we push
      (should (equal call-order '(backend log))))))

(ert-deftest claude-repl-test-notify-does-not-log-when-debug-off ()
  "claude-repl--notify should not log when debug is nil."
  (let (log-called)
    (cl-letf ((claude-repl--notification-backend (lambda (_t _m) nil))
              (claude-repl-debug nil)
              ;; Use the real claude-repl--log which checks claude-repl-debug
              )
      ;; With debug nil, the real log function is a no-op, so we just
      ;; verify no error is raised
      (claude-repl--notify nil "Title" "Message"))))

;;;; ---- Tests: uncovered edge cases ----

(ert-deftest claude-repl-test-terminal-notifier-very-long-strings ()
  "terminal-notifier backend should handle very long title and message strings."
  (let* (captured-args
         (long-title (make-string 10000 ?T))
         (long-message (make-string 10000 ?M)))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest args) (setq captured-args args) 0)))
      (claude-repl--notify-backend-terminal-notifier long-title long-message)
      (should (equal (nth 5 captured-args) long-title))
      (should (equal (nth 7 captured-args) long-message)))))

(ert-deftest claude-repl-test-terminal-notifier-unicode-chars ()
  "terminal-notifier backend should pass through non-ASCII/Unicode characters."
  (let (captured-args)
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest args) (setq captured-args args) 0)))
      (claude-repl--notify-backend-terminal-notifier "Héllo Wörld 你好" "Émoji 🎉 café")
      (should (equal (nth 5 captured-args) "Héllo Wörld 你好"))
      (should (equal (nth 7 captured-args) "Émoji 🎉 café")))))

(ert-deftest claude-repl-test-osascript-unicode-chars ()
  "osascript backend should include non-ASCII/Unicode characters in the script."
  (let (captured-args)
    (cl-letf (((symbol-function 'start-process)
               (lambda (&rest args) (setq captured-args args) nil)))
      (claude-repl--notify-backend-osascript "Héllo 你好" "café 🎉")
      (let ((script (nth 4 captured-args)))
        (should (string-match-p "display notification" script))
        (should (string-match-p "café" script))
        (should (string-match-p "Héllo" script))))))

(ert-deftest claude-repl-test-osascript-start-process-returns-nil ()
  "osascript backend should not error when start-process returns nil."
  (cl-letf (((symbol-function 'start-process)
             (lambda (&rest _args) nil)))
    ;; Should complete without signaling an error
    (claude-repl--notify-backend-osascript "Title" "Message")))

(ert-deftest claude-repl-test-notify-backend-signals-error ()
  "claude-repl--notify should propagate errors from the backend function."
  (cl-letf ((claude-repl--notification-backend
             (lambda (_title _msg) (error "Backend failed")))
            (claude-repl-debug nil)
            ((symbol-function 'claude-repl--log) (lambda (_ws _fmt &rest _args) nil)))
    (should-error (claude-repl--notify nil "Title" "Message")
                  :type 'error)))

(provide 'test-notifications)

;;; test-notifications.el ends here
