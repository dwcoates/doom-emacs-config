;;; test-notifications.el --- Tests for claude-repl notifications -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for notifications.el -- desktop notification backend selection,
;; dispatch, and the failure-surfacing spawn/sentinel layer.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: notification backend selection ----

(ert-deftest claude-repl-test-select-backend-prefers-terminal-notifier-when-both-available ()
  "When both are available, prefer terminal-notifier (it supports click actions)."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_cmd) "/usr/bin/found")))
    (should (eq (claude-repl--select-notification-backend)
                #'claude-repl--notify-backend-terminal-notifier))))

(ert-deftest claude-repl-test-select-backend-uses-osascript-when-only-osascript ()
  "When only osascript is available, select osascript."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd) (when (equal cmd "osascript") "/usr/bin/osascript"))))
    (should (eq (claude-repl--select-notification-backend)
                #'claude-repl--notify-backend-osascript))))

(ert-deftest claude-repl-test-select-backend-uses-terminal-notifier-when-only-terminal-notifier ()
  "When only terminal-notifier is available, select terminal-notifier."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd) (when (equal cmd "terminal-notifier") "/usr/local/bin/terminal-notifier"))))
    (should (eq (claude-repl--select-notification-backend)
                #'claude-repl--notify-backend-terminal-notifier))))

(ert-deftest claude-repl-test-select-backend-falls-back-to-osascript ()
  "When terminal-notifier is NOT available but osascript is, fall back to osascript."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd) (when (equal cmd "osascript") "/usr/bin/osascript"))))
    (should (eq (claude-repl--select-notification-backend)
                #'claude-repl--notify-backend-osascript))))

(ert-deftest claude-repl-test-select-backend-errors-when-none-available ()
  "When neither terminal-notifier nor osascript is available, signal an error."
  (cl-letf (((symbol-function 'executable-find) (lambda (_cmd) nil)))
    (should-error (claude-repl--select-notification-backend) :type 'error)))

;;;; ---- Tests: notification backend variable ----

(ert-deftest claude-repl-test-notification-backend-is-bound ()
  "The notification backend variable should be set at load time."
  (should (boundp 'claude-repl--notification-backend))
  (should (functionp claude-repl--notification-backend)))

;;;; ---- Tests: terminal-notifier backend ----

(ert-deftest claude-repl-test-terminal-notifier-spawns-with-args ()
  "terminal-notifier backend should spawn with -title/-message/-sound and a click -execute."
  (let (captured)
    (cl-letf (((symbol-function 'claude-repl--notify-spawn)
               (lambda (&rest args) (setq captured args) nil))
              ((symbol-function 'claude-repl--ensure-server) #'ignore))
      (claude-repl--notify-backend-terminal-notifier "ws-a" "My Title" "My Message")
      (should (equal captured
                     (list "ws-a" 'terminal-notifier "terminal-notifier"
                           "-title" "My Title"
                           "-message" "My Message"
                           "-sound" claude-repl-notification-sound
                           "-execute" (claude-repl--notification-click-command "ws-a")))))))

(ert-deftest claude-repl-test-terminal-notifier-nil-ws-omits-execute ()
  "With a nil WS the terminal-notifier backend must not add a -execute action."
  (let (captured)
    (cl-letf (((symbol-function 'claude-repl--notify-spawn)
               (lambda (&rest args) (setq captured args) nil))
              ((symbol-function 'claude-repl--ensure-server)
               (lambda () (error "ensure-server must not run for a non-clickable notification"))))
      (claude-repl--notify-backend-terminal-notifier nil "T" "M")
      (should-not (member "-execute" captured)))))

(ert-deftest claude-repl-test-terminal-notifier-ensures-server-when-clickable ()
  "The terminal-notifier backend must ensure the Emacs server for a clickable WS."
  (let (ensured)
    (cl-letf (((symbol-function 'claude-repl--notify-spawn) (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--ensure-server)
               (lambda () (setq ensured t))))
      (claude-repl--notify-backend-terminal-notifier "ws-a" "T" "M")
      (should ensured))))

(ert-deftest claude-repl-test-terminal-notifier-empty-strings ()
  "terminal-notifier backend should handle empty title and message."
  (let (captured)
    (cl-letf (((symbol-function 'claude-repl--notify-spawn)
               (lambda (&rest args) (setq captured args) nil)))
      (claude-repl--notify-backend-terminal-notifier "ws-a" "" "")
      ;; positions: ws backend program -title <title> -message <message>
      (should (equal (nth 4 captured) ""))
      (should (equal (nth 6 captured) "")))))

(ert-deftest claude-repl-test-terminal-notifier-special-chars ()
  "terminal-notifier backend should pass through special characters."
  (let (captured)
    (cl-letf (((symbol-function 'claude-repl--notify-spawn)
               (lambda (&rest args) (setq captured args) nil)))
      (claude-repl--notify-backend-terminal-notifier "ws-a" "Title \"quoted\"" "Message with\nnewline")
      (should (equal (nth 4 captured) "Title \"quoted\""))
      (should (equal (nth 6 captured) "Message with\nnewline")))))

;;;; ---- Tests: osascript backend ----

(ert-deftest claude-repl-test-osascript-spawns-with-script ()
  "osascript backend should spawn osascript with a -e display-notification script."
  (let (captured)
    (cl-letf (((symbol-function 'claude-repl--notify-spawn)
               (lambda (&rest args) (setq captured args) nil)))
      (claude-repl--notify-backend-osascript "ws-a" "My Title" "My Message")
      ;; positions: ws backend program -e <script>
      (should (equal (nth 0 captured) "ws-a"))
      (should (eq (nth 1 captured) 'osascript))
      (should (equal (nth 2 captured) "osascript"))
      (should (equal (nth 3 captured) "-e"))
      (let ((script (nth 4 captured)))
        (should (string-match-p "display notification" script))
        (should (string-match-p "My Message" script))
        (should (string-match-p "My Title" script))
        (should (string-match-p "sound name" script))))))

(ert-deftest claude-repl-test-osascript-empty-strings ()
  "osascript backend should handle empty title and message."
  (let (captured)
    (cl-letf (((symbol-function 'claude-repl--notify-spawn)
               (lambda (&rest args) (setq captured args) nil)))
      (claude-repl--notify-backend-osascript "ws-a" "" "")
      (let ((script (nth 4 captured)))
        (should (string-match-p "display notification" script))))))

(ert-deftest claude-repl-test-osascript-special-chars ()
  "osascript backend should properly quote special characters in the script."
  (let (captured)
    (cl-letf (((symbol-function 'claude-repl--notify-spawn)
               (lambda (&rest args) (setq captured args) nil)))
      (claude-repl--notify-backend-osascript "ws-a" "Title \"quoted\"" "Line1\nLine2")
      (let ((script (nth 4 captured)))
        (should (stringp script))
        (should (string-match-p "display notification" script))))))

;;;; ---- Tests: claude-repl--notify-spawn ----

(ert-deftest claude-repl-test-notify-spawn-captures-output-into-buffer ()
  "spawn should pass a live capture buffer (not nil) to start-process."
  (let (captured-buffer)
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name buffer &rest _args)
                 (setq captured-buffer buffer)
                 ;; Return a non-process so no real sentinel is attached.
                 nil)))
      (claude-repl--notify-spawn "ws-a" 'osascript "osascript" "-e" "noop")
      (should (bufferp captured-buffer)))))

(ert-deftest claude-repl-test-notify-spawn-sets-sentinel-on-process ()
  "spawn should attach a sentinel to the spawned process."
  (let (sentinel-set)
    (cl-letf (((symbol-function 'start-process)
               (lambda (&rest _args) 'fake-proc))
              ((symbol-function 'processp) (lambda (obj) (eq obj 'fake-proc)))
              ((symbol-function 'set-process-sentinel)
               (lambda (proc fn)
                 (setq sentinel-set (and (eq proc 'fake-proc) (functionp fn))))))
      (claude-repl--notify-spawn "ws-a" 'osascript "osascript" "-e" "noop")
      (should sentinel-set))))

(ert-deftest claude-repl-test-notify-spawn-non-process-kills-buffer ()
  "When start-process returns a non-process, the capture buffer must be killed."
  (let (captured-buffer)
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name buffer &rest _args)
                 (setq captured-buffer buffer)
                 nil)))
      (should (null (claude-repl--notify-spawn "ws-a" 'osascript "osascript" "-e" "noop")))
      (should-not (buffer-live-p captured-buffer)))))

(ert-deftest claude-repl-test-notify-spawn-returns-process ()
  "spawn should return the spawned process when start-process yields one."
  (cl-letf (((symbol-function 'start-process)
             (lambda (&rest _args) 'fake-proc))
            ((symbol-function 'processp) (lambda (obj) (eq obj 'fake-proc)))
            ((symbol-function 'set-process-sentinel) (lambda (&rest _) nil)))
    (should (eq (claude-repl--notify-spawn "ws-a" 'osascript "osascript" "-e" "noop")
                'fake-proc))))

;;;; ---- Tests: claude-repl--notify-process-sentinel ----

(ert-deftest claude-repl-test-sentinel-logs-ok-on-zero-exit ()
  "Sentinel should log a gated `ok' line when the command exits zero."
  (let ((buffer (generate-new-buffer " *sentinel-ok*"))
        log-args)
    (unwind-protect
        (cl-letf (((symbol-function 'process-status) (lambda (_p) 'exit))
                  ((symbol-function 'process-exit-status) (lambda (_p) 0))
                  ((symbol-function 'claude-repl--log)
                   (lambda (_ws fmt &rest args) (setq log-args (cons fmt args))))
                  ((symbol-function 'claude-repl--do-log)
                   (lambda (&rest _) (error "do-log must not fire on success"))))
          (funcall (claude-repl--notify-process-sentinel "ws-a" 'osascript buffer)
                   'proc "finished\n")
          (should (string-match-p "notify-backend=%s ok" (car log-args)))
          (should (equal (cdr log-args) '(osascript))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest claude-repl-test-sentinel-logs-failure-on-nonzero-exit ()
  "Sentinel should loudly log via do-log when the command exits non-zero."
  (let ((buffer (generate-new-buffer " *sentinel-fail*"))
        do-log-args)
    (unwind-protect
        (progn
          (with-current-buffer buffer (insert "boom: not allowed"))
          (cl-letf (((symbol-function 'process-status) (lambda (_p) 'exit))
                    ((symbol-function 'process-exit-status) (lambda (_p) 1))
                    ((symbol-function 'claude-repl--log)
                     (lambda (&rest _) (error "log must not fire on failure")))
                    ((symbol-function 'claude-repl--do-log)
                     (lambda (_ws fmt args &optional _err) (setq do-log-args (cons fmt args)))))
            (funcall (claude-repl--notify-process-sentinel "ws-a" 'osascript buffer)
                     'proc "exited abnormally with code 1\n")
            (should (string-match-p "FAILED" (car do-log-args)))
            ;; args list: (backend status event output)
            (should (eq (nth 0 (cdr do-log-args)) 'osascript))
            (should (equal (nth 1 (cdr do-log-args)) 1))
            (should (string-match-p "boom: not allowed" (nth 3 (cdr do-log-args))))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest claude-repl-test-sentinel-kills-buffer-on-termination ()
  "Sentinel should kill the capture buffer once the process terminates."
  (let ((buffer (generate-new-buffer " *sentinel-kill*")))
    (cl-letf (((symbol-function 'process-status) (lambda (_p) 'exit))
              ((symbol-function 'process-exit-status) (lambda (_p) 0))
              ((symbol-function 'claude-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--do-log) (lambda (&rest _) nil)))
      (funcall (claude-repl--notify-process-sentinel "ws-a" 'osascript buffer)
               'proc "finished\n")
      (should-not (buffer-live-p buffer)))))

(ert-deftest claude-repl-test-sentinel-ignores-non-terminal-events ()
  "Sentinel should do nothing for non-terminal process states (e.g. `run')."
  (let ((buffer (generate-new-buffer " *sentinel-run*")))
    (unwind-protect
        (cl-letf (((symbol-function 'process-status) (lambda (_p) 'run))
                  ((symbol-function 'claude-repl--log)
                   (lambda (&rest _) (error "log must not fire while running")))
                  ((symbol-function 'claude-repl--do-log)
                   (lambda (&rest _) (error "do-log must not fire while running"))))
          (funcall (claude-repl--notify-process-sentinel "ws-a" 'osascript buffer)
                   'proc "run\n")
          ;; Buffer stays live because the process has not terminated.
          (should (buffer-live-p buffer)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

;;;; ---- Tests: claude-repl--notify dispatch ----

(ert-deftest claude-repl-test-notify-dispatches-to-backend ()
  "claude-repl--notify should funcall the selected backend with ws/title/message."
  (let (called-with)
    (cl-letf ((claude-repl--notification-backend
               (lambda (ws title msg) (setq called-with (list ws title msg))))
              (claude-repl-debug nil)
              ((symbol-function 'claude-repl--log) (lambda (_ws _fmt &rest _args) nil)))
      (claude-repl--notify "ws-a" "Test Title" "Test Message")
      (should (equal called-with '("ws-a" "Test Title" "Test Message"))))))

(ert-deftest claude-repl-test-notify-logs-when-debug-enabled ()
  "claude-repl--notify should log when debug is enabled."
  (let (log-called)
    (cl-letf ((claude-repl--notification-backend (lambda (_ws _t _m) nil))
              (claude-repl-debug t)
              ((symbol-function 'claude-repl--log)
               (lambda (_ws _fmt &rest _args) (setq log-called t))))
      (claude-repl--notify nil "Title" "Message")
      (should log-called))))

(ert-deftest claude-repl-test-notify-logs-before-sending ()
  "claude-repl--notify should call log before dispatching to backend."
  (let (call-order)
    (cl-letf ((claude-repl--notification-backend
               (lambda (_ws _t _m) (push 'backend call-order)))
              (claude-repl-debug t)
              ((symbol-function 'claude-repl--log)
               (lambda (_ws _fmt &rest _args) (push 'log call-order))))
      (claude-repl--notify nil "Title" "Msg")
      ;; call-order is reversed because we push
      (should (equal call-order '(backend log))))))

(ert-deftest claude-repl-test-notify-does-not-log-when-debug-off ()
  "claude-repl--notify should not log when debug is nil."
  (cl-letf ((claude-repl--notification-backend (lambda (_ws _t _m) nil))
            (claude-repl-debug nil))
    ;; With debug nil, the real log function is a no-op, so we just
    ;; verify no error is raised.
    (claude-repl--notify nil "Title" "Message")))

;;;; ---- Tests: uncovered edge cases ----

(ert-deftest claude-repl-test-terminal-notifier-very-long-strings ()
  "terminal-notifier backend should handle very long title and message strings."
  (let* (captured
         (long-title (make-string 10000 ?T))
         (long-message (make-string 10000 ?M)))
    (cl-letf (((symbol-function 'claude-repl--notify-spawn)
               (lambda (&rest args) (setq captured args) nil)))
      (claude-repl--notify-backend-terminal-notifier "ws-a" long-title long-message)
      (should (equal (nth 4 captured) long-title))
      (should (equal (nth 6 captured) long-message)))))

(ert-deftest claude-repl-test-terminal-notifier-unicode-chars ()
  "terminal-notifier backend should pass through non-ASCII/Unicode characters."
  (let (captured)
    (cl-letf (((symbol-function 'claude-repl--notify-spawn)
               (lambda (&rest args) (setq captured args) nil)))
      (claude-repl--notify-backend-terminal-notifier "ws-a" "Héllo Wörld 你好" "Émoji 🎉 café")
      (should (equal (nth 4 captured) "Héllo Wörld 你好"))
      (should (equal (nth 6 captured) "Émoji 🎉 café")))))

(ert-deftest claude-repl-test-osascript-unicode-chars ()
  "osascript backend should include non-ASCII/Unicode characters in the script."
  (let (captured)
    (cl-letf (((symbol-function 'claude-repl--notify-spawn)
               (lambda (&rest args) (setq captured args) nil)))
      (claude-repl--notify-backend-osascript "ws-a" "Héllo 你好" "café 🎉")
      (let ((script (nth 4 captured)))
        (should (string-match-p "display notification" script))
        (should (string-match-p "café" script))
        (should (string-match-p "Héllo" script))))))

(ert-deftest claude-repl-test-osascript-start-process-returns-nil ()
  "osascript backend should not error when start-process returns nil."
  (cl-letf (((symbol-function 'start-process)
             (lambda (&rest _args) nil)))
    ;; Should complete without signaling an error (buffer cleanup path).
    (claude-repl--notify-backend-osascript "ws-a" "Title" "Message")))

(ert-deftest claude-repl-test-notify-backend-signals-error ()
  "claude-repl--notify should propagate errors from the backend function."
  (cl-letf ((claude-repl--notification-backend
             (lambda (_ws _title _msg) (error "Backend failed")))
            (claude-repl-debug nil)
            ((symbol-function 'claude-repl--log) (lambda (_ws _fmt &rest _args) nil)))
    (should-error (claude-repl--notify nil "Title" "Message")
                  :type 'error)))

;;;; ---- Tests: claude-repl--notification-click-command ----

(ert-deftest claude-repl-test-click-command-nil-ws-returns-nil ()
  "A nil WS produces no click command (notification stays non-clickable)."
  (should (null (claude-repl--notification-click-command nil))))

(ert-deftest claude-repl-test-click-command-empty-ws-returns-nil ()
  "An empty-string WS produces no click command."
  (should (null (claude-repl--notification-click-command ""))))

(ert-deftest claude-repl-test-click-command-invokes-emacsclient-eval ()
  "The click command should run emacsclient --eval on the configured binary."
  (let ((claude-repl-emacsclient-executable "/opt/emacs/bin/emacsclient"))
    (let ((cmd (claude-repl--notification-click-command "ws-a")))
      (should (string-match-p "/opt/emacs/bin/emacsclient" cmd))
      (should (string-match-p "--eval" cmd)))))

(ert-deftest claude-repl-test-click-command-embeds-activate-form-with-ws ()
  "The click command should eval `claude-repl--notification-activate' with WS."
  (let ((cmd (claude-repl--notification-click-command "DWC/foo")))
    (should (string-match-p "claude-repl--notification-activate" cmd))
    (should (string-match-p "DWC/foo" cmd))))

(ert-deftest claude-repl-test-click-command-shell-quotes-executable-with-spaces ()
  "An emacsclient path with spaces must be shell-quoted in the click command."
  (let ((claude-repl-emacsclient-executable "/Applications/Emacs.app/a b/emacsclient"))
    (let ((cmd (claude-repl--notification-click-command "ws-a")))
      ;; The raw space-containing path must be wrapped so the shell sees one arg.
      (should (string-match-p (regexp-quote (shell-quote-argument
                                             claude-repl-emacsclient-executable))
                              cmd)))))

;;;; ---- Tests: claude-repl--notification-activate ----

(ert-deftest claude-repl-test-activate-jumps-to-workspace ()
  "Activate should jump to the given workspace."
  (let (jumped)
    (cl-letf (((symbol-function 'claude-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'claude-repl-jump-to-workspace)
               (lambda (ws &optional _no-flash) (setq jumped ws)))
              ((symbol-function 'select-frame-set-input-focus) (lambda (&rest _) nil))
              ((symbol-function 'selected-frame) (lambda () 'frame)))
      (claude-repl--notification-activate "ws-a")
      (should (equal jumped "ws-a")))))

(ert-deftest claude-repl-test-activate-focuses-selected-frame ()
  "Activate should focus the selected frame so Emacs comes forward."
  (let (focused)
    (cl-letf (((symbol-function 'claude-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'claude-repl-jump-to-workspace) (lambda (&rest _) nil))
              ((symbol-function 'selected-frame) (lambda () 'the-frame))
              ((symbol-function 'select-frame-set-input-focus)
               (lambda (frame) (setq focused frame))))
      (claude-repl--notification-activate "ws-a")
      (should (eq focused 'the-frame)))))

(ert-deftest claude-repl-test-activate-nil-ws-focuses-without-jump ()
  "Activate with a nil WS should focus Emacs but not attempt a jump."
  (let ((jumped nil) (focused nil))
    (cl-letf (((symbol-function 'claude-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'claude-repl-jump-to-workspace)
               (lambda (&rest _) (setq jumped t)))
              ((symbol-function 'selected-frame) (lambda () 'frame))
              ((symbol-function 'select-frame-set-input-focus)
               (lambda (&rest _) (setq focused t))))
      (claude-repl--notification-activate nil)
      (should-not jumped)
      (should focused))))

;;;; ---- Tests: claude-repl--ensure-server ----

(ert-deftest claude-repl-test-ensure-server-starts-when-not-running ()
  "ensure-server should start the server when none is running."
  (let (started)
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'server-running-p) (lambda () nil))
              ((symbol-function 'server-start) (lambda (&rest _) (setq started t))))
      (let ((noninteractive nil))
        (claude-repl--ensure-server))
      (should started))))

(ert-deftest claude-repl-test-ensure-server-noop-when-already-running ()
  "ensure-server should not start a second server when one already runs."
  (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
            ((symbol-function 'server-running-p) (lambda () t))
            ((symbol-function 'server-start)
             (lambda (&rest _) (error "server-start must not run when already running"))))
    (let ((noninteractive nil))
      (claude-repl--ensure-server))))

(ert-deftest claude-repl-test-ensure-server-noop-in-batch ()
  "ensure-server should be a no-op under `noninteractive' (batch/ERT)."
  (cl-letf (((symbol-function 'require)
             (lambda (&rest _) (error "require must not run in batch")))
            ((symbol-function 'server-start)
             (lambda (&rest _) (error "server-start must not run in batch"))))
    (let ((noninteractive t))
      (claude-repl--ensure-server))))

(provide 'test-notifications)

;;; test-notifications.el ends here
