;;; test-core.el --- ERT tests for claude-repl core.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs -batch -Q -l ert -l test-core.el -f ert-run-tests-batch-and-exit
;;
;; Or interactively:
;;   M-x load-file RET test-core.el RET
;;   M-x ert RET t RET

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: Workspace ID / root resolution ----

(ert-deftest claude-repl-test-workspace-id-from-project-root ()
  "Workspace ID should be first 8 chars of MD5 of the canonical ws-dir path."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
            ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/test/project")))
    (should (equal (claude-repl--workspace-id)
                   (substring (md5 (claude-repl--path-canonical "/test/project")) 0 8)))))

(ert-deftest claude-repl-test-workspace-id-fallback-to-main-git-root ()
  "Workspace ID falls back to main-git-root when ws-dir errors."
  (let ((claude-repl--main-git-root "/fallback/dir/"))
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'claude-repl--ws-dir)
               (lambda (_ws) (error "no dir"))))
      (should (equal (claude-repl--workspace-id)
                     (substring (md5 (claude-repl--path-canonical "/fallback/dir/")) 0 8))))))

;;;; ---- Tests: resolve-current-git-root ----

(ert-deftest claude-repl-test-resolve-current-git-root-prefers-ws-dir ()
  "When the current workspace has a :project-dir, the resolver uses it as
the directory to run `git rev-parse --show-toplevel' from (not
`default-directory')."
  (let ((captured-default-dir nil))
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'claude-repl--ws-dir)
               (lambda (_ws) "/repo/subdir"))
              ((symbol-function 'claude-repl--git-string-quiet)
               (lambda (&rest _)
                 (setq captured-default-dir default-directory)
                 "/repo")))
      (let ((default-directory "/elsewhere/"))
        (should (equal (claude-repl--resolve-current-git-root) "/repo/"))
        ;; git was invoked from the ws-dir, not default-directory
        (should (equal captured-default-dir "/repo/subdir"))))))

(ert-deftest claude-repl-test-resolve-current-git-root-falls-back-to-default-directory ()
  "When no workspace has a :project-dir, the resolver runs git from
`default-directory'."
  (let ((captured-default-dir nil))
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'claude-repl--ws-dir)
               (lambda (_ws) (error "no dir")))
              ((symbol-function 'claude-repl--git-string-quiet)
               (lambda (&rest _)
                 (setq captured-default-dir default-directory)
                 "/fallback/repo")))
      (let ((default-directory "/fallback/repo/deep/"))
        (should (equal (claude-repl--resolve-current-git-root) "/fallback/repo/"))
        (should (equal captured-default-dir "/fallback/repo/deep/"))))))

(ert-deftest claude-repl-test-resolve-current-git-root-errors-outside-repo ()
  "When `git rev-parse' returns empty (not inside any repo), the resolver
signals `user-error' rather than silently returning a bogus path."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
            ((symbol-function 'claude-repl--ws-dir)
             (lambda (_ws) (error "no dir")))
            ((symbol-function 'claude-repl--git-string-quiet)
             (lambda (&rest _) "")))
    (should-error (claude-repl--resolve-current-git-root) :type 'user-error)))


;;;; ---- Tests: Buffer naming ----

(ert-deftest claude-repl-test-buffer-name-format ()
  "Buffer names should follow *claude-panel-WS* and *claude-panel-input-WS* pattern."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws")))
    (should (equal (claude-repl--buffer-name) "*claude-panel-my-ws*"))
    (should (equal (claude-repl--buffer-name "-input") "*claude-panel-input-my-ws*"))))

(ert-deftest claude-repl-test-buffer-name-default ()
  "Buffer name signals an error when no workspace name is available."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
    (should-error (claude-repl--buffer-name) :type 'error)))

(ert-deftest claude-repl-test-buffer-name-uses-explicit-ws ()
  "Buffer name should prefer the explicit WS argument over the current workspace."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws")))
    (should (equal (claude-repl--buffer-name nil "other-ws")
                   "*claude-panel-other-ws*"))))

(ert-deftest claude-repl-test-buffer-name-sanitizes-unsafe-chars ()
  "Workspace names with unsafe characters should be sanitized to underscores."
  (should (equal (claude-repl--buffer-name nil "feat/login")
                 "*claude-panel-feat_login*"))
  (should (equal (claude-repl--buffer-name nil "ws with space")
                 "*claude-panel-ws_with_space*"))
  (should (equal (claude-repl--buffer-name "-input" "a*b")
                 "*claude-panel-input-a_b*")))

(ert-deftest claude-repl-test-sanitize-ws-name ()
  "sanitize-ws-name keeps alphanumerics, hyphens, and underscores."
  (should (equal (claude-repl--sanitize-ws-name "abc-123_xyz") "abc-123_xyz"))
  (should (equal (claude-repl--sanitize-ws-name "feat/login") "feat_login"))
  (should (equal (claude-repl--sanitize-ws-name "a b*c") "a_b_c"))
  (should-not (claude-repl--sanitize-ws-name nil)))

;;;; ---- Tests: Buffer predicates ----

(ert-deftest claude-repl-test-claude-buffer-p ()
  "claude-buffer-p should match *claude-panel-WS* pattern only (excluding input)."
  (claude-repl-test--with-temp-buffer "*claude-panel-abcd1234*"
    (should (claude-repl--claude-buffer-p)))
  (claude-repl-test--with-temp-buffer "*claude-panel-input-abcd1234*"
    (should-not (claude-repl--claude-buffer-p)))
  ;; Use a name that does NOT begin with `*claude-panel-' so the vterm regex
  ;; can't match.  The real `*scratch*' is the current buffer when the
  ;; aggregate test runner starts, so naming a temp buffer `*scratch*' and
  ;; then killing it swaps us out of the original buffer and leaves
  ;; `default-directory' pointing at whatever buffer ert lands in next
  ;; (Emacs.app/Contents/MacOS on macOS).  That breaks subsequent tests
  ;; that call git without a -C flag.
  (claude-repl-test--with-temp-buffer "*repl-test-non-claude-buf*"
    (should-not (claude-repl--claude-buffer-p))))

;;;; ---- Tests: Logging ----

(ert-deftest claude-repl-test-log-respects-debug-flag ()
  "When `claude-repl-debug' is nil, `claude-repl--log' should NOT call `message'.
When t, it should call `message'."
  (let ((message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) (setq message-called t))))
      ;; debug off: no message
      (let ((claude-repl-debug nil))
        (claude-repl--log nil "test %s" "hello")
        (should-not message-called))
      ;; debug on: message called
      (let ((claude-repl-debug t))
        (setq message-called nil)
        (claude-repl--log nil "test %s" "hello")
        (should message-called)))))

;;;; ---- Tests: vterm buffer predicates ----

(ert-deftest claude-repl-test-vterm-live-p-nil ()
  "Returns nil when no vterm buffer is stored for the workspace."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (should-not (claude-repl--vterm-live-p)))))

(ert-deftest claude-repl-test-vterm-live-p-dead ()
  "Returns nil for a killed buffer."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (let ((buf (get-buffer-create " *test-dead-buf*")))
        (claude-repl--ws-put "ws1" :vterm-buffer buf)
        (kill-buffer buf)
        (should-not (claude-repl--vterm-live-p))))))

(ert-deftest claude-repl-test-vterm-live-p-live ()
  "Returns non-nil for a live buffer."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-live-buf*"
      (claude-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
        (should (claude-repl--vterm-live-p))))))

(ert-deftest claude-repl-test-vterm-running-p-no-process ()
  "Returns nil when buffer is live but has no process."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-no-proc*"
      (claude-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
        (should-not (claude-repl--vterm-running-p))))))

(ert-deftest claude-repl-test-vterm-running-p-with-process ()
  "Returns non-nil when buffer has a live process."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-with-proc*"
      (claude-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'get-buffer-process)
                 (lambda (_buf) 'fake-process)))
        (should (claude-repl--vterm-running-p))))))

;;;; ---- Tests: Deferred macro ----

(ert-deftest claude-repl-test-deferred-macro ()
  "The deferred macro should create a debouncing lambda."
  (let ((claude-repl--sync-timer nil)
        (call-count 0))
    (let ((debounced (claude-repl--deferred claude-repl--sync-timer
                       (lambda () (cl-incf call-count)))))
      ;; Calling it should set the timer var
      (funcall debounced)
      (should claude-repl--sync-timer)
      ;; Cancel it to prevent side effects
      (cancel-timer claude-repl--sync-timer)
      (setq claude-repl--sync-timer nil))))

;;;; ---- Bug regression tests ----

(ert-deftest claude-repl-test-bug1-cursor-reset-timer-defvar ()
  "Bug 1: claude-repl--cursor-reset-timer should be defined (not void)."
  (should (boundp 'claude-repl--cursor-reset-timer)))

(ert-deftest claude-repl-test-bug9-paste-delay-configurable ()
  "Bug 9: claude-repl-paste-delay should be a configurable variable."
  (should (boundp 'claude-repl-paste-delay))
  (should (numberp claude-repl-paste-delay)))

(ert-deftest claude-repl-test-bug10-defvar-declarations ()
  "Bug 10: All key variables should be properly declared.
Note: claude-repl--notification-backend may not be bound in headless
environments without notification tools (terminal-notifier or osascript)."
  (should (boundp 'claude-repl--workspaces))
  (should (boundp 'claude-repl-hide-input-box))
  ;; notification-backend requires osascript or terminal-notifier at load time
  (when (or (executable-find "terminal-notifier") (executable-find "osascript"))
    (should (boundp 'claude-repl--notification-backend)))
  (should (boundp 'claude-repl--sync-timer)))

(ert-deftest claude-repl-test-bug11-fullscreen-config-stored-in-plist ()
  "Bug 11: fullscreen-config is per-workspace plist storage, not a global defvar."
  ;; The original test checked (boundp 'claude-repl--fullscreen-config), but that
  ;; symbol is never defvar'd.  The real storage is the :fullscreen-config plist
  ;; key accessed via ws-get/ws-put in panels.el.
  (claude-repl-test--with-clean-state
    (let ((ws-id "test-fc"))
      (puthash ws-id (list :status nil) claude-repl--workspaces)
      (claude-repl--ws-put ws-id :fullscreen-config 'some-config)
      (should (equal (claude-repl--ws-get ws-id :fullscreen-config) 'some-config)))))

(ert-deftest claude-repl-test-package-provide ()
  "Package should provide 'claude-repl feature."
  (should (featurep 'claude-repl)))

;;;; ---- Tests: Workspace-for-buffer ----

(ert-deftest claude-repl-test-workspace-for-buffer-no-persp ()
  "When `persp-mode' is nil, `workspace-for-buffer' should return nil."
  (let ((persp-mode nil))
    (should-not (claude-repl--workspace-for-buffer (current-buffer)))))

;;;; ---- Tests: Misc declared variables ----

(ert-deftest claude-repl-test-in-redraw-advice-declared-core ()
  "`claude-repl--in-redraw-advice' should be `boundp' (loaded via overlay.el)."
  ;; Renamed from claude-repl-test-in-redraw-advice-declared to avoid duplicate
  ;; with the canonical copy in test-overlay.el.
  (should (boundp 'claude-repl--in-redraw-advice)))

;;;; ---- Tests: cancel-all-timers ----

(ert-deftest claude-repl-test-cancel-all-timers-empty-list ()
  "Cancelling timers with an empty list should be a no-op."
  (let ((claude-repl--timers nil))
    (claude-repl--cancel-all-timers)
    (should (null claude-repl--timers))))

(ert-deftest claude-repl-test-cancel-all-timers-mix-valid-and-nil ()
  "Cancelling timers with mix of valid timers and nil entries should not error."
  (let* ((timer1 (run-with-timer 9999 nil #'ignore))
         (claude-repl--timers (list timer1 nil nil)))
    (claude-repl--cancel-all-timers)
    (should (null claude-repl--timers))))

(ert-deftest claude-repl-test-cancel-all-timers-already-cancelled ()
  "Cancelling already-cancelled timers should not error."
  (let* ((timer1 (run-with-timer 9999 nil #'ignore)))
    (cancel-timer timer1)
    (let ((claude-repl--timers (list timer1)))
      (claude-repl--cancel-all-timers)
      (should (null claude-repl--timers)))))

(ert-deftest claude-repl-test-cancel-all-timers-sets-nil ()
  "After cancellation, `claude-repl--timers' should be nil."
  (let* ((timer1 (run-with-timer 9999 nil #'ignore))
         (timer2 (run-with-timer 9999 nil #'ignore))
         (claude-repl--timers (list timer1 timer2)))
    (claude-repl--cancel-all-timers)
    (should (null claude-repl--timers))))

(ert-deftest claude-repl-test-cancel-all-timers-idempotent ()
  "Calling cancel-all-timers twice should be safe."
  (let* ((timer1 (run-with-timer 9999 nil #'ignore))
         (claude-repl--timers (list timer1)))
    (claude-repl--cancel-all-timers)
    (should (null claude-repl--timers))
    (claude-repl--cancel-all-timers)
    (should (null claude-repl--timers))))

;;;; ---- Tests: log-format ----

(ert-deftest claude-repl-test-log-format-empty-string ()
  "log-format with empty string should still include timestamp and tag."
  (let ((result (claude-repl--log-format nil "")))
    (should (string-match-p "\\[claude-repl\\]" result))
    (should (string-match-p "^[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\." result))))

(ert-deftest claude-repl-test-log-format-with-format-specifiers ()
  "log-format should pass format specifiers through literally, not expand them."
  (let ((result (claude-repl--log-format nil "hello %s %d")))
    (should (string-match-p "%s" result))
    (should (string-match-p "%d" result))))

(ert-deftest claude-repl-test-log-format-contains-timestamp-and-tag ()
  "log-format output should contain timestamp and [claude-repl] tag."
  (let ((result (claude-repl--log-format nil "test message")))
    (should (string-match-p "^[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\.[0-9]+" result))
    (should (string-match-p "\\[claude-repl\\]" result))
    (should (string-match-p "test message$" result))))

;;;; ---- Tests: do-log ----

(ert-deftest claude-repl-test-do-log-fmt-no-args ()
  "do-log with fmt and no args should call message with formatted prefix."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (claude-repl--do-log nil "simple message" nil)
      (should (string-match-p "\\[claude-repl\\] simple message" captured-msg)))))

(ert-deftest claude-repl-test-do-log-fmt-multiple-args ()
  "do-log with fmt and multiple args should expand them."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (claude-repl--do-log nil "hello %s and %d" '("world" 42))
      (should (string-match-p "hello world and 42" captured-msg)))))

(ert-deftest claude-repl-test-do-log-nil-args ()
  "do-log with nil args should work like no args."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (claude-repl--do-log nil "no args here" nil)
      (should (string-match-p "no args here" captured-msg)))))

(ert-deftest claude-repl-test-do-log-message-has-prefix ()
  "do-log should emit message with timestamp prefix."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (claude-repl--do-log nil "test" nil)
      (should (string-match-p "^[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\." captured-msg)))))

;;;; ---- Tests: log ----

(ert-deftest claude-repl-test-log-verbose-symbol-still-logs ()
  "`claude-repl--log' should log when debug is set to 'verbose (non-nil)."
  (let ((message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) (setq message-called t))))
      (let ((claude-repl-debug 'verbose))
        (claude-repl--log nil "test %s" "verbose")
        (should message-called)))))

(ert-deftest claude-repl-test-log-multiple-format-args ()
  "`claude-repl--log' should correctly expand multiple format arguments."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (let ((claude-repl-debug t))
        (claude-repl--log nil "a=%s b=%d c=%s" "x" 1 "z")
        (should (string-match-p "a=x b=1 c=z" captured-msg))))))

(ert-deftest claude-repl-test-log-bare-string ()
  "`claude-repl--log' with bare string (no format args) should work."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (let ((claude-repl-debug t))
        (claude-repl--log nil "bare message")
        (should (string-match-p "bare message" captured-msg))))))

(ert-deftest claude-repl-test-log-includes-timestamp ()
  "`claude-repl--log' output should include timestamp prefix."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (let ((claude-repl-debug t))
        (claude-repl--log nil "ts check")
        (should (string-match-p "^[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\." captured-msg))))))

;;;; ---- Tests: log-verbose ----

(ert-deftest claude-repl-test-log-verbose-nil-no-log ()
  "`claude-repl--log-verbose' should NOT log when debug is nil."
  (let ((message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) (setq message-called t))))
      (let ((claude-repl-debug nil))
        (claude-repl--log-verbose nil "test")
        (should-not message-called)))))

(ert-deftest claude-repl-test-log-verbose-t-no-log ()
  "`claude-repl--log-verbose' should NOT log when debug is t (only logs for 'verbose)."
  (let ((message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) (setq message-called t))))
      (let ((claude-repl-debug t))
        (claude-repl--log-verbose nil "test")
        (should-not message-called)))))

(ert-deftest claude-repl-test-log-verbose-verbose-logs ()
  "`claude-repl--log-verbose' should log when debug is 'verbose."
  (let ((message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) (setq message-called t))))
      (let ((claude-repl-debug 'verbose))
        (claude-repl--log-verbose nil "test")
        (should message-called)))))

(ert-deftest claude-repl-test-log-verbose-includes-timestamp ()
  "`claude-repl--log-verbose' output should include timestamp prefix."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (let ((claude-repl-debug 'verbose))
        (claude-repl--log-verbose nil "ts check")
        (should (string-match-p "^[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\." captured-msg))))))

;;;; ---- Tests: log-to-file ----

(ert-deftest claude-repl-test-logfile-path-returns-fixed-path ()
  "`claude-repl--logfile-path' should return ~/.claude/doom-claude-repl.log."
  (should (equal (claude-repl--logfile-path)
                 (expand-file-name "~/.claude/doom-claude-repl.log"))))

(ert-deftest claude-repl-test-logfile-path-honors-defcustom ()
  "`claude-repl--logfile-path' should expand `claude-repl-log-file-name'."
  (let* ((tmpdir (make-temp-file "test-logpath-" t))
         (custom-path (expand-file-name "sub/custom.log" tmpdir))
         (claude-repl-log-file-name custom-path))
    (unwind-protect
        (should (equal (claude-repl--logfile-path) custom-path))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-do-log-to-file-writes-when-enabled ()
  "`claude-repl--do-log-to-file' should append text to the logfile."
  (let* ((tmpdir (make-temp-file "test-log-" t))
         (logpath (expand-file-name ".claude-repl.log" tmpdir))
         (claude-repl-log-to-file t))
    (unwind-protect
        (cl-letf (((symbol-function 'claude-repl--logfile-path) (lambda () logpath)))
          (claude-repl--do-log-to-file "first line")
          (claude-repl--do-log-to-file "second line")
          (let ((contents (with-temp-buffer
                            (insert-file-contents logpath)
                            (buffer-string))))
            (should (string-match-p "first line" contents))
            (should (string-match-p "second line" contents))))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-do-log-to-file-skips-when-disabled ()
  "`claude-repl--do-log-to-file' should not write when `claude-repl-log-to-file' is nil."
  (let* ((tmpdir (make-temp-file "test-log-" t))
         (logpath (expand-file-name ".claude-repl.log" tmpdir))
         (claude-repl-log-to-file nil))
    (unwind-protect
        (cl-letf (((symbol-function 'claude-repl--logfile-path) (lambda () logpath)))
          (claude-repl--do-log-to-file "should not appear")
          (should-not (file-exists-p logpath)))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-do-log-writes-to-file ()
  "`claude-repl--do-log' should write to the logfile when log-to-file is enabled."
  (let* ((tmpdir (make-temp-file "test-log-" t))
         (logpath (expand-file-name ".claude-repl.log" tmpdir))
         (claude-repl-log-to-file t)
         (claude-repl-debug t))
    (unwind-protect
        (cl-letf (((symbol-function 'claude-repl--logfile-path) (lambda () logpath))
                  ((symbol-function 'message) (lambda (&rest _args) nil)))
          (claude-repl--log nil "hello %s" "world")
          (let ((contents (with-temp-buffer
                            (insert-file-contents logpath)
                            (buffer-string))))
            (should (string-match-p "hello world" contents))
            (should (string-match-p "\\[claude-repl\\]" contents))))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-do-log-to-file-survives-write-error ()
  "`claude-repl--do-log-to-file' should not signal on write errors."
  (let ((claude-repl-log-to-file t))
    (cl-letf (((symbol-function 'claude-repl--logfile-path)
               (lambda () "/nonexistent/dir/impossible.log")))
      ;; Should not error
      (claude-repl--do-log-to-file "test"))))

;;;; ---- Tests: dir-has-git-p ----

(ert-deftest claude-repl-test-dir-has-git-p-with-git-dir ()
  "dir-has-git-p should return non-nil for directory with .git subdirectory."
  (let ((tmpdir (make-temp-file "test-git-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" tmpdir) t)
          (should (claude-repl--dir-has-git-p tmpdir)))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-dir-has-git-p-with-git-file ()
  "dir-has-git-p should return non-nil for directory with .git file (worktree)."
  (let ((tmpdir (make-temp-file "test-git-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".git" tmpdir)
            (insert "gitdir: /some/other/path"))
          (should (claude-repl--dir-has-git-p tmpdir)))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-dir-has-git-p-no-git ()
  "dir-has-git-p should return nil for directory without .git."
  (let ((tmpdir (make-temp-file "test-git-" t)))
    (unwind-protect
        (should-not (claude-repl--dir-has-git-p tmpdir))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-dir-has-git-p-nonexistent ()
  "dir-has-git-p should return nil for non-existent directory."
  (should-not (claude-repl--dir-has-git-p "/nonexistent/path/does/not/exist")))

(ert-deftest claude-repl-test-dir-has-git-p-nil ()
  "dir-has-git-p with nil should not error (expand-file-name handles nil)."
  ;; nil becomes default-directory; verify it doesn't crash.
  ;; No ignore-errors: if it signals, ERT correctly fails the test.
  (should (or (claude-repl--dir-has-git-p nil) t)))

(ert-deftest claude-repl-test-dir-has-git-p-empty-string ()
  "dir-has-git-p with empty string should not error."
  ;; No ignore-errors: if it signals, ERT correctly fails the test.
  (should (or (claude-repl--dir-has-git-p "") t)))

;;;; ---- Tests: git-root ----

(ert-deftest claude-repl-test-git-root-in-repo ()
  "git-root should return the repo root when called from within a git repo."
  (let ((tmpdir (make-temp-file "test-git-root-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" tmpdir) t)
          (let ((subdir (expand-file-name "a/b/c" tmpdir)))
            (make-directory subdir t)
            (let ((result (claude-repl--git-root subdir)))
              ;; Should find the tmpdir as root (it has .git)
              (should result)
              (should (string-match-p (regexp-quote (file-name-nondirectory tmpdir)) result)))))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-git-root-no-repo ()
  "git-root should return nil when called outside any git repo."
  ;; /tmp is very unlikely to be a git repo
  (let ((tmpdir (make-temp-file "test-no-repo-" t)))
    (unwind-protect
        ;; Stub dir-has-git-p to always return nil so we don't depend on host
        (cl-letf (((symbol-function 'claude-repl--dir-has-git-p) (lambda (_d) nil)))
          (should-not (claude-repl--git-root tmpdir)))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-git-root-uses-default-directory ()
  "git-root with no DIR arg should use `default-directory'."
  (let ((tmpdir (make-temp-file "test-git-dd-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" tmpdir) t)
          (let ((default-directory (file-name-as-directory tmpdir)))
            (let ((result (claude-repl--git-root)))
              (should result))))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-git-root-deeply-nested ()
  "git-root should find root from deeply nested subdirectory."
  (let ((tmpdir (make-temp-file "test-git-deep-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" tmpdir) t)
          (let ((deep (expand-file-name "a/b/c/d/e" tmpdir)))
            (make-directory deep t)
            (should (claude-repl--git-root deep))))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-git-root-explicit-dir ()
  "git-root with explicit DIR argument should search from that directory."
  (let ((tmpdir (make-temp-file "test-git-explicit-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" tmpdir) t)
          (should (claude-repl--git-root tmpdir)))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-git-root-worktree ()
  "git-root should find root when .git is a file (worktree)."
  (let ((tmpdir (make-temp-file "test-git-wt-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".git" tmpdir)
            (insert "gitdir: /some/other/.git/worktrees/foo"))
          (let ((subdir (expand-file-name "sub" tmpdir)))
            (make-directory subdir t)
            (should (claude-repl--git-root subdir))))
      (delete-directory tmpdir t))))

;;;; ---- Tests: git-string ----

(ert-deftest claude-repl-test-git-string-valid-command ()
  "git-string with a valid command should return trimmed output."
  (let ((result (claude-repl--git-string "rev-parse" "--show-toplevel")))
    ;; We're in a git repo, so this should return a non-empty path
    (should (stringp result))
    (should (> (length result) 0))))

(ert-deftest claude-repl-test-git-string-empty-result ()
  "git-string with a command producing empty output returns empty string."
  ;; `git tag -l nonexistent-tag-xxxxx` should produce nothing
  (let ((result (claude-repl--git-string "tag" "-l" "nonexistent-tag-xxxxx-99999")))
    (should (stringp result))
    (should (equal result ""))))

(ert-deftest claude-repl-test-git-string-invalid-command ()
  "git-string with an invalid command should return a string (includes stderr)."
  (let ((result (claude-repl--git-string "not-a-real-git-command-xyz")))
    (should (stringp result))
    ;; Should contain error text since stderr is included
    (should (> (length result) 0))))

;;;; ---- Tests: git-string-quiet ----

(ert-deftest claude-repl-test-git-string-quiet-valid ()
  "git-string-quiet with a valid command should return output."
  (let ((result (claude-repl--git-string-quiet "rev-parse" "--show-toplevel")))
    (should (stringp result))
    (should (> (length result) 0))))

(ert-deftest claude-repl-test-git-string-quiet-invalid ()
  "git-string-quiet with an invalid command should return empty string."
  (let ((default-directory "/tmp/"))
    (let ((result (claude-repl--git-string-quiet "rev-parse" "--show-toplevel")))
      ;; /tmp is not a git repo, so stderr is suppressed and result should be empty
      (should (stringp result)))))

(ert-deftest claude-repl-test-git-string-quiet-outside-repo ()
  "git-string-quiet called outside any git repo should return empty string."
  (let ((tmpdir (make-temp-file "test-no-git-" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory tmpdir)))
          (let ((result (claude-repl--git-string-quiet "rev-parse" "--show-toplevel")))
            (should (stringp result))
            (should (equal result ""))))
      (delete-directory tmpdir t))))

;;;; ---- Tests: print-git-branch ----

(ert-deftest claude-repl-test-print-git-branch-message ()
  "print-git-branch should include the git branch value in its message."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (claude-repl-print-git-branch)
      (should (stringp captured-msg))
      (should (string-match-p (regexp-quote claude-repl-git-branch) captured-msg)))))

;;;; ---- Tests: path-canonical ----

(ert-deftest claude-repl-test-path-canonical-trailing-slash ()
  "path-canonical should strip trailing slash."
  (let ((result (claude-repl--path-canonical "/tmp/foo/")))
    (should-not (string-suffix-p "/" result))))

(ert-deftest claude-repl-test-path-canonical-no-trailing-slash ()
  "path-canonical without trailing slash should remain unchanged (modulo truename)."
  (let ((result (claude-repl--path-canonical "/tmp")))
    (should-not (string-suffix-p "/" result))))

(ert-deftest claude-repl-test-path-canonical-tilde ()
  "path-canonical should expand tilde."
  (let ((result (claude-repl--path-canonical "~")))
    (should-not (string-prefix-p "~" result))
    (should (string-prefix-p "/" result))))

(ert-deftest claude-repl-test-path-canonical-relative-path ()
  "path-canonical should expand relative paths."
  (let ((result (claude-repl--path-canonical ".")))
    (should (string-prefix-p "/" result))))

(ert-deftest claude-repl-test-path-canonical-root ()
  "path-canonical for / should return /."
  ;; directory-file-name of "/" is "" on some systems, but file-truename "/" is "/"
  ;; and directory-file-name "/" is "/"; this just verifies no crash.
  (let ((result (claude-repl--path-canonical "/")))
    (should (stringp result))))

(ert-deftest claude-repl-test-path-canonical-symlink ()
  "path-canonical should resolve symlinks to true path."
  (let ((tmpdir (make-temp-file "test-sym-" t)))
    (unwind-protect
        (let* ((real-dir (expand-file-name "real" tmpdir))
               (link-dir (expand-file-name "link" tmpdir)))
          (make-directory real-dir t)
          (make-symbolic-link real-dir link-dir)
          (let ((result (claude-repl--path-canonical link-dir)))
            ;; Should resolve to the real path
            (should (string-match-p "real" result))
            (should-not (string-match-p "link" result))))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-path-canonical-empty-string ()
  "path-canonical with empty string should not error."
  (let ((result (claude-repl--path-canonical "")))
    (should (stringp result))))

;;;; ---- Tests: workspace-id ----

(ert-deftest claude-repl-test-workspace-id-nil-root ()
  "workspace-id should return nil when ws-dir errors and main-git-root is empty."
  (let ((claude-repl--main-git-root ""))
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'claude-repl--ws-dir)
               (lambda (_ws) (error "no dir"))))
      (should-not (claude-repl--workspace-id)))))

(ert-deftest claude-repl-test-workspace-id-hash-length ()
  "workspace-id should return exactly 8 characters."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
            ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/test/project")))
    (let ((id (claude-repl--workspace-id)))
      (should (= (length id) 8)))))

(ert-deftest claude-repl-test-workspace-id-different-roots ()
  "Two different roots should produce different IDs."
  (let (id1 id2)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/path/one")))
      (setq id1 (claude-repl--workspace-id)))
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws2"))
              ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/path/two")))
      (setq id2 (claude-repl--workspace-id)))
    (should-not (equal id1 id2))))

(ert-deftest claude-repl-test-workspace-id-deterministic ()
  "Same root should always produce the same ID."
  (let (id1 id2)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/stable/path")))
      (setq id1 (claude-repl--workspace-id))
      (setq id2 (claude-repl--workspace-id)))
    (should (equal id1 id2))))

;;;; ---- Tests: ws-get / ws-put ----

(ert-deftest claude-repl-test-ws-get-nonexistent-workspace ()
  "ws-get on non-existent workspace should return nil."
  (claude-repl-test--with-clean-state
    (should-not (claude-repl--ws-get "nonexistent" :status))))

(ert-deftest claude-repl-test-ws-get-nonexistent-key ()
  "ws-get for non-existent key on existing workspace should return nil."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :status "active")
    (should-not (claude-repl--ws-get "ws1" :nonexistent-key))))

(ert-deftest claude-repl-test-ws-get-zero-value ()
  "ws-get should return 0 when key is set to 0 (not confuse with nil)."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :counter 0)
    (should (equal (claude-repl--ws-get "ws1" :counter) 0))))

(ert-deftest claude-repl-test-ws-get-empty-string-value ()
  "ws-get should return empty string when key is set to empty string."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :name "")
    (should (equal (claude-repl--ws-get "ws1" :name) ""))))

(ert-deftest claude-repl-test-ws-put-new-workspace ()
  "ws-put to a brand new workspace should create the entry."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "new-ws" :status "ready")
    (should (equal (claude-repl--ws-get "new-ws" :status) "ready"))))

(ert-deftest claude-repl-test-ws-put-overwrite ()
  "ws-put should overwrite an existing key."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :status "old")
    (claude-repl--ws-put "ws1" :status "new")
    (should (equal (claude-repl--ws-get "ws1" :status) "new"))))

(ert-deftest claude-repl-test-ws-put-nil-value ()
  "ws-put with nil value should set key to nil."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :status "active")
    (claude-repl--ws-put "ws1" :status nil)
    (should-not (claude-repl--ws-get "ws1" :status))))

(ert-deftest claude-repl-test-ws-put-multiple-keys ()
  "ws-put should support multiple keys on the same workspace."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :status "ready")
    (claude-repl--ws-put "ws1" :priority "p1")
    (claude-repl--ws-put "ws1" :counter 42)
    (should (equal (claude-repl--ws-get "ws1" :status) "ready"))
    (should (equal (claude-repl--ws-get "ws1" :priority) "p1"))
    (should (equal (claude-repl--ws-get "ws1" :counter) 42))))

;;;; ---- Tests: ws-del ----

(ert-deftest claude-repl-test-ws-del-existing ()
  "ws-del should remove an existing workspace."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :status "ready")
    (claude-repl--ws-del "ws1")
    (should-not (claude-repl--ws-get "ws1" :status))))

(ert-deftest claude-repl-test-ws-del-nonexistent ()
  "ws-del on a non-existent workspace should be a no-op."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-del "nonexistent")
    ;; Should not error
    (should t)))

(ert-deftest claude-repl-test-ws-del-get-returns-nil ()
  "After ws-del, ws-get should return nil for any key."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :status "ready")
    (claude-repl--ws-put "ws1" :priority "p1")
    (claude-repl--ws-del "ws1")
    (should-not (claude-repl--ws-get "ws1" :status))
    (should-not (claude-repl--ws-get "ws1" :priority))))

;;;; ---- Tests: record-project-dir ----

(ert-deftest claude-repl-test-record-project-dir-sets-when-unset ()
  "record-project-dir writes :project-dir when workspace has none."
  (claude-repl-test--with-clean-state
    (claude-repl--record-project-dir "ws1" "/path/to/ws1")
    (should (equal (claude-repl--ws-get "ws1" :project-dir) "/path/to/ws1"))))

(ert-deftest claude-repl-test-record-project-dir-preserves-existing ()
  "record-project-dir never overwrites an already-set :project-dir.
Load-bearing: worktree workspaces store their canonical path via
`register-worktree-ws' before ensure-vterm-buffer runs, and a drifted
`default-directory' from elsewhere must not clobber it."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/canonical/worktree")
    (claude-repl--record-project-dir "ws1" "/drifted/default-dir")
    (should (equal (claude-repl--ws-get "ws1" :project-dir) "/canonical/worktree"))))

(ert-deftest claude-repl-test-record-project-dir-creates-ws-entry ()
  "record-project-dir works even when the workspace has no hash entry yet.
`ws-put' via `plist-put' on nil produces a fresh plist, so a workspace
that was never registered still gets :project-dir — this is the exact
path that closes the SPC-j-x-restart warning."
  (claude-repl-test--with-clean-state
    (should-not (gethash "fresh-ws" claude-repl--workspaces))
    (claude-repl--record-project-dir "fresh-ws" "/path")
    (should (equal (claude-repl--ws-get "fresh-ws" :project-dir) "/path"))))

;;;; ---- Tests: create-buffer ----

(ert-deftest claude-repl-test-create-buffer-vterm-name ()
  "create-buffer with no suffix produces the vterm buffer name."
  (let ((buf nil))
    (unwind-protect
        (progn
          (setq buf (claude-repl--create-buffer "ws1"))
          (should (buffer-live-p buf))
          (should (equal (buffer-name buf) "*claude-panel-ws1*")))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-test-create-buffer-input-suffix ()
  "create-buffer with \"-input\" suffix produces the input buffer name."
  (let ((buf nil))
    (unwind-protect
        (progn
          (setq buf (claude-repl--create-buffer "ws1" "-input"))
          (should (equal (buffer-name buf) "*claude-panel-input-ws1*")))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-test-create-buffer-sets-owning-workspace ()
  "create-buffer sets `claude-repl--owning-workspace' buffer-locally."
  (let ((buf nil))
    (unwind-protect
        (progn
          (setq buf (claude-repl--create-buffer "ws1"))
          (should (equal (buffer-local-value 'claude-repl--owning-workspace buf)
                         "ws1")))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-test-create-buffer-owning-workspace-survives-mode ()
  "`claude-repl--owning-workspace' survives `kill-all-local-variables'.
Major-mode activation (vterm-mode, claude-input-mode) wipes buffer-local
bindings; the permanent-local property on this variable is what keeps
ownership intact across that transition."
  (let ((buf nil))
    (unwind-protect
        (progn
          (setq buf (claude-repl--create-buffer "ws1"))
          (with-current-buffer buf
            (kill-all-local-variables))
          (should (equal (buffer-local-value 'claude-repl--owning-workspace buf)
                         "ws1")))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-test-create-buffer-attaches-to-persp ()
  "create-buffer adds the buffer to WS's perspective when it exists."
  (let ((buf nil)
        (added nil))
    (unwind-protect
        (cl-letf (((symbol-function 'persp-get-by-name)
                   (lambda (_name) 'fake-persp))
                  ((symbol-function 'persp-add-buffer)
                   (lambda (b persp &rest _)
                     (setq added (list b persp)))))
          (setq buf (claude-repl--create-buffer "ws1"))
          (should (equal added (list buf 'fake-persp))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-test-create-buffer-skips-persp-when-not-found ()
  "create-buffer does not error when no perspective named WS exists."
  (let ((buf nil)
        (add-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_name) nil))
                  ((symbol-function 'persp-add-buffer)
                   (lambda (&rest _) (setq add-called t))))
          (setq buf (claude-repl--create-buffer "ws1"))
          (should-not add-called))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-test-create-buffer-errors-when-ws-nil ()
  "create-buffer signals an error when WS is nil and no current workspace."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
    (should-error (claude-repl--create-buffer nil) :type 'error)))

(ert-deftest claude-repl-test-create-buffer-idempotent ()
  "Calling create-buffer twice with the same args reuses the buffer."
  (let ((first nil))
    (unwind-protect
        (let* ((_ (setq first (claude-repl--create-buffer "ws1")))
               (second (claude-repl--create-buffer "ws1")))
          (should (eq first second)))
      (when (buffer-live-p first) (kill-buffer first)))))

;;;; ---- Tests: active-inst ----

(ert-deftest claude-repl-test-active-inst-default-bare-metal ()
  "active-inst should error when no :active-env is set (ensure-ws-env not called)."
  (claude-repl-test--with-clean-state
    (should-error (claude-repl--active-inst "ws1") :type 'error)))

(ert-deftest claude-repl-test-active-inst-sandbox-env ()
  "active-inst should use :sandbox when :active-env is set to :sandbox."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :active-env :sandbox)
    (claude-repl--ws-put "ws1" :sandbox (make-claude-repl-instantiation))
    (let ((inst (claude-repl--active-inst "ws1")))
      (should (claude-repl-instantiation-p inst))
      (should (equal (claude-repl--ws-get "ws1" :sandbox) inst)))))

(ert-deftest claude-repl-test-active-inst-returns-same-struct ()
  "active-inst should return the same struct on second call (not create a new one)."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :active-env :bare-metal)
    (claude-repl--ws-put "ws1" :bare-metal (make-claude-repl-instantiation))
    (let ((inst1 (claude-repl--active-inst "ws1"))
          (inst2 (claude-repl--active-inst "ws1")))
      (should (eq inst1 inst2)))))

(ert-deftest claude-repl-test-active-inst-is-struct ()
  "active-inst should return a claude-repl-instantiation struct."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :active-env :bare-metal)
    (claude-repl--ws-put "ws1" :bare-metal (make-claude-repl-instantiation))
    (let ((inst (claude-repl--active-inst "ws1")))
      (should (claude-repl-instantiation-p inst)))))

(ert-deftest claude-repl-test-active-inst-fields-nil-by-default ()
  "active-inst struct fields should be nil by default."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :active-env :bare-metal)
    (claude-repl--ws-put "ws1" :bare-metal (make-claude-repl-instantiation))
    (let ((inst (claude-repl--active-inst "ws1")))
      (should-not (claude-repl-instantiation-session-id inst))
      (should-not (claude-repl-instantiation-start-cmd inst)))))

;;;; ---- Tests: buffer-name edge cases ----

(ert-deftest claude-repl-test-buffer-name-empty-suffix ()
  "Buffer name with empty string suffix should work like no suffix."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "abcd1234")))
    (should (equal (claude-repl--buffer-name "") "*claude-panel-abcd1234*"))))

(ert-deftest claude-repl-test-buffer-name-various-suffixes ()
  "Buffer name with various suffix values should include them."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "abcd1234")))
    (should (equal (claude-repl--buffer-name "-debug") "*claude-panel-debug-abcd1234*"))
    (should (equal (claude-repl--buffer-name "-log") "*claude-panel-log-abcd1234*"))))

(ert-deftest claude-repl-test-buffer-name-matches-regexps ()
  "Buffer names should match their respective regexp patterns.
Use `claude-repl--claude-buffer-p' for the vterm-vs-input distinction —
`claude-repl--vterm-buffer-re' is a superset that also matches input names."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "abcd1234")))
    (let ((vterm-name (claude-repl--buffer-name))
          (input-name (claude-repl--buffer-name "-input")))
      (should (string-match-p claude-repl--vterm-buffer-re vterm-name))
      (should (string-match-p claude-repl--input-buffer-re input-name))
      ;; Vterm name does NOT match input-re; predicate on vterm name is true.
      (should-not (string-match-p claude-repl--input-buffer-re vterm-name))
      (with-temp-buffer
        (rename-buffer vterm-name t)
        (should (claude-repl--claude-buffer-p)))
      ;; Input name matches vterm-re (superset), but the predicate correctly
      ;; excludes it.
      (should (string-match-p claude-repl--vterm-buffer-re input-name))
      (with-temp-buffer
        (rename-buffer input-name t)
        (should-not (claude-repl--claude-buffer-p))))))

;;;; ---- Tests: claude-buffer-p edge cases ----

(ert-deftest claude-repl-test-claude-buffer-p-no-hash ()
  "claude-buffer-p should not match *claude-panel-* without hex chars."
  (claude-repl-test--with-temp-buffer "*claude-panel-*"
    (should-not (claude-repl--claude-buffer-p))))

(ert-deftest claude-repl-test-claude-buffer-p-extra-after-pattern ()
  "claude-buffer-p should not match buffer with extra characters after pattern."
  (claude-repl-test--with-temp-buffer "*claude-panel-abcd1234*extra"
    (should-not (claude-repl--claude-buffer-p))))

(ert-deftest claude-repl-test-claude-buffer-p-nil-uses-current ()
  "claude-buffer-p with nil should use current buffer."
  (claude-repl-test--with-temp-buffer "*claude-panel-abcd1234*"
    (should (claude-repl--claude-buffer-p nil))))

(ert-deftest claude-repl-test-claude-buffer-p-explicit-buffer ()
  "claude-buffer-p with explicit buffer argument."
  (claude-repl-test--with-temp-buffer "*claude-panel-abcd1234*"
    (let ((buf (current-buffer)))
      (claude-repl-test--with-temp-buffer "*scratch-test*"
        ;; Current buffer is *scratch-test*, but pass buf explicitly
        (should (claude-repl--claude-buffer-p buf))))))

;;;; ---- Tests: claude-panel-buffer-p ----

(ert-deftest claude-repl-test-claude-panel-buffer-p-vterm ()
  "claude-panel-buffer-p should match vterm buffer names."
  (claude-repl-test--with-temp-buffer "*claude-panel-abcd1234*"
    (should (claude-repl--claude-panel-buffer-p))))

(ert-deftest claude-repl-test-claude-panel-buffer-p-input ()
  "claude-panel-buffer-p should match input buffer names."
  (claude-repl-test--with-temp-buffer "*claude-panel-input-abcd1234*"
    (should (claude-repl--claude-panel-buffer-p))))

(ert-deftest claude-repl-test-claude-panel-buffer-p-regular ()
  "claude-panel-buffer-p should not match regular buffer names."
  (claude-repl-test--with-temp-buffer "*scratch*"
    (should-not (claude-repl--claude-panel-buffer-p))))

(ert-deftest claude-repl-test-claude-panel-buffer-p-nil ()
  "claude-panel-buffer-p with nil should use current buffer."
  (claude-repl-test--with-temp-buffer "*claude-panel-input-abcd1234*"
    (should (claude-repl--claude-panel-buffer-p nil))))

;;;; ---- Tests: non-user-buffer-p ----

(ert-deftest claude-repl-test-non-user-buffer-p-claude-panel ()
  "non-user-buffer-p should return non-nil for claude panel buffer."
  (claude-repl-test--with-temp-buffer "*claude-panel-abcd1234*"
    (should (claude-repl--non-user-buffer-p (current-buffer)))))

(ert-deftest claude-repl-test-non-user-buffer-p-minibuffer ()
  "non-user-buffer-p should return non-nil for minibuffer-like names."
  (claude-repl-test--with-temp-buffer " *Minibuf-0*"
    (should (claude-repl--non-user-buffer-p (current-buffer)))))

(ert-deftest claude-repl-test-non-user-buffer-p-dead-buffer ()
  "non-user-buffer-p should return non-nil for dead/killed buffer."
  (let ((buf (get-buffer-create " *test-dead*")))
    (kill-buffer buf)
    (should (claude-repl--non-user-buffer-p buf))))

(ert-deftest claude-repl-test-non-user-buffer-p-nil ()
  "non-user-buffer-p should return non-nil for nil input."
  (should (claude-repl--non-user-buffer-p nil)))

(ert-deftest claude-repl-test-non-user-buffer-p-string-nonexistent ()
  "non-user-buffer-p should return non-nil for string name of non-existent buffer."
  (should (claude-repl--non-user-buffer-p "nonexistent-buffer-name-99999")))

(ert-deftest claude-repl-test-non-user-buffer-p-string-existing ()
  "non-user-buffer-p should return nil for string name of existing normal buffer."
  (claude-repl-test--with-temp-buffer "*normal-test-buf*"
    (should-not (claude-repl--non-user-buffer-p "*normal-test-buf*"))))

(ert-deftest claude-repl-test-non-user-buffer-p-normal-buffer ()
  "non-user-buffer-p should return nil for a normal live buffer."
  (claude-repl-test--with-temp-buffer "*normal-test-buf2*"
    (should-not (claude-repl--non-user-buffer-p (current-buffer)))))

;;;; ---- Tests: non-claude-buffers ----

(ert-deftest claude-repl-test-non-claude-buffers-empty-list ()
  "non-claude-buffers with empty list should return empty list."
  (should (null (claude-repl--non-claude-buffers nil))))

(ert-deftest claude-repl-test-non-claude-buffers-all-claude ()
  "non-claude-buffers with all claude buffers should return empty list."
  (claude-repl-test--with-temp-buffer "*claude-panel-aaaa1111*"
    (let ((buf1 (current-buffer)))
      (claude-repl-test--with-temp-buffer "*claude-panel-input-bbbb2222*"
        (let ((buf2 (current-buffer)))
          (should (null (claude-repl--non-claude-buffers (list buf1 buf2)))))))))

(ert-deftest claude-repl-test-non-claude-buffers-no-claude ()
  "non-claude-buffers with no claude buffers should return all."
  (claude-repl-test--with-temp-buffer "*normal-a*"
    (let ((buf1 (current-buffer)))
      (claude-repl-test--with-temp-buffer "*normal-b*"
        (let ((buf2 (current-buffer)))
          (let ((result (claude-repl--non-claude-buffers (list buf1 buf2))))
            (should (= (length result) 2))))))))

(ert-deftest claude-repl-test-non-claude-buffers-mixed ()
  "non-claude-buffers with mixed list should filter correctly."
  (claude-repl-test--with-temp-buffer "*claude-panel-aaaa1111*"
    (let ((claude-buf (current-buffer)))
      (claude-repl-test--with-temp-buffer "*normal-buf*"
        (let ((normal-buf (current-buffer)))
          (let ((result (claude-repl--non-claude-buffers (list claude-buf normal-buf))))
            (should (= (length result) 1))
            (should (eq (car result) normal-buf))))))))

(ert-deftest claude-repl-test-non-claude-buffers-nil-entries ()
  "non-claude-buffers should filter out nil entries."
  (claude-repl-test--with-temp-buffer "*normal-c*"
    (let ((buf (current-buffer)))
      (let ((result (claude-repl--non-claude-buffers (list nil buf nil))))
        (should (= (length result) 1))
        (should (eq (car result) buf))))))

(ert-deftest claude-repl-test-non-claude-buffers-string-names ()
  "non-claude-buffers should handle string names (non-existent buffers are filtered)."
  (claude-repl-test--with-temp-buffer "*normal-str*"
    ;; String names of non-existent buffers should be filtered (non-user-buffer-p returns t)
    (let ((result (claude-repl--non-claude-buffers (list "*normal-str*" "nonexistent-xyz"))))
      (should (= (length result) 1))
      (should (equal (car result) "*normal-str*")))))

;;;; ---- Tests: current-ws-p ----

(ert-deftest claude-repl-test-current-ws-p-match ()
  "current-ws-p should return non-nil when WS matches current workspace."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws")))
    (should (claude-repl--current-ws-p "my-ws"))))

(ert-deftest claude-repl-test-current-ws-p-no-match ()
  "current-ws-p should return nil when WS does not match."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws")))
    (should-not (claude-repl--current-ws-p "other-ws"))))

(ert-deftest claude-repl-test-current-ws-p-empty-string ()
  "current-ws-p with empty string should not match non-empty workspace."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws")))
    (should-not (claude-repl--current-ws-p ""))))

;;;; ---- Tests: current-ws-live-vterm ----

(ert-deftest claude-repl-test-current-ws-live-vterm-no-buffer ()
  "current-ws-live-vterm should return nil when no vterm buffer is stored."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (should-not (claude-repl--current-ws-live-vterm)))))

(ert-deftest claude-repl-test-current-ws-live-vterm-dead-buffer ()
  "current-ws-live-vterm should return nil for a killed buffer."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (let ((buf (get-buffer-create " *test-dead-vterm*")))
        (claude-repl--ws-put "ws1" :vterm-buffer buf)
        (kill-buffer buf)
        (should-not (claude-repl--current-ws-live-vterm))))))

(ert-deftest claude-repl-test-current-ws-live-vterm-live-buffer ()
  "current-ws-live-vterm should return the buffer when live."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-live-vterm*"
      (let ((buf (current-buffer)))
        (claude-repl--ws-put "ws1" :vterm-buffer buf)
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
          (should (eq (claude-repl--current-ws-live-vterm) buf)))))))

;;;; ---- Tests: vterm-live-p edge case ----

(ert-deftest claude-repl-test-vterm-live-p-nil-explicit ()
  "vterm-live-p should return nil when buffer is explicitly set to nil."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (claude-repl--ws-put "ws1" :vterm-buffer nil)
      (should-not (claude-repl--vterm-live-p)))))

;;;; ---- Tests: with-vterm-buf macro ----

(ert-deftest claude-repl-test-with-vterm-buf-no-live-buffer ()
  "with-vterm-buf should return nil when no live vterm buffer exists."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (should-not (claude-repl--with-vterm-buf 'should-not-reach)))))

(ert-deftest claude-repl-test-with-vterm-buf-live-buffer ()
  "with-vterm-buf should execute body when live vterm buffer exists."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-vterm-macro*"
      (let ((buf (current-buffer)))
        (claude-repl--ws-put "ws1" :vterm-buffer buf)
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
          (let ((executed nil))
            (claude-repl--with-vterm-buf
              (setq executed t))
            (should executed)))))))

(ert-deftest claude-repl-test-with-vterm-buf-binds-vterm-buf ()
  "with-vterm-buf should bind `vterm-buf' to the live buffer."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-vterm-bind*"
      (let ((buf (current-buffer)))
        (claude-repl--ws-put "ws1" :vterm-buffer buf)
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
          (should (eq (claude-repl--with-vterm-buf vterm-buf) buf)))))))

(ert-deftest claude-repl-test-with-vterm-buf-returns-value ()
  "with-vterm-buf should return the value of the body."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-vterm-ret*"
      (let ((buf (current-buffer)))
        (claude-repl--ws-put "ws1" :vterm-buffer buf)
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
          (should (equal (claude-repl--with-vterm-buf 42) 42)))))))

;;;; ---- Tests: instantiation struct ----

(ert-deftest claude-repl-test-instantiation-create ()
  "make-claude-repl-instantiation should create a struct."
  (let ((inst (make-claude-repl-instantiation)))
    (should (claude-repl-instantiation-p inst))))

(ert-deftest claude-repl-test-instantiation-default-fields ()
  "Instantiation struct fields should default to nil."
  (let ((inst (make-claude-repl-instantiation)))
    (should-not (claude-repl-instantiation-session-id inst))
    (should-not (claude-repl-instantiation-start-cmd inst))))

(ert-deftest claude-repl-test-instantiation-setf ()
  "Instantiation struct fields should be modifiable with setf."
  (let ((inst (make-claude-repl-instantiation)))
    (setf (claude-repl-instantiation-session-id inst) "sess-123")
    (setf (claude-repl-instantiation-start-cmd inst) "claude --resume")
    (should (equal (claude-repl-instantiation-session-id inst) "sess-123"))
    (should (equal (claude-repl-instantiation-start-cmd inst) "claude --resume"))))

;;;; ---- Tests: defvar/defcustom declarations ----

(ert-deftest claude-repl-test-timers-var-bound ()
  "`claude-repl--timers' should be bound."
  (should (boundp 'claude-repl--timers)))

(ert-deftest claude-repl-test-debug-default-nil ()
  "`claude-repl-debug' should default to nil."
  (should (boundp 'claude-repl-debug))
  ;; Note: default-value because tests may let-bind it
  (should-not (default-value 'claude-repl-debug)))

(ert-deftest claude-repl-test-vterm-buffer-re-matches ()
  "`claude-repl--vterm-buffer-re' matches vterm and input names (superset by design).
Callers that need a vterm-only check must use `claude-repl--claude-buffer-p'."
  (should (string-match-p claude-repl--vterm-buffer-re "*claude-panel-abcd1234*"))
  (should (string-match-p claude-repl--vterm-buffer-re "*claude-panel-my-workspace*"))
  ;; Vterm-re intentionally also matches input buffers (workspace names can
  ;; contain hyphens, so the regex can't cheaply exclude "input-*").
  (should (string-match-p claude-repl--vterm-buffer-re "*claude-panel-input-abcd1234*"))
  (should-not (string-match-p claude-repl--vterm-buffer-re "*scratch*")))

(ert-deftest claude-repl-test-input-buffer-re-matches ()
  "`claude-repl--input-buffer-re' should match expected input buffer patterns."
  (should (string-match-p claude-repl--input-buffer-re "*claude-panel-input-abcd1234*"))
  (should (string-match-p claude-repl--input-buffer-re "*claude-panel-input-my-workspace*"))
  (should-not (string-match-p claude-repl--input-buffer-re "*claude-panel-abcd1234*"))
  (should-not (string-match-p claude-repl--input-buffer-re "*scratch*")))

;;;; ---- Tests: log-format hardening against non-string fmt ----

(ert-deftest claude-repl-test-log-format-tolerates-symbol-fmt ()
  "`claude-repl--log-format' must not crash when FMT is a symbol.
Regression guard for callers that pass a file-notify action symbol by mistake."
  (let ((claude-repl--log-format-bug-captured t)) ; suppress capture side-effect
    (let ((result (claude-repl--log-format nil 'stopped)))
      (should (stringp result))
      (should (string-match-p "BUG non-string-fmt=stopped" result)))))

(ert-deftest claude-repl-test-log-format-captures-backtrace-once ()
  "Non-string FMT should capture a backtrace into *claude-repl-log-bug* only once."
  (let ((claude-repl--log-format-bug-captured nil))
    (unwind-protect
        (progn
          (when (get-buffer "*claude-repl-log-bug*")
            (kill-buffer "*claude-repl-log-bug*"))
          (claude-repl--log-format nil 'stopped)
          (should (get-buffer "*claude-repl-log-bug*"))
          (should claude-repl--log-format-bug-captured)
          (let ((size (buffer-size (get-buffer "*claude-repl-log-bug*"))))
            (claude-repl--log-format nil 'changed)
            ;; Second call should NOT add more content.
            (should (= size (buffer-size (get-buffer "*claude-repl-log-bug*"))))))
      (when (get-buffer "*claude-repl-log-bug*")
        (kill-buffer "*claude-repl-log-bug*")))))

(ert-deftest claude-repl-test-do-log-survives-percent-in-metadata ()
  "`claude-repl--do-log' must not raise arity errors when workspace metadata
contains a literal `%' character.  Regression for \"Not enough arguments for
format string\" seen when running `claude-repl-reset-sentinel-watchers'."
  (let ((claude-repl-debug t))
    (cl-letf (((symbol-function 'claude-repl--format-ws-metadata)
               (lambda (_ws) " {dir=/path/with/%s/literal}"))
              ((symbol-function 'message) #'ignore))
      ;; Should complete without signaling a format-string arity error.
      (claude-repl--log nil "plain message, no specifiers")
      (claude-repl--log nil "one-specifier=%s" "value")
      ;; Non-string fmt path should also be safe.
      (claude-repl--log nil 'stopped)
      (should t))))

(provide 'test-core)

;;; test-core.el ends here
