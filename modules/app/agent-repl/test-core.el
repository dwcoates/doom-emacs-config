;;; test-core.el --- ERT tests for agent-repl core.el -*- lexical-binding: t; -*-

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

(ert-deftest agent-repl-test-workspace-id-from-project-root ()
  "Workspace ID should be first 8 chars of MD5 of the canonical ws-dir path."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
            ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/test/project")))
    (should (equal (agent-repl--workspace-id)
                   (substring (md5 (agent-repl--path-canonical "/test/project")) 0 8)))))

;;;; ---- Tests: resolve-current-git-root ----

(ert-deftest agent-repl-test-resolve-current-git-root-prefers-ws-dir ()
  "When the current workspace has a :project-dir, the resolver uses it as
the directory to run `git rev-parse --show-toplevel' from (not
`default-directory')."
  (let ((captured-default-dir nil))
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'agent-repl--ws-dir)
               (lambda (_ws) "/repo/subdir"))
              ((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _)
                 (setq captured-default-dir default-directory)
                 "/repo")))
      (let ((default-directory "/elsewhere/"))
        (should (equal (agent-repl--resolve-current-git-root) "/repo/"))
        ;; git was invoked from the ws-dir, not default-directory
        (should (equal captured-default-dir "/repo/subdir"))))))

(ert-deftest agent-repl-test-resolve-current-git-root-falls-back-to-default-directory ()
  "When no workspace has a :project-dir, the resolver runs git from
`default-directory'."
  (let ((captured-default-dir nil))
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'agent-repl--ws-dir)
               (lambda (_ws) (error "no dir")))
              ((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _)
                 (setq captured-default-dir default-directory)
                 "/fallback/repo")))
      (let ((default-directory "/fallback/repo/deep/"))
        (should (equal (agent-repl--resolve-current-git-root) "/fallback/repo/"))
        (should (equal captured-default-dir "/fallback/repo/deep/"))))))

(ert-deftest agent-repl-test-resolve-current-git-root-errors-outside-repo ()
  "When `git rev-parse' returns empty (not inside any repo), the resolver
signals `user-error' rather than silently returning a bogus path."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
            ((symbol-function 'agent-repl--ws-dir)
             (lambda (_ws) (error "no dir")))
            ((symbol-function 'agent-repl--git-string-quiet)
             (lambda (&rest _) "")))
    (should-error (agent-repl--resolve-current-git-root) :type 'user-error)))

;;;; ---- Tests: Buffer naming ----

(ert-deftest agent-repl-test-buffer-name-format ()
  "Buffer names should follow *agent-panel-WS* and *agent-panel-input-WS* pattern."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws")))
    (should (equal (agent-repl--buffer-name) "*agent-panel-my-ws*"))
    (should (equal (agent-repl--buffer-name "-input") "*agent-panel-input-my-ws*"))))

(ert-deftest agent-repl-test-buffer-name-default ()
  "Buffer name signals an error when no workspace name is available."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
    (should-error (agent-repl--buffer-name) :type 'error)))

(ert-deftest agent-repl-test-buffer-name-empty-ws ()
  "Buffer name signals an error when the resolved workspace name is empty."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "")))
    (should-error (agent-repl--buffer-name) :type 'error))
  (should-error (agent-repl--buffer-name nil "") :type 'error))

(ert-deftest agent-repl-test-buffer-name-uses-explicit-ws ()
  "Buffer name should prefer the explicit WS argument over the current workspace."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws")))
    (should (equal (agent-repl--buffer-name nil "other-ws")
                   "*agent-panel-other-ws*"))))

(ert-deftest agent-repl-test-buffer-name-sanitizes-unsafe-chars ()
  "Workspace names with unsafe characters should be sanitized to underscores."
  (should (equal (agent-repl--buffer-name nil "feat/login")
                 "*agent-panel-feat_login*"))
  (should (equal (agent-repl--buffer-name nil "ws with space")
                 "*agent-panel-ws_with_space*"))
  (should (equal (agent-repl--buffer-name "-input" "a*b")
                 "*agent-panel-input-a_b*")))

(ert-deftest agent-repl-test-sanitize-ws-name ()
  "sanitize-ws-name keeps alphanumerics, hyphens, and underscores."
  (should (equal (agent-repl--sanitize-ws-name "abc-123_xyz") "abc-123_xyz"))
  (should (equal (agent-repl--sanitize-ws-name "feat/login") "feat_login"))
  (should (equal (agent-repl--sanitize-ws-name "a b*c") "a_b_c"))
  (should-not (agent-repl--sanitize-ws-name nil)))

;;;; ---- Tests: Buffer predicates ----

(ert-deftest agent-repl-test-agent-buffer-p ()
  "agent-buffer-p should match *agent-panel-WS* pattern only (excluding input)."
  (agent-repl-test--with-temp-buffer "*agent-panel-abcd1234*"
    (should (agent-repl--agent-buffer-p)))
  (agent-repl-test--with-temp-buffer "*agent-panel-input-abcd1234*"
    (should-not (agent-repl--agent-buffer-p)))
  ;; Use a name that does NOT begin with `*agent-panel-' so the vterm regex
  ;; can't match.  The real `*scratch*' is the current buffer when the
  ;; aggregate test runner starts, so naming a temp buffer `*scratch*' and
  ;; then killing it swaps us out of the original buffer and leaves
  ;; `default-directory' pointing at whatever buffer ert lands in next
  ;; (Emacs.app/Contents/MacOS on macOS).  That breaks subsequent tests
  ;; that call git without a -C flag.
  (agent-repl-test--with-temp-buffer "*repl-test-non-agent-buf*"
    (should-not (agent-repl--agent-buffer-p))))

;;;; ---- Tests: agent-view-buffer-name-p (the RENDERING predicate) ----

(ert-deftest agent-repl-test-agent-view-name-matches-the-vterm-output ()
  "A vterm workspace shows its agent in the vterm output buffer."
  (should (agent-repl--agent-view-buffer-name-p "*agent-panel-my-ws*")))

(ert-deftest agent-repl-test-agent-view-name-matches-the-gui-webview ()
  "A gui workspace shows its agent in the webview.
This is the whole point of the predicate: the tab-bar asks \"is the agent
view open\", and answering nil for every gui workspace is what left their
tabs permanently drawn as though the panels were closed."
  (should (agent-repl--agent-view-buffer-name-p "*agent-frontend-my-ws*")))

(ert-deftest agent-repl-test-agent-view-name-excludes-the-input-panel ()
  "The input panel alone is not a workspace showing its agent."
  (should-not (agent-repl--agent-view-buffer-name-p "*agent-panel-input-my-ws*")))

(ert-deftest agent-repl-test-agent-view-name-excludes-the-explain-config-popup ()
  "`SPC j h c' mounts a webview too, but it is not any workspace's agent view."
  (should-not (agent-repl--agent-view-buffer-name-p "*agent-explain-config*")))

(ert-deftest agent-repl-test-agent-view-name-excludes-an-unrelated-buffer ()
  "An ordinary buffer is not an agent view."
  (should-not (agent-repl--agent-view-buffer-name-p "*repl-test-non-agent-buf*")))

(ert-deftest agent-repl-test-agent-view-name-tolerates-a-non-string ()
  "A `window-state-get' tree can hold a non-string where a buffer name went."
  (should-not (agent-repl--agent-view-buffer-name-p nil)))

(ert-deftest agent-repl-test-agent-view-buffer-p-reads-the-current-buffer ()
  "The buffer-shaped form defers to the name-shaped one."
  (agent-repl-test--with-temp-buffer "*agent-frontend-my-ws*"
    (should (agent-repl--agent-view-buffer-p))))

(ert-deftest agent-repl-test-agent-buffer-p-still-refuses-the-webview ()
  "The PANEL predicate must NOT widen to the webview.
The input-panel bounce and the orphan sweep key off it, and frontend.el
keeps the webview out of that namespace on purpose."
  (agent-repl-test--with-temp-buffer "*agent-frontend-my-ws*"
    (should-not (agent-repl--agent-buffer-p))
    (should-not (agent-repl--agent-panel-buffer-p))))

;;;; ---- Tests: Logging ----

(ert-deftest agent-repl-test-log-respects-debug-flag ()
  "When `agent-repl-debug' is nil, `agent-repl--log' should NOT call `message'.
When t, it should call `message'."
  (let ((message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) (setq message-called t))))
      ;; debug off: no message
      (let ((agent-repl-debug nil))
        (agent-repl--log nil "test %s" "hello")
        (should-not message-called))
      ;; debug on: message called
      (let ((agent-repl-debug t))
        (setq message-called nil)
        (agent-repl--log nil "test %s" "hello")
        (should message-called)))))

;;;; ---- Tests: vterm buffer predicates ----

(ert-deftest agent-repl-test-vterm-live-p-nil ()
  "Returns nil when no vterm buffer is stored for the workspace."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (should-not (agent-repl--vterm-live-p)))))

(ert-deftest agent-repl-test-vterm-live-p-dead ()
  "Returns nil for a killed buffer."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (let ((buf (get-buffer-create " *test-dead-buf*")))
        (agent-repl--ws-put "ws1" :vterm-buffer buf)
        (kill-buffer buf)
        (should-not (agent-repl--vterm-live-p))))))

(ert-deftest agent-repl-test-vterm-live-p-live ()
  "Returns non-nil for a live buffer."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-live-buf*"
      (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
        (should (agent-repl--vterm-live-p))))))

(ert-deftest agent-repl-test-agent-running-p-no-process ()
  "Returns nil when buffer is live but has no process."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-no-proc*"
      (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
        (should-not (agent-repl--agent-running-p))))))

(ert-deftest agent-repl-test-agent-running-p-with-process ()
  "Returns non-nil when buffer has a live process."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-with-proc*"
      (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'get-buffer-process)
                 (lambda (_buf) 'fake-process)))
        (should (agent-repl--agent-running-p))))))

;;;; ---- Tests: Deferred macro ----

(ert-deftest agent-repl-test-deferred-macro ()
  "The deferred macro should create a debouncing lambda."
  (let ((agent-repl--sync-timer nil)
        (call-count 0))
    (let ((debounced (agent-repl--deferred agent-repl--sync-timer
                       (lambda () (cl-incf call-count)))))
      ;; Calling it should set the timer var
      (funcall debounced)
      (should agent-repl--sync-timer)
      ;; Cancel it to prevent side effects
      (cancel-timer agent-repl--sync-timer)
      (setq agent-repl--sync-timer nil))))

;;;; ---- Bug regression tests ----

(ert-deftest agent-repl-test-bug9-paste-delay-configurable ()
  "Bug 9: agent-repl-paste-delay should be a configurable variable."
  (should (boundp 'agent-repl-paste-delay))
  (should (numberp agent-repl-paste-delay)))

(ert-deftest agent-repl-test-bug10-defvar-declarations ()
  "Bug 10: All key variables should be properly declared.
Note: agent-repl--notification-backend may not be bound in headless
environments without notification tools (terminal-notifier or osascript)."
  (should (boundp 'agent-repl--workspaces))
  (should (boundp 'agent-repl-hide-input-box))
  ;; notification-backend requires osascript or terminal-notifier at load time
  (when (or (executable-find "terminal-notifier") (executable-find "osascript"))
    (should (boundp 'agent-repl--notification-backend)))
  (should (boundp 'agent-repl--sync-timer)))

(ert-deftest agent-repl-test-bug11-fullscreen-config-stored-in-plist ()
  "Bug 11: fullscreen-config is per-workspace plist storage, not a global defvar."
  ;; The original test checked (boundp 'agent-repl--fullscreen-config), but that
  ;; symbol is never defvar'd.  The real storage is the :fullscreen-config plist
  ;; key accessed via ws-get/ws-put in panels.el.
  (agent-repl-test--with-clean-state
    (let ((ws-id "test-fc"))
      (puthash ws-id (list :status nil) agent-repl--workspaces)
      (agent-repl--ws-put ws-id :fullscreen-config 'some-config)
      (should (equal (agent-repl--ws-get ws-id :fullscreen-config) 'some-config)))))

(ert-deftest agent-repl-test-package-provide ()
  "Package should provide 'agent-repl feature."
  (should (featurep 'agent-repl)))

;;;; ---- Tests: Workspace-for-buffer ----

(ert-deftest agent-repl-test-workspace-for-buffer-no-persp ()
  "When `persp-mode' is nil, `workspace-for-buffer' should return nil."
  (let ((persp-mode nil))
    (should-not (agent-repl--workspace-for-buffer (current-buffer)))))

;;;; ---- Tests: Misc declared variables ----

(ert-deftest agent-repl-test-in-redraw-advice-declared-core ()
  "`agent-repl--in-redraw-advice' should be `boundp' (loaded via overlay.el)."
  ;; Renamed from agent-repl-test-in-redraw-advice-declared to avoid duplicate
  ;; with the canonical copy in test-overlay.el.
  (should (boundp 'agent-repl--in-redraw-advice)))

;;;; ---- Tests: cancel-all-timers ----

(ert-deftest agent-repl-test-cancel-all-timers-empty-list ()
  "Cancelling timers with an empty list should be a no-op."
  (let ((agent-repl--timers nil))
    (agent-repl--cancel-all-timers)
    (should (null agent-repl--timers))))

(ert-deftest agent-repl-test-cancel-all-timers-mix-valid-and-nil ()
  "Cancelling timers with mix of valid timers and nil entries should not error."
  (let* ((timer1 (run-with-timer 9999 nil #'ignore))
         (agent-repl--timers (list timer1 nil nil)))
    (agent-repl--cancel-all-timers)
    (should (null agent-repl--timers))))

(ert-deftest agent-repl-test-cancel-all-timers-already-cancelled ()
  "Cancelling already-cancelled timers should not error."
  (let* ((timer1 (run-with-timer 9999 nil #'ignore)))
    (cancel-timer timer1)
    (let ((agent-repl--timers (list timer1)))
      (agent-repl--cancel-all-timers)
      (should (null agent-repl--timers)))))

(ert-deftest agent-repl-test-cancel-all-timers-sets-nil ()
  "After cancellation, `agent-repl--timers' should be nil."
  (let* ((timer1 (run-with-timer 9999 nil #'ignore))
         (timer2 (run-with-timer 9999 nil #'ignore))
         (agent-repl--timers (list timer1 timer2)))
    (agent-repl--cancel-all-timers)
    (should (null agent-repl--timers))))

(ert-deftest agent-repl-test-cancel-all-timers-idempotent ()
  "Calling cancel-all-timers twice should be safe."
  (let* ((timer1 (run-with-timer 9999 nil #'ignore))
         (agent-repl--timers (list timer1)))
    (agent-repl--cancel-all-timers)
    (should (null agent-repl--timers))
    (agent-repl--cancel-all-timers)
    (should (null agent-repl--timers))))

;;;; ---- Tests: log-format ----

(ert-deftest agent-repl-test-log-format-empty-string ()
  "log-format with empty string should still include timestamp and tag."
  (let ((result (agent-repl--log-format nil "")))
    (should (string-match-p "\\[agent-repl\\]" result))
    (should (string-match-p "^[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\." result))))

(ert-deftest agent-repl-test-log-format-with-format-specifiers ()
  "log-format should pass format specifiers through literally, not expand them."
  (let ((result (agent-repl--log-format nil "hello %s %d")))
    (should (string-match-p "%s" result))
    (should (string-match-p "%d" result))))

(ert-deftest agent-repl-test-log-format-contains-timestamp-and-tag ()
  "log-format output should contain timestamp and [agent-repl] tag."
  (let ((result (agent-repl--log-format nil "test message")))
    (should (string-match-p "^[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\.[0-9]+" result))
    (should (string-match-p "\\[agent-repl\\]" result))
    (should (string-match-p "test message$" result))))

;;;; ---- Tests: do-log ----

(ert-deftest agent-repl-test-do-log-fmt-no-args ()
  "do-log with fmt and no args should call message with formatted prefix."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (agent-repl--do-log nil "simple message" nil)
      (should (string-match-p "\\[agent-repl\\] simple message" captured-msg)))))

(ert-deftest agent-repl-test-do-log-fmt-multiple-args ()
  "do-log with fmt and multiple args should expand them."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (agent-repl--do-log nil "hello %s and %d" '("world" 42))
      (should (string-match-p "hello world and 42" captured-msg)))))

(ert-deftest agent-repl-test-do-log-nil-args ()
  "do-log with nil args should work like no args."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (agent-repl--do-log nil "no args here" nil)
      (should (string-match-p "no args here" captured-msg)))))

(ert-deftest agent-repl-test-do-log-message-has-prefix ()
  "do-log should emit message with timestamp prefix."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (agent-repl--do-log nil "test" nil)
      (should (string-match-p "^[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\." captured-msg)))))

(ert-deftest agent-repl-test-do-log-error-p-signals-error ()
  "do-log with ERROR-P non-nil should signal `error' instead of calling `message'."
  (let ((message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _) (setq message-called t)))
              ((symbol-function 'agent-repl--do-log-to-file) #'ignore))
      (should-error (agent-repl--do-log nil "boom %s" '("reason") t) :type 'error)
      (should-not message-called))))

(ert-deftest agent-repl-test-do-log-error-p-includes-formatted-body ()
  "do-log with ERROR-P should include FMT expansion in the error's data."
  (cl-letf (((symbol-function 'agent-repl--do-log-to-file) #'ignore))
    (condition-case err
        (progn
          (agent-repl--do-log nil "thing failed: %s" '("why") t)
          (should nil))
      (error
       (should (string-match-p "thing failed: why" (error-message-string err)))))))

(ert-deftest agent-repl-test-do-log-error-p-writes-to-file-before-signalling ()
  "do-log with ERROR-P should write to the logfile before the error unwinds execution."
  (let ((file-write-called nil))
    (cl-letf (((symbol-function 'agent-repl--do-log-to-file)
               (lambda (_text) (setq file-write-called t))))
      (ignore-errors
        (agent-repl--do-log nil "boom" nil t))
      (should file-write-called))))

;;;; ---- Tests: log ----

(ert-deftest agent-repl-test-log-verbose-symbol-still-logs ()
  "`agent-repl--log' should log when debug is set to 'verbose (non-nil)."
  (let ((message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) (setq message-called t))))
      (let ((agent-repl-debug 'verbose))
        (agent-repl--log nil "test %s" "verbose")
        (should message-called)))))

(ert-deftest agent-repl-test-log-multiple-format-args ()
  "`agent-repl--log' should correctly expand multiple format arguments."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (let ((agent-repl-debug t))
        (agent-repl--log nil "a=%s b=%d c=%s" "x" 1 "z")
        (should (string-match-p "a=x b=1 c=z" captured-msg))))))

(ert-deftest agent-repl-test-log-bare-string ()
  "`agent-repl--log' with bare string (no format args) should work."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (let ((agent-repl-debug t))
        (agent-repl--log nil "bare message")
        (should (string-match-p "bare message" captured-msg))))))

(ert-deftest agent-repl-test-log-includes-timestamp ()
  "`agent-repl--log' output should include timestamp prefix."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (let ((agent-repl-debug t))
        (agent-repl--log nil "ts check")
        (should (string-match-p "^[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\." captured-msg))))))

;;;; ---- Tests: log-verbose ----

(ert-deftest agent-repl-test-log-verbose-nil-no-log ()
  "`agent-repl--log-verbose' should NOT log when debug is nil."
  (let ((message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) (setq message-called t))))
      (let ((agent-repl-debug nil))
        (agent-repl--log-verbose nil "test")
        (should-not message-called)))))

(ert-deftest agent-repl-test-log-verbose-t-no-log ()
  "`agent-repl--log-verbose' should NOT log when debug is t (only logs for 'verbose)."
  (let ((message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) (setq message-called t))))
      (let ((agent-repl-debug t))
        (agent-repl--log-verbose nil "test")
        (should-not message-called)))))

(ert-deftest agent-repl-test-log-verbose-verbose-logs ()
  "`agent-repl--log-verbose' should log when debug is 'verbose."
  (let ((message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) (setq message-called t))))
      (let ((agent-repl-debug 'verbose))
        (agent-repl--log-verbose nil "test")
        (should message-called)))))

;;;; ---- Tests: echo-area (modeline) severity gate ----
;;
;; The invariant under test: the log file and *Messages* are the QUIET sink
;; and take everything; the echo area / modeline is the LOUD sink and takes
;; only warnings and errors.  `inhibit-message' is what separates them — when
;; it is non-nil at the moment `message' runs, the line reaches *Messages*
;; but never the echo area.  So "did this line reach the modeline?" is
;; exactly "was `inhibit-message' nil inside the `message' call?".

(defun agent-repl-test--capture-emission (thunk)
  "Run THUNK with `message' stubbed and report what it emitted, and how loudly.
Returns a plist (:text TEXT :echoed BOOL :called BOOL), where :echoed is
non-nil only when `inhibit-message' was nil at `message' time — i.e. only
when the line actually reached the echo area / modeline.  File writes are
suppressed so the suite never touches the real logfile."
  (let ((text nil) (echoed nil) (called nil))
    (cl-letf (((symbol-function 'agent-repl--do-log-to-file) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq called t
                       text (apply #'format fmt args)
                       echoed (not inhibit-message)))))
      (funcall thunk))
    (list :text text :echoed echoed :called called)))

(ert-deftest agent-repl-test-emit-message-echo-nil-suppresses-echo-area ()
  "`agent-repl--emit-message' with ECHO nil binds `inhibit-message'."
  (let ((res (agent-repl-test--capture-emission
              (lambda () (agent-repl--emit-message "quiet line" nil)))))
    (should-not (plist-get res :echoed))))

(ert-deftest agent-repl-test-emit-message-echo-nil-still-reaches-messages ()
  "`agent-repl--emit-message' with ECHO nil still calls `message' (so *Messages* gets it)."
  (let ((res (agent-repl-test--capture-emission
              (lambda () (agent-repl--emit-message "quiet line" nil)))))
    (should (plist-get res :called))
    (should (equal "quiet line" (plist-get res :text)))))

(ert-deftest agent-repl-test-emit-message-echo-t-reaches-echo-area ()
  "`agent-repl--emit-message' with ECHO non-nil leaves `inhibit-message' unbound."
  (let ((res (agent-repl-test--capture-emission
              (lambda () (agent-repl--emit-message "loud line" t)))))
    (should (plist-get res :echoed))))

(ert-deftest agent-repl-test-info-never-reaches-echo-area ()
  "`agent-repl--info' is the quiet sink: it must NOT reach the echo area."
  (let ((res (agent-repl-test--capture-emission
              (lambda () (agent-repl--info nil "background chatter")))))
    (should (plist-get res :called))
    (should-not (plist-get res :echoed))))

(ert-deftest agent-repl-test-info-emits-when-debug-off ()
  "`agent-repl--info' is ungated by `agent-repl-debug' — it always records."
  (let* ((agent-repl-debug nil)
         (res (agent-repl-test--capture-emission
               (lambda () (agent-repl--info nil "recorded anyway")))))
    (should (plist-get res :called))
    (should (string-match-p "recorded anyway" (plist-get res :text)))))

(ert-deftest agent-repl-test-info-writes-to-file ()
  "`agent-repl--info' always writes to the logfile."
  (let ((written nil))
    (cl-letf (((symbol-function 'agent-repl--do-log-to-file)
               (lambda (text) (setq written text)))
              ((symbol-function 'message) #'ignore))
      (agent-repl--info nil "on the record")
      (should (string-match-p "on the record" written)))))

(ert-deftest agent-repl-test-warn-reaches-echo-area ()
  "`agent-repl--warn' is the loud sink: it MUST reach the echo area."
  (let ((res (agent-repl-test--capture-emission
              (lambda () (agent-repl--warn nil "something broke")))))
    (should (plist-get res :echoed))))

(ert-deftest agent-repl-test-warn-prepends-severity-tag ()
  "`agent-repl--warn' prepends a `WARNING: ' tag so call sites need not."
  (let ((res (agent-repl-test--capture-emission
              (lambda () (agent-repl--warn nil "disk on fire")))))
    (should (string-match-p "WARNING: disk on fire" (plist-get res :text)))))

(ert-deftest agent-repl-test-warn-expands-format-args ()
  "`agent-repl--warn' takes &rest ARGS and expands them into FMT."
  (let ((res (agent-repl-test--capture-emission
              (lambda () (agent-repl--warn nil "failed %s after %d tries" "sync" 3)))))
    (should (string-match-p "WARNING: failed sync after 3 tries" (plist-get res :text)))))

(ert-deftest agent-repl-test-warn-non-string-fmt-preserves-args ()
  "A non-string FMT is a caller bug: `agent-repl--warn' must route it to the
existing bug-capture path WITHOUT dropping ARGS on the floor."
  (let ((res (agent-repl-test--capture-emission
              (lambda ()
                (cl-letf (((symbol-function 'agent-repl--log-format-capture-bug) #'ignore))
                  (agent-repl--warn nil 'not-a-string "kept"))))))
    (should (string-match-p "BUG non-string-fmt" (plist-get res :text)))))

(ert-deftest agent-repl-test-warn-writes-to-file ()
  "`agent-repl--warn' always writes to the logfile."
  (let ((written nil))
    (cl-letf (((symbol-function 'agent-repl--do-log-to-file)
               (lambda (text) (setq written text)))
              ((symbol-function 'message) #'ignore))
      (agent-repl--warn nil "wrote this")
      (should (string-match-p "WARNING: wrote this" written)))))

(ert-deftest agent-repl-test-log-never-reaches-echo-area-when-debug-on ()
  "Turning debug logging on must not turn the modeline into a firehose."
  (let* ((agent-repl-debug t)
         (res (agent-repl-test--capture-emission
               (lambda () (agent-repl--log nil "debug chatter")))))
    (should (plist-get res :called))
    (should-not (plist-get res :echoed))))

(ert-deftest agent-repl-test-log-verbose-never-reaches-echo-area ()
  "Verbose hot-path chatter must not reach the echo area either."
  (let* ((agent-repl-debug 'verbose)
         (res (agent-repl-test--capture-emission
               (lambda () (agent-repl--log-verbose nil "hot path")))))
    (should (plist-get res :called))
    (should-not (plist-get res :echoed))))

(ert-deftest agent-repl-test-do-log-reaches-echo-area ()
  "`agent-repl--do-log' is loud by design — it must still reach the echo area."
  (let ((res (agent-repl-test--capture-emission
              (lambda () (agent-repl--do-log nil "invariant violated" nil)))))
    (should (plist-get res :echoed))))

(ert-deftest agent-repl-test-log-verbose-includes-timestamp ()
  "`agent-repl--log-verbose' output should include timestamp prefix."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (let ((agent-repl-debug 'verbose))
        (agent-repl--log-verbose nil "ts check")
        (should (string-match-p "^[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\." captured-msg))))))

;;;; ---- Tests: error ----

(ert-deftest agent-repl-test-error-signals-regardless-of-debug ()
  "`agent-repl--error' should signal even when `agent-repl-debug' is nil."
  (cl-letf (((symbol-function 'agent-repl--do-log-to-file) #'ignore))
    (let ((agent-repl-debug nil))
      (should-error (agent-repl--error nil "bad %s" "thing") :type 'error))))

(ert-deftest agent-repl-test-error-formats-fmt-and-args ()
  "`agent-repl--error' should expand FMT with ARGS in the error message."
  (cl-letf (((symbol-function 'agent-repl--do-log-to-file) #'ignore))
    (condition-case err
        (progn
          (agent-repl--error nil "ws=%s code=%d" "foo" 7)
          (should nil))
      (error
       (should (string-match-p "ws=foo code=7" (error-message-string err)))))))

(ert-deftest agent-repl-test-error-includes-agent-repl-tag ()
  "`agent-repl--error' output should include the [agent-repl] tag."
  (cl-letf (((symbol-function 'agent-repl--do-log-to-file) #'ignore))
    (condition-case err
        (progn
          (agent-repl--error nil "something")
          (should nil))
      (error
       (should (string-match-p "\\[agent-repl\\] something" (error-message-string err)))))))

;;;; ---- Tests: state-dir / state-file ----

(ert-deftest agent-repl-test-state-dir-uses-env-override ()
  "state-dir honors the AGENT_REPL_STATE_DIR override when set."
  (let ((process-environment (cons "AGENT_REPL_STATE_DIR=/tmp/statetest" process-environment)))
    (should (equal (agent-repl--global-state-dir)
                   (file-name-as-directory (expand-file-name "/tmp/statetest"))))))

(ert-deftest agent-repl-test-state-dir-falls-back-to-claude-emacs ()
  "state-dir falls back to ~/.claude-emacs/ when the override is unset."
  ;; A bare \"AGENT_REPL_STATE_DIR\" entry (no =) makes getenv return nil.
  (let ((process-environment (cons "AGENT_REPL_STATE_DIR" process-environment)))
    (should (equal (agent-repl--global-state-dir)
                   (file-name-as-directory (expand-file-name "~/.claude-emacs"))))))

(ert-deftest agent-repl-test-state-file-joins-under-state-dir ()
  "state-file returns RELATIVE joined under state-dir."
  (let ((process-environment (cons "AGENT_REPL_STATE_DIR=/tmp/statetest" process-environment)))
    (should (equal (agent-repl--global-state-file "x.el")
                   (expand-file-name "/tmp/statetest/x.el")))))

(ert-deftest agent-repl-test-state-file-empty-relative-yields-state-dir ()
  "An empty RELATIVE yields the state dir itself (used by the flatten migration)."
  (let ((process-environment (cons "AGENT_REPL_STATE_DIR=/tmp/statetest" process-environment)))
    (should (equal (agent-repl--global-state-file "")
                   (expand-file-name "/tmp/statetest")))))

;;;; ---- Tests: legacy-state migration ----

(ert-deftest agent-repl-test-migrate-moves-legacy-when-new-absent ()
  "Migration moves a legacy dir to its new location when new is absent."
  (let* ((tmp (make-temp-file "migtest-" t))
         (legacy (expand-file-name "legacy" tmp))
         (process-environment (cons (format "AGENT_REPL_STATE_DIR=%s" (expand-file-name "state" tmp))
                                    process-environment))
         (agent-repl--legacy-state-migrations (list (cons legacy "emacs"))))
    (unwind-protect
        (progn
          (make-directory legacy t)
          (with-temp-file (expand-file-name "f.el" legacy) (insert "data"))
          (agent-repl--migrate-legacy-state)
          (should-not (file-exists-p legacy))
          (should (file-exists-p (agent-repl--global-state-file "emacs/f.el"))))
      (delete-directory tmp t))))

(ert-deftest agent-repl-test-migrate-skips-when-new-exists ()
  "Migration never overwrites an existing new-location path."
  (let* ((tmp (make-temp-file "migtest2-" t))
         (legacy (expand-file-name "legacy" tmp))
         (process-environment (cons (format "AGENT_REPL_STATE_DIR=%s" (expand-file-name "state" tmp))
                                    process-environment))
         (agent-repl--legacy-state-migrations (list (cons legacy "emacs"))))
    (unwind-protect
        (progn
          (make-directory legacy t)
          (with-temp-file (expand-file-name "old.el" legacy) (insert "old"))
          (make-directory (agent-repl--global-state-file "emacs") t)
          (with-temp-file (agent-repl--global-state-file "emacs/new.el") (insert "new"))
          (agent-repl--migrate-legacy-state)
          (should (file-exists-p (expand-file-name "old.el" legacy)))
          (should-not (file-exists-p (agent-repl--global-state-file "emacs/old.el")))
          (should (file-exists-p (agent-repl--global-state-file "emacs/new.el"))))
      (delete-directory tmp t))))

(ert-deftest agent-repl-test-migrate-noop-when-legacy-absent ()
  "Migration is a no-op when the legacy path does not exist."
  (let* ((tmp (make-temp-file "migtest3-" t))
         (legacy (expand-file-name "nonexistent" tmp))
         (process-environment (cons (format "AGENT_REPL_STATE_DIR=%s" (expand-file-name "state" tmp))
                                    process-environment))
         (agent-repl--legacy-state-migrations (list (cons legacy "emacs"))))
    (unwind-protect
        (progn
          (agent-repl--migrate-legacy-state)
          (should-not (file-exists-p (agent-repl--global-state-file "emacs"))))
      (delete-directory tmp t))))

;;;; ---- Tests: log-to-file ----

(ert-deftest agent-repl-test-logfile-path-returns-state-dir-path ()
  "`agent-repl--logfile-path' should return the log under the state dir."
  (should (equal (agent-repl--logfile-path)
                 (agent-repl--global-state-file "doom-agent-repl.log"))))

(ert-deftest agent-repl-test-logfile-path-honors-defcustom ()
  "`agent-repl--logfile-path' should expand `agent-repl-log-file-name'."
  (let* ((tmpdir (make-temp-file "test-logpath-" t))
         (custom-path (expand-file-name "sub/custom.log" tmpdir))
         (agent-repl-log-file-name custom-path))
    (unwind-protect
        (should (equal (agent-repl--logfile-path) custom-path))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-do-log-to-file-writes-when-enabled ()
  "`agent-repl--do-log-to-file' should append text to the logfile."
  (let* ((tmpdir (make-temp-file "test-log-" t))
         (logpath (expand-file-name ".agent-repl.log" tmpdir))
         (agent-repl-log-to-file t))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--logfile-path) (lambda () logpath)))
          (agent-repl--do-log-to-file "first line")
          (agent-repl--do-log-to-file "second line")
          (let ((contents (with-temp-buffer
                            (insert-file-contents logpath)
                            (buffer-string))))
            (should (string-match-p "first line" contents))
            (should (string-match-p "second line" contents))))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-do-log-to-file-skips-when-disabled ()
  "`agent-repl--do-log-to-file' should not write when `agent-repl-log-to-file' is nil."
  (let* ((tmpdir (make-temp-file "test-log-" t))
         (logpath (expand-file-name ".agent-repl.log" tmpdir))
         (agent-repl-log-to-file nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--logfile-path) (lambda () logpath)))
          (agent-repl--do-log-to-file "should not appear")
          (should-not (file-exists-p logpath)))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-do-log-writes-to-file ()
  "`agent-repl--do-log' should write to the logfile when log-to-file is enabled."
  (let* ((tmpdir (make-temp-file "test-log-" t))
         (logpath (expand-file-name ".agent-repl.log" tmpdir))
         (agent-repl-log-to-file t)
         (agent-repl-debug t))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--logfile-path) (lambda () logpath))
                  ((symbol-function 'message) (lambda (&rest _args) nil)))
          (agent-repl--log nil "hello %s" "world")
          (let ((contents (with-temp-buffer
                            (insert-file-contents logpath)
                            (buffer-string))))
            (should (string-match-p "hello world" contents))
            (should (string-match-p "\\[agent-repl\\]" contents))))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-do-log-to-file-survives-write-error ()
  "`agent-repl--do-log-to-file' should not signal on write errors."
  (let ((agent-repl-log-to-file t))
    (cl-letf (((symbol-function 'agent-repl--logfile-path)
               (lambda () "/nonexistent/dir/impossible.log")))
      ;; Should not error
      (agent-repl--do-log-to-file "test"))))

;;;; ---- Tests: dir-has-git-p ----

(ert-deftest agent-repl-test-dir-has-git-p-with-git-dir ()
  "dir-has-git-p should return non-nil for directory with .git subdirectory."
  (let ((tmpdir (make-temp-file "test-git-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" tmpdir) t)
          (should (agent-repl--dir-has-git-p tmpdir)))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-dir-has-git-p-with-git-file ()
  "dir-has-git-p should return non-nil for directory with .git file (worktree)."
  (let ((tmpdir (make-temp-file "test-git-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".git" tmpdir)
            (insert "gitdir: /some/other/path"))
          (should (agent-repl--dir-has-git-p tmpdir)))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-dir-has-git-p-no-git ()
  "dir-has-git-p should return nil for directory without .git."
  (let ((tmpdir (make-temp-file "test-git-" t)))
    (unwind-protect
        (should-not (agent-repl--dir-has-git-p tmpdir))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-dir-has-git-p-nonexistent ()
  "dir-has-git-p should return nil for non-existent directory."
  (should-not (agent-repl--dir-has-git-p "/nonexistent/path/does/not/exist")))

(ert-deftest agent-repl-test-dir-has-git-p-nil ()
  "dir-has-git-p with nil should not error (expand-file-name handles nil)."
  ;; nil becomes default-directory; verify it doesn't crash.
  ;; No ignore-errors: if it signals, ERT correctly fails the test.
  (should (or (agent-repl--dir-has-git-p nil) t)))

(ert-deftest agent-repl-test-dir-has-git-p-empty-string ()
  "dir-has-git-p with empty string should not error."
  ;; No ignore-errors: if it signals, ERT correctly fails the test.
  (should (or (agent-repl--dir-has-git-p "") t)))

;;;; ---- Tests: git-root ----

(ert-deftest agent-repl-test-git-root-in-repo ()
  "git-root should return the repo root when called from within a git repo."
  (let ((tmpdir (make-temp-file "test-git-root-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" tmpdir) t)
          (let ((subdir (expand-file-name "a/b/c" tmpdir)))
            (make-directory subdir t)
            (let ((result (agent-repl--git-root subdir)))
              ;; Should find the tmpdir as root (it has .git)
              (should result)
              (should (string-match-p (regexp-quote (file-name-nondirectory tmpdir)) result)))))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-git-root-no-repo ()
  "git-root should return nil when called outside any git repo."
  ;; /tmp is very unlikely to be a git repo
  (let ((tmpdir (make-temp-file "test-no-repo-" t)))
    (unwind-protect
        ;; Stub dir-has-git-p to always return nil so we don't depend on host
        (cl-letf (((symbol-function 'agent-repl--dir-has-git-p) (lambda (_d) nil)))
          (should-not (agent-repl--git-root tmpdir)))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-git-root-uses-default-directory ()
  "git-root with no DIR arg should use `default-directory'."
  (let ((tmpdir (make-temp-file "test-git-dd-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" tmpdir) t)
          (let ((default-directory (file-name-as-directory tmpdir)))
            (let ((result (agent-repl--git-root)))
              (should result))))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-git-root-deeply-nested ()
  "git-root should find root from deeply nested subdirectory."
  (let ((tmpdir (make-temp-file "test-git-deep-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" tmpdir) t)
          (let ((deep (expand-file-name "a/b/c/d/e" tmpdir)))
            (make-directory deep t)
            (should (agent-repl--git-root deep))))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-git-root-explicit-dir ()
  "git-root with explicit DIR argument should search from that directory."
  (let ((tmpdir (make-temp-file "test-git-explicit-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" tmpdir) t)
          (should (agent-repl--git-root tmpdir)))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-git-root-worktree ()
  "git-root should find root when .git is a file (worktree)."
  (let ((tmpdir (make-temp-file "test-git-wt-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".git" tmpdir)
            (insert "gitdir: /some/other/.git/worktrees/foo"))
          (let ((subdir (expand-file-name "sub" tmpdir)))
            (make-directory subdir t)
            (should (agent-repl--git-root subdir))))
      (delete-directory tmpdir t))))

;;;; ---- Tests: git-string / git-string-quiet ----
;;
;; Intentionally none for the wrappers themselves.
;; `agent-repl--git-string' and `agent-repl--git-string-quiet' are
;; the external-boundary wrappers described by AGENTS.md "No External
;; Processes or External State in Tests".  Per that policy they are
;; not tested in isolation — testing them would require either
;; invoking real git (forbidden) or stubbing
;; `agent-repl--capture-process-output' (tautological).  Their
;; behavior is covered indirectly by the many callers that stub these
;; wrappers via `cl-letf'.
;;
;; The shared implementation, `agent-repl--capture-process-output',
;; IS tested below — it carries the worker-thread safety contract
;; (routes through `agent-repl--wait-for-process-exit' instead of
;; `shell-command-to-string') and the silent-on-timeout contract that
;; the quiet variants rely on, both of which are worth pinning.

;;;; ---- Tests: capture-process-output ----

(ert-deftest agent-repl-test-capture-process-output-returns-trimmed-buffer ()
  "On clean exit, `--capture-process-output' returns the stdout buffer's
contents with leading/trailing whitespace trimmed."
  (let ((captured-buf nil))
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name buf &rest _cmd)
                 (setq captured-buf buf)
                 (with-current-buffer buf
                   (insert "  trimmed result  \n"))
                 (list :fake-proc buf)))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--wait-for-process-exit)
               (lambda (&rest _) 0)))
      (should (equal (agent-repl--capture-process-output
                      "git" '("rev-parse" "HEAD"))
                     "trimmed result"))
      (should-not (buffer-live-p captured-buf)))))

(ert-deftest agent-repl-test-capture-process-output-returns-empty-on-timeout ()
  "On timeout, `--capture-process-output' returns an empty string.
This is load-bearing for the silent-failure contract of the `--quiet'
wrappers: init-time callers that may run outside a git repository
must not explode when git hangs or doesn't terminate in time."
  (cl-letf (((symbol-function 'start-process)
             (lambda (_name buf &rest _cmd)
               (with-current-buffer buf
                 (insert "partial output that should not be returned\n"))
               (list :fake-proc buf)))
            ((symbol-function 'set-process-query-on-exit-flag)
             (lambda (&rest _) nil))
            ((symbol-function 'agent-repl--wait-for-process-exit)
             (lambda (&rest _) 'timeout)))
    (should (equal (agent-repl--capture-process-output
                    "git" '("rev-parse" "HEAD"))
                   ""))))

(ert-deftest agent-repl-test-capture-process-output-logs-on-timeout ()
  "On timeout, `--capture-process-output' emits a log line naming the
stalled program and args so the otherwise-silent \"\" return leaves a
post-mortem breadcrumb."
  (let ((logged nil))
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name buf &rest _cmd) (list :fake-proc buf)))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--wait-for-process-exit)
               (lambda (&rest _) 'timeout))
              ((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args)
                 (setq logged (apply #'format fmt args)))))
      (agent-repl--capture-process-output "git" '("fetch" "origin"))
      (should (string-match-p "TIMEOUT" logged))
      (should (string-match-p "git" logged))
      (should (string-match-p "fetch" logged)))))

(ert-deftest agent-repl-test-capture-process-output-uses-make-process-when-suppress-stderr ()
  "When SUPPRESS-STDERR is non-nil, `--capture-process-output' uses
`make-process' with `:stderr' set to a separate buffer so stderr is
discarded — matches the `2>/dev/null' contract the quiet wrappers
depend on."
  (let ((make-process-called nil)
        (start-process-called nil)
        (stderr-buf-arg nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq make-process-called t
                       stderr-buf-arg (plist-get args :stderr))
                 (list :fake-proc (plist-get args :buffer))))
              ((symbol-function 'start-process)
               (lambda (&rest _)
                 (setq start-process-called t)
                 (list :fake-proc nil)))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--wait-for-process-exit)
               (lambda (&rest _) 0)))
      (agent-repl--capture-process-output "git" '("status") t))
    (should make-process-called)
    (should-not start-process-called)
    (should (bufferp stderr-buf-arg))))

(ert-deftest agent-repl-test-capture-process-output-uses-start-process-when-no-suppress-stderr ()
  "When SUPPRESS-STDERR is nil (default), `--capture-process-output' uses
`start-process', which merges stderr into the same buffer as stdout
\(matches `shell-command-to-string''s default and the existing
`--git-string' contract that includes stderr in the returned text)."
  (let ((make-process-called nil)
        (start-process-called nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _)
                 (setq make-process-called t)
                 (list :fake-proc nil)))
              ((symbol-function 'start-process)
               (lambda (_name buf &rest _cmd)
                 (setq start-process-called t)
                 (list :fake-proc buf)))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--wait-for-process-exit)
               (lambda (&rest _) 0)))
      (agent-repl--capture-process-output "git" '("status") nil))
    (should start-process-called)
    (should-not make-process-called)))

(ert-deftest agent-repl-test-capture-process-output-kills-stderr-buffer ()
  "The temporary stderr buffer (when SUPPRESS-STDERR is non-nil) is killed
before return so we don't accumulate hidden buffers across many git calls."
  (let ((stderr-buf-arg nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq stderr-buf-arg (plist-get args :stderr))
                 (list :fake-proc (plist-get args :buffer))))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--wait-for-process-exit)
               (lambda (&rest _) 0)))
      (agent-repl--capture-process-output "git" '("status") t))
    (should stderr-buf-arg)
    (should-not (buffer-live-p stderr-buf-arg))))

;;;; ---- Tests: print-git-branch ----

(ert-deftest agent-repl-test-print-git-branch-message ()
  "print-git-branch should include the git branch value in its message."
  ;; Reset the lazy cache so the cl-letf'd boundary wrapper actually fires
  ;; (otherwise a populated `agent-repl-git-branch' from a prior session
  ;; or earlier test short-circuits the only call this test exercises).
  (let ((agent-repl-git-branch nil)
        (captured-msg nil))
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _args) "test-branch"))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (agent-repl-print-git-branch)
      (should (stringp captured-msg))
      (should (string-match-p (regexp-quote agent-repl-git-branch) captured-msg)))))

;;;; ---- Tests: path-canonical ----

(ert-deftest agent-repl-test-path-canonical-trailing-slash ()
  "path-canonical should strip trailing slash."
  (let ((result (agent-repl--path-canonical "/tmp/foo/")))
    (should-not (string-suffix-p "/" result))))

(ert-deftest agent-repl-test-path-canonical-no-trailing-slash ()
  "path-canonical without trailing slash should remain unchanged (modulo truename)."
  (let ((result (agent-repl--path-canonical "/tmp")))
    (should-not (string-suffix-p "/" result))))

(ert-deftest agent-repl-test-path-canonical-tilde ()
  "path-canonical should expand tilde."
  (let ((result (agent-repl--path-canonical "~")))
    (should-not (string-prefix-p "~" result))
    (should (string-prefix-p "/" result))))

(ert-deftest agent-repl-test-path-canonical-relative-path ()
  "path-canonical should expand relative paths."
  (let ((result (agent-repl--path-canonical ".")))
    (should (string-prefix-p "/" result))))

(ert-deftest agent-repl-test-path-canonical-root ()
  "path-canonical for / should return /."
  ;; directory-file-name of "/" is "" on some systems, but file-truename "/" is "/"
  ;; and directory-file-name "/" is "/"; this just verifies no crash.
  (let ((result (agent-repl--path-canonical "/")))
    (should (stringp result))))

(ert-deftest agent-repl-test-path-canonical-symlink ()
  "path-canonical should resolve symlinks to true path."
  (let ((tmpdir (make-temp-file "test-sym-" t)))
    (unwind-protect
        (let* ((real-dir (expand-file-name "real" tmpdir))
               (link-dir (expand-file-name "link" tmpdir)))
          (make-directory real-dir t)
          (make-symbolic-link real-dir link-dir)
          (let ((result (agent-repl--path-canonical link-dir)))
            ;; Should resolve to the real path
            (should (string-match-p "real" result))
            (should-not (string-match-p "link" result))))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-path-canonical-empty-string ()
  "path-canonical with empty string should not error."
  (let ((result (agent-repl--path-canonical "")))
    (should (stringp result))))

;;;; ---- Tests: workspace-id ----

(ert-deftest agent-repl-test-workspace-id-nil-when-no-ws-dir ()
  "workspace-id should return nil when no workspace has a :project-dir."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
            ((symbol-function 'agent-repl--ws-dir)
             (lambda (_ws) (error "no dir"))))
    (should-not (agent-repl--workspace-id))))

(ert-deftest agent-repl-test-workspace-id-hash-length ()
  "workspace-id should return exactly 8 characters."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
            ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/test/project")))
    (let ((id (agent-repl--workspace-id)))
      (should (= (length id) 8)))))

(ert-deftest agent-repl-test-workspace-id-different-roots ()
  "Two different roots should produce different IDs."
  (let (id1 id2)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/path/one")))
      (setq id1 (agent-repl--workspace-id)))
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws2"))
              ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/path/two")))
      (setq id2 (agent-repl--workspace-id)))
    (should-not (equal id1 id2))))

(ert-deftest agent-repl-test-workspace-id-deterministic ()
  "Same root should always produce the same ID."
  (let (id1 id2)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/stable/path")))
      (setq id1 (agent-repl--workspace-id))
      (setq id2 (agent-repl--workspace-id)))
    (should (equal id1 id2))))

;;;; ---- Tests: create-buffer ----

(ert-deftest agent-repl-test-create-buffer-vterm-name ()
  "create-buffer with no suffix produces the vterm buffer name."
  (let ((buf nil))
    (unwind-protect
        (progn
          (setq buf (agent-repl--create-buffer "ws1"))
          (should (buffer-live-p buf))
          (should (equal (buffer-name buf) "*agent-panel-ws1*")))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-create-buffer-input-suffix ()
  "create-buffer with \"-input\" suffix produces the input buffer name."
  (let ((buf nil))
    (unwind-protect
        (progn
          (setq buf (agent-repl--create-buffer "ws1" "-input"))
          (should (equal (buffer-name buf) "*agent-panel-input-ws1*")))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-create-buffer-sets-owning-workspace ()
  "create-buffer sets `agent-repl--owning-workspace' buffer-locally."
  (let ((buf nil))
    (unwind-protect
        (progn
          (setq buf (agent-repl--create-buffer "ws1"))
          (should (equal (buffer-local-value 'agent-repl--owning-workspace buf)
                         "ws1")))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-create-buffer-owning-workspace-survives-mode ()
  "`agent-repl--owning-workspace' survives `kill-all-local-variables'.
Major-mode activation (vterm-mode, agent-repl-input-mode) wipes buffer-local
bindings; the permanent-local property on this variable is what keeps
ownership intact across that transition."
  (let ((buf nil))
    (unwind-protect
        (progn
          (setq buf (agent-repl--create-buffer "ws1"))
          (with-current-buffer buf
            (kill-all-local-variables))
          (should (equal (buffer-local-value 'agent-repl--owning-workspace buf)
                         "ws1")))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-create-buffer-attaches-to-persp ()
  "create-buffer adds the buffer to WS's perspective when it exists."
  (let ((buf nil)
        (added nil))
    (unwind-protect
        (cl-letf (((symbol-function 'persp-get-by-name)
                   (lambda (_name) 'fake-persp))
                  ((symbol-function 'persp-add-buffer)
                   (lambda (b persp &rest _)
                     (setq added (list b persp)))))
          (setq buf (agent-repl--create-buffer "ws1"))
          (should (equal added (list buf 'fake-persp))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-create-buffer-skips-persp-when-not-found ()
  "create-buffer does not error when no perspective named WS exists."
  (let ((buf nil)
        (add-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_name) nil))
                  ((symbol-function 'persp-add-buffer)
                   (lambda (&rest _) (setq add-called t))))
          (setq buf (agent-repl--create-buffer "ws1"))
          (should-not add-called))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-create-buffer-errors-when-ws-nil ()
  "create-buffer signals an error when WS is nil and no current workspace."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
    (should-error (agent-repl--create-buffer nil) :type 'error)))

(ert-deftest agent-repl-test-create-buffer-idempotent ()
  "Calling create-buffer twice with the same args reuses the buffer."
  (let ((first nil))
    (unwind-protect
        (let* ((_ (setq first (agent-repl--create-buffer "ws1")))
               (second (agent-repl--create-buffer "ws1")))
          (should (eq first second)))
      (when (buffer-live-p first) (kill-buffer first)))))

;;;; ---- Tests: active-inst ----

(ert-deftest agent-repl-test-active-inst-default-bare-metal ()
  "active-inst should error when no :active-env is set (initialize-ws-env not called)."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--active-inst "ws1") :type 'error)))

;;;; ---- Tests: ws-durable-claude-session-id ----

(ert-deftest agent-repl-test-durable-session-id-returns-recorded-uuid ()
  "ws-durable-claude-session-id reads the active instantiation's uuid."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :active-env :bare-metal)
    (agent-repl--ws-put "ws1" :bare-metal
                        (make-agent-repl-instantiation :session-id "cli-uuid-1"))
    (should (equal (agent-repl--ws-durable-claude-session-id "ws1") "cli-uuid-1"))))

(ert-deftest agent-repl-test-durable-session-id-nil-without-active-env ()
  "A workspace that never initialized an env has no durable id (nil, no signal)."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--ws-durable-claude-session-id "ws1"))))

(ert-deftest agent-repl-test-durable-session-id-nil-without-instantiation ()
  "An :active-env with no instantiation struct yields nil, not a signal."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :active-env :sandbox)
    (should-not (agent-repl--ws-durable-claude-session-id "ws1"))))

(ert-deftest agent-repl-test-durable-session-id-nil-when-never-ran ()
  "An instantiation that never captured a session id yields nil."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :active-env :sandbox)
    (agent-repl--ws-put "ws1" :sandbox (make-agent-repl-instantiation))
    (should-not (agent-repl--ws-durable-claude-session-id "ws1"))))

(ert-deftest agent-repl-test-active-inst-sandbox-env ()
  "active-inst should use :sandbox when :active-env is set to :sandbox."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :active-env :sandbox)
    (agent-repl--ws-put "ws1" :sandbox (make-agent-repl-instantiation))
    (let ((inst (agent-repl--active-inst "ws1")))
      (should (agent-repl-instantiation-p inst))
      (should (equal (agent-repl--ws-get "ws1" :sandbox) inst)))))

(ert-deftest agent-repl-test-active-inst-returns-same-struct ()
  "active-inst should return the same struct on second call (not create a new one)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :active-env :bare-metal)
    (agent-repl--ws-put "ws1" :bare-metal (make-agent-repl-instantiation))
    (let ((inst1 (agent-repl--active-inst "ws1"))
          (inst2 (agent-repl--active-inst "ws1")))
      (should (eq inst1 inst2)))))

(ert-deftest agent-repl-test-active-inst-is-struct ()
  "active-inst should return a agent-repl-instantiation struct."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :active-env :bare-metal)
    (agent-repl--ws-put "ws1" :bare-metal (make-agent-repl-instantiation))
    (let ((inst (agent-repl--active-inst "ws1")))
      (should (agent-repl-instantiation-p inst)))))

(ert-deftest agent-repl-test-active-inst-fields-nil-by-default ()
  "active-inst struct fields should be nil by default."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :active-env :bare-metal)
    (agent-repl--ws-put "ws1" :bare-metal (make-agent-repl-instantiation))
    (let ((inst (agent-repl--active-inst "ws1")))
      (should-not (agent-repl-instantiation-session-id inst))
      (should-not (agent-repl-instantiation-start-cmd inst)))))

;;;; ---- Tests: buffer-name edge cases ----

(ert-deftest agent-repl-test-buffer-name-empty-suffix ()
  "Buffer name with empty string suffix should work like no suffix."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "abcd1234")))
    (should (equal (agent-repl--buffer-name "") "*agent-panel-abcd1234*"))))

(ert-deftest agent-repl-test-buffer-name-various-suffixes ()
  "Buffer name with various suffix values should include them."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "abcd1234")))
    (should (equal (agent-repl--buffer-name "-debug") "*agent-panel-debug-abcd1234*"))
    (should (equal (agent-repl--buffer-name "-log") "*agent-panel-log-abcd1234*"))))

(ert-deftest agent-repl-test-buffer-name-matches-regexps ()
  "Buffer names should match their respective regexp patterns.
Use `agent-repl--agent-buffer-p' for the vterm-vs-input distinction —
`agent-repl--vterm-buffer-re' is a superset that also matches input names."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "abcd1234")))
    (let ((vterm-name (agent-repl--buffer-name))
          (input-name (agent-repl--buffer-name "-input")))
      (should (string-match-p agent-repl--vterm-buffer-re vterm-name))
      (should (string-match-p agent-repl--input-buffer-re input-name))
      ;; Vterm name does NOT match input-re; predicate on vterm name is true.
      (should-not (string-match-p agent-repl--input-buffer-re vterm-name))
      (with-temp-buffer
        (rename-buffer vterm-name t)
        (should (agent-repl--agent-buffer-p)))
      ;; Input name matches vterm-re (superset), but the predicate correctly
      ;; excludes it.
      (should (string-match-p agent-repl--vterm-buffer-re input-name))
      (with-temp-buffer
        (rename-buffer input-name t)
        (should-not (agent-repl--agent-buffer-p))))))

;;;; ---- Tests: agent-buffer-p edge cases ----

(ert-deftest agent-repl-test-agent-buffer-p-no-hash ()
  "agent-buffer-p should not match *agent-panel-* without hex chars."
  (agent-repl-test--with-temp-buffer "*agent-panel-*"
    (should-not (agent-repl--agent-buffer-p))))

(ert-deftest agent-repl-test-agent-buffer-p-extra-after-pattern ()
  "agent-buffer-p should not match buffer with extra characters after pattern."
  (agent-repl-test--with-temp-buffer "*agent-panel-abcd1234*extra"
    (should-not (agent-repl--agent-buffer-p))))

(ert-deftest agent-repl-test-agent-buffer-p-nil-uses-current ()
  "agent-buffer-p with nil should use current buffer."
  (agent-repl-test--with-temp-buffer "*agent-panel-abcd1234*"
    (should (agent-repl--agent-buffer-p nil))))

(ert-deftest agent-repl-test-agent-buffer-p-explicit-buffer ()
  "agent-buffer-p with explicit buffer argument."
  (agent-repl-test--with-temp-buffer "*agent-panel-abcd1234*"
    (let ((buf (current-buffer)))
      (agent-repl-test--with-temp-buffer "*scratch-test*"
        ;; Current buffer is *scratch-test*, but pass buf explicitly
        (should (agent-repl--agent-buffer-p buf))))))

;;;; ---- Tests: agent-panel-buffer-p ----

(ert-deftest agent-repl-test-agent-panel-buffer-p-vterm ()
  "agent-panel-buffer-p should match vterm buffer names."
  (agent-repl-test--with-temp-buffer "*agent-panel-abcd1234*"
    (should (agent-repl--agent-panel-buffer-p))))

(ert-deftest agent-repl-test-agent-panel-buffer-p-input ()
  "agent-panel-buffer-p should match input buffer names."
  (agent-repl-test--with-temp-buffer "*agent-panel-input-abcd1234*"
    (should (agent-repl--agent-panel-buffer-p))))

(ert-deftest agent-repl-test-agent-panel-buffer-p-regular ()
  "agent-panel-buffer-p should not match regular buffer names."
  (agent-repl-test--with-temp-buffer "*scratch*"
    (should-not (agent-repl--agent-panel-buffer-p))))

(ert-deftest agent-repl-test-agent-panel-buffer-p-nil ()
  "agent-panel-buffer-p with nil should use current buffer."
  (agent-repl-test--with-temp-buffer "*agent-panel-input-abcd1234*"
    (should (agent-repl--agent-panel-buffer-p nil))))

;;;; ---- Tests: non-user-buffer-p ----

(ert-deftest agent-repl-test-non-user-buffer-p-agent-panel ()
  "non-user-buffer-p should return non-nil for claude panel buffer."
  (agent-repl-test--with-temp-buffer "*agent-panel-abcd1234*"
    (should (agent-repl--non-user-buffer-p (current-buffer)))))

(ert-deftest agent-repl-test-non-user-buffer-p-minibuffer ()
  "non-user-buffer-p should return non-nil for minibuffer-like names."
  (agent-repl-test--with-temp-buffer " *Minibuf-0*"
    (should (agent-repl--non-user-buffer-p (current-buffer)))))

(ert-deftest agent-repl-test-non-user-buffer-p-dead-buffer ()
  "non-user-buffer-p should return non-nil for dead/killed buffer."
  (let ((buf (get-buffer-create " *test-dead*")))
    (kill-buffer buf)
    (should (agent-repl--non-user-buffer-p buf))))

(ert-deftest agent-repl-test-non-user-buffer-p-nil ()
  "non-user-buffer-p should return non-nil for nil input."
  (should (agent-repl--non-user-buffer-p nil)))

(ert-deftest agent-repl-test-non-user-buffer-p-string-nonexistent ()
  "non-user-buffer-p should return non-nil for string name of non-existent buffer."
  (should (agent-repl--non-user-buffer-p "nonexistent-buffer-name-99999")))

(ert-deftest agent-repl-test-non-user-buffer-p-string-existing ()
  "non-user-buffer-p should return nil for string name of existing normal buffer."
  (agent-repl-test--with-temp-buffer "*normal-test-buf*"
    (should-not (agent-repl--non-user-buffer-p "*normal-test-buf*"))))

(ert-deftest agent-repl-test-non-user-buffer-p-normal-buffer ()
  "non-user-buffer-p should return nil for a normal live buffer."
  (agent-repl-test--with-temp-buffer "*normal-test-buf2*"
    (should-not (agent-repl--non-user-buffer-p (current-buffer)))))

;;;; ---- Tests: non-agent-buffers ----

(ert-deftest agent-repl-test-non-agent-buffers-empty-list ()
  "non-agent-buffers with empty list should return empty list."
  (should (null (agent-repl--non-agent-buffers nil))))

(ert-deftest agent-repl-test-non-agent-buffers-all-claude ()
  "non-agent-buffers with all claude buffers should return empty list."
  (agent-repl-test--with-temp-buffer "*agent-panel-aaaa1111*"
    (let ((buf1 (current-buffer)))
      (agent-repl-test--with-temp-buffer "*agent-panel-input-bbbb2222*"
        (let ((buf2 (current-buffer)))
          (should (null (agent-repl--non-agent-buffers (list buf1 buf2)))))))))

(ert-deftest agent-repl-test-non-agent-buffers-no-agent ()
  "non-agent-buffers with no claude buffers should return all."
  (agent-repl-test--with-temp-buffer "*normal-a*"
    (let ((buf1 (current-buffer)))
      (agent-repl-test--with-temp-buffer "*normal-b*"
        (let ((buf2 (current-buffer)))
          (let ((result (agent-repl--non-agent-buffers (list buf1 buf2))))
            (should (= (length result) 2))))))))

(ert-deftest agent-repl-test-non-agent-buffers-mixed ()
  "non-agent-buffers with mixed list should filter correctly."
  (agent-repl-test--with-temp-buffer "*agent-panel-aaaa1111*"
    (let ((agent-buf (current-buffer)))
      (agent-repl-test--with-temp-buffer "*normal-buf*"
        (let ((normal-buf (current-buffer)))
          (let ((result (agent-repl--non-agent-buffers (list agent-buf normal-buf))))
            (should (= (length result) 1))
            (should (eq (car result) normal-buf))))))))

(ert-deftest agent-repl-test-non-agent-buffers-nil-entries ()
  "non-agent-buffers should filter out nil entries."
  (agent-repl-test--with-temp-buffer "*normal-c*"
    (let ((buf (current-buffer)))
      (let ((result (agent-repl--non-agent-buffers (list nil buf nil))))
        (should (= (length result) 1))
        (should (eq (car result) buf))))))

(ert-deftest agent-repl-test-non-agent-buffers-string-names ()
  "non-agent-buffers should handle string names (non-existent buffers are filtered)."
  (agent-repl-test--with-temp-buffer "*normal-str*"
    ;; String names of non-existent buffers should be filtered (non-user-buffer-p returns t)
    (let ((result (agent-repl--non-agent-buffers (list "*normal-str*" "nonexistent-xyz"))))
      (should (= (length result) 1))
      (should (equal (car result) "*normal-str*")))))

;;;; ---- Tests: current-ws-p ----

(ert-deftest agent-repl-test-current-ws-p-match ()
  "current-ws-p should return non-nil when WS matches current workspace."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws")))
    (should (agent-repl--current-ws-p "my-ws"))))

(ert-deftest agent-repl-test-current-ws-p-no-match ()
  "current-ws-p should return nil when WS does not match."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws")))
    (should-not (agent-repl--current-ws-p "other-ws"))))

(ert-deftest agent-repl-test-current-ws-p-empty-string ()
  "current-ws-p with empty string should not match non-empty workspace."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws")))
    (should-not (agent-repl--current-ws-p ""))))

;;;; ---- Tests: current-ws-live-vterm ----

(ert-deftest agent-repl-test-current-ws-live-vterm-no-buffer ()
  "current-ws-live-vterm should return nil when no vterm buffer is stored."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (should-not (agent-repl--current-ws-live-vterm)))))

(ert-deftest agent-repl-test-current-ws-live-vterm-dead-buffer ()
  "current-ws-live-vterm should return nil for a killed buffer."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (let ((buf (get-buffer-create " *test-dead-vterm*")))
        (agent-repl--ws-put "ws1" :vterm-buffer buf)
        (kill-buffer buf)
        (should-not (agent-repl--current-ws-live-vterm))))))

(ert-deftest agent-repl-test-current-ws-live-vterm-live-buffer ()
  "current-ws-live-vterm should return the buffer when live."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-live-vterm*"
      (let ((buf (current-buffer)))
        (agent-repl--ws-put "ws1" :vterm-buffer buf)
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
          (should (eq (agent-repl--current-ws-live-vterm) buf)))))))

;;;; ---- Tests: vterm-live-p edge case ----

(ert-deftest agent-repl-test-vterm-live-p-nil-explicit ()
  "vterm-live-p should return nil when buffer is explicitly set to nil."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (agent-repl--ws-put "ws1" :vterm-buffer nil)
      (should-not (agent-repl--vterm-live-p)))))

;;;; ---- Tests: with-vterm-buf macro ----

(ert-deftest agent-repl-test-with-vterm-buf-no-live-buffer ()
  "with-vterm-buf should return nil when no live vterm buffer exists."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (should-not (agent-repl--with-vterm-buf 'should-not-reach)))))

(ert-deftest agent-repl-test-with-vterm-buf-live-buffer ()
  "with-vterm-buf should execute body when live vterm buffer exists."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-vterm-macro*"
      (let ((buf (current-buffer)))
        (agent-repl--ws-put "ws1" :vterm-buffer buf)
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
          (let ((executed nil))
            (agent-repl--with-vterm-buf
              (setq executed t))
            (should executed)))))))

(ert-deftest agent-repl-test-with-vterm-buf-binds-vterm-buf ()
  "with-vterm-buf should bind `vterm-buf' to the live buffer."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-vterm-bind*"
      (let ((buf (current-buffer)))
        (agent-repl--ws-put "ws1" :vterm-buffer buf)
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
          (should (eq (agent-repl--with-vterm-buf vterm-buf) buf)))))))

(ert-deftest agent-repl-test-with-vterm-buf-returns-value ()
  "with-vterm-buf should return the value of the body."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-vterm-ret*"
      (let ((buf (current-buffer)))
        (agent-repl--ws-put "ws1" :vterm-buffer buf)
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
          (should (equal (agent-repl--with-vterm-buf 42) 42)))))))

;;;; ---- Tests: instantiation struct ----

(ert-deftest agent-repl-test-instantiation-create ()
  "make-agent-repl-instantiation should create a struct."
  (let ((inst (make-agent-repl-instantiation)))
    (should (agent-repl-instantiation-p inst))))

(ert-deftest agent-repl-test-instantiation-default-fields ()
  "Instantiation struct fields should default to nil."
  (let ((inst (make-agent-repl-instantiation)))
    (should-not (agent-repl-instantiation-session-id inst))
    (should-not (agent-repl-instantiation-start-cmd inst))))

(ert-deftest agent-repl-test-instantiation-setf ()
  "Instantiation struct fields should be modifiable with setf."
  (let ((inst (make-agent-repl-instantiation)))
    (setf (agent-repl-instantiation-session-id inst) "sess-123")
    (setf (agent-repl-instantiation-start-cmd inst) "claude --resume")
    (should (equal (agent-repl-instantiation-session-id inst) "sess-123"))
    (should (equal (agent-repl-instantiation-start-cmd inst) "claude --resume"))))

;;;; ---- Tests: defvar/defcustom declarations ----

(ert-deftest agent-repl-test-timers-var-bound ()
  "`agent-repl--timers' should be bound."
  (should (boundp 'agent-repl--timers)))

(ert-deftest agent-repl-test-debug-default-nil ()
  "`agent-repl-debug' should default to nil."
  (should (boundp 'agent-repl-debug))
  ;; Note: default-value because tests may let-bind it
  (should-not (default-value 'agent-repl-debug)))

(ert-deftest agent-repl-test-vterm-buffer-re-matches ()
  "`agent-repl--vterm-buffer-re' matches vterm and input names (superset by design).
Callers that need a vterm-only check must use `agent-repl--agent-buffer-p'."
  (should (string-match-p agent-repl--vterm-buffer-re "*agent-panel-abcd1234*"))
  (should (string-match-p agent-repl--vterm-buffer-re "*agent-panel-my-workspace*"))
  ;; Vterm-re intentionally also matches input buffers (workspace names can
  ;; contain hyphens, so the regex can't cheaply exclude "input-*").
  (should (string-match-p agent-repl--vterm-buffer-re "*agent-panel-input-abcd1234*"))
  (should-not (string-match-p agent-repl--vterm-buffer-re "*scratch*")))

(ert-deftest agent-repl-test-input-buffer-re-matches ()
  "`agent-repl--input-buffer-re' should match expected input buffer patterns."
  (should (string-match-p agent-repl--input-buffer-re "*agent-panel-input-abcd1234*"))
  (should (string-match-p agent-repl--input-buffer-re "*agent-panel-input-my-workspace*"))
  (should-not (string-match-p agent-repl--input-buffer-re "*agent-panel-abcd1234*"))
  (should-not (string-match-p agent-repl--input-buffer-re "*scratch*")))

;;;; ---- Tests: log-format hardening against non-string fmt ----

(ert-deftest agent-repl-test-log-format-tolerates-symbol-fmt ()
  "`agent-repl--log-format' must not crash when FMT is a symbol.
Regression guard for callers that pass a file-notify action symbol by mistake."
  (let ((agent-repl--log-format-bug-captured t)) ; suppress capture side-effect
    (let ((result (agent-repl--log-format nil 'stopped)))
      (should (stringp result))
      (should (string-match-p "BUG non-string-fmt=stopped" result)))))

(ert-deftest agent-repl-test-log-format-captures-backtrace-once ()
  "Non-string FMT should capture a backtrace into *agent-repl-log-bug* only once."
  (let ((agent-repl--log-format-bug-captured nil))
    (unwind-protect
        (progn
          (when (get-buffer "*agent-repl-log-bug*")
            (kill-buffer "*agent-repl-log-bug*"))
          (agent-repl--log-format nil 'stopped)
          (should (get-buffer "*agent-repl-log-bug*"))
          (should agent-repl--log-format-bug-captured)
          (let ((size (buffer-size (get-buffer "*agent-repl-log-bug*"))))
            (agent-repl--log-format nil 'changed)
            ;; Second call should NOT add more content.
            (should (= size (buffer-size (get-buffer "*agent-repl-log-bug*"))))))
      (when (get-buffer "*agent-repl-log-bug*")
        (kill-buffer "*agent-repl-log-bug*")))))

(ert-deftest agent-repl-test-do-log-survives-percent-in-metadata ()
  "`agent-repl--do-log' must not raise arity errors when workspace metadata
contains a literal `%' character.  Regression for \"Not enough arguments for
format string\" seen when running `agent-repl-reset-sentinel-watchers'."
  (let ((agent-repl-debug t))
    (cl-letf (((symbol-function 'agent-repl--format-ws-metadata)
               (lambda (_ws) " {dir=/path/with/%s/literal}"))
              ((symbol-function 'message) #'ignore))
      ;; Should complete without signaling a format-string arity error.
      (agent-repl--log nil "plain message, no specifiers")
      (agent-repl--log nil "one-specifier=%s" "value")
      ;; Non-string fmt path should also be safe.
      (agent-repl--log nil 'stopped)
      (should t))))

;;;; ---- Tests: file-write decoupled from debug gate ----

(defmacro agent-repl-test--with-temp-logfile (sym &rest body)
  "Bind a fresh per-test logfile path to SYM and route agent-repl writes to it.
The temp file is deleted after BODY runs.  `agent-repl-log-to-file' is
forced on inside BODY (test-helpers globally turns it off to keep other
tests pollution-free), and the write counter is reset so size-cap tests
do not pick up state from earlier tests."
  (declare (indent 1))
  `(let ((,sym (make-temp-file "agent-repl-test-log-")))
     (unwind-protect
         (let ((agent-repl-log-to-file t)
               (agent-repl-log-file-name ,sym)
               (agent-repl--log-write-counter 0))
           ,@body)
       (when (file-exists-p ,sym) (delete-file ,sym))
       (when (file-exists-p (concat ,sym ".prev"))
         (delete-file (concat ,sym ".prev")))
       (when (file-exists-p (concat ,sym ".trunc-tmp"))
         (delete-file (concat ,sym ".trunc-tmp"))))))

(ert-deftest agent-repl-test-log-always-writes-file-when-debug-off ()
  "`agent-repl--log' must write to file even when `agent-repl-debug' is nil.
This is the core decoupling guarantee — the file is the canonical
record; `agent-repl-debug' now only gates the *Messages* emit."
  (agent-repl-test--with-temp-logfile path
    (cl-letf (((symbol-function 'message) #'ignore))
      (let ((agent-repl-debug nil))
        (agent-repl--log nil "debug-off line")
        (should (file-exists-p path))
        (with-temp-buffer
          (insert-file-contents path)
          (should (string-match-p "debug-off line" (buffer-string))))))))

(ert-deftest agent-repl-test-log-still-suppresses-message-when-debug-off ()
  "`agent-repl--log' must NOT call `message' when `agent-repl-debug' is nil."
  (agent-repl-test--with-temp-logfile _
    (let ((message-called nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest _) (setq message-called t))))
        (let ((agent-repl-debug nil))
          (agent-repl--log nil "test")
          (should-not message-called))))))

(ert-deftest agent-repl-test-log-verbose-no-file-write-when-debug-off ()
  "`agent-repl--log-verbose' must NOT write to file when debug is nil.
The verbose-only gating is the load-bearing perf fix: profiling showed
the always-on file write at the bottom of every alive-check / timer
tick / window-change callback dominated Emacs CPU.  Verbose logs are
strictly opt-in now."
  (agent-repl-test--with-temp-logfile path
    (cl-letf (((symbol-function 'message) #'ignore))
      (let ((agent-repl-debug nil))
        (agent-repl--log-verbose nil "verbose-line")
        ;; The helper pre-creates an empty temp file, so check that no
        ;; bytes were appended rather than non-existence.
        (should (zerop (nth 7 (file-attributes path))))))))

(ert-deftest agent-repl-test-log-verbose-no-file-write-when-debug-t ()
  "`agent-repl--log-verbose' must NOT write to file when debug is t.
Only `verbose' enables the file write — plain `t' is for `--log' only.
Regression guard: the verbose-mode gate must not collapse with the
standard debug gate."
  (agent-repl-test--with-temp-logfile path
    (cl-letf (((symbol-function 'message) #'ignore))
      (let ((agent-repl-debug t))
        (agent-repl--log-verbose nil "verbose-line")
        (should (zerop (nth 7 (file-attributes path))))))))

(ert-deftest agent-repl-test-log-verbose-writes-file-when-debug-verbose ()
  "`agent-repl--log-verbose' MUST write to file when debug is `verbose'.
Positive case for the gating change — verbose mode is the opt-in
configuration that re-enables the file write."
  (agent-repl-test--with-temp-logfile path
    (cl-letf (((symbol-function 'message) #'ignore))
      (let ((agent-repl-debug 'verbose))
        (agent-repl--log-verbose nil "verbose-line")
        (should (file-exists-p path))
        (with-temp-buffer
          (insert-file-contents path)
          (should (string-match-p "verbose-line" (buffer-string))))))))

(ert-deftest agent-repl-test-log-verbose-still-suppresses-message-when-debug-t ()
  "`agent-repl--log-verbose' must NOT call `message' when debug is t (only verbose).
Regression guard: the file-write decoupling must not collapse the
verbose-vs-standard distinction at the message-emit layer."
  (agent-repl-test--with-temp-logfile _
    (let ((message-called nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest _) (setq message-called t))))
        (let ((agent-repl-debug t))
          (agent-repl--log-verbose nil "test")
          (should-not message-called))))))

(ert-deftest agent-repl-test-log-no-file-write-when-log-to-file-nil ()
  "`agent-repl--log' must not touch the disk when `agent-repl-log-to-file' is nil.
The master kill-switch overrides the always-on file-write decoupling."
  (let ((path (make-temp-file "agent-repl-test-log-")))
    (unwind-protect
        (let ((agent-repl-log-to-file nil)
              (agent-repl-log-file-name path)
              (initial-size (nth 7 (file-attributes path))))
          (cl-letf (((symbol-function 'message) #'ignore))
            (let ((agent-repl-debug nil))
              (agent-repl--log nil "should-not-land"))
            (should (= initial-size (nth 7 (file-attributes path))))))
      (when (file-exists-p path) (delete-file path)))))

;;;; ---- Tests: startup rollover ----

(ert-deftest agent-repl-test-rotate-renames-existing-log-to-prev ()
  "Rollover renames an existing logfile to `<path>.prev'."
  (agent-repl-test--with-temp-logfile path
    (write-region "old session line\n" nil path)
    (agent-repl--rotate-log-on-startup)
    (let ((prev (concat path ".prev")))
      (should (file-exists-p prev))
      (should-not (file-exists-p path))
      (with-temp-buffer
        (insert-file-contents prev)
        (should (string-match-p "old session line" (buffer-string)))))))

(ert-deftest agent-repl-test-rotate-overwrites-existing-prev ()
  "Rollover clobbers an existing `<path>.prev' (only one prior session is kept)."
  (agent-repl-test--with-temp-logfile path
    (let ((prev (concat path ".prev")))
      (write-region "ancient prev\n" nil prev)
      (write-region "current session\n" nil path)
      (agent-repl--rotate-log-on-startup)
      (should (file-exists-p prev))
      (with-temp-buffer
        (insert-file-contents prev)
        ;; The ancient one is gone; the rotated one is the new content.
        (should-not (string-match-p "ancient prev" (buffer-string)))
        (should (string-match-p "current session" (buffer-string)))))))

(ert-deftest agent-repl-test-rotate-noop-when-log-absent ()
  "Rollover is a no-op when the current logfile does not exist yet."
  (agent-repl-test--with-temp-logfile path
    (delete-file path)
    (let ((prev (concat path ".prev")))
      ;; Should not error and should not create prev out of nothing.
      (agent-repl--rotate-log-on-startup)
      (should-not (file-exists-p prev))
      (should-not (file-exists-p path)))))

(ert-deftest agent-repl-test-rotate-resets-write-counter ()
  "Rollover resets the write counter so size accounting starts fresh."
  (agent-repl-test--with-temp-logfile path
    (write-region "x\n" nil path)
    (setq agent-repl--log-write-counter 12345)
    (agent-repl--rotate-log-on-startup)
    (should (zerop agent-repl--log-write-counter))))

(ert-deftest agent-repl-test-rotate-noop-when-log-to-file-off ()
  "Rollover does nothing when `agent-repl-log-to-file' is nil."
  (let ((path (make-temp-file "agent-repl-test-log-")))
    (unwind-protect
        (let ((agent-repl-log-to-file nil)
              (agent-repl-log-file-name path))
          (write-region "stays-put\n" nil path)
          (agent-repl--rotate-log-on-startup)
          (should (file-exists-p path))
          (should-not (file-exists-p (concat path ".prev"))))
      (when (file-exists-p path) (delete-file path)))))

;;;; ---- Tests: size cap and truncation ----

(ert-deftest agent-repl-test-truncate-keeps-last-20-percent-line-aligned ()
  "`agent-repl--log-truncate' keeps the last ~20% of the file, aligned to a newline."
  (agent-repl-test--with-temp-logfile path
    ;; Build a file with 100 fixed-width lines so we can assert which survive.
    (with-temp-buffer
      (dotimes (i 100)
        (insert (format "line-%03d-padding-to-make-it-wider\n" i)))
      (write-region (point-min) (point-max) path))
    (let ((size (nth 7 (file-attributes path))))
      (agent-repl--log-truncate path size)
      (with-temp-buffer
        (insert-file-contents path)
        (let ((content (buffer-string)))
          ;; First lines must be gone.
          (should-not (string-match-p "line-000-" content))
          (should-not (string-match-p "line-010-" content))
          ;; Last lines must survive.
          (should (string-match-p "line-099-" content))
          ;; A WARNING line was appended.
          (should (string-match-p "WARNING: log truncated" content))
          ;; The file must start on a clean line boundary, not mid-line.
          (should (string-match-p "\\`line-[0-9][0-9][0-9]-" content)))))))

(ert-deftest agent-repl-test-truncate-appends-warning ()
  "Truncation appends a WARNING entry naming the cap and observed size."
  (agent-repl-test--with-temp-logfile path
    (write-region (make-string 5000 ?x) nil path)
    (let ((size (nth 7 (file-attributes path))))
      (agent-repl--log-truncate path size)
      (with-temp-buffer
        (insert-file-contents path)
        (let ((content (buffer-string)))
          (should (string-match-p "WARNING: log truncated" content))
          (should (string-match-p "cap=" content))
          (should (string-match-p "kept last" content)))))))

(ert-deftest agent-repl-test-size-check-fires-every-interval ()
  "`--do-log-to-file' invokes `--log-maybe-truncate' on the Nth write."
  (agent-repl-test--with-temp-logfile path
    (let ((check-calls 0)
          (agent-repl-log-size-check-interval 5))
      (cl-letf (((symbol-function 'agent-repl--log-maybe-truncate)
                 (lambda (_p) (cl-incf check-calls))))
        (dotimes (_ 12)
          (agent-repl--do-log-to-file "line"))
        ;; 12 writes with interval=5 → checks at 5 and 10 → 2 fires.
        (should (= 2 check-calls))))))

(ert-deftest agent-repl-test-size-check-noop-when-under-cap ()
  "`--log-maybe-truncate' is a no-op when the file is under the cap."
  (agent-repl-test--with-temp-logfile path
    (write-region "small file\n" nil path)
    (let ((agent-repl-log-size-cap-bytes (* 1024 1024)))
      (let ((truncate-called nil))
        (cl-letf (((symbol-function 'agent-repl--log-truncate)
                   (lambda (&rest _) (setq truncate-called t))))
          (agent-repl--log-maybe-truncate path)
          (should-not truncate-called))))))

(ert-deftest agent-repl-test-size-check-triggers-truncate-when-over-cap ()
  "`--log-maybe-truncate' fires `--log-truncate' when the file exceeds the cap."
  (agent-repl-test--with-temp-logfile path
    (write-region (make-string 4096 ?x) nil path)
    (let ((agent-repl-log-size-cap-bytes 1024)
          (truncate-args nil))
      (cl-letf (((symbol-function 'agent-repl--log-truncate)
                 (lambda (p size) (setq truncate-args (list p size)))))
        (agent-repl--log-maybe-truncate path)
        (should (equal (car truncate-args) path))
        (should (>= (cadr truncate-args) 4096))))))

(ert-deftest agent-repl-test-write-counter-increments-per-write ()
  "Every successful file-write bumps `agent-repl--log-write-counter'."
  (agent-repl-test--with-temp-logfile _
    (dotimes (_ 7)
      (agent-repl--do-log-to-file "x"))
    (should (= 7 agent-repl--log-write-counter))))

;;;; ---- Tests: buffer-owner accessor ----

(ert-deftest agent-repl-test-core-buffer-owner-returns-owner ()
  "buffer-owner returns the buffer-local owning workspace."
  (let ((buf (get-buffer-create "*bo-owned*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local agent-repl--owning-workspace "owner-ws"))
          (should (equal "owner-ws" (agent-repl--buffer-owner buf))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-core-buffer-owner-nil-when-unset ()
  "buffer-owner returns nil for a buffer with no owner set."
  (let ((buf (get-buffer-create "*bo-unset*")))
    (unwind-protect
        (should-not (agent-repl--buffer-owner buf))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-core-buffer-owner-nil-when-dead ()
  "buffer-owner returns nil for a dead buffer instead of erroring."
  (let ((buf (get-buffer-create "*bo-dead*")))
    (with-current-buffer buf
      (setq-local agent-repl--owning-workspace "owner-ws"))
    (kill-buffer buf)
    (should-not (agent-repl--buffer-owner buf))))

(ert-deftest agent-repl-test-core-buffer-owner-nil-when-nil-buffer ()
  "buffer-owner returns nil for a nil buffer argument."
  (should-not (agent-repl--buffer-owner nil)))

;;;; ---- Tests: foreign-owned-buffer-p ----

(ert-deftest agent-repl-test-core-foreign-owned-buffer-p/foreign-owner ()
  "foreign-owned-buffer-p is non-nil for a buffer owned by another workspace."
  (let ((buf (get-buffer-create "*fo-foreign*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local agent-repl--owning-workspace "other-ws"))
          (should (agent-repl--foreign-owned-buffer-p buf "this-ws")))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-core-foreign-owned-buffer-p/same-owner ()
  "foreign-owned-buffer-p is nil for a buffer owned by the same workspace."
  (let ((buf (get-buffer-create "*fo-own*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local agent-repl--owning-workspace "this-ws"))
          (should-not (agent-repl--foreign-owned-buffer-p buf "this-ws")))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-core-foreign-owned-buffer-p/no-owner ()
  "foreign-owned-buffer-p is nil for an unowned buffer (e.g. magit/file)."
  (let ((buf (get-buffer-create "*fo-unowned*")))
    (unwind-protect
        (should-not (agent-repl--foreign-owned-buffer-p buf "this-ws"))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-core-foreign-owned-buffer-p/dead-buffer ()
  "foreign-owned-buffer-p is nil for a dead buffer."
  (let ((buf (get-buffer-create "*fo-dead*")))
    (with-current-buffer buf
      (setq-local agent-repl--owning-workspace "other-ws"))
    (kill-buffer buf)
    (should-not (agent-repl--foreign-owned-buffer-p buf "this-ws"))))

;;;; ---- Tests: workspace-name prefix ----

(ert-deftest agent-repl-test-workspace-prefix-env-set ()
  "workspace-prefix falls back to the legacy CLAUDE_WORKSPACE_PREFIX."
  (cl-letf (((symbol-function 'getenv)
             (lambda (k) (and (equal k "CLAUDE_WORKSPACE_PREFIX") "DWC"))))
    (should (equal (agent-repl--workspace-prefix) "DWC"))))

(ert-deftest agent-repl-test-workspace-prefix-new-env-set ()
  "workspace-prefix returns the AGENT_WORKSPACE_PREFIX value when set."
  (cl-letf (((symbol-function 'getenv)
             (lambda (k) (and (equal k "AGENT_WORKSPACE_PREFIX") "AWP"))))
    (should (equal (agent-repl--workspace-prefix) "AWP"))))

(ert-deftest agent-repl-test-workspace-prefix-new-env-wins-over-legacy ()
  "workspace-prefix prefers AGENT_WORKSPACE_PREFIX over the legacy var."
  (cl-letf (((symbol-function 'getenv)
             (lambda (k) (cond ((equal k "AGENT_WORKSPACE_PREFIX") "AWP")
                               ((equal k "CLAUDE_WORKSPACE_PREFIX") "DWC")))))
    (should (equal (agent-repl--workspace-prefix) "AWP"))))

(ert-deftest agent-repl-test-workspace-prefix-empty-new-env-falls-back ()
  "workspace-prefix treats an empty AGENT_WORKSPACE_PREFIX as unset."
  (cl-letf (((symbol-function 'getenv)
             (lambda (k) (cond ((equal k "AGENT_WORKSPACE_PREFIX") "")
                               ((equal k "CLAUDE_WORKSPACE_PREFIX") "DWC")))))
    (should (equal (agent-repl--workspace-prefix) "DWC"))))

(ert-deftest agent-repl-test-workspace-prefix-env-unset ()
  "workspace-prefix returns the empty string when the env var is unset."
  (cl-letf (((symbol-function 'getenv) (lambda (_) nil)))
    (should (equal (agent-repl--workspace-prefix) ""))))

(ert-deftest agent-repl-test-workspace-prefix-slash-env-set ()
  "workspace-prefix-slash appends a trailing slash to a non-empty prefix."
  (cl-letf (((symbol-function 'getenv)
             (lambda (k) (and (equal k "CLAUDE_WORKSPACE_PREFIX") "DWC"))))
    (should (equal (agent-repl--workspace-prefix-slash) "DWC/"))))

(ert-deftest agent-repl-test-workspace-prefix-slash-env-unset ()
  "workspace-prefix-slash returns the empty string when the env var is unset."
  (cl-letf (((symbol-function 'getenv) (lambda (_) nil)))
    (should (equal (agent-repl--workspace-prefix-slash) ""))))

;;;; ---- Tests: harness-injected (meta) prompt spans ----

(ert-deftest agent-repl-test-meta-wrap-brackets-the-text ()
  "`agent-repl--meta-wrap' brackets TEXT with the open/close markers."
  (should (equal (agent-repl--meta-wrap "injected")
                 (concat agent-repl--meta-open "injected" agent-repl--meta-close))))

(ert-deftest agent-repl-test-meta-wrap-keeps-the-text-verbatim ()
  "The wrapped span still carries its text intact — the agent reads it."
  (should (string-match-p (regexp-quote "read the file at /x/metaprompt.md")
                          (agent-repl--meta-wrap "read the file at /x/metaprompt.md"))))

(ert-deftest agent-repl-test-meta-markers-are-html-comments ()
  "Both markers are inert HTML comments, so no renderer treats them as content."
  (should (string-prefix-p "<!--" agent-repl--meta-open))
  (should (string-suffix-p "-->" agent-repl--meta-open))
  (should (string-prefix-p "<!--" agent-repl--meta-close))
  (should (string-suffix-p "-->" agent-repl--meta-close)))

(ert-deftest agent-repl-test-meta-unmark-drops-markers-keeps-text ()
  "`agent-repl--meta-unmark' removes the markers but keeps the span's text."
  (should (equal (agent-repl--meta-unmark
                  (concat (agent-repl--meta-wrap "directive") "\n\nuser prompt"))
                 "directive\n\nuser prompt")))

(ert-deftest agent-repl-test-meta-unmark-drops-every-marked-span ()
  "Unmarking handles multiple spans in one prompt (prefix AND suffix)."
  (should (equal (agent-repl--meta-unmark
                  (concat (agent-repl--meta-wrap "preamble: ")
                          "the task"
                          (agent-repl--meta-wrap " wrap-up gate")))
                 "preamble: the task wrap-up gate")))

(ert-deftest agent-repl-test-meta-unmark-leaves-unmarked-text-alone ()
  "Text carrying no markers passes through unmarking unchanged."
  (should (equal (agent-repl--meta-unmark "plain prompt") "plain prompt")))

(provide 'test-core)

;;; test-core.el ends here
