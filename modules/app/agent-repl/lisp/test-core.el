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

;;;; ---- Tests: bounded warning deduplication ----

(ert-deftest agent-repl-test-warn-once-emits-first-observation-only ()
  "A repeated causal fingerprint records one identity-complete warning."
  (let ((agent-repl--warn-once-fingerprints (make-hash-table :test 'equal))
        (agent-repl--warn-once-order nil)
        (warnings nil))
    (cl-letf (((symbol-function 'agent-repl--warn)
               (lambda (ws fmt &rest args)
                 (push (list ws (apply #'format fmt args)) warnings))))
      (should (agent-repl--warn-once "ws" "api-message=m payload=abc"
                                     "response warning api_message_id=%s" "m"))
      (should-not (agent-repl--warn-once "ws" "api-message=m payload=abc"
                                         "response warning api_message_id=%s" "m"))
      (should (equal warnings '(("ws" "response warning api_message_id=m")))))))

(ert-deftest agent-repl-test-warn-once-fifo-bound-makes-evicted-key-observable ()
  "A full warning cache evicts FIFO state without retaining unbounded entries."
  (let ((agent-repl--warn-once-capacity 2)
        (agent-repl--warn-once-fingerprints (make-hash-table :test 'equal))
        (agent-repl--warn-once-order nil)
        (warnings nil))
    (cl-letf (((symbol-function 'agent-repl--warn)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warnings))))
      (dolist (fingerprint '("first" "second" "third" "first"))
        (should (agent-repl--warn-once "ws" fingerprint "warning=%s" fingerprint)))
      (should (= (hash-table-count agent-repl--warn-once-fingerprints) 2))
      (should (equal (nreverse warnings)
                     '("warning=first" "warning=second" "warning=third" "warning=first"))))))

(ert-deftest agent-repl-test-warn-once-rejects-empty-causal-fingerprint ()
  "A missing causal identity signals before warning-state mutation."
  (let ((agent-repl--warn-once-fingerprints (make-hash-table :test 'equal))
        (agent-repl--warn-once-order nil))
    (cl-letf (((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
      (should-error (agent-repl--warn-once "ws" "" "impossible")))
    (should (= (hash-table-count agent-repl--warn-once-fingerprints) 0))))

(ert-deftest agent-repl-test-log-on-transition-emits-only-initial-and-changed-states ()
  "Hot diagnostics retain initial state and causal state transitions."
  (let ((agent-repl--log-transition-states (make-hash-table :test 'equal))
        (logs nil))
    (cl-letf (((symbol-function 'agent-repl--log-verbose)
               (lambda (ws fmt &rest args) (push (list ws (apply #'format fmt args)) logs))))
      (should (agent-repl--log-on-transition "ws" "poll" '(1 2) "state=%S" '(1 2)))
      (should-not (agent-repl--log-on-transition "ws" "poll" '(1 2) "state=%S" '(1 2)))
      (should (agent-repl--log-on-transition "ws" "poll" '(2 2) "state=%S" '(2 2)))
      (should (equal (nreverse logs)
                     '(("ws" "state=(1 2)") ("ws" "state=(2 2)")))))))

(ert-deftest agent-repl-test-log-on-transition-fifo-bound-evicts-oldest-key ()
  "Transition state retains a fixed number of caller keys."
  (let ((agent-repl--log-transition-capacity 2)
        (agent-repl--log-transition-states (make-hash-table :test 'equal))
        (agent-repl--log-transition-order nil))
    (cl-letf (((symbol-function 'agent-repl--log-verbose) (lambda (&rest _) nil)))
      (dolist (key '("first" "second" "third"))
        (should (agent-repl--log-on-transition "ws" key :state "key=%s" key)))
      (should (= (hash-table-count agent-repl--log-transition-states) 2))
      (should (agent-repl--log-on-transition "ws" "first" :state "key=first")))))

(ert-deftest agent-repl-test-diagnostic-fingerprint-distinguishes-same-byte-content ()
  "Same-size status payloads with different content get distinct transition keys."
  (let ((first (agent-repl--diagnostic-fingerprint "state=idle"))
        (second (agent-repl--diagnostic-fingerprint "state=done")))
    (should (= (length "state=idle") (length "state=done")))
    (should (= (length first) 64))
    (should-not (equal first second))))

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

;;;; ---- Tests: agent-view-buffer-name-p (the RENDERING predicate) ----

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

(ert-deftest agent-repl-test-bug10-defvar-declarations ()
  "Bug 10: All key variables should be properly declared.
Note: agent-repl--notification-backend may not be bound in headless
environments without notification tools (terminal-notifier or osascript)."
  (should (boundp 'agent-repl--workspaces))
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

(ert-deftest agent-repl-test-cancel-all-timers-clears-update-in-flight-flag ()
  "Cancelling all timers tears down the workspace-state update chain.
The chain's continuations are unregistered one-shot timers, so cancelling
the registry alone left its in-flight flag armed across a module reload —
observed as a stale flag force-cleared with a warning minutes later."
  (let ((agent-repl--timers nil)
        (agent-repl--keyed-timers nil)
        (agent-repl--update-chain-timer nil)
        (agent-repl--update-in-flight (float-time)))
    (agent-repl--cancel-all-timers)
    (should-not agent-repl--update-in-flight)))

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
  (let ((file-write-called nil)
        (agent-repl-log-to-file t))
    (cl-letf (((symbol-function 'agent-repl--do-log-to-file)
               (lambda (&rest _args) (setq file-write-called t))))
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

(ert-deftest agent-repl-test-log-verbose-nil-no-message ()
  "`agent-repl--log-verbose' should not emit to *Messages* when debug is nil."
  (let ((message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) (setq message-called t))))
      (let ((agent-repl-debug nil))
        (agent-repl--log-verbose nil "test")
        (should-not message-called)))))

(ert-deftest agent-repl-test-log-verbose-t-no-message ()
  "`agent-repl--log-verbose' should not emit to *Messages* when debug is t."
  (let ((message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) (setq message-called t))))
      (let ((agent-repl-debug t))
        (agent-repl--log-verbose nil "test")
        (should-not message-called)))))

(ert-deftest agent-repl-test-log-verbose-verbose-emits-message ()
  "`agent-repl--log-verbose' should emit to *Messages* when debug is `verbose'."
  (let ((message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) (setq message-called t))))
      (let ((agent-repl-debug 'verbose))
        (agent-repl--log-verbose nil "test")
        (should message-called)))))

;;;; ---- Tests: workspace-owned live log buffers ----

(ert-deftest agent-repl-test-workspace-log-buffer-is-owned-and-attached ()
  "The live log buffer is created through the workspace attachment boundary."
  (let ((buf nil)
        (attached nil)
        (agent-repl--workspace-log-buffer-enabled t))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp)
                   (lambda (_ws) 'fake-persp))
                  ((symbol-function 'agent-repl--ws-add-buffer)
                   (lambda (buffer persp switch)
                     (setq attached (list buffer persp switch)))))
          (setq buf (agent-repl--workspace-log-buffer "ws-live-log"))
          (should (equal (buffer-name buf) "*agent-panel-log-ws-live-log*"))
          (should (equal (agent-repl--buffer-owner buf) "ws-live-log"))
          (should (equal attached (list buf 'fake-persp nil))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-append-workspace-log-keeps-workspaces-isolated ()
  "Each live log buffer contains its exact lines and no other workspace's lines."
  (let ((first nil)
        (second nil)
        (agent-repl--workspace-log-buffer-enabled t))
    (unwind-protect
        (progn
          (agent-repl--append-workspace-log "first-log-ws" "first exact line")
          (agent-repl--append-workspace-log "second-log-ws" "second exact line")
          (setq first (agent-repl--workspace-log-buffer "first-log-ws")
                second (agent-repl--workspace-log-buffer "second-log-ws"))
          (should (equal (with-current-buffer first (buffer-string))
                         "first exact line\n"))
          (should (equal (with-current-buffer second (buffer-string))
                         "second exact line\n")))
      (dolist (buf (list first second))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest agent-repl-test-log-ladder-routes-workspace-lines-to-live-log-buffer ()
  "Every emitted workspace log-ladder line reaches its live buffer once."
  (agent-repl-test--with-clean-state
    (let ((ws "ladder-log-ws")
          (project (make-temp-file "agent-repl-live-ladder-" t))
          (buf nil)
          (agent-repl-debug nil)
          (agent-repl-log-to-file nil)
          ;; The ladder's verbose rung is one of the lines this asserts on, so
          ;; the BUFFER threshold is opened to admit it.  Its default excludes
          ;; verbose; that exclusion is the sibling test's subject.
          (agent-repl-log-buffer-level 'verbose)
          (agent-repl--workspace-log-buffer-enabled nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl--workspace-log-buffer-enabled t))
              (agent-repl--log ws "normal-entry")
              (agent-repl--info ws "info-entry")
              (agent-repl--warn ws "warn-entry")
              (should-error (agent-repl--error ws "error-entry") :type 'error)
              ;; Verbose records persist even when terminal visibility is off.
              (agent-repl--log-verbose ws "terminal-hidden-verbose-entry")
              (let ((agent-repl-debug 'verbose))
                (agent-repl--log-verbose ws "terminal-visible-verbose-entry")))
            (setq buf (agent-repl--workspace-log-buffer ws))
            (let* ((contents (with-current-buffer buf (buffer-string)))
                   (records (mapcar
                             (lambda (line)
                               (json-parse-string line :object-type 'alist))
                             (split-string contents "\n" t)))
                   (messages (mapcar
                              (lambda (record) (alist-get 'message record))
                              records)))
              (should (equal messages
                             '("normal-entry" "info-entry"
                               "WARNING: warn-entry" "error-entry"
                               "terminal-hidden-verbose-entry"
                               "terminal-visible-verbose-entry")))))
        (when (buffer-live-p buf)
          (kill-buffer buf))
        (delete-directory project t)))))

;;;; ---- Tests: echo-area (modeline) severity gate ----
;;
;; The invariant under test: the log file and *Messages* are the QUIET sink
;; and take everything; the echo area / modeline is the LOUD sink and is
;; reserved for GENUINE FATAL errors alone — `agent-repl--error', which
;; reaches it by SIGNALLING an `error', not through the gate below.  Every
;; ladder level (including `agent-repl--warn') emits quietly.
;; `inhibit-message' is what separates the quiet emits — when it is non-nil
;; at the moment `message' runs, the line reaches *Messages* but never the
;; echo area.  So "did this quiet line reach the modeline?" is exactly "was
;; `inhibit-message' nil inside the `message' call?".

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
  (let ((written nil)
        (agent-repl-log-to-file t))
    (cl-letf (((symbol-function 'agent-repl--do-log-to-file)
               (lambda (text &rest _args) (setq written text)))
              ((symbol-function 'message) #'ignore))
      (agent-repl--info nil "on the record")
      (should (string-match-p "on the record" written)))))

(ert-deftest agent-repl-test-warn-never-reaches-echo-area ()
  "`agent-repl--warn' is now quiet: a non-fatal warning must NOT reach the modeline."
  (let ((res (agent-repl-test--capture-emission
              (lambda () (agent-repl--warn nil "something broke")))))
    (should-not (plist-get res :echoed))))

(ert-deftest agent-repl-test-warn-still-emits-to-messages ()
  "`agent-repl--warn' still emits (into *Messages*) even though it is quiet."
  (let ((res (agent-repl-test--capture-emission
              (lambda () (agent-repl--warn nil "something broke")))))
    (should (plist-get res :called))
    (should (string-match-p "something broke" (plist-get res :text)))))

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
  (let ((written nil)
        (agent-repl-log-to-file t))
    (cl-letf (((symbol-function 'agent-repl--do-log-to-file)
               (lambda (text &rest _args) (setq written text)))
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

(ert-deftest agent-repl-test-do-log-non-error-never-reaches-echo-area ()
  "`agent-repl--do-log' non-error path is quiet — it must NOT reach the modeline."
  (let ((res (agent-repl-test--capture-emission
              (lambda () (agent-repl--do-log nil "invariant violated" nil)))))
    (should (plist-get res :called))
    (should-not (plist-get res :echoed))))

(ert-deftest agent-repl-test-do-log-error-p-bypasses-quiet-gate ()
  "`agent-repl--do-log' with ERROR-P signals directly, never routing through the
quiet `agent-repl--emit-message' gate, so a fatal line always reaches the modeline."
  (let ((emitted nil))
    (cl-letf (((symbol-function 'agent-repl--do-log-to-file) #'ignore)
              ((symbol-function 'agent-repl--emit-message)
               (lambda (&rest _) (setq emitted t))))
      (should-error (agent-repl--do-log nil "fatal boom" nil t) :type 'error)
      (should-not emitted))))

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

(ert-deftest agent-repl-test-default-log-directory-uses-os-temp-root-and-uid ()
  "The default log directory should be UID-qualified under the OS temp root."
  (let* ((tmpdir (make-temp-file "test-log-root-" t))
         (temporary-file-directory (file-name-as-directory tmpdir)))
    (unwind-protect
        (should
         (equal (agent-repl--default-log-directory)
                (file-name-as-directory
                 (expand-file-name (format "doom-agent-repl-%d" (user-uid))
                                   temporary-file-directory))))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-default-log-directory-rejects-invalid-temp-root ()
  "The default log resolver should fail loudly without an absolute temp root."
  (dolist (bad-root '(nil "" "relative/tmp"))
    (let ((temporary-file-directory bad-root))
      (should-error (agent-repl--default-log-directory) :type 'error))))

(ert-deftest agent-repl-test-normalize-log-file-name-redirects-retired-default ()
  "A reload should redirect the retired default without moving its file."
  (should
   (equal (agent-repl--normalize-log-file-name
           agent-repl--retired-state-log-file-name)
          agent-repl--default-log-file-name)))

(ert-deftest agent-repl-test-normalize-log-file-name-preserves-explicit-path ()
  "An explicitly different logfile path should remain untouched."
  (let ((custom "/tmp/agent-repl-explicit-test.log"))
    (should (equal (agent-repl--normalize-log-file-name custom) custom))))

(ert-deftest agent-repl-test-logfile-path-returns-private-temp-path ()
  "`agent-repl--logfile-path' should create the private default temp directory."
  (let* ((tmpdir (make-temp-file "test-log-root-" t))
         (temporary-file-directory (file-name-as-directory tmpdir))
         (expected-dir (agent-repl--default-log-directory))
         (agent-repl-log-file-name
          (expand-file-name "doom-agent-repl.log" expected-dir))
         (agent-repl--validated-private-log-directories
          (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (should (equal (agent-repl--logfile-path)
                         (expand-file-name "doom-agent-repl.log" expected-dir)))
          (should (file-directory-p expected-dir))
          (should (= (logand (file-modes expected-dir) #o777) #o700))
          (should (gethash expected-dir
                           agent-repl--validated-private-log-directories)))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-logfile-path-revalidates-recreated-temp-directory ()
  "Deleting the private directory should evict and rebuild its validation."
  (let* ((tmpdir (make-temp-file "test-log-root-" t))
         (temporary-file-directory (file-name-as-directory tmpdir))
         (expected-dir (agent-repl--default-log-directory))
         (agent-repl-log-file-name
          (expand-file-name "doom-agent-repl.log" expected-dir))
         (agent-repl--validated-private-log-directories
          (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (agent-repl--logfile-path)
          (delete-directory expected-dir)
          (should (gethash expected-dir
                           agent-repl--validated-private-log-directories))
          (agent-repl--logfile-path)
          (should (file-directory-p expected-dir))
          (should (= (logand (file-modes expected-dir) #o777) #o700))
          (should (gethash expected-dir
                           agent-repl--validated-private-log-directories)))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-logfile-path-rejects-symlinked-temp-directory ()
  "The private temporary log directory must not be a symlink."
  (let* ((tmpdir (make-temp-file "test-log-root-" t))
         (temporary-file-directory (file-name-as-directory tmpdir))
         (target (expand-file-name "target" tmpdir))
         (expected-dir (agent-repl--default-log-directory))
         (agent-repl-log-file-name
          (expand-file-name "doom-agent-repl.log" expected-dir))
         (agent-repl--validated-private-log-directories
          (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (make-directory target)
          (make-symbolic-link target (directory-file-name expected-dir))
          (should-error (agent-repl--logfile-path) :type 'error))
      (delete-directory tmpdir t))))

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
          (should (= (logand (file-modes logpath) #o777) #o600))
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

(ert-deftest agent-repl-test-do-log-writes-jsonl-to-global-file ()
  "A nil-WS log record uses the global sink and is valid JSONL."
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
            (let ((record (json-parse-string contents :object-type 'alist)))
              (should (equal (alist-get 'runtime record) "emacs"))
              (should (equal (alist-get 'message record) "hello world"))
              (should (numberp (alist-get 'pid record)))
              (should (equal (alist-get 'verbosity record) "normal")))))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-log-operation-normalizes-format-template ()
  "Operation names are stable slugs derived from format templates alone."
  (should (equal (agent-repl--log-operation "Started request %s: %d")
                 "agent-repl.started-request-s-d"))
  (should (equal (agent-repl--log-operation "punctuation ///")
                 "agent-repl.punctuation"))
  (should (equal (agent-repl--log-operation "") "agent-repl.log")))

(ert-deftest agent-repl-test-do-log-to-file-signals-write-error ()
  "A sink failure is loud because persistence cannot silently degrade."
  (let ((agent-repl-log-to-file t))
    (cl-letf (((symbol-function 'agent-repl--logfile-path)
               (lambda () "/nonexistent/dir/impossible.log")))
      (should-error (agent-repl--do-log-to-file "test")))))

;;;; ---- Tests: JSONL workspace logging contract ----

(ert-deftest agent-repl-test-log-workspace-record-is-jsonl-and-uses-external-target ()
  "Workspace logging writes the complete Emacs JSONL schema through a symlink."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-workspace-log-" t))
           (ws "json-ws")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl-log-to-file t))
              (cl-letf (((symbol-function 'message) #'ignore))
                (agent-repl--log ws "started request %s" "r-1")))
            (let* ((canonical (expand-file-name ".claude/emacs/emacs.log" project))
                   (target (plist-get (gethash ws agent-repl--workspace-log-targets) :target))
                   (record (with-temp-buffer
                             (insert-file-contents target)
                             (json-parse-string (buffer-string) :object-type 'alist))))
              (should (equal (file-symlink-p canonical) target))
              (should (string-prefix-p (file-truename temporary-file-directory)
                                       (file-truename target)))
              (dolist (field '("timestamp" "runtime" "pid" "level" "verbosity"
                               "operation" "message" "context" "workspace_dir" "workspace_id"))
                (should (assoc (intern field) record)))
              (should (equal (alist-get 'runtime record) "emacs"))
              (should (equal (alist-get 'message record) "started request r-1"))
              (should (equal (alist-get 'workspace_dir record)
                             (directory-file-name (file-truename project))))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-workspace-record-attributes-known-sessions ()
  "Workspace JSONL records expose the DURABLE conversation id, and only it.
The daemon session id is ephemeral — it dies with the daemon process, so
a log line carrying one cannot be correlated after a bounce, while the
workspace beside it can."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-identity-log-" t))
           (ws "identity-ws")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl-log-to-file t))
              (cl-letf (((symbol-function 'message) #'ignore)
                        ((symbol-function 'agent-repl--frontend-session-view)
                         (lambda (_) '(:claudeSessionId "claude-session-1"))))
                (agent-repl--log ws "identity test")))
            (let* ((target (plist-get (gethash ws agent-repl--workspace-log-targets) :target))
                   (record (with-temp-buffer
                             (insert-file-contents target)
                             (json-parse-string (buffer-string) :object-type 'alist))))
              (should-not (assoc 'agent_repl_session_id record))
              (should (equal (alist-get 'claude_session_id record) "claude-session-1"))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-session-identity-does-not-reenter-persistence ()
  "Resolving a session identity while logging produces exactly one record."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-nonrecursive-log-" t))
           (ws "nonrecursive-ws")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal))
           (original-persist (symbol-function 'agent-repl--persist-log-record))
           (persist-calls 0))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :project-dir project)
            (agent-repl--ws-put ws :active-env :bare-metal)
            (agent-repl--ws-put ws :bare-metal
                                (make-agent-repl-instantiation :session-id "claude-session-1"))
            (let ((agent-repl-log-to-file t))
              (cl-letf (((symbol-function 'agent-repl--persist-log-record)
                         (lambda (&rest args)
                           (cl-incf persist-calls)
                           (apply original-persist args))))
                (agent-repl--log ws "one record")
                (should (= persist-calls 1)))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-workspace-record-omits-missing-session-identities ()
  "An absent durable conversation id is not serialized as a null field."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-missing-identity-" t))
           (ws "missing-identity-ws")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl-log-to-file t))
              (agent-repl--log ws "missing identities"))
            (let* ((target (plist-get (gethash ws agent-repl--workspace-log-targets) :target))
                   (record (with-temp-buffer
                             (insert-file-contents target)
                             (json-parse-string (buffer-string) :object-type 'alist))))
              (should-not (assoc 'claude_session_id record))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-workspace-invalid-session-identity-fails-before-write ()
  "Malformed workspace identity aborts logging without creating a target."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-invalid-identity-" t))
           (ws "invalid-identity-ws")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl-log-to-file t))
              (cl-letf (((symbol-function 'agent-repl--frontend-session-view)
                         (lambda (_) '(:claudeSessionId 99))))
                (should-error (agent-repl--log ws "invalid claude identity"))))
            (should-not (gethash ws agent-repl--workspace-log-targets)))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-verbose-persists-with-terminal-visibility-disabled ()
  "Verbose records persist even when verbose terminal visibility is disabled."
  (let* ((dir (make-temp-file "agent-repl-verbose-log-" t))
         (path (expand-file-name "global.log" dir))
         (agent-repl-log-to-file t)
         (agent-repl-log-file-name path)
         (agent-repl-debug nil)
         ;; Opened so the assertion is about verbosity persisting independently
         ;; of `agent-repl-debug', not about the durable threshold's gate.
         (agent-repl-log-file-level 'verbose)
         (message-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'message) (lambda (&rest _) (setq message-called t))))
          (agent-repl--log-verbose nil "timer tick")
          (let ((record (with-temp-buffer
                          (insert-file-contents path)
                          (json-parse-string (buffer-string) :object-type 'alist))))
            (should-not message-called)
            (should (equal (alist-get 'verbosity record) "verbose"))
            (should (equal (alist-get 'message record) "timer tick"))))
      (delete-directory dir t))))

(ert-deftest agent-repl-test-log-workspace-without-directory-routes-globally ()
  "A non-nil workspace without a registered directory routes to the global sink.

This test previously asserted the opposite — that the write was REFUSED and
the caller signalled.  That contract could not hold: a workspace's worktree
can be deleted while Emacs is running, at which point its owner is still
correct to log about it and no call site can prevent the condition.  Making
it fatal aborted `doom-init-ui-hook' and the startup snapshot restore.

The coverage is kept, not dropped: the same input is still exercised, and
the record must still be persisted rather than silently discarded."
  (agent-repl-test--with-clean-state
    (let* ((dir (make-temp-file "agent-repl-routing-error-" t))
           (global (expand-file-name "global.log" dir))
           (agent-repl-log-to-file t)
           (agent-repl-log-file-name global)
           (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
      (unwind-protect
          (cl-letf (((symbol-function 'message) #'ignore))
            (agent-repl--log "missing-ws" "must still route")
            (should (file-exists-p global))
            (with-temp-buffer
              (insert-file-contents global)
              (should (string-match-p "must still route" (buffer-string)))))
        (delete-directory dir t)))))

(ert-deftest agent-repl-test-workspace-log-replaces-hostile-canonical-symlink ()
  "A workspace-provided symlink is replaced without writing its target."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-hostile-link-" t))
           (foreign (make-temp-file "agent-repl-foreign-log-"))
           (canonical (expand-file-name ".claude/emacs/emacs.log" project))
           (ws "hostile-link")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (with-temp-file foreign (insert "foreign content\n"))
            (make-directory (file-name-directory canonical) t)
            (make-symbolic-link foreign canonical)
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl-log-to-file t))
              (cl-letf (((symbol-function 'message) #'ignore))
                (agent-repl--log ws "safe write")))
            (should (equal (with-temp-buffer (insert-file-contents foreign) (buffer-string))
                           "foreign content\n"))
            (should-not (equal (file-symlink-p canonical) foreign)))
        (delete-file foreign)
        (delete-directory project t)))))

(ert-deftest agent-repl-test-workspace-log-replaces-hostile-regular-file ()
  "A workspace-provided regular file is replaced rather than opened as a sink."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-hostile-file-" t))
           (canonical (expand-file-name ".claude/emacs/emacs.log" project))
           (ws "hostile-file")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (make-directory (file-name-directory canonical) t)
            (with-temp-file canonical (insert "hostile content\n"))
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl-log-to-file t))
              (cl-letf (((symbol-function 'message) #'ignore))
                (agent-repl--log ws "safe write")))
            (should (file-symlink-p canonical)))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-workspace-log-rejects-symlinked-claude-directory ()
  "A `.claude' symlink aborts before any external directory is modified."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-hostile-claude-" t))
           (external (make-temp-file "agent-repl-external-claude-" t))
           (ws "hostile-claude")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (make-symbolic-link external (expand-file-name ".claude" project))
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl-log-to-file t))
              (should-error (agent-repl--log ws "must fail")))
            (should-not (file-exists-p (expand-file-name "emacs/emacs.log" external)))
            (should-not (gethash ws agent-repl--workspace-log-targets)))
        (delete-directory project t)
        (delete-directory external t)))))

(ert-deftest agent-repl-test-workspace-log-rejects-symlinked-emacs-directory ()
  "A `.claude/emacs' symlink aborts before any external directory is modified."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-hostile-emacs-" t))
           (external (make-temp-file "agent-repl-external-emacs-" t))
           (claude (expand-file-name ".claude" project))
           (ws "hostile-emacs")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (make-directory claude)
            (make-symbolic-link external (expand-file-name "emacs" claude))
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl-log-to-file t))
              (should-error (agent-repl--log ws "must fail")))
            (should-not (file-exists-p (expand-file-name "emacs.log" external)))
            (should-not (gethash ws agent-repl--workspace-log-targets)))
        (delete-directory project t)
        (delete-directory external t)))))

(ert-deftest agent-repl-test-workspace-log-unsafe-parent-creates-no-target ()
  "Unsafe workspace components fail before creating an external target file."
  (agent-repl-test--with-clean-state
    (let* ((sandbox (make-temp-file "agent-repl-unsafe-target-sandbox-" t))
           ;; This assertion inventories every matching target in the temp
           ;; directory, so isolate it from concurrent test-all processes.
           (temporary-file-directory (file-name-as-directory sandbox))
           (project (make-temp-file "agent-repl-unsafe-target-" t))
           (external (make-temp-file "agent-repl-unsafe-external-" t))
           (ws "unsafe-no-target")
           (before (directory-files temporary-file-directory t "\\`agent-repl-emacs-"))
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (make-symbolic-link external (expand-file-name ".claude" project))
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl-log-to-file t))
              (should-error (agent-repl--log ws "must fail")))
            (should (equal before (directory-files temporary-file-directory t "\\`agent-repl-emacs-")))
            (should-not (file-exists-p (expand-file-name "emacs/emacs.log" external))))
        (delete-directory project t)
        (delete-directory external t)
        (delete-directory sandbox t)))))

(ert-deftest agent-repl-test-workspace-log-rebinds-to-a-fresh-target ()
  "Rebinding a WS forgets in-memory ownership and creates a fresh target."
  (agent-repl-test--with-clean-state
    (let* ((first (make-temp-file "agent-repl-first-project-" t))
           (second (make-temp-file "agent-repl-second-project-" t))
           (ws "rebound-ws")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :project-dir first)
            (let ((agent-repl-log-to-file t))
              (agent-repl--log ws "first"))
            (let ((target (plist-get (gethash ws agent-repl--workspace-log-targets) :target)))
              (agent-repl--ws-put ws :project-dir second)
              (should-not (gethash ws agent-repl--workspace-log-targets))
              (let ((agent-repl-log-to-file t))
                (agent-repl--log ws "second"))
              (let ((rebound (plist-get (gethash ws agent-repl--workspace-log-targets) :target)))
                (should-not (equal target rebound))
                (should (file-symlink-p (expand-file-name ".claude/emacs/emacs.log" second))))))
        (delete-directory first t)
        (delete-directory second t)))))

(ert-deftest agent-repl-test-workspace-log-cleans-reserved-staging-path-on-link-failure ()
  "A failed staging symlink leaves neither a cache entry nor a staging file."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-staging-failure-" t))
           (ws "staging-failure")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :project-dir project)
            (cl-letf (((symbol-function 'make-symbolic-link)
                       (lambda (&rest _) (error "simulated staging failure"))))
              (let ((agent-repl-log-to-file t))
                (should-error (agent-repl--log ws "must fail"))))
            (should-not (gethash ws agent-repl--workspace-log-targets))
            (should-not (directory-files-recursively project "\\.emacs\\.log-link-")))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-workspace-log-rename-failure-cleans-target-and-staging ()
  "A failed atomic install leaves no target or staging artifact behind."
  (agent-repl-test--with-clean-state
    (let* ((sandbox (make-temp-file "agent-repl-rename-sandbox-" t))
           (temporary-file-directory (file-name-as-directory sandbox))
           (project (make-temp-file "agent-repl-rename-failure-" t))
           (ws "rename-failure")
           (before (directory-files temporary-file-directory t "\\`agent-repl-emacs-"))
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :project-dir project)
            (cl-letf (((symbol-function 'rename-file)
                       (lambda (&rest _) (error "simulated rename failure"))))
              (let ((agent-repl-log-to-file t))
                (should-error (agent-repl--log ws "must fail"))))
            (should-not (gethash ws agent-repl--workspace-log-targets))
            (should (equal before (directory-files temporary-file-directory t "\\`agent-repl-emacs-")))
            (should-not (directory-files-recursively project "\\.emacs\\.log-link-"))
            (should-not (file-exists-p (expand-file-name ".claude/emacs/emacs.log" project))))
        (delete-directory sandbox t)))))

(ert-deftest agent-repl-test-workspace-truncation-record-includes-identity ()
  "A workspace truncation warning includes the owning workspace identity."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-truncate-ws-" t))
           (ws "truncate-ws")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl-log-to-file t))
              (agent-repl--log ws "seed"))
            (let* ((target (plist-get (gethash ws agent-repl--workspace-log-targets) :target)))
              (with-temp-file target (insert (make-string 5000 ?x)))
              (agent-repl--log-truncate target (file-attribute-size (file-attributes target)) ws)
              (let* ((lines (with-temp-buffer (insert-file-contents target) (split-string (buffer-string) "\n" t)))
                     (record (json-parse-string (car (last lines)) :object-type 'alist)))
                (should (equal (alist-get 'workspace_dir record) (directory-file-name (file-truename project))))
                (should (stringp (alist-get 'workspace_id record))))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-maybe-truncate-signals-sink-emergency ()
  "A truncation failure reports its emergency and signals to the caller."
  (let ((emergency nil))
    (cl-letf (((symbol-function 'agent-repl--log-truncate)
               (lambda (&rest _) (error "simulated truncate failure")))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq emergency (apply #'format fmt args)))) )
      (let ((agent-repl-log-size-cap-bytes 1))
        (agent-repl-test--with-temp-logfile path
          (write-region "xx" nil path)
          (should-error (agent-repl--log-maybe-truncate path) :type 'agent-repl-log-truncate-failure)
          (should (string-match-p "LOG SINK FAILURE operation=truncate" emergency)))))))

(ert-deftest agent-repl-test-log-truncate-preserves-open-target-inode ()
  "Truncation rewrites the active external target without replacing its inode."
  (let ((path (make-temp-file "agent-repl-truncate-inode-")))
    (unwind-protect
        (progn
          (with-temp-file path (insert (make-string 5000 ?x)))
          (let ((inode (file-attribute-file-identifier (file-attributes path))))
            (agent-repl--log-truncate path (file-attribute-size (file-attributes path)) )
            (should (equal inode (file-attribute-file-identifier (file-attributes path))))))
      (delete-file path))))

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
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (let ((buf (plist-get args :buffer)))
                   (setq captured-buf buf)
                   (with-current-buffer buf
                     (insert "  trimmed result  \n"))
                   (list :fake-proc buf))))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'set-process-sentinel)
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
  (cl-letf (((symbol-function 'make-process)
             (lambda (&rest args)
               (let ((buf (plist-get args :buffer)))
                 (with-current-buffer buf
                   (insert "partial output that should not be returned\n"))
                 (list :fake-proc buf))))
            ((symbol-function 'set-process-query-on-exit-flag)
             (lambda (&rest _) nil))
            ((symbol-function 'set-process-sentinel)
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
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args) (list :fake-proc (plist-get args :buffer))))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'set-process-sentinel)
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

;;;; ---- Tests: async-gh terminal outcomes ----

(ert-deftest agent-repl-test-async-gh-calls-callback-after-abnormal-exit ()
  "An abnormal gh exit is a completed command, not a silently dropped callback."
  (let ((buf nil)
        (callback-result nil)
        (agent-repl-log-to-file nil))
    (unwind-protect
        (progn
          (setq buf (generate-new-buffer " *agent-repl-test-async-gh*"))
          (cl-letf (((symbol-function 'process-live-p) (lambda (_proc) nil))
                  ((symbol-function 'process-buffer) (lambda (_proc) buf))
                  ((symbol-function 'process-exit-status) (lambda (_proc) 1))
                  ((symbol-function 'agent-repl--kill-buffer-safely)
                   (lambda (buffer) (kill-buffer buffer))))
            (with-current-buffer buf (insert "network unavailable"))
            (agent-repl--async-gh-handle-completion
             "failed-pr-poll"
             (lambda (ok output) (setq callback-result (list ok output)))
             'fake-gh-process "exited abnormally with code 1\n"))
          (should (equal callback-result '(nil "network unavailable"))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-capture-process-output-uses-make-process-when-suppress-stderr ()
  "When SUPPRESS-STDERR is non-nil, `--capture-process-output' uses
`make-process' with `:stderr' set to a separate buffer so stderr is
discarded — matches the `2>/dev/null' contract the quiet wrappers
depend on."
  (let ((stderr-buf-arg :unset))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq stderr-buf-arg (plist-get args :stderr))
                 (list :fake-proc (plist-get args :buffer))))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'set-process-sentinel)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--wait-for-process-exit)
               (lambda (&rest _) 0)))
      (agent-repl--capture-process-output "git" '("status") t))
    (should (bufferp stderr-buf-arg))))

(ert-deftest agent-repl-test-capture-process-output-merges-stderr-when-no-suppress ()
  "When SUPPRESS-STDERR is nil (default), no separate `:stderr' buffer is
passed, so stderr merges into the stdout buffer (matches
`shell-command-to-string''s default and the existing `--git-string'
contract that includes stderr in the returned text)."
  (let ((stderr-buf-arg :unset))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq stderr-buf-arg (plist-get args :stderr))
                 (list :fake-proc (plist-get args :buffer))))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'set-process-sentinel)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--wait-for-process-exit)
               (lambda (&rest _) 0)))
      (agent-repl--capture-process-output "git" '("status") nil))
    (should-not stderr-buf-arg)))

(ert-deftest agent-repl-test-capture-process-output-spawns-on-pipe-when-no-suppress ()
  "The merged-stderr branch spawns with `:connection-type' `pipe'.
A pty spawn makes `git log' see a terminal and launch its pager, which
hangs until the timeout on every call — the 2026-07-18 freeze trigger."
  (let ((conn-type :unset))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq conn-type (plist-get args :connection-type))
                 (list :fake-proc (plist-get args :buffer))))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'set-process-sentinel)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--wait-for-process-exit)
               (lambda (&rest _) 0)))
      (agent-repl--capture-process-output "git" '("log" "-1") nil))
    (should (eq conn-type 'pipe))))

(ert-deftest agent-repl-test-capture-process-output-spawns-on-pipe-when-suppress ()
  "The suppress-stderr branch spawns with `:connection-type' `pipe' too."
  (let ((conn-type :unset))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq conn-type (plist-get args :connection-type))
                 (list :fake-proc (plist-get args :buffer))))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'set-process-sentinel)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--wait-for-process-exit)
               (lambda (&rest _) 0)))
      (agent-repl--capture-process-output "git" '("log" "-1") t))
    (should (eq conn-type 'pipe))))

(ert-deftest agent-repl-test-capture-process-output-cleanup-uses-safe-buffer-kill ()
  "Buffer cleanup routes through `agent-repl--kill-buffer-safely' when it
is bound: on the timeout path the child can still be alive, and a bare
`kill-buffer' on its buffer from the merge worker is the AppKit
off-main teardown deadlock (2026-07-18)."
  (let ((safe-killed nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (list :fake-proc (plist-get args :buffer))))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'set-process-sentinel)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--wait-for-process-exit)
               (lambda (&rest _) 'timeout))
              ((symbol-function 'agent-repl--kill-buffer-safely)
               (lambda (buf) (push (buffer-name buf) safe-killed)
                 (kill-buffer buf) t)))
      (agent-repl--capture-process-output "git" '("log" "-1") nil)
      (should (= 1 (length safe-killed))))))

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
              ((symbol-function 'set-process-sentinel)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--wait-for-process-exit)
               (lambda (&rest _) 0)))
      (agent-repl--capture-process-output "git" '("status") t))
    (should stderr-buf-arg)
    (should-not (buffer-live-p stderr-buf-arg))))

(ert-deftest agent-repl-test-capture-process-output-installs-noop-sentinel ()
  "`--capture-process-output' installs `#'ignore' as the capture process's
sentinel.  This displaces Emacs's `internal-default-process-sentinel',
whose `Process NAME finished' status insertion into the shared buffer
would otherwise be read back as command output and corrupt the result."
  (let ((sentinel-arg 'unset))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (let ((buf (plist-get args :buffer)))
                   (with-current-buffer buf (insert "clean-output\n"))
                   (list :fake-proc buf))))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'set-process-sentinel)
               (lambda (_proc sentinel) (setq sentinel-arg sentinel)))
              ((symbol-function 'agent-repl--wait-for-process-exit)
               (lambda (&rest _) 0)))
      (should (equal (agent-repl--capture-process-output
                      "git" '("rev-parse" "HEAD"))
                     "clean-output"))
      (should (eq sentinel-arg #'ignore)))))

(ert-deftest agent-repl-test-capture-process-output-installs-sentinel-before-waiting ()
  "The no-op sentinel is installed BEFORE the wait, not after.  Installing
it after `--wait-for-process-exit' would be useless on the main-thread
path, where the default sentinel fires during the wait's
`accept-process-output' loop."
  (let ((waited nil)
        (installed-before-wait nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (let ((buf (plist-get args :buffer)))
                   (with-current-buffer buf (insert "out\n"))
                   (list :fake-proc buf))))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'set-process-sentinel)
               (lambda (&rest _) (setq installed-before-wait (not waited))))
              ((symbol-function 'agent-repl--wait-for-process-exit)
               (lambda (&rest _) (setq waited t) 0)))
      (agent-repl--capture-process-output "git" '("rev-parse" "HEAD"))
      (should installed-before-wait))))

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

(ert-deftest agent-repl-test-create-buffer-bare-name ()
  "create-buffer with no suffix produces the bare *agent-panel-WS* name.
No current production caller creates a buffer through this path (the
input buffer, via \"-input\", is the only one that does), but the
capability is still part of the function's contract."
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

;;;; ---- Tests: ws-observed-claude-session-id ----
;;
;; Emacs holds NO durable copy of a vendor conversation uuid. It reads the
;; current one off the daemon-pushed `SessionView\=' store, purely to attribute
;; its own log records. The accessor this replaced read a PERSISTED uuid and
;; handed it back as a resume pointer, which made Emacs a second authority on
;; which conversation a workspace owns.

(ert-deftest agent-repl-test-observed-session-id-reads-the-pushed-view ()
  "ws-observed-claude-session-id reads the daemon-pushed SessionView."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--frontend-session-view)
               (lambda (key) (when (equal key "/w") '(:claudeSessionId "cli-uuid-1")))))
      (should (equal (agent-repl--ws-observed-claude-session-id "ws1") "cli-uuid-1")))))

(ert-deftest agent-repl-test-observed-session-id-ignores-the-in-memory-instantiation ()
  "An instantiation carrying a uuid is NOT a source for attribution.
Reading it back would be the persisted-pointer path returning by another
name; the daemon-pushed view is the only source."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :active-env :bare-metal)
    (agent-repl--ws-put "ws1" :bare-metal
                        (make-agent-repl-instantiation :session-id "stale-uuid"))
    (cl-letf (((symbol-function 'agent-repl--frontend-session-view)
               (lambda (_) nil)))
      (should-not (agent-repl--ws-observed-claude-session-id "ws1")))))

(ert-deftest agent-repl-test-observed-session-id-nil-without-a-bound-session ()
  "A workspace with no daemon session has nothing to attribute to."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--frontend-session-view)
               (lambda (_) '(:claudeSessionId "cli-uuid-1"))))
      (should-not (agent-repl--ws-observed-claude-session-id "ws1")))))

(ert-deftest agent-repl-test-observed-session-id-nil-before-the-first-push ()
  "Nil before the first pushed frame is a NORMAL answer, not a failure.
An unattributed log record is accepted by the daemon; a misattributed one is
what gets rejected, so guessing would be strictly worse than saying nothing."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--frontend-session-view)
               (lambda (_) nil)))
      (should-not (agent-repl--ws-observed-claude-session-id "ws1")))))

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

;;;; ---- Tests: agent-panel-buffer-p ----

(ert-deftest agent-repl-test-agent-panel-buffer-p-frontend ()
  "agent-panel-buffer-p should match the gui webview buffer name too.
The predicate widened to cover the two buffers a workspace actually has
now that vterm is gone: the input composer and the webview."
  (agent-repl-test--with-temp-buffer "*agent-frontend-abcd1234*"
    (should (agent-repl--agent-panel-buffer-p))))

(ert-deftest agent-repl-test-agent-panel-buffer-p-input ()
  "agent-panel-buffer-p should match input buffer names."
  (agent-repl-test--with-temp-buffer "*agent-panel-input-abcd1234*"
    (should (agent-repl--agent-panel-buffer-p))))

(ert-deftest agent-repl-test-agent-panel-buffer-p-workspace-log ()
  "The workspace-owned live log buffer is an agent-repl buffer."
  (agent-repl-test--with-temp-buffer "*agent-panel-log-abcd1234*"
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
  "non-user-buffer-p should return non-nil for the agent's own panel buffers."
  (agent-repl-test--with-temp-buffer "*agent-frontend-abcd1234*"
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
  (agent-repl-test--with-temp-buffer "*agent-frontend-aaaa1111*"
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
  (agent-repl-test--with-temp-buffer "*agent-frontend-aaaa1111*"
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
  (agent-repl-test--with-temp-logfile path
    (let ((message-called nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest _) (setq message-called t))))
        (let ((agent-repl-debug nil))
          (agent-repl--log nil "test")
          (should-not message-called))))))

(ert-deftest agent-repl-test-log-verbose-persists-when-debug-off ()
  "`agent-repl--log-verbose' persists even when terminal visibility is off.
The durable THRESHOLD is opened here so the subject under test is the
decoupling from `agent-repl-debug' alone; `agent-repl-log-file-level' is
the separate knob that decides whether this rung is written at all."
  (agent-repl-test--with-temp-logfile path
    (cl-letf (((symbol-function 'message) #'ignore))
      (let ((agent-repl-debug nil)
            (agent-repl-log-file-level 'verbose))
        (agent-repl--log-verbose nil "verbose-line")
        (should (> (nth 7 (file-attributes path)) 0))))))

(ert-deftest agent-repl-test-log-verbose-persists-when-debug-t ()
  "`agent-repl--log-verbose' persists when normal debug visibility is on.
See the sibling test: the durable threshold is opened so this asserts the
decoupling from `agent-repl-debug' rather than the threshold's own gate."
  (agent-repl-test--with-temp-logfile path
    (cl-letf (((symbol-function 'message) #'ignore))
      (let ((agent-repl-debug t)
            (agent-repl-log-file-level 'verbose))
        (agent-repl--log-verbose nil "verbose-line")
        (should (> (nth 7 (file-attributes path)) 0))))))

(ert-deftest agent-repl-test-log-verbose-writes-file-when-debug-verbose ()
  "`agent-repl--log-verbose' writes to file when debug is `verbose'.
The durable threshold is opened alongside it: both knobs must be at
verbose for this rung to reach the file, and this test is about the
second one not blocking the first."
  (agent-repl-test--with-temp-logfile path
    (cl-letf (((symbol-function 'message) #'ignore))
      (let ((agent-repl-debug 'verbose)
            (agent-repl-log-file-level 'verbose))
        (agent-repl--log-verbose nil "verbose-line")
        (should (file-exists-p path))
        (with-temp-buffer
          (insert-file-contents path)
          (should (string-match-p "verbose-line" (buffer-string))))))))

(ert-deftest agent-repl-test-log-verbose-still-suppresses-message-when-debug-t ()
  "`agent-repl--log-verbose' must NOT call `message' when debug is t (only verbose).
Regression guard: the file-write decoupling must not collapse the
verbose-vs-standard distinction at the message-emit layer."
  (agent-repl-test--with-temp-logfile path
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

(ert-deftest agent-repl-test-log-disabled-sinks-do-not-resolve-workspace ()
  "Disabled persistence sinks do not demand a workspace routing identity."
  (let ((agent-repl-log-to-file nil)
        (agent-repl--workspace-log-buffer-enabled nil)
        (identity-calls 0))
    (cl-letf (((symbol-function 'agent-repl--workspace-log-identity)
               (lambda (&rest _)
                 (cl-incf identity-calls)
                 (error "identity resolver must remain idle")))
              ((symbol-function 'message) #'ignore))
      (agent-repl--log "workspace-without-state" "persistence disabled")
      (should (zerop identity-calls)))))

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
          ;; A JSON warning record was appended.
          (should (string-match-p "agent-repl.log.truncate" content))
          ;; The file must start on a clean line boundary, not mid-line.
          (should (string-match-p "\\`line-[0-9][0-9][0-9]-" content)))))))

(ert-deftest agent-repl-test-truncate-appends-json-warning ()
  "Truncation appends a schema-valid warning record with size evidence."
  (agent-repl-test--with-temp-logfile path
    (write-region (make-string 5000 ?x) nil path)
    (let ((size (nth 7 (file-attributes path))))
      (agent-repl--log-truncate path size)
      (with-temp-buffer
        (insert-file-contents path)
        (let ((content (buffer-string)))
          (let* ((lines (split-string content "\n" t))
                 (warning (json-parse-string (car (last lines)) :object-type 'alist))
                 (context (alist-get 'context warning)))
             (should (equal (alist-get 'level warning) "warn"))
             (should (numberp (alist-get 'cap_bytes context)))
             (should (numberp (alist-get 'kept_bytes context))))))
    (should (= (logand (file-modes path) #o777) #o600)))))

(ert-deftest agent-repl-test-size-check-fires-every-interval ()
  "`--do-log-to-file' invokes `--log-maybe-truncate' on the Nth write."
  (agent-repl-test--with-temp-logfile path
    (let ((check-calls 0)
          (agent-repl-log-size-check-interval 5))
      (cl-letf (((symbol-function 'agent-repl--log-maybe-truncate)
                 (lambda (&rest _args) (cl-incf check-calls))))
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
                 (lambda (&rest args) (setq truncate-args args))))
        (agent-repl--log-maybe-truncate path)
        (should (equal (car truncate-args) path))
        (should (>= (cadr truncate-args) 4096))))))

(ert-deftest agent-repl-test-write-counter-increments-per-write ()
  "Every successful file-write bumps `agent-repl--log-write-counter'."
  (agent-repl-test--with-temp-logfile path
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

;;;; ---- Tests: agent-repl--output-dir constant ----

(ert-deftest agent-repl-test-output-dir-is-absolute ()
  "output-dir should be an absolute path under ~/.claude-emacs/output/."
  (should (file-name-absolute-p agent-repl--output-dir))
  (should (string-match-p "output/$" agent-repl--output-dir)))

;;;; ---- Tests: Buffer background color (moved from overlay.el) ----

(ert-deftest agent-repl-test-grey-hex-format ()
  "grey-hex should format N as a #rrggbb hex string with equal channels."
  (should (equal (agent-repl--grey-hex 0) "#000000"))
  (should (equal (agent-repl--grey-hex 255) "#ffffff"))
  (should (equal (agent-repl--grey-hex 15) "#0f0f0f")))

(ert-deftest agent-repl-test-grey-hex-boundary-128 ()
  "grey-hex for 128 (middle grey) should return #808080."
  (should (equal (agent-repl--grey-hex 128) "#808080")))

(ert-deftest agent-repl-test-rgb-hex-format ()
  "rgb-hex should format R, G, B independently into a #rrggbb string."
  (should (equal (agent-repl--rgb-hex 0 0 0) "#000000"))
  (should (equal (agent-repl--rgb-hex 255 255 255) "#ffffff"))
  (should (equal (agent-repl--rgb-hex 20 20 26) "#14141a")))

(ert-deftest agent-repl-test-rgb-hex-channels-independent ()
  "rgb-hex keeps each channel distinct rather than collapsing to grey."
  (should (equal (agent-repl--rgb-hex 1 2 3) "#010203")))

(ert-deftest agent-repl-test-set-buffer-background-remaps-default-and-fringe ()
  "set-buffer-background remaps both the default and fringe faces."
  (agent-repl-test--with-temp-buffer " *test-bg*"
    (let ((remapped-faces nil))
      (cl-letf (((symbol-function 'face-remap-add-relative)
                 (lambda (face &rest props)
                   (push (list face props) remapped-faces))))
        (agent-repl--set-buffer-background "#1e1e1e")
        (should (= (length remapped-faces) 2))
        (should (assq 'default remapped-faces))
        (should (assq 'fringe remapped-faces))))))

(ert-deftest agent-repl-test-set-buffer-background-applies-passed-color ()
  "set-buffer-background passes its COLOR argument through as the :background."
  (agent-repl-test--with-temp-buffer " *test-bg-hex*"
    (let ((hex-used nil))
      (cl-letf (((symbol-function 'face-remap-add-relative)
                 (lambda (_face &rest props)
                   (setq hex-used (plist-get props :background)))))
        (agent-repl--set-buffer-background "#0f0f0f")
        (should (equal hex-used "#0f0f0f"))))))

(ert-deftest agent-repl-test-set-buffer-background-different-colors-differ ()
  "Different colors passed to set-buffer-background produce different :background values."
  (let (hex-a hex-b)
    (agent-repl-test--with-temp-buffer " *test-bg-diff-a*"
      (cl-letf (((symbol-function 'face-remap-add-relative)
                 (lambda (_face &rest props)
                   (setq hex-a (plist-get props :background)))))
        (agent-repl--set-buffer-background "#0f0f0f")))
    (agent-repl-test--with-temp-buffer " *test-bg-diff-b*"
      (cl-letf (((symbol-function 'face-remap-add-relative)
                 (lambda (_face &rest props)
                   (setq hex-b (plist-get props :background)))))
        (agent-repl--set-buffer-background "#1e1e1e")))
    (should-not (equal hex-a hex-b))))

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

;;;; ---- Tests: kill-cause attribution ----

(ert-deftest agent-repl-test-kill-cause-str-unbound-is-loud-bug-marker ()
  "An unbound kill-cause renders as a self-documenting BUG marker, so an
unattributed teardown is visible in the log rather than silently blank."
  (let ((agent-repl--kill-cause nil))
    (should (equal (agent-repl--kill-cause-str)
                   "unattributed(BUG: bind agent-repl--kill-cause)"))))

(ert-deftest agent-repl-test-kill-cause-str-returns-bound-cause ()
  "A let-bound kill-cause is returned verbatim for the log line."
  (let ((agent-repl--kill-cause "interactive nuke command (test)"))
    (should (equal (agent-repl--kill-cause-str)
                   "interactive nuke command (test)"))))

;;;; ---- Tests: assert-main-thread ----

(ert-deftest agent-repl-test-assert-main-thread-passes-on-main ()
  "assert-main-thread is a nil-returning no-op on the main thread."
  (should-not (agent-repl--assert-main-thread "op-x")))

(ert-deftest agent-repl-test-assert-main-thread-signals-off-main ()
  "assert-main-thread signals on a worker thread, naming the operation."
  ;; Arrange — capture the outcome of a genuine non-main-thread call.
  (let ((outcome nil))
    ;; Act.
    (thread-join
     (make-thread
      (lambda ()
        (setq outcome
              (condition-case err
                  (agent-repl--assert-main-thread "op-x")
                (error (error-message-string err)))))))
    ;; Assert — the guard fired and the message names the operation.
    (should (stringp outcome))
    (should (string-match-p "REFUSING op-x off the main thread" outcome))))

;;;; ---- Tests: log-sink routability ----

(ert-deftest agent-repl-test-log-routable-rejects-nil-workspace ()
  "nil is the ladder's global-sink value, never a routable workspace."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--ws-log-routable-p nil))))

(ert-deftest agent-repl-test-log-routable-rejects-unregistered-name ()
  "A name absent from the workspace hash owns no durable sink."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--ws-log-routable-p "never-registered"))))

(ert-deftest agent-repl-test-log-routable-rejects-persp-placeholder ()
  "A registered entry without `:project-dir' is a placeholder, not a sink."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "none" :repl-state :inactive)
    (should-not (agent-repl--ws-log-routable-p "none"))))

(ert-deftest agent-repl-test-log-routable-rejects-vanished-project-dir ()
  "A registered `:project-dir' that no longer exists owns no sink."
  (agent-repl-test--with-clean-state
    (let ((project (make-temp-file "agent-repl-routable-gone-" t)))
      (agent-repl--ws-put "gone-ws" :project-dir project)
      (delete-directory project t)
      (should-not (agent-repl--ws-log-routable-p "gone-ws")))))

(ert-deftest agent-repl-test-log-routable-accepts-registered-workspace ()
  "A registered workspace with an existing project directory owns a sink."
  (agent-repl-test--with-clean-state
    (let ((project (make-temp-file "agent-repl-routable-ok-" t)))
      (unwind-protect
          (should (agent-repl--ws-log-routable-p
                   (progn (agent-repl--ws-put "real-ws" :project-dir project)
                          "real-ws")))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-routable-refusal-matches-identity-signal ()
  "A non-nil ws the predicate refuses is one the identity resolver signals on.
Pins the two against drift: the predicate exists so callers can avoid
violating the invariant, which only holds while they agree."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "placeholder-ws" :repl-state :inactive)
    (should-not (agent-repl--ws-log-routable-p "placeholder-ws"))
    (should-error (agent-repl--workspace-log-identity "placeholder-ws"))))

(ert-deftest agent-repl-test-log-routable-acceptance-matches-identity-success ()
  "A ws the predicate accepts never makes the identity resolver signal."
  (agent-repl-test--with-clean-state
    (let ((project (make-temp-file "agent-repl-routable-agree-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "agree-ws" :project-dir project)
            (should (agent-repl--ws-log-routable-p "agree-ws"))
            (should (plist-get (agent-repl--workspace-log-identity "agree-ws")
                               :workspace-id)))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-from-persp-placeholder-reaches-global-sink ()
  "A log line emitted while a persp placeholder is current must not signal.
Regression for the boot failure: `agent-repl--ws-current-name' answers
persp-mode's \"none\" outside any workspace, and routing that name into the
ladder made a debug line abort `doom-init-ui-hook'."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "none"))
                ((symbol-function 'message) #'ignore))
        (agent-repl--log (agent-repl--ws-current-log-name) "placeholder probe")
        (with-temp-buffer
          (insert-file-contents path)
          (should (string-match-p "placeholder probe" (buffer-string))))))))

;;;; ---- Tests: unroutable workspaces degrade instead of signalling ----

(ert-deftest agent-repl-test-deleted-worktree-does-not-abort-its-caller ()
  "A registered workspace whose worktree vanished must not signal.
No call site can prevent this: the caller legitimately owns the workspace
and the directory was deleted underneath it.  Signalling here aborted the
whole snapshot restore at startup."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((project (make-temp-file "agent-repl-vanished-" t))
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        (agent-repl--ws-put "vanished-ws" :project-dir project)
        (delete-directory project t)
        (cl-letf (((symbol-function 'message) #'ignore))
          (agent-repl--log "vanished-ws" "line after the worktree went away"))))))

(ert-deftest agent-repl-test-unroutable-workspace-record-reaches-global-sink ()
  "The record is written, not dropped, when its workspace cannot be routed."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        (cl-letf (((symbol-function 'message) #'ignore))
          (agent-repl--log "no-such-ws" "must still be recorded"))
        (with-temp-buffer
          (insert-file-contents path)
          (should (string-match-p "must still be recorded" (buffer-string))))))))

(ert-deftest agent-repl-test-unroutable-workspace-is-named-in-a-warning ()
  "The degraded routing is announced loudly enough to grep for."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        (cl-letf (((symbol-function 'message) #'ignore))
          (agent-repl--log "no-such-ws" "probe"))
        (with-temp-buffer
          (insert-file-contents path)
          (should (string-match-p "unroutable log workspace" (buffer-string)))
          (should (string-match-p "no-such-ws" (buffer-string))))))))

(ert-deftest agent-repl-test-unroutable-workspace-warns-only-once ()
  "A hot path must not flood the sink with the same routing complaint."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        (cl-letf (((symbol-function 'message) #'ignore))
          (dotimes (_ 5) (agent-repl--log "no-such-ws" "repeated probe")))
        ;; Counted per RECORD, not per occurrence: each JSONL line carries the
        ;; text twice, once as `message' and once inside `context.format'.
        (with-temp-buffer
          (insert-file-contents path)
          (should (= 1 (cl-count-if
                        (lambda (l) (string-match-p "unroutable log workspace" l))
                        (split-string (buffer-string) "\n" t)))))))))

(ert-deftest agent-repl-test-routable-workspace-still-uses-its-own-target ()
  "Degrading an unroutable workspace must not disturb a routable one."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-still-routed-" t))
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "routed-ws" :project-dir project)
            (let ((agent-repl-log-to-file t))
              (cl-letf (((symbol-function 'message) #'ignore))
                (agent-repl--log "routed-ws" "owned line")))
            (should (plist-get (gethash "routed-ws" agent-repl--workspace-log-targets)
                               :target)))
        (delete-directory project t)))))

;;;; ---- Tests: pseudo-perspectives are classified, not warned about ----
;;
;; persp-mode's own perspectives are not agent-repl workspaces, so a record
;; attributed to one is routed globally by `agent-repl--log-sink-workspace'
;; without a warning.  Screening at the router (rather than at each producer)
;; is what stops an unscreened producer from putting the warning back.

(ert-deftest agent-repl-test-pseudo-workspace-record-emits-no-warning ()
  "Doom's startup perspective owns no sink and that is not an anomaly."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((+workspaces-main "main")
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        ;; Act
        (cl-letf (((symbol-function 'message) #'ignore))
          (agent-repl--log "main" "startup perspective probe"))
        ;; Assert
        (with-temp-buffer
          (insert-file-contents path)
          (should-not (string-match-p "unroutable log workspace" (buffer-string))))))))

(ert-deftest agent-repl-test-persp-nil-name-record-emits-no-warning ()
  "persp-mode's nil perspective is the other built-in and warns no more than main."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((persp-nil-name "none")
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        ;; Act
        (cl-letf (((symbol-function 'message) #'ignore))
          (agent-repl--log "none" "nil perspective probe"))
        ;; Assert
        (with-temp-buffer
          (insert-file-contents path)
          (should-not (string-match-p "unroutable log workspace" (buffer-string))))))))

(ert-deftest agent-repl-test-pseudo-workspace-record-reaches-global-sink ()
  "Demoting the attribution must not drop the record."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((+workspaces-main "main")
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        ;; Act
        (cl-letf (((symbol-function 'message) #'ignore))
          (agent-repl--log "main" "still recorded globally"))
        ;; Assert
        (with-temp-buffer
          (insert-file-contents path)
          (should (string-match-p "still recorded globally" (buffer-string))))))))

(ert-deftest agent-repl-test-pseudo-workspace-name-is-preserved-on-the-record ()
  "The routed record still says which perspective it was about."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((+workspaces-main "main")
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        ;; Act
        (cl-letf (((symbol-function 'message) #'ignore))
          (agent-repl--log "main" "named probe"))
        ;; Assert
        (with-temp-buffer
          (insert-file-contents path)
          (should (string-match-p "\"pseudo_workspace\":\"main\"" (buffer-string))))))))

(ert-deftest agent-repl-test-pseudo-workspace-record-carries-no-workspace-identity ()
  "A globally-routed pseudo record must not claim a workspace sink identity."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((+workspaces-main "main")
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        ;; Act
        (cl-letf (((symbol-function 'message) #'ignore))
          (agent-repl--log "main" "identity probe"))
        ;; Assert
        (with-temp-buffer
          (insert-file-contents path)
          (should-not (string-match-p "workspace_id" (buffer-string))))))))

(ert-deftest agent-repl-test-pseudo-name-owns-no-sink-even-when-registered ()
  "A persp built-in never owns a sink, whatever got registered under its name.

THIS REVERSES A PRIOR RULE, deliberately.  The test that stood here asserted
`the pseudo screen never outranks a real registration of the same name', and
that rule is what let the defect through: on 2026-08-11 the live registry
held `main' -> \".../marcos-pr-remediation/\" and
`none' -> \".../slack-cee-ceac-integration-shj/\", so both built-ins were
routable and 60 of 60 `recovery-slo:' records in each of those two
workspaces' durable logs named the perspective instead of the workspace.
The reversal costs nothing real: persp-mode owns \"none\" and Doom owns
\"main\", so an agent-repl workspace cannot hold either name without
colliding with the perspective itself."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-real-main-" t))
           (+workspaces-main "main")
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (puthash "main" (list :project-dir project)
                     agent-repl--workspaces)
            ;; Act / Assert
            (should-not (agent-repl--ws-log-routable-p "main"))
            (should-not (agent-repl--log-sink-workspace "main")))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-pseudo-name-cannot-shadow-a-real-workspace-sink ()
  "A registered pseudo sharing a real workspace's dir never wins its sink.
The shadowing this pins is the one that was measured: the reverse lookup in
`agent-repl--log-canonical-workspace' walks the registry for a dir match, and
with the pseudo routable it could return the perspective for a path that
belongs to a workspace."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-shadowed-" t))
           (+workspaces-main "main"))
      (unwind-protect
          (progn
            (agent-repl--ws-put "real-ws" :project-dir project)
            (puthash "main" (list :project-dir (file-name-as-directory project))
                     agent-repl--workspaces)
            ;; Act
            (let ((resolved (agent-repl--log-canonical-workspace project)))
              ;; Assert
              (should (equal "real-ws" resolved))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-identity-resolver-still-signals-when-called-directly ()
  "The invariant itself is unchanged; only the ladder screens before calling it.
Keeping this coverage means a direct caller that skips the screen is still
caught loudly."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--workspace-log-identity "never-registered"))))

;;;; ---- Tests: the log sink does not instrument itself ----

(ert-deftest agent-repl-test-workspace-scoped-record-emits-no-buffer-name-record ()
  "Writing one workspace-scoped record must not produce a second record.
`agent-repl--append-workspace-log' documents that it does not instrument its
own buffer resolution, but the buffer is NAMED by `agent-repl--buffer-name',
which logged — so every workspace-scoped line emitted a `buffer-name:' line
too.  In production that doubling put `buffer-name: suffix=-log' third in the
log at 71,425 records."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((project (make-temp-file "agent-repl-no-amplify-" t))
            (agent-repl--workspace-log-buffer-enabled t)
            (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
        (unwind-protect
            (progn
              (agent-repl--ws-put "amp-ws" :project-dir project)
              (cl-letf (((symbol-function 'message) #'ignore))
                (agent-repl--log "amp-ws" "single line"))
              (with-temp-buffer
                (insert-file-contents path)
                (should (= 0 (cl-count-if
                              (lambda (l) (string-match-p "buffer-name: suffix=-log" l))
                              (split-string (buffer-string) "\n" t))))))
          (delete-directory project t))))))

(ert-deftest agent-repl-test-buffer-name-still-logs-for-ordinary-callers ()
  "Silencing the sink's own path must not silence genuine callers."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      ;; `agent-repl--buffer-name' reports on the verbose rung, so the durable
      ;; threshold is opened: the subject is the sink's self-silencing, not the
      ;; level gate.
      (let ((agent-repl-log-file-level 'verbose)
            (project (make-temp-file "agent-repl-still-logs-" t)))
        (unwind-protect
            (progn
              (agent-repl--ws-put "named-ws" :project-dir project)
              (cl-letf (((symbol-function 'message) #'ignore))
                (agent-repl--buffer-name "-view" "named-ws"))
              (with-temp-buffer
                (insert-file-contents path)
                (should (string-match-p "buffer-name: suffix=-view" (buffer-string)))))
          (delete-directory project t))))))

;;;; ---- Tests: keyed timer registry ----

(defmacro agent-repl-test--with-timer-registry (&rest body)
  "Run BODY against fresh, isolated timer registries.
Any timer BODY armed is cancelled on the way out, so no real timer
survives into the rest of the batch run."
  (declare (indent 0))
  `(let ((agent-repl--timers nil)
         (agent-repl--keyed-timers nil))
     (unwind-protect (progn ,@body)
       (agent-repl--cancel-all-timers))))

(defun agent-repl-test--armed-timer ()
  "Return a genuinely scheduled timer that will not fire during the run."
  (run-with-timer 3600 nil #'ignore))

(defvar agent-repl-test--fake-heartbeat-arm-count 0
  "Number of times `agent-repl-test--arm-fake-heartbeat' has been called.")

(defun agent-repl-test--arm-fake-heartbeat ()
  "Arm a stand-in heartbeat under `:test-heartbeat', counting the call."
  (setq agent-repl-test--fake-heartbeat-arm-count
        (1+ agent-repl-test--fake-heartbeat-arm-count))
  (agent-repl--register-timer :test-heartbeat (agent-repl-test--armed-timer)))

(ert-deftest agent-repl-test-register-timer-replaces-rather-than-stacks ()
  "Re-arming a key cancels the prior timer instead of stacking a duplicate."
  ;; Arrange
  (agent-repl-test--with-timer-registry
    (let ((first (agent-repl--register-timer :test-key (agent-repl-test--armed-timer))))
      ;; Act
      (let ((second (agent-repl--register-timer :test-key (agent-repl-test--armed-timer))))
        ;; Assert
        (should (equal 1 (length agent-repl--timers)))
        (should (equal 1 (length agent-repl--keyed-timers)))
        (should (eq second (cdr (assq :test-key agent-repl--keyed-timers))))
        (should-not (memq first timer-list))))))

(ert-deftest agent-repl-test-cancel-all-then-one-arm-per-key-yields-one-each ()
  "Cancel-all followed by a single arm per key leaves exactly one timer per key."
  ;; Arrange
  (agent-repl-test--with-timer-registry
    (agent-repl--register-timer :key-a (agent-repl-test--armed-timer))
    (agent-repl--register-timer :key-b (agent-repl-test--armed-timer))
    (agent-repl--cancel-all-timers)
    ;; Act
    (agent-repl--register-timer :key-a (agent-repl-test--armed-timer))
    (agent-repl--register-timer :key-b (agent-repl-test--armed-timer))
    ;; Assert
    (should (equal 2 (length agent-repl--timers)))
    (should (equal 2 (length agent-repl--keyed-timers)))
    (should (agent-repl--timer-armed-p :key-a))
    (should (agent-repl--timer-armed-p :key-b))))

(ert-deftest agent-repl-test-cancel-all-timers-clears-the-keyed-registry ()
  "Cancel-all empties the keyed registry, not just the flat timer list."
  ;; Arrange
  (agent-repl-test--with-timer-registry
    (agent-repl--register-timer :test-key (agent-repl-test--armed-timer))
    ;; Act
    (agent-repl--cancel-all-timers)
    ;; Assert
    (should (null agent-repl--keyed-timers))))

(ert-deftest agent-repl-test-cancel-timer-key-deregisters-only-that-key ()
  "Cancelling one key leaves every other key's timer armed."
  ;; Arrange
  (agent-repl-test--with-timer-registry
    (agent-repl--register-timer :key-a (agent-repl-test--armed-timer))
    (agent-repl--register-timer :key-b (agent-repl-test--armed-timer))
    ;; Act
    (should (agent-repl--cancel-timer-key :key-a))
    ;; Assert
    (should-not (agent-repl--timer-armed-p :key-a))
    (should (agent-repl--timer-armed-p :key-b))))

(ert-deftest agent-repl-test-register-timer-signals-on-a-non-timer ()
  "A caller handing something that is not a timer is a bug and must signal."
  ;; Arrange
  (agent-repl-test--with-timer-registry
    ;; Act / Assert
    (should-error (agent-repl--register-timer :test-key "not-a-timer") :type 'error)
    (should (null agent-repl--keyed-timers))))

(ert-deftest agent-repl-test-register-timer-signals-on-a-nil-key ()
  "A nil KEY would make the registry unaddressable, so it must signal."
  ;; Arrange
  (agent-repl-test--with-timer-registry
    (let ((timer (agent-repl-test--armed-timer)))
      (unwind-protect
          ;; Act / Assert
          (should-error (agent-repl--register-timer nil timer) :type 'error)
        (cancel-timer timer)))))

;;;; ---- Tests: heartbeat assertion ----

(ert-deftest agent-repl-test-assert-heartbeat-rearms-a-stranded-key ()
  "A key with no live timer is re-armed through its owner's arm function."
  ;; Arrange
  (agent-repl-test--with-timer-registry
    (let ((warnings nil)
          (agent-repl--required-timer-keys
           '((:test-heartbeat . agent-repl-test--arm-fake-heartbeat)))
          (agent-repl-test--fake-heartbeat-arm-count 0))
      (cl-letf (((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warnings))))
        ;; Act
        (let ((result (agent-repl--assert-heartbeat-armed)))
          ;; Assert
          (should (equal '(:test-heartbeat) (plist-get result :rearmed)))
          (should (equal 1 agent-repl-test--fake-heartbeat-arm-count))
          (should (agent-repl--timer-armed-p :test-heartbeat)))))))

(ert-deftest agent-repl-test-assert-heartbeat-warns-naming-the-stranded-key ()
  "The re-arm is reported loudly, naming the key that was stranded."
  ;; Arrange
  (agent-repl-test--with-timer-registry
    (let ((warnings nil)
          (agent-repl--required-timer-keys
           '((:test-heartbeat . agent-repl-test--arm-fake-heartbeat)))
          (agent-repl-test--fake-heartbeat-arm-count 0))
      (cl-letf (((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warnings))))
        ;; Act
        (agent-repl--assert-heartbeat-armed)
        ;; Assert
        (should (cl-some (lambda (w)
                           (string-match-p "key=:test-heartbeat outcome=stranded" w))
                         warnings))))))

(ert-deftest agent-repl-test-assert-heartbeat-is-quiet-when-all-armed ()
  "An already-armed contract produces no warning and no re-arm."
  ;; Arrange
  (agent-repl-test--with-timer-registry
    (let ((warnings nil)
          (agent-repl--required-timer-keys
           '((:test-heartbeat . agent-repl-test--arm-fake-heartbeat)))
          (agent-repl-test--fake-heartbeat-arm-count 0))
      (agent-repl--register-timer :test-heartbeat (agent-repl-test--armed-timer))
      (cl-letf (((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warnings))))
        ;; Act
        (let ((result (agent-repl--assert-heartbeat-armed)))
          ;; Assert
          (should (equal '(:test-heartbeat) (plist-get result :armed)))
          (should (null (plist-get result :rearmed)))
          (should (equal 0 agent-repl-test--fake-heartbeat-arm-count))
          (should (null warnings)))))))

(ert-deftest agent-repl-test-assert-heartbeat-reports-an-unloaded-owner ()
  "A key whose owner file is not loaded is reported, never silently passed."
  ;; Arrange
  (agent-repl-test--with-timer-registry
    (let ((agent-repl--required-timer-keys
           '((:test-heartbeat . agent-repl-test--arm-fn-that-does-not-exist))))
      (cl-letf (((symbol-function 'agent-repl--warn) (lambda (&rest _) nil)))
        ;; Act
        (let ((result (agent-repl--assert-heartbeat-armed)))
          ;; Assert
          (should (equal '(:test-heartbeat) (plist-get result :unavailable))))))))

(ert-deftest agent-repl-test-heartbeat-assertion-defers-on-a-cold-load ()
  "A cold load (owners not yet defined) defers instead of erroring or stranding."
  ;; Arrange — this is exactly core.el's own load-time state in a fresh
  ;; batch process: core.el is evaluated before status.el, autosave.el,
  ;; workspace-status-export.el, and readiness.el define the arm functions.
  (agent-repl-test--with-timer-registry
    (let ((agent-repl--required-timer-keys
           '((:test-heartbeat . agent-repl-test--arm-fn-that-does-not-exist)))
          (agent-repl--heartbeat-assert-deferral-timer nil)
          (scheduled nil))
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (&rest _) (setq scheduled t) (timer-create))))
        ;; Act
        (let ((outcome (agent-repl--assert-heartbeat-armed-when-owners-load)))
          ;; Assert
          (should (eq outcome :deferred))
          (should scheduled)
          (should (timerp agent-repl--heartbeat-assert-deferral-timer)))))))

(ert-deftest agent-repl-test-heartbeat-assertion-runs-immediately-when-owners-exist ()
  "With every arm function defined the check runs inline rather than deferring."
  ;; Arrange — a bare core.el hot-load into a RUNNING Emacs looks like this.
  (agent-repl-test--with-timer-registry
    (let ((agent-repl--required-timer-keys
           '((:test-heartbeat . agent-repl-test--arm-fake-heartbeat)))
          (agent-repl-test--fake-heartbeat-arm-count 0)
          (agent-repl--heartbeat-assert-deferral-timer nil))
      (cl-letf (((symbol-function 'agent-repl--warn) (lambda (&rest _) nil)))
        ;; Act
        (let ((outcome (agent-repl--assert-heartbeat-armed-when-owners-load)))
          ;; Assert
          (should (eq :checked (car outcome)))
          (should (equal '(:test-heartbeat) (plist-get (cdr outcome) :rearmed)))
          (should (null agent-repl--heartbeat-assert-deferral-timer)))))))


(provide 'test-core)

;;; test-core.el ends here

;;;; ---- Tests: durable log level threshold ----
;;
;; The knob exists because `agent-repl-debug' governs *Messages* only: the
;; verbose rung was reaching the file unconditionally, and nothing short of
;; the all-or-nothing kill-switch would stop it.  One edge per test.

(defmacro agent-repl-test--with-captured-file-writes (level &rest body)
  "Run BODY with `agent-repl-log-file-level' at LEVEL, collecting file writes."
  (declare (indent 2))
  `(let ((writes 0)
         (agent-repl-log-to-file t)
         (agent-repl-log-file-level ,level))
     (cl-letf (((symbol-function 'agent-repl--do-log-to-file)
                (lambda (&rest _args) (setq writes (1+ writes)))))
       ,@body)
     writes))

(ert-deftest agent-repl-test-log-file-level-drops-verbose-by-default ()
  "The default threshold keeps hot-path chatter out of the durable sink."
  ;; Arrange / Act
  (let ((writes (agent-repl-test--with-captured-file-writes 'debug
                  (agent-repl--log-verbose nil "chatter %s" "x"))))
    ;; Assert
    (should (= writes 0))))

(ert-deftest agent-repl-test-log-file-level-keeps-debug-at-the-default ()
  "The default threshold still records ordinary debug lines."
  ;; Arrange / Act
  (let ((writes (agent-repl-test--with-captured-file-writes 'debug
                  (agent-repl--log nil "ordinary %s" "x"))))
    ;; Assert
    (should (= writes 1))))

(ert-deftest agent-repl-test-log-file-level-verbose-restores-old-behavior ()
  "Setting the threshold to verbose writes the chatter again."
  ;; Arrange / Act
  (let ((writes (agent-repl-test--with-captured-file-writes 'verbose
                  (agent-repl--log-verbose nil "chatter %s" "x"))))
    ;; Assert
    (should (= writes 1))))

(ert-deftest agent-repl-test-log-file-level-warn-drops-info ()
  "A warn threshold drops the info rung beneath it."
  ;; Arrange / Act
  (let ((writes (agent-repl-test--with-captured-file-writes 'warn
                  (agent-repl--info nil "notice %s" "x"))))
    ;; Assert
    (should (= writes 0))))

(ert-deftest agent-repl-test-log-file-level-never-drops-errors ()
  "The most severe threshold still records errors."
  ;; Arrange / Act
  (let ((writes (agent-repl-test--with-captured-file-writes 'error
                  (ignore-errors (agent-repl--do-log nil "boom" nil t)))))
    ;; Assert
    (should (= writes 1))))

(ert-deftest agent-repl-test-log-file-level-ranks-unknown-levels-at-the-top ()
  "An unrecognized level is never what a threshold silently discards."
  ;; Arrange / Act / Assert — ranked with `error', so it clears every setting.
  (should (agent-repl--log-record-persists-p "no-such-level" "normal")))

(ert-deftest agent-repl-test-log-file-level-verbose-outranks-its-carried-level ()
  "A verbose record ranks by its verbosity, not by the level it carries."
  ;; Arrange / Act / Assert — `agent-repl--log-verbose' stamps records `debug',
  ;; so ranking by level alone would let all the chatter through at the default.
  (let ((agent-repl-log-file-level 'debug))
    (should-not (agent-repl--log-record-persists-p "debug" "verbose"))))

(ert-deftest agent-repl-test-log-file-level-leaves-messages-visibility-alone ()
  "The durable threshold does not change what reaches *Messages*."
  ;; Arrange
  (let ((messaged nil)
        (agent-repl-log-to-file t)
        (agent-repl-log-file-level 'error)
        (agent-repl-debug 'verbose))
    (cl-letf (((symbol-function 'agent-repl--do-log-to-file) #'ignore)
              ((symbol-function 'message)
               (lambda (&rest _args) (setq messaged t))))
      ;; Act — the record is far below the durable threshold.
      (agent-repl--log-verbose nil "chatter %s" "x")
      ;; Assert — visibility is the other knob's business, and it still fired.
      (should messaged))))

;;;; ---- Tests: workspace log BUFFER threshold ----
;;
;; The buffers are read live by a human while debugging; the file is a forensic
;; record nobody reads top to bottom.  They are gated separately so a quiet
;; buffer never costs a complete file.

(defmacro agent-repl-test--with-log-buffer (ws &rest body)
  "Run BODY with WS's live log buffer enabled, returning its contents."
  (declare (indent 1))
  `(agent-repl-test--with-clean-state
     (let ((project (make-temp-file "agent-repl-buffer-level-" t))
           (buf nil))
       (unwind-protect
           (let ((agent-repl-log-to-file nil)
                 (agent-repl-debug nil))
             (agent-repl--ws-put ,ws :project-dir project)
             (let ((agent-repl--workspace-log-buffer-enabled t))
               ,@body)
             (setq buf (agent-repl--workspace-log-buffer ,ws))
             (with-current-buffer buf (buffer-string)))
         (when (buffer-live-p buf) (kill-buffer buf))
         (delete-directory project t)))))

(ert-deftest agent-repl-test-log-buffer-level-drops-verbose-by-default ()
  "Hot-path chatter stays out of the buffer a human is reading."
  ;; Arrange / Act
  (let ((contents (agent-repl-test--with-log-buffer "buf-level-ws"
                    (agent-repl--log-verbose "buf-level-ws" "chatter"))))
    ;; Assert
    (should (equal contents ""))))

(ert-deftest agent-repl-test-log-buffer-level-drops-ordinary-debug-by-default ()
  "The buffers are stricter than the file: they are for what went WRONG."
  ;; Arrange / Act
  (let ((contents (agent-repl-test--with-log-buffer "buf-level-ws"
                    (agent-repl--log "buf-level-ws" "ordinary"))))
    ;; Assert
    (should (equal contents ""))))

(ert-deftest agent-repl-test-log-buffer-level-keeps-warnings-by-default ()
  "A warning is exactly what the default threshold exists to surface."
  ;; Arrange / Act
  (let ((contents (agent-repl-test--with-log-buffer "buf-level-ws"
                    (agent-repl--warn "buf-level-ws" "something wrong"))))
    ;; Assert
    (should (string-match-p "something wrong" contents))))

(ert-deftest agent-repl-test-log-buffer-level-debug-follows-ordinary-activity ()
  "Lowering the buffer threshold puts the ordinary lines back."
  ;; Arrange / Act
  (let ((contents (let ((agent-repl-log-buffer-level 'debug))
                    (agent-repl-test--with-log-buffer "buf-level-ws"
                      (agent-repl--log "buf-level-ws" "ordinary")))))
    ;; Assert
    (should (string-match-p "ordinary" contents))))

(ert-deftest agent-repl-test-log-buffer-level-verbose-admits-chatter ()
  "Opening the buffer threshold puts the chatter back."
  ;; Arrange / Act
  (let ((contents (let ((agent-repl-log-buffer-level 'verbose))
                    (agent-repl-test--with-log-buffer "buf-level-ws"
                      (agent-repl--log-verbose "buf-level-ws" "chatter")))))
    ;; Assert
    (should (string-match-p "chatter" contents))))

(ert-deftest agent-repl-test-log-buffer-level-independent-of-the-file-level ()
  "A quiet buffer does not cost a complete file."
  ;; Arrange — the file admits a rung the buffer does not.
  (let ((writes 0)
        (agent-repl-log-to-file t)
        (agent-repl-log-file-level 'debug)
        (agent-repl-log-buffer-level 'warn))
    (cl-letf (((symbol-function 'agent-repl--do-log-to-file)
               (lambda (&rest _args) (setq writes (1+ writes)))))
      ;; Act
      (agent-repl--log nil "ordinary")
      ;; Assert — the file recorded it even though no buffer would show it.
      (should (= writes 1)))))

;;;; ---- Tests: backend-initiation phase surfacing ----
;;
;; The backend-initiation ladder (artifact builds, launchd kickstarts, the
;; daemon spawn and the runtime bounce) is the ONE background flow allowed to
;; reach the echo area, and `agent-repl--backend-phase' is its only door.  A
;; regression here is invisible by construction: the user sees nothing at all
;; while a synchronous build blocks the frame.

(ert-deftest agent-repl-test-backend-output-tail-names-an-empty-capture ()
  "An empty capture is reported as empty rather than as an absent field."
  ;; Arrange / Act
  (let ((tail (agent-repl--backend-output-tail "")))
    ;; Assert
    (should (equal tail "<no output>"))))

(ert-deftest agent-repl-test-backend-output-tail-keeps-only-the-last-lines ()
  "Only the trailing nonblank lines survive into the echo fragment."
  ;; Arrange
  (let ((output (mapconcat #'number-to-string (number-sequence 1 20) "\n")))
    ;; Act
    (let ((tail (agent-repl--backend-output-tail output 3)))
      ;; Assert
      (should (equal tail "18 / 19 / 20")))))

(ert-deftest agent-repl-test-backend-output-tail-truncates-an-overlong-line ()
  "A single enormous line is truncated so one echo line cannot be flooded."
  ;; Arrange
  (let ((output (make-string 2000 ?x)))
    ;; Act
    (let ((tail (agent-repl--backend-output-tail output)))
      ;; Assert
      (should (= (length tail) (1+ agent-repl--backend-output-tail-limit)))
      (should (string-prefix-p "…" tail)))))

(ert-deftest agent-repl-test-backend-output-tail-suffix-keeps-a-short-string-whole ()
  "A capture already inside the scan bound is passed through untouched."
  ;; Arrange
  (let ((output "one\ntwo\n"))
    ;; Act
    (let ((suffix (agent-repl--backend-output-tail-suffix output)))
      ;; Assert
      (should (eq suffix output)))))

(ert-deftest agent-repl-test-backend-output-tail-suffix-bounds-a-huge-string ()
  "An oversized capture is cut down to exactly the scan bound."
  ;; Arrange
  (let ((output (make-string (* 4 agent-repl--backend-output-tail-scan-limit) ?x)))
    ;; Act
    (let ((suffix (agent-repl--backend-output-tail-suffix output)))
      ;; Assert
      (should (= (length suffix) agent-repl--backend-output-tail-scan-limit)))))

(ert-deftest agent-repl-test-backend-output-tail-matches-its-bounded-suffix ()
  "A multi-megabyte multiline capture yields the same tail as its suffix.
The bound is what keeps the helper off the quadratic `split-string' path
that froze Emacs; this pins the equivalence the bound relies on."
  ;; Arrange — multibyte content, since char-to-byte indexing is the cost.
  (let* ((filler (mapconcat (lambda (i) (format "récord %d ————" i))
                            (number-sequence 1 120000) "\n"))
         (output (concat filler "\nlast-one\nlast-two")))
    (should (> (length output) (* 2 1024 1024)))
    ;; Act
    (let ((tail (agent-repl--backend-output-tail output))
          (suffix-tail (agent-repl--backend-output-tail
                        (agent-repl--backend-output-tail-suffix output))))
      ;; Assert
      (should (equal tail suffix-tail)))))

(ert-deftest agent-repl-test-backend-output-tail-keeps-the-final-lines-of-a-huge-capture ()
  "The trailing lines of a multi-megabyte capture still reach the echo tail."
  ;; Arrange
  (let ((output (concat (make-string (* 3 1024 1024) ?x) "\nfinal-line")))
    ;; Act
    (let ((tail (agent-repl--backend-output-tail output 1)))
      ;; Assert
      (should (equal tail "final-line")))))

(ert-deftest agent-repl-test-backend-phase-persists-a-structured-record ()
  "A phase transition reaches the durable sink at the info rung."
  ;; Arrange
  (let (records)
    (cl-letf (((symbol-function 'agent-repl--persist-log-record)
               (lambda (ws level verbosity fmt args)
                 (push (list ws level verbosity fmt args) records)))
              ((symbol-function 'agent-repl--emit-message) #'ignore))
      ;; Act
      (agent-repl--backend-phase nil "daemon up (pid %s)" 41)
      ;; Assert
      (should (equal (car records)
                     (list nil "info" "normal" "daemon up (pid %s)" '(41)))))))

(ert-deftest agent-repl-test-backend-phase-reaches-the-echo-area ()
  "A phase transition is echoed loudly, not filed quietly like other chatter."
  ;; Arrange
  (let (emitted)
    (cl-letf (((symbol-function 'agent-repl--persist-log-record) #'ignore)
              ((symbol-function 'agent-repl--emit-message)
               (lambda (text &optional echo) (setq emitted (cons text echo)))))
      ;; Act
      (agent-repl--backend-phase nil "daemon up (pid %s)" 41)
      ;; Assert
      (should (equal emitted (cons "agent-repl: daemon up (pid 41)" t))))))

;;;; ---- Tests: the registration window is classified, not warned about ----
;;
;; A workspace is created before it is registered, and its creator logs
;; throughout that window.  Those records correctly name a workspace that owns
;; no sink yet, so a perfectly normal creation used to warn about its own
;; prologue.  The window is declared by its creator and classified here.

(ert-deftest agent-repl-test-preregistration-record-emits-no-warning ()
  "A record inside the declared registration window is not an anomaly."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      ;; Arrange
      (let ((agent-repl--log-preregistration-workspace "being-created")
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal))
            (agent-repl--preregistration-log-workspaces (make-hash-table :test #'equal)))
        ;; Act
        (cl-letf (((symbol-function 'message) #'ignore))
          (agent-repl--log "being-created" "creation prologue probe"))
        ;; Assert
        (with-temp-buffer
          (insert-file-contents path)
          (should-not (string-match-p "unroutable log workspace" (buffer-string))))))))

(ert-deftest agent-repl-test-preregistration-record-reaches-the-global-sink ()
  "Classifying the attribution must not drop the record."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      ;; Arrange
      (let ((agent-repl--log-preregistration-workspace "being-created")
            (agent-repl--preregistration-log-workspaces (make-hash-table :test #'equal)))
        ;; Act
        (cl-letf (((symbol-function 'message) #'ignore))
          (agent-repl--log "being-created" "must still be recorded"))
        ;; Assert
        (with-temp-buffer
          (insert-file-contents path)
          (should (string-match-p "must still be recorded" (buffer-string))))))))

(ert-deftest agent-repl-test-preregistration-window-is-announced-once ()
  "The window is announced, and only once, so the sink question has an answer."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      ;; Arrange
      (let ((agent-repl--log-preregistration-workspace "being-created")
            (agent-repl--preregistration-log-workspaces (make-hash-table :test #'equal)))
        ;; Act
        (cl-letf (((symbol-function 'message) #'ignore))
          (dotimes (_ 5) (agent-repl--log "being-created" "repeated probe")))
        ;; Assert — counted per RECORD; each JSONL line carries the text twice.
        (with-temp-buffer
          (insert-file-contents path)
          (should (= 1 (cl-count-if
                        (lambda (l) (string-match-p "pre-registration log workspace" l))
                        (split-string (buffer-string) "\n" t)))))))))

(ert-deftest agent-repl-test-preregistration-announcement-is-not-a-warning ()
  "The global sink is where these records BELONG, so this is info, not warn."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      ;; Arrange
      (let ((agent-repl--log-preregistration-workspace "being-created")
            (agent-repl--preregistration-log-workspaces (make-hash-table :test #'equal)))
        ;; Act
        (cl-letf (((symbol-function 'message) #'ignore))
          (agent-repl--log "being-created" "probe"))
        ;; Assert
        (with-temp-buffer
          (insert-file-contents path)
          (let ((line (car (cl-remove-if-not
                            (lambda (l) (string-match-p "pre-registration log workspace" l))
                            (split-string (buffer-string) "\n" t)))))
            (should line)
            (should (string-match-p "\"level\":\"info\"" line))))))))

(ert-deftest agent-repl-test-preregistration-window-covers-only-its-own-name ()
  "An unrelated unregistered workspace still warns while a window is open."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      ;; Arrange
      (let ((agent-repl--log-preregistration-workspace "being-created")
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal))
            (agent-repl--preregistration-log-workspaces (make-hash-table :test #'equal)))
        ;; Act
        (cl-letf (((symbol-function 'message) #'ignore))
          (agent-repl--log "some-other-ws" "probe"))
        ;; Assert
        (with-temp-buffer
          (insert-file-contents path)
          (should (string-match-p "unroutable log workspace" (buffer-string))))))))

(ert-deftest agent-repl-test-record-after-the-window-closes-still-warns ()
  "The classification lasts exactly as long as the creator holds the binding."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      ;; Arrange
      (let ((agent-repl--log-preregistration-workspace nil)
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal))
            (agent-repl--preregistration-log-workspaces (make-hash-table :test #'equal)))
        ;; Act
        (cl-letf (((symbol-function 'message) #'ignore))
          (agent-repl--log "being-created" "probe after the window"))
        ;; Assert
        (with-temp-buffer
          (insert-file-contents path)
          (should (string-match-p "unroutable log workspace" (buffer-string))))))))

(ert-deftest agent-repl-test-registered-workspace-ignores-an-open-window ()
  "A workspace that owns a sink routes to it, window open or not."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let* ((project (make-temp-file "agent-repl-window-routed-" t))
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "routed-ws" :project-dir project)
            ;; Act
            (let ((agent-repl-log-to-file t)
                  (agent-repl--log-preregistration-workspace "routed-ws"))
              (cl-letf (((symbol-function 'message) #'ignore))
                (agent-repl--log "routed-ws" "owned line")))
            ;; Assert
            (should (plist-get (gethash "routed-ws" agent-repl--workspace-log-targets)
                               :target)))
        (delete-directory project t)))))

;;;; ---- Tests: user-facing minibuffer copy ----
;;
;; The policy these cover: the echo area carries ONE short sentence, the log
;; carries that sentence AND the verbose counterpart, and no raw daemon error
;; chain ever reaches the minibuffer.

(ert-deftest agent-repl-test-user-message-echoes-prefixed-user-copy ()
  "`agent-repl--user-message' echoes the user copy under the module prefix."
  ;; Arrange
  (let ((echoed nil))
    (cl-letf (((symbol-function 'agent-repl--emit-message)
               (lambda (text &optional _echo) (setq echoed text)))
              ((symbol-function 'agent-repl--log) #'ignore))
      ;; Act
      (agent-repl--user-message nil "prompt refused — %s" '("queued for a merge"))
      ;; Assert
      (should (equal echoed
                     "agent-repl: prompt refused — queued for a merge")))))

(ert-deftest agent-repl-test-user-message-reaches-the-echo-area ()
  "The user line is the LOUD sink: it is emitted with ECHO non-nil."
  ;; Arrange
  (let ((echo-flag 'unset))
    (cl-letf (((symbol-function 'agent-repl--emit-message)
               (lambda (_text &optional echo) (setq echo-flag echo)))
              ((symbol-function 'agent-repl--log) #'ignore))
      ;; Act
      (agent-repl--user-message nil "something happened" nil)
      ;; Assert
      (should echo-flag))))

(ert-deftest agent-repl-test-user-message-does-not-double-prefix ()
  "Copy that already carries the prefix is echoed unchanged."
  ;; Arrange
  (let ((echoed nil))
    (cl-letf (((symbol-function 'agent-repl--emit-message)
               (lambda (text &optional _echo) (setq echoed text)))
              ((symbol-function 'agent-repl--log) #'ignore))
      ;; Act
      (agent-repl--user-message nil "agent-repl: already prefixed" nil)
      ;; Assert
      (should (equal echoed "agent-repl: already prefixed")))))

(ert-deftest agent-repl-test-user-message-files-both-lines-globally ()
  "A nil-workspace call files the user line AND the detail to the global sink."
  ;; Arrange
  (let* ((tmpdir (make-temp-file "agent-repl-user-message-" t))
         (logpath (expand-file-name ".agent-repl.log" tmpdir))
         (agent-repl-log-to-file t))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--logfile-path) (lambda () logpath))
                  ((symbol-function 'agent-repl--emit-message) #'ignore))
          ;; Act
          (agent-repl--user-message nil "prompt refused — queued for a merge" nil
                                    :detail "ssm: invariant failed state=RENDER_STATE_MERGE_QUEUED")
          ;; Assert
          (let ((contents (with-temp-buffer
                            (insert-file-contents logpath)
                            (buffer-string))))
            (should (string-match-p "prompt refused — queued for a merge" contents))
            (should (string-match-p "RENDER_STATE_MERGE_QUEUED" contents))))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-user-message-files-both-lines-for-a-workspace ()
  "A workspace-scoped call files both lines through that workspace's own sink."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let* ((project (make-temp-file "agent-repl-user-message-ws-" t))
           (ws "user-copy-ws")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :project-dir project)
            ;; Act
            (let ((agent-repl-log-to-file t))
              (cl-letf (((symbol-function 'agent-repl--emit-message) #'ignore))
                (agent-repl--user-message ws "prompt refused — queued for a merge" nil
                                          :detail "state=RENDER_STATE_MERGE_QUEUED turn_active=true")))
            ;; Assert
            (let* ((target (plist-get (gethash ws agent-repl--workspace-log-targets) :target))
                   (contents (with-temp-buffer
                               (insert-file-contents target)
                               (buffer-string))))
              (should (string-match-p "prompt refused — queued for a merge" contents))
              (should (string-match-p "state=RENDER_STATE_MERGE_QUEUED turn_active=true"
                                      contents))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-user-message-nil-detail-files-only-the-user-line ()
  "With no DETAIL there is no second record to write."
  ;; Arrange
  (let ((logged nil))
    (cl-letf (((symbol-function 'agent-repl--emit-message) #'ignore)
              ((symbol-function 'agent-repl--log)
               (lambda (ws fmt &rest args) (push (list ws (apply #'format fmt args)) logged))))
      ;; Act
      (agent-repl--user-message "ws" "nothing verbose here" nil)
      ;; Assert
      (should (equal logged '(("ws" "user-message: agent-repl: nothing verbose here")))))))

(ert-deftest agent-repl-test-user-message-empty-detail-files-only-the-user-line ()
  "An empty DETAIL string is treated as no detail, not as a blank record."
  ;; Arrange
  (let ((logged nil))
    (cl-letf (((symbol-function 'agent-repl--emit-message) #'ignore)
              ((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
      ;; Act
      (agent-repl--user-message nil "terse" nil :detail "")
      ;; Assert
      (should (= 1 (length logged))))))

(ert-deftest agent-repl-test-user-message-non-string-format-is-captured-not-signalled ()
  "A caller bug is captured the way the logging ladder captures it, not signalled."
  ;; Arrange
  (let ((captured nil)
        (echoed nil))
    (cl-letf (((symbol-function 'agent-repl--log-format-capture-bug)
               (lambda (fmt) (setq captured fmt)))
              ((symbol-function 'agent-repl--log) #'ignore)
              ((symbol-function 'agent-repl--emit-message)
               (lambda (text &optional _echo) (setq echoed text))))
      ;; Act
      (agent-repl--user-message nil '(not a string) nil)
      ;; Assert
      (should (equal captured '(not a string)))
      (should (string-prefix-p "agent-repl: " echoed)))))

;;;; ---- Tests: daemon-error translation ----

(ert-deftest agent-repl-test-user-copy-translates-a-merge-queued-refusal ()
  "The observed merge-queued nack becomes the queued-for-a-merge sentence."
  ;; Arrange
  (let ((raw (concat "session-controller: synchronous state publication failed before "
                     "submitting for workspace \"/tmp/ws\" session \"s_ea\" request \"fe-79\": "
                     "ssm: synchronous prompt state invariant failed for workspace \"/tmp/ws\" "
                     "session \"s_ea\" request \"fe-79\": "
                     "state=RENDER_STATE_MERGE_QUEUED turn_active=true")))
    ;; Act
    (let ((copy (agent-repl--user-copy-for-error raw "prompt")))
      ;; Assert
      (should (equal copy
                     (concat "prompt refused — this workspace is queued for a merge; "
                             "wait for the merge to finish or interrupt it"))))))

(ert-deftest agent-repl-test-user-copy-never-quotes-the-raw-chain ()
  "The translated sentence carries none of the raw chain's evidence."
  ;; Arrange
  (let ((raw "ssm: invariant failed for workspace \"/tmp/ws\": state=RENDER_STATE_MERGE_QUEUED"))
    ;; Act
    (let ((copy (agent-repl--user-copy-for-error raw "prompt")))
      ;; Assert
      (should-not (string-match-p "RENDER_STATE" copy))
      (should-not (string-match-p "ssm:" copy)))))

(ert-deftest agent-repl-test-user-copy-translates-a-merge-lease-refusal ()
  "A merge-lease refusal says a merge run owns the session."
  ;; Arrange
  (let ((raw (concat "session-controller: the workspace's session is held by a merge "
                     "exclusivity lease and cannot be hibernated: workspace \"/tmp/ws\"")))
    ;; Act
    (let ((copy (agent-repl--user-copy-for-error raw "hibernate")))
      ;; Assert
      (should (equal copy
                     (concat "hibernate refused — a merge run owns this session; "
                             "wait for the merge to finish or interrupt it"))))))

(ert-deftest agent-repl-test-user-copy-translates-a-not-live-refusal ()
  "A workspace with no live session gets the not-live sentence."
  ;; Act
  (let ((copy (agent-repl--user-copy-for-error
               "shimclient: request nacked: workspace \"doom\" has no live session to drive"
               "prompt")))
    ;; Assert
    (should (equal copy
                   "prompt refused — this workspace is not live; start or wake it first"))))

(ert-deftest agent-repl-test-user-copy-translates-a-reconnect-supersession ()
  "A superseded reconnect promises a resync rather than reporting a fault."
  ;; Act
  (let ((copy (agent-repl--user-copy-for-error
               "session-controller: command superseded by the current workspace generation"
               "reconnect")))
    ;; Assert
    (should (equal copy
                   (concat "reconnect superseded — a newer connection owns this "
                           "workspace; the view will resync")))))

(ert-deftest agent-repl-test-user-copy-translates-a-missing-transcript-refusal ()
  "A resume target with no transcript names the transcript, not the daemon."
  ;; Act
  (let ((copy (agent-repl--user-copy-for-error
               "createSession: resume target s_1 has no transcript" "resume")))
    ;; Assert
    (should (equal copy
                   (concat "resume refused — the conversation being resumed has no "
                           "transcript on disk")))))

(ert-deftest agent-repl-test-user-copy-falls-back-to-the-generic-sentence ()
  "An unrecognized error names the verb and points at the log."
  ;; Act
  (let ((copy (agent-repl--user-copy-for-error
               "shimclient: dial unix /tmp/x.sock: connect: connection refused"
               "hibernate")))
    ;; Assert
    (should (equal copy "hibernate failed — see the workspace log for detail"))))

(ert-deftest agent-repl-test-user-copy-generic-sentence-omits-the-raw-chain ()
  "The generic sentence never quotes the error it could not classify."
  ;; Arrange
  (let ((raw "shimclient: dial unix /tmp/x.sock: connect: connection refused"))
    ;; Act
    (let ((copy (agent-repl--user-copy-for-error raw "hibernate")))
      ;; Assert
      (should-not (string-match-p "shimclient" copy)))))

(ert-deftest agent-repl-test-user-copy-degrades-a-missing-verb ()
  "A nil VERB yields a whole sentence rather than one with a hole in it."
  ;; Act
  (let ((copy (agent-repl--user-copy-for-error "unclassifiable" nil)))
    ;; Assert
    (should (equal copy "the command failed — see the workspace log for detail"))))

(ert-deftest agent-repl-test-user-copy-classifies-nil-error-text-generically ()
  "A refusal with no error text at all still produces the generic sentence."
  ;; Act
  (let ((copy (agent-repl--user-copy-for-error nil "prompt")))
    ;; Assert
    (should (equal copy "prompt failed — see the workspace log for detail"))))

(ert-deftest agent-repl-test-user-copy-matches-case-insensitively ()
  "Pattern matching does not depend on the daemon's capitalization."
  ;; Act
  (let ((copy (agent-repl--user-copy-for-error "Workspace has NO LIVE SESSION" "prompt")))
    ;; Assert
    (should (equal copy
                   "prompt refused — this workspace is not live; start or wake it first"))))

(ert-deftest agent-repl-test-user-message-for-error-echoes-copy-and-files-the-chain ()
  "The composition echoes only the sentence and files the raw chain beside it."
  ;; Arrange
  (let ((echoed nil)
        (logged nil))
    (cl-letf (((symbol-function 'agent-repl--emit-message)
               (lambda (text &optional _echo) (setq echoed text)))
              ((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
      ;; Act
      (agent-repl--user-message-for-error
       "ws" "prompt" "ssm: invariant failed: state=RENDER_STATE_MERGE_QUEUED turn_active=true")
      ;; Assert
      (should-not (string-match-p "RENDER_STATE" echoed))
      (should (seq-find (lambda (line) (string-match-p "RENDER_STATE_MERGE_QUEUED" line))
                        logged)))))

;;;; ---- Tests: one-shot settle latch ----
;;
;; The latch is what makes an asynchronous operation's settle atomic with
;; respect to `C-g'.  Each test below pins one half of that: exactly one
;; claimant, no timer outliving the operation, and the whole claim running
;; with quit held off.

(ert-deftest agent-repl-test-latch-claim-succeeds-once ()
  "Exactly one caller may claim a latch."
  ;; Arrange
  (let ((latch (agent-repl--make-latch)))
    ;; Act / Assert
    (should (agent-repl--latch-claim latch))
    (should-not (agent-repl--latch-claim latch))))

(ert-deftest agent-repl-test-latch-claim-reports-settled ()
  "A claimed latch reports itself settled."
  (let ((latch (agent-repl--make-latch)))
    (should-not (agent-repl--latch-settled-p latch))
    (agent-repl--latch-claim latch)
    (should (agent-repl--latch-settled-p latch))))

(ert-deftest agent-repl-test-latch-claim-cancels-held-timers ()
  "Claiming a latch cancels every timer it holds.
A deadline surviving its own operation is exactly the stranded timer the
heartbeat assertion reports."
  ;; Arrange
  (let* ((latch (agent-repl--make-latch))
         (timer (run-with-timer 3600 nil #'ignore)))
    (agent-repl--latch-set-timer latch 'deadline timer)
    (should (memq timer timer-list))
    ;; Act
    (agent-repl--latch-claim latch)
    ;; Assert
    (should-not (memq timer timer-list))))

(ert-deftest agent-repl-test-latch-claim-runs-cleanup-once ()
  "The winning claim runs CLEANUP; later claims run nothing."
  ;; Arrange
  (let* ((runs 0)
         (latch (agent-repl--make-latch (lambda () (setq runs (1+ runs))))))
    ;; Act
    (agent-repl--latch-claim latch)
    (agent-repl--latch-claim latch)
    ;; Assert
    (should (equal runs 1))))

(ert-deftest agent-repl-test-latch-claim-inhibits-quit-across-cleanup ()
  "CLEANUP runs with quit inhibited, so a `C-g' cannot split the settle."
  ;; Arrange
  (let (observed)
    (let ((latch (agent-repl--make-latch (lambda () (setq observed inhibit-quit)))))
      ;; Act
      (agent-repl--latch-claim latch))
    ;; Assert
    (should observed)))

(ert-deftest agent-repl-test-latch-set-timer-replaces-same-key ()
  "Re-arming a key cancels its predecessor rather than accumulating beside it.
The create's 20Hz view poll re-arms every tick, so a list would grow one
entry per tick for the whole bring-up."
  ;; Arrange
  (let* ((latch (agent-repl--make-latch))
         (first (run-with-timer 3600 nil #'ignore))
         (second (run-with-timer 3600 nil #'ignore)))
    (agent-repl--latch-set-timer latch 'poll first)
    ;; Act
    (agent-repl--latch-set-timer latch 'poll second)
    ;; Assert
    (should-not (memq first timer-list))
    (should (memq second timer-list))
    (should (equal (length (agent-repl--latch-timers latch)) 1))
    ;; Cleanup
    (agent-repl--latch-claim latch)))

(ert-deftest agent-repl-test-latch-set-timer-drops-a-late-timer ()
  "A timer armed after the settle is cancelled instead of recorded."
  ;; Arrange
  (let* ((latch (agent-repl--make-latch))
         (late (run-with-timer 3600 nil #'ignore)))
    (agent-repl--latch-claim latch)
    ;; Act
    (should-not (agent-repl--latch-set-timer latch 'poll late))
    ;; Assert
    (should-not (memq late timer-list))
    (should-not (agent-repl--latch-timers latch))))

(ert-deftest agent-repl-test-latch-set-timer-rejects-a-non-latch ()
  "A non-latch first argument is a programming error, not a coped-with case."
  (should-error (agent-repl--latch-set-timer 'not-a-latch 'poll nil)))

(ert-deftest agent-repl-test-latch-claim-rejects-a-non-latch ()
  "Claiming something that is not a latch signals."
  (should-error (agent-repl--latch-claim 'not-a-latch)))
;;;; ---- Tests: deferred quit around asynchronous critical sections ----

(ert-deftest agent-repl-test-deferred-quit-inhibits-quitting-inside-the-body ()
  "The guarded body observes `inhibit-quit' bound, so a C-g cannot land in it."
  ;; Arrange
  (let ((observed 'unset))
    ;; Act
    (agent-repl--with-deferred-quit "test"
      (setq observed inhibit-quit))
    ;; Assert
    (should (eq observed t))))

(ert-deftest agent-repl-test-deferred-quit-returns-the-body-value ()
  "The guard is transparent to the body's value."
  ;; Act
  (let ((value (agent-repl--with-deferred-quit "test" 41 (1+ 41))))
    ;; Assert
    (should (equal value 42))))

(ert-deftest agent-repl-test-deferred-quit-re-arms-a-quit-for-the-command-loop ()
  "A C-g raised inside the body survives in `quit-flag' after the guard exits."
  ;; Act / Assert -- the helper makes the observation the command loop would.
  (should (agent-repl-test--quit-deferred-p
            (agent-repl--with-deferred-quit "test"
              ;; What Emacs itself does when C-g arrives under `inhibit-quit'.
              (setq quit-flag t)))))

(ert-deftest agent-repl-test-deferred-quit-records-the-deferral ()
  "A deferred quit is explainable from the canonical log alone."
  ;; Arrange
  (let ((logged nil))
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
      ;; Act
      (agent-repl-test--quit-deferred-p
        (agent-repl--with-deferred-quit "uds-filter"
          (setq quit-flag t))))
    ;; Assert
    (should (seq-find (lambda (line)
                        (and (string-match-p "deferred-quit" line)
                             (string-match-p "uds-filter" line)))
                      logged))))

(ert-deftest agent-repl-test-deferred-quit-stays-silent-without-a-quit ()
  "No quit means no deferral record — the guard is quiet on the ordinary path."
  ;; Arrange
  (let ((quit-flag nil)
        (logged nil))
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
      ;; Act
      (agent-repl--with-deferred-quit "uds-filter" t))
    ;; Assert
    (should-not (seq-find (lambda (line) (string-match-p "deferred-quit" line))
                          logged))))

(ert-deftest agent-repl-test-deferred-quit-propagates-an-error-from-the-body ()
  "The guard defers quits only; a signalled error still reaches the caller."
  ;; Act / Assert
  (should-error (agent-repl--with-deferred-quit "test"
                  (error "boom"))
                :type 'error))

;;;; ---- Tests: a workspace's directory is canonicalized to its registry key ----

(ert-deftest agent-repl-test-log-workspace-dir-routes-to-its-registered-name ()
  "A record naming a live workspace by its DIRECTORY routes to that workspace.
Regression: a caller holding the worktree path rather than the workspace
name made a live, registered workspace look unroutable."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((project (make-temp-file "agent-repl-dirkey-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "dirkey-ws" :project-dir project)
            ;; Act / Assert
            (should (equal "dirkey-ws" (agent-repl--log-sink-workspace project))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-workspace-dir-does-not-warn-as-unroutable ()
  "Routing by directory must not emit the unroutable-workspace warning."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((project (make-temp-file "agent-repl-dirkey-quiet-" t))
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        (unwind-protect
            (progn
              (agent-repl--ws-put "dirkey-quiet-ws" :project-dir project)
              ;; Act
              (cl-letf (((symbol-function 'message) #'ignore))
                (agent-repl--log project "probe by directory"))
              ;; Assert
              (with-temp-buffer
                (insert-file-contents path)
                (should-not (string-match-p "unroutable log workspace"
                                            (buffer-string)))))
          (delete-directory project t))))))

(ert-deftest agent-repl-test-log-workspace-dir-with-trailing-slash-resolves ()
  "The registry key is reached through the shared path canonicalizer.
A trailing slash is the same directory, so it must resolve to the same
workspace name rather than to a second, unroutable spelling."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((project (make-temp-file "agent-repl-dirkey-slash-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "dirkey-slash-ws" :project-dir project)
            ;; Act / Assert
            (should (equal "dirkey-slash-ws"
                           (agent-repl--log-sink-workspace
                            (file-name-as-directory project)))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-unknown-directory-still-warns-as-unroutable ()
  "Canonicalization must not silence a genuinely unknown name."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        ;; Act
        (cl-letf (((symbol-function 'message) #'ignore))
          (agent-repl--log "/no/such/worktree/anywhere" "probe"))
        ;; Assert
        (with-temp-buffer
          (insert-file-contents path)
          (should (string-match-p "unroutable log workspace" (buffer-string))))))))

(ert-deftest agent-repl-test-tombstoned-workspace-dir-is-not-adopted ()
  "A dead workspace's preserved directory must not claim the record.
`agent-repl--ws-log-routable-p' is the gate, so an entry whose worktree is
gone cannot silently swallow a directory-spelled record."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((project (make-temp-file "agent-repl-dirkey-tomb-" t)))
      (agent-repl--ws-put "dirkey-tomb-ws" :project-dir project)
      (delete-directory project t)
      ;; Act / Assert
      (should-not (equal "dirkey-tomb-ws"
                         (agent-repl--log-sink-workspace project))))))
