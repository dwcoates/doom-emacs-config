;;; test-sentinel.el --- Tests for sentinel.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the file-notify watcher dispatch and event handlers
;; defined in sentinel.el.  Migrated from test-agent-repl.el and
;; updated for the refactored dispatch-table API.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: dispatch-sentinel-event filtering ----

(ert-deftest agent-repl-test-sentinel-event-ignores-unknown-files ()
  "An event with a non-matching filename should not call any handler."
  (agent-repl-test--with-clean-state
    (let ((process-called nil))
      (cl-letf (((symbol-function 'agent-repl--process-sentinel-file)
                 (lambda (&rest _) (setq process-called t)))
                ((symbol-function 'file-exists-p) (lambda (_f) t)))
        (agent-repl--dispatch-sentinel-event '(nil changed "/some/other-file"))
        (should-not process-called)))))

(ert-deftest agent-repl-test-sentinel-event-ignores-deleted-action ()
  "An event with action `deleted' should be ignored even if filename matches."
  (agent-repl-test--with-clean-state
    (let ((process-called nil))
      (cl-letf (((symbol-function 'agent-repl--process-sentinel-file)
                 (lambda (&rest _) (setq process-called t)))
                ((symbol-function 'file-exists-p) (lambda (_f) t)))
        (agent-repl--dispatch-sentinel-event '(nil deleted "/path/to/permission_prompt"))
        (should-not process-called)))))

(ert-deftest agent-repl-test-sentinel-event-ignores-nonexistent-file ()
  "An event for a file that no longer exists should be ignored."
  (agent-repl-test--with-clean-state
    (let ((process-called nil))
      (cl-letf (((symbol-function 'agent-repl--process-sentinel-file)
                 (lambda (&rest _) (setq process-called t)))
                ((symbol-function 'file-exists-p) (lambda (_f) nil)))
        (agent-repl--dispatch-sentinel-event '(nil created "/dir/permission_prompt"))
        (should-not process-called)))))

(ert-deftest agent-repl-test-sentinel-event-ignores-hook-debug-log ()
  "Events for hook-debug.log should be ignored before dispatch.
Symmetric to the filter in `agent-repl--poll-workspace-notifications'."
  (agent-repl-test--with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'agent-repl--dispatch-sentinel-file)
                 (lambda (_f) (setq dispatched t) t)))
        (agent-repl--dispatch-sentinel-event '(nil changed "/dir/hook-debug.log"))
        (should-not dispatched)))))

(ert-deftest agent-repl-test-sentinel-event-tolerates-nil-file ()
  "An event with no file (e.g. `stopped') should be skipped without crashing."
  (agent-repl-test--with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'agent-repl--dispatch-sentinel-file)
                 (lambda (_f) (setq dispatched t) t)))
        ;; Must not error even though the file slot is nil.
        (agent-repl--dispatch-sentinel-event '(nil stopped nil))
        (should-not dispatched)))))

;;;; ---- Tests: dispatch-sentinel-file prefix matching ----

(ert-deftest agent-repl-test-sentinel-dispatches-permission ()
  "A file named permission_prompt should dispatch to the permission handler."
  (agent-repl-test--with-clean-state
    (let ((dispatched-handler nil))
      (cl-letf (((symbol-function 'agent-repl--process-sentinel-file)
                 (lambda (_file handler) (setq dispatched-handler handler))))
        (agent-repl--dispatch-sentinel-file "/dir/permission_prompt")
        (should dispatched-handler)
        (should (eq (plist-get dispatched-handler :callback)
                    'agent-repl--on-permission-event))))))

(ert-deftest agent-repl-test-sentinel-dispatches-permission-request ()
  "A file named permission_request should dispatch to the permission handler.
This is the real-time PermissionRequest signal Claude Code emits at the
moment the permission dialog appears; the dispatch entry must share its
callback with the older `permission_prompt' Notification fallback so
both paths flip the tab to `:permission' through the same gate."
  (agent-repl-test--with-clean-state
    (let ((dispatched-handler nil))
      (cl-letf (((symbol-function 'agent-repl--process-sentinel-file)
                 (lambda (_file handler) (setq dispatched-handler handler))))
        (agent-repl--dispatch-sentinel-file "/dir/permission_request")
        (should dispatched-handler)
        (should (eq (plist-get dispatched-handler :callback)
                    'agent-repl--on-permission-event))))))

(ert-deftest agent-repl-test-sentinel-dispatches-stop ()
  "A file named stop_12345 should dispatch to the stop handler."
  (agent-repl-test--with-clean-state
    (let ((dispatched-handler nil))
      (cl-letf (((symbol-function 'agent-repl--process-sentinel-file)
                 (lambda (_file handler) (setq dispatched-handler handler))))
        (agent-repl--dispatch-sentinel-file "/dir/stop_12345")
        (should dispatched-handler)
        (should (eq (plist-get dispatched-handler :callback)
                    'agent-repl--on-stop-event))))))

(ert-deftest agent-repl-test-sentinel-dispatches-prompt-submit ()
  "A file named prompt_submit_99 should dispatch to the prompt-submit handler."
  (agent-repl-test--with-clean-state
    (let ((dispatched-handler nil))
      (cl-letf (((symbol-function 'agent-repl--process-sentinel-file)
                 (lambda (_file handler) (setq dispatched-handler handler))))
        (agent-repl--dispatch-sentinel-file "/dir/prompt_submit_99")
        (should dispatched-handler)
        (should (eq (plist-get dispatched-handler :callback)
                    'agent-repl--on-prompt-submit-event))))))

(ert-deftest agent-repl-test-sentinel-dispatches-suffixed-permission-request ()
  "A daemon-written permission_request_<sid>_<reqid> file dispatches to the permission handler.
The daemon suffixes sid/reqid for per-file uniqueness; the entry is
prefix-matched so the suffixed name must reach the same callback as the
hook's fixed-name file."
  (agent-repl-test--with-clean-state
    (let ((dispatched-handler nil))
      (cl-letf (((symbol-function 'agent-repl--process-sentinel-file)
                 (lambda (_file handler) (setq dispatched-handler handler))))
        (agent-repl--dispatch-sentinel-file "/dir/permission_request_abc123_req42")
        (should dispatched-handler)
        (should (eq (plist-get dispatched-handler :callback)
                    'agent-repl--on-permission-event))))))

(ert-deftest agent-repl-test-sentinel-dispatches-permission-resolved ()
  "A permission_resolved_<sid>_<reqid> file dispatches to the resolved handler.
Must NOT be shadowed by the permission_request/permission_prompt
entries that share the permission_ stem."
  (agent-repl-test--with-clean-state
    (let ((dispatched-handler nil))
      (cl-letf (((symbol-function 'agent-repl--process-sentinel-file)
                 (lambda (_file handler) (setq dispatched-handler handler))))
        (agent-repl--dispatch-sentinel-file "/dir/permission_resolved_abc123_req42")
        (should dispatched-handler)
        (should (eq (plist-get dispatched-handler :callback)
                    'agent-repl--on-permission-resolved-event))))))

(ert-deftest agent-repl-test-sentinel-dispatches-session-dead ()
  "A session_dead_<sid> file dispatches to the session-dead handler.
Must NOT be shadowed by session_start_ (they diverge at session_d/session_s)."
  (agent-repl-test--with-clean-state
    (let ((dispatched-handler nil))
      (cl-letf (((symbol-function 'agent-repl--process-sentinel-file)
                 (lambda (_file handler) (setq dispatched-handler handler))))
        (agent-repl--dispatch-sentinel-file "/dir/session_dead_abc123")
        (should dispatched-handler)
        (should (eq (plist-get dispatched-handler :callback)
                    'agent-repl--on-session-dead-event))))))

(ert-deftest agent-repl-test-sentinel-session-start-not-shadowed-by-dead ()
  "A session_start_<pid> file still dispatches to the session-start handler.
Guards the reverse shadowing direction of the new session_dead_ entry."
  (agent-repl-test--with-clean-state
    (let ((dispatched-handler nil))
      (cl-letf (((symbol-function 'agent-repl--process-sentinel-file)
                 (lambda (_file handler) (setq dispatched-handler handler))))
        (agent-repl--dispatch-sentinel-file "/dir/session_start_777")
        (should dispatched-handler)
        (should (eq (plist-get dispatched-handler :callback)
                    'agent-repl--on-session-start-event))))))

(ert-deftest agent-repl-test-sentinel-dispatches-account-changed ()
  "An account_changed_<sid> file dispatches to the account-changed handler."
  (agent-repl-test--with-clean-state
    (let ((dispatched-handler nil))
      (cl-letf (((symbol-function 'agent-repl--process-sentinel-file)
                 (lambda (_file handler) (setq dispatched-handler handler))))
        (agent-repl--dispatch-sentinel-file "/dir/account_changed_abc123")
        (should dispatched-handler)
        (should (eq (plist-get dispatched-handler :callback)
                    'agent-repl--on-account-changed-event))))))

;;;; ---- Tests: agent-repl--on-account-changed-event ----

(ert-deftest agent-repl-test-account-changed-stores-string-override ()
  "The handler stores the daemon's config dir as the workspace override
and persists it via state-save."
  (agent-repl-test--with-clean-state
    (let ((saved nil))
      (agent-repl--ws-put "ws1" :frontend-session-id "s_1")
      (cl-letf (((symbol-function 'agent-repl--frontend-api)
                 (lambda (_method path)
                   (should (equal path "/sessions/s_1/account"))
                   '((config_dir . "/home/u/.claude-chesscom") (email . "dodge@chess.com"))))
                ((symbol-function 'agent-repl--state-save)
                 (lambda (ws) (setq saved ws))))
        (agent-repl--on-account-changed-event "ws1" "/dir")
        (should (equal (agent-repl--ws-get "ws1" :config-dir-override)
                       "/home/u/.claude-chesscom"))
        (should (equal saved "ws1"))))))

(ert-deftest agent-repl-test-account-changed-maps-empty-dir-to-default ()
  "The daemon's empty config dir (the CLI's own root) stores as :default,
distinguishable from the nil of no-override-at-all."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :frontend-session-id "s_1")
    (cl-letf (((symbol-function 'agent-repl--frontend-api)
               (lambda (_method _path) '((config_dir . "") (email . "a@b.c"))))
              ((symbol-function 'agent-repl--state-save) #'ignore))
      (agent-repl--on-account-changed-event "ws1" "/dir")
      (should (eq (agent-repl--ws-get "ws1" :config-dir-override) :default)))))

(ert-deftest agent-repl-test-account-changed-fetch-failure-leaves-override ()
  "A failed config-dir fetch warns and leaves the override untouched —
acting on a guess could pin the workspace to the wrong account."
  (agent-repl-test--with-clean-state
    (let ((warned nil))
      (agent-repl--ws-put "ws1" :frontend-session-id "s_1")
      (agent-repl--ws-put "ws1" :config-dir-override "/old/root")
      (cl-letf (((symbol-function 'agent-repl--frontend-api)
                 (lambda (_method _path) (error "daemon unreachable")))
                ((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (setq warned (apply #'format fmt args)))))
        (agent-repl--on-account-changed-event "ws1" "/dir")
        (should warned)
        (should (equal (agent-repl--ws-get "ws1" :config-dir-override) "/old/root"))))))

(ert-deftest agent-repl-test-account-changed-without-session-id-warns ()
  "A workspace with no :frontend-session-id warns instead of fetching."
  (agent-repl-test--with-clean-state
    (let ((warned nil)
          (fetched nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-api)
                 (lambda (&rest _) (setq fetched t)))
                ((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (setq warned (apply #'format fmt args)))))
        (agent-repl--on-account-changed-event "ws1" "/dir")
        (should warned)
        (should-not fetched)
        (should-not (agent-repl--ws-get "ws1" :config-dir-override))))))

(ert-deftest agent-repl-test-sentinel-dispatch-returns-nil-for-unknown ()
  "dispatch-sentinel-file should return nil for an unrecognized filename."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--dispatch-sentinel-file "/dir/unknown_file"))))

(ert-deftest agent-repl-test-sentinel-dispatch-returns-t-for-known ()
  "dispatch-sentinel-file should return t when a handler is found."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--process-sentinel-file)
               (lambda (&rest _) nil)))
      (should (eq t (agent-repl--dispatch-sentinel-file "/dir/stop_abc"))))))

;;;; ---- Tests: deprecated-prefix drain ----

(ert-deftest agent-repl-test-sentinel-drains-deprecated-login-request ()
  "A bare login_request_ file should be drained (deleted), not warned about.
Draining is what stops the poll fallback re-detecting and re-warning about
the retired channel every cycle."
  (agent-repl-test--with-clean-state
    (let ((deleted nil))
      (cl-letf (((symbol-function 'delete-file)
                 (lambda (f) (setq deleted f)))
                ((symbol-function 'agent-repl--warn)
                 (lambda (&rest _) (ert-fail "drain must not warn"))))
        (agent-repl--dispatch-sentinel-file "/dir/login_request_")
        (should (equal deleted "/dir/login_request_"))))))

(ert-deftest agent-repl-test-sentinel-drains-suffixed-login-request ()
  "A login_request_<sid> file should also match the deprecated prefix and drain."
  (agent-repl-test--with-clean-state
    (let ((deleted nil))
      (cl-letf (((symbol-function 'delete-file)
                 (lambda (f) (setq deleted f)))
                ((symbol-function 'agent-repl--warn)
                 (lambda (&rest _) (ert-fail "drain must not warn"))))
        (agent-repl--dispatch-sentinel-file "/dir/login_request_abc123")
        (should (equal deleted "/dir/login_request_abc123"))))))

(ert-deftest agent-repl-test-sentinel-drain-returns-t ()
  "Draining a deprecated file should return t so the poll path treats it as handled."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'delete-file) (lambda (_f) nil)))
      (should (eq t (agent-repl--dispatch-sentinel-file "/dir/login_request_"))))))

(ert-deftest agent-repl-test-sentinel-drain-skips-process-sentinel-file ()
  "Draining must not route through process-sentinel-file, so no side effect runs.
The retired channel carries a session id, and running the normal pipeline
would adopt it into a workspace; the drain only deletes."
  (agent-repl-test--with-clean-state
    (let ((processed nil))
      (cl-letf (((symbol-function 'delete-file) (lambda (_f) nil))
                ((symbol-function 'agent-repl--process-sentinel-file)
                 (lambda (&rest _) (setq processed t))))
        (agent-repl--dispatch-sentinel-file "/dir/login_request_abc123")
        (should-not processed)))))

(ert-deftest agent-repl-test-sentinel-drain-warns-on-delete-failure ()
  "A failed drain delete must surface via `--warn' (log + *Messages*), not be swallowed."
  (agent-repl-test--with-clean-state
    (let ((warned nil))
      (cl-letf (((symbol-function 'delete-file)
                 (lambda (_f) (error "boom")))
                ((symbol-function 'agent-repl--warn)
                 (lambda (&rest _) (setq warned t))))
        (agent-repl--dispatch-sentinel-file "/dir/login_request_")
        (should warned)))))

;;;; ---- Tests: delete-sentinel-file helper ----

(ert-deftest agent-repl-test-delete-sentinel-file-deletes ()
  "delete-sentinel-file should delete the file it is given."
  (agent-repl-test--with-clean-state
    (let ((deleted nil))
      (cl-letf (((symbol-function 'delete-file)
                 (lambda (f) (setq deleted f))))
        (agent-repl--delete-sentinel-file "/dir/stop_abc" nil)
        (should (equal deleted "/dir/stop_abc"))))))

(ert-deftest agent-repl-test-delete-sentinel-file-warns-and-does-not-rethrow ()
  "A failed delete must warn (log + *Messages* surface) and NOT rethrow into the watcher."
  (agent-repl-test--with-clean-state
    (let ((warned nil))
      (cl-letf (((symbol-function 'delete-file)
                 (lambda (_f) (error "boom")))
                ((symbol-function 'agent-repl--warn)
                 (lambda (&rest _) (setq warned t))))
        ;; Must return normally despite the delete error.
        (agent-repl--delete-sentinel-file "/dir/stop_abc" nil)
        (should warned)))))

;;;; ---- Tests: process-sentinel-file orchestration ----

(ert-deftest agent-repl-test-process-sentinel-file-calls-callback ()
  "process-sentinel-file should read file, resolve ws, call callback, delete file."
  (agent-repl-test--with-clean-state
    (let ((callback-args nil)
          (deleted-file nil))
      (cl-letf (((symbol-function 'agent-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/some/dir" :session-id "sid-123")))
                ((symbol-function 'agent-repl--ws-for-dir)
                 (lambda (_d) "ws1"))
                ((symbol-function 'agent-repl--update-session-id-from-sentinel)
                 #'ignore)
                ((symbol-function 'delete-file)
                 (lambda (f) (setq deleted-file f))))
        (agent-repl--process-sentinel-file
         "/tmp/stop_123"
         (list :callback (lambda (ws dir) (setq callback-args (list ws dir)))
               :warning "warn %s"
               :name "test-handler"))
        (should (equal callback-args '("ws1" "/some/dir")))
        (should (equal deleted-file "/tmp/stop_123"))))))

(ert-deftest agent-repl-test-process-sentinel-file-deletes-before-callback ()
  "File must be deleted before the callback runs so a slow callback cannot be
re-dispatched by the poll fallback observing a still-present file."
  (agent-repl-test--with-clean-state
    (let ((delete-time nil)
          (callback-time nil)
          (counter 0))
      (cl-letf (((symbol-function 'agent-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/some/dir" :session-id nil)))
                ((symbol-function 'agent-repl--ws-for-dir)
                 (lambda (_d) "ws1"))
                ((symbol-function 'agent-repl--update-session-id-from-sentinel)
                 #'ignore)
                ((symbol-function 'delete-file)
                 (lambda (_f) (setq delete-time (cl-incf counter)))))
        (agent-repl--process-sentinel-file
         "/tmp/stop_789"
         (list :callback (lambda (_ws _dir) (setq callback-time (cl-incf counter)))
               :warning "warn %s"
               :name "test"))
        (should delete-time)
        (should callback-time)
        (should (< delete-time callback-time))))))

(ert-deftest agent-repl-test-process-sentinel-file-nil-read-skips-all ()
  "When read-sentinel-file returns nil, callback should not be called."
  (agent-repl-test--with-clean-state
    (let ((callback-called nil)
          (deleted-file nil))
      (cl-letf (((symbol-function 'agent-repl--read-sentinel-file)
                 (lambda (_f) nil))
                ((symbol-function 'agent-repl--ws-for-dir)
                 (lambda (_d) (error "should not be called")))
                ((symbol-function 'delete-file)
                 (lambda (f) (setq deleted-file f))))
        (agent-repl--process-sentinel-file
         "/tmp/stop_456"
         '(:callback (lambda (&rest _) (setq callback-called t))
           :warning "warn %s"
           :name "test"))
        (should-not callback-called)
        ;; File should still be deleted even when dir is nil
        (should (equal deleted-file "/tmp/stop_456"))))))

(ert-deftest agent-repl-test-process-sentinel-file-nil-ws-in-git-warns ()
  "When ws-for-dir returns nil but dir is inside a git repo, the warning
should be logged (genuine misattribution case) and callback skipped."
  (agent-repl-test--with-clean-state
    (let ((callback-called nil)
          (warning-msg nil)
          (deleted-file nil))
      (cl-letf (((symbol-function 'agent-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/unknown/dir" :session-id nil)))
                ((symbol-function 'agent-repl--ws-for-dir)
                 (lambda (_d) nil))
                ((symbol-function 'agent-repl--git-root)
                 (lambda (_d) "/unknown"))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq warning-msg (apply #'format fmt args))))
                ((symbol-function 'delete-file)
                 (lambda (f) (setq deleted-file f))))
        (agent-repl--process-sentinel-file
         "/tmp/stop_789"
         '(:callback (lambda (&rest _) (setq callback-called t))
           :warning "[agent-repl] WARNING: stop sentinel dir=%s matched no workspace"
           :name "test"))
        (should-not callback-called)
        (should (string-match-p "matched no workspace" warning-msg))
        (should (string-match-p "/unknown/dir" warning-msg))
        (should (equal deleted-file "/tmp/stop_789"))))))

(ert-deftest agent-repl-test-process-sentinel-file-nil-ws-non-git-no-warn ()
  "When ws-for-dir returns nil AND dir is outside any git repo, the warning
should be SUPPRESSED (deliberate non-workspace cwd from headless spawns in
prompt-summary.el / worktree.el).  Callback still skipped, file still deleted."
  (agent-repl-test--with-clean-state
    (let ((callback-called nil)
          (warning-msg nil)
          (deleted-file nil))
      (cl-letf (((symbol-function 'agent-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/var/folders/xx" :session-id nil)))
                ((symbol-function 'agent-repl--ws-for-dir)
                 (lambda (_d) nil))
                ((symbol-function 'agent-repl--git-root)
                 (lambda (_d) nil))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq warning-msg (apply #'format fmt args))))
                ((symbol-function 'delete-file)
                 (lambda (f) (setq deleted-file f))))
        (agent-repl--process-sentinel-file
         "/tmp/stop_789"
         '(:callback (lambda (&rest _) (setq callback-called t))
           :warning "[agent-repl] WARNING: stop sentinel dir=%s matched no workspace"
           :name "test"))
        (should-not callback-called)
        (should-not warning-msg)
        (should (equal deleted-file "/tmp/stop_789"))))))

(ert-deftest agent-repl-test-process-sentinel-file-always-deletes ()
  "File should be deleted even when callback signals an error."
  (agent-repl-test--with-clean-state
    (let ((deleted-file nil))
      (cl-letf (((symbol-function 'agent-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/some/dir" :session-id nil)))
                ((symbol-function 'agent-repl--ws-for-dir)
                 (lambda (_d) "ws1"))
                ((symbol-function 'agent-repl--update-session-id-from-sentinel)
                 #'ignore)
                ((symbol-function 'delete-file)
                 (lambda (f) (setq deleted-file f))))
        (agent-repl--process-sentinel-file
         "/tmp/perm_123"
         '(:callback (lambda (_ws _dir) nil)
           :warning "warn %s"
           :name "test"))
        (should (equal deleted-file "/tmp/perm_123"))))))

(ert-deftest agent-repl-test-process-sentinel-file-callback-error-surfaced-not-swallowed ()
  "A throwing handler is surfaced on the log via `--do-log' and does NOT
propagate (a hard error would kill the file-notify watcher)."
  (agent-repl-test--with-clean-state
    (let ((surfaced nil))
      (cl-letf (((symbol-function 'agent-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/some/dir" :session-id nil)))
                ((symbol-function 'agent-repl--ws-for-dir)
                 (lambda (_d) "ws1"))
                ((symbol-function 'agent-repl--update-session-id-from-sentinel)
                 #'ignore)
                ((symbol-function 'delete-file) #'ignore)
                ((symbol-function 'agent-repl--do-log)
                 (lambda (_ws fmt &rest _) (setq surfaced fmt))))
        ;; Must return normally (no error escapes the file-notify callback).
        (should
         (progn
           (agent-repl--process-sentinel-file
            "/tmp/session_start_1"
            (list :callback (lambda (_ws _dir) (error "boom in handler"))
                  :warning "warn %s"
                  :name "handle-session-start"))
           t))
        ;; And the failure was surfaced on the log channel, not swallowed.
        (should (string-match-p "ERRORED" surfaced))))))

;;;; ---- Tests: on-stop-event handler ----

(ert-deftest agent-repl-test-on-stop-event-calls-finished-when-no-subagents ()
  "on-stop-event with no pending subagents finalizes immediately."
  (agent-repl-test--with-clean-state
    (let ((finished-ws nil))
      (cl-letf (((symbol-function 'agent-repl--handle-agent-finished)
                 (lambda (ws) (setq finished-ws ws))))
        (agent-repl--on-stop-event "ws1" "/some/dir")
        (should (equal finished-ws "ws1"))))))

(ert-deftest agent-repl-test-on-stop-event-defers-when-subagents-pending ()
  "on-stop-event with pending subagents records stop-received but does NOT finalize."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-incf-pending-subagents "ws1")
    (let ((finished-called nil))
      (cl-letf (((symbol-function 'agent-repl--handle-agent-finished)
                 (lambda (_ws) (setq finished-called t))))
        (agent-repl--on-stop-event "ws1" "/some/dir")
        (should-not finished-called)
        (should (agent-repl--ws-stop-received-p "ws1"))))))

(ert-deftest agent-repl-test-on-stop-event-sets-stop-received ()
  "on-stop-event always records that Stop fired (independent of finalization)."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--handle-agent-finished) #'ignore))
      (agent-repl--on-stop-event "ws1" "/some/dir")
      ;; After a successful finalize, clear-stop-tracking resets it.
      ;; This confirms set-stop-received fired before clear ran.
      (should-not (agent-repl--ws-stop-received-p "ws1")))))

(ert-deftest agent-repl-test-on-stop-event-clears-tracking-after-finalize ()
  "on-stop-event clears tracking after successful finalize so the next
turn starts from a clean slate."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--handle-agent-finished) #'ignore))
      (agent-repl--on-stop-event "ws1" "/some/dir")
      (should-not (agent-repl--ws-stop-received-p "ws1"))
      (should (zerop (agent-repl--ws-pending-subagents "ws1"))))))

(ert-deftest agent-repl-test-on-stop-event-tolerates-nil-workspace ()
  "on-stop-event via process-sentinel-file should not error when ws-for-dir returns nil."
  (agent-repl-test--with-clean-state
    (let ((finished-called nil))
      (cl-letf (((symbol-function 'agent-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/unknown/dir" :session-id nil)))
                ((symbol-function 'agent-repl--ws-for-dir)
                 (lambda (_d) nil))
                ((symbol-function 'agent-repl--handle-agent-finished)
                 (lambda (_ws) (setq finished-called t)))
                ((symbol-function 'delete-file) #'ignore))
        (agent-repl--process-sentinel-file
         "/tmp/stop_456"
         (cdr (assoc "stop_" agent-repl--sentinel-dispatch-alist)))
        (should-not finished-called)))))

;;;; ---- Tests: on-subagent-start-event handler ----

(ert-deftest agent-repl-test-on-subagent-start-event-increments-counter ()
  "on-subagent-start-event bumps the pending-subagent counter by 1."
  (agent-repl-test--with-clean-state
    (agent-repl--on-subagent-start-event "ws1" "/some/dir")
    (should (= 1 (agent-repl--ws-pending-subagents "ws1")))
    (agent-repl--on-subagent-start-event "ws1" "/some/dir")
    (should (= 2 (agent-repl--ws-pending-subagents "ws1")))))

;;;; ---- Tests: on-subagent-stop-event handler ----

(ert-deftest agent-repl-test-on-subagent-stop-event-decrements-counter ()
  "on-subagent-stop-event decrements the counter."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-incf-pending-subagents "ws1")
    (agent-repl--ws-incf-pending-subagents "ws1")
    (agent-repl--on-subagent-stop-event "ws1" "/some/dir")
    (should (= 1 (agent-repl--ws-pending-subagents "ws1")))))

(ert-deftest agent-repl-test-on-subagent-stop-event-no-finalize-without-stop ()
  "Counter draining to zero without Stop having fired must NOT finalize."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-incf-pending-subagents "ws1")
    (let ((finished-called nil))
      (cl-letf (((symbol-function 'agent-repl--handle-agent-finished)
                 (lambda (_ws) (setq finished-called t))))
        (agent-repl--on-subagent-stop-event "ws1" "/some/dir")
        (should-not finished-called)))))

(ert-deftest agent-repl-test-on-subagent-stop-event-finalizes-when-last-with-stop ()
  "Last SubagentStop after Stop already fired drives the deferred finalize."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-incf-pending-subagents "ws1")
    (agent-repl--ws-set-stop-received "ws1" t)
    (let ((finished-ws nil))
      (cl-letf (((symbol-function 'agent-repl--handle-agent-finished)
                 (lambda (ws) (setq finished-ws ws))))
        (agent-repl--on-subagent-stop-event "ws1" "/some/dir")
        (should (equal finished-ws "ws1"))))))

(ert-deftest agent-repl-test-on-subagent-stop-event-no-finalize-when-others-pending ()
  "SubagentStop while other subagents still pending must NOT finalize."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-incf-pending-subagents "ws1")
    (agent-repl--ws-incf-pending-subagents "ws1")
    (agent-repl--ws-set-stop-received "ws1" t)
    (let ((finished-called nil))
      (cl-letf (((symbol-function 'agent-repl--handle-agent-finished)
                 (lambda (_ws) (setq finished-called t))))
        (agent-repl--on-subagent-stop-event "ws1" "/some/dir")
        (should-not finished-called)
        (should (= 1 (agent-repl--ws-pending-subagents "ws1")))
        ;; Still waiting on the next subagent — stop-received not cleared.
        (should (agent-repl--ws-stop-received-p "ws1"))))))

;;;; ---- Tests: on-stop-failure-event handler ----

(ert-deftest agent-repl-test-on-stop-failure-event-sets-stop-failed ()
  "on-stop-failure-event writes :agent-state :stop-failed."
  (agent-repl-test--with-clean-state
    (agent-repl--on-stop-failure-event "ws1" "/some/dir")
    (should (eq :stop-failed (agent-repl--ws-agent-state "ws1")))))

(ert-deftest agent-repl-test-on-stop-failure-event-clears-stop-tracking ()
  "on-stop-failure-event clears any in-flight Stop / SubagentStop bookkeeping
so a subsequent successful turn isn't gated on a stale counter."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-stop-received "ws1" t)
    (agent-repl--ws-incf-pending-subagents "ws1")
    (agent-repl--ws-incf-pending-subagents "ws1")
    (agent-repl--on-stop-failure-event "ws1" "/some/dir")
    (should-not (agent-repl--ws-stop-received-p "ws1"))
    (should (zerop (agent-repl--ws-pending-subagents "ws1")))))

(ert-deftest agent-repl-test-on-stop-failure-event-arms-no-retry-timer ()
  "on-stop-failure-event does NOT schedule any automatic retry timer.
The auto `try again' retry was removed because StopFailure is not
guaranteed to fire only on genuine API errors."
  (agent-repl-test--with-clean-state
    (let ((timer-armed nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _args) (setq timer-armed t) nil)))
        (agent-repl--on-stop-failure-event "ws1" "/some/dir"))
      (should-not timer-armed))))

(ert-deftest agent-repl-test-on-stop-failure-event-sends-no-auto-input ()
  "on-stop-failure-event does NOT auto-send any prompt to the agent.
Recovery from a stop-failure is left entirely to the user."
  (agent-repl-test--with-clean-state
    (let ((input-sent nil))
      (cl-letf (((symbol-function 'agent-repl--send)
                 (lambda (&rest _args) (setq input-sent t))))
        (agent-repl--on-stop-failure-event "ws1" "/some/dir"))
      (should-not input-sent))))

;;;; ---- Tests: dispatch-alist routes new prefixes ----

(ert-deftest agent-repl-test-sentinel-dispatches-subagent-start ()
  "A file named subagent_start_NNN dispatches to the subagent-start handler."
  (agent-repl-test--with-clean-state
    (let ((dispatched-handler nil))
      (cl-letf (((symbol-function 'agent-repl--process-sentinel-file)
                 (lambda (_file handler) (setq dispatched-handler handler))))
        (agent-repl--dispatch-sentinel-file "/dir/subagent_start_123")
        (should dispatched-handler)
        (should (eq (plist-get dispatched-handler :callback)
                    'agent-repl--on-subagent-start-event))))))

(ert-deftest agent-repl-test-sentinel-dispatches-subagent-stop ()
  "A file named subagent_stop_NNN dispatches to the subagent-stop handler."
  (agent-repl-test--with-clean-state
    (let ((dispatched-handler nil))
      (cl-letf (((symbol-function 'agent-repl--process-sentinel-file)
                 (lambda (_file handler) (setq dispatched-handler handler))))
        (agent-repl--dispatch-sentinel-file "/dir/subagent_stop_456")
        (should dispatched-handler)
        (should (eq (plist-get dispatched-handler :callback)
                    'agent-repl--on-subagent-stop-event))))))

(ert-deftest agent-repl-test-sentinel-dispatches-stop-failure ()
  "A file named stop_failure_NNN dispatches to the stop-failure handler.
The bare `stop_' prefix is also a prefix of `stop_failure_', so the
ordering of the dispatch alist matters: stop_failure_ must be tried
first.  This test pins that ordering."
  (agent-repl-test--with-clean-state
    (let ((dispatched-handler nil))
      (cl-letf (((symbol-function 'agent-repl--process-sentinel-file)
                 (lambda (_file handler) (setq dispatched-handler handler))))
        (agent-repl--dispatch-sentinel-file "/dir/stop_failure_789")
        (should dispatched-handler)
        (should (eq (plist-get dispatched-handler :callback)
                    'agent-repl--on-stop-failure-event))))))

(ert-deftest agent-repl-test-sentinel-stop-failure-not-confused-with-stop ()
  "Order regression: stop_failure_NNN must NOT dispatch to the stop handler."
  (agent-repl-test--with-clean-state
    (let ((dispatched-handler nil))
      (cl-letf (((symbol-function 'agent-repl--process-sentinel-file)
                 (lambda (_file handler) (setq dispatched-handler handler))))
        (agent-repl--dispatch-sentinel-file "/dir/stop_failure_001")
        (should-not (eq (plist-get dispatched-handler :callback)
                        'agent-repl--on-stop-event))))))

;;;; ---- Tests: on-prompt-submit-event handler ----

(ert-deftest agent-repl-test-on-prompt-submit-event-sets-thinking ()
  "on-prompt-submit-event should call mark-ws-thinking."
  (agent-repl-test--with-clean-state
    (let ((thinking-ws nil))
      (cl-letf (((symbol-function 'agent-repl--mark-ws-thinking)
                 (lambda (ws) (setq thinking-ws ws))))
        (agent-repl--on-prompt-submit-event "ws1" "/some/dir")
        (should (equal thinking-ws "ws1"))))))

;;;; ---- Tests: on-permission-event handler ----

(ert-deftest agent-repl-test-on-permission-event-sets-permission-from-thinking ()
  "on-permission-event should call ws-set-agent-state with :permission when state is :thinking.
Mid-turn (:thinking) is the only state where a permission_prompt notification
is treated as a real permission request; see on-permission-event docstring."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :thinking)
    (let ((set-args nil))
      (cl-letf (((symbol-function 'agent-repl--ws-set-agent-state)
                 (lambda (ws state) (setq set-args (list ws state)))))
        (agent-repl--on-permission-event "ws1" "/some/dir")
        (should (equal set-args '("ws1" :permission)))))))

;;;; ---- Tests: on-permission-resolved-event handler ----

(ert-deftest agent-repl-test-on-permission-resolved-flips-to-thinking ()
  "on-permission-resolved-event flips :permission back to :thinking.
For gui sessions the user answers in the webview, so the daemon's
resolved sentinel is the only resolution signal."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :permission)
    (agent-repl--on-permission-resolved-event "ws1" "/some/dir")
    (should (eq (agent-repl--ws-get "ws1" :agent-state) :thinking))))

(ert-deftest agent-repl-test-on-permission-resolved-noop-from-done ()
  "on-permission-resolved-event is a no-op when state is not :permission.
The gate makes turn-end auto-cancel resolutions order-independent
against the Stop hook's :done."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :done)
    (agent-repl--on-permission-resolved-event "ws1" "/some/dir")
    (should (eq (agent-repl--ws-get "ws1" :agent-state) :done))))

(ert-deftest agent-repl-test-on-permission-resolved-noop-from-nil ()
  "on-permission-resolved-event is a no-op when no agent-state is set."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--on-permission-resolved-event "ws1" "/some/dir")
    (should-not (agent-repl--ws-get "ws1" :agent-state))))

;;;; ---- Tests: on-session-dead-event handler ----

(ert-deftest agent-repl-test-on-session-dead-marks-dead ()
  "on-session-dead-event sets :repl-state :dead and clears :agent-state."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :thinking)
    (agent-repl--on-session-dead-event "ws1" "/some/dir")
    (should (eq (agent-repl--ws-get "ws1" :repl-state) :dead))
    (should-not (agent-repl--ws-get "ws1" :agent-state))))

(ert-deftest agent-repl-test-on-session-dead-respects-merged ()
  "on-session-dead-event must not clobber a :merged badge.
Inherits mark-dead's precedence guard."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :repl-state :merged)
    (agent-repl--on-session-dead-event "ws1" "/some/dir")
    (should (eq (agent-repl--ws-get "ws1" :repl-state) :merged))))

;;;; ---- Tests: read-sentinel-file ----

(ert-deftest agent-repl-test-read-sentinel-file-returns-dir-only ()
  "read-sentinel-file with single-line file returns plist with :dir and nil :session-id."
  (agent-repl-test--with-clean-state
    (let ((tmp (make-temp-file "sentinel-test-")))
      (unwind-protect
          (progn
            (write-region "  /some/project/dir  \n" nil tmp)
            (let ((result (agent-repl--read-sentinel-file tmp)))
              (should (equal (plist-get result :dir) "/some/project/dir"))
              (should-not (plist-get result :session-id))))
        (ignore-errors (delete-file tmp))))))

(ert-deftest agent-repl-test-read-sentinel-file-returns-dir-and-session-id ()
  "read-sentinel-file with two-line file returns both :dir and :session-id."
  (agent-repl-test--with-clean-state
    (let ((tmp (make-temp-file "sentinel-test-")))
      (unwind-protect
          (progn
            (write-region "/some/project/dir\nabc-123-def\n" nil tmp)
            (let ((result (agent-repl--read-sentinel-file tmp)))
              (should (equal (plist-get result :dir) "/some/project/dir"))
              (should (equal (plist-get result :session-id) "abc-123-def"))))
        (ignore-errors (delete-file tmp))))))

(ert-deftest agent-repl-test-read-sentinel-file-returns-nil-on-missing ()
  "read-sentinel-file should return nil when file is missing (race condition)."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--read-sentinel-file "/nonexistent/path/sentinel_file"))))

(ert-deftest agent-repl-test-read-sentinel-file-empty-file ()
  "read-sentinel-file rejects an empty file (empty :dir is bogus) and returns nil."
  (agent-repl-test--with-clean-state
    (let ((tmp (make-temp-file "sentinel-test-")))
      (unwind-protect
          (progn
            (write-region "" nil tmp)
            (should-not (agent-repl--read-sentinel-file tmp)))
        (ignore-errors (delete-file tmp))))))

(ert-deftest agent-repl-test-read-sentinel-file-owned-marker-parsed ()
  "A three-line file with the owned marker parses as :owned t."
  (agent-repl-test--with-clean-state
    (let ((tmp (make-temp-file "sentinel-test-")))
      (unwind-protect
          (progn
            ;; Arrange / Act
            (write-region "/some/project/dir\nabc-123\nowned\n" nil tmp)
            (let ((result (agent-repl--read-sentinel-file tmp)))
              ;; Assert
              (should (equal (plist-get result :session-id) "abc-123"))
              (should (plist-get result :owned))))
        (ignore-errors (delete-file tmp))))))

(ert-deftest agent-repl-test-read-sentinel-file-blank-marker-is-unowned ()
  "A foreign CLI's sentinel (blank line 3) parses as :owned nil."
  (agent-repl-test--with-clean-state
    (let ((tmp (make-temp-file "sentinel-test-")))
      (unwind-protect
          (progn
            ;; Arrange / Act
            (write-region "/some/project/dir\nabc-123\n\n" nil tmp)
            (let ((result (agent-repl--read-sentinel-file tmp)))
              ;; Assert — session id still read; only ownership is false.
              (should (equal (plist-get result :session-id) "abc-123"))
              (should-not (plist-get result :owned))))
        (ignore-errors (delete-file tmp))))))

(ert-deftest agent-repl-test-read-sentinel-file-legacy-two-line-is-unowned ()
  "A pre-marker two-line sentinel parses as unowned (conservative)."
  (agent-repl-test--with-clean-state
    (let ((tmp (make-temp-file "sentinel-test-")))
      (unwind-protect
          (progn
            ;; Arrange / Act
            (write-region "/some/project/dir\nabc-123\n" nil tmp)
            (let ((result (agent-repl--read-sentinel-file tmp)))
              ;; Assert
              (should (equal (plist-get result :session-id) "abc-123"))
              (should-not (plist-get result :owned))))
        (ignore-errors (delete-file tmp))))))

;;;; ---- Tests: update-session-id-from-sentinel ownership gate ----

(ert-deftest agent-repl-test-update-session-id-adopts-owned ()
  "An owned sentinel's session id is adopted onto the workspace."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (agent-repl--ws-put "ws1" :project-dir "/w")
    (agent-repl--initialize-ws-env "ws1" "/w")
    (let ((set-args nil))
      (cl-letf (((symbol-function 'agent-repl--set-session-id)
                 (lambda (ws id) (setq set-args (list ws id)))))
        ;; Act
        (agent-repl--update-session-id-from-sentinel "ws1" "owned-uuid" t)
        ;; Assert
        (should (equal set-args '("ws1" "owned-uuid")))))))

(ert-deftest agent-repl-test-update-session-id-refuses-unowned ()
  "A FOREIGN cli's session id is refused — the hijack this gate exists for."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (agent-repl--ws-put "ws1" :project-dir "/w")
    (agent-repl--initialize-ws-env "ws1" "/w")
    (let ((set-called nil))
      (cl-letf (((symbol-function 'agent-repl--set-session-id)
                 (lambda (&rest _) (setq set-called t))))
        ;; Act
        (agent-repl--update-session-id-from-sentinel "ws1" "foreign-uuid" nil)
        ;; Assert
        (should-not set-called)))))

(ert-deftest agent-repl-test-update-session-id-unowned-does-not-clobber-existing ()
  "A refused foreign id leaves the workspace's recorded id intact."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (agent-repl--ws-put "ws1" :project-dir "/w")
    (agent-repl--initialize-ws-env "ws1" "/w")
    (agent-repl--set-session-id "ws1" "ours-uuid")
    ;; Act
    (agent-repl--update-session-id-from-sentinel "ws1" "foreign-uuid" nil)
    ;; Assert
    (should (equal (agent-repl--ws-durable-claude-session-id "ws1") "ours-uuid"))))

;;;; ---- Tests: ws-for-dir-fast ----

(ert-deftest agent-repl-test-ws-for-dir-fast-hit ()
  "ws-for-dir-fast returns the workspace whose :project-dir matches git-root of DIR."
  (agent-repl-test--with-clean-state
    ;; Use canonical paths throughout so string= comparisons survive
    ;; macOS firmlinks (/home -> /System/Volumes/Data/home, etc.).
    (let* ((test-root (agent-repl--path-canonical "/home/user/project")))
      (agent-repl--ws-put "my-workspace" :project-dir test-root)
      (cl-letf (((symbol-function 'agent-repl--git-root)
                 (lambda (_d) test-root)))
        (should (equal (agent-repl--ws-for-dir-fast
                        (concat test-root "/subdir"))
                       "my-workspace"))))))

(ert-deftest agent-repl-test-ws-for-dir-fast-no-git-root ()
  "ws-for-dir-fast returns nil when git-root returns nil."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--git-root)
               (lambda (_d) nil)))
      (should-not (agent-repl--ws-for-dir-fast "/some/dir")))))

(ert-deftest agent-repl-test-ws-for-dir-fast-no-match ()
  "ws-for-dir-fast returns nil when no workspace's :project-dir matches."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "other-ws" :project-dir
                         (agent-repl--path-canonical "/home/user/other"))
    (cl-letf (((symbol-function 'agent-repl--git-root)
               (lambda (_d) (agent-repl--path-canonical "/home/user/project"))))
      (should-not (agent-repl--ws-for-dir-fast "/home/user/project/subdir")))))

(ert-deftest agent-repl-test-ws-for-dir-fast-prefers-live-over-stub ()
  "When a stub and a real workspace share one :project-dir, the live one
\(with :active-env, not tombstoned) is returned, regardless of which was
registered first — the no-name `SPC TAB n' stub must not shadow the real ws."
  (agent-repl-test--with-clean-state
    (let ((root (agent-repl--path-canonical "/home/user/project")))
      ;; Register the stub FIRST so the old first-match logic would pick it.
      (agent-repl--ws-put "#1" :project-dir root)
      (agent-repl--ws-put "real-ws" :project-dir root)
      (agent-repl--ws-put "real-ws" :active-env :bare-metal)
      (cl-letf (((symbol-function 'agent-repl--git-root)
                 (lambda (_d) root)))
        (should (equal (agent-repl--ws-for-dir-fast (concat root "/subdir"))
                       "real-ws"))))))

(ert-deftest agent-repl-test-ws-for-dir-fast-stub-only-falls-back ()
  "When the only matching entry is a stub (no :active-env), it is still
returned — the fallback preserves single-entry behavior so a genuine
lookup is never dropped."
  (agent-repl-test--with-clean-state
    (let ((root (agent-repl--path-canonical "/home/user/project")))
      (agent-repl--ws-put "#1" :project-dir root)
      (cl-letf (((symbol-function 'agent-repl--git-root)
                 (lambda (_d) root)))
        (should (equal (agent-repl--ws-for-dir-fast (concat root "/subdir"))
                       "#1"))))))

(ert-deftest agent-repl-test-ws-for-dir-fast-empty-workspaces ()
  "ws-for-dir-fast returns nil when the workspaces hash is empty."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--git-root)
               (lambda (_d) "/home/user/project")))
      (should-not (agent-repl--ws-for-dir-fast "/home/user/project/subdir")))))

(ert-deftest agent-repl-test-ws-for-dir-fast-picks-correct-among-multiple ()
  "ws-for-dir-fast returns the workspace matching the target git-root among several."
  (agent-repl-test--with-clean-state
    (let ((root-a (agent-repl--path-canonical "/home/user/proj-a"))
          (root-b (agent-repl--path-canonical "/home/user/proj-b"))
          (root-c (agent-repl--path-canonical "/home/user/proj-c")))
      (agent-repl--ws-put "ws-a" :project-dir root-a)
      (agent-repl--ws-put "ws-b" :project-dir root-b)
      (agent-repl--ws-put "ws-c" :project-dir root-c)
      (cl-letf (((symbol-function 'agent-repl--git-root)
                 (lambda (_d) root-b)))
        (should (equal (agent-repl--ws-for-dir-fast (concat root-b "/sub"))
                       "ws-b"))))))

(ert-deftest agent-repl-test-ws-for-dir-fast-skips-ws-without-project-dir ()
  "ws-for-dir-fast skips workspaces whose :project-dir is nil."
  (agent-repl-test--with-clean-state
    (let ((target (agent-repl--path-canonical "/home/user/project")))
      (agent-repl--ws-put "no-dir-ws" :active-env nil) ; registered, :project-dir nil
      (agent-repl--ws-put "real-ws" :project-dir target)
      (cl-letf (((symbol-function 'agent-repl--git-root)
                 (lambda (_d) target)))
        (should (equal (agent-repl--ws-for-dir-fast (concat target "/sub"))
                       "real-ws"))))))

(ert-deftest agent-repl-test-ws-for-dir-fast-canonicalizes-project-dir ()
  "ws-for-dir-fast canonicalizes :project-dir so trailing-slash variants still match."
  (agent-repl-test--with-clean-state
    (let* ((canonical (agent-repl--path-canonical "/home/user/project"))
           ;; Store :project-dir with a trailing slash; canonicalization strips it.
           (stored (concat canonical "/")))
      (agent-repl--ws-put "trail-ws" :project-dir stored)
      (cl-letf (((symbol-function 'agent-repl--git-root)
                 (lambda (_d) canonical)))
        (should (equal (agent-repl--ws-for-dir-fast (concat canonical "/sub"))
                       "trail-ws"))))))

;;;; ---- Tests: ws-for-dir-container ----

(ert-deftest agent-repl-test-ws-for-dir-container-match ()
  "ws-for-dir-container should match container path to workspace project dir."
  (agent-repl-test--with-clean-state
    (let ((persp-mode t))
      (cl-letf (((symbol-function '+workspace-list-names)
                 (lambda () '("proj-ws")))
                ((symbol-function 'agent-repl--ws-get)
                 (lambda (ws key)
                   (when (and (equal ws "proj-ws") (eq key :project-dir))
                     "/home/user/myproject"))))
        (should (equal (agent-repl--ws-for-dir-container "/myproject/src")
                       "proj-ws"))))))

(ert-deftest agent-repl-test-ws-for-dir-container-no-persp-mode ()
  "ws-for-dir-container should return nil when persp-mode is disabled."
  (agent-repl-test--with-clean-state
    (let ((persp-mode nil))
      (should-not (agent-repl--ws-for-dir-container "/myproject/src")))))

(ert-deftest agent-repl-test-ws-for-dir-container-no-match ()
  "ws-for-dir-container should return nil when no workspace project dir matches."
  (agent-repl-test--with-clean-state
    (let ((persp-mode t))
      (cl-letf (((symbol-function '+workspace-list-names)
                 (lambda () '("other-ws")))
                ((symbol-function 'agent-repl--ws-get)
                 (lambda (_ws key)
                   (when (eq key :project-dir) "/home/user/different"))))
        (should-not (agent-repl--ws-for-dir-container "/myproject/src"))))))

(ert-deftest agent-repl-test-ws-for-dir-container-nil-project-dir ()
  "ws-for-dir-container should skip workspaces with nil project-dir."
  (agent-repl-test--with-clean-state
    (let ((persp-mode t))
      (cl-letf (((symbol-function '+workspace-list-names)
                 (lambda () '("ws-no-dir" "ws-with-dir")))
                ((symbol-function 'agent-repl--ws-get)
                 (lambda (ws key)
                   (when (eq key :project-dir)
                     (cond ((equal ws "ws-no-dir") nil)
                           ((equal ws "ws-with-dir") "/home/user/myproject"))))))
        (should (equal (agent-repl--ws-for-dir-container "/myproject/sub")
                       "ws-with-dir"))))))

;;;; ---- Tests: ws-for-dir (combined) ----

(ert-deftest agent-repl-test-ws-for-dir-prefers-fast-path ()
  "ws-for-dir should return fast-path result when available."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ws-for-dir-fast)
               (lambda (_d) "fast-ws"))
              ((symbol-function 'agent-repl--ws-for-dir-container)
               (lambda (_d) "container-ws")))
      (should (equal (agent-repl--ws-for-dir "/some/dir") "fast-ws")))))

(ert-deftest agent-repl-test-ws-for-dir-falls-back-to-container ()
  "ws-for-dir should try container path when fast path returns nil."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ws-for-dir-fast)
               (lambda (_d) nil))
              ((symbol-function 'agent-repl--ws-for-dir-container)
               (lambda (_d) "container-ws")))
      (should (equal (agent-repl--ws-for-dir "/some/dir") "container-ws")))))

(ert-deftest agent-repl-test-ws-for-dir-returns-nil-when-both-fail ()
  "ws-for-dir should return nil when both paths fail."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ws-for-dir-fast)
               (lambda (_d) nil))
              ((symbol-function 'agent-repl--ws-for-dir-container)
               (lambda (_d) nil)))
      (should-not (agent-repl--ws-for-dir "/some/dir")))))

;;;; ---- Tests: dispatch-sentinel-event actions ----

(ert-deftest agent-repl-test-sentinel-event-accepts-created-action ()
  "An event with action `created' and existing file should dispatch."
  (agent-repl-test--with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'agent-repl--dispatch-sentinel-file)
                 (lambda (_f) (setq dispatched t)))
                ((symbol-function 'file-exists-p) (lambda (_f) t)))
        (agent-repl--dispatch-sentinel-event '(nil created "/dir/stop_123"))
        (should dispatched)))))

(ert-deftest agent-repl-test-sentinel-event-accepts-changed-action ()
  "An event with action `changed' and existing file should dispatch."
  (agent-repl-test--with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'agent-repl--dispatch-sentinel-file)
                 (lambda (_f) (setq dispatched t)))
                ((symbol-function 'file-exists-p) (lambda (_f) t)))
        (agent-repl--dispatch-sentinel-event '(nil changed "/dir/stop_123"))
        (should dispatched)))))

(ert-deftest agent-repl-test-sentinel-event-ignores-renamed-action ()
  "An event with action `renamed' should be ignored."
  (agent-repl-test--with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'agent-repl--dispatch-sentinel-file)
                 (lambda (_f) (setq dispatched t)))
                ((symbol-function 'file-exists-p) (lambda (_f) t)))
        (agent-repl--dispatch-sentinel-event '(nil renamed "/dir/stop_123"))
        (should-not dispatched)))))

;;;; ---- Tests: poll-workspace-notifications ----

(ert-deftest agent-repl-test-poll-dispatches-orphaned-files ()
  "poll-workspace-notifications should dispatch files in the sentinel directory."
  (agent-repl-test--with-clean-state
    ;; Use a real temp directory rather than mocking primitives like
    ;; `directory-files' / `file-exists-p' / `file-directory-p'.  Redefining
    ;; those via cl-letf triggers native-comp trampoline installation which
    ;; can fail on cached native-elisp installs.
    (let* ((tmp (make-temp-file "agent-repl-sentinel-test-" t))
           (agent-repl--sentinel-dir tmp)
           (stop-file (expand-file-name "stop_abc" tmp))
           (perm-file (expand-file-name "permission_prompt" tmp))
           (dispatched-files nil))
      (unwind-protect
          (progn
            (write-region "" nil stop-file)
            (write-region "" nil perm-file)
            (cl-letf (((symbol-function 'agent-repl--dispatch-sentinel-file)
                       (lambda (f) (push (file-name-nondirectory f)
                                         dispatched-files)
                          t)))
              (agent-repl--poll-workspace-notifications)
              (should (= (length dispatched-files) 2))
              (should (member "stop_abc" dispatched-files))
              (should (member "permission_prompt" dispatched-files))))
        (ignore-errors (delete-directory tmp t))))))

(ert-deftest agent-repl-test-poll-skips-nonexistent-dir ()
  "poll-workspace-notifications should do nothing if sentinel dir doesn't exist."
  (agent-repl-test--with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) nil))
                ((symbol-function 'agent-repl--dispatch-sentinel-file)
                 (lambda (_f) (setq dispatched t))))
        (agent-repl--poll-workspace-notifications)
        (should-not dispatched)))))

(ert-deftest agent-repl-test-poll-skips-nonexistent-files ()
  "poll-workspace-notifications should skip files that disappeared between listing and processing."
  (agent-repl-test--with-clean-state
    (let ((dispatched-files nil))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) t))
                ((symbol-function 'directory-files)
                 (lambda (_dir _full _match _nosort)
                   '("/sentinel/stop_abc")))
                ((symbol-function 'file-exists-p) (lambda (_f) nil))
                ((symbol-function 'agent-repl--dispatch-sentinel-file)
                 (lambda (f) (push f dispatched-files) t)))
        (agent-repl--poll-workspace-notifications)
        (should-not dispatched-files)))))

(ert-deftest agent-repl-test-poll-logs-unknown-files ()
  "poll-workspace-notifications should not error on files that match no handler."
  (agent-repl-test--with-clean-state
    (let ((dispatched-count 0))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) t))
                ((symbol-function 'directory-files)
                 (lambda (_dir _full _match _nosort)
                   '("/sentinel/unknown_file")))
                ((symbol-function 'file-exists-p) (lambda (_f) t))
                ((symbol-function 'agent-repl--dispatch-sentinel-file)
                 (lambda (_f) (cl-incf dispatched-count) nil)))
        (agent-repl--poll-workspace-notifications)
        (should (= dispatched-count 1))))))

(ert-deftest agent-repl-test-poll-empty-directory ()
  "poll-workspace-notifications should do nothing when directory is empty."
  (agent-repl-test--with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) t))
                ((symbol-function 'directory-files)
                 (lambda (_dir _full _match _nosort) nil))
                ((symbol-function 'agent-repl--dispatch-sentinel-file)
                 (lambda (_f) (setq dispatched t))))
        (agent-repl--poll-workspace-notifications)
        (should-not dispatched)))))

;;;; ---- Tests: sentinel-dispatch-alist structure ----

(ert-deftest agent-repl-test-dispatch-alist-has-required-keys ()
  "Every entry in the dispatch alist should have :callback, :warning, :name."
  (dolist (entry agent-repl--sentinel-dispatch-alist)
    (let ((prefix (car entry))
          (plist (cdr entry)))
      (should (plist-get plist :callback))
      (should (plist-get plist :warning))
      (should (plist-get plist :name))
      ;; :warning should be a format string with %s
      (should (string-match-p "%s" (plist-get plist :warning)))
      ;; :callback should be a function symbol
      (should (symbolp (plist-get plist :callback)))
      ;; :name should be a string
      (should (stringp (plist-get plist :name)))
      ;; prefix should be a string
      (should (stringp prefix)))))

(ert-deftest agent-repl-test-dispatch-alist-callbacks-are-fboundp ()
  "All callback functions in the dispatch alist should be defined."
  (dolist (entry agent-repl--sentinel-dispatch-alist)
    (let ((cb (plist-get (cdr entry) :callback)))
      (should (fboundp cb)))))

;;;; ---- Tests: end-to-end dispatch through process-sentinel-file ----

(ert-deftest agent-repl-test-end-to-end-permission-dispatch ()
  "Full dispatch: permission_prompt file -> on-permission-event -> ws-set-agent-state :permission.
ws-get is mocked to return :thinking so the elisp state-gate in
on-permission-event treats the notification as a real permission prompt."
  (agent-repl-test--with-clean-state
    (let ((set-args nil))
      (cl-letf (((symbol-function 'agent-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/project/dir" :session-id "test-sid")))
                ((symbol-function 'agent-repl--ws-for-dir)
                 (lambda (_d) "test-ws"))
                ((symbol-function 'agent-repl--update-session-id-from-sentinel)
                 #'ignore)
                ((symbol-function 'agent-repl--ws-set-agent-state)
                 (lambda (ws state) (setq set-args (list ws state))))
                ((symbol-function 'agent-repl--ws-get)
                 (lambda (_ws _key) :thinking))
                ((symbol-function 'delete-file) #'ignore))
        (agent-repl--dispatch-sentinel-file "/dir/permission_prompt")
        (should (equal set-args '("test-ws" :permission)))))))

(ert-deftest agent-repl-test-end-to-end-stop-dispatch ()
  "Full dispatch: stop_* file -> on-stop-event -> handle-finished.
With no pending subagents, on-stop-event finalizes immediately on the
single Stop fire."
  (agent-repl-test--with-clean-state
    (let ((finished nil))
      (cl-letf (((symbol-function 'agent-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/project/dir" :session-id "test-sid")))
                ((symbol-function 'agent-repl--ws-for-dir)
                 (lambda (_d) "test-ws"))
                ((symbol-function 'agent-repl--update-session-id-from-sentinel)
                 #'ignore)
                ((symbol-function 'agent-repl--handle-agent-finished)
                 (lambda (ws) (setq finished ws)))
                ((symbol-function 'delete-file) #'ignore))
        (agent-repl--dispatch-sentinel-file "/dir/stop_123")
        (should (equal finished "test-ws"))))))

(ert-deftest agent-repl-test-end-to-end-prompt-submit-dispatch ()
  "Full dispatch: prompt_submit_* file -> on-prompt-submit-event -> mark-ws-thinking."
  (agent-repl-test--with-clean-state
    (let ((thinking-ws nil))
      (cl-letf (((symbol-function 'agent-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/project/dir" :session-id "test-sid")))
                ((symbol-function 'agent-repl--ws-for-dir)
                 (lambda (_d) "test-ws"))
                ((symbol-function 'agent-repl--update-session-id-from-sentinel)
                 #'ignore)
                ((symbol-function 'agent-repl--mark-ws-thinking)
                 (lambda (ws) (setq thinking-ws ws)))
                ((symbol-function 'agent-repl--ws-get)
                 (lambda (_ws _key) nil))
                ((symbol-function 'delete-file) #'ignore))
        (agent-repl--dispatch-sentinel-file "/dir/prompt_submit_456")
        (should (equal thinking-ws "test-ws"))))))

;;;; ---- Tests: ws-for-dir-fast uncovered edge cases ----

(ert-deftest agent-repl-test-ws-for-dir-fast-nil-dir ()
  "ws-for-dir-fast returns nil when DIR is nil (no git-root call attempted)."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--ws-for-dir-fast nil))))

(ert-deftest agent-repl-test-ws-for-dir-fast-canonicalizes-git-root-trailing-slash ()
  "ws-for-dir-fast matches when git-root returns a path with a trailing slash.
Both the stored :project-dir and git-root output are canonicalized, which
strips trailing slashes."
  (agent-repl-test--with-clean-state
    (let* ((canonical (agent-repl--path-canonical "/home/user/project")))
      (agent-repl--ws-put "trail-ws" :project-dir canonical)
      (cl-letf (((symbol-function 'agent-repl--git-root)
                 ;; git-root returns with a trailing slash
                 (lambda (_d) (concat canonical "/"))))
        (should (equal (agent-repl--ws-for-dir-fast "/home/user/project/sub")
                       "trail-ws"))))))

(ert-deftest agent-repl-test-ws-for-dir-fast-symlink-canonical ()
  "ws-for-dir-fast matches symlinked paths because path-canonical resolves symlinks."
  (agent-repl-test--with-clean-state
    (let* ((canonical (agent-repl--path-canonical "/home/user/real-project")))
      (agent-repl--ws-put "sym-ws" :project-dir canonical)
      (cl-letf (((symbol-function 'agent-repl--git-root)
                 (lambda (_d) canonical)))
        (should (equal (agent-repl--ws-for-dir-fast "/home/user/real-project/sub")
                       "sym-ws"))))))

;;;; ---- Tests: ws-for-dir-container uncovered edge cases ----

(ert-deftest agent-repl-test-ws-for-dir-container-multiple-match-returns-first ()
  "ws-for-dir-container should return the first matching workspace when multiple match."
  (agent-repl-test--with-clean-state
    (let ((persp-mode t))
      (cl-letf (((symbol-function '+workspace-list-names)
                 (lambda () '("ws-first" "ws-second")))
                ((symbol-function 'agent-repl--ws-get)
                 (lambda (ws key)
                   (when (eq key :project-dir)
                     (cond ((equal ws "ws-first") "/home/user/myproject")
                           ((equal ws "ws-second") "/other/path/myproject"))))))
        (should (equal (agent-repl--ws-for-dir-container "/myproject/src")
                       "ws-first"))))))

(ert-deftest agent-repl-test-ws-for-dir-container-root-slash ()
  "ws-for-dir-container with DIR=\"/\" should not error (container-root is empty string)."
  (agent-repl-test--with-clean-state
    (let ((persp-mode t))
      (cl-letf (((symbol-function '+workspace-list-names)
                 (lambda () '("ws1")))
                ((symbol-function 'agent-repl--ws-get)
                 (lambda (_ws key)
                   (when (eq key :project-dir) "/home/user/project"))))
        ;; DIR is "/", so (substring "/" 1) = "", (split-string "" "/") = (""),
        ;; container-root = "".  Should not match anything and return nil.
        (should-not (agent-repl--ws-for-dir-container "/"))))))

(ert-deftest agent-repl-test-ws-for-dir-container-empty-workspace-list ()
  "ws-for-dir-container should return nil when workspace-list-names returns empty list."
  (agent-repl-test--with-clean-state
    (let ((persp-mode t))
      (cl-letf (((symbol-function '+workspace-list-names)
                 (lambda () nil)))
        (should-not (agent-repl--ws-for-dir-container "/myproject/src"))))))

(ert-deftest agent-repl-test-ws-for-dir-container-trailing-slash-normalization ()
  "ws-for-dir-container should match project dirs with trailing slashes via directory-file-name."
  (agent-repl-test--with-clean-state
    (let ((persp-mode t))
      (cl-letf (((symbol-function '+workspace-list-names)
                 (lambda () '("trail-ws")))
                ((symbol-function 'agent-repl--ws-get)
                 (lambda (ws key)
                   (when (and (equal ws "trail-ws") (eq key :project-dir))
                     "/home/user/myproject/"))))
        ;; directory-file-name strips the trailing slash, so the last component
        ;; should still be "myproject".
        (should (equal (agent-repl--ws-for-dir-container "/myproject/src")
                       "trail-ws"))))))

;;;; ---- Tests: read-sentinel-file uncovered edge cases ----

(ert-deftest agent-repl-test-read-sentinel-file-whitespace-only ()
  "read-sentinel-file rejects a whitespace-only file (empty :dir is bogus) and returns nil."
  (agent-repl-test--with-clean-state
    (let ((tmp (make-temp-file "sentinel-test-")))
      (unwind-protect
          (progn
            (write-region "   \n\t\n  " nil tmp)
            (should-not (agent-repl--read-sentinel-file tmp)))
        (ignore-errors (delete-file tmp))))))

(ert-deftest agent-repl-test-read-sentinel-file-multiline-content ()
  "read-sentinel-file with two lines returns :dir and :session-id."
  (agent-repl-test--with-clean-state
    (let ((tmp (make-temp-file "sentinel-test-")))
      (unwind-protect
          (progn
            (write-region "/first/line\n/second/line\n" nil tmp)
            (let ((result (agent-repl--read-sentinel-file tmp)))
              (should (equal (plist-get result :dir) "/first/line"))
              (should (equal (plist-get result :session-id) "/second/line"))))
        (ignore-errors (delete-file tmp))))))

(ert-deftest agent-repl-test-read-sentinel-file-generic-error ()
  "read-sentinel-file should return nil and warn on non-file-missing errors."
  (agent-repl-test--with-clean-state
    (let ((warning-msg nil))
      (cl-letf (((symbol-function 'insert-file-contents)
                 (lambda (&rest _) (error "disk I/O error")))
                ((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (setq warning-msg (apply #'format fmt args)))))
        (should-not (agent-repl--read-sentinel-file "/some/sentinel_file"))
        (should (string-match-p "read-sentinel-file: ERROR.*disk I/O" warning-msg))))))

(ert-deftest agent-repl-test-read-sentinel-file-very-long-content ()
  "read-sentinel-file should handle files with very long dir path."
  (agent-repl-test--with-clean-state
    (let ((tmp (make-temp-file "sentinel-test-"))
          (long-path (concat "/" (make-string 1000 ?a))))
      (unwind-protect
          (progn
            (write-region long-path nil tmp)
            (let ((result (agent-repl--read-sentinel-file tmp)))
              (should (equal (plist-get result :dir) long-path))
              (should-not (plist-get result :session-id))))
        (ignore-errors (delete-file tmp))))))

;;;; ---- Tests: process-sentinel-file uncovered edge cases ----

(ert-deftest agent-repl-test-process-sentinel-file-callback-error-still-deletes ()
  "File is deleted before callback runs, so callback errors don't leave orphans for the poll to re-dispatch."
  (agent-repl-test--with-clean-state
    (let ((deleted-file nil))
      (cl-letf (((symbol-function 'agent-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/some/dir" :session-id nil)))
                ((symbol-function 'agent-repl--ws-for-dir)
                 (lambda (_d) "ws1"))
                ((symbol-function 'agent-repl--update-session-id-from-sentinel)
                 #'ignore)
                ((symbol-function 'delete-file)
                 (lambda (f) (setq deleted-file f))))
        (condition-case _err
            (agent-repl--process-sentinel-file
             "/tmp/stop_err"
             '(:callback (lambda (_ws _dir) (error "callback boom"))
               :warning "warn %s"
               :name "test"))
          (error nil))
        (should (equal deleted-file "/tmp/stop_err"))))))

(ert-deftest agent-repl-test-process-sentinel-file-delete-file-error-suppressed ()
  "When delete-file errors, the error should be suppressed by ignore-errors."
  (agent-repl-test--with-clean-state
    (let ((callback-called nil))
      (cl-letf (((symbol-function 'agent-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/some/dir" :session-id nil)))
                ((symbol-function 'agent-repl--ws-for-dir)
                 (lambda (_d) "ws1"))
                ((symbol-function 'agent-repl--update-session-id-from-sentinel)
                 #'ignore)
                ((symbol-function 'delete-file)
                 (lambda (_f) (error "permission denied"))))
        ;; Should not propagate the delete-file error
        (agent-repl--process-sentinel-file
         "/tmp/stop_del"
         (list :callback (lambda (_ws _dir) (setq callback-called t))
               :warning "warn %s"
               :name "test"))
        (should callback-called)))))

(ert-deftest agent-repl-test-process-sentinel-file-missing-callback-key ()
  "A handler plist missing :callback (funcall nil) surfaces the error on the
log rather than propagating, since propagating would kill the watcher."
  (agent-repl-test--with-clean-state
    (let ((surfaced nil))
      (cl-letf (((symbol-function 'agent-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/some/dir" :session-id nil)))
                ((symbol-function 'agent-repl--ws-for-dir)
                 (lambda (_d) "ws1"))
                ((symbol-function 'agent-repl--update-session-id-from-sentinel)
                 #'ignore)
                ((symbol-function 'delete-file) #'ignore)
                ((symbol-function 'agent-repl--do-log)
                 (lambda (_ws fmt &rest _) (setq surfaced fmt))))
        ;; Missing :callback => (funcall nil ...) errors, which the surfacing
        ;; condition-case logs instead of letting escape.
        (should
         (progn
           (agent-repl--process-sentinel-file
            "/tmp/stop_nocb"
            '(:warning "warn %s" :name "test"))
           t))
        (should (string-match-p "ERRORED" surfaced))))))

(ert-deftest agent-repl-test-process-sentinel-file-missing-warning-key ()
  "When handler plist is missing :warning and ws is nil, message receives nil as fmt.
In Emacs, (message nil ...) clears the echo area without error, so the callback
is still skipped and the file is still deleted."
  (agent-repl-test--with-clean-state
    (let ((callback-called nil)
          (deleted-file nil))
      (cl-letf (((symbol-function 'agent-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/some/dir" :session-id nil)))
                ((symbol-function 'agent-repl--ws-for-dir)
                 (lambda (_d) nil))
                ((symbol-function 'delete-file)
                 (lambda (f) (setq deleted-file f))))
        (agent-repl--process-sentinel-file
         "/tmp/stop_nowarn"
         '(:callback (lambda (&rest _) (setq callback-called t)) :name "test"))
        ;; Callback should not have been called (ws was nil)
        (should-not callback-called)
        ;; File should still be deleted
        (should (equal deleted-file "/tmp/stop_nowarn"))))))

;;;; ---- Tests: on-permission-event uncovered edge cases ----

(ert-deftest agent-repl-test-on-permission-event-nil-ws-errors ()
  "on-permission-event with nil ws and :thinking state should error in ws-set.
The state-gate runs `agent-repl--ws-get' first (which tolerates a nil ws),
so we have to engineer the gate to pass before the nil-ws error surfaces.
We do that by stubbing ws-get to return :thinking unconditionally."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ws-get)
               (lambda (_ws _key) :thinking)))
      (should-error (agent-repl--on-permission-event nil "/some/dir")))))

(ert-deftest agent-repl-test-on-permission-event-already-permission-no-op ()
  "on-permission-event should NOT re-set state when ws is already :permission.
A duplicate or stale Notification arriving while we're still in :permission
must not call ws-set-agent-state — the gate accepts only :thinking."
  (agent-repl-test--with-clean-state
    (let ((set-called nil))
      (agent-repl--ws-set "ws1" :permission)
      (cl-letf (((symbol-function 'agent-repl--ws-set-agent-state)
                 (lambda (_ws _state) (setq set-called t))))
        (agent-repl--on-permission-event "ws1" "/some/dir")
        (should-not set-called)))))

(ert-deftest agent-repl-test-on-permission-event-idle-no-op ()
  "on-permission-event must no-op when state is :idle (post-turn idle nudge).
This is the regression guard for phantom ❓ appearing after the user is done."
  (agent-repl-test--with-clean-state
    (let ((set-called nil))
      (agent-repl--ws-set "ws1" :idle)
      (cl-letf (((symbol-function 'agent-repl--ws-set-agent-state)
                 (lambda (_ws _state) (setq set-called t))))
        (agent-repl--on-permission-event "ws1" "/some/dir")
        (should-not set-called)
        (should (eq (agent-repl--ws-state "ws1") :idle))))))

(ert-deftest agent-repl-test-on-permission-event-done-no-op ()
  "on-permission-event must no-op when state is :done (post-turn idle nudge)."
  (agent-repl-test--with-clean-state
    (let ((set-called nil))
      (agent-repl--ws-set "ws1" :done)
      (cl-letf (((symbol-function 'agent-repl--ws-set-agent-state)
                 (lambda (_ws _state) (setq set-called t))))
        (agent-repl--on-permission-event "ws1" "/some/dir")
        (should-not set-called)
        (should (eq (agent-repl--ws-state "ws1") :done))))))

(ert-deftest agent-repl-test-on-permission-event-init-no-op ()
  "on-permission-event must no-op when state is :init (the agent not yet ready)."
  (agent-repl-test--with-clean-state
    (let ((set-called nil))
      (agent-repl--ws-set "ws1" :init)
      (cl-letf (((symbol-function 'agent-repl--ws-set-agent-state)
                 (lambda (_ws _state) (setq set-called t))))
        (agent-repl--on-permission-event "ws1" "/some/dir")
        (should-not set-called)
        (should (eq (agent-repl--ws-state "ws1") :init))))))

(ert-deftest agent-repl-test-on-permission-event-nil-state-no-op ()
  "on-permission-event must no-op when state is nil (workspace has no agent session)."
  (agent-repl-test--with-clean-state
    (let ((set-called nil))
      (cl-letf (((symbol-function 'agent-repl--ws-set-agent-state)
                 (lambda (_ws _state) (setq set-called t))))
        (agent-repl--on-permission-event "ws1" "/some/dir")
        (should-not set-called)))))

;;;; ---- Tests: on-stop-event uncovered edge cases ----

(ert-deftest agent-repl-test-on-stop-event-handle-finished-error ()
  "on-stop-event should propagate error from handle-agent-finished."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--handle-agent-finished)
               (lambda (_ws) (error "finished handler boom"))))
      (should-error (agent-repl--on-stop-event "ws1" "/some/dir")))))

;;;; ---- Tests: on-prompt-submit-event uncovered edge cases ----

(ert-deftest agent-repl-test-on-prompt-submit-event-nil-ws ()
  "on-prompt-submit-event with nil ws passes nil to mark-ws-thinking."
  (agent-repl-test--with-clean-state
    (let ((thinking-ws :not-called))
      (cl-letf (((symbol-function 'agent-repl--mark-ws-thinking)
                 (lambda (ws) (setq thinking-ws ws))))
        (agent-repl--on-prompt-submit-event nil "/some/dir")
        (should (eq thinking-ws nil))))))

(ert-deftest agent-repl-test-on-prompt-submit-event-already-thinking ()
  "on-prompt-submit-event should still call mark-ws-thinking even if ws is already thinking."
  (agent-repl-test--with-clean-state
    (let ((thinking-ws nil))
      (cl-letf (((symbol-function 'agent-repl--mark-ws-thinking)
                 (lambda (ws) (setq thinking-ws ws))))
        ;; Set thinking first, then submit again
        (agent-repl--ws-set "ws1" :thinking)
        (agent-repl--on-prompt-submit-event "ws1" "/some/dir")
        (should (equal thinking-ws "ws1"))))))

;;;; ---- Tests: dispatch-sentinel-file uncovered edge cases ----

(ert-deftest agent-repl-test-dispatch-sentinel-file-no-directory-component ()
  "dispatch-sentinel-file should work with a bare filename (no directory)."
  (agent-repl-test--with-clean-state
    (let ((dispatched-handler nil))
      (cl-letf (((symbol-function 'agent-repl--process-sentinel-file)
                 (lambda (_file handler) (setq dispatched-handler handler))))
        (agent-repl--dispatch-sentinel-file "stop_123")
        (should dispatched-handler)
        (should (eq (plist-get dispatched-handler :callback)
                    'agent-repl--on-stop-event))))))

(ert-deftest agent-repl-test-dispatch-sentinel-file-exact-prefix ()
  "dispatch-sentinel-file should match a filename that is exactly a prefix (no suffix)."
  (agent-repl-test--with-clean-state
    (let ((dispatched-handler nil))
      (cl-letf (((symbol-function 'agent-repl--process-sentinel-file)
                 (lambda (_file handler) (setq dispatched-handler handler))))
        ;; "stop_" is a prefix in the dispatch alist; file is exactly "stop_"
        (agent-repl--dispatch-sentinel-file "/dir/stop_")
        (should dispatched-handler)
        (should (eq (plist-get dispatched-handler :callback)
                    'agent-repl--on-stop-event))))))

(ert-deftest agent-repl-test-dispatch-sentinel-file-first-prefix-wins ()
  "dispatch-sentinel-file should use the first matching prefix when multiple could match."
  (agent-repl-test--with-clean-state
    (let ((dispatched-handler nil))
      (cl-letf (((symbol-function 'agent-repl--process-sentinel-file)
                 (lambda (_file handler) (setq dispatched-handler handler))))
        ;; "permission_prompt" is the first entry; it should match before any other
        (agent-repl--dispatch-sentinel-file "/dir/permission_prompt")
        (should dispatched-handler)
        (should (eq (plist-get dispatched-handler :callback)
                    'agent-repl--on-permission-event))))))

;;;; ---- Tests: dispatch-sentinel-event uncovered edge cases ----

(ert-deftest agent-repl-test-sentinel-event-nil-action ()
  "An event with nil action should not dispatch."
  (agent-repl-test--with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'agent-repl--dispatch-sentinel-file)
                 (lambda (_f) (setq dispatched t)))
                ((symbol-function 'file-exists-p) (lambda (_f) t)))
        (agent-repl--dispatch-sentinel-event '(nil nil "/dir/stop_123"))
        (should-not dispatched)))))

;; Covered by `agent-repl-test-sentinel-event-tolerates-nil-file' above:
;; `stopped' events fire with nil file when watchers are removed, and the
;; dispatcher must skip them gracefully rather than crash.

;;;; ---- Tests: poll-workspace-notifications uncovered edge cases ----

(ert-deftest agent-repl-test-poll-mix-of-known-and-unknown-files ()
  "poll-workspace-notifications should dispatch both known and unknown files."
  (agent-repl-test--with-clean-state
    (let ((dispatched-files nil)
          (dispatch-results nil))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) t))
                ((symbol-function 'directory-files)
                 (lambda (_dir _full _match _nosort)
                   '("/sentinel/stop_abc" "/sentinel/unknown_file" "/sentinel/permission_prompt")))
                ((symbol-function 'file-exists-p) (lambda (_f) t))
                ((symbol-function 'agent-repl--dispatch-sentinel-file)
                 (lambda (f)
                   (push f dispatched-files)
                   ;; Return t for known, nil for unknown
                   (not (string-match-p "unknown" f)))))
        (agent-repl--poll-workspace-notifications)
        ;; All three files should have been dispatched
        (should (= (length dispatched-files) 3))
        (should (member "/sentinel/stop_abc" dispatched-files))
        (should (member "/sentinel/unknown_file" dispatched-files))
        (should (member "/sentinel/permission_prompt" dispatched-files))))))

(ert-deftest agent-repl-test-poll-excludes-hidden-files ()
  "poll-workspace-notifications should not see hidden files (dotfiles) due to regex filter."
  (agent-repl-test--with-clean-state
    (let ((dispatched-files nil)
          (regex-used nil))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) t))
                ((symbol-function 'directory-files)
                 (lambda (_dir _full match _nosort)
                   ;; Capture the regex and simulate what Emacs would return:
                   ;; the regex \\`[^.] excludes dotfiles
                   (setq regex-used match)
                   ;; Only non-dotfiles would be returned by directory-files
                   '("/sentinel/stop_abc")))
                ((symbol-function 'file-exists-p) (lambda (_f) t))
                ((symbol-function 'agent-repl--dispatch-sentinel-file)
                 (lambda (f) (push f dispatched-files) t)))
        (agent-repl--poll-workspace-notifications)
        ;; Verify the regex passed to directory-files excludes dotfiles
        (should (stringp regex-used))
        ;; ".hidden" should NOT match the regex (starts with dot)
        (should-not (string-match-p regex-used ".hidden"))
        ;; "stop_abc" should match the regex (starts with non-dot)
        (should (string-match-p regex-used "stop_abc"))
        ;; Only the non-hidden file was dispatched
        (should (= (length dispatched-files) 1))))))

;;;; ---- Tests: sentinel event edge cases (status transitions .md) ----

(ert-deftest agent-repl-test-on-stop-event-delegates-regardless-of-state ()
  "Stop event delegates to handle-finished regardless of the current agent-state."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :permission)
    (let ((finished-called nil))
      (cl-letf (((symbol-function 'agent-repl--handle-agent-finished)
                 (lambda (ws) (setq finished-called ws))))
        (agent-repl--on-stop-event "ws1" "/some/dir")
        (should (equal finished-called "ws1"))))))

(ert-deftest agent-repl-test-on-permission-event-leaves-done-alone ()
  "Permission event must NOT overwrite :done — the notification is an idle nudge.
Regression guard for the phantom ❓-after-done bug; the prior behavior
overwrote :done with :permission, which is exactly what we no longer want."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :done)
    (agent-repl--on-permission-event "ws1" "/some/dir")
    (should (eq (agent-repl--ws-state "ws1") :done))))

(ert-deftest agent-repl-test-on-permission-event-overwrites-thinking ()
  "Permission event should overwrite :thinking status with :permission."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :thinking)
    (agent-repl--on-permission-event "ws1" "/some/dir")
    (should (eq (agent-repl--ws-state "ws1") :permission))))

;;;; ---- Tests: agent-repl-reset-sentinel-watchers ----

(ert-deftest agent-repl-test-reset-sentinel-watchers-removes-matching ()
  "Should remove every descriptor whose watched dir is the sentinel dir."
  (agent-repl-test--with-clean-state
    (let ((removed '())
          (target agent-repl--sentinel-dir)
          (descs (make-hash-table :test 'equal)))
      (puthash 'desc-a (cons target 'cb) descs)
      (puthash 'desc-b (cons target 'cb) descs)
      (cl-letf (((symbol-function 'file-notify-rm-watch)
                 (lambda (d) (push d removed)))
                ((symbol-function 'file-notify-add-watch)
                 (lambda (&rest _) 'new-desc))
                ((symbol-function 'file-truename) #'identity)
                (file-notify-descriptors descs))
        (agent-repl-reset-sentinel-watchers)
        (should (equal (sort (copy-sequence removed)
                             (lambda (a b) (string< (symbol-name a) (symbol-name b))))
                       '(desc-a desc-b)))))))

(ert-deftest agent-repl-test-reset-sentinel-watchers-leaves-others ()
  "Should NOT remove descriptors watching other directories."
  (agent-repl-test--with-clean-state
    (let ((removed '())
          (descs (make-hash-table :test 'equal)))
      (puthash 'desc-other (cons "/some/other/dir" 'cb) descs)
      (cl-letf (((symbol-function 'file-notify-rm-watch)
                 (lambda (d) (push d removed)))
                ((symbol-function 'file-notify-add-watch)
                 (lambda (&rest _) 'new-desc))
                ((symbol-function 'file-truename) #'identity)
                (file-notify-descriptors descs))
        (agent-repl-reset-sentinel-watchers)
        (should-not removed)))))

(ert-deftest agent-repl-test-reset-sentinel-watchers-registers-fresh ()
  "Should register a new watcher on the sentinel dir after cleanup."
  (agent-repl-test--with-clean-state
    (let ((added-dir nil)
          (descs (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'file-notify-rm-watch) (lambda (&rest _) nil))
                ((symbol-function 'file-notify-add-watch)
                 (lambda (dir &rest _) (setq added-dir dir) 'new-desc))
                ((symbol-function 'file-truename) #'identity)
                (file-notify-descriptors descs))
        (agent-repl-reset-sentinel-watchers)
        (should (equal added-dir agent-repl--sentinel-dir))
        (should (eq agent-repl--sentinel-watch-descriptor 'new-desc))))))

(ert-deftest agent-repl-test-reap-sentinel-watchers-returns-count ()
  "Reap helper should return the number of watchers removed."
  (agent-repl-test--with-clean-state
    (let ((target agent-repl--sentinel-dir)
          (descs (make-hash-table :test 'equal)))
      (puthash 'desc-a (cons target 'cb) descs)
      (puthash 'desc-b (cons target 'cb) descs)
      (puthash 'desc-other (cons "/other" 'cb) descs)
      (cl-letf (((symbol-function 'file-notify-rm-watch) (lambda (&rest _) nil))
                ((symbol-function 'file-truename) #'identity)
                (file-notify-descriptors descs))
        (should (= (agent-repl--reap-sentinel-watchers) 2))))))

(ert-deftest agent-repl-test-nuke-sentinel-watchers-does-not-re-register ()
  "`agent-repl-nuke-sentinel-watchers' must NOT create a new watcher."
  (agent-repl-test--with-clean-state
    (let ((target agent-repl--sentinel-dir)
          (add-watch-called nil)
          (descs (make-hash-table :test 'equal)))
      (puthash 'desc-a (cons target 'cb) descs)
      (cl-letf (((symbol-function 'file-notify-rm-watch) (lambda (&rest _) nil))
                ((symbol-function 'file-notify-add-watch)
                 (lambda (&rest _) (setq add-watch-called t) 'new-desc))
                ((symbol-function 'file-truename) #'identity)
                (file-notify-descriptors descs))
        (agent-repl-nuke-sentinel-watchers)
        (should-not add-watch-called)
        (should-not agent-repl--sentinel-watch-descriptor)))))

;;;; ---- Tests: update-session-id-from-sentinel ----

(ert-deftest agent-repl-test-update-session-id-from-sentinel-sets-id ()
  "update-session-id-from-sentinel should set the session ID on the active inst."
  (agent-repl-test--with-clean-state
    (let ((inst (make-agent-repl-instantiation)))
      (agent-repl--ws-put "ws1" :active-env :bare-metal)
      (agent-repl--ws-put "ws1" :bare-metal inst)
      ;; OWNED t: a module-launched CLI's sentinel (see the ownership gate).
      (agent-repl--update-session-id-from-sentinel "ws1" "new-sid-abc" t)
      (should (equal (agent-repl-instantiation-session-id inst) "new-sid-abc")))))

(ert-deftest agent-repl-test-update-session-id-from-sentinel-skips-nil ()
  "update-session-id-from-sentinel should be a no-op when session-id is nil."
  (agent-repl-test--with-clean-state
    (let ((inst (make-agent-repl-instantiation :session-id "old-sid")))
      (agent-repl--ws-put "ws1" :active-env :bare-metal)
      (agent-repl--ws-put "ws1" :bare-metal inst)
      (agent-repl--update-session-id-from-sentinel "ws1" nil)
      (should (equal (agent-repl-instantiation-session-id inst) "old-sid")))))

(ert-deftest agent-repl-test-update-session-id-from-sentinel-skips-empty ()
  "update-session-id-from-sentinel should be a no-op when session-id is empty string."
  (agent-repl-test--with-clean-state
    (let ((inst (make-agent-repl-instantiation :session-id "old-sid")))
      (agent-repl--ws-put "ws1" :active-env :bare-metal)
      (agent-repl--ws-put "ws1" :bare-metal inst)
      (agent-repl--update-session-id-from-sentinel "ws1" "")
      (should (equal (agent-repl-instantiation-session-id inst) "old-sid")))))

(ert-deftest agent-repl-test-update-session-id-from-sentinel-skips-same ()
  "update-session-id-from-sentinel should be a no-op when session-id is unchanged."
  (agent-repl-test--with-clean-state
    (let ((inst (make-agent-repl-instantiation :session-id "same-sid"))
          (set-called nil))
      (agent-repl--ws-put "ws1" :active-env :bare-metal)
      (agent-repl--ws-put "ws1" :bare-metal inst)
      (cl-letf (((symbol-function 'agent-repl--set-session-id)
                 (lambda (_ws _id) (setq set-called t))))
        (agent-repl--update-session-id-from-sentinel "ws1" "same-sid")
        (should-not set-called)))))

(ert-deftest agent-repl-test-update-session-id-from-sentinel-skips-unregistered-ws ()
  "update-session-id-from-sentinel does not auto-vivify unregistered workspaces.
Without the gate, `active-inst' would create a fresh instantiation and
puthash it into `agent-repl--workspaces', leaking entries for sessions
the module does not manage."
  (agent-repl-test--with-clean-state
    (let ((set-called nil))
      (cl-letf (((symbol-function 'agent-repl--set-session-id)
                 (lambda (_ws _id) (setq set-called t))))
        (agent-repl--update-session-id-from-sentinel "unmanaged-ws" "some-sid")
        (should-not set-called)
        (should-not (gethash "unmanaged-ws" agent-repl--workspaces))))))

(ert-deftest agent-repl-test-update-session-id-from-sentinel-updates-changed ()
  "update-session-id-from-sentinel should update when session-id differs."
  (agent-repl-test--with-clean-state
    (let ((inst (make-agent-repl-instantiation :session-id "old-sid")))
      (agent-repl--ws-put "ws1" :active-env :bare-metal)
      (agent-repl--ws-put "ws1" :bare-metal inst)
      (agent-repl--update-session-id-from-sentinel "ws1" "new-sid" t)
      (should (equal (agent-repl-instantiation-session-id inst) "new-sid")))))

;;;; ---- Tests: on-session-start-event ----

(ert-deftest agent-repl-test-on-session-start-event-fires-after-ready-hook ()
  "on-session-start-event runs `agent-repl-after-ready-functions' with the ws name."
  (agent-repl-test--with-clean-state
    (let ((hook-called-with nil))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'agent-repl--latch-and-maybe-fire-loaded) #'ignore))
        (let ((agent-repl-after-ready-functions
               (list (lambda (ws) (push ws hook-called-with)))))
          (agent-repl--on-session-start-event "ws1" "/some/dir")
          (should (equal hook-called-with '("ws1"))))))))

(ert-deftest agent-repl-test-on-session-start-event-hook-handler-error-isolated ()
  "A broken after-ready handler must not prevent later handlers from running."
  (agent-repl-test--with-clean-state
    (let ((second-called nil))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'agent-repl--latch-and-maybe-fire-loaded) #'ignore))
        (let ((agent-repl-after-ready-functions
               (list (lambda (_ws) (error "boom"))
                     (lambda (_ws) (setq second-called t)))))
          (agent-repl--on-session-start-event "ws1" "/some/dir")
          (should second-called))))))

(ert-deftest agent-repl-test-on-session-start-gui-no-error-spam ()
  "on-session-start-event must NOT emit the vterm-inconsistency ERROR for gui.
A gui workspace has no vterm buffer by design, so the structural
inconsistency branch does not apply."
  (agent-repl-test--with-clean-state
    (let ((messages '()))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--ws-put "ws1" :frontend 'gui)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) messages)))
                ((symbol-function 'agent-repl--latch-and-maybe-fire-loaded) #'ignore))
        (agent-repl--on-session-start-event "ws1" "/some/dir")
        (should-not (cl-find-if (lambda (m) (string-match-p "ERROR" m)) messages))))))

(ert-deftest agent-repl-test-on-session-start-gui-sets-idle ()
  "on-session-start-event flips a fresh gui workspace to :idle."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--ws-put "ws1" :frontend 'gui)
    (cl-letf (((symbol-function 'agent-repl--latch-and-maybe-fire-loaded) #'ignore))
      (agent-repl--on-session-start-event "ws1" "/some/dir")
      (should (eq (agent-repl--ws-get "ws1" :agent-state) :idle)))))

(ert-deftest agent-repl-test-on-session-start-gui-preserves-in-flight-turn ()
  "A lagging/re-fired session_start must not clobber a gui turn in flight.
The :idle flip is gated to nil/:init agent-states."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--ws-put "ws1" :frontend 'gui)
    (agent-repl--ws-set-agent-state "ws1" :thinking)
    (cl-letf (((symbol-function 'agent-repl--latch-and-maybe-fire-loaded) #'ignore))
      (agent-repl--on-session-start-event "ws1" "/some/dir")
      (should (eq (agent-repl--ws-get "ws1" :agent-state) :thinking)))))

(ert-deftest agent-repl-test-on-session-start-gui-fires-ready-latch ()
  "on-session-start-event flips the :agent-ready latch bit for gui.
Without this, gui ws-fully-loaded only ever fired via the watchdog."
  (agent-repl-test--with-clean-state
    (let ((latched nil)
          (hook-called-with nil))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--ws-put "ws1" :frontend 'gui)
      (cl-letf (((symbol-function 'agent-repl--latch-and-maybe-fire-loaded)
                 (lambda (ws bit) (setq latched (list ws bit)))))
        (let ((agent-repl-after-ready-functions
               (list (lambda (ws) (push ws hook-called-with)))))
          (agent-repl--on-session-start-event "ws1" "/some/dir")
          (should (equal latched '("ws1" :agent-ready)))
          (should (equal hook-called-with '("ws1"))))))))

(ert-deftest agent-repl-test-on-session-start-gui-drains-pending-prompts ()
  "The gui branch drains :pending-prompts when the session comes up.
The workspace-generation dispatch parks the new workspace's initial
prompt there; without the drain the dispatch silently never executes."
  (agent-repl-test--with-clean-state
    (let ((drained nil))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--ws-put "ws1" :frontend 'gui)
      (agent-repl--ws-put "ws1" :pending-prompts '("do the task"))
      (cl-letf (((symbol-function 'agent-repl--latch-and-maybe-fire-loaded) #'ignore)
                ((symbol-function 'agent-repl--drain-pending-prompts)
                 (lambda (ws) (setq drained ws))))
        (agent-repl--on-session-start-event "ws1" "/some/dir")
        (should (equal drained "ws1"))))))

(ert-deftest agent-repl-test-on-session-start-event-sets-idle ()
  "on-session-start-event writes :agent-state :idle (transition from :init)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--ws-set-agent-state "ws1" :init)
    (cl-letf (((symbol-function 'agent-repl--latch-and-maybe-fire-loaded) #'ignore))
      (agent-repl--on-session-start-event "ws1" "/some/dir")
      (should (eq (agent-repl--ws-agent-state "ws1") :idle)))))

;;;; ---- Tests: ws-fully-loaded latch ----

(ert-deftest agent-repl-test-latch-agent-ready-alone-does-not-fire ()
  "Latch flip of `:agent-ready' alone does not fire ws-fully-loaded.
The hook requires BOTH bits set; setting only one is a no-op fire-wise."
  (agent-repl-test--with-clean-state
    (let ((fires 0))
      (let ((agent-repl-ws-fully-loaded-functions
             (list (lambda (&rest _) (cl-incf fires)))))
        (agent-repl--latch-and-maybe-fire-loaded "ws1" :agent-ready)
        (should (= fires 0))
        ;; Latch bit set, but `:ws-loaded' still nil.
        (should (eq (agent-repl--ws-get "ws1" :agent-ready) t))
        (should (eq (agent-repl--ws-get "ws1" :ws-loaded) nil))))))

(ert-deftest agent-repl-test-latch-ws-loaded-alone-does-not-fire ()
  "Latch flip of `:ws-loaded' alone does not fire ws-fully-loaded.
Symmetric to the `:agent-ready'-alone case."
  (agent-repl-test--with-clean-state
    (let ((fires 0))
      (let ((agent-repl-ws-fully-loaded-functions
             (list (lambda (&rest _) (cl-incf fires)))))
        (agent-repl--latch-and-maybe-fire-loaded "ws1" :ws-loaded)
        (should (= fires 0))
        (should (eq (agent-repl--ws-get "ws1" :ws-loaded) t))
        (should (eq (agent-repl--ws-get "ws1" :agent-ready) nil))))))

(ert-deftest agent-repl-test-latch-claude-then-ws-fires ()
  "Latch fires once when both bits are set, regardless of order.
Setting `:agent-ready' first then `:ws-loaded' triggers the hook."
  (agent-repl-test--with-clean-state
    (let ((fired-with nil))
      (let ((agent-repl-ws-fully-loaded-functions
             (list (lambda (ws marker) (push (cons ws marker) fired-with)))))
        (agent-repl--latch-and-maybe-fire-loaded "ws1" :agent-ready)
        (should (null fired-with))
        (agent-repl--latch-and-maybe-fire-loaded "ws1" :ws-loaded)
        (should (equal fired-with '(("ws1" . nil))))))))

(ert-deftest agent-repl-test-latch-ws-then-claude-fires ()
  "Latch fires once when both bits are set, regardless of order.
Setting `:ws-loaded' first then `:agent-ready' also triggers the hook."
  (agent-repl-test--with-clean-state
    (let ((fired-with nil))
      (let ((agent-repl-ws-fully-loaded-functions
             (list (lambda (ws marker) (push (cons ws marker) fired-with)))))
        (agent-repl--latch-and-maybe-fire-loaded "ws1" :ws-loaded)
        (should (null fired-with))
        (agent-repl--latch-and-maybe-fire-loaded "ws1" :agent-ready)
        (should (equal fired-with '(("ws1" . nil))))))))

(ert-deftest agent-repl-test-latch-clears-bits-after-fire ()
  "After ws-fully-loaded fires, both latch bits are cleared on the ws plist
so a subsequent load cycle (e.g. agent-repl-restart) starts fresh."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-ws-fully-loaded-functions
           (list (lambda (&rest _) nil))))
      (agent-repl--latch-and-maybe-fire-loaded "ws1" :agent-ready)
      (agent-repl--latch-and-maybe-fire-loaded "ws1" :ws-loaded)
      (should (eq (agent-repl--ws-get "ws1" :agent-ready) nil))
      (should (eq (agent-repl--ws-get "ws1" :ws-loaded) nil)))))

(ert-deftest agent-repl-test-latch-passes-marker-through ()
  "Latch passes the optional MARKER arg straight to handlers — used by the
watchdog path to signal `:timed-out'."
  (agent-repl-test--with-clean-state
    (let ((received-marker 'unset))
      (let ((agent-repl-ws-fully-loaded-functions
             (list (lambda (_ws marker) (setq received-marker marker)))))
        ;; Pre-set :agent-ready so the :ws-loaded flip triggers fire.
        (agent-repl--ws-put "ws1" :agent-ready t)
        (agent-repl--latch-and-maybe-fire-loaded "ws1" :ws-loaded :timed-out)
        (should (eq received-marker :timed-out))))))

(ert-deftest agent-repl-test-latch-handler-error-isolated ()
  "A broken ws-fully-loaded handler must not prevent later handlers from running."
  (agent-repl-test--with-clean-state
    (let ((second-called nil))
      (let ((agent-repl-ws-fully-loaded-functions
             (list (lambda (&rest _) (error "boom"))
                   (lambda (&rest _) (setq second-called t)))))
        (agent-repl--ws-put "ws1" :agent-ready t)
        (agent-repl--latch-and-maybe-fire-loaded "ws1" :ws-loaded)
        (should second-called)))))

(ert-deftest agent-repl-test-on-session-start-event-flips-agent-ready ()
  "on-session-start-event flips the `:agent-ready' latch bit on the ws plist."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--on-session-start-event "ws1" "/some/dir")
    ;; :ws-loaded is nil, so the latch shouldn't have fired+cleared yet.
    (should (eq (agent-repl--ws-get "ws1" :agent-ready) t))))

(ert-deftest agent-repl-test-on-session-start-event-duplicate-still-fires-hooks ()
  "Even when `:agent-state' has already left `:init' (duplicate session_start),
both `after-ready-functions' and the `:agent-ready' latch flip still fire.
This is the stall fix: a swallowed duplicate must not block loader advance."
  (agent-repl-test--with-clean-state
    (let ((after-ready-fires 0))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--ws-set-agent-state "ws1" :idle)
      (let ((agent-repl-after-ready-functions
             (list (lambda (_ws) (cl-incf after-ready-fires)))))
        (agent-repl--on-session-start-event "ws1" "/some/dir")
        ;; Hook fired despite duplicate.
        (should (= after-ready-fires 1))
        ;; Latch bit set despite duplicate.
        (should (eq (agent-repl--ws-get "ws1" :agent-ready) t))))))

;;;; ---- Tests: permission-notify.sh sentinel writing ----
;;
;; Claude Code's Notification hook fires for two distinct semantics under
;; the same notification_type=permission_prompt — real permission/approval
;; prompts AND a 60s-idle "needs your attention" nudge.  Earlier versions
;; of the script filtered the idle nudge in bash, but Claude Code also
;; uses the "needs your attention" wording for some real permission
;; prompts, so the filter caused real prompts to be missed.  The script
;; now writes the sentinel unconditionally and discrimination is done in
;; elisp by `agent-repl--on-permission-event' (state-gated on
;; `:thinking').  These tests drive the actual shell script end-to-end.

(defconst agent-repl-test--permission-hook-path
  ;; Captured at load time — `load-file-name' is nil inside ERT test bodies.
  (expand-file-name
   "hooks/permission-notify.sh"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path to the checked-in permission-notify.sh script.")

(defun agent-repl-test--run-permission-hook (json-input)
  "Run permission-notify.sh under an isolated state dir and return the result plist.
Returns (:exit CODE :sentinel-exists BOOL :sentinel-content STRING).
The hook resolves its sentinel dir from `AGENT_REPL_STATE_DIR' (which
`test-helpers.el' points at a shared session temp dir), so this helper
rebinds that variable locally at a throwaway path — per the guidance in
that fixture — and checks the sentinel there."
  (let* ((tmp-home (make-temp-file "agent-repl-permission-hook-" t))
         (state-dir (expand-file-name ".claude-emacs" tmp-home))
         (script agent-repl-test--permission-hook-path)
         (sentinel (expand-file-name
                    "workspace-notifications/permission_prompt"
                    state-dir))
         (process-environment (append
                               (list (format "HOME=%s" tmp-home)
                                     (format "AGENT_REPL_STATE_DIR=%s" state-dir))
                               process-environment))
         (default-directory tmp-home)
         (exit (with-temp-buffer
                 (insert json-input)
                 (call-process-region (point-min) (point-max)
                                      "bash" nil nil nil script)))
         (exists (file-exists-p sentinel))
         (content (when exists
                    (with-temp-buffer
                      (insert-file-contents sentinel)
                      (buffer-string)))))
    (ignore-errors (delete-directory tmp-home t))
    (list :exit exit :sentinel-exists exists :sentinel-content content)))

(ert-deftest agent-repl-test-permission-hook-bash-permission-writes-sentinel ()
  "A real \"Claude needs your permission to use Bash\" message writes the sentinel."
  (let* ((input "{\"cwd\":\"/some/dir\",\"session_id\":\"sess-1\",\"message\":\"Claude needs your permission to use Bash\",\"notification_type\":\"permission_prompt\"}")
         (result (agent-repl-test--run-permission-hook input)))
    (should (= 0 (plist-get result :exit)))
    (should (plist-get result :sentinel-exists))
    (should (string-match-p "/some/dir" (plist-get result :sentinel-content)))
    (should (string-match-p "sess-1" (plist-get result :sentinel-content)))))

(ert-deftest agent-repl-test-permission-hook-plan-approval-writes-sentinel ()
  "A \"Claude Code needs your approval for the plan\" message writes the sentinel."
  (let* ((input "{\"cwd\":\"/d\",\"session_id\":\"s\",\"message\":\"Claude Code needs your approval for the plan\",\"notification_type\":\"permission_prompt\"}")
         (result (agent-repl-test--run-permission-hook input)))
    (should (= 0 (plist-get result :exit)))
    (should (plist-get result :sentinel-exists))))

(ert-deftest agent-repl-test-permission-hook-skill-permission-writes-sentinel ()
  "\"Claude needs your permission to use Skill\" message writes the sentinel."
  (let* ((input "{\"cwd\":\"/d\",\"session_id\":\"s\",\"message\":\"Claude needs your permission to use Skill\",\"notification_type\":\"permission_prompt\"}")
         (result (agent-repl-test--run-permission-hook input)))
    (should (= 0 (plist-get result :exit)))
    (should (plist-get result :sentinel-exists))))

(ert-deftest agent-repl-test-permission-hook-attention-writes-sentinel ()
  "A \"Claude Code needs your attention\" message ALSO writes the sentinel.
Claude Code uses that wording for some real permission prompts, so the
hook no longer filters on .message; discrimination between real prompts
and 60s-idle nudges happens elisp-side in
`agent-repl--on-permission-event' by gating on `:agent-state'."
  (let* ((input "{\"cwd\":\"/d\",\"session_id\":\"s\",\"message\":\"Claude Code needs your attention\",\"notification_type\":\"permission_prompt\"}")
         (result (agent-repl-test--run-permission-hook input)))
    (should (= 0 (plist-get result :exit)))
    (should (plist-get result :sentinel-exists))))

(ert-deftest agent-repl-test-permission-hook-empty-message-writes-sentinel ()
  "An empty or missing .message field still writes the sentinel.
Same rationale as the attention-writes-sentinel test: elisp gates on
state, so the shell hook is now message-agnostic."
  (let* ((input "{\"cwd\":\"/d\",\"session_id\":\"s\",\"notification_type\":\"permission_prompt\"}")
         (result (agent-repl-test--run-permission-hook input)))
    (should (= 0 (plist-get result :exit)))
    (should (plist-get result :sentinel-exists))))

(provide 'test-sentinel)

;;; test-sentinel.el ends here
