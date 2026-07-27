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

;; The permission_request / permission_prompt / permission_resolved /
;; session_dead_ / account_changed_ DISPATCH-to-handler tests were deleted
;; in the S8/S9 sentinel endgame: those handlers are gone and the dispatch
;; alist is empty.  Each retired prefix now DRAINS — asserted per-channel
;; below (mirroring `agent-repl-test-sentinel-drains-retired-status-prefix'
;; for the status prefixes).  The account-changed handler tests
;; (`agent-repl--on-account-changed-event') went with the handler: Emacs is
;; out of account switching entirely.

(defun agent-repl-test--assert-drains (file)
  "Assert that dispatching FILE drains it: returns t, deletes it, no warn/process."
  (let ((deleted nil)
        (processed nil))
    (cl-letf (((symbol-function 'delete-file)
               (lambda (f) (setq deleted f)))
              ((symbol-function 'agent-repl--process-sentinel-file)
               (lambda (&rest _) (setq processed t)))
              ((symbol-function 'agent-repl--warn)
               (lambda (&rest _) (ert-fail "retired prefix must not warn"))))
      (should (eq t (agent-repl--dispatch-sentinel-file file)))
      (should (equal deleted file))
      (should-not processed))))

(ert-deftest agent-repl-test-sentinel-drains-permission-prompt ()
  "A retired permission_prompt file is drained, not dispatched or warned."
  (agent-repl-test--with-clean-state
    (agent-repl-test--assert-drains "/dir/permission_prompt")))

(ert-deftest agent-repl-test-sentinel-drains-permission-request ()
  "A retired permission_request_<sid>_<reqid> file is drained."
  (agent-repl-test--with-clean-state
    (agent-repl-test--assert-drains "/dir/permission_request_abc123_req42")))

(ert-deftest agent-repl-test-sentinel-drains-permission-resolved ()
  "A retired permission_resolved_<sid>_<reqid> file is drained."
  (agent-repl-test--with-clean-state
    (agent-repl-test--assert-drains "/dir/permission_resolved_abc123_req42")))

(ert-deftest agent-repl-test-sentinel-drains-session-dead ()
  "A retired session_dead_<sid> file is drained (death is pushed DEAD state now)."
  (agent-repl-test--with-clean-state
    (agent-repl-test--assert-drains "/dir/session_dead_abc123")))

(ert-deftest agent-repl-test-sentinel-drains-account-changed ()
  "A retired account_changed_<sid> file is drained (Emacs is out of account switching)."
  (agent-repl-test--with-clean-state
    (agent-repl-test--assert-drains "/dir/account_changed_abc123")))

(ert-deftest agent-repl-test-sentinel-dispatch-returns-nil-for-unknown ()
  "dispatch-sentinel-file should return nil for an unrecognized filename."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--dispatch-sentinel-file "/dir/unknown_file"))))

(ert-deftest agent-repl-test-sentinel-drains-retired-status-prefix ()
  "A retired STATUS-hook sentinel (stop_/subagent_/prompt_submit_/session_start_)
is DRAINED, not dispatched to a handler and not warned about — the
agent-shim cutover deleted those handlers, and a stale hook install
could still emit the files."
  (agent-repl-test--with-clean-state
    (let ((deleted nil)
          (processed nil))
      (cl-letf (((symbol-function 'delete-file)
                 (lambda (f) (setq deleted f)))
                ((symbol-function 'agent-repl--process-sentinel-file)
                 (lambda (&rest _) (setq processed t)))
                ((symbol-function 'agent-repl--warn)
                 (lambda (&rest _) (ert-fail "retired status prefix must not warn"))))
        (should (eq t (agent-repl--dispatch-sentinel-file "/dir/stop_12345")))
        (should (equal deleted "/dir/stop_12345"))
        (should-not processed)))))

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

;; The three-argument (source-threading) callback path was deleted in the
;; agent-shim cutover (design §10): the only 3-arg handler was
;; `--on-session-start-event'.  Every surviving handler takes (ws dir),
;; verified by the test below.

;; The status-handler tests (on-stop-event / on-subagent-start-event /
;; on-subagent-stop-event / on-stop-failure-event / on-prompt-submit-event
;; and the stop_/subagent_/prompt_submit_ dispatch-routing tests) were
;; DELETED in the agent-shim cutover (design §10): those handlers and the
;; `:pending-subagents' / `:stop-received' machinery they drove no longer
;; exist.  Turn-finished / subagent-in-flight resolution is owned by the
;; daemon's SSM and pushed as `frontend.v1' WorkspaceState frames now.

;; The on-permission-event / on-permission-resolved-event /
;; on-session-dead-event handler tests were deleted in the S8/S9 sentinel
;; endgame along with their handlers.  The permission UX is now covered by
;; test-permission.el (pushed PermissionItem present/clear/notify/answer);
;; session-death parity (pushed DEAD -> mark-dead) is covered by
;; `agent-repl-test-react-to-pushed-death-*' in test-status.el.

;;;; ---- Tests: read-sentinel-file ----

;;;; ---- Tests: update-session-id-from-sentinel ownership gate ----

;;;; ---- Tests: session-start staging vs activity promotion ----

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

(ert-deftest agent-repl-test-ws-for-dir-fast-uses-registered-name-boundary ()
  "ws-for-dir-fast obtains its complete candidate set from workspace.el.
The registered-name API intentionally includes tombstones, preserving the
old raw-hash iteration semantics for a sole matching historical entry."
  (agent-repl-test--with-clean-state
    (let ((root (agent-repl--path-canonical "/home/user/project"))
          (registered-names-called nil))
      (agent-repl--ws-put "tombstoned-ws" :project-dir root)
      (agent-repl--ws-put "tombstoned-ws" :nuked-at t)
      (cl-letf (((symbol-function 'agent-repl--git-root)
                 (lambda (_d) root))
                ((symbol-function 'agent-repl--ws-registered-names)
                 (lambda ()
                   (setq registered-names-called t)
                   '("tombstoned-ws"))))
        (should (equal (agent-repl--ws-for-dir-fast (concat root "/subdir"))
                       "tombstoned-ws"))
        (should registered-names-called)))))

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

;;;; ---- Tests: end-to-end dispatch through process-sentinel-file ----

;; The end-to-end permission / stop_ / prompt_submit_ dispatch tests were
;; deleted in the sentinel endgame (design §10, S8/S9): every prefix drains
;; now — there is no handler to reach end-to-end.  Drain coverage lives in
;; the per-channel `agent-repl-test-sentinel-drains-*' tests above.

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

;;;; ---- Tests: process-sentinel-file uncovered edge cases ----

;; The on-permission-event / on-stop-event / on-prompt-submit-event
;; edge-case tests were deleted in the sentinel endgame (design §10): those
;; handlers are gone.  Permission-state gating now lives in the daemon's SSM
;; (pushed :permission WorkspaceState); the Emacs-side permission UX is
;; covered by test-permission.el.

;;;; ---- Tests: dispatch-sentinel-file uncovered edge cases ----

(ert-deftest agent-repl-test-dispatch-sentinel-file-no-directory-component ()
  "dispatch-sentinel-file drains a bare (no-directory) retired filename."
  (agent-repl-test--with-clean-state
    (agent-repl-test--assert-drains "session_dead_123")))

(ert-deftest agent-repl-test-dispatch-sentinel-file-exact-prefix ()
  "dispatch-sentinel-file drains a filename that is exactly a retired prefix (no suffix)."
  (agent-repl-test--with-clean-state
    ;; "session_dead_" is a retired drain prefix; the file is exactly it.
    (agent-repl-test--assert-drains "/dir/session_dead_")))

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
    (let ((dispatched-files nil))
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

;; The on-permission-event status-transition edge-case tests (leaves-:done,
;; overwrites-:thinking) were deleted in the sentinel endgame: permission
;; state gating moved to the daemon SSM (pushed :permission WorkspaceState).

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

;; The on-session-start-event tests were DELETED in the agent-shim
;; cutover (design §10): the handler is gone (the SessionStart managed
;; hook is removed and the daemon owns session-ready reporting / prompt
;; submission / metaprompt re-fire).  The ws-fully-loaded LATCH itself
;; (`agent-repl--latch-and-maybe-fire-loaded') is retained — its
;; `:ws-loaded' callers (commands.el / panels.el) still drive it — so its
;; tests remain below.

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

(provide 'test-sentinel)

;;; test-sentinel.el ends here
