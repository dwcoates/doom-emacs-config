;;; test-session.el --- Tests for session.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for session lifecycle management: sandbox config, command building,
;; session startup, completion handling, session ID management, readiness
;; handling, process state predicates, and the ready timer.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Migrated tests ----

(ert-deftest claude-repl-test-finished-from-hook-hidden-sets-done ()
  "handle-claude-finished sets claude-state :done when vterm buffer is not visible."
  (claude-repl-test--with-clean-state
    (let ((done-set nil)
          (fake-buf (generate-new-buffer " *test-hook-hidden*")))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws1" :vterm-buffer fake-buf)
            (cl-letf (((symbol-function 'get-buffer-window)
                       (lambda (_buf &rest _) nil))
                      ((symbol-function 'claude-repl--do-refresh) #'ignore)
                      ((symbol-function 'claude-repl--update-hide-overlay) #'ignore)
                      ((symbol-function 'claude-repl--maybe-notify-finished) #'ignore)
                      ((symbol-function 'claude-repl--ws-set-claude-state)
                       (lambda (ws state)
                         (when (eq state :done) (setq done-set ws)))))
              (claude-repl--handle-claude-finished "ws1")
              (should (equal done-set "ws1"))))
        (kill-buffer fake-buf)))))

(ert-deftest claude-repl-test-finished-from-hook-visible-also-sets-done ()
  "handle-claude-finished sets :done regardless of vterm visibility.
Visibility is no longer a gate; the renderer decides the display via
the composed-state rule with :repl-state."
  (claude-repl-test--with-clean-state
    (let ((done-set nil)
          (fake-buf (generate-new-buffer " *test-hook-visible*")))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws1" :vterm-buffer fake-buf)
            (cl-letf (((symbol-function 'get-buffer-window)
                       (lambda (_buf &rest _) (selected-window)))
                      ((symbol-function 'claude-repl--do-refresh) #'ignore)
                      ((symbol-function 'claude-repl--update-hide-overlay) #'ignore)
                      ((symbol-function 'claude-repl--fix-vterm-scroll) #'ignore)
                      ((symbol-function 'claude-repl--maybe-notify-finished) #'ignore)
                      ((symbol-function 'claude-repl--ws-set-claude-state)
                       (lambda (ws state)
                         (when (eq state :done) (setq done-set ws)))))
              (claude-repl--handle-claude-finished "ws1")
              (should (equal done-set "ws1"))))
        (kill-buffer fake-buf)))))

(ert-deftest claude-repl-test-maybe-notify-debounce ()
  "maybe-notify-finished should debounce within 2 seconds."
  (claude-repl-test--with-clean-state
    (let ((notify-count 0))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda () nil))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat fn &rest args)
                   (cl-incf notify-count))))
        ;; First call should notify
        (claude-repl--maybe-notify-finished "ws1")
        (should (= notify-count 1))
        ;; Second call within 2s window should be suppressed
        (claude-repl--maybe-notify-finished "ws1")
        (should (= notify-count 1))
        ;; Simulate time passing beyond debounce window
        (claude-repl--ws-put "ws1" :last-notify-time (- (float-time) 3.0))
        (claude-repl--maybe-notify-finished "ws1")
        (should (= notify-count 2))))))

(ert-deftest claude-repl-test-maybe-notify-skips-when-focused ()
  "maybe-notify-finished should NOT send desktop notification when frame is focused."
  (claude-repl-test--with-clean-state
    (let ((notify-count 0))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda () t))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat fn &rest args)
                   (cl-incf notify-count))))
        (claude-repl--maybe-notify-finished "ws1")
        (should (= notify-count 0))))))

(ert-deftest claude-repl-test-finished-from-hook-nil-vterm-sets-done ()
  "handle-claude-finished still sets :done when vterm-buf is nil.
The Stop signal's intent is \"Claude finished\" — unrelated to whether
the vterm buffer is still around.  Refresh-vterm-after-finish is guarded
by vterm-buf presence; the :done write is not."
  (claude-repl-test--with-clean-state
    (let ((done-set nil))
      ;; Register ws1 (required by handle-claude-finished guard) but
      ;; do NOT set :vterm-buffer.
      (claude-repl--ws-put "ws1" :project-dir "/tmp/fake")
      (cl-letf (((symbol-function 'claude-repl--do-refresh) #'ignore)
                ((symbol-function 'claude-repl--update-hide-overlay) #'ignore)
                ((symbol-function 'claude-repl--maybe-notify-finished) #'ignore)
                ((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'claude-repl--ws-set-claude-state)
                 (lambda (ws state)
                   (when (eq state :done) (setq done-set ws)))))
        (claude-repl--handle-claude-finished "ws1")
        (should (equal done-set "ws1"))))))

;;;; ---- Tests: Deferred prompt drain ----

(ert-deftest claude-repl-test-drain-deferred-empty-queue-noop ()
  "`claude-repl--drain-deferred-prompts' on an empty queue does nothing."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :claude-state :done)
    (claude-repl--ws-put "ws1" :deferred-prompts nil)
    (let ((sent nil))
      (cl-letf (((symbol-function 'claude-repl--send)
                 (lambda (&rest args) (push args sent))))
        (claude-repl--drain-deferred-prompts "ws1")
        (should (null sent))))))

(ert-deftest claude-repl-test-drain-deferred-pops-and-sends-when-done ()
  "Drain pops the head and sends it via `claude-repl--send' when state is `:done'."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :claude-state :done)
    (claude-repl--ws-put "ws1" :deferred-prompts '("alpha" "beta" "gamma"))
    (let ((sent nil))
      (cl-letf (((symbol-function 'claude-repl--send)
                 (lambda (prompt ws &rest _) (setq sent (list prompt ws)))))
        (claude-repl--drain-deferred-prompts "ws1")
        (should (equal sent '("alpha" "ws1")))
        (should (equal (claude-repl--ws-get "ws1" :deferred-prompts)
                       '("beta" "gamma")))))))

(ert-deftest claude-repl-test-drain-deferred-pops-and-sends-when-idle ()
  "Drain also fires when state is `:idle' (decayed from `:done')."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :claude-state :idle)
    (claude-repl--ws-put "ws1" :deferred-prompts '("only-one"))
    (let ((sent nil))
      (cl-letf (((symbol-function 'claude-repl--send)
                 (lambda (prompt ws &rest _) (setq sent (list prompt ws)))))
        (claude-repl--drain-deferred-prompts "ws1")
        (should (equal sent '("only-one" "ws1")))
        (should (null (claude-repl--ws-get "ws1" :deferred-prompts)))))))

(ert-deftest claude-repl-test-drain-deferred-skipped-when-thinking ()
  "Drain does NOT pop or send when state is `:thinking', even with a non-empty queue."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :claude-state :thinking)
    (claude-repl--ws-put "ws1" :deferred-prompts '("hold-me"))
    (let ((sent nil))
      (cl-letf (((symbol-function 'claude-repl--send)
                 (lambda (&rest args) (push args sent))))
        (claude-repl--drain-deferred-prompts "ws1")
        (should (null sent))
        (should (equal (claude-repl--ws-get "ws1" :deferred-prompts)
                       '("hold-me")))))))

(ert-deftest claude-repl-test-drain-deferred-skipped-when-permission ()
  "Drain does NOT fire while Claude is at a `:permission' prompt."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :claude-state :permission)
    (claude-repl--ws-put "ws1" :deferred-prompts '("hold-me"))
    (let ((sent nil))
      (cl-letf (((symbol-function 'claude-repl--send)
                 (lambda (&rest args) (push args sent))))
        (claude-repl--drain-deferred-prompts "ws1")
        (should (null sent))
        (should (equal (claude-repl--ws-get "ws1" :deferred-prompts)
                       '("hold-me")))))))

(ert-deftest claude-repl-test-drain-deferred-skipped-when-init ()
  "Drain does NOT fire while Claude is still initializing (`:init')."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :claude-state :init)
    (claude-repl--ws-put "ws1" :deferred-prompts '("hold-me"))
    (let ((sent nil))
      (cl-letf (((symbol-function 'claude-repl--send)
                 (lambda (&rest args) (push args sent))))
        (claude-repl--drain-deferred-prompts "ws1")
        (should (null sent))))))

(ert-deftest claude-repl-test-handle-claude-finished-drains-deferred ()
  "`claude-repl--handle-claude-finished' drains the deferred queue at the end."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/fake")
    (claude-repl--ws-put "ws1" :deferred-prompts '("first-deferred" "second"))
    (let ((sent nil))
      (cl-letf (((symbol-function 'claude-repl--do-refresh) #'ignore)
                ((symbol-function 'claude-repl--update-hide-overlay) #'ignore)
                ((symbol-function 'claude-repl--maybe-notify-finished) #'ignore)
                ((symbol-function 'claude-repl--refresh-magit-status) #'ignore)
                ((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'claude-repl--send)
                 (lambda (prompt ws &rest _) (setq sent (list prompt ws)))))
        ;; handle-claude-finished marks state :done first, then drains.
        (claude-repl--handle-claude-finished "ws1")
        (should (equal sent '("first-deferred" "ws1")))
        ;; One drained, one remains for the next turn.
        (should (equal (claude-repl--ws-get "ws1" :deferred-prompts)
                       '("second")))))))

(ert-deftest claude-repl-test-handle-claude-finished-no-deferred-noop ()
  "`handle-claude-finished' with an empty deferred queue does not call `--send'."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/fake")
    (claude-repl--ws-put "ws1" :deferred-prompts nil)
    (let ((sent nil))
      (cl-letf (((symbol-function 'claude-repl--do-refresh) #'ignore)
                ((symbol-function 'claude-repl--update-hide-overlay) #'ignore)
                ((symbol-function 'claude-repl--maybe-notify-finished) #'ignore)
                ((symbol-function 'claude-repl--refresh-magit-status) #'ignore)
                ((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'claude-repl--send)
                 (lambda (&rest args) (push args sent))))
        (claude-repl--handle-claude-finished "ws1")
        (should (null sent))))))

;;;; ---- Tests: Sandbox configuration ----

(ert-deftest claude-repl-test-find-sandbox-script-prefers-path ()
  "find-sandbox-script should prefer executable on PATH over repo script."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (name)
               (when (equal name "claude-sandbox") "/usr/bin/claude-sandbox")))
            ((symbol-function 'file-executable-p) (lambda (_f) t)))
    (should (equal (claude-repl--find-sandbox-script "/repo")
                   "/usr/bin/claude-sandbox"))))

(ert-deftest claude-repl-test-find-sandbox-script-falls-back-to-repo ()
  "find-sandbox-script should fall back to .agents-sandbox/sandbox in repo."
  (cl-letf (((symbol-function 'executable-find) (lambda (_n) nil))
            ((symbol-function 'file-executable-p)
             (lambda (f)
               (string-suffix-p ".agents-sandbox/sandbox" f))))
    (should (equal (claude-repl--find-sandbox-script "/my/repo")
                   "/my/repo/.agents-sandbox/sandbox"))))

(ert-deftest claude-repl-test-find-sandbox-script-returns-nil ()
  "find-sandbox-script should return nil when no launcher exists."
  (cl-letf (((symbol-function 'executable-find) (lambda (_n) nil))
            ((symbol-function 'file-executable-p) (lambda (_f) nil)))
    (should-not (claude-repl--find-sandbox-script "/my/repo"))))

(ert-deftest claude-repl-test-query-sandbox-image-returns-trimmed ()
  "query-sandbox-image should return trimmed output from script."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_script _infile destination _display &rest args)
               ;; Real call-process with destination=t inserts into current buffer
               (when (and (eq destination t) (equal args '("--image-name")))
                 (insert "  my-image:latest  "))
               0)))
    (should (equal (claude-repl--query-sandbox-image "/path/to/script")
                   "my-image:latest"))))

(ert-deftest claude-repl-test-query-sandbox-image-empty-returns-nil ()
  "query-sandbox-image should return nil when script outputs empty string."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_script _infile _dest _display &rest _args) 0)))
    (should-not (claude-repl--query-sandbox-image "/path/to/script"))))

(ert-deftest claude-repl-test-find-install-script-exists ()
  "find-install-script should return path when install script is executable."
  (cl-letf (((symbol-function 'file-executable-p)
             (lambda (f) (string-suffix-p "install-claude.sh" f))))
    (should (equal (claude-repl--find-install-script "/repo")
                   "/repo/.agents-sandbox/install-claude.sh"))))

(ert-deftest claude-repl-test-find-install-script-missing ()
  "find-install-script should return nil when install script not found."
  (cl-letf (((symbol-function 'file-executable-p) (lambda (_f) nil)))
    (should-not (claude-repl--find-install-script "/repo"))))

(ert-deftest claude-repl-test-docker-image-exists-p-true ()
  "docker-image-exists-p should return non-nil when docker inspect returns 0."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_prog _infile _dest _display &rest _args) 0)))
    (should (claude-repl--docker-image-exists-p "my-image:latest"))))

(ert-deftest claude-repl-test-docker-image-exists-p-false ()
  "docker-image-exists-p should return nil when docker inspect returns non-zero."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_prog _infile _dest _display &rest _args) 1)))
    (should-not (claude-repl--docker-image-exists-p "no-such-image"))))

(ert-deftest claude-repl-test-resolve-sandbox-config-no-launcher ()
  "resolve-sandbox-config should return nil when no launcher script is found."
  (cl-letf (((symbol-function 'claude-repl--find-sandbox-script) (lambda (_r) nil)))
    (should-not (claude-repl--resolve-sandbox-config "/repo"))))

(ert-deftest claude-repl-test-resolve-sandbox-config-image-ready ()
  "resolve-sandbox-config should return (:image IMAGE :script SCRIPT) when image exists."
  (cl-letf (((symbol-function 'claude-repl--find-sandbox-script)
             (lambda (_r) "/usr/bin/claude-sandbox"))
            ((symbol-function 'claude-repl--query-sandbox-image)
             (lambda (_s) "my-image:latest"))
            ((symbol-function 'claude-repl--docker-image-exists-p)
             (lambda (_i) t)))
    (let ((result (claude-repl--resolve-sandbox-config "/repo")))
      (should (equal (plist-get result :image) "my-image:latest"))
      (should (equal (plist-get result :script) "/usr/bin/claude-sandbox"))
      (should-not (plist-get result :needs-build)))))

(ert-deftest claude-repl-test-resolve-sandbox-config-needs-build ()
  "resolve-sandbox-config should return :needs-build when image does not exist."
  (cl-letf (((symbol-function 'claude-repl--find-sandbox-script)
             (lambda (_r) "/usr/bin/claude-sandbox"))
            ((symbol-function 'claude-repl--query-sandbox-image)
             (lambda (_s) "my-image:latest"))
            ((symbol-function 'claude-repl--docker-image-exists-p)
             (lambda (_i) nil))
            ((symbol-function 'claude-repl--find-install-script)
             (lambda (_r) "/repo/.agents-sandbox/install-claude.sh")))
    (let ((result (claude-repl--resolve-sandbox-config "/repo")))
      (should (plist-get result :needs-build))
      (should (equal (plist-get result :image) "my-image:latest"))
      (should (equal (plist-get result :install-script)
                     "/repo/.agents-sandbox/install-claude.sh")))))

(ert-deftest claude-repl-test-resolve-sandbox-config-empty-image ()
  "resolve-sandbox-config should return nil when launcher outputs empty image."
  (cl-letf (((symbol-function 'claude-repl--find-sandbox-script)
             (lambda (_r) "/usr/bin/claude-sandbox"))
            ((symbol-function 'claude-repl--query-sandbox-image)
             (lambda (_s) nil)))
    (should-not (claude-repl--resolve-sandbox-config "/repo"))))

;;;; ---- Tests: Command building ----

(ert-deftest claude-repl-test-compute-claude-flags-continue ()
  "compute-claude-flags should emit --continue when session-id is set and no fork."
  (should (equal (claude-repl--compute-claude-flags "abc123" nil nil)
                 "--continue")))

(ert-deftest claude-repl-test-compute-claude-flags-no-continue-without-session-id ()
  "compute-claude-flags should not emit --continue when session-id is nil."
  (should (equal (claude-repl--compute-claude-flags nil nil nil) "")))

(ert-deftest claude-repl-test-compute-claude-flags-fork ()
  "compute-claude-flags should emit --resume <id> --fork-session for forks."
  (should (equal (claude-repl--compute-claude-flags "current" "fork-id" nil)
                 "--resume fork-id --fork-session")))

(ert-deftest claude-repl-test-compute-claude-flags-fork-ignores-session ()
  "compute-claude-flags with fork should not also emit --continue for session-id."
  (let ((result (claude-repl--compute-claude-flags "current" "fork-id" nil)))
    (should (string-match-p "--resume fork-id --fork-session" result))
    (should-not (string-match-p "--continue" result))))

(ert-deftest claude-repl-test-compute-claude-flags-perm-flag ()
  "compute-claude-flags should include permission flag when provided."
  (should (equal (claude-repl--compute-claude-flags nil nil "--permission-mode auto")
                 "--permission-mode auto")))

(ert-deftest claude-repl-test-compute-claude-flags-all-nil ()
  "compute-claude-flags should return empty string when all args are nil."
  (should (equal (claude-repl--compute-claude-flags nil nil nil) "")))

(ert-deftest claude-repl-test-compute-claude-flags-continue-plus-perm ()
  "compute-claude-flags should combine --continue and perm flag."
  (should (equal (claude-repl--compute-claude-flags "sess1" nil "--dangerously-skip-permissions")
                 "--continue --dangerously-skip-permissions")))

(ert-deftest claude-repl-test-compute-perm-flag-sandboxed ()
  "compute-perm-flag should return nil when sandboxed."
  (should-not (claude-repl--compute-perm-flag t "/some/dir")))

(ert-deftest claude-repl-test-compute-perm-flag-chesscom ()
  "compute-perm-flag should return --permission-mode auto for ChessCom repos."
  (should (equal (claude-repl--compute-perm-flag nil "/home/user/ChessCom/project")
                 "--permission-mode auto")))

(ert-deftest claude-repl-test-compute-perm-flag-personal ()
  "compute-perm-flag should return --dangerously-skip-permissions for personal repos."
  (should (equal (claude-repl--compute-perm-flag nil "/home/user/personal/project")
                 "--dangerously-skip-permissions")))

(ert-deftest claude-repl-test-compute-perm-flag-nil-dir ()
  "compute-perm-flag with nil project-dir should signal an error."
  (should-error (claude-repl--compute-perm-flag nil nil) :type 'error))

(ert-deftest claude-repl-test-compute-perm-flag-chesscom-dir ()
  "compute-perm-flag with ChessCom project-dir returns auto permissions."
  (should (equal (claude-repl--compute-perm-flag nil "/tmp/ChessCom/test")
                 "--permission-mode auto")))

(ert-deftest claude-repl-test-assemble-cmd-sandboxed ()
  "assemble-cmd should use sandbox script when sandboxed."
  (let ((config (list :script "/usr/bin/claude-sandbox" :image "img")))
    (should (equal (claude-repl--assemble-cmd config t "--resume abc")
                   "/usr/bin/claude-sandbox --resume abc"))))

(ert-deftest claude-repl-test-assemble-cmd-bare-metal ()
  "assemble-cmd should use 'claude' when not sandboxed."
  (should (equal (claude-repl--assemble-cmd nil nil "--resume abc")
                 "claude --resume abc")))

(ert-deftest claude-repl-test-assemble-cmd-no-flags ()
  "assemble-cmd with empty flags should produce clean command."
  (should (equal (claude-repl--assemble-cmd nil nil "") "claude")))

;;;; ---- Tests: Workspace mode-line ----

(ert-deftest claude-repl-test-workspace-mode-line-with-parent-no-merge ()
  "Parent segment shows just the parent name when no merge target is resolvable."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "child-ws" :source-ws-dir "/tmp/parent-worktrees/feature-foo")
    (cl-letf (((symbol-function 'claude-repl--merge-target-name) (lambda (_ws) nil)))
      (let ((result (claude-repl--workspace-mode-line "child-ws")))
        (should (listp result))
        (should (string-match-p " feature-foo\\'" (car result)))
        (should-not (string-match-p "PARENT" (car result)))))))

(ert-deftest claude-repl-test-workspace-mode-line-without-parent ()
  "First segment is empty when there is neither parent nor merge target."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--merge-target-name) (lambda (_ws) nil)))
      (let ((result (claude-repl--workspace-mode-line "ws-no-parent")))
        (should (listp result))
        (should (equal (car result) ""))))))

(ert-deftest claude-repl-test-workspace-mode-line-empty-source-dir-treated-as-no-parent ()
  "Empty-string :source-ws-dir is treated the same as nil for parent resolution."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws-blank" :source-ws-dir "")
    (cl-letf (((symbol-function 'claude-repl--merge-target-name) (lambda (_ws) nil)))
      (let ((result (claude-repl--workspace-mode-line "ws-blank")))
        (should (equal (car result) ""))))))

(ert-deftest claude-repl-test-workspace-mode-line-keeps-prompt-summary-segment ()
  "Mode-line list has 2 elements; the trailing :eval prompt-summary segment is preserved."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--merge-target-name) (lambda (_ws) nil)))
      (let ((result (claude-repl--workspace-mode-line "ws")))
        (should (= (length result) 2))
        (should (eq (car (car (last result))) :eval))))))

(ert-deftest claude-repl-test-workspace-mode-line-strips-trailing-slash ()
  "A trailing slash on :source-ws-dir does not leak into the parent name."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :source-ws-dir "/tmp/parent-worktrees/feature-bar/")
    (cl-letf (((symbol-function 'claude-repl--merge-target-name) (lambda (_ws) nil)))
      (let ((result (claude-repl--workspace-mode-line "ws")))
        (should (string-match-p " feature-bar\\'" (car result)))))))

(ert-deftest claude-repl-test-workspace-mode-line-merge-shown-in-parens-when-different ()
  "When merge target differs from parent, it appears in parens after the parent."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :source-ws-dir "/tmp/parent-worktrees/feature-foo")
    (cl-letf (((symbol-function 'claude-repl--merge-target-name)
               (lambda (_ws) "explanation-engine")))
      (let ((result (claude-repl--workspace-mode-line "ws")))
        (should (string-match-p " feature-foo (explanation-engine)" (car result)))))))

(ert-deftest claude-repl-test-workspace-mode-line-merge-omitted-when-equal-to-parent ()
  "When merge target equals parent, no parens are appended (avoids redundant ` foo (foo)`)."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :source-ws-dir "/tmp/parent-worktrees/feature-foo")
    (cl-letf (((symbol-function 'claude-repl--merge-target-name)
               (lambda (_ws) "feature-foo")))
      (let ((result (claude-repl--workspace-mode-line "ws")))
        (should (string-match-p " feature-foo\\'" (car result)))
        (should-not (string-match-p "(feature-foo)" (car result)))))))

;;;; ---- Tests: parent-label ----

(ert-deftest claude-repl-test-parent-label-both-nil ()
  "Returns nil when both inputs are nil."
  (should (null (claude-repl--parent-label nil nil))))

(ert-deftest claude-repl-test-parent-label-parent-only ()
  "Returns (\" <parent>\" nil) when only parent is set."
  (should (equal (claude-repl--parent-label "feature-foo" nil)
                 '(" feature-foo" nil))))

(ert-deftest claude-repl-test-parent-label-merge-only ()
  "Returns (\"\" \" (<merge>)\") when only merge is set (rare fallback case)."
  (should (equal (claude-repl--parent-label nil "explanation-engine")
                 '("" " (explanation-engine)"))))

(ert-deftest claude-repl-test-parent-label-equal-omits-parens ()
  "Returns (green-only nil) when parent equals merge — parens would be redundant."
  (should (equal (claude-repl--parent-label "feature-foo" "feature-foo")
                 '(" feature-foo" nil))))

(ert-deftest claude-repl-test-parent-label-different-splits-parts ()
  "Returns (\" <parent>\" \" (<merge>)\") when they differ (master-redirect case)."
  (should (equal (claude-repl--parent-label "feature-foo" "explanation-engine")
                 '(" feature-foo" " (explanation-engine)"))))

;;;; ---- Tests: workspace-mode-line face splitting ----

(ert-deftest claude-repl-test-workspace-mode-line-yellow-face-on-merge-suffix ()
  "When merge differs from parent, the (...) suffix is yellow while PARENT: <parent> stays green."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :source-ws-dir "/tmp/parent-worktrees/feature-foo")
    (cl-letf (((symbol-function 'claude-repl--merge-target-name)
               (lambda (_ws) "explanation-engine")))
      (let* ((seg (car (claude-repl--workspace-mode-line "ws")))
             (paren-start (string-match-p " (" seg)))
        (should paren-start)
        (let ((face-before (get-text-property (1- paren-start) 'face seg))
              (face-at-paren (get-text-property paren-start 'face seg)))
          (should (equal face-before '(:foreground "green" :weight bold)))
          (should (equal face-at-paren '(:foreground "yellow" :weight bold))))))))

(ert-deftest claude-repl-test-workspace-mode-line-green-throughout-when-no-merge-suffix ()
  "When merge equals parent (no suffix), the entire segment is green."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :source-ws-dir "/tmp/parent-worktrees/feature-foo")
    (cl-letf (((symbol-function 'claude-repl--merge-target-name)
               (lambda (_ws) "feature-foo")))
      (let ((seg (car (claude-repl--workspace-mode-line "ws"))))
        (should (equal (get-text-property 0 'face seg)
                       '(:foreground "green" :weight bold)))
        (should (equal (get-text-property (1- (length seg)) 'face seg)
                       '(:foreground "green" :weight bold)))))))

;;;; ---- Tests: merge-target-name ----

(ert-deftest claude-repl-test-merge-target-name-no-project-dir ()
  "Returns nil when the workspace has no :project-dir."
  (claude-repl-test--with-clean-state
    (should (null (claude-repl--merge-target-name "unknown-ws")))))

(ert-deftest claude-repl-test-merge-target-name-uses-resolved-target ()
  "Returns the basename of resolve-merge-into-source-target's output."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "wt-ws" :project-dir "/tmp/wt-dir/")
    (claude-repl--ws-put "wt-ws" :source-ws-dir "/tmp/parent-dir/")
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) t))
              ((symbol-function 'claude-repl--master-worktree-path)
               (lambda (_root) "/tmp/master-dir/"))
              ((symbol-function 'claude-repl--resolve-merge-into-source-target)
               (lambda (_p _m) "/tmp/parent-dir/"))
              ((symbol-function 'claude-repl--path-canonical) #'identity))
      (should (equal (claude-repl--merge-target-name "wt-ws") "parent-dir")))))

(ert-deftest claude-repl-test-merge-target-name-nil-when-target-equals-source ()
  "Returns nil when the resolved target equals the workspace's own dir
\(nothing to merge into — typically the master worktree)."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "self-ws" :project-dir "/tmp/self-dir/")
    (cl-letf (((symbol-function 'claude-repl--master-worktree-path)
               (lambda (_root) "/tmp/self-dir/"))
              ((symbol-function 'claude-repl--resolve-merge-into-source-target)
               (lambda (_p _m) "/tmp/self-dir/"))
              ((symbol-function 'claude-repl--path-canonical) #'identity))
      (should (null (claude-repl--merge-target-name "self-ws"))))))

(ert-deftest claude-repl-test-merge-target-name-redirects-to-master-via-resolver ()
  "When resolve-merge-into-source-target redirects to master, the basename
shown is the master worktree's, not the recorded parent's."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "wt-ws" :project-dir "/tmp/wt-dir/")
    (claude-repl--ws-put "wt-ws" :source-ws-dir "/tmp/parent-dir/")
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) t))
              ((symbol-function 'claude-repl--master-worktree-path)
               (lambda (_root) "/tmp/explanation-engine/"))
              ((symbol-function 'claude-repl--resolve-merge-into-source-target)
               (lambda (_p m) m))
              ((symbol-function 'claude-repl--path-canonical) #'identity))
      (should (equal (claude-repl--merge-target-name "wt-ws") "explanation-engine")))))

;;;; ---- Tests: Session completion handling ----

(ert-deftest claude-repl-test-mark-claude-done-sets-done ()
  "mark-claude-done sets :claude-state :done unconditionally."
  (claude-repl-test--with-clean-state
    (let ((done-set nil))
      (cl-letf (((symbol-function 'claude-repl--ws-set-claude-state)
                 (lambda (ws state)
                   (when (eq state :done) (setq done-set ws)))))
        (claude-repl--mark-claude-done "ws1")
        (should (equal done-set "ws1"))))))

(ert-deftest claude-repl-test-mark-claude-done-regardless-of-visibility ()
  "mark-claude-done no longer branches on vterm visibility.
The previous mark-done-if-hidden used the vterm window as a \"user is
already looking\" gate. Post-axis-split that gate is the renderer's job."
  (claude-repl-test--with-clean-state
    (let ((done-set nil))
      ;; Any hypothetical visibility — mark-claude-done does not read it.
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (&rest _) 'some-window))
                ((symbol-function 'claude-repl--ws-set-claude-state)
                 (lambda (ws state)
                   (when (eq state :done) (setq done-set ws)))))
        (claude-repl--mark-claude-done "ws1")
        (should (equal done-set "ws1"))))))

(ert-deftest claude-repl-test-mark-claude-done-current-ws-acks ()
  "mark-claude-done sets :done-acked t when the workspace is current
(user is actively looking when :done arrives)."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--current-ws-p)
               (lambda (_ws) t)))
      (claude-repl--mark-claude-done "ws1")
      (should (eq (claude-repl--ws-get "ws1" :done-acked) t)))))

(ert-deftest claude-repl-test-mark-claude-done-non-current-ws-clears-ack ()
  "mark-claude-done clears :done-acked to nil for non-current workspaces
so a fresh :done starts unacknowledged regardless of any leftover ack
from a prior cycle."
  (claude-repl-test--with-clean-state
    ;; Pretend a prior cycle left :done-acked t — must be cleared.
    (claude-repl--ws-put "ws1" :done-acked t)
    (cl-letf (((symbol-function 'claude-repl--current-ws-p)
               (lambda (_ws) nil)))
      (claude-repl--mark-claude-done "ws1")
      (should (null (claude-repl--ws-get "ws1" :done-acked))))))

(ert-deftest claude-repl-test-refresh-vterm-after-finish-live ()
  "refresh-vterm-after-finish should call do-refresh and update-hide-overlay for live buffer."
  (let ((refreshed nil)
        (overlay-updated nil)
        (scroll-fixed nil)
        (fake-buf (generate-new-buffer " *test-refresh*")))
    (unwind-protect
        (cl-letf (((symbol-function 'claude-repl--do-refresh)
                   (lambda () (setq refreshed t)))
                  ((symbol-function 'claude-repl--update-hide-overlay)
                   (lambda () (setq overlay-updated t)))
                  ((symbol-function 'claude-repl--fix-vterm-scroll)
                   (lambda (_buf) (setq scroll-fixed t))))
          (claude-repl--refresh-vterm-after-finish fake-buf)
          (should refreshed)
          (should overlay-updated)
          (should scroll-fixed))
      (kill-buffer fake-buf))))

(ert-deftest claude-repl-test-refresh-vterm-after-finish-dead ()
  "refresh-vterm-after-finish should be a no-op for a dead buffer."
  (let ((refreshed nil)
        (fake-buf (generate-new-buffer " *test-refresh-dead*")))
    (kill-buffer fake-buf)
    (cl-letf (((symbol-function 'claude-repl--do-refresh)
               (lambda () (setq refreshed t)))
              ((symbol-function 'claude-repl--fix-vterm-scroll) #'ignore))
      (claude-repl--refresh-vterm-after-finish fake-buf)
      (should-not refreshed))))

(ert-deftest claude-repl-test-handle-claude-finished-notifies-other-ws ()
  "handle-claude-finished should message when WS is not the current workspace."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/fake")
    (let ((messaged nil))
      (cl-letf (((symbol-function 'claude-repl--maybe-notify-finished) #'ignore)
                ((symbol-function '+workspace-current-name) (lambda () "other-ws"))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when (string-match-p "Claude finished" fmt)
                     (setq messaged t)))))
        (claude-repl--handle-claude-finished "ws1")
        (should messaged)))))

(ert-deftest claude-repl-test-handle-claude-finished-no-message-current-ws ()
  "handle-claude-finished should NOT message when WS is the current workspace."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/fake")
    (let ((messaged nil))
      (cl-letf (((symbol-function 'claude-repl--maybe-notify-finished) #'ignore)
                ((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when (string-match-p "Claude finished" fmt)
                     (setq messaged t)))))
        (claude-repl--handle-claude-finished "ws1")
        (should-not messaged)))))

(ert-deftest claude-repl-test-handle-claude-finished-errors-on-unregistered-ws ()
  "handle-claude-finished errors hard when WS is not registered — guards
against stop events arriving after kill."
  (claude-repl-test--with-clean-state
    (should-error (claude-repl--handle-claude-finished "not-a-ws"))))

;;;; ---- Tests: refresh-magit-status ----

(ert-deftest claude-repl-test-refresh-magit-status-refreshes-matching-buffer ()
  "refresh-magit-status calls magit-refresh on a magit-status buffer whose
default-directory matches the workspace's :project-dir."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "claude-magit-" t))
          (buf (generate-new-buffer " *test-magit-match*"))
          (refreshed 0))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws1" :project-dir tmpdir)
            (with-current-buffer buf
              (setq-local major-mode 'magit-status-mode)
              (setq-local default-directory (file-name-as-directory tmpdir)))
            (cl-letf (((symbol-function 'magit-refresh)
                       (lambda (&rest _) (cl-incf refreshed))))
              (claude-repl--refresh-magit-status "ws1")
              (should (= refreshed 1))))
        (kill-buffer buf)
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-refresh-magit-status-skips-non-matching-dir ()
  "refresh-magit-status does not refresh a magit-status buffer whose
default-directory points at a different repo."
  (claude-repl-test--with-clean-state
    (let ((ws-dir (make-temp-file "claude-magit-ws-" t))
          (other-dir (make-temp-file "claude-magit-other-" t))
          (buf (generate-new-buffer " *test-magit-other*"))
          (refreshed 0))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws1" :project-dir ws-dir)
            (with-current-buffer buf
              (setq-local major-mode 'magit-status-mode)
              (setq-local default-directory (file-name-as-directory other-dir)))
            (cl-letf (((symbol-function 'magit-refresh)
                       (lambda (&rest _) (cl-incf refreshed))))
              (claude-repl--refresh-magit-status "ws1")
              (should (= refreshed 0))))
        (kill-buffer buf)
        (delete-directory ws-dir t)
        (delete-directory other-dir t)))))

(ert-deftest claude-repl-test-refresh-magit-status-skips-non-magit-buffer ()
  "refresh-magit-status does not refresh a non-magit buffer even when
default-directory matches the workspace."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "claude-magit-" t))
          (buf (generate-new-buffer " *test-non-magit*"))
          (refreshed 0))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws1" :project-dir tmpdir)
            (with-current-buffer buf
              (setq-local major-mode 'fundamental-mode)
              (setq-local default-directory (file-name-as-directory tmpdir)))
            (cl-letf (((symbol-function 'magit-refresh)
                       (lambda (&rest _) (cl-incf refreshed))))
              (claude-repl--refresh-magit-status "ws1")
              (should (= refreshed 0))))
        (kill-buffer buf)
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-refresh-magit-status-no-buffer-is-noop ()
  "refresh-magit-status is a no-op when no magit-status buffer exists for WS."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "claude-magit-" t))
          (refreshed 0))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws1" :project-dir tmpdir)
            (cl-letf (((symbol-function 'magit-refresh)
                       (lambda (&rest _) (cl-incf refreshed))))
              (claude-repl--refresh-magit-status "ws1")
              (should (= refreshed 0))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-refresh-magit-status-no-project-dir-is-noop ()
  "refresh-magit-status is a no-op when WS has no :project-dir."
  (claude-repl-test--with-clean-state
    (let ((refreshed 0))
      (cl-letf (((symbol-function 'magit-refresh)
                 (lambda (&rest _) (cl-incf refreshed))))
        (claude-repl--refresh-magit-status "ws1")
        (should (= refreshed 0))))))

(ert-deftest claude-repl-test-handle-claude-finished-refreshes-magit ()
  "handle-claude-finished calls refresh-magit-status as part of the done policy."
  (claude-repl-test--with-clean-state
    (let ((refresh-ws nil))
      (claude-repl--ws-put "ws1" :vterm-buffer nil)
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                ((symbol-function 'claude-repl--maybe-notify-finished) #'ignore)
                ((symbol-function 'claude-repl--refresh-magit-status)
                 (lambda (ws) (setq refresh-ws ws))))
        (claude-repl--handle-claude-finished "ws1")
        (should (equal refresh-ws "ws1"))))))

;;;; ---- Tests: maybe-notify-finished edge cases ----

(ert-deftest claude-repl-test-maybe-notify-first-call-no-last-time ()
  "maybe-notify-finished should notify on first call (no :last-notify-time set)."
  (claude-repl-test--with-clean-state
    (let ((notify-count 0))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda () nil))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat _fn &rest _args)
                   (cl-incf notify-count))))
        (claude-repl--maybe-notify-finished "ws1")
        (should (= notify-count 1))))))

(ert-deftest claude-repl-test-maybe-notify-stores-time ()
  "maybe-notify-finished should store :last-notify-time after notifying."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'frame-focus-state) (lambda () nil))
              ((symbol-function 'run-at-time) #'ignore))
      (claude-repl--maybe-notify-finished "ws1")
      (should (numberp (claude-repl--ws-get "ws1" :last-notify-time))))))

;;;; ---- Tests: Readiness handling ----

(ert-deftest claude-repl-test-drain-pending-prompts-empty ()
  "drain-pending-prompts should return nil when no pending prompts."
  (claude-repl-test--with-clean-state
    (should-not (claude-repl--drain-pending-prompts "ws1"))))

(ert-deftest claude-repl-test-drain-pending-prompts-sends ()
  "drain-pending-prompts should clear prompts and schedule delivery."
  (claude-repl-test--with-clean-state
    (let ((timer-scheduled nil)
          (fake-buf (generate-new-buffer " *test-drain*")))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws1" :pending-prompts '("prompt1" "prompt2"))
            (claude-repl--ws-put "ws1" :vterm-buffer fake-buf)
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (_delay _repeat fn &rest args)
                         (setq timer-scheduled t))))
              (should (claude-repl--drain-pending-prompts "ws1"))
              (should timer-scheduled)
              (should-not (claude-repl--ws-get "ws1" :pending-prompts))))
        (kill-buffer fake-buf)))))

;; Test helper: install mocks that capture sends into SEND-SLOT (a cons
;; cell; the caller reads (car send-slot) for the reverse-chronological
;; list of prompts) and capture the scheduled verify-timer thunk into
;; TIMER-SLOT (a cons cell whose car is the most recent thunk).  Returns
;; nothing; callers invoke the body inside the cl-letf via the macro form.
(defmacro claude-repl-test--with-deliver-mocks (send-slot timer-slot &rest body)
  "Run BODY with `claude-repl--send' and `run-at-time' mocked.
SEND-SLOT is a cons cell; each send pushes its prompt onto (car SEND-SLOT).
TIMER-SLOT is a cons cell; the most recent scheduled thunk is stored at
(car TIMER-SLOT) for synchronous firing."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'claude-repl--send)
              (lambda (p _ws _force-meta on-settle)
                (setcar ,send-slot (cons p (car ,send-slot)))
                (when on-settle (funcall on-settle))))
             ((symbol-function 'run-at-time)
              (lambda (_delay _repeat fn &rest args)
                (setcar ,timer-slot (lambda () (apply fn args))))))
     ,@body))

(ert-deftest claude-repl-test-deliver-pending-prompts-sends-first ()
  "deliver-pending-prompts sends the first prompt immediately."
  (claude-repl-test--with-clean-state
    (let ((sent (list nil))
          (timer-slot (list nil))
          (fake-buf (generate-new-buffer " *test-deliver*")))
      (unwind-protect
          (progn
            (claude-repl-test--with-deliver-mocks sent timer-slot
              (claude-repl--deliver-pending-prompts fake-buf '("a" "b") "ws1"))
            (should (equal (car sent) '("a"))))
        (kill-buffer fake-buf)))))

(ert-deftest claude-repl-test-deliver-pending-prompts-schedules-verify ()
  "deliver-pending-prompts schedules a verify timer via on-settle."
  (claude-repl-test--with-clean-state
    (let ((sent (list nil))
          (timer-slot (list nil))
          (fake-buf (generate-new-buffer " *test-deliver-verify*")))
      (unwind-protect
          (progn
            (claude-repl-test--with-deliver-mocks sent timer-slot
              (claude-repl--deliver-pending-prompts fake-buf '("a") "ws1"))
            (should (functionp (car timer-slot))))
        (kill-buffer fake-buf)))))

(ert-deftest claude-repl-test-deliver-pending-prompts-chains-on-ack ()
  "When the verify step sees an acknowledged state, the next prompt is sent."
  (claude-repl-test--with-clean-state
    (let ((sent (list nil))
          (timer-slot (list nil))
          (fake-buf (generate-new-buffer " *test-deliver-chain*")))
      (unwind-protect
          (claude-repl-test--with-deliver-mocks sent timer-slot
            (claude-repl--deliver-pending-prompts fake-buf '("a" "b") "ws1")
            (should (equal (car sent) '("a")))
            ;; Simulate `prompt_submit' arrival between paste and verify.
            (claude-repl--ws-put "ws1" :claude-state :thinking)
            (funcall (car timer-slot))
            (should (equal (reverse (car sent)) '("a" "b"))))
        (kill-buffer fake-buf)))))

(ert-deftest claude-repl-test-deliver-pending-prompts-resends-when-not-acked ()
  "When verify sees :idle (no ack), the same prompt is resent."
  (claude-repl-test--with-clean-state
    (let ((sent (list nil))
          (timer-slot (list nil))
          (fake-buf (generate-new-buffer " *test-deliver-resend*")))
      (unwind-protect
          (claude-repl-test--with-deliver-mocks sent timer-slot
            (claude-repl--deliver-pending-prompts fake-buf '("a") "ws1")
            (should (equal (car sent) '("a")))
            ;; State remains :idle — Claude never saw the paste.
            (claude-repl--ws-put "ws1" :claude-state :idle)
            (funcall (car timer-slot))
            ;; Same prompt resent.
            (should (equal (car sent) '("a" "a"))))
        (kill-buffer fake-buf)))))

(ert-deftest claude-repl-test-deliver-pending-prompts-gives-up-after-max-retries ()
  "After `claude-repl-prompt-delivery-max-retries' failed resends, give up
without sending again."
  (claude-repl-test--with-clean-state
    (let ((sent (list nil))
          (timer-slot (list nil))
          (fake-buf (generate-new-buffer " *test-deliver-giveup*"))
          (claude-repl-prompt-delivery-max-retries 2))
      (unwind-protect
          (claude-repl-test--with-deliver-mocks sent timer-slot
            (claude-repl--ws-put "ws1" :claude-state :idle)
            (claude-repl--deliver-pending-prompts fake-buf '("a") "ws1")
            ;; First send + 2 retries = 3 total.
            (funcall (car timer-slot))   ; retry 1
            (funcall (car timer-slot))   ; retry 2
            (funcall (car timer-slot))   ; give up — no further send
            (should (equal (car sent) '("a" "a" "a"))))
        (kill-buffer fake-buf)))))

(ert-deftest claude-repl-test-deliver-pending-prompts-abandons-on-dead-vterm-at-verify ()
  "If the vterm buffer dies between send and verify, abandon silently."
  (claude-repl-test--with-clean-state
    (let ((sent (list nil))
          (timer-slot (list nil))
          (fake-buf (generate-new-buffer " *test-deliver-dead-verify*")))
      (claude-repl-test--with-deliver-mocks sent timer-slot
        (claude-repl--deliver-pending-prompts fake-buf '("a" "b") "ws1")
        (should (equal (car sent) '("a")))
        ;; Kill the buffer before verify fires.
        (kill-buffer fake-buf)
        (claude-repl--ws-put "ws1" :claude-state :thinking)
        (funcall (car timer-slot))
        ;; "b" was never sent — vterm-buf is dead.
        (should (equal (car sent) '("a")))))))

(ert-deftest claude-repl-test-deliver-pending-prompts-dead-buf ()
  "deliver-pending-prompts should signal an error when buffer is dead."
  (let ((fake-buf (generate-new-buffer " *test-deliver-dead*")))
    (kill-buffer fake-buf)
    (should-error (claude-repl--deliver-pending-prompts fake-buf '("a") "ws1")
                  :type 'error)))

(ert-deftest claude-repl-test-prompt-acknowledged-p-states ()
  "prompt-acknowledged-p recognizes thinking/permission/done as ack'd."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :claude-state :thinking)
    (should (claude-repl--prompt-acknowledged-p "ws1"))
    (claude-repl--ws-put "ws1" :claude-state :permission)
    (should (claude-repl--prompt-acknowledged-p "ws1"))
    (claude-repl--ws-put "ws1" :claude-state :done)
    (should (claude-repl--prompt-acknowledged-p "ws1"))))

(ert-deftest claude-repl-test-prompt-acknowledged-p-idle-not-acked ()
  "prompt-acknowledged-p returns nil for :idle (the race state)."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :claude-state :idle)
    (should-not (claude-repl--prompt-acknowledged-p "ws1"))))

(ert-deftest claude-repl-test-prompt-acknowledged-p-init-not-acked ()
  "prompt-acknowledged-p returns nil for :init (pre-ready)."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :claude-state :init)
    (should-not (claude-repl--prompt-acknowledged-p "ws1"))))

(ert-deftest claude-repl-test-prompt-acknowledged-p-nil-not-acked ()
  "prompt-acknowledged-p returns nil when :claude-state is nil."
  (claude-repl-test--with-clean-state
    (should-not (claude-repl--prompt-acknowledged-p "ws1"))))

(ert-deftest claude-repl-test-show-panels-or-defer-current-ws ()
  "show-panels-or-defer should show panels when WS is current."
  (claude-repl-test--with-clean-state
    (let ((panels-opened nil))
      (cl-letf (((symbol-function 'claude-repl--current-ws-p) (lambda (_ws) t))
                ((symbol-function 'claude-repl--loading-placeholder-visible-p)
                 (lambda () nil))
                ((symbol-function 'claude-repl--show-hidden-panels)
                 (lambda () (setq panels-opened t))))
        (claude-repl--show-panels-or-defer "ws1")
        (should panels-opened)))))

(ert-deftest claude-repl-test-show-panels-or-defer-other-ws ()
  "show-panels-or-defer should set :pending-show-panels when WS is not current."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--current-ws-p) (lambda (_ws) nil)))
      (claude-repl--show-panels-or-defer "ws1")
      (should (claude-repl--ws-get "ws1" :pending-show-panels)))))

(ert-deftest claude-repl-test-open-panels-after-ready-with-pending ()
  "open-panels-after-ready should show panels when there are pending prompts."
  (claude-repl-test--with-clean-state
    (let ((shown nil))
      (cl-letf (((symbol-function 'claude-repl--drain-pending-prompts)
                 (lambda (_ws) '("prompt1")))
                ((symbol-function 'claude-repl--show-panels-or-defer)
                 (lambda (_ws) (setq shown t))))
        (claude-repl--open-panels-after-ready "ws1")
        (should shown)))))

(ert-deftest claude-repl-test-open-panels-after-ready-no-pending-current ()
  "open-panels-after-ready should open panels when no pending prompts and WS is current."
  (claude-repl-test--with-clean-state
    (let ((panels-opened nil))
      (cl-letf (((symbol-function 'claude-repl--drain-pending-prompts)
                 (lambda (_ws) nil))
                ((symbol-function 'claude-repl--current-ws-p) (lambda (_ws) t))
                ((symbol-function 'claude-repl--loading-placeholder-visible-p)
                 (lambda () nil))
                ((symbol-function 'claude-repl--show-hidden-panels)
                 (lambda () (setq panels-opened t))))
        (claude-repl--open-panels-after-ready "ws1")
        (should panels-opened)))))

(ert-deftest claude-repl-test-open-panels-after-ready-no-pending-other ()
  "open-panels-after-ready should NOT open panels when no pending prompts and WS is other."
  (claude-repl-test--with-clean-state
    (let ((panels-opened nil))
      (cl-letf (((symbol-function 'claude-repl--drain-pending-prompts)
                 (lambda (_ws) nil))
                ((symbol-function 'claude-repl--current-ws-p) (lambda (_ws) nil))
                ((symbol-function 'claude-repl--show-hidden-panels)
                 (lambda () (setq panels-opened t))))
        (claude-repl--open-panels-after-ready "ws1")
        (should-not panels-opened)))))

(ert-deftest claude-repl-test-open-panels-after-ready-respects-persisted-inactive ()
  "open-panels-after-ready must NOT open panels when the workspace's
hydrated `:repl-state' is `:inactive' — even on the current ws — so
hide-mode survives Emacs restart."
  (claude-repl-test--with-clean-state
    (let ((panels-opened nil))
      (claude-repl--ws-put "ws1" :repl-state :inactive)
      (cl-letf (((symbol-function 'claude-repl--drain-pending-prompts)
                 (lambda (_ws) nil))
                ((symbol-function 'claude-repl--current-ws-p) (lambda (_ws) t))
                ((symbol-function 'claude-repl--loading-placeholder-visible-p)
                 (lambda () nil))
                ((symbol-function 'claude-repl)
                 (lambda () (setq panels-opened t))))
        (claude-repl--open-panels-after-ready "ws1")
        (should-not panels-opened)))))

(ert-deftest claude-repl-test-open-panels-after-ready-respects-persisted-hidden ()
  "open-panels-after-ready must NOT open panels when the workspace's
hydrated `:repl-state' is `:hidden' — same skip as `:inactive' — so a
deprio-closed ws (`SPC o C') stays hidden across restart."
  (claude-repl-test--with-clean-state
    (let ((panels-opened nil))
      (claude-repl--ws-put "ws1" :repl-state :hidden)
      (cl-letf (((symbol-function 'claude-repl--drain-pending-prompts)
                 (lambda (_ws) nil))
                ((symbol-function 'claude-repl--current-ws-p) (lambda (_ws) t))
                ((symbol-function 'claude-repl--loading-placeholder-visible-p)
                 (lambda () nil))
                ((symbol-function 'claude-repl)
                 (lambda () (setq panels-opened t))))
        (claude-repl--open-panels-after-ready "ws1")
        (should-not panels-opened)))))

(ert-deftest claude-repl-test-open-panels-after-ready-pending-prompts-override-inactive ()
  "Pending prompts must force panel display even when persisted
`:repl-state' is `:inactive' — the user has explicitly queued work, so
they want to see the result; hide-mode is overridden."
  (claude-repl-test--with-clean-state
    (let ((shown nil))
      (claude-repl--ws-put "ws1" :repl-state :inactive)
      (cl-letf (((symbol-function 'claude-repl--drain-pending-prompts)
                 (lambda (_ws) '("prompt1")))
                ((symbol-function 'claude-repl--show-panels-or-defer)
                 (lambda (_ws) (setq shown t))))
        (claude-repl--open-panels-after-ready "ws1")
        (should shown)))))

;;;; ---- Tests: Process state predicates ----

(ert-deftest claude-repl-test-vterm-process-alive-no-buffer ()
  "vterm-process-alive-p should return nil when no vterm buffer is set."
  (claude-repl-test--with-clean-state
    (should-not (claude-repl--vterm-process-alive-p "ws1"))))

(ert-deftest claude-repl-test-vterm-process-alive-dead-buffer ()
  "vterm-process-alive-p should return nil when buffer has been killed."
  (claude-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *test-dead-vterm*")))
      (claude-repl--ws-put "ws1" :vterm-buffer buf)
      (kill-buffer buf)
      (should-not (claude-repl--vterm-process-alive-p "ws1")))))

(ert-deftest claude-repl-test-vterm-process-alive-no-process ()
  "vterm-process-alive-p should return nil when buffer has no process."
  (claude-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *test-no-process*")))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws1" :vterm-buffer buf)
            (should-not (claude-repl--vterm-process-alive-p "ws1")))
        (kill-buffer buf)))))

(ert-deftest claude-repl-test-session-starting-p-not-running ()
  "session-starting-p should return nil when vterm is not running."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--vterm-process-alive-p)
               (lambda (_ws) nil)))
      (should-not (claude-repl--session-starting-p "ws1")))))

(ert-deftest claude-repl-test-session-starting-p-ready ()
  "session-starting-p should return nil when process is alive but already ready."
  (claude-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *test-starting-ready*")))
      (unwind-protect
          (progn
            (with-current-buffer buf (setq-local claude-repl--ready t))
            (claude-repl--ws-put "ws1" :vterm-buffer buf)
            (cl-letf (((symbol-function 'claude-repl--vterm-process-alive-p)
                       (lambda (_ws) t)))
              (should-not (claude-repl--session-starting-p "ws1"))))
        (kill-buffer buf)))))

(ert-deftest claude-repl-test-session-starting-p-true ()
  "session-starting-p should return t when process alive but not ready."
  (claude-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *test-starting*")))
      (unwind-protect
          (progn
            (with-current-buffer buf (setq-local claude-repl--ready nil))
            (claude-repl--ws-put "ws1" :vterm-buffer buf)
            (cl-letf (((symbol-function 'claude-repl--vterm-process-alive-p)
                       (lambda (_ws) t)))
              (should (claude-repl--session-starting-p "ws1"))))
        (kill-buffer buf)))))

;;;; ---- Tests: Readiness timer ----

(ert-deftest claude-repl-test-cancel-ready-timer-no-timer ()
  "cancel-ready-timer should be a no-op when no timer is set."
  (claude-repl-test--with-clean-state
    (claude-repl--cancel-ready-timer "ws1")
    (should-not (claude-repl--ws-get "ws1" :ready-timer))))

(ert-deftest claude-repl-test-cancel-ready-timer-with-timer ()
  "cancel-ready-timer should cancel and clear the timer."
  (claude-repl-test--with-clean-state
    (let ((cancelled nil)
          (fake-timer (run-at-time 999 nil #'ignore)))
      (claude-repl--ws-put "ws1" :ready-timer fake-timer)
      (claude-repl--cancel-ready-timer "ws1")
      (should-not (claude-repl--ws-get "ws1" :ready-timer)))))

(ert-deftest claude-repl-test-ready-timer-tick-timeout ()
  "ready-timer-tick should cancel timer after 30s timeout."
  (claude-repl-test--with-clean-state
    (let ((cancelled nil))
      (cl-letf (((symbol-function 'claude-repl--cancel-ready-timer)
                 (lambda (_ws) (setq cancelled t))))
        (claude-repl--ready-timer-tick "ws1" (- (float-time) 31.0))
        (should cancelled)))))

(ert-deftest claude-repl-test-ready-timer-tick-still-starting ()
  "ready-timer-tick should do nothing when session is still starting."
  (claude-repl-test--with-clean-state
    (let ((cancelled nil))
      (cl-letf (((symbol-function 'claude-repl--cancel-ready-timer)
                 (lambda (_ws) (setq cancelled t)))
                ((symbol-function 'claude-repl--session-starting-p)
                 (lambda (_ws) t)))
        (claude-repl--ready-timer-tick "ws1" (float-time))
        (should-not cancelled)))))

(ert-deftest claude-repl-test-ready-timer-tick-ready-current-ws ()
  "ready-timer-tick should cancel timer and open panels when ready and current."
  (claude-repl-test--with-clean-state
    (let ((cancelled nil)
          (panels-opened nil))
      (cl-letf (((symbol-function 'claude-repl--cancel-ready-timer)
                 (lambda (_ws) (setq cancelled t)))
                ((symbol-function 'claude-repl--session-starting-p)
                 (lambda (_ws) nil))
                ((symbol-function 'claude-repl--current-ws-p) (lambda (_ws) t))
                ((symbol-function 'claude-repl)
                 (lambda () (setq panels-opened t))))
        (claude-repl--ready-timer-tick "ws1" (float-time))
        (should cancelled)
        (should panels-opened)))))

(ert-deftest claude-repl-test-ready-timer-tick-ready-other-ws ()
  "ready-timer-tick should cancel timer but not open panels when ready and not current."
  (claude-repl-test--with-clean-state
    (let ((cancelled nil)
          (panels-opened nil))
      (cl-letf (((symbol-function 'claude-repl--cancel-ready-timer)
                 (lambda (_ws) (setq cancelled t)))
                ((symbol-function 'claude-repl--session-starting-p)
                 (lambda (_ws) nil))
                ((symbol-function 'claude-repl--current-ws-p) (lambda (_ws) nil))
                ((symbol-function 'claude-repl)
                 (lambda () (setq panels-opened t))))
        (claude-repl--ready-timer-tick "ws1" (float-time))
        (should cancelled)
        (should-not panels-opened)))))

(ert-deftest claude-repl-test-schedule-ready-timer ()
  "schedule-ready-timer should cancel existing timer and schedule new one."
  (claude-repl-test--with-clean-state
    (let ((cancel-count 0))
      (cl-letf (((symbol-function 'claude-repl--cancel-ready-timer)
                 (lambda (_ws) (cl-incf cancel-count)))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat _fn &rest _args) 'fake-timer)))
        (claude-repl--schedule-ready-timer "ws1")
        (should (= cancel-count 1))
        (should (eq (claude-repl--ws-get "ws1" :ready-timer) 'fake-timer))))))

;;;; ---- Tests: Workspace environment initialization ----

(ert-deftest claude-repl-test-initialize-ws-env-initializes-fresh ()
  "initialize-ws-env on a fresh workspace with no state file sets up
default `:active-env' and instantiation structs for each environment.
The project-dir hint is used to locate the (absent) state file."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-fresh-" t)))
      (unwind-protect
          (progn
            (claude-repl--initialize-ws-env "ws1" tmpdir)
            (should (eq (claude-repl--ws-get "ws1" :active-env) :bare-metal))
            (should (claude-repl-instantiation-p (claude-repl--ws-get "ws1" :sandbox)))
            (should (claude-repl-instantiation-p (claude-repl--ws-get "ws1" :bare-metal))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-initialize-ws-env-errors-when-no-root-derivable ()
  "initialize-ws-env errors when :project-dir cannot be derived from any source."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--git-root) (lambda (&optional _d) nil)))
      (should-error (claude-repl--initialize-ws-env "ws1") :type 'error))))

(ert-deftest claude-repl-test-initialize-ws-env-idempotent-recovers-partial-state ()
  "initialize-ws-env can be called on a workspace with partial state
(`:active-env' set, `:project-dir' nil) and re-initializes it correctly
using the project-dir hint.  Models the fix for the partial-init bug
where fresh-ws-env wrote :active-env without :project-dir."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-partial-" t)))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws1" :active-env :bare-metal)
            ;; No :project-dir set — exactly the partial-init case.
            (claude-repl--initialize-ws-env "ws1" tmpdir)
            (should (equal (claude-repl--ws-get "ws1" :project-dir)
                           (claude-repl--path-canonical tmpdir)))
            (should (claude-repl-instantiation-p (claude-repl--ws-get "ws1" :bare-metal))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-initialize-ws-env-restores-last-prompt-time ()
  "initialize-ws-env hydrates `:last-prompt-time' from the saved state file."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-lpt-" t)))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws1" :project-dir tmpdir)
            (claude-repl--ws-put "ws1" :active-env :bare-metal)
            (claude-repl--ws-put "ws1" :last-prompt-time 1700000000.5)
            (claude-repl--ws-put "ws1" :bare-metal (make-claude-repl-instantiation))
            (claude-repl--ws-put "ws1" :sandbox (make-claude-repl-instantiation))
            (claude-repl--state-save "ws1")
            (remhash "ws1" claude-repl--workspaces)
            (claude-repl--initialize-ws-env "ws1" tmpdir)
            (should (equal (claude-repl--ws-get "ws1" :last-prompt-time)
                           1700000000.5)))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-initialize-ws-env-active-env-hint-sets-sandbox ()
  "initialize-ws-env uses ACTIVE-ENV-HINT when provided and no state file exists."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-sandbox-" t)))
      (unwind-protect
          (progn
            (claude-repl--initialize-ws-env "ws1" tmpdir :sandbox)
            (should (eq (claude-repl--ws-get "ws1" :active-env) :sandbox)))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-initialize-ws-env-state-file-beats-hint ()
  "State file value for :project-dir and :active-env overrides caller hints."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-override-" t)))
      (unwind-protect
          (progn
            (claude-repl--write-sexp-file
             (claude-repl--state-file (claude-repl--path-canonical tmpdir))
             `(:project-dir ,(claude-repl--path-canonical tmpdir)
               :active-env :sandbox
               :bare-metal (:session-id "bm-saved")
               :sandbox (:session-id "sb-saved")))
            (claude-repl--initialize-ws-env "ws1" tmpdir :bare-metal)
            (should (eq (claude-repl--ws-get "ws1" :active-env) :sandbox))
            (should (equal (claude-repl-instantiation-session-id
                            (claude-repl--ws-get "ws1" :bare-metal))
                           "bm-saved")))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-initialize-ws-env-restores-saved-tab-index ()
  "initialize-ws-env hydrates `:saved-tab-index' from the saved file so a
ws that was deprioritized at quit returns to its prior slot on restart."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-tabidx-" t)))
      (unwind-protect
          (progn
            (claude-repl--write-sexp-file
             (claude-repl--state-file tmpdir)
             `(:project-dir ,(claude-repl--path-canonical tmpdir)
               :active-env :bare-metal
               :saved-tab-index 4
               :bare-metal nil
               :sandbox nil))
            (claude-repl--initialize-ws-env "ws1" tmpdir)
            (should (eq (claude-repl--ws-get "ws1" :saved-tab-index) 4)))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-initialize-ws-env-restores-fork-session-id ()
  "initialize-ws-env hydrates `:fork-session-id' from the saved file so a
fork-ws whose claude session never started before quit can launch with
--fork-session on the next start."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-fork-" t)))
      (unwind-protect
          (progn
            (claude-repl--write-sexp-file
             (claude-repl--state-file tmpdir)
             `(:project-dir ,(claude-repl--path-canonical tmpdir)
               :active-env :bare-metal
               :fork-session-id "fsid-abc"
               :bare-metal nil
               :sandbox nil))
            (claude-repl--initialize-ws-env "ws1" tmpdir)
            (should (equal (claude-repl--ws-get "ws1" :fork-session-id) "fsid-abc")))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-initialize-ws-env-restores-last-prompt-summary ()
  "initialize-ws-env hydrates `:last-prompt-summary' so the tabline hint
survives restart."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-sum-" t)))
      (unwind-protect
          (progn
            (claude-repl--write-sexp-file
             (claude-repl--state-file tmpdir)
             `(:project-dir ,(claude-repl--path-canonical tmpdir)
               :active-env :bare-metal
               :last-prompt-summary "refactor auth"
               :bare-metal nil
               :sandbox nil))
            (claude-repl--initialize-ws-env "ws1" tmpdir)
            (should (equal (claude-repl--ws-get "ws1" :last-prompt-summary)
                           "refactor auth")))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-initialize-ws-env-restores-repl-state-inactive ()
  "initialize-ws-env hydrates `:repl-state :inactive' from the saved file
so hide-mode survives Emacs restart."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-rs-" t)))
      (unwind-protect
          (progn
            (claude-repl--write-sexp-file
             (claude-repl--state-file (claude-repl--path-canonical tmpdir))
             `(:project-dir ,(claude-repl--path-canonical tmpdir)
               :active-env :bare-metal
               :repl-state :inactive
               :bare-metal nil
               :sandbox nil))
            (claude-repl--initialize-ws-env "ws1" tmpdir)
            (should (eq (claude-repl--ws-get "ws1" :repl-state) :inactive)))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-initialize-ws-env-restores-repl-state-hidden ()
  "initialize-ws-env hydrates `:repl-state :hidden' from the saved file
so the deprio-hide marker survives Emacs restart."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-rs-hidden-" t)))
      (unwind-protect
          (progn
            (claude-repl--write-sexp-file
             (claude-repl--state-file tmpdir)
             `(:project-dir ,(claude-repl--path-canonical tmpdir)
               :active-env :bare-metal
               :repl-state :hidden
               :bare-metal nil
               :sandbox nil))
            (claude-repl--initialize-ws-env "ws1" tmpdir)
            (should (eq (claude-repl--ws-get "ws1" :repl-state) :hidden)))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-initialize-ws-env-skips-non-persistable-repl-state ()
  "initialize-ws-env ignores `:repl-state :dead' / nil from the saved file
— those are not desired-state hints and should not pin behavior on
restart (the lazy-start path applies its defaults instead)."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-rs-dead-" t)))
      (unwind-protect
          (progn
            (claude-repl--write-sexp-file
             (claude-repl--state-file (claude-repl--path-canonical tmpdir))
             `(:project-dir ,(claude-repl--path-canonical tmpdir)
               :active-env :bare-metal
               :repl-state :dead
               :bare-metal nil
               :sandbox nil))
            (claude-repl--initialize-ws-env "ws1" tmpdir)
            (should (null (claude-repl--ws-get "ws1" :repl-state))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-initialize-ws-env-restores-priority-from-state ()
  "initialize-ws-env hydrates `:priority' from the saved state file."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-pri-" t)))
      (unwind-protect
          (progn
            (claude-repl--write-sexp-file
             (claude-repl--state-file (claude-repl--path-canonical tmpdir))
             `(:project-dir ,(claude-repl--path-canonical tmpdir)
               :active-env :bare-metal
               :priority "p1"
               :bare-metal nil
               :sandbox nil))
            (claude-repl--initialize-ws-env "ws1" tmpdir)
            (should (equal (claude-repl--ws-get "ws1" :priority) "p1")))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-initialize-ws-env-priority-fallback-to-plist ()
  "With no saved priority, existing plist `:priority' is preserved (covers
`claude-repl-set-priority' running before any state-save)."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-pri-fb-" t)))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws1" :priority "p2")
            (claude-repl--initialize-ws-env "ws1" tmpdir)
            (should (equal (claude-repl--ws-get "ws1" :priority) "p2")))
        (delete-directory tmpdir t)))))

;;;; ---- Tests: prompt-sandbox-build ----

(ert-deftest claude-repl-test-prompt-sandbox-build-yes-compiles-and-errors ()
  "prompt-sandbox-build should call compile then signal user-error when user says yes."
  (let ((compiled-cmd nil))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
              ((symbol-function 'compile)
               (lambda (cmd) (setq compiled-cmd cmd))))
      (should-error
       (claude-repl--prompt-sandbox-build
        '(:image "my-img:latest" :install-script "/repo/.agents-sandbox/install-claude.sh"))
       :type 'user-error)
      (should (string-match-p "install-claude.sh" compiled-cmd)))))

(ert-deftest claude-repl-test-prompt-sandbox-build-no-falls-through ()
  "prompt-sandbox-build should return nil (no error) when user answers no."
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil))
            ((symbol-function 'compile) #'ignore))
    ;; Should NOT signal user-error -- just returns nil
    (should-not
     (claude-repl--prompt-sandbox-build
      '(:image "my-img:latest" :install-script "/repo/.agents-sandbox/install-claude.sh")))))

(ert-deftest claude-repl-test-prompt-sandbox-build-no-install-script ()
  "prompt-sandbox-build should signal user-error with manual instruction when no install-script."
  (should-error
   (claude-repl--prompt-sandbox-build '(:image "my-img:latest" :install-script nil))
   :type 'user-error))

(ert-deftest claude-repl-test-prompt-sandbox-build-empty-image ()
  "prompt-sandbox-build with empty image should still signal user-error when no install-script."
  (should-error
   (claude-repl--prompt-sandbox-build '(:image "" :install-script nil))
   :type 'user-error))

;;;; ---- Tests: get-sandbox-image ----

(ert-deftest claude-repl-test-get-sandbox-image-non-worktree ()
  "get-sandbox-image should return nil for non-worktree workspaces."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :worktree-p nil)
    (claude-repl--ws-put "ws1" :active-env :bare-metal)
    (let ((resolve-called nil))
      (cl-letf (((symbol-function 'claude-repl--resolve-sandbox-config)
                 (lambda (_r) (setq resolve-called t) nil)))
        (should-not (claude-repl--get-sandbox-image "ws1"))
        (should-not resolve-called)))))

(ert-deftest claude-repl-test-get-sandbox-image-not-sandbox-env ()
  "get-sandbox-image should return nil when :active-env is not :sandbox."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :worktree-p t)
    (claude-repl--ws-put "ws1" :active-env :bare-metal)
    (let ((resolve-called nil))
      (cl-letf (((symbol-function 'claude-repl--resolve-sandbox-config)
                 (lambda (_r) (setq resolve-called t) nil)))
        (should-not (claude-repl--get-sandbox-image "ws1"))
        (should-not resolve-called)))))

(ert-deftest claude-repl-test-get-sandbox-image-exists ()
  "get-sandbox-image should return config plist when image is ready."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :worktree-p t)
    (claude-repl--ws-put "ws1" :active-env :sandbox)
    (claude-repl--ws-put "ws1" :project-dir "/my/project")
    (cl-letf (((symbol-function 'claude-repl--git-root) (lambda (_d) "/my/project"))
              ((symbol-function 'claude-repl--resolve-sandbox-config)
               (lambda (_r) '(:image "img:latest" :script "/usr/bin/claude-sandbox"))))
      (let ((result (claude-repl--get-sandbox-image "ws1")))
        (should (equal (plist-get result :image) "img:latest"))
        (should (equal (plist-get result :script) "/usr/bin/claude-sandbox"))))))

(ert-deftest claude-repl-test-get-sandbox-image-needs-build ()
  "get-sandbox-image should call prompt-sandbox-build when image needs building."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :worktree-p t)
    (claude-repl--ws-put "ws1" :active-env :sandbox)
    (claude-repl--ws-put "ws1" :project-dir "/my/project")
    (let ((prompt-called nil))
      (cl-letf (((symbol-function 'claude-repl--git-root) (lambda (_d) "/my/project"))
                ((symbol-function 'claude-repl--resolve-sandbox-config)
                 (lambda (_r) '(:needs-build t :image "img:latest"
                                :install-script "/repo/install.sh")))
                ((symbol-function 'claude-repl--prompt-sandbox-build)
                 (lambda (_cfg) (setq prompt-called t))))
        (claude-repl--get-sandbox-image "ws1")
        (should prompt-called)))))

(ert-deftest claude-repl-test-get-sandbox-image-nil-project-dir ()
  "get-sandbox-image should return nil when :project-dir is nil."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :worktree-p t)
    (claude-repl--ws-put "ws1" :active-env :sandbox)
    (claude-repl--ws-put "ws1" :project-dir nil)
    (cl-letf (((symbol-function 'claude-repl--git-root) (lambda (_d) nil))
              ((symbol-function 'claude-repl--resolve-sandbox-config)
               (lambda (_r) nil)))
      (should-not (claude-repl--get-sandbox-image "ws1")))))

;;;; ---- Tests: build-start-cmd ----

(ert-deftest claude-repl-test-build-start-cmd-bare-metal-no-session ()
  "build-start-cmd for bare-metal with no session should produce claude --dangerously-skip-permissions."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :active-env :bare-metal)
    (claude-repl--ws-put "ws1" :bare-metal (make-claude-repl-instantiation))
    (claude-repl--ws-put "ws1" :worktree-p nil)
    (claude-repl--ws-put "ws1" :project-dir "/home/user/personal/project")
    (cl-letf (((symbol-function 'claude-repl--get-sandbox-image)
               (lambda (_ws) nil)))
      (let ((result (claude-repl--build-start-cmd "ws1")))
        (should (equal (plist-get result :cmd) "claude --dangerously-skip-permissions"))
        (should-not (plist-get result :sandboxed-p))))))

(ert-deftest claude-repl-test-build-start-cmd-sandbox-with-session ()
  "build-start-cmd for worktree sandbox with session should use sandbox script + --continue."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :active-env :sandbox)
    (claude-repl--ws-put "ws1" :sandbox
                         (make-claude-repl-instantiation :session-id "sess-123"))
    (claude-repl--ws-put "ws1" :worktree-p t)
    (claude-repl--ws-put "ws1" :project-dir "/home/user/project")
    (cl-letf (((symbol-function 'claude-repl--get-sandbox-image)
               (lambda (_ws) '(:image "img:latest" :script "/usr/bin/claude-sandbox"))))
      (let ((result (claude-repl--build-start-cmd "ws1")))
        (should (string-match-p "/usr/bin/claude-sandbox" (plist-get result :cmd)))
        (should (string-match-p "--continue" (plist-get result :cmd)))
        (should-not (string-match-p "--resume sess-123" (plist-get result :cmd)))
        (should (plist-get result :sandboxed-p))
        (should (equal (plist-get result :docker-image) "img:latest"))))))

(ert-deftest claude-repl-test-build-start-cmd-fork-session ()
  "build-start-cmd with fork-session-id should include --fork-session flag."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :active-env :bare-metal)
    (claude-repl--ws-put "ws1" :bare-metal (make-claude-repl-instantiation))
    (claude-repl--ws-put "ws1" :worktree-p nil)
    (claude-repl--ws-put "ws1" :project-dir "/home/user/personal/project")
    (claude-repl--ws-put "ws1" :fork-session-id "fork-abc")
    (cl-letf (((symbol-function 'claude-repl--get-sandbox-image)
               (lambda (_ws) nil)))
      (let ((result (claude-repl--build-start-cmd "ws1")))
        (should (string-match-p "--resume fork-abc --fork-session" (plist-get result :cmd)))
        (should (equal (plist-get result :fork-session-id) "fork-abc"))))))

(ert-deftest claude-repl-test-build-start-cmd-needs-build ()
  "build-start-cmd when sandbox needs-build should have nil docker-image and sandboxed-p false."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :active-env :sandbox)
    (claude-repl--ws-put "ws1" :sandbox (make-claude-repl-instantiation))
    (claude-repl--ws-put "ws1" :worktree-p t)
    (claude-repl--ws-put "ws1" :project-dir "/home/user/project")
    (cl-letf (((symbol-function 'claude-repl--get-sandbox-image)
               (lambda (_ws) '(:needs-build t :image "img:latest"))))
      (let ((result (claude-repl--build-start-cmd "ws1")))
        (should-not (plist-get result :docker-image))
        (should-not (plist-get result :sandboxed-p))))))

(ert-deftest claude-repl-test-build-start-cmd-nil-project-dir ()
  "build-start-cmd with personal :project-dir should produce a command."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :active-env :bare-metal)
    (claude-repl--ws-put "ws1" :bare-metal (make-claude-repl-instantiation))
    (claude-repl--ws-put "ws1" :worktree-p nil)
    (claude-repl--ws-put "ws1" :project-dir "/tmp/personal/project")
    (cl-letf (((symbol-function 'claude-repl--get-sandbox-image)
               (lambda (_ws) nil)))
      (let* ((result (claude-repl--build-start-cmd "ws1")))
        (should (stringp (plist-get result :cmd)))
        (should (string-match-p "claude" (plist-get result :cmd)))))))

(ert-deftest claude-repl-test-build-start-cmd-empty-session-id ()
  "build-start-cmd with nil session-id should not include --resume or --continue."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :active-env :bare-metal)
    (claude-repl--ws-put "ws1" :bare-metal (make-claude-repl-instantiation :session-id nil))
    (claude-repl--ws-put "ws1" :worktree-p nil)
    (claude-repl--ws-put "ws1" :project-dir "/home/user/personal/project")
    (cl-letf (((symbol-function 'claude-repl--get-sandbox-image)
               (lambda (_ws) nil)))
      (let ((result (claude-repl--build-start-cmd "ws1")))
        (should-not (string-match-p "--resume" (plist-get result :cmd)))
        (should-not (string-match-p "--continue" (plist-get result :cmd)))))))

;;;; ---- Tests: log-session-start ----

(ert-deftest claude-repl-test-log-session-start-all-fields ()
  "log-session-start should format message with all fields present."
  (let ((logged-msg nil))
    (cl-letf (((symbol-function 'claude-repl--log)
               (lambda (_ws fmt &rest args) (setq logged-msg (apply #'format fmt args)))))
      (claude-repl--log-session-start "ws1"
                                      '(:cmd "claude --resume abc"
                                        :session-id "abc"
                                        :fork-session-id "fork-1"
                                        :worktree-p t
                                        :active-env :sandbox))
      (should (string-match-p "ws=ws1" logged-msg))
      (should (string-match-p "session-id=abc" logged-msg))
      (should (string-match-p "fork-session-id=fork-1" logged-msg))
      (should (string-match-p "worktree=yes" logged-msg))
      (should (string-match-p "env=:sandbox" logged-msg)))))

(ert-deftest claude-repl-test-log-session-start-nil-optional-fields ()
  "log-session-start should handle nil session-id and fork-session-id."
  (let ((logged-msg nil))
    (cl-letf (((symbol-function 'claude-repl--log)
               (lambda (_ws fmt &rest args) (setq logged-msg (apply #'format fmt args)))))
      (claude-repl--log-session-start "ws1"
                                      '(:cmd "claude"
                                        :session-id nil
                                        :fork-session-id nil
                                        :worktree-p nil
                                        :active-env :bare-metal))
      (should (string-match-p "session-id=nil" logged-msg))
      (should (string-match-p "fork-session-id=nil" logged-msg)))))

(ert-deftest claude-repl-test-log-session-start-worktree-formatting ()
  "log-session-start should format worktree-p as yes/no strings."
  (let ((msg-yes nil)
        (msg-no nil))
    (cl-letf (((symbol-function 'claude-repl--log)
               (lambda (_ws fmt &rest args) (setq msg-yes (apply #'format fmt args)))))
      (claude-repl--log-session-start "ws1"
                                      '(:cmd "claude" :worktree-p t :active-env :sandbox
                                        :session-id nil :fork-session-id nil)))
    (cl-letf (((symbol-function 'claude-repl--log)
               (lambda (_ws fmt &rest args) (setq msg-no (apply #'format fmt args)))))
      (claude-repl--log-session-start "ws1"
                                      '(:cmd "claude" :worktree-p nil :active-env :bare-metal
                                        :session-id nil :fork-session-id nil)))
    (should (string-match-p "worktree=yes" msg-yes))
    (should (string-match-p "worktree=no" msg-no))))

;;;; ---- Tests: claude-running-p ----

(ert-deftest claude-repl-test-claude-running-p-explicit-ws ()
  "claude-running-p with explicit ws should delegate to vterm-process-alive-p."
  (claude-repl-test--with-clean-state
    (let ((checked-ws nil))
      (cl-letf (((symbol-function 'claude-repl--vterm-process-alive-p)
                 (lambda (ws) (setq checked-ws ws) t)))
        (should (claude-repl--claude-running-p "my-ws"))
        (should (equal checked-ws "my-ws"))))))

(ert-deftest claude-repl-test-claude-running-p-nil-ws-uses-current ()
  "claude-running-p with nil ws should fall back to +workspace-current-name."
  (claude-repl-test--with-clean-state
    (let ((checked-ws nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
                ((symbol-function 'claude-repl--vterm-process-alive-p)
                 (lambda (ws) (setq checked-ws ws) nil)))
        (should-not (claude-repl--claude-running-p))
        (should (equal checked-ws "current-ws"))))))

;;;; ---- Tests: session edge cases (status transitions .md) ----

(ert-deftest claude-repl-test-handle-claude-finished-second-notify-debounced ()
  "Two calls to handle-claude-finished within 2s should only produce one notification."
  (claude-repl-test--with-clean-state
    (let ((notify-count 0))
      (cl-letf (((symbol-function 'claude-repl--notify)
                 (lambda (&rest _) (cl-incf notify-count)))
                ((symbol-function 'claude-repl--do-refresh) #'ignore)
                ((symbol-function 'claude-repl--update-hide-overlay) #'ignore)
                ((symbol-function 'claude-repl--fix-vterm-scroll) #'ignore)
                ((symbol-function 'frame-focus-state) (lambda () nil))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat fn &rest args)
                   (apply fn args)))
                ((symbol-function '+workspace-current-name) (lambda () "other-ws"))
                ((symbol-function 'claude-repl--current-ws-p) (lambda (_ws) nil)))
        ;; Create a hidden vterm buffer for ws1
        (let ((vterm-buf (generate-new-buffer "*claude-panel-testfinish*")))
          (unwind-protect
              (progn
                (claude-repl--ws-put "ws1" :vterm-buffer vterm-buf)
                ;; First call — should notify
                (claude-repl--handle-claude-finished "ws1")
                (should (= notify-count 1))
                ;; Second call within 2s window — should be debounced
                (claude-repl--handle-claude-finished "ws1")
                (should (= notify-count 1)))
            (when (buffer-live-p vterm-buf)
              (kill-buffer vterm-buf))))))))

;;;; ---- Tests: set-session-id ----

(ert-deftest claude-repl-test-set-session-id-persists-to-disk ()
  "set-session-id mutates the active instantiation AND writes state to disk.
Persistence-on-capture is what makes a hook-delivered SID durable
through an Emacs crash — without it, the SID would only reach
.claude-repl-state at graceful teardown."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-set-sid-" t)))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws" :project-dir tmpdir)
            (claude-repl--ws-put "ws" :active-env :bare-metal)
            (claude-repl--ws-put "ws" :bare-metal (make-claude-repl-instantiation))
            (claude-repl--ws-put "ws" :sandbox (make-claude-repl-instantiation))
            (claude-repl--set-session-id "ws" "captured-sid")
            (let* ((file (claude-repl--state-file tmpdir))
                   (data (claude-repl--read-sexp-file file)))
              (should (equal (plist-get (plist-get data :bare-metal) :session-id)
                             "captured-sid"))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-set-session-id-no-project-dir-does-not-error ()
  "set-session-id is safe when :project-dir is nil; state-save logs and skips."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :active-env :bare-metal)
    (claude-repl--ws-put "ws" :bare-metal (make-claude-repl-instantiation))
    (claude-repl--ws-put "ws" :sandbox (make-claude-repl-instantiation))
    (claude-repl--set-session-id "ws" "captured-sid")
    (should (equal (claude-repl-instantiation-session-id
                    (claude-repl--ws-get "ws" :bare-metal))
                   "captured-sid"))))

;;; test-session.el ends here
