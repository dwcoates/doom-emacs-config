;;; test-magit.el --- ERT tests for magit.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for magit integration — including the :before advice that hides
;; Claude panels when RET-triggered actions run inside `magit-status-mode'.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-magit.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: claude-repl--magit-hide-panels-before-action ----

(ert-deftest claude-repl-test-magit-advice-hides-panels-in-magit-status ()
  "Advice hides panels when in `magit-status-mode' and panels are visible."
  (claude-repl-test--with-clean-state
    (let ((hide-calls 0))
      (cl-letf (((symbol-function 'claude-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'claude-repl--hide-panels)
                 (lambda () (cl-incf hide-calls))))
        (with-temp-buffer
          (setq major-mode 'magit-status-mode)
          (claude-repl--magit-hide-panels-before-action)
          (should (= hide-calls 1)))))))

(ert-deftest claude-repl-test-magit-advice-no-op-when-panels-hidden ()
  "Advice does not hide panels when they are not currently visible."
  (claude-repl-test--with-clean-state
    (let ((hide-calls 0))
      (cl-letf (((symbol-function 'claude-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'claude-repl--hide-panels)
                 (lambda () (cl-incf hide-calls))))
        (with-temp-buffer
          (setq major-mode 'magit-status-mode)
          (claude-repl--magit-hide-panels-before-action)
          (should (= hide-calls 0)))))))

(ert-deftest claude-repl-test-magit-advice-no-op-outside-magit-status ()
  "Advice does not hide panels when the caller's buffer is not `magit-status-mode'."
  (claude-repl-test--with-clean-state
    (let ((hide-calls 0))
      (cl-letf (((symbol-function 'claude-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'claude-repl--hide-panels)
                 (lambda () (cl-incf hide-calls))))
        (with-temp-buffer
          (setq major-mode 'magit-log-mode)
          (claude-repl--magit-hide-panels-before-action)
          (should (= hide-calls 0)))))))

(ert-deftest claude-repl-test-magit-advice-ignores-extra-args ()
  "Advice accepts and ignores any arguments passed through by `advice-add :before'."
  (claude-repl-test--with-clean-state
    (let ((hide-calls 0))
      (cl-letf (((symbol-function 'claude-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'claude-repl--hide-panels)
                 (lambda () (cl-incf hide-calls))))
        (with-temp-buffer
          (setq major-mode 'magit-status-mode)
          (claude-repl--magit-hide-panels-before-action 'arg1 "arg2" 42)
          (should (= hide-calls 1)))))))

;;;; ---- Tests: advice registration ----

(ert-deftest claude-repl-test-magit-advice-registered-on-visit-thing ()
  "`magit-visit-thing' has the hide-panels advice attached."
  (let ((found nil))
    (advice-mapc (lambda (fn _props)
                   (when (eq fn #'claude-repl--magit-hide-panels-before-action)
                     (setq found t)))
                 'magit-visit-thing)
    (should found)))

(ert-deftest claude-repl-test-magit-advice-registered-on-diff-visit-file ()
  "`magit-diff-visit-file' has the hide-panels advice attached."
  (let ((found nil))
    (advice-mapc (lambda (fn _props)
                   (when (eq fn #'claude-repl--magit-hide-panels-before-action)
                     (setq found t)))
                 'magit-diff-visit-file)
    (should found)))

(ert-deftest claude-repl-test-magit-advice-registered-on-diff-visit-worktree-file ()
  "`magit-diff-visit-worktree-file' has the hide-panels advice attached."
  (let ((found nil))
    (advice-mapc (lambda (fn _props)
                   (when (eq fn #'claude-repl--magit-hide-panels-before-action)
                     (setq found t)))
                 'magit-diff-visit-worktree-file)
    (should found)))

;;;; ---- Tests: claude-repl--gh-pr-url-for-branch ----

(ert-deftest claude-repl-test-gh-pr-url-for-branch-returns-url ()
  "Returns the URL emitted by `gh pr view --json url --jq .url'."
  (claude-repl-test--with-clean-state
    (let ((captured-cmd nil))
      (cl-letf (((symbol-function 'shell-command-to-string)
                 (lambda (cmd)
                   (setq captured-cmd cmd)
                   "https://github.com/ChessCom/repo/pull/42\n")))
        (should (equal (claude-repl--gh-pr-url-for-branch "/tmp/proj" "feat/x")
                       "https://github.com/ChessCom/repo/pull/42"))
        (should (string-match-p "gh pr view" captured-cmd))
        (should (string-match-p "feat/x" captured-cmd))
        (should (string-match-p "--json url" captured-cmd))
        (should (string-match-p "2>/dev/null" captured-cmd))))))

(ert-deftest claude-repl-test-gh-pr-url-for-branch-no-pr-returns-nil ()
  "Returns nil when `gh' produces empty output (no PR for branch)."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) "")))
      (should (null (claude-repl--gh-pr-url-for-branch "/tmp/proj" "feat/x"))))))

(ert-deftest claude-repl-test-gh-pr-url-for-branch-non-url-returns-nil ()
  "Returns nil when `gh' output isn't a URL (e.g. error text leaked)."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) "no pull requests found for branch \"feat/x\"")))
      (should (null (claude-repl--gh-pr-url-for-branch "/tmp/proj" "feat/x"))))))

(ert-deftest claude-repl-test-gh-pr-url-for-branch-shell-quotes-branch ()
  "Branch names with shell metacharacters are quoted, not interpolated."
  (claude-repl-test--with-clean-state
    (let ((captured-cmd nil))
      (cl-letf (((symbol-function 'shell-command-to-string)
                 (lambda (cmd) (setq captured-cmd cmd) "")))
        (claude-repl--gh-pr-url-for-branch "/tmp/proj" "feat;rm -rf /")
        (should (string-match-p "feat\\\\;rm" captured-cmd))))))

;;;; ---- Tests: +dwc/open-workspace-pr-in-browser ----

(ert-deftest claude-repl-test-open-workspace-pr-browses-resolved-url ()
  "Calls `browse-url' with the URL returned by the gh resolver."
  (claude-repl-test--with-clean-state
    (let ((browsed nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws-1"))
                ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'shell-command-to-string)
                 (lambda (_cmd) "feature-branch\n"))
                ((symbol-function 'claude-repl--gh-pr-url-for-branch)
                 (lambda (dir branch)
                   (should (equal dir "/tmp/proj"))
                   (should (equal branch "feature-branch"))
                   "https://github.com/ChessCom/repo/pull/7"))
                ((symbol-function 'browse-url)
                 (lambda (url) (setq browsed url))))
        (+dwc/open-workspace-pr-in-browser)
        (should (equal browsed "https://github.com/ChessCom/repo/pull/7"))))))

(ert-deftest claude-repl-test-open-workspace-pr-errors-when-no-pr ()
  "Signals `user-error' when no PR is associated with the branch."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws-1"))
              ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
              ((symbol-function 'shell-command-to-string) (lambda (_cmd) "main\n"))
              ((symbol-function 'claude-repl--gh-pr-url-for-branch)
               (lambda (_dir _branch) nil))
              ((symbol-function 'browse-url)
               (lambda (_url) (error "browse-url should not be called"))))
      (should-error (+dwc/open-workspace-pr-in-browser) :type 'user-error))))

(provide 'test-magit)
;;; test-magit.el ends here
