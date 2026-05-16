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

;;;; ---- Tests: +dwc/magit-status-workspace (always-replace-current-buffer) ----

(ert-deftest claude-repl-test-magit-status-workspace-calls-magit-status-with-ws-dir ()
  "Calls `magit-status' with the workspace's project directory."
  (claude-repl-test--with-clean-state
    (let ((magit-status-args nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                ((symbol-function 'magit-status)
                 (lambda (&rest args) (setq magit-status-args args))))
        (+dwc/magit-status-workspace)
        (should (equal magit-status-args '("/tmp/proj")))))))

(ert-deftest claude-repl-test-magit-status-workspace-never-splits ()
  "Never calls `split-window' -- magit always replaces the current buffer."
  (claude-repl-test--with-clean-state
    (let ((split-calls 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                ((symbol-function 'magit-status) #'ignore)
                ((symbol-function 'split-window)
                 (lambda (&rest _) (cl-incf split-calls) 'should-not-happen)))
        (+dwc/magit-status-workspace)
        (should (= split-calls 0))))))

(ert-deftest claude-repl-test-magit-status-workspace-clears-fullscreen-config ()
  "Clears any saved `:fullscreen-config' -- magit replacing the current
buffer means claude is no longer fullscreen."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :fullscreen-config 'fake-config)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
              ((symbol-function 'window-parameter) (lambda (_w _p) nil))
              ((symbol-function 'magit-status) #'ignore))
      (+dwc/magit-status-workspace)
      (should (null (claude-repl--ws-get "test-ws" :fullscreen-config))))))

(ert-deftest claude-repl-test-magit-status-workspace-binds-same-window-display-fn ()
  "Let-binds `magit-display-buffer-function' to the same-window helper
so `magit-status' replaces the current buffer instead of popping a new
window via the default traditional display behavior."
  (claude-repl-test--with-clean-state
    (let ((observed-fn nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                ((symbol-function 'magit-status)
                 (lambda (&rest _)
                   (setq observed-fn magit-display-buffer-function))))
        (+dwc/magit-status-workspace)
        (should (eq observed-fn
                    #'claude-repl--magit-display-buffer-same-window))))))

(ert-deftest claude-repl-test-magit-status-workspace-does-not-mutate-global-display-fn ()
  "Outside the let-binding, the global `magit-display-buffer-function'
is unchanged -- other magit buffers (diffs, logs) keep normal display."
  (claude-repl-test--with-clean-state
    (let ((magit-display-buffer-function 'global-default))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                ((symbol-function 'magit-status) #'ignore))
        (+dwc/magit-status-workspace)
        (should (eq magit-display-buffer-function 'global-default))))))

(ert-deftest claude-repl-test-magit-status-workspace-pops-out-of-side-window ()
  "When the selected window is a side window (e.g., the workspace drawer),
first calls `select-window' on `window-main-window' so magit replaces
the main buffer rather than failing on the dedicated side window."
  (claude-repl-test--with-clean-state
    (let ((selected-window-arg nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'window-parameter)
                 (lambda (_win param)
                   (and (eq param 'window-side) 'left)))
                ((symbol-function 'window-main-window) (lambda (&rest _) 'fake-main))
                ((symbol-function 'select-window)
                 (lambda (w) (setq selected-window-arg w)))
                ((symbol-function 'magit-status) #'ignore))
        (+dwc/magit-status-workspace)
        (should (eq selected-window-arg 'fake-main))))))

(ert-deftest claude-repl-test-magit-status-workspace-skips-pop-when-not-side-window ()
  "When the selected window is NOT a side window, does NOT call
`select-window' -- magit replaces the current buffer in place."
  (claude-repl-test--with-clean-state
    (let ((select-calls 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'window-parameter)
                 (lambda (_win _param) nil))
                ((symbol-function 'select-window)
                 (lambda (_w) (cl-incf select-calls)))
                ((symbol-function 'magit-status) #'ignore))
        (+dwc/magit-status-workspace)
        (should (= select-calls 0))))))

;;;; ---- Tests: claude-repl--magit-display-buffer-same-window ----

(ert-deftest claude-repl-test-magit-display-buffer-same-window-uses-display-buffer-same-window ()
  "Calls `display-buffer' with the `display-buffer-same-window' action
so the buffer replaces the selected window's content."
  (let ((captured-action nil)
        (captured-buffer nil))
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (buf action)
                 (setq captured-buffer buf
                       captured-action action)
                 'fake-win)))
      (should (eq (claude-repl--magit-display-buffer-same-window 'fake-buf)
                  'fake-win))
      (should (eq captured-buffer 'fake-buf))
      (should (equal captured-action '(display-buffer-same-window))))))

;;;; ---- Tests: claude-repl-magit-show-tags-header (defcustom default) ----

(ert-deftest claude-repl-test-magit-show-tags-header-defaults-to-nil ()
  "Tags header is hidden by default — the option ships off."
  (should (null (default-value 'claude-repl-magit-show-tags-header))))

;;;; ---- Tests: claude-repl--magit-apply-tags-header-visibility ----

(ert-deftest claude-repl-test-magit-apply-tags-header-removes-when-nil ()
  "When the option is nil, `magit-insert-tags-header' is removed from the hook."
  (let ((claude-repl-magit-show-tags-header nil)
        (magit-status-headers-hook (list #'magit-insert-error-header
                                          #'magit-insert-tags-header)))
    (cl-letf (((symbol-function 'magit-insert-error-header)
               (lambda (&rest _) nil)))
      (claude-repl--magit-apply-tags-header-visibility)
      (should-not (memq #'magit-insert-tags-header magit-status-headers-hook))
      ;; Other hook members are not disturbed.
      (should (memq #'magit-insert-error-header magit-status-headers-hook)))))

(ert-deftest claude-repl-test-magit-apply-tags-header-adds-when-non-nil ()
  "When the option is non-nil, `magit-insert-tags-header' is added to the hook."
  (let ((claude-repl-magit-show-tags-header t)
        (magit-status-headers-hook nil))
    (claude-repl--magit-apply-tags-header-visibility)
    (should (memq #'magit-insert-tags-header magit-status-headers-hook))))

(ert-deftest claude-repl-test-magit-apply-tags-header-idempotent-on-add ()
  "Adding when already present must not duplicate the hook entry."
  (let ((claude-repl-magit-show-tags-header t)
        (magit-status-headers-hook (list #'magit-insert-tags-header)))
    (claude-repl--magit-apply-tags-header-visibility)
    (should (equal (cl-count #'magit-insert-tags-header
                             magit-status-headers-hook)
                   1))))

(ert-deftest claude-repl-test-magit-apply-tags-header-idempotent-on-remove ()
  "Removing when already absent is a no-op."
  (let ((claude-repl-magit-show-tags-header nil)
        (magit-status-headers-hook nil))
    (claude-repl--magit-apply-tags-header-visibility)
    (should (null magit-status-headers-hook))))

;;;; ---- Tests: +dwc/magit-toggle-tags-header ----

(ert-deftest claude-repl-test-magit-toggle-flips-from-nil-to-t ()
  "Toggle flips the option from nil to t and adds the hook entry."
  (let ((claude-repl-magit-show-tags-header nil)
        (magit-status-headers-hook nil))
    (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) nil))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (+dwc/magit-toggle-tags-header)
      (should (eq claude-repl-magit-show-tags-header t))
      (should (memq #'magit-insert-tags-header magit-status-headers-hook)))))

(ert-deftest claude-repl-test-magit-toggle-flips-from-t-to-nil ()
  "Toggle flips the option from t to nil and removes the hook entry."
  (let ((claude-repl-magit-show-tags-header t)
        (magit-status-headers-hook (list #'magit-insert-tags-header)))
    (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) nil))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (+dwc/magit-toggle-tags-header)
      (should (null claude-repl-magit-show-tags-header))
      (should-not (memq #'magit-insert-tags-header magit-status-headers-hook)))))

(ert-deftest claude-repl-test-magit-toggle-refreshes-in-magit-status-mode ()
  "Toggle calls `magit-refresh' when invoked from a `magit-status-mode' buffer."
  (let ((claude-repl-magit-show-tags-header nil)
        (magit-status-headers-hook nil)
        (refresh-calls 0))
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (mode) (eq mode 'magit-status-mode)))
              ((symbol-function 'magit-refresh)
               (lambda (&rest _) (cl-incf refresh-calls)))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (+dwc/magit-toggle-tags-header)
      (should (= refresh-calls 1)))))

(ert-deftest claude-repl-test-magit-toggle-no-refresh-outside-magit-status ()
  "Toggle does NOT call `magit-refresh' when invoked outside magit-status."
  (let ((claude-repl-magit-show-tags-header nil)
        (magit-status-headers-hook nil)
        (refresh-calls 0))
    (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) nil))
              ((symbol-function 'magit-refresh)
               (lambda (&rest _) (cl-incf refresh-calls)))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (+dwc/magit-toggle-tags-header)
      (should (= refresh-calls 0)))))

(ert-deftest claude-repl-test-magit-toggle-is-interactive ()
  "Toggle is an interactive command so it can be bound to a key."
  (should (commandp #'+dwc/magit-toggle-tags-header)))

(provide 'test-magit)
;;; test-magit.el ends here
