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

;;;; ---- Tests: agent-repl--magit-hide-panels-before-action ----

(ert-deftest agent-repl-test-magit-advice-hides-panels-in-magit-status ()
  "Advice hides panels when in `magit-status-mode' and panels are visible."
  (agent-repl-test--with-clean-state
    (let ((hide-calls 0))
      (cl-letf (((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'agent-repl--hide-panels)
                 (lambda () (cl-incf hide-calls))))
        (with-temp-buffer
          (setq major-mode 'magit-status-mode)
          (agent-repl--magit-hide-panels-before-action)
          (should (= hide-calls 1)))))))

(ert-deftest agent-repl-test-magit-advice-no-op-when-panels-hidden ()
  "Advice does not hide panels when they are not currently visible."
  (agent-repl-test--with-clean-state
    (let ((hide-calls 0))
      (cl-letf (((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'agent-repl--hide-panels)
                 (lambda () (cl-incf hide-calls))))
        (with-temp-buffer
          (setq major-mode 'magit-status-mode)
          (agent-repl--magit-hide-panels-before-action)
          (should (= hide-calls 0)))))))

(ert-deftest agent-repl-test-magit-advice-no-op-outside-magit-status ()
  "Advice does not hide panels when the caller's buffer is not `magit-status-mode'."
  (agent-repl-test--with-clean-state
    (let ((hide-calls 0))
      (cl-letf (((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'agent-repl--hide-panels)
                 (lambda () (cl-incf hide-calls))))
        (with-temp-buffer
          (setq major-mode 'magit-log-mode)
          (agent-repl--magit-hide-panels-before-action)
          (should (= hide-calls 0)))))))

(ert-deftest agent-repl-test-magit-advice-ignores-extra-args ()
  "Advice accepts and ignores any arguments passed through by `advice-add :before'."
  (agent-repl-test--with-clean-state
    (let ((hide-calls 0))
      (cl-letf (((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'agent-repl--hide-panels)
                 (lambda () (cl-incf hide-calls))))
        (with-temp-buffer
          (setq major-mode 'magit-status-mode)
          (agent-repl--magit-hide-panels-before-action 'arg1 "arg2" 42)
          (should (= hide-calls 1)))))))

;;;; ---- Tests: advice registration ----

(ert-deftest agent-repl-test-magit-advice-registered-on-visit-thing ()
  "`magit-visit-thing' has the hide-panels advice attached."
  (let ((found nil))
    (advice-mapc (lambda (fn _props)
                   (when (eq fn #'agent-repl--magit-hide-panels-before-action)
                     (setq found t)))
                 'magit-visit-thing)
    (should found)))

(ert-deftest agent-repl-test-magit-advice-registered-on-diff-visit-file ()
  "`magit-diff-visit-file' has the hide-panels advice attached."
  (let ((found nil))
    (advice-mapc (lambda (fn _props)
                   (when (eq fn #'agent-repl--magit-hide-panels-before-action)
                     (setq found t)))
                 'magit-diff-visit-file)
    (should found)))

(ert-deftest agent-repl-test-magit-advice-registered-on-diff-visit-worktree-file ()
  "`magit-diff-visit-worktree-file' has the hide-panels advice attached."
  (let ((found nil))
    (advice-mapc (lambda (fn _props)
                   (when (eq fn #'agent-repl--magit-hide-panels-before-action)
                     (setq found t)))
                 'magit-diff-visit-worktree-file)
    (should found)))

;;;; ---- Tests: agent-repl--gh-pr-url-for-branch ----
;;
;; Routes through `agent-repl--gh-string-quiet' (the external-boundary
;; wrapper for the GitHub CLI).  Tests mock the wrapper rather than
;; `shell-command-to-string' wholesale, per AGENTS.md.

(ert-deftest agent-repl-test-gh-pr-url-for-branch-returns-url ()
  "Returns the URL emitted by `gh pr view --json url --jq .url'."
  (agent-repl-test--with-clean-state
    (let ((captured-args nil))
      (cl-letf (((symbol-function 'agent-repl--gh-string-quiet)
                 (lambda (&rest args)
                   (setq captured-args args)
                   "https://github.com/ChessCom/repo/pull/42")))
        (should (equal (agent-repl--gh-pr-url-for-branch "/tmp/proj" "feat/x")
                       "https://github.com/ChessCom/repo/pull/42"))
        (should (equal captured-args
                       '("pr" "view" "feat/x" "--json" "url" "--jq" ".url")))))))

(ert-deftest agent-repl-test-gh-pr-url-for-branch-no-pr-returns-nil ()
  "Returns nil when `gh' produces empty output (no PR for branch)."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--gh-string-quiet)
               (lambda (&rest _args) "")))
      (should (null (agent-repl--gh-pr-url-for-branch "/tmp/proj" "feat/x"))))))

(ert-deftest agent-repl-test-gh-pr-url-for-branch-non-url-returns-nil ()
  "Returns nil when `gh' output isn't a URL (e.g. error text leaked)."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--gh-string-quiet)
               (lambda (&rest _args) "no pull requests found for branch \"feat/x\"")))
      (should (null (agent-repl--gh-pr-url-for-branch "/tmp/proj" "feat/x"))))))

(ert-deftest agent-repl-test-gh-pr-url-for-branch-passes-branch-as-arg ()
  "Branch names are passed as a discrete arg to the wrapper, not interpolated
into a shell string — the wrapper handles shell-quoting internally, so
metacharacters are never reachable as shell syntax."
  (agent-repl-test--with-clean-state
    (let ((captured-args nil))
      (cl-letf (((symbol-function 'agent-repl--gh-string-quiet)
                 (lambda (&rest args) (setq captured-args args) "")))
        (agent-repl--gh-pr-url-for-branch "/tmp/proj" "feat;rm -rf /")
        ;; The metacharacter-bearing branch name is the 3rd arg, exactly
        ;; as-is — never spliced into a shell command at the production-
        ;; code level.
        (should (equal (nth 2 captured-args) "feat;rm -rf /"))))))

;;;; ---- Tests: +dwc/open-workspace-pr-in-browser ----

(ert-deftest agent-repl-test-open-workspace-pr-browses-resolved-url ()
  "Calls `browse-url' with the URL returned by the gh resolver."
  (agent-repl-test--with-clean-state
    (let ((browsed nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws-1"))
                ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'agent-repl--git-string)
                 (lambda (&rest args)
                   (should (equal args '("rev-parse" "--abbrev-ref" "HEAD")))
                   "feature-branch"))
                ((symbol-function 'agent-repl--gh-pr-url-for-branch)
                 (lambda (dir branch)
                   (should (equal dir "/tmp/proj"))
                   (should (equal branch "feature-branch"))
                   "https://github.com/ChessCom/repo/pull/7"))
                ((symbol-function 'browse-url)
                 (lambda (url) (setq browsed url))))
        (+dwc/open-workspace-pr-in-browser)
        (should (equal browsed "https://github.com/ChessCom/repo/pull/7"))))))

(ert-deftest agent-repl-test-open-workspace-pr-errors-when-no-pr ()
  "Signals `user-error' when no PR is associated with the branch."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws-1"))
              ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
              ((symbol-function 'agent-repl--git-string)
               (lambda (&rest _args) "main"))
              ((symbol-function 'agent-repl--gh-pr-url-for-branch)
               (lambda (_dir _branch) nil))
              ((symbol-function 'browse-url)
               (lambda (_url) (error "browse-url should not be called"))))
      (should-error (+dwc/open-workspace-pr-in-browser) :type 'user-error))))

;;;; ---- Tests: +dwc/magit-status-workspace (always-replace-current-buffer) ----

(ert-deftest agent-repl-test-magit-status-workspace-calls-magit-status-with-ws-dir ()
  "Calls `magit-status' with the workspace's project directory."
  (agent-repl-test--with-clean-state
    (let ((magit-status-args nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                ((symbol-function 'magit-status)
                 (lambda (&rest args) (setq magit-status-args args))))
        (+dwc/magit-status-workspace)
        (should (equal magit-status-args '("/tmp/proj")))))))

(ert-deftest agent-repl-test-magit-status-workspace-never-splits ()
  "Never calls `split-window' -- magit always replaces the current buffer."
  (agent-repl-test--with-clean-state
    (let ((split-calls 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                ((symbol-function 'magit-status) #'ignore)
                ((symbol-function 'split-window)
                 (lambda (&rest _) (cl-incf split-calls) 'should-not-happen)))
        (+dwc/magit-status-workspace)
        (should (= split-calls 0))))))

(ert-deftest agent-repl-test-magit-status-workspace-clears-fullscreen-config ()
  "Clears any saved `:fullscreen-config' -- magit replacing the current
buffer means claude is no longer fullscreen."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :project-dir "/tmp/proj")
    (agent-repl--ws-put "test-ws" :fullscreen-config 'fake-config)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'window-parameter) (lambda (_w _p) nil))
              ((symbol-function 'magit-status) #'ignore))
      (+dwc/magit-status-workspace)
      (should (null (agent-repl--ws-get "test-ws" :fullscreen-config))))))

(ert-deftest agent-repl-test-magit-status-workspace-binds-same-window-display-fn ()
  "Let-binds `magit-display-buffer-function' to the same-window helper
so `magit-status' replaces the current buffer instead of popping a new
window via the default traditional display behavior."
  (agent-repl-test--with-clean-state
    (let ((observed-fn nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                ((symbol-function 'magit-status)
                 (lambda (&rest _)
                   (setq observed-fn magit-display-buffer-function))))
        (+dwc/magit-status-workspace)
        (should (eq observed-fn
                    #'agent-repl--magit-display-buffer-same-window))))))

(ert-deftest agent-repl-test-magit-status-workspace-does-not-mutate-global-display-fn ()
  "Outside the let-binding, the global `magit-display-buffer-function'
is unchanged -- other magit buffers (diffs, logs) keep normal display."
  (agent-repl-test--with-clean-state
    (let ((magit-display-buffer-function 'global-default))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                ((symbol-function 'magit-status) #'ignore))
        (+dwc/magit-status-workspace)
        (should (eq magit-display-buffer-function 'global-default))))))

(ert-deftest agent-repl-test-magit-status-workspace-pops-out-of-side-window ()
  "When the selected window is a side window, first calls
`select-window' on `window-main-window' so magit replaces the main
buffer rather than failing on the dedicated side window."
  (agent-repl-test--with-clean-state
    (let ((selected-window-arg nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'window-parameter)
                 (lambda (_win param)
                   (and (eq param 'window-side) 'left)))
                ((symbol-function 'window-main-window) (lambda (&rest _) 'fake-main))
                ((symbol-function 'select-window)
                 (lambda (w) (setq selected-window-arg w)))
                ((symbol-function 'magit-status) #'ignore))
        (+dwc/magit-status-workspace)
        (should (eq selected-window-arg 'fake-main))))))

(ert-deftest agent-repl-test-magit-status-workspace-skips-pop-when-not-side-window ()
  "When the selected window is NOT a side window, does NOT call
`select-window' -- magit replaces the current buffer in place."
  (agent-repl-test--with-clean-state
    (let ((select-calls 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'window-parameter)
                 (lambda (_win _param) nil))
                ((symbol-function 'select-window)
                 (lambda (_w) (cl-incf select-calls)))
                ((symbol-function 'magit-status) #'ignore))
        (+dwc/magit-status-workspace)
        (should (= select-calls 0))))))

(ert-deftest agent-repl-test-magit-status-workspace-falls-back-to-default-directory ()
  "When the workspace is untracked (agent-repl--ws-dir errors), falls back
to `default-directory' so magit still opens without signalling.
Repro: SPC g g from the main \"doom\" workspace or after a workspace's
hash entry has been nuked."
  (agent-repl-test--with-clean-state
    (let ((magit-status-args nil)
          (default-directory "/tmp/fallback-dir/"))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "untracked-ws"))
                ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                ((symbol-function 'magit-status)
                 (lambda (&rest args) (setq magit-status-args args))))
        (+dwc/magit-status-workspace)
        (should (equal magit-status-args '("/tmp/fallback-dir/")))))))

(ert-deftest agent-repl-test-magit-status-workspace-no-stub-when-untracked ()
  "When the workspace is untracked, does NOT call `agent-repl--ws-put'
so no STUB-CREATE entry leaks into the workspace state table."
  (agent-repl-test--with-clean-state
    (let ((ws-put-calls 0)
          (default-directory "/tmp/fallback-dir/"))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "untracked-ws"))
                ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                ((symbol-function 'agent-repl--ws-put)
                 (lambda (&rest _) (cl-incf ws-put-calls)))
                ((symbol-function 'magit-status) #'ignore))
        (+dwc/magit-status-workspace)
        (should (= ws-put-calls 0))))))

(ert-deftest agent-repl-test-magit-status-workspace-still-clears-fullscreen-when-tracked ()
  "When the workspace IS tracked, still clears `:fullscreen-config'.
Guards against the untracked-fallback regressing the tracked path."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :project-dir "/tmp/proj")
    (agent-repl--ws-put "test-ws" :fullscreen-config 'fake-config)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'window-parameter) (lambda (_w _p) nil))
              ((symbol-function 'magit-status) #'ignore))
      (+dwc/magit-status-workspace)
      (should (null (agent-repl--ws-get "test-ws" :fullscreen-config))))))

;;;; ---- Tests: +dwc/magit-status-workspace (fullscreen panel handling) ----

(ert-deftest agent-repl-test-magit-status-workspace-fullscreen-closes-input-window ()
  "When the panels are visible, closes the input window so magit can fill the webview window."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :project-dir "/tmp/proj")
    (agent-repl--ws-put "test-ws" :input-buffer 'fake-input-buf)
    (agent-repl--ws-put "test-ws" :frontend-buffer 'fake-webview-buf)
    (let ((closed-bufs '()))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'agent-repl--close-buffer-window)
                 (lambda (buf) (push buf closed-bufs)))
                ((symbol-function 'get-buffer-window)
                 (lambda (_buf) 'fake-webview-win))
                ((symbol-function 'set-window-dedicated-p) #'ignore)
                ((symbol-function 'select-window) #'ignore)
                ((symbol-function 'magit-status) #'ignore))
        (+dwc/magit-status-workspace)
        (should (memq 'fake-input-buf closed-bufs))))))

(ert-deftest agent-repl-test-magit-status-workspace-fullscreen-undedicates-webview-window ()
  "When the panels are visible, un-dedicates the webview window so same-window display succeeds."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :project-dir "/tmp/proj")
    (agent-repl--ws-put "test-ws" :input-buffer 'fake-input-buf)
    (agent-repl--ws-put "test-ws" :frontend-buffer 'fake-webview-buf)
    (let ((dedicate-args nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'agent-repl--close-buffer-window) #'ignore)
                ((symbol-function 'get-buffer-window)
                 (lambda (_buf) 'fake-webview-win))
                ((symbol-function 'set-window-dedicated-p)
                 (lambda (win dedicated) (setq dedicate-args (list win dedicated))))
                ((symbol-function 'select-window) #'ignore)
                ((symbol-function 'magit-status) #'ignore))
        (+dwc/magit-status-workspace)
        (should (equal dedicate-args '(fake-webview-win nil)))))))

(ert-deftest agent-repl-test-magit-status-workspace-fullscreen-selects-webview-window ()
  "When the panels are visible, selects the webview window before calling magit-status."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :project-dir "/tmp/proj")
    (agent-repl--ws-put "test-ws" :input-buffer 'fake-input-buf)
    (agent-repl--ws-put "test-ws" :frontend-buffer 'fake-webview-buf)
    (let ((selected-wins '()))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'agent-repl--close-buffer-window) #'ignore)
                ((symbol-function 'get-buffer-window)
                 (lambda (_buf) 'fake-webview-win))
                ((symbol-function 'set-window-dedicated-p) #'ignore)
                ((symbol-function 'select-window)
                 (lambda (w) (push w selected-wins)))
                ((symbol-function 'magit-status) #'ignore))
        (+dwc/magit-status-workspace)
        (should (memq 'fake-webview-win selected-wins))))))

(ert-deftest agent-repl-test-magit-status-workspace-not-fullscreen-no-panel-close ()
  "When the panels are NOT visible, does not close the input window or un-dedicate the webview."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :project-dir "/tmp/proj")
    (agent-repl--ws-put "test-ws" :input-buffer 'fake-input-buf)
    (let ((close-calls 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'agent-repl--close-buffer-window)
                 (lambda (_buf) (cl-incf close-calls)))
                ((symbol-function 'magit-status) #'ignore))
        (+dwc/magit-status-workspace)
        (should (= close-calls 0))))))

(ert-deftest agent-repl-test-magit-status-workspace-fullscreen-no-input-buf-no-error ()
  "When the panels are visible but no input buffer stored, does not error and skips close."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :project-dir "/tmp/proj")
    ;; :input-buffer and :frontend-buffer left nil
    (let ((close-calls 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'agent-repl--close-buffer-window)
                 (lambda (_buf) (cl-incf close-calls)))
                ((symbol-function 'magit-status) #'ignore))
        (+dwc/magit-status-workspace)
        (should (= close-calls 0))))))

;;;; ---- Tests: agent-repl--magit-display-buffer-same-window ----

(ert-deftest agent-repl-test-magit-display-buffer-same-window-uses-display-buffer-same-window ()
  "Calls `display-buffer' with the `display-buffer-same-window' action
so the buffer replaces the selected window's content."
  (let ((captured-action nil)
        (captured-buffer nil))
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (buf action)
                 (setq captured-buffer buf
                       captured-action action)
                 'fake-win)))
      (should (eq (agent-repl--magit-display-buffer-same-window 'fake-buf)
                  'fake-win))
      (should (eq captured-buffer 'fake-buf))
      (should (equal captured-action '(display-buffer-same-window))))))

;;;; ---- Tests: agent-repl--magit-status-same-window ----

(ert-deftest agent-repl-test-magit-status-same-window-calls-magit-status-with-dir ()
  "Passes DIR straight through to `magit-status'."
  (let ((magit-status-args nil))
    (cl-letf (((symbol-function 'magit-status)
               (lambda (&rest args) (setq magit-status-args args))))
      (agent-repl--magit-status-same-window "/tmp/proj")
      (should (equal magit-status-args '("/tmp/proj"))))))

(ert-deftest agent-repl-test-magit-status-same-window-binds-same-window-display-fn ()
  "Binds `magit-display-buffer-function' to the same-window helper for the
`magit-status' call, so the status buffer replaces the current window
instead of splitting via Doom's traditional display behavior."
  (let ((observed-fn nil))
    (cl-letf (((symbol-function 'magit-status)
               (lambda (&rest _)
                 (setq observed-fn magit-display-buffer-function))))
      (agent-repl--magit-status-same-window "/tmp/proj")
      (should (eq observed-fn #'agent-repl--magit-display-buffer-same-window)))))

(ert-deftest agent-repl-test-magit-status-same-window-does-not-mutate-global-display-fn ()
  "Restores the global `magit-display-buffer-function' after returning, so
other magit buffers (diffs, logs) keep their normal display behavior."
  (let ((magit-display-buffer-function 'global-default))
    (cl-letf (((symbol-function 'magit-status) #'ignore))
      (agent-repl--magit-status-same-window "/tmp/proj")
      (should (eq magit-display-buffer-function 'global-default)))))

(ert-deftest agent-repl-test-magit-status-same-window-loads-magit-before-binding ()
  "Loads magit (noerror require) BEFORE let-binding the display function.
Without the preload, a fresh session's first call binds
`magit-display-buffer-function' lexically (the var is not yet special),
and magit's own `defvar' then aborts magit's autoload mid-file —
the first-restored-workspace splash-screen bug."
  (let ((order nil))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _file noerror)
                 (push (list 'require feature noerror) order)
                 nil))
              ((symbol-function 'magit-status)
               (lambda (&rest _) (push '(status) order))))
      (agent-repl--magit-status-same-window "/tmp/proj")
      (should (equal (nreverse order)
                     '((require magit t) (status)))))))

;;;; ---- Tests: agent-repl-magit-show-tags-in-log (defcustom default) ----

(ert-deftest agent-repl-test-magit-show-tags-in-log-defaults-to-nil ()
  "Commit-list tag refs are hidden by default — the option ships off."
  (should (null (default-value 'agent-repl-magit-show-tags-in-log))))

;;;; ---- Tests: agent-repl--magit-strip-tag-refs (filter-args advice) ----

(ert-deftest agent-repl-test-magit-strip-tag-refs-noop-when-shown ()
  "Returns ARGS unchanged when `agent-repl-magit-show-tags-in-log' is non-nil."
  (let ((agent-repl-magit-show-tags-in-log t))
    (should (equal (agent-repl--magit-strip-tag-refs
                    '("HEAD -> master, tag: v1.0, origin/master"))
                   '("HEAD -> master, tag: v1.0, origin/master")))))

(ert-deftest agent-repl-test-magit-strip-tag-refs-strips-trailing-tag ()
  "Strips `, tag: NAME' when the tag follows another ref."
  (let ((agent-repl-magit-show-tags-in-log nil))
    (should (equal (agent-repl--magit-strip-tag-refs
                    '("HEAD -> master, tag: v1.0"))
                   '("HEAD -> master")))))

(ert-deftest agent-repl-test-magit-strip-tag-refs-strips-leading-tag ()
  "Strips `tag: NAME, ' when the tag precedes another ref."
  (let ((agent-repl-magit-show-tags-in-log nil))
    (should (equal (agent-repl--magit-strip-tag-refs
                    '("tag: v1.0, master"))
                   '("master")))))

(ert-deftest agent-repl-test-magit-strip-tag-refs-strips-middle-tag ()
  "Strips a tag entry sandwiched between two other refs."
  (let ((agent-repl-magit-show-tags-in-log nil))
    (should (equal (agent-repl--magit-strip-tag-refs
                    '("HEAD -> master, tag: v1.0, origin/master"))
                   '("HEAD -> master, origin/master")))))

(ert-deftest agent-repl-test-magit-strip-tag-refs-strips-solo-tag ()
  "Strips a bare `tag: NAME' with no surrounding refs to an empty string."
  (let ((agent-repl-magit-show-tags-in-log nil))
    (should (equal (agent-repl--magit-strip-tag-refs '("tag: v1.0"))
                   '("")))))

(ert-deftest agent-repl-test-magit-strip-tag-refs-strips-multiple-tags ()
  "Strips multiple tag entries in a single refs string."
  (let ((agent-repl-magit-show-tags-in-log nil))
    (should (equal (agent-repl--magit-strip-tag-refs
                    '("tag: v1.0, tag: v2.0, master"))
                   '("master")))))

(ert-deftest agent-repl-test-magit-strip-tag-refs-handles-full-decoration ()
  "Strips `tag: refs/tags/NAME' (the `--decorate=full' form magit uses)."
  (let ((agent-repl-magit-show-tags-in-log nil))
    (should (equal (agent-repl--magit-strip-tag-refs
                    '("HEAD -> refs/heads/master, tag: refs/tags/v1.0, refs/remotes/origin/master"))
                   '("HEAD -> refs/heads/master, refs/remotes/origin/master")))))

(ert-deftest agent-repl-test-magit-strip-tag-refs-leaves-non-tag-refs-alone ()
  "Branches, HEAD pointers, and remotes pass through untouched."
  (let ((agent-repl-magit-show-tags-in-log nil))
    (should (equal (agent-repl--magit-strip-tag-refs
                    '("HEAD -> master, origin/master, upstream/master"))
                   '("HEAD -> master, origin/master, upstream/master")))))

(ert-deftest agent-repl-test-magit-strip-tag-refs-handles-nil-args ()
  "Returns nil when ARGS is nil — never errors on empty arg lists."
  (let ((agent-repl-magit-show-tags-in-log nil))
    (should (null (agent-repl--magit-strip-tag-refs nil)))))

(ert-deftest agent-repl-test-magit-strip-tag-refs-handles-non-string-arg ()
  "Returns ARGS unchanged when the first arg isn't a string."
  (let ((agent-repl-magit-show-tags-in-log nil))
    (should (equal (agent-repl--magit-strip-tag-refs '(nil))
                   '(nil)))))

;;;; ---- Tests: advice registration on `magit-format-ref-labels' ----

(ert-deftest agent-repl-test-magit-strip-tag-refs-advice-registered ()
  "`magit-format-ref-labels' has the strip-tag-refs filter-args advice attached."
  (let ((found nil))
    (advice-mapc (lambda (fn _props)
                   (when (eq fn #'agent-repl--magit-strip-tag-refs)
                     (setq found t)))
                 'magit-format-ref-labels)
    (should found)))

;;;; ---- Tests: +dwc/magit-toggle-tags-in-log ----

(ert-deftest agent-repl-test-magit-toggle-tags-in-log-flips-from-nil-to-t ()
  "Toggle flips the option from nil to t."
  (let ((agent-repl-magit-show-tags-in-log nil))
    (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) nil))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (+dwc/magit-toggle-tags-in-log)
      (should (eq agent-repl-magit-show-tags-in-log t)))))

(ert-deftest agent-repl-test-magit-toggle-tags-in-log-flips-from-t-to-nil ()
  "Toggle flips the option from t to nil."
  (let ((agent-repl-magit-show-tags-in-log t))
    (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) nil))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (+dwc/magit-toggle-tags-in-log)
      (should (null agent-repl-magit-show-tags-in-log)))))

(ert-deftest agent-repl-test-magit-toggle-tags-in-log-refreshes-in-magit-mode ()
  "Toggle calls `magit-refresh' when invoked from a `magit-mode' buffer."
  (let ((agent-repl-magit-show-tags-in-log nil)
        (refresh-calls 0))
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (mode) (eq mode 'magit-mode)))
              ((symbol-function 'magit-refresh)
               (lambda (&rest _) (cl-incf refresh-calls)))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (+dwc/magit-toggle-tags-in-log)
      (should (= refresh-calls 1)))))

(ert-deftest agent-repl-test-magit-toggle-tags-in-log-no-refresh-outside-magit ()
  "Toggle does NOT call `magit-refresh' when invoked outside any magit buffer."
  (let ((agent-repl-magit-show-tags-in-log nil)
        (refresh-calls 0))
    (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) nil))
              ((symbol-function 'magit-refresh)
               (lambda (&rest _) (cl-incf refresh-calls)))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (+dwc/magit-toggle-tags-in-log)
      (should (= refresh-calls 0)))))

(ert-deftest agent-repl-test-magit-toggle-tags-in-log-is-interactive ()
  "Toggle is an interactive command so it can be bound to a key."
  (should (commandp #'+dwc/magit-toggle-tags-in-log)))

;;;; ---- Tests: agent-repl-magit-merge-base-ref (defcustom default) ----

(ert-deftest agent-repl-test-magit-merge-base-ref-defaults-to-nil ()
  "The merge-base base ref defaults to nil — meaning the repo main branch."
  (should (null (default-value 'agent-repl-magit-merge-base-ref))))

;;;; ---- Tests: agent-repl--magit-merge-base-ref (ref resolution) ----

(ert-deftest agent-repl-test-magit-merge-base-ref-uses-explicit-string ()
  "Returns `agent-repl-magit-merge-base-ref' verbatim when set to a string."
  (let ((agent-repl-magit-merge-base-ref "origin/master"))
    (cl-letf (((symbol-function 'magit-main-branch)
               (lambda () (error "magit-main-branch should not be consulted"))))
      (should (equal (agent-repl--magit-merge-base-ref) "origin/master")))))

(ert-deftest agent-repl-test-magit-merge-base-ref-falls-back-to-main-branch ()
  "Falls back to `magit-main-branch' when the option is nil."
  (let ((agent-repl-magit-merge-base-ref nil))
    (cl-letf (((symbol-function 'magit-main-branch) (lambda () "master")))
      (should (equal (agent-repl--magit-merge-base-ref) "master")))))

(ert-deftest agent-repl-test-magit-merge-base-ref-empty-string-falls-back ()
  "An empty-string option is treated as unset and falls back to the main branch."
  (let ((agent-repl-magit-merge-base-ref ""))
    (cl-letf (((symbol-function 'magit-main-branch) (lambda () "main")))
      (should (equal (agent-repl--magit-merge-base-ref) "main")))))

(ert-deftest agent-repl-test-magit-merge-base-ref-nil-when-no-main-branch ()
  "Returns nil when the option is nil and no main branch exists."
  (let ((agent-repl-magit-merge-base-ref nil))
    (cl-letf (((symbol-function 'magit-main-branch) (lambda () nil)))
      (should (null (agent-repl--magit-merge-base-ref))))))

;;;; ---- Tests: agent-repl--magit-merge-base-commit ----

(ert-deftest agent-repl-test-magit-merge-base-commit-returns-base-and-sha ()
  "Returns (BASE . SHA) on the happy path with a diverged current branch."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--magit-merge-base-ref)
               (lambda () "master"))
              ((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (cond
                  ((equal args '("rev-parse" "--abbrev-ref" "HEAD")) "feature")
                  ((equal args '("merge-base" "HEAD" "master")) "abc123def")
                  (t (error "unexpected git args %S" args))))))
      (should (equal (agent-repl--magit-merge-base-commit)
                     '("master" . "abc123def"))))))

(ert-deftest agent-repl-test-magit-merge-base-commit-nil-when-no-base ()
  "Returns nil (and never touches git) when no base ref resolves."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--magit-merge-base-ref) (lambda () nil))
              ((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _args)
                 (error "git should not run when base ref is nil"))))
      (should (null (agent-repl--magit-merge-base-commit))))))

(ert-deftest agent-repl-test-magit-merge-base-commit-nil-when-on-base-branch ()
  "Returns nil when HEAD is on the base branch itself (no merge-base call)."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--magit-merge-base-ref)
               (lambda () "master"))
              ((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (cond
                  ((equal args '("rev-parse" "--abbrev-ref" "HEAD")) "master")
                  (t (error "merge-base should not run when on the base branch"))))))
      (should (null (agent-repl--magit-merge-base-commit))))))

(ert-deftest agent-repl-test-magit-merge-base-commit-nil-when-empty-output ()
  "Returns nil when git yields empty merge-base output."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--magit-merge-base-ref)
               (lambda () "master"))
              ((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (cond
                  ((equal args '("rev-parse" "--abbrev-ref" "HEAD")) "feature")
                  ((equal args '("merge-base" "HEAD" "master")) "")
                  (t (error "unexpected git args %S" args))))))
      (should (null (agent-repl--magit-merge-base-commit))))))

(ert-deftest agent-repl-test-magit-merge-base-commit-nil-when-fatal-output ()
  "Returns nil when merge-base output is git error text (`fatal: ...')."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--magit-merge-base-ref)
               (lambda () "nonexistent"))
              ((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (cond
                  ((equal args '("rev-parse" "--abbrev-ref" "HEAD")) "feature")
                  ((equal args '("merge-base" "HEAD" "nonexistent"))
                   "fatal: Not a valid object name nonexistent")
                  (t (error "unexpected git args %S" args))))))
      (should (null (agent-repl--magit-merge-base-commit))))))

(ert-deftest agent-repl-test-magit-merge-base-commit-passes-head-and-base-to-git ()
  "Computes the merge-base of HEAD against the resolved base ref."
  (agent-repl-test--with-clean-state
    (let ((merge-base-args nil))
      (cl-letf (((symbol-function 'agent-repl--magit-merge-base-ref)
                 (lambda () "develop"))
                ((symbol-function 'agent-repl--git-string-quiet)
                 (lambda (&rest args)
                   (cond
                    ((equal args '("rev-parse" "--abbrev-ref" "HEAD")) "feature")
                    ((equal (car args) "merge-base")
                     (setq merge-base-args args)
                     "deadbeef")
                    (t (error "unexpected git args %S" args))))))
        (agent-repl--magit-merge-base-commit)
        (should (equal merge-base-args '("merge-base" "HEAD" "develop")))))))

;;;; ---- Tests: merge-base section registration ----

(ert-deftest agent-repl-test-magit-merge-base-section-registered ()
  "`agent-repl--magit-insert-merge-base' is on `magit-status-sections-hook'."
  (should (memq 'agent-repl--magit-insert-merge-base
                magit-status-sections-hook)))

(provide 'test-magit)
;;; test-magit.el ends here
