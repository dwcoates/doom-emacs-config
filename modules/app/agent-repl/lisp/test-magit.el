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

;;;; ---- Tests: agent-repl-open-workspace-pr-in-browser ----

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
        (agent-repl-open-workspace-pr-in-browser)
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
      (should-error (agent-repl-open-workspace-pr-in-browser) :type 'user-error))))

;;;; ---- Tests: agent-repl-magit-status-workspace (always-replace-current-buffer) ----

(ert-deftest agent-repl-test-magit-status-workspace-calls-magit-status-with-ws-dir ()
  "Calls magit's status setup with the workspace's project directory."
  (agent-repl-test--with-clean-state
    (let ((magit-status-args nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                ((symbol-function 'magit-status-setup-buffer)
                 (lambda (&rest args) (setq magit-status-args args))))
        (agent-repl-magit-status-workspace)
        (should (equal magit-status-args '("/tmp/proj/")))))))

(ert-deftest agent-repl-test-magit-status-workspace-never-splits ()
  "Never calls `split-window' -- magit always replaces the current buffer."
  (agent-repl-test--with-clean-state
    (let ((split-calls 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                ((symbol-function 'magit-status-setup-buffer) #'ignore)
                ((symbol-function 'split-window)
                 (lambda (&rest _) (cl-incf split-calls) 'should-not-happen)))
        (agent-repl-magit-status-workspace)
        (should (= split-calls 0))))))

(ert-deftest agent-repl-test-magit-status-workspace-keeps-fullscreen-config ()
  "Keeps the saved `:fullscreen-config' -- `q' on the status buffer
brings the panels back, and their later close restores that layout."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :project-dir "/tmp/proj")
    (agent-repl--ws-put "test-ws" :fullscreen-config 'fake-config)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'window-parameter) (lambda (_w _p) nil))
              ((symbol-function 'magit-status-setup-buffer) #'ignore))
      (agent-repl-magit-status-workspace)
      (should (eq (agent-repl--ws-get "test-ws" :fullscreen-config) 'fake-config)))))

(ert-deftest agent-repl-test-magit-status-workspace-binds-same-window-display-fn ()
  "Let-binds `magit-display-buffer-function' to the same-window helper
so `magit-status' replaces the current buffer instead of popping a new
window via the default traditional display behavior."
  (agent-repl-test--with-clean-state
    (let ((observed-fn nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                ((symbol-function 'magit-status-setup-buffer)
                 (lambda (&rest _)
                   (setq observed-fn magit-display-buffer-function))))
        (agent-repl-magit-status-workspace)
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
                ((symbol-function 'magit-status-setup-buffer) #'ignore))
        (agent-repl-magit-status-workspace)
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
                ((symbol-function 'magit-status-setup-buffer) #'ignore))
        (agent-repl-magit-status-workspace)
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
                ((symbol-function 'magit-status-setup-buffer) #'ignore))
        (agent-repl-magit-status-workspace)
        (should (= select-calls 0))))))

(ert-deftest agent-repl-test-magit-status-workspace-falls-back-to-default-directory ()
  "When the workspace is untracked (agent-repl--ws-dir errors), falls back
to `default-directory' so magit still opens without signalling.
Repro: SPC g g from the main \"doom\" workspace or after a workspace's
hash entry has been killed."
  (agent-repl-test--with-clean-state
    (let ((magit-status-args nil)
          (default-directory "/tmp/fallback-dir/"))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "untracked-ws"))
                ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                ((symbol-function 'magit-status-setup-buffer)
                 (lambda (&rest args) (setq magit-status-args args))))
        (agent-repl-magit-status-workspace)
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
                ((symbol-function 'magit-status-setup-buffer) #'ignore))
        (agent-repl-magit-status-workspace)
        (should (= ws-put-calls 0))))))

;;;; ---- Tests: agent-repl-magit-status-main-worktree (SPC g G) ----

(defmacro agent-repl-test-magit--with-common-dir (common-dir &rest body)
  "Run BODY with every git common-dir lookup answering COMMON-DIR.
Paths canonicalize by trailing-slash removal alone, so the fixture
directories need not exist."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-repl--git-string-quiet) (lambda (&rest _) ,common-dir))
             ((symbol-function 'agent-repl--path-canonical) #'directory-file-name))
     ,@body))

(ert-deftest agent-repl-test-magit-status-main-worktree-opens-main-checkout ()
  "Opens magit's status at the main worktree of the workspace's repository."
  (agent-repl-test--with-clean-state
    (let ((magit-status-args nil))
      (agent-repl-test-magit--with-common-dir "/repos/doom/.git"
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/repos/doom-worktrees/feat"))
                  ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                  ((symbol-function 'magit-status-setup-buffer)
                   (lambda (&rest args) (setq magit-status-args args))))
          (agent-repl-magit-status-main-worktree)
          (should (equal magit-status-args '("/repos/doom/"))))))))

(ert-deftest agent-repl-test-magit-status-main-worktree-resolves-from-workspace-dir ()
  "Resolves the main worktree from the workspace's own directory."
  (agent-repl-test--with-clean-state
    (let ((asked nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/repos/doom-worktrees/feat"))
                ((symbol-function 'agent-repl--main-worktree-for-dir)
                 (lambda (dir) (push dir asked) "/repos/doom"))
                ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                ((symbol-function 'magit-status-setup-buffer) #'ignore))
        (agent-repl-magit-status-main-worktree)
        (should (equal asked '("/repos/doom-worktrees/feat")))))))

(ert-deftest agent-repl-test-magit-status-main-worktree-untracked-resolves-from-default-directory ()
  "An untracked workspace resolves the main worktree from `default-directory'."
  (agent-repl-test--with-clean-state
    (let ((asked nil)
          (default-directory "/repos/doom-worktrees/other/"))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "untracked-ws"))
                ((symbol-function 'agent-repl--main-worktree-for-dir)
                 (lambda (dir) (push dir asked) "/repos/doom"))
                ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                ((symbol-function 'magit-status-setup-buffer) #'ignore))
        (agent-repl-magit-status-main-worktree)
        (should (equal asked '("/repos/doom-worktrees/other/")))))))

(ert-deftest agent-repl-test-magit-status-main-worktree-binds-same-window-display-fn ()
  "Opens with the same-window display function, exactly as `SPC g g' does."
  (agent-repl-test--with-clean-state
    (let ((observed-fn nil))
      (agent-repl-test-magit--with-common-dir "/repos/doom/.git"
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/repos/doom-worktrees/feat"))
                  ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                  ((symbol-function 'magit-status-setup-buffer)
                   (lambda (&rest _) (setq observed-fn magit-display-buffer-function))))
          (agent-repl-magit-status-main-worktree)
          (should (eq observed-fn #'agent-repl--magit-display-buffer-same-window)))))))

(ert-deftest agent-repl-test-magit-status-main-worktree-keeps-fullscreen-config ()
  "A tracked workspace's `:fullscreen-config' is kept, exactly as `SPC g g' does."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :project-dir "/repos/doom-worktrees/feat")
    (agent-repl--ws-put "test-ws" :fullscreen-config 'fake-config)
    (agent-repl-test-magit--with-common-dir "/repos/doom/.git"
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                ((symbol-function 'magit-status-setup-buffer) #'ignore))
        (agent-repl-magit-status-main-worktree)
        (should (eq (agent-repl--ws-get "test-ws" :fullscreen-config) 'fake-config))))))

(ert-deftest agent-repl-test-magit-status-main-worktree-refuses-outside-a-repository ()
  "Outside a git repository: a `user-error', an error log, and no magit."
  (agent-repl-test--with-clean-state
    (let ((magit-calls 0)
          (logged nil))
      (agent-repl-test-magit--with-common-dir "fatal: not a git repository"
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/nowhere"))
                  ((symbol-function 'window-parameter) (lambda (_w _p) nil))
                  ((symbol-function 'agent-repl--error)
                   (agent-repl-test--record-into logged))
                  ((symbol-function 'magit-status-setup-buffer)
                   (lambda (&rest _) (cl-incf magit-calls))))
          (should-error (agent-repl-magit-status-main-worktree) :type 'user-error)
          (should (= magit-calls 0))
          (should (equal logged
                         '("magit-status-main-worktree: ws=test-ws ws-dir=/tmp/nowhere cause=not-a-git-repository"))))))))

(ert-deftest agent-repl-test-magit-status-main-worktree-refusal-leaves-state-untouched ()
  "A refusal neither clears `:fullscreen-config' nor pops out of a side window."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :project-dir "/tmp/nowhere")
    (agent-repl--ws-put "test-ws" :fullscreen-config 'fake-config)
    (let ((select-calls 0))
      (agent-repl-test-magit--with-common-dir "fatal: not a git repository"
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'window-parameter)
                   (lambda (_win param) (and (eq param 'window-side) 'left)))
                  ((symbol-function 'window-main-window) (lambda (&rest _) 'fake-main))
                  ((symbol-function 'select-window) (lambda (_w) (cl-incf select-calls)))
                  ((symbol-function 'agent-repl--error) #'ignore)
                  ((symbol-function 'magit-status-setup-buffer) #'ignore))
          (should-error (agent-repl-magit-status-main-worktree) :type 'user-error)
          (should (= select-calls 0))
          (should (eq (agent-repl--ws-get "test-ws" :fullscreen-config) 'fake-config)))))))

(ert-deftest agent-repl-test-magit-status-commands-share-the-workspace-opener ()
  "`SPC g g' and `SPC g G' both open through `agent-repl--magit-status-workspace-open'."
  (let ((target-fns nil))
    (cl-letf (((symbol-function 'agent-repl--magit-status-workspace-open)
               (lambda (target-fn) (push target-fn target-fns))))
      (agent-repl-magit-status-workspace)
      (agent-repl-magit-status-main-worktree)
      (should (= (length target-fns) 2))
      (should (eq (car target-fns) #'agent-repl--magit-main-worktree-target))
      (should (equal (funcall (cadr target-fns) "ws" "/repos/doom-worktrees/feat")
                     "/repos/doom-worktrees/feat")))))

(ert-deftest agent-repl-test-magit-status-main-worktree-bound-to-spc-g-G ()
  "`SPC g G' names the main-worktree command in the module's leader bindings."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "magit.el" agent-repl-test--module-dir))
    (should (re-search-forward
             "\"g G\" #'agent-repl-magit-status-main-worktree" nil t))))

;;;; ---- Tests: agent-repl-magit-status-workspace (fullscreen panel handling) ----

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
                ((symbol-function 'magit-status-setup-buffer) #'ignore))
        (agent-repl-magit-status-workspace)
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
                ((symbol-function 'magit-status-setup-buffer) #'ignore))
        (agent-repl-magit-status-workspace)
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
                ((symbol-function 'magit-status-setup-buffer) #'ignore))
        (agent-repl-magit-status-workspace)
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
                ((symbol-function 'magit-status-setup-buffer) #'ignore))
        (agent-repl-magit-status-workspace)
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
                ((symbol-function 'magit-status-setup-buffer) #'ignore))
        (agent-repl-magit-status-workspace)
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

(ert-deftest agent-repl-test-magit-status-same-window-passes-dir-to-the-setup-entry-point ()
  "Passes DIR, as a directory name, to `magit-status-setup-buffer'."
  (let ((magit-status-args nil))
    (cl-letf (((symbol-function 'magit-status-setup-buffer)
               (lambda (&rest args) (setq magit-status-args args))))
      (agent-repl--magit-status-same-window "/tmp/proj")
      (should (equal magit-status-args '("/tmp/proj/"))))))

(ert-deftest agent-repl-test-magit-status-same-window-never-calls-the-prompting-entry-point ()
  "Never calls `magit-status', whose Lisp path can ask an unanswerable question.

`magit-status' is `interactive-only': on a `file-equal-p' mismatch between
DIR and the toplevel it re-derives, it asks `y-or-n-p' whether to create a
repository.  `file-equal-p' compares two separate stats' whole
`file-attributes' lists, so a write landing in DIR between them — the
daemon's own registration writes `.claude/emacs/' there — makes one path
compare unequal to itself and raises that prompt.  Nothing in a workspace
bring-up can answer it, and the reader's recursive edit stops the command
loop for good."
  (cl-letf (((symbol-function 'magit-status)
             (lambda (&rest _) (error "agent-repl called the prompting magit-status")))
            ((symbol-function 'magit-status-setup-buffer) #'ignore))
    (agent-repl--magit-status-same-window "/tmp/proj")))

(ert-deftest agent-repl-test-magit-status-same-window-binds-same-window-display-fn ()
  "Binds `magit-display-buffer-function' to the same-window helper for the
`magit-status' call, so the status buffer replaces the current window
instead of splitting via Doom's traditional display behavior."
  (let ((observed-fn nil))
    (cl-letf (((symbol-function 'magit-status-setup-buffer)
               (lambda (&rest _)
                 (setq observed-fn magit-display-buffer-function))))
      (agent-repl--magit-status-same-window "/tmp/proj")
      (should (eq observed-fn #'agent-repl--magit-display-buffer-same-window)))))

(ert-deftest agent-repl-test-magit-status-same-window-does-not-mutate-global-display-fn ()
  "Restores the global `magit-display-buffer-function' after returning, so
other magit buffers (diffs, logs) keep their normal display behavior."
  (let ((magit-display-buffer-function 'global-default))
    (cl-letf (((symbol-function 'magit-status-setup-buffer) #'ignore))
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
              ((symbol-function 'magit-status-setup-buffer)
               (lambda (&rest _) (push '(status) order))))
      (agent-repl--magit-status-same-window "/tmp/proj")
      (should (equal (nreverse order)
                     '((require magit t) (status)))))))

;;;; ---- Tests: agent-repl--magit-displaying ----

(ert-deftest agent-repl-test-magit-displaying-requires-magit-before-binding ()
  "Requires magit (NOERROR) before the display function is bound."
  (let ((order nil))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _file noerror)
                 (push (list 'require feature noerror magit-display-buffer-function) order)
                 'magit)))
      (let ((magit-display-buffer-function 'global-default))
        (agent-repl--magit-displaying
         'shown (lambda (_) (push (list 'open magit-display-buffer-function) order)))))
    (should (equal (nreverse order)
                   '((require magit t global-default) (open shown))))))

(ert-deftest agent-repl-test-magit-displaying-hands-the-require-result-over ()
  "OPEN-FN receives the `require' result and its value is returned."
  (cl-letf (((symbol-function 'require) (lambda (&rest _) 'loaded)))
    (should (equal (agent-repl--magit-displaying 'shown (lambda (result) (list :got result)))
                   '(:got loaded)))))

(ert-deftest agent-repl-test-magit-displaying-restores-the-global-display-fn ()
  "The binding ends with OPEN-FN, leaving the global display function alone."
  (let ((magit-display-buffer-function 'global-default))
    (cl-letf (((symbol-function 'require) #'ignore))
      (agent-repl--magit-displaying 'shown #'ignore))
    (should (eq magit-display-buffer-function 'global-default))))

;;;; ---- Tests: agent-repl-magit-log-commit ----

(defmacro agent-repl-test-magit--with-log (found &rest body)
  "Run BODY with magit's log entry points recorded; FOUND answers the goto.
Binds `calls' to the recorded calls, newest first, and `infos' to the
recorded `agent-repl--info' lines."
  (declare (indent 1))
  `(let ((calls nil) (infos nil) (log-buffer (generate-new-buffer " *log*")))
     (unwind-protect
         (cl-letf (((symbol-function 'require) (lambda (&rest _) 'magit))
                   ((symbol-function 'magit-log-arguments) (lambda (&rest _) '(("-n256") ("a.el"))))
                   ((symbol-function 'magit-log-setup-buffer)
                    (lambda (&rest args)
                      (push (list :setup default-directory magit-display-buffer-function args) calls)
                      log-buffer))
                   ((symbol-function 'magit-log-goto-commit-section)
                    (lambda (rev) (push (list :goto rev) calls) ,found))
                   ((symbol-function 'magit-section-update-highlight)
                    (lambda (&rest _) (push (list :highlight) calls)))
                   ((symbol-function 'agent-repl--info)
                    (agent-repl-test--record-into infos)))
           ,@body)
       (kill-buffer log-buffer))))

(ert-deftest agent-repl-test-magit-log-commit-opens-the-branch-log-in-the-popup ()
  "The branch's log is set up in DIR with magit's own arguments, shown by the popup."
  (agent-repl-test-magit--with-log t
    ;; Act
    (agent-repl-magit-log-commit "ws-1" "/repo" "master" "abc123")
    ;; Assert
    (should (equal (car (last calls))
                   (list :setup "/repo/" #'agent-repl-popup-show
                         '(("master") ("-n256") ("a.el")))))))

(ert-deftest agent-repl-test-magit-log-commit-moves-point-to-the-commit ()
  "Point goes to the commit's section, which is highlighted, and it is logged."
  (agent-repl-test-magit--with-log t
    ;; Act
    (agent-repl-magit-log-commit "ws-1" "/repo" "master" "abc123")
    ;; Assert
    (should (equal (seq-take calls 2) '((:highlight) (:goto "abc123"))))
    (should (seq-some (lambda (line) (string-search "commit=abc123 branch=shown" line)) infos))))

(ert-deftest agent-repl-test-magit-log-commit-says-so-when-the-log-omits-the-commit ()
  "A commit the log does not show leaves it at the top, logs and tells the user."
  (agent-repl-test-magit--with-log nil
    (let ((said nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
        ;; Act
        (agent-repl-magit-log-commit "ws-1" "/repo" "master" "abc123"))
      ;; Assert
      (should-not (assq :highlight calls))
      (should (seq-some (lambda (line) (string-search "branch=not-in-log" line)) infos))
      (should (equal said "agent-repl: abc123 is not among the commits master's log shows")))))

(defconst agent-repl-test-magit--source
  (expand-file-name "magit.el" (file-name-directory (or load-file-name buffer-file-name)))
  "The magit.el source these tests load.")

(ert-deftest agent-repl-test-magit-every-display-binding-goes-through-the-helper ()
  "No magit.el definition but the helper let-binds the display function itself."
  (with-temp-buffer
    (insert-file-contents agent-repl-test-magit--source)
    (should (= (how-many "(let\\*? ((magit-display-buffer-function" (point-min) (point-max)) 1))))

;;;; ---- Tests: commit decorations keep their tags ----

(ert-deftest agent-repl-test-magit-ref-labels-keep-tags ()
  "`magit-format-ref-labels' receives tag refs untouched, so tags show in status and log."
  (should (equal (magit-format-ref-labels "HEAD -> master, tag: v1.0, origin/master")
                 "HEAD -> master, tag: v1.0, origin/master")))

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

;;;; ---- Tests: GitHub commit-link commands ----
;;
;; Both are interactive commands bound in this file's own `map!' (the magit
;; transient's "g O" / "g C"), so they are live entry points with no elisp
;; caller.  These pin them against a dead-code sweep.

(ert-deftest agent-repl-test-magit-open-commit-in-github-browses-the-commit-url ()
  "The command browses the GitHub URL built from the remote and HEAD."
  (agent-repl-test--with-clean-state
    (let ((browsed nil))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) default-directory))
                ((symbol-function 'agent-repl--git-string)
                 (lambda (&rest args)
                   (if (equal args '("rev-parse" "HEAD"))
                       "cafebabe"
                     "https://github.com/ChessCom/repo.git")))
                ((symbol-function 'browse-url) (lambda (url) (setq browsed url))))
        (agent-repl-magit-open-commit-in-github)
        (should (equal browsed
                       "https://github.com/ChessCom/repo/commit/cafebabe"))))))

(ert-deftest agent-repl-test-magit-open-commit-in-github-refuses-a-foreign-remote ()
  "A remote outside the expected org is an error, never a guessed URL."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
              ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) default-directory))
              ((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 (if (equal args '("rev-parse" "HEAD"))
                     "cafebabe"
                   "https://example.com/someone/repo.git")))
              ((symbol-function 'browse-url) (lambda (_url) (error "must not browse"))))
      (should-error (agent-repl-magit-open-commit-in-github)))))

(ert-deftest agent-repl-test-magit-copy-commit-link-kills-the-commit-url ()
  "The command puts the commit URL for the commit at point on the kill ring."
  (agent-repl-test--with-clean-state
    (let ((kill-ring nil))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) default-directory))
                ((symbol-function 'magit-commit-at-point) (lambda () "d00dfeed"))
                ((symbol-function 'agent-repl--git-string)
                 (lambda (&rest _args) "https://github.com/ChessCom/repo.git")))
        (agent-repl-magit-copy-commit-link)
        (should (equal (car kill-ring)
                       "https://github.com/ChessCom/repo/commit/d00dfeed"))))))

;;;; ---- Tests: SSH/HTTPS remote -> GitHub commit URL (table-driven) ----
;;
;; `agent-repl-magit-github-base-url' replaces
;; `agent-repl-magit-github-ssh-prefix-regexp' ("^git@github.com:") in an
;; SSH remote; a base URL with no trailing slash glued the owner/repo path
;; straight onto "github.com" with no separator at all
;; ("https://github.comChessCom/repo"), so it never matched
;; `agent-repl-magit-github-org-regexp' and every SSH remote fell into the
;; "does not match expected pattern" error — even a genuine ChessCom one.
;; One case per test, all routed through the same production command so
;; the fix is pinned exactly where the bug lived.

(defun agent-repl-test--magit-commit-url-for-remote (remote-url)
  "Return the GitHub commit URL `agent-repl-magit-open-commit-in-github' browses
for REMOTE-URL, or signal whatever error the command signals."
  (let ((browsed nil))
    (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
              ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) default-directory))
              ((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 (if (equal args '("rev-parse" "HEAD"))
                     "cafebabe"
                   remote-url)))
              ((symbol-function 'browse-url) (lambda (url) (setq browsed url))))
      (agent-repl-magit-open-commit-in-github))
    browsed))

(ert-deftest agent-repl-test-magit-commit-url-ssh-remote ()
  "An SSH remote (\"git@github.com:...\") builds a well-formed HTTPS URL.
The regression case: the slash between \"github.com\" and the owner was
being eaten by the prefix substitution."
  ;; Act / Assert
  (should (equal (agent-repl-test--magit-commit-url-for-remote
                   "git@github.com:ChessCom/repo")
                 "https://github.com/ChessCom/repo/commit/cafebabe")))

(ert-deftest agent-repl-test-magit-commit-url-https-remote ()
  "An already-HTTPS remote is unaffected by the SSH-prefix substitution."
  ;; Act / Assert
  (should (equal (agent-repl-test--magit-commit-url-for-remote
                   "https://github.com/ChessCom/repo")
                 "https://github.com/ChessCom/repo/commit/cafebabe")))

(ert-deftest agent-repl-test-magit-commit-url-strips-trailing-dot-git ()
  "A trailing `.git' on an SSH remote is stripped before the URL is built."
  ;; Act / Assert
  (should (equal (agent-repl-test--magit-commit-url-for-remote
                   "git@github.com:ChessCom/repo.git")
                 "https://github.com/ChessCom/repo/commit/cafebabe")))

(ert-deftest agent-repl-test-magit-commit-url-non-github-remote-errors ()
  "A remote outside the expected org is still an error, never a guessed URL.
Pins that the fix did not weaken this refusal."
  ;; Act / Assert
  (should-error
   (agent-repl-test--magit-commit-url-for-remote
    "git@example.com:someone/repo.git")))

;;;; ---- Tests: agent-repl-magit-status-quit (q on magit-status) ----

(defmacro agent-repl-test-magit--with-status-quit (mode saved &rest body)
  "Run BODY in a buffer of major MODE whose saved window configuration is SAVED.
Binds `status' to that buffer, `calls' to magit's quit entry points as
they ran, newest first, each with the buffer current when it ran, and
`infos' and `errors' to the recorded `agent-repl--info' and
`agent-repl--error' lines.  The restore stub leaves another buffer
current, as magit's does when it restores a configuration."
  (declare (indent 2))
  `(let ((status (generate-new-buffer "magit: repo"))
         (elsewhere (generate-new-buffer " *restored*"))
         (calls nil) (infos nil) (errors nil))
     (unwind-protect
         (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws-1"))
                   ((symbol-function 'magit-restore-window-configuration)
                    (lambda (&rest _)
                      (push (list :restore (current-buffer)) calls)
                      (set-buffer elsewhere)))
                   ((symbol-function '+magit/quit-all)
                    (lambda () (push (list :quit-all (current-buffer)) calls)))
                   ((symbol-function 'agent-repl--info)
                    (agent-repl-test--record-into infos))
                   ((symbol-function 'agent-repl--error)
                    (agent-repl-test--record-into errors)))
           (with-current-buffer status
             (setq major-mode ,mode
                   default-directory "/repo/")
             (setq-local magit-previous-window-configuration ,saved)
             ,@body))
       (kill-buffer status)
       (kill-buffer elsewhere))))

(ert-deftest agent-repl-test-magit-status-quit-restores-then-closes-from-the-status-buffer ()
  "Restores the saved windows, then closes the repository's magit from the status buffer."
  (agent-repl-test-magit--with-status-quit 'magit-status-mode (current-window-configuration)
    ;; Act
    (agent-repl-magit-status-quit)
    ;; Assert
    (should (equal (reverse calls) (list (list :restore status) (list :quit-all status))))))

(ert-deftest agent-repl-test-magit-status-quit-logs-the-restore ()
  "The info record names the workspace, buffer, directory and restore branch."
  (agent-repl-test-magit--with-status-quit 'magit-status-mode (current-window-configuration)
    ;; Act
    (agent-repl-magit-status-quit)
    ;; Assert
    (should (equal infos
                   '("elisp.magit.status-quit ws=ws-1 buffer=magit: repo dir=/repo/ branch=restore-windows")))))

(ert-deftest agent-repl-test-magit-status-quit-closes-magit-without-saved-windows ()
  "With no saved configuration, magit still closes, and the record says so."
  (agent-repl-test-magit--with-status-quit 'magit-status-mode nil
    ;; Act
    (agent-repl-magit-status-quit)
    ;; Assert
    (should (equal (mapcar #'car (reverse calls)) '(:restore :quit-all)))
    (should (string-suffix-p "branch=no-saved-windows" (car infos)))))

(ert-deftest agent-repl-test-magit-status-quit-refuses-outside-magit-status ()
  "Outside a magit-status buffer: a `user-error', an error log, and no quit."
  (agent-repl-test-magit--with-status-quit 'magit-log-mode (current-window-configuration)
    ;; Act
    (should-error (agent-repl-magit-status-quit) :type 'user-error)
    ;; Assert
    (should (null calls))
    (should (null infos))
    (should (equal errors
                   '("elisp.magit.status-quit ws=ws-1 buffer=magit: repo mode=magit-log-mode cause=not-a-magit-status-buffer")))))

(ert-deftest agent-repl-test-magit-status-quit-branch-names-each-saved-state ()
  "Each saved configuration state gets its own branch name."
  (dolist (case `(("nothing saved" nil this "no-saved-windows")
                  ("this frame" saved this "restore-windows")
                  ("another frame" saved other "other-frame")))
    (pcase-let ((`(,name ,saved ,frame ,expected) case))
      ;; Arrange
      (cl-letf (((symbol-function 'window-configuration-frame)
                 (lambda (_c) (if (eq frame 'this) (selected-frame) 'another-frame))))
        ;; Act + Assert
        (should (equal (list name (agent-repl--magit-status-quit-branch saved))
                       (list name expected)))))))

(provide 'test-magit)
;;; test-magit.el ends here
