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

;;;; ---- Tests: +dwc/magit-status-workspace fullscreen-takeover ----

(ert-deftest claude-repl-test-magit-status-workspace-fullscreen-calls-magit-status ()
  "When claude is fullscreen, calls `magit-status' with the workspace dir."
  (claude-repl-test--with-clean-state
    (let ((magit-status-args nil))
      (claude-repl--ws-put "test-ws" :fullscreen-config 'fake-config)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'magit-status)
                 (lambda (&rest args) (setq magit-status-args args)))
                ((symbol-function 'split-window) (lambda (&rest _) 'fake-left-win))
                ((symbol-function 'select-window) #'ignore))
        (+dwc/magit-status-workspace)
        (should (equal magit-status-args '("/tmp/proj")))))))

(ert-deftest claude-repl-test-magit-status-workspace-fullscreen-does-not-delete-other-windows ()
  "When claude is fullscreen, does NOT call `delete-other-windows' — claude panels stay visible."
  (claude-repl-test--with-clean-state
    (let ((delete-calls 0))
      (claude-repl--ws-put "test-ws" :fullscreen-config 'fake-config)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'magit-status) #'ignore)
                ((symbol-function 'split-window) (lambda (&rest _) 'fake-left-win))
                ((symbol-function 'select-window) #'ignore)
                ((symbol-function 'delete-other-windows)
                 (lambda () (cl-incf delete-calls))))
        (+dwc/magit-status-workspace)
        (should (= delete-calls 0))))))

(ert-deftest claude-repl-test-magit-status-workspace-fullscreen-splits-left-of-main ()
  "When claude is fullscreen, splits the frame's MAIN window (side windows
excluded) with the new window on the left — so the new magit window can
never land as a sibling of the drawer side window."
  (claude-repl-test--with-clean-state
    (let ((split-args nil))
      (claude-repl--ws-put "test-ws" :fullscreen-config 'fake-config)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'window-main-window) (lambda (&rest _) 'fake-main))
                ((symbol-function 'split-window)
                 (lambda (&rest args) (setq split-args args) 'fake-left-win))
                ((symbol-function 'select-window) #'ignore)
                ((symbol-function 'magit-status) #'ignore))
        (+dwc/magit-status-workspace)
        (should (equal split-args '(fake-main nil left)))))))

(ert-deftest claude-repl-test-magit-status-workspace-fullscreen-selects-new-left-window ()
  "When claude is fullscreen, the newly-created left window is selected before opening magit."
  (claude-repl-test--with-clean-state
    (let ((selected-window nil)
          (magit-call-window nil))
      (claude-repl--ws-put "test-ws" :fullscreen-config 'fake-config)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'split-window) (lambda (&rest _) 'fake-left-win))
                ((symbol-function 'select-window)
                 (lambda (win) (setq selected-window win)))
                ((symbol-function 'magit-status)
                 (lambda (&rest _) (setq magit-call-window selected-window))))
        (+dwc/magit-status-workspace)
        (should (eq selected-window 'fake-left-win))
        (should (eq magit-call-window 'fake-left-win))))))

(ert-deftest claude-repl-test-magit-status-workspace-fullscreen-clears-saved-config ()
  "When claude is fullscreen, the saved `:fullscreen-config' is cleared."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :fullscreen-config 'fake-config)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
              ((symbol-function 'magit-status) #'ignore)
              ((symbol-function 'split-window) (lambda (&rest _) 'fake-left-win))
              ((symbol-function 'select-window) #'ignore))
      (+dwc/magit-status-workspace)
      (should (null (claude-repl--ws-get "test-ws" :fullscreen-config))))))

(ert-deftest claude-repl-test-magit-status-workspace-not-fullscreen-no-delete ()
  "When neither fullscreen nor panels visible, does NOT call `delete-other-windows'."
  (claude-repl-test--with-clean-state
    (let ((delete-calls 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'magit-status) #'ignore)
                ((symbol-function 'delete-other-windows)
                 (lambda () (cl-incf delete-calls))))
        (+dwc/magit-status-workspace)
        (should (= delete-calls 0))))))

(ert-deftest claude-repl-test-magit-status-workspace-not-fullscreen-no-magit-opens-fresh ()
  "When neither fullscreen nor panels visible, falls back to plain `magit-status'."
  (claude-repl-test--with-clean-state
    (let ((magit-status-args nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'magit-status)
                 (lambda (&rest args) (setq magit-status-args args))))
        (+dwc/magit-status-workspace)
        (should (equal magit-status-args '("/tmp/proj")))))))

;;;; ---- Tests: panels-visible-but-not-fullscreen path ----

(defmacro claude-repl-test--with-panels-visible (&rest body)
  "Run BODY with two live buffers wired up as the workspace's claude panels.
Creates real (live) vterm + input buffers, stubs `claude-repl--ws-get'
to return them for `:vterm-buffer' / `:input-buffer', and stubs
`get-buffer-window' so those buffers look like they have a live window.
Cleanup kills the buffers in an `unwind-protect'."
  `(let ((vterm-buf (generate-new-buffer " *test-vterm*"))
         (input-buf (generate-new-buffer " *test-input*")))
     (unwind-protect
         (cl-letf* ((real-ws-get (symbol-function 'claude-repl--ws-get))
                    ((symbol-function 'claude-repl--ws-get)
                     (lambda (ws key)
                       (cond ((eq key :vterm-buffer) vterm-buf)
                             ((eq key :input-buffer) input-buf)
                             (t (funcall real-ws-get ws key)))))
                    ((symbol-function 'get-buffer-window)
                     (lambda (b &optional _all)
                       (cond ((eq b vterm-buf) 'fake-vterm-win)
                             ((eq b input-buf) 'fake-input-win)))))
           ,@body)
       (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))
       (when (buffer-live-p input-buf) (kill-buffer input-buf)))))

(ert-deftest claude-repl-test-magit-status-workspace-panels-visible-deletes-non-panel-windows ()
  "When panels are visible (not fullscreen), `delete-non-panel-windows' is called first."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-panels-visible
      (let ((delete-args nil))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                  ((symbol-function 'claude-repl--delete-non-panel-windows)
                   (lambda (vterm input) (setq delete-args (list vterm input))))
                  ((symbol-function 'split-window) (lambda (&rest _) 'fake-left-win))
                  ((symbol-function 'select-window) #'ignore)
                  ((symbol-function 'magit-status) #'ignore))
          (+dwc/magit-status-workspace)
          (should (equal delete-args (list vterm-buf input-buf))))))))

(ert-deftest claude-repl-test-magit-status-workspace-panels-visible-splits-left-of-main ()
  "When panels are visible (not fullscreen), splits the frame's MAIN window
from the left — never the frame root, so the new magit window cannot land
adjacent to the drawer side window."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-panels-visible
      (let ((split-args nil))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                  ((symbol-function 'claude-repl--delete-non-panel-windows) #'ignore)
                  ((symbol-function 'window-main-window) (lambda (&rest _) 'fake-main))
                  ((symbol-function 'split-window)
                   (lambda (&rest args) (setq split-args args) 'fake-left-win))
                  ((symbol-function 'select-window) #'ignore)
                  ((symbol-function 'magit-status) #'ignore))
          (+dwc/magit-status-workspace)
          (should (equal split-args '(fake-main nil left))))))))

(ert-deftest claude-repl-test-magit-status-workspace-panels-visible-magit-in-new-left-window ()
  "When panels are visible (not fullscreen), magit opens in the new left window."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-panels-visible
      (let ((selected nil)
            (magit-call-window nil))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                  ((symbol-function 'claude-repl--delete-non-panel-windows) #'ignore)
                  ((symbol-function 'split-window) (lambda (&rest _) 'fake-left-win))
                  ((symbol-function 'select-window) (lambda (w) (setq selected w)))
                  ((symbol-function 'magit-status)
                   (lambda (&rest _) (setq magit-call-window selected))))
          (+dwc/magit-status-workspace)
          (should (eq selected 'fake-left-win))
          (should (eq magit-call-window 'fake-left-win)))))))

(ert-deftest claude-repl-test-magit-status-workspace-fullscreen-skips-delete-non-panel-windows ()
  "When already fullscreen, `delete-non-panel-windows' is NOT called (no non-panels to remove)."
  (claude-repl-test--with-clean-state
    (let ((delete-calls 0))
      (claude-repl--ws-put "test-ws" :fullscreen-config 'fake-config)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'claude-repl--delete-non-panel-windows)
                 (lambda (&rest _) (cl-incf delete-calls)))
                ((symbol-function 'split-window) (lambda (&rest _) 'fake-left-win))
                ((symbol-function 'select-window) #'ignore)
                ((symbol-function 'magit-status) #'ignore))
        (+dwc/magit-status-workspace)
        (should (= delete-calls 0))))))

(ert-deftest claude-repl-test-magit-status-workspace-no-panels-no-split ()
  "When neither fullscreen nor panels visible, does NOT split the frame root."
  (claude-repl-test--with-clean-state
    (let ((split-calls 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                ((symbol-function 'split-window)
                 (lambda (&rest _) (cl-incf split-calls) 'fake-left-win))
                ((symbol-function 'magit-status) #'ignore))
        (+dwc/magit-status-workspace)
        (should (= split-calls 0))))))

;;;; ---- Tests: claude-repl--existing-magit-status-window ----

(defmacro claude-repl-test--with-magit-mode-buffer (sym &rest body)
  "Bind SYM to a real buffer whose `major-mode' is `magit-status-mode',
execute BODY, then kill the buffer."
  (declare (indent 1))
  `(let ((,sym (generate-new-buffer " *test-magit-status*")))
     (unwind-protect
         (progn
           (with-current-buffer ,sym
             (setq major-mode 'magit-status-mode))
           ,@body)
       (when (buffer-live-p ,sym) (kill-buffer ,sym)))))

(defmacro claude-repl-test--with-buffer-in-selected-window (buf &rest body)
  "Temporarily show BUF in the selected window, run BODY, restore prior buffer.
Uses real Emacs window state rather than stubbing `window-list' /
`window-buffer' — those are C primitives whose `cl-letf' overrides
trigger native-comp trampoline compilation that fails in batch."
  (declare (indent 1))
  (let ((orig-buf (make-symbol "orig-buf"))
        (win (make-symbol "win")))
    `(let* ((,win (selected-window))
            (,orig-buf (window-buffer ,win)))
       (set-window-buffer ,win ,buf)
       (unwind-protect
           (progn ,@body)
         (set-window-buffer ,win ,orig-buf)))))

(ert-deftest claude-repl-test-existing-magit-status-window-finds-magit-window ()
  "Returns the live, non-side window displaying a `magit-status-mode' buffer."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-magit-mode-buffer magit-buf
      (claude-repl-test--with-buffer-in-selected-window magit-buf
        (should (eq (claude-repl--existing-magit-status-window)
                    (selected-window)))))))

(ert-deftest claude-repl-test-existing-magit-status-window-returns-nil-when-none ()
  "Returns nil when no window displays a `magit-status-mode' buffer."
  (claude-repl-test--with-clean-state
    ;; Selected window holds a vanilla buffer (fundamental-mode), so
    ;; the search must yield no match.
    (let ((plain-buf (generate-new-buffer " *test-plain*")))
      (unwind-protect
          (claude-repl-test--with-buffer-in-selected-window plain-buf
            (should (null (claude-repl--existing-magit-status-window))))
        (when (buffer-live-p plain-buf) (kill-buffer plain-buf))))))

(ert-deftest claude-repl-test-existing-magit-status-window-skips-side-windows ()
  "Side windows are excluded even if they hold a `magit-status-mode' buffer
— guards against the drawer or any future side panel being mis-selected."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-magit-mode-buffer magit-buf
      (claude-repl-test--with-buffer-in-selected-window magit-buf
        (let ((win (selected-window)))
          (unwind-protect
              (progn
                (set-window-parameter win 'window-side 'left)
                (should (null (claude-repl--existing-magit-status-window))))
            (set-window-parameter win 'window-side nil)))))))

;;;; ---- Tests: +dwc/magit-status-workspace reuse-existing branch ----

(defmacro claude-repl-test--with-existing-magit-win (win &rest body)
  "Run BODY with `claude-repl--existing-magit-status-window' stubbed to
return WIN."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'claude-repl--existing-magit-status-window)
              (lambda (&rest _) ,win)))
     ,@body))

(ert-deftest claude-repl-test-magit-status-workspace-reuses-existing-magit-window ()
  "When a magit-status window already exists, selects it and calls
`magit-status' (refresh in place) — no split, no panel teardown."
  (claude-repl-test--with-clean-state
    (let ((selected nil)
          (magit-args nil))
      (claude-repl-test--with-existing-magit-win 'existing-win
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                  ((symbol-function 'select-window) (lambda (w) (setq selected w)))
                  ((symbol-function 'magit-status)
                   (lambda (&rest args) (setq magit-args args))))
          (+dwc/magit-status-workspace)
          (should (eq selected 'existing-win))
          (should (equal magit-args '("/tmp/proj"))))))))

(ert-deftest claude-repl-test-magit-status-workspace-reuse-does-not-split ()
  "Reuse branch must not call `split-window' — a second magit window must
never appear next to the drawer."
  (claude-repl-test--with-clean-state
    (let ((split-calls 0))
      (claude-repl-test--with-existing-magit-win 'existing-win
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                  ((symbol-function 'select-window) #'ignore)
                  ((symbol-function 'magit-status) #'ignore)
                  ((symbol-function 'split-window)
                   (lambda (&rest _) (cl-incf split-calls) 'should-not-happen)))
          (+dwc/magit-status-workspace)
          (should (= split-calls 0)))))))

(ert-deftest claude-repl-test-magit-status-workspace-reuse-skips-delete-non-panel-windows ()
  "Reuse branch must not tear down panel-adjacent windows — only the
existing magit window is touched."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-panels-visible
      (let ((delete-calls 0))
        (claude-repl-test--with-existing-magit-win 'existing-win
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                    ((symbol-function 'select-window) #'ignore)
                    ((symbol-function 'magit-status) #'ignore)
                    ((symbol-function 'claude-repl--delete-non-panel-windows)
                     (lambda (&rest _) (cl-incf delete-calls))))
            (+dwc/magit-status-workspace)
            (should (= delete-calls 0))))))))

(ert-deftest claude-repl-test-magit-status-workspace-reuse-takes-priority-over-fullscreen ()
  "When BOTH an existing magit window AND `:fullscreen-config' are present,
the reuse branch wins — no split, fullscreen-config is left alone (the
existing magit window already shares the frame; no transition needed)."
  (claude-repl-test--with-clean-state
    (let ((split-calls 0))
      (claude-repl--ws-put "test-ws" :fullscreen-config 'fake-config)
      (claude-repl-test--with-existing-magit-win 'existing-win
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/proj"))
                  ((symbol-function 'select-window) #'ignore)
                  ((symbol-function 'magit-status) #'ignore)
                  ((symbol-function 'split-window)
                   (lambda (&rest _) (cl-incf split-calls) 'should-not-happen)))
          (+dwc/magit-status-workspace)
          (should (= split-calls 0))
          (should (eq (claude-repl--ws-get "test-ws" :fullscreen-config)
                      'fake-config)))))))

;;;; ---- Tests: claude-repl--magit-split-base-window ----

(ert-deftest claude-repl-test-magit-split-base-prefers-window-main-window ()
  "Returns `window-main-window' when available — guarantees splits land
inside the non-side main area."
  (cl-letf (((symbol-function 'window-main-window) (lambda (&rest _) 'fake-main))
            ((symbol-function 'frame-root-window) (lambda (&rest _) 'fake-root)))
    (should (eq (claude-repl--magit-split-base-window) 'fake-main))))

(provide 'test-magit)
;;; test-magit.el ends here
