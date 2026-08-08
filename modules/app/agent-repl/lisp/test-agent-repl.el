;;; test-agent-repl.el --- Aggregator: run all agent-repl tests -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is now a thin wrapper that loads the shared test helpers and
;; all per-module test files.  Individual tests have been migrated to
;; dedicated files (test-core.el, test-history.el, etc.).
;;
;; To run the full suite:
;;
;;   emacs -batch -Q -l ert -l test-agent-repl.el -f ert-run-tests-batch-and-exit
;;
;; To run a single module's tests, load test-helpers.el + the specific file:
;;
;;   emacs -batch -Q -l ert -l test-core.el -f ert-run-tests-batch-and-exit

;;; Code:

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  ;; Shared stub layer and test utilities
  (load (expand-file-name "test-helpers.el" dir) nil t)

  ;; Per-module test files (alphabetical order).
  ;;
  ;; This list must stay EXHAUSTIVE over test-*.el in this directory. A file
  ;; that exists but is not loaded here is a suite everyone believes is running
  ;; and nothing is: the aggregate is the only thing CI and the pre-commit gate
  ;; invoke, so an unlisted file's failures are invisible.
  ;;
  ;; There is no exclusion any more. test-sidebar.el was held out while 14
  ;; of its 81 tests were red — casualties of the SSM cutover, which moved
  ;; render state onto the daemon-pushed :pushed-render-state while those
  ;; tests still seeded the pre-cutover :repl-state / :agent-state. The
  ;; five-color remap fixed the vocabulary half and re-seeded the rest, so
  ;; the file is green and back in the list where its failures are visible.
  (load (expand-file-name "test-ai-title.el" dir) nil t)
  (load (expand-file-name "test-autosave.el" dir) nil t)
  (load (expand-file-name "test-backend.el" dir) nil t)
  (load (expand-file-name "test-clipboard-image.el" dir) nil t)
  (load (expand-file-name "test-close-panels-on-open.el" dir) nil t)
  (load (expand-file-name "test-codex.el" dir) nil t)
  (load (expand-file-name "test-commands.el" dir) nil t)
  (load (expand-file-name "test-config.el" dir) nil t)
  (load (expand-file-name "test-context.el" dir) nil t)
  (load (expand-file-name "test-context-cost.el" dir) nil t)
  (load (expand-file-name "test-core.el" dir) nil t)
  (load (expand-file-name "test-daemon.el" dir) nil t)
  (load (expand-file-name "test-emoji.el" dir) nil t)
  (load (expand-file-name "test-explain-config.el" dir) nil t)
  (load (expand-file-name "test-failure.el" dir) nil t)
  (load (expand-file-name "test-render-colors.el" dir) nil t)
  (load (expand-file-name "test-frontend-client.el" dir) nil t)
  (load (expand-file-name "test-frontend-state.el" dir) nil t)
  (load (expand-file-name "test-frontend-uds.el" dir) nil t)
  (load (expand-file-name "test-frontend.el" dir) nil t)
  (load (expand-file-name "test-frontends.el" dir) nil t)
  (load (expand-file-name "test-hide-project-dirs.el" dir) nil t)
  (load (expand-file-name "test-history.el" dir) nil t)
  (load (expand-file-name "test-input.el" dir) nil t)
  (load (expand-file-name "test-install.el" dir) nil t)
  (load (expand-file-name "test-keybindings.el" dir) nil t)
  (load (expand-file-name "test-log-timestamp.el" dir) nil t)
  (load (expand-file-name "test-magit.el" dir) nil t)
  (load (expand-file-name "test-memory-state.el" dir) nil t)
  (load (expand-file-name "test-merge-handlers.el" dir) nil t)
  (load (expand-file-name "test-notifications.el" dir) nil t)
  (load (expand-file-name "test-output-nav.el" dir) nil t)
  (load (expand-file-name "test-panels.el" dir) nil t)
  (load (expand-file-name "test-permission.el" dir) nil t)
  (load (expand-file-name "test-prevent-select.el" dir) nil t)
  (load (expand-file-name "test-prompt-summary.el" dir) nil t)
  (load (expand-file-name "test-rename.el" dir) nil t)
  (load (expand-file-name "test-sentinel.el" dir) nil t)
  (load (expand-file-name "test-session.el" dir) nil t)
  (load (expand-file-name "test-services.el" dir) nil t)
  (load (expand-file-name "test-sidebar.el" dir) nil t)
  (load (expand-file-name "test-sibling-popup.el" dir) nil t)
  (load (expand-file-name "test-status.el" dir) nil t)
  (load (expand-file-name "test-tasks.el" dir) nil t)
  (load (expand-file-name "test-test-helpers.el" dir) nil t)
  (load (expand-file-name "test-window.el" dir) nil t)
  (load (expand-file-name "test-workspace-status-export.el" dir) nil t)
  (load (expand-file-name "test-workspace.el" dir) nil t)
  (load (expand-file-name "test-workspace-create-client.el" dir) nil t)
  (load (expand-file-name "test-worktree.el" dir) nil t))

(provide 'test-agent-repl)

;;; test-agent-repl.el ends here
