;;; test-claude-repl.el --- Aggregator: run all claude-repl tests -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is now a thin wrapper that loads the shared test helpers and
;; all per-module test files.  Individual tests have been migrated to
;; dedicated files (test-core.el, test-history.el, etc.).
;;
;; To run the full suite:
;;
;;   emacs -batch -Q -l ert -l test-claude-repl.el -f ert-run-tests-batch-and-exit
;;
;; To run a single module's tests, load test-helpers.el + the specific file:
;;
;;   emacs -batch -Q -l ert -l test-core.el -f ert-run-tests-batch-and-exit

;;; Code:

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  ;; Shared stub layer and test utilities
  (load (expand-file-name "test-helpers.el" dir) nil t)

  ;; Per-module test files (alphabetical order)
  (load (expand-file-name "test-autosave.el" dir) nil t)
  (load (expand-file-name "test-commands.el" dir) nil t)
  (load (expand-file-name "test-core.el" dir) nil t)
  (load (expand-file-name "test-history.el" dir) nil t)
  (load (expand-file-name "test-input.el" dir) nil t)
  (load (expand-file-name "test-keybindings.el" dir) nil t)
  (load (expand-file-name "test-magit.el" dir) nil t)
  (load (expand-file-name "test-notifications.el" dir) nil t)
  (load (expand-file-name "test-overlay.el" dir) nil t)
  (load (expand-file-name "test-panels.el" dir) nil t)
  (load (expand-file-name "test-sentinel.el" dir) nil t)
  (load (expand-file-name "test-session.el" dir) nil t)
  (load (expand-file-name "test-status.el" dir) nil t)
  (load (expand-file-name "test-worktree.el" dir) nil t))

(provide 'test-claude-repl)

;;; test-claude-repl.el ends here
