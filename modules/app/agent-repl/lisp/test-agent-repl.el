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
  ;; There is no exclusion. During the overhaul many of these suites are RED by
  ;; design — their modules are being rewritten against agentrepl.v1 — and that
  ;; is the starting condition, not a reason to hide one.
  (load (expand-file-name "test-autosave.el" dir) nil t)
  (load (expand-file-name "test-clipboard-image.el" dir) nil t)
  (load (expand-file-name "test-close-panels-on-open.el" dir) nil t)
  (load (expand-file-name "test-commands.el" dir) nil t)
  (load (expand-file-name "test-config.el" dir) nil t)
  (load (expand-file-name "test-connect.el" dir) nil t)
  (load (expand-file-name "test-core.el" dir) nil t)
  (load (expand-file-name "test-daemon.el" dir) nil t)
  (load (expand-file-name "test-daemon-link.el" dir) nil t)
  (load (expand-file-name "test-declarations.el" dir) nil t)
  (load (expand-file-name "test-emoji.el" dir) nil t)
  (load (expand-file-name "test-find-file-workspace.el" dir) nil t)
  (load (expand-file-name "test-frontend.el" dir) nil t)
  (load (expand-file-name "test-frontends.el" dir) nil t)
  (load (expand-file-name "test-held-edit.el" dir) nil t)
  (load (expand-file-name "test-history.el" dir) nil t)
  (load (expand-file-name "test-host.el" dir) nil t)
  (load (expand-file-name "test-input.el" dir) nil t)
  (load (expand-file-name "test-interaction-record.el" dir) nil t)
  (load (expand-file-name "test-keybindings.el" dir) nil t)
  (load (expand-file-name "test-log-timestamp.el" dir) nil t)
  (load (expand-file-name "test-magit.el" dir) nil t)
  (load (expand-file-name "test-mutation-progress.el" dir) nil t)
  (load (expand-file-name "test-notes.el" dir) nil t)
  (load (expand-file-name "test-notifications.el" dir) nil t)
  (load (expand-file-name "test-open-progress.el" dir) nil t)
  (load (expand-file-name "test-panels.el" dir) nil t)
  (load (expand-file-name "test-popup.el" dir) nil t)
  (load (expand-file-name "test-prevent-select.el" dir) nil t)
  (load (expand-file-name "test-prompt-queue.el" dir) nil t)
  (load (expand-file-name "test-prompt-summary.el" dir) nil t)
  (load (expand-file-name "test-render-colors.el" dir) nil t)
  (load (expand-file-name "test-roster.el" dir) nil t)
  (load (expand-file-name "test-rpc.el" dir) nil t)
  (load (expand-file-name "test-services.el" dir) nil t)
  (load (expand-file-name "test-session.el" dir) nil t)
  (load (expand-file-name "test-sibling-popup.el" dir) nil t)
  (load (expand-file-name "test-status.el" dir) nil t)
  (load (expand-file-name "test-test-helpers.el" dir) nil t)
  (load (expand-file-name "test-webview-recovery.el" dir) nil t)
  (load (expand-file-name "test-verbs.el" dir) nil t)
  (load (expand-file-name "test-window.el" dir) nil t)
  (load (expand-file-name "test-wire-common.el" dir) nil t)
  (load (expand-file-name "test-wire-host.el" dir) nil t)
  (load (expand-file-name "test-wire-roster.el" dir) nil t)
  (load (expand-file-name "test-wire-verbs.el" dir) nil t)
  (load (expand-file-name "test-workspace.el" dir) nil t)
  (load (expand-file-name "test-worktree.el" dir) nil t)

  ;; integration suites
  ;;
  ;; These drive the production transport against a REAL agentrepl.v1 server
  ;; (lisp/testsupport/fakedaemon, built once per run) over a real loopback
  ;; socket.  They mock Emacs's one NEIGHBOR and compose no other system, so
  ;; they are integration tests, never end-to-end ones.  test-integration-
  ;; helpers.el loads test-helpers.el itself and inherits its batch-only
  ;; gating.
  (load (expand-file-name "test-integration-connect.el" dir) nil t)
  (load (expand-file-name "test-integration-host.el" dir) nil t)
  (load (expand-file-name "test-integration-link.el" dir) nil t)
  (load (expand-file-name "test-integration-roster.el" dir) nil t)
  (load (expand-file-name "test-integration-composer.el" dir) nil t)
  (load (expand-file-name "test-integration-verbs.el" dir) nil t)
  (load (expand-file-name "test-integration-daemon.el" dir) nil t)
  (load (expand-file-name "test-integration-fixture.el" dir) nil t))

(provide 'test-agent-repl)

;;; test-agent-repl.el ends here
