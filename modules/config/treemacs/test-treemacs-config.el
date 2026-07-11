;;; ui/treemacs/test-treemacs-config.el -*- lexical-binding: t; -*-
;;
;; Tests for modules/config/treemacs/config.el.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-treemacs-config.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; The config lives inside Doom's `(after! treemacs ...)' block and calls the
;; Doom `map!' macro plus the treemacs minor-mode toggles.  None of those are
;; available in a bare `-Q' batch Emacs, so we stub them just enough to let the
;; body run and evaluate its `setq'.  The stub names do not exist without Doom
;; loaded, so defining them here is safe.

(require 'ert)
(require 'cl-lib)

(unless (fboundp 'after!)
  (defmacro after! (_feature &rest body)
    (declare (indent defun))
    `(progn ,@body)))

(unless (fboundp 'map!)
  (defmacro map! (&rest _) nil))

(unless (fboundp 'treemacs-follow-mode)
  (defun treemacs-follow-mode (&rest _) nil))

(unless (fboundp 'treemacs-filewatch-mode)
  (defun treemacs-filewatch-mode (&rest _) nil))

;; Declared special so the tests can `let'-bind it dynamically while the config
;; `setq's it under load.
(defvar treemacs-missing-project-action 'ask)

;; Captured at load time: `load-file-name'/`buffer-file-name' are both nil once
;; ERT is actually running the tests, so the directory must be resolved now.
(defconst agent-repl-test-treemacs--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding this test file and the `config.el' under test.")

(defun agent-repl-test-treemacs--config-file ()
  "Absolute path to the treemacs `config.el' under test."
  (expand-file-name "config.el" agent-repl-test-treemacs--dir))

(defun agent-repl-test-treemacs--load-config ()
  "Load the treemacs `config.el' under the top-level Doom stubs."
  (load (agent-repl-test-treemacs--config-file) nil t t))

(ert-deftest agent-repl-test-treemacs-missing-project-action-is-remove ()
  "config.el sets `treemacs-missing-project-action' to `remove'.

This is the setting that suppresses the blocking \"Project <name> at
<path> cannot be read.\" minibuffer prompt on perspective switches by
pruning the dead ephemeral-worktree project instead of asking."
  ;; Arrange: start from the upstream default so a no-op load fails the assert.
  (let ((treemacs-missing-project-action 'ask))
    ;; Act.
    (agent-repl-test-treemacs--load-config)
    ;; Assert.
    (should (eq treemacs-missing-project-action 'remove))))

(ert-deftest agent-repl-test-treemacs-missing-project-action-not-ask ()
  "config.el never leaves `treemacs-missing-project-action' at `ask'.

`ask' is the upstream default and the one value that produces the
blocking prompt, so the fix is meaningless if the load leaves it there."
  (let ((treemacs-missing-project-action 'ask))
    (agent-repl-test-treemacs--load-config)
    (should-not (eq treemacs-missing-project-action 'ask))))

(provide 'test-treemacs-config)
;;; test-treemacs-config.el ends here
