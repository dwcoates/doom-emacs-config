;;; config.el --- claude repl for doom emacs -*- lexical-binding: t; -*-

;; Author: Dodge Coates
;; URL: https://github.com/dodgecoates
;; Version: 0.1.0

;;; Commentary:
;; Main loader for the claude-repl module. Sub-files are loaded in
;; dependency order; Elisp's call-time function resolution means
;; forward references between defuns are safe.

;;; Code:

(message "[claude-repl] Loading Claude-Repl package...")

(defvar claude-repl--config-file
  (or load-file-name buffer-file-name)
  "Absolute path to this config.el, captured at load time for reloading.")

(defvar claude-repl--load-errors nil
  "List of (FILE . ERROR) pairs for sub-files that failed to load.")

(defmacro claude-repl--load-module (file)
  "Load FILE via `load!', catching and recording any error."
  `(condition-case err
       (progn
         (load! ,file)
         (message "[claude-repl] %s.el loaded." ,file))
     (error
      (push (cons ,file err) claude-repl--load-errors)
      (message "[claude-repl] FAILED to load %s.el: %S" ,file err))))

(claude-repl--load-module "core")
(claude-repl--load-module "install")
(claude-repl--load-module "notifications")
(claude-repl--load-module "history")
(claude-repl--load-module "overlay")
(claude-repl--load-module "status")
(claude-repl--load-module "autosave")
(claude-repl--load-module "sentinel")
(claude-repl--load-module "input")
(claude-repl--load-module "commands")
(claude-repl--load-module "session")
(claude-repl--load-module "panels")
(claude-repl--load-module "worktree")
(claude-repl--load-module "keybindings")
(claude-repl--load-module "magit")
(claude-repl--load-module "emoji")

(if claude-repl--load-errors
    (progn
      (message "[claude-repl] Loaded with %d ERROR(S):" (length claude-repl--load-errors))
      (dolist (pair (nreverse claude-repl--load-errors))
        (message "[claude-repl]   %s.el: %S" (car pair) (cdr pair)))
      ;; core.el is required for the module to function at all.
      ;; Other modules can fail (degraded mode), but without core
      ;; the entire package is non-functional.
      (when (assoc "core" claude-repl--load-errors)
        (error "[claude-repl] FATAL: core.el failed to load — module is non-functional")))
  (message "[claude-repl] Loaded Claude-Repl package."))

;; Workspace snapshot save (quit) and lazy claude start (on first visit to
;; a restored workspace).  The restore side is NOT wired to
;; `emacs-startup-hook' — calling `+dwc/switch-to-project' 10x at startup
;; hits `magit-status' per project and can hang the UI / race
;; persp-mode's initialization (causing void-function errors on
;; `safe-persp-name').  Users run `M-x claude-repl-load-workspace-snapshot'
;; manually once Emacs is fully up.  TODO: re-enable auto-restore behind a
;; safer entry point (e.g. `doom-init-ui-hook' + idle timer, with a
;; lighter persp-creation path that skips magit/recent-file openings).
(add-hook 'kill-emacs-hook #'claude-repl--save-workspace-snapshot-on-quit)
(with-eval-after-load 'persp-mode
  (add-hook 'persp-activated-functions #'claude-repl--maybe-start-on-activate))

(provide 'claude-repl)
;;; config.el ends here
