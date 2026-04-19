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

(if claude-repl--load-errors
    (progn
      (message "[claude-repl] Loaded with %d ERROR(S):" (length claude-repl--load-errors))
      (dolist (pair (nreverse claude-repl--load-errors))
        (message "[claude-repl]   %s.el: %S" (car pair) (cdr pair))))
  (message "[claude-repl] Loaded Claude-Repl package."))

(provide 'claude-repl)
;;; config.el ends here
