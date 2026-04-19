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

(defvar claude-repl--module-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the claude-repl module files.")

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

(defvar claude-repl--module-files
  '("core" "install" "notifications" "history" "overlay" "status"
    "autosave" "sentinel" "input" "commands" "session" "panels"
    "worktree" "keybindings" "magit")
  "Sub-module files in dependency order.")

(defun claude-repl-reload ()
  "Reload all claude-repl sub-modules from disk.
Useful after merging a branch that may have changed module code."
  (interactive)
  (message "[claude-repl] Reloading Claude-Repl package...")
  (setq claude-repl--load-errors nil)
  (dolist (file claude-repl--module-files)
    (let ((path (expand-file-name (concat file ".el") claude-repl--module-dir)))
      (condition-case err
          (progn
            (load-file path)
            (message "[claude-repl] %s.el reloaded." file))
        (error
         (push (cons file err) claude-repl--load-errors)
         (message "[claude-repl] FAILED to reload %s.el: %S" file err)))))
  (if claude-repl--load-errors
      (message "[claude-repl] Reloaded with %d ERROR(S)." (length claude-repl--load-errors))
    (message "[claude-repl] Reloaded Claude-Repl package.")))

(dolist (file claude-repl--module-files)
  (claude-repl--load-module file))

(if claude-repl--load-errors
    (progn
      (message "[claude-repl] Loaded with %d ERROR(S):" (length claude-repl--load-errors))
      (dolist (pair (nreverse claude-repl--load-errors))
        (message "[claude-repl]   %s.el: %S" (car pair) (cdr pair))))
  (message "[claude-repl] Loaded Claude-Repl package."))

(provide 'claude-repl)
;;; config.el ends here
