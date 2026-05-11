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

;; Reset the accumulator on every load — `defvar' only initializes once,
;; so without this `M-x doom/reload' would re-report stale errors from a
;; prior failed load even after the next load succeeded, masking actual
;; status.
(setq claude-repl--load-errors nil)

(defmacro claude-repl--load-module (file)
  "Load FILE via `load!', recording any error for collective reporting."
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
(claude-repl--load-module "workspace-status-export")
(claude-repl--load-module "autosave")
(claude-repl--load-module "sentinel")
(claude-repl--load-module "input")
(claude-repl--load-module "commands")
(claude-repl--load-module "session")
(claude-repl--load-module "prompt-summary")
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
      (error "[claude-repl] FATAL: %d module(s) failed to load — see messages above"
             (length claude-repl--load-errors)))
  (message "[claude-repl] Loaded Claude-Repl package."))

;; Snapshot restore is wired to `emacs-startup-hook' through an idle
;; timer (`claude-repl-snapshot-startup-load-delay' seconds).  The
;; deferral exists only to let persp-mode finish its own initialization
;; before our loader iterates entries — once the timer fires, restore
;; runs fully synchronously: each entry is created, activated,
;; project-aligned (default-directory, dir-locals, magit lambda,
;; find-file recent), and has its claude session started before the
;; loader moves to the next entry.  The loader returns to whichever
;; workspace was active when it began.
;;
;; Companion save-guard (`claude-repl--snapshot-loaded-p') prevents
;; `--state-save' from clobbering the on-disk roster if a state-
;; mutation fires before the idle timer resolves.
;;
;; Snapshot save is paired with `claude-repl--state-save' (history.el) so
;; the roster is updated on every workspace mutation rather than only at
;; Emacs quit — that way a crash before quit doesn't lose the roster.
(defcustom claude-repl-snapshot-startup-load-delay 2.0
  "Idle seconds to wait after `emacs-startup-hook' before restoring snapshot.
Tuned to let persp-mode finish initialization (so `safe-persp-name'
and friends are bound) before the loader iterates entries.  Set to nil
to disable startup-time restore entirely."
  :type '(choice (const :tag "Disabled" nil) number)
  :group 'claude-repl)

(defun claude-repl--schedule-snapshot-startup-load ()
  "Schedule `--load-workspace-snapshot-on-startup' on an idle timer.
Honours `claude-repl-snapshot-startup-load-delay'; a nil delay disables
the auto-load entirely.  Intended to run from `emacs-startup-hook'."
  (when claude-repl-snapshot-startup-load-delay
    (run-with-idle-timer claude-repl-snapshot-startup-load-delay
                         nil
                         #'claude-repl--load-workspace-snapshot-on-startup)))

(add-hook 'emacs-startup-hook #'claude-repl--schedule-snapshot-startup-load)

(provide 'claude-repl)
;;; config.el ends here
