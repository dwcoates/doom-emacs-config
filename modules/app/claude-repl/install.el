;;; install.el --- Emacs wrapper around .claude/install.sh -*- lexical-binding: t; -*-

;;; Commentary:

;; Interactive entry points for installing / uninstalling / reinstalling
;; the managed Claude Code hooks used by this module.  The canonical
;; implementation lives in `.claude/install.sh' at the Doom-config root;
;; this file shells out to it and surfaces output in a buffer.
;;
;; Also exposes `claude-repl--hooks-installed-p' as a predicate used by
;; `doctor.el' to report missing registrations.
;;
;; Host-only: no-ops when running inside the agent sandbox.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)

;;;; ---- Constants --------------------------------------------------------

(defconst claude-repl--managed-hooks
  '((Stop             . "~/.claude/hooks/stop-notify.sh")
    (UserPromptSubmit . "~/.claude/hooks/prompt-submit-notify.sh")
    (SessionStart     . "~/.claude/hooks/session-start-notify.sh")
    (Notification     . "~/.claude/hooks/permission-notify.sh"))
  "Alist (EVENT-SYMBOL . COMMAND-PATH) for hooks this module manages.
The COMMAND-PATH matches what `install.sh' writes into
`~/.claude/settings.json' — the literal `~/' is preserved
because Claude Code expands it at dispatch time.")

(defconst claude-repl--install-script
  (let ((module-dir (file-name-directory (or load-file-name
                                              (buffer-file-name)))))
    (expand-file-name "../../../.claude/install.sh" module-dir))
  "Absolute path to the bash install script.
Resolved relative to this file so the wrapper keeps working regardless
of where the Doom config tree is mounted.")

(defconst claude-repl--hooks-source-dir
  (let ((module-dir (file-name-directory (or load-file-name
                                              (buffer-file-name)))))
    (file-name-as-directory (expand-file-name "hooks" module-dir)))
  "Absolute path to the checked-in managed hook scripts.")

(defconst claude-repl--install-output-buffer "*claude-repl-install*"
  "Buffer name used to surface install-script output to the user.")

(defconst claude-repl--settings-file "~/.claude/settings.json"
  "Path to the Claude Code settings file we read for installed-state checks.")

(defconst claude-repl--hooks-dest-dir "~/.claude/hooks/"
  "Destination directory where managed hook scripts are installed.")

;;;; ---- Sandbox detection ------------------------------------------------

(defun claude-repl--in-sandbox-p ()
  "Return non-nil when Emacs is running inside the agent sandbox.
Mirrors the detection rule in `install.sh' so the Emacs wrappers no-op
under the same conditions: a `/.dockerenv' file exists or the
`DOOM_SANDBOX' environment variable is set to `1'."
  (or (file-exists-p "/.dockerenv")
      (equal (getenv "DOOM_SANDBOX") "1")))

;;;; ---- Installed-state predicate ----------------------------------------

(defun claude-repl--settings-json ()
  "Return parsed `~/.claude/settings.json' or nil if absent/unreadable."
  (let ((path (expand-file-name claude-repl--settings-file)))
    (when (file-exists-p path)
      (ignore-errors (json-read-file path)))))

(defun claude-repl--event-has-command-p (hooks event cmd)
  "Return non-nil when HOOKS alist has a CMD registered under EVENT.
HOOKS is the value of `.hooks' from `settings.json'.  EVENT is a
symbol (e.g. `Stop').  CMD is the literal command string we expect to
find in any entry's inner `.hooks[].command'."
  (let ((entries (cdr (assq event hooks))))
    (and entries
         (seq-some
          (lambda (entry)
            (let ((inner (cdr (assq 'hooks entry))))
              (seq-some (lambda (h) (equal (cdr (assq 'command h)) cmd))
                        inner)))
          entries))))

(defun claude-repl--hooks-installed-p ()
  "Return non-nil iff every managed hook is registered under its event.
Checks `~/.claude/settings.json' for an entry whose inner
`.hooks[].command' matches the canonical path for each member of
`claude-repl--managed-hooks'."
  (when-let* ((json (claude-repl--settings-json))
              (hooks (cdr (assq 'hooks json))))
    (cl-every (lambda (pair)
                (claude-repl--event-has-command-p hooks (car pair) (cdr pair)))
              claude-repl--managed-hooks)))

;;;; ---- Running the bash script ------------------------------------------

(defun claude-repl--run-install-script (action)
  "Invoke `install.sh' with ACTION, capturing output.
Returns a list (EXIT-CODE OUTPUT-STRING).  Signals an error when the
script cannot be located."
  (unless (file-exists-p claude-repl--install-script)
    (error "claude-repl install script not found: %s"
           claude-repl--install-script))
  (with-temp-buffer
    (let ((exit-code (call-process "bash" nil t nil
                                   claude-repl--install-script action)))
      (list exit-code (buffer-string)))))

(defun claude-repl--surface-install-output (output)
  "Place OUTPUT in `claude-repl--install-output-buffer' and return the buffer."
  (with-current-buffer (get-buffer-create claude-repl--install-output-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert output)
      (goto-char (point-min)))
    (current-buffer)))

(defun claude-repl--run-install-action (action)
  "Run install script ACTION (install / uninstall / reinstall).
No-op in sandbox.  On success, messages the user with a pointer to the
output buffer.  On failure, pops the output buffer and signals an error."
  (if (claude-repl--in-sandbox-p)
      (message "[claude-repl] Sandbox detected; skipping hooks %s." action)
    (pcase-let ((`(,code ,output)
                 (claude-repl--run-install-script action)))
      (claude-repl--surface-install-output output)
      (if (= code 0)
          (message "[claude-repl] hooks %s succeeded (see %s)."
                   action claude-repl--install-output-buffer)
        (display-buffer claude-repl--install-output-buffer)
        (error "[claude-repl] hooks %s failed (exit %d); see %s"
               action code claude-repl--install-output-buffer)))))

;;;; ---- Interactive entry points -----------------------------------------

;;;###autoload
(defun claude-repl-install-hooks ()
  "Install managed Claude Code hooks into `~/.claude/'.
Copies the checked-in scripts from `modules/app/claude-repl/hooks/' to
`~/.claude/hooks/' and appends registrations in `~/.claude/settings.json'
under each event.  Idempotent: foreign entries are preserved."
  (interactive)
  (claude-repl--run-install-action "install"))

;;;###autoload
(defun claude-repl-uninstall-hooks ()
  "Uninstall managed Claude Code hooks from `~/.claude/'.
Removes the managed command paths from `~/.claude/settings.json' (leaving
foreign entries untouched) and deletes the managed scripts from
`~/.claude/hooks/'."
  (interactive)
  (claude-repl--run-install-action "uninstall"))

;;;###autoload
(defun claude-repl-reinstall-hooks ()
  "Reinstall managed Claude Code hooks: uninstall then install."
  (interactive)
  (claude-repl--run-install-action "reinstall"))

;;;; ---- Doctor support ---------------------------------------------------

(defconst claude-repl--hook-severity
  '((Stop             . error)
    (SessionStart     . error)
    (UserPromptSubmit . warn)
    (Notification     . warn))
  "Severity of a missing managed hook.
`error' means the module is non-functional without it; `warn' means a
degraded UX but still usable.  Script-file problems for any hook are
treated as `error' (a registered hook pointing at a missing script will
fail noisily at dispatch time).")

(defun claude-repl--managed-script-name (cmd)
  "Return the bare filename for a managed command path CMD.
CMD is of the form \"~/.claude/hooks/<name>.sh\"."
  (file-name-nondirectory cmd))

(defun claude-repl--installed-script-path (cmd)
  "Absolute path where CMD's managed script should live after install."
  (expand-file-name (claude-repl--managed-script-name cmd)
                    (expand-file-name claude-repl--hooks-dest-dir)))

(defun claude-repl--source-script-path (cmd)
  "Absolute path of the checked-in source for CMD's managed script."
  (expand-file-name (claude-repl--managed-script-name cmd)
                    claude-repl--hooks-source-dir))

(defun claude-repl--file-contents (path)
  "Return PATH's contents as a string, or nil if unreadable."
  (when (file-readable-p path)
    (with-temp-buffer
      (insert-file-contents-literally path)
      (buffer-string))))

(defun claude-repl--script-drift-p (cmd)
  "Return non-nil when the installed managed script for CMD differs from source."
  (let ((installed (claude-repl--file-contents
                    (claude-repl--installed-script-path cmd)))
        (source (claude-repl--file-contents
                 (claude-repl--source-script-path cmd))))
    (and installed source (not (equal installed source)))))

(defun claude-repl--push-issue (issues-cell level msg)
  "Prepend (LEVEL . MSG) to the list held in ISSUES-CELL (a single-cons list)."
  (setcar issues-cell (cons (cons level msg) (car issues-cell))))

(defun claude-repl--check-registration (hooks issues-cell)
  "Populate ISSUES-CELL with any missing registrations per `claude-repl--managed-hooks'."
  (dolist (pair claude-repl--managed-hooks)
    (let* ((event (car pair))
           (cmd (cdr pair)))
      (unless (claude-repl--event-has-command-p hooks event cmd)
        (claude-repl--push-issue
         issues-cell
         (or (cdr (assq event claude-repl--hook-severity)) 'warn)
         (format "%s hook not registered in %s — run M-x claude-repl-install-hooks"
                 event claude-repl--settings-file))))))

(defun claude-repl--check-script-files (issues-cell)
  "Populate ISSUES-CELL with problems at each managed script's install location."
  (dolist (pair claude-repl--managed-hooks)
    (let* ((cmd (cdr pair))
           (path (claude-repl--installed-script-path cmd)))
      (cond
       ((not (file-exists-p path))
        (claude-repl--push-issue
         issues-cell 'error
         (format "Managed script missing: %s — run M-x claude-repl-install-hooks"
                 path)))
       ((not (file-executable-p path))
        (claude-repl--push-issue
         issues-cell 'error
         (format "Managed script not executable: %s" path)))
       ((claude-repl--script-drift-p cmd)
        (claude-repl--push-issue
         issues-cell 'warn
         (format "Managed script drift: %s differs from checked-in source — run M-x claude-repl-reinstall-hooks"
                 path)))))))

(defun claude-repl--doctor-issues ()
  "Return a list of (LEVEL . MESSAGE) describing hook-install problems.
LEVEL is `error' or `warn'.  Empty list means all managed hooks are
registered and on-disk.  No-ops (returns nil) when running inside the
sandbox — the host is where installation happens, not the container.
When settings.json is missing or unreadable, a single top-level error is
returned and the per-hook checks are skipped."
  (if (claude-repl--in-sandbox-p)
      nil
    (let* ((issues (list nil))
           (json (claude-repl--settings-json)))
      (if (not json)
          (claude-repl--push-issue
           issues 'error
           (format "%s is missing or unreadable — run M-x claude-repl-install-hooks"
                   (expand-file-name claude-repl--settings-file)))
        (claude-repl--check-registration (cdr (assq 'hooks json)) issues)
        (claude-repl--check-script-files issues))
      (nreverse (car issues)))))

(provide 'claude-repl-install)

;;; install.el ends here
