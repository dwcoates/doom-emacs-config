;;; notes.el --- Per-workspace org notes -*- lexical-binding: t; -*-

;;; Commentary:

;; Org notes stay EMACS-LOCAL (docs/overhaul/elisp.md, "Removals ruled
;; 2026-08-29": tasks move to the wire, org NOTES files do not).  This file
;; is what survives of tasks.el: the notes file itself, keyed now by the
;; WORKSPACE it belongs to rather than by a task id, since the task model is
;; gone and a workspace is the only local thing left to hang notes on.
;;
;; One file per workspace, `<state dir>/notes/<workspace name>.org', seeded
;; with an org title header on first open.  The buffer gets a buffer-local
;; `kill-buffer-hook' that saves it when modified, so dismissing the popup
;; persists the notes however it closes.
;;
;; OPENER: the shared popup subroutine `agent-repl-popup-open' (popup.el),
;; the one place Emacs puts an agent-repl popup on screen.

;;; Code:

(require 'subr-x)

(declare-function agent-repl--global-state-file "core" (relative))
(declare-function agent-repl--log "core" (ws format-string &rest args))
(declare-function agent-repl--info "core" (ws format-string &rest args))
(declare-function agent-repl--warn "core" (ws format-string &rest args))
(declare-function agent-repl--error "core" (ws format-string &rest args))
(declare-function agent-repl--ws-current-name "workspace" ())
(declare-function agent-repl--save-buffer-if-modified "autosave" (buf &optional ws aggregate-p))
(declare-function agent-repl-popup-open "popup" (path &optional line))
(defvar agent-repl--global-log-scope)

;;;; ---- On-disk locations -----------------------------------------------

(defun agent-repl--notes-dir ()
  "Absolute path (trailing slash) of the org notes directory.
Same convention tasks.el used for its per-task notes: a single
subdirectory of the global state root, resolved through core.el's
`agent-repl--global-state-file' so the `AGENT_REPL_STATE_DIR' override
governs it like every other piece of Emacs-local state."
  (let ((dir (file-name-as-directory (agent-repl--global-state-file "notes"))))
    (agent-repl--log '(:agent-repl-central "the notes directory is shared storage") "elisp.notes.dir: dir=%s" dir)
    dir))

(defun agent-repl--notes-file (workspace)
  "Absolute path of WORKSPACE's org notes file.
Signals when WORKSPACE is not a nonempty string: a notes file with no
workspace to name it would collide with every other workspace's, so the
missing name is surfaced rather than defaulted."
  (unless (and (stringp workspace) (not (string-empty-p workspace)))
    (agent-repl--error '(:agent-repl-central "the rejected notes request names no workspace")
                       "elisp.notes.file: rejected workspace=%S reason=not-a-nonempty-string"
                       workspace)
    (error "agent-repl--notes-file: workspace must be a nonempty string, got %S" workspace))
  (let ((file (expand-file-name (format "%s.org" workspace) (agent-repl--notes-dir))))
    (agent-repl--log workspace "elisp.notes.file: workspace=%s file=%s" workspace file)
    file))

;;;; ---- Seeding ---------------------------------------------------------

(defun agent-repl--notes-ensure (workspace)
  "Ensure WORKSPACE's org notes file exists, seeded with a title header.
Returns the file path.  Idempotent: an existing file is left untouched."
  (let ((file (agent-repl--notes-file workspace))
        (created nil))
    (unless (file-exists-p file)
      (agent-repl--log workspace "elisp.notes.ensure: create workspace=%s file=%s" workspace file)
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert (format "#+TITLE: %s\n#+CREATED: %s\n\n"
                        workspace (format-time-string "%Y-%m-%d %H:%M"))))
      (setq created t))
    (agent-repl--log workspace "elisp.notes.ensure: ready workspace=%s file=%s outcome=%s"
                     workspace file (if created 'created 'existing))
    file))

;;;; ---- Opening ---------------------------------------------------------

(defun agent-repl--notes-install-save-on-kill (buf workspace)
  "Install a buffer-local save-on-kill hook on BUF owned by WORKSPACE.
The notes popup is dismissed far more often than it is explicitly saved,
so the save rides the kill rather than the user's memory."
  (with-current-buffer buf
    (add-hook 'kill-buffer-hook
              (lambda ()
                (agent-repl--save-buffer-if-modified (current-buffer) workspace))
              nil t))
  (agent-repl--log workspace
                   "elisp.notes.save-on-kill: installed workspace=%s buffer=%s"
                   workspace (buffer-name buf))
  buf)

;;;###autoload
(defun agent-repl-notes-open ()
  "Open the current workspace's org notes file.
Seeds the file on first open and installs the save-on-kill hook, then
visits it.  Signals when there is no current workspace: notes are keyed
by workspace and there is nothing to key them by."
  (interactive)
  (let ((workspace (agent-repl--ws-current-name)))
    (unless (and (stringp workspace) (not (string-empty-p workspace)))
      (agent-repl--error '(:agent-repl-central "the notes command has no current workspace")
                         "elisp.notes.open: rejected reason=no-current-workspace workspace=%S"
                         workspace)
      (user-error "agent-repl: no current workspace to open notes for"))
    (agent-repl--log workspace "elisp.notes.open: begin workspace=%s" workspace)
    (let ((file (agent-repl--notes-ensure workspace)))
      ;; The ONE shared popup subroutine (popup.el).  Every agent-repl popup
      ;; goes through it, and a local `find-file' here would be the
      ;; divergence that rule forbids.
      (agent-repl-popup-open file)
      (agent-repl--notes-install-save-on-kill (get-file-buffer file) workspace)
      (agent-repl--info workspace "elisp.notes.open: opened workspace=%s file=%s" workspace file)
      (current-buffer))))

(provide 'agent-repl-notes)
;;; notes.el ends here
