;;; popup.el --- the one shared agent-repl popup subroutine -*- lexical-binding: t; -*-

;;; Commentary:

;; ONE SUBROUTINE BACKS EVERY VERTICAL-SPLIT POPUP AGENT-REPL SHOWS.  Every
;; such popup is a Doom popup on the RIGHT, 40% of the frame's width, focused
;; when it opens, and closed by `q' in command mode, which also kills its
;; buffer.  Only the buffer's own major and minor modes differ between them.
;;
;; Two entry points share that one display:
;;
;;   - `agent-repl-popup-show' shows a BUFFER: the daemon build log, the
;;     health report, a selected response's markdown.
;;   - `agent-repl-popup-open' shows a PATH[:line]: dired for a directory,
;;     the file otherwise.  Its callers are the plan bubble's edit button,
;;     every findings-row location jump, the worktree separation-divider
;;     paths, commands.el's link-code, notes.el's notes file, and the
;;     `open_in_editor' push arm of `agentrepl.v1' WatchHostWorkspace
;;     (`HostOpenInEditor {path, optional line}').
;;
;; DIVERGENCE BETWEEN CALL SITES IS A DEFECT: the consistency requirement is
;; code-level, the same ruling as the blink cadence.  A call site never calls
;; `display-buffer' for one of these windows itself, and adding a per-caller
;; variant here is the defect this file exists to prevent.
;;
;; THE GEOMETRY LIVES IN ONE DOOM POPUP RULE (`agent-repl-popup-rule').  A
;; buffer is routed to it by a buffer-local mark that `agent-repl-popup-show'
;; sets, never by its name: a file buffer's name cannot tell it apart from the
;; same file opened any other way.  `:ttl 0' is what kills the buffer the
;; moment its popup closes, which is Doom's own mechanism for that.
;;
;; LINE is 1-INDEXED, exactly as `HostOpenInEditor.line' is; nil means the
;; file's top (and it is always nil for a directory, which has no lines).

;;; Code:

(declare-function agent-repl--log "core")
(declare-function agent-repl--info "core")
(declare-function agent-repl--error "core")
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))
(declare-function +popup/quit-window "ext:popup" (&optional arg))

(defconst agent-repl-popup--log-scope
  '(:agent-repl-central "the popup utility is buffer-scoped, not workspace-scoped")
  "The log scope every popup record is filed under.")

(defconst agent-repl-popup-rule
  '(:side right :size 0.4 :select t :quit t :ttl 0 :autosave t)
  "The ONE Doom popup rule every agent-repl popup is displayed by.
Right side at 40% of the frame's width, focused on open.  `:ttl 0' kills
the buffer when its popup closes, and `:autosave t' saves a file buffer
first, so closing a notes or source popup never loses an edit.  `:quit t'
is Doom's default escape behavior, kept so a popup closes the way every
other Doom popup does.")

(defvar-local agent-repl-popup--owned nil
  "Non-nil in a buffer `agent-repl-popup-show' displayed.
The mark the popup rule's predicate reads; it dies with the buffer.")

(defvar agent-repl-popup-mode-map (make-sparse-keymap)
  "Keymap of `agent-repl-popup-mode'; `q' is bound in Evil normal state.")

(define-minor-mode agent-repl-popup-mode
  "Buffer-local behavior every agent-repl popup shares: `q' closes it.
Enabled by `agent-repl-popup-show', never by hand."
  :keymap agent-repl-popup-mode-map)

(defun agent-repl-popup--bind-keys ()
  "Bind `q' in Evil normal state to `agent-repl-popup-quit' in the popup map.
Normal state only: in insert state `q' is a letter."
  (evil-define-key* 'normal agent-repl-popup-mode-map "q" #'agent-repl-popup-quit))

(with-eval-after-load 'evil
  (agent-repl-popup--bind-keys))

(defun agent-repl-popup-buffer-p (buffer-or-name &optional _action)
  "Return non-nil when BUFFER-OR-NAME was displayed by `agent-repl-popup-show'.
The predicate of `agent-repl-popup-rule'.  Doom hands it a buffer name."
  (let ((buffer (get-buffer buffer-or-name)))
    (and (buffer-live-p buffer)
         (buffer-local-value 'agent-repl-popup--owned buffer)
         t)))

(defun agent-repl-popup-install-rule ()
  "Install `agent-repl-popup-rule' with Doom's popup system.
Returns non-nil when installed.  `set-popup-rule!' is absent only under
`emacs -Q' (the batch ERT suite), which has no popup system to configure."
  (if (fboundp 'set-popup-rule!)
      (progn
        (apply 'set-popup-rule! #'agent-repl-popup-buffer-p agent-repl-popup-rule)
        (agent-repl--info agent-repl-popup--log-scope "elisp.popup.rule-installed rule=%S"
                          agent-repl-popup-rule)
        t)
    (agent-repl--info agent-repl-popup--log-scope
                      "elisp.popup.rule-skipped reason=no-popup-module rule=%S"
                      agent-repl-popup-rule)
    nil))

;;;###autoload
(defun agent-repl-popup-show (buffer)
  "Show BUFFER in the shared agent-repl popup and return its window.
Marks BUFFER for `agent-repl-popup-rule' and enables `agent-repl-popup-mode'
in it, so it opens on the right at 40% width and `q' closes it and kills it.
A display that yields no window is a broken popup system, and signals."
  (unless (buffer-live-p buffer)
    (agent-repl--error agent-repl-popup--log-scope "elisp.popup.show: rejected reason=dead-buffer buffer=%S"
                       buffer)
    (error "agent-repl: cannot show a dead buffer in a popup: %S" buffer))
  (with-current-buffer buffer
    (setq agent-repl-popup--owned t)
    (agent-repl-popup-mode 1))
  (let ((window (display-buffer buffer)))
    (unless (window-live-p window)
      (agent-repl--error agent-repl-popup--log-scope "elisp.popup.show: rejected reason=no-window buffer=%s"
                         (buffer-name buffer))
      (error "agent-repl: the popup system showed no window for %s" (buffer-name buffer)))
    (agent-repl--log agent-repl-popup--log-scope "elisp.popup.show: buffer=%s window=%S"
                     (buffer-name buffer) window)
    window))

(defun agent-repl-popup-quit ()
  "Close the current agent-repl popup; its `:ttl 0' kills the buffer.
Bound to `q' in Evil normal state inside every agent-repl popup."
  (interactive)
  (agent-repl--log agent-repl-popup--log-scope "elisp.popup.quit: buffer=%s" (buffer-name))
  (+popup/quit-window))

(defun agent-repl-popup--path-buffer (path)
  "Return the buffer showing PATH: dired for a directory, the file otherwise.
PATH must already exist — the caller checks, because a missing path is a
refusal and not a buffer this function could invent."
  (if (file-directory-p path)
      (progn
        (agent-repl--log agent-repl-popup--log-scope "elisp.popup.buffer: kind=directory path=%s" path)
        (dired-noselect path))
    (agent-repl--log agent-repl-popup--log-scope "elisp.popup.buffer: kind=file path=%s" path)
    (find-file-noselect path)))

(defun agent-repl-popup--goto-line (buffer line)
  "Move point in BUFFER to LINE, 1-indexed, and return the position.
A LINE past the end of the buffer lands on the last line rather than
failing: the daemon's line is a hint at a location, and a stale hint must
still open the file."
  (with-current-buffer buffer
    (goto-char (point-min))
    (forward-line (1- line))
    (agent-repl--log agent-repl-popup--log-scope "elisp.popup.goto-line: buffer=%s line=%d point=%d"
                     (buffer-name buffer) line (point))
    (point)))

;;;###autoload
(defun agent-repl-popup-open (path &optional line)
  "Open PATH in the shared agent-repl popup.
A directory opens in dired; a file opens at LINE when given (1-indexed)
and at its top otherwise.  Returns the buffer.

A PATH that does not exist is a refusal, not a new file: the callers all
relay a location someone else observed, so inventing an empty buffer for
a stale one would hide the staleness."
  (interactive "fOpen in popup: ")
  (unless (and (stringp path) (not (string-empty-p path)))
    (agent-repl--error agent-repl-popup--log-scope "elisp.popup.open: rejected reason=empty-path path=%S" path)
    (user-error "agent-repl: no path to open"))
  (let ((expanded (expand-file-name path)))
    (unless (file-exists-p expanded)
      (agent-repl--error agent-repl-popup--log-scope "elisp.popup.open: rejected reason=missing-path path=%s"
                         expanded)
      (user-error "agent-repl: no such path: %s" expanded))
    (let ((buffer (agent-repl-popup--path-buffer expanded)))
      (when (and line (not (file-directory-p expanded)))
        (agent-repl-popup--goto-line buffer line))
      (when (and line (file-directory-p expanded))
        (agent-repl--log agent-repl-popup--log-scope "elisp.popup.open: line ignored reason=directory path=%s line=%s"
                         expanded line))
      (agent-repl-popup-show buffer)
      (agent-repl--info agent-repl-popup--log-scope "elisp.popup.open: opened path=%s line=%s buffer=%s"
                        expanded (or line "none") (buffer-name buffer))
      buffer)))

(provide 'agent-repl-popup)
;;; popup.el ends here
