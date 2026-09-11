;;; popup.el --- the one shared editor-popup subroutine -*- lexical-binding: t; -*-

;;; Commentary:

;; ONE SUBROUTINE BACKS EVERY OPEN-A-FILE AFFORDANCE: "open path[:line] in a
;; doom popup, right side, half width" — dired when the path is a directory.
;;
;; Its callers are mandated to share this one implementation: the plan
;; bubble's edit button, every findings-row location jump, the worktree
;; separation-divider paths, commands.el's link-code, notes.el's notes file,
;; and the `open_in_editor' push arm of `agentrepl.v1' WatchHostWorkspace
;; (`HostOpenInEditor {path, optional line}').  DIVERGENCE BETWEEN CALL SITES
;; IS A DEFECT: the consistency requirement is code-level, the same ruling as
;; the blink cadence.  There are no per-caller variants here, and adding one
;; is the defect this file exists to prevent.
;;
;; LINE is 1-INDEXED, exactly as `HostOpenInEditor.line' is; nil means the
;; file's top (and it is always nil for a directory, which has no lines).

;;; Code:

(declare-function agent-repl--log "core")
(declare-function agent-repl--info "core")
(declare-function agent-repl--warn "core")
(declare-function agent-repl--error "core")

(defcustom agent-repl-popup-width-fraction 0.5
  "The popup side window's share of the frame width.
Half the frame, per the one shared spec; the fraction is the mechanism,
not the contract."
  :type 'float
  :group 'agent-repl)

(defun agent-repl-popup--buffer (path)
  "Return the buffer showing PATH: dired for a directory, the file otherwise.
PATH must already exist — the caller checks, because a missing path is a
refusal and not a buffer this function could invent."
  (if (file-directory-p path)
      (progn
        (agent-repl--log '(:agent-repl-central "the popup utility is path-scoped") "elisp.popup.buffer: kind=directory path=%s" path)
        (dired-noselect path))
    (agent-repl--log '(:agent-repl-central "the popup utility is path-scoped") "elisp.popup.buffer: kind=file path=%s" path)
    (find-file-noselect path)))

(defun agent-repl-popup--goto-line (buffer line)
  "Move point in BUFFER to LINE, 1-indexed, and return the position.
A LINE past the end of the buffer lands on the last line rather than
failing: the daemon's line is a hint at a location, and a stale hint must
still open the file."
  (with-current-buffer buffer
    (goto-char (point-min))
    (forward-line (1- line))
    (agent-repl--log '(:agent-repl-central "the popup utility is path-scoped") "elisp.popup.goto-line: buffer=%s line=%d point=%d"
                     (buffer-name buffer) line (point))
    (point)))

(defun agent-repl-popup--display (buffer)
  "Display BUFFER in the shared popup: a right side window at half the frame.
Returns the window."
  (let* ((width (max 1 (round (* agent-repl-popup-width-fraction
                                 (frame-width)))))
         (window (display-buffer-in-side-window
                  buffer
                  `((side . right)
                    (slot . 0)
                    (window-width . ,width)))))
    (if (window-live-p window)
        (agent-repl--log '(:agent-repl-central "the popup utility is path-scoped") "elisp.popup.display: buffer=%s width=%d window=%S"
                         (buffer-name buffer) width window)
      (agent-repl--warn '(:agent-repl-central "the popup utility is path-scoped") "elisp.popup.display: no window buffer=%s width=%d"
                        (buffer-name buffer) width))
    window))

;;;###autoload
(defun agent-repl-popup-open (path &optional line)
  "Open PATH in the shared editor popup — right side, half the frame width.
A directory opens in dired; a file opens at LINE when given (1-indexed)
and at its top otherwise.  Returns the buffer.

A PATH that does not exist is a refusal, not a new file: the callers all
relay a location someone else observed, so inventing an empty buffer for
a stale one would hide the staleness."
  (interactive "fOpen in popup: ")
  (unless (and (stringp path) (not (string-empty-p path)))
    (agent-repl--error '(:agent-repl-central "the popup utility is path-scoped") "elisp.popup.open: rejected reason=empty-path path=%S" path)
    (user-error "agent-repl: no path to open"))
  (let ((expanded (expand-file-name path)))
    (unless (file-exists-p expanded)
      (agent-repl--error '(:agent-repl-central "the popup utility is path-scoped") "elisp.popup.open: rejected reason=missing-path path=%s"
                         expanded)
      (user-error "agent-repl: no such path: %s" expanded))
    (let ((buffer (agent-repl-popup--buffer expanded)))
      (when (and line (not (file-directory-p expanded)))
        (agent-repl-popup--goto-line buffer line))
      (when (and line (file-directory-p expanded))
        (agent-repl--log '(:agent-repl-central "the popup utility is path-scoped") "elisp.popup.open: line ignored reason=directory path=%s line=%s"
                         expanded line))
      (agent-repl-popup--display buffer)
      (agent-repl--info '(:agent-repl-central "the popup utility is path-scoped") "elisp.popup.open: opened path=%s line=%s buffer=%s"
                        expanded (or line "none") (buffer-name buffer))
      buffer)))

(provide 'agent-repl-popup)
;;; popup.el ends here
