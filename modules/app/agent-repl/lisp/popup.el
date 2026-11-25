;;; popup.el --- the one shared agent-repl popup subroutine -*- lexical-binding: t; -*-

;;; Commentary:

;; ONE SUBROUTINE BACKS EVERY POPUP AGENT-REPL SHOWS.  Every such popup
;; takes the place of the current workspace's VIEW (the webapp's window): it
;; is shown in that window, focused, and closed by `q' in command mode, which
;; kills its buffer and puts the view back.  The composer below the view and
;; the sidebar beside it are never touched, so neither moves nor narrows.
;; Only the buffer's own major and minor modes differ between popups.
;;
;; Two entry points share that one display:
;;
;;   - `agent-repl-popup-show' shows a BUFFER: the daemon build log, the
;;     health report, a selected response's markdown, and the branch log a
;;     landed merge's commit opens (magit.el's `agent-repl-magit-log-commit',
;;     which hands it to magit as `magit-display-buffer-function').
;;   - `agent-repl-popup-open' shows a PATH[:line]: dired for a directory,
;;     the file otherwise.  Its callers are the plan bubble's edit button,
;;     every findings-row location jump, the worktree separation-divider
;;     paths, commands.el's link-code, notes.el's notes file, and the
;;     `open_in_editor' push arm of `agentrepl.v1' WatchHostWorkspace
;;     (`HostOpenInEditor {path, optional line}').
;;
;; DIVERGENCE BETWEEN CALL SITES IS A DEFECT: the consistency requirement is
;; code-level, the same ruling as the blink cadence.  A call site never
;; chooses a window for one of these buffers itself, and adding a per-caller
;; variant here is the defect this file exists to prevent.
;;
;; THE WINDOW A POPUP TAKES (`agent-repl-popup--host-window'), in order:
;; the window another popup already stands in (the new popup replaces it,
;; and inherits what it stood in for); the current workspace's view window;
;; the selected window when it is in the main area; the main area's largest
;; window.  The popup buffer records what it stands in for
;; (`agent-repl-popup--stand-in'), and its `kill-buffer-hook' puts that back,
;; so the view returns however the popup's buffer dies.  While a popup
;; stands in for the view, the layout reconciler leaves the lone composer
;; alone (`agent-repl-popup-standing-in-for').
;;
;; LINE is 1-INDEXED, exactly as `HostOpenInEditor.line' is; nil means the
;; file's top (and it is always nil for a directory, which has no lines).

;;; Code:

;; Every log call is a log-site macro (log-site.el), needed where this
;; file is expanded; config.el loads it first.
(require 'log-site)
(require 'seq)

(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))
(declare-function agent-repl--ws-current-name "workspace" ())
(declare-function agent-repl-window--panel-window "window" (kind &optional ws frame))
(declare-function agent-repl-window--side-window-p "window" (win &optional ws))
(declare-function agent-repl--frontend-largest-main-area-window "frontend" ())

(defconst agent-repl-popup--log-scope
  '(:agent-repl-central "the popup utility is buffer-scoped, not workspace-scoped")
  "The log scope every popup record is filed under.")

(defvar-local agent-repl-popup--owned nil
  "Non-nil in a buffer `agent-repl-popup-show' displayed; it dies with the buffer.")

(defvar-local agent-repl-popup--stand-in nil
  "What this popup buffer's window showed before the popup took it.
A plist: `:window' the window, `:buffer' the buffer it showed and
`:dedicated' its dedication then.  Put back when this buffer dies
\=(`agent-repl-popup--put-back'); nil once put back or handed to a popup
that replaced this one.")

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

(defun agent-repl-popup-buffer-p (buffer-or-name)
  "Return non-nil when BUFFER-OR-NAME was displayed by `agent-repl-popup-show'."
  (let ((buffer (get-buffer buffer-or-name)))
    (and (buffer-live-p buffer)
         (buffer-local-value 'agent-repl-popup--owned buffer)
         t)))

(defun agent-repl-popup--standing-window (&optional frame)
  "Return FRAME's window a popup stands in, or nil.
A popup stands in a window while that window shows it and its
`agent-repl-popup--stand-in' names that window.  FRAME defaults to the
selected frame."
  (seq-find (lambda (win)
              (let ((buffer (window-buffer win)))
                (and (agent-repl-popup-buffer-p buffer)
                     (eq (plist-get (buffer-local-value 'agent-repl-popup--stand-in buffer)
                                    :window)
                         win))))
            (window-list frame 'nomini)))

(defun agent-repl-popup-standing-in-for (buffer &optional frame)
  "Return FRAME's window where a popup stands in for BUFFER, or nil.
The layout reconciler asks this of a workspace's view buffer: a view a
popup replaced is not a view that went missing."
  (let ((win (agent-repl-popup--standing-window frame)))
    (and win
         buffer
         (eq (plist-get (buffer-local-value 'agent-repl-popup--stand-in (window-buffer win))
                        :buffer)
             buffer)
         win)))

(defun agent-repl-popup--host-window ()
  "Return the window the next popup takes, and why, as (WINDOW . REASON).
See the Commentary for the order.  A frame with no main-area window at
all breaks the frame's own invariant, and signals."
  (let ((standing (agent-repl-popup--standing-window))
        (view (let ((ws (agent-repl--ws-current-name)))
                (and ws (agent-repl-window--panel-window :view ws))))
        (selected (selected-window)))
    (cond
     (standing (cons standing "replaces-popup"))
     (view (cons view "view"))
     ((not (agent-repl-window--side-window-p selected)) (cons selected "selected"))
     (t
      (let ((largest (agent-repl--frontend-largest-main-area-window)))
        (unless (window-live-p largest)
          (agent-repl--error agent-repl-popup--log-scope "elisp.popup.host-window: rejected reason=no-main-area-window frame={frame}"
                             :frame (prin1-to-string (selected-frame)))
          (error "agent-repl: the frame has no main-area window to show a popup in"))
        (cons largest "largest-main-area"))))))

(defun agent-repl-popup--put-back ()
  "Put back what the current popup buffer's window showed before it.
The popup buffer's `kill-buffer-hook'.  Does nothing once the popup has
handed its window to a popup that replaced it, or when its window no
longer shows it.  A put-back buffer that died meanwhile leaves the
window to Emacs, which shows another buffer there as it does for every
killed buffer."
  (let* ((stand-in agent-repl-popup--stand-in)
         (win (plist-get stand-in :window))
         (prior (plist-get stand-in :buffer)))
    (setq agent-repl-popup--stand-in nil)
    (cond
     ((null stand-in) nil)
     ((not (and (window-live-p win) (eq (window-buffer win) (current-buffer))))
      (agent-repl--log agent-repl-popup--log-scope "elisp.popup.put-back: skipped reason=window-moved-on buffer={buffer} window={window}"
                       :buffer (buffer-name) :window (prin1-to-string win)))
     ((not (buffer-live-p prior))
      (agent-repl--info agent-repl-popup--log-scope "elisp.popup.put-back: skipped reason=prior-buffer-dead buffer={buffer} window={window}"
                        :buffer (buffer-name) :window (prin1-to-string win)))
     (t
      (set-window-buffer win prior)
      (set-window-dedicated-p win (plist-get stand-in :dedicated))
      (agent-repl--log agent-repl-popup--log-scope "elisp.popup.put-back: buffer={buffer} window={window} restored={restored}"
                       :buffer (buffer-name) :window (prin1-to-string win) :restored (buffer-name prior))))))

(defun agent-repl-popup--save-if-visiting ()
  "Save the current buffer when it visits a file and has unsaved edits.
A popup's close never loses an edit to a notes or source file."
  (when (and buffer-file-name (buffer-modified-p))
    (agent-repl--info agent-repl-popup--log-scope "elisp.popup.autosave: buffer={buffer} file={file}"
                      :buffer (buffer-name) :file buffer-file-name)
    (save-buffer)))

(defun agent-repl-popup--close (buffer)
  "Close the popup BUFFER: save its file edits, then kill it.
Its `kill-buffer-hook' puts back what it stood in for."
  (with-current-buffer buffer
    (agent-repl-popup--save-if-visiting))
  (kill-buffer buffer))

;;;###autoload
(defun agent-repl-popup-show (buffer)
  "Show BUFFER in the shared agent-repl popup and return its window.
Takes the window `agent-repl-popup--host-window' names (the current
workspace's view, normally), selects it, and enables
`agent-repl-popup-mode' in BUFFER, so `q' closes it, kills it and puts
back what the window showed.  No other window moves or resizes."
  (unless (buffer-live-p buffer)
    (agent-repl--error agent-repl-popup--log-scope "elisp.popup.show: rejected reason=dead-buffer buffer={buffer}"
                       :buffer (prin1-to-string buffer))
    (error "agent-repl: cannot show a dead buffer in a popup: %S" buffer))
  (let* ((host (agent-repl-popup--host-window))
         (win (car host))
         (shown (window-buffer win))
         (replaced (and (equal (cdr host) "replaces-popup") (not (eq shown buffer)) shown)))
    (cond
     ((eq shown buffer) nil)
     (replaced
      ;; The replaced popup hands over what it stood in for, so the view
      ;; comes back when the NEW popup closes, never the old popup.
      (let ((stand-in (buffer-local-value 'agent-repl-popup--stand-in replaced)))
        (with-current-buffer replaced (setq agent-repl-popup--stand-in nil))
        (with-current-buffer buffer (setq agent-repl-popup--stand-in stand-in))))
     (t
      (with-current-buffer buffer
        (setq agent-repl-popup--stand-in
              (list :window win :buffer shown :dedicated (window-dedicated-p win))))))
    (with-current-buffer buffer
      (setq agent-repl-popup--owned t)
      (add-hook 'kill-buffer-hook #'agent-repl-popup--put-back nil t)
      (agent-repl-popup-mode 1))
    (set-window-dedicated-p win nil)
    (set-window-buffer win buffer)
    (select-window win)
    (when replaced
      (agent-repl-popup--close replaced))
    (agent-repl--log agent-repl-popup--log-scope "elisp.popup.show: buffer={buffer} window={window} host={host} replaced={replaced}"
                     :buffer (buffer-name buffer) :window (prin1-to-string win) :host (cdr host)
                     :replaced (if replaced (buffer-name replaced) "none"))
    win))

(defun agent-repl-popup-quit ()
  "Close the current agent-repl popup and put back what it stood in for.
Bound to `q' in Evil normal state inside every agent-repl popup."
  (interactive)
  (agent-repl--log agent-repl-popup--log-scope "elisp.popup.quit: buffer={buffer}" :buffer (buffer-name))
  (agent-repl-popup--close (current-buffer)))

(defun agent-repl-popup--path-buffer (path)
  "Return the buffer showing PATH: dired for a directory, the file otherwise.
PATH must already exist — the caller checks, because a missing path is a
refusal and not a buffer this function could invent."
  (if (file-directory-p path)
      (progn
        (agent-repl--log agent-repl-popup--log-scope "elisp.popup.buffer: kind=directory path={path}" :path path)
        (dired-noselect path))
    (agent-repl--log agent-repl-popup--log-scope "elisp.popup.buffer: kind=file path={path}" :path path)
    (find-file-noselect path)))

(defun agent-repl-popup--goto-line (buffer line)
  "Move point in BUFFER to LINE, 1-indexed, and return the position.
A LINE past the end of the buffer lands on the last line rather than
failing: the daemon's line is a hint at a location, and a stale hint must
still open the file."
  (with-current-buffer buffer
    (goto-char (point-min))
    (forward-line (1- line))
    (agent-repl--log agent-repl-popup--log-scope "elisp.popup.goto-line: buffer={buffer} line={line} point={point}"
                     :buffer (buffer-name buffer) :line line :point (point))
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
    (agent-repl--error agent-repl-popup--log-scope "elisp.popup.open: rejected reason=empty-path path={path}" :path (prin1-to-string path))
    (user-error "agent-repl: no path to open"))
  (let ((expanded (expand-file-name path)))
    (unless (file-exists-p expanded)
      (agent-repl--error agent-repl-popup--log-scope "elisp.popup.open: rejected reason=missing-path path={path}"
                         :path expanded)
      (user-error "agent-repl: no such path: %s" expanded))
    (let ((buffer (agent-repl-popup--path-buffer expanded)))
      (when (and line (not (file-directory-p expanded)))
        (agent-repl-popup--goto-line buffer line))
      (when (and line (file-directory-p expanded))
        (agent-repl--log agent-repl-popup--log-scope "elisp.popup.open: line ignored reason=directory path={path} line={line}"
                         :path expanded :line line))
      (agent-repl-popup-show buffer)
      (agent-repl--info agent-repl-popup--log-scope "elisp.popup.open: opened path={path} line={line} buffer={buffer}"
                        :path expanded :line (or line "none") :buffer (buffer-name buffer))
      buffer)))

(provide 'agent-repl-popup)
;;; popup.el ends here
