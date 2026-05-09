;;; drawer.el --- Workspace drawer side-window for claude-repl -*- lexical-binding: t; -*-

;;; Commentary:

;; Read-only side-window listing every claude-repl workspace.  Lives "above"
;; the workspaces themselves: it is rendered into a left-side slot and is
;; not tied to any single workspace's window configuration.
;;
;; Each line shows: priority, claude-state icon, workspace name, and the
;; rendered aiTitle (`:last-prompt-summary').  Hidden workspaces appear
;; below a separator at the bottom.
;;
;; This is the read-only first cut: navigation (n/p) and RET-to-switch
;; only.  Sending prompts, interrupting, toggling hidden, etc. are
;; intentionally deferred.

;;; Code:

(require 'subr-x)

;;;; Customization ----------------------------------------------------------

(defcustom claude-repl-drawer-buffer-name "*claude-repl-drawer*"
  "Buffer name for the claude-repl workspace drawer."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-drawer-width-fraction 0.20
  "Fraction of the frame width the drawer should occupy.
Computed against `frame-width' at display time so the drawer scales
with the frame.  Capped at 20% by default — the drawer is meant to
stay open during work, not dominate the layout."
  :type 'float
  :group 'claude-repl)

(defcustom claude-repl-drawer-state-icons
  '((:init        . "⏳")
    (:thinking    . "⌛")
    (:done        . "✅")
    (:idle        . "💤")
    (:permission  . "❓")
    (:stop-failed . "❗")
    (:dead        . "❌"))
  "Alist mapping claude-state keyword to an indicator glyph.
The :dead entry is used when `:repl-state' is `:dead' (overrides
:claude-state).  Unrecognized values fall through to a single middot
placeholder, used for workspaces registered but with no live session."
  :type '(alist :key-type symbol :value-type string)
  :group 'claude-repl)

;; Force-apply the latest palette on every (re)load.  `defcustom' only
;; initializes the value when the symbol is unbound, so palette tweaks
;; otherwise require an Emacs restart to take effect.  Source is the
;; canonical palette in this personal config; `M-x customize' values
;; for this variable will be overwritten on reload.
(setq claude-repl-drawer-state-icons
      (eval (car (get 'claude-repl-drawer-state-icons 'standard-value))))

(defcustom claude-repl-drawer-state-icon-default "·"
  "Glyph shown when a workspace has no recognized claude-state.
Used for registered-but-not-yet-started workspaces (claude-state nil)."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-drawer-section-rule-width 12
  "Number of `─' characters in section header rule lines."
  :type 'integer
  :group 'claude-repl)

(defcustom claude-repl-drawer-empty-section-label "(none)"
  "Placeholder shown under a section header when the section has no entries."
  :type 'string
  :group 'claude-repl)

;;;; Faces ------------------------------------------------------------------

(defface claude-repl-drawer-workspace-name
  '((t :weight bold))
  "Face for the workspace name line in the drawer."
  :group 'claude-repl)

(defface claude-repl-drawer-current-workspace
  '((t :inherit highlight :weight bold))
  "Face for the currently active workspace line in the drawer."
  :group 'claude-repl)

(defface claude-repl-drawer-summary
  '((t :inherit shadow :slant italic))
  "Face for the aiTitle/prompt-summary subtitle line."
  :group 'claude-repl)

(defface claude-repl-drawer-hidden
  '((t :inherit shadow))
  "Face used to dim hidden workspaces."
  :group 'claude-repl)

(defface claude-repl-drawer-section-title
  '((t :weight bold :inherit font-lock-keyword-face))
  "Face for the MAIN/HIDDEN section title line."
  :group 'claude-repl)

(defface claude-repl-drawer-section-rule
  '((t :inherit shadow))
  "Face for the rule line beneath a section title."
  :group 'claude-repl)

(defface claude-repl-drawer-empty
  '((t :inherit shadow :slant italic))
  "Face for the placeholder shown under an empty section."
  :group 'claude-repl)

;;;; Mode -------------------------------------------------------------------

(defvar claude-repl-drawer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n")     #'claude-repl-drawer-next)
    (define-key map (kbd "j")     #'claude-repl-drawer-next)
    (define-key map (kbd "<down>") #'claude-repl-drawer-next)
    (define-key map (kbd "p")     #'claude-repl-drawer-prev)
    (define-key map (kbd "k")     #'claude-repl-drawer-prev)
    (define-key map (kbd "<up>")  #'claude-repl-drawer-prev)
    (define-key map (kbd "RET")   #'claude-repl-drawer-visit)
    (define-key map (kbd "g")     #'claude-repl-drawer-refresh)
    (define-key map (kbd "q")     #'claude-repl-drawer-hide)
    map)
  "Keymap for `claude-repl-drawer-mode'.")

(define-derived-mode claude-repl-drawer-mode special-mode "ClaudeDrawer"
  "Major mode for the claude-repl workspace drawer."
  (setq truncate-lines t
        buffer-read-only t
        cursor-type 'box
        mode-line-format nil)
  (hl-line-mode 1))

;;;; Sorting + selection helpers --------------------------------------------

(defun claude-repl-drawer--workspace-hidden-p (ws)
  "Return non-nil if workspace WS is in the `:hidden' repl-state."
  (eq (claude-repl--ws-get ws :repl-state) :hidden))

(defun claude-repl-drawer--sort-key (ws)
  "Return a sort key for workspace name WS.
Lower keys come first.  Sort by `:priority' rank, then name."
  (cons (claude-repl--priority-rank (claude-repl--ws-get ws :priority))
        ws))

(defun claude-repl-drawer--sort (names)
  "Return NAMES sorted by priority rank, then alphabetically."
  (sort (copy-sequence names)
        (lambda (a b)
          (let ((ka (claude-repl-drawer--sort-key a))
                (kb (claude-repl-drawer--sort-key b)))
            (or (< (car ka) (car kb))
                (and (= (car ka) (car kb))
                     (string< (cdr ka) (cdr kb))))))))

(defun claude-repl-drawer--partition (names)
  "Return (VISIBLE . HIDDEN) lists from NAMES, each sorted."
  (let (visible hidden)
    (dolist (ws names)
      (if (claude-repl-drawer--workspace-hidden-p ws)
          (push ws hidden)
        (push ws visible)))
    (cons (claude-repl-drawer--sort visible)
          (claude-repl-drawer--sort hidden))))

;;;; Render -----------------------------------------------------------------

(defun claude-repl-drawer--state-glyph (ws)
  "Return the indicator glyph for workspace WS."
  (let ((repl-state (claude-repl--ws-get ws :repl-state))
        (claude-state (claude-repl--ws-get ws :claude-state)))
    (or (and (eq repl-state :dead)
             (alist-get :dead claude-repl-drawer-state-icons))
        (alist-get claude-state claude-repl-drawer-state-icons)
        claude-repl-drawer-state-icon-default)))

(defun claude-repl-drawer--priority-display (priority)
  "Return a display string for PRIORITY.
Uses the badge PNG from `claude-repl--priority-images' when available,
falling back to the raw PRIORITY string for terminal/batch contexts.
Returns the empty string when PRIORITY is nil so unprioritized
workspaces don't carry a phantom space."
  (cond
   ((null priority) "")
   ((claude-repl--priority-image priority)
    (propertize priority 'display (claude-repl--priority-image priority)))
   (t priority)))

(defun claude-repl-drawer--name-face (ws)
  "Return the face spec for WS's name, colored by claude-state.
:dead falls through to the default workspace-name face (the existing
hidden/dim treatment provides the muting).  Unrecognized states render
as plain bold."
  (let* ((repl-state   (claude-repl--ws-get ws :repl-state))
         (claude-state (claude-repl--ws-get ws :claude-state))
         (color (cond
                 ((eq repl-state :dead)        nil)
                 ((eq claude-state :init)      claude-repl--color-init-blue)
                 ((eq claude-state :thinking)  claude-repl--color-thinking-red)
                 ((memq claude-state '(:done :permission))
                  claude-repl--color-done-green)
                 ((eq claude-state :idle)      claude-repl--color-idle-orange)
                 ((eq claude-state :stop-failed)
                  claude-repl--color-stop-failed-magenta))))
    (if color
        `(:foreground ,color :weight bold)
      'claude-repl-drawer-workspace-name)))

(defun claude-repl-drawer--summary-text (ws)
  "Return the aiTitle/prompt-summary string for WS, or a placeholder."
  (let ((summary (claude-repl--ws-get ws :last-prompt-summary))
        (pending (claude-repl--ws-get ws :last-prompt-summary-pending)))
    (cond
     ((and (stringp summary) (not (string-empty-p summary)))
      summary)
     (pending "…")
     (t "—"))))

(defun claude-repl-drawer--current-ws ()
  "Return the currently active workspace name, or nil."
  (and (fboundp '+workspace-current-name) (+workspace-current-name)))

(defun claude-repl-drawer--render-workspace (ws current hidden)
  "Insert the rendered representation for workspace WS into the current buffer.
CURRENT is the active workspace name (for highlighting).  HIDDEN means
the workspace is in the hidden section and should be dimmed."
  (let* ((priority  (claude-repl--ws-get ws :priority))
         (glyph     (claude-repl-drawer--state-glyph ws))
         (dirty     (eq (claude-repl--ws-get ws :git-clean) 'dirty))
         (selected  (and current (equal ws current)))
         (start     (point))
         (prio-disp (claude-repl-drawer--priority-display priority))
         (sep       (if priority " " ""))
         (name-face (claude-repl-drawer--name-face ws))
         (header    (concat "  " glyph " " prio-disp sep
                            (propertize ws 'face name-face)
                            (if dirty " ●" "")))
         (summary   (claude-repl-drawer--summary-text ws)))
    (insert header "\n")
    (insert "    "
            (propertize summary 'face 'claude-repl-drawer-summary)
            "\n")
    (let ((end (point)))
      (add-text-properties
       start end
       (list 'claude-repl-drawer-workspace ws
             'help-echo (format "Workspace: %s%s"
                                ws
                                (if hidden " (hidden)" ""))))
      (cond
       (selected
        (add-face-text-property start end 'claude-repl-drawer-current-workspace))
       (hidden
        (add-face-text-property start end 'claude-repl-drawer-hidden))))))

(defun claude-repl-drawer--insert-section-header (label)
  "Insert a bold section header LABEL with a rule line beneath it."
  (insert (propertize (format " %s\n" label)
                      'face 'claude-repl-drawer-section-title))
  (insert (propertize (concat " "
                              (make-string claude-repl-drawer-section-rule-width
                                           ?─)
                              "\n")
                      'face 'claude-repl-drawer-section-rule)))

(defun claude-repl-drawer--insert-section (label workspaces current hidden)
  "Render a section titled LABEL containing WORKSPACES.
CURRENT highlights the active workspace.  HIDDEN dims the entries when
they live in the hidden section."
  (claude-repl-drawer--insert-section-header label)
  (if (null workspaces)
      (insert (propertize (format "  %s\n"
                                  claude-repl-drawer-empty-section-label)
                          'face 'claude-repl-drawer-empty))
    (dolist (ws workspaces)
      (claude-repl-drawer--render-workspace ws current hidden))))

(defun claude-repl-drawer--render ()
  "Render the drawer contents from `claude-repl--workspaces' into the current buffer."
  (let* ((inhibit-read-only t)
         (saved-line (line-number-at-pos))
         (saved-col  (current-column))
         (current    (claude-repl-drawer--current-ws))
         (parts      (claude-repl-drawer--partition
                      (hash-table-keys claude-repl--workspaces)))
         (visible    (car parts))
         (hidden     (cdr parts)))
    (erase-buffer)
    (claude-repl-drawer--insert-section "MAIN" visible current nil)
    (insert "\n")
    (claude-repl-drawer--insert-section "HIDDEN" hidden current t)
    (goto-char (point-min))
    (forward-line (1- saved-line))
    (move-to-column saved-col)))

;;;; Navigation -------------------------------------------------------------

(defun claude-repl-drawer--workspace-at-point ()
  "Return the workspace name at point, or nil."
  (get-text-property (point) 'claude-repl-drawer-workspace))

(defun claude-repl-drawer--goto-workspace-line (ws)
  "Move point to the start of the line for workspace WS, if present.
Returns non-nil on success."
  (let ((target nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not target) (not (eobp)))
        (when (equal (get-text-property (point) 'claude-repl-drawer-workspace) ws)
          (setq target (point)))
        (forward-line 1)))
    (when target
      (goto-char target)
      t)))

(defun claude-repl-drawer-next ()
  "Move point to the next workspace entry."
  (interactive)
  (let ((current (claude-repl-drawer--workspace-at-point))
        (start   (point))
        (found   nil))
    (forward-line 1)
    (while (and (not found) (not (eobp)))
      (let ((ws (claude-repl-drawer--workspace-at-point)))
        (if (and ws (not (equal ws current)))
            (setq found t)
          (forward-line 1))))
    (unless found
      (goto-char start))))

(defun claude-repl-drawer-prev ()
  "Move point to the previous workspace entry."
  (interactive)
  (let ((current (claude-repl-drawer--workspace-at-point))
        (start   (point))
        (found   nil))
    (forward-line -1)
    (while (and (not found) (not (bobp)))
      (let ((ws (claude-repl-drawer--workspace-at-point)))
        (if (and ws (not (equal ws current)))
            (setq found t)
          (forward-line -1))))
    (when found
      ;; Snap to the start of the workspace block (handles the summary
      ;; subtitle line being the first one we land on when moving up).
      (let ((ws (claude-repl-drawer--workspace-at-point)))
        (while (and (not (bobp))
                    (equal (get-text-property (1- (point))
                                              'claude-repl-drawer-workspace)
                           ws))
          (forward-line -1))))
    (unless found
      (goto-char start))))

;;;; Commands ---------------------------------------------------------------

(defun claude-repl-drawer-visit ()
  "Switch to the workspace at point."
  (interactive)
  (let ((ws (claude-repl-drawer--workspace-at-point)))
    (unless ws
      (user-error "No workspace at point"))
    (claude-repl--log ws "drawer-visit: ws=%s" ws)
    (+workspace-switch ws)))

(defun claude-repl-drawer-refresh ()
  "Manually refresh the drawer contents."
  (interactive)
  (when-let ((buf (get-buffer claude-repl-drawer-buffer-name)))
    (with-current-buffer buf
      (claude-repl-drawer--render))))

(defun claude-repl-drawer--refresh-if-visible ()
  "Refresh the drawer if its buffer exists and is shown in some window.
Intended to be called from the 1Hz poll in `status.el'."
  (when-let* ((buf (get-buffer claude-repl-drawer-buffer-name))
              ((get-buffer-window buf t)))
    (with-current-buffer buf
      (claude-repl-drawer--render))))

;;;; Display + toggle -------------------------------------------------------

(defvar claude-repl-drawer--display-action
  `((display-buffer-in-side-window)
    (side . left)
    (slot . 0)
    (window-width . ,#'claude-repl-drawer--window-width)
    (window-parameters
     (no-delete-other-windows . t)
     (no-other-window . nil)))
  "Display action for the drawer buffer.")

(defun claude-repl-drawer--window-width (window)
  "Return the configured drawer width in columns for WINDOW.
Computed as `claude-repl-drawer-width-fraction' of the frame width."
  (max 1
       (round (* claude-repl-drawer-width-fraction
                 (frame-width (window-frame window))))))

(defun claude-repl-drawer--get-or-create-buffer ()
  "Return the drawer buffer, creating and initializing if necessary."
  (let ((buf (get-buffer claude-repl-drawer-buffer-name)))
    (unless (buffer-live-p buf)
      (setq buf (get-buffer-create claude-repl-drawer-buffer-name))
      (with-current-buffer buf
        (claude-repl-drawer-mode)))
    buf))

(defun claude-repl-drawer--goto-first-workspace ()
  "Move point to the first workspace line in the current buffer, if any."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (if (claude-repl-drawer--workspace-at-point)
          (setq found t)
        (forward-line 1)))
    found))

(defun claude-repl-drawer-show ()
  "Show the workspace drawer in a left-side window."
  (interactive)
  (let* ((buf (claude-repl-drawer--get-or-create-buffer))
         (win (display-buffer buf claude-repl-drawer--display-action)))
    (with-current-buffer buf
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace))
    (when win
      (set-window-dedicated-p win t))
    win))

(defun claude-repl-drawer-hide ()
  "Hide the workspace drawer."
  (interactive)
  (when-let ((buf (get-buffer claude-repl-drawer-buffer-name)))
    (dolist (win (get-buffer-window-list buf nil t))
      (when (window-live-p win)
        (set-window-dedicated-p win nil)
        (delete-window win)))))

(defun claude-repl-drawer-toggle ()
  "Toggle visibility of the workspace drawer."
  (interactive)
  (if-let* ((buf (get-buffer claude-repl-drawer-buffer-name))
            (win (get-buffer-window buf t)))
      (claude-repl-drawer-hide)
    (claude-repl-drawer-show)))

(provide 'claude-repl-drawer)
;;; drawer.el ends here
