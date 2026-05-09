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

(defcustom claude-repl-drawer-width 38
  "Column width of the claude-repl drawer side-window."
  :type 'integer
  :group 'claude-repl)

(defcustom claude-repl-drawer-state-icons
  '((:init       . "◔")
    (:thinking   . "◍")
    (:done       . "✓")
    (:idle       . "·")
    (:permission . "❓")
    (:stop-failed . "⚠")
    (:dead       . "❌"))
  "Alist mapping claude-state keyword to an indicator glyph.
The :dead entry is used when `:repl-state' is `:dead' (overrides
:claude-state).  Unrecognized values fall through to a single space."
  :type '(alist :key-type symbol :value-type string)
  :group 'claude-repl)

(defcustom claude-repl-drawer-hidden-separator "── hidden ──"
  "Separator line between visible and hidden workspaces in the drawer."
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

(defface claude-repl-drawer-separator
  '((t :inherit shadow :weight bold))
  "Face for the hidden-section separator line."
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
        " ")))

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
  (let* ((priority (claude-repl--ws-get ws :priority))
         (glyph    (claude-repl-drawer--state-glyph ws))
         (dirty    (eq (claude-repl--ws-get ws :git-clean) 'dirty))
         (selected (and current (equal ws current)))
         (start    (point))
         (header   (format "  %-3s %s %s%s"
                           (or priority "")
                           glyph
                           ws
                           (if dirty " ●" "")))
         (summary  (claude-repl-drawer--summary-text ws)))
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
        (add-face-text-property start end 'claude-repl-drawer-hidden))
       (t
        ;; Apply name face only to the header line.
        (let ((header-end (save-excursion (goto-char start) (line-end-position))))
          (add-face-text-property start header-end 'claude-repl-drawer-workspace-name)))))))

(defun claude-repl-drawer--render ()
  "Render the drawer contents from `claude-repl--workspaces' into the current buffer."
  (let ((inhibit-read-only t)
        (saved-line (line-number-at-pos))
        (saved-col  (current-column))
        (current    (claude-repl-drawer--current-ws))
        (parts      (claude-repl-drawer--partition
                     (hash-table-keys claude-repl--workspaces))))
    (erase-buffer)
    (let ((visible (car parts))
          (hidden  (cdr parts)))
      (if (and (null visible) (null hidden))
          (insert (propertize "  (no claude-repl workspaces)\n"
                              'face 'claude-repl-drawer-summary))
        (dolist (ws visible)
          (claude-repl-drawer--render-workspace ws current nil))
        (when hidden
          (when visible (insert "\n"))
          (insert (propertize (format "  %s\n" claude-repl-drawer-hidden-separator)
                              'face 'claude-repl-drawer-separator))
          (dolist (ws hidden)
            (claude-repl-drawer--render-workspace ws current t)))))
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

(defun claude-repl-drawer--window-width (_window)
  "Return the configured drawer width."
  claude-repl-drawer-width)

(defun claude-repl-drawer--get-or-create-buffer ()
  "Return the drawer buffer, creating and initializing if necessary."
  (let ((buf (get-buffer claude-repl-drawer-buffer-name)))
    (unless (buffer-live-p buf)
      (setq buf (get-buffer-create claude-repl-drawer-buffer-name))
      (with-current-buffer buf
        (claude-repl-drawer-mode)))
    buf))

(defun claude-repl-drawer-show ()
  "Show the workspace drawer in a left-side window."
  (interactive)
  (let* ((buf (claude-repl-drawer--get-or-create-buffer))
         (win (display-buffer buf claude-repl-drawer--display-action)))
    (with-current-buffer buf
      (claude-repl-drawer--render))
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
