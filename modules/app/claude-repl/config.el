;;; app/claude-repl/config.el -*- lexical-binding: t; -*-

;; Workspace identity
(defvar-local claude-repl--project-root nil
  "Buffer-local git root for Claude REPL buffers.
Set when vterm/input buffers are created so workspace-id works from them.")

(defun claude-repl--git-root (&optional dir)
  "Find the git root by walking up from DIR (default `default-directory').
Checks for both .git directory and .git file (worktrees)."
  (let ((dir (or dir default-directory)))
    (locate-dominating-file dir
      (lambda (d)
        (let ((git (expand-file-name ".git" d)))
          (or (file-directory-p git) (file-regular-p git)))))))

(defun claude-repl--workspace-id ()
  "Return a short identifier for the current git workspace.
Uses an MD5 hash of the git root path.  Falls back to the buffer-local
`claude-repl--project-root' and then `default-directory'."
  (let ((root (or (claude-repl--git-root)
                  claude-repl--project-root
                  default-directory)))
    (when root
      (substring (md5 root) 0 8))))

;; Transient globals — always set by `claude-repl--load-session' before use.
;; No defvar needed; these are just scratch space swapped per-project.
;; claude-repl-vterm-buffer, claude-repl-input-buffer, claude-repl-return-window,
;; claude-repl--saved-window-config

(defvar-local claude-repl-hide-overlay nil
  "Overlay used to hide Claude CLI input box.")

;; Input history
(defvar-local claude-repl--input-history nil
  "List of previous inputs, most recent first.")

(defvar-local claude-repl--history-index -1
  "Current position in history. -1 means not browsing.")

(defvar-local claude-repl--history-stash nil
  "Stashed in-progress text saved when history browsing begins.")

(defvar-local claude-repl--history-navigating nil
  "Non-nil while history navigation is replacing buffer text.")

(setq claude-repl-hide-input-box nil)


;; Set to t once Claude has set its terminal title (meaning it's ready).
(defvar-local claude-repl--ready nil
  "Non-nil once Claude Code has finished starting up.")

;; Per-project session storage
(setq claude-repl--sessions (or (bound-and-true-p claude-repl--sessions)
                                (make-hash-table :test 'equal)))

(defun claude-repl--buffer-name (&optional suffix)
  "Return a project-specific buffer name like *claude-HASH* or *claude-input-HASH*.
SUFFIX, if provided, is inserted before the hash (e.g. \"-input\")."
  (let ((id (claude-repl--workspace-id)))
    (format "*claude%s-%s*" (or suffix "") (or id "default"))))

(defun claude-repl--load-session ()
  "Load the current project's session state into global vars.
Sets `claude-repl-vterm-buffer', `claude-repl-input-buffer' from buffer names,
and restores window config from the sessions hash table."
  (let* ((id (claude-repl--workspace-id))
         (vterm-name (claude-repl--buffer-name))
         (input-name (claude-repl--buffer-name "-input"))
         (session (gethash id claude-repl--sessions)))
    (setq claude-repl-vterm-buffer (get-buffer vterm-name)
          claude-repl-input-buffer (get-buffer input-name)
          claude-repl--saved-window-config (plist-get session :saved-window-config)
          claude-repl-return-window (plist-get session :return-window))))

(defun claude-repl--save-session ()
  "Save the current global state back to the sessions hash table."
  (let ((id (claude-repl--workspace-id)))
    (puthash id
             (list :saved-window-config claude-repl--saved-window-config
                   :return-window claude-repl-return-window)
             claude-repl--sessions)))

;; Override vterm--get-color so claude vterm buffers get a black background.
;; Solaire-mode advises this function and face-background ignores buffer-local
;; remaps, so we hardcode black for claude buffers' default background case.
(after! vterm
  (advice-add 'vterm--get-color :around
              (lambda (fn index &rest args)
                (let ((result (apply fn index args)))
                  (if (and (not (member :foreground args))
                           (= index -1)
                           (not (member :inverse-video args))
                           (string-match-p "^\\*claude-[0-9a-f]" (buffer-name)))
                      (claude-repl--grey 15)
                    result)))))

;; Manual window layout: vterm on the right (full height), input below vterm.
(defun claude-repl--show-panels ()
  "Display vterm and input panels to the right of the current window.
Splits right for vterm (55% of frame), then splits vterm bottom for input (30%)."
  (let* ((work-win (or claude-repl-return-window (selected-window)))
         (vterm-win (split-window work-win (round (* 0.6 (window-total-width work-win))) 'right))
         (input-win (split-window vterm-win (round (* -0.15 (window-total-height vterm-win))) 'below)))
    (claude-repl--refresh-vterm)
    (set-window-buffer vterm-win claude-repl-vterm-buffer)
    (set-window-buffer input-win claude-repl-input-buffer)
    (set-window-dedicated-p vterm-win t)
    (set-window-dedicated-p input-win t)
    ;; Lock width to prevent resize-triggered reflow in vterm
    (set-window-parameter vterm-win 'window-size-fixed 'width)
    (select-window input-win)
    (evil-insert-state)))

(defun claude-repl--grey (n)
  "Return a hex color string for greyscale value N (0=black, 255=white)."
  (format "#%02x%02x%02x" n n n))

;; Instructions bar face
(defface claude-repl-header-line
  '((t :background "white" :foreground "black" :weight bold))
  "Face for the Claude Input header line.")

;; Input mode
(define-derived-mode claude-input-mode fundamental-mode "Claude Input"
  "Major mode for Claude REPL input buffer."
  (setq-local header-line-format
              "C-RET: send+hide | C-c C-c: clear+save | C-c C-k: interrupt | <up>/<down>: history")
  (face-remap-add-relative 'header-line 'claude-repl-header-line)
  (face-remap-add-relative 'default :background (claude-repl--grey 37))
  (face-remap-add-relative 'fringe :background (claude-repl--grey 37))
  (add-hook 'after-change-functions #'claude-repl--history-on-change nil t))

(map! :map claude-input-mode-map
      :ni "RET"       #'claude-repl-send
      :ni "S-RET"     #'newline
      :ni "C-RET"     #'claude-repl-send-and-hide
      :ni "C-c C-k"   #'claude-repl-interrupt
      :ni "C-c C-c"   (cmd! (claude-repl--history-push) (erase-buffer) (evil-insert-state))
      :ni "C-c y"     (cmd! (claude-repl-send-char "y"))
      :ni "C-c n"     (cmd! (claude-repl-send-char "n"))
      :ni "C-c r"     #'claude-repl-restart
      :ni "C-c q"     #'claude-repl-kill
      :ni "C-S-m"     #'claude-repl-cycle
      :ni "C-S-1"     (cmd! (claude-repl-send-char "1"))
      :ni "C-S-2"     (cmd! (claude-repl-send-char "2"))
      :ni "C-S-3"     (cmd! (claude-repl-send-char "3"))
      :ni "C-S-4"     (cmd! (claude-repl-send-char "4"))
      :ni "C-S-5"     (cmd! (claude-repl-send-char "5"))
      :ni "C-S-6"     (cmd! (claude-repl-send-char "6"))
      :ni "C-S-7"     (cmd! (claude-repl-send-char "7"))
      :ni "C-S-8"     (cmd! (claude-repl-send-char "8"))
      :ni "C-S-9"     (cmd! (claude-repl-send-char "9"))
      :ni "C-S-0"     (cmd! (claude-repl-send-char "0"))
      :ni "C-h"       #'evil-window-left
      :n  "C-n"       (cmd! (claude-repl--load-session)
                            (when (and claude-repl-vterm-buffer (buffer-live-p claude-repl-vterm-buffer))
                              (with-current-buffer claude-repl-vterm-buffer (vterm-send-down))))
      :n  "C-p"       (cmd! (claude-repl--load-session)
                            (when (and claude-repl-vterm-buffer (buffer-live-p claude-repl-vterm-buffer))
                              (with-current-buffer claude-repl-vterm-buffer (vterm-send-up))))
      :ni "<up>"        #'claude-repl--history-prev
      :ni "<down>"      #'claude-repl--history-next)

;; Input history persistence
(defun claude-repl--history-file ()
  "Return the path to the history file for the current project."
  (let ((root (or (claude-repl--git-root)
                  claude-repl--project-root
                  default-directory)))
    (expand-file-name ".claude-repl-history" root)))

(defun claude-repl--history-save ()
  "Write input history to disk."
  (when claude-repl-input-buffer
    (let ((history (buffer-local-value 'claude-repl--input-history claude-repl-input-buffer))
          (file (claude-repl--history-file)))
      (when history
        (with-temp-file file
          (prin1 history (current-buffer)))))))

(defun claude-repl--history-restore ()
  "Load input history from disk into the current buffer."
  (let ((file (claude-repl--history-file)))
    (when (file-exists-p file)
      (setq claude-repl--input-history
            (with-temp-buffer
              (insert-file-contents file)
              (read (current-buffer)))))))

;; Input history functions
(defun claude-repl--history-push ()
  "Save current input buffer text to history.
Skips empty strings and duplicates of the most recent entry.
Resets history browsing index."
  (let ((text (string-trim (buffer-string))))
    (unless (string-empty-p text)
      (unless (equal text (car claude-repl--input-history))
        (push text claude-repl--input-history))))
  (setq claude-repl--history-index -1))

(defun claude-repl--history-prev ()
  "Navigate to the previous (older) history entry."
  (interactive)
  (when claude-repl--input-history
    (let ((next-index (1+ claude-repl--history-index)))
      (when (< next-index (length claude-repl--input-history))
        ;; Stash current text on first navigation
        (when (= claude-repl--history-index -1)
          (setq claude-repl--history-stash (buffer-string)))
        (setq claude-repl--history-index next-index)
        (let ((claude-repl--history-navigating t))
          (erase-buffer)
          (insert (nth claude-repl--history-index claude-repl--input-history)))))))

(defun claude-repl--history-next ()
  "Navigate to the next (newer) history entry, or restore stashed text."
  (interactive)
  (when (>= claude-repl--history-index 0)
    (let ((next-index (1- claude-repl--history-index)))
      (let ((claude-repl--history-navigating t))
        (erase-buffer)
        (if (< next-index 0)
            ;; Past newest entry — restore stash
            (progn
              (setq claude-repl--history-index -1)
              (when claude-repl--history-stash
                (insert claude-repl--history-stash)))
          (setq claude-repl--history-index next-index)
          (insert (nth claude-repl--history-index claude-repl--input-history)))))))

(defun claude-repl--history-on-change (&rest _)
  "Reset history browsing when the user edits the buffer directly."
  (unless claude-repl--history-navigating
    (setq claude-repl--history-index -1)))

;; Core functions
(defun claude-repl-send ()
  "Send input buffer contents to Claude, clear buffer, and return to previous window."
  (interactive)
  (claude-repl--load-session)
  (when (and claude-repl-vterm-buffer (buffer-live-p claude-repl-vterm-buffer))
    (let ((input (with-current-buffer claude-repl-input-buffer
                   (buffer-string))))
      ;; Clear the "done" indicator for this workspace
      (when-let ((ws (claude-repl--workspace-for-buffer claude-repl-vterm-buffer)))
        (remhash ws claude-repl--done-workspaces))
      (with-current-buffer claude-repl-vterm-buffer
        ;; Use vterm's built-in paste mode for large inputs to avoid
        ;; truncation from character-by-character sending.
        (if (> (length input) 200)
            (let ((buf (current-buffer)))
              (vterm-send-string input t)
              (run-at-time 0.1 nil
                           (lambda ()
                             (when (buffer-live-p buf)
                               (with-current-buffer buf
                                 (vterm-send-return)
                                 (claude-repl--refresh-vterm))))))
          (vterm-send-string input)
          (vterm-send-return)
          (claude-repl--refresh-vterm)))
      (with-current-buffer claude-repl-input-buffer
        (claude-repl--history-push)
        (erase-buffer))
      (when (and claude-repl-return-window (window-live-p claude-repl-return-window))
        (select-window claude-repl-return-window)))))

(defun claude-repl-send-and-hide ()
  "Send input to Claude and hide both panels."
  (interactive)
  (claude-repl--load-session)
  (claude-repl-send)
  (if claude-repl--saved-window-config
      (progn
        (set-window-configuration claude-repl--saved-window-config)
        (setq claude-repl--saved-window-config nil))
    (claude-repl--hide-panels)
    (when (and claude-repl-return-window (window-live-p claude-repl-return-window))
      (select-window claude-repl-return-window)))
  (claude-repl--save-session))

(defun claude-repl-send-char (char)
  "Send a single character to Claude."
  (claude-repl--load-session)
  (when (and claude-repl-vterm-buffer (buffer-live-p claude-repl-vterm-buffer))
    (with-current-buffer claude-repl-vterm-buffer
      (vterm-send-string char)
      (vterm-send-return))))

(defun claude-repl--send-to-claude (text)
  "Send TEXT to Claude, starting it if needed."
  (claude-repl--load-session)
  (unless (claude-repl--vterm-running-p)
    (claude-repl--ensure-vterm-buffer)
    (claude-repl--ensure-input-buffer)
    (claude-repl--enable-hide-overlay))
  (with-current-buffer claude-repl-vterm-buffer
    (vterm-send-string text)
    (vterm-send-return))
  (claude-repl--save-session))

(defun claude-repl-explain ()
  "Ask Claude to explain the selected region or current file.
With active region: sends file path and line range.
Without region: sends relative file path."
  (interactive)
  (claude-repl--load-session)
  (let* ((rel-path (file-relative-name (buffer-file-name)
                                        (or (claude-repl--git-root) default-directory)))
         (msg (if (use-region-p)
                  (let ((start-line (line-number-at-pos (region-beginning)))
                        (end-line (line-number-at-pos (region-end))))
                    (deactivate-mark)
                    (format "please explain %s:%d-%d" rel-path start-line end-line))
                (format "please explain %s" rel-path))))
    (claude-repl--send-to-claude msg)))

(defun claude-repl-explain-line ()
  "Ask Claude to explain the current line."
  (interactive)
  (claude-repl--load-session)
  (let* ((rel-path (file-relative-name (buffer-file-name)
                                        (or (claude-repl--git-root) default-directory)))
         (line (line-number-at-pos (point)))
         (msg (format "please explain %s:%d" rel-path line)))
    (claude-repl--send-to-claude msg)))

(defun claude-repl-interrupt ()
  "Send Escape to interrupt Claude."
  (interactive)
  (claude-repl--load-session)
  (when (and claude-repl-vterm-buffer (buffer-live-p claude-repl-vterm-buffer))
    (with-current-buffer claude-repl-vterm-buffer
      (vterm-send-key "<escape>")
      (vterm-send-key "<escape>"))))

;; Hide overlay functions
(defun claude-repl--update-hide-overlay ()
  "Update overlay to hide bottom lines of Claude vterm buffer."
  (when (and claude-repl-vterm-buffer
             (buffer-live-p claude-repl-vterm-buffer))
    (with-current-buffer claude-repl-vterm-buffer
      ;; Remove old overlay if it exists
      (when (and claude-repl-hide-overlay
                 (overlay-buffer claude-repl-hide-overlay))
        (delete-overlay claude-repl-hide-overlay))
      ;; Only create new overlay if enabled and buffer has content
      (when (and claude-repl-hide-input-box (> (point-max) 1))
        (let* ((end (point-max))
               (start (save-excursion
                        (goto-char end)
                        (forward-line -4)
                        (line-beginning-position))))
          (when (< start end)
            (setq claude-repl-hide-overlay (make-overlay start end nil t nil))
            (overlay-put claude-repl-hide-overlay 'display "")
            (overlay-put claude-repl-hide-overlay 'evaporate t)))))))

(defun claude-repl-toggle-hide-input-box ()
  "Toggle hiding of Claude CLI's input box in the vterm buffer."
  (interactive)
  (setq claude-repl-hide-input-box (not claude-repl-hide-input-box))
  (claude-repl--update-hide-overlay)
  (message "Claude input box hiding %s" (if claude-repl-hide-input-box "enabled" "disabled")))

;; Notifications
(defun claude-repl--notify-terminal-notifier (title message)
  "Send a desktop notification via terminal-notifier."
  (call-process "terminal-notifier" nil 0 nil
                "-title" title
                "-message" message))

(defun claude-repl--notify-osascript (title message)
  "Send a desktop notification via osascript."
  (start-process "claude-notify" nil
                 "osascript" "-e"
                 (format "display notification %S with title %S sound name \"default\""
                         message title)))

(setq claude-repl--notify-fn
      (if (executable-find "terminal-notifier")
          #'claude-repl--notify-terminal-notifier
        #'claude-repl--notify-osascript))

(defun claude-repl--notify (title message)
  "Send a desktop notification with TITLE and MESSAGE."
  (funcall claude-repl--notify-fn title message))

;; Workspace tab indicator for Claude status
(setq claude-repl--done-workspaces (or (bound-and-true-p claude-repl--done-workspaces)
                                       (make-hash-table :test 'equal)))

(defun claude-repl--workspace-for-buffer (buf)
  "Return the workspace name that contains BUF, or nil."
  (when (bound-and-true-p persp-mode)
    (cl-loop for persp in (persp-persps)
             when (persp-contain-buffer-p buf persp)
             return (safe-persp-name persp))))

(defun claude-repl--tabline-advice (&optional names)
  "Override for `+workspace--tabline' to show ✅ for workspaces where Claude is done."
  (let* ((names (or names (+workspace-list-names)))
         (current-name (+workspace-current-name)))
    (mapconcat
     #'identity
     (cl-loop for name in names
              for i to (length names)
              collect
              (let ((indicator (if (gethash name claude-repl--done-workspaces)
                                   "✅"
                                 (number-to-string (1+ i)))))
                (propertize (format " [%s] %s " indicator name)
                            'face (if (equal current-name name)
                                      '+workspace-tab-selected-face
                                    '+workspace-tab-face))))
     " ")))

(advice-add '+workspace--tabline :override #'claude-repl--tabline-advice)

;; Title-based "Claude is done" detection.
;; Claude Code sets the terminal title to "<spinner> Claude Code" while thinking
;; and plain "Claude Code" when idle.  We poll via vterm--set-title advice.
(setq claude-repl--title-thinking nil)

(defun claude-repl--title-has-spinner-p (title)
  "Return non-nil if TITLE contains a spinner (i.e. not the idle ✳ icon)."
  (and (> (length title) 0)
       (not (string-prefix-p "✳" title))
       (string-match-p "^[^[:ascii:]]" title)))

(defun claude-repl--on-title-change (title)
  "Detect thinking->idle transition from vterm title changes.
On first title change, reveal panels (Claude is ready)."
  (when (string-match-p "^\\*claude-[0-9a-f]" (buffer-name))
    ;; First title = Claude is ready. Swap placeholder for real vterm.
    (unless claude-repl--ready
      (setq claude-repl--ready t)
      (let ((buf (current-buffer))
            (placeholder (get-buffer " *claude-loading*")))
        (when placeholder
          (run-at-time 0 nil
                       (lambda ()
                         (when (buffer-live-p buf)
                           (dolist (win (get-buffer-window-list placeholder nil t))
                             (set-window-dedicated-p win nil)
                             (set-window-buffer win buf)
                             (set-window-dedicated-p win t))
                           (kill-buffer placeholder)))))))
    (let ((thinking (claude-repl--title-has-spinner-p title)))
      (when (and claude-repl--title-thinking (not thinking))
        ;; Claude just finished — refresh display
        (claude-repl--refresh-vterm)
        (claude-repl--update-hide-overlay)
        (when-let ((ws (claude-repl--workspace-for-buffer (current-buffer))))
          (puthash ws t claude-repl--done-workspaces))
        (unless (frame-focus-state)
          (let ((notify-ws (or ws "Claude")))
            (run-at-time 0.1 nil #'claude-repl--notify "Claude REPL"
                         (format "%s: Claude ready" notify-ws)))))
      (setq claude-repl--title-thinking thinking))))

(after! vterm
  (advice-add 'vterm--set-title :before #'claude-repl--on-title-change))

(defun claude-repl--do-refresh ()
  "Low-level refresh of the current vterm buffer.
Must be called with a vterm-mode buffer current."
  (redisplay t))

(defun claude-repl--refresh-vterm ()
  "Refresh the claude vterm display and clear the done indicator.
Works from any buffer (loads session) or from within the vterm buffer itself."
  (let ((buf (if (eq major-mode 'vterm-mode)
                 (current-buffer)
               (claude-repl--load-session)
               claude-repl-vterm-buffer)))
    (when (and buf (buffer-live-p buf))
      (let ((vterm-win (get-buffer-window buf))
            (orig-win (selected-window)))
        (with-current-buffer buf
          (when (eq major-mode 'vterm-mode)
            (claude-repl--do-refresh)))
        ;; Briefly select the vterm window to fix scroll position
        (when (and vterm-win (not (eq vterm-win orig-win)))
          (select-window vterm-win 'norecord)
          (select-window orig-win 'norecord)))
      (when-let ((ws (claude-repl--workspace-for-buffer buf)))
        (remhash ws claude-repl--done-workspaces)))))

;; Refresh vterm on frame focus
(defun claude-repl--on-frame-focus ()
  "Refresh claude vterm when Emacs regains focus."
  (when (frame-focus-state)
    (claude-repl--refresh-vterm)))

(add-function :after after-focus-change-function #'claude-repl--on-frame-focus)

;; Refresh vterm on workspace switch
(when (modulep! :ui workspaces)
  (add-hook 'persp-activated-functions
            (lambda (&rest _) (claude-repl--refresh-vterm))))

(defun claude-repl--after-vterm-redraw (&rest _)
  "Apply hide overlay after vterm redraws."
  (when (string-match-p "^\\*claude-[0-9a-f]" (buffer-name))
    (claude-repl--load-session)
    (claude-repl--update-hide-overlay)))

(defun claude-repl--enable-hide-overlay ()
  "Enable the hide overlay advice."
  (advice-add 'vterm--redraw :after #'claude-repl--after-vterm-redraw))

(defun claude-repl--disable-hide-overlay ()
  "Disable the hide overlay advice and clean up."
  (advice-remove 'vterm--redraw #'claude-repl--after-vterm-redraw)
  (when (and claude-repl-hide-overlay
             (overlay-buffer claude-repl-hide-overlay))
    (delete-overlay claude-repl-hide-overlay))
  (setq claude-repl-hide-overlay nil))

(defun claude-repl--vterm-running-p ()
  "Return t if Claude vterm buffer exists with a live process."
  (and claude-repl-vterm-buffer
       (buffer-live-p claude-repl-vterm-buffer)
       (get-buffer-process claude-repl-vterm-buffer)))

(defun claude-repl--input-visible-p ()
  "Return t if input buffer is visible in a window."
  (and claude-repl-input-buffer
       (buffer-live-p claude-repl-input-buffer)
       (get-buffer-window claude-repl-input-buffer)))

(defun claude-repl--vterm-visible-p ()
  "Return t if vterm buffer is visible in a window."
  (and claude-repl-vterm-buffer
       (buffer-live-p claude-repl-vterm-buffer)
       (get-buffer-window claude-repl-vterm-buffer)))

(defun claude-repl--panels-visible-p ()
  "Return t if both panels are visible."
  (and (claude-repl--input-visible-p)
       (claude-repl--vterm-visible-p)))

(defun claude-repl--hide-panels ()
  "Hide both Claude panels without killing buffers."
  (when-let ((win (get-buffer-window claude-repl-input-buffer)))
    (delete-window win))
  (when-let ((win (get-buffer-window claude-repl-vterm-buffer)))
    (delete-window win)))

;; Auto-close orphaned panels: if one is closed, close the other.
;; Also refresh the hide overlay in case a window change invalidated it.
(defun claude-repl--sync-panels ()
  "Close any Claude panel whose partner is no longer visible.
Iterates over all windows so it works across sessions.
Also refreshes the hide-input-box overlay."
  (claude-repl--load-session)
  (claude-repl--update-hide-overlay)
  (dolist (win (window-list))
    (let ((name (buffer-name (window-buffer win))))
      (cond
       ;; Vterm visible, input missing -> close vterm
       ((and (string-match "^\\*claude-\\([0-9a-f]+\\)\\*$" name)
             (not (get-buffer-window
                   (format "*claude-input-%s*" (match-string 1 name)))))
        (delete-window win))
       ;; Input visible, vterm missing -> close input
       ;; (but not if placeholder is showing — Claude is still starting)
       ((and (string-match "^\\*claude-input-\\([0-9a-f]+\\)\\*$" name)
             (not (get-buffer-window
                   (format "*claude-%s*" (match-string 1 name))))
             (not (get-buffer " *claude-loading*")))
        (delete-window win))))))

(setq claude-repl--sync-timer nil)

(defun claude-repl--schedule-sync (&rest _)
  "Defer panel sync to after the current command finishes."
  (when claude-repl--sync-timer
    (cancel-timer claude-repl--sync-timer))
  (setq claude-repl--sync-timer
        (run-at-time 0 nil #'claude-repl--sync-panels)))

(add-hook 'window-configuration-change-hook #'claude-repl--schedule-sync)

;; Redirect focus from vterm output to input buffer (keyboard only, not mouse)
(defun claude-repl--redirect-to-input (_frame)
  "If the selected window shows a Claude vterm buffer, jump to its input window.
Only redirects for keyboard navigation; mouse clicks are allowed through
so the user can select and copy text from the output."
  (unless (mouse-event-p last-input-event)
    (let ((name (buffer-name (window-buffer (selected-window)))))
      (when (string-match "^\\*claude-\\([0-9a-f]+\\)\\*$" name)
        (let* ((id (match-string 1 name))
               (input-buf (get-buffer (format "*claude-input-%s*" id))))
          (when-let ((input-win (and input-buf (get-buffer-window input-buf))))
            (select-window input-win)
            (when (bound-and-true-p evil-mode)
              (evil-insert-state))))))))

(add-hook 'window-selection-change-functions #'claude-repl--redirect-to-input)

(defun claude-repl--ensure-input-buffer ()
  "Create input buffer if needed, put in claude-input-mode."
  (let ((root (or (claude-repl--git-root)
                  claude-repl--project-root
                  default-directory)))
    (setq claude-repl-input-buffer (get-buffer-create (claude-repl--buffer-name "-input")))
    (with-current-buffer claude-repl-input-buffer
      (setq-local claude-repl--project-root root)
      (unless (eq major-mode 'claude-input-mode)
        (claude-input-mode)
        (claude-repl--history-restore)))))

(defun claude-repl--ensure-vterm-buffer ()
  "Create vterm buffer running claude if needed.
Kills any stale buffer (no live process) first. Starts claude from the git root."
  (let* ((root (or (claude-repl--git-root)
                   claude-repl--project-root
                   default-directory))
         (default-directory root)
         (existing (get-buffer (claude-repl--buffer-name))))
    ;; Kill stale buffer (exists but process died)
    (when (and existing (not (get-buffer-process existing)))
      (kill-buffer existing))
    (setq claude-repl-vterm-buffer (get-buffer-create (claude-repl--buffer-name)))
    (with-current-buffer claude-repl-vterm-buffer
      (setq-local claude-repl--project-root root)
      (unless (eq major-mode 'vterm-mode)
        (vterm-mode)
        (setq-local truncate-lines nil)
        (setq-local word-wrap t)
        (face-remap-add-relative 'default :background (claude-repl--grey 15))
        (face-remap-add-relative 'fringe :background (claude-repl--grey 15))
        (vterm-send-string "clear && claude -c")
        (vterm-send-return)))))

;; Entry point - smart toggle
(defun claude-repl ()
  "Toggle Claude REPL panels.
If text is selected: send it directly to Claude.
If not running: start Claude and show both panels.
If panels visible: hide both panels.
If panels hidden: show both panels."
  (interactive)
  (claude-repl--load-session)
  ;; Save current window to return to after hiding
  (unless (eq (current-buffer) claude-repl-input-buffer)
    (setq claude-repl-return-window (selected-window)))
  (let ((vterm-running (claude-repl--vterm-running-p))
        (panels-visible (claude-repl--panels-visible-p))
        (selection (when (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end)))))
    (cond
     ;; Text selected - send directly to Claude
     (selection
      (deactivate-mark)
      (claude-repl--send-to-claude selection)
      (claude-repl--save-session))
     ;; Nothing running - start fresh with placeholder until Claude is ready
     ((not vterm-running)
      (setq claude-repl--saved-window-config (current-window-configuration))
      (claude-repl--ensure-vterm-buffer)
      (claude-repl--ensure-input-buffer)
      (claude-repl--enable-hide-overlay)
      ;; Show panels with a blank placeholder in the output slot
      (let ((real-vterm claude-repl-vterm-buffer)
            (placeholder (get-buffer-create " *claude-loading*")))
        (with-current-buffer placeholder
          (setq-local mode-line-format nil)
          (face-remap-add-relative 'default :background (claude-repl--grey 15))
          (face-remap-add-relative 'fringe :background (claude-repl--grey 15)))
        (setq claude-repl-vterm-buffer placeholder)
        (claude-repl--show-panels)
        (setq claude-repl-vterm-buffer real-vterm))
      (claude-repl--save-session)
      (message "Starting Claude..."))
     ;; Panels visible - hide both, restore window layout
     (panels-visible
      (if claude-repl--saved-window-config
          (progn
            (set-window-configuration claude-repl--saved-window-config)
            (setq claude-repl--saved-window-config nil))
        (claude-repl--hide-panels)
        (when (and claude-repl-return-window (window-live-p claude-repl-return-window))
          (select-window claude-repl-return-window)))
      (claude-repl--save-session))
     ;; Panels hidden - show both
     (t
      (claude-repl--refresh-vterm)
      (setq claude-repl--saved-window-config (current-window-configuration))
      (claude-repl--ensure-input-buffer)
      (claude-repl--show-panels)
      (claude-repl--update-hide-overlay)
      (claude-repl--save-session)))))

(defun claude-repl-kill ()
  "Kill ALL Claude REPL buffers and windows unconditionally.
Sweeps every buffer and window by name so nothing survives."
  (interactive)
  (claude-repl--load-session)
  (claude-repl--history-save)
  (claude-repl--disable-hide-overlay)
  ;; Close all claude windows first (across all frames)
  (dolist (win (window-list-1 nil nil t))
    (when (string-match-p "^\\*claude-\\(input-\\)?[0-9a-f]+\\*$"
                          (buffer-name (window-buffer win)))
      (delete-window win)))
  ;; Kill all claude buffers — no prompts
  (dolist (buf (buffer-list))
    (when (string-match-p "^\\*claude-\\(input-\\)?[0-9a-f]+\\*$"
                          (buffer-name buf))
      (when-let ((proc (get-buffer-process buf)))
        (set-process-query-on-exit-flag proc nil))
      (kill-buffer buf)))
  ;; Kill loading placeholder if present
  (when-let ((placeholder (get-buffer " *claude-loading*")))
    (kill-buffer placeholder))
  (setq claude-repl-vterm-buffer nil
        claude-repl-input-buffer nil
        claude-repl--saved-window-config nil)
  (claude-repl--save-session))

(defun claude-repl-restart ()
  "Kill Claude REPL and restart with `claude -c` to continue session."
  (interactive)
  (claude-repl--load-session)
  (let* ((root (or (claude-repl--git-root)
                   claude-repl--project-root
                   default-directory))
         (default-directory root))
    (claude-repl-kill)
    ;; Start fresh with -c flag
    (setq claude-repl-vterm-buffer (get-buffer-create (claude-repl--buffer-name)))
    (with-current-buffer claude-repl-vterm-buffer
      (setq-local claude-repl--project-root root)
      (vterm-mode)
      (vterm-send-string "clear && claude -c")
      (vterm-send-return))
    (claude-repl--ensure-input-buffer)
    (claude-repl--enable-hide-overlay)
    (claude-repl--show-panels)
    (claude-repl--save-session)))

(defun claude-repl-focus-input ()
  "Focus the Claude input buffer, or return to previous window if already there.
If Claude isn't running, start it (same as `claude-repl')."
  (interactive)
  (claude-repl--load-session)
  (cond
   ;; Already in the input buffer — jump back
   ((eq (current-buffer) claude-repl-input-buffer)
    (if (and claude-repl-return-window (window-live-p claude-repl-return-window))
        (select-window claude-repl-return-window)
      (evil-window-left 1)))
   ;; Not running — start fresh
   ((not (claude-repl--vterm-running-p))
    (claude-repl))
   ;; Running but panels hidden — show them
   (t
    (unless (claude-repl--panels-visible-p)
      (setq claude-repl--saved-window-config (current-window-configuration))
      (claude-repl--ensure-input-buffer)
      (claude-repl--show-panels)
      (claude-repl--save-session))
    (setq claude-repl-return-window (selected-window))
    (when-let ((win (get-buffer-window claude-repl-input-buffer)))
      (select-window win)
      (when (bound-and-true-p evil-mode)
        (evil-insert-state))))))

(defun claude-repl-cycle ()
  "Send backtab to Claude vterm to cycle through options."
  (interactive)
  (claude-repl--load-session)
  (when (and claude-repl-vterm-buffer (buffer-live-p claude-repl-vterm-buffer))
    (with-current-buffer claude-repl-vterm-buffer
      (vterm-send-key "<backtab>"))))

;; Global C-S-m binding
(map! :nvi "C-S-m" #'claude-repl-cycle)

;; Keybindings
(map! :leader
      :desc "Claude REPL" "o c" #'claude-repl
      :desc "Claude input" "o v" #'claude-repl-focus-input
      :desc "Kill Claude" "o C" #'claude-repl-kill
      :desc "Claude explain" "o e" #'claude-repl-explain
      :desc "Claude explain line" "o E" #'claude-repl-explain-line
      :desc "Claude interrupt" "o i" #'claude-repl-interrupt
      :desc "Send 1 to Claude" "o 1" (lambda () (interactive) (claude-repl-send-char "1"))
      :desc "Send 2 to Claude" "o 2" (lambda () (interactive) (claude-repl-send-char "2"))
      :desc "Send 3 to Claude" "o 3" (lambda () (interactive) (claude-repl-send-char "3"))
      :desc "Send 4 to Claude" "o 4" (lambda () (interactive) (claude-repl-send-char "4"))
      :desc "Send 5 to Claude" "o 5" (lambda () (interactive) (claude-repl-send-char "5"))
      :desc "Send 6 to Claude" "o 6" (lambda () (interactive) (claude-repl-send-char "6"))
      :desc "Send 7 to Claude" "o 7" (lambda () (interactive) (claude-repl-send-char "7"))
      :desc "Send 8 to Claude" "o 8" (lambda () (interactive) (claude-repl-send-char "8"))
      :desc "Send 9 to Claude" "o 9" (lambda () (interactive) (claude-repl-send-char "9"))
      :desc "Send 0 to Claude" "o 0" (lambda () (interactive) (claude-repl-send-char "0")))


;; FIXME: vterm-clear &&  vterm-reset-cursor-point
