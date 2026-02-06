;;; app/claude-repl/config.el -*- lexical-binding: t; -*-

;; Force monospace fallback for Unicode ranges that cause alignment issues.
;; Without this, Emacs falls back to non-monospaced fonts (e.g. Apple Braille)
;; and the vterm grid drifts horizontally.
(dolist (range '((#x2500 . #x259F)    ;; Box drawing + block elements
                 (#x25A0 . #x25FF))) ;; Geometric shapes
  (set-fontset-font t range "JetBrains Mono"))

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
;; claude-repl--saved-window-config, claude-repl--notify-timer, claude-repl--notify-when-done

(defvar-local claude-repl-hide-overlay nil
  "Overlay used to hide Claude CLI input box.")

(setq claude-repl-hide-input-box nil)

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
and restores window config / notify state from the sessions hash table."
  (let* ((id (claude-repl--workspace-id))
         (vterm-name (claude-repl--buffer-name))
         (input-name (claude-repl--buffer-name "-input"))
         (session (gethash id claude-repl--sessions)))
    (setq claude-repl-vterm-buffer (get-buffer vterm-name)
          claude-repl-input-buffer (get-buffer input-name)
          claude-repl--saved-window-config (plist-get session :saved-window-config)
          claude-repl-return-window (plist-get session :return-window)
          claude-repl--notify-timer (plist-get session :notify-timer)
          claude-repl--notify-when-done (plist-get session :notify-when-done))))

(defun claude-repl--save-session ()
  "Save the current global state back to the sessions hash table."
  (let ((id (claude-repl--workspace-id)))
    (puthash id
             (list :saved-window-config claude-repl--saved-window-config
                   :return-window claude-repl-return-window
                   :notify-timer claude-repl--notify-timer
                   :notify-when-done claude-repl--notify-when-done)
             claude-repl--sessions)))

;; Popup rules (input rule first — it's more specific and Doom matches first)
(set-popup-rule! "^\\*claude-input-[0-9a-f]+" :size 0.3 :side 'bottom :select t :quit nil :ttl nil)
(set-popup-rule! "^\\*claude-[0-9a-f]+" :size 0.55 :side 'right :select nil :quit nil :ttl nil :no-other-window t)

;; Input mode
(define-derived-mode claude-input-mode fundamental-mode "Claude Input"
  "Major mode for Claude REPL input buffer."
  (setq-local header-line-format "Claude Input | RET: send | C-RET: send+hide | S-RET: newline | C-c C-c: clear | ESC ESC: interrupt | C-c: y/n/r/q"))

(map! :map claude-input-mode-map
      :ni "RET"       #'claude-repl-send
      :ni "S-RET"     #'newline
      :ni "C-RET"     #'claude-repl-send-and-hide
      :ni "C-c C-c"   (cmd! (erase-buffer) (evil-insert-state))
      :ni "C-c y"     (cmd! (claude-repl-send-char "y"))
      :ni "C-c n"     (cmd! (claude-repl-send-char "n"))
      :ni "C-c r"     #'claude-repl-restart
      :ni "C-c q"     #'claude-repl-kill
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
      :n  "C-n"       (cmd! (claude-repl--load-session)
                            (when (and claude-repl-vterm-buffer (buffer-live-p claude-repl-vterm-buffer))
                              (with-current-buffer claude-repl-vterm-buffer (vterm-send-down))))
      :n  "C-p"       (cmd! (claude-repl--load-session)
                            (when (and claude-repl-vterm-buffer (buffer-live-p claude-repl-vterm-buffer))
                              (with-current-buffer claude-repl-vterm-buffer (vterm-send-up)))))

;; Core functions
(defun claude-repl-send ()
  "Send input buffer contents to Claude, clear buffer, and return to previous window."
  (interactive)
  (claude-repl--load-session)
  (when (and claude-repl-vterm-buffer (buffer-live-p claude-repl-vterm-buffer))
    (let ((input (with-current-buffer claude-repl-input-buffer
                   (buffer-string))))
      (with-current-buffer claude-repl-vterm-buffer
        (vterm-send-string input)
        (vterm-send-return))
      (with-current-buffer claude-repl-input-buffer
        (erase-buffer))
      (when (and claude-repl-return-window (window-live-p claude-repl-return-window))
        (select-window claude-repl-return-window)))))

(defun claude-repl-send-and-hide ()
  "Send input to Claude and hide both panels."
  (interactive)
  (claude-repl--load-session)
  (claude-repl-send)
  (setq claude-repl--notify-when-done t)
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
  (setq claude-repl--notify-when-done t)
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

(defun claude-repl-interrupt ()
  "Send interrupt (C-c) to Claude vterm."
  (interactive)
  (claude-repl--load-session)
  (when (and claude-repl-vterm-buffer (buffer-live-p claude-repl-vterm-buffer))
    (with-current-buffer claude-repl-vterm-buffer
      (vterm-send "<escape>")
      (vterm-send "<escape>"))))

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

(defun claude-repl--notify-claude-done (workspace-id)
  "Send a desktop notification that Claude has finished.
WORKSPACE-ID identifies which project's session to update."
  (let ((session (gethash workspace-id claude-repl--sessions)))
    (when session
      (puthash workspace-id
               (plist-put session :notify-when-done nil)
               claude-repl--sessions)))
  (start-process "claude-notify" nil
                 "osascript" "-e"
                 "display notification \"Claude has finished working\" with title \"Claude REPL\""))

(defun claude-repl--after-vterm-redraw (&rest _)
  "Apply hide overlay after vterm redraws.
When panels are hidden, debounce a notification for when output stops."
  (when (string-match-p "^\\*claude-[0-9a-f]" (buffer-name))
    (claude-repl--load-session)
    (claude-repl--update-hide-overlay)
    ;; Debounce notification: reset timer on each redraw
    (when claude-repl--notify-when-done
      (when claude-repl--notify-timer
        (cancel-timer claude-repl--notify-timer))
      (let ((ws-id (claude-repl--workspace-id)))
        (setq claude-repl--notify-timer
              (run-at-time 3 nil (lambda () (claude-repl--notify-claude-done ws-id))))))
    (claude-repl--save-session)))

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
  "Return t if Claude vterm buffer exists and is alive."
  (and claude-repl-vterm-buffer (buffer-live-p claude-repl-vterm-buffer)))

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

(defun claude-repl--ensure-input-buffer ()
  "Create input buffer if needed, put in claude-input-mode."
  (let ((root (or (claude-repl--git-root)
                  claude-repl--project-root
                  default-directory)))
    (setq claude-repl-input-buffer (get-buffer-create (claude-repl--buffer-name "-input")))
    (with-current-buffer claude-repl-input-buffer
      (setq-local claude-repl--project-root root)
      (unless (eq major-mode 'claude-input-mode)
        (claude-input-mode)))))

(defun claude-repl--ensure-vterm-buffer ()
  "Create vterm buffer running claude if needed.
Starts claude from the git root."
  (let* ((root (or (claude-repl--git-root)
                   claude-repl--project-root
                   default-directory))
         (default-directory root))
    (setq claude-repl-vterm-buffer (get-buffer-create (claude-repl--buffer-name)))
    (with-current-buffer claude-repl-vterm-buffer
      (setq-local claude-repl--project-root root)
      (unless (eq major-mode 'vterm-mode)
        (vterm-mode)
        (setq-local truncate-lines nil)
        (setq-local word-wrap t)
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
     ;; Nothing running - start fresh
     ((not vterm-running)
      (setq claude-repl--saved-window-config (current-window-configuration))
      (claude-repl--ensure-vterm-buffer)
      (claude-repl--ensure-input-buffer)
      (claude-repl--enable-hide-overlay)
      (delete-other-windows claude-repl-return-window)
      (display-buffer claude-repl-vterm-buffer)
      (display-buffer claude-repl-input-buffer)
      (select-window (get-buffer-window claude-repl-input-buffer))
      (evil-insert-state)
      (claude-repl--save-session))
     ;; Panels visible - hide both, restore window layout
     (panels-visible
      (setq claude-repl--notify-when-done t)
      (if claude-repl--saved-window-config
          (progn
            (set-window-configuration claude-repl--saved-window-config)
            (setq claude-repl--saved-window-config nil))
        (claude-repl--hide-panels)
        (when (and claude-repl-return-window (window-live-p claude-repl-return-window))
          (select-window claude-repl-return-window)))
      (claude-repl--save-session))
     ;; Panels hidden - show both, cancel notifications
     (t
      (setq claude-repl--notify-when-done nil)
      (when claude-repl--notify-timer
        (cancel-timer claude-repl--notify-timer)
        (setq claude-repl--notify-timer nil))
      (setq claude-repl--saved-window-config (current-window-configuration))
      (claude-repl--ensure-input-buffer)
      (delete-other-windows claude-repl-return-window)
      (display-buffer claude-repl-vterm-buffer)
      (display-buffer claude-repl-input-buffer)
      (claude-repl--update-hide-overlay)
      (select-window (get-buffer-window claude-repl-input-buffer))
      (evil-insert-state)
      (claude-repl--save-session)))))

(defun claude-repl-kill ()
  "Kill Claude REPL buffers and windows without confirmation."
  (interactive)
  (claude-repl--load-session)
  (claude-repl--disable-hide-overlay)
  (when-let ((win (get-buffer-window claude-repl-input-buffer)))
    (delete-window win))
  (when-let ((win (get-buffer-window claude-repl-vterm-buffer)))
    (delete-window win))
  (when (and claude-repl-input-buffer (buffer-live-p claude-repl-input-buffer))
    (kill-buffer claude-repl-input-buffer))
  (when (and claude-repl-vterm-buffer (buffer-live-p claude-repl-vterm-buffer))
    ;; Don't prompt about killing the process
    (when-let ((proc (get-buffer-process claude-repl-vterm-buffer)))
      (set-process-query-on-exit-flag proc nil))
    (kill-buffer claude-repl-vterm-buffer))
  (setq claude-repl-vterm-buffer nil
        claude-repl-input-buffer nil
        claude-repl--saved-window-config nil
        claude-repl--notify-when-done nil)
  (when claude-repl--notify-timer
    (cancel-timer claude-repl--notify-timer)
    (setq claude-repl--notify-timer nil))
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
    (display-buffer claude-repl-vterm-buffer)
    (display-buffer claude-repl-input-buffer)
    (select-window (get-buffer-window claude-repl-input-buffer))
    (claude-repl--save-session)))

;; Keybindings
(map! :leader
      :desc "Claude REPL" "o c" #'claude-repl
      :desc "Kill Claude" "o C" #'claude-repl-kill
      :desc "Claude explain" "o e" #'claude-repl-explain
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
