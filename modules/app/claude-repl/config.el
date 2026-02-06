;;; app/claude-repl/config.el -*- lexical-binding: t; -*-

;; State
(defvar claude-repl-vterm-buffer nil
  "The vterm buffer running Claude.")

(defvar claude-repl-input-buffer nil
  "The input buffer for composing messages.")

(defvar claude-repl-return-window nil
  "The window to return to after sending input.")

(defvar-local claude-repl-hide-overlay nil
  "Overlay used to hide Claude CLI input box.")

(defvar claude-repl--notify-timer nil
  "Debounce timer for detecting when Claude finishes working.")

(defvar claude-repl--notify-when-done nil
  "When non-nil, send a notification when Claude stops producing output.")

;; Popup rules
(set-popup-rule! "^\\*claude\\*$" :size 0.5 :side 'right :select nil :quit nil :ttl nil :no-other-window t)
(set-popup-rule! "^\\*claude-input\\*$" :size 0.3 :side 'bottom :select t :quit nil :ttl nil)

;; Input mode keymap
(defvar claude-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<return>") #'claude-repl-send)
    (define-key map (kbd "S-<return>") #'newline)
    (define-key map (kbd "C-S-1") (lambda () (interactive) (claude-repl-send-char "1")))
    (define-key map (kbd "C-S-2") (lambda () (interactive) (claude-repl-send-char "2")))
    (define-key map (kbd "C-S-3") (lambda () (interactive) (claude-repl-send-char "3")))
    (define-key map (kbd "C-S-4") (lambda () (interactive) (claude-repl-send-char "4")))
    (define-key map (kbd "C-S-5") (lambda () (interactive) (claude-repl-send-char "5")))
    (define-key map (kbd "C-S-6") (lambda () (interactive) (claude-repl-send-char "6")))
    (define-key map (kbd "C-S-7") (lambda () (interactive) (claude-repl-send-char "7")))
    (define-key map (kbd "C-S-8") (lambda () (interactive) (claude-repl-send-char "8")))
    (define-key map (kbd "C-S-9") (lambda () (interactive) (claude-repl-send-char "9")))
    (define-key map (kbd "C-S-0") (lambda () (interactive) (claude-repl-send-char "0")))
    (define-key map (kbd "C-c y") (lambda () (interactive) (claude-repl-send-char "y")))
    (define-key map (kbd "C-c n") (lambda () (interactive) (claude-repl-send-char "n")))
    (define-key map (kbd "C-c C-c") #'claude-repl-interrupt)
    (define-key map (kbd "C-c r") #'claude-repl-restart)
    (define-key map (kbd "C-c q") #'claude-repl-kill)
    map)
  "Keymap for claude-input-mode.")

(define-derived-mode claude-input-mode fundamental-mode "Claude Input"
  "Major mode for Claude REPL input buffer."
  (setq-local header-line-format "Claude Input | RET: send | C-RET: send+hide | S-RET: newline | C-c: y/n/C-c/r/q"))

;; Evil insert state bindings (override evil's RET)
(evil-define-key 'insert claude-input-mode-map
  (kbd "<return>") #'claude-repl-send
  (kbd "S-<return>") #'newline
  (kbd "C-<return>") #'claude-repl-send-and-hide)

;; Core functions
(defun claude-repl-send ()
  "Send input buffer contents to Claude, clear buffer, and return to previous window."
  (interactive)
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
  (claude-repl-send)
  (setq claude-repl--notify-when-done t)
  (claude-repl--hide-panels)
  (when (and claude-repl-return-window (window-live-p claude-repl-return-window))
    (select-window claude-repl-return-window)))

(defun claude-repl-send-char (char)
  "Send a single character to Claude."
  (when (and claude-repl-vterm-buffer (buffer-live-p claude-repl-vterm-buffer))
    (with-current-buffer claude-repl-vterm-buffer
      (vterm-send-string char)
      (vterm-send-return))))

(defun claude-repl-interrupt ()
  "Send interrupt (C-c) to Claude vterm."
  (when (and claude-repl-vterm-buffer (buffer-live-p claude-repl-vterm-buffer))
    (with-current-buffer claude-repl-vterm-buffer
      (vterm-send "C-c"))))

;; Hide overlay functions
(defun claude-repl--update-hide-overlay ()
  "Update overlay to hide bottom lines of Claude vterm buffer."
  (when (and claude-repl-vterm-buffer
             (buffer-live-p claude-repl-vterm-buffer))
    (with-current-buffer claude-repl-vterm-buffer
      ;; Only proceed if buffer has content
      (when (> (point-max) 1)
        ;; Remove old overlay if it exists
        (when (and claude-repl-hide-overlay
                   (overlay-buffer claude-repl-hide-overlay))
          (delete-overlay claude-repl-hide-overlay))
        ;; Create new overlay covering bottom 4 lines
        (let* ((end (point-max))
               (start (save-excursion
                        (goto-char end)
                        (forward-line -4)
                        (line-beginning-position))))
          (when (< start end)
            (setq claude-repl-hide-overlay (make-overlay start end nil t nil))
            ;; Use display property to replace with empty string
            (overlay-put claude-repl-hide-overlay 'display "")
            (overlay-put claude-repl-hide-overlay 'evaporate t)))))))

(defun claude-repl--notify-claude-done ()
  "Send a desktop notification that Claude has finished."
  (setq claude-repl--notify-when-done nil)
  (start-process "claude-notify" nil
                 "osascript" "-e"
                 "display notification \"Claude has finished working\" with title \"Claude REPL\""))

(defun claude-repl--after-vterm-redraw (&rest _)
  "Apply hide overlay after vterm redraws.
When panels are hidden, debounce a notification for when output stops."
  (when (and claude-repl-vterm-buffer
             (eq (current-buffer) claude-repl-vterm-buffer))
    (claude-repl--update-hide-overlay)
    ;; Debounce notification: reset timer on each redraw
    (when claude-repl--notify-when-done
      (when claude-repl--notify-timer
        (cancel-timer claude-repl--notify-timer))
      (setq claude-repl--notify-timer
            (run-at-time 3 nil #'claude-repl--notify-claude-done)))))

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
  (setq claude-repl-input-buffer (get-buffer-create "*claude-input*"))
  (with-current-buffer claude-repl-input-buffer
    (unless (eq major-mode 'claude-input-mode)
      (claude-input-mode))))

(defun claude-repl--ensure-vterm-buffer ()
  "Create vterm buffer running claude if needed.
Starts claude from the projectile project root."
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (setq claude-repl-vterm-buffer (get-buffer-create "*claude*"))
    (with-current-buffer claude-repl-vterm-buffer
      (unless (eq major-mode 'vterm-mode)
        (vterm-mode)
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
      (unless vterm-running
        (claude-repl--ensure-vterm-buffer)
        (claude-repl--ensure-input-buffer)
        (claude-repl--enable-hide-overlay))
      (setq claude-repl--notify-when-done t)
      (with-current-buffer claude-repl-vterm-buffer
        (vterm-send-string selection)
        (vterm-send-return)))
     ;; Nothing running - start fresh
     ((not vterm-running)
      (claude-repl--ensure-vterm-buffer)
      (claude-repl--ensure-input-buffer)
      (claude-repl--enable-hide-overlay)
      (display-buffer claude-repl-vterm-buffer)
      (display-buffer claude-repl-input-buffer)
      (select-window (get-buffer-window claude-repl-input-buffer))
      (evil-insert-state))
     ;; Panels visible - hide both, enable notifications
     (panels-visible
      (setq claude-repl--notify-when-done t)
      (claude-repl--hide-panels)
      (when (and claude-repl-return-window (window-live-p claude-repl-return-window))
        (select-window claude-repl-return-window)))
     ;; Panels hidden - show both, cancel notifications
     (t
      (setq claude-repl--notify-when-done nil)
      (when claude-repl--notify-timer
        (cancel-timer claude-repl--notify-timer)
        (setq claude-repl--notify-timer nil))
      (claude-repl--ensure-input-buffer)
      (display-buffer claude-repl-vterm-buffer)
      (display-buffer claude-repl-input-buffer)
      (claude-repl--update-hide-overlay)
      (select-window (get-buffer-window claude-repl-input-buffer))
      (evil-insert-state)))))

(defun claude-repl-kill ()
  "Kill Claude REPL buffers and windows without confirmation."
  (interactive)
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
        claude-repl-input-buffer nil))

(defun claude-repl-restart ()
  "Kill Claude REPL and restart with `claude -c` to continue session."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (claude-repl-kill)
    ;; Start fresh with -c flag
    (setq claude-repl-vterm-buffer (get-buffer-create "*claude*"))
    (with-current-buffer claude-repl-vterm-buffer
      (vterm-mode)
      (vterm-send-string "clear && claude -c")
      (vterm-send-return))
    (claude-repl--ensure-input-buffer)
    (claude-repl--enable-hide-overlay)
    (display-buffer claude-repl-vterm-buffer)
    (display-buffer claude-repl-input-buffer)
    (select-window (get-buffer-window claude-repl-input-buffer))))

;; Keybindings
(map! :leader
      :desc "Claude REPL" "o c" #'claude-repl
      :desc "Kill Claude" "o C" #'claude-repl-kill
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

;; FIXME: opening repl should start in insert mode
;; FIXME: RET should send to claude code, S-RET should go to new line
;; FIXME: C-c C-k can close clode (input and output boxes) when in insert mode
;; FIXME: sending to claude should not close the input window, only move the cursor back to the window from which claude was just opened
;; FIXME: C-RET should send the input to claude and close BOTH windows
;; FIXME: closing clode UI (in any way that doesn't kill cluade) should result in desktop notifications when claude is finished working. Maybe vterm has some way to know when claude is done thinking
