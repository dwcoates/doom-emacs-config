;;; app/claude-repl/config.el -*- lexical-binding: t; -*-

;; State
(defvar claude-repl-vterm-buffer nil
  "The vterm buffer running Claude.")

(defvar claude-repl-input-buffer nil
  "The input buffer for composing messages.")

(defvar claude-repl-return-window nil
  "The window to return to after sending input.")

;; Popup rules
(set-popup-rule! "^\\*claude\\*$" :size 0.4 :side 'right :select nil :quit nil :ttl nil :no-other-window t)
(set-popup-rule! "^\\*claude-input\\*$" :size 0.3 :side 'bottom :select t :quit nil :ttl nil)

;; Input mode keymap
(defvar claude-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") #'claude-repl-send)
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
    map)
  "Keymap for claude-input-mode.")

(define-derived-mode claude-input-mode fundamental-mode "Claude Input"
  "Major mode for Claude REPL input buffer."
  (setq-local header-line-format "Claude Input | C-RET: send | C-S-0-9: select | C-c y/n: yes/no"))

;; Core functions
(defun claude-repl-send ()
  "Send input buffer contents to Claude, clear buffer, and return to original window."
  (interactive)
  (when (and claude-repl-vterm-buffer (buffer-live-p claude-repl-vterm-buffer))
    (let ((input (with-current-buffer claude-repl-input-buffer
                   (buffer-string))))
      (with-current-buffer claude-repl-vterm-buffer
        (vterm-send-string input)
        (vterm-send-return))
      (with-current-buffer claude-repl-input-buffer
        (erase-buffer))
      ;; Close input popup and return to original window
      (when-let ((win (get-buffer-window claude-repl-input-buffer)))
        (delete-window win))
      (when (and claude-repl-return-window (window-live-p claude-repl-return-window))
        (select-window claude-repl-return-window)))))

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

(defun claude-repl--vterm-running-p ()
  "Return t if Claude vterm buffer exists and is alive."
  (and claude-repl-vterm-buffer (buffer-live-p claude-repl-vterm-buffer)))

(defun claude-repl--input-visible-p ()
  "Return t if input buffer is visible in a window."
  (and claude-repl-input-buffer
       (buffer-live-p claude-repl-input-buffer)
       (get-buffer-window claude-repl-input-buffer)))

(defun claude-repl--ensure-input-buffer ()
  "Create input buffer if needed, put in claude-input-mode."
  (setq claude-repl-input-buffer (get-buffer-create "*claude-input*"))
  (with-current-buffer claude-repl-input-buffer
    (unless (eq major-mode 'claude-input-mode)
      (claude-input-mode))))

(defun claude-repl--ensure-vterm-buffer ()
  "Create vterm buffer running claude if needed."
  (setq claude-repl-vterm-buffer (get-buffer-create "*claude*"))
  (with-current-buffer claude-repl-vterm-buffer
    (unless (eq major-mode 'vterm-mode)
      (vterm-mode)
      (vterm-send-string "claude")
      (vterm-send-return))))

;; Entry point - smart toggle
(defun claude-repl ()
  "Smart toggle for Claude REPL.
If nothing open: open both Claude output and input.
If Claude open but input closed: open input and interrupt Claude.
Always focuses the input buffer, never the output."
  (interactive)
  ;; Save current window to return to after sending
  (unless (eq (current-buffer) claude-repl-input-buffer)
    (setq claude-repl-return-window (selected-window)))
  (let ((vterm-running (claude-repl--vterm-running-p))
        (input-visible (claude-repl--input-visible-p)))
    (cond
     ;; Nothing running - start fresh
     ((not vterm-running)
      (claude-repl--ensure-vterm-buffer)
      (claude-repl--ensure-input-buffer)
      (display-buffer claude-repl-vterm-buffer)
      (display-buffer claude-repl-input-buffer)
      (select-window (get-buffer-window claude-repl-input-buffer)))
     ;; Vterm running but input not visible - open input and interrupt
     ((and vterm-running (not input-visible))
      (claude-repl-interrupt)
      (claude-repl--ensure-input-buffer)
      (display-buffer claude-repl-input-buffer)
      (select-window (get-buffer-window claude-repl-input-buffer)))
     ;; Both visible - just focus input
     (t
      (select-window (get-buffer-window claude-repl-input-buffer))))))

;; Keybinding
(map! :leader :desc "Claude REPL" "o c" #'claude-repl)
