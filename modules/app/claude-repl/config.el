;;; app/claude-repl/config.el -*- lexical-binding: t; -*-

;; State
(defvar claude-repl-vterm-buffer nil
  "The vterm buffer running Claude.")

(defvar claude-repl-input-buffer nil
  "The input buffer for composing messages.")

;; Popup rules
(set-popup-rule! "^\\*claude\\*$" :size 0.4 :side 'right :select nil :quit nil :ttl nil)
(set-popup-rule! "^\\*claude-input\\*$" :size 0.15 :side 'bottom :select t :quit nil :ttl nil)

;; Input mode
(defvar claude-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") #'claude-repl-send)
    (define-key map (kbd "C-c 1") (lambda () (interactive) (claude-repl-send-char "1")))
    (define-key map (kbd "C-c 2") (lambda () (interactive) (claude-repl-send-char "2")))
    (define-key map (kbd "C-c 3") (lambda () (interactive) (claude-repl-send-char "3")))
    (define-key map (kbd "C-c 4") (lambda () (interactive) (claude-repl-send-char "4")))
    (define-key map (kbd "C-c 5") (lambda () (interactive) (claude-repl-send-char "5")))
    (define-key map (kbd "C-c 6") (lambda () (interactive) (claude-repl-send-char "6")))
    (define-key map (kbd "C-c 7") (lambda () (interactive) (claude-repl-send-char "7")))
    (define-key map (kbd "C-c 8") (lambda () (interactive) (claude-repl-send-char "8")))
    (define-key map (kbd "C-c 9") (lambda () (interactive) (claude-repl-send-char "9")))
    (define-key map (kbd "C-c 0") (lambda () (interactive) (claude-repl-send-char "0")))
    (define-key map (kbd "C-c y") (lambda () (interactive) (claude-repl-send-char "y")))
    (define-key map (kbd "C-c n") (lambda () (interactive) (claude-repl-send-char "n")))
    map)
  "Keymap for claude-input-mode.")

(define-derived-mode claude-input-mode fundamental-mode "Claude Input"
  "Major mode for Claude REPL input buffer."
  (setq-local header-line-format "Claude Input | C-RET: send | C-c 0-9: select | C-c y/n: yes/no"))

;; Core functions
(defun claude-repl-send ()
  "Send input buffer contents to Claude, clear buffer, and close input popup."
  (interactive)
  (when (and claude-repl-vterm-buffer (buffer-live-p claude-repl-vterm-buffer))
    (let ((input (with-current-buffer claude-repl-input-buffer
                   (buffer-string))))
      (with-current-buffer claude-repl-vterm-buffer
        (vterm-send-string input)
        (vterm-send-return))
      (with-current-buffer claude-repl-input-buffer
        (erase-buffer))
      ;; Close input popup, keep claude output visible
      (when-let ((win (get-buffer-window claude-repl-input-buffer)))
        (delete-window win)))))

(defun claude-repl-send-char (char)
  "Send a single character to Claude."
  (when (and claude-repl-vterm-buffer (buffer-live-p claude-repl-vterm-buffer))
    (with-current-buffer claude-repl-vterm-buffer
      (vterm-send-string char)
      (vterm-send-return))))

;; Entry point
(defun claude-repl ()
  "Start Claude REPL with separate input and display buffers."
  (interactive)
  ;; Create or switch to vterm buffer running claude
  (setq claude-repl-vterm-buffer (get-buffer-create "*claude*"))
  (with-current-buffer claude-repl-vterm-buffer
    (unless (eq major-mode 'vterm-mode)
      (vterm-mode)
      (vterm-send-string "claude")
      (vterm-send-return)))

  ;; Create input buffer
  (setq claude-repl-input-buffer (get-buffer-create "*claude-input*"))
  (with-current-buffer claude-repl-input-buffer
    (claude-input-mode))

  ;; Display both as popups - vterm first (larger), then input (on top/below)
  (display-buffer claude-repl-vterm-buffer)
  (display-buffer claude-repl-input-buffer)
  (select-window (get-buffer-window claude-repl-input-buffer)))

;; Keybinding
(map! :leader :desc "Claude REPL" "o c" #'claude-repl)
