;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;; Load theme
(load-theme 'doom-molokai t)

;;; TODO: is this necessary?
(add-to-list 'exec-path "$HOME/bin")

;; Display the start-up time after loading.
(add-hook 'window-setup-hook #'doom-display-benchmark-h)

(defun doom/popup-ctrl-g-close (popup-to-close)
  (equal (kbd "C-g") (this-command-keys)))

(setq!
 display-line-numbers-type 'relative)

(setq-default
 fill-column 100)

(map!
 :v
 "." 'er/expand-region)

;; Relative line numbers are pretty cool. Makes a lot of VIM commands easier to use.

;; When on the laptop, use some special settings.
(when (string= system-name "blackbox")
  (display-battery-mode t)
  ;; Use a smaller font.
  (set-face-attribute 'default nil :height 100)
  ;; Turn off mode line, it's too intrusive.
  (global-hide-mode-line-mode +1))

;; Show directory contents in dired
(use-package! dired-subtree
  :after 'dired
  :config
  (map! :map dired-mode-map
        :n "C-i" 'dired-subtree-cycle))

;;; Roland's package.
(use-package! fixmee)
