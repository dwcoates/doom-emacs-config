;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;; Load theme
(load-theme 'doom-tomorrow-night t)

;;; TODO: is this necessary?
(add-to-list 'exec-path "$HOME/bin")

;; Display the start-up time after loading.
(add-hook 'window-setup-hook #'doom-display-benchmark-h)

;; Use Jetbrains Mono: https://www.jetbrains.com/lp/mono/
(set-frame-font "Jetbrains Mono" nil t)
(setq-default line-spacing 1)

(defconst doom-frame-transparency 90)
(set-frame-parameter (selected-frame) 'alpha doom-frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,doom-frame-transparency))
(defun dwc-smart-transparent-frame ()
  (set-frame-parameter
    (selected-frame)
    'alpha (if (frame-parameter (selected-frame) 'fullscreen)
              100
             doom-frame-transparency)))

;; I don't much like the highlighting done on snipe matches. Confuses me with isearch-forward
;; behavior.

(add-hook 'eshell-mode-hook (lambda () (when company-mode (company-mode -1))))
(after! evil-snipe
  (set-face-attribute 'evil-snipe-first-match-face nil :background nil))

(defun doom/popup-ctrl-g-close (popup-to-close)
  (equal (kbd "C-g") (this-command-keys)))

(setq
 display-line-numbers-type nil
;;; From Henrik:
 ;;
 ;; "By default it should keep scrolling until it detects the first error. It's controlled by the
 ;;  compilation-scroll-output variable (set to 'first-error in doom, nil in vanilla. Set to t to
 ;;  always scroll). Also, it stops auto-scrolling if you scroll it manually, i believe."
 ;;
 ;; https://discordapp.com/channels/406534637242810369/406554085794381833/654479202560770065
 ;;
 ;;NOTE: For some reason, 'first-error isn't working for me. Set to t.
 compilation-scroll-output t)

(set-popup-rule! "\*input/output of .*\*" :height 18 :quit 'other)

;;; Custom function for looking at Stockfish-TEP JSON output
(defconst tep-json-buf-name " \*TEP JSON\*")
(defun dwc-view-tep-json-at-point ()
  (interactive)
  (let ((json-data (buffer-substring-no-properties
                    (save-excursion
                      (beginning-of-line)
                      (or (search-forward-regexp "json ")
                          (error "No TEP json found")))
                    (line-end-position))))
    (when (buffer-live-p tep-json-buf-name)
      (kill-buffer tep-json-buf-name))
    (display-buffer (get-buffer-create tep-json-buf-name))
    (with-current-buffer tep-json-buf-name
      (json-mode)
      (insert json-data)
      (json-pretty-print (point-min) (point-max)))))
(set-popup-rule! tep-json-buf-name :side 'right :quit 'other :width 60 :select t)

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
  (set-face-attribute 'default nil :height 110)
  ;; Turn off mode line, it's too intrusive.
  (global-hide-mode-line-mode +1))

;; Show directory contents in dired
(use-package! dired-subtree
  :after 'dired
  :config
  (map! :map dired-mode-map
        :n "C-i" 'dired-subtree-cycle))

(when (string= system-name "goldbox")
  (global-hide-mode-line-mode +1))

;;; Roland's package.
(use-package! fixmee)
