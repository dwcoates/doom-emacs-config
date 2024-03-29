;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(toggle-frame-fullscreen)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Dodge Coates"
      user-mail-address "dodge@chess.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tomorrow-night)

(add-hook 'window-setup-hook #'doom-display-benchmark-h)

(defun doom/popup-ctrl-g-close (popup-to-close)
  (equal (kbd "C-g") (this-command-keys)))

(set-popup-rule! "\\*helpful.*:.*\\*" :width 85 :quit 'nil :side 'right)

(setq
 display-line-numbers-type t
;;; From Henrik:
 ;;
 ;; "By default it should keep scrolling until it detects the first error. It's controlled by the
 ;;  compilation-scroll-output variable (set to 'first-error in doom, nil in vanilla. Set to t to
 ;;  always scroll). Also, it stops auto-scrolling if you scroll it manually, i believe."
 ;;
 ;; https://discordapp.com/channels/406534637242810369/406554085794381833/654479202560770065
 ;;
 ;;NOTE: For some reason, 'first-error isn't working for me. Set to t.
 compilation-scroll-output t
 fill-column 100)

(display-time)

(after! helpful (if (s-contains-p  "silverbox" system-name)
    (progn
      (set-face-attribute 'default nil :height 125) ;; Smaller character sizes
      (setq kill-ring-max 10000)
      (display-battery-mode t))
  (set-face-attribute 'default nil :height 110)))

(use-package! dired-subtree
  :after 'dired
  :config
  (map! :map dired-mode-map
        :n "C-i" 'dired-subtree-cycle))

(use-package! yaml-mode)

(add-to-list 'load-path (concat (getenv "HOME") "/" "workspace"))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Do not tab complete with company (tab is for snippet expansion).
(after! company
  (define-key company-active-map [tab] nil)
  (define-key company-active-map (kbd "TAB") nil))

;;; Roland's package.
(use-package! fixmee)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
