;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(toggle-frame-maximized)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(let ((full-name (getenv "EMACS_FULL_NAME"))
      (email (getenv "EMACS_EMAIL")))
  (if (and full-name email)
      (setq user-full-name full-name
            user-mail-address email)
    (warn "Set ${EMACS_FULL_NAME} and ${EMACS_EMAIL} environment variables so they can be used by features which support them."))
  )

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
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

(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each
;; theme may have their own settings.
(load-theme 'doom-tomorrow-night t)

;; Enable custom neotree theme
(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(set-popup-rule! "\\*helpful.*:.*\\*" :width 95 :quit nil :side 'right)

(setq
 fill-column 100)


;; TODO: move to compilation config module?
(after! compile
  (setq
   compilation-auto-jump-to-first-error t
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

  (add-hook! 'compilation-mode-hook
    (setq-local truncate-lines nil
                word-wrap t
                line-move-visual t))
  (defadvice! center-after-jump-a (&rest _)
    "Center the screen after jumping to an error."
    :after '(compilation-next-error compilation-previous-error next-error previous-error)
    (recenter nil))

  (defun +dwc/toggle-compilation-auto-jump ()
    (interactive)
    (setq compilation-auto-jump-to-first-error (not compilation-auto-jump-to-first-error))
    (message "Set `compilation-auto-jump-to-first-error' to %s" compilation-auto-jump-to-first-error))

  (map! :leader
        :desc "Toggle compilation auto-jump on error" "c y" #'+dwc/toggle-compilation-auto-jump)

  (defun +dwc/counsel-compile (initial-input &optional dir)
    "Call `compile' completing with smart suggestions, optionally for DIR. From `counsel-compile'."
    (interactive)
    (setq counsel-compile--current-build-dir (or dir
                                                 (counsel--compile-root)
                                                 default-directory))
    (ivy-read "Compile command: "
              (delete-dups (counsel--get-compile-candidates dir))
              :action #'counsel-compile--action
              :keymap counsel-compile-map
              :caller 'counsel-compile
              :initial-input initial-input))

  (after! ivy
    (defun +dwc/ivy-project-compile (initial-input)
      "Execute a compile command from the current project's root."
      (interactive)
      (+dwc/counsel-compile initial-input (projectile-project-root)))

    (defun +dwc/quick-build-unit-test (&optional beg end)
      (interactive "r")
      (let ((marked-text (if beg (concat " -r " (string-trim (buffer-substring-no-properties beg end))) ""))
            (quickbuild-str "./quick_build.sh -u"))
        (deactivate-mark)
        (+dwc/ivy-project-compile (concat quickbuild-str marked-text))))

    (map! :leader
          :desc "quick_build.sh unit-test" "c u" #'+dwc/quick-build-unit-test)
    )
  )

(after! company
  (setq company-idle-delay 0.1
        company-tooltip-idle-delay 0.1))

(defun +dwc/close-doom-popup ()
  "Close the currently open Doom popup window."
  (interactive)
  (let ((popup-windows (+popup-windows)))
    (if (> (length popup-windows) 0)
        (mapc (lambda (window) (delete-window window)) popup-windows)
      (user-error "No popup windows open"))))
(map! :leader
      :desc "Close Doom popup" "w P" #'+dwc/close-doom-popup)

;; Some UI doodads
(display-time)
(set-face-attribute 'default nil :height 125)
(setq kill-ring-max 100000)
(display-battery-mode t)

(use-package! dired-subtree
  :after 'dired
  :config
  (map! :map dired-mode-map
        :n "C-i" 'dired-subtree-cycle))

(set-popup-rule! "^\\*compilation\\*" :side 'bottom :size 0.3 :select t :quit nil)

(add-to-list 'load-path (concat (getenv "HOME") "/" "workspace"))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(after! lsp-mode
  (setq lsp-log-io t))

(setq confirm-kill-emacs nil)

(after! evil
  (setq evil-ex-search-persistent-highlight nil))

(after! vterm
  (set-popup-rule! "\\.*doom:vterm\\.*" :size 0.3 :side 'bottom :select t :quit nil :ttl nil)
  (defun +dwc/vterm-toggle (ARG)
    (interactive "P")
    (if (and (s-contains-p  "vterm" (buffer-name)) (= (length (window-list)) 1))
        (+dwc/toggle-window-fullscreen))
    (+vterm/toggle ARG))
  (map! :leader :desc "Toggle vterm popup" "o t" #'+dwc/vterm-toggle)
  )

(after! evil
  ;; Highlight the current search match more brightly
  (set-face-attribute 'evil-ex-search nil
                      :foreground "black" :background "gold" :weight 'bold)

  ;; Optionally, make other matches less prominent
  (set-face-attribute 'evil-ex-lazy-highlight nil
                      :foreground "gray" :background "#a9a9a9")

  )

(custom-set-faces!
  '(hl-line :background "#2d2d2d"))

(setq evil-disable-insert-state-bindings t)

(defun save-without-formatting ()
  "Save current buffer without formatting"
  (interactive)
  (let ((old-value +format-on-save-disabled-modes))
    (unwind-protect
        (progn
          (setq +format-on-save-disabled-modes (cons major-mode +format-on-save-disabled-modes))
          (save-buffer)
          (message "Saving without formatting: %s" +format-on-save-disabled-modes))
      (setq +format-on-save-disabled-modes old-value)
      (message "Could not unwind: %s" +format-on-save-disabled-modes))))


(map! :map override-global-map
      ;; Better window navigation bindings
      :nv "C-h" #'evil-window-left
      :nv "C-j" #'evil-window-down
      :nv "C-k" #'evil-window-up
      :nv "C-l" #'evil-window-right
      ;; Use Emacs bindings in insert mode
      :i "C-a"  #'beginning-of-line
      :i "C-e"  #'end-of-line
      :i "C-f"  #'forward-char
      :i "C-b"  #'backward-char
      :i "C-n"  #'next-line
      :i "C-p"  #'previous-line
      :i "C-d"  #'delete-char
      :i "C-k"  #'kill-line
      :i "C-y"  #'yank
      :i "M-f"  #'forward-word
      :i "M-b"  #'backward-word
      :i "C-v"  #'scroll-up-command
      :i "M-v"  #'scroll-down-command
      :i "M-<"  #'beginning-of-buffer
      :i "M->"  #'end-of-buffer
      :nv "C-{" #'+goto-previous-function.outer
      :nv "C-}" #'+goto-next-function.outer
      :leader
      ;; Toggle between source and header files
      :desc "Find other file" "T" #'ff-find-other-file
      ;; Use the Neovim-style search bindings
      :desc "Search buffer" "/" #'swiper
      :desc "Search project" "ps" #'+default/search-project
      ;; Saving files
      :desc "Save without formatting the file" "W" #'save-without-formatting
      :map general-override-local-mode-map
      :nv "C-h" #'evil-window-left
      :nv "C-j" #'evil-window-down
      :nv "C-k" #'evil-window-up
      :nv "C-l" #'evil-window-right
      )
(after! cc-mode
  (map! :map c++-mode-map
        "C-l" #'evil-window-right))

;; TODO: move this to ivy config
(after! ivy
  (setq ivy-format-function #'ivy-format-function-line)

  (defun DWC--add-last-ivy-command-to-projectile-history ()
    "For making `projectile-repeat-last-command' work by updating projectile history."
    (let ((hist (projectile--get-command-history (projectile-project-root)))
          (command (DWC--get-last-ivy-command)))
      (cond
       ((eq projectile-cmd-hist-ignoredups t)
        (unless (string= (car-safe (ring-elements hist)) command)
          (ring-insert hist command)))
       ((eq projectile-cmd-hist-ignoredups 'erase)
        (let ((idx (ring-member hist command)))
          (while idx
            (ring-remove hist idx)
            (setq idx (ring-member hist command))))
        (ring-insert hist command))
       (t (ring-insert hist command)))
      (message (format "command: '%s'" command))))

  (defun DWC--get-last-ivy-command ()
    "Get only the command part from the last Ivy selection."
    (when ivy-last
      (let ((candidate (ivy-state-current ivy-last)))
        (if (listp candidate)
            (progn (message "car %s" (car candidat)) (car candidate))
          (message "not car: ==%s==" candidate)
          candidate))))

  (advice-add '+ivy/project-compile :after #'DWC--add-command-to-projectile-history))

(after! magit
  (setq magit-no-confirm (append magit-no-confirm '(abort-revert abort-rebase abort-merge)))
  (map! :map (magit-unstaged-section-map magit-staged-section-map magit-untracked-section-map magit-mode-map)
        :desc "Jump to recent commits"
        "g r"
        #'magit-jump-to-unpushed-to-upstream))

(unless (display-graphic-p)
  ;; activate mouse-based scrolling
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
  )

(defun +dwc/toggle-window-fullscreen ()
  "Toggle the current window between its normal size and fullscreen."
  (interactive)
  (if (= 1 (length (window-list)))
      (progn
        (jump-to-register '_) ;; Restore window configuration
        (setq was-fullscreen nil))
    (window-configuration-to-register '_) ;; Save window configuration
    (delete-other-windows)))

(map! :leader
      :desc "Toggle popup fullscreen" "w f" #'+dwc/toggle-window-fullscreen)

;; (with-eval-after-load 'lsp-mode
;;   (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]vendor/[^g][^o][^o][^g][^l][^e][^/][^p][^r][^o][^t][^o][^b][^u][^f]$") ;; Only care about protobuf in Chesscom monolith PHP vendor/
;;   (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]client/node_modules$") ;; ChessCom monolith client/node_modules has couple hundred thousand files in it
;;   (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]node_modules$") ;; ChessCom monolith client/node_modules has couple hundred thousand files in it
;;   )

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
;;;
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
