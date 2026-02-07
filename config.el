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

;; Swift LSP (sourcekit-lsp from Xcode)
(after! lsp-mode
  (setq lsp-sourcekit-executable
        (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp"))))

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
(defvar +dwc/font 'menlo
  "Which monospace font to use. Options: 'jetbrains, 'menlo, 'dejavu.")

(display-time)
(when +dwc/font
 (set-face-attribute 'default nil
                     :width 'normal
                     :height 125
                     :family (pcase +dwc/font
                               ('menlo "Menlo")
                               ('dejavu "DejaVu Sans Mono")
                               (_ "JetBrains Mono")))
 )

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

(after! org
  (setq org-agenda-custom-commands '(("A" "High Priority Tasks" tags-todo "PRIORITY=\"A\""))
        org-use-tag-inheritance t
        org-agenda-custom-commands
        '(("A" "High Priority Tasks Grouped by Tag"
           ((tags-todo "PRIORITY=\"A\""
                       ((org-super-agenda-groups
                         '((:auto-group t :name "Grouped by tag"))))))))
        ))

(after! lsp-mode
  (setq lsp-log-io nil))  ; Only enable when debugging LSP issues

(setq confirm-kill-emacs nil)

(defadvice! +dwc/save-before-restart-a (&rest _)
  "Save all buffers without prompting before restarting Emacs."
  :before #'doom/restart
  (save-some-buffers nil t))

(after! evil
  (setq evil-ex-search-persistent-highlight nil))

(after! hl-todo
  (add-to-list 'hl-todo-keyword-faces '("PRREVIEW" . "#7cb8bb")))

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

  ;; Enable fuzzy matching for workspace buffer switch (SPC ,)
  ;; ivy--regex-ignore-order: space-separated terms match anywhere, any order
  (setq ivy-re-builders-alist
        '((persp-switch-to-buffer . ivy--regex-ignore-order)
          (t . ivy--regex-plus)))

  ;; Grey out directory path, keep filename normal in buffer list
  (defun +dwc/ivy-buffer-transformer (candidate)
    "Display directory path in grey, filename in default face."
    (let* ((dir (file-name-directory candidate))
           (file (file-name-nondirectory candidate)))
      (if dir
          (concat (propertize dir 'face 'font-lock-comment-face) file)
        candidate)))

  (ivy-configure 'persp-switch-to-buffer
    :display-transformer-fn #'+dwc/ivy-buffer-transformer)

  ;; Persistent highlight for ivy-call previews
  (defvar +dwc/ivy-preview-overlay nil
    "Overlay for highlighting the previewed line.")

  (defun +dwc/ivy-call-with-highlight ()
    "Call `ivy-call' and persistently highlight the current line."
    (interactive)
    (ivy-call)
    (with-ivy-window
      (when +dwc/ivy-preview-overlay
        (delete-overlay +dwc/ivy-preview-overlay))
      (setq +dwc/ivy-preview-overlay (make-overlay (line-beginning-position) (1+ (line-end-position))))
      (overlay-put +dwc/ivy-preview-overlay 'face 'highlight)))

  (defun +dwc/ivy-cleanup-preview-overlay ()
    "Remove the preview overlay when ivy exits."
    (when +dwc/ivy-preview-overlay
      (delete-overlay +dwc/ivy-preview-overlay)
      (setq +dwc/ivy-preview-overlay nil)))

  (add-hook 'minibuffer-exit-hook #'+dwc/ivy-cleanup-preview-overlay)

  (map! :map ivy-minibuffer-map
        "C-S-SPC " #'+dwc/ivy-call-with-highlight)

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
  (setq magit-no-confirm (append magit-no-confirm '(abort-revert abort-rebase abort-merge))
        magit-diff-visit-previous-blob nil)
  (map! :map (magit-unstaged-section-map magit-staged-section-map magit-untracked-section-map magit-mode-map)
        :desc "Jump to recent commits"
        "g r"
        #'magit-jump-to-unpushed-to-upstream))

;; Section map bindings must be done after magit-diff loads
(after! magit-diff
  (define-key magit-file-section-map [return] #'magit-diff-visit-worktree-file)
  (define-key magit-file-section-map [C-return] #'magit-diff-visit-file)
  (define-key magit-hunk-section-map [return] #'magit-diff-visit-worktree-file)
  (define-key magit-hunk-section-map [C-return] #'magit-diff-visit-file))

;; Workspace configuration
(after! persp-mode
  ;; Auto-restore workspaces from last session on startup
  (setq persp-auto-resume-time 0.1)

  ;; Show workspace names in tab-bar
  (defun +dwc/workspace-tabline-formatted ()
    "Format workspace list for tab-bar display."
    (+doom-dashboard--center (frame-width) (+workspace--tabline)))

  (defun +dwc/current-workspace-name ()
    "Return current workspace name (invisible, for triggering updates)."
    (propertize (safe-persp-name (get-current-persp)) 'invisible t))

  (setq tab-bar-format '(+dwc/workspace-tabline-formatted
                         tab-bar-format-align-right
                         +dwc/current-workspace-name))

  (setq tab-bar-show t
        tab-bar-new-button-show nil
        tab-bar-close-button-show nil)
  (tab-bar-mode 1)

  ;; Delete the "main" workspace after session restore.
  ;; Uses emacs-startup-hook + idle timer to ensure all workspace restoration is complete.
  (add-hook 'emacs-startup-hook
            (lambda ()
              (run-with-idle-timer 1 nil
                                   (lambda ()
                                     (if (member "main" (+workspace-list-names))
                                         (condition-case err
                                             (progn
                                               (+workspace-delete "main")
                                               (message "Deleted 'main' workspace"))
                                           (error (message "Failed to delete 'main' workspace: %s" err)))
                                       (message "'main' workspace not found, nothing to delete")))))))

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

(defun +dwc/generate-github-link (&optional open-in-browser)
  "Generate a GitHub link to the current line or selected line range in the file.
If OPEN-IN-BROWSER is non-nil, open the link in the default browser."
  (interactive "P")
  (let* ((git-root (string-trim (shell-command-to-string "git rev-parse --show-toplevel")))
         (file-path (file-relative-name (buffer-file-name) git-root))
         (start-line (line-number-at-pos (if (use-region-p) (region-beginning) (point))))
         (end-line (line-number-at-pos (if (use-region-p) (region-end) (point))))
         (line-range (if (= start-line end-line)
                         (format "#L%d" start-line)
                       (format "#L%d-L%d" start-line end-line)))
         (default-directory git-root)
         (commit-sha (string-trim (shell-command-to-string "git rev-parse HEAD")))
         (remote-url (string-trim (shell-command-to-string "git config --get remote.origin.url")))
         (cleaned-url (replace-regexp-in-string "^git@github.com:" "https://github.com"
                                                (replace-regexp-in-string "\\.git$" "" remote-url)))
         (repo-name (progn
                      (if (string-match "github.com[:/]ChessCom/\\(.*\\)" cleaned-url)
                          (match-string 1 cleaned-url)
                        (error (format "Remote URL '%s' does not match expected pattern" cleaned-url)))))
         (github-url (format "https://github.com/ChessCom/%s/blob/%s/%s%s"
                             repo-name commit-sha file-path line-range)))
    (if open-in-browser
        (browse-url github-url)
      (kill-new github-url)
      (message "GitHub link copied to clipboard: %s" github-url))))

(defun +dwc/magit-open-commit-in-github ()
  "Open the current commit in GitHub browser."
  (interactive)
  (let* ((commit-sha (string-trim (shell-command-to-string "git rev-parse HEAD")))
         (remote-url (string-trim (shell-command-to-string "git config --get remote.origin.url")))
         (cleaned-url (replace-regexp-in-string "^git@github.com:" "https://github.com"
                                                (replace-regexp-in-string "\\.git$" "" remote-url)))
         (repo-name (progn
                      (if (string-match "github.com[:/]ChessCom/\\(.*\\)" cleaned-url)
                          (match-string 1 cleaned-url)
                        (error (format "Remote URL '%s' does not match expected pattern" cleaned-url)))))
         (github-url (format "https://github.com/ChessCom/%s/commit/%s" repo-name commit-sha)))
    (browse-url github-url)))

(defun +dwc/magit-copy-commit-link ()
  "Copy GitHub link for commit at point in magit buffer."
  (interactive)
  (let* ((commit-sha (magit-commit-at-point))
         (default-directory (magit-toplevel))
         (remote-url (string-trim (shell-command-to-string "git config --get remote.origin.url")))
         (cleaned-url (replace-regexp-in-string "^git@github.com:" "https://github.com"
                                                (replace-regexp-in-string "\\.git$" "" remote-url)))
         (repo-name (progn
                      (if (string-match "github.com[:/]ChessCom/\\(.*\\)" cleaned-url)
                          (match-string 1 cleaned-url)
                        (error (format "Remote URL '%s' does not match expected pattern" cleaned-url)))))
         (github-url (format "https://github.com/ChessCom/%s/commit/%s" repo-name commit-sha)))
    (kill-new github-url)
    (message "GitHub commit link copied to clipboard: %s" github-url)))

(map! :leader
      :desc "Generate GitHub link for current line"
      "g h" #'+dwc/generate-github-link
      :desc "Generate and open GitHub link in browser"
      "g H" (lambda () (interactive) (+dwc/generate-github-link t))
      :desc "Open commit in GitHub"
      "g O" #'+dwc/magit-open-commit-in-github)

;; Add magit-specific keybinding
(map! :map magit-status-mode-map
      "g c" #'+dwc/magit-copy-commit-link
      "g C" #'+dwc/magit-open-commit-in-github)

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

(after! cc-mode
  (after! lsp
    ;; Enable LSP headerline breadcrumbs for function names
    (setq lsp-headerline-breadcrumb-enable t
          lsp-headerline-breadcrumb-segments '(symbols))
    (after! lsp-headerline
     (set-face-attribute 'lsp-headerline-breadcrumb-symbols-face nil
                         :inherit 'font-lock-function-name-face))
    )

  (after! yasnippet
    ;; Override this broken function
    ;;
    ;; TODO: fix the function instead of overriding it
    (defun doom-snippets-c++-using-std-p ()
      "Return non-nil if 'using namespace std' is found at the top of this file."
      nil)

    (defun doom-snippets-c++-class-name (str)
      "Search for a class name like `DerivedClass' in STR
(which may look like `DerivedClass : ParentClass1, ParentClass2, ...')
If found, the class name is returned, otherwise STR is returned"
      (yas-substr str "[^: ]*"))

    (defun doom-snippets-c++-class-method-decl-choice ()
      "Choose and return the end of a C++11 class method declaration"
      (yas-choose-value '(";" " = default;" " = delete;")))
    )

  (after! company
    ;; (map! :map company-active-map "C-SPC" #'yas-expand)
    ;; (map! :map company-mode-map "C-SPC" #'yas-expand)
    ;; (map! :map override-global-map "C-SPC" #'yas-expand)
    ;; (map! :map company-active-map [tab] nil)
    )
  )

;; Ensure snippets in ./snippets take precedence over the Doom internal ones
(after! yasnippet
  (setq yas-snippet-dirs
        (append yas-snippet-dirs '("~/.doom.d/snippets"))))

;; One option is the Evil plugin "evil-replace-with-register":
;; https://github.com/emacs-evil/evil-replace-with-register
;;
;; It lets you replace text within any motion/text-object using
;; what's in a register (e.g., the default yank register).
;; In Doom Emacs, install and configure it:

;; FIXME: make work
;; (use-package! evil-replace-with-register
;;   :after evil
;;   :config
;;   (evil-replace-with-register-install))

;; Usage:
;; 1. Yank/copy something into the default register (y, yi(, etc.)
;; 2. Move the cursor where you want to replace.
;; 3. Use: `gr` + a motion/text-object
;;    e.g. `gr i )`  -> replaces inside parentheses with the last yanked text
;;    e.g. `gr a )`  -> replaces including the parentheses
;;
;; This way, it won't overwrite your current yank buffer,
;; and you won't have to jump through separate "delete + p" steps.

(after! projectile
  (setq projectile-indexing-method 'alien)
  
  ;; Custom project name function with improved logic
  (defun +dwc/projectile-project-name (project-root)
    "Generate project name using .projectile at git root if available, with smart bracketing"
    (let* ((default-name (file-name-nondirectory (directory-file-name project-root)))
           (projectile-file (expand-file-name ".projectile" project-root))
           (git-root (ignore-errors 
                       (string-trim (shell-command-to-string 
                                     (format "cd %s && git rev-parse --show-toplevel" 
                                             (shell-quote-argument project-root)))))))
      (if git-root
          (let* ((git-root-dir (file-name-nondirectory (directory-file-name git-root)))
                 (projectile-dir (file-name-nondirectory (directory-file-name project-root)))
                 (git-root-projectile-file (expand-file-name ".projectile" git-root))
                 (git-root-name (if (file-exists-p git-root-projectile-file)
                                    (string-trim (with-temp-buffer
                                                   (insert-file-contents git-root-projectile-file)
                                                   (buffer-string)))
                                  git-root-dir)))
            (if (and (file-exists-p projectile-file) 
                     (not (string= projectile-dir git-root-dir)))
                ;; Different directories, show both
                (format "%s [%s]" git-root-name projectile-dir)
              ;; Same directory or no .projectile in subdir, just show git root name
              git-root-name))
        default-name)))
  
  (setq projectile-project-name-function '+dwc/projectile-project-name)
  
  ;; Configure LSP to use full path to git root for workspace identification
  (defun +dwc/lsp-workspace-root (orig-fun &rest args)
    "Override LSP workspace root to use full path to git root."
    (let ((git-root (ignore-errors 
                      (string-trim (shell-command-to-string "git rev-parse --show-toplevel")))))
      (if git-root
          git-root
        (apply orig-fun args))))
  
  ;; Apply the advice to LSP workspace root functions
  (advice-add 'lsp-workspace-root :around #'+dwc/lsp-workspace-root)
  
  ;; Custom project switching behavior
  (defun +dwc/switch-to-project ()
    "Switch to project and open most recent file from that project."
    (interactive)
    (let ((project (projectile-completing-read "Switch to project: "
                                               (projectile-relevant-known-projects))))
      (projectile-switch-project-by-name project)
      ;; Try to open the most recent file from this project
      (let ((recent-file (+dwc/get-most-recent-file-in-project project)))
        (when (and recent-file (file-exists-p recent-file))
          (find-file recent-file)))))
  
  (defun +dwc/get-most-recent-file-in-project (project-root)
    "Get the most recently accessed file in the given project."
    (let ((project-files (seq-filter 
                          (lambda (file)
                            (and (file-exists-p file)
                                 (string-prefix-p project-root file)))
                          recentf-list)))
      (car project-files)))
  
  (defun +dwc/switch-to-project-with-file ()
    "Switch to project and always prompt for file (old behavior)."
    (interactive)
    (call-interactively #'projectile-switch-project))
  )

;; Remap project switching keybindings
(map! :leader
      (:prefix "p"
       :desc "Switch to project" "p" #'+dwc/switch-to-project
       :desc "Switch to project with file" "P" #'+dwc/switch-to-project-with-file))

;; Custom named vterm function
(defun +dwc/vterm-named ()
  "Create a new vterm with a custom name in current window, cd'd to current file's directory."
  (interactive)
  (let ((name (read-string "Terminal name: "))
        (current-dir (if (buffer-file-name)
                         (file-name-directory (buffer-file-name))
                       default-directory)))
    (when (not (string-empty-p name))
      (let ((default-directory current-dir)
            (display-buffer-alist
             (cons '("\\*vterm.*\\*"
                     (display-buffer-same-window))
                   display-buffer-alist)))
        (vterm (format "*vterm: %s*" name))))))

;; Custom unnamed vterm function
(defun +dwc/vterm-here ()
  "Create a new unnamed vterm in current window, cd'd to current file's directory."
  (interactive)
  (let ((current-dir (if (buffer-file-name)
                         (file-name-directory (buffer-file-name))
                       default-directory))
        (display-buffer-alist
         (cons '("\\*vterm.*\\*"
                 (display-buffer-same-window))
               display-buffer-alist)))
    (let ((default-directory current-dir))
      (vterm))))

;; Map SPC o T to named vterm and SPC o C-t to unnamed vterm
(map! :leader
      (:prefix "o"
       :desc "Named vterm" "T" #'+dwc/vterm-named
       :desc "Unnamed vterm here" "C-t" #'+dwc/vterm-here))

;; Garbage collection
;; (setq gc-cons-threshold 10000000000    ;; ~1gb, probably not taking
;;       garbage-collection-messages t ;; show diagnostics
;;       gc-cons-percentage 0.5
;;       )
;; (run-with-idle-timer 30 t #'garbage-collect) ;; Run garbage collection if idle for at least 20 seconds (e.g., when checking Slack, etc)

;; Reasonable thresholds for interactive usage
(setq gc-cons-threshold (* 100 1024 1024))  ;; 100MB during runtime
(setq gc-cons-percentage 0.1)

;; Max threshold for startup (temporarily disables GC)
;; (setq doom-gc-cons-threshold (* 100 1024 1024)) ;; Doom uses this internally already

;; Automatically run GC when idle
(run-with-idle-timer 5 t #'garbage-collect)

;; Reduce GC during minibuffer usage
(add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook (lambda () (setq gc-cons-threshold (* 100 1024 1024))))


(after! lsp
  (setq
   lsp-ui-sideline-diagnostic-max-line-length 1000
   lsp-ui-sideline-diagnostic-max-lines 2
   lsp-ui-sideline-show-code-actions t
   lsp-ui-sideline-show-symbol t
   lsp-ui-sideline-delay 0.1

   ;;; NOTE: lsp-ui-doc seems like it sucks
   ;; lsp-ui-doc-enable t
   ;; lsp-ui-doc-position 'top
   ;; lsp-ui-doc-max-width 100
   ;; lsp-ui-doc-max-height 30

   ;; eldoc-echo-area-use-multiline-p t
   ;; lsp-eldoc-enable-hover t
   ;; lsp-eldoc-render-all t
   ;; max-lisp-eval-depth 10000 ;;FIXME: remove
   )
  )

(defun +dwc/toggle-last-buffer ()
  "Toggle between the last two buffers, including popups."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

(map! :leader "b p" #'+dwc/toggle-last-buffer)

;; (advice-add 'command-execute :before
;;             (lambda (cmd)
;;               (when cmd (message "Executing command: %s" cmd))))
;;
;;
;;

(setq org-latex-packages-alist '(("" "geometry" t))
      org-latex-default-class "article"
      org-latex-default-packages-alist
      '(("AUTO" "inputenc" t)
        ("T1" "fontenc" t)
        ("" "fixltx2e" nil)
        ("" "graphicx" t)
        ("" "longtable" nil)
        ("" "float" nil)
        ("" "wrapfig" nil)
        ("" "soul" t)
        ("" "textcomp" t)
        ("" "marvosym" t)
        ("" "wasysym" t)
        ("" "amsmath" t)
        ("" "amssymb" t)
        ("" "hyperref" nil))
      org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"))

(setq org-latex-packages-alist
      '(("top=0.75in,bottom=0.75in,left=0.75in,right=0.75in" "geometry" nil)))

(map! :leader
      "/" #'+default/search-buffer) ;; or #'consult-line if using Vertico/Consult

(map! :map (magit-status-mode-map magit-diff-section-base-map magit-diff-section-map)
      "C-<return>" #'magit-diff-visit-file-other-window)

;; just-mode configuration
(use-package! just-mode
  :mode "\\.justfile\\'"
  :mode "justfile\\'")

;; .envrc files should use sh-mode
(add-to-list 'auto-mode-alist '("\\.envrc.*\\'" . sh-mode))

;; Enable LSP for .envrc files
(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration '("\\.envrc.*\\'" . "shellscript")))

(after! org (define-key org-mode-map (kbd "C-c t")
                        (lambda () (interactive) (insert (format-time-string "%Y-%m-%d")) (org-cycle))))

;; color between windows. Dark gray.
(set-face-foreground 'vertical-border "#686868")

(setq scroll-margin 999)  ; Keep cursor vertically centered when scolling with mouse

;; Centered cursor mode
(use-package! centered-cursor-mode
  :config
  (global-centered-cursor-mode 1))
