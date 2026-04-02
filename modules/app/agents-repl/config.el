;;; config.el --- claude repl for doom emacs -*- lexical-binding: t; -*-

;; Author: Dodge Coates
;; URL: https://github.com/dodgecoates
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; A Claude Code REPL integration for Doom Emacs with workspace-aware
;; session management, input history, and status indicators.

;;; Code:

(message "hello world, this is agents-repl")

;; Cancel all previously registered timers on re-eval so we don't accumulate.
(defvar agents-repl--timers nil
  "List of active timers created by agents-repl.
Cancelled and reset whenever this file is re-evaluated.")

(dolist (timer agents-repl--timers)
  (when (timerp timer)
    (cancel-timer timer)))
(setq agents-repl--timers nil)

(defun agents-repl-debug/cancel-timers ()
  "Cancel all agents-repl timers."
  (interactive)
  (dolist (timer agents-repl--timers)
    (when (timerp timer)
      (cancel-timer timer)))
  (setq agents-repl--timers nil)
  (message "Cancelled all agents-repl timers."))

(defgroup agents-repl nil
  "Claude Code REPL integration for Doom Emacs."
  :group 'tools
  :prefix "agents-repl-")

;; Workspace identity
(defvar-local agents-repl--project-root nil
  "Buffer-local git root for Claude REPL buffers.
Set when vterm/input buffers are created so workspace-id works from them.")

(defun agents-repl--git-root (&optional dir)
  "Find the git root by walking up from DIR (default `default-directory').
Checks for both .git directory and .git file (worktrees)."
  (let ((dir (or dir default-directory)))
    (locate-dominating-file dir
      (lambda (d)
        (let ((git (expand-file-name ".git" d)))
          (or (file-directory-p git) (file-regular-p git)))))))

(defun agents-repl--path-canonical (path)
  "Return a canonical, stable string for PATH suitable for hashing.
Expands tildes and symlinks via `file-truename', then strips any trailing slash
via `directory-file-name' so that the same directory always produces the same hash."
  (directory-file-name (file-truename path)))

(defun agents-repl--workspace-id ()
  "Return a short identifier for the current git workspace.
Uses an MD5 hash of the canonical git root path.  Falls back to the buffer-local
`agents-repl--project-root' and then `default-directory'."
  (let ((root (agents-repl--resolve-root)))
    (when root
      (substring (md5 (agents-repl--path-canonical root)) 0 8))))

;; Single hash table for all per-workspace state.
(defvar agents-repl--workspaces (make-hash-table :test 'equal)
  "Hash table mapping workspace name → state plist.
Keys: :vterm-buffer :input-buffer :saved-window-config
      :return-window :prefix-counter :status :activity-time
      :git-clean :git-proc :backend")

(defun agents-repl--ws-get (ws key)
  "Get KEY from workspace WS's plist."
  (plist-get (gethash ws agents-repl--workspaces) key))

(defun agents-repl--ws-put (ws key val)
  "Set KEY to VAL in workspace WS's plist in `agents-repl--workspaces'.
Internally uses plist-put (which returns a new list) threaded into puthash."
  (puthash ws (plist-put (gethash ws agents-repl--workspaces) key val)
           agents-repl--workspaces))

(defun agents-repl--ws-del (ws)
  "Remove all state for workspace WS."
  (remhash ws agents-repl--workspaces))

(defun agents-repl--register-worktree-ws (ws-id path)
  "Mark the current workspace as a worktree workspace rooted at PATH.
WS-ID is the hash identifier (used for logging/buffer naming); the state
is stored under the workspace name from `+workspace-current-name'.
Sets :fresh-start t so the first SPC o c opens Claude without -c."
  (let ((ws (+workspace-current-name)))
    (agents-repl--log "register-worktree-ws ws-id=%s ws=%s path=%s" ws-id ws path)
    (agents-repl--ws-put ws :worktree-p t)
    (agents-repl--ws-put ws :worktree-path path)
    (agents-repl--ws-put ws :fresh-start t)))

(defun agents-repl-create-worktree-workspace ()
  "Create a new git worktree and switch to it as a project workspace.
Prompts for a name (may use branch-style slashes like DC/CV-100/cool-branch).
The worktree directory uses only the last path component; the full name becomes
the branch name.

If called from a worktree, the new worktree is created as a sibling (../<dirname>).
If called from a normal repo, it is created under ../<repo-name>-worktrees/<dirname>."
  (interactive)
  (let* ((git-root (string-trim
                    (shell-command-to-string "git rev-parse --show-toplevel")))
         (_ (when (string-match-p "^fatal" git-root)
              (user-error "Not in a git repository")))
         (name (read-string "Worktree name: "))
         (_ (when (string-empty-p name)
              (user-error "Name cannot be empty")))
         (dirname (file-name-nondirectory (directory-file-name name)))
         (branch-name (if (string-match-p "/" name) name nil))
         (git-root-parent (file-name-directory (directory-file-name git-root)))
         (in-worktree (file-regular-p (expand-file-name ".git" git-root)))
         (worktree-parent (if in-worktree
                              git-root-parent
                            (let* ((repo-name (file-name-nondirectory (directory-file-name git-root)))
                                   (wt-dir (expand-file-name (concat repo-name "-worktrees") git-root-parent)))
                              (make-directory wt-dir t)
                              wt-dir)))
         (path (expand-file-name dirname worktree-parent)))
    (message "[worktree] git-root=%s name=%s dirname=%s branch=%s in-worktree=%s path=%s old-ws=%s old-ws-id=%s"
             git-root name dirname (or branch-name "none") in-worktree path
             (+workspace-current-name) (agents-repl--workspace-id))
    (when (projectile-project-p path)
      (user-error "Worktree '%s' already exists — use SPC p p to switch to it" dirname))
    (let* ((cmd (if branch-name
                    (format "git -C %s worktree add -b %s %s 2>&1"
                            (shell-quote-argument git-root)
                            (shell-quote-argument branch-name)
                            (shell-quote-argument path))
                  (format "git -C %s worktree add %s 2>&1"
                          (shell-quote-argument git-root)
                          (shell-quote-argument path))))
           (result (shell-command-to-string cmd)))
      (message "[worktree] git cmd: %s" cmd)
      (message "[worktree] git result: %s" (string-trim result))
      (when (string-match-p "^fatal\\|^error" result)
        (user-error "git worktree add failed: %s" (string-trim result))))
    (write-region dirname nil (expand-file-name ".projectile" path))
    (message "[worktree] wrote .projectile, adding to projectile known projects")
    (projectile-add-known-project (file-name-as-directory path))
    (message "[worktree] switching to project: %s" (file-name-as-directory path))
    (projectile-switch-project-by-name (file-name-as-directory path))
    (let* ((canonical (agents-repl--path-canonical path))
           (ws-id (substring (md5 canonical) 0 8)))
      (message "[worktree] registering ws-id=%s canonical=%s" ws-id canonical)
      (agents-repl--register-worktree-ws ws-id path))))

(defvar agents-repl--fullscreen-config nil
  "Saved window configuration before fullscreen toggle.")

(defvar agents-repl--hide-overlay-refcount 0
  "Reference count for the hide overlay advice.
Only add advice when going from 0 to 1; only remove when going from 1 to 0.")

(defvar-local agents-repl-hide-overlay nil
  "Overlay used to hide Claude CLI input box.")

(defvar-local agents-repl--owning-workspace nil
  "Workspace name that owns this claude session.
Set when the user sends a message; used to correctly target workspace
state changes regardless of which persp the buffer drifts into.")

;; Input history
(defvar-local agents-repl--input-history nil
  "List of previous inputs, most recent first.")

(defvar-local agents-repl--history-index -1
  "Current position in history. -1 means not browsing.")

(defvar-local agents-repl--history-stash nil
  "Stashed in-progress text saved when history browsing begins.")

(defvar-local agents-repl--history-navigating nil
  "Non-nil while history navigation is replacing buffer text.")

(defcustom agents-repl-hide-input-box nil
  "When non-nil, hide the Claude CLI input box in the vterm buffer."
  :type 'boolean
  :group 'agents-repl)

(defcustom agents-repl-debug nil
  "When non-nil, emit debug messages to *Messages*."
  :type 'boolean
  :group 'agents-repl)

(defun agents-repl--log (fmt &rest args)
  "Log a timestamped debug message when `agents-repl-debug' is non-nil.
FMT and ARGS are passed to `message', prefixed with timestamp and [agents-repl]."
  (when agents-repl-debug
    (apply #'message (concat (format-time-string "%H:%M:%S.%3N") " [agents-repl] " fmt) args)))

(defcustom agents-repl-skip-permissions t
  "When non-nil, start Claude with --dangerously-skip-permissions and prepend the command prefix metaprompt."
  :type 'boolean
  :group 'agents-repl)

(defcustom agents-repl-prefix-period 7
  "Number of prompts between metaprompt prefix injections.
The prefix is sent on the first prompt and every Nth prompt thereafter."
  :type 'integer
  :group 'agents-repl)


(defcustom agents-repl-send-postfix "\n what do you think? do NOT code, just analyze."
  "String appended to input when sending via `agents-repl-send-with-postfix'."
  :type 'string
  :group 'agents-repl)

(defvar agents-repl-paste-delay 0.1
  "Seconds to wait after pasting before sending Return.
Used by `agents-repl--send-input-to-vterm' for large inputs.")

(defcustom agents-repl-command-prefix "DO NOT run any mutating git commands (push, reset, checkout, etc) without EXPLICIT PERMISSION from ME. Do not INSTALL or UNINSTALL anything without my EXPLICIT PERMISSION. Do not operate on any files OUTSIDE OF PROJECT without MY EXPLICIT PERMISSION. Do not take any actions unless it FOLLOWS DIRECTLY from an action EXPLICITLY REQUESTED in the following prompt. I will NEVER ask a rhetorical question -- do not infer that I want you to take action to fix the source of a bug i've just asked a question about."
  "When non-nil, this string is prepended (with a newline) before every input sent to Claude."
  :type 'string
  :group 'agents-repl)

(defvar agents-repl--command-prefix (format "<<*this is a metaprompt. I will periodically prefix my prompts with this to remind you of our restrictions for freely making changes. Do not be alarmed, this is merely a periodic reminder*: %s *metaprompt over* (rest is actual user request that you should respond to directly)>>\n\n" agents-repl-command-prefix)
  "Formatted metaprompt string prepended before every input when `agents-repl-skip-permissions' is non-nil.")

;;; Backend abstraction

(defcustom agents-repl-default-backend 'claude
  "Default backend for new agent REPL sessions.
Each workspace tracks its own backend in the workspace plist (:backend key).
Use `agents-repl-set-backend' to change the backend for the current workspace."
  :type '(choice (const :tag "Claude Code" claude)
                 (const :tag "OpenAI Codex" codex))
  :group 'agents-repl)

(defun agents-repl--claude-spinner-p (title)
  "Return non-nil if TITLE has a Claude Code spinner (non-ASCII prefix, not ✳)."
  (and (> (length title) 0)
       (not (string-prefix-p "✳" title))
       (string-match-p "^[^[:ascii:]]" title)))

(defun agents-repl--codex-spinner-p (title)
  "Return non-nil if TITLE has a Codex braille spinner prefix."
  (string-match-p "^[⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏]" title))

(defun agents-repl--interrupt-claude (vterm-buf)
  "Interrupt Claude: send ESC ESC, then 'i' after 250ms to re-enter chat."
  (with-current-buffer vterm-buf
    (vterm-send-key "<escape>")
    (vterm-send-key "<escape>"))
  (run-at-time 0.25 nil
               (lambda ()
                 (when (buffer-live-p vterm-buf)
                   (with-current-buffer vterm-buf
                     (vterm-send-string "i"))))))

(defun agents-repl--interrupt-codex (vterm-buf)
  "Interrupt Codex: send C-c."
  (with-current-buffer vterm-buf
    (vterm-send-key "C-c")))

(defvar agents-repl--backends
  `((claude
     :buffer-prefix   "claude"
     :start-command   ,(lambda (skip-perms)
                         (concat "claude -c"
                                 (when skip-perms " --dangerously-skip-permissions")))
     :spinner-p       agents-repl--claude-spinner-p
     :interrupt       agents-repl--interrupt-claude
     :exempt-inputs   ("/clear" "/usage" "/login" "/logout")
     :paste-returns   2
     :permission-watch t)
    (codex
     :buffer-prefix   "codex"
     :start-command   ,(lambda (skip-perms)
                         (concat "codex resume --last"
                                 (when skip-perms " --dangerously-bypass-approvals-and-sandbox")))
     :spinner-p       agents-repl--codex-spinner-p
     :interrupt       agents-repl--interrupt-codex
     :exempt-inputs   ("/clear" "/status" "/logout")
     :paste-returns   1
     :permission-watch nil))
  "Alist of backend symbol → config plist.
Each plist specifies how to start, detect activity, interrupt, and
interact with that particular AI CLI backend.")

(defun agents-repl--backend-get (key &optional ws)
  "Return KEY from the config plist for workspace WS's backend.
WS defaults to the current workspace."
  (let* ((ws (or ws (+workspace-current-name)))
         (backend (agents-repl--ws-backend ws)))
    (plist-get (alist-get backend agents-repl--backends) key)))

(defun agents-repl--ws-backend (&optional ws)
  "Return the backend symbol for workspace WS.
Falls back to `agents-repl-default-backend' if not set."
  (let ((ws (or ws (+workspace-current-name))))
    (or (agents-repl--ws-get ws :backend)
        agents-repl-default-backend)))

(defun agents-repl-set-backend (backend)
  "Set the agent backend for the current workspace.
Takes effect on the next session start; use `agents-repl-restart' to apply now."
  (interactive
   (list (intern (completing-read "Backend: "
                                  (mapcar #'car agents-repl--backends)
                                  nil t))))
  (agents-repl--ws-put (+workspace-current-name) :backend backend)
  (message "agents-repl: backend set to %s for this workspace (restart to apply)" backend))

;; Set to t once the backend has set its terminal title (meaning it's ready).
(defvar-local agents-repl--ready nil
  "Non-nil once Claude Code has finished starting up.")

(defun agents-repl--buffer-name (&optional suffix ws)
  "Return a project-specific buffer name for the current backend.
E.g. *claude-HASH*, *claude-input-HASH*, *codex-HASH*, *codex-input-HASH*.
SUFFIX is inserted before the hash (e.g. \"-input\"); WS overrides the workspace."
  (let* ((prefix (agents-repl--backend-get :buffer-prefix ws))
         (id (agents-repl--workspace-id)))
    (format "*%s%s-%s*" prefix (or suffix "") (or id "default"))))


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
                           (agents-repl--claude-buffer-p))
                      (agents-repl--grey 15)
                    result)))))

(defun agents-repl--live-return-window ()
  "Return the return-window for the current workspace if live, else `selected-window'."
  (let ((win (agents-repl--ws-get (+workspace-current-name) :return-window)))
    (if (and win (window-live-p win)) win (selected-window))))

;; Manual window layout: vterm on the right (full height), input below vterm.
(defun agents-repl--show-panels ()
  "Display vterm and input panels to the right of the current window.
Splits right for vterm (60% width to work window), then splits vterm bottom for input (15%)."
  (let* ((ws (+workspace-current-name))
         (vterm-buf (agents-repl--ws-get ws :vterm-buffer))
         (input-buf (agents-repl--ws-get ws :input-buffer)))
    (agents-repl--log "show-panels vterm=%s input=%s"
                      (and vterm-buf (buffer-name vterm-buf))
                      (and input-buf (buffer-name input-buf)))
    (let* ((work-win (agents-repl--live-return-window))
           (vterm-win (split-window work-win (round (* 0.6 (window-total-width work-win))) 'right))
           (input-win (split-window vterm-win (round (* -0.15 (window-total-height vterm-win))) 'below)))
      (agents-repl--refresh-vterm)
      (set-window-buffer vterm-win vterm-buf)
      (set-window-buffer input-win input-buf)
      (set-window-dedicated-p vterm-win t)
      (set-window-dedicated-p input-win t)
      ;; Hide from other-window, display-buffer, etc. so magit and friends
      ;; never try to reuse these windows.
      (set-window-parameter vterm-win 'no-other-window t)
      (set-window-parameter input-win 'no-other-window t)
      ;; Lock width to prevent resize-triggered reflow in vterm
      (set-window-parameter vterm-win 'window-size-fixed 'width)))
  (agents-repl--update-all-workspace-states))

(defun agents-repl--focus-input-panel ()
  "Focus the input panel window and enter insert state."
  (agents-repl--log "focus-input-panel")
  (when-let ((buf (agents-repl--ws-get (+workspace-current-name) :input-buffer))
             (win (get-buffer-window buf)))
    (select-window win)
    (evil-insert-state)))

(defun agents-repl--grey (n)
  "Return a hex color string for greyscale value N (0=black, 255=white)."
  (format "#%02x%02x%02x" n n n))

(defun agents-repl--claude-buffer-p (&optional buf)
  "Return non-nil if BUF (default: current buffer) is an agents-repl vterm buffer.
Matches buffers from any backend: *claude-HASH* or *codex-HASH*."
  (string-match-p "^\\*\\(claude\\|codex\\)-[0-9a-f]+\\*$"
                  (buffer-name (or buf (current-buffer)))))

(defun agents-repl--resolve-root ()
  "Return the project root directory.
Tries git root, then buffer-local project root, then `default-directory'."
  (let* ((git (agents-repl--git-root))
         (root (or git agents-repl--project-root default-directory))
         (source (cond (git "git") (agents-repl--project-root "buffer-local") (t "default-directory"))))
    (agents-repl--log "resolve-root source=%s root=%s" source root)
    root))

(defun agents-repl--vterm-live-p ()
  "Return non-nil if the Claude vterm buffer for the current workspace exists and is live."
  (let ((buf (agents-repl--ws-get (+workspace-current-name) :vterm-buffer)))
    (and buf (buffer-live-p buf))))

(defun agents-repl--set-buffer-background (grey-level)
  "Set default and fringe background to greyscale GREY-LEVEL in current buffer."
  (face-remap-add-relative 'default :background (agents-repl--grey grey-level))
  (face-remap-add-relative 'fringe :background (agents-repl--grey grey-level)))

(defun agents-repl--start-backend (&optional ws)
  "Send the backend startup command to the current vterm buffer.
On a fresh worktree workspace (:fresh-start t) the -c flag is omitted so
Claude starts a new conversation rather than resuming a prior one."
  (let* ((ws (or ws (+workspace-current-name)))
         (fresh (agents-repl--ws-get ws :fresh-start))
         (cmd (funcall (agents-repl--backend-get :start-command ws)
                       agents-repl-skip-permissions))
         (cmd (if fresh (string-replace " -c" "" cmd) cmd)))
    (when fresh
      (agents-repl--ws-put ws :fresh-start nil))
    (message "[agents-repl] start-backend dir=%s backend=%s fresh=%s cmd=%s"
             default-directory (agents-repl--ws-backend ws) (if fresh "yes" "no") cmd)
    (vterm-send-string (concat "clear && " cmd))
    (vterm-send-return)))

;; Instructions bar face
(defface agents-repl-header-line
  '((t :background "white" :foreground "black" :weight bold))
  "Face for the Claude Input header line.")

;; Input mode
(defun agents-repl--slash-intercept-backspace ()
  "Always forward backspace to vterm; also redirect to slash handler in slash mode.
Runs as a buffer-local `pre-command-hook'.  Setting `this-command' here causes
Emacs to call our handler instead of the originally resolved command."
  (when (memq this-command
              '(evil-delete-backward-char-and-join
                evil-delete-backward-char
                delete-backward-char
                backward-delete-char-untabify))
    (unless agents-slash-input-mode
      (when (= (buffer-size) 0)
        (let* ((ws (+workspace-current-name))
               (vterm-buf (agents-repl--ws-get ws :vterm-buffer)))
          (when (and vterm-buf (buffer-live-p vterm-buf))
            (with-current-buffer vterm-buf
              (vterm-send-key "<backspace>"))))))
    (when agents-slash-input-mode
      (setq this-command #'agents-repl--slash-backspace))))

(define-derived-mode agents-input-mode fundamental-mode "Claude Input"
  "Major mode for Claude REPL input buffer."
  (setq-local header-line-format
              "RET: send | C-RET: send+postfix | C-c C-c: clear+save | C-c C-k: interrupt | <up>/<down>: history")
  (face-remap-add-relative 'header-line 'agents-repl-header-line)
  (agents-repl--set-buffer-background 37)
  (visual-line-mode 1)
  (add-hook 'after-change-functions #'agents-repl--history-on-change nil t)
  (add-hook 'pre-command-hook #'agents-repl--slash-intercept-backspace nil t))

(defun agents-repl-discard-input ()
  "Save current input to history, clear the buffer, and enter insert state."
  (interactive)
  (agents-repl--log "discard-input")
  (when agents-slash-input-mode
    (setq agents-repl--slash-stack nil)
    (agents-slash-input-mode -1))
  (agents-repl--history-push)
  (agents-repl--history-reset)
  (erase-buffer)
  (evil-insert-state))

(defun agents-repl-scroll-down ()
  "Scroll the Claude vterm buffer down."
  (interactive)
  (agents-repl--log "scroll-down")
  (when (agents-repl--vterm-live-p)
    (with-current-buffer (agents-repl--ws-get (+workspace-current-name) :vterm-buffer)
      (vterm-send-down))))

(defun agents-repl-scroll-up ()
  "Scroll the Claude vterm buffer up."
  (interactive)
  (agents-repl--log "scroll-up")
  (when (agents-repl--vterm-live-p)
    (with-current-buffer (agents-repl--ws-get (+workspace-current-name) :vterm-buffer)
      (vterm-send-up))))

(defun agents-repl--scroll-output-up ()
  "Scroll the Claude vterm output window up (toward older output)."
  (interactive)
  (when (agents-repl--vterm-live-p)
    (let* ((vterm-buf (agents-repl--ws-get (+workspace-current-name) :vterm-buffer))
           (vterm-win (get-buffer-window vterm-buf)))
      (when vterm-win
        (with-selected-window vterm-win
          (scroll-down 3))))))

(defun agents-repl--scroll-output-down ()
  "Scroll the Claude vterm output window down (toward newer output)."
  (interactive)
  (when (agents-repl--vterm-live-p)
    (let* ((vterm-buf (agents-repl--ws-get (+workspace-current-name) :vterm-buffer))
           (vterm-win (get-buffer-window vterm-buf)))
      (when vterm-win
        (with-selected-window vterm-win
          (scroll-up 3))))))

(defun agents-repl--input-wheel-up ()
  "Scroll the Claude vterm window toward older output (mouse wheel up from input)."
  (interactive)
  (when (agents-repl--vterm-live-p)
    (let* ((vterm-buf (agents-repl--ws-get (+workspace-current-name) :vterm-buffer))
           (vterm-win (get-buffer-window vterm-buf)))
      (when vterm-win
        (with-selected-window vterm-win
          (scroll-down 3))))))

(defun agents-repl--input-wheel-down ()
  "Scroll the Claude vterm window toward newer output (mouse wheel down from input)."
  (interactive)
  (when (agents-repl--vterm-live-p)
    (let* ((vterm-buf (agents-repl--ws-get (+workspace-current-name) :vterm-buffer))
           (vterm-win (get-buffer-window vterm-buf)))
      (when vterm-win
        (with-selected-window vterm-win
          (scroll-up 3))))))

(map! :map agents-input-mode-map
      :ni "RET"       #'agents-repl-send
      :ni "S-RET"     #'newline
      :i  "/"         #'agents-repl--slash-start
      :ni "C-RET"     #'agents-repl-send-with-postfix
      :ni "C-S-RET"   #'agents-repl-send-with-metaprompt
      [remap +default/newline-below] #'agents-repl-send-with-postfix
      [remap +default/newline-above] #'agents-repl-send-with-metaprompt
      :ni "C-c C-k"   #'agents-repl-interrupt
      :ni "C-c C-c"   #'agents-repl-discard-input
      :ni "C-c y"     (cmd! (agents-repl-send-char "y"))
      :ni "C-c n"     (cmd! (agents-repl-send-char "n"))
      :ni "C-c r"     #'agents-repl-restart
      :ni "C-c q"     #'agents-repl-kill
      :ni "C-S-m"     #'agents-repl-cycle
      :ni "C-h"       #'evil-window-left
      :n  "C-n"       #'agents-repl-scroll-down
      :n  "C-p"       #'agents-repl-scroll-up
      :ni "C-v"       #'agents-repl-paste-to-vterm
      :n  "<up>"        #'agents-repl--history-prev
      :n  "<down>"      #'agents-repl--history-next
      :i  "<up>"        #'agents-repl--send-up-arrow
      :i  "<down>"      #'agents-repl--send-down-arrow
      :ni "S-<up>"      #'agents-repl--scroll-output-up
      :ni "S-<down>"    #'agents-repl--scroll-output-down
      :ni "C-S-p"       #'agents-repl--scroll-output-up
      :ni "C-S-n"       #'agents-repl--scroll-output-down
      [wheel-up]        #'agents-repl--input-wheel-up
      [wheel-down]      #'agents-repl--input-wheel-down)

;; C-S-0 through C-S-9: send digit to Claude
(dotimes (i 10)
  (let ((char (number-to-string i)))
    (define-key agents-input-mode-map (kbd (format "C-S-%s" char))
      (lambda () (interactive) (agents-repl-send-char char)))))

;; 0-9 in insert mode: if the buffer is empty, enter pass-through mode and
;; forward the digit to vterm; otherwise insert normally.
(dotimes (i 10)
  (let ((char (number-to-string i)))
    (evil-define-key 'insert agents-input-mode-map (kbd char)
      (lambda () (interactive)
        (agents-repl--passthrough-start char)))))

;; Input history persistence
(defun agents-repl--history-file ()
  "Return the path to the history file for the current project."
  (expand-file-name ".agents-repl-history" (agents-repl--resolve-root)))

(defun agents-repl--history-save (&optional ws)
  "Write input history to disk.
Resolves the project root from the input buffer's local variable
rather than `default-directory' of whatever buffer is current.
WS defaults to the current workspace name."
  (agents-repl--log "history-save")
  (let* ((ws (or ws (+workspace-current-name)))
         (buf (and ws (agents-repl--ws-get ws :input-buffer))))
    (when (and buf (buffer-live-p buf))
      (let* ((root (buffer-local-value 'agents-repl--project-root buf))
             (history (buffer-local-value 'agents-repl--input-history buf))
             (file (expand-file-name ".agents-repl-history" (or root default-directory))))
        (when history
          (with-temp-file file
            (prin1 history (current-buffer))))))))

(defun agents-repl--history-restore ()
  "Load input history from disk into the current buffer."
  (agents-repl--log "history-restore")
  (let ((file (agents-repl--history-file)))
    (when (file-exists-p file)
      (setq agents-repl--input-history
            (with-temp-buffer
              (insert-file-contents file)
              (read (current-buffer)))))))

;; Input history functions
(defun agents-repl--history-push ()
  "Save current input buffer text to history.
Skips empty strings and duplicates of the most recent entry."
  (let ((text (string-trim (buffer-string))))
    (unless (string-empty-p text)
      (unless (equal text (car agents-repl--input-history))
        (push text agents-repl--input-history)))))

(defun agents-repl--history-reset ()
  "Reset history browsing index to the default (not browsing) state."
  (setq agents-repl--history-index -1))

(defun agents-repl--history-prev ()
  "Navigate to the previous (older) history entry."
  (interactive)
  (agents-repl--log "history-prev index=%d" agents-repl--history-index)
  (when agents-repl--input-history
    (let ((next-index (1+ agents-repl--history-index)))
      (when (< next-index (length agents-repl--input-history))
        ;; Stash current text on first navigation
        (when (= agents-repl--history-index -1)
          (setq agents-repl--history-stash (buffer-string)))
        (setq agents-repl--history-index next-index)
        (let ((agents-repl--history-navigating t))
          (erase-buffer)
          (insert (nth agents-repl--history-index agents-repl--input-history)))))))

(defun agents-repl--history-next ()
  "Navigate to the next (newer) history entry, or restore stashed text."
  (interactive)
  (agents-repl--log "history-next index=%d" agents-repl--history-index)
  (when (>= agents-repl--history-index 0)
    (let ((next-index (1- agents-repl--history-index)))
      (let ((agents-repl--history-navigating t))
        (erase-buffer)
        (if (< next-index 0)
            ;; Past newest entry — restore stash
            (progn
              (setq agents-repl--history-index -1)
              (when agents-repl--history-stash
                (insert agents-repl--history-stash)))
          (setq agents-repl--history-index next-index)
          (insert (nth agents-repl--history-index agents-repl--input-history)))))))

(defun agents-repl--history-on-change (&rest _)
  "Reset history browsing when the user edits the buffer directly."
  (unless agents-repl--history-navigating
    (when (>= agents-repl--history-index 0)
      (agents-repl--log "history-on-change resetting from index=%d" agents-repl--history-index))
    (setq agents-repl--history-index -1)))

;; Core functions
(defun agents-repl--skip-metaprompt-p (raw)
  "Return non-nil if RAW input should never have the metaprompt prepended.
Exempt inputs are defined per-backend via :exempt-inputs.  Bare numerals
(option-selection inputs) are also always exempt."
  (let ((trimmed (string-trim-right raw)))
    (or (member trimmed (agents-repl--backend-get :exempt-inputs))
        (string-match-p "^[0-9]+$" trimmed))))

(defvar agents-repl-send-posthooks
  '(("^/clear$" . agents-repl--posthook-reset-prefix-counter))
  "Alist of (PATTERN . FUNCTION) posthooks run after input is sent.
PATTERN is a string or regexp matched against the raw input (trimmed).
FUNCTION is called with (WS RAW) where WS is the workspace name and RAW is the input.")

(defun agents-repl--posthook-reset-prefix-counter (ws _raw)
  "Reset the metaprompt prefix counter for workspace WS.
Resets to 1 (just past the firing point) so the next send does not
immediately re-trigger the metaprompt."
  (agents-repl--ws-put ws :prefix-counter 1))

(defun agents-repl--run-send-posthooks (ws raw)
  "Run posthooks matching RAW input for workspace WS."
  (let ((trimmed (string-trim-right raw)))
    (dolist (hook agents-repl-send-posthooks)
      (when (string-match-p (car hook) trimmed)
        (agents-repl--log "posthook matched pattern=%s" (car hook))
        (funcall (cdr hook) ws raw)))))

(defun agents-repl--prepare-input (ws)
  "Read and return input from the input buffer for workspace WS.
Periodically prepends the metaprompt prefix when `agents-repl-skip-permissions' is on."
  (let* ((input-buf (agents-repl--ws-get ws :input-buffer))
         (counter (or (agents-repl--ws-get ws :prefix-counter) 0))
         (raw (with-current-buffer input-buf (buffer-string))))
    (agents-repl--log "prepare-input counter=%d period=%d" counter agents-repl-prefix-period)
    (if (and agents-repl-skip-permissions agents-repl-command-prefix
             (not (agents-repl--skip-metaprompt-p raw))
             (zerop (mod counter agents-repl-prefix-period)))
        (concat agents-repl--command-prefix raw)
      raw)))

(defun agents-repl--send-input-to-vterm (vterm-buf input)
  "Send INPUT string to VTERM-BUF.
Uses paste mode for large inputs to avoid truncation.
The number of Return keypresses after a large paste is controlled by the
backend's :paste-returns config (2 for Claude, 1 for Codex)."
  (agents-repl--log "send-input-to-vterm len=%d mode=%s" (length input)
                    (if (> (length input) 200) "paste" "direct"))
  (let ((paste-returns (or (buffer-local-value 'agents-repl--owning-workspace vterm-buf)
                           (+workspace-current-name))))
    (setq paste-returns (agents-repl--backend-get :paste-returns paste-returns))
    (with-current-buffer vterm-buf
      (if (> (length input) 200)
          (let ((buf (current-buffer)))
            (vterm-send-string input t)
            (run-at-time agents-repl-paste-delay nil
                         (lambda ()
                           (when (buffer-live-p buf)
                             (with-current-buffer buf
                               (vterm-send-return)
                               (when (> paste-returns 1)
                                 (run-at-time 0.05 nil
                                              (lambda ()
                                                (when (buffer-live-p buf)
                                                  (with-current-buffer buf
                                                    (vterm-send-return)
                                                    (agents-repl--refresh-vterm)))))))))))
        (vterm-send-string input)
        (vterm-send-return)
        (agents-repl--refresh-vterm)))))

(defun agents-repl--mark-ws-thinking (ws)
  "Mark workspace WS as thinking: set state and record activity."
  (agents-repl--log "mark-ws-thinking ws=%s" ws)
  (agents-repl--ws-set ws :thinking)
  (agents-repl--touch-activity ws))

(defun agents-repl--clear-input (ws)
  "Push current input to history, reset browsing, and clear the input buffer for workspace WS."
  (agents-repl--log "clear-input")
  (let ((input-buf (agents-repl--ws-get ws :input-buffer)))
    (when input-buf
      (with-current-buffer input-buf
        (agents-repl--history-push)
        (agents-repl--history-reset)
        (erase-buffer)))))

(defun agents-repl-send ()
  "Send input buffer contents to Claude, clear buffer, and return to previous window."
  (interactive)
  (let* ((ws (+workspace-current-name)))
    (unless ws (error "agents-repl-send: no active workspace"))
    (let* ((input-buf (agents-repl--ws-get ws :input-buffer))
           (vterm-buf (agents-repl--ws-get ws :vterm-buffer)))
      (when (and input-buf (agents-repl--vterm-live-p))
        (let* ((raw (with-current-buffer input-buf (buffer-string)))
               (input (agents-repl--prepare-input ws)))
        (agents-repl--log "send ws=%s len=%d" ws (length input))
        (agents-repl--ws-put ws :prefix-counter
                             (1+ (or (agents-repl--ws-get ws :prefix-counter) 0)))
        ;; Pin the owning workspace on the vterm buffer so title-change
        ;; clears the correct workspace even if the buffer drifts between persps.
        (when vterm-buf
          (with-current-buffer vterm-buf
            (setq-local agents-repl--owning-workspace ws)))
        (agents-repl--mark-ws-thinking ws)
        (agents-repl--send-input-to-vterm vterm-buf input)
        (agents-repl--clear-input ws)
        (agents-repl--run-send-posthooks ws raw)
        (let ((ret (agents-repl--ws-get ws :return-window)))
          (when (and ret (window-live-p ret))
            (select-window ret))))))))

(defun agents-repl--remember-return-window ()
  "Save the current window as the return target, unless we're in the input buffer."
  (let ((ws (+workspace-current-name)))
    (unless (eq (current-buffer) (agents-repl--ws-get ws :input-buffer))
      (agents-repl--log "remember-return-window %s" (selected-window))
      (agents-repl--ws-put ws :return-window (selected-window)))))

(defun agents-repl--restore-layout ()
  "Restore the window layout from before panels were shown.
Falls back to hiding panels and selecting the return window."
  (let* ((ws (+workspace-current-name))
         (saved (agents-repl--ws-get ws :saved-window-config)))
    (agents-repl--log "restore-layout saved-config=%s" (if saved "yes" "no"))
    (if saved
        (progn
          (set-window-configuration saved)
          (agents-repl--ws-put ws :saved-window-config nil))
      (agents-repl--hide-panels)
      (let ((ret (agents-repl--ws-get ws :return-window)))
        (when (and ret (window-live-p ret))
          (select-window ret))))))

(defun agents-repl-send-and-hide ()
  "Send input to Claude and hide both panels."
  (interactive)
  (agents-repl--log "send-and-hide")
  (agents-repl-send)
  (agents-repl--restore-layout))

(defun agents-repl-send-with-metaprompt ()
  "Send input with the metaprompt prefix, bypassing the counter."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (input-buf (agents-repl--ws-get ws :input-buffer))
         (vterm-buf (agents-repl--ws-get ws :vterm-buffer)))
    (when (and input-buf (agents-repl--vterm-live-p))
      (let* ((raw (with-current-buffer input-buf (buffer-string)))
             (input (if (and agents-repl-skip-permissions agents-repl-command-prefix
                             (not (agents-repl--skip-metaprompt-p raw)))
                        (concat agents-repl--command-prefix raw)
                      raw)))
        (agents-repl--log "send-with-metaprompt ws=%s len=%d" ws (length input))
        (agents-repl--ws-put ws :prefix-counter
                             (1+ (or (agents-repl--ws-get ws :prefix-counter) 0)))
        (when vterm-buf
          (with-current-buffer vterm-buf
            (setq-local agents-repl--owning-workspace ws)))
        (agents-repl--mark-ws-thinking ws)
        (agents-repl--send-input-to-vterm vterm-buf input)
        (agents-repl--clear-input ws)
        (agents-repl--run-send-posthooks ws raw)
        (let ((ret (agents-repl--ws-get ws :return-window)))
          (when (and ret (window-live-p ret))
            (select-window ret)))))))

(defun agents-repl-send-with-postfix ()
  "Append `agents-repl-send-postfix' to the input buffer, then send."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (input-buf (agents-repl--ws-get ws :input-buffer)))
    (when input-buf
      (with-current-buffer input-buf
        (goto-char (point-max))
        (insert agents-repl-send-postfix))))
  (agents-repl-send))

(defun agents-repl-send-char (char)
  "Send a single character to Claude."
  (agents-repl--log "send-char %s" char)
  (when (agents-repl--vterm-live-p)
    (let ((vterm-buf (agents-repl--ws-get (+workspace-current-name) :vterm-buffer)))
      (with-current-buffer vterm-buf
        (vterm-send-string char)
        (vterm-send-return)))))

;;; Slash-command pass-through mode
;;
;; When the user types "/" into an empty input buffer, every subsequent
;; keystroke is forwarded directly to vterm without being inserted into the
;; input buffer.  The buffer stays visually empty.  Backspace is forwarded too,
;; and deleting back past the initial "/" exits the mode.

(defvar-local agents-repl--slash-stack nil
  "Stack of characters forwarded to vterm in slash mode.
Each element is the string that was sent (the leading \"/\" is the first entry).
Popped on backspace; when empty the mode exits.")

(defun agents-repl--slash-vterm-send (str)
  "Send STR to the current workspace's vterm buffer without a trailing return."
  (let* ((ws (+workspace-current-name))
         (vterm-buf (agents-repl--ws-get ws :vterm-buffer)))
    (when (and vterm-buf (buffer-live-p vterm-buf))
      (with-current-buffer vterm-buf
        (vterm-send-string str)))))

(defun agents-repl--slash-forward-char ()
  "Forward the typed character to vterm without inserting it into the buffer."
  (interactive)
  (let ((char (string last-command-event)))
    (agents-repl--slash-vterm-send char)
    (push char agents-repl--slash-stack)))

(defun agents-repl--slash-backspace ()
  "Pop from the slash stack; send backspace to vterm; exit mode when stack is empty."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (vterm-buf (agents-repl--ws-get ws :vterm-buffer)))
    (when (and vterm-buf (buffer-live-p vterm-buf))
      (with-current-buffer vterm-buf
        (vterm-send-key "<backspace>"))))
  (pop agents-repl--slash-stack)
  (when (null agents-repl--slash-stack)
    (agents-slash-input-mode -1)))

(defun agents-repl--slash-return ()
  "Send return to vterm and exit slash mode."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (vterm-buf (agents-repl--ws-get ws :vterm-buffer)))
    (when (and vterm-buf (buffer-live-p vterm-buf))
      (with-current-buffer vterm-buf
        (vterm-send-return))))
  (setq agents-repl--slash-stack nil)
  (agents-slash-input-mode -1))

(defun agents-repl--passthrough-start (char)
  "Enter pass-through mode and send CHAR to vterm if the buffer is empty.
Otherwise insert CHAR normally.  Used for /, digits, and any other
characters that should go directly to Claude when typed first."
  (if (= (buffer-size) 0)
      (progn
        (agents-slash-input-mode 1)
        (setq agents-repl--slash-stack (list char))
        (agents-repl--slash-vterm-send char))
    (self-insert-command 1 (string-to-char char))))

(defun agents-repl--slash-start ()
  "Enter pass-through mode if the buffer is empty, else insert / normally."
  (interactive)
  (agents-repl--passthrough-start "/"))

(define-minor-mode agents-slash-input-mode
  "Minor mode that transparently forwards keystrokes to Claude vterm.
Active when the user begins input with /. The input buffer stays empty;
all characters are sent directly to vterm."
  :lighter " /…"
  :keymap (make-sparse-keymap))

;; Use remaps throughout so evil keymap priority is irrelevant — remaps are
;; resolved after key→command lookup and apply across all evil states.
(defun agents-repl--slash-tab ()
  "Forward a tab character to vterm in slash mode."
  (interactive)
  (agents-repl--slash-vterm-send "\t")
  (push "\t" agents-repl--slash-stack))

(defun agents-repl--send-up-arrow ()
  "Send up arrow key to the current workspace's vterm."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (vterm-buf (agents-repl--ws-get ws :vterm-buffer)))
    (when (and vterm-buf (buffer-live-p vterm-buf))
      (with-current-buffer vterm-buf
        (vterm-send-key "<up>")))))

(defun agents-repl--send-down-arrow ()
  "Send down arrow key to the current workspace's vterm."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (vterm-buf (agents-repl--ws-get ws :vterm-buffer)))
    (when (and vterm-buf (buffer-live-p vterm-buf))
      (with-current-buffer vterm-buf
        (vterm-send-key "<down>")))))

(map! :map agents-slash-input-mode-map
      [remap self-insert-command]                #'agents-repl--slash-forward-char
      [remap indent-for-tab-command]             #'agents-repl--slash-tab
      [remap evil-delete-backward-char-and-join] #'agents-repl--slash-backspace
      [remap delete-backward-char]               #'agents-repl--slash-backspace
      [remap backward-delete-char-untabify]      #'agents-repl--slash-backspace
      [remap agents-repl-send]                   #'agents-repl--slash-return
      :ni "<up>"   #'ignore
      :ni "<down>" #'ignore)

(defun agents-repl--ensure-session ()
  "Ensure a Claude session exists (vterm + input + overlay).
No-op if already running."
  (unless (agents-repl--vterm-running-p)
    (agents-repl--log "ensure-session: starting new session")
    (let ((ws (+workspace-current-name)))
      (unless ws (error "agents-repl--ensure-session: no active workspace"))
      (agents-repl--ensure-vterm-buffer ws)
      (agents-repl--ensure-input-buffer ws)
      (agents-repl--ws-put ws :prefix-counter 0))
    (agents-repl--enable-hide-overlay)))

(defun agents-repl--send-to-backend (text)
  "Send TEXT to Claude, starting it if needed."
  (agents-repl--log "send-to-claude len=%d" (length text))
  (let ((ws (+workspace-current-name)))
    (agents-repl--ensure-session)
    (agents-repl--send-input-to-vterm
     (agents-repl--ws-get ws :vterm-buffer) text)))

(defun agents-repl--rel-path ()
  "Return the current file path relative to the project root."
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Buffer %s is not visiting a file" (buffer-name)))
    (file-relative-name file (agents-repl--resolve-root))))

(defun agents-repl-explain ()
  "Ask Claude to explain the selected region, current line, or current file.
With active region: sends file path and line range.
In magit hunk: sends file path and hunk line range.
Without region: sends file path and current line."
  (interactive)
  (let* ((msg
          (cond
           ;; Active region in any buffer
           ((use-region-p)
            (let ((rel (agents-repl--rel-path))
                  (start-line (line-number-at-pos (region-beginning)))
                  (end-line (line-number-at-pos (region-end))))
              (deactivate-mark)
              (format "please explain %s:%d-%d" rel start-line end-line)))
           ;; Magit hunk section
           ((and (derived-mode-p 'magit-diff-mode 'magit-status-mode
                                 'magit-revision-mode)
                 (magit-section-match 'hunk))
            (let* ((section (magit-current-section))
                   (file (magit-file-at-point))
                   (range (oref section to-range))
                   (start (car range))
                   (len (cadr range))
                   (end (+ start len -1))
                   (rel (file-relative-name
                         (expand-file-name file (magit-toplevel))
                         (agents-repl--resolve-root))))
              (format "please explain %s:%d-%d" rel start end)))
           ;; Default: current line
           (t
            (format "please explain %s:%d"
                    (agents-repl--rel-path) (line-number-at-pos (point)))))))
    (agents-repl--log "explain %s" msg)
    (agents-repl--send-to-backend msg)))

(defcustom agents-repl-explain-diff-prompt
  "please explain the changes"
  "Prompt sent to Claude by explain-diff commands."
  :type 'string
  :group 'agents-repl)

(defun agents-repl-explain-diff-worktree ()
  "Ask Claude to explain unstaged changes."
  (interactive)
  (agents-repl--send-diff-analysis "unstaged changes (git diff)" agents-repl-explain-diff-prompt))

(defun agents-repl-explain-diff-staged ()
  "Ask Claude to explain staged changes."
  (interactive)
  (agents-repl--send-diff-analysis "staged changes (git diff --cached)" agents-repl-explain-diff-prompt))

(defun agents-repl-explain-diff-uncommitted ()
  "Ask Claude to explain all uncommitted changes."
  (interactive)
  (agents-repl--send-diff-analysis "uncommitted changes (git diff HEAD)" agents-repl-explain-diff-prompt))

(defun agents-repl-explain-diff-head ()
  "Ask Claude to explain the last commit."
  (interactive)
  (agents-repl--send-diff-analysis "last commit (git show HEAD)" agents-repl-explain-diff-prompt))

(defun agents-repl-explain-diff-branch ()
  "Ask Claude to explain all changes in the current branch."
  (interactive)
  (agents-repl--send-diff-analysis agents-repl-branch-diff-spec agents-repl-explain-diff-prompt))

(defun agents-repl-interrupt ()
  "Interrupt the active backend operation."
  (interactive)
  (agents-repl--log "interrupt backend=%s" (agents-repl--ws-backend))
  (when (agents-repl--vterm-live-p)
    (let ((vterm-buf (agents-repl--ws-get (+workspace-current-name) :vterm-buffer)))
      (funcall (agents-repl--backend-get :interrupt) vterm-buf))))

;; Git diff analysis commands
(defcustom agents-repl-update-pr-prompt
  "please update the PR description for the PR corresponding to our branch"
  "Prompt sent to Claude by `agents-repl-update-pr'."
  :type 'string
  :group 'agents-repl)

(defun agents-repl-update-pr ()
  "Ask Claude to update the PR description for the current branch."
  (interactive)
  (agents-repl--send-to-backend agents-repl-update-pr-prompt))

(defcustom agents-repl-update-pr-diff-prompt
  "please update the PR description"
  "Prompt sent to Claude by update-pr-diff commands."
  :type 'string
  :group 'agents-repl)

(defun agents-repl-update-pr-diff-worktree ()
  "Ask Claude to update the PR description for unstaged changes."
  (interactive)
  (agents-repl--send-diff-analysis "UNSTAGED changes (git diff). So not consider staged changes or committed changes." agents-repl-update-pr-diff-prompt))

(defun agents-repl-update-pr-diff-staged ()
  "Ask Claude to update the PR description for staged changes."
  (interactive)
  (agents-repl--send-diff-analysis "STAGED changes (git diff --cached). Do not consider unstaged changes or committed changes." agents-repl-update-pr-diff-prompt))

(defun agents-repl-update-pr-diff-uncommitted ()
  "Ask Claude to update the PR description for all uncommitted changes."
  (interactive)
  (agents-repl--send-diff-analysis "All UNCOMMITTED changes (git diff HEAD). Consider BOTH staged and unstaged changes. Do not consider committed changes." agents-repl-update-pr-diff-prompt))

(defun agents-repl-update-pr-diff-head ()
  "Ask Claude to update the PR description for the last commit."
  (interactive)
  (agents-repl--send-diff-analysis "last commit (git show HEAD)." agents-repl-update-pr-diff-prompt))

(defun agents-repl-update-pr-diff-branch ()
  "Ask Claude to update the PR description for all changes in the current branch."
  (interactive)
  (agents-repl--send-diff-analysis agents-repl-branch-diff-spec agents-repl-update-pr-diff-prompt))

(defcustom agents-repl-run-tests-prompt
  "please run tests, and summarize the issues found and probable causes"
  "Prompt sent to Claude by run-tests commands."
  :type 'string
  :group 'agents-repl)

(defcustom agents-repl-run-lint-prompt
  "please run lint, and address any issues found"
  "Prompt sent to Claude by run-lint commands."
  :type 'string
  :group 'agents-repl)

(defcustom agents-repl-run-all-prompt
  "please run lint and tests, and address any issues found for both"
  "Prompt sent to Claude by run-all commands."
  :type 'string
  :group 'agents-repl)

(defun agents-repl-run-tests-worktree ()
  "Run tests for unstaged changes."
  (interactive)
  (agents-repl--send-diff-analysis "unstaged changes (git diff)" agents-repl-run-tests-prompt))

(defun agents-repl-run-tests-staged ()
  "Run tests for staged changes."
  (interactive)
  (agents-repl--send-diff-analysis "staged changes (git diff --cached)" agents-repl-run-tests-prompt))

(defun agents-repl-run-tests-uncommitted ()
  "Run tests for all uncommitted changes."
  (interactive)
  (agents-repl--send-diff-analysis "uncommitted changes (git diff HEAD)" agents-repl-run-tests-prompt))

(defun agents-repl-run-tests-head ()
  "Run tests for the last commit."
  (interactive)
  (agents-repl--send-diff-analysis "last commit (git show HEAD)" agents-repl-run-tests-prompt))

(defun agents-repl-run-tests-branch ()
  "Run tests for all changes in the current branch."
  (interactive)
  (agents-repl--send-diff-analysis agents-repl-branch-diff-spec agents-repl-run-tests-prompt))

(defun agents-repl-run-lint-worktree ()
  "Run lint for unstaged changes."
  (interactive)
  (agents-repl--send-diff-analysis "unstaged changes (git diff)" agents-repl-run-lint-prompt))

(defun agents-repl-run-lint-staged ()
  "Run lint for staged changes."
  (interactive)
  (agents-repl--send-diff-analysis "staged changes (git diff --cached)" agents-repl-run-lint-prompt))

(defun agents-repl-run-lint-uncommitted ()
  "Run lint for all uncommitted changes."
  (interactive)
  (agents-repl--send-diff-analysis "uncommitted changes (git diff HEAD)" agents-repl-run-lint-prompt))

(defun agents-repl-run-lint-head ()
  "Run lint for the last commit."
  (interactive)
  (agents-repl--send-diff-analysis "last commit (git show HEAD)" agents-repl-run-lint-prompt))

(defun agents-repl-run-lint-branch ()
  "Run lint for all changes in the current branch."
  (interactive)
  (agents-repl--send-diff-analysis agents-repl-branch-diff-spec agents-repl-run-lint-prompt))

(defun agents-repl-run-all-worktree ()
  "Run lint and tests for unstaged changes."
  (interactive)
  (agents-repl--send-diff-analysis "unstaged changes (git diff)" agents-repl-run-all-prompt))

(defun agents-repl-run-all-staged ()
  "Run lint and tests for staged changes."
  (interactive)
  (agents-repl--send-diff-analysis "staged changes (git diff --cached)" agents-repl-run-all-prompt))

(defun agents-repl-run-all-uncommitted ()
  "Run lint and tests for all uncommitted changes."
  (interactive)
  (agents-repl--send-diff-analysis "uncommitted changes (git diff HEAD)" agents-repl-run-all-prompt))

(defun agents-repl-run-all-head ()
  "Run lint and tests for the last commit."
  (interactive)
  (agents-repl--send-diff-analysis "last commit (git show HEAD)" agents-repl-run-all-prompt))

(defun agents-repl-run-all-branch ()
  "Run lint and tests for all changes in the current branch."
  (interactive)
  (agents-repl--send-diff-analysis agents-repl-branch-diff-spec agents-repl-run-all-prompt))

(defconst agents-repl--test-quality-prompt
  "please analyze tests to ensure they are following AAA standards for testing. Please be sure to confine your analysis to the specified context (branch, HEAD, uncommitted changes, etc). They should be employing DRY principle for refactoring as well (extract repeated code into helpers, use builder pattern to facilitate test DSL). We should only be testing one thing per test (can extract tests into subtests to ensure this). Ensure that tests are correctly grouped into subtests, and that very similar/redundant suites are merged. We should not be using ANY timing logic in tests. If there is any timing logic found, surface it. It is FINE for potentially hanging tests to become unblocked with ERROR after some amount of time -- we are only concerned with not attempting to ballpark synchronization via time. We should be careful to NOT reduce the production code path coverage of our refactors -- for example, we should avoid removing asserts in the effort to 'only test one thing', and instead prefer adding a new subtest. Please spin up ONE AGENT PER TEST FILE!")

(defconst agents-repl--test-coverage-prompt
  "Please be sure to confine your analysis to the specified context (branch, HEAD, uncommitted changes, etc). <<IF AND ONLY IF YOU JUST PRODUCED A LIST OF EDGE CASES>>: write up a plan for producing a unit test that covers each and every one of the edge cases you just enumerated. Each test should cover *precisely* one edge case. Each test file should be worked on by a separate agent. <<IF AND ONLY IF YOU DID NOT -- I REPEAT, NOT -- JUST PRODUCE A LIST OF EDGE CASES IN YOUR LAST RESPONSE MESSAGE>>: please enumerate each and every edge cases introduced or modified by each and every function added or modified.")

(defun agents-repl--send-diff-analysis (change-spec prompt)
  "Send a diff analysis request to Claude.
CHANGE-SPEC describes which changes (e.g. \"unstaged changes (git diff)\").
PROMPT is the analysis instruction."
  (let ((msg (format "for the %s, %s" change-spec prompt)))
    (agents-repl--log "diff-analysis: %s" change-spec)
    (agents-repl--send-to-backend msg)))

(defcustom agents-repl-branch-diff-spec
  "changes in current branch (git diff $(git merge-base HEAD origin/master))"
  "Change-spec string used for branch-level quality and coverage analysis commands."
  :type 'string
  :group 'agents-repl)

(defun agents-repl-test-quality-worktree ()
  "Analyze test quality for unstaged changes."
  (interactive)
  (agents-repl--send-diff-analysis "unstaged changes (git diff)" agents-repl--test-quality-prompt))

(defun agents-repl-test-quality-staged ()
  "Analyze test quality for staged changes."
  (interactive)
  (agents-repl--send-diff-analysis "staged changes (git diff --cached)" agents-repl--test-quality-prompt))

(defun agents-repl-test-quality-uncommitted ()
  "Analyze test quality for all uncommitted changes."
  (interactive)
  (agents-repl--send-diff-analysis "uncommitted changes (git diff HEAD)" agents-repl--test-quality-prompt))

(defun agents-repl-test-quality-head ()
  "Analyze test quality for the last commit."
  (interactive)
  (agents-repl--send-diff-analysis "last commit (git show HEAD)" agents-repl--test-quality-prompt))

(defun agents-repl-test-quality-branch ()
  "Analyze test quality for all changes in the current branch."
  (interactive)
  (agents-repl--send-diff-analysis agents-repl-branch-diff-spec agents-repl--test-quality-prompt))

(defun agents-repl-test-coverage-worktree ()
  "Analyze test coverage for unstaged changes."
  (interactive)
  (agents-repl--send-diff-analysis "unstaged changes (git diff)" agents-repl--test-coverage-prompt))

(defun agents-repl-test-coverage-staged ()
  "Analyze test coverage for staged changes."
  (interactive)
  (agents-repl--send-diff-analysis "staged changes (git diff --cached)" agents-repl--test-coverage-prompt))

(defun agents-repl-test-coverage-uncommitted ()
  "Analyze test coverage for all uncommitted changes."
  (interactive)
  (agents-repl--send-diff-analysis "uncommitted changes (git diff HEAD)" agents-repl--test-coverage-prompt))

(defun agents-repl-test-coverage-head ()
  "Analyze test coverage for the last commit."
  (interactive)
  (agents-repl--send-diff-analysis "last commit (git show HEAD)" agents-repl--test-coverage-prompt))

(defun agents-repl-test-coverage-branch ()
  "Analyze test coverage for all changes in the current branch."
  (interactive)
  (agents-repl--send-diff-analysis agents-repl-branch-diff-spec agents-repl--test-coverage-prompt))

;; Hide overlay functions
(defun agents-repl--create-hide-overlay ()
  "Create a new hide overlay covering the bottom 4 lines of the current buffer."
  (when (and agents-repl-hide-input-box (> (point-max) 1))
    (let* ((end (point-max))
           (start (save-excursion
                    (goto-char end)
                    (forward-line -4)
                    (line-beginning-position))))
      (when (< start end)
        (setq agents-repl-hide-overlay (make-overlay start end nil t nil))
        (overlay-put agents-repl-hide-overlay 'display "")
        (overlay-put agents-repl-hide-overlay 'evaporate t)))))

(defun agents-repl--update-hide-overlay ()
  "Update overlay to hide bottom lines of Claude vterm buffer.
When called from within a claude buffer, updates it directly.
Otherwise looks up the vterm buffer for the current workspace."
  (let ((buf (if (agents-repl--claude-buffer-p)
                 (current-buffer)
               (let ((ws (+workspace-current-name)))
                 (and ws (agents-repl--ws-get ws :vterm-buffer))))))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (agents-repl--delete-hide-overlay)
        (agents-repl--create-hide-overlay)))))

(defun agents-repl-toggle-hide-input-box ()
  "Toggle hiding of Claude CLI's input box in the vterm buffer."
  (interactive)
  (agents-repl--log "toggle-hide-input-box -> %s" (not agents-repl-hide-input-box))
  (setq agents-repl-hide-input-box (not agents-repl-hide-input-box))
  (agents-repl--update-hide-overlay)
  (message "Claude input box hiding %s" (if agents-repl-hide-input-box "enabled" "disabled")))

;; Notifications
(defun agents-repl--notify-terminal-notifier (title message)
  "Send a desktop notification via terminal-notifier."
  (call-process "terminal-notifier" nil 0 nil
                "-title" title
                "-message" message))

(defun agents-repl--notify-osascript (title message)
  "Send a desktop notification via osascript."
  (start-process "agents-notify" nil
                 "osascript" "-e"
                 (format "display notification %S with title %S sound name \"default\""
                         message title)))

(defvar agents-repl--notify-fn
  (if (executable-find "terminal-notifier")
      #'agents-repl--notify-terminal-notifier
    #'agents-repl--notify-osascript)
  "Function used to send desktop notifications.")

(defun agents-repl--notify (title message)
  "Send a desktop notification with TITLE and MESSAGE."
  (agents-repl--log "notify title=%s msg=%s" title message)
  (funcall agents-repl--notify-fn title message))

;; Workspace tab indicator for Claude status

(defcustom agents-repl-stale-minutes 60
  "Minutes after last input before a workspace tab stops showing as stale."
  :type 'integer
  :group 'agents-repl)

(defun agents-repl--ws-state (ws)
  "Return the current status keyword for workspace WS.
Returns one of: :thinking, :done, :permission, :stale, or nil."
  (let ((plist (gethash ws agents-repl--workspaces)))
    (or (plist-get plist :status)
        (when-let ((t0 (plist-get plist :activity-time)))
          (when (< (- (float-time) t0) (* agents-repl-stale-minutes 60))
            :stale)))))

(defun agents-repl--ws-set (ws state)
  "Set workspace WS to STATE.
STATE is one of: :thinking, :done, :permission."
  (unless ws (error "agents-repl--ws-set: ws is nil"))
  (agents-repl--log "state %s -> %s" ws state)
  (agents-repl--ws-put ws :status state)
  (force-mode-line-update t))

(defun agents-repl--ws-clear (ws state)
  "Clear a single STATE for workspace WS.
STATE is one of: :thinking, :done, :permission, :stale."
  (unless ws (error "agents-repl--ws-clear: ws is nil"))
  (agents-repl--log "clear %s %s" ws state)
  (pcase state
    ((or :thinking :done :permission)
     (when (eq (agents-repl--ws-get ws :status) state)
       (agents-repl--ws-put ws :status nil)))
    (:stale (agents-repl--ws-put ws :activity-time nil)))
  (force-mode-line-update t))

(defun agents-repl--ws-dir (ws)
  "Return a directory associated with workspace WS, used for git checks.
Returns the `default-directory' of the first live buffer in WS, or nil."
  (ignore-errors
    (let* ((persp (persp-get-by-name ws))
           (bufs (and persp (not (symbolp persp)) (persp-buffers persp))))
      (cl-loop for buf in bufs
               when (buffer-live-p buf)
               return (buffer-local-value 'default-directory buf)))))

(defun agents-repl--workspace-clean-p (ws)
  "Return non-nil if workspace WS has no unstaged changes to tracked files.
Reads from a cached value updated asynchronously by
`agents-repl--async-refresh-git-status'.  Defaults to non-nil (clean) when
the cache has not yet been populated."
  (not (eq (agents-repl--ws-get ws :git-clean) 'dirty)))

(defun agents-repl--async-refresh-git-status (ws)
  "Asynchronously refresh the git cleanliness cache for workspace WS.
Starts `git diff --quiet' in WS's directory.  On exit, sets `:git-clean'
to `clean' or `dirty' in the workspace plist and calls
`agents-repl--update-ws-state' to apply any resulting state transition.
A no-op if a check is already in progress for WS."
  (when-let ((dir (agents-repl--ws-dir ws)))
    (unless (let ((proc (agents-repl--ws-get ws :git-proc)))
              (and proc (process-live-p proc)))
      (let* ((default-directory dir)
             (proc (make-process
                    :name (format "agents-repl-git-%s" ws)
                    :command '("git" "diff" "--quiet")
                    :connection-type 'pipe
                    :noquery t
                    :buffer nil
                    :sentinel
                    (lambda (proc _event)
                      (unless (process-live-p proc)
                        (agents-repl--ws-put ws :git-clean
                                            (if (= 0 (process-exit-status proc))
                                                'clean
                                              'dirty))
                        (agents-repl--ws-put ws :git-proc nil)
                        (agents-repl--update-ws-state ws)
                        (force-mode-line-update t))))))
        (agents-repl--ws-put ws :git-proc proc)))))

(defun agents-repl--touch-activity (ws)
  "Record current time as last input for workspace WS.
Only sets stale if the workspace has no unstaged changes to tracked files."
  (unless ws (error "agents-repl--touch-activity: ws is nil"))
  (if (agents-repl--workspace-clean-p ws)
      (agents-repl--ws-put ws :activity-time (float-time))
    (agents-repl--ws-put ws :activity-time nil)))

(defun agents-repl--workspace-for-buffer (buf)
  "Return the workspace name that contains BUF, or nil."
  (when (bound-and-true-p persp-mode)
    (cl-loop for persp in (persp-persps)
             when (persp-contain-buffer-p buf persp)
             return (safe-persp-name persp))))

(defface agents-repl-tab-thinking
  '((t :background "#cc3333" :foreground "white" :weight bold))
  "Face for workspace tabs where Claude is thinking (red).")

(defface agents-repl-tab-done
  '((t :background "#1a7a1a" :foreground "black" :weight bold))
  "Face for workspace tabs where Claude is done (green).")

(defface agents-repl-tab-permission
  '((t :background "#1a7a1a" :foreground "black" :weight bold))
  "Face for workspace tabs where Claude needs permission (green + emoji).")

(defface agents-repl-tab-stale
  '((t :background "#cc8800" :foreground "black" :weight bold))
  "Face for workspace tabs you've worked in recently but aren't viewing (orange).")

(defun agents-repl--tabline-advice (&optional names)
  "Override for `+workspace--tabline' to color tabs by Claude status."
  (let* ((names (or names (+workspace-list-names)))
         (current-name (+workspace-current-name)))
    (mapconcat
     #'identity
     (cl-loop for name in names
              for i to (length names)
              collect
              (let* ((num (number-to-string (1+ i)))
                     (selected (equal current-name name))
                     (state (agents-repl--ws-state name))
                     (backend-face (pcase state
                                   ;; Permission: always visible (even when selected), green + ❓
                                   (:permission 'agents-repl-tab-permission)
                                   ;; Remaining states only on background tabs
                                   ((guard selected) nil)
                                   (:done       'agents-repl-tab-done)
                                   (:thinking   'agents-repl-tab-thinking)
                                   (:stale      'agents-repl-tab-stale)))
                     (label (pcase state
                              (:permission "❓")
                              (_ num)))
                     (base-face (if (and selected (not backend-face))
                                    '+workspace-tab-selected-face
                                  '+workspace-tab-face))
                     (face (or backend-face base-face)))
                (propertize (format " [%s] %s " label name) 'face face)))
     " ")))

(advice-add '+workspace--tabline :override #'agents-repl--tabline-advice)

;; Suppress the echo area flash when switching workspaces.
;; Doom calls (+workspace/display) after switch/cycle/new/load, which uses
;; (message ...) to show the tabline in the echo area.  Since tabs are
;; already visible at the top, the bottom flash is redundant.
(advice-add '+workspace/display :override #'ignore)

;; Periodically redraw invisible claude vterm buffers in :thinking workspaces.
;; This catches missed title transitions (e.g., Claude finished while we were
;; on a different workspace and vterm never redrawed).
(defun agents-repl--poll-thinking-workspaces ()
  "Redraw invisible claude vterm buffers whose workspace is :thinking."
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (agents-repl--claude-buffer-p buf)
               (not (get-buffer-window buf))  ;; invisible
               (with-current-buffer buf (eq major-mode 'vterm-mode)))
      (let ((ws (or (buffer-local-value 'agents-repl--owning-workspace buf)
                     (agents-repl--workspace-for-buffer buf))))
        (when (and ws (eq (agents-repl--ws-state ws) :thinking))
          (agents-repl--log "poll-thinking: redrawing %s (ws=%s)" (buffer-name buf) ws)
          (with-current-buffer buf
            (agents-repl--do-refresh)))))))

(push (run-with-timer 5 5 #'agents-repl--poll-thinking-workspaces)
      agents-repl--timers)

;; Periodically update all workspace states (catches git changes, etc.)
(push (run-with-timer 1 1 #'agents-repl--update-all-workspace-states)
      agents-repl--timers)

;; Title-based "Claude is done" detection.
;; Claude Code sets the terminal title to "<spinner> Claude Code" while thinking
;; and plain "Claude Code" when idle.  We poll via vterm--set-title advice.
(defvar-local agents-repl--title-thinking nil
  "Non-nil when the vterm title indicates Claude is thinking.
Buffer-local so multiple Claude sessions don't interfere.")

(defun agents-repl--title-has-spinner-p (title)
  "Return non-nil if TITLE contains a spinner for the current buffer's backend."
  (let* ((ws (or agents-repl--owning-workspace
                 (agents-repl--workspace-for-buffer (current-buffer))))
         (pred (agents-repl--backend-get :spinner-p ws)))
    (and pred (funcall pred title))))

(defun agents-repl--detect-title-transition (title)
  "Classify a vterm TITLE change into a transition type.
Returns a plist with:
  :thinking    - non-nil if the new title has a spinner
  :transition  - one of 'started, 'finished, or nil (no change)
  :ws          - the owning workspace name for the current buffer, or nil"
  (let* ((thinking (agents-repl--title-has-spinner-p title))
         (ws (or agents-repl--owning-workspace
                 (agents-repl--workspace-for-buffer (current-buffer))))
         (transition (cond
                      ((and thinking (not agents-repl--title-thinking)) 'started)
                      ((and (not thinking) agents-repl--title-thinking) 'finished)
                      (t nil))))
    (list :thinking thinking :transition transition :ws ws)))

(defun agents-repl--swap-placeholder ()
  "Replace the loading placeholder window with the real vterm buffer.
Called once when Claude sets its first terminal title (meaning it's ready)."
  (agents-repl--log "swap-placeholder buf=%s" (buffer-name))
  (let ((buf (current-buffer))
        (placeholder (get-buffer " *agents-loading*")))
    (when placeholder
      (run-at-time 0 nil
                   (lambda ()
                     (when (buffer-live-p buf)
                       (dolist (win (get-buffer-window-list placeholder nil t))
                         (set-window-dedicated-p win nil)
                         (set-window-buffer win buf)
                         (set-window-dedicated-p win t))
                       (kill-buffer placeholder)))))))

(defun agents-repl--maybe-notify-finished (ws)
  "Send a desktop notification that Claude finished in WS, if frame is unfocused."
  (agents-repl--log "maybe-notify-finished ws=%s focused=%s" ws (if (frame-focus-state) "yes" "no"))
  (unless (frame-focus-state)
    (run-at-time 0.1 nil #'agents-repl--notify "Claude REPL"
                 (format "%s: Claude ready" ws))))

(defun agents-repl--on-backend-finished (ws)
  "Handle Claude finishing work in workspace WS.
Sets done state if buffer is hidden, refreshes display."
  (agents-repl--log "on-claude-finished ws=%s visible=%s focused=%s"
                    ws (if (get-buffer-window (current-buffer) t) "yes" "no")
                    (if (frame-focus-state) "yes" "no"))
  (unless (get-buffer-window (current-buffer) t)
    (agents-repl--ws-set ws :done))
  (agents-repl--refresh-vterm)
  (agents-repl--update-hide-overlay)
  (agents-repl--maybe-notify-finished ws)
  (unless (string= ws (+workspace-current-name))
    (message "Claude finished in workspace: %s" ws)
    (run-at-time 0.1 nil #'agents-repl--notify "Claude REPL"
                 (format "%s: Claude ready" ws))))

(defun agents-repl--handle-first-ready ()
  "Handle the first terminal title set — Claude is now ready.
Swaps the loading placeholder for the real vterm buffer."
  (unless agents-repl--ready
    (agents-repl--log "first-ready buf=%s" (buffer-name))
    (setq agents-repl--ready t)
    (agents-repl--swap-placeholder)))

(defun agents-repl--on-title-change (title)
  "Detect thinking->idle transition from vterm title changes."
  (when (agents-repl--claude-buffer-p)
    (agents-repl--handle-first-ready)
    (let* ((info (agents-repl--detect-title-transition title))
           (thinking (plist-get info :thinking))
           (transition (plist-get info :transition))
           (ws (plist-get info :ws)))
      ;; Update buffer-local thinking state before side effects so
      ;; re-entrant vterm--redraw calls see the current value and
      ;; detect no transition (prevents infinite recursion).
      (setq agents-repl--title-thinking thinking)
      (when transition
        (if (not ws)
            (agents-repl--log "on-title-change: ws nil for buffer %s, transition %s skipped"
                              (buffer-name) transition)
          (agents-repl--log "title transition=%s ws=%s" transition ws)
          (pcase transition
            ('started  (agents-repl--ws-set ws :thinking))
            ('finished (agents-repl--ws-clear ws :thinking)))
          (when (eq transition 'finished)
            (agents-repl--on-backend-finished ws))
          (agents-repl--update-all-workspace-states))))))


(after! vterm
  (advice-add 'vterm--set-title :before #'agents-repl--on-title-change))

;; Permission prompt detection via file-notify watcher.
;; A Claude Code Notification hook writes the CWD to a sentinel file;
;; we watch for it and set the permission state for the matching workspace.
(defun agents-repl--ws-for-dir (dir)
  "Return the workspace name for an agent session rooted at DIR, or nil.
Tries all known backend buffer name prefixes."
  (when-let* ((root (agents-repl--git-root dir))
              (hash (substring (md5 (agents-repl--path-canonical root)) 0 8))
              (buf (cl-some (lambda (backend)
                              (let ((prefix (plist-get (cdr backend) :buffer-prefix)))
                                (get-buffer (format "*%s-%s*" prefix hash))))
                            agents-repl--backends)))
    (agents-repl--workspace-for-buffer buf)))

(defun agents-repl--on-permission-notify (event)
  "Handle file-notify event for permission prompt sentinel file."
  (let ((action (nth 1 event))
        (file (nth 2 event)))
    (when (and (memq action '(created changed))
               (string-match-p "permission_prompt$" file))
      (let ((ws (agents-repl--ws-for-dir
                 (string-trim (with-temp-buffer
                                (insert-file-contents file)
                                (buffer-string))))))
        (agents-repl--log "permission notify ws=%s" ws)
        (unless ws
          (error "agents-repl--on-permission-notify: no workspace for dir in %s" file))
        (agents-repl--ws-set ws :permission)
        (delete-file file)))))

(require 'filenotify)
;; Permission sentinel file watching is Claude-only.
;; Codex has no equivalent file-based notification mechanism.
(when (plist-get (alist-get 'claude agents-repl--backends) :permission-watch)
  (let ((dir (expand-file-name "~/.claude/workspace-notifications")))
    (make-directory dir t)
    (file-notify-add-watch dir '(change) #'agents-repl--on-permission-notify)))

(defun agents-repl--do-refresh ()
  "Low-level refresh of the current vterm buffer.
Must be called with a vterm-mode buffer current."
  (let ((inhibit-read-only t))
    (when vterm--term
      (vterm--redraw vterm--term)))
  (redisplay t))

(defun agents-repl--fix-vterm-scroll (buf)
  "Briefly select the vterm window for BUF to fix Emacs scroll position."
  (let ((vterm-win (get-buffer-window buf))
        (orig-win (selected-window)))
    (when (and vterm-win (not (eq vterm-win orig-win)))
      (select-window vterm-win 'norecord)
      (select-window orig-win 'norecord))))

(defun agents-repl--refresh-vterm ()
  "Refresh the claude vterm display.
Works from any buffer or from within the vterm buffer itself."
  (let ((buf (if (eq major-mode 'vterm-mode)
                 (current-buffer)
               (let ((ws (+workspace-current-name)))
                 (and ws (agents-repl--ws-get ws :vterm-buffer))))))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (when (eq major-mode 'vterm-mode)
          (agents-repl--do-refresh)))
      (agents-repl--fix-vterm-scroll buf))))

;; Walk saved window-configuration tree to find agent vterm buffers.
(defun agents-repl--wconf-has-claude-p (wconf)
  "Return non-nil if WCONF (a `window-state-get' tree) contains an agents-repl vterm buffer."
  (when (and wconf (proper-list-p wconf))
    (let ((buf-entry (alist-get 'buffer wconf)))
      (if (and buf-entry (stringp (car-safe buf-entry))
               (string-match-p "^\\*\\(claude\\|codex\\)-[0-9a-f]+\\*$" (car buf-entry)))
          t
        (cl-some #'agents-repl--wconf-has-claude-p
                 (cl-remove-if-not #'proper-list-p wconf))))))

(defun agents-repl--ws-claude-open-p (ws-name)
  "Return non-nil if workspace WS-NAME has a claude buffer in its window layout.
For the current workspace, checks live windows.
For background workspaces, inspects the saved persp window configuration."
  (if (equal ws-name (+workspace-current-name))
      ;; Current workspace: check live windows
      (cl-some (lambda (buf)
                 (and (buffer-live-p buf)
                      (agents-repl--claude-buffer-p buf)
                      (get-buffer-window buf)))
               (buffer-list))
    ;; Background workspace: inspect saved window config
    (let* ((persp (persp-get-by-name ws-name))
           (wconf (and persp (not (symbolp persp)) (persp-window-conf persp))))
      (agents-repl--wconf-has-claude-p wconf))))

(defun agents-repl--update-ws-state (ws)
  "Update workspace WS state according to claude visibility and git status.
State table:
  :thinking  → unchanged (never touch)
  :done      + clean → :stale  |  :done      + dirty → :done
  :permission         → unchanged
  :stale     + dirty → :done   |  :stale     + clean → :stale
  nil        + dirty → :done   |  nil        + clean → nil"
  (let ((state (agents-repl--ws-state ws))
        (dirty (not (agents-repl--workspace-clean-p ws))))
    (pcase (cons state dirty)
      ;; :done + clean → :stale
      (`(:done . nil)
       (agents-repl--log "update-ws-state ws=%s :done->:stale" ws)
       (agents-repl--ws-clear ws :done)
       (agents-repl--touch-activity ws))
      ;; :stale + dirty → :done
      (`(:stale . t)
       (agents-repl--log "update-ws-state ws=%s :stale->:done" ws)
       (agents-repl--ws-clear ws :stale)
       (agents-repl--ws-set ws :done))
      ;; nil + dirty → :done
      (`(nil . t)
       (agents-repl--log "update-ws-state ws=%s nil->:done" ws)
       (agents-repl--ws-set ws :done))
      ;; :thinking, :permission, :done+dirty, :stale+clean, nil+clean → no-op
      (_ nil))))

(defun agents-repl--update-all-workspace-states ()
  "Update state for all workspaces based on claude visibility and git status.
Uses cached git status (`:git-clean') and kicks off async refreshes."
  (when (bound-and-true-p persp-mode)
    (dolist (ws (+workspace-list-names))
      (if (agents-repl--ws-claude-open-p ws)
          (progn
            (agents-repl--update-ws-state ws)
            (agents-repl--async-refresh-git-status ws))
        ;; Claude not open on this workspace → clear all state
        (let ((state (agents-repl--ws-state ws)))
          (when (and state (not (eq state :thinking)))
            (agents-repl--log "update-all: ws=%s clearing %s (claude not open)" ws state)
            (agents-repl--ws-clear ws state)))))))

(defun agents-repl--on-frame-focus ()
  "Refresh claude vterm and update all workspace states when Emacs regains focus."
  (when (frame-focus-state)
    (agents-repl--log "on-frame-focus")
    (agents-repl--refresh-vterm)
    (agents-repl--update-all-workspace-states)))

(add-function :after after-focus-change-function #'agents-repl--on-frame-focus)

;; Refresh vterm on workspace switch
(defun agents-repl--on-workspace-switch ()
  "Handle workspace switch: update all workspace states, refresh vterm, reset cursors."
  (agents-repl--log "workspace-switch ws=%s" (+workspace-current-name))
  (agents-repl--update-all-workspace-states)
  (agents-repl--refresh-vterm)
  (agents-repl--reset-vterm-cursors))

;; Save window state for current workspace before switching away,
;; so update-all-workspace-states can inspect the saved config.
(when (modulep! :ui workspaces)
  (add-hook 'persp-before-deactivate-functions
            (lambda (&rest _)
              (ignore-errors (persp-frame-save-state))))
  (add-hook 'persp-activated-functions
            (lambda (&rest _)
              (run-at-time 0 nil #'agents-repl--on-workspace-switch))))

(defvar agents-repl--in-redraw-advice nil
  "Non-nil while the vterm redraw advice is executing, to prevent recursion.")

(defun agents-repl--after-vterm-redraw (&rest _)
  "Apply hide overlay after vterm redraws."
  (unless agents-repl--in-redraw-advice
    (let ((agents-repl--in-redraw-advice t))
      (when (agents-repl--claude-buffer-p)
        (agents-repl--update-hide-overlay)))))

(defun agents-repl--enable-hide-overlay ()
  "Enable the hide overlay advice.  Uses reference counting so
multiple sessions don't clobber each other."
  (agents-repl--log "enable-hide-overlay refcount=%d" agents-repl--hide-overlay-refcount)
  (when (zerop agents-repl--hide-overlay-refcount)
    (advice-add 'vterm--redraw :after #'agents-repl--after-vterm-redraw))
  (cl-incf agents-repl--hide-overlay-refcount))

(defun agents-repl--delete-hide-overlay ()
  "Delete the hide overlay if it exists and nil the variable."
  (when (and agents-repl-hide-overlay
             (overlay-buffer agents-repl-hide-overlay))
    (delete-overlay agents-repl-hide-overlay))
  (setq agents-repl-hide-overlay nil))

(defun agents-repl--disable-hide-overlay ()
  "Disable the hide overlay advice and clean up any existing overlay.
Uses reference counting so the advice is only removed when the last
session releases it."
  (cl-decf agents-repl--hide-overlay-refcount)
  (when (< agents-repl--hide-overlay-refcount 0)
    (setq agents-repl--hide-overlay-refcount 0))
  (agents-repl--log "disable-hide-overlay refcount=%d" agents-repl--hide-overlay-refcount)
  (when (zerop agents-repl--hide-overlay-refcount)
    (advice-remove 'vterm--redraw #'agents-repl--after-vterm-redraw))
  (agents-repl--delete-hide-overlay))

(defun agents-repl--vterm-running-p ()
  "Return t if Claude vterm buffer for the current workspace exists with a live process."
  (let ((buf (agents-repl--ws-get (+workspace-current-name) :vterm-buffer)))
    (and buf (buffer-live-p buf) (get-buffer-process buf))))

(defun agents-repl--input-visible-p ()
  "Return t if input buffer for the current workspace is visible in a window."
  (let ((buf (agents-repl--ws-get (+workspace-current-name) :input-buffer)))
    (and buf (buffer-live-p buf) (get-buffer-window buf))))

(defun agents-repl--vterm-visible-p ()
  "Return t if vterm buffer for the current workspace is visible in a window."
  (let ((buf (agents-repl--ws-get (+workspace-current-name) :vterm-buffer)))
    (and buf (buffer-live-p buf) (get-buffer-window buf))))

(defun agents-repl--panels-visible-p ()
  "Return t if both panels are visible."
  (and (agents-repl--input-visible-p)
       (agents-repl--vterm-visible-p)))

(defun agents-repl--hide-panels ()
  "Hide both Claude panels without killing buffers."
  (agents-repl--log "hide-panels")
  (let* ((ws (+workspace-current-name))
         (input-buf (agents-repl--ws-get ws :input-buffer))
         (vterm-buf (agents-repl--ws-get ws :vterm-buffer)))
    (when-let ((win (and input-buf (get-buffer-window input-buf))))
      (delete-window win))
    (when-let ((win (and vterm-buf (get-buffer-window vterm-buf))))
      (delete-window win))))

;; Auto-close orphaned panels: if one is closed, close the other.
;; Also refresh the hide overlay in case a window change invalidated it.
(defun agents-repl--orphaned-vterm-p (name)
  "Return non-nil if NAME is an agents-repl vterm buffer with no matching input window."
  (and (string-match "^\\*\\(claude\\|codex\\)-\\([0-9a-f]+\\)\\*$" name)
       (not agents-repl--fullscreen-config)
       (not (one-window-p))
       (not (get-buffer-window
             (format "*%s-input-%s*" (match-string 1 name) (match-string 2 name))))))

(defun agents-repl--orphaned-input-p (name)
  "Return non-nil if NAME is an agents-repl input buffer with no matching vterm window."
  (and (string-match "^\\*\\(claude\\|codex\\)-input-\\([0-9a-f]+\\)\\*$" name)
       (not (one-window-p))
       (not (get-buffer-window
             (format "*%s-%s*" (match-string 1 name) (match-string 2 name))))
       (not (get-buffer " *agents-loading*"))))

(defun agents-repl--sync-panels ()
  "Close any Claude panel whose partner is no longer visible."
  (dolist (win (window-list))
    (let ((name (buffer-name (window-buffer win))))
      (when (or (agents-repl--orphaned-vterm-p name)
                (agents-repl--orphaned-input-p name))
        (agents-repl--log "sync-panels closing orphaned %s" name)
        (delete-window win)))))

;; Keep visible Claude vterm buffers scrolled to the cursor.
;; Skips the selected window so clicking into vterm to read/copy isn't disrupted.
(defun agents-repl--refresh-vterm-window (win)
  "Refresh the Claude vterm buffer shown in WIN.
Resets cursor, redraws, and syncs window point."
  (let ((buf (window-buffer win)))
    (when (and buf (buffer-live-p buf) (agents-repl--claude-buffer-p buf))
      (with-current-buffer buf
        (when (and (eq major-mode 'vterm-mode)
                   (fboundp 'vterm-reset-cursor-point))
          (let ((inhibit-read-only t))
            (vterm-reset-cursor-point)
            (when vterm--term
              (vterm--redraw vterm--term))
            (vterm-reset-cursor-point)
            (set-window-point win (point))))))))

(defun agents-repl--reset-vterm-cursors ()
  "Refresh every visible Claude vterm window except the selected one."
  (let ((sel (selected-window)))
    (dolist (win (window-list))
      (unless (eq win sel)
        (agents-repl--refresh-vterm-window win)))))

(defvar agents-repl--sync-timer nil
  "Timer for debounced window-change handler.")

(defvar agents-repl--cursor-reset-timer nil
  "Timer for debounced cursor reset.")

(defun agents-repl--on-window-change ()
  "Deferred handler for window configuration changes.
Syncs orphaned panels, refreshes overlay, and resets cursors."
  (agents-repl--log "on-window-change")
  (agents-repl--sync-panels)
  (agents-repl--update-hide-overlay)
  (agents-repl--reset-vterm-cursors))

(defmacro agents-repl--deferred (timer-var fn)
  "Return a lambda that debounces calls to FN via TIMER-VAR."
  `(lambda (&rest _)
     (when ,timer-var
       (cancel-timer ,timer-var))
     (setq ,timer-var (run-at-time 0 nil ,fn))))

(add-hook 'window-configuration-change-hook
          (agents-repl--deferred agents-repl--sync-timer #'agents-repl--on-window-change))

(let ((debounced (agents-repl--deferred agents-repl--cursor-reset-timer #'agents-repl--reset-vterm-cursors)))
  (add-hook 'window-selection-change-functions debounced)
  (add-hook 'buffer-list-update-hook debounced))

;; Redirect focus from vterm output to input buffer (keyboard only, not mouse)
(defun agents-repl--redirect-to-input (_frame)
  "If the selected window shows a Claude vterm buffer, jump to its input window.
Only redirects for keyboard navigation; mouse clicks are allowed through
so the user can select and copy text from the output."
  (unless (mouse-event-p last-input-event)
    (let ((name (buffer-name (window-buffer (selected-window)))))
      (when (string-match "^\\*\\(claude\\|codex\\)-\\([0-9a-f]+\\)\\*$" name)
        (let* ((prefix (match-string 1 name))
               (id (match-string 2 name))
               (input-buf (get-buffer (format "*%s-input-%s*" prefix id))))
          (when-let ((input-win (and input-buf (get-buffer-window input-buf))))
            (agents-repl--log "redirect-to-input from %s" name)
            (select-window input-win)
            (when (bound-and-true-p evil-mode)
              (evil-insert-state))))))))

(add-hook 'window-selection-change-functions #'agents-repl--redirect-to-input)

(defun agents-repl--ensure-input-buffer (ws)
  "Create input buffer for workspace WS if needed, put in agents-input-mode."
  (agents-repl--log "ensure-input-buffer")
  (let* ((root (agents-repl--resolve-root))
         (input-buf (get-buffer-create (agents-repl--buffer-name "-input"))))
    (agents-repl--ws-put ws :input-buffer input-buf)
    (with-current-buffer input-buf
      (setq-local agents-repl--project-root root)
      (unless (eq major-mode 'agents-input-mode)
        (agents-input-mode)
        (agents-repl--history-restore)))))

(defun agents-repl--kill-stale-vterm ()
  "Kill the Claude vterm buffer if it exists but has no live process."
  (when-let ((existing (get-buffer (agents-repl--buffer-name))))
    (unless (get-buffer-process existing)
      (agents-repl--log "killing stale vterm %s" (buffer-name existing))
      (kill-buffer existing))))

(defun agents-repl--ensure-vterm-buffer (ws)
  "Create vterm buffer for workspace WS if needed. Starts the backend from the git root.
Stamps the workspace's :backend into the plist on first creation."
  (let* ((root (agents-repl--resolve-root))
         (default-directory root))
    (agents-repl--log "ensure-vterm-buffer ws=%s root=%s default-directory=%s" ws root default-directory)
    ;; Stamp backend now so --buffer-name uses the right prefix and all
    ;; backend-get calls during startup resolve correctly.
    (unless (agents-repl--ws-get ws :backend)
      (agents-repl--ws-put ws :backend agents-repl-default-backend))
    (agents-repl--kill-stale-vterm)
    (let ((vterm-buf (get-buffer-create (agents-repl--buffer-name nil ws))))
      (agents-repl--ws-put ws :vterm-buffer vterm-buf)
      (with-current-buffer vterm-buf
        (setq-local agents-repl--project-root root)
        (setq-local agents-repl--owning-workspace ws)
        (if (eq major-mode 'vterm-mode)
            (agents-repl--log "ensure-vterm reusing existing buffer %s" (buffer-name vterm-buf))
          (vterm-mode)
          (setq-local truncate-lines nil)
          (setq-local word-wrap t)
          (agents-repl--set-buffer-background 15)
          (agents-repl--log "ensure-vterm created %s root=%s backend=%s"
                            (buffer-name vterm-buf) root (agents-repl--ws-backend ws))
          (agents-repl--start-backend ws))))))

(defun agents-repl--show-panels-with-placeholder ()
  "Show panels using a loading placeholder in the vterm slot.
The placeholder is swapped for the real vterm buffer once Claude is ready."
  (agents-repl--log "show-panels-with-placeholder")
  (let* ((ws (+workspace-current-name))
         (real-vterm (agents-repl--ws-get ws :vterm-buffer))
         (placeholder (get-buffer-create " *agents-loading*")))
    (with-current-buffer placeholder
      (setq-local mode-line-format nil)
      (agents-repl--set-buffer-background 15))
    (agents-repl--ws-put ws :vterm-buffer placeholder)
    (agents-repl--show-panels)
    (agents-repl--focus-input-panel)
    (agents-repl--ws-put ws :vterm-buffer real-vterm)))

(defun agents-repl--start-fresh ()
  "Start a new Claude session with placeholder panels."
  (agents-repl--log "start-fresh")
  (let ((ws (+workspace-current-name)))
    (unless ws (error "agents-repl--start-fresh: no active workspace"))
    (agents-repl--touch-activity ws)
    (agents-repl--ws-put ws :saved-window-config (current-window-configuration)))
  (delete-other-windows (agents-repl--live-return-window))
  (agents-repl--ensure-session)
  (agents-repl--show-panels-with-placeholder)
  (message "Starting Claude... ws=%s ws-id=%s dir=%s"
           (+workspace-current-name)
           (agents-repl--workspace-id)
           (agents-repl--resolve-root)))

(defun agents-repl--show-existing-panels ()
  "Show panels for an already-running Claude session.
Demotes indicators, refreshes display, and restores panel layout."
  (agents-repl--log "show-existing-panels")
  (let ((ws (+workspace-current-name)))
    (unless ws (error "agents-repl--show-existing-panels: no active workspace"))
    (agents-repl--touch-activity ws)
    (agents-repl--refresh-vterm)
    (agents-repl--ws-put ws :saved-window-config (current-window-configuration))
    (delete-other-windows (agents-repl--live-return-window))
    (agents-repl--ensure-input-buffer ws)
    (agents-repl--show-panels)
    (agents-repl--focus-input-panel)
    (agents-repl--update-hide-overlay)))

;; Entry point - smart toggle
(defun agents-repl ()
  "Toggle Claude REPL panels.
If text is selected: send it directly to Claude.
If not running: start Claude and show both panels.
If panels visible: hide both panels.
If panels hidden: show both panels."
  (interactive)
  (agents-repl--remember-return-window)
  (let ((vterm-running (agents-repl--vterm-running-p))
        (panels-visible (agents-repl--panels-visible-p))
        (selection (when (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end)))))
    (agents-repl--log "agents-repl running=%s visible=%s selection=%s"
                      vterm-running panels-visible (if selection "yes" "no"))
    (cond
     ;; Text selected - send directly to Claude
     (selection
      (deactivate-mark)
      (agents-repl--send-to-backend selection))
     ;; Nothing running - start fresh with placeholder until Claude is ready
     ((not vterm-running)
      (agents-repl--start-fresh))
     ;; Panels visible - hide both, restore window layout
     (panels-visible
      (let ((ws (+workspace-current-name)))
        (unless ws (error "agents-repl: no active workspace when hiding panels"))
        (agents-repl--ws-clear ws :thinking)
        (agents-repl--ws-clear ws :done)
        (agents-repl--ws-clear ws :permission)
        (agents-repl--ws-clear ws :stale))
      (agents-repl--restore-layout))
     ;; Panels hidden - show both
     (t
      (agents-repl--show-existing-panels)))))

(defun agents-repl--close-buffer-windows (&rest bufs)
  "Close windows displaying any of BUFS."
  (agents-repl--log "close-buffer-windows %s" (mapcar (lambda (b) (and b (buffer-name b))) bufs))
  (dolist (buf bufs)
    (when (and buf (buffer-live-p buf))
      (when-let ((win (get-buffer-window buf)))
        (ignore-errors (delete-window win))))))

(defun agents-repl--kill-placeholder ()
  "Close and kill the loading placeholder buffer if it exists."
  (agents-repl--log "kill-placeholder exists=%s" (if (get-buffer " *agents-loading*") "yes" "no"))
  (when-let ((placeholder (get-buffer " *agents-loading*")))
    (when-let ((win (get-buffer-window placeholder)))
      (ignore-errors (delete-window win)))
    (kill-buffer placeholder)))

(defun agents-repl--schedule-sigkill (proc)
  "Schedule a SIGKILL for PROC after 0.5s if it's still alive."
  (run-at-time 0.5 nil
               (lambda ()
                 (when (process-live-p proc)
                   (agents-repl--log "sigkill fallback for lingering process")
                   (signal-process proc 'SIGKILL)))))

(defun agents-repl--kill-vterm-process (buf)
  "Kill the vterm buffer BUF and its process."
  (agents-repl--log "kill-vterm-process buf=%s" (and buf (buffer-name buf)))
  (when (and buf (buffer-live-p buf))
    (let ((proc (get-buffer-process buf)))
      (when proc
        (set-process-query-on-exit-flag proc nil))
      (kill-buffer buf)
      (when proc
        (agents-repl--schedule-sigkill proc)))))

(defun agents-repl--teardown-session-state (ws)
  "Save history, disable overlay, cancel timers, and clear session state for workspace WS."
  (agents-repl--log "teardown-session-state")
  (ignore-errors (agents-repl--history-save ws))
  (ignore-errors (agents-repl--disable-hide-overlay))
  (when agents-repl--sync-timer
    (cancel-timer agents-repl--sync-timer)
    (setq agents-repl--sync-timer nil))
  (agents-repl--ws-put ws :vterm-buffer nil)
  (agents-repl--ws-put ws :input-buffer nil)
  (agents-repl--ws-put ws :saved-window-config nil))

(defun agents-repl--destroy-session-buffers (vterm-buf input-buf)
  "Close windows and kill VTERM-BUF, INPUT-BUF, and any placeholder."
  (agents-repl--log "destroy-session-buffers")
  (agents-repl--close-buffer-windows vterm-buf input-buf)
  (agents-repl--kill-placeholder)
  (agents-repl--kill-vterm-process vterm-buf)
  (when (and input-buf (buffer-live-p input-buf))
    (kill-buffer input-buf)))

(defun agents-repl-kill ()
  "Kill Claude REPL buffers and windows for the current workspace."
  (interactive)
  (agents-repl--log "kill")
  (let* ((ws (+workspace-current-name))
         (vterm-buf (agents-repl--ws-get ws :vterm-buffer))
         (input-buf (agents-repl--ws-get ws :input-buffer))
         (saved-wconf (agents-repl--ws-get ws :saved-window-config))
         (ret-win (agents-repl--ws-get ws :return-window)))
    (unless ws (error "agents-repl-kill: no active workspace"))
    (agents-repl--ws-put ws :status nil)
    (agents-repl--ws-put ws :activity-time nil)
    (force-mode-line-update t)
    (agents-repl--teardown-session-state ws)
    (if saved-wconf
        (progn
          (set-window-configuration saved-wconf)
          (agents-repl--destroy-session-buffers vterm-buf input-buf))
      (agents-repl--destroy-session-buffers vterm-buf input-buf)
      (when (and ret-win (window-live-p ret-win))
        (select-window ret-win)))))

(defun agents-repl-restart ()
  "Kill Claude REPL and restart with `claude -c` to continue session."
  (interactive)
  (agents-repl--log "restart")
  (let ((ws (+workspace-current-name)))
    (agents-repl-kill)
    (agents-repl--ensure-vterm-buffer ws)
    (agents-repl--ensure-input-buffer ws)
    (agents-repl--enable-hide-overlay)
    (agents-repl--show-panels)
    (agents-repl--focus-input-panel)))

(defun agents-repl-focus-input ()
  "Focus the Claude input buffer, or return to previous window if already there.
If Claude isn't running, start it (same as `agents-repl')."
  (interactive)
  (let ((ws (+workspace-current-name)))
    (cond
     ;; Already in the input buffer — jump back
     ((eq (current-buffer) (agents-repl--ws-get ws :input-buffer))
      (agents-repl--log "focus-input branch=jump-back")
      (let ((ret (agents-repl--ws-get ws :return-window)))
        (if (and ret (window-live-p ret))
            (select-window ret)
          (evil-window-left 1))))
     ;; Not running — start fresh
     ((not (agents-repl--vterm-running-p))
      (agents-repl--log "focus-input branch=start-fresh")
      (agents-repl))
     ;; Running but panels hidden — show them
     (t
      (agents-repl--log "focus-input branch=show-or-focus")
      (unless (agents-repl--panels-visible-p)
        (agents-repl--ws-put ws :saved-window-config (current-window-configuration))
        (agents-repl--ensure-input-buffer ws)
        (agents-repl--show-panels))
      (agents-repl--ws-put ws :return-window (selected-window))
      (when-let ((win (get-buffer-window (agents-repl--ws-get ws :input-buffer))))
        (select-window win)
        (when (bound-and-true-p evil-mode)
          (evil-insert-state)))))))

(defun agents-repl-toggle-fullscreen ()
  "Toggle fullscreen for the Claude REPL vterm and input windows.
Saves the current window configuration per-workspace and expands the
Claude panels to fill the frame.  Calling again restores the layout."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (saved (agents-repl--ws-get ws :fullscreen-config)))
    (agents-repl--log "toggle-fullscreen ws=%s currently=%s" ws (if saved "fullscreen" "normal"))
    (cond
     ;; Already fullscreen — restore
     (saved
      (set-window-configuration saved)
      (agents-repl--ws-put ws :fullscreen-config nil))
     ;; Not fullscreen — go fullscreen if panels are visible
     ((agents-repl--vterm-live-p)
      (let* ((vterm-buf (agents-repl--ws-get ws :vterm-buffer))
             (input-buf (agents-repl--ws-get ws :input-buffer)))
        (unless (and vterm-buf input-buf
                     (get-buffer-window vterm-buf)
                     (get-buffer-window input-buf))
          (user-error "Claude REPL panels are not visible"))
        (agents-repl--ws-put ws :fullscreen-config (current-window-configuration))
        (dolist (win (window-list))
          (unless (memq (window-buffer win) (list vterm-buf input-buf))
            (ignore-errors (delete-window win))))))
     (t (message "No Claude vterm buffer for this workspace.")))))

(defun agents-repl-cycle ()
  "Send backtab to Claude vterm to cycle through options."
  (interactive)
  (agents-repl--log "cycle")
  (when (agents-repl--vterm-live-p)
    (with-current-buffer (agents-repl--ws-get (+workspace-current-name) :vterm-buffer)
      (vterm-send-key "<backtab>"))))

;; Global bindings
(map! :nvi "C-S-m" #'agents-repl-cycle)
(map! :i "C-S-f" #'agents-repl-toggle-fullscreen)
(map! :leader :prefix "w" :n "c" #'agents-repl-toggle-fullscreen)

(defun agents-repl-copy-reference ()
  "Copy the current file and line reference to the clipboard.
With active region: copies file:startline-endline.
Without region: copies file:line."
  (interactive)
  (let* ((rel (agents-repl--rel-path))
         (ref (if (use-region-p)
                  (let ((start-line (line-number-at-pos (region-beginning)))
                        (end-line (line-number-at-pos (region-end))))
                    (deactivate-mark)
                    (format "%s:%d-%d" rel start-line end-line))
                (format "%s:%d" rel (line-number-at-pos (point))))))
    (kill-new ref)
    (message "Copied: %s" ref)))

;; Keybindings
;; SPC o — Claude session control (open, focus, kill, interrupt, utilities)
(map! :leader
      :desc "Claude REPL" "o c" #'agents-repl
      :desc "Claude input" "o v" #'agents-repl-focus-input
      :desc "Kill Claude" "o C" #'agents-repl-kill
      :desc "Claude interrupt" "o x" #'agents-repl-interrupt
      :desc "Copy file reference" "o r" #'agents-repl-copy-reference)

(map! :leader
      (:prefix "p"
       :desc "Create worktree workspace" "w" #'agents-repl-create-worktree-workspace))

;; SPC j — Agent commands
(map! :leader
      (:prefix ("j" . "agent")
       :desc "Set backend for workspace" "b" #'agents-repl-set-backend
       :desc "Explain line/region/hunk"      "e" #'agents-repl-explain
       :desc "Update GitHub PR description"  "r" #'agents-repl-update-pr
       (:prefix ("E" . "explain diff")
        :desc "worktree"    "w" #'agents-repl-explain-diff-worktree
        :desc "staged"      "s" #'agents-repl-explain-diff-staged
        :desc "uncommitted" "u" #'agents-repl-explain-diff-uncommitted
        :desc "HEAD"        "h" #'agents-repl-explain-diff-head
        :desc "branch"      "b" #'agents-repl-explain-diff-branch)
       (:prefix ("R" . "update PR diff")
        :desc "worktree"    "w" #'agents-repl-update-pr-diff-worktree
        :desc "staged"      "s" #'agents-repl-update-pr-diff-staged
        :desc "uncommitted" "u" #'agents-repl-update-pr-diff-uncommitted
        :desc "HEAD"        "h" #'agents-repl-update-pr-diff-head
        :desc "branch"      "b" #'agents-repl-update-pr-diff-branch)
       (:prefix ("t" . "tests")
        (:prefix ("r" . "run")
         (:prefix ("t" . "tests")
          :desc "worktree"    "w" #'agents-repl-run-tests-worktree
          :desc "staged"      "s" #'agents-repl-run-tests-staged
          :desc "uncommitted" "u" #'agents-repl-run-tests-uncommitted
          :desc "HEAD"        "h" #'agents-repl-run-tests-head
          :desc "branch"      "b" #'agents-repl-run-tests-branch)
         (:prefix ("l" . "lint")
          :desc "worktree"    "w" #'agents-repl-run-lint-worktree
          :desc "staged"      "s" #'agents-repl-run-lint-staged
          :desc "uncommitted" "u" #'agents-repl-run-lint-uncommitted
          :desc "HEAD"        "h" #'agents-repl-run-lint-head
          :desc "branch"      "b" #'agents-repl-run-lint-branch)
         (:prefix ("a" . "all")
          :desc "worktree"    "w" #'agents-repl-run-all-worktree
          :desc "staged"      "s" #'agents-repl-run-all-staged
          :desc "uncommitted" "u" #'agents-repl-run-all-uncommitted
          :desc "HEAD"        "h" #'agents-repl-run-all-head
          :desc "branch"      "b" #'agents-repl-run-all-branch))
        (:prefix ("a" . "analyze")
         (:prefix ("q" . "quality")
          :desc "worktree"    "w" #'agents-repl-test-quality-worktree
          :desc "staged"      "s" #'agents-repl-test-quality-staged
          :desc "uncommitted" "u" #'agents-repl-test-quality-uncommitted
          :desc "HEAD"        "h" #'agents-repl-test-quality-head
          :desc "branch"      "b" #'agents-repl-test-quality-branch)
         (:prefix ("c" . "coverage")
          :desc "worktree"    "w" #'agents-repl-test-coverage-worktree
          :desc "staged"      "s" #'agents-repl-test-coverage-staged
          :desc "uncommitted" "u" #'agents-repl-test-coverage-uncommitted
          :desc "HEAD"        "h" #'agents-repl-test-coverage-head
          :desc "branch"      "b" #'agents-repl-test-coverage-branch)))))

(dotimes (i 10)
  (let ((char (number-to-string i)))
    (define-key doom-leader-map (kbd (format "o %s" char))
      (lambda () (interactive) (agents-repl-send-char char)))))

;; FIXME: fix magit handling

;; C-v paste forwarding to vterm
(defun agents-repl-paste-to-vterm ()
  "Forward a Ctrl-V keystroke to the Claude vterm buffer.
This lets Claude CLI handle paste natively, including images."
  (interactive)
  (when (agents-repl--vterm-live-p)
    (with-current-buffer (agents-repl--ws-get (+workspace-current-name) :vterm-buffer)
      (vterm-send-key "v" nil nil t))))

;;; Debug helpers — interactive commands for diagnosing workspace state issues.
;;; Call via M-x agents-repl-debug/...

(defun agents-repl-debug/workspace-states ()
  "Display all workspace states."
  (interactive)
  (let ((states (mapcar (lambda (name)
                          (cons name (agents-repl--ws-state name)))
                        (+workspace-list-names))))
    (message "Workspace states:\n%s"
             (mapconcat (lambda (s) (format "  %s: %s" (car s) (or (cdr s) "nil")))
                        states "\n"))))

(defun agents-repl-debug/buffer-info ()
  "Display all claude vterm buffers with their owning and persp workspaces."
  (interactive)
  (let (result)
    (dolist (buf (buffer-list))
      (when (agents-repl--claude-buffer-p buf)
        (push (format "  %s  owning=%s  persp=%s  thinking=%s"
                      (buffer-name buf)
                      (or (buffer-local-value 'agents-repl--owning-workspace buf) "nil")
                      (or (agents-repl--workspace-for-buffer buf) "nil")
                      (buffer-local-value 'agents-repl--title-thinking buf))
              result)))
    (message "Claude buffers:\n%s"
             (if result (mapconcat #'identity (nreverse result) "\n") "  (none)"))))

(defun agents-repl-debug/clear-state (ws)
  "Clear all states for workspace WS without killing buffers."
  (interactive
   (list (completing-read "Workspace: " (+workspace-list-names) nil t)))
  (agents-repl--ws-clear ws :thinking)
  (agents-repl--ws-clear ws :done)
  (agents-repl--ws-clear ws :permission)
  (agents-repl--ws-clear ws :stale)
  (message "Cleared all states for %s" ws))

(defun agents-repl-debug/obliterate (ws)
  "Completely remove workspace WS from all agents-repl tracking.
Kills claude buffers, closes windows, and removes all state."
  (interactive
   (list (completing-read "Obliterate workspace: " (+workspace-list-names) nil t)))
  ;; Find and destroy any claude buffers owned by this workspace
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (string-match-p "^\\*\\(claude\\|codex\\)-\\(input-\\)?[0-9a-f]+\\*$" (buffer-name buf))
               (equal ws (buffer-local-value 'agents-repl--owning-workspace buf)))
      (when-let ((win (get-buffer-window buf)))
        (ignore-errors (delete-window win)))
      (let ((proc (get-buffer-process buf)))
        (when proc (set-process-query-on-exit-flag proc nil)))
      (kill-buffer buf)))
  ;; Remove all state for this workspace from the single hash table
  (agents-repl--ws-del ws)
  (message "Obliterated all agents-repl state for %s" ws))

(defun agents-repl-debug/set-owning-workspace ()
  "Set the owning workspace for a claude vterm buffer."
  (interactive)
  (let* ((bufs (cl-remove-if-not
                (lambda (b) (agents-repl--claude-buffer-p b))
                (buffer-list)))
         (buf-name (completing-read "Buffer: " (mapcar #'buffer-name bufs) nil t))
         (ws (completing-read "Owning workspace: " (+workspace-list-names) nil t)))
    (with-current-buffer buf-name
      (setq-local agents-repl--owning-workspace ws))
    (message "Set %s owning workspace to %s" buf-name ws)))

(defun agents-repl-debug/toggle-logging ()
  "Toggle debug logging."
  (interactive)
  (setq agents-repl-debug (not agents-repl-debug))
  (message "Claude REPL debug logging: %s" (if agents-repl-debug "ON" "OFF")))

(defun agents-repl-debug/toggle-metaprompt ()
  "Toggle the metaprompt prefix injection."
  (interactive)
  (setq agents-repl-skip-permissions (not agents-repl-skip-permissions))
  (message "Claude REPL metaprompt: %s" (if agents-repl-skip-permissions "ON" "OFF")))

(defun agents-repl-debug/prefix-counter ()
  "Show the current metaprompt prefix counter, period, and workspace."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (counter (or (agents-repl--ws-get ws :prefix-counter) 0)))
    (message "[%s] Prefix counter: %d  period: %d  next metaprompt in: %d sends"
             ws counter agents-repl-prefix-period
             (- agents-repl-prefix-period (mod counter agents-repl-prefix-period)))))

(defun agents-repl--git-branch-for-workspace (ws-name)
  "Return the git branch for workspace WS-NAME, or nil.
Uses the first live buffer's default-directory and runs
git rev-parse --abbrev-ref HEAD (works for both repos and worktrees)."
  (ignore-errors
    (let* ((persp (persp-get-by-name ws-name))
           (bufs (and persp (not (symbolp persp)) (persp-buffers persp)))
           (dir (cl-loop for buf in bufs
                         when (buffer-live-p buf)
                         return (buffer-local-value 'default-directory buf))))
      (when dir
        (let ((default-directory dir))
          (with-temp-buffer
            (when (= 0 (process-file "git" nil t nil "rev-parse" "--abbrev-ref" "HEAD"))
              (string-trim (buffer-string)))))))))

(defun agents-repl--ws-base-name (ws-name)
  "Strip any existing branch suffix from WS-NAME.
E.g. \"my-project (main)\" → \"my-project\"."
  (if (string-match "\\(.+?\\) (.*?)$" ws-name)
      (match-string 1 ws-name)
    ws-name))

(defun agents-repl-debug/ws-set-branch (ws-name)
  "Rename workspace WS-NAME to include its current git branch.
Strips any existing branch suffix first, then appends \" (branch)\"."
  (interactive
   (list (completing-read "Workspace: " (+workspace-list-names) nil t
                          nil nil (+workspace-current-name))))
  (let* ((base (agents-repl--ws-base-name ws-name))
         (branch (agents-repl--git-branch-for-workspace ws-name))
         (new-name (if branch (format "%s (%s)" base branch) base)))
    (if (equal ws-name new-name)
        (message "Workspace %s: already up to date" ws-name)
      (+workspace-rename ws-name new-name)
      (message "Renamed: %s → %s" ws-name new-name))))

(defun agents-repl-debug/workspace-clean-p (ws-name)
  "Show whether workspace WS-NAME has unstaged changes to tracked files.
Uses `agents-repl--workspace-clean-p' — the same function used in production."
  (interactive
   (list (completing-read "Workspace: " (+workspace-list-names) nil t
                          nil nil (+workspace-current-name))))
  (let ((clean (agents-repl--workspace-clean-p ws-name)))
    (message "Workspace %s: %s" ws-name (if clean "clean" "dirty"))))

(defun agents-repl-debug/refresh-state (ws-name)
  "Force a full state refresh for workspace WS-NAME.
Runs the same logic as the periodic `update-all-workspace-states' timer:
checks claude visibility, git dirty status, and applies the state table.
Reports comprehensive diagnostics."
  (interactive
   (list (completing-read "Workspace: " (+workspace-list-names) nil t
                          nil nil (+workspace-current-name))))
  (let* ((before (agents-repl--ws-state ws-name))
         (open (agents-repl--ws-claude-open-p ws-name))
         (dirty (not (agents-repl--workspace-clean-p ws-name)))
         ;; Find the claude vterm buffer for this workspace
         (persp (persp-get-by-name ws-name))
         (persp-bufs (and persp (not (symbolp persp)) (persp-buffers persp)))
         (vterm-buf (cl-loop for buf in persp-bufs
                             when (and (buffer-live-p buf)
                                       (agents-repl--claude-buffer-p buf))
                             return buf))
         ;; Read buffer-local state from the vterm buffer
         (title-thinking (and vterm-buf
                              (buffer-local-value 'agents-repl--title-thinking vterm-buf)))
         (proc (and vterm-buf (get-buffer-process vterm-buf)))
         (proc-alive (and proc (process-live-p proc)))
         (owning-ws (and vterm-buf
                         (buffer-local-value 'agents-repl--owning-workspace vterm-buf)))
         (has-window (and vterm-buf (get-buffer-window vterm-buf t))))
    ;; Apply the state update
    (if open
        (agents-repl--update-ws-state ws-name)
      (let ((state (agents-repl--ws-state ws-name)))
        (when (and state (not (eq state :thinking)))
          (agents-repl--ws-clear ws-name state))))
    (let ((after (agents-repl--ws-state ws-name)))
      (force-mode-line-update t)
      (message (concat "Workspace %s:\n"
                       "  vterm-buf=%s process=%s\n"
                       "  owning-ws=%s title-thinking=%s has-window=%s\n"
                       "  claude-open=%s dirty=%s\n"
                       "  state=%s -> %s")
               ws-name
               (and vterm-buf (buffer-name vterm-buf))
               (if proc-alive "alive" "dead/nil")
               (or owning-ws "nil") title-thinking (if has-window "yes" "no")
               (if open "yes" "no") (if dirty "yes" "no")
               (or before "nil") (or after "nil")))))

(provide 'agents-repl)
;;; config.el ends here
