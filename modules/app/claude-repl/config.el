;;; config.el --- claude repl for doom emacs -*- lexical-binding: t; -*-

;; Author: Dodge Coates
;; URL: https://github.com/dodgecoates
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; A Claude Code REPL integration for Doom Emacs with workspace-aware
;; session management, input history, and status indicators.

;;; Code:

;; Cancel all previously registered timers on re-eval so we don't accumulate.
(defvar claude-repl--timers nil
  "List of active timers created by claude-repl.
Cancelled and reset whenever this file is re-evaluated.")

(dolist (timer claude-repl--timers)
  (when (timerp timer)
    (cancel-timer timer)))
(setq claude-repl--timers nil)

(defun claude-repl-debug/cancel-timers ()
  "Cancel all claude-repl timers."
  (interactive)
  (dolist (timer claude-repl--timers)
    (when (timerp timer)
      (cancel-timer timer)))
  (setq claude-repl--timers nil)
  (message "Cancelled all claude-repl timers."))

(defgroup claude-repl nil
  "Claude Code REPL integration for Doom Emacs."
  :group 'tools
  :prefix "claude-repl-")

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

(defvar claude-repl-git-branch
  (string-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD 2>/dev/null"))
  "The git branch active when claude-repl config was loaded.")

(defun claude-repl-print-git-branch ()
  "Print the git branch that was active when claude-repl config was loaded."
  (interactive)
  (message "claude-repl loaded on branch: %s" claude-repl-git-branch))

(defun claude-repl--path-canonical (path)
  "Return a canonical, stable string for PATH suitable for hashing.
Expands tildes and symlinks via `file-truename', then strips any trailing slash
via `directory-file-name' so that the same directory always produces the same hash."
  (directory-file-name (file-truename path)))

(defun claude-repl--workspace-id ()
  "Return a short identifier for the current git workspace.
Uses an MD5 hash of the canonical git root path.  Falls back to the buffer-local
`claude-repl--project-root' and then `default-directory'."
  (let ((root (claude-repl--resolve-root)))
    (when root
      (substring (md5 (claude-repl--path-canonical root)) 0 8))))


;; Single hash table for all per-workspace state.
;;
;; NOTE: workspace name ≠ git branch name.
;; `claude-repl--do-create-worktree-workspace' derives the persp name from the
;; *last path component* of the input (e.g. "DWC/fix-login" → persp "fix-login"),
;; while the full input becomes the branch name ("DWC/fix-login").  Never assume
;; the two are equal.  To resolve a workspace to its branch, retrieve its
;; :worktree-path from this hash and run `git rev-parse --abbrev-ref HEAD' there.
;; If :worktree-path is absent (e.g. after session restore), fall back to
;; `claude-repl--workspace-dir', which scans the workspace's live buffers.
(defvar claude-repl--workspaces (make-hash-table :test 'equal)
  "Hash table mapping workspace name → state plist.
Keys: :vterm-buffer :input-buffer :saved-window-config
      :return-window :prefix-counter :status :activity-time
      :git-clean :git-proc :worktree-p :worktree-path
      :force-bare-metal :fork-session-id :had-session
      :start-cmd :ready-timer :thinking :done
      :pending-prompts :pending-show-panels")

(defun claude-repl--ws-get (ws key)
  "Get KEY from workspace WS's plist."
  (plist-get (gethash ws claude-repl--workspaces) key))

(defun claude-repl--ws-put (ws key val)
  "Set KEY to VAL in workspace WS's plist in `claude-repl--workspaces'.
Internally uses plist-put (which returns a new list) threaded into puthash."
  (puthash ws (plist-put (gethash ws claude-repl--workspaces) key val)
           claude-repl--workspaces))

(defun claude-repl--ws-del (ws)
  "Remove all state for workspace WS."
  (remhash ws claude-repl--workspaces))

(defun claude-repl--register-worktree-ws (ws-id path &optional ws)
  "Mark workspace WS as a worktree workspace rooted at PATH.
WS-ID is the hash identifier (used for logging/buffer naming); the state
is stored under WS, defaulting to `+workspace-current-name'."
  (let ((ws (or ws (+workspace-current-name))))
    (claude-repl--log "register-worktree-ws ws-id=%s ws=%s path=%s" ws-id ws path)
    (claude-repl--ws-put ws :worktree-p t)
    (claude-repl--ws-put ws :worktree-path path)))

(defun claude-repl--setup-worktree-session (ws-id path ws force-bare-metal)
  "Register WS as a worktree at PATH and start its Claude session.
Sets :force-bare-metal if requested, then starts the session from PATH."
  (claude-repl--register-worktree-ws ws-id path ws)
  (when force-bare-metal
    (claude-repl--ws-put ws :force-bare-metal t))
  (let ((default-directory (file-name-as-directory path)))
    (claude-repl--ensure-session ws))
  (claude-repl--log "worktree pre-started Claude ws=%s cmd=%s"
                    ws (claude-repl--ws-get ws :start-cmd)))

(defun claude-repl--do-create-worktree-workspace (name &optional force-bare-metal fork-session-id preemptive-prompt)
  (let* ((git-root (string-trim
                    (shell-command-to-string "git rev-parse --show-toplevel")))
         (_ (when (string-match-p "^fatal" git-root)
              (user-error "Not in a git repository")))
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
         (path (expand-file-name dirname worktree-parent))
         (has-prompt (and preemptive-prompt (not (string-empty-p preemptive-prompt)))))
    (claude-repl--log "worktree git-root=%s name=%s dirname=%s branch=%s in-worktree=%s path=%s old-ws=%s old-ws-id=%s"
             git-root name dirname (or branch-name "none") in-worktree path
             (+workspace-current-name) (claude-repl--workspace-id))
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
      (claude-repl--log "worktree git cmd: %s" cmd)
      (claude-repl--log "worktree git result: %s" (string-trim result))
      (when (string-match-p "^fatal\\|^error" result)
        (user-error "git worktree add failed: %s" (string-trim result))))
    (write-region dirname nil (expand-file-name ".projectile" path))
    (claude-repl--log "worktree wrote .projectile, adding to projectile known projects")
    (projectile-add-known-project (file-name-as-directory path))
    (let* ((canonical (claude-repl--path-canonical path))
           (ws-id (substring (md5 canonical) 0 8))
           (ws dirname))
      (claude-repl--log "worktree creating workspace %s" ws)
      (+workspace-new ws)
      (when has-prompt
        (claude-repl--ws-put ws :pending-prompts (list preemptive-prompt))
        (claude-repl--ws-put ws :pending-show-panels t))
      (when fork-session-id
        (claude-repl--ws-put ws :fork-session-id fork-session-id))
      (claude-repl--setup-worktree-session ws-id path ws force-bare-metal))))

(defun claude-repl-create-worktree-workspace (arg)
  "Create a new git worktree and switch to it as a project workspace.
Prompts for a name (may use branch-style slashes like DC/CV-100/cool-branch).
The worktree directory uses only the last path component; the full name becomes
the branch name.

If called from a worktree, the new worktree is created as a sibling (../<dirname>).
If called from a normal repo, it is created under ../<repo-name>-worktrees/<dirname>.

Prefix arguments:
  \\[universal-argument]       — force bare-metal (skip Docker sandbox)
  \\[universal-argument] \\[universal-argument]     — fork current Claude session into new worktree
  \\[universal-argument] \\[universal-argument] \\[universal-argument]   — fork + force bare-metal

Optionally prompts for a preemptive prompt.  If provided, the new workspace is
created in the background (no switch) and the prompt is sent to Claude the moment
its session becomes ready.  If left blank, behavior is the same as before: switch
to the new workspace immediately."
  (interactive "P")
  (let* ((raw (prefix-numeric-value arg))
         (force-bare-metal (memq raw '(4 64)))
         (fork-p (>= raw 16))
         (fork-session-id
          (when fork-p
            (let ((sid (claude-repl--ws-get (+workspace-current-name) :session-id)))
              (unless sid
                (user-error "No session ID for current workspace — cannot fork"))
              sid)))
         (name (read-string "Worktree name: "))
         (preemptive-prompt (read-string "Preemptive prompt (blank to switch there normally): "))
         (dirname (file-name-nondirectory (directory-file-name name))))
    (claude-repl--do-create-worktree-workspace name force-bare-metal fork-session-id preemptive-prompt)
    (unless (and preemptive-prompt (not (string-empty-p preemptive-prompt)))
      (message "[claude-repl] pre-switch: fboundp(+workspace-switch-to)=%s current-ws=%s target=%s"
               (fboundp '+workspace-switch-to) (+workspace-current-name) dirname)
      (condition-case err
          (+workspace-switch-to dirname)
        (error
         (message "[claude-repl] +workspace-switch-to FAILED: %s — trying fallback (+workspace/switch-to)"
                  (error-message-string err))
         (condition-case err2
             (+workspace/switch-to dirname)
           (error
            (message "[claude-repl] fallback also FAILED: %s" (error-message-string err2)))))))))

(defun claude-repl--dispatch-prompt-command (ws prompt)
  "Send PROMPT to WS immediately if ready, otherwise enqueue on :pending-prompts.
WS may be a full branch name (e.g. DWC/foo) or a bare workspace name (e.g. foo);
it is normalized to the dirname before lookup."
  (let* ((ws (file-name-nondirectory (directory-file-name ws)))
         (vterm-buf (claude-repl--ws-get ws :vterm-buffer)))
    (if (and vterm-buf (buffer-local-value 'claude-repl--ready vterm-buf))
        (claude-repl--send-prompt-to-workspace ws prompt)
      (if (not vterm-buf)
          (claude-repl--log "dispatch-prompt-command: ws=%s not in registry, enqueuing" ws)
        (claude-repl--log "dispatch-prompt-command: ws=%s not ready, enqueuing" ws))
      (claude-repl--ws-put ws :pending-prompts
                           (append (claude-repl--ws-get ws :pending-prompts)
                                   (list prompt))))))

(defun claude-repl--process-workspace-commands-file (file)
  "Process a workspace commands file FILE, dispatching each typed command.
Create commands are staggered by 5 seconds each to avoid concurrent Claude
startup writes corrupting ~/.claude.json."
  (if (not (file-exists-p file))
      (claude-repl--log "workspace-commands-file not found: %s" file)
    (claude-repl--log "workspace-commands-file processing: %s" file)
    (let ((commands (json-read-file file))
          (create-delay 0))
      (mapc (lambda (cmd)
              (let ((type (alist-get 'type cmd)))
                (cond
                 ((string= type "create")
                  (let ((name (alist-get 'name cmd))
                        (prompt (alist-get 'prompt cmd nil))
                        (delay create-delay))
                    (claude-repl--log "workspace-commands-file create: %s (delay %.1fs)" name delay)
                    (run-with-timer delay nil
                                    (lambda ()
                                      (claude-repl--do-create-worktree-workspace name nil prompt))))
                  (cl-incf create-delay 5))
                 ((string= type "prompt")
                  (claude-repl--log "workspace-commands-file prompt: ws=%s" (alist-get 'workspace cmd))
                  (claude-repl--dispatch-prompt-command
                   (alist-get 'workspace cmd)
                   (alist-get 'prompt cmd)))
                 (t
                  (claude-repl--log "workspace-commands-file unknown type: %s" type)))))
            commands))
    (delete-file file)
    (claude-repl--log "workspace-commands-file deleted: %s" file)))

(defvar claude-repl--workspace-generation-watch nil)

(defvar claude-repl--fullscreen-config nil
  "Saved window configuration before fullscreen toggle.")

(defvar claude-repl--hide-overlay-refcount 0
  "Reference count for the hide overlay advice.
Only add advice when going from 0 to 1; only remove when going from 1 to 0.")

(defvar-local claude-repl-hide-overlay nil
  "Overlay used to hide Claude CLI input box.")

(defvar-local claude-repl--owning-workspace nil
  "Workspace name that owns this claude session.
Set when the user sends a message; used to correctly target workspace
state changes regardless of which persp the buffer drifts into.")

;; Input history
(defvar-local claude-repl--input-history nil
  "List of previous inputs, most recent first.")

(defvar-local claude-repl--history-index -1
  "Current position in history. -1 means not browsing.")

(defvar-local claude-repl--history-stash nil
  "Stashed in-progress text saved when history browsing begins.")

(defvar-local claude-repl--history-navigating nil
  "Non-nil while history navigation is replacing buffer text.")

(defcustom claude-repl-hide-input-box nil
  "When non-nil, hide the Claude CLI input box in the vterm buffer."
  :type 'boolean
  :group 'claude-repl)

(defcustom claude-repl-debug nil
  "Controls debug logging level.
nil means no logging; t means standard logging; \\='verbose also includes
high-frequency events (window changes, title spinner, resolve-root, poll-thinking)."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)
                 (const :tag "Verbose" verbose))
  :group 'claude-repl)

(defun claude-repl--log (fmt &rest args)
  "Log a timestamped debug message when `claude-repl-debug' is non-nil.
FMT and ARGS are passed to `message', prefixed with timestamp and [claude-repl]."
  (when claude-repl-debug
    (apply #'message (concat (format-time-string "%H:%M:%S.%3N") " [claude-repl] " fmt) args)))

(defun claude-repl--log-verbose (fmt &rest args)
  "Log a timestamped debug message only when `claude-repl-debug' is \\='verbose.
Used for high-frequency, low-signal events."
  (when (eq claude-repl-debug 'verbose)
    (apply #'message (concat (format-time-string "%H:%M:%S.%3N") " [claude-repl] " fmt) args)))

(make-directory (expand-file-name "~/.claude/output/") t)
(claude-repl--log "workspace-commands-watch: registering watch on %s for workspace_commands_*.json"
                  (expand-file-name "~/.claude/output/"))
(setq claude-repl--workspace-generation-watch
      (file-notify-add-watch
       (expand-file-name "~/.claude/output/")
       '(change)
       (lambda (event)
         (let* ((action (nth 1 event))
                ;; renamed events carry (descriptor renamed old-file new-file)
                ;; all other events carry (descriptor action file)
                (file (if (eq action 'renamed)
                          (nth 3 event)
                        (nth 2 event))))
           (when (and (memq action '(changed created renamed))
                      (string-prefix-p "workspace_commands_"
                                       (file-name-nondirectory file)))
             (claude-repl--process-workspace-commands-file file))))))

(defcustom claude-repl-skip-permissions t
  "When non-nil, prepend the command prefix metaprompt to each input sent to Claude."
  :type 'boolean
  :group 'claude-repl)

(defcustom claude-repl-prefix-period 7
  "Number of prompts between metaprompt prefix injections.
The prefix is sent on the first prompt and every Nth prompt thereafter."
  :type 'integer
  :group 'claude-repl)


(defcustom claude-repl-send-postfix "\n what do you think? do NOT code, just analyze."
  "String appended to input when sending via `claude-repl-send-with-postfix'."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-docker-image ""
  "Fallback Docker image for sandboxed worktree workspaces with no .claude/sandbox/image.
Prefer per-repo .claude/sandbox/image files over this global setting.
If empty (the default), worktrees without a .claude/sandbox/image run Claude directly."
  :type 'string
  :group 'claude-repl)

(defvar claude-repl-paste-delay 0.25
  "Seconds to wait after pasting before sending Return.
Used by `claude-repl--send-input-to-vterm' for large inputs.")

(defcustom claude-repl-command-prefix "DO NOT run any mutating git commands (push, reset, checkout, etc) without EXPLICIT PERMISSION from ME. Do not INSTALL or UNINSTALL anything without my EXPLICIT PERMISSION. Do not operate on any files OUTSIDE OF PROJECT without MY EXPLICIT PERMISSION. Do not take any actions unless it FOLLOWS DIRECTLY from an action EXPLICITLY REQUESTED in the following prompt. I will NEVER ask a rhetorical question -- do not infer that I want you to take action to fix the source of a bug i've just asked a question about."
  "When non-nil, this string is prepended (with a newline) before every input sent to Claude."
  :type 'string
  :group 'claude-repl)

(defvar claude-repl--command-prefix (format "<<*this is a metaprompt. I will periodically prefix my prompts with this to remind you of our restrictions for freely making changes. Do not be alarmed, this is merely a periodic reminder*: %s *metaprompt over* (rest is actual user request that you should respond to directly)>>\n\n" claude-repl-command-prefix)
  "Formatted metaprompt string prepended before every input when `claude-repl-skip-permissions' is non-nil (see `claude-repl-prefix-period').")


;; Set to t once Claude has set its terminal title (meaning it's ready).
(defvar-local claude-repl--ready nil
  "Non-nil once Claude Code has finished starting up.")

(defun claude-repl--buffer-name (&optional suffix)
  "Return a project-specific buffer name like *claude-HASH* or *claude-input-HASH*.
SUFFIX, if provided, is inserted before the hash (e.g. \"-input\")."
  (let ((id (claude-repl--workspace-id)))
    (format "*claude%s-%s*" (or suffix "") (or id "default"))))


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
                           (claude-repl--claude-buffer-p))
                      (claude-repl--grey 15)
                    result)))))

(defun claude-repl--live-return-window ()
  "Return the return-window for the current workspace if live, else `selected-window'."
  (let ((win (claude-repl--ws-get (+workspace-current-name) :return-window)))
    (if (and win (window-live-p win)) win (selected-window))))

;; Manual window layout: vterm on the right (full height), input below vterm.
(defun claude-repl--show-panels ()
  "Display vterm and input panels to the right of the current window.
Splits right for vterm (60% width to work window), then splits vterm bottom for input (15%)."
  (let* ((ws (+workspace-current-name))
         (vterm-buf (claude-repl--ws-get ws :vterm-buffer))
         (input-buf (claude-repl--ws-get ws :input-buffer)))
    (claude-repl--log "show-panels vterm=%s input=%s"
                      (and vterm-buf (buffer-name vterm-buf))
                      (and input-buf (buffer-name input-buf)))
    (let* ((work-win (claude-repl--live-return-window))
           (vterm-win (split-window work-win (round (* 0.6 (window-total-width work-win))) 'right))
           (input-win (split-window vterm-win (round (* -0.15 (window-total-height vterm-win))) 'below)))
      (claude-repl--refresh-vterm)
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
  (claude-repl--update-all-workspace-states))

(defun claude-repl--focus-input-panel ()
  "Focus the input panel window and enter insert state."
  (claude-repl--log "focus-input-panel")
  (when-let ((buf (claude-repl--ws-get (+workspace-current-name) :input-buffer))
             (win (get-buffer-window buf)))
    (select-window win)
    (evil-insert-state)))

(defun claude-repl--grey (n)
  "Return a hex color string for greyscale value N (0=black, 255=white)."
  (format "#%02x%02x%02x" n n n))

(defun claude-repl--claude-buffer-p (&optional buf)
  "Return non-nil if BUF (default: current buffer) is a Claude vterm buffer."
  (string-match-p "^\\*claude-[0-9a-f]+\\*$"
                  (buffer-name (or buf (current-buffer)))))

(defun claude-repl--resolve-root ()
  "Return the project root directory.
Tries git root, then buffer-local project root, then `default-directory'."
  (let* ((git (claude-repl--git-root))
         (root (or git claude-repl--project-root default-directory))
         (source (cond (git "git") (claude-repl--project-root "buffer-local") (t "default-directory"))))
    (claude-repl--log-verbose "resolve-root source=%s root=%s" source root)
    root))

(defun claude-repl--vterm-live-p ()
  "Return non-nil if the Claude vterm buffer for the current workspace exists and is live."
  (let ((buf (claude-repl--ws-get (+workspace-current-name) :vterm-buffer)))
    (and buf (buffer-live-p buf))))

(defun claude-repl--set-buffer-background (grey-level)
  "Set default and fringe background to greyscale GREY-LEVEL in current buffer."
  (face-remap-add-relative 'default :background (claude-repl--grey grey-level))
  (face-remap-add-relative 'fringe :background (claude-repl--grey grey-level)))

(defun claude-repl--docker-image-exists-p (image)
  "Return non-nil if IMAGE exists in the local Docker image store."
  (= 0 (call-process "docker" nil nil nil "image" "inspect" "--format" "." image)))

(defun claude-repl--resolve-sandbox-config (git-root)
  "Return a plist (:image IMAGE :script SCRIPT) for a worktree at GIT-ROOT.
Tries .claude/sandbox/image first (new layout), then .claude/sandbox-image (legacy).
Returns nil if no image file exists or the image is not present locally.
Returns (:needs-build t :install-script PATH) if the image file exists but the
Docker image has not been built yet."
  (let* ((new-image-file (expand-file-name ".claude/sandbox/image" git-root))
         (old-image-file (expand-file-name ".claude/sandbox-image" git-root))
         (image-file (cond ((file-readable-p new-image-file) new-image-file)
                           ((file-readable-p old-image-file) old-image-file)))
         (new-layout (equal image-file new-image-file)))
    (when image-file
      (let* ((image (string-trim (with-temp-buffer
                                   (insert-file-contents image-file)
                                   (buffer-string))))
             (script (expand-file-name (if new-layout
                                           ".claude/sandbox/claude-sandbox"
                                         ".claude/claude-sandbox")
                                       git-root)))
        (if (string-empty-p image)
            (progn (message "[claude-repl] sandbox image file is empty in %s" git-root) nil)
          (if (claude-repl--docker-image-exists-p image)
              (list :image image :script script)
            (let ((install-script (expand-file-name ".claude/sandbox/install.sh" git-root)))
              (list :needs-build t
                    :image image
                    :install-script (when (file-executable-p install-script) install-script)))))))))

(defun claude-repl--start-claude ()
  "Send the claude startup command to the current vterm buffer.
For worktree workspaces (:worktree-p t), delegates to .claude/sandbox/claude-sandbox
if a .claude/sandbox/image file exists.  Falls back to bare-metal Claude otherwise."
  (let* ((ws (or claude-repl--owning-workspace (+workspace-current-name)))
         (fresh (not (claude-repl--ws-get ws :had-session)))
         (worktree-p (claude-repl--ws-get ws :worktree-p))
         (worktree-path (claude-repl--ws-get ws :worktree-path))
         (force-bare-metal (claude-repl--ws-get ws :force-bare-metal))
         (fork-session-id (claude-repl--ws-get ws :fork-session-id))
         (sandbox-config (when (and worktree-p (not force-bare-metal))
                           (claude-repl--resolve-sandbox-config
                            (claude-repl--git-root worktree-path))))
         (_ (when (plist-get sandbox-config :needs-build)
              (let* ((image (plist-get sandbox-config :image))
                     (install-script (plist-get sandbox-config :install-script)))
                (if install-script
                    (when (y-or-n-p (format "Sandbox image '%s' not built. Run install.sh now? " image))
                      (compile (format "bash %s" install-script))
                      (user-error "Run 'SPC o c' again once the build completes"))
                  (user-error "Sandbox image '%s' not built — run .claude/sandbox/install.sh manually" image)))))
         (docker-image (and (not (plist-get sandbox-config :needs-build))
                            (plist-get sandbox-config :image)))
         ;; For docker: claude-sandbox script handles permission mode automatically.
         ;; For bare-metal: detect by path — ChessCom repos use --permission-mode auto,
         ;; personal repos use --dangerously-skip-permissions.
         (perm-flag (unless (and worktree-p docker-image)
                      (if (string-match-p "ChessCom" (expand-file-name (or worktree-path default-directory)))
                          "--permission-mode auto"
                        "--dangerously-skip-permissions")))
         (claude-flags (string-trim
                        (mapconcat #'identity
                                   (delq nil (list
                                              (when (and fresh fork-session-id)
                                                (format "--resume %s --fork-session"
                                                        fork-session-id))
                                              (unless fresh "-c")
                                              perm-flag))
                                   " ")))
         (cmd (if (and worktree-p docker-image)
                  (string-trim (concat (plist-get sandbox-config :script) " " claude-flags))
                (string-trim (concat "claude " claude-flags)))))
    (claude-repl--ws-put ws :start-cmd cmd)
    (when fork-session-id
      (claude-repl--ws-put ws :fork-session-id nil))
    (setq-local mode-line-format
                (list (if (and worktree-p docker-image)
                          (propertize (format " DOCKER SANDBOX: %s" docker-image)
                                      'face '(:foreground "green" :weight bold))
                        (propertize (format " BARE METAL: %s" (system-name))
                                    'face '(:foreground "red" :weight bold)))))
    (message "[claude-repl] start-claude ws=%s had-session=%s fork-session-id=%s fresh=%s worktree=%s cmd=%s"
             ws (claude-repl--ws-get ws :had-session) fork-session-id
             (if fresh "yes" "no") (if worktree-p "yes" "no") cmd)
    (claude-repl--log "start-claude dir=%s fresh=%s worktree=%s cmd=%s"
                      default-directory (if fresh "yes" "no") (if worktree-p "yes" "no") cmd)
    (setq-local claude-repl--ready nil)
    (vterm-send-string (concat "clear && " cmd))
    (vterm-send-return)
    (claude-repl--schedule-ready-timer ws)))

;; Instructions bar face
(defface claude-repl-header-line
  '((t :background "white" :foreground "black" :weight bold))
  "Face for the Claude Input header line.")

;; Input mode
(defun claude-repl--slash-intercept-backspace ()
  "Always forward backspace to vterm; also redirect to slash handler in slash mode.
Runs as a buffer-local `pre-command-hook'.  Setting `this-command' here causes
Emacs to call our handler instead of the originally resolved command."
  (when (memq this-command
              '(evil-delete-backward-char-and-join
                evil-delete-backward-char
                delete-backward-char
                backward-delete-char-untabify))
    (unless claude-slash-input-mode
      (when (= (buffer-size) 0)
        (let* ((ws (+workspace-current-name))
               (vterm-buf (claude-repl--ws-get ws :vterm-buffer)))
          (when (and vterm-buf (buffer-live-p vterm-buf))
            (with-current-buffer vterm-buf
              (vterm-send-key "<backspace>"))))))
    (when claude-slash-input-mode
      (setq this-command #'claude-repl--slash-backspace))))

(define-derived-mode claude-input-mode fundamental-mode "Claude Input"
  "Major mode for Claude REPL input buffer."
  (setq-local header-line-format
              "RET: send | C-RET: send+postfix | C-c C-c: clear+save (empty→C-c) | C-c C-k: interrupt | <up>/<down>: history")
  (face-remap-add-relative 'header-line 'claude-repl-header-line)
  (claude-repl--set-buffer-background 37)
  (visual-line-mode 1)
  (add-hook 'after-change-functions #'claude-repl--history-on-change nil t)
  (add-hook 'pre-command-hook #'claude-repl--slash-intercept-backspace nil t))

(defun claude-repl-discard-input ()
  "Save current input to history, clear the buffer, and enter insert state."
  (interactive)
  (claude-repl--log "discard-input")
  (when claude-slash-input-mode
    (setq claude-repl--slash-stack nil)
    (claude-slash-input-mode -1))
  (claude-repl--history-push)
  (claude-repl--history-reset)
  (erase-buffer)
  (evil-insert-state))

(defun claude-repl-discard-or-send-interrupt ()
  "If input buffer is empty, send C-c to Claude; otherwise discard input."
  (interactive)
  (if (string-blank-p (buffer-string))
      (progn
        (claude-repl--log "discard-or-send-interrupt: empty buffer, sending C-c to vterm")
        (when (claude-repl--vterm-live-p)
          (let ((vterm-buf (claude-repl--ws-get (+workspace-current-name) :vterm-buffer)))
            (with-current-buffer vterm-buf
              (vterm-send-key "c" nil nil t)))))
    (claude-repl-discard-input)))

(defun claude-repl-scroll-down ()
  "Scroll the Claude vterm buffer down."
  (interactive)
  (claude-repl--log "scroll-down")
  (when (claude-repl--vterm-live-p)
    (with-current-buffer (claude-repl--ws-get (+workspace-current-name) :vterm-buffer)
      (vterm-send-down))))

(defun claude-repl-scroll-up ()
  "Scroll the Claude vterm buffer up."
  (interactive)
  (claude-repl--log "scroll-up")
  (when (claude-repl--vterm-live-p)
    (with-current-buffer (claude-repl--ws-get (+workspace-current-name) :vterm-buffer)
      (vterm-send-up))))

(defun claude-repl--scroll-output-up ()
  "Scroll the Claude vterm output window up (toward older output)."
  (interactive)
  (when (claude-repl--vterm-live-p)
    (let* ((vterm-buf (claude-repl--ws-get (+workspace-current-name) :vterm-buffer))
           (vterm-win (get-buffer-window vterm-buf)))
      (when vterm-win
        (with-selected-window vterm-win
          (scroll-down 3))))))

(defun claude-repl--scroll-output-down ()
  "Scroll the Claude vterm output window down (toward newer output)."
  (interactive)
  (when (claude-repl--vterm-live-p)
    (let* ((vterm-buf (claude-repl--ws-get (+workspace-current-name) :vterm-buffer))
           (vterm-win (get-buffer-window vterm-buf)))
      (when vterm-win
        (with-selected-window vterm-win
          (scroll-up 3))))))

(defun claude-repl--input-wheel-up ()
  "Scroll the Claude vterm window toward older output (mouse wheel up from input)."
  (interactive)
  (when (claude-repl--vterm-live-p)
    (let* ((vterm-buf (claude-repl--ws-get (+workspace-current-name) :vterm-buffer))
           (vterm-win (get-buffer-window vterm-buf)))
      (when vterm-win
        (with-selected-window vterm-win
          (scroll-down 3))))))

(defun claude-repl--input-wheel-down ()
  "Scroll the Claude vterm window toward newer output (mouse wheel down from input)."
  (interactive)
  (when (claude-repl--vterm-live-p)
    (let* ((vterm-buf (claude-repl--ws-get (+workspace-current-name) :vterm-buffer))
           (vterm-win (get-buffer-window vterm-buf)))
      (when vterm-win
        (with-selected-window vterm-win
          (scroll-up 3))))))

(map! :map claude-input-mode-map
      :ni "RET"       #'claude-repl-send
      :ni "S-RET"     #'newline
      :i  "/"         #'claude-repl--slash-start
      :ni "C-RET"     #'claude-repl-send-with-postfix
      :ni "C-S-RET"   #'claude-repl-send-with-metaprompt
      [remap +default/newline-below] #'claude-repl-send-with-postfix
      [remap +default/newline-above] #'claude-repl-send-with-metaprompt
      :ni "C-c C-k"   #'claude-repl-interrupt
      :ni "C-c C-c"   #'claude-repl-discard-or-send-interrupt
      :ni "C-c y"     (cmd! (claude-repl-send-char "y"))
      :ni "C-c n"     (cmd! (claude-repl-send-char "n"))
      :ni "C-c r"     #'claude-repl-restart
      :ni "C-c q"     #'claude-repl-kill
      :ni "C-S-m"     #'claude-repl-cycle
      :ni "C-h"       #'evil-window-left
      :n  "C-n"       #'claude-repl-scroll-down
      :n  "C-p"       #'claude-repl-scroll-up
      :ni "C-v"       #'claude-repl-paste-to-vterm
      :n  "<up>"        #'claude-repl--history-prev
      :n  "<down>"      #'claude-repl--history-next
      :i  "<up>"        #'claude-repl--send-up-arrow
      :i  "<down>"      #'claude-repl--send-down-arrow
      :ni "S-<up>"      #'claude-repl--scroll-output-up
      :ni "S-<down>"    #'claude-repl--scroll-output-down
      :ni "C-S-p"       #'claude-repl--scroll-output-up
      :ni "C-S-n"       #'claude-repl--scroll-output-down
      [wheel-up]        #'claude-repl--input-wheel-up
      [wheel-down]      #'claude-repl--input-wheel-down)

;; C-S-0 through C-S-9: send digit to Claude
(dotimes (i 10)
  (let ((char (number-to-string i)))
    (define-key claude-input-mode-map (kbd (format "C-S-%s" char))
      (lambda () (interactive) (claude-repl-send-char char)))))

;; 0-9 in insert mode: if the buffer is empty, enter pass-through mode and
;; forward the digit to vterm; otherwise insert normally.
(dotimes (i 10)
  (let ((char (number-to-string i)))
    (evil-define-key 'insert claude-input-mode-map (kbd char)
      (lambda () (interactive)
        (claude-repl--passthrough-start char)))))

;; Input history persistence
(defun claude-repl--history-file ()
  "Return the path to the history file for the current project."
  (expand-file-name ".claude-repl-history" (claude-repl--resolve-root)))

(defun claude-repl--history-save (&optional ws)
  "Write input history to disk.
Resolves the project root from the input buffer's local variable
rather than `default-directory' of whatever buffer is current.
WS defaults to the current workspace name."
  (claude-repl--log "history-save")
  (let* ((ws (or ws (+workspace-current-name)))
         (buf (and ws (claude-repl--ws-get ws :input-buffer))))
    (when (and buf (buffer-live-p buf))
      (let* ((root (buffer-local-value 'claude-repl--project-root buf))
             (history (buffer-local-value 'claude-repl--input-history buf))
             (file (expand-file-name ".claude-repl-history" (or root default-directory))))
        (when history
          (with-temp-file file
            (prin1 history (current-buffer))))))))

(defun claude-repl--history-restore ()
  "Load input history from disk into the current buffer."
  (claude-repl--log "history-restore")
  (let ((file (claude-repl--history-file)))
    (when (file-exists-p file)
      (setq claude-repl--input-history
            (with-temp-buffer
              (insert-file-contents file)
              (read (current-buffer)))))))

;; Input history functions
(defun claude-repl--history-push ()
  "Save current input buffer text to history.
Skips empty strings and duplicates of the most recent entry."
  (let ((text (string-trim (buffer-string))))
    (unless (string-empty-p text)
      (unless (equal text (car claude-repl--input-history))
        (push text claude-repl--input-history)))))

(defun claude-repl--history-reset ()
  "Reset history browsing index to the default (not browsing) state."
  (setq claude-repl--history-index -1))

(defun claude-repl--history-prev ()
  "Navigate to the previous (older) history entry."
  (interactive)
  (claude-repl--log "history-prev index=%d" claude-repl--history-index)
  (when claude-repl--input-history
    (let ((next-index (1+ claude-repl--history-index)))
      (when (< next-index (length claude-repl--input-history))
        ;; Stash current text on first navigation
        (when (= claude-repl--history-index -1)
          (setq claude-repl--history-stash (buffer-string)))
        (setq claude-repl--history-index next-index)
        (let ((claude-repl--history-navigating t))
          (erase-buffer)
          (insert (nth claude-repl--history-index claude-repl--input-history)))))))

(defun claude-repl--history-next ()
  "Navigate to the next (newer) history entry, or restore stashed text."
  (interactive)
  (claude-repl--log "history-next index=%d" claude-repl--history-index)
  (when (>= claude-repl--history-index 0)
    (let ((next-index (1- claude-repl--history-index)))
      (let ((claude-repl--history-navigating t))
        (erase-buffer)
        (if (< next-index 0)
            ;; Past newest entry — restore stash
            (progn
              (setq claude-repl--history-index -1)
              (when claude-repl--history-stash
                (insert claude-repl--history-stash)))
          (setq claude-repl--history-index next-index)
          (insert (nth claude-repl--history-index claude-repl--input-history)))))))

(defun claude-repl--history-on-change (&rest _)
  "Reset history browsing when the user edits the buffer directly."
  (unless claude-repl--history-navigating
    (when (>= claude-repl--history-index 0)
      (claude-repl--log "history-on-change resetting from index=%d" claude-repl--history-index))
    (setq claude-repl--history-index -1)))

;; Core functions
(defvar claude-repl-metaprompt-exempt-strings
  '("/clear" "/usage" "/login" "/logout")
  "Inputs that should never have the metaprompt prepended.
Compared exactly against the trimmed input.")

(defun claude-repl--skip-metaprompt-p (raw)
  "Return non-nil if RAW input should never have the metaprompt prepended.
Matches `claude-repl-metaprompt-exempt-strings' and bare numerals,
ignoring trailing whitespace."
  (let ((trimmed (string-trim-right raw)))
    (or (member trimmed claude-repl-metaprompt-exempt-strings)
        (string-match-p "^[0-9]+$" trimmed))))

(defvar claude-repl-send-posthooks
  '(("^/clear$" . claude-repl--posthook-reset-prefix-counter))
  "Alist of (PATTERN . FUNCTION) posthooks run after input is sent.
PATTERN is a string or regexp matched against the raw input (trimmed).
FUNCTION is called with (WS RAW) where WS is the workspace name and RAW is the input.")

(defun claude-repl--posthook-reset-prefix-counter (ws _raw)
  "Reset the metaprompt prefix counter for workspace WS.
Resets to 1 (just past the firing point) so the next send does not
immediately re-trigger the metaprompt."
  (claude-repl--ws-put ws :prefix-counter 1))

(defun claude-repl--run-send-posthooks (ws raw)
  "Run posthooks matching RAW input for workspace WS."
  (let ((trimmed (string-trim-right raw)))
    (dolist (hook claude-repl-send-posthooks)
      (when (string-match-p (car hook) trimmed)
        (claude-repl--log "posthook matched pattern=%s" (car hook))
        (funcall (cdr hook) ws raw)))))

(defun claude-repl--prepare-input (ws)
  "Read and return input from the input buffer for workspace WS.
Periodically prepends the metaprompt prefix when `claude-repl-skip-permissions' is on."
  (let* ((input-buf (claude-repl--ws-get ws :input-buffer))
         (counter (or (claude-repl--ws-get ws :prefix-counter) 0))
         (raw (with-current-buffer input-buf (buffer-string))))
    (claude-repl--log "prepare-input counter=%d period=%d" counter claude-repl-prefix-period)
    (if (and claude-repl-skip-permissions claude-repl-command-prefix
             (not (claude-repl--skip-metaprompt-p raw))
             (zerop (mod counter claude-repl-prefix-period)))
        (concat claude-repl--command-prefix raw)
      raw)))

(defun claude-repl--send-input-to-vterm (vterm-buf input)
  "Send INPUT string to VTERM-BUF.
Uses paste mode for large inputs to avoid truncation."
  (claude-repl--log "send-input-to-vterm len=%d mode=%s" (length input)
                    (if (> (length input) 200) "paste" "direct"))
  (with-current-buffer vterm-buf
    (if (> (length input) 200)
        (let ((buf (current-buffer)))
          (vterm-send-string input t)
          (run-at-time claude-repl-paste-delay nil
                       (lambda ()
                         (when (buffer-live-p buf)
                           (with-current-buffer buf
                             (vterm-send-return)
                             (run-at-time 0.05 nil
                                          (lambda ()
                                            (when (buffer-live-p buf)
                                              (with-current-buffer buf
                                                (vterm-send-return)
                                                (claude-repl--refresh-vterm))))))))))
      (vterm-send-string input)
      (vterm-send-return)
      (claude-repl--refresh-vterm))))

(defun claude-repl--mark-ws-thinking (ws)
  "Mark workspace WS as thinking: set state and record activity."
  (claude-repl--log "mark-ws-thinking ws=%s" ws)
  (claude-repl--ws-set ws :thinking)
  (claude-repl--touch-activity ws))

(defun claude-repl--clear-input (ws)
  "Push current input to history, reset browsing, and clear the input buffer for workspace WS."
  (claude-repl--log "clear-input")
  (let ((input-buf (claude-repl--ws-get ws :input-buffer)))
    (when input-buf
      (with-current-buffer input-buf
        (claude-repl--history-push)
        (claude-repl--history-reset)
        (erase-buffer)))))

(defun claude-repl--do-send (ws input raw)
  "Core send: dispatch INPUT to WS's vterm.
Increments the prefix counter, pins the owning workspace, marks the workspace
as thinking, sends INPUT, and runs posthooks with RAW (the undecorated text)."
  (let ((vterm-buf (claude-repl--ws-get ws :vterm-buffer)))
    (claude-repl--log "do-send ws=%s len=%d" ws (length input))
    (claude-repl--ws-put ws :prefix-counter
                         (1+ (or (claude-repl--ws-get ws :prefix-counter) 0)))
    ;; Pin the owning workspace on the vterm buffer so title-change
    ;; clears the correct workspace even if the buffer drifts between persps.
    (when vterm-buf
      (with-current-buffer vterm-buf
        (setq-local claude-repl--owning-workspace ws)))
    (claude-repl--mark-ws-thinking ws)
    (claude-repl--send-input-to-vterm vterm-buf input)
    (claude-repl--run-send-posthooks ws raw)))

(defun claude-repl--send-prompt-to-workspace (ws prompt)
  "Programmatically send PROMPT string to workspace WS.
Intended for non-interactive callers (e.g. file-based dispatch).
Signals an error if WS has no live vterm buffer."
  (unless (claude-repl--ws-get ws :vterm-buffer)
    (error "claude-repl--send-prompt-to-workspace: no vterm buffer for workspace %s" ws))
  (claude-repl--do-send ws prompt prompt))

(defun claude-repl-send ()
  "Send input buffer contents to Claude, clear buffer, and return to previous window."
  (interactive)
  (let* ((ws (+workspace-current-name)))
    (unless ws (error "claude-repl-send: no active workspace"))
    (let* ((input-buf (claude-repl--ws-get ws :input-buffer)))
      (when (and input-buf (claude-repl--vterm-live-p))
        (let* ((raw (with-current-buffer input-buf (buffer-string)))
               (input (claude-repl--prepare-input ws)))
          (claude-repl--do-send ws input raw)
          (claude-repl--clear-input ws)
          (claude-repl--select-return-window ws))))))

(defun claude-repl--select-return-window (ws)
  "Select the saved return window for WS if it is still live."
  (let ((ret (claude-repl--ws-get ws :return-window)))
    (when (and ret (window-live-p ret))
      (select-window ret))))

(defun claude-repl--remember-return-window ()
  "Save the current window as the return target, unless we're in the input buffer."
  (let ((ws (+workspace-current-name)))
    (unless (eq (current-buffer) (claude-repl--ws-get ws :input-buffer))
      (claude-repl--log "remember-return-window %s" (selected-window))
      (claude-repl--ws-put ws :return-window (selected-window)))))

(defun claude-repl--restore-layout ()
  "Restore the window layout from before panels were shown.
Falls back to hiding panels and selecting the return window."
  (let* ((ws (+workspace-current-name))
         (saved (claude-repl--ws-get ws :saved-window-config)))
    (claude-repl--log "restore-layout saved-config=%s" (if saved "yes" "no"))
    (if saved
        (progn
          (set-window-configuration saved)
          (claude-repl--ws-put ws :saved-window-config nil))
      (claude-repl--hide-panels)
      (let ((ret (claude-repl--ws-get ws :return-window)))
        (when (and ret (window-live-p ret))
          (select-window ret))))))

(defun claude-repl-send-and-hide ()
  "Send input to Claude and hide both panels."
  (interactive)
  (claude-repl--log "send-and-hide")
  (claude-repl-send)
  (claude-repl--restore-layout))

(defun claude-repl-send-with-metaprompt ()
  "Send input with the metaprompt prefix, bypassing the counter."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (input-buf (claude-repl--ws-get ws :input-buffer)))
    (when (and input-buf (claude-repl--vterm-live-p))
      (let* ((raw (with-current-buffer input-buf (buffer-string)))
             (input (if (and claude-repl-skip-permissions claude-repl-command-prefix
                             (not (claude-repl--skip-metaprompt-p raw)))
                        (concat claude-repl--command-prefix raw)
                      raw)))
        (claude-repl--do-send ws input raw)
        (claude-repl--clear-input ws)
        (claude-repl--select-return-window ws)))))

(defun claude-repl-send-with-postfix ()
  "Append `claude-repl-send-postfix' to the input buffer, then send."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (input-buf (claude-repl--ws-get ws :input-buffer)))
    (when input-buf
      (with-current-buffer input-buf
        (goto-char (point-max))
        (insert claude-repl-send-postfix))))
  (claude-repl-send))

(defun claude-repl-send-char (char)
  "Send a single character to Claude."
  (claude-repl--log "send-char %s" char)
  (when (claude-repl--vterm-live-p)
    (let ((vterm-buf (claude-repl--ws-get (+workspace-current-name) :vterm-buffer)))
      (with-current-buffer vterm-buf
        (vterm-send-string char)
        (vterm-send-return)))))

;;; Slash-command pass-through mode
;;
;; When the user types "/" into an empty input buffer, every subsequent
;; keystroke is forwarded directly to vterm without being inserted into the
;; input buffer.  The buffer stays visually empty.  Backspace is forwarded too,
;; and deleting back past the initial "/" exits the mode.

(defvar-local claude-repl--slash-stack nil
  "Stack of characters forwarded to vterm in slash mode.
Each element is the string that was sent (the leading \"/\" is the first entry).
Popped on backspace; when empty the mode exits.")

(defun claude-repl--slash-vterm-send (str)
  "Send STR to the current workspace's vterm buffer without a trailing return."
  (let* ((ws (+workspace-current-name))
         (vterm-buf (claude-repl--ws-get ws :vterm-buffer)))
    (when (and vterm-buf (buffer-live-p vterm-buf))
      (with-current-buffer vterm-buf
        (vterm-send-string str)))))

(defun claude-repl--slash-forward-char ()
  "Forward the typed character to vterm without inserting it into the buffer."
  (interactive)
  (let ((char (string last-command-event)))
    (claude-repl--slash-vterm-send char)
    (push char claude-repl--slash-stack)))

(defun claude-repl--slash-backspace ()
  "Pop from the slash stack; send backspace to vterm; exit mode when stack is empty."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (vterm-buf (claude-repl--ws-get ws :vterm-buffer)))
    (when (and vterm-buf (buffer-live-p vterm-buf))
      (with-current-buffer vterm-buf
        (vterm-send-key "<backspace>"))))
  (pop claude-repl--slash-stack)
  (when (null claude-repl--slash-stack)
    (claude-slash-input-mode -1)))

(defun claude-repl--slash-return ()
  "Send return to vterm and exit slash mode."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (vterm-buf (claude-repl--ws-get ws :vterm-buffer)))
    (when (and vterm-buf (buffer-live-p vterm-buf))
      (with-current-buffer vterm-buf
        (vterm-send-return))))
  (setq claude-repl--slash-stack nil)
  (claude-slash-input-mode -1))

(defun claude-repl--passthrough-start (char)
  "Enter pass-through mode and send CHAR to vterm if the buffer is empty.
Otherwise insert CHAR normally.  Used for /, digits, and any other
characters that should go directly to Claude when typed first."
  (if (= (buffer-size) 0)
      (progn
        (claude-slash-input-mode 1)
        (setq claude-repl--slash-stack (list char))
        (claude-repl--slash-vterm-send char))
    (self-insert-command 1 (string-to-char char))))

(defun claude-repl--slash-start ()
  "Enter pass-through mode if the buffer is empty, else insert / normally."
  (interactive)
  (claude-repl--passthrough-start "/"))

(define-minor-mode claude-slash-input-mode
  "Minor mode that transparently forwards keystrokes to Claude vterm.
Active when the user begins input with /. The input buffer stays empty;
all characters are sent directly to vterm."
  :lighter " /…"
  :keymap (make-sparse-keymap))

;; Use remaps throughout so evil keymap priority is irrelevant — remaps are
;; resolved after key→command lookup and apply across all evil states.
(defun claude-repl--slash-tab ()
  "Forward a tab character to vterm in slash mode."
  (interactive)
  (claude-repl--slash-vterm-send "\t")
  (push "\t" claude-repl--slash-stack))

(defun claude-repl--send-up-arrow ()
  "Send up arrow key to the current workspace's vterm."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (vterm-buf (claude-repl--ws-get ws :vterm-buffer)))
    (when (and vterm-buf (buffer-live-p vterm-buf))
      (with-current-buffer vterm-buf
        (vterm-send-key "<up>")))))

(defun claude-repl--send-down-arrow ()
  "Send down arrow key to the current workspace's vterm."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (vterm-buf (claude-repl--ws-get ws :vterm-buffer)))
    (when (and vterm-buf (buffer-live-p vterm-buf))
      (with-current-buffer vterm-buf
        (vterm-send-key "<down>")))))

(map! :map claude-slash-input-mode-map
      [remap self-insert-command]                #'claude-repl--slash-forward-char
      [remap indent-for-tab-command]             #'claude-repl--slash-tab
      [remap evil-delete-backward-char-and-join] #'claude-repl--slash-backspace
      [remap delete-backward-char]               #'claude-repl--slash-backspace
      [remap backward-delete-char-untabify]      #'claude-repl--slash-backspace
      [remap claude-repl-send]                   #'claude-repl--slash-return
      :ni "<up>"   #'ignore
      :ni "<down>" #'ignore)

(defun claude-repl--ensure-session (&optional ws)
  "Ensure a Claude session exists (vterm + input + overlay) for WS.
WS defaults to the current workspace name.  No-op if already running."
  (unless (claude-repl--vterm-running-p ws)
    (claude-repl--log "ensure-session: starting new session")
    (let ((ws (or ws (+workspace-current-name))))
      (unless ws (error "claude-repl--ensure-session: no active workspace"))
      (claude-repl--ensure-vterm-buffer ws)
      (claude-repl--ensure-input-buffer ws)
      (claude-repl--ws-put ws :prefix-counter 0))
    (claude-repl--enable-hide-overlay)))

(defun claude-repl--send-to-claude (text)
  "Send TEXT to Claude, starting it if needed."
  (claude-repl--log "send-to-claude len=%d" (length text))
  (let ((ws (+workspace-current-name)))
    (claude-repl--ensure-session)
    (claude-repl--send-input-to-vterm
     (claude-repl--ws-get ws :vterm-buffer) text)))

(defun claude-repl--rel-path ()
  "Return the current file path relative to the project root."
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Buffer %s is not visiting a file" (buffer-name)))
    (file-relative-name file (claude-repl--resolve-root))))

(defun claude-repl-explain ()
  "Ask Claude to explain the selected region, current line, or current file.
With active region: sends file path and line range.
In magit hunk: sends file path and hunk line range.
Without region: sends file path and current line."
  (interactive)
  (let* ((msg
          (cond
           ;; Active region in any buffer
           ((use-region-p)
            (let ((rel (claude-repl--rel-path))
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
                         (claude-repl--resolve-root))))
              (format "please explain %s:%d-%d" rel start end)))
           ;; Default: current line
           (t
            (format "please explain %s:%d"
                    (claude-repl--rel-path) (line-number-at-pos (point)))))))
    (claude-repl--log "explain %s" msg)
    (claude-repl--send-to-claude msg)))

(defcustom claude-repl-explain-diff-prompt
  "please explain the changes"
  "Prompt sent to Claude by explain-diff commands."
  :type 'string
  :group 'claude-repl)

(defun claude-repl-explain-diff-worktree ()
  "Ask Claude to explain unstaged changes."
  (interactive)
  (claude-repl--send-diff-analysis "unstaged changes (git diff)" claude-repl-explain-diff-prompt))

(defun claude-repl-explain-diff-staged ()
  "Ask Claude to explain staged changes."
  (interactive)
  (claude-repl--send-diff-analysis "staged changes (git diff --cached)" claude-repl-explain-diff-prompt))

(defun claude-repl-explain-diff-uncommitted ()
  "Ask Claude to explain all uncommitted changes."
  (interactive)
  (claude-repl--send-diff-analysis "uncommitted changes (git diff HEAD)" claude-repl-explain-diff-prompt))

(defun claude-repl-explain-diff-head ()
  "Ask Claude to explain the last commit."
  (interactive)
  (claude-repl--send-diff-analysis "last commit (git show HEAD)" claude-repl-explain-diff-prompt))

(defun claude-repl-explain-diff-branch ()
  "Ask Claude to explain all changes in the current branch."
  (interactive)
  (claude-repl--send-diff-analysis claude-repl-branch-diff-spec claude-repl-explain-diff-prompt))

(defun claude-repl-interrupt ()
  "Send Escape to interrupt Claude."
  (interactive)
  (claude-repl--log "interrupt")
  (when (claude-repl--vterm-live-p)
    (let ((vterm-buf (claude-repl--ws-get (+workspace-current-name) :vterm-buffer)))
      (with-current-buffer vterm-buf
        (vterm-send-key "<escape>")
        (vterm-send-key "<escape>"))
      (run-at-time 0.25 nil
                   (lambda ()
                     (when (buffer-live-p vterm-buf)
                       (with-current-buffer vterm-buf
                         (vterm-send-string "i"))))))))

;; Git diff analysis commands
(defcustom claude-repl-update-pr-prompt
  "please update the PR description for the PR corresponding to our branch"
  "Prompt sent to Claude by `claude-repl-update-pr'."
  :type 'string
  :group 'claude-repl)

(defun claude-repl-update-pr ()
  "Ask Claude to update the PR description for the current branch."
  (interactive)
  (claude-repl--send-to-claude claude-repl-update-pr-prompt))

(defcustom claude-repl-update-pr-diff-prompt
  "please update the PR description"
  "Prompt sent to Claude by update-pr-diff commands."
  :type 'string
  :group 'claude-repl)

(defun claude-repl-update-pr-diff-worktree ()
  "Ask Claude to update the PR description for unstaged changes."
  (interactive)
  (claude-repl--send-diff-analysis "UNSTAGED changes (git diff). So not consider staged changes or committed changes." claude-repl-update-pr-diff-prompt))

(defun claude-repl-update-pr-diff-staged ()
  "Ask Claude to update the PR description for staged changes."
  (interactive)
  (claude-repl--send-diff-analysis "STAGED changes (git diff --cached). Do not consider unstaged changes or committed changes." claude-repl-update-pr-diff-prompt))

(defun claude-repl-update-pr-diff-uncommitted ()
  "Ask Claude to update the PR description for all uncommitted changes."
  (interactive)
  (claude-repl--send-diff-analysis "All UNCOMMITTED changes (git diff HEAD). Consider BOTH staged and unstaged changes. Do not consider committed changes." claude-repl-update-pr-diff-prompt))

(defun claude-repl-update-pr-diff-head ()
  "Ask Claude to update the PR description for the last commit."
  (interactive)
  (claude-repl--send-diff-analysis "last commit (git show HEAD)." claude-repl-update-pr-diff-prompt))

(defun claude-repl-update-pr-diff-branch ()
  "Ask Claude to update the PR description for all changes in the current branch."
  (interactive)
  (claude-repl--send-diff-analysis claude-repl-branch-diff-spec claude-repl-update-pr-diff-prompt))

(defcustom claude-repl-run-tests-prompt
  "please run tests, and summarize the issues found and probable causes"
  "Prompt sent to Claude by run-tests commands."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-run-lint-prompt
  "please run lint, and address any issues found"
  "Prompt sent to Claude by run-lint commands."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-run-all-prompt
  "please run lint and tests, and address any issues found for both"
  "Prompt sent to Claude by run-all commands."
  :type 'string
  :group 'claude-repl)

(defun claude-repl-run-tests-worktree ()
  "Run tests for unstaged changes."
  (interactive)
  (claude-repl--send-diff-analysis "unstaged changes (git diff)" claude-repl-run-tests-prompt))

(defun claude-repl-run-tests-staged ()
  "Run tests for staged changes."
  (interactive)
  (claude-repl--send-diff-analysis "staged changes (git diff --cached)" claude-repl-run-tests-prompt))

(defun claude-repl-run-tests-uncommitted ()
  "Run tests for all uncommitted changes."
  (interactive)
  (claude-repl--send-diff-analysis "uncommitted changes (git diff HEAD)" claude-repl-run-tests-prompt))

(defun claude-repl-run-tests-head ()
  "Run tests for the last commit."
  (interactive)
  (claude-repl--send-diff-analysis "last commit (git show HEAD)" claude-repl-run-tests-prompt))

(defun claude-repl-run-tests-branch ()
  "Run tests for all changes in the current branch."
  (interactive)
  (claude-repl--send-diff-analysis claude-repl-branch-diff-spec claude-repl-run-tests-prompt))

(defun claude-repl-run-lint-worktree ()
  "Run lint for unstaged changes."
  (interactive)
  (claude-repl--send-diff-analysis "unstaged changes (git diff)" claude-repl-run-lint-prompt))

(defun claude-repl-run-lint-staged ()
  "Run lint for staged changes."
  (interactive)
  (claude-repl--send-diff-analysis "staged changes (git diff --cached)" claude-repl-run-lint-prompt))

(defun claude-repl-run-lint-uncommitted ()
  "Run lint for all uncommitted changes."
  (interactive)
  (claude-repl--send-diff-analysis "uncommitted changes (git diff HEAD)" claude-repl-run-lint-prompt))

(defun claude-repl-run-lint-head ()
  "Run lint for the last commit."
  (interactive)
  (claude-repl--send-diff-analysis "last commit (git show HEAD)" claude-repl-run-lint-prompt))

(defun claude-repl-run-lint-branch ()
  "Run lint for all changes in the current branch."
  (interactive)
  (claude-repl--send-diff-analysis claude-repl-branch-diff-spec claude-repl-run-lint-prompt))

(defun claude-repl-run-all-worktree ()
  "Run lint and tests for unstaged changes."
  (interactive)
  (claude-repl--send-diff-analysis "unstaged changes (git diff)" claude-repl-run-all-prompt))

(defun claude-repl-run-all-staged ()
  "Run lint and tests for staged changes."
  (interactive)
  (claude-repl--send-diff-analysis "staged changes (git diff --cached)" claude-repl-run-all-prompt))

(defun claude-repl-run-all-uncommitted ()
  "Run lint and tests for all uncommitted changes."
  (interactive)
  (claude-repl--send-diff-analysis "uncommitted changes (git diff HEAD)" claude-repl-run-all-prompt))

(defun claude-repl-run-all-head ()
  "Run lint and tests for the last commit."
  (interactive)
  (claude-repl--send-diff-analysis "last commit (git show HEAD)" claude-repl-run-all-prompt))

(defun claude-repl-run-all-branch ()
  "Run lint and tests for all changes in the current branch."
  (interactive)
  (claude-repl--send-diff-analysis claude-repl-branch-diff-spec claude-repl-run-all-prompt))

(defconst claude-repl--test-quality-prompt
  "please analyze tests to ensure they are following AAA standards for testing. Please be sure to confine your analysis to the specified context (branch, HEAD, uncommitted changes, etc). They should be employing DRY principle for refactoring as well (extract repeated code into helpers, use builder pattern to facilitate test DSL). We should only be testing one thing per test (can extract tests into subtests to ensure this). Ensure that tests are correctly grouped into subtests, and that very similar/redundant suites are merged. We should not be using ANY timing logic in tests. If there is any timing logic found, surface it. It is FINE for potentially hanging tests to become unblocked with ERROR after some amount of time -- we are only concerned with not attempting to ballpark synchronization via time. We should be careful to NOT reduce the production code path coverage of our refactors -- for example, we should avoid removing asserts in the effort to 'only test one thing', and instead prefer adding a new subtest. Please spin up ONE AGENT PER TEST FILE!")

(defconst claude-repl--test-coverage-prompt
  "Please be sure to confine your analysis to the specified context (branch, HEAD, uncommitted changes, etc). <<IF AND ONLY IF YOU JUST PRODUCED A LIST OF EDGE CASES>>: write up a plan for producing a unit test that covers each and every one of the edge cases you just enumerated. Each test should cover *precisely* one edge case. Each test file should be worked on by a separate agent. <<IF AND ONLY IF YOU DID NOT -- I REPEAT, NOT -- JUST PRODUCE A LIST OF EDGE CASES IN YOUR LAST RESPONSE MESSAGE>>: please enumerate each and every edge cases introduced or modified by each and every function added or modified.")

(defun claude-repl--send-diff-analysis (change-spec prompt)
  "Send a diff analysis request to Claude.
CHANGE-SPEC describes which changes (e.g. \"unstaged changes (git diff)\").
PROMPT is the analysis instruction."
  (let ((msg (format "for the %s, %s" change-spec prompt)))
    (claude-repl--log "diff-analysis: %s" change-spec)
    (claude-repl--send-to-claude msg)))

(defcustom claude-repl-branch-diff-spec
  "changes in current branch (git diff $(git merge-base HEAD origin/master))"
  "Change-spec string used for branch-level quality and coverage analysis commands."
  :type 'string
  :group 'claude-repl)

(defun claude-repl-test-quality-worktree ()
  "Analyze test quality for unstaged changes."
  (interactive)
  (claude-repl--send-diff-analysis "unstaged changes (git diff)" claude-repl--test-quality-prompt))

(defun claude-repl-test-quality-staged ()
  "Analyze test quality for staged changes."
  (interactive)
  (claude-repl--send-diff-analysis "staged changes (git diff --cached)" claude-repl--test-quality-prompt))

(defun claude-repl-test-quality-uncommitted ()
  "Analyze test quality for all uncommitted changes."
  (interactive)
  (claude-repl--send-diff-analysis "uncommitted changes (git diff HEAD)" claude-repl--test-quality-prompt))

(defun claude-repl-test-quality-head ()
  "Analyze test quality for the last commit."
  (interactive)
  (claude-repl--send-diff-analysis "last commit (git show HEAD)" claude-repl--test-quality-prompt))

(defun claude-repl-test-quality-branch ()
  "Analyze test quality for all changes in the current branch."
  (interactive)
  (claude-repl--send-diff-analysis claude-repl-branch-diff-spec claude-repl--test-quality-prompt))

(defun claude-repl-test-coverage-worktree ()
  "Analyze test coverage for unstaged changes."
  (interactive)
  (claude-repl--send-diff-analysis "unstaged changes (git diff)" claude-repl--test-coverage-prompt))

(defun claude-repl-test-coverage-staged ()
  "Analyze test coverage for staged changes."
  (interactive)
  (claude-repl--send-diff-analysis "staged changes (git diff --cached)" claude-repl--test-coverage-prompt))

(defun claude-repl-test-coverage-uncommitted ()
  "Analyze test coverage for all uncommitted changes."
  (interactive)
  (claude-repl--send-diff-analysis "uncommitted changes (git diff HEAD)" claude-repl--test-coverage-prompt))

(defun claude-repl-test-coverage-head ()
  "Analyze test coverage for the last commit."
  (interactive)
  (claude-repl--send-diff-analysis "last commit (git show HEAD)" claude-repl--test-coverage-prompt))

(defun claude-repl-test-coverage-branch ()
  "Analyze test coverage for all changes in the current branch."
  (interactive)
  (claude-repl--send-diff-analysis claude-repl-branch-diff-spec claude-repl--test-coverage-prompt))

;; Hide overlay functions
(defun claude-repl--create-hide-overlay ()
  "Create a new hide overlay covering the bottom 4 lines of the current buffer."
  (when (and claude-repl-hide-input-box (> (point-max) 1))
    (let* ((end (point-max))
           (start (save-excursion
                    (goto-char end)
                    (forward-line -4)
                    (line-beginning-position))))
      (when (< start end)
        (setq claude-repl-hide-overlay (make-overlay start end nil t nil))
        (overlay-put claude-repl-hide-overlay 'display "")
        (overlay-put claude-repl-hide-overlay 'evaporate t)))))

(defun claude-repl--update-hide-overlay ()
  "Update overlay to hide bottom lines of Claude vterm buffer.
When called from within a claude buffer, updates it directly.
Otherwise looks up the vterm buffer for the current workspace."
  (let ((buf (if (claude-repl--claude-buffer-p)
                 (current-buffer)
               (let ((ws (+workspace-current-name)))
                 (and ws (claude-repl--ws-get ws :vterm-buffer))))))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (claude-repl--delete-hide-overlay)
        (claude-repl--create-hide-overlay)))))

(defun claude-repl-toggle-hide-input-box ()
  "Toggle hiding of Claude CLI's input box in the vterm buffer."
  (interactive)
  (claude-repl--log "toggle-hide-input-box -> %s" (not claude-repl-hide-input-box))
  (setq claude-repl-hide-input-box (not claude-repl-hide-input-box))
  (claude-repl--update-hide-overlay)
  (message "Claude input box hiding %s" (if claude-repl-hide-input-box "enabled" "disabled")))

;; Notifications
(defun claude-repl--notify-terminal-notifier (title message)
  "Send a desktop notification via terminal-notifier."
  (call-process "terminal-notifier" nil 0 nil
                "-title" title
                "-message" message))

(defun claude-repl--notify-osascript (title message)
  "Send a desktop notification via osascript."
  (start-process "claude-notify" nil
                 "osascript" "-e"
                 (format "display notification %S with title %S sound name \"default\""
                         message title)))

(defvar claude-repl--notify-fn
  (if (executable-find "terminal-notifier")
      #'claude-repl--notify-terminal-notifier
    #'claude-repl--notify-osascript)
  "Function used to send desktop notifications.")

(defun claude-repl--notify (title message)
  "Send a desktop notification with TITLE and MESSAGE."
  (claude-repl--log "notify title=%s msg=%s" title message)
  (funcall claude-repl--notify-fn title message))

;; Workspace tab indicator for Claude status

(defcustom claude-repl-stale-minutes 60
  "Minutes after last input before a workspace tab stops showing as stale."
  :type 'integer
  :group 'claude-repl)

(defun claude-repl--ws-state (ws)
  "Return the current status keyword for workspace WS.
Returns one of: :thinking, :done, :permission, :stale, or nil."
  (let ((plist (gethash ws claude-repl--workspaces)))
    (or (plist-get plist :status)
        (when-let ((t0 (plist-get plist :activity-time)))
          (when (< (- (float-time) t0) (* claude-repl-stale-minutes 60))
            :stale)))))

(defun claude-repl--ws-set (ws state)
  "Set workspace WS to STATE.
STATE is one of: :thinking, :done, :permission."
  (unless ws (error "claude-repl--ws-set: ws is nil"))
  (claude-repl--log "state %s -> %s" ws state)
  (claude-repl--ws-put ws :status state)
  (force-mode-line-update t))

(defun claude-repl--ws-clear (ws state)
  "Clear a single STATE for workspace WS.
STATE is one of: :thinking, :done, :permission, :stale."
  (unless ws (error "claude-repl--ws-clear: ws is nil"))
  (claude-repl--log "clear %s %s" ws state)
  (pcase state
    ((or :thinking :done :permission)
     (when (eq (claude-repl--ws-get ws :status) state)
       (claude-repl--ws-put ws :status nil)))
    (:stale (claude-repl--ws-put ws :activity-time nil)))
  (force-mode-line-update t))

(defun claude-repl--ws-dir (ws)
  "Return a directory associated with workspace WS, used for git checks.
Returns the `default-directory' of the first live buffer in WS, or nil."
  (ignore-errors
    (let* ((persp (persp-get-by-name ws))
           (bufs (and persp (not (symbolp persp)) (persp-buffers persp))))
      (cl-loop for buf in bufs
               when (buffer-live-p buf)
               return (buffer-local-value 'default-directory buf)))))

(defun claude-repl--workspace-clean-p (ws)
  "Return non-nil if workspace WS has no unstaged changes to tracked files.
Reads from a cached value updated asynchronously by
`claude-repl--async-refresh-git-status'.  Defaults to non-nil (clean) when
the cache has not yet been populated."
  (not (eq (claude-repl--ws-get ws :git-clean) 'dirty)))

(defun claude-repl--async-refresh-git-status (ws)
  "Asynchronously refresh the git cleanliness cache for workspace WS.
Starts `git diff --quiet' in WS's directory.  On exit, sets `:git-clean'
to `clean' or `dirty' in the workspace plist and calls
`claude-repl--update-ws-state' to apply any resulting state transition.
A no-op if a check is already in progress for WS."
  (when-let ((dir (claude-repl--ws-dir ws)))
    (unless (let ((proc (claude-repl--ws-get ws :git-proc)))
              (and proc (process-live-p proc)))
      (let* ((default-directory dir)
             (proc (make-process
                    :name (format "claude-repl-git-%s" ws)
                    :command '("git" "diff" "--quiet")
                    :connection-type 'pipe
                    :noquery t
                    :buffer nil
                    :sentinel
                    (lambda (proc _event)
                      (unless (process-live-p proc)
                        (claude-repl--ws-put ws :git-clean
                                            (if (= 0 (process-exit-status proc))
                                                'clean
                                              'dirty))
                        (claude-repl--ws-put ws :git-proc nil)
                        (claude-repl--update-ws-state ws)
                        (force-mode-line-update t))))))
        (claude-repl--ws-put ws :git-proc proc)))))

(defun claude-repl--touch-activity (ws)
  "Record current time as last input for workspace WS.
Only sets stale if the workspace has no unstaged changes to tracked files."
  (unless ws (error "claude-repl--touch-activity: ws is nil"))
  (if (claude-repl--workspace-clean-p ws)
      (claude-repl--ws-put ws :activity-time (float-time))
    (claude-repl--ws-put ws :activity-time nil)))

(defun claude-repl--workspace-for-buffer (buf)
  "Return the workspace name that contains BUF, or nil."
  (when (bound-and-true-p persp-mode)
    (cl-loop for persp in (persp-persps)
             when (persp-contain-buffer-p buf persp)
             return (safe-persp-name persp))))

(defface claude-repl-tab-thinking
  '((t :background "#cc3333" :foreground "white" :weight bold))
  "Face for workspace tabs where Claude is thinking (red).")

(defface claude-repl-tab-done
  '((t :background "#1a7a1a" :foreground "black" :weight bold))
  "Face for workspace tabs where Claude is done (green).")

(defface claude-repl-tab-permission
  '((t :background "#1a7a1a" :foreground "black" :weight bold))
  "Face for workspace tabs where Claude needs permission (green + emoji).")

(defface claude-repl-tab-stale
  '((t :background "#cc8800" :foreground "black" :weight bold))
  "Face for workspace tabs you've worked in recently but aren't viewing (orange).")

(defun claude-repl--tabline-advice (&optional names)
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
                     (state (claude-repl--ws-state name))
                     (claude-face (pcase state
                                   (:permission 'claude-repl-tab-permission)
                                   (:done       'claude-repl-tab-done)
                                   (:thinking   'claude-repl-tab-thinking)
                                   (:stale      'claude-repl-tab-stale)))
                     (label (pcase state
                              (:permission "❓")
                              (_ num)))
                     (base-face (if (and selected (not claude-face))
                                    '+workspace-tab-selected-face
                                  '+workspace-tab-face))
                     (face (or claude-face base-face)))
                (if selected
                    ;; Selected: light grey background on whole tab, state foreground on all text
                    (let* ((fg (pcase state
                                 (:thinking   "#cc3333")
                                 (:done       "#2a8c2a")
                                 (:permission "#2a8c2a")
                                 (:stale      "#cc8800")))
                           (fg "black")
                           (base-face `(:background "#c0c0c0"
                                        :foreground ,fg
                                        :weight bold))
                           (no-bg-face `(:background nil
                                         :foreground ,fg
                                         :weight bold)))
                      (concat (propertize " " 'face no-bg-face)
                              (propertize (format "[%s]" label)
                                          'face `(:foreground ,fg :weight bold
                                                  :background ,(plist-get base-face :background)))
                              (propertize (format " %s " name) 'face base-face)))
                  ;; Unselected: full background across the whole tab
                  (concat (propertize " " 'face '(:background nil))
                          (propertize (format "[%s]" label)
                                      'face '(:foreground "#4477cc" :background nil))
                          (propertize (format " %s " name) 'face face)))))
     " ")))

(advice-add '+workspace--tabline :override #'claude-repl--tabline-advice)

;; Suppress the echo area flash when switching workspaces.
;; Doom calls (+workspace/display) after switch/cycle/new/load, which uses
;; (message ...) to show the tabline in the echo area.  Since tabs are
;; already visible at the top, the bottom flash is redundant.
(advice-add '+workspace/display :override #'ignore)

;; Periodically redraw invisible claude vterm buffers in :thinking workspaces.
;; This catches missed title transitions (e.g., Claude finished while we were
;; on a different workspace and vterm never redrawed).
(defun claude-repl--poll-thinking-workspaces ()
  "Redraw invisible claude vterm buffers whose workspace is :thinking."
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (string-match-p "^\\*claude-[0-9a-f]+\\*$" (buffer-name buf))
               (not (get-buffer-window buf))  ;; invisible
               (with-current-buffer buf (eq major-mode 'vterm-mode)))
      (let ((ws (or (buffer-local-value 'claude-repl--owning-workspace buf)
                     (claude-repl--workspace-for-buffer buf))))
        (when (and ws (eq (claude-repl--ws-state ws) :thinking))
          (claude-repl--log-verbose "poll-thinking: redrawing %s (ws=%s)" (buffer-name buf) ws)
          (with-current-buffer buf
            (claude-repl--do-refresh)))))))

(push (run-with-timer 5 5 #'claude-repl--poll-thinking-workspaces)
      claude-repl--timers)

;; Periodically update all workspace states (catches git changes, etc.)
(push (run-with-timer 1 1 #'claude-repl--update-all-workspace-states)
      claude-repl--timers)

;; Title-based "Claude is done" detection.
;; Claude Code sets the terminal title to "<spinner> Claude Code" while thinking
;; and plain "Claude Code" when idle.  We poll via vterm--set-title advice.
(defvar-local claude-repl--title-thinking nil
  "Non-nil when the vterm title indicates Claude is thinking.
Buffer-local so multiple Claude sessions don't interfere.")

(defun claude-repl--title-has-spinner-p (title)
  "Return non-nil if TITLE contains a spinner (i.e. not the idle ✳ icon)."
  (and (> (length title) 0)
       (not (string-prefix-p "✳" title))
       (string-match-p "^[^[:ascii:]]" title)))

(defun claude-repl--detect-title-transition (title)
  "Classify a vterm TITLE change into a transition type.
Returns a plist with:
  :thinking    - non-nil if the new title has a spinner
  :transition  - one of 'started, 'finished, or nil (no change)
  :ws          - the owning workspace name for the current buffer, or nil"
  (let* ((thinking (claude-repl--title-has-spinner-p title))
         (ws (or claude-repl--owning-workspace
                 (claude-repl--workspace-for-buffer (current-buffer))))
         (transition (cond
                      ((and thinking (not claude-repl--title-thinking)) 'started)
                      ((and (not thinking) claude-repl--title-thinking) 'finished)
                      (t nil))))
    (list :thinking thinking :transition transition :ws ws)))

(defun claude-repl--swap-placeholder ()
  "Replace the loading placeholder window with the real vterm buffer.
Called once when Claude sets its first terminal title (meaning it's ready)."
  (claude-repl--log "swap-placeholder buf=%s" (buffer-name))
  (let ((buf (current-buffer))
        (placeholder (get-buffer " *claude-loading*")))
    (when placeholder
      (run-at-time 0 nil
                   (lambda ()
                     (when (buffer-live-p buf)
                       (dolist (win (get-buffer-window-list placeholder nil t))
                         (set-window-dedicated-p win nil)
                         (set-window-buffer win buf)
                         (set-window-dedicated-p win t))
                       (kill-buffer placeholder)))))))

(defun claude-repl--maybe-notify-finished (ws)
  "Send a desktop notification that Claude finished in WS, if frame is unfocused."
  (claude-repl--log "maybe-notify-finished ws=%s focused=%s" ws (if (frame-focus-state) "yes" "no"))
  (unless (frame-focus-state)
    (run-at-time 0.1 nil #'claude-repl--notify "Claude REPL"
                 (format "%s: Claude ready" ws))))

(defun claude-repl--on-claude-finished (ws)
  "Handle Claude finishing work in workspace WS.
Sets done state if buffer is hidden, refreshes display."
  (claude-repl--log "on-claude-finished ws=%s visible=%s focused=%s"
                    ws (if (get-buffer-window (current-buffer) t) "yes" "no")
                    (if (frame-focus-state) "yes" "no"))
  (unless (get-buffer-window (current-buffer) t)
    (claude-repl--ws-set ws :done))
  (claude-repl--refresh-vterm)
  (claude-repl--update-hide-overlay)
  (claude-repl--maybe-notify-finished ws)
  (unless (string= ws (+workspace-current-name))
    (message "Claude finished in workspace: %s" ws)
    (run-at-time 0.1 nil #'claude-repl--notify "Claude REPL"
                 (format "%s: Claude ready" ws))))

(defun claude-repl--capture-session-id (ws)
  "Scan ~/.claude/sessions/ for a running session matching WS's project root.
Stores the session ID as :session-id on the workspace plist."
  (let* ((root (claude-repl--path-canonical
                (or (claude-repl--ws-get ws :worktree-path)
                    claude-repl--project-root
                    default-directory)))
         ;; Docker sandbox mounts the worktree at /<dirname>, so session files
         ;; record a container path rather than the full host path.
         (container-path (concat "/" (file-name-nondirectory root)))
         (sessions-dir (expand-file-name "~/.claude/sessions/"))
         (found-id nil))
    (when (file-directory-p sessions-dir)
      (dolist (file (directory-files sessions-dir t "\\.json\\'"))
        (condition-case nil
            (let* ((json (json-read-file file))
                   (cwd (cdr (assq 'cwd json)))
                   (session-id (cdr (assq 'sessionId json))))
              (when (and cwd session-id
                         (or (string= (claude-repl--path-canonical cwd) root)
                             (string= cwd container-path)))
                (setq found-id session-id)))
          (error nil))))
    (if found-id
        (progn
          (claude-repl--ws-put ws :session-id found-id)
          (claude-repl--log "capture-session-id ws=%s id=%s" ws found-id))
      (claude-repl--log "capture-session-id ws=%s: no matching session found" ws))))

(defun claude-repl--handle-first-ready ()
  "Handle the first terminal title set — Claude is now ready.
Cancels the ready-timer, swaps the loading placeholder, drains any pending
prompts (with a 0.3s delay), and auto-opens panels if appropriate."
  (unless claude-repl--ready
    (claude-repl--log "first-ready buf=%s ws=%s" (buffer-name) claude-repl--owning-workspace)
    (setq claude-repl--ready t)
    (when claude-repl--owning-workspace
      (claude-repl--cancel-ready-timer claude-repl--owning-workspace)
      (claude-repl--capture-session-id claude-repl--owning-workspace))
    (claude-repl--swap-placeholder)
    (let* ((ws claude-repl--owning-workspace)
           (pending (when ws (claude-repl--ws-get ws :pending-prompts))))
      (if pending
          (progn
            (claude-repl--log "first-ready draining %d pending prompt(s) for ws=%s" (length pending) ws)
            (claude-repl--ws-put ws :pending-prompts nil)
            (let ((vterm-buf (claude-repl--ws-get ws :vterm-buffer)))
              (run-at-time 0.3 nil
                           (lambda ()
                             (when (buffer-live-p vterm-buf)
                               (dolist (p pending)
                                 (claude-repl--send-prompt-to-workspace ws p))))))
            ;; Open panels now if on this workspace, otherwise defer until switch.
            ;; claude-repl--on-workspace-switch checks :pending-show-panels.
            (if (string= ws (+workspace-current-name))
                (claude-repl)
              (claude-repl--ws-put ws :pending-show-panels t)))
        (progn
          (claude-repl--log "first-ready no pending prompts for ws=%s" ws)
          (when (string= ws (+workspace-current-name))
            (claude-repl)))))))

(defun claude-repl--on-title-change (title)
  "Detect thinking->idle transition from vterm title changes."
  (claude-repl--log-verbose "title-change buf=%s title=%s" (buffer-name) title)
  (when (claude-repl--claude-buffer-p)
    (claude-repl--handle-first-ready)
    (let* ((info (claude-repl--detect-title-transition title))
           (thinking (plist-get info :thinking))
           (transition (plist-get info :transition))
           (ws (plist-get info :ws)))
      ;; Update buffer-local thinking state before side effects so
      ;; re-entrant vterm--redraw calls see the current value and
      ;; detect no transition (prevents infinite recursion).
      (setq claude-repl--title-thinking thinking)
      (when transition
        (if (not ws)
            (claude-repl--log "on-title-change: ws nil for buffer %s, transition %s skipped"
                              (buffer-name) transition)
          (claude-repl--log "title transition=%s ws=%s" transition ws)
          (pcase transition
            ('started  (claude-repl--ws-set ws :thinking))
            ('finished (claude-repl--ws-clear ws :thinking)))
          (when (eq transition 'finished)
            (claude-repl--on-claude-finished ws))
          (claude-repl--update-all-workspace-states))))))


(after! vterm
  (advice-add 'vterm--set-title :before #'claude-repl--on-title-change))

;; Permission prompt detection via file-notify watcher.
;; A Claude Code Notification hook writes the CWD to a sentinel file;
;; we watch for it and set the permission state for the matching workspace.
(defun claude-repl--ws-for-dir (dir)
  "Return the workspace name for a Claude session rooted at DIR, or nil."
  (when-let* ((root (claude-repl--git-root dir))
              (hash (substring (md5 (claude-repl--path-canonical root)) 0 8))
              (buf (get-buffer (format "*claude-%s*" hash))))
    (claude-repl--workspace-for-buffer buf)))

(defun claude-repl--on-permission-notify (event)
  "Handle file-notify event for permission prompt sentinel file."
  (let ((action (nth 1 event))
        (file (nth 2 event)))
    (when (and (memq action '(created changed))
               (string-match-p "permission_prompt$" file))
      (let ((ws (claude-repl--ws-for-dir
                 (string-trim (with-temp-buffer
                                (insert-file-contents file)
                                (buffer-string))))))
        (claude-repl--log "permission notify ws=%s" ws)
        (unless ws
          (error "claude-repl--on-permission-notify: no workspace for dir in %s" file))
        (claude-repl--ws-set ws :permission)
        (delete-file file)))))

(require 'filenotify)
(let ((dir (expand-file-name "~/.claude/workspace-notifications")))
  (make-directory dir t)
  (file-notify-add-watch dir '(change) #'claude-repl--on-permission-notify))

(defun claude-repl--do-refresh ()
  "Low-level refresh of the current vterm buffer.
Must be called with a vterm-mode buffer current."
  (let ((inhibit-read-only t))
    (when vterm--term
      (vterm--redraw vterm--term)))
  (redisplay t))

(defun claude-repl--fix-vterm-scroll (buf)
  "Briefly select the vterm window for BUF to fix Emacs scroll position."
  (let ((vterm-win (get-buffer-window buf))
        (orig-win (selected-window)))
    (when (and vterm-win (not (eq vterm-win orig-win)))
      (select-window vterm-win 'norecord)
      (select-window orig-win 'norecord))))

(defun claude-repl--refresh-vterm ()
  "Refresh the claude vterm display.
Works from any buffer or from within the vterm buffer itself."
  (let ((buf (if (eq major-mode 'vterm-mode)
                 (current-buffer)
               (let ((ws (+workspace-current-name)))
                 (and ws (claude-repl--ws-get ws :vterm-buffer))))))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (when (eq major-mode 'vterm-mode)
          (claude-repl--do-refresh)))
      (claude-repl--fix-vterm-scroll buf))))

;; Walk saved window-configuration tree to find claude buffers.
(defun claude-repl--wconf-has-claude-p (wconf)
  "Return non-nil if WCONF (a `window-state-get' tree) contains a claude buffer."
  (when (and wconf (proper-list-p wconf))
    (let ((buf-entry (alist-get 'buffer wconf)))
      (if (and buf-entry (stringp (car-safe buf-entry))
               (string-match-p "^\\*claude-[0-9a-f]+\\*$" (car buf-entry)))
          t
        (cl-some #'claude-repl--wconf-has-claude-p
                 (cl-remove-if-not #'proper-list-p wconf))))))

(defun claude-repl--ws-claude-open-p (ws-name)
  "Return non-nil if workspace WS-NAME has a claude buffer in its window layout.
For the current workspace, checks live windows.
For background workspaces, inspects the saved persp window configuration."
  (if (equal ws-name (+workspace-current-name))
      ;; Current workspace: check live windows
      (cl-some (lambda (buf)
                 (and (buffer-live-p buf)
                      (claude-repl--claude-buffer-p buf)
                      (get-buffer-window buf)))
               (buffer-list))
    ;; Background workspace: inspect saved window config
    (let* ((persp (persp-get-by-name ws-name))
           (wconf (and persp (not (symbolp persp)) (persp-window-conf persp))))
      (claude-repl--wconf-has-claude-p wconf))))

(defun claude-repl--update-ws-state (ws)
  "Update workspace WS state according to claude visibility and git status.
State table:
  :thinking  → unchanged (never touch)
  :done      + clean → :stale  |  :done      + dirty → :done
  :permission         → unchanged
  :stale     + dirty → :done   |  :stale     + clean → :stale
  nil        + dirty → :done   |  nil        + clean → nil"
  (let ((state (claude-repl--ws-state ws))
        (dirty (not (claude-repl--workspace-clean-p ws))))
    (pcase (cons state dirty)
      ;; :done + clean → :stale
      (`(:done . nil)
       (claude-repl--log "update-ws-state ws=%s :done->:stale" ws)
       (claude-repl--ws-clear ws :done)
       (claude-repl--touch-activity ws))
      ;; :stale + dirty → :done
      (`(:stale . t)
       (claude-repl--log "update-ws-state ws=%s :stale->:done" ws)
       (claude-repl--ws-clear ws :stale)
       (claude-repl--ws-set ws :done))
      ;; nil + dirty → :done
      (`(nil . t)
       (claude-repl--log "update-ws-state ws=%s nil->:done" ws)
       (claude-repl--ws-set ws :done))
      ;; :thinking, :permission, :done+dirty, :stale+clean, nil+clean → no-op
      (_ nil))))

(defun claude-repl--update-all-workspace-states ()
  "Update state for all workspaces based on claude visibility and git status.
Uses cached git status (`:git-clean') and kicks off async refreshes."
  (when (bound-and-true-p persp-mode)
    (dolist (ws (+workspace-list-names))
      (if (claude-repl--ws-claude-open-p ws)
          (progn
            (claude-repl--update-ws-state ws)
            (claude-repl--async-refresh-git-status ws))
        ;; Claude not open on this workspace → clear all state
        (let ((state (claude-repl--ws-state ws)))
          (when (and state (not (eq state :thinking)))
            (claude-repl--log "update-all: ws=%s clearing %s (claude not open)" ws state)
            (claude-repl--ws-clear ws state)))))))

(defun claude-repl--on-frame-focus ()
  "Refresh claude vterm and update all workspace states when Emacs regains focus."
  (when (frame-focus-state)
    (claude-repl--log "on-frame-focus")
    (claude-repl--refresh-vterm)
    (claude-repl--update-all-workspace-states)))

(add-function :after after-focus-change-function #'claude-repl--on-frame-focus)

;; Refresh vterm on workspace switch
(defun claude-repl--on-workspace-switch ()
  "Handle workspace switch: update all workspace states, refresh vterm, reset cursors.
Also opens panels for workspaces that were created with a preemptive prompt."
  (claude-repl--log "workspace-switch ws=%s" (+workspace-current-name))
  (claude-repl--update-all-workspace-states)
  (claude-repl--refresh-vterm)
  (claude-repl--reset-vterm-cursors)
  (let ((ws (+workspace-current-name)))
    (when (claude-repl--ws-get ws :pending-show-panels)
      (claude-repl--ws-put ws :pending-show-panels nil)
      (claude-repl))))

;; Save window state for current workspace before switching away,
;; so update-all-workspace-states can inspect the saved config.

(defun claude-repl--redirect-from-claude-before-save ()
  "If the selected window shows a Claude buffer, select a real window instead.
Called before persp saves window state so the saved config has a non-claude
buffer selected. This prevents Doom's `+workspace/kill' from trying to display
its fallback buffer in a dedicated window on workspace restore, which would
otherwise split the first real window (e.g. a repl) and show a doom buffer.
Skips redirect if claude is the only window (fullscreen case)."
  (let* ((sel (selected-window))
         (name (buffer-name (window-buffer sel)))
         (claude-p (or (string-match-p "^\\*claude-[0-9a-f]+\\*$" name)
                       (string-match-p "^\\*claude-input-[0-9a-f]+\\*$" name))))
    (when claude-p
      (when-let ((target (cl-find-if
                          (lambda (w)
                            (let ((n (buffer-name (window-buffer w))))
                              (not (or (string-match-p "^\\*claude-[0-9a-f]+\\*$" n)
                                       (string-match-p "^\\*claude-input-[0-9a-f]+\\*$" n)))))
                          (window-list))))
        (select-window target)))))

(when (modulep! :ui workspaces)
  (add-hook 'persp-before-deactivate-functions
            (lambda (&rest _)
              (claude-repl--redirect-from-claude-before-save)
              (ignore-errors (persp-frame-save-state))))
  (add-hook 'persp-activated-functions
            (lambda (&rest _)
              (run-at-time 0 nil #'claude-repl--on-workspace-switch))))

(defvar claude-repl--in-redraw-advice nil
  "Non-nil while the vterm redraw advice is executing, to prevent recursion.")

(defun claude-repl--after-vterm-redraw (&rest _)
  "Apply hide overlay after vterm redraws."
  (unless claude-repl--in-redraw-advice
    (let ((claude-repl--in-redraw-advice t))
      (when (claude-repl--claude-buffer-p)
        (claude-repl--update-hide-overlay)))))

(defun claude-repl--enable-hide-overlay ()
  "Enable the hide overlay advice.  Uses reference counting so
multiple sessions don't clobber each other."
  (claude-repl--log "enable-hide-overlay refcount=%d" claude-repl--hide-overlay-refcount)
  (when (zerop claude-repl--hide-overlay-refcount)
    (advice-add 'vterm--redraw :after #'claude-repl--after-vterm-redraw))
  (cl-incf claude-repl--hide-overlay-refcount))

(defun claude-repl--delete-hide-overlay ()
  "Delete the hide overlay if it exists and nil the variable."
  (when (and claude-repl-hide-overlay
             (overlay-buffer claude-repl-hide-overlay))
    (delete-overlay claude-repl-hide-overlay))
  (setq claude-repl-hide-overlay nil))

(defun claude-repl--disable-hide-overlay ()
  "Disable the hide overlay advice and clean up any existing overlay.
Uses reference counting so the advice is only removed when the last
session releases it."
  (cl-decf claude-repl--hide-overlay-refcount)
  (when (< claude-repl--hide-overlay-refcount 0)
    (setq claude-repl--hide-overlay-refcount 0))
  (claude-repl--log "disable-hide-overlay refcount=%d" claude-repl--hide-overlay-refcount)
  (when (zerop claude-repl--hide-overlay-refcount)
    (advice-remove 'vterm--redraw #'claude-repl--after-vterm-redraw))
  (claude-repl--delete-hide-overlay))

(defun claude-repl--vterm-running-p (&optional ws)
  "Return t if Claude vterm buffer for WS exists with a live process.
WS defaults to the current workspace name."
  (let ((buf (claude-repl--ws-get (or ws (+workspace-current-name)) :vterm-buffer)))
    (and buf (buffer-live-p buf) (get-buffer-process buf))))

(defun claude-repl--session-starting-p (&optional ws)
  "Return t if vterm exists with a live process but Claude is not yet ready.
WS defaults to the current workspace name."
  (let* ((ws (or ws (+workspace-current-name)))
         (buf (claude-repl--ws-get ws :vterm-buffer)))
    (and buf
         (buffer-live-p buf)
         (get-buffer-process buf)
         (not (buffer-local-value 'claude-repl--ready buf)))))

(defun claude-repl--cancel-ready-timer (ws)
  "Cancel the readiness-poll timer for workspace WS, if any."
  (when-let ((timer (claude-repl--ws-get ws :ready-timer)))
    (when (timerp timer) (cancel-timer timer))
    (claude-repl--ws-put ws :ready-timer nil)))

(defun claude-repl--schedule-ready-timer (ws)
  "Poll every 0.5s until Claude is ready in WS, then auto-open panels.
Gives up after 30s. This is a fallback — the title-change path is the happy path."
  (claude-repl--cancel-ready-timer ws)
  (let ((start-time (float-time)))
    (claude-repl--ws-put ws :ready-timer
      (run-at-time
       0.5 0.5
       (lambda ()
         (cond
          ((> (- (float-time) start-time) 30.0)
           (claude-repl--cancel-ready-timer ws)
           (claude-repl--log "ready-timer: timed out for ws=%s" ws))
          ((claude-repl--session-starting-p ws) nil)
          (t
           (claude-repl--cancel-ready-timer ws)
           (when (string= ws (+workspace-current-name))
             (claude-repl)))))))))

(defun claude-repl--input-visible-p ()
  "Return t if input buffer for the current workspace is visible in a window."
  (let ((buf (claude-repl--ws-get (+workspace-current-name) :input-buffer)))
    (and buf (buffer-live-p buf) (get-buffer-window buf))))

(defun claude-repl--vterm-visible-p ()
  "Return t if vterm buffer for the current workspace is visible in a window."
  (let ((buf (claude-repl--ws-get (+workspace-current-name) :vterm-buffer)))
    (and buf (buffer-live-p buf) (get-buffer-window buf))))

(defun claude-repl--panels-visible-p ()
  "Return t if both panels are visible."
  (and (claude-repl--input-visible-p)
       (claude-repl--vterm-visible-p)))

(defun claude-repl--hide-panels ()
  "Hide both Claude panels without killing buffers."
  (claude-repl--log "hide-panels")
  (let* ((ws (+workspace-current-name))
         (input-buf (claude-repl--ws-get ws :input-buffer))
         (vterm-buf (claude-repl--ws-get ws :vterm-buffer)))
    (when-let ((win (and input-buf (get-buffer-window input-buf))))
      (delete-window win))
    (when-let ((win (and vterm-buf (get-buffer-window vterm-buf))))
      (delete-window win))))

;; Auto-close orphaned panels: if one is closed, close the other.
;; Also refresh the hide overlay in case a window change invalidated it.
(defun claude-repl--orphaned-vterm-p (name)
  "Return non-nil if NAME is a claude vterm buffer with no matching input window."
  (and (string-match "^\\*claude-\\([0-9a-f]+\\)\\*$" name)
       (not claude-repl--fullscreen-config)
       (not (one-window-p))
       (not (get-buffer-window
             (format "*claude-input-%s*" (match-string 1 name))))))

(defun claude-repl--orphaned-input-p (name)
  "Return non-nil if NAME is a claude input buffer with no matching vterm window."
  (and (string-match "^\\*claude-input-\\([0-9a-f]+\\)\\*$" name)
       (not (one-window-p))
       (not (get-buffer-window
             (format "*claude-%s*" (match-string 1 name))))
       (not (get-buffer " *claude-loading*"))))

(defun claude-repl--sync-panels ()
  "Close any Claude panel whose partner is no longer visible."
  (dolist (win (window-list))
    (let ((name (buffer-name (window-buffer win))))
      (when (or (claude-repl--orphaned-vterm-p name)
                (claude-repl--orphaned-input-p name))
        (claude-repl--log "sync-panels closing orphaned %s" name)
        (delete-window win)))))

;; Keep visible Claude vterm buffers scrolled to the cursor.
;; Skips the selected window so clicking into vterm to read/copy isn't disrupted.
(defun claude-repl--refresh-vterm-window (win)
  "Refresh the Claude vterm buffer shown in WIN.
Resets cursor, redraws, and syncs window point."
  (let ((buf (window-buffer win)))
    (when (and buf (buffer-live-p buf) (claude-repl--claude-buffer-p buf))
      (with-current-buffer buf
        (when (and (eq major-mode 'vterm-mode)
                   (fboundp 'vterm-reset-cursor-point))
          (condition-case nil
              (let ((inhibit-read-only t))
                (vterm-reset-cursor-point)
                (when vterm--term
                  (vterm--redraw vterm--term))
                (vterm-reset-cursor-point)
                (set-window-point win (point)))
            (end-of-buffer nil)))))))

(defun claude-repl--reset-vterm-cursors ()
  "Refresh every visible Claude vterm window except the selected one."
  (let ((sel (selected-window)))
    (dolist (win (window-list))
      (unless (eq win sel)
        (claude-repl--refresh-vterm-window win)))))

(defvar claude-repl--sync-timer nil
  "Timer for debounced window-change handler.")

(defvar claude-repl--cursor-reset-timer nil
  "Timer for debounced cursor reset.")

(defun claude-repl--on-window-change ()
  "Deferred handler for window configuration changes.
Syncs orphaned panels, refreshes overlay, and resets cursors."
  (claude-repl--log-verbose "on-window-change")
  (condition-case nil
      (claude-repl--sync-panels)
    (error nil))
  (claude-repl--update-hide-overlay)
  (claude-repl--reset-vterm-cursors))

(defmacro claude-repl--deferred (timer-var fn)
  "Return a lambda that debounces calls to FN via TIMER-VAR."
  `(lambda (&rest _)
     (when ,timer-var
       (cancel-timer ,timer-var))
     (setq ,timer-var (run-at-time 0 nil ,fn))))

(add-hook 'window-configuration-change-hook
          (claude-repl--deferred claude-repl--sync-timer #'claude-repl--on-window-change))

(let ((debounced (claude-repl--deferred claude-repl--cursor-reset-timer #'claude-repl--reset-vterm-cursors)))
  (add-hook 'window-selection-change-functions debounced)
  (add-hook 'buffer-list-update-hook debounced))

;; Redirect focus from vterm output to input buffer (keyboard only, not mouse)
(defun claude-repl--redirect-to-input (_frame)
  "If the selected window shows a Claude vterm buffer, jump to its input window.
Only redirects for keyboard navigation; mouse clicks are allowed through
so the user can select and copy text from the output."
  (unless (mouse-event-p last-input-event)
    (let ((name (buffer-name (window-buffer (selected-window)))))
      (when (string-match "^\\*claude-\\([0-9a-f]+\\)\\*$" name)
        (let* ((id (match-string 1 name))
               (input-buf (get-buffer (format "*claude-input-%s*" id))))
          (when-let ((input-win (and input-buf (get-buffer-window input-buf))))
            (claude-repl--log "redirect-to-input from %s" name)
            (select-window input-win)
            (when (bound-and-true-p evil-mode)
              (evil-insert-state))))))))

(add-hook 'window-selection-change-functions #'claude-repl--redirect-to-input)

(defun claude-repl--ensure-input-buffer (ws)
  "Create input buffer for workspace WS if needed, put in claude-input-mode."
  (claude-repl--log "ensure-input-buffer")
  (let* ((root (claude-repl--resolve-root))
         (input-buf (get-buffer-create (claude-repl--buffer-name "-input"))))
    (claude-repl--ws-put ws :input-buffer input-buf)
    (with-current-buffer input-buf
      (setq-local claude-repl--project-root root)
      (unless (eq major-mode 'claude-input-mode)
        (claude-input-mode)
        (claude-repl--history-restore)))))

(defun claude-repl--kill-stale-vterm ()
  "Kill the Claude vterm buffer if it exists but has no live process."
  (when-let ((existing (get-buffer (claude-repl--buffer-name))))
    (unless (get-buffer-process existing)
      (claude-repl--log "killing stale vterm %s" (buffer-name existing))
      (kill-buffer existing))))

(defun claude-repl--ensure-vterm-buffer (ws)
  "Create vterm buffer for workspace WS running claude if needed. Starts claude from the git root."
  (let* ((root (claude-repl--resolve-root))
         (default-directory root))
    (claude-repl--log "ensure-vterm-buffer ws=%s root=%s default-directory=%s" ws root default-directory)
    (claude-repl--kill-stale-vterm)
    (let ((vterm-buf (get-buffer-create (claude-repl--buffer-name))))
      (claude-repl--ws-put ws :vterm-buffer vterm-buf)
      (with-current-buffer vterm-buf
        (if (eq major-mode 'vterm-mode)
            (progn
              (setq-local claude-repl--project-root root)
              (setq-local claude-repl--owning-workspace ws)
              (message "[claude-repl] ensure-vterm REUSING existing buffer %s for ws=%s (no --start-claude)" (buffer-name vterm-buf) ws)
              (claude-repl--log "ensure-vterm reusing existing buffer %s" (buffer-name vterm-buf)))
          (vterm-mode)
          (setq-local claude-repl--project-root root)
          (setq-local claude-repl--owning-workspace ws)
          (setq-local truncate-lines nil)
          (setq-local word-wrap t)
          (claude-repl--set-buffer-background 15)
          (claude-repl--log "ensure-vterm created %s root=%s" (buffer-name vterm-buf) root)
          (claude-repl--start-claude))))))

(defun claude-repl--show-panels-with-placeholder ()
  "Show panels using a loading placeholder in the vterm slot.
The placeholder is swapped for the real vterm buffer once Claude is ready."
  (claude-repl--log "show-panels-with-placeholder")
  (let* ((ws (+workspace-current-name))
         (real-vterm (claude-repl--ws-get ws :vterm-buffer))
         (placeholder (get-buffer-create " *claude-loading*")))
    (with-current-buffer placeholder
      (setq-local mode-line-format nil)
      (claude-repl--set-buffer-background 15))
    (claude-repl--ws-put ws :vterm-buffer placeholder)
    (claude-repl--show-panels)
    (claude-repl--focus-input-panel)
    (claude-repl--ws-put ws :vterm-buffer real-vterm)))

(defun claude-repl--start-fresh ()
  "Start a new Claude session with placeholder panels."
  (claude-repl--log "start-fresh")
  (let ((ws (+workspace-current-name)))
    (unless ws (error "claude-repl--start-fresh: no active workspace"))
    (claude-repl--touch-activity ws)
    (claude-repl--ws-put ws :saved-window-config (current-window-configuration)))
  (delete-other-windows (claude-repl--live-return-window))
  (claude-repl--ensure-session)
  (claude-repl--show-panels-with-placeholder)
  (let* ((ws (+workspace-current-name))
         (start-cmd (claude-repl--ws-get ws :start-cmd)))
    (message "Starting Claude... ws=%s ws-id=%s dir=%s cmd=%s"
             ws
             (claude-repl--workspace-id)
             (claude-repl--resolve-root)
             (or start-cmd "?"))))

(defun claude-repl--show-existing-panels ()
  "Show panels for an already-running Claude session.
Demotes indicators, refreshes display, and restores panel layout."
  (claude-repl--log "show-existing-panels")
  (let ((ws (+workspace-current-name)))
    (unless ws (error "claude-repl--show-existing-panels: no active workspace"))
    (claude-repl--touch-activity ws)
    (claude-repl--refresh-vterm)
    (claude-repl--ws-put ws :saved-window-config (current-window-configuration))
    (delete-other-windows (claude-repl--live-return-window))
    (claude-repl--ensure-input-buffer ws)
    (claude-repl--show-panels)
    (claude-repl--focus-input-panel)
    (claude-repl--update-hide-overlay)))

;; Entry point - smart toggle
(defun claude-repl ()
  "Toggle Claude REPL panels.
If text is selected: send it directly to Claude.
If not running: start Claude and show both panels.
If panels visible: hide both panels.
If panels hidden: show both panels."
  (interactive)
  (claude-repl--remember-return-window)
  (let ((vterm-running (claude-repl--vterm-running-p))
        (session-starting (claude-repl--session-starting-p))
        (panels-visible (claude-repl--panels-visible-p))
        (selection (when (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end)))))
    (claude-repl--log "claude-repl running=%s starting=%s visible=%s selection=%s"
                      vterm-running session-starting panels-visible (if selection "yes" "no"))
    (cond
     ;; Text selected - send directly to Claude
     (selection
      (deactivate-mark)
      (claude-repl--send-to-claude selection))
     ;; Nothing running - start fresh with placeholder until Claude is ready
     ((not vterm-running)
      (claude-repl--start-fresh))
     ;; Vterm alive but Claude not yet ready - hold off, panels will open automatically
     (session-starting
      (message "Claude is loading…"))
     ;; Panels visible - hide both, restore window layout
     (panels-visible
      (let ((ws (+workspace-current-name)))
        (unless ws (error "claude-repl: no active workspace when hiding panels"))
        (claude-repl--ws-clear ws :thinking)
        (claude-repl--ws-clear ws :done)
        (claude-repl--ws-clear ws :permission)
        (claude-repl--ws-clear ws :stale))
      (claude-repl--restore-layout))
     ;; Panels hidden - show both
     (t
      (claude-repl--show-existing-panels)))))

(defun claude-repl--close-buffer-windows (&rest bufs)
  "Close windows displaying any of BUFS."
  (claude-repl--log "close-buffer-windows %s" (mapcar (lambda (b) (and b (buffer-name b))) bufs))
  (dolist (buf bufs)
    (when (and buf (buffer-live-p buf))
      (when-let ((win (get-buffer-window buf)))
        (ignore-errors (delete-window win))))))

(defun claude-repl--kill-placeholder ()
  "Close and kill the loading placeholder buffer if it exists."
  (claude-repl--log "kill-placeholder exists=%s" (if (get-buffer " *claude-loading*") "yes" "no"))
  (when-let ((placeholder (get-buffer " *claude-loading*")))
    (when-let ((win (get-buffer-window placeholder)))
      (ignore-errors (delete-window win)))
    (kill-buffer placeholder)))

(defun claude-repl--schedule-sigkill (proc)
  "Schedule a SIGKILL for PROC after 0.5s if it's still alive."
  (run-at-time 0.5 nil
               (lambda ()
                 (when (process-live-p proc)
                   (claude-repl--log "sigkill fallback for lingering process")
                   (signal-process proc 'SIGKILL)))))

(defun claude-repl--kill-vterm-process (buf)
  "Kill the vterm buffer BUF and its process."
  (claude-repl--log "kill-vterm-process buf=%s" (and buf (buffer-name buf)))
  (when (and buf (buffer-live-p buf))
    (let ((proc (get-buffer-process buf)))
      (when proc
        (set-process-query-on-exit-flag proc nil))
      (kill-buffer buf)
      (when proc
        (claude-repl--schedule-sigkill proc)))))

(defun claude-repl--teardown-session-state (ws)
  "Save history, disable overlay, cancel timers, and clear session state for workspace WS."
  (message "[claude-repl] teardown-session-state ws=%s (setting :had-session t)" ws)
  (claude-repl--log "teardown-session-state")
  (ignore-errors (claude-repl--history-save ws))
  (ignore-errors (claude-repl--disable-hide-overlay))
  (when claude-repl--sync-timer
    (cancel-timer claude-repl--sync-timer)
    (setq claude-repl--sync-timer nil))
  (claude-repl--ws-put ws :vterm-buffer nil)
  (claude-repl--ws-put ws :input-buffer nil)
  (claude-repl--ws-put ws :saved-window-config nil)
  (claude-repl--ws-put ws :start-cmd nil)
  (claude-repl--ws-put ws :had-session t))

(defun claude-repl--destroy-session-buffers (vterm-buf input-buf)
  "Close windows and kill VTERM-BUF, INPUT-BUF, and any placeholder."
  (claude-repl--log "destroy-session-buffers")
  (claude-repl--close-buffer-windows vterm-buf input-buf)
  (claude-repl--kill-placeholder)
  (claude-repl--kill-vterm-process vterm-buf)
  (when (and input-buf (buffer-live-p input-buf))
    (kill-buffer input-buf)))

(defun claude-repl-kill ()
  "Kill Claude REPL buffers and windows for the current workspace."
  (interactive)
  (claude-repl--log "kill")
  (let* ((ws (+workspace-current-name))
         (_ (claude-repl--cancel-ready-timer ws))
         (vterm-buf (claude-repl--ws-get ws :vterm-buffer))
         (input-buf (claude-repl--ws-get ws :input-buffer))
         (saved-wconf (claude-repl--ws-get ws :saved-window-config))
         (ret-win (claude-repl--ws-get ws :return-window)))
    (unless ws (error "claude-repl-kill: no active workspace"))
    (claude-repl--ws-put ws :status nil)
    (claude-repl--ws-put ws :activity-time nil)
    (force-mode-line-update t)
    (claude-repl--teardown-session-state ws)
    (if saved-wconf
        (progn
          (set-window-configuration saved-wconf)
          (claude-repl--destroy-session-buffers vterm-buf input-buf))
      (claude-repl--destroy-session-buffers vterm-buf input-buf)
      (when (and ret-win (window-live-p ret-win))
        (select-window ret-win)))))

(defun claude-repl-restart ()
  "Kill Claude REPL and restart with `claude -c` to continue session."
  (interactive)
  (claude-repl--log "restart")
  (let ((ws (+workspace-current-name)))
    (claude-repl-kill)
    (claude-repl--ensure-vterm-buffer ws)
    (claude-repl--ensure-input-buffer ws)
    (claude-repl--enable-hide-overlay)
    (claude-repl--show-panels)
    (claude-repl--focus-input-panel)))

(defun claude-repl-focus-input ()
  "Focus the Claude input buffer, or return to previous window if already there.
If Claude isn't running, start it (same as `claude-repl')."
  (interactive)
  (let ((ws (+workspace-current-name)))
    (cond
     ;; Already in the input buffer — jump back
     ((eq (current-buffer) (claude-repl--ws-get ws :input-buffer))
      (claude-repl--log "focus-input branch=jump-back")
      (let ((ret (claude-repl--ws-get ws :return-window)))
        (if (and ret (window-live-p ret))
            (select-window ret)
          (evil-window-left 1))))
     ;; Not running — start fresh
     ((not (claude-repl--vterm-running-p))
      (claude-repl--log "focus-input branch=start-fresh")
      (claude-repl))
     ;; Running but panels hidden — show them
     (t
      (claude-repl--log "focus-input branch=show-or-focus")
      (unless (claude-repl--panels-visible-p)
        (claude-repl--ws-put ws :saved-window-config (current-window-configuration))
        (claude-repl--ensure-input-buffer ws)
        (claude-repl--show-panels))
      (claude-repl--ws-put ws :return-window (selected-window))
      (when-let ((win (get-buffer-window (claude-repl--ws-get ws :input-buffer))))
        (select-window win)
        (when (bound-and-true-p evil-mode)
          (evil-insert-state)))))))

(defun claude-repl-toggle-fullscreen ()
  "Toggle fullscreen for the Claude REPL vterm and input windows.
Saves the current window configuration per-workspace and expands the
Claude panels to fill the frame.  Calling again restores the layout."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (saved (claude-repl--ws-get ws :fullscreen-config)))
    (claude-repl--log "toggle-fullscreen ws=%s currently=%s" ws (if saved "fullscreen" "normal"))
    (cond
     ;; Already fullscreen — restore
     (saved
      (set-window-configuration saved)
      (claude-repl--ws-put ws :fullscreen-config nil))
     ;; Not fullscreen — go fullscreen if panels are visible
     ((claude-repl--vterm-live-p)
      (let* ((vterm-buf (claude-repl--ws-get ws :vterm-buffer))
             (input-buf (claude-repl--ws-get ws :input-buffer)))
        (unless (and vterm-buf input-buf
                     (get-buffer-window vterm-buf)
                     (get-buffer-window input-buf))
          (user-error "Claude REPL panels are not visible"))
        (claude-repl--ws-put ws :fullscreen-config (current-window-configuration))
        (dolist (win (window-list))
          (unless (memq (window-buffer win) (list vterm-buf input-buf))
            (ignore-errors (delete-window win))))))
     (t (message "No Claude vterm buffer for this workspace.")))))

(defun claude-repl-cycle ()
  "Send backtab to Claude vterm to cycle through options."
  (interactive)
  (claude-repl--log "cycle")
  (when (claude-repl--vterm-live-p)
    (with-current-buffer (claude-repl--ws-get (+workspace-current-name) :vterm-buffer)
      (vterm-send-key "<backtab>"))))

;; Global bindings
(map! :nvi "C-S-m" #'claude-repl-cycle)
(map! :i "C-S-f" #'claude-repl-toggle-fullscreen)
(map! :leader :prefix "w" :n "c" #'claude-repl-toggle-fullscreen)

(defun claude-repl-copy-reference ()
  "Copy the current file and line reference to the clipboard.
With active region: copies file:startline-endline.
Without region: copies file:line."
  (interactive)
  (let* ((rel (claude-repl--rel-path))
         (ref (if (use-region-p)
                  (let ((start-line (line-number-at-pos (region-beginning)))
                        (end-line (line-number-at-pos (region-end))))
                    (deactivate-mark)
                    (format "%s:%d-%d" rel start-line end-line))
                (format "%s:%d" rel (line-number-at-pos (point))))))
    (kill-new ref)
    (message "Copied: %s" ref)))

;; Workspace merge

(defun +dwc/workspace-merge--fork (project-root target-branch)
  "Compute cherry-pick start point for incorporating TARGET-BRANCH into HEAD.
Scans HEAD's unique commits (HEAD...TARGET-BRANCH left-only) for -x annotations
of the form \"(cherry picked from commit SHA)\". Returns the most recent TARGET
commit whose SHA appears in those annotations — so only genuinely new commits are
replayed. Falls back to `merge-base HEAD TARGET-BRANCH' when no annotations match
(first-time merge, or pre-annotation history)."
  (let* ((target-commits
          (split-string
           (string-trim
            (shell-command-to-string
             (format "git -C %s log --right-only --pretty=%%H --no-merges HEAD...%s"
                     (shell-quote-argument project-root)
                     target-branch)))
           "\n" t))
         (head-log
          (shell-command-to-string
           (format "git -C %s log --left-only --pretty=%%B HEAD...%s"
                   (shell-quote-argument project-root)
                   target-branch)))
         (incorporated
          (let (shas)
            (with-temp-buffer
              (insert head-log)
              (goto-char (point-min))
              (while (re-search-forward
                      "(cherry picked from commit \\([0-9a-f]\\{40\\}\\))"
                      nil t)
                (push (match-string 1) shas)))
            shas)))
    (or (cl-find-if (lambda (sha) (member sha incorporated)) target-commits)
        (string-trim
         (shell-command-to-string
          (format "git -C %s merge-base HEAD %s"
                  (shell-quote-argument project-root)
                  target-branch))))))

(defun +dwc/workspace->branch (ws)
  "Return the git branch checked out in workspace WS's worktree, or nil.
Workspace name ≠ branch name: e.g. persp \"fix-login\" was created from
\"DWC/fix-login\", so the branch is \"DWC/fix-login\" but the persp is \"fix-login\".
Resolves via :worktree-path stored in `claude-repl--workspaces'; falls back to
buffer-scanning via `claude-repl--ws-dir' for session-restored workspaces."
  (let ((path (or (claude-repl--ws-get ws :worktree-path)
                  (claude-repl--ws-dir ws))))
    (when path
      (let ((branch (string-trim
                     (shell-command-to-string
                      (format "git -C %s rev-parse --abbrev-ref HEAD"
                              (shell-quote-argument path))))))
        (unless (or (string-empty-p branch) (string-prefix-p "fatal" branch))
          branch)))))

(defun +dwc/workspace-merge ()
  "Cherry-pick another workspace's branch commits onto the current branch.
Replays each commit from the target branch (since it diverged from master)
individually. Aborts cleanly if any commit conflicts."
  (interactive)
  (let* ((current-ws (+workspace-current-name))
         (other-ws (remove current-ws (+workspace-list-names))))
    (unless other-ws
      (user-error "No other workspaces to merge"))
    ;; Guard: uncommitted changes would interfere with cherry-pick.
    (let ((project-root (or (projectile-project-root) (user-error "Not in a project"))))
      (unless (and (= 0 (call-process "git" nil nil nil "-C" project-root "diff" "--quiet"))
                   (= 0 (call-process "git" nil nil nil "-C" project-root "diff" "--cached" "--quiet")))
        (user-error "Uncommitted changes present — stash or commit them before merging a workspace")))
    (let* ((target-ws (completing-read "Merge workspace into current: " other-ws nil t))
           (target-branch (+dwc/workspace->branch target-ws)))
      (unless target-branch
        (user-error "Cannot resolve branch for workspace '%s'" target-ws))
      (let* ((project-root (or (projectile-project-root)
                               (user-error "Not in a project"))))
        (unless (= 0 (call-process "git" nil nil nil
                                   "-C" project-root
                                   "rev-parse" "--verify" target-branch))
          (user-error "Branch '%s' not found in this repo" target-branch))
        (let* ((fork (+dwc/workspace-merge--fork project-root target-branch))
               (range (format "%s..%s" fork target-branch)))
          (let ((range-count (string-trim
                              (shell-command-to-string
                               (format "git -C %s rev-list --count %s"
                                       (shell-quote-argument project-root)
                                       range)))))
            (when (string= range-count "0")
              (user-error "All commits from workspace '%s' are already incorporated" target-ws)))
          (claude-repl--log "workspace-merge target-ws=%s target-branch=%s fork=%s range=%s"
                            target-ws target-branch fork range)
          (let ((exit-code (call-process "git" nil nil nil "-C" project-root "cherry-pick" "-x" range)))
            (claude-repl--log "workspace-merge cherry-pick exit-code=%s" exit-code))
          ;; Non-zero exit doesn't always mean conflict — git also exits non-zero for
          ;; empty commits (already applied). Only abort if CHERRY_PICK_HEAD exists,
          ;; which git writes only when stopped mid-way on a real conflict.
          (let* ((git-dir (string-trim (shell-command-to-string
                                        (format "git -C %s rev-parse --git-dir"
                                                (shell-quote-argument project-root)))))
                 (cherry-pick-head (expand-file-name "CHERRY_PICK_HEAD" git-dir))
                 (head-exists (file-exists-p cherry-pick-head)))
            (claude-repl--log "workspace-merge git-dir=%s cherry-pick-head=%s exists=%s"
                              git-dir cherry-pick-head head-exists)
            (when head-exists
              (let ((conflicting-commit (string-trim
                                         (shell-command-to-string
                                          (format "git -C %s rev-parse --short CHERRY_PICK_HEAD"
                                                  (shell-quote-argument project-root))))))
                (magit-status)
                (user-error "Conflict cherry-picking %s from '%s' — resolve in magit" conflicting-commit target-ws))))
          (when (member target-ws (+workspace-list-names))
            ;; Kill Claude process first to suppress vterm's "process running" prompt.
            (claude-repl--kill-vterm-process (claude-repl--ws-get target-ws :vterm-buffer))
            (persp-kill target-ws))
          (message "Merged workspace '%s' → '%s'." target-ws current-ws)
          (magit-status))))))

;; Keybindings
;; SPC o — Claude session control (open, focus, kill, interrupt, utilities)
(map! :leader
      :desc "Claude REPL" "o c" #'claude-repl
      :desc "Claude input" "o v" #'claude-repl-focus-input
      :desc "Kill Claude" "o C" #'claude-repl-kill
      :desc "Claude interrupt" "o x" #'claude-repl-interrupt
      :desc "Copy file reference" "o r" #'claude-repl-copy-reference)

(map! :leader
      (:prefix "p"
       :desc "Create worktree workspace" "w" #'claude-repl-create-worktree-workspace))

(map! :leader
      (:prefix "TAB"
       :desc "Create worktree workspace" "n" #'claude-repl-create-worktree-workspace
       :desc "New workspace"             "N" #'+workspace/new
       :desc "Merge workspace into current" "m" #'+dwc/workspace-merge))

;; SPC j — Tell Claude to do a predefined thing
(map! :leader
      (:prefix ("j" . "claude")
       :desc "Explain line/region/hunk"      "e" #'claude-repl-explain
       :desc "Update GitHub PR description"  "r" #'claude-repl-update-pr
       (:prefix ("E" . "explain diff")
        :desc "worktree"    "w" #'claude-repl-explain-diff-worktree
        :desc "staged"      "s" #'claude-repl-explain-diff-staged
        :desc "uncommitted" "u" #'claude-repl-explain-diff-uncommitted
        :desc "HEAD"        "h" #'claude-repl-explain-diff-head
        :desc "branch"      "b" #'claude-repl-explain-diff-branch)
       (:prefix ("R" . "update PR diff")
        :desc "worktree"    "w" #'claude-repl-update-pr-diff-worktree
        :desc "staged"      "s" #'claude-repl-update-pr-diff-staged
        :desc "uncommitted" "u" #'claude-repl-update-pr-diff-uncommitted
        :desc "HEAD"        "h" #'claude-repl-update-pr-diff-head
        :desc "branch"      "b" #'claude-repl-update-pr-diff-branch)
       (:prefix ("t" . "tests")
        (:prefix ("r" . "run")
         (:prefix ("t" . "tests")
          :desc "worktree"    "w" #'claude-repl-run-tests-worktree
          :desc "staged"      "s" #'claude-repl-run-tests-staged
          :desc "uncommitted" "u" #'claude-repl-run-tests-uncommitted
          :desc "HEAD"        "h" #'claude-repl-run-tests-head
          :desc "branch"      "b" #'claude-repl-run-tests-branch)
         (:prefix ("l" . "lint")
          :desc "worktree"    "w" #'claude-repl-run-lint-worktree
          :desc "staged"      "s" #'claude-repl-run-lint-staged
          :desc "uncommitted" "u" #'claude-repl-run-lint-uncommitted
          :desc "HEAD"        "h" #'claude-repl-run-lint-head
          :desc "branch"      "b" #'claude-repl-run-lint-branch)
         (:prefix ("a" . "all")
          :desc "worktree"    "w" #'claude-repl-run-all-worktree
          :desc "staged"      "s" #'claude-repl-run-all-staged
          :desc "uncommitted" "u" #'claude-repl-run-all-uncommitted
          :desc "HEAD"        "h" #'claude-repl-run-all-head
          :desc "branch"      "b" #'claude-repl-run-all-branch))
        (:prefix ("a" . "analyze")
         (:prefix ("q" . "quality")
          :desc "worktree"    "w" #'claude-repl-test-quality-worktree
          :desc "staged"      "s" #'claude-repl-test-quality-staged
          :desc "uncommitted" "u" #'claude-repl-test-quality-uncommitted
          :desc "HEAD"        "h" #'claude-repl-test-quality-head
          :desc "branch"      "b" #'claude-repl-test-quality-branch)
         (:prefix ("c" . "coverage")
          :desc "worktree"    "w" #'claude-repl-test-coverage-worktree
          :desc "staged"      "s" #'claude-repl-test-coverage-staged
          :desc "uncommitted" "u" #'claude-repl-test-coverage-uncommitted
          :desc "HEAD"        "h" #'claude-repl-test-coverage-head
          :desc "branch"      "b" #'claude-repl-test-coverage-branch)))))

(dotimes (i 10)
  (let ((char (number-to-string i)))
    (define-key doom-leader-map (kbd (format "o %s" char))
      (lambda () (interactive) (claude-repl-send-char char)))))

;; FIXME: fix magit handling

;; C-v paste forwarding to vterm
(defun claude-repl-paste-to-vterm ()
  "Forward a Ctrl-V keystroke to the Claude vterm buffer.
This lets Claude CLI handle paste natively, including images."
  (interactive)
  (when (claude-repl--vterm-live-p)
    (with-current-buffer (claude-repl--ws-get (+workspace-current-name) :vterm-buffer)
      (vterm-send-key "v" nil nil t))))

;;; Debug helpers — interactive commands for diagnosing workspace state issues.
;;; Call via M-x claude-repl-debug/...

(defun claude-repl-debug/mock-workspace-generation (&optional names)
  "Write a mock workspace_generation.json to trigger the file watcher.
NAMES is an optional list of branch name strings; defaults to a single test entry."
  (interactive)
  (let ((file (expand-file-name "~/.claude/output/workspace_generation.json"))
        (names (or names '("DWC/mock-test"))))
    (make-directory (expand-file-name "~/.claude/output/") t)
    (with-temp-file file
      (insert (json-encode names)))
    (claude-repl--log "mock workspace-generation file written: %s names=%s" file names)
    (message "Wrote mock workspace_generation.json: %s" names)))

(defun claude-repl-debug/process-pending-commands ()
  "Manually scan ~/.claude/output/ and process any workspace_commands_*.json files.
Use this to verify the processor works independently of the file watcher."
  (interactive)
  (let* ((dir (expand-file-name "~/.claude/output/"))
         (files (when (file-directory-p dir)
                  (directory-files dir t "^workspace_commands_.*\\.json$"))))
    (if (not files)
        (message "No workspace_commands_*.json files found in %s" dir)
      (message "Found %d file(s), processing..." (length files))
      (dolist (file files)
        (message "Processing: %s" file)
        (claude-repl--process-workspace-commands-file file)))))

(defun claude-repl-debug/workspace-states ()
  "Display all workspace states."
  (interactive)
  (let ((states (mapcar (lambda (name)
                          (cons name (claude-repl--ws-state name)))
                        (+workspace-list-names))))
    (message "Workspace states:\n%s"
             (mapconcat (lambda (s) (format "  %s: %s" (car s) (or (cdr s) "nil")))
                        states "\n"))))

(defun claude-repl-debug/buffer-info ()
  "Display all claude vterm buffers with their owning and persp workspaces."
  (interactive)
  (let (result)
    (dolist (buf (buffer-list))
      (when (string-match-p "^\\*claude-[0-9a-f]+\\*$" (buffer-name buf))
        (push (format "  %s  owning=%s  persp=%s  thinking=%s"
                      (buffer-name buf)
                      (or (buffer-local-value 'claude-repl--owning-workspace buf) "nil")
                      (or (claude-repl--workspace-for-buffer buf) "nil")
                      (buffer-local-value 'claude-repl--title-thinking buf))
              result)))
    (message "Claude buffers:\n%s"
             (if result (mapconcat #'identity (nreverse result) "\n") "  (none)"))))

(defun claude-repl-debug/clear-state (ws)
  "Clear all states for workspace WS without killing buffers."
  (interactive
   (list (completing-read "Workspace: " (+workspace-list-names) nil t)))
  (claude-repl--ws-clear ws :thinking)
  (claude-repl--ws-clear ws :done)
  (claude-repl--ws-clear ws :permission)
  (claude-repl--ws-clear ws :stale)
  (message "Cleared all states for %s" ws))

(defun claude-repl-debug/obliterate (ws)
  "Completely remove workspace WS from all claude-repl tracking.
Kills claude buffers, closes windows, and removes all state."
  (interactive
   (list (completing-read "Obliterate workspace: " (+workspace-list-names) nil t)))
  ;; Find and destroy any claude buffers owned by this workspace
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (string-match-p "^\\*claude-\\(input-\\)?[0-9a-f]+\\*$" (buffer-name buf))
               (equal ws (buffer-local-value 'claude-repl--owning-workspace buf)))
      (when-let ((win (get-buffer-window buf)))
        (ignore-errors (delete-window win)))
      (let ((proc (get-buffer-process buf)))
        (when proc (set-process-query-on-exit-flag proc nil)))
      (kill-buffer buf)))
  ;; Remove all state for this workspace from the single hash table
  (claude-repl--ws-del ws)
  (message "Obliterated all claude-repl state for %s" ws))

(defun claude-repl-debug/set-owning-workspace ()
  "Set the owning workspace for a claude vterm buffer."
  (interactive)
  (let* ((bufs (cl-remove-if-not
                (lambda (b) (string-match-p "^\\*claude-[0-9a-f]+\\*$" (buffer-name b)))
                (buffer-list)))
         (buf-name (completing-read "Buffer: " (mapcar #'buffer-name bufs) nil t))
         (ws (completing-read "Owning workspace: " (+workspace-list-names) nil t)))
    (with-current-buffer buf-name
      (setq-local claude-repl--owning-workspace ws))
    (message "Set %s owning workspace to %s" buf-name ws)))

(defun claude-repl-debug/toggle-logging (&optional verbose)
  "Toggle debug logging.
With prefix argument (\\[universal-argument]), toggle verbose mode instead, which
additionally logs high-frequency events: window changes, title spinner,
resolve-root, and poll-thinking redraws."
  (interactive "P")
  (setq claude-repl-debug
        (if verbose
            (if (eq claude-repl-debug 'verbose) nil 'verbose)
          (if claude-repl-debug nil t)))
  (message "Claude REPL debug logging: %s"
           (pcase claude-repl-debug
             ('nil "OFF")
             ('t   "ON")
             ('verbose "ON (verbose)"))))

(defun claude-repl-debug/toggle-metaprompt ()
  "Toggle the metaprompt prefix injection."
  (interactive)
  (setq claude-repl-skip-permissions (not claude-repl-skip-permissions))
  (message "Claude REPL metaprompt: %s" (if claude-repl-skip-permissions "ON" "OFF")))

(defun claude-repl-debug/prefix-counter ()
  "Show the current metaprompt prefix counter, period, and workspace."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (counter (or (claude-repl--ws-get ws :prefix-counter) 0)))
    (message "[%s] Prefix counter: %d  period: %d  next metaprompt in: %d sends"
             ws counter claude-repl-prefix-period
             (- claude-repl-prefix-period (mod counter claude-repl-prefix-period)))))

(defun claude-repl--git-branch-for-workspace (ws-name)
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

(defun claude-repl--ws-base-name (ws-name)
  "Strip any existing branch suffix from WS-NAME.
E.g. \"my-project (main)\" → \"my-project\"."
  (if (string-match "\\(.+?\\) (.*?)$" ws-name)
      (match-string 1 ws-name)
    ws-name))

(defun claude-repl-debug/ws-set-branch (ws-name)
  "Rename workspace WS-NAME to include its current git branch.
Strips any existing branch suffix first, then appends \" (branch)\"."
  (interactive
   (list (completing-read "Workspace: " (+workspace-list-names) nil t
                          nil nil (+workspace-current-name))))
  (let* ((base (claude-repl--ws-base-name ws-name))
         (branch (claude-repl--git-branch-for-workspace ws-name))
         (new-name (if branch (format "%s (%s)" base branch) base)))
    (if (equal ws-name new-name)
        (message "Workspace %s: already up to date" ws-name)
      (+workspace-rename ws-name new-name)
      (message "Renamed: %s → %s" ws-name new-name))))

(defun claude-repl-debug/workspace-clean-p (ws-name)
  "Show whether workspace WS-NAME has unstaged changes to tracked files.
Uses `claude-repl--workspace-clean-p' — the same function used in production."
  (interactive
   (list (completing-read "Workspace: " (+workspace-list-names) nil t
                          nil nil (+workspace-current-name))))
  (let ((clean (claude-repl--workspace-clean-p ws-name)))
    (message "Workspace %s: %s" ws-name (if clean "clean" "dirty"))))

(defun claude-repl-debug/refresh-state (ws-name)
  "Force a full state refresh for workspace WS-NAME.
Runs the same logic as the periodic `update-all-workspace-states' timer:
checks claude visibility, git dirty status, and applies the state table.
Reports comprehensive diagnostics."
  (interactive
   (list (completing-read "Workspace: " (+workspace-list-names) nil t
                          nil nil (+workspace-current-name))))
  (let* ((before (claude-repl--ws-state ws-name))
         (open (claude-repl--ws-claude-open-p ws-name))
         (dirty (not (claude-repl--workspace-clean-p ws-name)))
         ;; Find the claude vterm buffer for this workspace
         (persp (persp-get-by-name ws-name))
         (persp-bufs (and persp (not (symbolp persp)) (persp-buffers persp)))
         (vterm-buf (cl-loop for buf in persp-bufs
                             when (and (buffer-live-p buf)
                                       (claude-repl--claude-buffer-p buf))
                             return buf))
         ;; Read buffer-local state from the vterm buffer
         (title-thinking (and vterm-buf
                              (buffer-local-value 'claude-repl--title-thinking vterm-buf)))
         (proc (and vterm-buf (get-buffer-process vterm-buf)))
         (proc-alive (and proc (process-live-p proc)))
         (owning-ws (and vterm-buf
                         (buffer-local-value 'claude-repl--owning-workspace vterm-buf)))
         (has-window (and vterm-buf (get-buffer-window vterm-buf t))))
    ;; Apply the state update
    (if open
        (claude-repl--update-ws-state ws-name)
      (let ((state (claude-repl--ws-state ws-name)))
        (when (and state (not (eq state :thinking)))
          (claude-repl--ws-clear ws-name state))))
    (let ((after (claude-repl--ws-state ws-name)))
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

;; Kill Claude session before workspace deletion so buffers/windows are cleaned
;; up while the workspace is still current.
(advice-add #'+workspace/kill :before
            (lambda (&rest _)
              (when (claude-repl--vterm-running-p)
                (claude-repl-kill))))

;; SPC b R — revert buffer from disk then eval as Elisp (fast config reload)
(defun claude-repl-revert-and-eval-buffer ()
  "Revert the current buffer from disk, then evaluate it as Elisp."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm)
  (eval-buffer))

(map! :leader "b R" #'claude-repl-revert-and-eval-buffer)

(provide 'claude-repl)
;;; config.el ends here
