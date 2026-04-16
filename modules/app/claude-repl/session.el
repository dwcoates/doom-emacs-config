;;; session.el --- session lifecycle management -*- lexical-binding: t; -*-

;;; Code:

;;;; Session readiness

(defvar-local claude-repl--ready nil
  "Non-nil once Claude Code has set its terminal title, indicating startup is complete.")

;;;; Sandbox configuration

(defcustom claude-repl-docker-image ""
  "Fallback Docker image for sandboxed worktree workspaces with no .claude/sandbox/image.
Prefer per-repo .claude/sandbox/image files over this global setting.
If empty (the default), worktrees without a .claude/sandbox/image run Claude directly."
  :type 'string
  :group 'claude-repl)

(defun claude-repl--docker-image-exists-p (image)
  "Return non-nil if IMAGE exists in the local Docker image store."
  (let ((result (= 0 (call-process "docker" nil nil nil "image" "inspect" "--format" "." image))))
    (claude-repl--log nil "docker-image-exists-p: image=%s exists=%s" image (if result "yes" "no"))
    result))

(defun claude-repl--find-sandbox-script (git-root)
  "Return the path to the sandbox launcher script for GIT-ROOT, or nil.
Checks for `claude-sandbox' on PATH first, then falls back to
`.agents-sandbox/sandbox' inside the repository."
  (let ((result (or (when-let ((p (executable-find "claude-sandbox")))
                      (claude-repl--log nil "find-sandbox-script: found claude-sandbox on PATH at %s" p)
                      p)
                    (let ((f (expand-file-name ".agents-sandbox/sandbox" git-root)))
                      (if (file-executable-p f)
                          (progn
                            (claude-repl--log nil "find-sandbox-script: found .agents-sandbox/sandbox at %s" f)
                            f)
                        nil)))))
    (unless result
      (claude-repl--log nil "find-sandbox-script: no sandbox script found for git-root=%s" git-root))
    result))

(defun claude-repl--query-sandbox-image (script)
  "Return the Docker image name reported by sandbox SCRIPT, or nil on failure.
Runs SCRIPT with --image-name and trims the output."
  (let ((image (string-trim
                (with-output-to-string
                  (with-current-buffer standard-output
                    (call-process script nil t nil "--image-name"))))))
    (if (string-empty-p image)
        (progn
          (claude-repl--log nil "query-sandbox-image: script=%s returned empty image" script)
          nil)
      (claude-repl--log nil "query-sandbox-image: script=%s image=%s" script image)
      image)))

(defun claude-repl--find-install-script (git-root)
  "Return the path to the sandbox install script in GIT-ROOT, or nil."
  (let ((f (expand-file-name ".agents-sandbox/install-claude.sh" git-root)))
    (if (file-executable-p f)
        (progn
          (claude-repl--log nil "find-install-script: found %s" f)
          f)
      (claude-repl--log nil "find-install-script: no install script in git-root=%s" git-root)
      nil)))

(defun claude-repl--resolve-sandbox-config (git-root)
  "Return a plist (:image IMAGE :script SCRIPT) for a worktree at GIT-ROOT.
Detects sandbox support by looking for the `claude-sandbox' launcher on PATH
or `.agents-sandbox/sandbox' in the repo.  Queries the launcher's --image-name
flag to determine the Docker image.
Returns nil if no sandbox launcher is found.
Returns (:needs-build t :install-script PATH) if the image is not built yet."
  (let ((script (claude-repl--find-sandbox-script git-root)))
    (if (null script)
        (progn
          (claude-repl--log nil "resolve-sandbox-config: no-launcher for git-root=%s" git-root)
          nil)
      (if-let ((image (claude-repl--query-sandbox-image script)))
          (if (claude-repl--docker-image-exists-p image)
              (progn
                (claude-repl--log nil "resolve-sandbox-config: success image=%s script=%s" image script)
                (list :image image :script script))
            (progn
              (claude-repl--log nil "resolve-sandbox-config: needs-build image=%s" image)
              (list :needs-build t
                    :image image
                    :install-script (claude-repl--find-install-script git-root))))
        (claude-repl--log nil "resolve-sandbox-config: empty-image from script=%s in git-root=%s" script git-root)
        nil))))

;;;; Workspace environment initialization

(defun claude-repl--ensure-ws-env (ws)
  "Initialize environment state for workspace WS if not already set.
Sets up bare-metal as the default :active-env, creates instantiation
structs for both :sandbox and :bare-metal, then restores any persisted
state from disk and verifies the session-id against live sessions."
  (claude-repl--log ws "ensure-ws-env: ws=%s" ws)
  (if (claude-repl--ws-get ws :active-env)
      (claude-repl--log ws "ensure-ws-env: restoring existing env for ws=%s" ws)
    (claude-repl--log ws "ensure-ws-env: initializing for first time ws=%s" ws)
    (claude-repl--ws-put ws :active-env :bare-metal)
    (claude-repl--ws-put ws :sandbox (make-claude-repl-instantiation))
    (claude-repl--ws-put ws :bare-metal (make-claude-repl-instantiation)))
  (claude-repl--state-restore ws)
  (claude-repl--capture-session-id ws))

(defun claude-repl--prompt-sandbox-build (sandbox-config)
  "Prompt the user to build a missing sandbox image from SANDBOX-CONFIG.
Signals `user-error' unconditionally -- either after kicking off the build
or telling the user to do it manually."
  (let ((image (plist-get sandbox-config :image))
        (install-script (plist-get sandbox-config :install-script)))
    (claude-repl--log nil "prompt-sandbox-build: image=%s install-script=%s" image install-script)
    (if install-script
        (when (y-or-n-p (format "Sandbox image '%s' not built. Run install.sh now? " image))
          (compile (format "bash %s" install-script))
          (user-error "Run 'SPC o c' again once the build completes"))
      (user-error "Sandbox image '%s' not built — run .agents-sandbox/install-claude.sh manually" image))))

(defun claude-repl--ensure-sandbox-image (ws)
  "Ensure the sandbox Docker image for workspace WS is available.
Returns a sandbox-config plist from `claude-repl--resolve-sandbox-config',
or nil if sandboxing is not applicable.  Signals `user-error' if the image
needs building, optionally kicking off the build first."
  (let* ((worktree-p (claude-repl--ws-get ws :worktree-p))
         (active-env (claude-repl--ws-get ws :active-env))
         (project-dir (claude-repl--ws-get ws :project-dir))
         (sandbox-config (when (and worktree-p (eq active-env :sandbox))
                           (claude-repl--resolve-sandbox-config
                            (claude-repl--git-root project-dir)))))
    (claude-repl--log ws "ensure-sandbox-image: ws=%s worktree-p=%s env=%s config=%s"
                      ws (if worktree-p "yes" "no") active-env
                      (if sandbox-config "found" "nil"))
    (when (plist-get sandbox-config :needs-build)
      (claude-repl--prompt-sandbox-build sandbox-config))
    sandbox-config))

;;;; Command building

(defun claude-repl--compute-claude-flags (session-id fork-session-id perm-flag)
  "Build the CLI flags string for the Claude command.
SESSION-ID is the current session to resume, FORK-SESSION-ID is
a session to fork from, and PERM-FLAG is the permission flag string
or nil.  Returns a trimmed flags string."
  (let ((flags (string-trim
                (mapconcat #'identity
                           (delq nil (list
                                      ;; Fork from another session (worktree creation).
                                      (when fork-session-id
                                        (format "--resume %s --fork-session" fork-session-id))
                                      ;; Resume known session in this environment.
                                      (when (and (not fork-session-id) session-id)
                                        (format "--resume %s" session-id))
                                      perm-flag))
                           " "))))
    (claude-repl--log nil "compute-claude-flags: flags=%s" flags)
    flags))

(defun claude-repl--compute-perm-flag (sandboxed-p project-dir)
  "Return the permission flag string for the Claude CLI, or nil.
SANDBOXED-P means Docker handles permissions.  Otherwise, PROJECT-DIR
determines the flag: ChessCom repos use --permission-mode auto,
personal repos use --dangerously-skip-permissions."
  (if sandboxed-p
      (progn
        (claude-repl--log nil "compute-perm-flag: sandboxed — no perm flag")
        nil)
    (let ((flag (if (string-match-p "ChessCom" (expand-file-name (or project-dir default-directory)))
                    "--permission-mode auto"
                  "--dangerously-skip-permissions")))
      (claude-repl--log nil "compute-perm-flag: branch=%s flag=%s"
                        (if (string-match-p "ChessCom" (expand-file-name (or project-dir default-directory)))
                            "ChessCom" "personal")
                        flag)
      flag)))

(defun claude-repl--assemble-cmd (sandbox-config sandboxed-p claude-flags)
  "Assemble the final shell command string.
SANDBOX-CONFIG is the sandbox plist (may be nil), SANDBOXED-P indicates
Docker mode, and CLAUDE-FLAGS is the pre-built flags string."
  (let ((cmd (string-trim
              (if sandboxed-p
                  (concat (plist-get sandbox-config :script) " " claude-flags)
                (concat "claude " claude-flags)))))
    (claude-repl--log nil "assemble-cmd: cmd=%s" cmd)
    cmd))

(defun claude-repl--build-start-cmd (ws)
  "Build the shell command string to start Claude for workspace WS.
Returns a plist (:cmd CMD :sandboxed-p BOOL :docker-image IMAGE
:session-id ID :fork-session-id ID :worktree-p BOOL :active-env ENV :inst INST)
with everything the caller needs for logging and mode-line setup."
  (claude-repl--log ws "build-start-cmd: ws=%s" ws)
  (let* ((inst (claude-repl--active-inst ws))
         (session-id (claude-repl-instantiation-session-id inst))
         (worktree-p (claude-repl--ws-get ws :worktree-p))
         (project-dir (claude-repl--ws-get ws :project-dir))
         (active-env (claude-repl--ws-get ws :active-env))
         (fork-session-id (claude-repl--ws-get ws :fork-session-id))
         (sandbox-config (claude-repl--ensure-sandbox-image ws))
         (docker-image (and (not (plist-get sandbox-config :needs-build))
                            (plist-get sandbox-config :image)))
         (sandboxed-p (and worktree-p docker-image))
         (perm-flag (claude-repl--compute-perm-flag sandboxed-p project-dir))
         (claude-flags (claude-repl--compute-claude-flags session-id fork-session-id perm-flag))
         (cmd (claude-repl--assemble-cmd sandbox-config sandboxed-p claude-flags)))
    (list :cmd cmd
          :sandboxed-p sandboxed-p
          :docker-image docker-image
          :session-id session-id
          :fork-session-id fork-session-id
          :worktree-p worktree-p
          :active-env active-env
          :inst inst)))

;;;; Session startup

(defun claude-repl--sandbox-mode-line (sandboxed-p docker-image)
  "Return a mode-line format list indicating the execution environment.
SANDBOXED-P means Docker mode with DOCKER-IMAGE; otherwise bare metal."
  (list (if sandboxed-p
            (propertize (format " DOCKER SANDBOX: %s" docker-image)
                        'face '(:foreground "green" :weight bold))
          (propertize (format " BARE METAL: %s" (system-name))
                      'face '(:foreground "red" :weight bold)))))

(defun claude-repl--log-session-start (ws start-info)
  "Log session startup details for workspace WS from START-INFO plist."
  (let ((cmd             (plist-get start-info :cmd))
        (session-id      (plist-get start-info :session-id))
        (fork-session-id (plist-get start-info :fork-session-id))
        (worktree-str    (if (plist-get start-info :worktree-p) "yes" "no"))
        (active-env      (plist-get start-info :active-env)))
    (claude-repl--log ws "start-claude ws=%s session-id=%s fork-session-id=%s worktree=%s env=%s cmd=%s dir=%s"
                      ws session-id fork-session-id worktree-str active-env cmd default-directory)))

(defun claude-repl--start-claude ()
  "Send the claude startup command to the current vterm buffer.
For worktree workspaces with :active-env :sandbox, delegates to
.claude/sandbox/claude-sandbox if a sandbox image is configured.
Falls back to bare-metal Claude otherwise."
  (let* ((ws (or claude-repl--owning-workspace (+workspace-current-name)))
         (start-info (progn
                       (claude-repl--ensure-ws-env ws)
                       (claude-repl--build-start-cmd ws)))
         (cmd         (plist-get start-info :cmd))
         (sandboxed-p (plist-get start-info :sandboxed-p))
         (inst        (plist-get start-info :inst)))
    (setf (claude-repl-instantiation-start-cmd inst) cmd)
    (when (plist-get start-info :fork-session-id)
      (claude-repl--log ws "start-claude: clearing fork-session-id for ws=%s" ws)
      (claude-repl--ws-put ws :fork-session-id nil))
    (setq-local mode-line-format
                (claude-repl--sandbox-mode-line sandboxed-p
                                               (plist-get start-info :docker-image)))
    (claude-repl--log-session-start ws start-info)
    (claude-repl--log ws "start-claude: setting ready=nil for ws=%s" ws)
    (setq-local claude-repl--ready nil)
    (vterm-send-string (concat "clear && " cmd))
    (vterm-send-return)
    (claude-repl--schedule-ready-timer ws)))

;;;; Loading placeholder

(defun claude-repl--swap-placeholder-into-windows (buf placeholder)
  "Replace PLACEHOLDER windows with BUF and kill PLACEHOLDER.
Only acts when BUF is still live.  Each window showing PLACEHOLDER is
un-dedicated, switched to BUF, and re-dedicated."
  (when (buffer-live-p buf)
    (let ((wins (get-buffer-window-list placeholder nil t)))
      (claude-repl--log nil "swap-placeholder-into-windows: swapping %d window(s) buf=%s"
                        (length wins) (buffer-name buf))
      (dolist (win wins)
        (set-window-dedicated-p win nil)
        (set-window-buffer win buf)
        (set-window-dedicated-p win t))
      (kill-buffer placeholder))))

(defun claude-repl--swap-placeholder ()
  "Replace the loading placeholder window with the real vterm buffer.
Called once when Claude sets its first terminal title (meaning it's ready)."
  (claude-repl--log nil "swap-placeholder buf=%s" (buffer-name))
  (let ((buf (current-buffer))
        (placeholder (get-buffer " *claude-loading*")))
    (when placeholder
      (run-at-time 0 nil
                   #'claude-repl--swap-placeholder-into-windows
                   buf placeholder))))

;;;; Session completion handling

(defun claude-repl--maybe-notify-finished (ws)
  "Send a desktop notification that Claude finished in WS, if frame is unfocused.
Debounces per-workspace to avoid duplicate notifications when both the hook
and title-change paths fire for the same turn completion."
  (claude-repl--log ws "maybe-notify-finished ws=%s focused=%s" ws (if (frame-focus-state) "yes" "no"))
  (let ((last (claude-repl--ws-get ws :last-notify-time))
        (now  (float-time)))
    (if (and (not (frame-focus-state))
             (or (null last) (> (- now last) 2.0)))
        (progn
          (claude-repl--ws-put ws :last-notify-time now)
          (run-at-time 0.1 nil #'claude-repl--notify ws "Claude REPL"
                       (format "%s: Claude ready" ws)))
      (when (and last (<= (- now last) 2.0))
        (claude-repl--log ws "maybe-notify-finished: debounce-hit ws=%s elapsed=%.2f" ws (- now last))))))

(defun claude-repl--mark-done-if-hidden (ws vterm-buf)
  "Mark WS as :done and clear :viewed when VTERM-BUF is not visible."
  (if (get-buffer-window vterm-buf t)
      (claude-repl--log ws "mark-done-if-hidden: visible — no-op ws=%s" ws)
    (claude-repl--log ws "mark-done-if-hidden: hidden — marking done ws=%s" ws)
    (claude-repl--ws-put ws :viewed nil)
    (claude-repl--ws-set ws :done)))

(defun claude-repl--refresh-vterm-after-finish (vterm-buf)
  "Refresh display and scroll position for VTERM-BUF if it is still live."
  (claude-repl--log nil "refresh-vterm-after-finish: buf=%s" (buffer-name vterm-buf))
  (if (buffer-live-p vterm-buf)
      (progn
        (with-current-buffer vterm-buf
          (claude-repl--do-refresh)
          (claude-repl--update-hide-overlay))
        (claude-repl--fix-vterm-scroll vterm-buf))
    (claude-repl--log nil "refresh-vterm-after-finish: buffer is dead buf=%s" (buffer-name vterm-buf))))

(defun claude-repl--handle-claude-finished (ws)
  "Handle Claude finishing in WS.
Looks up the vterm buffer from the workspace plist, marks it :done if not
visible, refreshes output, and notifies the user if the frame is unfocused."
  (let ((vterm-buf (claude-repl--ws-get ws :vterm-buffer)))
    (claude-repl--log ws "handle-claude-finished ws=%s visible=%s"
                      ws (if (and vterm-buf (get-buffer-window vterm-buf t)) "yes" "no"))
    (when vterm-buf
      (claude-repl--mark-done-if-hidden ws vterm-buf)
      (claude-repl--refresh-vterm-after-finish vterm-buf))
    (claude-repl--maybe-notify-finished ws)
    (unless (claude-repl--current-ws-p ws)
      (message "Claude finished in workspace: %s" ws))))

;;;; Session ID management

(defun claude-repl--set-session-id (ws id)
  "Set the session ID for workspace WS to ID."
  (claude-repl--log ws "set-session-id: ws=%s id=%s" ws id)
  (setf (claude-repl-instantiation-session-id (claude-repl--active-inst ws)) id))

(defun claude-repl--session-file-matches-p (file root container-path)
  "Return the session ID from JSON FILE if it matches ROOT or CONTAINER-PATH, or nil."
  (condition-case nil
      (let* ((json (json-read-file file))
             (cwd (cdr (assq 'cwd json)))
             (session-id (cdr (assq 'sessionId json))))
        (when (and cwd session-id
                   (let ((canonical-cwd (claude-repl--path-canonical cwd)))
                     (or (string= canonical-cwd root)
                         (string= canonical-cwd container-path))))
          session-id))
    (error nil)))

(defun claude-repl--find-session-id-in-dir (root container-path)
  "Search ~/.claude/sessions/ for a session matching ROOT or CONTAINER-PATH.
ROOT is the canonical host path and CONTAINER-PATH is the Docker mount
path (e.g. \"/<dirname>\").  Returns the session ID string or nil."
  (claude-repl--log nil "find-session-id-in-dir: root=%s container-path=%s" root container-path)
  (let ((sessions-dir (expand-file-name "~/.claude/sessions/")))
    (let ((result (when (file-directory-p sessions-dir)
                    (cl-loop for file in (directory-files sessions-dir t "\\.json\\'")
                             thereis (claude-repl--session-file-matches-p file root container-path)))))
      (claude-repl--log nil "find-session-id-in-dir: result=%s" result)
      result)))

(defun claude-repl--capture-session-id (ws)
  "Scan ~/.claude/sessions/ for a running session matching WS's project root.
Stores the session ID on the active instantiation, or clears a stale one."
  (let ((project-dir (claude-repl--ws-get ws :project-dir)))
    (if (null project-dir)
        (claude-repl--log ws "capture-session-id: no :project-dir for ws=%s, skipping" ws)
      (let* ((root (claude-repl--path-canonical project-dir))
             ;; Docker sandbox mounts the worktree at /<dirname>, so session files
             ;; record a container path rather than the full host path.
             (container-path (concat "/" (file-name-nondirectory root)))
             (found-id (claude-repl--find-session-id-in-dir root container-path))
             (env (claude-repl--ws-get ws :active-env)))
        (claude-repl--set-session-id ws found-id)
        (if found-id
            (progn
              (claude-repl--log ws "capture-session-id ws=%s env=%s id=%s" ws env found-id)
              (claude-repl--state-save ws))
          ;; Clear stale session-id so a restored-from-disk value doesn't
          ;; cause --resume with a dead session.
          (claude-repl--log ws "capture-session-id ws=%s env=%s: no matching session found (cleared)"
                            ws env))))))

;;;; Readiness and pending prompt handling

(defun claude-repl--deliver-pending-prompts (vterm-buf pending ws)
  "Deliver PENDING prompts to WS if VTERM-BUF is still live.
Each prompt in PENDING is sent via `claude-repl--send'."
  (claude-repl--log ws "deliver-pending-prompts: ws=%s count=%d" ws (length pending))
  (if (buffer-live-p vterm-buf)
      (dolist (p pending)
        (claude-repl--send p ws))
    (claude-repl--log ws "deliver-pending-prompts: buffer is dead ws=%s" ws)))

(defun claude-repl--drain-pending-prompts (ws)
  "Drain queued prompts for workspace WS after Claude becomes ready.
Clears :pending-prompts and schedules them for delivery with a 0.3s delay
so the terminal has time to settle."
  (let ((pending (claude-repl--ws-get ws :pending-prompts)))
    (when pending
      (claude-repl--log ws "first-ready draining %d pending prompt(s) for ws=%s" (length pending) ws)
      (claude-repl--ws-put ws :pending-prompts nil)
      (let ((vterm-buf (claude-repl--ws-get ws :vterm-buffer)))
        (run-at-time 0.3 nil
                     #'claude-repl--deliver-pending-prompts
                     vterm-buf pending ws)))
    pending))

(defun claude-repl--loading-placeholder-visible-p ()
  "Return non-nil if the loading placeholder buffer is displayed in a window."
  (when-let ((ph (get-buffer " *claude-loading*")))
    (get-buffer-window ph)))

(defun claude-repl--show-panels-or-defer (ws)
  "Open panels if WS is the current workspace, otherwise defer until switch.
`claude-repl--on-workspace-switch' checks :pending-show-panels.
Skip if the loading placeholder is still visible — `--swap-placeholder'
handles the visual transition and calling `claude-repl' here would
trigger `--show-existing-panels' with the wrong selected window."
  (if (claude-repl--current-ws-p ws)
      ;; Skip if the loading placeholder is still visible — --swap-placeholder
      ;; handles the visual transition via run-at-time 0.
      (unless (claude-repl--loading-placeholder-visible-p)
        (claude-repl--log ws "show-panels-or-defer: current ws=%s — showing panels" ws)
        (claude-repl))
    (claude-repl--log ws "show-panels-or-defer: other ws=%s — deferring" ws)
    (claude-repl--ws-put ws :pending-show-panels t)))

(defun claude-repl--finalize-ready-state (ws)
  "Finalize readiness for workspace WS: cancel timer and capture session ID."
  (claude-repl--log ws "finalize-ready-state: ws=%s" ws)
  (claude-repl--cancel-ready-timer ws)
  (claude-repl--capture-session-id ws))

(defun claude-repl--open-panels-after-ready (ws)
  "Open panels for WS after Claude becomes ready.
If there were pending prompts, always show panels (or defer).
Otherwise, only show panels if WS is the current workspace."
  (if (claude-repl--drain-pending-prompts ws)
      (progn
        (claude-repl--log ws "open-panels-after-ready: had pending prompts ws=%s — show or defer" ws)
        (claude-repl--show-panels-or-defer ws))
    (claude-repl--log ws "first-ready no pending prompts for ws=%s" ws)
    (if (and (claude-repl--current-ws-p ws)
             (not (claude-repl--loading-placeholder-visible-p)))
        (progn
          (claude-repl--log ws "open-panels-after-ready: no pending + current ws=%s — showing panels" ws)
          (claude-repl))
      (claude-repl--log ws "open-panels-after-ready: no pending + other ws=%s — no-op" ws))))

(defun claude-repl--handle-first-ready ()
  "Handle the first terminal title set — Claude is now ready.
Cancels the ready-timer, swaps the loading placeholder, drains any pending
prompts (with a 0.3s delay), and auto-opens panels if appropriate."
  (unless claude-repl--ready
    (claude-repl--log claude-repl--owning-workspace "first-ready buf=%s ws=%s" (buffer-name) claude-repl--owning-workspace)
    (setq claude-repl--ready t)
    (when claude-repl--owning-workspace
      (claude-repl--finalize-ready-state claude-repl--owning-workspace))
    (claude-repl--swap-placeholder)
    (when claude-repl--owning-workspace
      (claude-repl--open-panels-after-ready claude-repl--owning-workspace))))

(defun claude-repl--on-vterm-title-set (_title)
  "Detect first terminal title set to trigger ready handshake.
Ignores TITLE -- only uses the event as a signal that Claude is ready."
  (if (claude-repl--claude-buffer-p)
      (claude-repl--handle-first-ready)
    (claude-repl--log nil "on-vterm-title-set: not a claude buffer — skipping buf=%s" (buffer-name))))


(after! vterm
  (advice-add 'vterm--set-title :before #'claude-repl--on-vterm-title-set))

;;;; Process state predicates

(defun claude-repl--vterm-process-alive-p (ws)
  "Return non-nil if WS has a live vterm buffer with an active process."
  (let* ((buf (claude-repl--ws-get ws :vterm-buffer))
         (result (and buf (buffer-live-p buf) (get-buffer-process buf))))
    (claude-repl--log-verbose ws "vterm-process-alive-p: ws=%s alive=%s" ws (if result "yes" "no"))
    result))

(defun claude-repl--vterm-running-p (&optional ws)
  "Return t if Claude vterm buffer for WS exists with a live process.
WS defaults to the current workspace name."
  (claude-repl--vterm-process-alive-p (or ws (+workspace-current-name))))

(defun claude-repl--session-starting-p (&optional ws)
  "Return t if vterm exists with a live process but Claude is not yet ready.
WS defaults to the current workspace name."
  (let* ((ws (or ws (+workspace-current-name)))
         (result (and (claude-repl--vterm-process-alive-p ws)
                      (not (buffer-local-value 'claude-repl--ready
                                               (claude-repl--ws-get ws :vterm-buffer))))))
    (claude-repl--log-verbose ws "session-starting-p: ws=%s starting=%s" ws (if result "yes" "no"))
    result))

;;;; Readiness timer (fallback polling)

(defun claude-repl--cancel-ready-timer (ws)
  "Cancel the readiness-poll timer for workspace WS, if any."
  (let ((timer (claude-repl--ws-get ws :ready-timer)))
    (if timer
        (progn
          (claude-repl--log ws "cancel-ready-timer: canceling timer for ws=%s" ws)
          (when (timerp timer) (cancel-timer timer))
          (claude-repl--ws-put ws :ready-timer nil))
      (claude-repl--log ws "cancel-ready-timer: no timer to cancel for ws=%s" ws))))

(defun claude-repl--ready-timer-tick (ws start-time)
  "Handle one tick of the readiness-poll timer for workspace WS.
START-TIME is the `float-time' when polling began.  Cancels the timer and
gives up after 30 seconds, or cancels and opens panels once Claude is ready."
  (let ((elapsed (- (float-time) start-time)))
    (claude-repl--log-verbose ws "ready-timer-tick: ws=%s elapsed=%.1fs" ws elapsed)
    (cond
     ((> elapsed 30.0)
      (claude-repl--cancel-ready-timer ws)
      (claude-repl--log ws "ready-timer: timed out for ws=%s" ws))
     ((claude-repl--session-starting-p ws) nil)
     (t
      (claude-repl--cancel-ready-timer ws)
      (when (claude-repl--current-ws-p ws)
        (claude-repl))))))

(defun claude-repl--schedule-ready-timer (ws)
  "Poll every 0.5s until Claude is ready in WS, then auto-open panels.
Gives up after 30s. This is a fallback — the title-change path is the happy path."
  (claude-repl--cancel-ready-timer ws)
  (let ((start-time (float-time)))
    (claude-repl--ws-put ws :ready-timer
      (run-at-time
       0.5 0.5
       #'claude-repl--ready-timer-tick
       ws start-time))))
