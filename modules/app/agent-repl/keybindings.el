;;; keybindings.el --- keybindings and debug helpers -*- lexical-binding: t; -*-

;;; Section 1: Internal helpers

(defconst agent-repl--output-dir
  (file-name-as-directory (agent-repl--global-state-file "output"))
  "Directory for workspace command files and other IPC output.
Lives at `~/.claude-emacs/output/' (under `agent-repl--global-state-dir').")

(defcustom agent-repl-debug-mock-workspace-default-slug "mock-test"
  "Default bare slug used in mock workspace generation.
The full branch name is built at the consumption site by prepending the
workspace prefix from `agent-repl--workspace-prefix-slash' (derived from
CLAUDE_WORKSPACE_PREFIX), so this holds no literal prefix."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-debug-mock-priority-branch-default-slug "mock-priority-test"
  "Default bare slug used in mock workspace priority generation.
The full branch name is built at the consumption site by prepending the
workspace prefix from `agent-repl--workspace-prefix-slash' (derived from
CLAUDE_WORKSPACE_PREFIX), so this holds no literal prefix."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-workspace-commands-file-regexp "^workspace_commands_.*\\.json$"
  "Regexp matching workspace command files in the output directory."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-dump-buffer-name "*agent-repl-dump*"
  "Buffer name for workspace state dump output."
  :type 'string
  :group 'agent-repl)

;;;; Faces for `agent-repl-debug/dump-workspace' ---------------------------

(defface agent-repl-dump-title
  '((t :weight bold :height 1.6 :inherit font-lock-function-name-face))
  "Face for the workspace title line in `agent-repl-debug/dump-workspace'."
  :group 'agent-repl)

(defface agent-repl-dump-section
  '((t :weight bold :height 1.25 :inherit font-lock-keyword-face))
  "Face for section headers in `agent-repl-debug/dump-workspace'."
  :group 'agent-repl)

(defface agent-repl-dump-key
  '((t :weight bold :inherit font-lock-variable-name-face))
  "Face for plist keys in `agent-repl-debug/dump-workspace'."
  :group 'agent-repl)

(defface agent-repl-dump-rule
  '((t :inherit shadow))
  "Face for the rule line beneath the title in
`agent-repl-debug/dump-workspace'."
  :group 'agent-repl)

;;;; Section layout for `agent-repl-debug/dump-workspace' ------------------

(defconst agent-repl--dump-sections
  '(("🏷️  Identity"
     (:name :ws-id :priority :group-key))
    ("⚡ State"
     (:agent-state :repl-state :status :stop-received
      :flashing :hidden :dead :bogus :merged))
    ("🌳 Project / Git"
     (:project-dir :worktree-p :source-ws-dir :source-ws-name
      :merge-parent-dir :branch-merged :branch-merged-last-check
      :detail-branch :detail-dirty-count :detail-last-commit
      :detail-last-commit-time :detail-master-ahead :detail-source-ahead
      :detail-source-branch :git-clean :git-proc))
    ("🧠 Session"
     (:session-id :fork-session-id :vterm-buffer :active-env
      :sandbox :bare-metal :agent-ready :ws-loaded :ready-timer))
    ("💬 Prompts"
     (:last-prompt-time :last-prompt-text :last-prompt-summary
      :last-prompt-summary-pending :deferred-prompts :pending-prompts
      :pending-subagents :clipboard))
    ("🔔 Notifications"
     (:done :done-acked :done-acked-at :last-notify-time))
    ("🔀 Merge"
     (:merge-completed :merge-completed-at :merge-conflict
      :merge-failed :merge-proc :merge-queued :merging))
    ("🪟 UI / Panels"
     (:input-buffer :pending-magit :pending-show-panels
      :pending-initial-buffers :fullscreen-config :ai-title-cache
      :saved-tab-index))
    ("🔢 Counters"
     (:counter :prefix-counter)))
  "Section layout for `agent-repl-debug/dump-workspace'.
Each entry is (TITLE KEYS).  TITLE is the section header string (with a
leading emoji); KEYS is the list of plist keys that belong in that
section, in display order.  Any key present in the workspace plist that
is not listed in any section falls through to the
`agent-repl--dump-other-section' bucket at the end of the dump.")

(defconst agent-repl--dump-other-section "📦 Other"
  "Section header used for plist keys not classified by
`agent-repl--dump-sections'.")

(defun agent-repl--format-dump-value (val)
  "Render VAL for the workspace dump output.
Buffers, processes, timers, and cl-structs become readable strings;
every other value goes through `pp-to-string' so cons cells and lists
render as Lisp."
  (cond
   ((bufferp val)
    (format "#<buffer %s %s>"
            (buffer-name val)
            (if (buffer-live-p val) "live" "dead")))
   ((processp val)
    (format "#<process %s %s>"
            (process-name val)
            (if (process-live-p val) "running" "exited")))
   ((timerp val)
    (format "#<timer %s>" (if (timer--triggered val) "triggered" "pending")))
   ((cl-struct-p val)
    (string-trim (pp-to-string val)))
   (t (string-trim (pp-to-string val)))))

(defun agent-repl--dump-plist-to-alist (plist)
  "Convert PLIST to an alist of (KEY . VALUE), preserving insertion order."
  (let (result)
    (while plist
      (let ((k (pop plist))
            (v (pop plist)))
        (push (cons k v) result)))
    (nreverse result)))

(defun agent-repl--dump-insert-row (key val)
  "Insert one KEY/VAL row at point in the current buffer.
KEY is rendered with `agent-repl-dump-key' face; VAL is rendered via
`agent-repl--format-dump-value' with no face."
  (insert "  ")
  (insert (propertize (format "%-30s" (symbol-name key))
                      'face 'agent-repl-dump-key))
  (insert "  ")
  (insert (agent-repl--format-dump-value val))
  (insert "\n"))

(defun agent-repl--dump-insert-section (title rows)
  "Insert section TITLE followed by ROWS (an alist of (KEY . VALUE)).
No-op when ROWS is empty so empty sections do not clutter the output."
  (when rows
    (insert "\n")
    (insert (propertize title 'face 'agent-repl-dump-section))
    (insert "\n")
    (dolist (row rows)
      (agent-repl--dump-insert-row (car row) (cdr row)))))

(defun agent-repl--dump-partition (alist sections)
  "Partition ALIST by SECTIONS.
Returns a list of (TITLE . ROWS) plus a final (OTHER-TITLE . REMAINING)
entry holding any cells whose key did not appear in SECTIONS.  Order
within each section follows the key order in SECTIONS; OTHER preserves
the original ALIST order."
  (let ((remaining alist)
        (result nil))
    (dolist (section sections)
      (let* ((title (car section))
             (keys (cadr section))
             (rows nil))
        (dolist (k keys)
          (let ((cell (assoc k remaining)))
            (when cell
              (push cell rows)
              (setq remaining (delq cell remaining)))))
        (push (cons title (nreverse rows)) result)))
    (push (cons agent-repl--dump-other-section remaining) result)
    (nreverse result)))

(defun agent-repl--cons-name-state (name)
  "Return (NAME . agent-state) for workspace NAME."
  (cons name (agent-repl--ws-agent-state name)))

(defun agent-repl--format-workspace-state (pair)
  "Format a (NAME . STATE) PAIR as an indented diagnostic string."
  (format "  %s: %s" (car pair) (or (cdr pair) "nil")))

(defun agent-repl--format-buffer-info (buf)
  "Format BUF's name, owning workspace, and persp workspace as a diagnostic string."
  (format "  %s  owning=%s  persp=%s"
          (buffer-name buf)
          (or (agent-repl--buffer-owner buf) "nil")
          (or (agent-repl--workspace-for-buffer buf) "nil")))

(defun agent-repl--kill-before-workspace-delete (&optional name &rest _)
  "Before-advice for `+workspace/kill': tear down any running agent session.
NAME is the workspace `+workspace/kill' was invoked on.  Only fire when
NAME refers to the current workspace — `agent-repl--agent-running-p'
inspects the current ws's vterm, so applying it cross-workspace would
kill the wrong session (e.g. when the hide-mode sweep persp-kills a
background `:hidden' workspace from inside a workspace-switch handler,
the named workspace's session has already been torn down by the sweep
and the current workspace's session must be left alone).  Callers that
need to kill a specific named workspace's session (the nuke / kill /
sweep paths) handle teardown explicitly via `agent-repl--kill-session'
before invoking `+workspace/kill'."
  (let ((target (or name (agent-repl--ws-current-name)))
        (current (agent-repl--ws-current-name)))
    (agent-repl--log current
                      "kill-before-workspace-delete: target=%s current=%s"
                      target current)
    (cond
     ((not (equal target current))
      (agent-repl--log current
                        "kill-before-workspace-delete: target!=current, skipping (caller handles teardown)"))
     ((agent-repl--agent-running-p)
      (agent-repl--log current "kill-before-workspace-delete: vterm running, killing session")
      (agent-repl-kill))
     (t
      (agent-repl--log current "kill-before-workspace-delete: vterm not running, no-op")))))

(defun agent-repl--read-workspace (prompt)
  "Prompt for a workspace name with PROMPT.  Requires an exact match."
  (completing-read prompt (agent-repl--ws-list-names) nil t))

(defun agent-repl--read-workspace-with-default (prompt)
  "Prompt for a workspace name with PROMPT, defaulting to the current workspace."
  (completing-read prompt (agent-repl--ws-list-names) nil t
                   nil nil (agent-repl--ws-current-name)))

(defun agent-repl--read-known-workspace (prompt)
  "Prompt for a workspace registered in `agent-repl--workspaces'.
Defaults to the current workspace when it is registered (so RET picks
the obvious target).  Signals `user-error' when no workspaces exist.

Filters out tombstoned entries via `agent-repl--live-ws-names' — a
nuked workspace's identity record survives in the hash for
`--ws-dir' callers, but it must not surface in interactive pickers."
  (let* ((known (agent-repl--live-ws-names))
         (current (agent-repl--ws-current-name))
         (default (and current (member current known) current)))
    (unless known (user-error "No agent-repl workspaces registered"))
    (completing-read prompt known nil t nil nil default)))

(defun agent-repl--nukeable-workspace-names ()
  "Return candidate names for the nuke/kill picker.
Union of live agent-repl workspaces (`agent-repl--live-ws-names')
and tab-bar workspaces (`agent-repl--ws-all-names'), preserving the live
entries first.  Tab-bar entries whose agent-repl session has been
torn down (or never existed) are included so the user can dispatch a
plain persp/doom kill on stray tabs through the same picker — the
dispatcher (`agent-repl--nuke-or-kill-workspace') decides per-entry
whether to run the agent-repl teardown or a bare `+workspace/kill'."
  (let* ((live (agent-repl--live-ws-names))
         (tabbar (agent-repl--ws-all-names))
         (extras (cl-remove-if (lambda (n) (member n live)) tabbar)))
    (append live extras)))

(defun agent-repl--read-nukeable-workspace (prompt)
  "Prompt for a workspace to nuke/kill.
Candidates come from `agent-repl--nukeable-workspace-names': live
agent-repl workspaces plus tab-bar workspaces whose agent has
already been killed.  Defaults to the current workspace when it
appears in the candidate list.  Signals `user-error' when no
candidates exist."
  (let* ((known (agent-repl--nukeable-workspace-names))
         (current (agent-repl--ws-current-name))
         (default (and current (member current known) current)))
    (unless known (user-error "No workspaces available to nuke/kill"))
    (completing-read prompt known nil t nil nil default)))

(defun agent-repl--write-output-json (filename content)
  "Write CONTENT as JSON to FILENAME inside `agent-repl--output-dir'.
Ensures the output directory exists.  Returns the full path of the written file."
  (make-directory agent-repl--output-dir t)
  (let ((file (expand-file-name filename agent-repl--output-dir)))
    (agent-repl--log (agent-repl--ws-current-name) "write-output-json: filename=%s dir=%s" filename agent-repl--output-dir)
    (with-temp-file file
      (insert (json-encode content)))
    file))

(defun agent-repl--list-agent-vterm-buffers ()
  "Return a list of live agent vterm buffers (matching `agent-repl--vterm-buffer-re')."
  (cl-remove-if-not #'agent-repl--agent-buffer-p (buffer-list)))

;;; Section 2: Utility commands used by keybindings

;; SPC o 0-9: send a digit character to the agent from the leader keymap.
(defun agent-repl--send-digit-char ()
  "Send the digit from the current key event to the agent.
Extracts the trailing digit from the key sequence (e.g. SPC o 3 -> \"3\")."
  (interactive)
  (let* ((keys (this-command-keys-vector))
         (last-key (aref keys (1- (length keys)))))
    (agent-repl--log (agent-repl--ws-current-name) "send-digit-char: digit=%s" (string last-key))
    (agent-repl-send-char (string last-key))))

;; C-v paste forwarding to vterm
(defun agent-repl-paste-to-vterm ()
  "Forward a Ctrl-V keystroke to the agent vterm buffer.
This lets the agent CLI handle paste natively, including images."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-name) "paste-to-vterm: entry")
  (if (agent-repl--vterm-live-p)
      (progn
        (agent-repl--log (agent-repl--ws-current-name) "paste-to-vterm: vterm live, forwarding C-v")
        (with-current-buffer (agent-repl--ws-get (agent-repl--ws-current-name) :vterm-buffer)
          (vterm-send-key "v" nil nil t)))
    (user-error "No live Claude session — paste not forwarded")))

;; TODO: agent-repl-set-priority belongs in commands.el rather than
;; keybindings.el.  Do not move yet -- other agents are modifying that file.
(defconst agent-repl--priority-remove-label "*remove*"
  "Label shown in the priority completion list for the remove option.
Maps to the empty-string priority value when chosen.  Used because
the clear sentinel has no badge image and an empty string cannot
carry a `display' text property in any usable way.

Only offered when the current workspace already has a priority — when
there is nothing to remove, the entry is omitted from the candidate
list to avoid presenting a no-op choice.")

(defun agent-repl--decorate-priority-candidate (priority)
  "Return a completion candidate for PRIORITY whose `display' is the badge image.
The underlying string content remains PRIORITY, so completing-read's
matcher and return value are unchanged — only the visual rendering in
the minibuffer is replaced by the image.  When no image is registered
for PRIORITY (e.g. running in a no-image build), returns PRIORITY
unchanged so the prompt remains usable as plain text.

The image spec is attached directly as the `display' value (rather
than wrapped in a propertized space) because completion frameworks
like vertico render candidates by inspecting the candidate's own
text properties, and a nested `display' property on a wrapper string
collapses to nothing in that path — leaving the row appearing empty."
  (let ((img (and (fboundp 'agent-repl--priority-image)
                  (agent-repl--priority-image priority))))
    (if img
        (propertize priority 'display img)
      priority)))

(defun agent-repl--read-priority (prompt default)
  "Prompt for a priority level using PROMPT, defaulting to DEFAULT.
Candidates are the entries in `agent-repl-priority-levels' rendered
purely as their badge images (no accompanying text).  When DEFAULT is
a non-empty priority — meaning the workspace already has one set —
the textual `agent-repl--priority-remove-label' entry is appended,
mapping back to the empty-string \"clear\" sentinel when chosen.
When DEFAULT is empty or nil, the remove entry is omitted because
there is nothing to remove."
  (let* ((has-current (and default (not (string-empty-p default))))
         (candidates (append (mapcar #'agent-repl--decorate-priority-candidate
                                     agent-repl-priority-levels)
                             (when has-current
                               (list agent-repl--priority-remove-label))))
         (effective-default (and has-current default))
         (raw (completing-read prompt candidates nil t nil nil effective-default))
         (chosen (substring-no-properties raw)))
    (if (equal chosen agent-repl--priority-remove-label) "" chosen)))

(defun agent-repl-set-priority (priority &optional ws)
  "Set or change the priority badge for workspace WS.
WS defaults to the current workspace.  PRIORITY is one of the strings
in `agent-repl-priority-levels', or \"\" to clear.  Persists through
`agent-repl--state-save' so the badge survives restarts, reorders the
workspace in the tab-bar by its new priority, and forces a mode-line
repaint so the glyph updates immediately.  Pulses the workspace's tab
via `agent-repl-flash-tab' so the user can spot the slot whose
priority just shifted (matches the `SPC p p' / worktree-jump flash
semantic).

Interactively, always targets the current workspace and prompts only
for the priority (defaulting to the workspace's current priority, if
any).  Each candidate in the prompt is annotated with its badge
image so the visual mapping between key and glyph is obvious."
  (interactive
   (let* ((target (agent-repl--ws-current-name))
          (current (agent-repl--ws-get target :priority))
          (prompt (format "Priority%s: "
                          (if current (format " (current: %s)" current) "")))
          (priority (agent-repl--read-priority prompt (or current ""))))
     (list priority target)))
  (let* ((ws-explicit-p (not (null ws)))
         (ws (or ws (agent-repl--ws-current-name)))
         (old-priority (agent-repl--ws-get ws :priority))
         (new-priority (if (string-empty-p priority) nil priority))
         (had-entry (not (null (gethash ws agent-repl--workspaces))))
         (cache-before (or (agent-repl--ws-names-cache) "(unbound)")))
    (agent-repl--log ws "set-priority: ws=%s ws-explicit=%s had-entry=%s priority %s -> %s cache=%S"
                      ws (if ws-explicit-p "t" "nil") (if had-entry "t" "nil")
                      (or old-priority "nil") (or new-priority "(cleared)")
                      cache-before)
    (agent-repl--ws-put ws :priority new-priority)
    (agent-repl--state-save ws)
    (agent-repl--reorder-workspace-by-priority ws)
    (force-mode-line-update t)
    (when (fboundp 'agent-repl-flash-tab)
      (agent-repl-flash-tab ws))
    (message "Workspace '%s' priority: %s" ws (if (string-empty-p priority) "cleared" priority))))

;; SPC b R -- revert buffer from disk then eval as Elisp (fast config reload)
(defun agent-repl-revert-and-eval-buffer ()
  "Revert the current buffer from disk, then evaluate it as Elisp."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-name) "revert-and-eval-buffer: entry buffer=%s" (buffer-name))
  (revert-buffer :ignore-auto :noconfirm)
  (eval-buffer))

;; SPC j R -- reload the agent-repl module's config.el (the agent
;; workspace's config), independent of whatever buffer is current.
(defun agent-repl--reload-config-file ()
  "Return the config.el path to reload for the current workspace.

Prefers `<project-dir>/modules/app/agent-repl/config.el' when it exists,
so reloading inside a doom-config worktree (e.g.
`~/.config/doom-worktrees/foo/') picks up THAT worktree's checkout
rather than the root `~/.config/doom' copy the module was originally
loaded from.  Falls back to `agent-repl--config-file' (the original
load path) for non-doom-config workspaces, unregistered workspaces, or
workspaces with no `:project-dir'."
  (let* ((ws (agent-repl--ws-current-name))
         (proj (and ws (agent-repl--ws-get ws :project-dir)))
         (candidate (and proj (expand-file-name "modules/app/agent-repl/config.el" proj))))
    (if (and candidate (file-exists-p candidate))
        candidate
      agent-repl--config-file)))

(defun agent-repl-reload-config ()
  "Reload the agent-repl module config for the current workspace.
Resolves the config path via `agent-repl--reload-config-file' so a
doom-config worktree reloads its own checkout."
  (interactive)
  (let ((file (agent-repl--reload-config-file)))
    (agent-repl--log (agent-repl--ws-current-name) "reload-config: file=%s" file)
    (load-file file)
    (message "[agent-repl] Reloaded %s" file)))

;;; Section 3: Debug helpers -- interactive commands for diagnosing workspace state issues.
;;; Call via M-x agent-repl-debug/...

(defun agent-repl-debug/cancel-timers ()
  "Cancel all agent-repl timers."
  (interactive)
  (agent-repl--cancel-all-timers)
  (message "Cancelled all agent-repl timers."))

(defun agent-repl-debug/mock-workspace-generation (&optional names)
  "Write a mock workspace_generation.json to trigger the file watcher.
NAMES is an optional list of branch name strings; defaults to a single test entry."
  (interactive)
  (let* ((names (or names
                     (list (concat (agent-repl--workspace-prefix-slash)
                                   agent-repl-debug-mock-workspace-default-slug))))
         (file (agent-repl--write-output-json "workspace_generation.json" names)))
    (agent-repl--log (agent-repl--ws-current-name) "mock workspace-generation file written: %s names=%s" file names)
    (message "Wrote mock workspace_generation.json: %s" names)))

(defun agent-repl-debug/mock-workspace-commands-with-priority ()
  "Write a mock workspace_commands file with a priority field to test image badges."
  (interactive)
  (let* ((priority (completing-read "Priority: " agent-repl-priority-levels nil t))
         (name (read-string "Branch name: "
                            (concat (agent-repl--workspace-prefix-slash)
                                    agent-repl-debug-mock-priority-branch-default-slug)))
         (filename (format "workspace_commands_%s.json" (format-time-string "%s")))
         (commands (vector `((type . "create")
                             (name . ,name)
                             (priority . ,priority))))
         (file (agent-repl--write-output-json filename commands)))
    (message "Wrote %s with priority=%s" file priority)))

(defun agent-repl-debug/process-pending-commands ()
  "Manually scan ~/.claude-emacs/output/ and process any workspace_commands_*.json files.
Use this to verify the processor works independently of the file watcher."
  (interactive)
  (let ((files (when (file-directory-p agent-repl--output-dir)
                 (directory-files agent-repl--output-dir t
                                  agent-repl-workspace-commands-file-regexp))))
    (if (not files)
        (message "No workspace_commands_*.json files found in %s"
                 agent-repl--output-dir)
      (message "Found %d file(s), processing..." (length files))
      (dolist (file files)
        (agent-repl--log (agent-repl--ws-current-name) "process-pending-commands: processing file=%s" file)
        (agent-repl--process-workspace-commands-file file)))))

(defun agent-repl-debug/workspace-states ()
  "Display all workspace states."
  (interactive)
  (let ((states (mapcar #'agent-repl--cons-name-state (agent-repl--ws-list-names))))
    (message "Workspace states:\n%s"
             (mapconcat #'agent-repl--format-workspace-state states "\n"))))

(defun agent-repl-debug/buffer-info ()
  "Display all agent vterm buffers with their owning and persp workspaces."
  (interactive)
  (let* ((bufs (agent-repl--list-agent-vterm-buffers))
         (lines (mapcar #'agent-repl--format-buffer-info bufs)))
    (message "Claude buffers:\n%s"
             (if lines (mapconcat #'identity lines "\n") "  (none)"))))

(defun agent-repl-debug/clear-state (ws)
  "Clear all states for workspace WS without killing buffers."
  (interactive (list (agent-repl--read-workspace "Workspace: ")))
  (dolist (state '(:thinking :done :permission :inactive))
    (agent-repl--ws-agent-state-clear-if ws state))
  (message "Cleared all states for %s" ws))

(defun agent-repl--kill-owned-panel-buffers (ws)
  "Kill all agent panel buffers owned by workspace WS.
Closes their windows (selected-frame, to preserve historical scope)
and silences process exit queries before killing."
  (agent-repl--log ws "kill-owned-panel-buffers: entry ws=%s" ws)
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (agent-repl--agent-panel-buffer-p buf)
               (equal ws (agent-repl--buffer-owner buf)))
      (agent-repl--log ws "kill-owned-panel-buffers: killing buffer=%s" (buffer-name buf))
      (agent-repl-window--delete-buffer-windows buf :all-frames nil)
      (let ((proc (get-buffer-process buf)))
        (when proc (set-process-query-on-exit-flag proc nil)))
      (kill-buffer buf))))

(defun agent-repl-debug/obliterate (ws)
  "Completely remove workspace WS from all agent-repl tracking.
Kills agent buffers, closes windows, and removes all state."
  (interactive (list (agent-repl--read-workspace "Obliterate workspace: ")))
  (agent-repl--log ws "debug/obliterate: entry ws=%s" ws)
  (agent-repl--kill-owned-panel-buffers ws)
  (agent-repl--ws-del ws)
  (message "Obliterated all agent-repl state for %s" ws))

(defun agent-repl-debug/set-owning-workspace ()
  "Set the owning workspace for an agent vterm buffer."
  (interactive)
  (let* ((bufs (agent-repl--list-agent-vterm-buffers))
         (buf-name (completing-read "Buffer: " (mapcar #'buffer-name bufs) nil t))
         (ws (agent-repl--read-workspace "Owning workspace: ")))
    (with-current-buffer buf-name
      (setq-local agent-repl--owning-workspace ws))
    (message "Set %s owning workspace to %s" buf-name ws)))

(defun agent-repl-debug/toggle-logging (&optional verbose)
  "Toggle debug logging.
Without prefix argument: cycle nil → t → nil.
With prefix argument (\\[universal-argument]): cycle nil → verbose → nil.
Verbose mode additionally logs high-frequency events (timer ticks,
window changes, git-diff sentinels, resolve-root, etc.)."
  (interactive "P")
  (setq agent-repl-debug
        (if verbose
            (if (eq agent-repl-debug 'verbose) nil 'verbose)
          (if agent-repl-debug nil t)))
  (let ((label (pcase agent-repl-debug
                 ('nil "OFF")
                 ('t   "ON")
                 ('verbose "ON (verbose)")
                 (_ (error "agent-repl-debug has unexpected value: %S" agent-repl-debug)))))
    ;; Always emit via message so it's visible even when logging is off.
    (message "[agent-repl] debug logging: %s" label)
    ;; Also emit via the log system so it appears in the log stream.
    (when agent-repl-debug
      (agent-repl--log (agent-repl--ws-current-name) "debug logging toggled: %s" label))))

(defun agent-repl-debug/toggle-log-to-file ()
  "Toggle writing debug log output to `~/.claude-emacs/doom-agent-repl.log'.
When enabled, all messages that pass through `agent-repl--do-log' are
appended to the file regardless of the `agent-repl-debug' level."
  (interactive)
  (setq agent-repl-log-to-file (not agent-repl-log-to-file))
  (let ((label (if agent-repl-log-to-file "ON" "OFF"))
        (path (agent-repl--logfile-path)))
    (message "[agent-repl] log-to-file: %s%s"
             label
             (if path (format " (%s)" path) ""))))

(defun agent-repl-debug/toggle-metaprompt ()
  "Toggle the metaprompt prefix injection."
  (interactive)
  (setq agent-repl-skip-permissions (not agent-repl-skip-permissions))
  (message "Agent REPL metaprompt: %s" (if agent-repl-skip-permissions "ON" "OFF")))

(defun agent-repl-debug/prefix-counter ()
  "Show the current metaprompt prefix counter, period, and workspace."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (counter (or (agent-repl--ws-get ws :prefix-counter) 0)))
    (message "[%s] Prefix counter: %d  period: %d  next metaprompt in: %d sends"
             ws counter agent-repl-prefix-period
             (- agent-repl-prefix-period (mod counter agent-repl-prefix-period)))))

(defun agent-repl-debug/dump-workspace ()
  "Display the full serialized plist for a selected workspace from the hashmap.
Prompts to select from workspaces registered in `agent-repl--workspaces',
defaulting to the current workspace when registered.

Output is organized into emoji-prefixed sections (Identity, State,
Project / Git, Session, Prompts, Notifications, Merge, UI / Panels,
Counters) per `agent-repl--dump-sections', with any unclassified keys
emitted under the `Other' bucket.  Section headers and the workspace
title are rendered with `agent-repl-dump-section' / `agent-repl-dump-title'
faces so they stand out visually in the help buffer."
  (interactive)
  (let* ((ws (agent-repl--read-known-workspace "Dump workspace: "))
         (plist (gethash ws agent-repl--workspaces))
         (alist (agent-repl--dump-plist-to-alist plist))
         (partition (agent-repl--dump-partition
                     alist agent-repl--dump-sections)))
    (with-help-window agent-repl-dump-buffer-name
      (with-current-buffer agent-repl-dump-buffer-name
        (insert (propertize (format "Workspace: %s" ws)
                            'face 'agent-repl-dump-title))
        (insert "\n")
        (insert (propertize (make-string 60 ?─)
                            'face 'agent-repl-dump-rule))
        (insert "\n")
        (dolist (section partition)
          (agent-repl--dump-insert-section (car section) (cdr section)))))))

(defun agent-repl-debug/workspace-clean-p (ws-name)
  "Show whether workspace WS-NAME has unstaged changes to tracked files.
Uses `agent-repl--workspace-clean-p' -- the same function used in production."
  (interactive (list (agent-repl--read-workspace-with-default "Workspace: ")))
  (let ((clean (agent-repl--workspace-clean-p ws-name)))
    (message "Workspace %s: %s" ws-name (if clean "clean" "dirty"))))

(defun agent-repl-debug/--gather-ws-diagnostics (ws-name)
  "Gather diagnostic information about workspace WS-NAME.
Returns a plist with keys :vterm-buf :proc-alive :owning-ws :has-window
:agent-open :dirty."
  (let* ((open (agent-repl--ws-agent-open-p ws-name))
         (dirty (not (agent-repl--workspace-clean-p ws-name)))
         (persp (agent-repl--ws-resolve-persp ws-name))
         (persp-bufs (agent-repl--ws-buffers persp))
         (vterm-buf (cl-loop for buf in persp-bufs
                             when (and (buffer-live-p buf)
                                       (agent-repl--agent-buffer-p buf))
                             return buf))
         (proc (and vterm-buf (get-buffer-process vterm-buf)))
         (proc-alive (and proc (process-live-p proc)))
         (owning-ws (agent-repl--buffer-owner vterm-buf))
         (has-window (and vterm-buf (get-buffer-window vterm-buf t))))
    (list :vterm-buf vterm-buf :proc-alive proc-alive
          :owning-ws owning-ws :has-window has-window
          :agent-open open :dirty dirty)))

(defun agent-repl-debug/--apply-state-refresh (ws-name agent-open)
  "Apply a state refresh for WS-NAME given whether AGENT-OPEN is non-nil.
Mirrors the logic in `agent-repl--update-all-workspace-states'."
  (if agent-open
      (agent-repl--update-ws-state ws-name)
    (agent-repl--mark-dead-vterm ws-name)))

(defun agent-repl-debug/--format-diagnostics (ws-name diag before after)
  "Format a diagnostic summary string for WS-NAME.
DIAG is the plist from `agent-repl-debug/--gather-ws-diagnostics'.
BEFORE and AFTER are the workspace states before and after refresh."
  (let ((vterm-buf (plist-get diag :vterm-buf)))
    (format (concat "Workspace %s:\n"
                    "  vterm-buf=%s process=%s\n"
                    "  owning-ws=%s has-window=%s\n"
                    "  agent-open=%s dirty=%s\n"
                    "  state=%s -> %s")
            ws-name
            (and vterm-buf (buffer-name vterm-buf))
            (if (plist-get diag :proc-alive) "alive" "dead/nil")
            (or (plist-get diag :owning-ws) "nil")
            (if (plist-get diag :has-window) "yes" "no")
            (if (plist-get diag :agent-open) "yes" "no")
            (if (plist-get diag :dirty) "yes" "no")
            (or before "nil") (or after "nil"))))

(defun agent-repl-debug/refresh-state (ws-name)
  "Force a full state refresh for workspace WS-NAME.
Runs the same logic as the periodic `update-all-workspace-states' timer:
checks agent visibility, git dirty status, and applies the state table.
Reports comprehensive diagnostics."
  (interactive (list (agent-repl--read-workspace-with-default "Workspace: ")))
  (let* ((before (agent-repl--ws-agent-state ws-name))
         (diag (agent-repl-debug/--gather-ws-diagnostics ws-name)))
    (agent-repl-debug/--apply-state-refresh ws-name (plist-get diag :agent-open))
    (let ((after (agent-repl--ws-agent-state ws-name)))
      (force-mode-line-update t)
      (message "%s" (agent-repl-debug/--format-diagnostics ws-name diag before after)))))

;;; Section 4: Keybinding definitions

;; C-S-<key>: drawer-equivalent operations dispatched against the drawer's
;; selected entry.  Auto-revert keeps the drawer cursor sync'd with the
;; active workspace by default, so these naturally target the current
;; workspace until the user moves the drawer cursor.
;;
;; Bind in `global-map' (no `:nvi' state prefix) so the chord works
;; uniformly across evil normal/visual/insert AND evil-emacs-state
;; buffers (vterm, *scratch*, magit popups, etc.).  Evil state maps
;; only override global-map when they bind the same key, so leaving
;; these in global-map means they fall through correctly everywhere.
(map! "C-S-n"        #'agent-repl-drawer-global-next)
(map! "C-S-p"        #'agent-repl-drawer-global-prev)
(map! "C-S-x"        #'agent-repl-drawer-global-nuke)
(map! "C-S-d"        #'agent-repl-drawer-global-kill)
(map! "C-S-i"        #'agent-repl-drawer-global-send-prompt)
(map! "C-S-m"        #'agent-repl-drawer-global-merge-into-master)
(map! "C-S-h"        #'agent-repl-drawer-global-toggle-hidden)
(map! "C-S-t"        #'agent-repl-drawer-global-toggle-mark)
(map! "C-S-u"        #'agent-repl-drawer-global-clear-marks)
(map! "C-S-+"        #'agent-repl-drawer-global-priority-up)
(map! "C-S--"        #'agent-repl-drawer-global-priority-down)

;; `C-S-j' / `C-S-k' need a stronger binding than the plain global-map
;; entry the rest of the drawer chords use, because `config.el' wires
;; `:nv "C-j" -> evil-window-down' / `:nv "C-k" -> evil-window-up' into
;; `general-override-mode-map''s evil intercept aux maps for
;; normal/visual.  When the user presses `C-S-j' in normal state,
;; `read-key-sequence' looks up `[?\C-\S-j]' across the active keymaps;
;; if no map binds the shifted key, Emacs performs shift-translation,
;; retrying as `[?\C-j]' -- which HITS the intercept aux map and fires
;; `evil-window-down', shadowing the global-map binding to
;; `agent-repl-scroll-output-down'.  Defeat the fallback by planting
;; explicit `C-S-j' / `C-S-k' entries in the same intercept aux maps
;; (for every evil state), and a matching top-level entry in
;; `general-override-mode-map' for non-evil contexts.
(defconst agent-repl--scroll-output-chords
  '(("C-S-j" . agent-repl-scroll-output-down)
    ("C-S-k" . agent-repl-scroll-output-up))
  "Alist of (KEY-STRING . COMMAND) for scroll-output chords that must
win key lookup above any minor-mode-map and any evil intercept aux map
-- specifically defeating shift-translation back to `C-j' / `C-k' which
would otherwise route the chord to `evil-window-down/up'.")

(defconst agent-repl--scroll-output-intercept-states
  '(normal visual insert emacs operator motion replace)
  "Evil states for which `agent-repl--scroll-output-chords' install
intercept aux map entries on `general-override-mode-map'.  Covers every
evil state so the chord wins regardless of which state is current.")

(defun agent-repl--install-scroll-output-overrides ()
  "Install `agent-repl--scroll-output-chords' into
`general-override-mode-map' at top-level AND into its evil intercept
aux maps for every state in `agent-repl--scroll-output-intercept-states'.
The top-level entry covers non-evil contexts; the per-state aux entries
beat any same-state evil binding and prevent shift-translation fallback.
Idempotent."
  (dolist (entry agent-repl--scroll-output-chords)
    (let ((seq (kbd (car entry)))
          (cmd (cdr entry)))
      (define-key general-override-mode-map seq cmd)
      (when (fboundp 'evil-get-auxiliary-keymap)
        (dolist (state agent-repl--scroll-output-intercept-states)
          (define-key (evil-get-auxiliary-keymap
                       general-override-mode-map state t t)
                      seq cmd))))))

(agent-repl--install-scroll-output-overrides)

;; `C-S-<return>' -> `agent-repl-drawer-global-visit' needs the same
;; override treatment as the scroll chords -- a plain `(map! ... )' lands
;; in `global-map', which loses to Doom default's `:gi/:gn "C-S-RET"' ->
;; `+default/newline-above' (evil aux on global state maps) and to
;; `agent-repl-input-mode-map's `:ni "C-S-RET"' major-mode aux.  Bind the
;; chord on `general-override-mode-map' AND its per-state aux maps so
;; it wins above all of them.
(defconst agent-repl--drawer-visit-chord
  '(("C-S-<return>" . agent-repl-drawer-global-visit))
  "Alist of (KEY-STRING . COMMAND) for the global drawer-visit chord
that must win key lookup above the Doom default's `:gi/:gn \"C-S-RET\"'
binding and above `agent-repl-input-mode-map's `:ni \"C-S-RET\"' aux.")

(defun agent-repl--install-drawer-visit-override ()
  "Install `agent-repl--drawer-visit-chord' into `general-override-mode-map'
at top-level AND into its evil intercept aux maps for every state in
`agent-repl--scroll-output-intercept-states' (reused as the canonical
\"all evil states\" list).  Idempotent."
  (dolist (entry agent-repl--drawer-visit-chord)
    (let ((seq (kbd (car entry)))
          (cmd (cdr entry)))
      (define-key general-override-mode-map seq cmd)
      (when (fboundp 'evil-get-auxiliary-keymap)
        (dolist (state agent-repl--scroll-output-intercept-states)
          (define-key (evil-get-auxiliary-keymap
                       general-override-mode-map state t t)
                      seq cmd))))))

(agent-repl--install-drawer-visit-override)

;; vterm-mode-map binds every `C-S-<letter>' to `vterm--self-insert' via
;; its define-keys loop over '("C-" "M-" "C-S-").  That major-mode
;; binding shadows our global-map entries whenever point lands in a
;; vterm buffer (e.g. immediately after `C-S-<return>' visits a
;; workspace and selects its Agent REPL output window), causing the
;; chord to be sent to the shell instead of triggering drawer nav.
;; Strip the conflicting keys from `vterm-mode-map' so global-map sees
;; them.  Done lazily inside `after! vterm' so it survives package
;; reloads.

(defconst agent-repl--vterm-shadow-keys
  '("C-S-n" "C-S-p" "C-S-j" "C-S-k" "C-S-x" "C-S-d"
    "C-S-i" "C-S-m" "C-S-h" "C-S-t" "C-S-u")
  "C-S-<letter> chords that `vterm-mode-map' would otherwise capture
via `vterm--self-insert', shadowing our global drawer-mirror bindings.
Non-letter chords like `C-S-<return>', `C-S-+', `C-S--' are not in
vterm's exclusion loop and need no stripping.")

(defun agent-repl--strip-vterm-shadow-keys ()
  "Unmap `agent-repl--vterm-shadow-keys' from `vterm-mode-map' so the
global drawer-mirror bindings win in vterm buffers."
  (dolist (key agent-repl--vterm-shadow-keys)
    (define-key vterm-mode-map (kbd key) nil)))

(after! vterm
  (agent-repl--strip-vterm-shadow-keys))

(map! :leader :prefix "w" :n "f" #'agent-repl-fullscreen-and-focus)

;; SPC o -- agent session control (open, focus, kill, interrupt, utilities)
(map! :leader
      :desc "Agent REPL (simple)" "o c" #'agent-repl-simple
      :desc "Agent REPL (deprio)" "o C" #'agent-repl
      :desc "Kill Claude" "o C-c" #'agent-repl-kill
      :desc "Kill claude process (keep panels)" "o k" #'agent-repl-kill-agent-process
      :desc "Claude input" "o v" #'agent-repl-focus-input
      :desc "Claude interrupt" "o x" #'agent-repl-interrupt
      :desc "Copy file reference" "o r" #'agent-repl-copy-reference
      :desc "Switch sandbox/bare-metal" "o s" #'agent-repl-switch-environment
      ;; `agent-repl-select-frontend' is deliberately UNBOUND: flipping a
      ;; workspace's presentation without killing its session invites two
      ;; agent processes on one directory.  `SPC o F' (kill-then-open,
      ;; conversation carried via the durable session id) is the only
      ;; user-facing frontend switch; select-frontend remains available
      ;; to internal callers.
      :desc "Switch frontend (kill + carry conversation)" "o F" #'agent-repl-switch-frontend
      :desc "Toggle hide-mode (closed-REPL workspaces)" "o h" #'agent-repl-toggle-hide-mode
      :desc "Toggle hide-project-dirs (ChessCom workspaces)" "o H" #'agent-repl-toggle-hide-project-dirs
      :desc "Toggle workspace drawer" "o d" #'agent-repl-drawer-toggle)

(map! :leader
      (:prefix "p"
       :desc "Switch to project" "p" #'agent-repl-switch-to-project
       :desc "Create worktree workspace" "w" #'agent-repl-create-worktree-workspace))

(map! :leader
      (:prefix "TAB"
       :desc "New worktree ws (from current)" "n" #'agent-repl-create-worktree-workspace
       :desc "New worktree ws (from local master)" "N" #'agent-repl-create-worktree-workspace-from-origin-master
       :desc "Fork worktree ws + fork Claude session" "f" #'agent-repl-fork-worktree-workspace
       :desc "Merge workspace into current" "m" #'agent-repl-workspace-merge
       :desc "Merge current workspace into source" "M" #'agent-repl-workspace-merge-current-into-source
       :desc "Rename current workspace" "r" #'agent-repl-rename-workspace
       :desc "Push workspace to second-to-last" "p" #'agent-repl-workspace-push-to-back
       :desc "Pull workspace to second" "P" #'agent-repl-workspace-pull-to-front
       :desc "Open most recent workspace" "R" #'agent-repl-open-most-recent-workspace))

(map! "s-{" #'agent-repl-switch-left
      "s-}" #'agent-repl-switch-right)

(map! :leader
      :desc "Switch to 1st workspace"   "1" #'agent-repl-workspace-switch-to-0
      :desc "Switch to 2nd workspace"   "2" #'agent-repl-workspace-switch-to-1
      :desc "Switch to 3rd workspace"   "3" #'agent-repl-workspace-switch-to-2
      :desc "Switch to 4th workspace"   "4" #'agent-repl-workspace-switch-to-3
      :desc "Switch to 5th workspace"   "5" #'agent-repl-workspace-switch-to-4
      :desc "Switch to 6th workspace"   "6" #'agent-repl-workspace-switch-to-5
      :desc "Switch to 7th workspace"   "7" #'agent-repl-workspace-switch-to-6
      :desc "Switch to 8th workspace"   "8" #'agent-repl-workspace-switch-to-7
      :desc "Switch to 9th workspace"   "9" #'agent-repl-workspace-switch-to-8
      :desc "Switch to final workspace" "0" #'agent-repl-workspace-switch-to-final)

;; Workspace-jump chords (M-1..M-9 / M-0 and s-1..s-9 / s-0) must beat:
;;
;; On this macOS setup Option emits `M-' (`ns-option-modifier' = meta)
;; and Command emits `s-' (`ns-command-modifier' = super), so the two
;; digit rows address two DIFFERENT nines:
;;   - Command `s-1..s-9' -> the FIRST nine workspaces (indices 0-8).
;;   - Option  `M-1..M-9' -> the SECOND nine workspaces (indices 9-17);
;;     the key digits stay 1-9 but land on workspaces 10-18.
;;   - Both `M-0' and `s-0' -> the final (last) workspace.
;;
;;   - Doom default's `:n "s-9" #'+workspace/switch-to-final'
;;     (modules/config/default/config.el:356, normal state only) which
;;     would otherwise route Cmd+9 to the LAST workspace from normal
;;     state instead of the 9th.
;;   - Doom default's `"s-0" #'doom/reset-font-size'
;;     (modules/config/default/config.el:328) which would otherwise
;;     route Cmd+0 to `text-scale-set' and emit "The font hasn't been
;;     resized" when font size is already default.
;;   - `vterm-mode-map's blanket `M-X' -> `vterm--self-insert-meta'
;;     binding (vterm.el:633-660) which would swallow `M-1..M-9 / M-0'
;;     inside vterm buffers and send the byte to the shell.
;;
;; A plain `(map! :g ...)' binding lands in `global-map' and loses to
;; both the Doom `:n' entry (in `evil-normal-state-map') and the vterm
;; major-mode entry.  Mirror the
;; `agent-repl--install-drawer-visit-override' pattern instead:
;; install the chord into `general-override-mode-map' at top-level AND
;; into its evil aux maps for every evil state, so the binding wins
;; lookup regardless of evil state and regardless of major mode.
;;
;; Sourced from the prefix-arg-free wrappers in
;; `modules/app/agent-repl/commands.el' so `current-prefix-arg' cannot
;; redirect the jump (the original M-9 -> final / M-0 -> font-resize
;; bug).
(defconst agent-repl--workspace-jump-chords
  '(("M-1" . agent-repl-workspace-switch-to-9)
    ("M-2" . agent-repl-workspace-switch-to-10)
    ("M-3" . agent-repl-workspace-switch-to-11)
    ("M-4" . agent-repl-workspace-switch-to-12)
    ("M-5" . agent-repl-workspace-switch-to-13)
    ("M-6" . agent-repl-workspace-switch-to-14)
    ("M-7" . agent-repl-workspace-switch-to-15)
    ("M-8" . agent-repl-workspace-switch-to-16)
    ("M-9" . agent-repl-workspace-switch-to-17)
    ("M-0" . agent-repl-workspace-switch-to-final)
    ("s-1" . agent-repl-workspace-switch-to-0)
    ("s-2" . agent-repl-workspace-switch-to-1)
    ("s-3" . agent-repl-workspace-switch-to-2)
    ("s-4" . agent-repl-workspace-switch-to-3)
    ("s-5" . agent-repl-workspace-switch-to-4)
    ("s-6" . agent-repl-workspace-switch-to-5)
    ("s-7" . agent-repl-workspace-switch-to-6)
    ("s-8" . agent-repl-workspace-switch-to-7)
    ("s-9" . agent-repl-workspace-switch-to-8)
    ("s-0" . agent-repl-workspace-switch-to-final))
  "Alist of (KEY-STRING . COMMAND) for the workspace-jump chords that
must win key lookup above Doom default's `:n s-9' / `s-0', above
`vterm-mode-map's `M-X' blanket bindings, and across every evil
state.  Command `s-1..s-9' address the FIRST nine workspaces and Option
`M-1..M-9' address the SECOND nine (workspaces 10-18); `M-0'/`s-0' both
address the final workspace.  Each KEY-STRING is passed to `kbd' at
install time.")

(defun agent-repl--install-workspace-jump-overrides ()
  "Install `agent-repl--workspace-jump-chords' into
`general-override-mode-map' at top-level AND into its evil intercept
aux maps for every state in `agent-repl--scroll-output-intercept-states'
\(reused as the canonical \"all evil states\" list).  Idempotent."
  (dolist (entry agent-repl--workspace-jump-chords)
    (let ((seq (kbd (car entry)))
          (cmd (cdr entry)))
      (define-key general-override-mode-map seq cmd)
      (when (fboundp 'evil-get-auxiliary-keymap)
        (dolist (state agent-repl--scroll-output-intercept-states)
          (define-key (evil-get-auxiliary-keymap
                       general-override-mode-map state t t)
                      seq cmd))))))

(agent-repl--install-workspace-jump-overrides)

;; SPC j -- Tell the agent to do a predefined thing
(map! :leader
      (:prefix ("j" . "claude")
       :desc "Enqueue input as deferred prompt"        "RET" #'agent-repl-queue-deferred-prompt
       :desc "One-shot doom edit (from master)"        "o" #'agent-repl-create-doom-oneshot-workspace
       :desc "One-shot explanation-engine edit (PR on success)" "O" #'agent-repl-create-explanation-engine-oneshot-workspace
       :desc "Amend last doom one-shot (send/queue)"   "C-o" #'agent-repl-amend-doom-oneshot-prompt
       :desc "Amend last explanation-engine one-shot (send/queue)" "C-S-o" #'agent-repl-amend-explanation-engine-oneshot-prompt
       :desc "Kill workspace"           "d" #'agent-repl-kill-workspace
       :desc "Update GitHub PR description"  "r" #'agent-repl-update-pr
       :desc "Rebase branch onto origin/master" "b" #'agent-repl-rebase-onto-origin-master
       :desc "Nuke workspace"           "x" #'agent-repl-nuke-workspace
       :desc "Nuke ALL workspaces"      "X" #'agent-repl-nuke-all-workspaces
       :desc "Paste workspace clipboard" "p" #'agent-repl-paste-clipboard
       :desc "Toggle debug logging"    "D" #'agent-repl-debug/toggle-logging
       (:prefix ("h" . "help/debug")
        :desc "Dump workspace state"     "p" #'agent-repl-debug/dump-workspace
        :desc "Explain config (read-only Q&A)" "c" #'agent-repl-explain-config
        :desc "Close explain-config popup"     "C" #'agent-repl-explain-config-close
        :desc "New explain-config conversation" "n" #'agent-repl-explain-config-reset)
       (:prefix ("e" . "explain")
        :desc "line/region/hunk (prompt)" "e" #'agent-repl-explain-prompt
        :desc "line/region/hunk (canned)" "E" #'agent-repl-explain
        (:prefix ("d" . "diff")
         :desc "worktree"    "w" #'agent-repl-explain-diff-worktree
         :desc "staged"      "s" #'agent-repl-explain-diff-staged
         :desc "uncommitted" "u" #'agent-repl-explain-diff-uncommitted
         :desc "HEAD"        "h" #'agent-repl-explain-diff-head
         :desc "branch"      "b" #'agent-repl-explain-diff-branch))
       :desc "Reload agent-repl config" "R" #'agent-repl-reload-config
       (:prefix ("s" "Send predefined input to Claude")
        :desc "create PR (no --self-certified)"       "p"   #'agent-repl-create-or-update-pr-no-self-certified
        :desc "create PR"                             "P"   #'agent-repl-create-or-update-pr
        :desc "paste: create PR (no --self-certified)" "C-p" #'agent-repl-create-or-update-pr-no-self-certified-paste
        :desc "paste: create PR"                       "C-S-p" #'agent-repl-create-or-update-pr-paste)
       (:prefix ("m" . "modify workspace")
        :desc "Set/change priority" "p" #'agent-repl-set-priority)
       (:prefix ("t" . "tests")
        (:prefix ("r" . "run")
         (:prefix ("t" . "tests")
          :desc "worktree"    "w" #'agent-repl-run-tests-worktree
          :desc "staged"      "s" #'agent-repl-run-tests-staged
          :desc "uncommitted" "u" #'agent-repl-run-tests-uncommitted
          :desc "HEAD"        "h" #'agent-repl-run-tests-head
          :desc "branch"      "b" #'agent-repl-run-tests-branch)
         (:prefix ("l" . "lint")
          :desc "worktree"    "w" #'agent-repl-run-lint-worktree
          :desc "staged"      "s" #'agent-repl-run-lint-staged
          :desc "uncommitted" "u" #'agent-repl-run-lint-uncommitted
          :desc "HEAD"        "h" #'agent-repl-run-lint-head
          :desc "branch"      "b" #'agent-repl-run-lint-branch)
         (:prefix ("a" . "all")
          :desc "worktree"    "w" #'agent-repl-run-all-worktree
          :desc "staged"      "s" #'agent-repl-run-all-staged
          :desc "uncommitted" "u" #'agent-repl-run-all-uncommitted
          :desc "HEAD"        "h" #'agent-repl-run-all-head
          :desc "branch"      "b" #'agent-repl-run-all-branch))
        (:prefix ("a" . "analyze")
         (:prefix ("q" . "quality")
          :desc "worktree"    "w" #'agent-repl-test-quality-worktree
          :desc "staged"      "s" #'agent-repl-test-quality-staged
          :desc "uncommitted" "u" #'agent-repl-test-quality-uncommitted
          :desc "HEAD"        "h" #'agent-repl-test-quality-head
          :desc "branch"      "b" #'agent-repl-test-quality-branch)
         (:prefix ("c" . "coverage")
          :desc "worktree"    "w" #'agent-repl-test-coverage-worktree
          :desc "staged"      "s" #'agent-repl-test-coverage-staged
          :desc "uncommitted" "u" #'agent-repl-test-coverage-uncommitted
          :desc "HEAD"        "h" #'agent-repl-test-coverage-head
          :desc "branch"      "b" #'agent-repl-test-coverage-branch)))))

(dotimes (i 10)
  (define-key doom-leader-map (kbd (format "o %s" i))
    #'agent-repl--send-digit-char))

(map! :leader "b R" #'agent-repl-revert-and-eval-buffer)
(map! :leader "m e B" #'agent-repl-revert-and-eval-buffer)

;;; Section 5: Advice registrations

;; TODO: This +workspace/kill advice is a behavioral hook, not a keybinding.
;; It belongs in session.el or panels.el.  Do not move yet -- other agents are
;; modifying those files.

;; Kill the agent session before workspace deletion so buffers/windows are cleaned
;; up while the workspace is still current.
(agent-repl--ws-advise-kill-before #'agent-repl--kill-before-workspace-delete)

