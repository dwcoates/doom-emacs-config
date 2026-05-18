;;; keybindings.el --- keybindings and debug helpers -*- lexical-binding: t; -*-

;;; Section 1: Internal helpers

(defconst claude-repl--output-dir (expand-file-name "~/.claude/output/")
  "Directory for workspace command files and other IPC output.")

(defcustom claude-repl-debug-mock-workspace-default-name "DWC/mock-test"
  "Default branch name used in mock workspace generation."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-debug-mock-priority-branch-default "DWC/mock-priority-test"
  "Default branch name used in mock workspace priority generation."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-workspace-commands-file-regexp "^workspace_commands_.*\\.json$"
  "Regexp matching workspace command files in the output directory."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-dump-buffer-name "*claude-repl-dump*"
  "Buffer name for workspace state dump output."
  :type 'string
  :group 'claude-repl)

;;;; Faces for `claude-repl-debug/dump-workspace' ---------------------------

(defface claude-repl-dump-title
  '((t :weight bold :height 1.6 :inherit font-lock-function-name-face))
  "Face for the workspace title line in `claude-repl-debug/dump-workspace'."
  :group 'claude-repl)

(defface claude-repl-dump-section
  '((t :weight bold :height 1.25 :inherit font-lock-keyword-face))
  "Face for section headers in `claude-repl-debug/dump-workspace'."
  :group 'claude-repl)

(defface claude-repl-dump-key
  '((t :weight bold :inherit font-lock-variable-name-face))
  "Face for plist keys in `claude-repl-debug/dump-workspace'."
  :group 'claude-repl)

(defface claude-repl-dump-rule
  '((t :inherit shadow))
  "Face for the rule line beneath the title in
`claude-repl-debug/dump-workspace'."
  :group 'claude-repl)

;;;; Section layout for `claude-repl-debug/dump-workspace' ------------------

(defconst claude-repl--dump-sections
  '(("🏷️  Identity"
     (:name :ws-id :priority :group-key))
    ("⚡ State"
     (:claude-state :repl-state :status :stop-received
      :flashing :hidden :dead :bogus :merged))
    ("🌳 Project / Git"
     (:project-dir :worktree-p :source-ws-dir :source-ws-name
      :merge-parent-dir :branch-merged :branch-merged-last-check
      :detail-branch :detail-dirty-count :detail-last-commit
      :detail-last-commit-time :detail-master-ahead :detail-source-ahead
      :git-clean :git-proc))
    ("🧠 Session"
     (:session-id :fork-session-id :vterm-buffer :active-env
      :sandbox :bare-metal :claude-ready :ws-loaded :ready-timer))
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
  "Section layout for `claude-repl-debug/dump-workspace'.
Each entry is (TITLE KEYS).  TITLE is the section header string (with a
leading emoji); KEYS is the list of plist keys that belong in that
section, in display order.  Any key present in the workspace plist that
is not listed in any section falls through to the
`claude-repl--dump-other-section' bucket at the end of the dump.")

(defconst claude-repl--dump-other-section "📦 Other"
  "Section header used for plist keys not classified by
`claude-repl--dump-sections'.")

(defun claude-repl--format-dump-value (val)
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

(defun claude-repl--dump-plist-to-alist (plist)
  "Convert PLIST to an alist of (KEY . VALUE), preserving insertion order."
  (let (result)
    (while plist
      (let ((k (pop plist))
            (v (pop plist)))
        (push (cons k v) result)))
    (nreverse result)))

(defun claude-repl--dump-insert-row (key val)
  "Insert one KEY/VAL row at point in the current buffer.
KEY is rendered with `claude-repl-dump-key' face; VAL is rendered via
`claude-repl--format-dump-value' with no face."
  (insert "  ")
  (insert (propertize (format "%-30s" (symbol-name key))
                      'face 'claude-repl-dump-key))
  (insert "  ")
  (insert (claude-repl--format-dump-value val))
  (insert "\n"))

(defun claude-repl--dump-insert-section (title rows)
  "Insert section TITLE followed by ROWS (an alist of (KEY . VALUE)).
No-op when ROWS is empty so empty sections do not clutter the output."
  (when rows
    (insert "\n")
    (insert (propertize title 'face 'claude-repl-dump-section))
    (insert "\n")
    (dolist (row rows)
      (claude-repl--dump-insert-row (car row) (cdr row)))))

(defun claude-repl--dump-partition (alist sections)
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
    (push (cons claude-repl--dump-other-section remaining) result)
    (nreverse result)))

(defun claude-repl--cons-name-state (name)
  "Return (NAME . claude-state) for workspace NAME."
  (cons name (claude-repl--ws-claude-state name)))

(defun claude-repl--format-workspace-state (pair)
  "Format a (NAME . STATE) PAIR as an indented diagnostic string."
  (format "  %s: %s" (car pair) (or (cdr pair) "nil")))

(defun claude-repl--format-buffer-info (buf)
  "Format BUF's name, owning workspace, and persp workspace as a diagnostic string."
  (format "  %s  owning=%s  persp=%s"
          (buffer-name buf)
          (or (buffer-local-value 'claude-repl--owning-workspace buf) "nil")
          (or (claude-repl--workspace-for-buffer buf) "nil")))

(defun claude-repl--kill-before-workspace-delete (&optional name &rest _)
  "Before-advice for `+workspace/kill': tear down any running Claude session.
NAME is the workspace `+workspace/kill' was invoked on.  Only fire when
NAME refers to the current workspace — `claude-repl--claude-running-p'
inspects the current ws's vterm, so applying it cross-workspace would
kill the wrong session (e.g. when the hide-mode sweep persp-kills a
background `:hidden' workspace from inside a workspace-switch handler,
the named workspace's session has already been torn down by the sweep
and the current workspace's session must be left alone).  Callers that
need to kill a specific named workspace's session (the nuke / kill /
sweep paths) handle teardown explicitly via `claude-repl--kill-session'
before invoking `+workspace/kill'."
  (let ((target (or name (+workspace-current-name)))
        (current (+workspace-current-name)))
    (claude-repl--log current
                      "kill-before-workspace-delete: target=%s current=%s"
                      target current)
    (cond
     ((not (equal target current))
      (claude-repl--log current
                        "kill-before-workspace-delete: target!=current, skipping (caller handles teardown)"))
     ((claude-repl--claude-running-p)
      (claude-repl--log current "kill-before-workspace-delete: vterm running, killing session")
      (claude-repl-kill))
     (t
      (claude-repl--log current "kill-before-workspace-delete: vterm not running, no-op")))))

(defun claude-repl--read-workspace (prompt)
  "Prompt for a workspace name with PROMPT.  Requires an exact match."
  (completing-read prompt (+workspace-list-names) nil t))

(defun claude-repl--read-workspace-with-default (prompt)
  "Prompt for a workspace name with PROMPT, defaulting to the current workspace."
  (completing-read prompt (+workspace-list-names) nil t
                   nil nil (+workspace-current-name)))

(defun claude-repl--read-known-workspace (prompt)
  "Prompt for a workspace registered in `claude-repl--workspaces'.
Defaults to the current workspace when it is registered (so RET picks
the obvious target).  Signals `user-error' when no workspaces exist.

Filters out tombstoned entries via `claude-repl--live-ws-names' — a
nuked workspace's identity record survives in the hash for
`--ws-dir' callers, but it must not surface in interactive pickers."
  (let* ((known (claude-repl--live-ws-names))
         (current (and (fboundp '+workspace-current-name)
                       (+workspace-current-name)))
         (default (and current (member current known) current)))
    (unless known (user-error "No claude-repl workspaces registered"))
    (completing-read prompt known nil t nil nil default)))

(defun claude-repl--write-output-json (filename content)
  "Write CONTENT as JSON to FILENAME inside `claude-repl--output-dir'.
Ensures the output directory exists.  Returns the full path of the written file."
  (make-directory claude-repl--output-dir t)
  (let ((file (expand-file-name filename claude-repl--output-dir)))
    (claude-repl--log (+workspace-current-name) "write-output-json: filename=%s dir=%s" filename claude-repl--output-dir)
    (with-temp-file file
      (insert (json-encode content)))
    file))

(defun claude-repl--list-claude-vterm-buffers ()
  "Return a list of live Claude vterm buffers (matching `claude-repl--vterm-buffer-re')."
  (cl-remove-if-not #'claude-repl--claude-buffer-p (buffer-list)))

;;; Section 2: Utility commands used by keybindings

;; SPC o 0-9: send a digit character to Claude from the leader keymap.
(defun claude-repl--send-digit-char ()
  "Send the digit from the current key event to Claude.
Extracts the trailing digit from the key sequence (e.g. SPC o 3 -> \"3\")."
  (interactive)
  (let* ((keys (this-command-keys-vector))
         (last-key (aref keys (1- (length keys)))))
    (claude-repl--log (+workspace-current-name) "send-digit-char: digit=%s" (string last-key))
    (claude-repl-send-char (string last-key))))

;; C-v paste forwarding to vterm
(defun claude-repl-paste-to-vterm ()
  "Forward a Ctrl-V keystroke to the Claude vterm buffer.
This lets Claude CLI handle paste natively, including images."
  (interactive)
  (claude-repl--log (+workspace-current-name) "paste-to-vterm: entry")
  (if (claude-repl--vterm-live-p)
      (progn
        (claude-repl--log (+workspace-current-name) "paste-to-vterm: vterm live, forwarding C-v")
        (with-current-buffer (claude-repl--ws-get (+workspace-current-name) :vterm-buffer)
          (vterm-send-key "v" nil nil t)))
    (user-error "No live Claude session — paste not forwarded")))

;; TODO: claude-repl-set-priority belongs in commands.el rather than
;; keybindings.el.  Do not move yet -- other agents are modifying that file.
(defconst claude-repl--priority-remove-label "*remove*"
  "Label shown in the priority completion list for the remove option.
Maps to the empty-string priority value when chosen.  Used because
the clear sentinel has no badge image and an empty string cannot
carry a `display' text property in any usable way.

Only offered when the current workspace already has a priority — when
there is nothing to remove, the entry is omitted from the candidate
list to avoid presenting a no-op choice.")

(defun claude-repl--decorate-priority-candidate (priority)
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
  (let ((img (and (fboundp 'claude-repl--priority-image)
                  (claude-repl--priority-image priority))))
    (if img
        (propertize priority 'display img)
      priority)))

(defun claude-repl--read-priority (prompt default)
  "Prompt for a priority level using PROMPT, defaulting to DEFAULT.
Candidates are the entries in `claude-repl-priority-levels' rendered
purely as their badge images (no accompanying text).  When DEFAULT is
a non-empty priority — meaning the workspace already has one set —
the textual `claude-repl--priority-remove-label' entry is appended,
mapping back to the empty-string \"clear\" sentinel when chosen.
When DEFAULT is empty or nil, the remove entry is omitted because
there is nothing to remove."
  (let* ((has-current (and default (not (string-empty-p default))))
         (candidates (append (mapcar #'claude-repl--decorate-priority-candidate
                                     claude-repl-priority-levels)
                             (when has-current
                               (list claude-repl--priority-remove-label))))
         (effective-default (and has-current default))
         (raw (completing-read prompt candidates nil t nil nil effective-default))
         (chosen (substring-no-properties raw)))
    (if (equal chosen claude-repl--priority-remove-label) "" chosen)))

(defun claude-repl-set-priority (priority &optional ws)
  "Set or change the priority badge for workspace WS.
WS defaults to the current workspace.  PRIORITY is one of the strings
in `claude-repl-priority-levels', or \"\" to clear.  Persists through
`claude-repl--state-save' so the badge survives restarts, reorders the
workspace in the tab-bar by its new priority, and forces a mode-line
repaint so the glyph updates immediately.  Pulses the workspace's tab
via `claude-repl-flash-tab' so the user can spot the slot whose
priority just shifted (matches the `SPC p p' / worktree-jump flash
semantic).

Interactively, always targets the current workspace and prompts only
for the priority (defaulting to the workspace's current priority, if
any).  Each candidate in the prompt is annotated with its badge
image so the visual mapping between key and glyph is obvious."
  (interactive
   (let* ((target (+workspace-current-name))
          (current (claude-repl--ws-get target :priority))
          (prompt (format "Priority%s: "
                          (if current (format " (current: %s)" current) "")))
          (priority (claude-repl--read-priority prompt (or current ""))))
     (list priority target)))
  (let* ((ws-explicit-p (not (null ws)))
         (ws (or ws (+workspace-current-name)))
         (old-priority (claude-repl--ws-get ws :priority))
         (new-priority (if (string-empty-p priority) nil priority))
         (had-entry (not (null (gethash ws claude-repl--workspaces))))
         (cache-before (if (boundp 'persp-names-cache) persp-names-cache "(unbound)")))
    (claude-repl--log ws "set-priority: ws=%s ws-explicit=%s had-entry=%s priority %s -> %s cache=%S"
                      ws (if ws-explicit-p "t" "nil") (if had-entry "t" "nil")
                      (or old-priority "nil") (or new-priority "(cleared)")
                      cache-before)
    (claude-repl--ws-put ws :priority new-priority)
    (claude-repl--state-save ws)
    (claude-repl--reorder-workspace-by-priority ws)
    (force-mode-line-update t)
    (when (fboundp 'claude-repl-flash-tab)
      (claude-repl-flash-tab ws))
    (message "Workspace '%s' priority: %s" ws (if (string-empty-p priority) "cleared" priority))))

;; SPC b R -- revert buffer from disk then eval as Elisp (fast config reload)
(defun claude-repl-revert-and-eval-buffer ()
  "Revert the current buffer from disk, then evaluate it as Elisp."
  (interactive)
  (claude-repl--log (+workspace-current-name) "revert-and-eval-buffer: entry buffer=%s" (buffer-name))
  (revert-buffer :ignore-auto :noconfirm)
  (eval-buffer))

;; SPC j R -- reload the claude-repl module's config.el (the claude
;; workspace's config), independent of whatever buffer is current.
(defun claude-repl--reload-config-file ()
  "Return the config.el path to reload for the current workspace.

Prefers `<project-dir>/modules/app/claude-repl/config.el' when it exists,
so reloading inside a doom-config worktree (e.g.
`~/.config/doom-worktrees/foo/') picks up THAT worktree's checkout
rather than the root `~/.config/doom' copy the module was originally
loaded from.  Falls back to `claude-repl--config-file' (the original
load path) for non-doom-config workspaces, unregistered workspaces, or
workspaces with no `:project-dir'."
  (let* ((ws (+workspace-current-name))
         (proj (and ws (claude-repl--ws-get ws :project-dir)))
         (candidate (and proj (expand-file-name "modules/app/claude-repl/config.el" proj))))
    (if (and candidate (file-exists-p candidate))
        candidate
      claude-repl--config-file)))

(defun claude-repl-reload-config ()
  "Reload the claude-repl module config for the current workspace.
Resolves the config path via `claude-repl--reload-config-file' so a
doom-config worktree reloads its own checkout."
  (interactive)
  (let ((file (claude-repl--reload-config-file)))
    (claude-repl--log (+workspace-current-name) "reload-config: file=%s" file)
    (load-file file)
    (message "[claude-repl] Reloaded %s" file)))

;;; Section 3: Debug helpers -- interactive commands for diagnosing workspace state issues.
;;; Call via M-x claude-repl-debug/...

(defun claude-repl-debug/cancel-timers ()
  "Cancel all claude-repl timers."
  (interactive)
  (claude-repl--cancel-all-timers)
  (message "Cancelled all claude-repl timers."))

(defun claude-repl-debug/mock-workspace-generation (&optional names)
  "Write a mock workspace_generation.json to trigger the file watcher.
NAMES is an optional list of branch name strings; defaults to a single test entry."
  (interactive)
  (let* ((names (or names (list claude-repl-debug-mock-workspace-default-name)))
         (file (claude-repl--write-output-json "workspace_generation.json" names)))
    (claude-repl--log (+workspace-current-name) "mock workspace-generation file written: %s names=%s" file names)
    (message "Wrote mock workspace_generation.json: %s" names)))

(defun claude-repl-debug/mock-workspace-commands-with-priority ()
  "Write a mock workspace_commands file with a priority field to test image badges."
  (interactive)
  (let* ((priority (completing-read "Priority: " claude-repl-priority-levels nil t))
         (name (read-string "Branch name: " claude-repl-debug-mock-priority-branch-default))
         (filename (format "workspace_commands_%s.json" (format-time-string "%s")))
         (commands (vector `((type . "create")
                             (name . ,name)
                             (priority . ,priority))))
         (file (claude-repl--write-output-json filename commands)))
    (message "Wrote %s with priority=%s" file priority)))

(defun claude-repl-debug/process-pending-commands ()
  "Manually scan ~/.claude/output/ and process any workspace_commands_*.json files.
Use this to verify the processor works independently of the file watcher."
  (interactive)
  (let ((files (when (file-directory-p claude-repl--output-dir)
                 (directory-files claude-repl--output-dir t
                                  claude-repl-workspace-commands-file-regexp))))
    (if (not files)
        (message "No workspace_commands_*.json files found in %s"
                 claude-repl--output-dir)
      (message "Found %d file(s), processing..." (length files))
      (dolist (file files)
        (claude-repl--log (+workspace-current-name) "process-pending-commands: processing file=%s" file)
        (claude-repl--process-workspace-commands-file file)))))

(defun claude-repl-debug/workspace-states ()
  "Display all workspace states."
  (interactive)
  (let ((states (mapcar #'claude-repl--cons-name-state (+workspace-list-names))))
    (message "Workspace states:\n%s"
             (mapconcat #'claude-repl--format-workspace-state states "\n"))))

(defun claude-repl-debug/buffer-info ()
  "Display all claude vterm buffers with their owning and persp workspaces."
  (interactive)
  (let* ((bufs (claude-repl--list-claude-vterm-buffers))
         (lines (mapcar #'claude-repl--format-buffer-info bufs)))
    (message "Claude buffers:\n%s"
             (if lines (mapconcat #'identity lines "\n") "  (none)"))))

(defun claude-repl-debug/clear-state (ws)
  "Clear all states for workspace WS without killing buffers."
  (interactive (list (claude-repl--read-workspace "Workspace: ")))
  (dolist (state '(:thinking :done :permission :inactive))
    (claude-repl--ws-claude-state-clear-if ws state))
  (message "Cleared all states for %s" ws))

(defun claude-repl--kill-owned-panel-buffers (ws)
  "Kill all Claude panel buffers owned by workspace WS.
Closes their windows (selected-frame, to preserve historical scope)
and silences process exit queries before killing."
  (claude-repl--log ws "kill-owned-panel-buffers: entry ws=%s" ws)
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (claude-repl--claude-panel-buffer-p buf)
               (equal ws (buffer-local-value 'claude-repl--owning-workspace buf)))
      (claude-repl--log ws "kill-owned-panel-buffers: killing buffer=%s" (buffer-name buf))
      (claude-repl-window--delete-buffer-windows buf :all-frames nil)
      (let ((proc (get-buffer-process buf)))
        (when proc (set-process-query-on-exit-flag proc nil)))
      (kill-buffer buf))))

(defun claude-repl-debug/obliterate (ws)
  "Completely remove workspace WS from all claude-repl tracking.
Kills claude buffers, closes windows, and removes all state."
  (interactive (list (claude-repl--read-workspace "Obliterate workspace: ")))
  (claude-repl--log ws "debug/obliterate: entry ws=%s" ws)
  (claude-repl--kill-owned-panel-buffers ws)
  (claude-repl--ws-del ws)
  (message "Obliterated all claude-repl state for %s" ws))

(defun claude-repl-debug/set-owning-workspace ()
  "Set the owning workspace for a claude vterm buffer."
  (interactive)
  (let* ((bufs (claude-repl--list-claude-vterm-buffers))
         (buf-name (completing-read "Buffer: " (mapcar #'buffer-name bufs) nil t))
         (ws (claude-repl--read-workspace "Owning workspace: ")))
    (with-current-buffer buf-name
      (setq-local claude-repl--owning-workspace ws))
    (message "Set %s owning workspace to %s" buf-name ws)))

(defun claude-repl-debug/toggle-logging (&optional verbose)
  "Toggle debug logging.
Without prefix argument: cycle nil → t → nil.
With prefix argument (\\[universal-argument]): cycle nil → verbose → nil.
Verbose mode additionally logs high-frequency events (timer ticks,
window changes, git-diff sentinels, resolve-root, etc.)."
  (interactive "P")
  (setq claude-repl-debug
        (if verbose
            (if (eq claude-repl-debug 'verbose) nil 'verbose)
          (if claude-repl-debug nil t)))
  (let ((label (pcase claude-repl-debug
                 ('nil "OFF")
                 ('t   "ON")
                 ('verbose "ON (verbose)")
                 (_ (error "claude-repl-debug has unexpected value: %S" claude-repl-debug)))))
    ;; Always emit via message so it's visible even when logging is off.
    (message "[claude-repl] debug logging: %s" label)
    ;; Also emit via the log system so it appears in the log stream.
    (when claude-repl-debug
      (claude-repl--log (+workspace-current-name) "debug logging toggled: %s" label))))

(defun claude-repl-debug/toggle-log-to-file ()
  "Toggle writing debug log output to `~/.claude/emacs/doom-claude-repl.log'.
When enabled, all messages that pass through `claude-repl--do-log' are
appended to the file regardless of the `claude-repl-debug' level."
  (interactive)
  (setq claude-repl-log-to-file (not claude-repl-log-to-file))
  (let ((label (if claude-repl-log-to-file "ON" "OFF"))
        (path (claude-repl--logfile-path)))
    (message "[claude-repl] log-to-file: %s%s"
             label
             (if path (format " (%s)" path) ""))))

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

(defun claude-repl-debug/dump-workspace ()
  "Display the full serialized plist for a selected workspace from the hashmap.
Prompts to select from workspaces registered in `claude-repl--workspaces',
defaulting to the current workspace when registered.

Output is organized into emoji-prefixed sections (Identity, State,
Project / Git, Session, Prompts, Notifications, Merge, UI / Panels,
Counters) per `claude-repl--dump-sections', with any unclassified keys
emitted under the `Other' bucket.  Section headers and the workspace
title are rendered with `claude-repl-dump-section' / `claude-repl-dump-title'
faces so they stand out visually in the help buffer."
  (interactive)
  (let* ((ws (claude-repl--read-known-workspace "Dump workspace: "))
         (plist (gethash ws claude-repl--workspaces))
         (alist (claude-repl--dump-plist-to-alist plist))
         (partition (claude-repl--dump-partition
                     alist claude-repl--dump-sections)))
    (with-help-window claude-repl-dump-buffer-name
      (with-current-buffer claude-repl-dump-buffer-name
        (insert (propertize (format "Workspace: %s" ws)
                            'face 'claude-repl-dump-title))
        (insert "\n")
        (insert (propertize (make-string 60 ?─)
                            'face 'claude-repl-dump-rule))
        (insert "\n")
        (dolist (section partition)
          (claude-repl--dump-insert-section (car section) (cdr section)))))))

(defun claude-repl-debug/workspace-clean-p (ws-name)
  "Show whether workspace WS-NAME has unstaged changes to tracked files.
Uses `claude-repl--workspace-clean-p' -- the same function used in production."
  (interactive (list (claude-repl--read-workspace-with-default "Workspace: ")))
  (let ((clean (claude-repl--workspace-clean-p ws-name)))
    (message "Workspace %s: %s" ws-name (if clean "clean" "dirty"))))

(defun claude-repl-debug/--gather-ws-diagnostics (ws-name)
  "Gather diagnostic information about workspace WS-NAME.
Returns a plist with keys :vterm-buf :proc-alive :owning-ws :has-window
:claude-open :dirty."
  (let* ((open (claude-repl--ws-claude-open-p ws-name))
         (dirty (not (claude-repl--workspace-clean-p ws-name)))
         (persp (persp-get-by-name ws-name))
         (persp-bufs (and persp (not (symbolp persp)) (persp-buffers persp)))
         (vterm-buf (cl-loop for buf in persp-bufs
                             when (and (buffer-live-p buf)
                                       (claude-repl--claude-buffer-p buf))
                             return buf))
         (proc (and vterm-buf (get-buffer-process vterm-buf)))
         (proc-alive (and proc (process-live-p proc)))
         (owning-ws (and vterm-buf
                         (buffer-local-value 'claude-repl--owning-workspace vterm-buf)))
         (has-window (and vterm-buf (get-buffer-window vterm-buf t))))
    (list :vterm-buf vterm-buf :proc-alive proc-alive
          :owning-ws owning-ws :has-window has-window
          :claude-open open :dirty dirty)))

(defun claude-repl-debug/--apply-state-refresh (ws-name claude-open)
  "Apply a state refresh for WS-NAME given whether CLAUDE-OPEN is non-nil.
Mirrors the logic in `claude-repl--update-all-workspace-states'."
  (if claude-open
      (claude-repl--update-ws-state ws-name)
    (claude-repl--mark-dead-vterm ws-name)))

(defun claude-repl-debug/--format-diagnostics (ws-name diag before after)
  "Format a diagnostic summary string for WS-NAME.
DIAG is the plist from `claude-repl-debug/--gather-ws-diagnostics'.
BEFORE and AFTER are the workspace states before and after refresh."
  (let ((vterm-buf (plist-get diag :vterm-buf)))
    (format (concat "Workspace %s:\n"
                    "  vterm-buf=%s process=%s\n"
                    "  owning-ws=%s has-window=%s\n"
                    "  claude-open=%s dirty=%s\n"
                    "  state=%s -> %s")
            ws-name
            (and vterm-buf (buffer-name vterm-buf))
            (if (plist-get diag :proc-alive) "alive" "dead/nil")
            (or (plist-get diag :owning-ws) "nil")
            (if (plist-get diag :has-window) "yes" "no")
            (if (plist-get diag :claude-open) "yes" "no")
            (if (plist-get diag :dirty) "yes" "no")
            (or before "nil") (or after "nil"))))

(defun claude-repl-debug/refresh-state (ws-name)
  "Force a full state refresh for workspace WS-NAME.
Runs the same logic as the periodic `update-all-workspace-states' timer:
checks claude visibility, git dirty status, and applies the state table.
Reports comprehensive diagnostics."
  (interactive (list (claude-repl--read-workspace-with-default "Workspace: ")))
  (let* ((before (claude-repl--ws-claude-state ws-name))
         (diag (claude-repl-debug/--gather-ws-diagnostics ws-name)))
    (claude-repl-debug/--apply-state-refresh ws-name (plist-get diag :claude-open))
    (let ((after (claude-repl--ws-claude-state ws-name)))
      (force-mode-line-update t)
      (message "%s" (claude-repl-debug/--format-diagnostics ws-name diag before after)))))

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
(map! "C-S-n"        #'claude-repl-drawer-global-next)
(map! "C-S-p"        #'claude-repl-drawer-global-prev)
(map! "C-S-x"        #'claude-repl-drawer-global-nuke)
(map! "C-S-d"        #'claude-repl-drawer-global-kill)
(map! "C-S-i"        #'claude-repl-drawer-global-send-prompt)
(map! "C-S-m"        #'claude-repl-drawer-global-merge-into-master)
(map! "C-S-h"        #'claude-repl-drawer-global-toggle-hidden)
(map! "C-S-t"        #'claude-repl-drawer-global-toggle-mark)
(map! "C-S-u"        #'claude-repl-drawer-global-clear-marks)
(map! "C-S-+"        #'claude-repl-drawer-global-priority-up)
(map! "C-S--"        #'claude-repl-drawer-global-priority-down)

;; `C-S-j' / `C-S-k' need a stronger binding than the plain global-map
;; entry the rest of the drawer chords use, because `config.el' wires
;; `:nv "C-j" -> evil-window-down' / `:nv "C-k" -> evil-window-up' into
;; `general-override-mode-map''s evil intercept aux maps for
;; normal/visual.  When the user presses `C-S-j' in normal state,
;; `read-key-sequence' looks up `[?\C-\S-j]' across the active keymaps;
;; if no map binds the shifted key, Emacs performs shift-translation,
;; retrying as `[?\C-j]' -- which HITS the intercept aux map and fires
;; `evil-window-down', shadowing the global-map binding to
;; `claude-repl-scroll-output-down'.  Defeat the fallback by planting
;; explicit `C-S-j' / `C-S-k' entries in the same intercept aux maps
;; (for every evil state), and a matching top-level entry in
;; `general-override-mode-map' for non-evil contexts.
(defconst claude-repl--scroll-output-chords
  '(("C-S-j" . claude-repl-scroll-output-down)
    ("C-S-k" . claude-repl-scroll-output-up))
  "Alist of (KEY-STRING . COMMAND) for scroll-output chords that must
win key lookup above any minor-mode-map and any evil intercept aux map
-- specifically defeating shift-translation back to `C-j' / `C-k' which
would otherwise route the chord to `evil-window-down/up'.")

(defconst claude-repl--scroll-output-intercept-states
  '(normal visual insert emacs operator motion replace)
  "Evil states for which `claude-repl--scroll-output-chords' install
intercept aux map entries on `general-override-mode-map'.  Covers every
evil state so the chord wins regardless of which state is current.")

(defun claude-repl--install-scroll-output-overrides ()
  "Install `claude-repl--scroll-output-chords' into
`general-override-mode-map' at top-level AND into its evil intercept
aux maps for every state in `claude-repl--scroll-output-intercept-states'.
The top-level entry covers non-evil contexts; the per-state aux entries
beat any same-state evil binding and prevent shift-translation fallback.
Idempotent."
  (dolist (entry claude-repl--scroll-output-chords)
    (let ((seq (kbd (car entry)))
          (cmd (cdr entry)))
      (define-key general-override-mode-map seq cmd)
      (when (fboundp 'evil-get-auxiliary-keymap)
        (dolist (state claude-repl--scroll-output-intercept-states)
          (define-key (evil-get-auxiliary-keymap
                       general-override-mode-map state t t)
                      seq cmd))))))

(claude-repl--install-scroll-output-overrides)

;; `C-S-<return>' -> `claude-repl-drawer-global-visit' needs the same
;; override treatment as the scroll chords -- a plain `(map! ... )' lands
;; in `global-map', which loses to Doom default's `:gi/:gn "C-S-RET"' ->
;; `+default/newline-above' (evil aux on global state maps) and to
;; `claude-input-mode-map's `:ni "C-S-RET"' major-mode aux.  Bind the
;; chord on `general-override-mode-map' AND its per-state aux maps so
;; it wins above all of them.
(defconst claude-repl--drawer-visit-chord
  '(("C-S-<return>" . claude-repl-drawer-global-visit))
  "Alist of (KEY-STRING . COMMAND) for the global drawer-visit chord
that must win key lookup above the Doom default's `:gi/:gn \"C-S-RET\"'
binding and above `claude-input-mode-map's `:ni \"C-S-RET\"' aux.")

(defun claude-repl--install-drawer-visit-override ()
  "Install `claude-repl--drawer-visit-chord' into `general-override-mode-map'
at top-level AND into its evil intercept aux maps for every state in
`claude-repl--scroll-output-intercept-states' (reused as the canonical
\"all evil states\" list).  Idempotent."
  (dolist (entry claude-repl--drawer-visit-chord)
    (let ((seq (kbd (car entry)))
          (cmd (cdr entry)))
      (define-key general-override-mode-map seq cmd)
      (when (fboundp 'evil-get-auxiliary-keymap)
        (dolist (state claude-repl--scroll-output-intercept-states)
          (define-key (evil-get-auxiliary-keymap
                       general-override-mode-map state t t)
                      seq cmd))))))

(claude-repl--install-drawer-visit-override)

;; vterm-mode-map binds every `C-S-<letter>' to `vterm--self-insert' via
;; its define-keys loop over '("C-" "M-" "C-S-").  That major-mode
;; binding shadows our global-map entries whenever point lands in a
;; vterm buffer (e.g. immediately after `C-S-<return>' visits a
;; workspace and selects its Claude REPL output window), causing the
;; chord to be sent to the shell instead of triggering drawer nav.
;; Strip the conflicting keys from `vterm-mode-map' so global-map sees
;; them.  Done lazily inside `after! vterm' so it survives package
;; reloads.

(defconst claude-repl--vterm-shadow-keys
  '("C-S-n" "C-S-p" "C-S-j" "C-S-k" "C-S-x" "C-S-d"
    "C-S-i" "C-S-m" "C-S-h" "C-S-t" "C-S-u")
  "C-S-<letter> chords that `vterm-mode-map' would otherwise capture
via `vterm--self-insert', shadowing our global drawer-mirror bindings.
Non-letter chords like `C-S-<return>', `C-S-+', `C-S--' are not in
vterm's exclusion loop and need no stripping.")

(defun claude-repl--strip-vterm-shadow-keys ()
  "Unmap `claude-repl--vterm-shadow-keys' from `vterm-mode-map' so the
global drawer-mirror bindings win in vterm buffers."
  (dolist (key claude-repl--vterm-shadow-keys)
    (define-key vterm-mode-map (kbd key) nil)))

(after! vterm
  (claude-repl--strip-vterm-shadow-keys))

(map! :i "C-S-f" #'claude-repl-toggle-fullscreen)
(map! :leader :prefix "w" :n "c" #'claude-repl-toggle-fullscreen)
(map! :leader :prefix "w" :n "f" #'claude-repl-fullscreen-and-focus)

;; SPC o -- Claude session control (open, focus, kill, interrupt, utilities)
(map! :leader
      :desc "Claude REPL (simple)" "o c" #'claude-repl-simple
      :desc "Claude REPL (deprio)" "o C" #'claude-repl
      :desc "Kill Claude" "o C-c" #'claude-repl-kill
      :desc "Claude input" "o v" #'claude-repl-focus-input
      :desc "Claude interrupt" "o x" #'claude-repl-interrupt
      :desc "Copy file reference" "o r" #'claude-repl-copy-reference
      :desc "Switch sandbox/bare-metal" "o s" #'claude-repl-switch-environment
      :desc "Toggle hide-mode (closed-REPL workspaces)" "o h" #'claude-repl-toggle-hide-mode
      :desc "Toggle workspace drawer" "o d" #'claude-repl-drawer-toggle)

(map! :leader
      (:prefix "p"
       :desc "Switch to project" "p" #'claude-repl-switch-to-project
       :desc "Create worktree workspace" "w" #'claude-repl-create-worktree-workspace))

(map! :leader
      (:prefix "TAB"
       :desc "New worktree ws (from current)" "n" #'claude-repl-create-worktree-workspace
       :desc "New worktree ws (from local master)" "N" #'claude-repl-create-worktree-workspace-from-origin-master
       :desc "Fork worktree ws + fork Claude session" "f" #'claude-repl-fork-worktree-workspace
       :desc "Merge workspace into current" "m" #'+dwc/workspace-merge
       :desc "Merge current workspace into source" "M" #'+dwc/workspace-merge-current-into-source
       :desc "Rename current workspace" "r" #'+dwc/workspace-rename
       :desc "Push workspace to second-to-last" "p" #'+dwc/workspace-push-to-back
       :desc "Pull workspace to second" "P" #'+dwc/workspace-pull-to-front
       :desc "Open most recent workspace" "R" #'+dwc/open-most-recent-workspace))

(map! "s-{" #'claude-repl-switch-left
      "s-}" #'claude-repl-switch-right)

(map! :leader
      :desc "Switch to 1st workspace"   "1" #'claude-repl-workspace-switch-to-0
      :desc "Switch to 2nd workspace"   "2" #'claude-repl-workspace-switch-to-1
      :desc "Switch to 3rd workspace"   "3" #'claude-repl-workspace-switch-to-2
      :desc "Switch to 4th workspace"   "4" #'claude-repl-workspace-switch-to-3
      :desc "Switch to 5th workspace"   "5" #'claude-repl-workspace-switch-to-4
      :desc "Switch to 6th workspace"   "6" #'claude-repl-workspace-switch-to-5
      :desc "Switch to 7th workspace"   "7" #'claude-repl-workspace-switch-to-6
      :desc "Switch to 8th workspace"   "8" #'claude-repl-workspace-switch-to-7
      :desc "Switch to 9th workspace"   "9" #'claude-repl-workspace-switch-to-8
      :desc "Switch to final workspace" "0" #'claude-repl-workspace-switch-to-final)

;; Workspace-jump chords (M-1..M-9 / M-0 and s-1..s-9 / s-0) must beat:
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
;; `claude-repl--install-drawer-visit-override' pattern instead:
;; install the chord into `general-override-mode-map' at top-level AND
;; into its evil aux maps for every evil state, so the binding wins
;; lookup regardless of evil state and regardless of major mode.
;;
;; Sourced from the prefix-arg-free wrappers in
;; `modules/app/claude-repl/commands.el' so `current-prefix-arg' cannot
;; redirect the jump (the original M-9 -> final / M-0 -> font-resize
;; bug).
(defconst claude-repl--workspace-jump-chords
  '(("M-1" . claude-repl-workspace-switch-to-0)
    ("M-2" . claude-repl-workspace-switch-to-1)
    ("M-3" . claude-repl-workspace-switch-to-2)
    ("M-4" . claude-repl-workspace-switch-to-3)
    ("M-5" . claude-repl-workspace-switch-to-4)
    ("M-6" . claude-repl-workspace-switch-to-5)
    ("M-7" . claude-repl-workspace-switch-to-6)
    ("M-8" . claude-repl-workspace-switch-to-7)
    ("M-9" . claude-repl-workspace-switch-to-8)
    ("M-0" . claude-repl-workspace-switch-to-final)
    ("s-1" . claude-repl-workspace-switch-to-0)
    ("s-2" . claude-repl-workspace-switch-to-1)
    ("s-3" . claude-repl-workspace-switch-to-2)
    ("s-4" . claude-repl-workspace-switch-to-3)
    ("s-5" . claude-repl-workspace-switch-to-4)
    ("s-6" . claude-repl-workspace-switch-to-5)
    ("s-7" . claude-repl-workspace-switch-to-6)
    ("s-8" . claude-repl-workspace-switch-to-7)
    ("s-9" . claude-repl-workspace-switch-to-8)
    ("s-0" . claude-repl-workspace-switch-to-final))
  "Alist of (KEY-STRING . COMMAND) for the workspace-jump chords that
must win key lookup above Doom default's `:n s-9' / `s-0', above
`vterm-mode-map's `M-X' blanket bindings, and across every evil
state.  Each KEY-STRING is passed to `kbd' at install time.")

(defun claude-repl--install-workspace-jump-overrides ()
  "Install `claude-repl--workspace-jump-chords' into
`general-override-mode-map' at top-level AND into its evil intercept
aux maps for every state in `claude-repl--scroll-output-intercept-states'
\(reused as the canonical \"all evil states\" list).  Idempotent."
  (dolist (entry claude-repl--workspace-jump-chords)
    (let ((seq (kbd (car entry)))
          (cmd (cdr entry)))
      (define-key general-override-mode-map seq cmd)
      (when (fboundp 'evil-get-auxiliary-keymap)
        (dolist (state claude-repl--scroll-output-intercept-states)
          (define-key (evil-get-auxiliary-keymap
                       general-override-mode-map state t t)
                      seq cmd))))))

(claude-repl--install-workspace-jump-overrides)

;; SPC j -- Tell Claude to do a predefined thing
(map! :leader
      (:prefix ("j" . "claude")
       :desc "Enqueue input as deferred prompt"        "RET" #'claude-repl-queue-deferred-prompt
       :desc "One-shot doom edit (from master)"        "o" #'claude-repl-create-doom-oneshot-workspace
       :desc "One-shot explanation-engine edit (PR on success)" "O" #'claude-repl-create-explanation-engine-oneshot-workspace
       :desc "Kill workspace"           "d" #'claude-repl-kill-workspace
       :desc "Update GitHub PR description"  "r" #'claude-repl-update-pr
       :desc "Rebase branch onto origin/master" "b" #'claude-repl-rebase-onto-origin-master
       :desc "Nuke workspace"           "x" #'claude-repl-nuke-workspace
       :desc "Nuke ALL workspaces"      "X" #'claude-repl-nuke-all-workspaces
       :desc "Paste workspace clipboard" "p" #'claude-repl-paste-clipboard
       :desc "Toggle debug logging"    "D" #'claude-repl-debug/toggle-logging
       (:prefix ("h" . "help/debug")
        :desc "Dump workspace state"     "p" #'claude-repl-debug/dump-workspace
        :desc "Explain config (read-only Q&A)" "c" #'claude-repl-explain-config
        :desc "Close explain-config popup"     "C" #'claude-repl-explain-config-close)
       (:prefix ("e" . "explain")
        :desc "line/region/hunk (prompt)" "e" #'claude-repl-explain-prompt
        :desc "line/region/hunk (canned)" "E" #'claude-repl-explain
        (:prefix ("d" . "diff")
         :desc "worktree"    "w" #'claude-repl-explain-diff-worktree
         :desc "staged"      "s" #'claude-repl-explain-diff-staged
         :desc "uncommitted" "u" #'claude-repl-explain-diff-uncommitted
         :desc "HEAD"        "h" #'claude-repl-explain-diff-head
         :desc "branch"      "b" #'claude-repl-explain-diff-branch))
       :desc "Reload claude-repl config" "R" #'claude-repl-reload-config
       (:prefix ("s" "Send predefined input to Claude")
        :desc "create PR (no --self-certified)"       "p"   #'claude-repl-create-or-update-pr-no-self-certified
        :desc "create PR"                             "P"   #'claude-repl-create-or-update-pr
        :desc "paste: create PR (no --self-certified)" "C-p" #'claude-repl-create-or-update-pr-no-self-certified-paste
        :desc "paste: create PR"                       "C-S-p" #'claude-repl-create-or-update-pr-paste)
       (:prefix ("m" . "modify workspace")
        :desc "Set/change priority" "p" #'claude-repl-set-priority)
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
  (define-key doom-leader-map (kbd (format "o %s" i))
    #'claude-repl--send-digit-char))

(map! :leader "b R" #'claude-repl-revert-and-eval-buffer)
(map! :leader "m e B" #'claude-repl-revert-and-eval-buffer)

;;; Section 5: Advice registrations

;; TODO: This +workspace/kill advice is a behavioral hook, not a keybinding.
;; It belongs in session.el or panels.el.  Do not move yet -- other agents are
;; modifying those files.

;; Kill Claude session before workspace deletion so buffers/windows are cleaned
;; up while the workspace is still current.
(advice-add #'+workspace/kill :before #'claude-repl--kill-before-workspace-delete)

