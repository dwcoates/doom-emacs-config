;;; keybindings.el --- keybindings and debug helpers -*- lexical-binding: t; -*-

;; `map!' comes from Doom.  Byte-compiled outside a Doom session
;; (emacs -batch -Q) the macro is undefined, so the compiler treats each
;; `(map! ...)' body as a function-call form — and the dotted
;; `(:prefix ("j" . "claude"))' pair is not a valid form, which aborts
;; the compile.  Provide an expansion-time no-op for that case only; a
;; real Doom session (and any Doom-driven compile) has the genuine
;; macro, so this guard is inert there.
(eval-when-compile
  (unless (fboundp 'map!)
    (defmacro map! (&rest _args) nil)))

;;; Section 1: Internal helpers

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
     (:agent-state :repl-state :status :pushed-render-state
      :pushed-session-connectivity :pushed-session-status
      :pushed-render-state-meta
      :dead :bogus :merged))
    ("🌳 Project / Git"
     (:project-dir :worktree-p :source-ws-dir :source-ws-name
      :merge-parent-dir :branch-merged :branch-merged-last-check
      :detail-branch :detail-dirty-count :detail-last-commit
      :detail-last-commit-time :detail-master-ahead :detail-source-ahead
      :detail-source-branch))
    ("🧠 Session"
     (:session-id :fork-session-id :frontend-buffer :active-env
      :bare-metal :agent-ready :ws-loaded :ready-timer))
    ("💬 Prompts"
     (:last-prompt-time :last-prompt-text :last-prompt-summary
      :last-prompt-summary-pending :deferred-prompts :pending-prompts
      :clipboard))
    ("🔔 Notifications"
     (:done :done-acked :done-acked-at :last-notify-time))
    ("🔀 Merge"
     (:merge-completed :merge-completed-at :merge-conflict
      :merge-failed :merge-proc :merge-queued :merging
      :pushed-merge-status :merge-echo-last))
    ("🪟 UI / Panels"
     (:input-buffer :pending-magit :pending-show-panels
      :pending-initial-buffers :fullscreen-config :ai-title-cache
      :saved-tab-index))
    ("🔢 Counters"
     (:counter)))
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
(called here with no WS argument) inspects the CURRENT workspace's
frontend, so applying it cross-workspace would kill the wrong session
(e.g. when a background workspace is persp-killed
workspace from inside a workspace-switch handler, the named workspace's
session has already been torn down by the sweep and the current
workspace's session must be left alone).  Callers that need to kill a
specific named workspace's session (the nuke / kill / sweep paths)
handle teardown explicitly through the frontend registry's kill
dispatch before invoking `+workspace/kill'."
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
      (agent-repl--log current "kill-before-workspace-delete: agent running, killing session")
      (let ((agent-repl--kill-cause "persp-delete advice (+workspace/kill on current ws)"))
        (agent-repl-kill)))
     (t
      (agent-repl--log current "kill-before-workspace-delete: agent not running, no-op")))))

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
    (unless known
      (agent-repl--log current
                        "read-known-workspace: rejected prompt=%S current=%S known-count=0"
                        prompt current)
      (user-error "No agent-repl workspaces registered"))
    (let ((selected (completing-read prompt known nil t nil nil default)))
      (agent-repl--log selected
                        "read-known-workspace: prompt=%S current=%S default=%S known-count=%d selected=%S"
                        prompt current default (length known) selected)
      selected)))

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
    (unless known
      (agent-repl--log current
                        "read-nukeable-workspace: rejected prompt=%S current=%S candidate-count=0"
                        prompt current)
      (user-error "No workspaces available to nuke/kill"))
    (let ((selected (completing-read prompt known nil t nil nil default)))
      (agent-repl--log selected
                        "read-nukeable-workspace: prompt=%S current=%S default=%S candidate-count=%d selected=%S"
                        prompt current default (length known) selected)
      selected)))

;;; Section 2: Utility commands used by keybindings

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
repaint so the glyph updates immediately.

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
         (had-entry (agent-repl--ws-known-p ws))
         (cache-before (or (agent-repl--ws-names-cache) "(unbound)")))
    (agent-repl--log ws "set-priority: ws=%s ws-explicit=%s had-entry=%s priority %s -> %s cache=%S"
                      ws (if ws-explicit-p "t" "nil") (if had-entry "t" "nil")
                      (or old-priority "nil") (or new-priority "(cleared)")
                      cache-before)
    (agent-repl--ws-put ws :priority new-priority)
    (agent-repl--state-save ws)
    (agent-repl--reorder-workspace-by-priority ws)
    (force-mode-line-update t)
    (message "Workspace '%s' priority: %s" ws (if (string-empty-p priority) "cleared" priority))))

;; SPC b R -- revert buffer from disk then eval as Elisp (fast config reload)
(defun agent-repl-revert-and-eval-buffer ()
  "Revert the current buffer from disk, then evaluate it as Elisp."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-log-name) "revert-and-eval-buffer: entry buffer=%s" (buffer-name))
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
    (agent-repl--log (agent-repl--ws-current-log-name) "reload-config: file=%s" file)
    (load-file file)
    (message "[agent-repl] Reloaded %s" file)))

;;; Section 3: Debug helpers -- interactive commands for diagnosing workspace state issues.
;;; Call via M-x agent-repl-debug/...

(defun agent-repl-debug/cancel-timers ()
  "Cancel all agent-repl timers."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-log-name)
                    "debug/cancel-timers: requested")
  (agent-repl--cancel-all-timers)
  (message "Cancelled all agent-repl timers."))

(defun agent-repl-debug/workspace-states ()
  "Display all workspace states."
  (interactive)
  (let ((states (mapcar #'agent-repl--cons-name-state (agent-repl--ws-list-names))))
    (agent-repl--log (agent-repl--ws-current-log-name)
                      "debug/workspace-states: count=%d states=%S"
                      (length states) states)
    (message "Workspace states:\n%s"
             (mapconcat #'agent-repl--format-workspace-state states "\n"))))

(defun agent-repl-debug/buffer-info ()
  "Display all agent view (webview) buffers with their owning and persp workspaces."
  (interactive)
  (let* ((bufs (cl-remove-if-not #'agent-repl--agent-view-buffer-p (buffer-list)))
         (lines (mapcar #'agent-repl--format-buffer-info bufs)))
    (agent-repl--log (agent-repl--ws-current-log-name)
                      "debug/buffer-info: agent-view-count=%d buffers=%S"
                      (length bufs) (mapcar #'buffer-name bufs))
    (message "Claude buffers:\n%s"
             (if lines (mapconcat #'identity lines "\n") "  (none)"))))

(defun agent-repl-debug/clear-state (ws)
  "Clear all states for workspace WS without killing buffers."
  (interactive (list (agent-repl--read-workspace "Workspace: ")))
  (agent-repl--log ws "debug/clear-state: ws=%s states=%S"
                    ws '(:thinking :done :permission :inactive))
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
  "Set the owning workspace for an agent view (webview) buffer."
  (interactive)
  (let* ((bufs (cl-remove-if-not #'agent-repl--agent-view-buffer-p (buffer-list)))
         (buf-name (completing-read "Buffer: " (mapcar #'buffer-name bufs) nil t))
         (ws (agent-repl--read-workspace "Owning workspace: ")))
    (with-current-buffer buf-name
      (setq-local agent-repl--owning-workspace ws))
    (agent-repl--log ws "debug/set-owning-workspace: buffer=%s ws=%s candidate-count=%d"
                      buf-name ws (length bufs))
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
      (agent-repl--log (agent-repl--ws-current-log-name) "debug logging toggled: %s" label))))

(defun agent-repl-debug/toggle-log-to-file ()
  "Toggle writing debug log output to `agent-repl-log-file-name'.
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
  "Toggle the on-demand metaprompt re-read.
Does NOT affect the metaprompt the shim installs as each session's
system prompt, which is how the guidelines ordinarily reach the agent."
  (interactive)
  (setq agent-repl-skip-permissions (not agent-repl-skip-permissions))
  (agent-repl--log (agent-repl--ws-current-log-name)
                    "debug/toggle-metaprompt: skip-permissions=%s"
                    (if agent-repl-skip-permissions "t" "nil"))
  (message "Agent REPL on-demand metaprompt re-read: %s" (if agent-repl-skip-permissions "ON" "OFF")))

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
         (plist (agent-repl--ws-plist ws))
         (alist (agent-repl--dump-plist-to-alist plist))
         (partition (agent-repl--dump-partition
                     alist agent-repl--dump-sections)))
    (agent-repl--log ws "debug/dump-workspace: ws=%s plist-key-count=%d section-count=%d"
                      ws (/ (length plist) 2) (length partition))
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

(defun agent-repl-debug/--gather-ws-diagnostics (ws-name)
  "Gather diagnostic information about workspace WS-NAME.
Returns a plist with keys :owning-ws :has-window :agent-open.
`:owning-ws' and `:has-window' are derived from WS-NAME's agent view
\(webview) buffer, independently re-derived from the persp's actual
buffer list rather than trusted from the `:frontend-buffer' plist
entry — this is a diagnostic tool for exactly the buffer-ownership
drift it would otherwise be trying to take on faith."
  (let* ((open (agent-repl--ws-agent-open-p ws-name))
         (persp (agent-repl--ws-resolve-persp ws-name))
         (persp-bufs (agent-repl--ws-buffers persp))
         (view-buf (cl-loop for buf in persp-bufs
                            when (and (buffer-live-p buf)
                                      (agent-repl--agent-view-buffer-p buf))
                            return buf))
         (owning-ws (agent-repl--buffer-owner view-buf))
         (has-window (and view-buf (get-buffer-window view-buf t))))
    (list :owning-ws owning-ws :has-window has-window
          :agent-open open)))

(defun agent-repl-debug/--apply-state-refresh (ws-name agent-open)
  "Apply a state refresh for WS-NAME given whether AGENT-OPEN is non-nil.
Mirrors the logic in `agent-repl--update-all-workspace-states'."
  (if agent-open
      (agent-repl--update-ws-state ws-name)
    (agent-repl--mark-dead ws-name)))

(defun agent-repl-debug/--format-diagnostics (ws-name diag before after)
  "Format a diagnostic summary string for WS-NAME.
DIAG is the plist from `agent-repl-debug/--gather-ws-diagnostics'.
BEFORE and AFTER are the workspace states before and after refresh."
  (format (concat "Workspace %s:\n"
                  "  owning-ws=%s has-window=%s\n"
                  "  agent-open=%s\n"
                  "  state=%s -> %s")
          ws-name
          (or (plist-get diag :owning-ws) "nil")
          (if (plist-get diag :has-window) "yes" "no")
          (if (plist-get diag :agent-open) "yes" "no")
          (or before "nil") (or after "nil")))

(defun agent-repl-debug/refresh-state (ws-name)
  "Force a full state refresh for workspace WS-NAME.
Runs the same logic as the periodic `update-all-workspace-states' timer:
checks agent visibility and applies the state table.
Reports comprehensive diagnostics."
  (interactive (list (agent-repl--read-workspace-with-default "Workspace: ")))
  (let* ((before (agent-repl--ws-agent-state ws-name))
         (diag (agent-repl-debug/--gather-ws-diagnostics ws-name)))
    (agent-repl--log ws-name
                      "debug/refresh-state: ws=%s before=%S agent-open=%s owning-ws=%S has-window=%s"
                      ws-name before
                      (if (plist-get diag :agent-open) "t" "nil")
                      (plist-get diag :owning-ws)
                      (if (plist-get diag :has-window) "t" "nil"))
    (agent-repl-debug/--apply-state-refresh ws-name (plist-get diag :agent-open))
    (let ((after (agent-repl--ws-agent-state ws-name)))
      (agent-repl--log ws-name "debug/refresh-state: ws=%s outcome=%s"
                        ws-name (if (plist-get diag :agent-open) "update" "mark-dead"))
      (force-mode-line-update t)
      (message "%s" (agent-repl-debug/--format-diagnostics ws-name diag before after)))))

;;; Section 4: Keybinding definitions

(defconst agent-repl--workspace-create-keybindings
  '(("n" . agent-repl-create-worktree-workspace)
    ("N" . agent-repl-create-worktree-workspace-from-origin-master))
  "Canonical `SPC TAB' daemon-owned workspace creation bindings.")

(defconst agent-repl--scroll-output-intercept-states
  '(normal visual insert emacs operator motion replace)
  "Canonical list of every evil state.  Originally introduced so the
\(now-removed) scroll-output chords could install intercept aux map
entries on `general-override-mode-map' uniformly across all of them;
reused as-is by `agent-repl--install-workspace-jump-overrides' below
so a chord wins key lookup regardless of which evil state is current.")

(map! :leader :prefix "w" :n "f" #'agent-repl-fullscreen-and-focus)

;; SPC o -- agent session control (open, focus, kill, interrupt, utilities)
(map! :leader
      :desc "Agent REPL (simple)" "o c" #'agent-repl-simple
      :desc "Agent REPL (deprio)" "o C" #'agent-repl
      ;; SPC o C-c is the HARD RESTART, not the kill.  A restart keeps the
      ;; conversation and replaces only the process serving it, which is what
      ;; is wanted in every situation the old kill was reached for -- a wedged
      ;; shim, a shim still running pre-deploy code -- without losing the
      ;; conversation as collateral.  `agent-repl-kill' is still available by
      ;; name for the rarer case of genuinely ending a session.
      :desc "Restart Claude session (same conversation)" "o C-c" #'agent-repl-restart-session
      :desc "Claude input" "o v" #'agent-repl-focus-input
      :desc "Claude interrupt" "o x" #'agent-repl-interrupt
      :desc "Copy file reference" "o r" #'agent-repl-copy-reference
      :desc "Reload webview (rebuilt bundle)" "o l" #'agent-repl-frontend-reload-webview
      :desc "Force fresh conversation" "o f" #'agent-repl-force-fresh-conversation
      :desc "Toggle hide-project-dirs (ChessCom workspaces)" "o H" #'agent-repl-toggle-hide-project-dirs)

(map! :leader
      (:prefix "p"
       :desc "Switch to workspace" "p" #'agent-repl-switch-to-project))

(map! :leader
      (:prefix "TAB"
       :desc "New worktree ws (from current)" "n" #'agent-repl-create-worktree-workspace
       :desc "New worktree ws (from local master)" "N" #'agent-repl-create-worktree-workspace-from-origin-master
       :desc "Fork worktree ws + fork Claude session" "f" #'agent-repl-fork-worktree-workspace
       :desc "Merge current workspace into source" "M" #'agent-repl-workspace-merge-current-into-source
       :desc "Continue merge after resolving conflict" "c" #'agent-repl-workspace-merge-continue-after-resolve
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
;;
;; A plain `(map! :g ...)' binding lands in `global-map' and loses to
;; the Doom `:n' entry (in `evil-normal-state-map').  Install the
;; chord into `general-override-mode-map' at top-level AND into its
;; evil aux maps for every evil state instead, so the binding wins
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
must win key lookup above Doom default's `:n s-9' / `s-0' and across
every evil state.  Command `s-1..s-9' address the FIRST nine
workspaces and Option `M-1..M-9' address the SECOND nine (workspaces
10-18); `M-0'/`s-0' both address the final workspace.  Each KEY-STRING
is passed to `kbd' at install time.")

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
       :desc "One-shot doom edit, pick model (from master)"    "C-o" #'agent-repl-create-doom-oneshot-workspace-with-model
       :desc "One-shot explanation-engine edit, pick model (PR on success)" "C-S-o" #'agent-repl-create-explanation-engine-oneshot-workspace-with-model
       :desc "Amend last doom one-shot (send/queue)"   "M-o" #'agent-repl-amend-doom-oneshot-prompt
       :desc "Amend last explanation-engine one-shot (send/queue)" "M-S-o" #'agent-repl-amend-explanation-engine-oneshot-prompt
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

(map! :leader "b R" #'agent-repl-revert-and-eval-buffer)
(map! :leader "m e B" #'agent-repl-revert-and-eval-buffer)

;;; Section 5: Advice registrations

;; TODO: This +workspace/kill advice is a behavioral hook, not a keybinding.
;; It belongs in session.el or panels.el.  Do not move yet -- other agents are
;; modifying those files.

;; Kill the agent session before workspace deletion so buffers/windows are cleaned
;; up while the workspace is still current.
(agent-repl--ws-advise-kill-before #'agent-repl--kill-before-workspace-delete)
