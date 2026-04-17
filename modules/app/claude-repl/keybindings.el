;;; keybindings.el --- keybindings and debug helpers -*- lexical-binding: t; -*-

;;; Section 1: Internal helpers

(defconst claude-repl--output-dir (expand-file-name "~/.claude/output/")
  "Directory for workspace command files and other IPC output.")

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

(defun claude-repl--kill-before-workspace-delete (&rest _)
  "Before-advice for `+workspace/kill': tear down any running Claude session.
This ensures Claude buffers and windows are cleaned up while the workspace
is still current."
  (claude-repl--log nil "kill-before-workspace-delete: entry")
  (if (claude-repl--vterm-running-p)
      (progn
        (claude-repl--log nil "kill-before-workspace-delete: vterm running, killing session")
        (claude-repl-kill))
    (claude-repl--log nil "kill-before-workspace-delete: vterm not running, no-op")))

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
the obvious target).  Signals `user-error' when no workspaces exist."
  (let* ((known (hash-table-keys claude-repl--workspaces))
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
    (claude-repl--log nil "write-output-json: filename=%s dir=%s" filename claude-repl--output-dir)
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
    (claude-repl--log nil "send-digit-char: digit=%s" (string last-key))
    (claude-repl-send-char (string last-key))))

;; C-v paste forwarding to vterm
(defun claude-repl-paste-to-vterm ()
  "Forward a Ctrl-V keystroke to the Claude vterm buffer.
This lets Claude CLI handle paste natively, including images."
  (interactive)
  (claude-repl--log nil "paste-to-vterm: entry")
  (if (claude-repl--vterm-live-p)
      (progn
        (claude-repl--log nil "paste-to-vterm: vterm live, forwarding C-v")
        (with-current-buffer (claude-repl--ws-get (+workspace-current-name) :vterm-buffer)
          (vterm-send-key "v" nil nil t)))
    (claude-repl--log nil "paste-to-vterm: vterm not live, skipping")))

;; TODO: claude-repl-set-priority has no keybinding anywhere; it belongs in
;; commands.el rather than keybindings.el.  Do not move yet -- other agents are
;; modifying that file.
(defun claude-repl-set-priority (priority)
  "Set the priority badge for the current workspace.
PRIORITY is one of \"p05\", \"p1\", \"p2\", \"p3\", or \"\" to clear."
  (interactive
   (list (completing-read "Priority: " '("p05" "p1" "p2" "p3" "") nil t)))
  (let ((ws (+workspace-current-name)))
    (claude-repl--log ws "set-priority: ws=%s priority=%s" ws (if (string-empty-p priority) "(cleared)" priority))
    (claude-repl--ws-put ws :priority (if (string-empty-p priority) nil priority))
    (force-mode-line-update t)
    (message "Workspace '%s' priority: %s" ws (if (string-empty-p priority) "cleared" priority))))

;; SPC b R -- revert buffer from disk then eval as Elisp (fast config reload)
(defun claude-repl-revert-and-eval-buffer ()
  "Revert the current buffer from disk, then evaluate it as Elisp."
  (interactive)
  (claude-repl--log nil "revert-and-eval-buffer: entry buffer=%s" (buffer-name))
  (revert-buffer :ignore-auto :noconfirm)
  (eval-buffer))

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
  (let* ((names (or names '("DWC/mock-test")))
         (file (claude-repl--write-output-json "workspace_generation.json" names)))
    (claude-repl--log nil "mock workspace-generation file written: %s names=%s" file names)
    (message "Wrote mock workspace_generation.json: %s" names)))

(defun claude-repl-debug/mock-workspace-commands-with-priority ()
  "Write a mock workspace_commands file with a priority field to test image badges."
  (interactive)
  (let* ((priority (completing-read "Priority: " '("p05" "p1" "p2" "p3") nil t))
         (name (read-string "Branch name: " "DWC/mock-priority-test"))
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
                                  "^workspace_commands_.*\\.json$"))))
    (if (not files)
        (message "No workspace_commands_*.json files found in %s"
                 claude-repl--output-dir)
      (message "Found %d file(s), processing..." (length files))
      (dolist (file files)
        (claude-repl--log nil "process-pending-commands: processing file=%s" file)
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
Closes their windows and silences process exit queries before killing."
  (claude-repl--log ws "kill-owned-panel-buffers: entry ws=%s" ws)
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (claude-repl--claude-panel-buffer-p buf)
               (equal ws (buffer-local-value 'claude-repl--owning-workspace buf)))
      (claude-repl--log ws "kill-owned-panel-buffers: killing buffer=%s" (buffer-name buf))
      (when-let ((win (get-buffer-window buf)))
        (ignore-errors (delete-window win)))
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
                 ('verbose "ON (verbose)"))))
    ;; Always emit via message so it's visible even when logging is off.
    (message "[claude-repl] debug logging: %s" label)
    ;; Also emit via the log system so it appears in the log stream.
    (when claude-repl-debug
      (claude-repl--log nil "debug logging toggled: %s" label))))

(defun claude-repl-debug/toggle-log-to-file ()
  "Toggle writing debug log output to `.claude-repl.log' in the repository root.
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
defaulting to the current workspace when registered."
  (interactive)
  (let* ((ws (claude-repl--read-known-workspace "Dump workspace: "))
         (plist (gethash ws claude-repl--workspaces)))
    (with-help-window "*claude-repl-dump*"
      (with-current-buffer "*claude-repl-dump*"
        (insert (format "Workspace: %s\n\n" ws))
        (let ((pl plist))
          (while pl
            (let* ((key (pop pl))
                   (val (pop pl))
                   (val-str (cond
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
                              (pp-to-string val))
                             (t (pp-to-string val)))))
              (insert (format "  %-20s %s\n" key val-str)))))))))

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

(map! :nvi "C-S-m" #'claude-repl-cycle)
(map! :i "C-S-f" #'claude-repl-toggle-fullscreen)
(map! :leader :prefix "w" :n "c" #'claude-repl-toggle-fullscreen)
(map! :leader :prefix "w" :n "f" #'claude-repl-fullscreen-and-focus)

;; SPC o -- Claude session control (open, focus, kill, interrupt, utilities)
(map! :leader
      :desc "Claude REPL" "o c" #'claude-repl
      :desc "Claude input" "o v" #'claude-repl-focus-input
      :desc "Kill Claude" "o C" #'claude-repl-kill
      :desc "Claude interrupt" "o x" #'claude-repl-interrupt
      :desc "Copy file reference" "o r" #'claude-repl-copy-reference
      :desc "Switch sandbox/bare-metal" "o s" #'claude-repl-switch-environment)

(map! :leader
      (:prefix "p"
       :desc "Create worktree workspace" "w" #'claude-repl-create-worktree-workspace))

(map! :leader
      (:prefix "TAB"
       :desc "Create worktree workspace" "n" #'claude-repl-create-worktree-workspace
       :desc "Fork into worktree workspace" "N" #'claude-repl-fork-worktree-workspace
       :desc "Merge workspace into current" "m" #'+dwc/workspace-merge
       :desc "Merge current workspace into master" "M" #'+dwc/workspace-merge-current-into-master
       :desc "Push workspace to second-to-last" "p" #'+dwc/workspace-push-to-back
       :desc "Pull workspace to second" "P" #'+dwc/workspace-pull-to-front
       :desc "Open most recent workspace" "R" #'+dwc/open-most-recent-workspace))

;; SPC j -- Tell Claude to do a predefined thing
(map! :leader
      (:prefix ("j" . "claude")
       :desc "Explain line/region/hunk"      "e" #'claude-repl-explain
       :desc "Update GitHub PR description"  "r" #'claude-repl-update-pr
       :desc "Nuke workspace"           "x" #'claude-repl-nuke-workspace
       :desc "Nuke ALL workspaces"      "X" #'claude-repl-nuke-all-workspaces
       :desc "Dump workspace state"     "p" #'claude-repl-debug/dump-workspace
       :desc "Toggle debug logging"    "D" #'claude-repl-debug/toggle-logging
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

