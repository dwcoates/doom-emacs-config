;;; keybindings.el --- keybindings -*- lexical-binding: t; -*-

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
specific named workspace's session (the kill / close / sweep paths)
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
killed workspace's identity record survives in the hash for
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

(defun agent-repl--killable-workspace-names ()
  "Return candidate names for the kill/close picker.
Union of live agent-repl workspaces (`agent-repl--live-ws-names')
and tab-bar workspaces (`agent-repl--ws-all-names'), preserving the live
entries first.  Tab-bar entries whose agent-repl session has been
torn down (or never existed) are included so the user can dispatch a
plain persp/doom kill on stray tabs through the same picker — the
dispatcher (`agent-repl--kill-or-close-workspace') decides per-entry
whether to run the agent-repl teardown or a bare `+workspace/kill'."
  (let* ((live (agent-repl--live-ws-names))
         (tabbar (agent-repl--ws-all-names))
         (extras (cl-remove-if (lambda (n) (member n live)) tabbar)))
    (append live extras)))

(defun agent-repl--read-killable-workspace (prompt)
  "Prompt for a workspace to kill/close.
Candidates come from `agent-repl--killable-workspace-names': live
agent-repl workspaces plus tab-bar workspaces whose agent has
already been killed.  Defaults to the current workspace when it
appears in the candidate list.  Signals `user-error' when no
candidates exist."
  (let* ((known (agent-repl--killable-workspace-names))
         (current (agent-repl--ws-current-name))
         (default (and current (member current known) current)))
    (unless known
      (agent-repl--log current
                        "read-killable-workspace: rejected prompt=%S current=%S candidate-count=0"
                        prompt current)
      (user-error "No workspaces available to kill/close"))
    (let ((selected (completing-read prompt known nil t nil nil default)))
      (agent-repl--log selected
                        "read-killable-workspace: prompt=%S current=%S default=%S candidate-count=%d selected=%S"
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

;;; Section 3: Keybinding definitions

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
      ;;
      ;; The verb refreshes BOTH halves of the workspace: alongside the shim
      ;; restart it rebuilds the webapp if stale (in the background, so the
      ;; restart is not held behind it) and redeploys it by bouncing the
      ;; workspace's webview onto the new build id.  That is why it is the one
      ;; key to reach for after editing either side -- `o l' remounts the
      ;; webview on whatever bundle is already built, `o C-c' builds it first.
      ;; The daemon is neither rebuilt nor restarted here.
      :desc "Restart Claude session + rebuild/redeploy webapp" "o C-c" #'agent-repl-restart-session
      :desc "Claude input" "o v" #'agent-repl-focus-input
      :desc "Claude interrupt" "o x" #'agent-repl-interrupt
      :desc "Copy file reference" "o r" #'agent-repl-copy-reference
      :desc "Reload webview (rebuilt bundle)" "o l" #'agent-repl-frontend-reload-webview
      ;; SPC o L sits on the shifted twin of the reload because the two are the
      ;; same act with different reasons: `o l' remounts a page that is where it
      ;; should be but running an old bundle, `o L' remounts one that navigated
      ;; somewhere else entirely (an external link inside the webapp takes the
      ;; xwidget with it) and has no way back on its own.
      :desc "Rescue webview (navigated away)" "o L" #'agent-repl-frontend-rescue-webview
      ;; SPC o m -- go home.  Every worktree workspace of a repo has exactly one
      ;; main checkout behind it, so this is a jump with no picker: from any
      ;; workspace on a linked worktree of the doom repo, `o m' lands on `doom'.
      :desc "Switch to main worktree workspace" "o m" #'agent-repl-switch-to-main-worktree-workspace)

(map! :leader
      (:prefix "p"
       :desc "Switch to workspace" "p" #'agent-repl-switch-to-project))

(map! :leader
      (:prefix "TAB"
       :desc "Add project from directory" "C-n" #'agent-repl-add-project-workspace
       :desc "New worktree ws (from current)" "n" #'agent-repl-create-worktree-workspace
       :desc "New worktree ws (from local master)" "N" #'agent-repl-create-worktree-workspace-from-origin-master
       :desc "Fork worktree ws + fork Claude session" "f" #'agent-repl-fork-worktree-workspace
       :desc "Merge current workspace into source" "M" #'agent-repl-workspace-merge-current-into-source
       :desc "Continue merge after resolving conflict" "c" #'agent-repl-workspace-merge-continue-after-resolve
       :desc "Open most recent workspace" "R" #'agent-repl-open-most-recent-workspace))

(map! "s-{" #'agent-repl-switch-left
      "s-}" #'agent-repl-switch-right)

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
       :desc "Close workspace"          "d" #'agent-repl-close-workspace
       :desc "Update GitHub PR description"  "r" #'agent-repl-update-pr
       :desc "Rebase branch onto origin/master" "b" #'agent-repl-rebase-onto-origin-master
       :desc "Kill workspace"           "x" #'agent-repl-kill-workspace
       :desc "Kill ALL workspaces"      "X" #'agent-repl-kill-all-workspaces
       :desc "Paste workspace clipboard" "p" #'agent-repl-paste-clipboard
       (:prefix ("h" . "help")
        :desc "Copy workspace name"      "y" #'agent-repl-copy-workspace-name)
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

;;; Section 4: Advice registrations

;; TODO: This +workspace/kill advice is a behavioral hook, not a keybinding.
;; It belongs in session.el or panels.el.  Do not move yet -- other agents are
;; modifying those files.

;; Kill the agent session before workspace deletion so buffers/windows are cleaned
;; up while the workspace is still current.
(agent-repl--ws-advise-kill-before #'agent-repl--kill-before-workspace-delete)
