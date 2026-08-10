;;; session.el --- session lifecycle management -*- lexical-binding: t; -*-

;;; Code:

(declare-function agent-repl--ws-dir-owner "agent-repl-workspace" (dir &optional except))
(declare-function agent-repl--ws-gui-frontend-p "frontends" (ws))
(declare-function agent-repl--gui-running-p "frontend-client" (ws))
(declare-function agent-repl--ws-frontend "frontends" (ws))
(declare-function agent-repl-frontend-running-p-fn "frontends" (frontend))
(declare-function agent-repl--frontend-dispatch-show "frontends" (ws))
(declare-function agent-repl--ensure-input-buffer "agent-repl-panels" (ws))
(declare-function agent-repl--emacs-focused-p "notifications" ())

;; Defined in worktree.el, which may load after this file; referenced only
;; at call time by `agent-repl--doom-config-tree-p'.
(defvar agent-repl-worktree-dir-suffix)

;;;; Session readiness

(defcustom agent-repl-managed-project-pattern "ChessCom"
  "Pattern matched against the project directory to determine permission mode.
Projects whose expanded path contains this pattern use
`agent-repl-managed-permission-flag'; all others use
`agent-repl-personal-permission-flag'."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-managed-permission-flag "--permission-mode auto"
  "Permission flag for managed projects.
Applies to projects matching `agent-repl-managed-project-pattern'."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-personal-permission-flag "--permission-mode auto"
  "Permission flag for personal projects.
Applies to projects not matching `agent-repl-managed-project-pattern'.
Defaults to --permission-mode auto so generated workspaces never run
under --dangerously-skip-permissions."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-multi-repo-root-env "MULTI_REPO_ROOT"
  "Name of the environment variable naming the multi-repo root directory.
Workspaces whose project directory lies under the value of this
environment variable use `agent-repl-multi-repo-config-dir' as the
Claude CLI's CLAUDE_CONFIG_DIR, selecting the account logged in there.
All other workspaces use `agent-repl-default-config-dir'."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-multi-repo-config-dir "~/.claude-chesscom"
  "CLAUDE_CONFIG_DIR for workspaces under the multi-repo root.
Points at the config directory holding the credentials of the account
used for repositories under `agent-repl-multi-repo-root-env' (the
dodge@chess.com account).  Run `claude login' once with this directory
exported as CLAUDE_CONFIG_DIR to populate its credentials."
  :type 'directory
  :group 'agent-repl)

(defcustom agent-repl-default-config-dir nil
  "CLAUDE_CONFIG_DIR for workspaces outside the multi-repo root.
When nil, no CLAUDE_CONFIG_DIR is set and the Claude CLI uses its
default ~/.claude, i.e. the dodge.w.coates@gmail.com account.  When a
string, that directory is used explicitly."
  :type '(choice (const :tag "Use Claude's default (~/.claude)" nil)
                 (directory :tag "Explicit config dir"))
  :group 'agent-repl)

(defcustom agent-repl-doom-config-root "~/.config/doom"
  "Canonical doom config checkout used by `agent-repl-doom-multi-repo-mode'.
The mode treats this directory, everything under it, and every worktree
of it (the sibling `<root>`+`agent-repl-worktree-dir-suffix' directory)
as lying under the multi-repo root for agent-config selection."
  :type 'directory
  :group 'agent-repl)

(defcustom agent-repl-system-prompt "."
  "Custom system prompt for the main Agent REPL launched via `SPC o c'.
When non-nil, passed to the Claude CLI as `--system-prompt <prompt>',
which fully replaces the default system prompt.  When nil, no
`--system-prompt' flag is added and Claude uses its default system
prompt.  Defaults to a single period — the default system prompt has
been judged worse than essentially-empty input."
  :type '(choice (const :tag "Use Claude's default" nil)
                 (string :tag "Custom system prompt"))
  :group 'agent-repl)

(defcustom agent-repl-interactive-model "opus"
  "Model alias passed to `--model' for interactive Claude sessions.
When non-nil, passed to the Claude CLI as `--model <model>' so every
interactive workspace uses this model.  When nil, no `--model' flag is
added and Claude uses its configured default.  Does NOT affect headless
`claude -p' invocations such as workspace generation or prompt summaries
— those have their own model variables."
  :type '(choice (const :tag "Use Claude's default" nil)
                 (string :tag "Model alias"))
  :group 'agent-repl)

(defun agent-repl--effective-model (model)
  "Return MODEL, or `agent-repl-interactive-model' when MODEL is nil.
The single resolver for which model a session launches under, shared by
the CLI-launch path (`agent-repl--compute-claude-flags') and the
gui-frontend create path (`agent-repl--frontend-after-create-session').  A nil
result — MODEL and `agent-repl-interactive-model' both nil — means no
`--model' is pinned and Claude picks its configured default."
  (or model agent-repl-interactive-model))

(defcustom agent-repl-notify-debounce-seconds 2.0
  "Minimum seconds between desktop notifications for the same workspace."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-notify-delay 0.1
  "Seconds to delay before sending a desktop notification."
  :type 'number
  :group 'agent-repl)

;;;; Workspace environment initialization

(defun agent-repl--apply-display-state (ws saved)
  "Apply persisted display/metadata state from SAVED plist onto workspace WS.
SAVED is a parsed state-file plist (or nil).  Hydrates the
non-env-struct keys that drive the tabline badge and workspace state
glyphs:
`:priority', `:source-ws-dir', `:model', `:last-prompt-time',
`:repl-state', `:saved-tab-index', `:backend', `:fork-session-id',
`:backend-session-stash', `:frontend' (only
when the save marked it a deliberate choice — see below),
`:last-prompt-summary', `:last-prompt-summary-at', `:worktree-p', and
the `:merge-completed' / `:merge-failed' / `:merge-completed-at'
bookkeeping.

Shared by `agent-repl--initialize-ws-env' (the agent-start path) and
`agent-repl--load-display-state' (the `SPC p p' / workspace-creation
path) so the set of persisted display keys is defined in exactly one
place.  Performs no disk I/O — callers supply the already-parsed SAVED
plist.  Idempotent.

`:priority', `:source-ws-dir', `:model', and `:last-prompt-time' prefer
the SAVED value but fall back to whatever is already on WS's plist (e.g.
`agent-repl-set-priority' run before any state-save happened); the
remaining keys are written only when SAVED carries them."
  (agent-repl--log ws
                    "apply-display-state: ws=%s saved=%s priority=%s model=%s backend=%s explicit-frontend=%s repl-state=%s merge-completed=%s"
                    ws (if saved "present" "absent")
                    (and saved (plist-get saved :priority))
                    (and saved (plist-get saved :model))
                    (and saved (plist-get saved :backend))
                    (and saved (plist-get saved :frontend-explicit))
                    (and saved (plist-get saved :repl-state))
                    (and saved (plist-get saved :merge-completed)))
  ;; Priority: prefer the saved value, fall back to whatever is already in
  ;; the plist (e.g., `agent-repl-set-priority' called before any
  ;; state-save happened for this workspace).
  (agent-repl--ws-put ws :priority
                       (or (and saved (plist-get saved :priority))
                           (agent-repl--ws-get ws :priority)))
  ;; Source workspace dir: prefer the saved value, fall back to whatever is
  ;; already in the plist (e.g., set by `--finalize-worktree-workspace'
  ;; before any state-save happened for this workspace).
  (agent-repl--ws-put ws :source-ws-dir
                       (or (and saved (plist-get saved :source-ws-dir))
                           (agent-repl--ws-get ws :source-ws-dir)))
  ;; Model: prefer the saved value (the session's current model, captured
  ;; from the workspace's Claude config dir at the last state-save so a
  ;; mid-session `/model' switch — e.g. `opus' to `fable' — survives
  ;; restart), falling back to whatever is already in the plist (the
  ;; workspace-generation model, set by `--finalize-worktree-workspace'
  ;; before any state-save happened).  `agent-repl--claude-start-cmd'
  ;; reads `:model' to pass `--model' when booting, so restoring it here
  ;; re-launches the session under the same model.
  (agent-repl--ws-put ws :model
                       (or (and saved (plist-get saved :model))
                           (agent-repl--ws-get ws :model)))
  ;; Backend: prefer the saved value, fall back to whatever is already in
  ;; the plist (e.g., `agent-repl-select-backend' run before any
  ;; state-save happened).  Restoring the backend across restarts is
  ;; load-bearing: a codex workspace's session id resumes correctly only
  ;; through the codex CLI.
  (agent-repl--ws-put ws :backend
                       (or (and saved (plist-get saved :backend))
                           (agent-repl--ws-get ws :backend)))
  ;; Frontend: restored ONLY when the saved plist marks it as a
  ;; DELIBERATE choice (`:frontend-explicit', written by
  ;; `agent-repl--ws-choose-frontend' — `SPC o F' and friends).  Anything
  ;; else in the saved `:frontend' is an INCIDENTAL stamp from an older
  ;; boot, and honoring it would pin every such workspace to whatever it
  ;; happened to boot under forever, so a restored workspace could never
  ;; follow `agent-repl-default-frontend' forward.  Left unset, the
  ;; frontend re-resolves from the default (constrained by the workspace's
  ;; backend and env — see `agent-repl--frontend-default-for-ws', which is
  ;; also what makes a codex workspace fail loudly now that no frontend
  ;; can drive it).
  (when (and saved (plist-get saved :frontend-explicit))
    (agent-repl--ws-put ws :frontend-explicit t)
    (agent-repl--ws-put ws :frontend
                         (or (plist-get saved :frontend)
                             (agent-repl--ws-get ws :frontend))))
  (agent-repl--log ws "apply-display-state: frontend-restore ws=%s explicit=%s restored=%s"
                    ws (and saved (plist-get saved :frontend-explicit))
                    (and saved (plist-get saved :frontend)))
  ;; Last-prompt-time: prefer saved value, fall back to whatever is
  ;; already in the plist.  Records when the last user prompt was
  ;; sent; survives Emacs restarts so duration-since-last-prompt
  ;; displays reflect real elapsed wall-clock, not session age.
  (agent-repl--ws-put ws :last-prompt-time
                       (or (and saved (plist-get saved :last-prompt-time))
                           (agent-repl--ws-get ws :last-prompt-time)))
  ;; Repl-state: hydrate the *desired* panel-visibility lifecycle from the
  ;; saved file so `:inactive' (panels closed via `SPC o c' or `SPC o C')
  ;; survives Emacs restart.  Only persistable values matter at restart —
  ;; `:dead'/nil reduce to "no opinion, default to opening panels", so we
  ;; only restore `:active' / `:inactive'.
  (let ((saved-repl-state (and saved (plist-get saved :repl-state))))
    (when (memq saved-repl-state '(:active :inactive))
      (agent-repl--ws-put ws :repl-state saved-repl-state))
    (agent-repl--log ws "apply-display-state: repl-state ws=%s saved=%s restored=%s"
                      ws saved-repl-state
                      (memq saved-repl-state '(:active :inactive))))
  ;; Tab-bar slot: if the ws was deprioritized at the prior quit (i.e.
  ;; pushed to second-to-last via `SPC o C'), `:saved-tab-index' was
  ;; left non-nil pending the next reopen.  Restore it so the next
  ;; `--show-existing-panels' returns the ws to its prior slot.
  (when-let ((idx (and saved (plist-get saved :saved-tab-index))))
    (agent-repl--ws-put ws :saved-tab-index idx))
  ;; Fork session ID: a worktree-fork ws whose agent session was never
  ;; actually started before quit needs the fork pointer to survive so
  ;; the next `--initialize-agent' can launch with `--resume FORK
  ;; --fork-session'.  Cleared by `--initialize-agent' once consumed.
  (when-let ((fork (and saved (plist-get saved :fork-session-id))))
    (agent-repl--ws-put ws :fork-session-id fork))
  ;; Backend session stash: the per-backend session ids captured when the
  ;; user switched AWAY from a backend, so switching BACK restores that
  ;; backend's `--continue'/`resume' continuity (see
  ;; `agent-repl--ws-switch-backend-session-ids').  Survives restart so a
  ;; switch-back after re-boot still resumes the prior backend's session.
  (when-let ((stash (and saved (plist-get saved :backend-session-stash))))
    (agent-repl--ws-put ws :backend-session-stash stash))
  ;; Last prompt summary: the tabline / mode-line uses this to render a
  ;; short "what is this ws working on" hint.  Restore just the summary
  ;; — `:last-prompt-text' and `:last-prompt-summary-pending' are
  ;; coordination fields for the in-flight async summary task, which
  ;; can't survive Emacs quit, so persisting them would only confuse
  ;; the apply-summary path on the next prompt.
  (when-let ((summary (and saved (plist-get saved :last-prompt-summary))))
    (agent-repl--ws-put ws :last-prompt-summary summary))
  ;; Restore the send-time of the prompt that produced the persisted
  ;; summary so the mode-line's "X ago" prefix survives Emacs restart
  ;; and continues counting against the actual prompt's wall-clock,
  ;; not the post-restart re-init moment.
  (when-let ((at (and saved (plist-get saved :last-prompt-summary-at))))
    (agent-repl--ws-put ws :last-prompt-summary-at at))
  ;; Worktree-flag + merge-completed: survive restart so the
  ;; merged-completed state reappears and `--finish-workspace' can
  ;; still remove the worktree when the user finishes a post-restart
  ;; merged entry.  `:merge-completed-at' rides alongside for display
  ;; only.
  (when (and saved (eq (plist-get saved :worktree-p) t))
    (agent-repl--ws-put ws :worktree-p t))
  (when (and saved (eq (plist-get saved :merge-completed) t))
    (agent-repl--ws-put ws :merge-completed t)
    ;; Restore the merged repl-state alongside `:merge-completed'
    ;; so the badge re-appears post-restart instead of falling
    ;; through to `:dead' (or whatever the poll resolves).  A
    ;; persisted `:merge-failed t' wins over the success path: the
    ;; workspace stays merged-completed but surfaces the ❌ badge via
    ;; `:repl-state :merge-failed' so the prior silent-failure
    ;; signal isn't lost across restart.
    (let ((mf (eq (plist-get saved :merge-failed) t)))
      (agent-repl--ws-put ws :merge-failed mf)
      (agent-repl--ws-put ws :repl-state (if mf :merge-failed :merged))
      (agent-repl--log ws "apply-display-state: merge-restored ws=%s outcome=%s"
                        ws (if mf :merge-failed :merged)))
    (when-let ((mca (plist-get saved :merge-completed-at)))
      (agent-repl--ws-put ws :merge-completed-at mca))))

(defun agent-repl--load-display-state (ws project-root)
  "Hydrate WS's persisted display state from PROJECT-ROOT's state file.
Reads PROJECT-ROOT's state file once and applies the persisted
display/metadata keys (`:priority' and the state-badge / merge /
last-prompt fields) to WS via `agent-repl--apply-display-state', so
the tabline badge and workspace glyphs render the moment a workspace
is switched to (`SPC p p') or created — without waiting for
`agent-repl--initialize-ws-env', which only runs when the agent starts.

No-op when WS or PROJECT-ROOT is nil, or when WS has already been
env-initialized (`:active-env' set): an env-initialized workspace
already carries its display state in memory, so re-reading the state
file would be a redundant disk read and could clobber live in-memory
values with staler on-disk ones.  Also a no-op when the state file is
missing or malformed."
  (cond
   ((null ws)
    (agent-repl--log nil "load-display-state: skipped reason=nil-workspace root=%s" project-root))
   ((null project-root)
    (agent-repl--log ws "load-display-state: skipped ws=%s reason=nil-project-root" ws))
   ((agent-repl--ws-get ws :active-env)
    (agent-repl--log ws "load-display-state: skipped ws=%s reason=env-initialized active-env=%s"
                      ws (agent-repl--ws-get ws :active-env)))
   (t
    (let* ((state-file (agent-repl--state-file-for-read project-root))
           (saved (condition-case err
                      (agent-repl--migrate-saved-state
                       (agent-repl--read-sexp-file-if-exists state-file))
                    (error
                     (agent-repl--log ws "load-display-state: read failed ws=%s file=%s err=%S"
                                       ws state-file err)
                     (signal (car err) (cdr err))))))
      (if saved
          (progn
            (agent-repl--log ws "load-display-state: applying ws=%s root=%s file=%s" ws project-root state-file)
            (agent-repl--apply-display-state ws saved)
            (force-mode-line-update t)
            (agent-repl--log ws "load-display-state: complete ws=%s mode-line-updated=t" ws))
        (agent-repl--log ws "load-display-state: skipped ws=%s root=%s reason=state-file-missing file=%s"
                          ws project-root state-file))))))

(defun agent-repl--initialize-ws-env (ws &optional project-dir-hint active-env-hint)
  "Initialize environment state for workspace WS (idempotent).
Writes `:project-dir', `:active-env', and per-env instantiation
structs for WS, and validates the result.

The project root is resolved in this order:
  1. PROJECT-DIR-HINT, if provided (creation path — worktree setup
     or new-workspace pass the known path here).
  2. The already-set `:project-dir' in the workspace plist.
  3. `(agent-repl--git-root default-directory)' — the repo of the
     current buffer.

The state file at that root (`<root>/.claude/emacs/state.el', or the
legacy `<root>/.agent-repl-state' if only it exists) is loaded when
present and its contents supersede the derived defaults (`:project-dir'
from the file is canonical, and per-env instantiation structs are
reconstructed from the saved plists).  When absent, fresh defaults are
written (`:active-env' from ACTIVE-ENV-HINT or `:bare-metal', empty
instantiation structs) and an initial state file is persisted.

Signals an error if `:project-dir' cannot be resolved from any of the
three sources.  Idempotent: safe to call more than once for the same
workspace (a prior partial init is overwritten).

Must not be called while WS has a live agent session — instantiation
structs would be clobbered with any session-id mutations since the last
state-save.  Callers already guard on `agent-repl--agent-running-p'."
  (let* ((root-candidate (or project-dir-hint
                             (agent-repl--ws-get ws :project-dir)
                             (agent-repl--git-root default-directory)))
         (root (and root-candidate (agent-repl--path-canonical root-candidate)))
         (state-file (and root (agent-repl--state-file-for-read root)))
         (state-present-p (and state-file (file-exists-p state-file)))
         (saved (and state-present-p
                     (condition-case err
                         (agent-repl--migrate-saved-state
                          (agent-repl--read-sexp-file state-file))
                       (error
                        (agent-repl--log ws "initialize-ws-env: state-file-read-failed ws=%s file=%s err=%S"
                                          ws state-file err)
                        (signal (car err) (cdr err)))))))
    (unless root
      (agent-repl--log ws "initialize-ws-env: rejected ws=%s reason=project-dir-unresolved hint=%s existing-dir=%s"
                        ws (not (null project-dir-hint))
                        (agent-repl--ws-get ws :project-dir))
      (error "agent-repl--initialize-ws-env: cannot derive :project-dir for ws=%s (no hint, no prior :project-dir, no git-root for default-directory=%s)"
             ws default-directory))
    (when state-present-p
      (unless (and (listp saved) (plist-member saved :project-dir)
                   (stringp (plist-get saved :project-dir)))
        (agent-repl--log ws "initialize-ws-env: rejected ws=%s file=%s reason=invalid-persisted-state value-type=%s has-project-dir=%s project-dir-type=%s"
                          ws state-file (type-of saved)
                          (and (listp saved) (plist-member saved :project-dir))
                          (if (listp saved) (type-of (plist-get saved :project-dir)) :not-a-plist))
        (error "agent-repl--initialize-ws-env: persisted state is invalid for ws=%s file=%s (expected plist with string :project-dir)"
               ws state-file)))
    (agent-repl--log ws "initialize-ws-env: ws=%s root=%s state-present=%s saved=%s hint=%s env-hint=%s"
                      ws root state-present-p (if saved "yes" "no")
                      (if project-dir-hint "yes" "no")
                      (or active-env-hint "nil"))
    ;; Clear any pre-existing `:nuked-at' tombstone before writing the
    ;; identity keys below.  A workspace being re-initialized is, by
    ;; definition, live again; leaving the tombstone in place would let
    ;; `agent-repl--ws-live-p' return nil right after a successful init.
    ;; Enforce one-live-workspace-per-:project-dir: refuse to register WS for
    ;; a dir a DIFFERENT live workspace already owns.  That shadowing is what
    ;; lets a stub (e.g. a Doom-auto-named "#N" perspective) collide with the
    ;; real workspace in `agent-repl--ws-for-dir' and break opening it.
    (let ((target (if saved
                      (agent-repl--path-canonical (plist-get saved :project-dir))
                    root)))
      (when-let ((owner (agent-repl--ws-dir-owner target ws)))
        (agent-repl--log ws "initialize-ws-env: rejected ws=%s target=%s reason=owned-by-live-ws owner=%s"
                          ws target owner)
        (error "agent-repl--initialize-ws-env: refusing to register ws=%s for %s — live workspace %s already owns it"
               ws target owner))
      (agent-repl--ws-put ws :nuked-at nil)
      (agent-repl--ws-put ws :project-dir target))
    (agent-repl--ws-put ws :active-env
                         (or (and saved (plist-get saved :active-env))
                             active-env-hint
                             :bare-metal))
    (agent-repl--log ws "initialize-ws-env: active-env-selected ws=%s env=%s source=%s"
                      ws (agent-repl--ws-get ws :active-env)
                      (cond ((and saved (plist-get saved :active-env)) :persisted)
                            (active-env-hint :hint)
                            (t :new)))
    ;; Display/metadata state — priority badge, repl-state lifecycle,
    ;; merge bookkeeping, last-prompt summary/time, tab slot, fork
    ;; pointer — is hydrated from the saved plist by the shared applier
    ;; that `agent-repl--load-display-state' also drives, so a `SPC p p'
    ;; switch shows the same badges before the agent ever starts.
    (agent-repl--apply-display-state ws saved)
    (dolist (key agent-repl--environment-keys)
      (agent-repl--ws-put ws key
                           (agent-repl--make-instantiation-from-plist
                            (and saved (plist-get saved key)))))
    (agent-repl--validate-ws-env ws)
    (unless saved
      (agent-repl--log ws "initialize-ws-env: writing-initial-state ws=%s root=%s state-present=%s"
                        ws root state-present-p)
      (agent-repl--state-save ws))
    (agent-repl--log ws "initialize-ws-env: complete ws=%s root=%s active-env=%s persisted-state=%s"
                      ws (agent-repl--ws-get ws :project-dir)
                      (agent-repl--ws-get ws :active-env) (if saved "yes" "no"))))


;;;; Command building

(defun agent-repl--compute-claude-flags (session-id fork-session-id perm-flag &optional model)
  "Build the CLI flags string for the Claude command.
SESSION-ID, when non-nil, signals this env has run Claude before and we
should resume its most recent session via `--continue'.  FORK-SESSION-ID
is a session UUID to fork from (used when a new worktree/env needs to
carry a conversation across from another env — the target env has no
local history yet, so `--continue' won't find anything).  PERM-FLAG is
the permission flag string or nil.  MODEL, when non-nil, is the
per-workspace model from the `:model' workspace property — the
workspace-generation JSON's `model' alias, or whatever a model-picking
variant like `SPC j C-o' supplied — and overrides the global default.  It
is ALWAYS the model that was ASKED FOR and never one read back off a live
session; when nil, `agent-repl-interactive-model' supplies the model
\(which itself defaults to \"opus\").  A `--model' flag is appended whenever the resolved
model is non-nil, and a `--system-prompt' flag when `agent-repl-system-prompt'
is non-nil.  Returns a trimmed flags string."
  (let* ((effective-model (agent-repl--effective-model model))
         (flags (string-trim
                (mapconcat #'identity
                           (delq nil (list
                                      ;; Pin the model for every interactive session.
                                      (when effective-model
                                        (format "--model %s" effective-model))
                                      ;; Fork from another session (cross-env/worktree seed).
                                      (when fork-session-id
                                        (format "--resume %s --fork-session" fork-session-id))
                                      ;; Resume most recent session in this env's cwd.
                                      (when (and (not fork-session-id) session-id)
                                        "--continue")
                                      perm-flag
                                      (when agent-repl-system-prompt
                                        ;; Wrap value in literal double quotes
                                        ;; (instead of `shell-quote-argument',
                                        ;; which leaves shell-safe values like
                                        ;; "." bare).  Empirically, the Claude
                                        ;; CLI mis-parses the bare period when
                                        ;; spawning from another Claude session.
                                        (format "--system-prompt \"%s\""
                                                (replace-regexp-in-string
                                                 "\\([\"\\$`]\\)" "\\\\\\1"
                                                 agent-repl-system-prompt)))))
                           " "))))
    (agent-repl--log nil "compute-claude-flags: flags=%s" flags)
    flags))

(defun agent-repl--model-haiku-p (model)
  "Return non-nil when MODEL denotes a Haiku-tier model.
MODEL is a model alias string (e.g. \"haiku\", \"haiku-4-5\",
\"claude-haiku-4-5\") or nil.  Matching is case-insensitive on the
presence of the `haiku' family token so every Haiku variant counts.
Returns nil for nil, empty, or non-Haiku models."
  (let ((result (and (stringp model)
                     (not (string-empty-p model))
                     (string-match-p "haiku" (downcase model))
                     t)))
    (agent-repl--log-verbose nil "model-haiku-p: model=%s model-type=%s result=%s"
                              model (type-of model) result)
    result))

(defun agent-repl--managed-project-p (project-dir)
  "Return non-nil when PROJECT-DIR is a managed (work) project.
Matches the expanded path against `agent-repl-managed-project-pattern'.
Shared by every backend's permission-flag selection (claude and codex
pick different flag spellings for the same managed/personal split).
Signals when PROJECT-DIR is nil, since the split cannot be resolved
without it."
  (unless project-dir
    (agent-repl--log nil "managed-project-p: rejected reason=nil-project-dir")
    (error "agent-repl--managed-project-p: project-dir is nil — cannot determine permission mode"))
  (let ((result (string-match-p agent-repl-managed-project-pattern
                                (expand-file-name project-dir))))
    (agent-repl--log-verbose nil "managed-project-p: project-dir=%s result=%s" project-dir result)
    result))

(defun agent-repl--compute-perm-flag (project-dir &optional model)
  "Return the permission flag string for the Claude CLI, or nil.
PROJECT-DIR determines the base flag: ChessCom repos use
`agent-repl-managed-permission-flag', all others use
`agent-repl-personal-permission-flag' (both default to
--permission-mode auto).  MODEL is the effective interactive model alias;
`--permission-mode auto' is only allowed when MODEL is not Haiku (see
`agent-repl--model-haiku-p'), so a resolved base flag of
`--permission-mode auto' is downgraded to `--dangerously-skip-permissions'
whenever MODEL denotes Haiku."
  (let* ((managed (agent-repl--managed-project-p project-dir))
         (base (if managed
                   agent-repl-managed-permission-flag
                 agent-repl-personal-permission-flag))
         (flag (if (and (agent-repl--model-haiku-p model)
                        (equal base "--permission-mode auto"))
                   "--dangerously-skip-permissions"
                 base)))
    (agent-repl--log nil "compute-perm-flag: branch=%s model=%s flag=%s"
                      (if managed "managed" "personal") model flag)
    flag))

(defun agent-repl--under-dir-p (dir project-dir)
  "Non-nil when PROJECT-DIR is DIR or lies beneath it.
Both paths are expanded and slash-terminated before the prefix test, so
`/a/bc' never counts as living under `/a/b'."
  (let ((result (string-prefix-p (file-name-as-directory (expand-file-name dir))
                                 (file-name-as-directory (expand-file-name project-dir)))))
    (agent-repl--log-verbose nil "under-dir-p: dir=%s project-dir=%s result=%s" dir project-dir result)
    result))

(defun agent-repl--doom-config-tree-p (project-dir)
  "Non-nil when PROJECT-DIR belongs to the doom config tree.
The tree is `agent-repl-doom-config-root' itself plus the sibling
worktrees directory agent-repl creates for it (`~/.config/doom' and
`~/.config/doom-worktrees/'), so a workspace generated off the doom
config counts exactly like the canonical checkout."
  (let* ((root (expand-file-name agent-repl-doom-config-root))
         (worktrees (concat (directory-file-name root)
                            agent-repl-worktree-dir-suffix))
         (main-p (agent-repl--under-dir-p root project-dir))
         (worktree-p (and (not main-p)
                           (agent-repl--under-dir-p worktrees project-dir))))
    (agent-repl--log-verbose nil "doom-config-tree-p: project-dir=%s main=%s worktree=%s" project-dir main-p worktree-p)
    (or main-p worktree-p)))

;;;###autoload
(define-minor-mode agent-repl-doom-multi-repo-mode
  "Global mode putting the doom config tree under the multi-repo root's purview.
Off (the default), only projects beneath the directory named by the
`agent-repl-multi-repo-root-env' environment variable resolve to
`agent-repl-multi-repo-config-dir'; the doom config checkout resolves to
`agent-repl-default-config-dir' like any other personal project.  On,
`agent-repl--doom-config-tree-p' projects resolve to the multi-repo
config dir too, so agent sessions rooted in the doom config (or one of
its generated worktrees) run under the multi-repo account.

The mode is read at session-START time, so toggling it does not
re-point an already-running agent session at another config dir — kill
and restart the session for the new account to take effect."
  :global t
  :init-value nil
  :group 'agent-repl
  (let ((dir (agent-repl--compute-config-dir agent-repl-doom-config-root)))
    (agent-repl--log nil "doom-multi-repo-mode: %s doom-config-dir=%s"
                     (if agent-repl-doom-multi-repo-mode "enabled" "disabled")
                     (or dir "<claude default ~/.claude>"))
    (message "agent-repl: doom config tree %s the multi-repo root — config dir %s"
             (if agent-repl-doom-multi-repo-mode "counts as under" "is outside")
             (or dir "~/.claude (Claude default)"))))

(defun agent-repl--under-multi-repo-p (project-dir)
  "Non-nil when PROJECT-DIR counts as living under the multi-repo root.
True when PROJECT-DIR lies under the directory named by the
`agent-repl-multi-repo-root-env' environment variable, and also when
`agent-repl-doom-multi-repo-mode' is on and PROJECT-DIR belongs to the
doom config tree (see `agent-repl--doom-config-tree-p')."
  (let* ((root (getenv agent-repl-multi-repo-root-env))
         (root-match-p (and root (> (length root) 0)
                            (agent-repl--under-dir-p root project-dir)))
         (doom-match-p (and (not root-match-p) agent-repl-doom-multi-repo-mode
                            (agent-repl--doom-config-tree-p project-dir)))
         (result (or root-match-p doom-match-p)))
    (agent-repl--log-verbose nil "under-multi-repo-p: project-dir=%s root-present=%s root-match=%s doom-mode=%s doom-match=%s result=%s"
                              project-dir (not (null root)) root-match-p
                              agent-repl-doom-multi-repo-mode doom-match-p result)
    result))

(defun agent-repl--compute-config-dir (project-dir)
  "Return the CLAUDE_CONFIG_DIR to use for PROJECT-DIR, or nil.

THE PATH IS THE ONLY INPUT.  When PROJECT-DIR counts as under the
multi-repo root (see `agent-repl--under-multi-repo-p'), returns the
expanded `agent-repl-multi-repo-config-dir' (the dodge@chess.com
account).  Otherwise returns the expanded
`agent-repl-default-config-dir', or nil when that is nil so the CLI
falls back to its default ~/.claude (the dodge.w.coates@gmail.com
account).  Signals an error when PROJECT-DIR is nil, since account
selection cannot be resolved without it.

THERE IS NO PER-WORKSPACE OVERRIDE, and that is the point.  A
`:config-dir-override' used to win outright here, so one workspace's
account could disagree with the account its path names — and because an
absent value on the wire is indistinguishable from a deliberate \\='use
the CLI default\\=', a creation that named no account silently pinned
its workspace to ~/.claude.  A workspace under $MULTI_REPO_ROOT then
wrote its transcript into a root nothing else reads and could never
resume.  With the path as the sole determinant, a workspace and its
transcripts cannot end up in different accounts."
  (unless project-dir
    (agent-repl--log nil "compute-config-dir: rejected reason=nil-project-dir")
    (error "agent-repl--compute-config-dir: project-dir is nil — cannot determine account"))
  (let* ((under-multi-repo (agent-repl--under-multi-repo-p project-dir))
         (dir (if under-multi-repo
                  agent-repl-multi-repo-config-dir
                agent-repl-default-config-dir)))
    (agent-repl--log nil "compute-config-dir: project-dir=%s root=%s doom-mode=%s branch=%s dir=%s"
                      project-dir (getenv agent-repl-multi-repo-root-env)
                      agent-repl-doom-multi-repo-mode
                      (if under-multi-repo "multi-repo" "default") dir)
    (and dir (expand-file-name dir))))

(defun agent-repl--assemble-cmd (claude-flags &optional config-dir)
  "Assemble the final `claude' shell command string.
CLAUDE-FLAGS is the pre-built flags string.  CONFIG-DIR, when non-nil,
is prepended as a `CLAUDE_CONFIG_DIR=...' environment assignment so the
launched Claude uses that account's credentials.

`AGENT_REPL_OWNED=1' is ALWAYS prepended: the hook scripts stamp the
ownership marker into their sentinel files only for module-launched
CLIs, which is what stops a foreign claude in the same cwd (e.g. a
terminal session) from having its session id adopted onto the
workspace (see `agent-repl--update-session-id-from-sentinel').

agent-repl ALWAYS launches plain `claude': there is exactly one launch
shape, with no environment-conditional branching."
  (let* ((base (concat "claude " claude-flags))
         (env-prefix (concat "AGENT_REPL_OWNED=1 "
                             (if config-dir
                                 (format "CLAUDE_CONFIG_DIR=%s " (shell-quote-argument config-dir))
                               "")))
         (cmd (string-trim (concat env-prefix base))))
    (agent-repl--log nil "assemble-cmd: cmd=%s" cmd)
    cmd))

(defun agent-repl--claude-start-cmd (opts)
  "Build the interactive `claude' start command from OPTS.
This is the `claude' backend's START-CMD-FN (see `agent-repl-backend').
OPTS is a plist carrying `:session-id', `:fork-session-id',
`:project-dir' and `:model' (any may be nil except `:project-dir').
Returns the full shell command string, wrapping the perm-flag,
config-dir and flag-assembly helpers."
  (let* ((session-id      (plist-get opts :session-id))
         (fork-session-id (plist-get opts :fork-session-id))
         (project-dir     (plist-get opts :project-dir))
         (model           (plist-get opts :model))
         ;; The EFFECTIVE model drives the permission flag: Haiku
         ;; sessions can't use `--permission-mode auto', so resolve the
         ;; interactive-model fallback here (mirroring what
         ;; `--compute-claude-flags' resolves for the --model flag).
         (effective-model (or model agent-repl-interactive-model))
         (perm-flag   (agent-repl--compute-perm-flag project-dir effective-model))
         (config-dir  (agent-repl--compute-config-dir project-dir))
         (claude-flags (agent-repl--compute-claude-flags
                        session-id fork-session-id perm-flag model)))
    (agent-repl--log nil "claude-start-cmd: project-dir=%s session-id=%s fork-session-id=%s model=%s effective-model=%s perm-flag=%s config-dir=%s"
                      project-dir session-id fork-session-id model effective-model perm-flag config-dir)
    (agent-repl--assemble-cmd claude-flags config-dir)))

(defun agent-repl--claude-headless-cmd (model extra-args)
  "Return the argv for a one-shot headless `claude' run.
This is the `claude' backend's HEADLESS-CMD-FN (see
`agent-repl-backend').  MODEL is the `--model' alias; EXTRA-ARGS is a
list of additional flags appended after the standard `-p --model MODEL'
prefix.  `-p' makes `claude' exit after a single turn; the prompt is
delivered on the process's stdin by the caller."
  (let ((argv (append (list "claude" "-p" "--model" model) extra-args)))
    (agent-repl--log nil "claude-headless-cmd: model=%s extra-args-count=%d argv-count=%d"
                      model (length extra-args) (length argv))
    argv))

;;;; Session completion handling

(defun agent-repl--maybe-notify-finished (ws)
  "Send a desktop notification that the agent finished in WS.
Only fires when Emacs is not the focused desktop application (any-frame
focus, see `agent-repl--emacs-focused-p'), so a banner never appears
while the user is already looking at Emacs.  Debounces per-workspace to
avoid duplicate notifications when both the hook and title-change
paths fire for the same turn completion."
  (let ((last (agent-repl--ws-get ws :last-notify-time))
        (now  (float-time))
        (focused (agent-repl--emacs-focused-p)))
    (agent-repl--log ws "maybe-notify-finished: ws=%s focused=%s last-present=%s"
                      ws focused (not (null last)))
    (if (and (not focused)
             (or (null last) (> (- now last) agent-repl-notify-debounce-seconds)))
        (progn
          (agent-repl--ws-put ws :last-notify-time now)
          (run-at-time agent-repl-notify-delay nil #'agent-repl--notify ws "Agent REPL"
                       (format "%s: Agent ready" ws))
          (agent-repl--log ws "maybe-notify-finished: scheduled ws=%s delay=%.2f"
                            ws agent-repl-notify-delay))
      (when (and last (<= (- now last) agent-repl-notify-debounce-seconds))
        (agent-repl--log ws "maybe-notify-finished: debounce-hit ws=%s elapsed=%.2f" ws (- now last)))
      (when focused
        (agent-repl--log ws "maybe-notify-finished: skipped ws=%s reason=emacs-focused" ws)))))

(defun agent-repl--mark-agent-done (ws)
  "Mark WS's agent-state as :done.
Unconditional: called on every Stop hook.

No viewed-acknowledgment is recorded: `:done', `:ready' and `:idle' are
all READY under the five-color vocabulary, so there is no decay left for
an acknowledgment to pace.

Fires the finished desktop notification via
`agent-repl--maybe-notify-finished', so every transition to :done —
not just the Stop-hook completion path — notifies the user when Emacs
is unfocused.  The notification is gated on frame focus and debounced
there, so the interrupt and /clear paths (which mark :done while the
user is focused) stay silent."
  (agent-repl--log ws "mark-agent-done ws=%s merged=%s repl-state=%s merge-completed-at=%s"
                    ws
                    (or (eq (agent-repl--ws-get ws :repl-state) :merged)
                        (eq (agent-repl--ws-get ws :merge-completed) t))
                    (agent-repl--ws-get ws :repl-state)
                    (agent-repl--ws-get ws :merge-completed-at))
  (agent-repl--ws-set-agent-state ws :done)
  (agent-repl--log ws "mark-agent-done: state-updated ws=%s current=%s" ws (agent-repl--current-ws-p ws))
  (agent-repl--maybe-notify-finished ws))

(defun agent-repl--refresh-magit-status-for-dir (dir &optional ws)
  "Refresh any magit-status buffer whose `default-directory' canonicalizes to DIR.
Iterates `buffer-list', filters to `magit-status-mode' buffers whose
`default-directory' matches the canonical form of DIR, and runs
`magit-refresh' in each.  No-op when DIR is nil or has no matching
buffer.

WS is optional and used only for the log line so a caller with a
workspace context (e.g. `agent-repl--refresh-magit-status') can keep
the existing log prefix; directory-keyed callers (e.g. the post-merge
refresh after a merge) pass nil and the log
falls back to the bare directory."
  (if-let* ((canonical (and dir (agent-repl--path-canonical dir))))
      (let ((refreshed 0))
        (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (with-current-buffer buf
                   (and (eq major-mode 'magit-status-mode)
                        (equal (agent-repl--path-canonical default-directory)
                               canonical))))
        (agent-repl--log ws "refresh-magit-status-for-dir: dir=%s refreshing buf=%s"
                          canonical (buffer-name buf))
        (with-current-buffer buf (magit-refresh))
        (setq refreshed (1+ refreshed))))
        (agent-repl--log ws "refresh-magit-status-for-dir: complete dir=%s refreshed-count=%d"
                          canonical refreshed))
    (agent-repl--log ws "refresh-magit-status-for-dir: skipped dir=%s reason=nil-directory" dir)))

(defun agent-repl--refresh-magit-status (ws)
  "Refresh any magit-status buffer whose repo root matches WS's :project-dir.
No-op when WS has no :project-dir or no matching buffer exists.

Thin wrapper over `agent-repl--refresh-magit-status-for-dir' so the
WS-keyed call sites (e.g. `--handle-agent-finished') and
directory-keyed call sites (e.g. the post-merge refresh in
a merge teardown, which has the target directory but not a
target workspace) share the same buffer-matching logic."
  (agent-repl--refresh-magit-status-for-dir
   (agent-repl--ws-get ws :project-dir) ws))

(defun agent-repl--handle-agent-finished (ws)
  "Handle the agent finishing in WS.
Errors hard if WS is not registered in `agent-repl--workspaces' — a
stop event arriving for an unknown workspace indicates a race (e.g.
sentinel firing after kill cleared state) that we surface rather than
silently absorb.  Otherwise: marks agent-state as :done (which also
notifies the user when the frame is unfocused — see
`agent-repl--mark-agent-done'), refreshes any open magit-status buffer
for the workspace's repo, emits a finished-in-workspace message when the
current workspace is different, and drains any deferred-prompt queue
\(see `agent-repl--drain-deferred-prompts')."
  (unless (agent-repl--ws-known-p ws)
    (agent-repl--log ws "handle-agent-finished: rejected ws=%S reason=unregistered" ws)
    (error "agent-repl--handle-agent-finished: ws=%S not registered in agent-repl--workspaces" ws))
  (agent-repl--log ws "handle-agent-finished ws=%s" ws)
  (agent-repl--mark-agent-done ws)
  (agent-repl--refresh-magit-status ws)
  (if (agent-repl--current-ws-p ws)
      (agent-repl--log ws "handle-agent-finished: ws=%s current=t notification=skipped" ws)
    (agent-repl--info ws "Agent finished in workspace: %s" ws))
  (agent-repl--drain-deferred-prompts ws)
  (agent-repl--log ws "handle-agent-finished: complete ws=%s" ws))

;;;; Deferred prompt queue
;;
;; `:deferred-prompts' is a runtime FIFO seeded by the leader-key
;; command `agent-repl-queue-deferred-prompt' (bound to `SPC j RET'):
;; the user keeps typing prompts while the agent is busy, and each one
;; is held until the agent reaches `:done' / `:idle', at which point the
;; head of the queue is sent.  Subsequent prompts drain one per
;; finished turn.  The queue is arbitrarily long.

(defun agent-repl--deferred-drain-eligible-p (ws)
  "Return non-nil if WS's agent-state permits a deferred-queue drain.
Drains are only permitted from `:done' or `:idle' — sending mid-turn
would defeat the whole point of the deferral.  Returns nil for any
other state (or nil)."
  (let* ((state (agent-repl--ws-agent-state ws))
         (eligible (memq state '(:done :idle))))
    (agent-repl--log-verbose ws "deferred-drain-eligible-p: ws=%s state=%s eligible=%s" ws state eligible)
    eligible))

(defun agent-repl--pop-deferred-prompt (ws)
  "Pop and return the head of WS's `:deferred-prompts' queue, or nil.
Mutates the workspace plist in place.  Logs the pop with the resulting
queue depth so drains are easy to trace."
  (let* ((q (agent-repl--ws-get ws :deferred-prompts))
         (head (car q))
         (rest (cdr q)))
    (when head
      (agent-repl--ws-put ws :deferred-prompts rest)
      (agent-repl--log ws "pop-deferred-prompt: ws=%s len-after=%d head-len=%d"
                        ws (length rest) (length head)))
    (unless head
      (agent-repl--log-verbose ws "pop-deferred-prompt: ws=%s queue-empty=t" ws))
    head))

(defun agent-repl--drain-deferred-prompts (ws)
  "Send the next deferred prompt for WS if the state and queue allow.
Called from `agent-repl--handle-agent-finished' (`:thinking → :done'
turn boundary) and from `agent-repl-queue-deferred-prompt' (so a
prompt enqueued while WS is already `:done'/`:idle' fires immediately).

Sends exactly one prompt per call.  Sending re-enters the agent into
`:thinking' via the `UserPromptSubmit' hook; the next `handle-claude-
finished' for this workspace will re-trigger the drain and pop the
next queued prompt.  This keeps the deferred queue strictly serialized
with the agent's turn boundaries — the whole point of using the queue
over the agent's native paste-while-thinking buffering."
  (cond
   ((null (agent-repl--ws-get ws :deferred-prompts))
    (agent-repl--log-verbose ws "drain-deferred-prompts: ws=%s queue empty" ws))
   ((not (agent-repl--deferred-drain-eligible-p ws))
    (agent-repl--log ws "drain-deferred-prompts: ws=%s skipped — state=%s not :done/:idle"
                      ws (agent-repl--ws-agent-state ws)))
   (t
    (let ((prompt (agent-repl--pop-deferred-prompt ws)))
      (agent-repl--log ws "drain-deferred-prompts: ws=%s sending head len=%d remaining=%d"
                        ws (length prompt)
                        (length (agent-repl--ws-get ws :deferred-prompts)))
      (agent-repl--send "PROMPT_ORIGIN_DEFERRED_PROMPT" prompt ws)))))

;;;; Session ID management

(defun agent-repl--set-session-id (ws id)
  "Set the session ID for workspace WS to ID and persist to disk.
Persisting on capture is what makes a hook-delivered SID durable
through an Emacs crash — without this `state-save', the SID would
only reach the per-project state file at graceful teardown, so a crash
mid-session would drop the resume signal and the next launch would
not pass `--continue'."
  (agent-repl--log ws "set-session-id: ws=%s id=%s (was %s)"
                    ws id
                    (or (agent-repl-instantiation-session-id
                         (agent-repl--active-inst ws))
                        "nil"))
  (setf (agent-repl-instantiation-session-id (agent-repl--active-inst ws)) id)
  (agent-repl--state-save ws))

;; Session ID capture is handled exclusively by Claude Code hooks.
;; Every hook event (session_start, stop, prompt_submit, permission_prompt)
;; delivers session_id in the sentinel file, and
;; agent-repl--update-session-id-from-sentinel (in sentinel.el) sets it
;; on the workspace's active instantiation.  No file scanning needed.

(defun agent-repl--loading-placeholder-visible-p ()
  "Return non-nil if the loading placeholder buffer is displayed in a window."
  (let* ((ph (get-buffer " *agent-loading*"))
         (window (and ph (get-buffer-window ph))))
    (agent-repl--log-verbose (agent-repl--ws-current-log-name)
                              "loading-placeholder-visible-p: buffer-present=%s visible=%s"
                              (not (null ph)) (not (null window)))
    window))

;; Panel opening after readiness is handled entirely by the session_start
;; hook via sentinel.el, which sets ready state and opens panels through the
;; workspace-switch `:pending-show-panels' drain.
;; The two helpers that used to run a second panel-open pass off the old
;; readiness POLL (`--show-panels-or-defer', `--open-panels-after-ready')
;; had no caller left after that cutover and are gone.

;;;; Process state predicates

(defun agent-repl--agent-running-p (&optional ws)
  "Return non-nil when WS has a live agent session on ITS OWN frontend.
WS defaults to the current workspace name.  Signals an error if no
workspace can be determined.

Dispatches through the frontend registry rather than looking for a live
vterm process directly, so a gui workspace's daemon session counts as
running too.  While this asked only about `:vterm-buffer' it answered
\"not running\" for EVERY gui workspace, which silently disarmed every
guard keyed to it: the refusal to switch backend under a live agent
\(`agent-repl-select-backend'), the kill-before-workspace-delete advice,
and the status poll's liveness gate.

Each registered frontend's `:running-p-fn' must point at a concrete
liveness check (e.g. `agent-repl--gui-running-p' for the gui frontend)
rather than back at this function, or the dispatch closes a loop on
itself."
  (let ((ws (or ws (agent-repl--ws-current-name))))
    (unless ws
      (agent-repl--log nil "agent-running-p: rejected reason=no-workspace")
      (error "agent-repl--agent-running-p: no workspace specified and no current workspace"))
    (let* ((frontend (agent-repl--ws-frontend ws))
           (running (funcall (agent-repl-frontend-running-p-fn frontend) ws)))
      (agent-repl--log-verbose ws "agent-running-p: ws=%s frontend=%s running=%s" ws
                                (agent-repl-frontend-name frontend) running)
      running)))

;; Alt-account config-dir HOOK provisioning was removed in the S8/S9
;; sentinel endgame: Emacs manages no Claude Code hooks, so there is
;; nothing to replicate into a per-account CLAUDE_CONFIG_DIR's
;; settings.json.  The per-account launch logic
;; (`agent-repl--compute-config-dir') still selects the right config dir
;; for the session (createSession's `configDir'); the daemon owns the
;; session's harness behavior from there.
