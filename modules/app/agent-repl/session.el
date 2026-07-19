;;; session.el --- session lifecycle management -*- lexical-binding: t; -*-

;;; Code:

(declare-function agent-repl--ws-dir-owner "agent-repl-workspace" (dir &optional except))
(declare-function agent-repl--ws-gui-frontend-p "frontends" (ws))
(declare-function agent-repl--gui-running-p "frontend-client" (ws))
(declare-function agent-repl--ws-frontend "frontends" (ws))
(declare-function agent-repl-frontend-running-p-fn "frontends" (frontend))
(declare-function agent-repl--frontend-dispatch-show "frontends" (ws))
(declare-function agent-repl--ensure-input-buffer "agent-repl-panels" (ws))

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
gui-frontend create path (`agent-repl--frontend-create-session').  A nil
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

(defcustom agent-repl-pending-prompt-deliver-delay 0.3
  "Seconds to wait before delivering pending prompts after the agent becomes ready."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-prompt-delivery-verify-seconds 4.0
  "Seconds to wait after a preemptive prompt before verifying delivery.
A preemptive prompt is considered acknowledged when `:agent-state'
transitions away from `:idle' (the `UserPromptSubmit' hook fires and
`--on-prompt-submit-event' flips state to `:thinking').  When the state
is still `:idle' after this window, the bracketed paste is assumed to
have raced the agent's TUI input-area paint and is resent — see
`agent-repl-prompt-delivery-max-retries'."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-prompt-delivery-max-retries 2
  "Maximum resend attempts when a preemptive prompt is not acknowledged.
A preemptive prompt is considered acknowledged when `:agent-state'
moves away from `:idle' within `agent-repl-prompt-delivery-verify-seconds'.
If the state is still `:idle' after the verify window, the prompt is
resent.  After this many resends have all failed to elicit a state
transition, the delivery is abandoned with a user-visible warning
rather than looping forever."
  :type 'integer
  :group 'agent-repl)

;;;; Workspace environment initialization

(defun agent-repl--apply-display-state (ws saved)
  "Apply persisted display/metadata state from SAVED plist onto workspace WS.
SAVED is a parsed state-file plist (or nil).  Hydrates the
non-env-struct keys that drive the tabline badge and workspace state
glyphs:
`:priority', `:source-ws-dir', `:model', `:last-prompt-time',
`:repl-state', `:saved-tab-index', `:backend', `:fork-session-id',
`:backend-session-stash', `:config-dir-override', `:frontend' (only
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
  ;; Account override: prefer the saved value, fall back to whatever is
  ;; already in the plist (e.g., an `account_changed_' sentinel handled
  ;; before any state-save happened).  Restoring it is load-bearing: a
  ;; switched workspace whose override was lost would re-create its next
  ;; session under the path-computed account it deliberately moved off.
  (agent-repl--ws-put ws :config-dir-override
                       (or (and saved (plist-get saved :config-dir-override))
                           (agent-repl--ws-get ws :config-dir-override)))
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
  ;; Last-prompt-time: prefer saved value, fall back to whatever is
  ;; already in the plist.  Records when the last user prompt was
  ;; sent; survives Emacs restarts so duration-since-last-prompt
  ;; displays reflect real elapsed wall-clock, not session age.
  (agent-repl--ws-put ws :last-prompt-time
                       (or (and saved (plist-get saved :last-prompt-time))
                           (agent-repl--ws-get ws :last-prompt-time)))
  ;; Repl-state: hydrate the *desired* panel-visibility lifecycle from the
  ;; saved file so `:inactive' (panels closed via plain `SPC o c') and
  ;; `:hidden' (deprio-close via `SPC o C') survive Emacs restart.  Only
  ;; persistable values matter at restart — `:dead'/nil reduce to "no
  ;; opinion, default to opening panels", so we only restore `:active' /
  ;; `:inactive' / `:hidden'.  `--open-panels-after-ready' reads this on
  ;; first ready and skips the panel-open call for `:inactive'/`:hidden';
  ;; `--maybe-sweep-hidden-on-switch' demotes `:hidden' to `:inactive'
  ;; when the user actually arrives back on the workspace.
  (let ((saved-repl-state (and saved (plist-get saved :repl-state))))
    (when (memq saved-repl-state '(:active :inactive :hidden))
      (agent-repl--ws-put ws :repl-state saved-repl-state)))
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
      (agent-repl--ws-put ws :repl-state (if mf :merge-failed :merged)))
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
  (when (and ws project-root
             (null (agent-repl--ws-get ws :active-env)))
    (let* ((state-file (agent-repl--state-file-for-read project-root))
           (saved (agent-repl--migrate-saved-state
                   (and state-file
                        (condition-case err
                            (agent-repl--read-sexp-file-if-exists state-file)
                          (error
                           (agent-repl--log ws "load-display-state: read error file=%s err=%S"
                                             state-file err)
                           nil))))))
      (when saved
        (agent-repl--log ws "load-display-state: ws=%s root=%s" ws project-root)
        (agent-repl--apply-display-state ws saved)
        (force-mode-line-update t)))))

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
         (saved (agent-repl--migrate-saved-state
                 (and state-file
                      (file-exists-p state-file)
                      (condition-case err
                          (agent-repl--read-sexp-file state-file)
                        (error
                         (agent-repl--log ws "initialize-ws-env: state file read error file=%s err=%S"
                                           state-file err)
                         nil))))))
    (unless root
      (error "agent-repl--initialize-ws-env: cannot derive :project-dir for ws=%s (no hint, no prior :project-dir, no git-root for default-directory=%s)"
             ws default-directory))
    (agent-repl--log ws "initialize-ws-env: ws=%s root=%s saved=%s hint=%s env-hint=%s"
                      ws root (if saved "yes" "no")
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
        (error "agent-repl--initialize-ws-env: refusing to register ws=%s for %s — live workspace %s already owns it"
               ws target owner))
      (agent-repl--ws-put ws :nuked-at nil)
      (agent-repl--ws-put ws :project-dir target))
    (agent-repl--ws-put ws :active-env
                         (or (and saved (plist-get saved :active-env))
                             active-env-hint
                             :bare-metal))
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
      (agent-repl--log ws "initialize-ws-env: no state file, writing initial state ws=%s root=%s" ws root)
      (agent-repl--state-save ws))))


;;;; Command building

(defun agent-repl--compute-claude-flags (session-id fork-session-id perm-flag &optional model)
  "Build the CLI flags string for the Claude command.
SESSION-ID, when non-nil, signals this env has run Claude before and we
should resume its most recent session via `--continue'.  FORK-SESSION-ID
is a session UUID to fork from (used when a new worktree/env needs to
carry a conversation across from another env — the target env has no
local history yet, so `--continue' won't find anything).  PERM-FLAG is
the permission flag string or nil.  MODEL, when non-nil, is the
per-workspace model from the `:model' workspace property — either the
workspace-generation JSON's `model' alias or, once a session has run, the
current model captured from the config dir and persisted across restarts
\(see `agent-repl--model-persist-value') — and overrides the global
default; when nil, `agent-repl-interactive-model' supplies the model
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
  (and (stringp model)
       (not (string-empty-p model))
       (string-match-p "haiku" (downcase model))
       t))

(defun agent-repl--managed-project-p (project-dir)
  "Return non-nil when PROJECT-DIR is a managed (work) project.
Matches the expanded path against `agent-repl-managed-project-pattern'.
Shared by every backend's permission-flag selection (claude and codex
pick different flag spellings for the same managed/personal split).
Signals when PROJECT-DIR is nil, since the split cannot be resolved
without it."
  (unless project-dir
    (error "agent-repl--managed-project-p: project-dir is nil — cannot determine permission mode"))
  (string-match-p agent-repl-managed-project-pattern
                  (expand-file-name project-dir)))

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
  (string-prefix-p (file-name-as-directory (expand-file-name dir))
                   (file-name-as-directory (expand-file-name project-dir))))

(defun agent-repl--doom-config-tree-p (project-dir)
  "Non-nil when PROJECT-DIR belongs to the doom config tree.
The tree is `agent-repl-doom-config-root' itself plus the sibling
worktrees directory agent-repl creates for it (`~/.config/doom' and
`~/.config/doom-worktrees/'), so a workspace generated off the doom
config counts exactly like the canonical checkout."
  (let* ((root (expand-file-name agent-repl-doom-config-root))
         (worktrees (concat (directory-file-name root)
                            agent-repl-worktree-dir-suffix)))
    (or (agent-repl--under-dir-p root project-dir)
        (agent-repl--under-dir-p worktrees project-dir))))

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
  (let ((root (getenv agent-repl-multi-repo-root-env)))
    (or (and root
             (> (length root) 0)
             (agent-repl--under-dir-p root project-dir))
        (and agent-repl-doom-multi-repo-mode
             (agent-repl--doom-config-tree-p project-dir)))))

(defun agent-repl--compute-config-dir (project-dir)
  "Return the CLAUDE_CONFIG_DIR to use for PROJECT-DIR, or nil.

A workspace-level `:config-dir-override' wins outright when PROJECT-DIR
resolves to a workspace carrying one — the mark a daemon-side account
switch (or a restored state file) left, meaning the user deliberately
moved this session off its computed default.  The override is either a
config-dir string or the keyword `:default' (the CLI's own ~/.claude
root, which the string convention below expresses as nil).

Otherwise the account is computed from the path: when PROJECT-DIR
counts as under the multi-repo root (see
`agent-repl--under-multi-repo-p'), returns the expanded
`agent-repl-multi-repo-config-dir' (the dodge@chess.com account).
Otherwise returns the expanded `agent-repl-default-config-dir', or nil
when that is nil so the CLI falls back to its default ~/.claude (the
dodge.w.coates@gmail.com account).  Signals an error when PROJECT-DIR is
nil, since account selection cannot be resolved without it."
  (unless project-dir
    (error "agent-repl--compute-config-dir: project-dir is nil — cannot determine account"))
  (let* ((ws (agent-repl--ws-for-dir project-dir))
         (override (and ws (agent-repl--ws-get ws :config-dir-override))))
    (cond
     ((eq override :default)
      (agent-repl--log ws "compute-config-dir: ws=%s override=:default -> CLI default root" ws)
      nil)
     ((stringp override)
      (agent-repl--log ws "compute-config-dir: ws=%s override=%s" ws override)
      (expand-file-name override))
     (t
      (let* ((under-multi-repo (agent-repl--under-multi-repo-p project-dir))
             (dir (if under-multi-repo
                      agent-repl-multi-repo-config-dir
                    agent-repl-default-config-dir)))
        (agent-repl--log nil "compute-config-dir: project-dir=%s root=%s doom-mode=%s branch=%s dir=%s"
                          project-dir (getenv agent-repl-multi-repo-root-env)
                          agent-repl-doom-multi-repo-mode
                          (if under-multi-repo "multi-repo" "default") dir)
        (and dir (expand-file-name dir)))))))

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

agent-repl ALWAYS launches plain `claude'.  There is deliberately no
sandbox branch here, and there never was one: the retired `:sandbox'
environment was a label, not a container."
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
    (agent-repl--assemble-cmd claude-flags config-dir)))

(defun agent-repl--claude-headless-cmd (model extra-args)
  "Return the argv for a one-shot headless `claude' run.
This is the `claude' backend's HEADLESS-CMD-FN (see
`agent-repl-backend').  MODEL is the `--model' alias; EXTRA-ARGS is a
list of additional flags appended after the standard `-p --model MODEL'
prefix.  `-p' makes `claude' exit after a single turn; the prompt is
delivered on the process's stdin by the caller."
  (append (list "claude" "-p" "--model" model) extra-args))

;;;; Session completion handling

(defun agent-repl--maybe-notify-finished (ws)
  "Send a desktop notification that the agent finished in WS.
Only fires when the frame is unfocused.  Debounces per-workspace to
avoid duplicate notifications when both the hook and title-change
paths fire for the same turn completion."
  (agent-repl--log ws "maybe-notify-finished ws=%s focused=%s" ws (if (frame-focus-state) "yes" "no"))
  (let ((last (agent-repl--ws-get ws :last-notify-time))
        (now  (float-time)))
    (if (and (not (frame-focus-state))
             (or (null last) (> (- now last) agent-repl-notify-debounce-seconds)))
        (progn
          (agent-repl--ws-put ws :last-notify-time now)
          (run-at-time agent-repl-notify-delay nil #'agent-repl--notify ws "Agent REPL"
                       (format "%s: Agent ready" ws)))
      (when (and last (<= (- now last) agent-repl-notify-debounce-seconds))
        (agent-repl--log ws "maybe-notify-finished: debounce-hit ws=%s elapsed=%.2f" ws (- now last))))))

(defun agent-repl--mark-agent-done (ws)
  "Mark WS's agent-state as :done.
Unconditional: called on every Stop hook.  Also manages the
`:done-acked' acknowledgment flag and `:done-acked-at' focus-start
timestamp (orthogonal to `:repl-state'):
  - If WS is the current workspace, the user is actively looking at
    this :done as it arrives — set `:done-acked' to t and stamp
    `:done-acked-at' with the current time so the decay timer can
    clear it once `agent-repl-done-idle-delay' seconds have
    elapsed.
  - Otherwise, clear both flags so this fresh :done starts
    unacknowledged (regardless of any leftover ack from a prior
    cycle); `on-workspace-switch' sets them when the user next
    selects the workspace."
  (agent-repl--log ws "mark-agent-done ws=%s" ws)
  (agent-repl--ws-set-agent-state ws :done)
  (let ((current (agent-repl--current-ws-p ws)))
    (agent-repl--ws-put ws :done-acked current)
    (agent-repl--ws-put ws :done-acked-at (and current (float-time)))))

(defun agent-repl--refresh-magit-status-for-dir (dir &optional ws)
  "Refresh any magit-status buffer whose `default-directory' canonicalizes to DIR.
Iterates `buffer-list', filters to `magit-status-mode' buffers whose
`default-directory' matches the canonical form of DIR, and runs
`magit-refresh' in each.  No-op when DIR is nil or has no matching
buffer.

WS is optional and used only for the log line so a caller with a
workspace context (e.g. `agent-repl--refresh-magit-status') can keep
the existing log prefix; directory-keyed callers (e.g. the post-merge
refresh in `agent-repl--workspace-merge-do') pass nil and the log
falls back to the bare directory."
  (when-let* ((canonical (and dir (agent-repl--path-canonical dir))))
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (with-current-buffer buf
                   (and (eq major-mode 'magit-status-mode)
                        (equal (agent-repl--path-canonical default-directory)
                               canonical))))
        (agent-repl--log ws "refresh-magit-status-for-dir: dir=%s refreshing buf=%s"
                          canonical (buffer-name buf))
        (with-current-buffer buf (magit-refresh))))))

(defun agent-repl--refresh-magit-status (ws)
  "Refresh any magit-status buffer whose repo root matches WS's :project-dir.
No-op when WS has no :project-dir or no matching buffer exists.

Thin wrapper over `agent-repl--refresh-magit-status-for-dir' so the
WS-keyed call sites (e.g. `--handle-agent-finished') and
directory-keyed call sites (e.g. the post-merge refresh in
`--workspace-merge-do', which has the target directory but not a
target workspace) share the same buffer-matching logic."
  (agent-repl--refresh-magit-status-for-dir
   (agent-repl--ws-get ws :project-dir) ws))

(defun agent-repl--handle-agent-finished (ws)
  "Handle the agent finishing in WS.
Errors hard if WS is not registered in `agent-repl--workspaces' — a
stop event arriving for an unknown workspace indicates a race (e.g.
sentinel firing after kill cleared state) that we surface rather than
silently absorb.  Otherwise: marks agent-state as :done, refreshes any
open magit-status buffer for the workspace's repo, notifies the user if
the frame is unfocused, emits a finished-in-workspace message when the
current workspace is different, and drains any deferred-prompt queue
\(see `agent-repl--drain-deferred-prompts')."
  (unless (gethash ws agent-repl--workspaces)
    (error "agent-repl--handle-agent-finished: ws=%S not registered in agent-repl--workspaces" ws))
  (agent-repl--log ws "handle-agent-finished ws=%s" ws)
  (agent-repl--mark-agent-done ws)
  (agent-repl--refresh-magit-status ws)
  (agent-repl--maybe-notify-finished ws)
  (unless (agent-repl--current-ws-p ws)
    (agent-repl--info ws "Agent finished in workspace: %s" ws))
  (agent-repl--drain-deferred-prompts ws))

;;;; Deferred prompt queue
;;
;; Distinct from `:pending-prompts' (the at-startup queue drained when
;; the session_start hook arrives — see `--drain-pending-prompts').
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
  (memq (agent-repl--ws-agent-state ws) '(:done :idle)))

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
      (agent-repl--send prompt ws)))))

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

;;;; Readiness and pending prompt handling

(defun agent-repl--prompt-acknowledged-p (ws)
  "Return non-nil when WS's `:agent-state' indicates the agent received a prompt.
Acknowledged states are `:thinking' (the `UserPromptSubmit' hook flipped
state via `--on-prompt-submit-event'), `:permission' (the agent paused
to ask for permission), or `:done' (a fast turn already finished).
Returns nil for `:idle' / `:init' / nil — i.e. when the prompt does
not appear to have reached the agent."
  (memq (agent-repl--ws-agent-state ws)
        '(:thinking :permission :done)))

(defun agent-repl--pending-delivery-alive-p (ws vterm-buf)
  "Return non-nil when WS can still receive queued prompt deliveries.
Frontend-aware liveness for the pending-prompt pipeline: a gui
workspace is deliverable while it has a daemon session binding
\(`agent-repl--gui-running-p' — VTERM-BUF is nil by design there); a
vterm workspace is deliverable while VTERM-BUF (captured at drain
time, pinning the delivery to that specific session) is live."
  (if (agent-repl--ws-gui-frontend-p ws)
      (agent-repl--gui-running-p ws)
    (buffer-live-p vterm-buf)))

(defun agent-repl--make-pending-prompt (text &optional origin)
  "Return a pending-prompt entry carrying TEXT and an optional ORIGIN.
A bare string when ORIGIN is nil (the ordinary entry shape, unchanged),
else a plist `(:text TEXT :origin ORIGIN)'.  ORIGIN rides WITH the prompt
so every delivery attempt re-stamps the send (see
`agent-repl--deliver-pending-prompts'); a one-shot ws flag would instead
be dropped by a verify retry, leaving the resent turn untagged."
  (if origin (list :text text :origin origin) text))

(defun agent-repl--pending-prompt-text (entry)
  "Return the prompt text of a pending-prompt ENTRY (a string or a plist)."
  (if (stringp entry) entry (plist-get entry :text)))

(defun agent-repl--pending-prompt-origin (entry)
  "Return the origin of a pending-prompt ENTRY, or nil for a bare string."
  (if (stringp entry) nil (plist-get entry :origin)))

(defun agent-repl--deliver-pending-prompts (pending ws &optional retries)
  "Deliver PENDING prompts to WS if its frontend can still receive them.
Sends the first prompt via `agent-repl--send' with an ON-SETTLE that
schedules `agent-repl--maybe-retry-or-continue' after
`agent-repl-prompt-delivery-verify-seconds'.  That verify step
confirms the agent actually saw the paste (state advanced past `:idle')
before draining the next prompt, and resends the current prompt up
to `agent-repl-prompt-delivery-max-retries' times when the verify
fails — closing the race between `SessionStart' (which flips Emacs
to ready) and the agent's TUI input-area becoming interactive.

RETRIES is the number of resends already performed for the prompt at
the head of PENDING; nil/0 on the first attempt.

A workspace born from generation is never switched to (the no-switch
contract on the sentinel-driven path), so its `:input-buffer' is still
nil at this point — `agent-repl--send' would then skip
`agent-repl--history-push' entirely and the preemptive prompt would
never land in input history.  `agent-repl--ensure-input-buffer' heads
that off by materializing the (unshown) input buffer first, the same
buffer the panel-show path later adopts."
  (agent-repl--log ws "deliver-pending-prompts: ws=%s count=%d retries=%d"
                    ws (length pending) (or retries 0))
  (unless (agent-repl--pending-delivery-alive-p ws nil)
    (error "agent-repl--deliver-pending-prompts: frontend session is gone for ws=%s — %d prompt(s) lost"
           ws (length pending)))
  (when pending
    (agent-repl--ensure-input-buffer ws)
    (let* ((retries (or retries 0))
           (head (car pending)))
      ;; Re-stamp the per-prompt origin on EVERY attempt: the initial send and
      ;; each verify retry both route through here, so a retry re-arms the tag
      ;; a one-shot consumed flag would have dropped.  A bare-string entry arms
      ;; nil, clearing any stale tag so an ordinary prompt is never tagged.
      (agent-repl--ws-put ws :next-send-origin (agent-repl--pending-prompt-origin head))
      (agent-repl--send
       (agent-repl--pending-prompt-text head) ws nil
       (lambda ()
         (run-at-time
          agent-repl-prompt-delivery-verify-seconds nil
          #'agent-repl--maybe-retry-or-continue
          pending ws retries))))))

(defun agent-repl--maybe-retry-or-continue (pending ws retries)
  "Verify the current preemptive prompt was acknowledged; retry or continue.
Called by a timer scheduled in `agent-repl--deliver-pending-prompts'
after the send's `on-settle' fires.  Inspects `:agent-state' on WS
via `agent-repl--prompt-acknowledged-p':

- Acknowledged: drain the next pending prompt (if any) with a fresh
  retry count.
- Not acknowledged AND RETRIES below the cap: resend the same prompt
  (head of PENDING) with RETRIES + 1.
- Not acknowledged AND cap reached: abandon the delivery with a
  user-visible warning and stop — better than looping forever, and
  the prompt is still in input history for the user to resend
  manually.

When the frontend session has died in the meantime (a gui workspace's
daemon binding released), abandons silently."
  (cond
   ((not (agent-repl--pending-delivery-alive-p ws nil))
    (agent-repl--log ws
                      "deliver-verify: frontend session gone for ws=%s — abandoning %d prompt(s)"
                      ws (length pending)))
   ((agent-repl--prompt-acknowledged-p ws)
    (agent-repl--log ws
                      "deliver-verify: ws=%s prompt acknowledged after %d retries — continuing"
                      ws retries)
    (when (cdr pending)
      (agent-repl--deliver-pending-prompts (cdr pending) ws 0)))
   ((< retries agent-repl-prompt-delivery-max-retries)
    (let ((next-retries (1+ retries)))
      (agent-repl--log ws
                        "deliver-verify: ws=%s NOT acknowledged after %.1fs — retry %d/%d"
                        ws agent-repl-prompt-delivery-verify-seconds
                        next-retries agent-repl-prompt-delivery-max-retries)
      (agent-repl--deliver-pending-prompts pending ws next-retries)))
   (t
    (agent-repl--log ws
                      "deliver-verify: ws=%s GIVING UP after %d retries — prompt may be lost"
                      ws retries)
    (agent-repl--warn ws
                      "preemptive prompt for ws=%s not acknowledged after %d retries — the agent may not have seen it"
                      ws retries))))

(defun agent-repl--drain-pending-prompts (ws)
  "Drain queued prompts for workspace WS after the agent becomes ready.
Clears :pending-prompts and schedules them for delivery with a 0.3s delay
so the daemon session has time to settle.  Delivery liveness is judged
by WS's daemon session binding (see
`agent-repl--pending-delivery-alive-p')."
  (let ((pending (agent-repl--ws-get ws :pending-prompts)))
    (when pending
      (agent-repl--log ws "first-ready draining %d pending prompt(s) for ws=%s" (length pending) ws)
      (agent-repl--ws-put ws :pending-prompts nil)
      (run-at-time agent-repl-pending-prompt-deliver-delay nil
                   #'agent-repl--deliver-pending-prompts
                   pending ws))
    pending))

(defun agent-repl--loading-placeholder-visible-p ()
  "Return non-nil if the loading placeholder buffer is displayed in a window."
  (when-let ((ph (get-buffer " *agent-loading*")))
    (get-buffer-window ph)))

(defun agent-repl--show-panels-or-defer (ws)
  "Open panels if WS is the current workspace, otherwise defer until switch.
`agent-repl--on-workspace-switch' checks :pending-show-panels.
Skip if the loading placeholder is still visible — showing panels
here would race the placeholder's teardown and mount the frontend
view against the wrong selected window."
  (if (agent-repl--current-ws-p ws)
      (unless (agent-repl--loading-placeholder-visible-p)
        (agent-repl--log ws "show-panels-or-defer: current ws=%s — showing panels" ws)
        (agent-repl--frontend-dispatch-show ws))
    (agent-repl--log ws "show-panels-or-defer: other ws=%s — deferring" ws)
    (agent-repl--ws-put ws :pending-show-panels t)))

(defun agent-repl--open-panels-after-ready (ws)
  "Open panels for WS after the agent becomes ready.
If there were pending prompts, always show panels (or defer).
Otherwise, only show panels if WS is the current workspace AND its
persisted `:repl-state' is not `:inactive' or `:hidden' — both signal
that the user wants panels closed (hide-mode survives restart: when
`--initialize-ws-env' hydrated either value from the saved file, we
honor it here by skipping the panel-open call).

WS's panels may already have been shown by the workspace-switch's own
`:pending-show-panels' drain (`agent-repl--drain-pending-show-panels'
in panels.el), so this after-ready call is often a SECOND show attempt
rather than the first — and re-running the show path while panels are
already visible is not safe (it can resolve both windows onto the
input buffer and die on its window-dedication mid-layout, leaving a
broken input-only frame), so an already-visible current-workspace
layout is left untouched in both branches."
  (if (agent-repl--drain-pending-prompts ws)
      (if (and (agent-repl--current-ws-p ws)
               (agent-repl--panels-visible-p))
          (agent-repl--log ws "open-panels-after-ready: pending drained, panels already visible ws=%s — no re-show" ws)
        (agent-repl--log ws "open-panels-after-ready: had pending prompts ws=%s — show or defer" ws)
        (agent-repl--show-panels-or-defer ws))
    (agent-repl--log ws "first-ready no pending prompts for ws=%s" ws)
    (cond
     ((memq (agent-repl--ws-repl-state ws) '(:inactive :hidden))
      (agent-repl--log ws "open-panels-after-ready: persisted %s ws=%s — skipping panel open"
                        (agent-repl--ws-repl-state ws) ws))
     ((and (agent-repl--current-ws-p ws)
           (agent-repl--panels-visible-p))
      (agent-repl--log ws "open-panels-after-ready: panels already visible ws=%s — no re-show" ws))
     ((and (agent-repl--current-ws-p ws)
           (not (agent-repl--loading-placeholder-visible-p)))
      (agent-repl--log ws "open-panels-after-ready: no pending + current ws=%s — showing panels" ws)
      (agent-repl--frontend-dispatch-show ws))
     (t
      (agent-repl--log ws "open-panels-after-ready: no pending + other ws=%s — no-op" ws)))))

;; Readiness is handled entirely by the session_start hook via sentinel.el.
;; The hook fires when Claude Code initializes, delivering session-id and
;; triggering agent-repl--on-session-start-event, which sets ready state,
;; drains pending prompts, and opens panels.  No polling is needed.

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
    (unless ws (error "agent-repl--agent-running-p: no workspace specified and no current workspace"))
    (funcall (agent-repl-frontend-running-p-fn (agent-repl--ws-frontend ws)) ws)))

;;;; Alt-account config-dir hook provisioning
;;
;; The per-account launch logic (`agent-repl--compute-config-dir') can
;; select CLAUDE_CONFIG_DIRs OTHER than the default ~/.claude — at minimum
;; `agent-repl-multi-repo-config-dir' (~/.claude-chesscom).  The
;; agent-repl readiness handshake depends on the SessionStart hook (and the
;; rest of the managed hooks) being registered in the settings.json of
;; whichever account launches the workspace, so those alt dirs must carry
;; the SAME managed-hook registrations as ~/.claude.  install.sh owns
;; ~/.claude; `agent-repl--provision-config-dirs' (install.el) writes the
;; registrations into every alt dir the account logic can select, deriving
;; the set from the defcustoms above.
;;
;; Runs at load, guarded exactly like `agent-repl--maybe-install-hooks':
;; no-op in a `noninteractive' (batch/ERT) session or inside the sandbox,
;; and a failure is logged (never swallowed silently — the underlying
;; `agent-repl--provision-config-dirs' still signals loudly when called
;; directly, e.g. from tests).  Deferred to this point (rather than
;; install.el's own load) because the config-dir defcustoms are defined
;; above in THIS file, which loads after install.el.
(when (and (not noninteractive)
           (fboundp 'agent-repl--provision-config-dirs)
           (not (agent-repl--in-sandbox-p)))
  (condition-case err
      (agent-repl--provision-config-dirs)
    (error
     (agent-repl--log nil "provision-config-dirs failed: %S" err))))
