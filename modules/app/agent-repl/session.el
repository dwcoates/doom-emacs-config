;;; session.el --- session lifecycle management -*- lexical-binding: t; -*-

;;; Code:

(declare-function agent-repl--ws-dir-owner "agent-repl-workspace" (dir &optional except))
(declare-function agent-repl--mark-start-failed "agent-repl-worktree" (ws err))

(defconst agent-repl--start-failure-marker "AGENT_REPL_START_FAILURE:"
  "Marker a start command (e.g. `claude-sandbox') prints to its vterm when it
cannot launch — Docker daemon down, image missing, etc.  The start command
runs inside a vterm whose exit code agent-repl cannot observe, so
`agent-repl--detect-start-failure' scrapes this line and the ready-timer
routes the failure to `agent-repl--mark-start-failed'.  The text after the
marker on its line is surfaced to the user as the failure reason.")

(defconst agent-repl--start-failure-patterns
  '(("Cannot connect to the Docker daemon" . "Docker daemon is not running — start Docker Desktop, then retry"))
  "Alist of (SUBSTRING . REASON) for known fatal start-command output that
lacks an explicit `agent-repl--start-failure-marker' line — e.g. Docker's
native daemon-down error.  Lets agent-repl surface the failure even when the
start command itself has not been taught to print the marker.")

;;;; Session readiness

(defvar-local agent-repl--ready nil
  "Non-nil once Claude Code has set its terminal title, indicating startup is complete.")

;;;; Sandbox configuration

(defcustom agent-repl-docker-image ""
  "Fallback Docker image for sandboxed worktree workspaces with no .claude/sandbox/image.
Prefer per-repo .claude/sandbox/image files over this global setting.
If empty (the default), worktrees without a .claude/sandbox/image run Claude directly."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-managed-project-pattern "ChessCom"
  "Pattern matched against the project directory to determine permission mode.
Projects whose expanded path contains this pattern use `agent-repl-managed-permission-flag';
all others use `agent-repl-personal-permission-flag'."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-managed-permission-flag "--permission-mode auto"
  "Permission flag for managed projects matching `agent-repl-managed-project-pattern'."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-personal-permission-flag "--permission-mode auto"
  "Permission flag for personal projects not matching `agent-repl-managed-project-pattern'.
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

(defcustom agent-repl-startup-prefix "clear && "
  "Shell command prefix prepended before the Claude command at startup."
  :type 'string
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

(defcustom agent-repl-notify-debounce-seconds 2.0
  "Minimum seconds between desktop notifications for the same workspace."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-notify-delay 0.1
  "Seconds to delay before sending a desktop notification."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-pending-prompt-deliver-delay 0.3
  "Seconds to wait before delivering pending prompts after Claude becomes ready."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-prompt-delivery-verify-seconds 4.0
  "Seconds to wait after a preemptive prompt before verifying delivery.
A preemptive prompt is considered acknowledged when `:agent-state'
transitions away from `:idle' (the `UserPromptSubmit' hook fires and
`--on-prompt-submit-event' flips state to `:thinking').  When the state
is still `:idle' after this window, the bracketed paste is assumed to
have raced Claude's TUI input-area paint and is resent — see
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

(defcustom agent-repl-ready-timeout-seconds 30.0
  "Maximum seconds to wait for Claude to signal readiness before giving up."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-ready-poll-interval 0.5
  "Seconds between readiness-poll timer ticks."
  :type 'number
  :group 'agent-repl)

(defun agent-repl--docker-image-exists-p (image)
  "Return non-nil if IMAGE exists in the local Docker image store.
Routes through `agent-repl--docker-exit-code' (the registered
external-boundary wrapper) so the call is mocked by the test-time
runtime guards."
  (let ((result (= 0 (agent-repl--docker-exit-code
                      "image" "inspect" "--format" "." image))))
    (agent-repl--log nil "docker-image-exists-p: image=%s exists=%s" image (if result "yes" "no"))
    result))

(defun agent-repl--find-sandbox-script (git-root)
  "Return the path to the sandbox launcher script for GIT-ROOT, or nil.
Checks for `claude-sandbox' on PATH first, then falls back to
`.agents-sandbox/sandbox' inside the repository."
  (let ((result (or (when-let ((p (executable-find "claude-sandbox")))
                      (agent-repl--log nil "find-sandbox-script: found claude-sandbox on PATH at %s" p)
                      p)
                    (let ((f (expand-file-name ".agents-sandbox/sandbox" git-root)))
                      (if (file-executable-p f)
                          (progn
                            (agent-repl--log nil "find-sandbox-script: found .agents-sandbox/sandbox at %s" f)
                            f)
                        nil)))))
    (unless result
      (agent-repl--log nil "find-sandbox-script: no sandbox script found for git-root=%s" git-root))
    result))

(defun agent-repl--query-sandbox-image (script)
  "Return the Docker image name reported by sandbox SCRIPT, or nil on failure.
Runs SCRIPT with --image-name and trims the output."
  (let ((image (string-trim
                (with-output-to-string
                  (with-current-buffer standard-output
                    (call-process script nil t nil "--image-name"))))))
    (if (string-empty-p image)
        (progn
          (agent-repl--log nil "query-sandbox-image: script=%s returned empty image" script)
          nil)
      (agent-repl--log nil "query-sandbox-image: script=%s image=%s" script image)
      image)))

(defun agent-repl--find-install-script (git-root)
  "Return the path to the sandbox install script in GIT-ROOT, or nil."
  (let ((f (expand-file-name ".agents-sandbox/install-claude.sh" git-root)))
    (if (file-executable-p f)
        (progn
          (agent-repl--log nil "find-install-script: found %s" f)
          f)
      (agent-repl--log nil "find-install-script: no install script in git-root=%s" git-root)
      nil)))

(defun agent-repl--resolve-sandbox-config (git-root)
  "Return a plist (:image IMAGE :script SCRIPT) for a worktree at GIT-ROOT.
Detects sandbox support by looking for the `claude-sandbox' launcher on PATH
or `.agents-sandbox/sandbox' in the repo.  Queries the launcher's --image-name
flag to determine the Docker image.
Returns nil if no sandbox launcher is found.
Returns (:needs-build t :install-script PATH) if the image is not built yet."
  (let ((script (agent-repl--find-sandbox-script git-root)))
    (if (null script)
        (progn
          (agent-repl--log nil "resolve-sandbox-config: no-launcher for git-root=%s" git-root)
          nil)
      (if-let ((image (agent-repl--query-sandbox-image script)))
          (if (agent-repl--docker-image-exists-p image)
              (progn
                (agent-repl--log nil "resolve-sandbox-config: success image=%s script=%s" image script)
                (list :image image :script script))
            (progn
              (agent-repl--log nil "resolve-sandbox-config: needs-build image=%s" image)
              (list :needs-build t
                    :image image
                    :install-script (agent-repl--find-install-script git-root))))
        (agent-repl--log nil "resolve-sandbox-config: empty-image from script=%s in git-root=%s" script git-root)
        nil))))

;;;; Workspace environment initialization

(defun agent-repl--apply-display-state (ws saved)
  "Apply persisted display/metadata state from SAVED plist onto workspace WS.
SAVED is a parsed state-file plist (or nil).  Hydrates the
non-env-struct keys that drive the tabline badge and drawer glyphs:
`:priority', `:source-ws-dir', `:model', `:last-prompt-time',
`:repl-state', `:saved-tab-index', `:fork-session-id',
`:last-prompt-summary', `:last-prompt-summary-at', `:worktree-p', and
the `:merge-completed' / `:merge-failed' / `:merge-completed-at'
bookkeeping.

Shared by `agent-repl--initialize-ws-env' (the Claude-start path) and
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
  ;; before any state-save happened).  `agent-repl--build-start-cmd'
  ;; reads `:model' to pass `--model' when booting, so restoring it here
  ;; re-launches the session under the same model.
  (agent-repl--ws-put ws :model
                       (or (and saved (plist-get saved :model))
                           (agent-repl--ws-get ws :model)))
  ;; Last-prompt-time: prefer saved value, fall back to whatever is
  ;; already in the plist.  Used by the drawer's detail view to show
  ;; "duration since last user message"; survives Emacs restarts so
  ;; the duration reflects real elapsed wall-clock, not session age.
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
  ;; Fork session ID: a worktree-fork ws whose claude session was never
  ;; actually started before quit needs the fork pointer to survive so
  ;; the next `--initialize-agent' can launch with `--resume FORK
  ;; --fork-session'.  Cleared by `--initialize-agent' once consumed.
  (when-let ((fork (and saved (plist-get saved :fork-session-id))))
    (agent-repl--ws-put ws :fork-session-id fork))
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
  ;; Worktree-flag + merge-completed: survive restart so the drawer's
  ;; MERGED bucket reappears and `--finish-workspace' can still remove
  ;; the worktree when the user presses `x' on a post-restart MERGED
  ;; entry.  `:merge-completed-at' rides alongside for display only.
  (when (and saved (eq (plist-get saved :worktree-p) t))
    (agent-repl--ws-put ws :worktree-p t))
  (when (and saved (eq (plist-get saved :merge-completed) t))
    (agent-repl--ws-put ws :merge-completed t)
    ;; Restore the merged repl-state alongside `:merge-completed'
    ;; so the badge re-appears post-restart instead of falling
    ;; through to `:dead' (or whatever the poll resolves).  A
    ;; persisted `:merge-failed t' wins over the success path: the
    ;; workspace stays in MERGED but surfaces the ❌ badge via
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
display/metadata keys (`:priority' and the drawer-badge / merge /
last-prompt fields) to WS via `agent-repl--apply-display-state', so
the tabline badge and drawer glyphs render the moment a workspace is
switched to (`SPC p p') or created — without waiting for
`agent-repl--initialize-ws-env', which only runs when Claude starts.

No-op when WS or PROJECT-ROOT is nil, or when WS has already been
env-initialized (`:active-env' set): an env-initialized workspace
already carries its display state in memory, so re-reading the state
file would be a redundant disk read and could clobber live in-memory
values with staler on-disk ones.  Also a no-op when the state file is
missing or malformed."
  (when (and ws project-root
             (null (agent-repl--ws-get ws :active-env)))
    (let* ((state-file (agent-repl--state-file-for-read project-root))
           (saved (and state-file
                       (condition-case err
                           (agent-repl--read-sexp-file-if-exists state-file)
                         (error
                          (agent-repl--log ws "load-display-state: read error file=%s err=%S"
                                            state-file err)
                          nil)))))
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

Must not be called while WS has a live Claude session — instantiation
structs would be clobbered with any session-id mutations since the last
state-save.  Callers already guard on `agent-repl--agent-running-p'."
  (let* ((root-candidate (or project-dir-hint
                             (agent-repl--ws-get ws :project-dir)
                             (agent-repl--git-root default-directory)))
         (root (and root-candidate (agent-repl--path-canonical root-candidate)))
         (state-file (and root (agent-repl--state-file-for-read root)))
         (saved (and state-file
                     (file-exists-p state-file)
                     (condition-case err
                         (agent-repl--read-sexp-file state-file)
                       (error
                        (agent-repl--log ws "initialize-ws-env: state file read error file=%s err=%S"
                                          state-file err)
                        nil)))))
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
    ;; switch shows the same badges before Claude ever starts.
    (agent-repl--apply-display-state ws saved)
    (dolist (key agent-repl--environment-keys)
      (agent-repl--ws-put ws key
                           (agent-repl--make-instantiation-from-plist
                            (and saved (plist-get saved key)))))
    (agent-repl--validate-ws-env ws)
    (unless saved
      (agent-repl--log ws "initialize-ws-env: no state file, writing initial state ws=%s root=%s" ws root)
      (agent-repl--state-save ws))))

(defun agent-repl--prompt-sandbox-build (sandbox-config)
  "Prompt the user to build a missing sandbox image from SANDBOX-CONFIG.
Signals `user-error' unconditionally -- either after kicking off the build
or telling the user to do it manually."
  (let ((image (plist-get sandbox-config :image))
        (install-script (plist-get sandbox-config :install-script)))
    (agent-repl--log nil "prompt-sandbox-build: image=%s install-script=%s" image install-script)
    (if install-script
        (when (y-or-n-p (format "Sandbox image '%s' not built. Run install.sh now? " image))
          (compile (format "bash %s" install-script))
          (user-error "Run 'SPC o c' again once the build completes"))
      (user-error "Sandbox image '%s' not built — run .agents-sandbox/install-claude.sh manually" image))))

(defun agent-repl--get-sandbox-image (ws)
  "Return the sandbox Docker image config plist for workspace WS.
Returns a sandbox-config plist from `agent-repl--resolve-sandbox-config',
or nil if sandboxing is not applicable.  Signals `user-error' if the image
needs building, optionally kicking off the build first."
  (let* ((worktree-p (agent-repl--ws-get ws :worktree-p))
         (active-env (agent-repl--ws-get ws :active-env))
         (project-dir (agent-repl--ws-get ws :project-dir))
         (sandbox-config (when (and worktree-p (eq active-env :sandbox))
                           (agent-repl--resolve-sandbox-config project-dir))))
    (agent-repl--log ws "get-sandbox-image: ws=%s worktree-p=%s env=%s config=%s"
                      ws (if worktree-p "yes" "no") active-env
                      (if sandbox-config "found" "nil"))
    (when (plist-get sandbox-config :needs-build)
      (agent-repl--prompt-sandbox-build sandbox-config))
    sandbox-config))

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
  (let* ((effective-model (or model agent-repl-interactive-model))
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

(defun agent-repl--compute-perm-flag (sandboxed-p project-dir &optional model)
  "Return the permission flag string for the Claude CLI, or nil.
SANDBOXED-P means Docker handles permissions.  Otherwise, PROJECT-DIR
determines the base flag: ChessCom repos use `agent-repl-managed-permission-flag',
all others use `agent-repl-personal-permission-flag' (both default to
--permission-mode auto).  MODEL is the effective interactive model alias;
`--permission-mode auto' is only allowed when MODEL is not Haiku (see
`agent-repl--model-haiku-p'), so a resolved base flag of
`--permission-mode auto' is downgraded to `--dangerously-skip-permissions'
whenever MODEL denotes Haiku."
  (if sandboxed-p
      (progn
        (agent-repl--log nil "compute-perm-flag: sandboxed — no perm flag")
        nil)
    (unless project-dir
      (error "agent-repl--compute-perm-flag: project-dir is nil — cannot determine permission mode"))
    (let* ((managed (string-match-p agent-repl-managed-project-pattern (expand-file-name project-dir)))
           (base (if managed
                     agent-repl-managed-permission-flag
                   agent-repl-personal-permission-flag))
           (flag (if (and (agent-repl--model-haiku-p model)
                          (equal base "--permission-mode auto"))
                     "--dangerously-skip-permissions"
                   base)))
      (agent-repl--log nil "compute-perm-flag: branch=%s model=%s flag=%s"
                        (if managed "managed" "personal") model flag)
      flag)))

(defun agent-repl--compute-config-dir (project-dir)
  "Return the CLAUDE_CONFIG_DIR to use for PROJECT-DIR, or nil.
When PROJECT-DIR lies under the directory named by the
`agent-repl-multi-repo-root-env' environment variable, returns the
expanded `agent-repl-multi-repo-config-dir' (the dodge@chess.com
account).  Otherwise returns the expanded `agent-repl-default-config-dir',
or nil when that is nil so the CLI falls back to its default ~/.claude
(the dodge.w.coates@gmail.com account).  Signals an error when
PROJECT-DIR is nil, since account selection cannot be resolved without
it."
  (unless project-dir
    (error "agent-repl--compute-config-dir: project-dir is nil — cannot determine account"))
  (let* ((root (getenv agent-repl-multi-repo-root-env))
         (under-multi-repo
          (and root
               (> (length root) 0)
               (string-prefix-p (file-name-as-directory (expand-file-name root))
                                (file-name-as-directory (expand-file-name project-dir)))))
         (dir (if under-multi-repo
                  agent-repl-multi-repo-config-dir
                agent-repl-default-config-dir)))
    (agent-repl--log nil "compute-config-dir: project-dir=%s root=%s branch=%s dir=%s"
                      project-dir root
                      (if under-multi-repo "multi-repo" "default") dir)
    (and dir (expand-file-name dir))))

(defun agent-repl--assemble-cmd (claude-flags &optional config-dir)
  "Assemble the final `claude' shell command string.
CLAUDE-FLAGS is the pre-built flags string.  CONFIG-DIR, when non-nil,
is prepended as a `CLAUDE_CONFIG_DIR=...' environment assignment so the
launched Claude uses that account's credentials.

agent-repl ALWAYS launches plain `claude' — it never shells out to
`claude-sandbox'.  There is deliberately no sandbox branch here."
  (let* ((base (concat "claude " claude-flags))
         (env-prefix (if config-dir
                         (format "CLAUDE_CONFIG_DIR=%s " (shell-quote-argument config-dir))
                       ""))
         (cmd (string-trim (concat env-prefix base))))
    (agent-repl--log nil "assemble-cmd: cmd=%s" cmd)
    cmd))

(defun agent-repl--build-start-cmd (ws)
  "Build the shell command string to start Claude for workspace WS.
Returns a plist (:cmd CMD :sandboxed-p BOOL :docker-image IMAGE
:session-id ID :fork-session-id ID :worktree-p BOOL :active-env ENV :inst INST)
with everything the caller needs for logging and mode-line setup."
  (agent-repl--log ws "build-start-cmd: ws=%s" ws)
  (let* ((inst (agent-repl--active-inst ws))
         ;; FIXME we have to ensure that every time we start claude process for any reason, we have sentinel watching for a session-id update. we can't eb always blindly reading session-id from a file, because chance
         (session-id (agent-repl-instantiation-session-id inst))
         (worktree-p (agent-repl--ws-get ws :worktree-p))
         (project-dir (agent-repl--ws-get ws :project-dir))
         (active-env (agent-repl--ws-get ws :active-env))
         (fork-session-id (agent-repl--ws-get ws :fork-session-id))
         (model (agent-repl--ws-get ws :model))
         ;; Effective model drives the permission flag: Haiku sessions can't
         ;; use `--permission-mode auto', so resolve the fallback here too.
         (effective-model (or model agent-repl-interactive-model))
         ;; agent-repl always launches plain `claude', never claude-sandbox,
         ;; so no Docker image is resolved and the command is never sandboxed.
         (perm-flag (agent-repl--compute-perm-flag nil project-dir effective-model))
         (config-dir (agent-repl--compute-config-dir project-dir))
         (claude-flags (agent-repl--compute-claude-flags session-id fork-session-id perm-flag model))
         (cmd (agent-repl--assemble-cmd claude-flags config-dir)))
    (list :cmd cmd
          :sandboxed-p nil
          :docker-image nil
          :session-id session-id
          :fork-session-id fork-session-id
          :worktree-p worktree-p
          :active-env active-env
          :inst inst)))

;;;; Session startup

(defun agent-repl--merge-target-name (ws)
  "Return the basename of the workspace `SPC TAB M' would merge WS into.
Mirrors the resolution logic in `agent-repl--workspace-merge-into-source':
prefers WS's `:source-ws-dir' as the parent, then asks
`agent-repl--resolve-merge-into-source-target' whether to redirect to
the master worktree (when the parent's branch is already on master via
patch-id equivalence).

Returns nil when WS has no `:project-dir' (unknown workspace), or when
the resolved target equals WS's own dir (nothing to merge into).  This
is a snapshot at call time — the `git cherry' check runs once when the
mode-line is constructed, not on every redisplay."
  (when-let* ((source-dir (agent-repl--ws-get ws :project-dir)))
    (let* ((recorded (agent-repl--ws-get ws :source-ws-dir))
           (parent-dir (or (and recorded (file-directory-p recorded) recorded)
                           (agent-repl--master-worktree-path source-dir)))
           (master-dir (agent-repl--master-worktree-path source-dir))
           (target-dir (agent-repl--resolve-merge-into-source-target parent-dir master-dir)))
      (when (and target-dir
                 (not (string= (agent-repl--path-canonical target-dir)
                               (agent-repl--path-canonical source-dir))))
        (file-name-nondirectory (directory-file-name target-dir))))))

(defun agent-repl--parent-label (parent-name merge-name)
  "Return (GREEN-STR YELLOW-STR) for the parent mode-line label, or nil.
PARENT-NAME is the basename of `:source-ws-dir' (the recorded parent
worktree, or nil if none).  MERGE-NAME is the basename of the
workspace `SPC TAB M' would target (or nil if none).

GREEN-STR is the always-green leading part (\" <parent>\" or empty when
no parent).  YELLOW-STR is the parens-wrapped merge target with a
leading space (\" (<merge>)\"), or nil when the merge target is absent
or matches the parent (parens omitted to avoid the redundant
\" foo (foo)\" form).  Returns nil overall when both inputs are nil —
caller should render an empty segment.

The split is so callers can propertize each part with a different face
\(green for the parent, yellow for the (...) suffix) without having to
parse a single composed string.  The label has no textual prefix; the
green/yellow coloring is the sole signal that the segment denotes the
parent/merge-target relationship."
  (cond
   ((and (null parent-name) (null merge-name)) nil)
   ((null parent-name) (list "" (format " (%s)" merge-name)))
   ((or (null merge-name) (string= parent-name merge-name))
    (list (format " %s" parent-name) nil))
   (t (list (format " %s" parent-name) (format " (%s)" merge-name)))))

(defun agent-repl--workspace-mode-line (ws)
  "Return a mode-line format list for workspace WS's vterm.
Segments, in order:
  1. Composed parent label: green ` <parent>' followed by a yellow
     ` (<merge-target>)' suffix when `SPC TAB M' would redirect to a
     different workspace than the recorded parent (typically master).
     When the merge target equals the parent, the yellow suffix is
     omitted.  Empty when WS has neither a recorded parent nor a
     resolvable merge target.  The green/yellow coloring is the sole
     signal that the segment denotes the parent/merge-target
     relationship — there is no textual prefix.
  2. `:eval' segment that renders the Claude model serving the active
     session (see `agent-repl--model-segment'), reading from the
     project's session jsonl under `~/.claude/projects/'.
  3. `:eval' segment that renders the used context token count for the
     active session (see `agent-repl--context-segment'), reading from
     the project's session jsonl under `~/.claude/projects/'.

The parent segment is computed once when the vterm is initialized; it
is not reactive to later state changes."
  (let* ((source-dir (agent-repl--ws-get ws :source-ws-dir))
         (parent-name (when (and source-dir (not (string-empty-p source-dir)))
                        (file-name-nondirectory (directory-file-name source-dir))))
         (merge-name (agent-repl--merge-target-name ws))
         (parts (agent-repl--parent-label parent-name merge-name))
         (green (car parts))
         (yellow (cadr parts)))
    (list (cond
           ((null parts) "")
           (yellow (concat (propertize green 'face '(:foreground "green" :weight bold))
                           (propertize yellow 'face '(:foreground "yellow" :weight bold))))
           (t (propertize green 'face '(:foreground "green" :weight bold))))
          '(:eval (agent-repl--model-segment))
          '(:eval (agent-repl--context-segment)))))

(defun agent-repl--log-session-start (ws start-info)
  "Log session startup details for workspace WS from START-INFO plist."
  (let ((cmd             (plist-get start-info :cmd))
        (session-id      (plist-get start-info :session-id))
        (fork-session-id (plist-get start-info :fork-session-id))
        (worktree-str    (if (plist-get start-info :worktree-p) "yes" "no"))
        (active-env      (plist-get start-info :active-env)))
    (agent-repl--log ws "log-session-start ws=%s session-id=%s fork-session-id=%s worktree=%s env=%s cmd=%s dir=%s"
                      ws session-id fork-session-id worktree-str active-env cmd
                      (agent-repl--ws-get ws :project-dir))))

;;;; Session completion handling

(defun agent-repl--maybe-notify-finished (ws)
  "Send a desktop notification that Claude finished in WS, if frame is unfocused.
Debounces per-workspace to avoid duplicate notifications when both the hook
and title-change paths fire for the same turn completion."
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

(defun agent-repl--refresh-vterm-after-finish (vterm-buf)
  "Refresh display and scroll position for VTERM-BUF if it is still live."
  (let ((ws (agent-repl--buffer-owner vterm-buf)))
    (agent-repl--log ws "refresh-vterm-after-finish: buf=%s" (buffer-name vterm-buf))
    (if (buffer-live-p vterm-buf)
        (progn
          (with-current-buffer vterm-buf
            (agent-repl--do-refresh)
            (agent-repl--update-hide-overlay))
          (agent-repl--fix-vterm-scroll vterm-buf))
      (agent-repl--log ws "refresh-vterm-after-finish: buffer is dead buf=%s" (buffer-name vterm-buf)))))

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
  "Handle Claude finishing in WS.
Errors hard if WS is not registered in `agent-repl--workspaces' — a
stop event arriving for an unknown workspace indicates a race (e.g.
sentinel firing after kill cleared state) that we surface rather than
silently absorb.  Otherwise: marks agent-state as :done, refreshes the
vterm display if the buffer is still live, refreshes any open
magit-status buffer for the workspace's repo, notifies the user if the
frame is unfocused, emits a finished-in-workspace message when the
current workspace is different, and drains any deferred-prompt queue
\(see `agent-repl--drain-deferred-prompts')."
  (unless (gethash ws agent-repl--workspaces)
    (error "agent-repl--handle-agent-finished: ws=%S not registered in agent-repl--workspaces" ws))
  (let ((vterm-buf (agent-repl--ws-get ws :vterm-buffer)))
    (agent-repl--log ws "handle-agent-finished ws=%s" ws)
    (agent-repl--mark-agent-done ws)
    (when vterm-buf
      (agent-repl--refresh-vterm-after-finish vterm-buf))
    (agent-repl--refresh-magit-status ws)
    (agent-repl--maybe-notify-finished ws)
    (unless (agent-repl--current-ws-p ws)
      (message "Claude finished in workspace: %s" ws))
    (agent-repl--drain-deferred-prompts ws)))

;;;; Deferred prompt queue
;;
;; Distinct from `:pending-prompts' (the at-startup queue drained when
;; the session_start hook arrives — see `--drain-pending-prompts').
;; `:deferred-prompts' is a runtime FIFO seeded by the leader-key
;; command `agent-repl-queue-deferred-prompt' (bound to `SPC j RET'):
;; the user keeps typing prompts while Claude is busy, and each one
;; is held until Claude reaches `:done' / `:idle', at which point the
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

Sends exactly one prompt per call.  Sending re-enters Claude into
`:thinking' via the `UserPromptSubmit' hook; the next `handle-claude-
finished' for this workspace will re-trigger the drain and pop the
next queued prompt.  This keeps the deferred queue strictly serialized
with Claude's turn boundaries — the whole point of using the queue
over Claude's native paste-while-thinking buffering."
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
  (agent-repl--log ws "set-session-id: ws=%s id=%s" ws id)
  (setf (agent-repl-instantiation-session-id (agent-repl--active-inst ws)) id)
  (agent-repl--state-save ws))

;; Session ID capture is handled exclusively by Claude Code hooks.
;; Every hook event (session_start, stop, prompt_submit, permission_prompt)
;; delivers session_id in the sentinel file, and
;; agent-repl--update-session-id-from-sentinel (in sentinel.el) sets it
;; on the workspace's active instantiation.  No file scanning needed.

;;;; Readiness and pending prompt handling

(defun agent-repl--prompt-acknowledged-p (ws)
  "Return non-nil when WS's `:agent-state' indicates Claude received a prompt.
Acknowledged states are `:thinking' (the `UserPromptSubmit' hook flipped
state via `--on-prompt-submit-event'), `:permission' (Claude paused to
ask for permission), or `:done' (a fast turn already finished).
Returns nil for `:idle' / `:init' / nil — i.e. when the prompt does
not appear to have reached Claude."
  (memq (agent-repl--ws-agent-state ws)
        '(:thinking :permission :done)))

(defun agent-repl--deliver-pending-prompts (vterm-buf pending ws &optional retries)
  "Deliver PENDING prompts to WS if VTERM-BUF is still live.
Sends the first prompt via `agent-repl--send' with an ON-SETTLE that
schedules `agent-repl--maybe-retry-or-continue' after
`agent-repl-prompt-delivery-verify-seconds'.  That verify step
confirms Claude actually saw the paste (state advanced past `:idle')
before draining the next prompt, and resends the current prompt up
to `agent-repl-prompt-delivery-max-retries' times when the verify
fails — closing the race between `SessionStart' (which flips Emacs
to ready) and Claude's TUI input-area becoming interactive.

RETRIES is the number of resends already performed for the prompt at
the head of PENDING; nil/0 on the first attempt."
  (agent-repl--log ws "deliver-pending-prompts: ws=%s count=%d retries=%d"
                    ws (length pending) (or retries 0))
  (unless (buffer-live-p vterm-buf)
    (error "agent-repl--deliver-pending-prompts: vterm buffer is dead for ws=%s — %d prompt(s) lost"
           ws (length pending)))
  (when pending
    (let ((retries (or retries 0)))
      (agent-repl--send
       (car pending) ws nil
       (lambda ()
         (run-at-time
          agent-repl-prompt-delivery-verify-seconds nil
          #'agent-repl--maybe-retry-or-continue
          vterm-buf pending ws retries))))))

(defun agent-repl--maybe-retry-or-continue (vterm-buf pending ws retries)
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

When the vterm buffer has died in the meantime, abandons silently."
  (cond
   ((not (buffer-live-p vterm-buf))
    (agent-repl--log ws
                      "deliver-verify: vterm dead for ws=%s — abandoning %d prompt(s)"
                      ws (length pending)))
   ((agent-repl--prompt-acknowledged-p ws)
    (agent-repl--log ws
                      "deliver-verify: ws=%s prompt acknowledged after %d retries — continuing"
                      ws retries)
    (when (cdr pending)
      (agent-repl--deliver-pending-prompts vterm-buf (cdr pending) ws 0)))
   ((< retries agent-repl-prompt-delivery-max-retries)
    (let ((next-retries (1+ retries)))
      (agent-repl--log ws
                        "deliver-verify: ws=%s NOT acknowledged after %.1fs — retry %d/%d"
                        ws agent-repl-prompt-delivery-verify-seconds
                        next-retries agent-repl-prompt-delivery-max-retries)
      (agent-repl--deliver-pending-prompts vterm-buf pending ws next-retries)))
   (t
    (agent-repl--log ws
                      "deliver-verify: ws=%s GIVING UP after %d retries — prompt may be lost"
                      ws retries)
    (message "[agent-repl] WARNING: preemptive prompt for ws=%s not acknowledged after %d retries — Claude may not have seen it"
             ws retries))))

(defun agent-repl--drain-pending-prompts (ws)
  "Drain queued prompts for workspace WS after Claude becomes ready.
Clears :pending-prompts and schedules them for delivery with a 0.3s delay
so the terminal has time to settle."
  (let ((pending (agent-repl--ws-get ws :pending-prompts)))
    (when pending
      (agent-repl--log ws "first-ready draining %d pending prompt(s) for ws=%s" (length pending) ws)
      (agent-repl--ws-put ws :pending-prompts nil)
      (let ((vterm-buf (agent-repl--ws-get ws :vterm-buffer)))
        (run-at-time agent-repl-pending-prompt-deliver-delay nil
                     #'agent-repl--deliver-pending-prompts
                     vterm-buf pending ws)))
    pending))

(defun agent-repl--loading-placeholder-visible-p ()
  "Return non-nil if the loading placeholder buffer is displayed in a window."
  (when-let ((ph (get-buffer " *agent-loading*")))
    (get-buffer-window ph)))

(defun agent-repl--show-panels-or-defer (ws)
  "Open panels if WS is the current workspace, otherwise defer until switch.
`agent-repl--on-workspace-switch' checks :pending-show-panels.
Skip if the loading placeholder is still visible — showing panels
here would trigger `--show-existing-panels' with the wrong selected
window."
  (if (agent-repl--current-ws-p ws)
      (unless (agent-repl--loading-placeholder-visible-p)
        (agent-repl--log ws "show-panels-or-defer: current ws=%s — showing panels" ws)
        (agent-repl--show-hidden-panels))
    (agent-repl--log ws "show-panels-or-defer: other ws=%s — deferring" ws)
    (agent-repl--ws-put ws :pending-show-panels t)))

(defun agent-repl--open-panels-after-ready (ws)
  "Open panels for WS after Claude becomes ready.
If there were pending prompts, always show panels (or defer).
Otherwise, only show panels if WS is the current workspace AND its
persisted `:repl-state' is not `:inactive' or `:hidden' — both signal
that the user wants panels closed (hide-mode survives restart: when
`--initialize-ws-env' hydrated either value from the saved file, we
honor it here by skipping the panel-open call)."
  (if (agent-repl--drain-pending-prompts ws)
      (progn
        (agent-repl--log ws "open-panels-after-ready: had pending prompts ws=%s — show or defer" ws)
        (agent-repl--show-panels-or-defer ws))
    (agent-repl--log ws "first-ready no pending prompts for ws=%s" ws)
    (cond
     ((memq (agent-repl--ws-repl-state ws) '(:inactive :hidden))
      (agent-repl--log ws "open-panels-after-ready: persisted %s ws=%s — skipping panel open"
                        (agent-repl--ws-repl-state ws) ws))
     ((and (agent-repl--current-ws-p ws)
           (not (agent-repl--loading-placeholder-visible-p)))
      (agent-repl--log ws "open-panels-after-ready: no pending + current ws=%s — showing panels" ws)
      (agent-repl--show-hidden-panels))
     (t
      (agent-repl--log ws "open-panels-after-ready: no pending + other ws=%s — no-op" ws)))))

;; Readiness is handled by the session_start hook via sentinel.el.
;; The hook fires when Claude Code initializes, delivering session-id and
;; triggering agent-repl--on-session-start-event which sets ready state,
;; drains pending prompts, and opens panels.  No vterm title-change advice
;; is needed.

;;;; Process state predicates

(defun agent-repl--vterm-process-alive-p (ws)
  "Return non-nil if WS has a live vterm buffer with an active process."
  (let* ((buf (agent-repl--ws-get ws :vterm-buffer))
         (result (and buf (buffer-live-p buf) (get-buffer-process buf))))
    (agent-repl--log-verbose ws "vterm-process-alive-p: ws=%s alive=%s" ws (if result "yes" "no"))
    result))

(defun agent-repl--agent-running-p (&optional ws)
  "Return t if Claude vterm buffer for WS exists with a live process.
WS defaults to the current workspace name.  Signals an error if no
workspace can be determined."
  (let ((ws (or ws (agent-repl--ws-current-name))))
    (unless ws (error "agent-repl--agent-running-p: no workspace specified and no current workspace"))
    (agent-repl--vterm-process-alive-p ws)))

(defun agent-repl--session-starting-p (&optional ws)
  "Return t if vterm exists with a live process but Claude is not yet ready.
WS defaults to the current workspace name.  Signals an error if no
workspace can be determined."
  (let ((ws (or ws (agent-repl--ws-current-name))))
    (unless ws (error "agent-repl--session-starting-p: no workspace specified and no current workspace"))
    (let ((result (and (agent-repl--vterm-process-alive-p ws)
                       (not (buffer-local-value 'agent-repl--ready
                                                (agent-repl--ws-get ws :vterm-buffer))))))
      (agent-repl--log-verbose ws "session-starting-p: ws=%s starting=%s" ws (if result "yes" "no"))
      result)))

;;;; Readiness timer (fallback polling)

(defun agent-repl--cancel-ready-timer (ws)
  "Cancel the readiness-poll timer for workspace WS, if any."
  (let ((timer (agent-repl--ws-get ws :ready-timer)))
    (if timer
        (progn
          (agent-repl--log ws "cancel-ready-timer: canceling timer for ws=%s" ws)
          (when (timerp timer) (cancel-timer timer))
          (agent-repl--ws-put ws :ready-timer nil))
      (agent-repl--log ws "cancel-ready-timer: no timer to cancel for ws=%s" ws))))

(defun agent-repl--detect-start-failure (ws)
  "Return a failure reason string if WS's vterm shows a start failure, else nil.
Prefers an explicit `agent-repl--start-failure-marker' line (returning the
trimmed text after it); otherwise matches a known fatal substring from
`agent-repl--start-failure-patterns'.  Returns nil when the vterm buffer is
absent/dead or shows no failure.  This is how a start command running in a
vterm (whose exit code agent-repl cannot observe) reports an unrecoverable
launch failure — e.g. `claude-sandbox' when Docker is down."
  (let ((buf (agent-repl--ws-get ws :vterm-buffer)))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-max))
          (if (search-backward agent-repl--start-failure-marker nil t)
              (let ((reason (string-trim
                             (buffer-substring-no-properties
                              (+ (point) (length agent-repl--start-failure-marker))
                              (line-end-position)))))
                (if (string-empty-p reason)
                    "start command reported a failure"
                  reason))
            (let (found)
              (dolist (pair agent-repl--start-failure-patterns found)
                (when (and (not found)
                           (progn (goto-char (point-max))
                                  (search-backward (car pair) nil t)))
                  (setq found (cdr pair)))))))))))

(defun agent-repl--ready-timer-tick (ws start-time)
  "Handle one tick of the readiness-poll timer for workspace WS.
START-TIME is the `float-time' when polling began.  Cancels the timer and
gives up after 30 seconds, surfaces a start-failure marker as `:start-failed'
the moment it appears, or cancels and opens panels once Claude is ready."
  (let ((elapsed (- (float-time) start-time))
        (failure (agent-repl--detect-start-failure ws)))
    (agent-repl--log-verbose ws "ready-timer-tick: ws=%s elapsed=%.1fs" ws elapsed)
    (cond
     ;; A start command (e.g. `claude-sandbox') printed an explicit failure
     ;; marker into the vterm — surface it loudly as `:start-failed' instead
     ;; of silently waiting out the timeout.
     (failure
      (agent-repl--cancel-ready-timer ws)
      (agent-repl--mark-start-failed ws (list 'error failure)))
     ((> elapsed agent-repl-ready-timeout-seconds)
      (agent-repl--cancel-ready-timer ws)
      (agent-repl--log ws "ready-timer: timed out for ws=%s" ws))
     ((agent-repl--session-starting-p ws) nil)
     (t
      (agent-repl--cancel-ready-timer ws)
      (message "[agent-repl] ready-timer catch-all for ws=%s (session no longer starting)" ws)
      (agent-repl--log ws "ready-timer: catch-all branch hit for ws=%s — not starting but not timed out" ws)
      (when (agent-repl--current-ws-p ws)
        (agent-repl))))))

(defun agent-repl--schedule-ready-timer (ws)
  "Poll every 0.5s until Claude is ready in WS, then auto-open panels.
Gives up after 30s. This is a fallback — the title-change path is the happy path."
  (agent-repl--cancel-ready-timer ws)
  (let ((start-time (float-time)))
    (agent-repl--ws-put ws :ready-timer
                         (run-at-time
                          agent-repl-ready-poll-interval agent-repl-ready-poll-interval
                          #'agent-repl--ready-timer-tick
                          ws start-time))))

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
