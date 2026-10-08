;;; core.el --- agent-repl core definitions -*- lexical-binding: t; -*-

;;; Code:

;; Cross-file forward declarations.  These sources load in the dependency
;; order config.el establishes and resolve each other's calls at call time,
;; so the declarations below exist for the byte-compiler alone.
(declare-function agent-repl--kill-buffer-safely "worktree")
(declare-function agent-repl--ws-add-buffer "workspace")
(declare-function agent-repl--ws-current-name "workspace")
(declare-function agent-repl--ws-dir "status")
(declare-function agent-repl--ws-get "workspace")
(declare-function agent-repl--ws-resolve-persp "workspace")

;; Special variables owned by other sources in this module, declared here
;; so the byte-compiler binds and reads them dynamically rather than
;; lexically.
(defvar agent-repl--workspaces)

(require 'cl-lib)

;;;; ---- Timer registry ----
;;
;; Every long-lived agent-repl timer is armed under a KEY through
;; `agent-repl--register-timer'.  Keyed registration is what makes arming
;; IDEMPOTENT: re-arming a key cancels and replaces the timer already held
;; under it instead of stacking a second one beside it.
;;
;; The landmine this exists to disarm has two halves, both reachable by an
;; ordinary hot-reload of a subset of the module's files:
;;
;;   (a) loading core.el WITHOUT its timer owners ran `--cancel-all-timers'
;;       and nothing re-armed, leaving zero timers — a dead 1Hz heartbeat
;;       and a frozen tab bar; and
;;   (b) re-loading an owner file WITHOUT a preceding cancel-all pushed a
;;       SECOND timer for the same job, so the heartbeat ran at 2Hz, 3Hz, …
;;
;; (b) is now structurally impossible: arming goes through the key, and a
;; key holds at most one timer.  (a) is caught by
;; `agent-repl--assert-heartbeat-armed' at the bottom of this file.

(defvar agent-repl--timers nil
  "List of active timers created by agent-repl.
Cancelled and reset whenever this file is re-evaluated.  Every entry is
also reachable by key through `agent-repl--keyed-timers'.")

(defvar agent-repl--keyed-timers nil
  "Alist of (KEY . TIMER) for every timer armed via `agent-repl--register-timer'.
KEY is a keyword naming the JOB, not the timer object, so re-arming the
same job replaces its timer rather than adding one.  Reset alongside
`agent-repl--timers' by `agent-repl--cancel-all-timers'.")

(defconst agent-repl--required-timer-keys
  '((:state-poll . agent-repl--arm-state-poll-timer)
    (:autosave   . agent-repl--arm-autosave-timer))
  "Alist of (KEY . ARM-FUNCTION) for every timer the module must keep armed.

This is the contract `agent-repl--assert-heartbeat-armed' enforces: each
KEY names a job that must have exactly one live timer once the module is
loaded, and ARM-FUNCTION is the owner file's entry point for (re-)arming
it.  The owners are `status.el' (:state-poll, the heartbeat that repaints
the tab bar) and `autosave.el' (:autosave).

EVERY KEY HERE MUST HAVE A LIVE OWNER.  Two did not: `:readiness-poll' and
`:workspace-status-export' outlived `readiness.el' and
`workspace-status-export.el', both deleted in the overhaul's dead-module
pre-pass, so their arm functions are defined nowhere.  No load order can
satisfy a key whose owner does not exist, and the deferral cannot either:
every cold start spent its idle second waiting and then warned
`outcome=unavailable reason=owner-not-loaded' twice, for jobs the daemon's
own roster push and address file had already taken over.  A key added here
without an owner is that warning again, forever.

Declared HERE rather than accumulated by the owners so a core.el loaded
by itself still knows what is supposed to be running — which is exactly
the case where the owners have not run and cannot have registered
anything.")

(defun agent-repl--timer-armed-p (key)
  "Return non-nil when KEY holds a timer that is actually scheduled.
A cancelled timer is still a `timerp', so membership in `timer-list' /
`timer-idle-list' is the only honest test of \"armed\"."
  (let ((timer (cdr (assq key agent-repl--keyed-timers))))
    (and (timerp timer)
         (or (memq timer timer-list)
             (memq timer timer-idle-list))
         t)))

(defun agent-repl--register-timer (key timer)
  "Register TIMER under KEY, cancelling and replacing any timer already held.
KEY is a keyword naming the job (see `agent-repl--required-timer-keys').
Returns TIMER.

This is the ONLY sanctioned way to arm a long-lived agent-repl timer:
going through the key is what makes re-loading an owner file idempotent
instead of additive.

Both arguments are invariants of the caller, not user input: a non-symbol
KEY or a non-timer TIMER is a programming error and is signalled rather
than absorbed."
  (unless (and key (symbolp key))
    (error "[agent-repl] register-timer: KEY must be a non-nil symbol, got %S" key))
  (unless (timerp timer)
    (error "[agent-repl] register-timer: TIMER must be a timer, got %S" timer))
  (let* ((cell (assq key agent-repl--keyed-timers))
         (prior (cdr cell))
         (replaced (timerp prior)))
    (when replaced
      (cancel-timer prior)
      (setq agent-repl--timers (delq prior agent-repl--timers)))
    (if cell
        (setcdr cell timer)
      (setq agent-repl--keyed-timers
            (append agent-repl--keyed-timers (list (cons key timer)))))
    (push timer agent-repl--timers)
    ;; Guard: owner files can arm before `agent-repl--log' exists on an
    ;; unusual load order; the registration itself must not depend on it.
    (when (fboundp 'agent-repl--log)
      (agent-repl--log '(:agent-repl-central "process-wide logging and utility state") "register-timer: key=%s replaced=%s timer=%S keyed-count=%d total-count=%d"
                       key (if replaced "t" "nil") timer
                       (length agent-repl--keyed-timers) (length agent-repl--timers)))
    timer))

(defun agent-repl--cancel-timer-key (key)
  "Cancel and deregister the timer held under KEY.
Returns non-nil when a timer was actually cancelled."
  (let* ((cell (assq key agent-repl--keyed-timers))
         (timer (cdr cell))
         (cancelled (timerp timer)))
    (when cancelled
      (cancel-timer timer)
      (setq agent-repl--timers (delq timer agent-repl--timers)))
    (when cell
      (setq agent-repl--keyed-timers (delq cell agent-repl--keyed-timers)))
    (when (fboundp 'agent-repl--log)
      (agent-repl--log '(:agent-repl-central "process-wide logging and utility state") "cancel-timer-key: key=%s cancelled=%s keyed-count=%d"
                       key (if cancelled "t" "nil") (length agent-repl--keyed-timers)))
    cancelled))

(defun agent-repl--cancel-all-timers ()
  "Cancel every timer in `agent-repl--timers' and reset both registries.

THE ONLY TIMERS LEFT ARE REGISTERED ONES.  The workspace-state update
chain this used to tear down separately — one-shot `run-at-time'
continuations outside the registry — died with the local state machine:
the roster pushes every workspace's state, so nothing polls and there is
no unregistered chain to leave armed across a reload."
  (let ((count (length agent-repl--timers))
        (keyed-count (length agent-repl--keyed-timers)))
    (dolist (timer agent-repl--timers)
      (when (timerp timer)
        (cancel-timer timer)))
    (setq agent-repl--timers nil)
    (setq agent-repl--keyed-timers nil)
    ;; Guard: this function is called at load time (line below), before
    ;; agent-repl--log is defined.  Only log when logging is available.
    (when (fboundp 'agent-repl--log)
      (agent-repl--log '(:agent-repl-central "process-wide logging and utility state") "cancel-all-timers: cancelled=%d keyed-cleared=%d"
                       count keyed-count))))

(agent-repl--cancel-all-timers)

;;;; ---- One-shot settle latch ----
;;
;; Every asynchronous operation in this module races several arrivals for
;; the right to finish it: the daemon's ack, a deadline timer, a polling
;; tick, a command rejection.  Exactly one of them may settle the
;; operation, and settling means three things happening together —
;; claiming the one-shot flag, cancelling the timers the operation armed,
;; and releasing whatever it registered elsewhere.
;;
;; Written inline, that sequence is INTERRUPTIBLE.  `C-g' landing between
;; the flag and the timer cancels leaves a settled operation with a live
;; deadline timer behind it; landing between the cancels and the release
;; leaves a workspace registered to a request nothing will ever untrack.
;; Both are exactly the stranded state the heartbeat assertion reports as
;; `outcome=stranded'.
;;
;; The latch below makes the sequence UNINTERRUPTIBLE instead of merely
;; short: `agent-repl--latch-claim' runs the whole of it with `inhibit-quit'
;; bound, so a quit arriving anywhere inside is held until the state is
;; consistent again.  Continuations run OUTSIDE that guard — a quit there
;; abandons the caller's work, which is what the user asked for, and finds
;; the registry already whole.
;;
;; Timers are held by KEY (`agent-repl--latch-set-timer') rather than in a
;; list, because a polling operation re-arms the same logical timer many
;; times per settle and a list would accumulate one entry per tick.  A
;; timer armed after the latch is claimed is cancelled on the spot rather
;; than recorded, which is what makes "no timer outlives its operation" a
;; structural property of the latch instead of a discipline every caller
;; has to keep.

(cl-defstruct (agent-repl--latch
               (:constructor agent-repl--make-latch (&optional cleanup))
               (:copier nil))
  "A one-shot, quit-safe settle guard for one asynchronous operation.

CLEANUP is an optional thunk run — with quit inhibited, after the latch's
timers are cancelled — by the single `agent-repl--latch-claim' that wins.
It is where the operation releases correlation handles it registered
elsewhere (tracked command ids, in-flight reservations).

TIMERS is an alist of (KEY . TIMER); see `agent-repl--latch-set-timer'."
  settled timers cleanup)

(defun agent-repl--latch-set-timer (latch key timer)
  "Hold TIMER on LATCH under KEY, cancelling any timer already held there.
KEY is a symbol naming the ROLE the timer plays for the operation
\(`deadline', `poll'), so re-arming a poll replaces its predecessor
instead of accumulating beside it.

A latch that is already settled records nothing and cancels TIMER
immediately: the operation it belonged to is over, so the only correct
lifetime for a timer arriving late is none at all.  Returns TIMER when it
was recorded, nil when it was cancelled as late.

Runs with quit inhibited: a `C-g' between cancelling the predecessor and
recording the replacement would strand whichever one the latch is no
longer holding."
  (unless (agent-repl--latch-p latch)
    (error "[agent-repl] latch-set-timer: LATCH must be a latch, got %S" latch))
  (unless (and key (symbolp key))
    (error "[agent-repl] latch-set-timer: KEY must be a non-nil symbol, got %S" key))
  (let ((inhibit-quit t))
    (if (agent-repl--latch-settled latch)
        (progn (when (timerp timer) (cancel-timer timer)) nil)
      (let* ((cell (assq key (agent-repl--latch-timers latch)))
             (prior (cdr cell)))
        (when (timerp prior) (cancel-timer prior))
        (if cell
            (setcdr cell timer)
          (setf (agent-repl--latch-timers latch)
                (append (agent-repl--latch-timers latch) (list (cons key timer)))))
        timer))))

(defun agent-repl--latch-claim (latch)
  "Claim LATCH for the caller, returning non-nil for exactly one caller.

The winner's claim cancels every timer LATCH holds and runs its CLEANUP
thunk, all with quit inhibited, so the operation's bookkeeping is whole
before any `C-g' is delivered.  Every later caller gets nil and must do
nothing.

The caller runs its own continuation AFTER this returns, deliberately
outside the guard."
  (unless (agent-repl--latch-p latch)
    (error "[agent-repl] latch-claim: LATCH must be a latch, got %S" latch))
  (let ((inhibit-quit t))
    (unless (agent-repl--latch-settled latch)
      (setf (agent-repl--latch-settled latch) t)
      (dolist (cell (agent-repl--latch-timers latch))
        (when (timerp (cdr cell)) (cancel-timer (cdr cell))))
      (setf (agent-repl--latch-timers latch) nil)
      (when-let ((cleanup (agent-repl--latch-cleanup latch)))
        (funcall cleanup))
      t)))

(defvar agent-repl--eager-open-in-progress nil
  "Non-nil while `agent-repl--eager-open-panels' transiently activates a
background workspace to pre-build its REPL panels at generation time.

The two `persp-activated-functions' reactions that would misfire on the
eager-open switch-in / build-panels / switch-back consult this flag and
no-op while it is set:

- `agent-repl--after-persp-activated' must NOT schedule the async
  `agent-repl--on-workspace-switch' — that deferred pass would fire for
  the now-background workspace after focus has returned to the caller and
  reclaim the caller's frame with the background workspace's panels (the
  eviction bug `agent-repl--gui-boot' documents).  The panels are built
  directly by `agent-repl--eager-open-panels' via the same drains that
  pass would run, so suppressing it loses no work.
- `agent-repl--record-workspace-history' must NOT record the transient
  visit — otherwise `SPC b p' would treat the just-generated workspace as
  the caller's previous workspace and stamp a phantom `:last-viewed-at'.")

(defgroup agent-repl nil
  "Claude Code REPL integration for Doom Emacs."
  :group 'tools
  :prefix "agent-repl-")

;;;; Canonical state directory
;;
;; agent-repl's OWN persisted state and cross-process IPC live in a
;; single, account-independent tree at `~/.claude-emacs' — a fixed
;; sibling of the Claude CLI config dir (~/.claude), deliberately
;; separate so account selection can never split it.
;;
;; The path is a plain hardcoded sibling rather than `doom-data-dir' so
;; the managed out-of-process shell scripts (hooks/*.sh,
;; skills/emit-workspace-commands.sh) can compute the IDENTICAL location
;; as `$HOME/.claude-emacs' without replicating Doom/XDG internals.
;;
;; The `AGENT_REPL_STATE_DIR' environment variable overrides the root,
;; honored identically by BOTH the elisp helper below and the shell
;; scripts.  It exists so the test suite can isolate state to a temp dir;
;; production leaves it unset and uses the `~/.claude-emacs' fallback.

(defconst agent-repl--state-dir-env "AGENT_REPL_STATE_DIR"
  "Name of the environment variable overriding `agent-repl--global-state-dir'.
Honored identically by the managed shell scripts.  Unset in production;
the test suite sets it to a temp dir to isolate state.")

(defconst agent-repl--state-dir-default "~/.claude-emacs"
  "Fallback root for agent-repl state when `agent-repl--state-dir-env' is unset.
A fixed sibling of the Claude CLI config dir (~/.claude).")

(defun agent-repl--global-state-dir ()
  "Return agent-repl's canonical state directory (with trailing slash).
Resolves the `agent-repl--state-dir-env' override, else
`agent-repl--state-dir-default' (~/.claude-emacs).  See the commentary
above for why this is a fixed sibling dir rather than `doom-data-dir'.
Creates nothing."
  (file-name-as-directory
   (expand-file-name (or (getenv agent-repl--state-dir-env)
                         agent-repl--state-dir-default))))

(defun agent-repl--global-state-file (relative)
  "Return the absolute path of RELATIVE under `agent-repl--global-state-dir'.
RELATIVE is a path fragment such as \"workspaces.el\" or \"output\".  An
empty string yields the state dir itself.  The parent directory is NOT
created here."
  (expand-file-name relative (agent-repl--global-state-dir)))

(defconst agent-repl--output-dir
  (file-name-as-directory (agent-repl--global-state-file "output"))
  "The daemon's workspace-command inbox at `~/.claude-emacs/output/'.
Every workspace-creation flavor — an Emacs chord, the generation skill, an
out-of-band agent — reaches the daemon by dropping a
`workspace_commands_<uuid>.json' file here.  Emacs writes into this
directory and never reads from it: the daemon is the sole watcher,
claimant, and deleter.")

(defconst agent-repl--legacy-state-migrations
  '(("~/.claude/emacs"                  . "")
    ("~/.claude/output"                 . "output")
    ("~/.claude/workspace-notifications" . "workspace-notifications"))
  "Alist (LEGACY-ABS . NEW-RELATIVE) of state dirs to migrate once.
LEGACY-ABS is the old location under the Claude CLI config dir;
NEW-RELATIVE is the fragment under `agent-repl--global-state-dir'.  The
legacy `~/.claude/emacs' dir flattens onto the state-dir root (empty
NEW-RELATIVE), so `~/.claude/emacs/workspaces.el' becomes
`~/.claude-emacs/workspaces.el'.")

(defun agent-repl--migrate-legacy-state ()
  "One-time move of agent-repl state out of the legacy ~/.claude location.
Historically agent-repl kept its own state and IPC under the Claude CLI
config dir; it now lives under `agent-repl--global-state-dir'.  For each entry
in `agent-repl--legacy-state-migrations' whose legacy path still exists
and whose new counterpart does NOT, move it.  Idempotent: a no-op once
migrated or on a fresh install.  Never overwrites an existing
new-location path.  A failed move is surfaced (warned/logged), never
swallowed, and does not abort the remaining migrations."
  (dolist (pair agent-repl--legacy-state-migrations)
    (let ((old (expand-file-name (car pair)))
          (new (agent-repl--global-state-file (cdr pair))))
      (when (and (file-exists-p old) (not (file-exists-p new)))
        (condition-case err
            (progn
              (make-directory (file-name-directory (directory-file-name new)) t)
              (rename-file old new)
              ;; This runs at load time BEFORE the severity ladder below is
              ;; defined (the `fboundp' guard is exactly that case), so the
              ;; fallback cannot call `agent-repl--info' / `agent-repl--warn'.
              ;; The success line is quieted with `inhibit-message' (*Messages*
              ;; only, never the echo area); the failure line stays a LOUD bare
              ;; `message', which is already the channel `agent-repl--warn'
              ;; would have used.
              (if (fboundp 'agent-repl--log)
                  (agent-repl--log '(:agent-repl-central "process-wide logging and utility state") "migrate-legacy-state: moved %s -> %s" old new)
                (let ((inhibit-message t))
                  (message "[agent-repl] migrated state %s -> %s" old new))))
          (error
           (message "[agent-repl] WARNING: state migration %s -> %s failed: %S"
                    old new err)))))))

;; Run only in a real interactive session.  The `noninteractive' guard
;; keeps batch invocations (the ERT suite, CI, ad-hoc `emacs -batch'
;; scripts) from MOVING the developer's live ~/.claude state out from
;; under a separately-running interactive Emacs — which would split state
;; across the two locations.  The function itself is still exercised
;; directly (with isolated temp paths) by test-core.el.
(unless noninteractive
  (agent-repl--migrate-legacy-state))

(defcustom agent-repl-debug nil
  "Controls debug visibility without changing durable persistence.
nil suppresses debug output from *Messages*; t shows standard debug output;
\\='verbose also shows high-frequency events (window changes, resolve-root,
process-alive predicates, sentinel re-entry).

THIS SETTING CONTROLS VISIBILITY ONLY.  It does not gate the durable
sinks and never has: records still persist whenever they clear
`agent-repl-log-file-level' and `agent-repl-log-to-file' is non-nil.  If
what you want is a smaller LOG FILE, this is the wrong knob — set
`agent-repl-log-file-level'.  Use \\[agent-repl-toggle-debug] (with a
`C-u' prefix for verbose) to flip at runtime."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)
                 (const :tag "Verbose" verbose))
  :group 'agent-repl)

(defcustom agent-repl-log-to-file t
  "Master kill-switch for file-writing of agent-repl log lines.
When non-nil (the default), every call to `agent-repl--log',
`agent-repl--info', `agent-repl--warn', `agent-repl--do-log',
`agent-repl--error', or `agent-repl--fatal' appends its JSONL record to
the workspace's canonical
sink, or to `agent-repl-log-file-name' when the call is genuinely
workspace-agnostic, REGARDLESS of `agent-repl-debug'.
`agent-repl--log-verbose' persists as well; `agent-repl-debug' controls
only *Messages* visibility.  This is the ALL-OR-NOTHING switch; for a
threshold that keeps warnings and errors while dropping chatter, use
`agent-repl-log-file-level'.  `setq' this variable to flip the
kill-switch at runtime."
  :type 'boolean
  :group 'agent-repl)

(defconst agent-repl--log-level-window-seconds 300
  "The longest a durable log level other than `info' may last, in seconds.
A level other than `info' is a WINDOW, never a standing setting: it holds
until its end and then reverts to `info' by itself.  The same rule binds
every runtime (proto/vocab/log-level-window.json).")

(defvar agent-repl--log-level-clock #'float-time
  "Function answering the current time in seconds, for level windows.
Tests bind it to move time instead of waiting for it.")

(defvar agent-repl--log-level-expires-at nil
  "When the current non-`info' `agent-repl-log-file-level' ends, or nil.
Seconds since the epoch.  nil is a level that never ends, which is what
`info' always is and what a direct `setq' of the level gives.")

(defun agent-repl--log-level-window (level until now)
  "Decide the durable log level from LEVEL and UNTIL at NOW.
LEVEL and UNTIL are the raw values of `AGENT_REPL_LOG_LEVEL' and
`AGENT_REPL_LOG_LEVEL_UNTIL' (nil when unset); UNTIL and NOW are Unix
seconds.  Return a plist: `:level' the symbol to start at, `:until' when it
ends (nil when it never does), `:outcome' one of `default', `honored',
`no_expiry', `expired' or `beyond_window', and `:requested' /
`:requested-until' as read.

A level other than `info' holds only inside an unexpired window no longer
than `agent-repl--log-level-window-seconds'; otherwise the answer is `info'.
An unknown LEVEL or an UNTIL that is not a decimal integer signals: a
setting nobody can read is refused, never reinterpreted."
  (let ((parsed (cond
                 ((null level) 'info)
                 ((member level '("debug" "info" "warn" "error")) (intern level))
                 (t (error "agent-repl: AGENT_REPL_LOG_LEVEL must be debug, info, warn, or error; got %S"
                           level))))
        (end (cond
              ((or (null until) (string-empty-p until)) nil)
              ((string-match-p "\\`[+-]?[0-9]+\\'" until) (string-to-number until))
              (t (error "agent-repl: AGENT_REPL_LOG_LEVEL_UNTIL must be a Unix second; got %S"
                        until)))))
    (append
     (list :requested level :requested-until until)
     (cond
      ((eq parsed 'info) (list :level 'info :until nil :outcome 'default))
      ((null end) (list :level 'info :until nil :outcome 'no_expiry))
      ((>= now end) (list :level 'info :until nil :outcome 'expired))
      ((> (- end now) agent-repl--log-level-window-seconds)
       (list :level 'info :until nil :outcome 'beyond_window))
      (t (list :level parsed :until end :outcome 'honored))))))

(defun agent-repl--log-level-selection-from-environment ()
  "Return `agent-repl--log-level-window' for this process's environment now."
  (agent-repl--log-level-window (getenv "AGENT_REPL_LOG_LEVEL")
                                (getenv "AGENT_REPL_LOG_LEVEL_UNTIL")
                                (funcall agent-repl--log-level-clock)))

(defun agent-repl--log-level-from-environment ()
  "Return the durable log level selected by `AGENT_REPL_LOG_LEVEL'.
An unset variable selects `info', and so does a level other than `info'
outside the window `AGENT_REPL_LOG_LEVEL_UNTIL' names.  Any set value
outside the shared `debug|info|warn|error' vocabulary, or a malformed
window, aborts the module load."
  (plist-get (agent-repl--log-level-selection-from-environment) :level))

(defcustom agent-repl-log-file-level (agent-repl--log-level-from-environment)
  "Least severe rung the LOG FILE records.  See the ladder in core.el.

This is the knob `agent-repl-debug' is repeatedly mistaken for.  That one
governs *Messages* visibility ONLY and has never had any effect on what
reaches disk.

Ordered least to most severe: `debug', `info', `warn', `error'.  A record is
written when its level is at or above this one.  Verbose records carry level
`debug' and persist whenever debug records do; verbosity controls presentation,
not durability.

The initial value comes from `AGENT_REPL_LOG_LEVEL' at module load and is
`info' when that variable is absent.  A level other than `info' is a window
of at most `agent-repl--log-level-window-seconds': it is honored only with
an unexpired `AGENT_REPL_LOG_LEVEL_UNTIL' (Unix seconds) and reverts to
`info' by itself when the window ends.  The Elisp variable remains the runtime
knob: \\[agent-repl-set-log-file-level] changes subsequent records immediately.

This does NOT control the per-workspace log BUFFERS; see
`agent-repl-log-buffer-level'."
  :type '(choice (const :tag "Debug" debug)
                 (const :tag "Info" info)
                 (const :tag "Warnings" warn)
                 (const :tag "Errors only" error))
  :group 'agent-repl)

;; `defcustom' preserves an already-bound value across a Doom reload.  The
;; process environment is the shared startup switch, so every module load
;; re-reads it and resets the Elisp knob to the process-level selection --
;; including its window's end, so a leftover debug level never outlives it.
(defvar agent-repl--log-level-startup-selection nil
  "The level decision this module load made, noted once logging exists.")

(let ((selection (agent-repl--log-level-selection-from-environment)))
  (setq agent-repl-log-file-level (plist-get selection :level)
        agent-repl--log-level-expires-at (plist-get selection :until)
        agent-repl--log-level-startup-selection selection))

(defcustom agent-repl-log-buffer-level 'warn
  "Least severe rung the per-workspace log BUFFERS display.

Same ladder as `agent-repl-log-file-level', and independent of it: the
file is a forensic record nobody reads top to bottom, while these buffers
are read by a human, live, while debugging.  The verbose rung is
hot-path chatter — `json-null-if-nil', `ws-keyword-to-string',
`sidebar-tick' — which drowns out the lines a reader is actually
following, so the default keeps it out of the buffers while the file goes
on recording it.

THE DEFAULT IS `warn', which is deliberately stricter than the file's.
These buffers exist to show a human what went WRONG in a workspace, and a
running commentary of ordinary debug lines buries that.  Lower it to
`debug' to follow a workspace's ordinary activity, or to `verbose' when
the chatter itself is the object of study."
  :type '(choice (const :tag "Verbose (everything)" verbose)
                 (const :tag "Debug" debug)
                 (const :tag "Info" info)
                 (const :tag "Warnings (default)" warn)
                 (const :tag "Errors only" error))
  :group 'agent-repl)

(defcustom agent-repl-log-size-cap-bytes (* 64 1024 1024)
  "Maximum bytes in one active Emacs log generation.  Default 64 MiB.
The write that would cross the cap first moves the active file to `.1', shifts
older generations, and writes the complete new record to a fresh file."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-log-generation-count 5
  "Number of completed Emacs log generations retained beside the active file.
`.1' is the newest completed generation and `.N' is the oldest."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-workspace-dir-hash-length 8
  "Number of hex characters from the MD5 of a workspace's canonical directory.
This is the width of `workspace_dir_hash', the shim-held kernel lock file's
derivation (`dlog.WorkspaceDirHashLength').  It is NOT a `workspace_id':
that identity is minted by the daemon and only received here."
  :type 'integer
  :group 'agent-repl)

(defun agent-repl--default-log-directory ()
  "Return agent-repl's private directory under the OS temporary root.
It held the central sink until the sink moved under the state root's
`logs/' (see `agent-repl--default-log-file-name'); it is still validated
as private whenever `agent-repl-log-file-name' is customized into it.
The numeric Unix user ID keeps users from colliding when
`temporary-file-directory' names a shared directory such as /tmp on Linux.
The directory is not created here.

Do not instrument this path resolver through the logging ladder: resolving
the logfile path is itself a prerequisite for emitting a log line."
  (unless (and (stringp temporary-file-directory)
               (not (string= temporary-file-directory ""))
               (file-name-absolute-p temporary-file-directory))
    (error "agent-repl--default-log-directory: invalid temporary-file-directory=%S"
           temporary-file-directory))
  (file-name-as-directory
   (expand-file-name (format "doom-agent-repl-%d" (user-uid))
                     temporary-file-directory)))

(defconst agent-repl--central-log-file-basename "emacs.central.log"
  "File name of the durable central Emacs sink inside the state `logs/'.
It follows that directory's `<runtime>.<kind>.log' scheme, beside the
daemon's own `daemon.run.log'.")

(defconst agent-repl--default-log-file-name
  (agent-repl--global-state-file
   (concat "logs/" agent-repl--central-log-file-basename))
  "Default durable path for workspace-agnostic agent-repl records.

THE CENTRAL SINK LIVES BESIDE EVERY OTHER DURABLE TARGET (owner ruling,
2026-09-27): `~/.claude-emacs/logs/emacs.central.log'.  Creation, fork,
kill, teardown and daemon administration are recorded under the
`:agent-repl-central' scope, and a record a person is asked to read must
not sit where the operating system may sweep it or where a per-launcher
TMPDIR moves it -- the same rule the workspace targets already follow
\(`agent-repl--emacs-log-target-directory').")

(defconst agent-repl--retired-state-log-file-name
  (agent-repl--global-state-file "doom-agent-repl.log")
  "Retired pre-temp-directory default for workspace-agnostic records.")

(defconst agent-repl--retired-temp-log-file-name
  (expand-file-name "doom-agent-repl.log"
                    (agent-repl--default-log-directory))
  "Retired OS-temporary default for workspace-agnostic records.")

(defun agent-repl--normalize-log-file-name (value)
  "Return the active logfile path for configured VALUE.
Each retired default -- the state-tree root file and the OS-temporary
file -- is redirected to the current default so reloading this module
updates an already-bound defcustom.  Every other explicit path is
preserved.  No file is moved, copied, read, or deleted."
  (if (member (expand-file-name value)
              (list (expand-file-name agent-repl--retired-state-log-file-name)
                    (expand-file-name agent-repl--retired-temp-log-file-name)))
      agent-repl--default-log-file-name
    value))

(defcustom agent-repl-log-file-name agent-repl--default-log-file-name
  "Path to the workspace-agnostic agent-repl log file.
Defaults to `emacs.central.log' in the state root's `logs/' directory,
normally ~/.claude-emacs/logs/emacs.central.log, beside the daemon's
`daemon.run.log' and every workspace's log target.

Workspace-owned records do not use this path.  They persist through the
workspace's canonical .claude/emacs/emacs.log symlink.

Logs at the retired defaults are intentionally neither migrated nor
deleted.  The value is passed through `expand-file-name', and the parent
directory is created on demand by `agent-repl--logfile-path'."
  :type 'string
  :group 'agent-repl)

;; `defcustom' does not reset an already-bound variable during a Doom module
;; reload.  Redirect the exact retired default so this change takes effect
;; without an Emacs restart.  This runs before the logging ladder exists and
;; intentionally performs no file migration or logging.
(setq agent-repl-log-file-name
      (agent-repl--normalize-log-file-name agent-repl-log-file-name))

;; NOTE: agent-repl-default-workspace-name was removed as part of the
;; no-defaults-no-fallbacks refactor.  Buffer naming now errors when no
;; workspace name can be determined, rather than silently using "default"
;; (which caused unrelated contexts to collide on the same buffer).

(defcustom agent-repl-ws-name-allowed-chars-re "[^[:alnum:]_-]"
  "Regexp matching characters to replace in workspace names.
Characters matching this pattern are replaced with underscores."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-panel-buffer-name-format "*agent-panel%s-%s*"
  "Format string for agent panel buffer names.
First %s is the suffix (e.g. \"-input\" or empty), second %s is the
workspace name."
  :type 'string
  :group 'agent-repl)

;;; Workspace-name prefix

(defun agent-repl--workspace-prefix ()
  "Return the workspace-name prefix from the workspace-prefix env vars.
Reads AGENT_WORKSPACE_PREFIX first, falling back to the legacy
CLAUDE_WORKSPACE_PREFIX (external launchers still set the old name).
Returns the bare prefix with no trailing slash (e.g. \"ABC\"), or the
empty string when neither env var is set or non-empty.  This mirrors how
the workspace-dispatch run.sh derives the branch prefix solely from the
same env vars, so the Emacs process must be launched with one of them
set to obtain a prefix."
  (let ((new (getenv "AGENT_WORKSPACE_PREFIX"))
        (legacy (getenv "CLAUDE_WORKSPACE_PREFIX")))
    (if (and new (not (string-empty-p new)))
        (progn
          (agent-repl--log '(:agent-repl-central "process-wide logging and utility state")
                            "workspace-prefix: source=AGENT_WORKSPACE_PREFIX value=%S"
                            new)
          new)
      (let ((result (or legacy "")))
        (agent-repl--log '(:agent-repl-central "process-wide logging and utility state")
                          "workspace-prefix: source=CLAUDE_WORKSPACE_PREFIX value=%S"
                          result)
        result))))

;;; Kill-cause attribution

(defvar agent-repl--kill-cause nil
  "Why the current teardown is happening, for log attribution.
Every entry point that kills an agent session or tears down a
workspace let-binds this to a short human-readable cause string
\(e.g. \"interactive kill command\", \"merged-clear idle timer (auto)\")
for the dynamic extent of the teardown.  The shared chokepoints
\(`agent-repl--kill-one-workspace', `agent-repl--finish-workspace',
`agent-repl--ws-del', the frontend kill dispatch) read it into their
log lines so the log always answers HOW a session was killed.  A nil
value logs as \"unattributed(BUG: bind agent-repl--kill-cause)\" —
treat that as a missing binding at the initiator, not an acceptable
state.")

(defun agent-repl--kill-cause-str ()
  "Return `agent-repl--kill-cause' rendered for a log line."
  (or agent-repl--kill-cause "unattributed(BUG: bind agent-repl--kill-cause)"))

;;; Logging

(defun agent-repl--ws-dir-hash-cached (ws)
  "Return the cached workspace DIRECTORY HASH for WS, computing if needed.
Uses :project-dir from the workspace state to derive
md5hex(canonical dir)[:`agent-repl-workspace-dir-hash-length'] -- the
derivation the shim-held kernel lock file uses, recorded on every workspace
record as `workspace_dir_hash'.  Caches the result under :ws-dir-hash to
avoid repeated `file-truename' calls.  Returns nil if WS is nil or no
:project-dir is set.

THIS IS NOT A `workspace_id'.  A record's `workspace_id' is the
daemon-minted 16-hex identity every runtime shares
(`agent-repl--ws-daemon-log-id'); the two never substitute for each other.
See logging-contract.md (\"JSONL schema\")."
  (when ws
    (or (plist-get (gethash ws agent-repl--workspaces) :ws-dir-hash)
        (when-let ((dir (plist-get (gethash ws agent-repl--workspaces) :project-dir)))
          (let ((hash (substring (md5 (directory-file-name (file-truename dir)))
                                 0 agent-repl-workspace-dir-hash-length)))
            (puthash ws (plist-put (gethash ws agent-repl--workspaces) :ws-dir-hash hash)
                     agent-repl--workspaces)
            hash)))))

(defun agent-repl--ws-daemon-log-id (ws)
  "Return the DAEMON-MINTED workspace id for WS, or nil when unknown.
The roster push is what teaches Emacs this id: each reconciled row writes
its ref onto the workspace (`agent-repl--ws-put name :ref ref'), and the
ref carries `:id' -- the 16-hex `wsm.IDLength' identity the daemon minted
and every other runtime stamps as `workspace_id'.  Emacs RECEIVES it; it
never computes one.

Nil is the honest answer before the first roster push has reached this
workspace, and `agent-repl--log-add-workspace-identity' then omits
`workspace_id' entirely rather than stamping a path-derived stand-in that
would make one workspace look like two in a harvest grouped by that field."
  (when ws
    (let ((id (or (plist-get (agent-repl--ws-get ws :ref) :id)
                  (agent-repl--ws-get ws :id))))
      (and (stringp id) (not (string-empty-p id)) id))))

(defun agent-repl--format-ws-metadata (ws)
  "Return a context string with all workspace metadata for WS, or \"\".
Includes every meaningful key from the workspace plist in
`agent-repl--workspaces'.  Returns \"\" when WS is nil or has no
registered state.  Object-valued keys (buffers, processes, timers,
structs) are represented compactly (live/dead, running/nil, present/nil)."
  (if (or (null ws) (not (boundp 'agent-repl--workspaces)))
      ""
    (let ((plist (gethash ws agent-repl--workspaces)))
      (if (null plist)
          (format " {ws=%s}" ws)
        (let* ((id       (agent-repl--ws-dir-hash-cached ws))
               (dir      (plist-get plist :project-dir))
               (cstate   (plist-get plist :agent-state))
               (rstate   (plist-get plist :repl-state))
               (env      (plist-get plist :active-env))
               (fbuf     (plist-get plist :frontend-buffer))
               (ibuf     (plist-get plist :input-buffer))
               (wt       (plist-get plist :worktree-p))
               (fork     (plist-get plist :fork-session-id))
               (rtimer   (plist-get plist :ready-timer))
               (pri      (plist-get plist :priority))
               (pshow    (plist-get plist :pending-show-panels)))
          (format (concat " {ws=%s id=%s dir=%s cst=%s rst=%s env=%s"
                          " fe=%s in=%s"
                          " wt=%s fork=%s"
                          " rtmr=%s pri=%s pshow=%s}")
                  ws
                  (or id "-")
                  (or dir "-")
                  (or cstate "-")
                  (or rstate "-")
                  (or env "-")
                  (if fbuf (if (buffer-live-p fbuf) "live" "dead") "-")
                  (if ibuf (if (buffer-live-p ibuf) "live" "dead") "-")
                  (if wt "t" "-")
                  (or fork "-")
                  (if rtimer "t" "-")
                  (or pri "-")
                  (if pshow "t" "-")))))))

(defvar agent-repl--log-format-bug-captured nil
  "Set to t once a non-string FMT has been captured by `agent-repl--log-format'.
Prevents repeated backtrace captures from flooding the diagnostic buffer.")

(defun agent-repl--log-format-capture-bug (fmt)
  "Write a backtrace to *agent-repl-log-bug* the first time FMT isn't a string.
Lets us find the caller passing a bad FMT without crashing it.  Subsequent
bad calls are silently coerced so the log stays usable."
  (unless agent-repl--log-format-bug-captured
    (setq agent-repl--log-format-bug-captured t)
    (let ((buf (get-buffer-create "*agent-repl-log-bug*"))
          (bt (with-output-to-string (ignore-errors (backtrace)))))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (format "\n=== non-string fmt=%S at %s ===\n"
                        fmt (format-time-string "%H:%M:%S.%3N")))
        (insert bt)))))

(defun agent-repl--log-format (ws fmt)
  "Return FMT with a timestamp, [agent-repl] tag, and trailing workspace context.
WS is the workspace name (or nil for workspace-free contexts).  When non-nil,
all workspace metadata from `agent-repl--workspaces' is appended after FMT.

Hardened against non-string FMT: captures a backtrace to *agent-repl-log-bug*
the first time it happens, then coerces the value so the caller doesn't crash.

Note: callers using this to build a format string for `apply #\\='message'
should be aware that the returned string embeds the workspace metadata
literally, so any `%' characters in metadata will be interpreted as
format directives.  `agent-repl--do-log' avoids this by passing
metadata as an argument rather than splicing it into the format."
  (let ((safe-fmt (if (stringp fmt)
                      fmt
                    (agent-repl--log-format-capture-bug fmt)
                    (format "[BUG non-string-fmt=%S]" fmt))))
    (concat (format-time-string "%H:%M:%S.%3N") " [agent-repl] "
            safe-fmt (agent-repl--format-ws-metadata ws))))

(defvar agent-repl--validated-private-log-directories
  (make-hash-table :test #'equal)
  "Private temporary log directories validated during this Emacs process.")

(defun agent-repl--logfile-path ()
  "Return the expanded path of `agent-repl-log-file-name'.
The parent directory is created if it does not exist.  The state root's
`logs/' directory, where the default sink lives, is required to be a real
directory rather than a symlink.  The retired UID-qualified temporary
directory, when a customized path still names it, is required to be a real
directory owned by the current user and is forced to mode 0700.

Do not instrument this helper through the logging ladder: it is called while
constructing every file-backed log entry."
  (let* ((path (expand-file-name agent-repl-log-file-name))
         (dir (file-name-directory path)))
    (unless (file-directory-p dir)
      (make-directory dir t)
      (remhash dir agent-repl--validated-private-log-directories))
    (when (equal (directory-file-name dir)
                 (directory-file-name (agent-repl--default-log-directory)))
      (agent-repl--validate-private-log-directory dir))
    ;; The state `logs/' directory holds every Emacs target, so the central
    ;; sink is held to the SAME real-directory rule the workspace targets are
    ;; minted under: a symlinked `logs/' is refused, never followed.
    (when (equal (directory-file-name dir)
                 (directory-file-name (agent-repl--emacs-log-target-directory)))
      (agent-repl--ensure-real-log-directory dir))
    path))

(defun agent-repl--validate-private-log-directory (dir)
  "Validate ownership and permissions for private temporary log DIR once.
Validation is cached because this path runs for every file-backed log line.
`agent-repl--logfile-path' evicts the cache entry whenever it has to recreate
DIR.  This helper intentionally does not log because doing so would recurse."
  (unless (gethash dir agent-repl--validated-private-log-directories)
    (when (file-symlink-p (directory-file-name dir))
      (error "agent-repl--validate-private-log-directory: directory is a symlink: %s"
             dir))
    (let* ((attrs (file-attributes dir 'integer))
           (owner (and attrs (file-attribute-user-id attrs))))
      (unless (equal owner (user-uid))
        (error "agent-repl--validate-private-log-directory: owner=%S expected=%S path=%s"
               owner (user-uid) dir)))
    (set-file-modes dir #o700)
    (puthash dir t agent-repl--validated-private-log-directories)))

(defvar agent-repl--workspace-log-targets (make-hash-table :test #'equal)
  "Runtime-owned external targets for workspace `emacs.log' symlinks.

KEYED BY THE WORKSPACE'S IDENTITY, NOT BY ITS NAME
\(`agent-repl--workspace-log-target-key'), because the thing a target
belongs to is a DIRECTORY and the name is only one spelling of it.

Keyed by name, two names for one directory each installed their own
target and each overwrote the other's canonical `emacs.log' symlink, so
the LOSER wrote to an orphaned temp file that nothing on disk pointed at.
Measured 2026-08-11: `main' held
`/var/.../agent-repl-emacs-EGrKmo.log' and `marcos-pr-remediation' held
`/var/.../agent-repl-emacs-ZqvegH.log' for the SAME `:project-dir' and
the SAME `workspace_id', the symlink pointed at the pseudo's, and every
record the real workspace wrote for a day was invisible in the file every
reader opens.

With the identity as the key that is unrepresentable: one directory has
one target and one link, whatever names resolve to it.")

(defun agent-repl--workspace-log-target-key (identity)
  "Return the durable-target registry key for IDENTITY.
IDENTITY is an `agent-repl--workspace-log-identity' plist.  Both halves
are in the key because either changing rebinds the sink: a directory-hash
change is a different workspace at the same path, and a project-dir
change is the same workspace at a different path.  NUL-joined because it
is the one byte a path cannot contain, so two identities can never
collide by concatenation.

The DIRECTORY HASH is the half used, not the daemon-minted `workspace_id':
the id is unknown until the roster push arrives, so keying by it would give
one directory two sinks — one for its pre-registration records and one for
the rest — which is exactly the split of records this registry exists to
make unrepresentable."
  (concat (plist-get identity :workspace-dir-hash)
          "\0"
          (plist-get identity :project-dir)))

(defun agent-repl--workspace-log-target-entry (ws)
  "Return the durable-target registry entry for WS's identity, or nil.
The registry is keyed by IDENTITY, so a caller holding only a NAME asks
through here rather than rebuilding the key — which is also what keeps a
reader from pinning the key's encoding.

Screened through `agent-repl--ws-log-routable-p', the documented predicate
form of the identity resolver's precondition, so a name that owns no sink
answers nil instead of reaching a resolver that would signal.  The signal
itself is untouched at the one call site that must honour it,
`agent-repl--workspace-emacs-log-target'."
  (and (agent-repl--ws-log-routable-p ws)
       (gethash (agent-repl--workspace-log-target-key
                 (agent-repl--workspace-log-identity ws))
                agent-repl--workspace-log-targets)))

(defconst agent-repl--emacs-log-target-prefix "agent-repl-emacs-"
  "Filename prefix that identifies an Emacs-owned external log target.")

(defun agent-repl--json-object (&rest pairs)
  "Return PAIRS as a JSON object with string keys.
Each member of PAIRS is a cons whose car is the field name and whose cdr is
already JSON-serializable.  A hash table avoids the nil/empty-alist ambiguity
in `json-serialize'."
  (let ((object (make-hash-table :test #'equal)))
    (dolist (pair pairs object)
      (puthash (car pair) (cdr pair) object))))

(defconst agent-repl--log-timestamp-format "%FT%T.%6N%:z"
  "The log timestamp representation shared by every agent-repl runtime.
RFC 3339 in the machine's local zone, on a 24-hour clock, with fixed-width
microseconds and an explicit numeric offset.  Fixed width keeps records from
different runtimes lexically comparable.")

(defun agent-repl--log-rfc3339-timestamp (&optional time)
  "Return TIME, or the current time, in `agent-repl--log-timestamp-format'."
  (format-time-string agent-repl--log-timestamp-format time))

(defun agent-repl--log-operation (fmt)
  "Return the stable operation name represented by FMT.
The public logging APIs retain their format-string signatures, so the format
template itself is normalized into a deterministic machine-readable name.
Runtime values in ARGS never participate in the operation name."
  (let* ((source (if (stringp fmt) fmt (format "%S" fmt)))
         (normalized (replace-regexp-in-string
                      "[^[:alnum:]]+" "-" (downcase source))))
    (setq normalized (replace-regexp-in-string "\\`-+\\|-+\\'" "" normalized))
    (concat "agent-repl." (if (string-empty-p normalized)
                                "log"
                              normalized))))

(defvar agent-repl--log-dir-truenames (make-hash-table :test #'equal)
  "Each workspace directory's canonical spelling, keyed by the registered spelling.
Logging asks for it on EVERY record, and asking the filesystem each time
\(`file-directory-p' then `file-truename', which walks every path
component) was two-thirds of Emacs's CPU while workspaces were switched
\(profiled 2026-10-03).  A directory's canonical spelling does not change
while it exists, so it is resolved once.  Only an EXISTING directory is
remembered: one that does not exist yet is asked again, so it routes the
moment it appears.  A workspace that leaves the registry stops routing at
the registry lookup, before this is consulted.

THE SPELLING IS REMEMBERED, NOT THE EXISTENCE.  A remembered directory is
still `stat'ed on every lookup (one call, never the `file-truename' walk),
and one that has gone is forgotten.  A worktree a merge or a plain `rm'
removed while its tab still stands is otherwise routable forever: every
record of that workspace then tried to re-make its `.claude/emacs' link
under the vanished directory and signalled out of whatever handler logged
-- a roster push, a successor's link-up -- instead of landing centrally.")

(defvar agent-repl--log-dir-confirmed nil
  "Directories confirmed to exist while the record in progress is emitted.
nil outside `agent-repl--emit-log-record'.  Inside it, a list headed by
`:confirmed' whose tail is (DIR . CANONICAL) pairs.  One record asks
whether its workspace's directory exists twice -- once to route, once to
stamp or check its identity -- microseconds apart, and the second `stat'
was a quarter of a dropped workspace record's cost.  The confirmation lasts
for that one record only; the record's central-fallback re-check binds this
back to nil, so a directory that goes between routing and writing is still
found gone.")

(defun agent-repl--log-dir-truename (dir)
  "Return DIR's canonical spelling when it is an existing directory, else nil.
The spelling is resolved once per DIR (`agent-repl--log-dir-truenames');
its existence is confirmed on every call, and a directory that has gone
is forgotten.  Within one record, a confirmation is reused
\=(`agent-repl--log-dir-confirmed')."
  (or (cdr (assoc dir (cdr agent-repl--log-dir-confirmed)))
      (let* ((remembered (gethash dir agent-repl--log-dir-truenames))
             (canonical
              (if (and remembered (file-directory-p remembered))
                  remembered
                (when remembered
                  (remhash dir agent-repl--log-dir-truenames))
                (and (file-directory-p dir)
                     (puthash dir (directory-file-name (file-truename dir))
                              agent-repl--log-dir-truenames)))))
        (when (and canonical agent-repl--log-dir-confirmed)
          (setcdr agent-repl--log-dir-confirmed
                  (cons (cons dir canonical) (cdr agent-repl--log-dir-confirmed))))
        canonical)))

(defun agent-repl--ws-log-routable-p (ws)
  "Return non-nil when WS can be resolved to a durable workspace log sink.
This is the predicate form of `agent-repl--workspace-log-identity''s
precondition, and the two must agree: passing a WS that satisfies this
predicate must never make the identity resolver signal.  A test pins that
equivalence so the two cannot drift apart.

A nil WS is deliberately NOT routable.  nil is the logging ladder's explicit
global-sink value, not a workspace, and callers pass it directly rather than
asking this predicate about it.

This exists so a caller holding a name of UNKNOWN provenance — a persp-mode
perspective, a wire field, a workspace mid-teardown — can ask whether that
name owns a sink before handing it to the ladder.  It never suppresses the
invariant; it lets a caller avoid violating it.

A PSEUDO-PERSPECTIVE IS REFUSED BEFORE THE REGISTRY IS CONSULTED, and the
order is the whole point.  persp-mode's built-ins (\"none\",
`+workspaces-main') are not workspaces and own no directory of their own —
but nothing STOPS a stray `agent-repl--ws-put' from writing one into their
hash entry, and one did: on 2026-08-11 the live registry held
`main' -> .../marcos-pr-remediation/ and
`none' -> .../slack-cee-ceac-integration-shj/,
the trailing-slash shape of a captured `default-directory'.  Those entries
satisfied every clause below, so both built-ins were ROUTABLE, and every
record they carried was written into a real workspace's durable log and
stamped with that workspace's `workspace_dir' / `workspace_id'.  Measured:
60 of 60 `recovery-slo:' records in each of those two files named the
pseudo, and neither workspace had ever produced a record of its own.

Deciding the name CLASS first makes that unrepresentable: a built-in
perspective cannot own a sink no matter what got registered under its name,
so it can never shadow the workspace whose directory it borrowed.  The
record is classified as central and stamped with `pseudo_workspace', so it
cannot be confused with a workspace-owned record."
  (and ws
       (not (agent-repl--pseudo-workspace-name-p ws))
       (fboundp 'agent-repl--ws-get)
       (let ((dir (agent-repl--ws-get ws :project-dir)))
         (and (stringp dir)
              (agent-repl--log-dir-truename dir)
              (agent-repl--ws-dir-hash-cached ws)
              t))))

;; persp-mode and Doom own these; core.el only reads them, and only when the
;; owning package has bound them.
(defvar persp-nil-name)
(declare-function agent-repl--ws-main-name "workspace" ())

(defun agent-repl--pseudo-workspace-name-p (ws)
  "Return non-nil when WS names one of persp-mode's own built-in perspectives.
Those are `persp-nil-name' (\"none\", the perspective that is active outside
any perspective) and Doom's startup perspective (`+workspaces-main', read
through `agent-repl--ws-main-name').  Both are real, live perspectives that
agent-repl never created and that intentionally own no `:project-dir', so a
record attributed to one is not an anomaly — it is simply a record about
something that is not a workspace.

The classification is grounded in the two variables persp-mode and Doom
publish for their own perspectives rather than in string literals, so it
tracks a renamed built-in instead of drifting from it.  Neither variable being
bound means the built-in does not exist in this session, and nothing can be
that perspective.

This is deliberately NOT \"any name without a registered directory\": a real
agent-repl workspace whose registration is missing is exactly the anomaly
`agent-repl--note-unroutable-log-workspace' exists to shout about, and it must
keep shouting."
  (and (stringp ws)
       (let ((nil-name (and (boundp 'persp-nil-name) persp-nil-name))
             (main-name (and (fboundp 'agent-repl--ws-main-name)
                             (agent-repl--ws-main-name))))
         (or (and (stringp nil-name) (equal ws nil-name))
             (and (stringp main-name) (equal ws main-name))))))

(defvar agent-repl--log-canonicalizing-workspace nil
  "Non-nil while `agent-repl--log-canonical-workspace' is resolving a name.
The reverse lookup reads the workspace registry, and anything it touches
may itself log; the guard makes the resolution non-reentrant so one record
can never drive an unbounded chain of resolutions.")

(defun agent-repl--log-canonical-workspace (ws)
  "Return the registry key naming the same workspace as WS.

The registry (`agent-repl--workspaces') is keyed by workspace NAME, but a
log record can reach the ladder carrying that workspace's DIRECTORY
instead: a caller holding a worktree path — a daemon `cwd' field, a
`default-directory', a project root — is naming a real, live, registered
workspace under its other spelling.  Routing that record on the raw string
found it absent from the registry and warned that a LIVE workspace was
unroutable, while its records went to the global sink.

So the two spellings are reduced to the one the registry uses, here, at
the single point where attribution becomes a sink.  A path-shaped WS is
resolved through `agent-repl--path-canonical' — the same canonicalizer
every other workspace-key producer uses, so a symlinked worktree resolves
to the same entry — against each live workspace's `:project-dir'.

Anything else is returned unchanged: a name that is genuinely absent from
the registry is exactly the anomaly
`agent-repl--note-unroutable-log-workspace' exists to shout about, and it
must keep shouting."
  (if (or agent-repl--log-canonicalizing-workspace
          (not (stringp ws))
          (not (file-name-absolute-p ws))
          (not (boundp 'agent-repl--workspaces))
          (not (fboundp 'agent-repl--ws-get)))
      ws
    (let ((agent-repl--log-canonicalizing-workspace t))
      (or (let ((canon (ignore-errors (agent-repl--path-canonical ws)))
                (found nil))
            (when canon
              (maphash
               (lambda (name plist)
                 (unless found
                   (let ((dir (plist-get plist :project-dir)))
                     (when (and (stringp dir)
                                (equal canon
                                       (ignore-errors
                                         (agent-repl--path-canonical dir)))
                                (agent-repl--ws-log-routable-p name))
                       (setq found name)))))
               agent-repl--workspaces))
            found)
          ws))))

(defvar agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)
  "Workspace names already reported as unable to own a durable log sink.")

(defconst agent-repl--global-log-scope :agent-repl-global-log-scope
  "Explicit workspace argument for a record that is genuinely central.")

(defconst agent-repl--central-log-scope-tag :agent-repl-central
  "Tag introducing an explicitly reasoned central log scope.")

(defconst agent-repl--context-log-scope-tag :agent-repl-context
  "Tag introducing a reasoned context-derived log scope.")

(defun agent-repl--reasoned-log-scope-reason (scope tag)
  "Return SCOPE's nonempty reason when SCOPE is a well-formed TAG marker.
A reasoned marker is the quoted two-element list `(TAG REASON)'.  The reason
lives at the call site so source review and the static logging audit can tell
an intentional central record from omitted workspace attribution."
  (when (and (consp scope)
             (eq (car scope) tag)
             (consp (cdr scope))
             (null (cddr scope))
             (stringp (cadr scope))
             (not (string-empty-p (cadr scope))))
    (cadr scope)))

(defun agent-repl--central-log-scope-reason (scope)
  "Return SCOPE's reason when SCOPE explicitly requires the central sink."
  (agent-repl--reasoned-log-scope-reason
   scope agent-repl--central-log-scope-tag))

(defun agent-repl--context-log-scope-reason (scope)
  "Return SCOPE's reason when SCOPE derives workspace context when available.
The marker's reason explains why the same site is genuinely central when no
request edge, owning buffer, or current workspace supplies an identity."
  (agent-repl--reasoned-log-scope-reason
   scope agent-repl--context-log-scope-tag))

(defun agent-repl--capture-log-scope (scope)
  "Resolve SCOPE now and return a durable scope for an asynchronous callback.
The returned value is either the concrete workspace name or a reasoned central
marker.  Callers capture before registering callbacks so later process filters,
sentinels, and timers cannot inherit whichever buffer happens to be current
when Emacs eventually dispatches them.

This helper deliberately emits no record: it is part of the logging boundary,
and logging its own resolution would recurse through the same boundary."
  (let ((routing (agent-repl--resolve-log-workspace
                  scope "elisp.core.capture-log-scope")))
    (cond
     ((plist-get routing :workspace) (plist-get routing :workspace))
     ((plist-get routing :central)
      (list agent-repl--central-log-scope-tag (plist-get routing :central)))
     (t
      (error "agent-repl cannot capture log scope: offender=%S reason=%s"
             (plist-get routing :offender)
             (plist-get routing :reason))))))

(defvar agent-repl--log-context-workspace nil
  "Dynamically bound workspace for records emitted below an asynchronous edge.")

(defvar agent-repl--log-context-request-id nil
  "Dynamically bound request identifier for records below a request edge.")

(defvar agent-repl--log-request-sequence 0
  "Process-local sequence used to mint Emacs request correlation ids.")

(defun agent-repl--next-log-request-id ()
  "Return a fresh process-local request correlation id."
  (format "emacs-%d-%d-%d"
          (emacs-pid)
          (truncate (* 1000000 (float-time)))
          (cl-incf agent-repl--log-request-sequence)))

(defconst agent-repl--central-log-format-prefixes
  '(("Loading Agent-Repl" . "module bootstrap precedes every workspace")
    ("%s.el loaded" . "module loading is process-wide")
    ("FAILED to load" . "module loading is process-wide")
    ("early-git-string:" . "module version discovery is process-wide")
    ("version command:" . "the loaded module version is process-wide")
    ("version:" . "the loaded module version is process-wide")
    ("workspace-notes popup rule" . "module UI registration is process-wide")
    ("prevent-select: installed" . "module UI registration is process-wide")
    ("Loaded with" . "module loading is process-wide")
    ("Loaded Agent-Repl" . "module loading is process-wide")
    ("  " . "indented module-load diagnostics are process-wide")
    ("cold-start:" . "module startup policy is process-wide")
    ("register-timer:" . "the timer registry is process-wide")
    ("cancel-all-timers:" . "module reload clears the process-wide timer registry")
    ("assert-heartbeat-armed:" . "the process-wide heartbeat owns all workspaces")
    ("cancel-timer-key:" . "the timer registry is process-wide")
    ("data-dir:" . "the process data directory is shared")
    ("workspace-prefix:" . "workspace naming policy is process-wide")
    ("async-gh:" . "the generic GitHub subprocess boundary has no workspace")
    ("capture-process-output:" . "the generic subprocess boundary has no workspace")
    ("kill-process-safely:" . "generic process cleanup has no workspace")
    ("deferred-quit:" . "generic control-flow diagnostics have no workspace")
    ("assert-main-thread:" . "generic thread guards have no workspace")
    ("print-git-branch:" . "branch display can run outside a workspace")
    ("restore-focus:" . "focus restoration spans perspectives")
    ("tabline-advice:" . "tab rendering spans every workspace")
    ("migrate-legacy-state:" . "state migration is process-wide")
    ("elisp.daemon." . "the resident daemon lifecycle spans workspaces")
    ("elisp.link." . "the primary daemon link spans workspaces")
    ("elisp.connect." . "an unscoped transport exchange is process-wide")
    ("elisp.commands.change-spec" . "macro expansion has no runtime workspace")
    ("elisp.commands.add-project" . "project registration precedes workspace creation")
    ("elisp.host." . "host registration and selection span workspace lifecycles")
    ("elisp.input.no-workspace" . "the record reports that no workspace exists")
    ("elisp.input.attach-outside-composer" . "the record reports a non-workspace buffer")
    ("elisp.notes.dir:" . "the notes directory is shared storage")
    ("elisp.notes.file: rejected" . "the record reports that no valid workspace was supplied")
    ("elisp.notes.open: rejected" . "the record reports that no current workspace exists")
    ("elisp.notes.popup-predicate" . "popup classification is process-wide")
    ("elisp.popup." . "the generic popup utility is path-scoped")
    ("elisp.status.tab-color: unknown" . "the shared status palette is not workspace-specific")
    ("elisp.verbs." . "workspace-management verbs can precede workspace creation")
    ("elisp.worktree." . "generic worktree utilities are path-scoped")
    ("elisp.rpc." . "an unscoped RPC exchange is process-wide")
    ("elisp.wire." . "an unscoped codec operation has no workspace")
    ("elisp.roster." . "the roster stream is a whole-process view")
    ("frontend-register:" . "frontend registration is process-wide")
    ("find-file-workspace:" . "file routing precedes workspace selection")
    ("interaction-record:" . "interaction capture spans workspaces")
    ("interaction-replay:" . "interaction replay spans workspaces")
    ("elisp.core." . "logging controls and routing are process-wide")
    ("read-sexp-file:" . "shared state-file IO has no workspace")
    ("read-sexp-file-if-exists:" . "shared state-file IO has no workspace")
    ("write-sexp-file:" . "shared state-file IO has no workspace")
    ("migrate-saved-state:" . "saved-state migration spans workspaces")
    ("history-load:" . "history loading precedes workspace restoration")
    ("history-file" . "history storage is process-wide")
    ("autosave:" . "the autosave scheduler spans every workspace")
    ("autosave-workspace-buffers:" . "the autosave sweep spans every workspace")
    ("WARN: autosave" . "the autosave sweep spans every workspace")
    ("state-load:" . "state loading precedes workspace restoration")
    ("state-save:" . "the saved workspace registry is process-wide")
    ("state-file" . "workspace registry storage is process-wide")
    ("after-persp-activated:" . "foreign perspective activation has no agent workspace")
    ("before-persp-deactivate:" . "perspective deactivation can precede workspace selection")
    ("on-window-change" . "frame-wide window reconciliation can run without an agent workspace")
    ("sync-panels" . "frame-wide orphan-panel reconciliation spans workspaces")
    ("frontend webview adopt-hook failed" . "the frontend hook resolves no safe workspace")
    ("instantiation-to-plist:" . "pure instantiation conversion has no workspace")
    ("elisp.services." . "service discovery and launch are process-wide")
    ("services-" . "service discovery and launch are process-wide")
    ("invalid launchctl verb " . "service control is process-wide")
    ("launchd service " . "service control is process-wide")
    ("shim service build completed " . "service build validation is process-wide")
    ("select-notification-backend:" . "notification backend selection is process-wide")
    ("select-notification-backend " . "notification backend selection is process-wide")
    ("prompt-summary: attach-all" . "the attach-all command spans workspaces")
    ("prompt-summary: kickoff skipped reason=no-workspace"
     . "the record reports that no workspace was supplied")
    ("elisp.webview-recovery." . "webview recovery spans workspaces")
    ("load-priority-images:" . "shared image loading is process-wide")
    ("ws-rewrite-source-back-refs:" . "source-reference migration spans workspaces")
    ("ws-put:" . "workspace registry validation can precede sink ownership")
    ("ws-by-ref-id:" . "reverse workspace lookup can return no workspace")
    ("ws-persp-identity:" . "the record reports a non-workspace perspective")
    ("ws-registered-names:" . "workspace registry enumeration spans workspaces")
    ("ws-switch-project-display:" . "project display can precede activation")
    ("ws-tabline-names:" . "tab rendering spans every workspace")
    ("window--ensure-layout:" . "the record reports that no workspace exists")
    ("%s error:" . "module loader error reporting is process-wide"))
  "Reasoned allowlist of format prefixes that may use the central log sink.
Explicit or dynamically bound workspace scope always wins.  A nil-workspace
site not covered here must resolve through its buffer or current workspace;
otherwise the record is a routing invariant violation.")

(defun agent-repl--central-log-reason (fmt)
  "Return the reason FMT is genuinely central, or nil when it is workspace-owned."
  (when (stringp fmt)
    (cl-loop for (prefix . reason) in agent-repl--central-log-format-prefixes
             when (string-prefix-p prefix fmt)
             return reason)))

(defun agent-repl--with-log-context (workspace request-id function)
  "Call FUNCTION with WORKSPACE and REQUEST-ID available to nested log calls."
  (let ((agent-repl--log-context-workspace workspace)
        (agent-repl--log-context-request-id request-id))
    (funcall function)))

(defun agent-repl--note-unroutable-log-workspace (ws)
  "Display a user-visible warning that WS cannot own a durable log sink."
  (unless (gethash ws agent-repl--unroutable-log-workspaces)
    (puthash ws t agent-repl--unroutable-log-workspaces)
    (let ((dir (and (fboundp 'agent-repl--ws-get)
                    (agent-repl--ws-get ws :project-dir))))
      (display-warning
       'agent-repl
       (format "log routing failed for workspace %S (registered-dir=%s); the original record was not written"
               ws
               (cond ((not (stringp dir)) "unregistered")
                     ((file-directory-p dir) dir)
                     (t (format "%s [MISSING]" dir))))
       :error))))

(defconst agent-repl--central-log-fallback-format
  "elisp.core.log-central-fallback workspace=%S class=%s reason=%s"
  "Format of the once-per-workspace record announcing a central fallback.")

(defvar agent-repl--departed-log-workspaces (make-hash-table :test #'equal)
  "Names of workspaces whose departure THIS EDITOR ordered.

A workspace does not leave all at once.  The editor asks for the nuke, the
daemon removes the worktree and answers, and only then are the tab, the
streams, the buffers and finally the registry row taken down.  Records go on
being written for the whole of that window — a roster reconcile, a stream
cancellation, a panel switch — and each of them is attributed to a workspace
whose registered directory is ALREADY gone from disk.

Read from the registration alone, that is indistinguishable from a registry
row that outlived its worktree, and it was read that way: on 2026-09-13 the
delete in realtest 5 raised the `stale-registration' popup for
`workspace-d4841171eaf540f4' 49ms before its own tab teardown, and a link
teardown raised another for `scratch-repo' half an hour after that workspace
had left the roster.  Both popups named a workspace the user had already
forgotten, and neither named anything the user could act on.

So the ORDER IS REMEMBERED.  A directory that is missing BECAUSE the editor
asked for the workspace to go is an ordinary consequence of that order; a
directory that is missing with NO order behind it is the stale row worth
interrupting for.  The name is forgotten again the moment a workspace
registers a directory under it, so a recreated workspace starts from the
same clean slate as a fresh one.")

(defun agent-repl--log-note-workspace-departing (ws)
  "Remember that this editor ordered WS's departure.
Called where the departure is ORDERED and again where it is CARRIED OUT:
the destructive verb's answer can be preceded by records the daemon's own
worktree removal has already made unroutable."
  (when (stringp ws)
    (puthash ws t agent-repl--departed-log-workspaces)))

(defun agent-repl--log-forget-workspace-departure (ws)
  "Forget an order to depart that WS did not obey.
A REFUSED destructive verb leaves the workspace standing with its worktree
intact, so the order must not go on excusing a directory that later goes
missing on its own."
  (when (stringp ws)
    (remhash ws agent-repl--departed-log-workspaces)))

(defun agent-repl--log-note-workspace-registered (ws)
  "Forget WS's departure and its spent fallback claim: the name is live again.
A name is reusable, and a workspace registering a directory under one is a
new workspace whatever became of the last: it is owed the same single
announcement, and the same popup, as any other."
  (when (stringp ws)
    (remhash ws agent-repl--departed-log-workspaces)
    (remhash ws agent-repl--unroutable-log-workspaces)))

(defun agent-repl--log-workspace-departing-p (ws)
  "Return non-nil when this editor has ordered WS's departure."
  (and (stringp ws)
       (gethash ws agent-repl--departed-log-workspaces)
       t))

(declare-function agent-repl-roster-row-for-ws "agent-repl-roster" (ws))
(declare-function agent-repl-roster-row-status "agent-repl-roster" (row))
(declare-function agent-repl-roster-row-closed-p "agent-repl-roster" (row))
(declare-function agent-repl-host--live-composer-arm "agent-repl-host" (ws))

(defun agent-repl--log-workspace-merge-departing-p (ws)
  "Return non-nil when the daemon has told this editor a merge is taking WS.
A merge that lands closes its workspace and removes its worktree, and it
may have been asked for anywhere -- this editor, the webapp's sidebar, an
agent's command file.  Its removal therefore reaches this editor as a
directory that is simply gone, ahead of the roster push that closes the
row (measured 2026-10-06: worktree removed at .325, a record at .340 found
it gone, the closed row arrived at .383).  What the daemon HAS already
said by then is that the merge is under way: WS's roster row is `:merging'
or `:merged' from the merge's admission on, its host composer is
`:merging', and the closed row follows.  Any of those makes the missing
directory the merge's own work.  Read through roster.el and host.el only
when they are loaded; core.el is below both."
  (or (and (fboundp 'agent-repl-roster-row-for-ws)
           (let ((row (agent-repl-roster-row-for-ws ws)))
             (and row
                  (or (memq (agent-repl-roster-row-status row) '(:merging :merged))
                      (agent-repl-roster-row-closed-p row))
                  t)))
      (and (fboundp 'agent-repl-host--live-composer-arm)
           (eq (agent-repl-host--live-composer-arm ws) :merging))))

(defun agent-repl--central-log-fallback-class (ws)
  "Classify WHY WS's records fall back to the central sink.

FOUR DIFFERENT FACTS ARRIVE HERE, and conflating them is what put a popup in
front of the user for a condition that is not a defect:

  - `no-durable-home' — WS names no registered directory at all, or names a
    directory that still EXISTS but cannot host a sink of its own (a scratch
    or temporary path, a workspace still inside its registration window).
    Nothing is broken.  The workspace simply has nowhere of its own to write,
    its records are written centrally carrying its name, and the user has
    nothing to do about it.  ORDINARY.

  - `ordered-departure' — the editor ASKED for WS to go, and the directory
    went.  The trailing records of a departure the user themselves ordered
    are the ordinary sound of that departure finishing; see
    `agent-repl--departed-log-workspaces'.  ORDINARY.

  - `merge-departure' — a merge the daemon reported as under way for WS
    (`agent-repl--log-workspace-merge-departing-p') removed its worktree.
    The merge is a departure the user asked for, from wherever they asked.
    ORDINARY.

  - `stale-registration' — WS IS registered, the directory it is registered
    AT IS GONE, and NOTHING ASKED FOR IT TO GO.  That is a registry row that
    outlived its worktree: a real inconsistency in durable state, which
    writing centrally hides rather than repairs.  It is the one case worth
    interrupting for.

The departure is checked FIRST, because it explains the missing directory
that would otherwise read as the anomaly.  All three are facts about WS
rather than about whichever record happened to meet it."
  (let ((dir (and (fboundp 'agent-repl--ws-get)
                  (agent-repl--ws-get ws :project-dir))))
    (cond
     ((agent-repl--log-workspace-departing-p ws) 'ordered-departure)
     ((agent-repl--log-workspace-merge-departing-p ws) 'merge-departure)
     ((and (stringp dir) (not (file-directory-p dir))) 'stale-registration)
     (t 'no-durable-home))))

(defun agent-repl--claim-central-log-fallback (ws)
  "Claim the ONE announcement that WS's records go to the central sink.

Returns WS's `agent-repl--central-log-fallback-class' exactly once per WS and
nil thereafter.  The condition belongs to the WORKSPACE rather than to the
record that met it: announcing it per record is what turned a single tab
render into a screenful of identical lines.

THE USER-VISIBLE WARNING IS RAISED ONLY FOR `stale-registration'.  A
workspace that simply owns no durable sink, and a workspace whose departure
the editor itself ordered, are ORDINARY conditions and get no popup at all —
each is recorded, once, and that is the whole of it.  A REGISTERED directory
that is MISSING with nothing having asked for it to go is not ordinary, so it
keeps the popup.

The record itself is built and written by `agent-repl--emit-log-record', the
one builder and writer of records, which is also the only place that can do
so without re-entering the ladder it runs inside."
  (unless (gethash ws agent-repl--unroutable-log-workspaces)
    (puthash ws t agent-repl--unroutable-log-workspaces)
    (let ((class (agent-repl--central-log-fallback-class ws)))
      (when (eq class 'stale-registration)
        (display-warning
         'agent-repl
         (format "workspace %S cannot host a durable log sink (registered-dir=%s); its records are written centrally"
                 ws
                 (format "%s [MISSING]"
                         (agent-repl--ws-get ws :project-dir)))
         :warning))
      class)))

(defvar agent-repl--log-preregistration-workspace nil
  "Name of the workspace currently inside its own registration window.

A workspace is CREATED before it is REGISTERED, and the code doing the
creating legitimately logs throughout: the announcement arriving, the
payload validating, the conflict checks that must all pass BEFORE the
single `puthash' that commits the bookkeeping.  Every one of those
records is correctly attributed to a workspace that does not own a sink
yet, so every one of them reached
`agent-repl--note-unroutable-log-workspace' and made a normal creation
warn about its own normal prologue.

Registering earlier is not the fix here — unlike
`agent-repl--establish-workspace', which genuinely did register too late
and now registers first, the creation path's `puthash' IS the commit
point, and moving it ahead of the conflict checks would publish a
half-materialized workspace to every observer.

So the window is DECLARED instead, by the one function that knows it is open,
and only for the one name it is opening.  Panel ownership uses this marker to
distinguish a workspace being created from a foreign perspective.  Logging is
deliberately stricter: a record emitted before registration has no durable
workspace sink and aborts rather than being rerouted.")

(defun agent-repl--preregistration-log-workspace-p (ws)
  "Return non-nil when WS is the workspace whose registration window is open."
  (and (stringp ws)
       (stringp agent-repl--log-preregistration-workspace)
       (equal ws agent-repl--log-preregistration-workspace)))

(defun agent-repl--log-sink-workspace (ws)
  "Return the registered sink workspace for explicit WS.
Nil and `agent-repl--global-log-scope' name the central sink.  A pseudo
perspective is central by definition.  Every other unroutable non-nil value is
an invariant violation and signals; it is never rewritten to the central sink."
  (cond
   ((or (null ws) (eq ws agent-repl--global-log-scope)) nil)
   ((agent-repl--ws-log-routable-p ws) ws)
   ;; WS may be the registry's other spelling of a live workspace — its
   ;; worktree path rather than its name.  Reduce both spellings to the
   ;; registry's key here, before the absence is treated as an anomaly, so a
   ;; registered workspace is never unroutable under its alternate spelling.
   ((let ((canonical (agent-repl--log-canonical-workspace ws)))
      (and (not (equal canonical ws))
           (agent-repl--ws-log-routable-p canonical)
           canonical)))
   ((agent-repl--pseudo-workspace-name-p ws) nil)
   (t
    (error "agent-repl log routing invariant violated: workspace %S has no durable sink" ws))))

(defun agent-repl--log-scope-candidate (ws fmt)
  "Return the scope WS names for FMT before any sink is resolved.
The answer is `agent-repl--global-log-scope' for a central record, a
workspace name, or nil when nothing attributes the record at all.  Explicit
scope wins, followed by the dynamically bound request edge, the current
buffer's owner, the current workspace, and finally an explicit reasoned
context marker or the legacy reasoned central registry.

This is the ONE ordering both `agent-repl--resolve-log-workspace' and the
dropped-record fast path of `agent-repl--emit-log-record' read, so the fast
path can never call a record central that routing would have attributed."
  (let ((context-central-reason (agent-repl--context-log-scope-reason ws)))
    (cond
     ((agent-repl--central-log-scope-reason ws) agent-repl--global-log-scope)
     ((eq ws agent-repl--global-log-scope) agent-repl--global-log-scope)
     ((and ws (not context-central-reason)) ws)
     (agent-repl--log-context-workspace agent-repl--log-context-workspace)
     ((agent-repl--buffer-owner (current-buffer)))
     ((and (fboundp 'agent-repl--ws-current-log-name)
           (agent-repl--ws-current-log-name)))
     (context-central-reason agent-repl--global-log-scope)
     ((agent-repl--central-log-reason fmt) agent-repl--global-log-scope)
     (t nil))))

(defun agent-repl--resolve-log-workspace (ws fmt)
  "Resolve WS for FMT to a sink workspace, central nil, or a routing error.
The return value is `(:workspace NAME)', `(:central REASON)', or a plist with
`:routing-error', `:offender', and `:reason'.  The scope is chosen by
`agent-repl--log-scope-candidate'."
  (let* ((explicit-central-reason
          (agent-repl--central-log-scope-reason ws))
         (context-central-reason
          (agent-repl--context-log-scope-reason ws))
         (candidate (agent-repl--log-scope-candidate ws fmt)))
    (cond
     ((eq candidate agent-repl--global-log-scope)
      (list :central (or explicit-central-reason
                         context-central-reason
                         (agent-repl--central-log-reason fmt)
                         "explicit central request context")))
     ((null candidate)
      (list :routing-error t :offender nil
            :reason "no explicit, request, buffer, or current-workspace scope"))
     ((agent-repl--pseudo-workspace-name-p candidate)
      (list :central "perspective is not an agent-repl workspace" :pseudo candidate))
     (t
      (condition-case err
          (list :workspace (agent-repl--log-sink-workspace candidate))
        ;; A NAMED WORKSPACE THAT OWNS NO SINK IS NOT A ROUTING FAILURE.
        ;; Resolving a workspace's sink is a TOTAL function: a workspace whose
        ;; own directory cannot host one — a scratch or temporary path, a
        ;; worktree that has been deleted, a directory that does not exist yet
        ;; — resolves to the CENTRAL sink with the workspace preserved on the
        ;; record as `unroutable_workspace'.  The condition is announced ONCE
        ;; per workspace; announcing it per RECORD is what turned one tab
        ;; render of one such workspace into 22 ERROR lines.
        ;;
        ;; A routing failure with NO offender is a different fact and keeps
        ;; its ERROR: nothing named the workspace at all, which is missing
        ;; attribution at the call site rather than an unavailable directory.
        (error (list :central "the workspace owns no durable log sink"
                     :unroutable candidate
                     :reason (error-message-string err))))))))

(defun agent-repl--workspace-log-identity (ws)
  "Return WS's registered canonical directory and stable workspace identity.
The logging boundary deliberately refuses to derive either value from ambient
state because a non-nil WS must identify one specific workspace sink.

Callers that cannot guarantee WS names a registered workspace must screen it
through `agent-repl--ws-log-routable-p' first and pass nil when it does not."
  (let* ((dir (and (fboundp 'agent-repl--ws-get)
                   (agent-repl--ws-get ws :project-dir)))
         (canonical (and (stringp dir) (agent-repl--log-dir-truename dir))))
    (unless canonical
      (error "agent-repl log routing invariant violated: workspace %S has no registered project directory" ws))
    (list :project-dir canonical
          ;; The directory hash is what this runtime can always derive, so it
          ;; is what the sink is keyed and named by.  `:workspace-id' is the
          ;; daemon's, so it is nil until the roster push carries it here.
          :workspace-dir-hash (or (agent-repl--ws-dir-hash-cached ws)
                                  (error "agent-repl log routing invariant violated: workspace %S has no workspace directory hash" ws))
          :workspace-id (agent-repl--ws-daemon-log-id ws))))

(defun agent-repl--workspace-log-session-ids (ws)
  "Return WS's known session identifiers as a validated alist.
Each entry is (FIELD . VALUE) for a nonempty string VALUE; an unknown
identifier is omitted and any other value signals the identity invariant.
`agent_repl_session_id' is the daemon session echo token and
`claude_session_id' is the vendor conversation uuid."
  (let (fields)
    (dolist (field-value
             `(("agent_repl_session_id" . ,(agent-repl--ws-observed-agent-repl-session-id ws))
               ("claude_session_id" . ,(agent-repl--ws-observed-claude-session-id ws))))
      (let ((field (car field-value))
            (value (cdr field-value)))
        (cond
         ((null value))
         ((and (stringp value) (not (string-empty-p value)))
          (push (cons field value) fields))
         (t
          (error "agent-repl log routing invariant violated: workspace %S has invalid %s: %S"
                 ws field value)))))
    (nreverse fields)))

(defun agent-repl--log-context-request-id-checked ()
  "Return the bound request id, nil when none, signalling on an invalid one."
  (when agent-repl--log-context-request-id
    (unless (and (stringp agent-repl--log-context-request-id)
                 (not (string-empty-p agent-repl--log-context-request-id)))
      (error "agent-repl log identity invariant violated: invalid request_id=%S"
             agent-repl--log-context-request-id))
    agent-repl--log-context-request-id))

(defun agent-repl--log-check-identity (ws)
  "Signal exactly as `agent-repl--log-record' would for WS, building nothing.
A record that will be neither persisted nor shown is not built, but the
identity invariants its builder enforces still hold for it: a workspace
without a registered directory or directory hash, an invalid session id and
an invalid bound request id signal here just as they do while building."
  (when ws
    (agent-repl--workspace-log-identity ws)
    (agent-repl--workspace-log-session-ids ws))
  (agent-repl--log-context-request-id-checked))

(defun agent-repl--log-add-workspace-identity (record ws)
  "Add WS identity and its known session identifiers to JSON RECORD.
A nil WS adds nothing.  The session identifiers come from
`agent-repl--workspace-log-session-ids'."
  (when ws
    (let ((identity (agent-repl--workspace-log-identity ws))
          (context (gethash "context" record)))
      (puthash "workspace_dir" (plist-get identity :project-dir) record)
      ;; The daemon mints `workspace_id' and the roster push delivers it.  A
      ;; record written before that push OMITS the field rather than stamping
      ;; the directory hash in its place: a harvest groups by `workspace_id',
      ;; and a path-derived stand-in would split one workspace into two groups
      ;; and put the Emacs half in neither runtime's.  Absent, the record
      ;; still routes by `workspace_dir'.
      (when-let ((id (plist-get identity :workspace-id)))
        (puthash "workspace_id" id record))
      (when (hash-table-p context)
        (puthash "workspace_dir_hash" (plist-get identity :workspace-dir-hash)
                 context))
      (pcase-dolist (`(,field . ,value) (agent-repl--workspace-log-session-ids ws))
        (puthash field value record))))
  record)

(defun agent-repl--log-record (ws level verbosity fmt args
                                  &optional pseudo-ws operation-fmt unroutable-ws)
  "Serialize WS / LEVEL / VERBOSITY / FMT / ARGS as one JSONL record.
PSEUDO-WS, when non-nil, is the persp-mode pseudo-perspective the caller
attributed this record to (see `agent-repl--pseudo-workspace-name-p').  Such a
name owns no durable sink, so WS is nil and the record lands globally; the name
is preserved on the record as `pseudo_workspace' so the line still says which
perspective it is about.

UNROUTABLE-WS, when non-nil, is the workspace the caller attributed this
record to which is NOT a registered sink and not a pseudo perspective — an
invariant violation the routing-error record beside this one reports.  The
record still lands globally rather than being dropped, and the name is
preserved as `unroutable_workspace' for the same reason `pseudo_workspace'
is: the line must still say which workspace it is about.

OPERATION-FMT, when non-nil, is the format string the stable `operation'
name is derived from, INSTEAD of FMT.  The severity rungs
(`agent-repl--warn', `agent-repl--error') prepend a display tag to FMT so
the recorded message reads \"WARNING: ...\"; deriving `operation' from that
tagged string would fold the severity into the operation name and give the
same logical operation two names depending on which rung logged it.
logging-contract.md reserves `level' for severity and requires `operation'
to be stable, so the BARE format string travels here separately."
  (let* ((message (if (stringp fmt)
                      (apply #'format fmt args)
                    (agent-repl--log-format-capture-bug fmt)
                    (format "[BUG non-string-fmt=%S]" fmt)))
         (context (agent-repl--json-object
                   (cons "format" (if (stringp fmt) fmt (format "%S" fmt)))
                   (cons "arguments" (vconcat (mapcar #'prin1-to-string args))))
                   )
         (record (agent-repl--json-object
                  (cons "timestamp" (agent-repl--log-rfc3339-timestamp))
                  (cons "runtime" "emacs")
                  (cons "pid" (emacs-pid))
                  (cons "level" level)
                  (cons "verbosity" verbosity)
                  (cons "operation" (agent-repl--log-operation
                                     (or operation-fmt fmt)))
                  (cons "message" message)
                  (cons "context" context))))
    (agent-repl--log-add-workspace-identity record ws)
    (when-let ((request-id (agent-repl--log-context-request-id-checked)))
      (puthash "request_id" request-id record))
    (when pseudo-ws
      (puthash "pseudo_workspace" pseudo-ws record))
    (when unroutable-ws
      (puthash "unroutable_workspace" unroutable-ws record))
    (json-serialize record)))

(defun agent-repl--workspace-emacs-log-path (project-dir)
  "Return the canonical `emacs.log' symlink path below PROJECT-DIR."
  (expand-file-name ".claude/emacs/emacs.log" project-dir))

(defun agent-repl--ensure-real-log-directory (path)
  "Ensure PATH is a real directory without following a hostile symlink."
  ;; A trailing slash makes Emacs resolve a symlink before `file-symlink-p'
  ;; sees it, so normalize before every safety check.
  (let ((component (directory-file-name path)))
    (when (file-symlink-p component)
      (error "agent-repl log routing invariant violated: directory component is a symlink: %s" component))
    (cond
     ((file-exists-p component)
      (unless (file-directory-p component)
        (error "agent-repl log routing invariant violated: directory component is not a directory: %s" component)))
     (t
      (make-directory component)))
    ;; Recheck after creation because the path is workspace-controlled.
    (when (or (file-symlink-p component) (not (file-directory-p component)))
      (error "agent-repl log routing invariant violated: unsafe directory component: %s" component))
    component))

(defun agent-repl--install-workspace-log-link (canonical target)
  "Atomically point CANONICAL at TARGET, replacing whatever link is there.
CANONICAL's directory is proven real before anything is created, so a
hostile parent leaves no artifact behind.  Reserving a temporary name
first gives a collision-proof link name; removing that reservation
immediately before a non-overwriting `make-symbolic-link' makes an
interloper cause failure rather than silent reuse, and `rename-file'
makes the canonical-link replacement atomic.

Never logs: this runs inside the file sink (see
`agent-repl--do-log-to-file'), so the logging ladder would re-enter it."
  (let ((canonical-dir (file-name-directory canonical))
        (link-tmp nil))
    (agent-repl--ensure-real-log-directory canonical-dir)
    (when (file-directory-p canonical)
      (error "agent-repl log routing invariant violated: canonical log path is a directory: %s" canonical))
    (unwind-protect
        (progn
          (setq link-tmp (make-temp-file
                          (expand-file-name ".emacs.log-link-" canonical-dir)))
          (delete-file link-tmp)
          (make-symbolic-link target link-tmp)
          (rename-file link-tmp canonical t)
          (setq link-tmp nil)
          target)
      (when (and link-tmp
                 (or (file-exists-p link-tmp) (file-symlink-p link-tmp)))
        (delete-file link-tmp)))))

(defun agent-repl--workspace-log-link-current-p (canonical target)
  "Return non-nil when CANONICAL is a symlink naming TARGET."
  (equal (file-symlink-p canonical) target))

(defun agent-repl--emacs-log-target-directory ()
  "Return agent-repl's own durable directory for Emacs log targets.
`~/.claude-emacs/logs/' — a STABLE directory, the same one the daemon keeps
its targets in, never the OS temporary root.  A durable log a person is
asked to read must not live where the operating system may sweep it, must
not move with a per-launcher TMPDIR, and must not accumulate one orphan per
Emacs instance in a directory nothing owns.  Creates nothing."
  (file-name-as-directory (agent-repl--global-state-file "logs")))

(defun agent-repl--emacs-log-owned-target-p (path)
  "Return non-nil when PATH is a target THIS MODULE could have minted.
Two shapes qualify: a file directly inside
`agent-repl--emacs-log-target-directory', which is where targets are minted
now, and a file whose basename carries
`agent-repl--emacs-log-target-prefix', which is what every target minted
into the OS temporary root before this change looks like.  The second shape
is why MIGRATION IS BY REUSE: nothing is renamed or copied, the old target
keeps being appended to for as long as a canonical link names it, and only
a genuinely new target gets the durable-directory name."
  (and (stringp path)
       (file-name-absolute-p path)
       (or (equal (directory-file-name (file-name-directory path))
                  (directory-file-name (agent-repl--emacs-log-target-directory)))
           (string-prefix-p agent-repl--emacs-log-target-prefix
                            (file-name-nondirectory path)))))

(defun agent-repl--emacs-log-standing-target (canonical)
  "Return the target CANONICAL already names and may be appended to, else nil.
A NEW EMACS INSTANCE JOINS THE FILE THE LAST ONE WROTE.  Minting a fresh
target per instance and retargeting the link cost realtest 1 its whole
verdict on 2026-09-11: the harvest had resolved the link before the instance
under test started, so every record proving the tabs were drawn went to a
file no reader was looking at, and 22,112 orphaned targets had piled up in
the temporary root behind the same behaviour.

The three things that make a target ours are all checked.  CANONICAL must be
a SYMLINK — a regular file or foreign symlink the workspace put there is
displaced, never written to — it must name a regular file this module could
have minted \(`agent-repl--emacs-log-owned-target-p'), and that file must be
under the cap, because a target at the cap is a generation to roll rather
than one to join.  Anything else answers nil, which is the caller's signal
to mint.

Never logs: this runs inside the file sink."
  (let ((dest (file-symlink-p canonical)))
    (and (stringp dest)
         (agent-repl--emacs-log-owned-target-p dest)
         (file-regular-p dest)
         (not (file-symlink-p dest))
         (< (or (file-attribute-size (file-attributes dest)) 0)
            agent-repl-log-size-cap-bytes)
         dest)))

(defun agent-repl--emacs-log-mint-target (identity)
  "Create and return a fresh durable log target for IDENTITY.
THE NAME CARRIES THE DAEMON-MINTED 16-hex `workspace_id' when the roster
push has delivered it, the same identity every runtime's records carry and
the same naming the daemon's own targets use.  Before that push the
directory hash names the target instead: the workspace still needs a sink,
and the hash is the only identity this runtime can always derive.

Never logs: this runs inside the file sink."
  (let* ((dir (agent-repl--emacs-log-target-directory))
         (id (or (plist-get identity :workspace-id)
                 (plist-get identity :workspace-dir-hash))))
    (unless (and (stringp id) (not (string-empty-p id)))
      (error "agent-repl log routing invariant violated: no identity to name a log target: %S"
             identity))
    (make-directory dir t)
    (agent-repl--ensure-real-log-directory dir)
    (let ((target (make-temp-file
                   (expand-file-name (format "agent-repl-%s-emacs-" id) dir)
                   nil ".log")))
      target)))

(defvar agent-repl--pending-sink-opened nil
  "Sink-open decisions awaiting their INFO record.
The decision is made inside the file sink, where the logging ladder cannot
be re-entered, so the record is emitted from an idle timer once the target
is installed and logging about it no longer recurses.")

(defun agent-repl--flush-sink-opened-records ()
  "Emit the INFO record for every pending sink-open decision.
Mirrors the daemon's `daemon.dlog.sink_opened' shape: which target was
opened and whether it was the workspace's STANDING target or one this
instance MINTED.  A reader that meets an old temporary-root target beside a
newer durable one can tell from the log which is which."
  (let ((pending (prog1 agent-repl--pending-sink-opened
                   (setq agent-repl--pending-sink-opened nil))))
    (dolist (entry (nreverse pending))
      (let ((ws (plist-get entry :ws)))
        ;; The sink was installed for this workspace a moment ago, so it is
        ;; routable; a name that stopped being routable in between still gets
        ;; its decision recorded, centrally and at WARN, rather than dropped.
        (if (agent-repl--ws-log-routable-p ws)
            (agent-repl--info
             ws
             "elisp.core.sink-opened: sink=%s target=%s target_origin=%s id_scheme=%s"
             "emacs.log"
             (plist-get entry :target)
             (plist-get entry :origin)
             (plist-get entry :id-scheme))
          (agent-repl--warn
           '(:agent-repl-central "the sink-open decision outlived its workspace's routability")
           "elisp.core.sink-opened-unattributed: workspace=%S sink=%s target=%s target_origin=%s id_scheme=%s"
           ws "emacs.log"
           (plist-get entry :target)
           (plist-get entry :origin)
           (plist-get entry :id-scheme)))))))

(defun agent-repl--note-sink-opened (ws target origin identity)
  "Queue the INFO record for WS's sink open of TARGET with ORIGIN.
Never logs directly: the caller is the file sink."
  (push (list :ws ws :target target :origin origin
              :id-scheme (if (plist-get identity :workspace-id)
                             "daemon_minted_workspace_id"
                           "workspace_dir_hash"))
        agent-repl--pending-sink-opened)
  (run-with-idle-timer 0 nil #'agent-repl--flush-sink-opened-records))

(defun agent-repl--workspace-emacs-log-target (ws)
  "Return WS's runtime-owned external target and atomically install its link.
WS must have a registered project directory.  Workspace-controlled paths are
never opened for writing: the durable target lives in
`agent-repl--emacs-log-target-directory' and the workspace path is only an
atomic symlink.

On the first open of this runtime the workspace's STANDING target is joined
when the canonical link names one \(`agent-repl--emacs-log-standing-target');
a new target is minted only when there is none, it is not ours, or it is at
the cap.

The registry is keyed by WS's IDENTITY rather than by WS, so every name
that resolves to one directory shares that directory's single target and
its single canonical link — see `agent-repl--workspace-log-targets' for
the day of invisible records that keying by name cost."
  (let* ((identity (agent-repl--workspace-log-identity ws))
         (key (agent-repl--workspace-log-target-key identity))
         (cached (gethash key agent-repl--workspace-log-targets)))
    (if cached
        (let ((target (plist-get cached :target)))
          (unless (and (equal (plist-get cached :project-dir) (plist-get identity :project-dir))
                       (equal (plist-get cached :workspace-dir-hash)
                              (plist-get identity :workspace-dir-hash)))
            (error "agent-repl log routing invariant violated: workspace %S retained a target after identity rebinding" ws))
          (unless (file-regular-p target)
            (error "agent-repl log routing invariant violated: owned target vanished: %s" target))
          ;; THE LINK IS PART OF THE OWNERSHIP, not a one-time side effect of
          ;; minting the target.  Everything that reads a workspace's records
          ;; -- an operator, the log reader, the integration harness -- reaches
          ;; them ONLY through the canonical path, so a link that stops naming
          ;; the owned target makes every record written afterwards invisible
          ;; while the sink reports success.  Anything can unseat it: another
          ;; Emacs runtime registering the same directory, a `.claude' tree
          ;; restored from a backup, a stray `rm'.  Re-establishing it on the
          ;; reuse path is what makes "written" and "findable" the same fact.
          (unless (agent-repl--workspace-log-link-current-p
                   (agent-repl--workspace-emacs-log-path
                    (plist-get identity :project-dir))
                   target)
            (agent-repl--install-workspace-log-link
             (agent-repl--workspace-emacs-log-path (plist-get identity :project-dir))
             target))
          target)
      (let* ((project-dir (plist-get identity :project-dir))
             (canonical (agent-repl--workspace-emacs-log-path project-dir)))
        ;; The workspace path stays untrusted across runtimes: the standing
        ;; target is joined ONLY when the canonical path is a symlink naming a
        ;; regular file this module could have minted and under the cap, so a
        ;; regular file or foreign symlink a workspace plants there is still
        ;; displaced rather than written into.
        (agent-repl--ensure-real-log-directory (expand-file-name ".claude" project-dir))
        (agent-repl--ensure-real-log-directory (file-name-directory canonical))
        (when (file-directory-p canonical)
          (error "agent-repl log routing invariant violated: canonical log path is a directory: %s" canonical))
        ;; Target creation happens only after both workspace-controlled
        ;; directory components are proven real, so a hostile parent leaves no
        ;; runtime-owned temporary artifact behind.
        (let ((standing (agent-repl--emacs-log-standing-target canonical)))
          (if standing
              ;; THE STANDING TARGET IS JOINED, NOT DISPLACED: the link keeps
              ;; naming the file the previous instance wrote, so one file spans
              ;; instances, a reader that resolved the link before this
              ;; instance started still reads this instance's records, and a
              ;; bounce loop cannot evict history by restarting.
              (progn
                (puthash key (append (list :target standing) identity)
                         agent-repl--workspace-log-targets)
                (agent-repl--note-sink-opened ws standing "standing_canonical_link" identity)
                standing)
            (let ((target (agent-repl--emacs-log-mint-target identity))
                  (installed nil))
              (unwind-protect
                  (progn
                    (agent-repl--install-workspace-log-link canonical target)
                    (puthash key (append (list :target target) identity)
                             agent-repl--workspace-log-targets)
                    (setq installed t)
                    (agent-repl--note-sink-opened ws target "minted_target" identity)
                    target)
                (unless installed
                  (when (file-exists-p target)
                    (delete-file target)))))))))))

;;; Orphaned log-target sweep

(defcustom agent-repl-log-sweep-max-files 200
  "Most orphaned Emacs log targets one sweep tick deletes.
The sweep runs in the editor's own process, so it is BOUNDED rather than
exhaustive: 22,112 orphans had accumulated in the temporary root by
2026-09-11, and unlinking them all in one tick would stall the command loop.
Whatever the tick does not reach is reached by the next Emacs instance's."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-log-sweep-min-age-seconds 86400
  "Age an orphaned Emacs log target must reach before the sweep deletes it.
One day.  A target younger than this may belong to an Emacs instance that
has not yet written the canonical link a sweep would read, so age is what
keeps the sweep from deleting a live sink out from under its owner."
  :type 'integer
  :group 'agent-repl)

(defconst agent-repl--emacs-log-target-re
  (concat "\\`" (regexp-quote agent-repl--emacs-log-target-prefix)
          ".*\\.log\\(\\.[0-9]+\\)?\\'")
  "Matches a basename this module minted into the OS temporary root.
The optional generation suffix is included so a swept target does not leave
its `.1'-`.5' generations behind as a second class of orphan.")

(defun agent-repl--log-target-generation-base (path)
  "Return PATH with a trailing `.N' log generation suffix removed.
A generation is referenced exactly when its current target is, because the
canonical link names the current target alone."
  (if (string-match "\\`\\(.*\\.log\\)\\.[0-9]+\\'" path)
      (match-string 1 path)
    path))

(defvar agent-repl-roster-view)
(defvar agent-repl-roster-update-functions)
(declare-function agent-repl-roster-walk "roster")
(declare-function agent-repl-roster-row-ref "roster")

(defun agent-repl--roster-workspace-dirs ()
  "Return every workspace directory the last roster push names, CLOSED included.
Nil before the first push.  A closed workspace has no tab and no entry in
`agent-repl--workspaces', so the roster is the only thing in this runtime
that still names its directory -- and with it the canonical link that names
its log target."
  (when (and (boundp 'agent-repl-roster-view) agent-repl-roster-view
             (fboundp 'agent-repl-roster-walk))
    (let (dirs)
      (dolist (entry (agent-repl-roster-walk agent-repl-roster-view))
        (let ((dir (plist-get (agent-repl-roster-row-ref (plist-get entry :row)) :dir)))
          (when (and (stringp dir) (not (string-empty-p dir)))
            (push dir dirs))))
      (nreverse dirs))))

(defun agent-repl--referenced-log-targets ()
  "Return a hash table of every log target a canonical link or this runtime names.
Three sources are consulted because each alone is incomplete: the in-memory
registry knows only this Emacs instance's sinks, the open workspaces' links
know only the workspaces with a tab, and the roster's directories are the
only record of a CLOSED workspace's link.  A target named by any of them is
NOT an orphan.

THE ROSTER IS A SOURCE BECAUSE ITS ABSENCE DELETED A LIVE TARGET.  A closed
workspace has no tab, so its link was never read, and the sweep unlinked
the target that link named: workspace 2b81f45a724642ef's `emacs.log' was
found dangling at a target minted 2026-09-11 14:29, the day this sweep
landed (realtest 7, 2026-09-24)."
  (let ((referenced (make-hash-table :test #'equal)))
    (maphash (lambda (_key entry)
               (when-let ((target (plist-get entry :target)))
                 (puthash target t referenced)))
             agent-repl--workspace-log-targets)
    (dolist (ws (and (fboundp 'agent-repl--ws-all-names)
                     (agent-repl--ws-all-names)))
      (when (agent-repl--ws-log-routable-p ws)
        (let* ((dir (plist-get (agent-repl--workspace-log-identity ws) :project-dir))
               (dest (file-symlink-p (agent-repl--workspace-emacs-log-path dir))))
          (when (stringp dest)
            (puthash dest t referenced)))))
    (dolist (dir (agent-repl--roster-workspace-dirs))
      (let ((dest (file-symlink-p (agent-repl--workspace-emacs-log-path dir))))
        (when (stringp dest)
          (puthash dest t referenced))))
    referenced))

(defun agent-repl--sweep-orphan-log-targets (&optional directory)
  "Delete unreferenced, day-old Emacs log targets in DIRECTORY, bounded.
DIRECTORY defaults to `temporary-file-directory', which is where every
target minted before the durable-directory change landed.  A file is deleted
only when all three hold: its basename is one this module mints, no
canonical link and no in-memory sink names it
\\(`agent-repl--referenced-log-targets'), and it is older than
`agent-repl-log-sweep-min-age-seconds'.  At most
`agent-repl-log-sweep-max-files' are deleted per call.

Returns a plist of the counts it records."
  (let* ((dir (file-name-as-directory
               (expand-file-name (or directory temporary-file-directory))))
         (referenced (agent-repl--referenced-log-targets))
         (cutoff (- (float-time) agent-repl-log-sweep-min-age-seconds))
         (examined 0) (deleted 0) (kept-referenced 0) (kept-young 0)
         (failed 0) (remaining 0))
    (dolist (name (or (ignore-errors (directory-files dir nil
                                                      agent-repl--emacs-log-target-re
                                                      t))
                      nil))
      (let ((path (expand-file-name name dir)))
        (cond
         ((>= deleted agent-repl-log-sweep-max-files)
          (setq remaining (1+ remaining)))
         (t
          (setq examined (1+ examined))
          (cond
           ((gethash (agent-repl--log-target-generation-base path) referenced)
            (setq kept-referenced (1+ kept-referenced)))
           ((let ((attrs (file-attributes path)))
              (or (null attrs)
                  (file-symlink-p path)
                  (> (float-time (file-attribute-modification-time attrs)) cutoff)))
            (setq kept-young (1+ kept-young)))
           (t
            (condition-case err
                (progn (delete-file path)
                       (setq deleted (1+ deleted)))
              (error
               (setq failed (1+ failed))
               (agent-repl--warn
                '(:agent-repl-central "the orphan log-target sweep spans workspaces")
                "elisp.core.log-sweep-delete-failed: path=%s error=%S" path err)))))))))
    (agent-repl--info
     '(:agent-repl-central "the orphan log-target sweep spans workspaces")
     (concat "elisp.core.log-sweep: directory=%s examined=%d deleted=%d "
             "kept_referenced=%d kept_young=%d failed=%d remaining=%d")
     dir examined deleted kept-referenced kept-young failed remaining)
    (list :examined examined :deleted deleted :kept-referenced kept-referenced
          :kept-young kept-young :failed failed :remaining remaining)))

(defcustom agent-repl-log-sweep-idle-seconds 30
  "Idle seconds before the startup orphan-log-target sweep runs.
The sweep is scheduled on an IDLE timer rather than run from the startup
hook: the hook runs before the first redisplay, and a sweep that unlinks
files there would hold the frame off screen to tidy a directory."
  :type 'integer
  :group 'agent-repl)

(defun agent-repl-schedule-orphan-log-sweep ()
  "Arm the one-shot idle timer that sweeps orphaned Emacs log targets.
THE SWEEP WAITS FOR THE FIRST ROSTER PUSH.  Before it, a closed workspace's
link is named by nothing this runtime holds, so every target such a link
names would read as an orphan (`agent-repl--referenced-log-targets').  With
no roster yet, the arming is deferred to the first accepted push."
  (if (and (boundp 'agent-repl-roster-view) agent-repl-roster-view)
      (agent-repl--arm-orphan-log-sweep)
    (add-hook 'agent-repl-roster-update-functions
              #'agent-repl--arm-orphan-log-sweep-on-first-roster)
    (agent-repl--info
     '(:agent-repl-central "the orphan log-target sweep spans workspaces")
     "elisp.core.log-sweep-deferred: until=first-roster-push")
    nil))

(defun agent-repl--arm-orphan-log-sweep-on-first-roster (_roster)
  "Arm the deferred orphan log sweep once, on the first roster push."
  (remove-hook 'agent-repl-roster-update-functions
               #'agent-repl--arm-orphan-log-sweep-on-first-roster)
  (agent-repl--arm-orphan-log-sweep))

(defun agent-repl--arm-orphan-log-sweep ()
  "Arm the one-shot idle timer that runs the orphan log sweep."
  (prog1 (run-with-idle-timer agent-repl-log-sweep-idle-seconds nil
                              #'agent-repl--sweep-orphan-log-targets)
    (agent-repl--info
     '(:agent-repl-central "the orphan log-target sweep spans workspaces")
     "elisp.core.log-sweep-scheduled: idle=%s max_files=%s min_age_seconds=%s"
     agent-repl-log-sweep-idle-seconds
     agent-repl-log-sweep-max-files
     agent-repl-log-sweep-min-age-seconds)))


;;; Dangling canonical links

(defvar agent-repl--dangling-log-links-checked (make-hash-table :test #'equal)
  "Workspace directories whose canonical `emacs.log' link this runtime checked.
The check is once per directory per runtime: a link this module later
re-points always names a target it just created.")

(defun agent-repl--retire-dangling-emacs-log-links (_roster)
  "Remove every roster workspace's canonical `emacs.log' link that names no file.
A link whose target is gone names records that no longer exist anywhere, and
every reader that resolves it -- an operator, the log reader, the realtest
harvest -- meets a path to nothing.  Only a link naming a target THIS module
could have minted (`agent-repl--emacs-log-owned-target-p') is removed: a
foreign link is the workspace's, not ours.  The next record the workspace
writes mints a fresh target and a fresh link, exactly as a first open does.

Registered on `agent-repl-roster-update-functions', because the roster is the
one place a CLOSED workspace's directory is still named."
  (dolist (dir (agent-repl--roster-workspace-dirs))
    (unless (gethash dir agent-repl--dangling-log-links-checked)
      (puthash dir t agent-repl--dangling-log-links-checked)
      (let* ((canonical (agent-repl--workspace-emacs-log-path dir))
             (dest (file-symlink-p canonical)))
        (when (and (stringp dest)
                   (not (file-exists-p canonical))
                   (agent-repl--emacs-log-owned-target-p dest))
          (condition-case err
              (progn
                (delete-file canonical)
                (agent-repl--info
                 '(:agent-repl-central "the roster names workspaces this runtime may hold no sink for")
                 "elisp.core.log-link-retired: dir=%s link=%s gone_target=%s"
                 dir canonical dest))
            (file-error
             (agent-repl--warn
              '(:agent-repl-central "the roster names workspaces this runtime may hold no sink for")
              "elisp.core.log-link-retire-failed: dir=%s link=%s error=%S"
              dir canonical err))))))))

(add-hook 'agent-repl-roster-update-functions #'agent-repl--retire-dangling-emacs-log-links)

(defun agent-repl--secure-log-file-mode (path)
  "Require PATH to be a regular file and force its permissions to 0600.
This helper intentionally does not log: it runs inside the logfile sink, so
using the logging ladder here would recurse."
  (unless (file-regular-p path)
    (error "agent-repl--secure-log-file-mode: not a regular file: %s" path))
  (unless (= (logand (file-modes path) #o777) #o600)
    (set-file-modes path #o600)))

(defun agent-repl--log-generation-path (path generation)
  "Return PATH's positive integer GENERATION filename."
  (unless (and (integerp generation) (> generation 0))
    (error "agent-repl: log generation must be a positive integer, got %S" generation))
  (format "%s.%d" path generation))

(defun agent-repl--log-rotate-generations (path)
  "Move PATH to `.1' and retain exactly `agent-repl-log-generation-count'."
  (when (< agent-repl-log-generation-count 1)
    (error "agent-repl: log generation count must be positive, got %S"
           agent-repl-log-generation-count))
  (let ((oldest (agent-repl--log-generation-path
                 path agent-repl-log-generation-count)))
    (when (file-exists-p oldest)
      (delete-file oldest)))
  (cl-loop for generation downfrom (1- agent-repl-log-generation-count) to 1
           for from = (agent-repl--log-generation-path path generation)
           for to = (agent-repl--log-generation-path path (1+ generation))
           when (file-exists-p from)
           do (rename-file from to))
  (when (file-exists-p path)
    (rename-file path (agent-repl--log-generation-path path 1))))

(defun agent-repl--log-rotate-before-write (path bytes)
  "Rotate PATH when appending BYTES would cross the configured size cap."
  (unless (> agent-repl-log-size-cap-bytes 0)
    (error "agent-repl: log size cap must be positive, got %S"
           agent-repl-log-size-cap-bytes))
  (let* ((attrs (file-attributes path))
         (size (if attrs (file-attribute-size attrs) 0)))
    (when (and (> size 0)
               (> (+ size bytes) agent-repl-log-size-cap-bytes))
      (agent-repl--log-rotate-generations path))))

(defun agent-repl--do-log-to-file (text &optional ws)
  "Append TEXT as a line to the logfile when `agent-repl-log-to-file' is non-nil.
Sink failures, including rotation failures, emit the emergency diagnostic and
signal an error; persistence must never silently degrade.
The size check runs before every append so a record is never split across
generations."
  (when agent-repl-log-to-file
    (let ((path (if ws
                    (agent-repl--workspace-emacs-log-target ws)
                  (agent-repl--logfile-path))))
      (condition-case err
          (let* ((line (concat text "\n"))
                 (bytes (string-bytes (encode-coding-string line 'utf-8 t))))
            (agent-repl--log-rotate-before-write path bytes)
            (let ((new-file (not (file-exists-p path))))
              (write-region line nil path t 'silent)
              (when new-file
                (agent-repl--secure-log-file-mode path))))
        (error
         ;; Sink failure cannot enter its own failed sink.  This emergency
         ;; message is therefore the sole permitted alternate output channel.
         (message "[agent-repl] LOG SINK FAILURE path=%s error=%S" path err)
         (error "agent-repl log sink failure for %s: %S" path err))))))

(defun agent-repl--build-log-text (ws fmt args)
  "Build the formatted log line for WS / FMT / ARGS.
Shared by `agent-repl--do-log' and its message-gated wrappers so the
file-write path and the message-emit path always agree on the exact
text.  Handles the non-string-FMT bug-capture in one place."
  (if (stringp fmt)
      (let ((msg  (apply #'format fmt args))
            (ts   (format-time-string "%H:%M:%S.%3N"))
            (meta (agent-repl--format-ws-metadata ws)))
        (format "%s [agent-repl] %s%s" ts msg meta))
    (agent-repl--log-format-capture-bug fmt)
    (format "%s [agent-repl] [BUG non-string-fmt=%S]%s"
            (format-time-string "%H:%M:%S.%3N")
            fmt
            (agent-repl--format-ws-metadata ws))))

(defconst agent-repl--workspace-log-buffer-suffix "-log"
  "Suffix used for the workspace-owned live log buffer.")

(defvar agent-repl--workspace-log-buffer-enabled t
  "Non-nil when workspace-scoped log lines should populate live buffers.
Production leaves this enabled.  The pure-Elisp batch harness binds it nil so
unrelated tests that assert exact buffer or perspective effects do not acquire
a logging side effect; the dedicated workspace-log tests bind it back to t.")

(defvar agent-repl--log-sink-reentrant nil
  "Non-nil while the logging ladder is resolving its own sink.
Helpers the sink itself calls consult this to stay silent.  Without it the
de-instrumentation below holds for one level only: this function does not
log, but the buffer it resolves is named by `agent-repl--buffer-name', which
does — so every workspace-scoped record produced a SECOND record announcing
the name of the buffer it was about to be appended to.  That doubling is
what made `buffer-name: suffix=-log' the third-largest operation in the log
at 71,425 records, and it scales with ALL workspace-scoped logging, not with
any one noisy caller.")

(defun agent-repl--workspace-log-buffer (ws)
  "Return WS's workspace-owned live agent-repl log buffer.
The buffer is created through `agent-repl--create-buffer', which sets its
permanent-local owner and attaches it through workspace.el's perspective
boundary.  Its contents are an in-memory view only; the durable logfile
remains the authoritative persisted record.  Buffer resolution is itself part
of the sink, so it suppresses the buffer-name diagnostic that would recurse
or require routing before this in-memory sink exists."
  (let ((agent-repl--log-sink-reentrant t))
    (agent-repl--create-buffer ws agent-repl--workspace-log-buffer-suffix)))

(defun agent-repl--append-workspace-log (ws text)
  "Append the exact formatted log TEXT to WS's live log buffer.
Only a non-nil WS is workspace-scoped.  This helper deliberately does not
log its own buffer creation or append work: it runs for every log entry, and
instrumenting it through the logging ladder would recurse indefinitely.  The
`agent-repl--log-sink-reentrant' binding extends that intent to the helpers
it calls, which would otherwise reinstate the instrumentation it avoids."
  (when (and agent-repl--workspace-log-buffer-enabled ws)
    (with-current-buffer (let ((agent-repl--log-sink-reentrant t))
                           (agent-repl--workspace-log-buffer ws))
      (let ((inhibit-read-only t))
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-max))
            (insert text "\n")))))))

(defconst agent-repl--log-level-rank
  '(("verbose" . 0) ("debug" . 1) ("info" . 2) ("warn" . 3) ("error" . 4))
  "Rank of each rung of the logging ladder, least to most severe.
`verbose' is a rung here even though it travels as a VERBOSITY rather than
a level: on the durable sink the two axes collapse into one ordering, and
a threshold is only useful if every record can be placed on it.")

(defun agent-repl--log-record-rank (level verbosity)
  "Rank LEVEL and VERBOSITY on the workspace-buffer display ladder.
The durable sink deliberately ranks only by LEVEL; see
`agent-repl--log-record-persists-p'."
  (or (cdr (assoc (if (equal verbosity "verbose") "verbose" level)
                  agent-repl--log-level-rank))
      (error "agent-repl: unknown log level=%S verbosity=%S" level verbosity)))

(defun agent-repl--log-record-clears-p (level verbosity threshold)
  "Whether a LEVEL / VERBOSITY record ranks at or above THRESHOLD."
  (>= (agent-repl--log-record-rank level verbosity)
      (agent-repl--log-record-rank (symbol-name threshold) nil)))

(defun agent-repl--log-record-persists-p (level verbosity)
  "Whether a LEVEL / VERBOSITY record clears `agent-repl-log-file-level'.
A level window that is over is ended here first
\(`agent-repl--log-level-tick')."
  (ignore verbosity)
  (agent-repl--log-level-tick)
  (>= (agent-repl--log-record-rank level "normal")
      (agent-repl--log-record-rank (symbol-name agent-repl-log-file-level)
                                   "normal")))

(defconst agent-repl--log-level-window-scope
  '(:agent-repl-central "process-wide logging and utility state")
  "The scope of every level window record.")

(defun agent-repl--log-level-tick ()
  "End the durable log level's window when it is over.
The level reverts to `info' and the revert is recorded at info.  The window
is cleared before the record is written, so that record cannot end it again."
  (when (and agent-repl--log-level-expires-at
             (>= (funcall agent-repl--log-level-clock) agent-repl--log-level-expires-at))
    (let ((from agent-repl-log-file-level)
          (until agent-repl--log-level-expires-at))
      (setq agent-repl-log-file-level 'info
            agent-repl--log-level-expires-at nil)
      (agent-repl--info agent-repl--log-level-window-scope
                        "elisp.core.log-level-window: log level %s window ended at %s; reverted to info outcome=window_ended"
                        from (format-time-string "%FT%T%z" (seconds-to-time until))))))

(defun agent-repl--note-log-level-selection (selection)
  "Record at info that SELECTION started at `info' though another level was asked.
Silent for the default and for an honored window."
  (pcase (plist-get selection :outcome)
    ((and outcome (or 'no_expiry 'expired 'beyond_window))
     (agent-repl--info agent-repl--log-level-window-scope
                       "elisp.core.log-level-window: log level %S ignored; starting at info outcome=%s requested-until=%S"
                       (plist-get selection :requested) outcome
                       (plist-get selection :requested-until)))))

(defun agent-repl--set-log-level-window (level)
  "Make LEVEL the durable level: a window of the longest length unless `info'.
Return when the window ends, nil for `info'."
  (setq agent-repl-log-file-level level
        agent-repl--log-level-expires-at
        (unless (eq level 'info)
          (+ (funcall agent-repl--log-level-clock) agent-repl--log-level-window-seconds))))

(defun agent-repl--log-record-displays-p (level verbosity)
  "Whether a LEVEL / VERBOSITY record clears `agent-repl-log-buffer-level'."
  (agent-repl--log-record-clears-p level verbosity agent-repl-log-buffer-level))

(cl-defun agent-repl--emit-log-record
    (ws level verbosity fmt args &key operation-fmt message-mode fatal)
  "Build, route, persist, and present one log record.
Every logging rung calls this function directly.  MESSAGE-MODE is nil,
`quiet', or `echo'.  FATAL signals after persistence.  OPERATION-FMT preserves
the untagged operation template used by warning and error wrappers.

A ROUTING FAILURE IS NOT THE CALLER'S ERROR TO HANDLE.  Logging is called
from everywhere, including from the middle of loops whose remaining
iterations matter; a rung that signalled into its caller turned one
workspace with a deleted worktree into a whole roster push aborting on its
first row and zero tabs drawn.  FATAL is the one exception: it is a
control-flow act, not a rung, and still signals.

TWO DIFFERENT FACTS REACH THIS FUNCTION UNROUTED, and they are recorded
differently:

  - A NAMED WORKSPACE THAT OWNS NO DURABLE SINK falls back to the central
    sink, and `agent-repl--central-log-fallback-class' says which of the two
    facts that is.  A workspace with no durable home of its own — an
    unregistered name, or a registered scratch path — is an ORDINARY
    outcome, recorded once at INFO and silently, and so is a workspace
    whose departure this editor ordered.  A workspace registered at a
    directory that is GONE with nothing having asked for it to go is a
    stale registry row, recorded once at WARN and keeping its
    user-visible popup.  Either way the record is
    written to the global sink carrying `unroutable_workspace', so the
    line still says which workspace it is about, and nothing that merely
    renders or sweeps such a workspace may fail or spam because of it: a
    single tab render used to write 22 ERROR lines this way.

  - NO WORKSPACE AT ALL is missing attribution at the call site, which no
    directory can supply.  It keeps its `log-routing-error' line at ERROR
    beside the rerouted original.

A RECORD NOBODY WILL SEE COSTS NEARLY NOTHING.  A record that will not be
persisted (`agent-repl-log-file-level' or `agent-repl-log-to-file' drops
it), echoes no message, is not FATAL and whose scope is certainly central
\(`agent-repl--log-scope-candidate' answers `agent-repl--global-log-scope',
so routing cannot fail or name a workspace) returns nil before anything is
routed or built.  Measured 2026-10-08: the wire codec's dropped debug lines
cost ~0.1-0.18 ms each and dominated a workspace switch.

A dropped record that names a WORKSPACE, or names nothing at all, is still
routed in full, because routing it is error-handling coverage: a missing
attribution still records its `log-routing-error' and a sinkless workspace
still announces its central fallback.  Its identity invariants are still
checked (`agent-repl--log-check-identity'); only the JSON record, the
formatted message and the display text are not built."
  (when (and (not fatal)
             (not message-mode)
             (stringp fmt)
             (not (and agent-repl-log-to-file
                       (agent-repl--log-record-persists-p level verbosity)))
             (eq (agent-repl--log-scope-candidate ws (or operation-fmt fmt))
                 agent-repl--global-log-scope))
    (cl-return-from agent-repl--emit-log-record nil))
  (cl-flet
      ;; Record, once per workspace, that its records go to the central sink.
      ;; The level follows the class: a stale registry row is WARN, every
      ;; other class INFO.  A local function so the one builder and the one
      ;; writer stay this function's own.
      ((announce-central-fallback (unroutable reason)
         (when-let* ((class (agent-repl--claim-central-log-fallback unroutable))
                     (fallback-level (if (eq class 'stale-registration)
                                         "warn"
                                       "info")))
           (when (and agent-repl-log-to-file
                      (agent-repl--log-record-persists-p fallback-level "normal"))
             (agent-repl--do-log-to-file
              (agent-repl--log-record nil fallback-level "normal"
                                      agent-repl--central-log-fallback-format
                                      (list unroutable class reason)
                                      nil nil unroutable)
              nil)))))
    (let* ((agent-repl--log-dir-confirmed (list :confirmed))
           (routing (agent-repl--resolve-log-workspace ws (or operation-fmt fmt)))
           (routing-error (plist-get routing :routing-error))
           (sink-ws (plist-get routing :workspace))
           (pseudo-ws (plist-get routing :pseudo))
           (unroutable-ws (plist-get routing :unroutable)))
      ;; THE LEVEL FOLLOWS THE CLASS, and the record is still built and written
      ;; here rather than handed to a rung: `agent-repl--emit-log-record' is the
      ;; one builder and the one writer, and re-entering a rung from inside it
      ;; would break that and strip the `unroutable_workspace' stamp this record
      ;; exists to carry.  A stale registry row is a real inconsistency in
      ;; durable state, so it is recorded at WARN; a workspace that simply has
      ;; no durable home of its own is ordinary, so it is recorded at INFO.
      (when unroutable-ws
	(announce-central-fallback unroutable-ws (plist-get routing :reason)))
      (if routing-error
          (let* ((offender (plist-get routing :offender))
		 (reason (plist-get routing :reason))
		 (route-fmt "elisp.core.log-routing-error workspace=%S reason=%s original-operation=%s")
		 (route-args (list offender reason
                                   (agent-repl--log-operation (or operation-fmt fmt))))
		 (record (agent-repl--log-record nil "error" "normal"
						 route-fmt route-args))
		 (original (agent-repl--log-record nil level verbosity fmt args
                                                   nil operation-fmt offender))
		 (text (agent-repl--build-log-text nil fmt args)))
            (when agent-repl-log-to-file
              (agent-repl--do-log-to-file record nil))
            (agent-repl--note-unroutable-log-workspace offender)
            (when (and agent-repl-log-to-file
                       (agent-repl--log-record-persists-p level verbosity))
              (agent-repl--do-log-to-file original nil))
            (unless fatal
              (pcase message-mode
		('quiet (agent-repl--emit-message text nil))
		('echo (agent-repl--emit-message text t))
		('backend
		 (agent-repl--emit-message
                  (concat "agent-repl: " (apply #'format fmt args)) t))))
            (when fatal
              (error "%s" text))
            original)
	(let* ((to-file (and agent-repl-log-to-file
                             (agent-repl--log-record-persists-p level verbosity)))
               (to-buffer (and agent-repl--workspace-log-buffer-enabled sink-ws
				(agent-repl--log-record-displays-p level verbosity)))
               ;; A record nobody will read is routed and its identity checked,
               ;; but never built: see the docstring.
               (build (or to-file to-buffer message-mode fatal))
               (record nil))
          ;; THE DIRECTORY CAN GO BETWEEN ROUTING AND WRITING.  A worktree a
          ;; merge or a plain `rm' removes is removed by another process, so a
          ;; workspace routed to its own sink a moment ago may own none by the
          ;; time its identity is stamped or its link re-made.  That is the same
          ;; fact routing would have found a moment later, so the record takes
          ;; the same central fallback rather than signalling into its caller.
          ;; A failure while the workspace is STILL routable is not that fact
          ;; and is signalled unchanged.
          (condition-case err
              (if (not build)
                  (agent-repl--log-check-identity sink-ws)
		(setq record (agent-repl--log-record sink-ws level verbosity fmt args
                                                     pseudo-ws operation-fmt unroutable-ws))
		(when to-file
                  (agent-repl--do-log-to-file record sink-ws)))
            (error
             (unless (and sink-ws
                          (not (let ((agent-repl--log-dir-confirmed nil))
                                 (agent-repl--ws-log-routable-p sink-ws))))
               (signal (car err) (cdr err)))
             (announce-central-fallback sink-ws (error-message-string err))
             (if (not build)
                 (agent-repl--log-check-identity nil)
               (setq record (agent-repl--log-record nil level verbosity fmt args
                                                    nil operation-fmt sink-ws))
               (when to-file
                 (agent-repl--do-log-to-file record nil)))
             (setq sink-ws nil
                   to-buffer nil)))
          (let ((text (and (or message-mode fatal)
                           (agent-repl--build-log-text sink-ws fmt args))))
            (when to-buffer
              (agent-repl--append-workspace-log sink-ws record))
            (unless fatal
              (pcase message-mode
		('quiet (agent-repl--emit-message text nil))
		('echo (agent-repl--emit-message text t))
		('backend
		 (agent-repl--emit-message
                  (concat "agent-repl: " (apply #'format fmt args)) t))))
            (when fatal
              (error "%s" text))
            record))))))

;;;; ---- Echo-area (modeline) severity gate ----
;;
;; agent-repl has two distinct log sinks and they are NOT the same channel:
;;
;;   1. The QUIET sink — the log file plus the *Messages* buffer.  Everything
;;      goes here.  It is free, durable, greppable, and nobody has to look at
;;      it unless they are debugging.
;;
;;   2. The LOUD sink — the echo area / modeline.  This is the highest-
;;      sensitivity channel we have: it interrupts the user and covers the
;;      minibuffer.  It is reserved for GENUINE FATAL conditions alone — the
;;      ones the user (or an agent watching the modeline for them) MUST act
;;      on immediately.  Nothing on the LADDER reaches it: a fatal condition
;;      is SIGNALLED (`agent-repl--fatal', which Emacs always displays), and
;;      signalling is a control-flow act, not a log level.  Every ladder
;;      level — `agent-repl--error' included — stays on the quiet sink and
;;      NEVER flashes in the modeline; the records remain durable and
;;      greppable for whoever needs them.
;;
;; `agent-repl--emit-message' is the single chokepoint that decides which
;; sink a line reaches.  Binding `inhibit-message' suppresses the echo-area
;; display while STILL logging the line to *Messages' — that is exactly the
;; bifurcation we want, and it means quieting a line never costs us the log.
;; Every ladder level below emits QUIETLY through it; the modeline is left
;; to `error' signalling alone.
;;
;; Pick a level, do not reach for `message' directly:
;;
;;   `agent-repl--log-verbose'  hot-path chatter   file at debug, terminal verbose
;;   `agent-repl--log'          debug chatter      file at debug, quiet
;;   `agent-repl--info'         background notice  file + *Messages*, quiet
;;   `agent-repl--warn'         recorded warning   file + *Messages*, quiet
;;   `agent-repl--error'        recorded error     file + *Messages*, quiet
;;
;; Aborting is not a rung: `agent-repl--fatal' records at the `error' level
;; and then SIGNALS, for a refusal or a broken precondition the caller must
;; not continue past.
;;
;; A bare `message' remains correct for one case only: synchronous feedback
;; from an interactive command the user just ran ("Copied: <ref>").  Async,
;; background, progress, and lifecycle chatter must never reach the echo area.

(defun agent-repl--emit-message (text &optional echo)
  "Emit TEXT via `message', reaching the echo area only when ECHO is non-nil.
With ECHO nil, `inhibit-message' is bound so TEXT still lands in the
*Messages* buffer (and, via the caller, the log file) but never flashes
in the echo area / modeline.  This is the single chokepoint separating
agent-repl's quiet sink from its loud one."
  (let ((inhibit-message (not echo)))
    (message "%s" text)))

(defun agent-repl--do-log-level (ws fmt args level &optional error-p operation-fmt)
  "Persist and emit WS / FMT / ARGS at LEVEL without changing public APIs.
OPERATION-FMT, when non-nil, is the untagged format string the persisted
record's stable `operation' name is derived from, while FMT — which may
carry a severity display tag — still supplies the recorded and displayed
message."
  (agent-repl--emit-log-record ws level "normal" fmt args
                               :operation-fmt operation-fmt
                               :message-mode 'quiet
                               :fatal error-p))

(defun agent-repl--do-log (ws fmt args &optional error-p)
  "Unconditional log entry: ALWAYS write to file AND emit to message/error.
WS is the workspace name for context (or nil).  When ERROR-P is non-nil,
signals the formatted line via `error' instead of `message' — the
file-write still happens first so the line is captured before unwinding.

Only the ERROR-P path reaches the echo area / modeline: a signalled
`error' is a genuine fatal condition the user must act on immediately, so
Emacs displays it regardless of `inhibit-message'.  The non-error path is
captured unconditionally to the log file AND *Messages* but emitted
QUIETLY (never the echo area), so warnings and background diagnostics no
longer flash in the modeline — they stay durable and greppable on the
quiet channels without interrupting the user.  This reserves the modeline
as a signal for fatal errors alone.

This is the entry point for log calls that MUST be captured regardless
of `agent-repl-debug'.  Debug-gated callers (`agent-repl--log',
`agent-repl--log-verbose') use the file-write path directly and emit
quietly; `agent-repl--info' is the equivalent ungated quiet-notice level."
  (agent-repl--emit-log-record ws (if error-p "error" "info") "normal" fmt args
                               :message-mode 'quiet
                               :fatal error-p))

(defun agent-repl--log (ws fmt &rest args)
  "Log a timestamped message for WS, always to file, conditionally to *Messages*.
File write happens whenever `agent-repl-log-to-file' is non-nil (the
default) — REGARDLESS of `agent-repl-debug'.  The `message' call only
fires when `agent-repl-debug' is non-nil, and even then it is emitted
quietly (into *Messages* only, never the echo area), so turning debug
logging on never turns the modeline into a firehose.
FMT and ARGS use the same format conventions as `message'."
  (agent-repl--emit-log-record ws "debug" "normal" fmt args
                               :message-mode (and agent-repl-debug 'quiet)))

(defun agent-repl--log-verbose (ws fmt &rest args)
  "Persist a high-frequency message and show it only in verbose mode.
`agent-repl-debug' affects terminal and *Messages* visibility only.  The
JSONL record carries level `debug' and persists whenever the durable level
switch admits debug records."
  (agent-repl--emit-log-record ws "debug" "verbose" fmt args
                               :message-mode (and (eq agent-repl-debug 'verbose)
                                                  'quiet)))

(defun agent-repl--info (ws fmt &rest args)
  "Log an informational line for WS to the QUIET sink, ungated by debug.
The line ALWAYS reaches the log file and the *Messages* buffer, but it
never reaches the echo area / modeline.  This is the level for background
and lifecycle chatter that is valuable to have on the record but that the
user must not be interrupted by: module loads, worktree creation progress,
snapshot-load steps, sentinel bookkeeping, agent start/finish notices.

Use `agent-repl--warn' instead to tag a recorded line with `WARNING:'
severity (still quiet), or `agent-repl--error' for the `error' rung — a
contract breach or a failed operation, recorded at the level the logging
contract reserves for them.  `agent-repl--fatal' is the abort."
  (agent-repl--emit-log-record ws "info" "normal" fmt args :message-mode 'quiet))

(defun agent-repl--warn (ws fmt &rest args)
  "Log a WARNING for WS to the QUIET sink: the log file and *Messages'.
A `WARNING: ' severity tag is prepended, so call sites pass the bare
message (no literal \"WARNING:\" prefix of their own).

A warning is NOT fatal, so it does not reach the echo area / modeline:
the line is recorded on the durable, greppable channels (log file plus
*Messages*) for the user or a watching agent to find, but it never
interrupts.  Reserve `agent-repl--error' for the `error' rung — a broken
contract or a failed operation, which is worse but no louder — and
`agent-repl--fatal' for a condition the caller must not continue past.
This level still carries
the `WARNING: ' severity that a plain `agent-repl--info' notice lacks:
use it for failed writes, dropped state, broken invariants, and degraded
functionality that are worth flagging in the log but are not fatal."
  (if (stringp fmt)
      (agent-repl--emit-log-record ws "warn" "normal" (concat "WARNING: " fmt) args
                                   :operation-fmt fmt :message-mode 'quiet)
    ;; A non-string FMT is a caller bug.  Hand it through untouched rather
    ;; than `concat'-ing it (which would raise a wrong-type-argument here and
    ;; bury the real culprit): `agent-repl--build-log-text' already captures a
    ;; backtrace to *agent-repl-log-bug* for exactly this case, and ARGS is
    ;; preserved so nothing about the offending call is lost.
    (agent-repl--emit-log-record ws "warn" "normal" fmt args :message-mode 'quiet)))

(defconst agent-repl--warn-once-capacity 4096
  "Maximum process-local warning fingerprints retained by `agent-repl--warn-once'.")

(defvar agent-repl--warn-once-fingerprints (make-hash-table :test 'equal)
  "Process-local set of warning fingerprints already emitted.")

(defvar agent-repl--warn-once-order nil
  "FIFO order for `agent-repl--warn-once-fingerprints'.")

(defun agent-repl--warn-once (ws fingerprint fmt &rest args)
  "Log a warning once for stable causal FINGERPRINT and return whether emitted.
WS, FMT, and ARGS have the same identity-complete logging contract as
`agent-repl--warn'.  FINGERPRINT must be a nonempty stable string naming the
causal diagnostic identity.  The process-local FIFO cache is bounded by
`agent-repl--warn-once-capacity'; eviction makes a later observation eligible
to emit again rather than allowing unbounded diagnostic state."
  (unless (and (stringp fingerprint) (not (string-empty-p fingerprint)))
    (agent-repl--log ws "warn-once: rejected fingerprint=%S reason=empty-or-nonstring" fingerprint)
    (error "agent-repl--warn-once: fingerprint must be a nonempty string"))
  (if (gethash fingerprint agent-repl--warn-once-fingerprints)
      nil
    (when (= (hash-table-count agent-repl--warn-once-fingerprints)
             agent-repl--warn-once-capacity)
      (remhash (car agent-repl--warn-once-order) agent-repl--warn-once-fingerprints)
      (setq agent-repl--warn-once-order (cdr agent-repl--warn-once-order)))
    (puthash fingerprint t agent-repl--warn-once-fingerprints)
    (setq agent-repl--warn-once-order
          (append agent-repl--warn-once-order (list fingerprint)))
    (if (stringp fmt)
        (agent-repl--emit-log-record ws "warn" "normal"
                                     (concat "WARNING: " fmt) args
                                     :operation-fmt fmt :message-mode 'quiet)
      (agent-repl--emit-log-record ws "warn" "normal" fmt args
                                   :message-mode 'quiet))
    t))

(defvar agent-repl--log-transition-states (make-hash-table :test 'equal)
  "Last observed diagnostic state keyed by caller-owned transition key.")

;;;; ---- Runtime log-verbosity controls ----
;;
;; The three knobs a reader reaches for mid-investigation, as ordinary
;; commands.  They live here, beside the variables they set, rather than in
;; keybindings.el: `agent-repl-debug' governs *Messages* visibility and
;; `agent-repl-log-file-level' governs durable volume, and both are defined
;; above.  keybindings.el only binds them (SPC j D / L / V).

(defun agent-repl-toggle-debug (&optional verbose)
  "Toggle debug logging VISIBILITY in *Messages*.
Without a prefix argument: cycle nil -> t -> nil.  With a prefix argument
\(\\[universal-argument]): cycle nil -> verbose -> nil.  Verbose
additionally SHOWS the hot-path rung (timer ticks, window changes, and the
rest of `agent-repl--log-verbose').

This changes NOTHING about what is written to the log file, and turning it
off will not shrink one — that is `agent-repl-set-log-file-level'."
  (interactive "P")
  (setq agent-repl-debug
        (if verbose
            (if (eq agent-repl-debug 'verbose) nil 'verbose)
          (if agent-repl-debug nil t)))
  (let ((label (pcase agent-repl-debug
                 ('nil "OFF")
                 ('t "ON")
                 ('verbose "ON (verbose)")
                 (_ (agent-repl--fatal
                     '(:agent-repl-central "process-wide logging and utility state")
                     "elisp.core.toggle-debug: agent-repl-debug has unexpected value=%S"
                     agent-repl-debug)))))
    ;; Emitted through `message' unconditionally: this is synchronous feedback
    ;; from a command the user just ran, and it must be visible in exactly the
    ;; case where the ladder has just been turned OFF.
    (message "[agent-repl] debug logging: %s" label)
    (if agent-repl-debug
        (agent-repl--info '(:agent-repl-central "process-wide logging and utility state") "elisp.core.toggle-debug: visibility=%s" label)
      ;; The turn-OFF record cannot ride `--info' honestly once visibility is
      ;; gone from *Messages*, but the durable sink still wants the boundary.
      (agent-repl--log '(:agent-repl-central "process-wide logging and utility state") "elisp.core.toggle-debug: visibility=%s" label))))

(defun agent-repl-set-log-file-level (level)
  "Set `agent-repl-log-file-level' to LEVEL.
This is the control for LOG FILE volume, which `agent-repl-debug' has
never governed.  A level other than `info' is a window: it reverts to
`info' by itself after `agent-repl--log-level-window-seconds', and the
revert is recorded at info.  It takes effect on the very next record — no
restart and no reload — so a log can be turned down while it is actively
being flooded and back up before a reproduction is captured."
  (interactive
   (list (intern
          (completing-read
           (format "Durable log level (currently %s): " agent-repl-log-file-level)
           '("debug" "info" "warn" "error")
           nil t nil nil (symbol-name agent-repl-log-file-level)))))
  (unless (memq level '(debug info warn error))
    (agent-repl--error
     '(:agent-repl-central "process-wide logging and utility state")
     "elisp.core.set-log-file-level: rejected level=%S reason=not-a-log-level" level)
    (error "agent-repl: %S is not a log level; expected one of debug info warn error"
           level))
  ;; A level other than info is a window: it reverts to info by itself
  ;; (`agent-repl--log-level-tick').
  (agent-repl--set-log-level-window level)
  ;; Announced through the durable sink as well as the echo area: the record
  ;; saying the threshold moved is itself the boundary a later reader needs to
  ;; explain why the surrounding volume changed.
  (agent-repl--info '(:agent-repl-central "process-wide logging and utility state") "elisp.core.set-log-file-level: durable log level now %s" level)
  (message "[agent-repl] durable log level: %s" level)
  level)

(defun agent-repl-toggle-verbose-to-disk ()
  "Toggle whether the verbose rung is written to the LOG FILE.
Verbose records carry debug level, so this flips `agent-repl-log-file-level'
between `debug' and `info'.  Debug is a window: it reverts to `info' by
itself after `agent-repl--log-level-window-seconds'.

Affects the FILE only.  The per-workspace log buffers follow
`agent-repl-log-buffer-level' and *Messages* follows `agent-repl-debug'."
  (interactive)
  (agent-repl--set-log-level-window
   (if (eq agent-repl-log-file-level 'debug) 'info 'debug))
  (let ((on (eq agent-repl-log-file-level 'debug)))
    (agent-repl--info '(:agent-repl-central "process-wide logging and utility state") "elisp.core.toggle-verbose-to-disk: verbose-to-file=%s"
                      (if on "ON" "OFF"))
    (message "[agent-repl] verbose logging to disk: %s%s"
             (if on "ON" "OFF")
             (if on "" " (warnings and errors still recorded)"))
    agent-repl-log-file-level))

;; The level this load started at, stated now that logging exists: a level
;; asked for and not honored (no window, an ended one, one too long) is said
;; at info, once per load.
(agent-repl--note-log-level-selection agent-repl--log-level-startup-selection)

;;;; ---- Quit deferral around asynchronous critical sections ----------------
;;
;; A process filter, a process sentinel and a timer callback all run at
;; whatever moment the event loop chooses, INCLUDING the moment the user is
;; holding `C-g'.  Emacs runs them with quitting ENABLED, so the quit lands
;; wherever the callback happens to be — halfway through draining a batch of
;; UDS frames, between registering a pending command and writing its frame,
;; between spawning a build and recording the request that owns it.  The
;; observed symptom is the `error in process filter: Quit' line in
;; *Messages*; the observed damage is the half-mutated bookkeeping the
;; callback was in the middle of writing.
;;
;; The fix is the standard one and it is STRUCTURAL rather than probabilistic:
;; the critical section runs under `inhibit-quit', so a `C-g' arriving inside
;; it cannot interrupt it at all.  Emacs records the request in `quit-flag'
;; and leaves it there; the flag survives the binding and the command loop
;; acts on it at its next quit checkpoint.  So the user's quit is DEFERRED to
;; a boundary where nothing is half-written, and never LOST.
;;
;; This is deliberately NOT a swallowed quit: `agent-repl--with-deferred-quit'
;; never clears `quit-flag', and it records the deferral through the module's
;; canonical log so a quit that looks ignored is explainable from the log
;; alone.
;;
;; WHAT THIS GUARD IS FOR, AND WHAT IT IS NOT FOR.  An earlier revision of this
;; commentary blamed the 2026-09-12 sweep's "a real `C-g' did not dismiss the
;; prompt" finding on this deferral, on the reasoning that `handle_interrupt'
;; throws into `read_char' when Emacs is idle and can only arm `quit-flag' when
;; it is busy, so a quit armed inside one of this module's callbacks is taken
;; INSIDE that callback and never reaches the standing minibuffer.  That
;; reasoning is wrong on this platform, and reading Emacs 30.2's own source is
;; what settles it:
;;
;;   - `handle_interrupt' (src/keyboard.c) guards its call to
;;     `quit_throw_to_read_char' with `#ifndef HAVE_NS'.  On the macOS build
;;     this module runs on it NEVER throws into the read.  Busy or idle, all a
;;     `C-g' ever does at first is arm `quit-flag'.
;;
;;   - The flag is then taken by `kbd_buffer_get_event''s own wait loop, which
;;     throws to `read_char', and `read_char' RETURNS the quit character as an
;;     event and records it.  So on this build a `C-g' that reaches Emacs while
;;     a prompt stands is dispatched as the `C-g' key and shows up in
;;     `(recent-keys)'.
;;
;;   - Emacs already binds `inhibit-quit' to t around process filters
;;     (`read_and_dispose_of_process_output'), process sentinels
;;     (`exec_sentinel') and timer callbacks (`timer_check_2') — the three
;;     contexts named above.  An armed flag is therefore not consumed inside
;;     them, and the `error in process filter: Quit' line this section opens
;;     with is not something a quit arriving during a filter can produce.
;;
;; The 2026-09-12 sweep was read as key DELIVERY -- a synthetic `C-g' that
;; never entered Emacs's input at all, on the evidence that the same runs lost
;; other posted keys (`(recent-keys)' came back missing a `<tab>' and several
;; `<escape>'s).  That reading did not survive 2026-09-13.  The harness fixed
;; its own probe interference and the failure persisted UNCHANGED, while
;; ordinary keys posted the identical way arrived 5 of 5; and the four prompts
;; that refused were all in runs carrying a pending pre-creation, i.e. runs
;; where the focus edge that precedes every keypress ran a GUARDED section.
;; What was eating the chord was this module's own delivery timer -- see THE
;; DELIVERY HALF IS GONE below, which carries the Emacs source that proves the
;; abort it attempted could not take.  e2e/realtest/minibuffer.go carries the
;; sweep evidence.
;;
;; So this guard earns its place on ATOMICITY, not on quit delivery: a section
;; that must not be left half-written runs whole, the quit is left armed for
;; the command loop, and the deferral is recorded so a quit that looks ignored
;; is explainable from the log alone.
;;
;; THE DELIVERY HALF IS GONE, and its removal is the 2026-09-13 fix.  An
;; earlier revision handed a still-armed flag to a zero-delay timer that
;; CLEARED `quit-flag' and then aborted the standing minibuffer itself.  Both
;; halves of that are wrong on this build, and Emacs's own source is what
;; settles it:
;;
;;   - `abort-minibuffers' (src/minibuf.c) reads `this_minibuffer_depth' of
;;     the CURRENT BUFFER and signals `error "Not in a minibuffer"' when the
;;     current buffer is not one.  A timer callback's current buffer is
;;     whatever `timer-event-handler' preserved with `save-current-buffer' --
;;     the workspace buffer the user was in, never the minibuffer -- so the
;;     abort could not take.
;;
;;   - `timer-event-handler' runs the callback inside
;;     `condition-case-unless-debug ... (error (message ...))', so that
;;     `error' became a *Messages* line nobody reads rather than a failure.
;;
;; The flag had already been taken down before either of those.  Net effect:
;; the user's `C-g' was DROPPED -- not honoured, not re-armed, and not even
;; recorded in `(recent-keys)', because our timer consumed the flag before
;; `kbd_buffer_get_event' could turn it into the `C-g' EVENT a standing
;; prompt is bound to abort on.  That is exactly the realtest 5-8 finding:
;; two posted quits in a row against "Initial prompt: ", "Add project
;; directory: ", "Priority: " and "Open workspace: ", prompt still up, chord
;; absent from `(recent-keys)', every time -- and it only happened in runs
;; where a freshly created workspace left a pre-creation PENDING, so the
;; focus edge that precedes every keypress ran the guarded drain tick.
;;
;; THE INVARIANT, stated once and enforced in one place: A QUIT THE USER
;; PRESSED DURING A DEFERRED-QUIT SECTION IS DEFERRED, NEVER DROPPED.  It is
;; the hand-off at the END of the section that has to honour it, and the
;; right hand-off depends on whether a prompt is standing.
;;
;; LEAVING THE FLAG ARMED IS NOT ENOUGH WHEN A MINIBUFFER IS UP, and the
;; 2026-09-13 sweep after the delivery half came out is what proves it.  Three
;; of the four prompts still lost the chord; the one that did NOT -- "Open
;; workspace: ", pressed right after a close, with no pre-creation pending and
;; therefore no guarded section on the focus edge -- arrived and was recorded.
;; The reason is that a guarded section on the focus-in path does not return
;; to `read_char'.  It returns into MORE LISP: the rest of the focus hook
;; runner, the timer that follows, whatever else the edge set off.  The first
;; QUIT check in any of that takes the armed flag and signals `quit' THERE.
;; Inside a standing minibuffer's recursive command loop that quit is caught
;; and printed as a plain `Quit' -- the prompt stays up, and no `C-g' EVENT is
;; ever recorded, because the flag never survived to `read_char''s idle wait.
;;
;; SO A QUIT OWED TO A STANDING PROMPT IS REQUEUED AS THE KEY IT WAS.  The
;; flag comes down and the terminal's quit character (read from
;; `current-input-mode', never hardcoded) goes onto `unread-command-events'.
;; The minibuffer's own binding then reads it as an ordinary key: it is
;; recorded in `(recent-keys)', `abort-minibuffers' runs from the minibuffer
;; itself where its `this_minibuffer_depth' check passes, and the prompt is
;; dismissed exactly as a natively read `C-g' dismisses it.  This is a
;; TRANSLATION of the quit, not a delivery of it: the same user intent, put
;; back into the input stream at the one place that can act on it.
;;
;; WITH NO MINIBUFFER STANDING the flag stays armed, unchanged.  There is no
;; recursive command loop to swallow it, the ordinary command loop is exactly
;; who should take it, and `unread-command-events' would only guess at which
;; keymap ought to see a `C-g' the user aimed at nothing in particular.
;;
;; A deferral also arms an AUDIT: a zero-delay timer that re-reads the flag.
;; It records which hand-off was taken, and if it finds a quit STILL owed with
;; a prompt standing -- a prompt that went up between the section ending and
;; the timer firing -- it requeues it on the same terms.  It never drops one.

(defmacro agent-repl--with-deferred-quit (context &rest body)
  "Run BODY with quitting inhibited, deferring any `C-g' that arrives.
CONTEXT is a string naming the critical section, used only for the
canonical log records written when a quit was in fact deferred.

Returns BODY's value.  A quit requested while BODY ran is handed off as
BODY returns, by `agent-repl--deferred-quit-hand-off': REQUEUED as the
quit character on `unread-command-events' when a minibuffer is standing,
so the prompt's own binding dismisses it and records the key; left armed
in `quit-flag' for the command loop when none is.  Either way the quit is
honoured somewhere the user can see, and an audit is armed so the
deferral and its outcome are both on the record.  See the commentary
above for why the armed flag alone is not enough at a prompt.

Intended for process filters, process sentinels and timer callbacks,
whose run moments the user cannot see and therefore cannot avoid quitting
into.  It is NOT a general error guard: an error signalled by BODY
propagates exactly as it did before."
  (declare (indent 1) (debug (form body)))
  (let ((result (make-symbol "result")))
    `(let ((inhibit-quit t))
       (let ((,result (progn ,@body)))
         (when quit-flag
           (agent-repl--log
            '(:agent-repl-central "process-wide logging and utility state")
            "deferred-quit: C-g arrived inside %s — deferred, handing it off"
            ,context)
           (agent-repl--deferred-quit-hand-off ,context)
           (agent-repl--deferred-quit-arm-audit ,context))
         ,result))))

(defun agent-repl--deferred-quit-char ()
  "Return the terminal's quit character, as `read-event' would yield it.
Read from `current-input-mode' rather than hardcoded: a user who moved
their quit character away from `C-g' must get the key they actually
pressed put back, not the one this module assumed."
  (or (nth 3 (current-input-mode)) ?\C-g))

(defun agent-repl--deferred-quit-hand-off (context)
  "Honour the quit deferred out of CONTEXT, returning the path taken.

`requeued' -- A MINIBUFFER IS STANDING, so `quit-flag' comes down and the
quit character goes onto `unread-command-events'.  The prompt's own
binding reads it as the key it was: recorded in `(recent-keys)', and
`abort-minibuffers' runs from inside the minibuffer where its
`this_minibuffer_depth' check passes.  Leaving the flag armed instead is
what lost three of four prompts on 2026-09-13 -- the focus-in path this
guard runs on returns into more lisp, not into `read_char', and the first
QUIT check there signals a `quit' the recursive command loop prints as
`Quit' with the prompt still up.

`armed' -- NO MINIBUFFER, so the flag is left exactly as it was and the
command loop takes it.  Nothing is queued: there is no prompt whose
keymap a requeued `C-g' belongs to.

Called with `inhibit-quit' bound -- by the guard, and by
`timer-event-handler' for the audit -- so taking the flag down here
cannot itself be interrupted.  It never signals."
  (if (active-minibuffer-window)
      (let ((quit-char (agent-repl--deferred-quit-char)))
        (setq quit-flag nil)
        (push quit-char unread-command-events)
        (agent-repl--log
         '(:agent-repl-central "process-wide logging and utility state")
         "deferred-quit: requeued the quit deferred out of %s as key %s for the standing minibuffer"
         context (single-key-description quit-char))
        'requeued)
    (agent-repl--log
     '(:agent-repl-central "process-wide logging and utility state")
     "deferred-quit: the quit deferred out of %s is still armed for the command loop (minibuffer=none)"
     context)
    'armed))

(defun agent-repl--deferred-quit-arm-audit (context)
  "Arm the audit of a quit deferred out of CONTEXT.

A ZERO-DELAY TIMER, because the audit's question is what happened to the
quit after control left the guarded section, and a timer is the earliest
Lisp context that runs after it: Emacs runs its timers from the very
input wait a deferred quit is owed to.

This function itself touches nothing -- `quit-flag' and
`unread-command-events' are exactly as the hand-off left them when it
returns.  The audit it arms may hand a still-owed quit off again, which
is honouring it, never dropping it."
  (run-at-time 0 nil #'agent-repl--deferred-quit-audit context))

(defun agent-repl--deferred-quit-audit (context)
  "Settle what became of the quit deferred out of CONTEXT.

A quit ALREADY HONOURED is recorded and left alone: re-signalling it here
would abort a second, innocent thing.  A quit STILL OWED goes through
`agent-repl--deferred-quit-hand-off' again, on the same terms as at the
end of the section, because a prompt can go up in the moment between the
section ending and this timer firing -- and a quit owed to a prompt that
now stands must be requeued as a key rather than left for a checkpoint
that will print `Quit' and leave the prompt up.

IT NEVER SIGNALS.  It runs from a timer, and `timer-event-handler'
reduces a signalled `error' to a *Messages* line -- which is how an
earlier revision's failure to abort went unseen -- so it reports through
the canonical log and honours the quit by handing it off, never by
raising it here."
  (if quit-flag
      (agent-repl--deferred-quit-hand-off context)
    (agent-repl--log
     '(:agent-repl-central "process-wide logging and utility state")
     "deferred-quit: the quit deferred out of %s was honoured before the audit ran"
     context)))

;;;; ---- User-facing copy: the one thing the minibuffer is allowed to say ----
;;
;; THE MINIBUFFER CARRIES USER COPY, NEVER EVIDENCE.  A wrapped Go error
;; chain reaching the echo area verbatim — package prefixes, workspace and
;; session and request identifiers, a protobuf enum name — is unreadable, and
;; worse, it makes a BY-DESIGN refusal (a prompt sent to a workspace queued
;; for a merge) look like a broken workspace.
;;
;; So the split is mechanical rather than per-call-site judgment:
;;
;;   `agent-repl--user-message'    ONE short present-tense sentence to the
;;                                 echo area, plus BOTH that sentence and its
;;                                 verbose counterpart to the canonical
;;                                 per-workspace log through `agent-repl--log'.
;;   `agent-repl--user-copy-for-error'  turns a daemon nack / error string
;;                                 into that sentence.
;;
;; The verbose text is never dropped — it is routed.  The log file carries the
;; user line AND the chain, adjacent, so correlating what the user read with
;; what actually failed needs no second source.

(defconst agent-repl--user-message-prefix "agent-repl: "
  "Prefix every echo-area line from `agent-repl--user-message' carries.
Matches the convention the module's existing minibuffer messages already
use, so a routed line is indistinguishable from the ones it replaces.")

(defconst agent-repl--user-copy-patterns
  '(("RENDER_STATE_MERGE_QUEUED"
     . "%s refused — this workspace is queued for a merge; wait for the merge to finish or interrupt it")
    ("RENDER_STATE_MERGE_RUNNING"
     . "%s refused — a merge is running in this workspace; wait for it to finish or interrupt it")
    ("merge exclusivity lease"
     . "%s refused — a merge run owns this session; wait for the merge to finish or interrupt it")
    ("merge lease"
     . "%s refused — a merge run owns this session; wait for the merge to finish or interrupt it")
    ("superseded by the current workspace generation"
     . "%s superseded — a newer connection owns this workspace; the view will resync")
    ("reconnect_superseded"
     . "%s superseded — the reconnect was overtaken; the view will resync")
    ("has no live session controller"
     . "%s refused — this workspace is not live; start or wake it first")
    ("no live session"
     . "%s refused — this workspace is not live; start or wake it first")
    ("has no transcript"
     . "%s refused — the conversation being resumed has no transcript on disk")
    ("not settled"
     . "%s refused — a turn is still in flight; wait for it to finish or interrupt it"))
  "Ordered (SUBSTRING . COPY) table `agent-repl--user-copy-for-error' consults.
SUBSTRING is matched literally and case-insensitively against the daemon's
error text.  COPY is a format string taking exactly one `%s', the verb
naming the command that was attempted, so one entry serves every command
that can earn the same refusal.

ORDER IS LOAD-BEARING: the first match wins, so a more specific substring
must precede any substring it contains.")

(defun agent-repl--user-copy-for-error (error-text verb)
  "Return one short user-facing sentence for ERROR-TEXT about VERB.

ERROR-TEXT is the daemon's raw account — a wrapped Go error chain, a nack
string, anything.  It is READ here and never returned: the sentence this
produces is the only thing a caller may put in the echo area, and the raw
text belongs in the `:detail' of `agent-repl--user-message'.

VERB names the command that was attempted (\"prompt\", \"hibernate\",
\"merge\").  A nil or empty VERB degrades to \"the command\" rather than
producing a sentence with a hole in it.

An UNRECOGNIZED error yields the generic sentence, which still names the
verb and points at the log.  It never quotes ERROR-TEXT: an error nobody
has written copy for is exactly the one whose raw form is least readable."
  (let* ((verb (if (and (stringp verb) (not (string-empty-p verb)))
                   verb
                 "the command"))
         (copy (and (stringp error-text)
                    (not (string-empty-p error-text))
                    (let ((case-fold-search t))
                      (cl-loop for (pattern . template) in agent-repl--user-copy-patterns
                               when (string-match-p (regexp-quote pattern) error-text)
                               return template)))))
    (format (or copy "%s failed — see the workspace log for detail") verb)))

(cl-defun agent-repl--user-message (ws fmt args &key detail)
  "Echo one line of user-facing copy for WS and file it, with DETAIL, in the log.

FMT and ARGS are the USER COPY: one short present-tense sentence saying
what happened and, when it is knowable, what to do about it.  ARGS is a
LIST of format arguments (nil when FMT needs none), matching the
`(ws fmt args)' shape the internal logging helpers already use.

DETAIL is the verbose counterpart — the raw error chain, the identifiers,
the state names.  It NEVER reaches the echo area; it is written to the
same canonical log the user line goes to, so the file carries both and
correlation is trivial.  nil DETAIL files only the user line.

Routing follows `agent-repl--log': workspace-owned when WS names a
workspace, global otherwise.  Returns the echoed text."
  (let* ((copy (cond
                ((not (stringp fmt))
                 ;; A non-string FMT is a caller bug.  Capture it the way the
                 ;; logging ladder does rather than signalling from a
                 ;; presentation helper, and still say something.
                 (agent-repl--log-format-capture-bug fmt)
                 (format "%S" fmt))
                (args (apply #'format fmt args))
                (t fmt)))
         (text (if (string-prefix-p agent-repl--user-message-prefix copy)
                   copy
                 (concat agent-repl--user-message-prefix copy))))
    (agent-repl--log ws "user-message: %s" text)
    (when (and (stringp detail) (not (string-empty-p detail)))
      (agent-repl--log ws "user-message-detail: %s" detail))
    (agent-repl--emit-message text t)
    text))

(defconst agent-repl--backend-output-tail-lines 5
  "Nonblank captured lines a backend-initiation echo line may carry.")

(defconst agent-repl--backend-output-tail-limit 400
  "Maximum characters a backend-initiation echo tail may occupy.")

(defconst agent-repl--backend-output-tail-scan-limit 65536
  "Trailing characters of OUTPUT `agent-repl--backend-output-tail' will scan.

A hard bound on the work the helper can be ASKED to do, not a tuning
preference.  `split-string' with a trim regexp indexes a multibyte
string once per field and each index costs a scan, so splitting a
multi-megabyte capture is effectively quadratic: a `*claude-repld*'
capture grown to tens of megabytes pinned Emacs at 99% CPU for over ten
minutes inside a server-filter eval, where quit is inhibited and the
whole frame therefore froze.

Bounding the INPUT is what makes that unrepresentable rather than
unlikely.  The helper keeps at most
`agent-repl--backend-output-tail-lines' lines capped at
`agent-repl--backend-output-tail-limit' characters, so every character
before this suffix was already destined to be discarded — the result is
unchanged for any capture a caller realistically holds, and the
pathological one now costs one bounded scan.")

(defun agent-repl--backend-output-tail-suffix (text)
  "Return the trailing `agent-repl--backend-output-tail-scan-limit' of TEXT.

Returns TEXT itself when it already fits, so the common case allocates
nothing.  The cut is deliberately NOT aligned to a line boundary:
aligning would make the result depend on whether truncation happened,
whereas an unaligned cut makes `agent-repl--backend-output-tail' of a
string equal to `agent-repl--backend-output-tail' of its bounded suffix.
A leading fragment can only ever survive when the kept lines already
exceed the character cap, which truncates it anyway."
  (if (<= (length text) agent-repl--backend-output-tail-scan-limit)
      text
    (substring text (- (length text) agent-repl--backend-output-tail-scan-limit))))

(defun agent-repl--backend-output-tail (output &optional lines)
  "Return the last LINES nonblank lines of OUTPUT as one echo-safe string.

The FULL captured output always goes to the durable structured record;
this is only the fragment a one-line minibuffer failure can carry, so it
is deliberately lossy and never the sole record of anything.  Returns
\"<no output>\" when OUTPUT carries nothing, so an empty capture is
reported as an empty capture rather than as an absent field.

Only the trailing `agent-repl--backend-output-tail-scan-limit'
characters are examined — see that constant for why an unbounded input
is a freeze rather than merely a slow call."
  (let* ((text (agent-repl--backend-output-tail-suffix
                (if (stringp output) output "")))
         (kept (last (split-string text "\n" t "[ \t\r]+")
                     (or lines agent-repl--backend-output-tail-lines)))
         (joined (string-join kept " / ")))
    (cond
     ((string-empty-p joined) "<no output>")
     ((> (length joined) agent-repl--backend-output-tail-limit)
      (concat "…" (substring joined
                             (- (length joined)
                                agent-repl--backend-output-tail-limit))))
     (t joined))))

(defun agent-repl--backend-phase (ws fmt &rest args)
  "Record a backend-initiation phase transition and ECHO it to the user.

This is the ONE background flow the ladder commentary above exempts from
its no-`message' rule, and the exemption is narrow: artifact builds,
launchd kickstarts, the daemon spawn and the runtime bounce.  The build
step is a synchronous `call-process' that blocks the frame outright, so a
user with no line at all cannot tell a rebuild from a hang — and a bounce
that fails silently looks exactly like a bounce that never started.

Only PHASE TRANSITIONS and outcomes come through here.  The subprocess
spew itself stays on `agent-repl--log', where the runbooks mine it; a
failure echo carries at most `agent-repl--backend-output-tail' of it plus
a pointer at the log file.

FMT and ARGS keep the ladder's format-string signature, so the persisted
`operation' name stays derived from the template and never from a runtime
value."
  (agent-repl--emit-log-record ws "info" "normal" fmt args :message-mode 'backend))

(defun agent-repl--minibuffer-busy-p ()
  "Return non-nil when the user is currently working in the minibuffer.

A STARTUP PHASE MUST NOT TYPE OVER THE USER.  The echo area and the
minibuffer are the same screen real estate, so a background phase line
arriving while a `find-file' prompt or an `M-x' completion is up either
covers what the user is reading or shoves their own prompt aside.  The
phase is still WORTH RECORDING at that moment -- it just is not worth
interrupting for -- so callers route it to the quiet sink instead of
dropping it."
  (or (> (minibuffer-depth) 0)
      (and (minibufferp) t)))

(defun agent-repl--phase-echo (ws fmt &rest args)
  "Record a high-level startup phase for WS and echo it when the user is free.

The same one-call contract as `agent-repl--backend-phase' -- the log
record and the echo-area line are produced by a single call, never by a
log call plus a bare `message' -- with the minibuffer guard
`agent-repl--minibuffer-busy-p' describes: a phase reached while the user
is typing is recorded QUIETLY and never echoed.

Only high-level, once-per-transition phases belong here: the daemon
build, spawn, link-up and outcome, and the workspace bring-up count.
Anything finer-grained is log chatter and belongs on `agent-repl--info'."
  (agent-repl--emit-log-record
   ws "info" "normal" fmt args
   :message-mode (if (agent-repl--minibuffer-busy-p) 'quiet 'backend)))

(defun agent-repl--fatal (ws fmt &rest args)
  "Record a fatal condition for WS and then SIGNAL it.
WS is the workspace name for context (or nil).  FMT and ARGS are formatted
the same way `agent-repl--log' formats them, and the resulting line is
written to the durable sink BEFORE the `error' is signalled so the failure
is captured regardless of whether debug logging is on.  Fires regardless
of `agent-repl-debug'.

This is the ABORT, not a log level: a signalled `error' is the one thing
Emacs always displays, so it is also the only path to the loud sink (see
the severity-gate commentary above).  Reach for it where the caller must
not continue — a refusal, a broken precondition, an unusable input.  When
the branch only needs to RECORD that something failed and carry on, that
is `agent-repl--error' one line below."
  (agent-repl--emit-log-record ws "error" "normal" fmt args
                               :message-mode 'quiet :fatal t))

(defun agent-repl--error (ws fmt &rest args)
  "Log an ERROR for WS to the QUIET sink: the log file and *Messages*.
An `ERROR: \' severity tag is prepended, so call sites pass the bare
message (no literal \"ERROR:\" prefix of their own).

This is the top rung of the ladder and it is a LOGGING level, exactly like
`agent-repl--warn' one rung below it: the JSONL record carries
`level: \"error\"' (the spelling logging-contract.md reserves for a broken
invariant or a failed operation), it is persisted whenever it clears
`agent-repl-log-file-level', and it is emitted quietly so it never flashes
in the modeline.

It does NOT signal.  Recording that something failed and ABORTING the
caller are separate acts, and conflating them makes the error level
unusable for the every-logical-branch instrumentation it exists for: a
branch that logs its own failure and then returns a failure to its caller
must be able to say so without unwinding the stack underneath itself.

A caller that must abort uses `agent-repl--fatal' (one line above), which
records at this same level and then signals; a refusal that already has
its own `error' / `user-error' keeps it and calls this to put the reason
on the record first."
  (if (stringp fmt)
      (agent-repl--emit-log-record ws "error" "normal" (concat "ERROR: " fmt) args
                                   :operation-fmt fmt :message-mode 'quiet)
    ;; A non-string FMT is a caller bug.  Hand it through untouched rather
    ;; than `concat'-ing it (which would raise a wrong-type-argument here and
    ;; bury the real culprit): `agent-repl--build-log-text' already captures a
    ;; backtrace to *agent-repl-log-bug* for exactly this case, and ARGS is
    ;; preserved so nothing about the offending call is lost.
    (agent-repl--emit-log-record ws "error" "normal" fmt args :message-mode 'quiet)))

(defun agent-repl--assert-main-thread (what)
  "Signal an error when called off the main thread; no-op (nil) on main.
WHAT names the guarded operation in the log line and error text.

Guards main-thread-only operations against the AGENTS.md `ns_select_1'
worker-thread trap: anything that reaches `accept-process-output' /
`[NSApp run]' (e.g. the blocking frontend UDS waits — readiness, the
command awaits) deadlocks Emacs when run on a worker thread, and an
indirect call chain can smuggle such an operation onto a worker without
any call site noticing \(the 2026-07-18 freeze arrived via merge worker
-> config reload -> watcher re-arm -> notification drain -> the
then-synchronous frontend HTTP call).  Signaling here converts
the would-be hard deadlock into an ordinary error that the caller's
failure handling surfaces."
  (unless (eq (current-thread) main-thread)
    (agent-repl--do-log
     nil
     "assert-main-thread: REFUSING %s off the main thread (thread=%s) — ns_select_1 worker-thread trap, see AGENTS.md"
     (list what (thread-name (current-thread)))
     t)))

;;; Git and workspace identity

(defun agent-repl--dir-has-git-p (d)
  "Return non-nil if directory D contains a .git directory or file."
  (let ((git (expand-file-name ".git" d)))
    (or (file-directory-p git) (file-regular-p git))))

;;;; ---- Thread-safe process teardown and waiting ----
;;
;; `delete-process' — and `kill-buffer' on a buffer that still owns a live
;; process, which calls it implicitly — can trigger a REDISPLAY
;; (`delete-process' -> status update -> `redisplay_preserve_echo_area' ->
;; `gui_consider_frame_title').  On the macOS NS build redisplay calls into
;; AppKit (`-[NSWindow setTitle:]'), which is main-thread-only: from a
;; worker thread it raises an uncaught ObjC exception, which `abort's Emacs
;; into its fatal-signal handler.  The worker then sits suspended in that
;; handler STILL HOLDING the global Lisp lock, and the main thread
;; deadlocks forever on the next form it evaluates.
;;
;; The same family reaches Emacs through `accept-process-output', which on
;; macOS routes into `ns_select_1' + `[NSApp run]' — also main-thread-only.
;; So a worker thread may neither busy-wait on a process nor tear one down
;; directly; both go through the wrappers below.

(defun agent-repl--defer-to-main-thread (thunk)
  "Schedule zero-arg THUNK to run on the main thread on the next event-loop tick.
Safe to call from any thread, including the main thread itself.

A tick of delay even when already on the main thread is intentional: it
keeps the call semantics uniform across contexts, so a regression caused
by a direct call from a worker cannot hide behind \"works on the main
thread, fails on a worker\"."
  (run-at-time 0 nil thunk))

(defun agent-repl--kill-process-safely (proc)
  "Delete PROC on the MAIN thread, whatever thread this is called from.
See this section's preamble: `delete-process' can redisplay, and redisplay
off the main thread aborts Emacs on macOS.  A no-op for a nil or already
dead PROC.  Returns non-nil when a deletion was performed or scheduled."
  (when (process-live-p proc)
    (if (eq (current-thread) main-thread)
        (progn (delete-process proc) t)
      (agent-repl--log '(:agent-repl-central "process-wide logging and utility state")
                       "kill-process-safely: deferring delete-process %s to main thread"
                       (ignore-errors (process-name proc)))
      (agent-repl--defer-to-main-thread
       (lambda () (when (process-live-p proc) (delete-process proc))))
      t)))

(defun agent-repl--wait-for-process-exit--main (proc timeout-seconds log-tag log-ws)
  "Main-thread wait for PROC, bounded by TIMEOUT-SECONDS.
Busy-waits via `accept-process-output', which is legal on the main thread.
LOG-TAG and LOG-WS, when both non-nil, name the completion log line."
  (let* ((started-at (float-time))
         (deadline (+ started-at timeout-seconds))
         (timed-out nil))
    (while (and (process-live-p proc) (not timed-out))
      (accept-process-output proc 0.2 nil t)
      (when (> (float-time) deadline)
        (setq timed-out t)
        (agent-repl--kill-process-safely proc)))
    (let ((status (if timed-out 'timeout (process-exit-status proc))))
      (when (and log-tag log-ws)
        (agent-repl--log log-ws
                         "%s: process exited status=%S elapsed=%.1fs (main-thread wait)"
                         log-tag status (- (float-time) started-at)))
      status)))

(defun agent-repl--wait-for-process-exit--worker (proc timeout-seconds log-tag log-ws)
  "Worker-thread wait for PROC, bounded by TIMEOUT-SECONDS.
Blocks on a condition variable signalled by a process sentinel and by a
timeout timer.  Does NOT call `accept-process-output', which would route
through `ns_select_1' and trap the worker in main-thread-only AppKit code
on macOS.  LOG-TAG and LOG-WS, when both non-nil, name the completion log
line."
  (let* ((started-at (float-time))
         (mutex (make-mutex
                 (format "agent-repl-await-%s"
                         (or (ignore-errors (process-name proc)) "proc"))))
         (condvar (make-condition-variable mutex))
         (done nil)
         (status nil)
         (timeout-timer nil))
    (set-process-sentinel
     proc
     (lambda (p _event)
       (when (memq (process-status p) '(exit signal))
         (with-mutex mutex
           (unless done
             (setq done t)
             (setq status (process-exit-status p))
             (condition-notify condvar))))))
    ;; Close the install race: a fast child can exit BEFORE the sentinel
    ;; above is installed, in which case Emacs has already consumed the
    ;; status-change notification and the sentinel never fires — the wait
    ;; would then burn the full TIMEOUT-SECONDS for a long-dead process.
    ;; Sample the status once after installing the sentinel; the `done'
    ;; guard keeps a concurrently-firing sentinel from double-completing.
    (when (memq (process-status proc) '(exit signal))
      (with-mutex mutex
        (unless done
          (setq done t)
          (setq status (process-exit-status proc)))))
    (unless done
      (setq timeout-timer
            (run-at-time
             timeout-seconds nil
             (lambda ()
               (with-mutex mutex
                 (unless done
                   (setq done t)
                   (setq status 'timeout)
                   (ignore-errors (agent-repl--kill-process-safely proc))
                   (condition-notify condvar)))))))
    (unwind-protect
        (with-mutex mutex
          (while (not done)
            (condition-wait condvar)))
      (when (timerp timeout-timer) (cancel-timer timeout-timer)))
    (when (and log-tag log-ws)
      (agent-repl--log log-ws
                       "%s: process exited status=%S elapsed=%.1fs (worker-thread wait)"
                       log-tag status (- (float-time) started-at)))
    status))

(defun agent-repl--wait-for-process-exit (proc timeout-seconds &optional log-tag log-ws)
  "Synchronously block until PROC exits or TIMEOUT-SECONDS elapses.
Returns the process exit status (an integer) on clean exit, or the symbol
`timeout' when the deadline elapses — on which PROC is deleted as a side
effect.

Dispatches by calling thread to avoid the macOS worker-thread hazard
described in this section's preamble.  LOG-TAG and LOG-WS, when both
non-nil, are used to emit a single completion log line at the end of the
wait."
  (if (eq (current-thread) main-thread)
      (agent-repl--wait-for-process-exit--main proc timeout-seconds log-tag log-ws)
    (agent-repl--wait-for-process-exit--worker proc timeout-seconds log-tag log-ws)))

(defun agent-repl--capture-process-output (program args &optional suppress-stderr timeout)
  "Run PROGRAM with ARGS, capture stdout, return its trimmed contents.
Internal helper used by the per-binary capturing wrappers below
\(`agent-repl--git-string', `agent-repl--git-string-quiet',
`agent-repl--gh-string-quiet').  Not registered as an external
boundary in its own right because tests mock the per-binary wrappers,
which sit one layer above it.

Worker-thread safe: routes the wait through
`agent-repl--wait-for-process-exit', so on macOS this does NOT trap
in `ns_select_1' + `[NSApp run]' when called from a non-main thread
\(unlike `shell-command-to-string', which always does — that was the
historical hang source for the merge worker).  See AGENTS.md
`ns_select_1 worker-thread trap'.

SUPPRESS-STDERR controls stderr handling:
- nil (default): stderr is merged into the same buffer as stdout
  (matches `shell-command-to-string''s default).
- non-nil: stderr is captured to a separate throwaway buffer and
  discarded (matches the `2>/dev/null' piping that the legacy
  `--git-string-quiet' / `--gh-string-quiet' relied on).

TIMEOUT defaults to 60 seconds.  On expiry the process is killed and
an empty string is returned — no exception is signalled.  This
matches the silent-failure contract that the quiet variants rely on:
init-time callers that may run outside a git repository must not
explode."
  (let* ((timeout (or timeout 60))
         (stdout-buf (generate-new-buffer
                      (format " *agent-repl-capture-%s*" program)))
         (stderr-buf (when suppress-stderr
                       (generate-new-buffer
                        (format " *agent-repl-capture-%s-stderr*"
                                program)))))
    (unwind-protect
        ;; Both branches MUST spawn on a PIPE, never a pty.  The
        ;; merged-stderr branch used `start-process' with the default
        ;; `process-connection-type' (a pty), so children that behave
        ;; differently on a terminal misbehaved here — `git log' saw a
        ;; tty, spawned its pager, and hung until TIMEOUT on every
        ;; call.  Those 60s stalls froze the UI whenever the caller was
        ;; a main-thread timer, and their timeout path fed the
        ;; worker-thread `kill-buffer' deadlock (2026-07-18 freezes).
        (let ((proc (if suppress-stderr
                        (make-process ;; ALLOW-EXTERNAL-BOUNDARY
                         :name (format "agent-repl-capture-%s" program)
                         :command (cons program args)
                         :buffer stdout-buf
                         :stderr stderr-buf
                         :connection-type 'pipe
                         :noquery t)
                      (make-process ;; ALLOW-EXTERNAL-BOUNDARY
                       :name (format "agent-repl-capture-%s" program)
                       :command (cons program args)
                       :buffer stdout-buf
                       :connection-type 'pipe
                       :noquery t))))
          (agent-repl--log-verbose
           '(:agent-repl-central "process-wide logging and utility state")
           "capture-process-output: spawned program=%s args=%S suppress-stderr=%s timeout=%s"
           program args suppress-stderr timeout)
          (set-process-query-on-exit-flag proc nil)
          ;; Install a no-op sentinel BEFORE waiting.  Left alone, the
          ;; process keeps Emacs's `internal-default-process-sentinel',
          ;; which appends a human-readable "Process NAME finished" line
          ;; into the very buffer this helper reads back as command output.
          ;; On the main-thread wait path (`accept-process-output') that
          ;; default sentinel fires before the buffer is read, folding the
          ;; status line into the returned string — that is what poisoned a
          ;; cached `:branch-name' into a multi-line, unusable git ref.  The
          ;; worker-thread wait installs its own sentinel and is already
          ;; immune; `#'ignore' covers the main-thread path the same way.
          (set-process-sentinel proc #'ignore)
          (let ((status (agent-repl--wait-for-process-exit
                         proc timeout nil nil)))
            (cond
             ((eq status 'timeout)
              ;; A timeout here means a child outlived its budget and was
              ;; killed; the silent "" return otherwise erases all trace
              ;; of it.  Log so post-mortems can see WHICH command stalled
              ;; (the per-call cherry-pick-base stalls were invisible
              ;; until this line existed).
              (agent-repl--log '(:agent-repl-central "process-wide logging and utility state")
                                "capture-process-output: TIMEOUT after %ss %s %S"
                                timeout program args)
              "")
             (t
              (with-current-buffer stdout-buf
                (let ((output (string-trim
                               (buffer-substring-no-properties
                                (point-min) (point-max)))))
                  (agent-repl--log-verbose
                   '(:agent-repl-central "process-wide logging and utility state")
                   "capture-process-output: completed program=%s args=%S status=%S output-length=%d"
                   program args status (length output))
                  output))))))
      ;; Buffer cleanup MUST be thread-safe: on the timeout path the
      ;; child can still be alive (its `delete-process' was deferred to
      ;; the main thread by `agent-repl--kill-process-safely'), and a
      ;; bare `kill-buffer' on a process-owning buffer from the merge
      ;; worker implicitly `delete-process'es -> redisplays -> AppKit
      ;; `setTitle' off-main -> abort with the global Lisp lock held —
      ;; the 2026-07-18 hard deadlock.  The `fboundp' fallback only
      ;; fires during module load (worktree.el, which owns the safe
      ;; wrapper, loads after core.el) — always on the main thread,
      ;; where a bare kill is legal.
      (dolist (buf (list stdout-buf stderr-buf))
        (when (buffer-live-p buf)
          (if (fboundp 'agent-repl--kill-buffer-safely)
              (agent-repl--kill-buffer-safely buf)
            (kill-buffer buf)))))))

(defun agent-repl--git-string (&rest args)
  "Run a synchronous git command and return its trimmed output.
ARGS are the git subcommand and arguments.
Note: stderr is included in the output (Emacs default).  Use
`agent-repl--git-string-quiet' when errors should be silently swallowed.

Routes through `agent-repl--capture-process-output' so this is safe
to call from worker threads on macOS — `shell-command-to-string'
would trap in `ns_select_1' + `[NSApp run]'.  See AGENTS.md
`ns_select_1 worker-thread trap'."
  (agent-repl--capture-process-output "git" args nil))

(defun agent-repl--git-string-quiet (&rest args)
  "Like `agent-repl--git-string' but suppress stderr.
Returns an empty string when git fails, rather than error text.
Suitable for init-time calls that may run outside a git repository.

Routes through `agent-repl--capture-process-output' for worker-thread
safety on macOS; see `agent-repl--git-string'."
  (agent-repl--capture-process-output "git" args t))

(defun agent-repl--gh-string-quiet (&rest args)
  "Run a synchronous `gh' command and return its trimmed stdout.
Stderr is suppressed.  ARGS are the `gh' subcommand and arguments.
Returns an empty string when `gh' fails (no PR for branch, not
authenticated, etc.).  The wrapper IS the external boundary for the
GitHub CLI: tests must mock this function via `cl-letf' rather than
invoke real `gh' (see AGENTS.md \"No External Processes or External
State in Tests\").

Routes through `agent-repl--capture-process-output' for worker-thread
safety on macOS; see `agent-repl--git-string'."
  (agent-repl--capture-process-output "gh" args t))

(defun agent-repl--async-gh (label dir args callback)
  "Run `gh ARGS' asynchronously in DIR and call CALLBACK on completion.
LABEL is used to name the process and its output buffer.
CALLBACK is called as `(CALLBACK ok output)' where OK is non-nil when
gh exited with status 0 and OUTPUT is the trimmed stdout string.

This IS the external-boundary wrapper for async `gh' invocations —
tests must mock this function via `cl-letf' rather than spawn real
gh (see AGENTS.md \"No External Processes or External State in Tests\")."
  (let* ((buf (generate-new-buffer (format " *agent-repl-%s*" label)))
         (default-directory (file-name-as-directory dir))
         (proc (apply #'start-process  ;; ALLOW-EXTERNAL-BOUNDARY
                      (format "agent-repl-%s" label)
                      buf
                      "gh" args)))
    (agent-repl--log '(:agent-repl-central "process-wide logging and utility state")
                      "async-gh: spawned label=%s dir=%s args=%S"
                      label default-directory args)
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel
     proc
     (lambda (p event)
       (agent-repl--async-gh-handle-completion label callback p event)))))

(defun agent-repl--async-gh-handle-completion (label callback process event)
  "Handle one async-gh PROCESS sentinel EVENT for LABEL and CALLBACK.
Only terminal events invoke CALLBACK.  Both successful and abnormal exits are
terminal: callers must receive `ok=nil' for a failed `gh' command rather than
silently waiting forever.  This helper owns the completion branch separately
from the external spawn boundary, so its pure Elisp behavior is directly
testable with process fixtures."
  (if (process-live-p process)
      (agent-repl--log-verbose '(:agent-repl-central "process-wide logging and utility state")
                                "async-gh: nonterminal-sentinel label=%s event=%S"
                                label event)
    (let* ((buffer (process-buffer process))
           (output (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (buffer-substring-no-properties
                        (point-min) (point-max)))))
           (status (process-exit-status process))
           (ok (zerop status))
           (safe-output (or output "")))
      (when (buffer-live-p buffer)
        ;; Sentinels may be serviced while a worker is waiting on the same
        ;; command, so teardown must retain worktree.el's thread-safe path.
        ;; `async-gh' can only run after the full module load, whose order
        ;; defines `agent-repl--kill-buffer-safely' before a caller can spawn.
        (agent-repl--kill-buffer-safely buffer))
      (agent-repl--log '(:agent-repl-central "process-wide logging and utility state")
                        "async-gh: completed label=%s status=%s ok=%s event=%S output-length=%d"
                        label status ok event (length safe-output))
      (funcall callback ok safe-output))))

(defun agent-repl--signal-process (pid sig)
  "Send signal SIG to process PID (an external-state mutation).
This IS the external-boundary wrapper for `signal-process': tests must
mock this function via `cl-letf' rather than signal a real OS process
\(see AGENTS.md \"No External Processes or External State in Tests\")."
  (signal-process pid sig)) ;; ALLOW-EXTERNAL-BOUNDARY

(defun agent-repl--make-process-git (name args sentinel)
  "Async git via `make-process'.
NAME is the process name (a string); ARGS is the git subcommand
argument list (no leading \"git\"); SENTINEL is the process sentinel.
Returns the live process so the caller can record / kill / inspect it.

This IS the external-boundary wrapper for `make-process'-style async
git invocations — distinct from `agent-repl--async-git', which uses
the older `start-process' API with a process-PUT callback.  Tests must
mock this function via `cl-letf' rather than spawn real git (see
AGENTS.md \"No External Processes or External State in Tests\").

`:connection-type \\='pipe' / `:noquery t' / `:buffer nil' are baked in
because every existing caller wants the same shape; if a future
caller needs different keywords, extend the signature rather than
introducing a sibling raw `make-process' site."
  (make-process ;; ALLOW-EXTERNAL-BOUNDARY
   :name name
   :command (cons "git" args)
   :connection-type 'pipe
   :noquery t
   :buffer nil
   :sentinel sentinel))

;;;; --- External-boundary registry -----------------------------------------
;;
;; Every function that wraps an external process or external-state side
;; effect MUST be listed here.  The test harness (`test-helpers.el')
;; installs unmocked-call guards on every entry at load time so any test
;; that fails to `cl-letf' over the wrapper fails LOUDLY rather than
;; silently shelling out to the real binary.
;;
;; **There is no automated backstop for missing wrappers.**  If you add
;; a raw `(shell-command-to-string ...)' / `(call-process ...)' /
;; `(start-process ...)' to production code without extracting it into
;; a wrapper, NOTHING — not the test harness, not the pre-commit hook,
;; not the registry — will catch you.  The agent's diligence on every
;; diff IS the enforcement.  Audit every change for raw subprocess
;; calls before committing; see AGENTS.md "No External Processes or
;; External State in Tests" for the explicit per-diff checklist.
;;
;; When adding a new wrapper:
;;   1. Define it in core.el (or the closest production file) — body
;;      must do nothing but invoke the external thing.
;;   2. Add its symbol here in the SAME commit.
;;   3. Update AGENTS.md "No External Processes or External State in
;;      Tests" if a new naming-convention class is introduced.

(defvar agent-repl--external-boundary-functions
  '(agent-repl--git-string
    agent-repl--git-string-quiet
    agent-repl--async-git
    agent-repl--gh-string-quiet
    agent-repl--early-git-string
    agent-repl--make-process-git
    agent-repl--async-gh
    agent-repl--signal-process
    agent-repl--frontend-run-build-script
    agent-repl--frontend-file-mtime
    agent-repl--frontend-source-files
    agent-repl--frontend-artifact-exists-p
    agent-repl--frontend-probe-boot-claim
    agent-repl--frontend-spawn-daemon
    agent-repl--frontend-run-log-tail
    agent-repl--frontend-stdio-log-tail
    agent-repl--launchctl-call
    agent-repl--shim-service-file-sha256
    agent-repl--shim-service-read-stamp
    agent-repl--shim-service-write-stamp
    agent-repl--shim-store-socket-present-p
    agent-repl--elisp-reload-load-file
    agent-repl--frontend-make-webview-buffer
    agent-repl--frontend-webview-execute-script-value
    agent-repl--frontend-webview-live-widget
    agent-repl--frontend-widget-size
    agent-repl--frontend-resize-widget
    agent-repl--frontend-webview-reload-widget
    agent-repl--frontend-webview-navigate-widget
    agent-repl--frontend-webview-uri
    agent-repl--image-call-process
    agent-repl--image-call-process-to-file
    agent-repl--image-executable-find
    agent-repl--prompt-summary-process-start
    agent-repl--prompt-summary-process-send-input
    agent-repl-connect--open-socket)
  "Symbols of every external-process or external-state-mutation wrapper.
Each MUST be mocked by tests that reach it via production code.  The
test harness installs guards so unmocked invocations fail loudly.

Maintainer rule: when adding a new external-boundary wrapper, you
MUST register it here in the same commit that introduces it.  There
is no static lint backstop — the agent's audit of every diff for
raw subprocess calls is the only enforcement.")

;; Lazily-populated debug accessor.  Originally a `defvar' whose default
;; value shelled out to git at module load time — that real-git call
;; fired during every test-suite run and violated AGENTS.md "No
;; External Processes or External State in Tests" before any test had
;; a chance to install a mock.  Now `nil' until first request via
;; `agent-repl-print-git-branch', which caches the result.
(defvar agent-repl-git-branch nil
  "Cached git branch active when `agent-repl-print-git-branch' was first called.
Populated lazily on first call; remains nil until then.  Do not rely
on this being set at load time.")

(defun agent-repl-print-git-branch ()
  "Print the git branch that was active when agent-repl config was loaded.
Lazily computes and caches the value on first invocation."
  (interactive)
  (let ((cache-hit (not (null agent-repl-git-branch))))
    (unless agent-repl-git-branch
      (setq agent-repl-git-branch
            (agent-repl--git-string-quiet "rev-parse" "--abbrev-ref" "HEAD")))
    (agent-repl--log '(:agent-repl-central "process-wide logging and utility state") "print-git-branch: cache-hit=%s branch=%S"
                      cache-hit agent-repl-git-branch))
  (message "agent-repl loaded on branch: %s" agent-repl-git-branch))

(defvar agent-repl--path-canonical-cache nil
  "A `let'-bound memo table for `agent-repl--path-canonical', or nil.

nil — the default everywhere — means NO memoization: every call asks the
filesystem, which is the only honest answer for a path that may have moved
since it was last resolved.

A caller that is about to resolve the SAME handful of paths hundreds of
times inside one uninterruptible operation binds a fresh hash table here
for exactly the length of that operation.  `file-truename' is a syscall per
path component, and resolving a daemon path to a workspace name scans every
live workspace and canonicalizes each one's `:project-dir'
\(`agent-repl--ws-dir-owner'), so a connect snapshot that resolves R records
against W workspaces costs R*W of them.

The table is per-operation and never global: a retained cache would outlive
the no-yield guarantee that makes it correct and start answering with a
path's history instead of its state.")

(defun agent-repl--path-canonical (path)
  "Return a canonical, stable string for PATH suitable for hashing.
Expands tildes and symlinks via `file-truename', then strips any
trailing slash via `directory-file-name' so that the same directory
always produces the same hash.

Memoized through `agent-repl--path-canonical-cache' when a caller has bound
one; see that variable for the scope a memo is valid in."
  (if (null agent-repl--path-canonical-cache)
      (directory-file-name (file-truename path))
    (or (gethash path agent-repl--path-canonical-cache)
        (puthash path (directory-file-name (file-truename path))
                 agent-repl--path-canonical-cache))))

(defun agent-repl--workspace-dir-hash ()
  "Return the directory hash of the current workspace, or nil.
The hash is md5hex of the canonical project root from the workspace hashmap,
truncated to `agent-repl-workspace-dir-hash-length'.  It identifies a
DIRECTORY, not a workspace: a record's `workspace_id' is the daemon-minted
id (`agent-repl--ws-daemon-log-id').
Returns nil when no workspace has a registered `:project-dir' — callers are
expected to only invoke this from contexts where a workspace is active."
  (let* ((ws (agent-repl--ws-current-name))
         (root (ignore-errors (agent-repl--ws-dir ws)))
         (hash (when root
               (substring (md5 (agent-repl--path-canonical root))
                          0 agent-repl-workspace-dir-hash-length))))
    (agent-repl--log-verbose ws "workspace-dir-hash: ws=%s root=%s hash=%s" ws root hash)
    hash))

;;; Workspace state management
;;
;; The `agent-repl--workspaces' hash table, its accessors
;; (`--ws-get'/`--ws-put'/`--ws-del'), the liveness predicates
;; (`--ws-live-p'/`--live-ws-names'), and the runtime-keys constant
;; were extracted into `workspace.el' during the render-state
;; unification refactor.  `workspace.el' is now the sole owner of
;; that hash and exposes the wrapper API every other file uses; see
;; AGENTS.md ("Workspace state encapsulation") for the contract.
;;
;; Two helpers in this file (`--ws-dir-hash-cached' and
;; `--format-ws-metadata' above) still `gethash' / `puthash' on the
;; var directly.  They are core logging primitives — wrapping them
;; via `--ws-get'/`--ws-put' would create a logging-to-workspace
;; cycle because `--ws-put' itself calls `--do-log' on stub-create.
;; Treat these as part of the encapsulation boundary (they live
;; immediately upstream of the wrapper API rather than downstream).

(declare-function agent-repl-host--live "host" (ws))
(declare-function agent-repl-host-session-id "host" (ws))

(defun agent-repl--ws-observed-agent-repl-session-id (ws)
  "Return the daemon session id in WS's latest host push, or nil.
The id is read at record construction time and never persisted as Emacs state.
Nil is valid before a host push and for a workspace with no session."
  (when (fboundp 'agent-repl-host-session-id)
    (agent-repl-host-session-id ws)))

(defun agent-repl--ws-observed-claude-session-id (ws)
  "Return the vendor conversation uuid WS's session is CURRENTLY on, or nil.

FOR OBSERVABILITY ONLY, and read straight off the last `HostWorkspace'
the daemon pushed rather than from anything Emacs remembers.  Emacs holds
no durable copy of this value and must never acquire one: a persisted
vendor uuid made Emacs a second authority on which conversation a workspace
owns, and when its copy went stale five workspaces opened fresh
conversations over intact transcripts.  The daemon owns that question now.

The value is `HostSessionLive.claude.session_id', which the host stream
already decodes and `host.el' already stores; it is UNSET while no vendor
conversation exists yet, which the oneof allows on exactly this message.

Nil is a normal answer — before the first pushed frame, for a workspace
whose session is not live, and for one whose vendor conversation has not
started.  An unattributed log record is ACCEPTED by the daemon; a
misattributed one is what gets rejected, so guessing here would be strictly
worse than saying nothing.

Never enters the logger: JSON record construction calls this while the
logging stack is already active, and instrumenting it would recursively
construct another workspace record."
  (when (fboundp 'agent-repl-host--live)
    (let ((vendor (plist-get (agent-repl-host--live ws) :vendor-info)))
      (when (eq (plist-get vendor :arm) :claude)
        (let ((id (plist-get (plist-get vendor :value) :session-id)))
          ;; An EMPTY id is the vendor naming nothing, so it is answered as the
          ;; "not yet" it is.  Anything else is handed on unexamined: a
          ;; malformed identity is the record builder's invariant violation to
          ;; raise, and screening it here would hide it.
          (if (equal id "") nil id))))))

(defvar-local agent-repl--owning-workspace nil
  "Workspace name that owns this agent session.
Set when the user sends a message; used to correctly target workspace
state changes regardless of which persp the buffer drifts into.")
(put 'agent-repl--owning-workspace 'permanent-local t)

(defun agent-repl--buffer-owner (buf)
  "Return the workspace name that owns BUF, or nil.
Reads the permanent-local `agent-repl--owning-workspace'.  Returns nil
for a nil or dead BUF, so callers need not guard liveness themselves."
  (and (buffer-live-p buf)
       (buffer-local-value 'agent-repl--owning-workspace buf)))

(defun agent-repl--foreign-owned-buffer-p (buf ws)
  "Return non-nil if BUF is an agent buffer owned by a workspace other than WS.
A buffer is foreign-owned when its owner (see `agent-repl--buffer-owner')
is a non-nil name unequal to WS.  Buffers with no owner (nil) — e.g. magit,
file, or other non-agent buffers that persp-mode swept into a perspective
or window — are NOT foreign and stay eligible for teardown.  Guards against
persp-mode drifting another workspace's live agent panel into this persp,
which would otherwise kill that workspace's session along with WS's own."
  (let ((owner (agent-repl--buffer-owner buf)))
    (and owner (not (equal owner ws)))))

;;; Buffer naming and predicates

;; The input buffer uses the "agent-panel-input-" prefix to distinguish
;; it from other agent-repl utility buffers (e.g. *agent-repl-dump*,
;; *agent-repl-log-bug*).  The webview buffer lives in its own
;; "agent-frontend-" namespace (see `agent-repl--frontend-buffer-re'
;; below).  Each workspace also has an in-memory "-log" buffer, whose
;; ownership is set through `agent-repl--create-buffer'.

(defconst agent-repl--input-buffer-re
  "^\\*agent-panel-input-[[:alnum:]_-]+\\( [^*]*\\)?\\*$"
  "Regexp matching agent input buffer names.
For example, *agent-panel-input-my-workspace*.

The OPTIONAL trailing segment is the workspace's display title
\(`agent-repl-host-display-title'), which host.el appends to the name
when the daemon pushes `naming' — titles name the buffers (fanout §7).
It is separated by a space, which the workspace-identity segment can
never contain, so the identity stays recoverable
\(`agent-repl--extract-panel-id') and a titled buffer is still an agent
panel everywhere the predicates ask.")

(defconst agent-repl--workspace-log-buffer-re "^\\*agent-panel-log-[[:alnum:]_-]+\\*$"
  "Regexp matching workspace-owned live log buffers.
For example, *agent-panel-log-my-workspace*.")

(defconst agent-repl--frontend-buffer-re "^\\*agent-frontend-[[:alnum:]_-]+\\*$"
  "Regexp matching gui webview buffer names (e.g. *agent-frontend-my-workspace*).
Mirrors `agent-repl-frontend-buffer-name-format'.  Kept as its own
namespace rather than folded into `agent-repl--input-buffer-re' because
the two buffers are named by entirely different schemes
\(\"agent-panel-input-\" vs \"agent-frontend-\"); predicates that need
both — `agent-repl--agent-panel-buffer-p' and
`agent-repl--agent-view-buffer-name-p' — OR the two regexes together
rather than matching either name against one shared pattern.")

(defun agent-repl--sanitize-ws-name (name)
  "Return NAME with unsafe characters replaced by underscores.
Keeps alphanumerics, hyphens, and underscores.  Returns nil for nil NAME."
  (when name
    (replace-regexp-in-string agent-repl-ws-name-allowed-chars-re "_" name)))

(defun agent-repl--buffer-name (&optional suffix ws)
  "Return a workspace-specific buffer name.
The result is like *agent-panel-WS* or *agent-panel-input-WS*.
SUFFIX, if provided, is inserted before the workspace name (e.g. \"-input\").
WS, if provided, is the workspace name; otherwise uses the current workspace.
Signals an error when the resolved workspace name is nil or empty — an
empty id produces a degenerate name like *agent-panel-input-*, which
fails to match `agent-repl--input-buffer-re' (it requires at least one
character after the \"-input-\" segment), causing `agent-repl--sync-panels'
to delete the input panel as orphaned."
  (let* ((ws-name (or ws (agent-repl--ws-current-name)))
         (safe (agent-repl--sanitize-ws-name ws-name)))
    (when (or (null safe) (string-empty-p safe))
      (agent-repl--log ws-name
                        "buffer-name: FAILED suffix=%S ws=%S sanitized=%S reason=empty-workspace-name"
                        suffix ws-name safe)
      (error "agent-repl--buffer-name: empty workspace name (ws=%S, +workspace-current-name=%S, sanitized=%S)"
             ws (agent-repl--ws-current-name) safe))
    (let ((name (format agent-repl-panel-buffer-name-format (or suffix "") safe)))
      ;; Silent when the log sink is resolving its own buffer: instrumenting
      ;; that path makes every workspace-scoped record emit a second record.
      (unless agent-repl--log-sink-reentrant
        (agent-repl--log-verbose ws-name "buffer-name: suffix=%s ws=%s name=%s" suffix ws-name name))
      name)))

(defun agent-repl--input-buffer-name (ws title)
  "Return WS's input buffer name carrying TITLE.
The canonical name (`agent-repl--buffer-name') with TITLE appended after
a space, so `agent-repl--input-buffer-re' still matches it and the
identity segment is still the first thing after the prefix.  TITLE equal
to WS is the daemon's not-yet-derived fallback and adds nothing, so it
yields the bare canonical name; asterisks are dropped from TITLE because
they are the name form's own delimiter.

The webview buffer is deliberately NOT titled this way: its name is a
LOOKUP KEY (`agent-repl--frontend-webview-buffer-name'), derived from the
workspace name by callers that never saw the title."
  (let ((base (agent-repl--buffer-name "-input" ws))
        (clean (and (stringp title)
                    (string-trim (replace-regexp-in-string "[*\n]" "" title)))))
    (if (or (null clean) (string-empty-p clean) (equal clean ws))
        base
      (concat (substring base 0 (1- (length base))) " " clean "*"))))

(defun agent-repl--create-buffer (ws &optional suffix)
  "Create a workspace-owned buffer for WS and return it.
SUFFIX is passed to `agent-repl--buffer-name' to select the buffer's
role: nil for the bare *agent-panel-WS* form, \"-input\" for the input
buffer (*agent-panel-input-WS*) — the input buffer is the only one any
current caller creates through this path.

Single entry point for every workspace-owned buffer.  Derives the
canonical name, sets `agent-repl--owning-workspace' buffer-locally
(permanent-local so it survives subsequent major-mode activation), and
registers the buffer with WS's perspective so it appears in
`+workspace-buffer-list' and related listings.

Idempotent — `get-buffer-create' returns an existing buffer of the
same name, and `persp-add-buffer' internally no-ops when the buffer is
already in the perspective.  Skips persp attachment when WS is nil or
no perspective named WS exists (e.g. early in session startup).

Do not instrument this helper through the logging ladder: the workspace
log sink calls it while servicing every workspace-scoped log line, so a
log here would recursively create and append log lines."
  (let ((buf (get-buffer-create (agent-repl--buffer-name suffix ws))))
    (with-current-buffer buf
      (setq-local agent-repl--owning-workspace ws))
    (when ws
      (when-let ((persp (agent-repl--ws-resolve-persp ws)))
        (agent-repl--ws-add-buffer buf persp nil)))
    buf))

(defun agent-repl--agent-panel-buffer-p (&optional buf)
  "Return non-nil if BUF (default: current buffer) is an agent-repl buffer.
Matches the input composer, webview, and workspace-owned live log buffer."
  (let ((name (buffer-name (or buf (current-buffer)))))
    (or (string-match-p agent-repl--input-buffer-re name)
        (string-match-p agent-repl--workspace-log-buffer-re name)
        (string-match-p agent-repl--frontend-buffer-re name))))

(defun agent-repl--agent-view-buffer-name-p (name)
  "Return non-nil when NAME is the buffer a workspace SHOWS its agent in.
Now that the vterm frontend is gone, that is simply the webview buffer —
the only place a workspace renders its agent — so this collapses to
`agent-repl--frontend-buffer-re' alone.  Kept as its own named predicate
(rather than inlining the regex at call sites) so callers ask the
semantic RENDERING question — \"where does the user watch this agent\"
— instead of matching a regex directly.

Takes a NAME rather than a buffer because both callers need it that way —
one walks live buffers, the other walks a saved `window-state-get' tree,
where buffers survive only as their names.  The input panel is excluded
for the same reason it always was: a saved layout holding only the input
panel is not a workspace showing its agent."
  (and (stringp name)
       (string-match-p agent-repl--frontend-buffer-re name)))

(defun agent-repl--agent-view-buffer-p (&optional buf)
  "Return non-nil if BUF (default: current buffer) is a workspace's agent view.
Buffer-shaped form of `agent-repl--agent-view-buffer-name-p'."
  (agent-repl--agent-view-buffer-name-p
   (buffer-name (or buf (current-buffer)))))

(defun agent-repl--agent-input-buffer-name-p (name)
  "Return non-nil when NAME is a workspace's agent INPUT composer buffer.
The companion of `agent-repl--agent-view-buffer-name-p': the two together
are the pair the owner calls a workspace's agent-repl PANELS (the webapp
panel and the input window), and the tab bar's full-vs-partial extent
asks for both.

Takes a NAME rather than a buffer for the same reason the view predicate
does — one caller walks live buffers, the other a saved
`window-state-get' tree, where buffers survive only as their names."
  (and (stringp name)
       (string-match-p agent-repl--input-buffer-re name)))

(defun agent-repl--agent-input-buffer-p (&optional buf)
  "Return non-nil if BUF (default: current buffer) is an agent input composer.
Buffer-shaped form of `agent-repl--agent-input-buffer-name-p'."
  (agent-repl--agent-input-buffer-name-p
   (buffer-name (or buf (current-buffer)))))

(defun agent-repl--non-user-buffer-p (buf)
  "Return non-nil if BUF is not a user-facing buffer.
Matches agent panel buffers, minibuffers, and dead/nil buffers.
BUF may be a buffer object or a name string."
  (let* ((b (if (stringp buf) (get-buffer buf) buf))
         (name (and b (buffer-name b))))
    (or (not name)
        (agent-repl--agent-panel-buffer-p b)
        (string-match-p "^ \\*Minibuf" name))))

;;; Buffer background color
;;
;; Moved here from the now-deleted overlay.el, which otherwise existed
;; only for the vterm hide-overlay / font-scale / color-advice machinery.
;; This survives because `agent-repl-input-mode' (input.el) calls
;; `agent-repl--set-buffer-background' to tint the input composer.
;; `agent-repl--grey-hex' (its only production caller) was deleted as
;; dead code, so `agent-repl--rgb-hex' is now used directly.

(defun agent-repl--rgb-hex (r g b)
  "Return a #rrggbb hex color string for channel values R, G, B (0-255 each)."
  (format "#%02x%02x%02x" r g b))

(defun agent-repl--set-buffer-background (color)
  "Set default and fringe background to COLOR in the current buffer.
COLOR is any Emacs color spec, e.g. a #rrggbb hex string."
  (face-remap-add-relative 'default :background color)
  (face-remap-add-relative 'fringe :background color))

;;; Harness-injected (meta) prompt spans
;;
;; Some prompts agent-repl sends carry text the USER never typed: the
;; autonomous-execution preamble, the one-shot wrap-up gate (worktree.el),
;; and the on-demand read-directive pointing at the metaprompt file
;; (input.el).  The agent must receive all of it verbatim, but a human
;; reading the conversation wants only their own words back.
;;
;; So each injected span is bracketed with inert HTML-comment markers at
;; the point it is composed.  The markers are the ONE source of truth for
;; "this text is harness-injected": the gui frontend (webapp) hides marked
;; spans from the user-turn bubble so the human only ever sees their own
;; words.

(defconst agent-repl--meta-open "<!--agent-repl:meta-->"
  "Opening marker bracketing a harness-injected span of a sent prompt.
Paired with `agent-repl--meta-close'.  Kept in sync with the webapp's
`META_OPEN' (webapp/src/meta.ts).")

(defconst agent-repl--meta-close "<!--/agent-repl:meta-->"
  "Closing marker bracketing a harness-injected span of a sent prompt.
Paired with `agent-repl--meta-open'.")

(defun agent-repl--meta-wrap (text)
  "Bracket TEXT as a harness-injected span with the meta markers.
TEXT reaches the agent verbatim; the markers only tell a frontend that
the span was injected rather than typed by the user."
  (concat agent-repl--meta-open text agent-repl--meta-close))

;;; Workspace helpers

(defun agent-repl--current-ws-p (ws)
  "Return non-nil when WS is the currently active workspace name.

NO WORKSPACE SELECTED IS AN ANSWER, NOT AN ERROR.
`agent-repl--ws-current-name' documents nil as a legal return — persp-mode
unloaded, or startup before the workspace system is ready — and nil is
simply not WS.  `string=' would signal `wrong-type-argument' on it, and
this predicate is read from the finish-edge reactions, where a signal
would abort the remaining reactions for that edge."
  (let ((current (agent-repl--ws-current-name)))
    (and (stringp ws) (stringp current) (string= ws current))))

;;;; ---- Heartbeat assertion ----
;;
;; `--cancel-all-timers' runs at the TOP of this file, and the timers it
;; clears are re-armed only by their owner files.  A hot-load of a set that
;; contains core.el but not all four owners therefore leaves the module with
;; no heartbeat at all: the 1Hz `agent-repl--update-all-workspace-states'
;; tick that repaints the tab bar is gone, and the tabs freeze.
;;
;; The assertion below closes that hole from the core side, so even a BARE
;; core.el load self-heals.  It is deliberately loud: a re-arm here means
;; something upstream stranded a timer, and that is worth a WARNING on the
;; record even though the state is repaired.

(defvar agent-repl--heartbeat-assert-deferral-timer nil
  "Idle timer holding a deferred `agent-repl--assert-heartbeat-armed' run, or nil.
Set by `agent-repl--assert-heartbeat-armed-when-owners-load' when the
owner files have not been loaded yet.  Held OUTSIDE `agent-repl--timers'
on purpose: it must survive the very `--cancel-all-timers' whose fallout
it exists to check for.")

(defcustom agent-repl-heartbeat-assert-defer-delay 1.0
  "Idle seconds to wait before re-checking the timer contract on a cold load.
Only used when core.el is loaded before its timer owners, which is the
normal cold-boot order: config.el loads core.el first, then status.el
and autosave.el."
  :type 'number
  :group 'agent-repl)

(defun agent-repl--assert-heartbeat-armed ()
  "Verify every key in `agent-repl--required-timer-keys' has a live timer.

Any key found un-armed is re-armed by calling its owner's arm function,
and the strand is reported naming the key, the arm function, and whether
the re-arm took.  A key whose owner has not been loaded (its arm function
is not `fboundp') cannot be re-armed and is reported as unavailable rather
than silently passed over.

Severity splits on whether the condition PERSISTED.  A strand that heals
is self-correcting bookkeeping, not a fault, so `outcome=stranded' and
`outcome=rearmed' are recorded at `agent-repl--info': fully on the durable
record, with the same key and arm-fn detail, but without a `WARNING:' tag
that would train the reader to ignore warnings.  The two outcomes that
OUTLIVE the check — `outcome=rearm-failed' (the arm function ran and the
key is still un-armed) and `outcome=unavailable' (nothing can re-arm it at
all) — stay at `agent-repl--warn'.

Returns a plist:
  :armed        keys that were already armed
  :rearmed      keys that were stranded and are now armed again
  :failed       keys whose arm function ran but left the key un-armed
  :unavailable  keys whose owner file is not loaded

Quiet when every key is armed: the only output in that case is a verbose
log line."
  (let ((armed nil) (rearmed nil) (failed nil) (unavailable nil))
    (dolist (entry agent-repl--required-timer-keys)
      (let ((key (car entry))
            (arm-fn (cdr entry)))
        (cond
         ((agent-repl--timer-armed-p key)
          (push key armed))
         ((not (fboundp arm-fn))
          (push key unavailable)
          (agent-repl--warn
           '(:agent-repl-central "process-wide logging and utility state")
           "assert-heartbeat-armed: key=%s outcome=unavailable arm-fn=%s reason=owner-not-loaded"
           key arm-fn))
         (t
          (agent-repl--info
           '(:agent-repl-central "process-wide logging and utility state")
           "assert-heartbeat-armed: key=%s outcome=stranded arm-fn=%s action=re-arming"
           key arm-fn)
          (funcall arm-fn)
          (if (agent-repl--timer-armed-p key)
              (progn
                (push key rearmed)
                (agent-repl--info
                 '(:agent-repl-central "process-wide logging and utility state")
                 "assert-heartbeat-armed: key=%s outcome=rearmed arm-fn=%s" key arm-fn))
            (push key failed)
            (agent-repl--warn
             '(:agent-repl-central "process-wide logging and utility state")
             "assert-heartbeat-armed: key=%s outcome=rearm-failed arm-fn=%s"
             key arm-fn))))))
    (let ((result (list :armed (nreverse armed)
                        :rearmed (nreverse rearmed)
                        :failed (nreverse failed)
                        :unavailable (nreverse unavailable))))
      (agent-repl--log-verbose
       '(:agent-repl-central "process-wide logging and utility state")
       "assert-heartbeat-armed: armed=%d rearmed=%d failed=%d unavailable=%d"
       (length (plist-get result :armed))
       (length (plist-get result :rearmed))
       (length (plist-get result :failed))
       (length (plist-get result :unavailable)))
      result)))

(defun agent-repl--heartbeat-arm-functions-available-p ()
  "Return non-nil when every owner's arm function is defined.
False during a COLD load, where core.el is evaluated before the four
owner files that define the arm functions."
  (let ((all t))
    (dolist (entry agent-repl--required-timer-keys)
      (unless (fboundp (cdr entry))
        (setq all nil)))
    all))

(defun agent-repl--assert-heartbeat-armed-when-owners-load ()
  "Run the timer-contract assertion, deferring it if the owners are absent.

This is core.el's load-time entry point.  Two cases, and the distinction
is the whole point:

- Every arm function is already defined, which is what a hot-load of
  core.el into a RUNNING Emacs looks like.  The check runs immediately, so
  the bare core.el load repairs the heartbeat its own `--cancel-all-timers'
  just cleared.
- Some arm function is undefined, which is what a COLD load looks like:
  config.el loads core.el first and the owners afterwards.  Asserting now
  would report four bogus strands, so the check is deferred onto an idle
  timer that fires once the load has settled.  Under `noninteractive' the
  idle timer never fires, which is correct — a batch run has no heartbeat
  to keep alive.

Returns `:checked' with the assertion result consed on, or `:deferred'."
  (when (timerp agent-repl--heartbeat-assert-deferral-timer)
    (cancel-timer agent-repl--heartbeat-assert-deferral-timer)
    (setq agent-repl--heartbeat-assert-deferral-timer nil))
  (if (agent-repl--heartbeat-arm-functions-available-p)
      (cons :checked (agent-repl--assert-heartbeat-armed))
    (setq agent-repl--heartbeat-assert-deferral-timer
          (run-with-idle-timer agent-repl-heartbeat-assert-defer-delay nil
                               #'agent-repl--assert-heartbeat-armed))
    (agent-repl--log
     '(:agent-repl-central "process-wide logging and utility state")
     "assert-heartbeat-armed: outcome=deferred reason=owners-not-loaded delay=%s timer=%S"
     agent-repl-heartbeat-assert-defer-delay
     agent-repl--heartbeat-assert-deferral-timer)
    :deferred))

;;;; ---- Feed text zoom keys ------------------------------------------------

(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))

(defconst agent-repl-feed-zoom-keys
  '(("C-+" . agent-repl-feed-text-scale-increase)
    ("C--" . agent-repl-feed-text-scale-decrease))
  "The feed text zoom chords and the commands they run.")

(defun agent-repl-bind-feed-zoom-keys (map)
  "Bind the feed text zoom chords in MAP and in each of its Evil state maps.
Doom binds `C-+' and `C--' in `evil-normal-state-map', and an Evil state
map outranks any major or minor mode map, so a plain binding in MAP alone
never fires (observed 2026-10-06: the composer resolved `C-+' to
`doom/reset-font-size').  Planting the chords in MAP's own state maps
makes them win in every state; the plain binding keeps them observable
under `emacs -Q', where Evil is absent."
  (dolist (binding agent-repl-feed-zoom-keys)
    (define-key map (kbd (car binding)) (cdr binding)))
  (when (fboundp 'evil-define-key*)
    (dolist (state '(normal motion visual insert emacs))
      (dolist (binding agent-repl-feed-zoom-keys)
        (evil-define-key* state map (kbd (car binding)) (cdr binding))))))

(agent-repl--assert-heartbeat-armed-when-owners-load)
