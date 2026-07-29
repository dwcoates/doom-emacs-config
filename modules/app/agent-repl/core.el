;;; core.el --- agent-repl core definitions -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

;; Cancel all previously registered timers on re-eval so we don't accumulate.
(defvar agent-repl--timers nil
  "List of active timers created by agent-repl.
Cancelled and reset whenever this file is re-evaluated.")

(defun agent-repl--cancel-all-timers ()
  "Cancel every timer in `agent-repl--timers' and reset the list."
  (let ((count (length agent-repl--timers)))
    (dolist (timer agent-repl--timers)
      (when (timerp timer)
        (cancel-timer timer)))
    (setq agent-repl--timers nil)
    ;; Guard: this function is called at load time (line below), before
    ;; agent-repl--log is defined.  Only log when logging is available.
    (when (fboundp 'agent-repl--log)
      (agent-repl--log nil "cancel-all-timers: cancelled=%d" count))))

(agent-repl--cancel-all-timers)

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
                  (agent-repl--log nil "migrate-legacy-state: moved %s -> %s" old new)
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
  "Controls debug logging level.
nil means no logging; t means standard logging; \\='verbose also enables
high-frequency events (window changes, resolve-root, process-alive
predicates, sentinel re-entry).  Verbose mode also gates
`agent-repl--log-verbose's file writes: when debug is anything other
than \\='verbose, those calls are a no-op.  Use
\\[agent-repl-debug/toggle-logging] (with `C-u' prefix for verbose) to
flip at runtime."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)
                 (const :tag "Verbose" verbose))
  :group 'agent-repl)

(defcustom agent-repl-log-to-file t
  "Master kill-switch for file-writing of agent-repl log lines.
When non-nil (the default), every call to `agent-repl--log',
`agent-repl--info', `agent-repl--warn', `agent-repl--do-log', or
`agent-repl--error' appends its formatted line to
`agent-repl-log-file-name' — REGARDLESS of `agent-repl-debug'.
`agent-repl--log-verbose' is the exception: it ADDITIONALLY requires
`agent-repl-debug' to be \\='verbose, because its hot-path callers
(timer ticks, alive predicates) would otherwise spend ~25% of Emacs CPU
in `write-region'.  Use `agent-repl-debug/toggle-log-to-file' to flip
the kill-switch at runtime."
  :type 'boolean
  :group 'agent-repl)

(defcustom agent-repl-log-size-cap-bytes (* 1024 1024 1024)
  "Hard cap on the log file size in bytes.  Default 1 GiB.
Checked every `agent-repl-log-size-check-interval' writes (not on every
write — `file-attributes' on a multi-GB file is cheap but not free).
When the cap is exceeded, the first 80% of the file is dropped
(line-aligned) and a WARNING line is appended noting the truncation."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-log-size-check-interval 1000
  "Number of file-writes between size-cap checks.
Lower values catch overruns sooner but pay more `file-attributes' calls;
the default of 1000 keeps the check effectively free for typical usage
(one stat per ~1000 log lines)."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-workspace-id-length 8
  "Number of hex characters from MD5 hash used for workspace IDs.
Longer values reduce collision risk in setups with many workspaces."
  :type 'integer
  :group 'agent-repl)

(defun agent-repl--default-log-directory ()
  "Return agent-repl's private directory under the OS temporary root.
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

(defconst agent-repl--default-log-file-name
  (expand-file-name "doom-agent-repl.log"
                    (agent-repl--default-log-directory))
  "Default OS-temporary path for the aggregate agent-repl log.")

(defconst agent-repl--retired-state-log-file-name
  (agent-repl--global-state-file "doom-agent-repl.log")
  "Retired pre-temp-directory default for the aggregate agent-repl log.")

(defun agent-repl--normalize-log-file-name (value)
  "Return the active logfile path for configured VALUE.
The retired state-tree default is redirected to the new OS-temporary default
so reloading this module updates an already-bound defcustom.  Every other
explicit path is preserved.  No file is moved, copied, read, or deleted."
  (if (equal (expand-file-name value)
             (expand-file-name agent-repl--retired-state-log-file-name))
      agent-repl--default-log-file-name
    value))

(defcustom agent-repl-log-file-name agent-repl--default-log-file-name
  "Path to the agent-repl log file.
Defaults to `doom-agent-repl.log' in a UID-qualified private directory under
Emacs's `temporary-file-directory'.  On macOS that is normally the per-user
/var/folders/.../T tree; on Linux it is commonly
/tmp/doom-agent-repl-<uid>/doom-agent-repl.log.

Existing logs under ~/.claude-emacs are intentionally neither migrated nor
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
Returns the bare prefix with no trailing slash (e.g. \"DWC\"), or the
empty string when neither env var is set or non-empty.  This mirrors how
the workspace-dispatch run.sh derives the branch prefix solely from the
same env vars, so the Emacs process must be launched with one of them
set to obtain a prefix."
  (let ((new (getenv "AGENT_WORKSPACE_PREFIX"))
        (legacy (getenv "CLAUDE_WORKSPACE_PREFIX")))
    (if (and new (not (string-empty-p new)))
        (progn
          (agent-repl--log nil
                            "workspace-prefix: source=AGENT_WORKSPACE_PREFIX value=%S"
                            new)
          new)
      (let ((result (or legacy "")))
        (agent-repl--log nil
                          "workspace-prefix: source=CLAUDE_WORKSPACE_PREFIX value=%S"
                          result)
        result))))

(defun agent-repl--workspace-prefix-slash ()
  "Return the workspace-name prefix in `<prefix>/' form, or \"\" when unset.
Builds on `agent-repl--workspace-prefix': when a non-empty prefix is
set this appends a single trailing slash so callers can concatenate a
bare slug directly; when no prefix is set this returns the empty string
so names are generated without any leading slash."
  (let ((prefix (agent-repl--workspace-prefix)))
    (if (string-empty-p prefix)
        ""
      (concat prefix "/"))))

;;; Kill-cause attribution

(defvar agent-repl--kill-cause nil
  "Why the current teardown is happening, for log attribution.
Every entry point that kills an agent session or tears down a
workspace let-binds this to a short human-readable cause string
\(e.g. \"interactive nuke command\", \"merged-clear idle timer (auto)\")
for the dynamic extent of the teardown.  The shared chokepoints
\(`agent-repl--nuke-one-workspace', `agent-repl--finish-workspace',
`agent-repl--ws-del', the frontend kill dispatch) read it into their
log lines so the log always answers HOW a session was killed.  A nil
value logs as \"unattributed(BUG: bind agent-repl--kill-cause)\" —
treat that as a missing binding at the initiator, not an acceptable
state.")

(defun agent-repl--kill-cause-str ()
  "Return `agent-repl--kill-cause' rendered for a log line."
  (or agent-repl--kill-cause "unattributed(BUG: bind agent-repl--kill-cause)"))

;;; Logging

(defun agent-repl--ws-id-cached (ws)
  "Return the cached workspace ID hash for WS, computing if needed.
Uses :project-dir from the workspace state to derive the 8-char MD5 hash.
Caches the result under :ws-id to avoid repeated `file-truename' calls.
Returns nil if WS is nil or no :project-dir is set."
  (when ws
    (or (plist-get (gethash ws agent-repl--workspaces) :ws-id)
        (when-let ((dir (plist-get (gethash ws agent-repl--workspaces) :project-dir)))
          (let ((id (substring (md5 (directory-file-name (file-truename dir))) 0 agent-repl-workspace-id-length)))
            (puthash ws (plist-put (gethash ws agent-repl--workspaces) :ws-id id)
                     agent-repl--workspaces)
            id)))))

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
        (let* ((id       (agent-repl--ws-id-cached ws))
               (dir      (plist-get plist :project-dir))
               (cstate   (plist-get plist :agent-state))
               (rstate   (plist-get plist :repl-state))
               (env      (plist-get plist :active-env))
               (fbuf     (plist-get plist :frontend-buffer))
               (ibuf     (plist-get plist :input-buffer))
               (pcnt     (plist-get plist :prefix-counter))
               (wt       (plist-get plist :worktree-p))
               (fork     (plist-get plist :fork-session-id))
               (rtimer   (plist-get plist :ready-timer))
               (pri      (plist-get plist :priority))
               (pprompts (plist-get plist :pending-prompts))
               (pshow    (plist-get plist :pending-show-panels))
               (dprompts (plist-get plist :deferred-prompts)))
          (format (concat " {ws=%s id=%s dir=%s cst=%s rst=%s env=%s"
                          " fe=%s in=%s cnt=%s"
                          " wt=%s fork=%s"
                          " rtmr=%s pri=%s pend=%s pshow=%s defq=%s}")
                  ws
                  (or id "-")
                  (or dir "-")
                  (or cstate "-")
                  (or rstate "-")
                  (or env "-")
                  (if fbuf (if (buffer-live-p fbuf) "live" "dead") "-")
                  (if ibuf (if (buffer-live-p ibuf) "live" "dead") "-")
                  (or pcnt "-")
                  (if wt "t" "-")
                  (or fork "-")
                  (if rtimer "t" "-")
                  (or pri "-")
                  (if pprompts (length pprompts) "-")
                  (if pshow "t" "-")
                  (if dprompts (length dprompts) "-")))))))

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

(defun agent-repl--logfile-path ()
  "Return the expanded path of `agent-repl-log-file-name'.
The parent directory is created if it does not exist.  The default
UID-qualified temporary directory is required to be a real directory owned by
the current user and is forced to mode 0700.

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
    path))

(defvar agent-repl--validated-private-log-directories
  (make-hash-table :test #'equal)
  "Private temporary log directories validated during this Emacs process.")

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
  "Runtime-owned external targets for workspace `emacs.log' symlinks.")

(defconst agent-repl--emacs-log-target-prefix "agent-repl-emacs-"
  "Filename prefix that identifies an Emacs-owned external log target.")

(define-error 'agent-repl-log-truncate-failure "agent-repl log truncation failure")

(defun agent-repl--json-object (&rest pairs)
  "Return PAIRS as a JSON object with string keys.
Each member of PAIRS is a cons whose car is the field name and whose cdr is
already JSON-serializable.  A hash table avoids the nil/empty-alist ambiguity
in `json-serialize'."
  (let ((object (make-hash-table :test #'equal)))
    (dolist (pair pairs object)
      (puthash (car pair) (cdr pair) object))))

(defun agent-repl--log-rfc3339-timestamp ()
  "Return the current time as an RFC 3339 timestamp with microseconds."
  (format-time-string "%FT%T.%6N%:z"))

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

(defun agent-repl--workspace-log-identity (ws)
  "Return WS's registered canonical directory and stable workspace identity.
The logging boundary deliberately refuses to derive either value from ambient
state because a non-nil WS must identify one specific workspace sink."
  (let ((dir (and (fboundp 'agent-repl--ws-get)
                  (agent-repl--ws-get ws :project-dir))))
    (unless (and (stringp dir) (file-directory-p dir))
      (error "agent-repl log routing invariant violated: workspace %S has no registered project directory" ws))
    (list :project-dir (directory-file-name (file-truename dir))
          :workspace-id (or (agent-repl--ws-id-cached ws)
                            (error "agent-repl log routing invariant violated: workspace %S has no workspace ID" ws)))))

(defun agent-repl--log-add-workspace-identity (record ws)
  "Add WS identity and known session fields to JSON RECORD when WS is non-nil."
  (when ws
    (let ((identity (agent-repl--workspace-log-identity ws)))
      (puthash "workspace_dir" (plist-get identity :project-dir) record)
      (puthash "workspace_id" (plist-get identity :workspace-id) record)
      (dolist (field-value
               `(("agent_repl_session_id" . ,(agent-repl--ws-get ws :frontend-session-id))
                 ("claude_session_id" . ,(agent-repl--ws-durable-claude-session-id ws))))
        (let ((field (car field-value))
              (value (cdr field-value)))
          (cond
           ((null value))
           ((and (stringp value) (not (string-empty-p value)))
           (puthash field value record))
           (t
            (error "agent-repl log routing invariant violated: workspace %S has invalid %s: %S"
                   ws field value)))))))
  record)

(defun agent-repl--log-record (ws level verbosity fmt args)
  "Serialize WS / LEVEL / VERBOSITY / FMT / ARGS as one JSONL record."
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
                  (cons "operation" (agent-repl--log-operation fmt))
                  (cons "message" message)
                  (cons "context" context))))
    (agent-repl--log-add-workspace-identity record ws)
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

(defun agent-repl--workspace-emacs-log-target (ws)
  "Return WS's runtime-owned external target and atomically install its link.
WS must have a registered project directory.  Workspace-controlled paths are
never opened for writing: the durable target is created in
`temporary-file-directory' and the workspace path is only an atomic symlink."
  (let* ((identity (agent-repl--workspace-log-identity ws))
         (cached (gethash ws agent-repl--workspace-log-targets)))
    (if cached
        (let ((target (plist-get cached :target)))
          (unless (and (equal (plist-get cached :project-dir) (plist-get identity :project-dir))
                       (equal (plist-get cached :workspace-id) (plist-get identity :workspace-id)))
            (error "agent-repl log routing invariant violated: workspace %S retained a target after identity rebinding" ws))
          (unless (file-regular-p target)
            (error "agent-repl log routing invariant violated: owned target vanished: %s" target))
          target)
      (let ((project-dir (plist-get identity :project-dir)))
        (let* ((canonical (agent-repl--workspace-emacs-log-path project-dir))
               (canonical-dir (file-name-directory canonical)))
               ;; On a new Emacs runtime, the workspace path is untrusted even
               ;; when it names an old temporary file.  Only this in-memory
               ;; registry authorizes target reuse, which makes link poisoning
               ;; structurally unable to redirect a durable write.
          (agent-repl--ensure-real-log-directory (expand-file-name ".claude" project-dir))
          (agent-repl--ensure-real-log-directory canonical-dir)
          (when (file-directory-p canonical)
            (error "agent-repl log routing invariant violated: canonical log path is a directory: %s" canonical))
          ;; Target creation happens only after both workspace-controlled
          ;; directory components are proven real, so a hostile parent leaves
          ;; no runtime-owned temporary artifact behind.
          (let ((target (make-temp-file agent-repl--emacs-log-target-prefix nil ".log"))
                (link-tmp nil)
                (installed nil))
            (unwind-protect
                (progn
                  (setq link-tmp (make-temp-file
                                  (expand-file-name ".emacs.log-link-" canonical-dir)))
                  ;; Reserving first gives a collision-proof name.  Removing
                  ;; that reservation immediately before a non-overwriting
                  ;; symlink creation ensures an interloper causes failure.
                  (delete-file link-tmp)
                  (make-symbolic-link target link-tmp)
                  ;; `rename-file' makes the canonical-link replacement atomic.
                  (rename-file link-tmp canonical t)
                  (puthash ws (append (list :target target) identity)
                           agent-repl--workspace-log-targets)
                  (setq installed t)
                  target)
              (when (and link-tmp
                         (or (file-exists-p link-tmp) (file-symlink-p link-tmp)))
                (delete-file link-tmp))
              (unless installed
                (when (file-exists-p target)
                  (delete-file target))))))))))

(defvar agent-repl--log-write-counter 0
  "Monotonic counter of successful log-file writes.
Used by `agent-repl--do-log-to-file' to decide when to size-check.")

(defun agent-repl--secure-log-file-mode (path)
  "Require PATH to be a regular file and force its permissions to 0600.
This helper intentionally does not log: it runs inside the logfile sink, so
using the logging ladder here would recurse."
  (unless (file-regular-p path)
    (error "agent-repl--secure-log-file-mode: not a regular file: %s" path))
  (unless (= (logand (file-modes path) #o777) #o600)
    (set-file-modes path #o600)))

(defun agent-repl--log-truncate (path size &optional ws)
  "Drop the first 80% of PATH (SIZE bytes) and append a WARNING line.
Reads the last 20% of the file as raw bytes, aligns to the next
newline (so we don't keep a partial first line), then overwrites PATH in
place so readers holding the target open retain its inode.

Pure side-effect — no logging facilities are called here so we cannot
re-enter `agent-repl--do-log-to-file' and recurse."
  (let* ((keep-bytes (max 1 (- size (floor (* 0.8 size)))))
         (start (- size keep-bytes))
         (warning nil)
         (needs-newline nil))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((coding-system-for-read 'no-conversion)
            (coding-system-for-write 'no-conversion))
        (insert-file-contents-literally path nil start size)
        ;; Drop the partial first line (everything up to and including the
        ;; first newline) so the resulting file starts on a clean line.
        (goto-char (point-min))
        (when (search-forward "\n" nil t)
          (delete-region (point-min) (point)))
        (setq needs-newline
              (and (> (point-max) (point-min))
                   (not (eq (char-before (point-max)) ?\n))))
        (write-region (point-min) (point-max) path nil 'silent)))
    (setq warning
          (agent-repl--json-object
            (cons "timestamp" (agent-repl--log-rfc3339-timestamp))
            (cons "runtime" "emacs") (cons "pid" (emacs-pid))
            (cons "level" "warn") (cons "verbosity" "normal")
            (cons "operation" "agent-repl.log.truncate")
            (cons "message" "log truncated after size cap exceeded")
            (cons "context" (agent-repl--json-object
                              (cons "cap_bytes" agent-repl-log-size-cap-bytes)
                              (cons "size_bytes" size)
                              (cons "kept_bytes" keep-bytes)))))
    (agent-repl--log-add-workspace-identity warning ws)
    (setq warning (json-serialize warning))
    (write-region (concat (if needs-newline "\n" "") warning "\n") nil path t 'silent)
    (agent-repl--secure-log-file-mode path)))

(defun agent-repl--log-maybe-truncate (path &optional ws)
  "Truncate PATH when it exceeds `agent-repl-log-size-cap-bytes'.
Called periodically from `agent-repl--do-log-to-file'."
  (let ((attrs (file-attributes path)))
    (when attrs
      (let ((size (file-attribute-size attrs)))
        (when (and size (> size agent-repl-log-size-cap-bytes))
          (condition-case err
              (agent-repl--log-truncate path size ws)
            (error
             (message "[agent-repl] LOG SINK FAILURE operation=truncate path=%s workspace=%S error=%S"
                      path ws err)
             (signal 'agent-repl-log-truncate-failure (list path ws err)))))))))

(defun agent-repl--do-log-to-file (text &optional ws)
  "Append TEXT as a line to the logfile when `agent-repl-log-to-file' is non-nil.
Sink failures, including truncation failures, emit the emergency diagnostic and
signal an error; persistence must never silently degrade.

Increments `agent-repl--log-write-counter' on every successful write
and runs `agent-repl--log-maybe-truncate' once every
`agent-repl-log-size-check-interval' writes."
  (when agent-repl-log-to-file
    (let ((path (if ws
                    (agent-repl--workspace-emacs-log-target ws)
                  (agent-repl--logfile-path))))
      (condition-case err
          (let ((new-file (not (file-exists-p path))))
            (write-region (concat text "\n") nil path t 'silent)
            (when new-file
              (agent-repl--secure-log-file-mode path))
            (cl-incf agent-repl--log-write-counter)
            (when (and (> agent-repl-log-size-check-interval 0)
                       (zerop (mod agent-repl--log-write-counter
                                   agent-repl-log-size-check-interval)))
              (agent-repl--log-maybe-truncate path ws)))
        (agent-repl-log-truncate-failure
         (signal (car err) (cdr err)))
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

(defun agent-repl--workspace-log-buffer (ws)
  "Return WS's workspace-owned live agent-repl log buffer.
The buffer is created through `agent-repl--create-buffer', which sets its
permanent-local owner and attaches it through workspace.el's perspective
boundary.  Its contents are an in-memory view only; the durable logfile
remains the authoritative persisted record."
  (agent-repl--create-buffer ws agent-repl--workspace-log-buffer-suffix))

(defun agent-repl--append-workspace-log (ws text)
  "Append the exact formatted log TEXT to WS's live log buffer.
Only a non-nil WS is workspace-scoped.  This helper deliberately does not
log its own buffer creation or append work: it runs for every log entry, and
instrumenting it through the logging ladder would recurse indefinitely."
  (when (and agent-repl--workspace-log-buffer-enabled ws)
    (with-current-buffer (agent-repl--workspace-log-buffer ws)
      (let ((inhibit-read-only t))
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-max))
            (insert text "\n")))))))

(defun agent-repl--persist-log-record (ws level verbosity fmt args)
  "Persist one JSONL record for WS without changing caller-facing signatures."
  ;; Tests deliberately bind the explicit file sink kill-switch off.  Do not
  ;; construct or route a record when persistence itself was opted out of.
  (let ((record (agent-repl--log-record ws level verbosity fmt args)))
    (when agent-repl-log-to-file
      (agent-repl--do-log-to-file record ws))
    (agent-repl--append-workspace-log ws record)))

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
;;      minibuffer.  It is reserved for GENUINE FATAL errors alone — the
;;      conditions the user (or an agent watching the modeline for them)
;;      MUST act on immediately.  In this ladder that means ONLY
;;      `agent-repl--error', which reaches the modeline by SIGNALLING an
;;      `error' (Emacs always displays a signalled error), NOT through the
;;      `inhibit-message' gate below.  Warnings and every other diagnostic
;;      are non-fatal, so they stay on the quiet sink and NEVER flash in the
;;      modeline; they remain durable and greppable for whoever needs them.
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
;;   `agent-repl--log-verbose'  hot-path chatter   file (verbose only), quiet
;;   `agent-repl--log'          debug chatter      file always, quiet
;;   `agent-repl--info'         background notice  file + *Messages*, quiet
;;   `agent-repl--warn'         recorded warning   file + *Messages*, quiet
;;   `agent-repl--error'        signals an error   file + *Messages* + ECHO
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

(defun agent-repl--do-log-level (ws fmt args level &optional error-p)
  "Persist and emit WS / FMT / ARGS at LEVEL without changing public APIs."
  (let ((text (agent-repl--build-log-text ws fmt args)))
    (agent-repl--persist-log-record ws level "normal" fmt args)
    (if error-p
        (error "%s" text)
      (agent-repl--emit-message text nil))))

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
  (agent-repl--do-log-level ws fmt args (if error-p "error" "info") error-p))

(defun agent-repl--log (ws fmt &rest args)
  "Log a timestamped message for WS, always to file, conditionally to *Messages*.
File write happens whenever `agent-repl-log-to-file' is non-nil (the
default) — REGARDLESS of `agent-repl-debug'.  The `message' call only
fires when `agent-repl-debug' is non-nil, and even then it is emitted
quietly (into *Messages* only, never the echo area), so turning debug
logging on never turns the modeline into a firehose.
FMT and ARGS use the same format conventions as `message'."
  (let ((text (agent-repl--build-log-text ws fmt args)))
    (agent-repl--persist-log-record ws "debug" "normal" fmt args)
    (when agent-repl-debug
      (agent-repl--emit-message text nil))))

(defun agent-repl--log-verbose (ws fmt &rest args)
  "Persist a high-frequency message and show it only in verbose mode.
Verbose affects terminal and *Messages* visibility only.  Its JSONL record is
always persisted through the same durable sink as normal logging."
  (let ((text (agent-repl--build-log-text ws fmt args)))
    (agent-repl--persist-log-record ws "debug" "verbose" fmt args)
    (when (eq agent-repl-debug 'verbose)
      (agent-repl--emit-message text nil))))

(defun agent-repl--info (ws fmt &rest args)
  "Log an informational line for WS to the QUIET sink, ungated by debug.
The line ALWAYS reaches the log file and the *Messages* buffer, but it
never reaches the echo area / modeline.  This is the level for background
and lifecycle chatter that is valuable to have on the record but that the
user must not be interrupted by: module loads, worktree creation progress,
snapshot-load steps, sentinel bookkeeping, agent start/finish notices.

Use `agent-repl--warn' instead to tag a recorded line with `WARNING:'
severity (still quiet), or `agent-repl--error' to signal a genuine fatal
condition loudly into the modeline."
  (let ((text (agent-repl--build-log-text ws fmt args)))
    (agent-repl--persist-log-record ws "info" "normal" fmt args)
    (agent-repl--emit-message text nil)))

(defun agent-repl--warn (ws fmt &rest args)
  "Log a WARNING for WS to the QUIET sink: the log file and *Messages'.
A `WARNING: ' severity tag is prepended, so call sites pass the bare
message (no literal \"WARNING:\" prefix of their own).

A warning is NOT fatal, so it no longer reaches the echo area / modeline:
the line is recorded on the durable, greppable channels (log file plus
*Messages*) for the user or a watching agent to find, but it never
interrupts.  Reserve `agent-repl--error' for the genuine fatal conditions
that MUST surface in the modeline immediately.  This level still carries
the `WARNING: ' severity that a plain `agent-repl--info' notice lacks:
use it for failed writes, dropped state, broken invariants, and degraded
functionality that are worth flagging in the log but are not fatal."
  (if (stringp fmt)
      (agent-repl--do-log-level ws (concat "WARNING: " fmt) args "warn")
    ;; A non-string FMT is a caller bug.  Hand it through untouched rather
    ;; than `concat'-ing it (which would raise a wrong-type-argument here and
    ;; bury the real culprit): `agent-repl--build-log-text' already captures a
    ;; backtrace to *agent-repl-log-bug* for exactly this case, and ARGS is
    ;; preserved so nothing about the offending call is lost.
    (agent-repl--do-log-level ws fmt args "warn")))

(defun agent-repl--error (ws fmt &rest args)
  "Signal an error with a [agent-repl] tag, timestamp, and workspace metadata.
WS is the workspace name for context (or nil).  FMT and ARGS are formatted
the same way `agent-repl--log' formats them, and the resulting line is also
written to the logfile before the error is signalled so the failure is
captured regardless of whether debug logging is on.

Unlike `agent-repl--log', this fires regardless of `agent-repl-debug' —
errors are not gated on the debug flag."
  (agent-repl--do-log ws fmt args t))

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

(defun agent-repl--rotate-log-on-startup ()
  "Rename an existing log file to `<path>.prev', preserving one prior session.
Idempotent: clobbers any existing `.prev'.  No-op when the current log
file does not exist or `agent-repl-log-to-file' is nil.  Errors are
caught and surfaced as a message — the rollover must not block startup."
  (when agent-repl-log-to-file
    (condition-case err
        (let* ((path (agent-repl--logfile-path))
               (prev (concat path ".prev")))
          (when (file-exists-p path)
            (when (file-exists-p prev) (delete-file prev))
            (rename-file path prev)
            ;; Reset the write counter — size accounting is per-file.
            (setq agent-repl--log-write-counter 0)))
      (error (message "[agent-repl] WARNING: log rotate failed: %S" err)))))

;; Run inline at load so each Emacs session begins with a fresh log file.
;; Guarded against `noninteractive' so ERT batch runs don't trash the user's
;; real log on every test invocation.
(unless noninteractive
  (agent-repl--rotate-log-on-startup))

;;; Git and workspace identity

(defun agent-repl--dir-has-git-p (d)
  "Return non-nil if directory D contains a .git directory or file."
  (let ((git (expand-file-name ".git" d)))
    (or (file-directory-p git) (file-regular-p git))))

(defun agent-repl--git-root (&optional dir)
  "Find the git root by walking up from DIR (default `default-directory').
Checks for both .git directory and .git file (worktrees)."
  (let* ((dir (or dir default-directory))
         (root (locate-dominating-file dir #'agent-repl--dir-has-git-p)))
    (agent-repl--log-verbose nil "git-root: dir=%s root=%s" dir root)
    (when root (agent-repl--path-canonical root))))

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
           nil
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
              (agent-repl--log nil
                                "capture-process-output: TIMEOUT after %ss %s %S"
                                timeout program args)
              "")
             (t
              (with-current-buffer stdout-buf
                (let ((output (string-trim
                               (buffer-substring-no-properties
                                (point-min) (point-max)))))
                  (agent-repl--log-verbose
                   nil
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
    (agent-repl--log nil
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
      (agent-repl--log-verbose nil
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
      (agent-repl--log nil
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
    agent-repl--git-exit-code
    agent-repl--git-exit-code-streaming
    agent-repl--git-branch-exists-p
    agent-repl--async-git
    agent-repl--gh-string-quiet
    agent-repl--early-git-string
    agent-repl--early-git-exit-code
    agent-repl--make-process-git
    agent-repl--async-gh
    agent-repl--signal-process
    agent-repl--cee-agent-reinstall-and-bounce-exit-code
    agent-repl--notify-parent-of-child-merge
    agent-repl--frontend-run-build-script
    agent-repl--frontend-run-listener-probe
    agent-repl--frontend-artifact-exists-p
    agent-repl--frontend-spawn-daemon
    agent-repl--launchctl-call
    agent-repl--shim-service-file-sha256
    agent-repl--shim-service-write-stamp
    agent-repl--shim-store-socket-present-p
    agent-repl--frontend-make-webview-buffer
    agent-repl--frontend-webview-selection
    agent-repl--frontend-webview-execute-script
    agent-repl--uds-connect
    agent-repl--uds-probe
    agent-repl--image-call-process
    agent-repl--run-install-script
    agent-repl--readiness-run-script)
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

(defun agent-repl--resolve-current-git-root ()
  "Resolve the git root for the caller's current context.
Prefers the current workspace's `:project-dir' when one is registered,
otherwise falls back to `default-directory'.  Signals `user-error' when
the resolved directory is not inside a git repository.

Intended to be called exactly once per workspace, at creation time, so
new worktrees are always rooted at the repository the user is currently
working in (rather than wherever Emacs happened to be launched)."
  (let* ((ws (agent-repl--ws-current-name))
         (ws-dir (ignore-errors (agent-repl--ws-dir ws)))
         (dir (or ws-dir default-directory))
         (default-directory dir)
         (raw (agent-repl--git-string-quiet "rev-parse" "--show-toplevel")))
    (agent-repl--log ws
                      "resolve-current-git-root: ws-dir=%S default-directory=%S resolved-dir=%S raw=%S"
                      ws-dir default-directory dir raw)
    (when (string-empty-p raw)
      (agent-repl--log ws
                        "resolve-current-git-root: FAILED ws-dir=%S resolved-dir=%S reason=not-a-git-repository"
                        ws-dir dir)
      (user-error "agent-repl: %s is not inside a git repository" dir))
    (let ((root (file-name-as-directory raw)))
      (agent-repl--log ws "resolve-current-git-root: SUCCESS root=%S" root)
      root)))

(defun agent-repl-print-git-branch ()
  "Print the git branch that was active when agent-repl config was loaded.
Lazily computes and caches the value on first invocation."
  (interactive)
  (let ((cache-hit (not (null agent-repl-git-branch))))
    (unless agent-repl-git-branch
      (setq agent-repl-git-branch
            (agent-repl--git-string-quiet "rev-parse" "--abbrev-ref" "HEAD")))
    (agent-repl--log nil "print-git-branch: cache-hit=%s branch=%S"
                      cache-hit agent-repl-git-branch))
  (message "agent-repl loaded on branch: %s" agent-repl-git-branch))

(defun agent-repl--path-canonical (path)
  "Return a canonical, stable string for PATH suitable for hashing.
Expands tildes and symlinks via `file-truename', then strips any
trailing slash via `directory-file-name' so that the same directory
always produces the same hash."
  (directory-file-name (file-truename path)))

(defun agent-repl--workspace-id ()
  "Return a short identifier for the current git workspace.
Uses an MD5 hash of the canonical project root path from the workspace hashmap.
Returns nil when no workspace has a registered `:project-dir' — callers are
expected to only invoke this from contexts where a workspace is active."
  (let* ((root (ignore-errors (agent-repl--ws-dir (agent-repl--ws-current-name))))
         (id (when root
               (substring (md5 (agent-repl--path-canonical root)) 0 agent-repl-workspace-id-length))))
    (agent-repl--log-verbose nil "workspace-id: root=%s id=%s" root id)
    id))

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
;; Two helpers in this file (`--ws-id-cached' and
;; `--format-ws-metadata' above) still `gethash' / `puthash' on the
;; var directly.  They are core logging primitives — wrapping them
;; via `--ws-get'/`--ws-put' would create a logging-to-workspace
;; cycle because `--ws-put' itself calls `--do-log' on stub-create.
;; Treat these as part of the encapsulation boundary (they live
;; immediately upstream of the wrapper API rather than downstream).

(defun agent-repl--active-inst (ws)
  "Return the active `agent-repl-instantiation' for workspace WS.
Signals an error if the environment or instantiation struct is
missing — both must be initialized by `agent-repl--initialize-ws-env'
before this is called."
  (let ((env (agent-repl--ws-get ws :active-env)))
    (unless env
      (agent-repl--log ws "active-inst: FAILED env=nil reason=missing-active-env")
      (error "agent-repl--active-inst: workspace %s has no :active-env (initialize-ws-env not called?)" ws))
    (let ((inst (agent-repl--ws-get ws env)))
      (unless inst
        (agent-repl--log ws "active-inst: FAILED env=%S reason=missing-instantiation" env)
        (error "agent-repl--active-inst: no instantiation struct for ws=%s env=%s (initialize-ws-env not called?)" ws env))
      (agent-repl--log-verbose ws "active-inst: SUCCESS env=%S inst=%S" env inst)
      inst)))

(declare-function agent-repl-instantiation-session-id "workspace")

(defun agent-repl--ws-durable-claude-session-id (ws)
  "Return WS's durable claude session uuid, or nil when none is recorded.
Reads the active instantiation's `session-id' — the hook-captured CLI
session uuid the gui frontend uses as its resume currency, via POST
/sessions' `resume' field.  Unlike `agent-repl--active-inst' this
returns nil instead of signaling when WS has no `:active-env' or no
instantiation struct yet — a workspace that never booted a session
legitimately has no durable id."
  (let* ((env (agent-repl--ws-get ws :active-env))
         (inst (and env (agent-repl--ws-get ws env)))
         (session-id (and inst (agent-repl-instantiation-session-id inst))))
    (agent-repl--log-verbose
     ws
     "ws-durable-claude-session-id: env=%S inst-present=%s session-id-present=%s"
     env (not (null inst)) (not (null session-id)))
    session-id))

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
which would otherwise nuke that workspace's session along with WS's own."
  (let ((owner (agent-repl--buffer-owner buf)))
    (and owner (not (equal owner ws)))))

;;; Buffer naming and predicates

;; The input buffer uses the "agent-panel-input-" prefix to distinguish
;; it from other agent-repl utility buffers (e.g. *agent-repl-dump*,
;; *agent-repl-log-bug*).  The webview buffer lives in its own
;; "agent-frontend-" namespace (see `agent-repl--frontend-buffer-re'
;; below).  Each workspace also has an in-memory "-log" buffer, whose
;; ownership is set through `agent-repl--create-buffer'.

(defconst agent-repl--input-buffer-re "^\\*agent-panel-input-[[:alnum:]_-]+\\*$"
  "Regexp matching agent input buffer names.
For example, *agent-panel-input-my-workspace*.")

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
      (agent-repl--log-verbose nil "buffer-name: suffix=%s ws=%s name=%s" suffix ws-name name)
      name)))

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

(defun agent-repl--non-user-buffer-p (buf)
  "Return non-nil if BUF is not a user-facing buffer.
Matches agent panel buffers, minibuffers, and dead/nil buffers.
BUF may be a buffer object or a name string."
  (let* ((b (if (stringp buf) (get-buffer buf) buf))
         (name (and b (buffer-name b))))
    (or (not name)
        (agent-repl--agent-panel-buffer-p b)
        (string-match-p "^ \\*Minibuf" name))))

(defun agent-repl--non-agent-buffers (buffers)
  "Return BUFFERS with agent panels, minibuffers, and dead buffers removed.
BUFFERS may be buffer objects or name strings."
  (cl-remove-if #'agent-repl--non-user-buffer-p buffers))

;;; Buffer background color
;;
;; Moved here from the now-deleted overlay.el, which otherwise existed
;; only for the vterm hide-overlay / font-scale / color-advice machinery.
;; These two survive because `agent-repl-input-mode' (input.el) calls
;; `agent-repl--set-buffer-background' to tint the input composer.

(defun agent-repl--rgb-hex (r g b)
  "Return a #rrggbb hex color string for channel values R, G, B (0-255 each)."
  (format "#%02x%02x%02x" r g b))

(defun agent-repl--grey-hex (n)
  "Return a hex color string for greyscale value N (0=black, 255=white)."
  (agent-repl--rgb-hex n n n))

(defun agent-repl--set-buffer-background (color)
  "Set default and fringe background to COLOR in the current buffer.
COLOR is any Emacs color spec, e.g. a #rrggbb hex string."
  (face-remap-add-relative 'default :background color)
  (face-remap-add-relative 'fringe :background color))

;;; Harness-injected (meta) prompt spans
;;
;; Every prompt agent-repl sends carries text the USER never typed: the
;; periodic read-directive pointing at the metaprompt file (input.el), the
;; autonomous-execution preamble, and the one-shot wrap-up gate
;; (worktree.el).  The agent must receive all of it verbatim, but a human
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
  "Return non-nil when WS is the currently active workspace name."
  (string= ws (agent-repl--ws-current-name)))
