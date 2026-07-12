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
high-frequency events (window changes, resolve-root, vterm-process-alive
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

(defcustom agent-repl-log-file-name (agent-repl--global-state-file "doom-agent-repl.log")
  "Path to the agent-repl log file.
Defaults under `agent-repl--global-state-dir' (agent-repl's own canonical
state tree at ~/.claude-emacs), NOT the Claude CLI config dir.  The value
is passed through `expand-file-name', and the parent directory is created
on demand by `agent-repl--logfile-path'."
  :type 'string
  :group 'agent-repl)

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
First %s is the suffix (e.g. \"-input\" or empty), second %s is the workspace name."
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
  (let ((new (getenv "AGENT_WORKSPACE_PREFIX")))
    (if (and new (not (string-empty-p new)))
        new
      (or (getenv "CLAUDE_WORKSPACE_PREFIX") ""))))

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
               (vbuf     (plist-get plist :vterm-buffer))
               (ibuf     (plist-get plist :input-buffer))
               (pcnt     (plist-get plist :prefix-counter))
               (gclean   (plist-get plist :git-clean))
               (gproc    (plist-get plist :git-proc))
               (wt       (plist-get plist :worktree-p))
               (fork     (plist-get plist :fork-session-id))
               (rtimer   (plist-get plist :ready-timer))
               (pri      (plist-get plist :priority))
               (pprompts (plist-get plist :pending-prompts))
               (pshow    (plist-get plist :pending-show-panels))
               (dprompts (plist-get plist :deferred-prompts)))
          (format (concat " {ws=%s id=%s dir=%s cst=%s rst=%s env=%s"
                          " vt=%s in=%s cnt=%s"
                          " git=%s gproc=%s wt=%s fork=%s"
                          " rtmr=%s pri=%s pend=%s pshow=%s defq=%s}")
                  ws
                  (or id "-")
                  (or dir "-")
                  (or cstate "-")
                  (or rstate "-")
                  (or env "-")
                  (if vbuf (if (buffer-live-p vbuf) "live" "dead") "-")
                  (if ibuf (if (buffer-live-p ibuf) "live" "dead") "-")
                  (or pcnt "-")
                  (or gclean "-")
                  (if gproc (if (process-live-p gproc) "run" "done") "-")
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

Note: callers using this to build a format string for `apply #'message'
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
The parent directory is created if it does not exist."
  (let* ((path (expand-file-name agent-repl-log-file-name))
         (dir (file-name-directory path)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    path))

(defvar agent-repl--log-write-counter 0
  "Monotonic counter of successful log-file writes.
Used by `agent-repl--do-log-to-file' to decide when to size-check.")

(defun agent-repl--log-truncate (path size)
  "Drop the first 80% of PATH (SIZE bytes) and append a WARNING line.
Reads the last 20% of the file as raw bytes, aligns to the next
newline (so we don't keep a partial first line), atomically replaces
PATH, then appends a single line noting the truncation.

Pure side-effect — no logging facilities are called here so we cannot
re-enter `agent-repl--do-log-to-file' and recurse."
  (let* ((keep-bytes (max 1 (- size (floor (* 0.8 size)))))
         (start (- size keep-bytes))
         (tmp (concat path ".trunc-tmp")))
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
        (write-region (point-min) (point-max) tmp nil 'silent)))
    (rename-file tmp path t)
    (let ((warning (format
                    "%s [agent-repl] WARNING: log truncated — file exceeded cap=%d bytes (was %d bytes); dropped first 80%%, kept last %d bytes"
                    (format-time-string "%H:%M:%S.%3N")
                    agent-repl-log-size-cap-bytes size keep-bytes)))
      (let ((coding-system-for-write 'no-conversion))
        (write-region (concat warning "\n") nil path t 'silent)))))

(defun agent-repl--log-maybe-truncate (path)
  "Truncate PATH when it exceeds `agent-repl-log-size-cap-bytes'.
Called periodically from `agent-repl--do-log-to-file'."
  (let ((attrs (file-attributes path)))
    (when attrs
      (let ((size (file-attribute-size attrs)))
        (when (and size (> size agent-repl-log-size-cap-bytes))
          (condition-case err
              (agent-repl--log-truncate path size)
            (error
             (message "[agent-repl] WARNING: log truncate failed for %s: %S"
                      path err))))))))

(defun agent-repl--do-log-to-file (text)
  "Append TEXT as a line to the logfile when `agent-repl-log-to-file' is non-nil.
No-ops when the logfile path cannot be determined.  Displays a warning
if a write error occurs (e.g. read-only filesystem) but does not signal
an error — logging must not break the caller.

Increments `agent-repl--log-write-counter' on every successful write
and runs `agent-repl--log-maybe-truncate' once every
`agent-repl-log-size-check-interval' writes."
  (when agent-repl-log-to-file
    (when-let ((path (agent-repl--logfile-path)))
      (condition-case err
          (progn
            (write-region (concat text "\n") nil path t 'silent)
            (cl-incf agent-repl--log-write-counter)
            (when (and (> agent-repl-log-size-check-interval 0)
                       (zerop (mod agent-repl--log-write-counter
                                   agent-repl-log-size-check-interval)))
              (agent-repl--log-maybe-truncate path)))
        (error (message "[agent-repl] WARNING: log write failed to %s: %S" path err))))))

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

;;;; ---- Echo-area (modeline) severity gate ----
;;
;; agent-repl has two distinct log sinks and they are NOT the same channel:
;;
;;   1. The QUIET sink — the log file plus the *Messages* buffer.  Everything
;;      goes here.  It is free, durable, greppable, and nobody has to look at
;;      it unless they are debugging.
;;
;;   2. The LOUD sink — the echo area / modeline.  This is the highest-
;;      sensitivity channel we have: it interrupts the user, it covers the
;;      minibuffer, and it is the only place a message can be *missed* by
;;      being drowned out.  It is reserved for things the user MUST act on
;;      or be aware of: warnings, errors, and direct feedback from a command
;;      they just invoked.
;;
;; `agent-repl--emit-message' is the single chokepoint that decides which
;; sink a line reaches.  Binding `inhibit-message' suppresses the echo-area
;; display while STILL logging the line to *Messages' — that is exactly the
;; bifurcation we want, and it means quieting a line never costs us the log.
;;
;; Pick a level, do not reach for `message' directly:
;;
;;   `agent-repl--log-verbose'  hot-path chatter   file (verbose only), quiet
;;   `agent-repl--log'          debug chatter      file always, quiet
;;   `agent-repl--info'         background notice  file + *Messages*, quiet
;;   `agent-repl--warn'         user must know     file + *Messages* + ECHO
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

(defun agent-repl--do-log (ws fmt args &optional error-p)
  "Unconditional log entry: ALWAYS write to file AND emit to message/error.
WS is the workspace name for context (or nil).  When ERROR-P is non-nil,
signals the formatted line via `error' instead of `message' — the
file-write still happens first so the line is captured before unwinding.

This is the entry point for log calls that MUST be captured regardless
of `agent-repl-debug' AND must surface in the echo area — errors,
invariant violations, and the STUB-CREATE warnings.  Callers that want
the line captured but NOT flashed at the user use `agent-repl--info';
debug-gated callers (`agent-repl--log', `agent-repl--log-verbose') use
the file-write path directly and emit quietly."
  (let ((text (agent-repl--build-log-text ws fmt args)))
    (agent-repl--do-log-to-file text)
    (if error-p
        (error "%s" text)
      (agent-repl--emit-message text t))))

(defun agent-repl--log (ws fmt &rest args)
  "Log a timestamped message for WS, always to file, conditionally to *Messages*.
File write happens whenever `agent-repl-log-to-file' is non-nil (the
default) — REGARDLESS of `agent-repl-debug'.  The `message' call only
fires when `agent-repl-debug' is non-nil, and even then it is emitted
quietly (into *Messages* only, never the echo area), so turning debug
logging on never turns the modeline into a firehose.
FMT and ARGS use the same format conventions as `message'."
  (let ((text (agent-repl--build-log-text ws fmt args)))
    (agent-repl--do-log-to-file text)
    (when agent-repl-debug
      (agent-repl--emit-message text nil))))

(defun agent-repl--log-verbose (ws fmt &rest args)
  "Log a high-frequency message for WS, gated on verbose-mode for BOTH sinks.
No-op unless `agent-repl-debug' is `verbose'.  The file write (which
profiling showed dominated Emacs CPU when this was always-on) and the
*Messages* emit are both behind the same gate, so hot-path callbacks
(timer ticks, window changes, resolve-root, async git sentinels) cost
nothing in the default-off configuration.  The `agent-repl-log-to-file'
kill-switch still wins — when it is nil, no file write occurs even in
verbose mode.  The *Messages* emit is quiet: hot-path chatter never
reaches the echo area.  Toggle via \\[agent-repl-debug/toggle-logging]
with a `C-u' prefix."
  (when (eq agent-repl-debug 'verbose)
    (let ((text (agent-repl--build-log-text ws fmt args)))
      (agent-repl--do-log-to-file text)
      (agent-repl--emit-message text nil))))

(defun agent-repl--info (ws fmt &rest args)
  "Log an informational line for WS to the QUIET sink, ungated by debug.
The line ALWAYS reaches the log file and the *Messages* buffer, but it
never reaches the echo area / modeline.  This is the level for background
and lifecycle chatter that is valuable to have on the record but that the
user must not be interrupted by: module loads, worktree creation progress,
snapshot-load steps, sentinel bookkeeping, agent start/finish notices.

Use `agent-repl--warn' instead when the user genuinely needs to see it."
  (let ((text (agent-repl--build-log-text ws fmt args)))
    (agent-repl--do-log-to-file text)
    (agent-repl--emit-message text nil)))

(defun agent-repl--warn (ws fmt &rest args)
  "Log a WARNING for WS to the LOUD sink: file, *Messages', and the echo area.
A `WARNING: ' severity tag is prepended, so call sites pass the bare
message (no literal \"WARNING:\" prefix of their own).

This is one of only two levels that may interrupt the user (the other
being `agent-repl--error'), so reserve it for conditions the user must
actually know about: failed writes, dropped state, broken invariants,
degraded functionality.  Anything merely informational is `agent-repl--info'."
  (if (stringp fmt)
      (agent-repl--do-log ws (concat "WARNING: " fmt) args)
    ;; A non-string FMT is a caller bug.  Hand it through untouched rather
    ;; than `concat'-ing it (which would raise a wrong-type-argument here and
    ;; bury the real culprit): `agent-repl--build-log-text' already captures a
    ;; backtrace to *agent-repl-log-bug* for exactly this case, and ARGS is
    ;; preserved so nothing about the offending call is lost.
    (agent-repl--do-log ws fmt args)))

(defun agent-repl--error (ws fmt &rest args)
  "Signal an error with a [agent-repl] tag, timestamp, and workspace metadata.
WS is the workspace name for context (or nil).  FMT and ARGS are formatted
the same way `agent-repl--log' formats them, and the resulting line is also
written to the logfile before the error is signalled so the failure is
captured regardless of whether debug logging is on.

Unlike `agent-repl--log', this fires regardless of `agent-repl-debug' —
errors are not gated on the debug flag."
  (agent-repl--do-log ws fmt args t))

(defun agent-repl--rotate-log-on-startup ()
  "Rename an existing log file to `<path>.prev', preserving one prior session.
Idempotent: clobbers any existing `.prev'.  No-op when the current log
file does not exist or `agent-repl-log-to-file' is nil.  Errors are
caught and surfaced as a message — the rollover must not block startup."
  (when agent-repl-log-to-file
    (condition-case err
        (let* ((path (expand-file-name agent-repl-log-file-name))
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
        (let ((proc (if suppress-stderr
                        (make-process ;; ALLOW-EXTERNAL-BOUNDARY
                         :name (format "agent-repl-capture-%s" program)
                         :command (cons program args)
                         :buffer stdout-buf
                         :stderr stderr-buf
                         :connection-type 'pipe
                         :noquery t)
                      (apply #'start-process ;; ALLOW-EXTERNAL-BOUNDARY
                             (format "agent-repl-capture-%s" program)
                             stdout-buf
                             program args))))
          (set-process-query-on-exit-flag proc nil)
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
                (string-trim
                 (buffer-substring-no-properties
                  (point-min) (point-max))))))))
      (when (buffer-live-p stdout-buf) (kill-buffer stdout-buf))
      (when (and stderr-buf (buffer-live-p stderr-buf))
        (kill-buffer stderr-buf)))))

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
  "Run a synchronous `gh' command and return its trimmed stdout, suppressing stderr.
ARGS are the `gh' subcommand and arguments.  Returns an empty string
when `gh' fails (no PR for branch, not authenticated, etc.).  The
wrapper IS the external boundary for the GitHub CLI: tests must mock
this function via `cl-letf' rather than invoke real `gh' (see
AGENTS.md \"No External Processes or External State in Tests\").

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
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel
     proc
     (lambda (p event)
       (when (string-prefix-p "finished" event)
         (let ((output (when (buffer-live-p (process-buffer p))
                         (with-current-buffer (process-buffer p)
                           (buffer-substring-no-properties
                            (point-min) (point-max))))))
           (when (buffer-live-p (process-buffer p))
             (kill-buffer (process-buffer p)))
           (funcall callback (zerop (process-exit-status p)) (or output ""))))))))

(defun agent-repl--docker-exit-code (&rest args)
  "Run `docker ARGS' synchronously and return its exit code (stdout discarded).
This IS the external-boundary wrapper for the Docker CLI: tests must
mock this function via `cl-letf' rather than invoke real `docker'
\(see AGENTS.md \"No External Processes or External State in Tests\")."
  (apply #'call-process "docker" nil nil nil args)) ;; ALLOW-EXTERNAL-BOUNDARY

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

`:connection-type 'pipe' / `:noquery t' / `:buffer nil' are baked in
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
    agent-repl--git-branch-exists-p
    agent-repl--git-tag-exists-p
    agent-repl--async-git
    agent-repl--gh-string-quiet
    agent-repl--early-git-string
    agent-repl--early-git-exit-code
    agent-repl--docker-exit-code
    agent-repl--make-process-git
    agent-repl--async-gh
    agent-repl--signal-process
    agent-repl--cee-agent-reinstall-and-bounce-exit-code)
  "Symbols of every function that wraps an external process or external-state mutation.
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
  (let* ((ws-dir (ignore-errors (agent-repl--ws-dir (agent-repl--ws-current-name))))
         (dir (or ws-dir default-directory))
         (default-directory dir)
         (raw (agent-repl--git-string-quiet "rev-parse" "--show-toplevel")))
    (when (string-empty-p raw)
      (user-error "agent-repl: %s is not inside a git repository" dir))
    (file-name-as-directory raw)))

(defun agent-repl-print-git-branch ()
  "Print the git branch that was active when agent-repl config was loaded.
Lazily computes and caches the value on first invocation."
  (interactive)
  (unless agent-repl-git-branch
    (setq agent-repl-git-branch
          (agent-repl--git-string-quiet "rev-parse" "--abbrev-ref" "HEAD")))
  (message "agent-repl loaded on branch: %s" agent-repl-git-branch))

(defun agent-repl--path-canonical (path)
  "Return a canonical, stable string for PATH suitable for hashing.
Expands tildes and symlinks via `file-truename', then strips any trailing slash
via `directory-file-name' so that the same directory always produces the same hash."
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
Signals an error if the environment or instantiation struct is missing —
both must be initialized by `agent-repl--initialize-ws-env' before this is called."
  (let ((env (agent-repl--ws-get ws :active-env)))
    (unless env
      (error "agent-repl--active-inst: workspace %s has no :active-env (initialize-ws-env not called?)" ws))
    (let ((inst (agent-repl--ws-get ws env)))
      (unless inst
        (error "agent-repl--active-inst: no instantiation struct for ws=%s env=%s (initialize-ws-env not called?)" ws env))
      inst)))

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

;; Panel buffers use the "agent-panel-" prefix to distinguish them from
;; other agent-repl utility buffers (e.g. *agent-repl-dump*,
;; *agent-repl-log-bug*).  The vterm regex is still a superset that
;; matches input buffers too; `agent-repl--agent-buffer-p' explicitly
;; excludes them.
(defconst agent-repl--vterm-buffer-re "^\\*agent-panel-[[:alnum:]_-]+\\*$"
  "Regexp matching agent panel buffer names (e.g. *agent-panel-my-workspace*).
Caveat: also matches input buffer names.  Use `agent-repl--agent-buffer-p'
for the combined check.")

(defconst agent-repl--input-buffer-re "^\\*agent-panel-input-[[:alnum:]_-]+\\*$"
  "Regexp matching agent input buffer names (e.g. *agent-panel-input-my-workspace*).")

(defun agent-repl--sanitize-ws-name (name)
  "Return NAME with unsafe characters replaced by underscores.
Keeps alphanumerics, hyphens, and underscores.  Returns nil for nil NAME."
  (when name
    (replace-regexp-in-string agent-repl-ws-name-allowed-chars-re "_" name)))

(defun agent-repl--buffer-name (&optional suffix ws)
  "Return a workspace-specific buffer name like *agent-panel-WS* or *agent-panel-input-WS*.
SUFFIX, if provided, is inserted before the workspace name (e.g. \"-input\").
WS, if provided, is the workspace name; otherwise uses the current workspace.
Signals an error when the resolved workspace name is nil or empty — an
empty id produces buffer names like *agent-panel-*, which the
`agent-repl--vterm-buffer-re' / `agent-repl--input-buffer-re' regexes
mis-classify (input names match the vterm regex with id=\"input-\"),
causing `agent-repl--sync-panels' to delete the input panel as orphaned."
  (let* ((ws-name (or ws (agent-repl--ws-current-name)))
         (safe (agent-repl--sanitize-ws-name ws-name)))
    (when (or (null safe) (string-empty-p safe))
      (error "agent-repl--buffer-name: empty workspace name (ws=%S, +workspace-current-name=%S, sanitized=%S)"
             ws (agent-repl--ws-current-name) safe))
    (let ((name (format agent-repl-panel-buffer-name-format (or suffix "") safe)))
      (agent-repl--log-verbose nil "buffer-name: suffix=%s ws=%s name=%s" suffix ws-name name)
      name)))

(defun agent-repl--create-buffer (ws &optional suffix)
  "Create a workspace-owned buffer for WS and return it.
SUFFIX is passed to `agent-repl--buffer-name' to select the buffer's
role: nil for the vterm buffer (*agent-panel-WS*), \"-input\" for the input
buffer (*agent-panel-input-WS*).

Single entry point for every workspace-owned buffer.  Derives the
canonical name, sets `agent-repl--owning-workspace' buffer-locally
(permanent-local so it survives subsequent major-mode activation), and
registers the buffer with WS's perspective so it appears in
`+workspace-buffer-list' and related listings.

Idempotent — `get-buffer-create' returns an existing buffer of the
same name, and `persp-add-buffer' internally no-ops when the buffer is
already in the perspective.  Skips persp attachment when WS is nil or
no perspective named WS exists (e.g. early in session startup)."
  (let ((buf (get-buffer-create (agent-repl--buffer-name suffix ws))))
    (with-current-buffer buf
      (setq-local agent-repl--owning-workspace ws))
    (when ws
      (when-let ((persp (agent-repl--ws-resolve-persp ws)))
        (agent-repl--ws-add-buffer buf persp nil)))
    buf))

(defun agent-repl--agent-buffer-p (&optional buf)
  "Return non-nil if BUF (default: current buffer) is an agent vterm buffer.
Excludes agent input buffers (which share a common prefix)."
  (let ((name (buffer-name (or buf (current-buffer)))))
    (and (string-match-p agent-repl--vterm-buffer-re name)
         (not (string-match-p agent-repl--input-buffer-re name)))))

(defun agent-repl--agent-panel-buffer-p (&optional buf)
  "Return non-nil if BUF (default: current buffer) is any agent panel buffer.
Matches both vterm and input buffers."
  (let ((name (buffer-name (or buf (current-buffer)))))
    (or (string-match-p agent-repl--vterm-buffer-re name)
        (string-match-p agent-repl--input-buffer-re name))))

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

;;; Workspace and vterm helpers

(defun agent-repl--current-ws-p (ws)
  "Return non-nil when WS is the currently active workspace name."
  (string= ws (agent-repl--ws-current-name)))

(defun agent-repl--current-ws-live-vterm ()
  "Return the live vterm buffer for the current workspace, or nil.
Looks up :vterm-buffer in the current workspace state and returns it only if
the buffer object is still live."
  (let* ((ws (agent-repl--ws-current-name))
         (buf (agent-repl--ws-get ws :vterm-buffer))
         (live (and buf (buffer-live-p buf))))
    (agent-repl--log-verbose ws "current-ws-live-vterm: buf=%s live=%s" buf live)
    (when live buf)))

(defun agent-repl--vterm-live-p ()
  "Return non-nil if the agent vterm buffer for the current workspace exists and is live."
  (not (null (agent-repl--current-ws-live-vterm))))

(defmacro agent-repl--with-vterm-buf (&rest body)
  "Execute BODY with `vterm-buf' bound to the current workspace's live vterm buffer.
If the vterm buffer does not exist or is dead, BODY is not executed and the
form returns nil."
  (declare (indent 0) (debug body))
  `(when-let ((vterm-buf (agent-repl--current-ws-live-vterm)))
     ,@body))


