;;; core.el --- claude-repl core definitions -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

;; Cancel all previously registered timers on re-eval so we don't accumulate.
(defvar claude-repl--timers nil
  "List of active timers created by claude-repl.
Cancelled and reset whenever this file is re-evaluated.")

(defun claude-repl--cancel-all-timers ()
  "Cancel every timer in `claude-repl--timers' and reset the list."
  (let ((count (length claude-repl--timers)))
    (dolist (timer claude-repl--timers)
      (when (timerp timer)
        (cancel-timer timer)))
    (setq claude-repl--timers nil)
    ;; Guard: this function is called at load time (line below), before
    ;; claude-repl--log is defined.  Only log when logging is available.
    (when (fboundp 'claude-repl--log)
      (claude-repl--log nil "cancel-all-timers: cancelled=%d" count))))

(claude-repl--cancel-all-timers)

(defgroup claude-repl nil
  "Claude Code REPL integration for Doom Emacs."
  :group 'tools
  :prefix "claude-repl-")

(defcustom claude-repl-debug nil
  "Controls debug logging level.
nil means no logging; t means standard logging; \\='verbose also enables
high-frequency events (window changes, resolve-root, vterm-process-alive
predicates, sentinel re-entry).  Verbose mode also gates
`claude-repl--log-verbose's file writes: when debug is anything other
than \\='verbose, those calls are a no-op.  Use
\\[claude-repl-debug/toggle-logging] (with `C-u' prefix for verbose) to
flip at runtime."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)
                 (const :tag "Verbose" verbose))
  :group 'claude-repl)

(defcustom claude-repl-log-to-file t
  "Master kill-switch for file-writing of claude-repl log lines.
When non-nil (the default), every call to `claude-repl--log',
`claude-repl--do-log', or `claude-repl--error' appends its formatted
line to `claude-repl-log-file-name' — REGARDLESS of `claude-repl-debug'.
`claude-repl--log-verbose' is the exception: it ADDITIONALLY requires
`claude-repl-debug' to be \\='verbose, because its hot-path callers
(timer ticks, alive predicates) would otherwise spend ~25% of Emacs CPU
in `write-region'.  Use `claude-repl-debug/toggle-log-to-file' to flip
the kill-switch at runtime."
  :type 'boolean
  :group 'claude-repl)

(defcustom claude-repl-log-size-cap-bytes (* 1024 1024 1024)
  "Hard cap on the log file size in bytes.  Default 1 GiB.
Checked every `claude-repl-log-size-check-interval' writes (not on every
write — `file-attributes' on a multi-GB file is cheap but not free).
When the cap is exceeded, the first 80% of the file is dropped
(line-aligned) and a WARNING line is appended noting the truncation."
  :type 'integer
  :group 'claude-repl)

(defcustom claude-repl-log-size-check-interval 1000
  "Number of file-writes between size-cap checks.
Lower values catch overruns sooner but pay more `file-attributes' calls;
the default of 1000 keeps the check effectively free for typical usage
(one stat per ~1000 log lines)."
  :type 'integer
  :group 'claude-repl)

(defcustom claude-repl-workspace-id-length 8
  "Number of hex characters from MD5 hash used for workspace IDs.
Longer values reduce collision risk in setups with many workspaces."
  :type 'integer
  :group 'claude-repl)

(defcustom claude-repl-log-file-name "~/.claude/emacs/doom-claude-repl.log"
  "Path to the claude-repl log file.
The value is passed through `expand-file-name', and the parent directory
is created on demand by `claude-repl--logfile-path'."
  :type 'string
  :group 'claude-repl)

;; NOTE: claude-repl-default-workspace-name was removed as part of the
;; no-defaults-no-fallbacks refactor.  Buffer naming now errors when no
;; workspace name can be determined, rather than silently using "default"
;; (which caused unrelated contexts to collide on the same buffer).

(defcustom claude-repl-ws-name-allowed-chars-re "[^[:alnum:]_-]"
  "Regexp matching characters to replace in workspace names.
Characters matching this pattern are replaced with underscores."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-panel-buffer-name-format "*claude-panel%s-%s*"
  "Format string for Claude panel buffer names.
First %s is the suffix (e.g. \"-input\" or empty), second %s is the workspace name."
  :type 'string
  :group 'claude-repl)

;;; Logging

(defun claude-repl--ws-id-cached (ws)
  "Return the cached workspace ID hash for WS, computing if needed.
Uses :project-dir from the workspace state to derive the 8-char MD5 hash.
Caches the result under :ws-id to avoid repeated `file-truename' calls.
Returns nil if WS is nil or no :project-dir is set."
  (when ws
    (or (plist-get (gethash ws claude-repl--workspaces) :ws-id)
        (when-let ((dir (plist-get (gethash ws claude-repl--workspaces) :project-dir)))
          (let ((id (substring (md5 (directory-file-name (file-truename dir))) 0 claude-repl-workspace-id-length)))
            (puthash ws (plist-put (gethash ws claude-repl--workspaces) :ws-id id)
                     claude-repl--workspaces)
            id)))))

(defun claude-repl--format-ws-metadata (ws)
  "Return a context string with all workspace metadata for WS, or \"\".
Includes every meaningful key from the workspace plist in
`claude-repl--workspaces'.  Returns \"\" when WS is nil or has no
registered state.  Object-valued keys (buffers, processes, timers,
structs) are represented compactly (live/dead, running/nil, present/nil)."
  (if (or (null ws) (not (boundp 'claude-repl--workspaces)))
      ""
    (let ((plist (gethash ws claude-repl--workspaces)))
      (if (null plist)
          (format " {ws=%s}" ws)
        (let* ((id       (claude-repl--ws-id-cached ws))
               (dir      (plist-get plist :project-dir))
               (cstate   (plist-get plist :claude-state))
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

(defvar claude-repl--log-format-bug-captured nil
  "Set to t once a non-string FMT has been captured by `claude-repl--log-format'.
Prevents repeated backtrace captures from flooding the diagnostic buffer.")

(defun claude-repl--log-format-capture-bug (fmt)
  "Write a backtrace to *claude-repl-log-bug* the first time FMT isn't a string.
Lets us find the caller passing a bad FMT without crashing it.  Subsequent
bad calls are silently coerced so the log stays usable."
  (unless claude-repl--log-format-bug-captured
    (setq claude-repl--log-format-bug-captured t)
    (let ((buf (get-buffer-create "*claude-repl-log-bug*"))
          (bt (with-output-to-string (ignore-errors (backtrace)))))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (format "\n=== non-string fmt=%S at %s ===\n"
                        fmt (format-time-string "%H:%M:%S.%3N")))
        (insert bt)))))

(defun claude-repl--log-format (ws fmt)
  "Return FMT with a timestamp, [claude-repl] tag, and trailing workspace context.
WS is the workspace name (or nil for workspace-free contexts).  When non-nil,
all workspace metadata from `claude-repl--workspaces' is appended after FMT.

Hardened against non-string FMT: captures a backtrace to *claude-repl-log-bug*
the first time it happens, then coerces the value so the caller doesn't crash.

Note: callers using this to build a format string for `apply #'message'
should be aware that the returned string embeds the workspace metadata
literally, so any `%' characters in metadata will be interpreted as
format directives.  `claude-repl--do-log' avoids this by passing
metadata as an argument rather than splicing it into the format."
  (let ((safe-fmt (if (stringp fmt)
                      fmt
                    (claude-repl--log-format-capture-bug fmt)
                    (format "[BUG non-string-fmt=%S]" fmt))))
    (concat (format-time-string "%H:%M:%S.%3N") " [claude-repl] "
            safe-fmt (claude-repl--format-ws-metadata ws))))

(defun claude-repl--logfile-path ()
  "Return the expanded path of `claude-repl-log-file-name'.
The parent directory is created if it does not exist."
  (let* ((path (expand-file-name claude-repl-log-file-name))
         (dir (file-name-directory path)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    path))

(defvar claude-repl--log-write-counter 0
  "Monotonic counter of successful log-file writes.
Used by `claude-repl--do-log-to-file' to decide when to size-check.")

(defun claude-repl--log-truncate (path size)
  "Drop the first 80% of PATH (SIZE bytes) and append a WARNING line.
Reads the last 20% of the file as raw bytes, aligns to the next
newline (so we don't keep a partial first line), atomically replaces
PATH, then appends a single line noting the truncation.

Pure side-effect — no logging facilities are called here so we cannot
re-enter `claude-repl--do-log-to-file' and recurse."
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
                    "%s [claude-repl] WARNING: log truncated — file exceeded cap=%d bytes (was %d bytes); dropped first 80%%, kept last %d bytes"
                    (format-time-string "%H:%M:%S.%3N")
                    claude-repl-log-size-cap-bytes size keep-bytes)))
      (let ((coding-system-for-write 'no-conversion))
        (write-region (concat warning "\n") nil path t 'silent)))))

(defun claude-repl--log-maybe-truncate (path)
  "Truncate PATH when it exceeds `claude-repl-log-size-cap-bytes'.
Called periodically from `claude-repl--do-log-to-file'."
  (let ((attrs (file-attributes path)))
    (when attrs
      (let ((size (file-attribute-size attrs)))
        (when (and size (> size claude-repl-log-size-cap-bytes))
          (condition-case err
              (claude-repl--log-truncate path size)
            (error
             (message "[claude-repl] WARNING: log truncate failed for %s: %S"
                      path err))))))))

(defun claude-repl--do-log-to-file (text)
  "Append TEXT as a line to the logfile when `claude-repl-log-to-file' is non-nil.
No-ops when the logfile path cannot be determined.  Displays a warning
if a write error occurs (e.g. read-only filesystem) but does not signal
an error — logging must not break the caller.

Increments `claude-repl--log-write-counter' on every successful write
and runs `claude-repl--log-maybe-truncate' once every
`claude-repl-log-size-check-interval' writes."
  (when claude-repl-log-to-file
    (when-let ((path (claude-repl--logfile-path)))
      (condition-case err
          (progn
            (write-region (concat text "\n") nil path t 'silent)
            (cl-incf claude-repl--log-write-counter)
            (when (and (> claude-repl-log-size-check-interval 0)
                       (zerop (mod claude-repl--log-write-counter
                                   claude-repl-log-size-check-interval)))
              (claude-repl--log-maybe-truncate path)))
        (error (message "[claude-repl] WARNING: log write failed to %s: %S" path err))))))

(defun claude-repl--build-log-text (ws fmt args)
  "Build the formatted log line for WS / FMT / ARGS.
Shared by `claude-repl--do-log' and its message-gated wrappers so the
file-write path and the message-emit path always agree on the exact
text.  Handles the non-string-FMT bug-capture in one place."
  (if (stringp fmt)
      (let ((msg  (apply #'format fmt args))
            (ts   (format-time-string "%H:%M:%S.%3N"))
            (meta (claude-repl--format-ws-metadata ws)))
        (format "%s [claude-repl] %s%s" ts msg meta))
    (claude-repl--log-format-capture-bug fmt)
    (format "%s [claude-repl] [BUG non-string-fmt=%S]%s"
            (format-time-string "%H:%M:%S.%3N")
            fmt
            (claude-repl--format-ws-metadata ws))))

(defun claude-repl--do-log (ws fmt args &optional error-p)
  "Unconditional log entry: ALWAYS write to file AND emit to message/error.
WS is the workspace name for context (or nil).  When ERROR-P is non-nil,
signals the formatted line via `error' instead of `message' — the
file-write still happens first so the line is captured before unwinding.

This is the entry point for log calls that MUST be captured regardless
of `claude-repl-debug' — errors, invariant violations, and the
STUB-CREATE warnings.  Gated callers (`claude-repl--log',
`claude-repl--log-verbose') use the file-write path directly and
conditionally call `message' themselves."
  (let ((text (claude-repl--build-log-text ws fmt args)))
    (claude-repl--do-log-to-file text)
    (if error-p
        (error "%s" text)
      (message "%s" text))))

(defun claude-repl--log (ws fmt &rest args)
  "Log a timestamped message for WS, always to file, conditionally to *Messages*.
File write happens whenever `claude-repl-log-to-file' is non-nil (the
default) — REGARDLESS of `claude-repl-debug'.  The `message' call only
fires when `claude-repl-debug' is non-nil, so the minibuffer stays quiet
unless the user opts in.
FMT and ARGS use the same format conventions as `message'."
  (let ((text (claude-repl--build-log-text ws fmt args)))
    (claude-repl--do-log-to-file text)
    (when claude-repl-debug
      (message "%s" text))))

(defun claude-repl--log-verbose (ws fmt &rest args)
  "Log a high-frequency message for WS, gated on verbose-mode for BOTH sinks.
No-op unless `claude-repl-debug' is `verbose'.  The file write (which
profiling showed dominated Emacs CPU when this was always-on) and the
*Messages* emit are both behind the same gate, so hot-path callbacks
(timer ticks, window changes, resolve-root, async git sentinels) cost
nothing in the default-off configuration.  The `claude-repl-log-to-file'
kill-switch still wins — when it is nil, no file write occurs even in
verbose mode.  Toggle via \\[claude-repl-debug/toggle-logging] with a
`C-u' prefix."
  (when (eq claude-repl-debug 'verbose)
    (let ((text (claude-repl--build-log-text ws fmt args)))
      (claude-repl--do-log-to-file text)
      (message "%s" text))))

(defun claude-repl--error (ws fmt &rest args)
  "Signal an error with a [claude-repl] tag, timestamp, and workspace metadata.
WS is the workspace name for context (or nil).  FMT and ARGS are formatted
the same way `claude-repl--log' formats them, and the resulting line is also
written to the logfile before the error is signalled so the failure is
captured regardless of whether debug logging is on.

Unlike `claude-repl--log', this fires regardless of `claude-repl-debug' —
errors are not gated on the debug flag."
  (claude-repl--do-log ws fmt args t))

(defun claude-repl--rotate-log-on-startup ()
  "Rename an existing log file to `<path>.prev', preserving one prior session.
Idempotent: clobbers any existing `.prev'.  No-op when the current log
file does not exist or `claude-repl-log-to-file' is nil.  Errors are
caught and surfaced as a message — the rollover must not block startup."
  (when claude-repl-log-to-file
    (condition-case err
        (let* ((path (expand-file-name claude-repl-log-file-name))
               (prev (concat path ".prev")))
          (when (file-exists-p path)
            (when (file-exists-p prev) (delete-file prev))
            (rename-file path prev)
            ;; Reset the write counter — size accounting is per-file.
            (setq claude-repl--log-write-counter 0)))
      (error (message "[claude-repl] WARNING: log rotate failed: %S" err)))))

;; Run inline at load so each Emacs session begins with a fresh log file.
;; Guarded against `noninteractive' so ERT batch runs don't trash the user's
;; real log on every test invocation.
(unless noninteractive
  (claude-repl--rotate-log-on-startup))

;;; Git and workspace identity

(defun claude-repl--dir-has-git-p (d)
  "Return non-nil if directory D contains a .git directory or file."
  (let ((git (expand-file-name ".git" d)))
    (or (file-directory-p git) (file-regular-p git))))

(defun claude-repl--git-root (&optional dir)
  "Find the git root by walking up from DIR (default `default-directory').
Checks for both .git directory and .git file (worktrees)."
  (let* ((dir (or dir default-directory))
         (root (locate-dominating-file dir #'claude-repl--dir-has-git-p)))
    (claude-repl--log-verbose nil "git-root: dir=%s root=%s" dir root)
    (when root (claude-repl--path-canonical root))))

(defun claude-repl--git-string (&rest args)
  "Run a synchronous git command and return its trimmed output.
ARGS are the git subcommand and arguments.  Each argument is shell-quoted
before being passed to `shell-command-to-string'.
Note: stderr is included in the output (Emacs default).  Use
`claude-repl--git-string-quiet' when errors should be silently swallowed."
  (string-trim
   (shell-command-to-string
    (mapconcat #'shell-quote-argument (cons "git" args) " "))))

(defun claude-repl--git-string-quiet (&rest args)
  "Like `claude-repl--git-string' but suppress stderr.
Returns an empty string when git fails, rather than error text.
Suitable for init-time calls that may run outside a git repository."
  (string-trim
   (shell-command-to-string
    (concat (mapconcat #'shell-quote-argument (cons "git" args) " ")
            " 2>/dev/null"))))

(defun claude-repl--gh-string-quiet (&rest args)
  "Run a synchronous `gh' command and return its trimmed stdout, suppressing stderr.
ARGS are the `gh' subcommand and arguments; each is shell-quoted.
Returns an empty string when `gh' fails (no PR for branch, not
authenticated, etc.).  The wrapper IS the external boundary for the
GitHub CLI: tests must mock this function via `cl-letf' rather than
invoke real `gh' (see AGENTS.md \"No External Processes or External
State in Tests\")."
  (string-trim
   (shell-command-to-string
    (concat (mapconcat #'shell-quote-argument (cons "gh" args) " ")
            " 2>/dev/null"))))

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

(defvar claude-repl--external-boundary-functions
  '(claude-repl--git-string
    claude-repl--git-string-quiet
    claude-repl--git-exit-code
    claude-repl--git-branch-exists-p
    claude-repl--git-tag-exists-p
    claude-repl--async-git
    claude-repl--gh-string-quiet)
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
;; `claude-repl-print-git-branch', which caches the result.
(defvar claude-repl-git-branch nil
  "Cached git branch active when `claude-repl-print-git-branch' was first called.
Populated lazily on first call; remains nil until then.  Do not rely
on this being set at load time.")

(defun claude-repl--resolve-current-git-root ()
  "Resolve the git root for the caller's current context.
Prefers the current workspace's `:project-dir' when one is registered,
otherwise falls back to `default-directory'.  Signals `user-error' when
the resolved directory is not inside a git repository.

Intended to be called exactly once per workspace, at creation time, so
new worktrees are always rooted at the repository the user is currently
working in (rather than wherever Emacs happened to be launched)."
  (let* ((ws-dir (ignore-errors (claude-repl--ws-dir (+workspace-current-name))))
         (dir (or ws-dir default-directory))
         (default-directory dir)
         (raw (claude-repl--git-string-quiet "rev-parse" "--show-toplevel")))
    (when (string-empty-p raw)
      (user-error "claude-repl: %s is not inside a git repository" dir))
    (file-name-as-directory raw)))

(defun claude-repl-print-git-branch ()
  "Print the git branch that was active when claude-repl config was loaded.
Lazily computes and caches the value on first invocation."
  (interactive)
  (unless claude-repl-git-branch
    (setq claude-repl-git-branch
          (claude-repl--git-string-quiet "rev-parse" "--abbrev-ref" "HEAD")))
  (message "claude-repl loaded on branch: %s" claude-repl-git-branch))

(defun claude-repl--path-canonical (path)
  "Return a canonical, stable string for PATH suitable for hashing.
Expands tildes and symlinks via `file-truename', then strips any trailing slash
via `directory-file-name' so that the same directory always produces the same hash."
  (directory-file-name (file-truename path)))

(defun claude-repl--workspace-id ()
  "Return a short identifier for the current git workspace.
Uses an MD5 hash of the canonical project root path from the workspace hashmap.
Returns nil when no workspace has a registered `:project-dir' — callers are
expected to only invoke this from contexts where a workspace is active."
  (let* ((root (ignore-errors (claude-repl--ws-dir (+workspace-current-name))))
         (id (when root
               (substring (md5 (claude-repl--path-canonical root)) 0 claude-repl-workspace-id-length))))
    (claude-repl--log-verbose nil "workspace-id: root=%s id=%s" root id)
    id))

;;; Workspace state management
;;
;; Single hash table for all per-workspace state.
;;
;; NOTE: workspace name ≠ git branch name.
;; `claude-repl--do-create-worktree-workspace' derives the persp name from the
;; *last path component* of the input (e.g. "DWC/fix-login" → persp "fix-login"),
;; while the full input becomes the branch name ("DWC/fix-login").  Never assume
;; the two are equal.  To resolve a workspace to its branch, retrieve its
;; :project-dir from this hash and run `git rev-parse --abbrev-ref HEAD' there.
(cl-defstruct claude-repl-instantiation
  "Per-environment session state for a Claude REPL workspace.
Each workspace has one instantiation for :sandbox and one for :bare-metal."
  session-id    ; Claude Code session ID, captured from the `session_start' hook payload via `claude-repl--update-session-id-from-sentinel'
  start-cmd)    ; last startup command (for logging/display)

(defvar claude-repl--workspaces (make-hash-table :test 'equal)
  "Hash table mapping workspace name → state plist.
Keys: :vterm-buffer :input-buffer
      :prefix-counter :claude-state :repl-state
      :git-clean :git-proc :worktree-p :project-dir
      :active-env :sandbox :bare-metal :fork-session-id
      :ready-timer :priority
      :pending-prompts :pending-show-panels :deferred-prompts
:active-env is :sandbox or :bare-metal; :sandbox and :bare-metal are
`claude-repl-instantiation' structs holding per-environment session state.")

(defun claude-repl--ws-get (ws key)
  "Get KEY from workspace WS's plist."
  (plist-get (gethash ws claude-repl--workspaces) key))

(defun claude-repl--ws-put-caller-trace ()
  "Return a short caller chain string for diagnostic logging.
Used by `claude-repl--ws-put' to identify the producer of stub-create
calls (entries written without `:project-dir').  Filters
`backtrace-frames' to function-call frames only (the `EVALD' slot is
t for fully-evaluated function calls, nil for special-form / macro
frames), so the trace is named-function symbols rather than
`let'/`and'/`if' noise.  Returns at most 8 frames joined with ` <- `.
Wrapped in `ignore-errors' at the call site so any failure here
cannot break `--ws-put' itself."
  (let ((frames (and (fboundp 'backtrace-frames) (backtrace-frames)))
        (collected nil))
    (dolist (frame frames)
      (let ((evald (nth 0 frame))
            (fn    (nth 1 frame)))
        (when (and evald
                   (symbolp fn)
                   (not (memq fn '(claude-repl--ws-put-caller-trace
                                   claude-repl--ws-put
                                   backtrace-frames)))
                   (< (length collected) 8))
          (push fn collected))))
    (if collected
        (mapconcat #'symbol-name (nreverse collected) " <- ")
      "<no-trace>")))

(defun claude-repl--ws-put (ws key val)
  "Set KEY to VAL in workspace WS's plist in `claude-repl--workspaces'.
Internally uses plist-put (which returns a new list) threaded into puthash.

Emits an unconditional log line (via `claude-repl--do-log', bypassing
`claude-repl-debug') when this call CREATES a fresh hash entry whose
plist will lack `:project-dir' — the shape that leaks workspaces into
the drawer's `(no repo)' bucket.  Includes a caller trace so the
producer can be identified without first turning debug logging on."
  (let ((stub-create (and (null (gethash ws claude-repl--workspaces))
                          (not (eq key :project-dir)))))
    (puthash ws (plist-put (gethash ws claude-repl--workspaces) key val)
             claude-repl--workspaces)
    (when stub-create
      (let ((trace (or (ignore-errors (claude-repl--ws-put-caller-trace))
                       "<trace-failed>")))
        (claude-repl--do-log
         ws
         "ws-put: STUB-CREATE ws=%s key=%s val=%S — entry created without :project-dir (will appear under drawer \"(no repo)\" bucket). caller-trace=%s"
         (list ws key val trace))))))

(defconst claude-repl--ws-runtime-keys
  '(:claude-state :repl-state :vterm-buffer :input-buffer :vterm-status
    :ready-timer :git-proc :flashing :pending-subagents :pending-show-panels
    :fork-session-id :fullscreen-config :active-env :sandbox :bare-metal
    :deferred-input-queue :done-ack :permission-prompt-active
    :done-ack-pending :source-ws-name)
  "Plist keys cleared by `claude-repl--ws-del' when tombstoning a workspace.
Anything not in this list is treated as identity/historical and survives
the tombstone — notably `:project-dir', `:created-at', `:last-killed-at',
`:priority', `:worktree-p', `:source-ws-dir', `:ws-id', and the
`:merge-completed*' family.  Preserving `:project-dir' across tombstone
is what lets `claude-repl--ws-dir' callers (magit-status, async git,
ws-id hashing) keep working on a persp that outlives its claude-repl
session — the failure mode that previously surfaced as
`no :project-dir for workspace X' errors after a nuke.")

(defun claude-repl--ws-live-p (ws)
  "Return non-nil iff WS is a live (non-tombstoned) registered workspace.
A workspace is live when it has a hash entry AND no `:nuked-at'
tombstone marker.  The single liveness predicate used by every hash
iterator that previously relied on the implicit `presence == live'
invariant (drawer, picker, periodic state updater, reverse-lookup) so
tombstoned entries don't surface in any UI/runtime path.

Uses a sentinel default in `gethash' so a registered entry whose plist
happens to be the empty list (`nil') is still counted as present —
distinguishing `key absent' from `key bound to ()'."
  (let ((plist (gethash ws claude-repl--workspaces 'claude-repl--ws-absent)))
    (and (not (eq plist 'claude-repl--ws-absent))
         (null (plist-get plist :nuked-at)))))

(defun claude-repl--live-ws-names ()
  "Return the list of live workspace names (hash keys minus tombstones).
Single helper for callers that previously did
`(hash-table-keys claude-repl--workspaces)' as a stand-in for `live
workspaces' — that idiom now over-includes tombstones, so route
through this filter instead."
  (cl-remove-if-not #'claude-repl--ws-live-p
                    (hash-table-keys claude-repl--workspaces)))

(defun claude-repl--ws-del (ws)
  "Tombstone workspace WS instead of removing its hash entry.
Stamps `:nuked-at' with the current time, clears every key in
`claude-repl--ws-runtime-keys' (vterm buffer / proc refs, timers,
session-bound state), and preserves identity/historical keys
(`:project-dir', `:created-at', `:last-killed-at', `:priority',
`:worktree-p', `:source-ws-dir', `:ws-id', merge metadata).  The entry
remains in `claude-repl--workspaces' so `claude-repl--ws-dir' and
reverse-lookups still resolve, but `claude-repl--ws-live-p' returns
nil and every filtered iterator (drawer, picker, periodic updater)
ignores the entry — preserving the prior UX of `nuke removes the
workspace from view' without destroying the identity record.

Sweeps peers' cached `:source-ws-name' so a tombstoned WS can never be
returned as a valid parent name.  `:last-killed-at' is bumped here too
so the picker's sort-by-last-killed sees this tombstone immediately.

No-op (beyond the log line) when WS has no hash entry — the bare ws-del
log line preserves the pre-existing diagnostic shape."
  (let ((had-entry (not (null (gethash ws claude-repl--workspaces)))))
    (claude-repl--log ws "ws-del: ws=%s had-entry=%s (tombstone)"
                      ws (if had-entry "t" "nil"))
    (maphash (lambda (peer plist)
               (when (equal (plist-get plist :source-ws-name) ws)
                 (claude-repl--ws-put peer :source-ws-name nil)))
             claude-repl--workspaces)
    (when had-entry
      (dolist (key claude-repl--ws-runtime-keys)
        (claude-repl--ws-put ws key nil))
      (claude-repl--ws-put ws :last-killed-at (current-time))
      (claude-repl--ws-put ws :nuked-at (current-time)))))

(defun claude-repl--active-inst (ws)
  "Return the active `claude-repl-instantiation' for workspace WS.
Signals an error if the environment or instantiation struct is missing —
both must be initialized by `claude-repl--initialize-ws-env' before this is called."
  (let ((env (claude-repl--ws-get ws :active-env)))
    (unless env
      (error "claude-repl--active-inst: workspace %s has no :active-env (initialize-ws-env not called?)" ws))
    (let ((inst (claude-repl--ws-get ws env)))
      (unless inst
        (error "claude-repl--active-inst: no instantiation struct for ws=%s env=%s (initialize-ws-env not called?)" ws env))
      inst)))

(defvar-local claude-repl--owning-workspace nil
  "Workspace name that owns this claude session.
Set when the user sends a message; used to correctly target workspace
state changes regardless of which persp the buffer drifts into.")
(put 'claude-repl--owning-workspace 'permanent-local t)

;;; Buffer naming and predicates

;; Panel buffers use the "claude-panel-" prefix to distinguish them from
;; other claude-repl utility buffers (e.g. *claude-repl-dump*,
;; *claude-repl-log-bug*).  The vterm regex is still a superset that
;; matches input buffers too; `claude-repl--claude-buffer-p' explicitly
;; excludes them.
(defconst claude-repl--vterm-buffer-re "^\\*claude-panel-[[:alnum:]_-]+\\*$"
  "Regexp matching Claude panel buffer names (e.g. *claude-panel-my-workspace*).
Caveat: also matches input buffer names.  Use `claude-repl--claude-buffer-p'
for the combined check.")

(defconst claude-repl--input-buffer-re "^\\*claude-panel-input-[[:alnum:]_-]+\\*$"
  "Regexp matching Claude input buffer names (e.g. *claude-panel-input-my-workspace*).")

(defun claude-repl--sanitize-ws-name (name)
  "Return NAME with unsafe characters replaced by underscores.
Keeps alphanumerics, hyphens, and underscores.  Returns nil for nil NAME."
  (when name
    (replace-regexp-in-string claude-repl-ws-name-allowed-chars-re "_" name)))

(defun claude-repl--buffer-name (&optional suffix ws)
  "Return a workspace-specific buffer name like *claude-panel-WS* or *claude-panel-input-WS*.
SUFFIX, if provided, is inserted before the workspace name (e.g. \"-input\").
WS, if provided, is the workspace name; otherwise uses the current workspace.
Signals an error when the resolved workspace name is nil or empty — an
empty id produces buffer names like *claude-panel-*, which the
`claude-repl--vterm-buffer-re' / `claude-repl--input-buffer-re' regexes
mis-classify (input names match the vterm regex with id=\"input-\"),
causing `claude-repl--sync-panels' to delete the input panel as orphaned."
  (let* ((ws-name (or ws (and (fboundp '+workspace-current-name)
                              (+workspace-current-name))))
         (safe (claude-repl--sanitize-ws-name ws-name)))
    (when (or (null safe) (string-empty-p safe))
      (error "claude-repl--buffer-name: empty workspace name (ws=%S, +workspace-current-name=%S, sanitized=%S)"
             ws (and (fboundp '+workspace-current-name) (+workspace-current-name)) safe))
    (let ((name (format claude-repl-panel-buffer-name-format (or suffix "") safe)))
      (claude-repl--log-verbose nil "buffer-name: suffix=%s ws=%s name=%s" suffix ws-name name)
      name)))

(defun claude-repl--create-buffer (ws &optional suffix)
  "Create a workspace-owned buffer for WS and return it.
SUFFIX is passed to `claude-repl--buffer-name' to select the buffer's
role: nil for the vterm buffer (*claude-panel-WS*), \"-input\" for the input
buffer (*claude-panel-input-WS*).

Single entry point for every workspace-owned buffer.  Derives the
canonical name, sets `claude-repl--owning-workspace' buffer-locally
(permanent-local so it survives subsequent major-mode activation), and
registers the buffer with WS's perspective so it appears in
`+workspace-buffer-list' and related listings.

Idempotent — `get-buffer-create' returns an existing buffer of the
same name, and `persp-add-buffer' internally no-ops when the buffer is
already in the perspective.  Skips persp attachment when WS is nil or
no perspective named WS exists (e.g. early in session startup)."
  (let ((buf (get-buffer-create (claude-repl--buffer-name suffix ws))))
    (with-current-buffer buf
      (setq-local claude-repl--owning-workspace ws))
    (when (and ws
               (fboundp 'persp-get-by-name)
               (fboundp 'persp-add-buffer))
      (when-let ((persp (persp-get-by-name ws)))
        (persp-add-buffer buf persp nil)))
    buf))

(defun claude-repl--claude-buffer-p (&optional buf)
  "Return non-nil if BUF (default: current buffer) is a Claude vterm buffer.
Excludes Claude input buffers (which share a common prefix)."
  (let ((name (buffer-name (or buf (current-buffer)))))
    (and (string-match-p claude-repl--vterm-buffer-re name)
         (not (string-match-p claude-repl--input-buffer-re name)))))

(defun claude-repl--claude-panel-buffer-p (&optional buf)
  "Return non-nil if BUF (default: current buffer) is any Claude panel buffer.
Matches both vterm and input buffers."
  (let ((name (buffer-name (or buf (current-buffer)))))
    (or (string-match-p claude-repl--vterm-buffer-re name)
        (string-match-p claude-repl--input-buffer-re name))))

(defun claude-repl--non-user-buffer-p (buf)
  "Return non-nil if BUF is not a user-facing buffer.
Matches Claude panel buffers, minibuffers, and dead/nil buffers.
BUF may be a buffer object or a name string."
  (let* ((b (if (stringp buf) (get-buffer buf) buf))
         (name (and b (buffer-name b))))
    (or (not name)
        (claude-repl--claude-panel-buffer-p b)
        (string-match-p "^ \\*Minibuf" name))))

(defun claude-repl--non-claude-buffers (buffers)
  "Return BUFFERS with Claude panels, minibuffers, and dead buffers removed.
BUFFERS may be buffer objects or name strings."
  (cl-remove-if #'claude-repl--non-user-buffer-p buffers))

;;; Workspace and vterm helpers

(defun claude-repl--current-ws-p (ws)
  "Return non-nil when WS is the currently active workspace name."
  (string= ws (+workspace-current-name)))

(defun claude-repl--current-ws-live-vterm ()
  "Return the live vterm buffer for the current workspace, or nil.
Looks up :vterm-buffer in the current workspace state and returns it only if
the buffer object is still live."
  (let* ((ws (+workspace-current-name))
         (buf (claude-repl--ws-get ws :vterm-buffer))
         (live (and buf (buffer-live-p buf))))
    (claude-repl--log-verbose ws "current-ws-live-vterm: buf=%s live=%s" buf live)
    (when live buf)))

(defun claude-repl--vterm-live-p ()
  "Return non-nil if the Claude vterm buffer for the current workspace exists and is live."
  (not (null (claude-repl--current-ws-live-vterm))))

(defmacro claude-repl--with-vterm-buf (&rest body)
  "Execute BODY with `vterm-buf' bound to the current workspace's live vterm buffer.
If the vterm buffer does not exist or is dead, BODY is not executed and the
form returns nil."
  (declare (indent 0) (debug body))
  `(when-let ((vterm-buf (claude-repl--current-ws-live-vterm)))
     ,@body))


