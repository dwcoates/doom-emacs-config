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

(defun claude-repl--capture-process-output (program args &optional suppress-stderr timeout)
  "Run PROGRAM with ARGS, capture stdout, return its trimmed contents.
Internal helper used by the per-binary capturing wrappers below
\(`claude-repl--git-string', `claude-repl--git-string-quiet',
`claude-repl--gh-string-quiet').  Not registered as an external
boundary in its own right because tests mock the per-binary wrappers,
which sit one layer above it.

Worker-thread safe: routes the wait through
`claude-repl--wait-for-process-exit', so on macOS this does NOT trap
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
                      (format " *claude-repl-capture-%s*" program)))
         (stderr-buf (when suppress-stderr
                       (generate-new-buffer
                        (format " *claude-repl-capture-%s-stderr*"
                                program)))))
    (unwind-protect
        (let ((proc (if suppress-stderr
                        (make-process ;; ALLOW-EXTERNAL-BOUNDARY
                         :name (format "claude-repl-capture-%s" program)
                         :command (cons program args)
                         :buffer stdout-buf
                         :stderr stderr-buf
                         :connection-type 'pipe
                         :noquery t)
                      (apply #'start-process ;; ALLOW-EXTERNAL-BOUNDARY
                             (format "claude-repl-capture-%s" program)
                             stdout-buf
                             program args))))
          (set-process-query-on-exit-flag proc nil)
          (let ((status (claude-repl--wait-for-process-exit
                         proc timeout nil nil)))
            (cond
             ((eq status 'timeout) "")
             (t
              (with-current-buffer stdout-buf
                (string-trim
                 (buffer-substring-no-properties
                  (point-min) (point-max))))))))
      (when (buffer-live-p stdout-buf) (kill-buffer stdout-buf))
      (when (and stderr-buf (buffer-live-p stderr-buf))
        (kill-buffer stderr-buf)))))

(defun claude-repl--git-string (&rest args)
  "Run a synchronous git command and return its trimmed output.
ARGS are the git subcommand and arguments.
Note: stderr is included in the output (Emacs default).  Use
`claude-repl--git-string-quiet' when errors should be silently swallowed.

Routes through `claude-repl--capture-process-output' so this is safe
to call from worker threads on macOS — `shell-command-to-string'
would trap in `ns_select_1' + `[NSApp run]'.  See AGENTS.md
`ns_select_1 worker-thread trap'."
  (claude-repl--capture-process-output "git" args nil))

(defun claude-repl--git-string-quiet (&rest args)
  "Like `claude-repl--git-string' but suppress stderr.
Returns an empty string when git fails, rather than error text.
Suitable for init-time calls that may run outside a git repository.

Routes through `claude-repl--capture-process-output' for worker-thread
safety on macOS; see `claude-repl--git-string'."
  (claude-repl--capture-process-output "git" args t))

(defun claude-repl--gh-string-quiet (&rest args)
  "Run a synchronous `gh' command and return its trimmed stdout, suppressing stderr.
ARGS are the `gh' subcommand and arguments.  Returns an empty string
when `gh' fails (no PR for branch, not authenticated, etc.).  The
wrapper IS the external boundary for the GitHub CLI: tests must mock
this function via `cl-letf' rather than invoke real `gh' (see
AGENTS.md \"No External Processes or External State in Tests\").

Routes through `claude-repl--capture-process-output' for worker-thread
safety on macOS; see `claude-repl--git-string'."
  (claude-repl--capture-process-output "gh" args t))

(defun claude-repl--async-gh (label dir args callback)
  "Run `gh ARGS' asynchronously in DIR and call CALLBACK on completion.
LABEL is used to name the process and its output buffer.
CALLBACK is called as `(CALLBACK ok output)' where OK is non-nil when
gh exited with status 0 and OUTPUT is the trimmed stdout string.

This IS the external-boundary wrapper for async `gh' invocations —
tests must mock this function via `cl-letf' rather than spawn real
gh (see AGENTS.md \"No External Processes or External State in Tests\")."
  (let* ((buf (generate-new-buffer (format " *claude-repl-%s*" label)))
         (default-directory (file-name-as-directory dir))
         (proc (apply #'start-process  ;; ALLOW-EXTERNAL-BOUNDARY
                      (format "claude-repl-%s" label)
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

(defun claude-repl--docker-exit-code (&rest args)
  "Run `docker ARGS' synchronously and return its exit code (stdout discarded).
This IS the external-boundary wrapper for the Docker CLI: tests must
mock this function via `cl-letf' rather than invoke real `docker'
\(see AGENTS.md \"No External Processes or External State in Tests\")."
  (apply #'call-process "docker" nil nil nil args)) ;; ALLOW-EXTERNAL-BOUNDARY

(defun claude-repl--make-process-git (name args sentinel)
  "Async git via `make-process'.
NAME is the process name (a string); ARGS is the git subcommand
argument list (no leading \"git\"); SENTINEL is the process sentinel.
Returns the live process so the caller can record / kill / inspect it.

This IS the external-boundary wrapper for `make-process'-style async
git invocations — distinct from `claude-repl--async-git', which uses
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

(defvar claude-repl--external-boundary-functions
  '(claude-repl--git-string
    claude-repl--git-string-quiet
    claude-repl--git-exit-code
    claude-repl--git-branch-exists-p
    claude-repl--git-tag-exists-p
    claude-repl--async-git
    claude-repl--gh-string-quiet
    claude-repl--early-git-string
    claude-repl--early-git-exit-code
    claude-repl--docker-exit-code
    claude-repl--make-process-git
    claude-repl--async-gh)
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
  (let* ((ws-dir (ignore-errors (claude-repl--ws-dir (claude-repl--ws-current-name))))
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
  (let* ((root (ignore-errors (claude-repl--ws-dir (claude-repl--ws-current-name))))
         (id (when root
               (substring (md5 (claude-repl--path-canonical root)) 0 claude-repl-workspace-id-length))))
    (claude-repl--log-verbose nil "workspace-id: root=%s id=%s" root id)
    id))

;;; Workspace state management
;;
;; The `claude-repl--workspaces' hash table, its accessors
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
  (let* ((ws-name (or ws (claude-repl--ws-current-name)))
         (safe (claude-repl--sanitize-ws-name ws-name)))
    (when (or (null safe) (string-empty-p safe))
      (error "claude-repl--buffer-name: empty workspace name (ws=%S, +workspace-current-name=%S, sanitized=%S)"
             ws (claude-repl--ws-current-name) safe))
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
    (when ws
      (when-let ((persp (claude-repl--ws-resolve-persp ws)))
        (claude-repl--ws-add-buffer buf persp nil)))
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
  (string= ws (claude-repl--ws-current-name)))

(defun claude-repl--current-ws-live-vterm ()
  "Return the live vterm buffer for the current workspace, or nil.
Looks up :vterm-buffer in the current workspace state and returns it only if
the buffer object is still live."
  (let* ((ws (claude-repl--ws-current-name))
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


