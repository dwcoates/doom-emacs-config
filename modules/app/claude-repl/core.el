;;; core.el --- claude-repl core definitions -*- lexical-binding: t; -*-

;;; Code:

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
nil means no logging; t means standard logging; \\='verbose also includes
high-frequency events (window changes, resolve-root)."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)
                 (const :tag "Verbose" verbose))
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
          (let ((id (substring (md5 (directory-file-name (file-truename dir))) 0 8)))
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
               (pshow    (plist-get plist :pending-show-panels)))
          (format (concat " {ws=%s id=%s dir=%s cst=%s rst=%s env=%s"
                          " vt=%s in=%s cnt=%s"
                          " git=%s gproc=%s wt=%s fork=%s"
                          " rtmr=%s pri=%s pend=%s pshow=%s}")
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
                  (if pshow "t" "-")))))))

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
  "Return FMT prefixed with a timestamp, [claude-repl] tag, and workspace context.
WS is the workspace name (or nil for workspace-free contexts).  When non-nil,
all workspace metadata from `claude-repl--workspaces' is included.

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
    (concat (format-time-string "%H:%M:%S.%3N") " [claude-repl]"
            (claude-repl--format-ws-metadata ws) " " safe-fmt)))

(defun claude-repl--do-log (ws fmt args)
  "Internal: emit FMT + ARGS via `message' with a timestamp and WS-metadata prefix.
Passes the timestamp and workspace metadata as %s arguments rather than
splicing them into the format string — so any `%' characters in the
metadata (e.g. from a project path) or in the prefix are treated as data,
not format directives, and can't cause \"Not enough arguments for format
string\" arity mismatches.

When FMT isn't a string (a caller bug), captures a backtrace to
*claude-repl-log-bug* on the first occurrence, then logs a safe
[BUG non-string-fmt=...] line without interpreting caller ARGS."
  (if (stringp fmt)
      (apply #'message
             (concat "%s [claude-repl]%s " fmt)
             (format-time-string "%H:%M:%S.%3N")
             (claude-repl--format-ws-metadata ws)
             args)
    (claude-repl--log-format-capture-bug fmt)
    (message "%s [claude-repl]%s [BUG non-string-fmt=%S]"
             (format-time-string "%H:%M:%S.%3N")
             (claude-repl--format-ws-metadata ws)
             fmt)))

(defun claude-repl--log (ws fmt &rest args)
  "Log a timestamped debug message when `claude-repl-debug' is non-nil.
WS is the workspace name for context (or nil for workspace-free contexts).
FMT and ARGS are passed to `message', prefixed with timestamp, [claude-repl]
tag, and full workspace metadata."
  (when claude-repl-debug
    (claude-repl--do-log ws fmt args)))

(defun claude-repl--log-verbose (ws fmt &rest args)
  "Log a timestamped debug message only when `claude-repl-debug' is \\='verbose.
WS is the workspace name for context (or nil).
Used for high-frequency, low-signal events."
  (when (eq claude-repl-debug 'verbose)
    (claude-repl--do-log ws fmt args)))

;;; Git and workspace identity

(defvar-local claude-repl--project-root nil
  "Buffer-local git root for Claude REPL buffers.
Set when vterm/input buffers are created so workspace-id works from them.")

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

(defvar claude-repl-git-branch
  (claude-repl--git-string-quiet "rev-parse" "--abbrev-ref" "HEAD")
  "The git branch active when claude-repl config was loaded.")

(defvar claude-repl--main-git-root
  (file-name-as-directory (claude-repl--git-string-quiet "rev-parse" "--show-toplevel"))
  "The main git root captured at module load time.
Used by the workspace-generation file watcher so new sessions are always
created from the main repo, not the currently selected worktree.")

(defun claude-repl-print-git-branch ()
  "Print the git branch that was active when claude-repl config was loaded."
  (interactive)
  (message "claude-repl loaded on branch: %s" claude-repl-git-branch))

(defun claude-repl--path-canonical (path)
  "Return a canonical, stable string for PATH suitable for hashing.
Expands tildes and symlinks via `file-truename', then strips any trailing slash
via `directory-file-name' so that the same directory always produces the same hash."
  (directory-file-name (file-truename path)))

(defun claude-repl--resolve-root ()
  "Return the project root directory.
Tries git root, then buffer-local project root, then `default-directory'."
  (let* ((git (claude-repl--git-root))
         (root (or git claude-repl--project-root default-directory))
         (source (cond (git "git") (claude-repl--project-root "buffer-local") (t "default-directory"))))
    (claude-repl--log-verbose nil "resolve-root source=%s root=%s" source root)
    (claude-repl--path-canonical root)))

(defun claude-repl--workspace-id ()
  "Return a short identifier for the current git workspace.
Uses an MD5 hash of the canonical git root path.  Falls back to the buffer-local
`claude-repl--project-root' and then `default-directory'."
  (let* ((root (claude-repl--resolve-root))
         (id (when root
               (substring (md5 (claude-repl--path-canonical root)) 0 8))))
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
  had-session   ; non-nil once Claude has run in this environment
  session-id    ; Claude Code session ID, captured from the `session_start' hook payload via `claude-repl--update-session-id-from-sentinel'
  start-cmd)    ; last startup command (for logging/display)

(defvar claude-repl--workspaces (make-hash-table :test 'equal)
  "Hash table mapping workspace name → state plist.
Keys: :vterm-buffer :input-buffer
      :prefix-counter :claude-state :repl-state
      :git-clean :git-proc :worktree-p :project-dir
      :active-env :sandbox :bare-metal :fork-session-id
      :ready-timer :priority
      :pending-prompts :pending-show-panels
:active-env is :sandbox or :bare-metal; :sandbox and :bare-metal are
`claude-repl-instantiation' structs holding per-environment session state.")

(defun claude-repl--ws-get (ws key)
  "Get KEY from workspace WS's plist."
  (plist-get (gethash ws claude-repl--workspaces) key))

(defun claude-repl--ws-put (ws key val)
  "Set KEY to VAL in workspace WS's plist in `claude-repl--workspaces'.
Internally uses plist-put (which returns a new list) threaded into puthash."
  (puthash ws (plist-put (gethash ws claude-repl--workspaces) key val)
           claude-repl--workspaces))

(defun claude-repl--ws-del (ws)
  "Remove all state for workspace WS."
  (remhash ws claude-repl--workspaces))

(defun claude-repl--active-inst (ws)
  "Return the active `claude-repl-instantiation' for workspace WS.
Creates the struct if not yet initialized for the current environment."
  (let ((env (or (claude-repl--ws-get ws :active-env) :bare-metal)))
    (or (claude-repl--ws-get ws env)
        (let ((inst (make-claude-repl-instantiation)))
          (claude-repl--log ws "active-inst: creating new instantiation env=%s" env)
          (claude-repl--ws-put ws env inst)
          inst))))

(defvar-local claude-repl--owning-workspace nil
  "Workspace name that owns this claude session.
Set when the user sends a message; used to correctly target workspace
state changes regardless of which persp the buffer drifts into.")

;;; Buffer naming and predicates

(defconst claude-repl--vterm-buffer-re "^\\*claude-[0-9a-f]+\\*$"
  "Regexp matching Claude vterm buffer names (e.g. *claude-ab12cd34*).")

(defconst claude-repl--input-buffer-re "^\\*claude-input-[0-9a-f]+\\*$"
  "Regexp matching Claude input buffer names (e.g. *claude-input-ab12cd34*).")

(defun claude-repl--buffer-name (&optional suffix)
  "Return a project-specific buffer name like *claude-HASH* or *claude-input-HASH*.
SUFFIX, if provided, is inserted before the hash (e.g. \"-input\")."
  (let* ((id (claude-repl--workspace-id))
         (name (format "*claude%s-%s*" (or suffix "") (or id "default"))))
    (claude-repl--log-verbose nil "buffer-name: suffix=%s name=%s" suffix name)
    name))

(defun claude-repl--claude-buffer-p (&optional buf)
  "Return non-nil if BUF (default: current buffer) is a Claude vterm buffer."
  (string-match-p claude-repl--vterm-buffer-re
                  (buffer-name (or buf (current-buffer)))))

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


