;;; merge-handlers.el --- Repo-routed /workspace-merge dispatch -*- lexical-binding: t; -*-

;;; Commentary:

;; Pluggable post-processing for `/workspace-merge' command-file
;; dispatch.  The skill's JSON contract is uniform — `{"type":"merge",
;; "workspace":"..."}` — but what the editor does on receipt is keyed
;; by the target workspace's repo via a small registry of named
;; handlers.
;;
;; Lookup order at dispatch time:
;;   1. `<repo-root>/.claude/emacs/workspace-merge.eld' — repo-checked-in
;;      data file naming a registered handler symbol plus optional args.
;;      Read with `read', NEVER evaluated, so repo content cannot inject
;;      arbitrary code into Emacs.
;;   2. `claude-repl-workspace-merge-handler-overrides' — user-side
;;      defcustom alist keyed by canonical repo-root path.  Acts as the
;;      fallback for repos that haven't opted in via the .eld file.
;;   3. `cherry-pick' — the default handler, preserving the pre-routing
;;      behaviour of `claude-repl--handle-merge-command'.
;;
;; Each handler function has signature `(TARGET-WS &optional ARGS)' and
;; is responsible for performing the post-merge work and recording
;; terminal state via the shared helpers in worktree.el
;; (`--mark-merge-failed', `--close-workspace', drawer refresh, etc.).
;;
;; PR polling (`refresh-master-from-origin' handler):
;;   When the handler is invoked for a repo whose PR is still in the
;;   merge queue (or not yet merged), it does NOT block the worker
;;   thread.  Instead it defers an async polling loop to the main
;;   thread.  Each poll tick spawns `gh pr view --json state,mergedAt'
;;   as a subprocess (`make-process') so the main thread is never
;;   blocked waiting for the GitHub API.  The process sentinel handles
;;   the result: merged → fetch+ff+close; closed-not-merged → revive
;;   workspace; still-open → wait for the next tick.  Active polls are
;;   tracked in `claude-repl--active-pr-polls' so they can be cancelled
;;   cleanly if the workspace is otherwise torn down.

;;; Code:

(require 'cl-lib)

(defvar claude-repl--merge-handler-registry nil
  "Alist mapping handler symbol → handler function.
Each function is called with `(TARGET-WS &optional ARGS)' where
TARGET-WS is the bare workspace name and ARGS is the optional plist
read from the repo's `.claude/emacs/workspace-merge.eld' file (or
declared in `claude-repl-workspace-merge-handler-overrides').

Entries are registered with
`claude-repl--register-merge-handler'.  The `cherry-pick' handler is
seeded by this file; additional handlers (e.g. `create-pr', `noop')
may register themselves in their own files.")

(defun claude-repl--register-merge-handler (symbol fn)
  "Register FN under SYMBOL in `claude-repl--merge-handler-registry'.
Replaces any prior binding for SYMBOL so reloads pick up the new
definition without leaving stale function references behind."
  (setq claude-repl--merge-handler-registry
        (cons (cons symbol fn)
              (assq-delete-all symbol claude-repl--merge-handler-registry))))

(defcustom claude-repl-workspace-merge-handler-overrides nil
  "User-side fallback merge handler config, keyed by repo root path.
Each entry is `(REPO-ROOT . CONFIG)' where CONFIG is an alist with
keys `handler' (registered symbol) and optional `args' (plist passed
to the handler).  Path matching is canonical (`file-truename' +
`directory-file-name'), so trailing slashes and tilde expansion are
normalised.

Consulted only when the repo itself does not provide
`.claude/emacs/workspace-merge.eld' — the repo-local file always wins.

Empty by default: every repo falls through to the `cherry-pick'
default unless it opts into a different handler via its repo-local
`.claude/emacs/workspace-merge.eld' or an entry added here.  The
`~/workspace/ChessCom/explanation-engine' repo previously routed to
`refresh-master-from-origin' here, but now merges via the cherry-pick
default like every other repo."
  :type '(alist :key-type directory
                :value-type (alist :key-type symbol :value-type sexp))
  :group 'claude-repl)

(defconst claude-repl--merge-handler-config-file
  ".claude/emacs/workspace-merge.eld"
  "Repo-relative path that declares a workspace-merge handler.
Format: a single alist sexp like
  ((handler . create-pr)
   (args . (:add-to-merge-queue t :skip-tests t))).
Read with `read', never `eval'.")

(defun claude-repl--read-merge-handler-config-file (repo-root)
  "Return the parsed handler-config alist from REPO-ROOT, or nil.
REPO-ROOT is a directory; the file
`claude-repl--merge-handler-config-file' is read from inside it.

Safe by construction: uses `read', not `eval'.  Returns nil on
missing file, IO error, or non-alist content (each case logged so
misconfigurations are debuggable)."
  (when (and repo-root (file-directory-p repo-root))
    (let ((path (expand-file-name claude-repl--merge-handler-config-file
                                  repo-root)))
      (when (file-readable-p path)
        (condition-case err
            (with-temp-buffer
              (insert-file-contents path)
              (goto-char (point-min))
              (let ((data (read (current-buffer))))
                (cond
                 ((consp data) data)
                 (t
                  (when (fboundp 'claude-repl--log)
                    (claude-repl--log
                     nil
                     "merge-handler-config: %s: not an alist (%S), ignoring"
                     path data))
                  nil))))
          (error
           (when (fboundp 'claude-repl--log)
             (claude-repl--log
              nil
              "merge-handler-config: failed to read %s: %S"
              path err))
           nil))))))

(defun claude-repl--lookup-merge-handler-override (repo-root)
  "Return the override config alist for REPO-ROOT, or nil.
Walks `claude-repl-workspace-merge-handler-overrides' and matches
entries by canonical path (`claude-repl--path-canonical')."
  (when repo-root
    (let ((canon (claude-repl--path-canonical repo-root)))
      (cl-loop for (root . config)
               in claude-repl-workspace-merge-handler-overrides
               when (and root (stringp root)
                         (string= canon
                                  (claude-repl--path-canonical root)))
               return config))))

(defun claude-repl--resolve-merge-handler (repo-root)
  "Resolve REPO-ROOT to a `(SYMBOL . ARGS)' handler descriptor.

Lookup order:
  1. REPO-ROOT's `.claude/emacs/workspace-merge.eld' file.
  2. `claude-repl-workspace-merge-handler-overrides' user alist.
  3. Fallback to `cherry-pick'.

If a config names an unknown handler symbol, falls back to
`cherry-pick' with a logged warning so a typo in the data file
cannot wedge merge dispatch."
  (let* ((config (or (claude-repl--read-merge-handler-config-file repo-root)
                     (claude-repl--lookup-merge-handler-override repo-root)))
         (raw-symbol (and (consp config) (alist-get 'handler config)))
         (args (and (consp config) (alist-get 'args config)))
         (known (and raw-symbol
                     (assq raw-symbol
                           claude-repl--merge-handler-registry)))
         (symbol (cond
                  (known raw-symbol)
                  (raw-symbol
                   (when (fboundp 'claude-repl--log)
                     (claude-repl--log
                      nil
                      "resolve-merge-handler: unknown handler %S for repo=%s — falling back to cherry-pick"
                      raw-symbol repo-root))
                   'cherry-pick)
                  (t 'cherry-pick))))
    (cons symbol args)))

(defun claude-repl--dispatch-merge-handler (target-ws repo-root)
  "Resolve and invoke the merge handler for TARGET-WS.
REPO-ROOT is the directory used to locate the repo-local handler
config; it is the workspace's `:source-ws-dir' when recorded, else
its `:project-dir'.  Either is typically a sibling worktree path
\(e.g. `~/workspace/<repo>-worktrees/<branch>'), so before resolution
REPO-ROOT is normalised to the repo's MAIN worktree via
`claude-repl--main-worktree-path'.  Without that step:
  - The `.eld' lookup reads from inside the sibling worktree, where
    the file is present only when the branch's tree happens to carry
    it.  A branch cut from master before the file landed would have
    no `.eld', silently falling through to the default cherry-pick.
  - The defcustom override is keyed by canonical main-repo path
    \(e.g. `~/workspace/ChessCom/explanation-engine'), so a sibling
    worktree path never matches, also falling through to cherry-pick.

Normalising to the main worktree at dispatch makes both lookups
keyed on the stable repo root regardless of which worktree the
workspace was created from.  When `--main-worktree-path' returns
nil (git unavailable, REPO-ROOT not inside a repo), falls back to
the caller-supplied REPO-ROOT as-is so the legacy behaviour
remains the safety net.

Logs both the caller-supplied REPO-ROOT and the resolved root so
failure modes are easy to trace.  Signals `user-error' if the
resolved symbol has no entry in the registry — defensive: the
resolver guarantees a valid symbol, but an unloaded handler file
could leave the registry short an entry."
  (let* ((resolved-root (or (and repo-root
                                 (claude-repl--main-worktree-path repo-root))
                            repo-root))
         (descriptor (claude-repl--resolve-merge-handler resolved-root))
         (symbol (car descriptor))
         (args (cdr descriptor))
         (entry (assq symbol claude-repl--merge-handler-registry))
         (fn (and entry (cdr entry))))
    (when (fboundp 'claude-repl--log)
      (claude-repl--log target-ws
                        "dispatch-merge-handler: ws=%s repo-root=%s resolved-root=%s handler=%S args=%S"
                        target-ws (or repo-root "nil")
                        (or resolved-root "nil") symbol args))
    (unless fn
      (user-error "No merge handler registered for symbol '%s'" symbol))
    (funcall fn target-ws args)))

;;; Built-in handlers

(defun claude-repl--merge-handler-cherry-pick (target-ws &optional _args)
  "Cherry-pick TARGET-WS's commits into its source workspace.
Default handler — wraps `claude-repl--workspace-merge-into-source'
with the same silent + auto-resolve semantics that
`claude-repl--handle-merge-command' has always used for skill-invoked
merges.  Ignores ARGS (none defined for this handler)."
  (claude-repl--workspace-merge-into-source target-ws t t))

(claude-repl--register-merge-handler 'cherry-pick
                                     #'claude-repl--merge-handler-cherry-pick)

;;; PR merge-queue polling

(defcustom claude-repl-pr-poll-interval 60
  "Seconds between successive `gh pr view' polls when waiting for a PR
to exit the merge queue.  Each tick spawns an async subprocess so the
main thread is never blocked between polls."
  :type 'integer
  :group 'claude-repl)

(defvar claude-repl--active-pr-polls (make-hash-table :test 'equal)
  "Hash table of workspace-name → active poll timer.
Populated by `claude-repl--pr-poll-start'; entries are removed by
`claude-repl--pr-poll-cancel' when a terminal PR state is reached.")

(defun claude-repl--pr-poll-cancel (ws)
  "Cancel and remove any active poll timer registered for workspace WS.
Safe to call when no poll is active — no-ops silently."
  (when-let ((timer (gethash ws claude-repl--active-pr-polls)))
    (cancel-timer timer)
    (remhash ws claude-repl--active-pr-polls)
    (claude-repl--log ws "pr-poll-cancel: ws=%s timer cancelled" ws)))

(defun claude-repl--pr-poll-start (ws project-dir main-dir)
  "Start async PR polling for workspace WS.
Fires an immediate first tick, then repeats every
`claude-repl-pr-poll-interval' seconds.  PROJECT-DIR is the worktree
used to invoke `gh'; MAIN-DIR is the main worktree used for the
eventual fetch+ff+checkout on merge success.

Must be called on the main thread (uses `run-with-timer' and
`make-process').  Cancels any pre-existing poll for WS before
registering the new one."
  (claude-repl--pr-poll-cancel ws)
  (claude-repl--log ws "pr-poll-start: ws=%s project-dir=%s main-dir=%s"
                    ws project-dir (or main-dir "nil"))
  ;; Fire immediately so the caller gets a fast result when the PR has
  ;; already merged (e.g. race between skill invocation and landing).
  (claude-repl--pr-poll-tick ws project-dir main-dir)
  (let ((timer (run-with-timer
                claude-repl-pr-poll-interval
                claude-repl-pr-poll-interval
                #'claude-repl--pr-poll-tick
                ws project-dir main-dir)))
    (puthash ws timer claude-repl--active-pr-polls)))

(defun claude-repl--pr-poll-tick (ws project-dir main-dir)
  "One poll iteration: asynchronously query the PR state for WS.
Spawns `gh pr view --json state,mergedAt,number' via
`claude-repl--async-gh' in PROJECT-DIR.  MAIN-DIR is forwarded to the
callback so `claude-repl--pr-poll-handle-result' can pass it to the
on-merged handler.  Runs on the main thread (timer callbacks and the
direct call from `claude-repl--pr-poll-start'); non-blocking."
  (claude-repl--log ws "pr-poll-tick: ws=%s project-dir=%s" ws project-dir)
  (claude-repl--async-gh
   (format "pr-poll-%s" ws)
   project-dir
   '("pr" "view" "--json" "state,mergedAt,number")
   (lambda (_ok output)
     (claude-repl--pr-poll-handle-result ws project-dir main-dir output))))

(defun claude-repl--pr-poll-handle-result (ws project-dir main-dir output)
  "Interpret gh OUTPUT for WS and act on the PR state.
Called on the main thread from the process sentinel.

  MERGED  → cancel poll, fetch+ff+checkout+close via
            `claude-repl--pr-poll-on-merged'.
  CLOSED  → cancel poll, revive workspace via
            `claude-repl--pr-poll-on-failed'.
  OPEN    → log and do nothing; next timer tick will poll again.
  error   → log the raw output and keep polling; transient gh
            failures (network hiccup, rate-limit) should not abort
            the loop."
  (let* ((json (condition-case err
                   (json-parse-string output
                                      :object-type 'alist
                                      :false-object nil
                                      :null-object nil)
                 (error
                  (claude-repl--log ws
                                    "pr-poll-handle-result: ws=%s JSON parse error %S — output=%S"
                                    ws err output)
                  nil)))
         (state (and json (alist-get 'state json))))
    (claude-repl--log ws "pr-poll-handle-result: ws=%s state=%s"
                      ws (or state "nil/error"))
    (cond
     ((null json)
      ;; gh returned non-JSON (error message, network failure, etc.).
      ;; If the PR number was not found at all, gh exits non-zero and
      ;; `finished' is replaced with `exited abnormally'; but the
      ;; sentinel only fires here on clean exit.  Log and keep polling.
      (claude-repl--log ws
                        "pr-poll-handle-result: ws=%s could not parse output — continuing to poll"
                        ws))
     ((equal state "MERGED")
      (claude-repl--log ws
                        "pr-poll-handle-result: ws=%s PR merged — advancing master"
                        ws)
      (claude-repl--pr-poll-cancel ws)
      (claude-repl--pr-poll-on-merged ws main-dir))
     ((equal state "CLOSED")
      (claude-repl--log ws
                        "pr-poll-handle-result: ws=%s PR closed without merging — reviving workspace"
                        ws)
      (claude-repl--pr-poll-cancel ws)
      (claude-repl--pr-poll-on-failed ws))
     (t
      (claude-repl--log ws
                        "pr-poll-handle-result: ws=%s PR state=%s — still open, polling continues"
                        ws state)))))

(defun claude-repl--pr-poll-on-merged (ws main-dir)
  "PR for WS has merged: fetch origin, fast-forward master, close workspace.
MAIN-DIR is the main worktree path; git work is skipped (with a log
line) when it is nil.  The workspace state and UI teardown mirror the
success path of `claude-repl--merge-handler-refresh-master-from-origin'.

Runs on the main thread (called from the process sentinel)."
  (when main-dir
    (let ((ec (claude-repl--git-exit-code
               main-dir "fetch" "origin"
               claude-repl-master-branch-name)))
      (claude-repl--log ws "pr-poll-on-merged: ws=%s fetch origin %s exit=%d"
                        ws claude-repl-master-branch-name ec))
    (claude-repl--maybe-fast-forward-master main-dir)
    (claude-repl--checkout-master-in-worktree main-dir))
  (claude-repl--ws-put ws :merging nil)
  (claude-repl--ws-put ws :merge-completed t)
  (claude-repl--ws-put ws :merge-completed-at (float-time))
  (claude-repl--ws-put ws :merge-failed nil)
  (claude-repl--ws-put ws :merge-target-name
                       (or (and main-dir
                                (claude-repl--git-branch-of-dir main-dir))
                           claude-repl-master-branch-name))
  (when (fboundp 'claude-repl--events-record)
    (claude-repl--events-record ws :merge))
  (claude-repl--ws-put ws :repl-state :merged)
  (claude-repl--ws-put ws :claude-state nil)
  (claude-repl--gns-sockets-close-then
   ws
   (lambda ()
     (claude-repl--close-workspace ws 'preserve-entry)
     (when main-dir
       (claude-repl--refresh-magit-status-for-dir main-dir ws)))))

(defun claude-repl--pr-poll-on-failed (ws)
  "PR for WS closed without merging: mark merge-failed and revive workspace.
Sets `:repl-state :merge-failed' then calls
`claude-repl--reopen-workspace-from-state' to restore the workspace UI
\(requires the plist entry to still carry `:project-dir', which
`workspace-merge-async' preserves via `preserve-entry').

Runs on the main thread (called from the process sentinel)."
  (claude-repl--ws-put ws :merging nil)
  (claude-repl--ws-put ws :merge-completed nil)
  (claude-repl--ws-put ws :merge-failed t)
  (claude-repl--ws-put ws :repl-state :merge-failed)
  (claude-repl--reopen-workspace-from-state ws)
  (claude-repl--dispatch-prompt-command
   ws
   (format
    "The pull request for workspace '%s' was closed without merging into master. \
The workspace has been revived — please investigate and retry the merge when ready."
    ws)))

(defun claude-repl--merge-handler-refresh-master-from-origin
    (target-ws &optional _args)
  "Poll for TARGET-WS's PR to merge, then refresh master from origin.

Handler for repos whose `/workspace-merge' contract is \"the PR is in
the merge queue; poll until it lands, then bring the local master
worktree up to date with origin\" — as opposed to the cherry-pick
default.  A repo opts into this via its repo-local
`.claude/emacs/workspace-merge.eld' (or an entry in
`claude-repl-workspace-merge-handler-overrides').

Steps:
  1. Resolve source-dir (`:project-dir' or `:source-ws-dir') and the
     MAIN worktree path via `claude-repl--main-worktree-path'.
  2. SIGNAL `user-error' if the main worktree has uncommitted changes
     (`claude-repl--worktree-dirty-p').  Advancing master while dirty
     work sits in the trunk checkout is ambiguous; the merge-async
     failure path re-enqueues with `:halt-until-human t'.
  3. Defer `claude-repl--pr-poll-start' to the main thread.  Polling
     is driven by repeating async subprocesses (`make-process') so the
     main thread is never blocked between ticks.  When a terminal PR
     state is reached the sentinel calls either
     `claude-repl--pr-poll-on-merged' (fetch+ff+close) or
     `claude-repl--pr-poll-on-failed' (revive workspace).

When source-dir cannot be resolved, logs and no-ops (same as the
previous immediate-fetch behaviour for that edge case).

ARGS is currently unused; reserved for future tuning.

SIGNALS `user-error' on a dirty main worktree (step 2)."
  (let* ((source-dir (or (claude-repl--ws-get target-ws :project-dir)
                         (claude-repl--ws-get target-ws :source-ws-dir)))
         (main-dir (and source-dir
                        (claude-repl--main-worktree-path source-dir))))
    (claude-repl--log target-ws
                      "merge-handler-refresh-master-from-origin: ws=%s source-dir=%s main-dir=%s"
                      target-ws (or source-dir "nil") (or main-dir "nil"))
    (cond
     ((not source-dir)
      (claude-repl--log target-ws
                        "merge-handler-refresh-master-from-origin: ws=%s no source-dir — skipping poll"
                        target-ws))
     ((and main-dir (claude-repl--worktree-dirty-p main-dir))
      (claude-repl--log target-ws
                        "merge-handler-refresh-master-from-origin: ws=%s main worktree %s is dirty — signaling error"
                        target-ws main-dir)
      (user-error
       "Cannot advance master from origin: main worktree '%s' has uncommitted changes — stash or commit first"
       main-dir))
     (t
      ;; Defer poll start to the main thread — `make-process' and
      ;; `run-with-timer' must run on main.  This handler is invoked
      ;; from the merge worker thread.
      (claude-repl--defer-to-main-thread
       (lambda ()
         (claude-repl--pr-poll-start target-ws source-dir main-dir)))))))

(claude-repl--register-merge-handler
 'refresh-master-from-origin
 #'claude-repl--merge-handler-refresh-master-from-origin)

(provide 'merge-handlers)

;;; merge-handlers.el ends here
