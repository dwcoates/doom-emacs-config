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

(defun claude-repl--merge-handler-refresh-master-from-origin
    (target-ws &optional _args)
  "Refresh master from origin and ensure the main worktree is on master.

Handler for repos whose `/workspace-merge' contract is \"the PR has
already landed via merge queue, just bring the local master worktree
up to date with origin and leave the main worktree checked out to
master\" — opposite of the cherry-pick default.  A repo opts into this
via its repo-local `.claude/emacs/workspace-merge.eld' (or an entry in
`claude-repl-workspace-merge-handler-overrides').

Steps:
  1. Resolve the MAIN worktree path of TARGET-WS's repo via
     `claude-repl--main-worktree-path' (the original clone — the
     worktree whose `.git' is a directory, NOT a sibling worktree
     added via `git worktree add'), starting from the workspace's
     own `:project-dir' or `:source-ws-dir'.  Skip the git work
     with a log line when neither can be resolved.
  2. SIGNAL `user-error' if the main worktree has uncommitted
     changes (`claude-repl--worktree-dirty-p').  Advancing master
     while the user has dirty work in the trunk checkout would risk
     ambiguity; the merge-async failure path re-enqueues the source
     workspace with `:halt-until-human t' so the user can resolve
     the dirty state before further merges proceed.
  3. Run `git fetch origin <master-branch-name>' in the main
     worktree synchronously — this thread runs on the merge worker,
     so blocking git here does not freeze the main UI thread.
  4. Hand off to `claude-repl--maybe-fast-forward-master', which
     advances the local <master> ref (via `merge --ff-only' on
     whatever worktree is on master, else `update-ref').  No-ops
     on diverged history, equal HEADs, or missing refs.
  5. Hand off to `claude-repl--checkout-master-in-worktree' against
     the main worktree so it ends checked out to the freshly-advanced
     <master>.  No-op when already on master, no-op-with-log when
     another worktree currently holds master (and checkout therefore
     refuses).

After the git work, marks TARGET-WS `:merge-completed t' /
`:repl-state :merged' so the drawer renders the 🔀 badge, then defers
`claude-repl--gns-sockets-close-then' + `claude-repl--close-workspace'
to the main thread so the workspace UI tears down cleanly — same
teardown chain the cherry-pick handler uses on success.

ARGS is currently unused; reserved for future tuning.

SIGNALS `user-error' on a dirty main worktree (step 2).
Deliberately does NOT signal on the other git failures (fetch
hiccup, diverged history, missing local master, checkout refused
because another worktree holds master): the PR has already landed
upstream, so the workspace's job is done regardless of whether the
local mirror could be advanced this run."
  (let* ((source-dir (or (claude-repl--ws-get target-ws :project-dir)
                         (claude-repl--ws-get target-ws :source-ws-dir)))
         (main-dir (and source-dir
                        (claude-repl--main-worktree-path source-dir))))
    (claude-repl--log target-ws
                      "merge-handler-refresh-master-from-origin: ws=%s source-dir=%s main-dir=%s"
                      target-ws (or source-dir "nil") (or main-dir "nil"))
    (cond
     ((not main-dir)
      (claude-repl--log target-ws
                        "merge-handler-refresh-master-from-origin: ws=%s no main worktree resolvable — skipping"
                        target-ws))
     ((claude-repl--worktree-dirty-p main-dir)
      (claude-repl--log target-ws
                        "merge-handler-refresh-master-from-origin: ws=%s main worktree %s is dirty — signaling error"
                        target-ws main-dir)
      (user-error
       "Cannot advance master from origin: main worktree '%s' has uncommitted changes — stash or commit first"
       main-dir))
     (t
      (let ((ec (claude-repl--git-exit-code
                 main-dir "fetch" "origin"
                 claude-repl-master-branch-name)))
        (claude-repl--log target-ws
                          "merge-handler-refresh-master-from-origin: ws=%s fetch origin %s exit=%d"
                          target-ws claude-repl-master-branch-name ec))
      (claude-repl--maybe-fast-forward-master main-dir)
      (claude-repl--checkout-master-in-worktree main-dir)))
    ;; Mark merged for the non-error branches (no main-dir / clean +
    ;; fetched).  The dirty-main case never reaches here because the
    ;; `user-error' above propagates out of the worker thread and into
    ;; `--workspace-merge-async''s centralized failure path, which
    ;; re-enqueues the source workspace with `:halt-until-human t'.
    (claude-repl--ws-put target-ws :merging nil)
    (claude-repl--ws-put target-ws :merge-completed t)
    (claude-repl--ws-put target-ws :merge-completed-at (float-time))
    (claude-repl--ws-put target-ws :merge-failed nil)
    ;; Record the destination branch (master, possibly via its main
    ;; worktree) so the drawer's MERGED-section folded detail can show
    ;; what this workspace merged into.
    (claude-repl--ws-put target-ws :merge-target-name
                         (or (and main-dir
                                  (claude-repl--git-branch-of-dir main-dir))
                             claude-repl-master-branch-name))
    (when (fboundp 'claude-repl--events-record)
      (claude-repl--events-record target-ws :merge))
    (claude-repl--ws-put target-ws :repl-state :merged)
    (claude-repl--ws-put target-ws :claude-state nil)
    ;; Defer the UI teardown to the main thread — this handler runs on
    ;; the merge worker thread spawned by
    ;; `claude-repl--workspace-merge-async', and persp/vterm kills must
    ;; happen on main.  Once the close is done, refresh any open
    ;; magit-status buffer for MAIN-DIR so it reflects the post-ff
    ;; state — magit's own auto-revert may have last fired before the
    ;; `git fetch' + `merge --ff-only' + checkout completed, leaving
    ;; the buffer stuck on the pre-ff HEAD.
    (claude-repl--defer-to-main-thread
     (lambda ()
       (claude-repl--gns-sockets-close-then
        target-ws
        (lambda ()
          (claude-repl--close-workspace target-ws 'preserve-entry)
          (when main-dir
            (claude-repl--refresh-magit-status-for-dir
             main-dir target-ws))))))))

(claude-repl--register-merge-handler
 'refresh-master-from-origin
 #'claude-repl--merge-handler-refresh-master-from-origin)

(provide 'merge-handlers)

;;; merge-handlers.el ends here
