;;; merge-handlers.el --- Repo-routed /create-or-update-workspace merge dispatch -*- lexical-binding: t; -*-

;;; Commentary:

;; Pluggable post-processing for `/create-or-update-workspace merge' command-file
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
;;   2. `agent-repl-workspace-merge-handler-overrides' — user-side
;;      defcustom alist keyed by canonical repo-root path.  Acts as the
;;      fallback for repos that haven't opted in via the .eld file.
;;   3. `cherry-pick' — the default handler, preserving the pre-routing
;;      behaviour of `agent-repl--handle-merge-command'.
;;
;; Each handler function has signature `(TARGET-WS &optional ARGS)' and
;; is responsible for performing the post-merge work and recording
;; terminal state via the shared helpers in worktree.el
;; (`--mark-merge-failed', `--close-workspace', etc.).
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
;;   tracked in `agent-repl--active-pr-polls' so they can be cancelled
;;   cleanly if the workspace is otherwise torn down.

;;; Code:

(require 'cl-lib)

;; Geometry + transport helpers used by the daemon-routed cherry-pick path.
;; Defined in worktree.el / frontend-uds.el; resolved at call time (all part
;; of the same module).
(declare-function agent-repl--merge-target-dir-for-ws "worktree" (source-ws))
(declare-function agent-repl--workspace-branch "worktree" (ws))
(declare-function agent-repl--cherry-pick-base "worktree" (root target-branch))
(declare-function agent-repl--uds-send-command "frontend-uds"
                  (field payload &optional workspace process))
(declare-function agent-repl--frontend-ws-command-key "frontend-client" (ws))
(declare-function agent-repl--uds-track-command "frontend-uds"
                  (request-id field workspace
                              &optional on-failure on-success on-challenge))
(declare-function agent-repl--host-action-defer "workspace-create-client"
                  (token))
(declare-function agent-repl--host-action-settle "workspace-create-client"
                  (token ok error-text))

(defvar agent-repl--merge-handler-registry nil
  "Alist mapping handler symbol → handler function.
Each function is called with `(TARGET-WS &optional ARGS)' where
TARGET-WS is the bare workspace name and ARGS is the optional plist
read from the repo's `.claude/emacs/workspace-merge.eld' file (or
declared in `agent-repl-workspace-merge-handler-overrides').

Entries are registered with
`agent-repl--register-merge-handler'.  The `cherry-pick' handler is
seeded by this file; additional handlers (e.g. `create-pr', `noop')
may register themselves in their own files.")

(defun agent-repl--register-merge-handler (symbol fn)
  "Register FN under SYMBOL in `agent-repl--merge-handler-registry'.
Replaces any prior binding for SYMBOL so reloads pick up the new
definition without leaving stale function references behind."
  (let ((replaced (assq symbol agent-repl--merge-handler-registry)))
    (setq agent-repl--merge-handler-registry
          (cons (cons symbol fn)
                (assq-delete-all symbol agent-repl--merge-handler-registry)))
    (when (fboundp 'agent-repl--log)
      (agent-repl--log nil
                        "register-merge-handler: symbol=%S fn=%S replaced=%s"
                        symbol fn (if replaced "t" "nil")))))

(defcustom agent-repl-workspace-merge-handler-overrides nil
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
  :group 'agent-repl)

(defconst agent-repl--merge-handler-config-file
  ".claude/emacs/workspace-merge.eld"
  "Repo-relative path that declares a workspace-merge handler.
Format: a single alist sexp like
  ((handler . create-pr)
   (args . (:add-to-merge-queue t))).
Read with `read', never `eval'.")

(defun agent-repl--read-merge-handler-config-file (repo-root)
  "Return the parsed handler-config alist from REPO-ROOT, or nil.
REPO-ROOT is a directory; the file
`agent-repl--merge-handler-config-file' is read from inside it.

Safe by construction: uses `read', not `eval'.  Returns nil on
missing file, IO error, or non-alist content (each case logged so
misconfigurations are debuggable)."
  (cond
   ((null repo-root)
    (when (fboundp 'agent-repl--log)
      (agent-repl--log nil
                        "merge-handler-config: repo-root=nil; config lookup skipped")))
   ((not (file-directory-p repo-root))
    (when (fboundp 'agent-repl--log)
      (agent-repl--log nil
                        "merge-handler-config: repo-root=%s is not a directory; config lookup skipped"
                        repo-root)))
   (t
    (let ((path (expand-file-name agent-repl--merge-handler-config-file
                                  repo-root)))
      (if (not (file-readable-p path))
          (when (fboundp 'agent-repl--log)
            (agent-repl--log nil
                              "merge-handler-config: path=%s unreadable-or-absent"
                              path))
        (condition-case err
            (with-temp-buffer
              (insert-file-contents path)
              (goto-char (point-min))
              (let ((data (read (current-buffer))))
                (cond
                 ((consp data) data)
                 (t
                  (when (fboundp 'agent-repl--log)
                    (agent-repl--log
                     nil
                     "merge-handler-config: %s: not an alist (%S), ignoring"
                     path data))
                  nil))))
          (error
           (when (fboundp 'agent-repl--log)
             (agent-repl--log
              nil
              "merge-handler-config: failed to read %s: %S"
              path err))
           nil)))))))

(defun agent-repl--lookup-merge-handler-override (repo-root)
  "Return the override config alist for REPO-ROOT, or nil.
Walks `agent-repl-workspace-merge-handler-overrides' and matches
entries by canonical path (`agent-repl--path-canonical')."
  (when repo-root
    (let ((canon (agent-repl--path-canonical repo-root)))
      (cl-loop for (root . config)
               in agent-repl-workspace-merge-handler-overrides
               when (and root (stringp root)
                         (string= canon
                                  (agent-repl--path-canonical root)))
               return config))))

(defun agent-repl--resolve-merge-handler (repo-root)
  "Resolve REPO-ROOT to a `(SYMBOL . ARGS)' handler descriptor.

Lookup order:
  1. REPO-ROOT's `.claude/emacs/workspace-merge.eld' file.
  2. `agent-repl-workspace-merge-handler-overrides' user alist.
  3. Fallback to `cherry-pick'.

If a config names an unknown handler symbol, falls back to
`cherry-pick' with a logged warning so a typo in the data file
cannot wedge merge dispatch."
  (let* ((config (or (agent-repl--read-merge-handler-config-file repo-root)
                     (agent-repl--lookup-merge-handler-override repo-root)))
         (raw-symbol (and (consp config) (alist-get 'handler config)))
         (args (and (consp config) (alist-get 'args config)))
         (known (and raw-symbol
                     (assq raw-symbol
                           agent-repl--merge-handler-registry)))
         (symbol (cond
                  (known raw-symbol)
                  (raw-symbol
                   (when (fboundp 'agent-repl--log)
                     (agent-repl--log
                      nil
                      "resolve-merge-handler: unknown handler %S for repo=%s — falling back to cherry-pick"
                      raw-symbol repo-root))
                   'cherry-pick)
                  (t 'cherry-pick))))
    (when (fboundp 'agent-repl--log)
      (agent-repl--log nil
                        "resolve-merge-handler: repo-root=%s config-present=%s raw-handler=%S resolved-handler=%S args-present=%s"
                        (or repo-root "nil") (if config "t" "nil") raw-symbol
                        symbol (if args "t" "nil")))
    (cons symbol args)))

(defun agent-repl--dispatch-merge-handler (target-ws repo-root &optional onto-master)
  "Resolve and invoke the merge handler for TARGET-WS.

When ONTO-MASTER is non-nil, the repo-routed resolution is BYPASSED and
the `onto-master' handler is used unconditionally.  This is the
`/create-or-update-workspace merge --onto-master' path: the
workspace's PR has already merged into `origin/master', so the
per-invocation intent (advance the local trunk, do not cherry-pick)
MUST win over the repo's checked-in `.eld' handler.

REPO-ROOT is the directory used to locate the repo-local handler
config; it is the workspace's `:source-ws-dir' when recorded, else
its `:project-dir'.  Either is typically a sibling worktree path
\(e.g. `~/workspace/<repo>-worktrees/<branch>'), so before resolution
REPO-ROOT is normalised to the repo's MAIN worktree via
`agent-repl--main-worktree-path'.  Without that step:
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
failure modes are easy to trace.

The `cherry-pick' resolution is DAEMON-ROUTED (design §4.6/§9.3): rather
than invoking a local handler, it sends a `mergeWorkspace' command over
the frontend UDS via `agent-repl--merge-dispatch-cherry-pick-over-uds' —
the daemon runs the cherry-pick and publishes the merge state, which
Emacs reacts to via the pushed-state hook.  Every OTHER handler
\(onto-master / refresh-master-from-origin / …) is DAEMON-PORT PENDING and
still runs locally through the registry.  Signals `user-error' if a
non-cherry-pick resolved symbol has no registry entry — defensive: the
resolver guarantees a registered symbol, but a forced `onto-master' (or
an unloaded handler file) could leave the registry short an entry."
  (let ((symbol (agent-repl--resolve-merge-handler-symbol repo-root onto-master)))
    (when (fboundp 'agent-repl--log)
      (agent-repl--log target-ws
                        "dispatch-merge-handler: ws=%s repo-root=%s onto-master=%s handler=%S"
                        target-ws (or repo-root "nil")
                        (if onto-master "t" "nil") symbol))
    (if (eq symbol 'cherry-pick)
        ;; DAEMON-ROUTED: send the merge command; the daemon owns execution
        ;; and state.  No local git, no registry fn.
        (agent-repl--merge-dispatch-cherry-pick-over-uds target-ws)
      ;; DAEMON-PORT PENDING local handler — re-resolve for its ARGS.
      (let* ((resolved-root (or (and repo-root
                                     (agent-repl--main-worktree-path repo-root))
                                repo-root))
             (descriptor (if onto-master
                             (cons 'onto-master nil)
                           (agent-repl--resolve-merge-handler resolved-root)))
             (args (cdr descriptor))
             (entry (assq symbol agent-repl--merge-handler-registry))
             (fn (and entry (cdr entry))))
        (agent-repl--log target-ws
                          "dispatch-merge-handler: ws=%s resolved-root=%s handler=%S args-present=%s registry-entry=%s"
                          target-ws (or resolved-root "nil") symbol
                          (if args "t" "nil") (if entry "t" "nil"))
        (unless fn
          (agent-repl--log target-ws
                            "dispatch-merge-handler: ws=%s handler=%S has no registered function — aborting"
                            target-ws symbol)
          (user-error "No merge handler registered for symbol '%s'" symbol))
        (agent-repl--log target-ws
                          "dispatch-merge-handler: ws=%s invoking handler=%S"
                          target-ws symbol)
        (funcall fn target-ws args)))))

(defun agent-repl--resolve-merge-handler-symbol (repo-root &optional onto-master)
  "Resolve REPO-ROOT to the merge-handler SYMBOL (no args), honoring ONTO-MASTER.
Normalises REPO-ROOT to the repo's MAIN worktree before resolving (the
`.eld'/override lookups are keyed there — see `--dispatch-merge-handler').
When ONTO-MASTER is non-nil the symbol is unconditionally `onto-master'.
Shared by `agent-repl--workspace-merge-async' (to branch the daemon-routed
cherry-pick path off the local worker path) and
`agent-repl--dispatch-merge-handler'."
  (if onto-master
      (progn
        (when (fboundp 'agent-repl--log)
          (agent-repl--log nil
                            "resolve-merge-handler-symbol: repo-root=%s onto-master=t resolved-handler=onto-master"
                            (or repo-root "nil")))
        'onto-master)
    (let* ((main-root (and repo-root (agent-repl--main-worktree-path repo-root)))
           (resolved-root (or main-root repo-root))
           (symbol (car (agent-repl--resolve-merge-handler resolved-root))))
      (when (fboundp 'agent-repl--log)
        (agent-repl--log nil
                          "resolve-merge-handler-symbol: repo-root=%s main-root=%s resolved-root=%s handler=%S"
                          (or repo-root "nil") (or main-root "nil")
                          (or resolved-root "nil") symbol))
      symbol)))

;;; Daemon-routed cherry-pick dispatch (design §4.6/§9.3)
;;
;; The cherry-pick driver moved to the daemon (daemon/internal/workspace/
;; merge/).  Emacs no longer runs the merge; it owns the workspace->worktree
;; geometry (the daemon has none) and supplies it with the command, then
;; reacts to the pushed merge state (see the reactor + hook in worktree.el).

(defun agent-repl--merge-cherry-pick-geometry (ws)
  "Resolve WS's cherry-pick merge geometry as a plist.
Returns (:source-branch B :source-dir S :target-dir T).  SIGNALS
`user-error' (loud, after a log) when any component is missing/blank — the
daemon has no workspace->worktree map, so an incomplete geometry would be a
merge command it cannot act on (No-Silent-Fallbacks: never send a partial
geometry)."
  (let ((source-dir (agent-repl--ws-get ws :project-dir))
        (target-dir (agent-repl--merge-target-dir-for-ws ws))
        (source-branch (agent-repl--workspace-branch ws)))
    (when (or (null source-dir) (and (stringp source-dir) (string-empty-p source-dir)))
      (agent-repl--log ws "merge-cherry-pick-geometry: ws=%s MISSING source-dir — aborting" ws)
      (user-error "Cannot merge '%s': no source worktree (:project-dir) recorded" ws))
    (when (or (null target-dir) (and (stringp target-dir) (string-empty-p target-dir)))
      (agent-repl--log ws "merge-cherry-pick-geometry: ws=%s MISSING target-dir — aborting" ws)
      (user-error "Cannot merge '%s': could not resolve a merge target directory" ws))
    (when (or (null source-branch) (and (stringp source-branch) (string-empty-p source-branch)))
      (agent-repl--log ws "merge-cherry-pick-geometry: ws=%s MISSING source-branch — aborting" ws)
      (user-error "Cannot merge '%s': could not resolve the source branch" ws))
    (agent-repl--log ws
                      "merge-cherry-pick-geometry: ws=%s resolved source-branch=%s source-dir=%s target-dir=%s"
                      ws source-branch source-dir target-dir)
    (list :source-branch source-branch :source-dir source-dir :target-dir target-dir)))

(defun agent-repl--merge-dispatch-cherry-pick-over-uds (ws)
  "Send the daemon a `mergeWorkspace' cherry-pick command for WS.
Resolves the geometry (`agent-repl--merge-cherry-pick-geometry'), stashes it
plus the PRE-merge cherry-pick base (the reactive phone-home needs the
base..branch range before the daemon advances the target) on WS's plist,
marks WS `:daemon-merge-dispatched' so the pushed-state reactor knows this
Emacs initiated the merge, and sends + tracks the command.  A rejected
CommandAck surfaces loudly (frontend-uds) and its tracked on-failure
callback clears the dispatch marker.  Returns the request-id."
  (let* ((geom (agent-repl--merge-cherry-pick-geometry ws))
         (source-branch (plist-get geom :source-branch))
         (source-dir (plist-get geom :source-dir))
         (target-dir (plist-get geom :target-dir))
         (base (agent-repl--cherry-pick-base target-dir source-branch)))
    (agent-repl--ws-put ws :resolved-target-dir target-dir)
    (agent-repl--ws-put ws :merge-target-branch source-branch)
    (agent-repl--ws-put ws :merge-base base)
    (agent-repl--ws-put ws :daemon-merge-dispatched t)
    (agent-repl--log ws
                      "merge-dispatch-cherry-pick-over-uds: ws=%s source-branch=%s source-dir=%s target-dir=%s base=%s"
                      ws source-branch source-dir target-dir (or base "nil"))
    ;; ROUTED BY THE WORKSPACE COMMAND KEY, like every other command. Sending
    ;; the bare name here filed the daemon's merge state rows under a workspace
    ;; key nothing else used, so their WorkspaceState carried no connectivity
    ;; verdict and Emacs refused the frame — the merge landed and its workspace
    ;; was never torn down. The name now rides `:workspaceName' for the tag.
    (let ((req (agent-repl--uds-send-command
                "mergeWorkspace"
                (list :handler "cherry-pick"
                      :workspaceName ws
                      :sourceBranch source-branch
                      :sourceDir source-dir
                      :targetDir target-dir)
                (agent-repl--frontend-ws-command-key ws))))
      (agent-repl--log ws
                        "merge-dispatch-cherry-pick-over-uds: ws=%s command-issued request-id=%s dispatch-marker=t"
                        ws req)
      ;; THE HOST ACTION'S OUTCOME IS THIS COMMAND'S ACK, not this dispatch.
      ;; Returning here without deferring is what let the daemon record a merge
      ;; as `ok=true' 52ms before its own rejection arrived, so the failure that
      ;; killed every merge left no trace on the workspace at all.
      (agent-repl--host-action-defer req)
      (agent-repl--uds-track-command
       req "mergeWorkspace" ws
       (lambda (err)
         (agent-repl--ws-put ws :daemon-merge-dispatched nil)
         (agent-repl--host-action-settle req nil err)
         (agent-repl--log ws
                           "merge-dispatch-cherry-pick-over-uds: ws=%s command REJECTED err=%s — cleared dispatch marker"
                           ws err))
       (lambda ()
         (agent-repl--host-action-settle req t nil)
         (agent-repl--log ws
                           "merge-dispatch-cherry-pick-over-uds: ws=%s request-id=%s command ACKED — host action completed on the merge's own outcome"
                           ws req)))
      (agent-repl--log ws
                        "merge-dispatch-cherry-pick-over-uds: ws=%s request-id=%s tracking-registered"
                        ws req)
      req)))

(defun agent-repl--merge-resume-over-uds (ws)
  "Send the daemon a `mergeWorkspace' resume for WS (resolve-and-continue).
The design §9.3 handoff: after a human resolves the in-tree conflict the
daemon left, this sets `conflict_resolved_continue' so the daemon runs
`git add -u' + `cherry-pick --continue'.  Re-supplies the SAME geometry
\(the daemon is stateless per call) via `agent-repl--merge-cherry-pick-geometry',
which SIGNALS `user-error' if it cannot be resolved (No-Silent-Fallbacks).
Returns the request-id."
  (let* ((geom (agent-repl--merge-cherry-pick-geometry ws))
         (source-branch (plist-get geom :source-branch))
         (source-dir (plist-get geom :source-dir))
         (target-dir (plist-get geom :target-dir)))
    (agent-repl--log ws
                      "merge-resume-over-uds: ws=%s source-branch=%s source-dir=%s target-dir=%s"
                      ws source-branch source-dir target-dir)
    (let ((req (agent-repl--uds-send-command
                "mergeWorkspace"
                (list :handler "cherry-pick"
                      :conflictResolvedContinue t
                      :workspaceName ws
                      :sourceBranch source-branch
                      :sourceDir source-dir
                      :targetDir target-dir)
                (agent-repl--frontend-ws-command-key ws))))
      (agent-repl--uds-track-command req "mergeWorkspace" ws)
      (agent-repl--log ws
                        "merge-resume-over-uds: ws=%s command-issued request-id=%s tracking-registered"
                        ws req)
      req)))

;;; DAEMON-PORT PENDING: PR merge-queue polling
;;
;; The following handlers (refresh-master-from-origin, onto-master, and the
;; cee-agent reinstall-and-bounce) were deliberately NOT ported to the daemon
;; in this cutover.  They still run locally through the registry; only the
;; cherry-pick driver moved.  Do not delete — port later.

(defcustom agent-repl-pr-poll-interval 60
  "Seconds between successive `gh pr view' polls when waiting for a PR
to exit the merge queue.  Each tick spawns an async subprocess so the
main thread is never blocked between polls."
  :type 'integer
  :group 'agent-repl)

(defvar agent-repl--active-pr-polls (make-hash-table :test 'equal)
  "Hash table of workspace-name → active poll timer.
Populated by `agent-repl--pr-poll-start'; entries are removed by
`agent-repl--pr-poll-cancel' when a terminal PR state is reached.")

(defun agent-repl--pr-poll-cancel (ws)
  "Cancel and remove any active poll timer registered for workspace WS.
Safe to call when no poll is active — no-ops silently."
  (when-let ((timer (gethash ws agent-repl--active-pr-polls)))
    (cancel-timer timer)
    (remhash ws agent-repl--active-pr-polls)
    (agent-repl--log ws "pr-poll-cancel: ws=%s timer cancelled" ws)))

(defun agent-repl--pr-poll-start (ws project-dir main-dir)
  "Start async PR polling for workspace WS.
Fires an immediate first tick, then repeats every
`agent-repl-pr-poll-interval' seconds.  PROJECT-DIR is the worktree
used to invoke `gh'; MAIN-DIR is the main worktree used for the
eventual fetch+ff+checkout on merge success.

Must be called on the main thread (uses `run-with-timer' and
`make-process').  Cancels any pre-existing poll for WS before
registering the new one."
  (agent-repl--pr-poll-cancel ws)
  (agent-repl--log ws "pr-poll-start: ws=%s project-dir=%s main-dir=%s"
                    ws project-dir (or main-dir "nil"))
  ;; Fire immediately so the caller gets a fast result when the PR has
  ;; already merged (e.g. race between skill invocation and landing).
  (agent-repl--pr-poll-tick ws project-dir main-dir)
  (let ((timer (run-with-timer
                agent-repl-pr-poll-interval
                agent-repl-pr-poll-interval
                #'agent-repl--pr-poll-tick
                ws project-dir main-dir)))
    (puthash ws timer agent-repl--active-pr-polls)))

(defun agent-repl--pr-poll-tick (ws project-dir main-dir)
  "One poll iteration: asynchronously query the PR state for WS.
Spawns `gh pr view --json state,mergedAt,number' via
`agent-repl--async-gh' in PROJECT-DIR.  MAIN-DIR is forwarded to the
callback so `agent-repl--pr-poll-handle-result' can pass it to the
on-merged handler.  Runs on the main thread (timer callbacks and the
direct call from `agent-repl--pr-poll-start'); non-blocking."
  (agent-repl--log ws "pr-poll-tick: ws=%s project-dir=%s" ws project-dir)
  (agent-repl--async-gh
   (format "pr-poll-%s" ws)
   project-dir
   '("pr" "view" "--json" "state,mergedAt,number")
   (lambda (ok output)
     (agent-repl--log-verbose
      ws "pr-poll-tick: ws=%s gh-complete ok=%s output-bytes=%d"
      ws (if ok "t" "nil") (length (or output "")))
     (agent-repl--pr-poll-handle-result ws project-dir main-dir output))))

(defun agent-repl--pr-poll-handle-result (ws _project-dir main-dir output)
  "Interpret gh OUTPUT for WS and act on the PR state.
Called on the main thread from the process sentinel.

  MERGED  → cancel poll, fetch+ff+checkout+close via
            `agent-repl--pr-poll-on-merged'.
  CLOSED  → cancel poll, revive workspace via
            `agent-repl--pr-poll-on-failed'.
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
                  (agent-repl--log ws
                                    "pr-poll-handle-result: ws=%s JSON parse error %S — output=%S"
                                    ws err output)
                  nil)))
         (state (and json (alist-get 'state json))))
    (agent-repl--log ws "pr-poll-handle-result: ws=%s state=%s"
                      ws (or state "nil/error"))
    (cond
     ((null json)
      ;; gh returned non-JSON (error message, network failure, etc.).
      ;; If the PR number was not found at all, gh exits non-zero and
      ;; `finished' is replaced with `exited abnormally'; but the
      ;; sentinel only fires here on clean exit.  Log and keep polling.
      (agent-repl--log ws
                        "pr-poll-handle-result: ws=%s could not parse output — continuing to poll"
                        ws))
     ((equal state "MERGED")
      (agent-repl--log ws
                        "pr-poll-handle-result: ws=%s PR merged — advancing master"
                        ws)
      (agent-repl--pr-poll-cancel ws)
      (agent-repl--pr-poll-on-merged ws main-dir))
     ((equal state "CLOSED")
      (agent-repl--log ws
                        "pr-poll-handle-result: ws=%s PR closed without merging — reviving workspace"
                        ws)
      (agent-repl--pr-poll-cancel ws)
      (agent-repl--pr-poll-on-failed ws))
     (t
      (agent-repl--log ws
                        "pr-poll-handle-result: ws=%s PR state=%s — still open, polling continues"
                        ws state)))))

(defun agent-repl--finalize-merged-workspace (ws main-dir)
  "Record terminal :merged state for WS and tear its workspace down.
Shared teardown tail for every \"the trunk now carries this work\"
path — `agent-repl--pr-poll-on-merged' (PR-poll handler) and
`agent-repl--merge-handler-onto-master' (--onto-master handler) both
call it after they have advanced the local trunk.  MAIN-DIR is the
main worktree path (used for the magit refresh and the merge-target
name); teardown still runs when it is nil.

Runs on the main thread (UI ops: close-workspace + magit refresh)."
  (agent-repl--log ws
                    "finalize-merged-workspace: ws=%s main-dir=%s begin terminal-state transition"
                    ws (or main-dir "nil"))
  (agent-repl--ws-put ws :merging nil)
  (agent-repl--ws-put ws :merge-completed t)
  (agent-repl--ws-put ws :merge-completed-at (float-time))
  (agent-repl--ws-put ws :merge-failed nil)
  ;; Record WS on the receiving (main/master) workspace's merged-in
  ;; list (`:merged-in-workspaces', the historical record of merges it
  ;; received).  MAIN-DIR is the main worktree the trunk work landed
  ;; on; nil when git was skipped.
  (agent-repl--record-merged-in-workspace main-dir ws)
  (agent-repl--ws-put ws :merge-target-name
                       (or (and main-dir
                                (agent-repl--git-branch-of-dir main-dir))
                           agent-repl-master-branch-name))
  (agent-repl--ws-put ws :repl-state :merged)
  (agent-repl--ws-put ws :agent-state nil)
  (agent-repl--log ws
                    "finalize-merged-workspace: ws=%s teardown-scheduled main-dir=%s"
                    ws (or main-dir "nil"))
  (agent-repl--gns-sockets-close-then
   ws
   (lambda ()
     ;; Bound inside the lambda: runs async after the GNS-socket close
     ;; poll, outside the merge handler's dynamic extent.
     (let ((agent-repl--kill-cause "pr-merged teardown (merge-handlers, auto)"))
       (agent-repl--merge-close-workspace ws 'preserve-entry))
     (when main-dir
       (agent-repl--refresh-magit-status-for-dir main-dir ws)))))

(defun agent-repl--pr-poll-on-merged (ws main-dir)
  "PR for WS has merged: fetch origin, fast-forward master, close workspace.
MAIN-DIR is the main worktree path; git work is skipped (with a log
line) when it is nil.  The workspace state and UI teardown are recorded
via `agent-repl--finalize-merged-workspace'.

Runs on the main thread (called from the process sentinel)."
  (when main-dir
    (let ((ec (agent-repl--git-exit-code
               main-dir "fetch" "origin"
               agent-repl-master-branch-name)))
      (agent-repl--log ws "pr-poll-on-merged: ws=%s fetch origin %s exit=%d"
                        ws agent-repl-master-branch-name ec))
    (agent-repl--maybe-fast-forward-master main-dir)
    (agent-repl--log ws
                      "pr-poll-on-merged: ws=%s fetch-succeeded; requesting fast-forward and checkout in %s"
                      ws main-dir)
    (agent-repl--checkout-master-in-worktree main-dir))
  (unless main-dir
    (agent-repl--log ws
                      "pr-poll-on-merged: ws=%s main-dir=nil; finalizing without local trunk update"
                      ws))
  (agent-repl--log ws
                    "pr-poll-on-merged: ws=%s entering workspace finalization main-dir=%s"
                    ws (or main-dir "nil"))
  (agent-repl--finalize-merged-workspace ws main-dir))

(defun agent-repl--pr-poll-on-failed (ws)
  "PR for WS closed without merging: mark merge-failed and revive workspace.
Sets `:repl-state :merge-failed' then calls
`agent-repl--reopen-workspace-from-state' to restore the workspace UI
\(requires the plist entry to still carry `:project-dir', which
`workspace-merge-async' preserves via `preserve-entry').

Runs on the main thread (called from the process sentinel)."
  (agent-repl--log ws
                    "pr-poll-on-failed: ws=%s recording merge-failed state and reopening workspace"
                    ws)
  (agent-repl--ws-put ws :merging nil)
  (agent-repl--ws-put ws :merge-completed nil)
  (agent-repl--ws-put ws :merge-failed t)
  (agent-repl--ws-put ws :repl-state :merge-failed)
  (agent-repl--reopen-workspace-from-state ws)
  (agent-repl--dispatch-prompt-command
   ws
   (format
    "The pull request for workspace '%s' was closed without merging into master. \
The workspace has been revived — please investigate and retry the merge when ready."
    ws))
  (agent-repl--log ws
                    "pr-poll-on-failed: ws=%s workspace reopened and retry notification dispatched"
                    ws))

(defun agent-repl--merge-handler-refresh-master-from-origin
    (target-ws &optional _args)
  "Poll for TARGET-WS's PR to merge, then refresh master from origin.

Handler for repos whose `/create-or-update-workspace merge' contract
is \"the PR is in the merge queue; poll until it lands, then bring
the local master worktree up to date with origin\" — as opposed to
the cherry-pick default.  A repo opts into this via its repo-local
`.claude/emacs/workspace-merge.eld' (or an entry in
`agent-repl-workspace-merge-handler-overrides').

Steps:
  1. Resolve source-dir (`:project-dir' or `:source-ws-dir') and the
     MAIN worktree path via `agent-repl--main-worktree-path'.
  2. SIGNAL `user-error' if the main worktree has uncommitted changes
     (`agent-repl--worktree-dirty-p').  Advancing master while dirty
     work sits in the trunk checkout is ambiguous; the merge-async
     failure path re-enqueues with `:halt-until-human t'.
  3. Defer `agent-repl--pr-poll-start' to the main thread.  Polling
     is driven by repeating async subprocesses (`make-process') so the
     main thread is never blocked between ticks.  When a terminal PR
     state is reached the sentinel calls either
     `agent-repl--pr-poll-on-merged' (fetch+ff+close) or
     `agent-repl--pr-poll-on-failed' (revive workspace).

When source-dir cannot be resolved, logs and no-ops (same as the
previous immediate-fetch behaviour for that edge case).

ARGS is currently unused; reserved for future tuning.

SIGNALS `user-error' on a dirty main worktree (step 2)."
  (let* ((project-dir (agent-repl--ws-get target-ws :project-dir))
         (source-ws-dir (agent-repl--ws-get target-ws :source-ws-dir))
         (source-dir (or project-dir source-ws-dir))
         (main-dir (and source-dir
                        (agent-repl--main-worktree-path source-dir)))
         (source-kind (cond
                       (project-dir "project-dir")
                       (source-ws-dir "source-ws-dir")
                       (t "missing"))))
    (agent-repl--log target-ws
                      "merge-handler-refresh-master-from-origin: ws=%s source-kind=%s source-dir=%s main-dir=%s"
                      target-ws source-kind (or source-dir "nil") (or main-dir "nil"))
    (cond
     ((not source-dir)
      (agent-repl--log target-ws
                        "merge-handler-refresh-master-from-origin: ws=%s no source-dir — skipping poll"
                        target-ws))
     ((and main-dir (agent-repl--worktree-dirty-p main-dir))
      (agent-repl--log target-ws
                        "merge-handler-refresh-master-from-origin: ws=%s main worktree %s is dirty — signaling error"
                        target-ws main-dir)
      (user-error
       "Cannot advance master from origin: main worktree '%s' has uncommitted changes — stash or commit first"
       main-dir))
     (t
      ;; Defer poll start to the main thread — `make-process' and
      ;; `run-with-timer' must run on main.  This handler is invoked
      ;; from the merge worker thread.
      (agent-repl--defer-to-main-thread
       (lambda ()
         (agent-repl--log target-ws
                           "merge-handler-refresh-master-from-origin: ws=%s main-thread poll start source-dir=%s main-dir=%s"
                           target-ws source-dir (or main-dir "nil"))
         (agent-repl--pr-poll-start target-ws source-dir main-dir)))))))

(agent-repl--register-merge-handler
 'refresh-master-from-origin
 #'agent-repl--merge-handler-refresh-master-from-origin)

;;; DAEMON-PORT PENDING: cee-agent reinstall-and-bounce (onto-master post-advance hook)

(defconst agent-repl--cee-agent-dir-prefix "apps/cee-agent/"
  "Repo-relative directory whose changes trigger the reinstall-and-bounce.
A merge whose delta touches any path at or under this prefix runs
`agent-repl--cee-agent-reinstall-script' after the `onto-master'
handler advances the trunk.")

(defconst agent-repl--cee-agent-reinstall-script
  "apps/cee-agent/scripts/reinstall-and-bounce.sh"
  "Repo-relative path to the cee-agent reinstall-and-bounce script.
Run by `agent-repl--merge-handler-onto-master' after the trunk
fast-forwards, but only when the merged delta touches
`agent-repl--cee-agent-dir-prefix'.  May not yet exist on every
trunk; absence surfaces as a non-zero exit code that is logged,
never signaled.")

(defun agent-repl--merge-delta-touches-dir-p (main-dir from-ref to-ref dir-prefix)
  "Return non-nil if the FROM-REF..TO-REF delta in MAIN-DIR touches DIR-PREFIX.
Lists changed paths via `git -C MAIN-DIR diff --name-only FROM-REF
TO-REF' and returns non-nil when any path is at or under DIR-PREFIX
\(a repo-relative directory, with or without a trailing slash).

Read-only: queries refs, never mutates them.  Used by the
`onto-master' handler to decide whether the just-merged work warrants
the cee-agent reinstall-and-bounce, so it MUST be called BEFORE the
fast-forward — afterwards local trunk equals TO-REF and the diff is
empty."
  (let* ((prefix (file-name-as-directory dir-prefix))
         (out (agent-repl--git-string-quiet
               "-C" main-dir "diff" "--name-only" from-ref to-ref)))
    (and out
         (not (string-empty-p out))
         (cl-some (lambda (path) (string-prefix-p prefix path))
                  (split-string out "\n" t)))))

(defconst agent-repl--cee-agent-bounce-timeout-seconds 600
  "Seconds the cee-agent reinstall-and-bounce script may run.
The script rebuilds and restarts a service, so the budget is generous;
on expiry the child is killed and the wrapper returns the symbol
`timeout' (the caller logs it as a non-fatal failure, never reverts
the already-advanced trunk).")

(defun agent-repl--cee-agent-reinstall-and-bounce-exit-code (worktree)
  "Run the cee-agent reinstall-and-bounce script in WORKTREE; return its exit code.
WORKTREE is the MAIN worktree the service is installed and run out of
\(e.g. `~/workspace/ChessCom/explanation-engine'); the script path
`agent-repl--cee-agent-reinstall-script' is resolved relative to it
and run with WORKTREE as the working directory.

Returns the script's integer exit code, or the symbol `timeout' when
`agent-repl--cee-agent-bounce-timeout-seconds' elapses first.

Worker-thread safe: the `onto-master' handler runs this on the merge
worker thread, where a blocking `call-process' would hold the global
Lisp lock for the script's entire (service-rebuild) runtime and freeze
the UI (the 2026-06-12 merge hang).  So it spawns the script
asynchronously with discarded output and blocks on
`agent-repl--wait-for-process-exit' (sentinel + condvar — releases
the lock) instead.

This IS the external-boundary wrapper for the script (registered in
`agent-repl--external-boundary-functions'): tests MUST mock it via
`cl-letf' rather than execute the real script."
  (let* ((default-directory (file-name-as-directory worktree))
         (script (expand-file-name agent-repl--cee-agent-reinstall-script
                                   worktree))
         (proc (start-process ;; ALLOW-EXTERNAL-BOUNDARY
                "agent-repl-cee-bounce" nil script)))
    (set-process-query-on-exit-flag proc nil)
    (agent-repl--wait-for-process-exit
     proc agent-repl--cee-agent-bounce-timeout-seconds nil nil)))

(defun agent-repl--onto-master-run-cee-agent-bounce (target-ws worktree)
  "Run the cee-agent reinstall-and-bounce script for TARGET-WS in WORKTREE.
Invoked by `agent-repl--merge-handler-onto-master' after the trunk
fast-forwards, but only when the merged delta touched
`agent-repl--cee-agent-dir-prefix'.  Returns the script's exit code,
or the symbol `timeout' when the script overran its budget.

NON-FATAL by design: the trunk has already advanced, so a script
failure (non-zero exit OR timeout) must NOT signal `user-error' —
that would drive the merge-async failure path to revive an
already-merged workspace.  The failure is logged for human follow-up
and teardown proceeds."
  ;; Log BEFORE spawning: if the script hangs past its timeout (or Emacs
  ;; is killed mid-run), this line is the only on-disk record that the
  ;; bounce was even attempted.
  (agent-repl--log target-ws
                    "merge-handler-onto-master: ws=%s cee-agent touched — running reinstall-and-bounce in %s"
                    target-ws worktree)
  (let ((ec (agent-repl--cee-agent-reinstall-and-bounce-exit-code worktree)))
    (agent-repl--log target-ws
                      "merge-handler-onto-master: ws=%s reinstall-and-bounce finished result=%S in %s"
                      target-ws ec worktree)
    (cond
     ((eq ec 'timeout)
      (agent-repl--log target-ws
                        "merge-handler-onto-master: ws=%s WARNING reinstall-and-bounce TIMED OUT after %ss — trunk already advanced, NOT reverting"
                        target-ws agent-repl--cee-agent-bounce-timeout-seconds))
     ((/= ec 0)
      (agent-repl--log target-ws
                        "merge-handler-onto-master: ws=%s WARNING reinstall-and-bounce FAILED (exit=%d) — trunk already advanced, NOT reverting"
                        target-ws ec)))
    ec))

(defun agent-repl--merge-handler-onto-master (target-ws &optional _args)
  "Advance local master to origin/master for an ALREADY-MERGED PR, then close.

Handler for the `/create-or-update-workspace merge --onto-master' contract: the
workspace's PR has already merged into `origin/master', so there is
nothing to cherry-pick or poll for — the only work is to bring the
main worktree's local trunk up to the merged commit and tear the
workspace down.  Selected per-invocation by the dispatcher's
`onto-master' argument, NOT by the repo's `.eld' handler, so it
overrides the checked-in cherry-pick prescription.

Never discards local-only trunk commits:
  - SIGNALS `user-error' on a dirty main worktree (so the merge-async
    failure path revives the workspace for the user), mirroring
    `agent-repl--merge-handler-refresh-master-from-origin'.
  - When local master is behind `origin/master' (an ancestor), advances
    it by fast-forward.
  - When local master has DIVERGED (carries commits not on origin),
    rebases those commits onto `origin/master' via
    `agent-repl--rebase-with-auto-resolve', which auto-resolves
    orthogonal conflicts through the shared headless resolver and aborts
    on any it cannot; a declined rebase SIGNALS `user-error' (the old
    resolve-by-hand path), so local commits are never discarded.
  - SIGNALS `user-error' when the fetch or the advance itself fails.

Git work runs synchronously on the merge worker thread (the same
thread context the cherry-pick handler does its git work on); only
the UI teardown is deferred to the main thread via
`agent-repl--finalize-merged-workspace'.

When the source dir cannot be resolved, logs and no-ops (mirroring the
refresh handler's same edge case).  ARGS is unused."
  (let* ((project-dir (agent-repl--ws-get target-ws :project-dir))
         (source-ws-dir (agent-repl--ws-get target-ws :source-ws-dir))
         (source-dir (or project-dir source-ws-dir))
         (main-dir (and source-dir
                        (agent-repl--main-worktree-path source-dir)))
         (branch agent-repl-master-branch-name)
         (origin-ref (concat "origin/" branch))
         (source-kind (cond
                       (project-dir "project-dir")
                       (source-ws-dir "source-ws-dir")
                       (t "missing"))))
    (agent-repl--log target-ws
                      "merge-handler-onto-master: ws=%s source-kind=%s source-dir=%s main-dir=%s branch=%s origin-ref=%s"
                      target-ws source-kind (or source-dir "nil")
                      (or main-dir "nil") branch origin-ref)
    (cond
     ((not main-dir)
      (agent-repl--log target-ws
                        "merge-handler-onto-master: ws=%s no main-dir — skipping"
                        target-ws))
     ((agent-repl--worktree-dirty-p main-dir)
      (agent-repl--log target-ws
                        "merge-handler-onto-master: ws=%s main worktree %s is dirty — signaling error"
                        target-ws main-dir)
      (user-error
       "Cannot advance master from origin: main worktree '%s' has uncommitted changes — stash or commit first"
       main-dir))
     (t
      ;; Fetch the merged commit (synchronous git on the worker thread,
      ;; same as the cherry-pick handler's git work).
      (let ((fec (agent-repl--git-exit-code main-dir "fetch" "origin" branch)))
        (agent-repl--log target-ws
                          "merge-handler-onto-master: ws=%s fetch origin %s exit=%d"
                          target-ws branch fec)
        (unless (= fec 0)
          (user-error "Cannot advance master: `git fetch origin %s' failed in %s (exit %d)"
                      branch main-dir fec)))
      ;; Whether local master has DIVERGED (carries commits not on origin)
      ;; and whether the merged work touches `apps/cee-agent' BOTH must be
      ;; sampled BEFORE any advance — afterwards local trunk equals
      ;; `origin-ref' and the `branch..origin-ref' diff is empty.
      (let ((diverged
             (not (= 0 (agent-repl--git-exit-code
                        main-dir "merge-base" "--is-ancestor"
                        branch origin-ref))))
            (cee-touched
             (agent-repl--merge-delta-touches-dir-p
              main-dir branch origin-ref agent-repl--cee-agent-dir-prefix))
            (master-wt (agent-repl--master-worktree-path main-dir)))
        (agent-repl--log target-ws
                          "merge-handler-onto-master: ws=%s pre-advance branch=%s origin-ref=%s diverged=%s cee-touched=%s master-worktree=%s"
                          target-ws branch origin-ref (if diverged "t" "nil")
                          (if cee-touched "t" "nil") (or master-wt "nil"))
        (cond
         ;; Diverged: rebase local master onto origin (auto-resolving
         ;; orthogonal conflicts via the shared headless resolver) rather
         ;; than refusing.  A conflict the resolver declines aborts the
         ;; rebase and falls through to the manual-resolution error, so the
         ;; worst case is exactly the old refuse-and-resolve-by-hand path.
         (diverged
          (unless master-wt
            (agent-repl--log target-ws
                              "merge-handler-onto-master: ws=%s diverged branch=%s but no master worktree — aborting"
                              target-ws branch)
            (user-error
             "Cannot rebase %s onto %s: no worktree has %s checked out — resolve manually"
             branch origin-ref branch))
          (agent-repl--log target-ws
                            "merge-handler-onto-master: ws=%s local %s diverged from %s — rebasing in %s"
                            target-ws branch origin-ref master-wt)
          (unless (agent-repl--rebase-with-auto-resolve
                   target-ws master-wt origin-ref)
            (agent-repl--log target-ws
                              "merge-handler-onto-master: ws=%s rebase branch=%s onto=%s result=unresolved — aborting"
                              target-ws branch origin-ref)
            (user-error
             "Cannot fast-forward %s to %s: local %s diverged and the rebase could not be auto-resolved — resolve manually"
             branch origin-ref branch)))
         ;; In sync with origin but behind: fast-forward the worktree that
         ;; holds master (so its working tree advances too).
         (master-wt
          (let ((mec (agent-repl--git-exit-code
                      master-wt "merge" "--ff-only" origin-ref)))
            (agent-repl--log target-ws
                              "merge-handler-onto-master: ws=%s merge --ff-only %s in %s exit=%d"
                              target-ws origin-ref master-wt mec)
            (unless (= mec 0)
              (user-error "Cannot fast-forward %s to %s in %s (exit %d)"
                          branch origin-ref master-wt mec))))
         ;; No master worktree: move the ref directly.
         (t
          (let ((uec (agent-repl--git-exit-code
                      main-dir "update-ref"
                      (concat "refs/heads/" branch)
                      (concat "refs/remotes/origin/" branch))))
            (agent-repl--log target-ws
                              "merge-handler-onto-master: ws=%s update-ref %s -> %s exit=%d"
                              target-ws branch origin-ref uec)
            (unless (= uec 0)
              (user-error "Cannot advance %s to %s via update-ref in %s (exit %d)"
                          branch origin-ref main-dir uec)))))
        ;; Trunk now carries the merged work: if it touched cee-agent,
        ;; reinstall and bounce the service.  Always run from the MAIN
        ;; worktree (the original clone, e.g.
        ;; `~/workspace/ChessCom/explanation-engine'), never the merged
        ;; workspace's sibling worktree — the service is installed and
        ;; run out of the main clone.
        (when cee-touched
          (agent-repl--onto-master-run-cee-agent-bounce
           target-ws main-dir))
        ;; Tear the workspace down on the main thread (UI ops).
        (agent-repl--log target-ws
                          "merge-handler-onto-master: ws=%s advance complete; scheduling main-thread finalization main-dir=%s"
                          target-ws main-dir)
        (agent-repl--defer-to-main-thread
         (lambda ()
           (agent-repl--log target-ws
                             "merge-handler-onto-master: ws=%s main-thread finalization starting main-dir=%s"
                             target-ws main-dir)
           (agent-repl--finalize-merged-workspace target-ws main-dir))))))))

(agent-repl--register-merge-handler
 'onto-master
 #'agent-repl--merge-handler-onto-master)

(provide 'merge-handlers)

;;; merge-handlers.el ends here
