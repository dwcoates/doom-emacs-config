;;; workspace.el --- Owner of `claude-repl--workspaces' state -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is the sole owner of the `claude-repl--workspaces' hash
;; table (workspace-name -> plist).  It exposes a wrapper API that
;; every other claude-repl source file is expected to use; direct
;; `gethash' / `puthash' / `maphash' against the variable elsewhere
;; is grandfathered for now but will be migrated in a follow-up
;; refactor.  See AGENTS.md ("Workspace state encapsulation").
;;
;; The wrapper API is (current set; expanded incrementally by the
;; render-state unification branch):
;;
;;   - `claude-repl--workspaces'           the hash itself
;;   - `claude-repl--ws-runtime-keys'      keys cleared on tombstone
;;   - `claude-repl--ws-get'                read one plist key
;;   - `claude-repl--ws-put'                set one plist key (logs stub-create)
;;   - `claude-repl--ws-put-caller-trace'   diagnostic helper for `--ws-put'
;;   - `claude-repl--ws-del'                tombstone (preserves identity)
;;   - `claude-repl--ws-live-p'             entry exists AND not tombstoned
;;   - `claude-repl--live-ws-names'         live names (filtered)
;;
;; Future wrappers introduced by this branch:
;;   - `claude-repl--ws-known-p'            entry exists (live or tombstoned)
;;   - `claude-repl--ws-require-known'      assert known, signal user-error
;;   - `claude-repl--ws-tombstoned-p'       entry exists AND tombstoned
;;   - `claude-repl--ws-open-p'             present in `persp-names-cache'
;;   - `claude-repl--ws-render-status'      single source of truth for what
;;                                          renderers (drawer / tab-bar /
;;                                          picker) should display
;;
;; Important non-API details that callers should NOT depend on:
;;
;;   - The hash key is the workspace's persp NAME (a string), but a
;;     workspace's BRANCH name may differ — never use one as a proxy
;;     for the other.  See the discussion above `defvar
;;     claude-repl--workspaces' below.
;;
;;   - `--ws-del' does NOT `remhash'.  It tombstones (stamps
;;     `:nuked-at', clears every key in `--ws-runtime-keys').  The
;;     identity keys (`:project-dir', `:created-at', `:ws-id',
;;     `:source-ws-dir', `:priority', the `:merge-completed*' family)
;;     survive so reverse-lookups, picker sort, and the MERGED drawer
;;     bucket keep working past nuke.

;;; Code:

(require 'cl-lib)

(cl-defstruct claude-repl-instantiation
  "Per-environment session state for a Claude REPL workspace.
Each workspace has one instantiation for :sandbox and one for :bare-metal."
  session-id    ; Claude Code session ID, captured from the `session_start' hook payload via `claude-repl--update-session-id-from-sentinel'
  start-cmd)    ; last startup command (for logging/display)

;; The hash and its accessors moved here from core.el during the
;; render-state unification refactor.  See file Commentary above for
;; the encapsulation contract.
;;
;; NOTE: workspace name != git branch name.
;; `claude-repl--do-create-worktree-workspace' derives the persp name from the
;; *last path component* of the input (e.g. "DWC/fix-login" -> persp "fix-login"),
;; while the full input becomes the branch name ("DWC/fix-login").  Never assume
;; the two are equal.  To resolve a workspace to its branch, retrieve its
;; :project-dir from this hash and run `git rev-parse --abbrev-ref HEAD' there.
(defvar claude-repl--workspaces (make-hash-table :test 'equal)
  "Hash table mapping workspace name -> state plist.
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

(provide 'claude-repl-workspace)
;;; workspace.el ends here
