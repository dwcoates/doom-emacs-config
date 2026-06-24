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

;; Forward declarations for symbols defined later in the load order (status.el).
;; Workspace.el loads before status.el; these calls fire only at runtime so
;; the cross-file reference is safe, but byte-compile-time would otherwise
;; warn about a free variable / unknown function.
(declare-function claude-repl--priority-rank "claude-repl-status" (priority))
(declare-function claude-repl--state-save "claude-repl-history" (ws))
(declare-function claude-repl--kill-session "claude-repl-panels" (ws))
(declare-function claude-repl--kill-workspace-buffers "claude-repl-commands" (ws))
(declare-function +workspace-exists-p "ext:persp-mode" (name))
(declare-function +workspace/kill "ext:persp-mode" (name))
(declare-function persp-update-names-cache "ext:persp-mode" (cache))
(declare-function magit-status "ext:magit" (&optional directory cache))
(declare-function claude-repl--path-canonical "claude-repl-core" (path))
(declare-function doom-real-buffer-list "ext:doom" (&optional buffer-list))
(defvar persp-nil-name)
(defvar persp-names-cache)
(defvar persp-mode)
(defvar persp-auto-resume-time)
(defvar persp-auto-save-opt)
(defvar persp-kill-foreign-buffer-behaviour)
(defvar persp-set-frame-buffer-predicate)
(defvar +workspaces-switch-project-function)
(defvar claude-repl--restored-workspaces)

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

(defvar claude-repl--workspace-history nil
  "Workspace names ordered by most-recently-visited first.
Maintained by `claude-repl--record-workspace-history' on every workspace
activation.  Read by the rename and workspace-merge paths and by
`claude-repl-open-most-recent-workspace'.")

(defvar claude-repl--opened-recent-workspaces nil
  "Workspaces already returned by `claude-repl-open-most-recent-workspace'
this session, so repeated invocations cycle through history instead of
returning the same workspace twice.")

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

(defun claude-repl--ws-dir-owner (dir &optional except)
  "Return a live workspace (other than EXCEPT) owning canonical DIR, or nil.
Enforces the one-live-workspace-per-`:project-dir' invariant: a second
workspace must not claim a dir a live workspace already owns, since that
shadowing is what lets a stub (e.g. a Doom-auto-named \"#N\" perspective)
collide with the real workspace in `claude-repl--ws-for-dir'."
  (when dir
    (let ((canonical (claude-repl--path-canonical dir)))
      (cl-find-if
       (lambda (ws)
         (and (not (equal ws except))
              (let ((p (claude-repl--ws-get ws :project-dir)))
                (and p (string= canonical (claude-repl--path-canonical p))))))
       (claude-repl--live-ws-names)))))

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

;;;; ---- Membership predicates -------------------------------------------

(defun claude-repl--ws-known-p (ws)
  "Return non-nil iff WS has a hash entry, live or tombstoned.
The membership question without the liveness filter — true for every
ws that has ever been registered and not hard-removed.  Wrappers that
must distinguish unknown from tombstoned (e.g. `--ws-render-status')
call this first to validate that the caller's WS argument refers to a
ws the module knows about at all.

Uses the same `claude-repl--ws-absent' sentinel as `--ws-live-p' so a
ws whose plist happens to be the empty list (`nil') still counts as
present."
  (not (eq (gethash ws claude-repl--workspaces 'claude-repl--ws-absent)
           'claude-repl--ws-absent)))

(defun claude-repl--ws-require-known (ws context)
  "Signal `user-error' unless WS is `--ws-known-p'.
CONTEXT is a short string identifying the caller for the message body,
e.g. `\"ws-render-status\"' or `\"ws-open-p\"'.  Used by wrappers that
contractually refuse to operate on an unknown ws (per the AGENTS.md
no-silent-fallback rule).  Returns nil on success."
  (unless (claude-repl--ws-known-p ws)
    (user-error "claude-repl: %s: workspace %S is not registered" context ws)))

(defun claude-repl--ws-tombstoned-p (ws)
  "Return non-nil iff WS is known AND has `:nuked-at' set.
Complementary to `--ws-live-p' over `--ws-known-p': a known ws is
either live or tombstoned, never both.  Unknown ws returns nil.

A tombstone may additionally carry a REASON marker explaining why
the entry was nuked, layered on top of the `:nuked-at' stamp:

  - `:hidden-project-dir t' — the entry was killed by
    `claude-repl-hide-project-dirs-mode' and is eligible for restore
    when the mode toggles off.  Use `--ws-hide-tombstoned-p' to test
    for this reason specifically.

The base `--ws-tombstoned-p' predicate intentionally collapses all
reasons because every renderer treats them identically (drawer
filters them out, `--ws-render-status' returns nil); callers that
need to distinguish reasons use the reason-specific helper."
  (and (claude-repl--ws-known-p ws)
       (not (null (claude-repl--ws-get ws :nuked-at)))))

(defun claude-repl--ws-hide-tombstoned-p (ws)
  "Return non-nil iff WS is tombstoned for the hide-project-dirs reason.
True when WS is `--ws-tombstoned-p' AND carries `:hidden-project-dir t'
on its plist (the marker stamped by
`claude-repl--hide-project-dirs--hide' before the nuke).  Used by the
restore path to enumerate the tombstones it owns without sweeping in
nuke-by-hand tombstones it does not.

Unknown ws returns nil.  Live ws (no `:nuked-at') returns nil even
if `:hidden-project-dir' happens to be t — the predicate is a
conjunction of tombstone state and reason marker."
  (and (claude-repl--ws-tombstoned-p ws)
       (not (null (claude-repl--ws-get ws :hidden-project-dir)))))

(defun claude-repl--ws-hide-tombstoned-names ()
  "Return the names of every workspace tombstoned for the hide reason.
Wrapper around the `(hash-table-keys claude-repl--workspaces)' walk
filtered by `--ws-hide-tombstoned-p'.  Used by
`claude-repl-hide-project-dirs-mode's restore path so it does not
poke `claude-repl--workspaces' directly (per the
\"Workspace state encapsulation\" rule in AGENTS.md).

Sorted by name so restore order is deterministic and matches what
the previous direct-hash-walk produced."
  (sort (cl-remove-if-not #'claude-repl--ws-hide-tombstoned-p
                          (hash-table-keys claude-repl--workspaces))
        #'string<))

(defun claude-repl--ws-tombstoned-names ()
  "Return the names of every tombstoned workspace, regardless of reason.
All entries in `claude-repl--workspaces' for which `--ws-tombstoned-p'
returns non-nil.  Sorted by name for determinism, paralleling
`--ws-hide-tombstoned-names' (which applies the additional
hide-reason filter).  Used by the snapshot collector to gather the
identity records that must survive Emacs restart without pulling
all-tombstones through a direct `hash-table-keys' walk at the call
site."
  (sort (cl-remove-if-not #'claude-repl--ws-tombstoned-p
                          (hash-table-keys claude-repl--workspaces))
        #'string<))

(defun claude-repl--ws-names-cache-usable-p ()
  "Return non-nil when `persp-names-cache' is bound and non-nil.
'Usable' means the cache is available as a reliable tab-bar membership
signal — the persp-mode cache has been populated with at least one
entry.  Returns nil when:
  - `persp-names-cache' is unbound (persp-mode not loaded), or
  - `persp-names-cache' is bound but nil (startup init phase or test
    stubs where persp-mode is not active and no persps exist yet).
Callers (principally `--collect-snapshot-entries') use this to decide
whether to consult the cache as the authoritative tab-bar source or fall
back to a plain hash-traversal that includes all live entries.  Part of
the persp-mode integration boundary owned by `workspace.el' (see file
Commentary and AGENTS.md)."
  (and (boundp 'persp-names-cache) persp-names-cache))

(defun claude-repl--ws-open-p (ws)
  "Return non-nil iff WS is currently visible in the tab-bar.
\"Open\" means `persp-names-cache' membership — the persp-mode hash
that drives the tab-bar's rendered names.  This is intentionally
DECOUPLED from `claude-repl--workspaces' membership because the two
can legitimately diverge:

  - During snapshot-restore, hash entries exist before
    `persp-add-new' runs.
  - After a successful merge with `preserve-entry', the hash entry
    survives `+workspace/kill' so the drawer's MERGED bucket can keep
    rendering.

Errors via `--ws-require-known' on an unknown ws so the caller never
silently asks about a name the module never heard of.  Returns nil
when `persp-names-cache' is unbound (vanilla Emacs / pre-persp init)."
  (claude-repl--ws-require-known ws "ws-open-p")
  (and (boundp 'persp-names-cache)
       (member ws persp-names-cache)
       t))

(defun claude-repl--ws-list-names ()
  "Return the list of workspace names visible in the tab-bar.
Intersection of `persp-names-cache' membership and
`claude-repl--workspaces' registration, minus the `persp-nil-name'
sentinel.  Equivalent to \"all names for which `--ws-open-p' returns
non-nil\" but computed in one pass.

This is the canonical iteration source for any renderer that
enumerates the tab-bar (e.g. `status.el's tabline render functions).
Callers should prefer it over `+workspace-list-names' so claude-repl
never depends on persp-mode's notion of \"all persps\" — only on its
own notion of \"workspaces this module owns\".

Divergence note: a persp in `persp-names-cache' that is NOT
registered in `claude-repl--workspaces' (e.g. one created without
going through claude-repl's establishment path) is excluded.  In
normal claude-repl operation every persp goes through
`--establish-workspace' or `--new-workspace' before reaching the
cache, so the two are equivalent.  Unregistered persps would
previously appear in the tab-bar with no glyph or state coloring;
they now silently drop out — intentional, since the tab-bar is a
claude-repl UI and should reflect claude-repl's worldview.

Returns nil when `persp-names-cache' is unbound."
  (when (boundp 'persp-names-cache)
    (let ((nil-name (and (boundp 'persp-nil-name) persp-nil-name)))
      (cl-loop for name in persp-names-cache
               when (and (not (and nil-name (equal name nil-name)))
                         (claude-repl--ws-known-p name))
               collect name))))

(defun claude-repl--ws-all-names ()
  "Return the raw list of ALL workspace names known to persp-mode.
Delegates to `+workspace-list-names', returning every persp in the
tab-bar regardless of whether claude-repl registered it.  Returns nil
when `+workspace-list-names' is unbound (persp-mode not loaded).

Differs from `claude-repl--ws-list-names', which intersects the cache
with `claude-repl--workspaces' to yield only claude-repl-owned names.
Use THIS wrapper for uniqueness checks and any path that must observe
persps claude-repl did not create.  Use `--ws-list-names' for
renderers that should reflect only claude-repl's own workspaces.

This is the persp-mode namespace boundary owned by `workspace.el'.
Callers must use this function instead of calling `+workspace-list-names'
directly or wrapping it themselves with `fboundp'."
  (when (fboundp '+workspace-list-names)
    (+workspace-list-names)))

;;;; ---- Render-state unification ----------------------------------------
;;
;; `claude-repl--ws-render-status' is the single source of truth for
;; what visual state every renderer (drawer state-glyph, drawer
;; name-face, tab-bar composed-state, project picker emoji) should
;; display for a workspace.  Renderers used to each re-derive this
;; from `:claude-state' + `:repl-state' + the `:merging' /
;; `:merge-completed' plist keys, and they disagreed: the drawer's
;; precedence had merge-state dominating claude-state, the tab-bar's
;; precedence had claude-state dominating merge-state, and the
;; `:merging' (in-flight) workflow signal had no visual at all.  The
;; unified function below is the new canonical precedence; the
;; rendering convergence is intentional.

(defun claude-repl--ws-render-status (ws)
  "Return the closed-set render-state keyword for workspace WS.
This is the SINGLE SOURCE OF TRUTH for what renderers (drawer,
tab-bar, project picker, mode-line) should display for a workspace's
status.  Every renderer reads this — none should re-derive status
from `:claude-state' / `:repl-state' / `:merging' / `:merge-completed'
on its own.

Precondition: WS must be `--ws-known-p'.  Unknown WS signals
`user-error' via `--ws-require-known' — there is no silent fallback
per AGENTS.md.

Returns one of (in precedence order; first match wins):

  :merge-conflict — `:repl-state' is `:merge-conflict' (cherry-pick
                    hit a non-orthogonal conflict; user action
                    required).  Dominates everything else because it
                    is the most actionable signal.

  :merge-failed   — `:repl-state' is `:merge-failed' (silent
                    cherry-pick abort, no CHERRY_PICK_HEAD remaining).
                    Same actionable rationale as :merge-conflict.

  :merged         — `:repl-state' is `:merged' OR `:merge-completed'
                    is t AND no active `:claude-state' is present
                    (workspace's branch landed in its source; terminal
                    positive).  The setter writes both in lockstep —
                    either signal alone suffices.  Accepting
                    `:merge-completed' as equivalent here preserves the
                    original drawer behavior across the brief transition
                    window where `:merge-completed t' is set before
                    `:repl-state :merged' has been written (and where
                    `:merging' may not yet be cleared).
                    When an active `:claude-state' is present the
                    merged badge is suppressed and the claude-state
                    wins: a merged workspace that resumes work (unusual
                    but possible) should surface the live run-state
                    rather than the stale merge badge.

  :merging        — `:merging' plist key is `t' (worker thread is
                    actively running cherry-pick).  Beats :dead so a
                    workspace whose vterm has been torn down (the
                    standard pre-merge `--close-workspace'
                    `preserve-entry' path) still surfaces the
                    in-flight signal until cherry-pick resolves.

  :merge-queued   — `:repl-state' is `:merge-queued' (parked on
                    `claude-repl--merge-queue' waiting for an
                    in-flight cherry-pick to clear).

  :dead           — `:repl-state' is `:dead' (vterm process is gone).
                    Ranks below merge-states because merge state is
                    more actionable; ranks above claude-states
                    because no live process means no claude activity
                    to color over.

  Claude-states (when no merge or dead signal applies):
    :thinking, :permission, :init, :done, :stop-failed, :idle
    — read from `:claude-state' in order of precedence.  Each is set
    by `claude-repl--ws-set-claude-state' through the typed setter.

  nil             — tombstoned workspace (`--ws-tombstoned-p' t,
                    regardless of REASON marker such as
                    `:hidden-project-dir'), or no session / unborn
                    (every signal above absent).  The cases are
                    intentionally collapsed: every renderer skips them
                    equally (the drawer's `--live-ws-names' filter
                    already excludes tombstones before this function
                    is called, and callers that need to distinguish
                    nuke-tombstoned from hide-tombstoned use the
                    reason-specific predicate
                    `--ws-hide-tombstoned-p').

Precedence rationale: a workspace that crashed its vterm *while a
merge was in flight* still needs to surface the merge signal — the
merge is the actionable concern.  Same logic stacks all the way up:
merge-conflict is more important than merge-failed (an active
conflict can be resolved; a silent abort has already aborted), and
both dominate claude-state (an active conflict is more important
than whether Claude was thinking when the merge hit it)."
  (claude-repl--ws-require-known ws "ws-render-status")
  (cond
   ((claude-repl--ws-tombstoned-p ws) nil)
   (t
    (let ((repl       (claude-repl--ws-get ws :repl-state))
          (claude     (claude-repl--ws-get ws :claude-state))
          (merging    (claude-repl--ws-get ws :merging))
          (completed  (claude-repl--ws-get ws :merge-completed)))
      (cond
       ((eq repl :merge-conflict)         :merge-conflict)
       ((eq repl :merge-failed)           :merge-failed)
       ;; `:merge-completed t' is accepted as equivalent to
       ;; `:repl-state :merged' so the transition window between the
       ;; two writes (and any tests fixtures that set only one of
       ;; them) still resolves to :merged.  See `--ws-render-status'
       ;; docstring for the rationale.
       ;; Guard: when an active `:claude-state' is present the merged
       ;; badge is suppressed so a merged workspace that resumes work
       ;; surfaces its live run-state rather than the stale merge
       ;; badge.  Without the (null claude) guard this arm would fire
       ;; even when claude is :thinking / :done / etc., hiding those
       ;; states behind the static 🔀 label.
       ((and (or (eq repl :merged) (eq completed t)) (null claude)) :merged)
       ;; :merging plist key (in-flight worker) dominates :dead so
       ;; the merge UI signal survives the pre-merge UI teardown.
       ((eq merging t)                    :merging)
       ((eq repl :merge-queued)           :merge-queued)
       ((eq repl :dead)                   :dead)
       ((eq claude :thinking)             :thinking)
       ((eq claude :permission)           :permission)
       ((eq claude :init)                 :init)
       ((eq claude :done)                 :done)
       ((eq claude :stop-failed)          :stop-failed)
       ((eq claude :idle)                 :idle)
       (t                                 nil))))))

;;;; ---- Persp-mode integration boundary ---------------------------------
;;
;; The functions below are the ONLY place inside claude-repl that touches
;; persp-mode internals (`persp-names-cache', `+workspace/kill',
;; `+workspace-exists-p', `persp-update-names-cache', etc).  See AGENTS.md
;; "NEVER manipulate third-party internals from a high-level layer" — the
;; wrappers in this section ARE the integration boundary they describe.
;; Callers in `commands.el', `status.el', `drawer.el', etc. must route
;; through these, not poke persp-mode directly.

(defun claude-repl--nuke-one-workspace (ws &optional preserve-entry)
  "Tear down a single claude-repl workspace WS without prompting.
Kills any in-flight git-diff process, tears down the vterm session
and buffers, removes WS from `claude-repl--workspaces', kills every
remaining buffer (and attached process) that belongs to the persp via
`claude-repl--kill-workspace-buffers', and finally kills the persp
workspace via `+workspace/kill'.  Designed to be reusable from
`claude-repl-nuke-workspace' (one-shot),
`claude-repl-nuke-all-workspaces' (loop), and
`claude-repl-kill-workspace'.

When PRESERVE-ENTRY is non-nil, the `claude-repl--workspaces' hashmap
entry is retained — every other teardown step runs as usual (vterm,
buffers, persp), but the ws plist survives so the drawer's MERGED
section can keep rendering the entry until the user explicitly
`finish'es it.  This is the merge-completed teardown path; standard
nuke/kill callers pass nil and the entry is dropped.

Persisted state (`<project>/.claude/emacs/state.el', including the
captured per-environment session-id) is ALWAYS preserved — nuke is
purely an in-memory teardown.  An explicit `--state-save' runs at the
top of the function so the file reflects the latest in-memory state
even if downstream teardown errors before the redundant state-save in
`--teardown-session-state' can fire.

The hashmap removal (`ws-del') runs inside an `unwind-protect' cleanup
so it always happens, even when kill-session errors partway through.
The persp kill is the very last step so all internal state is already
cleaned up before the UI workspace disappears.  Callers can rely on
the post-condition: after the call returns \(or throws), WS is not in
`claude-repl--workspaces' (unless PRESERVE-ENTRY was non-nil) and its
on-disk state.el is up-to-date.

This function is part of the persp-mode integration boundary owned
by `workspace.el' (see file Commentary and AGENTS.md).  It is the
only `+workspace/kill' call site inside claude-repl outside the
finish-workspace path."
  (claude-repl--log ws "nuke-one-workspace: ENTRY ws=%s preserve-entry=%s cache=%S"
                    ws (if preserve-entry "t" "nil")
                    (if (boundp 'persp-names-cache) persp-names-cache "(unbound)"))
  ;; Stamp the kill timestamp before the pre-teardown state-save so the
  ;; on-disk state.el reflects this kill.  The project picker
  ;; (`claude-repl-switch-to-project') reads `:last-killed-at' to sort
  ;; entries (most-recently-killed first) and to color the kill-date
  ;; column.  Recorded on the ws plist so the immediately-following
  ;; `--state-save' picks it up via `claude-repl--ws-get'.
  (claude-repl--ws-put ws :last-killed-at (current-time))
  ;; Save first, before any teardown touches the ws plist or risks
  ;; erroring.  The teardown path also calls state-save, but wrapping
  ;; ours up front guarantees preservation even if a downstream step
  ;; signals before that secondary save can run.  Wrapped in
  ;; condition-case so a save error doesn't abort the nuke itself.
  (condition-case err
      (claude-repl--state-save ws)
    (error (claude-repl--log ws "nuke-one-workspace: pre-teardown state-save error: %S" err)))
  (unwind-protect
      (progn
        (when-let ((proc (claude-repl--ws-get ws :git-proc)))
          (when (process-live-p proc)
            (claude-repl--log ws "nuke-one-workspace: killing git-proc")
            (condition-case err
                (delete-process proc)
              (error (claude-repl--log ws "nuke-one-workspace: git-proc kill error: %S" err)))))
        (claude-repl--log ws "nuke-one-workspace: calling kill-session ws=%s" ws)
        (condition-case err
            (claude-repl--kill-session ws)
          (error (claude-repl--log ws "nuke-one-workspace: kill-session error: %S" err)))
        (claude-repl--log ws "nuke-one-workspace: kill-session returned ws=%s" ws))
    ;; Cleanup: always remove the hashmap entry regardless of any error
    ;; in the steps above (unless PRESERVE-ENTRY was requested).
    ;; Persisted state.el is intentionally NOT touched here — see the
    ;; docstring.
    (unless preserve-entry
      (condition-case err
          (claude-repl--ws-del ws)
        (error (claude-repl--log ws "nuke-one-workspace: ws-del error: %S" err))))
    ;; WHY: keep `claude-repl--restored-workspaces' consistent with the
    ;; live hash — a ws that's been nuked is no longer a restore-batch
    ;; member, so a follow-up `nuke-restored-workspaces' won't try to
    ;; re-tear-down a stale name.  The defvar lives in commands.el; we
    ;; treat it as the snapshot-restore module's state and mutate
    ;; through the var directly (forward defvar at the top of this file).
    (setq claude-repl--restored-workspaces
          (delete ws claude-repl--restored-workspaces))
    ;; Kill every remaining buffer (and attached process) that belongs to
    ;; the persp before tearing down the persp itself.  `kill-session'
    ;; only handles the vterm/input panels it tracks in the hashmap;
    ;; this sweep catches file buffers, magit buffers, auxiliary shells,
    ;; or anything else the user opened while inside the workspace so
    ;; nothing is orphaned after the persp goes away.
    (claude-repl--log ws "nuke-one-workspace: calling kill-workspace-buffers ws=%s" ws)
    (condition-case err
        (claude-repl--kill-workspace-buffers ws)
      (error (claude-repl--log ws "nuke-one-workspace: kill-workspace-buffers error: %S" err)))
    (claude-repl--log ws "nuke-one-workspace: kill-workspace-buffers returned ws=%s" ws)
    ;; Kill the persp workspace last so all internal state is already
    ;; cleaned up before the UI workspace disappears.
    ;;
    ;; Existence guard uses `+workspace-exists-p' (which checks
    ;; `persp-names-cache' via `+workspace-list-names'), matching the
    ;; same check `+workspace/kill' itself performs.  Earlier versions
    ;; gated on `(persp-get-by-name ws)' — but persp-mode's
    ;; `persp-get-by-name' returns the keyword `persp-not-persp' (i.e.
    ;; `:nil', a truthy value) when the persp is missing, so that
    ;; guard never short-circuited.  In the merge-async flow that
    ;; double-closes the workspace (once preemptively in
    ;; `--workspace-merge-async', then again in the deferred
    ;; success callback of `--workspace-merge-do'), pass 2 would slip
    ;; through the broken guard and call `+workspace/kill', which then
    ;; emitted the user-visible warning `'<ws>' workspace doesn't
    ;; exist' in the echo area after every successful merge.
    (condition-case err
        (when (and (claude-repl--ws-system-available-p)
                   (fboundp '+workspace-exists-p)
                   (+workspace-exists-p ws))
          (claude-repl--log ws "nuke-one-workspace: pre-persp-kill ws=%s cache=%S"
                            ws persp-names-cache)
          (+workspace/kill ws)
          (claude-repl--log ws "nuke-one-workspace: post-persp-kill ws=%s in-cache=%s cache=%S"
                            ws (if (member ws persp-names-cache) "t" "nil") persp-names-cache))
      (error (claude-repl--log ws "nuke-one-workspace: workspace-kill error: %S" err)))
    (claude-repl--log ws "nuke-one-workspace: DONE ws=%s all-cleanup-complete" ws)))

(defun claude-repl--reorder-workspace-by-priority (ws)
  "Reorder workspace WS in `persp-names-cache' by its `:priority'.
Order: p05 < p1 < p2 < p3 < unprioritized.  WS is placed after every
existing workspace of equal-or-higher priority and before every
lower-priority one, so a new entry never displaces an existing peer or
higher-priority sibling.  No-op when WS has no `:priority', when the
cache does not contain WS, or when persp-mode is not loaded — those
fall back to the persp-mode default of appending at the end.

Each entry, every bail-out, and the post-mutation cache state are
logged so the silent no-op paths are observable when reproducing
ordering bugs.

This function is part of the persp-mode integration boundary owned
by `workspace.el' (see file Commentary and AGENTS.md).  Callers must
route through it; they may not mutate `persp-names-cache' directly."
  (let ((priority (claude-repl--ws-get ws :priority))
        (cache-snapshot (if (boundp 'persp-names-cache) persp-names-cache "(unbound)")))
    (claude-repl--log ws "reorder-workspace-by-priority: ENTRY ws=%s priority=%s cache=%S"
                      ws priority cache-snapshot)
    (cond
     ((null priority)
      (claude-repl--log ws "reorder-workspace-by-priority: BAIL ws=%s reason=no-priority" ws))
     ((not (boundp 'persp-names-cache))
      (claude-repl--log ws "reorder-workspace-by-priority: BAIL ws=%s reason=cache-unbound" ws))
     ((not (member ws persp-names-cache))
      (claude-repl--log ws "reorder-workspace-by-priority: BAIL ws=%s reason=not-in-cache cache=%S"
                        ws persp-names-cache))
     (t
      ;; Use the canonical string already in persp-names-cache as the
      ;; identity we splice in.  persp-mode's `persp-remove-from-menu' calls
      ;; `(cl-delete name cache :count 1)' with the default `:test #'eql' —
      ;; for strings, eql is identity comparison.  If we substitute a fresh
      ;; string here (e.g. one returned by `completing-read' in
      ;; `claude-repl-set-priority'), the cache ends up holding a different
      ;; object than the persp's stored name, and `persp-kill' silently
      ;; fails to remove the workspace from the cache later.  The result
      ;; is a tab-bar entry that survives nuke and re-duplicates on
      ;; subsequent recreations.  Recovering the canonical string via
      ;; `(car (member ws cache))' (which uses `equal') keeps identity
      ;; aligned with the persp internal name.
      (let* ((nil-name (and (boundp 'persp-nil-name) persp-nil-name))
             (rank (claude-repl--priority-rank priority))
             (canonical-ws (car (member ws persp-names-cache)))
             (without-ws (cl-remove canonical-ws persp-names-cache :test #'eq :count 1))
             (visible (if nil-name
                          (cl-remove nil-name without-ws :test #'equal :count 1)
                        without-ws))
             (insert-at (cl-position-if
                         (lambda (n)
                           (> (claude-repl--priority-rank
                               (claude-repl--ws-get n :priority))
                              rank))
                         visible))
             (new-visible (if insert-at
                              (append (cl-subseq visible 0 insert-at)
                                      (list canonical-ws)
                                      (cl-subseq visible insert-at))
                            (append visible (list canonical-ws))))
             (new-cache (if (and nil-name (member nil-name persp-names-cache))
                            (cons nil-name new-visible)
                          new-visible)))
        (claude-repl--log ws "reorder-workspace-by-priority: APPLY ws=%s canonical-eq-input=%s priority=%s rank=%s position=%s new-cache=%S"
                          ws (if (eq canonical-ws ws) "t" "nil")
                          priority rank (or insert-at "end") new-cache)
        (if (fboundp 'persp-update-names-cache)
            (persp-update-names-cache new-cache)
          (claude-repl--log ws "reorder-workspace-by-priority: SKIP-APPLY ws=%s reason=persp-update-names-cache-unbound"
                            ws)))))))

(defun claude-repl--reorder-workspace-to-front (ws)
  "Move workspace WS to the front of `persp-names-cache' (visible portion).
The visible portion is everything after `persp-nil-name' when that
variable is bound (persp-mode keeps its sentinel persp at the head);
WS is inserted as the first element of the visible portion so it
shows up as the leftmost tab.

Mirrors `claude-repl--reorder-workspace-by-priority' in structure:
preserves cache string identity via the `(car (member ws cache))'
canonicalization (`persp-remove-from-menu' relies on `eql' identity
for string removal), and the nil-name slot at the cache head.

No-op when the cache does not contain WS, when persp-mode is not
loaded, or when `persp-update-names-cache' is unavailable.  Each entry,
every bail-out, and the post-mutation cache state are logged so the
silent no-op paths are observable when reproducing ordering bugs.

Used by the snapshot loader's merge-failed restore path so a workspace
whose cherry-pick silently failed pre-restart surfaces as the leftmost
tab on the next session, demanding the user's attention instead of
hiding in the drawer's MERGED bucket."
  (let ((cache-snapshot (if (boundp 'persp-names-cache) persp-names-cache "(unbound)")))
    (claude-repl--log ws "reorder-workspace-to-front: ENTRY ws=%s cache=%S"
                      ws cache-snapshot)
    (cond
     ((not (boundp 'persp-names-cache))
      (claude-repl--log ws "reorder-workspace-to-front: BAIL ws=%s reason=cache-unbound" ws))
     ((not (member ws persp-names-cache))
      (claude-repl--log ws "reorder-workspace-to-front: BAIL ws=%s reason=not-in-cache cache=%S"
                        ws persp-names-cache))
     (t
      (let* ((nil-name (and (boundp 'persp-nil-name) persp-nil-name))
             (canonical-ws (car (member ws persp-names-cache)))
             (without-ws (cl-remove canonical-ws persp-names-cache :test #'eq :count 1))
             (visible (if nil-name
                          (cl-remove nil-name without-ws :test #'equal :count 1)
                        without-ws))
             (new-visible (cons canonical-ws visible))
             (new-cache (if (and nil-name (member nil-name persp-names-cache))
                            (cons nil-name new-visible)
                          new-visible)))
        (claude-repl--log ws "reorder-workspace-to-front: APPLY ws=%s canonical-eq-input=%s new-cache=%S"
                          ws (if (eq canonical-ws ws) "t" "nil") new-cache)
        (claude-repl--ws-update-names-cache new-cache))))))

;;;; ---- persp-mode identity / navigation boundary -----------------------
;;
;; These thin wrappers insulate callers from the +workspace-* API names so
;; (a) the fboundp guards are written once, in one place; (b) tests stub a
;; single symbol instead of juggling `fboundp' and the real function; and
;; (c) future persp-mode API changes require only a local edit here.

(defun claude-repl--ws-resolve-persp (ws)
  "Return the live persp object for workspace name WS, or nil.
Delegates to `persp-get-by-name'.  Returns nil when:
  - `persp-get-by-name' is not bound (persp-mode not loaded), or
  - WS is not found — persp-mode returns the keyword `persp-not-persp'
    (i.e. `:nil') in that case, which is truthy but not a persp object;
    this wrapper normalizes that sentinel to nil.

Callers must use this function instead of calling `persp-get-by-name'
directly, so the persp-not-persp normalization and the fboundp guard are
applied consistently.  This is part of the persp-mode integration
boundary owned by `workspace.el' (see AGENTS.md)."
  (when (fboundp 'persp-get-by-name)
    (let ((p (persp-get-by-name ws)))
      ;; persp-get-by-name returns the :nil keyword (the value of the
      ;; `persp-not-persp' variable) when the persp is absent.  That
      ;; sentinel is a keyword (not a plain symbol), so `keywordp'
      ;; distinguishes it from a real persp struct.  Filter it out so
      ;; callers receive either a real persp struct or nil.
      (and p (not (keywordp p)) p))))

(defun claude-repl--ws-system-available-p ()
  "Return non-nil when the persp-mode workspace system is active.
Specifically, returns non-nil when the variable `persp-mode' is both
bound and non-nil — i.e. the same test as `(bound-and-true-p persp-mode)'.

Use this predicate instead of calling `bound-and-true-p' on `persp-mode'
directly.  The single definition here is the workspace.el integration
boundary for system-availability checks (see AGENTS.md), so a future
change in how availability is detected requires only a local edit."
  (bound-and-true-p persp-mode))

(defun claude-repl--ws-current-name ()
  "Return the name of the currently-active workspace, or nil.
Delegates to `+workspace-current-name' when persp-mode is loaded;
returns nil when that function is unbound (e.g. during tests that
do not load persp-mode, or early during startup before the workspace
system is ready).

This is the persp-mode identity boundary owned by `workspace.el'.
Callers must use this function instead of calling
`+workspace-current-name' directly or wrapping it themselves with
`fboundp'."
  (and (fboundp '+workspace-current-name)
       (+workspace-current-name)))

(defun claude-repl--ws-switch (ws &rest args)
  "Switch the active workspace to WS, passing ARGS to `+workspace-switch'.
No-op when `+workspace-switch' is not bound (e.g. during tests that do
not load persp-mode).  Any error from the underlying call propagates to
the caller unchanged — silence errors at the call site when needed.

ARGS are forwarded verbatim to `+workspace-switch', which lets callers
pass optional flags (e.g., a non-nil second argument to suppress the
tab-bar flash that `+workspace-switch' normally triggers).

This is the persp-mode navigation boundary owned by `workspace.el'.
Callers must use this function instead of calling `+workspace-switch'
directly or wrapping it themselves with `fboundp'."
  (when (fboundp '+workspace-switch)
    (apply '+workspace-switch ws args)))

(defun claude-repl--ws-exists-p (ws)
  "Return non-nil when workspace WS exists in the tab-bar.
Delegates to `+workspace-exists-p'.  Returns nil when that function is
unbound (persp-mode not loaded).

This is the persp-mode existence boundary owned by `workspace.el'.
Callers must use this function instead of calling `+workspace-exists-p'
directly or wrapping it themselves with `fboundp'."
  (and (fboundp '+workspace-exists-p)
       (+workspace-exists-p ws)))

(defun claude-repl--ws-kill (ws)
  "Kill workspace WS via `+workspace/kill'.
No-op when `+workspace/kill' is unbound (e.g. persp-mode not loaded).
Any error from the underlying call propagates to the caller — wrap at
the call site with `condition-case' when teardown must stay robust.

This is the persp-mode kill boundary owned by `workspace.el'.
Callers must use this function instead of calling `+workspace/kill'
directly or wrapping it themselves with `fboundp'."
  (when (fboundp '+workspace/kill)
    (+workspace/kill ws)))

(defun claude-repl--ws-main-name ()
  "Return the name of Doom's main workspace, or nil.
Reads `+workspaces-main', the variable holding the name Doom assigns to
the startup workspace.  Returns nil when that variable is unbound or nil
(e.g. persp-mode not loaded).

This is the persp-mode main-workspace boundary owned by `workspace.el'.
Callers must use this function instead of reading `+workspaces-main'
directly or guarding it themselves with `boundp'."
  (and (boundp '+workspaces-main) +workspaces-main))

(defun claude-repl--ws-frame-switch (ws)
  "Activate workspace WS on the current frame via `persp-frame-switch'.
No-op when `persp-frame-switch' is unbound (persp-mode not loaded).

This is the persp-mode frame-activation boundary owned by `workspace.el'.
Callers must use this function instead of calling `persp-frame-switch'
directly or wrapping it themselves with `fboundp'."
  (when (fboundp 'persp-frame-switch)
    (persp-frame-switch ws)))

(defun claude-repl--ws-frame-save-state ()
  "Save the current frame's persp window-configuration state.
Delegates to `persp-frame-save-state'.  No-op when that function is
unbound (persp-mode not loaded).  Errors propagate to the caller — wrap
at the call site when the save must stay robust.

This is the persp-mode frame-save boundary owned by `workspace.el'.
Callers must use this function instead of calling `persp-frame-save-state'
directly or wrapping it themselves with `fboundp'."
  (when (fboundp 'persp-frame-save-state)
    (persp-frame-save-state)))

(defun claude-repl--ws-create (ws &optional project-dir)
  "Create persp WS via `persp-add-new' and tag it with PROJECT-DIR.
Returns the new persp object, or nil when `persp-add-new' is unbound.

When PROJECT-DIR is non-nil and the new persp is a real persp object
\(not the `persp-not-persp' keyword sentinel), sets the persp's
`+workspace-project' parameter to PROJECT-DIR.  This makes a later
`SPC p p' to PROJECT-DIR match this workspace via Doom's
`+workspaces-switch-to-project-h' instead of hitting its
uniquify-by-parent-dir branch.  Without it, the `file-equal-p' check on
`+workspace-project' inside that hook errors against nil, the loop walks
up the path, and the workspace gets recreated under names like
`doom-worktrees/<ws>'.  Doom's own project hook sets this parameter; we
mirror it so the snapshot-restore and `SPC j o' paths produce
equivalent state.

This is the persp-mode creation boundary owned by `workspace.el'.
Callers must use this function instead of calling `persp-add-new' or
`set-persp-parameter' directly or wrapping them with `fboundp'."
  (when (fboundp 'persp-add-new)
    (let ((persp (persp-add-new ws)))
      (when (and persp (not (keywordp persp)) project-dir
                 (fboundp 'set-persp-parameter))
        (set-persp-parameter '+workspace-project project-dir persp))
      persp)))

(defun claude-repl--ws-protected-p (ws)
  "Return non-nil when workspace WS is protected from deletion/cycling.
Delegates to `+workspace--protected-p'.  Returns nil when that function
is unbound (persp-mode not loaded).

This is the persp-mode protection boundary owned by `workspace.el'.
Callers must use this function instead of calling `+workspace--protected-p'
directly or wrapping it themselves with `fboundp'."
  (and (fboundp '+workspace--protected-p)
       (+workspace--protected-p ws)))

(defun claude-repl--ws-error (message &optional noerror)
  "Report a workspace error via `+workspace-error' with MESSAGE.
With NOERROR non-nil, `+workspace-error' displays the message instead of
signaling.  No-op when `+workspace-error' is unbound (persp-mode not
loaded).  When it does signal, the error propagates to the caller.

This is the persp-mode error boundary owned by `workspace.el'.
Callers must use this function instead of calling `+workspace-error'
directly or wrapping it themselves with `fboundp'."
  (when (fboundp '+workspace-error)
    (+workspace-error message noerror)))

(defun claude-repl--ws-add-buffer (buffer persp &optional switch)
  "Attach BUFFER to perspective PERSP via `persp-add-buffer'.
SWITCH is forwarded as persp-add-buffer's switch argument (nil means do
not switch to the buffer).  No-op when `persp-add-buffer' is unbound
(persp-mode not loaded).  Idempotent — persp-add-buffer no-ops when the
buffer is already in the perspective.

This is the persp-mode buffer-attachment boundary owned by `workspace.el'.
Callers must use this function instead of calling `persp-add-buffer'
directly or wrapping it themselves with `fboundp'."
  (when (fboundp 'persp-add-buffer)
    (persp-add-buffer buffer persp switch)))

(defun claude-repl--ws-buffers (persp)
  "Return the list of buffers belonging to perspective PERSP.
Delegates to `persp-buffers'.  Returns nil when PERSP is nil or
`persp-buffers' is unbound (persp-mode not loaded).

This is the persp-mode buffer-listing boundary owned by `workspace.el'.
Callers must use this function instead of calling `persp-buffers'
directly or wrapping it themselves with `fboundp'."
  (and persp (fboundp 'persp-buffers)
       (persp-buffers persp)))

(defun claude-repl--ws-rename-persp (old-ws new-ws)
  "Rename the live perspective for OLD-WS to NEW-WS.
Resolves OLD-WS's persp via `--ws-resolve-persp' and renames it with
`persp-rename'.  Returns non-nil on success or when there is nothing to
rename (persp-mode unloaded, or OLD-WS has no live persp).  Returns nil
ONLY when a live persp existed but `persp-rename' reported failure.

This is the persp-mode rename boundary owned by `workspace.el'.
Callers must use this function instead of calling `persp-rename'
directly or wrapping it themselves with `fboundp'."
  (if (not (fboundp 'persp-rename))
      t
    (if-let ((persp (claude-repl--ws-resolve-persp old-ws)))
        (and (persp-rename new-ws persp) t)
      t)))

(defun claude-repl--ws-frame-ordered-names ()
  "Return workspace names in current-frame tab-bar order.
Delegates to `persp-names-current-frame-fast-ordered'.  Returns nil when
that function is unbound (persp-mode not loaded).

This is the persp-mode frame-order boundary owned by `workspace.el'.
Callers must use this function instead of calling
`persp-names-current-frame-fast-ordered' directly or wrapping it with
`fboundp'."
  (when (fboundp 'persp-names-current-frame-fast-ordered)
    (persp-names-current-frame-fast-ordered)))

(defun claude-repl--ws-update-names-cache (names)
  "Replace the persp names cache with NAMES via `persp-update-names-cache'.
No-op when that function is unbound (persp-mode not loaded).

This is the persp-mode names-cache boundary owned by `workspace.el'.
Callers must use this function instead of calling
`persp-update-names-cache' directly or wrapping it with `fboundp'."
  (when (fboundp 'persp-update-names-cache)
    (persp-update-names-cache names)))

(defun claude-repl--ws-window-conf (persp)
  "Return the saved window-configuration for perspective PERSP.
Delegates to `persp-window-conf'.  Returns nil when PERSP is nil or
`persp-window-conf' is unbound (persp-mode not loaded).

This is the persp-mode window-config boundary owned by `workspace.el'.
Callers must use this function instead of calling `persp-window-conf'
directly or wrapping it with `fboundp'."
  (and persp (fboundp 'persp-window-conf)
       (persp-window-conf persp)))

(defun claude-repl--ws-tab-face ()
  "Return the Doom face symbol for an unselected workspace tab name.
Names the `+workspace-tab-face' face that Doom's tab-bar defines.

This is the Doom tab-face boundary owned by `workspace.el'.
Callers must use this function instead of referring to
`+workspace-tab-face' directly."
  '+workspace-tab-face)

(defun claude-repl--ws-tab-selected-face ()
  "Return the Doom face symbol for a selected workspace tab name.
Names the `+workspace-tab-selected-face' face that Doom's tab-bar defines.

This is the Doom tab-face boundary owned by `workspace.el'.
Callers must use this function instead of referring to
`+workspace-tab-selected-face' directly."
  '+workspace-tab-selected-face)

(defun claude-repl--workspace-for-buffer (buf)
  "Return the workspace name whose perspective contains BUF, or nil.
Scans `persp-persps' for the perspective that owns BUF.  Returns nil
when the workspace system is unavailable.

This is persp-mode buffer-ownership resolution; it lives in
`workspace.el' because it touches the raw persp set directly."
  (when (claude-repl--ws-system-available-p)
    (cl-loop for persp in (persp-persps)
             when (persp-contain-buffer-p buf persp)
             return (safe-persp-name persp))))

(defun claude-repl--ws-all-persps ()
  "Return the raw list of all perspective objects via `persp-persps'.
Returns nil when `persp-persps' is unbound (persp-mode not loaded).  The
list may include persp-mode's nil container and non-perspective symbol
entries; callers filter as needed.

This is the persp-mode enumeration boundary owned by `workspace.el'.
Callers must use this function instead of calling `persp-persps'
directly or wrapping it themselves with `fboundp'."
  (when (fboundp 'persp-persps)
    (persp-persps)))

(defun claude-repl--ws-persp-name (persp)
  "Return the name of perspective PERSP via `safe-persp-name'.
Returns nil when `safe-persp-name' is unbound (persp-mode not loaded).

This is the persp-mode name-resolution boundary owned by `workspace.el'.
Callers must use this function instead of calling `safe-persp-name'
directly or wrapping it themselves with `fboundp'."
  (when (fboundp 'safe-persp-name)
    (safe-persp-name persp)))

(defun claude-repl--ws-nil-name ()
  "Return persp-mode's sentinel \"no perspective\" name, or nil.
Reads `persp-nil-name'.  Returns nil when that variable is unbound
(persp-mode not loaded).

This is the persp-mode sentinel-name boundary owned by `workspace.el'.
Callers must use this function instead of reading `persp-nil-name'
directly or guarding it themselves with `boundp'."
  (and (boundp 'persp-nil-name) persp-nil-name))

(defun claude-repl--ws-names-cache ()
  "Return the raw `persp-names-cache' list, or nil when unbound.
Used mainly for diagnostic logging of cache state.  Returns nil both
when the cache is empty and when `persp-names-cache' is unbound
(persp-mode not loaded).

This is the persp-mode names-cache read boundary owned by `workspace.el'.
Callers must use this function instead of reading `persp-names-cache'
directly or guarding it themselves with `boundp'."
  (and (boundp 'persp-names-cache) persp-names-cache))

(defun claude-repl--ws-new (&optional name)
  "Create a new workspace, named NAME when given.
With NAME, delegates to `+workspace-new'.  Without NAME, delegates to
the interactive `+workspace/new', which auto-generates a name (the
caller then reads it back via `--ws-current-name').  No-op when the
corresponding function is unbound (persp-mode not loaded).

This is the persp-mode creation boundary owned by `workspace.el'.
Callers must use this function instead of calling `+workspace-new' or
`+workspace/new' directly or wrapping them with `fboundp'."
  (if name
      (when (fboundp '+workspace-new)
        (+workspace-new name))
    (when (fboundp '+workspace/new)
      (+workspace/new))))

(defun claude-repl--ws-persp-kill (ws)
  "Kill the perspective named WS via the low-level `persp-kill'.
No-op when `persp-kill' is unbound.  Distinct from `--ws-kill'
(`+workspace/kill'): this is the lower-level persp-mode kill used when
the caller has already decided the persp should be dropped.

This is the persp-mode low-level kill boundary owned by `workspace.el'.
Callers must use this function instead of calling `persp-kill' directly
or wrapping it themselves with `fboundp'."
  (when (fboundp 'persp-kill)
    (persp-kill ws)))

(defun claude-repl--ws-remove-buffer (buffer)
  "Detach BUFFER from its perspective via `persp-remove-buffer'.
No-op when `persp-remove-buffer' is unbound (persp-mode not loaded).

This is the persp-mode buffer-detach boundary owned by `workspace.el'.
Callers must use this function instead of calling `persp-remove-buffer'
directly or wrapping it themselves with `fboundp'."
  (when (fboundp 'persp-remove-buffer)
    (persp-remove-buffer buffer)))

;;;; ---- Projectile integration boundary ---------------------------------
;;
;; A claude-repl workspace IS a project (a dir-keyed persp), so projectile
;; is part of the same workspace domain.  These wrappers are the single
;; place claude-repl touches the projectile known-projects API, mirroring
;; the persp-mode boundary above.  Callers outside this file must use
;; these wrappers rather than naming `projectile-*' directly.

(defun claude-repl--ws-register-project (dir)
  "Register DIR with projectile via `projectile-add-known-project'.
No-op when `projectile-add-known-project' is unbound (projectile not
loaded).  DIR should already be normalized by the caller (e.g. via
`file-name-as-directory') when canonical form matters.

Projectile boundary owned by `workspace.el'."
  (when (fboundp 'projectile-add-known-project)
    (projectile-add-known-project dir)))

(defun claude-repl--ws-unregister-project (dir)
  "Drop DIR from projectile's known projects via `projectile-remove-known-project'.
No-op when `projectile-remove-known-project' is unbound (projectile not
loaded).

Projectile boundary owned by `workspace.el'."
  (when (fboundp 'projectile-remove-known-project)
    (projectile-remove-known-project dir)))

(defun claude-repl--ws-switch-project (project)
  "Switch to PROJECT via `projectile-switch-project-by-name'.
No-op when `projectile-switch-project-by-name' is unbound (projectile
not loaded).

Projectile boundary owned by `workspace.el'."
  (when (fboundp 'projectile-switch-project-by-name)
    (projectile-switch-project-by-name project)))

(defun claude-repl--ws-known-projects ()
  "Return the list of projectile-relevant known project roots.
Delegates to `projectile-relevant-known-projects'.  Returns nil when
that function is unbound (projectile not loaded).

Projectile boundary owned by `workspace.el'."
  (when (fboundp 'projectile-relevant-known-projects)
    (projectile-relevant-known-projects)))

;;;; ---- persp-mode load-ordering / hook-registration boundary -----------
;;
;; These installers let callers register persp-mode lifecycle hooks (and
;; run load-deferred setup) without naming the `persp-mode' feature or its
;; hook variables directly.  They are load-time wiring, mirroring the bare
;; `with-eval-after-load' / `add-hook' forms they replace.

(defun claude-repl--ws-add-activated-hook (fn)
  "Register FN to run when a perspective is activated.
Adds FN to `persp-activated-functions' once persp-mode loads.

This is the persp-mode activation-hook boundary owned by `workspace.el'.
Callers must use this function instead of touching `persp-activated-functions'
or `with-eval-after-load' on persp-mode directly."
  (with-eval-after-load 'persp-mode
    (add-hook 'persp-activated-functions fn)))

(defun claude-repl--ws-add-before-deactivate-hook (fn)
  "Register FN to run before a perspective is deactivated.
Adds FN to `persp-before-deactivate-functions' once persp-mode loads.

This is the persp-mode deactivation-hook boundary owned by `workspace.el'.
Callers must use this function instead of touching
`persp-before-deactivate-functions' or `with-eval-after-load' on
persp-mode directly."
  (with-eval-after-load 'persp-mode
    (add-hook 'persp-before-deactivate-functions fn)))

(defun claude-repl--ws-after-system-load (thunk)
  "Call THUNK once the persp-mode workspace system has loaded.
Thin wrapper over `with-eval-after-load' for the persp-mode feature so
callers do not name the feature directly.

Boundary owned by `workspace.el'."
  (with-eval-after-load 'persp-mode
    (funcall thunk)))

(defun claude-repl--ws-run-switch-project-function (dir)
  "Invoke `+workspaces-switch-project-function' on DIR when it is set.
No-op when that variable is unbound or nil.

This is the Doom switch-project-function boundary owned by `workspace.el'.
Callers must use this function instead of reading or funcalling
`+workspaces-switch-project-function' directly."
  (when (and (boundp '+workspaces-switch-project-function)
             +workspaces-switch-project-function)
    (funcall +workspaces-switch-project-function dir)))

(defun claude-repl--ws-advise-kill-before (fn)
  "Install FN as `:before' advice on `+workspace/kill'.
Lets a caller run teardown while the workspace is still current.  This
is load-time wiring registered once at module load.

This is the persp-mode kill-advice boundary owned by `workspace.el'.
Callers must use this function instead of calling `advice-add' on
`+workspace/kill' directly."
  (advice-add '+workspace/kill :before fn))

;;;; ---- persp-mode policy configuration ---------------------------------
;;
;; claude-repl owns workspace/persp policy.  These settings used to live
;; in the top-level config.el and were moved here so the persp boundary
;; owns persp-mode's own configuration.  Deferred until persp-mode loads.

(with-eval-after-load 'persp-mode
  ;; Skip the find-file prompt when switching to a project that already
  ;; has an open workspace; show magit instead when there are no buffers.
  (setq +workspaces-switch-project-function
        (lambda (dir)
          (unless (doom-real-buffer-list)
            (magit-status dir))))
  ;; persp-mode's own session persistence is disabled — claude-repl is the
  ;; single source of truth for workspace save/restore via its snapshot
  ;; mechanism.  -1 disables auto-resume; 0 disables auto-save on kill.
  (setq persp-auto-resume-time -1
        persp-auto-save-opt 0)
  ;; Never prompt when killing a buffer not in the current workspace.
  (setq persp-kill-foreign-buffer-behaviour 'kill)
  ;; Only show current-workspace buffers in buffer lists (SPC ,).
  (setq persp-set-frame-buffer-predicate t))

(defun claude-repl--record-workspace-history (&rest _)
  "Record the current workspace at the front of `claude-repl--workspace-history'.
Removes any prior occurrence of the name so the list stays
most-recently-visited-first with no duplicates.  No-op when there is no
current workspace.  Registered on the persp activation hook below."
  (let ((name (claude-repl--ws-current-name)))
    (when name
      (setq claude-repl--workspace-history
            (cons name (cl-remove name claude-repl--workspace-history
                                  :test #'string=))))))

(claude-repl--ws-add-activated-hook #'claude-repl--record-workspace-history)

(provide 'claude-repl-workspace)
;;; workspace.el ends here
