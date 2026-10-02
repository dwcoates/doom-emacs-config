;;; workspace.el --- Owner of `agent-repl--workspaces' state -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is the sole owner of the `agent-repl--workspaces' hash
;; table (workspace-name -> plist).  It exposes a wrapper API that
;; every other agent-repl source file is expected to use; direct
;; `gethash' / `puthash' / `maphash' against the variable elsewhere
;; is grandfathered for now but will be migrated in a follow-up
;; refactor.  See AGENTS.md ("Workspace state encapsulation").
;;
;; The wrapper API is (current set; expanded incrementally by the
;; render-state unification branch):
;;
;;   - `agent-repl--workspaces'           the hash itself
;;   - `agent-repl--ws-runtime-keys'      keys cleared on tombstone
;;   - `agent-repl--ws-get'                read one plist key
;;   - `agent-repl--ws-plist'              copy the complete known plist
;;   - `agent-repl--ws-rename-state'        atomically move state to a new name
;;   - `agent-repl--ws-rewrite-source-back-refs'
;;                                         rewrite renamed-source references
;;   - `agent-repl--ws-put'                set one plist key (logs stub-create)
;;   - `agent-repl--ws-put-caller-trace'   diagnostic helper for `--ws-put'
;;   - `agent-repl--ws-del'                tombstone (preserves identity)
;;   - `agent-repl--ws-forget'             hard-remove a tombstone (archived first)
;;   - `agent-repl--ws-live-p'             entry exists AND not tombstoned
;;   - `agent-repl--live-ws-names'         live names (no tombstones, no
;;                                          persp-mode pseudo perspectives)
;;   - `agent-repl--ws-registered-names'   all keys (live + tombstoned)
;;   - `agent-repl--ws-project-pollable-p' live entry with project dir
;;   - `agent-repl--ws-project-poll-partition'
;;                                         pollable names + placeholder names
;;
;; Future wrappers introduced by this branch:
;;   - `agent-repl--ws-known-p'            entry exists (live or tombstoned)
;;   - `agent-repl--ws-require-known'      assert known, signal user-error
;;   - `agent-repl--ws-tombstoned-p'       entry exists AND tombstoned
;;   - `agent-repl--ws-open-p'             present in `persp-names-cache'
;;   - `agent-repl--ws-render-status'      single source of truth for what
;;                                          renderers (tab-bar / picker)
;;                                          should display
;;
;; Important non-API details that callers should NOT depend on:
;;
;;   - The hash key is the workspace's persp NAME (a string), but a
;;     workspace's BRANCH name may differ — never use one as a proxy
;;     for the other.  See the discussion above `defvar
;;     agent-repl--workspaces' below.
;;
;;   - `--ws-del' does NOT `remhash'.  It tombstones (stamps
;;     `:killed-at', clears every key in `--ws-runtime-keys').  The
;;     identity keys (`:project-dir', `:created-at', `:ws-dir-hash',
;;     `:source-ws-dir', `:priority', the `:merge-completed*' family)
;;     survive so reverse-lookups, picker sort, and merged-state
;;     rendering keep working past kill.

;;; Code:

;; Cross-file forward declarations.  These sources load in the dependency
;; order config.el establishes and resolve each other's calls at call time,
;; so the declarations below exist for the byte-compiler alone.
(declare-function agent-repl--do-log "core")
(declare-function agent-repl--git-string-quiet "core")
(declare-function agent-repl--info "core")
(declare-function agent-repl--kill-cause-str "core")
(declare-function agent-repl--log "core")
(declare-function agent-repl--log-verbose "core")
(declare-function agent-repl--pseudo-workspace-name-p "core")
(declare-function agent-repl-roster-selection-recency-order "roster" (names))
(declare-function agent-repl-roster-last-selected-ms "roster" (ws))
(declare-function agent-repl--warn "core")
(declare-function agent-repl--ws-dir "status")
(declare-function agent-repl--ws-log-routable-p "core")

(defvar agent-repl--global-log-scope)

(require 'cl-lib)

;; Forward declarations for symbols defined later in the load order (status.el).
;; Workspace.el loads before status.el; these calls fire only at runtime so
;; the cross-file reference is safe, but byte-compile-time would otherwise
;; warn about a free variable / unknown function.
(declare-function agent-repl-roster-drawn-tab-order "roster" ())
(declare-function agent-repl-status-tab-state "status" (ws))
(declare-function agent-repl--priority-rank "agent-repl-status" (priority))
(declare-function agent-repl--state-save "agent-repl-history" (ws))
(declare-function agent-repl--ws-frontend "frontends" (ws))
(declare-function agent-repl-frontend-kill-fn "frontends" (frontend))
(declare-function agent-repl--kill-workspace-buffers "agent-repl-commands" (ws))
(declare-function agent-repl-window--delete-buffer-windows "window" (buf &rest keys))
(declare-function +workspace-exists-p "ext:persp-mode" (name))
(declare-function +workspace--protected-p "ext:persp-mode" (name))
(declare-function persp-frames-with-persp "ext:persp-mode" (&optional persp))
(declare-function persp-other-persps-with-buffer-except-nil "ext:persp-mode" (&optional buff-or-name persp phash del-weak))
(declare-function persp-update-names-cache "ext:persp-mode" (cache))
(declare-function persp-rename "ext:persp-mode" (new-name &optional persp phash))
(declare-function persp-add-new "ext:persp-mode" (name))
(declare-function set-persp-parameter "ext:persp-mode" (parameter value &optional persp))
(declare-function persp-kill "ext:persp-mode" (name))
(declare-function magit-status "ext:magit" (&optional directory cache))
(declare-function agent-repl--magit-status-same-window "agent-repl-magit" (dir))
(declare-function agent-repl--path-canonical "agent-repl-core" (path))
(declare-function agent-repl--log-note-workspace-departing "agent-repl-core" (ws))
(declare-function agent-repl--log-note-workspace-registered "agent-repl-core" (ws))
(declare-function agent-repl--ws-name-for-dir "agent-repl-worktree" (dir))
(declare-function agent-repl--sidebar-push "sidebar" (&optional force))
(declare-function doom-real-buffer-list "ext:doom" (&optional buffer-list))
(declare-function doom-fallback-buffer "ext:doom" ())
(declare-function agent-repl--foreign-owned-buffer-p "agent-repl-core" (buf ws))
(declare-function agent-repl--buffer-owner "agent-repl-core" (buf))
(declare-function agent-repl--safe-buffer-name "window" (b))
;; Defined by core.el, which loads first; declared special here so this file
;; byte-compiles standalone and the binding below is a dynamic one.
(defvar agent-repl--eager-open-in-progress)
(defvar persp-nil-name)
(defvar persp-names-cache)
(defvar persp-mode)
(defvar persp-auto-resume-time)
(defvar persp-auto-save-opt)
(defvar persp-kill-foreign-buffer-behaviour)
(defvar persp-set-frame-buffer-predicate)
(defvar persp-autokill-buffer-on-remove)
(defvar +workspaces-switch-project-function)
(defvar +workspaces-on-switch-project-behavior)
;; Defined by roster.el; declared special here so this file's hook
;; registration byte-compiles standalone (mirrors daemon.el's forward
;; declaration of the same abnormal hook).
(defvar agent-repl-roster-bringup-functions)

(cl-defstruct agent-repl-instantiation
  "Per-environment session state for a Agent REPL workspace.
Each workspace has one instantiation per `agent-repl--environment-keys'
environment, which today means one for :bare-metal."
  session-id    ; Claude Code session ID, captured from the `session_start' hook payload via `agent-repl--update-session-id-from-sentinel'
  start-cmd)    ; last startup command (for logging/display)

;; The hash and its accessors moved here from core.el during the
;; render-state unification refactor.  See file Commentary above for
;; the encapsulation contract.
;;
;; NOTE: workspace name != git branch name.
;; The daemon derives the workspace name from the *last path component* of
;; the requested name (e.g. "DWC/fix-login" -> workspace "fix-login"), while
;; the full request becomes the branch name ("DWC/fix-login").  Never assume
;; the two are equal.  To resolve a workspace to its branch, retrieve its
;; :project-dir from this hash and run `git rev-parse --abbrev-ref HEAD' there.
(defvar agent-repl--workspaces (make-hash-table :test 'equal)
  "Hash table mapping workspace name -> state plist.
Keys: :frontend-buffer :input-buffer
      :agent-state :repl-state
      :worktree-p :project-dir
      :active-env :bare-metal :fork-session-id
      :ready-timer :priority
      :pending-show-panels
:active-env names the workspace's environment (`:bare-metal'), and
:bare-metal is an `agent-repl-instantiation' struct holding that
environment's session state.")

(defvar agent-repl--workspace-history nil
  "Workspace names ordered by most-recently-visited first.
Maintained by `agent-repl--record-workspace-history' on every workspace
activation.  Read by `agent-repl--teardown-landing-target': closing the
workspace the user stands on selects the one selected before it.")

(defvar agent-repl--workspace-log-targets)

(defun agent-repl--ws-forget-emacs-log-target (ws reason)
  "Forget WS's in-memory Emacs log target for REASON.
The target and canonical symlink deliberately remain on disk: their history
is useful after an ordinary workspace teardown.  Forgetting ownership lets a
future workspace reusing WS establish a fresh runtime-owned target."
  ;; SWEPT BY IDENTITY, because that is what the registry is keyed by
  ;; (`agent-repl--workspace-log-targets').  Matching on WS's registered
  ;; directory rather than resolving its identity is deliberate: this runs
  ;; inside `agent-repl--ws-del' and from the rebinding path in
  ;; `agent-repl--ws-put', where a workspace whose worktree has been deleted
  ;; would make `agent-repl--workspace-log-identity' signal — and a teardown
  ;; must not be abortable by a diagnostic sink.  A WS with nothing to match
  ;; simply removes nothing, which is the correct outcome, not a swallowed
  ;; failure.
  (let* ((dir (agent-repl--ws-get ws :project-dir))
         (canonical (and (stringp dir) (agent-repl--path-canonical dir)))
         (stale nil))
    (when canonical
      (maphash (lambda (key entry)
                 (let ((owned (plist-get entry :project-dir)))
                   (when (and (stringp owned)
                              (equal canonical (agent-repl--path-canonical owned)))
                     (push key stale))))
               agent-repl--workspace-log-targets))
    (when stale
      ;; Emit before forgetting so this lifecycle record remains in the
      ;; existing workspace-owned history rather than recreating a target.
      (agent-repl--log ws "ws-forget-emacs-log-target: ws=%s reason=%s targets=%d"
                       ws reason (length stale))
      (dolist (key stale)
        (remhash key agent-repl--workspace-log-targets)))))

(defun agent-repl--ws-get (ws key)
  "Get KEY from workspace WS's plist."
  (plist-get (gethash ws agent-repl--workspaces) key))

(defun agent-repl--ws-plist (ws)
  "Return a shallow copy of the complete state plist for known workspace WS.
WS must name a registered workspace, live or tombstoned; unknown names
signal `user-error' through `agent-repl--ws-require-known'.  The returned
top-level plist is a copy, so consumers such as snapshot serialization may
filter or rewrite it without mutating workspace-owned state.  Values inside
the plist are intentionally shared: this is a state query, not a deep clone
of buffers, processes, or environment structs."
  (agent-repl--ws-require-known ws "ws-plist")
  (let ((plist (gethash ws agent-repl--workspaces)))
    (agent-repl--log-verbose
     ws "ws-plist: ws=%s key-count=%s tombstoned=%s"
     ws (/ (length plist) 2) (if (plist-get plist :killed-at) "t" "nil"))
    (copy-sequence plist)))

(defun agent-repl--ws-rename-state (old-ws new-ws new-project-dir)
  "Atomically move live OLD-WS state to NEW-WS at NEW-PROJECT-DIR.
OLD-WS and NEW-WS must be distinct non-empty strings.  OLD-WS must name
a live registered workspace, and NEW-WS must be unregistered.  All
preconditions and the canonical NEW-PROJECT-DIR are resolved before the
hash is mutated, so a rejected rename leaves both names untouched.

The complete OLD-WS plist is preserved under NEW-WS except that
`:project-dir' is replaced with the canonical new path and cached
`:ws-dir-hash' is cleared for lazy recomputation from that path.  OLD-WS is
then removed, and NEW-WS takes OLD-WS's place in
`agent-repl--workspace-history'.  Returns NEW-WS after the move; invariant
violations signal `user-error'."
  (unless (and (stringp old-ws) (not (string-empty-p old-ws)))
    (agent-repl--log old-ws
                     "ws-rename-state: REJECT old-ws=%S new-ws=%S reason=invalid-old-name"
                     old-ws new-ws)
    (user-error "agent-repl: ws-rename-state: invalid old workspace name %S"
                old-ws))
  (unless (and (stringp new-ws) (not (string-empty-p new-ws)))
    (agent-repl--log old-ws
                     "ws-rename-state: REJECT old-ws=%S new-ws=%S reason=invalid-new-name"
                     old-ws new-ws)
    (user-error "agent-repl: ws-rename-state: invalid new workspace name %S"
                new-ws))
  (when (equal old-ws new-ws)
    (agent-repl--log old-ws
                     "ws-rename-state: REJECT old-ws=%s new-ws=%s reason=identical-names"
                     old-ws new-ws)
    (user-error "agent-repl: ws-rename-state: workspace names are identical: %s"
                old-ws))
  (agent-repl--ws-require-known old-ws "ws-rename-state")
  (unless (agent-repl--ws-live-p old-ws)
    (agent-repl--log old-ws
                     "ws-rename-state: REJECT old-ws=%s new-ws=%s reason=tombstoned"
                     old-ws new-ws)
    (user-error "agent-repl: ws-rename-state: workspace %S is tombstoned"
                old-ws))
  (when (agent-repl--ws-known-p new-ws)
    (agent-repl--log old-ws
                     "ws-rename-state: REJECT old-ws=%s new-ws=%s reason=target-registered target-live=%s"
                     old-ws new-ws
                     (if (agent-repl--ws-live-p new-ws) "t" "nil"))
    (user-error "agent-repl: ws-rename-state: target workspace %S is already registered"
                new-ws))
  (unless (and (stringp new-project-dir)
               (not (string-empty-p new-project-dir)))
    (agent-repl--log old-ws
                     "ws-rename-state: REJECT old-ws=%s new-ws=%s new-project-dir=%S reason=invalid-project-dir"
                     old-ws new-ws new-project-dir)
    (user-error "agent-repl: ws-rename-state: invalid project directory %S"
                new-project-dir))
  (let* ((canonical-dir (agent-repl--path-canonical new-project-dir))
         (old-plist (gethash old-ws agent-repl--workspaces))
         (new-plist (plist-put
                     (plist-put (copy-sequence old-plist)
                                :project-dir canonical-dir)
                     :ws-dir-hash nil)))
    (agent-repl--log old-ws
                     "ws-rename-state: MOVE old-ws=%s new-ws=%s old-project-dir=%S new-project-dir=%s key-count=%d"
                     old-ws new-ws (plist-get old-plist :project-dir)
                     canonical-dir (/ (length new-plist) 2))
    ;; No Lisp call that can signal sits between these primitive hash
    ;; mutations, so observers cannot run against a half-moved entry.
    (puthash new-ws new-plist agent-repl--workspaces)
    (remhash old-ws agent-repl--workspaces)
    ;; The selection history is keyed by name, so the renamed workspace keeps
    ;; its place in it: a rename is not a visit, and losing the entry would
    ;; make a close skip the workspace the user was on before
    ;; (`agent-repl--teardown-landing-target').
    (setq agent-repl--workspace-history
          (cl-substitute new-ws old-ws agent-repl--workspace-history :test #'equal))
    new-ws))

(defun agent-repl--ws-rewrite-source-back-refs
    (old-source-dir new-source-dir)
  "Rewrite workspace source back-references from OLD-SOURCE-DIR to NEW-SOURCE-DIR.
Both paths must be non-empty strings and must canonicalize to distinct
directories.  Every registered workspace whose canonical
`:source-ws-dir' matches OLD-SOURCE-DIR receives the canonical new path
and has its cached `:source-ws-name' cleared, forcing the next source
resolution to discover the renamed workspace identity.

Live and tombstoned entries are both rewritten because source identity
is historical state that must remain correct if a tombstone is later
restored.  Returns the number of rewritten workspaces.  Invalid path
arguments signal `user-error' before any workspace is mutated."
  (unless (and (stringp old-source-dir)
               (not (string-empty-p old-source-dir)))
    (agent-repl--log '(:agent-repl-central "workspace registry operations can span workspaces")
                     "ws-rewrite-source-back-refs: REJECT old-source-dir=%S new-source-dir=%S reason=invalid-old-dir"
                     old-source-dir new-source-dir)
    (user-error "agent-repl: ws-rewrite-source-back-refs: invalid old source directory %S"
                old-source-dir))
  (unless (and (stringp new-source-dir)
               (not (string-empty-p new-source-dir)))
    (agent-repl--log '(:agent-repl-central "workspace registry operations can span workspaces")
                     "ws-rewrite-source-back-refs: REJECT old-source-dir=%S new-source-dir=%S reason=invalid-new-dir"
                     old-source-dir new-source-dir)
    (user-error "agent-repl: ws-rewrite-source-back-refs: invalid new source directory %S"
                new-source-dir))
  (let ((canonical-old (agent-repl--path-canonical old-source-dir))
        (canonical-new (agent-repl--path-canonical new-source-dir))
        (rewritten 0))
    (when (string= canonical-old canonical-new)
      (agent-repl--log '(:agent-repl-central "workspace registry operations can span workspaces")
                       "ws-rewrite-source-back-refs: REJECT old-source-dir=%s new-source-dir=%s reason=identical-canonical-dirs"
                       canonical-old canonical-new)
      (user-error "agent-repl: ws-rewrite-source-back-refs: source directories are identical: %s"
                  canonical-old))
    (maphash
     (lambda (ws plist)
       (let ((source-dir (plist-get plist :source-ws-dir)))
         (cond
          ((null source-dir)
           (agent-repl--log
            ws
            "ws-rewrite-source-back-refs: SKIP ws=%s source-dir=nil old-source-dir=%s reason=no-source"
            ws canonical-old))
          ((not (string=
                 (agent-repl--path-canonical source-dir)
                 canonical-old))
           (agent-repl--log
            ws
            "ws-rewrite-source-back-refs: SKIP ws=%s source-dir=%s old-source-dir=%s reason=different-source"
            ws source-dir canonical-old))
          (t
           (agent-repl--ws-put ws :source-ws-dir canonical-new)
           (agent-repl--ws-put ws :source-ws-name nil)
           (cl-incf rewritten)
           (agent-repl--log
            ws
            "ws-rewrite-source-back-refs: REWROTE ws=%s source-dir=%s -> %s source-ws-name-cleared=t tombstoned=%s"
            ws source-dir canonical-new
            (if (plist-get plist :killed-at) "t" "nil"))))))
     agent-repl--workspaces)
    (agent-repl--log '(:agent-repl-central "workspace registry operations can span workspaces")
                     "ws-rewrite-source-back-refs: DONE old-source-dir=%s new-source-dir=%s rewritten=%d"
                     canonical-old canonical-new rewritten)
    rewritten))

(defun agent-repl--ws-put-caller-trace ()
  "Return a short caller chain string for diagnostic logging.
Used by `agent-repl--ws-put' to identify the producer of stub-create
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
                   (not (memq fn '(agent-repl--ws-put-caller-trace
                                   agent-repl--ws-put
                                   backtrace-frames)))
                   (< (length collected) 8))
          (push fn collected))))
    (if collected
        (mapconcat #'symbol-name (nreverse collected) " <- ")
      "<no-trace>")))

(defun agent-repl--ws-put (ws key val)
  "Set KEY to VAL in workspace WS's plist in `agent-repl--workspaces'.
Internally uses plist-put (which returns a new list) threaded into puthash.

Emits an unconditional log line (via `agent-repl--do-log', bypassing
`agent-repl-debug') when this call CREATES a fresh hash entry whose
plist will lack `:project-dir' — the non-workspace stub shape (a plain
persp such as Doom's default \"main\" auto-vivified by a persp hook).
The workspace renderers (tab-bar, picker) and project-state poller
filter that shape out entirely, so the log line is a producer
diagnostic rather than a user-visible-bug warning.  Includes a
caller trace so the producer can be identified without first turning
debug logging on.

A `:project-dir' write for one of persp-mode's OWN perspectives is
REFUSED — see the body."
  (if (and (eq key :project-dir)
           (agent-repl--pseudo-workspace-name-p ws))
      ;; A BUILT-IN PERSPECTIVE MAY NEVER CLAIM A DIRECTORY.  "none" and
      ;; Doom's "main" are persp-mode's own perspectives, and every reader in
      ;; this file already documents them as live entries that intentionally
      ;; own no `:project-dir'.  A write is what made that documentation
      ;; false: the live registry on 2026-08-11 held
      ;; `main' -> ".../marcos-pr-remediation/" and
      ;; `none' -> ".../slack-cee-ceac-integration-shj/" — the trailing-slash
      ;; shape of a captured `default-directory'.  A pseudo holding a
      ;; directory is LOG-ROUTABLE, so it shadowed the real workspace at that
      ;; path: every durable record written for either of those two
      ;; workspaces named the perspective, and neither workspace had ever
      ;; produced a record of its own.
      ;;
      ;; Refused at the WRITE rather than screened at every read, so the
      ;; shadowing entry never exists to be screened.  Loud rather than
      ;; silent, and the caller trace names the producer.  Deliberately not a
      ;; signal: `agent-repl--ws-current-log-name' documents how a signalling
      ;; persp hook takes `doom-init-ui-hook' down with it, and a rejected
      ;; registration must not be able to do that.
      (agent-repl--do-log
       nil
       "ws-put: REFUSED :project-dir on persp-mode pseudo-perspective ws=%s val=%S — a built-in perspective owns no workspace directory, and one that did would shadow the real workspace at that path in every dir-keyed lookup and every durable log sink. caller-trace=%s"
       (list ws val (or (ignore-errors (agent-repl--ws-put-caller-trace))
                        "<trace-failed>")))
    (agent-repl--ws-put-1 ws key val)))

(defun agent-repl--ws-put-1 (ws key val)
  "Commit KEY to VAL for WS.  See `agent-repl--ws-put', the only caller."
  (let* ((existing (gethash ws agent-repl--workspaces))
         (stub-create (and (null existing) (not (eq key :project-dir))))
         (old-value (plist-get existing key)))
    (when (and (memq key '(:project-dir :ws-dir-hash))
               old-value
               (not (equal old-value val)))
      (agent-repl--ws-forget-emacs-log-target
       ws (format "%s changed from %S to %S" key old-value val)))
    (puthash ws (plist-put (gethash ws agent-repl--workspaces) key val)
             agent-repl--workspaces)
    ;; A `:project-dir' write is a workspace ARRIVING under this name, and
    ;; names are reused.  Whatever the last tenant did — departed on the
    ;; editor's order, spent its one central-fallback announcement — is not
    ;; this workspace's history, so the logging boundary forgets it here.
    (when (eq key :project-dir)
      (agent-repl--log-note-workspace-registered ws))
    (when stub-create
      (let ((trace (or (ignore-errors (agent-repl--ws-put-caller-trace))
                       "<trace-failed>")))
        ;; Emitted with a nil workspace ON PURPOSE.  This record's whole
        ;; subject is that WS owns no `:project-dir', which is exactly the
        ;; state that gives WS no durable log sink; attributing the record to
        ;; WS would ask the ladder to route it somewhere that does not exist,
        ;; and `--note-unroutable-log-workspace' would warn about the very
        ;; condition this line already reports.  The global sink is the
        ;; correct destination, and the name is carried in the message text.
        (agent-repl--do-log
         nil
         "ws-put: STUB-CREATE ws=%s key=%s val=%S — entry created without :project-dir (non-workspace stub; filtered out of workspace renders). caller-trace=%s"
         (list ws key val trace))))))

(defconst agent-repl--ws-runtime-keys
  '(:agent-state :repl-state :input-buffer
    :ready-timer :pending-show-panels
    :fork-session-id :fullscreen-config :active-env :bare-metal
    :deferred-input-queue :done-ack :permission-prompt-active
    :done-ack-pending :source-ws-name
    :frontend-buffer
    :incoming-session-id)
  "Plist keys cleared by `agent-repl--ws-del' when tombstoning a workspace.
Anything not in this list is treated as identity/historical and survives
the tombstone — notably `:project-dir', `:created-at', `:last-killed-at',
`:last-viewed-at', `:priority', `:worktree-p', `:source-ws-dir', `:ws-dir-hash',
and the `:merge-completed*' family.  Preserving `:project-dir' across tombstone
is what lets `agent-repl--ws-dir' callers (magit-status, async git,
ws-dir-hash hashing) keep working on a persp that outlives its agent-repl
session — the failure mode that previously surfaced as
`no :project-dir for workspace X' errors after a kill.")

(defun agent-repl--ws-live-p (ws)
  "Return non-nil iff WS is a live (non-tombstoned) registered workspace.
A workspace is live when it has a hash entry AND no `:killed-at'
tombstone marker.  The single liveness predicate used by every hash
iterator that previously relied on the implicit `presence == live'
invariant (picker, periodic state updater, reverse-lookup) so
tombstoned entries don't surface in any UI/runtime path.

Uses a sentinel default in `gethash' so a registered entry whose plist
happens to be the empty list (`nil') is still counted as present —
distinguishing `key absent' from `key bound to ()'."
  (let ((plist (gethash ws agent-repl--workspaces 'agent-repl--ws-absent)))
    (and (not (eq plist 'agent-repl--ws-absent))
         (null (plist-get plist :killed-at)))))

(defun agent-repl--live-ws-names ()
  "Return the list of live workspace names (hash keys minus tombstones).
Single helper for callers that previously did
`(hash-table-keys agent-repl--workspaces)' as a stand-in for `live
workspaces' — that idiom now over-includes tombstones, so route
through this filter instead.

A PSEUDO PERSPECTIVE IS NEVER A WORKSPACE CANDIDATE ANYWHERE.  persp-mode's
own perspectives — `persp-nil-name' (\"none\") and Doom's startup
perspective (\"main\") — are auto-vivified into the registry by a persp
hook and were live entries here, so every reader of this list carried
them: the `SPC p p' switcher OFFERED \"main\" and \"none\" as workspaces to
switch to, and the link-up walked them.  They are excluded at this source
rather than screened by each reader, so no reader can forget."
  (cl-remove-if-not
   (lambda (ws)
     (and (agent-repl--ws-live-p ws)
          (not (agent-repl--pseudo-workspace-name-p ws))))
   (hash-table-keys agent-repl--workspaces)))

(defun agent-repl--ws-registered-names ()
  "Return every registered workspace name, live and tombstoned.
The result preserves `hash-table-keys' traversal order exactly: it is
not sorted, filtered, or otherwise normalized.  This is the canonical
workspace-owned API for callers that need the complete registration
set rather than the live-only view from `agent-repl--live-ws-names'."
  (let ((names (hash-table-keys agent-repl--workspaces)))
    (agent-repl--log-verbose
     '(:agent-repl-central "workspace registry operations can span workspaces")
     "ws-registered-names: count=%d names=%S"
     (length names) names)
    names))

(defun agent-repl--ws-project-pollable-p (ws)
  "Return non-nil when WS is live and owns a non-nil `:project-dir'.
This is the workspace-layer precondition for project-state polling.
Persp-mode placeholder entries such as \"main\" and \"none\" can be live
hash entries while intentionally lacking `:project-dir'; they are not
agent-repl projects and must never reach git or project-directory
operations."
  (and (agent-repl--ws-live-p ws)
       (agent-repl--ws-get ws :project-dir)))

(defun agent-repl--ws-project-poll-partition ()
  "Return `(POLLABLE . PLACEHOLDERS)' for all live workspace entries.
POLLABLE contains live names satisfying
`agent-repl--ws-project-pollable-p'.  PLACEHOLDERS contains the other
live names, which currently means persp-mode stubs without
`:project-dir'.  The explicit second list lets the periodic poller log
every exclusion instead of silently skipping malformed input."
  (let (pollable placeholders)
    (dolist (ws (agent-repl--live-ws-names))
      (if (agent-repl--ws-project-pollable-p ws)
          (push ws pollable)
        (push ws placeholders)))
    (cons (nreverse pollable) (nreverse placeholders))))

(defun agent-repl--ws-dir-owner (dir &optional except)
  "Return a live workspace (other than EXCEPT) owning canonical DIR, or nil.
Enforces the one-live-workspace-per-`:project-dir' invariant: a second
workspace must not claim a dir a live workspace already owns, since that
shadowing is what lets a stub (e.g. a Doom-auto-named \"#N\" perspective)
collide with the real workspace in `agent-repl--ws-for-dir'."
  (when dir
    (let ((canonical (agent-repl--path-canonical dir)))
      (cl-find-if
       (lambda (ws)
         (and (not (equal ws except))
              (let ((p (agent-repl--ws-get ws :project-dir)))
                (and p (string= canonical (agent-repl--path-canonical p))))))
       (agent-repl--live-ws-names)))))

(defvar agent-repl-ws-del-hook nil
  "Abnormal hook run with WS just before `agent-repl--ws-del' tombstones it.
Runs while the runtime keys (`agent-repl--ws-runtime-keys') are still
readable, so consumers can release external resources keyed on them.
Handlers must not signal: a teardown hook that errors would abort the
kill midway.")

(defun agent-repl--ws-del (ws)
  "Tombstone workspace WS instead of removing its hash entry.
Stamps `:killed-at' with the current time, clears every key in
`agent-repl--ws-runtime-keys' (frontend buffer / proc refs, timers,
session-bound state), and preserves identity/historical keys
(`:project-dir', `:created-at', `:last-killed-at', `:priority',
`:worktree-p', `:source-ws-dir', `:ws-dir-hash', merge metadata).  The entry
remains in `agent-repl--workspaces' so `agent-repl--ws-dir' and
reverse-lookups still resolve, but `agent-repl--ws-live-p' returns
nil and every filtered iterator (picker, periodic updater)
ignores the entry — preserving the prior UX of `kill removes the
workspace from view' without destroying the identity record.

Sweeps peers' cached `:source-ws-name' so a tombstoned WS can never be
returned as a valid parent name.  `:last-killed-at' is bumped here too
so the picker's sort-by-last-killed sees this tombstone immediately.

No-op (beyond the log line) when WS has no hash entry — the bare ws-del
log line preserves the pre-existing diagnostic shape."
  (let ((had-entry (not (null (gethash ws agent-repl--workspaces)))))
    (maphash (lambda (peer plist)
               (when (equal (plist-get plist :source-ws-name) ws)
                 (agent-repl--ws-put peer :source-ws-name nil)))
             agent-repl--workspaces)
    (when had-entry
      ;; Pre-tombstone hook: runs while the runtime keys are still
      ;; readable, so a handler can act on them before the clear below.
      (run-hook-with-args 'agent-repl-ws-del-hook ws)
      (dolist (key agent-repl--ws-runtime-keys)
        (agent-repl--ws-put ws key nil))
      (agent-repl--ws-put ws :last-killed-at (current-time))
      (agent-repl--ws-put ws :killed-at (current-time)))
    ;; Keep this as the operation's final normal log.  Consumers use it as
    ;; the canonical completed-tombstone record after setter advice settles.
    (agent-repl--log ws "ws-del: ws=%s had-entry=%s (tombstone) kill-cause=%s"
                     ws (if had-entry "t" "nil") (agent-repl--kill-cause-str))
    ;; FORGETTING IS THE LAST ACT OF THE TEARDOWN, after every record this
    ;; teardown writes.  Forgetting first left the hook's records, the
    ;; runtime-key clears and the closing `ws-del' line above with no owned
    ;; target, so the very next one minted a FRESH target and re-pointed the
    ;; canonical `<ws>/.claude/emacs/emacs.log' symlink at it -- detaching
    ;; this workspace's entire pre-teardown history from the path a reader
    ;; follows.  Done here, the link still resolves to the target that holds
    ;; the history, and only a record logged for the name AFTER the teardown
    ;; mints a new one.
    (when had-entry
      (agent-repl--ws-forget-emacs-log-target ws "workspace deletion"))))

(defun agent-repl--ws-forget (ws)
  "Hard-remove tombstoned WS from `agent-repl--workspaces'.
The deliberate counterpart to `agent-repl--ws-del': where `--ws-del'
tombstones (preserving the identity record so reverse-lookups and the
revival picker still resolve), this drops the entry outright so it stops
being collected, swept, and re-serialized on every roster write.

Refuses — with an `error', never a silent no-op — when WS is unknown or
still live.  Forgetting a live workspace would strand its persp, buffers
and session with no hash entry to reach them by, so the caller's bug must
surface rather than be absorbed.

The ONLY sanctioned caller is the snapshot tombstone-eviction path
\(`agent-repl--snapshot-evict-expired-tombstones'), which archives the
entry to a loadable snapshot file BEFORE calling this — the identity
record is moved out of the live roster, never destroyed."
  (unless (agent-repl--ws-known-p ws)
    (agent-repl--log ws "ws-forget: REJECT ws=%S reason=unregistered" ws)
    (error "agent-repl: ws-forget: workspace %S is not registered" ws))
  (unless (agent-repl--ws-tombstoned-p ws)
    (agent-repl--log ws "ws-forget: REJECT ws=%S reason=live" ws)
    (error "agent-repl: ws-forget: refusing to forget live workspace %S" ws))
  (remhash ws agent-repl--workspaces)
  (agent-repl--log ws "ws-forget: ws=%s removed=t" ws))

;;;; ---- Membership predicates -------------------------------------------

(defun agent-repl--ws-known-p (ws)
  "Return non-nil iff WS has a hash entry, live or tombstoned.
The membership question without the liveness filter — true for every
ws that has ever been registered and not hard-removed.  Wrappers that
must distinguish unknown from tombstoned (e.g. `--ws-render-status')
call this first to validate that the caller's WS argument refers to a
ws the module knows about at all.

Uses the same `agent-repl--ws-absent' sentinel as `--ws-live-p' so a
ws whose plist happens to be the empty list (`nil') still counts as
present."
  (not (eq (gethash ws agent-repl--workspaces 'agent-repl--ws-absent)
           'agent-repl--ws-absent)))

(defun agent-repl--ws-require-known (ws context)
  "Signal `user-error' unless WS is `--ws-known-p'.
CONTEXT is a short string identifying the caller for the message body,
e.g. `\"ws-render-status\"' or `\"ws-open-p\"'.  Used by wrappers that
contractually refuse to operate on an unknown ws (per the AGENTS.md
no-silent-fallback rule).  Returns nil on success."
  (unless (agent-repl--ws-known-p ws)
    (agent-repl--log '(:agent-repl-central "workspace registration has no durable sink yet")
                     "ws-require-known: REJECT ws=%S context=%s reason=unregistered"
                     ws context)
    (user-error "agent-repl: %s: workspace %S is not registered" context ws)))

(defun agent-repl--ws-revive (ws)
  "Clear WS's tombstone so the name is LIVE again, returning non-nil when it moved.
The counterpart of `agent-repl--ws-del' and the only sanctioned way to
un-tombstone a name.  `--ws-del' does not `remhash' -- it stamps
`:killed-at' -- so a name that is closed and later REOPENED under the
same daemon identity would otherwise stay tombstoned forever: every
filtered iterator, `--ws-live-p', `--live-ws-names' and
`--ws-by-ref-id' skip it, which means the reopened workspace has no tab
and nothing to answer with.  fanout §8 makes a `closed = false' row an
ensure-a-tab, so the reopen path must be able to bring the name back.

Only `:killed-at' is cleared.  `:last-killed-at' SURVIVES on purpose: it
is the historical record of the previous close and the picker sorts on
it; a revive is not a claim the close never happened.  The runtime keys
`--ws-del' cleared stay cleared, because the reopened workspace's
buffers, timers and session state are genuinely gone and the reopen
re-establishes them.

Nothing to revive is not a failure: an unknown name and a live name both
answer nil, so a caller may call this unconditionally before it opens."
  (cond
   ((not (agent-repl--ws-known-p ws))
    ;; An unknown name has no workspace sink by definition.  This is a
    ;; registry-level observation about refusing to create state, so preserve
    ;; the candidate name in the record body and route it explicitly to the
    ;; process-wide sink.
    (agent-repl--log '(:agent-repl-central "workspace registration has no durable sink yet")
                     "ws-revive: SKIP ws=%s reason=unknown" ws)
    nil)
   ((null (agent-repl--ws-get ws :killed-at))
    (agent-repl--log ws "ws-revive: SKIP ws=%s reason=already-live" ws)
    nil)
   (t
    (agent-repl--ws-put ws :killed-at nil)
    (agent-repl--info ws "ws-revive: REVIVED ws=%s last-killed-at-kept=%s"
                      ws (if (agent-repl--ws-get ws :last-killed-at) "t" "nil"))
    t)))

(defun agent-repl--ws-tombstoned-p (ws)
  "Return non-nil iff WS is known AND has `:killed-at' set.
Complementary to `--ws-live-p' over `--ws-known-p': a known ws is
either live or tombstoned, never both.  Unknown ws returns nil.

A tombstone may additionally carry a REASON marker explaining why
the entry was killed, layered on top of the `:killed-at' stamp:

There is exactly ONE reason left: the user's own close.  Client-authored
tab hiding died with the roster becoming the tab bar's source, so a
tombstone no longer carries a reason marker anybody branches on."
  (and (agent-repl--ws-known-p ws)
       (not (null (agent-repl--ws-get ws :killed-at)))))

(defun agent-repl--ws-open-p (ws)
  "Return non-nil iff WS is currently visible in the tab-bar.
\"Open\" means `persp-names-cache' membership — the persp-mode hash
that drives the tab-bar's rendered names.  This is intentionally
DECOUPLED from `agent-repl--workspaces' membership because the two
can legitimately diverge:

  - During snapshot-restore, hash entries exist before
    `persp-add-new' runs.
  - After a successful merge with `preserve-entry', the hash entry
    survives the persp kill so the workspace's merged state stays
    visible to the surviving renderers (e.g. the picker).

Errors via `--ws-require-known' on an unknown ws so the caller never
silently asks about a name the module never heard of.  Returns nil
when `persp-names-cache' is unbound (vanilla Emacs / pre-persp init)."
  (agent-repl--ws-require-known ws "ws-open-p")
  (and (boundp 'persp-names-cache)
       (member ws persp-names-cache)
       t))

(defun agent-repl--ws-list-names ()
  "Return the list of workspace names visible in the tab-bar.
Intersection of `persp-names-cache' membership and
`agent-repl--workspaces' registration, minus persp-mode's own
perspectives (`agent-repl--pseudo-workspace-name-p': \"none\" and Doom's
startup \"main\").  Equivalent to \"all names for which `--ws-open-p' returns
non-nil\" but computed in one pass.

This is the canonical iteration source for any renderer that
enumerates the tab-bar (e.g. `status.el's tabline render functions).
Callers should prefer it over `+workspace-list-names' so agent-repl
never depends on persp-mode's notion of \"all persps\" — only on its
own notion of \"workspaces this module owns\".

Divergence note: a persp in `persp-names-cache' that is NOT
registered in `agent-repl--workspaces' (e.g. one created without
going through agent-repl's establishment path) is excluded.  In
normal agent-repl operation every persp goes through
`--establish-workspace' or `--new-workspace' before reaching the
cache, so the two are equivalent.  Unregistered persps would
previously appear in the tab-bar with no glyph or state coloring;
they now silently drop out — intentional, since the tab-bar is a
agent-repl UI and should reflect agent-repl's worldview.

Returns nil when `persp-names-cache' is unbound."
  (when (boundp 'persp-names-cache)
    (cl-loop for name in persp-names-cache
             when (and (not (agent-repl--pseudo-workspace-name-p name))
                       (agent-repl--ws-known-p name))
             collect name)))

(defun agent-repl--ws-all-names ()
  "Return the raw list of ALL workspace names known to persp-mode.
Delegates to `+workspace-list-names', returning every persp in the
tab-bar regardless of whether agent-repl registered it.  Returns nil
when `+workspace-list-names' is unbound (persp-mode not loaded).

Differs from `agent-repl--ws-list-names', which intersects the cache
with `agent-repl--workspaces' to yield only agent-repl-owned names.
Use THIS wrapper for uniqueness checks and any path that must observe
persps agent-repl did not create.  Use `--ws-list-names' for
renderers that should reflect only agent-repl's own workspaces.

This is the persp-mode namespace boundary owned by `workspace.el'.
Callers must use this function instead of calling `+workspace-list-names'
directly or wrapping it themselves with `fboundp'."
  (when (fboundp '+workspace-list-names)
    (+workspace-list-names)))

;;;; ---- Repo grouping + folding -----------------------------------------
;;
;; A "repo" here is the set of workspaces that share a git common-dir —
;; a top-level clone plus every worktree cut from it.  A repo can be
;; FOLDED: its workspaces vanish from the tab-bar, and the tab-bar's
;; 1-based selection numbers close up over the survivors so `SPC <n>'
;; stays contiguous.  `agent-repl--toggle-repo-fold' is the model-level
;; toggle; no interactive fold-toggling UI currently exists.
;;
;; The fold set lives in workspace.el because it is read by two layers
;; — the tab-bar render (status.el) and the indexed workspace switchers
;; (commands.el) — and workspace.el is the canonical owner of "which
;; workspaces does the UI enumerate".  It is deliberately in-memory
;; only: folding is a view preference, not workspace state, so it does
;; not round-trip through the workspace snapshot.

(defconst agent-repl--repo-key-unknown "(no repo)"
  "Repo key used when a workspace's git common-dir cannot be resolved.
Every such workspace shares this one key, so they group — and fold —
together.")

(defun agent-repl--repo-key-for-dir (dir)
  "Return the repo key (canonical git common-dir) for directory DIR.
Pure derivation with no workspace-plist caching: shells out to git via
`agent-repl--git-string-quiet' on every call.  Returns nil when DIR is
nil or git fails on it (deleted directory, not a repository) — a
documented lookup-or-nil contract, since \"no repo\" is an expected
state the caller maps onto `agent-repl--repo-key-unknown'.

`agent-repl--ws-repo-key' layers the `:group-key' plist cache on top
for workspaces; callers with only a directory (e.g. the sidebar
roster's snapshot-only entries, which have no plist to cache on) use
this directly and own their own caching."
  (when-let* ((raw (and dir (agent-repl--git-string-quiet
                             "-C" dir "rev-parse" "--git-common-dir"))))
    (when (and (not (string-empty-p raw))
               (not (string-prefix-p "fatal" raw)))
      (let ((abs (if (file-name-absolute-p raw) raw
                   (expand-file-name raw dir))))
        (agent-repl--path-canonical abs)))))

(defun agent-repl--ws-repo-key (ws)
  "Return WS's repo key: the canonical git common-dir of its project-dir.
Cached on the workspace plist as `:group-key' so each workspace shells
out to git at most once.  Returns nil when git fails on WS's
project-dir — e.g. the worktree directory was deleted out from under
the workspace.  Callers that need a total function should use
`agent-repl--ws-repo-group', which maps that nil onto
`agent-repl--repo-key-unknown'."
  (or (agent-repl--ws-get ws :group-key)
      (when-let* ((dir (ignore-errors (agent-repl--ws-dir ws)))
                  (key (agent-repl--repo-key-for-dir dir)))
        (agent-repl--ws-put ws :group-key key)
        key)))

(defun agent-repl--ws-repo-group (ws)
  "Return WS's fold-group key, never nil.
`agent-repl--ws-repo-key' when git resolves the repo, otherwise the
`agent-repl--repo-key-unknown' sentinel."
  (or (agent-repl--ws-repo-key ws) agent-repl--repo-key-unknown))

(defvar agent-repl--folded-repos (make-hash-table :test 'equal)
  "Set of repo keys (see `agent-repl--ws-repo-group') currently folded.
Keys are repo keys, values are `t' — presence is the signal.  Global
rather than per-buffer: a fold is a statement about the repo, and it
must be observable by the tab-bar renderer and the indexed workspace
switchers alike.")

(defun agent-repl--repo-folded-p (group)
  "Return non-nil when repo GROUP (a repo key) is folded."
  (and group (gethash group agent-repl--folded-repos) t))

(defun agent-repl--toggle-repo-fold (group)
  "Toggle the fold state of repo GROUP (a repo key).
Returns non-nil when GROUP is folded after the toggle."
  (unless group
    (error "agent-repl--toggle-repo-fold: nil repo group"))
  (if (gethash group agent-repl--folded-repos)
      (progn (remhash group agent-repl--folded-repos) nil)
    (puthash group t agent-repl--folded-repos)
    t))

(defun agent-repl--ws-tabline-names ()
  "Return the workspace names the tab-bar shows, in ROSTER ORDER.
The roster is the tab bar's one source: `roster.el' walks the pushed
view (repository sections in order, rows depth-first, then recently
merged) and records that order, and this function follows it strictly.

CLIENT-AUTHORED ORDERING AND HIDING ARE DEAD.  The resolver orders the
roster — priority included — so a local re-sort would be a second answer
to a question the daemon already answered, and a local filter (the old
merged-tab hiding) would hide a workspace the daemon says is open.  The
one hiding left is the DAEMON'S: a repository it holds collapsed
\(frontend.v1.RosterRepoSection.fold, owner ruling 2026-10-01) keeps its
workspaces' tabs but draws none of them, which is
`agent-repl-roster-drawn-tab-order'.

Names the roster lists but the perspective layer has not caught up with
are dropped: the tab bar can only render tabs that exist.  Before the
first push the order is empty, and the workspaces Emacs knows about are
rendered in their registration order so a pre-roster boot still draws."
  (let ((order (and (fboundp 'agent-repl-roster-drawn-tab-order)
                    (agent-repl-roster-drawn-tab-order)))
        (known (agent-repl--ws-list-names)))
    (if (null order)
        (progn
          (agent-repl--log-verbose
           '(:agent-repl-central "tab rendering spans every workspace")
           "ws-tabline-names: no roster order yet count=%d" (length known))
          known)
      (let ((names (cl-remove-if-not (lambda (name) (member name known)) order)))
        (agent-repl--log-verbose
         '(:agent-repl-central "tab rendering spans every workspace")
         "ws-tabline-names: roster-order=%d rendered=%d"
         (length order) (length names))
        names))))

(defun agent-repl--ws-by-ref-id (id)
  "Return the live workspace whose `WorkspaceRef' id is ID, or nil.
THE REF ID IS THE TAB IDENTITY: it is daemon-minted, opaque and compared
byte-wise, so a workspace is found by it and never by its display name
\(names collide across repos) or its directory (paths have many
spellings).  Tombstoned entries are excluded — a tombstone has no tab,
so answering with one would resurrect a closed workspace."
  (let ((found nil))
    (maphash (lambda (name plist)
               (when (and (null found)
                          (null (plist-get plist :killed-at))
                          (equal (plist-get (plist-get plist :ref) :id) id))
                 (setq found name)))
             agent-repl--workspaces)
    (agent-repl--log-verbose
     (if found
         found
       '(:agent-repl-central "an unmatched roster reference has no workspace"))
     "ws-by-ref-id: id=%s ws=%S" id found)
    found))

;;;; ---- Render state: the roster row's status arm ---------------------
;;
;; THE ROSTER IS THE ONE SOURCE.  `WatchWorkspaceRoster' carries a status
;; arm per row, the daemon resolves it (its own lifecycle coarsened onto
;; that vocabulary), and the ARM IS THE STATE a renderer paints.  Emacs
;; derives nothing: the old local precedence ladder, the `:agent-state' /
;; `:repl-state' axes and the `:pushed-render-state' key that briefly
;; replaced them are all gone, and there is exactly one status mechanism.

(defun agent-repl--ws-render-status (ws)
  "Return the roster status arm keyword for workspace WS.
The SINGLE SOURCE OF TRUTH for what renderers (tab bar, picker) draw for
a workspace.  Every renderer reads this; none re-derives status.

Precondition: WS must be `--ws-known-p'.  Unknown WS signals `user-error'
via `--ws-require-known' — there is no silent fallback.

Returns:

  nil  — a TOMBSTONED workspace, or one the roster has not spoken about
         yet.  Both are honestly \"no state to draw\": a tombstone is a
         workspace closed locally, and a workspace with no row is one the
         daemon has not reported on.  Renderers draw an uncoloured tab
         for either rather than inventing an arm.

  otherwise — the arm keyword verbatim, one of
         `agent-repl-wire-roster-row-status-keywords'."
  (agent-repl--ws-require-known ws "ws-render-status")
  (if (agent-repl--ws-tombstoned-p ws)
      nil
    (and (fboundp 'agent-repl-status-tab-state)
         (agent-repl-status-tab-state ws))))

(defcustom agent-repl-ws-state-icons
  '((:init           . "⏳")
    ;; 💤 belongs to HIBERNATED: it is the glyph for asleep-on-purpose, and it
    ;; was always the wrong glyph for the broken half of the old `:dormant'.
    ;; SEVERED gets the unplugged cord.  The substrate between us and the shim
    ;; is gone, which is a different thing from a nap, and the glyph is the only
    ;; distinction the tab-bar carries once the color says "something is wrong".
    (:severed        . "🔌")
    ;; SUBMITTING gets the outbound envelope: the prompt is on its way to the
    ;; agent and the agent has not started on it, which the hourglass would
    ;; overstate.
    (:submitting     . "📤")
    (:thinking       . "⌛")
    (:clearing       . "🧹")
    (:compacting     . "🗜")
    (:done           . "✅")
    (:ready          . "✅")
    (:idle           . "✅")
    (:interrupted    . "✋")
    (:turn-failed    . "⚠")
    (:idle-async     . "🌙")
    (:permission     . "❓")
    (:vendor-blocked . "⛔")
    (:api-retrying   . "🔁")
    (:start-failed   . "🚫")
    (:dead           . "❌")
    (:degraded       . "📡")
    (:merged         . "🔀")
    (:merge-failed   . "⛔")
    (:merging        . "🔄")
    (:merge-queued   . "🕒"))
  "Alist mapping a render-state keyword to its indicator glyph.
The glyph half of the render-state unification: renderers resolve a
workspace's state through `agent-repl--ws-render-status' and look the
resulting keyword up here, so every renderer shows the same emoji for
a given workspace.  Keys are the closed set `--ws-render-status'
returns (see its docstring for each state's meaning and precedence).
Unrecognized values fall through to
`agent-repl-ws-state-icon-default', used for workspaces registered
but with no live session."
  :type '(alist :key-type symbol :value-type string)
  :group 'agent-repl)

;; Force-apply the latest palette on every (re)load.  `defcustom' only
;; initializes the value when the symbol is unbound, so palette tweaks
;; otherwise require an Emacs restart to take effect.  Source is the
;; canonical palette in this personal config; `M-x customize' values
;; for this variable will be overwritten on reload.
(setq agent-repl-ws-state-icons
      (eval (car (get 'agent-repl-ws-state-icons 'standard-value))))

(defcustom agent-repl-ws-state-icon-default "·"
  "Glyph shown when a workspace has no recognized render-state.
Used for registered-but-not-yet-started workspaces (render-status nil)."
  :type 'string
  :group 'agent-repl)

;;;; ---- Persp-mode integration boundary ---------------------------------
;;
;; The functions below are the ONLY place inside agent-repl that touches
;; persp-mode internals (`persp-names-cache', `persp-kill',
;; `+workspace-exists-p', `persp-update-names-cache', etc).  See AGENTS.md
;; "NEVER manipulate third-party internals from a high-level layer" — the
;; wrappers in this section ARE the integration boundary they describe.
;; Callers in `commands.el', `status.el', etc. must route
;; through these, not poke persp-mode directly.

(defconst agent-repl--unfinished-merge-states
  '(:merging :merge-queued)
  "Roster status arms that mean WS's merge has NOT reached a verdict.

`:merged' and `:merge-failed' are absent deliberately: both are terminal,
and a workspace sitting on either is finished with the daemon and free to
be torn down.  Everything here is a merge the daemon is still carrying —
running it (conflict resolution and test repair included) or holding it
behind a sibling in its repository's queue.")

(defun agent-repl--ws-merge-unfinished-p (ws)
  "Return non-nil when WS's ROSTER STATUS ARM is an unfinished merge.
The answer is the daemon's verdict rather than anything Emacs derived:
merging is daemon-owned and Emacs holds no merge state of its own to
consult.  Read through `agent-repl-status-tab-state' rather than
`agent-repl--ws-render-status' so the predicate stays answerable for a
workspace the registry no longer calls known."
  (memq (and (fboundp 'agent-repl-status-tab-state)
             (agent-repl-status-tab-state ws))
        agent-repl--unfinished-merge-states))

(defun agent-repl--assert-mergeable-teardown (ws)
  "SIGNAL `user-error' when WS may not be torn down yet.
The one merge responsibility Emacs still carries: a workspace whose merge
has not reached a verdict must not have its session, buffers, or
perspective killed underneath the daemon.  Tearing one down mid-merge
kills the very session the merge lease is driving the conflict resolution
through, and the merge then fails against a worktree nobody is watching.

Refuses loudly (logged, then signalled) rather than deferring or
no-opping: a caller that asked to kill a merging workspace asked for
something the system cannot honour, and a silent skip would read to the
user as a completed teardown."
  (when (agent-repl--ws-merge-unfinished-p ws)
    (let ((state (agent-repl-status-tab-state ws)))
      (agent-repl--log ws
                        "kill-one-workspace: REFUSED ws=%s state=%s — merge not finished"
                        ws state)
      (user-error
       "Cannot tear down workspace '%s': its merge is still %s"
       ws (substring (symbol-name state) 1)))))

(defun agent-repl--kill-one-workspace (ws &optional preserve-entry)
  "Tear down a single agent-repl workspace WS without prompting.

REFUSES (via `agent-repl--assert-mergeable-teardown') any workspace whose
daemon-pushed state is an unfinished merge, before any teardown step runs
so no partial teardown is left behind.
Kills any in-flight git-diff process, tears down the agent session
and buffers, removes WS from `agent-repl--workspaces', kills every
remaining buffer (and attached process) that belongs to the persp via
`agent-repl--kill-workspace-buffers', and finally lands the user and
kills the persp workspace via `agent-repl--ws-land-then-kill'.  Designed
to be reusable from
`agent-repl-kill-workspace' (one-shot),
`agent-repl-kill-all-workspaces' (loop), and
`agent-repl-close-workspace'.

When PRESERVE-ENTRY is non-nil, the `agent-repl--workspaces' hashmap
entry is retained — every other teardown step runs as usual (agent
session, buffers, persp), but the ws plist survives so the workspace's
merged state stays visible to the surviving renderers until the user
explicitly `finish'es it.  This is the merge-completed teardown path;
standard kill/close callers pass nil and the entry is dropped.

Persisted state (`<project>/.claude/emacs/state.el', including the
captured per-environment session-id) is ALWAYS preserved — kill is
purely an in-memory teardown.  An explicit `--state-save' runs at the
top of the function so the file reflects the latest in-memory state
even if downstream teardown errors before the redundant state-save in
`--teardown-session-state' can fire.

The hashmap removal (`ws-del') runs inside an `unwind-protect' cleanup
so it always happens, even when the frontend kill dispatch errors
partway through.  The persp kill is the very last step so all internal
state is already cleaned up before the UI workspace disappears.
Callers can rely on the post-condition: after the call returns \(or
throws), WS is not in
`agent-repl--workspaces' (unless PRESERVE-ENTRY was non-nil) and its
on-disk state.el is up-to-date.

REFUSES (via `agent-repl--assert-persp-killable'), before any teardown
step runs, a workspace whose perspective cannot be killed: one visible in
another frame, or persp-mode's protected nil perspective.

This function is part of the persp-mode integration boundary owned
by `workspace.el' (see file Commentary and AGENTS.md).  It never calls
Doom's `+workspace/kill': see `agent-repl--ws-land-then-kill' for why."
  (agent-repl--assert-mergeable-teardown ws)
  (agent-repl--assert-persp-killable ws)
  ;; The teardown is under way, so every record it writes from here on is a
  ;; departing workspace's own, and a directory that has gone with it is not
  ;; a stale registry row.  Declared AFTER the mergeable assertion: a REFUSED
  ;; teardown leaves the workspace standing, and a standing workspace has
  ;; ordered no departure.
  (agent-repl--log-note-workspace-departing ws)
  (agent-repl--log ws "kill-one-workspace: ENTRY ws=%s preserve-entry=%s kill-cause=%s cache=%S"
                    ws (if preserve-entry "t" "nil")
                    (agent-repl--kill-cause-str)
                    (if (boundp 'persp-names-cache) persp-names-cache "(unbound)"))
  ;; Stamp the kill timestamp before the pre-teardown state-save so the
  ;; on-disk state.el reflects this kill.  The project picker
  ;; (`agent-repl-switch-to-project') reads `:last-killed-at' to sort
  ;; entries (most-recently-killed first) and to color the kill-date
  ;; column.  Recorded on the ws plist so the immediately-following
  ;; `--state-save' picks it up via `agent-repl--ws-get'.
  (agent-repl--ws-put ws :last-killed-at (current-time))
  ;; Save first, before any teardown touches the ws plist or risks
  ;; erroring.  The teardown path also calls state-save, but wrapping
  ;; ours up front guarantees preservation even if a downstream step
  ;; signals before that secondary save can run.  Wrapped in
  ;; condition-case so a save error doesn't abort the kill itself.
  (condition-case err
      (agent-repl--state-save ws)
    (error (agent-repl--warn ws "kill-one-workspace: pre-teardown state-save error: %S" err)))
  (unwind-protect
      (progn
        (agent-repl--log ws "kill-one-workspace: calling frontend kill-fn ws=%s" ws)
        (condition-case err
            (funcall (agent-repl-frontend-kill-fn (agent-repl--ws-frontend ws)) ws)
          (error (agent-repl--warn ws "kill-one-workspace: frontend kill-fn error: %S" err)))
        (agent-repl--log ws "kill-one-workspace: frontend kill-fn returned ws=%s" ws))
    ;; Cleanup: always remove the hashmap entry regardless of any error
    ;; in the steps above (unless PRESERVE-ENTRY was requested).
    ;; Persisted state.el is intentionally NOT touched here — see the
    ;; docstring.
    (if preserve-entry
        (agent-repl--log ws "kill-one-workspace: ws-del decision=preserve-entry")
      (agent-repl--log ws "kill-one-workspace: ws-del decision=tombstone")
      (condition-case err
          (agent-repl--ws-del ws)
        (error (agent-repl--warn ws "kill-one-workspace: ws-del error: %S" err))))
    ;; NO RESTORE-BATCH BOOKKEEPING.  This used to drop WS from
    ;; `agent-repl--restored-workspaces' so a follow-up
    ;; `kill-restored-workspaces' would not re-tear-down a stale name.
    ;; Both the batch and that command died with the snapshot restore:
    ;; THE DAEMON is the source of which workspaces exist, and Emacs opens
    ;; tabs from the roster stream rather than replaying a saved batch.
    ;; Kill every remaining buffer (and attached process) that belongs to
    ;; the persp before tearing down the persp itself.  The frontend kill
    ;; dispatch only handles the webview/input panels it tracks in the
    ;; hashmap; this sweep catches file buffers, magit buffers, auxiliary shells,
    ;; or anything else the user opened while inside the workspace so
    ;; nothing is orphaned after the persp goes away.
    (agent-repl--log ws "kill-one-workspace: calling kill-workspace-buffers ws=%s" ws)
    (condition-case err
        (agent-repl--kill-workspace-buffers ws)
      (error (agent-repl--warn ws "kill-one-workspace: kill-workspace-buffers error: %S" err)))
    (agent-repl--log ws "kill-one-workspace: kill-workspace-buffers returned ws=%s" ws)
    ;; Land, then kill the persp workspace -- last, so all internal state
    ;; is already cleaned up before the UI workspace disappears.  The one
    ;; teardown order lives in `agent-repl--ws-land-then-kill': when WS is
    ;; the workspace the user stands on, the user is switched to the
    ;; landing workspace FIRST, and only then is WS -- no longer current --
    ;; killed, so the kill can never reach the landing workspace's windows.
    (condition-case err
        (agent-repl--ws-land-then-kill ws)
      (error (agent-repl--warn ws "kill-one-workspace: land-then-kill error: %S" err)))
    ;; The workspace is now gone from both the hash (tombstoned, unless
    ;; PRESERVE-ENTRY) and the tab bar, so its sidebar row is gone too.
    ;; Repaint immediately rather than letting the row outlive its tab
    ;; until the 1Hz signature tick notices.
    (agent-repl--ws-repaint-sidebar ws "kill-one-workspace")
    (agent-repl--log ws "kill-one-workspace: DONE ws=%s all-cleanup-complete" ws)))

(defun agent-repl--reorder-workspace-to-front (ws)
  "Move workspace WS to the front of `persp-names-cache' (visible portion).
The visible portion is everything after `persp-nil-name' when that
variable is bound (persp-mode keeps its sentinel persp at the head);
WS is inserted as the first element of the visible portion so it
shows up as the leftmost tab.

Mirrors `agent-repl--reorder-workspace-by-priority' in structure:
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
passing for an already-merged workspace."
  (let ((cache-snapshot (if (boundp 'persp-names-cache) persp-names-cache "(unbound)")))
    (agent-repl--log ws "reorder-workspace-to-front: ENTRY ws=%s cache=%S"
                      ws cache-snapshot)
    (cond
     ((not (boundp 'persp-names-cache))
      (agent-repl--log ws "reorder-workspace-to-front: BAIL ws=%s reason=cache-unbound" ws))
     ((not (member ws persp-names-cache))
      (agent-repl--log ws "reorder-workspace-to-front: BAIL ws=%s reason=not-in-cache cache=%S"
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
        (agent-repl--log ws "reorder-workspace-to-front: APPLY ws=%s canonical-eq-input=%s new-cache=%S"
                          ws (if (eq canonical-ws ws) "t" "nil") new-cache)
        (agent-repl--ws-update-names-cache new-cache))))))

;;;; ---- persp-mode identity / navigation boundary -----------------------
;;
;; These thin wrappers insulate callers from the +workspace-* API names so
;; (a) the fboundp guards are written once, in one place; (b) tests stub a
;; single symbol instead of juggling `fboundp' and the real function; and
;; (c) future persp-mode API changes require only a local edit here.

(defun agent-repl--ws-resolve-persp (ws)
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

(defun agent-repl--ws-system-available-p ()
  "Return non-nil when the persp-mode workspace system is active.
Specifically, returns non-nil when the variable `persp-mode' is both
bound and non-nil — i.e. the same test as `(bound-and-true-p persp-mode)'.

Use this predicate instead of calling `bound-and-true-p' on `persp-mode'
directly.  The single definition here is the workspace.el integration
boundary for system-availability checks (see AGENTS.md), so a future
change in how availability is detected requires only a local edit."
  (bound-and-true-p persp-mode))

(defun agent-repl--ws-current-name ()
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

(defun agent-repl--ws-log-name (ws)
  "Return WS when it owns a durable log sink, else nil for the global sink.
The name-taking form of `agent-repl--ws-current-log-name', for the callers
that hold a workspace name they did not read from ambient state — a persp
name captured at hook-fire time and carried through a timer, a name passed
down as a function argument.

Use it wherever a name of persp-mode provenance reaches the logging ladder.
persp-mode's own perspectives (`persp-nil-name' \"none\", Doom's initial
\"main\") are live perspectives that are not agent-repl workspaces and own no
`:project-dir', so attributing a record to one asks the ladder to route to a
sink that does not exist.  Passing nil instead routes the record to the
global sink, which is where a line about a non-workspace belongs; keep the
name in the message text so the record still says which perspective it is
about.

This never suppresses `agent-repl--note-unroutable-log-workspace': a
workspace that genuinely should own a sink and does not still reaches the
ladder unscreened from its own call sites and still warns."
  (and (agent-repl--ws-log-routable-p ws) ws))

(defun agent-repl--ws-current-log-name ()
  "Return the current workspace name, or nil when it owns no log sink.
`agent-repl--ws-current-name' answers a persp-mode question, and persp-mode
answers with its placeholder perspectives too: outside any agent-repl
workspace the current perspective is `persp-nil-name' (\"none\"), and Doom's
initial perspective is \"main\".  Both are real perspectives that are not
agent-repl workspaces — `agent-repl--ws-project-pollable-p' already
documents them as live entries that intentionally own no `:project-dir'.

Logging must not mistake them for workspaces.  The ladder's WS argument
selects a durable per-workspace sink, so a name owning no sink makes
`agent-repl--workspace-log-identity' signal — and because every level of the
ladder resolves that identity, a mere debug line would then abort its
caller.  That is how a persp-activated hook took `doom-init-ui-hook' down
with it.

nil is the correct answer rather than a fallback: a diagnostic emitted while
no workspace is current IS a global-scope line, which is exactly what nil
routes it to.  Use this instead of `agent-repl--ws-current-name' whenever
the value is destined for the logging ladder; the behavioral callers that
switch, compare, or resolve directories keep using the unscreened name."
  (agent-repl--ws-log-name (agent-repl--ws-current-name)))

(defun agent-repl--ws-switch (ws &rest args)
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

;;;; ---- Transient activation of a background workspace --------------------

(defun agent-repl--restore-focus (orig-persp orig-window orig-buffer)
  "Restore perspective to ORIG-PERSP and select ORIG-WINDOW / ORIG-BUFFER.
Helper for `agent-repl--with-preserved-focus' — kept as a separate defun
so the restoration logic is observable in tests via `cl-letf' and the
macro body stays small.

Each restore step is a no-op when the corresponding state has not
drifted from its captured value, so a body that did not change focus
does not pay for redundant switch / `select-window' / `set-buffer'
calls.  A failure in the switch back is RECORDED and not re-signaled:
the macro's job is best-effort focus restoration, and a body that
already succeeded must not be turned into a failure by the unwind."
  (when (and orig-persp
             (not (equal orig-persp (agent-repl--ws-current-name))))
    (condition-case err
        (agent-repl--ws-switch orig-persp)
      (error
       (agent-repl--warn
        (if (agent-repl--ws-log-routable-p orig-persp)
            orig-persp
          '(:agent-repl-central "a foreign perspective has no workspace sink"))
        "restore-focus: switch back to %s failed err=%S"
                         orig-persp err))))
  (when (and (window-live-p orig-window)
             (not (eq orig-window (selected-window))))
    (select-window orig-window 'norecord))
  (when (and (buffer-live-p orig-buffer)
             (not (eq orig-buffer (current-buffer))))
    (set-buffer orig-buffer)))

(defmacro agent-repl--with-preserved-focus (&rest body)
  "Run BODY while preserving the caller's active workspace + window + buffer.
Captures `(agent-repl--ws-current-name)', `(selected-window)', and
`(current-buffer)' before BODY runs, then restores all three afterward
through an `unwind-protect' even when BODY signals.

Used to wrap the side effects of building a workspace in the background
so any internal focus change stays invisible to the user.  Restoration
delegates to `agent-repl--restore-focus' so tests can observe the
contract by stubbing that defun."
  (declare (indent 0) (debug t))
  (let ((orig-persp-sym (make-symbol "orig-persp"))
        (orig-window-sym (make-symbol "orig-window"))
        (orig-buffer-sym (make-symbol "orig-buffer")))
    `(let ((,orig-persp-sym (agent-repl--ws-current-name))
           (,orig-window-sym (selected-window))
           (,orig-buffer-sym (current-buffer)))
       (unwind-protect
           (progn ,@body)
         (agent-repl--restore-focus
          ,orig-persp-sym ,orig-window-sym ,orig-buffer-sym)))))

(defun agent-repl--clean-frame-foreign-windows (ws)
  "Delete frame windows whose buffer is owned by a workspace other than WS.
A window is foreign iff `agent-repl--foreign-owned-buffer-p' says its
buffer belongs to a DIFFERENT workspace.  Buffers with no owning
workspace (files, dashboard, scratch, fallback) are workspace-agnostic
and are left alone.

Strips `no-delete-other-windows' and dedication from foreign windows
first, so a prior workspace's agent-repl panel windows — which carry
both — can be torn down.  When EVERY frame window is foreign, all but
one are deleted and that one's buffer is swapped to the Doom fallback
buffer, so WS starts from a clean single-window layout instead of
inheriting the previous workspace's window configuration."
  (let* ((fallback (and (fboundp 'doom-fallback-buffer) (doom-fallback-buffer)))
         (all (window-list nil 'nomini))
         (foreign (cl-remove-if-not
                   (lambda (w)
                     (agent-repl--foreign-owned-buffer-p (window-buffer w) ws))
                   all))
         (any-native (< (length foreign) (length all))))
    (if (null foreign)
        (agent-repl--log ws "clean-frame-foreign-windows: ws=%s no foreign windows total=%d"
                         ws (length all))
      (agent-repl--log ws "clean-frame-foreign-windows: ws=%s removing=%d total=%d"
                       ws (length foreign) (length all))
      (dolist (win foreign)
        (set-window-parameter win 'no-delete-other-windows nil)
        (set-window-dedicated-p win nil))
      (cond
       (any-native
        (dolist (win foreign)
          (ignore-errors (delete-window win))))
       (t
        (dolist (win (cdr foreign))
          (ignore-errors (delete-window win)))
        (when (and fallback (car foreign) (window-live-p (car foreign)))
          (set-window-buffer (car foreign) fallback)))))))

(defun agent-repl--call-in-background-workspace (ws fn)
  "Call FN with WS transiently activated, then restore the caller's focus.

THE single anchor every \"do this IN WS's perspective, whoever is looking
at whatever\" caller goes through — today the gui frontend's webview
mount and pre-creation (frontend.el).

Anchoring on WS rather than on the caller's timing is what makes a
mis-mount unrepresentable instead of merely unlikely: the switch-in
happens at the moment FN runs, so no amount of user switching in between
can land WS's panels in someone else's frame.

The switch-in is skipped when WS is ALREADY current, and
`agent-repl--restore-focus' then restores nothing, so the anchor costs
the foreground path no perspective traffic at all.

`agent-repl--eager-open-in-progress' is bound around the whole dance so
the activation-reactive hooks that must not fire for a background
workspace are suppressed — see that variable's docstring.  Those hooks
only run on a real activation, so the binding is inert when WS is
already current.

Activating WS is NOT by itself enough to give FN a frame of WS's own:
activating a perspective that has never saved a window configuration
leaves the frame showing the PREVIOUS workspace's windows, and FN would
build into them.  `agent-repl--clean-frame-foreign-windows' therefore
runs before FN — even when WS was already current, since the invariant
FN depends on is about the FRAME's contents, not about whether a switch
happened."
  (let ((agent-repl--eager-open-in-progress t))
    (agent-repl--with-preserved-focus
      (unless (equal ws (agent-repl--ws-current-name))
        (agent-repl--ws-switch ws))
      (agent-repl--clean-frame-foreign-windows ws)
      (funcall fn))))

(defun agent-repl--ws-repaint-sidebar (ws reason)
  "Push a fresh sidebar roster after WS left the tab bar, tagged REASON.
Killing a perspective REMOVES that workspace's sidebar row
\(`agent-repl--sidebar-rostered-p'), and a removal the user triggered
must land at once rather than waiting on the 1Hz signature tick — the
row would otherwise linger for up to a second after its tab vanished.
Guarded with `fboundp' and `condition-case': the repaint is a courtesy
on top of the tick that would eventually notice anyway, so it must
never turn a teardown into an error."
  (if (not (fboundp 'agent-repl--sidebar-push))
      (progn
        (agent-repl--log ws
                         "ws-repaint-sidebar: skip ws=%s reason=%s (sidebar not loaded)"
                         ws reason)
        nil)
    (agent-repl--log ws "ws-repaint-sidebar: pushing ws=%s reason=%s" ws reason)
    (condition-case err
        ;; Forced past the sidebar's signature gate: the removal this repaint
        ;; exists for is a perspective membership change, and the membership
        ;; cache the signature reads may not have caught up with the kill yet.
        (agent-repl--sidebar-push t)
      (error (agent-repl--warn ws "ws-repaint-sidebar: push error ws=%s reason=%s err=%S"
                               ws reason err)))))

(defun agent-repl--ws-main-name ()
  "Return the name of Doom's main workspace, or nil.
Reads `+workspaces-main', the variable holding the name Doom assigns to
the startup workspace.  Returns nil when that variable is unbound or nil
(e.g. persp-mode not loaded).

This is the persp-mode main-workspace boundary owned by `workspace.el'.
Callers must use this function instead of reading `+workspaces-main'
directly or guarding it themselves with `boundp'."
  (and (boundp '+workspaces-main) +workspaces-main))

(defun agent-repl--ws-create (ws &optional project-dir)
  "Create persp WS via `persp-add-new' and tag it with PROJECT-DIR.
Returns the new persp object, or nil when `persp-add-new' is unbound.

When PROJECT-DIR is non-nil and a real persp is created, also seeds
`:project-dir' into `agent-repl--workspaces' so the hash entry carries
its identity key from the moment of creation — never project-dir-less.
This is the single root that prevents a `(no repo)' stub entry when a
creation flow aborts before session-init would otherwise set it.

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
      (when (and persp (not (keywordp persp)) project-dir)
        (when (fboundp 'set-persp-parameter)
          (set-persp-parameter '+workspace-project project-dir persp))
        ;; Seed `:project-dir' into `agent-repl--workspaces' at the
        ;; creation boundary so the hash entry is never project-dir-less
        ;; from birth.  Without this, the first later `--ws-put' on this
        ;; name (e.g. `:pending-magit' in `--finalize-worktree-workspace')
        ;; auto-vivifies a stub WITHOUT `:project-dir'; if the creation
        ;; flow then aborts before session-init writes `:project-dir', the
        ;; stub persists under the `(no repo)' repo group.
        ;; Seeding here closes that window for every caller that routes
        ;; through this creation boundary.
        (agent-repl--ws-put ws :project-dir project-dir))
      persp)))

(defun agent-repl--ws-add-buffer (buffer persp &optional switch)
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

(defun agent-repl--ws-buffers (persp)
  "Return the list of buffers belonging to perspective PERSP.
Delegates to `persp-buffers'.  Returns nil when PERSP is nil or
`persp-buffers' is unbound (persp-mode not loaded).

This is the persp-mode buffer-listing boundary owned by `workspace.el'.
Callers must use this function instead of calling `persp-buffers'
directly or wrapping it themselves with `fboundp'."
  (and persp (fboundp 'persp-buffers)
       (persp-buffers persp)))

(defun agent-repl--ws-rename-persp (old-ws new-ws)
  "Rename the live perspective for OLD-WS to NEW-WS.
Resolves OLD-WS's persp via `--ws-resolve-persp' and renames it with
`persp-rename'.  Returns non-nil on success or when there is nothing to
rename (persp-mode unloaded, or OLD-WS has no live persp).  Returns nil
ONLY when a live persp existed but `persp-rename' reported failure.

This is the persp-mode rename boundary owned by `workspace.el'.
Callers must use this function instead of calling `persp-rename'
directly or wrapping it themselves with `fboundp'."
  (cond
   ((not (fboundp 'persp-rename))
    (agent-repl--log new-ws
                     "ws-rename-persp: SKIP old-ws=%s new-ws=%s reason=persp-rename-unbound"
                     old-ws new-ws)
    t)
   (t
    (let ((persp (agent-repl--ws-resolve-persp old-ws)))
      (if (not persp)
          (progn
            (agent-repl--log new-ws
                             "ws-rename-persp: SKIP old-ws=%s new-ws=%s reason=no-live-persp"
                             old-ws new-ws)
            t)
        (if (persp-rename new-ws persp)
            (progn
              (agent-repl--log new-ws
                               "ws-rename-persp: RENAMED old-ws=%s new-ws=%s persp=%S"
                               old-ws new-ws persp)
              t)
          (agent-repl--log new-ws
                           "ws-rename-persp: FAILED old-ws=%s new-ws=%s persp=%S"
                           old-ws new-ws persp)
          nil))))))

(defun agent-repl--ws-frame-ordered-names ()
  "Return workspace names in current-frame tab-bar order.
Delegates to `persp-names-current-frame-fast-ordered'.  Returns nil when
that function is unbound (persp-mode not loaded).

This is the persp-mode frame-order boundary owned by `workspace.el'.
Callers must use this function instead of calling
`persp-names-current-frame-fast-ordered' directly or wrapping it with
`fboundp'."
  (when (fboundp 'persp-names-current-frame-fast-ordered)
    (persp-names-current-frame-fast-ordered)))

(defun agent-repl--ws-update-names-cache (names)
  "Replace the persp names cache with NAMES via `persp-update-names-cache'.
No-op when that function is unbound (persp-mode not loaded).

This is the persp-mode names-cache boundary owned by `workspace.el'.
Callers must use this function instead of calling
`persp-update-names-cache' directly or wrapping it with `fboundp'."
  (when (fboundp 'persp-update-names-cache)
    (persp-update-names-cache names)))

(defun agent-repl--ws-window-conf (persp)
  "Return the saved window-configuration for perspective PERSP.
Delegates to `persp-window-conf'.  Returns nil when PERSP is nil or
`persp-window-conf' is unbound (persp-mode not loaded).

This is the persp-mode window-config boundary owned by `workspace.el'.
Callers must use this function instead of calling `persp-window-conf'
directly or wrapping it with `fboundp'."
  (and persp (fboundp 'persp-window-conf)
       (persp-window-conf persp)))

(defun agent-repl--ws-tab-face ()
  "Return the Doom face symbol for an unselected workspace tab name.
Names the `+workspace-tab-face' face that Doom's tab-bar defines.

This is the Doom tab-face boundary owned by `workspace.el'.
Callers must use this function instead of referring to
`+workspace-tab-face' directly."
  '+workspace-tab-face)

(defun agent-repl--ws-tab-selected-face ()
  "Return the Doom face symbol for a selected workspace tab name.
Names the `+workspace-tab-selected-face' face that Doom's tab-bar defines.

This is the Doom tab-face boundary owned by `workspace.el'.
Callers must use this function instead of referring to
`+workspace-tab-selected-face' directly."
  '+workspace-tab-selected-face)

(defun agent-repl--ws-all-persps ()
  "Return the raw list of all perspective objects via `persp-persps'.
Returns nil when `persp-persps' is unbound (persp-mode not loaded).  The
list may include persp-mode's nil container and non-perspective symbol
entries; callers filter as needed.

This is the persp-mode enumeration boundary owned by `workspace.el'.
Callers must use this function instead of calling `persp-persps'
directly or wrapping it themselves with `fboundp'."
  (when (fboundp 'persp-persps)
    (persp-persps)))

(defun agent-repl--ws-persp-name (persp)
  "Return the name of perspective PERSP via `safe-persp-name'.
Returns nil when `safe-persp-name' is unbound (persp-mode not loaded).

This is the persp-mode name-resolution boundary owned by `workspace.el'.
Callers must use this function instead of calling `safe-persp-name'
directly or wrapping it themselves with `fboundp'."
  (when (fboundp 'safe-persp-name)
    (safe-persp-name persp)))

(defun agent-repl--ws-persp-identity (persp)
  "Return a bounded process-local diagnostic identity for perspective PERSP.
The identity uses object identity without printing PERSP, whose recursive
representation may contain buffer names, window state, and other editor
contents.  It is diagnostic correlation only and is not persisted as product
state.  The owning high-level operation logs this returned identity as part of
its aggregate record, so this boundary does not emit a duplicate record."
  (unless persp
    (agent-repl--log '(:agent-repl-central "the rejected perspective is absent")
                     "ws-persp-identity: rejected reason=nil-perspective")
    (error "agent-repl--ws-persp-identity: perspective must be non-nil"))
  (format "persp@%x" (sxhash-eq persp)))

(defun agent-repl--ws-names-cache ()
  "Return the raw `persp-names-cache' list, or nil when unbound.
Used mainly for diagnostic logging of cache state.  Returns nil both
when the cache is empty and when `persp-names-cache' is unbound
(persp-mode not loaded).

This is the persp-mode names-cache read boundary owned by `workspace.el'.
Callers must use this function instead of reading `persp-names-cache'
directly or guarding it themselves with `boundp'."
  (and (boundp 'persp-names-cache) persp-names-cache))

;; ---- The one teardown order: land first, then kill -------------------
;;
;; LANDING NEVER FOLLOWS THE KILL.  Doom's `+workspace/kill', asked to kill
;; the CURRENT workspace, kills it (dropping the frame into persp-mode's nil
;; perspective), switches to `+workspace--last', and then -- when the window
;; it lands on shows no Doom-"real" buffer -- puts `doom-fallback-buffer' in
;; that window.  agent-repl's panels are not Doom-real and a panels-only
;; workspace has no other window to select, so the landing workspace's panel
;; window got the Doom splash: closing workspace B destroyed workspace A's
;; layout.  agent-repl therefore never calls `+workspace/kill'.  It lands on
;; the chosen survivor through the ordinary switch a user makes, and only
;; then kills the departing perspective, which is by then NOT current -- the
;; branch of Doom's kill that is a bare `persp-kill', with no switch and no
;; fallback.  Closing a workspace changes nothing about any other workspace.

(defun agent-repl--teardown-landing-target (ws)
  "Return the workspace a teardown of WS lands the user on, or nil.

THE WORKSPACE SELECTED BEFORE WS (owner ruling, 2026-09-30): the most
recently selected agent-repl workspace that is still open, in the one
selection-recency order (`agent-repl-roster-selection-recency-order'):
this session's `agent-repl--workspace-history' first, then the roster's
durable last-selected instant, so a fresh Emacs still knows which
workspace came before.  A workspace that has since closed is not a
candidate, so the next most recent survivor is taken.  How WS closed does
not matter -- the user's close, kill or nuke and an implicit close such as
a landed merge all reach this one rule through
`agent-repl--land-before-teardown' -- and it is only ever asked when the
user stands on WS (the landing is not owed otherwise).

Tab order decides only among workspaces never selected at all, the
genuine, expected case of workspaces the user has not yet looked at; with
no agent-repl workspace left, the first surviving perspective of any kind
is taken.  Which source decided (history, roster, tab-order or
foreign-persp) is recorded at INFO.  WS itself is never a candidate.

A BUILT-IN PERSPECTIVE IS NOT A LANDING.  \"none\" and Doom's startup
\"main\" are persp-mode's own perspectives
\(`agent-repl--pseudo-workspace-name-p'): they own no project, no session
and no panels, and Doom's \"main\" is auto-vivified into the registry by a
persp hook, so it can lead `agent-repl--ws-list-names' and be picked ahead
of every real workspace.  Landing there put the user on an EMPTY frame --
the fallback buffer under a lone tab, observed as `*scratch*' in playbook
B.16 after the merged child's tab was torn down.  So every candidate
source is filtered to workspaces this module actually owns."
  (let* ((landable-p (lambda (n)
                       (and (stringp n)
                            (not (equal n ws))
                            (not (agent-repl--pseudo-workspace-name-p n)))))
         (open (agent-repl-roster-selection-recency-order
                (cl-remove-if-not landable-p (agent-repl--ws-list-names))))
         (target (or (car open)
                     (car (cl-remove-if-not landable-p (agent-repl--ws-all-names)))))
         (source (cond ((null (car open)) "foreign-persp")
                       ((member target agent-repl--workspace-history) "history")
                       ((agent-repl-roster-last-selected-ms target) "roster")
                       (t "tab-order"))))
    (agent-repl--info ws "elisp.workspace.teardown-landing-target: ws=%s target=%s source=%s history=%S"
                      ws target source agent-repl--workspace-history)
    target))

(defun agent-repl--land-before-teardown (ws)
  "Move the user off WS, which is about to be killed, onto a survivor.

TEARING DOWN A WORKSPACE MUST NAME WHERE THE USER ENDS UP, and it names
it BEFORE the kill (see the section commentary above).  A landing is owed
when the user stands on WS itself, or stands in no workspace at all -- no
current perspective, a perspective no longer in the tab bar, or one of
persp-mode's built-ins, which is a landing still owed rather than one
already made.  Standing on some OTHER live workspace owes nothing, so a
teardown of a tab the user is not on never moves them.

The target is `agent-repl--teardown-landing-target', and the switch is
`agent-repl--ws-switch' -- the switch a user's own `SPC TAB' makes -- so
the landing workspace's saved layout is restored and its panels are
reconciled by the same persp activation path as every other arrival
\(`agent-repl--on-workspace-switch').  Nothing is armed on the target:
an arrival re-shows panels by default, and a workspace whose panels the
user explicitly closed keeps them closed, because tearing down one
workspace must change nothing about another.

When nothing survives there is nowhere to land: the frame is left as
persp-mode arranges it, and that is recorded at WARN rather than silently
accepted.

Returns the workspace landed on, or nil when no switch was owed or
possible.  A failing switch SIGNALS: a landing that did not happen must
not read as one that did, and the caller must not go on to kill the
workspace the user is still standing on."
  (if (not (agent-repl--ws-system-available-p))
      (progn
        (agent-repl--info ws "elisp.workspace.teardown-landing: ws=%s decision=skip reason=no-workspace-system" ws)
        nil)
    (let ((current (agent-repl--ws-current-name))
          (survivors (agent-repl--ws-all-names)))
      (if (and current
               (not (equal current ws))
               (not (agent-repl--pseudo-workspace-name-p current))
               (member current survivors))
          (progn
            (agent-repl--info ws "elisp.workspace.teardown-landing: ws=%s decision=not-owed current=%s"
                              ws current)
            nil)
        (let ((target (agent-repl--teardown-landing-target ws)))
          (if (null target)
              (progn
                (agent-repl--warn
                 ws "teardown-landing: NO surviving workspace to land in (ws=%s current=%s survivors=%S)"
                 ws current survivors)
                nil)
            (agent-repl--info ws "elisp.workspace.teardown-landing: ws=%s decision=land current=%s target=%s"
                              ws current target)
            (agent-repl--ws-switch target)
            (agent-repl--info ws "elisp.workspace.teardown-landing: ws=%s landed=%s" ws target)
            target))))))

(defun agent-repl--ws-persp-kill-refusal (ws)
  "Return why persp WS may not be killed, or nil when it may.

The refusals Doom's `+workspace/kill' makes, reproduced here because
agent-repl no longer calls it (`agent-repl--ws-land-then-kill'):

  - the perspective is visible in another frame
    \(`persp-frames-with-persp'), which Doom refuses with \"Can't close
    workspace, it's visible in another frame\".  Doom asks the question of
    the CURRENT frame's perspective; it is asked here of WS's own, which is
    the same question whenever WS is current and the meaningful one when it
    is not;
  - WS is persp-mode's protected nil perspective (`+workspace--protected-p',
    the check Doom's `+workspace-kill' makes).

Each probe is guarded on its persp-mode or Doom function being bound, so
nil is also the answer when the workspace system is not loaded."
  (cond
   ((and (fboundp '+workspace--protected-p) (+workspace--protected-p ws))
    "it is persp-mode's protected nil perspective")
   ((when-let ((persp (agent-repl--ws-resolve-persp ws)))
      (and (fboundp 'persp-frames-with-persp)
           (delq (selected-frame) (persp-frames-with-persp persp))))
    "it is visible in another frame")))

(defun agent-repl--assert-persp-killable (ws)
  "SIGNAL `user-error' when persp WS may not be killed.
Logged at INFO, then signalled -- never swallowed: a caller that asked to
kill a workspace the system will not kill asked for something it cannot
have, and a silent skip would read as a completed teardown.  See
`agent-repl--ws-persp-kill-refusal' for the refusals."
  (when-let ((reason (agent-repl--ws-persp-kill-refusal ws)))
    (agent-repl--info ws "elisp.workspace.teardown-refused: ws=%s reason=%s" ws reason)
    (user-error "Can't close workspace '%s': %s" ws reason)))

(defun agent-repl--ws-persp-exists-p (ws)
  "Return non-nil when a perspective named WS is in the tab bar.
Delegates to `+workspace-exists-p', the existence check Doom's
`+workspace/kill' makes; nil when that function is unbound.  Part of the
persp-mode integration boundary owned by `workspace.el'.

`persp-get-by-name' is NOT the test: it answers the truthy keyword
`persp-not-persp' for a missing persp, so a guard on it never
short-circuits -- which let the merge flow's second close of an
already-killed workspace through to a kill that echoed \"'<ws>' workspace
doesn't exist\" after every successful merge."
  (and (fboundp '+workspace-exists-p)
       (+workspace-exists-p ws)
       t))

(defun agent-repl--ws-land-then-kill (ws)
  "Tear down perspective WS in the one teardown order: land, then kill.

Emulates Doom's `+workspace/kill' without its landing and fallback (see
the section commentary): the existence check, the refusals
\(`agent-repl--assert-persp-killable'), then -- in place of Doom's
kill-switch-fallback -- `agent-repl--land-before-teardown' followed by
the kill through `agent-repl--ws-persp-kill'.  By the time the kill runs
WS is not current unless nothing survives, so the kill cannot touch the
landing workspace's windows and no fallback buffer is ever displayed.

A WS that is not a workspace is not killed and is not an error: the merge
flow closes the same workspace twice, and the second close finds it gone.
The landing still runs, because the user may be standing nowhere.

This is the ONE teardown order: `agent-repl--kill-one-workspace' and the
roster's tab teardown (`agent-repl-roster--teardown-tab') both call it.
Refusals and a failed landing signal to the caller.  Returns non-nil
when the perspective was killed."
  (let ((exists (agent-repl--ws-persp-exists-p ws)))
    (when exists
      (agent-repl--assert-persp-killable ws))
    (agent-repl--land-before-teardown ws)
    (if (not exists)
        (progn
          (agent-repl--info ws "elisp.workspace.teardown-kill: ws=%s decision=skip reason=not-a-workspace" ws)
          nil)
      (agent-repl--info ws "elisp.workspace.teardown-kill: ws=%s decision=kill current=%s"
                        ws (agent-repl--ws-current-name))
      (agent-repl--ws-persp-kill ws)
      (agent-repl--info ws "elisp.workspace.teardown-kill: ws=%s killed still-listed=%s"
                        ws (if (agent-repl--ws-persp-exists-p ws) "t" "nil"))
      t)))

(defun agent-repl--arm-landing-panels (target)
  "Arm TARGET so ARRIVING at it puts its panel on the frame.

AN ARRIVAL THAT SHOWS NOTHING IS NOT AN ARRIVAL.  Switching to a
workspace restores whatever window configuration persp-mode
saved for it, and a workspace the user has not stood in since its panel
was pre-created has no configuration worth restoring — so the frame came
up EMPTY: one window, no buffer content, no mode line, with only the tab
bar to say anything had happened at all, indistinguishable from a
wedged editor.

It is the flag rather than a direct show because the flag is the
module's own way of saying \"this workspace becomes visible on arrival\"
\(`agent-repl--drain-pending-show-panels').  A second mechanism beside
it would be a second answer to one question.

`agent-repl-switch-to-project' arms it for the workspace a projectile
switch is about to stand on -- a workspace you just created has never
been stood in, so without the arm `SPC TAB n' left the user on an empty
frame.  A teardown landing does NOT arm (`agent-repl--land-before-teardown'):
the arrival re-shows panels by default, and arming would override an
explicit close in a workspace the teardown was not about."
  (when target
    (agent-repl--log target "arm-landing-panels: ws=%s" target)
    (agent-repl--ws-put target :pending-show-panels t)))

(defun agent-repl--ws-shared-unowned-buffer-p (buf ws)
  "Return non-nil when BUF is no workspace's own and belongs to a persp besides WS.

A TEARDOWN OF WS MUST CHANGE NOTHING ABOUT ANY OTHER WORKSPACE.  A file,
magit or scratch buffer owned by no agent-repl workspace
\(`agent-repl--buffer-owner' nil) can sit in several perspectives at
once, and when another live perspective holds it -- the workspace the
teardown lands on among them -- it is that workspace's buffer too:
killing it, or retiring its windows, would reach into a bystander.
Agent buffers are answered by ownership instead
\(`agent-repl--foreign-owned-buffer-p'), so a buffer WS owns is never
spared here even if persp-mode drifted it elsewhere.

Asks persp-mode's `persp-other-persps-with-buffer-except-nil', which
leaves out WS's own persp and persp-mode's nil perspective.  nil when
persp-mode is not loaded or WS has no live persp.  Part of the persp-mode
integration boundary owned by `workspace.el'."
  (and (buffer-live-p buf)
       (null (agent-repl--buffer-owner buf))
       (fboundp 'persp-other-persps-with-buffer-except-nil)
       (when-let ((persp (agent-repl--ws-resolve-persp ws)))
         (and (persp-other-persps-with-buffer-except-nil buf persp) t))))

(defun agent-repl--ws-retire-persp-windows (ws)
  "Retire every window displaying a buffer of perspective WS.

RUN BEFORE `persp-kill', and it is what makes that kill possible at all.
`persp-kill' removes each of the persp's buffers, and persp-mode retires
a removed buffer from the windows showing it with `set-window-buffer'
(`persp-set-another-buffer-for-window').  A STRONGLY dedicated window
refuses that call and signals \"Window is dedicated to ...\", and
agent-repl's own panels are precisely strongly dedicated
(`agent-repl-window--harden').  So a workspace killed while its panels
were on screen -- which is every workspace the user was STANDING ON, the
merged child among them -- aborted the kill mid-walk: the persp survived,
its tab stayed on the bar, and the signal escaped the roster push that
asked for the teardown.

The windows go through the module's own retire recipe
(`agent-repl-window--delete-buffer-windows'), which deletes each one or,
when it is the frame's last, switches it to the fallback buffer.  Either
way the dying workspace stops being displayed BEFORE persp-mode reaches
for the window, so the signal is not caught -- it is not raised.

Agent buffers owned by a different workspace (see
`agent-repl--foreign-owned-buffer-p') are skipped, not retired: persp-mode
can drift another workspace's live panel into this persp, and retiring it
here would delete that neighbor's on-screen panel window across all frames
-- closing a bystander workspace's panels.  An unowned buffer another
live perspective also holds is skipped for the same reason
\(`agent-repl--ws-shared-unowned-buffer-p').  This mirrors the same skips
in `agent-repl--kill-workspace-buffers'.

No-op when persp-mode is not loaded or WS has no live persp."
  (when-let* ((persp (agent-repl--ws-resolve-persp ws))
              (bufs (agent-repl--ws-buffers persp)))
    (agent-repl--log ws "ws-retire-persp-windows: ws=%s buffers=%d" ws (length bufs))
    (dolist (buf bufs)
      (when (buffer-live-p buf)
        (cond
         ((agent-repl--foreign-owned-buffer-p buf ws)
          (agent-repl--log ws "ws-retire-persp-windows: SKIP foreign buf=%s owner=%s"
                           (agent-repl--safe-buffer-name buf)
                           (agent-repl--buffer-owner buf)))
         ((agent-repl--ws-shared-unowned-buffer-p buf ws)
          (agent-repl--log ws "ws-retire-persp-windows: SKIP shared buf=%s"
                           (agent-repl--safe-buffer-name buf)))
         (t (agent-repl-window--delete-buffer-windows buf :ws ws)))))))

(defun agent-repl--ws-persp-kill (ws)
  "Kill the perspective named WS via the low-level `persp-kill'.
No-op when `persp-kill' is unbound.  This is the
lower-level persp-mode kill used when the caller has already decided the
persp should be dropped; a teardown reaches it through
`agent-repl--ws-land-then-kill', which lands the user first.

Repaints the sidebar afterwards (`agent-repl--ws-repaint-sidebar') for
the same reason every tab-bar exit does: the workspace just left the
tab bar, so its roster row is gone and the webview should say so now.

This is the persp-mode low-level kill boundary owned by `workspace.el'.
Callers must use this function instead of calling `persp-kill' directly
or wrapping it themselves with `fboundp'.

The persp's windows are retired FIRST
(`agent-repl--ws-retire-persp-windows'): persp-mode's own buffer removal
cannot retire a strongly dedicated window, and agent-repl's panels are
exactly that -- see that function's docstring."
  (when (fboundp 'persp-kill)
    (agent-repl--ws-retire-persp-windows ws)
    (prog1 (persp-kill ws)
      (agent-repl--ws-repaint-sidebar ws "persp-kill"))))

(defun agent-repl--ws-remove-buffer (buffer)
  "Detach BUFFER from its perspective via `persp-remove-buffer'.
No-op when `persp-remove-buffer' is unbound (persp-mode not loaded).

Detach means DETACH: `persp-autokill-buffer-on-remove' is bound to nil
for the call, so persp-mode's autokill never escalates the removal into
a `kill-buffer'.  Doom ships that option as `kill-weak', under which
persp-mode kills any removed buffer belonging to no perspective — and
the frontend webview is exactly such a buffer, since it is mounted as a
raw xwidget-webkit session and never `persp-add-buffer'ed.  Left
unbound, detaching a foreign panel on a workspace switch would kill the
OTHER workspace's live GUI, and `xwidget-kill-buffer-query-function'
would raise a blocking \"has xwidgets; kill it?\" prompt mid-switch.

This is the persp-mode buffer-detach boundary owned by `workspace.el'.
Callers must use this function instead of calling `persp-remove-buffer'
directly or wrapping it themselves with `fboundp'."
  (when (fboundp 'persp-remove-buffer)
    (let ((persp-autokill-buffer-on-remove nil))
      (persp-remove-buffer buffer))))

;;;; ---- Startup deletion of persp-mode's pseudo perspectives -------------

(defvar agent-repl--pseudo-perspectives-deleted nil
  "Non-nil once the startup pseudo-perspective deletion has run.
`agent-repl--delete-pseudo-perspectives' clears persp-mode's leading
pseudo perspectives exactly ONCE per session, and this flag is what makes
it once: a later roster bring-up finds it set and does nothing.  It
re-defaults to nil on a full module reload; tests reset it explicitly.")

(defun agent-repl--pseudo-perspective-killable-p (name)
  "Return non-nil when pseudo perspective NAME can be removed by `persp-kill'.

Only a NON-nil pseudo perspective is killable.  persp-mode stores its nil
perspective (`persp-nil-name', \"none\") in the hash under the LITERAL value
nil, and `persp-kill' removes a name only through `persp-remove-by-name'
guarded on the perspective being non-nil — which that one is not — while
`persp-remove-by-name' itself refuses it outright (it messages that the
nil perspective cannot be removed).  Worse, `persp-kill' walks and
detaches the perspective's buffers BEFORE that guard, so calling it on
\"none\" would strip the nil
perspective's buffers WITHOUT ever removing the \"none\" name — a
destructive no-op.  The caller must therefore SKIP it.  Doom's initial
\"main\" IS a normal `make-persp' perspective and is killable."
  (and (agent-repl--pseudo-workspace-name-p name)
       (let ((nil-name (and (boundp 'persp-nil-name) persp-nil-name)))
         (not (and (stringp nil-name) (equal name nil-name))))))

(defun agent-repl--delete-pseudo-perspectives ()
  "Delete persp-mode's leading pseudo perspectives now a real one exists.

WHY DELETE, NOT HIDE (regression watch, 2026-09-15).  persp-mode seeds a
session with its own perspectives — `persp-nil-name' (\"none\") and Doom's
initial \"main\" — that agent-repl never created and that own no workspace
\(`agent-repl--pseudo-workspace-name-p').  While they sit in
`persp-names-cache' they take its LEADING slots, so the perspective-list
index is off by one against the drawn tab bar: the numeral chords and any
`+workspace/switch-to-N' reach the tab after the one their number names.
Filtering the pseudos out of the DRAWN list left the slots in the
underlying list, so the durable fix is to REMOVE them — once they are gone
`[N]' == `M-<n>' with nothing to filter or index around.

DELETED AFTER THE FIRST REAL WORKSPACE, NEVER BEFORE.  persp-mode requires
at least one perspective, so deleting the pseudos while nothing else
exists just makes persp recreate them.  This is driven from the roster's
first bring-up completion (`agent-repl--delete-pseudos-on-bringup') and it
refuses to act unless a real workspace perspective is already present in
`persp-names-cache', so the kill can never strand the session at zero
perspectives.

Runs at most once (`agent-repl--pseudo-perspectives-deleted').  \"none\" is
`persp-nil' and is NOT killable (`agent-repl--pseudo-perspective-killable-p');
it is recorded and left in place rather than hacked out of persp
internals.  If the frame is standing IN a pseudo when the kill fires, it
is moved to a real workspace first so the kill does not strand it."
  (when (and (not agent-repl--pseudo-perspectives-deleted)
             (agent-repl--ws-system-available-p))
    (let* ((names (agent-repl--ws-names-cache))
           (reals (cl-remove-if #'agent-repl--pseudo-workspace-name-p names))
           (pseudos (cl-remove-if-not #'agent-repl--pseudo-workspace-name-p names)))
      ;; Never delete while no real perspective survives the kill, and
      ;; never consume the one-shot before there is anything to delete.
      (when (and reals pseudos)
        (setq agent-repl--pseudo-perspectives-deleted t)
        (let ((current (agent-repl--ws-current-name)))
          (when (and current (agent-repl--pseudo-workspace-name-p current))
            (agent-repl--info
             '(:agent-repl-central "workspace numbering spans the whole session")
             "elisp.workspace.pseudo-delete-vacate current=%s target=%s"
             current (car reals))
            (agent-repl--ws-switch (car reals))))
        (dolist (pseudo pseudos)
          (if (agent-repl--pseudo-perspective-killable-p pseudo)
              (progn
                (agent-repl--info
                 '(:agent-repl-central "workspace numbering spans the whole session")
                 "elisp.workspace.pseudo-delete ws=%s" pseudo)
                (agent-repl--ws-persp-kill pseudo))
            (agent-repl--info
             '(:agent-repl-central "workspace numbering spans the whole session")
             "elisp.workspace.pseudo-delete-skip-nil ws=%s (persp-nil is not killable)"
             pseudo)))))))

(defun agent-repl--delete-pseudos-on-bringup (opened _total finished)
  "Delete the pseudo perspectives once the first roster reconcile settles.

Registered on `agent-repl-roster-bringup-functions' (roster.el), whose
FINISHED call closes a reconcile pass and whose OPENED counts the real
workspace tabs that pass brought up.  A finished pass that opened at least
one tab is the earliest point a real workspace perspective is guaranteed
to exist — exactly the precondition `agent-repl--delete-pseudo-perspectives'
needs before it may clear the pseudos.  See that function for why the
pseudos are DELETED, not hidden."
  (when (and finished (> opened 0))
    (agent-repl--delete-pseudo-perspectives)))

(add-hook 'agent-repl-roster-bringup-functions
          #'agent-repl--delete-pseudos-on-bringup)

;;;; ---- Projectile integration boundary ---------------------------------
;;
;; A agent-repl workspace IS a project (a dir-keyed persp), so projectile
;; is part of the same workspace domain.  These wrappers are the single
;; place agent-repl touches the projectile known-projects API, mirroring
;; the persp-mode boundary above.  Callers outside this file must use
;; these wrappers rather than naming `projectile-*' directly.

(defun agent-repl--ws-register-project (dir)
  "Register DIR with projectile via `projectile-add-known-project'.
No-op when `projectile-add-known-project' is unbound (projectile not
loaded).  DIR should already be normalized by the caller (e.g. via
`file-name-as-directory') when canonical form matters.

Projectile boundary owned by `workspace.el'."
  (when (fboundp 'projectile-add-known-project)
    (projectile-add-known-project dir)))

(defun agent-repl--ws-switch-project (project)
  "Switch to PROJECT via `projectile-switch-project-by-name'.
No-op when `projectile-switch-project-by-name' is unbound (projectile
not loaded).

Projectile boundary owned by `workspace.el'."
  (when (fboundp 'projectile-switch-project-by-name)
    (projectile-switch-project-by-name project)))

;;;; ---- persp-mode load-ordering / hook-registration boundary -----------
;;
;; These installers let callers register persp-mode lifecycle hooks (and
;; run load-deferred setup) without naming the `persp-mode' feature or its
;; hook variables directly.  They are load-time wiring, mirroring the bare
;; `with-eval-after-load' / `add-hook' forms they replace.

(defun agent-repl--ws-add-activated-hook (fn)
  "Register FN to run when a perspective is activated.
Adds FN to `persp-activated-functions' once persp-mode loads.

This is the persp-mode activation-hook boundary owned by `workspace.el'.
Callers must use this function instead of touching `persp-activated-functions'
or `with-eval-after-load' on persp-mode directly."
  (with-eval-after-load 'persp-mode
    (add-hook 'persp-activated-functions fn)))

(defun agent-repl--ws-add-before-deactivate-hook (fn)
  "Register FN to run before a perspective is deactivated.
Adds FN to `persp-before-deactivate-functions' once persp-mode loads.

This is the persp-mode deactivation-hook boundary owned by `workspace.el'.
Callers must use this function instead of touching
`persp-before-deactivate-functions' or `with-eval-after-load' on
persp-mode directly."
  (with-eval-after-load 'persp-mode
    (add-hook 'persp-before-deactivate-functions fn)))

(defun agent-repl--ws-after-system-load (thunk)
  "Call THUNK once the persp-mode workspace system has loaded.
Thin wrapper over `with-eval-after-load' for the persp-mode feature so
callers do not name the feature directly.

Boundary owned by `workspace.el'."
  (with-eval-after-load 'persp-mode
    (funcall thunk)))

;;;; ---- persp-mode policy configuration ---------------------------------
;;
;; agent-repl owns workspace/persp policy.  These settings used to live
;; in the top-level config.el and were moved here so the persp boundary
;; owns persp-mode's own configuration.  Deferred until persp-mode loads.

(defun agent-repl--ws-switch-project-display (dir)
  "Land a projectile switch to DIR on the display that dir OWNS.

This is `+workspaces-switch-project-function\' -- Doom calls it right
after `+workspaces-switch-to-project-h\' has switched the perspective,
to decide what the new perspective shows.

An agent-repl workspace ALREADY OWNS ITS DISPLAY: its panel is what the
persp activation drains put in the main area, so this function has
nothing left to choose and returns without touching the layout.  The
panel is a webview buffer and is deliberately not a `doom-real-buffer-list\'
member, so without this branch the empty-project fallback below fires for
every workspace and replaces the panel with magit status -- which is
exactly what a workspace you just created showed you instead of its
agent.

For a plain project (no workspace at DIR) the old behavior stands: skip
the find-file prompt when the project already has buffers open, and show
magit status when it has none."
  (let ((ws (agent-repl--ws-name-for-dir dir)))
    (cond
     (ws
      (agent-repl--log ws "ws-switch-project-display: ws=%s dir=%s branch=workspace-panel-owns-display"
                       ws dir))
     ((doom-real-buffer-list)
      (agent-repl--log '(:agent-repl-central "a plain project is not an agent workspace")
                       "ws-switch-project-display: dir=%s branch=has-real-buffers" dir))
     (t
      (agent-repl--log '(:agent-repl-central "a plain project is not an agent workspace")
                       "ws-switch-project-display: dir=%s branch=magit-status" dir)
      (agent-repl--magit-status-same-window dir)))))

(defun agent-repl--ws-install-persp-policy ()
  "Install agent-repl's persp-mode policy into persp-mode's own variables.

A NAMED FUNCTION rather than a bare `with-eval-after-load\=' body, because
the policy is a claim about how the module behaves and a claim nothing
can call is a claim nothing can check: this is the one place the settings
are written, and `lisp/test-workspace.el\=' exercises it directly instead
of arranging for persp-mode to load inside a batch Emacs.

Run from the `persp-mode\=' after-load below, which is the only caller in
the product."
  ;; What a project switch lands on -- a workspace's own panel, or magit
  ;; for a plain project with nothing open.  See the function's docstring.
  (setq +workspaces-switch-project-function
        #'agent-repl--ws-switch-project-display)
  ;; A PROJECT SWITCH ALWAYS MAKES ITS OWN WORKSPACE, AND NEVER RECYCLES THE
  ;; ONE IT LEAVES.  This is the second half of the fact the line above is the
  ;; first half of: the panel is a webview buffer and is deliberately not a
  ;; `doom-real-buffer-list' member, so an agent-repl workspace showing its
  ;; agent looks EMPTY to Doom.
  ;;
  ;; Under the `non-empty' default, `+workspaces-switch-to-project-h' reads
  ;; that emptiness as "nothing here worth keeping" and takes its recycle
  ;; branch, which `+workspace-rename's the workspace being LEFT to the name
  ;; of the project being entered.  The renamed persp then leaves
  ;; `persp-names-cache' under its old name while `agent-repl--workspaces' and
  ;; the daemon's roster still carry it, and since `--ws-tabline-names'
  ;; intersects the two, the abandoned workspace's TAB SILENTLY VANISHES
  ;; while the roster keeps listing it.
  ;;
  ;; Measured, in a headless run's K.63 case: a workspace created on the
  ;; daemon's own repository drew its tab, and registering a second repository
  ;; a moment later left the bar drawing [repo-turning self-repo] with the
  ;; roster's own order still [repo-turning self-repo k63-merging] and the
  ;; perspective cache reading [none self-repo repo-turning].
  ;;
  ;; `t' is the only value that cannot express the recycle: agent-repl owns
  ;; every workspace of its own, and none of them is ever a scratch one to be
  ;; taken over.  Doom's protected main workspace already forced this branch
  ;; for itself, so what changes is exactly the agent workspaces.
  (setq +workspaces-on-switch-project-behavior t)
  ;; persp-mode's own session persistence is disabled — agent-repl is the
  ;; single source of truth for workspace save/restore via its snapshot
  ;; mechanism.  -1 disables auto-resume; 0 disables auto-save on kill.
  (setq persp-auto-resume-time -1
        persp-auto-save-opt 0)
  ;; Never prompt when killing a buffer not in the current workspace.
  (setq persp-kill-foreign-buffer-behaviour 'kill)
  ;; Only show current-workspace buffers in buffer lists (SPC ,).
  (setq persp-set-frame-buffer-predicate t))

(with-eval-after-load 'persp-mode
  (agent-repl--ws-install-persp-policy))

(defun agent-repl--record-workspace-history (&rest _)
  "Record the current workspace at the front of `agent-repl--workspace-history'.
Removes any prior occurrence of the name so the list stays
most-recently-visited-first with no duplicates.  No-op when there is no
current workspace.  Registered on the persp activation hook below.

Also stamps `:last-viewed-at' with `current-time' on the activated
workspace's plist.  This is the single view chokepoint every
perspective activation funnels through, so the project picker
\(`agent-repl-switch-to-project') can sort by most-recently-viewed and
`agent-repl--state-save' can persist the stamp for dead workspaces."
  ;; Suppressed during `agent-repl--eager-open-panels': the transient
  ;; activation of a just-generated background workspace is not a real
  ;; visit, so recording it would make `SPC b p' treat the generated
  ;; workspace as the caller's previous one and stamp a phantom
  ;; `:last-viewed-at'.
  (if agent-repl--eager-open-in-progress
      (agent-repl--log (agent-repl--ws-current-log-name)
                        "record-workspace-history: suppressed (eager-open in progress)")
    (let ((name (agent-repl--ws-current-name)))
      (when name
        ;; Stamp only known agent-repl workspaces; a foreign persp (the main
        ;; persp, a non-agent-repl one) has no hash entry, and `--ws-put'
        ;; would otherwise STUB-CREATE a spurious :project-dir-less entry.
        (when (agent-repl--ws-known-p name)
          (agent-repl--ws-put name :last-viewed-at (current-time)))
        (setq agent-repl--workspace-history
              (cons name (cl-remove name agent-repl--workspace-history
                                    :test #'string=)))))))

(agent-repl--ws-add-activated-hook #'agent-repl--record-workspace-history)

(provide 'agent-repl-workspace)
;;; workspace.el ends here
