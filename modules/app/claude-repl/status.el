;;; status.el --- workspace status state machine and tab bar rendering -*- lexical-binding: t; -*-

;;; Code:

;;; Priority badge images
;;
;; Each image is a small PNG loaded from the module's images/ directory and
;; scaled to fit the tab-bar line height.

(defcustom claude-repl-priority-levels '("p05" "p1" "p2" "p3")
  "List of recognized priority level strings for workspace badges."
  :type '(repeat string)
  :group 'claude-repl)

(defcustom claude-repl-repo-default-priorities '(("explanation-engine" . "p1"))
  "Alist mapping repository names to default `:priority' values for new workspaces.
The repository name is the basename of the parent of `git rev-parse
--git-common-dir' for a path, matching how the drawer groups workspaces
by repo.  Used by workspace-creation paths as a final fallback when no
explicit priority was supplied and none was inherited from a source
workspace.  An entry whose value is nil disables the default for that
repo."
  :type '(alist :key-type string
                :value-type (choice (string :tag "Priority") (const :tag "None" nil)))
  :group 'claude-repl)

(defun claude-repl--repo-name-for-path (path)
  "Return the repository name for PATH, or nil.
Resolved as the basename of the parent of `git rev-parse --git-common-dir',
matching the drawer's repo-group-label logic.  Returns nil when PATH is
nil, does not exist, is not inside a git repository, or git fails."
  (when (and path
             (stringp path)
             (file-directory-p (expand-file-name path)))
    (let* ((dir (expand-file-name path))
           (raw (let ((default-directory dir))
                  (claude-repl--git-string-quiet "rev-parse" "--git-common-dir"))))
      (when (and raw
                 (not (string-empty-p raw))
                 (not (string-prefix-p "fatal" raw)))
        (let* ((abs (if (file-name-absolute-p raw) raw
                      (expand-file-name raw dir)))
               (canon (claude-repl--path-canonical abs))
               (parent (file-name-directory canon)))
          (when parent
            (file-name-nondirectory (directory-file-name parent))))))))

(defun claude-repl--repo-default-priority-for-path (path)
  "Return the default `:priority' string for a workspace rooted at PATH.
Looks up the repo name (see `claude-repl--repo-name-for-path') in
`claude-repl-repo-default-priorities'.  Returns nil when PATH has no
recognized repo or the repo has no configured default."
  (when-let ((name (claude-repl--repo-name-for-path path)))
    (cdr (assoc name claude-repl-repo-default-priorities))))

(defcustom claude-repl-tab-bracket-format "[%s]"
  "Format string for tab bracket labels.
%s is replaced with the tab index number or emoji."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-tab-name-padding " %s "
  "Format string for tab workspace name padding."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-state-poll-interval 1
  "Seconds between workspace state update polls."
  :type 'integer
  :group 'claude-repl)

(defcustom claude-repl-state-git-tick-modulus 5
  "Per-workspace git refreshes fire once every N timer ticks.
The 1Hz `claude-repl--update-all-workspace-states' timer drives both
the cheap state-machine work (claude-running-p, update-ws-state,
mark-dead-vterm) and the expensive git work
\(`claude-repl--async-refresh-git-status' and
`claude-repl--async-refresh-branch-merged').  Cheap work runs every
tick so transitions like `:done' -> `:idle' stay snappy.  Git work
runs only when `(mod tick-counter N) == 0' so the per-ws fork load is
amortized to one-in-N ticks; the on-disk reality git observes does
not change at 1Hz, so polling that fast is wasteful.

Lower values mean fresher cached git state at higher CPU cost;
higher values do the inverse.  The default of 5 yields one git
refresh per workspace per ~5 seconds, paired with the spread (see
`claude-repl-state-spread-window') so even those refreshes are not
bursty."
  :type 'integer
  :group 'claude-repl)

(defcustom claude-repl-state-spread-window 1.0
  "Seconds over which per-workspace state updates are spread per tick.
Each tick, `claude-repl--update-all-workspace-states' snapshots the
workspace list and processes one workspace at a time via
`run-at-time' with gap `(max claude-repl-state-spread-min-gap (/ this
N))', where N is the workspace count.  This flattens the per-tick
burst (N forks landing simultaneously when the git modulus hits) into
a smooth trickle paced across the window.

Setting this to 0 collapses the spread to synchronous serial
iteration, which is what tests want."
  :type 'number
  :group 'claude-repl)

(defcustom claude-repl-state-spread-min-gap 0.05
  "Floor on the per-step gap inside the workspace-state update chain.
Computed as `(max this (/ claude-repl-state-spread-window N))' so
high workspace counts can't spawn very-fast `run-at-time' timers."
  :type 'number
  :group 'claude-repl)

(defcustom claude-repl-state-stale-threshold 5.0
  "Seconds after which an in-flight update chain is considered wedged.
`claude-repl--update-all-workspace-states' (the periodic timer
entrypoint) skips its tick when the previous chain has not finished.
If the in-flight marker is older than this threshold, the chain is
treated as stuck (likely due to an error in a per-step body that
escaped the `condition-case' net) and the flag is force-cleared so a
new chain can start.  Belt-and-braces against permanent wedging."
  :type 'number
  :group 'claude-repl)

(defcustom claude-repl-done-idle-delay 1
  "Seconds the user must focus a :done workspace before it decays to :idle.
The countdown starts when the workspace becomes the active workspace
\(or when :done arrives while it is already active).  Switching away
from a :done workspace before the delay elapses clears the timestamp,
so a quick transit through the tab does not silently strip the green
\"ready for review\" indicator — the user must return and dwell again."
  :type 'number
  :group 'claude-repl)

(defvar claude-repl--priority-images nil
  "Alist mapping priority strings (\"p05\" \"p1\" \"p2\" \"p3\") to Emacs image specs.")

;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; !! DO NOT REMOVE `claude-repl--tabline-space-toggle' OR ITS USAGE   !!
;; !! IN `claude-repl--tabline-advice',                                !!
;; !! `claude-repl--force-tab-bar-redraw', AND                         !!
;; !! `claude-repl--update-all-workspace-states'.                      !!
;; !!                                                                  !!
;; !! The tab-bar will NOT repaint unless the string it displays       !!
;; !! actually changes between ticks.  Toggling a trailing space on    !!
;; !! every poll cycle forces the tab-bar to detect a "new" string     !!
;; !! and re-render, giving us real-time visual updates.  Without      !!
;; !! this, state-color changes (thinking → done, etc.) are invisible  !!
;; !! until the user manually triggers a redisplay.                    !!
;; !!                                                                  !!
;; !! The toggle is read on TWO rendering paths:                       !!
;; !!  - `claude-repl--tabline-advice' (override of `+workspace--      !!
;; !!    tabline'), used by callers that still go through Doom's       !!
;; !!    workspace tabline API (e.g. echo-area helpers, tests).        !!
;; !!  - `claude-repl-workspace-tabline-formatted' /                   !!
;; !!    `claude-repl-current-workspace-name-segment', installed in    !!
;; !!    `tab-bar-format' below and therefore driving the visible      !!
;; !!    tab-bar.                                                      !!
;; !!                                                                  !!
;; !! Just flipping the toggle is NOT enough — Emacs's tab-bar caches  !!
;; !! the format result and will keep painting the cached value until  !!
;; !! something forces a re-read.  `claude-repl--force-tab-bar-redraw' !!
;; !! flips the toggle AND drives `tab-bar-tabs-set' /                 !!
;; !! `tab-bar--update-tab-bar-lines' / `force-mode-line-update' so    !!
;; !! the alternating string actually reaches the display.  The 1Hz   !!
;; !! `claude-repl--update-all-workspace-states' timer calls            !!
;; !! `--force-tab-bar-redraw' every tick.                              !!
;; !!                                                                  !!
;; !! This has been accidentally removed multiple times.  DO NOT       !!
;; !! remove it again.  It is NOT dead code.  It is NOT cosmetic.     !!
;; !! It is the mechanism that makes tab-bar updates work.             !!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(defvar claude-repl--tabline-space-toggle nil
  "Non-nil means append an extra trailing space to the tabline string.
Flipped on every poll cycle by `claude-repl--update-all-workspace-states'
\(via `claude-repl--force-tab-bar-redraw').  Read by
`claude-repl--tabline-advice' AND by
`claude-repl-workspace-tabline-formatted' /
`claude-repl-current-workspace-name-segment' (the functions installed
into `tab-bar-format') so both rendering paths produce an alternating
string that forces the tab-bar to repaint.  DO NOT REMOVE — see
comment above.")

(defvar claude-repl-hide-mode-enabled nil
  "Non-nil means persp-kill `:hidden' workspaces on workspace switch.
A workspace becomes `:hidden' when the user invokes `SPC o C' (the
deprio close path, `claude-repl--on-close').  The kill happens in
`claude-repl--sweep-hidden-workspaces' from the workspace-switch
handler via `claude-repl--nuke-one-workspace', which always preserves
the on-disk state file so the workspace can be re-opened later via
project switch.

The tab-bar itself is NOT filtered — it reflects the raw persp list.
Workspace cycling (`claude-repl-switch-left/right') skips `:hidden'
workspaces while hide-mode is on so the user does not land on a
soon-to-be-killed workspace mid-cycle.

The current workspace is exempt from sweep, and arriving on a `:hidden'
workspace resets its state to `:inactive' (so it survives the next
sweep).  To persistently keep a hidden workspace, toggle hide-mode off.

Toggle via `claude-repl-toggle-hide-mode'.")

(defun claude-repl--load-priority-images ()
  "Load priority badge PNGs from the module images/ directory.
Populates `claude-repl--priority-images' with display-ready image specs."
  (let* ((dir (file-name-directory (or load-file-name buffer-file-name)))
         (img-dir (expand-file-name "images/" dir))
         (names claude-repl-priority-levels)
         (height (frame-char-height)))
    (setq claude-repl--priority-images
          (cl-loop for name in names
                   for file = (expand-file-name (concat name ".png") img-dir)
                   when (file-exists-p file)
                   collect (cons name (create-image file 'png nil
                                                    :height height
                                                    :ascent 'center))))
    (claude-repl--log nil "load-priority-images: loaded=%d" (length claude-repl--priority-images))))

(when (image-type-available-p 'png)
  (claude-repl--load-priority-images))

(defun claude-repl--priority-image (priority)
  "Return the Emacs image spec for PRIORITY string, or nil."
  (cdr (assoc priority claude-repl--priority-images)))

(defun claude-repl--priority-rank (priority)
  "Return the sort rank for PRIORITY string; lower means higher precedence.
Ranks come from the position of PRIORITY in `claude-repl-priority-levels',
so adding levels there propagates without code changes here.  Returns
`most-positive-fixnum' for nil or unrecognized values so they sort after
every recognized priority."
  (or (and priority (cl-position priority claude-repl-priority-levels :test #'equal))
      most-positive-fixnum))

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
ordering bugs."
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

;;; Workspace state accessors ------------------------------------------------

;; --- Two-axis state model (analysis #8) ---
;;
;; Workspace state is split into two orthogonal plist keys:
;;   :claude-state — Claude-owned lifecycle.  Values: nil | :init |
;;                   :idle | :thinking | :done | :permission.
;;                   Written primarily by hook sentinels; narrow
;;                   Emacs-side exceptions at lifecycle boundaries
;;                   (initialize-claude writes :init; kill clears).
;;   :repl-state   — Emacs-owned session-lifecycle flag.  Values:
;;                     nil       — workspace registered, no Claude
;;                                 session has ever been attached.
;;                     :active   — panels open, session running.
;;                     :inactive — panels closed, session preserved.
;;                     :dead     — vterm process has died.
;;                   Only :dead contributes to tab display (❌ badge);
;;                   other values are bookkeeping only.

(defun claude-repl--ws-state (ws)
  "Return the current :claude-state keyword for workspace WS, or nil.
Compat shim: equivalent to `claude-repl--ws-claude-state', retained for
test callers that have not yet migrated."
  (claude-repl--ws-get ws :claude-state))

(defun claude-repl--ws-claude-state (ws)
  "Return the current :claude-state keyword for workspace WS, or nil."
  (claude-repl--ws-get ws :claude-state))

(defun claude-repl--ws-repl-state (ws)
  "Return the current :repl-state keyword for workspace WS, or nil."
  (claude-repl--ws-get ws :repl-state))

(defun claude-repl--ws-set-claude-state (ws state)
  "Set workspace WS's :claude-state to STATE.
STATE is one of: nil, :init, :idle, :thinking, :done, :permission."
  (unless ws (error "claude-repl--ws-set-claude-state: ws is nil"))
  (claude-repl--log ws "claude-state %s -> %s" ws state)
  (claude-repl--ws-put ws :claude-state state)
  (force-mode-line-update t)
  (claude-repl--memory-state-save ws))

(defun claude-repl--ws-set-repl-state (ws state)
  "Set workspace WS's :repl-state to STATE.
STATE is one of:
  nil        — freshly killed / no session
  :active    — panels displayed, session alive
  :inactive  — panels hidden, session alive (plain `SPC o c' close)
  :hidden    — semantically `:inactive', but additionally marks the
               workspace for persp-kill on the next workspace change
               when `claude-repl-hide-mode-enabled' is non-nil.  Set
               by the `SPC o C' deprio-close path; the kill happens in
               `claude-repl--sweep-hidden-workspaces' from the
               workspace-switch handler.  The on-disk state file is
               always preserved by `--nuke-one-workspace' so the
               workspace can be re-opened later via project-switch.
  :merged    — workspace's branch has been merged into its source.
               Set by `claude-repl--workspace-merge-do' on success
               (alongside `:merge-completed t').  Takes precedence
               over `:dead' so the 🔀 badge survives the post-merge
               nuke-and-poll cycle that would otherwise mark the
               (now-vterm-less) workspace dead.
  :dead      — vterm process gone

The orthogonal `:done-acked' boolean tracks whether the user has seen
the current `:claude-state :done' result.  It used to be the
`:repl-state :viewed' value but was lifted out — viewing isn't a
lifecycle phase, it's an acknowledgment flag that overlays :done.

Persists the new value to disk via `claude-repl--state-save' when STATE
is `:active', `:inactive', or `:hidden' so panel-visibility (and the
deprio-hide marker) survives Emacs restart.  `:dead' / nil are not
persisted — they reduce to \"no opinion\" at restart, so default
open-panels behavior applies.  `:dead' is set via `--ws-put' directly
(in `--mark-dead-vterm'), bypassing this setter, so no special-case is
needed there."
  (unless ws (error "claude-repl--ws-set-repl-state: ws is nil"))
  (claude-repl--log ws "repl-state %s -> %s" ws state)
  (claude-repl--ws-put ws :repl-state state)
  (force-mode-line-update t)
  (when (memq state '(:active :inactive :hidden))
    (claude-repl--state-save ws))
  (claude-repl--memory-state-save ws))

(defun claude-repl--ws-claude-state-clear-if (ws state)
  "Clear WS's :claude-state when it currently equals STATE.
Compare-and-clear: no-op if the current value is not STATE."
  (unless ws (error "claude-repl--ws-claude-state-clear-if: ws is nil"))
  (if (eq (claude-repl--ws-get ws :claude-state) state)
      (progn
        (claude-repl--log ws "claude-state-clear-if %s %s -> nil" ws state)
        (claude-repl--ws-put ws :claude-state nil)
        (force-mode-line-update t))
    (claude-repl--log-verbose ws
                              "claude-state-clear-if ws=%s state=%s no-op (current=%s)"
                              ws state (claude-repl--ws-get ws :claude-state))))

;; --- Stop / SubagentStop coordination ---
;;
;; The Stop hook fires when Claude finishes its main response.  When
;; Claude has spawned background subagents (Task tool with
;; run_in_background: true), Stop can fire while those subagents are
;; still running — so transitioning the workspace to :done on Stop alone
;; would falsely advertise "ready for review" while work is still in
;; flight.
;;
;; To gate the transition correctly we track two pieces of state:
;;
;;   :stop-received      - boolean, set by the Stop hook callback.
;;   :pending-subagents  - integer counter, incremented by SubagentStart
;;                         and decremented by SubagentStop.
;;
;; The transition to :done happens when both conditions are true:
;; Stop has fired AND the counter is zero.  Whichever event resolves
;; that conjunction (Stop arriving last, or the final SubagentStop
;; arriving last) triggers `claude-repl--handle-claude-finished'.
;;
;; Empirical hook asymmetry (verified 2026-05-05):
;;
;; Claude Code fires SubagentStop *every turn*, not just per real
;; subagent — even on a turn that invokes zero Task/Agent tools we see
;; an unpaired SubagentStop arrive ~1–2s after Stop.  Best guess: the
;; main agent's own end-of-turn fires both Stop (outer) and SubagentStop
;; (inner), making the hooks asymmetric (N starts → N+1 stops per turn).
;;
;; This means the floor-at-zero in `decf-pending-subagents' is
;; LOAD-BEARING ON EVERY TURN, not a defensive guard for rare edge
;; cases.  Without it, the counter would drift toward -infinity over
;; a long session.
;;
;; Why this still works correctly:
;;   - clear-stop-tracking inside maybe-finalize-stop resets the counter
;;     to 0 *before* the phantom arrives (the phantom lands ~1–2s after
;;     Stop processing completes).
;;   - The phantom hits decf with current=0; floor-at-zero clamps it to
;;     0; net effect is a no-op.
;;   - Steady state between turns: counter=0.
;;
;; Known narrow risk: cross-turn race.  If the user submits a new prompt
;; very fast (within the ~2s phantom-arrival window) AND the new turn
;; spawns multiple background subagents, the lingering phantom can
;; cancel out one real SubagentStop and Stop may then false-finalize
;; while a real subagent is still running.  Mitigation if this becomes
;; observable: reset the counter on prompt-submit (treat each new turn
;; as a fresh tracking window).  Not implemented today.

(defun claude-repl--ws-stop-received-p (ws)
  "Return non-nil if the Stop hook has fired for workspace WS without
having yet been resolved into a `:done' transition."
  (claude-repl--ws-get ws :stop-received))

(defun claude-repl--ws-set-stop-received (ws val)
  "Set workspace WS's :stop-received flag to VAL (a boolean)."
  (unless ws (error "claude-repl--ws-set-stop-received: ws is nil"))
  (claude-repl--log ws "stop-received %s -> %s" ws val)
  (claude-repl--ws-put ws :stop-received val))

(defun claude-repl--ws-pending-subagents (ws)
  "Return WS's pending-subagent count (0 when unset)."
  (or (claude-repl--ws-get ws :pending-subagents) 0))

(defun claude-repl--ws-incf-pending-subagents (ws)
  "Increment WS's pending-subagent counter and return the new value."
  (unless ws (error "claude-repl--ws-incf-pending-subagents: ws is nil"))
  (let ((new (1+ (claude-repl--ws-pending-subagents ws))))
    (claude-repl--log ws "pending-subagents %s -> %d (incf)" ws new)
    (claude-repl--ws-put ws :pending-subagents new)
    new))

(defun claude-repl--ws-decf-pending-subagents (ws)
  "Decrement WS's pending-subagent counter and return the new value.

Floors at 0.  This is LOAD-BEARING ON EVERY TURN, not a defensive
edge-case guard: Claude Code empirically fires one unpaired
SubagentStop per turn (see the block comment above for the
N-starts/N+1-stops asymmetry).  Without the floor, the counter would
drift toward -infinity across a session and the gating predicate
`(zerop ...)' would silently start mis-classifying \"still running\"
as \"all done\"."
  (unless ws (error "claude-repl--ws-decf-pending-subagents: ws is nil"))
  (let* ((cur (claude-repl--ws-pending-subagents ws))
         (new (max 0 (1- cur))))
    (claude-repl--log ws "pending-subagents %s -> %d (decf, was %d)" ws new cur)
    (claude-repl--ws-put ws :pending-subagents new)
    new))

(defun claude-repl--fully-stopped-p (ws)
  "Return non-nil when WS is fully stopped — Stop fired and no pending subagents.
Used by Stop and SubagentStop callbacks to decide whether to drive the
`:thinking → :done' transition.  See the block comment above for the
coordination model."
  (and (claude-repl--ws-stop-received-p ws)
       (zerop (claude-repl--ws-pending-subagents ws))))

(defun claude-repl--ws-clear-stop-tracking (ws)
  "Reset the Stop / SubagentStop tracking fields on WS.
Called when the workspace transitions out of `:thinking' so the next
turn starts from a clean slate.  Resets `:stop-received' to nil and
`:pending-subagents' to 0."
  (unless ws (error "claude-repl--ws-clear-stop-tracking: ws is nil"))
  (claude-repl--log ws "clear-stop-tracking %s" ws)
  (claude-repl--ws-put ws :stop-received nil)
  (claude-repl--ws-put ws :pending-subagents 0))

;; Legacy APIs below delegate into the typed setters.  Call sites migrate
;; to the typed names in a later commit; retained here for the duration
;; of the migration so every existing caller keeps working.

(defun claude-repl--ws-set (ws state)
  "Set workspace WS to STATE.
Thin wrapper around `claude-repl--ws-set-claude-state' preserved for
callers that have not yet migrated to the typed setter.
STATE is one of: :thinking, :done, :permission, :inactive."
  (claude-repl--ws-set-claude-state ws state))

(defun claude-repl--ws-dir (ws)
  "Return the project root directory for workspace WS.
Reads :project-dir from the workspace plist.  Errors if not set."
  (or (claude-repl--ws-get ws :project-dir)
      (error "claude-repl--ws-dir: no :project-dir for workspace %s" ws)))

;;; Git status (async) -------------------------------------------------------

(defun claude-repl--workspace-clean-p (ws)
  "Return non-nil if workspace WS has no unstaged changes to tracked files.
Reads from a cached value updated asynchronously by
`claude-repl--async-refresh-git-status'.  Signals an error if the cache
has not yet been populated — callers must ensure the async git check has
completed before consulting this predicate."
  (let ((status (claude-repl--ws-get ws :git-clean)))
    (unless status
      (error "claude-repl--workspace-clean-p: :git-clean not populated for workspace %s" ws))
    (let ((result (eq status 'clean)))
      (claude-repl--log-verbose ws "workspace-clean-p ws=%s status=%s result=%s" ws status result)
      result)))

(defun claude-repl--git-check-in-progress-p (ws)
  "Return non-nil if a git-diff process is already running for workspace WS."
  (let ((result (when-let ((proc (claude-repl--ws-get ws :git-proc)))
                  (process-live-p proc))))
    (claude-repl--log-verbose ws "git-check-in-progress-p ws=%s result=%s" ws result)
    result))

(defun claude-repl--git-diff-sentinel (ws proc _event)
  "Process sentinel for `git diff --quiet' in workspace WS.
When PROC finishes, records `:git-clean' as `clean' or `dirty' and
triggers a state update via `claude-repl--update-ws-state'.
_EVENT is ignored."
  (unless (process-live-p proc)
    (let* ((exit-code (process-exit-status proc))
           (clean-result (cond
                          ((= 0 exit-code) 'clean)
                          ((= 1 exit-code) 'dirty)
                          (t (message "[claude-repl] WARNING: git diff --quiet exited with code %d for ws=%s (git error, not dirty)"
                                      exit-code ws)
                             (claude-repl--log ws "git-diff-sentinel: unexpected exit-code=%d for ws=%s" exit-code ws)
                             nil))))
      (claude-repl--log-verbose ws "git-diff-sentinel: ws=%s exit-code=%s result=%s" ws exit-code clean-result)
      (when clean-result
        (claude-repl--ws-put ws :git-clean clean-result))
      (claude-repl--ws-put ws :git-proc nil)
      (claude-repl--update-ws-state ws))))

(defun claude-repl--async-refresh-git-status (ws)
  "Asynchronously refresh the git cleanliness cache for workspace WS.
Starts `git diff --quiet' in WS's directory.  On exit, sets `:git-clean'
to `clean' or `dirty' in the workspace plist and calls
`claude-repl--update-ws-state' to apply any resulting state transition.
A no-op if a check is already in progress for WS."
  (when-let ((dir (claude-repl--ws-dir ws)))
    (if (claude-repl--git-check-in-progress-p ws)
        (claude-repl--log-verbose ws "async-refresh-git-status: ws=%s skipped (already in progress)" ws)
      (claude-repl--log-verbose ws "async-refresh-git-status: ws=%s starting git diff" ws)
      (let* ((default-directory dir)
             (proc (make-process
                    :name (format "claude-repl-git-%s" ws)
                    :command '("git" "diff" "--quiet")
                    :connection-type 'pipe
                    :noquery t
                    :buffer nil
                    :sentinel (apply-partially
                               #'claude-repl--git-diff-sentinel ws))))
        (claude-repl--ws-put ws :git-proc proc)))))

;;; Buffer/workspace resolution -----------------------------------------------

(defun claude-repl--workspace-for-buffer (buf)
  "Return the workspace name that contains BUF, or nil."
  (when (bound-and-true-p persp-mode)
    (cl-loop for persp in (persp-persps)
             when (persp-contain-buffer-p buf persp)
             return (safe-persp-name persp))))

;;; Tab-bar rendering ---------------------------------------------------------
;;
;; Appearance is described by a small pyramid:
;;
;;   1. Named constants — every color / label / font-weight literal lives
;;      in a `claude-repl--color-*' / `--label-*' / `--tab-weight' defconst.
;;   2. `claude-repl--tab-default' and `claude-repl--tab-palette' — the
;;      two defconsts that compose those named values into per-state
;;      appearance specs.  No palette row contains a string literal.
;;   3. Faces — four `defface' forms that reference the same named
;;      constants (Doom theming hook).
;;   4. Renderers — take a spec, emit a propertized string.
;;
;; Palette shape (per-state):
;;   :face       — defface name for unselected tabs.
;;   :label      — optional bracket content override (e.g. the permission
;;                 glyph).
;;   :unselected — plist describing unselected appearance.
;;   :selected   — plist describing selected appearance.
;;
;; Spec plist keys:
;;   :bg          — bracket (and separator) background.
;;   :fg          — separator foreground.
;;   :bracket-fg  — [LABEL] foreground.
;;   :bracket-bg  — [LABEL] background (optional; falls back to :bg).
;;   :weight      — font weight (default `bold').
;;
;; Use `unspecified' (the symbol) for "inherit from frame default".

;; --- Named color / style constants --- ;;

(defconst claude-repl--color-init-blue        "#3366cc"
  "Blue used for the :init claude-state tab background.")

(defconst claude-repl--color-thinking-red     "#cc3333"
  "Red used for the :thinking claude-state tab background.")

(defconst claude-repl--color-done-green       "#1a7a1a"
  "Dark green used for :done and :permission tab backgrounds.")

(defconst claude-repl--color-idle-orange      "#d97706"
  "Orange used for the :idle claude-state tab background.
:idle means \"session alive, awaiting prompt or decayed from :done\" — an
explicit palette entry (not a fallback) so idle workspaces are
visually distinct from states that have no palette mapping.")

(defconst claude-repl--color-stop-failed-magenta "#8b1f8b"
  "Magenta used for the :stop-failed claude-state tab background.
:stop-failed means the StopFailure hook fired — Claude's turn ended
due to an API error (rate limit, auth failure, billing, etc.).  The
vterm session is still alive and re-promptable; :dead (the plain ❌
badge) is reserved for vterm process death.  A distinct color signals
\"needs your attention, but not the same kind of attention as :thinking
or :dead\".")

(defconst claude-repl--color-done-green-bright "#2a8c2a"
  "Brighter green used for :done / :permission bracket-fg on selected
tabs; readable against `claude-repl--color-selected-bg'.")

(defconst claude-repl--color-default-bracket  "white"
  "White used for bracket numerals on unselected tabs of any state.")

(defconst claude-repl--color-selected-bg      "#c0c0c0"
  "Grey used for the background of selected tabs.")

(defconst claude-repl--color-light            "white"
  "Light foreground for dark state backgrounds.")

(defconst claude-repl--color-dark             "black"
  "Dark foreground for light state backgrounds.")

(defconst claude-repl--label-permission       "❓"
  "Bracket label shown adjacent to the numeric index when Claude is
asking for a permission decision.")

(defconst claude-repl--label-dead             "❌"
  "Bracket label shown adjacent to the numeric index when the vterm
process has died.")

(defconst claude-repl--label-stop-failed      "⚠"
  "Bracket label shown adjacent to the numeric index when the
StopFailure hook fired (turn ended on an API error, but the vterm
session is still alive and re-promptable).")

(defconst claude-repl--label-merge-conflict   "💥"
  "Bracket label shown adjacent to the numeric index when a workspace's
merge was rejected by a cherry-pick conflict (real conflict markers
left behind, auto-resolver declined or interactive abort).  Distinct
from `:dead' (vterm died) and `:merge-failed' (silent git-aborted, no
CHERRY_PICK_HEAD) — collision metaphor reflects the actual conflict.")

(defconst claude-repl--label-merged           "🔀"
  "Bracket label shown adjacent to the numeric index when the
workspace's branch has been merged into its source (`:repl-state'
`:merged').  Takes precedence over the `:dead' badge so a merged
workspace whose vterm has since died still reads as merged, not
dead.")

(defconst claude-repl--label-merge-failed     "⛔"
  "Bracket label shown adjacent to the numeric index when a workspace's
merge dispatch failed silently (`:repl-state' `:merge-failed') —
typically because the source repo is mid cherry-pick/rebase/merge and
git refused the cherry-pick.  Distinct from `:dead' (❌) so a stuck
merge does not look like a dead vterm at a glance.")

(defconst claude-repl--tab-weight             'bold
  "Font weight applied to every tab face.")

(defconst claude-repl--color-flash-bg         "#3b82f6"
  "Saturated blue used for the transient flash face — see `claude-repl-flash-tab'.
Distinct from `claude-repl--color-init-blue' so a flash is not confused
with the :init claude-state at a glance.")

(defcustom claude-repl-flash-count 2
  "Number of on/off cycles when `claude-repl-flash-tab' pulses a tab."
  :type 'integer
  :group 'claude-repl)

(defcustom claude-repl-flash-duration 1.0
  "Total duration of a `claude-repl-flash-tab' pulse, in seconds.
Distributed evenly across `claude-repl-flash-count' on/off cycles."
  :type 'number
  :group 'claude-repl)

;; --- Appearance palette --- ;;

(defconst claude-repl--tab-default
  `(:unselected (:bg unspecified
                 :fg unspecified
                 :bracket-fg ,claude-repl--color-default-bracket
                 :weight ,claude-repl--tab-weight)
    :selected   (:bg ,claude-repl--color-selected-bg
                 :fg ,claude-repl--color-dark
                 :bracket-fg ,claude-repl--color-dark
                 :weight ,claude-repl--tab-weight))
  "Default tab-appearance spec for states absent from `claude-repl--tab-palette'.")

(defconst claude-repl--tab-palette
  `((:init
     :face       claude-repl-tab-init
     :unselected (:bg ,claude-repl--color-init-blue
                  :fg ,claude-repl--color-light
                  :bracket-fg ,claude-repl--color-default-bracket
                  :weight ,claude-repl--tab-weight)
     :selected   (:bg ,claude-repl--color-selected-bg
                  :fg ,claude-repl--color-dark
                  :bracket-bg ,claude-repl--color-init-blue
                  :bracket-fg ,claude-repl--color-light
                  :weight ,claude-repl--tab-weight))
    (:thinking
     :face       claude-repl-tab-thinking
     :unselected (:bg ,claude-repl--color-thinking-red
                  :fg ,claude-repl--color-light
                  :bracket-fg ,claude-repl--color-default-bracket
                  :weight ,claude-repl--tab-weight)
     :selected   (:bg ,claude-repl--color-selected-bg
                  :fg ,claude-repl--color-dark
                  :bracket-bg ,claude-repl--color-thinking-red
                  :bracket-fg ,claude-repl--color-light
                  :weight ,claude-repl--tab-weight))
    (:done
     :face       claude-repl-tab-done
     :unselected (:bg ,claude-repl--color-done-green
                  :fg ,claude-repl--color-dark
                  :bracket-fg ,claude-repl--color-default-bracket
                  :weight ,claude-repl--tab-weight)
     :selected   (:bg ,claude-repl--color-selected-bg
                  :fg ,claude-repl--color-dark
                  :bracket-bg ,claude-repl--color-done-green
                  :bracket-fg ,claude-repl--color-light
                  :weight ,claude-repl--tab-weight))
    (:permission
     :face       claude-repl-tab-permission
     :label      ,claude-repl--label-permission
     :unselected (:bg ,claude-repl--color-done-green
                  :fg ,claude-repl--color-dark
                  :bracket-fg ,claude-repl--color-default-bracket
                  :weight ,claude-repl--tab-weight)
     :selected   (:bg ,claude-repl--color-selected-bg
                  :fg ,claude-repl--color-dark
                  :bracket-bg ,claude-repl--color-done-green
                  :bracket-fg ,claude-repl--color-light
                  :weight ,claude-repl--tab-weight))
    (:idle
     :face       claude-repl-tab-idle
     :unselected (:bg ,claude-repl--color-idle-orange
                  :fg ,claude-repl--color-dark
                  :bracket-fg ,claude-repl--color-default-bracket
                  :weight ,claude-repl--tab-weight)
     :selected   (:bg ,claude-repl--color-selected-bg
                  :fg ,claude-repl--color-dark
                  :bracket-bg ,claude-repl--color-idle-orange
                  :bracket-fg ,claude-repl--color-light
                  :weight ,claude-repl--tab-weight))
    (:stop-failed
     :face       claude-repl-tab-stop-failed
     :label      ,claude-repl--label-stop-failed
     :unselected (:bg ,claude-repl--color-stop-failed-magenta
                  :fg ,claude-repl--color-light
                  :bracket-fg ,claude-repl--color-default-bracket
                  :weight ,claude-repl--tab-weight)
     :selected   (:bg ,claude-repl--color-selected-bg
                  :fg ,claude-repl--color-dark
                  :bracket-bg ,claude-repl--color-stop-failed-magenta
                  :bracket-fg ,claude-repl--color-light
                  :weight ,claude-repl--tab-weight))
    (:dead
     :label      ,claude-repl--label-dead)
    (:merge-conflict
     :label      ,claude-repl--label-merge-conflict)
    (:merge-failed
     :label      ,claude-repl--label-merge-failed)
    (:merged
     :label      ,claude-repl--label-merged))
  "Per-state tab-appearance palette.
Each entry fully describes both selected and unselected looks for a
claude-state keyword via nested `:unselected' and `:selected' plists.
`:repl-state :inactive' does not contribute to color (it is bookkeeping
only).")

(defun claude-repl--tab-spec (state selected)
  "Return the appearance spec (plist) for STATE with SELECTED flag.
Falls back to `claude-repl--tab-default' when STATE has no palette entry.
Keys in the returned plist: :bg :fg :bracket-fg :bracket-bg :weight."
  (let* ((row (alist-get state claude-repl--tab-palette))
         (key (if selected :selected :unselected)))
    (or (plist-get row key)
        (plist-get claude-repl--tab-default key))))

(defun claude-repl--tab-spec-bracket-only (state selected)
  "Return appearance spec applying STATE's color to the [N] bracket only.
Pulls bracket-bg/bracket-fg/weight from STATE's palette row (per
SELECTED) and leaves :bg/:fg unspecified so the separator and name
region inherit defaults.  Used for workspaces whose Claude panels
have been dismissed: the bracket retains the state's color so the
workspace's claude-state stays visible while the rest of the tab
falls back to the default appearance."
  (let* ((full (claude-repl--tab-spec state selected))
         (bracket-bg (or (plist-get full :bracket-bg)
                         (plist-get full :bg))))
    `(:bg unspecified
      :fg unspecified
      :bracket-bg ,bracket-bg
      :bracket-fg ,(plist-get full :bracket-fg)
      :weight ,(or (plist-get full :weight) claude-repl--tab-weight))))

;; --- defface forms referencing the named constants --- ;;
;; Each `:unselected' palette row has the same colors these forms read,
;; by construction.  Kept as explicit defface calls so Doom users can
;; customize via `customize-face' (the Doom theming hook).

(defface claude-repl-tab-init
  `((t :background ,claude-repl--color-init-blue
       :foreground ,claude-repl--color-light
       :weight ,claude-repl--tab-weight))
  "Face for workspace tabs where Claude is initializing (blue).")

(defface claude-repl-tab-thinking
  `((t :background ,claude-repl--color-thinking-red
       :foreground ,claude-repl--color-light
       :weight ,claude-repl--tab-weight))
  "Face for workspace tabs where Claude is thinking (red).")

(defface claude-repl-tab-done
  `((t :background ,claude-repl--color-done-green
       :foreground ,claude-repl--color-dark
       :weight ,claude-repl--tab-weight))
  "Face for workspace tabs where Claude is done (green).")

(defface claude-repl-tab-permission
  `((t :background ,claude-repl--color-done-green
       :foreground ,claude-repl--color-dark
       :weight ,claude-repl--tab-weight))
  "Face for workspace tabs where Claude needs permission (green + emoji).")

(defface claude-repl-tab-idle
  `((t :background ,claude-repl--color-idle-orange
       :foreground ,claude-repl--color-dark
       :weight ,claude-repl--tab-weight))
  "Face for workspace tabs where Claude is idle (orange).")

(defface claude-repl-tab-stop-failed
  `((t :background ,claude-repl--color-stop-failed-magenta
       :foreground ,claude-repl--color-light
       :weight ,claude-repl--tab-weight))
  "Face for workspace tabs where the last turn failed via the
StopFailure hook (magenta + ⚠).")

(defface claude-repl-tab-flash
  `((t :background ,claude-repl--color-flash-bg
       :foreground ,claude-repl--color-light
       :weight ,claude-repl--tab-weight))
  "Transient face applied while a workspace is in a `claude-repl-flash-tab'
pulse — solid blue background regardless of the underlying state.")

(defun claude-repl--ws-flashing-p (ws)
  "Return non-nil if workspace WS is currently in a flash pulse."
  (claude-repl--ws-get ws :flashing))

(defun claude-repl--ws-set-flashing (ws val)
  "Set workspace WS's :flashing flag to VAL.
The tab renderer treats non-nil as an instruction to paint the tab with
the flash face/spec on the next refresh."
  (claude-repl--ws-put ws :flashing val))

(defun claude-repl--flash-spec ()
  "Return the appearance spec plist used for a flashing tab.
Mirrors a normal palette row (see `claude-repl--tab-palette' docstring)
but paints both the bracket and the name region in a uniform blue."
  `(:bg ,claude-repl--color-flash-bg
    :fg ,claude-repl--color-light
    :bracket-bg ,claude-repl--color-flash-bg
    :bracket-fg ,claude-repl--color-light
    :weight ,claude-repl--tab-weight))

(defun claude-repl--force-tab-bar-redraw ()
  "Force the tab-bar to repaint NOW, bypassing its string-equality cache.
Tab-bar rendering caches by string equality, and `equal' on propertized
strings ignores text properties — so changes that only differ in face
\(e.g. a `:flashing' toggle\) won't trigger a repaint via
`force-mode-line-update' alone.  This helper flips the load-bearing
`claude-repl--tabline-space-toggle' so the next tabline render produces
a different string, then drives the tab-bar update primitives.  See the
block comment above the toggle's defvar for the rationale."
  (setq claude-repl--tabline-space-toggle (not claude-repl--tabline-space-toggle))
  (when (fboundp 'tab-bar-tabs-set)
    (tab-bar-tabs-set (tab-bar-tabs)))
  (when (fboundp 'tab-bar--update-tab-bar-lines)
    (tab-bar--update-tab-bar-lines t))
  (force-mode-line-update t))

(defun claude-repl--flash-step (ws step total-steps interval)
  "Drive step STEP of WS's flash, then chain the next step via `run-at-time'.
STEP is 0-based.  TOTAL-STEPS is `(1+ (* 2 COUNT))' — one entry for
each on/off toggle plus a final cleanup.  Even STEPs paint the flash
on, odd STEPs paint it off.  The terminal step (STEP == TOTAL-STEPS-1)
clears `:flashing' and stops the chain — it does NOT schedule a
successor.  Every step calls `claude-repl--force-tab-bar-redraw' so
the tab repaints at flash speed instead of waiting for the 1-Hz poll."
  (if (>= step (1- total-steps))
      (progn
        (claude-repl--ws-set-flashing ws nil)
        (claude-repl--force-tab-bar-redraw))
    (claude-repl--ws-set-flashing ws (cl-evenp step))
    (claude-repl--force-tab-bar-redraw)
    (run-at-time interval nil
                 #'claude-repl--flash-step ws (1+ step) total-steps interval)))

(defun claude-repl-flash-tab (ws &optional count duration)
  "Pulse the tab for workspace WS COUNT times across DURATION seconds.
COUNT defaults to `claude-repl-flash-count'; DURATION defaults to
`claude-repl-flash-duration'.  Used to draw the user's attention to a
workspace whose tab-bar position just changed (e.g., after a deprio
push-to-back), so the eye can track it to its new home.

Drives the sequence via `claude-repl--flash-step', which runs the
first step synchronously and then chains each subsequent step from
the previous one via `run-at-time'.  Versus scheduling every toggle
up-front, the chain gives uniform real-time spacing when Emacs is
busy, makes mid-sequence cancellation easy (only one timer is ever
pending), and avoids the closure-capture pitfalls of shared loop
variables."
  (let* ((count (or count claude-repl-flash-count))
         (duration (or duration claude-repl-flash-duration))
         (interval (/ duration (* 2.0 count)))
         (total-steps (1+ (* 2 count))))
    (claude-repl--log ws "flash-tab ws=%s count=%d duration=%s" ws count duration)
    (claude-repl--flash-step ws 0 total-steps interval)))

(defun claude-repl--flash-current-tab ()
  "Pulse the current workspace's tab via `claude-repl-flash-tab'.
Centralizes the post-jump flash so every identity-based workspace jump
draws the eye to the destination tab uniformly.  No-op when
`claude-repl-flash-tab' or `+workspace-current-name' is unbound — those
come from optional layers that may not be loaded yet at startup."
  (when (and (fboundp 'claude-repl-flash-tab)
             (fboundp '+workspace-current-name))
    (claude-repl-flash-tab (+workspace-current-name))))

(defun claude-repl--render-tab (name spec label name-face img-str)
  "Render a tab string for workspace NAME from SPEC.
SPEC is a plist with keys :bg :fg :bracket-fg :weight (see
`claude-repl--tab-palette' docstring).  NAME-FACE is applied to the
workspace-name portion.  LABEL is the bracket content (number or
emoji).  IMG-STR, when non-nil, is inserted between bracket and name
with a single un-faced space on each side so the image does not butt
up against the name's background.

The string ends with an un-faced trailing space so each entry
self-terminates.  Emacs's `display_tab_bar_line' calls
`extend_face_to_end_of_line', which paints the row's last glyph face
across the remainder regardless of `:extend' — without the unfaced
terminator, the name-face background would bleed to the right edge
whenever an entry landed at a wrap (or the final row's) end."
  (let* ((bg         (or (plist-get spec :bg)         'unspecified))
         (fg         (or (plist-get spec :fg)         'unspecified))
         (bracket-bg (or (plist-get spec :bracket-bg) bg))
         (bracket-fg (or (plist-get spec :bracket-fg) 'unspecified))
         (weight     (or (plist-get spec :weight)     'normal))
         (separator-face `(:background unspecified :foreground ,fg :weight ,weight))
         (bracket-face   `(:background ,bracket-bg  :foreground ,bracket-fg :weight ,weight)))
    (concat (propertize " " 'face separator-face)
            (propertize (format claude-repl-tab-bracket-format label) 'face bracket-face)
            (when img-str (concat " " img-str " "))
            (propertize (format claude-repl-tab-name-padding name) 'face name-face)
            " ")))

(defun claude-repl--tab-label (state index)
  "Return the tab label for STATE and numeric INDEX.
When the palette defines a `:label' for STATE (e.g. \"❓\" for permission,
\"❌\" for dead), the suffix is appended after the numeric index
\(e.g. \"1❓\").  Otherwise returns the index as a plain string."
  (let ((suffix (plist-get (alist-get state claude-repl--tab-palette) :label)))
    (if suffix
        (concat (number-to-string index) suffix)
      (number-to-string index))))

(defun claude-repl--tab-face (state selected)
  "Return the face symbol for the NAME portion of a tab.
For unselected tabs, uses the palette row's `:face' or falls back to
`+workspace-tab-face'.  For selected tabs, always uses
`+workspace-tab-selected-face' so selection dims the state color."
  (if selected
      '+workspace-tab-selected-face
    (or (plist-get (alist-get state claude-repl--tab-palette) :face)
        '+workspace-tab-face)))

(defun claude-repl--tab-priority-image-str (name)
  "Return a propertized image string for workspace NAME's priority, or nil."
  (when-let ((priority (claude-repl--ws-get name :priority)))
    (when-let ((img (claude-repl--priority-image priority)))
      (propertize " " 'display img))))

(defun claude-repl--composed-state (claude repl &optional ws)
  "Project CLAUDE and REPL onto the palette's display key.
Tab color is primarily a function of `:claude-state'.  Two
`:repl-state' values contribute: `:merged' (workspace's branch has
been merged into its source, shows the 🔀 badge) and `:dead' (vterm
died, shows the ❌ badge).  `:merged' takes precedence over `:dead'
so a merged workspace whose vterm has since died still reads as
merged.

Optional WS is threaded through for diagnostic logging only; it does
not affect the mapping.

Every known claude-state is mapped explicitly.  Unknown states error
hard — no silent fallback.

Rule:
  :thinking    → :thinking                  (red)
  :permission  → :permission                 (green + ❓)
  :init        → :init                       (blue — Claude starting)
  :done        → :done                       (green — unacknowledged work)
  :idle        → :idle                       (orange)
  :stop-failed → :stop-failed                (magenta + ⚠ — turn errored)
  nil + :merged         → :merged            (default + 🔀)
  nil + :merge-conflict → :merge-conflict    (default + 💥)
  nil + :merge-failed   → :merge-failed      (default + ⛔)
  nil + :dead           → :dead              (default + ❌)
  nil                   → nil                (no session / unborn)"
  (cond
   ((eq claude :thinking)    :thinking)
   ((eq claude :permission)  :permission)
   ((eq claude :init)        :init)
   ((eq claude :done)        :done)
   ((eq claude :idle)        :idle)
   ((eq claude :stop-failed) :stop-failed)
   ((and (null claude) (eq repl :merged)) :merged)
   ;; `:merge-conflict' takes precedence over `:dead' so a workspace
   ;; whose vterm later dies still surfaces the conflict signal — the
   ;; merge failure is the more actionable badge.
   ((and (null claude) (eq repl :merge-conflict)) :merge-conflict)
   ;; `:merge-failed' (silent cherry-pick abort, no CHERRY_PICK_HEAD)
   ;; ranks above `:dead' for the same reason: a blocked merge is the
   ;; actionable signal, the dead vterm is incidental.
   ((and (null claude) (eq repl :merge-failed)) :merge-failed)
   ((and (null claude) (eq repl :dead)) :dead)
   ((null claude)           nil)
   (t
    ;; DIAG: we expected this branch to be unreachable — every writer of
    ;; :claude-state goes through the typed setter and passes one of the
    ;; documented keywords. If we hit this, some other path is leaking an
    ;; unexpected value into the hashmap. Log once per value and return
    ;; nil so redisplay doesn't blank the tab-bar while we investigate.
    (claude-repl--log ws
                      "composed-state: UNKNOWN claude-state %S (type=%s) — returning nil"
                      claude (type-of claude))
    nil)))

(defun claude-repl--ws-display-state (ws)
  "Return the palette display key for WS.
Reads `:claude-state' as the source of truth for tab color when the
panels are visible.  When the composed state is non-nil AND no Claude
panel is present in WS's live-or-saved window layout, returns nil
regardless of state — this suppresses full-tab coloring (state-colored
name and label badges like ❓/❌/⚠) for workspaces whose panels the
user has dismissed.  `:claude-state' is preserved on the plist so the
original color reappears the next time the user reopens panels.  The
nil-state shortcut avoids calling `claude-repl--ws-claude-open-p' on
workspaces that have no state to suppress in the first place.

NOTE: this function answers the question \"what state should drive the
full tab appearance?\".  The orthogonal question \"what state should
color the [N] bracket alone?\" is answered by
`claude-repl--ws-bracket-state', which ignores panel visibility so the
bracket keeps its color when panels are closed."
  (let ((state (claude-repl--composed-state (claude-repl--ws-claude-state ws)
                                            (claude-repl--ws-repl-state ws)
                                            ws)))
    (if (and state (not (claude-repl--ws-claude-open-p ws)))
        nil
      state)))

(defun claude-repl--ws-bracket-state (ws)
  "Return WS's underlying composed state for [N]-bracket coloring.
Unlike `claude-repl--ws-display-state', this does NOT suppress when
panels are closed: the bracket should retain the state's color even
for workspaces whose Claude panels have been dismissed, so the
claude-state remains visible at a glance."
  (claude-repl--composed-state (claude-repl--ws-claude-state ws)
                               (claude-repl--ws-repl-state ws)
                               ws))

(defun claude-repl--render-tab-entry (name current-name index)
  "Render a single tab entry for workspace NAME.
CURRENT-NAME is the active workspace name.  INDEX is the 1-based
tab position.  The display state (from `claude-repl--ws-display-state')
drives the name face.  The appearance spec is resolved via
`claude-repl--tab-spec' when display-state is non-nil; when display-state
is nil but `claude-repl--ws-bracket-state' returns a state (i.e., panels
dismissed for a workspace that still has claude-state), the spec is
built via `claude-repl--tab-spec-bracket-only' so only the [N] bracket
keeps the state's color.  The bracket label is driven by bracket-state
when display-state is suppressed, so palette `:label' glyphs (❓ for
:permission, ❌ for :dead, ⚠ for :stop-failed) render even on
workspaces whose Claude panels are closed — only the full-tab
background requires panels to be open.  When the workspace's
`:flashing' flag is set \(see `claude-repl-flash-tab'\), the spec and
name face are overridden to a uniform pulse so the tab stands out."
  (let* ((selected      (equal current-name name))
         (flashing      (claude-repl--ws-flashing-p name))
         (display-state (claude-repl--ws-display-state name))
         (bracket-state (and (null display-state)
                             (claude-repl--ws-bracket-state name)))
         (spec          (cond
                         (flashing      (claude-repl--flash-spec))
                         (bracket-state (claude-repl--tab-spec-bracket-only
                                         bracket-state selected))
                         (t             (claude-repl--tab-spec
                                         display-state selected))))
         (label         (claude-repl--tab-label
                         (or display-state bracket-state) index))
         (face          (if flashing
                            'claude-repl-tab-flash
                          (claude-repl--tab-face display-state selected)))
         (img-str       (claude-repl--tab-priority-image-str name)))
    (claude-repl--render-tab name spec label face img-str)))

(defun claude-repl--filter-hidden-names (names current-name)
  "Drop NAMES whose `:repl-state' is `:hidden' when hide-mode is on.
CURRENT-NAME is always retained so the active workspace stays visible.
When `claude-repl-hide-mode-enabled' is nil, returns NAMES unchanged.

Used by workspace cycling (`claude-repl--workspace-cycle' in commands.el)
to skip soon-to-be-killed `:hidden' workspaces.  The tab-bar itself is
NOT filtered — it reflects the raw persp-names-cache, and `:hidden'
workspaces disappear naturally once the next workspace switch triggers
`claude-repl--sweep-hidden-workspaces'."
  (if claude-repl-hide-mode-enabled
      (cl-remove-if
       (lambda (n)
         (and (not (equal n current-name))
              (eq (claude-repl--ws-repl-state n) :hidden)))
       names)
    names))

(cl-defun claude-repl--tabline-rendered-entries (&optional (names nil names-supplied-p))
  "Return the list of rendered tab-entry strings for NAMES.

Each element is the propertized output of `claude-repl--render-tab-entry'
for the corresponding workspace, 1-indexed.  Used by both
`claude-repl--tabline-advice' (which mapconcats with a space separator)
and `claude-repl-workspace-tabline-formatted' (which packs entries
into lines so wrapping happens between entries, not mid-name)."
  (let* ((names (if names-supplied-p names (+workspace-list-names)))
         (current-name (+workspace-current-name)))
    (cl-loop for name in names
             for i from 1
             collect (claude-repl--render-tab-entry name current-name i))))

(defun claude-repl--pack-tabline-entries (entries width)
  "Greedily pack ENTRIES into lines no wider than WIDTH columns.

ENTRIES is a list of rendered tab-entry strings (see
`claude-repl--tabline-rendered-entries').  Returns a list of line
strings, where adjacent entries on the same line are joined with a
single space.  A single entry wider than WIDTH still occupies its own
line — packing never breaks an entry mid-string.

The result is suitable for `claude-repl--join-tabline-rows' so the
tab-bar wraps only at entry boundaries.  WIDTH is treated as a
character count (`length' of propertized strings), which is approximate
for entries containing display-property images but close enough that
wrap points remain at entry boundaries."
  (if (null entries)
      (list "")
    (let ((lines '())
          (current nil)
          (current-w 0))
      (dolist (e entries)
        (let ((ew (length e)))
          (cond
           ((null current)
            (setq current e current-w ew))
           ((> (+ current-w 1 ew) width)
            (push current lines)
            (setq current e current-w ew))
           (t
            (setq current (concat current " " e)
                  current-w (+ current-w 1 ew))))))
      (when current (push current lines))
      (nreverse lines))))

(defun claude-repl--join-tabline-rows (lines)
  "Join LINES (pre-centered tab-bar rows) with row separators.

Each row is terminated with a single unfaced space; adjacent rows are
separated by that space followed by a newline, and the final row also
gets the trailing space (no newline after it).  This is what stops the
tab-bar's per-row redisplay (`display_tab_bar_line' in src/xdisp.c)
from painting the previous row's last glyph face across the row's
remainder.  `extend_face_to_end_of_line' uses the last glyph's face
regardless of the face's `:extend' attribute, and since each rendered
tab-entry ends with a faced name-padding space (see
`claude-repl--render-tab'), the selected tab's background would
otherwise visibly stretch to the frame's right edge whenever the
selected tab landed at the end of any wrapped row, including the
final one.

Callers must size each row so the trailing unfaced space lands within
the frame's visible columns (col < `frame-width').  `+doom-dashboard
--center' only left-pads, so a row of length `frame-width' has its
appended space at column `frame-width' — offscreen — and the last
visible glyph is still the faced one.  Pack and center to
`(1- (frame-width))' to leave room for the terminator."
  (if (null lines)
      ""
    (concat (mapconcat #'identity lines " \n") " ")))

(cl-defun claude-repl--tabline-advice (&optional (names nil names-supplied-p))
  "Override for `+workspace--tabline' to color tabs by Claude status.

The tab-bar reflects every workspace in NAMES (the raw persp list);
no hide-mode filtering is applied here.  Hide-mode operates at the
persp level — `:hidden' workspaces get persp-killed by
`claude-repl--sweep-hidden-workspaces' on the next workspace switch
and disappear from `persp-names-cache' (and therefore the tab-bar)
naturally."
  (let* ((entries (claude-repl--tabline-rendered-entries
                   (if names-supplied-p names (+workspace-list-names))))
         (current-name (+workspace-current-name))
         (states (mapcar (lambda (n)
                           (cons n (claude-repl--ws-display-state n)))
                         (if names-supplied-p names (+workspace-list-names)))))
    (claude-repl--log-verbose nil "tabline-advice: current=%s hide=%s states=%S"
                              current-name claude-repl-hide-mode-enabled states)
    (concat
     (mapconcat #'identity entries " ")
     ;; Trailing space toggle — DO NOT REMOVE.  See the block comment
     ;; above `claude-repl--tabline-space-toggle' for why this exists.
     (if claude-repl--tabline-space-toggle " " ""))))

(advice-add '+workspace--tabline :override #'claude-repl--tabline-advice)

;; --- Visible tab-bar installation -----------------------------------------
;;
;; The functions below are what `tab-bar-format' actually invokes to produce
;; the visible tab-bar.  They live here (next to `claude-repl--tabline-*'
;; entries that they call) rather than in the user-config layer so the
;; package ships with its own working tab-bar, and so the package's
;; workspace-merge reload picks up changes to them.  See the block comment
;; above `claude-repl--tabline-space-toggle' for the alternating-space hack
;; rationale.

(defun claude-repl-workspace-tabline-formatted ()
  "Format workspace list for tab-bar display.
Packs entries into lines no wider than `(1- (frame-width))' so the
tab-bar wraps only between entries (never mid-name) AND so the
unfaced terminator that `claude-repl--join-tabline-rows' appends to
each row lands within the visible columns (col < `frame-width').
Centers each packed line to the same reserved width so left-only
padding from `+doom-dashboard--center' doesn't push the row back to
the right edge.  Joins via `claude-repl--join-tabline-rows' so the
per-row face extension does not paint the rightmost tab's background
to the right edge.  Appends a toggled trailing space tied to
`claude-repl--tabline-space-toggle' so the left-aligned tab-bar
segment's string content actually changes across refresh ticks (the
alternating-space trick — see the block comment above the toggle's
defvar).  Without the toggle, face-only status transitions
\(e.g. :thinking -> :done) stay invisible until a workspace switch."
  (let* ((width (frame-width))
         (line-width (max 1 (1- width)))
         (entries (claude-repl--tabline-rendered-entries))
         (lines (claude-repl--pack-tabline-entries entries line-width))
         (centered-lines (mapcar (lambda (line)
                                   (if (fboundp '+doom-dashboard--center)
                                       (+doom-dashboard--center line-width line)
                                     line))
                                 lines))
         (joined (claude-repl--join-tabline-rows centered-lines)))
    (concat joined
            (if claude-repl--tabline-space-toggle " " ""))))

(defun claude-repl-current-workspace-name-segment ()
  "Return current workspace name as an invisible tab-bar segment.
Same alternating-space trick as
`claude-repl-workspace-tabline-formatted': the trailing space toggles
each second via `claude-repl--tabline-space-toggle' to force the
right-aligned segment to repaint too.

The segment's actual text is invisible (`'invisible t' text property)
so its only purpose is the cache-busting role."
  (let ((name (if (fboundp 'safe-persp-name)
                  (safe-persp-name (and (fboundp 'get-current-persp)
                                        (get-current-persp)))
                "")))
    (propertize (if claude-repl--tabline-space-toggle
                    (concat name " ")
                  name)
                'invisible t)))

;; Install the tab-bar after persp-mode loads so `safe-persp-name' /
;; `get-current-persp' resolve cleanly at render time; persp-mode is the
;; dep that the workspace-list entries read in
;; `claude-repl--tabline-rendered-entries' and below.
(after! persp-mode
  (setq tab-bar-format '(claude-repl-workspace-tabline-formatted
                         tab-bar-format-align-right
                         claude-repl-current-workspace-name-segment)
        tab-bar-show t
        tab-bar-new-button-show nil
        tab-bar-close-button-show nil)
  (tab-bar-mode 1))

(defun claude-repl-toggle-hide-mode ()
  "Toggle `claude-repl-hide-mode-enabled'.
When toggled ON, `:hidden' workspaces (those closed via `SPC o C')
are persp-killed on the next workspace switch.  When OFF, they remain
in the workspace list and behave like ordinary `:inactive' workspaces.
Forces a tab-bar repaint so cycling-skip semantics update immediately."
  (interactive)
  (setq claude-repl-hide-mode-enabled (not claude-repl-hide-mode-enabled))
  (claude-repl--force-tab-bar-redraw)
  (message "claude-repl hide-mode %s"
           (if claude-repl-hide-mode-enabled "enabled" "disabled")))

;; Suppress the echo area flash when switching workspaces.
;; Doom calls (+workspace/display) after switch/cycle/new/load, which uses
;; (message ...) to show the tabline in the echo area.  Since tabs are
;; already visible at the top, the bottom flash is redundant.
(advice-add '+workspace/display :override #'ignore)

(defun claude-repl--workspace-message-body-advice (message &optional type)
  "Override for `+workspace--message-body' that strips the tabline prefix.

Doom's stock `+workspace--message-body' builds the echo-area string as
`<tabline> | <message>', so every `+workspace-message' / `+workspace-error'
call (e.g. the `Deleted '<ws>' workspace' notification emitted by
`+workspace/kill' inside `claude-repl--nuke-one-workspace's merge-teardown
path) briefly flashes the full workspaces tabline in the minibuffer.

Mirrors the rationale for the `+workspace/display' override above: the
tab-bar is already painted at the top of the frame, so duplicating its
contents in the echo area is redundant and visually disruptive — most
noticeable right after a workspace merge, where the source workspace's
teardown drops a tabline flash on top of an otherwise quiet UI.

Returns only the propertized MESSAGE text, faced per TYPE
\(`error' / `warn' / `success' / `info'), preserving the textual
notification while dropping the leading workspace list."
  (propertize (format "%s" message)
              'face (pcase type
                      ('error 'error)
                      ('warn 'warning)
                      ('success 'success)
                      ('info 'font-lock-comment-face))))

(advice-add '+workspace--message-body :override
            #'claude-repl--workspace-message-body-advice)

;;; Claude panel visibility ---------------------------------------------------

;; Walk saved window-configuration tree to find claude buffers.
(defun claude-repl--wconf-has-claude-p (wconf)
  "Return non-nil if WCONF (a `window-state-get' tree) contains a claude vterm buffer.
Excludes input buffers: presence of only the input panel in a saved
config (e.g. from a placeholder layout) should not count as claude open."
  (when (and wconf (proper-list-p wconf))
    (let ((buf-entry (alist-get 'buffer wconf)))
      (if (and buf-entry (stringp (car-safe buf-entry))
               (string-match-p claude-repl--vterm-buffer-re (car buf-entry))
               (not (string-match-p claude-repl--input-buffer-re (car buf-entry))))
          t
        (cl-some #'claude-repl--wconf-has-claude-p
                 (cl-remove-if-not #'proper-list-p wconf))))))

(defun claude-repl--visible-claude-buffer-p (buf)
  "Return non-nil if BUF is a live, visible Claude buffer."
  (and (buffer-live-p buf)
       (claude-repl--claude-buffer-p buf)
       (get-buffer-window buf)))

(defun claude-repl--claude-visible-in-current-ws-p ()
  "Return non-nil if a claude buffer is visible in the current workspace."
  (cl-some #'claude-repl--visible-claude-buffer-p
           (buffer-list)))

(defun claude-repl--claude-in-saved-wconf-p (ws-name)
  "Return non-nil if background workspace WS-NAME has a claude buffer in its saved config."
  (let* ((persp (persp-get-by-name ws-name))
         (wconf (and persp (not (symbolp persp)) (persp-window-conf persp))))
    (claude-repl--wconf-has-claude-p wconf)))

(defun claude-repl--ws-claude-open-p (ws-name)
  "Return non-nil if workspace WS-NAME has a claude buffer in its window layout.
For the current workspace, checks live windows.
For background workspaces, inspects the saved persp window configuration."
  (if (equal ws-name (+workspace-current-name))
      (claude-repl--claude-visible-in-current-ws-p)
    (claude-repl--claude-in-saved-wconf-p ws-name)))

;;; State machine ------------------------------------------------------------

(defun claude-repl--update-ws-state (ws)
  "Decay WS's claude-state from :done to :idle when conditions are met.

This is the sole transition the timer drives on the claude-state axis.
Every other transition is sentinel-owned (see the hook handlers in
`sentinel.el').  When Claude finishes a turn the Stop hook writes
`:done'; if the worktree is clean AND the user has been focused on
the workspace for at least `claude-repl-done-idle-delay' seconds
\(tracked via `:done-acked-at'), the tab decays to `:idle'.  If the
worktree is dirty, the user has not yet focused the workspace, or
the focus dwell time has not yet elapsed, the tab stays green.

The dwell requirement prevents a quick transit through a `:done'
workspace from silently dropping the green indicator: the focus
timestamp is cleared on switch-away from a `:done' workspace, so the
countdown restarts on every return.

Decay also clears `:done-acked' and `:done-acked-at' so a future
:done cycle starts from a clean slate.

State table:
  :done + clean + acked + dwell-elapsed   → :idle   (this function)
  :done + clean + acked + !dwell          → unchanged (still counting)
  :done + clean + !acked                  → unchanged (wait for user to view)
  :done + dirty                           → unchanged (wait for stage/commit)
  anything else                           → unchanged (sentinel-owned or terminal)"
  (let* ((state (claude-repl--ws-claude-state ws))
         (acked (claude-repl--ws-get ws :done-acked))
         (acked-at (claude-repl--ws-get ws :done-acked-at))
         (git-status (claude-repl--ws-get ws :git-clean))
         (dwell (and acked-at (- (float-time) acked-at)))
         (dwell-elapsed (and dwell (> dwell claude-repl-done-idle-delay))))
    (cond
     ((null git-status)
      (claude-repl--log-verbose ws "update-ws-state ws=%s state=%s git-clean not yet populated, skipping" ws state))
     ((and (eq state :done) (eq git-status 'clean) acked dwell-elapsed)
      (claude-repl--log ws "update-ws-state ws=%s :done->:idle (clean, acked, dwell=%.2fs)" ws dwell)
      (claude-repl--ws-set-claude-state ws :idle)
      (claude-repl--ws-put ws :done-acked nil)
      (claude-repl--ws-put ws :done-acked-at nil))
     (t
      (claude-repl--log-verbose ws "update-ws-state ws=%s state=%s acked=%s dwell=%s git-status=%s no-op"
                                ws state acked dwell git-status)))))

(defvar claude-repl--update-tick-counter 0
  "Monotonic tick counter for the workspace-state update timer.
Incremented at the top of every `claude-repl--update-all-workspace-states-now'
pass.  Read by the inner per-workspace step to gate git work via
`(zerop (mod counter claude-repl-state-git-tick-modulus))'.")

(defvar claude-repl--update-in-flight nil
  "Float-time of the most recent chain start, or nil when no chain is in flight.
Set by `claude-repl--update-all-workspace-states-now' at chain
kickoff and cleared by the terminal finalize step.  Read by
`claude-repl--update-in-flight-p' so the periodic timer entrypoint
can skip its tick when a previous chain has not finished.  Carries a
timestamp rather than a plain `t' so stale flags from an errored
chain (one that escaped the per-step `condition-case' and never
finalized) can be detected and force-cleared via
`claude-repl-state-stale-threshold'.")

(defvar claude-repl--update-spread-sync nil
  "When non-nil, the chain processes all workspaces synchronously.
Test-only affordance: production code never sets this.  Tests bind it
to `t' so multi-workspace dispatch assertions can read state
immediately after the call, without having to advance time to let
`run-at-time'-scheduled steps fire.")

(defun claude-repl--update-in-flight-p ()
  "Return non-nil when an update chain is in flight and not stale.
A non-nil `claude-repl--update-in-flight' set within the last
`claude-repl-state-stale-threshold' seconds means a chain is still
running and a new tick should skip.  An older stamp is treated as a
wedged chain (the per-step `condition-case' didn't catch some error
path or the finalize never ran), force-cleared in place, and the
caller is told to proceed."
  (cond
   ((null claude-repl--update-in-flight) nil)
   ((< (- (float-time) claude-repl--update-in-flight)
       claude-repl-state-stale-threshold)
    t)
   (t
    (claude-repl--log nil "update-in-flight-p: stale flag (%.2fs old), force-clearing"
                      (- (float-time) claude-repl--update-in-flight))
    (setq claude-repl--update-in-flight nil)
    nil)))

(defun claude-repl--update-one-workspace-state (ws do-git-p)
  "Run the per-workspace state-update body for WS.
The cheap parts (`claude-repl--claude-running-p',
`claude-repl--update-ws-state', `claude-repl--mark-dead-vterm') run
every tick so transitions like `:done' -> `:idle' stay snappy.
DO-GIT-P gates the expensive git refreshes
\(`claude-repl--async-refresh-git-status' and
`claude-repl--async-refresh-branch-merged') so they fire only on the
mod-N tick selected by `claude-repl-state-git-tick-modulus'."
  (if (claude-repl--claude-running-p ws)
      (progn
        (claude-repl--update-ws-state ws)
        (when do-git-p
          (claude-repl--async-refresh-git-status ws)))
    ;; No live vterm process → clear non-thinking state
    (claude-repl--mark-dead-vterm ws))
  ;; Merged-ness is independent of claude/vterm liveness — refresh
  ;; for every workspace so the drawer's flatten-through-merged
  ;; rendering has fresh `:branch-merged' values.  Gated on DO-GIT-P
  ;; because the refresh's preconditions and process spawn are
  ;; comparable in cost to the diff refresh above.
  (when (and do-git-p
             (fboundp 'claude-repl--async-refresh-branch-merged))
    (claude-repl--async-refresh-branch-merged ws)))

(defun claude-repl--update-all-workspace-states--step (remaining do-git-p gap)
  "Process the head of REMAINING; schedule self for the rest.
DO-GIT-P is the precomputed mod-N gate for the whole pass (snapshotted
at chain kickoff so every workspace in this pass sees the same value).
GAP is the inter-step delay in seconds.

Per-step `gethash' recheck against `claude-repl--workspaces' covers
the snapshot-vs-live divergence: a workspace can be removed mid-chain
\(`--ws-del' from a merge, kill, or sweep) and we must not act on
ghost names.  The body itself is wrapped in `condition-case' so an
error in one ws step never wedges the in-flight flag for subsequent
ticks — the chain logs and keeps going.

When `claude-repl--update-spread-sync' is non-nil (tests only),
recurses directly instead of via `run-at-time'."
  (cond
   ((null remaining)
    (claude-repl--update-all-workspace-states--finalize))
   (t
    (let ((ws (car remaining))
          (rest (cdr remaining)))
      (condition-case err
          (when (gethash ws claude-repl--workspaces)
            (claude-repl--update-one-workspace-state ws do-git-p))
        (error
         (claude-repl--log ws "update-all-workspace-states--step: error ws=%s err=%S"
                           ws err)))
      (if rest
          (if claude-repl--update-spread-sync
              (claude-repl--update-all-workspace-states--step rest do-git-p gap)
            (run-at-time gap nil
                         #'claude-repl--update-all-workspace-states--step
                         rest do-git-p gap))
        (claude-repl--update-all-workspace-states--finalize))))))

(defun claude-repl--update-all-workspace-states--finalize ()
  "Terminal step of the workspace-state update chain.
Refreshes the drawer (only one renderer call per pass — internal
signature short-circuit covers the no-change case) and clears the
in-flight flag.  Wrapped in `unwind-protect' so the flag clears even
if the drawer refresh errors, preventing permanent timer wedging."
  (unwind-protect
      (when (fboundp 'claude-repl-drawer--refresh-if-visible)
        (claude-repl-drawer--refresh-if-visible))
    (setq claude-repl--update-in-flight nil)))

(defun claude-repl--update-all-workspace-states-now ()
  "Unguarded entrypoint for the workspace-state update chain.
Snapshots `(hash-table-keys claude-repl--workspaces)' so the chain
iterates a stable list even as the hash mutates mid-pass; per-step
`gethash' recheck (inside `--step') filters out workspaces deleted
during the spread window.

Increments `claude-repl--update-tick-counter' and computes DO-GIT-P
once so every ws in this pass agrees on the mod-N decision.  Sets the
in-flight marker; the terminal finalize step clears it.

Polls the sentinel directory as a file-notify fallback (`--poll-
workspace-notifications').  Does NOT flip the tabline space toggle —
that's the periodic timer's job (`claude-repl--update-all-workspace-
states', the guarded entrypoint), since event-driven callers
\(frame-focus, workspace-switch, show-panels) already trigger a
redisplay through other paths.

For event-driven callers that want to kick a refresh independent of
the 1Hz reentry guard.  Concurrent chains from rapid sync calls are
permitted (rare in practice); each tracks its own snapshot and
finalize, and the last to finalize clears the flag harmlessly."
  (claude-repl--poll-workspace-notifications)
  (setq claude-repl--update-tick-counter (1+ claude-repl--update-tick-counter))
  ;; Filter to live workspaces only — tombstoned entries have no
  ;; vterm/process to probe and would burn git status calls for no UI.
  (let* ((ws-names (claude-repl--live-ws-names))
         (n (length ws-names))
         (do-git-p (zerop (mod claude-repl--update-tick-counter
                               claude-repl-state-git-tick-modulus)))
         (gap (if (and (> n 0) (> claude-repl-state-spread-window 0))
                  (max claude-repl-state-spread-min-gap
                       (/ claude-repl-state-spread-window (float n)))
                claude-repl-state-spread-min-gap)))
    (claude-repl--log-verbose nil "update-all-workspace-states-now: count=%d do-git=%s gap=%.3fs counter=%d"
                              n do-git-p gap claude-repl--update-tick-counter)
    (setq claude-repl--update-in-flight (float-time))
    (claude-repl--update-all-workspace-states--step ws-names do-git-p gap)))

(defun claude-repl--update-all-workspace-states ()
  "Periodic 1Hz timer entrypoint for workspace-state updates.
Always drives `claude-repl--force-tab-bar-redraw' to force a tab-bar
repaint (DO NOT REMOVE — see the block comment above
`claude-repl--tabline-space-toggle').  The redraw happens BEFORE the
in-flight check so the tab-bar keeps animating even when the update
chain is stacking and we skip a tick.

Flipping `claude-repl--tabline-space-toggle' alone is not enough:
the active tab-bar format function
\(`claude-repl-workspace-tabline-formatted') calls
`claude-repl--tabline-rendered-entries' directly and bypasses
`+workspace--tabline', so the `claude-repl--tabline-advice' path is
no longer on the displayed-rendering hot path.  `--force-tab-bar-
redraw' flips the toggle AND drives `tab-bar-tabs-set' /
`tab-bar--update-tab-bar-lines' / `force-mode-line-update' so the
alternating-string cache-bust actually reaches the display.

When a previous chain is still in flight (per
`claude-repl--update-in-flight-p'), skips this tick — the in-flight
chain will catch up.  Stale flags older than
`claude-repl-state-stale-threshold' are force-cleared so a wedged
chain can't permanently disable the timer.

Otherwise delegates to `claude-repl--update-all-workspace-states-now',
which owns the actual per-workspace iteration with the mod-N git
gate and the recursive serial spread.

Event-driven callers (frame-focus, workspace-switch, show-panels)
should call `-now' directly instead of this guarded entrypoint — they
want to kick a fresh refresh and don't compete with the timer for
the in-flight slot."
  ;; Drive the tab-bar redraw on every tick so face-only status
  ;; transitions (:thinking -> :done, etc.) actually reach the display.
  ;; DO NOT REMOVE — see the block comment above
  ;; `claude-repl--tabline-space-toggle'.  Happens before the in-flight
  ;; check so the animation survives long chains.
  (claude-repl--force-tab-bar-redraw)
  (unless (claude-repl--update-in-flight-p)
    (claude-repl--update-all-workspace-states-now)))

;; Periodically update all workspace states (catches git changes, etc.)
(push (run-with-timer claude-repl-state-poll-interval claude-repl-state-poll-interval #'claude-repl--update-all-workspace-states)
      claude-repl--timers)

(defun claude-repl--mark-dead-vterm (ws)
  "Record that WS's vterm process is no longer running.
Sets `:repl-state :dead' and clears `:claude-state'.  This is a
documented lifecycle-cleanup exception to the sentinel-only writer
rule: no hook will ever fire again for a dead process, so Emacs is
the only observer that can reset state.

No-op in four cases:
- `:repl-state' is already `:dead' (idempotent on the poll path).
- `:repl-state' is `:merged' — the workspace was nuked after a
  successful merge and `:merged' takes precedence over `:dead'.
  Without this guard, the next poll would clobber the merge badge.
- `:repl-state' is `:merge-failed' — the workspace was nuked after
  a silent-failure merge and `:merge-failed' is the canonical badge
  for that state (also ❌, but routed under MERGED, not orphaned as
  :dead).  Without this guard, the next poll would re-classify the
  workspace as plain `:dead' and the MERGED-section semantics would
  be lost.
- `:claude-state' is `:init' — Claude is starting, the vterm process
  may not have reached running state yet, and observing no process
  does not mean dead.  The session-start hook will transition away
  from `:init' shortly; until then the timer leaves things alone."
  (unless (or (eq (claude-repl--ws-repl-state ws) :dead)
              (eq (claude-repl--ws-repl-state ws) :merged)
              (eq (claude-repl--ws-repl-state ws) :merge-failed)
              (eq (claude-repl--ws-claude-state ws) :init))
    (claude-repl--log ws "mark-dead-vterm: ws=%s claude-state=%s -> :dead"
                      ws (claude-repl--ws-claude-state ws))
    (claude-repl--ws-put ws :repl-state :dead)
    (claude-repl--ws-put ws :claude-state nil)
    (force-mode-line-update t)))

;;; Frame focus handler -------------------------------------------------------

(defun claude-repl--on-frame-focus ()
  "Refresh claude vterm and update all workspace states when Emacs regains focus.
Calls `claude-repl--update-all-workspace-states-now' (the unguarded
entrypoint) rather than the periodic-timer entrypoint: frame focus is
an event-driven signal that the user is back and wants fresh data, so
it should kick a refresh regardless of the in-flight reentry guard."
  (if (frame-focus-state)
      (progn
        (claude-repl--log (+workspace-current-name) "on-frame-focus: focused")
        (claude-repl--refresh-vterm)
        (claude-repl--update-all-workspace-states-now))
    (claude-repl--log-verbose (+workspace-current-name) "on-frame-focus: not focused")))

(add-function :after after-focus-change-function #'claude-repl--on-frame-focus)


