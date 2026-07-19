;;; status.el --- workspace status state machine and tab bar rendering -*- lexical-binding: t; -*-

;;; Code:

;;; Priority badge images
;;
;; Each image is a small PNG loaded from the module's images/ directory and
;; scaled to fit the tab-bar line height.

(defcustom agent-repl-priority-levels '("p05" "p1" "p2" "p3")
  "List of recognized priority level strings for workspace badges."
  :type '(repeat string)
  :group 'agent-repl)

(defcustom agent-repl-repo-default-priorities '(("explanation-engine" . "p1"))
  "Alist mapping repository names to default `:priority' values for new workspaces.
The repository name is the basename of the parent of `git rev-parse
--git-common-dir' for a path, matching how agent-repl groups workspaces
by repo (the fold set, tab-bar grouping).  Used by
workspace-creation paths as a final fallback when no
explicit priority was supplied and none was inherited from a source
workspace.  An entry whose value is nil disables the default for that
repo."
  :type '(alist :key-type string
                :value-type (choice (string :tag "Priority") (const :tag "None" nil)))
  :group 'agent-repl)

(defun agent-repl--repo-name-for-path (path)
  "Return the repository name for PATH, or nil.
Resolved as the basename of the parent of `git rev-parse --git-common-dir',
the key agent-repl uses to group workspaces by repo.  Returns nil
when PATH is nil, does not exist, is not inside a git repository, or
git fails."
  (when (and path
             (stringp path)
             (file-directory-p (expand-file-name path)))
    (let* ((dir (expand-file-name path))
           (raw (let ((default-directory dir))
                  (agent-repl--git-string-quiet "rev-parse" "--git-common-dir"))))
      (when (and raw
                 (not (string-empty-p raw))
                 (not (string-prefix-p "fatal" raw)))
        (let* ((abs (if (file-name-absolute-p raw) raw
                      (expand-file-name raw dir)))
               (canon (agent-repl--path-canonical abs))
               (parent (file-name-directory canon)))
          (when parent
            (file-name-nondirectory (directory-file-name parent))))))))

(defun agent-repl--repo-default-priority-for-path (path)
  "Return the default `:priority' string for a workspace rooted at PATH.
Looks up the repo name (see `agent-repl--repo-name-for-path') in
`agent-repl-repo-default-priorities'.  Returns nil when PATH has no
recognized repo or the repo has no configured default."
  (when-let ((name (agent-repl--repo-name-for-path path)))
    (cdr (assoc name agent-repl-repo-default-priorities))))

(defcustom agent-repl-tab-bracket-format "[%s]"
  "Format string for tab bracket labels.
%s is replaced with the tab index number or emoji."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-tab-name-padding " %s "
  "Format string for tab workspace name padding."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-state-poll-interval 1
  "Seconds between workspace state update polls."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-state-git-tick-modulus 5
  "Per-workspace git refreshes fire once every N timer ticks.
The 1Hz `agent-repl--update-all-workspace-states' timer drives both
the cheap state-machine work (agent-running-p, update-ws-state,
mark-dead) and the expensive git work
\(`agent-repl--async-refresh-git-status' and
`agent-repl--async-refresh-branch-merged').  Cheap work runs every
tick so transitions like `:done' -> `:idle' stay snappy.  Git work
runs only when `(mod tick-counter N) == 0' so the per-ws fork load is
amortized to one-in-N ticks; the on-disk reality git observes does
not change at 1Hz, so polling that fast is wasteful.

Lower values mean fresher cached git state at higher CPU cost;
higher values do the inverse.  The default of 5 yields one git
refresh per workspace per ~5 seconds, paired with the spread (see
`agent-repl-state-spread-window') so even those refreshes are not
bursty."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-state-spread-window 1.0
  "Seconds over which per-workspace state updates are spread per tick.
Each tick, `agent-repl--update-all-workspace-states' snapshots the
workspace list and processes one workspace at a time via
`run-at-time' with gap `(max agent-repl-state-spread-min-gap (/ this
N))', where N is the workspace count.  This flattens the per-tick
burst (N forks landing simultaneously when the git modulus hits) into
a smooth trickle paced across the window.

Setting this to 0 collapses the spread to synchronous serial
iteration, which is what tests want."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-state-spread-min-gap 0.05
  "Floor on the per-step gap inside the workspace-state update chain.
Computed as `(max this (/ agent-repl-state-spread-window N))' so
high workspace counts can't spawn very-fast `run-at-time' timers."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-state-stale-threshold 5.0
  "Seconds after which an in-flight update chain is considered wedged.
`agent-repl--update-all-workspace-states' (the periodic timer
entrypoint) skips its tick when the previous chain has not finished.
If the in-flight marker is older than this threshold, the chain is
treated as stuck (likely due to an error in a per-step body that
escaped the `condition-case' net) and the flag is force-cleared so a
new chain can start.  Belt-and-braces against permanent wedging."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-done-idle-delay 1
  "Seconds the user must focus a :done workspace before it decays to :idle.
The countdown starts when the workspace becomes the active workspace
\(or when :done arrives while it is already active).  Switching away
from a :done workspace before the delay elapses clears the timestamp,
so a quick transit through the tab does not silently strip the green
\"ready for review\" indicator — the user must return and dwell again."
  :type 'number
  :group 'agent-repl)

(defvar agent-repl--priority-images nil
  "Alist mapping priority strings (\"p05\" \"p1\" \"p2\" \"p3\") to Emacs image specs.")

;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; !! DO NOT REMOVE `agent-repl--tabline-space-toggle' OR ITS USAGE   !!
;; !! IN `agent-repl--tabline-advice',                                !!
;; !! `agent-repl--force-tab-bar-redraw', AND                         !!
;; !! `agent-repl--update-all-workspace-states'.                      !!
;; !!                                                                  !!
;; !! The tab-bar will NOT repaint unless the string it displays       !!
;; !! actually changes between ticks.  Toggling the cache-buster       !!
;; !! suffix (`agent-repl--tabline-cache-buster') on every poll       !!
;; !! cycle forces the tab-bar to detect a "new" string and            !!
;; !! re-render, giving us real-time visual updates.  Without this,    !!
;; !! state-color changes (thinking → done, etc.) are invisible        !!
;; !! until the user manually triggers a redisplay.                    !!
;; !!                                                                  !!
;; !! The suffix MUST be zero-width and non-visible: it used to be a   !!
;; !! plain trailing space, and that one-column width tick could push  !!
;; !! the tabline across a row-wrap threshold, changing the tab-bar    !!
;; !! height and (on macOS) resizing the NSWindow every second — the   !!
;; !! trigger edge of the redisplay livelock described in              !!
;; !! `agent-repl-workspace-tabline-formatted'.  The cache only       !!
;; !! compares string CONTENTS (`equal' ignores text properties), so   !!
;; !! an `invisible'-propertized space busts it without any visible    !!
;; !! or width effect.                                                 !!
;; !!                                                                  !!
;; !! The toggle is read on TWO rendering paths:                       !!
;; !!  - `agent-repl--tabline-advice' (override of `+workspace--      !!
;; !!    tabline'), used by callers that still go through Doom's       !!
;; !!    workspace tabline API (e.g. echo-area helpers, tests).        !!
;; !!  - `agent-repl-workspace-tabline-formatted' /                   !!
;; !!    `agent-repl-current-workspace-name-segment', installed in    !!
;; !!    `tab-bar-format' below and therefore driving the visible      !!
;; !!    tab-bar.                                                      !!
;; !!                                                                  !!
;; !! Just flipping the toggle is NOT enough — Emacs's tab-bar caches  !!
;; !! the format result and will keep painting the cached value until  !!
;; !! something forces a re-read.  `agent-repl--force-tab-bar-redraw' !!
;; !! flips the toggle AND drives `tab-bar-tabs-set' /                 !!
;; !! `tab-bar--update-tab-bar-lines' / `force-mode-line-update' so    !!
;; !! the alternating string actually reaches the display.  The 1Hz   !!
;; !! `agent-repl--update-all-workspace-states' timer calls            !!
;; !! `--force-tab-bar-redraw' every tick.                              !!
;; !!                                                                  !!
;; !! This has been accidentally removed multiple times.  DO NOT       !!
;; !! remove it again.  It is NOT dead code.  It is NOT cosmetic.     !!
;; !! It is the mechanism that makes tab-bar updates work.             !!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(defvar agent-repl--tabline-space-toggle nil
  "Non-nil means append the zero-width cache-buster to the tabline string.
Flipped on every poll cycle by `agent-repl--update-all-workspace-states'
\(via `agent-repl--force-tab-bar-redraw').  Read (through
`agent-repl--tabline-cache-buster') by `agent-repl--tabline-advice'
AND by `agent-repl-workspace-tabline-formatted' /
`agent-repl-current-workspace-name-segment' (the functions installed
into `tab-bar-format') so both rendering paths produce an alternating
string that forces the tab-bar to repaint.  DO NOT REMOVE — see
comment above.")

(defun agent-repl--tabline-cache-buster ()
  "Return the toggled zero-width suffix that defeats the tab-bar string cache.
Returns an `invisible'-propertized single space when
`agent-repl--tabline-space-toggle' is non-nil, else the empty string.
The tab-bar's repaint gate compares string contents (`equal' ignores
text properties), so the suffix must alternate the string's characters
between ticks — but it must never change the rendered width, because a
width tick can move the tabline across a row-wrap threshold and set
off the tab-bar-height/frame-resize oscillation that livelocks
redisplay (see `agent-repl-workspace-tabline-formatted').  DO NOT
replace this with a bare \" \" — see the block comment above
`agent-repl--tabline-space-toggle'."
  (if agent-repl--tabline-space-toggle
      (propertize " " 'invisible t)
    ""))

(defvar agent-repl-hide-mode-enabled nil
  "Non-nil means persp-kill `:hidden' workspaces on workspace switch.
A workspace becomes `:hidden' when the user invokes `SPC o C' (the
deprio close path, `agent-repl--on-close').  The kill happens in
`agent-repl--sweep-hidden-workspaces' from the workspace-switch
handler via `agent-repl--nuke-one-workspace', which always preserves
the on-disk state file so the workspace can be re-opened later via
project switch.

The tab-bar itself is NOT filtered — it reflects the raw persp list.
Workspace cycling (`agent-repl-switch-left/right') skips `:hidden'
workspaces while hide-mode is on so the user does not land on a
soon-to-be-killed workspace mid-cycle.

The current workspace is exempt from sweep, and arriving on a `:hidden'
workspace resets its state to `:inactive' (so it survives the next
sweep).  To persistently keep a hidden workspace, toggle hide-mode off.

Toggle via `agent-repl-toggle-hide-mode'.")

(defun agent-repl--load-priority-images ()
  "Load priority badge PNGs from the module images/ directory.
Populates `agent-repl--priority-images' with display-ready image specs."
  (let* ((dir (file-name-directory (or load-file-name buffer-file-name)))
         (img-dir (expand-file-name "images/" dir))
         (names agent-repl-priority-levels)
         (height (frame-char-height)))
    (setq agent-repl--priority-images
          (cl-loop for name in names
                   for file = (expand-file-name (concat name ".png") img-dir)
                   when (file-exists-p file)
                   collect (cons name (create-image file 'png nil
                                                    :height height
                                                    :ascent 'center))))
    (agent-repl--log nil "load-priority-images: loaded=%d" (length agent-repl--priority-images))))

(when (image-type-available-p 'png)
  (agent-repl--load-priority-images))

(defun agent-repl--priority-image (priority)
  "Return the Emacs image spec for PRIORITY string, or nil."
  (cdr (assoc priority agent-repl--priority-images)))

(defun agent-repl--priority-rank (priority)
  "Return the sort rank for PRIORITY string; lower means higher precedence.
Ranks come from the position of PRIORITY in `agent-repl-priority-levels',
so adding levels there propagates without code changes here.  Returns
`most-positive-fixnum' for nil or unrecognized values so they sort after
every recognized priority."
  (or (and priority (cl-position priority agent-repl-priority-levels :test #'equal))
      most-positive-fixnum))

;; `agent-repl--reorder-workspace-by-priority' and
;; `agent-repl--reorder-workspace-to-front' both live in `workspace.el'
;; (the persp-mode boundary for `persp-names-cache' reordering); status.el
;; does not own persp-cache mutation.

;;; Workspace state accessors ------------------------------------------------

;; --- Two-axis state model (analysis #8) ---
;;
;; Workspace state is split into two orthogonal plist keys:
;;   :agent-state — Agent-owned lifecycle.  Values: nil | :init |
;;                   :idle | :thinking | :done | :permission.
;;                   Written primarily by hook sentinels; narrow
;;                   Emacs-side exceptions at lifecycle boundaries
;;                   (the boot path writes :init; kill clears).
;;   :repl-state   — Emacs-owned session-lifecycle flag.  Values:
;;                     nil       — workspace registered, no agent
;;                                 session has ever been attached.
;;                     :active   — panels open, session running.
;;                     :inactive — panels closed, session preserved.
;;                     :dead     — agent session has died.
;;                   Only :dead contributes to tab display (❌ badge);
;;                   other values are bookkeeping only.

(defun agent-repl--ws-state (ws)
  "Return the current :agent-state keyword for workspace WS, or nil.
Compat shim: equivalent to `agent-repl--ws-agent-state', retained for
test callers that have not yet migrated."
  (agent-repl--ws-get ws :agent-state))

(defun agent-repl--ws-agent-state (ws)
  "Return the current :agent-state keyword for workspace WS, or nil."
  (agent-repl--ws-get ws :agent-state))

(defun agent-repl--ws-repl-state (ws)
  "Return the current :repl-state keyword for workspace WS, or nil."
  (agent-repl--ws-get ws :repl-state))

(defun agent-repl--ws-set-agent-state (ws state)
  "Set workspace WS's :agent-state to STATE.
STATE is one of: nil, :init, :idle, :thinking, :done, :permission."
  (unless ws (error "agent-repl--ws-set-agent-state: ws is nil"))
  (agent-repl--log ws "agent-state %s -> %s" ws state)
  (agent-repl--ws-put ws :agent-state state)
  (force-mode-line-update t)
  (agent-repl--memory-state-save ws))

(defun agent-repl--ws-set-repl-state (ws state)
  "Set workspace WS's :repl-state to STATE.
STATE is one of:
  nil        — freshly killed / no session
  :active    — panels displayed, session alive
  :inactive  — panels hidden, session alive (plain `SPC o c' close)
  :hidden    — semantically `:inactive', but additionally marks the
               workspace for persp-kill on the next workspace change
               when `agent-repl-hide-mode-enabled' is non-nil.  Set
               by the `SPC o C' deprio-close path; the kill happens in
               `agent-repl--sweep-hidden-workspaces' from the
               workspace-switch handler.  The on-disk state file is
               always preserved by `--nuke-one-workspace' so the
               workspace can be re-opened later via project-switch.
  :merged    — workspace's branch has been merged into its source.
               Set by `agent-repl--workspace-merge-do' on success
               (alongside `:merge-completed t').  Takes precedence
               over `:dead' so the 🔀 badge survives the post-merge
               nuke-and-poll cycle that would otherwise mark the
               now-sessionless workspace dead.
  :dead      — agent session gone

The orthogonal `:done-acked' boolean tracks whether the user has seen
the current `:agent-state :done' result.  It used to be the
`:repl-state :viewed' value but was lifted out — viewing isn't a
lifecycle phase, it's an acknowledgment flag that overlays :done.

Persists the new value to disk via `agent-repl--state-save' when STATE
is `:active', `:inactive', or `:hidden' so panel-visibility (and the
deprio-hide marker) survives Emacs restart.  `:dead' / nil are not
persisted — they reduce to \"no opinion\" at restart, so default
open-panels behavior applies.  `:dead' is set via `--ws-put' directly
(in `--mark-dead'), bypassing this setter, so no special-case is
needed there."
  (unless ws (error "agent-repl--ws-set-repl-state: ws is nil"))
  (agent-repl--log ws "repl-state %s -> %s" ws state)
  (agent-repl--ws-put ws :repl-state state)
  (force-mode-line-update t)
  (when (memq state '(:active :inactive :hidden))
    (agent-repl--state-save ws))
  (agent-repl--memory-state-save ws))

(defun agent-repl--ws-agent-state-clear-if (ws state)
  "Clear WS's :agent-state when it currently equals STATE.
Compare-and-clear: no-op if the current value is not STATE."
  (unless ws (error "agent-repl--ws-agent-state-clear-if: ws is nil"))
  (if (eq (agent-repl--ws-get ws :agent-state) state)
      (progn
        (agent-repl--log ws "agent-state-clear-if %s %s -> nil" ws state)
        (agent-repl--ws-put ws :agent-state nil)
        (force-mode-line-update t))
    (agent-repl--log-verbose ws
                              "agent-state-clear-if ws=%s state=%s no-op (current=%s)"
                              ws state (agent-repl--ws-get ws :agent-state))))

;; --- Stop / SubagentStop coordination ---
;;
;; The Stop hook fires when the agent finishes its main response.  When
;; the agent has spawned background subagents (Task tool with
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
;; arriving last) triggers `agent-repl--handle-agent-finished'.
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

(defun agent-repl--ws-stop-received-p (ws)
  "Return non-nil if the Stop hook has fired for workspace WS without
having yet been resolved into a `:done' transition."
  (agent-repl--ws-get ws :stop-received))

(defun agent-repl--ws-set-stop-received (ws val)
  "Set workspace WS's :stop-received flag to VAL (a boolean)."
  (unless ws (error "agent-repl--ws-set-stop-received: ws is nil"))
  (agent-repl--log ws "stop-received %s -> %s" ws val)
  (agent-repl--ws-put ws :stop-received val))

(defun agent-repl--ws-pending-subagents (ws)
  "Return WS's pending-subagent count (0 when unset)."
  (or (agent-repl--ws-get ws :pending-subagents) 0))

(defun agent-repl--ws-incf-pending-subagents (ws)
  "Increment WS's pending-subagent counter and return the new value."
  (unless ws (error "agent-repl--ws-incf-pending-subagents: ws is nil"))
  (let ((new (1+ (agent-repl--ws-pending-subagents ws))))
    (agent-repl--log ws "pending-subagents %s -> %d (incf)" ws new)
    (agent-repl--ws-put ws :pending-subagents new)
    new))

(defun agent-repl--ws-decf-pending-subagents (ws)
  "Decrement WS's pending-subagent counter and return the new value.

Floors at 0.  This is LOAD-BEARING ON EVERY TURN, not a defensive
edge-case guard: Claude Code empirically fires one unpaired
SubagentStop per turn (see the block comment above for the
N-starts/N+1-stops asymmetry).  Without the floor, the counter would
drift toward -infinity across a session and the gating predicate
`(zerop ...)' would silently start mis-classifying \"still running\"
as \"all done\"."
  (unless ws (error "agent-repl--ws-decf-pending-subagents: ws is nil"))
  (let* ((cur (agent-repl--ws-pending-subagents ws))
         (new (max 0 (1- cur))))
    (agent-repl--log ws "pending-subagents %s -> %d (decf, was %d)" ws new cur)
    (agent-repl--ws-put ws :pending-subagents new)
    new))

(defun agent-repl--fully-stopped-p (ws)
  "Return non-nil when WS is fully stopped — Stop fired and no pending subagents.
Used by Stop and SubagentStop callbacks to decide whether to drive the
`:thinking → :done' transition.  See the block comment above for the
coordination model."
  (and (agent-repl--ws-stop-received-p ws)
       (zerop (agent-repl--ws-pending-subagents ws))))

(defun agent-repl--ws-clear-stop-tracking (ws)
  "Reset the Stop / SubagentStop tracking fields on WS.
Called when the workspace transitions out of `:thinking' so the next
turn starts from a clean slate.  Resets `:stop-received' to nil and
`:pending-subagents' to 0."
  (unless ws (error "agent-repl--ws-clear-stop-tracking: ws is nil"))
  (agent-repl--log ws "clear-stop-tracking %s" ws)
  (agent-repl--ws-put ws :stop-received nil)
  (agent-repl--ws-put ws :pending-subagents 0))

;; Legacy APIs below delegate into the typed setters.  Call sites migrate
;; to the typed names in a later commit; retained here for the duration
;; of the migration so every existing caller keeps working.

(defun agent-repl--ws-set (ws state)
  "Set workspace WS to STATE.
Thin wrapper around `agent-repl--ws-set-agent-state' preserved for
callers that have not yet migrated to the typed setter.
STATE is one of: :thinking, :done, :permission, :inactive."
  (agent-repl--ws-set-agent-state ws state))

(defun agent-repl--ws-dir (ws)
  "Return the project root directory for workspace WS.
Reads :project-dir from the workspace plist.  Errors if not set."
  (or (agent-repl--ws-get ws :project-dir)
      (error "agent-repl--ws-dir: no :project-dir for workspace %s" ws)))

(defun agent-repl--align-buffer-to-ws-dir (buf ws)
  "Point BUF's buffer-local `default-directory' at WS's project root.
A workspace's panel buffers (the input composer, the webview) are born
via `get-buffer-create' / xwidget session creation, which both seed
`default-directory' from whatever buffer happened to be current at
creation time — frequently an unrelated repository or worktree.  Left
uncorrected, `SPC .' and every other `default-directory'-relative
command run from a panel window resolves against that foreign directory
rather than the worktree the REPL is actually attached to.  Repointing
BUF at WS's `:project-dir' keeps the panels anchored to their own
workspace, mirroring the same repoint the rename path already performs.

No-op when BUF is dead or WS has no `:project-dir' recorded yet — the
latter happens early in session startup, before the dir is initialized,
and a later panel show re-runs this alignment once the dir lands.  This
is deliberately soft rather than an assertion: the buffer can legitimately
exist before its workspace directory is known."
  (when (buffer-live-p buf)
    (when-let ((dir (agent-repl--ws-get ws :project-dir)))
      (with-current-buffer buf
        (setq default-directory (file-name-as-directory dir))))))

;;; Git status (async) -------------------------------------------------------

(defun agent-repl--workspace-clean-p (ws)
  "Return non-nil if workspace WS has no unstaged changes to tracked files.
Reads from a cached value updated asynchronously by
`agent-repl--async-refresh-git-status'.  Signals an error if the cache
has not yet been populated — callers must ensure the async git check has
completed before consulting this predicate."
  (let ((status (agent-repl--ws-get ws :git-clean)))
    (unless status
      (error "agent-repl--workspace-clean-p: :git-clean not populated for workspace %s" ws))
    (let ((result (eq status 'clean)))
      (agent-repl--log-verbose ws "workspace-clean-p ws=%s status=%s result=%s" ws status result)
      result)))

(defun agent-repl--git-check-in-progress-p (ws)
  "Return non-nil if a git-diff process is already running for workspace WS."
  (let ((result (when-let ((proc (agent-repl--ws-get ws :git-proc)))
                  (process-live-p proc))))
    (agent-repl--log-verbose ws "git-check-in-progress-p ws=%s result=%s" ws result)
    result))

(defun agent-repl--git-diff-sentinel (ws proc _event)
  "Process sentinel for `git diff --quiet' in workspace WS.
When PROC finishes, records `:git-clean' as `clean' or `dirty' and
triggers a state update via `agent-repl--update-ws-state'.
_EVENT is ignored."
  (unless (process-live-p proc)
    (let* ((exit-code (process-exit-status proc))
           (clean-result (cond
                          ((= 0 exit-code) 'clean)
                          ((= 1 exit-code) 'dirty)
                          (t (agent-repl--warn ws "git diff --quiet exited with code %d for ws=%s (git error, not dirty)"
                                               exit-code ws)
                             (agent-repl--log ws "git-diff-sentinel: unexpected exit-code=%d for ws=%s" exit-code ws)
                             nil))))
      (agent-repl--log-verbose ws "git-diff-sentinel: ws=%s exit-code=%s result=%s" ws exit-code clean-result)
      (when clean-result
        (agent-repl--ws-put ws :git-clean clean-result))
      (agent-repl--ws-put ws :git-proc nil)
      (agent-repl--update-ws-state ws))))

(defun agent-repl--async-refresh-git-status (ws)
  "Asynchronously refresh the git cleanliness cache for workspace WS.
Starts `git diff --quiet' in WS's directory.  On exit, sets `:git-clean'
to `clean' or `dirty' in the workspace plist and calls
`agent-repl--update-ws-state' to apply any resulting state transition.
A no-op if a check is already in progress for WS."
  (when-let ((dir (agent-repl--ws-dir ws)))
    (if (agent-repl--git-check-in-progress-p ws)
        (agent-repl--log-verbose ws "async-refresh-git-status: ws=%s skipped (already in progress)" ws)
      (agent-repl--log-verbose ws "async-refresh-git-status: ws=%s starting git diff" ws)
      (let* ((default-directory dir)
             (proc (agent-repl--make-process-git
                    (format "agent-repl-git-%s" ws)
                    '("diff" "--quiet")
                    (apply-partially #'agent-repl--git-diff-sentinel ws))))
        (agent-repl--ws-put ws :git-proc proc)))))

;;; Tab-bar rendering ---------------------------------------------------------
;;
;; Appearance is described by a small pyramid:
;;
;;   1. Named constants — every color / label / font-weight literal lives
;;      in a `agent-repl--color-*' / `--label-*' / `--tab-weight' defconst.
;;   2. `agent-repl--tab-default' and `agent-repl--tab-palette' — the
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

(defconst agent-repl--color-init-blue        "#3366cc"
  "Blue used for the :init agent-state tab background.")

(defconst agent-repl--color-thinking-red     "#cc3333"
  "Red used for the :thinking agent-state tab background.")

(defconst agent-repl--color-done-green       "#1a7a1a"
  "Dark green used for :done and :permission tab backgrounds.")

(defconst agent-repl--color-idle-orange      "#d97706"
  "Orange used for the :idle agent-state tab background.
:idle means \"session alive, awaiting prompt or decayed from :done\" — an
explicit palette entry (not a fallback) so idle workspaces are
visually distinct from states that have no palette mapping.")

(defconst agent-repl--color-idle-async-amber "#f59e0b"
  "Amber used for the :idle-async agent-state tab background.
:idle-async means \"session idle/available, but detached background work
is still running\" — mirrors the webapp's amber async bubble border
\(--async #f59e0b).  Distinct from :idle orange (available, nothing
running) and :thinking red (a turn actively in flight).")

(defconst agent-repl--color-stop-failed-magenta "#8b1f8b"
  "Magenta used for the :stop-failed agent-state tab background.
:stop-failed means the StopFailure hook fired — the agent's turn ended
due to an API error (rate limit, auth failure, billing, etc.).  The
agent session is still alive and re-promptable; :dead (the plain ❌
badge) is reserved for agent session death.  A distinct color signals
\"needs your attention, but not the same kind of attention as :thinking
or :dead\".")

(defconst agent-repl--color-done-green-bright "#2a8c2a"
  "Brighter green used for :done / :permission bracket-fg on selected
tabs; readable against `agent-repl--color-selected-bg'.")

(defconst agent-repl--color-default-bracket  "white"
  "White used for bracket numerals on unselected tabs of any state.")

(defconst agent-repl--color-selected-bg      "#c0c0c0"
  "Grey used for the background of selected tabs.")

(defconst agent-repl--color-light            "white"
  "Light foreground for dark state backgrounds.")

(defconst agent-repl--color-dark             "black"
  "Dark foreground for light state backgrounds.")

(defconst agent-repl--label-permission       "❓"
  "Bracket label shown adjacent to the numeric index when the agent is
asking for a permission decision.")

(defconst agent-repl--label-dead             "❌"
  "Bracket label shown adjacent to the numeric index when the agent
session has died.")

(defconst agent-repl--label-stop-failed      "⚠"
  "Bracket label shown adjacent to the numeric index when the
StopFailure hook fired (turn ended on an API error, but the agent
session is still alive and re-promptable).")

(defconst agent-repl--label-start-failed     "🚫"
  "Bracket label shown adjacent to the numeric index when starting
the agent failed.  Distinct from `:stop-failed' (⚠, a live re-promptable
session) and `:dead' (❌, a session that died) — the session never came
up at all.")

(defconst agent-repl--label-merge-conflict   "💥"
  "Bracket label shown adjacent to the numeric index when a workspace's
merge was rejected by a cherry-pick conflict (real conflict markers
left behind, auto-resolver declined or interactive abort).  Distinct
from `:dead' (session died) and `:merge-failed' (silent git-aborted, no
CHERRY_PICK_HEAD) — collision metaphor reflects the actual conflict.")

(defconst agent-repl--label-merged           "🔀"
  "Bracket label shown adjacent to the numeric index when the
workspace's branch has been merged into its source (`:repl-state'
`:merged').  Takes precedence over the `:dead' badge so a merged
workspace whose session has since died still reads as merged, not
dead.")

(defconst agent-repl--label-merge-failed     "⛔"
  "Bracket label shown adjacent to the numeric index when a workspace's
merge dispatch failed silently (`:repl-state' `:merge-failed') —
typically because the source repo is mid cherry-pick/rebase/merge and
git refused the cherry-pick.  Distinct from `:dead' (❌) so a stuck
merge does not look like a dead session at a glance.")

(defconst agent-repl--tab-weight             'bold
  "Font weight applied to every tab face.")

(defconst agent-repl--color-flash-bg         "#3b82f6"
  "Saturated blue used for the transient flash face — see `agent-repl-flash-tab'.
Distinct from `agent-repl--color-init-blue' so a flash is not confused
with the :init agent-state at a glance.")

(defcustom agent-repl-flash-count 2
  "Number of on/off cycles when `agent-repl-flash-tab' pulses a tab."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-flash-duration 1.0
  "Total duration of a `agent-repl-flash-tab' pulse, in seconds.
Distributed evenly across `agent-repl-flash-count' on/off cycles."
  :type 'number
  :group 'agent-repl)

;; --- Appearance palette --- ;;

(defconst agent-repl--tab-default
  `(:unselected (:bg unspecified
                 :fg unspecified
                 :bracket-fg ,agent-repl--color-default-bracket
                 :weight ,agent-repl--tab-weight)
    :selected   (:bg ,agent-repl--color-selected-bg
                 :fg ,agent-repl--color-dark
                 :bracket-fg ,agent-repl--color-dark
                 :weight ,agent-repl--tab-weight))
  "Default tab-appearance spec for states absent from `agent-repl--tab-palette'.")

(defconst agent-repl--tab-palette
  `((:init
     :face       agent-repl-tab-init
     :unselected (:bg ,agent-repl--color-init-blue
                  :fg ,agent-repl--color-light
                  :bracket-fg ,agent-repl--color-default-bracket
                  :weight ,agent-repl--tab-weight)
     :selected   (:bg ,agent-repl--color-selected-bg
                  :fg ,agent-repl--color-dark
                  :bracket-bg ,agent-repl--color-init-blue
                  :bracket-fg ,agent-repl--color-light
                  :weight ,agent-repl--tab-weight))
    (:thinking
     :face       agent-repl-tab-thinking
     :unselected (:bg ,agent-repl--color-thinking-red
                  :fg ,agent-repl--color-light
                  :bracket-fg ,agent-repl--color-default-bracket
                  :weight ,agent-repl--tab-weight)
     :selected   (:bg ,agent-repl--color-selected-bg
                  :fg ,agent-repl--color-dark
                  :bracket-bg ,agent-repl--color-thinking-red
                  :bracket-fg ,agent-repl--color-light
                  :weight ,agent-repl--tab-weight))
    (:done
     :face       agent-repl-tab-done
     :unselected (:bg ,agent-repl--color-done-green
                  :fg ,agent-repl--color-dark
                  :bracket-fg ,agent-repl--color-default-bracket
                  :weight ,agent-repl--tab-weight)
     :selected   (:bg ,agent-repl--color-selected-bg
                  :fg ,agent-repl--color-dark
                  :bracket-bg ,agent-repl--color-done-green
                  :bracket-fg ,agent-repl--color-light
                  :weight ,agent-repl--tab-weight))
    (:permission
     :face       agent-repl-tab-permission
     :label      ,agent-repl--label-permission
     :unselected (:bg ,agent-repl--color-done-green
                  :fg ,agent-repl--color-dark
                  :bracket-fg ,agent-repl--color-default-bracket
                  :weight ,agent-repl--tab-weight)
     :selected   (:bg ,agent-repl--color-selected-bg
                  :fg ,agent-repl--color-dark
                  :bracket-bg ,agent-repl--color-done-green
                  :bracket-fg ,agent-repl--color-light
                  :weight ,agent-repl--tab-weight))
    (:idle
     :face       agent-repl-tab-idle
     :unselected (:bg ,agent-repl--color-idle-orange
                  :fg ,agent-repl--color-dark
                  :bracket-fg ,agent-repl--color-default-bracket
                  :weight ,agent-repl--tab-weight)
     :selected   (:bg ,agent-repl--color-selected-bg
                  :fg ,agent-repl--color-dark
                  :bracket-bg ,agent-repl--color-idle-orange
                  :bracket-fg ,agent-repl--color-light
                  :weight ,agent-repl--tab-weight))
    (:idle-async
     :face       agent-repl-tab-idle-async
     :unselected (:bg ,agent-repl--color-idle-async-amber
                  :fg ,agent-repl--color-dark
                  :bracket-fg ,agent-repl--color-default-bracket
                  :weight ,agent-repl--tab-weight)
     :selected   (:bg ,agent-repl--color-selected-bg
                  :fg ,agent-repl--color-dark
                  :bracket-bg ,agent-repl--color-idle-async-amber
                  :bracket-fg ,agent-repl--color-light
                  :weight ,agent-repl--tab-weight))
    (:stop-failed
     :face       agent-repl-tab-stop-failed
     :label      ,agent-repl--label-stop-failed
     :unselected (:bg ,agent-repl--color-stop-failed-magenta
                  :fg ,agent-repl--color-light
                  :bracket-fg ,agent-repl--color-default-bracket
                  :weight ,agent-repl--tab-weight)
     :selected   (:bg ,agent-repl--color-selected-bg
                  :fg ,agent-repl--color-dark
                  :bracket-bg ,agent-repl--color-stop-failed-magenta
                  :bracket-fg ,agent-repl--color-light
                  :weight ,agent-repl--tab-weight))
    (:start-failed
     :face       agent-repl-tab-stop-failed
     :label      ,agent-repl--label-start-failed
     :unselected (:bg ,agent-repl--color-stop-failed-magenta
                  :fg ,agent-repl--color-light
                  :bracket-fg ,agent-repl--color-default-bracket
                  :weight ,agent-repl--tab-weight)
     :selected   (:bg ,agent-repl--color-selected-bg
                  :fg ,agent-repl--color-dark
                  :bracket-bg ,agent-repl--color-stop-failed-magenta
                  :bracket-fg ,agent-repl--color-light
                  :weight ,agent-repl--tab-weight))
    (:dead
     :label      ,agent-repl--label-dead)
    (:merge-conflict
     :label      ,agent-repl--label-merge-conflict)
    (:merge-failed
     :label      ,agent-repl--label-merge-failed)
    (:merged
     :label      ,agent-repl--label-merged))
  "Per-state tab-appearance palette.
Each entry fully describes both selected and unselected looks for a
agent-state keyword via nested `:unselected' and `:selected' plists.
`:repl-state :inactive' does not contribute to color (it is bookkeeping
only).")

(defun agent-repl--tab-spec (state selected)
  "Return the appearance spec (plist) for STATE with SELECTED flag.
Falls back to `agent-repl--tab-default' when STATE has no palette entry.
Keys in the returned plist: :bg :fg :bracket-fg :bracket-bg :weight."
  (let* ((row (alist-get state agent-repl--tab-palette))
         (key (if selected :selected :unselected)))
    (or (plist-get row key)
        (plist-get agent-repl--tab-default key))))

(defun agent-repl--tab-spec-bracket-only (state selected)
  "Return appearance spec applying STATE's color to the [N] bracket only.
Pulls bracket-bg/bracket-fg/weight from STATE's palette row (per
SELECTED) and leaves :bg/:fg unspecified so the separator and name
region inherit defaults.  Used for workspaces whose agent panels
have been dismissed: the bracket retains the state's color so the
workspace's agent-state stays visible while the rest of the tab
falls back to the default appearance."
  (let* ((full (agent-repl--tab-spec state selected))
         (bracket-bg (or (plist-get full :bracket-bg)
                         (plist-get full :bg))))
    `(:bg unspecified
      :fg unspecified
      :bracket-bg ,bracket-bg
      :bracket-fg ,(plist-get full :bracket-fg)
      :weight ,(or (plist-get full :weight) agent-repl--tab-weight))))

;; --- defface forms referencing the named constants --- ;;
;; Each `:unselected' palette row has the same colors these forms read,
;; by construction.  Kept as explicit defface calls so Doom users can
;; customize via `customize-face' (the Doom theming hook).

(defface agent-repl-tab-init
  `((t :background ,agent-repl--color-init-blue
       :foreground ,agent-repl--color-light
       :weight ,agent-repl--tab-weight))
  "Face for workspace tabs where the agent is initializing (blue).")

(defface agent-repl-tab-thinking
  `((t :background ,agent-repl--color-thinking-red
       :foreground ,agent-repl--color-light
       :weight ,agent-repl--tab-weight))
  "Face for workspace tabs where the agent is thinking (red).")

(defface agent-repl-tab-done
  `((t :background ,agent-repl--color-done-green
       :foreground ,agent-repl--color-dark
       :weight ,agent-repl--tab-weight))
  "Face for workspace tabs where the agent is done (green).")

(defface agent-repl-tab-permission
  `((t :background ,agent-repl--color-done-green
       :foreground ,agent-repl--color-dark
       :weight ,agent-repl--tab-weight))
  "Face for workspace tabs where the agent needs permission (green + emoji).")

(defface agent-repl-tab-idle
  `((t :background ,agent-repl--color-idle-orange
       :foreground ,agent-repl--color-dark
       :weight ,agent-repl--tab-weight))
  "Face for workspace tabs where the agent is idle (orange).")

(defface agent-repl-tab-idle-async
  `((t :background ,agent-repl--color-idle-async-amber
       :foreground ,agent-repl--color-dark
       :weight ,agent-repl--tab-weight))
  "Face for workspace tabs that are idle but have background work running (amber).")

(defface agent-repl-tab-stop-failed
  `((t :background ,agent-repl--color-stop-failed-magenta
       :foreground ,agent-repl--color-light
       :weight ,agent-repl--tab-weight))
  "Face for workspace tabs where the last turn failed via the
StopFailure hook (magenta + ⚠).")

(defface agent-repl-tab-flash
  `((t :background ,agent-repl--color-flash-bg
       :foreground ,agent-repl--color-light
       :weight ,agent-repl--tab-weight))
  "Transient face applied while a workspace is in a `agent-repl-flash-tab'
pulse — solid blue background regardless of the underlying state.")

(defface agent-repl-queued-messages
  '((t :inherit font-lock-comment-face))
  "Face for the queued-message count segment.
Subdued on purpose (per §2.13's UI intent): a queued message is parked
for later, explicitly NOT interrupting the in-flight turn, so its
indicator must not read as an active-state badge.")

(defun agent-repl--ws-flashing-p (ws)
  "Return non-nil if workspace WS is currently in a flash pulse."
  (agent-repl--ws-get ws :flashing))

(defun agent-repl--ws-set-flashing (ws val)
  "Set workspace WS's :flashing flag to VAL.
The tab renderer treats non-nil as an instruction to paint the tab with
the flash face/spec on the next refresh."
  (agent-repl--ws-put ws :flashing val))

(defun agent-repl--flash-spec ()
  "Return the appearance spec plist used for a flashing tab.
Mirrors a normal palette row (see `agent-repl--tab-palette' docstring)
but paints both the bracket and the name region in a uniform blue."
  `(:bg ,agent-repl--color-flash-bg
    :fg ,agent-repl--color-light
    :bracket-bg ,agent-repl--color-flash-bg
    :bracket-fg ,agent-repl--color-light
    :weight ,agent-repl--tab-weight))

(defun agent-repl--force-tab-bar-redraw ()
  "Force the tab-bar to repaint NOW, bypassing its string-equality cache.
Tab-bar rendering caches by string equality, and `equal' on propertized
strings ignores text properties — so changes that only differ in face
\(e.g. a `:flashing' toggle\) won't trigger a repaint via
`force-mode-line-update' alone.  This helper flips the load-bearing
`agent-repl--tabline-space-toggle' so the next tabline render appends
a different cache-buster suffix (`agent-repl--tabline-cache-buster')
and produces a different string, then drives the tab-bar update
primitives.  See the block comment above the toggle's defvar for the
rationale."
  (setq agent-repl--tabline-space-toggle (not agent-repl--tabline-space-toggle))
  (when (fboundp 'tab-bar-tabs-set)
    (tab-bar-tabs-set (tab-bar-tabs)))
  (when (fboundp 'tab-bar--update-tab-bar-lines)
    (tab-bar--update-tab-bar-lines t))
  (force-mode-line-update t))

(defun agent-repl--flash-step (ws step total-steps interval)
  "Drive step STEP of WS's flash, then chain the next step via `run-at-time'.
STEP is 0-based.  TOTAL-STEPS is `(1+ (* 2 COUNT))' — one entry for
each on/off toggle plus a final cleanup.  Even STEPs paint the flash
on, odd STEPs paint it off.  The terminal step (STEP == TOTAL-STEPS-1)
clears `:flashing' and stops the chain — it does NOT schedule a
successor.  Every step calls `agent-repl--force-tab-bar-redraw' so
the tab repaints at flash speed instead of waiting for the 1-Hz poll."
  (if (>= step (1- total-steps))
      (progn
        (agent-repl--ws-set-flashing ws nil)
        (agent-repl--force-tab-bar-redraw))
    (agent-repl--ws-set-flashing ws (cl-evenp step))
    (agent-repl--force-tab-bar-redraw)
    (run-at-time interval nil
                 #'agent-repl--flash-step ws (1+ step) total-steps interval)))

(defun agent-repl-flash-tab (ws &optional count duration)
  "Pulse the tab for workspace WS COUNT times across DURATION seconds.
COUNT defaults to `agent-repl-flash-count'; DURATION defaults to
`agent-repl-flash-duration'.  Used to draw the user's attention to a
workspace whose tab-bar position just changed (e.g., after a deprio
push-to-back), so the eye can track it to its new home.

Drives the sequence via `agent-repl--flash-step', which runs the
first step synchronously and then chains each subsequent step from
the previous one via `run-at-time'.  Versus scheduling every toggle
up-front, the chain gives uniform real-time spacing when Emacs is
busy, makes mid-sequence cancellation easy (only one timer is ever
pending), and avoids the closure-capture pitfalls of shared loop
variables."
  (let* ((count (or count agent-repl-flash-count))
         (duration (or duration agent-repl-flash-duration))
         (interval (/ duration (* 2.0 count)))
         (total-steps (1+ (* 2 count))))
    (agent-repl--log ws "flash-tab ws=%s count=%d duration=%s" ws count duration)
    (agent-repl--flash-step ws 0 total-steps interval)))

(defun agent-repl--flash-current-tab ()
  "Pulse the current workspace's tab via `agent-repl-flash-tab'.
Centralizes the post-jump flash so every identity-based workspace jump
draws the eye to the destination tab uniformly.  No-op when
`agent-repl-flash-tab' is unbound or when `agent-repl--ws-current-name'
returns nil — those come from optional layers that may not be loaded yet
at startup."
  (when (fboundp 'agent-repl-flash-tab)
    (agent-repl-flash-tab (agent-repl--ws-current-name))))

(defun agent-repl--render-tab (name spec label name-face img-str)
  "Render a tab string for workspace NAME from SPEC.
SPEC is a plist with keys :bg :fg :bracket-fg :weight (see
`agent-repl--tab-palette' docstring).  NAME-FACE is applied to the
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
            (propertize (format agent-repl-tab-bracket-format label) 'face bracket-face)
            (when img-str (concat " " img-str " "))
            (propertize (format agent-repl-tab-name-padding name) 'face name-face)
            " ")))

(defun agent-repl--tab-label (state index)
  "Return the tab label for STATE and numeric INDEX.
When the palette defines a `:label' for STATE (e.g. \"❓\" for permission,
\"❌\" for dead), the suffix is appended after the numeric index
\(e.g. \"1❓\").  Otherwise returns the index as a plain string."
  (let ((suffix (plist-get (alist-get state agent-repl--tab-palette) :label)))
    (if suffix
        (concat (number-to-string index) suffix)
      (number-to-string index))))

(defun agent-repl--tab-face (state selected)
  "Return the face symbol for the NAME portion of a tab.
For unselected tabs, uses the palette row's `:face' or falls back to
the Doom tab face.  For selected tabs, always uses the Doom selected-tab
face so selection dims the state color."
  (if selected
      (agent-repl--ws-tab-selected-face)
    (or (plist-get (alist-get state agent-repl--tab-palette) :face)
        (agent-repl--ws-tab-face))))

(defun agent-repl--tab-priority-image-str (name)
  "Return a propertized image string for workspace NAME's priority, or nil."
  (when-let ((priority (agent-repl--ws-get name :priority)))
    (when-let ((img (agent-repl--priority-image priority)))
      (propertize " " 'display img))))

(defun agent-repl--ws-display-state (ws)
  "Return the palette display key for WS.
Delegates to `agent-repl--ws-render-status' (the single source of
truth for visual state across the tab-bar and project
picker), then layers panel-visibility suppression on top: when the
render-state is non-nil AND no agent panel is present in WS's
live-or-saved window layout, returns nil regardless of state — this
suppresses full-tab coloring (state-colored name and label badges
like ❓/❌/⚠) for workspaces whose panels the user has dismissed.
`:agent-state' is preserved on the plist so the original color
reappears the next time the user reopens panels.  The nil-state
shortcut avoids calling `agent-repl--ws-agent-open-p' on
workspaces that have no state to suppress in the first place.

UI-boundary tolerance: the tab-bar iterates `persp-names-cache',
which can briefly contain names the workspace hash doesn't yet know
about (a mid-creation persp, the `none' sentinel persp).
`--ws-render-status' would signal `user-error' for those; here we
short-circuit to nil so rendering proceeds without color or badge.
This is the documented exception to the no-fallback rule, scoped
to the renderer-input boundary.

NOTE: this function answers the question \"what state should drive
the full tab appearance?\".  The orthogonal question \"what state
should color the [N] bracket alone?\" is answered by
`agent-repl--ws-bracket-state', which ignores panel visibility so
the bracket keeps its color when panels are closed."
  (when (agent-repl--ws-known-p ws)
    (let ((state (agent-repl--ws-render-status ws)))
      (if (and state (not (agent-repl--ws-agent-open-p ws)))
          nil
        state))))

(defun agent-repl--ws-bracket-state (ws)
  "Return WS's render-state for [N]-bracket coloring.
Unlike `agent-repl--ws-display-state', this does NOT suppress when
panels are closed: the bracket should retain the state's color even
for workspaces whose agent panels have been dismissed, so the
render-state remains visible at a glance.

UI-boundary tolerance: returns nil for unknown ws (see
`--ws-display-state' docstring for rationale)."
  (when (agent-repl--ws-known-p ws)
    (agent-repl--ws-render-status ws)))

(defun agent-repl--render-tab-entry (name current-name index)
  "Render a single tab entry for workspace NAME.
CURRENT-NAME is the active workspace name.  INDEX is the 1-based
tab position.  The display state (from `agent-repl--ws-display-state')
drives the name face.  The appearance spec is resolved via
`agent-repl--tab-spec' when display-state is non-nil; when display-state
is nil but `agent-repl--ws-bracket-state' returns a state (i.e., panels
dismissed for a workspace that still has agent-state), the spec is
built via `agent-repl--tab-spec-bracket-only' so only the [N] bracket
keeps the state's color.  The bracket label is driven by bracket-state
when display-state is suppressed, so palette `:label' glyphs (❓ for
:permission, ❌ for :dead, ⚠ for :stop-failed) render even on
workspaces whose agent panels are closed — only the full-tab
background requires panels to be open.  When the workspace's
`:flashing' flag is set \(see `agent-repl-flash-tab'\), the spec and
name face are overridden to a uniform pulse so the tab stands out."
  (let* ((selected      (equal current-name name))
         (flashing      (agent-repl--ws-flashing-p name))
         (display-state (agent-repl--ws-display-state name))
         (bracket-state (and (null display-state)
                             (agent-repl--ws-bracket-state name)))
         (spec          (cond
                         (flashing      (agent-repl--flash-spec))
                         (bracket-state (agent-repl--tab-spec-bracket-only
                                         bracket-state selected))
                         (t             (agent-repl--tab-spec
                                         display-state selected))))
         (label         (agent-repl--tab-label
                         (or display-state bracket-state) index))
         (face          (if flashing
                            'agent-repl-tab-flash
                          (agent-repl--tab-face display-state selected)))
         (img-str       (agent-repl--tab-priority-image-str name)))
    (agent-repl--render-tab name spec label face img-str)))

(defun agent-repl--filter-hidden-names (names current-name)
  "Drop NAMES whose `:repl-state' is `:hidden' when hide-mode is on.
CURRENT-NAME is always retained so the active workspace stays visible.
When `agent-repl-hide-mode-enabled' is nil, returns NAMES unchanged.

Used by workspace cycling (`agent-repl--workspace-cycle' in commands.el)
to skip soon-to-be-killed `:hidden' workspaces.  The tab-bar itself is
NOT filtered — it reflects the raw persp-names-cache, and `:hidden'
workspaces disappear naturally once the next workspace switch triggers
`agent-repl--sweep-hidden-workspaces'."
  (if agent-repl-hide-mode-enabled
      (cl-remove-if
       (lambda (n)
         (and (not (equal n current-name))
              (eq (agent-repl--ws-repl-state n) :hidden)))
       names)
    names))

(cl-defun agent-repl--tabline-rendered-entries (&optional (names nil names-supplied-p))
  "Return the list of rendered tab-entry strings for NAMES.

Each element is the propertized output of `agent-repl--render-tab-entry'
for the corresponding workspace, 1-indexed.  Used by both
`agent-repl--tabline-advice' (which mapconcats with a space separator)
and `agent-repl-workspace-tabline-formatted' (which packs entries
into a single row, eliding overflow behind \"+N\" badges).

No hide-project-dirs filtering happens here: that mode hides matching
workspaces at the persp layer (they are killed and leave
`persp-names-cache' entirely — see `agent-repl-toggle-hide-project-dirs'),
so the raw persp list this renders is already the visible set and the
1-indexed positions match `SPC <n>'.

When NAMES is not supplied, defaults to `agent-repl--ws-tabline-names'
(the persp-mode integration wrapper in `workspace.el', minus the
workspaces of folded repos) rather than
`+workspace-list-names' directly — the tab-bar reflects agent-repl's
own notion of which workspaces it owns, not persp-mode's raw cache.
Folded repos drop out here, and since the index is a 1-based position
in the surviving list, the visible tab numbers stay contiguous and keep
matching `SPC <n>' (which indexes the same list)."
  (let* ((names (if names-supplied-p names (agent-repl--ws-tabline-names)))
         (current-name (agent-repl--ws-current-name)))
    (cl-loop for name in names
             for i from 1
             collect (agent-repl--render-tab-entry name current-name i))))

(defconst agent-repl--tabline-row-count 2
  "Number of rows the workspace tab-bar ALWAYS renders.
Fixed (never varies with workspace count), so the tab-bar's pixel
height is constant.  A height change resizes the NSWindow on macOS,
and a clipped resize livelocks redisplay at 100% CPU
\(`ns_change_tab_bar_height' -> `adjust_frame_size' in src/); pinning
the row count sidesteps that entirely.  Entries beyond what the fixed
rows hold are elided behind `+N' overflow badges rather than wrapping
to a third row (see `agent-repl--tabline-rows').")

(defun agent-repl--pack-first-fit (widths caps)
  "Greedily first-fit WIDTHS into rows sized by CAPS.
WIDTHS is a list of entry column-widths in display order.  CAPS is a
list of each row's maximum column budget; its length is the row count.
Entries are placed left to right: each is appended to the current row
when it (plus a one-column separator after the first entry already on
that row) still fits that row's CAPS budget, otherwise the next row is
started.  Returns a list of per-row entry COUNTS (same length as CAPS)
when every entry is placed, or nil when the entries do not all fit in
`(length CAPS)' rows."
  (let* ((nrows (length caps))
         (counts (make-list nrows 0))
         (row 0)
         (used 0)
         (rest widths)
         (ok t))
    (while (and ok rest)
      (let* ((w (car rest))
             (sep (if (> (nth row counts) 0) 1 0)))
        (cond
         ((<= (+ used sep w) (nth row caps))
          (setf (nth row counts) (1+ (nth row counts)))
          (setq used (+ used sep w)
                rest (cdr rest)))
         ((< row (1- nrows))
          (setq row (1+ row) used 0))
         (t (setq ok nil)))))
    (and ok counts)))

(defun agent-repl--tabline-overflow-caps (width max-rows badge-w)
  "Return per-row column budgets for an overflowing MAX-ROWS tab-bar.
Reserves a `+N' overflow badge worth of columns (BADGE-W) at the start
of the first row and the end of the last row, so the leading/trailing
badges never push a row past WIDTH; interior rows keep the full WIDTH.
With a single row both badges share it."
  (if (= max-rows 1)
      (list (max 1 (- width (* 2 badge-w))))
    (let ((edge (max 1 (- width badge-w))))
      (append (list edge)
              (make-list (- max-rows 2) width)
              (list edge)))))

(defun agent-repl--tabline-render-rows (entries counts lead trail width)
  "Render ENTRIES into rows per COUNTS, with LEAD/TRAIL badge strings.
COUNTS is a per-row entry count (see `agent-repl--pack-first-fit').
Each row joins its slice of ENTRIES with single spaces; LEAD is
prepended to the first row and TRAIL appended to the last row.  Every
row is hard-truncated to WIDTH columns as a final guard, so a
pathologically narrow frame can never make a row wrap.  Returns a list
of `(length COUNTS)' strings, none containing a newline."
  (let ((idx 0)
        (nrows (length counts))
        (rows nil))
    (dotimes (r nrows)
      (let* ((k (nth r counts))
             (slice (seq-subseq entries idx (+ idx k)))
             (row (mapconcat #'identity slice " ")))
        (setq idx (+ idx k))
        (when (= r 0)
          (setq row (concat lead row)))
        (when (= r (1- nrows))
          (setq row (concat row trail)))
        (when (> (length row) width)
          (setq row (substring row 0 width)))
        (push row rows)))
    (nreverse rows)))

(defun agent-repl--tabline-entry-width (entry)
  "Return ENTRY's rendered width in character-column units.

The tab-bar packs entries against a column budget (see
`agent-repl--tabline-rows'), but physical line wrapping is decided in
PIXELS.  Measuring an entry by its character `length' undercounts any
entry carrying a `display' image: a priority badge is one space wide
in characters but a whole glyph wide in pixels (see
`agent-repl--tab-priority-image-str').  A row the packer believed fit
could then overflow the frame in pixels and wrap to a third physical
row — the `ns_change_tab_bar_height' livelock this whole subsystem
exists to prevent.

Entries with no `display' property are measured with `string-width'
\(exact for mono and wide/CJK glyphs, and equal to `length' for plain
ASCII, so pre-existing fit decisions are unchanged).  An entry that
carries a `display' property is measured with `string-pixel-width'
and converted to columns by dividing by `frame-char-width', rounded
UP so the estimate never under-reserves.  Never returns less than 1."
  (max 1
       (if (text-property-not-all 0 (length entry) 'display nil entry)
           (ceiling (string-pixel-width entry) (max 1 (frame-char-width)))
         (string-width entry))))

(defun agent-repl--tabline-rows (entries current-pos width max-rows)
  "Pack ENTRIES into EXACTLY MAX-ROWS rows, each no wider than WIDTH.

ENTRIES is a list of rendered tab-entry strings (see
`agent-repl--tabline-rendered-entries').  Returns a list of MAX-ROWS
strings, adjacent entries joined by a single space within a row and no
string ever containing a newline.  Unused trailing rows are the empty
string, so the row COUNT is fixed at MAX-ROWS regardless of how many
entries there are.

When all ENTRIES fit within MAX-ROWS full-width rows they are all
shown with no badges.  Otherwise a contiguous window of entries around
CURRENT-POS (0-based index of the current workspace; nil falls back to
0) is shown — the current entry is ALWAYS included — and the elided
entries before/after the window are summarized by a leading \"+N \"
badge on the first row and a trailing \" +N\" badge on the last row.

The row count must be FIXED, never varying with the entry count: a
change in row count alters the tab-bar's pixel height, and on macOS a
tab-bar height change resizes the NSWindow; when that resize is clipped
\(e.g. by the screen edge) the requested and realized frame sizes never
agree and redisplay livelocks at 100% CPU retrying the resize
\(`ns_change_tab_bar_height' -> `adjust_frame_size' in src/).  Elision
behind badges, not wrapping to a further row, absorbs any overflow.

WIDTH and the per-row caps are column budgets.  Entry widths are
measured with `agent-repl--tabline-entry-width', which counts an
image-bearing entry by its pixel width (converted to columns), not
its character length — a column-accurate width is what keeps this
fixed two-row fit decision from letting a badge-bearing row overflow
the frame in pixels and wrap to a third physical row."
  (let ((n (length entries)))
    (if (= n 0)
        (make-list max-rows "")
      (let* ((widths (mapcar #'agent-repl--tabline-entry-width entries))
             ;; Do all entries fit MAX-ROWS full-width rows?  If so, no
             ;; badges and no windowing are needed.
             (full (agent-repl--pack-first-fit
                    widths (make-list max-rows width))))
        (if full
            (agent-repl--tabline-render-rows entries full "" "" width)
          ;; Overflow: grow a window around the current entry, packing
          ;; it into MAX-ROWS rows with badge columns reserved
          ;; conservatively on the first and last rows.
          (let* ((cur (min (max (or current-pos 0) 0) (1- n)))
                 (badge-w (+ 2 (length (number-to-string n)))) ; "+N " / " +N"
                 (caps (agent-repl--tabline-overflow-caps width max-rows badge-w))
                 (lo cur)
                 (hi cur))
            (catch 'full
              (while (or (> lo 0) (< hi (1- n)))
                (let ((grew nil))
                  (when (and (< hi (1- n))
                             (agent-repl--pack-first-fit
                              (seq-subseq widths lo (+ hi 2)) caps))
                    (setq hi (1+ hi) grew t))
                  (when (and (> lo 0)
                             (agent-repl--pack-first-fit
                              (seq-subseq widths (1- lo) (1+ hi)) caps))
                    (setq lo (1- lo) grew t))
                  (unless grew (throw 'full nil)))))
            (let* ((window (seq-subseq entries lo (1+ hi)))
                   (win-widths (seq-subseq widths lo (1+ hi)))
                   (counts (or (agent-repl--pack-first-fit win-widths caps)
                               ;; Degenerate: the lone current entry is
                               ;; wider than a row's budget; still show
                               ;; it (truncated by the render guard).
                               (cons 1 (make-list (1- max-rows) 0))))
                   (lead (if (> lo 0) (format "+%d " lo) ""))
                   (trail (if (< hi (1- n)) (format " +%d" (- n 1 hi)) "")))
              (agent-repl--tabline-render-rows window counts lead trail width))))))))

(defun agent-repl--join-tabline-rows (lines)
  "Join LINES (pre-centered tab-bar rows) with row separators.

Each row is terminated with a single unfaced space; adjacent rows are
separated by that space followed by a newline, and the final row also
gets the trailing space (no newline after it).  This is what stops the
tab-bar's per-row redisplay (`display_tab_bar_line' in src/xdisp.c)
from painting the previous row's last glyph face across the row's
remainder.  `extend_face_to_end_of_line' uses the last glyph's face
regardless of the face's `:extend' attribute, and since each rendered
tab-entry ends with a faced name-padding space (see
`agent-repl--render-tab'), the selected tab's background would
otherwise visibly stretch to the frame's right edge whenever the
selected tab landed at the end of any wrapped row, including the
final one.

Callers must size each row so the trailing unfaced space lands within
the frame's visible columns (col < `frame-width').  `+doom-dashboard
--center' only left-pads, so a row of length `frame-width' has its
appended space at column `frame-width' — offscreen — and the last
visible glyph is still the faced one.  Size and center rows to
`(1- (frame-width))' to leave room for the terminator."
  (if (null lines)
      ""
    (concat (mapconcat #'identity lines " \n") " ")))

(cl-defun agent-repl--tabline-advice (&optional (names nil names-supplied-p))
  "Override for `+workspace--tabline' to color tabs by agent status.

The tab-bar reflects every workspace in NAMES (defaulting to
`agent-repl--ws-tabline-names' — the persp-mode integration wrapper
in `workspace.el', which intersects `persp-names-cache' with
agent-repl's own registration, then drops the workspaces of
folded repos); no hide-mode filtering is applied here.
Hide-mode operates at the persp level — `:hidden' workspaces
get persp-killed by `agent-repl--sweep-hidden-workspaces' on the
next workspace switch and disappear from `persp-names-cache' (and
therefore the tab-bar) naturally."
  (let* ((resolved-names (if names-supplied-p names (agent-repl--ws-tabline-names)))
         (entries (agent-repl--tabline-rendered-entries resolved-names))
         (current-name (agent-repl--ws-current-name))
         (states (mapcar (lambda (n)
                           (cons n (agent-repl--ws-display-state n)))
                         resolved-names)))
    (agent-repl--log-verbose nil "tabline-advice: current=%s hide=%s states=%S"
                              current-name agent-repl-hide-mode-enabled states)
    (concat
     (mapconcat #'identity entries " ")
     ;; Cache-buster toggle — DO NOT REMOVE.  See the block comment
     ;; above `agent-repl--tabline-space-toggle' for why this exists.
     (agent-repl--tabline-cache-buster))))

(advice-add '+workspace--tabline :override #'agent-repl--tabline-advice)

;; --- Visible tab-bar installation -----------------------------------------
;;
;; The functions below are what `tab-bar-format' actually invokes to produce
;; the visible tab-bar.  They live here (next to `agent-repl--tabline-*'
;; entries that they call) rather than in the user-config layer so the
;; package ships with its own working tab-bar, and so the package's
;; workspace-merge reload picks up changes to them.  See the block comment
;; above `agent-repl--tabline-space-toggle' for the alternating-space hack
;; rationale.

(defun agent-repl-workspace-tabline-formatted ()
  "Format workspace list for tab-bar display as EXACTLY TWO rows.
Renders `agent-repl--tabline-row-count' rows, each no wider than
`(1- (frame-width))', via `agent-repl--tabline-rows', which keeps the
current workspace visible and elides overflow behind \"+N\" badges.
The row count is FIXED even when the tabs need only one row (the
second row renders blank): a row-count change alters the tab-bar pixel
height, and on macOS `ns_change_tab_bar_height' resizes the NSWindow —
when that resize is clipped by the screen edge, redisplay retries it
forever and Emacs livelocks at 100% CPU (see
`agent-repl--tabline-rows').  Pinning the row count sidesteps that.

The `(1- (frame-width))' cap also keeps the unfaced terminator that
`agent-repl--join-tabline-rows' appends within the visible columns
\(col < `frame-width'), and each row is centered so left-only padding
from `+doom-dashboard--center' doesn't push it to the right edge.
Appends the zero-width cache-buster
\(`agent-repl--tabline-cache-buster') so the segment's string content
actually changes across refresh ticks without changing its rendered
width.  Without the cache-buster, face-only status transitions
\(e.g. :thinking -> :done) stay invisible until a workspace switch.

Enumerates `agent-repl--ws-tabline-names', so workspaces belonging to
a folded repo are absent from the rendered rows and the
remaining tabs carry contiguous 1-based numbers."
  (let* ((width (frame-width))
         (line-width (max 1 (1- width)))
         (names (agent-repl--ws-tabline-names))
         (entries (agent-repl--tabline-rendered-entries names))
         (current (agent-repl--ws-current-name))
         (cur-pos (and current (cl-position current names :test #'equal)))
         (rows (agent-repl--tabline-rows entries cur-pos line-width
                                          agent-repl--tabline-row-count))
         (centered (mapcar (lambda (row)
                             (if (fboundp '+doom-dashboard--center)
                                 (+doom-dashboard--center line-width row)
                               row))
                           rows))
         (joined (agent-repl--join-tabline-rows centered)))
    (concat joined (agent-repl--tabline-cache-buster))))

(defun agent-repl-current-workspace-name-segment ()
  "Return current workspace name as an invisible tab-bar segment.
Same alternating-space trick as
`agent-repl-workspace-tabline-formatted': the trailing space toggles
each second via `agent-repl--tabline-space-toggle' to force the
right-aligned segment to repaint too.

The segment's actual text is invisible (`'invisible t' text property)
so its only purpose is the cache-busting role."
  (let ((name (or (agent-repl--ws-current-name) "")))
    (propertize (if agent-repl--tabline-space-toggle
                    (concat name " ")
                  name)
                'invisible t)))

(defun agent-repl--ws-queued-segment (ws)
  "Return a status/mode-line segment naming WS's queued-message count.
Empty string when WS has no in-flight-queued messages; otherwise a
subdued \"⋯N queued\" indicator (faced with `agent-repl-queued-messages',
per §2.13's parked-message affordance).  The count comes from
`agent-repl--ws-queued-count', refreshed off the reattach sweep's
GET /sessions poll — this only renders it."
  (let ((n (agent-repl--ws-queued-count ws)))
    (if (> n 0)
        (propertize (format "⋯%d queued" n)
                    'face 'agent-repl-queued-messages
                    'help-echo "agent-repl: messages queued behind the in-flight turn")
      "")))

;; Install the tab-bar after persp-mode loads so `agent-repl--ws-current-name'
;; resolves cleanly at render time; persp-mode is the
;; dep that the workspace-list entries read in
;; `agent-repl--tabline-rendered-entries' and below.
(agent-repl--ws-after-system-load
 (lambda ()
   (setq tab-bar-format '(agent-repl-workspace-tabline-formatted
                          tab-bar-format-align-right
                          agent-repl-current-workspace-name-segment)
         tab-bar-show t
         tab-bar-close-button-show nil)
   ;; Obsolete since 28.1 but still honored; the tab-bar-format migration is deliberate future work.
   (with-suppressed-warnings ((obsolete tab-bar-new-button-show))
     (setq tab-bar-new-button-show nil))
   ;; A tab-bar height change must NEVER imply an NSWindow resize.  On
   ;; macOS the implied resize can be clipped by the screen edge, so the
   ;; requested and realized frame sizes disagree and redisplay retries
   ;; the resize on every cycle — a 100%-CPU livelock
   ;; (`ns_change_tab_bar_height' -> `adjust_frame_size' ->
   ;; `ns_set_window_size').  Absorb height changes into the text area
   ;; instead.  Belt-and-suspenders with the fixed row count in
   ;; `agent-repl-workspace-tabline-formatted'.
   (unless (eq frame-inhibit-implied-resize t)
     (cl-pushnew 'tab-bar-lines frame-inhibit-implied-resize))
   (tab-bar-mode 1)))

;;; Redisplay-storm circuit breaker -------------------------------------------
;;
;; When the tab-bar height oscillation (see the comments in the install
;; block above and `agent-repl-workspace-tabline-formatted') does break
;; through the preventive guards, it livelocks INSIDE `redisplay_internal':
;; the C `retry:' loop (xdisp.c) never returns to the command loop, so
;; timers, process filters, emacsclient, and even SIGUSR2 are all starved
;; and the only recovery is `kill -9'.  Ordinary elisp cannot run during
;; the storm — with one exception: `pre-redisplay-function' is invoked
;; from `prepare_menu_bars', which sits AFTER the `retry:' label
;; (verified against Emacs 30.2 xdisp.c: `retry:' at 16921,
;; `prepare_menu_bars' at 16990), so it executes on EVERY iteration of
;; the livelock.  That is the one vantage point from which a watchdog
;; can observe the storm and break it from inside.
;;
;; Detection pairs two signals that only coincide during the storm:
;;   1. the 1s heartbeat timer has not fired for
;;      `agent-repl--storm-starvation-secs' (the timer wheel is starved),
;;      AND
;;   2. redisplay passes keep happening (the watchdog keeps being called).
;; A long-running elisp command starves timers but does not redisplay;
;; heavy interactive redisplay (smooth scrolling) redisplays but never
;; starves timers.  Only the C retry loop does both.
;;
;; The corrective action is `(setq auto-resize-tab-bars nil)': the entire
;; height-recomputation block in xdisp.c's `redisplay_tab_bar' is gated
;; on that variable, so the next iteration stops requesting a height
;; change, nothing re-garbages the frame, and the loop drains.  This
;; makes a false trip cheap — the tab-bar height merely stays fixed
;; until the cooldown re-enables auto-resizing — while a missed storm
;; costs a hard kill of Emacs.  The breaker re-arms itself
;; (`agent-repl--storm-reenable') up to `agent-repl--storm-max-trips'
;; times per session, then stays tripped.

(defvar agent-repl--storm-starvation-secs 5.0
  "Seconds without a `agent-repl--storm-tick' before timers count as starved.
The tick timer runs every second, so anything beyond a couple of
seconds means the command loop is not being reached.")

(defvar agent-repl--storm-pass-threshold 32
  "Redisplay passes observed while starved before the breaker trips.
Filters out the timer-lag-after-sleep case: waking from suspend can
briefly look starved, but produces only a handful of redisplay passes
before the timer wheel catches up.")

(defvar agent-repl--storm-cooldown-secs 30
  "Seconds after a trip before `auto-resize-tab-bars' is restored.")

(defvar agent-repl--storm-max-trips 3
  "Trips per session after which the breaker stays tripped permanently.")

(defvar agent-repl--storm-last-tick nil
  "`float-time' of the last watchdog heartbeat tick.
nil until `agent-repl--storm-install' runs; the watchdog is inert
until then so load-time redisplay bursts can never trip it.")

(defvar agent-repl--storm-starved-passes 0
  "Count of redisplay passes observed while the timer wheel was starved.
Reset by every heartbeat tick and by a trip.")

(defvar agent-repl--storm-trips 0
  "Number of times the breaker has tripped this session.")

(defvar agent-repl--storm-saved-auto-resize nil
  "Value of `auto-resize-tab-bars' captured at trip time, for restore.")

(defvar agent-repl--storm-reenable-timer nil
  "Pending cooldown timer that will restore `auto-resize-tab-bars'.")

(defvar agent-repl--storm-tick-timer nil
  "The repeating 1s heartbeat timer feeding `agent-repl--storm-last-tick'.")

(defun agent-repl--storm-tick ()
  "Heartbeat: record that the timer wheel is alive and reset the pass count.
Runs every second; its NOT running is the primary storm signal read by
`agent-repl--redisplay-storm-watchdog'."
  (setq agent-repl--storm-last-tick (float-time)
        agent-repl--storm-starved-passes 0))

(defun agent-repl--redisplay-storm-watchdog (_windows)
  "Trip the tab-bar circuit breaker when redisplay storms while timers starve.
Installed `:after' `pre-redisplay-function', so this runs once per
redisplay pass — including every iteration of the C `retry:' livelock
loop, which is the only elisp execution point that survives the storm
(see the section comment above).  Inert until the first heartbeat tick
and while `auto-resize-tab-bars' is already nil (either the breaker has
tripped or the user disabled auto-resizing themselves — in both cases
the oscillation cannot occur, so there is nothing to break)."
  (when (and auto-resize-tab-bars
             agent-repl--storm-last-tick
             (> (- (float-time) agent-repl--storm-last-tick)
                agent-repl--storm-starvation-secs))
    (setq agent-repl--storm-starved-passes (1+ agent-repl--storm-starved-passes))
    (when (>= agent-repl--storm-starved-passes agent-repl--storm-pass-threshold)
      (agent-repl--storm-trip))))

(defun agent-repl--storm-trip ()
  "Break a redisplay storm: disable tab-bar auto-resizing, schedule re-arm.
Saves the current `auto-resize-tab-bars' for the cooldown restore, then
nils it so xdisp.c's `redisplay_tab_bar' stops requesting height
changes and the retry loop drains.  Re-arms after
`agent-repl--storm-cooldown-secs' unless `agent-repl--storm-max-trips'
is reached, in which case auto-resizing stays off for the session.
Runs INSIDE redisplay, so it only flips variables, schedules a timer,
and logs — no window, frame, or buffer mutation."
  (setq agent-repl--storm-saved-auto-resize auto-resize-tab-bars
        auto-resize-tab-bars nil
        agent-repl--storm-starved-passes 0
        agent-repl--storm-trips (1+ agent-repl--storm-trips))
  (let ((final (>= agent-repl--storm-trips agent-repl--storm-max-trips)))
    (agent-repl--do-log nil
                        "redisplay-storm: BREAKER TRIPPED (%d/%d) — auto-resize-tab-bars disabled%s"
                        (list agent-repl--storm-trips
                              agent-repl--storm-max-trips
                              (if final
                                  " permanently for this session"
                                (format "; re-enabling in %ss"
                                        agent-repl--storm-cooldown-secs))))
    (unless final
      (setq agent-repl--storm-reenable-timer
            (run-with-timer agent-repl--storm-cooldown-secs nil
                            #'agent-repl--storm-reenable)))))

(defun agent-repl--storm-reenable ()
  "Cooldown expiry: restore `auto-resize-tab-bars' and re-arm the watchdog.
That this runs at all proves the trip worked — timers only fire once
the retry loop has drained.  If the oscillation condition still holds,
the storm resumes and the watchdog trips again, up to
`agent-repl--storm-max-trips'."
  (setq auto-resize-tab-bars agent-repl--storm-saved-auto-resize
        agent-repl--storm-reenable-timer nil)
  (agent-repl--do-log nil
                      "redisplay-storm: cooldown over — auto-resize-tab-bars restored to %S"
                      (list auto-resize-tab-bars)))

(defun agent-repl--storm-install ()
  "Install the redisplay-storm watchdog: heartbeat timer + redisplay hook.
Idempotent for hot-reload: cancels any prior heartbeat timer, and
`add-function' replaces an already-present member rather than
duplicating it."
  (when (timerp agent-repl--storm-tick-timer)
    (cancel-timer agent-repl--storm-tick-timer))
  (setq agent-repl--storm-last-tick (float-time)
        agent-repl--storm-starved-passes 0
        agent-repl--storm-tick-timer (run-with-timer 1 1 #'agent-repl--storm-tick))
  (push agent-repl--storm-tick-timer agent-repl--timers)
  (add-function :after pre-redisplay-function
                #'agent-repl--redisplay-storm-watchdog))

(agent-repl--storm-install)

(defun agent-repl-toggle-hide-mode ()
  "Toggle `agent-repl-hide-mode-enabled'.
When toggled ON, `:hidden' workspaces (those closed via `SPC o C')
are persp-killed on the next workspace switch.  When OFF, they remain
in the workspace list and behave like ordinary `:inactive' workspaces.
Forces a tab-bar repaint so cycling-skip semantics update immediately."
  (interactive)
  (setq agent-repl-hide-mode-enabled (not agent-repl-hide-mode-enabled))
  (agent-repl--force-tab-bar-redraw)
  (message "agent-repl hide-mode %s"
           (if agent-repl-hide-mode-enabled "enabled" "disabled")))

;; Suppress the echo area flash when switching workspaces.
;; Doom calls (+workspace/display) after switch/cycle/new/load, which uses
;; (message ...) to show the tabline in the echo area.  Since tabs are
;; already visible at the top, the bottom flash is redundant.
(advice-add '+workspace/display :override #'ignore)

(defun agent-repl--workspace-message-body-advice (message &optional type)
  "Override for `+workspace--message-body' that strips the tabline prefix.

Doom's stock `+workspace--message-body' builds the echo-area string as
`<tabline> | <message>', so every `+workspace-message' / `+workspace-error'
call (e.g. the `Deleted '<ws>' workspace' notification emitted by
`+workspace/kill' inside `agent-repl--nuke-one-workspace's merge-teardown
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
            #'agent-repl--workspace-message-body-advice)

;;; Agent panel visibility ---------------------------------------------------

;; Walk saved window-configuration tree to find agent buffers.
(defun agent-repl--wconf-has-agent-p (wconf)
  "Return non-nil if WCONF (a `window-state-get' tree) shows a workspace's agent.
The agent view is the webview buffer — see
`agent-repl--agent-view-buffer-name-p'.  Excludes input buffers: presence
of only the input panel in a saved config (e.g. from a placeholder layout)
should not count as agent open."
  (when (and wconf (proper-list-p wconf))
    (let ((buf-entry (alist-get 'buffer wconf)))
      (if (and buf-entry
               (agent-repl--agent-view-buffer-name-p (car-safe buf-entry)))
          t
        (cl-some #'agent-repl--wconf-has-agent-p
                 (cl-remove-if-not #'proper-list-p wconf))))))

(defun agent-repl--visible-agent-buffer-p (buf)
  "Return non-nil if BUF is a live, visible agent VIEW buffer.
The view is the webview buffer — see `agent-repl--agent-view-buffer-p'."
  (and (buffer-live-p buf)
       (agent-repl--agent-view-buffer-p buf)
       (get-buffer-window buf)))

(defun agent-repl--agent-visible-in-current-ws-p ()
  "Return non-nil if an agent buffer is visible in the current workspace."
  (cl-some #'agent-repl--visible-agent-buffer-p
           (buffer-list)))

(defun agent-repl--agent-in-saved-wconf-p (ws-name)
  "Return non-nil if background workspace WS-NAME has an agent buffer in
its saved config."
  (let* ((persp (agent-repl--ws-resolve-persp ws-name))
         (wconf (agent-repl--ws-window-conf persp)))
    (agent-repl--wconf-has-agent-p wconf)))

(defun agent-repl--ws-agent-open-p (ws-name)
  "Return non-nil if workspace WS-NAME has an agent buffer in its window layout.
For the current workspace, checks live windows.
For background workspaces, inspects the saved persp window configuration."
  (if (equal ws-name (agent-repl--ws-current-name))
      (agent-repl--agent-visible-in-current-ws-p)
    (agent-repl--agent-in-saved-wconf-p ws-name)))

;;; State machine ------------------------------------------------------------

(defun agent-repl--update-ws-state (ws)
  "Decay WS's agent-state from :done to :idle when conditions are met.

This is the sole transition the timer drives on the agent-state axis.
Every other transition is sentinel-owned (see the hook handlers in
`sentinel.el').  When the agent finishes a turn the Stop hook writes
`:done'; if the worktree is clean AND the user has been focused on
the workspace for at least `agent-repl-done-idle-delay' seconds
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
  anything else                           → unchanged
                                            (sentinel-owned or terminal)"
  (let* ((state (agent-repl--ws-agent-state ws))
         (acked (agent-repl--ws-get ws :done-acked))
         (acked-at (agent-repl--ws-get ws :done-acked-at))
         (git-status (agent-repl--ws-get ws :git-clean))
         (dwell (and acked-at (- (float-time) acked-at)))
         (dwell-elapsed (and dwell (> dwell agent-repl-done-idle-delay))))
    (cond
     ((null git-status)
      (agent-repl--log-verbose ws "update-ws-state ws=%s state=%s git-clean not yet populated, skipping" ws state))
     ((and (eq state :done) (eq git-status 'clean) acked dwell-elapsed)
      (agent-repl--log ws "update-ws-state ws=%s :done->:idle (clean, acked, dwell=%.2fs)" ws dwell)
      (agent-repl--ws-set-agent-state ws :idle)
      (agent-repl--ws-put ws :done-acked nil)
      (agent-repl--ws-put ws :done-acked-at nil))
     (t
      (agent-repl--log-verbose ws "update-ws-state ws=%s state=%s acked=%s dwell=%s git-status=%s no-op"
                                ws state acked dwell git-status)))))

(defvar agent-repl--update-tick-counter 0
  "Monotonic tick counter for the workspace-state update timer.
Incremented at the top of every `agent-repl--update-all-workspace-states-now'
pass.  Read by the inner per-workspace step to gate git work via
`(zerop (mod counter agent-repl-state-git-tick-modulus))'.")

(defvar agent-repl--update-in-flight nil
  "Float-time of the most recent chain start, or nil when no chain is in flight.
Set by `agent-repl--update-all-workspace-states-now' at chain
kickoff and cleared by the terminal finalize step.  Read by
`agent-repl--update-in-flight-p' so the periodic timer entrypoint
can skip its tick when a previous chain has not finished.  Carries a
timestamp rather than a plain `t' so stale flags from an errored
chain (one that escaped the per-step `condition-case' and never
finalized) can be detected and force-cleared via
`agent-repl-state-stale-threshold'.")

(defvar agent-repl--update-spread-sync nil
  "When non-nil, the chain processes all workspaces synchronously.
Test-only affordance: production code never sets this.  Tests bind it
to `t' so multi-workspace dispatch assertions can read state
immediately after the call, without having to advance time to let
`run-at-time'-scheduled steps fire.")

(defun agent-repl--update-in-flight-p ()
  "Return non-nil when an update chain is in flight and not stale.
A non-nil `agent-repl--update-in-flight' set within the last
`agent-repl-state-stale-threshold' seconds means a chain is still
running and a new tick should skip.  An older stamp is treated as a
wedged chain (the per-step `condition-case' didn't catch some error
path or the finalize never ran), force-cleared in place, and the
caller is told to proceed."
  (cond
   ((null agent-repl--update-in-flight) nil)
   ((< (- (float-time) agent-repl--update-in-flight)
       agent-repl-state-stale-threshold)
    t)
   (t
    (agent-repl--log nil "update-in-flight-p: stale flag (%.2fs old), force-clearing"
                      (- (float-time) agent-repl--update-in-flight))
    (setq agent-repl--update-in-flight nil)
    nil)))

(defun agent-repl--update-one-workspace-state (ws do-git-p)
  "Run the per-workspace state-update body for WS.
The cheap parts (`agent-repl--agent-running-p',
`agent-repl--update-ws-state', `agent-repl--mark-dead') run every tick
so transitions like `:done' -> `:idle' stay snappy.  DO-GIT-P gates the
expensive git refreshes (`agent-repl--async-refresh-git-status' and
`agent-repl--async-refresh-branch-merged') so they fire only on the
mod-N tick selected by `agent-repl-state-git-tick-modulus'.

A gui-frontend workspace always takes the alive branch here,
regardless of what `agent-repl--agent-running-p' would separately
answer: for a gui workspace that predicate is a CHEAP check (a daemon
session binding exists — see `agent-repl--gui-running-p') rather than
a live daemon health probe, and it can be transiently nil for reasons
that are not a death (e.g. mid-reattach after a daemon restart).
Liveness/death for a gui workspace is owned exclusively by the daemon
\(`session_dead_*' sentinels — see `agent-repl--on-session-dead-event'),
so this poll deliberately never marks a gui workspace dead; it only
runs the frontend-agnostic decay + git refresh for it."
  (if (or (agent-repl--ws-gui-frontend-p ws)
          (agent-repl--agent-running-p ws))
      (progn
        (agent-repl--update-ws-state ws)
        (when do-git-p
          (agent-repl--async-refresh-git-status ws)))
    ;; No live agent session → clear non-thinking state.
    (agent-repl--mark-dead ws))
  ;; Merged-ness is independent of agent liveness — refresh for every
  ;; workspace so `agent-repl--ws-merged-p' always reads a
  ;; fresh `:branch-merged' value.  Gated on DO-GIT-P because the
  ;; refresh's preconditions and process spawn are comparable in cost
  ;; to the diff refresh above.
  (when (and do-git-p
             (fboundp 'agent-repl--async-refresh-branch-merged))
    (agent-repl--async-refresh-branch-merged ws)))

(defun agent-repl--update-all-workspace-states--step (remaining do-git-p gap)
  "Process the head of REMAINING; schedule self for the rest.
DO-GIT-P is the precomputed mod-N gate for the whole pass (snapshotted
at chain kickoff so every workspace in this pass sees the same value).
GAP is the inter-step delay in seconds.

Per-step `gethash' recheck against `agent-repl--workspaces' covers
the snapshot-vs-live divergence: a workspace can be removed mid-chain
\(`--ws-del' from a merge, kill, or sweep) and we must not act on
ghost names.  The body itself is wrapped in `condition-case' so an
error in one ws step never wedges the in-flight flag for subsequent
ticks — the chain logs and keeps going.

When `agent-repl--update-spread-sync' is non-nil (tests only),
recurses directly instead of via `run-at-time'."
  (cond
   ((null remaining)
    (agent-repl--update-all-workspace-states--finalize))
   (t
    (let ((ws (car remaining))
          (rest (cdr remaining)))
      (condition-case err
          (when (gethash ws agent-repl--workspaces)
            (agent-repl--update-one-workspace-state ws do-git-p))
        (error
         (agent-repl--log ws "update-all-workspace-states--step: error ws=%s err=%S"
                           ws err)))
      (if rest
          (if agent-repl--update-spread-sync
              (agent-repl--update-all-workspace-states--step rest do-git-p gap)
            (run-at-time gap nil
                         #'agent-repl--update-all-workspace-states--step
                         rest do-git-p gap))
        (agent-repl--update-all-workspace-states--finalize))))))

(defun agent-repl--update-all-workspace-states--finalize ()
  "Terminal step of the workspace-state update chain.
Clears the in-flight flag so the next timer tick can run."
  (setq agent-repl--update-in-flight nil))

(defun agent-repl--update-all-workspace-states-now ()
  "Unguarded entrypoint for the workspace-state update chain.
Snapshots `(hash-table-keys agent-repl--workspaces)' so the chain
iterates a stable list even as the hash mutates mid-pass; per-step
`gethash' recheck (inside `--step') filters out workspaces deleted
during the spread window.

Increments `agent-repl--update-tick-counter' and computes DO-GIT-P
once so every ws in this pass agrees on the mod-N decision.  Sets the
in-flight marker; the terminal finalize step clears it.

Polls the sentinel directory as a file-notify fallback (`--poll-
workspace-notifications').  Does NOT flip the tabline space toggle —
that's the periodic timer's job (`agent-repl--update-all-workspace-
states', the guarded entrypoint), since event-driven callers
\(frame-focus, workspace-switch, show-panels) already trigger a
redisplay through other paths.

For event-driven callers that want to kick a refresh independent of
the 1Hz reentry guard.  Concurrent chains from rapid sync calls are
permitted (rare in practice); each tracks its own snapshot and
finalize, and the last to finalize clears the flag harmlessly.

Polling (`agent-repl--poll-workspace-notifications') is intentionally
NOT called here.  The poll is a file-notify fallback — it has no
purpose on event-driven refreshes (workspace-switch, frame-focus,
show-panels) and its `directory-files' scan on every call is
unnecessary overhead on those paths.  The periodic timer
\(`agent-repl--update-all-workspace-states') is the sole caller of
the poll."
  (setq agent-repl--update-tick-counter (1+ agent-repl--update-tick-counter))
  ;; Filter to live workspaces only — tombstoned entries have no live
  ;; session to probe and would burn git status calls for no UI.
  (let* ((ws-names (agent-repl--live-ws-names))
         (n (length ws-names))
         (do-git-p (zerop (mod agent-repl--update-tick-counter
                               agent-repl-state-git-tick-modulus)))
         (gap (if (and (> n 0) (> agent-repl-state-spread-window 0))
                  (max agent-repl-state-spread-min-gap
                       (/ agent-repl-state-spread-window (float n)))
                agent-repl-state-spread-min-gap)))
    (agent-repl--log-verbose nil "update-all-workspace-states-now: count=%d do-git=%s gap=%.3fs counter=%d"
                              n do-git-p gap agent-repl--update-tick-counter)
    (setq agent-repl--update-in-flight (float-time))
    (agent-repl--update-all-workspace-states--step ws-names do-git-p gap)))

(declare-function agent-repl--sidebar-tick "sidebar" ())

(defun agent-repl--update-all-workspace-states ()
  "Periodic 1Hz timer entrypoint for workspace-state updates.
Always drives `agent-repl--force-tab-bar-redraw' to force a tab-bar
repaint (DO NOT REMOVE — see the block comment above
`agent-repl--tabline-space-toggle').  The redraw happens BEFORE the
in-flight check so the tab-bar keeps animating even when the update
chain is stacking and we skip a tick.

Flipping `agent-repl--tabline-space-toggle' alone is not enough:
the active tab-bar format function
\(`agent-repl-workspace-tabline-formatted') calls
`agent-repl--tabline-rendered-entries' directly and bypasses
`+workspace--tabline', so the `agent-repl--tabline-advice' path is
no longer on the displayed-rendering hot path.  `--force-tab-bar-
redraw' flips the toggle AND drives `tab-bar-tabs-set' /
`tab-bar--update-tab-bar-lines' / `force-mode-line-update' so the
alternating-string cache-bust actually reaches the display.

When a previous chain is still in flight (per
`agent-repl--update-in-flight-p'), skips this tick — the in-flight
chain will catch up.  Stale flags older than
`agent-repl-state-stale-threshold' are force-cleared so a wedged
chain can't permanently disable the timer.

Otherwise delegates to `agent-repl--update-all-workspace-states-now',
which owns the actual per-workspace iteration with the mod-N git
gate and the recursive serial spread.

Event-driven callers (frame-focus, workspace-switch, show-panels)
should call `-now' directly instead of this guarded entrypoint — they
want to kick a fresh refresh and don't compete with the timer for
the in-flight slot."
  ;; Drive the tab-bar redraw on every tick so face-only status
  ;; transitions (:thinking -> :done, etc.) actually reach the display.
  ;; DO NOT REMOVE — see the block comment above
  ;; `agent-repl--tabline-space-toggle'.  Happens before the in-flight
  ;; check so the animation survives long chains.
  (agent-repl--force-tab-bar-redraw)
  ;; Poll here (timer path only) so the file-notify fallback scan runs once
  ;; per second rather than on every event-driven refresh.  See
  ;; `--update-all-workspace-states-now' for why it was moved here.
  (agent-repl--poll-workspace-notifications)
  ;; Sidebar roster push (sidebar.el): rides this timer rather than
  ;; owning one so the whole 1Hz heartbeat lives in one place; its
  ;; signature gate keeps the per-tick cost to in-memory reads + one
  ;; stat.
  (agent-repl--sidebar-tick)
  (unless (agent-repl--update-in-flight-p)
    (agent-repl--update-all-workspace-states-now)))

;; Periodically update all workspace states (catches git changes, etc.)
(push (run-with-timer agent-repl-state-poll-interval agent-repl-state-poll-interval #'agent-repl--update-all-workspace-states)
      agent-repl--timers)

(defun agent-repl--mark-dead (ws)
  "Record that WS's agent session is no longer running.
Sets `:repl-state :dead' and clears `:agent-state'.  This is a
documented lifecycle-cleanup exception to the sentinel-only writer
rule: no hook will ever fire again for a dead session, so Emacs is
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
- `:agent-state' is `:init' — the agent is starting, the daemon
  session may not have reached running state yet, and observing no
  session does not mean dead.  The session-start hook will transition
  away from `:init' shortly; until then the timer leaves things alone.

An already-`:dead' workspace is idempotent for `:repl-state' but NOT
for `:agent-state': a gui send into a dead binding optimistically
marks `:thinking' before the heal, and when the healed session also
dies the death event must still clear it — otherwise the tab spins
`:thinking' forever (observed in the resume-death-loop incident)."
  (cond
   ((or (eq (agent-repl--ws-repl-state ws) :merged)
        (eq (agent-repl--ws-repl-state ws) :merge-failed)
        (eq (agent-repl--ws-agent-state ws) :init))
    nil)
   ((eq (agent-repl--ws-repl-state ws) :dead)
    (when (agent-repl--ws-agent-state ws)
      (agent-repl--log ws "mark-dead: ws=%s already :dead — clearing stale agent-state=%s"
                        ws (agent-repl--ws-agent-state ws))
      (agent-repl--ws-put ws :agent-state nil)
      (force-mode-line-update t)))
   (t
    (agent-repl--log ws "mark-dead: ws=%s agent-state=%s -> :dead"
                      ws (agent-repl--ws-agent-state ws))
    (agent-repl--ws-put ws :repl-state :dead)
    (agent-repl--ws-put ws :agent-state nil)
    (force-mode-line-update t))))

;;; Frame focus handler -------------------------------------------------------

(defun agent-repl--on-frame-focus ()
  "Update all workspace states when Emacs regains focus.
Calls `agent-repl--update-all-workspace-states-now' (the unguarded
entrypoint) rather than the periodic-timer entrypoint: frame focus is
an event-driven signal that the user is back and wants fresh data, so
it should kick a refresh regardless of the in-flight reentry guard."
  (if (frame-focus-state)
      (progn
        (agent-repl--log (agent-repl--ws-current-name) "on-frame-focus: focused")
        (agent-repl--update-all-workspace-states-now))
    (agent-repl--log-verbose (agent-repl--ws-current-name) "on-frame-focus: not focused")))

(add-function :after after-focus-change-function #'agent-repl--on-frame-focus)


