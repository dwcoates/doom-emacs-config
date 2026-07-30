;;; status.el --- workspace status state machine and tab bar rendering -*- lexical-binding: t; -*-

;;; Code:

;;; Priority badge images
;;
;; Each image is a small PNG loaded from the module's images/ directory and
;; scaled to fit the tab-bar line height.  A workspace's `:priority' is
;; whatever the daemon announced for it in `WorkspaceAvailable' (or what
;; the user later set by hand); nothing derives one locally, so a tab
;; showing an image is a tab whose priority the daemon actually knows.

(defcustom agent-repl-priority-levels '("p05" "p1" "p2" "p3")
  "List of recognized priority level strings for workspace badges."
  :type '(repeat string)
  :group 'agent-repl)

(defcustom agent-repl-tab-bracket-format "[%s]"
  "Format string for tab bracket labels.
%s is replaced with the tab index number."
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
the cheap state-machine work (agent-running-p, mark-dead) and the
expensive git work (`agent-repl--async-refresh-branch-merged').
Cheap work runs every tick so transitions like `:done' -> `:idle'
stay snappy.  Git work runs only when `(mod tick-counter N) == 0' so
the per-ws fork load is amortized to one-in-N ticks; the on-disk
reality git observes does not change at 1Hz, so polling that fast is
wasteful.

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

;; There is no `agent-repl-done-idle-delay' any more, and no :done->:idle
;; decay for it to pace.  The decay moved a workspace off the green "ready
;; for review" color once the user had looked at it, which mattered while
;; green and orange were two different claims.  They are not: `:done',
;; `:ready' and `:idle' are ALL green — the route works and the agent is
;; available — so decaying one into another changed the color without
;; changing anything true.  The `:done-acked' / `:done-acked-at'
;; viewed-bookkeeping that drove it went with it.
;;
;; It was already vestigial for the tab: the tab reads the SSM-pushed
;; render state, while the decay mutated only the local `:agent-state'.

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
;; !! `force-mode-line-update' so                                     !!
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
;;                   Only :dead contributes to tab display (blue);
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
  (let ((previous (agent-repl--ws-get ws :agent-state)))
    (agent-repl--log ws "agent-state: ws=%s previous=%s next=%s" ws previous state))
  (agent-repl--ws-put ws :agent-state state)
  (force-mode-line-update t)
  (agent-repl--memory-state-save ws))

(defun agent-repl--ws-set-repl-state (ws state)
  "Set workspace WS's :repl-state to STATE.
STATE is one of:
  nil        — freshly killed / no session
  :active    — panels displayed, session alive
  :inactive  — panels hidden, session alive (plain `SPC o c' close)
  :merged    — workspace's branch has been merged into its source.
               Set by `agent-repl--workspace-merge-do' on success
               (alongside `:merge-completed t').  Takes precedence
               over `:dead' so the 🔀 badge survives the post-merge
               nuke-and-poll cycle that would otherwise mark the
               now-sessionless workspace dead.
  :dead      — agent session gone

There is no viewed-acknowledgment axis any more: `:done', `:ready' and
`:idle' are all READY, so tracking whether the user had looked at a
`:done' only ever changed the color without changing anything true.

Persists the new value to disk via `agent-repl--state-save' when STATE
is `:active' or `:inactive' so panel-visibility survives Emacs
restart.  `:dead' / nil are not
persisted — they reduce to \"no opinion\" at restart, so default
open-panels behavior applies.  `:dead' is set via `--ws-put' directly
(in `--mark-dead'), bypassing this setter, so no special-case is
needed there."
  (unless ws (error "agent-repl--ws-set-repl-state: ws is nil"))
  (let ((previous (agent-repl--ws-get ws :repl-state)))
    (agent-repl--log ws "repl-state: ws=%s previous=%s next=%s persists=%s"
                      ws previous state (memq state '(:active :inactive))))
  (agent-repl--ws-put ws :repl-state state)
  (force-mode-line-update t)
  (when (memq state '(:active :inactive))
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

;; --- Stop / SubagentStop coordination: DELETED (agent-shim cutover) ---
;;
;; The `:stop-received' / `:pending-subagents' hook-counter block and
;; `agent-repl--fully-stopped-p' were deleted in the agent-shim cutover
;; (design §10).  The turn-finished (`:thinking → :done') resolution and
;; the subagent-in-flight accounting are now owned by the daemon's SSM,
;; which resolves THE render-state (RENDER_STATE_DONE / IDLE / etc.) and
;; pushes it as a `frontend.v1' WorkspaceState frame — Emacs no longer
;; counts SubagentStart/SubagentStop hooks or gates on them.  The Stop /
;; SubagentStart / SubagentStop / StopFailure managed hooks that fed this
;; block are removed from `install.el', and their sentinel dispatch
;; handlers are removed from `sentinel.el'.

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
  (cond
   ((not (buffer-live-p buf))
    (agent-repl--log-verbose ws "align-buffer-to-ws-dir: ws=%s skipped dead-buffer=%S" ws buf))
   ((not (agent-repl--ws-get ws :project-dir))
    (agent-repl--log-verbose ws "align-buffer-to-ws-dir: ws=%s skipped missing-project-dir buffer=%s"
                              ws (buffer-name buf)))
   (t
    (let ((dir (agent-repl--ws-get ws :project-dir)))
      (with-current-buffer buf
        (let ((previous default-directory)
              (resolved (file-name-as-directory dir)))
          (setq default-directory resolved)
          (agent-repl--log ws
                            "align-buffer-to-ws-dir: ws=%s buffer=%s previous=%s next=%s"
                            ws (buffer-name buf) previous resolved)))))))

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
  "BLUE: no live backend session, and SOMETHING IS WRONG.
A workspace\='s color is CONNECTION TRUTH: blue is every way green\='s
promise cannot be kept AND there is evidence of a breakage — no session
yet, the shim dead or unspawned, bring-up in progress, a bring-up that
failed or a driver that died on a terminal protocol error, a store
outage, or a backfill that failed.

It is deliberately ONE color for all of them.  The distinctions matter
to whoever debugs it, not to the user reading a tab: every one of them
means the same thing to them, which is that this workspace cannot be
relied on right now.  The sidebar carries the distinction where it is
worth having.

WHAT BLUE NO LONGER COVERS is the benign half it used to.  A single
`:dormant\=' state meant both \"we put this session to sleep on purpose to
reclaim its ~500MB\" and \"the backend substrate is broken\", so the most
ordinary event in the system — the idle sweeper reaping a workspace
nobody had touched for an hour — painted a tab exactly like a dead shim
did.  A color that fires on both means neither, and a user who watches
every workspace go blue after an ordinary daemon bounce learns to ignore
blue.  `agent-repl--color-hibernated-teal\=' took that half, and blue
finally means something is actually wrong.")

(defconst agent-repl--color-hibernated-teal  "#0d9488"
  "TEAL: no live backend session, and NOTHING IS WRONG.
The session was deliberately put to sleep to reclaim its memory, or
nothing has ever been wired to this workspace.  No bring-up failed and
no driver died — there is simply nobody home, on purpose.

It is NOT green, and its precedence is the blue band\='s rather than
green\='s: a teal workspace cannot be interacted with until a bring-up is
paid for, which is exactly the claim green exists to deny.  Only the
REASON is benign.  Ranked below green instead, a stale `:thinking\=' row
from the turn a workspace was hibernated after would mask a workspace
that is genuinely asleep.

Deliberately far from `agent-repl--color-init-blue\=' rather than a shade
of it: the two states were ONE before the split, and a teal that reads as
\"bluish\" would re-merge them in the only place it matters, which is a
glance at the tab bar.")

(defconst agent-repl--color-thinking-red     "#cc3333"
  "RED: a turn is in flight.
A failed interrupt is NOT a state here.  A stop that did not land means
the turn is still running, so the workspace stays red and the failure
surfaces in the feed — the old `:stop-failed' magenta said \"stopped\"
about a session that was still working.")

(defconst agent-repl--color-done-green       "#1a7a1a"
  "GREEN: ready.
The session is wired, the route is proven usable WITHOUT requiring a
first message, and the backfill has settled.
Covers `:ready', `:idle', `:done', and `:permission' alike — a pending
permission means the agent is ready for the user to view the response
and answer it.")

(defconst agent-repl--color-idle-async-yellow "#f59e0b"
  "YELLOW: no foreground turn, but live detached work.
The one state between \"a turn is running\" and \"nothing is running\".
Shares its value with the webapp's `--async' so the async bubble border
and this tab are literally the same color rather than two that nearly
match.")

(defconst agent-repl--color-vendor-blocked-purple "#a21caf"
  "PURPLE: blocked on the VENDOR or the ACCOUNT.
Auth needed, usage limit reached, a persistent 4xx/5xx, or any other
abnormal turn-CONCLUDING error — a user-set max-turns/max-budget stop, a
model refusal, an execution error.  Purple is a REPORT of how the last
turn ended, the counterpart to green's \"it ended normally\", not a gate
on anything future.  Prompting stays available throughout, and the next
thing the agent does replaces the purple outright: an in-flight retry is
red, and a retry that hits the same wall is purple again.

A magenta-leaning purple, deliberately clear of any violet a merge or a
retry wears: those are the system working, and confusing one with a
session that has stopped is the misread this color exists to prevent.")

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

;; There are no bracket-label glyphs.  The [N] bracket carries its number
;; and the state's COLOR, nothing else: a glyph beside the numeral was a
;; second vocabulary saying what the color already says, and the sidebar
;; is where a state's DETAIL belongs.

(defconst agent-repl--tab-weight             'bold
  "Font weight applied to every tab face.")

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

;; --- The six-color assignment --- ;;

(defconst agent-repl--state-color
  '((:init           . "blue")
    (:severed        . "blue")
    (:dead           . "blue")
    (:degraded       . "blue")
    (:start-failed   . "blue")
    ;; TEAL, alone.  It is the benign half of the state `:dormant' used to be,
    ;; and the whole reason it is not blue is that blue was firing on both an
    ;; intentional teardown and a broken substrate.
    (:hibernated     . "teal")
    (:vendor-blocked . "purple")
    (:thinking       . "red")
    (:clearing       . "red")
    (:compacting     . "red")
    (:idle-async     . "yellow")
    (:idle           . "green")
    (:ready          . "green")
    (:done           . "green")
    (:interrupted    . "green")
    (:permission     . "green")
    (:merging        . "none")
    (:merge-queued   . "none")
    (:merge-conflict . "none")
    (:merge-failed   . "none")
    (:merged         . "none"))
  "Which of the six colors each render state takes, BY NAME.

This is Emacs\='s corner of the cross-language contract in
proto/vocab/render-colors.json.  Go, TypeScript and this table each
assert against that one file, which is the only mechanism that makes a
divergence between the three fail loudly rather than quietly — sidebar.el
has claimed in a COMMENT that its wire table and the webapp\='s union are
one contract, and until now nothing checked it.

It names the color rather than its value: each renderer keeps its own
hex, since a tab-bar background and a CSS dot legitimately want different
shades of one idea.  What may never differ is the ASSIGNMENT.

\"none\" is a real answer.  The merge states take none of the six:
the sidebar reports them, and the tab-bar does not.")

(defconst agent-repl--color-by-name
  `(("blue"   . ,agent-repl--color-init-blue)
    ("teal"   . ,agent-repl--color-hibernated-teal)
    ("purple" . ,agent-repl--color-vendor-blocked-purple)
    ("red"    . ,agent-repl--color-thinking-red)
    ("yellow" . ,agent-repl--color-idle-async-yellow)
    ("green"  . ,agent-repl--color-done-green))
  "Map each of the six color NAMES to the constant this renderer draws it with.

The indirection is what lets `agent-repl--state-color\=' speak the shared
vocabulary while the palette keeps painting with Emacs\='s own values.")

(defconst agent-repl--color-precedence
  '("blue" "teal" "purple" "red" "yellow" "green")
  "The six-color precedence, strongest claim first.

Each color is a strictly stronger claim about what the user CANNOT do
than the one beneath it.  The SSM\='s SQL `prec\=' ranks are the sole
authority; this restates that order for the cross-language assertion and
may never reorder it.

TEAL SITS BETWEEN BLUE AND PURPLE, which is the SSM\='s `hibernated\=' at
rank 15 — directly below the blue band (severed 12, starting 14) and
above purple\='s 20.  Emphatically NOT below green: hibernation makes the
same actionability claim blue does, and only the reason is benign.")

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
    ;; SEVERED borrows init's blue: the claim about what the user can do is
    ;; identical — this workspace has no live session and something on our side
    ;; broke — and only the word and the glyph distinguish "coming up" from
    ;; "the substrate is gone".
    (:severed
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
    ;; HIBERNATED takes a color of its OWN, which is the one place in this
    ;; palette where a borrowed shade was not enough.  Every other borrow above
    ;; shares a hue because the two states share a claim; these two shared a
    ;; claim about ACTIONABILITY and disagreed completely about FAULT, and
    ;; painting them alike is what made blue unreadable.
    (:hibernated
     :face       agent-repl-tab-hibernated
     :unselected (:bg ,agent-repl--color-hibernated-teal
                  :fg ,agent-repl--color-light
                  :bracket-fg ,agent-repl--color-default-bracket
                  :weight ,agent-repl--tab-weight)
     :selected   (:bg ,agent-repl--color-selected-bg
                  :fg ,agent-repl--color-dark
                  :bracket-bg ,agent-repl--color-hibernated-teal
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
    ;; The two context cuts borrow thinking's red rather than taking a shade
    ;; of their own: they make the SAME claim about what the user cannot do,
    ;; and only the phase word in the footer distinguishes them.
    (:clearing
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
    (:compacting
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
    ;; INTERRUPTED takes done's green, and had NO palette row at all until
    ;; now: the state resolved, the shared color table assigned it green, and
    ;; the tab bar fell through to `agent-repl--tab-default' and painted it
    ;; uncolored.  An assignment with no row is an assignment nothing honors.
    (:interrupted
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
     :face       agent-repl-tab-ready
     :unselected (:bg ,agent-repl--color-done-green
                  :fg ,agent-repl--color-dark
                  :bracket-fg ,agent-repl--color-default-bracket
                  :weight ,agent-repl--tab-weight)
     :selected   (:bg ,agent-repl--color-selected-bg
                  :fg ,agent-repl--color-dark
                  :bracket-bg ,agent-repl--color-done-green
                  :bracket-fg ,agent-repl--color-light
                  :weight ,agent-repl--tab-weight))
    (:ready
     :face       agent-repl-tab-ready
     :unselected (:bg ,agent-repl--color-done-green
                  :fg ,agent-repl--color-dark
                  :bracket-fg ,agent-repl--color-default-bracket
                  :weight ,agent-repl--tab-weight)
     :selected   (:bg ,agent-repl--color-selected-bg
                  :fg ,agent-repl--color-dark
                  :bracket-bg ,agent-repl--color-done-green
                  :bracket-fg ,agent-repl--color-light
                  :weight ,agent-repl--tab-weight))
    (:idle-async
     :face       agent-repl-tab-idle-async
     :unselected (:bg ,agent-repl--color-idle-async-yellow
                  :fg ,agent-repl--color-dark
                  :bracket-fg ,agent-repl--color-default-bracket
                  :weight ,agent-repl--tab-weight)
     :selected   (:bg ,agent-repl--color-selected-bg
                  :fg ,agent-repl--color-dark
                  :bracket-bg ,agent-repl--color-idle-async-yellow
                  :bracket-fg ,agent-repl--color-light
                  :weight ,agent-repl--tab-weight))
    (:vendor-blocked
     :face       agent-repl-tab-vendor-blocked
     :unselected (:bg ,agent-repl--color-vendor-blocked-purple
                  :fg ,agent-repl--color-light
                  :bracket-fg ,agent-repl--color-default-bracket
                  :weight ,agent-repl--tab-weight)
     :selected   (:bg ,agent-repl--color-selected-bg
                  :fg ,agent-repl--color-dark
                  :bracket-bg ,agent-repl--color-vendor-blocked-purple
                  :bracket-fg ,agent-repl--color-light
                  :weight ,agent-repl--tab-weight))
    ;; `:start-failed', `:dead' and `:degraded' are BLUE, not colors of
    ;; their own: a shim that never came up, one that has gone away, and a
    ;; store outage are the same compromised route.  Which way the route is
    ;; broken is the sidebar's to report, not the tab's.
    (:start-failed
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
    (:dead
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
    (:degraded
     :face       agent-repl-tab-init
     :unselected (:bg ,agent-repl--color-init-blue
                  :fg ,agent-repl--color-light
                  :bracket-fg ,agent-repl--color-default-bracket
                  :weight ,agent-repl--tab-weight)
     :selected   (:bg ,agent-repl--color-selected-bg
                  :fg ,agent-repl--color-dark
                  :bracket-bg ,agent-repl--color-init-blue
                  :bracket-fg ,agent-repl--color-light
                  :weight ,agent-repl--tab-weight)))
  "Per-state tab-appearance palette.
Each entry fully describes both selected and unselected looks for a
agent-state keyword via nested `:unselected' and `:selected' plists.
`:repl-state :inactive' does not contribute to color (it is bookkeeping
only).

The merge states have NO entry: they take none of the six colors, and
the tab no longer carries badges, so the merge pipeline says what it has
to say in the sidebar.  `:merged' likewise never reaches the tab-bar at
all (`agent-repl--filter-merged-names').")

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

(defface agent-repl-tab-hibernated
  `((t :background ,agent-repl--color-hibernated-teal
       :foreground ,agent-repl--color-light
       :weight ,agent-repl--tab-weight))
  "Face for workspace tabs put to sleep on purpose (teal + 💤).
Not blue: nothing is wrong here, and blue\='s only job now is to mean
that something is.")

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

(defface agent-repl-tab-ready
  `((t :background ,agent-repl--color-done-green
       :foreground ,agent-repl--color-dark
       :weight ,agent-repl--tab-weight))
  "Face for workspace tabs whose agent is ready (green): came up and was
never prompted, or went quiet after a clean conclusion.")

(defface agent-repl-tab-idle-async
  `((t :background ,agent-repl--color-idle-async-yellow
       :foreground ,agent-repl--color-dark
       :weight ,agent-repl--tab-weight))
  "Face for workspace tabs with no foreground turn but live detached
background work (yellow).")

(defface agent-repl-tab-vendor-blocked
  `((t :background ,agent-repl--color-vendor-blocked-purple
       :foreground ,agent-repl--color-light
       :weight ,agent-repl--tab-weight))
  "Face for workspace tabs blocked on the vendor or the account
\(purple + ⛔): auth, usage limit, a persistent API failure, or an
abnormal turn conclusion.")

(defun agent-repl--force-tab-bar-redraw ()
  "Force the tab-bar to repaint NOW, bypassing its string-equality cache.
Tab-bar rendering caches by string equality, and `equal' on propertized
strings ignores text properties — so a change that only differs in face
\(e.g. a state color going from red to green\) won't trigger a repaint via
`force-mode-line-update' alone.  This helper flips the load-bearing
`agent-repl--tabline-space-toggle' so the next tabline render appends
a different cache-buster suffix (`agent-repl--tabline-cache-buster')
and produces a different string, then drives the tab-bar update
primitive that invalidates the tab data plus the ordinary mode-line
redisplay path.  It deliberately does NOT call
`tab-bar--update-tab-bar-lines': Emacs 30.2 defines that private
recalculation as a one-line policy when `tab-bar-show' is t, so calling
it would destroy agent-repl's fixed two-line frame parameter and its
future-frame default.  See the block comment above the toggle's defvar
for the cache-buster rationale."
  (let* ((frame (selected-frame))
         (prior-toggle agent-repl--tabline-space-toggle)
         (tabs-set-available (fboundp 'tab-bar-tabs-set)))
    (setq agent-repl--tabline-space-toggle
          (not agent-repl--tabline-space-toggle))
    (when tabs-set-available
      (tab-bar-tabs-set (tab-bar-tabs)))
    (force-mode-line-update t)
    ;; This runs on the 1Hz status timer.  Record changed redraw
    ;; prerequisites, plus one sample per second during a bounded capture,
    ;; rather than writing an unconditional heartbeat.
    (let ((signature
           (list (frame-parameter frame 'tab-bar-lines)
                 (frame-parameter frame 'tab-bar-lines-keep-state)
                 tab-bar-mode tab-bar-show auto-resize-tab-bars
                 tab-bar-auto-width tab-bar-format tabs-set-available)))
      (when (agent-repl--tabbar-observation-due-p
             frame :redraw-signature :redraw-at signature)
        (agent-repl--log-verbose
         (let ((current (agent-repl--ws-current-name)))
           (and current (agent-repl--ws-known-p current) current))
         "tabbar-redraw: frame=%S prior-toggle=%S toggle=%S tabs-set-available=%S tab-bar-lines=%S keep-state=%S tab-bar-mode=%S tab-bar-show=%S auto-resize=%S auto-width=%S format=%S"
         frame prior-toggle agent-repl--tabline-space-toggle
         tabs-set-available (frame-parameter frame 'tab-bar-lines)
         (frame-parameter frame 'tab-bar-lines-keep-state)
         tab-bar-mode tab-bar-show auto-resize-tab-bars tab-bar-auto-width
         tab-bar-format)))))

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
suppresses full-tab coloring (the state-colored name region) for
workspaces whose panels the user has dismissed.
`:agent-state' is preserved on the plist so the original color
reappears the next time the user reopens panels.  The nil-state
shortcut avoids calling `agent-repl--ws-agent-open-p' on
workspaces that have no state to suppress in the first place.

UI-boundary tolerance: the tab-bar iterates `persp-names-cache',
which can briefly contain names the workspace hash doesn't yet know
about (a mid-creation persp, the `none' sentinel persp).
`--ws-render-status' would signal `user-error' for those; here we
short-circuit to nil so rendering proceeds without color.
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
keeps the state's color.  The bracket label is the tab's 1-based INDEX
and nothing else: state reaches the bracket as COLOR, so a workspace
whose panels are closed still reads its state from the bracket's
color without any glyph beside the numeral."
  ;; Called on every tab-bar redisplay, potentially many times per second;
  ;; renderer branch traces would overwhelm even verbose diagnostics.
  (let* ((selected      (equal current-name name))
         (display-state (agent-repl--ws-display-state name))
         (bracket-state (and (null display-state)
                             (agent-repl--ws-bracket-state name)))
         (spec          (if bracket-state
                            (agent-repl--tab-spec-bracket-only
                             bracket-state selected)
                          (agent-repl--tab-spec display-state selected)))
         (label         (number-to-string index))
         (face          (agent-repl--tab-face display-state selected))
         (img-str       (agent-repl--tab-priority-image-str name)))
    (agent-repl--render-tab name spec label face img-str)))

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
the row count sidesteps that entirely.

Two rows, always exactly two.  `agent-repl--tabline-rows' returns this
many strings whatever the workspace count, and
`agent-repl-workspace-tabline-formatted' blank-pads a row the entries
do not fill to the full line width, so the rendered segment is always
two full-width lines and the tab-bar's pixel height never varies.

The height contract is carried by `tab-bar-lines', which
`agent-repl--install-fixed-height-tab-bar' pins to this value on every
current graphical frame and in `default-frame-alist'.  The installer
first adds `tab-bar-lines' to `frame-inhibit-implied-resize', so changing
a live frame consumes text-area height instead of requesting an outer
NSWindow resize.")

(defun agent-repl--pack-prefix (widths caps)
  "Greedily first-fit as long a PREFIX of WIDTHS as fits rows sized by CAPS.
WIDTHS is a list of entry column-widths in display order.  CAPS is a
list of each row's maximum column budget; its length is the row count.
Entries are placed left to right: each is appended to the current row
when it (plus a one-column separator after the first entry already on
that row) still fits that row's CAPS budget, otherwise the next row is
started.  Placement stops at the first entry that fits no remaining
row.  Returns a list of per-row entry COUNTS (same length as CAPS)
whose sum is the length of the placed prefix — which may be shorter
than WIDTHS, and may be zero when even the first entry fits nowhere."
  (let* ((nrows (length caps))
         (counts (make-list nrows 0))
         (row 0)
         (used 0)
         (rest widths)
         (done nil))
    (while (and (not done) rest)
      (let* ((w (car rest))
             (sep (if (> (nth row counts) 0) 1 0)))
        (cond
         ((<= (+ used sep w) (nth row caps))
          (setf (nth row counts) (1+ (nth row counts)))
          (setq used (+ used sep w)
                rest (cdr rest)))
         ((< row (1- nrows))
          (setq row (1+ row) used 0))
         (t (setq done t)))))
    counts))

(defun agent-repl--pack-first-fit (widths caps)
  "Greedily first-fit WIDTHS into rows sized by CAPS.
Returns a list of per-row entry COUNTS (same length as CAPS) when
EVERY entry is placed, or nil when the entries do not all fit in
`(length CAPS)' rows.  The placement itself is
`agent-repl--pack-prefix'; this is the all-or-nothing wrapper the
no-elision fit decision uses."
  (let ((counts (agent-repl--pack-prefix widths caps)))
    (and (= (apply #'+ counts) (length widths)) counts)))

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
        (setq row (agent-repl--tabline-truncate-row row width))
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

(defvar agent-repl--tabline-last-truncation nil
  "Last tab-line truncation signature written to the canonical log.

The renderer can run many times per second.  Remembering the most recent
overflow shape lets `agent-repl--tabline-truncate-row' record a changed
pathological row once without writing the same diagnostic on every
redisplay.")

(defun agent-repl--tabline-truncate-row (row width)
  "Return ROW truncated to at most WIDTH rendered columns.

Uses `agent-repl--tabline-entry-width' for every candidate prefix, so a
`display' image and a wide glyph consume their real pixel-derived column
width rather than their character count.  This is the final physical
overflow guard after row packing.  It is especially important for the
degenerate branch that deliberately keeps one anchor entry even when
the entry is wider than every row budget.

Truncation removes complete source characters from the right until the
rendered prefix fits.  The function logs only when the overflow signature
changes because tab-bar redisplay is an extremely hot path."
  (let ((original-width (agent-repl--tabline-entry-width row)))
    (if (<= original-width width)
        row
      (let ((end (length row)))
        (while (and (> end 0)
                    (> (agent-repl--tabline-entry-width
                        (substring row 0 end))
                       width))
          (setq end (1- end)))
        (let* ((result (substring row 0 end))
               (result-width (agent-repl--tabline-entry-width result))
               (signature
                (list width original-width result-width
                      (substring-no-properties row)
                      (substring-no-properties result))))
          (unless (equal signature agent-repl--tabline-last-truncation)
            (setq agent-repl--tabline-last-truncation signature)
            (agent-repl--log-verbose
             (let ((current (agent-repl--ws-current-name)))
               (and current (agent-repl--ws-known-p current) current))
             "tabline-truncate-row: budget=%d original-columns=%d original-chars=%d original=%S result-columns=%d result-chars=%d result=%S"
             width original-width (length row) (substring-no-properties row)
             result-width (length result) (substring-no-properties result)))
          result)))))

(defun agent-repl--tabline-window-size (widths caps start)
  "Return how many consecutive WIDTHS from START fit rows sized by CAPS.
Never returns less than 1: a window always shows its leading entry,
even one too wide for any row's budget (the render guard truncates it)."
  (max 1 (apply #'+ (agent-repl--pack-prefix (nthcdr start widths) caps))))

(defvar agent-repl--tabline-view-states
  (make-hash-table :test #'eq :weakness 'key)
  "Weak hash table mapping frames to their tab-bar view-state plists.

Each value carries `:anchor', `:width', and `:names'.  The state says
where that FRAME's rendered workspace window starts.  Frame ownership is
essential because frames can have different widths and can redisplay in
alternation; a single global anchor lets one frame continually rewrite
another frame's view.

The table is deliberately outside `agent-repl--workspaces': tab-bar
position is frame view state rather than workspace lifecycle state.
Weak keys ensure deleting a frame also makes its cached view collectible.")

(defvar agent-repl--tabbar-observation-states
  (make-hash-table :test #'eq :weakness 'key)
  "Weak hash table mapping frames to tab-bar diagnostic observation state.

Each value stores the last signature and log timestamp independently for
the redraw, formatter, and final keymap boundaries.  Those boundaries run
inside redisplay, so logging every invocation would create an
instrumentation-driven redisplay storm.  State-change logging preserves
the evidence needed to diagnose a rendering transition without multiplying
unchanged records.")

(defvar agent-repl--tabbar-diagnostic-until nil
  "Absolute time until which unchanged tab-bar observations are sampled.

Nil disables periodic sampling.  During a bounded investigation, set this
to a future `float-time'; each instrumented boundary then logs unchanged
state at most once per second.  State changes are always logged regardless
of this value.")

(defun agent-repl--tabbar-observation-due-p
    (frame signature-key time-key signature)
  "Return non-nil when FRAME's SIGNATURE should be logged.

SIGNATURE-KEY and TIME-KEY identify one instrumented boundary in FRAME's
observation plist.  A changed SIGNATURE is always due.  An unchanged
signature is due at most once per second while
`agent-repl--tabbar-diagnostic-until' names a future time.  Records the
accepted signature and timestamp before returning.

This helper is intentionally silent: it is the recursion and rate-limit
boundary for logging performed from redisplay."
  (let* ((now (float-time))
         (state (gethash frame agent-repl--tabbar-observation-states))
         (prior-signature (plist-get state signature-key))
         (prior-time (or (plist-get state time-key) 0.0))
         (capture-active
          (and (numberp agent-repl--tabbar-diagnostic-until)
               (< now agent-repl--tabbar-diagnostic-until)))
         (due (or (not (equal signature prior-signature))
                  (and capture-active (>= (- now prior-time) 1.0)))))
    (when due
      (setq state (plist-put state signature-key signature)
            state (plist-put state time-key now))
      (puthash frame state agent-repl--tabbar-observation-states))
    due))

(defun agent-repl--tabline-surviving-anchor (anchor prev-names names)
  "Return the anchor name to render NAMES from, given the previous ANCHOR.

Implements the membership-change rule: keep ANCHOR when it still
appears in NAMES, otherwise fall back to its nearest surviving
neighbor in PREV-NAMES (the ordering ANCHOR was chosen against).  Ties
at equal distance resolve to the RIGHT neighbor: when the anchor
workspace is killed, the entry that follows it is the one that
naturally slides into the leftmost slot.  With no anchor, no survivor,
and for an empty NAMES, falls back to the first name (or nil)."
  (cond
   ((null names) nil)
   ((null anchor) (car names))
   ((member anchor names) anchor)
   (t
    (let ((idx (cl-position anchor prev-names :test #'equal))
          (prev-n (length prev-names)))
      (or (and idx
               (cl-loop for d from 1 to prev-n
                        for right = (+ idx d)
                        for left = (- idx d)
                        thereis (or (and (< right prev-n)
                                         (let ((c (nth right prev-names)))
                                           (and (member c names) c)))
                                    (and (>= left 0)
                                         (let ((c (nth left prev-names)))
                                           (and (member c names) c))))))
          (car names))))))

(defun agent-repl--tabline-window-anchor (names current anchor prev-names
                                                widths width max-rows)
  "Return the 0-based index in NAMES the tab-bar window should start at.

Pure: computes the anchor position without touching the frame view-state
table (`agent-repl--tabline-anchor-index' is the stateful wrapper).
ANCHOR is the previous anchor name and PREV-NAMES the name list it was
chosen against; CURRENT is the current workspace name; WIDTHS are the
entries' column widths, matching NAMES positionally.

When every entry fits MAX-ROWS full-width rows there is nothing to
elide, so the window is the whole list and the anchor is index 0.
Otherwise exactly three rules move the anchor, in order:

  1. membership — keep the anchor workspace if it survives in NAMES,
     else its nearest surviving neighbor
     \(`agent-repl--tabline-surviving-anchor');
  2. CURRENT left of the window — the anchor becomes CURRENT;
  3. CURRENT beyond the window's end — the anchor advances by the
     SMALLEST number of positions that brings CURRENT back inside.

Nothing else moves it.  In particular a CURRENT already inside the
window moves it not at all, so switching between two visible tabs
renders an identical set of entries in identical places."
  (let ((n (length names)))
    (cond
     ((= n 0) 0)
     ;; No elision needed: the window is everything, anchored at the head.
     ((agent-repl--pack-first-fit widths (make-list max-rows width)) 0)
     (t
      (let* ((badge-w (+ 2 (length (number-to-string n)))) ; "+N " / " +N"
             (caps (agent-repl--tabline-overflow-caps width max-rows badge-w))
             (survivor (agent-repl--tabline-surviving-anchor
                        anchor prev-names names))
             (lo (min (or (cl-position survivor names :test #'equal) 0)
                      (1- n)))
             (cur (cl-position current names :test #'equal)))
        (when cur
          ;; Rule 2: current sits left of the window.
          (when (< cur lo) (setq lo cur))
          ;; Rule 3: current sits past the window's last entry.  Advance
          ;; one position at a time so the move is the smallest one that
          ;; works; LO reaching CUR always terminates the loop, since a
          ;; window shows at least its own leading entry.
          (while (and (< lo cur)
                      (> (1+ cur)
                         (+ lo (agent-repl--tabline-window-size
                                widths caps lo))))
            (setq lo (1+ lo))))
        lo)))))

(defun agent-repl--tabline-anchor-index (frame widths names current width max-rows)
  "Update FRAME's tab-bar anchor state for NAMES and return its 0-based index.

Stateful wrapper over `agent-repl--tabline-window-anchor': applies the
three anchor rules to FRAME's current `:anchor' and `:names', then
records the resulting anchor name, WIDTH, and NAMES back under FRAME.
WIDTHS are the rendered entries' column widths, matching NAMES
positionally; returns the index `agent-repl--tabline-rows' should render
its window from.

This function runs inside redisplay more than once per second, so the
frame-local state write is deliberately not logged."
  (let* ((state (gethash frame agent-repl--tabline-view-states))
         (lo (agent-repl--tabline-window-anchor
              names current
              (plist-get state :anchor)
              (plist-get state :names)
              widths width max-rows)))
    (puthash frame
             (list :anchor (nth lo names)
                   :width width
                   :names names)
             agent-repl--tabline-view-states)
    lo))

(defun agent-repl--tabline-rows (entries anchor-pos width max-rows &optional widths)
  "Pack ENTRIES into EXACTLY MAX-ROWS rows, each no wider than WIDTH.

ENTRIES is a list of rendered tab-entry strings (see
`agent-repl--tabline-rendered-entries').  Returns a list of MAX-ROWS
strings, adjacent entries joined by a single space within a row and no
string ever containing a newline.  Unused trailing rows are the empty
string, so the row COUNT is fixed at MAX-ROWS regardless of how many
entries there are.

When all ENTRIES fit within MAX-ROWS full-width rows they are all
shown with no badges.  Otherwise the rendered window STARTS at
ANCHOR-POS (0-based; nil falls back to 0) and runs as far right as the
rows hold — the window is anchored, never recentered on the current
workspace, so switching between two visible tabs changes nothing about
what renders where.  `agent-repl--tabline-anchor-index' owns the
anchor and its three update rules.

Entries elided on EITHER side of the window are summarized by a
badge: a leading \"+N \" on the first row counts the entries before
ANCHOR-POS, a trailing \" +N\" on the last row counts those past the
window's end.

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
the frame in pixels and wrap to a third physical row.  WIDTHS may
supply that measurement when the caller has already taken it (the
formatter measures once for both the anchor and the rows), since
`string-pixel-width' is far from free inside redisplay."
  (let ((n (length entries)))
    (if (= n 0)
        (make-list max-rows "")
      (let* ((widths (or widths (mapcar #'agent-repl--tabline-entry-width entries)))
             ;; Do all entries fit MAX-ROWS full-width rows?  If so, no
             ;; badges and no windowing are needed.
             (full (agent-repl--pack-first-fit
                    widths (make-list max-rows width))))
        (if full
            (agent-repl--tabline-render-rows entries full "" "" width)
          ;; Overflow: render the window that starts at the anchor,
          ;; with badge columns reserved conservatively on the first
          ;; and last rows for the two elision counts.
          (let* ((lo (min (max (or anchor-pos 0) 0) (1- n)))
                 (badge-w (+ 2 (length (number-to-string n)))) ; "+N " / " +N"
                 (caps (agent-repl--tabline-overflow-caps width max-rows badge-w))
                 (packed (agent-repl--pack-prefix (nthcdr lo widths) caps))
                 (counts (if (> (apply #'+ packed) 0)
                             packed
                           ;; Degenerate: the anchor entry alone is wider
                           ;; than any row's budget; still show it
                           ;; (truncated by the render guard).
                           (cons 1 (make-list (1- max-rows) 0))))
                 (hi (+ lo (max 1 (apply #'+ packed)) -1))
                 (window (seq-subseq entries lo (1+ hi)))
                 (lead (if (> lo 0) (format "+%d " lo) ""))
                 (trail (if (< hi (1- n)) (format " +%d" (- n 1 hi)) "")))
            (agent-repl--tabline-render-rows window counts lead trail width)))))))

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
the frame's visible columns (col < `frame-width').
`agent-repl--center-tabline-row' only left-pads, so a row sized to
`frame-width' would put the appended space at column `frame-width' and
therefore offscreen.  Size and center rows to `(1- (frame-width))' to
leave room for the terminator."
  (if (null lines)
      ""
    (concat (mapconcat #'identity lines " \n") " ")))

(cl-defun agent-repl--tabline-advice (&optional (names nil names-supplied-p))
  "Override for `+workspace--tabline' to color tabs by agent status.

The tab-bar reflects every workspace in NAMES (defaulting to
`agent-repl--ws-tabline-names' — the persp-mode integration wrapper
in `workspace.el', which intersects `persp-names-cache' with
agent-repl's own registration, then drops the workspaces of folded
repos).  Repo folding is the only mechanism that hides a workspace
from the tab-bar; a workspace closed via `SPC o C' simply stays
listed as inactive."
  (let* ((resolved-names (if names-supplied-p names (agent-repl--ws-tabline-names)))
         (entries (agent-repl--tabline-rendered-entries resolved-names))
         (current-name (agent-repl--ws-current-name))
         (states (mapcar (lambda (n)
                           (cons n (agent-repl--ws-display-state n)))
                         resolved-names)))
    (agent-repl--log-verbose nil "tabline-advice: current=%s states=%S"
                              current-name states)
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

(defun agent-repl--pad-tabline-row (row width)
  "Blank-pad an EMPTY ROW out to WIDTH columns of spaces.

Only an empty row is padded.  A row the entries did not fill would
otherwise render as a zero-length line, and the tab-bar's fixed pixel
height depends on every one of its rows actually being a line; padding
gives the unfilled row real columns to occupy.

A row that already has entries is returned untouched, deliberately: its
column width is measured with `agent-repl--tabline-entry-width', which
counts an image-bearing entry by PIXELS, so padding it out to WIDTH
character columns could push it past the frame in pixels and wrap it to
a further physical row — the `ns_change_tab_bar_height' livelock.  The
padding is spaces with no face, so it also cannot extend a tab's
background to the frame edge (see `agent-repl--join-tabline-rows')."
  (if (string-empty-p row)
      (make-string (max 0 width) ?\s)
    row))

(defun agent-repl--center-tabline-row (row width)
  "Left-pad ROW so its rendered content is centered within WIDTH columns.

Measures ROW through `agent-repl--tabline-entry-width', so display images
and wide glyphs affect the padding by their rendered pixel width.  The
function assumes the physical overflow guard has already limited ROW to
WIDTH.  This pure helper runs inside redisplay more than once per second
and therefore deliberately performs no logging."
  (let ((row-width (agent-repl--tabline-entry-width row)))
    (concat (make-string (max 0 (/ (- width row-width) 2)) ?\s)
            row)))

(defun agent-repl--tabbar-log-render
    (frame width line-width names states current widths anchor-pos rows padded
           centered joined output)
  "Log one diagnostic observation of the visible tab-bar render boundary.

FRAME and WIDTH describe the rendering frame.  LINE-WIDTH is the physical
row budget; NAMES, STATES, CURRENT, WIDTHS, and ANCHOR-POS describe window
selection; ROWS, PADDED, CENTERED, JOINED, and OUTPUT capture every
formatter stage.  Text properties are stripped only in the diagnostic
payload so the rendered values themselves remain untouched.

The observation is emitted when its diagnostic signature changes, or at
most once per second during a bounded capture.  This function is the
instrumentation exception for the redisplay hot path: the signature gate
runs before the canonical logger."
  (let* ((plain-rows (mapcar #'substring-no-properties rows))
         (plain-centered (mapcar #'substring-no-properties centered))
         (plain-joined (substring-no-properties joined))
         (plain-output (substring-no-properties output))
         (row-widths (mapcar #'agent-repl--tabline-entry-width rows))
         (padded-widths
          (mapcar #'agent-repl--tabline-entry-width padded))
         (centered-widths
          (mapcar #'agent-repl--tabline-entry-width centered))
         (signature
          (list width line-width names states current widths anchor-pos
                plain-rows row-widths padded-widths plain-centered
                centered-widths plain-joined
                (frame-parameter frame 'tab-bar-lines)
                (frame-parameter frame 'tab-bar-lines-keep-state)
                tab-bar-mode tab-bar-show auto-resize-tab-bars
                tab-bar-auto-width tab-bar-format
                frame-inhibit-implied-resize)))
    (when (agent-repl--tabbar-observation-due-p
           frame :render-signature :render-at signature)
      (agent-repl--log-verbose
       (and current (agent-repl--ws-known-p current) current)
       "tabbar-render: frame=%S frame-width=%d frame-pixel-width=%d frame-char-width=%d line-width=%d configured-rows=%d tab-bar-lines=%S keep-state=%S tab-bar-mode=%S tab-bar-show=%S auto-resize=%S auto-width=%S inhibit-implied-resize=%S format=%S names=%S states=%S current=%S entry-widths=%S anchor-pos=%d rows=%S row-widths=%S padded-widths=%S centered=%S centered-widths=%S joined-newlines=%d output-newlines=%d output-chars=%d output=%S"
       frame width (frame-pixel-width frame) (frame-char-width frame)
       line-width agent-repl--tabline-row-count
       (frame-parameter frame 'tab-bar-lines)
       (frame-parameter frame 'tab-bar-lines-keep-state)
       tab-bar-mode tab-bar-show auto-resize-tab-bars tab-bar-auto-width
       frame-inhibit-implied-resize tab-bar-format names states current widths
       anchor-pos plain-rows row-widths padded-widths plain-centered
       centered-widths (cl-count ?\n plain-joined)
       (cl-count ?\n plain-output) (length output) plain-output))))

(defun agent-repl-workspace-tabline-formatted ()
  "Format workspace list for tab-bar display as a FIXED row count.
Renders `agent-repl--tabline-row-count' rows, each no wider than
`(1- (frame-width))', via `agent-repl--tabline-rows', which renders an
anchored window of workspaces and elides overflow behind \"+N\" badges
on both ends.  The window start is
`agent-repl--tabline-anchor-index' — a stable anchor, not a recentering
on the current workspace, so switching between two visible tabs leaves
the rendered rows identical.

The row count is FIXED even when the tabs need only one row: the
unfilled row is blank-padded to the full line width
\(`agent-repl--pad-tabline-row') so the segment is ALWAYS exactly
`agent-repl--tabline-row-count' full lines.  A row-count change alters
the tab-bar pixel height, and on macOS `ns_change_tab_bar_height'
resizes the NSWindow — when that resize is clipped by the screen edge,
redisplay retries it forever and Emacs livelocks at 100% CPU (see
`agent-repl--tabline-rows').  Pinning the row count sidesteps that.

The `(1- (frame-width))' cap also keeps the unfaced terminator that
`agent-repl--join-tabline-rows' appends within the visible columns
\(col < `frame-width'), and each row is centered by rendered pixel width
through `agent-repl--center-tabline-row'.
Appends the zero-width cache-buster
\(`agent-repl--tabline-cache-buster') so the segment's string content
actually changes across refresh ticks without changing its rendered
width.  Without the cache-buster, face-only status transitions
\(e.g. :thinking -> :done) stay invisible until a workspace switch.

Enumerates `agent-repl--ws-tabline-names', so workspaces belonging to
a folded repo are absent from the rendered rows and the
remaining tabs carry contiguous 1-based numbers."
  ;; The visible formatter runs in redisplay, often more than once per
  ;; second.  Its per-branch values are deliberately not logged.
  (let* ((width (frame-width))
         (line-width (max 1 (1- width)))
         (names (agent-repl--ws-tabline-names))
         (states
          (mapcar (lambda (name)
                    (cons name
                          (and (agent-repl--ws-known-p name)
                               (agent-repl--ws-render-status name))))
                  names))
         (entries (agent-repl--tabline-rendered-entries names))
         (current (agent-repl--ws-current-name))
         ;; Measured once and handed to both the anchor and the rows:
         ;; `string-pixel-width' is expensive and this runs in redisplay.
         (widths (mapcar #'agent-repl--tabline-entry-width entries))
         (anchor-pos (agent-repl--tabline-anchor-index
                      (selected-frame) widths names current line-width
                      agent-repl--tabline-row-count))
         (rows (agent-repl--tabline-rows entries anchor-pos line-width
                                          agent-repl--tabline-row-count widths))
         (padded (mapcar (lambda (row)
                           (agent-repl--pad-tabline-row row line-width))
                         rows))
         (centered
          (mapcar (lambda (row)
                    (agent-repl--center-tabline-row row line-width))
                  padded))
         (joined (agent-repl--join-tabline-rows centered))
         (output (concat joined (agent-repl--tabline-cache-buster))))
    (agent-repl--tabbar-log-render
     (selected-frame) width line-width names states current widths anchor-pos
     rows padded centered joined output)
    output))

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

(defun agent-repl--tabbar-keymap-caption-observations (keymap)
  "Return diagnostic observations for string captions in KEYMAP.

Each observation records the menu-item key, source-character count,
newline count, rendered column width, property-free source caption, and
visible caption with `invisible' characters removed.  This is the last Lisp
boundary before Emacs C code consumes the tab-bar items, so it reveals
transformations such as `tab-bar-auto-width' deleting part of a multi-line
formatter string."
  (cl-loop for item in keymap
           for observation =
           (pcase item
             (`(,key menu-item ,caption . ,_)
              (when (stringp caption)
                (let ((visible-caption
                       (apply
                        #'string
                        (cl-loop for index below (length caption)
                                 unless (get-text-property
                                         index 'invisible caption)
                                 collect (aref caption index)))))
                  (list :key key
                        :chars (length caption)
                        :newlines (cl-count ?\n caption)
                        :columns (agent-repl--tabline-entry-width caption)
                        :caption (substring-no-properties caption)
                        :visible-caption visible-caption))))
             (_ nil))
           when observation
           collect observation))

(defun agent-repl--tabbar-audit-keymap (keymap)
  "Log KEYMAP's final string captions and return KEYMAP unchanged.

Installed as `tab-bar-make-keymap' return advice.  Its state-change and
bounded-capture gate makes the actual Lisp-to-C handoff observable without
logging every redisplay."
  (let* ((frame (selected-frame))
         (captions (agent-repl--tabbar-keymap-caption-observations keymap))
         ;; The cache-buster intentionally alternates an invisible trailing
         ;; character every poll.  Exclude that character from the
         ;; state-change signature while retaining the exact source caption
         ;; in the emitted observation.
         (semantic-captions
          (mapcar
           (lambda (caption)
             (list :key (plist-get caption :key)
                   :visible-caption
                   (plist-get caption :visible-caption)))
           captions))
         (signature
          (list semantic-captions tab-bar-auto-width
                (frame-parameter frame 'tab-bar-lines)
                (frame-parameter frame 'tab-bar-lines-keep-state))))
    (when (agent-repl--tabbar-observation-due-p
           frame :keymap-signature :keymap-at signature)
      (agent-repl--log-verbose
       (let ((current (agent-repl--ws-current-name)))
         (and current (agent-repl--ws-known-p current) current))
       "tabbar-keymap-boundary: frame=%S tab-bar-lines=%S keep-state=%S auto-width=%S captions=%S"
       frame (frame-parameter frame 'tab-bar-lines)
       (frame-parameter frame 'tab-bar-lines-keep-state)
       tab-bar-auto-width captions))
    keymap))

(advice-add 'tab-bar-make-keymap :filter-return
            #'agent-repl--tabbar-audit-keymap)

;; The queued-message status segment (agent-repl--ws-queued-segment) and its
;; face were deleted in the S9 endgame along with the retired queue plane: it
;; had no production caller and its count source (:queued-messages) is gone.

;;; Fixed-height tab-bar installation ----------------------------------------
;;
;; `auto-resize-tab-bars' is unsafe for this formatter on macOS.  A Magit
;; subprocess finishing inside `kill-buffer' can force
;; `redisplay_preserve_echo_area'; if tab-bar redisplay then requests a
;; different height, Emacs 30.2 loops under
;; `ns_change_tab_bar_height' -> `adjust_frame_glyphs', starving every Lisp
;; timer and consuming a CPU core indefinitely.  The old reactive watchdog
;; could not run from that redisplay path, so recovery code was itself
;; starved.
;;
;; The formatter contract is already exactly `agent-repl--tabline-row-count'
;; rows.  Pin both current frames and `default-frame-alist' to that height
;; and disable the C auto-resize path altogether.  There is no useful dynamic
;; height to preserve, so prevention is both simpler and stronger than a
;; post-starvation circuit breaker.

(defvar agent-repl--storm-tick-timer nil
  "Obsolete watchdog timer retained only for hot-reload cleanup.")

(defvar agent-repl--tabbar-frame-parameter-audit-active nil
  "Non-nil while logging a tab-bar frame-parameter mutation.

The guard prevents canonical logging internals from recursively entering the
global frame-parameter advice.  Calls made while it is non-nil retain their
normal behavior but do not emit a nested diagnostic record.")

(defun agent-repl--tabbar-backtrace-string ()
  "Return the current Lisp backtrace as a string.

`backtrace' writes to `standard-output'; capturing that documented output
works on the Emacs 30 build used by agent-repl, which does not provide the
newer convenience function `backtrace-to-string'."
  (with-output-to-string
    (backtrace)))

(defun agent-repl--tabbar-log-frame-lines-mutation
    (api frame prior requested final outcome backtrace)
  "Log one `tab-bar-lines' mutation attempted through API.

FRAME is the resolved live frame, PRIOR its value before the call, REQUESTED
the API input, FINAL the value afterward, and OUTCOME is either `returned' or
an error object.  BACKTRACE is captured before invoking the underlying API so
the record identifies the caller that initiated the mutation."
  (let ((agent-repl--tabbar-frame-parameter-audit-active t))
    (agent-repl--log
     (let ((current (agent-repl--ws-current-name)))
       (and current (agent-repl--ws-known-p current) current))
     "tabbar-lines-mutation: api=%S frame=%S prior=%S requested=%S final=%S outcome=%S backtrace=%S"
     api frame prior requested final outcome backtrace)))

(defun agent-repl--tabbar-audit-set-frame-parameter
    (original frame parameter value)
  "Around advice tracing `tab-bar-lines' changes made through ORIGINAL.

All other frame parameters pass through without instrumentation.  The return
value and any signaled error remain identical to `set-frame-parameter'."
  (if (or agent-repl--tabbar-frame-parameter-audit-active
          (not (eq parameter 'tab-bar-lines)))
      (funcall original frame parameter value)
    (let* ((resolved-frame (or frame (selected-frame)))
           (prior (frame-parameter resolved-frame 'tab-bar-lines))
           (backtrace (agent-repl--tabbar-backtrace-string)))
      (condition-case error-data
          (let* ((agent-repl--tabbar-frame-parameter-audit-active t)
                 (result (funcall original frame parameter value))
                 (final (frame-parameter resolved-frame 'tab-bar-lines)))
            (unless (equal prior final)
              (agent-repl--tabbar-log-frame-lines-mutation
               'set-frame-parameter resolved-frame prior value final
               'returned backtrace))
            result)
        (error
         (agent-repl--tabbar-log-frame-lines-mutation
          'set-frame-parameter resolved-frame prior value
          (frame-parameter resolved-frame 'tab-bar-lines)
          error-data backtrace)
         (signal (car error-data) (cdr error-data)))))))

(defun agent-repl--tabbar-audit-modify-frame-parameters
    (original frame parameters)
  "Around advice tracing `tab-bar-lines' changes in PARAMETERS via ORIGINAL.

Parameter lists without `tab-bar-lines' pass through without instrumentation.
The return value and any signaled error remain identical to
`modify-frame-parameters'."
  (let ((line-cell (assq 'tab-bar-lines parameters)))
    (if (or agent-repl--tabbar-frame-parameter-audit-active
            (null line-cell))
        (funcall original frame parameters)
      (let* ((resolved-frame (or frame (selected-frame)))
             (prior (frame-parameter resolved-frame 'tab-bar-lines))
             (requested (cdr line-cell))
             (backtrace (agent-repl--tabbar-backtrace-string)))
        (condition-case error-data
            (let* ((agent-repl--tabbar-frame-parameter-audit-active t)
                   (result (funcall original frame parameters))
                   (final (frame-parameter resolved-frame 'tab-bar-lines)))
              (unless (equal prior final)
                (agent-repl--tabbar-log-frame-lines-mutation
                 'modify-frame-parameters resolved-frame prior requested
                 final 'returned backtrace))
              result)
          (error
           (agent-repl--tabbar-log-frame-lines-mutation
            'modify-frame-parameters resolved-frame prior requested
            (frame-parameter resolved-frame 'tab-bar-lines)
            error-data backtrace)
           (signal (car error-data) (cdr error-data))))))))

(advice-add 'set-frame-parameter :around
            #'agent-repl--tabbar-audit-set-frame-parameter)
(advice-add 'modify-frame-parameters :around
            #'agent-repl--tabbar-audit-modify-frame-parameters)

(defun agent-repl--retire-redisplay-storm-watchdog ()
  "Remove the obsolete reactive redisplay watchdog after a hot reload.
Returns a plist recording whether a heartbeat timer was cancelled and
whether the watchdog function was present on `pre-redisplay-function'.
Fresh Emacs processes have neither; the cleanup exists so loading this
fix into an older live process does not leave its timer or hook behind."
  (let ((timer-cancelled nil)
        (hook-present nil))
    (when (and (boundp 'agent-repl--storm-tick-timer)
               (timerp agent-repl--storm-tick-timer))
      (cancel-timer agent-repl--storm-tick-timer)
      (when (boundp 'agent-repl--timers)
        (setq agent-repl--timers
              (delq agent-repl--storm-tick-timer agent-repl--timers)))
      (setq agent-repl--storm-tick-timer nil
            timer-cancelled t))
    (when (boundp 'pre-redisplay-function)
      (let ((prior-hook pre-redisplay-function))
        (remove-function pre-redisplay-function
                         #'agent-repl--redisplay-storm-watchdog)
        (setq hook-present (not (eq prior-hook pre-redisplay-function)))))
    (list :timer-cancelled timer-cancelled :hook-present hook-present)))

(defun agent-repl--tabbar-pin-frame (frame rows)
  "Pin FRAME's tab bar to ROWS and preserve that explicit line count.

This is status.el's frame-parameter integration boundary.  It sets
`tab-bar-lines-keep-state' before `tab-bar-lines', preventing Emacs's
native one-line recalculation from overwriting the fixed agent-repl
height during later tab operations.  `frame-inhibit-implied-resize'
must already contain `tab-bar-lines' so the height change is absorbed by
the frame text area rather than resizing the outer NSWindow."
  (set-frame-parameter frame 'tab-bar-lines-keep-state t)
  (set-frame-parameter frame 'tab-bar-lines rows))

(defun agent-repl-tabbar-apply-row-count ()
  "Reapply the fixed agent-repl row count to the selected frame.

`agent-repl--install-fixed-height-tab-bar' normally pins every graphical
frame automatically.  This interactive command reasserts the same
contract for manual recovery after external code has changed the selected
frame.  Returns the applied row count."
  (interactive)
  (let* ((rows agent-repl--tabline-row-count)
         (frame (selected-frame))
         (prior-lines (frame-parameter frame 'tab-bar-lines))
         (prior-keep-state
          (frame-parameter frame 'tab-bar-lines-keep-state)))
    (agent-repl--tabbar-pin-frame frame rows)
    (agent-repl--log
     (let ((current (agent-repl--ws-current-name)))
       (and current (agent-repl--ws-known-p current) current))
     "tab-bar-apply-row-count: frame=%S rows=%d prior-lines=%S lines=%S prior-keep-state=%S keep-state=%S"
     frame rows prior-lines (frame-parameter frame 'tab-bar-lines)
     prior-keep-state
     (frame-parameter frame 'tab-bar-lines-keep-state))
    (message "agent-repl: tab-bar set to %d line%s on this frame"
             rows (if (= rows 1) "" "s"))
    rows))

(defun agent-repl--install-fixed-height-tab-bar ()
  "Install agent-repl's fixed-height tab bar without native auto-resizing.
Sets the global formatter, disables `auto-resize-tab-bars', and pins
`agent-repl--tabline-row-count' on every current graphical frame plus
`default-frame-alist'.  Both scopes also receive
`tab-bar-lines-keep-state', so Emacs's native one-line recalculation
cannot overwrite the explicit two-line contract.

`frame-inhibit-implied-resize' is configured before any live frame
height changes.  The changed tab-bar height therefore comes out of the
frame's text area instead of requesting an outer NSWindow resize, which
prevents the clipped-resize redisplay livelock.  The obsolete reactive
watchdog is removed after a hot reload.

Logs every before/after value needed to diagnose a future regression:
row count, auto-resize value, default frame parameters, current frame
parameters, and the watchdog cleanup result."
  (let* ((rows agent-repl--tabline-row-count)
         (frames (cl-remove-if-not #'display-graphic-p (frame-list)))
         (prior-auto-resize auto-resize-tab-bars)
         (prior-auto-width tab-bar-auto-width)
         (prior-format tab-bar-format)
         (prior-default-lines (alist-get 'tab-bar-lines default-frame-alist))
         (prior-default-keep-state
          (alist-get 'tab-bar-lines-keep-state default-frame-alist))
         (prior-frame-state
          (mapcar (lambda (frame)
                    (list frame
                          :lines (frame-parameter frame 'tab-bar-lines)
                          :keep-state
                          (frame-parameter frame 'tab-bar-lines-keep-state)))
                  frames))
         (watchdog-cleanup (agent-repl--retire-redisplay-storm-watchdog)))
    (setq tab-bar-format '(agent-repl-workspace-tabline-formatted
                           tab-bar-format-align-right
                           agent-repl-current-workspace-name-segment)
          tab-bar-show t
          tab-bar-close-button-show nil
          auto-resize-tab-bars nil
          ;; The visible formatter returns one menu-item caption containing
          ;; both rows.  Emacs 30's auto-width pass treats a caption whose
          ;; first glyph has a tab face as one resizable tab and deletes
          ;; characters from its end, which can erase the entire second row
          ;; before C redisplay sees it.
          tab-bar-auto-width nil)
    ;; Obsolete since 28.1 but still honored; the tab-bar-format migration is deliberate future work.
    (with-suppressed-warnings ((obsolete tab-bar-new-button-show))
      (setq tab-bar-new-button-show nil))
    ;; Establish the no-outer-resize invariant before `tab-bar-mode' or any
    ;; explicit frame parameter update can alter the live tab-bar height.
    (unless (eq frame-inhibit-implied-resize t)
      (cl-pushnew 'tab-bar-lines frame-inhibit-implied-resize))
    (tab-bar-mode 1)
    ;; `tab-bar-mode' writes `(tab-bar-lines . 1)' into
    ;; `default-frame-alist', so pin both parts of the fixed contract after
    ;; enabling it and then apply the same contract to every live GUI frame.
    (setf (alist-get 'tab-bar-lines default-frame-alist) rows
          (alist-get 'tab-bar-lines-keep-state default-frame-alist) t)
    (dolist (frame frames)
      (agent-repl--tabbar-pin-frame frame rows))
    (agent-repl--log
     (let ((current (agent-repl--ws-current-name)))
       (and current (agent-repl--ws-known-p current) current))
     "tab-bar-fixed-height: rows=%d frames=%d prior-auto-resize=%S auto-resize=%S prior-auto-width=%S auto-width=%S prior-format=%S format=%S prior-default-lines=%S default-lines=%S prior-default-keep-state=%S default-keep-state=%S prior-frame-state=%S frame-state=%S watchdog-cleanup=%S"
     rows (length frames) prior-auto-resize auto-resize-tab-bars
     prior-auto-width tab-bar-auto-width prior-format tab-bar-format
     prior-default-lines (alist-get 'tab-bar-lines default-frame-alist)
     prior-default-keep-state
     (alist-get 'tab-bar-lines-keep-state default-frame-alist)
     prior-frame-state
     (mapcar (lambda (frame)
               (list frame
                     :lines (frame-parameter frame 'tab-bar-lines)
                     :keep-state
                     (frame-parameter frame 'tab-bar-lines-keep-state)))
             frames)
     watchdog-cleanup)))

;; Install after persp-mode loads so workspace names resolve during render.
(agent-repl--ws-after-system-load #'agent-repl--install-fixed-height-tab-bar)

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

;; `agent-repl--update-ws-state' is gone with the decay it drove.  The
;; timer no longer touches the agent-state axis at all: every transition on
;; it is now owned by the SSM's pushed WorkspaceState, which is what makes
;; a tab's color a report of something that happened rather than partly a
;; report and partly a clock.

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
   ((null agent-repl--update-in-flight)
    (agent-repl--log-verbose nil "update-in-flight-p: result=nil reason=no-chain")
    nil)
   ((< (- (float-time) agent-repl--update-in-flight)
       agent-repl-state-stale-threshold)
    (agent-repl--log-verbose nil "update-in-flight-p: result=t age=%.2fs threshold=%.2fs"
                              (- (float-time) agent-repl--update-in-flight)
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
`agent-repl--mark-dead') run every tick.  DO-GIT-P gates the
expensive git refresh (`agent-repl--async-refresh-branch-merged') so
it fires only on the mod-N tick selected by
`agent-repl-state-git-tick-modulus'.

A gui-frontend workspace always takes the alive branch here,
regardless of what `agent-repl--agent-running-p' would separately
answer: for a gui workspace that predicate is a CHEAP check (a daemon
session binding exists — see `agent-repl--gui-running-p') rather than
a live daemon health probe, and it can be transiently nil for reasons
that are not a death (e.g. mid-reattach after a daemon restart).
Liveness/death for a gui workspace is owned exclusively by the daemon
\(pushed DEAD `WorkspaceState' — see
`agent-repl--status-react-to-pushed-death'), so this poll deliberately
never marks a gui workspace dead."
  (let* ((gui-p (agent-repl--ws-gui-frontend-p ws))
         (running-p (and (not gui-p) (agent-repl--agent-running-p ws))))
    ;; Per-workspace timer body: record detailed branch inputs only in
    ;; verbose traces because it normally runs once per second per workspace.
    (agent-repl--log-verbose ws
                              "update-one-workspace-state: ws=%s gui=%s running=%s do-git=%s"
                              ws gui-p running-p do-git-p)
    (unless (or gui-p running-p)
      ;; No live agent session → clear non-thinking state.
      (agent-repl--mark-dead ws)))
  ;; Merged-ness is independent of agent liveness — refresh for every
  ;; workspace so `agent-repl--ws-merged-p' always reads a
  ;; fresh `:branch-merged' value.  Gated on DO-GIT-P because the
  ;; refresh's preconditions and process spawn are the reason the
  ;; whole pass is gated at all.
  (when (and do-git-p
             (fboundp 'agent-repl--async-refresh-branch-merged))
    (agent-repl--async-refresh-branch-merged ws)))

(defun agent-repl--update-all-workspace-states--step (remaining do-git-p gap)
  "Process the head of REMAINING; schedule self for the rest.
DO-GIT-P is the precomputed mod-N gate for the whole pass (snapshotted
at chain kickoff so every workspace in this pass sees the same value).
GAP is the inter-step delay in seconds.

Per-step `agent-repl--ws-project-pollable-p' recheck covers
snapshot-vs-live divergence: a workspace can be removed mid-chain
\(`--ws-del' from a merge, kill, or sweep), or become a non-project
placeholder, and we must not act on either shape.  The body itself is
wrapped in `condition-case' so an error in one ws step never wedges
the in-flight flag for subsequent ticks — the chain logs and keeps
going.

When `agent-repl--update-spread-sync' is non-nil (tests only),
recurses directly instead of via `run-at-time'."
  (cond
   ((null remaining)
    (agent-repl--update-all-workspace-states--finalize))
   (t
    (let ((ws (car remaining))
          (rest (cdr remaining)))
      (condition-case err
          (if (agent-repl--ws-project-pollable-p ws)
              (agent-repl--update-one-workspace-state ws do-git-p)
            (agent-repl--log-verbose
             ws
             "update-all-workspace-states--step: skipped ws=%s live=%S project-dir=%S"
             ws (agent-repl--ws-live-p ws)
             (agent-repl--ws-get ws :project-dir)))
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
  (agent-repl--log-verbose nil "update-all-workspace-states--finalize: previous=%S"
                            agent-repl--update-in-flight)
  (setq agent-repl--update-in-flight nil))

(defun agent-repl--update-all-workspace-states-now ()
  "Unguarded entrypoint for the workspace-state update chain.
Snapshots `agent-repl--ws-project-poll-partition' so the chain
iterates a stable list of live project workspaces even as state mutates
mid-pass; per-step `agent-repl--ws-project-pollable-p' recheck filters
out workspaces deleted or reduced to placeholders during the spread
window.  The partition's placeholder names are included in the kickoff
log so every exclusion is observable.

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
  ;; Poll only live project workspaces.  Persp-mode placeholders such as
  ;; "main" and "none" are real hash entries but intentionally have no
  ;; project directory; the workspace-layer partition makes the exclusion
  ;; explicit and observable.
  (let* ((partition (agent-repl--ws-project-poll-partition))
         (ws-names (car partition))
         (placeholder-names (cdr partition))
         (n (length ws-names))
         (do-git-p (zerop (mod agent-repl--update-tick-counter
                               agent-repl-state-git-tick-modulus)))
         (gap (if (and (> n 0) (> agent-repl-state-spread-window 0))
                  (max agent-repl-state-spread-min-gap
                       (/ agent-repl-state-spread-window (float n)))
                agent-repl-state-spread-min-gap)))
    (agent-repl--log-verbose
     nil
     "update-all-workspace-states-now: count=%d placeholders=%S do-git=%s gap=%.3fs counter=%d"
     n placeholder-names do-git-p gap agent-repl--update-tick-counter)
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
`force-mode-line-update' so the alternating-string cache-bust actually
reaches the display without invoking Emacs's one-line height policy.

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
  (if (agent-repl--update-in-flight-p)
      (agent-repl--log-verbose nil "update-all-workspace-states: skipped reason=in-flight")
    (agent-repl--update-all-workspace-states-now)))

;; Periodically update all workspace states.
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
  for that state (routed under MERGED, not orphaned as :dead).  Without this guard, the next poll would re-classify the
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
    (agent-repl--log-verbose ws
                              "mark-dead: ws=%s skipped repl-state=%s agent-state=%s"
                              ws (agent-repl--ws-repl-state ws) (agent-repl--ws-agent-state ws))
    nil)
   ((eq (agent-repl--ws-repl-state ws) :dead)
    (when (agent-repl--ws-agent-state ws)
      (agent-repl--log ws "mark-dead: ws=%s already :dead — clearing stale agent-state=%s"
                        ws (agent-repl--ws-agent-state ws))
      (agent-repl--ws-put ws :agent-state nil)
      (force-mode-line-update t))
    (unless (agent-repl--ws-agent-state ws)
      (agent-repl--log-verbose ws "mark-dead: ws=%s skipped already-dead-clean" ws)))
   (t
    (agent-repl--log ws "mark-dead: ws=%s agent-state=%s -> :dead"
                      ws (agent-repl--ws-agent-state ws))
    (agent-repl--ws-put ws :repl-state :dead)
    (agent-repl--ws-put ws :agent-state nil)
    (force-mode-line-update t))))

(defun agent-repl--status-react-to-pushed-death (ws new previous)
  "Mark WS dead when the daemon pushes a DEAD render state NEW.
Subscriber for `agent-repl-ws-state-transition-functions' (frontend-state.el).

The daemon owns session death now (design §10 sentinel endgame): the
deleted `session_dead_' sentinel handler's SOLE effect was
`agent-repl--mark-dead', which is re-anchored here onto the pushed DEAD
`WorkspaceState' (the terminal/death-reason detail rides the pushed
`SessionView').  `mark-dead' owns the guarded `:dead' transition
\(idempotent; respects the `:merged'/`:merge-failed' precedence and the
`:init' grace), so a non-DEAD NEW is a no-op — this subscriber only
forwards the DEAD case that used to arrive as a daemon-written sentinel."
  (if (eq new :dead)
      (progn
        (agent-repl--log ws "status-react-to-pushed-death: ws=%s previous=%s next=%s action=mark-dead"
                          ws previous new)
        (agent-repl--mark-dead ws))
    (agent-repl--log ws "status-react-to-pushed-death: ws=%s previous=%s next=%s action=ignored"
                      ws previous new)))

;; Registered here (status.el owns `mark-dead') though the hook variable is
;; defined later in frontend-state.el: `add-hook' auto-vivifies the unbound
;; variable, and frontend-state.el's `defvar ... nil' does not reset an
;; already-bound variable, so this subscriber survives the load order.
(add-hook 'agent-repl-ws-state-transition-functions
          #'agent-repl--status-react-to-pushed-death)

;;; Frame focus handler -------------------------------------------------------

(defun agent-repl--on-frame-focus ()
  "Update all workspace states when Emacs regains focus.
Calls `agent-repl--update-all-workspace-states-now' (the unguarded
entrypoint) rather than the periodic-timer entrypoint: frame focus is
an event-driven signal that the user is back and wants fresh data, so
it should kick a refresh regardless of the in-flight reentry guard."
  (if (frame-focus-state)
      (progn
        (agent-repl--log (agent-repl--ws-current-log-name) "on-frame-focus: focused")
        (agent-repl--update-all-workspace-states-now))
    (agent-repl--log-verbose (agent-repl--ws-current-log-name) "on-frame-focus: not focused")))

(add-function :after after-focus-change-function #'agent-repl--on-frame-focus)
