;;; status.el --- workspace status state machine and tab bar rendering -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

;; Cross-file forward declarations.  These sources load in the dependency
;; order config.el establishes and resolve each other's calls at call time,
;; so the declarations below exist for the byte-compiler alone.
(declare-function agent-repl--agent-view-buffer-name-p "core")
(declare-function agent-repl--agent-view-buffer-p "core")
(declare-function agent-repl--agent-input-buffer-name-p "core")
(declare-function agent-repl--agent-input-buffer-p "core")
(declare-function agent-repl--cancel-timer-key "core")
(declare-function agent-repl--error "core")
(declare-function agent-repl--info "core")
(declare-function agent-repl--log "core")
(declare-function agent-repl--log-verbose "core")
(declare-function agent-repl--register-timer "core")
(declare-function agent-repl--ws-add-activated-hook "workspace")
(declare-function agent-repl--ws-after-system-load "workspace")
(declare-function agent-repl--ws-by-ref-id "workspace")
(declare-function agent-repl--ws-current-log-name "workspace")
(declare-function agent-repl--ws-current-name "workspace")
(declare-function agent-repl--ws-get "workspace")
(declare-function agent-repl--ws-known-p "workspace")
(declare-function agent-repl--ws-put "workspace")
(declare-function agent-repl--ws-render-status "workspace")
(declare-function agent-repl--ws-resolve-persp "workspace")
(declare-function agent-repl--ws-tab-face "workspace")
(declare-function agent-repl--ws-tabline-names "workspace")
(declare-function agent-repl--ws-window-conf "workspace")
(declare-function agent-repl-host-mark-viewed "host")
(declare-function agent-repl-roster-row-attention-p "roster")
(declare-function agent-repl-roster-row-id "roster")
(declare-function agent-repl-roster-row-priority-label "roster")
(declare-function agent-repl-roster-row-status "roster")
(declare-function agent-repl-roster-walk "roster")
(declare-function agent-repl-roster-viewed-for-ws "roster" (ws))

(defun agent-repl--status-log-scope (central-reason)
  "Return the active workspace or an explicit CENTRAL-REASON marker.
Status rendering and frame setup both run before workspace activation.  This
helper keeps their log sites attributed when a workspace exists and makes the
frame-wide case reviewable rather than returning an anonymous nil scope.

This logging-boundary helper emits no record because doing so would recurse."
  (let ((current (agent-repl--ws-current-name)))
    (if (and current (agent-repl--ws-known-p current))
        current
      (list :agent-repl-central central-reason))))

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
  "Format string for tab workspace name padding.

Only the TRAILING half of this format survives as written: it is a width
fill drawn after the name.  Any LEADING whitespace is stripped by
`agent-repl--render-tab', which emits the one space between `[N]' and the
name itself, so the gap is exactly one space whatever this format and the
badge run do (owner ruling, 2026-09-13)."
  :type 'string
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
;; !! `agent-repl--status-dwell-tick'.                            !!
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
;; !! `agent-repl--status-dwell-tick' timer calls            !!
;; !! `--force-tab-bar-redraw' every tick.                              !!
;; !!                                                                  !!
;; !! This has been accidentally removed multiple times.  DO NOT       !!
;; !! remove it again.  It is NOT dead code.  It is NOT cosmetic.     !!
;; !!                                                                  !!
;; !! It is, however, no longer the mechanism that makes an ARM        !!
;; !! CHANGE reach the pixels.  The toggle changes the string on a     !!
;; !! clock; `agent-repl--tabline-render-key' (below) changes it the   !!
;; !! moment the rendered rows differ in ANY way, faces included, and  !!
;; !! `agent-repl-status-repaint-on-roster-push' schedules the         !!
;; !! redisplay that draws it.  Measured before the key existed: with  !!
;; !! one gated turn in flight the bar painted the PREVIOUS arm until  !!
;; !! the next tick, because the two strings compared `equal'.         !!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(defvar agent-repl--tabline-space-toggle nil
  "Non-nil means append the zero-width cache-buster to the tabline string.
Flipped on every repaint by `agent-repl--force-tab-bar-redraw'
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

;; --- Render identity: the structural repaint key -------------------------
;;
;; The toggle above changes the string on a CLOCK.  That is not enough: the
;; tab bar is repainted by Emacs's C redisplay, which keeps the last items
;; vector it built and compares the next one with `Fequal' (src/xdisp.c,
;; `update_tab_bar'; src/keyboard.c, `tab_bar_items').  `equal' ignores text
;; properties, so a render whose only change is a FACE — every arm change —
;; is the same item to that comparison, and nothing is repainted until the
;; clock happens to tick.  Between a roster push and that tick the bar
;; paints the arm the workspace had BEFORE the push.
;;
;; The repaint key below closes that gap structurally: every render carries
;; an invisible generation number that advances exactly when the rendered
;; string changes INCLUDING its properties.  Two renders that would paint
;; differently therefore never compare `equal', on any Emacs, on any
;; schedule, and the repaint follows the render instead of the clock.

(defvar agent-repl--tabline-render-identities (make-hash-table :test 'eq)
  "Frame -> (GENERATION . RENDERED) for the last tab-bar render on that frame.
GENERATION is the integer `agent-repl--tabline-render-key' embedded in
the frame's last render, and RENDERED is the propertized string it
was derived from.  Keyed by frame because each frame renders its own
anchor and width, and a shared last-render would advance the generation
on every alternate frame's redisplay and repaint both bars for nothing.")

(defun agent-repl--tabline-render-key (rendered &optional frame)
  "Return the invisible run that gives RENDERED a property-aware identity.
Advances FRAME's generation when RENDERED differs from FRAME's previous
render under `equal-including-properties', and returns that generation
as an `invisible'-propertized decimal string.  Appended to the visible
formatter's output so the tab bar's C-side items cache, which compares
with plain `equal', sees a DIFFERENT string whenever the paint differs
and the SAME string when it does not.  Zero rendered width, like the
clock-driven cache-buster, for the same row-wrap reason."
  (let* ((frame (or frame (selected-frame)))
         (last (gethash frame agent-repl--tabline-render-identities))
         (generation
          (if (and last (equal-including-properties rendered (cdr last)))
              (car last)
            (1+ (if last (car last) 0)))))
    (puthash frame (cons generation rendered)
             agent-repl--tabline-render-identities)
    (propertize (number-to-string generation) 'invisible t)))

(defun agent-repl--load-priority-images ()
  "Load priority badge PNGs from the module images/ directory.
Populates `agent-repl--priority-images' with display-ready image specs."
  (let* ((dir (file-name-directory (or load-file-name buffer-file-name)))
         ;; `images/' sits at the module root; this file lives in `lisp/'.
         (img-dir (expand-file-name "../images/" dir))
         (names agent-repl-priority-levels)
         (height (frame-char-height)))
    (setq agent-repl--priority-images
          (cl-loop for name in names
                   for file = (expand-file-name (concat name ".png") img-dir)
                   when (file-exists-p file)
                   collect (cons name (create-image file 'png nil
                                                    :height height
                                                    :ascent 'center))))
    (agent-repl--log '(:agent-repl-central "tab rendering and shared assets span workspaces") "load-priority-images: loaded=%d" (length agent-repl--priority-images))))

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
    (agent-repl--log-verbose ws "align-buffer-to-ws-dir: ws=%s skipped dead-buffer=%S" ws buf)
    nil)
   ((not (agent-repl--ws-get ws :project-dir))
    (agent-repl--log-verbose ws "align-buffer-to-ws-dir: ws=%s skipped missing-project-dir buffer=%s"
                              ws (buffer-name buf))
    nil)
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
;;      default-spec FUNCTION (it reads the bar's own background, which
;;      only the frame can answer) and the palette defconst that compose
;;      those named values into per-state appearance specs.  No palette row contains a string literal,
;;      and no palette row spells its own shape out either: every one
;;      is built by `agent-repl--tab-palette-row'.
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
failed or a session controller that died on a terminal protocol error, a store
outage, or a backfill that failed.

It is deliberately ONE color for all of them.  The distinctions matter
to whoever debugs it, not to the user reading a tab: every one of them
means the same thing to them, which is that this workspace cannot be
relied on right now.  The sidebar carries the distinction where it is
worth having.

THERE IS NO TEAL BESIDE IT ANY MORE.  Teal existed solely to hold
hibernation apart from a broken substrate; hibernation LEFT THE CONTRACT
\(a parked workspace presents as live with `shim_attached' false\), so the
sixth color went with it, and blue no longer carries a benign second
job.")

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

(defconst agent-repl--color-merging-purple "#a21caf"
  "PURPLE: a MERGE is in flight.
About to enqueue, waiting behind a sibling, or running.  The work is the
system\='s rather than the agent\='s, which is exactly why red is wrong for
it: a merging workspace\='s turn ENDED before its merge began, and red
would claim a turn was still running.  What purple shares with red is
only the actionability claim — the user cannot act on the workspace
until the merge resolves.

Purple is the vendor-blocked color on every OTHER surface, and this
renderer is the declared exception (`agent-repl--tab-bar-color-overrides\=').
A tab bar has no glyph to tell two purples apart, so `:vendor-blocked\='
takes blue here and purple says one thing.

A magenta-leaning purple, deliberately clear of any violet: a merge is
the system working, and confusing it with a session that has stopped is
the misread this color exists to prevent.")

(defconst agent-repl--color-default-bracket  "white"
  "White used for bracket numerals on unselected tabs of any state.")

(defconst agent-repl--color-selected-bg      "#c0c0c0"
  "Lightish grey: THE selected tab's background (owner ruling, 2026-09-14).

Silver.  Previously dormant — for a stretch, selection was an
`:underline' alone and a tab's background carried only its connection
COLOR (armed) or the panels-open EXTENT (any tab).  The owner overrode
that: the SELECTED tab now paints this grey across whatever extent it
would otherwise have painted — bracket-only or full — in place of its
connection color, so the selected tab is legible-grey end to end and the
underline (kept, belt-and-suspenders) is the secondary marker.  An
UNSELECTED tab is untouched by this: it still carries its connection
color (armed) or sits flush on the bar (un-armed), and still goes FULL
exactly when its panels are open, per owner ruling 5 (2026-09-13).  See
`agent-repl--tab-palette-row', `agent-repl--tab-default' and
`agent-repl--tab-face' for where this grey is applied, and
`agent-repl--tab-bar-legible-fg' for how the foreground is chosen
against it so `agent-repl-tab-contrast-floor' still holds.")

(defconst agent-repl--color-light            "white"
  "Light foreground for dark state backgrounds.")

(defconst agent-repl--color-dark             "black"
  "Dark foreground for light state backgrounds.")

;; THERE IS NO `agent-repl--color-unarmed-bg' HERE ANY MORE, and its absence is
;; the point: AN UNSELECTED TAB SITS FLUSH ON THE BAR.  Its name region takes
;; the TAB BAR's own background, read off the `tab-bar' face at render time by
;; `agent-repl--tab-bar-background', so the only thing separating one
;; unselected tab from the bar it sits in is the text drawn on it.
;;
;; It was a stated dark grey (`#4a4a4a') for exactly one day, and that grey was
;; the overshoot of a real fix.  The defect was a tab that stated NEITHER half
;; of its pair: `:none', `:inactive' and the terminal merge arms take no
;; lifecycle color, and the tab was drawn by leaving background and foreground
;; `unspecified' — "whatever this frame's faces happen to resolve to".  Measured
;; on a headless sandbox frame that came out as a name run of BLACK glyphs on
;; `#14141a' (about 1.06:1) and a bracket numeral of WHITE on the tab bar's own
;; `#d9d9d9' (about 1.3:1): a tab nobody could read, in two ways at once, and
;; both invisible to every assertion because the STRING was correct and only its
;; resolved value was not.
;;
;; ONLY THE FOREGROUND HALF OF THAT FIX SURVIVES, because only the foreground
;; half was the defect.  A background painted to match the bar cannot be
;; illegible against the bar; what can be illegible is the ink on it, and that
;; is now CHOSEN against the bar's measured background rather than inherited
;; (`agent-repl--tab-bar-legible-fg').  So the pair is still stated outright,
;; still holds on a themed frame and on the no-theme sandbox, and no longer
;; introduces a grey that reads as a sixth state.

(defconst agent-repl-tab-contrast-floor 3.0
  "The contrast ratio every tab's own foreground/background pair must meet.

3.0:1 is WCAG 2.1's AA floor for LARGE text, and that is the floor this
surface answers to because every tab is drawn at
`agent-repl--tab-weight' — bold — which is what makes text large by the
standard's own definition.  The ordinary-text floor of 4.5:1 is NOT used
here, and that is a measurement rather than a convenience: two of the
palette's existing state colors sit between the two numbers (white on
`agent-repl--color-thinking-red' is 4.00:1 and on
`agent-repl--color-merging-purple' 3.14:1), and re-choosing a state
color is a change to the cross-language color contract in
`proto/vocab/render-colors.json', not to a tab's appearance.

What the floor exists to catch is nothing near that line.  The un-armed
tab measured 1.06:1 for its name and 1.3:1 for its numeral — text drawn
in a color one step off its own background — and every number above
fails it by a wide margin.

It is stated once, here, so the palette's rows, the faces built from
them and a reader that takes a pair off a screenshot are all held to
ONE number rather than to three that could drift apart.")

(defun agent-repl--relative-luminance (color)
  "Return COLOR's WCAG relative luminance, or signal if Emacs cannot read it.

COLOR is any name or hex string `color-name-to-rgb' accepts.  A color
this frame cannot resolve is an ERROR rather than a guess: a luminance
invented here would let an illegible pair pass the very check that
exists to catch it."
  (let ((rgb (color-name-to-rgb color)))
    (unless rgb
      (error "agent-repl: cannot resolve the color %S, so its legibility cannot be checked" color))
    (cl-loop for channel in rgb
             for weight in '(0.2126 0.7152 0.0722)
             sum (* weight
                    (if (<= channel 0.03928)
                        (/ channel 12.92)
                      (expt (/ (+ channel 0.055) 1.055) 2.4))))))

(defun agent-repl-color-contrast-ratio (foreground background)
  "Return the WCAG contrast ratio between FOREGROUND and BACKGROUND.

1.0 is two identical colors and 21.0 is black on white.  This is the
module's own answer to \"can this be read?\", so the palette, the faces
built from it and a reader that takes a pair off a screenshot all
ask ONE function rather than each carrying its own arithmetic."
  (let* ((a (agent-repl--relative-luminance foreground))
         (b (agent-repl--relative-luminance background))
         (lighter (max a b))
         (darker  (min a b)))
    (/ (+ lighter 0.05) (+ darker 0.05))))

(defun agent-repl--tab-bar-background ()
  "Return the color THIS frame paints the tab bar's own background.

Read off the `tab-bar\=' face with inheritance resolved, so it is whatever
the active theme says the bar is, and `grey\='/`grey85\=' on the no-theme
sandbox frame where `tab-bar\=' falls back to its own defface.

IT IS READ, NEVER SPELLED.  An unselected tab must be the same color as
the bar it sits in, and a literal here would be that color on exactly one
theme — which is how the tab bar came to draw a white numeral on its own
`#d9d9d9\=' and call it an appearance.

A bar whose background this frame cannot resolve is an ERROR rather than
a guess: every caller is about to pair a foreground with this color and
check that pair against `agent-repl-tab-contrast-floor\=', and a color
invented here would let an illegible pair pass the very check that exists
to catch it.  `default\=' is deliberately NOT consulted as a second
choice — in a batch frame it answers the pseudo-color
\=`unspecified-bg\=', which no contrast arithmetic can read."
  (let ((bg (face-background 'tab-bar nil t)))
    (unless (and (stringp bg) (color-name-to-rgb bg))
      (error "agent-repl: this frame resolves the tab bar's background to %S, so an unselected tab has no ground to draw on" bg))
    bg))

(defun agent-repl--tab-bar-legible-fg (&optional background)
  "Return the foreground to draw an unselected tab's text in.

BACKGROUND defaults to `agent-repl--tab-bar-background\=' — the ground the
text will actually sit on, since an unselected tab is painted the bar's
own color.

The choice is between the palette's two existing foregrounds,
`agent-repl--color-light\=' and `agent-repl--color-dark\=', and it is the
one with MORE contrast against BACKGROUND.  That is what makes the pair
theme-proof without a theme-specific literal anywhere: the better of
black and white clears `agent-repl-tab-contrast-floor\=' against ANY
background, because the two ratios are only equal at a luminance where
each is about 4.58:1 — over the 3.0:1 floor with room to spare.

There is no third candidate on purpose.  A foreground derived from the
background by some luminance rule would land on colors the rest of the
palette never uses, and this surface's whole vocabulary is that a tab's
ink is light or dark and its GROUND carries the meaning."
  (let* ((bg (or background (agent-repl--tab-bar-background)))
         (light agent-repl--color-light)
         (dark  agent-repl--color-dark))
    (if (>= (agent-repl-color-contrast-ratio light bg)
            (agent-repl-color-contrast-ratio dark bg))
        light
      dark)))

;; There are no bracket-label glyphs.  The [N] bracket carries its number
;; and the state's COLOR, nothing else: a glyph beside the numeral was a
;; second vocabulary saying what the color already says, and the sidebar
;; is where a state's DETAIL belongs.

(defconst agent-repl--tab-weight             'bold
  "Font weight applied to every tab face.")

(defun agent-repl--tab-default ()
  "Default tab-appearance spec for states absent from `agent-repl--tab-palette\='.

THE UNSELECTED HALF IS THE BAR'S OWN BACKGROUND with a foreground chosen
against it, which is why this is a FUNCTION and not the defconst it used
to be: the bar's color is a property of the frame's theme, so it can only
be read at render time (`agent-repl--tab-bar-background\=').  A constant
folded at load time would be the color of whatever theme happened to be
active when this file loaded — and would go stale the moment one was
enabled.

An unselected tab therefore sits FLUSH on the bar: no grey, no second
ground, nothing but its text to separate it from the bar it lives in.
What it does NOT do is inherit its foreground, which is the defect the
un-armed row exists for — see the comment above
`agent-repl-tab-contrast-floor\=' for the measurement.

The SELECTED half no longer sits flush on the bar (owner ruling,
2026-09-14): it paints `agent-repl--color-selected-bg' instead, with a
foreground chosen against THAT grey rather than against the bar, plus
`:underline t' kept as a secondary marker.  A state absent from the
palette therefore looks exactly like an armed one once selected — grey
background, legible ink, underline — and only the UNSELECTED half still
sits flush on the bar with no ground of its own.  See
`agent-repl--tab-palette-row' for the same override on armed rows."
  (let* ((bg (agent-repl--tab-bar-background))
         (fg (agent-repl--tab-bar-legible-fg bg))
         (selected-fg (agent-repl--tab-bar-legible-fg
                       agent-repl--color-selected-bg)))
    `(:unselected (:bg ,bg
                   :fg ,fg
                   :bracket-fg ,fg
                   :weight ,agent-repl--tab-weight)
      :selected   (:bg ,agent-repl--color-selected-bg
                   :fg ,selected-fg
                   :bracket-fg ,selected-fg
                   :underline t
                   :weight ,agent-repl--tab-weight))))

;; --- The six-color assignment --- ;;

(defconst agent-repl-status-color-table
  '((:none            . "none")
    (:inactive        . "none")

    (:init            . "blue")
    (:severed         . "blue")
    (:dead            . "blue")
    (:degraded        . "blue")
    (:start-failed    . "blue")

    (:vendor-blocked  . "blue")

    (:turn-failed     . "blue")

    (:submitting      . "red")
    (:thinking        . "red")
    (:clearing        . "red")
    (:compacting      . "red")

    (:idle-async      . "yellow")

    (:ready           . "green")
    (:done            . "green")
    (:interrupted     . "green")
    (:permission      . "green")

    (:merge-enqueuing . "none")
    (:merging         . "none")
    (:merge-queued    . "none")
    (:merge-conflict  . "green")
    (:merge-failed    . "blue")
    (:merged          . "green"))
  "Which of the five colors each ROSTER STATUS ARM takes, BY NAME.

Keyed by the `RosterRow.status' arm keywords `wire-roster.el' decodes
\(`agent-repl-wire-roster-row-status-keywords\='), which is the ONE
lifecycle vocabulary now: the daemon resolves every workspace\='s state
and the arm it sets IS the state a renderer paints.  There is no second
spelling and no local state machine left to keep aligned with it.

This is Emacs\='s corner of the cross-language contract in
proto/vocab/render-colors.json\='s `roster_status\=' section.  Go,
TypeScript and this table each assert against that one file, which is
the only mechanism that makes a divergence between the three fail loudly
instead of quietly; `test-render-colors.el\=' is this side of it and
fails on any row that diverges and on any arm missing from either side.

It names the color rather than its value: each renderer keeps its own
hex, since a tab-bar background and a CSS dot legitimately want
different shades of one idea.  What may never differ is the ASSIGNMENT.

\"none\" is a real answer.  The merge arms take none of the five here —
the sidebar reports them with a glyph rather than spending a lifecycle
color on the merge pipeline — and `none\=' and `inactive\=' take none
because a workspace with no session has no lifecycle to report at all.

THREE MERGE ARMS ARE COLORED AS WELL AS GLYPHED (owner ruling,
2026-09-28), as the fixture\='s `colored_merge_arms\=' declares.
`:merge-failed\=' is BLUE, the color that says something is wrong with
the workspace.  `:merge-conflict\=' (a parked merge included) is GREEN:
an expected state, ready for a human response, and never blue.
`:merged\=' is GREEN: a merge that landed is a settled success, where a
merge in progress is purple.
THE TAB BAR DECLARES ITS OWN OVERRIDES (see
`agent-repl-status-tab-bar-color-overrides\='); this table is what every
surface starts from, never what the tab bar finishes with.

THERE IS NO TEAL, and no RENDER_STATE_* enum: both left with
hibernation.")

(defconst agent-repl-status-tab-bar-color-overrides
  '((:merge-enqueuing . "purple")
    (:merge-queued    . "purple")
    (:merging         . "purple"))
  "Where the TAB BAR paints an arm differently from the shared assignment.

Emacs\='s corner of the fixture\='s `surface_overrides.emacs_tab_bar\='
section, asserted against it row for row.  The override is DECLARED in
the shared file rather than kept as a private local table: a surface that
quietly disagrees with the contract is the exact drift the contract
exists to catch.

The tab bar has no room for the sidebar\='s status word — a state reaches
it as the [N] bracket\='s color plus at most one glyph — so `none\' there
would render a workspace whose merge is running identically to one nobody
has touched.  PURPLE says what is true of the three IN-FLIGHT merge arms:
work is in flight, it is the SYSTEM\='s rather than the agent\='s, and the
user cannot act on the workspace until it resolves.  `merge_conflict\'
wants the user and the other two are terminal, so none of them is
overridden: the first two take their shared green and blue here too.

`:vendor-blocked\=' is NO LONGER an override.  It was one while the shared
assignment painted it purple and this glyph-less surface could not tell
two purples apart, so it borrowed blue here.  It is now blue in
`agent-repl-status-color-table\=' itself — every way the route to a working
session is compromised is blue — so the tab bar inherits blue with
nothing to declare.  An override that repaints an arm the color it already
has is not a divergence, and this table holds only real divergences.")

(defconst agent-repl-status-tab-bar-color-table
  (mapcar (lambda (row)
            (cons (car row)
                  (or (alist-get (car row) agent-repl-status-tab-bar-color-overrides)
                      (cdr row))))
          agent-repl-status-color-table)
  "The color each roster status arm takes ON THE TAB BAR, by name.

`agent-repl-status-color-table\=' with
`agent-repl-status-tab-bar-color-overrides\=' layered over it.  DERIVED
rather than written out, so the tab bar\='s table can never disagree with
the shared one about which arms EXIST — only about the four colors it
explicitly overrides.")

(defconst agent-repl-status-merge-glyphs
  '((:merge-enqueuing . "⧖")
    (:merge-queued    . "⧖")
    (:merging         . "↻")
    (:merge-conflict  . "≠")
    (:merge-failed    . "✗")
    (:merged          . "✓"))
  "The character each merge arm draws, keyed by the fixture\='s glyph NAMES.

proto/vocab/render-colors.json\='s `merge_glyphs\=' section names WHICH
glyph a merge state gets (queue, recycle, conflict, failed, check) and
deliberately not which character: each renderer maps those names to
whatever its surface can draw.  This is the tab bar\='s mapping, and the
assertion test checks that every named glyph has a character here.

The merge arms take no color on the shared assignment precisely so the
glyph can be the whole report; on the tab bar the three in-flight arms
ALSO take purple, and the glyph then says which of the three it is.
`:merge-conflict\=', `:merge-failed\=' and `:merged\=' take their shared
green, blue and green, and draw their glyph over it.")

(defconst agent-repl-status-inactive-glyph "?"
  "The glyph an `inactive\=' row draws.
Registered but with no open perspective, so there is no live session
whose lifecycle a dot could report — the contract itself says \"drawn as
a question mark\".")

(defconst agent-repl-status-attention-glyph "●"
  "The steady attention marker: the workspace has an unseen notification.
The daemon sets `RosterRow.attention\=' when a notification fires and
CLEARS it when SelectWorkspace names the workspace, so an ordinary tab
switch is the whole clearing act — no dedicated ack verb exists.")

(defconst agent-repl--color-by-name
  `(("blue"   . ,agent-repl--color-init-blue)
    ("purple" . ,agent-repl--color-merging-purple)
    ("red"    . ,agent-repl--color-thinking-red)
    ("yellow" . ,agent-repl--color-idle-async-yellow)
    ("green"  . ,agent-repl--color-done-green))
  "Map each of the five color NAMES to the constant this renderer draws it with.

The indirection is what lets the color tables speak the shared
vocabulary while the palette keeps painting with Emacs\='s own values.")

(defconst agent-repl--color-precedence
  '("blue" "purple" "red" "yellow" "green")
  "The five-color precedence, strongest claim first.

Each color is a strictly stronger claim about what the user CANNOT do
than the one beneath it: blue leads because a compromised route to a
session denies everything else; purple is the vendor or the account
refusing; red is the agent holding the turn; yellow is detached work the
user can talk over; green is the session yours to use.

The fixture\='s `precedence\=' array is the authority and this restates it
for the cross-language assertion.  THERE IS NO TEAL in it any more.")

(defun agent-repl--tab-palette-row (face color fg)
  "Build one `agent-repl--tab-palette' row from the parts that VARY.

Every row in that palette says the same three things, so the shape is
built here once rather than written out twenty times:

  FACE  — the `defface' the name region takes.
  COLOR — the state's color, painted across the WHOLE entry — the SAME
          color whether the tab is selected or not, because a tab's COLOR
          is its connection state and selection is an orthogonal axis
          (see `agent-repl--render-tab').
  FG    — the foreground legible against COLOR: light for the dark
          backgrounds, dark for the light ones.  Not derived from COLOR,
          because the six are not separable by a luminance rule that
          lands on the right answer for each.

An entry is ONE color end to end, so no row paints the [N] bracket
differently from the name region: the bracket carries the tab's number
and the state's color, and a second color inside one entry would be a
second vocabulary saying what the state color already says.

SELECTION IS THE LIGHTISH GREY (owner ruling, 2026-09-14).  The selected
look no longer shares COLOR/FG with the unselected one: it paints
`agent-repl--color-selected-bg' instead of the state color, with a
foreground `agent-repl--tab-bar-legible-fg' chooses against that grey so
`agent-repl-tab-contrast-floor' still holds — dark ink on this light
grey, in practice.  The bracket numeral matches that same foreground
rather than `agent-repl--color-default-bracket' (white), since white on
a light grey would reintroduce the illegible pair the floor exists to
catch.  `:underline t' is kept alongside the grey as a secondary,
belt-and-suspenders marker; it draws in the run's own foreground so it
still reads on the grey.  The full-vs-bracket background EXTENT still
belongs to panel visibility (`agent-repl--ws-display-state') for an
UNSELECTED tab — this override is scoped to `:selected' alone.

The invariant parts are the ones no row has ever varied:
`agent-repl--tab-weight' throughout, and the unselected bracket numeral
in `agent-repl--color-default-bracket'."
  (let* ((look `(:bg ,color
                 :fg ,fg
                 :bracket-fg ,agent-repl--color-default-bracket
                 :weight ,agent-repl--tab-weight))
         (selected-fg (agent-repl--tab-bar-legible-fg
                       agent-repl--color-selected-bg)))
    `(:face       ,face
      :unselected ,look
      :selected   (:bg ,agent-repl--color-selected-bg
                   :fg ,selected-fg
                   :bracket-fg ,selected-fg
                   :underline t
                   :weight ,agent-repl--tab-weight))))

(defconst agent-repl--tab-palette
  `((:init . ,(agent-repl--tab-palette-row
               'agent-repl-tab-init
               agent-repl--color-init-blue
               agent-repl--color-light))
    ;; SEVERED borrows init's blue: the claim about what the user can do is
    ;; identical — this workspace has no live session and something on our side
    ;; broke — and only the word and the glyph distinguish "coming up" from
    ;; "the substrate is gone".
    (:severed . ,(agent-repl--tab-palette-row
                  'agent-repl-tab-init
                  agent-repl--color-init-blue
                  agent-repl--color-light))
    (:thinking . ,(agent-repl--tab-palette-row
                   'agent-repl-tab-thinking
                   agent-repl--color-thinking-red
                   agent-repl--color-light))
    ;; :submitting borrows thinking's red for the same reason the context cuts
    ;; below do: the claim about what the user cannot do is identical, and only
    ;; the phase word says the shim has not taken the prompt yet.
    (:submitting . ,(agent-repl--tab-palette-row
                     'agent-repl-tab-thinking
                     agent-repl--color-thinking-red
                     agent-repl--color-light))
    ;; The two context cuts borrow thinking's red rather than taking a shade
    ;; of their own: they make the SAME claim about what the user cannot do,
    ;; and only the phase word in the footer distinguishes them.
    (:clearing . ,(agent-repl--tab-palette-row
                   'agent-repl-tab-thinking
                   agent-repl--color-thinking-red
                   agent-repl--color-light))
    (:compacting . ,(agent-repl--tab-palette-row
                     'agent-repl-tab-thinking
                     agent-repl--color-thinking-red
                     agent-repl--color-light))
    (:done . ,(agent-repl--tab-palette-row
               'agent-repl-tab-done
               agent-repl--color-done-green
               agent-repl--color-dark))
    ;; INTERRUPTED takes done's green, and had NO palette row at all until
    ;; now: the state resolved, the shared color table assigned it green, and
    ;; the tab bar fell through to `agent-repl--tab-default' and painted it
    ;; uncolored.  An assignment with no row is an assignment nothing honors.
    (:interrupted . ,(agent-repl--tab-palette-row
                      'agent-repl-tab-done
                      agent-repl--color-done-green
                      agent-repl--color-dark))
    ;; TURN-FAILED is BLUE (owner ruling, 2026-09-28): a turn end like the
    ;; two greens above, holding the tab as an unread result the same way, but
    ;; the turn did not produce what it was asked for, so it takes the blue
    ;; that says something is wrong.
    (:turn-failed . ,(agent-repl--tab-palette-row
                      'agent-repl-tab-init
                      agent-repl--color-init-blue
                      agent-repl--color-light))
    (:permission . ,(agent-repl--tab-palette-row
                     'agent-repl-tab-permission
                     agent-repl--color-done-green
                     agent-repl--color-dark))
    (:ready . ,(agent-repl--tab-palette-row
                'agent-repl-tab-ready
                agent-repl--color-done-green
                agent-repl--color-dark))
    (:idle-async . ,(agent-repl--tab-palette-row
                     'agent-repl-tab-idle-async
                     agent-repl--color-idle-async-yellow
                     agent-repl--color-dark))
    ;; VENDOR-BLOCKED is BLUE on the tab bar and purple everywhere else, which
    ;; is the one row here that reads its color from the override table rather
    ;; than the shared assignment.  An auth wall, a usage limit or a persistent
    ;; vendor failure is a compromised route to a working session, exactly like
    ;; the three blues below it; purple stays spent on the merge pipeline,
    ;; which a tab bar with no glyph could not otherwise tell apart from it.
    (:vendor-blocked . ,(agent-repl--tab-palette-row
                         'agent-repl-tab-init
                         agent-repl--color-init-blue
                         agent-repl--color-light))
    ;; MERGE-FAILED is BLUE and keeps its ✗ (owner ruling, 2026-09-28): "blue
    ;; status in general is used to signal something is wrong with the current
    ;; workspace".  It had no row at all, so it fell through to the default
    ;; and drew a bare uncolored ✗ that read as no status.
    (:merge-failed . ,(agent-repl--tab-palette-row
                       'agent-repl-tab-init
                       agent-repl--color-init-blue
                       agent-repl--color-light))
    ;; MERGE-CONFLICT is GREEN and keeps its ≠ (owner ruling, 2026-09-28): a
    ;; conflict, a parked merge included, is an EXPECTED state ready for a
    ;; human response.  Blue is reserved for what is unexpected or wrong.
    (:merge-conflict . ,(agent-repl--tab-palette-row
                         'agent-repl-tab-ready
                         agent-repl--color-done-green
                         agent-repl--color-dark))
    ;; MERGED is GREEN and keeps its ✓ (owner ruling, 2026-09-28): a merge in
    ;; progress is purple, and one that landed successfully is green.
    (:merged . ,(agent-repl--tab-palette-row
                 'agent-repl-tab-ready
                 agent-repl--color-done-green
                 agent-repl--color-dark))
    ;; `:start-failed', `:dead' and `:degraded' are BLUE, not colors of
    ;; their own: a shim that never came up, one that has gone away, and a
    ;; store outage are the same compromised route.  Which way the route is
    ;; broken is the sidebar's to report, not the tab's.
    (:start-failed . ,(agent-repl--tab-palette-row
                       'agent-repl-tab-init
                       agent-repl--color-init-blue
                       agent-repl--color-light))
    (:dead . ,(agent-repl--tab-palette-row
               'agent-repl-tab-init
               agent-repl--color-init-blue
               agent-repl--color-light))
    (:degraded . ,(agent-repl--tab-palette-row
                   'agent-repl-tab-init
                   agent-repl--color-init-blue
                   agent-repl--color-light))
    ;; The three IN-FLIGHT merge states take PURPLE, and it is theirs alone on
    ;; this surface.  They took no color at all until recently, which rendered
    ;; a workspace whose merge was running identically to one nobody had
    ;; touched; they then borrowed thinking's red, which said "a turn is
    ;; running" about a workspace whose turn ended before the merge began.
    ;; Purple says what is true: work is in flight, it is the SYSTEM's rather
    ;; than the agent's, and the user cannot act on the workspace while it
    ;; runs.  `:vendor-blocked' moved to blue above so purple means this and
    ;; nothing else here.
    (:merge-enqueuing . ,(agent-repl--tab-palette-row
                          'agent-repl-tab-merging
                          agent-repl--color-merging-purple
                          agent-repl--color-light))
    (:merge-queued . ,(agent-repl--tab-palette-row
                       'agent-repl-tab-merging
                       agent-repl--color-merging-purple
                       agent-repl--color-light))
    (:merging . ,(agent-repl--tab-palette-row
                  'agent-repl-tab-merging
                  agent-repl--color-merging-purple
                  agent-repl--color-light)))
  "Per-arm tab-appearance palette, keyed by the ROSTER STATUS ARM.
Each entry fully describes both selected and unselected looks for one
`RosterRow.status' arm keyword via nested `:unselected' and `:selected'
plists.

Every row is built by `agent-repl--tab-palette-row', so a row states
ONLY what it does not share with the others: its face, its color, and
its unselected foreground.  The shape itself lives in that one function,
and an entry is always ONE color end to end.

Two kinds of row answer to `agent-repl-status-tab-bar-color-table'
rather than to the shared `agent-repl-status-color-table', and both
divergences are declared in
`agent-repl-status-tab-bar-color-overrides'.  The three IN-FLIGHT merge
arms have rows at all because of the override that gives them purple;
`:vendor-blocked' has a row whose color is BLUE here and purple on every
badge-bearing surface.

The arms taking `none' have NO entry and fall through to
`agent-repl--tab-default': `:none' and `:inactive' have no lifecycle
to report at all.  `:merge-conflict' (green), `:merge-failed' (blue)
and `:merged' (green) have rows, and draw their glyph over them.")


;;; The roster is the state -----------------------------------------------
;;
;; EMACS SUBSCRIBES WatchWorkspaceRoster and the row's status arm is the
;; ONE source for tab coloring.  There is no HostWorkspace lifecycle axis to
;; combine it with and no local machine to reconcile it against: the daemon
;; resolves the lifecycle, coarsens it onto this vocabulary, and pushes the
;; whole roster on any change.

(defun agent-repl-status-tab-state (ws)
  "Return WS's tab state: its roster row's status arm keyword, or nil.

nil means the roster has not spoken about WS yet — a tab that exists
locally before its first push, or a workspace with no row.  It is drawn
UNCOLORED rather than guessed at: a colour invented here would be a
second answer to a question only the daemon answers."
  (let ((arm (and (fboundp 'agent-repl-roster-status-for-ws)
                  (agent-repl-roster-status-for-ws ws))))
    (agent-repl--log-verbose ws "elisp.status.tab-state: ws=%s arm=%s" ws arm)
    arm))

(defun agent-repl-status-tab-color (arm)
  "Return the color NAME the tab bar paints ARM with, or \"none\".
Reads `agent-repl-status-tab-bar-color-table', which is the shared
assignment with the tab bar's declared overrides layered over it.  An
arm this build does not know is a contract breach the codec already
refused, so reaching here with one is a programming error and is logged
at ERROR rather than painted."
  (cond
   ((null arm) "none")
   ((alist-get arm agent-repl-status-tab-bar-color-table))
   (t
    (agent-repl--error '(:agent-repl-central "tab rendering and shared assets span workspaces") "elisp.status.tab-color: unknown arm=%S" arm)
    "none")))

(defun agent-repl-status-tab-glyph (ws arm)
  "Return the glyph WS draws for ARM, or nil when it draws none.
Three glyphs exist, in precedence order: the merge pipeline's (most
merge arms carry no lifecycle color, so the glyph is their whole
report, and `:merge-conflict', `:merge-failed' and `:merged' draw
theirs over their green, blue and green), the inactive question mark,
and the attention marker."
  (let ((glyph (or (alist-get arm agent-repl-status-merge-glyphs)
                   (and (eq arm :inactive) agent-repl-status-inactive-glyph)
                   (and (agent-repl-status-attention-visible-p ws)
                        agent-repl-status-attention-glyph))))
    (agent-repl--log-verbose ws "elisp.status.tab-glyph: ws=%s arm=%s glyph=%s"
                             ws arm (or glyph "none"))
    glyph))

;;; The attention marker and its blink ---------------------------------------
;;
;; THE CANONICAL BLINK CADENCE is specified ONCE, on `frontend.v1'
;; `RosterRowAttention' (frontend/v1/sidebar.proto): TWO blinks — 500 ms
;; on, 500 ms off, twice — then a steady marker until cleared.  The webapp
;; sidebar and this tab bar both implement exactly that spec and cite that
;; message; a divergent cadence is a DEFECT, and the consistency requirement
;; is code-level rather than coincidental.
;;
;; The marker's LIFECYCLE is daemon-owned: it is set when a notification
;; fires and cleared when SelectWorkspace names the workspace, both of which
;; arrive as a re-pushed roster.  Emacs times only the blink.

(defconst agent-repl-status-blink-schedule
  '((0.0 . t) (0.5 . nil) (1.0 . t) (1.5 . nil) (2.0 . t))
  "The blink cadence of `frontend.v1' `RosterRowAttention', in seconds.
Marker ON at 0 ms, OFF at 500, ON at 1000, OFF at 1500, and STEADY ON
from 2000 — two blinks of 500 ms on and 500 ms off, then steady.  The
schedule is data so the test can assert the exact instants against the
one spec rather than against a re-reading of the implementation.")

(defvar agent-repl-status--marker-on (make-hash-table :test 'equal)
  "Workspace -> whether its attention marker is drawn right now.")

(defun agent-repl-status--blink-timer-key (ws index)
  "Return the keyed-timer key for WS's blink step INDEX.
Deterministic per workspace and per step, which is what makes a second
`agent-repl-status-blink-tab' RESTART the cadence:
`agent-repl--register-timer' cancels and replaces the timer already
held under the key."
  (intern (format "agent-repl-status-blink-%s-%d" ws index)))

(defun agent-repl-status--set-marker (ws on)
  "Draw or undraw WS's attention marker and repaint the tab bar."
  (puthash ws on agent-repl-status--marker-on)
  (agent-repl--log ws "elisp.status.blink-step: ws=%s marker=%s" ws (if on "on" "off"))
  (agent-repl--force-tab-bar-redraw))

(defun agent-repl-status-attention-visible-p (ws)
  "Return non-nil when WS's attention marker is drawn right now."
  (and (gethash ws agent-repl-status--marker-on) t))

(defun agent-repl-status-blink-tab (ws)
  "Blink WS's tab-bar entry per the canonical cadence, then leave it steady.

IMPLEMENTS `frontend.v1' `RosterRowAttention' EXACTLY, which is where
that cadence is specified once for every surface: two blinks — 500 ms on,
500 ms off, twice — then a steady marker until cleared.  The webapp
sidebar implements the same spec from the same message, and a divergence
between the two is a defect.

Re-entrant: a second call while a blink is in flight RESTARTS the
cadence rather than interleaving with it, because each step is armed
under a deterministic per-workspace key that replaces its predecessor.
The marker is cleared by `agent-repl-status-clear-attention', which the
roster drives when the marker leaves the row."
  (agent-repl--info ws "elisp.status.blink: ws=%s steps=%d"
                    ws (length agent-repl-status-blink-schedule))
  (let ((index 0))
    (dolist (step agent-repl-status-blink-schedule)
      (let ((delay (car step))
            (on (cdr step)))
        (agent-repl--register-timer
         (agent-repl-status--blink-timer-key ws index)
         (run-with-timer delay nil #'agent-repl-status--set-marker ws on)))
      (setq index (1+ index))))
  ws)

(defun agent-repl-status-clear-attention (ws)
  "Clear WS's attention marker and cancel any blink still in flight.
Called when the marker LEAVES THE ROW — the daemon clears it on
SelectWorkspace and re-pushes the roster — so a blink that has not
finished must not paint a marker the daemon has already retracted."
  (let ((index 0))
    (dolist (_step agent-repl-status-blink-schedule)
      (agent-repl--cancel-timer-key (agent-repl-status--blink-timer-key ws index))
      (setq index (1+ index))))
  (if (gethash ws agent-repl-status--marker-on)
      (progn
        (remhash ws agent-repl-status--marker-on)
        (agent-repl--log ws "elisp.status.attention-cleared: ws=%s" ws)
        (agent-repl--force-tab-bar-redraw))
    (agent-repl--log-verbose ws "elisp.status.attention-cleared: ws=%s already-clear" ws)))

(defun agent-repl-status-sync-attention (roster)
  "Follow ROSTER's attention markers.
Blink then steady where a marker is set, cleared where it is not.
Registered on `agent-repl-roster-update-functions'.

A marker that ARRIVES on a row runs the canonical cadence — the cadence
IS blink-then-steady, and `RosterRowAttention' states it at the marker
itself, so the arrival of the marker is what starts it.  A marker that
merely PERSISTS across a re-push is left alone: re-blinking on every
unrelated roster push would blink at the daemon's push rate rather than
at the notification's.  A marker that LEAVES is cleared, cancelling any
blink still in flight."
  (dolist (entry (agent-repl-roster-walk roster))
    (let* ((row (plist-get entry :row))
           (ws (agent-repl--ws-by-ref-id (agent-repl-roster-row-id row))))
      (when ws
        (if (agent-repl-roster-row-attention-p row)
            (unless (agent-repl-status-attention-visible-p ws)
              ;; Marked visible BEFORE the cadence is armed: the first step
              ;; is a timer, so a second push landing before it fires would
              ;; otherwise see no marker and restart the blink.
              (puthash ws t agent-repl-status--marker-on)
              (agent-repl-status-blink-tab ws))
          (agent-repl-status-clear-attention ws))))))

(add-hook 'agent-repl-roster-update-functions #'agent-repl-status-sync-attention)

(defun agent-repl-status-repaint-on-roster-push (_roster)
  "Repaint the tab bar because a roster push just changed what it draws.
Registered on `agent-repl-roster-update-functions'.  The roster is the
tab bar's one source, so the push IS the paint event: this schedules the
redisplay that reads the freshly applied roster, instead of leaving the
bar to the dwell heartbeat's next tick and painting the previous arm
until then.  The render key (`agent-repl--tabline-render-key') is what
makes that redisplay actually reach the pixels when only a face changed."
  (agent-repl--force-tab-bar-redraw))

(add-hook 'agent-repl-roster-update-functions
          #'agent-repl-status-repaint-on-roster-push)

(defun agent-repl--tab-spec (state selected)
  "Return the appearance spec (plist) for STATE with SELECTED flag.
Falls back to `agent-repl--tab-default' when STATE has no palette entry.

The selected look no longer shares its background/foreground with the
unselected one (owner ruling, 2026-09-14): it paints the lightish grey
`agent-repl--color-selected-bg' with a foreground chosen against THAT
grey, in place of the state color, and also carries `:underline', which
`agent-repl--render-tab' turns into the secondary selection marker.
Keys in the returned plist: :bg :fg :bracket-fg :underline :weight."
  (let* ((row (alist-get state agent-repl--tab-palette))
         (key (if selected :selected :unselected)))
    (or (plist-get row key)
        (plist-get (agent-repl--tab-default) key))))

(defun agent-repl--tab-spec-bracket-only (state selected)
  "Return appearance spec applying STATE's color to the [N] bracket only.
Pulls bracket-bg/bracket-fg/weight from STATE's palette row and leaves
:bg/:fg unspecified so the separator and name region inherit defaults.
Used wherever `agent-repl--ws-display-state' suppresses the full-tab
color — that is, whenever the workspace's panels are not open — so the
bracket retains the state's color and the workspace's
state stays visible while the rest of the tab falls back to the default
appearance.

When SELECTED, `agent-repl--tab-spec' already answered with the
selection grey rather than the connection color (owner ruling,
2026-09-14), so a selected panels-closed tab's bracket paints THAT grey,
not the state color — the owner's grey wins over both the connection
color and the panels-closed extent for the selected tab.  The `:underline'
is carried through unchanged as the secondary marker.  The name region's
own grey comes independently from `agent-repl--tab-face', not from this
spec's (unspecified) :bg/:fg."
  (let* ((full (agent-repl--tab-spec state selected))
         (bracket-bg (or (plist-get full :bracket-bg)
                         (plist-get full :bg))))
    `(:bg unspecified
      :fg unspecified
      :bracket-bg ,bracket-bg
      :bracket-fg ,(plist-get full :bracket-fg)
      ,@(when (plist-get full :underline) (list :underline t))
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

(defface agent-repl-tab-unarmed
  `((t :inherit tab-bar
       :weight ,agent-repl--tab-weight))
  "Face for the name of an unselected tab carrying NO state color.

The arms that take no lifecycle color — `:none', `:inactive' and the
terminal merge arms — plus a workspace the roster has not spoken about
yet, and one whose full-tab color is suppressed because its panels are
dismissed.

IT INHERITS `tab-bar', deliberately and only for its background: an
unselected tab is the same color as the bar it sits in, so it takes that
color from the bar itself rather than restating it.

IT DOES NOT INHERIT ITS FOREGROUND, and that is the whole reason this
face exists rather than Doom's `+workspace-tab-face'.  That one inherits
BOTH halves from the frame, which is no pairing at all — measured, it
drew BLACK glyphs on `#14141a', about 1.06:1.  The foreground is chosen
against the bar's measured background by
`agent-repl--tab-bar-legible-fg' and applied over this face by
`agent-repl--tab-face', because no `defface' spec can express \"whichever
of black and white can be read on this bar\".")

(defface agent-repl-tab-merging
  `((t :background ,agent-repl--color-merging-purple
       :foreground ,agent-repl--color-light
       :weight ,agent-repl--tab-weight))
  "Face for workspace tabs with a merge in flight (purple): about to
enqueue, waiting behind a sibling, or running.

There is no `agent-repl-tab-vendor-blocked' face beside this one any
more.  A vendor-blocked workspace paints BLUE on the tab bar, so it
takes `agent-repl-tab-init' exactly as `:severed', `:dead' and
`:degraded' do — every one of them a compromised route to a working
session.")

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
         (agent-repl--status-log-scope
          "frame-wide redraw can run before workspace activation")
         "tabbar-redraw: frame=%S prior-toggle=%S toggle=%S tabs-set-available=%S tab-bar-lines=%S keep-state=%S tab-bar-mode=%S tab-bar-show=%S auto-resize=%S auto-width=%S format=%S"
         frame prior-toggle agent-repl--tabline-space-toggle
         tabs-set-available (frame-parameter frame 'tab-bar-lines)
         (frame-parameter frame 'tab-bar-lines-keep-state)
         tab-bar-mode tab-bar-show auto-resize-tab-bars tab-bar-auto-width
         tab-bar-format)))))

(defun agent-repl--render-tab (name spec label name-face img-str)
  "Render a tab string for workspace NAME from SPEC.
SPEC is a plist with keys :bg :fg :bracket-fg :underline :weight (see
`agent-repl--tab-palette' docstring).  NAME-FACE is applied to the
workspace-name portion.  LABEL is the bracket content (the tab number).
IMG-STR, when non-nil, is the badge run (priority label and glyph)
inserted between bracket and name with a single space on each side so it
does not butt up against the name's background.  A blank IMG-STR is
treated as no badge at all.

EXACTLY ONE SPACE SEPARATES [N] FROM THE NAME, always (owner ruling,
2026-09-13).  The gap is emitted HERE, as one space carrying the name's
own face, and never by `agent-repl-tab-name-padding': that format's
leading whitespace is stripped, so a padding format and a badge run can
no longer each contribute a space and draw `[3]   ws'.  The name itself
is trimmed too, so a daemon-supplied leading space cannot widen the gap
either.  Whatever TRAILING whitespace the padding format adds is a width
FILL and is kept, drawn in the name's face, after the name.

SELECTION IS AN UNDERLINE UNDER THE NAME, AND UNDER NOTHING ELSE.  When
SPEC carries `:underline', exactly the workspace name's own characters are
drawn `:underline t' — not the leading separator, not `[N]', not the space
between them, not the badge run, not the padding fill, not the terminator
(owner ruling, 2026-09-13; the marker used to run under the separator and
the bracket too).  It is a subtle, distinct marker that says which
workspace the user is standing in, secondary to the grey background NAME-FACE
already carries when selected (`agent-repl--tab-face').  `:underline t'
draws in the run's own foreground, so it reads on the selected grey and
on every unselected state color alike, and it is layered OVER NAME-FACE
(a symbol or a list of faces) so the name keeps whatever ground/color
NAME-FACE gave it and gains the marker on top.

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
         (underline  (plist-get spec :underline))
         (separator-face `(:background unspecified :foreground ,fg :weight ,weight))
         (bracket-face   `(:background ,bracket-bg  :foreground ,bracket-fg :weight ,weight))
         (name-face*     (if underline
                             (cons '(:underline t)
                                   (if (listp name-face) name-face (list name-face)))
                           name-face))
         (badge          (and img-str
                              (not (string-blank-p img-str))
                              img-str))
         (padded         (string-trim-left
                          (format agent-repl-tab-name-padding (string-trim name))))
         (text           (string-trim-right padded))
         (fill           (substring padded (length text))))
    (concat (propertize " " 'face separator-face)
            (propertize (format agent-repl-tab-bracket-format label) 'face bracket-face)
            (when badge (concat " " badge))
            (propertize " " 'face name-face)
            (propertize text 'face name-face*)
            (propertize fill 'face name-face)
            " ")))

(defun agent-repl--tab-face (state selected)
  "Return the face for the NAME portion of a tab.

UNSELECTED: for an ARMED tab this is the palette row's `:face' symbol —
the arm's color, untouched.  For an UN-ARMED tab it is
`agent-repl--tab-unarmed-face', a face SPEC rather than a symbol.

SELECTED (owner ruling, 2026-09-14): the name portion paints the SAME
lightish grey (`agent-repl--color-selected-bg') that `agent-repl--tab-spec'
already puts on the bracket and separator, with a foreground
`agent-repl--tab-bar-legible-fg' chooses against that grey — a face SPEC,
for the same reason `agent-repl--tab-unarmed-face' is one: no single
`defface' can state a foreground measured against a fixed background.
Every arm's own color is therefore overridden while selected, armed or
not, so the whole entry (bracket, separator, name) reads as one grey
region; the underline `agent-repl--render-tab' layers on top is the
secondary marker, never the color.

THE UN-ARMED UNSELECTED FALLTHROUGH IS THIS MODULE'S OWN FACE, not Doom's
`+workspace-tab-face'.  Every other row here states a foreground legible
against its background; that one inherited both from the frame, which is
no pairing at all — measured, it drew BLACK glyphs on `#14141a', about
1.06:1."
  (if selected
      (list (list :background agent-repl--color-selected-bg
                   :foreground (agent-repl--tab-bar-legible-fg
                                agent-repl--color-selected-bg)
                   :weight agent-repl--tab-weight))
    (or (plist-get (alist-get state agent-repl--tab-palette) :face)
        (agent-repl--tab-unarmed-face))))

(defun agent-repl--tab-unarmed-face ()
  "Return the face spec for the name of an unselected UN-ARMED tab.

A list of two face references, innermost-wins order: the pair measured
off the bar THIS frame is drawing, then `agent-repl-tab-unarmed' for
everything else the face says (its weight, and whatever a Doom user has
customized on it).

WHY A SPEC AND NOT JUST THE FACE SYMBOL.  The background half of the
pair is the BAR's own background, and the foreground half is chosen
against it — both answers only the live frame can give
\(`agent-repl--tab-bar-background', `agent-repl--tab-bar-legible-fg').  A
`defface' is evaluated once, so it can inherit the bar's background but
cannot state a foreground measured against it; stating the resolved pair
here is what makes an un-armed tab as readable as an armed one on a
themed frame and on the no-theme sandbox alike."
  (let ((bg (agent-repl--tab-bar-background)))
    (list (list :background bg
                :foreground (agent-repl--tab-bar-legible-fg bg))
          'agent-repl-tab-unarmed)))

(defun agent-repl--tab-priority-image-str (name)
  "Return a propertized image string for workspace NAME's priority, or nil."
  (when-let ((priority (agent-repl--ws-get name :priority)))
    (when-let ((img (agent-repl--priority-image priority)))
      (propertize " " 'display img))))

(defun agent-repl--tab-badge-str (name arm)
  "Return the run drawn BEFORE NAME's name region for ARM, or nil.

Two things live there, in this order: the roster's PRIORITY BADGE label
\(resolver-composed and drawn verbatim; ordering is already the
resolver's and this is only the label) and the arm's glyph
\(the merge pipeline's, the inactive question mark, or the attention
marker).  Both are absent far more often than present, so the whole run
is nil in the ordinary case and the tab is name and bracket alone.

A BLANK PART IS NO PART.  A roster row can carry a priority label that is
present but empty, and joining that into the run produced a run made of
nothing but spaces — which the renderer then padded on both sides and drew
as extra gap between `[N]' and the name.  Blank parts are dropped, and a
run left with no parts is nil."
  (let* ((row (and (fboundp 'agent-repl-roster-row-for-ws)
                   (agent-repl-roster-row-for-ws name)))
         (badge (and row (agent-repl-roster-row-priority-label row)))
         (glyph (agent-repl-status-tab-glyph name arm))
         (parts (seq-remove #'string-blank-p
                            (delq nil (list badge glyph)))))
    (when parts
      (string-join parts " "))))

;;; The tab-bar repaint heartbeat
;;
;; THERE IS NO READY-VIEW FADE ANY MORE.  A `:ready' workspace used to
;; paint its whole tab green until the user had stood in it for a couple
;; of seconds, after which a local latch dropped it to the bracket-only
;; paint.  Owner ruling 5 (2026-09-13) states the extent rule outright:
;; a tab is FULL if and only if that workspace's agent-repl panels are
;; open, and PARTIAL if and only if they are not.  A dwell latch made a
;; panels-OPEN workspace draw partial, which is the one thing the rule
;; forbids, so the latch, its dwell clock, its state-transition clear and
;; its fade-delay knob are all gone.
;;
;; What survives is the heartbeat they rode on, because the tab bar needs
;; something to repaint it on a clock (see
;; `agent-repl--force-tab-bar-redraw').  It polls nothing and writes no
;; state.

(defconst agent-repl--tab-repaint-interval-seconds 2.0
  "Seconds between tab-bar repaint heartbeats.
Inherited unchanged from the ready-view fade delay this heartbeat used to
pace, so the repaint cadence is exactly what it has always been.")


(defvar agent-repl--tab-background-modes (make-hash-table :test 'equal)
  "Workspace -> the tab background mode (`:full' or `:partial') last drawn.
Only `agent-repl--note-tab-background-mode' reads or writes it, and only
so a FLIP can be recorded once instead of on every redisplay.")

(defun agent-repl--note-tab-background-mode (ws mode reason)
  "Record that WS's tab background is now MODE, because of REASON.
MODE is `:full' (the whole `[N] <name>' entry carries the status color)
or `:partial' (only `[N]' does).  Writes a DEBUG record the first time a
workspace resolves to a mode and on every flip afterwards, and nothing at
all while the mode holds — this runs inside tab-bar redisplay, which is
one of the hottest paths in the module.

An extent that changes with no visible cause is exactly the invisible
action the module's logging rule forbids, so the flip says which mode it
went to and which fact decided it."
  (let ((previous (gethash ws agent-repl--tab-background-modes 'none)))
    (unless (eq previous mode)
      (puthash ws mode agent-repl--tab-background-modes)
      (agent-repl--log-verbose
       ws "tab-background: ws=%s mode=%s previous=%s reason=%s"
       ws mode previous reason))))

;;; The view-dwell demotion --------------------------------------------------
;;
;; RE-INTRODUCED, GENERALIZED, from the ready-view dwell latch removed in
;; commit abc0ee3c3 ("the tab background extent is the panels-open fact, and
;; only that").  That latch demoted ONLY a `:ready' workspace's tab from full
;; to bracket-only after a couple of seconds of viewing, and owner ruling 5
;; (2026-09-13) struck it because green-into-green changed a color without
;; changing anything true.  Owner's NEW ruling (2026-09-15) reinstates the
;; demotion for EVERY state, as a plain "you've seen it" signal: once a
;; workspace's panels have been VIEWED for at least
;; `agent-repl-tab-dwell-demote-seconds', its tab drops from FULL to PARTIAL
;; (only `[N]' keeps the status color) even with the panels open.  It is NOT
;; the status going away — the bracket still carries it — and it RESETS to
;; FULL on the next STATUS UPDATE for that workspace (new activity), which
;; re-arms the dwell.
;;
;; Unlike the old apparatus, demotion is a LATCH (a per-workspace flag), not
;; a time comparison performed inside `agent-repl--ws-display-state': that
;; function runs on the hottest redisplay path (see
;; `agent-repl--note-tab-background-mode'), so it reads a boolean and never
;; calls the clock.  The clock lives in `agent-repl--tab-dwell-note', which
;; the dwell timer (and the repaint heartbeat) call to SET the latch once the
;; armed-at stamp is old enough.

(defconst agent-repl-tab-dwell-demote-seconds 5
  "Seconds a workspace's panels must be VIEWED before its tab demotes.
After this much continuous viewing of a panels-open, currently-viewed
workspace, `agent-repl--ws-display-state' draws it PARTIAL (only the
`[N]' bracket keeps the status color) instead of FULL — the \"you've
seen it\" demotion.  The next status update for the workspace clears the
demotion and re-arms this dwell.")

(defvar agent-repl--tab-dwell-armed-at (make-hash-table :test 'equal)
  "Workspace -> the time its current 5-second view dwell was armed.
Stamped when the workspace is activated (viewing begins) and re-stamped
when a status update resets the demotion.  `agent-repl--tab-dwell-note'
measures the dwell from here.")

(defvar agent-repl--tab-dwell-timer nil
  "The single pending one-shot timer that demotes the current workspace.
Only the currently-viewed workspace accrues a dwell, so one timer
suffices: (re)arming cancels the previous one.  It fires at the dwell
deadline so the demotion repaints then, rather than waiting for the next
heartbeat tick.")

(defun agent-repl--tab-dwell-demoted-p (ws)
  "Return non-nil when WS's tab is view-demoted to PARTIAL.
The daemon is the single source: the decoded roster row's `:viewed'
marker (`agent-repl-roster-viewed-for-ws') is the ONLY input."
  (and (agent-repl-roster-viewed-for-ws ws) t))

(defun agent-repl--ws-display-state (ws)
  "Return the palette display key for WS.
Delegates to `agent-repl--ws-render-status' (the single source of
truth for visual state across the tab-bar and project picker), then
layers TWO suppressions on top, each of which hands the tab to the
bracket-only appearance (`agent-repl--tab-spec-bracket-only' plus the
default name face):

- Panel visibility — when the render-state is non-nil AND WS's
  agent-repl panels are not both open in its live-or-saved window
  layout (`agent-repl--ws-agent-open-p'), returns nil regardless of
  state, suppressing the full-tab color for a workspace whose panels
  the user has dismissed.

- View dwell — when the panels ARE open but the workspace has been
  view-demoted (`agent-repl--tab-dwell-demoted-p'), returns nil so the
  tab draws PARTIAL: the \"you've seen it\" demotion that fires after
  `agent-repl-tab-dwell-demote-seconds' of viewing and resets to FULL on
  the next status update.  See the view-dwell section above.

THE PANEL-VISIBILITY EXTENT RULE IS AN IF AND ONLY IF (owner ruling 5,
2026-09-13): a panels-CLOSED tab is always PARTIAL and a panels-OPEN tab
is FULL unless the view dwell has demoted it (owner ruling, 2026-09-15).
The ORIGINAL ready-view dwell latch that ruling 5 removed demoted only a
panels-OPEN `:ready' workspace; the dwell here is its generalized
successor — it applies to every state and is reset by new activity
rather than by a state transition.

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
      (cond
       ((null state) nil)
       ((not (agent-repl--ws-agent-open-p ws))
        (agent-repl--note-tab-background-mode ws :partial "panels-closed")
        nil)
       ((agent-repl--tab-dwell-demoted-p ws)
        (agent-repl--note-tab-background-mode ws :partial "dwell-demoted")
        nil)
       (t
        (agent-repl--note-tab-background-mode ws :full "panels-open")
        state)))))

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
CURRENT-NAME is the active workspace name.  INDEX is the 1-based tab
position.  The display state (from `agent-repl--ws-display-state') drives
the name face.  The appearance spec is resolved via `agent-repl--tab-spec'
when display-state is non-nil; when display-state is nil but
`agent-repl--ws-bracket-state' returns an arm (panels dismissed for a
workspace the roster does report on, or a `:ready' workspace whose
panels are not open), the spec is built via
`agent-repl--tab-spec-bracket-only' so only the [N] bracket keeps the
arm's color.  The bracket label is the tab's 1-based INDEX and nothing
else: state reaches the bracket as COLOR.

The badge run — the roster's priority label and the arm's glyph — is
drawn BEFORE the name."
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
         (badge         (agent-repl--tab-badge-str
                         name (or display-state bracket-state))))
    (agent-repl--render-tab name spec label face badge)))

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
             (agent-repl--status-log-scope
              "tab-row truncation can run before workspace activation")
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
    (agent-repl--log-verbose '(:agent-repl-central "tab rendering and shared assets span workspaces") "tabline-advice: current=%s states=%S"
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
       (agent-repl--status-log-scope
        "tab-bar rendering can run before workspace activation")
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
Appends two zero-width runs: the render key
\(`agent-repl--tabline-render-key'), which changes the string's CONTENT
exactly when the rows change in any way including their faces, so a
face-only transition (e.g. :thinking -> :done) is a different string
to the tab bar's `equal'-keyed items cache and is repainted at the very
next redisplay; and the clock-driven cache-buster
\(`agent-repl--tabline-cache-buster').

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
         ;; The render key LEADS the rows: an invisible run at the front
         ;; of the first row leaves the last row's own characters exactly
         ;; as the padding and join produced them.
         (output (concat (agent-repl--tabline-render-key joined (selected-frame))
                         joined
                         (agent-repl--tabline-cache-buster))))
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
       (agent-repl--status-log-scope
        "the frame-wide keymap boundary can run before workspace activation")
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
     (agent-repl--status-log-scope
      "a frame parameter mutation can run before workspace activation")
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
        ;; Quoted, not `#'': the watchdog is DELETED.  Its name survives
        ;; only as the hook entry an older live process still carries, so
        ;; there is deliberately no definition for `#'' to point at.
        (remove-function pre-redisplay-function
                         'agent-repl--redisplay-storm-watchdog)
        (setq hook-present (not (eq prior-hook pre-redisplay-function)))))
    (list :timer-cancelled timer-cancelled :hook-present hook-present)))

(defun agent-repl--tabbar-pin-frame (frame rows)
  "Pin FRAME's tab bar to ROWS and preserve that explicit line count.

This is status.el's frame-parameter integration boundary.  It sets
`tab-bar-lines-keep-state' before `tab-bar-lines', preventing Emacs's
native one-line recalculation from overwriting the fixed agent-repl
height during later tab operations.

On Emacs 30.2's NS backend, the native frame-parameter setter deliberately
ignores nonzero-to-nonzero changes: changing the Lisp parameter from one
line to two leaves the native tab-bar window at one physical line.  Force
the supported zero-to-ROWS transition on NS so its
`ns_change_tab_bar_height' path actually updates native geometry.  Other
display backends receive the direct ROWS assignment.

`frame-inhibit-implied-resize' must already contain `tab-bar-lines' so
the height change is absorbed by the frame text area rather than resizing
the outer window.  Returns ROWS after logging the complete transition."
  (let* ((display-type (framep-on-display frame))
         (native-zero-transition-p
          (and (eq display-type 'ns) (> rows 0)))
         (prior-lines (frame-parameter frame 'tab-bar-lines))
         (prior-keep-state
          (frame-parameter frame 'tab-bar-lines-keep-state))
         (ws (agent-repl--status-log-scope
              "frame-height installation can run before workspace activation"))
         (stage 'keep-state)
         (zero-lines 'not-requested))
    (condition-case error-data
        (let ((agent-repl--tabbar-frame-parameter-audit-active t))
          (set-frame-parameter frame 'tab-bar-lines-keep-state t)
          (when native-zero-transition-p
            (setq stage 'native-zero)
            (set-frame-parameter frame 'tab-bar-lines 0)
            (setq zero-lines (frame-parameter frame 'tab-bar-lines)))
          (setq stage 'target)
          (set-frame-parameter frame 'tab-bar-lines rows)
          (let ((final-lines (frame-parameter frame 'tab-bar-lines))
                (final-keep-state
                 (frame-parameter frame 'tab-bar-lines-keep-state)))
            (agent-repl--log
             ws
             "tabbar-pin-frame: outcome=installed frame=%S display-type=%S rows=%d native-zero-transition=%s prior-lines=%S zero-lines=%S final-lines=%S prior-keep-state=%S final-keep-state=%S inhibit-implied-resize=%S"
             frame display-type rows native-zero-transition-p prior-lines
             zero-lines final-lines prior-keep-state final-keep-state
             frame-inhibit-implied-resize)
            rows))
      (error
       (agent-repl--log
        ws
        "tabbar-pin-frame: outcome=error frame=%S display-type=%S rows=%d native-zero-transition=%s stage=%S prior-lines=%S zero-lines=%S current-lines=%S prior-keep-state=%S current-keep-state=%S inhibit-implied-resize=%S err=%S"
        frame display-type rows native-zero-transition-p stage prior-lines
        zero-lines (frame-parameter frame 'tab-bar-lines)
        prior-keep-state
        (frame-parameter frame 'tab-bar-lines-keep-state)
        frame-inhibit-implied-resize error-data)
       (signal (car error-data) (cdr error-data))))))

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
     (agent-repl--status-log-scope
      "manual frame-height repair can run outside a workspace")
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

Finally registers `agent-repl--tabbar-reassert-row-count' on
`window-setup-hook' and `persp-activated-functions', because the pin
above does not survive on its own: see that function for which resets
undo it and why hook ordering, not advice, is the fix.

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
    ;; Both hooks are APPENDED, and both registrations happen after
    ;; `tab-bar-mode' above: that is what puts the re-assertion after Doom's
    ;; `+workspaces-load-tab-bar-data-h' (which `tab-bar-mode-hook' installs
    ;; on `persp-activated-functions') and after `frame-notice-user-settings'
    ;; (which `command-line' runs immediately before `window-setup-hook').
    ;; See `agent-repl--tabbar-reassert-row-count' for the measured cause.
    (add-hook 'window-setup-hook #'agent-repl--tabbar-reassert-row-count t)
    (add-hook 'persp-activated-functions
              #'agent-repl--tabbar-reassert-row-count t)
    (agent-repl--log
     (agent-repl--status-log-scope
      "tab-bar installation runs before workspace activation")
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

(defun agent-repl--tabbar-reassert-row-count (&rest _)
  "Re-pin the fixed two-row tab-bar contract in both scopes Emacs reads.

Two resets undo `agent-repl--install-fixed-height-tab-bar' after it has
already run, and neither is stopped by `tab-bar-lines-keep-state':

- Doom's `+workspaces-load-tab-bar-data-h' (on `persp-activated-functions')
  ends with `(tab-bar--update-tab-bar-lines t)'.  In Emacs 30.2 that
  function honors `tab-bar-lines-keep-state' only for the per-frame
  parameter; the `frames' = t branch afterwards rewrites
  `default-frame-alist' unconditionally, putting `(tab-bar-lines . 1)'
  back.  Every frame created after a workspace switch therefore starts
  one row tall — and inherits the alist's `tab-bar-lines-keep-state' t,
  which then LOCKS it at one row.

- At startup `frame-notice-user-settings' applies `default-frame-alist'
  to the initial frame, so an alist already clobbered to 1 (by
  `tab-bar-mode' itself, or by the reset above) sizes the initial frame
  to one row.  The bar stays one row until something else repaints it.

Both resets are followed by a hook, so no advice on the private
`tab-bar--update-tab-bar-lines' is needed: `window-setup-hook' runs
immediately after `frame-notice-user-settings' in `command-line', and
appending to `persp-activated-functions' places this after Doom's
handler.  `agent-repl--install-fixed-height-tab-bar' registers both.

Repair is conditional: a scope already carrying the contract is left
untouched, so a workspace switch does not push a correct frame through
`agent-repl--tabbar-pin-frame''s NS zero-transition on every activation.
Returns the list of graphical frames that were actually re-pinned."
  (let* ((rows agent-repl--tabline-row-count)
         (prior-default-lines (alist-get 'tab-bar-lines default-frame-alist))
         (prior-default-keep-state
          (alist-get 'tab-bar-lines-keep-state default-frame-alist))
         (default-repaired
          (not (and (equal prior-default-lines rows)
                    (eq t prior-default-keep-state))))
         (repinned nil))
    (when default-repaired
      (setf (alist-get 'tab-bar-lines default-frame-alist) rows
            (alist-get 'tab-bar-lines-keep-state default-frame-alist) t))
    (dolist (frame (cl-remove-if-not #'display-graphic-p (frame-list)))
      (unless (and (equal (frame-parameter frame 'tab-bar-lines) rows)
                   (eq t (frame-parameter frame 'tab-bar-lines-keep-state)))
        (agent-repl--tabbar-pin-frame frame rows)
        (push frame repinned)))
    (setq repinned (nreverse repinned))
    (when (or default-repaired repinned)
      (agent-repl--log
       (agent-repl--status-log-scope
        "frame-wide tab-bar repair can run before workspace activation")
       "tabbar-reassert: rows=%d default-repaired=%s prior-default-lines=%S prior-default-keep-state=%S repinned=%S"
       rows default-repaired prior-default-lines prior-default-keep-state
       repinned))
    repinned))

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
call (e.g. the `Deleted '<ws>' workspace' notification Doom's
`+workspace/kill' emits) briefly flashes the full workspaces tabline in the
minibuffer.

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
(defun agent-repl--wconf-has-buffer-p (wconf name-p)
  "Return non-nil if WCONF (a `window-state-get' tree) shows a NAME-P buffer.
NAME-P is a predicate over a buffer NAME, because a saved window state
carries buffers only as their names."
  (when (and wconf (proper-list-p wconf))
    (let ((buf-entry (alist-get 'buffer wconf)))
      (if (and buf-entry (funcall name-p (car-safe buf-entry)))
          t
        (cl-some (lambda (child) (agent-repl--wconf-has-buffer-p child name-p))
                 (cl-remove-if-not #'proper-list-p wconf))))))

(defun agent-repl--wconf-has-agent-p (wconf)
  "Return non-nil if WCONF (a `window-state-get' tree) shows BOTH agent panels.
A workspace's agent-repl PANELS are the webapp panel
\(`agent-repl--agent-view-buffer-name-p') and the input window
\(`agent-repl--agent-input-buffer-name-p'), and this answers the question
the tab bar's extent rule asks: are they open?

BOTH, not either.  Owner ruling 5 (2026-09-13) states the full background
belongs to a workspace whose webapp panel AND input window are open; this
predicate used to accept the view alone, so a layout carrying the webapp
panel with the composer dismissed drew a full tab the rule calls partial.
A layout carrying only the input panel still does not count, exactly as
before."
  (and (agent-repl--wconf-has-buffer-p
        wconf #'agent-repl--agent-view-buffer-name-p)
       (agent-repl--wconf-has-buffer-p
        wconf #'agent-repl--agent-input-buffer-name-p)))

(defun agent-repl--visible-agent-buffer-p (buf)
  "Return non-nil if BUF is a live, visible agent VIEW buffer.
The view is the webview buffer — see `agent-repl--agent-view-buffer-p'."
  (and (buffer-live-p buf)
       (agent-repl--agent-view-buffer-p buf)
       (get-buffer-window buf)))

(defun agent-repl--visible-input-buffer-p (buf)
  "Return non-nil if BUF is a live, visible agent INPUT composer buffer."
  (and (buffer-live-p buf)
       (agent-repl--agent-input-buffer-p buf)
       (get-buffer-window buf)))

(defun agent-repl--agent-visible-in-current-ws-p ()
  "Return non-nil if BOTH agent panels are visible in the current workspace.
The webapp panel and the input window, per owner ruling 5 — the live-frame
half of `agent-repl--wconf-has-agent-p'."
  (and (cl-some #'agent-repl--visible-agent-buffer-p (buffer-list))
       (cl-some #'agent-repl--visible-input-buffer-p (buffer-list))))

(defun agent-repl--agent-in-saved-wconf-p (ws-name)
  "Return non-nil if background workspace WS-NAME has an agent buffer in
its saved config."
  (let* ((persp (agent-repl--ws-resolve-persp ws-name))
         (wconf (agent-repl--ws-window-conf persp)))
    (agent-repl--wconf-has-agent-p wconf)))

(defun agent-repl--ws-agent-open-p (ws-name)
  "Return non-nil if workspace WS-NAME has BOTH agent panels in its layout.
The webapp panel and the input window (owner ruling 5, 2026-09-13).  This
is the panels-open fact the tab bar's full-vs-partial background keys on,
and the only fact it keys on.
For the current workspace, checks live windows.
For background workspaces, inspects the saved persp window configuration."
  (if (equal ws-name (agent-repl--ws-current-name))
      (agent-repl--agent-visible-in-current-ws-p)
    (agent-repl--agent-in-saved-wconf-p ws-name)))


;;; The heartbeat ------------------------------------------------------------
;;
;; THE LOCAL STATE MACHINE IS GONE.  A workspace's colour is the roster's
;; status arm and nothing else: no poll of the process table, no git ticks,
;; no spread window, no staleness threshold, no `:agent-state' / `:repl-state'
;; axes, no local death detection.  The daemon resolves every workspace's
;; lifecycle and pushes the whole roster on any change (event-driven, whole
;; view, no ticks), so a local re-derivation could only ever be a second,
;; drifting answer to a question already answered.
;;
;; TWO local clocks survive, and neither derives lifecycle: the heartbeat
;; below repaints the tab bar on a fixed cadence, and the view-dwell timer
;; (`agent-repl--tab-dwell-timer', armed in the view-dwell section above)
;; fires once at the demotion deadline to repaint the "you've seen it" flip.
;; Both are LOCAL PRESENTATION modifiers (blessed as such): they read no
;; daemon state, fork nothing, and touch no workspace plist.

;;; The view-dwell mechanism (timers, arming, reset) ------------------------
;;
;; Declared here, alongside the heartbeat, because both are the module's
;; local presentation clocks; the latch and its reader live in the
;; view-dwell section next to `agent-repl--ws-display-state', which is the
;; one place that reads them.

(defun agent-repl--tab-dwell-cancel-timer ()
  "Cancel the pending view-dwell demotion timer, if any."
  (when (timerp agent-repl--tab-dwell-timer)
    (cancel-timer agent-repl--tab-dwell-timer))
  (setq agent-repl--tab-dwell-timer nil))

(defun agent-repl--tab-dwell-eligible-p (ws)
  "Return non-nil when WS can still accrue a view dwell.
That is: WS is the currently-viewed workspace, its panels are open, and
it has not already been demoted.  A workspace nobody is looking at is not
being viewed, and a panels-closed tab is PARTIAL already, so neither
dwells."
  (and ws
       (agent-repl--ws-known-p ws)
       (equal ws (agent-repl--ws-current-name))
       (agent-repl--ws-agent-open-p ws)
       (not (agent-repl--tab-dwell-demoted-p ws))))

(defun agent-repl--tab-dwell-note (ws now)
  "Demote WS's tab if its view dwell has elapsed as of NOW.
NOW is a time value (`current-time' in production; injected in tests).
This function is the CLOCK and nothing else: it decides whether the dwell
has elapsed for an eligible workspace (`agent-repl--tab-dwell-eligible-p')
and hands the verdict to `agent-repl--tab-view-partial', which is what
applies PARTIAL everywhere it has to be applied.  Returns non-nil when it
demoted.

It reads the injected clock, never `current-time', so the dwell can be
exercised without sleeping."
  (let ((armed-at (gethash ws agent-repl--tab-dwell-armed-at)))
    (when (and (agent-repl--tab-dwell-eligible-p ws)
               armed-at
               (>= (float-time (time-subtract now armed-at))
                   agent-repl-tab-dwell-demote-seconds))
      (agent-repl--tab-view-partial ws "dwell")
      t)))

(defun agent-repl--tab-dwell-fire (ws)
  "Timer callback: try to demote WS now that its dwell deadline arrived."
  (agent-repl--tab-dwell-note ws (current-time)))

(defun agent-repl--tab-dwell-arm (ws now)
  "Arm WS's view dwell as of NOW: stamp armed-at and schedule the demotion.
Stamps `agent-repl--tab-dwell-armed-at' and, when WS is still eligible,
(re)schedules the single one-shot demotion timer for
`agent-repl-tab-dwell-demote-seconds' out so the flip repaints at the
deadline.  Does NOT clear the demotion latch — only a status update does
that (`agent-repl--tab-view-restore-full').  A workspace already demoted, or
whose panels are closed, or that is not current, gets its stamp but no
timer."
  (puthash ws now agent-repl--tab-dwell-armed-at)
  (agent-repl--tab-dwell-cancel-timer)
  (when (agent-repl--tab-dwell-eligible-p ws)
    (setq agent-repl--tab-dwell-timer
          (run-with-timer agent-repl-tab-dwell-demote-seconds nil
                          #'agent-repl--tab-dwell-fire ws))))

(defun agent-repl--tab-view-partial (ws reason)
  "Draw WS PARTIAL everywhere, because REASON says the user has seen it.

THE ONE PLACE STALENESS IS APPLIED.  Whatever detects it — today the view
dwell, tomorrow anything else — calls exactly this, and it does BOTH
halves of applying the mode:

  - it REPORTS the workspace to the daemon (`agent-repl-host-mark-viewed'),
    which raises the row's viewed marker and re-pushes the roster; and
  - it forces a tab-bar repaint.

It latches NOTHING locally.  The daemon is the single source of the mode:
the tab bar and the webapp sidebar both render it from the roster row's
`:viewed' marker, so the tab flips one roster round trip after the
report (ruled acceptable) and the two surfaces can never disagree.

There is no matching notification on the RESTORE — see
`agent-repl--tab-view-restore-full'."
  (agent-repl--log
   ws
   "tab-view: PARTIAL ws=%s reason=%s — the tab name falls back to default and [N] keeps the status color; the sidebar row's name greys"
   ws reason)
  (agent-repl--force-tab-bar-redraw)
  (agent-repl-host-mark-viewed ws))

(defun agent-repl--tab-view-restore-full (ws now)
  "Re-arm WS's dwell as of NOW, because the daemon cleared its viewed marker.

THE ONE PLACE THE RESTORE IS REACTED TO.  The tab already draws FULL:
the row it renders from no longer carries the marker.  This only re-arms
the dwell (`agent-repl--tab-dwell-arm'), so the clock restarts from the
new activity rather than from whenever the workspace was last activated.

IT NOTIFIES THE DAEMON OF NOTHING, deliberately.  The daemon ORIGINATED
the status change that brought us here — every status Emacs knows arrives
on the roster stream — and it clears the row's viewed marker on that same
edge, in that same push.  A report back would be Emacs telling the daemon
a fact the daemon has just told Emacs.

The daemon drops the marker whenever the row leaves a read turn-end state,
from any origin."
  (agent-repl--tab-dwell-arm ws now))

(defun agent-repl--tab-dwell-on-activation (&rest _)
  "Arm the view dwell for the workspace just activated.
Registered on the persp activation hook, alongside
`agent-repl--record-workspace-history' (which stamps `:last-viewed-at').
Switching INTO a workspace restarts the continuous-viewing clock; it does
not un-demote a workspace that was already demoted with no new activity.

Takes and ignores its arguments (`&rest _'): `persp-activated-functions'
invokes each registered function WITH the activation type (e.g. `frame' or
`window'), so a nil arglist signalled \"Wrong number of arguments\" on every
persp switch and aborted the activation hook run.  It mirrors its sibling
`agent-repl--record-workspace-history', which is on the same hook for the
same reason."
  (let ((ws (agent-repl--ws-current-name)))
    (when (and ws (agent-repl--ws-known-p ws))
      (agent-repl--tab-dwell-arm ws (current-time)))))

(agent-repl--ws-add-activated-hook #'agent-repl--tab-dwell-on-activation)

(defun agent-repl--tab-view-restore-on-viewed-cleared (ws)
  "React to the daemon clearing WS's viewed marker: restore FULL.
Registered on `agent-repl-roster-viewed-cleared-functions', which fires
once per row whose marker went present->absent.  The daemon drops the
marker on any change that leaves a read turn-end state, from any origin,
so this reaction never has to enumerate origins."
  (agent-repl--log ws "tab-view: FULL ws=%s reason=viewed-cleared" ws)
  (agent-repl--tab-view-restore-full ws (current-time)))

(add-hook 'agent-repl-roster-viewed-cleared-functions
          #'agent-repl--tab-view-restore-on-viewed-cleared)

(defun agent-repl--tab-dwell-rearm-on-status-change (ws previous current)
  "Re-arm WS's view dwell because its status moved from PREVIOUS to CURRENT.
Registered on `agent-repl-roster-status-change-functions'.

The daemon takes a dwell on a turn-end row (done, interrupted or
turn-failed) and on a vendor-blocked one, whose failed turn it reads;
one reported while WS is thinking, waiting, severed or anything else is
dropped, so there is no marker for the viewed-cleared edge to announce later.
Without this, a user who watched a turn run to done would have spent the
one-shot dwell on the running turn, and the finished response would never go
PARTIAL.  Every status change therefore restarts the clock, and the next
report lands on whatever the row has become; whether it takes is the
daemon's decision, never this one's."
  (agent-repl--log ws "tab-view: dwell re-armed ws=%s reason=status-change from=%s to=%s"
                   ws previous current)
  (agent-repl--tab-dwell-arm ws (current-time)))

(add-hook 'agent-repl-roster-status-change-functions
          #'agent-repl--tab-dwell-rearm-on-status-change)

(defun agent-repl--arm-state-poll-timer ()
  "Arm the tab-bar repaint heartbeat under the `:state-poll' key.
Idempotent: `agent-repl--register-timer' cancels and replaces any timer
already held under the key, so any number of re-loads of this file leave
exactly one heartbeat running.  Returns the timer.

The key keeps its name because core.el's `agent-repl--required-timer-keys'
names the JOB the tab bar depends on — a heartbeat that repaints it —
and that job survives even though everything it used to poll does not."
  (agent-repl--register-timer
   :state-poll
   (run-with-timer agent-repl--tab-repaint-interval-seconds
                   agent-repl--tab-repaint-interval-seconds
                   #'agent-repl--status-dwell-tick)))

(defun agent-repl--status-dwell-tick ()
  "Repaint the tab bar.
The whole heartbeat: no workspace is polled and no state is written.
The name is unchanged because `agent-repl--required-timer-keys' and the
suites name this function; the dwell it used to latch is gone with the
ready-view fade."
  (agent-repl--force-tab-bar-redraw))

(agent-repl--arm-state-poll-timer)

;;; Frame focus handler -------------------------------------------------------

(defun agent-repl--on-frame-focus ()
  "Repaint the tab bar when Emacs regains focus.
There is nothing to refresh: the roster stream pushed every workspace's
state while the frame was unfocused, and the tab bar simply has to draw
what already arrived."
  (if (frame-focus-state)
      (progn
        (agent-repl--log
         (agent-repl--status-log-scope
          "frame focus can change before workspace activation")
         "elisp.status.frame-focus: focused")
        (agent-repl--force-tab-bar-redraw))
    (agent-repl--log-verbose
     (agent-repl--status-log-scope
      "frame focus can change before workspace activation")
     "elisp.status.frame-focus: not focused")))

(add-function :after after-focus-change-function #'agent-repl--on-frame-focus)

(defun agent-repl--repaint-tab-on-panel-change ()
  "Repaint the tab bar when the current workspace's panels open or close.
The tab's background EXTENT is the panels-open fact
\(`agent-repl--ws-agent-open-p'), so opening or dismissing a panel changes
what the tab should draw with nothing else to announce it.  The repaint
heartbeat would get there within its interval; this makes the flip land on
the redisplay that caused it.

It fires only on an actual flip: the mode the renderer last drew is
recorded (`agent-repl--tab-background-modes'), and a window-configuration
change that leaves it alone costs one hash lookup.  A workspace the
renderer has not resolved a mode for yet has nothing to flip."
  (let ((ws (agent-repl--ws-current-name)))
    (when (and ws (agent-repl--ws-known-p ws))
      (let ((recorded (gethash ws agent-repl--tab-background-modes))
            (mode (if (agent-repl--ws-agent-open-p ws) :full :partial)))
        (when (and recorded (not (eq recorded mode)))
          (agent-repl--force-tab-bar-redraw))))))

(add-hook 'window-configuration-change-hook
          #'agent-repl--repaint-tab-on-panel-change)


(provide 'agent-repl-status)
;;; status.el ends here
