;;; drawer.el --- Workspace drawer side-window for agent-repl -*- lexical-binding: t; -*-

;;; Commentary:

;; Read-only side-window listing every agent-repl workspace.  Lives "above"
;; the workspaces themselves: it is rendered into a left-side slot and is
;; not tied to any single workspace's window configuration.
;;
;; Each line shows: priority, agent-state icon, workspace name, and the
;; rendered aiTitle (`:last-prompt-summary').  Hidden workspaces appear
;; below a separator at the bottom.
;;
;; This is the read-only first cut: navigation (n/p) and RET-to-switch
;; only.  Sending prompts, interrupting, toggling hidden, etc. are
;; intentionally deferred.

;;; Code:

(require 'subr-x)

;;;; Customization ----------------------------------------------------------

(defcustom agent-repl-drawer-buffer-name "*agent-repl-drawer*"
  "Buffer name for the agent-repl workspace drawer."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-drawer-width-fraction 0.243
  "Fraction of the frame width the drawer should occupy.
Computed against `frame-width' so the drawer scales with the frame.
Capped at 24.3% by default — the drawer is meant to stay open during
work, not dominate the layout.

This is the SOLE determinant of drawer width: the width is always
`round(agent-repl-drawer-width-fraction × frame-width)' and is
constant at runtime.  There is no manual-resize override and no
depth-based adjustment, so the only thing that ever changes the
drawer width is editing this fraction (or resizing the frame
itself)."
  :type 'float
  :group 'agent-repl)

(defcustom agent-repl-drawer-indent-per-level 2
  "Columns to indent each nesting level in the drawer.
Read by the render to build the per-depth indent string.  Does NOT
affect the drawer's window width, which is constant (see
`agent-repl-drawer-width-fraction')."
  :type 'integer
  :group 'agent-repl)

;; Force-apply on reload — defcustom only initializes for unbound
;; symbols, so source tweaks otherwise need a full Emacs restart.
(setq agent-repl-drawer-width-fraction
      (eval (car (get 'agent-repl-drawer-width-fraction 'standard-value))))

(defcustom agent-repl-drawer-state-icons
  '((:init           . "⏳")
    (:thinking       . "⌛")
    (:done           . "✅")
    (:idle           . "💤")
    (:permission     . "❓")
    (:stop-failed    . "❗")
    (:start-failed   . "🚫")
    (:dead           . "❌")
    (:merged         . "🔀")
    (:merge-failed   . "⛔")
    (:merge-conflict . "💥")
    (:merging        . "🔄")
    (:merge-queued   . "🕒"))
  "Alist mapping agent-state keyword to an indicator glyph.
The :dead entry is used when `:repl-state' is `:dead' (overrides
:agent-state).  The :merged entry is used when `:repl-state' is
`:merged' and takes precedence over `:dead' (so a merged workspace
whose session has since died still reads as merged).  The :merge-failed
entry is used when `:repl-state' is `:merge-failed' (a workspace that
landed in the MERGED bucket but whose cherry-pick reported failure);
it uses the ⛔ glyph (distinct from :dead's ❌) to signal a blocked
merge — typically the source repo is mid cherry-pick/rebase/merge.
The :merge-queued
entry is used when `:repl-state' is `:merge-queued' (a merge request
parked on `agent-repl--merge-queue' waiting for a live cherry-pick
to finish); it routes under MERGING alongside in-flight merges.
Unrecognized values fall through to a single middot placeholder, used
for workspaces registered but with no live session."
  :type '(alist :key-type symbol :value-type string)
  :group 'agent-repl)

;; Force-apply the latest palette on every (re)load.  `defcustom' only
;; initializes the value when the symbol is unbound, so palette tweaks
;; otherwise require an Emacs restart to take effect.  Source is the
;; canonical palette in this personal config; `M-x customize' values
;; for this variable will be overwritten on reload.
(setq agent-repl-drawer-state-icons
      (eval (car (get 'agent-repl-drawer-state-icons 'standard-value))))

(defcustom agent-repl-drawer-state-icon-default "·"
  "Glyph shown when a workspace has no recognized agent-state.
Used for registered-but-not-yet-started workspaces (agent-state nil)."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-drawer-section-rule-width 12
  "Number of `─' characters in section header rule lines."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-drawer-empty-section-label "(none)"
  "Placeholder shown under a section header when the section has no entries."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-drawer-group-label-format "%s\n"
  "Format string for the repo group label text in drawer sections.
Within a section, entries are partitioned by their workspace's git
common-dir (each top-level repo / its worktree set is one group).
Groups are separated by a blank line and labeled with this format.

The label is rendered AFTER the static gutter and the fold glyph
\(`agent-repl-drawer-group-expanded-glyph' /
`agent-repl-drawer-group-folded-glyph'), so this format carries only
the label text and its terminating newline."
  :type 'string
  :group 'agent-repl)

;; Force-apply on reload — the format lost its leading \" ▸ \" when the
;; fold glyph took over that column, and `defcustom' only initializes
;; unbound symbols, so a live Emacs would otherwise keep the old value.
(setq agent-repl-drawer-group-label-format
      (eval (car (get 'agent-repl-drawer-group-label-format 'standard-value))))

(defcustom agent-repl-drawer-group-expanded-glyph "▾ "
  "Fold glyph shown on an UNFOLDED repo group header.
Rendered between the gutter and the repo label.  Its width must match
`agent-repl-drawer-group-folded-glyph' so folding a repo does not shift
the label column."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-drawer-group-folded-glyph "▸ "
  "Fold glyph shown on a FOLDED repo group header.
A folded repo renders its header only: every workspace beneath it is
hidden from the drawer AND from the tab-bar (see
`agent-repl--filter-folded-names')."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-drawer-merge-lookahead 3
  "How many upcoming commits the MERGE QUEUE section shows behind the current one.
The budget is global rather than per-project: it is spent walking the
commit stream in order, so a lookahead that crosses a project boundary
shows commits from the next project rather than padding the current one."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-drawer-merge-slow-commit-threshold 3.0
  "Seconds a commit must be cherry-picking before its elapsed clock appears.
A fast queue then stays quiet and a slow commit announces itself, which is
the only case where the number carries information."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-drawer-merge-subject-width 34
  "Column budget for a commit subject in the MERGE QUEUE section.
Subjects longer than this are truncated with an ellipsis: the drawer is a
narrow side-window, and a wrapped subject costs a whole extra line."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-drawer-marked-glyph "● "
  "Gutter glyph for entries the user has marked for bulk operations.
Width must match `agent-repl-drawer-gutter' for column alignment."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-drawer-current-arrow "▶ "
  "Gutter glyph displayed on the entry the cursor is currently on.
Width must match the static `agent-repl-drawer-gutter' so the arrow
overlay can replace the gutter region without shifting alignment."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-drawer-gutter "  "
  "Static gutter prefix rendered at the start of every workspace block.
The current-entry overlay covers this region with
`agent-repl-drawer-current-arrow'; widths must match."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-drawer-background "#0a0a0a"
  "Background color for the drawer buffer.
Applied via `face-remap-add-relative' so the drawer reads as a
distinct UI region rather than blending with the workspace below.
Default is near-black."
  :type 'color
  :group 'agent-repl)

;;;; Faces ------------------------------------------------------------------

(defface agent-repl-drawer-workspace-name
  '((t :weight bold))
  "Face for the workspace name line in the drawer."
  :group 'agent-repl)

(defface agent-repl-drawer-marked
  '((t :foreground "red" :weight bold))
  "Face for the marked-entry gutter glyph in the drawer."
  :group 'agent-repl)

(defface agent-repl-drawer-current-arrow
  '((t :weight bold :inherit font-lock-keyword-face))
  "Face for the gutter arrow that marks the currently selected workspace."
  :group 'agent-repl)

(defface agent-repl-drawer-summary
  '((t :inherit shadow :slant italic))
  "Face for the aiTitle/prompt-summary subtitle line."
  :group 'agent-repl)

(defface agent-repl-drawer-hidden
  '((t :inherit shadow))
  "Face used to dim hidden workspaces."
  :group 'agent-repl)

(defface agent-repl-drawer-section-title
  '((t :weight bold :inherit font-lock-keyword-face))
  "Face for the MAIN/HIDDEN section title line."
  :group 'agent-repl)

(defface agent-repl-drawer-section-rule
  '((t :inherit shadow))
  "Face for the rule line beneath a section title."
  :group 'agent-repl)

(defface agent-repl-drawer-group-label
  '((t :foreground "white" :weight bold :height 1.15))
  "Face for repo group labels in drawer sections.
Rendered larger than `agent-repl-drawer-workspace-name' (which sits at
the default height) and in white, so the repo heading outranks the
workspace names nested beneath it."
  :group 'agent-repl)

(defface agent-repl-drawer-empty
  '((t :inherit shadow :slant italic))
  "Face for the placeholder shown under an empty section."
  :group 'agent-repl)

(defface agent-repl-drawer-detail-branch
  '((t :foreground "deep sky blue"))
  "Face for the branch name in expanded detail view."
  :group 'agent-repl)

(defface agent-repl-drawer-detail-merge-target
  '((t :foreground "spring green"))
  "Face for the merge-target branch in the MERGED-section detail view."
  :group 'agent-repl)

(defface agent-repl-drawer-detail-ahead-master
  '((t :foreground "spring green" :weight bold))
  "Face for the ahead-master commit count in expanded detail view."
  :group 'agent-repl)

(defface agent-repl-drawer-detail-ahead-source
  '((t :foreground "gold" :weight bold))
  "Face for the ahead-source commit count in expanded detail view."
  :group 'agent-repl)

(defface agent-repl-drawer-detail-last-commit
  '((t :foreground "medium orchid"))
  "Face for the last commit subject + relative time in expanded detail view."
  :group 'agent-repl)

(defface agent-repl-drawer-detail-dirty
  '((t :foreground "tomato" :weight bold))
  "Face for the dirty file count in expanded detail view."
  :group 'agent-repl)

(defface agent-repl-drawer-detail-last-prompt
  '((t :foreground "light steel blue" :slant italic))
  "Face for the duration-since-last-prompt value in expanded detail view."
  :group 'agent-repl)

(defface agent-repl-drawer-detail-pending
  '((t :foreground "orange" :weight bold))
  "Face for the pending-prompt count in expanded detail view."
  :group 'agent-repl)

(defface agent-repl-drawer-detail-merged-in
  '((t :foreground "spring green"))
  "Face for a merged-in workspace name in expanded detail view.
Each entry lists a workspace whose commits were successfully merged
into this workspace (see `:merged-in-workspaces')."
  :group 'agent-repl)

(defface agent-repl-drawer-detail-merge-status
  '((t :foreground "cyan" :weight bold))
  "Face for the merge-status line in an expanded MERGING-section entry.
Colors the brief phase-plus-count indicator (in-flight vs queued, and
the number of commits to be cherry-picked) produced by
`agent-repl-drawer--merge-status-text'."
  :group 'agent-repl)

(defface agent-repl-drawer-merge-sha
  '((t :foreground "medium orchid"))
  "Face for a commit SHA in the MERGE QUEUE section."
  :group 'agent-repl)

(defface agent-repl-drawer-merge-current
  '((t :foreground "spring green" :weight bold))
  "Face for the subject of the commit being cherry-picked right now."
  :group 'agent-repl)

(defface agent-repl-drawer-merge-pending
  '((t :inherit shadow))
  "Face for the subject of a commit still queued behind the current one."
  :group 'agent-repl)

(defface agent-repl-drawer-merge-elapsed
  '((t :foreground "gold" :weight bold))
  "Face for the elapsed clock on a slow commit in the MERGE QUEUE section.
Gold rather than shadow because the clock only appears once the commit has
exceeded `agent-repl-drawer-merge-slow-commit-threshold', so its presence
is itself the signal."
  :group 'agent-repl)

(defface agent-repl-drawer-merge-conflict
  '((t :foreground "tomato" :weight bold))
  "Face for the conflict detail line in the MERGE QUEUE section."
  :group 'agent-repl)

;;;; Mode -------------------------------------------------------------------

(defvar agent-repl-drawer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j")       #'agent-repl-drawer-next)
    (define-key map (kbd "<down>")  #'agent-repl-drawer-next)
    (define-key map (kbd "k")       #'agent-repl-drawer-prev)
    (define-key map (kbd "<up>")    #'agent-repl-drawer-prev)
    (define-key map (kbd "RET")     #'agent-repl-drawer-visit)
    (define-key map (kbd "TAB")     #'agent-repl-drawer-toggle-expand)
    (define-key map (kbd "<tab>")   #'agent-repl-drawer-toggle-expand)
    (define-key map (kbd "g")       #'agent-repl-drawer-refresh)
    (define-key map (kbd "q")       #'agent-repl-drawer-hide)
    ;; Per-entry actions mirroring leader-key bindings:
    (define-key map (kbd "x")       #'agent-repl-drawer-nuke)
    (define-key map (kbd "d")       #'agent-repl-drawer-kill)
    (define-key map (kbd "i")       #'agent-repl-drawer-send-prompt)
    (define-key map (kbd "M")       #'agent-repl-drawer-merge-into-master)
    (define-key map (kbd "m")       #'agent-repl-drawer-merge-child)
    (define-key map (kbd "n")       #'agent-repl-drawer-new-child)
    (define-key map (kbd "f")       #'agent-repl-drawer-new-fork)
    (define-key map (kbd "H")       #'agent-repl-drawer-toggle-hidden)
    (define-key map (kbd "+")       #'agent-repl-drawer-priority-up)
    (define-key map (kbd "-")       #'agent-repl-drawer-priority-down)
    (define-key map (kbd "t")       #'agent-repl-drawer-toggle-mark)
    (define-key map (kbd "u")       #'agent-repl-drawer-clear-marks)
    (define-key map (kbd "C-c C-k") #'agent-repl-drawer-interrupt)
    ;; Block horizontal char navigation — the entry is the unit of
    ;; selection; in-line cursor placement is reserved for searches.
    (define-key map (kbd "<left>")  #'ignore)
    (define-key map (kbd "<right>") #'ignore)
    map)
  "Keymap for `agent-repl-drawer-mode'.")

(defvar-local agent-repl-drawer--current-entry-overlay nil
  "Overlay that draws the current-entry arrow over the static gutter.
Repositioned by `agent-repl-drawer--post-command' to follow point.")

(defvar-local agent-repl-drawer--last-post-command-entry 'unset
  "Entry at point on the previous `--post-command' tick.
An entry is either a workspace or a repo group header (see
`agent-repl-drawer--entry-at').  Used to short-circuit the
per-keystroke overlay refresh + recenter when navigation did not cross
an entry boundary.  `recenter' forces a window redisplay, so gating it
on entry change saves noticeable cost on no-op commands and intra-entry
motion.  Sentinel value `unset' (not nil) so the first tick always runs
even when point starts on a non-entry line.")

(defun agent-repl-drawer--entry-at (pos)
  "Return the navigable drawer entry at POS, or nil.
Two kinds of entry are navigable, and both are selection targets for
`j'/`k':

  - `(:workspace . WS)' — a workspace block (header + summary + any
    expanded detail lines), carrying the `agent-repl-drawer-workspace'
    text property.
  - `(:repo . KEY)' — a repo group header, carrying the
    `agent-repl-drawer-repo' text property whose value is the repo key
    \(see `agent-repl--ws-repo-group').

Returns nil on section headers, rules, blank lines, and the empty-
section placeholder."
  (or (when-let ((ws (get-text-property pos 'agent-repl-drawer-workspace)))
        (cons :workspace ws))
      (when-let ((repo (get-text-property pos 'agent-repl-drawer-repo)))
        (cons :repo repo))))

(defun agent-repl-drawer--entry-at-point ()
  "Return the navigable drawer entry at point, or nil.
See `agent-repl-drawer--entry-at' for the entry shape."
  (agent-repl-drawer--entry-at (point)))

(defun agent-repl-drawer--repo-at-point ()
  "Return the repo key of the group header at point, or nil."
  (let ((entry (agent-repl-drawer--entry-at-point)))
    (and (eq (car-safe entry) :repo) (cdr entry))))

(defun agent-repl-drawer--entry-bounds-at-point ()
  "Return (START . END) of the entry block at point, or nil.
Works for both workspace blocks and repo group headers — the block is
the maximal run of buffer positions whose `--entry-at' equals the entry
at point."
  (let ((entry (agent-repl-drawer--entry-at-point)))
    (when entry
      (save-excursion
        (let (start end)
          (while (and (not (bobp))
                      (equal (agent-repl-drawer--entry-at (1- (point))) entry))
            (forward-char -1))
          (setq start (point))
          (while (and (not (eobp))
                      (equal (agent-repl-drawer--entry-at (point)) entry))
            (forward-char 1))
          (setq end (point))
          (cons start end))))))

(defun agent-repl-drawer--update-current-entry-overlay ()
  "Move the current-entry arrow overlay onto the entry containing point.
Covers the static gutter region (chars [START, START+gutter-width)) of
the entry's first line with a `display' override that renders the
arrow.  Applies to repo group headers as well as workspace blocks —
both are navigable entries and both reserve the same leading gutter.
Removes the overlay when point is not on an entry, or when the entry is
a marked workspace (the red `●' takes precedence so the cursor's
identity is folded into the marked set)."
  (let ((bounds (agent-repl-drawer--entry-bounds-at-point))
        (ws-at-point (agent-repl-drawer--workspace-at-point))
        (gutter-len (length agent-repl-drawer-gutter)))
    (cond
     ((or (null bounds)
          (agent-repl-drawer--marked-p ws-at-point))
      (when (overlayp agent-repl-drawer--current-entry-overlay)
        (delete-overlay agent-repl-drawer--current-entry-overlay)))
     (t
      (let* ((start (car bounds))
             (cover-end (min (cdr bounds) (+ start gutter-len))))
        (if (overlayp agent-repl-drawer--current-entry-overlay)
            (move-overlay agent-repl-drawer--current-entry-overlay
                          start cover-end)
          (setq agent-repl-drawer--current-entry-overlay
                (make-overlay start cover-end)))
        (overlay-put agent-repl-drawer--current-entry-overlay 'display
                     (propertize agent-repl-drawer-current-arrow
                                 'face 'agent-repl-drawer-current-arrow))
        (overlay-put agent-repl-drawer--current-entry-overlay 'priority 100))))))

(defun agent-repl-drawer--update-cursor ()
  "Hide the cursor when point is at column 0 (covered by the arrow), show otherwise.
Searches that pull point mid-line make the cursor reappear; j/k snap
back to col 0 and re-hide it."
  (setq-local cursor-type (if (zerop (current-column)) nil 'box)))

(defun agent-repl-drawer--center-selection (&optional buf)
  "Vertically center the drawer cursor in every window showing BUF.
BUF defaults to the singleton drawer buffer.  Calls `recenter' in
each live window so the highlighted workspace line lands on the
window's middle line.  Near the top/bottom of the workspace list,
Emacs clamps `window-start' at `point-min'/`point-max', so the
cursor drifts off-center naturally rather than revealing blank space
— giving the file-manager-style \"selection-stays-centered when
there's content off-screen\" behavior."
  (when-let* ((buf (or buf (get-buffer agent-repl-drawer-buffer-name))))
    (dolist (win (get-buffer-window-list buf nil t))
      (when (window-live-p win)
        (with-selected-window win
          (recenter))))))

(defun agent-repl-drawer--post-command ()
  "Refresh the current-entry overlay and cursor visibility.
Runs after every command in the drawer buffer.  Short-circuits the
overlay rebuild and the `recenter' call when the entry at point
\(workspace or repo header) has not changed since the previous tick —
the overlay and scroll position are entry-granularity artifacts, and
`--render' already re-establishes overlay state after
mark/expand/fold mutations.  The cursor visibility flip is always-on
because column position can shift within the same entry (e.g. via
in-line search)."
  (agent-repl-drawer--update-cursor)
  (let ((entry (agent-repl-drawer--entry-at-point)))
    (unless (equal entry agent-repl-drawer--last-post-command-entry)
      (agent-repl-drawer--update-current-entry-overlay)
      (agent-repl-drawer--center-selection)
      (setq agent-repl-drawer--last-post-command-entry entry))))

(defvar-local agent-repl-drawer--background-remap-cookie nil
  "Cookie returned by `face-remap-add-relative' for the drawer background.
Tracked buffer-locally so repeated `--apply-background' calls do not
stack additional relative-remap entries on the `default' face.  Mode
init + every subsequent `agent-repl-drawer-show' both call
`--apply-background'; without this cookie each call would `cons' a new
entry onto the face's relative-remap list, growing it unboundedly across
reopens and slowing every redisplay in the drawer buffer.")

(defun agent-repl-drawer--apply-background ()
  "Remap the buffer's `default' face to the drawer background color.
Truly idempotent: first call records the cookie returned by
`face-remap-add-relative' in `agent-repl-drawer--background-remap-cookie'
and subsequent calls short-circuit.  Uses `face-remap-add-relative' so
the change is buffer-local and doesn't leak into the workspace's other
windows."
  (unless agent-repl-drawer--background-remap-cookie
    (setq agent-repl-drawer--background-remap-cookie
          (face-remap-add-relative
           'default :background agent-repl-drawer-background))))

(define-derived-mode agent-repl-drawer-mode special-mode "AgentDrawer"
  "Major mode for the agent-repl workspace drawer."
  (setq truncate-lines nil
        buffer-read-only t
        mode-line-format nil
        word-wrap t)
  (setq-local cursor-type nil)
  (agent-repl-drawer--apply-background)
  (add-hook 'post-command-hook
            #'agent-repl-drawer--post-command nil t))

;; Evil intercepts j/k/n/p in motion/normal state and routes them to
;; line-wise commands.  Bind our drawer commands explicitly in those
;; states so j/k jump workspace-by-workspace, not line-by-line.
(when (fboundp 'evil-define-key)
  (evil-define-key '(normal motion) agent-repl-drawer-mode-map
    "j"           #'agent-repl-drawer-next
    "k"           #'agent-repl-drawer-prev
    (kbd "<down>") #'agent-repl-drawer-next
    (kbd "<up>")   #'agent-repl-drawer-prev
    (kbd "RET")    #'agent-repl-drawer-visit
    "g"           #'agent-repl-drawer-refresh
    "q"           #'agent-repl-drawer-hide)
  ;; Block every insert-state entry point we don't repurpose, so the
  ;; drawer never flips into evil insert state.  `i', `m', `d' are
  ;; rebound to drawer commands below.
  (dolist (key '("I" "a" "A" "o" "O" "s" "S" "c" "C" "R"))
    (evil-define-key '(normal motion) agent-repl-drawer-mode-map
      key #'ignore))
  ;; Per-entry action keys — bind in evil states too so they aren't
  ;; intercepted by evil-motion's defaults.
  (evil-define-key '(normal motion) agent-repl-drawer-mode-map
    "x"             #'agent-repl-drawer-nuke
    "d"             #'agent-repl-drawer-kill
    "i"             #'agent-repl-drawer-send-prompt
    "M"             #'agent-repl-drawer-merge-into-master
    "m"             #'agent-repl-drawer-merge-child
    "n"             #'agent-repl-drawer-new-child
    "f"             #'agent-repl-drawer-new-fork
    "H"             #'agent-repl-drawer-toggle-hidden
    "+"             #'agent-repl-drawer-priority-up
    "-"             #'agent-repl-drawer-priority-down
    "t"             #'agent-repl-drawer-toggle-mark
    "u"             #'agent-repl-drawer-clear-marks
    (kbd "TAB")     #'agent-repl-drawer-toggle-expand
    (kbd "<tab>")   #'agent-repl-drawer-toggle-expand
    (kbd "C-c C-k") #'agent-repl-drawer-interrupt)
  ;; Block horizontal char navigation — entry is the navigational unit.
  (evil-define-key '(normal motion) agent-repl-drawer-mode-map
    "h"             #'ignore
    "l"             #'ignore
    (kbd "<left>")  #'ignore
    (kbd "<right>") #'ignore))

;; Force motion-state on entry so the drawer never starts in normal
;; (where insert keys could trigger before our overrides apply).
(when (fboundp 'evil-set-initial-state)
  (evil-set-initial-state 'agent-repl-drawer-mode 'motion))

;;;; Sorting + selection helpers --------------------------------------------

(defun agent-repl-drawer--visible-workspace-keys ()
  "Return the live agent-repl workspaces to render, from `agent-repl--workspaces'.

Filters three classes of key that are present in the hash but are not
agent-repl workspaces the drawer should show:

- Project-dir-less stubs.  Every real workspace carries `:project-dir'
  from birth (seeded at the `agent-repl--ws-create' boundary) and keeps
  it across tombstoning, so a live entry WITHOUT `:project-dir' is never
  a workspace — it is a plain persp (notably Doom's default \"main\")
  auto-vivified into the hash by a persp hook writing panel/repl
  bookkeeping onto whatever persp happens to be current.  Such stubs are
  what used to populate the drawer's `(no repo)' group.

- The persp-mode sentinel: any key whose name (or bare name) equals
  `persp-nil-name' (default \"none\").  Workspace creation already
  refuses bare names that collide with it, but the sentinel can still
  leak in via stray status syncs.

- Tombstoned entries (`:nuked-at' set), so nuked workspaces don't linger
  as drawer ghosts despite their identity records surviving in the hash.

hide-project-dirs needs no special handling here: that mode kills
matching workspaces (`agent-repl-toggle-hide-project-dirs'), so they
become tombstones and the `:nuked-at' filter above already drops them
from the drawer."
  (let ((nil-name (agent-repl--ws-nil-name)))
    (cl-remove-if
     (lambda (ws)
       (or (null (agent-repl--ws-get ws :project-dir))
           (and nil-name
                (or (equal ws nil-name)
                    (equal (agent-repl--bare-workspace-name ws) nil-name)))))
     (agent-repl--live-ws-names))))

(defun agent-repl-drawer--workspace-hidden-p (ws)
  "Return non-nil if workspace WS is in the `:hidden' repl-state."
  (eq (agent-repl--ws-get ws :repl-state) :hidden))

(defun agent-repl-drawer--sort-key (ws)
  "Return a sort key for workspace name WS.
Lower keys come first.  Sort by `:priority' rank, then name."
  (cons (agent-repl--priority-rank (agent-repl--ws-get ws :priority))
        ws))

(defun agent-repl-drawer--sort (names)
  "Return NAMES sorted by priority rank, then alphabetically."
  (sort (copy-sequence names)
        (lambda (a b)
          (let ((ka (agent-repl-drawer--sort-key a))
                (kb (agent-repl-drawer--sort-key b)))
            (or (< (car ka) (car kb))
                (and (= (car ka) (car kb))
                     (string< (cdr ka) (cdr kb))))))))

(defun agent-repl-drawer--partition (names)
  "Return (VISIBLE . HIDDEN) lists from NAMES, each sorted.
Legacy two-section partition; tree-aware sectioning lives in
`agent-repl-drawer--partition-by-section'."
  (let (visible hidden)
    (dolist (ws names)
      (if (agent-repl-drawer--workspace-hidden-p ws)
          (push ws hidden)
        (push ws visible)))
    (cons (agent-repl-drawer--sort visible)
          (agent-repl-drawer--sort hidden))))

;;;; Section + tree helpers -------------------------------------------------

(defun agent-repl-drawer--workspace-section (ws)
  "Return :main, :hidden, :merging, or :merged for WS based on render-status.
Sections are layout buckets, not appearance.  But the merge buckets
map 1:1 onto `agent-repl--ws-render-status' values — the same
keyword that drives the icon also drives the section — so the merge
branches delegate to render-status for the source of truth.

The MERGING section is defined to hold EXACTLY the members of the
live merge queue and nothing else: a workspace is bucketed under
MERGING if and only if it is a member of the merge queue.  Membership
means one of two render-states, which map 1:1 onto the two pieces of
queue bookkeeping:
  - `:merging'      ↔ an in-flight cherry-pick (`agent-repl--in-flight-merges').
  - `:merge-queued' ↔ a parked request (`agent-repl--merge-queue').
Every other merge render-state is NOT a queue member and therefore
must NOT land in MERGING.

Mapping:
  render-status :merged                → :merged section
  render-status :merging               → :merging section (in flight, in queue)
  render-status :merge-queued          → :merging section (parked, in queue)
  render-status :merge-conflict        → :merged section (a real cherry-pick
                                          conflict awaiting human resolution;
                                          it was removed from the queue and not
                                          re-enqueued by
                                          `agent-repl--mark-merge-conflict', so
                                          it is NOT a queue member and must not
                                          sit in MERGING — it groups with the
                                          other terminal merge outcomes under
                                          MERGED, distinguished by its 💥 glyph)
  render-status :merge-failed          → :merged section (the historical
                                          MERGED-with-⛔ bucket; the
                                          workspace did `complete' the
                                          merge command, just failed at
                                          the cherry-pick level)
  otherwise + `:repl-state :hidden'    → :hidden
  otherwise                            → :main

Precedence is encoded in `--ws-render-status' itself."
  (let ((status (and (agent-repl--ws-known-p ws)
                     (agent-repl--ws-render-status ws))))
    (cond
     ((eq status :merged)                              :merged)
     ((eq status :merge-failed)                        :merged)
     ((eq status :merge-conflict)                      :merged)
     ((memq status '(:merging :merge-queued))          :merging)
     ((eq (agent-repl--ws-get ws :repl-state) :hidden) :hidden)
     (t                                                 :main))))

(defun agent-repl-drawer--auto-expand-p (ws)
  "Return non-nil if WS should be auto-expanded when the drawer opens or switches.
Only MERGING-section entries auto-expand so their in-flight detail
lines stay visible while the merge is live.  Every other entry is
left folded — the drawer surfaces detail automatically only for the
merge queue.  Manual `TAB' expansion via
`agent-repl-drawer-toggle-expand' is unaffected and still works on
any entry."
  (eq (agent-repl-drawer--workspace-section ws) :merging))

(defvar agent-repl-drawer--dir->name-map nil
  "Dynamic-binding cache: canonical project-dir → workspace name.
When non-nil (bound by `agent-repl-drawer--with-dir-map'),
`--source-ws-name' resolves cold misses via O(1) hash lookup against
this map instead of falling through to `agent-repl--ws-name-for-dir',
whose `maphash' + per-workspace `file-truename' is O(N) per call.  N
chain walks during a render would otherwise be O(N²); the map collapses
this to O(N) — one build, all lookups constant-time.

Outside the macro's dynamic extent the var is nil and callers fall back
to the legacy lookup, so non-drawer callers (and the macro's first cold
build itself) keep the original semantics.")

(defun agent-repl-drawer--build-dir->name-map ()
  "Return a fresh hash table mapping canonical `:project-dir' → ws name.
One `maphash' over `agent-repl--workspaces' with one
`agent-repl--path-canonical' (i.e. `file-truename') per workspace.
Total cost O(N); see `agent-repl-drawer--dir->name-map' for context.

Skips tombstoned entries (`:nuked-at' set) so a nuked workspace's
preserved `:project-dir' cannot shadow a live workspace that later
resolves at the same canonical path."
  (let ((map (make-hash-table :test 'equal)))
    (maphash (lambda (ws plist)
               (when-let ((dir (plist-get plist :project-dir)))
                 (unless (plist-get plist :nuked-at)
                   (puthash (agent-repl--path-canonical dir) ws map))))
             agent-repl--workspaces)
    map))

(defmacro agent-repl-drawer--with-dir-map (&rest body)
  "Evaluate BODY with `agent-repl-drawer--dir->name-map' bound.
Builds the reverse-lookup map exactly once for the duration of BODY so
nested `--source-ws-name' calls amortize against a single O(N) walk.
Nested invocations reuse the outer binding (the `or' branch) so a
caller wrapping `agent-repl-drawer-show' doesn't pay a second build
when an inner `--render' also wraps."
  (declare (indent 0) (debug t))
  `(let ((agent-repl-drawer--dir->name-map
          (or agent-repl-drawer--dir->name-map
              (agent-repl-drawer--build-dir->name-map))))
     ,@body))

(defun agent-repl-drawer--source-ws-name (ws)
  "Return the workspace name recorded as WS's source, or nil.
Reverse-lookups `:source-ws-dir' and caches the resolved name on the
workspace plist as `:source-ws-name'.  Cache invalidation is centralized
at the two paths that mutate name→workspace mappings:
- `agent-repl--ws-del' sweeps peers whose `:source-ws-name' equals
  the deleted workspace.
- `agent-repl--rename-update-source-back-refs' clears
  `:source-ws-name' alongside the `:source-ws-dir' rewrite it already
  performs.
A cached value is therefore always either correct or nil.

Cold-miss path: when `agent-repl-drawer--dir->name-map' is bound (we
are inside a `--with-dir-map' body), resolves via O(1) hash lookup
against that map.  Otherwise falls back to `agent-repl--ws-name-for-dir',
which is correct but O(N) per call — driving the O(N²) cost of cold
tree walks that the macro exists to eliminate."
  (when-let ((dir (agent-repl--ws-get ws :source-ws-dir)))
    (or (agent-repl--ws-get ws :source-ws-name)
        (let ((resolved
               (if agent-repl-drawer--dir->name-map
                   (gethash (agent-repl--path-canonical dir)
                            agent-repl-drawer--dir->name-map)
                 (agent-repl--ws-name-for-dir dir))))
          (when resolved
            (agent-repl--ws-put ws :source-ws-name resolved))
          resolved))))

(defcustom agent-repl-drawer-tree-max-depth 16
  "Cycle defense for drawer parent-chain walks."
  :type 'integer
  :group 'agent-repl)

(defun agent-repl-drawer--ws-flattenable-ancestor-p (ws)
  "Return non-nil when WS should be skipped when flattening parent chains.
Driven exclusively by git ancestry (`:branch-merged' = `merged'): if
the work in WS has landed in its parent according to git, MAIN/HIDDEN
trees flatten through WS so the user does not see a stale link to a
workspace whose work has effectively landed.

Workflow state (`:merge-completed t', `:merging t') deliberately does
NOT count here.  Flattening exists to mirror git reality — the same
reality that guides `--resolve-merge-into-source-target' — not the
user-visible workflow lifecycle.  In practice a successful workspace
merge yields `:branch-merged' = `merged' on the next ancestry poll,
so the two converge; isolating the predicate ensures workflow-flag
oddities (e.g., `:merge-completed t' but git rolled back) can never
mis-flatten the tree."
  (agent-repl--ws-merged-p ws))

(defun agent-repl-drawer--effective-parent (ws section-set)
  "Return WS's effective parent in SECTION-SET (a list of workspace names).
Walks the source-ws chain skipping ancestors flagged as flattenable
by `agent-repl-drawer--ws-flattenable-ancestor-p' (git-ancestry-detected
merged only).  Returns the first non-flattenable ancestor that lives in
SECTION-SET, or nil when no ancestor qualifies (WS is a root in this
section).  Cycle-capped via `agent-repl-drawer-tree-max-depth'."
  (let ((candidate (agent-repl-drawer--source-ws-name ws))
        (depth 0)
        (result nil)
        (done nil))
    (while (and (not done) candidate
                (< depth agent-repl-drawer-tree-max-depth))
      (setq depth (1+ depth))
      (cond
       ((agent-repl-drawer--ws-flattenable-ancestor-p candidate)
        (setq candidate (agent-repl-drawer--source-ws-name candidate)))
       ((member candidate section-set)
        (setq result candidate done t))
       (t (setq done t))))
    result))

(defun agent-repl-drawer--effective-parent-in-section (ws section-set)
  "Return WS's parent in a same-section tree: source-ws if also in SECTION-SET, else nil.
Used by MERGING and MERGED, which preserve original topology rather
than flattening through merged ancestors — both sections want to show
the parent/child structure between their own members."
  (when-let ((src (agent-repl-drawer--source-ws-name ws)))
    (when (member src section-set) src)))

(defalias 'agent-repl-drawer--effective-parent-in-merged
  #'agent-repl-drawer--effective-parent-in-section
  "Back-compat alias.  Old name retained so external callers / tests
don't break; the implementation is now shared with MERGING.")

(defun agent-repl-drawer--partition-by-section (workspaces)
  "Partition WORKSPACES into (:main :hidden :merging :merged) buckets."
  (let (main hidden merging merged)
    (dolist (ws workspaces)
      (pcase (agent-repl-drawer--workspace-section ws)
        (:main    (push ws main))
        (:hidden  (push ws hidden))
        (:merging (push ws merging))
        (:merged  (push ws merged))))
    `((:main    . ,main)
      (:hidden  . ,hidden)
      (:merging . ,merging)
      (:merged  . ,merged))))

(defun agent-repl-drawer--build-tree (workspaces parent-fn)
  "Build a forest of trees from WORKSPACES using PARENT-FN to resolve parents.
PARENT-FN takes a workspace name and returns its parent in this
section, or nil if it's a root.  Each tree is `(WS . CHILDREN)' where
CHILDREN is a list of trees.  Roots and siblings are sorted by
`agent-repl-drawer--sort'."
  (let ((children-of (make-hash-table :test 'equal))
        (parents (make-hash-table :test 'equal)))
    (dolist (ws workspaces)
      (let ((p (funcall parent-fn ws)))
        (puthash ws (or p :no-parent) parents)
        (when p
          (puthash p (cons ws (gethash p children-of)) children-of))))
    (let (roots)
      (dolist (ws workspaces)
        (when (eq (gethash ws parents) :no-parent)
          (push ws roots)))
      (cl-labels ((build (ws)
                    (cons ws
                          (mapcar #'build
                                  (agent-repl-drawer--sort
                                   (or (gethash ws children-of) nil))))))
        (mapcar #'build (agent-repl-drawer--sort roots))))))

;;;; Render -----------------------------------------------------------------

(defun agent-repl-drawer--state-glyph (ws)
  "Return the indicator glyph for workspace WS.
Delegates render-state selection to `agent-repl--ws-render-status'
(the single source of truth for visual state across drawer, tab-bar,
and project picker) and maps the resulting keyword through
`agent-repl-drawer-state-icons'.  When render-status returns nil
(tombstoned or no signals), falls back to
`agent-repl-drawer-state-icon-default' (the middot placeholder)."
  (let* ((status (and (agent-repl--ws-known-p ws)
                      (agent-repl--ws-render-status ws))))
    (or (alist-get status agent-repl-drawer-state-icons)
        agent-repl-drawer-state-icon-default)))

(defun agent-repl-drawer--priority-display (priority)
  "Return a display string for PRIORITY.
Uses the badge PNG from `agent-repl--priority-images' when available,
falling back to the raw PRIORITY string for terminal/batch contexts.
Returns the empty string when PRIORITY is nil so unprioritized
workspaces don't carry a phantom space."
  (cond
   ((null priority) "")
   ((agent-repl--priority-image priority)
    (propertize priority 'display (agent-repl--priority-image priority)))
   (t priority)))

(defun agent-repl-drawer--name-face (ws)
  "Return the face spec for WS's name, colored by render-status.
Delegates to `agent-repl--ws-render-status' for the underlying state
keyword, then maps agent-activity states to colored bold-foreground
specs.  Merge-states and :dead intentionally fall through to the
default workspace-name face — the icon column already carries the
distinguishing signal (🔀/⛔/💥/🔄/🕒/❌), so the name itself can stay
uncolored while the hidden/dim treatment provides the muting.
:merging (the new in-flight signal) also falls through for the same
reason."
  (let* ((status (and (agent-repl--ws-known-p ws)
                      (agent-repl--ws-render-status ws)))
         (color (cond
                 ((eq status :init)         agent-repl--color-init-blue)
                 ((eq status :thinking)     agent-repl--color-thinking-red)
                 ((memq status '(:done :permission))
                  agent-repl--color-done-green)
                 ((eq status :idle)         agent-repl--color-idle-orange)
                 ((memq status '(:stop-failed :start-failed))
                  agent-repl--color-stop-failed-magenta))))
    (if color
        `(:foreground ,color :weight bold)
      'agent-repl-drawer-workspace-name)))

(defun agent-repl-drawer--summary-text (ws)
  "Return the aiTitle/prompt-summary string for WS, or a placeholder."
  (let ((summary (agent-repl--ws-get ws :last-prompt-summary))
        (pending (agent-repl--ws-get ws :last-prompt-summary-pending)))
    (cond
     ((and (stringp summary) (not (string-empty-p summary)))
      summary)
     (pending "…")
     (t "—"))))

(defun agent-repl-drawer--current-ws ()
  "Return the currently active workspace name, or nil."
  (agent-repl--ws-current-name))

(defun agent-repl-drawer--render-workspace (ws _current hidden &optional depth)
  "Insert the rendered representation for workspace WS into the current buffer.
Optional DEPTH (default 0) shifts the entry right by `depth × 2'
spaces *after* the static gutter — the gutter stays at column 0 so
the current-entry arrow overlay aligns regardless of nesting.  Sets a
`wrap-prefix' text property on both header and summary lines so soft
word-wrap continuation lines indent to the same start column as the
content (rather than back to column 0).  HIDDEN dims the block."
  (let* ((depth      (or depth 0))
         (priority   (agent-repl--ws-get ws :priority))
         (glyph      (agent-repl-drawer--state-glyph ws))
         (dirty      (eq (agent-repl--ws-get ws :git-clean) 'dirty))
         (start      (point))
         (prio-disp  (agent-repl-drawer--priority-display priority))
         (sep        (if priority " " ""))
         (name-face  (agent-repl-drawer--name-face ws))
         (indent-str (make-string (* depth agent-repl-drawer-indent-per-level)
                                  ?\s))
         (gutter-str (if (agent-repl-drawer--marked-p ws)
                         (propertize agent-repl-drawer-marked-glyph
                                     'face 'agent-repl-drawer-marked)
                       agent-repl-drawer-gutter))
         (header     (concat gutter-str indent-str
                             glyph "  " prio-disp sep
                             (propertize ws 'face name-face)
                             (if dirty " ●" "")))
         (summary    (agent-repl-drawer--summary-text ws))
         ;; Continuation lines align with content: header's content
         ;; (the glyph) starts at gutter+indent; summary's content
         ;; starts at gutter+indent+2 (the leading two-space pad
         ;; before the summary text).
         (header-wrap-prefix
          (concat agent-repl-drawer-gutter indent-str))
         (summary-wrap-prefix
          (concat agent-repl-drawer-gutter indent-str "  ")))
    (let ((header-start (point)))
      (insert header "\n")
      (add-text-properties header-start (point)
                           (list 'wrap-prefix header-wrap-prefix)))
    (let ((summary-start (point)))
      (insert agent-repl-drawer-gutter indent-str "  "
              (propertize summary 'face 'agent-repl-drawer-summary)
              "\n")
      (add-text-properties summary-start (point)
                           (list 'wrap-prefix summary-wrap-prefix)))
    (let ((end (point)))
      (add-text-properties
       start end
       (list 'agent-repl-drawer-workspace ws
             'help-echo (format "Workspace: %s%s"
                                ws
                                (if hidden " (hidden)" ""))))
      (when hidden
        (add-face-text-property start end 'agent-repl-drawer-hidden)))))

(defun agent-repl-drawer--insert-section-header (label)
  "Insert a bold section header LABEL with a rule line beneath it."
  (insert (propertize (format " %s\n" label)
                      'face 'agent-repl-drawer-section-title))
  (insert (propertize (concat " "
                              (make-string agent-repl-drawer-section-rule-width
                                           ?─)
                              "\n")
                      'face 'agent-repl-drawer-section-rule)))

(defun agent-repl-drawer--render-subtree (tree depth current section)
  "Render TREE (a `(WS . CHILDREN)' cell) at DEPTH.
SECTION is :main, :hidden, :merging, or :merged — only :hidden
propagates the dim treatment via the HIDDEN flag passed to
`--render-workspace'.  :merging and :merged render normally so the
operational state (badge, prompt summary) remains legible.
When the entry is in the expanded-set, additional detail lines are
appended under the standard 2-line render.  Children render
contiguously (no inter-child blank); blank-between-roots is the
caller's responsibility."
  (let ((ws (car tree)))
    (agent-repl-drawer--render-workspace ws current
                                          (eq section :hidden)
                                          depth)
    (when (agent-repl-drawer--expanded-p ws)
      (agent-repl-drawer--render-detail-lines ws depth)))
  (dolist (child (cdr tree))
    (agent-repl-drawer--render-subtree child (1+ depth) current section)))

(defun agent-repl-drawer--workspace-group-key (ws)
  "Return a stable group key for WS based on git common-dir.
Thin drawer-side alias for `agent-repl--ws-repo-key' (workspace.el),
which owns repo identity now that the tab-bar and the indexed
switchers read it too.  Returns nil when git fails on WS's project-dir
— e.g. the worktree directory was deleted out from under the
workspace.  Such workspaces fall into the `(no repo)' bucket via
`agent-repl--ws-repo-group'.

Project-dir-less stubs never reach this function: they are filtered
upstream by `agent-repl-drawer--visible-workspace-keys', so `(no repo)'
is now reachable only for a real workspace whose repo genuinely cannot
be resolved, never for Doom's default \"main\" persp."
  (agent-repl--ws-repo-key ws))

(defun agent-repl-drawer--group-label (key)
  "Derive a human-readable group label from KEY (a canonical .git path).
Thin drawer-side alias for `agent-repl--repo-label' (workspace.el).
Returns the basename of KEY's parent directory — i.e. the project
name, since git's common-dir is conventionally `<project>/.git'."
  (agent-repl--repo-label key))

(defun agent-repl-drawer--group-trees-by-repo (trees)
  "Partition TREES into (KEY LABEL . TREES-IN-GROUP) buckets by repo.
Bucket keys are each tree root's repo key (`agent-repl--ws-repo-group',
the total variant that folds an unresolvable repo onto the
`(no repo)' sentinel), and LABEL is that key rendered for display.

Buckets key on the repo KEY rather than its LABEL so two distinct
repos that share a basename stay distinct groups — and so a fold,
which is recorded against the key, addresses exactly one group.

Insertion order of the returned list matches the first-encounter order
in TREES, so groups appear in the order their first root appeared after
sorting."
  (let ((order nil)
        (buckets (make-hash-table :test 'equal)))
    (dolist (tree trees)
      (let* ((root (car tree))
             (key  (agent-repl--ws-repo-group root)))
        (unless (gethash key buckets)
          (push key order))
        (puthash key
                 (append (gethash key buckets) (list tree))
                 buckets)))
    (mapcar (lambda (key)
              (cons key (cons (agent-repl--repo-label key)
                              (gethash key buckets))))
            (nreverse order))))

(defun agent-repl-drawer--render-group-header (key label folded)
  "Insert the repo group header for repo KEY, displayed as LABEL.
FOLDED selects the fold glyph.  The header is a navigable entry: it
carries the `agent-repl-drawer-repo' text property (value KEY), so
`j'/`k' stop on it and `TAB' toggles its fold.  Rendered after the
static gutter so the current-entry arrow overlay lands in the same
column as it does on workspace blocks."
  (let ((start (point))
        (glyph (if folded
                   agent-repl-drawer-group-folded-glyph
                 agent-repl-drawer-group-expanded-glyph)))
    (insert agent-repl-drawer-gutter
            (propertize
             (concat glyph
                     (format agent-repl-drawer-group-label-format label))
             'face 'agent-repl-drawer-group-label))
    (add-text-properties
     start (point)
     (list 'agent-repl-drawer-repo key
           'help-echo (format "Repo: %s (TAB to %s)"
                              label (if folded "unfold" "fold"))))))

(defun agent-repl-drawer--render-trees (trees current section)
  "Render TREES (forest) grouped by top-level repo.
Within a group, root subtrees render contiguously with a blank line
between siblings.  Between groups, a labeled header (plus blank line)
marks the boundary so multi-repo drawers stay scannable.

A FOLDED repo renders its header and nothing else — every workspace in
the group is omitted from the buffer (and, via
`agent-repl--filter-folded-names', from the tab-bar)."
  (let ((groups (agent-repl-drawer--group-trees-by-repo trees))
        (first-group t))
    (dolist (group groups)
      (let* ((key         (car group))
             (label       (cadr group))
             (group-trees (cddr group))
             (folded      (agent-repl--repo-folded-p key)))
        (unless first-group
          (insert "\n"))
        (agent-repl-drawer--render-group-header key label folded)
        (unless folded
          (let ((tree-rest group-trees))
            (while tree-rest
              (agent-repl-drawer--render-subtree (car tree-rest) 0
                                                  current section)
              (when (cdr tree-rest) (insert "\n"))
              (setq tree-rest (cdr tree-rest)))))
        (setq first-group nil)))))

(defun agent-repl-drawer--parent-fn-for-section (workspaces section)
  "Return a parent-resolution function for SECTION.
:main and :hidden flatten through merged/completed ancestors;
:merging and :merged preserve original topology so their internal
parent/child structure stays intact."
  (lambda (ws)
    (cond
     ((memq section '(:merged :merging))
      (agent-repl-drawer--effective-parent-in-section ws workspaces))
     (t
      (agent-repl-drawer--effective-parent ws workspaces)))))

(defun agent-repl-drawer--insert-section (label workspaces current section)
  "Render a section titled LABEL.
WORKSPACES is the list of names belonging to this section.  CURRENT
is the currently selected workspace (per persp).  SECTION is :main,
:hidden, :merging, or :merged — controls parent-resolution and dim
treatment.  Empty sections render the `(none)' placeholder under the
header."
  (agent-repl-drawer--insert-section-header label)
  (if (null workspaces)
      (insert (propertize (format "  %s\n"
                                  agent-repl-drawer-empty-section-label)
                          'face 'agent-repl-drawer-empty))
    (let* ((parent-fn (agent-repl-drawer--parent-fn-for-section
                       workspaces section))
           (trees (agent-repl-drawer--build-tree workspaces parent-fn)))
      (agent-repl-drawer--render-trees trees current section))))

;;;; MERGE QUEUE section -----------------------------------------------------
;;
;; Forward declarations: these live in worktree.el, which config.el loads
;; before drawer.el.  Declared here so the section below compiles clean rather
;; than adding to the file's cross-module free-variable warnings.
(defvar agent-repl--merge-queue)
(defvar agent-repl--in-flight-merges)
(defvar agent-repl--merge-progress-seq)
(defvar agent-repl--merge-lookahead)
;;
;;
;; A COMMIT-level view, and the only section in the drawer whose rows are not
;; workspaces.  MERGING answers "which workspaces are merging"; this answers
;; "which commit is git applying right now, for how long, and what is behind
;; it".  The two are complementary, which is why both exist.
;;
;; Because these rows carry `agent-repl-drawer-commit' rather than
;; `agent-repl-drawer-workspace', every piece of workspace machinery — j/k
;; navigation, the current-entry overlay, marks, expansion, cursor restore —
;; skips them exactly the way it already skips headers and rule lines.  No
;; workspace is ever rendered twice.

(defun agent-repl-drawer--merge-project-label (target-dir)
  "Return the project label for a merge whose destination is TARGET-DIR."
  (when target-dir
    (file-name-nondirectory (directory-file-name target-dir))))

(defun agent-repl-drawer--merge-stream ()
  "Return the ordered commit stream backing the MERGE QUEUE section.

Each element is a plist with `:sha' `:subject' `:project' `:ws' `:state'
and, for the commit being applied, `:started-at' plus any conflict and
resolver detail.  `:state' is one of `current', `conflict', `pending', or
`halted'.

Order is in-flight picks first (from the commit being applied through the
rest of their range), then queued entries in bucket FIFO order.  That is
what lets a lookahead cross a project boundary."
  (let ((stream nil))
    (dolist (entry agent-repl--in-flight-merges)
      (let* ((ws       (plist-get entry :source-ws))
             (project  (agent-repl-drawer--merge-project-label
                        (plist-get entry :target-dir)))
             (progress (agent-repl--merge-progress-get ws))
             (commits  (plist-get progress :commits))
             (index    (or (plist-get progress :commit-index) 0))
             (conflict (plist-get progress :conflict-sha))
             (i        index))
        (while (< i (length commits))
          (let ((commit  (nth i commits))
                (currentp (= i index)))
            (push (append
                   (list :sha     (car commit)
                         :subject (cdr commit)
                         :project project
                         :ws      ws
                         :state   (cond ((not currentp) 'pending)
                                        (conflict       'conflict)
                                        (t              'current)))
                   (when currentp
                     (list :started-at          (plist-get progress :commit-started-at)
                           :conflict-files      (plist-get progress :conflict-files)
                           :resolver-phase      (plist-get progress :resolver-phase)
                           :resolver-started-at (plist-get progress :resolver-started-at))))
                  stream))
          (setq i (1+ i)))))
    (dolist (entry agent-repl--merge-queue)
      (let* ((ws      (plist-get entry :source-ws))
             (project (agent-repl-drawer--merge-project-label
                       (plist-get entry :target-dir)))
             (state   (if (plist-get entry :halt-until-human) 'halted 'pending)))
        (dolist (commit (plist-get (gethash ws agent-repl--merge-lookahead)
                                   :commits))
          (push (list :sha     (car commit)
                      :subject (cdr commit)
                      :project project
                      :ws      ws
                      :state   state)
                stream))))
    (nreverse stream)))

(defun agent-repl-drawer--merge-stream-visible (stream)
  "Truncate STREAM to the commits in flight plus the lookahead budget.

Every `current'/`conflict' commit survives regardless of budget: buckets
drain concurrently, so each active project has one, and hiding any of them
would hide a merge that is actually running.  The
`agent-repl-drawer-merge-lookahead' budget is then spent, in stream order,
on the commits waiting behind them."
  (let ((budget agent-repl-drawer-merge-lookahead)
        (visible nil))
    (dolist (element stream)
      (cond
       ((memq (plist-get element :state) '(current conflict))
        (push element visible))
       ((> budget 0)
        (setq budget (1- budget))
        (push element visible))))
    (nreverse visible)))

(defun agent-repl-drawer--merge-elapsed-string (started-at now)
  "Format NOW minus STARTED-AT as `M:SS', or nil below the slow threshold."
  (when started-at
    (let ((seconds (- now started-at)))
      (when (>= seconds agent-repl-drawer-merge-slow-commit-threshold)
        (format "%d:%02d" (floor seconds 60) (mod (floor seconds) 60))))))

(defun agent-repl-drawer--merge-truncate (text width)
  "Truncate TEXT to WIDTH columns, ellipsizing when it does not fit."
  (if (<= (length text) width)
      text
    (concat (substring text 0 (max 0 (1- width))) "…")))

(defun agent-repl-drawer--merge-conflict-detail (element now)
  "Return the conflict detail line for ELEMENT at NOW, or nil.
Names the unmerged files and, while the auto-resolver is running, what it
is doing and for how long."
  (let* ((files    (plist-get element :conflict-files))
         (phase    (plist-get element :resolver-phase))
         (resolver (agent-repl-drawer--merge-elapsed-string
                    (plist-get element :resolver-started-at) now))
         (parts    (delq nil
                         (list (when files
                                 (format "%d file%s unmerged"
                                         (length files)
                                         (if (= 1 (length files)) "" "s")))
                               (when phase
                                 (concat "resolver: " (symbol-name phase)
                                         (when resolver (concat " " resolver))))))))
    (when parts
      (string-join parts " · "))))

(defun agent-repl-drawer--insert-merge-commit (element now)
  "Insert one commit row for ELEMENT, evaluated at NOW."
  (let* ((state   (plist-get element :state))
         (glyph   (pcase state
                    ('current  "⟳")
                    ('conflict "💥")
                    ('halted   "⛔")
                    (_         " ")))
         (pending (memq state '(pending halted)))
         (elapsed (agent-repl-drawer--merge-elapsed-string
                   (plist-get element :started-at) now))
         (detail  (when (eq state 'conflict)
                    (agent-repl-drawer--merge-conflict-detail element now)))
         (start   (point)))
    (insert agent-repl-drawer-gutter glyph " ")
    (insert (propertize (plist-get element :sha)
                        'face 'agent-repl-drawer-merge-sha))
    (insert " ")
    (insert (propertize (agent-repl-drawer--merge-truncate
                         (plist-get element :subject)
                         agent-repl-drawer-merge-subject-width)
                        'face (if pending
                                  'agent-repl-drawer-merge-pending
                                'agent-repl-drawer-merge-current)))
    (when elapsed
      (insert (propertize (concat "  " elapsed)
                          'face 'agent-repl-drawer-merge-elapsed)))
    (insert "\n")
    (when detail
      (insert agent-repl-drawer-gutter "    ")
      (insert (propertize detail 'face 'agent-repl-drawer-merge-conflict))
      (insert "\n"))
    ;; Deliberately NOT `agent-repl-drawer-workspace': see the section header
    ;; comment.  A commit row must be invisible to workspace navigation.
    (put-text-property start (point) 'agent-repl-drawer-commit
                       (plist-get element :sha))
    (put-text-property start (point) 'agent-repl-drawer-commit-ws
                       (plist-get element :ws))))

(defun agent-repl-drawer--insert-merge-project-separator (project)
  "Insert the project separator for PROJECT in the MERGE QUEUE section.

Reuses the repo group header's gutter, glyph, format, and face so the two
read as the same kind of divider, and so a change to that styling cannot
leave this one behind.

Deliberately does NOT carry `agent-repl-drawer-repo': a repo group header
is a foldable, navigable entry (`--render-group-header'), and this is not
one.  Folding a project inside a commit stream is meaningless, and
carrying the property would additionally entangle this separator with the
real repo group's fold state."
  (insert agent-repl-drawer-gutter
          (propertize
           (concat agent-repl-drawer-group-expanded-glyph
                   (format agent-repl-drawer-group-label-format
                           (or project "(no repo)")))
           'face 'agent-repl-drawer-group-label)))

(defun agent-repl-drawer--insert-merge-queue-section (stream now)
  "Insert the MERGE QUEUE section for STREAM, evaluated at NOW.

A project separator is emitted whenever an element's project differs from
its predecessor's, so a run of commits within one project carries a single
header and a lookahead that crosses into the next project announces the
crossing."
  (agent-repl-drawer--insert-section-header
   (format "MERGE QUEUE (%d)" (length stream)))
  (let ((previous-project nil)
        (first t))
    (dolist (element stream)
      (let ((project (plist-get element :project)))
        (unless (and (not first) (equal project previous-project))
          (agent-repl-drawer--insert-merge-project-separator project)
          (setq previous-project project))
        (setq first nil)
        (agent-repl-drawer--insert-merge-commit element now)))))

(defun agent-repl-drawer--insert-content ()
  "Insert the drawer's full content into the current buffer.
Extracted from `--render' so `--render' can build content in a temp
buffer and diff-apply it.  Order: MAIN, HIDDEN, MERGING, MERGED —
the user reads the lifecycle top-to-bottom: in-progress (MAIN/HIDDEN)
→ changes-ready (MERGING) → completed (MERGED).
Reads buffer-local state (`--marked-set', `--expanded-set'); callers
must set those in the current buffer before calling."
  (let* ((current  (agent-repl-drawer--current-ws))
         (sections (agent-repl-drawer--partition-by-section
                    (agent-repl-drawer--visible-workspace-keys)))
         (stream   (agent-repl-drawer--merge-stream-visible
                    (agent-repl-drawer--merge-stream))))
    (insert "\n")
    ;; Omitted entirely when the queue is idle, following HIDDEN's precedent.
    ;; The drawer is a fixed fraction of the frame and vertical space is the
    ;; scarce resource, so a permanent `MERGE QUEUE (0) / (none)' block would
    ;; cost three lines forever to say nothing.
    (when stream
      (agent-repl-drawer--insert-merge-queue-section stream (float-time))
      (insert "\n"))
    (let ((mains    (alist-get :main    sections))
          (hiddens  (alist-get :hidden  sections))
          (mergings (alist-get :merging sections))
          (mergeds  (alist-get :merged  sections)))
      (agent-repl-drawer--insert-section
       (format "MAIN (%d)" (length mains))      mains    current :main)
      (insert "\n")
      (when hiddens
        (agent-repl-drawer--insert-section
         (format "HIDDEN (%d)" (length hiddens))  hiddens  current :hidden)
        (insert "\n"))
      (agent-repl-drawer--insert-section
       (format "MERGING (%d)" (length mergings)) mergings current :merging)
      (insert "\n")
      (agent-repl-drawer--insert-section
       (format "MERGED (%d)" (length mergeds))  mergeds  current :merged))))

(defvar-local agent-repl-drawer--last-render-signature 'unset
  "Signature of the inputs at the last successful `--render' build.
When the next `--render' call computes the same signature, the
temp-buffer build + content compare + cursor restore are all skipped
— the live buffer is already correct, so paying the build cost on
every 1Hz poll is wasted.  Sentinel value `unset' so the first render
always proceeds even if the natural signature happens to be nil.")

(defun agent-repl-drawer--render-signature ()
  "Return a cheap signature of every input that affects `--insert-content'.
Sorted on workspace names so the result is stable across hash-table
iteration order.  Captures the same plist values the render helpers
read (state, git/merge status, priority, summary, group), plus
marked/expanded sets per ws, the folded-repo set, and the current
workspace.  The folded set is global (not per-ws), so it is captured
once via `agent-repl--folded-repo-keys' — without it, a fold toggled
outside the drawer would not invalidate the cached render.

The MERGE QUEUE section adds three more inputs:

  - `agent-repl--merge-progress-seq', which every progress write bumps.
    One counter stands in for every field of the progress record, so a
    field added later cannot silently fail to redraw — the failure mode
    this signature already had for `:merging'.
  - the queue and in-flight lengths, so enqueue/dequeue/drain invalidate.
  - the current second, but ONLY while a merge is in flight, so the
    elapsed clock ticks without churning the signature when idle."
  (let (ws-sig)
    (dolist (ws (sort (agent-repl-drawer--visible-workspace-keys) #'string<))
      (push (list ws
                  (agent-repl--ws-get ws :repl-state)
                  (agent-repl--ws-get ws :agent-state)
                  (agent-repl--ws-get ws :git-clean)
                  (agent-repl--ws-get ws :branch-merged)
                  (agent-repl--ws-get ws :priority)
                  (agent-repl--ws-get ws :last-prompt-summary)
                  (agent-repl--ws-get ws :group-key)
                  (agent-repl--ws-get ws :merged-in-workspaces)
                  (agent-repl-drawer--marked-p ws)
                  (agent-repl-drawer--expanded-p ws))
            ws-sig))
    (list (agent-repl-drawer--current-ws)
          (agent-repl--folded-repo-keys)
          ws-sig
          agent-repl--merge-progress-seq
          (length agent-repl--merge-queue)
          (length agent-repl--in-flight-merges)
          (when agent-repl--in-flight-merges
            (floor (float-time))))))

(defun agent-repl-drawer--render ()
  "Render the drawer, skipping the buffer rewrite when content is unchanged.
First short-circuits on a cheap input signature (`--render-signature'):
when the signature matches the last successful render, the entire
temp-buffer build is skipped — the live buffer is already correct.
This is the 1Hz poll's idle-tick fast path: with the drawer open and
no state change, no allocation/insertion/comparison happens at all.

When the signature differs, builds the new content (with text
properties) in a temp buffer and compares it against the live
buffer's content.  When they match, the function is a true no-op for
the buffer — no `erase-buffer', no full re-insert, no full-buffer
redisplay.  Eliminates the gutter flicker visible during rapid
back-to-back renders: the persp-activated `--sync-cursor-to-current-ws'
followed by the deferred `--update-all-workspace-states' →
`--refresh-if-visible' double-fire.

`replace-buffer-contents' would skip the rewrite too, but its diff
algorithm preserves the destination buffer's text properties on
characters its LCS happens to match — leaving stale workspace text
properties pointing at a workspace whose visible text is gone.  A
string-equality check + clean erase-and-reinsert avoids that pitfall.

Anchors cursor restoration by entry identity, not just line number:
line numbers shift when an entry above the cursor expands/collapses,
appears/disappears, or has its repo folded between polls, and
`forward-line saved-line' can then land on a non-entry line (detail
line, blank, section header), which causes
`--update-current-entry-overlay' to delete the arrow.  Nested
children sit deeper in the buffer and so are most affected.  A repo
group header is an entry too, so a cursor parked on one survives the
re-render that its own fold toggle triggers.

Wrapped in `--with-dir-map' so every `--source-ws-name' lookup during
this render (signature compute, partition, tree build, max-depth, …)
amortizes against a single O(N) reverse-map build instead of N
independent `--ws-name-for-dir' scans."
  (agent-repl-drawer--with-dir-map
   (agent-repl-drawer--render-inner)))

(defun agent-repl-drawer--render-inner ()
  "Implementation half of `--render'; see that function for semantics.
Extracted so `--render' can wrap the body in `--with-dir-map' without
adding indentation noise to every line."
  (let ((sig (agent-repl-drawer--render-signature)))
    (unless (equal sig agent-repl-drawer--last-render-signature)
      (let* ((saved-line   (line-number-at-pos))
             (saved-col    (current-column))
             (saved-entry  (agent-repl-drawer--entry-at-point))
             (marked-set   agent-repl-drawer--marked-set)
             (expanded-set agent-repl-drawer--expanded-set)
             (new-content
              (with-temp-buffer
                ;; The render helpers consult these via buffer-local lookup;
                ;; mirror the source buffer's values so the temp render
                ;; reflects marks and expanded entries correctly.
                (setq-local agent-repl-drawer--marked-set marked-set)
                (setq-local agent-repl-drawer--expanded-set expanded-set)
                (agent-repl-drawer--insert-content)
                (buffer-substring (point-min) (point-max))))
             (current-content (buffer-substring (point-min) (point-max))))
        (unless (equal current-content new-content)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert new-content))
          (unless (and saved-entry
                       (agent-repl-drawer--goto-entry saved-entry))
            (goto-char (point-min))
            (forward-line (1- saved-line))
            (move-to-column saved-col))))
      (setq-local agent-repl-drawer--last-render-signature sig)))
  ;; Always refresh the overlay: point may have moved (e.g. via
  ;; `--sync-cursor-to-current-ws') even when content didn't change,
  ;; and `erase-buffer' above collapses the overlay to (1,1) when
  ;; content did change.
  (agent-repl-drawer--update-current-entry-overlay))

;;;; Navigation -------------------------------------------------------------

(defun agent-repl-drawer--workspace-at-point ()
  "Return the workspace name at point, or nil."
  (get-text-property (point) 'agent-repl-drawer-workspace))

(defun agent-repl-drawer--goto-entry (entry)
  "Move point to the start of ENTRY's first line, if present.
ENTRY is a `--entry-at' cell: `(:workspace . WS)' or `(:repo . KEY)'.
Returns non-nil on success."
  (let ((target nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not target) (not (eobp)))
        (when (equal (agent-repl-drawer--entry-at-point) entry)
          (setq target (point)))
        (forward-line 1)))
    (when target
      (goto-char target)
      t)))

(defun agent-repl-drawer--goto-workspace-line (ws)
  "Move point to the start of the line for workspace WS, if present.
Returns non-nil on success."
  (agent-repl-drawer--goto-entry (cons :workspace ws)))

(defun agent-repl-drawer--goto-repo-line (key)
  "Move point to the start of repo KEY's group header, if present.
Returns non-nil on success."
  (agent-repl-drawer--goto-entry (cons :repo key)))

(defun agent-repl-drawer-next ()
  "Move point to the next entry (workspace or repo group header)."
  (interactive)
  (let ((current (agent-repl-drawer--entry-at-point))
        (start   (point))
        (found   nil))
    (forward-line 1)
    (while (and (not found) (not (eobp)))
      (let ((entry (agent-repl-drawer--entry-at-point)))
        (if (and entry (not (equal entry current)))
            (setq found t)
          (forward-line 1))))
    (unless found
      (goto-char start))))

(defun agent-repl-drawer-prev ()
  "Move point to the previous entry (workspace or repo group header)."
  (interactive)
  (let ((current (agent-repl-drawer--entry-at-point))
        (start   (point))
        (found   nil))
    (forward-line -1)
    (while (and (not found) (not (bobp)))
      (let ((entry (agent-repl-drawer--entry-at-point)))
        (if (and entry (not (equal entry current)))
            (setq found t)
          (forward-line -1))))
    (when found
      ;; Snap to the start of the entry block (handles the summary
      ;; subtitle line being the first one we land on when moving up).
      (let ((entry (agent-repl-drawer--entry-at-point)))
        (while (and (not (bobp))
                    (equal (agent-repl-drawer--entry-at (1- (point))) entry))
          (forward-line -1))))
    (unless found
      (goto-char start))))

;;;; Commands ---------------------------------------------------------------

(defun agent-repl-drawer--leave-side-window-before-switch ()
  "Move out of the drawer side window before a persp switch.

Persp-mode's `persp-restore-window-conf' calls `persp-delete-other-windows'
with `ignore-window-parameters t' on the destination workspace.  When
the selected window is a side window (e.g. the drawer), persp picks an
arbitrary non-side fallback to anchor the sweep, then the subsequent
`window-state-put' restores the saved tree from that anchor.  In
practice the destination workspace's Claude panel windows can be
clobbered by the sweep before the put has a chance to restore them.

Pre-selecting a non-side main-area window sidesteps that path entirely
— the sweep+put happens with a normal anchor and the destination's
saved panels come back intact.

Selects the first live non-side window from `window-list'.  An earlier
version selected `window-main-window', but that returns an INTERNAL
\(non-live) window whenever the main area is split into two or more
windows, and handing a non-live window to `select-window' signals
`wrong-type-argument window-live-p' — the RET-in-drawer crash this
guards against.  `window-list' yields only live windows, so filtering
out the side windows leaves a live main-area leaf that serves as the
same non-side anchor the sweep needs.

No-op when the selected window is not a side window."
  (when (window-parameter (selected-window) 'window-side)
    (when-let ((target (seq-find (lambda (win)
                                   (not (window-parameter win 'window-side)))
                                 (window-list nil 'no-minibuf))))
      (select-window target))))

(defun agent-repl-drawer--reactivate-merged (ws)
  "Reactivate a MERGED workspace WS so it becomes a usable persp again.
Clears the `:merge-completed' / `:merge-completed-at' / `:merge-failed'
flags and the `:merged'/`:merge-failed' repl-state so the drawer stops
bucketing WS under MERGED, persists the cleared flags via
`--state-save', then re-establishes the persp + Claude session via
`--establish-workspace'.

The git cherry-pick performed by the original merge is not reverted
— reactivation only un-marks the workspace as completed in the UI."
  (let ((dir (agent-repl--ws-get ws :project-dir)))
    (unless (and dir (file-directory-p dir))
      (user-error "Cannot reactivate %s — project-dir missing or invalid (%s)"
                  ws (or dir "nil")))
    (agent-repl--log ws "drawer-visit: reactivating MERGED ws=%s dir=%s" ws dir)
    (agent-repl--ws-put ws :merge-completed nil)
    (agent-repl--ws-put ws :merge-completed-at nil)
    (agent-repl--ws-put ws :merge-failed nil)
    (agent-repl--ws-put ws :repl-state nil)
    (agent-repl--state-save ws)
    (agent-repl-drawer--leave-side-window-before-switch)
    (agent-repl--establish-workspace ws dir)))

(defun agent-repl-drawer-visit ()
  "Switch to the workspace at point.

When the workspace is in the MERGED section its persp has been torn
down, so plain `+workspace-switch' would fail.  Dispatch to
`--reactivate-merged' instead, which re-establishes the persp + Claude
session and un-marks the workspace as merge-completed."
  (interactive)
  (let ((ws (agent-repl-drawer--workspace-at-point)))
    (unless ws
      (user-error "No workspace at point"))
    (agent-repl--log ws "drawer-visit: ws=%s" ws)
    (cond
     ((eq (agent-repl-drawer--workspace-section ws) :merged)
      (agent-repl-drawer--reactivate-merged ws))
     (t
      (agent-repl-drawer--leave-side-window-before-switch)
      (agent-repl--ws-switch ws)))))

(defun agent-repl-drawer-refresh ()
  "Manually refresh the drawer contents.
Also refreshes the detail cache for any currently-expanded entries
so their git-derived fields (commits ahead, last commit, etc.) are
re-fetched."
  (interactive)
  (when-let ((buf (get-buffer agent-repl-drawer-buffer-name)))
    (with-current-buffer buf
      (when agent-repl-drawer--expanded-set
        (maphash (lambda (ws _)
                   (agent-repl-drawer--refresh-detail-cache ws))
                 agent-repl-drawer--expanded-set))
      (agent-repl-drawer--render))))

(defun agent-repl-drawer--require-ws-at-point ()
  "Return the workspace name at point, or signal a user-error."
  (or (agent-repl-drawer--workspace-at-point)
      (user-error "No workspace at point")))

;;;; Multi-select -----------------------------------------------------------

(defvar-local agent-repl-drawer--marked-set nil
  "Hash table of workspace names currently marked for bulk operations.
Buffer-local: each drawer buffer has its own set.  Keys are workspace
names, values are `t' (presence is the signal).")

(defun agent-repl-drawer--ensure-marked-set ()
  "Initialize `agent-repl-drawer--marked-set' if not yet created."
  (unless agent-repl-drawer--marked-set
    (setq-local agent-repl-drawer--marked-set
                (make-hash-table :test 'equal))))

(defun agent-repl-drawer--marked-p (ws)
  "Return non-nil when WS is in the marked-set."
  (and agent-repl-drawer--marked-set
       (gethash ws agent-repl-drawer--marked-set)))

(defun agent-repl-drawer--marked-count ()
  "Return the number of marked entries in the current drawer buffer."
  (if agent-repl-drawer--marked-set
      (hash-table-count agent-repl-drawer--marked-set)
    0))

(defun agent-repl-drawer--target-workspaces ()
  "Return the list of workspaces an action should target.
The marked-set if non-empty; otherwise just the entry at point.  This
is the standard 'act on marks if any, else on point' idiom — no
duplicate keybindings for bulk versions."
  (if (> (agent-repl-drawer--marked-count) 0)
      (hash-table-keys agent-repl-drawer--marked-set)
    (list (agent-repl-drawer--require-ws-at-point))))

(defun agent-repl-drawer--reject-merged-targets (targets action)
  "Signal `user-error' if any TARGETS workspace is in the MERGED section.
ACTION is a short noun naming the rejected operation (used in the
error message).  MERGED entries are removed only via the drawer `x'
key, which dispatches to `--finish-workspace'."
  (when-let ((merged (cl-remove-if-not
                      (lambda (ws)
                        (eq (agent-repl-drawer--workspace-section ws) :merged))
                      targets)))
    (user-error "Cannot %s a MERGED workspace (%s) — press `x' to finish it instead"
                action (mapconcat #'identity merged ", "))))

(defun agent-repl-drawer-toggle-mark ()
  "Toggle the mark on the entry at point.
Marked entries render with a red `●' in the gutter and become the
target set for action keys (x/d/i/M).  Auto-advances to the next
entry as a quality-of-life convenience."
  (interactive)
  (let ((ws (agent-repl-drawer--require-ws-at-point)))
    (agent-repl-drawer--ensure-marked-set)
    (if (gethash ws agent-repl-drawer--marked-set)
        (remhash ws agent-repl-drawer--marked-set)
      (puthash ws t agent-repl-drawer--marked-set))
    (agent-repl-drawer--render)
    (agent-repl-drawer-next)))

(defun agent-repl-drawer-clear-marks ()
  "Clear all marks in the current drawer buffer."
  (interactive)
  (when agent-repl-drawer--marked-set
    (clrhash agent-repl-drawer--marked-set))
  (agent-repl-drawer--render))

(defun agent-repl-drawer-nuke ()
  "Nuke the target workspaces.
Targets the marked-set when non-empty, otherwise the entry at point.
Mirrors `SPC j x' (`agent-repl-nuke-workspace') per target.

Routes per-entry by section: MERGED entries dispatch to
`agent-repl--finish-workspace' (the only way to drop a workspace out
of the MERGED bucket — removes the git worktree and the hash entry).
Non-MERGED entries take the standard `agent-repl-nuke-workspace'
path (preserves the worktree)."
  (interactive)
  (dolist (ws (agent-repl-drawer--target-workspaces))
    (if (eq (agent-repl-drawer--workspace-section ws) :merged)
        (when (y-or-n-p
               (format "Finish merged workspace '%s'? This removes the worktree directory and the hash entry. "
                       ws))
          (agent-repl--finish-workspace ws))
      (agent-repl-nuke-workspace ws))))

(defun agent-repl-drawer-kill ()
  "Kill the target workspaces.
Targets the marked-set when non-empty, otherwise the entry at point.
Mirrors `SPC j d' (`agent-repl-kill-workspace') per target.

Refuses to act on MERGED entries — `x' is the sole removal path for
those (and dispatches to `--finish-workspace')."
  (interactive)
  (let ((targets (agent-repl-drawer--target-workspaces)))
    (agent-repl-drawer--reject-merged-targets targets "kill")
    (dolist (ws targets)
      (agent-repl-kill-workspace ws))))

(defun agent-repl-drawer-interrupt ()
  "Interrupt Claude in the target workspaces.
Targets the marked-set when non-empty, otherwise the entry at point.
Mirrors `C-c C-k' per target."
  (interactive)
  (dolist (ws (agent-repl-drawer--target-workspaces))
    (agent-repl-interrupt ws)))

(defun agent-repl-drawer-send-prompt ()
  "Read a prompt and send it to the target workspaces.
Targets the marked-set when non-empty, otherwise the entry at point.
Mirrors the normal claude send (`agent-repl--send'), including
history logging.  After send, each target's summary transitions to
`:last-prompt-summary-pending' and renders as `…' until the haiku
summarizer returns the new aiTitle.

Refuses to act on MERGED entries (no live Claude to receive the prompt)."
  (interactive)
  (let* ((targets (agent-repl-drawer--target-workspaces))
         (_       (agent-repl-drawer--reject-merged-targets targets "send to"))
         (prompt  (read-string
                   (if (= 1 (length targets))
                       (format "Send to %s: " (car targets))
                     (format "Send to %d workspaces: " (length targets))))))
    (when (and prompt (not (string-empty-p prompt)))
      (dolist (ws targets)
        (agent-repl--send prompt ws)))))

(defun agent-repl-drawer--with-temp-current-ws (ws fn)
  "Switch to WS, call FN, then return to the previous workspace.
Used to dispatch merge commands for the entry at point — the merge
public functions read `(agent-repl--ws-current-name)' internally and
switch perspectives themselves, so we must temporarily inhabit the
target workspace before invoking them.

When WS is in the MERGED section its persp has been torn down (a
plain `+workspace-switch' would fail), so we first reactivate it
via `--reactivate-merged' — the same code path `drawer-visit'
uses on a MERGED entry.  This lets the drawer dispatch merge
commands on MERGED workspaces (e.g. when a prior cherry-pick
silently failed and the workspace was marked merged anyway).

Leaves the drawer side window before each switch (see
`agent-repl-drawer--leave-side-window-before-switch') so persp's
restore doesn't clobber the destination workspace's panel state."
  (let ((prev (agent-repl--ws-current-name)))
    (agent-repl-drawer--leave-side-window-before-switch)
    (if (eq (agent-repl-drawer--workspace-section ws) :merged)
        (agent-repl-drawer--reactivate-merged ws)
      (agent-repl--ws-switch ws))
    (unwind-protect
        (funcall fn)
      (when (and prev (not (equal prev (agent-repl--ws-current-name))))
        (agent-repl-drawer--leave-side-window-before-switch)
        (agent-repl--ws-switch prev)))))

(defun agent-repl-drawer-merge-into-master ()
  "Merge the target workspaces into their source/master.
Targets the marked-set when non-empty, otherwise the entry at point.
Mirrors `SPC TAB M' (`agent-repl-workspace-merge-current-into-source')
per target.  Each target requires temporarily switching to that
workspace before invoking the public function.

MERGED entries are accepted: a prior cherry-pick may have silently
failed yet still flipped the workspace into MERGED, so re-attempts
must be possible.  `--with-temp-current-ws' reactivates a MERGED
target before switching so the public merge function runs against a
live persp — the same flow as `drawer-visit' + `SPC TAB M'."
  (interactive)
  (dolist (ws (agent-repl-drawer--target-workspaces))
    (agent-repl-drawer--with-temp-current-ws
     ws #'agent-repl-workspace-merge-current-into-source)))

(defun agent-repl-drawer-merge-child ()
  "Merge a child workspace into the entry at point.
Mirrors `SPC TAB m' (`agent-repl-workspace-merge').  The public
function uses the current workspace as the merge destination and
prompts for the child to merge in, so we temporarily switch to the
entry-at-point before invoking it.

MERGED entries are accepted as the destination: a prior cherry-pick
may have silently failed yet still flipped the workspace into
MERGED, so re-attempts must be possible.  `--with-temp-current-ws'
reactivates a MERGED entry before switching."
  (interactive)
  (let ((ws (agent-repl-drawer--require-ws-at-point)))
    (agent-repl-drawer--with-temp-current-ws
     ws #'agent-repl-workspace-merge)))

(defun agent-repl-drawer-new-child ()
  "Create a new worktree branched from the entry at point.
Mirrors `SPC TAB n' (`agent-repl-create-worktree-workspace') with
BASE = `head' and SOURCE-WS = entry-at-point.  The public function
prompts for the preemptive prompt and dispatches to the async
workspace-generation skill.

Refuses when the entry at point is in the MERGED section — branching
from a merged-and-torn-down workspace would re-resurrect a stale tree."
  (interactive)
  (let ((ws (agent-repl-drawer--require-ws-at-point)))
    (agent-repl-drawer--reject-merged-targets (list ws) "create child from")
    (agent-repl-create-worktree-workspace 'head ws)))

(defcustom agent-repl-drawer-priority-cycle
  '("p05" "p1" "p2" "p3" nil)
  "Ordered list (highest → lowest) used by drawer `+'/`-' priority cycling.
The trailing `nil' represents 'no priority'.  Cycle wraps at both ends."
  :type '(repeat (choice string (const nil)))
  :group 'agent-repl)

(defun agent-repl-drawer--cycle-priority (ws step)
  "Cycle WS's priority by STEP through `agent-repl-drawer-priority-cycle'.
STEP is -1 (toward the head of the cycle, e.g. p05) or +1 (toward
the tail, e.g. nil).  Calls `agent-repl-set-priority' with the new
value (empty string when cycling to nil, since that's set-priority's
clear sentinel)."
  (let* ((cur (agent-repl--ws-get ws :priority))
         (cycle agent-repl-drawer-priority-cycle)
         (n (length cycle))
         (idx (or (cl-position cur cycle :test #'equal) (1- n)))
         (new-idx (mod (+ idx step) n))
         (new (nth new-idx cycle))
         (new-arg (or new "")))
    (agent-repl-set-priority new-arg ws)))

(defun agent-repl-drawer-priority-up ()
  "Cycle the entry-at-point's priority up (toward p05)."
  (interactive)
  (agent-repl-drawer--cycle-priority
   (agent-repl-drawer--require-ws-at-point) -1)
  (agent-repl-drawer-refresh))

(defun agent-repl-drawer-priority-down ()
  "Cycle the entry-at-point's priority down (toward nil)."
  (interactive)
  (agent-repl-drawer--cycle-priority
   (agent-repl-drawer--require-ws-at-point) +1)
  (agent-repl-drawer-refresh))

(defun agent-repl-drawer-toggle-hidden ()
  "Toggle the entry-at-point's hidden state.
When `:repl-state' is `:hidden', calls `agent-repl--unhide-workspace'
to flip it back to `:active'.  Otherwise calls `agent-repl--on-close'
which sets `:hidden' (the deprio-close path).  Refreshes the drawer
so the entry moves between MAIN and HIDDEN sections."
  (interactive)
  (let* ((ws     (agent-repl-drawer--require-ws-at-point))
         (rstate (agent-repl--ws-get ws :repl-state)))
    (if (eq rstate :hidden)
        (agent-repl--unhide-workspace ws)
      (agent-repl--on-close ws))
    (agent-repl-drawer-refresh)))

(defun agent-repl-drawer-new-fork ()
  "Fork the claude session of the entry at point into a new worktree.
Mirrors `SPC TAB f' (`agent-repl-fork-worktree-workspace') with
SOURCE-WS = entry-at-point.

Refuses when the entry at point is in the MERGED section — the source
session has been torn down by the merge, so there's nothing to fork."
  (interactive)
  (let ((ws (agent-repl-drawer--require-ws-at-point)))
    (agent-repl-drawer--reject-merged-targets (list ws) "fork from")
    (agent-repl-fork-worktree-workspace ws)))

(defun agent-repl-drawer--refresh-if-visible ()
  "Refresh the drawer if its buffer exists and is shown in some window.
Intended to be called from the 1Hz poll in `status.el'."
  (when-let* ((buf (get-buffer agent-repl-drawer-buffer-name))
              ((get-buffer-window buf t)))
    (with-current-buffer buf
      (agent-repl-drawer--render)
      ;; The poll renders via `with-current-buffer', which moves only the
      ;; buffer's point.  An unfocused drawer window keeps its own
      ;; `window-point', and the `erase-buffer' inside `--render'
      ;; collapses that window-point to `point-min' — so without this the
      ;; drawer cursor snaps to the top on every content-changing poll
      ;; while the user is focused elsewhere.  Mirror the restored
      ;; buffer-point onto every live drawer window (matching the
      ;; `set-window-point' idiom in `--show--inner' and
      ;; `--call-in-drawer').  The selected-window case is a no-op since
      ;; its window-point already tracks the buffer's point.
      (dolist (win (get-buffer-window-list buf nil t))
        (when (window-live-p win)
          (set-window-point win (point)))))))

;;;; Display + toggle -------------------------------------------------------

;;;; Expand-detail ----------------------------------------------------------

(defvar-local agent-repl-drawer--expanded-set nil
  "Hash table of workspace names currently expanded in detail view.
Buffer-local: one set per drawer buffer.  Keys are workspace names,
values are `t' (presence is the signal).")

(defun agent-repl-drawer--ensure-expanded-set ()
  "Create `agent-repl-drawer--expanded-set' if not yet initialized."
  (unless agent-repl-drawer--expanded-set
    (setq-local agent-repl-drawer--expanded-set
                (make-hash-table :test 'equal))))

(defun agent-repl-drawer--expanded-p (ws)
  "Return non-nil if WS is currently expanded."
  (and agent-repl-drawer--expanded-set
       (gethash ws agent-repl-drawer--expanded-set)))

(defun agent-repl-drawer--unmerged-ahead-count (dir upstream)
  "Return how many of DIR's HEAD commits UPSTREAM does not already carry.
Counts by patch-id equivalence (`git rev-list --cherry-pick
--right-only UPSTREAM...HEAD'), never by SHA ancestry.

Ancestry is the wrong measure here because `SPC TAB M' lands a
workspace by CHERRY-PICKING its commits into the target: the copies on
UPSTREAM carry fresh SHAs, so the SHA-ancestry range `UPSTREAM..HEAD'
keeps reporting every already-landed commit as still ahead.  That is
what produced the drawer's self-contradicting \"merged into: master\"
+ \"ahead master: 58\" pair.  `--cherry-pick' drops commits whose patch
already exists upstream, so a fully-landed workspace reads 0.

Returns nil when git prints nothing (unknown UPSTREAM, unreadable
worktree), leaving the caller's cache entry unset rather than
reporting a guessed count."
  (let ((out (agent-repl--git-string-quiet
              "-C" dir "rev-list" "--count" "--right-only" "--cherry-pick"
              (concat upstream "...HEAD"))))
    (and out (not (string-empty-p out))
         (string-to-number out))))

(defun agent-repl-drawer--refresh-detail-cache (ws)
  "Populate WS's `:detail-*' plist fields with synchronous git calls.
Called from TAB-toggle (when expanding) and `g'-refresh.  Avoids
running git every poll cycle.  All values are best-effort: nil left
in place when the underlying command errors or returns empty.

The two ahead-counts go through `--unmerged-ahead-count', so commits
already cherry-picked onto the trunk or onto the source branch do not
count as ahead of it."
  (when-let ((dir (ignore-errors (agent-repl--ws-dir ws))))
    (let* ((branch (agent-repl--git-string-quiet
                    "-C" dir "rev-parse" "--abbrev-ref" "HEAD")))
      (agent-repl--ws-put ws :detail-branch
                           (and branch (not (string-empty-p branch)) branch)))
    (agent-repl--ws-put ws :detail-master-ahead
                         (agent-repl-drawer--unmerged-ahead-count
                          dir agent-repl-master-branch-name))
    (when-let* ((src-dir (agent-repl--ws-get ws :source-ws-dir))
                ((file-directory-p src-dir))
                (src-branch (agent-repl--git-string-quiet
                             "-C" src-dir "rev-parse" "--abbrev-ref" "HEAD")))
      (when (and src-branch (not (string-empty-p src-branch))
                 (not (string-prefix-p "fatal" src-branch)))
        ;; Cached alongside the count so the detail line can name the
        ;; branch it is counting against ("ahead DWC/foo:") instead of
        ;; the anonymous "ahead source:".
        (agent-repl--ws-put ws :detail-source-branch src-branch)
        (agent-repl--ws-put ws :detail-source-ahead
                             (agent-repl-drawer--unmerged-ahead-count
                              dir src-branch))))
    (let ((subj (agent-repl--git-string-quiet
                 "-C" dir "log" "-1" "--pretty=format:%s")))
      (agent-repl--ws-put ws :detail-last-commit
                           (and subj (not (string-empty-p subj)) subj)))
    (let ((tm (agent-repl--git-string-quiet
               "-C" dir "log" "-1" "--pretty=format:%ar")))
      (agent-repl--ws-put ws :detail-last-commit-time
                           (and tm (not (string-empty-p tm)) tm)))
    (let ((status (agent-repl--git-string-quiet
                   "-C" dir "status" "--porcelain")))
      (agent-repl--ws-put ws :detail-dirty-count
                           (if (or (null status) (string-empty-p status))
                               0
                             (length (split-string status "\n" t)))))))

(defun agent-repl-drawer--format-duration (seconds)
  "Format SECONDS as a short human-readable duration."
  (cond
   ((< seconds 60)    (format "%ds ago"  (round seconds)))
   ((< seconds 3600)  (format "%dm ago"  (round (/ seconds 60))))
   ((< seconds 86400) (format "%.1fh ago" (/ seconds 3600.0)))
   (t                 (format "%.1fd ago" (/ seconds 86400.0)))))

(defun agent-repl-drawer--toggle-repo-fold (key)
  "Fold or unfold repo KEY, then re-render the drawer and the tab-bar.
Folding is global state (`agent-repl--folded-repos'), so the tab-bar
must repaint too: a folded repo's workspaces leave the tab-bar and the
survivors' 1-based numbers close up.  `agent-repl--force-tab-bar-redraw'
is what makes that visible immediately rather than on the next 1Hz
poll."
  (agent-repl--toggle-repo-fold key)
  (agent-repl-drawer--render)
  (agent-repl--force-tab-bar-redraw))

(defun agent-repl-drawer-toggle-expand ()
  "Toggle the entry at point.

Dispatches on the entry kind (see `agent-repl-drawer--entry-at'):

  - Repo group header → fold/unfold the repo.  A folded repo hides its
    workspaces from the drawer AND from the tab-bar, and the tab-bar's
    selection numbers close up over the remaining workspaces.
  - Workspace → toggle its expanded detail view.  On expand, refreshes
    the detail cache (synchronous git calls); on collapse, removes it
    from the expanded set.

Re-renders the drawer either way."
  (interactive)
  (pcase (agent-repl-drawer--entry-at-point)
    (`(:repo . ,key)
     (agent-repl-drawer--toggle-repo-fold key))
    (`(:workspace . ,ws)
     (agent-repl-drawer--ensure-expanded-set)
     (if (gethash ws agent-repl-drawer--expanded-set)
         (remhash ws agent-repl-drawer--expanded-set)
       (agent-repl-drawer--refresh-detail-cache ws)
       (puthash ws t agent-repl-drawer--expanded-set))
     (agent-repl-drawer--render))
    (_ (user-error "No entry at point"))))

(defun agent-repl-drawer--merge-status-text (ws)
  "Return a brief merge-status string for WS, or nil when WS is not merging.
Distinguishes the two MERGING-section render-states via
`agent-repl--ws-render-status':

  - `:merging'      → \"update in progress\" (worker thread's cherry-pick
                      is live).
  - `:merge-queued' → \"update queued\" (parked on the merge queue,
                      waiting for an in-flight cherry-pick to clear).

When the count of commits the workspace is ahead of its merge source is
cached in `:detail-source-ahead' (populated by
`--refresh-detail-cache'), appends it as \"N commit(s)\" so the reader
sees the size of the merge being cherry-picked.  The count is omitted
when the cache is empty (e.g. the source worktree is gone) rather than
guessed.

Returns nil for every non-MERGING render-state, so only MERGING-section
entries render a status line."
  (let ((status (agent-repl--ws-render-status ws)))
    (when (memq status '(:merging :merge-queued))
      (let ((phase (if (eq status :merging)
                       "update in progress"
                     "update queued"))
            (n (agent-repl--ws-get ws :detail-source-ahead)))
        (if (and (integerp n) (> n 0))
            (format "%s · %d commit%s" phase n (if (= n 1) "" "s"))
          phase)))))

(defun agent-repl-drawer--render-detail-lines (ws depth)
  "Insert detail lines for an expanded WS at DEPTH.
Reads only cached `:detail-*' fields and existing plist values; never
invokes git.  Caller is `--render-workspace-expanded'."
  (let* ((indent-str (make-string (* depth agent-repl-drawer-indent-per-level) ?\s))
         (detail-prefix (concat agent-repl-drawer-gutter indent-str "    "))
         (branch       (agent-repl--ws-get ws :detail-branch))
         (merge-target (and (eq (agent-repl-drawer--workspace-section ws) :merged)
                            (agent-repl--ws-get ws :merge-target-name)))
         (master-ahead (agent-repl--ws-get ws :detail-master-ahead))
         (source-branch (agent-repl--ws-get ws :detail-source-branch))
         (source-ahead (agent-repl--ws-get ws :detail-source-ahead))
         (last-commit  (agent-repl--ws-get ws :detail-last-commit))
         (last-commit-time (agent-repl--ws-get ws :detail-last-commit-time))
         (dirty-count  (agent-repl--ws-get ws :detail-dirty-count))
         (last-prompt-time (agent-repl--ws-get ws :last-prompt-time))
         (pending-count (length (agent-repl--ws-get ws :pending-prompts)))
         (merge-status (agent-repl-drawer--merge-status-text ws))
         (merged-in    (agent-repl--ws-get ws :merged-in-workspaces)))
    (cl-flet ((line (label value face)
                (insert detail-prefix
                        (propertize (concat label " ") 'face 'shadow)
                        (propertize (format "%s" value)
                                    'face face
                                    'wrap-prefix detail-prefix)
                        "\n")))
      ;; Headline status for MERGING-section entries: in-flight vs queued
      ;; plus the pending commit count.  Rendered first so the merge phase
      ;; is the first thing the reader sees when the entry auto-expands.
      (when merge-status
        (line "merge:" merge-status 'agent-repl-drawer-detail-merge-status))
      (when branch
        (line "branch:" branch 'agent-repl-drawer-detail-branch))
      (when merge-target
        (line "merged into:" merge-target
              'agent-repl-drawer-detail-merge-target))
      (when master-ahead
        (line (format "ahead %s:" agent-repl-master-branch-name)
              (format "%d" master-ahead)
              'agent-repl-drawer-detail-ahead-master))
      ;; Named after the actual source branch ("ahead DWC/foo:"), never
      ;; the anonymous "ahead source:".  Suppressed entirely when the
      ;; source branch IS the trunk, since the line would then restate
      ;; the "ahead <master>" line above verbatim.
      (when (and source-ahead source-branch
                 (not (string= source-branch agent-repl-master-branch-name)))
        (line (format "ahead %s:" source-branch) (format "%d" source-ahead)
              'agent-repl-drawer-detail-ahead-source))
      (when last-commit
        (line "last commit:"
              (if last-commit-time
                  (format "%s (%s)" last-commit last-commit-time)
                last-commit)
              'agent-repl-drawer-detail-last-commit))
      (when (and dirty-count (> dirty-count 0))
        (line "dirty:" (format "%d files" dirty-count)
              'agent-repl-drawer-detail-dirty))
      (when last-prompt-time
        (line "last prompt:"
              (agent-repl-drawer--format-duration
               (- (float-time) last-prompt-time))
              'agent-repl-drawer-detail-last-prompt))
      (when (and pending-count (> pending-count 0))
        (line "pending:" (format "%d prompt(s)" pending-count)
              'agent-repl-drawer-detail-pending))
      ;; List every workspace whose commits were successfully merged
      ;; into WS.  One "merged in:" line per workspace so the list stays
      ;; scannable in the narrow drawer regardless of length.
      (dolist (merged-ws merged-in)
        (line "merged in:" merged-ws
              'agent-repl-drawer-detail-merged-in)))))

(defvar agent-repl-drawer--display-action
  `((display-buffer-in-side-window)
    (side . left)
    (slot . 0)
    (window-width . ,#'agent-repl-drawer--window-width)
    (window-parameters
     (no-delete-other-windows . t)
     (no-other-window . t)))
  "Display action for the drawer buffer.

`no-other-window' is t so the drawer is invisible to `other-window',
`window-in-direction', and every `display-buffer' action that reuses
\"some other window\".  This matches the drawer's keyboard-inaccessible
design (mouse-click only; see the bounce hook) AND — critically — keeps
buffer-display machinery from ever repurposing the dedicated drawer.

The concrete bug this closes: Doom's `+magit--display-buffer-fn' routes
diff/revision buffers (e.g. RET on a commit in `magit-status') through
`+magit--display-buffer-in-direction'.  With no window in the primary
direction (the drawer sits to the *left* of the main window, so a
rightward split has nothing there), that handler falls back to the
window in the *opposite* direction — which was the drawer — then calls
`switch-to-buffer' in it.  The drawer is a dedicated side window, so
`switch-to-buffer' signals \"Cannot switch buffers in a dedicated
window\" and the diff never opens.  With `no-other-window' t,
`window-in-direction' skips the drawer, the handler splits the main
window instead, and the diff opens normally.")

(defun agent-repl-drawer--window-width (window)
  "Return the constant drawer width in columns for WINDOW.
The width is always `round(agent-repl-drawer-width-fraction ×
frame-width)', floored at 1.  It is fully determined by the fraction
and the frame width, so it is constant at runtime: there is no
manual-resize override and no depth-based adjustment, and the only
thing that changes it is editing the fraction (or resizing the
frame)."
  (let* ((frame-cols (frame-width (window-frame window)))
         (width      (round (* agent-repl-drawer-width-fraction frame-cols))))
    (max 1 width)))

(defun agent-repl-drawer--get-or-create-buffer ()
  "Return the drawer buffer, creating and initializing if necessary."
  (let ((buf (get-buffer agent-repl-drawer-buffer-name)))
    (unless (buffer-live-p buf)
      (setq buf (get-buffer-create agent-repl-drawer-buffer-name))
      (with-current-buffer buf
        (agent-repl-drawer-mode)))
    buf))

(defun agent-repl-drawer--goto-first-workspace ()
  "Move point to the first workspace line in the current buffer, if any."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (if (agent-repl-drawer--workspace-at-point)
          (setq found t)
        (forward-line 1)))
    found))

(defun agent-repl-drawer--apply-width (window)
  "Resize WINDOW to the constant drawer width.
Side-window action alists honor `window-width' only at window-creation
time, so a re-shown drawer keeps its old width even when the fraction
changed.  This re-applies the same constant width on every show.

Uses `shrink-window'/`enlarge-window' rather than `window-resize'
because side windows route through `window--resize-side-windows',
which silently rejects the direct `window-resize' path under
constraints (parent window slack, fixed-size flags, etc.) — the
shrink/enlarge wrappers go through the side-window aware codepath
and actually apply the delta.  Locally lowers `window-min-width' so
fractions below the global default (10 cols) are honored, and clears
`window-size-fixed' on the buffer in case a prior pass locked it."
  (let* ((target (agent-repl-drawer--window-width window))
         (window-min-width 1))
    (with-selected-window window
      (setq-local window-size-fixed nil)
      (let ((delta (- target (window-total-width window))))
        (cond
         ((> delta 0) (enlarge-window delta t))
         ((< delta 0) (shrink-window (abs delta) t)))))))

(defvar agent-repl-drawer--global-visible-p nil
  "Non-nil when the drawer should appear in every workspace/persp.
Set by `agent-repl-drawer-show', cleared by
`agent-repl-drawer-hide'.  The persp-activated hook
(`agent-repl-drawer--ensure-visible-on-persp-switch') consults this
flag and re-displays the drawer in newly-activated workspaces so the
drawer feels like a frame-level UI element rather than a per-workspace
artifact.")

(defun agent-repl-drawer-show ()
  "Show the workspace drawer in a left-side window.
Positions the drawer cursor on the currently selected workspace
(falling back to the first entry) WITHOUT selecting the drawer
window — the drawer is keyboard-inaccessible by policy, so callers
stay where they are and must mouse-click into the drawer to operate
it directly.  Sets the
global visible-flag so the drawer follows the user across workspace
switches.  Self-heals if an existing drawer buffer pre-dates the
current mode init by ensuring the overlay-driving post-command hook
is installed and firing it once so the arrow is positioned
immediately, not after the next command.

Wrapped in `--with-dir-map' at the outermost level so the side-window
display-action's `--window-width' callback, the `--render' that
follows, and the post-display `--apply-width' all share a single
O(N) reverse-lookup build instead of paying it three times."
  (interactive)
  (agent-repl-drawer--with-dir-map
   (agent-repl-drawer-show--inner)))

(defun agent-repl-drawer-show--inner ()
  "Implementation half of `agent-repl-drawer-show'.
Split out so the public entry point can wrap the body in
`--with-dir-map' without adding indentation noise to every line."
  (let* ((buf        (agent-repl-drawer--get-or-create-buffer))
         (current-ws (agent-repl-drawer--current-ws))
         (win        (display-buffer buf agent-repl-drawer--display-action)))
    (with-current-buffer buf
      (add-hook 'post-command-hook
                #'agent-repl-drawer--post-command nil t)
      (setq-local cursor-type nil)
      ;; Self-heal: pre-existing buffers from before the wrap rollout
      ;; still have `truncate-lines' = t.  These are buffer-local; the
      ;; mode-init only fires on first activation.
      (setq-local truncate-lines nil
                  word-wrap t)
      (agent-repl-drawer--apply-background)
      ;; Expand the current workspace so its detail lines are visible
      ;; immediately on open, but ONLY when it sits in the MERGING
      ;; section — every other entry stays folded (see
      ;; `agent-repl-drawer--auto-expand-p').  Refresh the detail cache
      ;; only when the workspace was not already expanded to avoid
      ;; redundant git calls.
      (when (and current-ws
                 (agent-repl-drawer--auto-expand-p current-ws))
        (agent-repl-drawer--ensure-expanded-set)
        (unless (gethash current-ws agent-repl-drawer--expanded-set)
          (agent-repl-drawer--refresh-detail-cache current-ws)
          (puthash current-ws t agent-repl-drawer--expanded-set)))
      (agent-repl-drawer--render)
      ;; Position cursor on current-ws (or first entry) without
      ;; selecting the drawer window — the bounce hook would redirect
      ;; us away anyway, so do it directly via `set-window-point'.
      (or (and current-ws
               (agent-repl-drawer--goto-workspace-line current-ws))
          (agent-repl-drawer--goto-first-workspace))
      (when (and win (window-live-p win))
        (set-window-point win (point)))
      (agent-repl-drawer--post-command))
    (when win
      ;; Drawer hardening recipe: dedicated (no display-buffer repurpose);
      ;; fringes 0/0 to suppress the wrap-continuation arrow.
      ;; `no-delete-other-windows' and `no-other-window' are set
      ;; declaratively via the display-action's `window-parameters', so
      ;; they aren't repeated here.
      (agent-repl-window--harden win :dedicate t :fringes 0)
      (agent-repl-drawer--apply-width win))
    (setq agent-repl-drawer--global-visible-p t)
    win))

(defun agent-repl-drawer-hide ()
  "Hide the workspace drawer.
Clears the global visible-flag so the drawer no longer auto-appears
on workspace switches.

Each drawer window is explicitly un-dedicated before deletion — the
display-action gives the drawer regular (`t') dedication which does
NOT block `delete-window' in modern Emacs, but the un-dedicate is
preserved as defense against legacy strong-dedication code paths
that may have set a non-`t' value.  Delegates the deletion itself to
`agent-repl-window--delete-buffer-windows', which targets the
buffer specifically and bypasses the side-window skip — drawer
hiding genuinely wants to delete a side window."
  (interactive)
  (setq agent-repl-drawer--global-visible-p nil)
  (when-let ((buf (get-buffer agent-repl-drawer-buffer-name)))
    (dolist (win (get-buffer-window-list buf nil t))
      (when (window-live-p win)
        (set-window-dedicated-p win nil)))
    (agent-repl-window--delete-buffer-windows buf)))

;;;; Global drawer-mirror dispatch -----------------------------------------

(defun agent-repl-drawer--call-in-drawer (fn &optional preserve-cursor)
  "Call FN with the drawer buffer current; sync window-point + overlay
afterward without selecting the drawer window.

When PRESERVE-CURSOR is non-nil, snapshots the entry at point (a
workspace or a repo group header) before FN runs and re-positions the
cursor onto that same entry afterward.  Without this, side effects of
FN can move the drawer cursor off the user's navigated entry:
`+workspace-switch' triggers
`--sync-cursor-to-current-ws' which snaps to the newly-active ws;
persp's window-config restoration can install a fresh drawer window
with a stale `window-point' from the saved config; a re-render whose
`saved-entry' anchor is no longer present in the buffer falls back to a
line-number heuristic that may land outside any entry.  Preservation
makes all non-navigational C-S-<key> dispatchers (visit, nuke, kill,
toggle-hidden, …) keep the drawer cursor where the user pointed it.
When the snapshot entry no longer exists in the buffer after FN
(e.g. after `agent-repl-drawer-nuke'), the cursor is left wherever
FN naturally placed it.

We deliberately avoid `with-selected-window' here.  Selecting and then
unselecting the drawer window for every keystroke fires window-selection
hooks and redisplays the modeline/hl-line/etc., which adds overhead vs.
in-drawer `j' / `k' (those just move point inside the already-selected
window).  Non-navigational dispatchers (visit, nuke, kill, …) fire
sparsely enough that the cost would be invisible, but the visual side
effect — a sticky `hl-line' overlay left on whatever entry the dispatch
happened to touch — would be wrong: those commands operate on an entry
but don't want to leave a focus highlight behind on it.  See
`--call-in-drawer-focused' for the navigation-only variant that does
select the window precisely so that highlight DOES stick.

Instead we run FN with the drawer current, then mirror the new
buffer-point onto the displayed window via `set-window-point' and
manually invoke `--post-command' so the current-entry arrow tracks
immediately (the buffer-local post-command-hook would not fire because
the actual command is running in the caller's buffer, not the drawer).

Errors if no drawer buffer exists — caller can recover by toggling
the drawer open with `SPC o d' first."
  (let ((buf (or (get-buffer agent-repl-drawer-buffer-name)
                 (user-error "Drawer not open — `SPC o d' first"))))
    (with-current-buffer buf
      (let ((pre-entry (and preserve-cursor
                            (agent-repl-drawer--entry-at-point))))
        (funcall fn)
        (when pre-entry
          (agent-repl-drawer--goto-entry pre-entry)))
      (agent-repl-drawer--post-command)
      ;; Re-query the drawer window after FN: persp's window-config
      ;; restoration during a workspace switch can replace it with a
      ;; fresh window, leaving any pre-fn handle dead.
      (when-let ((win (get-buffer-window buf t)))
        (set-window-point win (point))))))

(defun agent-repl-drawer--call-in-drawer-focused (fn)
  "Like `--call-in-drawer', but actually selects the drawer window for
the duration of FN so visual features keyed off window-selection
(`hl-line', cursor visibility, mode-line styling) engage as if the
user had focused the drawer directly.  After FN returns, focus reverts
to the originally selected window — but state that those features
install during the call (e.g. an `hl-line' overlay with default sticky
flag) persists, which is the whole point: the user wants the moved-to
entry to remain visibly highlighted while they stay in their original
window.

Reserved for navigation dispatchers (`global-next' / `global-prev')
where the persistent highlight is the desired side effect.  Other
dispatchers use the unfocused `--call-in-drawer' so rapid taps don't
pay window-selection overhead.

Errors with `user-error' if no drawer buffer exists.  Falls back to
unfocused dispatch when the drawer buffer exists but no live window
displays it — we can't select what isn't shown."
  (let ((buf (or (get-buffer agent-repl-drawer-buffer-name)
                 (user-error "Drawer not open — `SPC o d' first"))))
    (let ((win (get-buffer-window buf t)))
      (if (window-live-p win)
          (with-selected-window win
            (funcall fn)
            (agent-repl-drawer--post-command))
        (agent-repl-drawer--call-in-drawer fn)))))

(defun agent-repl-drawer--follow-cursor-workspace ()
  "Switch the active workspace to the one under the drawer cursor.

The realtime half of global drawer navigation: `C-S-n' / `C-S-p' move
the cursor AND the frame, so walking the list walks the workspaces and
the user sees each agent's panels as they pass over its entry, instead
of having to confirm with a separate `C-S-<return>' visit.

Three cursor positions name no switch to make, and each is a normal
resting place for the cursor rather than a violated precondition:

  - a repo group header, which names a repo and not a workspace;
  - the already-active workspace, whose switch would be a redundant
    persp round-trip (the first `C-S-n' out of a fresh drawer lands
    here, since the cursor auto-syncs to the active workspace);
  - a workspace in the MERGED section, whose persp has been torn down.
    Reviving one costs a fresh persp plus a fresh agent session
    \(`--reactivate-merged'), and that is `agent-repl-drawer-visit''s
    deliberate act — never a side effect of scrolling past the entry.

Errors when no drawer buffer exists, matching `--call-in-drawer'.

Reuses `--leave-side-window-before-switch' for the same reason
`agent-repl-drawer-visit' does: a persp switch anchored on a side
window can clobber the destination's panels."
  (let* ((buf (or (get-buffer agent-repl-drawer-buffer-name)
                  (user-error "Drawer not open — `SPC o d' first")))
         (ws (with-current-buffer buf
               (agent-repl-drawer--workspace-at-point)))
         (current (agent-repl--ws-current-name)))
    (cond
     ((null ws)
      (agent-repl--log current
                       "drawer-follow: no switch, cursor is on a repo group header"))
     ((equal ws current)
      (agent-repl--log ws "drawer-follow: no switch, ws=%s is already active" ws))
     ((eq (agent-repl-drawer--workspace-section ws) :merged)
      (agent-repl--log ws "drawer-follow: no switch, ws=%s is MERGED (visit it to reactivate)" ws))
     (t
      (agent-repl--log ws "drawer-follow: switching from=%s to=%s" current ws)
      (agent-repl-drawer--leave-side-window-before-switch)
      (agent-repl--ws-switch ws)))))

(defun agent-repl-drawer--navigate-and-follow (move-fn)
  "Move the drawer cursor with MOVE-FN, then switch to the workspace it lands on.

Ordering is load-bearing.  MOVE-FN runs inside `--call-in-drawer-focused'
so the drawer window is selected while the cursor moves (`hl-line' and
friends engage), and the follow-switch runs AFTER that dispatch returns
— once focus is back on the originating window.  Switching from inside
the focused dispatch would run the persp switch with the drawer's side
window selected, which is exactly the layout-clobbering anchor that
`--leave-side-window-before-switch' exists to avoid, and would leave
`with-selected-window' trying to restore a window the switch has already
replaced."
  (agent-repl-drawer--call-in-drawer-focused move-fn)
  (agent-repl-drawer--follow-cursor-workspace))

(defun agent-repl-drawer-global-next ()
  "Move drawer cursor to next entry from any window, switching to it.
Selects the drawer window for the duration of the cursor move so visual
selection features (`hl-line', etc.) engage and persist after focus
returns to the originating window — see `--call-in-drawer-focused'.
The workspace under the new cursor position becomes the active one — see
`--follow-cursor-workspace'."
  (interactive)
  (agent-repl-drawer--navigate-and-follow #'agent-repl-drawer-next))

(defun agent-repl-drawer-global-prev ()
  "Move drawer cursor to previous entry from any window, switching to it.
Selects the drawer window for the duration of the cursor move so visual
selection features (`hl-line', etc.) engage and persist after focus
returns to the originating window — see `--call-in-drawer-focused'.
The workspace under the new cursor position becomes the active one — see
`--follow-cursor-workspace'."
  (interactive)
  (agent-repl-drawer--navigate-and-follow #'agent-repl-drawer-prev))

(defun agent-repl-drawer-global-visit ()
  "Visit (switch to) the workspace at the drawer cursor."
  (interactive)
  (agent-repl-drawer--call-in-drawer #'agent-repl-drawer-visit t))

(defun agent-repl-drawer-global-nuke ()
  "Nuke the workspace at the drawer cursor."
  (interactive)
  (agent-repl-drawer--call-in-drawer #'agent-repl-drawer-nuke t))

(defun agent-repl-drawer-global-kill ()
  "Kill the workspace at the drawer cursor."
  (interactive)
  (agent-repl-drawer--call-in-drawer #'agent-repl-drawer-kill t))

(defun agent-repl-drawer-global-send-prompt ()
  "Read a prompt and send to the workspace at the drawer cursor."
  (interactive)
  (agent-repl-drawer--call-in-drawer #'agent-repl-drawer-send-prompt t))

(defun agent-repl-drawer-global-merge-into-master ()
  "Merge the workspace at the drawer cursor into its source/master."
  (interactive)
  (agent-repl-drawer--call-in-drawer #'agent-repl-drawer-merge-into-master t))

(defun agent-repl-drawer-global-new-fork ()
  "Fork the workspace at the drawer cursor into a new worktree."
  (interactive)
  (agent-repl-drawer--call-in-drawer #'agent-repl-drawer-new-fork t))

(defun agent-repl-drawer-global-toggle-hidden ()
  "Toggle hidden state of the workspace at the drawer cursor."
  (interactive)
  (agent-repl-drawer--call-in-drawer #'agent-repl-drawer-toggle-hidden t))

(defun agent-repl-drawer-global-toggle-mark ()
  "Toggle the mark on the workspace at the drawer cursor."
  (interactive)
  (agent-repl-drawer--call-in-drawer #'agent-repl-drawer-toggle-mark t))

(defun agent-repl-drawer-global-clear-marks ()
  "Clear all drawer marks."
  (interactive)
  (agent-repl-drawer--call-in-drawer #'agent-repl-drawer-clear-marks t))

(defun agent-repl-drawer-global-priority-up ()
  "Cycle priority of the workspace at the drawer cursor up (toward p05)."
  (interactive)
  (agent-repl-drawer--call-in-drawer #'agent-repl-drawer-priority-up t))

(defun agent-repl-drawer-global-priority-down ()
  "Cycle priority of the workspace at the drawer cursor down (toward nil)."
  (interactive)
  (agent-repl-drawer--call-in-drawer #'agent-repl-drawer-priority-down t))

;;;; Auto-revert: drawer cursor follows current workspace -------------------

(defun agent-repl-drawer--sync-cursor-to-current-ws (&rest _)
  "Position drawer cursor on the currently active workspace's entry.
No-op when the drawer buffer doesn't exist or no current workspace
can be resolved.  Used by `persp-activated-functions' to keep the
drawer cursor sync'd with the active workspace on workspace switches.

Expands the current workspace to show its detail lines ONLY when it
sits in the MERGING section (`--auto-expand-p'); every other entry
stays folded on switch.  Refreshes the detail cache when the
workspace was not already expanded.  Only fires on workspace-switch
(persp-activated) and explicit drawer-open paths — never from the
1Hz poll or focus-change hooks.

Also repositions the current-entry arrow overlay synchronously so the
arrow snaps to the active workspace immediately, not after the next
1Hz status-poll re-render."
  (when-let* ((buf (get-buffer agent-repl-drawer-buffer-name))
              (current-ws (agent-repl--ws-current-name)))
    (let ((win (get-buffer-window buf t)))
      (with-current-buffer buf
        ;; Auto-expand only MERGING-section entries; every other
        ;; workspace stays folded on switch (`--auto-expand-p').
        (when (agent-repl-drawer--auto-expand-p current-ws)
          (agent-repl-drawer--ensure-expanded-set)
          (unless (gethash current-ws agent-repl-drawer--expanded-set)
            (agent-repl-drawer--refresh-detail-cache current-ws)
            (puthash current-ws t agent-repl-drawer--expanded-set)))
        (agent-repl-drawer--render)
        (when (agent-repl-drawer--goto-workspace-line current-ws)
          (when win (set-window-point win (point)))
          (agent-repl-drawer--update-current-entry-overlay)
          (agent-repl-drawer--center-selection buf))))))


(agent-repl--ws-add-activated-hook
 #'agent-repl-drawer--sync-cursor-to-current-ws)

(defun agent-repl-drawer--ensure-visible-on-persp-switch (&rest _)
  "Reconcile drawer visibility AND width with the global state on workspace switch.

When `--global-visible-p' is non-nil and the drawer is not currently
visible, re-display it.  When the flag is nil but the drawer *is*
visible (because persp-mode just restored a saved window
configuration that contained the drawer), delete the drawer window
so hiding the drawer in one workspace truly hides it across all.
When the drawer is already visible AND should remain visible, force
the configured width onto it — persp-mode's window-state-put restores
whatever width was saved in the destination workspace's config, which
is stale relative to a globally pinned drawer width.  Reapplying here
makes the drawer feel frame-level: resize once, every workspace shows
the same width.

Does NOT select the drawer window or reposition point — the drawer
behaves as a frame-level UI element, persistent across workspace
switches with no cursor disruption."
  (let* ((buf (get-buffer agent-repl-drawer-buffer-name))
         (win (and buf (get-buffer-window buf))))
    (cond
     ;; Flag says show, drawer missing → display it.
     ((and agent-repl-drawer--global-visible-p (not win))
      (let* ((buf (agent-repl-drawer--get-or-create-buffer))
             (win (display-buffer buf agent-repl-drawer--display-action)))
        (with-current-buffer buf
          (add-hook 'post-command-hook
                    #'agent-repl-drawer--post-command nil t)
          (setq-local cursor-type nil
                      truncate-lines nil
                      word-wrap t)
          (agent-repl-drawer--apply-background))
        (when win
          ;; Same drawer recipe as `agent-repl-drawer-show' — dedicate +
          ;; fringes 0/0.  `no-delete-other-windows' comes from the
          ;; display-action's `window-parameters'.
          (agent-repl-window--harden win :dedicate t :fringes 0)
          (agent-repl-drawer--apply-width win)
          (set-window-point win (with-current-buffer buf (point))))))
     ;; Flag says show, drawer already visible → override persp's stale
     ;; restored width with the global pinned/configured one.
     ((and agent-repl-drawer--global-visible-p win)
      (agent-repl-drawer--apply-width win))
     ;; Flag says hide, persp restored a stale drawer window → delete it.
     ((and (not agent-repl-drawer--global-visible-p) win)
      (dolist (w (get-buffer-window-list buf nil t))
        (when (window-live-p w)
          (set-window-dedicated-p w nil)))
      (agent-repl-window--delete-buffer-windows buf)))))

(agent-repl--ws-add-activated-hook
 #'agent-repl-drawer--ensure-visible-on-persp-switch)

;;;; Keyboard-inaccessibility bounce ----------------------------------------
;;
;; The drawer is a frame-level listing, not a per-workspace panel, so
;; it has no single well-known redirect target the way a workspace's
;; own input window serves its panels.  Keyboard-driven selection
;; landing here (`windmove', `other-window', the `select-window'
;; previously inside `agent-repl-drawer-show', ...) is instead
;; redirected to the most-recently-used non-drawer window — i.e.
;; wherever the user came from before keyboard nav landed in the
;; drawer.
;;
;; Mouse clicks are exempt (checked via `last-input-event') so the user
;; can still click into the drawer to operate entries via RET/j/k/etc.;
;; only keyboard-driven selection is redirected away.

(defun agent-repl-drawer--buffer-p (&optional buf)
  "Return non-nil if BUF (default: current buffer) is the drawer buffer.
Buffer-identity match against `agent-repl-drawer-buffer-name'."
  (let ((name (buffer-name (or buf (current-buffer)))))
    (and name (string= name agent-repl-drawer-buffer-name))))

(defun agent-repl-drawer--bounce-from-drawer (_frame)
  "If the selected window shows the drawer, redirect to the previous window.
Allows mouse-initiated selection through (checked via `mouse-event-p'
on `last-input-event') so clicking entries to visit a workspace works.
When no other live window exists on the frame, emits a warning via
`message' rather than leaving point stranded silently.

Predicate is buffer-identity (`agent-repl-drawer--buffer-p') —
buffer-name match is cheap and unambiguous since the drawer buffer
name is a defcustom-controlled singleton.

Hook target is `window-selection-change-functions', so this fires
during redisplay after a selection change rather than synchronously
on every `select-window' — body wrappers like `with-selected-window'
that select-and-restore the drawer for size adjustments don't
trigger spurious bounces because the net selected window after the
body is the original caller's window, not the drawer."
  (let ((win (selected-window)))
    (if (and (agent-repl-drawer--buffer-p (window-buffer win))
             (not (mouse-event-p last-input-event)))
        (let ((target (get-mru-window nil nil t)))
          (if (and target (window-live-p target))
              (select-window target)
            (message "[agent-repl] keyboard navigation landed in the drawer but no other window is available — click out or open another window")))
      nil)))

(add-hook 'window-selection-change-functions #'agent-repl-drawer--bounce-from-drawer)

(defun agent-repl-drawer-toggle ()
  "Toggle visibility of the workspace drawer."
  (interactive)
  (if-let* ((buf (get-buffer agent-repl-drawer-buffer-name))
            (win (get-buffer-window buf t)))
      (agent-repl-drawer-hide)
    (agent-repl-drawer-show)))

(provide 'agent-repl-drawer)
;;; drawer.el ends here
