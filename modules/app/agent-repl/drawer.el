;;; drawer.el --- Workspace drawer side-window for agent-repl -*- lexical-binding: t; -*-

;;; Commentary:

;; Read-only side-window listing every agent-repl workspace.  Lives "above"
;; the workspaces themselves: it is rendered into a left-side slot and is
;; not tied to any single workspace's window configuration.
;;
;; Each line shows: priority, claude-state icon, workspace name, and the
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

(defcustom agent-repl-drawer-width-fraction 0.20
  "Fraction of the frame width the drawer should occupy.
Computed against `frame-width' so the drawer scales with the frame.
Capped at 20% by default — the drawer is meant to stay open during
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
  "Alist mapping claude-state keyword to an indicator glyph.
The :dead entry is used when `:repl-state' is `:dead' (overrides
:claude-state).  The :merged entry is used when `:repl-state' is
`:merged' and takes precedence over `:dead' (so a merged workspace
whose vterm has since died still reads as merged).  The :merge-failed
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
  "Glyph shown when a workspace has no recognized claude-state.
Used for registered-but-not-yet-started workspaces (claude-state nil)."
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

(defcustom agent-repl-drawer-group-label-format " ▸ %s\n"
  "Format string for repo group labels (separators) in drawer sections.
Within a section, entries are partitioned by their workspace's git
common-dir (each top-level repo / its worktree set is one group).
Groups are separated by a blank line and labeled with this format."
  :type 'string
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

(defface agent-repl-drawer-events-header
  '((t :inherit font-lock-comment-face :weight bold))
  "Face for the events summary header at the top of the drawer."
  :group 'agent-repl)

(defface agent-repl-drawer-events-line
  '((t :inherit shadow))
  "Face for the events summary body line(s) at the top of the drawer."
  :group 'agent-repl)

(defface agent-repl-drawer-group-label
  '((t :inherit font-lock-comment-face :weight bold))
  "Face for repo group labels in drawer sections."
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

(defvar-local agent-repl-drawer--last-post-command-ws 'unset
  "Workspace at point on the previous `--post-command' tick.
Used to short-circuit the per-keystroke overlay refresh + recenter
when navigation did not cross an entry boundary.  `recenter' forces a
window redisplay, so gating it on entry change saves noticeable cost
on no-op commands and intra-entry motion.  Sentinel value `unset' (not
nil) so the first tick always runs even when point starts on a non-
workspace line.")

(defun agent-repl-drawer--entry-bounds-at-point ()
  "Return (START . END) of the workspace block at point, or nil."
  (let ((ws (agent-repl-drawer--workspace-at-point)))
    (when ws
      (save-excursion
        (let (start end)
          (while (and (not (bobp))
                      (equal (get-text-property (1- (point))
                                                'agent-repl-drawer-workspace)
                             ws))
            (forward-char -1))
          (setq start (point))
          (while (and (not (eobp))
                      (equal (get-text-property (point)
                                                'agent-repl-drawer-workspace)
                             ws))
            (forward-char 1))
          (setq end (point))
          (cons start end))))))

(defun agent-repl-drawer--update-current-entry-overlay ()
  "Move the current-entry arrow overlay onto the entry containing point.
Covers the static gutter region (chars [START, START+gutter-width)) of
the entry's first line with a `display' override that renders the
arrow.  Removes the overlay when point is not on a workspace entry,
or when the entry is marked (the red `●' takes precedence so the
cursor's identity is folded into the marked set)."
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
overlay rebuild and the `recenter' call when the workspace at point
has not changed since the previous tick — the overlay and scroll
position are entry-granularity artifacts, and `--render' already
re-establishes overlay state after mark/expand mutations.  The cursor
visibility flip is always-on because column position can shift
within the same entry (e.g. via in-line search)."
  (agent-repl-drawer--update-cursor)
  (let ((ws (agent-repl-drawer--workspace-at-point)))
    (unless (equal ws agent-repl-drawer--last-post-command-ws)
      (agent-repl-drawer--update-current-entry-overlay)
      (agent-repl-drawer--center-selection)
      (setq agent-repl-drawer--last-post-command-ws ws))))

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

(define-derived-mode agent-repl-drawer-mode special-mode "ClaudeDrawer"
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
  "Return live workspace keys from `agent-repl--workspaces' minus the nil persp.
Filters any key whose name (or bare name) equals `persp-nil-name'
\(default \"none\") so the persp-mode sentinel never surfaces in the
drawer.  Workspace creation already refuses bare names that collide
with `persp-nil-name', but the sentinel can still leak in via stray
status syncs — this is the drawer-side safety net so it never renders.

Also filters out tombstoned entries (`:nuked-at' set) so nuked
workspaces don't linger as drawer ghosts despite their identity
records surviving in the hash.

hide-project-dirs needs no special handling here: that mode kills
matching workspaces (`agent-repl-toggle-hide-project-dirs'), so they
become tombstones and the `:nuked-at' filter above already drops them
from the drawer."
  (let ((nil-name (agent-repl--ws-nil-name)))
    (cl-remove-if
     (lambda (ws)
       (and nil-name
            (or (equal ws nil-name)
                (equal (agent-repl--bare-workspace-name ws) nil-name))))
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
keyword, then maps claude-activity states to colored bold-foreground
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
Cached on the workspace plist as `:group-key' so each workspace runs
git at most once.  Returns nil when WS has no project-dir or git
fails — such workspaces fall into the unlabeled `(no repo)' bucket."
  (or (agent-repl--ws-get ws :group-key)
      (when-let* ((dir (ignore-errors (agent-repl--ws-dir ws)))
                  (raw (agent-repl--git-string-quiet
                        "-C" dir "rev-parse" "--git-common-dir")))
        (when (and raw (not (string-empty-p raw))
                   (not (string-prefix-p "fatal" raw)))
          (let* ((abs (if (file-name-absolute-p raw) raw
                        (expand-file-name raw dir)))
                 (key (agent-repl--path-canonical abs)))
            (agent-repl--ws-put ws :group-key key)
            key)))))

(defun agent-repl-drawer--group-label (key)
  "Derive a human-readable group label from KEY (a canonical .git path).
Returns the basename of KEY's parent directory — i.e. the project
name, since git's common-dir is conventionally `<project>/.git'."
  (when key
    (let ((parent (file-name-directory key)))
      (when parent
        (file-name-nondirectory (directory-file-name parent))))))

(defun agent-repl-drawer--group-trees-by-repo (trees)
  "Partition TREES into (LABEL . TREES-IN-GROUP) buckets by repo.
Bucket keys are derived from each tree root's `:group-key' via
`--workspace-group-key' and `--group-label'.  Insertion order of the
returned alist matches the first-encounter order in TREES, so groups
appear in the order their first root appeared after sorting."
  (let ((order nil)
        (buckets (make-hash-table :test 'equal)))
    (dolist (tree trees)
      (let* ((root  (car tree))
             (key   (agent-repl-drawer--workspace-group-key root))
             (label (or (agent-repl-drawer--group-label key) "(no repo)")))
        (unless (gethash label buckets)
          (push label order))
        (puthash label
                 (append (gethash label buckets) (list tree))
                 buckets)))
    (mapcar (lambda (label) (cons label (gethash label buckets)))
            (nreverse order))))

(defun agent-repl-drawer--render-trees (trees current section)
  "Render TREES (forest) grouped by top-level repo.
Within a group, root subtrees render contiguously with a blank line
between siblings.  Between groups, a labeled separator (plus blank
line) marks the boundary so multi-repo drawers stay scannable."
  (let ((groups (agent-repl-drawer--group-trees-by-repo trees))
        (first-group t))
    (dolist (group groups)
      (let ((label       (car group))
            (group-trees (cdr group)))
        (unless first-group
          (insert "\n"))
        (insert (propertize (format agent-repl-drawer-group-label-format label)
                            'face 'agent-repl-drawer-group-label))
        (let ((tree-rest group-trees))
          (while tree-rest
            (agent-repl-drawer--render-subtree (car tree-rest) 0
                                                current section)
            (when (cdr tree-rest) (insert "\n"))
            (setq tree-rest (cdr tree-rest))))
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

(defun agent-repl-drawer--format-event-age (seconds)
  "Format SECONDS as a short relative age (e.g. \"3m\", \"2h\")."
  (cond
   ((< seconds 60)   (format "%ds" (max 0 (truncate seconds))))
   ((< seconds 3600) (format "%dm" (truncate (/ seconds 60))))
   (t                (format "%dh" (truncate (/ seconds 3600))))))

(defun agent-repl-drawer--insert-events-header ()
  "Insert a one-block summary of workspace create/merge events from the last 24h.
No-op when no events are recorded in that window."
  (when (fboundp 'agent-repl--events-recent)
    (let* ((now (float-time))
           (events (agent-repl--events-recent now))
           (creates (agent-repl--events-count-by-kind events :create))
           (merges  (agent-repl--events-count-by-kind events :merge)))
      (when events
        (insert (propertize " Last 24h\n"
                            'face 'agent-repl-drawer-events-header))
        (insert (propertize
                 (format "   ✨ %d created   🔀 %d merged\n" creates merges)
                 'face 'agent-repl-drawer-events-line))
        (dolist (ev (seq-take events 5))
          (let* ((kind (plist-get ev :kind))
                 (ws   (plist-get ev :ws))
                 (ts   (plist-get ev :time))
                 (glyph (cond ((eq kind :create) "✨")
                              ((eq kind :merge)  "🔀")
                              (t "·")))
                 (age  (agent-repl-drawer--format-event-age (- now (or ts now)))))
            (insert (propertize
                     (format "   %s %s  %s\n" glyph age (or ws "?"))
                     'face 'agent-repl-drawer-events-line))))))))

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
                    (agent-repl-drawer--visible-workspace-keys))))
    (agent-repl-drawer--insert-events-header)
    (insert "\n")
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
marked/expanded sets per ws, current workspace, and the events
header's formatted age buckets — the latter ensures the header
updates as ages cross the s/m/h boundaries (`--format-event-age')."
  (let (ws-sig)
    (dolist (ws (sort (agent-repl-drawer--visible-workspace-keys) #'string<))
      (push (list ws
                  (agent-repl--ws-get ws :repl-state)
                  (agent-repl--ws-get ws :claude-state)
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
          ws-sig
          (when (fboundp 'agent-repl--events-recent)
            (let ((now (float-time)))
              (mapcar (lambda (ev)
                        (cons (plist-get ev :kind)
                              (agent-repl-drawer--format-event-age
                               (- now (or (plist-get ev :time) now)))))
                      (seq-take (agent-repl--events-recent now) 5)))))))

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

Anchors cursor restoration by workspace identity, not just line
number: line numbers shift when an entry above the cursor
expands/collapses or appears/disappears between polls, and
`forward-line saved-line' can then land on a non-workspace line
(detail line, blank, section header), which causes
`--update-current-entry-overlay' to delete the arrow.  Nested
children sit deeper in the buffer and so are most affected.

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
             (saved-ws     (agent-repl-drawer--workspace-at-point))
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
          (unless (and saved-ws
                       (agent-repl-drawer--goto-workspace-line saved-ws))
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

(defun agent-repl-drawer--goto-workspace-line (ws)
  "Move point to the start of the line for workspace WS, if present.
Returns non-nil on success."
  (let ((target nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not target) (not (eobp)))
        (when (equal (get-text-property (point) 'agent-repl-drawer-workspace) ws)
          (setq target (point)))
        (forward-line 1)))
    (when target
      (goto-char target)
      t)))

(defun agent-repl-drawer-next ()
  "Move point to the next workspace entry."
  (interactive)
  (let ((current (agent-repl-drawer--workspace-at-point))
        (start   (point))
        (found   nil))
    (forward-line 1)
    (while (and (not found) (not (eobp)))
      (let ((ws (agent-repl-drawer--workspace-at-point)))
        (if (and ws (not (equal ws current)))
            (setq found t)
          (forward-line 1))))
    (unless found
      (goto-char start))))

(defun agent-repl-drawer-prev ()
  "Move point to the previous workspace entry."
  (interactive)
  (let ((current (agent-repl-drawer--workspace-at-point))
        (start   (point))
        (found   nil))
    (forward-line -1)
    (while (and (not found) (not (bobp)))
      (let ((ws (agent-repl-drawer--workspace-at-point)))
        (if (and ws (not (equal ws current)))
            (setq found t)
          (forward-line -1))))
    (when found
      ;; Snap to the start of the workspace block (handles the summary
      ;; subtitle line being the first one we land on when moving up).
      (let ((ws (agent-repl-drawer--workspace-at-point)))
        (while (and (not (bobp))
                    (equal (get-text-property (1- (point))
                                              'agent-repl-drawer-workspace)
                           ws))
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

(defun agent-repl-drawer--refresh-detail-cache (ws)
  "Populate WS's `:detail-*' plist fields with synchronous git calls.
Called from TAB-toggle (when expanding) and `g'-refresh.  Avoids
running git every poll cycle.  All values are best-effort: nil left
in place when the underlying command errors or returns empty."
  (when-let ((dir (ignore-errors (agent-repl--ws-dir ws))))
    (let* ((branch (agent-repl--git-string-quiet
                    "-C" dir "rev-parse" "--abbrev-ref" "HEAD")))
      (agent-repl--ws-put ws :detail-branch
                           (and branch (not (string-empty-p branch)) branch)))
    (let ((ahead (agent-repl--git-string-quiet
                  "-C" dir "rev-list" "--count"
                  (concat agent-repl-master-branch-name "..HEAD"))))
      (agent-repl--ws-put ws :detail-master-ahead
                           (and ahead (not (string-empty-p ahead))
                                (string-to-number ahead))))
    (when-let* ((src-dir (agent-repl--ws-get ws :source-ws-dir))
                ((file-directory-p src-dir))
                (src-branch (agent-repl--git-string-quiet
                             "-C" src-dir "rev-parse" "--abbrev-ref" "HEAD")))
      (when (and src-branch (not (string-empty-p src-branch))
                 (not (string-prefix-p "fatal" src-branch)))
        (let ((ahead (agent-repl--git-string-quiet
                      "-C" dir "rev-list" "--count"
                      (concat src-branch "..HEAD"))))
          (agent-repl--ws-put ws :detail-source-ahead
                               (and ahead (not (string-empty-p ahead))
                                    (string-to-number ahead))))))
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

(defun agent-repl-drawer-toggle-expand ()
  "Toggle the expanded detail view for the entry at point.
On expand, refreshes the detail cache (synchronous git calls); on
collapse, removes from the expanded set.  Re-renders the drawer."
  (interactive)
  (let ((ws (agent-repl-drawer--require-ws-at-point)))
    (agent-repl-drawer--ensure-expanded-set)
    (if (gethash ws agent-repl-drawer--expanded-set)
        (remhash ws agent-repl-drawer--expanded-set)
      (agent-repl-drawer--refresh-detail-cache ws)
      (puthash ws t agent-repl-drawer--expanded-set))
    (agent-repl-drawer--render)))

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
         (source-ahead (agent-repl--ws-get ws :detail-source-ahead))
         (last-commit  (agent-repl--ws-get ws :detail-last-commit))
         (last-commit-time (agent-repl--ws-get ws :detail-last-commit-time))
         (dirty-count  (agent-repl--ws-get ws :detail-dirty-count))
         (last-prompt-time (agent-repl--ws-get ws :last-prompt-time))
         (pending-count (length (agent-repl--ws-get ws :pending-prompts)))
         (merged-in    (agent-repl--ws-get ws :merged-in-workspaces)))
    (cl-flet ((line (label value face)
                (insert detail-prefix
                        (propertize (concat label " ") 'face 'shadow)
                        (propertize (format "%s" value)
                                    'face face
                                    'wrap-prefix detail-prefix)
                        "\n")))
      (when branch
        (line "branch:" branch 'agent-repl-drawer-detail-branch))
      (when merge-target
        (line "merged into:" merge-target
              'agent-repl-drawer-detail-merge-target))
      (when master-ahead
        (line "ahead master:" (format "%d" master-ahead)
              'agent-repl-drawer-detail-ahead-master))
      (when source-ahead
        (line "ahead source:" (format "%d" source-ahead)
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
window — the drawer mirrors the claude-output (vterm) panel's
keyboard-inaccessible policy, so callers stay where they are and
must mouse-click into the drawer to operate it directly.  Sets the
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

When PRESERVE-CURSOR is non-nil, snapshots the workspace at point
before FN runs and re-positions the cursor onto that same workspace
afterward.  Without this, side effects of FN can move the drawer
cursor off the user's navigated entry: `+workspace-switch' triggers
`--sync-cursor-to-current-ws' which snaps to the newly-active ws;
persp's window-config restoration can install a fresh drawer window
with a stale `window-point' from the saved config; a re-render whose
`saved-ws' anchor is no longer present in the buffer falls back to a
line-number heuristic that may land outside any entry.  Preservation
makes all non-navigational C-S-<key> dispatchers (visit, nuke, kill,
toggle-hidden, …) keep the drawer cursor where the user pointed it.
When the snapshot workspace no longer exists in the buffer after FN
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
      (let ((pre-ws (and preserve-cursor
                         (agent-repl-drawer--workspace-at-point))))
        (funcall fn)
        (when pre-ws
          (agent-repl-drawer--goto-workspace-line pre-ws)))
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

(defun agent-repl-drawer-global-next ()
  "Move drawer cursor to next entry from any window.
Selects the drawer window for the duration of the command so visual
selection features (`hl-line', etc.) engage and persist after focus
returns to the originating window — see `--call-in-drawer-focused'."
  (interactive)
  (agent-repl-drawer--call-in-drawer-focused #'agent-repl-drawer-next))

(defun agent-repl-drawer-global-prev ()
  "Move drawer cursor to previous entry from any window.
Selects the drawer window for the duration of the command so visual
selection features (`hl-line', etc.) engage and persist after focus
returns to the originating window — see `--call-in-drawer-focused'."
  (interactive)
  (agent-repl-drawer--call-in-drawer-focused #'agent-repl-drawer-prev))

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
;; Mirrors `agent-repl--bounce-from-vterm' (panels.el).  Same mechanism,
;; different target buffer and bounce destination: the vterm bounce
;; redirects to the workspace's input window (a well-known per-workspace
;; target); the drawer bounce has no comparable single target, so it
;; redirects to the most-recently-used non-drawer window — i.e. wherever
;; the user came from before keyboard nav landed in the drawer.
;;
;; Mouse clicks are exempt (checked via `last-input-event') so the user
;; can still click into the drawer to operate entries via RET/j/k/etc.;
;; only keyboard-driven selection (`windmove', `other-window', the
;; `select-window' previously inside `agent-repl-drawer-show', ...) is
;; redirected away.

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

Predicate is buffer-identity (`agent-repl-drawer--buffer-p'), mirroring
the vterm bounce — buffer-name match is cheap and unambiguous since the
drawer buffer name is a defcustom-controlled singleton.

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
