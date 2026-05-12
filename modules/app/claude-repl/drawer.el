;;; drawer.el --- Workspace drawer side-window for claude-repl -*- lexical-binding: t; -*-

;;; Commentary:

;; Read-only side-window listing every claude-repl workspace.  Lives "above"
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

(defcustom claude-repl-drawer-buffer-name "*claude-repl-drawer*"
  "Buffer name for the claude-repl workspace drawer."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-drawer-width-fraction 0.20
  "Fraction of the frame width the drawer should occupy.
Computed against `frame-width' at display time so the drawer scales
with the frame.  Capped at 20% by default — the drawer is meant to
stay open during work, not dominate the layout.

Acts as the seed/default; once the user manually resizes the drawer
the override in `claude-repl-drawer--persisted-cols' takes precedence
across every workspace/persp until cleared via
`claude-repl-drawer-reset-width'."
  :type 'float
  :group 'claude-repl)

(defvar claude-repl-drawer--persisted-cols nil
  "User-pinned drawer width in columns, or nil to fall back to the fraction.
Captured from manual resize gestures by
`claude-repl-drawer--capture-resize' and reapplied across every
workspace by `claude-repl-drawer--apply-width' so the drawer behaves
as a frame-level UI element rather than a per-persp artifact.")

(defvar claude-repl-drawer--expected-cols nil
  "Width applied by the last `claude-repl-drawer--apply-width' call.
Read by `claude-repl-drawer--capture-resize' to tell apart programmatic
size changes (matching) from manual user resizes (diverging) without
needing a dynamic flag — the size-change hook fires after redisplay,
long after any `let'-bound suppression flag has unwound.")

(defcustom claude-repl-drawer-indent-per-level 2
  "Columns to indent each nesting level in the drawer.
Read by both the render (per-depth indent string) and the window
width calculation (depth bonus added to fraction-derived base width),
so changing this single value resizes the drawer in proportion to
the indent change — no other knobs needed."
  :type 'integer
  :group 'claude-repl)

;; Force-apply on reload — defcustom only initializes for unbound
;; symbols, so source tweaks otherwise need a full Emacs restart.
(setq claude-repl-drawer-width-fraction
      (eval (car (get 'claude-repl-drawer-width-fraction 'standard-value))))

(defcustom claude-repl-drawer-state-icons
  '((:init        . "⏳")
    (:thinking    . "⌛")
    (:done        . "✅")
    (:idle        . "💤")
    (:permission  . "❓")
    (:stop-failed . "❗")
    (:dead        . "❌"))
  "Alist mapping claude-state keyword to an indicator glyph.
The :dead entry is used when `:repl-state' is `:dead' (overrides
:claude-state).  Unrecognized values fall through to a single middot
placeholder, used for workspaces registered but with no live session."
  :type '(alist :key-type symbol :value-type string)
  :group 'claude-repl)

;; Force-apply the latest palette on every (re)load.  `defcustom' only
;; initializes the value when the symbol is unbound, so palette tweaks
;; otherwise require an Emacs restart to take effect.  Source is the
;; canonical palette in this personal config; `M-x customize' values
;; for this variable will be overwritten on reload.
(setq claude-repl-drawer-state-icons
      (eval (car (get 'claude-repl-drawer-state-icons 'standard-value))))

(defcustom claude-repl-drawer-state-icon-default "·"
  "Glyph shown when a workspace has no recognized claude-state.
Used for registered-but-not-yet-started workspaces (claude-state nil)."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-drawer-section-rule-width 12
  "Number of `─' characters in section header rule lines."
  :type 'integer
  :group 'claude-repl)

(defcustom claude-repl-drawer-empty-section-label "(none)"
  "Placeholder shown under a section header when the section has no entries."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-drawer-group-label-format " ▸ %s\n"
  "Format string for repo group labels (separators) in drawer sections.
Within a section, entries are partitioned by their workspace's git
common-dir (each top-level repo / its worktree set is one group).
Groups are separated by a blank line and labeled with this format."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-drawer-marked-glyph "● "
  "Gutter glyph for entries the user has marked for bulk operations.
Width must match `claude-repl-drawer-gutter' for column alignment."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-drawer-current-arrow "▶ "
  "Gutter glyph displayed on the entry the cursor is currently on.
Width must match the static `claude-repl-drawer-gutter' so the arrow
overlay can replace the gutter region without shifting alignment."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-drawer-gutter "  "
  "Static gutter prefix rendered at the start of every workspace block.
The current-entry overlay covers this region with
`claude-repl-drawer-current-arrow'; widths must match."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-drawer-background "#0a0a0a"
  "Background color for the drawer buffer.
Applied via `face-remap-add-relative' so the drawer reads as a
distinct UI region rather than blending with the workspace below.
Default is near-black."
  :type 'color
  :group 'claude-repl)

;;;; Faces ------------------------------------------------------------------

(defface claude-repl-drawer-workspace-name
  '((t :weight bold))
  "Face for the workspace name line in the drawer."
  :group 'claude-repl)

(defface claude-repl-drawer-marked
  '((t :foreground "red" :weight bold))
  "Face for the marked-entry gutter glyph in the drawer."
  :group 'claude-repl)

(defface claude-repl-drawer-current-arrow
  '((t :weight bold :inherit font-lock-keyword-face))
  "Face for the gutter arrow that marks the currently selected workspace."
  :group 'claude-repl)

(defface claude-repl-drawer-summary
  '((t :inherit shadow :slant italic))
  "Face for the aiTitle/prompt-summary subtitle line."
  :group 'claude-repl)

(defface claude-repl-drawer-hidden
  '((t :inherit shadow))
  "Face used to dim hidden workspaces."
  :group 'claude-repl)

(defface claude-repl-drawer-section-title
  '((t :weight bold :inherit font-lock-keyword-face))
  "Face for the MAIN/HIDDEN section title line."
  :group 'claude-repl)

(defface claude-repl-drawer-section-rule
  '((t :inherit shadow))
  "Face for the rule line beneath a section title."
  :group 'claude-repl)

(defface claude-repl-drawer-group-label
  '((t :inherit font-lock-comment-face :weight bold))
  "Face for repo group labels in drawer sections."
  :group 'claude-repl)

(defface claude-repl-drawer-empty
  '((t :inherit shadow :slant italic))
  "Face for the placeholder shown under an empty section."
  :group 'claude-repl)

(defface claude-repl-drawer-detail-branch
  '((t :foreground "deep sky blue"))
  "Face for the branch name in expanded detail view."
  :group 'claude-repl)

(defface claude-repl-drawer-detail-ahead-master
  '((t :foreground "spring green" :weight bold))
  "Face for the ahead-master commit count in expanded detail view."
  :group 'claude-repl)

(defface claude-repl-drawer-detail-ahead-source
  '((t :foreground "gold" :weight bold))
  "Face for the ahead-source commit count in expanded detail view."
  :group 'claude-repl)

(defface claude-repl-drawer-detail-last-commit
  '((t :foreground "medium orchid"))
  "Face for the last commit subject + relative time in expanded detail view."
  :group 'claude-repl)

(defface claude-repl-drawer-detail-dirty
  '((t :foreground "tomato" :weight bold))
  "Face for the dirty file count in expanded detail view."
  :group 'claude-repl)

(defface claude-repl-drawer-detail-last-prompt
  '((t :foreground "light steel blue" :slant italic))
  "Face for the duration-since-last-prompt value in expanded detail view."
  :group 'claude-repl)

(defface claude-repl-drawer-detail-pending
  '((t :foreground "orange" :weight bold))
  "Face for the pending-prompt count in expanded detail view."
  :group 'claude-repl)

;;;; Mode -------------------------------------------------------------------

(defvar claude-repl-drawer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j")       #'claude-repl-drawer-next)
    (define-key map (kbd "<down>")  #'claude-repl-drawer-next)
    (define-key map (kbd "k")       #'claude-repl-drawer-prev)
    (define-key map (kbd "<up>")    #'claude-repl-drawer-prev)
    (define-key map (kbd "RET")     #'claude-repl-drawer-visit)
    (define-key map (kbd "TAB")     #'claude-repl-drawer-toggle-expand)
    (define-key map (kbd "<tab>")   #'claude-repl-drawer-toggle-expand)
    (define-key map (kbd "g")       #'claude-repl-drawer-refresh)
    (define-key map (kbd "q")       #'claude-repl-drawer-hide)
    ;; Per-entry actions mirroring leader-key bindings:
    (define-key map (kbd "x")       #'claude-repl-drawer-nuke)
    (define-key map (kbd "d")       #'claude-repl-drawer-kill)
    (define-key map (kbd "i")       #'claude-repl-drawer-send-prompt)
    (define-key map (kbd "M")       #'claude-repl-drawer-merge-into-master)
    (define-key map (kbd "m")       #'claude-repl-drawer-merge-child)
    (define-key map (kbd "n")       #'claude-repl-drawer-new-child)
    (define-key map (kbd "f")       #'claude-repl-drawer-new-fork)
    (define-key map (kbd "H")       #'claude-repl-drawer-toggle-hidden)
    (define-key map (kbd "+")       #'claude-repl-drawer-priority-up)
    (define-key map (kbd "-")       #'claude-repl-drawer-priority-down)
    (define-key map (kbd "t")       #'claude-repl-drawer-toggle-mark)
    (define-key map (kbd "u")       #'claude-repl-drawer-clear-marks)
    (define-key map (kbd "C-c C-k") #'claude-repl-drawer-interrupt)
    ;; Block horizontal char navigation — the entry is the unit of
    ;; selection; in-line cursor placement is reserved for searches.
    (define-key map (kbd "<left>")  #'ignore)
    (define-key map (kbd "<right>") #'ignore)
    map)
  "Keymap for `claude-repl-drawer-mode'.")

(defvar-local claude-repl-drawer--current-entry-overlay nil
  "Overlay that draws the current-entry arrow over the static gutter.
Repositioned by `claude-repl-drawer--post-command' to follow point.")

(defun claude-repl-drawer--entry-bounds-at-point ()
  "Return (START . END) of the workspace block at point, or nil."
  (let ((ws (claude-repl-drawer--workspace-at-point)))
    (when ws
      (save-excursion
        (let (start end)
          (while (and (not (bobp))
                      (equal (get-text-property (1- (point))
                                                'claude-repl-drawer-workspace)
                             ws))
            (forward-char -1))
          (setq start (point))
          (while (and (not (eobp))
                      (equal (get-text-property (point)
                                                'claude-repl-drawer-workspace)
                             ws))
            (forward-char 1))
          (setq end (point))
          (cons start end))))))

(defun claude-repl-drawer--update-current-entry-overlay ()
  "Move the current-entry arrow overlay onto the entry containing point.
Covers the static gutter region (chars [START, START+gutter-width)) of
the entry's first line with a `display' override that renders the
arrow.  Removes the overlay when point is not on a workspace entry,
or when the entry is marked (the red `●' takes precedence so the
cursor's identity is folded into the marked set)."
  (let ((bounds (claude-repl-drawer--entry-bounds-at-point))
        (ws-at-point (claude-repl-drawer--workspace-at-point))
        (gutter-len (length claude-repl-drawer-gutter)))
    (cond
     ((or (null bounds)
          (claude-repl-drawer--marked-p ws-at-point))
      (when (overlayp claude-repl-drawer--current-entry-overlay)
        (delete-overlay claude-repl-drawer--current-entry-overlay)))
     (t
      (let* ((start (car bounds))
             (cover-end (min (cdr bounds) (+ start gutter-len))))
        (if (overlayp claude-repl-drawer--current-entry-overlay)
            (move-overlay claude-repl-drawer--current-entry-overlay
                          start cover-end)
          (setq claude-repl-drawer--current-entry-overlay
                (make-overlay start cover-end)))
        (overlay-put claude-repl-drawer--current-entry-overlay 'display
                     (propertize claude-repl-drawer-current-arrow
                                 'face 'claude-repl-drawer-current-arrow))
        (overlay-put claude-repl-drawer--current-entry-overlay 'priority 100))))))

(defun claude-repl-drawer--update-cursor ()
  "Hide the cursor when point is at column 0 (covered by the arrow), show otherwise.
Searches that pull point mid-line make the cursor reappear; j/k snap
back to col 0 and re-hide it."
  (setq-local cursor-type (if (zerop (current-column)) nil 'box)))

(defun claude-repl-drawer--post-command ()
  "Refresh the current-entry overlay and cursor visibility.
Runs after every command in the drawer buffer."
  (claude-repl-drawer--update-current-entry-overlay)
  (claude-repl-drawer--update-cursor))

(defun claude-repl-drawer--apply-background ()
  "Remap the buffer's `default' face to the drawer background color.
Idempotent and cheap; safe to call from mode init and from show
self-heal paths.  Uses `face-remap-add-relative' so the change is
buffer-local and doesn't leak into the workspace's other windows."
  (face-remap-add-relative
   'default :background claude-repl-drawer-background))

(define-derived-mode claude-repl-drawer-mode special-mode "ClaudeDrawer"
  "Major mode for the claude-repl workspace drawer."
  (setq truncate-lines nil
        buffer-read-only t
        mode-line-format nil
        word-wrap t)
  (setq-local cursor-type nil)
  (claude-repl-drawer--apply-background)
  (add-hook 'post-command-hook
            #'claude-repl-drawer--post-command nil t))

;; Evil intercepts j/k/n/p in motion/normal state and routes them to
;; line-wise commands.  Bind our drawer commands explicitly in those
;; states so j/k jump workspace-by-workspace, not line-by-line.
(when (fboundp 'evil-define-key)
  (evil-define-key '(normal motion) claude-repl-drawer-mode-map
    "j"           #'claude-repl-drawer-next
    "k"           #'claude-repl-drawer-prev
    (kbd "<down>") #'claude-repl-drawer-next
    (kbd "<up>")   #'claude-repl-drawer-prev
    (kbd "RET")    #'claude-repl-drawer-visit
    "g"           #'claude-repl-drawer-refresh
    "q"           #'claude-repl-drawer-hide)
  ;; Block every insert-state entry point we don't repurpose, so the
  ;; drawer never flips into evil insert state.  `i', `m', `d' are
  ;; rebound to drawer commands below.
  (dolist (key '("I" "a" "A" "o" "O" "s" "S" "c" "C" "R"))
    (evil-define-key '(normal motion) claude-repl-drawer-mode-map
      key #'ignore))
  ;; Per-entry action keys — bind in evil states too so they aren't
  ;; intercepted by evil-motion's defaults.
  (evil-define-key '(normal motion) claude-repl-drawer-mode-map
    "x"             #'claude-repl-drawer-nuke
    "d"             #'claude-repl-drawer-kill
    "i"             #'claude-repl-drawer-send-prompt
    "M"             #'claude-repl-drawer-merge-into-master
    "m"             #'claude-repl-drawer-merge-child
    "n"             #'claude-repl-drawer-new-child
    "f"             #'claude-repl-drawer-new-fork
    "H"             #'claude-repl-drawer-toggle-hidden
    "+"             #'claude-repl-drawer-priority-up
    "-"             #'claude-repl-drawer-priority-down
    "t"             #'claude-repl-drawer-toggle-mark
    "u"             #'claude-repl-drawer-clear-marks
    (kbd "TAB")     #'claude-repl-drawer-toggle-expand
    (kbd "<tab>")   #'claude-repl-drawer-toggle-expand
    (kbd "C-c C-k") #'claude-repl-drawer-interrupt)
  ;; Block horizontal char navigation — entry is the navigational unit.
  (evil-define-key '(normal motion) claude-repl-drawer-mode-map
    "h"             #'ignore
    "l"             #'ignore
    (kbd "<left>")  #'ignore
    (kbd "<right>") #'ignore))

;; Force motion-state on entry so the drawer never starts in normal
;; (where insert keys could trigger before our overrides apply).
(when (fboundp 'evil-set-initial-state)
  (evil-set-initial-state 'claude-repl-drawer-mode 'motion))

;;;; Sorting + selection helpers --------------------------------------------

(defun claude-repl-drawer--workspace-hidden-p (ws)
  "Return non-nil if workspace WS is in the `:hidden' repl-state."
  (eq (claude-repl--ws-get ws :repl-state) :hidden))

(defun claude-repl-drawer--sort-key (ws)
  "Return a sort key for workspace name WS.
Lower keys come first.  Sort by `:priority' rank, then name."
  (cons (claude-repl--priority-rank (claude-repl--ws-get ws :priority))
        ws))

(defun claude-repl-drawer--sort (names)
  "Return NAMES sorted by priority rank, then alphabetically."
  (sort (copy-sequence names)
        (lambda (a b)
          (let ((ka (claude-repl-drawer--sort-key a))
                (kb (claude-repl-drawer--sort-key b)))
            (or (< (car ka) (car kb))
                (and (= (car ka) (car kb))
                     (string< (cdr ka) (cdr kb))))))))

(defun claude-repl-drawer--partition (names)
  "Return (VISIBLE . HIDDEN) lists from NAMES, each sorted.
Legacy two-section partition; tree-aware sectioning lives in
`claude-repl-drawer--partition-by-section'."
  (let (visible hidden)
    (dolist (ws names)
      (if (claude-repl-drawer--workspace-hidden-p ws)
          (push ws hidden)
        (push ws visible)))
    (cons (claude-repl-drawer--sort visible)
          (claude-repl-drawer--sort hidden))))

;;;; Section + tree helpers -------------------------------------------------

(defun claude-repl-drawer--workspace-section (ws)
  "Return :main, :hidden, :merging, or :merged for WS based on its plist state.
Precedence (highest first): :merged → :merging → :hidden → :main.
- :merged is reserved for workspaces whose explicit merge command
  completed successfully (`:merge-completed' t).
- :merging is the in-flight workflow signal — set when
  `claude-repl--workspace-merge-do' begins a merge attempt and
  cleared on success (alongside `:merge-completed t') or failure
  (alongside `--mark-merge-failed').  Git ancestry no longer drives
  this bucket: an empty child whose parent advanced past it used to
  mis-bucket here, and ancestry is now reserved for tree flattening.
- :hidden / :main are unchanged.

A completed merge dominates an in-flight one (transition state
during the brief window between setting `:merge-completed t' and
clearing `:merging') and both dominate hidden."
  (cond
   ((claude-repl--ws-merge-completed-p ws)            :merged)
   ((claude-repl--ws-merge-in-progress-p ws)          :merging)
   ((eq (claude-repl--ws-get ws :repl-state) :hidden) :hidden)
   (t :main)))

(defun claude-repl-drawer--source-ws-name (ws)
  "Return the workspace name recorded as WS's source, or nil.
Reverse-lookups `:source-ws-dir' through `claude-repl--ws-name-for-dir'."
  (when-let ((dir (claude-repl--ws-get ws :source-ws-dir)))
    (claude-repl--ws-name-for-dir dir)))

(defcustom claude-repl-drawer-tree-max-depth 16
  "Cycle defense for drawer parent-chain walks."
  :type 'integer
  :group 'claude-repl)

(defun claude-repl-drawer--ws-flattenable-ancestor-p (ws)
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
  (claude-repl--ws-merged-p ws))

(defun claude-repl-drawer--effective-parent (ws section-set)
  "Return WS's effective parent in SECTION-SET (a list of workspace names).
Walks the source-ws chain skipping ancestors flagged as flattenable
by `claude-repl-drawer--ws-flattenable-ancestor-p' (git-ancestry-detected
merged only).  Returns the first non-flattenable ancestor that lives in
SECTION-SET, or nil when no ancestor qualifies (WS is a root in this
section).  Cycle-capped via `claude-repl-drawer-tree-max-depth'."
  (let ((candidate (claude-repl-drawer--source-ws-name ws))
        (depth 0)
        (result nil)
        (done nil))
    (while (and (not done) candidate
                (< depth claude-repl-drawer-tree-max-depth))
      (setq depth (1+ depth))
      (cond
       ((claude-repl-drawer--ws-flattenable-ancestor-p candidate)
        (setq candidate (claude-repl-drawer--source-ws-name candidate)))
       ((member candidate section-set)
        (setq result candidate done t))
       (t (setq done t))))
    result))

(defun claude-repl-drawer--effective-parent-in-section (ws section-set)
  "Return WS's parent in a same-section tree: source-ws if also in SECTION-SET, else nil.
Used by MERGING and MERGED, which preserve original topology rather
than flattening through merged ancestors — both sections want to show
the parent/child structure between their own members."
  (when-let ((src (claude-repl-drawer--source-ws-name ws)))
    (when (member src section-set) src)))

(defalias 'claude-repl-drawer--effective-parent-in-merged
  #'claude-repl-drawer--effective-parent-in-section
  "Back-compat alias.  Old name retained so external callers / tests
don't break; the implementation is now shared with MERGING.")

(defun claude-repl-drawer--partition-by-section (workspaces)
  "Partition WORKSPACES into (:main :hidden :merging :merged) buckets."
  (let (main hidden merging merged)
    (dolist (ws workspaces)
      (pcase (claude-repl-drawer--workspace-section ws)
        (:main    (push ws main))
        (:hidden  (push ws hidden))
        (:merging (push ws merging))
        (:merged  (push ws merged))))
    `((:main    . ,main)
      (:hidden  . ,hidden)
      (:merging . ,merging)
      (:merged  . ,merged))))

(defun claude-repl-drawer--build-tree (workspaces parent-fn)
  "Build a forest of trees from WORKSPACES using PARENT-FN to resolve parents.
PARENT-FN takes a workspace name and returns its parent in this
section, or nil if it's a root.  Each tree is `(WS . CHILDREN)' where
CHILDREN is a list of trees.  Roots and siblings are sorted by
`claude-repl-drawer--sort'."
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
                                  (claude-repl-drawer--sort
                                   (or (gethash ws children-of) nil))))))
        (mapcar #'build (claude-repl-drawer--sort roots))))))

;;;; Render -----------------------------------------------------------------

(defun claude-repl-drawer--state-glyph (ws)
  "Return the indicator glyph for workspace WS."
  (let ((repl-state (claude-repl--ws-get ws :repl-state))
        (claude-state (claude-repl--ws-get ws :claude-state)))
    (or (and (eq repl-state :dead)
             (alist-get :dead claude-repl-drawer-state-icons))
        (alist-get claude-state claude-repl-drawer-state-icons)
        claude-repl-drawer-state-icon-default)))

(defun claude-repl-drawer--priority-display (priority)
  "Return a display string for PRIORITY.
Uses the badge PNG from `claude-repl--priority-images' when available,
falling back to the raw PRIORITY string for terminal/batch contexts.
Returns the empty string when PRIORITY is nil so unprioritized
workspaces don't carry a phantom space."
  (cond
   ((null priority) "")
   ((claude-repl--priority-image priority)
    (propertize priority 'display (claude-repl--priority-image priority)))
   (t priority)))

(defun claude-repl-drawer--name-face (ws)
  "Return the face spec for WS's name, colored by claude-state.
:dead falls through to the default workspace-name face (the existing
hidden/dim treatment provides the muting).  Unrecognized states render
as plain bold."
  (let* ((repl-state   (claude-repl--ws-get ws :repl-state))
         (claude-state (claude-repl--ws-get ws :claude-state))
         (color (cond
                 ((eq repl-state :dead)        nil)
                 ((eq claude-state :init)      claude-repl--color-init-blue)
                 ((eq claude-state :thinking)  claude-repl--color-thinking-red)
                 ((memq claude-state '(:done :permission))
                  claude-repl--color-done-green)
                 ((eq claude-state :idle)      claude-repl--color-idle-orange)
                 ((eq claude-state :stop-failed)
                  claude-repl--color-stop-failed-magenta))))
    (if color
        `(:foreground ,color :weight bold)
      'claude-repl-drawer-workspace-name)))

(defun claude-repl-drawer--summary-text (ws)
  "Return the aiTitle/prompt-summary string for WS, or a placeholder."
  (let ((summary (claude-repl--ws-get ws :last-prompt-summary))
        (pending (claude-repl--ws-get ws :last-prompt-summary-pending)))
    (cond
     ((and (stringp summary) (not (string-empty-p summary)))
      summary)
     (pending "…")
     (t "—"))))

(defun claude-repl-drawer--current-ws ()
  "Return the currently active workspace name, or nil."
  (and (fboundp '+workspace-current-name) (+workspace-current-name)))

(defun claude-repl-drawer--render-workspace (ws _current hidden &optional depth)
  "Insert the rendered representation for workspace WS into the current buffer.
Optional DEPTH (default 0) shifts the entry right by `depth × 2'
spaces *after* the static gutter — the gutter stays at column 0 so
the current-entry arrow overlay aligns regardless of nesting.  Sets a
`wrap-prefix' text property on both header and summary lines so soft
word-wrap continuation lines indent to the same start column as the
content (rather than back to column 0).  HIDDEN dims the block."
  (let* ((depth      (or depth 0))
         (priority   (claude-repl--ws-get ws :priority))
         (glyph      (claude-repl-drawer--state-glyph ws))
         (dirty      (eq (claude-repl--ws-get ws :git-clean) 'dirty))
         (start      (point))
         (prio-disp  (claude-repl-drawer--priority-display priority))
         (sep        (if priority " " ""))
         (name-face  (claude-repl-drawer--name-face ws))
         (indent-str (make-string (* depth claude-repl-drawer-indent-per-level)
                                  ?\s))
         (gutter-str (if (claude-repl-drawer--marked-p ws)
                         (propertize claude-repl-drawer-marked-glyph
                                     'face 'claude-repl-drawer-marked)
                       claude-repl-drawer-gutter))
         (header     (concat gutter-str indent-str
                             glyph "  " prio-disp sep
                             (propertize ws 'face name-face)
                             (if dirty " ●" "")))
         (summary    (claude-repl-drawer--summary-text ws))
         ;; Continuation lines align with content: header's content
         ;; (the glyph) starts at gutter+indent; summary's content
         ;; starts at gutter+indent+2 (the leading two-space pad
         ;; before the summary text).
         (header-wrap-prefix
          (concat claude-repl-drawer-gutter indent-str))
         (summary-wrap-prefix
          (concat claude-repl-drawer-gutter indent-str "  ")))
    (let ((header-start (point)))
      (insert header "\n")
      (add-text-properties header-start (point)
                           (list 'wrap-prefix header-wrap-prefix)))
    (let ((summary-start (point)))
      (insert claude-repl-drawer-gutter indent-str "  "
              (propertize summary 'face 'claude-repl-drawer-summary)
              "\n")
      (add-text-properties summary-start (point)
                           (list 'wrap-prefix summary-wrap-prefix)))
    (let ((end (point)))
      (add-text-properties
       start end
       (list 'claude-repl-drawer-workspace ws
             'help-echo (format "Workspace: %s%s"
                                ws
                                (if hidden " (hidden)" ""))))
      (when hidden
        (add-face-text-property start end 'claude-repl-drawer-hidden)))))

(defun claude-repl-drawer--insert-section-header (label)
  "Insert a bold section header LABEL with a rule line beneath it."
  (insert (propertize (format " %s\n" label)
                      'face 'claude-repl-drawer-section-title))
  (insert (propertize (concat " "
                              (make-string claude-repl-drawer-section-rule-width
                                           ?─)
                              "\n")
                      'face 'claude-repl-drawer-section-rule)))

(defun claude-repl-drawer--render-subtree (tree depth current section)
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
    (claude-repl-drawer--render-workspace ws current
                                          (eq section :hidden)
                                          depth)
    (when (claude-repl-drawer--expanded-p ws)
      (claude-repl-drawer--render-detail-lines ws depth)))
  (dolist (child (cdr tree))
    (claude-repl-drawer--render-subtree child (1+ depth) current section)))

(defun claude-repl-drawer--workspace-group-key (ws)
  "Return a stable group key for WS based on git common-dir.
Cached on the workspace plist as `:group-key' so each workspace runs
git at most once.  Returns nil when WS has no project-dir or git
fails — such workspaces fall into the unlabeled `(no repo)' bucket."
  (or (claude-repl--ws-get ws :group-key)
      (when-let* ((dir (ignore-errors (claude-repl--ws-dir ws)))
                  (raw (claude-repl--git-string-quiet
                        "-C" dir "rev-parse" "--git-common-dir")))
        (when (and raw (not (string-empty-p raw))
                   (not (string-prefix-p "fatal" raw)))
          (let* ((abs (if (file-name-absolute-p raw) raw
                        (expand-file-name raw dir)))
                 (key (claude-repl--path-canonical abs)))
            (claude-repl--ws-put ws :group-key key)
            key)))))

(defun claude-repl-drawer--group-label (key)
  "Derive a human-readable group label from KEY (a canonical .git path).
Returns the basename of KEY's parent directory — i.e. the project
name, since git's common-dir is conventionally `<project>/.git'."
  (when key
    (let ((parent (file-name-directory key)))
      (when parent
        (file-name-nondirectory (directory-file-name parent))))))

(defun claude-repl-drawer--group-trees-by-repo (trees)
  "Partition TREES into (LABEL . TREES-IN-GROUP) buckets by repo.
Bucket keys are derived from each tree root's `:group-key' via
`--workspace-group-key' and `--group-label'.  Insertion order of the
returned alist matches the first-encounter order in TREES, so groups
appear in the order their first root appeared after sorting."
  (let ((order nil)
        (buckets (make-hash-table :test 'equal)))
    (dolist (tree trees)
      (let* ((root  (car tree))
             (key   (claude-repl-drawer--workspace-group-key root))
             (label (or (claude-repl-drawer--group-label key) "(no repo)")))
        (unless (gethash label buckets)
          (push label order))
        (puthash label
                 (append (gethash label buckets) (list tree))
                 buckets)))
    (mapcar (lambda (label) (cons label (gethash label buckets)))
            (nreverse order))))

(defun claude-repl-drawer--render-trees (trees current section)
  "Render TREES (forest) grouped by top-level repo.
Within a group, root subtrees render contiguously with a blank line
between siblings.  Between groups, a labeled separator (plus blank
line) marks the boundary so multi-repo drawers stay scannable."
  (let ((groups (claude-repl-drawer--group-trees-by-repo trees))
        (first-group t))
    (dolist (group groups)
      (let ((label       (car group))
            (group-trees (cdr group)))
        (unless first-group
          (insert "\n"))
        (insert (propertize (format claude-repl-drawer-group-label-format label)
                            'face 'claude-repl-drawer-group-label))
        (let ((tree-rest group-trees))
          (while tree-rest
            (claude-repl-drawer--render-subtree (car tree-rest) 0
                                                current section)
            (when (cdr tree-rest) (insert "\n"))
            (setq tree-rest (cdr tree-rest))))
        (setq first-group nil)))))

(defun claude-repl-drawer--parent-fn-for-section (workspaces section)
  "Return a parent-resolution function for SECTION.
:main and :hidden flatten through merged/completed ancestors;
:merging and :merged preserve original topology so their internal
parent/child structure stays intact."
  (lambda (ws)
    (cond
     ((memq section '(:merged :merging))
      (claude-repl-drawer--effective-parent-in-section ws workspaces))
     (t
      (claude-repl-drawer--effective-parent ws workspaces)))))

(defun claude-repl-drawer--insert-section (label workspaces current section)
  "Render a section titled LABEL.
WORKSPACES is the list of names belonging to this section.  CURRENT
is the currently selected workspace (per persp).  SECTION is :main,
:hidden, :merging, or :merged — controls parent-resolution and dim
treatment.  Empty sections render the `(none)' placeholder under the
header."
  (claude-repl-drawer--insert-section-header label)
  (if (null workspaces)
      (insert (propertize (format "  %s\n"
                                  claude-repl-drawer-empty-section-label)
                          'face 'claude-repl-drawer-empty))
    (let* ((parent-fn (claude-repl-drawer--parent-fn-for-section
                       workspaces section))
           (trees (claude-repl-drawer--build-tree workspaces parent-fn)))
      (claude-repl-drawer--render-trees trees current section))))

(defun claude-repl-drawer--render ()
  "Render the drawer: MAIN, HIDDEN, MERGING, MERGED, in that order.
MERGING precedes MERGED so the user reads the lifecycle top-to-bottom:
in-progress (MAIN/HIDDEN) → changes-ready (MERGING) → completed
(MERGED)."
  (let* ((inhibit-read-only t)
         (saved-line (line-number-at-pos))
         (saved-col  (current-column))
         ;; Anchor cursor restoration by workspace identity, not just line
         ;; number: line numbers shift when an entry above the cursor
         ;; expands/collapses or appears/disappears between polls, and
         ;; `forward-line saved-line' can then land on a non-workspace
         ;; line (detail line, blank, section header), which causes
         ;; `--update-current-entry-overlay' to delete the arrow.  Nested
         ;; children sit deeper in the buffer and so are most affected.
         (saved-ws   (claude-repl-drawer--workspace-at-point))
         (current    (claude-repl-drawer--current-ws))
         (sections   (claude-repl-drawer--partition-by-section
                      (hash-table-keys claude-repl--workspaces))))
    (erase-buffer)
    (insert "\n")
    (let ((mains    (alist-get :main    sections))
          (hiddens  (alist-get :hidden  sections))
          (mergings (alist-get :merging sections))
          (mergeds  (alist-get :merged  sections)))
      (claude-repl-drawer--insert-section
       (format "MAIN (%d)" (length mains))      mains    current :main)
      (insert "\n")
      (claude-repl-drawer--insert-section
       (format "HIDDEN (%d)" (length hiddens))  hiddens  current :hidden)
      (insert "\n")
      (claude-repl-drawer--insert-section
       (format "MERGING (%d)" (length mergings)) mergings current :merging)
      (insert "\n")
      (claude-repl-drawer--insert-section
       (format "MERGED (%d)" (length mergeds))  mergeds  current :merged))
    (unless (and saved-ws
                 (claude-repl-drawer--goto-workspace-line saved-ws))
      (goto-char (point-min))
      (forward-line (1- saved-line))
      (move-to-column saved-col))
    ;; `erase-buffer' above collapses the current-entry overlay to (1,1).
    ;; Reposition it here so the arrow persists across 1Hz status-poll
    ;; renders, when the drawer's buffer-local post-command-hook is not
    ;; firing (drawer not selected).
    (claude-repl-drawer--update-current-entry-overlay)))

;;;; Navigation -------------------------------------------------------------

(defun claude-repl-drawer--workspace-at-point ()
  "Return the workspace name at point, or nil."
  (get-text-property (point) 'claude-repl-drawer-workspace))

(defun claude-repl-drawer--goto-workspace-line (ws)
  "Move point to the start of the line for workspace WS, if present.
Returns non-nil on success."
  (let ((target nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not target) (not (eobp)))
        (when (equal (get-text-property (point) 'claude-repl-drawer-workspace) ws)
          (setq target (point)))
        (forward-line 1)))
    (when target
      (goto-char target)
      t)))

(defun claude-repl-drawer-next ()
  "Move point to the next workspace entry."
  (interactive)
  (let ((current (claude-repl-drawer--workspace-at-point))
        (start   (point))
        (found   nil))
    (forward-line 1)
    (while (and (not found) (not (eobp)))
      (let ((ws (claude-repl-drawer--workspace-at-point)))
        (if (and ws (not (equal ws current)))
            (setq found t)
          (forward-line 1))))
    (unless found
      (goto-char start))))

(defun claude-repl-drawer-prev ()
  "Move point to the previous workspace entry."
  (interactive)
  (let ((current (claude-repl-drawer--workspace-at-point))
        (start   (point))
        (found   nil))
    (forward-line -1)
    (while (and (not found) (not (bobp)))
      (let ((ws (claude-repl-drawer--workspace-at-point)))
        (if (and ws (not (equal ws current)))
            (setq found t)
          (forward-line -1))))
    (when found
      ;; Snap to the start of the workspace block (handles the summary
      ;; subtitle line being the first one we land on when moving up).
      (let ((ws (claude-repl-drawer--workspace-at-point)))
        (while (and (not (bobp))
                    (equal (get-text-property (1- (point))
                                              'claude-repl-drawer-workspace)
                           ws))
          (forward-line -1))))
    (unless found
      (goto-char start))))

;;;; Commands ---------------------------------------------------------------

(defun claude-repl-drawer--leave-side-window-before-switch ()
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

No-op when the selected window is not a side window."
  (when (window-parameter (selected-window) 'window-side)
    (when-let ((main (and (fboundp 'window-main-window) (window-main-window))))
      (select-window main))))

(defun claude-repl-drawer-visit ()
  "Switch to the workspace at point."
  (interactive)
  (let ((ws (claude-repl-drawer--workspace-at-point)))
    (unless ws
      (user-error "No workspace at point"))
    (claude-repl--log ws "drawer-visit: ws=%s" ws)
    (claude-repl-drawer--leave-side-window-before-switch)
    (+workspace-switch ws)))

(defun claude-repl-drawer-refresh ()
  "Manually refresh the drawer contents.
Also refreshes the detail cache for any currently-expanded entries
so their git-derived fields (commits ahead, last commit, etc.) are
re-fetched."
  (interactive)
  (when-let ((buf (get-buffer claude-repl-drawer-buffer-name)))
    (with-current-buffer buf
      (when claude-repl-drawer--expanded-set
        (maphash (lambda (ws _)
                   (claude-repl-drawer--refresh-detail-cache ws))
                 claude-repl-drawer--expanded-set))
      (claude-repl-drawer--render))))

(defun claude-repl-drawer--require-ws-at-point ()
  "Return the workspace name at point, or signal a user-error."
  (or (claude-repl-drawer--workspace-at-point)
      (user-error "No workspace at point")))

;;;; Multi-select -----------------------------------------------------------

(defvar-local claude-repl-drawer--marked-set nil
  "Hash table of workspace names currently marked for bulk operations.
Buffer-local: each drawer buffer has its own set.  Keys are workspace
names, values are `t' (presence is the signal).")

(defun claude-repl-drawer--ensure-marked-set ()
  "Initialize `claude-repl-drawer--marked-set' if not yet created."
  (unless claude-repl-drawer--marked-set
    (setq-local claude-repl-drawer--marked-set
                (make-hash-table :test 'equal))))

(defun claude-repl-drawer--marked-p (ws)
  "Return non-nil when WS is in the marked-set."
  (and claude-repl-drawer--marked-set
       (gethash ws claude-repl-drawer--marked-set)))

(defun claude-repl-drawer--marked-count ()
  "Return the number of marked entries in the current drawer buffer."
  (if claude-repl-drawer--marked-set
      (hash-table-count claude-repl-drawer--marked-set)
    0))

(defun claude-repl-drawer--target-workspaces ()
  "Return the list of workspaces an action should target.
The marked-set if non-empty; otherwise just the entry at point.  This
is the standard 'act on marks if any, else on point' idiom — no
duplicate keybindings for bulk versions."
  (if (> (claude-repl-drawer--marked-count) 0)
      (hash-table-keys claude-repl-drawer--marked-set)
    (list (claude-repl-drawer--require-ws-at-point))))

(defun claude-repl-drawer-toggle-mark ()
  "Toggle the mark on the entry at point.
Marked entries render with a red `●' in the gutter and become the
target set for action keys (x/d/i/M).  Auto-advances to the next
entry as a quality-of-life convenience."
  (interactive)
  (let ((ws (claude-repl-drawer--require-ws-at-point)))
    (claude-repl-drawer--ensure-marked-set)
    (if (gethash ws claude-repl-drawer--marked-set)
        (remhash ws claude-repl-drawer--marked-set)
      (puthash ws t claude-repl-drawer--marked-set))
    (claude-repl-drawer--render)
    (claude-repl-drawer-next)))

(defun claude-repl-drawer-clear-marks ()
  "Clear all marks in the current drawer buffer."
  (interactive)
  (when claude-repl-drawer--marked-set
    (clrhash claude-repl-drawer--marked-set))
  (claude-repl-drawer--render))

(defun claude-repl-drawer-nuke ()
  "Nuke the target workspaces.
Targets the marked-set when non-empty, otherwise the entry at point.
Mirrors `SPC j x' (`claude-repl-nuke-workspace') per target."
  (interactive)
  (dolist (ws (claude-repl-drawer--target-workspaces))
    (claude-repl-nuke-workspace ws)))

(defun claude-repl-drawer-kill ()
  "Kill the target workspaces.
Targets the marked-set when non-empty, otherwise the entry at point.
Mirrors `SPC j d' (`claude-repl-kill-workspace') per target."
  (interactive)
  (dolist (ws (claude-repl-drawer--target-workspaces))
    (claude-repl-kill-workspace ws)))

(defun claude-repl-drawer-interrupt ()
  "Interrupt Claude in the target workspaces.
Targets the marked-set when non-empty, otherwise the entry at point.
Mirrors `C-c C-k' per target."
  (interactive)
  (dolist (ws (claude-repl-drawer--target-workspaces))
    (claude-repl-interrupt ws)))

(defun claude-repl-drawer-send-prompt ()
  "Read a prompt and send it to the target workspaces.
Targets the marked-set when non-empty, otherwise the entry at point.
Mirrors the normal claude send (`claude-repl--send'), including
history logging.  After send, each target's summary transitions to
`:last-prompt-summary-pending' and renders as `…' until the haiku
summarizer returns the new aiTitle."
  (interactive)
  (let* ((targets (claude-repl-drawer--target-workspaces))
         (prompt  (read-string
                   (if (= 1 (length targets))
                       (format "Send to %s: " (car targets))
                     (format "Send to %d workspaces: " (length targets))))))
    (when (and prompt (not (string-empty-p prompt)))
      (dolist (ws targets)
        (claude-repl--send prompt ws)))))

(defun claude-repl-drawer--with-temp-current-ws (ws fn)
  "Switch to WS, call FN, then return to the previous workspace.
Used to dispatch merge commands for the entry at point — the merge
public functions read `(+workspace-current-name)' internally and
switch perspectives themselves, so we must temporarily inhabit the
target workspace before invoking them.

Leaves the drawer side window before each switch (see
`claude-repl-drawer--leave-side-window-before-switch') so persp's
restore doesn't clobber the destination workspace's panel state."
  (let ((prev (and (fboundp '+workspace-current-name)
                   (+workspace-current-name))))
    (claude-repl-drawer--leave-side-window-before-switch)
    (+workspace-switch ws)
    (unwind-protect
        (funcall fn)
      (when (and prev (not (equal prev (+workspace-current-name))))
        (claude-repl-drawer--leave-side-window-before-switch)
        (+workspace-switch prev)))))

(defun claude-repl-drawer-merge-into-master ()
  "Merge the target workspaces into their source/master.
Targets the marked-set when non-empty, otherwise the entry at point.
Mirrors `SPC TAB M' (`claude-repl-workspace-merge-current-into-source')
per target.  Each target requires temporarily switching to that
workspace before invoking the public function."
  (interactive)
  (dolist (ws (claude-repl-drawer--target-workspaces))
    (claude-repl-drawer--with-temp-current-ws
     ws #'claude-repl-workspace-merge-current-into-source)))

(defun claude-repl-drawer-merge-child ()
  "Merge a child workspace into the entry at point.
Mirrors `SPC TAB m' (`claude-repl-workspace-merge').  The public
function uses the current workspace as the merge destination and
prompts for the child to merge in, so we temporarily switch to the
entry-at-point before invoking it."
  (interactive)
  (claude-repl-drawer--with-temp-current-ws
   (claude-repl-drawer--require-ws-at-point)
   #'claude-repl-workspace-merge))

(defun claude-repl-drawer-new-child ()
  "Create a new worktree branched from the entry at point.
Mirrors `SPC TAB n' (`claude-repl-create-worktree-workspace') with
BASE = `head' and SOURCE-WS = entry-at-point.  The public function
prompts for the preemptive prompt and dispatches to the async
workspace-generation skill."
  (interactive)
  (claude-repl-create-worktree-workspace
   'head (claude-repl-drawer--require-ws-at-point)))

(defcustom claude-repl-drawer-priority-cycle
  '("p05" "p1" "p2" "p3" nil)
  "Ordered list (highest → lowest) used by drawer `+'/`-' priority cycling.
The trailing `nil' represents 'no priority'.  Cycle wraps at both ends."
  :type '(repeat (choice string (const nil)))
  :group 'claude-repl)

(defun claude-repl-drawer--cycle-priority (ws step)
  "Cycle WS's priority by STEP through `claude-repl-drawer-priority-cycle'.
STEP is -1 (toward the head of the cycle, e.g. p05) or +1 (toward
the tail, e.g. nil).  Calls `claude-repl-set-priority' with the new
value (empty string when cycling to nil, since that's set-priority's
clear sentinel)."
  (let* ((cur (claude-repl--ws-get ws :priority))
         (cycle claude-repl-drawer-priority-cycle)
         (n (length cycle))
         (idx (or (cl-position cur cycle :test #'equal) (1- n)))
         (new-idx (mod (+ idx step) n))
         (new (nth new-idx cycle))
         (new-arg (or new "")))
    (claude-repl-set-priority new-arg ws)))

(defun claude-repl-drawer-priority-up ()
  "Cycle the entry-at-point's priority up (toward p05)."
  (interactive)
  (claude-repl-drawer--cycle-priority
   (claude-repl-drawer--require-ws-at-point) -1)
  (claude-repl-drawer-refresh))

(defun claude-repl-drawer-priority-down ()
  "Cycle the entry-at-point's priority down (toward nil)."
  (interactive)
  (claude-repl-drawer--cycle-priority
   (claude-repl-drawer--require-ws-at-point) +1)
  (claude-repl-drawer-refresh))

(defun claude-repl-drawer-toggle-hidden ()
  "Toggle the entry-at-point's hidden state.
When `:repl-state' is `:hidden', calls `claude-repl--unhide-workspace'
to flip it back to `:active'.  Otherwise calls `claude-repl--on-close'
which sets `:hidden' (the deprio-close path).  Refreshes the drawer
so the entry moves between MAIN and HIDDEN sections."
  (interactive)
  (let* ((ws     (claude-repl-drawer--require-ws-at-point))
         (rstate (claude-repl--ws-get ws :repl-state)))
    (if (eq rstate :hidden)
        (claude-repl--unhide-workspace ws)
      (claude-repl--on-close ws))
    (claude-repl-drawer-refresh)))

(defun claude-repl-drawer-new-fork ()
  "Fork the claude session of the entry at point into a new worktree.
Mirrors `SPC TAB f' (`claude-repl-fork-worktree-workspace') with
SOURCE-WS = entry-at-point."
  (interactive)
  (claude-repl-fork-worktree-workspace
   (claude-repl-drawer--require-ws-at-point)))

(defun claude-repl-drawer--refresh-if-visible ()
  "Refresh the drawer if its buffer exists and is shown in some window.
Intended to be called from the 1Hz poll in `status.el'."
  (when-let* ((buf (get-buffer claude-repl-drawer-buffer-name))
              ((get-buffer-window buf t)))
    (with-current-buffer buf
      (claude-repl-drawer--render))))

;;;; Display + toggle -------------------------------------------------------

;;;; Expand-detail ----------------------------------------------------------

(defvar-local claude-repl-drawer--expanded-set nil
  "Hash table of workspace names currently expanded in detail view.
Buffer-local: one set per drawer buffer.  Keys are workspace names,
values are `t' (presence is the signal).")

(defun claude-repl-drawer--ensure-expanded-set ()
  "Create `claude-repl-drawer--expanded-set' if not yet initialized."
  (unless claude-repl-drawer--expanded-set
    (setq-local claude-repl-drawer--expanded-set
                (make-hash-table :test 'equal))))

(defun claude-repl-drawer--expanded-p (ws)
  "Return non-nil if WS is currently expanded."
  (and claude-repl-drawer--expanded-set
       (gethash ws claude-repl-drawer--expanded-set)))

(defun claude-repl-drawer--refresh-detail-cache (ws)
  "Populate WS's `:detail-*' plist fields with synchronous git calls.
Called from TAB-toggle (when expanding) and `g'-refresh.  Avoids
running git every poll cycle.  All values are best-effort: nil left
in place when the underlying command errors or returns empty."
  (when-let ((dir (ignore-errors (claude-repl--ws-dir ws))))
    (let* ((branch (claude-repl--git-string-quiet
                    "-C" dir "rev-parse" "--abbrev-ref" "HEAD")))
      (claude-repl--ws-put ws :detail-branch
                           (and branch (not (string-empty-p branch)) branch)))
    (let ((ahead (claude-repl--git-string-quiet
                  "-C" dir "rev-list" "--count"
                  (concat claude-repl-master-branch-name "..HEAD"))))
      (claude-repl--ws-put ws :detail-master-ahead
                           (and ahead (not (string-empty-p ahead))
                                (string-to-number ahead))))
    (when-let* ((src-dir (claude-repl--ws-get ws :source-ws-dir))
                ((file-directory-p src-dir))
                (src-branch (claude-repl--git-string-quiet
                             "-C" src-dir "rev-parse" "--abbrev-ref" "HEAD")))
      (when (and src-branch (not (string-empty-p src-branch))
                 (not (string-prefix-p "fatal" src-branch)))
        (let ((ahead (claude-repl--git-string-quiet
                      "-C" dir "rev-list" "--count"
                      (concat src-branch "..HEAD"))))
          (claude-repl--ws-put ws :detail-source-ahead
                               (and ahead (not (string-empty-p ahead))
                                    (string-to-number ahead))))))
    (let ((subj (claude-repl--git-string-quiet
                 "-C" dir "log" "-1" "--pretty=format:%s")))
      (claude-repl--ws-put ws :detail-last-commit
                           (and subj (not (string-empty-p subj)) subj)))
    (let ((tm (claude-repl--git-string-quiet
               "-C" dir "log" "-1" "--pretty=format:%ar")))
      (claude-repl--ws-put ws :detail-last-commit-time
                           (and tm (not (string-empty-p tm)) tm)))
    (let ((status (claude-repl--git-string-quiet
                   "-C" dir "status" "--porcelain")))
      (claude-repl--ws-put ws :detail-dirty-count
                           (if (or (null status) (string-empty-p status))
                               0
                             (length (split-string status "\n" t)))))))

(defun claude-repl-drawer--format-duration (seconds)
  "Format SECONDS as a short human-readable duration."
  (cond
   ((< seconds 60)    (format "%ds ago"  (round seconds)))
   ((< seconds 3600)  (format "%dm ago"  (round (/ seconds 60))))
   ((< seconds 86400) (format "%.1fh ago" (/ seconds 3600.0)))
   (t                 (format "%.1fd ago" (/ seconds 86400.0)))))

(defun claude-repl-drawer-toggle-expand ()
  "Toggle the expanded detail view for the entry at point.
On expand, refreshes the detail cache (synchronous git calls); on
collapse, removes from the expanded set.  Re-renders the drawer."
  (interactive)
  (let ((ws (claude-repl-drawer--require-ws-at-point)))
    (claude-repl-drawer--ensure-expanded-set)
    (if (gethash ws claude-repl-drawer--expanded-set)
        (remhash ws claude-repl-drawer--expanded-set)
      (claude-repl-drawer--refresh-detail-cache ws)
      (puthash ws t claude-repl-drawer--expanded-set))
    (claude-repl-drawer--render)))

(defun claude-repl-drawer--render-detail-lines (ws depth)
  "Insert detail lines for an expanded WS at DEPTH.
Reads only cached `:detail-*' fields and existing plist values; never
invokes git.  Caller is `--render-workspace-expanded'."
  (let* ((indent-str (make-string (* depth claude-repl-drawer-indent-per-level) ?\s))
         (detail-prefix (concat claude-repl-drawer-gutter indent-str "    "))
         (branch       (claude-repl--ws-get ws :detail-branch))
         (master-ahead (claude-repl--ws-get ws :detail-master-ahead))
         (source-ahead (claude-repl--ws-get ws :detail-source-ahead))
         (last-commit  (claude-repl--ws-get ws :detail-last-commit))
         (last-commit-time (claude-repl--ws-get ws :detail-last-commit-time))
         (dirty-count  (claude-repl--ws-get ws :detail-dirty-count))
         (last-prompt-time (claude-repl--ws-get ws :last-prompt-time))
         (pending-count (length (claude-repl--ws-get ws :pending-prompts))))
    (cl-flet ((line (label value face)
                (insert detail-prefix
                        (propertize (concat label " ") 'face 'shadow)
                        (propertize (format "%s" value)
                                    'face face
                                    'wrap-prefix detail-prefix)
                        "\n")))
      (when branch
        (line "branch:" branch 'claude-repl-drawer-detail-branch))
      (when master-ahead
        (line "ahead master:" (format "%d" master-ahead)
              'claude-repl-drawer-detail-ahead-master))
      (when source-ahead
        (line "ahead source:" (format "%d" source-ahead)
              'claude-repl-drawer-detail-ahead-source))
      (when last-commit
        (line "last commit:"
              (if last-commit-time
                  (format "%s (%s)" last-commit last-commit-time)
                last-commit)
              'claude-repl-drawer-detail-last-commit))
      (when (and dirty-count (> dirty-count 0))
        (line "dirty:" (format "%d files" dirty-count)
              'claude-repl-drawer-detail-dirty))
      (when last-prompt-time
        (line "last prompt:"
              (claude-repl-drawer--format-duration
               (- (float-time) last-prompt-time))
              'claude-repl-drawer-detail-last-prompt))
      (when (and pending-count (> pending-count 0))
        (line "pending:" (format "%d prompt(s)" pending-count)
              'claude-repl-drawer-detail-pending)))))

(defvar claude-repl-drawer--display-action
  `((display-buffer-in-side-window)
    (side . left)
    (slot . 0)
    (window-width . ,#'claude-repl-drawer--window-width)
    (window-parameters
     (no-delete-other-windows . t)
     (no-other-window . nil)))
  "Display action for the drawer buffer.")

(defun claude-repl-drawer--max-depth ()
  "Return the deepest workspace nesting depth in `claude-repl--workspaces'.
Walks each workspace's `:source-ws-dir' chain and returns the maximum
hop count.  Cycle-capped via `claude-repl-drawer-tree-max-depth'.
Slight overestimate when merged ancestors exist (the rendered tree
flattens through them, but this counts raw chain hops) — acceptable;
the drawer ends up a couple cols wider than strictly needed."
  (let ((maxd 0))
    (maphash
     (lambda (ws _)
       (let ((d 0)
             (cur (claude-repl-drawer--source-ws-name ws)))
         (while (and cur (< d claude-repl-drawer-tree-max-depth))
           (setq d (1+ d))
           (setq cur (claude-repl-drawer--source-ws-name cur)))
         (when (> d maxd) (setq maxd d))))
     claude-repl--workspaces)
    maxd))

(defun claude-repl-drawer--window-width (window)
  "Return the configured drawer width in columns for WINDOW.
When `claude-repl-drawer--persisted-cols' is non-nil (set by a prior
user resize), that value wins — the user's pinned width follows them
across every workspace.  Otherwise the width is the sum of
`claude-repl-drawer-width-fraction' × frame-width and a depth bonus
equal to (max-tree-depth × `claude-repl-drawer-indent-per-level').
The bonus expands the drawer by exactly the same amount the render
indents children, so deep trees never get clipped — and changing
either knob propagates through both render and width consistently."
  (if claude-repl-drawer--persisted-cols
      (max 1 claude-repl-drawer--persisted-cols)
    (let* ((frame-cols (frame-width (window-frame window)))
           (base       (round (* claude-repl-drawer-width-fraction frame-cols)))
           (depth-bonus (* (claude-repl-drawer--max-depth)
                           claude-repl-drawer-indent-per-level)))
      (max 1 (+ base depth-bonus)))))

(defun claude-repl-drawer--get-or-create-buffer ()
  "Return the drawer buffer, creating and initializing if necessary."
  (let ((buf (get-buffer claude-repl-drawer-buffer-name)))
    (unless (buffer-live-p buf)
      (setq buf (get-buffer-create claude-repl-drawer-buffer-name))
      (with-current-buffer buf
        (claude-repl-drawer-mode)))
    buf))

(defun claude-repl-drawer--goto-first-workspace ()
  "Move point to the first workspace line in the current buffer, if any."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (if (claude-repl-drawer--workspace-at-point)
          (setq found t)
        (forward-line 1)))
    found))

(defun claude-repl-drawer--apply-width (window)
  "Resize WINDOW to the configured drawer width.
Side-window action alists honor `window-width' only at window-creation
time, so a re-shown drawer keeps its old width even when the fraction
changed.  This forces the resize on every show.

Uses `shrink-window'/`enlarge-window' rather than `window-resize'
because side windows route through `window--resize-side-windows',
which silently rejects the direct `window-resize' path under
constraints (parent window slack, fixed-size flags, etc.) — the
shrink/enlarge wrappers go through the side-window aware codepath
and actually apply the delta.  Locally lowers `window-min-width' so
fractions below the global default (10 cols) are honored, and clears
`window-size-fixed' on the buffer in case a prior pass locked it.

Records the applied width in `claude-repl-drawer--expected-cols' so
`--capture-resize' can distinguish this programmatic resize from a
subsequent user gesture."
  (let* ((target (claude-repl-drawer--window-width window))
         (window-min-width 1))
    (with-selected-window window
      (setq-local window-size-fixed nil)
      (let ((delta (- target (window-total-width window))))
        (cond
         ((> delta 0) (enlarge-window delta t))
         ((< delta 0) (shrink-window (abs delta) t))))
      (setq claude-repl-drawer--expected-cols
            (window-total-width window)))))

(defun claude-repl-drawer--capture-resize (frame-or-window)
  "Persist a manual drawer-window resize so it survives workspace switches.
Compares the drawer's current total width to
`claude-repl-drawer--expected-cols' (last value applied programmatically
by `--apply-width').  Divergence signals a user gesture (S-arrow drag,
`enlarge-window-horizontally', mouse drag of the side window edge,
...), and the new width is pinned into
`claude-repl-drawer--persisted-cols' so every subsequent persp
activation re-establishes it via `--apply-width'.

Hooked into `window-size-change-functions', which fires once per frame
at the end of redisplay — by which point persp-mode's restore +
`--ensure-visible-on-persp-switch's reapply have already settled,
so persp-driven sizing transients don't get mistaken for user intent
(actual = expected after our reapply).

The argument may be a frame or a window depending on the Emacs
version's calling convention; both are tolerated."
  (let ((frame (if (windowp frame-or-window)
                   (window-frame frame-or-window)
                 frame-or-window)))
    (when-let* ((buf (get-buffer claude-repl-drawer-buffer-name))
                (win (get-buffer-window buf frame)))
      (let ((actual (window-total-width win)))
        (when (and claude-repl-drawer--expected-cols
                   (/= actual claude-repl-drawer--expected-cols))
          (setq claude-repl-drawer--persisted-cols actual
                claude-repl-drawer--expected-cols actual))))))

(add-hook 'window-size-change-functions
          #'claude-repl-drawer--capture-resize)

(defun claude-repl-drawer-reset-width ()
  "Clear the user-pinned drawer width and revert to the fraction default.
Reapplies width immediately to any live drawer window."
  (interactive)
  (setq claude-repl-drawer--persisted-cols nil)
  (when-let* ((buf (get-buffer claude-repl-drawer-buffer-name))
              (win (get-buffer-window buf t)))
    (claude-repl-drawer--apply-width win)))

(defvar claude-repl-drawer--global-visible-p nil
  "Non-nil when the drawer should appear in every workspace/persp.
Set by `claude-repl-drawer-show', cleared by
`claude-repl-drawer-hide'.  The persp-activated hook
(`claude-repl-drawer--ensure-visible-on-persp-switch') consults this
flag and re-displays the drawer in newly-activated workspaces so the
drawer feels like a frame-level UI element rather than a per-workspace
artifact.")

(defun claude-repl-drawer-show ()
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
immediately, not after the next command."
  (interactive)
  (let* ((buf        (claude-repl-drawer--get-or-create-buffer))
         (current-ws (claude-repl-drawer--current-ws))
         (win        (display-buffer buf claude-repl-drawer--display-action)))
    (with-current-buffer buf
      (add-hook 'post-command-hook
                #'claude-repl-drawer--post-command nil t)
      (setq-local cursor-type nil)
      ;; Self-heal: pre-existing buffers from before the wrap rollout
      ;; still have `truncate-lines' = t.  These are buffer-local; the
      ;; mode-init only fires on first activation.
      (setq-local truncate-lines nil
                  word-wrap t)
      (claude-repl-drawer--apply-background)
      (claude-repl-drawer--render)
      ;; Position cursor on current-ws (or first entry) without
      ;; selecting the drawer window — the bounce hook would redirect
      ;; us away anyway, so do it directly via `set-window-point'.
      (or (and current-ws
               (claude-repl-drawer--goto-workspace-line current-ws))
          (claude-repl-drawer--goto-first-workspace))
      (when (and win (window-live-p win))
        (set-window-point win (point)))
      (claude-repl-drawer--post-command))
    (when win
      ;; Drawer hardening recipe: dedicated (no display-buffer repurpose);
      ;; fringes 0/0 to suppress the wrap-continuation arrow.
      ;; `no-delete-other-windows' is set declaratively via the
      ;; display-action's `window-parameters', so it isn't repeated here.
      (claude-repl-window--harden win :dedicate t :fringes 0)
      (claude-repl-drawer--apply-width win))
    (setq claude-repl-drawer--global-visible-p t)
    win))

(defun claude-repl-drawer-hide ()
  "Hide the workspace drawer.
Clears the global visible-flag so the drawer no longer auto-appears
on workspace switches.

Each drawer window is explicitly un-dedicated before deletion — the
display-action gives the drawer regular (`t') dedication which does
NOT block `delete-window' in modern Emacs, but the un-dedicate is
preserved as defense against legacy strong-dedication code paths
that may have set a non-`t' value.  Delegates the deletion itself to
`claude-repl-window--delete-buffer-windows', which targets the
buffer specifically and bypasses the side-window skip — drawer
hiding genuinely wants to delete a side window."
  (interactive)
  (setq claude-repl-drawer--global-visible-p nil)
  (when-let ((buf (get-buffer claude-repl-drawer-buffer-name)))
    (dolist (win (get-buffer-window-list buf nil t))
      (when (window-live-p win)
        (set-window-dedicated-p win nil)))
    (claude-repl-window--delete-buffer-windows buf)))

;;;; Global drawer-mirror dispatch -----------------------------------------

(defun claude-repl-drawer--call-in-drawer (fn &optional preserve-cursor)
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
(e.g. after `claude-repl-drawer-nuke'), the cursor is left wherever
FN naturally placed it.

We deliberately avoid `with-selected-window' here.  Selecting and then
unselecting the drawer window for every keystroke fires window-selection
hooks and redisplays the modeline/hl-line/etc., which makes rapid global
navigation (`C-S-n' / `C-S-p') feel sluggish compared to in-drawer `j' /
`k' — those just move point inside the already-selected window and pay
none of that overhead.

Instead we run FN with the drawer current, then mirror the new
buffer-point onto the displayed window via `set-window-point' and
manually invoke `--post-command' so the current-entry arrow tracks
immediately (the buffer-local post-command-hook would not fire because
the actual command is running in the caller's buffer, not the drawer).

Errors if no drawer buffer exists — caller can recover by toggling
the drawer open with `SPC o d' first."
  (let ((buf (or (get-buffer claude-repl-drawer-buffer-name)
                 (user-error "Drawer not open — `SPC o d' first"))))
    (with-current-buffer buf
      (let ((pre-ws (and preserve-cursor
                         (claude-repl-drawer--workspace-at-point))))
        (funcall fn)
        (when pre-ws
          (claude-repl-drawer--goto-workspace-line pre-ws)))
      (claude-repl-drawer--post-command)
      ;; Re-query the drawer window after FN: persp's window-config
      ;; restoration during a workspace switch can replace it with a
      ;; fresh window, leaving any pre-fn handle dead.
      (when-let ((win (get-buffer-window buf t)))
        (set-window-point win (point))))))

(defun claude-repl-drawer-global-next ()
  "Move drawer cursor to next entry from any window."
  (interactive)
  (claude-repl-drawer--call-in-drawer #'claude-repl-drawer-next))

(defun claude-repl-drawer-global-prev ()
  "Move drawer cursor to previous entry from any window."
  (interactive)
  (claude-repl-drawer--call-in-drawer #'claude-repl-drawer-prev))

(defun claude-repl-drawer-global-visit ()
  "Visit (switch to) the workspace at the drawer cursor."
  (interactive)
  (claude-repl-drawer--call-in-drawer #'claude-repl-drawer-visit t))

(defun claude-repl-drawer-global-nuke ()
  "Nuke the workspace at the drawer cursor."
  (interactive)
  (claude-repl-drawer--call-in-drawer #'claude-repl-drawer-nuke t))

(defun claude-repl-drawer-global-kill ()
  "Kill the workspace at the drawer cursor."
  (interactive)
  (claude-repl-drawer--call-in-drawer #'claude-repl-drawer-kill t))

(defun claude-repl-drawer-global-send-prompt ()
  "Read a prompt and send to the workspace at the drawer cursor."
  (interactive)
  (claude-repl-drawer--call-in-drawer #'claude-repl-drawer-send-prompt t))

(defun claude-repl-drawer-global-merge-into-master ()
  "Merge the workspace at the drawer cursor into its source/master."
  (interactive)
  (claude-repl-drawer--call-in-drawer #'claude-repl-drawer-merge-into-master t))

(defun claude-repl-drawer-global-new-fork ()
  "Fork the workspace at the drawer cursor into a new worktree."
  (interactive)
  (claude-repl-drawer--call-in-drawer #'claude-repl-drawer-new-fork t))

(defun claude-repl-drawer-global-toggle-hidden ()
  "Toggle hidden state of the workspace at the drawer cursor."
  (interactive)
  (claude-repl-drawer--call-in-drawer #'claude-repl-drawer-toggle-hidden t))

(defun claude-repl-drawer-global-toggle-mark ()
  "Toggle the mark on the workspace at the drawer cursor."
  (interactive)
  (claude-repl-drawer--call-in-drawer #'claude-repl-drawer-toggle-mark t))

(defun claude-repl-drawer-global-clear-marks ()
  "Clear all drawer marks."
  (interactive)
  (claude-repl-drawer--call-in-drawer #'claude-repl-drawer-clear-marks t))

(defun claude-repl-drawer-global-priority-up ()
  "Cycle priority of the workspace at the drawer cursor up (toward p05)."
  (interactive)
  (claude-repl-drawer--call-in-drawer #'claude-repl-drawer-priority-up t))

(defun claude-repl-drawer-global-priority-down ()
  "Cycle priority of the workspace at the drawer cursor down (toward nil)."
  (interactive)
  (claude-repl-drawer--call-in-drawer #'claude-repl-drawer-priority-down t))

;;;; Auto-revert: drawer cursor follows current workspace -------------------

(defun claude-repl-drawer--sync-cursor-to-current-ws (&rest _)
  "Position drawer cursor on the currently active workspace's entry.
No-op when the drawer buffer doesn't exist or no current workspace
can be resolved.  Used by `persp-activated-functions' and the global
post-command hook to keep the drawer cursor sync'd with the active
workspace whenever the user isn't actively navigating the drawer.

Also repositions the current-entry arrow overlay synchronously so the
arrow snaps to the active workspace immediately, not after the next
1Hz status-poll re-render.  The buffer-local `post-command-hook'
doesn't fire here because the actual command is running in a
different buffer (or via a persp-mode hook), so we mirror its
overlay-refresh action explicitly."
  (when-let* ((buf (get-buffer claude-repl-drawer-buffer-name))
              (current-ws (and (fboundp '+workspace-current-name)
                               (+workspace-current-name))))
    (let ((win (get-buffer-window buf t)))
      (with-current-buffer buf
        (when (claude-repl-drawer--goto-workspace-line current-ws)
          (when win (set-window-point win (point)))
          (claude-repl-drawer--update-current-entry-overlay))))))

(defvar claude-repl-drawer--last-was-drawer nil
  "Tracks whether the last command ran with the drawer buffer current.
Read by `--global-post-command' to detect focus-leave-drawer events.")

(defun claude-repl-drawer--global-post-command ()
  "Snap drawer cursor to current workspace when focus leaves the drawer.
Compares current buffer to drawer buffer; on transition `drawer →
elsewhere' calls `--sync-cursor-to-current-ws'.  Cheap (one buffer
identity compare) so safe as a global `post-command-hook'."
  (let* ((buf (get-buffer claude-repl-drawer-buffer-name))
         (in-drawer (and buf (eq (current-buffer) buf))))
    (when (and (not in-drawer) claude-repl-drawer--last-was-drawer)
      (claude-repl-drawer--sync-cursor-to-current-ws))
    (setq claude-repl-drawer--last-was-drawer in-drawer)))

(add-hook 'post-command-hook #'claude-repl-drawer--global-post-command)

(with-eval-after-load 'persp-mode
  (add-hook 'persp-activated-functions
            #'claude-repl-drawer--sync-cursor-to-current-ws))

(defun claude-repl-drawer--ensure-visible-on-persp-switch (&rest _)
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
  (let* ((buf (get-buffer claude-repl-drawer-buffer-name))
         (win (and buf (get-buffer-window buf))))
    (cond
     ;; Flag says show, drawer missing → display it.
     ((and claude-repl-drawer--global-visible-p (not win))
      (let* ((buf (claude-repl-drawer--get-or-create-buffer))
             (win (display-buffer buf claude-repl-drawer--display-action)))
        (with-current-buffer buf
          (add-hook 'post-command-hook
                    #'claude-repl-drawer--post-command nil t)
          (setq-local cursor-type nil
                      truncate-lines nil
                      word-wrap t)
          (claude-repl-drawer--apply-background))
        (when win
          ;; Same drawer recipe as `claude-repl-drawer-show' — dedicate +
          ;; fringes 0/0.  `no-delete-other-windows' comes from the
          ;; display-action's `window-parameters'.
          (claude-repl-window--harden win :dedicate t :fringes 0)
          (claude-repl-drawer--apply-width win)
          (set-window-point win (with-current-buffer buf (point))))))
     ;; Flag says show, drawer already visible → override persp's stale
     ;; restored width with the global pinned/configured one.
     ((and claude-repl-drawer--global-visible-p win)
      (claude-repl-drawer--apply-width win))
     ;; Flag says hide, persp restored a stale drawer window → delete it.
     ((and (not claude-repl-drawer--global-visible-p) win)
      (dolist (w (get-buffer-window-list buf nil t))
        (when (window-live-p w)
          (set-window-dedicated-p w nil)))
      (claude-repl-window--delete-buffer-windows buf)))))

(with-eval-after-load 'persp-mode
  (add-hook 'persp-activated-functions
            #'claude-repl-drawer--ensure-visible-on-persp-switch))

;;;; Keyboard-inaccessibility bounce ----------------------------------------
;;
;; Mirrors `claude-repl--bounce-from-vterm' (panels.el).  Same mechanism,
;; different target buffer and bounce destination: the vterm bounce
;; redirects to the workspace's input window (a well-known per-workspace
;; target); the drawer bounce has no comparable single target, so it
;; redirects to the most-recently-used non-drawer window — i.e. wherever
;; the user came from before keyboard nav landed in the drawer.
;;
;; Mouse clicks are exempt (checked via `last-input-event') so the user
;; can still click into the drawer to operate entries via RET/j/k/etc.;
;; only keyboard-driven selection (`windmove', `other-window', the
;; `select-window' previously inside `claude-repl-drawer-show', ...) is
;; redirected away.

(defun claude-repl-drawer--buffer-p (&optional buf)
  "Return non-nil if BUF (default: current buffer) is the drawer buffer.
Buffer-identity match against `claude-repl-drawer-buffer-name'."
  (let ((name (buffer-name (or buf (current-buffer)))))
    (and name (string= name claude-repl-drawer-buffer-name))))

(defun claude-repl-drawer--bounce-from-drawer (_frame)
  "If the selected window shows the drawer, redirect to the previous window.
Allows mouse-initiated selection through (checked via `mouse-event-p'
on `last-input-event') so clicking entries to visit a workspace works.
When no other live window exists on the frame, emits a warning via
`message' rather than leaving point stranded silently.

Predicate is buffer-identity (`claude-repl-drawer--buffer-p'), mirroring
the vterm bounce — buffer-name match is cheap and unambiguous since the
drawer buffer name is a defcustom-controlled singleton.

Hook target is `window-selection-change-functions', so this fires
during redisplay after a selection change rather than synchronously
on every `select-window' — body wrappers like `with-selected-window'
that select-and-restore the drawer for size adjustments don't
trigger spurious bounces because the net selected window after the
body is the original caller's window, not the drawer."
  (let ((win (selected-window)))
    (if (and (claude-repl-drawer--buffer-p (window-buffer win))
             (not (mouse-event-p last-input-event)))
        (let ((target (get-mru-window nil nil t)))
          (if (and target (window-live-p target))
              (select-window target)
            (message "[claude-repl] keyboard navigation landed in the drawer but no other window is available — click out or open another window")))
      nil)))

(add-hook 'window-selection-change-functions #'claude-repl-drawer--bounce-from-drawer)

(defun claude-repl-drawer-toggle ()
  "Toggle visibility of the workspace drawer."
  (interactive)
  (if-let* ((buf (get-buffer claude-repl-drawer-buffer-name))
            (win (get-buffer-window buf t)))
      (claude-repl-drawer-hide)
    (claude-repl-drawer-show)))

(provide 'claude-repl-drawer)
;;; drawer.el ends here
