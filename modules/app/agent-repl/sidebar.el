;;; sidebar.el --- Workspaces sidebar: roster feed + actions -*- lexical-binding: t; -*-

;;; Commentary:

;; Feeds the webapp's workspaces sidebar and services its actions.
;;
;; The roster universe is the LIVE workspaces — every registered,
;; non-tombstoned entry (`agent-repl--live-ws-names'), whether or not
;; a perspective is currently open for it.  Tombstoned registrations
;; and snapshot-only history do not render.  A live row agent-repl is
;; not actively hosting — no open perspective, or an open one whose
;; REPL is torn down — greys out (`agent-repl--sidebar-closed-p').
;;
;; Data flows one way, Emacs -> webview: Emacs owns the workspace model
;; (workspace.el), so it builds the roster — repos, family-nested rows,
;; per-row lifecycle status — serializes it to JSON, and pushes it into
;; every live webview through the `agentReplWorkspaceRoster' host hook
;; over the execute-script channel frontend.el owns.  The push is gated
;; by a cheap in-memory signature computed on the 1Hz state tick
;; (status.el), so the rebuild runs only when something visible changed.
;;
;; Actions flow the long way around, webview -> daemon -> Emacs: the
;; webapp cannot call into Emacs, so a sidebar click POSTs to the
;; daemon's /workspace-command endpoint, which drops a
;; workspace_commands_*.json file that worktree.el's watcher drains into
;; the "switch" / "fold" handlers here.
;;
;; Keyboard navigation never leaves Emacs: `C-S-n' / `C-S-p' (input.el,
;; from the input window — the xwidget swallows keystrokes, so the page
;; itself never sees keys) move an Emacs-owned cursor that rides the
;; roster push as `navDir' AND open the workspace it lands on, so the
;; highlight always doubles as the active selection.  `C-S-RET' unfolds
;; the cursor row's detail panel via the expand hook — the one gesture
;; that touches webapp-owned panel state instead of the roster.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--frontend-webview-execute-script "frontend" (buf script))
(declare-function agent-repl--picker-open-selection "commands" (payload))
(declare-function agent-repl--force-tab-bar-redraw "status" ())
(declare-function agent-repl--frontend-boot-session "frontends"
                  (ws &optional project-dir-hint active-env-hint))
(declare-function agent-repl--task-get "tasks" (id))
(declare-function agent-repl--task-create "tasks" (title))
(declare-function agent-repl--task-toggle-done "tasks" (id))
(declare-function agent-repl--task-open "tasks" (id))
(declare-function agent-repl--task-assign-workspace "tasks" (name id))
(declare-function agent-repl--ws-effective-task-id "tasks" (name))
(declare-function agent-repl--tasks-sorted "tasks" ())
(declare-function agent-repl--tasks-signature "tasks" ())

;;;; ---- The view selector -----------------------------------------------

(defvar agent-repl--sidebar-view :repository
  "Which sidebar view is active: `:repository' or `:task'.
`:repository' (the default) groups workspaces by their git repo; `:task'
groups them under the user-defined tasks (tasks.el) they belong to.
Session-scoped like the repo-fold set — a view preference the user
re-picks per session rather than persisted state.")

(defconst agent-repl--sidebar-view-wire
  '((:repository . "repository")
    (:task       . "task"))
  "Maps `agent-repl--sidebar-view' keywords onto the webapp's wire strings.
The value set is the webapp's WorkspaceRoster.view union
\(webapp/src/sidebar.ts) — the two sides are one contract and MUST match.")

(defconst agent-repl--sidebar-no-task-key "__no_task__"
  "Section key and fold key of the catch-all \"No task\" section.
Shaped like a task id without being one so the section renders through
the same group path; workspaces belonging to no task collect here.")

(defun agent-repl--sidebar-view-wire ()
  "Return the wire string for the active view, signalling on an unknown one."
  (or (alist-get agent-repl--sidebar-view agent-repl--sidebar-view-wire)
      (error "agent-repl--sidebar-view-wire: unknown view %S"
             agent-repl--sidebar-view)))

;;;; ---- The host-hook contract ------------------------------------------

(defconst agent-repl--sidebar-roster-hook "agentReplWorkspaceRoster"
  "Name of the webapp global that receives the workspace roster.
The webapp plants it on `window' at boot (`ROSTER_HOOK' in
webapp/src/host.ts) — the two names are one contract and MUST match.
The hook receives the roster as an already-parsed object: the push
script interpolates the JSON text directly into the call, and JSON is
a valid JS expression.")

(defconst agent-repl--sidebar-expand-hook "agentReplWorkspaceExpand"
  "Name of the webapp global that toggles a row's detail panel.
Planted on `window' at boot (`EXPAND_HOOK' in webapp/src/sidebar.ts) —
the two names are one contract and MUST match.  Receives one argument,
the row's canonical project dir, as a JSON string.  Backs `C-S-RET',
whose target panel is webapp-owned client state (`openDirs') the
roster never carries, so the keyboard reaches it through this hook
rather than a roster rebuild.")

(defconst agent-repl--sidebar-status-wire
  '((:thinking       . "thinking")
    (:permission     . "permission")
    (:init           . "init")
    (:done           . "done")
    (:ready          . "ready")
    (:idle           . "ready")
    (:idle-async     . "idle-async")
    (:vendor-blocked . "vendor-blocked")
    (:start-failed   . "start-failed")
    (:degraded       . "degraded")
    (:dead           . "dead")
    (:merging        . "merging")
    (:merge-queued   . "merge-queued")
    (:merge-conflict . "merge-conflict")
    (:merge-failed   . "merge-failed")
    (:merged         . "merged"))
  "Maps `agent-repl--ws-render-status' keywords onto sidebar wire strings.
The dots speak the SAME five-color vocabulary the tab-bar does, so a
workspace can never read one way in the rail and another in the tabs.
Nothing coarsens any more: `:idle-async' is its own yellow dot rather
than borrowing idle's ring, and `:vendor-blocked' is its own purple dot
rather than sharing the grey one with `:dead'.  A workspace that read
differently in the two places was exactly what the old coarsening
produced.

`:start-failed' and `:degraded' are mapped rather than absent: both are
real render states the daemon can push, and an unmapped one signals an
error instead of drawing a dot.

The value set is the webapp's closed WorkspaceRow.status union
\(webapp/src/sidebar.ts) — the two sides are one contract and MUST
stay in sync.  \"done-viewed\" and \"inactive\" are absent
here because neither is a render-status keyword —
`agent-repl--sidebar-wire-status' derives \"done-viewed\" from `:done' +
`agent-repl--ws-open-p'.")

(defun agent-repl--sidebar-wire-status (name)
  "Return the wire status string for known workspace NAME.
\"inactive\" when NAME has no open perspective (`agent-repl--ws-open-p'
nil): a workspace registered in the roster but absent from the tab-bar
has no live session whose lifecycle a status dot could report, so the
webapp draws a question mark in place of the dot.  This
perspective-membership check dominates every render state — a merged,
dead, or idle registration that is perspective-less still serializes
\"inactive\" — because being in the sidebar yet not the tab-bar is the
fact the row is conveying.

\"none\" when `agent-repl--ws-render-status' reports nil (tombstoned /
unborn).
Signals on an unmapped keyword: a render state missing from
`agent-repl--sidebar-status-wire' means a new state was added without
extending the sidebar contract — a violated invariant, never a silent
default dot."
  (if (not (agent-repl--ws-open-p name))
      "inactive"
    (let ((kw (agent-repl--ws-render-status name)))
      (cond
       ((null kw) "none")
       (t (or (alist-get kw agent-repl--sidebar-status-wire)
              (error "agent-repl--sidebar-wire-status: unmapped render state %S for ws=%s"
                     kw name)))))))

(defun agent-repl--sidebar-closed-p (name)
  "Return non-nil when live workspace NAME renders greyed (closed).
Closed means agent-repl is not actively hosting the workspace: either
no perspective is open for it (`agent-repl--ws-open-p' nil — e.g. a
merged-with-preserve-entry registration), or the perspective is open
but the REPL is torn down (`:repl-state' `:inactive', or `:hidden'
awaiting the hide-mode sweep).  A nil `:repl-state' on an OPEN
workspace is NOT closed: that is a session that never started, which
the \"none\" status dot already conveys."
  (or (not (agent-repl--ws-open-p name))
      (and (memq (agent-repl--ws-get name :repl-state) '(:inactive :hidden)) t)))

;;;; ---- The recently-merged window ---------------------------------------

(defconst agent-repl--sidebar-merged-key "__recently_merged__"
  "Fold key and roster key of the Recently Merged section.
Deliberately shaped like a repo key without being one: reusing the repo
fold machinery (`agent-repl--repo-folded-p') lets the section collapse
through the same click path and the same fold set the repo sections use,
so the webapp needs no second fold contract.")

(defcustom agent-repl-sidebar-merged-window-seconds (* 6 60 60)
  "Inactivity gap, in seconds, that wipes the Recently Merged section.
Once agent-repl observes no user activity for this long, every
already-merged workspace ages out of the section and then renders
nowhere in the sidebar.  Both flavors of inactivity count: Emacs sitting
idle, and Emacs not running at all."
  :type 'integer
  :group 'agent-repl)

(defvar agent-repl--sidebar-merged-epoch nil
  "Epoch seconds after which a completed merge still counts as recent.
`agent-repl--sidebar-refresh-merged-window' bumps it to now on every
observed inactivity gap, and that bump is what wipes the section.")

(defvar agent-repl--sidebar-last-activity nil
  "Epoch seconds of the last observed USER activity.
Persisted so the gap check spans restarts: a shutdown longer than
`agent-repl-sidebar-merged-window-seconds' must read as inactivity
exactly like an equally long idle stretch does.")

(defvar agent-repl--sidebar-merged-persisted-at 0
  "Epoch seconds of the last merged-window write, throttling disk churn.")

(defun agent-repl--sidebar-merged-state-file ()
  "Absolute path of the file persisting the recently-merged window."
  (agent-repl--global-state-file "sidebar-merged-window.el"))

(defun agent-repl--sidebar-load-merged-window ()
  "Hydrate the merged-window state from disk into memory."
  (let ((file (agent-repl--sidebar-merged-state-file)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((data (read (current-buffer))))
          (setq agent-repl--sidebar-last-activity (plist-get data :last-activity)
                agent-repl--sidebar-merged-epoch (plist-get data :epoch)))))))

(defun agent-repl--sidebar-save-merged-window ()
  "Write the merged-window state to disk."
  (let ((file (agent-repl--sidebar-merged-state-file)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (prin1 (list :last-activity agent-repl--sidebar-last-activity
                   :epoch agent-repl--sidebar-merged-epoch)
             (current-buffer)))))

(defun agent-repl--sidebar-refresh-merged-window ()
  "Advance the activity stamp, wiping the merged window across a gap.
Returns the current epoch.

The stamp is the moment of the last real user input, `now' minus
`current-idle-time', which is what makes ONE comparison cover both
flavors of inactivity.  While Emacs sits idle the stamp freezes at the
last keystroke, so the gap grows with the idle time.  Across a restart
the persisted stamp is still the pre-shutdown keystroke, so the gap is
caught on the first tick back.  A tick during ordinary use keeps the
stamp near `now', so the gap stays at zero.

The write is throttled: this runs at 1Hz and the state changes at human
timescales, so it hits disk only when the epoch actually moved or a
minute has passed."
  (unless agent-repl--sidebar-last-activity
    (agent-repl--sidebar-load-merged-window))
  (let* ((now (float-time))
         (idle (if (current-idle-time) (float-time (current-idle-time)) 0))
         (activity (- now idle))
         (last (or agent-repl--sidebar-last-activity activity))
         (wiped nil))
    (when (>= (- now last) agent-repl-sidebar-merged-window-seconds)
      (agent-repl--log nil "sidebar-merged-window: %.0fs gap >= %ds — wiping recently-merged"
                        (- now last) agent-repl-sidebar-merged-window-seconds)
      (setq agent-repl--sidebar-merged-epoch now
            wiped t))
    (setq agent-repl--sidebar-last-activity (max last activity))
    (when (or wiped (> (- now agent-repl--sidebar-merged-persisted-at) 60))
      (setq agent-repl--sidebar-merged-persisted-at now)
      (agent-repl--sidebar-save-merged-window))
    agent-repl--sidebar-merged-epoch))

(defun agent-repl--sidebar-merged-p (name)
  "Return non-nil when workspace NAME's render status is `:merged'."
  (eq (agent-repl--ws-render-status name) :merged))

(defun agent-repl--sidebar-merged-at (name)
  "Return NAME's merge-completion time as epoch seconds, or 0 when absent."
  (let ((at (agent-repl--ws-get name :merge-completed-at)))
    (cond ((null at) 0)
          ((numberp at) at)
          (t (float-time at)))))

(defun agent-repl--sidebar-recently-merged-p (name)
  "Return non-nil when merged NAME merged at or after the current epoch.
A nil epoch means no inactivity gap has been observed yet, so every
completed merge still counts as recent."
  (let ((at (agent-repl--sidebar-merged-at name)))
    (and (> at 0)
         (or (null agent-repl--sidebar-merged-epoch)
             (>= at agent-repl--sidebar-merged-epoch)))))

(defun agent-repl--sidebar-merged-sorted (entries)
  "Return the recently-merged ENTRIES, newest merge first."
  (sort (cl-remove-if-not (lambda (e) (agent-repl--sidebar-recently-merged-p (car e)))
                          (copy-sequence entries))
        (lambda (a b)
          (> (agent-repl--sidebar-merged-at (car a))
             (agent-repl--sidebar-merged-at (car b))))))

(defun agent-repl--sidebar-merged-group (entries current-name)
  "Return the Recently Merged group plist for ENTRIES, or nil when empty.
Shaped exactly like a repo group so the webapp validates and folds it
through the same path.  Merged rows carry no children: the family tree
is an organizing claim about live work, and a merged workspace has left
that tree."
  (let ((recent (agent-repl--sidebar-merged-sorted entries)))
    (when recent
      (list :key agent-repl--sidebar-merged-key
            :label "Recently Merged"
            :folded (if (agent-repl--repo-folded-p agent-repl--sidebar-merged-key)
                        t :false)
            ;; Uniform group shape: `:done' is meaningful only for task
            ;; sections, but every group carries it so the webapp's
            ;; validateGroup stays one code path.
            :done :false
            :rows (vconcat
                   (mapcar (lambda (e)
                             (agent-repl--sidebar-row-plist
                              (car e) (agent-repl--path-canonical (cdr e))
                              current-name []))
                           recent))))))

;;;; ---- Roster building --------------------------------------------------

(defvar agent-repl--sidebar-nav-dir nil
  "Canonical project dir of the keyboard cursor's row, or nil.
Serialized into the roster as `navDir'; the webapp draws the dashed
cursor ring on the matching row.  Emacs owns this state entirely —
see the Commentary for why keyboard selection needs no webapp
round-trip.")

(defvar agent-repl--sidebar-flat-dirs nil
  "Visible row dirs in sidebar render order, cached by the last build.
Depth-first over unfolded repos — exactly the order the webapp renders
rows in, so `C-S-n' / `C-S-p' walk what the user actually sees.  Rows
of folded repos are excluded (they are hidden); greyed closed-REPL
rows are included (they are visible and selectable).")

(defun agent-repl--sidebar-entries ()
  "Return the roster entries (NAME . DIR): live workspaces only.
The universe is `agent-repl--live-ws-names' — every registered,
non-tombstoned workspace, whether or not a perspective is open for it
\(perspective-less rows render greyed via
`agent-repl--sidebar-closed-p'; folds render as collapsed sections,
not drops).  Tombstoned registrations and snapshot-only history do
not render.  A live workspace without a `:project-dir' cannot be
keyed by dir; it is skipped with a log entry rather than silently,
since every normally-created workspace records one."
  (let (entries)
    (dolist (name (agent-repl--live-ws-names) (nreverse entries))
      (let ((dir (agent-repl--ws-get name :project-dir)))
        (if dir
            (push (cons name dir) entries)
          (agent-repl--log name "sidebar-entries: skipping live ws=%s with no :project-dir" name))))))

(defun agent-repl--sidebar-entry-created-at (name)
  "Return NAME's `:created-at' as a float for sibling ordering.
`most-positive-fixnum' when unrecorded, so undated rows sort after
dated ones rather than interleaving unpredictably."
  (let ((v (agent-repl--ws-get name :created-at)))
    (if v (float-time v) most-positive-fixnum)))

(defun agent-repl--sidebar-sibling-sort (nodes)
  "Sort sibling NODES (plists with :name) by creation time, then name.
Creation order IS the family chronology — a workspace cut earlier
lists earlier — and the name tiebreak keeps the order deterministic
for undated rows."
  (sort nodes
        (lambda (a b)
          (let ((ta (agent-repl--sidebar-entry-created-at (plist-get a :name)))
                (tb (agent-repl--sidebar-entry-created-at (plist-get b :name))))
            (if (= ta tb)
                (string< (plist-get a :name) (plist-get b :name))
              (< ta tb))))))

(defun agent-repl--sidebar-row-plist (name dir current-name children)
  "Serialize one roster row for open workspace NAME at canonical DIR.
CURRENT-NAME is the active workspace's name (computed once per build).
CHILDREN is the already-serialized vector of child rows.  Keys and
value shapes are the webapp's WorkspaceRow contract
\(webapp/src/sidebar.ts): JSON booleans (`t' / `:false'), `:null' for
absent optionals, epoch-seconds float for lastViewedAt."
  (let ((viewed (agent-repl--ws-get name :last-viewed-at)))
    (list :name name
          :dir dir
          :status (agent-repl--sidebar-wire-status name)
          :closed (if (agent-repl--sidebar-closed-p name) t :false)
          :current (if (equal name current-name) t :false)
          :lastViewedAt (if viewed (float-time viewed) :null)
          :mergedAt (let ((at (agent-repl--sidebar-merged-at name)))
                      (if (> at 0) at :null))
          :branch (or (agent-repl--ws-get name :branch-name) :null)
          :parentBranch (or (agent-repl--ws-get name :parent-branch-name) :null)
          :summary (or (agent-repl--ws-get name :last-prompt-summary) :null)
          :children children)))

(defun agent-repl--sidebar-build-sections (entries current key-fn label-fn done-fn key-sort extra-keys)
  "Group ENTRIES into folded sections; return (SECTIONS-VECTOR . FLAT-DIRS).
SECTIONS-VECTOR is a vector of `{:key :label :folded :done :rows}' group
plists; FLAT-DIRS the visible row dirs in render order.

KEY-FN maps a workspace NAME to its ROOT section key — a TOTAL function,
so every root lands in exactly one section.  LABEL-FN maps a key to its
display label, DONE-FN maps a key to a JSON boolean (`t' / `:false') for
the section's done state, and KEY-SORT orders the resulting key list.
EXTRA-KEYS lists keys that must render even with zero rows (an empty
task section for a brand-new task).

Family shape is view-independent and lives here: an entry whose
`:source-ws-dir' canonically matches another entry's project dir nests
under it; everything else roots in its own section via KEY-FN.  A child
renders under its parent regardless of how KEY-FN would classify the
child alone — the family line is the sidebar's organizing claim — so
the section a family renders in is its ROOT's key.  Siblings sort by
creation time (`agent-repl--sidebar-sibling-sort').

EMITTED counts every serialized row; a count short of the entry total
means a `:source-ws-dir' cycle orphaned a family from every root — rows
silently missing from the sidebar would be a debugging tarpit, so fail
loudly instead."
  (let* ((nodes (mapcar (lambda (e)
                          (list :name (car e)
                                :dir (agent-repl--path-canonical (cdr e))))
                        entries))
         (by-dir (make-hash-table :test 'equal))
         (children (make-hash-table :test 'equal))
         (roots-by-key (make-hash-table :test 'equal))
         (emitted 0)
         keys)
    (dolist (n nodes) (puthash (plist-get n :dir) n by-dir))
    ;; Attach each node under its parent when the parent is itself an
    ;; entry; self-parenting is a corrupted plist, not a family.
    (dolist (n nodes)
      (let* ((name (plist-get n :name))
             (dir (plist-get n :dir))
             (src (agent-repl--ws-get name :source-ws-dir))
             (parent-dir (and src (agent-repl--path-canonical src))))
        (when (equal parent-dir dir)
          (error "agent-repl--sidebar-build-sections: ws=%s is its own parent (dir=%s)"
                 name dir))
        (if (and parent-dir (gethash parent-dir by-dir))
            (push n (gethash parent-dir children))
          (let ((key (funcall key-fn name)))
            (unless (gethash key roots-by-key)
              (push key keys))
            (push n (gethash key roots-by-key))))))
    ;; A key that must render even empty (e.g. a fresh task) joins the
    ;; key set with a nil root list, so it becomes a zero-row section
    ;; without disturbing the emitted-vs-node-count invariant.
    (dolist (key extra-keys)
      (unless (gethash key roots-by-key)
        (push key keys)
        (puthash key nil roots-by-key)))
    (let* ((flat nil)
           (emit-node nil)
           (sections
            (vconcat
             (mapcar
              (lambda (key)
                (let ((folded (and (agent-repl--repo-folded-p key) t)))
                  (setq emit-node
                        (lambda (node)
                          (setq emitted (1+ emitted))
                          (let* ((dir (plist-get node :dir))
                                 (kids (agent-repl--sidebar-sibling-sort
                                        (gethash dir children))))
                            (unless folded (push dir flat))
                            (agent-repl--sidebar-row-plist
                             (plist-get node :name) dir current
                             (vconcat (mapcar (lambda (k) (funcall emit-node k))
                                              kids))))))
                  (list :key key
                        :label (funcall label-fn key)
                        :folded (if folded t :false)
                        :done (funcall done-fn key)
                        :rows (vconcat
                               (mapcar (lambda (n) (funcall emit-node n))
                                       (agent-repl--sidebar-sibling-sort
                                        (gethash key roots-by-key)))))))
              (funcall key-sort keys)))))
      (unless (= emitted (length nodes))
        (error "agent-repl--sidebar-build-sections: emitted %d of %d rows — :source-ws-dir cycle orphaned a family"
               emitted (length nodes)))
      (cons sections (nreverse flat)))))

(defun agent-repl--sidebar-repo-key-sort (keys)
  "Sort repo KEYS by label with the `(no repo)' sentinel forced last."
  (sort (copy-sequence keys)
        (lambda (a b)
          (cond
           ((equal a agent-repl--repo-key-unknown) nil)
           ((equal b agent-repl--repo-key-unknown) t)
           (t (string< (agent-repl--repo-label a)
                       (agent-repl--repo-label b)))))))

(defun agent-repl--sidebar-task-key (name)
  "Return the section key for workspace NAME under the task view.
Its effective task id (direct or inherited), or the no-task catch-all."
  (or (agent-repl--ws-effective-task-id name)
      agent-repl--sidebar-no-task-key))

(defun agent-repl--sidebar-task-label (key)
  "Return the task-section label for KEY.
\"No task\" for the catch-all; otherwise the task's title, falling back
to the bare id only when the task somehow no longer exists."
  (if (equal key agent-repl--sidebar-no-task-key)
      "No task"
    (let ((task (agent-repl--task-get key)))
      (if task (plist-get task :title) key))))

(defun agent-repl--sidebar-task-done (key)
  "Return the JSON boolean done state for task-section KEY.
`:false' for the catch-all and for any task not marked done."
  (if (equal key agent-repl--sidebar-no-task-key)
      :false
    (let ((task (agent-repl--task-get key)))
      (if (and task (plist-get task :done)) t :false))))

(defun agent-repl--sidebar-task-key-sort (keys)
  "Sort task-section KEYS by task creation order, the catch-all last."
  (let ((order (make-hash-table :test 'equal))
        (i 0))
    (dolist (task (agent-repl--tasks-sorted))
      (puthash (plist-get task :id) i order)
      (setq i (1+ i)))
    (sort (copy-sequence keys)
          (lambda (a b)
            (cond
             ((equal a agent-repl--sidebar-no-task-key) nil)
             ((equal b agent-repl--sidebar-no-task-key) t)
             (t (< (gethash a order most-positive-fixnum)
                   (gethash b order most-positive-fixnum))))))))

(defun agent-repl--sidebar-build ()
  "Build the roster from every live workspace, per the active view.
Returns (ROSTER . FLAT-DIRS): ROSTER is the `json-serialize'-ready
plist, FLAT-DIRS the visible row dirs in render order (see
`agent-repl--sidebar-flat-dirs').

The roster always carries both `:repos' and `:tasks'; the inactive
view's array is empty, so the webapp switches on `:view' and renders
exactly one grouping.  Repository view groups by git repo (repos sort by
label, `(no repo)' last); task view groups by the task each family's
ROOT belongs to (tasks in creation order, \"No task\" last) and always
renders every task, empty ones included.

Merged workspaces are partitioned out BEFORE the sections are built, so
they never enter the active grouping and the emitted-vs-node-count
invariant compares two exact counts.  They render instead in the
`:recentlyMerged' group while they sit inside the window
`agent-repl--sidebar-refresh-merged-window' maintains."
  (let* ((all-entries (agent-repl--sidebar-entries))
         (merged-entries (cl-remove-if-not
                          (lambda (e) (agent-repl--sidebar-merged-p (car e)))
                          all-entries))
         (entries (cl-remove-if
                   (lambda (e) (agent-repl--sidebar-merged-p (car e)))
                   all-entries))
         (current (ignore-errors (agent-repl--ws-current-name)))
         (task-view (eq agent-repl--sidebar-view :task))
         (built (if task-view
                    (agent-repl--sidebar-build-sections
                     entries current
                     #'agent-repl--sidebar-task-key
                     #'agent-repl--sidebar-task-label
                     #'agent-repl--sidebar-task-done
                     #'agent-repl--sidebar-task-key-sort
                     (mapcar (lambda (task) (plist-get task :id))
                             (agent-repl--tasks-sorted)))
                  (agent-repl--sidebar-build-sections
                   entries current
                   #'agent-repl--ws-repo-group
                   #'agent-repl--repo-label
                   (lambda (_key) :false)
                   #'agent-repl--sidebar-repo-key-sort
                   nil)))
         (sections (car built))
         (flat (cdr built))
         (recent (agent-repl--sidebar-merged-group merged-entries current))
         (recent-folded (agent-repl--repo-folded-p agent-repl--sidebar-merged-key))
         (recent-dirs (and recent (not recent-folded)
                           (mapcar (lambda (e)
                                     (agent-repl--path-canonical (cdr e)))
                                   (agent-repl--sidebar-merged-sorted
                                    merged-entries)))))
    (cons (list :view (agent-repl--sidebar-view-wire)
                :repos (if task-view [] sections)
                :tasks (if task-view sections [])
                :recentlyMerged (or recent :null)
                :navDir (or agent-repl--sidebar-nav-dir :null))
          (append flat recent-dirs))))

;;;; ---- Pushing into the webviews ----------------------------------------

(defun agent-repl--sidebar-push-script (json)
  "Return the JavaScript that hands JSON (the roster text) to the webapp.
Calls the hook only when the webapp has already planted it: a webview
mid-navigation has no hook yet, and that is an expected state rather
than a violated invariant — the next pushed change (or the signature
tick noticing the webview) re-delivers the roster."
  (format "window.%s && window.%s(%s);"
          agent-repl--sidebar-roster-hook
          agent-repl--sidebar-roster-hook
          json))

(defun agent-repl--sidebar-expand-script (dir)
  "Return the JavaScript that toggles DIR's detail panel in the webapp.
Guards on the hook exactly as `agent-repl--sidebar-push-script' does —
a webview mid-navigation has not planted it yet.  DIR rides in as a
JSON string literal (`json-serialize'), so any character in a path
survives interpolation intact."
  (format "window.%s && window.%s(%s);"
          agent-repl--sidebar-expand-hook
          agent-repl--sidebar-expand-hook
          (json-serialize dir)))

(defun agent-repl--sidebar-live-webview-buffers ()
  "Return every live frontend webview buffer across live workspaces."
  (let (bufs)
    (dolist (name (agent-repl--live-ws-names) (nreverse bufs))
      (let ((buf (agent-repl--ws-get name :frontend-buffer)))
        (when (buffer-live-p buf)
          (push buf bufs))))))

(defun agent-repl--sidebar-push ()
  "Rebuild the roster and push it into every live webview.
Unconditional: change-gating lives in `agent-repl--sidebar-tick's
signature compare, and the event-driven callers (fold / switch / nav)
push precisely because they just changed what the sidebar shows.
Also refreshes `agent-repl--sidebar-flat-dirs' as the build's side
product, so navigation always walks the order last shown."
  (let* ((built (agent-repl--sidebar-build))
         (json (json-serialize (car built)))
         (bufs (agent-repl--sidebar-live-webview-buffers))
         (script (agent-repl--sidebar-push-script json)))
    (setq agent-repl--sidebar-flat-dirs (cdr built))
    (dolist (buf bufs)
      (agent-repl--frontend-webview-execute-script buf script))
    (agent-repl--log nil "sidebar-push: rows=%d webviews=%d nav=%s json-bytes=%d"
                      (length (cdr built)) (length bufs)
                      agent-repl--sidebar-nav-dir (length json))))

(defun agent-repl--sidebar-expand-push (dir)
  "Toggle DIR's detail panel across every live webview.
The panel is webapp-owned client state (`openDirs'), so unlike a fold
this fires the expand hook WITHOUT rebuilding the roster — the page
flips the panel and re-renders on its own.  No roster round-trip means
`agent-repl--sidebar-flat-dirs' is left untouched here."
  (let ((script (agent-repl--sidebar-expand-script dir))
        (bufs (agent-repl--sidebar-live-webview-buffers)))
    (dolist (buf bufs)
      (agent-repl--frontend-webview-execute-script buf script))
    (agent-repl--log nil "sidebar-expand-push: dir=%s webviews=%d" dir (length bufs))))

;;;; ---- The 1Hz change gate ----------------------------------------------

(defvar agent-repl--sidebar-last-signature nil
  "Signature of the roster state as of the last pushed build.")

(defun agent-repl--sidebar-signature ()
  "Return a cheap value that changes whenever the roster would.
Pure in-memory reads plus the persp membership probe:
per-live-workspace render state (status keyword,
repl-state, open-p — the greyed flag's other input, last-viewed), the
current workspace, the fold set, the nav cursor, and the set of live
webview buffers.  The webview set matters because a freshly
\(re)mounted webview needs the roster even when nothing in the roster
itself changed."
  (list (mapcar (lambda (name)
                  (list name
                        (agent-repl--ws-render-status name)
                        (agent-repl--ws-get name :repl-state)
                        (and (agent-repl--ws-open-p name) t)
                        (agent-repl--ws-get name :last-viewed-at)
                        ;; Task membership: assigning a workspace to a
                        ;; task changes which section it renders under
                        ;; without touching any lifecycle field above.
                        (agent-repl--ws-get name :task-id)))
                (agent-repl--live-ws-names))
        (ignore-errors (agent-repl--ws-current-name))
        (agent-repl--folded-repo-keys)
        agent-repl--sidebar-nav-dir
        ;; The active view and the task list are their own axes: a view
        ;; switch, task create, rename, or done-toggle changes the
        ;; roster without moving any per-workspace lifecycle field.
        agent-repl--sidebar-view
        (agent-repl--tasks-signature)
        ;; The epoch is its own axis: wiping the Recently Merged section
        ;; drops rows without changing any per-workspace render state, so
        ;; without this the gate would skip the push that clears them.
        agent-repl--sidebar-merged-epoch
        (mapcar #'buffer-name (agent-repl--sidebar-live-webview-buffers))))

(defun agent-repl--sidebar-tick ()
  "1Hz entry point (status.el's state tick): push when the signature moved.
The signature compare is the hot-path gate — the rebuild and push
behind it run only on actual change, so per-tick logging stays on the
verbose ladder.

The merged-window refresh runs BEFORE the signature is taken, since the
epoch it maintains is one of the signature's inputs and a wipe must be
visible to this very tick's compare rather than the next one."
  (agent-repl--sidebar-refresh-merged-window)
  (let ((sig (agent-repl--sidebar-signature)))
    (if (equal sig agent-repl--sidebar-last-signature)
        (agent-repl--log-verbose nil "sidebar-tick: signature unchanged, skip")
      (setq agent-repl--sidebar-last-signature sig)
      (agent-repl--sidebar-push))))

;;;; ---- The dot and the tab take the same value at the same moment --------
;;
;; The rail's dots and the tab-bar's colors are two renderings of ONE value:
;; `agent-repl--ws-render-status', which is the render keyword the daemon
;; pushed.  What used to differ was WHEN each read it.  The tab bar recolors
;; the instant `frontend-state.el' stores the pushed state, while the rail
;; waited for the 1Hz tick's signature compare to notice — so for up to a
;; second the two surfaces showed different colors for the same workspace.
;; That is the green-tab-beside-a-yellow-dot divergence, on the Emacs side of
;; it.
;;
;; Pushing the roster FROM the state-transition hook removes the window rather
;; than shrinking it: the rail is rebuilt as part of applying the state, from
;; the same value the tab bar is about to read, so there is no interval in
;; which one surface has the new state and the other has the old one.
;;
;; The 1Hz tick stays for the axes no pushed state moves — folds, task
;; membership, the merged-window epoch, a freshly mounted webview — and its
;; signature compare makes the overlap a no-op rather than a double push.

(defun agent-repl--sidebar-react-to-pushed-state (ws new previous)
  "Repaint the rail as part of applying WS's pushed render state.
Subscriber for `agent-repl-ws-state-transition-functions'
\(frontend-state.el).  NEW and PREVIOUS are the render keywords; a push
that did not move the keyword changes no dot, so it is skipped.

The push also refreshes `agent-repl--sidebar-last-signature', so the
next 1Hz tick sees the state it already delivered and stays quiet."
  (unless (eq new previous)
    (agent-repl--log ws "sidebar-react-to-pushed-state: ws=%s %s -> %s — repainting the rail with the tab bar"
                     ws previous new)
    (setq agent-repl--sidebar-last-signature (agent-repl--sidebar-signature))
    (agent-repl--sidebar-push)))

;; Registered here though the hook variable is defined in frontend-state.el:
;; `add-hook' auto-vivifies the unbound variable and that file's `defvar ...
;; nil' does not reset an already-bound one, so this survives either load
;; order (the same arrangement status.el's death reactor uses).
(add-hook 'agent-repl-ws-state-transition-functions
          #'agent-repl--sidebar-react-to-pushed-state)

;;;; ---- Keyboard navigation ----------------------------------------------

(defun agent-repl--sidebar-visible-dirs ()
  "Return the visible row dirs, building the roster when never built.
The build's flat list is the navigation order; an empty cache before
any push simply means no tick has run yet, so build once rather than
navigate a stale nothing."
  (unless agent-repl--sidebar-flat-dirs
    (setq agent-repl--sidebar-flat-dirs (cdr (agent-repl--sidebar-build))))
  agent-repl--sidebar-flat-dirs)

(defun agent-repl--sidebar-nav-move (delta)
  "Move the keyboard cursor DELTA rows (wrapping) and open where it lands.
With no cursor yet, DELTA > 0 starts at the first visible row and
DELTA < 0 at the last — the two ends a user reaching for next/prev
expects to land on.  Opening the landed workspace IS the auto-select:
`C-S-n' / `C-S-p' both move the highlight and switch to it, so the
cursor is never a mere preview.  `agent-repl--sidebar-open-dir' pushes
the fresh roster, so the moved highlight rides that same push and no
separate highlight push is needed."
  (let ((dirs (agent-repl--sidebar-visible-dirs)))
    (unless dirs
      (user-error "agent-repl sidebar: no workspaces to navigate"))
    (let* ((cur (and agent-repl--sidebar-nav-dir
                     (cl-position agent-repl--sidebar-nav-dir dirs :test #'equal)))
           (next (if (null cur)
                     (if (> delta 0) 0 (1- (length dirs)))
                   (mod (+ cur delta) (length dirs)))))
      (setq agent-repl--sidebar-nav-dir (nth next dirs))
      (agent-repl--log nil "sidebar-nav-move: delta=%d idx=%s -> dir=%s of %d"
                        delta cur agent-repl--sidebar-nav-dir (length dirs))
      (agent-repl--sidebar-open-dir agent-repl--sidebar-nav-dir))))

(defun agent-repl-sidebar-nav-next ()
  "Move the sidebar's keyboard cursor to the next row and open it."
  (interactive)
  (agent-repl--sidebar-nav-move 1))

(defun agent-repl-sidebar-nav-prev ()
  "Move the sidebar's keyboard cursor to the previous row and open it."
  (interactive)
  (agent-repl--sidebar-nav-move -1))

(defun agent-repl-sidebar-nav-show-info ()
  "Unfold the detail panel of the workspace under the sidebar's cursor.
Toggles the same per-row panel the chevron click drives, so a second
press folds it back.  Emacs owns the cursor but the panel is
webapp-owned client state, so this reaches the page through
`agent-repl--sidebar-expand-push' rather than a roster rebuild."
  (interactive)
  (let ((dir agent-repl--sidebar-nav-dir))
    (unless dir
      (user-error "agent-repl sidebar: no row selected — C-S-n / C-S-p first"))
    (agent-repl--sidebar-expand-push dir)))

;;;; ---- Opening a workspace (shared by keyboard + click) ------------------

(defun agent-repl--sidebar-entry-for-dir (dir)
  "Return the live entry (NAME . DIR) canonically matching DIR, or nil."
  (let ((canon (agent-repl--path-canonical dir)))
    (cl-find-if (lambda (e)
                  (equal (agent-repl--path-canonical (cdr e)) canon))
                (agent-repl--sidebar-entries))))

(defun agent-repl--sidebar-open-dir (dir)
  "Switch Emacs to the live workspace whose project dir is DIR, and open it.
Routes through `agent-repl--picker-open-selection', the canonical
switch-or-revive: an open perspective switches in place, a
perspective-less or closed-REPL one is re-established.  Signals when
DIR matches no live entry — an unknown dir means the click/cursor and
the roster disagree, which is a contract violation to surface, not a
row to silently ignore.

Then boots the agent via `agent-repl--frontend-boot-session'.  Opening
from the sidebar means \"open the workspace AND start agent-repl in
it\", but `--picker-open-selection' boots a session only on its revive
branch: a workspace whose perspective is still open while its REPL was
torn down (`:repl-state' `:inactive' / `:hidden', the greyed rows
`agent-repl--sidebar-closed-p' marks) takes the switch branch and would
otherwise land the user in a dead REPL.  The boot door no-ops when a
live session already exists, so the revive branch's own boot is never
doubled.

Booting alone leaves the session running but HEADLESS — a closed target
\(`agent-repl--sidebar-closed-p') would land the user in bare windows
with the agent panels hidden.  So its `:pending-show-panels' flag is
armed BEFORE the switch, and the switch's own
`agent-repl--on-workspace-switch' drain then shows the panels the
instant the workspace activates — the same door workspace generation
comes up visible through.  This is the keyboard / click equivalent of
pressing `SPC o c' in the target while it is closed.  An already-open
target needs no arming: `agent-repl--ensure-own-panels-on-persp-switch'
restores its panels on switch."
  (let ((entry (agent-repl--sidebar-entry-for-dir dir)))
    (unless entry
      (error "agent-repl sidebar: no live workspace for dir %s" dir))
    (let* ((name (car entry))
           (closed (agent-repl--sidebar-closed-p name)))
      (agent-repl--log name "sidebar-open: name=%s dir=%s live=%s closed=%s merged=%s"
                        name (cdr entry)
                        (and (agent-repl--ws-live-p name) t)
                        closed
                        (or (eq (agent-repl--ws-get name :repl-state) :merged)
                            (eq (agent-repl--ws-get name :merge-completed) t)))
      ;; Arm the panel show BEFORE switching so a closed target comes up
      ;; running AND open rather than running-but-headless.  Drained by
      ;; `agent-repl--on-workspace-switch' once the workspace activates.
      (when closed
        (agent-repl--ws-put name :pending-show-panels t))
      (agent-repl--picker-open-selection
       (list :name name
             :project-dir (cdr entry)
             :live-p (agent-repl--ws-live-p name)))
      (agent-repl--frontend-boot-session name (cdr entry))
      (agent-repl--sidebar-push))))

;;;; ---- Workspace-command handlers (webview clicks, via the daemon) -------

(defun agent-repl--handle-switch-command (cmd)
  "Handle a \"switch\" workspace command CMD (a sidebar row click).
CMD carries `dir', the clicked row's canonical project dir (the
daemon's POST /workspace-command validated presence, but this handler
re-validates — the file channel is also writable by other emitters)."
  (let ((dir (alist-get 'dir cmd)))
    (unless (and (stringp dir) (not (string-empty-p dir)))
      (error "agent-repl switch command: missing dir in %S" cmd))
    (agent-repl--log nil "workspace-commands-file switch: dir=%s" dir)
    (agent-repl--sidebar-open-dir dir)))

(defun agent-repl--sidebar-repo-key-known-p (key)
  "Return non-nil when KEY names a repo some live workspace belongs to.
The `(no repo)' sentinel counts — its section folds like any other."
  (or (equal key agent-repl--repo-key-unknown)
      (cl-some (lambda (e) (equal key (agent-repl--ws-repo-group (car e))))
               (agent-repl--sidebar-entries))))

(defun agent-repl--handle-fold-command (cmd)
  "Handle a \"fold\" workspace command CMD (a sidebar repo-header click).
CMD carries `repo_key' and the DESIRED `folded' state (explicit target
rather than a toggle, so a duplicate or delayed file cannot double-
toggle past the user's intent).  Folding a repo hides its tab-bar
entries too — `agent-repl--ws-tabline-names' already filters on the
same fold set, so forcing the tab-bar redraw is the whole sync."
  (let ((key (alist-get 'repo_key cmd))
        (folded-cell (assq 'folded cmd)))
    (unless (and (stringp key) (not (string-empty-p key)))
      (error "agent-repl fold command: missing repo_key in %S" cmd))
    (unless folded-cell
      (error "agent-repl fold command: missing folded in %S" cmd))
    (unless (agent-repl--sidebar-repo-key-known-p key)
      (error "agent-repl fold command: unknown repo key %s" key))
    (let ((desired (eq (cdr folded-cell) t)))
      (unless (eq desired (agent-repl--repo-folded-p key))
        (agent-repl--toggle-repo-fold key))
      (agent-repl--log nil "workspace-commands-file fold: key=%s folded=%s" key desired)
      (agent-repl--force-tab-bar-redraw)
      (agent-repl--sidebar-push))))

;;;; ---- View + task command handlers (webview clicks, via the daemon) -----

(defun agent-repl--sidebar-set-view (view)
  "Set the active sidebar VIEW (`:repository' / `:task') and push the roster.
Signals on an unknown VIEW rather than defaulting — the caller is the
webapp's view selector, whose value set is the same contract."
  (unless (memq view '(:repository :task))
    (error "agent-repl--sidebar-set-view: unknown view %S" view))
  (setq agent-repl--sidebar-view view)
  (agent-repl--log nil "sidebar-set-view: view=%s" view)
  (agent-repl--sidebar-push))

(defun agent-repl--handle-set-view-command (cmd)
  "Handle a \"set-view\" command CMD (the sidebar's view selector).
CMD carries `view', one of the wire strings in
`agent-repl--sidebar-view-wire'."
  (let ((view (alist-get 'view cmd)))
    (unless (member view '("repository" "task"))
      (error "agent-repl set-view command: bad view in %S" cmd))
    (agent-repl--log nil "workspace-commands-file set-view: view=%s" view)
    (agent-repl--sidebar-set-view (if (equal view "task") :task :repository))))

(defun agent-repl--handle-task-create-command (_cmd)
  "Handle a \"task-create\" command (the Task view's + button).
Prompts for the title in the minibuffer — Emacs owns the task model, so
the webapp asks Emacs to create rather than carrying a title itself.  An
empty title is a cancelled prompt and creates nothing."
  (let ((title (read-string "Task title: ")))
    (if (string-empty-p (string-trim title))
        (agent-repl--log nil "workspace-commands-file task-create: empty title, skipping")
      (agent-repl--task-create title)
      (agent-repl--sidebar-push))))

(defun agent-repl--handle-task-toggle-done-command (cmd)
  "Handle a \"task-toggle-done\" command CMD (a task checkbox click).
CMD carries the task `id'."
  (let ((id (alist-get 'id cmd)))
    (unless (and (stringp id) (not (string-empty-p id)))
      (error "agent-repl task-toggle-done command: missing id in %S" cmd))
    (agent-repl--log nil "workspace-commands-file task-toggle-done: id=%s" id)
    (agent-repl--task-toggle-done id)
    (agent-repl--sidebar-push)))

(defun agent-repl--handle-task-open-command (cmd)
  "Handle a \"task-open\" command CMD (a task label click).
Opens the task's org notes file in a popup (`agent-repl--task-open')."
  (let ((id (alist-get 'id cmd)))
    (unless (and (stringp id) (not (string-empty-p id)))
      (error "agent-repl task-open command: missing id in %S" cmd))
    (agent-repl--log nil "workspace-commands-file task-open: id=%s" id)
    (agent-repl--task-open id)))

(defun agent-repl--task-add-workspace-interactive (id)
  "Prompt for a live workspace and assign it to task ID.
Offers every live workspace not already effectively in this task; the
chosen workspace's `:task-id' is set so it and its future children
render under the task.  A workspace already inheriting the task through
its parent is omitted — it needs no assignment.  A brand-new workspace
for a task is made the ordinary way (or cut as a child of one already in
the task, which inherits it), then added here."
  (unless (agent-repl--task-get id)
    (error "agent-repl--task-add-workspace-interactive: unknown task %s" id))
  (let* ((candidates
          (cl-remove-if
           (lambda (e) (equal (agent-repl--ws-effective-task-id (car e)) id))
           (agent-repl--sidebar-entries)))
         (names (mapcar #'car candidates)))
    (if (null names)
        (message "agent-repl: no workspaces available to add to this task")
      (let ((name (completing-read "Add workspace to task: " names nil t)))
        (when (and name (not (string-empty-p name)))
          (agent-repl--task-assign-workspace name id)
          (agent-repl--sidebar-push))))))

(defun agent-repl--handle-task-add-workspace-command (cmd)
  "Handle a \"task-add-workspace\" command CMD (a task section + button).
CMD carries the task `id'; the workspace is chosen in the minibuffer."
  (let ((id (alist-get 'id cmd)))
    (unless (and (stringp id) (not (string-empty-p id)))
      (error "agent-repl task-add-workspace command: missing id in %S" cmd))
    (agent-repl--log nil "workspace-commands-file task-add-workspace: id=%s" id)
    (agent-repl--task-add-workspace-interactive id)))

(provide 'agent-repl-sidebar)
;;; sidebar.el ends here
