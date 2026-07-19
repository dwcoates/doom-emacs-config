;;; sidebar.el --- Workspaces sidebar: roster feed + actions -*- lexical-binding: t; -*-

;;; Commentary:

;; Feeds the webapp's workspaces sidebar and services its actions.
;;
;; Data flows one way, Emacs -> webview: Emacs owns the workspace model
;; (workspace.el), so it builds the roster — repos, family-nested rows,
;; per-row lifecycle status — serializes it to JSON, and pushes it into
;; every live webview through the `agentReplWorkspaceRoster' host hook
;; over the execute-script channel frontend.el owns.  The push is gated
;; by a cheap in-memory signature computed on the 1Hz state tick
;; (status.el), so the rebuild (which stats/reads the snapshot file and
;; may resolve repo keys) runs only when something visible changed.
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
;; roster push as `navDir', and `C-S-RET' opens the cursor's workspace
;; directly — no webapp round-trip involved.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--frontend-webview-execute-script "frontend" (buf script))
(declare-function agent-repl--known-workspace-entries "commands" ())
(declare-function agent-repl--picker-open-selection "commands" (payload))
(declare-function agent-repl--workspace-snapshot-file-for-read "commands" ())
(declare-function agent-repl--force-tab-bar-redraw "status" ())

;;;; ---- The host-hook contract ------------------------------------------

(defconst agent-repl--sidebar-roster-hook "agentReplWorkspaceRoster"
  "Name of the webapp global that receives the workspace roster.
The webapp plants it on `window' at boot (`ROSTER_HOOK' in
webapp/src/host.ts) — the two names are one contract and MUST match.
The hook receives the roster as an already-parsed object: the push
script interpolates the JSON text directly into the call, and JSON is
a valid JS expression.")

(defconst agent-repl--sidebar-status-wire
  '((:thinking       . "thinking")
    (:permission     . "permission")
    (:init           . "init")
    (:done           . "done")
    (:idle           . "idle")
    (:idle-async     . "idle")
    (:stop-failed    . "dead")
    (:dead           . "dead")
    (:merging        . "merging")
    (:merge-queued   . "merge-queued")
    (:merge-conflict . "merge-conflict")
    (:merge-failed   . "merge-failed")
    (:merged         . "merged"))
  "Maps `agent-repl--ws-render-status' keywords onto sidebar wire strings.
The value set is the webapp's closed WorkspaceRow.status union
\(webapp/src/sidebar.ts) — the two sides are one contract and MUST
stay in sync.  Two states coarsen deliberately because the sidebar's
dot palette draws no distinct visual for them: `:idle-async' rides the
idle hollow ring (the topbar already carries the async signal), and
`:stop-failed' rides the dead grey dot (both mean \"session needs
attention, not running\").  \"done-viewed\" is absent here because it
is not a render-status keyword — `agent-repl--sidebar-wire-status'
derives it from `:done' + `:done-acked'.")

(defun agent-repl--sidebar-wire-status (name)
  "Return the wire status string for known workspace NAME.
\"done-viewed\" when the :done state has been acked (`:done-acked' —
the same viewed semantics the tab-bar renders); \"none\" when
`agent-repl--ws-render-status' reports nil (tombstoned / unborn).
Signals on an unmapped keyword: a render state missing from
`agent-repl--sidebar-status-wire' means a new state was added without
extending the sidebar contract — a violated invariant, never a silent
default dot."
  (let ((kw (agent-repl--ws-render-status name)))
    (cond
     ((null kw) "none")
     ((and (eq kw :done) (agent-repl--ws-get name :done-acked)) "done-viewed")
     (t (or (alist-get kw agent-repl--sidebar-status-wire)
            (error "agent-repl--sidebar-wire-status: unmapped render state %S for ws=%s"
                   kw name))))))

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
of folded repos are excluded (they are hidden), closed rows are
included (they are visible and selectable).")

(defvar agent-repl--sidebar-dir-repo-key-cache (make-hash-table :test 'equal)
  "Memo: canonical dir -> repo key (or `none') for snapshot-only entries.
Snapshot-only entries have no workspace plist to carry `:group-key',
so without this memo every rebuild would shell git once per such
entry.  Failed resolutions are cached as `none' on the same grounds —
a dir that is not a repo does not become one, and re-shelling git for
it every rebuild would dwarf the cost the memo exists to avoid.")

(defun agent-repl--sidebar-repo-key-for-entry (name dir)
  "Return the repo key for roster entry NAME at DIR — total, never nil.
Known workspaces resolve through `agent-repl--ws-repo-group' (which
caches on the plist); snapshot-only entries resolve DIR through
`agent-repl--repo-key-for-dir', memoized in
`agent-repl--sidebar-dir-repo-key-cache'.  Unresolvable dirs map onto
`agent-repl--repo-key-unknown', the same sentinel contract
`agent-repl--ws-repo-group' documents."
  (if (agent-repl--ws-known-p name)
      (agent-repl--ws-repo-group name)
    (let ((cached (gethash dir agent-repl--sidebar-dir-repo-key-cache)))
      (cond
       ((eq cached 'none) agent-repl--repo-key-unknown)
       (cached cached)
       (t (let ((key (agent-repl--repo-key-for-dir dir)))
            (puthash dir (or key 'none) agent-repl--sidebar-dir-repo-key-cache)
            (or key agent-repl--repo-key-unknown)))))))

(defun agent-repl--sidebar-entry-created-at (name)
  "Return NAME's `:created-at' as a float for sibling ordering.
`most-positive-fixnum' when unknown (snapshot-only entries and rows
that never recorded creation), so undated rows sort after dated ones
rather than interleaving unpredictably."
  (let ((v (and (agent-repl--ws-known-p name)
                (agent-repl--ws-get name :created-at))))
    (if v (float-time v) most-positive-fixnum)))

(defun agent-repl--sidebar-sibling-sort (nodes)
  "Sort sibling NODES (plists with :name) by creation time, then name.
Creation order IS the family chronology — a workspace cut earlier
lists earlier — and the name tiebreak keeps the order deterministic
for undated snapshot-only rows."
  (sort nodes
        (lambda (a b)
          (let ((ta (agent-repl--sidebar-entry-created-at (plist-get a :name)))
                (tb (agent-repl--sidebar-entry-created-at (plist-get b :name))))
            (if (= ta tb)
                (string< (plist-get a :name) (plist-get b :name))
              (< ta tb))))))

(defun agent-repl--sidebar-row-plist (name dir current-name children)
  "Serialize one roster row for NAME at canonical DIR.
CURRENT-NAME is the active workspace's name (computed once per build).
CHILDREN is the already-serialized vector of child rows.  Keys and
value shapes are the webapp's WorkspaceRow contract
\(webapp/src/sidebar.ts): JSON booleans (`t' / `:false'), `:null' for
absent optionals, epoch-seconds float for lastViewedAt."
  (let* ((known (agent-repl--ws-known-p name))
         (viewed (and known (agent-repl--ws-get name :last-viewed-at))))
    (list :name name
          :dir dir
          :status (if known (agent-repl--sidebar-wire-status name) "none")
          :closed (if (and known (agent-repl--ws-open-p name)) :false t)
          :current (if (equal name current-name) t :false)
          :lastViewedAt (if viewed (float-time viewed) :null)
          :branch (or (and known (agent-repl--ws-get name :branch-name)) :null)
          :parentBranch (or (and known (agent-repl--ws-get name :parent-branch-name)) :null)
          :summary (or (and known (agent-repl--ws-get name :last-prompt-summary)) :null)
          :children children)))

(defun agent-repl--sidebar-build ()
  "Build the roster from every known workspace entry.
Returns (ROSTER . FLAT-DIRS): ROSTER is the `json-serialize'-ready
plist, FLAT-DIRS the visible row dirs in render order (see
`agent-repl--sidebar-flat-dirs').

Family shape: an entry whose `:source-ws-dir' canonically matches
another entry's project dir nests under it; everything else roots in
its repo's section.  A child renders under its parent even if git
resolves their repo keys differently (children are worktrees cut from
the parent, and the family line is the sidebar's organizing claim) —
the section a family renders in is its ROOT's repo.  Repos sort by
label with the `(no repo)' sentinel last; siblings sort by creation
time (`agent-repl--sidebar-sibling-sort')."
  (let* ((entries (agent-repl--known-workspace-entries))
         (current (ignore-errors (agent-repl--ws-current-name)))
         (nodes (mapcar (lambda (e)
                          (list :name (car e)
                                :dir (agent-repl--path-canonical (cdr e))))
                        entries))
         (by-dir (make-hash-table :test 'equal))
         (children (make-hash-table :test 'equal))
         (roots-by-repo (make-hash-table :test 'equal))
         (emitted 0)
         repo-keys)
    (dolist (n nodes) (puthash (plist-get n :dir) n by-dir))
    ;; Attach each node under its parent when the parent is itself a
    ;; roster entry; self-parenting is a corrupted plist, not a family.
    (dolist (n nodes)
      (let* ((name (plist-get n :name))
             (dir (plist-get n :dir))
             (src (and (agent-repl--ws-known-p name)
                       (agent-repl--ws-get name :source-ws-dir)))
             (parent-dir (and src (agent-repl--path-canonical src))))
        (when (equal parent-dir dir)
          (error "agent-repl--sidebar-build: ws=%s is its own parent (dir=%s)"
                 name dir))
        (if (and parent-dir (gethash parent-dir by-dir))
            (push n (gethash parent-dir children))
          (let ((key (agent-repl--sidebar-repo-key-for-entry name dir)))
            (unless (gethash key roots-by-repo)
              (push key repo-keys))
            (push n (gethash key roots-by-repo))))))
    ;; Depth-first emit.  EMITTED counts every serialized row; a count
    ;; short of the entry total means a `:source-ws-dir' cycle orphaned
    ;; a family from every root — rows silently missing from the
    ;; sidebar would be a debugging tarpit, so fail loudly instead.
    (let* ((flat nil)
           (emit-node nil)
           (repos
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
                        :label (agent-repl--repo-label key)
                        :folded (if folded t :false)
                        :rows (vconcat
                               (mapcar (lambda (n) (funcall emit-node n))
                                       (agent-repl--sidebar-sibling-sort
                                        (gethash key roots-by-repo)))))))
              (sort repo-keys
                    (lambda (a b)
                      ;; The `(no repo)' sentinel always sections last —
                      ;; it is the catch-all, not a repo peer.
                      (cond
                       ((equal a agent-repl--repo-key-unknown) nil)
                       ((equal b agent-repl--repo-key-unknown) t)
                       (t (string< (agent-repl--repo-label a)
                                   (agent-repl--repo-label b))))))))))
      (unless (= emitted (length nodes))
        (error "agent-repl--sidebar-build: emitted %d of %d rows — :source-ws-dir cycle orphaned a family"
               emitted (length nodes)))
      (cons (list :repos repos
                  :navDir (or agent-repl--sidebar-nav-dir :null))
            (nreverse flat)))))

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

(defun agent-repl--sidebar-live-webview-buffers ()
  "Return every live frontend webview buffer across known workspaces."
  (let (bufs)
    (dolist (entry (agent-repl--known-workspace-entries) (nreverse bufs))
      (let ((name (car entry)))
        (when (agent-repl--ws-known-p name)
          (let ((buf (agent-repl--ws-get name :frontend-buffer)))
            (when (buffer-live-p buf)
              (push buf bufs))))))))

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

;;;; ---- The 1Hz change gate ----------------------------------------------

(defvar agent-repl--sidebar-last-signature nil
  "Signature of the roster state as of the last pushed build.")

(defun agent-repl--sidebar-snapshot-mtime ()
  "Modification time of the on-disk workspace snapshot, or nil.
The snapshot contributes roster rows for workspaces no longer in
memory, so its mtime stands in for their content in the signature —
one `file-attributes' stat per tick instead of a full read."
  (when-let* ((file (agent-repl--workspace-snapshot-file-for-read)))
    (file-attribute-modification-time (file-attributes file))))

(defun agent-repl--sidebar-signature ()
  "Return a cheap value that changes whenever the roster would.
Pure in-memory reads plus one stat: per-live-workspace render state
\(status keyword, done-acked, open-p, last-viewed), the current
workspace, the fold set, the nav cursor, the snapshot mtime, and the
set of live webview buffers.  The webview set matters because a
freshly (re)mounted webview needs the roster even when nothing in the
roster itself changed."
  (list (mapcar (lambda (name)
                  (list name
                        (agent-repl--ws-render-status name)
                        (and (agent-repl--ws-get name :done-acked) t)
                        (and (agent-repl--ws-open-p name) t)
                        (agent-repl--ws-get name :last-viewed-at)))
                (agent-repl--live-ws-names))
        (ignore-errors (agent-repl--ws-current-name))
        (agent-repl--folded-repo-keys)
        agent-repl--sidebar-nav-dir
        (agent-repl--sidebar-snapshot-mtime)
        (mapcar #'buffer-name (agent-repl--sidebar-live-webview-buffers))))

(defun agent-repl--sidebar-tick ()
  "1Hz entry point (status.el's state tick): push when the signature moved.
The signature compare is the hot-path gate — the rebuild and push
behind it run only on actual change, so per-tick logging stays on the
verbose ladder."
  (let ((sig (agent-repl--sidebar-signature)))
    (if (equal sig agent-repl--sidebar-last-signature)
        (agent-repl--log-verbose nil "sidebar-tick: signature unchanged, skip")
      (setq agent-repl--sidebar-last-signature sig)
      (agent-repl--sidebar-push))))

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
  "Move the keyboard cursor DELTA rows (wrapping) and push the highlight.
With no cursor yet, DELTA > 0 starts at the first visible row and
DELTA < 0 at the last — the two ends a user reaching for next/prev
expects to land on."
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
      (agent-repl--sidebar-push))))

(defun agent-repl-sidebar-nav-next ()
  "Move the sidebar's keyboard cursor to the next visible workspace row."
  (interactive)
  (agent-repl--sidebar-nav-move 1))

(defun agent-repl-sidebar-nav-prev ()
  "Move the sidebar's keyboard cursor to the previous visible workspace row."
  (interactive)
  (agent-repl--sidebar-nav-move -1))

(defun agent-repl-sidebar-nav-select ()
  "Open the workspace under the sidebar's keyboard cursor."
  (interactive)
  (let ((dir agent-repl--sidebar-nav-dir))
    (unless dir
      (user-error "agent-repl sidebar: no row selected — C-S-n / C-S-p first"))
    (agent-repl--sidebar-open-dir dir)))

;;;; ---- Opening a workspace (shared by keyboard + click) ------------------

(defun agent-repl--sidebar-entry-for-dir (dir)
  "Return the known entry (NAME . DIR) canonically matching DIR, or nil.
Deliberately spans the full known universe (in-memory plus snapshot),
unlike `agent-repl--ws-name-for-dir' (live-only): the sidebar lists
closed and prior-session workspaces precisely so they can be revived."
  (let ((canon (agent-repl--path-canonical dir)))
    (cl-find-if (lambda (e)
                  (equal (agent-repl--path-canonical (cdr e)) canon))
                (agent-repl--known-workspace-entries))))

(defun agent-repl--sidebar-open-dir (dir)
  "Switch Emacs to the workspace whose project dir is DIR.
Routes through `agent-repl--picker-open-selection', the canonical
switch-or-revive: a live perspective switches in place, anything else
\(closed, tombstoned, prior-session) is revived from persisted state.
Signals when DIR matches no known entry — an unknown dir means the
click/cursor and the roster disagree, which is a contract violation to
surface, not a row to silently ignore."
  (let ((entry (agent-repl--sidebar-entry-for-dir dir)))
    (unless entry
      (error "agent-repl sidebar: no known workspace for dir %s" dir))
    (let ((name (car entry)))
      (agent-repl--log name "sidebar-open: name=%s dir=%s live=%s open=%s"
                        name (cdr entry)
                        (and (agent-repl--ws-live-p name) t)
                        (and (agent-repl--ws-open-p name) t))
      (agent-repl--picker-open-selection
       (list :name name
             :project-dir (cdr entry)
             :live-p (agent-repl--ws-live-p name)))
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
  "Return non-nil when KEY names a repo some known entry belongs to.
The `(no repo)' sentinel counts — its section folds like any other."
  (or (equal key agent-repl--repo-key-unknown)
      (cl-some (lambda (e)
                 (equal key (agent-repl--sidebar-repo-key-for-entry
                             (car e) (agent-repl--path-canonical (cdr e)))))
               (agent-repl--known-workspace-entries))))

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

(provide 'agent-repl-sidebar)
;;; sidebar.el ends here
