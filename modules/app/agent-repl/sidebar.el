;;; sidebar.el --- GUI sidebar bridge for the workspace drawer -*- lexical-binding: t; -*-

;;; Commentary:

;; Bridges the Emacs workspace drawer to the browser GUI's left sidebar.
;;
;; Emacs owns every fact the sidebar shows.  This module projects the
;; drawer's view-model — sections, repo groups, workspace trees, and the
;; merge-queue commit stream — into a JSON snapshot and POSTs it to the
;; daemon (`/workspaces/status'), which broadcasts it to every connected
;; webview.  Structure is computed by the SAME drawer helpers the buffer
;; render uses (`agent-repl-drawer--partition-by-section',
;; `--build-tree', `--group-trees-by-repo', `--merge-stream', …), so the
;; sidebar cannot drift from the drawer: parity is structural.
;;
;; Sidebar actions come back the other way as JSON files the daemon
;; drops into `agent-repl-sidebar-actions-dir' (mirroring the
;; workspace-commands channel in worktree.el).  Each file is executed
;; against the same functions the drawer keys dispatch to, its outcome
;; is recorded in `agent-repl-sidebar--last-action-result', and a fresh
;; snapshot is force-pushed so the GUI sees the result.
;;
;; The drawer's buffer-local marked/expanded sets are shared with the
;; sidebar by running every read and mutation inside the singleton
;; drawer buffer (`agent-repl-drawer--get-or-create-buffer') — the
;; drawer buffer is the one owner, exactly as it is for the drawer's
;; own keys.
;;
;; Wire contract: the "Workspace sidebar stream" section of
;; shared/protocol.md.

(require 'subr-x)
(require 'json)

;;;; Customization ----------------------------------------------------------

(defcustom agent-repl-sidebar-enabled t
  "Non-nil enables the GUI sidebar bridge.
When enabled, workspace snapshots are pushed to the daemon on the
drawer's refresh triggers and the webview URL gains `sidebar=1'.
On by default (Phase 2); setting it nil stops every push and drops the
URL param, so the GUI renders no sidebar.  Batch tests bind it nil at
the `agent-repl-test--with-clean-state' choke point and opt in
explicitly."
  :type 'boolean
  :group 'agent-repl)

(defcustom agent-repl-sidebar-width-px 340
  "Fixed sidebar width in CSS pixels, shipped in the snapshot as `width_px'.
The sidebar never resizes at runtime — the GUI's output column absorbs
every horizontal resize — so editing this custom (and letting the next
push land) is the one way the width changes."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-sidebar-actions-file-prefix "sidebar_action_"
  "Filename prefix for sidebar action files.
Must match `filePrefix' in daemon/internal/sidebaraction."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-sidebar-actions-file-regexp "^sidebar_action_.*\\.json$"
  "Regexp matching sidebar action files in the actions directory."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-sidebar-actions-dir
  (file-name-as-directory (agent-repl--global-state-file "sidebar-actions"))
  "Directory watched for sidebar action files.
Lives under `agent-repl--global-state-dir'; must match `DirName' in
daemon/internal/sidebaraction."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-sidebar-heartbeat-seconds 30
  "Maximum seconds between snapshot pushes while the bridge is enabled.
An unchanged view-model normally suppresses the push (signature gate);
the heartbeat forces one anyway so clients can distinguish an idle
Emacs from a dead one."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-sidebar-push-failure-backoff-seconds 15
  "Seconds to suppress snapshot pushes after a failed POST.
Keeps a downed daemon from being re-POSTed at the full poll rate."
  :type 'integer
  :group 'agent-repl)

(defconst agent-repl-sidebar-version 1
  "Snapshot schema version, carried as `sidebar_version'.")

;;;; State --------------------------------------------------------------------

(defvar agent-repl-sidebar--last-push-signature 'unset
  "View-model signature at the last attempted push.
Sentinel `unset' (not nil) so the first push always fires, and so a
failed POST can force a retry by resetting to it.")

(defvar agent-repl-sidebar--last-push-time 0
  "`float-time' of the last attempted push (heartbeat anchor).")

(defvar agent-repl-sidebar--last-failure-time 0
  "`float-time' of the last failed POST (backoff anchor).")

(defvar agent-repl-sidebar--last-action-result nil
  "Alist describing the most recent sidebar action's outcome, or nil.
Shape: ((id . STRING) (ok . t|:json-false) (error . STRING|nil)).
Included verbatim in every snapshot as `last_action_result'.")

(defvar agent-repl-sidebar--actions-watch nil
  "File-notify descriptor for the sidebar actions directory.")

;;;; JSON helpers ---------------------------------------------------------------

(defun agent-repl-sidebar--bool (x)
  "Return a json-encodable boolean for X (t / :json-false, never nil)."
  (if x t :json-false))

;;;; Priority badge images --------------------------------------------------------

(defconst agent-repl-sidebar--images-dir
  (expand-file-name "images/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The module images/ directory holding the priority badge PNGs.
Same source `agent-repl--load-priority-images' (status.el) reads for
the drawer's in-buffer badges.")

(defvar agent-repl-sidebar--priority-image-uris 'unset
  "Cached alist of priority name → PNG data URI, or the `unset' sentinel.
Computed once per session by `agent-repl-sidebar--priority-images' —
the badge files never change at runtime, and re-base64ing them on every
snapshot push would be pure waste.")

(defun agent-repl-sidebar--read-image-uri (file)
  "Return FILE's bytes as a PNG data URI."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (concat "data:image/png;base64,"
            (base64-encode-string (buffer-string) t))))

(defun agent-repl-sidebar--priority-images ()
  "Return the priority badge data-URI alist, computing it on first use.
One entry per `agent-repl-priority-levels' name whose PNG exists; the
browser falls back to text chips for any name absent here."
  (when (eq agent-repl-sidebar--priority-image-uris 'unset)
    (setq agent-repl-sidebar--priority-image-uris
          (cl-loop for name in agent-repl-priority-levels
                   for file = (expand-file-name
                               (concat name ".png")
                               agent-repl-sidebar--images-dir)
                   when (file-exists-p file)
                   collect (cons name (agent-repl-sidebar--read-image-uri file)))))
  agent-repl-sidebar--priority-image-uris)

;;;; Snapshot: entries ----------------------------------------------------------

(defun agent-repl-sidebar--status-name (status)
  "Return STATUS (a `:keyword' or nil) as a bare string, or nil."
  (and status (substring (symbol-name status) 1)))

(defun agent-repl-sidebar--name-color (ws)
  "Return the drawer name-face foreground for WS, or nil.
`agent-repl-drawer--name-face' returns either an anonymous face plist
carrying the status color or the default face symbol; only the former
names a color."
  (let ((face (agent-repl-drawer--name-face ws)))
    (and (consp face) (plist-get face :foreground))))

(defun agent-repl-sidebar--detail (ws section)
  "Return the expanded-detail alist for WS in SECTION.
Reads only the cached `:detail-*' plist fields (populated by
`agent-repl-drawer--refresh-detail-cache') plus the same live fields
the drawer's detail render reads; never invokes git.  Values the drawer
would not render are nil here so the browser cannot show a line the
drawer suppresses: `ahead_source' (and its branch) when the source IS
the trunk, `dirty_count' / `pending_prompts' when zero, and the
MERGED-only fields outside the MERGED section."
  (let* ((source-branch (agent-repl--ws-get ws :detail-source-branch))
         (source-is-trunk (and source-branch
                               (string= source-branch
                                        agent-repl-master-branch-name)))
         (dirty-count (agent-repl--ws-get ws :detail-dirty-count))
         (pending (length (agent-repl--ws-get ws :pending-prompts)))
         (merge-completed-at (and (eq section :merged)
                                  (agent-repl--ws-get ws :merge-completed-at))))
    `((merge_status . ,(agent-repl-drawer--merge-status-text ws))
      (branch . ,(agent-repl--ws-get ws :detail-branch))
      (merged_into . ,(and (eq section :merged)
                           (agent-repl--ws-get ws :merge-target-name)))
      (merge_completed_at . ,(and (numberp merge-completed-at)
                                  merge-completed-at))
      (trunk . ,agent-repl-master-branch-name)
      (ahead_master . ,(agent-repl--ws-get ws :detail-master-ahead))
      (source_branch . ,(unless source-is-trunk source-branch))
      (ahead_source . ,(unless source-is-trunk
                         (agent-repl--ws-get ws :detail-source-ahead)))
      (last_commit . ,(agent-repl--ws-get ws :detail-last-commit))
      (last_commit_time . ,(agent-repl--ws-get ws :detail-last-commit-time))
      (dirty_count . ,(and (integerp dirty-count) (> dirty-count 0)
                           dirty-count))
      (last_prompt_at . ,(agent-repl--ws-get ws :last-prompt-time))
      (pending_prompts . ,(and (> pending 0) pending))
      (merged_in . ,(vconcat (agent-repl--ws-get ws :merged-in-workspaces))))))

(defun agent-repl-sidebar--entry (ws depth section)
  "Return the snapshot row alist for workspace WS at DEPTH in SECTION.
Must run with the drawer buffer current: `--marked-p'/`--expanded-p'
read the drawer's buffer-local sets."
  (let* ((status (and (agent-repl--ws-known-p ws)
                      (agent-repl--ws-render-status ws)))
         (expanded (and (agent-repl-drawer--expanded-p ws) t))
         (hidden (eq section :hidden)))
    `((ws . ,ws)
      (depth . ,depth)
      (section . ,(agent-repl-sidebar--status-name section))
      (session_id . ,(agent-repl--ws-get ws :frontend-session-id))
      (status . ,(agent-repl-sidebar--status-name status))
      (glyph . ,(agent-repl-drawer--state-glyph ws))
      (name_color . ,(agent-repl-sidebar--name-color ws))
      (priority . ,(agent-repl--ws-get ws :priority))
      (summary . ,(agent-repl-drawer--summary-text ws))
      (summary_pending . ,(agent-repl-sidebar--bool
                           (agent-repl--ws-get ws :last-prompt-summary-pending)))
      (dirty . ,(agent-repl-sidebar--bool
                 (eq (agent-repl--ws-get ws :git-clean) 'dirty)))
      (hidden . ,(agent-repl-sidebar--bool hidden))
      (marked . ,(agent-repl-sidebar--bool (agent-repl-drawer--marked-p ws)))
      (expanded . ,(agent-repl-sidebar--bool expanded))
      (current . ,(agent-repl-sidebar--bool
                   (equal ws (agent-repl-drawer--current-ws))))
      (help . ,(format "Workspace: %s%s" ws (if hidden " (hidden)" "")))
      (detail . ,(when expanded (agent-repl-sidebar--detail ws section))))))

(defun agent-repl-sidebar--flatten-tree (tree depth section)
  "Flatten TREE (a `(WS . CHILDREN)' cell) into entry alists from DEPTH.
Depth-first, mirroring `agent-repl-drawer--render-subtree' order."
  (cons (agent-repl-sidebar--entry (car tree) depth section)
        (apply #'append
               (mapcar (lambda (child)
                         (agent-repl-sidebar--flatten-tree
                          child (1+ depth) section))
                       (cdr tree)))))

;;;; Snapshot: sections + groups -------------------------------------------------

(defun agent-repl-sidebar--groups (workspaces section)
  "Return the repo-group alists for WORKSPACES in SECTION.
Same tree build + repo bucketing as the drawer's `--insert-section' /
`--render-trees'; a folded group ships an empty entries array, exactly
as the drawer renders its header and nothing else."
  (let* ((parent-fn (agent-repl-drawer--parent-fn-for-section
                     workspaces section))
         (trees (agent-repl-drawer--build-tree workspaces parent-fn))
         (groups (agent-repl-drawer--group-trees-by-repo trees)))
    (mapcar
     (lambda (group)
       (let* ((key (car group))
              (label (cadr group))
              (group-trees (cddr group))
              (folded (agent-repl--repo-folded-p key)))
         `((key . ,(if (stringp key) key (format "%s" key)))
           (label . ,label)
           (folded . ,(agent-repl-sidebar--bool folded))
           (entries . ,(vconcat
                        (unless folded
                          (apply #'append
                                 (mapcar (lambda (tree)
                                           (agent-repl-sidebar--flatten-tree
                                            tree 0 section))
                                         group-trees))))))))
     groups)))

(defun agent-repl-sidebar--section (id label workspaces section)
  "Return the section alist for WORKSPACES titled LABEL with dom id ID."
  `((id . ,id)
    (label . ,label)
    (count . ,(length workspaces))
    (groups . ,(vconcat (when workspaces
                          (agent-repl-sidebar--groups workspaces section))))))

;;;; Snapshot: merge queue --------------------------------------------------------

(defun agent-repl-sidebar--merge-row (element)
  "Return the snapshot row for merge-stream ELEMENT."
  (let ((files (plist-get element :conflict-files))
        (phase (plist-get element :resolver-phase)))
    `((kind . "commit")
      (sha . ,(plist-get element :sha))
      (subject . ,(plist-get element :subject))
      (state . ,(symbol-name (plist-get element :state)))
      (ws . ,(plist-get element :ws))
      (started_at . ,(plist-get element :started-at))
      (conflict_files . ,(and files (length files)))
      (resolver_phase . ,(and phase (symbol-name phase)))
      (resolver_started_at . ,(plist-get element :resolver-started-at)))))

(defun agent-repl-sidebar--merge-queue ()
  "Return the MERGE QUEUE alist, or nil when the queue is idle.
Separator rows interleave exactly as the drawer's
`--insert-merge-queue-section' emits project headers: before the first
commit and whenever the project changes."
  (let ((stream (agent-repl-drawer--merge-stream-visible
                 (agent-repl-drawer--merge-stream))))
    (when stream
      (let ((rows nil)
            (previous-project nil)
            (first t))
        (dolist (element stream)
          (let ((project (plist-get element :project)))
            (unless (and (not first) (equal project previous-project))
              (push `((kind . "separator")
                      (project . ,(or project "(no repo)")))
                    rows)
              (setq previous-project project))
            (setq first nil)
            (push (agent-repl-sidebar--merge-row element) rows)))
        `((count . ,(length stream))
          (rows . ,(vconcat (nreverse rows))))))))

;;;; Snapshot: top level ------------------------------------------------------------

(defun agent-repl-sidebar--marks ()
  "Return the shared marked-set's workspace names as a vector.
Must run with the drawer buffer current."
  (vconcat (when agent-repl-drawer--marked-set
             (hash-table-keys agent-repl-drawer--marked-set))))

(defun agent-repl-sidebar--snapshot ()
  "Build the full workspace snapshot alist.
Runs inside the singleton drawer buffer (the owner of the shared
marked/expanded sets) and inside `--with-dir-map' so the tree walks
amortize one reverse-lookup build, mirroring `--render'."
  (with-current-buffer (agent-repl-drawer--get-or-create-buffer)
    (agent-repl-drawer--with-dir-map
      (let* ((keys (agent-repl-drawer--visible-workspace-keys))
             (buckets (agent-repl-drawer--partition-by-section keys))
             (mains (alist-get :main buckets))
             (hiddens (alist-get :hidden buckets))
             (mergings (alist-get :merging buckets))
             (mergeds (alist-get :merged buckets))
             (merge-queue (agent-repl-sidebar--merge-queue)))
        (append
         `((type . "workspace-snapshot")
           (sidebar_version . ,agent-repl-sidebar-version)
           (generated_at . ,(float-time))
           (current_ws . ,(agent-repl-drawer--current-ws))
           (sidebar_visible . ,(agent-repl-sidebar--bool
                                agent-repl-drawer--global-visible-p))
           (merge_slow_threshold . ,agent-repl-drawer-merge-slow-commit-threshold)
           (width_px . ,agent-repl-sidebar-width-px)
           (marks . ,(agent-repl-sidebar--marks))
           (last_action_result . ,agent-repl-sidebar--last-action-result))
         (let ((images (agent-repl-sidebar--priority-images)))
           (when images
             `((priority_images . ,images))))
         (when merge-queue
           `((merge_queue . ,merge-queue)))
         `((sections . ,(vconcat
                         (delq nil
                               (list
                                (agent-repl-sidebar--section
                                 "main" "MAIN" mains :main)
                                (when hiddens
                                  (agent-repl-sidebar--section
                                   "hidden" "HIDDEN" hiddens :hidden))
                                (agent-repl-sidebar--section
                                 "merging" "MERGING" mergings :merging)
                                (agent-repl-sidebar--section
                                 "merged" "MERGED" mergeds :merged)))))))))))

;;;; Push ---------------------------------------------------------------------------

(defun agent-repl-sidebar--signature ()
  "Return a cheap signature of every input that affects the snapshot.
Reuses the drawer's render signature (the same inputs drive both
renders) plus the two sidebar-only inputs: the shared visibility flag
and the last action result."
  (with-current-buffer (agent-repl-drawer--get-or-create-buffer)
    (list (agent-repl-drawer--render-signature)
          agent-repl-drawer--global-visible-p
          agent-repl-sidebar--last-action-result)))

(defun agent-repl-sidebar--post-callback (status &rest _)
  "Async `url-retrieve' callback: record failure, kill the response buffer."
  (let ((err (plist-get status :error)))
    (when err
      (setq agent-repl-sidebar--last-failure-time (float-time)
            agent-repl-sidebar--last-push-signature 'unset)
      (agent-repl--log nil "sidebar-push: POST failed: %S" err)))
  (kill-buffer (current-buffer)))

(defun agent-repl-sidebar--post-snapshot (json-string)
  "External boundary: async POST of JSON-STRING to the daemon.
Asynchronous on purpose — the push rides the 1Hz poll and a downed
daemon must cost a callback, never a blocking wait."
  (require 'url)
  (let* ((url (concat (agent-repl--frontend-base-url) "/workspaces/status"))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string json-string 'utf-8)))
    (url-retrieve ;; ALLOW-EXTERNAL-BOUNDARY
     url #'agent-repl-sidebar--post-callback nil t t)))

(defun agent-repl-sidebar--push (&optional force)
  "Push a snapshot to the daemon when the view-model changed.
No-op unless `agent-repl-sidebar-enabled'.  Skips while inside the
post-failure backoff window (unless FORCE).  Pushes when FORCE, when
the signature differs from the last push, or when the heartbeat
interval has elapsed."
  (when agent-repl-sidebar-enabled
    (let ((now (float-time)))
      (unless (and (not force)
                   (< (- now agent-repl-sidebar--last-failure-time)
                      agent-repl-sidebar-push-failure-backoff-seconds))
        (let ((sig (agent-repl-sidebar--signature)))
          (when (or force
                    (not (equal sig agent-repl-sidebar--last-push-signature))
                    (>= (- now agent-repl-sidebar--last-push-time)
                        agent-repl-sidebar-heartbeat-seconds))
            (setq agent-repl-sidebar--last-push-signature sig
                  agent-repl-sidebar--last-push-time now)
            (agent-repl-sidebar--post-snapshot
             (json-encode (agent-repl-sidebar--snapshot)))))))))

(defun agent-repl-sidebar--on-ws-activated (&rest _)
  "Push on workspace switch so the sidebar arrow tracks immediately."
  (agent-repl-sidebar--push))

(agent-repl--ws-add-activated-hook #'agent-repl-sidebar--on-ws-activated)

;;;; Actions: execution ----------------------------------------------------------------

(defun agent-repl-sidebar--require-known (targets)
  "Signal `user-error' unless every TARGETS name is a known workspace."
  (dolist (ws targets)
    (unless (and (stringp ws) (agent-repl--ws-known-p ws))
      (user-error "Unknown workspace: %s" ws))))

(defun agent-repl-sidebar--visit (ws)
  "Visit WS: persp switch, reactivating a MERGED entry first.
Mirrors `agent-repl-drawer-visit' minus the point lookup."
  (if (eq (agent-repl-drawer--workspace-section ws) :merged)
      (agent-repl-drawer--reactivate-merged ws)
    (agent-repl-drawer--leave-side-window-before-switch)
    (agent-repl--ws-switch ws)))

(defun agent-repl-sidebar--nuke (targets confirmed)
  "Nuke TARGETS.  A MERGED target requires CONFIRMED (the browser's
confirm dialog stands in for the drawer's `y-or-n-p') and dispatches to
`agent-repl--finish-workspace'; others take `agent-repl-nuke-workspace'."
  (let ((agent-repl--kill-cause "gui sidebar nuke/finish action (browser confirm)"))
    (dolist (ws targets)
      (if (eq (agent-repl-drawer--workspace-section ws) :merged)
          (if confirmed
              (agent-repl--finish-workspace ws)
            (user-error "Finishing MERGED workspace %s requires confirmation" ws))
        (agent-repl-nuke-workspace ws)))))

(defun agent-repl-sidebar--send-prompt (targets prompt)
  "Send PROMPT to each of TARGETS via the drawer's send path."
  (when (or (null prompt) (string-empty-p (string-trim prompt)))
    (user-error "Empty prompt"))
  (agent-repl-drawer--reject-merged-targets targets "send to")
  (dolist (ws targets)
    (agent-repl--send prompt ws)))

(defun agent-repl-sidebar--toggle-hidden (targets)
  "Toggle each of TARGETS between `:hidden' and active.
Mirrors `agent-repl-drawer-toggle-hidden' minus the point lookup."
  (dolist (ws targets)
    (if (eq (agent-repl--ws-get ws :repl-state) :hidden)
        (agent-repl--unhide-workspace ws)
      (agent-repl--on-close ws))))

(defun agent-repl-sidebar--in-drawer-buffer (fn)
  "Call FN with the singleton drawer buffer current.
The shared marked/expanded sets are its buffer-locals."
  (with-current-buffer (agent-repl-drawer--get-or-create-buffer)
    (funcall fn)))

(defun agent-repl-sidebar--toggle-mark (targets)
  "Toggle each of TARGETS in the shared marked-set."
  (agent-repl-sidebar--require-known targets)
  (agent-repl-sidebar--in-drawer-buffer
   (lambda ()
     (agent-repl-drawer--ensure-marked-set)
     (dolist (ws targets)
       (if (gethash ws agent-repl-drawer--marked-set)
           (remhash ws agent-repl-drawer--marked-set)
         (puthash ws t agent-repl-drawer--marked-set))))))

(defun agent-repl-sidebar--clear-marks ()
  "Empty the shared marked-set."
  (agent-repl-sidebar--in-drawer-buffer
   (lambda ()
     (when agent-repl-drawer--marked-set
       (clrhash agent-repl-drawer--marked-set)))))

(defun agent-repl-sidebar--toggle-expand (targets)
  "Toggle each of TARGETS in the shared expanded-set.
Expanding refreshes the detail cache (synchronous git), exactly as the
drawer's `TAB' does."
  (agent-repl-sidebar--require-known targets)
  (agent-repl-sidebar--in-drawer-buffer
   (lambda ()
     (agent-repl-drawer--ensure-expanded-set)
     (dolist (ws targets)
       (if (gethash ws agent-repl-drawer--expanded-set)
           (remhash ws agent-repl-drawer--expanded-set)
         (agent-repl-drawer--refresh-detail-cache ws)
         (puthash ws t agent-repl-drawer--expanded-set))))))

(defun agent-repl-sidebar--toggle-fold (keys)
  "Fold/unfold each repo key in KEYS.
Model-level only plus a tab-bar repaint — deliberately NOT
`agent-repl-drawer--toggle-repo-fold', whose `--render' call assumes
the drawer buffer is current."
  (dolist (key keys)
    (agent-repl--toggle-repo-fold key))
  (agent-repl--force-tab-bar-redraw))

(defun agent-repl-sidebar--refresh ()
  "Re-fetch detail caches for every expanded entry (the drawer's `g')."
  (agent-repl-sidebar--in-drawer-buffer
   (lambda ()
     (when agent-repl-drawer--expanded-set
       (maphash (lambda (ws _)
                  (agent-repl-drawer--refresh-detail-cache ws))
                agent-repl-drawer--expanded-set)))))

(defun agent-repl-sidebar--execute-action (action targets args confirmed)
  "Execute ACTION against TARGETS with ARGS.
Signals (`user-error' for gating refusals, `error' for the rest) on
failure; the caller converts the signal into `last_action_result'."
  (pcase action
    ("visit"
     (agent-repl-sidebar--require-known targets)
     (agent-repl-sidebar--visit (car targets)))
    ("nuke"
     (agent-repl-sidebar--require-known targets)
     (agent-repl-sidebar--nuke targets confirmed))
    ("kill"
     (agent-repl-sidebar--require-known targets)
     (agent-repl-drawer--reject-merged-targets targets "kill")
     (dolist (ws targets) (agent-repl-kill-workspace ws)))
    ("send-prompt"
     (agent-repl-sidebar--require-known targets)
     (agent-repl-sidebar--send-prompt targets (alist-get 'prompt args)))
    ("interrupt"
     (agent-repl-sidebar--require-known targets)
     (dolist (ws targets) (agent-repl-interrupt ws)))
    ("merge-into-source"
     (agent-repl-sidebar--require-known targets)
     (dolist (ws targets)
       (agent-repl-drawer--with-temp-current-ws
        ws #'agent-repl-workspace-merge-current-into-source)))
    ("merge-child"
     (agent-repl-sidebar--require-known targets)
     (agent-repl-drawer--with-temp-current-ws
      (car targets) #'agent-repl-workspace-merge))
    ("new-child"
     (agent-repl-sidebar--require-known targets)
     (agent-repl-drawer--reject-merged-targets targets "create child from")
     (agent-repl-create-worktree-workspace 'head (car targets)))
    ("new-fork"
     (agent-repl-sidebar--require-known targets)
     (agent-repl-drawer--reject-merged-targets targets "fork from")
     (agent-repl-fork-worktree-workspace (car targets)))
    ("toggle-hidden"
     (agent-repl-sidebar--require-known targets)
     (agent-repl-sidebar--toggle-hidden targets))
    ("priority-up"
     (agent-repl-sidebar--require-known targets)
     (dolist (ws targets) (agent-repl-drawer--cycle-priority ws -1)))
    ("priority-down"
     (agent-repl-sidebar--require-known targets)
     (dolist (ws targets) (agent-repl-drawer--cycle-priority ws +1)))
    ("set-priority"
     (agent-repl-sidebar--require-known targets)
     (let ((priority (alist-get 'priority args)))
       (unless (or (null priority) (eq priority :null) (stringp priority))
         (user-error "set-priority: bad priority %S" priority))
       ;; The empty string is `agent-repl-set-priority''s clear sentinel,
       ;; standing in for the JSON null the drag-drop sends.
       (dolist (ws targets)
         (agent-repl-set-priority (if (stringp priority) priority "") ws))))
    ("show-commit"
     (agent-repl-sidebar--require-known targets)
     (let ((sha (alist-get 'sha args)))
       (unless (and (stringp sha) (not (string-empty-p sha)))
         (user-error "show-commit: missing sha"))
       (unless (fboundp 'magit-show-commit)
         (user-error "show-commit: magit is unavailable"))
       (agent-repl-drawer--with-temp-current-ws
        (car targets)
        (lambda () (magit-show-commit sha)))))
    ("toggle-mark" (agent-repl-sidebar--toggle-mark targets))
    ("clear-marks" (agent-repl-sidebar--clear-marks))
    ("toggle-expand" (agent-repl-sidebar--toggle-expand targets))
    ("toggle-fold" (agent-repl-sidebar--toggle-fold targets))
    ("refresh" (agent-repl-sidebar--refresh))
    ("hide-sidebar" (agent-repl-drawer-hide))
    (_ (user-error "Unknown sidebar action: %s" action))))

;;;; Actions: file channel ------------------------------------------------------------

(defun agent-repl-sidebar--process-action-file (file)
  "Parse, delete, and execute the sidebar action in FILE.
The file is deleted BEFORE execution so a crashing action cannot loop.
The outcome lands in `agent-repl-sidebar--last-action-result', the
drawer refreshes, and a snapshot is force-pushed either way."
  (let ((payload
         (condition-case err
             (json-parse-string
              (with-temp-buffer
                (insert-file-contents file)
                (buffer-string))
              :object-type 'alist :array-type 'list)
           (error
            (agent-repl--log nil "sidebar-action: unparseable file=%s err=%S"
                             file err)
            nil))))
    (when (file-exists-p file)
      (delete-file file))
    (when payload
      (let* ((id (alist-get 'id payload))
             (action (alist-get 'action payload))
             (targets (alist-get 'targets payload))
             (args (alist-get 'args payload))
             (confirmed (eq (alist-get 'confirmed payload) t))
             (failure
              (condition-case err
                  (progn
                    (agent-repl--log nil "sidebar-action: id=%s action=%s targets=%S"
                                     id action targets)
                    (agent-repl-sidebar--execute-action
                     action targets args confirmed)
                    nil)
                (error (error-message-string err)))))
        (setq agent-repl-sidebar--last-action-result
              `((id . ,id)
                (ok . ,(agent-repl-sidebar--bool (null failure)))
                (error . ,failure)))
        (when failure
          (agent-repl--log nil "sidebar-action: id=%s FAILED: %s" id failure))
        (when (fboundp 'agent-repl-drawer--refresh-if-visible)
          (agent-repl-drawer--refresh-if-visible))
        (agent-repl-sidebar--push t)))))

(defconst agent-repl-sidebar--actions-watcher
  '(:label "sidebar-actions-watch"
    :dir-var agent-repl-sidebar-actions-dir
    :prefix-var agent-repl-sidebar-actions-file-prefix
    :regexp-var agent-repl-sidebar-actions-file-regexp
    :descriptor-var agent-repl-sidebar--actions-watch
    :process-fn agent-repl-sidebar--process-action-file
    :register-fn agent-repl-sidebar--register-actions-watch
    :drain-fn agent-repl-sidebar--drain-action-files
    :handler-fn agent-repl-sidebar--actions-watch-handler)
  "dir-watcher spec for the sidebar action channel.
See dir-watcher.el for the key contract; every value is a symbol so
defcustom edits, test `let'-bindings, and `cl-letf' stubs of the named
functions all resolve at use time.")

(defun agent-repl-sidebar--drain-action-files ()
  "Process every sidebar action file currently in the actions directory.
Catch-up path for files that landed while the watch was down.  Returns
the number of files processed."
  (agent-repl--dir-watcher-drain agent-repl-sidebar--actions-watcher))

(defun agent-repl-sidebar--actions-watch-handler (event)
  "Handle a file-notify EVENT for the sidebar actions directory.
Dispatches matching creates/renames to
`agent-repl-sidebar--process-action-file'; a lost watch re-arms and
drains.  See `agent-repl--dir-watcher-handle-event' for the shared
semantics."
  (agent-repl--dir-watcher-handle-event
   agent-repl-sidebar--actions-watcher event))

(defun agent-repl-sidebar--register-actions-watch ()
  "Register the file-notify watch on the sidebar actions directory.
Tears down any existing watch first to avoid duplicates on re-eval."
  (agent-repl--dir-watcher-register agent-repl-sidebar--actions-watcher))

(agent-repl-sidebar--register-actions-watch)

;;;; Global chords -------------------------------------------------------------
;;
;; The C-S-* chords route here (keybindings.el).  With the drawer open,
;; coexistence keeps its semantics untouched: the drawer-global command
;; runs verbatim.  With no drawer buffer, the chords reach the GUI
;; sidebar instead — cursor-targeted ops dispatch into the current
;; workspace's webview over the execute-script channel (the xwidget
;; cannot receive Emacs keys), and follow-navigation is computed
;; Emacs-side against the same view-model order the sidebar renders.

(defun agent-repl-sidebar--webview-dispatch (op)
  "Run the sidebar host hook OP inside the current workspace's webview.
Guarded page-side (`window.agentReplSidebar && …'): a webview mid-boot
has no hook planted yet, and that is an expected state."
  (let* ((ws (or (agent-repl--ws-current-name)
                 (user-error "No current workspace")))
         (buf (agent-repl--ws-get ws :frontend-buffer)))
    (unless (buffer-live-p buf)
      (user-error "No live webview for %s" ws))
    (agent-repl--frontend-webview-execute-script
     buf (format "window.agentReplSidebar && window.agentReplSidebar(%S);"
                 op))))

(defun agent-repl-sidebar--follow-candidates ()
  "Return (WS . SECTION-ID) cells in sidebar render order.
Extracted from the same snapshot the GUI renders, so Emacs-side follow
navigation walks exactly the order the user sees."
  (let (result)
    (dolist (section (append (alist-get 'sections
                                        (agent-repl-sidebar--snapshot))
                             nil))
      (dolist (group (append (alist-get 'groups section) nil))
        (dolist (entry (append (alist-get 'entries group) nil))
          (push (cons (alist-get 'ws entry) (alist-get 'section entry))
                result))))
    (nreverse result)))

(defun agent-repl-sidebar--follow-step (delta)
  "Switch to the nearest eligible workspace DELTA steps away in sidebar order.
Skips MERGED entries (reviving one is `visit''s deliberate act, never a
scroll side effect) and the already-current workspace, and stops at the
list edge — the drawer's follow rules."
  (let* ((candidates (agent-repl-sidebar--follow-candidates))
         (names (mapcar #'car candidates))
         (current (agent-repl--ws-current-name))
         (idx (or (cl-position current names :test #'equal)
                  (if (> delta 0) -1 (length names))))
         (step (if (> delta 0) 1 -1))
         (i (+ idx step))
         (target nil))
    (while (and (null target) (>= i 0) (< i (length names)))
      (let ((candidate (nth i candidates)))
        (unless (or (equal (cdr candidate) "merged")
                    (equal (car candidate) current))
          (setq target (car candidate))))
      (setq i (+ i step)))
    (if (null target)
        (agent-repl--log current "sidebar-follow: no candidate (delta=%d)" delta)
      (agent-repl--log target "sidebar-follow: switching from=%s to=%s"
                       current target)
      (agent-repl-drawer--leave-side-window-before-switch)
      (agent-repl--ws-switch target))))

(defun agent-repl-sidebar--global (drawer-command sidebar-thunk)
  "Route a global chord: an open drawer wins, else the sidebar.
DRAWER-COMMAND runs whenever the drawer buffer exists, keeping
coexistence semantics untouched.  Otherwise SIDEBAR-THUNK runs when the
bridge is enabled.  With neither available, DRAWER-COMMAND still runs
so its own \"Drawer not open — `SPC o d' first\" user-error surfaces."
  (if (or (get-buffer agent-repl-drawer-buffer-name)
          (not agent-repl-sidebar-enabled))
      (call-interactively drawer-command)
    (funcall sidebar-thunk)))

(defun agent-repl-sidebar-global-next ()
  "Follow-next entry: the drawer's cursor when open, else sidebar order."
  (interactive)
  (agent-repl-sidebar--global
   #'agent-repl-drawer-global-next
   (lambda () (agent-repl-sidebar--follow-step 1))))

(defun agent-repl-sidebar-global-prev ()
  "Follow-previous entry: the drawer's cursor when open, else sidebar order."
  (interactive)
  (agent-repl-sidebar--global
   #'agent-repl-drawer-global-prev
   (lambda () (agent-repl-sidebar--follow-step -1))))

(defun agent-repl-sidebar-global-visit ()
  "Visit the selected entry: drawer cursor when open, else sidebar cursor."
  (interactive)
  (agent-repl-sidebar--global
   #'agent-repl-drawer-global-visit
   (lambda () (agent-repl-sidebar--webview-dispatch "visit"))))

(defun agent-repl-sidebar-global-nuke ()
  "Nuke the selected entry: drawer cursor when open, else sidebar cursor."
  (interactive)
  (agent-repl-sidebar--global
   #'agent-repl-drawer-global-nuke
   (lambda () (agent-repl-sidebar--webview-dispatch "nuke"))))

(defun agent-repl-sidebar-global-kill ()
  "Kill the selected entry: drawer cursor when open, else sidebar cursor."
  (interactive)
  (agent-repl-sidebar--global
   #'agent-repl-drawer-global-kill
   (lambda () (agent-repl-sidebar--webview-dispatch "kill"))))

(defun agent-repl-sidebar-global-send-prompt ()
  "Send a prompt to the selected entry via drawer or sidebar."
  (interactive)
  (agent-repl-sidebar--global
   #'agent-repl-drawer-global-send-prompt
   (lambda () (agent-repl-sidebar--webview-dispatch "send-prompt"))))

(defun agent-repl-sidebar-global-merge-into-master ()
  "Merge the selected entry into its source via drawer or sidebar."
  (interactive)
  (agent-repl-sidebar--global
   #'agent-repl-drawer-global-merge-into-master
   (lambda () (agent-repl-sidebar--webview-dispatch "merge-into-source"))))

(defun agent-repl-sidebar-global-toggle-hidden ()
  "Toggle hidden on the selected entry via drawer or sidebar."
  (interactive)
  (agent-repl-sidebar--global
   #'agent-repl-drawer-global-toggle-hidden
   (lambda () (agent-repl-sidebar--webview-dispatch "toggle-hidden"))))

(defun agent-repl-sidebar-global-toggle-mark ()
  "Toggle the mark on the selected entry via drawer or sidebar."
  (interactive)
  (agent-repl-sidebar--global
   #'agent-repl-drawer-global-toggle-mark
   (lambda () (agent-repl-sidebar--webview-dispatch "toggle-mark"))))

(defun agent-repl-sidebar-global-clear-marks ()
  "Clear all marks via drawer or sidebar."
  (interactive)
  (agent-repl-sidebar--global
   #'agent-repl-drawer-global-clear-marks
   (lambda () (agent-repl-sidebar--webview-dispatch "clear-marks"))))

(defun agent-repl-sidebar-global-priority-up ()
  "Cycle the selected entry's priority up via drawer or sidebar."
  (interactive)
  (agent-repl-sidebar--global
   #'agent-repl-drawer-global-priority-up
   (lambda () (agent-repl-sidebar--webview-dispatch "priority-up"))))

(defun agent-repl-sidebar-global-priority-down ()
  "Cycle the selected entry's priority down via drawer or sidebar."
  (interactive)
  (agent-repl-sidebar--global
   #'agent-repl-drawer-global-priority-down
   (lambda () (agent-repl-sidebar--webview-dispatch "priority-down"))))

(provide 'agent-repl-sidebar)
;;; sidebar.el ends here
