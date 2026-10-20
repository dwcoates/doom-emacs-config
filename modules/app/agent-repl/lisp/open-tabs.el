;;; open-tabs.el --- Workspaces opened by the daemon before their tabs show -*- lexical-binding: t; -*-

;;; Commentary:

;; THE EDITOR ASKS, THE DAEMON OPENS, THE EDITOR SHOWS (owner rulings
;; 2026-10-12, docs/protobuf-design/open-tabs.md).  A workspace's tab is
;; drawn only once the daemon has answered it `opened': its session served
;; by agent-repl's services and, when this Emacs loads a web page for it,
;; that page drawn.  This file is the editor's half:
;;
;;   - every roster row that has no open answered for it is asked for with
;;     ONE `OpenTabs' call per roster walk, its web page loaded at that same
;;     moment (`agent-repl-open-tabs-reconcile');
;;   - its tab stays out of the bar until its `opened' frame arrives
;;     (`agent-repl-open-tabs-holds-p'), and is then drawn at its place in
;;     the registry's tab order, never waiting on any other workspace;
;;   - every step the daemon reports is one minibuffer line;
;;   - a HIDDEN workspace (hidden by the user, or in a collapsed repository
;;     section) is asked for too, and answered at once without starting
;;     anything; when it is shown again it is asked for again, and when an
;;     open one is hidden its page is dropped (the daemon leaves its cache
;;     to lapse, then stands its session down);
;;   - A DAEMON-ONLY RESTART NEVER CLOSES A TAB.  What is open is this
;;     process's own record, so a daemon that restarts finds every open tab
;;     still standing; only an open still in flight when its stream ends is
;;     asked for again, on the next link-up or roster push.
;;
;; THE STARTUP is this process's FIRST open: from process start until it
;; finishes, the frame has no workspace of the user's yet, and the first
;; drawn tab it opens is selected.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--phase-echo "core" (ws fmt &rest args))
(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function agent-repl--ws-by-ref-id "workspace" (id))
(declare-function agent-repl--ws-get "workspace" (ws key))
(declare-function agent-repl-roster-refresh-order "roster" ())
(declare-function agent-repl-roster-apply-current "roster" ())
(declare-function agent-repl-roster-drawn-tab-order "roster" ())
(declare-function agent-repl-roster-note-row "roster" (row revision))
(declare-function agent-repl-host-request-switch "host" (ws trigger))
(declare-function agent-repl--ensure-input-buffer "panels" (ws))
(declare-function agent-repl--frontend-precreate-webview "frontend" (ws))
(declare-function agent-repl--frontend-detach-webview "frontend" (ws buf))
(declare-function agent-repl-open-progress-note-loaded "open-progress" (ws))
(declare-function agent-repl-rpc-open-tabs "rpc" (conn workspaces on-push on-close))
(declare-function agent-repl-link-primary "daemon-link" ())
(declare-function agent-repl-connect-stream-conn "connect" (stream))
(declare-function agent-repl-startup-first-hooks-arm "startup" ())
(declare-function agent-repl-startup-reach "startup" (milestone))
(declare-function agent-repl-startup-ended "startup" ())
(declare-function agent-repl-webhost-xwidget "webhost" ())
(declare-function agent-repl--frontend-make-slot-buffer "frontend" (name xw))
(declare-function agent-repl--pseudo-workspace-name-p "core" (ws))
(declare-function agent-repl--ws-current-name "workspace" ())

(defvar agent-repl-roster--stream)
(defvar agent-repl-roster--walking)
(defvar agent-repl-link-up-functions)

(defconst agent-repl-open-tabs--central
  '(:agent-repl-central "opening tabs spans every workspace")
  "The log scope of every record that names no one workspace.")

;;;; ---- State --------------------------------------------------------------

(defvar agent-repl-open-tabs--state (make-hash-table :test 'equal)
  "Ref id -> where its open stands.
`in-flight' while an `OpenTabs' names it and has not answered it; `open'
once answered ready or failed while visible (its tab is drawn); `dormant'
while it is hidden, or was answered hidden (asked for again when shown);
`refused' once the daemon refused to open it.  An id with no entry has
never been asked for, or its open's stream ended unanswered.  A defvar, so
a hot reload keeps every open tab.")

(defvar agent-repl-open-tabs--rows nil
  "The last roster walk's rows: plists `(:id :name :ref :collapsed)'.")

(defvar agent-repl-open-tabs--startup 'expected
  "Where this process's first open stands.
`expected' from process start until it is asked for, `running' while it
is, `done' once it finished or its stream ended.")

(defvar agent-repl-open-tabs--startup-selected nil
  "The workspace the startup selected, once one of its opened tabs is drawn.")

(defvar agent-repl-open-tabs-sender #'agent-repl-open-tabs--send-to-daemon
  "The function that sends one `OpenTabs' request.
Called with WORKSPACES, ON-FRAME and ON-CLOSE; returns non-nil when the
request was sent.  The daemon boundary: a test answers through its own.")

(defun agent-repl-open-tabs-starting-p ()
  "Return non-nil until this process's first open has finished."
  (not (eq agent-repl-open-tabs--startup 'done)))

(defun agent-repl-open-tabs-choosing-p ()
  "Return non-nil while the startup runs and has not chosen its selection.
Until it has, the frame has no workspace of the user's to move to."
  (and (agent-repl-open-tabs-starting-p) (null agent-repl-open-tabs--startup-selected)))

(defun agent-repl-open-tabs-hidden-p (ws)
  "Return non-nil when workspace WS is hidden or in a collapsed section.
Read from the last roster walk; a workspace it did not meet is not hidden."
  (let* ((id (plist-get (agent-repl--ws-get ws :ref) :id))
         (row (and id (agent-repl-open-tabs--row id))))
    (and row (plist-get row :collapsed) t)))

(defun agent-repl-open-tabs-holds-p (id)
  "Return non-nil while workspace ID's tab is kept out of the tab order.
Every tab is held until the daemon has answered it open.  A HIDDEN one
(`dormant') is not held: it keeps its place in the order and the bar
leaves it undrawn, as it always has; shown again, it is held for its new
open in the same walk."
  (and id (not (memq (gethash id agent-repl-open-tabs--state) '(open dormant)))))

;;;; ---- Lines --------------------------------------------------------------

(defun agent-repl-open-tabs--name (ref)
  "Return the name the lines use for workspace REF."
  (let ((id (plist-get ref :id)))
    (or (and id (agent-repl--ws-by-ref-id id))
        (file-name-nondirectory (directory-file-name (or (plist-get ref :dir) ""))))))

(defun agent-repl-open-tabs--say (fmt &rest args)
  "Say one line, FMT with ARGS, in the minibuffer and the log."
  (apply #'agent-repl--phase-echo agent-repl-open-tabs--central fmt args))

(defun agent-repl-open-tabs--say-step (ws step)
  "Say the line for workspace WS's STEP, a decoded `OpenTabsStep.step' oneof."
  (let ((value (plist-get step :value)))
    (pcase (plist-get step :arm)
      (:starting-session (agent-repl-open-tabs--say "%s: starting session…" ws))
      (:waking (agent-repl-open-tabs--say "%s: waking from sleep…" ws))
      (:resuming (agent-repl-open-tabs--say "%s: resuming the conversation…" ws))
      (:vendor-retrying
       (agent-repl-open-tabs--say "%s: Claude did not start, retrying (attempt %d)…"
                                  ws (plist-get value :attempt)))
      (:vendor-rejected
       (agent-repl-open-tabs--say "%s: Claude refused to start: %s." ws (plist-get value :cause)))
      (:vendor-failed (agent-repl-open-tabs--say "%s: Claude failed to start after 10 minutes." ws))
      (:cold-gate (agent-repl-open-tabs--say "%s: needs your answer to resume (large context)." ws))
      (:offline (agent-repl-open-tabs--say "%s: offline, waiting for the network…" ws))
      (:served (agent-repl-open-tabs--say "%s: session up, waiting for its page…" ws))
      (:page-connected (agent-repl-open-tabs--say "%s: page connected, drawing the conversation…" ws))
      (:conversation-read (agent-repl-open-tabs--say "%s: conversation read." ws))
      (:resuming-cut-turn
       (agent-repl-open-tabs--say "%s: resuming the turn the restart cut…" ws))
      (:page-drawn
       (agent-repl-open-tabs--say "%s: page drawn." ws)
       (when (fboundp 'agent-repl-open-progress-note-loaded)
         (agent-repl-open-progress-note-loaded ws)))
      (arm (agent-repl--error agent-repl-open-tabs--central
                              "elisp.open-tabs.unknown-step arm=%S ws=%s" arm ws)))))

;;;; ---- Asking -------------------------------------------------------------

(defun agent-repl-open-tabs--conn ()
  "Return the connection the roster rides, else the primary link, else nil.
The daemon that served the rows is the one asked to open them."
  (or (and (bound-and-true-p agent-repl-roster--stream)
           (agent-repl-connect-stream-conn agent-repl-roster--stream))
      (agent-repl-link-primary)))

(defun agent-repl-open-tabs--send-to-daemon (workspaces on-frame on-close)
  "Send `OpenTabs' for WORKSPACES; ON-FRAME and ON-CLOSE take its answers.
Returns nil, sending nothing, when there is no daemon connection."
  (let ((conn (agent-repl-open-tabs--conn)))
    (when conn
      (agent-repl-rpc-open-tabs conn workspaces on-frame on-close)
      t)))

(defun agent-repl-open-tabs--page (name collapsed)
  "Load workspace NAME's web page when it is visible; return the page arm.
`:loading' when NAME has a live page now (made here, or already standing),
`:none' when it is COLLAPSED or this Emacs makes it no page.  A page that
FAILS to load is recorded at ERROR and asked `:none', so the workspace
still opens (its tab shows no page) and no other row's open is lost."
  (unless collapsed
    (condition-case err
        (progn
          (agent-repl--ensure-input-buffer name)
          (agent-repl--frontend-precreate-webview name))
      (error
       (agent-repl--error name "elisp.open-tabs.page-failed ws=%s error=%s"
                          name (error-message-string err)))))
  (list :arm (if (and (not collapsed)
                      (buffer-live-p (agent-repl--ws-get name :frontend-buffer)))
                 :loading
               :none)))

(defun agent-repl-open-tabs--ask (rows)
  "Ask the daemon to open ROWS, loading each visible one's page first.
Every row is marked in flight; when the request cannot be sent they are
left unasked, and the next link-up or roster walk asks again."
  (let* ((startup (eq agent-repl-open-tabs--startup 'expected))
         (ids (mapcar (lambda (row) (plist-get row :id)) rows))
         (workspaces
          (mapcar (lambda (row)
                    (list :workspace (plist-get row :ref)
                          :page (agent-repl-open-tabs--page (plist-get row :name)
                                                            (plist-get row :collapsed))))
                  rows)))
    (dolist (id ids) (puthash id 'in-flight agent-repl-open-tabs--state))
    (agent-repl--info agent-repl-open-tabs--central
                      "elisp.open-tabs.ask workspaces=%d startup=%s pages=%S"
                      (length rows) startup
                      (cl-mapcar (lambda (row w)
                                   (cons (plist-get row :name)
                                         (plist-get (plist-get w :page) :arm)))
                                 rows workspaces))
    (when startup
      (setq agent-repl-open-tabs--startup 'running)
      (agent-repl-open-tabs--say "opening %d workspaces…" (length rows))
      (agent-repl-open-tabs--show-host-while-starting))
    (unless (funcall agent-repl-open-tabs-sender workspaces
                     (lambda (frame) (agent-repl-open-tabs--on-frame frame startup))
                     (lambda (outcome) (agent-repl-open-tabs--on-close ids outcome startup)))
      (dolist (id ids) (remhash id agent-repl-open-tabs--state))
      (when startup (setq agent-repl-open-tabs--startup 'expected))
      (agent-repl--info agent-repl-open-tabs--central
                        "elisp.open-tabs.unsent workspaces=%d reason=no-connection"
                        (length rows)))))

;;;; ---- The host on screen while the startup chooses -------------------------

;; A PAGE RUNS ONLY WHILE THE HOST IS ON SCREEN.  WebKit stops running a web
;; view that no window displays, shortly after it was made (2026-10-12: the
;; daemon answered the startup's first pages within 45ms, and the pages heard
;; the answers 194s later, once a tab put the host on screen).  The startup's
;; tabs wait for their pages to draw, so with nothing displaying the host the
;; startup could wait on itself.  While the frame stands on the startup's
;; placeholder, its window displays the host -- empty until a page is shown
;; -- so every page runs from the moment it is asked for.  The startup's
;; first opened tab then takes the frame, and the view is dropped when the
;; startup ends.

(defconst agent-repl-open-tabs--startup-view-name " *agent-repl-opening*"
  "The buffer that displays the host while the startup chooses.")

(defun agent-repl-open-tabs--show-host-while-starting ()
  "Display the web view host in the selected window, while on the placeholder.
Nothing happens when there is no host (no page was loaded), or when the
frame already stands on a workspace of the user's."
  (let ((xw (agent-repl-webhost-xwidget))
        (current (agent-repl--ws-current-name)))
    (if (not (and xw current (agent-repl--pseudo-workspace-name-p current)))
        (agent-repl--log agent-repl-open-tabs--central
                         "elisp.open-tabs.startup-view-skipped host=%s current=%s"
                         (and xw t) current)
      (let ((buf (or (get-buffer agent-repl-open-tabs--startup-view-name)
                     (agent-repl--frontend-make-slot-buffer
                      agent-repl-open-tabs--startup-view-name xw))))
        (set-window-buffer (selected-window) buf)
        (agent-repl--info agent-repl-open-tabs--central
                          "elisp.open-tabs.startup-view-shown current=%s" current)))))

(defun agent-repl-open-tabs--drop-startup-view ()
  "Drop the buffer that displayed the host while the startup chose."
  (when-let ((buf (get-buffer agent-repl-open-tabs--startup-view-name)))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buf))
    (agent-repl--info agent-repl-open-tabs--central "elisp.open-tabs.startup-view-dropped")))

(defun agent-repl-open-tabs--owed-p (row)
  "Return non-nil when ROW must be asked for now."
  (pcase (gethash (plist-get row :id) agent-repl-open-tabs--state)
    ('nil t)
    ('dormant (not (plist-get row :collapsed)))
    (_ nil)))

(defun agent-repl-open-tabs--dormant (id)
  "Mark workspace ID dormant: hidden, its tab back in the order, undrawn.
Outside a walk the order is re-derived at once; a walk derives it itself."
  (puthash id 'dormant agent-repl-open-tabs--state)
  (unless agent-repl-roster--walking
    (agent-repl-roster-refresh-order)))

(defun agent-repl-open-tabs--hide (row)
  "Drop open ROW's page, now that it is hidden: it shows nothing."
  (let* ((name (plist-get row :name))
         (buf (agent-repl--ws-get name :frontend-buffer)))
    (agent-repl-open-tabs--dormant (plist-get row :id))
    (agent-repl--info name "elisp.open-tabs.hidden ws=%s page=%s" name (buffer-live-p buf))
    (when (buffer-live-p buf)
      (agent-repl--frontend-detach-webview name buf))))

(defun agent-repl-open-tabs-reconcile (rows)
  "Bring every open in line with the roster walk's ROWS, then ask for the owed.
ROWS are plists `(:id :name :ref :collapsed)', every workspace the roster
gives a tab, in tab order.  A workspace gone from them is forgotten; an
open one now hidden drops its page; every one never asked for, or shown
again, is asked for in ONE request."
  (let ((wanted (make-hash-table :test 'equal)))
    (dolist (row rows) (puthash (plist-get row :id) t wanted))
    (dolist (id (hash-table-keys agent-repl-open-tabs--state))
      (unless (gethash id wanted)
        (remhash id agent-repl-open-tabs--state))))
  (setq agent-repl-open-tabs--rows rows)
  (dolist (row rows)
    (when (and (plist-get row :collapsed)
               (eq (gethash (plist-get row :id) agent-repl-open-tabs--state) 'open))
      (agent-repl-open-tabs--hide row)))
  (let ((owed (cl-remove-if-not #'agent-repl-open-tabs--owed-p rows)))
    (cond
     (owed (agent-repl-open-tabs--ask owed))
     ;; NOTHING TO OPEN AT ALL: an empty registry still ends the startup.
     ((and (null rows) (eq agent-repl-open-tabs--startup 'expected))
      (agent-repl-open-tabs--end-startup "no-workspaces")))))

(defun agent-repl-open-tabs--ask-owed (&rest _)
  "Ask again for every workspace of the last walk still owed an open.
On a link-up: an open whose stream ended unanswered is asked for again
once a daemon is serving."
  (let ((owed (cl-remove-if-not #'agent-repl-open-tabs--owed-p agent-repl-open-tabs--rows)))
    (when owed
      (agent-repl-open-tabs--ask owed))))

(add-hook 'agent-repl-link-up-functions #'agent-repl-open-tabs--ask-owed)

;;;; ---- Answers ------------------------------------------------------------

(defun agent-repl-open-tabs--row (id)
  "Return the last walk's row for workspace ID, or nil."
  (cl-find id agent-repl-open-tabs--rows
           :key (lambda (row) (plist-get row :id)) :test #'equal))

(defun agent-repl-open-tabs--on-frame (frame startup)
  "Take one decoded `OpenTabsResponse' FRAME; STARTUP marks the first open."
  (let ((value (plist-get frame :value)))
    (pcase (plist-get frame :arm)
      (:step
       (agent-repl-open-tabs--say-step (agent-repl-open-tabs--name (plist-get value :workspace))
                                       (plist-get value :step)))
      (:opened (agent-repl-open-tabs--opened value))
      (:finished
       (when startup
         (agent-repl-open-tabs--say-finished value)
         (agent-repl-open-tabs--end-startup "finished")))
      (arm (agent-repl--error agent-repl-open-tabs--central
                              "elisp.open-tabs.unknown-frame arm=%S" arm)))))

(defun agent-repl-open-tabs--opened (opened)
  "Take one workspace's OPENED answer: draw its tab, or record why not."
  (let* ((ref (plist-get opened :workspace))
         (id (plist-get ref :id))
         (ws (agent-repl-open-tabs--name ref))
         (outcome (plist-get opened :outcome))
         (row (agent-repl-open-tabs--row id)))
    (pcase (plist-get outcome :arm)
      ((or :ready :failed)
       (cond
        ((null row)
         ;; Closed while it opened: the walk already forgot it.
         (agent-repl--info ws "elisp.open-tabs.opened-gone ws=%s id=%s" ws id))
        ((plist-get row :collapsed)
         ;; Hidden while it opened: the daemon stands it down once its cache
         ;; lapses.
         (agent-repl-open-tabs--hide row))
        (t
         ;; THE ROW RIDES THE ANSWER: indexed before the tab is drawn, so it
         ;; is drawn with the workspace's status as of the answer.
         (let ((value (plist-get outcome :value)))
           (agent-repl-roster-note-row (plist-get value :row)
                                       (or (plist-get value :roster-revision) 0)))
         (when (eq (plist-get outcome :arm) :failed)
           (agent-repl-open-tabs--say "%s: session failed to start: %s."
                                      ws (plist-get (plist-get outcome :value) :reason)))
         (agent-repl-open-tabs--show id ws))))
      (:hidden
       (when row (agent-repl-open-tabs--dormant id))
       (agent-repl--info ws "elisp.open-tabs.opened-hidden ws=%s id=%s" ws id))
      (:refused
       (when row (puthash id 'refused agent-repl-open-tabs--state))
       (agent-repl--error ws "elisp.open-tabs.refused ws=%s id=%s cause=%S"
                          ws id (plist-get outcome :value))
       (agent-repl-open-tabs--say "%s: the daemon refused to open it (%s)."
                                  ws (agent-repl-open-tabs--cause-word outcome)))
      (arm (agent-repl--error ws "elisp.open-tabs.unknown-outcome arm=%S ws=%s" arm ws)))))

(defun agent-repl-open-tabs--cause-word (refused)
  "Return the words naming REFUSED's cause, e.g. \"unknown-workspace\"."
  (let ((arm (plist-get (plist-get (plist-get refused :value) :cause) :arm)))
    (if (keywordp arm) (substring (symbol-name arm) 1) "unnamed")))

(defun agent-repl-open-tabs--show (id ws)
  "Draw workspace WS's tab (ref ID), now that it is open.
THE STARTUP'S FIRST DRAWN TAB IS SELECTED at once, so a startup never
shows no workspace while the rest come up; the selection is asked of the
daemon like every other switch."
  (puthash id 'open agent-repl-open-tabs--state)
  (unless agent-repl-roster--walking
    (agent-repl-roster-refresh-order))
  (let ((select (and (agent-repl-open-tabs-starting-p)
                     (null agent-repl-open-tabs--startup-selected)
                     (member ws (agent-repl-roster-drawn-tab-order))
                     t)))
    (agent-repl--info ws "elisp.open-tabs.tab-opened ws=%s id=%s selects=%s" ws id select)
    (when (agent-repl-open-tabs-starting-p)
      (agent-repl-startup-reach :first-tab))
    (agent-repl-open-tabs--say "%s: ready." ws)
    (when select
      (setq agent-repl-open-tabs--startup-selected ws)
      (agent-repl-host-request-switch ws 'startup)))
  ;; The selection may name this workspace, and had no tab to land on.
  (unless agent-repl-roster--walking
    (agent-repl-roster-apply-current)))

(defun agent-repl-open-tabs--say-finished (finished)
  "Say how the startup's FINISHED open went."
  (let ((failed (plist-get finished :failed))
        (total (plist-get finished :total)))
    (if (null failed)
        (agent-repl-open-tabs--say "all %d workspaces ready." total)
      (agent-repl-open-tabs--say "%d of %d workspaces ready; %s failed to start."
                                 (plist-get finished :ready) total
                                 (mapconcat (lambda (f) (plist-get f :name)) failed ", ")))))

(defun agent-repl-open-tabs--on-close (ids outcome startup)
  "The stream of the open of IDS closed with OUTCOME; STARTUP marks the first.
Every id it left unanswered is unasked again, and asked for on the next
link-up or roster walk; never at once, so a daemon that is down is not
dialed in a loop."
  (let ((unanswered (cl-remove-if-not
                     (lambda (id) (eq (gethash id agent-repl-open-tabs--state) 'in-flight))
                     ids)))
    (dolist (id unanswered) (remhash id agent-repl-open-tabs--state))
    (if (and (eq (car outcome) :ended) (null unanswered))
        (agent-repl--log agent-repl-open-tabs--central "elisp.open-tabs.closed outcome=ended")
      ;; INFO: the open is not lost, only asked for again.
      (agent-repl--info agent-repl-open-tabs--central
                        "elisp.open-tabs.closed-unanswered outcome=%S unanswered=%S"
                        outcome unanswered))
    (when (and startup (agent-repl-open-tabs-starting-p))
      (agent-repl-open-tabs--end-startup "stream-ended"))))

(defun agent-repl-open-tabs--end-startup (why)
  "End this process's first open, for WHY, and arm Doom's one-time setup."
  (setq agent-repl-open-tabs--startup 'done)
  (agent-repl--info agent-repl-open-tabs--central "elisp.open-tabs.startup-ended why=%s" why)
  (agent-repl-open-tabs--drop-startup-view)
  (agent-repl-roster-apply-current)
  (agent-repl-startup-ended)
  (agent-repl-startup-first-hooks-arm))

(provide 'agent-repl-open-tabs)
;;; open-tabs.el ends here
