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
;;     open one is hidden by the user its page is dropped (the daemon leaves
;;     its cache to lapse, then stands its session down);
;;   - A FOLD CLOSES WHAT IT MAKES NOT VISIBLE (owner ruling, 2026-11-21,
;;     which supersedes the request of 2026-10-30 that a fold unloaded its
;;     pages and kept their slots).  A workspace the daemon stops calling
;;     visible is torn down whole, perspective, stream and page
;;     (frontend.v1.RosterRowVisibility);
;;   - A DAEMON-ONLY RESTART NEVER CLOSES A TAB.  What is open is this
;;     process's own record, so a daemon that restarts finds every open tab
;;     still standing; only an open still in flight when its stream ends is
;;     asked for again, on the next link-up or roster push.
;;
;; THE STARTUP is this process's FIRST open: from process start until it
;; finishes, the frame has no workspace of the user's yet.  It lands on the
;; workspace the user had selected before the restart -- the roster's
;; `current', which the daemon keeps durably -- once that tab is drawn
;; (owner ruling, 2026-10-22: a restart, a deploy or a landed merge never
;; changes the selection).  Only when the roster names no workspace the
;; startup can draw does it land on the first drawn tab it opens.

;;; Code:

;; Every log call is a log-site macro (log-site.el), needed where this
;; file is expanded; config.el loads it first.
(require 'log-site)

(require 'cl-lib)
(require 'subr-x)

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
(declare-function agent-repl-webhost-has-page-p "webhost" (ws))
(declare-function agent-repl-page-drop-announced "page-unload" (ws still-p drop on-kept))
(declare-function agent-repl-open-progress-note-loaded "open-progress" (ws))
(declare-function agent-repl-rpc-open-tabs "rpc" (conn workspaces on-push on-close))
(declare-function agent-repl-connect-stream-cancel "connect" (stream))
(declare-function agent-repl-link-primary "daemon-link" ())
(declare-function agent-repl-host-view-where-served "host" (ws address trigger))
(declare-function agent-repl-host-conn "host" (ws))
(declare-function agent-repl-host-serving-address "host" (arm-plist))
(declare-function agent-repl-connect-connection-address "connect" (conn))
(declare-function agent-repl-connect-connection-alive-p "connect" (conn))
(declare-function agent-repl-connect-stream-conn "connect" (stream))
(declare-function agent-repl-startup-first-hooks-arm "startup" ())
(declare-function agent-repl-startup-reach "startup" (milestone))
(declare-function agent-repl-startup-ended "startup" ())
(declare-function agent-repl-webhost-xwidget "webhost" ())
(declare-function agent-repl--frontend-make-slot-buffer "frontend" (name xw))
(declare-function agent-repl--pseudo-workspace-name-p "core" (ws))
(declare-function agent-repl--ws-current-name "workspace" ())

(declare-function agent-repl-roster--current-id "roster" (roster))

(defvar agent-repl-roster-view)
(defvar agent-repl-roster--stream)
(defvar agent-repl-roster--walking)
(defvar agent-repl-link-up-functions)
(defvar agent-repl-host-reattached-functions)

(defconst agent-repl-open-tabs--handover-causes '(:transferring-away :not-yet-adopted)
  "The refusal causes that are a rollout's handover, not a failure.
The workspace is moving between daemons: the one asked has handed it on,
or has not taken it over yet.  Its tab is held, and asked for again once
the workspace is re-attached to the daemon that serves it.")

(defconst agent-repl-open-tabs--central
  '(:agent-repl-central "opening tabs spans every workspace")
  "The log scope of every record that names no one workspace.")

;;;; ---- State --------------------------------------------------------------

(defvar agent-repl-open-tabs--state (make-hash-table :test 'equal)
  "Ref id -> where its open stands.
`in-flight' while an `OpenTabs' names it and has not answered it; `open'
once answered ready or failed while visible (its tab is drawn); `dormant'
while it is hidden, or was answered hidden (asked for again when shown);
`refused' once the daemon refused to open it for good; `handover' once it
refused because the workspace is moving between daemons (asked for again
when the workspace is re-attached, `agent-repl-open-tabs--on-reattached').
An id with no entry has never been asked for, or its open's stream ended
unanswered.  A defvar, so a hot reload keeps every open tab.")

(defvar agent-repl-open-tabs--reattached-while-asked (make-hash-table :test 'equal)
  "Ref ids re-attached to a daemon while their open was in flight.
A handover refusal that arrives AFTER its workspace's reattach finished
has no reattach left to wait for, so it is asked for again at once.  The
entry goes with the open's answer, whatever it is.")

(cl-defstruct (agent-repl-open-tabs--request (:constructor agent-repl-open-tabs--request-make))
  "One `OpenTabs' request.
CANCEL ends its stream (what the sender answered); PAGE-DROPPED is set
when a page it waited on was dropped, so its close asks again at once."
  cancel page-dropped)

(defvar agent-repl-open-tabs--page-waits (make-hash-table :test 'equal)
  "Ref id -> the `agent-repl-open-tabs--request' still waiting on its page.
AN OPEN ASKED WITH A PAGE LOADING WAITS FOR THAT PAGE TO DRAW, so a page
dropped before it drew would leave the open unanswered for as long as its
stream stands: the tab held, the row `opening' and unselectable
\(2026-11-24: a workspace parked 140ms after its revived slot loaded its
page stood so for an hour).  So every drop of such a page ends its
request's stream (`agent-repl-open-tabs--page-removed'), whose close asks
again for every workspace it left unanswered, each with the page it holds
then.  The entry goes with the workspace's answer or its stream's close.")

(defvar agent-repl-open-tabs--rows nil
  "The last roster walk's rows: plists `(:id :name :ref :collapsed :hidden)'.
`:collapsed' is a tab off the bar, folded or hidden; `:hidden' is only the
user\='s hide.")

(defvar agent-repl-open-tabs--startup 'expected
  "Where this process's first open stands.
`expected' from process start until it is asked for, `running' while it
is, `done' once it finished or its stream ended.")

(defvar agent-repl-open-tabs--startup-selected nil
  "The workspace the startup selected, once one of its opened tabs is drawn.")

(defvar agent-repl-open-tabs-sender #'agent-repl-open-tabs--send-to-daemon
  "The function that sends one `OpenTabs' request.
Called with WORKSPACES, ON-FRAME and ON-CLOSE; returns a function of no
arguments that cancels the request's stream when it was sent, nil when
it was not.  The daemon boundary: a test answers through its own.")

(defun agent-repl-open-tabs-starting-p ()
  "Return non-nil until this process's first open has finished."
  (not (eq agent-repl-open-tabs--startup 'done)))

(defun agent-repl-open-tabs-choosing-p ()
  "Return non-nil while the startup runs and has not chosen its selection.
Until it has, the frame has no workspace of the user's to move to."
  (and (agent-repl-open-tabs-starting-p) (null agent-repl-open-tabs--startup-selected)))

(defun agent-repl-open-tabs--ws-id (ws)
  "Return workspace WS\='s ref id, or nil when WS has none."
  (plist-get (agent-repl--ws-get ws :ref) :id))

(defun agent-repl-open-tabs--ws-row (ws)
  "Return workspace WS\='s row of the last roster walk, or nil."
  (let ((id (agent-repl-open-tabs--ws-id ws)))
    (and id (agent-repl-open-tabs--row id))))

(defun agent-repl-open-tabs-hidden-p (ws)
  "Return non-nil when the user hid workspace WS.
This editor holds a hidden workspace only while it is the selection
\(frontend.v1.RosterRowVisibility).  Read from the last roster walk; a workspace
it did not meet is not hidden."
  (let ((row (agent-repl-open-tabs--ws-row ws)))
    (and row (plist-get row :hidden) t)))

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

(defmacro agent-repl-open-tabs--say (template &rest named)
  "Say one line, TEMPLATE with NAMED, in the minibuffer and the log.
A log-site macro (`agent-repl--phase-echo' under this file's central
scope): TEMPLATE is a literal and NAMED its :name VALUE pairs."
  `(agent-repl--phase-echo agent-repl-open-tabs--central ,template ,@named))

(defun agent-repl-open-tabs--say-step (ws step)
  "Say the line for workspace WS's STEP, a decoded `OpenTabsStep.step' oneof."
  (let ((value (plist-get step :value)))
    (pcase (plist-get step :arm)
      (:starting-session (agent-repl-open-tabs--say "{ws}: starting session…" :ws ws))
      (:waking (agent-repl-open-tabs--say "{ws}: waking from sleep…" :ws ws))
      (:resuming (agent-repl-open-tabs--say "{ws}: resuming the conversation…" :ws ws))
      (:vendor-retrying
       (agent-repl-open-tabs--say "{ws}: Claude did not start, retrying (attempt {attempt})…"
                                  :ws ws :attempt (plist-get value :attempt)))
      (:vendor-rejected
       (agent-repl-open-tabs--say "{ws}: Claude refused to start: {start}." :ws ws :start (plist-get value :cause)))
      (:vendor-failed (agent-repl-open-tabs--say "{ws}: Claude failed to start after 10 minutes." :ws ws))
      (:cold-gate (agent-repl-open-tabs--say "{ws}: needs your answer to resume (large context)." :ws ws))
      (:offline (agent-repl-open-tabs--say "{ws}: offline, waiting for the network…" :ws ws))
      (:served (agent-repl-open-tabs--say "{ws}: session up, waiting for its page…" :ws ws))
      (:page-connected (agent-repl-open-tabs--say "{ws}: page connected, drawing the conversation…" :ws ws))
      (:conversation-read (agent-repl-open-tabs--say "{ws}: conversation read." :ws ws))
      (:resuming-cut-turn
       (agent-repl-open-tabs--say "{ws}: resuming the turn the restart cut…" :ws ws))
      (:page-drawn
       (agent-repl-open-tabs--say "{ws}: page drawn." :ws ws)
       (when (fboundp 'agent-repl-open-progress-note-loaded)
         (agent-repl-open-progress-note-loaded ws)))
      (arm (agent-repl--error agent-repl-open-tabs--central
                              "elisp.open-tabs.unknown-step arm={arm} ws={ws}" :arm (prin1-to-string arm) :ws ws)))))

;;;; ---- Asking -------------------------------------------------------------

(defun agent-repl-open-tabs--conn ()
  "Return the connection the roster rides, else the primary link, else nil.
The daemon that served the rows is the one asked to open them."
  (or (and (bound-and-true-p agent-repl-roster--stream)
           (agent-repl-connect-stream-conn agent-repl-roster--stream))
      (agent-repl-link-primary)))

(defun agent-repl-open-tabs--send-to-daemon (workspaces on-frame on-close &optional conn)
  "Send `OpenTabs' for WORKSPACES; ON-FRAME and ON-CLOSE take its answers.
CONN is the daemon asked when the workspaces are served by one other than
the roster's (`agent-repl-open-tabs--serving-conn'); the roster's
otherwise.  Returns nil, sending nothing, when there is no daemon
connection, and otherwise a function that cancels the request's stream."
  (let ((conn (or conn (agent-repl-open-tabs--conn))))
    (when conn
      (let ((stream (agent-repl-rpc-open-tabs conn workspaces on-frame on-close)))
        (lambda () (agent-repl-connect-stream-cancel stream))))))

(defun agent-repl-open-tabs--page (name hidden)
  "Make workspace NAME's slot unless it is HIDDEN; return the page arm.
`:loading' when the host holds NAME's page now (made here, or already
standing), `:none' when it is HIDDEN or holds no page: a slot is made with
a page wherever one is wanted (`agent-repl-page-unload-wanted-p': every
workspace held here but a cold-gated one, whose show holds it back).  A
page that FAILS to load is recorded at ERROR and asked `:none', so the
workspace still opens (its tab shows no page) and no other row's open is
lost."
  (unless hidden
    (condition-case err
        (progn
          (agent-repl--ensure-input-buffer name)
          (agent-repl--frontend-precreate-webview name))
      (error
       (agent-repl--error name "elisp.open-tabs.page-failed ws={ws} error={error}"
                          :ws name :error (error-message-string err)))))
  (list :arm (if (and (not hidden) (agent-repl-webhost-has-page-p name))
                 :loading
               :none)))

(defun agent-repl-open-tabs--ask (rows &optional conn)
  "Ask the daemon to open ROWS, loading each visible one's page first.
CONN, when given, is the daemon serving ROWS other than the roster's.
Every row is marked in flight; when the request cannot be sent they are
left unasked, and the next link-up or roster walk asks again."
  (let* ((startup (eq agent-repl-open-tabs--startup 'expected))
         (ids (mapcar (lambda (row) (plist-get row :id)) rows))
         (request (agent-repl-open-tabs--request-make))
         (workspaces
          (mapcar (lambda (row)
                    (list :workspace (plist-get row :ref)
                          :page (agent-repl-open-tabs--page (plist-get row :name)
                                                            (plist-get row :hidden))))
                  rows)))
    (dolist (id ids) (puthash id 'in-flight agent-repl-open-tabs--state))
    ;; THE PAGE WAITS ARE NOTED BEFORE THE SEND: a sender may answer before
    ;; it returns, and each answer clears its own.
    (cl-mapc (lambda (id w)
               (when (eq (plist-get (plist-get w :page) :arm) :loading)
                 (puthash id request agent-repl-open-tabs--page-waits)))
             ids workspaces)
    (agent-repl--info agent-repl-open-tabs--central
                      "elisp.open-tabs.ask workspaces={workspaces} startup={startup} pages={pages}"
                      :workspaces (length rows) :startup startup
                      :pages (prin1-to-string (cl-mapcar (lambda (row w)
                                   (cons (plist-get row :name)
                                         (plist-get (plist-get w :page) :arm)))
                                 rows workspaces)))
    (when startup
      (setq agent-repl-open-tabs--startup 'running)
      (agent-repl-open-tabs--say "opening {rows_count} workspaces…" :rows_count (length rows))
      (agent-repl-open-tabs--show-host-while-starting))
    (if-let ((cancel (apply agent-repl-open-tabs-sender workspaces
                            (lambda (frame) (agent-repl-open-tabs--on-frame frame startup))
                            (lambda (outcome)
                              (agent-repl-open-tabs--on-close ids outcome startup request))
                            (and conn (list conn)))))
        (progn
          (unless (functionp cancel)
            (agent-repl--fatal agent-repl-open-tabs--central
                               "elisp.open-tabs.sender-answered-no-cancel answer={answer}"
                               :answer (prin1-to-string cancel)))
          (setf (agent-repl-open-tabs--request-cancel request) cancel))
      (agent-repl-open-tabs--forget-page-waits ids request)
      (dolist (id ids) (remhash id agent-repl-open-tabs--state))
      (when startup (setq agent-repl-open-tabs--startup 'expected))
      (agent-repl--info agent-repl-open-tabs--central
                        "elisp.open-tabs.unsent workspaces={workspaces} reason=no-connection"
                        :workspaces (length rows)))))

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
                         "elisp.open-tabs.startup-view-skipped host={host} current={current}"
                         :host (and xw t) :current current)
      (let ((buf (or (get-buffer agent-repl-open-tabs--startup-view-name)
                     (agent-repl--frontend-make-slot-buffer
                      agent-repl-open-tabs--startup-view-name xw))))
        (set-window-buffer (selected-window) buf)
        (agent-repl--info agent-repl-open-tabs--central
                          "elisp.open-tabs.startup-view-shown current={current}" :current current)))))

(defun agent-repl-open-tabs--drop-startup-view ()
  "Drop the buffer that displayed the host while the startup chose."
  (when-let ((buf (get-buffer agent-repl-open-tabs--startup-view-name)))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buf))
    (agent-repl--info agent-repl-open-tabs--central "elisp.open-tabs.startup-view-dropped")))

(defun agent-repl-open-tabs--forget-page-waits (ids request)
  "Forget every page wait of IDS that REQUEST holds.
A later request may hold an id's wait by now; that one is kept."
  (dolist (id ids)
    (when (eq (gethash id agent-repl-open-tabs--page-waits) request)
      (remhash id agent-repl-open-tabs--page-waits))))

(defun agent-repl-open-tabs--page-removed (ws)
  "End the open waiting on WS's page, now that the host dropped that page.
The page will never draw, so the open would never be answered; its
stream's close asks again for every workspace it left unanswered
\(`agent-repl-open-tabs--on-close').  A page no open waits on ends nothing."
  (let* ((id (agent-repl-open-tabs--ws-id ws))
         (request (and id (gethash id agent-repl-open-tabs--page-waits))))
    (when request
      (remhash id agent-repl-open-tabs--page-waits)
      (let ((cancel (agent-repl-open-tabs--request-cancel request)))
        (unless cancel
          (agent-repl--fatal ws "elisp.open-tabs.page-wait-unsent ws={ws} id={id}"
                             :ws ws :id id))
        (agent-repl--info ws "elisp.open-tabs.page-dropped-while-asked ws={ws} id={id}"
                          :ws ws :id id)
        (setf (agent-repl-open-tabs--request-page-dropped request) t)
        (funcall cancel)))))

(add-hook 'agent-repl-webhost-page-removed-functions #'agent-repl-open-tabs--page-removed)

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

(defun agent-repl-open-tabs--withdraw (row)
  "Take open ROW's tab off the bar, now that the user hid it.
It drops its page and slot (`agent-repl-open-tabs--hide').  A COLLAPSED ROW
THIS EDITOR HOLDS IS ALWAYS HIDDEN: the daemon holds a folded workspace only
while it draws it inside its fold (surfaced, or the selection), whose tab is
numbered, and one it stops drawing is no longer held and is torn down
\(frontend.v1.RosterRowVisibility).  A collapsed, held row that is not hidden is
a broken contract, and is fatal."
  (unless (plist-get row :hidden)
    (agent-repl--fatal (plist-get row :name)
                       "elisp.open-tabs.withdraw-not-hidden ws={ws} id={id}"
                       :ws (plist-get row :name) :id (plist-get row :id)))
  (agent-repl-open-tabs--hide row))

(defun agent-repl-open-tabs--hide (row)
  "Drop open ROW's page, now that the user hid it: it shows nothing.
The tab goes dormant at once; the page goes once the daemon is told
\(`agent-repl-page-drop-announced'), so the workspace, which stays open
and may be running a turn, is never drawn severed.  A row shown again
while the report was in flight keeps its page."
  (let* ((name (plist-get row :name))
         (id (plist-get row :id))
         (buf (agent-repl--ws-get name :frontend-buffer)))
    (agent-repl-open-tabs--dormant id)
    (agent-repl--info name "elisp.open-tabs.hidden ws={ws} page={page}" :ws name :page (buffer-live-p buf))
    (when (buffer-live-p buf)
      (agent-repl-page-drop-announced
       name
       (lambda ()
         (and (eq (gethash id agent-repl-open-tabs--state) 'dormant)
              (eq (agent-repl--ws-get name :frontend-buffer) buf)
              (buffer-live-p buf)))
       (lambda ()
         (agent-repl--frontend-detach-webview name buf)
         (agent-repl--info name "elisp.open-tabs.page-dropped ws={ws}" :ws name))
       (lambda (why)
         (agent-repl--info name "elisp.open-tabs.page-kept ws={ws} why={why}" :ws name :why why))))))

(defun agent-repl-open-tabs-reconcile (rows)
  "Bring every open in line with the roster walk's ROWS, then ask for the owed.
ROWS are plists `(:id :name :ref :collapsed)', every workspace the roster
gives a tab, in tab order.  A workspace gone from them is forgotten; an
open one now off the bar is withdrawn (`agent-repl-open-tabs--withdraw');
every one never asked for, or shown again, is asked for in ONE request."
  (let ((wanted (make-hash-table :test 'equal)))
    (dolist (row rows) (puthash (plist-get row :id) t wanted))
    (dolist (id (hash-table-keys agent-repl-open-tabs--state))
      (unless (gethash id wanted)
        (remhash id agent-repl-open-tabs--state)
        (remhash id agent-repl-open-tabs--reattached-while-asked))))
  (setq agent-repl-open-tabs--rows rows)
  (dolist (row rows)
    (when (and (plist-get row :collapsed)
               (eq (gethash (plist-get row :id) agent-repl-open-tabs--state) 'open))
      (agent-repl-open-tabs--withdraw row)))
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
                              "elisp.open-tabs.unknown-frame arm={arm}" :arm (prin1-to-string arm))))))

(defun agent-repl-open-tabs--opened (opened)
  "Take one workspace's OPENED answer: draw its tab, or record why not."
  (let* ((ref (plist-get opened :workspace))
         (id (plist-get ref :id))
         (ws (agent-repl-open-tabs--name ref))
         (outcome (plist-get opened :outcome))
         (row (agent-repl-open-tabs--row id))
         (reattached (gethash id agent-repl-open-tabs--reattached-while-asked)))
    (remhash id agent-repl-open-tabs--reattached-while-asked)
    (remhash id agent-repl-open-tabs--page-waits)
    (pcase (plist-get outcome :arm)
      ((or :ready :failed)
       (cond
        ((null row)
         ;; Closed while it opened: the walk already forgot it.
         (agent-repl--info ws "elisp.open-tabs.opened-gone ws={ws} id={id}" :ws ws :id id))
        ((plist-get row :collapsed)
         ;; Folded or hidden while it opened: off the bar, and a hidden one
         ;; drops its page (the daemon stands it down once its cache lapses).
         (agent-repl-open-tabs--withdraw row))
        (t
         ;; THE ROW RIDES THE ANSWER: indexed before the tab is drawn, so it
         ;; is drawn with the workspace's status as of the answer.  A chat
         ;; thread has no row: the roster lists it in its thread section.
         (let* ((value (plist-get outcome :value))
                (listing (plist-get value :listing)))
           (when (eq (plist-get listing :arm) :row)
             (agent-repl-roster-note-row (plist-get listing :value)
                                         (or (plist-get value :roster-revision) 0))))
         (when (eq (plist-get outcome :arm) :failed)
           (agent-repl-open-tabs--say "{ws}: could not be opened: {opened}."
                                      :ws ws :opened (plist-get (plist-get outcome :value) :reason)))
         (agent-repl-open-tabs--show id ws))))
      (:hidden
       (when row (agent-repl-open-tabs--dormant id))
       (agent-repl--info ws "elisp.open-tabs.opened-hidden ws={ws} id={id}" :ws ws :id id)
       (agent-repl-open-tabs--maybe-land-startup))
      ((and :refused
            (guard (memq (agent-repl-open-tabs--cause-arm outcome)
                         agent-repl-open-tabs--handover-causes)))
       (agent-repl-open-tabs--handed-over row id ws outcome reattached))
      (:refused
       (when row (puthash id 'refused agent-repl-open-tabs--state))
       (agent-repl--error ws "elisp.open-tabs.refused ws={ws} id={id} cause={cause}"
                          :ws ws :id id :cause (prin1-to-string (plist-get outcome :value)))
       (agent-repl-open-tabs--say "{ws}: the daemon refused to open it ({cause})."
                                  :ws ws :cause (agent-repl-open-tabs--cause-word outcome))
       (agent-repl-open-tabs--maybe-land-startup))
      (arm (agent-repl--error ws "elisp.open-tabs.unknown-outcome arm={arm} ws={ws}" :arm (prin1-to-string arm) :ws ws)))))

(defun agent-repl-open-tabs--cause-arm (refused)
  "Return REFUSED's cause arm keyword, e.g. `:unknown-workspace'."
  (plist-get (plist-get (plist-get refused :value) :cause) :arm))

(defun agent-repl-open-tabs--cause-word (refused)
  "Return the words naming REFUSED's cause, e.g. \"unknown-workspace\"."
  (let ((arm (agent-repl-open-tabs--cause-arm refused)))
    (if (keywordp arm) (substring (symbol-name arm) 1) "unnamed")))

;;;; ---- The handover ---------------------------------------------------------

;; A HANDOVER REFUSAL IS NOT A FAILURE.  During a rollout the daemon asked can
;; have handed the workspace on (`transferring_away') or not taken it over yet
;; (`not_yet_adopted'), and a workspace in the middle of a turn stays with the
;; old daemon until the turn lets it go.  Held as `refused', its tab was held
;; for the rest of the Emacs session (2026-10-26: `charm-inference-sf17' was
;; adopted by the new daemon at 13:34 and its tab never appeared, while the
;; sidebar numbered it 7).  The workspace's REATTACH is the moment a daemon
;; serves it (`agent-repl-host-reattached-functions'), so it is asked for
;; again then: once per reattach, never on a walk, so a long handover is not
;; asked for on every roster push.

(defun agent-repl-open-tabs--handed-over (row id ws refused reattached)
  "Take workspace WS's (ref ID, walk ROW) handover refusal REFUSED.
REATTACHED is non-nil when WS was re-attached while the open was in flight:
there is no reattach left to wait for, so it is asked for again at once.

A `not_yet_adopted' NAMING THE DAEMON THAT SERVES WS opens WS there: WS is
re-attached to that daemon (`agent-repl-host-view-where-served'), whose
reattach asks for it again on that daemon, or it is asked for there at
once when WS already stands on it.  Its tab opens without waiting for the
handover, and the incumbent's `transferred' push moves it on later."
  (let ((address (agent-repl-open-tabs--serving-address refused)))
    (agent-repl--info ws "elisp.open-tabs.refused-handover ws={ws} id={id} cause={cause} reattached={reattached} serving={serving}"
                      :ws ws :id id :cause (agent-repl-open-tabs--cause-word refused) :reattached (and reattached t) :serving (prin1-to-string address))
    (when row
      (puthash id 'handover agent-repl-open-tabs--state)
      (cond
       (reattached (agent-repl-open-tabs--ask-again row "reattached-while-asked"))
       (address
        (unless (agent-repl-host-view-where-served ws address "open-tabs-not-yet-adopted")
          (agent-repl-open-tabs--ask-again row "served-elsewhere")))
       (t (agent-repl-open-tabs--say "{ws}: moving to the new daemon; its tab opens once it has." :ws ws))))
    (agent-repl-open-tabs--maybe-land-startup)))

(defun agent-repl-open-tabs--serving-address (refused)
  "Return the daemon address REFUSED\='s `not_yet_adopted' names, or nil."
  (agent-repl-host-serving-address (plist-get (plist-get refused :value) :cause)))

(defun agent-repl-open-tabs--serving-conn (ws)
  "Return the connection of the daemon serving WS when it is not the roster's.
A handover\='s incumbent serves WS until the joining daemon adopts it, and
WS\='s open goes there; nil when WS stands on the roster\='s daemon, or on
none."
  (let ((conn (agent-repl-host-conn ws))
        (roster (agent-repl-open-tabs--conn)))
    (and conn roster
         (agent-repl-connect-connection-alive-p conn)
         (not (equal (agent-repl-connect-connection-address conn)
                     (agent-repl-connect-connection-address roster)))
         conn)))

(defun agent-repl-open-tabs--ask-again (row why)
  "Ask again for ROW, held by a handover refusal; WHY is recorded."
  (agent-repl--info (plist-get row :name) "elisp.open-tabs.handover-ask-again ws={ws} id={id} why={why}"
                    :ws (plist-get row :name) :id (plist-get row :id) :why why)
  (remhash (plist-get row :id) agent-repl-open-tabs--state)
  (agent-repl-open-tabs--ask (list row) (agent-repl-open-tabs--serving-conn (plist-get row :name))))

(defun agent-repl-open-tabs--on-reattached (ws)
  "Workspace WS was re-attached to the daemon that serves it.
Held by a handover refusal, it is asked for again now; with its open in
flight, the refusal that may still come is asked for again on arrival."
  (let* ((row (agent-repl-open-tabs--ws-row ws))
         (id (plist-get row :id)))
    (pcase (and row (gethash id agent-repl-open-tabs--state))
      ('handover (agent-repl-open-tabs--ask-again row "reattached"))
      ('in-flight (puthash id t agent-repl-open-tabs--reattached-while-asked)))))

(add-hook 'agent-repl-host-reattached-functions #'agent-repl-open-tabs--on-reattached)

(defun agent-repl-open-tabs--drawn-open-p (name)
  "Return non-nil when workspace NAME's tab is answered open and drawn."
  (and (cl-find-if (lambda (row)
                     (and (equal (plist-get row :name) name)
                          (eq (gethash (plist-get row :id) agent-repl-open-tabs--state) 'open)))
                   agent-repl-open-tabs--rows)
       (member name (agent-repl-roster-drawn-tab-order))
       t))

(defun agent-repl-open-tabs--startup-landing ()
  "Return (WS . SOURCE): where the startup lands now, WS nil while it waits.
THE SELECTION BEFORE THE RESTART WINS.  The roster's `current' is the
workspace the user last selected, kept by the daemon across every restart,
so when it names a row of this walk that the bar draws and the daemon has
not refused, the startup waits for that tab (SOURCE `roster') and lands
on nothing else.  Only when the roster names no such row -- no selection
at all, or one that is hidden, refused (a handover included) or gone --
is the first drawn tab answered open taken (SOURCE `first-drawn'), so a
startup still never leaves the frame on no workspace."
  (let* ((id (and agent-repl-roster-view
                  (agent-repl-roster--current-id agent-repl-roster-view)))
         (row (and id (agent-repl-open-tabs--row id)))
         (name (and row (plist-get row :name))))
    (if (and row
             (not (plist-get row :collapsed))
             (not (memq (gethash id agent-repl-open-tabs--state) '(refused handover)))
             (member name (agent-repl-roster-drawn-tab-order)))
        (cons (and (agent-repl-open-tabs--drawn-open-p name) name) 'roster)
      (cons (cl-find-if #'agent-repl-open-tabs--drawn-open-p
                      (agent-repl-roster-drawn-tab-order))
            'first-drawn))))

(defun agent-repl-open-tabs--maybe-land-startup ()
  "Select the startup's landing (`agent-repl-open-tabs--startup-landing').
Only while the startup runs and has not landed yet; the selection is
asked of the daemon like every other switch.  Every answer that can
change the landing calls this: a tab drawn, and a tab answered hidden or
refused, which may have been the one the startup waited for."
  (when (agent-repl-open-tabs-choosing-p)
    (pcase-let ((`(,ws . ,source) (agent-repl-open-tabs--startup-landing)))
      (if (null ws)
          (agent-repl--log agent-repl-open-tabs--central
                           "elisp.open-tabs.startup-landing-waits source={source}" :source source)
        (agent-repl--info ws "elisp.open-tabs.startup-lands ws={ws} source={source}" :ws ws :source source)
        (setq agent-repl-open-tabs--startup-selected ws)
        (agent-repl-host-request-switch ws 'startup)))))

(defun agent-repl-open-tabs--show (id ws)
  "Draw workspace WS's tab (ref ID), now that it is open.
THE STARTUP LANDS once its landing is drawn
\(`agent-repl-open-tabs--maybe-land-startup'): the workspace selected
before the restart, else the first drawn tab, so a startup never shows
no workspace while the rest come up."
  (puthash id 'open agent-repl-open-tabs--state)
  (unless agent-repl-roster--walking
    (agent-repl-roster-refresh-order))
  (agent-repl--info ws "elisp.open-tabs.tab-opened ws={ws} id={id} choosing={choosing}"
                    :ws ws :id id :choosing (agent-repl-open-tabs-choosing-p))
  (when (agent-repl-open-tabs-starting-p)
    (agent-repl-startup-reach :first-tab))
  (agent-repl-open-tabs--say "{ws}: ready." :ws ws)
  (agent-repl-open-tabs--maybe-land-startup)
  ;; The selection may name this workspace, and had no tab to land on.
  (unless agent-repl-roster--walking
    (agent-repl-roster-apply-current)))

(defun agent-repl-open-tabs--say-finished (finished)
  "Say how the startup's FINISHED open went."
  (let ((failed (plist-get finished :failed))
        (total (plist-get finished :total)))
    (if (null failed)
        (agent-repl-open-tabs--say "all {total} workspaces ready." :total total)
      (agent-repl-open-tabs--say "{ready} of {total} workspaces ready; {failed} could not be opened."
                                 :ready (plist-get finished :ready) :total total
                                 :failed (mapconcat (lambda (f) (plist-get f :name)) failed ", ")))))

(defun agent-repl-open-tabs--on-close (ids outcome startup request)
  "The stream of REQUEST, the open of IDS, closed with OUTCOME.
STARTUP marks the first.  Every id it left unanswered is unasked again,
and asked for on the next link-up or roster walk; never at once, so a
daemon that is down is not dialed in a loop.  The one exception is a
stream this editor ended because a page it waited on was dropped
\(`agent-repl-open-tabs--page-removed'): its daemon is up, so its owed
are asked for at once."
  (agent-repl-open-tabs--forget-page-waits ids request)
  (let ((unanswered (cl-remove-if-not
                     (lambda (id) (eq (gethash id agent-repl-open-tabs--state) 'in-flight))
                     ids)))
    (dolist (id unanswered)
      (remhash id agent-repl-open-tabs--state)
      (remhash id agent-repl-open-tabs--reattached-while-asked))
    (if (and (eq (car outcome) :ended) (null unanswered))
        (agent-repl--log agent-repl-open-tabs--central "elisp.open-tabs.closed outcome=ended")
      ;; INFO: the open is not lost, only asked for again.
      (agent-repl--info agent-repl-open-tabs--central
                        "elisp.open-tabs.closed-unanswered outcome={outcome} unanswered={unanswered}"
                        :outcome (prin1-to-string outcome) :unanswered (prin1-to-string unanswered)))
    (when (and startup (agent-repl-open-tabs-starting-p))
      (agent-repl-open-tabs--end-startup "stream-ended"))
    (when (agent-repl-open-tabs--request-page-dropped request)
      (agent-repl--info agent-repl-open-tabs--central
                        "elisp.open-tabs.ask-again-page-dropped unanswered={unanswered}"
                        :unanswered (prin1-to-string unanswered))
      (agent-repl-open-tabs--ask-owed))))

(defun agent-repl-open-tabs--end-startup (why)
  "End this process's first open, for WHY, and arm Doom's one-time setup."
  (setq agent-repl-open-tabs--startup 'done)
  (agent-repl--info agent-repl-open-tabs--central "elisp.open-tabs.startup-ended why={why}" :why why)
  (agent-repl-open-tabs--drop-startup-view)
  (agent-repl-roster-apply-current)
  (agent-repl-startup-ended)
  (agent-repl-startup-first-hooks-arm))

(provide 'agent-repl-open-tabs)
;;; open-tabs.el ends here
