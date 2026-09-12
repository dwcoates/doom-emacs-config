;;; verbs.el --- the workspace and daemon-admin verbs -*- lexical-binding: t; -*-

;;; Commentary:

;; EMACS COMMANDS ARE THIN WRAPPERS.  Send the request, await the daemon's
;; ack, update editor state -- and that is the whole of it.  Every piece of
;; real machinery behind these verbs (git, worktrees, session lifecycle,
;; merge orchestration, naming, priority ordering) is the DAEMON's, and
;; every visible consequence arrives on the roster and host streams rather
;; than in the answer this file reads.  A verb here that started doing work
;; of its own would be re-growing the machinery the overhaul removed.
;;
;; THE VOCABULARY, renamed to match the contract: doom's old "kill" (remove
;; from the editor) is CLOSE; doom's old "nuke" is KILL; NUKE is reserved
;; for the one verb that destroys data.
;;
;;   CloseWorkspace   a VIEW act -- the tab goes, the session is UNTOUCHED.
;;                    Requires quiet; a refusal manifests in the WEBAPP
;;                    FOOTER, so Emacs draws no dialog and leaves the tab.
;;   KillWorkspace    forced session death.  Never blocks, never warns.
;;                    The worktree and branch survive.
;;   NukeWorkspace    DATA DESTRUCTION: worktree and branch deleted.  The
;;                    one verb this file confirms before sending.
;;   OpenWorkspace    opens a closed row; any revival is the daemon's.
;;   MergeWorkspace   success means ENQUEUED.  Emacs holds NO merge state.
;;   RestartWorkspace the daemon owns everything the restart entails.
;;   CreateWorkspace  the daemon names and creates everything; the caller
;;                    supplies facts and the workspace appears on the
;;                    roster.
;;
;; THREE ANSWER SHAPES, never collapsed into one another: a SUCCESS arm (do
;; the editor-state update), a daemon-authored ERROR arm (the daemon
;; refused -- report it, change nothing), and a TRANSPORT failure (nobody
;; answered -- report it louder, change nothing).  Treating a refusal as a
;; failure would tell the user the daemon is broken when it is working
;; exactly as designed.
;;
;; SUCCESS IS EMPTY wherever the new state arrives as a push, which is
;; almost everywhere.  Nothing here reads state out of a unary answer; the
;; streams are the authority.
;;
;; DAEMON-ADMIN VERBS (shutdown schedule, merge queue, the two health
;; pulls) are not host-natured at all: this file is merely today's caller
;; for them.  UNHEALTHY IS AN ANSWER -- it arrives inside success carrying
;; typed fault lists, and only a transport failure means no daemon.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--info "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--warn "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--error "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--next-log-request-id "agent-repl-core" ())
(declare-function agent-repl--with-log-context "agent-repl-core"
                  (workspace request-id function))
(defvar agent-repl--global-log-scope)
(defvar agent-repl--log-context-request-id)
(defvar agent-repl--log-context-workspace)
(declare-function agent-repl--ws-current-name "agent-repl-workspace" ())
(declare-function agent-repl--ws-current-log-name "agent-repl-workspace" ())
(declare-function agent-repl--ws-require-known "agent-repl-workspace" (ws context))
(declare-function agent-repl--live-ws-names "agent-repl-workspace" ())
(declare-function agent-repl--ws-get "agent-repl-workspace" (ws key))
(declare-function agent-repl--ws-by-ref-id "agent-repl-workspace" (id))
(declare-function agent-repl--pseudo-workspace-name-p "agent-repl-core" (ws))
(declare-function agent-repl--read-known-workspace "agent-repl-keybindings" (prompt))
(declare-function agent-repl--kill-one-workspace "agent-repl-workspace" (ws &optional preserve))
(declare-function agent-repl-host-ref "agent-repl-host" (ws))
(declare-function agent-repl-host-conn "agent-repl-host" (ws))
(declare-function agent-repl-host-faults "agent-repl-host" (ws))
(declare-function agent-repl-host-handle-refusal "agent-repl-host" (ws arm))
(declare-function agent-repl-host-take-restart-hold "agent-repl-host" (ws))
(declare-function agent-repl-host-release-restart-hold "agent-repl-host" (ws reason))
(declare-function agent-repl-switch-to-project "commands" (&optional project))
(declare-function agent-repl-link-primary "agent-repl-daemon-link" ())
(declare-function agent-repl-link-connect "agent-repl-daemon-link" ())
(declare-function agent-repl--read-input-buffer "agent-repl-input" (ws))
(declare-function agent-repl-rpc-close-workspace "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-kill-workspace "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-nuke-workspace "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-open-workspace "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-merge-workspace "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-restart-workspace "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-create-workspace "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-set-workspace-priority "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-update-shutdown-schedule "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-update-merge-queue "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-daemon-health "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-session-health "agent-repl-rpc" (conn request &rest keys))

;; Defined by roster.el.  Declared, never defined here.
(declare-function agent-repl-roster-repository-of "agent-repl-roster" (id &optional roster))
(defvar agent-repl-roster-view)

;; Defined by session.el.  Declared, never defined here.
(defvar agent-repl-interactive-model)


;;;; ---- Logging ----------------------------------------------------------
;;
;; A VERB LINE THAT IS ABOUT A WORKSPACE IS OWNED BY IT, and so is passed WS
;; as the record's owner: logging-contract.md routes an owned record into
;; that workspace's own `emacs.log', and failing to resolve a workspace for
;; an owned record is a routing invariant violation rather than permission
;; to write it globally.  The global sink is for the lines that genuinely
;; have no workspace -- the daemon-admin verbs and the health pull.
;;
;; THE SLUG NAMES THE VERB (`elisp.verbs.<op>-refused'), because the reader
;; of a refusal wants every refusal of ONE verb rather than a full-text
;; search over the context of a slug all of them share.

;;;; ---- Resolution -------------------------------------------------------

(defun agent-repl-verbs--ref (ws)
  "Return WS's daemon-minted `WorkspaceRef', or refuse.
The ref is an ECHO TOKEN obtained from RegisterWorkspace's success or from
the roster; there is no spelling of it Emacs could construct from a path,
so a workspace without one cannot be addressed at all."
  (agent-repl--ws-require-known ws "verb workspace resolution")
  (or (agent-repl-host-ref ws)
      (progn
        (agent-repl--warn ws "elisp.verbs.no-ref ws=%s" ws)
        (user-error "agent-repl: workspace %s has no daemon identity yet" ws))))

(defun agent-repl-verbs--registered-ws-names ()
  "Return the live registry entries that are really agent-repl workspaces.
persp-mode's and Doom's own perspectives (`persp-nil-name', \"main\") can
be live registry entries while intentionally owning no `:project-dir'
(see `agent-repl--pseudo-workspace-name-p'), and a verb must never act on
one: it is not a workspace, so there is nothing for the daemon to address."
  (cl-remove-if-not
   (lambda (ws)
     (and (not (agent-repl--pseudo-workspace-name-p ws))
          (agent-repl--ws-get ws :project-dir)
          t))
   (agent-repl--live-ws-names)))

(defun agent-repl-verbs--target-ws (ws prompt)
  "Return the workspace a verb should act on, or refuse.
An explicit WS is taken verbatim.  Otherwise the current perspective is
used when it is a registered workspace; when it is one of persp-mode's own
perspectives the user PICKS from the registered ones rather than having the
verb act on \"main\", and when nothing is registered at all the verb refuses
with the picker's own message before any wire call is made."
  (or ws
      (let ((current (agent-repl--ws-current-name))
            (known (agent-repl-verbs--registered-ws-names)))
        (unless known
          (agent-repl--warn current "elisp.verbs.no-workspaces-registered prompt=%S" prompt)
          (user-error "No agent-repl workspaces registered"))
        (if (member current known)
            current
          (agent-repl--read-known-workspace prompt)))))

(defun agent-repl-verbs--conn (&optional ws)
  "Return the connection to address WS on, or the primary link.
A workspace's own connection is preferred because during a blue-green
handover the two daemons own different workspaces, and a per-workspace
call must reach the daemon that owns THAT workspace."
  (or (and ws (agent-repl-host-conn ws))
      (agent-repl-link-primary)
      ;; A daemon-admin verb can be the FIRST thing that needs the link --
      ;; nothing about `SPC' on a fresh frame guarantees one is already
      ;; standing -- so the link is stood here rather than refused.
      (agent-repl-link-connect)
      (progn
        (agent-repl--warn ws "elisp.verbs.no-conn ws=%s" ws)
        (user-error "agent-repl: no daemon connection"))))


;;;; ---- Oneof plists ------------------------------------------------------
;;
;; THE CALLER SPELLS AN ARM FLAT, the codec takes it NESTED.  A verb's
;; arguments are written by a human at a keybinding -- `(:arm :evict
;; :workspace REF)' -- while the encoders take the representation the whole
;; codec shares, `(:arm KEYWORD :value FIELDS)' with a nil VALUE for an empty
;; arm.  The translation belongs HERE, at the verb boundary, so neither side
;; has to hold the other's convention.

(defun agent-repl-verbs--arm (flat)
  "Return the codec oneof plist for FLAT, or nil when FLAT is nil.
FLAT is `(:arm KEYWORD . FIELDS)'; the answer is `(:arm KEYWORD :value
FIELDS)', whose VALUE is nil exactly when the arm carries nothing."
  (when flat
    (let ((fields nil))
      (cl-loop for (key value) on flat by #'cddr
               unless (eq key :arm)
               do (setq fields (append fields (list key value))))
      (list :arm (plist-get flat :arm) :value fields))))

(defun agent-repl-verbs--level-arm (level)
  "Return the `WorkspacePriority' oneof for the bare LEVEL keyword, or nil.
The levels carry nothing at all, so the keyword IS the whole priority."
  (when level (list :arm level :value nil)))

;;;; ---- Refusals --------------------------------------------------------
;;
;; EVERY `<Rpc>Error' Emacs calls spells its refusal the same way: a `cause'
;; oneof whose ARM IS THE REASON, with whatever prose only that reason can
;; carry living inside the arm that owns it.  So the handling here is
;; ARM-GENERIC -- the arm keyword and its own fields go into the log context
;; and into the message -- and there is deliberately no per-arm table to
;; keep in step with the contract.  A new refusal arm reaches the user
;; correctly the day the daemon starts sending it.
;;
;; TWO ARMS ARE NOT REFUSALS TO SHOW THE USER.  `transferring_away' and
;; `not_yet_adopted' are the handover's ordering enforced BY REFUSAL: the
;; first says this daemon released the workspace and names the successor,
;; the second says the successor has not finished adopting it.  Both mean a
;; lagging client should self-heal, and both are handed to host.el's
;; handover path.  Reporting them would be telling the user something went
;; wrong during the one rollout that is supposed to be invisible.

(defconst agent-repl-verbs--handover-arms '(:transferring-away :not-yet-adopted)
  "Refusal arms that are HANDOVER SIGNALS rather than user-facing failures.
Handed to host.el's handover path; nothing is drawn for either.")

(defun agent-repl-verbs--refusal-arm (value)
  "Return VALUE's refusal oneof `(:arm KEYWORD :value FIELDS)'.
Every error message Emacs decodes spells the oneof `cause', so one
accessor serves them all."
  (plist-get value :cause))

(defun agent-repl-verbs--refusal-fields (arm)
  "Render ARM's own fields for a log context and a message, or the empty string.
An empty arm renders as nothing: being set is its whole assertion, and
appending an empty pair of parentheses would suggest otherwise."
  (let ((fields (plist-get arm :value)))
    (if fields (format " %S" fields) "")))

(defun agent-repl-verbs--on-refusal (ws op value)
  "Report a daemon-authored refusal of OP for WS, or route it to the handover.
The two handover arms are silent by design.  Every other arm is logged at
WARNING with its keyword and its own fields in the context, and drawn to
the user as the verb, the word refused, the arm keyword, and the fields."
  (let* ((arm (agent-repl-verbs--refusal-arm value))
         (keyword (plist-get arm :arm)))
    (cond
     ((memq keyword agent-repl-verbs--handover-arms)
      (agent-repl--info ws (format "elisp.verbs.%s-handover-refusal ws=%%s arm=%%S fields=%%S" op)
                        ws keyword (plist-get arm :value))
      (agent-repl-host-handle-refusal ws arm))
     (t
      ;; THE SLUG NAMES THE VERB: `elisp.verbs.<op>-refused'.  A refusal
      ;; reader wants every refusal of ONE verb, and a slug shared by all of
      ;; them would make that a full-text search over the context instead.
      (agent-repl--warn ws (format "elisp.verbs.%s-refused ws=%%s arm=%%S fields=%%S" op)
                        ws keyword (plist-get arm :value))
      (message "%s refused: %s%s" op
               (if keyword (substring (symbol-name keyword) 1) "unstated")
               (agent-repl-verbs--refusal-fields arm))))))

;;;; ---- The one dispatcher ----------------------------------------------

(cl-defun agent-repl-verbs--send (rpc conn request &key ws op on-success on-error)
  "Send REQUEST through RPC on CONN and dispatch the three answer shapes.
OP names the verb for the log.  ON-SUCCESS receives the decoded success
value and is the ONLY place editor state changes.  ON-ERROR, when given,
receives the decoded error value and OWNS the reporting for that verb;
without it a daemon-authored refusal is reported generically.  A transport
failure never reaches ON-ERROR: nobody answering and the daemon refusing
are different facts."
  (let ((request-id (or (plist-get request :idempotency-key)
                        (agent-repl--next-log-request-id)))
        (log-ws (or ws agent-repl--global-log-scope)))
    (agent-repl--with-log-context
     log-ws request-id
     (lambda ()
       (agent-repl--info ws "elisp.verbs.send op=%s ws=%s" op ws)
       (funcall rpc conn request
                :on-response
                (lambda (response)
                  (agent-repl--with-log-context
                   log-ws request-id
                   (lambda ()
                     (pcase (plist-get response :arm)
                       (:success
                        (agent-repl--info ws "elisp.verbs.ack op=%s ws=%s outcome=success" op ws)
                        (when on-success (funcall on-success (plist-get response :value))))
                       (:error
                        ;; ON-ERROR, when given, may CLAIM the arm (answering
                        ;; non-nil); anything it does not claim falls through to the
                        ;; arm-generic handling, so a verb with a special case for
                        ;; one arm still reports every other arm correctly.
                        (let ((value (plist-get response :value)))
                          (unless (and on-error (funcall on-error value))
                            (agent-repl-verbs--on-refusal ws op value))))
                       (arm
                        (agent-repl--error
                         ws "elisp.verbs.unknown-response-arm op=%s ws=%s arm=%S"
                         op ws arm))))))
                :on-failure
                (lambda (detail)
                  (agent-repl--with-log-context
                   log-ws request-id
                   (lambda ()
                     (agent-repl--error
                      ws "elisp.verbs.transport-failure op=%s ws=%s detail=%S"
                      op ws detail)
                     (message "agent-repl: %s failed -- the daemon did not answer" op)))))))))

;;;; ---- Editor-state updates ---------------------------------------------

(defun agent-repl-verbs--teardown-tab (ws op)
  "Tear WS's tab down after OP succeeded.
Idempotent, and deliberately so: the roster's own reconciliation tears the
same tab down when the closed row arrives, and whichever gets there first
must leave the other a no-op."
  (agent-repl--info ws "elisp.verbs.teardown ws=%s op=%s" ws op)
  (agent-repl--kill-one-workspace ws))

;;;; ---- Roster reading ---------------------------------------------------
;;
;; The roster is READ-ONLY for Emacs: the daemon authors it wholesale and
;; these helpers only pick values out of the last decoded view.

(defun agent-repl-verbs--row-ref (row)
  "Return ROW's `WorkspaceRef' -- the join key every gesture travels with."
  (plist-get (plist-get row :workspace) :workspace))

(defun agent-repl-verbs--row-name (row)
  "Return ROW's display name.  Never an identity: names collide across repos."
  (plist-get (plist-get row :name) :text))

(defun agent-repl-verbs--row-closed-p (row)
  "Return non-nil when ROW is closed."
  (plist-get (plist-get row :closed) :closed))

(defun agent-repl-verbs--flatten-rows (rows)
  "Return ROWS and every descendant, depth-first, in walk order."
  (apply #'append
         (mapcar (lambda (row)
                   (cons row (agent-repl-verbs--flatten-rows (plist-get row :children))))
                 rows)))

(defun agent-repl-verbs--all-rows (&optional roster)
  "Return every row of ROSTER (default `agent-repl-roster-view'), in walk order.
Repository sections depth-first, then the recently-merged section."
  (let ((roster (or roster (and (boundp 'agent-repl-roster-view) agent-repl-roster-view))))
    (append
     (apply #'append
            (mapcar (lambda (section)
                      (agent-repl-verbs--flatten-rows
                       (plist-get (plist-get section :rows) :rows)))
                    (plist-get (plist-get roster :repository) :sections)))
     (agent-repl-verbs--flatten-rows
      (plist-get (plist-get (plist-get roster :recently-merged) :rows) :rows)))))

(defun agent-repl-verbs--closed-rows (&optional roster)
  "Return the CLOSED rows of ROSTER -- exactly what OpenWorkspace can reopen."
  (cl-remove-if-not #'agent-repl-verbs--row-closed-p
                    (agent-repl-verbs--all-rows roster)))

(defun agent-repl-verbs--repo-sections (&optional roster)
  "Return ROSTER's repository sections, in the resolver's order."
  (let ((roster (or roster (and (boundp 'agent-repl-roster-view) agent-repl-roster-view))))
    (plist-get (plist-get roster :repository) :sections)))

(defun agent-repl-verbs--section-ref (section)
  "Return SECTION's `RepositoryRef' -- the echo token CreateWorkspace takes."
  (plist-get (plist-get section :key) :repository))

(defun agent-repl-verbs--section-label (section)
  "Return SECTION's drawn label."
  (plist-get (plist-get (plist-get section :header) :label) :text))

(defun agent-repl-verbs--section-of-ws (ws &optional roster)
  "Return the repository section WS's row sits in, or nil.
The default repository for a create started from inside a workspace: the
obvious answer is the repo the user is already working in."
  (let ((ref (agent-repl-host-ref ws)))
    (when ref
      (cl-find-if
       (lambda (section)
         (cl-find-if (lambda (row)
                       (equal (plist-get (agent-repl-verbs--row-ref row) :id)
                              (plist-get ref :id)))
                     (agent-repl-verbs--flatten-rows
                      (plist-get (plist-get section :rows) :rows))))
       (agent-repl-verbs--repo-sections roster)))))

;;;; ---- The workspace verbs ----------------------------------------------

(defun agent-repl-verb-close (ws)
  "Close WS: a VIEW act.  The daemon-shim session is untouched.
A BLOCKED refusal draws NO dialog and leaves the tab in place.  The
reasons are composed by the daemon onto the workspace footer, which is
where the user reads them; the refusal also carries the daemon\='s own
`summary\=' sentence, echoed to the echo area so a caller with no footer in
front of it can still say why."
  (let ((ref (agent-repl-verbs--ref ws)))
    (agent-repl-verbs--send
     #'agent-repl-rpc-close-workspace (agent-repl-verbs--conn ws)
     (list :workspace ref)
     :ws ws :op "close"
     :on-success (lambda (_) (agent-repl-verbs--teardown-tab ws "close"))
     :on-error
     (lambda (value)
       ;; `blocked' is the ONE arm this verb draws itself, because its
       ;; treatment is prescribed: no dialog, the tab stays, and the reasons
       ;; are read from the footer the daemon composed them onto.  Every
       ;; other arm is not claimed, and falls through to the generic
       ;; refusal handling.
       (let ((refusal (agent-repl-verbs--refusal-arm value)))
         (when (eq (plist-get refusal :arm) :blocked)
           (let* ((blocked (plist-get refusal :value))
                  (summary (plist-get blocked :summary)))
             (agent-repl--info
              ws "elisp.verbs.close-blocked ws=%s turn=%S live=%S held=%S merge=%S"
              ws (plist-get blocked :turn-in-flight) (plist-get blocked :live-work)
              (plist-get blocked :held-prompts) (plist-get blocked :merge-queued))
             ;; The daemon composes the sentence; when it sent one it is
             ;; echoed verbatim, so a user with no footer in front of them
             ;; still reads WHY.  Without one the footer stays the answer.
             (message "close blocked -- %s"
                      (if (and (stringp summary) (not (string-empty-p summary)))
                          summary
                        "see the workspace footer")))
           t))))))

(defun agent-repl-verb-kill (ws)
  "Kill WS's session by force.  The worktree and branch survive."
  (let ((ref (agent-repl-verbs--ref ws)))
    (agent-repl-verbs--send
     #'agent-repl-rpc-kill-workspace (agent-repl-verbs--conn ws)
     (list :workspace ref)
     :ws ws :op "kill"
     :on-success (lambda (_) (agent-repl-verbs--teardown-tab ws "kill")))))

(defun agent-repl-verb-nuke (ws)
  "Destroy WS: its session, its worktree and its branch.  Unrecoverable."
  (let ((ref (agent-repl-verbs--ref ws)))
    (agent-repl-verbs--send
     #'agent-repl-rpc-nuke-workspace (agent-repl-verbs--conn ws)
     (list :workspace ref)
     :ws ws :op "nuke"
     :on-success (lambda (_) (agent-repl-verbs--teardown-tab ws "nuke")))))

(defun agent-repl-verb-open (ref)
  "Open the closed workspace named by REF.  Any revival is the daemon's.
The tab arrives through the roster push, not through this answer."
  (agent-repl-verbs--send
   #'agent-repl-rpc-open-workspace (agent-repl-verbs--conn)
   (list :workspace ref)
   :op "open"
   :on-success (lambda (_) (message "agent-repl: opening %s" (plist-get ref :dir)))))

(defun agent-repl-verb-merge (ws)
  "Enqueue WS's merge.  Success means ENQUEUED and nothing more.
Emacs holds NO merge state whatsoever: the merge's whole life from the
queue onward is the feed's merge bubble and the roster."
  (let ((ref (agent-repl-verbs--ref ws)))
    (agent-repl-verbs--send
     #'agent-repl-rpc-merge-workspace (agent-repl-verbs--conn ws)
     (list :workspace ref)
     :ws ws :op "merge"
     :on-success (lambda (_) (message "merge enqueued")))))

(defun agent-repl-verb-restart (ws force)
  "Restart WS's session; FORCE interrupts the live turn first.
The DAEMON owns everything the restart entails, the webview bounce
included.  Forced restarts do NOT resume the agent afterwards: continuing
is the user's next prompt.

A FORCED restart CLOSES THE COMPOSER THE MOMENT IT IS SENT, rather than
waiting for the `restarting' arm to come back over the host stream.  The
send is async and the bounce is later still, so between the two the gate
would otherwise read the dying generation's `:open' and take a prompt the
daemon is about to refuse (`agent-repl-host-take-restart-hold').  A
REFUSED restart bounces nothing, so its arm releases the hold again; a
graceful restart is scheduled rather than immediate and takes none."
  (let ((ref (agent-repl-verbs--ref ws)))
    (when force (agent-repl-host-take-restart-hold ws))
    (agent-repl-verbs--send
     #'agent-repl-rpc-restart-workspace (agent-repl-verbs--conn ws)
     (list :workspace ref :force (and force t))
     :ws ws :op "restart"
     :on-success
     (lambda (_) (message "agent-repl: restart %s" (if force "under way" "scheduled")))
     :on-error
     (lambda (_)
       ;; The arm is NOT claimed (nil): the refusal still reports itself
       ;; through the generic path.  All this arm does is give back a hold
       ;; taken for a bounce that will never happen.
       (when force (agent-repl-host-release-restart-hold ws "restart-refused"))
       nil))))

(defun agent-repl-verb-set-priority (ws priority)
  "Set WS's PRIORITY, or CLEAR it when PRIORITY is nil.
PRIORITY is the BARE level keyword -- `:p05', `:p1', `:p2' or `:p3' -- and
the level arm is built here.  Clearing is the ABSENCE of the field, never a
sentinel level, so a nil priority omits it from the request entirely."
  (let ((ref (agent-repl-verbs--ref ws)))
    (agent-repl-verbs--send
     #'agent-repl-rpc-set-workspace-priority (agent-repl-verbs--conn ws)
     (list :workspace ref :priority (agent-repl-verbs--level-arm priority))
     :ws ws :op "set-priority"
     :on-success
     (lambda (_)
       (message "agent-repl: priority %s" (or priority "cleared"))))))

(cl-defun agent-repl-verb-create (repository form
                                             &key initial-prompt base-ref name
                                             merge-actions
                                             prompt finish self-certified add-to-merge-queue
                                             parent fork model priority allow-ungated
                                             select)
  "Create a workspace in REPOSITORY under FORM.
FORM is the creation form\'s BARE ARM KEYWORD -- `:standard' or
`:one-shot' -- and that arm\'s own fields ride as keyword arguments beside
it, because a caller at a keybinding writes facts, not nested oneofs.

  `:standard\' takes INITIAL-PROMPT (plain text, wrapped as `UserSaid\'),
    BASE-REF, NAME and MERGE-ACTIONS; every one of them is optional, and
    each absence is the statement the proto asks for -- an empty
    workspace, the repo\'s default branch resolution, a daemon-minted
    name, no configured merge actions.  MERGE-ACTIONS is spelled in plain
    text too: `(:before-ws-merge TEXT :postprocessing-prompt TEXT)\',
    either half optional, each wrapped as a `UserSaid\' here.
  `:one-shot\' takes PROMPT (required -- a one-shot IS its prompt) and
    FINISH, itself a bare arm keyword: `:self-merge\', or `:open-pr\' with
    SELF-CERTIFIED and ADD-TO-MERGE-QUEUE, two plain bools whose false is a
    VALUE the daemon must receive.

PARENT is the parent\'s `WorkspaceRef\' and FORK the presence-only fork
fact, which lives INSIDE the parent by construction -- a fork without a
parent is unrepresentable.  MODEL, PRIORITY (a bare level keyword) and
ALLOW-UNGATED are the remaining creation facts and are omitted when nil.

FORK WITHOUT PARENT IS REFUSED BEFORE THE SEND, with no rpc issued: the
fork fact lives inside the parent by construction, so there is no request
that carries it alone -- encoding one would silently drop the fork and
create a plain workspace the caller never asked for.

SELECT makes the new workspace CURRENT once the daemon answers.  The
answer is the one place the minted identity exists before the roster push
carries it: `CreateWorkspaceSuccess.workspace\' exists precisely because
callers learn the identity from the response.  The selection is the SAME
step `agent-repl-add-project-workspace\' takes after registering a
directory -- `agent-repl-switch-to-project\' on the minted dir -- and
reusing it is what keeps standing on a workspace you just made ONE
behavior rather than two.  It is OFF by default because a one-shot is
fire-and-forget and must not steal the user\'s place.

Nothing else happens on success: THE DAEMON names and creates everything,
and the new workspace\'s tab arrives through the roster push."
  (when (and fork (null parent))
    (agent-repl--error '(:agent-repl-central "workspace creation and daemon administration can precede workspace ownership") "elisp.verbs.create-fork-without-parent repository=%S form=%S"
                       repository form)
    (user-error "agent-repl: a fork needs a parent workspace"))
  (agent-repl-verbs--send
   #'agent-repl-rpc-create-workspace (agent-repl-verbs--conn)
   (list :repository repository
         :form (agent-repl-verbs--create-form
                form :initial-prompt initial-prompt :base-ref base-ref :name name
                :merge-actions merge-actions
                :prompt prompt :finish finish
                :self-certified self-certified :add-to-merge-queue add-to-merge-queue)
         :parent (when parent
                   (append (list :workspace parent) (when fork (list :fork fork))))
         :model model
         :priority (agent-repl-verbs--level-arm priority)
         :allow-ungated allow-ungated)
   :op "create"
   :on-success
   (lambda (success)
     (message "agent-repl: workspace requested")
     (when select
       (agent-repl-verbs--select-created success)))))

(defun agent-repl-verbs-select-minted (ref)
  "Stand on the workspace REF names, once the daemon has minted it.

THE ANSWER IS WHEN THE WORKSPACE EXISTS.  A minted `WorkspaceRef\' is the
one place the identity is known before the roster push carries it, so
every verb that stands on a workspace it just asked for stands on it HERE
-- `CreateWorkspaceSuccess.workspace\' for create and fork,
`RegisterWorkspace\''s ref for onboarding a directory.  Standing on the
directory any earlier is standing on a dir that is not a workspace yet:
`agent-repl-switch-to-project\' finds nothing to arm and Doom\'s
empty-project fallback lands the user on magit status instead of the
workspace\'s own panel.

The ref\'s `dir\' is the minted worktree, which is exactly what
`agent-repl-switch-to-project\' takes -- its argument is documented as a
PROJECT ROOT PATH.  A ref carrying no dir is REPORTED, never silently
skipped: the decoder already refuses a success without the ref, so a
missing dir is a contract breach and the user is owed the reason their
new workspace did not come up."
  (let ((dir (plist-get ref :dir))
        (id (plist-get ref :id)))
    (if (and dir (not (string-empty-p dir)))
        (let ((ws (agent-repl--ws-by-ref-id id)))
          (agent-repl--info
           (if ws
               ws
             '(:agent-repl-central
               "a minted workspace awaiting its roster row has no sink"))
           "elisp.verbs.select-minted dir=%s" dir)
          (if ws
              (agent-repl-verbs--land-on ref "already-a-tab" ws)
            ;; THE TAB IS NOT HERE YET.  The minted ref is the daemon's
            ;; ANSWER, and the workspace itself reaches Emacs on the ROSTER
            ;; stream -- `agent-repl-roster--open-tab' is the whole of its
            ;; editor-side birth.  The answer beats that push, so landing
            ;; here found no workspace at the directory, armed no panels
            ;; (`agent-repl--switch-project-arm-panels' logged
            ;; branch=not-a-workspace) and Doom's empty-project fallback put
            ;; MAGIT STATUS in the main area instead of the new workspace's
            ;; own panel.  Whether the push won that race decided whether a
            ;; freshly made workspace came up on itself at all.
            ;;
            ;; So the landing WAITS FOR THE TAB, exactly the way
            ;; `agent-repl--ffw-pending-fire' waits for one: the arrival of
            ;; the tab fires it. Nothing here polls, retries or schedules --
            ;; a tab that never arrives simply leaves the landing pending.
            (agent-repl-verbs--pending-landing-register ref)))
      (agent-repl--error '(:agent-repl-central
                           "a malformed minted reference owns no workspace sink")
                         "elisp.verbs.select-minted-no-dir ref=%S" ref)
      (message "agent-repl: the new workspace carries no directory to switch to"))))

(defvar agent-repl-verbs--pending-landing nil
  "The `WorkspaceRef' of a minted workspace whose tab has not arrived yet.
LATEST WINS, and there is only ever one: a landing moves the user, and
the user stands in one place -- so a second mint supersedes a first that
is still waiting rather than queueing behind it.  Cleared the moment it
fires, so nothing here outlives the arrival it waits on.")

(defun agent-repl-verbs--land-on (ref why ws)
  "Stand on workspace WS named by REF, recording WHY the landing ran now."
  (let ((dir (plist-get ref :dir)))
    (agent-repl--info ws "elisp.verbs.land-on ws=%s dir=%s why=%s" ws dir why)
    (agent-repl-switch-to-project dir)))

(defun agent-repl-verbs--pending-landing-register (ref)
  "Record REF as the landing waiting for its tab to reach the roster."
  (agent-repl--info '(:agent-repl-central "workspace creation and daemon administration can precede workspace ownership") "elisp.verbs.pending-landing-registered dir=%s id=%s"
                    (plist-get ref :dir) (plist-get ref :id))
  (setq agent-repl-verbs--pending-landing ref))

(defun agent-repl-verbs--pending-landing-fire (&rest _)
  "Land on the pending minted workspace once its tab exists.
Installed on `agent-repl-roster-update-functions', the hook the roster
runs after it has reconciled a push into tabs, so the ARRIVAL of the tab
is what lands the user.  Fires exactly once: the pending ref is cleared
before the landing runs."
  (when-let* ((ref agent-repl-verbs--pending-landing)
              (id (plist-get ref :id)))
    (let ((ws (agent-repl--ws-by-ref-id id)))
      (if ws
          (progn
            (setq agent-repl-verbs--pending-landing nil)
            (agent-repl--info ws "elisp.verbs.pending-landing-arrived ws=%s id=%s" ws id)
            (agent-repl-verbs--land-on ref "tab-arrived" ws))
        ;; A LANDING THAT IS STILL WAITING SAYS SO.  Nothing here polls or
        ;; times out -- a tab that never arrives simply leaves the landing
        ;; pending -- and for as long as that lasted the log went silent
        ;; between `pending-landing-registered\=' and nothing at all: a
        ;; register whose row came back CLOSED drew no tab, and three minutes
        ;; of waiting left no record saying what was being waited for.  Once
        ;; per push, which is why it is DEBUG.
        (agent-repl--log '(:agent-repl-central
                           "a landing whose tab has not arrived owns no workspace sink")
                         "elisp.verbs.pending-landing-waiting id=%s dir=%s"
                         id (plist-get ref :dir))))))

;; `agent-repl-roster-update-functions' is roster.el's, and roster.el loads
;; AFTER this file (config.el); `add-hook' binds the symbol itself, and a
;; later `defvar' leaves an already-bound variable alone, so the
;; registration holds either way.
(defvar agent-repl-roster-update-functions)
(add-hook 'agent-repl-roster-update-functions #'agent-repl-verbs--pending-landing-fire)

(defun agent-repl-verbs--select-created (success)
  "Stand on the workspace CreateWorkspaceSuccess names.
The identity lives in the success\'s own ref, which is handed to
`agent-repl-verbs-select-minted\' -- the one landing every minted
workspace comes up through."
  (agent-repl-verbs-select-minted (plist-get success :workspace)))

(cl-defun agent-repl-verbs--create-form (form &key initial-prompt base-ref name
                                              merge-actions prompt finish
                                              self-certified add-to-merge-queue)
  "Return the creation-form oneof for the bare arm keyword FORM.
The two forms take disjoint facts, so each arm reads only its own."
  (pcase form
    (:standard
     (list :arm :standard
           :value (list :initial-prompt (and initial-prompt
                                             (agent-repl-verbs--said initial-prompt))
                        :base-ref base-ref
                        :name name
                        :merge-actions (agent-repl-verbs--merge-actions merge-actions))))
    (:one-shot
     (list :arm :one-shot
           :value (list :prompt (and prompt (agent-repl-verbs--said prompt))
                        :finish (agent-repl-verbs--finish-arm
                                 finish self-certified add-to-merge-queue))))
    (_ (user-error "agent-repl: unknown creation form %S" form))))

(defun agent-repl-verbs--merge-actions (actions)
  "Return the `CreateWorkspaceMergeActions' plist for the plain-text ACTIONS.
ACTIONS is `(:before-ws-merge TEXT :postprocessing-prompt TEXT)', either
half optional; each present half is wrapped as a `UserSaid' here, because
a caller at a keybinding writes prose rather than content blocks.  Nil
ACTIONS, and ACTIONS with neither half, are the ABSENCE of any configured
action -- never an empty message."
  (let ((before (plist-get actions :before-ws-merge))
        (post (plist-get actions :postprocessing-prompt)))
    (when (or before post)
      (list :before-ws-merge (and before (agent-repl-verbs--said before))
            :postprocessing-prompt (and post (agent-repl-verbs--said post))))))

(defun agent-repl-verbs--finish-arm (finish self-certified add-to-merge-queue)
  "Return the one-shot finish oneof for the bare arm keyword FINISH.
`open_pr\'s two bools are stated explicitly, false included: they change
what happens to the branch, so neither may ride as an absence."
  (pcase finish
    ('nil nil)
    (:self-merge (list :arm :self-merge :value nil))
    (:open-pr (list :arm :open-pr
                    :value (list :self-certified (and self-certified t)
                                 :add-to-merge-queue (and add-to-merge-queue t))))
    (_ (user-error "agent-repl: unknown one-shot finish %S" finish))))

;;;; ---- The daemon-admin verbs -------------------------------------------

(defun agent-repl-verbs--shutdown-action (action)
  "Return the codec action oneof for the FLAT shutdown ACTION.
The `DrainReason' nested inside `schedule' and `now' is spelled flat too,
so it is translated on the way past rather than left half-converted."
  (let* ((oneof (agent-repl-verbs--arm action))
         (fields (plist-get oneof :value)))
    (if (plist-get fields :reason)
        (list :arm (plist-get oneof :arm)
              :value (plist-put (copy-sequence fields) :reason
                                (agent-repl-verbs--arm (plist-get fields :reason))))
      oneof)))

(defun agent-repl-verb-shutdown-schedule (action)
  "Send ACTION to UpdateShutdownSchedule.
ACTION is the request's action spelled FLAT: `(:arm :schedule :at-ms N
:reason REASON)', `(:arm :cancel)' or `(:arm :now :reason REASON)', where
REASON is itself a flat `DrainReason' arm.  The reason is REQUIRED on both
schedule and now: every client's drain banner names it."
  (agent-repl-verbs--send
   #'agent-repl-rpc-update-shutdown-schedule (agent-repl-verbs--conn)
   (list :action (agent-repl-verbs--shutdown-action action))
   :op "shutdown-schedule"
   :on-success
   (lambda (_) (message "agent-repl: shutdown %s" (plist-get action :arm)))))

(defun agent-repl-verbs--merge-queue-on-error (action value)
  "Claim UpdateMergeQueue's `unknown_repository' refusal of ACTION, else nil.
The arm is EMPTY -- the echoed ref is the only identity involved -- so the
repository it names is the one this verb sent, and that is what the log
context and the message carry.  Every other arm falls through to the
arm-generic reporting by answering nil."
  (let* ((arm (agent-repl-verbs--refusal-arm value))
         (keyword (plist-get arm :arm)))
    (when (eq keyword :unknown-repository)
      (agent-repl--warn '(:agent-repl-central "workspace creation and daemon administration can precede workspace ownership") "elisp.verbs.merge-queue-unknown-repository action=%S repository=%S"
                        (plist-get action :arm) (plist-get action :repository))
      (message "merge-queue refused: the daemon's registry does not hold repository %S"
               (plist-get action :repository))
      t)))

(defun agent-repl-verb-merge-queue (action)
  "Send ACTION to UpdateMergeQueue.
ACTION is spelled FLAT: `(:arm :pause :repository REF)', `(:arm :resume
:repository REF)' or `(:arm :evict :workspace REF)'.  A nil `repository'
is the daemon-wide switch and is omitted from the encoding."
  (agent-repl-verbs--send
   #'agent-repl-rpc-update-merge-queue (agent-repl-verbs--conn)
   (list :action (agent-repl-verbs--arm action))
   :op "merge-queue"
   :on-success
   (lambda (_) (message "agent-repl: merge queue %s" (plist-get action :arm)))
   :on-error
   (lambda (value) (agent-repl-verbs--merge-queue-on-error action value))))

;;;; ---- Health -----------------------------------------------------------

(defconst agent-repl-verbs-health-buffer "*agent-repl-health*"
  "Buffer every health pull renders into.")

(defun agent-repl-verbs--health-insert (lines)
  "Append LINES to the health buffer and display it."
  (with-current-buffer (get-buffer-create agent-repl-verbs-health-buffer)
    (goto-char (point-max))
    (dolist (line lines) (insert line "\n")))
  (display-buffer agent-repl-verbs-health-buffer))

(defun agent-repl-verbs--fault-line (fault indent)
  "Render FAULT as one line, prefixed by INDENT.
THE KIND IS THE FAULT CLASS; the detail SUPPLEMENTS it, never replaces it
(HostFault, DaemonFault and SessionFault all say so), so both are
printed.  Every family renders through this ONE formatter so a doctor
reads the same shape whichever stream the fault came from.  A fault with
no kind arm is a contract breach the decoder refuses, so reaching here
without one is loud rather than silently detail-only."
  (let ((kind (plist-get (plist-get fault :kind) :arm))
        (detail (plist-get fault :detail)))
    (if kind
        (format "%s- %s: %s" indent (substring (symbol-name kind) 1) detail)
      (agent-repl--error '(:agent-repl-context "daemon health has no workspace while session health inherits its request workspace") "elisp.verbs.fault-without-kind detail=%S" detail)
      (format "%s- unknown-kind: %s" indent detail))))

(defun agent-repl-verbs--fault-lines (faults)
  "Return one rendered line per fault in FAULTS."
  (mapcar (lambda (fault) (agent-repl-verbs--fault-line fault "  ")) faults))

(defun agent-repl-verbs--render-verdict (title verdict extra)
  "Render VERDICT under TITLE into the health buffer, followed by EXTRA lines.
UNHEALTHY IS AN ANSWER: it arrives inside success carrying its faults, so
both arms render the same way and neither is treated as a failure."
  (pcase (plist-get verdict :arm)
    (:healthy
     (agent-repl--info '(:agent-repl-context "daemon health has no workspace while session health inherits its request workspace") "elisp.verbs.health title=%s verdict=healthy" title)
     (agent-repl-verbs--health-insert (append (list "" (format "%s: HEALTHY" title)) extra)))
    (:unhealthy
     (let ((faults (plist-get (plist-get verdict :value) :faults)))
       (agent-repl--warn '(:agent-repl-context "daemon health has no workspace while session health inherits its request workspace") "elisp.verbs.health title=%s verdict=unhealthy faults=%d"
                         title (length faults))
       (agent-repl-verbs--health-insert
        (append (list "" (format "%s: UNHEALTHY (%d fault(s))" title (length faults)))
                (agent-repl-verbs--fault-lines faults)
                extra))))
    (arm
     (agent-repl--error '(:agent-repl-context "daemon health has no workspace while session health inherits its request workspace") "elisp.verbs.health-unknown-arm title=%s arm=%S" title arm))))

(defun agent-repl-daemon-health ()
  "Pull the daemon's own health verdict into `*agent-repl-health*'."
  (interactive)
  (agent-repl-verbs--send
   #'agent-repl-rpc-daemon-health (agent-repl-verbs--conn) nil
   :op "daemon-health"
   :on-success
   (lambda (verdict) (agent-repl-verbs--render-verdict "daemon" verdict nil))))

(defun agent-repl-session-health (&optional ws)
  "Pull WS's session health verdict into `*agent-repl-health*'.
The host stream's STANDING faults for WS are printed beside the pulled
ones: they are generation-scoped windows the daemon has already pushed,
and a doctor reading only the pull would miss them."
  (interactive)
  (let* ((ws (or ws (agent-repl--ws-current-name)))
         (ref (agent-repl-verbs--ref ws))
         (standing (agent-repl-host-faults ws))
         (extra (when standing
                  (cons (format "  standing host faults (%d):" (length standing))
                        (mapcar (lambda (f) (agent-repl-verbs--fault-line f "    "))
                                standing)))))
    (agent-repl-verbs--send
     #'agent-repl-rpc-session-health (agent-repl-verbs--conn ws)
     (list :workspace ref)
     :ws ws :op "session-health"
     :on-success
     (lambda (verdict)
       (agent-repl-verbs--render-verdict (format "session %s" ws) verdict extra)))))

;;;; ---- Interactive commands ---------------------------------------------

(defun agent-repl-close-workspace (&optional ws)
  "Close the current workspace (`SPC j x')."
  (interactive)
  (agent-repl-verb-close (agent-repl-verbs--target-ws ws "Close workspace: ")))

(defun agent-repl-kill-workspace (&optional ws)
  "Kill the current workspace's session by force."
  (interactive)
  (agent-repl-verb-kill (agent-repl-verbs--target-ws ws "Kill workspace: ")))

(defun agent-repl-nuke-workspace (&optional ws)
  "Destroy the current workspace, its worktree and its branch.
Confirms first: this is the ONE verb that destroys data, and it is
unrecoverable by design."
  (interactive)
  (let ((ws (agent-repl-verbs--target-ws ws "Nuke workspace: ")))
    (if (yes-or-no-p (format "Nuke %s?  Its worktree and branch are DELETED: " ws))
        (agent-repl-verb-nuke ws)
      (agent-repl--info ws "elisp.verbs.nuke-declined ws=%s" ws))))

(defun agent-repl-open-workspace ()
  "Re-open a closed workspace, chosen from the roster's closed rows."
  (interactive)
  (let* ((rows (agent-repl-verbs--closed-rows))
         (candidates (mapcar (lambda (row)
                               (cons (agent-repl-verbs--row-name row)
                                     (agent-repl-verbs--row-ref row)))
                             rows)))
    (unless candidates
      (user-error "agent-repl: the roster lists no closed workspaces"))
    (let* ((choice (completing-read "Open workspace: " (mapcar #'car candidates) nil t))
           (ref (cdr (assoc choice candidates))))
      (agent-repl--info '(:agent-repl-central "workspace creation and daemon administration can precede workspace ownership") "elisp.verbs.open-chosen name=%s" choice)
      (agent-repl-verb-open ref))))

(defun agent-repl-merge-workspace (&optional ws)
  "Enqueue the current workspace's merge (`SPC TAB M')."
  (interactive)
  (agent-repl-verb-merge (agent-repl-verbs--target-ws ws "Merge workspace: ")))

(defun agent-repl-restart-workspace (&optional force ws)
  "Restart the current workspace's session (`SPC o C-c').
A prefix argument makes it a FORCED restart: the live turn and every
background task are interrupted, and the agent is not resumed."
  (interactive "P")
  (agent-repl-verb-restart (agent-repl-verbs--target-ws ws "Restart workspace: ") (and force t)))

;;;; ---- Priority ---------------------------------------------------------

(defconst agent-repl-verbs-priority-levels
  '(("P0.5" . :p05) ("P1" . :p1) ("P2" . :p2) ("P3" . :p3))
  "The `WorkspacePriority' level arms, highest first, by their drawn label.")

(defconst agent-repl-verbs-priority-clear-label "*clear*"
  "The completion entry that CLEARS a priority.
Clearing is the absence of the field, which no level label can spell.")

(defun agent-repl-verbs--read-priority ()
  "Read a `WorkspacePriority' LEVEL keyword, or nil to clear."
  (let* ((labels (append (mapcar #'car agent-repl-verbs-priority-levels)
                         (list agent-repl-verbs-priority-clear-label)))
         (choice (completing-read "Priority: " labels nil t)))
    (unless (equal choice agent-repl-verbs-priority-clear-label)
      (cdr (assoc choice agent-repl-verbs-priority-levels)))))

(defun agent-repl-set-priority (&optional ws)
  "Set or clear the current workspace's priority."
  (interactive)
  (agent-repl-verb-set-priority (or ws (agent-repl--ws-current-name))
                                (agent-repl-verbs--read-priority)))

;;;; ---- Create -----------------------------------------------------------

(defun agent-repl-verbs--said (text)
  "Return the `UserSaid' carrying TEXT as its single text block."
  (list :content (list :blocks (list (list :arm :text :value (list :text text))))))

(defun agent-repl-verbs--dynamic-repository (command)
  "Return the `RepositoryRef' a DYNAMIC create targets, refusing without one.
Owner ruling, 2026-09-12: there are four creation modes, and each asks
only its own questions.  The three DYNAMIC modes -- a one-shot, a normal
workspace, and a fork -- ask for a prompt and nothing else: the repository
is the repository the CURRENT workspace sits in, taken off that repo's
MAIN (default) branch, and the daemon mints the name.  So the repository
is READ from the roster section the current workspace's row sits in, and
never picked.  Only the static mode asks for a repository.

COMMAND names the mode in the refusal, which is raised when the current
buffer has no workspace to derive from -- a dynamic create has nothing to
derive from then, and the static command is where a repository is named
by hand."
  (let* ((section (agent-repl-verbs--section-of-ws (agent-repl--ws-current-name)))
         (repository (and section (agent-repl-verbs--section-ref section))))
    (unless repository
      (user-error
       "agent-repl: %s needs a current workspace to take its repository from; use `agent-repl-create-workspace-static' to name one"
       command))
    repository))

(defun agent-repl-verbs--read-repository ()
  "Read a repository from the roster's sections, defaulting to the current one.
The STATIC create is the one mode that asks this; every dynamic mode
derives its repository through `agent-repl-verbs--dynamic-repository'."
  (let* ((sections (agent-repl-verbs--repo-sections))
         (default (agent-repl-verbs--section-of-ws (agent-repl--ws-current-name)))
         (candidates (mapcar (lambda (s)
                               (cons (agent-repl-verbs--section-label s)
                                     (agent-repl-verbs--section-ref s)))
                             sections)))
    (unless candidates
      (user-error "agent-repl: the roster lists no repositories"))
    (let ((choice (completing-read
                   "Repository: " (mapcar #'car candidates) nil t nil nil
                   (and default (agent-repl-verbs--section-label default)))))
      (cdr (assoc choice candidates)))))

(defun agent-repl-verbs--read-prompt (prompt-text)
  "Read the creation prompt, defaulting to the composer's current text.
The composer is where the user was already writing, so its contents are
the obvious default rather than something to retype."
  (let* ((ws (agent-repl--ws-current-log-name))
         (buffered (and ws (agent-repl--read-input-buffer ws)))
         (initial (and buffered (not (string-empty-p (string-trim buffered)))
                       (string-trim buffered))))
    (unless ws
      (agent-repl--log '(:agent-repl-central "the current buffer has no workspace composer")
                       "elisp.verbs.read-prompt prompt=%S composer=none" prompt-text))
    (read-string prompt-text initial)))

(defun agent-repl-create-workspace (&optional child)
  "Create a DYNAMIC workspace (`SPC TAB n'): a prompt, and nothing else.
Owner ruling, 2026-09-12.  This is the dynamic normal mode, so the only
question it asks is the prompt, which defaults to the composer's text.
The repository is the one the current workspace sits in, off that repo's
MAIN (default) branch -- an absent base ref IS that resolution -- and the
daemon mints the name.  A prefix argument makes the new workspace a CHILD
of the current one, whose merge target is then the parent's worktree and
branch rather than the repo's main checkout.

Naming a repository or a name by hand is the STATIC mode's business:
`agent-repl-create-workspace-static'.

THE NEW WORKSPACE IS SELECTED.  Creating one is a statement about where
you intend to work next, so this stands on it the moment the daemon
answers -- the same step registering a directory takes."
  (interactive "P")
  (let* ((repository (agent-repl-verbs--dynamic-repository "a dynamic create"))
         (prompt (agent-repl-verbs--read-prompt "Initial prompt: "))
         (parent (when child
                   (agent-repl-verbs--ref (agent-repl--ws-current-name)))))
    (agent-repl--info '(:agent-repl-central "workspace creation and daemon administration can precede workspace ownership") "elisp.verbs.create-standard mode=dynamic child=%s"
                      (and child t))
    (agent-repl-verb-create
     repository :standard
     :initial-prompt (unless (string-empty-p (string-trim prompt)) prompt)
     :parent parent
     :select t)))

(defun agent-repl-create-workspace-static (&optional child)
  "Create a STATIC workspace (`SPC TAB N'): a repository and a name, no prompt.
Owner ruling, 2026-09-12.  This is the one creation mode that asks for a
repository, and the one that REQUIRES a name; it sends no initial prompt
at all, so the workspace comes up idle and waits for the user.  A prefix
argument makes it a CHILD of the current workspace, exactly as the
dynamic create's does.

THE NEW WORKSPACE IS SELECTED, for the same reason the dynamic create's
is."
  (interactive "P")
  (let* ((repository (agent-repl-verbs--read-repository))
         (name (string-trim (read-string "Name: ")))
         (parent (when child
                   (agent-repl-verbs--ref (agent-repl--ws-current-name)))))
    (when (string-empty-p name)
      (user-error "agent-repl: a static workspace IS its name"))
    (agent-repl--info '(:agent-repl-central "workspace creation and daemon administration can precede workspace ownership") "elisp.verbs.create-standard mode=static child=%s"
                      (and child t))
    (agent-repl-verb-create
     repository :standard
     :name name
     :parent parent
     :select t)))

(defun agent-repl-fork-workspace ()
  "Create a CHILD workspace forking the current one's conversation.
A fork without a parent is unrepresentable by construction, which is why
this is its own command rather than a flag on the plain create.

It is a DYNAMIC mode (owner ruling, 2026-09-12), so it asks for the
prompt alone: the repository is the current workspace's, off that repo's
main branch, and the daemon mints the name.

THE FORK IS SELECTED, exactly as a plain create is: you forked in order
to work in the fork."
  (interactive)
  (let* ((repository (agent-repl-verbs--dynamic-repository "a fork"))
         (prompt (agent-repl-verbs--read-prompt "Initial prompt: "))
         (parent (agent-repl-verbs--ref (agent-repl--ws-current-name))))
    (agent-repl--info '(:agent-repl-central "workspace creation and daemon administration can precede workspace ownership") "elisp.verbs.create-fork mode=dynamic")
    (agent-repl-verb-create
     repository :standard
     :initial-prompt (unless (string-empty-p (string-trim prompt)) prompt)
     :parent parent :fork t
     :select t)))

;;;; ---- One-shots --------------------------------------------------------
;;
;; ONE-SHOTS RIDE THE WIRE.  Emacs supplies the prompt, the model and the
;; parentage; the DAEMON owns naming, the worktree, prompt decoration and
;; the finish action.  Everything the old doom / explanation-engine
;; one-shot commands did by hand is now one CreateWorkspace request whose
;; form arm says one_shot.

(defcustom agent-repl-oneshot-model-candidates '("opus" "sonnet" "haiku")
  "The models offered when creating a ONE-SHOT workspace, in order.
One-shots ride the wire -- Emacs supplies prompt, model and parentage
through the dedicated one-shot creation form and the daemon owns naming,
the worktree, decoration and the merge/PR postprocessing -- so this list
is a picker's contents and nothing more.  It lives beside the one-shot
commands that read it, which are its only consumer."
  :type '(repeat string)
  :group 'agent-repl)

(defun agent-repl-verbs--read-model ()
  "Read a model, or nil for the daemon's default.
Completes against `agent-repl-oneshot-model-candidates' without requiring
a match: the candidate list is a convenience, and the model vocabulary is
the vendor's, not ours.

`agent-repl-interactive-model' seeds the prompt as its INITIAL INPUT, so
the user's configured model is what a bare RET sends while the picker
still allows any override -- including erasing the seed back to blank,
which is what asks the daemon to choose.  A nil setting seeds nothing."
  (let ((value (string-trim
                (completing-read
                 "Model (blank = daemon default): "
                 agent-repl-oneshot-model-candidates nil nil
                 agent-repl-interactive-model))))
    (unless (string-empty-p value) value)))

(cl-defun agent-repl-verbs--create-oneshot (finish &key model
                                                   self-certified add-to-merge-queue)
  "Create a one-shot whose FINISH arm says what happens when the work ends.
A one-shot is a DYNAMIC mode (owner ruling, 2026-09-12): it asks for its
commission and nothing else -- the repository is the current workspace\='s,
off that repo\='s main branch, and the daemon mints the name."
  (let ((repository (agent-repl-verbs--dynamic-repository "a one-shot"))
        (prompt (agent-repl-verbs--read-prompt "One-shot commission: ")))
    (when (string-empty-p (string-trim prompt))
      (user-error "agent-repl: a one-shot IS its prompt"))
    (agent-repl--info '(:agent-repl-central "workspace creation and daemon administration can precede workspace ownership") "elisp.verbs.create-one-shot mode=dynamic finish=%S model=%S" finish model)
    (agent-repl-verb-create
     repository :one-shot
     :prompt prompt :finish finish
     :self-certified self-certified :add-to-merge-queue add-to-merge-queue
     :model model)))

(defun agent-repl-create-oneshot-self-merge (&optional pick-model)
  "Create a one-shot that MERGES itself back when the work concludes.
A prefix argument asks for the model."
  (interactive "P")
  (agent-repl-verbs--create-oneshot
   :self-merge
   :model (and pick-model (agent-repl-verbs--read-model))))

(defun agent-repl-create-oneshot-open-pr (&optional pick-model)
  "Create a one-shot that opens a SELF-CERTIFIED PR added to the merge queue.
A prefix argument asks for the model.  Both PR flags are stated
explicitly, false included: they change what happens to the branch."
  (interactive "P")
  (agent-repl-verbs--create-oneshot
   :open-pr :self-certified t :add-to-merge-queue t
   :model (and pick-model (agent-repl-verbs--read-model))))

(defun agent-repl-create-oneshot-open-pr-reviewed (&optional pick-model)
  "Create a one-shot that opens a PR demanding a review round.
Neither self-certified nor queued: the PR waits for a human."
  (interactive "P")
  (agent-repl-verbs--create-oneshot
   :open-pr :self-certified nil :add-to-merge-queue nil
   :model (and pick-model (agent-repl-verbs--read-model))))

;;;; ---- Shutdown and merge-queue commands --------------------------------

(defconst agent-repl-verbs-drain-reason-kinds
  '(("deploy" . :deploy) ("maintenance" . :maintenance) ("operator" . :operator))
  "The `DrainReason' arms by their prompt label.
THE ARM IS THE REASON -- never a bare string; the note lives inside the one
arm that owns it.")

(defun agent-repl-verbs--read-drain-reason ()
  "Read a FLAT `DrainReason'.  The operator arm's note is REQUIRED non-blank."
  (let* ((choice (completing-read "Reason: "
                                  (mapcar #'car agent-repl-verbs-drain-reason-kinds)
                                  nil t))
         (arm (cdr (assoc choice agent-repl-verbs-drain-reason-kinds))))
    (if (eq arm :operator)
        (let ((note (string-trim (read-string "Operator note: "))))
          (when (string-empty-p note)
            (user-error "agent-repl: an operator reason requires a note"))
          (list :arm :operator :note note))
      (list :arm arm))))

(defun agent-repl-daemon-shutdown-schedule (minutes)
  "Schedule the daemon to drain and exit MINUTES from now.
The reason is REQUIRED: the drain_scheduled push carries it verbatim and
every client's standing banner names it."
  (interactive "nDrain in how many minutes: ")
  (let ((at-ms (+ (truncate (* 1000 (float-time))) (* minutes 60 1000)))
        (reason (agent-repl-verbs--read-drain-reason)))
    (agent-repl-verb-shutdown-schedule
     (list :arm :schedule :at-ms at-ms :reason reason))))

(defun agent-repl-daemon-shutdown-cancel ()
  "Drop the daemon's standing shutdown schedule and resume normal intake."
  (interactive)
  (agent-repl-verb-shutdown-schedule (list :arm :cancel)))

(defun agent-repl-daemon-shutdown-now ()
  "Stop the daemon now.  The reason rides the shutdown announcement."
  (interactive)
  (agent-repl-verb-shutdown-schedule
   (list :arm :now :reason (agent-repl-verbs--read-drain-reason))))

(defun agent-repl-verbs--merge-queue-repository (&optional ws)
  "Return the repository ref scoping a queue change for WS, or refuse.
THE QUEUE IS PER REPOSITORY, so a caller that means one names it: the ref
is the roster section's own key for the workspace's section, which is the
only place Emacs holds one.  Errors when the roster has no section for WS
— sending the daemon-wide switch instead would pause every repository on
a caller who asked about one."
  (let* ((ws (or ws (agent-repl--ws-current-name)))
         (ref (agent-repl-verbs--ref ws))
         (repository (agent-repl-roster-repository-of (plist-get ref :id))))
    (or repository
        (progn
          (agent-repl--warn ws "elisp.verbs.no-repository ws=%s" ws)
          (user-error "agent-repl: no roster repository for workspace %s" ws)))))

(defun agent-repl-merge-queue-pause (&optional daemon-wide)
  "Stop admitting merges to the front of THIS workspace's repository queue.
With a prefix argument DAEMON-WIDE the repository is omitted, which is the
contract's spelling of every repository that has a queue."
  (interactive "P")
  (agent-repl-verb-merge-queue
   (list :arm :pause
         :repository (unless daemon-wide (agent-repl-verbs--merge-queue-repository)))))

(defun agent-repl-merge-queue-resume (&optional daemon-wide)
  "Resume admitting merges on THIS workspace's repository queue.
With a prefix argument DAEMON-WIDE the repository is omitted, resuming
every repository that has a queue."
  (interactive "P")
  (agent-repl-verb-merge-queue
   (list :arm :resume
         :repository (unless daemon-wide (agent-repl-verbs--merge-queue-repository)))))

(defun agent-repl-merge-queue-evict (&optional ws)
  "Take the current workspace's merge off the queue."
  (interactive)
  (let ((ws (or ws (agent-repl--ws-current-name))))
    (agent-repl-verb-merge-queue
     (list :arm :evict :workspace (agent-repl-verbs--ref ws)))))

(provide 'verbs)

;;; verbs.el ends here
