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

(declare-function agent-repl--panels-note-arrival-reason "agent-repl-panels"
                  (id reason))
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
(declare-function agent-repl--ws-switch "agent-repl-workspace" (ws &rest args))
(declare-function agent-repl--pseudo-workspace-name-p "agent-repl-core" (ws))
(declare-function agent-repl--log-note-workspace-departing "agent-repl-core" (ws))
(declare-function agent-repl--log-forget-workspace-departure "agent-repl-core" (ws))
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
(declare-function agent-repl-rpc-interrupt "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-create-workspace "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-mutation-progress-new-op-id "mutation-progress" ())
(declare-function agent-repl-mutation-progress-register "mutation-progress" (op-id &rest callbacks))
(declare-function agent-repl-mutation-progress-forget "mutation-progress" (op-id))
(declare-function agent-repl-workspace-progress-report "mutation-progress" (kind phase &rest details))
(declare-function agent-repl-rpc-register-repository "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-set-workspace-priority "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-deploy "agent-repl-rpc" (conn request &rest keys))
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
     ((eq keyword :lock-holder-unavailable)
      ;; THE ONE ARM WHOSE KEYWORD WOULD MISLEAD ON ITS OWN.  Nobody owns
      ;; the conversation: the shim's own lock helper would not start, and
      ;; the binary plus the OS error are the whole remediation, so they
      ;; are said in words rather than as a raw plist.
      (let ((fields (plist-get arm :value)))
        (agent-repl--warn ws (format "elisp.verbs.%s-refused ws=%%s arm=%%S fields=%%S" op)
                          ws keyword fields)
        (message "%s refused: the shim's lock helper %s failed to start (%s); no other process owns this conversation"
                 op (plist-get fields :binary) (plist-get fields :os-error))))
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

(cl-defun agent-repl-verbs--send (rpc conn request &key ws op on-success on-error
                                      on-accepted on-transport-failure timeout)
  "Send REQUEST through RPC on CONN and dispatch the answer shapes.
OP names the verb for the log.  ON-SUCCESS receives the decoded success
value and is the ONLY place editor state changes.  ON-ERROR, when given,
receives the decoded error value and OWNS the reporting for that verb;
without it a daemon-authored refusal is reported generically.  ON-ACCEPTED,
when given, receives the decoded accepted value -- the option-B ack a
mutation returns when its real outcome will arrive on the progress channel
rather than on this rpc; an accepted answer with no ON-ACCEPTED is an
unexpected shape and is logged.  A transport failure never reaches
ON-ERROR: nobody answering and the daemon refusing are different facts.
ON-TRANSPORT-FAILURE, when given, receives the failure detail on that
third path; it is for RETIRING STATE the verb armed before the send (a
registered progress op has no other way to learn nothing is coming) and
never for wording the failure, which this dispatcher owns.
TIMEOUT, when given, overrides `agent-repl-connect-unary-timeout-seconds\='
for this send -- for a verb whose work is a SESSION BRING-UP, which
outlasts the default deadline often enough that the default would report
a failure over work that is still running and will succeed."
  (let ((request-id (or (plist-get request :idempotency-key)
                        (agent-repl--next-log-request-id)))
        (log-ws (or ws agent-repl--global-log-scope)))
    (agent-repl--with-log-context
     log-ws request-id
     (lambda ()
       (agent-repl--info ws "elisp.verbs.send op=%s ws=%s" op ws)
       (funcall rpc conn request
                :timeout timeout
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
                       (:accepted
                        ;; THE OPTION-B ACK: the mutation was accepted and its
                        ;; outcome rides the progress channel, so nothing
                        ;; terminal happens here.
                        (agent-repl--info ws "elisp.verbs.ack op=%s ws=%s outcome=accepted" op ws)
                        (if on-accepted
                            (funcall on-accepted (plist-get response :value))
                          (agent-repl--error
                           ws "elisp.verbs.unexpected-accepted op=%s ws=%s" op ws)))
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
                     (when on-transport-failure (funcall on-transport-failure detail))
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
    ;; The worktree goes as soon as the daemon acts, which is BEFORE the
    ;; answer arrives and well before the tab teardown; records written in
    ;; between are attributed to a workspace whose registered directory has
    ;; already gone.  The order is what explains that, so the order is
    ;; declared at the moment it is given rather than when it completes.
    (agent-repl--log-note-workspace-departing ws)
    (agent-repl-verbs--send
     #'agent-repl-rpc-nuke-workspace (agent-repl-verbs--conn ws)
     (list :workspace ref)
     :ws ws :op "nuke"
     :on-success (lambda (_) (agent-repl-verbs--teardown-tab ws "nuke"))
     ;; A REFUSED nuke destroyed nothing, so the order above is withdrawn and
     ;; the arm is left unclaimed for the generic refusal handling.
     :on-error (lambda (_value)
                 (agent-repl--log-forget-workspace-departure ws)
                 nil))))

(defun agent-repl-verb-open (ref)
  "Open the closed workspace named by REF.  Any revival is the daemon's.
The tab arrives through the roster push, not through this answer.

THE WAIT IS REPORTED, not only its outcome.  A re-open sits inside the
daemon\='s `Sessions.Start\=' -- a shim spawn plus a vendor resume -- for as
long as a create sits in `git worktree add\=', and for all of it Emacs used
to say one line on the way in and one on the way out.  So the request
carries a minted op id, exactly as a create\='s does, and the daemon pushes
the open\='s stages back on the daemon-level stream keyed on it
\(`workspace_mutation_progress.proto\').

THE OP IS FORGOTTEN ON EVERY TERMINAL PATH, because unlike a create the
open\='s outcome arrives on THIS rpc rather than on the progress channel:
nothing on the stream would ever retire the registration."
  (let ((op-id (agent-repl-mutation-progress-new-op-id))
        (name (or (plist-get ref :dir) "the workspace")))
    ;; Registered BEFORE the send, so a stage push cannot outrun its handler.
    (agent-repl-mutation-progress-register
     op-id
     :on-stage (lambda (stage) (agent-repl-workspace-progress-report :open stage)))
    (agent-repl-workspace-progress-report :open :requested name)
    (agent-repl-verbs--send
     #'agent-repl-rpc-open-workspace (agent-repl-verbs--conn)
     (list :workspace ref :op-id op-id)
     :op "open"
     :on-success (lambda (_)
                   (agent-repl-mutation-progress-forget op-id)
                   ;; The tab arrives on the roster push, and when it does it
                   ;; opens its panels as a re-open (owner ruling,
                   ;; 2026-09-13, item 6).
                   (agent-repl--panels-note-arrival-reason
                    (plist-get ref :id) "reopened")
                   (agent-repl-workspace-progress-report :open :completed name))
     :on-error
     ;; The arm is NOT claimed (nil): the daemon-authored refusal is still
     ;; worded by the shared dispatcher, which names the arm and its fields.
     ;; All this does is retire an op no further stage will ever arrive for.
     (lambda (_value)
       (agent-repl-mutation-progress-forget op-id)
       nil)
     ;; A daemon that never answered will never push a stage either, so the
     ;; op is retired here too: otherwise every unanswered open would leave a
     ;; registration behind for the life of the session.
     :on-transport-failure
     (lambda (_detail) (agent-repl-mutation-progress-forget op-id)))))

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

(defun agent-repl-verb-interrupt (ws &optional confirm-agents)
  "Interrupt the running TURN of WS through the `Interrupt' verb.
This is the clean turn stop the footer's stop button drives, NOT a forced
restart: the session is not bounced and the agent is not resumed, only the
in-flight vendor query is ended.

THE OUTCOME IS THE ARM, and every arm reaches the user.  `interrupted_turn'
says the turn was stopped; `nothing_running' is a SUCCESS arm, not a
failure -- the daemon found the session already quiet -- so it is a calm
message rather than an error.  `no_session' (the workspace never had a
session) is a daemon refusal reported through the generic path, likewise as
a message and never a raised error, so an idle workspace is a no-op with
feedback either way.

CONFIRM-AGENTS answers the `confirm_required' challenge: a turn stop while
detached agents are live is refused once, naming the count, and re-sent
with this set to also end them.  The challenge is surfaced to the user as a
yes/no question here rather than swallowed -- declining leaves the turn
running and says so.  Every other refusal (and the transport failure) is
surfaced loudly by the shared dispatcher."
  (let ((ref (agent-repl-verbs--ref ws)))
    (agent-repl-verbs--send
     #'agent-repl-rpc-interrupt (agent-repl-verbs--conn ws)
     (list :workspace ref
           :target (list :arm :turn :value nil)
           :confirm-agents (and confirm-agents t))
     :ws ws :op "interrupt"
     :on-success
     (lambda (value)
       (pcase (plist-get value :arm)
         (:interrupted-turn
          (agent-repl--info ws "elisp.verbs.interrupt-stopped ws=%s" ws)
          (message "agent-repl: turn stopped"))
         (:nothing-running
          (agent-repl--info ws "elisp.verbs.interrupt-nothing-running ws=%s" ws)
          (message "agent-repl: nothing to interrupt"))
         (:interrupted-detached
          ;; A turn stop confirmed with live agents also ends them; the
          ;; daemon may answer with the fan-wide count rather than the bare
          ;; turn arm, so the count is stated when it comes.
          (let ((count (plist-get (plist-get value :value) :count)))
            (agent-repl--info ws "elisp.verbs.interrupt-detached ws=%s count=%S" ws count)
            (message "agent-repl: turn stopped, %s agent%s also ended"
                     (or count 0) (if (eql count 1) "" "s"))))
         (arm
          (agent-repl--error ws "elisp.verbs.interrupt-unknown-outcome ws=%s arm=%S" ws arm)
          (message "agent-repl: the interrupt answer could not be read"))))
     :on-error
     (lambda (value)
       ;; `confirm_required' is the ONE arm this verb claims: it is a
       ;; CHALLENGE, not a dead end.  The user is asked whether to also stop
       ;; the live agents and, on yes, the identical request is re-sent with
       ;; `confirm_agents' set.  Declining is a decision, not a failure, so
       ;; it is a message rather than a refusal.  Every other arm is left
       ;; unclaimed (nil) and reported by the generic refusal path.
       (let ((refusal (agent-repl-verbs--refusal-arm value)))
         (when (eq (plist-get refusal :arm) :confirm-required)
           (let ((count (plist-get (plist-get refusal :value) :live-agent-count)))
             (agent-repl--warn ws "elisp.verbs.interrupt-confirm-required ws=%s count=%S" ws count)
             (if (yes-or-no-p
                  (format "Also stop %s live agent%s the turn started? "
                          (or count 0) (if (eql count 1) "" "s")))
                 (agent-repl-verb-interrupt ws t)
               (agent-repl--info ws "elisp.verbs.interrupt-confirm-declined ws=%s" ws)
               (message "agent-repl: turn left running")))
           t))))))

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
                                             prompt
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
  `:one-shot\' takes PROMPT and nothing else (required -- a one-shot IS
    its prompt).  THERE IS NO FINISH CHOICE: what happens on completion is
    the repository\='s own directive, which the daemon appends to the
    commission and the agent carries out.

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
  ;; OPTION B.  The create is ACKED at once and worked in the daemon's
  ;; background, detached from this request so a client deadline can never kill
  ;; `git worktree add' mid-run.  Its staged progress and terminal outcome
  ;; arrive on the daemon-level WatchDaemon stream keyed on OP-ID -- the one
  ;; channel open before this workspace exists.  The callbacks that render them
  ;; are registered BEFORE the send, so a progress event never outruns them.
  (let ((op-id (agent-repl-mutation-progress-new-op-id))
        (arrival-reason (if fork "forked" "created")))
    (agent-repl-mutation-progress-register
     op-id
     :on-stage #'agent-repl-verbs--create-stage-message
     :on-succeeded
     (lambda (value)
       (let ((ref (plist-get value :workspace))
             (name (plist-get value :name)))
         (agent-repl-workspace-progress-report :create :completed name)
         ;; The minted ref is the FIRST place this workspace has an identity;
         ;; the arrival reason is claimed here so a one-shot (which never
         ;; selects) still opens its panels when its tab arrives.
         (agent-repl--panels-note-arrival-reason (plist-get ref :id) arrival-reason)
         (when select
           (agent-repl-verbs-select-minted ref))))
     :on-failed #'agent-repl-verbs--create-failure)
    ;; THE ACK UX IS IMMEDIATE: the minibuffer reflects the create the instant
    ;; the command runs, not when the slow work finishes -- the owner's rule
    ;; that phases are messages, not only a mode line.
    (agent-repl-workspace-progress-report :create :requested)
    (agent-repl-verbs--send
     #'agent-repl-rpc-create-workspace (agent-repl-verbs--conn)
     (list :repository repository
           :form (agent-repl-verbs--create-form
                  form :initial-prompt initial-prompt :base-ref base-ref :name name
                  :merge-actions merge-actions
                  :prompt prompt)
           :parent (when parent
                     (append (list :workspace parent) (when fork (list :fork fork))))
           :model model
           :priority (agent-repl-verbs--level-arm priority)
           :allow-ungated allow-ungated
           :op-id op-id)
     :op "create"
     :on-accepted
     (lambda (_accepted)
       (agent-repl--info '(:agent-repl-central "workspace creation and daemon administration can precede workspace ownership")
                         "elisp.verbs.create-accepted op-id=%s" op-id))
     :on-success
     ;; A daemon that answered success synchronously (an op_id it did not
     ;; honor) still landed the workspace: run the terminal and drop the op.
     (lambda (success)
       (agent-repl-mutation-progress-forget op-id)
       (agent-repl--panels-note-arrival-reason
        (plist-get (plist-get success :workspace) :id) arrival-reason)
       (when select
         (agent-repl-verbs--select-created success)))
     :on-error
     ;; A SYNCHRONOUS refusal (validation, an unknown repository) is answered
     ;; on the rpc before the work detaches, so no progress will follow: forget
     ;; the op and word the refusal, falling through for arms it does not claim.
     (lambda (value)
       (agent-repl-mutation-progress-forget op-id)
       (agent-repl-verbs--create-refusal value))
     ;; A daemon that never answered will never push progress either, so the
     ;; op is retired on that path too rather than left registered for the
     ;; life of the session.
     :on-transport-failure
     (lambda (_detail) (agent-repl-mutation-progress-forget op-id)))))

(defun agent-repl-verbs--create-stage-message (stage)
  "Echo the minibuffer line for a create STAGE keyword.
The wording lives in `agent-repl-workspace-progress-phases\=' with every
other phase of every other way of making a workspace, so the create\='s
stages and the open\='s cannot drift into two vocabularies.  A stage with
no template is reported as the contract breach it is, never invented."
  (agent-repl-workspace-progress-report :create stage))

(defun agent-repl-verbs--create-failure (arm detail)
  "Surface a background create's failure LOUDLY, never silently.
ARM is the failure kind -- `:refusal' with a typed CreateWorkspaceError
value, or `:internal' with the daemon's own sentence (the full cause is in
the daemon log)."
  (pcase arm
    (:refusal
     ;; The SAME wording a synchronous refusal gets: the create-specific
     ;; refusals Emacs words itself, and every other arm falls through.
     (unless (agent-repl-verbs--create-refusal detail)
       (agent-repl-verbs--on-refusal nil "create" detail)))
    (:internal
     (agent-repl--error '(:agent-repl-central "workspace creation and daemon administration can precede workspace ownership")
                        "elisp.verbs.create-failed-internal detail=%S" detail)
     (agent-repl-workspace-progress-report :create :failed detail))
    (_
     (agent-repl--error '(:agent-repl-central "workspace creation and daemon administration can precede workspace ownership")
                        "elisp.verbs.create-failed-unknown arm=%S detail=%S" arm detail)
     (agent-repl-workspace-progress-report :create :failed "the daemon named no cause"))))

(defun agent-repl-verbs--create-refusal (value)
  "Draw the create refusals Emacs words itself, from VALUE.
Answers non-nil when one of them claimed the arm, so every other arm
still falls through to the generic refusal handling."
  (or (agent-repl-verbs--create-policy-refusal value)
      (agent-repl-verbs--create-naming-refusal value)))

(defun agent-repl-verbs--create-naming-refusal (value)
  "Draw a `naming_failed\=' refusal of a create from VALUE.
Answers non-nil when it claimed the arm.

EVERY DYNAMICALLY CREATED WORKSPACE IS NAMED BY THE MODEL, and this one
could not be: there is no truncation fallback, so the create is refused
rather than given a name nobody chose.  It is drawn as the WARNING it
is, naming the cause and what the model last said, because those are
what say whether to retry or to supply a name by hand."
  (let ((refusal (agent-repl-verbs--refusal-arm value)))
    (when (eq (plist-get refusal :arm) :naming-failed)
      (let* ((failed (plist-get refusal :value))
             (cause (plist-get failed :cause))
             (answer (plist-get failed :answer))
             (attempts (plist-get failed :attempts)))
        (agent-repl--warn
         '(:agent-repl-central "workspace creation and daemon administration can precede workspace ownership")
         "elisp.verbs.create-refused arm=%S fields=%S" :naming-failed failed)
        (message "create refused: the workspace could not be named (%s, %s attempt%s)%s"
                 cause attempts (if (eql attempts 1) "" "s")
                 (if (and answer (not (string-empty-p answer)))
                     (format " -- the model answered %S" answer)
                   "")))
      t)))

(defun agent-repl-verbs--create-policy-refusal (value)
  "Draw a `one_shot_policy_missing\=' refusal of a create from VALUE.
Answers non-nil when it claimed the arm, so every other arm still falls
through to the generic refusal handling.

A ONE-SHOT RUNS ITS REPOSITORY\='S OWN POLICY, and the daemon detects a
repository that states none -- Emacs never looks at the filesystem for
this.  The refusal is drawn as the WARNING it is, naming the directory
the user must write and the files it needs, because the user\='s next
move is to write exactly those."
  (let ((refusal (agent-repl-verbs--refusal-arm value)))
    (when (eq (plist-get refusal :arm) :one-shot-policy-missing)
      (let* ((missing (plist-get refusal :value))
             (dir (plist-get missing :policy-dir))
             (files (plist-get missing :missing-files)))
        (agent-repl--warn
         '(:agent-repl-central "workspace creation and daemon administration can precede workspace ownership")
         "elisp.verbs.create-refused arm=%S fields=%S" :one-shot-policy-missing missing)
        (message "create refused: %s states no one-shot policy -- write %s"
                 (plist-get missing :repository-root)
                 (if files
                     (format "%s in %s" (string-join files ", ") dir)
                   dir)))
      t)))

(defun agent-repl-verbs-select-minted (ref &optional lander)
  "Stand on the workspace REF names, once the daemon has minted it.

LANDER is HOW the landing is made, and defaults to
`agent-repl-verbs--land-on\=' -- a projectile switch to the minted
WORKTREE, which is right for every verb whose workspace is a worktree
Doom has never seen.  A verb whose workspace is a repository\='s MAIN
worktree passes `agent-repl-verbs--land-on-tab\=' instead; see that
function for why landing by DIRECTORY cannot be used there.

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
        (id (plist-get ref :id))
        (land (or lander #'agent-repl-verbs--land-on)))
    (if (and dir (not (string-empty-p dir)))
        (let ((ws (agent-repl--ws-by-ref-id id)))
          (agent-repl--info
           (if ws
               ws
             '(:agent-repl-central
               "a minted workspace awaiting its roster row has no sink"))
           "elisp.verbs.select-minted dir=%s" dir)
          (if ws
              (funcall land ref "already-a-tab" ws)
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
            (agent-repl-verbs--pending-landing-register ref land)))
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

(defvar agent-repl-verbs--pending-landing-lander nil
  "The lander `agent-repl-verbs--pending-landing\=' is waiting to be made with.
Kept beside the ref rather than inside it because the ref is a decoded
`WorkspaceRef\=' -- a wire value -- and an editor-local choice about HOW
to stand on it is not part of that contract.  Set and cleared with the
ref it belongs to, so the two can never disagree.")

(defun agent-repl-verbs--land-on-tab (ref why ws)
  "Stand on workspace WS by switching to ITS OWN TAB, recording WHY.

A LANDING BY DIRECTORY IS NOT A LANDING BY IDENTITY, and for a
repository\='s MAIN worktree the difference is the whole bug.
`agent-repl-verbs--land-on\=' stands on the workspace through
`agent-repl-switch-to-project\=', i.e. through projectile, and Doom\='s
`+workspaces-switch-to-project-h\=' then picks the perspective by the
project directory\='s BASENAME.  A minted worktree\='s perspective is
named after its own directory, so for create, fork and register that
lookup finds the workspace\='s own tab.  The main worktree\='s workspace
carries the DAEMON\='S name, which is not that basename, so the lookup
missed, Doom minted a perspective of its own from the window
configuration of the perspective the user was standing in, and the
origin workspace\='s frame came up as the repository that had just been
registered (`SPC j .\=' from iterm-2 turned iterm-2 into
explanation-engine).

So the landing switches to the tab the ROSTER opened for this
workspace -- `agent-repl--ws-switch\=', which can only ever activate a
perspective that already exists and so cannot rename, recycle or
re-point the one being left.  The tab is guaranteed to be there: this
lander is only ever reached with WS resolved from the ref id, which is
the roster row itself."
  (agent-repl--info ws "elisp.verbs.land-on-tab ws=%s dir=%s why=%s"
                    ws (plist-get ref :dir) why)
  (agent-repl--ws-switch ws))

(defun agent-repl-verbs--pending-landing-register (ref &optional lander)
  "Record REF as the landing waiting for its tab to reach the roster.
LANDER is how it will be made when the tab arrives; see
`agent-repl-verbs-select-minted\='."
  (agent-repl--info '(:agent-repl-central "workspace creation and daemon administration can precede workspace ownership") "elisp.verbs.pending-landing-registered dir=%s id=%s"
                    (plist-get ref :dir) (plist-get ref :id))
  (setq agent-repl-verbs--pending-landing-lander
        (or lander #'agent-repl-verbs--land-on))
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
          (let ((land (or agent-repl-verbs--pending-landing-lander
                          #'agent-repl-verbs--land-on)))
            (setq agent-repl-verbs--pending-landing nil
                  agent-repl-verbs--pending-landing-lander nil)
            (agent-repl--info ws "elisp.verbs.pending-landing-arrived ws=%s id=%s" ws id)
            (funcall land ref "tab-arrived" ws))
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
                                              merge-actions prompt)
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
           :value (list :prompt (and prompt (agent-repl-verbs--said prompt)))))
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

;;;; ---- Deploy -----------------------------------------------------------
;;
;; THE DAEMON OWNS DEPLOYS.  Emacs only ASKS: the daemon builds every
;; component, decides what is out of date by content hash and restarts or
;; reloads exactly that.  The answer is the deploy's DECISIONS, one outcome
;; per component; what follows arrives on the surfaces that already carry it
;; (a handover's announcement, a shim bounce, this Emacs's own
;; `reload_elisp').

(defcustom agent-repl-deploy-timeout-seconds 900
  "Seconds `agent-repl-deploy' waits for the daemon's answer.
The answer comes after the daemon has BUILT every component, which is the
whole stack's build, so it is far longer than an ordinary verb's deadline."
  :type 'number
  :group 'agent-repl)

(defun agent-repl-verbs--deploy-component-name (component)
  "Return the DeployComponent keyword COMPONENT as its bare name."
  (substring (symbol-name component) 1))

(defun agent-repl-verbs--deploy-outcome-phrase (outcome)
  "Return the decision OUTCOME, a (:arm ARM :value V), as a short phrase."
  (let ((value (plist-get outcome :value)))
    (pcase (plist-get outcome :arm)
      (:up-to-date "up to date")
      (:restarted "restarted")
      (:handing-over
       (format "handing over %d workspace%s (%d busy%s)"
               (plist-get value :workspaces)
               (if (= (plist-get value :workspaces) 1) "" "s")
               (plist-get value :busy)
               (if (plist-get value :forced) ", forced" "")))
      (:shims
       (let ((bounces (plist-get value :bounces)))
         (format "%d bounced now, %d registered"
                 (cl-count :bounced-now bounces
                           :key (lambda (b) (plist-get (plist-get b :when) :arm)))
                 (cl-count :registered bounces
                           :key (lambda (b) (plist-get (plist-get b :when) :arm))))))
      (:reload-pushed
       (format "reload pushed to %d" (plist-get value :recipients)))
      (:deferred-to-successor "deferred to the successor")
      (arm (format "%S" arm)))))

(defun agent-repl-verbs--deploy-on-success (force value)
  "Report the DeploySuccess VALUE of a deploy sent with FORCE.
One log record per component outcome, then one echo-area line naming every
component's decision."
  (let ((components (plist-get value :components)))
    (dolist (outcome components)
      (agent-repl--info '(:agent-repl-central "a deploy is daemon administration spanning every workspace") "elisp.verbs.deploy-component component=%s build=%s decision=%S detail=%S"
                        (agent-repl-verbs--deploy-component-name (plist-get outcome :component))
                        (plist-get outcome :build)
                        (plist-get (plist-get outcome :outcome) :arm)
                        (plist-get (plist-get outcome :outcome) :value)))
    (message "agent-repl: deploy%s: %s"
             (if force " (forced)" "")
             (if components
                 (mapconcat
                  (lambda (outcome)
                    (format "%s %s"
                            (agent-repl-verbs--deploy-component-name (plist-get outcome :component))
                            (agent-repl-verbs--deploy-outcome-phrase (plist-get outcome :outcome))))
                  components "; ")
               "no component was listed"))))

(defun agent-repl-verbs--deploy-refusal-sentence (cause)
  "Return the DeployError CAUSE, a (:arm ARM :value V), as its sentence."
  (let ((value (plist-get cause :value)))
    (pcase (plist-get cause :arm)
      (:build-failed
       (format "the %s build failed, so nothing was deployed: %s%s"
               (plist-get value :step) (plist-get value :detail)
               (if (string-empty-p (or (plist-get value :log) ""))
                   ""
                 (format " (log: %s)" (plist-get value :log)))))
      (:already-deploying "a deploy is already running; ask again when it ends")
      (:already-rolling-out
       (format "a handover is already in flight, waiting on %s"
               (or (and (plist-get value :waiting-on)
                        (mapconcat #'identity (plist-get value :waiting-on) ", "))
                   "no workspace")))
      (:joining "this daemon is a successor still joining a handover")
      (:service-restart-failed
       (format "%s did not come back onto the fresh build: %s"
               (agent-repl-verbs--deploy-component-name (plist-get value :component))
               (plist-get value :detail)))
      (:install-failed
       (format "the %s artifact could not be installed, so nothing was restarted: %s"
               (agent-repl-verbs--deploy-component-name (plist-get value :component))
               (plist-get value :detail)))
      (arm (format "the daemon refused with %S" arm)))))

(defun agent-repl-verbs--deploy-on-error (value)
  "Report the DeployError VALUE loudly, and claim it.
Every arm is an ERROR record carrying its fields and an echo-area line
carrying its detail."
  (let ((cause (plist-get value :cause)))
    (agent-repl--error '(:agent-repl-central "a deploy is daemon administration spanning every workspace") "elisp.verbs.deploy-refused arm=%S fields=%S"
                       (plist-get cause :arm) (plist-get cause :value))
    (message "agent-repl: deploy refused: %s"
             (agent-repl-verbs--deploy-refusal-sentence cause))
    t))

(defun agent-repl-deploy (&optional force)
  "Ask the daemon to deploy the checkout's current source.
The daemon builds every component and restarts or reloads exactly what is
out of date; an unforced deploy ENDS NO TURN.  With a prefix argument the
deploy is FORCED: every out-of-date shim is bounced at once and an
out-of-date daemon hands over at once, ending running turns, so it asks
first.  The answer is reported in one echo-area line plus a log record per
component; every refusal is reported loudly with its detail."
  (interactive "P")
  (let ((force (and force t)))
    (when (and force
               (not (yes-or-no-p
                     "A forced deploy ends every running turn in an out-of-date workspace.  Deploy anyway? ")))
      (agent-repl--info '(:agent-repl-central "a deploy is daemon administration spanning every workspace") "elisp.verbs.deploy-force-declined")
      (user-error "agent-repl: forced deploy cancelled"))
    (agent-repl--info '(:agent-repl-central "a deploy is daemon administration spanning every workspace") "elisp.verbs.deploy-requested force=%s" force)
    (agent-repl-verbs--send
     #'agent-repl-rpc-deploy (agent-repl-verbs--conn)
     (list :force force)
     :op "deploy"
     :timeout agent-repl-deploy-timeout-seconds
     :on-success (lambda (value) (agent-repl-verbs--deploy-on-success force value))
     :on-error #'agent-repl-verbs--deploy-on-error)))

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

(defun agent-repl-interrupt-turn (&optional ws)
  "Interrupt the running turn of the current workspace (`C-c C-k').
The clean turn stop the footer's stop button drives: the in-flight vendor
query is ended, the session is left standing and the agent is not resumed.
When there is no turn in flight the daemon answers `nothing_running' and
this is a message, not an error; when the stop would also end live detached
agents the user is asked first."
  (interactive)
  (agent-repl-verb-interrupt (agent-repl-verbs--target-ws ws "Interrupt turn in workspace: ")))

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

(defconst agent-repl-verbs--register-repository-sentences
  '((:not-in-a-repository . "that path is not inside a git repository")
    (:unreadable-path . "that path cannot be read"))
  "The sentence each `RegisterRepositoryError' arm is reported with.
Both arms are EMPTY -- the path the verb sent is the only fact involved --
so the sentence is the whole of what the daemon can be quoted as saying,
and the path is added by the caller that knows it.")

(defun agent-repl-verbs--register-repository-on-error (path value)
  "Claim a `RegisterRepositoryError' refusal of PATH from VALUE, else nil.
An arm this command has a sentence for is reported as that sentence; any
other arm answers nil and falls through to the arm-generic reporting, so a
refusal the daemon adds later still reaches the user correctly."
  (let* ((arm (agent-repl-verbs--refusal-arm value))
         (sentence (cdr (assq (plist-get arm :arm)
                              agent-repl-verbs--register-repository-sentences))))
    (when sentence
      (agent-repl--warn agent-repl--global-log-scope
                        "elisp.verbs.register-repository-refused path=%S arm=%S"
                        path (plist-get arm :arm))
      (agent-repl-workspace-progress-report
       :register-repository :failed (format "%s: %s" sentence path))
      t)))

(defun agent-repl-verbs--workspace-display-name (ref)
  "Return the name a `WorkspaceRef\=' REF is reported to the user under.
A `WorkspaceRef\=' carries only an id and a dir, and the id is an opaque
echo token nobody reads aloud, so the name is the directory\='s last
segment -- the SAME derivation the daemon makes when a registration
supplies no name of its own, so the ack names the workspace the roster
will draw."
  (let ((dir (plist-get ref :dir)))
    (if (and (stringp dir) (not (string-empty-p dir)))
        (file-name-nondirectory (directory-file-name dir))
      "")))

(defun agent-repl-verbs--register-repository-read-path ()
  "Read the file the repository is registered FROM.
The default is the current buffer\='s own file, because the buffer you are
looking at is almost always in the repository you mean; a buffer visiting
no file falls back to `default-directory\=', which `read-file-name\=' opens
there."
  (let ((default (or (buffer-file-name) default-directory)))
    (read-file-name "Register repository from file: "
                    (file-name-directory default) default)))

(defun agent-repl-register-repository ()
  "Register the repository the picked FILE is inside (`SPC j .\=').
A repository used to enter the roster only as a SIDE EFFECT of registering
a workspace in it, so a checkout nobody had worked in yet could not be
named at all -- and the static create, which is the one mode that picks a
repository by hand, could offer only repositories some workspace had
already minted.

THE GESTURE IS PICKING A FILE, not naming a repository root: the daemon
resolves the repository from any path inside it, so the file you happen to
be looking at is a complete answer.  Registering one already known is
success and says so.

THE REPOSITORY\='S MAIN WORKTREE COMES WITH IT, as an open workspace (owner
ruling, 2026-09-14).  The first landing registered the repository ALONE,
and a repository with no workspace is not selectable: `SPC p p\='
(`agent-repl-switch-to-project\=') completes over LIVE WORKSPACES, so
registering the repository you were standing in still left you unable to
switch to it.  The daemon registers the main worktree through the same
registration `SPC TAB C-n\=' runs, so the row is an ordinary workspace row
in every respect.

AND THIS COMMAND SWITCHES TO IT (owner ruling, 2026-09-18).  \"Register
the repository I am standing in\" is a request to be ATTACHED TO its main
worktree, not merely told it now exists in a roster the user must still
find and click into -- a version of this command that only messaged left
the owner where they started, which read as \"nothing happened\" even
though the row was there.

THE SWITCH IS ONTO THE ROSTER\='S OWN TAB, and it leaves the workspace the
command was run FROM exactly as it was.  The arrival is claimed as
`registered\=' so the roster materializes the new workspace the same way
it materializes a created one -- tab and panels -- and the landing is
`agent-repl-verbs--land-on-tab\=', which activates that tab by identity.
An earlier attempt landed by DIRECTORY instead and re-pointed the origin
workspace at the registered repository; that function\='s docstring holds
the whole of why.

The ack still names both facts because the two halves are independently
new: the repository may be already known while the workspace is freshly
opened."
  (interactive)
  (let ((path (agent-repl-verbs--register-repository-read-path)))
    ;; The gesture leaves a mark the instant it is made, exactly as every
    ;; other way of putting a workspace on the roster now does.
    (agent-repl-workspace-progress-report :register-repository :requested path)
    (agent-repl-verbs--send
     #'agent-repl-rpc-register-repository (agent-repl-verbs--conn)
     (list :path path)
     :op "register-repository"
     :on-success
     (lambda (value)
       (let ((dir (plist-get (plist-get value :repository) :dir))
             (workspace (plist-get value :workspace)))
         (agent-repl-workspace-progress-report
          :register-repository :completed
          (if (plist-get value :already-known) "already known" "registered")
          dir
          (agent-repl-verbs--workspace-display-name workspace)
          (if (plist-get value :workspace-already-known)
              "already known" "opened"))
         ;; The roster is where the workspace is actually born in this
         ;; editor, and it only opens panels for an arrival some verb
         ;; claimed -- so this one claims it before standing on it, exactly
         ;; as `SPC TAB C-n\=' does.  `already known\=' is an answer about
         ;; MINTING and never a reason to leave the user where they were.
         (agent-repl--panels-note-arrival-reason
          (plist-get workspace :id) "registered")
         (agent-repl-verbs-select-minted
          workspace #'agent-repl-verbs--land-on-tab)))
     :on-error
     (lambda (value) (agent-repl-verbs--register-repository-on-error path value)))))

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

(defconst agent-repl-verbs--create-log-scope
  '(:agent-repl-central "workspace creation and daemon administration can precede workspace ownership")
  "The central log scope every creation command records under.
A create runs before the workspace it makes exists, so it can own no
workspace log of its own.")

(defun agent-repl-verbs--create-invoked (command)
  "Record that the creation COMMAND was invoked, before it asks anything.
A create used to log nothing until its questions were answered, so a
prompt that was abandoned, hidden or never shown left no trace at all and
was indistinguishable from the command never running."
  (agent-repl--info agent-repl-verbs--create-log-scope
                    "elisp.verbs.create-invoked command=%s" command))

(defun agent-repl-verbs--read-for (command read reader)
  "Answer COMMAND's READ question by calling READER, recording a failed read.
READ names the question (`repository\=', `name\=', `prompt\=').  A quit
out of the minibuffer, or a `user-error\' refusing the read, is recorded
with the signal and its message and then RE-SIGNALLED unchanged: the log
says where the create stopped, and the caller still sees it stop."
  (condition-case err
      (funcall reader)
    ((quit user-error)
     (agent-repl--info agent-repl-verbs--create-log-scope
                       "elisp.verbs.create-read-abandoned command=%s read=%s signal=%s detail=%S"
                       command read (car err) (error-message-string err))
     (signal (car err) (cdr err)))))

(defun agent-repl-verbs--read-name (refusal)
  "Read a REQUIRED workspace name, refusing a blank one with REFUSAL."
  (let ((name (string-trim (read-string "Name: "))))
    (when (string-empty-p name)
      (user-error "agent-repl: %s" refusal))
    name))

(defun agent-repl-verbs--create-standard (command mode child)
  "Create a standard workspace in MODE, as a CHILD of the current one when set.
MODE is `dynamic\=' -- a prompt and nothing else, the repository read off
the current workspace\='s roster section -- or `static\=', which asks for a
repository and a REQUIRED name and sends no prompt at all.

CHILD non-nil makes the new workspace a child of the current workspace:
its parent is the current workspace\='s ref, so its merge target is the
parent\='s worktree and branch rather than the repo\='s main checkout.
Owner ruling, 2026-09-12: the child variants are their own commands
rather than a prefix argument, and they share this body so the two
spellings of one mode cannot drift apart.

THE NEW WORKSPACE IS SELECTED.  Creating one is a statement about where
you intend to work next, so this stands on it the moment the daemon
answers -- the same step registering a directory takes.

COMMAND is the interactive command running this body; its invocation is
recorded before any question is asked, and a question it abandons is
recorded before the abandonment propagates."
  (agent-repl-verbs--create-invoked command)
  (let* ((static (eq mode 'static))
         (repository (agent-repl-verbs--read-for
                      command 'repository
                      (lambda ()
                        (if static
                            (agent-repl-verbs--read-repository)
                          (agent-repl-verbs--dynamic-repository "a dynamic create")))))
         (name (when static
                 (agent-repl-verbs--read-for
                  command 'name
                  (lambda () (agent-repl-verbs--read-name "a static workspace IS its name")))))
         (prompt (unless static
                   (agent-repl-verbs--read-for
                    command 'prompt
                    (lambda () (agent-repl-verbs--read-prompt "Initial prompt: ")))))
         (parent (when child
                   (agent-repl-verbs--ref (agent-repl--ws-current-name)))))
    (agent-repl--info agent-repl-verbs--create-log-scope "elisp.verbs.create-standard mode=%s child=%s"
                      mode (and child t))
    (apply #'agent-repl-verb-create
           repository :standard
           :parent parent
           :select t
           (if (eq mode 'static)
               (list :name name)
             (list :initial-prompt
                   (unless (string-empty-p (string-trim prompt)) prompt))))))

(defun agent-repl-create-workspace ()
  "Create a DYNAMIC workspace (`SPC TAB n\='): a prompt, and nothing else.
Owner ruling, 2026-09-12.  This is the dynamic normal mode, so the only
question it asks is the prompt, which defaults to the composer\='s text.
The repository is the one the current workspace sits in, off that repo\='s
MAIN (default) branch -- an absent base ref IS that resolution -- and the
daemon mints the name.

It never makes a child: `agent-repl-create-child-workspace\=' is the child
variant, and naming a repository or a name by hand is the STATIC mode\='s
business (`agent-repl-create-workspace-static\=').

THE NEW WORKSPACE IS SELECTED."
  (interactive)
  (agent-repl-verbs--create-standard 'agent-repl-create-workspace 'dynamic nil))

(defun agent-repl-create-child-workspace ()
  "Create a dynamic CHILD workspace (`SPC TAB c\='): a prompt, and nothing else.
Owner ruling, 2026-09-12.  Exactly `agent-repl-create-workspace\=', except
the new workspace is a CHILD of the current one: its parent is the
current workspace\='s ref, so it merges back into the parent\='s worktree and
branch rather than the repo\='s main checkout.

THE NEW WORKSPACE IS SELECTED."
  (interactive)
  (agent-repl-verbs--create-standard 'agent-repl-create-child-workspace 'dynamic t))

(defun agent-repl-create-workspace-static ()
  "Create a STATIC workspace (`SPC TAB N\='): a repository and a name, no prompt.
Owner ruling, 2026-09-12.  This is the one creation mode that asks for a
repository, and the one that REQUIRES a name; it sends no initial prompt
at all, so the workspace comes up idle and waits for the user.

It never makes a child: `agent-repl-create-child-workspace-static\=' is the
child variant.

THE NEW WORKSPACE IS SELECTED, for the same reason the dynamic create\='s
is."
  (interactive)
  (agent-repl-verbs--create-standard 'agent-repl-create-workspace-static 'static nil))

(defun agent-repl-create-child-workspace-static ()
  "Create a static CHILD workspace (`SPC TAB C\='): a repository and a name.
Owner ruling, 2026-09-12.  Exactly `agent-repl-create-workspace-static\=',
except the new workspace is a CHILD of the current one and merges back
into the parent\='s worktree and branch.  It asks no prompt.

THE NEW WORKSPACE IS SELECTED."
  (interactive)
  (agent-repl-verbs--create-standard 'agent-repl-create-child-workspace-static 'static t))

(defun agent-repl-fork-workspace ()
  "Create a CHILD workspace forking the current one's conversation.
A fork without a parent is unrepresentable by construction, which is why
this is its own command rather than a flag on the plain create.

It is a DYNAMIC mode (owner ruling, 2026-09-12), so it asks for the
prompt alone: the repository is the current workspace's, off that repo's
main branch, and the daemon mints the name.

THE FORK IS SELECTED, exactly as a plain create is: you forked in order
to work in the fork.

`agent-repl-fork-workspace-static\=' is the NAMED variant."
  (interactive)
  (agent-repl-verbs--create-fork 'agent-repl-fork-workspace 'dynamic))

(defun agent-repl-fork-workspace-static ()
  "Fork the current workspace into a NAMED child (`SPC TAB F\='), no prompt.
Exactly `agent-repl-fork-workspace\=' -- the repository is the current
workspace\='s, the parent is the current workspace, the conversation is
forked, the fork is SELECTED -- except it asks for a REQUIRED name instead
of a prompt and sends NO initial prompt, so the fork comes up idle on the
forked conversation.  It relates to the fork as `SPC TAB N\=' relates to
`SPC TAB n\=', but it never asks for a repository: a fork\='s repository is
its parent\='s by construction."
  (interactive)
  (agent-repl-verbs--create-fork 'agent-repl-fork-workspace-static 'static))

(defun agent-repl-verbs--create-fork (command mode)
  "Fork the current workspace\='s conversation into a CHILD, in MODE.
MODE is `dynamic\=' -- a prompt, and the daemon mints the name -- or
`static\=', a REQUIRED name and no prompt at all.  Everything else is the
fork\='s: the repository is the current workspace\='s, the parent is the
current workspace, and the fork is selected.  Both fork commands share
this body so the two spellings cannot drift apart.

COMMAND is the interactive command running this body; its invocation is
recorded before any question is asked, and a question it abandons is
recorded before the abandonment propagates."
  (agent-repl-verbs--create-invoked command)
  (let* ((static (eq mode 'static))
         (repository (agent-repl-verbs--read-for
                      command 'repository
                      (lambda ()
                        (agent-repl-verbs--dynamic-repository
                         (if static "a named fork" "a fork")))))
         (name (when static
                 (agent-repl-verbs--read-for
                  command 'name
                  (lambda () (agent-repl-verbs--read-name "a named fork IS its name")))))
         (prompt (unless static
                   (agent-repl-verbs--read-for
                    command 'prompt
                    (lambda () (agent-repl-verbs--read-prompt "Initial prompt: ")))))
         (parent (agent-repl-verbs--ref (agent-repl--ws-current-name))))
    (agent-repl--info agent-repl-verbs--create-log-scope "elisp.verbs.create-fork mode=%s" mode)
    (apply #'agent-repl-verb-create
           repository :standard
           :parent parent :fork t
           :select t
           (if static
               (list :name name)
             (list :initial-prompt
                   (unless (string-empty-p (string-trim prompt)) prompt))))))

;;;; ---- One-shots --------------------------------------------------------
;;
;; ONE-SHOTS RIDE THE WIRE.  Emacs supplies the prompt, the model and the
;; parentage; the DAEMON owns naming, the worktree and prompt decoration.
;; Everything the old doom / explanation-engine one-shot commands did by
;; hand is now one CreateWorkspace request whose form arm says one_shot.
;;
;; THERE IS ONE ONE-SHOT COMMAND, because there is no finish to choose
;; between (owner ruling, 2026-09-12): the repository states in one file
;; what is to be done on completion, and the agent carries it out.

(defcustom agent-repl-oneshot-model-candidates '("opus" "sonnet" "haiku")
  "The models offered when creating a ONE-SHOT workspace, in order.
One-shots ride the wire -- Emacs supplies prompt, model and parentage
through the dedicated one-shot creation form and the daemon owns naming,
the worktree and decoration -- so this list
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

(defun agent-repl-create-oneshot (&optional pick-model)
  "Create a ONE-SHOT workspace for a commission read from the minibuffer.
A one-shot is a DYNAMIC mode (owner ruling, 2026-09-12): it asks for its
commission and nothing else -- the repository is the current workspace\='s,
off that repo\='s main branch, and the daemon mints the name.

There is no finish to choose.  The repository states, in one plain-English
file of its own policy directory, what is to be done when the work is
done; the daemon appends that to the commission and the agent carries it
out itself.

A prefix argument PICK-MODEL asks for the model."
  (interactive "P")
  (agent-repl-verbs--create-invoked 'agent-repl-create-oneshot)
  (let* ((command 'agent-repl-create-oneshot)
         (repository (agent-repl-verbs--read-for
                      command 'repository
                      (lambda () (agent-repl-verbs--dynamic-repository "a one-shot"))))
         (prompt (agent-repl-verbs--read-for
                  command 'prompt
                  (lambda ()
                    (let ((text (agent-repl-verbs--read-prompt "One-shot commission: ")))
                      (when (string-empty-p (string-trim text))
                        (user-error "agent-repl: a one-shot IS its prompt"))
                      text))))
         (model (and pick-model
                     (agent-repl-verbs--read-for
                      command 'model #'agent-repl-verbs--read-model))))
    (agent-repl--info agent-repl-verbs--create-log-scope "elisp.verbs.create-one-shot mode=dynamic model=%S" model)
    (agent-repl-verb-create
     repository :one-shot
     :prompt prompt
     :model model)))

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
