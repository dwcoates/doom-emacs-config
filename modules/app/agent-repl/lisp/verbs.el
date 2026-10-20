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
;;                    Emacs merges only a workspace's OWN branch, as a
;;                    one-shot (`one_shot.own_branch') that closes it.
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
(require 'seq)

(declare-function agent-repl-popup-show "popup" (buffer))
(declare-function agent-repl--panels-note-arrival-reason "agent-repl-panels"
                  (id reason))
(declare-function agent-repl--path-canonical "agent-repl-core" (path))
(declare-function agent-repl--now-unix-ms "agent-repl-core" ())
(declare-function agent-repl--meta-wrap "agent-repl-core" (text))
(declare-function agent-repl--repo-key-for-dir "agent-repl-workspace" (dir))
(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--global-state-file "agent-repl-core" (relative))
(declare-function agent-repl--read-sexp-file-if-exists "history" (file))
(declare-function agent-repl--write-sexp-file "history" (file data))
(declare-function agent-repl--info "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--webview-window "agent-repl-panels" (ws))
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
(declare-function agent-repl--when-minibuffer-free "agent-repl-workspace" (scope label key fn))
(declare-function agent-repl--pseudo-workspace-name-p "agent-repl-core" (ws))
(declare-function agent-repl--log-note-workspace-departing "agent-repl-core" (ws))
(declare-function agent-repl--log-forget-workspace-departure "agent-repl-core" (ws))
(declare-function agent-repl--read-known-workspace "agent-repl-keybindings" (prompt))
(declare-function agent-repl--kill-one-workspace "agent-repl-workspace" (ws &optional preserve))
(declare-function agent-repl-host-ref "agent-repl-host" (ws))
(declare-function agent-repl-host-conn-or-primary "agent-repl-host" (ws))
(declare-function agent-repl-host-faults "agent-repl-host" (ws))
(declare-function agent-repl-host-handle-refusal "agent-repl-host" (ws arm))
(declare-function agent-repl-host-route-handover "agent-repl-host" (ws arm slug))
(declare-function agent-repl-host-take-restart-hold "agent-repl-host" (ws))
(declare-function agent-repl-host-release-restart-hold "agent-repl-host" (ws reason))
(declare-function agent-repl-host-editor-switch-count "agent-repl-host" ())
(defvar agent-repl-host-automatic-switch)
(declare-function agent-repl-switch-to-project "commands" (&optional project))
(declare-function agent-repl-link-primary "agent-repl-daemon-link" ())
(declare-function agent-repl-link-connect "agent-repl-daemon-link" ())
(declare-function agent-repl--read-input-buffer "agent-repl-input" (ws))
(declare-function agent-repl--input-rollback-offer "agent-repl-input" (ws offer))
(declare-function agent-repl--input-rollback-done "agent-repl-input" (ws success))
(declare-function agent-repl--input-rollback-refusal-text "agent-repl-input" (cause))
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
(declare-function agent-repl-rpc-set-workspace-hidden "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-swap-workspaces "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-move-workspace "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-deploy "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-update-shutdown-schedule "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-update-merge-queue "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-daemon-health "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-send-hidden-keepalives "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-session-health "agent-repl-rpc" (conn request &rest keys))

;; Defined by roster.el.  Declared, never defined here.
(declare-function agent-repl-roster-repository-of "agent-repl-roster" (id &optional roster))
(declare-function agent-repl-roster-walk "agent-repl-roster" (roster))
(declare-function agent-repl-roster-section-rows "agent-repl-roster" (section))
(declare-function agent-repl-roster-flatten "agent-repl-roster" (rows))
(declare-function agent-repl-roster-row-ref "agent-repl-roster" (row))
(declare-function agent-repl-roster-row-name "agent-repl-roster" (row))
(declare-function agent-repl-roster-row-closed-p "agent-repl-roster" (row))
(declare-function agent-repl-roster-drawn-tab-order "agent-repl-roster" ())
(declare-function agent-repl-host-request-switch "agent-repl-host" (ws trigger))
(defvar agent-repl-roster-view)
(defvar agent-repl-metaprompt-file)

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
      ;; A CLOSED WORKSPACE HAS NO TAB, and so no host entry, but it is still
      ;; a roster row carrying the daemon's ref -- and killing or nuking it is
      ;; an ordinary act (a close leaves the session running).
      (agent-repl-verbs--roster-ref ws)
      (progn
        (agent-repl--warn ws "elisp.verbs.no-ref ws=%s" ws)
        (user-error "agent-repl: workspace %s has no daemon identity yet" ws))))

(defun agent-repl-verbs--roster-ref (ws)
  "Return the ref of the roster row whose directory is WS\='s, or nil.
Matched by DIRECTORY, never by name: names collide across repositories."
  (let ((dir (agent-repl--ws-get ws :project-dir)))
    (when dir
      (let ((canonical (agent-repl--path-canonical dir)))
        (cl-some (lambda (row)
                   (let ((ref (agent-repl-roster-row-ref row)))
                     (and (plist-get ref :dir)
                          (string= canonical (agent-repl--path-canonical (plist-get ref :dir)))
                          ref)))
                 (agent-repl-verbs--all-rows))))))

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
  (or (agent-repl-host-conn-or-primary ws)
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

(defun agent-repl-verbs--lock-holder-how-text (how)
  "Word HOW, a decoded `LockHolderFailure' `how' oneof, as the helper failing.
The arm is the account: each case says what that arm carries, the same
sentence the shim and the daemon word it with."
  (let ((value (plist-get how :value)))
    (pcase (plist-get how :arm)
      (:spawn-failed (format "could not be spawned: %s" (plist-get value :os-error)))
      (:exited (format "exited with code %d before taking the lock%s"
                       (plist-get value :code)
                       (agent-repl-verbs--lock-holder-stderr (plist-get value :stderr))))
      (:signaled (format "was killed by %s before taking the lock%s"
                         (plist-get value :signal)
                         (agent-repl-verbs--lock-holder-stderr (plist-get value :stderr))))
      (:misanswered (format "answered %S instead of \"locked\" and was killed"
                            (plist-get value :line)))
      (:silent (format "gave no \"locked\" answer within %d ms and was killed"
                       (plist-get value :timeout-ms)))
      (arm (error "agent-repl-verbs: LockHolderFailure arm %S has no wording" arm)))))

(defun agent-repl-verbs--lock-holder-stderr (stderr)
  "Return STDERR as a parenthesized suffix, or the empty string when empty."
  (if (string-empty-p stderr) "" (format " (%s)" stderr)))

;; A FAMILY STAYS IN ONE LIST OF THE ROSTER (agentrepl.v1 family.proto): a
;; workspace with open children is not closed, killed, nuked, hidden or
;; merged closed, and no open child is made, re-opened or shown under a parent
;; that is hidden, closed or closing.  Both refusals are ANSWERS, not faults,
;; and every verb that can meet one words it here, naming the workspaces in
;; the way, as the sidebar words it (webapp `rpc/family-refusal.ts').

(defun agent-repl-verbs--ref-label (ref)
  "Name the `WorkspaceRef' REF in words: its directory's last part, else its id."
  (let ((dir (plist-get ref :dir)))
    (if (and (stringp dir) (not (string-empty-p dir)))
        (file-name-nondirectory (directory-file-name dir))
      (plist-get ref :id))))

(defun agent-repl-verbs-family-sentence (op keyword fields)
  "Word the family refusal KEYWORD of verb OP from its FIELDS, or nil.
KEYWORD `:has-open-children' names the parent's open children, and for a
merge (OP \"merge\") asks for them to be resolved and closed before a
one-shot merge; `:parent-not-open' names the parent and what to do about
it.  Nil for any other arm, which is not a family refusal."
  (pcase keyword
    (:has-open-children
     (let* ((children (plist-get fields :children))
            (names (mapconcat #'agent-repl-verbs--ref-label children ", "))
            (one (null (cdr children))))
       (if (equal op "merge")
           (format "%s still has open child workspaces (%s); resolve and close all of them before a one-shot merge"
                   (agent-repl-verbs--ref-label (plist-get fields :parent)) names)
         (format "%s refused: %s %s still open under this workspace; close %s first"
                 op names (if one "is" "are") (if one "it" "them")))))
    (:parent-not-open
     (let ((parent (agent-repl-verbs--ref-label (plist-get fields :parent))))
       (pcase (plist-get (plist-get fields :state) :arm)
         (:hidden (format "%s refused: its parent %s is hidden; show the parent first" op parent))
         (:closed (format "%s refused: its parent %s is closed; reopen the parent first" op parent))
         (:closing (format "%s refused: its parent %s closes once its queued merge lands" op parent))
         (state (error "agent-repl-verbs: parent_not_open state %S has no wording" state)))))))

(defun agent-repl-verbs--on-refusal (ws op value)
  "Report a daemon-authored refusal of OP for WS, or route it to the handover.
The two handover arms are silent by design.  `worktree_unrestorable' is an
ordinary answer: logged at INFO and drawn as the daemon's own sentence.
Every other arm is logged at WARNING with its keyword and its own fields in
the context, and drawn to the user as the verb, the word refused, the arm
keyword, and the fields."
  (let* ((arm (agent-repl-verbs--refusal-arm value))
         (keyword (plist-get arm :arm)))
    (cond
     ((agent-repl-host-route-handover ws arm (format "elisp.verbs.%s-handover-refusal" op)))
     ((eq keyword :worktree-unrestorable)
      ;; NOT A FAULT OF AGENT-REPL.  The user picked a row whose directory is
      ;; gone with nothing left to restore it from (its branch is gone, none
      ;; was recorded, or its repository is gone); a deleted worktree whose
      ;; branch survives -- a merged workspace's included -- is restored by
      ;; the daemon and never reaches here.  So it is an ordinary answer, recorded at INFO, and the
      ;; daemon's own sentence -- which names the directory, the branch and
      ;; the reason -- is what the user reads, not an arm keyword.
      (let ((fields (plist-get arm :value)))
        (agent-repl--info ws (format "elisp.verbs.%s-unrestorable ws=%%s dir=%%S branch=%%S" op)
                          ws (plist-get fields :dir) (plist-get fields :branch))
        (message "%s refused: %s" op (plist-get fields :detail))))
     ((memq keyword '(:has-open-children :parent-not-open))
      ;; A FAMILY RULE ANSWERED: recorded at INFO, said in words.
      (agent-repl--info ws (format "elisp.verbs.%s-family-refused ws=%%s arm=%%S fields=%%S" op)
                        ws keyword (plist-get arm :value))
      (message "agent-repl: %s" (agent-repl-verbs-family-sentence op keyword (plist-get arm :value))))
     ((eq keyword :lock-holder-unavailable)
      ;; THE ONE ARM WHOSE KEYWORD WOULD MISLEAD ON ITS OWN.  Nobody owns
      ;; the conversation: the shim's own lock helper failed, and the binary
      ;; plus how it failed are the whole remediation, so they are said in
      ;; words rather than as a raw plist.
      (let* ((fields (plist-get arm :value))
             (failure (plist-get fields :failure)))
        (agent-repl--warn ws (format "elisp.verbs.%s-refused ws=%%s arm=%%S fields=%%S" op)
                          ws keyword fields)
        (message "%s refused: the shim's lock helper %s %s; no other process owns this conversation"
                 op (plist-get failure :binary)
                 (agent-repl-verbs--lock-holder-how-text (plist-get failure :how)))))
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

(cl-defun agent-repl-verbs--send-op (op-id rpc conn request
                                       &rest keys
                                       &key on-success on-error on-transport-failure
                                       &allow-other-keys)
  "`agent-repl-verbs--send' for a verb that registered the progress op OP-ID.
EVERY ANSWER THAT ENDS THE OP ON THIS RPC FORGETS IT FIRST: a success, a
refusal and no answer at all each mean no further push will ever arrive
for OP-ID, so the registration is retired before ON-SUCCESS, ON-ERROR or
ON-TRANSPORT-FAILURE runs (an ON-ERROR still claims its arm by answering
non-nil).  Only an `accepted' ack keeps it: that op\='s end is still on its
way.  RPC, CONN, REQUEST and the remaining KEYS pass through unchanged."
  (let ((forget (lambda () (agent-repl-mutation-progress-forget op-id)))
        (passed (cl-loop for (key value) on keys by #'cddr
                         unless (memq key '(:on-success :on-error :on-transport-failure))
                         append (list key value))))
    (apply #'agent-repl-verbs--send rpc conn request
           :on-success (lambda (value)
                         (funcall forget)
                         (when on-success (funcall on-success value)))
           :on-error (lambda (value)
                       (funcall forget)
                       (and on-error (funcall on-error value)))
           :on-transport-failure (lambda (detail)
                                   (funcall forget)
                                   (when on-transport-failure
                                     (funcall on-transport-failure detail)))
           passed)))

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
;; these helpers only pick values out of the last decoded view.  A row's own
;; fields are read through roster.el's ONE set of row readers
;; (`agent-repl-roster-row-ref', `agent-repl-roster-row-name',
;; `agent-repl-roster-row-closed-p'), never a second spelling here.

(defun agent-repl-verbs--roster-or-view (roster)
  "Return ROSTER, or `agent-repl-roster-view' when ROSTER is nil.
THE ONE DEFAULTING every roster reader here shares: a nil ROSTER means the
last accepted push, and nil again before roster.el has loaded or pushed."
  (or roster (and (boundp 'agent-repl-roster-view) agent-repl-roster-view)))

(defun agent-repl-verbs--all-rows (&optional roster)
  "Return every row of ROSTER (default `agent-repl-roster-view'), in walk order.
THE ONE WALK is `agent-repl-roster-walk' -- the tab bar's own -- so a verb
can never look for a row in fewer sections than the bar draws tabs from."
  (let ((roster (agent-repl-verbs--roster-or-view roster)))
    (mapcar (lambda (entry) (plist-get entry :row))
            (agent-repl-roster-walk roster))))

(defun agent-repl-verbs--closed-rows (&optional roster)
  "Return the CLOSED rows of ROSTER -- exactly what OpenWorkspace can reopen."
  (cl-remove-if-not #'agent-repl-roster-row-closed-p
                    (agent-repl-verbs--all-rows roster)))

(defun agent-repl-verbs--repo-sections (&optional roster)
  "Return ROSTER's repository sections, in the resolver's order."
  (let ((roster (agent-repl-verbs--roster-or-view roster)))
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
                       (equal (plist-get (agent-repl-roster-row-ref row) :id)
                              (plist-get ref :id)))
                     (agent-repl-roster-flatten
                      (agent-repl-roster-section-rows section))))
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
     (list :workspace ref :requester (list :arm :user))
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

(defconst agent-repl-verbs--teardown-scope
  '(:agent-repl-central "a teardown's end arrives after its workspace's tab is gone")
  "The log scope for an accepted kill's or nuke's end.
The tab, and with it the workspace's own log routing, is gone by the time
the daemon pushes the end, so the end is recorded centrally.")

(cl-defun agent-repl-verbs--send-teardown (ws op rpc &key on-error)
  "Send the teardown verb OP (\"kill\" or \"nuke\") for WS through RPC.
THE TAB CLOSES ON THE DAEMON\='S ACK, NOT ON THE TEARDOWN\='S END (owner
ruling, 2026-09-29).  The request carries a minted op id, so the daemon
answers `accepted' the moment it has refused nothing and marked the
workspace closed -- the roster already says so everywhere -- and tears the
session (and for a nuke the worktree and branch) down in the background.
The tab goes on that ack; the teardown\='s end arrives on the progress
channel, where a failure is recorded at ERROR and echoed.

A daemon that answered `success' synchronously (an op id it did not honor)
tore everything down already, so the tab goes then too.  ON-ERROR, when
given, sees a synchronous refusal first and may claim it, as the shared
dispatcher\='s ON-ERROR does.  Every path that ends the op without a
progress push forgets it."
  (let ((ref (agent-repl-verbs--ref ws))
        (op-id (agent-repl-mutation-progress-new-op-id)))
    (agent-repl-mutation-progress-register
     op-id
     :on-succeeded
     (lambda (_)
       (agent-repl--info agent-repl-verbs--teardown-scope
                         "elisp.verbs.%s-teardown-finished ws=%s op-id=%s" op ws op-id))
     :on-failed
     (lambda (arm value)
       (agent-repl--error agent-repl-verbs--teardown-scope
                          "elisp.verbs.%s-teardown-failed ws=%s op-id=%s arm=%S value=%S"
                          op ws op-id arm value)
       (message "agent-repl: %s of %s FAILED after its tab closed: %s"
                op ws (agent-repl-verbs--teardown-failure-sentence arm value))))
    (agent-repl-verbs--send-op
     op-id rpc (agent-repl-verbs--conn ws)
     (list :workspace ref :op-id op-id)
     :ws ws :op op
     :on-accepted (lambda (_) (agent-repl-verbs--teardown-tab ws op))
     :on-success (lambda (_) (agent-repl-verbs--teardown-tab ws op))
     :on-error on-error)))

(defun agent-repl-verbs--teardown-failure-sentence (arm value)
  "Word a teardown failure ARM with VALUE for the echo area.
`:internal' carries the daemon\='s own sentence; `:refusal' a typed error,
worded as the refusal dispatcher words one."
  (pcase arm
    (:internal value)
    (:refusal
     (let* ((refusal (agent-repl-verbs--refusal-arm value))
            (keyword (plist-get refusal :arm)))
       (format "%s%s"
               (if keyword (substring (symbol-name keyword) 1) "unstated")
               (agent-repl-verbs--refusal-fields refusal))))
    (_ (format "%S %S" arm value))))

(defun agent-repl-verb-kill (ws)
  "Kill WS's session by force.  The worktree and branch survive.
The tab closes on the daemon\='s ack (`agent-repl-verbs--send-teardown')."
  (agent-repl-verbs--send-teardown ws "kill" #'agent-repl-rpc-kill-workspace))

(defun agent-repl-verb-nuke (ws)
  "Destroy WS: its session, its worktree and its branch.  Unrecoverable.
The tab closes on the daemon\='s ack (`agent-repl-verbs--send-teardown')."
  ;; The worktree goes as soon as the daemon acts, which is BEFORE the
  ;; answer arrives and well before the tab teardown; records written in
  ;; between are attributed to a workspace whose registered directory has
  ;; already gone.  The order is what explains that, so the order is
  ;; declared at the moment it is given rather than when it completes.
  (agent-repl--log-note-workspace-departing ws)
  (agent-repl-verbs--send-teardown
   ws "nuke" #'agent-repl-rpc-nuke-workspace
   ;; A REFUSED nuke destroyed nothing, so the order above is withdrawn and
   ;; the arm is left unclaimed for the generic refusal handling.
   :on-error (lambda (_value)
               (agent-repl--log-forget-workspace-departure ws)
               nil)))

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
    ;; No ON-ERROR: the arm is NOT claimed, so the daemon-authored refusal is
    ;; worded by the shared dispatcher, and the op is retired by
    ;; `agent-repl-verbs--send-op' on every path, an unanswered open's included.
    (agent-repl-verbs--send-op
     op-id #'agent-repl-rpc-open-workspace (agent-repl-verbs--conn)
     (list :workspace ref :op-id op-id)
     :op "open"
     :on-success (lambda (_)
                   ;; The tab arrives on the roster push, and when it does it
                   ;; opens its panels as a re-open (owner ruling,
                   ;; 2026-09-13, item 6).
                   (agent-repl--panels-note-arrival-reason
                    (plist-get ref :id) "reopened")
                   (agent-repl-workspace-progress-report :open :completed name)))))

(defun agent-repl-verb-merge (ws)
  "Enqueue a ONE-SHOT merge of WS's own branch, run in WS.
Success means ENQUEUED.  The request's mode is `one_shot' with the
`own_branch' landing, asked by the user and joining the back of the
queue: WS is the workspace the merge runs in and whose branch lands,
and WS closes once its branch lands.  A failed merge never closes it,
and its failure is handed back to WS\='s conversation.

Emacs holds NO merge state whatsoever: the merge's whole life from the
queue onward is WS's own feed merge bubble, footer and roster row."
  (let ((ref (agent-repl-verbs--ref ws)))
    (agent-repl--info ws "elisp.verbs.merge-one-shot ws=%s" ws)
    (agent-repl-verbs--send
     #'agent-repl-rpc-merge-workspace (agent-repl-verbs--conn ws)
     (list :workspace ref
           :requester (list :arm :user)
           :mode (list :arm :one-shot
                       :value (list :landing (list :arm :own-branch :value nil)))
           :placement (list :arm :back))
     :ws ws :op "merge"
     :on-success (lambda (_) (message "merge enqueued")))))

(defun agent-repl-verb-restart (ws)
  "Restart WS's backend and webapp page, immediately.
There is ONE mode.  The DAEMON owns everything the restart entails: it
interrupts the running turn and stops all detached work with bounded calls,
bounces the workspace's shim (hard-killing whatever did not stop), resumes
the same session with a fresh vendor-start run, releases prompts held
\"after reconnect\", and reloads the workspace's webapp page.

THE COMPOSER CLOSES THE MOMENT THE RESTART IS SENT, rather than waiting for
the `restarting' arm to come back over the host stream.  The send is async
and the bounce is later still, so between the two the gate would otherwise
read the dying generation's `:open' and take a prompt the daemon is about
to refuse (`agent-repl-host-take-restart-hold').  A REFUSED restart
bounces nothing, so its arm releases the hold again.

THE ANSWER IS THE OUTCOME (owner ruling, 2026-10-08: \"i expect it to just
restart instantly\").  The daemon answers once the restart happened, so a
success says nothing -- the fresh session and the page reload are what the
user sees -- and a relaunch that FAILED is its `relaunch_failed' arm,
recorded at ERROR and said in the echo area as a failure, never as a
refusal.

THE CALL HAS NO TIMEOUT (owner ruling, 2026-10-10).  A restart over a merge
in the middle of a step waits for that step -- a test run, a repair turn --
to finish before it happens, and the answer waits with it, however long that
takes; a dropped connection still ends the call (`agent-repl-connect-unary')."
  (let ((ref (agent-repl-verbs--ref ws)))
    (agent-repl-host-take-restart-hold ws)
    (agent-repl-verbs--send
     #'agent-repl-rpc-restart-workspace (agent-repl-verbs--conn ws)
     (list :workspace ref)
     :ws ws :op "restart"
     :timeout :none
     :on-success
     (lambda (_)
       (agent-repl--info ws "elisp.verbs.restart-done ws=%s" ws))
     :on-error
     (lambda (value)
       ;; Every error arm gives back the hold taken for a bounce that did
       ;; not happen.  Only `relaunch_failed' is CLAIMED: it is a failure,
       ;; not a refusal, so it is said as one; every other arm still reports
       ;; itself through the generic refusal path.
       (agent-repl-host-release-restart-hold ws "restart-refused")
       (let ((arm (agent-repl-verbs--refusal-arm value)))
         (when (eq (plist-get arm :arm) :relaunch-failed)
           (let ((detail (plist-get (plist-get arm :value) :detail)))
             (agent-repl--error ws "elisp.verbs.restart-failed ws=%s detail=%S" ws detail)
             (message "agent-repl: the restart failed: %s" detail)
             t)))))))

(defun agent-repl-verb-interrupt (ws &optional confirm-agents)
  "Interrupt the running TURN of WS through the `Interrupt' verb.
This is the clean turn stop the footer's stop button drives, NOT a forced
restart: the session is not bounced and the agent is not resumed, only the
in-flight vendor query is ended.

EVERY STOP ASKS TO ROLL AN UNANSWERED PROMPT BACK: Emacs holds the composer
the prompt's words return to, so the turn target always carries
`roll_back_unanswered'.  When nothing has answered the running turn's
prompt, the daemon stops the turn by rolling the prompt back and answers
`rolled_back_unanswered': the composer is refilled exactly as after a
rollback, and the feed draws no \"interrupted\" marker.

THE OUTCOME IS THE ARM, and every arm reaches the user.  `interrupted_turn'
says the turn was stopped, and why its prompt stayed when the rollback it
asked for was refused; `nothing_running' is a SUCCESS arm, not a
failure -- the daemon found the session already quiet -- so it is a calm
message rather than an error.  `merge_queue_offered' is the quiet
workspace whose sync merge is queued: the stop raised the merge-queue
question in the pinned merge bubble, so no rollback is offered and the
keyboard goes to the feed.  `no_session' (the workspace never had a
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
           :target (list :arm :turn :value (list :roll-back-unanswered t))
           :confirm-agents (and confirm-agents t))
     :ws ws :op "interrupt"
     :on-success
     (lambda (value)
       (pcase (plist-get value :arm)
         (:interrupted-turn
          (let ((refused (plist-get (plist-get value :value) :not-rolled-back)))
            (if (null refused)
                (progn
                  (agent-repl--info ws "elisp.verbs.interrupt-stopped ws=%s" ws)
                  (message "agent-repl: turn stopped"))
              ;; THE STOP HAPPENED, THE ROLLBACK DID NOT: the prompt stays in
              ;; the feed, and the reason is said beside the stop.
              (agent-repl--info ws "elisp.verbs.interrupt-stopped-not-rolled-back ws=%s cause=%S"
                                ws (plist-get refused :cause))
              (message "agent-repl: turn stopped, prompt kept (%s)"
                       (agent-repl--input-rollback-refusal-text (plist-get refused :cause))))))
         (:rolled-back-unanswered
          ;; NOTHING HAD ANSWERED THE PROMPT: the stop rolled it back, and its
          ;; words return to the composer as after any rollback.
          (agent-repl--info ws "elisp.verbs.interrupt-rolled-back ws=%s" ws)
          (agent-repl--input-rollback-done ws (plist-get value :value)))
         (:nothing-running
          ;; NOTHING RUNNING IS THE STOP KEY'S SECOND PRESS: the daemon says
          ;; what rolling the latest prompt back offers, and input.el acts
          ;; on it.  No offer means there is no prompt to roll back.
          (let ((offer (plist-get (plist-get value :value) :rollback)))
            (agent-repl--info ws "elisp.verbs.interrupt-nothing-running ws=%s offer=%S"
                              ws (plist-get offer :arm))
            (if offer
                (agent-repl--input-rollback-offer ws offer)
              (message "agent-repl: nothing to interrupt"))))
         (:merge-queue-offered
          ;; THE STOP WAS ABOUT THE QUEUED MERGE: the daemon raised the
          ;; merge-queue question in the pinned merge bubble and touched
          ;; nothing else.  No rollback is offered; the keyboard goes to the
          ;; feed, where the question has taken focus.
          (agent-repl-verbs--focus-merge-queue-offer ws))
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

(defun agent-repl-verbs--focus-merge-queue-offer (ws)
  "Hand the keyboard to WS's feed, where the merge-queue question stands.
The webview's own question takes focus inside the page; selecting the
webview window is what makes the next key reach it.  With no window
showing the webview the question still stands in the bubble, and the
message says where to answer it."
  (let ((win (agent-repl--webview-window ws)))
    (agent-repl--info ws "elisp.verbs.interrupt-merge-queue-offered ws=%s webview-window=%s"
                      ws (and win t))
    (when win (select-window win))
    (message "agent-repl: the merge is queued -- Evict, Prioritize or Cancel in its bubble")))

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

(defun agent-repl-verb-set-hidden (ws hidden)
  "Move WS into the sidebar's hidden section when HIDDEN, else back out.
A hidden workspace keeps its session and its tab; the roster push that
follows takes the tab off the bar's drawing, numbering and navigation,
exactly as a collapsed repository's tabs are.  A success draws nothing
of its own beyond the echo: the push is the new state."
  (let ((ref (agent-repl-verbs--ref ws)))
    (agent-repl-verbs--send
     #'agent-repl-rpc-set-workspace-hidden (agent-repl-verbs--conn ws)
     (list :workspace ref :visibility (list :arm (if hidden :hide :show) :value nil))
     :ws ws :op (if hidden "hide" "show")
     :on-success
     (lambda (_)
       (message "agent-repl: %s %s" ws (if hidden "hidden" "shown"))
       (agent-repl-verbs--land-after-visibility ws hidden)))))

(defun agent-repl-verbs--keepalive-outcome-text (outcome)
  "Return the words for one hidden workspace's decoded keep-alive OUTCOME."
  (let ((v (plist-get outcome :value)))
    (pcase (plist-get outcome :arm)
      (:sent "sent")
      (:not-running "not running")
      (:failed (format "failed (%s)" (plist-get v :detail)))
      (:skipped
       (let* ((reason (plist-get v :reason))
              (rv (plist-get reason :value)))
         (pcase (plist-get reason :arm)
           (:busy (format "skipped: %s" (plist-get rv :detail)))
           (:detached-work-live
            (format "skipped: %d background item(s) running" (plist-get rv :live-items)))
           (:cache-lapsed "skipped: its cache already lapsed")
           (arm (error "agent-repl-verbs: keep-alive skip arm %S has no wording" arm)))))
      (arm (error "agent-repl-verbs: keep-alive outcome arm %S has no wording" arm)))))

(defun agent-repl-verbs-describe-hidden-keepalives (workspaces)
  "Return the one line reporting the decoded keep-alive WORKSPACES.
Every workspace whose keep-alive was sent is counted; every other one is
named, by its directory's base name, beside what happened to it."
  (if (null workspaces)
      "no hidden workspaces to keep warm"
    (let ((sent (cl-count-if (lambda (ws) (eq (plist-get (plist-get ws :outcome) :arm) :sent))
                             workspaces))
          (others (cl-remove-if (lambda (ws) (eq (plist-get (plist-get ws :outcome) :arm) :sent))
                                workspaces)))
      (string-join
       (cons (format "keep-alive sent to %d of %d hidden workspace(s)" sent (length workspaces))
             (mapcar (lambda (ws)
                       (format "%s %s"
                               (file-name-nondirectory
                                (directory-file-name (plist-get (plist-get ws :workspace) :dir)))
                               (agent-repl-verbs--keepalive-outcome-text (plist-get ws :outcome))))
                     others))
       " · "))))

(defun agent-repl-send-hidden-keepalives ()
  "Send one keep-alive to every hidden workspace's session.
Each hidden workspace's cache is otherwise left to lapse; the keep-alive
moves its lapse later, and the hidden section's countdowns follow on the
roster stream.  The echo says how each one went."
  (interactive)
  (agent-repl-verbs--send
   #'agent-repl-rpc-send-hidden-keepalives (agent-repl-verbs--conn) nil
   :op "send-hidden-keepalives"
   :on-success
   (lambda (value)
     (let ((workspaces (plist-get value :workspaces)))
       (agent-repl--info agent-repl--global-log-scope
                         "elisp.verbs.hidden-keepalives workspaces=%S" workspaces)
       (message "agent-repl: %s" (agent-repl-verbs-describe-hidden-keepalives workspaces))))))

(defun agent-repl-verbs--land-after-visibility (ws hidden)
  "Request the switch an accepted visibility change of WS lands on.
A HIDE lands on the FIRST tab the bar draws other than WS: the hidden tab
is off the bar, so staying on it would leave the frame on a workspace the
picture no longer offers.  WS is excluded BY NAME rather than trusted to
be gone already, so the landing is the same whether the roster push that
takes WS off the bar arrives before this ack or after it.  A SHOW lands on
WS itself, which is what bringing a workspace back is for.

A hide that leaves no other drawn tab has nowhere to land, and stays put
with a record saying so."
  (if (not hidden)
      (agent-repl-host-request-switch ws 'show)
    (let ((target (car (remove ws (agent-repl-roster-drawn-tab-order)))))
      (if target
          (agent-repl-host-request-switch target 'hide)
        (agent-repl--info ws "elisp.verbs.hide-landing-none ws=%s" ws)))))

(defun agent-repl-verb-swap (ws target)
  "Exchange the places of WS and TARGET in the registry order.
WS takes TARGET's place and TARGET takes WS's; every other workspace keeps
its own.  The two must be siblings of one repository (both top-level, or
children of one parent); any other tab is refused and said in words
\(`agent-repl-verbs--no-tab-to-take').  The tab bar and the sidebar are
both views of the registry, and the new order arrives on the roster
stream, so success changes no editor state."
  (agent-repl-verbs--send
   #'agent-repl-rpc-swap-workspaces (agent-repl-verbs--conn ws)
   (list :workspace (agent-repl-verbs--ref ws) :target (agent-repl-verbs--ref target))
   :ws ws :op "swap"
   :on-success
   (lambda (_)
     (message "agent-repl: swapped %s with %s" ws target))
   :on-error
   (lambda (value) (agent-repl-verbs--no-tab-to-take "swap" ws target value))))

(defun agent-repl-verbs--no-tab-to-take (op ws target value)
  "Claim refusal VALUE of tab-slot gesture OP when it says no tab can be taken.
NO TAB TO TAKE IS AN ANSWER, NOT A FAULT: the user pressed the tab of a
workspace of another repository (`:other-repository') or another family
\(`:not-siblings', a child's tab always following its parent's), and the
daemon said why WS cannot take TARGET's tab.
Recorded at INFO and said in words, answering non-nil; every other arm is
left to the arm-generic report."
  (let ((arm (plist-get (agent-repl-verbs--refusal-arm value) :arm)))
    (when (memq arm '(:other-repository :not-siblings))
      (agent-repl--info ws (format "elisp.verbs.%s-no-tab ws=%%s target=%%s arm=%%S" op) ws target arm)
      (if (eq arm :other-repository)
          (message "agent-repl: %s and %s are in different repositories, so %s cannot take that tab"
                   ws target ws)
        (message "agent-repl: %s and %s are not siblings, so %s cannot take that tab"
                 ws target ws))
      t)))

(defun agent-repl-verb-move (ws target)
  "Move WS onto TARGET's tab, the tabs between shifting over.
The daemon places WS just before TARGET when it comes from a later tab and
just after TARGET's family when it comes from an earlier one; the two must
be siblings in the sidebar's arrangement, and any other pair is refused.
The new order arrives on the roster stream, so success changes no editor
state."
  (agent-repl-verbs--send
   #'agent-repl-rpc-move-workspace (agent-repl-verbs--conn ws)
   (list :workspace (agent-repl-verbs--ref ws) :target (agent-repl-verbs--ref target))
   :ws ws :op "move"
   :on-success
   (lambda (_)
     (message "agent-repl: moved %s onto %s's tab" ws target))
   :on-error
   (lambda (value) (agent-repl-verbs--no-tab-to-take "move" ws target value))))

(cl-defun agent-repl-verb-create (repository form
                                             &key initial-prompt name
                                             merge-actions
                                             prompt context
                                             owner fork-of model priority
                                             allow-ungated)
  "Create a workspace in REPOSITORY under FORM.
FORM is the creation form\'s BARE ARM KEYWORD -- `:standard' or
`:one-shot' -- and that arm\'s own fields ride as keyword arguments beside
it, because a caller at a keybinding writes facts, not nested oneofs.

  `:standard\' takes INITIAL-PROMPT (plain text, wrapped as `UserSaid\'),
    NAME and MERGE-ACTIONS; every one of them is optional, and each
    absence is the statement the proto asks for -- an empty workspace, a
    daemon-minted name, no configured merge actions.  MERGE-ACTIONS is
    spelled in plain text too:
    `(:before-ws-merge TEXT :postprocessing-prompt TEXT)\',
    either half optional, each wrapped as a `UserSaid\' here.
  `:one-shot\' takes PROMPT (required -- a one-shot IS its prompt) and
    CONTEXT, the optional `WorkspaceRef\' of the workspace the commission
    is about, which the daemon names in a note opening the commission.
    THERE IS NO FINISH CHOICE: what happens on completion is the
    repository\='s own directive, which the daemon appends to the
    commission and the agent carries out.

NOTHING IS IMPLIED: the request names its requester, its topology and
its context origin, and this function spells each from what the user did.
The requester is always the user, because every editor create is one.
OWNER is the `WorkspaceRef\' a CHILD is cut from and lands into; without
it the workspace is PRIMARY, cut from and landing on the primary branch.
FORK-OF is the `WorkspaceRef\' whose conversation the new workspace
copies; without it the context is fresh.  The editor makes a child only
as a fork of its owner, which lands itself and so carries no owner
notice.  MODEL, PRIORITY (a bare level keyword) and ALLOW-UNGATED are the
remaining creation facts and are omitted when nil.

CREATING A WORKSPACE NEVER MOVES THE USER (owner ruling, 2026-10-16).
No creation -- dynamic, static, fork or one-shot -- selects what it
made: the user may be typing in a minibuffer, or simply working where they
stand, and the new tab appearing on the bar is the whole of the news.

Nothing else happens on success: THE DAEMON names and creates everything,
and the new workspace\'s tab arrives through the roster push."
  ;; OPTION B.  The create is ACKED at once and worked in the daemon's
  ;; background, detached from this request so a client deadline can never kill
  ;; `git worktree add' mid-run.  Its staged progress and terminal outcome
  ;; arrive on the daemon-level WatchDaemon stream keyed on OP-ID -- the one
  ;; channel open before this workspace exists.  The callbacks that render them
  ;; are registered BEFORE the send, so a progress event never outruns them.
  (let ((op-id (agent-repl-mutation-progress-new-op-id))
        (arrival-reason (if fork-of "forked" "created"))
        ;; THE ACK AND THE PROGRESS STREAM ARE TWO CHANNELS, so the daemon's
        ;; first stage push can overtake the rpc's own ack.  Once anything
        ;; later in the sequence has been shown, the ack has nothing left to
        ;; say and would only put an older line over a newer one.
        (progressed nil))
    (agent-repl-mutation-progress-register
     op-id
     :on-stage
     (lambda (stage)
       (setq progressed t)
       (agent-repl-verbs--create-stage-message stage))
     :on-succeeded
     (lambda (value)
       (setq progressed t)
       (let ((ref (plist-get value :workspace))
             (name (plist-get value :name)))
         (agent-repl-workspace-progress-report :create :completed name)
         ;; The minted ref is the FIRST place this workspace has an identity;
         ;; the arrival reason is claimed here so the workspace, which is
         ;; never selected, still opens its panels when its tab arrives.
         (agent-repl--panels-note-arrival-reason (plist-get ref :id) arrival-reason)))
     :on-failed
     (lambda (arm detail)
       (setq progressed t)
       (agent-repl-verbs--create-failure arm detail)))
    ;; THE ACK UX IS IMMEDIATE: the minibuffer reflects the create the instant
    ;; the command runs, not when the slow work finishes -- the owner's rule
    ;; that phases are messages, not only a mode line.
    (agent-repl-workspace-progress-report :create :requested)
    (agent-repl-verbs--send-op
     op-id #'agent-repl-rpc-create-workspace (agent-repl-verbs--conn)
     (list :repository repository
           :form (agent-repl-verbs--create-form
                  form :initial-prompt initial-prompt :name name
                  :merge-actions merge-actions
                  :prompt prompt :context context
                  :topology (agent-repl-verbs--topology owner))
           :requester (list :arm :user)
           :context-origin (if fork-of
                               (list :arm :fork :value fork-of)
                             (list :arm :fresh))
           :model model
           :priority (agent-repl-verbs--level-arm priority)
           :allow-ungated allow-ungated
           :op-id op-id)
     :op "create"
     :on-accepted
     (lambda (_accepted)
       (agent-repl--info '(:agent-repl-central "workspace creation and daemon administration can precede workspace ownership")
                         "elisp.verbs.create-accepted op-id=%s" op-id)
       ;; THE WHOLE SEQUENCE IS SHOWN (owner ruling, 2026-09-27): accepted,
       ;; then every stage the daemon pushes, then created or FAILED.
       (unless progressed
         (agent-repl-workspace-progress-report :create :accepted)))
     :on-success
     ;; A daemon that answered success synchronously (an op_id it did not
     ;; honor) still landed the workspace: run the terminal and drop the op.
     ;; Its completion is said exactly as the stream's is; the answer carries
     ;; the minted ref and no name, so the line names the minted directory.
     (lambda (success)
       (agent-repl-workspace-progress-report
        :create :completed (plist-get (plist-get success :workspace) :dir))
       (agent-repl--panels-note-arrival-reason
        (plist-get (plist-get success :workspace) :id) arrival-reason))
     :on-error
     ;; A SYNCHRONOUS refusal (validation, an unknown repository) is answered
     ;; on the rpc before the work detaches, so no progress will follow: the
     ;; op is retired (`agent-repl-verbs--send-op') and the refusal worded,
     ;; falling through for arms it does not claim.
     #'agent-repl-verbs--create-refusal)))

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
  "Surface a daemon refusal of a create, from VALUE, as the create\='s FAILURE.
Answers non-nil when it claimed the arm.  It claims every arm except the
two handover arms (`agent-repl-verbs--handover-arms\='), which are the
rollout\='s ordering rather than a failure and fall through to the
generic refusal handling that routes them to the handover.

A REFUSED CREATE IS A FAILED CREATE, AND IT IS SAID AS ONE (owner ruling,
2026-09-27).  A synchronous refusal and one that arrives later on the
progress stream both reach the user through the SAME
`agent-repl-workspace-progress-report\=' `:failed\=' phase every other
create failure uses: an ERROR record and the minibuffer line
\"agent-repl: workspace creation FAILED: ...\".  A refusal drawn only as a
WARNING in *Messages* arrived after the user had moved on and read as
nothing having happened."
  (when-let ((sentence (agent-repl-verbs--create-refusal-sentence value)))
    (agent-repl-workspace-progress-report :create :failed sentence)
    t))

(defun agent-repl-verbs--create-refusal-sentence (value)
  "Word the refusal VALUE of a create, or answer nil for a handover arm.
The create-specific arms are worded by their own functions; every other
arm is named by its keyword and its own fields."
  (let* ((refusal (agent-repl-verbs--refusal-arm value))
         (keyword (plist-get refusal :arm)))
    (cond
     ((memq keyword agent-repl-verbs--handover-arms) nil)
     ((eq keyword :naming-failed)
      (agent-repl-verbs--create-naming-refusal-sentence (plist-get refusal :value)))
     ((eq keyword :one-shot-policy-missing)
      (agent-repl-verbs--create-policy-refusal-sentence (plist-get refusal :value)))
     ((eq keyword :inside-temporary-directory)
      (agent-repl-verbs-temporary-directory-sentence (plist-get refusal :value)))
     (t
      (format "the daemon refused it: %s%s"
              (if keyword (substring (symbol-name keyword) 1) "unstated")
              (agent-repl-verbs--refusal-fields refusal))))))

;; A TEMPORARY FOLDER IS NEVER REGISTERED (owner ruling, 2026-10-06): the
;; daemon refuses a repository or workspace whose directory lies inside a
;; temporary root, on the `inside_temporary_directory' arm of
;; RegisterWorkspaceError, RegisterRepositoryError and CreateWorkspaceError.
;; All three carry the same two fields, and all three are worded HERE.

(defun agent-repl-verbs-temporary-directory-sentence (fields)
  "Word an `inside_temporary_directory\=' refusal whose fields are FIELDS.
FIELDS is the decoded (:dir DIR :temporary-root ROOT); the sentence names
both, because the directory and why it was refused are the whole answer."
  (format "%s is inside the temporary directory %s; agent-repl does not register temporary folders"
          (plist-get fields :dir) (plist-get fields :temporary-root)))

(defun agent-repl-verbs-register-refusal-sentence (dir value)
  "Word the daemon\='s refusal VALUE of registering DIR as a workspace.
VALUE is a decoded `RegisterWorkspaceError\='.  Every arm the contract
declares has its own sentence; an arm this function has not been taught
is named by its keyword and fields rather than dropped."
  (let* ((arm (agent-repl-verbs--refusal-arm value))
         (keyword (plist-get arm :arm)))
    (pcase keyword
      (:inside-temporary-directory
       (agent-repl-verbs-temporary-directory-sentence (plist-get arm :value)))
      (:not-a-worktree (format "%s is not a git worktree" dir))
      (:parent-not-open (agent-repl-verbs-family-sentence "register" keyword (plist-get arm :value)))
      (_ (format "the daemon refused %s: %s%s" dir
                 (if keyword (substring (symbol-name keyword) 1) "unstated")
                 (agent-repl-verbs--refusal-fields arm))))))

(defun agent-repl-verbs--create-naming-refusal-sentence (failed)
  "Word a `naming_failed\=' refusal whose fields are FAILED.

EVERY DYNAMICALLY CREATED WORKSPACE IS NAMED BY THE MODEL, and this one
could not be: there is no truncation fallback, so the create is refused
rather than given a name nobody chose.  The sentence names the cause and
what the model last said, because those are what say whether to retry or
to supply a name by hand."
  (let ((cause (plist-get failed :cause))
        (answer (plist-get failed :answer))
        (attempts (plist-get failed :attempts)))
    (format "the workspace could not be named (%s, %s attempt%s)%s"
            cause attempts (if (eql attempts 1) "" "s")
            (if (and answer (not (string-empty-p answer)))
                (format " -- the model answered %S" answer)
              ""))))

(defun agent-repl-verbs--create-policy-refusal-sentence (missing)
  "Word a `one_shot_policy_missing\=' refusal whose fields are MISSING.

A ONE-SHOT RUNS ITS REPOSITORY\='S OWN POLICY, and the daemon detects a
repository that states none -- Emacs never looks at the filesystem for
this.  The sentence names the directory the user must write and the
files it needs, because the user\='s next move is to write exactly those."
  (let ((dir (plist-get missing :policy-dir))
        (files (plist-get missing :missing-files)))
    (format "%s states no one-shot policy -- write %s"
            (plist-get missing :repository-root)
            (if files
                (format "%s in %s" (string-join files ", ") dir)
              dir))))

(defun agent-repl-verbs-select-minted (ref dispatched-at &optional lander)
  "Stand on the workspace REF names, once the daemon has minted it.

DISPATCHED-AT is `agent-repl-host-editor-switch-count\=' when the user
made the gesture that minted REF.  THE LANDING IS WITHDRAWN once the user
has switched workspaces IN EMACS since then, whether before the answer or
while the landing waits for its tab: the user has moved on, and dragging
them back to the new workspace takes them off their own choice.  A
switch made from the sidebar or by an agent is not the user\='s move in
Emacs and withdraws nothing (`agent-repl-host--editor-switches\=').

LANDER is HOW the landing is made, and defaults to
`agent-repl-verbs--land-on\=' -- a projectile switch to the minted
WORKTREE, which is right for every verb whose workspace is a worktree
Doom has never seen.  A verb whose workspace is a repository\='s MAIN
worktree passes `agent-repl-verbs--land-on-tab\=' instead; see that
function for why landing by DIRECTORY cannot be used there.

THE ANSWER IS WHEN THE WORKSPACE EXISTS.  A minted `WorkspaceRef\' is the
one place the identity is known before the roster push carries it, so
every verb that stands on a workspace it just asked for stands on it HERE
-- `RegisterWorkspace\''s ref for onboarding a directory, the open of a
closed workspace from the switcher.  A CREATE never comes here: no
creation moves the user (`agent-repl-verb-create\=').  Standing on the
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
          ;; Owner ruling 2026-10-11: a registered or reopened workspace is
          ;; landed on when it becomes available, unless the user switched
          ;; in Emacs meanwhile.
          (cond
           ((agent-repl-verbs--landing-withdrawn-p ref dispatched-at "answered"))
           (ws
            (agent-repl-verbs--land ref "already-a-tab" ws land))
           (t
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
            (agent-repl-verbs--pending-landing-register ref dispatched-at land))))
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

(defun agent-repl-verbs--landing-withdrawn-p (ref dispatched-at edge)
  "Return non-nil when the landing on REF is withdrawn at EDGE.
It is withdrawn when the user has switched workspaces in Emacs since
DISPATCHED-AT (`agent-repl-verbs-select-minted\='); the withdrawal is
recorded at INFO, since an automatic move that did not happen must not
be invisible.  EDGE names where the landing was judged, for the record."
  (let ((now (agent-repl-host-editor-switch-count)))
    (unless (= now dispatched-at)
      (agent-repl--info '(:agent-repl-central
                          "a withdrawn landing moves the user to no workspace")
                        "elisp.verbs.landing-withdrawn id=%s dir=%s edge=%s reason=user-switched dispatched-at=%d now=%d"
                        (plist-get ref :id) (plist-get ref :dir) edge dispatched-at now)
      t)))

(defun agent-repl-verbs--land (ref why ws land)
  "Stand on WS named by REF through LAND, recording WHY.
The move is Emacs\='s own, not a switch the user made
(`agent-repl-host-automatic-switch\=').

A MINIBUFFER HOLDS THE LANDING (`agent-repl--when-minibuffer-free\='): it
arrives on a roster push, whenever that is, and moving the frame then
would take the user out of the prompt they are typing in.  A landing
released after its workspace left the roster has nowhere to stand, and
is recorded rather than made."
  (agent-repl--when-minibuffer-free
   ws (format "landing why=%s" why) 'landing
   (lambda (released)
     (if (and released (not (equal (agent-repl--ws-by-ref-id (plist-get ref :id)) ws)))
         (agent-repl--info '(:agent-repl-central "a landing whose workspace left owns no sink")
                           "elisp.verbs.landing-dropped ws=%s id=%s reason=workspace-gone"
                           ws (plist-get ref :id))
       (let ((agent-repl-host-automatic-switch t))
         (funcall land ref why ws))))))

(defun agent-repl-verbs--land-on (ref why ws)
  "Stand on workspace WS named by REF, recording WHY the landing ran now."
  (let ((dir (plist-get ref :dir)))
    (agent-repl--info ws "elisp.verbs.land-on ws=%s dir=%s why=%s" ws dir why)
    (agent-repl-switch-to-project dir)))

(defvar agent-repl-verbs--pending-landing-dispatched-at nil
  "The switch count `agent-repl-verbs--pending-landing\=' was dispatched at.
Set and cleared with the ref it belongs to; see
`agent-repl-verbs-select-minted\='.")

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

(defun agent-repl-verbs--pending-landing-register (ref dispatched-at &optional lander)
  "Record REF as the landing waiting for its tab to reach the roster.
DISPATCHED-AT and LANDER are as `agent-repl-verbs-select-minted\='
documents."
  (agent-repl--info '(:agent-repl-central "workspace creation and daemon administration can precede workspace ownership") "elisp.verbs.pending-landing-registered dir=%s id=%s"
                    (plist-get ref :dir) (plist-get ref :id))
  (setq agent-repl-verbs--pending-landing-lander
        (or lander #'agent-repl-verbs--land-on))
  (setq agent-repl-verbs--pending-landing-dispatched-at dispatched-at)
  (setq agent-repl-verbs--pending-landing ref))

(defun agent-repl-verbs--pending-landing-fire (&rest _)
  "Land on the pending minted workspace once its tab exists.
Installed on `agent-repl-roster-update-functions', the hook the roster
runs after it has reconciled a push into tabs, so the ARRIVAL of the tab
is what lands the user, unless the user has switched in Emacs since the
dispatch (`agent-repl-verbs-select-minted\=').  Fires exactly once: the
pending ref is cleared before the landing runs or is withdrawn."
  (when-let* ((ref agent-repl-verbs--pending-landing)
              (id (plist-get ref :id)))
    (let ((ws (agent-repl--ws-by-ref-id id)))
      (if ws
          (let ((land (or agent-repl-verbs--pending-landing-lander
                          #'agent-repl-verbs--land-on))
                (dispatched-at agent-repl-verbs--pending-landing-dispatched-at))
            (setq agent-repl-verbs--pending-landing nil
                  agent-repl-verbs--pending-landing-dispatched-at nil
                  agent-repl-verbs--pending-landing-lander nil)
            (agent-repl--info ws "elisp.verbs.pending-landing-arrived ws=%s id=%s" ws id)
            (unless (agent-repl-verbs--landing-withdrawn-p ref dispatched-at "tab-arrived")
              (agent-repl-verbs--land ref "tab-arrived" ws land)))
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

(defun agent-repl-verbs--topology (owner)
  "Return the `WorkspaceTopology\' oneof for OWNER, a `WorkspaceRef\' or nil.
An owner makes a CHILD of it, and no owner a PRIMARY workspace."
  (if owner
      (list :arm :child :value (list :owner owner))
    (list :arm :primary)))

(cl-defun agent-repl-verbs--create-form (form &key initial-prompt name
                                              merge-actions prompt context
                                              topology)
  "Return the creation-form oneof for the bare arm keyword FORM.
The two forms take disjoint facts, so each arm reads only its own, and
both carry TOPOLOGY."
  (pcase form
    (:standard
     (list :arm :standard
           :value (list :initial-prompt (and initial-prompt
                                             (agent-repl-verbs--said initial-prompt))
                        :name name
                        :merge-actions (agent-repl-verbs--merge-actions merge-actions)
                        :topology topology)))
    (:one-shot
     (list :arm :one-shot
           :value (list :prompt (and prompt (agent-repl-verbs--said prompt))
                        :context context
                        :topology topology)))
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
      (:restarting
       (format "restarting for state layout %d→%d, %d workspace%s (%d busy%s)"
               (plist-get value :running-state-layout)
               (plist-get value :fresh-state-layout)
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

(defconst agent-repl-verbs--deploy-fault-arms
  '(:build-failed :install-failed :service-restart-failed)
  "The DeployError arms the daemon ALSO stands as a `deploy_failed' fault.
A build, an install or a service restart that did not go through opens
the daemon-scoped fault, which WatchDaemon tells this Emacs as a standing
loud fault and daemon-link.el echoes (`agent-repl-link--faults-standing')
-- whoever started the deploy.  So for these arms THE PUSHED LINE IS THE
ONE ECHO: the verb records its refusal and leaves the minibuffer to it,
rather than saying one failure twice.")

(defun agent-repl-verbs--deploy-on-error (value)
  "Report the DeployError VALUE loudly, and claim it.
Every arm is an ERROR record carrying its fields.  An arm the daemon also
stands as a `deploy_failed' fault (`agent-repl-verbs--deploy-fault-arms')
is echoed by that fault's push; every other arm is echoed here, with its
detail."
  (let* ((cause (plist-get value :cause))
         (arm (plist-get cause :arm))
         (central '(:agent-repl-central "a deploy is daemon administration spanning every workspace")))
    (agent-repl--error central "elisp.verbs.deploy-refused arm=%S fields=%S"
                       arm (plist-get cause :value))
    (if (memq arm agent-repl-verbs--deploy-fault-arms)
        (agent-repl--log central "elisp.verbs.deploy-refusal-echoed-by-its-fault arm=%S sentence=%S"
                         arm (agent-repl-verbs--deploy-refusal-sentence cause))
      (message "agent-repl: deploy refused: %s"
               (agent-repl-verbs--deploy-refusal-sentence cause)))
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
  "Append LINES to the health buffer and show it in the shared popup."
  (with-current-buffer (get-buffer-create agent-repl-verbs-health-buffer)
    (goto-char (point-max))
    (dolist (line lines) (insert line "\n")))
  (agent-repl-popup-show (get-buffer agent-repl-verbs-health-buffer)))

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
  "Kill the current workspace's session by force (`SPC j x').
Confirms first with a y/n question (owner ruling, 2026-09-27): the kill
ends the session and closes the workspace's tab, so a stray keypress must
not do it."
  (interactive)
  (let ((ws (agent-repl-verbs--target-ws ws "Kill workspace: ")))
    (if (y-or-n-p (format "Kill workspace %s? " ws))
        (agent-repl-verb-kill ws)
      (agent-repl--info ws "elisp.verbs.kill-declined ws=%s" ws))))

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
                               (cons (agent-repl-roster-row-name row)
                                     (agent-repl-roster-row-ref row)))
                             rows)))
    (unless candidates
      (user-error "agent-repl: the roster lists no closed workspaces"))
    (let* ((choice (completing-read "Open workspace: " (mapcar #'car candidates) nil t))
           (ref (cdr (assoc choice candidates))))
      (agent-repl--info '(:agent-repl-central "workspace creation and daemon administration can precede workspace ownership") "elisp.verbs.open-chosen name=%s" choice)
      (agent-repl-verb-open ref))))

(defun agent-repl-merge-workspace (&optional ws)
  "Enqueue a one-shot merge of the current workspace's own branch (`SPC TAB M').
The workspace closes once its branch lands."
  (interactive)
  (agent-repl-verb-merge (agent-repl-verbs--target-ws ws "Merge workspace: ")))

(defun agent-repl-restart-workspace (&optional ws)
  "Restart the current workspace's backend and webapp page (`SPC o C-c').
IMMEDIATE, with no graceful mode and no prefix argument: the running turn
and all detached work are hard-stopped, the workspace's shim is relaunched
(rebuilt first when stale) with the same session resumed, and the
workspace's webapp page reloads.  Prompts sent meanwhile are held \"after
reconnect\".  The daemon, the store and the sidecar are never touched.

For a STUCK workspace: a turn that never ends, a Claude SDK that does not
respond or failed to start, a stale shim build, a page out of sync.  Not
for a page-only reload (`SPC o l') and not when the daemon itself is down.
NOT A ROUTINE ACTION: a fresh shim or page may speak an API the older
running daemon does not."
  (interactive)
  (agent-repl-verb-restart (agent-repl-verbs--target-ws ws "Restart workspace: ")))

(defun agent-repl-interrupt-turn (&optional ws)
  "Interrupt the running turn of the current workspace (`C-c C-k').
The clean turn stop the footer's stop button drives: the in-flight vendor
query is ended, the session is left standing and the agent is not resumed.
When the stop would also end live detached agents the user is asked first.

WHEN NOTHING HAS ANSWERED THE RUNNING TURN'S PROMPT, the first press rolls
that prompt back as it stops the turn: its words land in the composer and
the feed shows no \"interrupted\" marker.  A prompt that cannot be rolled
back (the first of its conversation, say) is kept, the turn stopped, and
the reason said.

PRESSED AGAIN, with no turn in flight, it rolls the latest prompt back:
at once, the prompt landing in the composer, when nothing has answered it
in the feed yet; otherwise it lists the prompts a rollback can reach,
newest first with the time since each was sent and the context size
before it, and rolls back to the one picked after confirming.  With no
prompt to roll back it says there is nothing to interrupt.

With no turn in flight while the workspace's own sync merge is queued, it
rolls nothing back: it asks about the merge instead, in the pinned merge
bubble (Evict, Prioritize or Cancel), and hands the keyboard to the feed.
The press itself changes nothing on the queue."
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

(defun agent-repl-hide-workspace (&optional ws)
  "Hide the current workspace (`SPC TAB h').
It moves to the sidebar's HIDDEN WORKSPACES section and off the tab bar,
so the tab numbers close up over it; its session runs on untouched,
and the frame lands on the first tab the bar still draws.  Clicking its
row in that section, or picking it from `SPC TAB H', shows it again."
  (interactive)
  (agent-repl-verb-set-hidden (agent-repl-verbs--target-ws ws "Hide workspace: ") t))

(defun agent-repl-verbs--hidden-rows (&optional roster)
  "Return the rows of ROSTER's hidden section, in walk order.
ROSTER defaults to `agent-repl-roster-view'.  The hidden list holds open
workspaces only (a closed one is in the closed list), so every row is one
to switch back to."
  (let ((roster (agent-repl-verbs--roster-or-view roster)))
    (agent-repl-roster-flatten
     (plist-get (plist-get (plist-get roster :hidden) :hidden-rows) :rows))))

(defun agent-repl-show-hidden-workspace ()
  "Show a hidden workspace again and switch to it (`SPC TAB H').
Completes over the workspaces the sidebar's HIDDEN WORKSPACES section
holds, by their tab names.  The chosen one comes back onto the tab bar,
and the frame lands on it once the daemon accepts."
  (interactive)
  (let ((names (delq nil (mapcar (lambda (row)
                                   (agent-repl--ws-by-ref-id
                                    (plist-get (agent-repl-roster-row-ref row) :id)))
                                 (agent-repl-verbs--hidden-rows)))))
    (unless names
      (agent-repl--info '(:agent-repl-central "the hidden-workspace picker spans every workspace")
                        "elisp.verbs.show-hidden-none")
      (user-error "agent-repl: no hidden workspaces"))
    (let ((choice (completing-read "Show hidden workspace: " names nil t)))
      (agent-repl--info choice "elisp.verbs.show-hidden-chosen ws=%s" choice)
      (agent-repl-verb-set-hidden choice nil))))

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
never picked.  Only the static mode asks for a repository.  The
exceptions are the one-shots bound to FIXED repositories (owner ruling,
2026-10-19): `SPC TAB o\=' always lands in agent-repl\='s own repository
and `SPC TAB O\=' in explanation-engine\='s; `SPC TAB C-o\=' derives.

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
refusal the daemon adds later still reaches the user correctly.

A REFUSAL IS AN ANSWER, NOT A FAULT, so it is recorded at INFO: the daemon
did what it was asked and said no, and the user is told why.  The failure
the user reads is the progress report's own."
  (let* ((arm (agent-repl-verbs--refusal-arm value))
         (keyword (plist-get arm :arm))
         (sentence
          (if (eq keyword :inside-temporary-directory)
              ;; THE ARM NAMES ITS OWN DIRECTORY: the main worktree the
              ;; path resolved to, which is what was refused.
              (agent-repl-verbs-temporary-directory-sentence (plist-get arm :value))
            (when-let ((said (cdr (assq keyword agent-repl-verbs--register-repository-sentences))))
              (format "%s: %s" said path)))))
    (when sentence
      (agent-repl--info agent-repl--global-log-scope
                        "elisp.verbs.register-repository-refused path=%S arm=%S"
                        path keyword)
      (agent-repl-workspace-progress-report :register-repository :failed sentence)
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
  (let ((path (agent-repl-verbs--register-repository-read-path))
        (dispatched-at (agent-repl-host-editor-switch-count)))
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
          workspace dispatched-at #'agent-repl-verbs--land-on-tab)))
     :on-error
     (lambda (value) (agent-repl-verbs--register-repository-on-error path value)))))

(defun agent-repl-verbs--read-prompt (prompt-text &optional history-file unseeded)
  "Read the creation prompt, defaulting to the composer's current text.
The composer is where the user was already writing, so its contents are
the obvious default rather than something to retype.

With UNSEEDED, the composer is neither read nor offered and the prompt
starts empty.  The one-shot commission reads this way: its prompt is a
fresh piece of work, not whatever the current workspace was drafting.

With HISTORY-FILE, the prompt's history lives in that file and the text
being typed is saved there on every edit
\(`agent-repl-verbs--read-string-persisted').  Without it, the read uses
the ordinary minibuffer history."
  (let* ((ws (unless unseeded (agent-repl--ws-current-log-name)))
         (buffered (and ws (agent-repl--read-input-buffer ws)))
         (initial (and buffered (not (string-empty-p (string-trim buffered)))
                       (string-trim buffered))))
    (unless (or ws unseeded)
      (agent-repl--log '(:agent-repl-central "the current buffer has no workspace composer")
                       "elisp.verbs.read-prompt prompt=%S composer=none" prompt-text))
    (if history-file
        (agent-repl-verbs--read-string-persisted prompt-text initial history-file)
      (read-string prompt-text initial))))

(defun agent-repl-verbs--read-required-prompt (prompt-text refusal &optional history-file unseeded)
  "Read a creation prompt with PROMPT-TEXT that must not be blank.
The text is returned as read.  A blank or whitespace-only answer is
refused with REFUSAL, before any request is built.  HISTORY-FILE and
UNSEEDED are passed to `agent-repl-verbs--read-prompt'."
  (let ((text (agent-repl-verbs--read-prompt prompt-text history-file unseeded)))
    (when (string-empty-p (string-trim text))
      (user-error "agent-repl: %s" refusal))
    text))

(defconst agent-repl-verbs--create-log-scope
  '(:agent-repl-central "workspace creation and daemon administration can precede workspace ownership")
  "The central log scope every creation command records under.
A create runs before the workspace it makes exists, so it can own no
workspace log of its own.")

;;;; ---- Persisted prompt history ------------------------------------------
;;
;; A prompt read with a HISTORY FILE keeps its minibuffer history on disk,
;; and keeps the text being typed there too: every edit rewrites the file
;; with the live text at the head of the history.  A crash mid-typing
;; therefore loses nothing -- the next read of the same prompt offers the
;; text, in full, one `M-p' / up-arrow away.  The text stays a history
;; entry however the read ends: submitted, quit, or never ended at all.
;;
;; The file is the only store.  Each read loads it afresh and every write
;; is atomic (`agent-repl--write-sexp-file'), so no Emacs-side copy can go
;; stale and no torn write can leave a file that does not read.

(defvar agent-repl-verbs--persisted-history nil
  "The history list of the persisted read in progress.
Let-bound around each read by `agent-repl-verbs--read-string-persisted',
from the history file, and handed to `read-string' as its HIST.")

(defun agent-repl-verbs--persisted-history-load (file)
  "Return the list of strings saved in history FILE, or nil when it is absent.
FILE is written only by `agent-repl-verbs--persisted-history-save', so a
value that is not a list of strings is a broken invariant: it is logged
with the file and the value, and signalled."
  (let ((entries (agent-repl--read-sexp-file-if-exists file)))
    (unless (and (listp entries) (cl-every #'stringp entries))
      (agent-repl--error agent-repl-verbs--create-log-scope
                         "elisp.verbs.persisted-history-invalid file=%s value=%S"
                         file entries)
      (error "agent-repl: prompt history %s holds %S, not a list of strings"
             file entries))
    entries))

(defun agent-repl-verbs--persisted-history-entries (text base)
  "Return history BASE with TEXT at its head.
A blank TEXT adds nothing and BASE is returned as is.  An earlier entry
equal to TEXT is dropped, so a resubmitted prompt appears once, and the
list is cut to `history-length' when that is a number."
  (if (or (null text) (string-empty-p (string-trim text)))
      base
    (let ((entries (cons text (remove text base))))
      (if (integerp history-length)
          (seq-take entries history-length)
        entries))))

(defun agent-repl-verbs--persisted-history-save (file text base)
  "Write history BASE with TEXT at its head to FILE.
A failed write is logged by `agent-repl--write-sexp-file' and re-signalled."
  (agent-repl--write-sexp-file
   file (agent-repl-verbs--persisted-history-entries text base)))

(defun agent-repl-verbs--read-string-persisted (prompt-text initial file)
  "Read a string with PROMPT-TEXT and INITIAL input, its history kept in FILE.
FILE's history is what `M-p' and the up arrow walk.  Every edit of the
minibuffer saves FILE with the live text at its head, and the read saves
it once more as it ends, so the text is never lost to a crash.

Text that is blank, or already one of FILE\='s entries, is not a new
draft, so it leaves the saved draft in place rather than displacing it.
That is what `M-p' shows while the user browses: it empties the
minibuffer, then inserts an entry.  A failed
save aborts the read loudly rather than letting the user type on
believing the text is kept."
  (let* ((base (agent-repl-verbs--persisted-history-load file))
         (agent-repl-verbs--persisted-history base)
         (text initial))
    (unwind-protect
        (setq text
              (minibuffer-with-setup-hook
                  (lambda ()
                    (add-hook 'after-change-functions
                              (lambda (&rest _)
                                (let ((live (minibuffer-contents-no-properties)))
                                  (unless (or (string-empty-p (string-trim live))
                                              (member live base))
                                    (setq text live)
                                    (agent-repl-verbs--persisted-history-save file text base))))
                              nil t))
                (read-string prompt-text initial 'agent-repl-verbs--persisted-history)))
      (agent-repl-verbs--persisted-history-save file text base))))

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

(defun agent-repl-verbs--create-standard (command mode)
  "Create a standard TOP-LEVEL workspace in MODE.
MODE is `dynamic\=' -- a prompt and nothing else, the repository read off
the current workspace\='s roster section -- or `static\=', which asks for a
repository and a REQUIRED name and sends no prompt at all.

It is cut from the repository\='s primary branch and merges into it; a
workspace that merges into another one is a CHILD FORK
\(`agent-repl-verbs--create-fork\').

THE NEW WORKSPACE IS NOT SELECTED: no creation moves the user
\(`agent-repl-verb-create\=').

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
                    (lambda () (agent-repl-verbs--read-prompt "Initial prompt: "))))))
    (agent-repl--info agent-repl-verbs--create-log-scope "elisp.verbs.create-standard mode=%s" mode)
    (apply #'agent-repl-verb-create
           repository :standard
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

It never makes a child: the child fork (`agent-repl-fork-workspace\=') is
the one way to make a workspace that merges into another, and naming a
repository or a name by hand is the STATIC mode\='s
business (`agent-repl-create-workspace-static\=').

THE NEW WORKSPACE IS NOT SELECTED: its tab arrives and the user stays put."
  (interactive)
  (agent-repl-verbs--create-standard 'agent-repl-create-workspace 'dynamic))

(defun agent-repl-create-workspace-static ()
  "Create a STATIC workspace (`SPC TAB N\='): a repository and a name, no prompt.
Owner ruling, 2026-09-12.  This is the one creation mode that asks for a
repository, and the one that REQUIRES a name; it sends no initial prompt
at all, so the workspace comes up idle and waits for the user.

It never makes a child: `agent-repl-fork-workspace-static\=' is the named
child fork.

THE NEW WORKSPACE IS NOT SELECTED, exactly as the dynamic create\='s is
not."
  (interactive)
  (agent-repl-verbs--create-standard 'agent-repl-create-workspace-static 'static))

(defun agent-repl-fork-workspace ()
  "Create a CHILD workspace forking the current one's conversation (`SPC TAB c').
The child is cut from the current workspace's tip and merges back into
it, so the roster nests it there; `agent-repl-fork-workspace-into-primary'
is the fork that merges into the primary branch instead.

It is a DYNAMIC mode (owner ruling, 2026-09-12), so it asks for the
prompt alone: the repository is the current workspace's, and the daemon
mints the name.

THE FORK IS NOT SELECTED, exactly as a plain create is not: no creation
moves the user (`agent-repl-verb-create\=').

`agent-repl-fork-workspace-static\=' is the NAMED variant."
  (interactive)
  (agent-repl-verbs--create-fork 'agent-repl-fork-workspace 'dynamic))

(defun agent-repl-fork-workspace-static ()
  "Fork the current workspace into a NAMED child (`SPC TAB C\='), no prompt.
Exactly `agent-repl-fork-workspace\=' -- the repository is the current
workspace\='s, the parent is the current workspace, the conversation is
forked, the fork is NOT selected -- except it asks for a REQUIRED name instead
of a prompt and sends NO initial prompt, so the fork comes up idle on the
forked conversation.  It relates to the fork as `SPC TAB N\=' relates to
`SPC TAB n\=', but it never asks for a repository: a fork\='s repository is
its parent\='s by construction."
  (interactive)
  (agent-repl-verbs--create-fork 'agent-repl-fork-workspace-static 'static))

(defun agent-repl-verbs--create-fork (command mode &optional into-primary)
  "Fork the current workspace\='s conversation into a new workspace, in MODE.
MODE is `dynamic\=' -- a prompt, and the daemon mints the name -- or
`static\=', a REQUIRED name and no prompt at all.

Without INTO-PRIMARY the fork is a CHILD: the current workspace is its
parent, it is cut from the parent\='s tip and merges back into it.  With
INTO-PRIMARY it is a TOP-LEVEL workspace cut from the primary branch that
merges into it, carrying only the conversation (`context_origin.fork\='); the
daemon opens its first message with a note naming the workspace the
conversation came from.

Everything else is the fork\='s: the repository is the current
workspace\='s, and the fork is selected.  All four fork commands share this
body so their spellings cannot drift apart.

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
         (source (agent-repl-verbs--ref (agent-repl--ws-current-name))))
    (agent-repl--info agent-repl-verbs--create-log-scope "elisp.verbs.create-fork mode=%s into-primary=%s"
                      mode (and into-primary t))
    (apply #'agent-repl-verb-create
           repository :standard
           (append
            (if into-primary
                (list :fork-of source)
              (list :owner source :fork-of source))
            (if static
                (list :name name)
              (list :initial-prompt
                    (unless (string-empty-p (string-trim prompt)) prompt)))))))

(defun agent-repl-fork-workspace-into-primary ()
  "Fork the current conversation into a TOP-LEVEL workspace (`SPC TAB f\=').
The worktree is cut from the primary branch and merges into it; only the
conversation is copied.  Asks for the prompt alone, and the daemon mints
the name.  `agent-repl-fork-workspace-into-primary-static\=' is the NAMED
variant, and `agent-repl-fork-workspace\=' the CHILD fork.

THE FORK IS NOT SELECTED."
  (interactive)
  (agent-repl-verbs--create-fork 'agent-repl-fork-workspace-into-primary 'dynamic t))

(defun agent-repl-fork-workspace-into-primary-static ()
  "Fork the current conversation into a NAMED top-level workspace (`SPC TAB F\=').
Exactly `agent-repl-fork-workspace-into-primary\=', except it asks for a
name and sends no prompt, so the workspace starts idle.

THE FORK IS NOT SELECTED."
  (interactive)
  (agent-repl-verbs--create-fork 'agent-repl-fork-workspace-into-primary-static 'static t))

;;;; ---- One-shots --------------------------------------------------------
;;
;; ONE-SHOTS RIDE THE WIRE.  Emacs supplies the prompt, the model and the
;; parentage; the DAEMON owns naming, the worktree and prompt decoration.
;; Everything the old doom / explanation-engine one-shot commands did by
;; hand is now one CreateWorkspace request whose form arm says one_shot.
;;
;; THERE IS ONE ONE-SHOT COMMAND PER REPOSITORY, because there is no finish
;; to choose between (owner ruling, 2026-09-12): the repository states in
;; one file what is to be done on completion, and the agent carries it out.
;; The repository is FIXED, not the current workspace's (owner ruling,
;; 2026-10-19): `SPC TAB o' lands in agent-repl's own repository and
;; `SPC TAB O' in explanation-engine's.  Only `SPC TAB C-o', a one-shot
;; ABOUT the current workspace, takes the current workspace's repository.

(defcustom agent-repl-oneshot-model-candidates '("opus" "sonnet" "haiku")
  "The models offered when creating a ONE-SHOT workspace, in order.
One-shots ride the wire -- Emacs supplies prompt, model and parentage
through the dedicated one-shot creation form and the daemon owns naming,
the worktree and decoration -- so this list
is a picker's contents and nothing more.  It lives beside the one-shot
commands that read it, which are its only consumer."
  :type '(repeat string)
  :group 'agent-repl)

(defconst agent-repl-verbs--oneshot-history-basename "oneshot-commission-history.el"
  "The file, under the global state directory, holding one-shot commissions.")

(defun agent-repl-verbs--oneshot-history-file ()
  "Return the file the one-shot commission prompt keeps its history in.
It is process-wide, under `agent-repl--global-state-dir', because a
one-shot is commissioned from any workspace or none."
  (agent-repl--global-state-file agent-repl-verbs--oneshot-history-basename))

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
A one-shot asks for its commission and nothing else: the repository is
ALWAYS agent-repl\='s own (`agent-repl-verbs--agent-repl-repository\='),
whichever workspace (or none) this runs from, off its main branch, and the
daemon mints the name.  `SPC TAB O\=' is the explanation-engine
counterpart.  The commission prompt starts EMPTY: unlike every other
creation prompt it is never seeded from the composer, whose draft belongs
to the current workspace rather than to the new one-shot.

There is no finish to choose.  The repository states, in one plain-English
file of its own policy directory, what is to be done when the work is
done; the daemon appends that to the commission and the agent carries it
out itself.

A prefix argument PICK-MODEL asks for the model."
  (interactive "P")
  (agent-repl-verbs--create-oneshot
   'agent-repl-create-oneshot pick-model nil #'agent-repl-verbs--agent-repl-repository))

(defun agent-repl-create-oneshot-for-current-workspace (&optional pick-model)
  "Create a ONE-SHOT that fixes something wrong with the CURRENT workspace.
Bound to `SPC TAB C-o\='.  Exactly `agent-repl-create-oneshot\=', except
the commission is ABOUT the workspace this runs from: the request names it
as the one-shot\='s context (`CreateWorkspaceOneShot.context\='), and the
daemon opens the commission with a note saying the issue it cites -- a bug
in \"the feed\", say -- is that workspace\='s, by name, worktree and
branch.  The one-shot is still top-level, cut from and merging into the
primary branch; nothing of the current workspace\='s conversation or
worktree is carried over.  Unlike `SPC TAB o\=', its repository is the
current workspace\='s (`agent-repl-verbs--oneshot-repository\=').

A prefix argument PICK-MODEL asks for the model."
  (interactive "P")
  (agent-repl-verbs--create-oneshot
   'agent-repl-create-oneshot-for-current-workspace pick-model t
   #'agent-repl-verbs--oneshot-repository))

(defun agent-repl-verbs--oneshot-repository ()
  "Return the current workspace\='s repository, the one `SPC TAB C-o\=' is cut in."
  (agent-repl-verbs--dynamic-repository "a one-shot"))

;; THE EXPLANATION-ENGINE ONE-SHOT (`SPC TAB O') is the one-shot whose
;; repository is explanation-engine's: it is commissioned from any
;; workspace, doom's included, and always lands in the explanation-engine
;; checkout.  What it does on completion is still that repository's own policy
;; (`docs/ONE-SHOT-POLICY.md'): its `.agent-repl/prompts/' at the main
;; checkout states the completion directive and the after-merge
;; `/create-or-update-pr --add-to-merge-queue'.  Emacs chooses no finish.

(defconst agent-repl-verbs--explanation-engine-basename "explanation-engine"
  "The explanation-engine checkout\='s directory name under `MULTI_REPO_ROOT\='.
The Setup section of `docs/USER-GUIDE.md\=' documents `MULTI_REPO_ROOT\=' as
the directory holding this checkout.")

(defvar agent-repl-daemon-multi-repo-root)

(defun agent-repl-verbs--explanation-engine-repository ()
  "Return the `RepositoryRef\=' of the explanation-engine checkout.
The checkout is `agent-repl-verbs--explanation-engine-basename\=' under
`agent-repl-daemon-multi-repo-root\=' (`MULTI_REPO_ROOT\='), matched against
the roster\='s repository sections by canonical directory.

Refuses when `MULTI_REPO_ROOT\=' is unset, and when the checkout is not on
the roster -- registering it (`SPC TAB C-n\=') is the user\='s act."
  (let ((root agent-repl-daemon-multi-repo-root))
    (unless root
      (agent-repl--error agent-repl-verbs--create-log-scope
                         "elisp.verbs.explanation-engine-repository-unresolved cause=multi-repo-root-unset")
      (user-error "agent-repl: MULTI_REPO_ROOT is unset, so the explanation-engine checkout has no known directory; see the Setup section of docs/USER-GUIDE.md"))
    (let ((main (agent-repl--path-canonical
                 (expand-file-name agent-repl-verbs--explanation-engine-basename root))))
      (or (agent-repl-verbs--roster-repository-at main)
          (progn
            (agent-repl--warn agent-repl-verbs--create-log-scope
                              "elisp.verbs.explanation-engine-repository-unregistered root=%s main=%s"
                              root main)
            (user-error "agent-repl: the explanation-engine repository %s is not on the roster; register it with SPC TAB C-n"
                        main))))))

(defun agent-repl-create-explanation-engine-oneshot (&optional pick-model)
  "Create a ONE-SHOT in the explanation-engine repository (`SPC TAB O\=').
Exactly `agent-repl-create-oneshot\=', except the repository is always
the explanation-engine checkout
\(`agent-repl-verbs--explanation-engine-repository\='), whichever workspace
\(or none) this runs from.  The repository\='s own policy finishes it:
a one-shot merge, then `/create-or-update-pr --add-to-merge-queue\=' once
the merge lands.

A prefix argument PICK-MODEL asks for the model."
  (interactive "P")
  (agent-repl-verbs--create-oneshot
   'agent-repl-create-explanation-engine-oneshot pick-model nil
   #'agent-repl-verbs--explanation-engine-repository))

(defun agent-repl-verbs--create-oneshot (command pick-model about-current repository-fn)
  "Create a one-shot for COMMAND, asking for the model when PICK-MODEL.
The one composition every one-shot command shares: the repository is what
REPOSITORY-FN answers, the commission is read UNSEEDED into the one-shot
history, and the model is asked for only under a prefix argument.

ABOUT-CURRENT names the current workspace as the one-shot\='s context, the
workspace the commission is about; its prompt names that workspace so the
user knows which one the commission will be read against."
  (agent-repl-verbs--create-invoked command)
  (let* ((repository (agent-repl-verbs--read-for
                      command 'repository
                      repository-fn))
         (about (and about-current (agent-repl--ws-current-name)))
         (prompt (agent-repl-verbs--read-for
                  command 'prompt
                  (lambda ()
                    (agent-repl-verbs--read-required-prompt
                     (if about
                         (format "One-shot fix for %s: " about)
                       "One-shot commission: ")
                     "a one-shot IS its prompt"
                     (agent-repl-verbs--oneshot-history-file) 'unseeded))))
         (model (and pick-model
                     (agent-repl-verbs--read-for
                      command 'model #'agent-repl-verbs--read-model)))
         (context (and about (agent-repl-verbs--ref about))))
    (agent-repl--info agent-repl-verbs--create-log-scope
                      "elisp.verbs.create-one-shot mode=dynamic model=%S context=%S"
                      model about)
    (agent-repl-verb-create
     repository :one-shot
     :prompt prompt
     :context context
     :model model)))

;;;; ---- A fixed repository on the roster ---------------------------------

(defun agent-repl-verbs--roster-repository-at (main)
  "Return the `RepositoryRef' of the roster section whose directory is MAIN.
MAIN is a canonical main worktree; each section\='s directory is
canonicalized before the comparison.  Answers nil when no section matches,
because what a missing repository means -- and how it is refused -- is the
caller\='s: every creation that targets a FIXED repository rather than the
current workspace\='s finds it here."
  (when-let* ((section (cl-find-if
                        (lambda (section)
                          (let ((dir (plist-get (agent-repl-verbs--section-ref section) :dir)))
                            (and dir (equal (agent-repl--path-canonical dir) main))))
                        (agent-repl-verbs--repo-sections))))
    (agent-repl-verbs--section-ref section)))

(defun agent-repl-verbs--agent-repl-repository ()
  "Return the `RepositoryRef' of the repository agent-repl\='s source lives in.
The repository is found by its main worktree: the parent of the git
common-dir of the module root (the directory holding
`agent-repl-metaprompt-file'), so a module loaded from a linked worktree
still names its main checkout.  It is matched against the roster\='s
repository sections by canonical directory.  Every creation FIXED to
agent-repl\='s own repository finds it here.

Refuses when the module root is not in a git repository, or when that
repository is not on the roster -- a creation fixed to it has nowhere
else to go, and registering the repository (`SPC TAB C-n') is the
user\='s act."
  (let* ((module-root (file-name-directory agent-repl-metaprompt-file))
         (main (agent-repl--main-worktree-for-dir module-root)))
    (unless main
      (agent-repl--error agent-repl-verbs--create-log-scope
                         "elisp.verbs.agent-repl-repository-unresolved module-root=%s cause=not-a-git-repository"
                         module-root)
      (user-error "agent-repl: agent-repl's source at %s is in no git repository" module-root))
    (or (agent-repl-verbs--roster-repository-at main)
        (progn
          (agent-repl--warn agent-repl-verbs--create-log-scope
                            "elisp.verbs.agent-repl-repository-unregistered module-root=%s main=%s"
                            module-root main)
          (user-error "agent-repl: agent-repl's repository %s is not on the roster; register it with SPC TAB C-n"
                      main)))))

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
  (let ((at-ms (+ (agent-repl--now-unix-ms) (* minutes 60 1000)))
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
