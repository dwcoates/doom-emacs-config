;;; mutation-progress.el --- Correlate workspace-mutation progress -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A workspace mutation under option B (a create with an op_id) is ACKED at once
;; and worked in the daemon's background; its staged progress and terminal
;; outcome arrive later on the daemon-level WatchDaemon stream, keyed on the
;; op_id the command minted.  This module is the correlation seat: a command
;; mints an op_id, registers the callbacks that render its progress, and the
;; daemon-link's push dispatch hands each decoded progress event here to match
;; it back to its command.
;;
;; The stream is a BROADCAST — every WatchDaemon subscriber receives every push
;; — so a progress event for an op this Emacs never registered (another client's
;; op, or a stale event the daemon's topic replayed to a fresh subscription) is
;; expected and dropped quietly, never an error.

;;; Code:

(declare-function agent-repl--info "core")
(declare-function agent-repl--log "core")
(declare-function agent-repl--error "core")
(declare-function agent-repl--backend-phase "core" (ws fmt &rest args))

(defconst agent-repl-mutation-progress--scope
  '(:agent-repl-central "workspace creation and daemon administration can precede workspace ownership")
  "The log scope for mutation progress: it precedes any workspace's ownership.")

;;;; ---- The one progress report ------------------------------------------
;;
;; EVERY WAY OF MAKING OR RESTORING A WORKSPACE REPORTS THROUGH THIS ONE
;; FUNCTION.  `SPC TAB n', `N', `c', `C', `f', `o', `O', `C-n' and `SPC j .'
;; are nine gestures over four rpcs, and each of them used to word its own
;; feedback -- where it worded any at all.  A user cannot learn nine
;; vocabularies for one operation, so the vocabulary is a TABLE here and the
;; entry points supply only which phase they reached.
;;
;; The echo goes through `agent-repl--backend-phase', which is the module's
;; existing startup-phase channel: one call produces both the durable log
;; record and the minibuffer line, and the line is prefixed "agent-repl: "
;; exactly as "bouncing the store service…" and "backend restart complete"
;; already are.  That is deliberate -- a workspace coming up is the same kind
;; of event as the backend coming up, and it should read the same.

(defconst agent-repl-workspace-progress-phases
  '((:create
     (:requested . "creating workspace…")
     (:accepted . "the daemon accepted the workspace create…")
     (:deriving-name . "deriving the workspace's name…")
     (:creating-worktree . "creating the workspace's git worktree…")
     (:starting-session . "starting the workspace's session…")
     (:completed . "workspace created: %s")
     (:failed . "workspace creation FAILED: %s"))
    (:open
     (:requested . "opening workspace %s…")
     (:checking-worktree . "checking the workspace's worktree…")
     (:restoring-worktree . "restoring the workspace's deleted worktree from its branch…")
     (:starting-session . "starting the workspace's session…")
     (:reviving . "reviving the hibernated workspace…")
     (:clearing-closed . "clearing the workspace's closed flag…")
     (:checking-build . "checking the workspace's build…")
     (:completed . "workspace opened: %s")
     (:failed . "opening the workspace FAILED: %s"))
    (:bind
     (:requested . "binding %s to the chosen conversation…")
     (:starting-session . "starting the session on the chosen conversation…")
     (:completed . "%s is now on the chosen conversation")
     (:failed . "binding %s to that conversation FAILED: %s"))
    (:register
     (:requested . "registering directory %s…")
     (:completed . "workspace registered: %s")
     (:failed . "registering the directory FAILED: %s"))
    (:register-repository
     (:requested . "registering the repository holding %s…")
     (:completed . "%s repository %s; workspace %s %s")
     (:failed . "registering the repository FAILED: %s")))
  "The sentence every workspace create/open/register phase is echoed as.

KIND -> ((PHASE . TEMPLATE) ...).  A template\='s `%s\=' holes are filled by
the DETAILS its caller passes, and a template with none takes no details
-- so every phase is reported through the same call whether or not it has
anything to name.

The three phases every KIND carries are `:requested\=' (Emacs is sending the
request), `:completed\=' and `:failed\='; everything between them is a stage
the DAEMON reported reaching, and a kind lists only the stages its rpc
actually has.  A create also carries `:accepted\=': its rpc answers
with an option-B ack before the background work starts, and that ack
is the first fact of the sequence the user is shown.

The wording follows the startup phases\=' own: a lowercase clause, a
trailing ellipsis while the work is in flight, none once it has landed,
and FAILED in capitals because a failure has to be readable at a glance
in a line the user was not watching for.")

(defconst agent-repl-workspace-progress--scope
  agent-repl-mutation-progress--scope
  "The log scope every workspace-progress phase is recorded under.
The same scope the correlation seat uses: a create\='s phases PRECEDE the
workspace\='s existence, so none of them has a workspace sink to route to.")

(defun agent-repl-workspace-progress-report (kind phase &rest details)
  "Report that a workspace mutation of KIND reached PHASE, naming DETAILS.

THE ONE REPORTING FUNCTION.  It records the phase and echoes it in a
single call, so a phase can never be logged without being shown or shown
without being logged.  The TEMPLATE is what the log record\='s operation
name is derived from, which is why DETAILS ride as format arguments
rather than being pasted into the sentence by the caller: a runtime
value must never become part of an operation name.

A `:failed\=' phase is ALSO recorded at ERROR, so a failure is in the
durable record at the level a failure deserves rather than only in a line
that scrolls away.

Returns the echoed sentence, or nil when KIND/PHASE names no template --
which is a bug in the caller, reported and never guessed at."
  (let ((template (alist-get phase (alist-get kind agent-repl-workspace-progress-phases))))
    (if (null template)
        (progn
          (agent-repl--error agent-repl-workspace-progress--scope
                             "elisp.workspace-progress.unknown-phase kind=%S phase=%S details=%S"
                             kind phase details)
          nil)
      (when (eq phase :failed)
        (agent-repl--error agent-repl-workspace-progress--scope
                           "elisp.workspace-progress.failed kind=%S details=%S" kind details))
      (apply #'agent-repl--backend-phase agent-repl-workspace-progress--scope
             template details)
      (apply #'format template details))))

(defvar agent-repl-mutation-progress--pending (make-hash-table :test 'equal)
  "Map of op id -> callback plist for an in-flight workspace mutation.
The plist keys are `:on-stage', `:on-succeeded' and `:on-failed'; each is
optional.  A terminal event (succeeded or failed) removes the entry.")

(defvar agent-repl-mutation-progress--counter 0
  "A per-session monotonic counter, the tail of every minted op id.")

(defun agent-repl-mutation-progress-new-op-id ()
  "Mint a fresh op id, unique among this Emacs's in-flight mutations.
The wall-clock prefix keeps it from colliding with an id a previous
session minted, whose stale progress the daemon's topic could replay to a
fresh subscription."
  (setq agent-repl-mutation-progress--counter
        (1+ agent-repl-mutation-progress--counter))
  (format "op-%d-%d"
          (time-convert nil 'integer)
          agent-repl-mutation-progress--counter))

(defun agent-repl-mutation-progress-register (op-id &rest callbacks)
  "Register CALLBACKS for the mutation OP-ID.
CALLBACKS is a plist of `:on-stage' (called with the stage keyword),
`:on-succeeded' (called with the succeeded value) and `:on-failed' (called
with the failure ARM and VALUE).  Each is optional."
  (puthash op-id callbacks agent-repl-mutation-progress--pending)
  (agent-repl--info agent-repl-mutation-progress--scope
                    "elisp.mutation-progress.register op-id=%s" op-id))

(defun agent-repl-mutation-progress-forget (op-id)
  "Drop any pending callbacks for OP-ID."
  (remhash op-id agent-repl-mutation-progress--pending))

(defun agent-repl-mutation-progress--arm-failure (value)
  "Return a failed VALUE shaped `(:arm ARM :value V)' as the list (ARM V)."
  (list (plist-get value :arm) (plist-get value :value)))

(defun agent-repl-mutation-progress--dispatch-steps (op-id callbacks progress failure)
  "Dispatch one decoded step-shaped PROGRESS for OP-ID to CALLBACKS.
PROGRESS is `(:arm STEP :value V)': an `entered_stage', or a terminal
`succeeded' or `failed'.  FAILURE maps a failed value to the list (ARM
VALUE) `:on-failed' is called with, since each mutation words its failure
in its own message.  Every mutation whose progress ends on this channel
dispatches through here, so the terminal rule below holds for all of them."
  (let ((step (plist-get progress :arm))
        (value (plist-get progress :value)))
    (pcase step
      (:entered-stage
       (when-let ((fn (plist-get callbacks :on-stage)))
         (funcall fn value)))
      (:succeeded
       ;; TERMINAL: the op is done, so forget it BEFORE the callback runs its
       ;; editor changes -- a callback that errors must not leave the op live.
       (agent-repl-mutation-progress-forget op-id)
       (when-let ((fn (plist-get callbacks :on-succeeded)))
         (funcall fn value)))
      (:failed
       (agent-repl-mutation-progress-forget op-id)
       (when-let ((fn (plist-get callbacks :on-failed)))
         (apply fn (funcall failure value))))
      (_
       (agent-repl--error agent-repl-mutation-progress--scope
                          "elisp.mutation-progress.unknown-step op-id=%s step=%S"
                          op-id step)))))

(defun agent-repl-mutation-progress--dispatch-create (op-id callbacks create)
  "Dispatch one decoded WorkspaceCreateProgress CREATE for OP-ID to CALLBACKS.
CREATE is `(:arm STEP :value V)'."
  (agent-repl-mutation-progress--dispatch-steps
   op-id callbacks create #'agent-repl-mutation-progress--arm-failure))

(defun agent-repl-mutation-progress--dispatch-open (op-id callbacks open)
  "Dispatch one decoded WorkspaceOpenProgress OPEN for OP-ID to CALLBACKS.
OPEN is `(:stage STAGE)\='.

AN OPEN HAS NO TERMINAL STEP ON THIS CHANNEL: its rpc answers
synchronously, so the caller learns success and every refusal from the
answer and forgets the op there.  Everything that arrives here is a
stage."
  (let ((stage (plist-get open :stage)))
    (if (null stage)
        (agent-repl--error agent-repl-mutation-progress--scope
                           "elisp.mutation-progress.open-without-a-stage op-id=%s open=%S"
                           op-id open)
      (when-let ((fn (plist-get callbacks :on-stage)))
        (funcall fn stage)))))

(defun agent-repl-mutation-progress-handle (progress)
  "Dispatch one decoded WorkspaceMutationProgress PROGRESS to its op's callbacks.
PROGRESS is `(:op-id ID :event (:arm ARM :value V))'.  An op id this Emacs
never registered is dropped quietly: the stream is a broadcast, so a push
for another client's op, or a replayed stale one, is expected."
  (let* ((op-id (plist-get progress :op-id))
         (callbacks (gethash op-id agent-repl-mutation-progress--pending)))
    (if (null callbacks)
        (agent-repl--log agent-repl-mutation-progress--scope
                         "elisp.mutation-progress.unknown-op op-id=%S" op-id)
      (let* ((event (plist-get progress :event))
             (arm (plist-get event :arm))
             (value (plist-get event :value)))
        (pcase arm
          (:create (agent-repl-mutation-progress--dispatch-create op-id callbacks value))
          (:open (agent-repl-mutation-progress--dispatch-open op-id callbacks value))
          ;; A kill's failure is a bare sentence; a nuke's is arm-shaped,
          ;; exactly as a create's is.
          (:kill (agent-repl-mutation-progress--dispatch-steps
                  op-id callbacks value
                  (lambda (failed) (list :internal (plist-get failed :internal)))))
          (:nuke (agent-repl-mutation-progress--dispatch-steps
                  op-id callbacks value #'agent-repl-mutation-progress--arm-failure))
          (_
           (agent-repl--error agent-repl-mutation-progress--scope
                              "elisp.mutation-progress.unknown-mutation op-id=%s arm=%S"
                              op-id arm)))))))

(provide 'mutation-progress)

;;; mutation-progress.el ends here
