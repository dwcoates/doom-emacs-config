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

;; Every log call is a log-site macro (log-site.el), needed where this
;; file is expanded; config.el loads it first.
(require 'log-site)


(defconst agent-repl-mutation-progress--scope
  '(:agent-repl-central "workspace creation and daemon administration can precede workspace ownership")
  "The log scope for mutation progress: it precedes any workspace's ownership.")

;;;; ---- The one progress report ------------------------------------------
;;
;; EVERY WAY OF MAKING OR RESTORING A WORKSPACE REPORTS THROUGH THIS ONE
;; FUNCTION.  `SPC TAB n', `N', `C-n', `c', `C', `f', `o', `O', `p' and `SPC j .'
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

(eval-and-compile
(defconst agent-repl-workspace-progress-phases
  '((:create
     (:requested . "creating workspace…")
     (:accepted . "creating workspace: request accepted…")
     (:deriving-name . "creating workspace: deriving its name…")
     (:creating-worktree . "creating workspace: creating its git worktree…")
     (:starting-session . "creating workspace: starting its session…")
     (:waiting-for-network . "creating workspace: no network, so it waits and completes once the network is back…")
     (:joined . "creating workspace: an identical create is already under way, and this request joined it…")
     (:completed . "workspace created: %s")
     (:failed . "workspace creation FAILED: %s"))
    (:open
     (:requested . "opening workspace %s…")
     (:checking-worktree . "checking the workspace's worktree…")
     (:restoring-worktree . "restoring the workspace's deleted worktree from its branch…")
     (:starting-session . "starting the workspace's session…")
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
is the first fact of the sequence the user is shown.  It carries
`:joined\=' for an ack that joined an identical pending create, and
`:waiting-for-network\=' for a create held until the network is back,
which is a wait and never a failure.

EVERY IN-FLIGHT CREATE LINE OPENS WITH \"creating workspace\".  Emacs
echoes `:requested\=' the instant the command sends, but the first
daemon-reported stage lands milliseconds later and replaces it, so a
stage line that did not repeat the denotation would leave the user never
seeing that a workspace is being created.  None of the lines names the
daemon: which process does the work is not the user\='s concern.

The wording follows the startup phases\=' own: a lowercase clause, a
trailing ellipsis while the work is in flight, none once it has landed,
and FAILED in capitals because a failure has to be readable at a glance
in a line the user was not watching for.

The table is read at COMPILE time too: each entry is its own log site
\(`agent-repl-workspace-progress--record').")

(defun agent-repl-workspace-progress--site-template (template)
  "Return phase TEMPLATE as its log site's template.
Each `%s' hole becomes the next placeholder, {detail_1}, {detail_2} ...,
and a literal brace is doubled, so the site renders from the details
exactly as `format' fills TEMPLATE from them."
  (let ((n 0))
    (replace-regexp-in-string
     "%s" (lambda (_) (format "{detail_%d}" (cl-incf n)))
     (replace-regexp-in-string "[{}]" "\\&\\&" template t)
     t t))))

(defmacro agent-repl-workspace-progress--record (kind phase details)
  "Record and echo KIND's PHASE, filled from DETAILS, at the phase's own site.
Expands to one literal `agent-repl--backend-phase' call per entry of
`agent-repl-workspace-progress-phases', so every phase is its own log site:
its template is the phase's sentence (each `%s' a {detail_N}), its
operation names the phase, and the details ride as its named values.  A
KIND/PHASE the table does not hold signals."
  (let ((k (make-symbol "kind")) (p (make-symbol "phase")) (d (make-symbol "details")))
    `(let ((,k ,kind) (,p ,phase) (,d ,details))
       (pcase (cons ,k ,p)
         ,@(cl-loop
            for (kind-key . phases) in agent-repl-workspace-progress-phases
            append
            (cl-loop
             for (phase-key . template) in phases
             collect
             (let* ((site-template (agent-repl-workspace-progress--site-template template))
                    (holes (length (agent-repl-log-site-placeholders site-template))))
               `(',(cons kind-key phase-key)
                 (agent-repl--backend-phase
                  agent-repl-workspace-progress--scope ,site-template
                  ,@(cl-loop for i from 1 to holes
                             append (list (intern (format ":detail_%d" i))
                                          `(nth ,(1- i) ,d))))))))
         (_ (error "agent-repl-workspace-progress--record: %S %S is no phase" ,k ,p))))))

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
                             "elisp.workspace-progress.unknown-phase kind={kind} phase={phase} details={details}"
                             :kind (prin1-to-string kind) :phase (prin1-to-string phase) :details (prin1-to-string details))
          nil)
      (when (eq phase :failed)
        (agent-repl--error agent-repl-workspace-progress--scope
                           "elisp.workspace-progress.failed kind={kind} details={details}" :kind (prin1-to-string kind) :details (prin1-to-string details)))
      ;; The sentence is the user's copy, worded per phase by the table, and
      ;; each phase is its own site, rendered from the details as `format'
      ;; fills the sentence: what echoes is exactly the sentence.  The
      ;; sentence is made first so a phase given too few details signals,
      ;; as `format' does, before anything is recorded.
      (let ((sentence (apply #'format template details)))
        (agent-repl-workspace-progress--record kind phase details)
        sentence))))

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
`:on-waiting-for-network' (called with the held create's value),
`:on-waiting-for-requester-turn' (called with the value of a create held
for its requester's first turn),
`:on-succeeded' (called with the succeeded value) and `:on-failed' (called
with the failure ARM and VALUE).  Each is optional."
  (puthash op-id callbacks agent-repl-mutation-progress--pending)
  (agent-repl--info agent-repl-mutation-progress--scope
                    "elisp.mutation-progress.register op-id={op_id}" :op_id op-id))

(defun agent-repl-mutation-progress-forget (op-id)
  "Drop any pending callbacks for OP-ID."
  (remhash op-id agent-repl-mutation-progress--pending))

(defun agent-repl-mutation-progress--arm-failure (value)
  "Return a failed VALUE shaped `(:arm ARM :value V)' as the list (ARM V)."
  (list (plist-get value :arm) (plist-get value :value)))

(defun agent-repl-mutation-progress--dispatch-steps (op-id callbacks progress failure)
  "Dispatch one decoded step-shaped PROGRESS for OP-ID to CALLBACKS.
PROGRESS is `(:arm STEP :value V)': an `entered_stage', a create's
`waiting_for_network' or `waiting_for_requester_turn', or a terminal
`succeeded' or `failed'.  FAILURE
maps a failed value to the list (ARM VALUE) `:on-failed' is called
with, since each mutation words its failure in its own message.  Every
mutation whose progress ends on this channel dispatches through here, so
the terminal rule below holds for all of them."
  (let ((step (plist-get progress :arm))
        (value (plist-get progress :value)))
    (pcase step
      (:entered-stage
       (when-let ((fn (plist-get callbacks :on-stage)))
         (funcall fn value)))
      (:waiting-for-network
       ;; NOT TERMINAL: a create held for the network ends later on this same
       ;; op, possibly from a successor daemon, so the op stays registered.
       (when-let ((fn (plist-get callbacks :on-waiting-for-network)))
         (funcall fn value)))
      (:waiting-for-requester-turn
       ;; NOT TERMINAL: a workspace forking itself in its first turn is held
       ;; until that turn ends, and the creation follows on this same op.
       (when-let ((fn (plist-get callbacks :on-waiting-for-requester-turn)))
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
                          "elisp.mutation-progress.unknown-step op-id={op_id} step={step}"
                          :op_id op-id :step (prin1-to-string step))))))

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
                           "elisp.mutation-progress.open-without-a-stage op-id={op_id} open={open}"
                           :op_id op-id :open (prin1-to-string open))
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
                         "elisp.mutation-progress.unknown-op op-id={op_id}" :op_id (prin1-to-string op-id))
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
                              "elisp.mutation-progress.unknown-mutation op-id={op_id} arm={arm}"
                              :op_id op-id :arm (prin1-to-string arm))))))))

(provide 'mutation-progress)

;;; mutation-progress.el ends here
