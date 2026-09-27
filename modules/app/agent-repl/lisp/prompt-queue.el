;;; prompt-queue.el --- prompts held for later delivery -*- lexical-binding: t; -*-

;;; Commentary:

;; TWO ROLES, ONE QUEUE.  A prompt lands here for exactly one of two
;; reasons, and the difference is WHICH EDGE releases it:
;;
;;   DEFERRAL (`agent-repl-queue-deferred-prompt', `SPC j RET').  The user
;;   wrote something they want delivered as its own discrete prompt once
;;   the agent finishes what it is doing.  Released on THE FINISH EDGE --
;;   the roster row's turn-running -> settled transition, through
;;   `agent-repl-roster-finish-functions'.  That edge is the daemon's
;;   pushed verdict on whether the turn is over; Emacs derives nothing.
;;
;;   OUTAGE (`agent-repl-prompt-queue-offer').  The composer submitted and
;;   the daemon never answered, so nothing is known about whether the
;;   prompt landed.  Released on LINK-UP, through
;;   `agent-repl-link-up-functions'.
;;
;; NOTHING IS DROPPED SILENTLY.  Every held prompt leaves this queue in
;; exactly one of two ways: as a real SubmitPrompt that went onto the
;; wire, or as its own surfaced failure carrying the prompt's text back to
;; the user.  A queue that quietly forgot a prompt would be the one bug
;; this module exists to make impossible.
;;
;; THE LIVENESS GATE IS TWO FACTS, both of which must hold before a drain
;; sends anything: the link is up (`agent-repl-link-up-p') and the
;; workspace's COMPOSER GATE is not one of the refusing arms.  A drain
;; into a merging or draining composer would be refused by the daemon and
;; lose the prompt's place in the user's order for nothing.
;;
;; DRAINED PROMPTS CARRY `:deferred-prompt', whatever origin they were
;; composed under: this queue is that origin's ONE production send site,
;; which is exactly what makes a stored turn say "this was held and
;; delivered later" rather than pretending it went out when it was typed.
;;
;; ORDER IS THE USER'S TYPING ORDER, per workspace.  The drain is strictly
;; sequential over the queue, so entry N+1 never overtakes entry N.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--info "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--warn "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--error "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--ws-current-name "agent-repl-workspace" ())
(declare-function agent-repl--history-push "agent-repl-history" (&optional text))
(declare-function agent-repl--history-reset "agent-repl-history" ())
(declare-function agent-repl--history-save "agent-repl-history" (ws))
(declare-function agent-repl--input-buffer "agent-repl-input" (ws))
(declare-function agent-repl--read-input-buffer "agent-repl-input" (ws))
(declare-function agent-repl--prepare-input "agent-repl-input" (ws raw &optional force))
(declare-function agent-repl--input-said "agent-repl-input" (text attachments))
(declare-function agent-repl--input-submit "agent-repl-input" (ws said origin raw &optional key from-buffer))
(declare-function agent-repl-input-attachments "agent-repl-input" (ws))
(declare-function agent-repl-input-clear-attachments "agent-repl-input" (ws))
(declare-function agent-repl-host-composer-gate "agent-repl-host" (ws))
(declare-function agent-repl-link-up-p "agent-repl-daemon-link" ())
(declare-function agent-repl-link-primary "agent-repl-daemon-link" ())
(declare-function agent-repl-host-conn "agent-repl-host" (ws))
(declare-function agent-repl-connect-connection-alive-p "agent-repl-connect" (conn))

;; Defined by W2-B (roster.el).  Declared, never defined here: `add-hook'
;; auto-vivifies the variable, so registering on it at load time is safe
;; whichever module loads first.
(defvar agent-repl-roster-finish-functions)
(defvar agent-repl-link-up-functions)
(defvar agent-repl-link-promote-functions)
(defvar agent-repl-host-reattached-functions)

;;;; ---- State -----------------------------------------------------------

(defvar agent-repl--prompt-queue (make-hash-table :test 'equal)
  "Workspace name -> ordered list of held entries, oldest first.
Each entry is the plist `(:id ID :kind KIND :said SAID :origin ORIGIN
:raw RAW :idempotency-key KEY :queued-at SECONDS)'.  KIND is `:deferred'
or `:outage' and names WHICH EDGE releases the entry; SAID is the fully composed
`UserSaid' the composer already built, so a drain re-composes nothing and
cannot decorate a prompt twice.")

(defvar agent-repl--prompt-queue-draining (make-hash-table :test 'equal)
  "Workspaces with a drain in flight, so two edges cannot interleave.")

(defvar agent-repl--prompt-queue-seq 0
  "Monotonic counter behind each entry's `:id'.")

(defconst agent-repl--prompt-queue-drain-origin :deferred-prompt
  "The `PromptOrigin' every drained prompt carries.
This queue is that value's ONE production send site: a turn stored under
it says the prompt was held and delivered later, which is a different
editor situation from the one it was typed in.")

;;;; ---- Bookkeeping -----------------------------------------------------

(defun agent-repl-prompt-queue-pending (ws &optional kind)
  "Return WS's held entries, oldest first; only those of KIND when given."
  (let ((held (gethash ws agent-repl--prompt-queue)))
    (if kind
        (cl-remove-if-not (lambda (e) (eq (plist-get e :kind) kind)) held)
      held)))

(defun agent-repl--prompt-queue-set (ws entries)
  "Store ENTRIES as WS's queue, dropping the key when it empties."
  (if entries
      (puthash ws entries agent-repl--prompt-queue)
    (remhash ws agent-repl--prompt-queue))
  entries)

(defun agent-repl--prompt-queue-drop (ws entry)
  "Remove ENTRY from WS's queue."
  (agent-repl--prompt-queue-set ws (delq entry (gethash ws agent-repl--prompt-queue))))

(defun agent-repl--prompt-queue-enqueue (ws kind said origin raw &optional key)
  "Append a KIND entry for WS carrying SAID, ORIGIN and RAW; return it.
KEY is the idempotency key of the attempt this entry re-drives, when
there was one: an OUTAGE entry is a RETRY of a submission that may
already have landed, so it must go back out under the SAME key and let
the daemon\='s duplicate refusal do its job.  A DEFERRAL never attempted
anything, so it carries no key and mints a fresh one at drain."
  (let ((entry (list :id (format "held:%d" (cl-incf agent-repl--prompt-queue-seq))
                     :kind kind
                     :said said
                     :origin origin
                     :raw raw
                     :idempotency-key key
                     :queued-at (float-time))))
    (agent-repl--prompt-queue-set
     ws (append (gethash ws agent-repl--prompt-queue) (list entry)))
    (agent-repl--info ws "elisp.prompt-queue.held ws=%s id=%s kind=%S origin=%S key=%s depth=%d"
                      ws (plist-get entry :id) kind origin key
                      (length (gethash ws agent-repl--prompt-queue)))
    entry))

;;;; ---- The liveness gate ------------------------------------------------

(defconst agent-repl--prompt-queue-blocked-gates '(:merging :draining :restarting)
  "Composer gate arms a drain must NOT send into.
The daemon would refuse the submission, and a refused held prompt has lost
its place in the user's order for nothing.  Every other arm sends: the
daemon starts or revives the session implicitly.")

(defun agent-repl--prompt-queue-conn (ws)
  "Return the connection a drain for WS would submit on, or nil.
The SAME resolution the composer's submit uses, deliberately: the gate has
to judge the connection the send would actually take, not a different
one."
  (or (and (fboundp 'agent-repl-host-conn) (agent-repl-host-conn ws))
      (and (fboundp 'agent-repl-link-primary) (agent-repl-link-primary))))

(defun agent-repl--prompt-queue-live-conn-p (ws)
  "Return non-nil when WS's send connection exists and is still ALIVE.
`agent-repl-link-up-p' answers for the LINK, which is a different fact
from this workspace's own connection: a handover leaves the old
connection closed while the link stands, and submitting a held prompt on
a closed connection would lose it for a send that cannot happen."
  (let ((conn (agent-repl--prompt-queue-conn ws)))
    (cond
     ((null conn)
      (agent-repl--warn ws "elisp.prompt-queue.no-conn ws=%s -- entries stay queued" ws)
      nil)
     ((not (agent-repl-connect-connection-alive-p conn))
      (agent-repl--warn ws "elisp.prompt-queue.dead-conn ws=%s -- entries stay queued" ws)
      nil)
     (t t))))

(defun agent-repl-prompt-queue-deliverable-p (ws)
  "Return non-nil when a held prompt for WS may be sent right now.
THREE facts must hold: the daemon link is up, WS's own send connection is
alive, and WS's composer gate is not one of the refusing arms.  When any
of them fails the held entries stay queued -- a drain never drops what it
did not send."
  (let* ((link-up (and (agent-repl-link-up-p) t))
         (live (and link-up (agent-repl--prompt-queue-live-conn-p ws)))
         (gate (and live (agent-repl-host-composer-gate ws)))
         (blocked (and gate (memq gate agent-repl--prompt-queue-blocked-gates) t))
         (result (and live (not blocked) t)))
    (agent-repl--log ws "elisp.prompt-queue.gate ws=%s link-up=%s live=%s gate=%S deliverable=%s"
                     ws link-up (and live t) gate result)
    result))

;;;; ---- Offering ---------------------------------------------------------

(defun agent-repl-prompt-queue-offer (ws said origin raw &optional key)
  "Hold SAID for WS after a transport failure; return the held entry.
Called by the composer when the daemon never answered a submission.  The
composer keeps its text as well: nothing here is evidence the prompt did
not land, so the user is left able to see and resend exactly what they
wrote.  KEY is the failed attempt\='s idempotency key, carried so the
re-drive goes out as a RETRY of that attempt rather than as a second
turn."
  (agent-repl--prompt-queue-enqueue ws :outage said origin raw key))

(defun agent-repl-queue-deferred-prompt ()
  "Hold the composer's contents until the agent finishes its current turn.

The vendor UI already buffers keystrokes typed mid-turn, but those
interleave with whatever else is typed before the turn ends -- there is no
guarantee the text fires as its own discrete prompt.  This command gives
that guarantee: the text is captured, the composer is cleared (and the
text pushed onto the input history, so it is recallable), and the prompt
is delivered on THE FINISH EDGE -- the roster row's turn-running ->
settled transition the daemon pushes.

Each finished turn drains one entry, so a queue of several delivers one
prompt per turn in the order they were written."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (raw (or (agent-repl--read-input-buffer ws) ""))
         (attachments (agent-repl-input-attachments ws))
         (empty-p (and (string-empty-p (string-trim raw)) (null attachments))))
    (unless ws
      (user-error "No current workspace to queue a prompt for"))
    (if empty-p
        (progn
          (agent-repl--log ws "elisp.prompt-queue.defer-empty ws=%s -- nothing to queue" ws)
          (message "agent-repl: no input to queue"))
      (let* ((text (agent-repl--prepare-input ws raw))
             (said (agent-repl--input-said text attachments))
             (entry (agent-repl--prompt-queue-enqueue
                     ws :deferred said agent-repl--prompt-queue-drain-origin raw))
             (buf (agent-repl--input-buffer ws)))
        (when buf
          (with-current-buffer buf
            (agent-repl--history-push raw)
            (agent-repl--history-reset)
            (erase-buffer)))
        (agent-repl--history-save ws)
        (agent-repl-input-clear-attachments ws)
        (message "agent-repl: queued prompt #%d for %s (fires when the turn settles)"
                 (length (agent-repl-prompt-queue-pending ws :deferred)) ws)
        entry))))

;;;; ---- Draining ---------------------------------------------------------

(defun agent-repl--prompt-queue-send (ws entry)
  "Submit ENTRY for WS under the queue's own origin.
The `UserSaid' was composed when the prompt was written, so this
re-composes nothing: a second `agent-repl--prepare-input' pass would
decorate an already-decorated prompt."
  (agent-repl--info ws "elisp.prompt-queue.sending ws=%s id=%s kind=%S composed-origin=%S key=%s"
                    ws (plist-get entry :id) (plist-get entry :kind)
                    (plist-get entry :origin) (plist-get entry :idempotency-key))
  (agent-repl--input-submit ws (plist-get entry :said)
                            agent-repl--prompt-queue-drain-origin
                            (plist-get entry :raw)
                            (plist-get entry :idempotency-key)))

(defun agent-repl-prompt-queue-drain (ws &optional kind)
  "Send WS's held prompts, oldest first; only those of KIND when given.
Safe to call when nothing is held, when the workspace is not deliverable,
and while a drain is already running -- wire it to the release edges
and to nothing else.  Returns the number of entries sent."
  (let ((held (agent-repl-prompt-queue-pending ws kind)))
    (cond
     ((null held)
      (agent-repl--log ws "elisp.prompt-queue.drain-empty ws=%s kind=%S" ws kind)
      0)
     ((gethash ws agent-repl--prompt-queue-draining)
      (agent-repl--log ws "elisp.prompt-queue.drain-reentrant ws=%s held=%d" ws (length held))
      0)
     ((not (agent-repl-prompt-queue-deliverable-p ws))
      (agent-repl--info ws "elisp.prompt-queue.drain-deferred ws=%s held=%d -- not deliverable"
                        ws (length held))
      0)
     (t
      (puthash ws t agent-repl--prompt-queue-draining)
      (unwind-protect
          (let ((sent 0))
            (dolist (entry held)
              ;; Out of the queue BEFORE the send: the submit is
              ;; fire-and-forget and its own failure path speaks for it
              ;; from here, so an entry left in place could be dispatched
              ;; a second time by the next edge.
              (agent-repl--prompt-queue-drop ws entry)
              (agent-repl--prompt-queue-send ws entry)
              (cl-incf sent))
            (agent-repl--info ws "elisp.prompt-queue.drained ws=%s kind=%S sent=%d"
                              ws kind sent)
            sent)
        (remhash ws agent-repl--prompt-queue-draining))))))

;;;; ---- The two release edges --------------------------------------------

(defun agent-repl--prompt-queue-on-finish (ws)
  "Release ONE deferred prompt for WS on the roster's finish edge.
One per finished turn, deliberately: each deferred prompt is meant to be
its own discrete turn, so firing the whole queue at one settle would put
them all into a single turn's wake and lose exactly the guarantee the
deferral was asked for."
  (let ((entry (car (agent-repl-prompt-queue-pending ws :deferred))))
    (cond
     ((null entry)
      (agent-repl--log ws "elisp.prompt-queue.finish-edge ws=%s deferred=0" ws))
     ((not (agent-repl-prompt-queue-deliverable-p ws))
      (agent-repl--info ws "elisp.prompt-queue.finish-edge-deferred ws=%s id=%s"
                        ws (plist-get entry :id)))
     (t
      (agent-repl--info ws "elisp.prompt-queue.finish-edge ws=%s releasing id=%s"
                        ws (plist-get entry :id))
      (agent-repl--prompt-queue-drop ws entry)
      (agent-repl--prompt-queue-send ws entry)))))

(defun agent-repl--prompt-queue-on-link-up (&optional _conn)
  "Release every OUTAGE-held prompt, in order, on the link-up edge."
  (let ((workspaces (hash-table-keys agent-repl--prompt-queue)))
    (agent-repl--info '(:agent-repl-central "link recovery scans every workspace") "elisp.prompt-queue.link-up workspaces=%d" (length workspaces))
    (dolist (ws workspaces)
      (agent-repl-prompt-queue-drain ws :outage))))

(add-hook 'agent-repl-roster-finish-functions #'agent-repl--prompt-queue-on-finish)
(defun agent-repl--prompt-queue-on-link-promote (&optional _old _new)
  "Release every OUTAGE-held prompt on the PROMOTION edge.
The same release as link-up, on the other edge that ends an outage.  A
handover never brings the link down, so link-up never fires for it, yet a
prompt refused with `transferring_away' or `not_yet_adopted' is held
exactly until the successor owns the workspace -- and the promotion is
that moment."
  (agent-repl--prompt-queue-on-link-up))

(defun agent-repl--prompt-queue-on-reattached (ws)
  "Release WS's OUTAGE-held prompts once WS is re-attached to a daemon.
The per-workspace edge that ends an outage.  A workspace whose daemon went
without transferring it is re-attached on its own walk, AFTER the link
edges above have already run -- the promotion runs every promote hook
before host.el\='s re-registration answers -- so a prompt held while that
workspace had no daemon is released here, on the daemon that now serves
it, rather than waiting for an edge that may never come."
  (agent-repl--info ws "elisp.prompt-queue.reattached ws=%s" ws)
  (agent-repl-prompt-queue-drain ws :outage))

(add-hook 'agent-repl-link-up-functions #'agent-repl--prompt-queue-on-link-up)
(add-hook 'agent-repl-link-promote-functions #'agent-repl--prompt-queue-on-link-promote)
(add-hook 'agent-repl-host-reattached-functions #'agent-repl--prompt-queue-on-reattached)

(provide 'prompt-queue)

;;; prompt-queue.el ends here
