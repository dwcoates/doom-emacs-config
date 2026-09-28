;;; prompt-queue.el --- prompts held for later delivery -*- lexical-binding: t; -*-

;;; Commentary:

;; ONE ROLE: THE USER'S DEFERRAL.  `agent-repl-queue-deferred-prompt'
;; (`SPC j RET') captures what the user wrote and delivers it as its own
;; discrete prompt once the agent finishes what it is doing.  It is
;; released on THE FINISH EDGE -- the roster row's turn-running -> settled
;; transition, through `agent-repl-roster-finish-functions'.  That edge is
;; the daemon's pushed verdict on whether the turn is over; Emacs derives
;; nothing.
;;
;; THIS QUEUE HOLDS NOTHING THE DAEMON FAILED TO TAKE.  A prompt whose
;; submission got no answer, or a handover refusal, is written to the
;; durable held-prompt ingress (`held-ingress.el') and becomes the
;; daemon's own held prompt; there is no in-memory outage queue and no
;; link-up, promotion or reattach edge releasing one (owner ruling,
;; 2026-09-28: held prompts survive outages and restarts).  A deferral is
;; a DIFFERENT case: it was never submitted at all, and what the user asked
;; for is "after this turn, as its own turn", which the daemon's queue
;; would classify and might interject.  When its release fails at the
;; transport, the send lands in the ingress like any other.
;;
;; NOTHING IS DROPPED SILENTLY.  Every deferred prompt leaves this queue as
;; a real SubmitPrompt, whose own failure path holds it durably.
;;
;; THE LIVENESS GATE IS THREE FACTS, all of which must hold before a
;; release sends anything: the link is up (`agent-repl-link-up-p'), the
;; workspace's connection is alive, and its COMPOSER GATE is not one of the
;; refusing arms.
;;
;; RELEASED PROMPTS CARRY `:deferred-prompt', whatever origin they were
;; composed under: this queue is that origin's ONE production send site.
;;
;; ORDER IS THE USER'S TYPING ORDER, per workspace: one entry per finished
;; turn, oldest first.

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

;;;; ---- State -----------------------------------------------------------

(defvar agent-repl--prompt-queue (make-hash-table :test 'equal)
  "Workspace name -> ordered list of deferred entries, oldest first.
Each entry is the plist `(:id ID :kind :deferred :said SAID :origin ORIGIN
:raw RAW :queued-at SECONDS)'.  SAID is the fully composed `UserSaid' the
composer already built, so a release re-composes nothing and cannot
decorate a prompt twice.")

(defvar agent-repl--prompt-queue-seq 0
  "Monotonic counter behind each entry's `:id'.")

(defconst agent-repl--prompt-queue-drain-origin :deferred-prompt
  "The `PromptOrigin' every released prompt carries.
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

(defun agent-repl--prompt-queue-enqueue (ws kind said origin raw)
  "Append a KIND entry for WS carrying SAID, ORIGIN and RAW; return it.
KIND is `:deferred', the one kind this queue holds; anything else is a
caller bug and signals.  A deferral never attempted anything, so it
carries no idempotency key and its release mints a fresh one."
  (unless (eq kind :deferred)
    (error "agent-repl prompt-queue: %S is not a kind this queue holds" kind))
  (let ((entry (list :id (format "held:%d" (cl-incf agent-repl--prompt-queue-seq))
                     :kind kind
                     :said said
                     :origin origin
                     :raw raw
                     :queued-at (float-time))))
    (agent-repl--prompt-queue-set
     ws (append (gethash ws agent-repl--prompt-queue) (list entry)))
    (agent-repl--info ws "elisp.prompt-queue.held ws=%s id=%s kind=%S origin=%S depth=%d"
                      ws (plist-get entry :id) kind origin
                      (length (gethash ws agent-repl--prompt-queue)))
    entry))

;;;; ---- The liveness gate ------------------------------------------------

(defconst agent-repl--prompt-queue-blocked-gates '(:merging :draining :restarting)
  "Composer gate arms a release must NOT send into.
The daemon would refuse the submission, and a refused held prompt has lost
its place in the user's order for nothing.  Every other arm sends: the
daemon starts or revives the session implicitly.")

(defun agent-repl--prompt-queue-conn (ws)
  "Return the connection a release for WS would submit on, or nil.
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
of them fails the held entries stay queued -- a release never drops what it
did not send."
  (let* ((link-up (and (agent-repl-link-up-p) t))
         (live (and link-up (agent-repl--prompt-queue-live-conn-p ws)))
         (gate (and live (agent-repl-host-composer-gate ws)))
         (blocked (and gate (memq gate agent-repl--prompt-queue-blocked-gates) t))
         (result (and live (not blocked) t)))
    (agent-repl--log ws "elisp.prompt-queue.gate ws=%s link-up=%s live=%s gate=%S deliverable=%s"
                     ws link-up (and live t) gate result)
    result))

;;;; ---- Deferring --------------------------------------------------------

(defun agent-repl-queue-deferred-prompt ()
  "Hold the composer's contents until the agent finishes its current turn.

The vendor UI already buffers keystrokes typed mid-turn, but those
interleave with whatever else is typed before the turn ends -- there is no
guarantee the text fires as its own discrete prompt.  This command gives
that guarantee: the text is captured, the composer is cleared (and the
text pushed onto the input history, so it is recallable), and the prompt
is delivered on THE FINISH EDGE -- the roster row's turn-running ->
settled transition the daemon pushes.

Each finished turn releases one entry, so a queue of several delivers one
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

;;;; ---- Sending ----------------------------------------------------------

(defun agent-repl--prompt-queue-send (ws entry)
  "Submit ENTRY for WS under the queue's own origin.
The `UserSaid' was composed when the prompt was written, so this
re-composes nothing: a second `agent-repl--prepare-input' pass would
decorate an already-decorated prompt."
  (agent-repl--info ws "elisp.prompt-queue.sending ws=%s id=%s kind=%S composed-origin=%S"
                    ws (plist-get entry :id) (plist-get entry :kind)
                    (plist-get entry :origin))
  (agent-repl--input-submit ws (plist-get entry :said)
                            agent-repl--prompt-queue-drain-origin
                            (plist-get entry :raw)))

;;;; ---- The release edge ------------------------------------------------

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

(add-hook 'agent-repl-roster-finish-functions #'agent-repl--prompt-queue-on-finish)

(provide 'prompt-queue)

;;; prompt-queue.el ends here
