;;; prompt-queue.el --- Prompts held across a backend bounce -*- lexical-binding: t; -*-

;;; Commentary:

;; THE HELD-PROMPT QUEUE (component C of the seamless-bounce contract).
;;
;; WHAT IT FIXES.  A prompt sent while the frontend UDS link was down did not
;; wait for the link — it went straight at a dead socket and came back as
;; `client.command_unacked': "the daemon never acknowledged the prompt
;; command".  The user's words were then gone, because the input buffer had
;; already been cleared by the send path.  A full backend bounce (store,
;; sidecar, daemon, shim roll) is supposed to be imperceptible, and that
;; failure card was the loudest thing about it.
;;
;; WHAT IT DOES.  A prompt offered while the link is down is HELD rather than
;; sent: recorded in a per-workspace ordered queue, surfaced as PENDING, and
;; sent for real once the link is back AND the workspace has a live session
;; controller behind it.
;;
;; THE DRAIN GATE IS REVIVAL, NOT RECONNECT, and the difference is the whole
;; design.  A reconnected socket proves the daemon is up; it proves nothing
;; about whether THIS workspace has anything attached to it.  A workspace whose
;; wired axis reads severed or hibernated serves a durable replay with no
;; controller reading it, so a prompt fired there would vanish into a
;; conversation nothing is driving — the exact silent loss this module exists to
;; prevent.  So the gate is `agent-repl--frontend-session-controller-live-p',
;; the same `SessionView.shim_attached' verdict the switch-ensure uses, and the
;; drain edge is the SNAPSHOT-APPLIED hook rather than the socket-open one (a
;; socket with no state behind it cannot answer the liveness question at all).
;;
;; NOTHING IS EVER DROPPED SILENTLY.  Every held prompt leaves this queue in
;; exactly one of two ways: as a real `submitPrompt' that went onto the wire, or
;; as its OWN surfaced failure.  A prompt refused on drain reports the refusal;
;; a workspace that never comes back within `agent-repl-prompt-queue-revival-bound'
;; reports that, per held prompt, with the prompt's own text — so the user can
;; still see and re-send what they wrote.
;;
;; ORDER IS THE USER'S TYPING ORDER, per workspace.  The drain is strictly
;; sequential: entry N+1 is not dispatched until entry N has left the queue, so
;; a slow ensure on the first prompt cannot let the second overtake it.  A
;; prompt offered while a drain is still running is appended behind the entries
;; still to go, for the same reason, even though the link is by then up.
;;
;; THE SEAMS ARE FUNCTION VARIABLES (link, revival, send, pending, failure).
;; The queue is pure bookkeeping over them, which is what makes every rule above
;; testable without a daemon, a socket or a workspace.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--log "core")
(declare-function agent-repl--warn "core")
(declare-function agent-repl--uds-connected-p "frontend-uds")
(declare-function agent-repl--frontend-session-controller-live-p "frontend-client")
(declare-function agent-repl--gui-dispatch-turn "frontend-client")

(defcustom agent-repl-prompt-queue-revival-bound 60
  "Seconds a held prompt waits for its workspace to come back.

The bound the daemon's restart announcement promises a bounce fits inside.
Past it the hold stops being an honest \"any moment now\" and becomes a
lie, so every overdue held prompt is failed individually and its text is
handed back to the user rather than kept waiting on a revival that is not
coming.

Counted per prompt from ITS OWN submission, never from the queue's first
entry: a prompt typed late in an outage is owed the full bound too."
  :type 'integer
  :group 'agent-repl)

(defvar agent-repl--prompt-queue (make-hash-table :test 'equal)
  "Workspace -> ordered list of held prompt entries, oldest first.

Each entry is a plist: `:id' (this entry's local identity — never a
request id, since no command has been sent), `:text' (what goes on the
wire), `:raw' (what goes back to the input buffer, which differs from
`:text' whenever a metaprompt decoration was applied), `:prompt-origin',
`:queued-at' and `:on-settle'.")

(defvar agent-repl--prompt-queue-draining (make-hash-table :test 'equal)
  "Workspaces with a drain in flight, so two link-up edges cannot interleave.")

(defvar agent-repl--prompt-queue-timers (make-hash-table :test 'equal)
  "Entry id -> the revival-deadline timer armed for it.")

(defvar agent-repl--prompt-queue-seq 0
  "Monotonic counter behind each entry's `:id'.")

;;;; ---- The injected seams ----------------------------------------------

(defvar agent-repl-prompt-queue-link-down-function
  (lambda () (not (agent-repl--uds-connected-p)))
  "Called with no args; non-nil when the link cannot carry a command.")

(defvar agent-repl-prompt-queue-revived-function
  (lambda (ws) (agent-repl--frontend-session-controller-live-p ws))
  "Called with a workspace; non-nil when a LIVE session controller holds it.
The drain gate.  See the commentary on why the link being up is not enough.")

(defvar agent-repl-prompt-queue-send-function
  (lambda (ws entry on-sent on-failed)
    ;; Through the gui frontend's OWN dispatch path, not the bare command
    ;; write: a prompt that waited out a bounce must land as a real turn —
    ;; thinking state, `:sent-turn' record, posthooks, summary kickoff — or
    ;; the user would watch it go out and see nothing happen.  The entry's
    ;; `:on-settle' is deliberately NOT passed down; this queue runs it, so
    ;; passing it too would settle the same prompt twice.
    (agent-repl--gui-dispatch-turn
     ws (plist-get entry :text) (plist-get entry :raw)
     (plist-get entry :prompt-origin) nil on-sent on-failed))
  "Send one held ENTRY for real.
Called with (WS ENTRY ON-SENT ON-FAILED).  ON-SENT receives the dispatched
request id; ON-FAILED receives a failure detail string.  Exactly one of the
two must run, or the drain behind this entry never resumes.")

(defvar agent-repl-prompt-queue-pending-function
  #'agent-repl--prompt-queue-default-note-pending
  "Called with (WS ENTRY) to surface a newly held prompt as PENDING.
Never an acknowledgement: nothing has been sent when this runs.")

(defvar agent-repl-prompt-queue-failure-function
  #'agent-repl--prompt-queue-default-note-failure
  "Called with (WS ENTRY REASON) for a held prompt that will never be sent.
The one thing this module may never skip.")

(defun agent-repl--prompt-queue-default-note-pending (ws entry)
  "Surface held ENTRY for WS as pending, through the quiet log sink."
  (agent-repl--log ws "prompt-queue: HELD id=%s len=%d — the link is down; \
the prompt will be sent when the workspace comes back"
                   (plist-get entry :id) (length (plist-get entry :text))))

(defun agent-repl--prompt-queue-default-note-failure (ws entry reason)
  "Surface held ENTRY for WS as LOST, naming REASON and returning its text."
  (agent-repl--warn ws "prompt-queue: a held prompt was NEVER SENT (%s); \
its text follows so it is not lost: %s"
                    reason (plist-get entry :raw)))

;;;; ---- Bookkeeping ------------------------------------------------------

(defun agent-repl-prompt-queue-pending (ws)
  "Return WS's held prompt entries, oldest first."
  (gethash ws agent-repl--prompt-queue))

(defun agent-repl--prompt-queue-set (ws entries)
  "Store ENTRIES as WS's queue, dropping the key when it empties."
  (if entries
      (puthash ws entries agent-repl--prompt-queue)
    (remhash ws agent-repl--prompt-queue))
  entries)

(defun agent-repl--prompt-queue-cancel-timer (entry)
  "Disarm ENTRY's revival deadline, if one is still armed."
  (let* ((id (plist-get entry :id))
         (timer (gethash id agent-repl--prompt-queue-timers)))
    (when timer (cancel-timer timer))
    (remhash id agent-repl--prompt-queue-timers)))

(defun agent-repl--prompt-queue-drop (ws entry)
  "Remove ENTRY from WS's queue and disarm its deadline."
  (agent-repl--prompt-queue-cancel-timer entry)
  (agent-repl--prompt-queue-set
   ws (delq entry (agent-repl-prompt-queue-pending ws))))

(defun agent-repl--prompt-queue-fail (ws entry reason)
  "Drop ENTRY from WS and surface REASON as its own failure."
  (agent-repl--prompt-queue-drop ws entry)
  (funcall agent-repl-prompt-queue-failure-function ws entry reason))

;;;; ---- Offering ---------------------------------------------------------

(defun agent-repl-prompt-queue-offer (ws text raw prompt-origin &optional on-settle)
  "Offer one prompt to WS's hold queue; return non-nil when it was HELD.

Nil means the link is up and nothing is already held for WS, so the caller
must send normally — this queue never stands between a working link and a
prompt.

TEXT is what goes on the wire and RAW is what belongs back in the input
buffer if the prompt is ultimately lost; they differ whenever the send path
decorated the prompt, and handing the decoration back to the user would be
handing them something they did not write.  ON-SETTLE, when given, runs once
this prompt reaches a verdict — sent or failed — exactly as the live send
path's does.

A prompt is ALSO held while a drain is still running, even though the link
is by then up: appending behind the entries still to go is what preserves
the user's order, where sending directly would overtake them."
  (let ((held (agent-repl-prompt-queue-pending ws)))
    (when (or (funcall agent-repl-prompt-queue-link-down-function)
              held
              (gethash ws agent-repl--prompt-queue-draining))
      (let ((entry (list :id (format "held:%d" (cl-incf agent-repl--prompt-queue-seq))
                         :text text
                         :raw raw
                         :prompt-origin prompt-origin
                         :queued-at (float-time)
                         :on-settle on-settle)))
        (agent-repl--prompt-queue-set ws (append held (list entry)))
        (funcall agent-repl-prompt-queue-pending-function ws entry)
        (agent-repl--prompt-queue-arm-deadline ws entry)
        entry))))

(defun agent-repl--prompt-queue-arm-deadline (ws entry)
  "Arm ENTRY's revival deadline: fail it if WS has not come back by then."
  (puthash (plist-get entry :id)
           (run-at-time agent-repl-prompt-queue-revival-bound nil
                        #'agent-repl--prompt-queue-deadline ws entry)
           agent-repl--prompt-queue-timers))

(defun agent-repl--prompt-queue-deadline (ws entry)
  "Fail ENTRY when WS is still not back, else let the drain have it.

A deadline that fires on a workspace that HAS revived is not a failure —
the drain simply has not reached this entry yet — so it re-drives the drain
rather than killing a prompt that is about to be sent."
  (when (memq entry (agent-repl-prompt-queue-pending ws))
    (if (funcall agent-repl-prompt-queue-revived-function ws)
        (agent-repl-prompt-queue-drain ws)
      (agent-repl--prompt-queue-settle
       ws entry
       (format "the workspace did not come back within %ds"
               agent-repl-prompt-queue-revival-bound)))))

(defun agent-repl--prompt-queue-settle (ws entry reason)
  "Fail ENTRY for REASON, run its `:on-settle', and continue WS's drain."
  (let ((on-settle (plist-get entry :on-settle)))
    (agent-repl--prompt-queue-fail ws entry reason)
    (when on-settle (funcall on-settle))))

;;;; ---- Draining ---------------------------------------------------------

(defun agent-repl-prompt-queue-drain (ws)
  "Send WS's held prompts, in order, once its session controller is live.

Safe to call when nothing is held, when WS has not revived, and while a
drain is already running — wire it to the snapshot-applied edge and to
nothing else.

Strictly sequential: the next entry is dispatched only from the previous
one's settle, so the queue cannot reorder itself under a slow ensure.  A
workspace that goes away mid-drain STOPS the drain, leaving the remaining
entries held in order for the next revival rather than firing them at a
link that just left."
  (let ((held (agent-repl-prompt-queue-pending ws)))
    (cond
     ((null held) nil)
     ((gethash ws agent-repl--prompt-queue-draining) nil)
     ((not (funcall agent-repl-prompt-queue-revived-function ws))
      (agent-repl--log ws "prompt-queue: drain DEFERRED held=%d — no live session controller"
                       (length held))
      nil)
     (t
      (puthash ws t agent-repl--prompt-queue-draining)
      (agent-repl--log ws "prompt-queue: draining held=%d" (length held))
      (agent-repl--prompt-queue-step ws)
      t))))

(defun agent-repl--prompt-queue-step (ws)
  "Dispatch WS's oldest held prompt, or finish the drain when none is left."
  (let ((entry (car (agent-repl-prompt-queue-pending ws))))
    (cond
     ((null entry) (remhash ws agent-repl--prompt-queue-draining))
     ;; The workspace left mid-drain.  Everything still held stays held, in
     ;; order, for the revival that may still come — the deadline armed on each
     ;; entry is what guarantees they are not held forever.
     ((not (funcall agent-repl-prompt-queue-revived-function ws))
      (remhash ws agent-repl--prompt-queue-draining)
      (agent-repl--log ws "prompt-queue: drain STOPPED held=%d — the session controller went away mid-drain"
                       (length (agent-repl-prompt-queue-pending ws))))
     (t
      (let ((on-settle (plist-get entry :on-settle)))
        ;; Out of the queue BEFORE the send: the send is fire-and-forget and its
        ;; own failure path is what speaks for it from here, so an entry left in
        ;; the queue could be dispatched a second time by a concurrent drain.
        (agent-repl--prompt-queue-drop ws entry)
        (funcall agent-repl-prompt-queue-send-function
                 ws entry
                 (lambda (request-id)
                   (agent-repl--log ws "prompt-queue: held prompt SENT id=%s request-id=%s"
                                    (plist-get entry :id) request-id)
                   (when on-settle (funcall on-settle))
                   (agent-repl--prompt-queue-step ws))
                 (lambda (detail)
                   ;; A refused held prompt is a lost prompt the user watched
                   ;; sit pending.  It gets its own account, and the drain
                   ;; carries on: the prompts behind it are not this one's
                   ;; casualties.
                   (funcall agent-repl-prompt-queue-failure-function
                            ws entry (format "%s" detail))
                   (when on-settle (funcall on-settle))
                   (agent-repl--prompt-queue-step ws))))))))

(defun agent-repl-prompt-queue-drain-all ()
  "Drain every workspace holding prompts.  The snapshot-applied subscriber."
  (dolist (ws (hash-table-keys agent-repl--prompt-queue))
    (agent-repl-prompt-queue-drain ws)))

;;;; ---- The drain edge ---------------------------------------------------
;;
;; THE SNAPSHOT-APPLIED HOOK, never the socket-open one.  An open socket is a
;; link, not a recovery: no daemon state has landed yet, so the revival gate
;; would be asked its question with an empty roster to answer it from and would
;; defer every held prompt for nothing.  The snapshot-applied edge is the first
;; instant Emacs holds the state of the world as of reconnection, which is
;; exactly when a held prompt's "has this workspace come back?" can be
;; answered — and it is the same edge the webapp drains on (`adoptSnapshot').
(add-hook 'agent-repl-uds-snapshot-applied-functions
          #'agent-repl-prompt-queue-drain-all)

(provide 'prompt-queue)
;;; prompt-queue.el ends here
