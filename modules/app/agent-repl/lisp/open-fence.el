;;; open-fence.el --- client-side terminal-open fence -*- lexical-binding: t; -*-

;;; Commentary:

;; open-fence.el is the CLIENT half of the daemon's vanished-resume fence.
;;
;; THE DEFECT IT CLOSES.  `open_workspace' is acked EARLY: the ack means
;; ACCEPTED, and the bring-up runs behind it (daemon
;; internal/server/openbringup.go).  So a bring-up that fails terminally —
;; the recorded conversation's transcript is gone and the workspace has no
;; sibling transcript to resume, which the daemon fences per boot and refuses
;; with `retry=never' — reaches this end as a SUCCESSFUL ack plus a pushed
;; failure card.  Nothing on the ensure ladder ever counted a failure, so
;; `agent-repl--frontend-note-ensure-failure' never gave up, and the three
;; ensure drivers (a perspective switch, the reconnect snapshot, the 15s
;; reattach sweep) re-sent the identical doomed open forever, bounded only by
;; the 30s cooldown.  Observed: an open every 20-40s, indefinitely, against a
;; fact that cannot change on its own.
;;
;; WHAT IT READS, AND WHY THAT AND NOTHING ELSE.  A pushed `FailureCardView'
;; states its own disposition on the wire, in two typed fields:
;;
;;   - the LIFECYCLE arm.  `terminal' means, verbatim from failure-card.proto,
;;     "the failure has no closing edge and never will".  An `open' card
;;     invites waiting; a `terminal' one does not.
;;   - the KIND arm.  Only the two CONTINUITY kinds fence — a resume that
;;     failed, and a conversation that cannot be resumed at all.  Those are the
;;     kinds whose failure means the workspace has no session and cannot get
;;     one by being asked again.
;;
;; BOTH are required.  A terminal card of some other kind (a turn that ended,
;; a vendor refusal) says nothing about whether the workspace can be opened,
;; and an OPEN continuity failure is exactly the retryable case the ensure
;; ladder already handles.  Neither field is prose, so this never matches on a
;; message a daemon build is free to reword.
;;
;; IT HIDES NOTHING.  The card itself is the webapp's to render and is
;; untouched here; the fence only stops the BACKGROUND retry, and it says so
;; once, loudly, when it engages.
;;
;; THE WAYS OUT ARE BOTH EXPLICIT.  A daemon instance change clears every
;; fence (`agent-repl--frontend-note-boot-id'), because the failure belonged to
;; the instance that reported it; and a user-issued session restart clears the
;; workspace's own fence before the command goes out, so `SPC o C-c' retries
;; exactly once and re-fences if the daemon still refuses.  This mirrors the
;; daemon's own fence, which is cleared by a hard restart and by nothing else.

;;; Code:

(require 'cl-lib)

(declare-function agent-repl--log "core")
(declare-function agent-repl--log-verbose "core")
(declare-function agent-repl--warn "core")
(declare-function agent-repl--ws-get "workspace")
(declare-function agent-repl--ws-put "workspace")
(declare-function agent-repl--ws-log-name "workspace")
(declare-function agent-repl--frontend-ws-name "frontend-state")

(defconst agent-repl--open-fence-terminal-kinds
  '(:sessionResumeFailed :conversationUnresumable)
  "`FailureKind' oneof arms whose TERMINAL card fences a workspace's opens.
Both are CONTINUITY failures: the workspace has a conversation the daemon
could not reach, and it will not start a blank one in its place.  A card
of any other kind — however terminal — describes something other than the
workspace's ability to be opened and never fences it.

Keyed by the protojson arm name, so an unfamiliar kind simply fails to
match rather than being read as one of these.")

(defun agent-repl--open-fence-card-terminal-p (card)
  "Return non-nil when CARD is a TERMINAL card of a fencing kind.
CARD is a `FailureCardView' plist.  Both halves are required — see this
module's commentary for why either one alone is the wrong question."
  (and (plist-member card :terminal)
       (let ((kind (plist-get card :kind)))
         (and kind
              (cl-some (lambda (arm) (plist-member kind arm))
                       agent-repl--open-fence-terminal-kinds)
              t))))

(defun agent-repl--open-fence-item-card (item)
  "Return ITEM's `FailureCardView' plist, or nil when it carries no card.
ITEM is a `ConversationItem' plist as pushed on a `ConversationDelta'."
  (plist-get item :failureCard))

(defun agent-repl--open-fence-active-p (ws)
  "Return non-nil when WS is fenced against AUTOMATIC opens."
  (and ws (eq (agent-repl--ws-get ws :open-fenced) t)))

(defun agent-repl--open-fence-detail (ws)
  "Return the sentence of the card that fenced WS, or nil."
  (and ws (agent-repl--ws-get ws :open-fenced-detail)))

(defun agent-repl--open-fence-mark (ws detail)
  "Fence WS against automatic opens, citing DETAIL.
Returns non-nil when THIS call established the fence.  A re-observation
is silent at anything above verbose: the daemon re-publishes its standing
card on every refused open, and one report per fence is the point."
  (cond
   ((null ws) nil)
   ((agent-repl--open-fence-active-p ws)
    (agent-repl--log-verbose (agent-repl--ws-log-name ws)
                             "open-fence: ws=%s already fenced" ws)
    nil)
   (t
    ;; One fact, so the marker and its account are written together: split by
    ;; a `C-g' the workspace would be fenced with nothing to say why.
    (let ((inhibit-quit t))
      (agent-repl--ws-put ws :open-fenced t)
      (agent-repl--ws-put ws :open-fenced-detail detail))
    (agent-repl--warn ws (concat "open-fence: ws=%s the daemon reported a TERMINAL "
                                 "open failure (%s) — automatic re-opens are stopped; "
                                 "the failure card stands, and an explicit session "
                                 "restart will retry once")
                      ws (or detail "no detail supplied"))
    t)))

(defun agent-repl--open-fence-clear (ws reason)
  "Drop WS's open fence because REASON entitles it to another attempt.
Clearing does NOT assert the open will now succeed: it only lets the
ensure ladder ask again, and a daemon that still refuses re-fences on the
card it publishes."
  (when (and ws (agent-repl--open-fence-active-p ws))
    (let ((inhibit-quit t))
      (agent-repl--ws-put ws :open-fenced nil)
      (agent-repl--ws-put ws :open-fenced-detail nil))
    (agent-repl--log ws "open-fence: ws=%s CLEARED (%s)" ws reason)
    t))

(defun agent-repl--open-fence-note-delta (delta)
  "Fence DELTA's workspace when it carries a terminal continuity failure.
DELTA is a `ConversationDelta' plist.  Returns the number of fencing
cards seen, which is 0 for the overwhelmingly common delta that carries
none.

The workspace is resolved exactly as the permission reader resolves it:
the frame names a session CWD and this end keys workspaces by persp NAME,
so an unresolved path would fence a workspace nothing else addresses."
  (let* ((raw (plist-get delta :workspace))
         (ws (agent-repl--frontend-ws-name raw))
         (seen 0))
    (if (null ws)
        ;; NOT silent: a terminal failure this end cannot address is a fence
        ;; that will never engage, and the retry loop it was meant to stop
        ;; goes on running.
        (when (cl-some (lambda (item)
                         (let ((card (agent-repl--open-fence-item-card item)))
                           (and card (agent-repl--open-fence-card-terminal-p card))))
                       (plist-get delta :items))
          (agent-repl--warn nil (concat "open-fence: a TERMINAL open failure arrived for "
                                        "wire-workspace=%s, which resolves to no known "
                                        "workspace — it cannot be fenced")
                            raw)
          0)
      (dolist (item (plist-get delta :items))
        (let ((card (agent-repl--open-fence-item-card item)))
          (when (and card (agent-repl--open-fence-card-terminal-p card))
            (cl-incf seen)
            (agent-repl--open-fence-mark ws (plist-get card :message)))))
      seen)))

(provide 'agent-repl-open-fence)
;;; open-fence.el ends here
