;;; prompt-queue.el --- the user's deferral, held by the daemon -*- lexical-binding: t; -*-

;;; Commentary:

;; ONE ROLE: THE USER'S DEFERRAL.  `agent-repl-queue-deferred-prompt'
;; (`SPC j RET') captures what the user wrote and asks for it to run as its
;; OWN turn after the current one, never interjected into it.
;;
;; EMACS HOLDS NOTHING.  Owner ruling, 2026-09-28: held prompts survive
;; outages AND restarts, deferred ones included.  So the deferral is
;; submitted at once, through the composer's ONE submit path, with
;; `SubmitPromptRequest.delivery = SUBMIT_PROMPT_DELIVERY_DEFERRED'; the
;; DAEMON holds it (it shows in the held tray), never classifies it, and
;; delivers it as a turn of its own when the running turn ends -- or at
;; once when nothing runs.  There is no in-memory queue, no finish-edge
;; release and no liveness gate here any more.
;;
;; A DEFERRAL THE DAEMON DID NOT TAKE IS STILL HELD, on disk: a transport
;; failure and a handover refusal write it to the durable held-prompt
;; ingress (`held-ingress.el') like any other prompt, and a merge in flight,
;; a cold gate or a session still coming up -- which refuse an ordinary
;; prompt -- hold a deferred one there too
;; (`agent-repl--input-deferred-held-arms').  Every entry carries its
;; deferred delivery, so the daemon re-drives it deferred.
;;
;; A DEFERRED PROMPT CARRIES `:deferred-prompt', whatever origin it was
;; composed under: this command is that origin's ONE production send site.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--info "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--ws-current-name "agent-repl-workspace" ())
(declare-function agent-repl--history-push "agent-repl-history" (&optional text))
(declare-function agent-repl--history-reset "agent-repl-history" ())
(declare-function agent-repl--history-save "agent-repl-history" (ws))
(declare-function agent-repl--input-buffer "agent-repl-input" (ws))
(declare-function agent-repl--read-input-buffer "agent-repl-input" (ws))
(declare-function agent-repl--prepare-input "agent-repl-input" (ws raw &optional force))
(declare-function agent-repl--input-said "agent-repl-input" (text attachments))
(declare-function agent-repl--input-submit "agent-repl-input"
                  (ws said origin raw &optional key from-buffer snapshot delivery))
(declare-function agent-repl-input-attachments "agent-repl-input" (ws))
(declare-function agent-repl-input-clear-attachments "agent-repl-input" (ws))

(defconst agent-repl--prompt-queue-drain-origin :deferred-prompt
  "The `PromptOrigin' every deferred prompt carries.
`agent-repl-queue-deferred-prompt' is that value's ONE production send
site: a turn stored under it says the prompt was deferred and delivered
later, which is a different editor situation from the one it was typed in.")

(defconst agent-repl--prompt-queue-delivery :deferred
  "The `SubmitPromptDelivery' every deferred prompt asks for.
Run as its own turn after the current one, never interjected.")

(defun agent-repl-queue-deferred-prompt ()
  "Defer the composer's contents until the agent finishes its current turn.

The vendor UI already buffers keystrokes typed mid-turn, but those
interleave with whatever else is typed before the turn ends -- there is no
guarantee the text fires as its own discrete prompt.  This command gives
that guarantee: the text is captured, the composer is cleared (and the
text pushed onto the input history, so it is recallable), and the prompt
is submitted DEFERRED.  The daemon holds it in the held tray, never
interjects it, and runs it as its own turn once the running one ends --
at once when nothing runs.  Several deferred prompts run as that many
turns, in the order they were written.

The daemon holds it durably, so it survives a restart of Emacs, the
daemon and the shim; one the daemon could not take is held on disk
\(`held-ingress.el').  Returns the submission's idempotency key."
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
             (buf (agent-repl--input-buffer ws)))
        (when buf
          (with-current-buffer buf
            (agent-repl--history-push raw)
            (agent-repl--history-reset)
            (erase-buffer)))
        (agent-repl--history-save ws)
        (agent-repl-input-clear-attachments ws)
        (agent-repl--info ws "elisp.prompt-queue.deferred ws=%s origin=%S delivery=%S"
                          ws agent-repl--prompt-queue-drain-origin
                          agent-repl--prompt-queue-delivery)
        (prog1 (agent-repl--input-submit ws said agent-repl--prompt-queue-drain-origin raw
                                         nil nil nil agent-repl--prompt-queue-delivery)
          (message "agent-repl: deferred the prompt for %s (it runs as its own turn once the current one ends)"
                   ws))))))

(provide 'prompt-queue)

;;; prompt-queue.el ends here
