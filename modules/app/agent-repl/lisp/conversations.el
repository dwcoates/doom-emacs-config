;;; conversations.el --- Choose which vendor conversation a workspace runs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A WORKSPACE IS BOUND TO ONE CONVERSATION, and until this command the binding
;; was whatever the workspace happened to start with.  A directory can hold
;; many: every `/clear' files a new one, and a CLI run outside agent-repl files
;; its own.  `ListWorkspaceTranscripts' asks the workspace's shim what is filed
;; under its own directory, and `BindWorkspaceSession' points the workspace at
;; the one the user picked.
;;
;; THE COMPLETION LINE READS AS A CONVERSATION, NOT AN ID.  A person does not
;; recognize a uuid; they recognize what they asked.  So each row leads with the
;; conversation's OPENING WORDS and follows them with the three facts that
;; separate one conversation from another at a glance -- how long ago it was
;; used, how big it is, and how many times the person spoke -- plus a plain
;; marker for the three states that change what picking it would mean:
;;
;;   - `current'  -- the workspace already runs it, and the daemon refuses it;
;;   - `held by X' -- another workspace is bound to it, and the daemon refuses
;;                    it, because two workspaces on one conversation is data
;;                    loss;
;;   - `in use'   -- something is writing to its transcript right now, which is
;;                   the same data loss by a different route.
;;
;; The markers are shown rather than the rows hidden: a person looking for the
;; conversation they just left in a terminal needs to SEE it and be told why it
;; is not available, not find it missing.
;;
;; THE BIND IS A SESSION SWAP AND IT IS REPORTED AS ONE.  The daemon ends the
;; session and brings one up on the chosen conversation through the ordinary
;; resume, so the wait is a session bring-up's wait; the command mints an op id
;; and reports the daemon's stages through the same minibuffer channel an open
;; reports through (`mutation-progress.el').

;;; Code:

(require 'cl-lib)

;; Cross-file forward declarations.  These sources load in the dependency order
;; config.el establishes and resolve each other's calls at call time, so the
;; declarations below exist for the byte-compiler alone.
(declare-function agent-repl--info "core")
(declare-function agent-repl--warn "core")
(declare-function agent-repl--ws-current-name "workspace")
(declare-function agent-repl-verbs--ref "verbs" (ws))
(declare-function agent-repl-verbs--conn "verbs" (&optional ws))
(declare-function agent-repl-verbs--send-op "verbs" (op-id rpc conn request &rest keys))
(declare-function agent-repl-verbs--send "verbs" (rpc conn request &rest keys))
(declare-function agent-repl-rpc-list-workspace-transcripts-sync "rpc" (conn request &optional timeout))
(declare-function agent-repl-rpc-bind-workspace-session "rpc" (conn request &rest keys))
(declare-function agent-repl-mutation-progress-new-op-id "mutation-progress" ())
(declare-function agent-repl-mutation-progress-register "mutation-progress" (op-id &rest callbacks))
(declare-function agent-repl-mutation-progress-forget "mutation-progress" (op-id))
(declare-function agent-repl-workspace-progress-report "mutation-progress" (kind phase &rest details))

;;;; ---- Rendering one conversation --------------------------------------

(defconst agent-repl-conversations--opening-width 48
  "How much of a conversation's opening words the completion line shows.

The shim already caps the opening it sends; this is the COLUMN, so the
facts that follow line up down the list and a long opening cannot push
them off the screen.  A shorter opening is padded rather than left ragged
-- the point of a column is that the eye can run down it.")

(defun agent-repl-conversations--age (at-ms now-ms)
  "Render the span between AT-MS and NOW-MS as a short age.
AT-MS is nil for a conversation whose transcript states no request at
all, which is `never' rather than a zero: a conversation that never
reached the model is not one from 1970."
  (if (null at-ms)
      "never"
    (let ((seconds (max 0 (/ (- now-ms at-ms) 1000))))
      (cond
       ((< seconds 60) (format "%ds ago" seconds))
       ((< seconds 3600) (format "%dm ago" (/ seconds 60)))
       ((< seconds 86400) (format "%dh ago" (/ seconds 3600)))
       (t (format "%dd ago" (/ seconds 86400)))))))

(defun agent-repl-conversations--size (transcript)
  "Render TRANSCRIPT's size: what resuming it would re-read.

A CLEARED conversation resumes EMPTY however large its last request was,
so the clear is what the line states -- showing the old figure beside a
conversation that will not re-read it is worse than showing none.  A
transcript that stated no usage at all is `unread', which is a different
fact from `0 tokens' and must not be spelled as one."
  (cond
   ((plist-get transcript :cleared) "cleared")
   ((null (plist-get transcript :context-tokens)) "unread")
   (t (let ((tokens (plist-get transcript :context-tokens)))
        (if (< tokens 1000)
            (format "%d ctx" tokens)
          (format "%dk ctx" (/ tokens 1000)))))))

(defun agent-repl-conversations--marker (transcript)
  "Render TRANSCRIPT's availability marker, or nil when it is plainly available.

The three markers are the three states that change what picking the
conversation would MEAN, and each is the plain word for it rather than
the arm's name: a person reads a list, not a contract."
  (cond
   ((plist-get transcript :current) "current")
   ((plist-get transcript :held)
    (let ((ref (plist-get (plist-get transcript :held) :workspace)))
      (format "held by %s" (or (plist-get ref :dir) (plist-get ref :id) "another workspace"))))
   ((plist-get transcript :active) "in use")))

(defun agent-repl-conversations--line (transcript now-ms)
  "Render TRANSCRIPT as one completion line, read at NOW-MS.

THE OPENING LEADS, because it is the only part a person recognizes.  A
conversation whose transcript holds no user prompt has no opening to
show, and says so rather than showing an id: the id is the completion's
VALUE, not its label."
  (let* ((opening (or (plist-get transcript :opening) "(no prompt yet)"))
         (prompts (or (plist-get transcript :prompts) 0))
         (marker (agent-repl-conversations--marker transcript)))
    (concat
     (truncate-string-to-width opening agent-repl-conversations--opening-width 0 ?\s)
     "  "
     (format "%3d prompts  %-10s %-9s"
             prompts
             (agent-repl-conversations--size transcript)
             (agent-repl-conversations--age (plist-get transcript :last-request-at-ms) now-ms))
     (if marker (concat " [" marker "]") ""))))

(defun agent-repl-conversations--candidates (transcripts now-ms)
  "Render TRANSCRIPTS as a completion alist of (LINE . VENDOR-SESSION-ID).

THE LABEL IS NEVER THE VALUE.  `completing-read' answers the label, and
the daemon wants the id it served; the alist is what keeps a person
reading conversations while the rpc echoes an id.

A DUPLICATE LINE IS DISAMBIGUATED BY ITS ID rather than silently
collapsed: two conversations opened with the same words are ordinary, and
an alist that lost one would make it unreachable."
  (let ((seen (make-hash-table :test 'equal))
        (out nil))
    (dolist (transcript transcripts (nreverse out))
      (let* ((id (plist-get transcript :vendor-session-id))
             (line (agent-repl-conversations--line transcript now-ms))
             (label (if (gethash line seen)
                        (format "%s  %s" line (substring id 0 (min 8 (length id))))
                      line)))
        (puthash line t seen)
        (push (cons label id) out)))))

;;;; ---- The command -----------------------------------------------------

(defun agent-repl-conversations--list (ws)
  "Return the conversations filed under WS's directory, or signal.

THE LIST IS FETCHED SYNCHRONOUSLY because the user is standing at a
prompt waiting for it: there is nothing to do with a completion whose
candidates arrive later.  A refusal is raised as a `user-error' naming
the arm, which is what the generic verb dispatcher would have printed had
this been an asynchronous send."
  (let* ((response (agent-repl-rpc-list-workspace-transcripts-sync
                    (agent-repl-verbs--conn ws)
                    (list :workspace (agent-repl-verbs--ref ws)))))
    (pcase (plist-get response :arm)
      (:success (plist-get (plist-get response :value) :transcripts))
      (:error
       (let* ((arm (plist-get (plist-get response :value) :cause))
              (keyword (plist-get arm :arm)))
         (agent-repl--warn ws "elisp.conversations.list-refused ws=%s arm=%S fields=%S"
                           ws keyword (plist-get arm :value))
         (user-error "agent-repl: listing conversations refused: %s"
                     (if keyword (substring (symbol-name keyword) 1) "unstated"))))
      (arm
       (agent-repl--warn ws "elisp.conversations.unknown-response-arm ws=%s arm=%S" ws arm)
       (user-error "agent-repl: the daemon answered an unknown shape")))))

(defconst agent-repl-conversations-bind-timeout-seconds 120
  "How long a bind may take before the editor calls it a transport failure.

A BIND IS A SESSION BRING-UP, and the default unary deadline is sized for
a verb that answers from the daemon\='s own state.  At the default this
reported a failure ten seconds into a bring-up that went on to succeed,
leaving the user with an error over a workspace that bound correctly.")

(defun agent-repl-conversations--refusal-sentence (value)
  "Say in PLAIN WORDS why the daemon refused a bind, from VALUE.

THE ARMS A PERSON CAN ACT ON ARE WORDED; everything else falls back to the
arm\='s own keyword, which is what the generic dispatcher would have shown.
An arm added to the proto and not to this table therefore still reads as
itself rather than as nothing."
  (let* ((arm (plist-get (plist-get value :cause) :arm))
         (fields (plist-get (plist-get value :cause) :value)))
    (pcase arm
      (:transcript-active
       "something is writing to that conversation right now — it is live somewhere else")
      (:transcript-held
       (format "another workspace is on that conversation: %s"
               (or (plist-get (plist-get fields :workspace) :dir) "unnamed")))
      (:already-bound "this workspace is already on that conversation")
      (:turn-in-flight "a turn is in flight; end it or interrupt it first")
      (:unknown-transcript "that conversation is no longer on disk")
      (:start-failed
       (format "the session would not come up: %s" (or (plist-get fields :detail) "unstated")))
      (:stop-failed
       (format "the current session would not end: %s" (or (plist-get fields :detail) "unstated")))
      (_ (if arm (substring (symbol-name arm) 1) "unstated")))))

(defun agent-repl-conversations--bind (ws vendor-session-id)
  "Bind WS to VENDOR-SESSION-ID, reporting the daemon's stages as it goes.

THE OP IS FORGOTTEN ON EVERY TERMINAL PATH, exactly as an open's is: the
bind's outcome arrives on ITS OWN rpc rather than on the progress
channel, so nothing on the stream would ever retire the registration."
  (let ((op-id (agent-repl-mutation-progress-new-op-id))
        (ref (agent-repl-verbs--ref ws)))
    ;; Registered BEFORE the send, so a stage push cannot outrun its handler.
    (agent-repl-mutation-progress-register
     op-id
     :on-stage (lambda (stage) (agent-repl-workspace-progress-report :bind stage)))
    ;; WS IS ALREADY A STRING.  Workspace names are strings everywhere in this
    ;; package -- `agent-repl--ws-current-name' answers one -- so naming the
    ;; workspace through `symbol-name' signalled `wrong-type-argument symbolp'
    ;; on the very first stage, before the rpc was ever sent.
    (agent-repl-workspace-progress-report :bind :requested ws)
    (agent-repl-verbs--send-op
     op-id #'agent-repl-rpc-bind-workspace-session (agent-repl-verbs--conn ws)
     (list :workspace ref :vendor-session-id vendor-session-id :op-id op-id)
     :ws ws :op "bind-conversation"
     :timeout agent-repl-conversations-bind-timeout-seconds
     :on-success
     (lambda (_)
       (agent-repl-workspace-progress-report :bind :completed ws))
     ;; THE ARM IS CLAIMED, because a bind's refusals are the ones a person
     ;; standing at the chooser has to act on -- and the dispatcher's generic
     ;; wording ("transcript-active at-ms=1790003430717") names an epoch
     ;; instant at somebody who asked for a conversation by its opening words.
     ;; Claiming it also RETIRES THE PROGRESS LINE: unclaimed, the last thing
     ;; the minibuffer said was that the bind had started, and the refusal
     ;; scrolled past under it.
     :on-error
     (lambda (value)
       (agent-repl-workspace-progress-report
        :bind :failed ws (agent-repl-conversations--refusal-sentence value))
       t))))

;;;###autoload
(defun agent-repl-bind-conversation ()
  "Point this workspace at one of the conversations filed under its directory.

Lists what the workspace's shim finds on disk, completes over them as
CONVERSATIONS rather than ids, and binds the one picked.  The bind is a
SESSION SWAP: the daemon ends the current session and brings one up on
the chosen conversation through the ordinary resume, so a cold
conversation comes up parked at its cold gate and is never paid for
silently."
  (interactive)
  (let ((ws (agent-repl--ws-current-name)))
    (unless ws
      (user-error "agent-repl: no current workspace"))
    (let* ((transcripts (agent-repl-conversations--list ws))
           (now-ms (* 1000 (time-convert nil 'integer)))
           (candidates (agent-repl-conversations--candidates transcripts now-ms)))
      (unless candidates
        (user-error "agent-repl: no conversations are filed under this workspace's directory"))
      (agent-repl--info ws "elisp.conversations.listed ws=%s count=%d" ws (length candidates))
      (let* ((label (completing-read "Bind workspace to conversation: "
                                     (mapcar #'car candidates) nil t))
             (id (cdr (assoc label candidates))))
        (unless id
          (user-error "agent-repl: that is not one of the conversations offered"))
        (agent-repl-conversations--bind ws id)))))

(provide 'conversations)

;;; conversations.el ends here
