;;; rpc.el --- one function per agentrepl.v1 rpc Emacs calls -*- lexical-binding: t; -*-

;;; Commentary:

;; The seam between the codec (`wire-*.el', which turns elisp plists into
;; protojson alists and back) and the transport (`connect.el', which moves
;; JSON strings over Connect).  There is exactly ONE function here per rpc
;; Emacs calls, and it does exactly three things: encode the request,
;; hand it to the transport, decode the answer.  No policy, no retries, no
;; state — those belong to `daemon-link.el', `host.el', `roster.el' and
;; `verbs.el', which call this file.
;;
;; SHAPE OF EVERY UNARY VERB.  `(agent-repl-rpc-<method-kebab> CONN REQUEST
;; &key on-response on-failure)' plus a `-sync' variant for tests and the
;; doctor.  REQUEST is the decoded request plist; ON-RESPONSE receives the
;; DECODED response plist, which for every verb in this contract is the
;; response oneof `(:arm :success :value ...)' / `(:arm :error :value
;; ...)'.  ON-FAILURE receives a transport failure plist from connect.el —
;; a TRANSPORT failure and a daemon-authored ERROR ARM are different facts
;; and never collapse into one another.
;;
;; SHAPE OF EVERY STREAM.  `(agent-repl-rpc-watch-... CONN [REF] ON-PUSH
;; ON-CLOSE)'.  ON-PUSH receives the decoded push plist.  A push that fails
;; to decode (`agent-repl-wire-error') is logged at ERROR with the raw JSON
;; in the context and DROPPED; the stream continues, because one malformed
;; push must not cost the subscription.  ON-CLOSE is connect.el's
;; vocabulary verbatim: `(:cancelled)', `(:ended)', `(:error DETAIL)'.
;;
;; REFS ARE ECHO TOKENS.  A `WorkspaceRef' plist `(:id ... :dir ...)' is
;; obtained from RegisterWorkspace's success or from the roster and echoed
;; verbatim; nothing here ever builds one from a path.

;;; Code:

(require 'cl-lib)

(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--warn "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))

(declare-function agent-repl-connect-unary "connect"
                  (conn method json-string &rest keys))
(declare-function agent-repl-connect-unary-sync "connect"
                  (conn method json-string &optional timeout))
(declare-function agent-repl-connect-stream "connect"
                  (conn method json-string on-push on-close))

;; The codec.  Defined in wire-*.el; named, never implemented, here.
(declare-function agent-repl-wire-encode-register-workspace-request "wire-host" (request))
(declare-function agent-repl-wire-decode-register-workspace-response "wire-host" (alist))
(declare-function agent-repl-wire-encode-select-workspace-request "wire-host" (request))
(declare-function agent-repl-wire-decode-select-workspace-response "wire-host" (alist))
(declare-function agent-repl-wire-encode-adopt-host-workspace-request "wire-host" (request))
(declare-function agent-repl-wire-decode-adopt-host-workspace-response "wire-host" (alist))
(declare-function agent-repl-wire-encode-watch-host-workspace-request "wire-host" (request))
(declare-function agent-repl-wire-decode-watch-host-workspace-response "wire-host" (alist))
(declare-function agent-repl-wire-encode-watch-daemon-request "wire-host" (request))
(declare-function agent-repl-wire-decode-watch-daemon-response "wire-host" (alist))
(declare-function agent-repl-wire-encode-watch-workspace-roster-request "wire-roster" (request))
(declare-function agent-repl-wire-decode-watch-workspace-roster-response "wire-roster" (alist))
(declare-function agent-repl-wire-encode-create-workspace-request "wire-verbs" (request))
(declare-function agent-repl-wire-decode-create-workspace-response "wire-verbs" (alist))
(declare-function agent-repl-wire-encode-open-workspace-request "wire-verbs" (request))
(declare-function agent-repl-wire-decode-open-workspace-response "wire-verbs" (alist))
(declare-function agent-repl-wire-encode-close-workspace-request "wire-verbs" (request))
(declare-function agent-repl-wire-decode-close-workspace-response "wire-verbs" (alist))
(declare-function agent-repl-wire-encode-kill-workspace-request "wire-verbs" (request))
(declare-function agent-repl-wire-decode-kill-workspace-response "wire-verbs" (alist))
(declare-function agent-repl-wire-encode-nuke-workspace-request "wire-verbs" (request))
(declare-function agent-repl-wire-decode-nuke-workspace-response "wire-verbs" (alist))
(declare-function agent-repl-wire-encode-merge-workspace-request "wire-verbs" (request))
(declare-function agent-repl-wire-decode-merge-workspace-response "wire-verbs" (alist))
(declare-function agent-repl-wire-encode-restart-workspace-request "wire-verbs" (request))
(declare-function agent-repl-wire-decode-restart-workspace-response "wire-verbs" (alist))
(declare-function agent-repl-wire-encode-set-workspace-priority-request "wire-verbs" (request))
(declare-function agent-repl-wire-decode-set-workspace-priority-response "wire-verbs" (alist))
(declare-function agent-repl-wire-encode-submit-prompt-request "wire-verbs" (request))
(declare-function agent-repl-wire-decode-submit-prompt-response "wire-verbs" (alist))
(declare-function agent-repl-wire-encode-update-shutdown-schedule-request "wire-verbs" (request))
(declare-function agent-repl-wire-decode-update-shutdown-schedule-response "wire-verbs" (alist))
(declare-function agent-repl-wire-encode-update-merge-queue-request "wire-verbs" (request))
(declare-function agent-repl-wire-decode-update-merge-queue-response "wire-verbs" (alist))
(declare-function agent-repl-wire-encode-daemon-health-request "wire-verbs" (request))
(declare-function agent-repl-wire-decode-daemon-health-response "wire-verbs" (alist))
(declare-function agent-repl-wire-encode-session-health-request "wire-verbs" (request))
(declare-function agent-repl-wire-decode-session-health-response "wire-verbs" (alist))

;;;; ---- The two generic paths ----

(defun agent-repl-rpc--serialize (encoder request)
  "Return the protojson text of REQUEST as produced by ENCODER.
ENCODER is the codec's base encode function for the request message; it
answers the alist that `json-serialize' turns into the wire body.  An
incomplete request never leaves this function — the encoder refuses it
first, which is the whole point of encoding before spawning anything."
  (json-serialize (funcall encoder request)))

(cl-defun agent-repl-rpc--unary (conn method encoder decoder request
                                      &key on-response on-failure timeout)
  "Send REQUEST to METHOD on CONN and decode the answer.
ENCODER and DECODER are the codec's base functions for METHOD's request
and response messages.  ON-RESPONSE receives the decoded response plist;
ON-FAILURE receives a transport failure plist.  A response that arrives
but will not DECODE is a contract breach, not a transport failure: it is
logged at ERROR and reported through ON-FAILURE so the caller is never
left waiting on a callback that will not come."
  (let ((json (agent-repl-rpc--serialize encoder request)))
    (agent-repl--log nil "elisp.rpc.send method=%S" method)
    (agent-repl-connect-unary
     conn method json
     :timeout timeout
     :on-response
     (lambda (alist)
       (condition-case err
           (let ((decoded (funcall decoder alist)))
             (agent-repl--log nil "elisp.rpc.answer method=%S arm=%S"
                              method (plist-get decoded :arm))
             (when on-response (funcall on-response decoded)))
         (error
          (agent-repl--error nil "elisp.rpc.response-invalid method=%S error=%S body=%S"
                             method err alist)
          (when on-failure
            (funcall on-failure
                     (list :kind :malformed :code nil :status 200
                           :message (format "%s: undecodable response (%S)" method err)))))))
     :on-failure
     (lambda (detail)
       (agent-repl--warn nil "elisp.rpc.transport-failure method=%S detail=%S" method detail)
       (when on-failure (funcall on-failure detail))))))

(defun agent-repl-rpc--unary-sync (conn method encoder decoder request &optional timeout)
  "Blocking form of `agent-repl-rpc--unary'; returns the decoded response.
Signals `agent-repl-connect-error' on a transport failure and whatever the
codec signals on an undecodable response — a synchronous caller wants the
failure in its own stack, not in a callback."
  (let ((json (agent-repl-rpc--serialize encoder request)))
    (agent-repl--log nil "elisp.rpc.send-sync method=%S" method)
    (let ((decoded (funcall decoder (agent-repl-connect-unary-sync conn method json timeout))))
      (agent-repl--log nil "elisp.rpc.answer-sync method=%S arm=%S"
                       method (plist-get decoded :arm))
      decoded)))

(defun agent-repl-rpc--stream (conn method encoder decoder request on-push on-close)
  "Open server-streaming METHOD on CONN and decode each push.
A push that fails to decode is logged at ERROR (`elisp.rpc.push-invalid')
with the raw JSON in the context and DROPPED — the subscription survives,
because the daemon is the authority and the next push may well be sound.
An ON-PUSH that itself signals is contained by connect.el at the filter
boundary, which likewise keeps the stream open."
  (let ((json (agent-repl-rpc--serialize encoder request)))
    (agent-repl--info nil "elisp.rpc.stream-open method=%S" method)
    (agent-repl-connect-stream
     conn method json
     (lambda (alist)
       (condition-case err
           (funcall on-push (funcall decoder alist))
         (error
          (agent-repl--error nil "elisp.rpc.push-invalid method=%S error=%S body=%S"
                             method err alist))))
     (lambda (outcome)
       (agent-repl--info nil "elisp.rpc.stream-close method=%S outcome=%S" method (car outcome))
       (when on-close (funcall on-close outcome))))))

;;;; ---- Unary verbs ----
;;
;; One pair (async, sync) per rpc.  The bodies are deliberately uniform:
;; the only per-rpc facts are the method name and the two codec functions,
;; and keeping them uniform is what makes a wrong pairing visible.

(defmacro agent-repl-rpc--defverb (name method encoder decoder docstring)
  "Define the async and `-sync' functions for rpc METHOD as NAME.
ENCODER and DECODER name the codec's base functions.  DOCSTRING describes
the rpc; the two generated functions extend it with their own contract."
  (declare (indent 1) (doc-string 5))
  (let ((sync-name (intern (concat (symbol-name name) "-sync"))))
    `(progn
       (cl-defun ,name (conn request &key on-response on-failure timeout)
         ,(concat docstring "

CONN is the connection, REQUEST the decoded request plist.  ON-RESPONSE
receives the decoded response plist (`:arm' `:success' or `:error');
ON-FAILURE receives a transport failure plist.  TIMEOUT overrides
`agent-repl-connect-unary-timeout-seconds'.")
         (agent-repl-rpc--unary conn ,method #',encoder #',decoder request
                                :on-response on-response
                                :on-failure on-failure
                                :timeout timeout))
       (defun ,sync-name (conn request &optional timeout)
         ,(concat docstring "

Blocking form: returns the decoded response plist, or signals
`agent-repl-connect-error' on a transport failure.")
         (agent-repl-rpc--unary-sync conn ,method #',encoder #',decoder request timeout)))))

(agent-repl-rpc--defverb agent-repl-rpc-register-workspace
  "RegisterWorkspace"
  agent-repl-wire-encode-register-workspace-request
  agent-repl-wire-decode-register-workspace-response
  "Hand the daemon a directory and receive the workspace identity it mints.
Idempotent by dir: re-registering after a reconnect or a daemon restart
reconciles to the same ref and answers one success.")

(agent-repl-rpc--defverb agent-repl-rpc-select-workspace
  "SelectWorkspace"
  agent-repl-wire-encode-select-workspace-request
  agent-repl-wire-decode-select-workspace-response
  "Tell the daemon which workspace the user switched to.
The daemon stamps `current', records last-selected and clears the
workspace's attention marker; the roster stream reflects it.  Idempotent —
re-selecting the current workspace succeeds.")

(agent-repl-rpc--defverb agent-repl-rpc-adopt-host-workspace
  "AdoptHostWorkspace"
  agent-repl-wire-encode-adopt-host-workspace-request
  agent-repl-wire-decode-adopt-host-workspace-response
  "Claim a released workspace on a SUCCESSOR daemon during a handover.
Called on the NEW connection after the old daemon's host stream pushed
`transferred'; the old stream is cancelled only once this succeeds.")

(agent-repl-rpc--defverb agent-repl-rpc-create-workspace
  "CreateWorkspace"
  agent-repl-wire-encode-create-workspace-request
  agent-repl-wire-decode-create-workspace-response
  "Ask the daemon to create a workspace.
Emacs holds no creation machinery: the request carries the facts and the
resulting workspace arrives through the roster stream, not through this
answer.")

(agent-repl-rpc--defverb agent-repl-rpc-open-workspace
  "OpenWorkspace"
  agent-repl-wire-encode-open-workspace-request
  agent-repl-wire-decode-open-workspace-response
  "Re-open a closed workspace, named by the ref of its closed roster row.")

(agent-repl-rpc--defverb agent-repl-rpc-close-workspace
  "CloseWorkspace"
  agent-repl-wire-encode-close-workspace-request
  agent-repl-wire-decode-close-workspace-response
  "Close a workspace, leaving its worktree in place.
The error oneof carries a `blocked' arm; the caller treats it as an
answer, not as a failure.")

(agent-repl-rpc--defverb agent-repl-rpc-kill-workspace
  "KillWorkspace"
  agent-repl-wire-encode-kill-workspace-request
  agent-repl-wire-decode-kill-workspace-response
  "Kill a workspace's session without destroying its data.")

(agent-repl-rpc--defverb agent-repl-rpc-nuke-workspace
  "NukeWorkspace"
  agent-repl-wire-encode-nuke-workspace-request
  agent-repl-wire-decode-nuke-workspace-response
  "Destroy a workspace and its worktree.  A nuked row leaves the roster.")

(agent-repl-rpc--defverb agent-repl-rpc-merge-workspace
  "MergeWorkspace"
  agent-repl-wire-encode-merge-workspace-request
  agent-repl-wire-decode-merge-workspace-response
  "Enqueue a workspace for merge.  Emacs holds no merge state whatsoever.")

(agent-repl-rpc--defverb agent-repl-rpc-restart-workspace
  "RestartWorkspace"
  agent-repl-wire-encode-restart-workspace-request
  agent-repl-wire-decode-restart-workspace-response
  "Restart a workspace's session, optionally forcing through a live turn.")

(agent-repl-rpc--defverb agent-repl-rpc-set-workspace-priority
  "SetWorkspacePriority"
  agent-repl-wire-encode-set-workspace-priority-request
  agent-repl-wire-decode-set-workspace-priority-response
  "Set or clear a workspace's priority.
Clearing is the ABSENCE of the priority field, never a sentinel value.")

(agent-repl-rpc--defverb agent-repl-rpc-submit-prompt
  "SubmitPrompt"
  agent-repl-wire-encode-submit-prompt-request
  agent-repl-wire-decode-submit-prompt-response
  "Submit the composer's prompt to a workspace's session.
The request carries the `UserSaid' content, an idempotency key and the
REQUIRED origin naming the send site.  There is no precondition: the
daemon starts or revives the session implicitly and answers with its own
refusal arms when it will not run the prompt.")

(agent-repl-rpc--defverb agent-repl-rpc-update-shutdown-schedule
  "UpdateShutdownSchedule"
  agent-repl-wire-encode-update-shutdown-schedule-request
  agent-repl-wire-decode-update-shutdown-schedule-response
  "Schedule, cancel or immediately trigger the daemon's shutdown.
This is how Emacs stops a daemon; Emacs never kills a daemon that answers.")

(agent-repl-rpc--defverb agent-repl-rpc-update-merge-queue
  "UpdateMergeQueue"
  agent-repl-wire-encode-update-merge-queue-request
  agent-repl-wire-decode-update-merge-queue-response
  "Pause, resume or evict an entry from the daemon's merge queue.")

(agent-repl-rpc--defverb agent-repl-rpc-daemon-health
  "DaemonHealth"
  agent-repl-wire-encode-daemon-health-request
  agent-repl-wire-decode-daemon-health-response
  "Ask the daemon for its own health verdict and standing faults.
ANY answer — healthy or unhealthy — proves a daemon is there; only a
transport failure means there is not one.")

(agent-repl-rpc--defverb agent-repl-rpc-session-health
  "SessionHealth"
  agent-repl-wire-encode-session-health-request
  agent-repl-wire-decode-session-health-response
  "Ask the daemon for one workspace session's health verdict and faults.")

;;;; ---- Streams ----

(defun agent-repl-rpc-watch-host-workspace (conn ref on-push on-close)
  "Subscribe to the host view of the workspace named by REF on CONN.
REF is the decoded `WorkspaceRef' plist `(:id ... :dir ...)', echoed
verbatim from registration or the roster and never built from a path.  The
daemon sends a snapshot first, then whole-replaces per push.  ON-PUSH
receives the decoded push plist (the `host' / `notification' /
`transferred' / `reload_webapp' / `open_in_editor' oneof).  Cancelling the
returned stream IS the graceful unsubscribe."
  (agent-repl-rpc--stream
   conn "WatchHostWorkspace"
   #'agent-repl-wire-encode-watch-host-workspace-request
   #'agent-repl-wire-decode-watch-host-workspace-response
   (list :workspace ref)
   on-push on-close))

(defun agent-repl-rpc-watch-daemon (conn on-push on-close)
  "Subscribe to CONN's daemon-scoped host stream.
Carries the graceful-shutdown announcement and the drain schedule, and
nothing workspace-scoped.  The request message is empty.  ON-PUSH receives
the decoded push plist."
  (agent-repl-rpc--stream
   conn "WatchDaemon"
   #'agent-repl-wire-encode-watch-daemon-request
   #'agent-repl-wire-decode-watch-daemon-response
   nil
   on-push on-close))

(defun agent-repl-rpc-watch-workspace-roster (conn on-push on-close)
  "Subscribe to the whole workspace roster on CONN.
The roster is the one source of Emacs's tabs and their paint; the request
message is empty and each push is the whole view.  ON-PUSH receives the
decoded roster push plist."
  (agent-repl-rpc--stream
   conn "WatchWorkspaceRoster"
   #'agent-repl-wire-encode-watch-workspace-roster-request
   #'agent-repl-wire-decode-watch-workspace-roster-response
   nil
   on-push on-close))

(provide 'rpc)

;;; rpc.el ends here
