;;; frontend-client.el --- session client for the claude-repld daemon -*- lexical-binding: t; -*-

;;; Commentary:

;; The Emacs-side seam onto claude-repld's SESSION plane.  Emacs speaks no
;; HTTP to the daemon at all any more (S8/S9 sentinel endgame): session
;; CRUD travels as `frontend.v1' commands over the UDS (frontend-uds.el)
;; and every read comes off pushed frames (frontend-state.el).  What is
;; left here is the command/await choreography and the workspace ⇄ session
;; binding, so the panel layer (frontend.el) and any future consumer share
;; one client instead of hand-rolling command round-trips.
;;
;; The one URL this module still builds is the WEBVIEW's
;; (`agent-repl--frontend-session-url'): the webapp bundle is served over
;; HTTP to an embedded browser, which is a browser navigation, not an
;; Emacs-side HTTP client call.
;;
;; Binding model:
;;   - Each workspace gets AT MOST one daemon session, tracked under the
;;     `:frontend-session-id' plist key.
;;   - The key is a RUNTIME key (cleared on tombstone) and is never
;;     persisted: daemon `s_<hex>' ids die with the daemon process, so
;;     resurrecting one from disk would always be stale.  The DURABLE id
;;     is `claude_session_id' (the CLI uuid), which is what a future
;;     persistence layer stores for `resume'.
;;   - `agent-repl-ws-del-hook' releases the daemon session when the
;;     workspace is nuked (best-effort: a dead daemon only logs).
;;
;; All external I/O funnels through the transport's single boundary wrapper
;; `agent-repl--uds-connect' (frontend-uds.el), registered in
;; `agent-repl--external-boundary-functions' per the test-harness
;; contract; tests mock it via `cl-letf'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--log-verbose "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--ws-current-name "agent-repl-workspace" ())
(declare-function agent-repl--ws-get "agent-repl-workspace" (ws key))
(declare-function agent-repl--ws-put "agent-repl-workspace" (ws key val))
(declare-function agent-repl--ws-name-for-dir "agent-repl-workspace" (dir))
(declare-function agent-repl--ensure-frontend-daemon "agent-repl-daemon" (&optional force))
(declare-function agent-repl--resolve-current-git-root "agent-repl-core" ())
(declare-function agent-repl--ws-durable-claude-session-id "agent-repl-core" (ws))
(declare-function agent-repl--initialize-ws-env "agent-repl-session" (ws &optional project-dir-hint active-env-hint))
(declare-function agent-repl--frontend-sync-webview "agent-repl-frontend" (ws session-id))
(declare-function agent-repl--frontend-snap-webview-to-tail "agent-repl-frontend" (ws))
(declare-function agent-repl--frontend-remount-all-webviews "agent-repl-frontend" ())
(declare-function agent-repl--frontend-init-inhibited-p "agent-repl-daemon" ())
(declare-function agent-repl--live-ws-names "agent-repl-workspace" ())
(declare-function agent-repl--mark-ws-thinking "input" (ws))
(declare-function agent-repl--dispatch-resume-investigation "agent-repl-worktree" (resume-id searched-paths cwd))
(declare-function agent-repl--assert-main-thread "agent-repl-worktree" (what))
;; The UDS command channel + pushed-frame SessionView store (the daemon plane
;; that replaced the GET /sessions poller); resolved at call time.
(declare-function agent-repl--uds-send-command "frontend-uds" (field payload &optional workspace process))
(declare-function agent-repl--uds-track-command "frontend-uds" (request-id field workspace &optional on-failure on-success))
(declare-function agent-repl--uds-untrack-command "frontend-uds" (request-id workspace reason))
(declare-function agent-repl--uds-track-health-response
                  "frontend-uds"
                  (request-id field workspace session-id on-response))
(declare-function agent-repl--uds-untrack-health-response
                  "frontend-uds" (request-id workspace reason))
(declare-function agent-repl--uds-connected-p "frontend-uds" ())
(declare-function agent-repl-uds-connect "frontend-uds" (&optional path readiness-p))
(declare-function agent-repl--frontend-session-view "frontend-state" (session-id))
(declare-function agent-repl--frontend-session-views-all "frontend-state" ())
(declare-function agent-repl--frontend-workspace-state-views-all "frontend-state" ())
(declare-function agent-repl--frontend-live-session-id-for-cwd "frontend-state" (cwd))
(declare-function agent-repl--frontend-daemon-view "frontend-state" ())
(declare-function agent-repl--ws-dir "agent-repl-status" (ws))

(defvar agent-repl--uds-process)
(defvar agent-repl-uds-socket-path)

;;;; ---- The workspace wire key ------------------------------------------
;;
;; The daemon keys every workspace-routed command (`submitPrompt',
;; `interrupt', `permissionAnswer') by the session's CWD: its
;; `SessionLocator.Locate' scans the registry for a non-terminal record whose
;; `CWD' EQUALS the `workspace' field, and `sessiondrv.Manager' maps live
;; drivers under that same cwd string (daemon/internal/server/drivers.go,
;; daemon/internal/sessiondrv/driver.go).
;;
;; Emacs, meanwhile, keys everything by the persp NAME ("doom").  Sending the
;; name as the `workspace' field therefore matches NO record, and the daemon
;; NACKs with `workspace "doom" has no live session to drive' — which is
;; exactly what the UDS command cutover shipped, silently breaking every
;; prompt/interrupt/permission answer (2026-07-25 incident).  `createSession'
;; escaped it only because it already had the cwd in hand.
;;
;; So: every ws-keyed command resolves its wire key through here, and NOTHING
;; puts a bare workspace name on the wire.

(defun agent-repl--frontend-ws-command-key (ws)
  "Return the `workspace' wire key the daemon routes WS's commands by.
That key is WS's `:project-dir' — the same cwd string
`agent-repl--frontend-create-session' registered the session under, so the
daemon's cwd-keyed lookup resolves it.  NEVER the persp name WS itself.

Signals (via `agent-repl--ws-dir') when WS has no `:project-dir': a
command with no resolvable cwd cannot be routed, and a loud failure here
beats a daemon NACK that reads as \"no live session\"."
  (agent-repl--ws-dir ws))

;; Signalled when the daemon HARD-FAILS a --resume because the target
;; session has no transcript in its config dir (§2.10 resume viability
;; gate, `code: "resume_transcript_missing"').  NON-recoverable by
;; design: rather than degrade to a fresh conversation, the client opens
;; an investigation workspace for the lost session and re-raises this,
;; naming that workspace.  Derived from `error' so existing
;; `condition-case' handlers still catch it.
(define-error 'agent-repl-resume-transcript-missing
  "agent-repl: resume target has no transcript" 'error)

;; When non-nil, a resume whose transcript the daemon cannot find DEGRADES
;; to a fresh conversation instead of opening a diagnostic investigation
;; workspace.  The default (nil) behavior on the daemon's
;; `resume_transcript_missing' hard-fail is to REFUSE a silent fresh start:
;; the client opens an investigation workspace
;; (`agent-repl--dispatch-resume-investigation') and re-raises a
;; non-recoverable error.  Binding this non-nil OVERRIDES that — the create
;; logs the lost-workspace resume attempt and simply recreates the session
;; with no resume.  `agent-repl-force-fresh-conversation' (the command)
;; starts a fresh conversation directly and does not rely on this override.
(defvar agent-repl--force-fresh-conversation nil
  "When non-nil, a lost-transcript resume degrades to a fresh conversation.
Overrides the default `resume_transcript_missing' investigation dispatch
in `agent-repl--frontend-create-session'.")

;;;; ---- Customization ----------------------------------------------------

(defcustom agent-repl-frontend-permission-mode "auto"
  "Permission mode for gui-created sessions (`CreateSessionCmd').
Defaults to `auto' to match the CLI's own permission-mode config
(`agent-repl-personal-permission-flag' /
`agent-repl-managed-permission-flag'), which requires the daemon to
drive the SYSTEM claude binary (daemon.el's -claude-bin) — the
SDK-bundled CLI predates the mode.  Set nil to omit the field and use
the SDK default."
  :type '(choice (const :tag "SDK default" nil) string)
  :group 'agent-repl)

;; The daemon REFUSES to create a session in a permission mode that leaves it
;; with no permission gate — `bypassPermissions', under which the SDK
;; auto-approves every tool call BEFORE the shim's `canUseTool' callback is
;; consulted, so the permission round-trip (request -> card -> answer) never
;; engages and no permission card can ever appear.  The refusal is lifted only
;; by this flag, which becomes `CreateSessionCmd.allow_ungated'.
;;
;; It is a DEFVAR and not a defcustom on purpose: the consent belongs to the
;; one call site that genuinely wants an ungated session (it `let'-binds this
;; alongside `agent-repl-frontend-permission-mode'), not to a global preference
;; that would quietly re-arm every later create.
(defvar agent-repl-frontend-allow-ungated nil
  "When non-nil, consent to creating a session with NO permission gate.
Required by the daemon whenever `agent-repl-frontend-permission-mode' names
an ungated mode (`bypassPermissions'); without it the create is refused
loudly rather than downgraded to a gated mode.  `let'-bind it at the call
site that wants such a session; never set it globally.")

(defconst agent-repl-frontend-ungated-permission-modes '("bypassPermissions")
  "Permission modes under which a session has NO permission gate.
The elisp twin of the daemon's `protocol.UngatedPermissionMode' and the
webapp's `UNGATED_PERMISSION_MODES'; all three must name the same set.

`bypassPermissions' is the only member.  `dontAsk' also bypasses the SDK's
`canUseTool' callback, but bypasses it by DENYING (fail-closed), so it
grants nothing behind the gate's back, and `default'/`acceptEdits'/`auto'
all still reach the callback for the ask path.")

(defun agent-repl-frontend-ungated-permission-mode-p (mode)
  "Return non-nil when MODE names a session with no permission gate.
See `agent-repl-frontend-ungated-permission-modes'."
  (and (stringp mode)
       (member mode agent-repl-frontend-ungated-permission-modes)
       t))

(defun agent-repl--frontend-session-posture (cwd)
  "Return the explicit daemon-session account and permission posture for CWD.
The result is a plist with `:config-dir', `:permission-mode', and
`:allow-ungated'.  Both direct session creation and daemon-owned workspace
creation consume this helper so the two entry points cannot drift."
  (let ((config-dir (agent-repl--compute-config-dir cwd))
        (permission-mode agent-repl-frontend-permission-mode)
        (allow-ungated (and agent-repl-frontend-allow-ungated t)))
    (agent-repl--log
     (agent-repl--ws-name-for-dir cwd)
     "frontend-session-posture: cwd=%s config-dir=%s permission-mode=%s allow-ungated=%S"
     cwd (or config-dir "CLI-default") (or permission-mode "SDK-default")
     allow-ungated)
    (list :config-dir config-dir
          :permission-mode permission-mode
          :allow-ungated allow-ungated)))

(defcustom agent-repl-frontend-ready-attempts 25
  "Poll attempts for `agent-repl--frontend-wait-ready' (0.2s apart)."
  :type 'integer
  :group 'agent-repl)

;;;; ---- Webview URL --------------------------------------------------------

(defun agent-repl--frontend-base-url ()
  "Return the daemon's HTTP base URL from the configured address.
The ONLY surviving URL construction in the Emacs client: it addresses the
webapp bundle the daemon serves to the embedded browser
\(`agent-repl--frontend-session-url'), which is a browser navigation.
Emacs itself never issues an HTTP request to the daemon."
  (format "http://%s" agent-repl-frontend-daemon-addr))

;;;; ---- Readiness ---------------------------------------------------------

(defun agent-repl--frontend-daemon-ready-p ()
  "Return non-nil when the daemon is up AND has identified itself.
Readiness is two facts, both of which the UDS plane already carries:

  1. the frontend UDS link is LIVE (`agent-repl--uds-connected-p') — the
     daemon bound its socket and accepted us;
  2. a `DaemonView' has been pushed (`agent-repl--frontend-daemon-view') —
     the connect `StateSnapshot' arrived, so the daemon is past bring-up
     and its identity (boot id, versions, binary mtime) is known.

Fact 1 alone is not enough: a connection can be accepted a beat before the
snapshot is composed, and every consumer of readiness immediately issues a
command whose answer arrives as pushed state.  This is the replacement for
the deleted `GET /sessions' probe, which conflated both facts in one
round-trip."
  (and (agent-repl--uds-connected-p)
       (agent-repl--frontend-daemon-view)
       t))

(defun agent-repl--frontend-wait-ready ()
  "Block until the frontend UDS link is ready, or signal an error.
`agent-repl--ensure-frontend-daemon' returns as soon as the process is
SPAWNED, which precedes the socket bind; polling closes that gap.  Each
attempt dials when the link is down (`agent-repl-uds-connect' in its
readiness-owned mode, which loud-logs without raising a premature outage
alarm or arming a competing reconnect timer) and otherwise PUMPS the live
connection so the connect snapshot — which carries the `DaemonView'
readiness depends on — is dispatched while we wait.  Polls
`agent-repl-frontend-ready-attempts' times, 0.2s apart, then fails loudly
once if the daemon never becomes ready.

MAIN THREAD ONLY: `accept-process-output' routes through `ns_select_1' ->
`[NSApp run]', which deadlocks Emacs off the main thread (the AGENTS.md
worker-thread trap), so this asserts it up front exactly as the old
synchronous HTTP boundary did."
  (agent-repl--assert-main-thread "frontend-wait-ready")
  (let ((attempt 0)
        (ready (agent-repl--frontend-daemon-ready-p)))
    (agent-repl--log
     nil
     "frontend-wait-ready: begin ready=%s connected=%s daemon-view=%s budget=%d"
     ready (agent-repl--uds-connected-p)
     (and (agent-repl--frontend-daemon-view) t)
     agent-repl-frontend-ready-attempts)
    (while (and (not ready) (< attempt agent-repl-frontend-ready-attempts))
      (setq attempt (1+ attempt))
      (unless (agent-repl--uds-connected-p)
        ;; WHY: a freshly spawned daemon has not bound its socket yet.  The
        ;; wait loop owns this expected retry window; routing the dial through
        ;; ordinary outage handling produced the false startup alarms this
        ;; function exists to prevent.
        (agent-repl-uds-connect nil t))
      (if (agent-repl--uds-connected-p)
          (accept-process-output agent-repl--uds-process 0.2)
        ;; sleep-for, NOT sit-for: sit-for returns immediately when
        ;; input is pending, which would collapse the whole readiness
        ;; window into back-to-back failed dials while the user types.
        (sleep-for 0.2))
      (setq ready (agent-repl--frontend-daemon-ready-p)))
    (unless ready
      (agent-repl--log
       nil
       "frontend-wait-ready: TIMEOUT attempts=%d socket=%s connected=%s daemon-view=%s"
       attempt agent-repl-uds-socket-path
       (agent-repl--uds-connected-p)
       (and (agent-repl--frontend-daemon-view) t))
      (error "agent-repl: daemon at %s never became ready (%d attempts; connected=%s daemon-view=%s)"
             agent-repl-uds-socket-path attempt
             (if (agent-repl--uds-connected-p) "yes" "no")
             (if (agent-repl--frontend-daemon-view) "yes" "no")))
    (agent-repl--log
     nil
     "frontend-wait-ready: READY attempts=%d connected=%s daemon-view=%s"
     attempt (agent-repl--uds-connected-p)
     (and (agent-repl--frontend-daemon-view) t))
    t))

;;;; ---- Session CRUD -------------------------------------------------------

(defcustom agent-repl-frontend-create-timeout 10
  "Seconds `agent-repl--frontend-create-session' awaits its UDS outcome.
createSession's `CommandAck' is a bare receipt; the new session id arrives
on a pushed `SessionView'.  This bounds the synchronous await for both."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-frontend-health-timeout 10
  "Seconds to await a correlated daemon or session health result over UDS."
  :type 'number
  :group 'agent-repl)

(defvar agent-repl--frontend-creates-in-flight (make-hash-table :test 'equal)
  "Set of cwds with a `createSession' command awaiting its ack.
Serializes concurrent creates per workspace so the SessionView->id
correlation (`agent-repl--frontend-live-session-id-for-cwd') stays
unambiguous: a second create for the same cwd while one is in flight is
refused loudly rather than racing two views onto one key.")

(defun agent-repl--frontend-await-uds (predicate timeout what)
  "Block until PREDICATE returns non-nil or TIMEOUT seconds elapse; return its value.
Pumps the frontend UDS connection via `accept-process-output' so inbound
frames (the CommandAck, the SessionView) are dispatched while we wait.
WHAT names the wait for the main-thread assertion + logging.

MAIN THREAD ONLY: `accept-process-output' routes through `ns_select_1' ->
`[NSApp run]', which deadlocks Emacs off the main thread (the AGENTS.md
worker-thread trap); every blocking wait in this module asserts it."
  (agent-repl--assert-main-thread (format "frontend-await-uds %s" what))
  (let ((deadline (+ (float-time) timeout)) result)
    (while (and (not (setq result (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output agent-repl--uds-process 0.1))
    result))

(defun agent-repl--frontend-await-health-command
    (field payload workspace session-id what)
  "Require successful UDS health FIELD with PAYLOAD for WORKSPACE.
SESSION-ID is the expected identity for session health and nil for daemon
health.  WHAT identifies the scope in diagnostics.  The correlated health
view with `healthy=true' is authoritative; a `CommandAck' only confirms
receipt.  Timeout, command rejection, and an unhealthy result all abort
before the caller mutates UI or workspace state."
  (agent-repl--frontend-wait-ready)
  (let (response rejection request-id)
    (agent-repl--log workspace
                     "frontend-health: begin what=%s field=%s payload=%S session-id=%s"
                     what field payload (or session-id "nil"))
    (setq request-id (agent-repl--uds-send-command field payload workspace))
    (agent-repl--uds-track-health-response
     request-id field workspace session-id
     (lambda (view) (setq response view)))
    (agent-repl--uds-track-command
     request-id field workspace
     (lambda (err) (setq rejection err)))
    (unless (agent-repl--frontend-await-uds
             (lambda () (or response rejection))
             agent-repl-frontend-health-timeout
             (format "health %s" what))
      (agent-repl--uds-untrack-command request-id workspace "health-timeout")
      (agent-repl--uds-untrack-health-response
       request-id workspace "health-timeout")
      (agent-repl--log workspace
                       "frontend-health: TIMEOUT what=%s field=%s request-id=%s timeout=%.3fs"
                       what field request-id agent-repl-frontend-health-timeout)
      (error "agent-repl: %s health check timed out after %.3fs"
             what agent-repl-frontend-health-timeout))
    (when rejection
      (agent-repl--uds-untrack-health-response
       request-id workspace "command-rejected")
      (agent-repl--log workspace
                       "frontend-health: REJECTED what=%s field=%s request-id=%s error=%s"
                       what field request-id rejection)
      (error "agent-repl: %s health check rejected: %s" what rejection))
    (unless (plist-get response :healthy)
      (let ((reason (plist-get response :reason)))
        (agent-repl--log workspace
                         "frontend-health: UNHEALTHY what=%s field=%s request-id=%s session-id=%s reason=%S response=%S"
                         what field request-id (or session-id "nil")
                         reason response)
        (error "agent-repl: %s is unhealthy: %s"
               what
               (if (and (stringp reason) (not (string-empty-p reason)))
                   reason
                 "daemon supplied no reason"))))
    (agent-repl--log workspace
                     "frontend-health: HEALTHY what=%s field=%s request-id=%s session-id=%s response=%S"
                     what field request-id (or session-id "nil") response)
    t))

(defun agent-repl--frontend-wait-daemon-healthy ()
  "Require the daemon's correlated initialization-readiness assertion.
This proves the daemon completed its startup assembly and bound its
boot-critical listeners.  It does not probe shim-store or the sidecar:
session health separately proves the live daemon -> shim route and the shim's
own dependencies, including shim-store; startup service orchestration
separately validates and kickstarts the launchd jobs."
  (agent-repl--frontend-await-health-command
   "daemonHealth" nil nil nil "daemon"))

(defun agent-repl--frontend-wait-session-healthy (ws session-id)
  "Require WS's live shim health and identity before rendering SESSION-ID.
The daemon routes the command by WS's project directory and verifies that
its live shim is connected and healthy.  SESSION-ID correlation prevents a
stale binding from being rendered after a restart or remount."
  (unless (and (stringp session-id) (not (string-empty-p session-id)))
    (agent-repl--log ws
                     "frontend-health: invalid session id for session health id=%S"
                     session-id)
    (error "agent-repl: cannot health-check workspace %s without a session id" ws))
  (agent-repl--frontend-await-health-command
   "sessionHealth" (list :sessionId session-id)
   (agent-repl--frontend-ws-command-key ws)
   session-id
   (format "session ws=%s id=%s" ws session-id)))

(defun agent-repl--frontend-create-handle-rejection (cwd model resume force-fresh err)
  "Handle a REJECTED `createSession' for CWD; MODEL/RESUME were the request.
ERR is the `CommandAck' error string.  When it names a missing resume
transcript (the daemon's §2.10 resume-viability hard-fail, whose message
contains \"has no transcript\"), reproduce the lost-transcript behavior:
FORCE-FRESH recreates with no resume; otherwise open an investigation
workspace and re-raise `agent-repl-resume-transcript-missing'.  Any other
rejection is a loud error.

NOTE (frontend.v1 constraint): a `CommandAck' carries only an error
STRING, not the structured `searched_paths' the old HTTP 422 body did, so
the investigation is dispatched with the resume id alone (nil paths)."
  (if (and (stringp err) resume (string-match-p "has no transcript" err))
      (progn
        (agent-repl--log nil
                         "createSession REJECTED for %s: resume %s transcript missing (%s)"
                         cwd resume err)
        (if force-fresh
            (agent-repl--frontend-force-fresh-on-lost-transcript cwd model resume)
          (let ((ws-name (agent-repl--dispatch-resume-investigation resume nil cwd)))
            (signal 'agent-repl-resume-transcript-missing
                    (list (format (concat "resume target %s has no transcript — refusing a fresh "
                                          "conversation; opened investigation workspace `%s'")
                                  resume ws-name)
                          resume ws-name)))))
    (error "agent-repl: createSession for %s failed: %s" cwd (or err "rejected"))))

(defun agent-repl--frontend-force-fresh-on-lost-transcript (cwd model resume-id)
  "Degrade a lost-transcript resume for RESUME-ID to a FRESH conversation.
Invoked by `agent-repl--frontend-create-handle-rejection' when
`agent-repl--force-fresh-conversation' (or the FORCE-FRESH arg) overrides
the default investigation dispatch.  Logs the override, then recreates the
session in CWD with MODEL and NO resume (which cannot re-trigger the
lost-transcript hard-fail), and returns the fresh session id."
  (agent-repl--log nil
                   (concat "force-fresh-conversation: lost resume %s overridden — creating a "
                           "fresh conversation in %s (skipping investigation)")
                   resume-id cwd)
  (agent-repl--frontend-create-session cwd model nil))

(defun agent-repl--frontend-create-session (cwd &optional model resume force-fresh)
  "Create a daemon session rooted at CWD over the UDS `createSession' command;
return the new session id.  MODEL and RESUME (a durable claude session
uuid) are optional passthroughs.

The daemon's `createSession' `CommandAck' is a BARE RECEIPT (it carries no
session id); the id arrives on a pushed `SessionView' whose workspace is
CWD.  So this: sends `createSession', awaits the ack (pumping the UDS link
via `agent-repl--frontend-await-uds'), then correlates the id by matching
the non-terminal `SessionView' the daemon pushed for CWD
\(`agent-repl--frontend-live-session-id-for-cwd').  The daemon pushes that
view BEFORE the ack, so on an accepted ack the view is already stored; a
brief extra await tolerates the reverse ordering.  Creates are serialized
per cwd (`agent-repl--frontend-creates-in-flight'), and the daemon stands
down every older session on the cwd — pushing each one's terminal view —
before it pushes the new session's, so at most one live view per cwd
exists and the correlation is unambiguous.

On a REJECTED ack the lost-workspace resume attempt is handled by
`agent-repl--frontend-create-handle-rejection': the resume-viability
hard-fail (ack error contains \"has no transcript\") either opens an
investigation workspace and re-raises `agent-repl-resume-transcript-missing'
\(default), or — under FORCE-FRESH / `agent-repl--force-fresh-conversation'
— recreates with no resume.

MODEL defaults to `agent-repl-interactive-model' when nil, matching the
CLI-launch path: a concrete `--model' makes the daemon's hello carry the
real model from the first frame instead of leaving the topbar picker empty
until the first turn.  A nil `agent-repl-interactive-model' is respected as
\"let the CLI choose\" and sends no model.

The account the CLI runs as travels in `configDir', computed from CWD by
`agent-repl--compute-config-dir' — one daemon serves every workspace, so
the daemon's own environment cannot encode a per-workspace account; without
this field every gui session would run as whichever account the daemon
inherited."
  (unless (and (stringp cwd) (not (string-empty-p cwd)))
    (error "agent-repl: create-session requires a cwd (got %S)" cwd))
  (when (gethash cwd agent-repl--frontend-creates-in-flight)
    (error "agent-repl: a createSession for %s is already in flight" cwd))
  (let* ((force-fresh (or force-fresh agent-repl--force-fresh-conversation))
         (model (agent-repl--effective-model model))
         (posture (agent-repl--frontend-session-posture cwd))
         (config-dir (plist-get posture :config-dir))
         (permission-mode (plist-get posture :permission-mode))
         (allow-ungated (plist-get posture :allow-ungated))
         (payload (append (list :cwd cwd)
                          (when model (list :model model))
                          (when resume (list :resumeClaudeSessionId resume))
                          (when config-dir (list :configDir config-dir))
                          (when permission-mode
                            (list :permissionMode permission-mode))
                          ;; Sent ONLY when a caller has bound the consent, so
                          ;; an ordinary create can never carry it by accident.
                          (when allow-ungated
                            (list :allowUngated t))))
         ;; Mutable outcome the ack callbacks flip; all keys pre-populated so
         ;; `plist-put' mutates in place and the closures + await loop observe it.
         (ack (list :done nil :ok nil :error nil)))
    ;; Send + await the ack UNDER the in-flight guard, but handle the OUTCOME
    ;; after releasing it: the force-fresh rejection branch recreates the SAME
    ;; cwd, which the guard would otherwise refuse as "already in flight".
    (puthash cwd t agent-repl--frontend-creates-in-flight)
    (unwind-protect
        (let ((req (agent-repl--uds-send-command "createSession" payload cwd)))
          (agent-repl--uds-track-command
           req "createSession" cwd
           (lambda (err) (plist-put ack :error err) (plist-put ack :done t))
           (lambda () (plist-put ack :ok t) (plist-put ack :done t)))
          (agent-repl--frontend-await-uds
           (lambda () (plist-get ack :done))
           agent-repl-frontend-create-timeout
           (format "createSession cwd=%s" cwd)))
      (remhash cwd agent-repl--frontend-creates-in-flight))
    (cond
     ((not (plist-get ack :done))
      (error "agent-repl: createSession for %s timed out after %ss"
             cwd agent-repl-frontend-create-timeout))
     ((plist-get ack :ok)
      (let ((id (agent-repl--frontend-await-uds
                 (lambda () (agent-repl--frontend-live-session-id-for-cwd cwd))
                 2 (format "sessionView cwd=%s" cwd))))
        (unless id
          (error "agent-repl: createSession for %s acked but no SessionView arrived" cwd))
        (agent-repl--log nil "createSession: cwd=%s -> session %s (resume=%s)"
                         cwd id (or resume "none"))
        id))
     (t
      (agent-repl--frontend-create-handle-rejection
       cwd model resume force-fresh (plist-get ack :error))))))

(defun agent-repl--frontend-delete-session (id &optional ws)
  "Send a `deleteSession' UDS command for session ID; return the request-id.
WS, when known, keys the frame + logging.
Fire-and-forget: the terminal `SessionView' the daemon pushes updates the
store, and a rejected ack surfaces loudly via the shared ack handler."
  (let ((req (agent-repl--uds-send-command "deleteSession" (list :sessionId id) ws)))
    (agent-repl--uds-track-command req "deleteSession" ws)
    req))

;; The GET /commands slash-menu fetch + POST /commands/refresh re-resolve
;; were deleted in the S9 cutover: the slash-command menu is now the pushed
;; `SessionInitView' (retained `SystemInit'), read off frontend-state.el's
;; session-init store by input.el's completion — no HTTP round-trip.

(defun agent-repl--frontend-session-live-p (id)
  "Return non-nil when ID has a pushed `SessionView' that is not terminal.
Reads the frontend-state.el SessionView store the daemon pushes into — the
replacement for the GET /sessions per-id probe."
  (let ((view (agent-repl--frontend-session-view id)))
    (and view (not (eq (plist-get view :terminal) t)))))

(defun agent-repl--frontend-ws-turn-active-p (ws)
  "Return non-nil when WS's last pushed `WorkspaceState' is turn-active.
Reads the `:turn-active' resolution input frontend-state.el stashed under
`:pushed-render-state-meta' from the pushed frame (protojson bool -> t)."
  (eq (plist-get (agent-repl--ws-get ws :pushed-render-state-meta) :turn-active) t))

(defun agent-repl--frontend-turn-active-sessions ()
  "Return workspace-bound session ids the daemon reports mid-turn.
Sourced entirely from pushed-frame state (no daemon round-trip): a session
counts when its bound workspace's last `WorkspaceState' push is turn-active
\(`agent-repl--frontend-ws-turn-active-p') AND its `SessionView' is
non-terminal (`agent-repl--frontend-session-live-p').  The daemon-stop
guard keys on this.

Iterating only live workspaces' bound ids intrinsically excludes both a
stale UNBOUND session (never visited) and a TERMINAL session
\(filtered by the live-p check) — exactly the two exclusions the old
GET /sessions sweep spelled out."
  (let (busy)
    (dolist (ws (agent-repl--live-ws-names) (nreverse busy))
      (let ((sid (agent-repl--ws-get ws :frontend-session-id)))
        (when (and sid
                   (agent-repl--frontend-ws-turn-active-p ws)
                   (agent-repl--frontend-session-live-p sid))
          (push sid busy))))))

(defun agent-repl--frontend-all-turn-active-session-ids ()
  "Return every live session id whose latest daemon state is turn-active.
Unlike `agent-repl--frontend-turn-active-sessions', this startup-safety
probe includes workspace paths Emacs has not restored yet.  A normal Emacs
restart must never kill a turn merely because the perspective-to-path
mapping has not been reconstructed.  Terminal sessions remain excluded."
  (let (busy)
    (dolist (state (agent-repl--frontend-workspace-state-views-all)
                   (delete-dups (nreverse busy)))
      (let ((sid (plist-get state :sessionId)))
        (when (and sid
                   (eq (plist-get state :turnActive) t)
                   (agent-repl--frontend-session-live-p sid))
          (push sid busy))))))

;; The client-side orphan reaper that used to live here is GONE, on purpose.
;; It existed because a superseded session's shim kept running with nobody
;; attached; the daemon now stands down every displaced session itself at
;; create time (supersede.go: terminal record + shim stop + pushed terminal
;; view), which removes the leak at its source.  The reaper's remaining
;; behavior was purely harmful: it keyed "orphan" off the workspace binding,
;; so a mis-bound workspace made it delete the REAL session (the 2026-07-26
;; every-restore mis-bind), and it looped forever on sessions whose deletes
;; the daemon silently no-op'd.  Client-driven deletion decided from a pushed
;; roster is the wrong layer for shim GC; do not reintroduce it.

;;;; ---- Workspace binding ---------------------------------------------------

(defun agent-repl--frontend-session-url (session-id)
  "Return the webapp URL that attaches to SESSION-ID."
  (format "%s/?session=%s" (agent-repl--frontend-base-url) session-id))

(defun agent-repl--frontend-ensure-session (ws)
  "Return WS's live daemon session id, creating the session if needed.
Lazily ensures the daemon itself (build-if-stale + launch via
daemon.el), waits for readiness, then reuses WS's recorded
`:frontend-session-id' when the daemon still lists it as live —
otherwise POSTs a new session rooted at WS's `:project-dir'.

A workspace WITHOUT a recorded `:project-dir' is not an error: plain
project perspectives (opened via projectile, never through agent-repl
workspace creation) have no registration at all.  The project dir is
adopted from the current git root exactly as workspace creation would
resolve it, and recorded so later consumers see the same value.
Signals `user-error' outside a git repository (the resolver's own
contract)."
  ;; ensure-frontend-daemon returns nil (without acting) when auto-start
  ;; is disabled or init is inhibited — polling readiness against a
  ;; daemon that was never started would burn the whole retry budget to
  ;; produce a misleading "never became ready" error, so fail fast with
  ;; the actual cause. A nil return with a LIVE daemon process cannot
  ;; happen (ensure returns the process in every acting branch).
  (unless (agent-repl--ensure-frontend-daemon)
    (error "agent-repl: frontend daemon not started (auto-start disabled or init inhibited)"))
  (agent-repl--frontend-wait-ready)
  (let ((existing (agent-repl--ws-get ws :frontend-session-id)))
    (if (and existing (agent-repl--frontend-session-live-p existing))
        existing
      (let ((dir (or (agent-repl--ws-get ws :project-dir)
                     (let ((root (agent-repl--resolve-current-git-root)))
                       (agent-repl--log ws "ensure-session: adopting git root %s for unregistered ws %s" root ws)
                       (agent-repl--ws-put ws :project-dir root)
                       root))))
        ;; Env restore: after an Emacs restart the workspace plist has no
        ;; :active-env and no instantiation structs, so the durable-id
        ;; lookup below resolves nil and the created session silently
        ;; starts a BLANK conversation (observed as "SPC o c doesn't
        ;; restore the gui session").  A gui-first open is now the ONLY
        ;; boot path a workspace has, so it must restore env itself from
        ;; the persisted state file via `agent-repl--initialize-ws-env'
        ;; rather than relying on some other boot path having already
        ;; done it.  Env presence also heals the sentinel handlers,
        ;; which error on a nil :active-env.
        (unless (agent-repl--ws-get ws :active-env)
          (agent-repl--initialize-ws-env ws dir))
        ;; Resume the workspace's durable claude session so a
        ;; recreated daemon binding (daemon restart, Emacs restart,
        ;; panel close/reopen) CONTINUES the conversation — the
        ;; frontend is presentation, the session is the shared backend.
        ;; nil (no session ever recorded) starts fresh.
        (let* ((resume (agent-repl--ws-durable-claude-session-id ws))
               (id (agent-repl--frontend-create-session
                    dir (agent-repl--ws-get ws :model) resume)))
          (agent-repl--ws-put ws :frontend-session-id id)
          (agent-repl--ws-put ws :reattach-failed nil)
          (agent-repl--ws-put ws :reattach-failures nil)
          (agent-repl--frontend-reattach-timer-start)
          (agent-repl--log ws "frontend session created: %s (cwd=%s resume=%s)"
                           id dir (or resume "none"))
          id)))))

;;;; ---- Daemon-bounce resilience: the reattach loop -----------------------
;;
;; The daemon may be bounced at ANY time by agents deploying builds —
;; that is policy, not an accident (see AGENTS.md "Daemon bounce
;; policy").  Sessions are daemon-memory-resident, so after a bounce
;; every recorded `:frontend-session-id' names a session the new
;; instance has never heard of.  This loop is the client half of the
;; contract: notice, re-ensure (resume + transcript replay brings the
;; conversation back), remount the webview — and when reattach REPEATEDLY
;; fails against a daemon that answers (the breaking-API case), stop
;; retrying and surface the failure loudly instead of spinning forever.

(defcustom agent-repl-frontend-reattach-interval 15
  "Seconds between reattach sweeps over gui workspace session bindings."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-frontend-reattach-max-failures 3
  "Consecutive reattach failures after which a workspace gives up.
A give-up sets `:reattach-failed', surfaces a warning naming the likely
cause (client/daemon version mismatch), and stops retrying until a
successful ensure or a manual panel open clears the marker."
  :type 'integer
  :group 'agent-repl)

(defvar agent-repl--frontend-reattach-timer nil
  "Repeating timer driving `agent-repl--frontend-reattach-check', or nil.")

(defvar agent-repl--frontend-last-boot-id nil
  "The daemon boot id last observed by the reattach sweep, or nil.
A change means a NEW daemon instance: every `:reattach-failed' give-up
is reset, because the failures belonged to the previous instance.")

(defun agent-repl--frontend-reattach-timer-start ()
  "Idempotently start the reattach sweep timer.
No-op in batch/sandbox (`agent-repl--frontend-init-inhibited-p') —
the same environments that never auto-start the daemon."
  (when (and (not agent-repl--frontend-reattach-timer)
             (not (agent-repl--frontend-init-inhibited-p)))
    (setq agent-repl--frontend-reattach-timer
          (run-with-timer agent-repl-frontend-reattach-interval
                          agent-repl-frontend-reattach-interval
                          #'agent-repl--frontend-reattach-check))))

(defun agent-repl--frontend-reattach-check ()
  "Reattach gui workspaces whose bound session vanished from the pushed roster.
Reads the pushed `SessionView' store (frontend-state.el), NOT GET /sessions:
a bound id absent from the store (or terminal) means a new daemon instance
\(or a deleted session) — the workspace re-ensures and remounts.  When the
UDS link is DOWN but bindings exist, ensure the daemon (spawn or adopt) so
the next sweep can reattach once the link + its snapshot return.

Boot-instance detection moved to the pushed `DaemonView'
\(`agent-repl--frontend-apply-daemon-view' -> `agent-repl--frontend-note-boot-id'):
a bounce drops the UDS link, and the reconnect snapshot carries the new
boot id, so the give-up reset no longer needs a GET /sessions probe here."
  (if (not (agent-repl--uds-connected-p))
      (when (cl-some (lambda (ws) (agent-repl--ws-get ws :frontend-session-id))
                     (agent-repl--live-ws-names))
        (agent-repl--log nil "reattach: UDS link down with bound sessions — ensuring daemon")
        (condition-case err
            (agent-repl--ensure-frontend-daemon)
          (error
           (agent-repl--log nil "reattach: daemon ensure failed: %s"
                            (error-message-string err)))))
    (let ((listed (delq nil (mapcar (lambda (v)
                                      (unless (eq (plist-get v :terminal) t)
                                        (plist-get v :sessionId)))
                                    (agent-repl--frontend-session-views-all)))))
      (dolist (ws (agent-repl--live-ws-names))
        (when-let ((bound (agent-repl--ws-get ws :frontend-session-id)))
          (cond
           ((member bound listed)
            (agent-repl--ws-put ws :reattach-failed nil)
            (agent-repl--ws-put ws :reattach-failures nil))
           ((agent-repl--ws-get ws :reattach-failed) nil)
           (t (agent-repl--frontend-reattach-ws ws bound))))))))

(defun agent-repl--frontend-note-boot-id (boot-id)
  "Record BOOT-ID; on an instance change, reset every reattach give-up.
A give-up (`:reattach-failed') binds a failure history to ONE daemon
instance — a fresh instance deserves fresh attempts.  Old daemons that
predate boot ids report nil, which never triggers a reset."
  (when (and boot-id (not (equal boot-id agent-repl--frontend-last-boot-id)))
    (when agent-repl--frontend-last-boot-id
      (agent-repl--log nil "reattach: daemon instance changed %s -> %s — resetting give-ups"
                        agent-repl--frontend-last-boot-id boot-id)
      (dolist (ws (agent-repl--live-ws-names))
        (when (agent-repl--ws-get ws :reattach-failed)
          (agent-repl--ws-put ws :reattach-failed nil)
          (agent-repl--ws-put ws :reattach-failures nil))
        ;; The switch-ensure give-up binds to ONE daemon instance for the same
        ;; reason: a workspace the old instance could not open deserves a
        ;; fresh attempt from the new one. Its cooldown stamp goes too, so the
        ;; first switch after a bounce is not swallowed by the old timer.
        (when (agent-repl--ws-get ws :switch-ensure-failed)
          (agent-repl--ws-put ws :switch-ensure-failed nil)
          (agent-repl--ws-put ws :switch-ensure-failures nil))
        (agent-repl--ws-put ws :switch-ensure-at nil)))
    (setq agent-repl--frontend-last-boot-id boot-id)))

(defun agent-repl--frontend-reattach-ws (ws stale-id)
  "Re-ensure WS's daemon session after STALE-ID vanished; remount webview.
On failure the stale binding is RESTORED so the next sweep retries;
after `agent-repl-frontend-reattach-max-failures' consecutive failures
the workspace is marked `:reattach-failed' and a warning surfaces."
  (condition-case err
      (progn
        (agent-repl--log ws "reattach: session %s vanished — re-ensuring ws=%s" stale-id ws)
        (agent-repl--ws-put ws :frontend-session-id nil)
        (let ((id (agent-repl--frontend-ensure-session ws)))
          (agent-repl--frontend-sync-webview ws id)
          (agent-repl--ws-put ws :reattach-failures nil)
          (agent-repl--log ws "reattach: ws=%s recovered as %s" ws id)))
    (error
     (let ((n (1+ (or (agent-repl--ws-get ws :reattach-failures) 0))))
       ;; Restore the vanished binding: it is the marker the next sweep
       ;; keys the retry on.
       (agent-repl--ws-put ws :frontend-session-id stale-id)
       (agent-repl--ws-put ws :reattach-failures n)
       (agent-repl--log ws "reattach: ws=%s attempt %d/%d failed: %s"
                         ws n agent-repl-frontend-reattach-max-failures
                         (error-message-string err))
       (when (>= n agent-repl-frontend-reattach-max-failures)
         (agent-repl--ws-put ws :reattach-failed t)
         (display-warning
          'agent-repl
          (format (concat "workspace %s failed to reattach to the new daemon instance "
                          "after %d attempts (%s) — likely a client/daemon version "
                          "mismatch; rebuild/reload, then reopen the panel")
                  ws n (error-message-string err))
          :error))))))

(defun agent-repl--frontend-rebind-workspaces-after-restart ()
  "Bounce every open gui workspace's shim onto the freshly restarted daemon.
Meant to run right after `agent-repl-frontend-daemon-restart' force-bounces
the daemon: rather than leaving each open panel dark until the next reattach
sweep timer fires (up to `agent-repl-frontend-reattach-interval' away), this
drives the reattach IMMEDIATELY so every workspace is good to go the moment
the restart returns.

Waits for the new daemon to answer, then delegates to
`agent-repl--frontend-reattach-check' — the SAME machinery the sweep timer
runs.  Against a fresh instance that lists none of the old bindings, the
sweep notes the new boot id (resetting the give-ups the previous instance
left behind), re-ensures each bound workspace's session (a fresh shim that
resumes the durable conversation), and remounts each live webview.  Returns
the count of open workspaces that carried a session binding to rebind."
  (agent-repl--frontend-wait-ready)
  (let ((n (cl-count-if (lambda (ws) (agent-repl--ws-get ws :frontend-session-id))
                        (agent-repl--live-ws-names))))
    (agent-repl--frontend-reattach-check)
    ;; reattach-check rebinds each workspace's daemon session and, via
    ;; `agent-repl--frontend-sync-webview', remounts only those whose
    ;; session id CHANGED.  A session that rehydrated under its old id is
    ;; left untouched, so its webview would keep rendering the pre-bounce
    ;; bundle.  Force a remount of EVERY open webview so a bounce reliably
    ;; reloads the served bundle across the board — a bounce is exactly
    ;; when a fresh build lands, and each remount replays history off the
    ;; live session, so nothing is lost.
    (agent-repl--frontend-remount-all-webviews)
    n))

;; The in-flight message-queue plane (§2.13) is fully retired.  It was dead
;; server-side (the post-cutover daemon carries no `queue' array and
;; frontend.v1 has no queue field or queue-control command), so the Emacs
;; `queue-run-now'/`queue-cancel' HTTP override routes and the
;; perpetually-empty `:queued-messages' accessors were deleted in the S9
;; endgame — the webapp owns the queued-message UI end to end.

(defun agent-repl--gui-send-turn (ws input raw &optional on-settle)
  "The gui frontend's send capability (registry `:send-fn').
INPUT (the prepared text, which may carry the metaprompt prefix —
genuine message content) goes to the daemon session.  There is no
owning-workspace pin to apply here — WS's daemon session id already
identifies the target unambiguously, unlike a shared vterm buffer that
once needed disambiguating — but the prefix counter still increments
so metaprompt periodicity is tracked the same way for every workspace.
Posthooks and prompt summary key on RAW, identically.

Sets `:thinking' optimistically BEFORE the send so the turn reads as
in-flight immediately, ahead of the daemon's authoritative pushed
THINKING `WorkspaceState' (the sentinel/hook confirmation this write
once raced was deleted in the S8/S9 sentinel endgame).

Records the sent turn's request id and RAW text under `:sent-turn',
which is what `agent-repl-interrupt' needs to undo the send: the
daemon names the turn it retracts by request id, and RAW (never
INPUT) is what goes back to the input buffer, since the metaprompt
decoration is not the user's to revise.

Snaps the webview feed to its tail FIRST, before anything else: a
prompt sent from a feed scrolled up in history jumps to the bottom
immediately, rather than waiting for the daemon to echo the turn back
and render it.  The webapp's own repin-on-render (repinsToTail in
webapp/src/render.ts) still lands the answer at the tail, but only
once the turn arrives — this snap closes the round-trip gap so the
sender watches the bottom from the instant the prompt leaves."
  (agent-repl--log ws "do-send[gui] ws=%s len=%d" ws (length input))
  (agent-repl--frontend-snap-webview-to-tail ws)
  (agent-repl--mark-ws-thinking ws)
  (agent-repl--increment-prefix-counter ws)
  (agent-repl--ws-put ws :last-prompt-time (float-time))
  (agent-repl--ws-put ws :sent-turn
                      (list :request-id (agent-repl--frontend-send-user-message ws input)
                            :raw raw))
  (agent-repl--run-send-posthooks ws raw)
  (agent-repl--kickoff-prompt-summary ws raw)
  (when on-settle (funcall on-settle)))

(defun agent-repl--gui-interrupt (ws kind)
  "The gui frontend's interrupt capability (registry `:interrupt-fn').
Sends the UDS `interrupt' command keyed by WS's cwd
\(`agent-repl--frontend-ws-command-key' — the daemon resolves that cwd ->
session).  KIND (`escape' = `C-c C-k' STOP, `ctrl-c' = `C-c C-c' clear
draft) no longer changes the wire request: frontend.v1's `InterruptCmd'
carries only `hard' and NO retract id, so the retract half of the old
`C-c C-k' undo is gone (it was already a daemon no-op post-cutover — the
daemon's HTTP interrupt always reported retracted=false).  Always returns
t: the interrupt is dispatched, never `retracted'."
  (let ((id (agent-repl--ws-get ws :frontend-session-id))
        (key (agent-repl--frontend-ws-command-key ws)))
    ;; nil payload -> the daemon reads InterruptCmd{} (hard=false), the same
    ;; soft interrupt the retired HTTP route drove.
    (let ((req (agent-repl--uds-send-command "interrupt" nil key)))
      (agent-repl--uds-track-command req "interrupt" ws)
      (agent-repl--log ws "interrupt[gui]: ws=%s session=%s kind=%s (uds interrupt)"
                       ws id kind)
      t)))

(defun agent-repl--gui-running-p (ws)
  "The gui frontend's liveness capability (registry `:running-p-fn').
Cheap check: a session binding exists.  Actual daemon liveness is
probed (and healed) lazily by the send path's ensure."
  (and (agent-repl--ws-get ws :frontend-session-id) t))

(defun agent-repl--gui-durable-session-id (ws)
  "The gui frontend's durable-id capability.
Fetches the daemon-captured claude_session_id for WS's bound session;
nil when unbound, not yet initialized, or the daemon is unreachable
\(logged — a dead daemon degrades a frontend switch to a fresh
conversation rather than aborting it)."
  (when-let ((sid (agent-repl--ws-get ws :frontend-session-id)))
    ;; Read the durable id off the pushed `SessionView' store (the daemon
    ;; stamps `claudeSessionId' once the CLI reports it), not a GET /sessions
    ;; round-trip.  Absent until the first frame lands — nil then, exactly as
    ;; the old unreachable/uninitialized path degraded.
    (plist-get (agent-repl--frontend-session-view sid) :claudeSessionId)))

(defun agent-repl--gui-adopt-session (ws claude-session-id)
  "The gui frontend's adopt capability: resume CLAUDE-SESSION-ID.
Creates a fresh daemon session with resume set and binds it to WS, so
the subsequent open attaches to the continued conversation."
  (unless (agent-repl--ensure-frontend-daemon)
    (error "agent-repl: frontend daemon not started (auto-start disabled or init inhibited)"))
  (agent-repl--frontend-wait-ready)
  (let* ((dir (or (agent-repl--ws-get ws :project-dir)
                  (agent-repl--resolve-current-git-root)))
         (id (agent-repl--frontend-create-session
              dir (agent-repl--ws-get ws :model) claude-session-id)))
    (agent-repl--ws-put ws :frontend-session-id id)
    (agent-repl--log ws "gui adopted claude session %s as %s" claude-session-id id)
    id))

(defun agent-repl--frontend-force-fresh-session (ws)
  "Create a FRESH daemon session for WS (NO resume), bind it, and return its id.
Mirrors `agent-repl--gui-adopt-session' but passes NO resume, so a BLANK
conversation replaces whatever the normal ensure path would replay.
Resets the reattach failure markers so the fresh binding reads as healthy.
Does NOT sync the webview — callers that display do that.  The fresh
session captures its own durable id through the usual hook path once it
runs, so a later resume continues the fresh conversation rather than the
discarded one."
  (unless (agent-repl--ensure-frontend-daemon)
    (error "agent-repl: frontend daemon not started (auto-start disabled or init inhibited)"))
  (agent-repl--frontend-wait-ready)
  (let* ((dir (or (agent-repl--ws-get ws :project-dir)
                  (agent-repl--resolve-current-git-root)))
         (id (agent-repl--frontend-create-session
              dir (agent-repl--ws-get ws :model) nil)))
    (agent-repl--ws-put ws :frontend-session-id id)
    (agent-repl--ws-put ws :reattach-failed nil)
    (agent-repl--ws-put ws :reattach-failures nil)
    (agent-repl--log ws "force-fresh-conversation: fresh session %s bound (cwd=%s)" id dir)
    id))

(defun agent-repl--frontend-send-user-message (ws text)
  "Send TEXT as WS's user turn over the UDS `submitPrompt' command.
Ensures the session first (recreating a stale binding), so a send into a
dead session heals instead of failing, then sends `submitPrompt' keyed by
WS's cwd (`agent-repl--frontend-ws-command-key') — the daemon resolves that
cwd -> session, so no session id is on the wire.  Returns the command
request-id (retained by the caller as the sent-turn handle).

The one-shot `:next-send-origin' tag (set by the merge remediation) is
consumed and cleared here but is NOT forwarded: frontend.v1's
`SubmitPromptCmd' has no `origin' field, so the merge status-card stamping
is gone — matching the already-dead server behavior (the retired HTTP
/message route never read `origin' into the driver either)."
  (let ((id (agent-repl--frontend-ensure-session ws))
        (origin (agent-repl--ws-get ws :next-send-origin)))
    ;; The ensure may have HEALED a dead binding into a fresh session —
    ;; the displayed webview must follow, or the user watches the dead
    ;; session while the turn streams into the replacement.
    (agent-repl--frontend-sync-webview ws id)
    (when origin (agent-repl--ws-put ws :next-send-origin nil))
    (agent-repl--log ws "frontend send: session=%s len=%d origin=%s (uds submitPrompt)"
                     id (length text) origin)
    (let ((req (agent-repl--uds-send-command
                "submitPrompt" (list :text text)
                (agent-repl--frontend-ws-command-key ws))))
      (agent-repl--uds-track-command req "submitPrompt" ws)
      req)))

;;;; ---- Never-blue: the workspace-SWITCH ensure ---------------------------
;;
;; WHY `openWorkspace' AND NOT A NEW `switchWorkspace' ARM.
;;
;; The daemon's open handler is already an idempotent "ensure this workspace"
;; and carries NO view side effects, so a switch means exactly what an open
;; means to it.  `server.WorkspaceOpener.Open' (daemon/internal/server/
;; workspaceopen.go) is precisely `BindWorkspace' + `Ensurer.Ensure':
;;
;;   - `bindRecord' returns early WITHOUT writing once the record already
;;     carries a `claude_session_id', so a re-open never re-binds;
;;   - `Ensure' -> `sessiondrv.Manager.bringUp' takes the manager mutex,
;;     finds the workspace in `byWS' and returns the live driver
;;     immediately — no shim spawn, no consumer, no push.
;;
;; Panel mounting is entirely Emacs-side, so there is no open-only side
;; effect a switch would wrongly imply.  A second command arm would therefore
;; be a synonym, and two names for one daemon behavior is exactly how the two
;; drift apart later.  So: open == ensure, and switch sends open.
;;
;; WHAT IS NOT FREE.  `bindRecord' runs `session.Discover' on EVERY call —
;; the already-bound early-return sits AFTER it, because the migration report
;; needs the probe.  That is a projects-directory scan per send, which is why
;; the debounce below is a real requirement rather than politeness.

(defcustom agent-repl-frontend-switch-ensure-cooldown 30
  "Seconds before a workspace switch may re-send `openWorkspace' for a ws.
Only reached when the workspace still has no live session: a switch to a
workspace that already has one skips the send outright (nothing to ensure).
The cooldown bounds the remaining case — repeatedly switching to a
workspace the daemon cannot bring up — so the daemon is not made to rescan
its projects directory on every persp activation."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-frontend-switch-ensure-max-failures 3
  "Consecutive switch-ensure failures after which a workspace gives up.
A give-up sets `:switch-ensure-failed', surfaces ONE warning, and stops
sending until the daemon instance changes (see
`agent-repl--frontend-note-boot-id') — the same give-up shape the reattach
sweep uses, so a workspace the daemon simply cannot open never retry-loops."
  :type 'integer
  :group 'agent-repl)

(defun agent-repl--frontend-backfill-settled-p (session-id)
  "Return non-nil when SESSION-ID's history has finished arriving (F2).
Reads the daemon-resolved `SessionView.backfill' off the pushed-frame store
\(frontend-state.el); the webapp and Emacs share one verdict and neither
derives it.

Settled means there is nothing more to wait for:
  `BACKFILL_STATE_DONE'          the transcript is in the store;
  `BACKFILL_STATE_UNSPECIFIED'   there is no transcript to backfill at all
                                 (a genuinely fresh workspace — an empty feed
                                 is the CORRECT render, not a blue bug).

`BACKFILL_STATE_PENDING' and `BACKFILL_STATE_FAILED' are both UNSETTLED, so
the switch re-ensures: pending may simply not have landed yet, and failed is
the sidecar telling us it could not read the transcript — the one case that
must never be mistaken for \"nothing to backfill\".

A daemon too old to send the field reports nil, which reads as settled: it
cannot backfill on switch either, so retrying would loop for nothing."
  (let ((state (plist-get (agent-repl--frontend-session-view session-id) :backfill)))
    (or (null state)
        (member state '("BACKFILL_STATE_DONE" "BACKFILL_STATE_UNSPECIFIED")))))

(defun agent-repl--frontend-switch-ensure-skip-reason (ws)
  "Return a string naming why WS's switch must not send, or nil to send.
Ordered cheapest-first.  Every arm is a genuine no-op rather than a
deferral: nothing here is retried later on this path, because the switch
that would have retried it is the retry."
  (cond
   ((null ws) "no workspace")
   ;; Nothing to send into. The reattach sweep owns daemon revival; a switch
   ;; must not race it into a second spawn.
   ((not (agent-repl--uds-connected-p)) "uds link down")
   ;; No cwd means no routable wire key, and the daemon routes purely by cwd.
   ((null (agent-repl--ws-get ws :project-dir)) "no project-dir")
   ;; THE common case: a workspace already driving a live session WHOSE
   ;; HISTORY ARRIVED has nothing to ensure, so the send would be pure
   ;; daemon-side rescan.
   ;;
   ;; Liveness alone is NOT enough (F2). A session the daemon bound and brought
   ;; up, but whose transcript the sidecar never delivered, is live and blue at
   ;; the same time — and that is precisely the workspace the never-blue
   ;; contract is about. So the skip additionally requires the daemon's
   ;; backfill verdict to be settled; `pending' and `failed' both fall through
   ;; to a send, bounded by the cooldown and give-up below.
   ((let ((id (agent-repl--ws-get ws :frontend-session-id)))
      (and id
           (agent-repl--frontend-session-live-p id)
           (agent-repl--frontend-backfill-settled-p id)))
    "session already live and backfilled")
   ((agent-repl--ws-get ws :switch-ensure-failed) "gave up after repeated failures")
   ((let ((at (agent-repl--ws-get ws :switch-ensure-at)))
      (and at (< (- (float-time) at) agent-repl-frontend-switch-ensure-cooldown)))
    "within cooldown")
   (t nil)))

(defun agent-repl--frontend-note-switch-ensure-failure (ws err)
  "Record a failed switch-ensure for WS, giving up loudly at the cap.
ERR is the ack's error string.  Mirrors the reattach sweep's give-up: a
workspace the daemon cannot open is surfaced ONCE, not once per switch."
  (let ((n (1+ (or (agent-repl--ws-get ws :switch-ensure-failures) 0))))
    (agent-repl--ws-put ws :switch-ensure-failures n)
    (agent-repl--log ws "switch-ensure: ws=%s attempt %d/%d failed: %s"
                     ws n agent-repl-frontend-switch-ensure-max-failures err)
    (when (>= n agent-repl-frontend-switch-ensure-max-failures)
      (agent-repl--ws-put ws :switch-ensure-failed t)
      (display-warning
       'agent-repl
       (format (concat "workspace %s could not be opened on the daemon after %d "
                       "switches (%s) — its history will not backfill; check the "
                       "daemon log for transcript discovery")
               ws n err)
       :warning))))

(defun agent-repl--frontend-notify-workspace-switch (&optional ws)
  "Tell the daemon WS was switched to, so it binds + ensures the session.
This is the SWITCH half of the never-blue requirement: a workspace with a
known on-disk transcript must render its history the moment the user looks
at it, rather than sitting blue until they type.  The daemon side
\(`server.WorkspaceOpener') does the transcript discovery, the resume bind
and the shim bring-up; this only has to ask.

Fire-and-forget and heavily skipped — see
`agent-repl--frontend-switch-ensure-skip-reason'.  A rejected ack counts
toward the per-workspace give-up rather than retrying.  Returns the
request-id when a command went out, else nil.

LOGS, NEVER SIGNALS — the same contract
`agent-repl--frontend-release-workspace-session' keeps, and for the same
reason: this runs on the persp-activation path, where a signal would
strand the switch before the tail that flips the `:ws-loaded' latch.  The
link can also die between the connected-p check and the send, so the
guards above cannot be the only protection."
  (let* ((ws (or ws (agent-repl--ws-current-name)))
         (skip (agent-repl--frontend-switch-ensure-skip-reason ws)))
    (if skip
        (progn (agent-repl--log-verbose ws "switch-ensure: skipped (%s)" skip) nil)
      (condition-case err
          (progn
            (agent-repl--ws-put ws :switch-ensure-at (float-time))
            (agent-repl--log ws "switch-ensure: ws=%s -> openWorkspace (never-blue backfill)" ws)
            (let ((req (agent-repl--uds-send-command
                        "openWorkspace" nil (agent-repl--frontend-ws-command-key ws))))
              (agent-repl--uds-track-command
               req "openWorkspace" ws
               (lambda (e) (agent-repl--frontend-note-switch-ensure-failure ws e))
               (lambda () (agent-repl--ws-put ws :switch-ensure-failures nil)))
              req))
        (error
         (agent-repl--log ws "switch-ensure: ws=%s send FAILED: %s"
                          ws (error-message-string err))
         nil)))))

(defun agent-repl--frontend-release-workspace-session (ws)
  "Best-effort DELETE of WS's daemon session (for `agent-repl-ws-del-hook').
Reads `:frontend-session-id' (still present pre-tombstone), DELETEs it,
and clears the key.  Errors are LOGGED, never signalled: the workspace
nuke must not abort because the daemon is already gone — but nothing is
silently dropped, the failure lands in the agent-repl log."
  (let ((id (agent-repl--ws-get ws :frontend-session-id)))
    (when id
      (condition-case err
          (progn
            (agent-repl--frontend-delete-session id ws)
            (agent-repl--log ws "frontend session released: %s" id))
        (error
         (agent-repl--log ws "frontend session release FAILED for %s: %s"
                          id (error-message-string err))))
      (agent-repl--ws-put ws :frontend-session-id nil))))

(add-hook 'agent-repl-ws-del-hook #'agent-repl--frontend-release-workspace-session)

(provide 'frontend-client)

;;; frontend-client.el ends here
