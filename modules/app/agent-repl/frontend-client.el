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
;; The only URLs this module still builds are the WEBVIEW's
;; (`agent-repl--frontend-workspace-url' for a workspace's own view,
;; `agent-repl--frontend-session-url' for a viewer with no workspace): the
;; webapp bundle is served over HTTP to an embedded browser, which is a
;; browser navigation, not an Emacs-side HTTP client call.
;;
;; Binding model:
;;   - Each workspace gets AT MOST one daemon session, tracked under the
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
(require 'url-util)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl-connection-notice-warn "connection-notice" (text &optional level))
(declare-function agent-repl-connection-notices-retract "connection-notice" (reason))
(declare-function agent-repl--log-verbose "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--ws-current-name "agent-repl-workspace" ())
(declare-function agent-repl--ws-get "agent-repl-workspace" (ws key))
(declare-function agent-repl--ws-put "agent-repl-workspace" (ws key val))
(declare-function agent-repl--ws-name-for-dir "agent-repl-workspace" (dir))
(declare-function agent-repl--ensure-frontend-daemon "agent-repl-daemon" (&optional force))
(declare-function agent-repl--resolve-current-git-root "agent-repl-core" ())
(declare-function agent-repl--initialize-ws-env "agent-repl-session" (ws &optional project-dir-hint active-env-hint))
(declare-function agent-repl--frontend-snap-webview-to-tail "agent-repl-frontend" (ws))
(declare-function agent-repl--frontend-remount-all-webviews "agent-repl-frontend" ())
(declare-function agent-repl--frontend-init-inhibited-p "agent-repl-daemon" ())
(declare-function agent-repl--live-ws-names "agent-repl-workspace" ())
(declare-function agent-repl--mark-ws-thinking "input" (ws))
;; The UDS command channel + pushed-frame SessionView store (the daemon plane
;; that replaced the GET /sessions poller); resolved at call time.
(declare-function agent-repl--uds-send-command "frontend-uds" (field payload &optional workspace process &rest keys))
(declare-function agent-repl--uds-untrack-command "frontend-uds" (request-id workspace reason))
(declare-function agent-repl--uds-track-health-response
                  "frontend-uds"
                  (request-id field workspace session-id on-response))
(declare-function agent-repl--uds-untrack-health-response
                  "frontend-uds" (request-id workspace reason))
(declare-function agent-repl--uds-connected-p "frontend-uds" ())
(declare-function agent-repl-uds-connect "frontend-uds" (&optional path readiness-p))
(declare-function agent-repl--uds-run-timer "frontend-uds" (delay fn))
(declare-function agent-repl--frontend-ws-name "frontend-state" (workspace))
(declare-function agent-repl--frontend-session-view "frontend-state" (workspace))
(declare-function agent-repl--frontend-workspace-session-live-p "frontend-state" (workspace))
(declare-function agent-repl--frontend-session-views-all "frontend-state" ())
(declare-function agent-repl--frontend-workspace-state-views-all "frontend-state" ())
(declare-function agent-repl--frontend-daemon-view "frontend-state" ())
(declare-function agent-repl--ws-dir "agent-repl-status" (ws))

(defvar agent-repl--uds-process)
(defvar agent-repl-uds-socket-path)

;;;; ---- The workspace wire key ------------------------------------------
;;
;; The daemon keys every workspace-routed command (`submitPrompt',
;; `interrupt', `permissionAnswer') by the session's CWD: its
;; `SessionLocator.Locate' scans the registry for a non-terminal record whose
;; `CWD' EQUALS the `workspace' field, and `sessioncontroller.Manager' maps live
;; session controllers under that same cwd string (daemon/internal/server/sessioncontrollers.go,
;; daemon/internal/sessioncontroller/sessioncontroller.go).
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
`agent-repl--frontend-after-create-session' registered the session under, so the
daemon's cwd-keyed lookup resolves it.  NEVER the persp name WS itself.

Signals (via `agent-repl--ws-dir') when WS has no `:project-dir': a
command with no resolvable cwd cannot be routed, and a loud failure here
beats a daemon NACK that reads as \"no live session\"."
  (let ((key (agent-repl--ws-dir ws)))
    (agent-repl--log ws "frontend-wire-key: ws=%s cwd=%s" ws key)
    key))

;; When non-nil, a resume whose transcript the daemon cannot find DEGRADES
;; to a fresh conversation.  The default (nil) behavior on the daemon's
;; transcript-missing refusal is to surface the refusal unchanged.  Binding
;; this non-nil explicitly OVERRIDES that and recreates the session with no
;; resume.  `agent-repl-force-fresh-conversation' starts a fresh conversation
;; directly and does not rely on this override.
(defvar agent-repl--force-fresh-conversation nil
  "When non-nil, a lost-transcript resume degrades to a fresh conversation.
Overrides the default hard refusal in
`agent-repl--frontend-after-create-session'.")

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
  "Dial attempts for `agent-repl--frontend-after-ready' (0.2s apart)."
  :type 'integer
  :group 'agent-repl)

;;;; ---- Webview URL --------------------------------------------------------

(defun agent-repl--frontend-base-url ()
  "Return the daemon's HTTP base URL from the configured address.
The ONLY surviving URL construction in the Emacs client: it addresses the
webapp bundle the daemon serves to the embedded browser
\(`agent-repl--frontend-workspace-url',
`agent-repl--frontend-session-url'), which is a browser navigation.
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

(defun agent-repl--frontend-after-ready (on-ready on-failure &optional ws)
  "Run ON-READY once the frontend UDS snapshot establishes readiness.
`agent-repl--ensure-frontend-daemon' returns as soon as the process is
SPAWNED, which precedes the socket bind; polling closes that gap.  Each
attempt dials when the link is down (`agent-repl-uds-connect' in its
readiness-owned mode).  The UDS filter dispatches the connect snapshot; this
function never pumps Emacs process I/O or waits on the main thread.  ON-FAILURE
receives a diagnostic string after the bounded dial budget expires.

Returns `:pending' when readiness is asynchronous, otherwise `:ready'."
  (unless (and (functionp on-ready) (functionp on-failure))
    (error "agent-repl: frontend readiness requires callable continuations"))
  (let ((attempt 0) (started (float-time)) timer settled)
    (cl-labels
        ((finish (outcome &optional detail)
           (unless settled
             (setq settled t)
             (when (timerp timer) (cancel-timer timer))
             (agent-repl--log ws "frontend-ready: outcome=%s attempts=%d elapsed=%.3fs detail=%S"
                              outcome attempt (- (float-time) started) detail)
             (if (eq outcome 'ready)
                 (funcall on-ready)
               (message "agent-repl: frontend readiness failed: %s" detail)
               (funcall on-failure detail))))
         (tick ()
           (cond
            ((agent-repl--frontend-daemon-ready-p) (finish 'ready))
            ((>= attempt agent-repl-frontend-ready-attempts)
             (finish 'timeout (format "daemon at %s never became ready" agent-repl-uds-socket-path)))
            (t
             (setq attempt (1+ attempt))
             (unless (agent-repl--uds-connected-p) (agent-repl-uds-connect nil t))
             (setq timer (agent-repl--uds-run-timer 0.2 #'tick))))))
    (agent-repl--log
     ws
     "frontend-ready: begin connected=%s daemon-view=%s budget=%d"
     (agent-repl--uds-connected-p)
     (and (agent-repl--frontend-daemon-view) t)
     agent-repl-frontend-ready-attempts)
    (if (agent-repl--frontend-daemon-ready-p) (progn (finish 'ready) :ready)
      (tick) :pending))))

(defun agent-repl--frontend-async-fail (ws operation request-id started on-failure detail)
  "Log and surface OPERATION failure, then deliver DETAIL to ON-FAILURE."
  (agent-repl--log ws
                   "frontend-async: FAILED operation=%s request-id=%s elapsed=%.3fs detail=%s"
                   operation request-id (- (float-time) started) detail)
  (message "agent-repl: %s failed: %s" operation detail)
  (funcall on-failure detail))

(defun agent-repl--frontend-after-health-command
    (field payload workspace session-id what on-success on-failure &optional ws)
  "Asynchronously require a healthy FIELD response before ON-SUCCESS.
ON-FAILURE receives a diagnostic string for readiness failure, command
rejection, timeout, or an unhealthy correlated response.  The caller owns no
timer or UDS callback after either continuation runs."
  (unless (and (functionp on-success) (functionp on-failure))
    (error "agent-repl: %s health requires callable continuations" what))
  (let ((started (float-time)) request-id timer settled)
    (cl-labels
        ((finish (ok detail)
           (unless settled
             (setq settled t)
             (when (timerp timer) (cancel-timer timer))
             (when request-id
               (agent-repl--uds-untrack-command request-id workspace "health-settled")
               (agent-repl--uds-untrack-health-response request-id workspace "health-settled"))
             (if ok
                 (condition-case err
                     (progn
                       (agent-repl--log ws "frontend-health-async: HEALTHY what=%s field=%s request-id=%s elapsed=%.3fs"
                                        what field request-id (- (float-time) started))
                       (funcall on-success))
                   (error
                    (agent-repl--frontend-async-fail
                     ws what request-id started on-failure
                     (format "healthy continuation failed: %s"
                             (error-message-string err)))))
               (agent-repl--frontend-async-fail ws what request-id started on-failure detail))))
         (dispatch ()
           ;; `:on-registered' runs before the frame is written, so BOTH this
           ;; dispatch's correlation handles — the lexical REQUEST-ID that
           ;; `finish' untracks by, and the health-response registry — exist
           ;; before an ack or a health reply can be delivered reentrantly
           ;; from inside `process-send-string's yield.
           (agent-repl--uds-send-command
            field payload workspace nil
            :on-registered
            (lambda (id)
              (setq request-id id)
              (agent-repl--uds-track-health-response
               id field workspace session-id
               (lambda (response)
                 (if (plist-get response :healthy)
                     (finish t nil)
                   (finish nil (format "daemon reported unhealthy: %s"
                                       (or (plist-get response :reason) "no reason supplied")))))))
            :on-failure (lambda (err) (finish nil (format "command rejected: %s" err))))
           (agent-repl--log ws "frontend-health-async: dispatched what=%s field=%s request-id=%s workspace=%s session-id=%s"
                            what field request-id workspace session-id)
           (unless settled
             (setq timer (agent-repl--uds-run-timer
                          agent-repl-frontend-health-timeout
                          (lambda () (finish nil (format "timed out after %.3fs" agent-repl-frontend-health-timeout))))))))
      (agent-repl--frontend-after-ready #'dispatch
                                         (lambda (detail) (finish nil detail)) ws)
      :pending)))

(defun agent-repl--frontend-open-workspace-payload (key)
  "Return the `openWorkspace' payload carrying KEY's run preferences.
These are the account and permission posture a session the daemon STARTS
for this workspace runs under — properties of the checkout, which the
editor owns.  They are read but unused when the workspace already has a
session: an open never re-postures a live one.

Built from the same `agent-repl--frontend-session-posture' the explicit
create consumes, so the two entry points cannot offer the daemon
different postures for one workspace."
  (let ((posture (agent-repl--frontend-session-posture key)))
    (append (when (plist-get posture :config-dir)
              (list :configDir (plist-get posture :config-dir)))
            (when (plist-get posture :permission-mode)
              (list :permissionMode (plist-get posture :permission-mode)))
            (when (plist-get posture :allow-ungated) (list :allowUngated t)))))

(defun agent-repl--frontend-after-open-workspace (ws on-success on-failure)
  "Asynchronously open WS, calling exactly one continuation.
An open ESTABLISHES the workspace: the daemon reattaches to the session
it has, or starts one when it has none."
  (unless (and (functionp on-success) (functionp on-failure))
    (error "agent-repl: openWorkspace requires callable continuations"))
  (let ((started (float-time)) request-id timer settled (key (agent-repl--frontend-ws-command-key ws)))
    (cl-labels ((finish (ok detail)
                  (unless settled
                    (setq settled t)
                    (when (timerp timer) (cancel-timer timer))
                    (when request-id (agent-repl--uds-untrack-command request-id key "open-workspace-settled"))
                    (if ok
                        (progn (agent-repl--log ws "open-workspace-async: ACCEPTED ws=%s key=%s request-id=%s elapsed=%.3fs" ws key request-id (- (float-time) started))
                               (funcall on-success))
                      (agent-repl--frontend-async-fail ws "openWorkspace" request-id started on-failure detail)))))
      (agent-repl--frontend-after-ready
       (lambda ()
         (agent-repl--uds-send-command
          "openWorkspace" (agent-repl--frontend-open-workspace-payload key) key nil
          :on-registered (lambda (id) (setq request-id id))
          :on-failure (lambda (err) (finish nil (format "command rejected: %s" err)))
          :on-success (lambda () (finish t nil)))
         (unless settled
           (setq timer (agent-repl--uds-run-timer agent-repl-frontend-open-workspace-timeout
                                                  (lambda () (finish nil (format "timed out after %.3fs" agent-repl-frontend-open-workspace-timeout)))))))
       (lambda (detail) (finish nil detail)) ws)
      :pending)))

(defun agent-repl--frontend-after-create-session
    (cwd model resume-mode explicit-id force-fresh on-success on-failure &optional ws)
  "Asynchronously create CWD's session and await its pushed `SessionView'."
  (unless (and (stringp cwd) (not (string-empty-p cwd))
               (functionp on-success) (functionp on-failure))
    (error "agent-repl: createSession requires cwd and callable continuations"))
  ;; Session routing is exact-string keyed. Canonicalize before the command,
  ;; reservation, pushed-view lookup, and every retry so all four use the same
  ;; byte string as later workspace-routed commands.
  (setq cwd (agent-repl--path-canonical cwd))
  (when (gethash cwd agent-repl--frontend-creates-in-flight)
    (agent-repl--log ws "createSession-async: REFUSED cwd=%s already-in-flight" cwd)
    (error "agent-repl: a createSession for %s is already in flight" cwd))
  (let* ((ws (or ws (agent-repl--ws-name-for-dir cwd)))
         (started (float-time)) request-id deadline-timer poll-timer settled acked
         (resume-mode (or resume-mode 'continue))
         (force-fresh (or force-fresh agent-repl--force-fresh-conversation))
         (model (agent-repl--effective-model model))
         (posture (agent-repl--frontend-session-posture cwd))
         (payload (append (list :cwd cwd :resumeMode (agent-repl--frontend-resume-mode-wire resume-mode))
                          (when model (list :model model))
                          (when (eq resume-mode 'explicit) (list :explicitClaudeSessionId explicit-id))
                          (when (plist-get posture :config-dir) (list :configDir (plist-get posture :config-dir)))
                          (when (plist-get posture :permission-mode) (list :permissionMode (plist-get posture :permission-mode)))
                          (when (plist-get posture :allow-ungated) (list :allowUngated t)))))
    (cl-labels
        ((cleanup ()
           (when (timerp deadline-timer) (cancel-timer deadline-timer))
           (when (timerp poll-timer) (cancel-timer poll-timer))
           (when request-id (agent-repl--uds-untrack-command request-id cwd "create-settled"))
           (remhash cwd agent-repl--frontend-creates-in-flight))
         (finish (id detail)
           (unless settled
             (setq settled t)
             (cleanup)
             (if id
                 (progn (agent-repl--log ws "createSession-async: CREATED cwd=%s session-id=%s request-id=%s elapsed=%.3fs" cwd id request-id (- (float-time) started))
                        (funcall on-success id))
               (agent-repl--frontend-async-fail ws "createSession" request-id started on-failure detail))))
         (reject (err)
           (unless settled
             (setq settled t)
             (cleanup)
             (if (and (stringp err) explicit-id
                      (string-match-p "has no transcript" err))
                 (if force-fresh
                     (progn
                       (agent-repl--log ws
                                        "createSession-async: lost transcript resume=%s force-fresh=t cwd=%s"
                                        explicit-id cwd)
                       (agent-repl--frontend-after-create-session
                        cwd model 'fresh nil nil on-success on-failure ws))
                   (let ((detail
                          (format (concat "resume target %s has no transcript; refusing a fresh "
                                          "conversation")
                                  explicit-id)))
                     (agent-repl--log ws
                                      "createSession-async: resume refused resume=%s force-fresh=nil cwd=%s detail=%s"
                                      explicit-id cwd detail)
                     (agent-repl--frontend-async-fail
                      ws "createSession" request-id started on-failure detail)))
               (agent-repl--frontend-async-fail
                ws "createSession" request-id started on-failure
                (format "command rejected: %s" (or err "no reason supplied"))))))
         (observe-view ()
           ;; The create is correlated by CWD, which is what the command was
           ;; keyed by: the daemon delivers the new session as the pushed
           ;; SessionView for that workspace, and the id this reports is read
           ;; out of that view rather than tracked by this end.
           (when acked
             (when-let ((view (agent-repl--frontend-session-view cwd)))
               (unless (eq (plist-get view :terminal) t)
                 (finish (plist-get view :sessionId) nil)))))
         (poll-view ()
           (observe-view)
           (unless settled (setq poll-timer (agent-repl--uds-run-timer 0.05 #'poll-view))))
         (dispatch ()
           (agent-repl--uds-send-command
            "createSession" payload cwd nil
            :on-registered (lambda (id) (setq request-id id))
            :on-failure #'reject
            :on-success (lambda () (setq acked t) (observe-view)))
           (agent-repl--log ws "createSession-async: dispatched cwd=%s request-id=%s resume-mode=%s model=%s" cwd request-id resume-mode model)
           (unless settled
             (setq deadline-timer (agent-repl--uds-run-timer agent-repl-frontend-create-timeout
                                                             (lambda () (finish nil (format "timed out after %.3fs awaiting acknowledgement and SessionView" agent-repl-frontend-create-timeout)))))
             (setq poll-timer (agent-repl--uds-run-timer 0.05 #'poll-view)))))
      ;; Reserve the cwd before readiness polling.  Two UI actions issued while
      ;; the daemon is still starting must not arm two future creates.
      (puthash cwd t agent-repl--frontend-creates-in-flight)
      (agent-repl--frontend-after-ready #'dispatch (lambda (detail) (finish nil detail)) ws)
      :pending)))

(defun agent-repl--frontend-after-ensure-session (ws on-success on-failure &optional purpose)
  "Asynchronously establish WS, then call exactly one continuation.
ON-SUCCESS takes no arguments: establishment is a fact about the
WORKSPACE, and which session the daemon has bound to it is the daemon\='s
to know.  Emacs routes everything by WS\='s workspace key.

PURPOSE is `presentation' or `send'.  A presentation reopens an existing
workspace before delivering it; a send dispatches directly because
`submitPrompt' performs the daemon-side establishment itself."
  (unless (and (functionp on-success) (functionp on-failure))
    (error "agent-repl: ensure-session requires callable continuations"))
  (if (not (agent-repl--ensure-frontend-daemon))
      (progn
        (agent-repl--frontend-async-fail ws "ensure-session" nil (float-time) on-failure
                                         "frontend daemon not started (auto-start disabled or init inhibited)")
        :failed)
    (let ((gated (not (eq purpose 'send))))
      (unless (agent-repl--ws-get ws :project-dir)
        (let ((dir (agent-repl--resolve-current-git-root)))
          (agent-repl--ws-put ws :project-dir dir)
          (unless (agent-repl--ws-get ws :active-env)
            (agent-repl--initialize-ws-env ws dir))))
      (agent-repl--frontend-after-ready
       (lambda ()
         (if gated
             (progn
               (agent-repl--ws-put ws :reattach-failed nil)
               (agent-repl--ws-put ws :reattach-failures nil)
               (agent-repl--frontend-reattach-timer-start)
               (agent-repl--frontend-after-open-workspace ws on-success on-failure))
           (funcall on-success)))
       on-failure ws)
      :pending)))

;;;; ---- Session CRUD -------------------------------------------------------

(defcustom agent-repl-frontend-create-timeout 30
  "Seconds `agent-repl--frontend-after-create-session' awaits its UDS outcome.
createSession's `CommandAck' is the daemon's ESTABLISHMENT verdict: it is
written only once the new session's shim answers a health probe healthy over
the fully wired connection, so the ack legitimately takes as long as the
bring-up does.

This MUST stay above the daemon's own establishment bound (20s,
`createEstablishTimeout' in daemon/internal/server/createestablish.go).
Whichever bound fires first is the one the user reads, and the daemon's nack
names the link that is still pending while a client-side timeout can only
say the daemon went quiet."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-frontend-health-timeout 10
  "Seconds to await a correlated daemon or session health result over UDS."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-frontend-open-workspace-timeout 30
  "Seconds to await an `openWorkspace' acknowledgement.
Opening the Agent REPL is a presentation gate: the daemon must finish
ensuring the workspace's existing session controller before Emacs mounts
or redisplays its webview.  A timeout therefore aborts presentation
rather than leaving a hibernated session looking open."
  :type 'number
  :group 'agent-repl)

(defvar agent-repl--frontend-creates-in-flight (make-hash-table :test 'equal)
  "Set of cwds with a `createSession' command awaiting its ack.
Serializes concurrent creates per workspace so the SessionView->id
correlation stays unambiguous: a second create for the same cwd while
one is in flight is refused loudly rather than racing two creates onto
one workspace.")

(defun agent-repl--frontend-after-daemon-healthy (on-success on-failure)
  "Asynchronously require the daemon's initialization-readiness assertion.
This proves the daemon completed its startup assembly and bound its
boot-critical listeners.  It does not probe shim-store or the sidecar:
session health separately proves the live daemon -> shim route and the shim's
own dependencies, including shim-store; startup service orchestration
separately validates and kickstarts the launchd jobs."
  (agent-repl--frontend-after-health-command
   "daemonHealth" nil nil nil "daemon" on-success on-failure))

(defun agent-repl--frontend-after-session-healthy
    (ws session-id on-success on-failure)
  "Asynchronously assert WS's live shim health and identity for SESSION-ID.
The daemon routes the command by WS's project directory and verifies that
its live shim is connected and healthy.  SESSION-ID correlation prevents a
stale binding from being reported after a restart or remount.

DIAGNOSTIC, NOT A BRING-UP GATE.  This used to run before every webview
mount, because `createSession' acked as soon as a spawn was issued and the
mount had no other way to know the shim was up — a create-then-poll shape
that lost the very races it existed to cover.  The daemon now acks a create
only once the session is ESTABLISHED, so nothing on the bring-up path asks
this question any more; it survives as `agent-repl-session-health', the
answer to \"is this already-open session's shim still there\"."
  (unless (and (stringp session-id) (not (string-empty-p session-id)))
    (agent-repl--log ws
                     "frontend-health: invalid session id for session health id=%S"
                     session-id)
    (error "agent-repl: cannot health-check workspace %s without a session id" ws))
  (agent-repl--frontend-after-health-command
   "sessionHealth" (list :sessionId session-id)
   (agent-repl--frontend-ws-command-key ws)
   session-id
   (format "session ws=%s id=%s" ws session-id)
   on-success on-failure ws))

;;;###autoload
(defun agent-repl-session-health (&optional ws)
  "Report whether WS's already-open session still has a healthy live shim.
WS defaults to the current workspace.  Interactive diagnostic only: bring-up
no longer probes health (the `createSession' ack proves establishment), so
this exists for the case the ack cannot speak to — a session that came up
fine and may have lost its shim since.

Signals with the daemon's own reason when the session is unhealthy,
unreachable, or has no recorded id."
  (interactive)
  (let* ((ws (or ws (agent-repl--ws-current-name)))
         ;; sessionHealth is the one command the daemon keys by session rather
         ;; than by workspace, so the id is read out of the view the daemon
         ;; pushed for this workspace and handed straight back on the wire.
         (session-id (and ws (plist-get (agent-repl--frontend-session-view
                                         (agent-repl--frontend-ws-command-key ws))
                                        :sessionId))))
    (unless ws
      (user-error "agent-repl: no current workspace to health-check"))
    (unless (and (stringp session-id) (not (string-empty-p session-id)))
      (user-error "agent-repl: workspace %s has no daemon session to health-check" ws))
    (agent-repl--frontend-after-session-healthy
     ws session-id
     (lambda ()
       (agent-repl--log ws "session-health command: HEALTHY session-id=%s" session-id)
       (message "agent-repl: %s session %s is healthy" ws session-id))
     (lambda (detail)
       (agent-repl--log ws "session-health command: FAILED session-id=%s detail=%s"
                        session-id detail)))
    :pending))

(defconst agent-repl--frontend-resume-modes
  '((continue . "RESUME_MODE_CONTINUE")
    (fresh    . "RESUME_MODE_FRESH")
    (explicit . "RESUME_MODE_EXPLICIT"))
  "Map of `agent-repl--frontend-after-create-session' RESUME-MODE to its wire name.
Deliberately has no entry for the proto's `RESUME_MODE_UNSPECIFIED': that
value exists so an older peer's absent field reads as `continue', and a
caller here always knows which of the three it means.")

(defun agent-repl--frontend-resume-mode-wire (mode)
  "Return the protojson enum name for resume MODE.
Signals on an unknown MODE rather than defaulting: a typo silently
becoming `continue' would resume a conversation a caller asked to replace,
and one silently becoming `fresh' would strand an intact one."
  (or (alist-get mode agent-repl--frontend-resume-modes)
      (error "agent-repl: unknown resume mode %S (want one of %S)"
             mode (mapcar #'car agent-repl--frontend-resume-modes))))

(defun agent-repl--frontend-delete-session (id &optional ws)
  "Send a `deleteSession' UDS command for session ID; return the request-id.
WS, when known, keys the frame + logging.
Fire-and-forget: the terminal `SessionView' the daemon pushes updates the
store, and a rejected ack surfaces loudly via the shared ack handler."
  (let ((req (agent-repl--uds-send-command "deleteSession" (list :sessionId id) ws)))
    (agent-repl--log ws "deleteSession: dispatched session=%s request-id=%s" id req)
    req))

;; The GET /commands slash-menu fetch + POST /commands/refresh re-resolve
;; were deleted in the S9 cutover: the slash-command menu is now the pushed
;; `SessionInitView' (retained `SystemInit'), read off frontend-state.el's
;; session-init store by input.el's completion — no HTTP round-trip.

(defun agent-repl--frontend-turn-active-sessions ()
  "Return the workspaces whose latest daemon state is mid-turn.
This daemon-stop guard delegates to
`agent-repl--frontend-all-turn-active-workspaces' so startup and explicit
stop cannot form two opinions from different caches."
  (agent-repl--frontend-all-turn-active-workspaces))

(defun agent-repl--frontend-all-turn-active-workspaces ()
  "Return every workspace with a live session whose daemon state is turn-active.
Correlates the two daemon-owned collections by WORKSPACE PATH, which is
the key both are stored under: a turn-active `WorkspaceState' counts when
the same path also has a non-terminal `SessionView'.

No session id is compared, and none could be: a lifecycle-backed
WorkspaceState row can name the vendor UUID while the SessionView beside
it names the daemon's own identity for the session.  The workspace path
is the one key both agree on.

This covers unrestored workspace paths too, protecting every real turn."
  (let ((states (agent-repl--frontend-workspace-state-views-all))
        busy
        active-workspaces
        unmatched)
    (dolist (state states)
      (when (eq (plist-get state :turnActive) t)
        (let* ((workspace (plist-get state :workspace))
               (live (agent-repl--frontend-workspace-session-live-p workspace)))
          (push workspace active-workspaces)
          (if live
              (push workspace busy)
            (push workspace unmatched))
          (agent-repl--log-verbose
           (agent-repl--frontend-ws-name workspace)
           "frontend turn-active correlate: workspace=%S live-session=%s"
           workspace (if live "t" "nil")))))
    (setq busy (sort (delete-dups busy) #'string<))
    (agent-repl--log
     nil
     "frontend turn-active probe: source=workspace-state+session-roster state-count=%d active-workspaces=%S busy=%S unmatched=%S"
     (length states) (nreverse active-workspaces) busy (nreverse unmatched))
    busy))

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

(defun agent-repl--frontend-workspace-url (workspace)
  "Return the webapp URL that renders WORKSPACE.
WORKSPACE is an absolute directory path: the same `workspace' wire key
the daemon routes that workspace's frames and commands by
\(`agent-repl--frontend-ws-command-key'), URL-encoded into the query
rather than hashed — the daemon's connection scope, every
`FrontendCommand', and this URL then all carry one greppable key.

The webapp holds no session identity behind this URL; the daemon rules
on which session the workspace owns and re-pushes the answer.  So the
URL outlives every session the workspace runs: a rotation, a supersede,
or a daemon bounce leaves it addressing the same thing."
  (format "%s/?workspace=%s"
          (agent-repl--frontend-base-url)
          (url-hexify-string workspace)))

(defun agent-repl--frontend-session-url (session-id)
  "Return the webapp URL that attaches to SESSION-ID alone.
For a viewer that has a session and NO workspace to render — the
config explainer's popup, whose session is global and rooted outside any
workspace.  A workspace's webview addresses itself by workspace
\(`agent-repl--frontend-workspace-url')."
  (format "%s/?session=%s" (agent-repl--frontend-base-url) session-id))

;;;; ---- Daemon-bounce resilience: the reattach loop -----------------------
;;
;; The daemon may be bounced at ANY time by agents deploying builds —
;; that is policy, not an accident (see AGENTS.md "Daemon bounce
;; policy").  A live shim is daemon-memory-resident, so after a bounce the
;; new instance is driving nothing, whatever its durable records say.  This
;; loop is the client half of the contract: ensure every live workspace, so
;; the daemon reattaches the session each one has (resume + transcript
;; replay brings the conversation back) or starts one it does not.
;;
;; It names no session.  "Is this workspace wired to the daemon answering
;; right now" is the whole question, and the daemon owns the answer to
;; "which session" — which is why a bounce needs no detection here beyond
;; the ensure itself.  When an ensure REPEATEDLY fails against a daemon that
;; answers (the breaking-API case), the give-up in
;; `agent-repl--frontend-note-ensure-failure' stops the retries and surfaces
;; the failure loudly instead of spinning forever.

(defcustom agent-repl-frontend-reattach-interval 15
  "Seconds between reattach sweeps over live gui workspaces."
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
No-op in batch (`agent-repl--frontend-init-inhibited-p') — the same
environment that never auto-starts the daemon."
  (cond
   (agent-repl--frontend-reattach-timer
    (agent-repl--log-verbose nil "reattach: timer already armed=%S"
                             agent-repl--frontend-reattach-timer))
   ((agent-repl--frontend-init-inhibited-p)
    (agent-repl--log-verbose nil "reattach: timer suppressed because init is inhibited"))
   (t
    (setq agent-repl--frontend-reattach-timer
          (run-with-timer agent-repl-frontend-reattach-interval
                          agent-repl-frontend-reattach-interval
                          #'agent-repl--frontend-reattach-check))
    (agent-repl--log nil "reattach: timer armed interval=%ss"
                     agent-repl-frontend-reattach-interval))))

(defun agent-repl--frontend-reattach-check ()
  "Recover EVERY live gui workspace by ensuring it.

ONE ARM, because there is one question: is this workspace WIRED to the
daemon that is answering right now?  `agent-repl--frontend-ensure-workspace'
asks exactly that (`agent-repl--frontend-ensure-skip-reason' consults
`shim_attached', the non-durable half of the pushed `SessionView'), and its
open both reattaches a session the daemon has and starts one it does not.
So a workspace the daemon is already driving costs nothing and every other
workspace recovers, without this sweep forming any opinion of its own about
which session a workspace should be on.

When the UDS link is DOWN, ensure the daemon (spawn or adopt) so the next
sweep can recover once the link and its snapshot return.

DRIVEN BY THE RECONNECT, NOT ONLY BY ITS TIMER.  The 15s sweep is the
floor; `agent-repl-uds-snapshot-applied-functions' runs this the instant a
reconnect's snapshot lands, so the recovery happens as soon as it is
possible to happen instead of up to one sweep interval later.

Boot-instance detection rides the pushed `DaemonView'
\(`agent-repl--frontend-apply-daemon-view' -> `agent-repl--frontend-note-boot-id'):
a bounce drops the UDS link, and the reconnect snapshot carries the new
boot id, which is what resets each workspace's give-up."
  (agent-repl--log-verbose nil "reattach: sweep connected=%s live-workspaces=%d"
                           (agent-repl--uds-connected-p)
                           (length (agent-repl--live-ws-names)))
  (if (not (agent-repl--uds-connected-p))
      (when (agent-repl--live-ws-names)
        (agent-repl--log nil "reattach: UDS link down with live workspaces — ensuring daemon")
        (condition-case err
            (agent-repl--ensure-frontend-daemon)
          (error
           (agent-repl--log nil "reattach: daemon ensure failed: %s"
                            (error-message-string err)))))
    (dolist (ws (agent-repl--live-ws-names))
      (agent-repl--frontend-ensure-workspace ws))))

(defun agent-repl--frontend-recover-after-reconnect ()
  "Recover every workspace and retract the outage notices, on reconnect.

Subscriber for `agent-repl-uds-snapshot-applied-functions': the edge at
which the daemon's state of the world has landed, and therefore the first
instant at which either half of this is answerable.

THE ORDER IS LOAD-BEARING.  Recovery runs FIRST and the retraction second,
so the notices come down over a recovery that has already been driven —
never over one this end has merely decided to attempt.  A workspace the
new instance still cannot open re-raises its own notice from
`agent-repl--frontend-note-ensure-failure', which is the honest outcome:
the outage is over, and what is left is a workspace-specific fault that
deserves to be said in its own words."
  (agent-repl--frontend-reattach-check)
  (agent-repl-connection-notices-retract "daemon-reconnected"))

(add-hook 'agent-repl-uds-snapshot-applied-functions
          #'agent-repl--frontend-recover-after-reconnect)

(defun agent-repl--frontend-note-boot-id (boot-id)
  "Record BOOT-ID; on an instance change, reset every reattach give-up.
A give-up (`:reattach-failed') binds a failure history to ONE daemon
instance — a fresh instance deserves fresh attempts.  Old daemons that
predate boot ids report nil, which never triggers a reset."
  (cond
   ((null boot-id)
    (agent-repl--log-verbose nil "reattach: daemon view has no boot id"))
   ((equal boot-id agent-repl--frontend-last-boot-id)
    (agent-repl--log-verbose nil "reattach: daemon boot id unchanged=%s" boot-id))
   (t
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
        (when (agent-repl--ws-get ws :ensure-failed)
          (agent-repl--ws-put ws :ensure-failed nil)
          (agent-repl--ws-put ws :ensure-failures nil))
        (agent-repl--ws-put ws :ensure-at nil)))
    (unless agent-repl--frontend-last-boot-id
      (agent-repl--log nil "reattach: recording initial daemon boot id=%s" boot-id))
    (setq agent-repl--frontend-last-boot-id boot-id))))

(defun agent-repl--frontend-rebind-workspaces-after-restart
    (&optional on-success on-failure)
  "Bounce every open gui workspace's shim onto the freshly restarted daemon.
Meant to run right after `agent-repl-frontend-daemon-restart' force-bounces
the daemon: rather than leaving each open panel dark until the next reattach
sweep timer fires (up to `agent-repl-frontend-reattach-interval' away), this
drives the reattach IMMEDIATELY so every workspace is good to go the moment
the restart returns.

Waits for the new daemon to answer after its reconnect snapshot has already
run `agent-repl--frontend-recover-after-reconnect'.  That snapshot hook is the
single owner of session recovery.  Launching the same sweep again here races
its asynchronous creates and violates their one-create-per-workspace
invariant.  This completion edge therefore remounts every live webview after
the snapshot-owned recovery was driven, then reports the count of open
workspaces that carried a session binding to rebind."
  (let ((n (length (agent-repl--live-ws-names))))
    (agent-repl--log nil "reattach: explicit rebind begin bound-workspaces=%d" n)
    (agent-repl--frontend-after-ready
     (lambda ()
      (agent-repl--log nil
                       "reattach: explicit rebind recovery-owner=snapshot-hook bound-workspaces=%d"
                       n)
      ;; The snapshot hook drives each workspace's daemon session, but no
      ;; webview is remounted for it: a webview addresses its workspace, so a
      ;; bounce leaves every mounted URL naming the same thing and the pages
      ;; keep rendering the pre-bounce BUNDLE.  Force a remount of EVERY open
      ;; webview so a bounce reliably
      ;; reloads the served bundle across the board — a bounce is exactly
      ;; when a fresh build lands, and each remount replays history off the
      ;; live session, so nothing is lost.
      (agent-repl--frontend-remount-all-webviews)
      (agent-repl--log nil "reattach: explicit rebind complete remounted-workspaces=%d" n)
      (when on-success (funcall on-success n)))
     (lambda (detail)
       (agent-repl--log nil "reattach: explicit rebind FAILED detail=%s" detail)
       (when on-failure (funcall on-failure detail))))
    :pending))

;; The in-flight message-queue plane (§2.13) is fully retired.  It was dead
;; server-side (the post-cutover daemon carries no `queue' array and
;; frontend.v1 has no queue field or queue-control command), so the Emacs
;; `queue-run-now'/`queue-cancel' HTTP override routes and the
;; perpetually-empty `:queued-messages' accessors were deleted in the S9
;; endgame — the webapp owns the queued-message UI end to end.

(defun agent-repl--gui-send-turn (ws input raw prompt-origin &optional on-settle)
  "The gui frontend's send capability (registry `:send-fn').
INPUT (the prepared text, which may carry an on-demand read-directive —
genuine message content) goes to the daemon session.  There is no
owning-workspace pin to apply here — WS's daemon session id already
identifies the target unambiguously, unlike a shared vterm buffer that
once needed disambiguating.  Posthooks and prompt summary key on RAW,
identically.

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
  (agent-repl--log ws "do-send[gui] ws=%s len=%d prompt-origin=%s" ws (length input) prompt-origin)
  (agent-repl--frontend-send-user-message
   ws input prompt-origin
   (lambda (request-id)
     (agent-repl--frontend-snap-webview-to-tail ws)
     (agent-repl--mark-ws-thinking ws)
     (agent-repl--ws-put ws :last-prompt-time (float-time))
     (agent-repl--ws-put ws :sent-turn (list :request-id request-id :raw raw))
     (agent-repl--run-send-posthooks ws raw)
     (agent-repl--kickoff-prompt-summary ws raw)
     (agent-repl--log ws "do-send[gui]: dispatched request-id=%s raw-len=%d"
                      request-id (length raw))
     (when on-settle (funcall on-settle)))
   (lambda (detail)
     (agent-repl--log ws "do-send[gui]: FAILED before dispatch detail=%s" detail)
     (when on-settle (funcall on-settle))))
  :pending)

(defun agent-repl--gui-interrupt (ws kind)
  "The gui frontend's interrupt capability (registry `:interrupt-fn').
Sends the UDS `interrupt' command keyed by WS's cwd
\(`agent-repl--frontend-ws-command-key' — the daemon resolves that cwd ->
session).  KIND (`escape' = `C-c C-k' STOP, `ctrl-c' = `C-c C-c' clear
draft) no longer changes the wire request: frontend.v1's `InterruptCmd'
carries only `confirm_agents' (the answer to the daemon's
interrupt-confirmation challenge) and NO retract id, so the retract half
of the old `C-c C-k' undo is gone (it was already a daemon no-op
post-cutover — the daemon's HTTP interrupt always reported
retracted=false).  Always returns t: the interrupt is dispatched, never
`retracted'.

The first send always ASKS (confirm_agents unset).  When no turn is live
but subagent tasks are, the daemon answers with the
`interrupt_confirm_required' CHALLENGE instead of interrupting, and
`agent-repl--gui-interrupt-challenge' puts the question to the user; a
yes resends the same command with `confirmAgents' set.  So the return
value reports DISPATCH, never the eventual interrupt outcome — the
confirmation round trip completes long after this returns."
  (let ((key (agent-repl--frontend-ws-command-key ws)))
    ;; nil payload -> the daemon reads InterruptCmd{} (confirm_agents=false),
    ;; a plain stop of the live turn.
    (agent-repl--uds-send-command
     "interrupt" nil key nil
     :on-challenge
     (lambda (challenge) (agent-repl--gui-interrupt-challenge ws key challenge)))
    (agent-repl--log ws "interrupt[gui]: ws=%s kind=%s (uds interrupt)" ws kind)
    t))

(defun agent-repl--frontend-restart-session (ws)
  "Send the `restartSession\=' command for WS, keyed by its cwd.
Returns the request-id.  Signals (via `agent-repl--uds-send-command\=') when
there is no link to send on; a REJECTED ack is surfaced loudly through the
shared ack handler, which is the whole point of tracking it: a restart that
failed must never read as a session that came back."
  (let ((key (agent-repl--frontend-ws-command-key ws))
        ;; Bound by `:on-registered' BEFORE the write, so the ack callbacks
        ;; below can name the request even when the ack is delivered
        ;; reentrantly from inside `process-send-string'.
        (req nil))
    (agent-repl--uds-send-command
     "restartSession" nil key nil
     :on-registered (lambda (id) (setq req id))
     :on-failure
     (lambda (err)
       (agent-repl--log ws "restart-session: ws=%s REJECTED: %s" ws err))
     :on-success
     (lambda ()
       (agent-repl--log ws "restart-session: ws=%s complete request-id=%s" ws req)
       (message "agent-repl: session restarted (same conversation, fresh shim)")))
    (agent-repl--log ws "restart-session: dispatched ws=%s key=%s request-id=%s" ws key req)
    req))

(defun agent-repl--frontend-hibernate-workspace (ws)
  "Send the `hibernateWorkspace\=' command for WS, keyed by its cwd.
Returns the request-id.  Signals (via `agent-repl--uds-send-command\=') when
there is no link to send on.

The ack is TRACKED for the same reason the restart\='s is, and the daemon
nacks more often here: hibernation is refused outright while a turn is
live or the merge lease is held, because the daemon never discards
in-flight work to satisfy a hibernate.  A refusal that read as success
would leave the user believing they had reclaimed memory the machine is
still holding, so the rejection is surfaced loudly through the shared ack
handler and echoed here as well."
  (let ((key (agent-repl--frontend-ws-command-key ws))
        (req nil))
    (agent-repl--uds-send-command
     "hibernateWorkspace" nil key nil
     :on-registered (lambda (id) (setq req id))
     :on-failure
     (lambda (err)
       (agent-repl--log ws "hibernate-workspace: ws=%s REJECTED: %s" ws err)
       (message "agent-repl: hibernate refused for %s: %s" ws err))
     :on-success
     (lambda ()
       (agent-repl--log ws "hibernate-workspace: ws=%s complete request-id=%s" ws req)
       (message "agent-repl: %s hibernated (session reclaimable, conversation kept)" ws)))
    (agent-repl--log ws "hibernate-workspace: dispatched ws=%s key=%s request-id=%s" ws key req)
    req))

(defun agent-repl--gui-interrupt-live-task-count (challenge)
  "Read the live subagent count off an `InterruptConfirmRequired' CHALLENGE.
protojson renders int64 as a STRING, so `liveTasks' arrives as \"3\" from
the daemon and as 3 from a hand-built plist; both are accepted.  Anything
else reads as 0 — an unknown count still gets a question, just a
countless one, rather than a fabricated number."
  (let ((raw (plist-get challenge :liveTasks)))
    (cond ((integerp raw) raw)
          ((and (stringp raw) (string-match-p "\\`[0-9]+\\'" raw))
           (string-to-number raw))
          (t 0))))

(defun agent-repl--gui-interrupt-challenge (ws key challenge)
  "Answer the daemon's interrupt confirmation CHALLENGE for WS.
CHALLENGE is the ack's `InterruptConfirmRequired' payload; KEY is the
workspace wire key the original command went out on.  Puts the stakes to
the user in the minibuffer and, on a yes, RESENDS the interrupt with
`confirmAgents' set — the only thing that makes the daemon act.  On a no
nothing further goes out; the decline is logged, since a swallowed
question is indistinguishable from a lost command.

The resend is tracked WITHOUT a challenge handler: a confirmed interrupt
that is challenged again is a daemon-side contradiction, not something to
re-ask, and re-arming here would loop the prompt."
  (let* ((live (agent-repl--gui-interrupt-live-task-count challenge))
         (question (if (> live 0)
                       (format "Interrupt %d running subagent%s? "
                               live (if (= live 1) "" "s"))
                     "Interrupt the running subagents? ")))
    (agent-repl--log ws "interrupt[gui]: CHALLENGE ws=%s live-tasks=%d — asking" ws live)
    (if (y-or-n-p question)
        (let ((req (agent-repl--uds-send-command
                    "interrupt" (list :confirmAgents t) key)))
          (agent-repl--log ws "interrupt[gui]: CONFIRMED ws=%s live-tasks=%d request-id=%s"
                           ws live req)
          t)
      (agent-repl--log ws "interrupt[gui]: DECLINED ws=%s live-tasks=%d — nothing resent"
                       ws live)
      nil)))

(defun agent-repl--gui-running-p (ws)
  "The gui frontend's liveness capability (registry `:running-p-fn').
Cheap check: the daemon has pushed a non-terminal `SessionView' for WS's
workspace.  Whether that session is DRIVEABLE right now is a different
question, probed (and healed) lazily by the send path's ensure.

A workspace with no project dir has no wire key to ask about and reads as
not running."
  (and (agent-repl--ws-get ws :project-dir)
       (agent-repl--frontend-workspace-session-live-p
        (agent-repl--frontend-ws-command-key ws))
       t))

(defun agent-repl--gui-durable-session-id (ws)
  "The gui frontend's durable-id capability.
Fetches the daemon-captured claude_session_id for WS's bound session;
nil when unbound, not yet initialized, or the daemon is unreachable
\(logged — a dead daemon degrades a frontend switch to a fresh
conversation rather than aborting it)."
  ;; Read the durable id off the pushed `SessionView' store (the daemon
  ;; stamps `claudeSessionId' once the CLI reports it), keyed by the same
  ;; workspace every command from WS is routed by.  Absent until the first
  ;; frame lands — nil then, exactly as an unreachable daemon degrades.
  (plist-get (agent-repl--frontend-session-view
              (agent-repl--frontend-ws-command-key ws))
             :claudeSessionId))

(defun agent-repl--gui-adopt-session
    (ws claude-session-id on-success on-failure)
  "The gui frontend's adopt capability: resume CLAUDE-SESSION-ID.
Creates a fresh daemon session with resume set and binds it to WS, so
the subsequent open attaches to the continued conversation."
  (unless (and (functionp on-success) (functionp on-failure))
    (error "agent-repl: gui adoption requires callable continuations"))
  (if (not (agent-repl--ensure-frontend-daemon))
      (agent-repl--frontend-async-fail
       ws "gui-adopt-session" nil (float-time) on-failure
       "frontend daemon not started (auto-start disabled or init inhibited)")
    (let ((dir (or (agent-repl--ws-get ws :project-dir)
                   (agent-repl--resolve-current-git-root))))
      (agent-repl--frontend-after-create-session
       dir (agent-repl--ws-get ws :model) 'explicit claude-session-id nil
       (lambda (id)
         (agent-repl--log ws "gui adopted claude session %s as %s"
                          claude-session-id id)
         (funcall on-success id))
       on-failure ws)))
  :pending)

(defun agent-repl--frontend-force-fresh-session (ws on-success on-failure)
  "Asynchronously create and bind a FRESH daemon session for WS.
Mirrors `agent-repl--gui-adopt-session' but passes NO resume, so a BLANK
conversation replaces whatever the normal ensure path would replay.
Resets the reattach failure markers so the fresh binding reads as healthy.
ON-SUCCESS takes no arguments.  The fresh session captures its own
durable id through the usual hook path once it runs, so a later resume
continues the fresh conversation rather than the discarded one."
  (unless (and (functionp on-success) (functionp on-failure))
    (error "agent-repl: force-fresh requires callable continuations"))
  (if (not (agent-repl--ensure-frontend-daemon))
      (agent-repl--frontend-async-fail
       ws "force-fresh-conversation" nil (float-time) on-failure
       "frontend daemon not started (auto-start disabled or init inhibited)")
    (let ((dir (or (agent-repl--ws-get ws :project-dir)
                   (agent-repl--resolve-current-git-root))))
      (agent-repl--frontend-after-create-session
       dir (agent-repl--ws-get ws :model) 'fresh nil nil
       (lambda (id)
         (agent-repl--ws-put ws :reattach-failed nil)
         (agent-repl--ws-put ws :reattach-failures nil)
         (agent-repl--log ws "force-fresh-conversation: fresh session %s bound (cwd=%s)"
                          id dir)
         (funcall on-success))
       on-failure ws)))
  :pending)

(defun agent-repl--frontend-send-user-message
    (ws text prompt-origin on-success on-failure)
  "Send TEXT as WS's user turn over the UDS `submitPrompt' command.
Ensures the session first (recreating a stale binding), so a send into a
dead session heals instead of failing, then sends `submitPrompt' keyed by
WS's cwd (`agent-repl--frontend-ws-command-key') — the daemon resolves that
cwd -> session, so no session id is on the wire.  ON-SUCCESS receives the
command request id.  ON-FAILURE receives the ensure failure detail.

FIRE AND FORGET, end to end.  The ensure runs with purpose `send' so it
skips the `openWorkspace' presentation gate, and the command itself is
written and tracked without awaiting its ack, so no step between the
user's key and the wire blocks the Emacs main thread.  The ack still
arrives asynchronously and still surfaces loudly on rejection.

The one-shot `:next-send-origin' tag (set by the merge remediation) is
consumed and cleared here but is NOT forwarded: frontend.v1's
`SubmitPromptCmd' has no `origin' field, so the merge status-card stamping
is gone — matching the already-dead server behavior (the retired HTTP
/message route never read `origin' into the session controller either)."
  (unless (and (functionp on-success) (functionp on-failure))
    (error "agent-repl: frontend send requires callable continuations"))
  (unless (and (stringp prompt-origin)
               (string-prefix-p "PROMPT_ORIGIN_" prompt-origin)
               (not (equal prompt-origin "PROMPT_ORIGIN_UNSPECIFIED")))
    (agent-repl--log ws "frontend send: invalid prompt-origin=%S" prompt-origin)
    (error "agent-repl: frontend send requires an explicit prompt origin"))
  (agent-repl--frontend-after-ensure-session
   ws
   (lambda ()
     (let ((origin (agent-repl--ws-get ws :next-send-origin)))
       (when origin (agent-repl--ws-put ws :next-send-origin nil))
       (agent-repl--log ws "frontend send: len=%d origin=%s prompt-origin=%s (uds submitPrompt)"
                        (length text) origin prompt-origin)
       (let ((req (agent-repl--uds-send-command
                   "submitPrompt" (list :text text :promptOrigin prompt-origin)
                   (agent-repl--frontend-ws-command-key ws))))
         (agent-repl--log ws "frontend send: dispatched request-id=%s" req)
         (funcall on-success req))))
   on-failure 'send)
  :pending)

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
;;   - `Ensure' -> `sessioncontroller.Manager.bringUp' takes the manager mutex,
;;     finds the workspace in `byWS' and returns the live session controller
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

(defcustom agent-repl-frontend-ensure-cooldown 30
  "Seconds before `openWorkspace' may be re-sent for one workspace.
Only reached when the workspace still has no live session: a workspace
that already has one skips the send outright (nothing to ensure).  The
cooldown bounds the remaining case — a workspace the daemon cannot bring
up, re-driven by every switch, every reconnect and every reattach sweep —
so the daemon is not made to rescan its projects directory per attempt."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-frontend-ensure-max-failures 3
  "Consecutive ensure failures after which a workspace gives up.
A give-up sets `:ensure-failed', surfaces ONE warning, and stops
sending until the daemon instance changes (see
`agent-repl--frontend-note-boot-id') — the same give-up shape the reattach
sweep uses, so a workspace the daemon simply cannot open never retry-loops."
  :type 'integer
  :group 'agent-repl)

(defun agent-repl--frontend-backfill-settled-p (workspace)
  "Return non-nil when WORKSPACE's history has finished arriving (F2).
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
  (let ((state (plist-get (agent-repl--frontend-session-view workspace) :backfill)))
    (or (null state)
        (member state '("BACKFILL_STATE_DONE" "BACKFILL_STATE_UNSPECIFIED")))))

(defun agent-repl--frontend-session-controller-live-p (workspace)
  "Return non-nil when the daemon holds a LIVE SESSION CONTROLLER for WORKSPACE.
Reads `SessionView.shim_attached\=' off the pushed-frame store — the one
field on that message that is NOT read back from the durable registry
record.

WHY LIVENESS CANNOT BE READ OFF THE RECORD (the dead perspective switch).
`agent-repl--frontend-workspace-session-live-p\=' answers whether this
record is non-terminal, and `agent-repl--frontend-backfill-settled-p\=' answers
whether its history finished arriving.  Both are DURABLE, so both keep
answering yes across a daemon restart — about a daemon that has no session controller for the
workspace at all.  The switch-ensure skipped on exactly that pair, so
after every restart a switch to an unwired workspace sent no
`openWorkspace\=', nothing brought the session up, and the workspace sat
blue until the user typed.

A daemon too old to send the field reports nil, which reads as NOT live
and therefore sends: the cooldown bounds the cost, and an unnecessary
ensure is idempotent while a skipped one is the bug above."
  (eq (plist-get (agent-repl--frontend-session-view workspace) :shimAttached) t))

(defun agent-repl--frontend-ensure-skip-reason (ws)
  "Return a string naming why WS must not send `openWorkspace', or nil to send.
Ordered cheapest-first.  Every arm is a genuine no-op rather than a
deferral: nothing here is retried on this call's own account, because the
three drivers that reach it — a switch, a reconnect's snapshot, the
reattach sweep — are each other's retry."
  (cond
   ((null ws) "no workspace")
   ;; Nothing to send into. The reattach sweep owns daemon revival; a switch
   ;; must not race it into a second spawn.
   ((not (agent-repl--uds-connected-p)) "uds link down")
   ;; No cwd means no routable wire key, and the daemon routes purely by cwd.
   ((null (agent-repl--ws-get ws :project-dir)) "no project-dir")
   ;; THE common case: a workspace the daemon is ALREADY DRIVING, whose
   ;; HISTORY ARRIVED, has nothing to ensure — so the send would be pure
   ;; daemon-side rescan.
   ;;
   ;; All three conditions are load-bearing, and each one closes a distinct
   ;; way the skip was wrong:
   ;;
   ;;   - A non-terminal record (F2's `live') alone is not enough: a session
   ;;     the daemon bound and brought up, but whose transcript the sidecar
   ;;     never delivered, is live and blue at the same time.
   ;;   - A settled backfill alone is not enough for the same reason in
   ;;     reverse; `pending' and `failed' both fall through to a send.
   ;;   - AND NEITHER OF THOSE IS A FACT ABOUT THIS DAEMON. Both are read back
   ;;     off the durable registry record, so both survive a daemon restart and
   ;;     go on describing a workspace the new daemon has never brought up.
   ;;     That is the dead perspective switch: after a restart every workspace
   ;;     looked live-and-backfilled, every switch skipped, and no workspace
   ;;     ever bootstrapped. `agent-repl--frontend-session-controller-live-p' is
   ;;     the non-durable half, so a missing session controller ALWAYS ensures.
   ;;
   ;; Everything that falls through is bounded by the cooldown and give-up
   ;; below.
   ((let ((key (agent-repl--frontend-ws-command-key ws)))
      (and (agent-repl--frontend-workspace-session-live-p key)
           (agent-repl--frontend-session-controller-live-p key)
           (agent-repl--frontend-backfill-settled-p key)))
    "session already live, driven and backfilled")
   ((agent-repl--ws-get ws :ensure-failed) "gave up after repeated failures")
   ((let ((at (agent-repl--ws-get ws :ensure-at)))
      (and at (< (- (float-time) at) agent-repl-frontend-ensure-cooldown)))
    "within cooldown")
   (t nil)))

(defun agent-repl--frontend-note-ensure-failure (ws err)
  "Record a failed ensure for WS, giving up loudly at the cap.
ERR is the ack's error string.  Mirrors the reattach sweep's give-up: a
workspace the daemon cannot open is surfaced ONCE, not once per attempt.

The give-up goes out as a RETRACTABLE connection notice
\(connection-notice.el).  It describes a workspace this daemon instance
cannot bring up, which is precisely a condition that stops being true the
moment a reconnect lands — and `agent-repl--frontend-note-boot-id' already
clears the give-up state it records, so leaving the warning text standing
would be the one half of the recovery that never happened."
  (let ((n (1+ (or (agent-repl--ws-get ws :ensure-failures) 0))))
    (agent-repl--ws-put ws :ensure-failures n)
    (agent-repl--log ws "ensure: ws=%s attempt %d/%d failed: %s"
                     ws n agent-repl-frontend-ensure-max-failures err)
    (when (>= n agent-repl-frontend-ensure-max-failures)
      (agent-repl--ws-put ws :ensure-failed t)
      (agent-repl-connection-notice-warn
       (format (concat "workspace %s could not be opened on the daemon after %d "
                       "attempts (%s) — its history will not backfill; check the "
                       "daemon log for transcript discovery")
               ws n err)
       :warning))))

(defun agent-repl--frontend-ensure-workspace (&optional ws)
  "Ask the daemon to bind + ensure WS's session (`openWorkspace').
This is the client half of the never-blue requirement: a workspace with a
known on-disk transcript must render its history without waiting to be
typed into.  The daemon side \(`server.WorkspaceOpener') does the
transcript discovery, the resume bind and the shim bring-up; this only
has to ask.

THREE DRIVERS, ONE FUNCTION.  A perspective switch calls it for the
workspace switched to; the reattach sweep and the reconnect's
snapshot-applied edge call it for EVERY live workspace.  A switch used to
be the only driver, which is what made recovery from a daemon bounce a
thing the user had to trigger by visiting each workspace in turn — the
workspaces they were not looking at stayed unwired indefinitely, and the
one they were looking at recovered only because they looked.

Fire-and-forget and heavily skipped — see
`agent-repl--frontend-ensure-skip-reason'.  A rejected ack counts
toward the per-workspace give-up rather than retrying.  Returns the
request-id when a command went out, else nil.

LOGS, NEVER SIGNALS: this runs on the persp-activation path, where a
signal would strand the switch before the tail that flips the
`:ws-loaded' latch.  The link can also die between the connected-p check
and the send, so the guards above cannot be the only protection."
  (let* ((ws (or ws (agent-repl--ws-current-name)))
         (skip (agent-repl--frontend-ensure-skip-reason ws)))
    (if skip
        (progn (agent-repl--log-verbose ws "ensure: skipped (%s)" skip) nil)
      (condition-case err
          (progn
            (agent-repl--ws-put ws :ensure-at (float-time))
            (agent-repl--log ws "ensure: ws=%s -> openWorkspace (never-blue backfill)" ws)
            (let ((req nil))
              (setq req
                    (agent-repl--uds-send-command
                     "openWorkspace" nil (agent-repl--frontend-ws-command-key ws) nil
                     :on-registered (lambda (id) (setq req id))
                     :on-failure (lambda (e) (agent-repl--frontend-note-ensure-failure ws e))
                     :on-success
                     (lambda ()
                       (agent-repl--ws-put ws :ensure-failures nil)
                       (agent-repl--log-verbose ws
                                                "ensure: ack ACCEPTED request-id=%s" req))))
              req))
        (error
         (agent-repl--log ws "ensure: ws=%s send FAILED: %s"
                          ws (error-message-string err))
         nil)))))


(provide 'frontend-client)

;;; frontend-client.el ends here
