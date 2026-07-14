;;; frontend-client.el --- HTTP session client for the claude-repld daemon -*- lexical-binding: t; -*-

;;; Commentary:

;; The Emacs-side seam onto claude-repld's HTTP surface (POST/GET/DELETE
;; /sessions).  One module owns URL construction, JSON encoding/decoding,
;; readiness polling, and the workspace ⇄ session binding, so the panel
;; layer (frontend.el) and any future consumer share a single client
;; instead of hand-rolling requests.
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
;; All external I/O funnels through the single boundary wrapper
;; `agent-repl--frontend-http-request', registered in
;; `agent-repl--external-boundary-functions' per the test-harness
;; contract; tests mock it via `cl-letf'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-http)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--ws-get "agent-repl-workspace" (ws key))
(declare-function agent-repl--ws-put "agent-repl-workspace" (ws key val))
(declare-function agent-repl--ensure-frontend-daemon "agent-repl-daemon" (&optional force))
(declare-function agent-repl--resolve-current-git-root "agent-repl-core" ())
(declare-function agent-repl--ws-durable-claude-session-id "agent-repl-core" (ws))
(declare-function agent-repl--initialize-ws-env "agent-repl-session" (ws &optional project-dir-hint active-env-hint))
(declare-function agent-repl--frontend-sync-webview "agent-repl-frontend" (ws session-id))
(declare-function agent-repl--frontend-init-inhibited-p "agent-repl-daemon" ())
(declare-function agent-repl--live-ws-names "agent-repl-workspace" ())
(declare-function agent-repl--mark-ws-thinking "input" (ws))

(defvar url-http-response-status)

;;;; ---- Customization ----------------------------------------------------

(defcustom agent-repl-frontend-http-timeout 10
  "Seconds `agent-repl--frontend-http-request' waits for a daemon response."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-frontend-permission-mode "auto"
  "Permission mode for gui-created sessions (POST /sessions).
Defaults to `auto' to match the CLI's own permission-mode config
(`agent-repl-personal-permission-flag' /
`agent-repl-managed-permission-flag'), which requires the daemon to
drive the SYSTEM claude binary (daemon.el's -claude-bin) — the
SDK-bundled CLI predates the mode.  Set nil to omit the field and use
the SDK default."
  :type '(choice (const :tag "SDK default" nil) string)
  :group 'agent-repl)

(defcustom agent-repl-frontend-ready-attempts 25
  "Poll attempts for `agent-repl--frontend-wait-ready' (0.2s apart)."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-queue-preview-length 60
  "Max characters of a queued message's content shown in previews.
Caps the one-line preview `agent-repl--frontend-session-queue' derives
from a queued item's content blocks, so the status segment and the
`agent-repl-queue-*' completion prompts stay a single readable line."
  :type 'integer
  :group 'agent-repl)

;;;; ---- External boundary ------------------------------------------------

(defun agent-repl--frontend-http-request (method url &optional payload)
  "External-boundary wrapper: synchronous HTTP METHOD to URL.
PAYLOAD, when non-nil, is a JSON string sent as the request body.
Returns (STATUS . BODY-STRING).  Body does nothing but perform the
external call so tests mock it via `cl-letf'; registered in
`agent-repl--external-boundary-functions'."
  (let* ((url-request-method method)
         (url-request-extra-headers
          (when payload '(("Content-Type" . "application/json"))))
         (url-request-data
          (when payload (encode-coding-string payload 'utf-8)))
         (buf (url-retrieve-synchronously ;; ALLOW-EXTERNAL-BOUNDARY
               url t t agent-repl-frontend-http-timeout)))
    (unless buf
      (error "agent-repl: no response from %s %s" method url))
    (with-current-buffer buf
      (unwind-protect
          (progn
            (goto-char (point-min))
            (let ((status url-http-response-status))
              (if (re-search-forward "\n\n" nil t)
                  (cons status (buffer-substring-no-properties
                                (point) (point-max)))
                (cons status ""))))
        (kill-buffer buf)))))

;;;; ---- JSON API core ----------------------------------------------------

(defun agent-repl--frontend-base-url ()
  "Return the daemon's HTTP base URL from the configured address."
  (format "http://%s" agent-repl-frontend-daemon-addr))

(defun agent-repl--frontend-api (method path &optional payload-alist)
  "Issue METHOD PATH against the daemon and return the parsed JSON body.
PAYLOAD-ALIST, when non-nil, is JSON-encoded as the request body.
Signals an error on any non-2xx status (the daemon's error text is
included) and on undecodable response bodies.  Returns nil for empty
bodies (e.g. DELETE's 204)."
  (let* ((url (concat (agent-repl--frontend-base-url) path))
         (payload (when payload-alist (json-encode payload-alist)))
         (resp (agent-repl--frontend-http-request method url payload))
         (status (car resp))
         (body (cdr resp)))
    (unless (and (integerp status) (<= 200 status 299))
      (error "agent-repl: %s %s failed (HTTP %s): %s"
             method path status (string-trim (or body ""))))
    (when (and body (not (string-empty-p (string-trim body))))
      (condition-case err
          (json-parse-string body :object-type 'alist :array-type 'list)
        (error
         (error "agent-repl: %s %s returned undecodable JSON (%s): %s"
                method path (error-message-string err) body))))))

;;;; ---- Readiness ---------------------------------------------------------

(defun agent-repl--frontend-wait-ready ()
  "Block until the daemon answers GET /sessions, or signal an error.
`agent-repl--ensure-frontend-daemon' returns as soon as the process is
SPAWNED, which precedes the port bind; polling closes that gap.  Polls
`agent-repl-frontend-ready-attempts' times, 0.2s apart."
  (let ((attempt 0)
        (ready nil)
        (last-err nil))
    (while (and (not ready) (< attempt agent-repl-frontend-ready-attempts))
      (setq attempt (1+ attempt))
      (condition-case err
          (progn (agent-repl--frontend-api "GET" "/sessions")
                 (setq ready t))
        (error
         (setq last-err err)
         ;; sleep-for, NOT sit-for: sit-for returns immediately when
         ;; input is pending, which would collapse the whole readiness
         ;; window into back-to-back failed probes while the user types.
         (sleep-for 0.2))))
    (unless ready
      (error "agent-repl: daemon at %s never became ready (%d attempts): %s"
             agent-repl-frontend-daemon-addr attempt
             (error-message-string last-err)))
    t))

;;;; ---- Session CRUD -------------------------------------------------------

(defun agent-repl--frontend-create-session (cwd &optional model resume)
  "POST /sessions rooted at CWD; return the new session id.
MODEL and RESUME (a durable claude session uuid) are optional
passthroughs.  Signals on HTTP failure or a malformed response.

MODEL defaults to `agent-repl-interactive-model' when nil, matching the
CLI-launch path (`agent-repl--compute-claude-flags'): a gui session that
omitted the flag would run on whatever default the CLI picks, and the
real Claude CLI names that model only AFTER the first turn, leaving the
topbar's model picker EMPTY until then.  Sending a concrete `--model'
makes the daemon's hello carry the real model from the first frame.  A
nil `agent-repl-interactive-model' is respected as a deliberate \"let the
CLI choose\" and still sends no flag.

The account the session's CLI runs as travels in the `config_dir' field,
computed from CWD by `agent-repl--compute-config-dir' — the SAME resolver
`ai-title.el' uses to locate a workspace's transcript, so a session
always lands on the same account as the rest of the module resolves
for the same project (~/.claude-chesscom under $MULTI_REPO_ROOT,
~/.claude elsewhere).  Sending it per-session is not optional: ONE daemon
serves every workspace, so the daemon's own environment cannot encode a
per-workspace account, and without this field every gui session would
silently run as whichever account the daemon happened to inherit."
  (unless (and (stringp cwd) (not (string-empty-p cwd)))
    (error "agent-repl: create-session requires a cwd (got %S)" cwd))
  (let* ((model (agent-repl--effective-model model))
         (config-dir (agent-repl--compute-config-dir cwd))
         (payload (append `(("cwd" . ,cwd))
                          (when model `(("model" . ,model)))
                          (when resume `(("resume" . ,resume)))
                          (when config-dir `(("config_dir" . ,config-dir)))
                          (when agent-repl-frontend-permission-mode
                            `(("permission_mode" . ,agent-repl-frontend-permission-mode)))))
         (resp (agent-repl--frontend-api "POST" "/sessions" payload))
         (id (alist-get 'session_id resp)))
    (unless (and (stringp id) (not (string-empty-p id)))
      (error "agent-repl: POST /sessions returned no session_id: %S" resp))
    id))

(defun agent-repl--frontend-list-sessions ()
  "Return the daemon's session list (alist entries, possibly nil)."
  (alist-get 'sessions (agent-repl--frontend-api "GET" "/sessions")))

(defun agent-repl--frontend-delete-session (id)
  "DELETE /sessions/ID.  Signals on HTTP failure."
  (agent-repl--frontend-api "DELETE" (concat "/sessions/" id))
  t)

(defun agent-repl--frontend-session-entry (id)
  "Return the GET /sessions entry for ID, or nil when not listed."
  (seq-find (lambda (entry) (equal (alist-get 'session_id entry) id))
            (agent-repl--frontend-list-sessions)))

(defun agent-repl--frontend-fetch-commands (session-id)
  "Return SESSION-ID's slash-command menu as a list of alists.
Each entry carries the symbol keys `name', `description', and
`argumentHint'.  The list may be empty when the daemon has not yet
resolved the menu off the SDK's init handshake, which is a transient
startup state rather than an error.  Signals on an HTTP or decode
failure, per `agent-repl--frontend-api'."
  (alist-get 'commands
             (agent-repl--frontend-api
              "GET" (format "/sessions/%s/commands" session-id))))

(defun agent-repl--frontend-refresh-commands (session-id)
  "Ask the daemon to re-resolve SESSION-ID's slash-command menu.
Fire-and-forget: the daemon answers 202 immediately and the fresh list
lands on its cache asynchronously once its re-probe completes, so this
never blocks on the probe.  Signals on HTTP failure."
  (agent-repl--frontend-api
   "POST" (format "/sessions/%s/commands/refresh" session-id))
  t)

(defun agent-repl--frontend-session-live-p (id)
  "Return non-nil when ID is listed by the daemon and not terminal."
  (let ((entry (agent-repl--frontend-session-entry id)))
    (and entry (eq (alist-get 'terminal entry) :false))))

(defun agent-repl--frontend-turn-active-sessions ()
  "Return session ids the daemon reports mid-turn; nil when unreachable.
The daemon-stop guard keys on this: an unreachable daemon has nothing
to protect, so unreachability reads as \"no turns\" (loudly logged)."
  (condition-case err
      (let (busy)
        (dolist (s (agent-repl--frontend-list-sessions) (nreverse busy))
          (when (eq (alist-get 'turn_active s) t)
            (push (alist-get 'session_id s) busy))))
    (error
     (agent-repl--log nil "turn-active-sessions: daemon unreachable (%s) — treating as none"
                       (error-message-string err))
     nil)))

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
  "Reattach gui workspaces whose bound session vanished from the daemon.
A binding missing from GET /sessions while the daemon answers means a
new daemon instance (or a deleted session) — either way the workspace
re-ensures and remounts.  When the daemon is unreachable but bindings
exist, ensure it (spawn or adopt) so the next sweep can reattach."
  (let ((resp (condition-case nil
                  (agent-repl--frontend-api "GET" "/sessions")
                (error :unreachable))))
    (if (eq resp :unreachable)
        (when (cl-some (lambda (ws) (agent-repl--ws-get ws :frontend-session-id))
                       (agent-repl--live-ws-names))
          (agent-repl--log nil "reattach: daemon unreachable with bound sessions — ensuring")
          (condition-case err
              (agent-repl--ensure-frontend-daemon)
            (error
             (agent-repl--log nil "reattach: daemon ensure failed: %s"
                               (error-message-string err)))))
      (agent-repl--frontend-note-boot-id (alist-get 'boot_id resp))
      ;; Same GET /sessions poll now carries the per-session queue
      ;; snapshot (§2.13); cache it per-workspace off the one listing we
      ;; already fetched rather than issuing a second request.
      (agent-repl--frontend-capture-queues (alist-get 'sessions resp))
      (let ((listed (mapcar (lambda (s) (alist-get 'session_id s))
                            (alist-get 'sessions resp))))
        (dolist (ws (agent-repl--live-ws-names))
        (when-let ((bound (agent-repl--ws-get ws :frontend-session-id)))
          (cond
           ((member bound listed)
            (agent-repl--ws-put ws :reattach-failed nil)
            (agent-repl--ws-put ws :reattach-failures nil))
           ((agent-repl--ws-get ws :reattach-failed) nil)
           (t (agent-repl--frontend-reattach-ws ws bound)))))))))

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
          (agent-repl--ws-put ws :reattach-failures nil))))
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

;;;; ---- Message / interrupt injection ------------------------------------------

(defun agent-repl--frontend-send-message (session-id text)
  "POST TEXT as a user message to SESSION-ID; return the request id.
The daemon injects the turn through the same pipeline as a WS submit,
so every attached tab renders the user-turn echo."
  (let* ((resp (agent-repl--frontend-api
                "POST" (format "/sessions/%s/message" session-id)
                `(("content" . ,text))))
         (rid (alist-get 'request_id resp)))
    (unless (and (stringp rid) (not (string-empty-p rid)))
      (error "agent-repl: message injection returned no request_id: %S" resp))
    rid))

(defun agent-repl--frontend-interrupt-session (session-id)
  "Abort SESSION-ID's in-flight turn over HTTP."
  (agent-repl--frontend-api "POST" (format "/sessions/%s/interrupt" session-id))
  t)

;;;; ---- In-flight message queue (protocol §2.13) ---------------------------
;;
;; A `user-message' submitted while a turn is in flight is parked in the
;; daemon's per-session FIFO queue rather than forwarded.  The webapp owns
;; the rich queued-message UI; the Emacs host holds no WebSocket, so it
;; reaches the queue through two HTTP override routes and reads the queue
;; SNAPSHOT off the `GET /sessions' listing (the `queue' array each entry
;; now carries).  The snapshot is cached per-workspace under
;; `:queued-messages' by `agent-repl--frontend-capture-queues', refreshed
;; on the reattach sweep's existing GET /sessions poll.

(defun agent-repl--frontend-queue-run-now (session-id queue-id)
  "Escalate queued message QUEUE-ID in SESSION-ID to run now over HTTP.
POSTs the daemon's run-now override route, a manual `interrupt' verdict
\(§2.13): the item moves to the queue front and, if a turn is in flight,
the running turn is interrupted so the drain picks this item up next.
A stale QUEUE-ID is a daemon-side no-op ack, not an error."
  (agent-repl--frontend-api
   "POST" (format "/sessions/%s/queue/%s/run-now" session-id queue-id))
  t)

(defun agent-repl--frontend-queue-cancel (session-id queue-id)
  "Cancel queued message QUEUE-ID in SESSION-ID over HTTP.
POSTs the daemon's cancel override route, removing the item from the
queue without ever sending it (§2.13).  A stale QUEUE-ID is a
daemon-side no-op ack, not an error."
  (agent-repl--frontend-api
   "POST" (format "/sessions/%s/queue/%s/cancel" session-id queue-id))
  t)

(defun agent-repl--frontend-queue-content-preview (content)
  "Return a one-line text preview of CONTENT, a list of content-block alists.
Concatenates the `text' of every text block, collapses runs of
whitespace to single spaces, and truncates to
`agent-repl-queue-preview-length' characters (an ellipsis marks a
truncation).  Non-text blocks contribute nothing, so a tool-only turn
previews as the empty string."
  (let* ((texts (delq nil
                      (mapcar (lambda (block)
                                (when (equal (alist-get 'type block) "text")
                                  (alist-get 'text block)))
                              content)))
         (joined (string-trim
                  (replace-regexp-in-string
                   "[ \t\n\r]+" " " (string-join texts " ")))))
    (if (> (length joined) agent-repl-queue-preview-length)
        (concat (substring joined 0 agent-repl-queue-preview-length) "…")
      joined)))

(defun agent-repl--frontend-session-queue (entry)
  "Return ENTRY's in-flight message queue as a list of plists, front-to-back.
ENTRY is a `GET /sessions' listing entry (an alist).  Each returned
plist carries `:queue-id', `:status', `:verdict', and a
`:content-preview' (a truncated one-line rendering of the item's content
per §2.13).  Returns nil when ENTRY carries no `queue' array."
  (mapcar (lambda (item)
            (list :queue-id (alist-get 'queue_id item)
                  :status (alist-get 'status item)
                  :verdict (alist-get 'verdict item)
                  :content-preview
                  (agent-repl--frontend-queue-content-preview
                   (alist-get 'content item))))
          (alist-get 'queue entry)))

(defun agent-repl--frontend-capture-queues (sessions)
  "Refresh every live workspace's `:queued-messages' from SESSIONS.
SESSIONS is the parsed `sessions' array of a GET /sessions poll.  For
each live workspace bound to a listed session, the parsed queue snapshot
\(`agent-repl--frontend-session-queue') is stored under
`:queued-messages'; a bound workspace whose session is absent from
SESSIONS has its queue cleared.  Forces a mode-line repaint so the
queued-count segment reflects the new snapshot."
  (dolist (ws (agent-repl--live-ws-names))
    (when-let ((bound (agent-repl--ws-get ws :frontend-session-id)))
      (let ((entry (seq-find (lambda (s) (equal (alist-get 'session_id s) bound))
                             sessions)))
        (agent-repl--ws-put ws :queued-messages
                            (and entry (agent-repl--frontend-session-queue entry))))))
  (force-mode-line-update t))

(defun agent-repl--ws-queued-messages (ws)
  "Return WS's last-known in-flight message queue (list of plists).
Front-to-back, empty when nothing is queued; see
`agent-repl--frontend-session-queue' for the plist shape.  Refreshed by
`agent-repl--frontend-capture-queues' on the reattach sweep's
GET /sessions poll."
  (agent-repl--ws-get ws :queued-messages))

(defun agent-repl--ws-queued-count (ws)
  "Return the number of messages queued for WS (0 when none)."
  (length (agent-repl--ws-queued-messages ws)))

(defun agent-repl--gui-send-turn (ws input raw &optional on-settle)
  "The gui frontend's send capability (registry `:send-fn').
INPUT (the prepared text, which may carry the metaprompt prefix —
genuine message content) goes to the daemon session.  There is no
owning-workspace pin to apply here — WS's daemon session id already
identifies the target unambiguously, unlike a shared vterm buffer that
once needed disambiguating — but the prefix counter still increments
so metaprompt periodicity is tracked the same way for every workspace.
Posthooks and prompt summary key on RAW, identically.

Sets `:thinking' optimistically BEFORE the HTTP send: the
UserPromptSubmit hook remains the authoritative confirmation, but a
permission request can beat a lagging hook and
`agent-repl--on-permission-event' gates on `:thinking' — without the
optimistic write the daemon's permission sentinel would be silently
dropped."
  (agent-repl--log ws "do-send[gui] ws=%s len=%d" ws (length input))
  (agent-repl--mark-ws-thinking ws)
  (agent-repl--increment-prefix-counter ws)
  (agent-repl--ws-put ws :last-prompt-time (float-time))
  (agent-repl--frontend-send-user-message ws input)
  (agent-repl--run-send-posthooks ws raw)
  (agent-repl--kickoff-prompt-summary ws raw)
  (when on-settle (funcall on-settle)))

(defun agent-repl--gui-interrupt (ws _kind)
  "The gui frontend's interrupt capability (registry `:interrupt-fn').
KIND is ignored: it distinguished the vterm TUI's two interrupt
gestures (`ctrl-c' clearing the prompt line vs `escape' stopping
generation), and with vterm gone no registered frontend still needs
that distinction — one wire-level HTTP interrupt serves the single
gesture the gui exposes.  The parameter itself stays (unused) so the
registry's `:interrupt-fn' shape stays uniform for whatever frontend
lands next.  Returns non-nil since the HTTP route either delivers or
signals."
  (let ((id (agent-repl--ws-get ws :frontend-session-id)))
    (agent-repl--log ws "interrupt[gui]: session=%s" id)
    (agent-repl--frontend-interrupt-session id)
    t))

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
    (condition-case err
        (alist-get 'claude_session_id (agent-repl--frontend-session-entry sid))
      (error
       (agent-repl--log ws "gui durable-id fetch FAILED for %s: %s"
                        sid (error-message-string err))
       nil))))

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

(defun agent-repl--frontend-send-user-message (ws text)
  "Send TEXT as WS's user turn via its bound daemon session.
Ensures the session first (recreating a stale binding), so a send into
a dead session heals instead of 404ing."
  (let ((id (agent-repl--frontend-ensure-session ws)))
    ;; The ensure may have HEALED a dead binding into a fresh session —
    ;; the displayed webview must follow, or the user watches the dead
    ;; session while the turn streams into the replacement.
    (agent-repl--frontend-sync-webview ws id)
    (agent-repl--log ws "frontend send: session=%s len=%d" id (length text))
    (agent-repl--frontend-send-message id text)))

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
            (agent-repl--frontend-delete-session id)
            (agent-repl--log ws "frontend session released: %s" id))
        (error
         (agent-repl--log ws "frontend session release FAILED for %s: %s"
                          id (error-message-string err))))
      (agent-repl--ws-put ws :frontend-session-id nil))))

(add-hook 'agent-repl-ws-del-hook #'agent-repl--frontend-release-workspace-session)

(provide 'frontend-client)

;;; frontend-client.el ends here
