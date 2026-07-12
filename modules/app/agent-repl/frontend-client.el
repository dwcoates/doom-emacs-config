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
(declare-function agent-repl--mark-ws-thinking "input" (ws))

(defvar url-http-response-status)

;;;; ---- Customization ----------------------------------------------------

(defcustom agent-repl-frontend-http-timeout 10
  "Seconds `agent-repl--frontend-http-request' waits for a daemon response."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-frontend-permission-mode "auto"
  "Permission mode for gui-created sessions (POST /sessions).
Defaults to `auto' to match the vterm start flags
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
passthroughs.  Signals on HTTP failure or a malformed response."
  (unless (and (stringp cwd) (not (string-empty-p cwd)))
    (error "agent-repl: create-session requires a cwd (got %S)" cwd))
  (let* ((payload (append `(("cwd" . ,cwd))
                          (when model `(("model" . ,model)))
                          (when resume `(("resume" . ,resume)))
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

(defun agent-repl--frontend-session-live-p (id)
  "Return non-nil when ID is listed by the daemon and not terminal."
  (let ((entry (agent-repl--frontend-session-entry id)))
    (and entry (eq (alist-get 'terminal entry) :false))))

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
        (let ((id (agent-repl--frontend-create-session dir)))
          (agent-repl--ws-put ws :frontend-session-id id)
          (agent-repl--log ws "frontend session created: %s (cwd=%s)" id dir)
          id)))))

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

(defun agent-repl--gui-send-turn (ws input raw &optional on-settle)
  "The gui frontend's send capability (registry `:send-fn').
INPUT (the prepared text, which may carry the metaprompt prefix —
genuine message content) goes to the daemon session.  Only the
owning-workspace pin is vterm-specific machinery skipped here; the
prefix counter still increments so metaprompt periodicity matches the
vterm frontend.  Posthooks and prompt summary key on RAW, identically.

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
One wire-level interrupt serves both TUI gestures; returns non-nil
since the HTTP route either delivers or signals."
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
         (id (agent-repl--frontend-create-session dir nil claude-session-id)))
    (agent-repl--ws-put ws :frontend-session-id id)
    (agent-repl--log ws "gui adopted claude session %s as %s" claude-session-id id)
    id))

(defun agent-repl--frontend-send-user-message (ws text)
  "Send TEXT as WS's user turn via its bound daemon session.
Ensures the session first (recreating a stale binding), so a send into
a dead session heals instead of 404ing."
  (let ((id (agent-repl--frontend-ensure-session ws)))
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
