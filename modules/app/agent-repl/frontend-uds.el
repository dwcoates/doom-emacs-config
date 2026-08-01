;;; frontend-uds.el --- UDS client for the agent-shim frontend surface -*- lexical-binding: t; -*-

;;; Commentary:

;; The Emacs-side transport onto the daemon's `agentshim.frontend.v1'
;; surface (design-agent-shim-architecture.md §3, §5.4, §10).  It replaces
;; the HTTP poller in `frontend-client.el': instead of Emacs deriving state
;; from raw frames, the daemon pushes RESOLVED `frontend.v1' frames over a
;; Unix-domain socket, newline-delimited, serialized with the canonical
;; proto3 JSON mapping (protojson, lowerCamelCase field names).
;;
;; This file owns ONLY the transport:
;;   - the single external boundary `agent-repl--uds-connect' (the sole
;;     `make-network-process' call; mock this in tests);
;;   - the connection lifecycle + timer-based reconnect (the timer seam
;;     `agent-repl--uds-run-timer' is injectable so tests never arm a real
;;     timer);
;;   - newline framing + protojson decode of each frame into an Emacs plist;
;;   - dispatch of each FrontendFrame to a handler registered by oneof field
;;     name (`snapshot', `workspaceState', ...);  and
;;   - outbound `FrontendCommand' send with `request_id' generation.
;;
;; It does NOT interpret frames.  Frame semantics (WorkspaceState → render
;; keyword, StateSnapshot resync, DegradedNotice surfacing) live in
;; `frontend-state.el', which registers handlers here at load.
;;
;; No-Silent-Fallbacks (AGENTS.md): a malformed frame (undecodable JSON,
;; a frame with no oneof field, or an unknown oneof field) is loud-logged
;; and SIGNALS `agent-repl-uds-malformed-frame' — it is never skipped.  A
;; frame whose oneof field is KNOWN but currently has no registered handler
;; is loud-logged (a not-yet-wired condition surfaced honestly), not
;; silently dropped.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)

;;;; ---- Errors ----------------------------------------------------------

(define-error 'agent-repl-uds-malformed-frame
  "Malformed agent-shim frontend UDS frame")

;;;; ---- Wire workspace -> workspace name --------------------------------
;;
;; The daemon names a workspace by its session CWD, and this file's own
;; wire key helper says so outright: the `workspace' field on a frame or a
;; command "is WS's `:project-dir' ... NEVER the persp name WS itself".  A
;; CWD can never index `agent-repl--workspaces', so handing one to the
;; logging ladder makes `agent-repl--workspace-log-identity' signal — inside
;; the connection's process filter, for inbound frames.  Every wire
;; workspace is therefore resolved to a NAME once, at its binding site,
;; before it reaches a log call.  The raw wire value stays in the message
;; TEXT so an operator can still correlate an unowned path.
(declare-function agent-repl--frontend-ws-name "frontend-state" (workspace))
(declare-function agent-repl--frontend-invalidate-daemon-view "frontend-state" (reason))

;;;; ---- Configuration ---------------------------------------------------

(defcustom agent-repl-uds-socket-path
  (expand-file-name "agent-repl/sock/daemon-frontend.sock"
                    (or (getenv "XDG_CACHE_HOME")
                        (expand-file-name "~/.cache")))
  "Path to the daemon's frontend UDS listener (design §3).
The daemon listens here; Emacs connects.  Serialized frames are
newline-delimited protojson `agentshim.frontend.v1' messages."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-uds-reconnect-delay 2.0
  "Seconds to wait before attempting to reconnect a dropped UDS link.
A dropped link means honest downtime (design §4.4): the display goes
stale until the daemon is reachable again.  There is no spill buffer."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-uds-command-ack-deadline 10.0
  "Seconds an outbound `FrontendCommand' may go unacknowledged.

Every command Emacs sends is answered by a `CommandAck' frame.  Past this
deadline the command is declared LOST: the daemon either never read it or
never answered, and in both cases nothing further is coming.  The frontend
says so loudly (log + echo) and marks its own command link degraded
\(`agent-repl-uds-link-health') rather than leaving the request pending
forever with no feedback at all.

This is a REPORTING deadline, never a retry timer: the command is not
resent, because a resend would double a submitPrompt or a mergeWorkspace
the daemon may in fact have performed."
  :type 'number
  :group 'agent-repl)

;;;; ---- Connection state ------------------------------------------------

(defvar agent-repl--uds-process nil
  "The live frontend UDS `process' object, or nil when disconnected.")

(defvar agent-repl--uds-read-accumulator ""
  "Bytes received on the UDS link not yet split into complete frames.
The daemon frames each protojson message with a trailing newline; a
chunk may carry several frames and/or a partial trailing frame, which
is retained here until its newline arrives.")

(defvar agent-repl--uds-reconnect-timer nil
  "The pending reconnect timer, or nil.  Reset via `agent-repl--uds-run-timer'.")

(defvar agent-repl--uds-request-id-counter 0
  "Monotonic counter feeding `agent-repl--uds-generate-request-id'.")

(defvar agent-repl--uds-pending-commands (make-hash-table :test 'equal)
  "Hash of outbound `request_id' -> plist awaiting its `CommandAck'.
Populated by `agent-repl--uds-track-command'; entries are removed by
`agent-repl--uds-handle-command-ack' when the matching ack lands.  Each
value plist carries `:field' (the command oneof name), `:workspace', and
an optional `:on-failure' (a function of one arg, the error string, run
when the ack reports failure — in addition to the loud log + echo).  Each
value also carries `:deadline-timer', the ack-aging alarm armed by
`agent-repl--uds-track-command'.")

(defvar agent-repl--uds-timed-out-commands (make-hash-table :test 'equal)
  "Hash of request_id -> plist for commands whose ack deadline expired.

An entry moves here from `agent-repl--uds-pending-commands' when
`agent-repl--uds-command-deadline-expired' declares it lost, and leaves
when a LATE `CommandAck' finally arrives.  A timed-out command is marked,
never silently dropped: without this record a late ack would read as an
ack for a request nobody ever sent.

Each value carries only `:field' and `:workspace'.  The caller callbacks
are deliberately NOT retained — the caller has already been told the
command was lost, so letting a delayed ack run `:on-success' would mutate
state that was abandoned ten seconds earlier.")

(defvar agent-repl--uds-link-health :healthy
  "The FRONTEND's own fact about its command link to the daemon.

`:healthy' means every command sent since the last check was answered.
`:degraded' means at least one command went unacknowledged past
`agent-repl-uds-command-ack-deadline' and nothing has been acknowledged
since.

This axis is Emacs-owned and Emacs-classified, exactly like
`client.daemon_unreachable': the daemon definitionally cannot report that
it failed to answer Emacs.  It is NOT the daemon-pushed workspace session
status, which describes the daemon-to-shim link and stays entirely the
daemon's to write.  A healthy session status and a degraded command link
are a perfectly coherent pair, and were precisely the pair that went
unreported when three `mergeWorkspace' commands vanished into a starved
connection.")

(defvar agent-repl--uds-pending-health-responses (make-hash-table :test 'equal)
  "Hash of health request_id -> correlation contract awaiting a health view.
Each value carries `:field', `:workspace', `:session-id', and
`:on-response'.  Health result frames consume these entries; CommandAck
receipt tracking remains separate because an accepted command is not proof
that the daemon or session is healthy.")

;;;; ---- Frame vocabulary ------------------------------------------------

(defconst agent-repl--uds-known-frame-fields
  '("snapshot" "workspaceState" "sessionView" "conversationDelta"
    "typingDelta" "taskCatalog" "commandAck" "daemonView"
    "sessionInit" "heartbeat" "queue" "progress"
    "workspaceAvailable" "hostAction" "daemonHealth" "sessionHealth")
  "The protojson (lowerCamelCase) names of every `FrontendFrame' oneof arm.
Mirrors the `frame' oneof in proto/agentshim/frontend/v1/frontend.proto.
A decoded frame whose sole top-level key is NOT one of these is
malformed (unknown wire field) and signals loudly.

`sessionInit' (S9) carries the session's retained `SystemInit' (slash
commands, tools, skills, model list); it is the pushed-frame replacement
for the deleted GET /commands HTTP slash-menu source.

`taskCatalog', `heartbeat' (E4), `queue' (E4), and `progress' (F1) are
decoded for wire parity and rendered by nothing here — see
`agent-repl--uds-ignored-frame-fields'.")

(defconst agent-repl--uds-ignored-frame-fields
  '("taskCatalog" "heartbeat" "queue" "progress")
  "Frame arms Emacs decodes for wire parity but DELIBERATELY renders nothing for.
These are a subset of `agent-repl--uds-known-frame-fields'.

The distinction matters for honest logging.  A known arm with no handler
is normally an UNFINISHED-WIRING condition and is loud-logged as such, so
the gap is visible.  An arm listed here is a settled DESIGN DECISION, not
a gap: the webapp owns the visual and the Emacs frontend has no business
rendering it.  Logging those at the same volume would train the reader to
ignore a message that is supposed to mean \"something is missing\".

`heartbeat' (E4): a tool-liveness tick whose only consumer is the
webapp's running-tool chip.  Emacs shows no per-tool elapsed clock.

`queue' (E4): the prompts the daemon is holding for a session.  The
queue's controls (force/accept/cancel) live where the prompt was typed —
the webapp composer — and Emacs does not offer them, so rendering the
chips here would show state the user could not act on.

`progress' (F1): the consolidated progress footer's whole input.  The
footer is webapp-only by settled decision (design-progress-footer.md,
\"No Emacs component\"), so this arm has no Emacs consumer and never
will.  It is registered because the daemon PUSHES it: this vocabulary
predated the frame, so every ProgressView push signalled
`agent-repl-uds-malformed-frame' and surfaced as a user-visible error on
workspace open.  Registering this one KNOWN arm is the fix; the
malformed guard itself is unchanged, so a genuinely unknown future arm
still fails loudly.

`taskCatalog': the complete detached-task roster.  The webapp renders it
in the progress footer's task counter; Emacs renders only the daemon's
resolved aggregate `WorkspaceState.live_task_count' through workspace
status and has no per-task roster.  Treating this pushed arm as an
unfinished handler flooded the durable log on every task transition even
though there is intentionally nothing for Emacs to apply.")

(defconst agent-repl--uds-known-command-fields
  '("submitPrompt" "interrupt" "permissionAnswer" "mergeWorkspace"
    "closeWorkspace" "openWorkspace" "resync" "createSession" "deleteSession"
    "shutdown" "clientLog" "queueForce" "queueAccept" "queueCancel"
    "workspaceMaterialized" "hostActionCompleted"
    "daemonHealth" "sessionHealth" "restartSession")
  "The protojson names of every SENDABLE `FrontendCommand' oneof arm.
Mirrors the `command' oneof in frontend.proto.  Sending an unknown
command field is a programming error and fails loudly.

`createWorkspace' is deliberately ABSENT even though the proto arm still
exists.  Workspace creation has exactly one ingestion point — a
`workspace_commands_<uuid>.json' file in the daemon's inbox — and the
daemon rejects the wire command outright, so omitting it here turns a
stray send into a loud Emacs-side failure instead of a daemon NACK.

`shutdown' (S9) is the graceful-daemon-shutdown command (`ShutdownCmd')
that replaces the Emacs POST /shutdown HTTP call.

`clientLog' (E4) mirrors a frontend diagnostic line into the daemon's
log.  It is listed so this stays a faithful mirror of the proto oneof,
but Emacs does not use it: Emacs already writes its own log file
directly (`agent-repl--log'), so routing its diagnostics through the
daemon would move them FURTHER from the reader, not closer.  The webapp
needs it because its console is invisible and unpersisted.

`queueForce' / `queueAccept' / `queueCancel' (E4) act on a held prompt.
Listed for the same mirror reason; Emacs does not send them, because it
does not render the queue they act on.")

(defvar agent-repl--uds-frame-handlers nil
  "Alist mapping a `FrontendFrame' oneof field name (string) to a handler fn.
The handler is called with ONE argument: the plist decoded from that
arm's protojson value.  Populated via `agent-repl--uds-register-handler'
(e.g. `frontend-state.el' registers `workspaceState'/`snapshot'/
`sessionView').  A known field with no entry here is logged, not
dropped silently.")

;;;; ---- Handler registry ------------------------------------------------

(defun agent-repl--uds-register-handler (field fn)
  "Register FN as the handler for FrontendFrame oneof FIELD (a string).
FIELD must be one of `agent-repl--uds-known-frame-fields' — registering
a handler for an unknown field is a programming error and signals (no
silent fallback).  A later registration for the same FIELD replaces the
earlier one (loud-logged)."
  (unless (member field agent-repl--uds-known-frame-fields)
    (agent-repl--log nil
                     "uds-register-handler: REFUSING unknown frame field=%s known=%S"
                     field agent-repl--uds-known-frame-fields)
    (error "agent-repl UDS: cannot register handler for unknown frame field %s"
           field))
  (let ((existing (assoc field agent-repl--uds-frame-handlers)))
    (when existing
      (agent-repl--log nil
                       "uds-register-handler: REPLACING handler for field=%s old=%s new=%s"
                       field (cdr existing) fn))
    (setq agent-repl--uds-frame-handlers
          (cons (cons field fn)
                (assoc-delete-all field agent-repl--uds-frame-handlers))))
  (agent-repl--log nil "uds-register-handler: field=%s -> %s" field fn)
  fn)

;;;; ---- External boundary (mock this) -----------------------------------

(defun agent-repl--uds-connect (path name filter sentinel)
  "External boundary: open a client connection to the UDS at PATH.
Does NOTHING but call `make-network-process' with a local-domain
socket — no conditional logic, no parsing, no retries (those live in
the callers this wrapper is mocked out of).  NAME is the process name;
FILTER and SENTINEL are installed on the process.  Returns the process.

AGENTS.md external-boundary wrapper: this symbol MUST be registered in
`agent-repl--external-boundary-functions' (see the stitch-phase note in
this module's landing report) so the test guard installs for it."
  (make-network-process
   :name name
   :family 'local
   :service path
   :coding 'utf-8-unix
   :nowait nil
   :noquery t
   :filter filter
   :sentinel sentinel))

(defun agent-repl--uds-probe (path)
  "External boundary: open a THROWAWAY connection to the UDS at PATH.
Does nothing but dial and immediately close, so the only thing its return
proves is that something is LISTENING at PATH.  Signals (like
`make-network-process') when nothing is; callers convert that to a
boolean.  Deliberately separate from `agent-repl--uds-connect': it must
not disturb `agent-repl--uds-process', install handlers, or arm the
reconnect timer, because the liveness probes in daemon.el ask the
question repeatedly (including while waiting for a daemon to EXIT).

AGENTS.md external-boundary wrapper: registered in
`agent-repl--external-boundary-functions'."
  (delete-process
   (make-network-process
    :name "agent-repl-frontend-uds-probe"
    :family 'local
    :service path
    :nowait nil
    :noquery t))
  t)

;;;; ---- Connection lifecycle --------------------------------------------

(defun agent-repl--uds-connected-p ()
  "Return non-nil iff the frontend UDS link is live."
  (process-live-p agent-repl--uds-process))

(defun agent-repl--uds-socket-live-p (&optional path)
  "Return non-nil when a daemon is listening on the frontend UDS at PATH.
PATH defaults to `agent-repl-uds-socket-path'.  An already-live link is
proof enough; otherwise a throwaway `agent-repl--uds-probe' dials.  A
refused dial (no listener, or a stale socket FILE left by a dead daemon)
counts as absent, which is how the daemon-adoption and shutdown-grace
polls in daemon.el read \"no daemon there\".

This is the UDS replacement for the deleted `GET /sessions' port probe:
the socket is the same one a FOREIGN daemon (one this Emacs did not
spawn) owns, so it detects an adopted daemon exactly as the HTTP probe
did."
  (or (agent-repl--uds-connected-p)
      (condition-case err
          (agent-repl--uds-probe (or path agent-repl-uds-socket-path))
        (error
         (agent-repl--log nil "uds-socket-live-p: no listener at %s (%s)"
                          (or path agent-repl-uds-socket-path)
                          (error-message-string err))
         nil))))

(cl-defun agent-repl-uds-connect (&optional path readiness-p)
  "Establish (or re-establish) the frontend UDS connection.
PATH defaults to `agent-repl-uds-socket-path'.  On success the daemon
pushes a `StateSnapshot' first, then deltas (design §5.4) — this client
does not poll.

On an ordinary dial failure the error is loud-logged and surfaced, and a
reconnect is scheduled (design §4.4 honest downtime); the connection is
left nil and the display goes stale until the daemon is reachable.

When READINESS-P is non-nil, the synchronous
`agent-repl--frontend-wait-ready' loop owns retry pacing and the final
hard error.  A refused cold-start dial is still fully logged, but it
neither raises a premature outage alarm nor arms a competing timer.
Returns the process on success, nil on a failed dial."
  (interactive)
  (when (agent-repl--uds-connected-p)
    (agent-repl--log nil "uds-connect: already connected (proc=%s) — no-op"
                     (process-name agent-repl--uds-process))
    (cl-return-from agent-repl-uds-connect agent-repl--uds-process))
  (let ((sock (or path agent-repl-uds-socket-path)))
    ;; A connected socket is not a ready daemon until THIS connection's
    ;; snapshot lands. Retaining the previous DaemonView let wait-ready return
    ;; immediately and made restart preflight read stale state stores.
    (agent-repl--frontend-invalidate-daemon-view "uds-connect-new-dial")
    (setq agent-repl--uds-read-accumulator "")
    (agent-repl--log nil "uds-connect: dialing %s" sock)
    (condition-case err
        (let ((proc (agent-repl--uds-connect
                     sock "agent-repl-frontend-uds"
                     #'agent-repl--uds-filter
                     #'agent-repl--uds-sentinel)))
          (setq agent-repl--uds-process proc)
          (agent-repl--log nil "uds-connect: connected proc=%s status=%s"
                           (process-name proc) (process-status proc))
          ;; A fresh connection is a fresh command plane: whatever went
          ;; unacknowledged belonged to the socket that is gone.  Any command
          ;; still aging on the NEW connection reports itself when its own
          ;; deadline expires, so this restore cannot hide a live failure.
          (agent-repl--uds-link-restore "reconnect" nil)
          proc)
      (error
       (setq agent-repl--uds-process nil)
       (agent-repl--log nil
                        (concat "uds-connect: FAILED dialing %s: %S "
                                "readiness=%s action=%s")
                        sock err readiness-p
                        (if readiness-p
                            "readiness-loop-retains-control"
                          (format "surface-and-reconnect-in-%ss"
                                  agent-repl-uds-reconnect-delay)))
       (unless readiness-p
         ;; Emacs's OWN classification (F4). The daemon definitionally
         ;; cannot report that Emacs could not reach it, so this is one of
         ;; the very few facts this end classifies for itself — and it
         ;; carries the reserved `client.' prefix that says so.
         (agent-repl-failure-surface
          nil
          (agent-repl-failure-local
           "client.daemon_unreachable"
           "the agent-repl daemon is unreachable; reconnecting"
           (format "socket=%s %s" sock (error-message-string err))))
         (agent-repl--uds-schedule-reconnect))
       nil))))

(defun agent-repl-uds-disconnect ()
  "Tear down the frontend UDS connection and cancel any pending reconnect."
  (interactive)
  (when (timerp agent-repl--uds-reconnect-timer)
    (cancel-timer agent-repl--uds-reconnect-timer)
    (setq agent-repl--uds-reconnect-timer nil))
  (when (process-live-p agent-repl--uds-process)
    (agent-repl--log nil "uds-disconnect: deleting proc=%s"
                     (process-name agent-repl--uds-process))
    (delete-process agent-repl--uds-process))
  (setq agent-repl--uds-process nil
        agent-repl--uds-read-accumulator ""))

;;;; ---- Reconnect scheduling (injectable timer seam) --------------------

(defun agent-repl--uds-run-timer (delay fn)
  "Injectable timer seam: schedule FN to run once after DELAY seconds.
Isolated so tests can `cl-letf' it to capture (DELAY FN) without arming
a real timer.  Production body does nothing but call `run-with-timer'."
  (run-with-timer delay nil fn))

(defun agent-repl--uds-schedule-reconnect ()
  "Schedule a single reconnect attempt after `agent-repl-uds-reconnect-delay'.
Cancels any previously-pending reconnect timer first so a burst of
disconnect signals collapses to one attempt."
  (when (timerp agent-repl--uds-reconnect-timer)
    (cancel-timer agent-repl--uds-reconnect-timer))
  (agent-repl--log nil "uds-schedule-reconnect: reconnect in %ss"
                   agent-repl-uds-reconnect-delay)
  (setq agent-repl--uds-reconnect-timer
        (agent-repl--uds-run-timer agent-repl-uds-reconnect-delay
                                   #'agent-repl-uds-connect)))

(defun agent-repl--uds-sentinel (proc event)
  "Process sentinel: on any non-live transition, schedule a reconnect.
EVENT is the raw sentinel string.  A closed/failed/deleted link clears
the accumulator (a partial frame across a disconnect is unrecoverable)
and reconnects (design §4.4)."
  (agent-repl--log nil "uds-sentinel: proc=%s event=%s"
                   (process-name proc) (string-trim event))
  (unless (process-live-p proc)
    (when (eq proc agent-repl--uds-process)
      (setq agent-repl--uds-process nil))
    (setq agent-repl--uds-read-accumulator "")
    (agent-repl--log nil "uds-sentinel: link down — scheduling reconnect")
    (agent-repl--uds-schedule-reconnect)))

;;;; ---- Inbound framing + decode ----------------------------------------

(defun agent-repl--uds-filter (_proc chunk)
  "Process filter: accumulate CHUNK and dispatch every complete frame.
Frames are newline-delimited; a partial trailing frame is retained in
`agent-repl--uds-read-accumulator' until its newline arrives."
  ;; A filter may receive a high rate of small chunks; retain only framing
  ;; metrics, never wire contents, in the verbose trace.
  (agent-repl--log-verbose
   nil "uds-filter: received bytes=%d buffered-before=%d"
   (length chunk) (length agent-repl--uds-read-accumulator))
  (setq agent-repl--uds-read-accumulator
        (concat agent-repl--uds-read-accumulator chunk))
  (let (line)
    (while (setq line (agent-repl--uds-next-line))
      (agent-repl--uds-handle-line line))
    (agent-repl--log-verbose
     nil "uds-filter: buffered-after=%d"
     (length agent-repl--uds-read-accumulator))))

(defun agent-repl--uds-next-line ()
  "Pop and return the next complete (newline-terminated) frame line.
Returns the line WITHOUT its trailing newline, advancing the
accumulator past it; returns nil when no complete line is buffered."
  (let ((nl (string-search "\n" agent-repl--uds-read-accumulator)))
    (when nl
      (prog1 (substring agent-repl--uds-read-accumulator 0 nl)
        (setq agent-repl--uds-read-accumulator
              (substring agent-repl--uds-read-accumulator (1+ nl)))))))

(defun agent-repl--uds-truncate (s &optional cap)
  "Return S truncated to CAP chars (default 500) for safe logging."
  (let ((cap (or cap 500)))
    (if (> (length s) cap)
        (concat (substring s 0 cap) (format "…[+%d bytes]" (- (length s) cap)))
      s)))

(defun agent-repl--uds-handle-line (line)
  "Decode and dispatch one frame LINE (a newline-stripped protojson string).
A blank framing line (empty or whitespace-only) is not a frame and is
skipped; every non-blank line is decoded and dispatched."
  (let ((trimmed (string-trim line)))
    (if (string-empty-p trimmed)
        (agent-repl--log-verbose nil "uds-handle-line: skipping blank framing line")
      (agent-repl--uds-dispatch-frame (agent-repl--uds-decode-frame trimmed)))))

(defun agent-repl--uds-decode-frame (line)
  "Decode protojson frame LINE into a plist.
Booleans decode to t/nil and JSON null to nil.  On undecodable JSON the
failure is loud-logged and SIGNALS `agent-repl-uds-malformed-frame' — it
is never silently skipped (AGENTS.md No-Silent-Fallbacks)."
  (condition-case err
      (json-parse-string line
                         :object-type 'plist
                         :array-type 'list
                         :false-object nil
                         :null-object nil)
    (json-parse-error
     (agent-repl--log nil
                      "uds-decode-frame: MALFORMED json bytes=%d error-type=%s"
                      (length line) (car err))
     (signal 'agent-repl-uds-malformed-frame
             (list (error-message-string err) line)))))

(defun agent-repl--uds-dispatch-frame (frame)
  "Dispatch decoded FRAME (a one-key plist) to its registered handler.
The single top-level key names the `FrontendFrame' oneof arm.  A frame
with no oneof field, or an unknown one, is loud-logged and SIGNALS
`agent-repl-uds-malformed-frame'.  A KNOWN field with no registered
handler is loud-logged (a not-yet-wired condition surfaced honestly),
not silently dropped.  Returns the handler's value, or nil."
  (unless (and (listp frame) (keywordp (car frame)) (null (cddr frame)))
    (agent-repl--log nil
                     "uds-dispatch: MALFORMED frame shape=%s element-count=%s — expected one oneof arm"
                     (type-of frame) (and (listp frame) (length frame)))
    (signal 'agent-repl-uds-malformed-frame
            (list "frame carries no oneof field" frame)))
  (let* ((key (car frame))
         (field (substring (symbol-name key) 1))
         (payload (plist-get frame key))
         (workspace (and (listp payload) (plist-get payload :workspace)))
         ;; Every frame arm carries the wire CWD here, so this is the
         ;; single hottest place a path could reach the log sink.
         (log-workspace (agent-repl--frontend-ws-name workspace))
         (session-id (and (listp payload) (plist-get payload :sessionId)))
         (revision (and (listp payload)
                        (or (plist-get payload :revision)
                            (plist-get payload :revisionId))))
         (state (and (listp payload) (plist-get payload :state))))
    (cond
     ((not (member field agent-repl--uds-known-frame-fields))
      (agent-repl--log log-workspace
                       "uds-dispatch: MALFORMED unknown oneof field=%s workspace=%S session-id=%S known=%S"
                       field workspace session-id agent-repl--uds-known-frame-fields)
      (signal 'agent-repl-uds-malformed-frame
              (list (format "unknown oneof field: %s" field) frame)))
     (t
      (let ((handler (cdr (assoc field agent-repl--uds-frame-handlers))))
        (cond
         (handler
          (agent-repl--log log-workspace
                           "uds-dispatch: field=%s workspace=%S session-id=%S revision=%S state=%S -> handler=%s"
                           field workspace session-id revision state handler)
          (funcall handler payload))
         ((member field agent-repl--uds-ignored-frame-fields)
          (let ((task-count (and (equal field "taskCatalog")
                                 (length (plist-get payload :tasks)))))
            (agent-repl--log-verbose
             log-workspace
             "uds-dispatch: field=%s deliberately ignored workspace=%s session=%s revision=%S state=%S task-count=%S"
             field workspace session-id revision state task-count))
          nil)
         (t
          (agent-repl--log log-workspace
                           "uds-dispatch: field=%s workspace=%S session-id=%S revision=%S state=%S KNOWN but no handler registered — not dispatched (register one at stitch)"
                           field workspace session-id revision state)
          nil)))))))

;;;; ---- Outbound commands -----------------------------------------------

(defun agent-repl--uds-generate-request-id ()
  "Generate a unique `request_id' for an outbound `FrontendCommand'.
Combines a monotonic counter with a random suffix.  Isolated so tests
`cl-letf' it for deterministic ids."
  (format "fe-%d-%04x"
          (cl-incf agent-repl--uds-request-id-counter)
          (random #x10000)))

(defun agent-repl--uds-send-command (field payload &optional workspace process)
  "Send a `FrontendCommand' selecting oneof arm FIELD with PAYLOAD.
FIELD is the protojson command name (e.g. \"submitPrompt\"); it must be
one of `agent-repl--uds-known-command-fields'.  PAYLOAD is the plist for
that command message (already protojson-shaped, e.g.
\(:text \"hi\" :permissionMode \"\")); nil PAYLOAD encodes as an empty
message object `{}' (for `closeWorkspace'/`openWorkspace').  WORKSPACE,
when non-nil, is set as the frame's `workspace' field.  PROCESS defaults
to the live connection.

Fails loudly (`user-error' + log) when there is no live connection or
FIELD is unknown — no queuing, no silent drop.  Returns the generated
`request_id'."
  (let ((proc (or process agent-repl--uds-process))
        ;; WORKSPACE goes ON THE WIRE verbatim below — the daemon routes by
        ;; cwd — so it must not be rewritten.  LOG-WORKSPACE is a separate
        ;; binding used only for the log sink.
        (log-workspace (agent-repl--frontend-ws-name workspace)))
    (unless (process-live-p proc)
      (agent-repl--log log-workspace
                       "uds-send-command: NO live connection (field=%s ws=%s) — aborting"
                       field workspace)
      (user-error "agent-repl UDS: not connected; cannot send %s" field))
    (unless (member field agent-repl--uds-known-command-fields)
      (agent-repl--log log-workspace
                       "uds-send-command: unknown command field=%s ws=%s known=%S — aborting"
                       field workspace agent-repl--uds-known-command-fields)
      (user-error "agent-repl UDS: unknown command field %s" field))
    (let* ((request-id (agent-repl--uds-generate-request-id))
           ;; An empty message serializes as `{}', not `null': a nil
           ;; PAYLOAD would encode as JSON null and the daemon could not
           ;; detect the oneof arm.
           (value (or payload (make-hash-table :test 'equal)))
           (frame (append (list :requestId request-id)
                          (when workspace (list :workspace workspace))
                          (list (intern (concat ":" field)) value)))
           (json (json-encode frame)))
      (agent-repl--log log-workspace
                       "uds-send-command: field=%s request-id=%s ws=%s bytes=%d"
                       field request-id workspace (length json))
      (process-send-string proc (concat json "\n"))
      request-id)))

;;;; ---- Command-ack tracking --------------------------------------------
;;
;; A `FrontendCommand' is acknowledged asynchronously by a `CommandAck'
;; frame ({request_id, ok, error}).  Callers that need the ack outcome
;; surfaced (e.g. the merge re-route) record the request via
;; `agent-repl--uds-track-command'; the `commandAck' handler below matches
;; it and, on failure, surfaces loudly (log + echo area) per
;; No-Silent-Fallbacks — a rejected command is never dropped silently.

;;;; ---- Command-link health (Emacs's own fact) --------------------------

(defun agent-repl-uds-link-health ()
  "Return the frontend command link's health: `:healthy' or `:degraded'.

The single reader of `agent-repl--uds-link-health'.  Surfaces are expected
to call this rather than touch the variable, so the two states stay the
whole vocabulary."
  agent-repl--uds-link-health)

(defun agent-repl--uds-link-degrade (request-id field workspace)
  "Mark the command link degraded because REQUEST-ID went unacknowledged.

FIELD and WORKSPACE name the lost command for the transition log.  The
transition is recorded once: a second lost command while already degraded
is logged by its own deadline handler, not by a repeated state change."
  (let ((previous agent-repl--uds-link-health))
    (setq agent-repl--uds-link-health :degraded)
    (agent-repl--log workspace
                     "uds-link-health: %S -> :degraded cause=unacked-command request-id=%s field=%s ws=%s"
                     previous request-id field (or workspace "none"))))

(defun agent-repl--uds-link-restore (reason workspace)
  "Return the command link to `:healthy' because of REASON.

WORKSPACE supplies log context.  A restore from an already-healthy link is
recorded only in the verbose trace: it is the ordinary case (every ack
takes this path) and would otherwise drown the durable log."
  (let ((previous agent-repl--uds-link-health))
    (setq agent-repl--uds-link-health :healthy)
    (if (eq previous :healthy)
        (agent-repl--log-verbose workspace
                                 "uds-link-health: already :healthy reason=%s ws=%s"
                                 reason (or workspace "none"))
      (agent-repl--log workspace
                       "uds-link-health: %S -> :healthy reason=%s ws=%s"
                       previous reason (or workspace "none")))))

;;;; ---- Ack aging -------------------------------------------------------

(defun agent-repl--uds-cancel-ack-deadline (pending)
  "Disarm the ack-aging alarm recorded on PENDING, if it is still armed.
Tolerates a non-timer value so the injected test seam can hand back a
sentinel instead of a real timer object."
  (let ((timer (plist-get pending :deadline-timer)))
    (when (timerp timer)
      (cancel-timer timer))))

(defun agent-repl--uds-command-deadline-expired (request-id)
  "Declare REQUEST-ID lost: no `CommandAck' arrived within the deadline.

Moves the entry to `agent-repl--uds-timed-out-commands', degrades the
command link, and surfaces a locally-classified `client.command_unacked'
failure through the module's one failure channel, so the user learns that
a command they issued went nowhere.

The tracked callbacks are NOT run.  `:on-failure' means \"the daemon
rejected this\", and a command that was never answered was never rejected;
inventing a rejection would hand callers a verdict the daemon never
reached.

A request-id already settled between the alarm firing and this body
running is not lost at all, and is recorded only in the verbose trace."
  (let ((pending (gethash request-id agent-repl--uds-pending-commands)))
    (if (null pending)
        (agent-repl--log-verbose
         nil "uds-ack-deadline: request-id=%s already settled — no timeout"
         request-id)
      (let* ((field (plist-get pending :field))
             (raw-workspace (plist-get pending :workspace))
             (workspace (agent-repl--frontend-ws-name raw-workspace)))
        (remhash request-id agent-repl--uds-pending-commands)
        (puthash request-id (list :field field :workspace raw-workspace)
                 agent-repl--uds-timed-out-commands)
        (agent-repl--log
         workspace
         "uds-ack-deadline: UNACKED request-id=%s field=%s ws=%s deadline=%ss — command lost, callbacks abandoned"
         request-id field (or raw-workspace "none")
         agent-repl-uds-command-ack-deadline)
        (agent-repl--uds-link-degrade request-id field workspace)
        (agent-repl-failure-surface
         workspace
         (agent-repl-failure-local
          "client.command_unacked"
          (format "the daemon never acknowledged the %s command; the daemon link is degraded"
                  (or field "frontend"))
          (format "request-id=%s workspace=%s deadline=%ss"
                  request-id (or raw-workspace "none")
                  agent-repl-uds-command-ack-deadline)))
        request-id))))

(defun agent-repl--uds-track-command (request-id field workspace
                                                 &optional on-failure on-success
                                                 on-challenge)
  "Record REQUEST-ID as an in-flight FIELD command for WORKSPACE.
Pends until its `CommandAck' arrives (see
`agent-repl--uds-handle-command-ack').  ON-FAILURE, when non-nil, is a
function of one argument (the ack error string) run if the ack reports
failure, IN ADDITION to the loud log + echo-area surfacing.  ON-SUCCESS,
when non-nil, is a thunk (no args) run when the ack reports success — the
synchronous createSession bridge uses it to unblock its await loop.
ON-CHALLENGE, when non-nil, is a function of one argument (the ack's
`:interruptConfirmRequired' payload plist) run when the ack carries the
interrupt confirmation CHALLENGE — NOT a failure: the command was
understood and deliberately not performed, so the challenge branch
replaces the failure surfacing rather than adding to it.

Tracking also ARMS an ack-aging alarm for
`agent-repl-uds-command-ack-deadline' seconds (via the injectable
`agent-repl--uds-run-timer' seam, so this is one scheduled alarm per
command rather than any polling).  If the ack has not landed by then,
`agent-repl--uds-command-deadline-expired' declares the command lost.
Returns REQUEST-ID."
  ;; The RETAINED `:workspace' stays raw: it is the identity the ack path
  ;; correlates against, and callers hand it whatever they put on the wire.
  (puthash request-id
           (list :field field :workspace workspace
                 :on-failure on-failure :on-success on-success
                 :on-challenge on-challenge
                 :deadline-timer
                 (agent-repl--uds-run-timer
                  agent-repl-uds-command-ack-deadline
                  (lambda ()
                    (agent-repl--uds-command-deadline-expired request-id))))
           agent-repl--uds-pending-commands)
  (agent-repl--log (agent-repl--frontend-ws-name workspace)
                   "uds-track-command: tracking request-id=%s field=%s ws=%s ack-deadline=%ss"
                   request-id field workspace
                   agent-repl-uds-command-ack-deadline)
  request-id)

(defun agent-repl--uds-untrack-command (request-id workspace reason)
  "Remove REQUEST-ID from pending commands after a local wait aborts.
WORKSPACE supplies log context and REASON records why the caller can no
longer consume a later `CommandAck'.  This is the only transport-owned
cleanup path for a synchronous command wait: retaining the callback after a
timeout would let a delayed acknowledgement mutate stale caller state.

Also disarms the entry's ack-aging alarm: the caller has already stopped
waiting and reported its own outcome, so a later ack-deadline warning
about the same request would be a second account of one event."
  (let ((log-workspace (agent-repl--frontend-ws-name workspace))
        (pending (gethash request-id agent-repl--uds-pending-commands)))
    (if pending
        (progn
          (agent-repl--uds-cancel-ack-deadline pending)
          (remhash request-id agent-repl--uds-pending-commands)
          (agent-repl--log log-workspace
                           "uds-untrack-command: request-id=%s ws=%s reason=%s"
                           request-id workspace reason))
      (agent-repl--log-verbose log-workspace
                               "uds-untrack-command: request-id=%s ws=%s no pending entry reason=%s"
                               request-id workspace reason)))
  nil)

(defun agent-repl--uds-track-health-response
    (request-id field workspace session-id on-response)
  "Await FIELD health result for REQUEST-ID and its expected identities.
WORKSPACE and SESSION-ID are the exact values a `sessionHealth' response
must carry; both are nil for `daemonHealth'.  ON-RESPONSE receives the
validated response plist.  This registry is intentionally separate from
CommandAck tracking: an ACK confirms command receipt, not health."
  ;; The RETAINED `:workspace' stays raw: the response handler compares it
  ;; byte-for-byte against the `workspace' the daemon echoes back, so
  ;; rewriting it here would turn every session-health reply into an
  ;; identity mismatch.
  (let ((log-workspace (agent-repl--frontend-ws-name workspace)))
    (unless (member field '("daemonHealth" "sessionHealth"))
      (agent-repl--log log-workspace
                       "uds-track-health: REFUSING field=%s request-id=%s ws=%s"
                       field request-id workspace)
      (error "agent-repl UDS: cannot track non-health response field %s" field))
    (unless (functionp on-response)
      (agent-repl--log log-workspace
                       "uds-track-health: REFUSING non-function callback=%S request-id=%s ws=%s"
                       on-response request-id workspace)
      (error "agent-repl UDS: health response callback is not callable"))
    (puthash request-id
             (list :field field :workspace workspace :session-id session-id
                   :on-response on-response)
             agent-repl--uds-pending-health-responses)
    (agent-repl--log log-workspace
                     "uds-track-health: request-id=%s field=%s ws=%s session-id=%s"
                     request-id field workspace (or session-id "nil")))
  request-id)

(defun agent-repl--uds-untrack-health-response (request-id workspace reason)
  "Stop awaiting REQUEST-ID's health result for WORKSPACE because of REASON."
  (let ((log-workspace (agent-repl--frontend-ws-name workspace)))
    (if (gethash request-id agent-repl--uds-pending-health-responses)
        (progn
          (remhash request-id agent-repl--uds-pending-health-responses)
          (agent-repl--log log-workspace
                           "uds-untrack-health: request-id=%s ws=%s reason=%s"
                           request-id workspace reason))
      (agent-repl--log-verbose log-workspace
                               "uds-untrack-health: request-id=%s ws=%s no pending entry reason=%s"
                               request-id workspace reason)))
  nil)

(defun agent-repl--uds-handle-health-response (field response)
  "Correlate FIELD health RESPONSE and deliver it to its awaiting caller."
  (let* ((request-id (plist-get response :requestId))
         (response-workspace (plist-get response :workspace))
         ;; Both the echoed and the retained workspace are wire cwds; the
         ;; comparisons below need them raw, the log sink needs a name.
         (log-workspace (agent-repl--frontend-ws-name response-workspace))
         (response-session-id (plist-get response :sessionId))
         (pending (and request-id
                       (gethash request-id
                                agent-repl--uds-pending-health-responses))))
    (unless (and (stringp request-id) (not (string-empty-p request-id)))
      (agent-repl--log log-workspace
                       "uds-health-response: MALFORMED field=%s missing request-id workspace=%S session-id=%S healthy=%S"
                       field response-workspace response-session-id
                       (plist-get response :healthy))
      (signal 'agent-repl-uds-malformed-frame
              (list "health response missing requestId" response)))
    (if (null pending)
        (progn
          ;; A result can legitimately arrive after a local timeout removed
          ;; its waiter.  Record it durably, but never let it satisfy another
          ;; request or mutate abandoned caller state.
          (agent-repl--log log-workspace
                           "uds-health-response: untracked request-id=%s field=%s workspace=%S session-id=%S healthy=%S"
                           request-id field response-workspace response-session-id
                           (plist-get response :healthy))
          nil)
      (let* ((expected-field (plist-get pending :field))
             (expected-workspace (plist-get pending :workspace))
             (expected-log-workspace
              (agent-repl--frontend-ws-name expected-workspace))
             (expected-session-id (plist-get pending :session-id))
             (actual-workspace (plist-get response :workspace))
             (actual-session-id (plist-get response :sessionId)))
        (unless (equal field expected-field)
          (remhash request-id agent-repl--uds-pending-health-responses)
          (agent-repl--log expected-log-workspace
                           "uds-health-response: FIELD MISMATCH request-id=%s expected=%s actual=%s"
                           request-id expected-field field)
          (signal 'agent-repl-uds-malformed-frame
                  (list "health response field mismatch" response)))
        (when (equal field "sessionHealth")
          (unless (and (equal actual-workspace expected-workspace)
                       (equal actual-session-id expected-session-id))
            (remhash request-id agent-repl--uds-pending-health-responses)
            (agent-repl--log expected-log-workspace
                             "uds-health-response: IDENTITY MISMATCH request-id=%s expected-workspace=%S actual-workspace=%S expected-session=%S actual-session=%S"
                             request-id expected-workspace actual-workspace
                             expected-session-id actual-session-id)
            (signal 'agent-repl-uds-malformed-frame
                    (list "session health identity mismatch" response))))
        (remhash request-id agent-repl--uds-pending-health-responses)
        (agent-repl--log expected-log-workspace
                         "uds-health-response: correlated request-id=%s field=%s healthy=%s reason-present=%s"
                         request-id field
                         (if (plist-get response :healthy) "yes" "no")
                         (if (plist-get response :reason) "yes" "no"))
        (condition-case callback-err
            (funcall (plist-get pending :on-response) response)
          (error
           (agent-repl--log expected-log-workspace
                            "uds-health-response: callback ERROR request-id=%s field=%s workspace=%S session-id=%S error-type=%s"
                            request-id field expected-workspace
                            expected-session-id (car callback-err))
           (signal (car callback-err) (cdr callback-err))))
        (agent-repl--log expected-log-workspace
                         "uds-health-response: delivered request-id=%s field=%s workspace=%S session-id=%S"
                         request-id field expected-workspace expected-session-id)
        response))))

(defun agent-repl--uds-handle-daemon-health (response)
  "Handle a correlated `daemonHealth' RESPONSE frame."
  (agent-repl--uds-handle-health-response "daemonHealth" response))

(defun agent-repl--uds-handle-session-health (response)
  "Handle a correlated `sessionHealth' RESPONSE frame."
  (agent-repl--uds-handle-health-response "sessionHealth" response))

(defun agent-repl--uds-handle-command-ack (ack)
  "Handler for the `commandAck' FrontendFrame arm.  ACK is a plist.
Reads `:requestId', `:ok', `:failure' and the legacy `:error'.  protojson
omits a false `ok', so a failed ack decodes with `:ok' nil.

The ECHO comes from `:failure' — the daemon's classified account — not
from `:error', which is an `err.Error()' funnel this end used to print
verbatim: package-prefixed Go text such as `shimclient: request nacked',
shown to a user who has no idea what a shimclient is.  The durable trace
records only whether raw error text was present, so command contents never
leak through a daemon-provided error string.

  ok=t   -> the command was accepted; log and drop the pending entry.
  ok=nil + `:interruptConfirmRequired'
         -> the interrupt confirmation CHALLENGE, not a failure: the
            command was understood and deliberately not performed.  Runs
            the tracked `:on-challenge' callback with the challenge
            payload INSTEAD of the failure surfacing, so a question to
            the user never reads as a command error.  A challenge with no
            tracked handler is still surfaced loudly (it means an
            interrupt went out untracked, and the daemon is waiting on a
            confirmation nobody will send).
  ok=nil -> the command was REJECTED; loud-log AND surface an echo-area
            message (No-Silent-Fallbacks — never a silent drop), run any
            tracked `:on-failure' callback, and drop the entry.

An ack for an UNTRACKED request-id is logged (a command sent without
tracking, or a duplicate ack) — informational, not an error.  Returns
the `:ok' flag."
  (let* ((request-id (plist-get ack :requestId))
         (ok (plist-get ack :ok))
         (err (plist-get ack :error))
         (failure (when-let ((item (plist-get ack :failure)))
                    (agent-repl-failure-from-wire item)))
         (challenge (plist-get ack :interruptConfirmRequired))
         (pending (and request-id
                       (gethash request-id agent-repl--uds-pending-commands)))
         ;; A LATE ack: the deadline already declared this command lost and
         ;; moved its record aside.  Recognized here so the arrival reads as
         ;; what it is rather than as an ack for a request nobody sent.
         (late (and request-id (null pending)
                    (gethash request-id agent-repl--uds-timed-out-commands)))
         (record (or pending late))
         (raw-workspace (if record
                            (plist-get record :workspace)
                          (plist-get ack :workspace)))
         ;; The tracked and the echoed workspace are both wire cwds.  This
         ;; value reaches every log call below AND `agent-repl-failure-surface',
         ;; which logs with it too, so one resolution covers both.
         (workspace (agent-repl--frontend-ws-name raw-workspace))
         (field (plist-get record :field)))
    (unless (and (stringp request-id) (not (string-empty-p request-id)))
      (agent-repl--log workspace
                       "uds-command-ack: MALFORMED missing request-id workspace=%S ok=%S error-present=%s"
                       raw-workspace ok (if err "yes" "no"))
      (signal 'agent-repl-uds-malformed-frame
              (list "command ack missing requestId" ack)))
    (when pending
      (agent-repl--uds-cancel-ack-deadline pending)
      ;; The ack landed inside its deadline, so the command plane is
      ;; demonstrably carrying traffic again.  Receipt is the fact here, not
      ;; the verdict: a REJECTED command still proves the daemon read and
      ;; answered, which is exactly what a degraded link says it did not.
      (agent-repl--uds-link-restore "command-ack" workspace))
    (when request-id
      (remhash request-id agent-repl--uds-pending-commands))
    (cond
     ((null pending)
      (if late
          (progn
            ;; Never an error: a late ack is a slow daemon, not a broken
            ;; one.  The link stays degraded — the deadline really was
            ;; missed, and only an ack that lands inside one clears it.
            (remhash request-id agent-repl--uds-timed-out-commands)
            (agent-repl--log workspace
                             "uds-command-ack: late ack after timeout request-id=%s field=%s ws=%s ok=%S error-present=%s — callbacks already abandoned"
                             request-id field raw-workspace ok
                             (if err "yes" "no")))
        (agent-repl--log workspace
                         "uds-command-ack: UNTRACKED request-id=%s ws=%s ok=%S error-present=%s — ignoring"
                         request-id raw-workspace ok (if err "yes" "no"))))
     (ok
      (agent-repl--log workspace
                       "uds-command-ack: ACCEPTED request-id=%s field=%s"
                       request-id field)
      (when-let ((on-success (plist-get pending :on-success)))
        (condition-case cb-err
            (progn
              (agent-repl--log workspace
                               "uds-command-ack: running on-success request-id=%s field=%s"
                               request-id field)
              (funcall on-success)
              (agent-repl--log workspace
                               "uds-command-ack: completed on-success request-id=%s field=%s"
                               request-id field))
          (error
           (agent-repl--log workspace
                            "uds-command-ack: on-success callback ERROR request-id=%s field=%s error-type=%s"
                            request-id field (car cb-err))))))
     (challenge
      (agent-repl--log workspace
                       "uds-command-ack: CHALLENGE request-id=%s field=%s live-tasks=%S handled=%s"
                       request-id field (plist-get challenge :liveTasks)
                       (if (plist-get pending :on-challenge) "yes" "no"))
      (if-let ((on-challenge (plist-get pending :on-challenge)))
          (condition-case cb-err
              (progn
                (funcall on-challenge challenge)
                (agent-repl--log workspace
                                 "uds-command-ack: completed on-challenge request-id=%s field=%s"
                                 request-id field))
            (error
             (agent-repl--log workspace
                              "uds-command-ack: on-challenge callback ERROR request-id=%s field=%s error-type=%s"
                              request-id field (car cb-err))))
        ;; No handler: the daemon deliberately did not act and nobody will
        ;; answer it.  Never a silent drop.
        (message "agent-repl: %s command needs a confirmation this end cannot ask for"
                 (or field "frontend"))))
     (t
      (agent-repl--log workspace
                       "uds-command-ack: REJECTED request-id=%s field=%s ws=%s error-present=%s — surfacing"
                       request-id field raw-workspace (if err "yes" "no"))
      ;; An ack with no classified failure is an OLD daemon, not a silent
      ;; case: it still surfaces, from the raw text, so a refusal is never
      ;; invisible while the two builds are mixed.
      (if failure
          (agent-repl-failure-surface workspace failure)
        (message "agent-repl: %s command failed: %s"
                 (or field "frontend") (or err "no error detail")))
      (when-let ((on-failure (plist-get pending :on-failure)))
        (condition-case cb-err
            (progn
              (agent-repl--log workspace
                               "uds-command-ack: running on-failure request-id=%s field=%s"
                               request-id field)
              (funcall on-failure (or err "command rejected"))
              (agent-repl--log workspace
                               "uds-command-ack: completed on-failure request-id=%s field=%s"
                               request-id field))
          (error
           (agent-repl--log workspace
                            "uds-command-ack: on-failure callback ERROR request-id=%s field=%s error-type=%s"
                            request-id field (car cb-err)))))))
    ok))

(agent-repl--uds-register-handler "commandAck"
                                  #'agent-repl--uds-handle-command-ack)
(agent-repl--uds-register-handler "daemonHealth"
                                  #'agent-repl--uds-handle-daemon-health)
(agent-repl--uds-register-handler "sessionHealth"
                                  #'agent-repl--uds-handle-session-health)

(provide 'frontend-uds)

;;; frontend-uds.el ends here
