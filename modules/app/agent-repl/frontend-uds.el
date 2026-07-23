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

;;;; ---- Frame vocabulary ------------------------------------------------

(defconst agent-repl--uds-known-frame-fields
  '("snapshot" "workspaceState" "sessionView" "conversationDelta"
    "typingDelta" "taskCatalog" "commandAck" "degradedNotice")
  "The protojson (lowerCamelCase) names of every `FrontendFrame' oneof arm.
Mirrors the `frame' oneof in proto/agentshim/frontend/v1/frontend.proto.
A decoded frame whose sole top-level key is NOT one of these is
malformed (unknown wire field) and signals loudly.")

(defconst agent-repl--uds-known-command-fields
  '("submitPrompt" "interrupt" "permissionAnswer" "mergeWorkspace"
    "closeWorkspace" "openWorkspace" "resync")
  "The protojson names of every `FrontendCommand' oneof arm.
Mirrors the `command' oneof in frontend.proto.  Sending an unknown
command field is a programming error and fails loudly.")

(defvar agent-repl--uds-frame-handlers nil
  "Alist mapping a `FrontendFrame' oneof field name (string) to a handler fn.
The handler is called with ONE argument: the plist decoded from that
arm's protojson value.  Populated via `agent-repl--uds-register-handler'
(e.g. `frontend-state.el' registers `workspaceState'/`snapshot'/
`degradedNotice').  A known field with no entry here is logged, not
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

;;;; ---- Connection lifecycle --------------------------------------------

(defun agent-repl--uds-connected-p ()
  "Return non-nil iff the frontend UDS link is live."
  (process-live-p agent-repl--uds-process))

(cl-defun agent-repl-uds-connect (&optional path)
  "Establish (or re-establish) the frontend UDS connection.
PATH defaults to `agent-repl-uds-socket-path'.  On success the daemon
pushes a `StateSnapshot' first, then deltas (design §5.4) — this client
does not poll.  On dial failure the error is loud-logged and a reconnect
is scheduled (design §4.4 honest downtime); the connection is left nil
and the display goes stale until the daemon is reachable.  Returns the
process on success, nil on a failed dial."
  (interactive)
  (when (agent-repl--uds-connected-p)
    (agent-repl--log nil "uds-connect: already connected (proc=%s) — no-op"
                     (process-name agent-repl--uds-process))
    (cl-return-from agent-repl-uds-connect agent-repl--uds-process))
  (let ((sock (or path agent-repl-uds-socket-path)))
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
          proc)
      (error
       (setq agent-repl--uds-process nil)
       (agent-repl--log nil
                        "uds-connect: FAILED dialing %s: %S — scheduling reconnect in %ss"
                        sock err agent-repl-uds-reconnect-delay)
       (message "agent-repl UDS: daemon unreachable at %s (%s); reconnecting"
                sock (error-message-string err))
       (agent-repl--uds-schedule-reconnect)
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
  (setq agent-repl--uds-read-accumulator
        (concat agent-repl--uds-read-accumulator chunk))
  (let (line)
    (while (setq line (agent-repl--uds-next-line))
      (agent-repl--uds-handle-line line))))

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
                      "uds-decode-frame: MALFORMED json (%d bytes): %S — line=%s"
                      (length line) err (agent-repl--uds-truncate line))
     (signal 'agent-repl-uds-malformed-frame
             (list (error-message-string err) line)))))

(defun agent-repl--uds-dispatch-frame (frame)
  "Dispatch decoded FRAME (a one-key plist) to its registered handler.
The single top-level key names the `FrontendFrame' oneof arm.  A frame
with no oneof field, or an unknown one, is loud-logged and SIGNALS
`agent-repl-uds-malformed-frame'.  A KNOWN field with no registered
handler is loud-logged (a not-yet-wired condition surfaced honestly),
not silently dropped.  Returns the handler's value, or nil."
  (unless (and (consp frame) (keywordp (car frame)))
    (agent-repl--log nil "uds-dispatch: MALFORMED frame — no oneof key: %S" frame)
    (signal 'agent-repl-uds-malformed-frame
            (list "frame carries no oneof field" frame)))
  (let* ((key (car frame))
         (field (substring (symbol-name key) 1))
         (payload (plist-get frame key)))
    (cond
     ((not (member field agent-repl--uds-known-frame-fields))
      (agent-repl--log nil
                       "uds-dispatch: MALFORMED frame — unknown oneof field=%s known=%S"
                       field agent-repl--uds-known-frame-fields)
      (signal 'agent-repl-uds-malformed-frame
              (list (format "unknown oneof field: %s" field) frame)))
     (t
      (let ((handler (cdr (assoc field agent-repl--uds-frame-handlers))))
        (if handler
            (progn
              (agent-repl--log nil "uds-dispatch: field=%s -> handler=%s" field handler)
              (funcall handler payload))
          (agent-repl--log nil
                           "uds-dispatch: field=%s KNOWN but no handler registered — not dispatched (register one at stitch)"
                           field)
          nil))))))

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
  (let ((proc (or process agent-repl--uds-process)))
    (unless (process-live-p proc)
      (agent-repl--log workspace
                       "uds-send-command: NO live connection (field=%s) — aborting"
                       field)
      (user-error "agent-repl UDS: not connected; cannot send %s" field))
    (unless (member field agent-repl--uds-known-command-fields)
      (agent-repl--log workspace
                       "uds-send-command: unknown command field=%s known=%S — aborting"
                       field agent-repl--uds-known-command-fields)
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
      (agent-repl--log workspace
                       "uds-send-command: field=%s request-id=%s ws=%s bytes=%d"
                       field request-id workspace (length json))
      (process-send-string proc (concat json "\n"))
      request-id)))

(provide 'frontend-uds)

;;; frontend-uds.el ends here
