;;; permission.el --- permission UX from pushed frontend.v1 state -*- lexical-binding: t; -*-

;;; Commentary:

;; The Emacs-side permission surface, driven ENTIRELY by daemon-pushed
;; `frontend.v1' state (design §5.4, §10; S8/S9 sentinel endgame).  It
;; replaces the deleted permission sentinels + managed Claude Code hooks
;; (`permission_request' / `permission_prompt' / `permission_resolved',
;; and the `Notification' / `PermissionRequest' hooks): Emacs no longer
;; learns about permission prompts from harness hooks at all.
;;
;; The daemon composes each `canUseTool' round-trip into a
;; `core.v1.PermissionItem' and pushes it as a `permission' arm of a
;; `ConversationDelta' (item uuid == the permission request id), with a
;; resolution lifecycle:
;;
;;   RESOLUTION_PENDING   -> a prompt is waiting on an answer.  This file
;;                           records it under the workspace's
;;                           `:permission-prompt-active' key (the existing
;;                           permission UI surface) AND fires Emacs's own
;;                           desktop notification (the user-visible effect
;;                           the deleted `Notification' hook used to have).
;;   RESOLUTION_ALLOWED   -> answered allow; clears the prompt.
;;   RESOLUTION_DENIED    -> answered deny; clears the prompt, may echo the
;;                           daemon's deny message.
;;   RESOLUTION_ABANDONED -> the turn ended / session torn down before an
;;                           answer; clears the prompt SILENTLY (the shim
;;                           re-asks on reattach, replayed off the retained
;;                           ring — no special-casing here).
;;
;; The `:permission' render state itself is pushed as a `WorkspaceState'
;; (handled in `frontend-state.el'); this file owns only the prompt
;; bookkeeping, the notification, and the answer round-trip.
;;
;; Answers travel back over the UDS as a `PermissionAnswerCmd'
;; \(permissionRequestId / allow / updatedInput / denyMessage) — the same
;; command the webapp answers with.  Reconnect re-presentation needs no
;; special handling: the retained-ring resync replays the pending item and
;; re-drives this handler; the per-uuid idempotency guard keeps a replay of
;; an already-active prompt from re-notifying.
;;
;; No-Silent-Fallbacks (AGENTS.md): a permission item with no workspace, or
;; with an unknown/absent resolution, is loud-logged and SIGNALS — never a
;; defaulted value.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--log-verbose "core" (ws fmt &rest args))
(declare-function agent-repl--ws-get "workspace" (ws key))
(declare-function agent-repl--ws-put "workspace" (ws key val))
(declare-function agent-repl--frontend-ws-name "frontend-state" (workspace))
(declare-function agent-repl--ws-current-name "workspace" ())
(declare-function agent-repl--notify "notifications" (ws title message))
(declare-function agent-repl--uds-register-handler "frontend-uds" (field fn))
(declare-function agent-repl--uds-send-command "frontend-uds" (field payload &optional workspace process))
(declare-function agent-repl--uds-track-command "frontend-uds" (request-id field workspace &optional on-failure on-success))
(declare-function agent-repl--frontend-ws-command-key "frontend-client" (ws))

;;;; ---- Resolution vocabulary -------------------------------------------

(defconst agent-repl--permission-resolution-pending "RESOLUTION_PENDING"
  "protojson enum name for an unanswered `PermissionItem'.")

(defconst agent-repl--permission-resolutions-terminal
  '("RESOLUTION_ALLOWED" "RESOLUTION_DENIED" "RESOLUTION_ABANDONED")
  "protojson enum names for the answered/abandoned `PermissionItem' states.
Each clears the active prompt for the item's workspace when its uuid
matches.  RENDER-agnostic: the `:permission' render state itself is
carried by the pushed `WorkspaceState', not by these.")

;;;; ---- ConversationDelta permission handling ---------------------------

(defun agent-repl--permission-item-p (item)
  "Return non-nil when ITEM (a `ConversationItem' plist) is a permission arm.
protojson emits ONLY the set oneof arm, so a `permission' key present
means this item is a `PermissionItem'; every other conversation item
type (assistant/user message, tool use/result, …) is for the webapp feed
and carries a different arm key, which Emacs ignores."
  (and (plist-member item :permission) t))

(defun agent-repl--frontend-apply-conversation-delta (delta)
  "Apply a `ConversationDelta' frame DELTA (a plist).  Handler for `conversationDelta'.
Emacs is state-centric: of the delta's typed items it consumes ONLY the
`permission' arm (the daemon-composed `PermissionItem'); every other item
type is the webapp's to render and is skipped here.  Each permission item
is dispatched to `agent-repl--permission-handle-item' with the delta's
workspace.  Returns the count of permission items handled."
  ;; The frame names its workspace by the session CWD; Emacs keys workspaces
  ;; by persp NAME. Handing the path straight on would key the permission
  ;; prompt to a stub the renderer never reads (see
  ;; `agent-repl--frontend-ws-name').
  (let* ((raw-workspace (plist-get delta :workspace))
         (resolved (agent-repl--frontend-ws-name raw-workspace))
         ;; DISPATCH keeps the `or' fallback: `agent-repl--permission-handle-item'
         ;; fails loudly on a blank workspace, and degrading an unresolvable
         ;; path to nil would turn that loud refusal into a silent one.
         (workspace (or resolved raw-workspace))
         ;; The LOG sink cannot: a cwd never indexes the workspace hash, and
         ;; this handler runs inside the connection's process filter.  An
         ;; unresolvable delta is a global line naming its raw path.
         (log-workspace resolved)
         (items (plist-get delta :items))
         (handled 0))
    ;; Conversation deltas can arrive in rapid succession while a turn is
    ;; streaming, so retain their frame-level trace only in verbose mode.
    ;; Do not log ITEM: it can contain conversation and tool-argument data.
    (agent-repl--log-verbose
     log-workspace
     "permission-delta: wire-workspace=%s workspace-source=%s item-count=%d"
     raw-workspace
     (if resolved "resolved" "raw")
     (length items))
    (dolist (item items)
      (when (agent-repl--permission-item-p item)
        (agent-repl--permission-handle-item workspace item)
        (cl-incf handled)))
    (if (> handled 0)
        (agent-repl--log log-workspace
                         "frontend-apply-conversation-delta: ws=%s handled %d permission item(s)"
                         workspace handled)
      (agent-repl--log-verbose log-workspace
                               "permission-delta: no permission items handled (ws=%s)"
                               workspace))
    handled))

(defun agent-repl--permission-handle-item (workspace item)
  "Dispatch permission ITEM for WORKSPACE on its resolution.
ITEM is a `ConversationItem' plist whose `:permission' arm is a
`PermissionItem' (request + resolution).  Fails loudly (No-Silent-Fallbacks)
when WORKSPACE is missing/blank or the resolution is unknown/absent —
never a defaulted value.  Returns the resolution keyword acted on."
  (when (or (null workspace) (and (stringp workspace) (string-empty-p workspace)))
    ;; Never print ITEM: its request input may contain sensitive tool arguments.
    (agent-repl--log nil
                     "permission-handle-item: request=%s workspace=missing — no fallback"
                     (plist-get item :uuid))
    (error "agent-repl permission: ConversationItem permission arm missing workspace"))
  (let* ((uuid (plist-get item :uuid))
         (perm (plist-get item :permission))
         (request (plist-get perm :request))
         (resolution (plist-get perm :resolution))
         (deny-message (plist-get perm :denyMessage))
         (tool-name (plist-get request :toolName))
         (input (plist-get request :input)))
    (cond
     ((equal resolution agent-repl--permission-resolution-pending)
      (agent-repl--permission-present workspace uuid tool-name input)
      :pending)
     ((member resolution agent-repl--permission-resolutions-terminal)
      (agent-repl--permission-clear workspace uuid resolution deny-message)
      (intern (concat ":" (downcase (or resolution "")))))
     (t
     ;; WORKSPACE reaches here as whatever the delta carried — a resolved
     ;; name, or the raw cwd the dispatch fallback preserved.  The sink only
     ;; indexes names, so resolve for the log and name the raw value in it.
     (agent-repl--log (agent-repl--frontend-ws-name workspace)
                       "permission-handle-item: ws=%s request=%s resolution=%S unknown — no fallback"
                       workspace uuid resolution)
      (error "agent-repl permission: unknown resolution %S" resolution)))))

;;;; ---- Present / clear the prompt --------------------------------------

(defun agent-repl--permission-present (ws uuid tool-name input)
  "Record a PENDING permission prompt for WS and fire a desktop notification.
UUID is the permission request id, TOOL-NAME the requested tool, INPUT its
argument Struct (a plist).  Stores the prompt under the existing
`:permission-prompt-active' workspace key so the answer command and a
later resolution update can find it.

Idempotent per uuid: when the workspace already has the SAME uuid active
\(a retained-ring replay after reconnect re-drives the same item), this is
a no-op and does NOT re-notify.  Returns the recorded prompt plist, or nil
when it was already active."
  ;; WS is the delta's workspace, which is a resolved persp name when one
  ;; owns the cwd and the raw cwd otherwise.  It stays as-is for the plist
  ;; key; the log sink only indexes names, so it gets the resolution.
  (let ((active (agent-repl--ws-get ws :permission-prompt-active))
        (log-ws (agent-repl--frontend-ws-name ws)))
    (if (and active (equal (plist-get active :request-id) uuid))
        (progn
          (agent-repl--log log-ws
                           "permission-present: ws=%s request=%s already-active=t — no re-notify"
                           ws uuid)
          nil)
      (let ((prompt (list :request-id uuid :tool-name tool-name :input input)))
        (agent-repl--log log-ws
                         "permission-present: ws=%s request=%s replaced-active=%s tool-present=%s — presenting + notifying"
                         ws uuid (if active "t" "nil") (if tool-name "t" "nil"))
        (agent-repl--permission-notify ws uuid tool-name)
        ;; Notify before changing workspace state: a notification failure must
        ;; abort presentation without leaving a prompt that cannot be surfaced.
        (agent-repl--ws-put ws :permission-prompt-active prompt)
        (agent-repl--log log-ws "permission-present: ws=%s request=%s active-prompt-recorded=t"
                         ws uuid)
        prompt))))

(defun agent-repl--permission-notify (ws request-id tool-name)
  "Fire Emacs's own desktop notification that WS is awaiting a permission answer.
REQUEST-ID identifies the permission lifecycle and TOOL-NAME names the requested tool.
Uses `agent-repl--notify' — the same
focus-gated desktop-notification mechanism the module already uses for the
agent-finished banner — so no banner appears while the user is already
looking at Emacs.  Replaces the notification the deleted `Notification'
managed hook used to fire."
  ;; The banner text keeps WS verbatim — it is what the user recognizes.
  ;; Only the log sink needs the resolved name.
  (let ((log-ws (agent-repl--frontend-ws-name ws)))
    (agent-repl--log log-ws "permission-notify: ws=%s request=%s dispatching" ws request-id)
    (condition-case err
        (progn
          (agent-repl--notify ws "Agent REPL"
                              (format "%s: permission requested%s"
                                      ws
                                      (if (and tool-name (not (string-empty-p tool-name)))
                                          (format " — %s" tool-name)
                                        "")))
          (agent-repl--log log-ws "permission-notify: ws=%s request=%s dispatched"
                           ws request-id))
      (error
       (agent-repl--log log-ws "permission-notify: ws=%s request=%s failed error-type=%s"
                        ws request-id (car err))
       (signal (car err) (cdr err))))))

(defun agent-repl--permission-clear (ws uuid resolution deny-message)
  "Clear WS's active permission prompt when its uuid matches UUID.
RESOLUTION is the terminal protojson enum name.  A `RESOLUTION_DENIED' may
echo DENY-MESSAGE to the echo area (when non-empty); `RESOLUTION_ABANDONED'
clears SILENTLY (log only); `RESOLUTION_ALLOWED' logs only.  A resolution
whose uuid does not match the active prompt (already cleared, or never seen
by this Emacs — e.g. resolved before connect) is a benign no-op, logged,
never an error.  Returns non-nil when a prompt was cleared."
  ;; As in `agent-repl--permission-present': WS keys the plist verbatim, the
  ;; resolved name scopes the log sink.
  (let ((active (agent-repl--ws-get ws :permission-prompt-active))
        (log-ws (agent-repl--frontend-ws-name ws)))
    (if (and active (equal (plist-get active :request-id) uuid))
        (progn
          (agent-repl--ws-put ws :permission-prompt-active nil)
          (agent-repl--log log-ws "permission-clear: ws=%s uuid=%s resolution=%s — cleared"
                           ws uuid resolution)
          (when (and (equal resolution "RESOLUTION_DENIED")
                     (stringp deny-message)
                     (not (string-empty-p deny-message)))
            (agent-repl--log log-ws
                             "permission-clear: ws=%s request=%s resolution=denied deny-message-present=t — echoing"
                             ws uuid)
            (message "agent-repl: permission denied — %s" deny-message))
          t)
      (agent-repl--log log-ws
                       "permission-clear: ws=%s uuid=%s resolution=%s — no matching active prompt (already cleared / never presented)"
                       ws uuid resolution)
      nil)))

;;;; ---- Answering over UDS ----------------------------------------------

(defun agent-repl--send-permission-answer (ws request-id allow &optional updated-input deny-message)
  "Send a `PermissionAnswerCmd' for REQUEST-ID over the UDS, keyed by WS.
The wire key is WS's cwd (`agent-repl--frontend-ws-command-key'), not the
persp name: the daemon routes the answer to the live session whose CWD
equals it, and a bare name matches nothing.
ALLOW non-nil answers allow; nil answers deny.  UPDATED-INPUT, when
non-nil, is an allow-with-edits Struct (a plist) sent as `updatedInput'.
DENY-MESSAGE, when non-nil on a deny, travels as `denyMessage'.  A false
  `allow' is OMITTED from the frame rather than sent as JSON false, matching
protojson's default-omission (the daemon reads an absent bool as deny).
Tracks the command so a rejected ack surfaces loudly.  Returns the
request id of the sent command."
  (let ((payload (append (list :permissionRequestId request-id)
                         (when allow (list :allow t))
                         (when updated-input (list :updatedInput updated-input))
                         (when (and (not allow) deny-message
                                    (not (string-empty-p deny-message)))
                           (list :denyMessage deny-message)))))
    ;; Payload values can contain edited tool input or a user-written denial;
    ;; retain only their presence, never their contents.
    (agent-repl--log ws
                     "send-permission-answer: request=%s allow=%s updated-input-present=%s deny-message-present=%s dispatching"
                     request-id (if allow "yes" "no")
                     (if updated-input "t" "nil")
                     (if (and (not allow) deny-message
                              (not (string-empty-p deny-message))) "t" "nil"))
    (let ((req (condition-case err
                   (agent-repl--uds-send-command
                    "permissionAnswer" payload
                    (agent-repl--frontend-ws-command-key ws))
                 (error
                  (agent-repl--log ws
                                   "send-permission-answer: request=%s uds-send-failed error-type=%s"
                                   request-id (car err))
                  (signal (car err) (cdr err))))))
      (condition-case err
          (agent-repl--uds-track-command req "permissionAnswer" ws)
        (error
         (agent-repl--log ws
                          "send-permission-answer: request=%s uds-request=%s tracking-failed error-type=%s"
                          request-id req (car err))
         (signal (car err) (cdr err))))
      (agent-repl--log ws "send-permission-answer: ws=%s request=%s uds-request=%s allow=%s"
                       ws request-id req (if allow "yes" "no"))
      req)))

;;;###autoload
(defun agent-repl-answer-permission (&optional ws)
  "Answer the active permission prompt for workspace WS over the UDS.
Reads WS's `:permission-prompt-active' (set from the pushed pending
`PermissionItem'), prompts allow/deny (deny optionally with a message),
and sends the answer as a `PermissionAnswerCmd'.  The prompt is NOT
cleared optimistically — the daemon pushes a resolution update that
clears it, keeping pushed state the single source of truth.  Signals
`user-error' when WS has no active permission prompt.  Defaults to the
current workspace."
  (interactive)
  (let* ((ws (or ws (agent-repl--ws-current-name)))
         (prompt (and ws (agent-repl--ws-get ws :permission-prompt-active))))
    (unless prompt
      (agent-repl--log ws "answer-permission: active-prompt=missing — rejecting command")
      (user-error "agent-repl: no active permission prompt in workspace '%s'" ws))
    (let* ((request-id (plist-get prompt :request-id))
           (tool-name (plist-get prompt :tool-name))
           (allow (y-or-n-p (format "Allow permission for %s in '%s'? "
                                    (or tool-name "tool") ws)))
           (deny-message (unless allow
                           (let ((m (read-string "Deny message (optional): ")))
                             (unless (string-empty-p m) m)))))
      (agent-repl--log ws "answer-permission: request=%s allow=%s deny-message-present=%s"
                       request-id (if allow "yes" "no") (if deny-message "t" "nil"))
      (agent-repl--send-permission-answer ws request-id allow nil deny-message)
      (message "agent-repl: %s permission for %s"
               (if allow "allowed" "denied") (or tool-name "tool")))))

;;;; ---- Handler registration --------------------------------------------
;;
;; Registered on the transport (`frontend-uds.el', loaded first per
;; config.el order).  Emacs is the only frontend that reduces a
;; ConversationDelta to just its permission items; the webapp renders the
;; full typed feed.

(agent-repl--uds-register-handler "conversationDelta"
                                  #'agent-repl--frontend-apply-conversation-delta)

(provide 'permission)

;;; permission.el ends here
