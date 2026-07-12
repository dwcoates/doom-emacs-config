;;; frontends.el --- pluggable presentation-frontend registry -*- lexical-binding: t; -*-

;;; Commentary:

;; The FRONTEND axis, mirroring backend.el's agent axis: a backend
;; picks WHICH agent CLI runs (claude, codex), a frontend picks HOW the
;; session is presented and driven — the interactive TUI inside a vterm,
;; or the web GUI (daemon + SDK session + xwidget webview over the
;; classic input panel).
;;
;; The two axes are orthogonal in the UX (`SPC o c' opens whatever the
;; workspace's frontend is, for whatever its backend is) but NOT in
;; implementation: the gui frontend drives sessions through the claude
;; Agent SDK, so it declares `supported-backends' and selection
;; validates the pair loudly instead of launching a broken hybrid.
;;
;; Dispatch points that previously hand-branched on the gui (do-send,
;; the interrupt commands) resolve through this registry; the workspace
;; carries its choice in the `:frontend' plist key (persisted like
;; `:backend'), defaulting to `agent-repl-default-frontend'.
;;
;; Choosing a frontend for a workspace is ALSO a statement about what
;; the NEXT workspace should be born with, so both selection commands
;; route through `agent-repl--frontend-adopt-default': it pins the
;; existing default-riding workspaces to what they are already showing,
;; then flips the default forward.  See that function for why the pin
;; is what keeps the flip from reaching backwards.

;;; Code:

(require 'cl-lib)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--ws-get "agent-repl-workspace" (ws key))
(declare-function agent-repl--ws-backend-name "agent-repl-backend" (ws))

;;;; ---- Struct ---------------------------------------------------------------

(cl-defstruct (agent-repl-frontend
               (:constructor agent-repl-frontend-create)
               (:copier nil))
  "One presentation frontend for agent sessions.

NAME is the identifying symbol (e.g. `vterm', `gui').

OPEN-FN (WS): open/start the frontend for WS — launch whatever the
frontend needs (vterm process, daemon session + webview) and show it.
KILL-FN (WS): destroy WS's session AND its view.
SEND-FN (WS INPUT RAW ON-SETTLE): deliver one prepared user turn.
INPUT is the decorated text actually sent; RAW the undecorated
original (history/posthooks currency).  ON-SETTLE, when non-nil, runs
once the send is committed.
INTERRUPT-FN (WS KIND): interrupt the in-flight turn.  KIND
distinguishes the vterm TUI's two interrupt gestures — `ctrl-c'
\(clear prompt line) vs `escape' (stop generation); frontends with a
single wire-level interrupt ignore it.
RUNNING-P-FN (WS): non-nil when WS has a live session on this frontend.
SHOW-FN (WS): make an already-running session's view visible.
HIDE-FN (WS): hide the view without killing the session.
RESTART-FN (WS): hard-restart the session.

SUPPORTED-BACKENDS is the list of backend name symbols this frontend
can drive; selection validates the pair against it.

DURABLE-SESSION-ID-FN (WS): the DURABLE claude session uuid of WS's
live session on this frontend, or nil — the cross-frontend resume
currency `agent-repl-switch-frontend' hands from one frontend to the
other.  ADOPT-SESSION-FN (WS CLAUDE-SESSION-ID): bind WS to a session
resuming that uuid, so the next open continues the conversation.
Both optional: a frontend without them simply cannot carry
conversations across a switch."
  name
  open-fn
  kill-fn
  send-fn
  interrupt-fn
  running-p-fn
  show-fn
  hide-fn
  restart-fn
  supported-backends
  durable-session-id-fn
  adopt-session-fn)

;;;; ---- Registry ---------------------------------------------------------------

(defvar agent-repl--frontends (make-hash-table :test #'eq)
  "Registry of known frontends, keyed by `agent-repl-frontend-name' symbol.")

(defun agent-repl-register-frontend (frontend)
  "Register FRONTEND in `agent-repl--frontends', replacing any same-named one.
Signals when FRONTEND is not an `agent-repl-frontend' struct or a
required slot is missing — a partially-defined frontend is a bug, not
a configuration to cope with."
  (unless (agent-repl-frontend-p frontend)
    (error "agent-repl-register-frontend: not a frontend struct: %S" frontend))
  (dolist (slot '(name open-fn kill-fn send-fn interrupt-fn running-p-fn
                  supported-backends))
    (unless (funcall (intern (format "agent-repl-frontend-%s" slot)) frontend)
      (error "agent-repl-register-frontend: frontend %S is missing slot %s"
             (agent-repl-frontend-name frontend) slot)))
  (puthash (agent-repl-frontend-name frontend) frontend agent-repl--frontends))

(defun agent-repl-frontend-get (name)
  "Return the registered frontend named NAME (a symbol).
Signals when no such frontend is registered — callers must never
silently fall back to a different presentation."
  (or (gethash name agent-repl--frontends)
      (error "agent-repl-frontend-get: no frontend registered under `%s'" name)))

(defun agent-repl--frontend-names ()
  "Return the list of registered frontend name symbols."
  (let (names)
    (maphash (lambda (name _fe) (push name names)) agent-repl--frontends)
    (nreverse names)))

;;;; ---- Selection ---------------------------------------------------------------

(defcustom agent-repl-default-frontend 'vterm
  "Name of the frontend used for workspaces without a `:frontend' override."
  :type 'symbol
  :group 'agent-repl)

(defun agent-repl--ws-frontend-name (ws)
  "Return the frontend name symbol for workspace WS.
The workspace's `:frontend' property wins; otherwise
`agent-repl-default-frontend'."
  (or (agent-repl--ws-get ws :frontend) agent-repl-default-frontend))

(defun agent-repl--ws-frontend (ws)
  "Return the resolved frontend struct for workspace WS.
Signals via `agent-repl-frontend-get' when the named frontend is not
registered."
  (agent-repl-frontend-get (agent-repl--ws-frontend-name ws)))

(defun agent-repl--ws-gui-frontend-p (ws)
  "Return non-nil when WS's frontend is the web GUI."
  (eq (agent-repl--ws-frontend-name ws) 'gui))

(defun agent-repl--frontend-validate-pair (frontend-name backend-name)
  "Signal `user-error' unless FRONTEND-NAME can drive BACKEND-NAME.
Returns t when the pair is valid."
  (let ((fe (agent-repl-frontend-get frontend-name)))
    (unless (memq backend-name (agent-repl-frontend-supported-backends fe))
      (user-error "agent-repl: the %s frontend cannot drive the %s backend (supported: %s)"
                  frontend-name backend-name
                  (mapconcat #'symbol-name
                             (agent-repl-frontend-supported-backends fe) ", ")))
    t))

;;;; ---- Dispatch helpers ----------------------------------------------------------

(defun agent-repl--frontend-dispatch-send (ws input raw &optional on-settle)
  "Send one prepared turn through WS's frontend."
  (funcall (agent-repl-frontend-send-fn (agent-repl--ws-frontend ws))
           ws input raw on-settle))

(defun agent-repl--frontend-dispatch-interrupt (ws kind)
  "Interrupt WS's in-flight turn through its frontend.
KIND is `ctrl-c' or `escape' (see the struct docstring)."
  (funcall (agent-repl-frontend-interrupt-fn (agent-repl--ws-frontend ws))
           ws kind))

;;;; ---- Selection & switching commands ----------------------------------------

(declare-function agent-repl--ws-current-name "agent-repl-workspace" ())
(declare-function agent-repl--ws-put "agent-repl-workspace" (ws key val))
(declare-function agent-repl--live-ws-names "agent-repl-workspace" ())
(declare-function agent-repl--state-save "agent-repl-history" (&optional ws))
(declare-function agent-repl-save-workspace-snapshot "agent-repl-commands" ())

(defun agent-repl--frontend-adopt-default (name)
  "Make NAME the frontend that NEW workspaces are born with.

Two effects, strictly in this order:

  1. PIN every live workspace still riding `agent-repl-default-frontend'
     \(i.e. carrying no `:frontend' of its own) to that CURRENT default,
     persisting each.  Without the pin, step 2 would reach backwards:
     resolution is lazy (`agent-repl--ws-frontend-name'), so a workspace
     that never chose a frontend would silently re-present itself under
     the new one.
  2. FLIP `agent-repl-default-frontend' to NAME and persist it to the
     workspace snapshot, the same cross-restart channel
     `agent-repl-hide-project-dirs-enabled' rides.

The pin is what makes the flip forward-only: workspaces that already
exist keep the frontend they are already being presented under, and
only workspaces created afterwards are born under NAME.

Scope is the LIVE roster.  A tombstoned workspace has no presentation
to preserve, so a later reopen legitimately picks up the new default.

No-op when NAME is already the default."
  ;; Invariant: the default must always name a REGISTERED frontend.  Every
  ;; workspace without a `:frontend' resolves through it, so an unregistered
  ;; default does not fail here — it detonates later, in some unrelated
  ;; workspace's next open.  Fail at the origin instead.
  (agent-repl-frontend-get name)
  (unless (eq name agent-repl-default-frontend)
    (dolist (ws (agent-repl--live-ws-names))
      (unless (agent-repl--ws-get ws :frontend)
        (agent-repl--log ws "adopt-default: pinning ws=%s to %s"
                         ws agent-repl-default-frontend)
        (agent-repl--ws-put ws :frontend agent-repl-default-frontend)
        (agent-repl--state-save ws)))
    (agent-repl--log nil "adopt-default: %s -> %s"
                     agent-repl-default-frontend name)
    (setq agent-repl-default-frontend name)
    (when (fboundp 'agent-repl-save-workspace-snapshot)
      (agent-repl-save-workspace-snapshot))))

;;;###autoload
(defun agent-repl-select-frontend (set-default)
  "Select the presentation frontend for the current workspace.
With prefix argument SET-DEFAULT, set `agent-repl-default-frontend'
\(for workspaces without a `:frontend' override) instead.

Validates the choice against the workspace's backend (the gui drives
only claude today) and refuses to change a workspace whose current
frontend has a RUNNING session — use `agent-repl-switch-frontend' for
a live, conversation-preserving switch.  The choice persists with the
workspace state, mirroring `agent-repl-select-backend'.

The choice ALSO becomes the default that NEW workspaces are born with,
via `agent-repl--frontend-adopt-default' — picking a frontend is a
statement about how you want sessions presented, not just this one.
Already-existing workspaces are unaffected (that function pins them
first)."
  (interactive "P")
  (let* ((names (agent-repl--frontend-names))
         (choice (intern (completing-read
                          (if set-default "Default frontend: " "Workspace frontend: ")
                          (mapcar #'symbol-name names) nil t))))
    (if set-default
        (progn
          (agent-repl--frontend-adopt-default choice)
          (message "agent-repl: default frontend -> %s" choice))
      (let ((ws (agent-repl--ws-current-name)))
        (unless ws
          (user-error "agent-repl-select-frontend: no current workspace"))
        (agent-repl--frontend-validate-pair choice (agent-repl--ws-backend-name ws))
        (when (funcall (agent-repl-frontend-running-p-fn (agent-repl--ws-frontend ws)) ws)
          (user-error "agent-repl-select-frontend: %s has a running session — use agent-repl-switch-frontend for a live switch" ws))
        (agent-repl--ws-put ws :frontend choice)
        (agent-repl--state-save ws)
        (agent-repl--frontend-adopt-default choice)
        (message "agent-repl: %s frontend -> %s (new workspaces too)" ws choice)))))

;;;###autoload
(defun agent-repl-switch-frontend ()
  "Switch the current workspace's frontend IN PLACE.
The literal composition of the manual recipe (close, select, open),
carrying the CONVERSATION across — the frontend is presentation, the
claude session is the shared backend:

  1. CAPTURE the durable claude session uuid from the current
     frontend (its `durable-session-id-fn'), BEFORE the kill destroys
     the session that knows it.
  2. KILL the current frontend's session and view (`SPC o C'
     semantics: close means done), then WAIT (bounded, 3s) for it to
     actually stop — vterm teardown is async (SIGKILL fallback), and
     opening early used to trip already-running guards.
  3. FLIP `:frontend' and persist it (what `agent-repl-select-frontend'
     would do, minus its running-session guard).
  4. ADOPT the captured session uuid into the target frontend (its
     `adopt-session-fn'), so the open below resumes the conversation
     rather than starting a fresh one.  Skipped when either side lacks
     the capability or no session ever ran (nil uuid).
  5. OPEN the other frontend exactly like `SPC o c': SHOW it when its
     session is somehow still alive from before, else open it fresh.
  6. ADOPT the target as the default NEW workspaces are born with
     (`agent-repl--frontend-adopt-default'), leaving every
     already-existing workspace on the frontend it is presenting now.

With exactly two registered frontends the target is the other one;
otherwise it is read by completion."
  (interactive)
  (let ((ws (agent-repl--ws-current-name)))
    (unless ws
      (user-error "agent-repl-switch-frontend: no current workspace"))
    (let* ((from-name (agent-repl--ws-frontend-name ws))
           (others (delq from-name (agent-repl--frontend-names)))
           (to-name (if (= (length others) 1)
                        (car others)
                      (intern (completing-read "Switch frontend to: "
                                               (mapcar #'symbol-name others) nil t)))))
      (agent-repl--frontend-validate-pair to-name (agent-repl--ws-backend-name ws))
      (let ((from (agent-repl-frontend-get from-name))
            (to (agent-repl-frontend-get to-name))
            (durable nil)
            (step "kill"))
        (agent-repl--log ws "switch-frontend: %s -> %s" from-name to-name)
        (condition-case err
            (progn
              ;; Capture the resume currency FIRST: the gui's durable id
              ;; lives in the daemon session the kill below deletes.
              (setq step (format "capture durable id from %s" from-name))
              (when-let ((durable-fn (agent-repl-frontend-durable-session-id-fn from)))
                (setq durable (funcall durable-fn ws))
                (agent-repl--log ws "switch-frontend: durable claude session=%s"
                                 (or durable "none")))
              (setq step (format "kill %s" from-name))
              (agent-repl--log ws "switch-frontend: step=%s" step)
              (funcall (agent-repl-frontend-kill-fn from) ws)
              (setq step (format "await %s stop" from-name))
              (let ((attempts 0))
                (while (and (funcall (agent-repl-frontend-running-p-fn from) ws)
                            (< attempts 30))
                  (setq attempts (1+ attempts))
                  (sleep-for 0.1))
                (when (funcall (agent-repl-frontend-running-p-fn from) ws)
                  (error "agent-repl-switch-frontend: %s still running %.1fs after kill"
                         from-name (* attempts 0.1))))
              (setq step "flip+persist")
              (agent-repl--ws-put ws :frontend to-name)
              (agent-repl--state-save ws)
              ;; Same step, wider scope: the switch also states what the
              ;; NEXT workspace should be born with.  Runs AFTER the
              ;; `:frontend' put above so the pin inside sees this
              ;; workspace as already-chosen and skips it.
              (setq step "adopt-default")
              (agent-repl--frontend-adopt-default to-name)
              ;; Hand the conversation to the target frontend before the
              ;; open, so it resumes the captured claude session instead
              ;; of starting fresh.
              (when durable
                (when-let ((adopt-fn (agent-repl-frontend-adopt-session-fn to)))
                  (setq step (format "adopt session into %s" to-name))
                  (agent-repl--log ws "switch-frontend: step=%s" step)
                  (funcall adopt-fn ws durable)))
              (if (funcall (agent-repl-frontend-running-p-fn to) ws)
                  (progn
                    (setq step (format "show %s" to-name))
                    (agent-repl--log ws "switch-frontend: step=%s (already running)" step)
                    (funcall (agent-repl-frontend-show-fn to) ws))
                (setq step (format "open %s" to-name))
                (agent-repl--log ws "switch-frontend: step=%s" step)
                (funcall (agent-repl-frontend-open-fn to) ws)))
          (error
           (agent-repl--log ws "switch-frontend: ERROR at step=%s: %s"
                            step (error-message-string err))
           (signal (car err) (cdr err))))
        (message "agent-repl: %s frontend -> %s" ws to-name)))))

(provide 'frontends)

;;; frontends.el ends here
