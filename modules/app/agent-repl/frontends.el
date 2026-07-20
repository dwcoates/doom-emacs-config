;;; frontends.el --- pluggable presentation-frontend registry -*- lexical-binding: t; -*-

;;; Commentary:

;; The FRONTEND axis, mirroring backend.el's agent axis: a backend
;; picks WHICH agent CLI runs (claude, codex), a frontend picks HOW the
;; session is presented and driven.  Today there is exactly one
;; registered frontend, the web GUI (daemon + SDK session + xwidget
;; webview over the classic input panel); the registry itself stays in
;; place as the seam a future second frontend would land in, and as
;; what makes selecting an unsupported pairing fail loudly rather than
;; silently (see the next paragraph).
;;
;; The two axes are orthogonal in the UX (`SPC o c' opens whatever the
;; workspace's frontend is, for whatever its backend is) but NOT in
;; implementation: the gui frontend drives sessions through the claude
;; Agent SDK, so it declares `supported-backends' and selection
;; validates the pair loudly instead of launching a broken hybrid.  This
;; is what makes the still-registered `codex' backend fail loudly when
;; resolved for presentation, rather than silently opening a session no
;; frontend can actually drive.
;;
;; Dispatch points that previously hand-branched on the gui (do-send,
;; the interrupt commands, and — via `agent-repl--frontend-boot-session'
;; — the paths that BIRTH a workspace) resolve through this registry.
;; A workspace carries its frontend in the `:frontend' plist key,
;; defaulting to `agent-repl-default-frontend' — which is the gui: the
;; web view is the out-of-the-box presentation for every workspace
;; (generated, hand-created, restored, or opened cold).
;;
;; `:frontend' is written DELIBERATELY, via
;; `agent-repl--ws-choose-frontend' (which also marks
;; `:frontend-explicit').  Only a deliberate choice is restored across a
;; restart; a workspace that never chose one re-resolves from the
;; default every time, via `agent-repl--frontend-default-for-ws' — so a
;; workspace that has only ever ridden the default follows the default
;; forward if it is ever changed, rather than being pinned to whatever
;; it happened to open under once.

;;; Code:

(require 'cl-lib)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--ws-get "agent-repl-workspace" (ws key))
(declare-function agent-repl--ws-put "agent-repl-workspace" (ws key val))
(declare-function agent-repl--ws-backend-name "agent-repl-backend" (ws))
(declare-function agent-repl--initialize-ws-env "agent-repl-session" (ws &optional project-dir-hint active-env-hint))

;;;; ---- Struct ---------------------------------------------------------------

(cl-defstruct (agent-repl-frontend
               (:constructor agent-repl-frontend-create)
               (:copier nil))
  "One presentation frontend for agent sessions.

NAME is the identifying symbol (e.g. `gui').

OPEN-FN (WS): open/start the frontend for WS — launch whatever the
frontend needs (daemon session + webview) and show it.
BOOT-FN (WS PROJECT-DIR-HINT ACTIVE-ENV-HINT): start WS's session
WITHOUT showing it — the headless half of OPEN-FN, used by the birth
and restore paths (worktree creation, snapshot restore), which run in
the CALLER's frame and must not mount the new workspace's view into
it.  The hints are `agent-repl--initialize-ws-env' hints (the creation
paths know the worktree path and environment before any state file
exists).  The view follows later, when the user actually switches to
WS (`:pending-show-panels').
KILL-FN (WS): destroy WS's session AND its view.
SEND-FN (WS INPUT RAW ON-SETTLE): deliver one prepared user turn.
INPUT is the decorated text actually sent; RAW the undecorated
original (history/posthooks currency).  ON-SETTLE, when non-nil, runs
once the send is committed.
INTERRUPT-FN (WS KIND): interrupt the in-flight turn.  KIND names the
gesture: `ctrl-c' (clear the prompt line) vs `escape' (stop
generation).  Both began as the vterm TUI's two keystrokes; they now
differ by INTENT, since `escape' before the agent has answered is an
undo and may additionally retract the sent turn.  Returns `retracted'
when the frontend withdrew the turn's prompt (the caller then owns that
text and is expected to restore it), or any other non-nil value when
the interrupt merely landed.  Nil means not delivered.
RUNNING-P-FN (WS): non-nil when WS has a live session on this frontend.
SHOW-FN (WS): make an already-running session's view visible.
HIDE-FN (WS): hide the view without killing the session.
RESTART-FN (WS): hard-restart the session.

SUPPORTED-BACKENDS is the list of backend name symbols this frontend
can drive; selection validates the pair against it.  This is what
makes the still-registered `codex' backend fail loudly (via
`agent-repl--frontend-default-for-ws') rather than silently, since the
gui does not list it here.

SUPPORTED-ENVS is the same idea on the environment axis: the list of
`agent-repl--environment-keys' values this frontend can run a session
in.  Every registered frontend currently supports the sole surviving
environment (`:bare-metal'), so the axis rules nothing out today; it
remains the seam a containerized environment would be gated on.
Resolution (`agent-repl--frontend-default-for-ws') and validation
(`agent-repl--frontend-validate-pair') both honor it.

DURABLE-SESSION-ID-FN (WS): the DURABLE claude session uuid of WS's
live session on this frontend, or nil.  ADOPT-SESSION-FN (WS
CLAUDE-SESSION-ID): bind WS to a session resuming that uuid, so the
next open continues the conversation.  Both optional: a frontend
without them simply cannot carry a conversation into an adopting
session.  Currently unexercised with a single registered frontend (the
cross-frontend switch that used to drive them is gone), but kept as
capabilities the registry offers any frontend that lands next to the
gui."
  name
  open-fn
  boot-fn
  kill-fn
  send-fn
  interrupt-fn
  running-p-fn
  show-fn
  hide-fn
  restart-fn
  supported-backends
  supported-envs
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
  (dolist (slot '(name open-fn boot-fn kill-fn send-fn interrupt-fn running-p-fn
                  supported-backends supported-envs))
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

(defcustom agent-repl-default-frontend 'gui
  "Name of the frontend used for workspaces without a `:frontend' override.

The web GUI is the out-of-the-box presentation for EVERY workspace,
however it comes into being: generated (the workspace-generation
dispatch), hand-created (`SPC TAB n'), restored from a snapshot or a
state file, or opened cold with `SPC o c'.  It is currently the ONLY
registered frontend.

The default is CAPABILITY-CONSTRAINED, not absolute: a workspace whose
backend the gui cannot drive (a codex workspace) resolves to whichever
registered frontend can — see `agent-repl--frontend-default-for-ws'.
With only the gui registered, and the gui unable to drive codex, a
codex workspace's resolution currently signals rather than resolving
to anything; this value becomes meaningful again once a second
frontend is registered."
  :type 'symbol
  :group 'agent-repl)

(defun agent-repl--frontend-supports-ws-p (fe backend env)
  "Return non-nil when frontend FE can drive BACKEND in ENV.
ENV nil (the workspace's `:active-env' is not hydrated yet) constrains
nothing — the environment axis only rules a frontend out once the
workspace has actually declared one."
  (and (memq backend (agent-repl-frontend-supported-backends fe))
       (or (null env)
           (memq env (agent-repl-frontend-supported-envs fe)))))

(defun agent-repl--frontend-default-for-ws (ws)
  "Return the frontend name a workspace WS with no `:frontend' is presented under.

`agent-repl-default-frontend' when it can drive WS's backend and
environment; otherwise the first registered frontend that can.

The second clause is capability RESOLUTION, not a fallback that papers
over an error: a codex workspace has no gui to be presented under, so
for it the default names a presentation that does not exist.  Picking
the frontend that CAN present them is the only correct reading of
\"the default\"; there is no failure here to swallow.  When NO
registered frontend can drive the pair, that IS a failure, and it
signals."
  (let* ((backend (agent-repl--ws-backend-name ws))
         (env (agent-repl--ws-get ws :active-env))
         (default (agent-repl-frontend-get agent-repl-default-frontend)))
    (if (agent-repl--frontend-supports-ws-p default backend env)
        (agent-repl-frontend-name default)
      (let ((capable (seq-find (lambda (name)
                                 (agent-repl--frontend-supports-ws-p
                                  (agent-repl-frontend-get name) backend env))
                               (agent-repl--frontend-names))))
        (unless capable
          (error "agent-repl: no registered frontend can drive backend `%s' in env `%s' (ws=%s)"
                 backend env ws))
        (agent-repl--log ws "default-for-ws: %s cannot drive %s/%s — resolving to %s"
                         agent-repl-default-frontend backend env capable)
        capable))))

(defun agent-repl--ws-frontend-name (ws)
  "Return the frontend name symbol for workspace WS.
The workspace's `:frontend' property wins; otherwise the
capability-constrained default (`agent-repl--frontend-default-for-ws')."
  (or (agent-repl--ws-get ws :frontend)
      (agent-repl--frontend-default-for-ws ws)))

(defun agent-repl--ws-choose-frontend (ws name)
  "Record NAME as WS's DELIBERATE frontend choice.

Two writes, and the second is the point: `:frontend' is the resolution
key, while `:frontend-explicit' marks the value as CHOSEN rather than
merely resolved-to.  Only a chosen frontend is restored across an
Emacs restart (`agent-repl--apply-display-state'); a workspace with no
`:frontend' of its own re-resolves from the default every time
\(`agent-repl--frontend-default-for-ws'), which is what lets a
workspace that only ever rode the default follow the default forward
if it is ever changed, instead of being pinned to whatever it happened
to open under once."
  (agent-repl--ws-put ws :frontend name)
  (agent-repl--ws-put ws :frontend-explicit t))

(defun agent-repl--ws-frontend (ws)
  "Return the resolved frontend struct for workspace WS.
Signals via `agent-repl-frontend-get' when the named frontend is not
registered."
  (agent-repl-frontend-get (agent-repl--ws-frontend-name ws)))

(defun agent-repl--ws-gui-frontend-p (ws)
  "Return non-nil when WS's frontend is the web GUI."
  (eq (agent-repl--ws-frontend-name ws) 'gui))

(defun agent-repl--frontend-validate-pair (frontend-name backend-name &optional env)
  "Signal `user-error' unless FRONTEND-NAME can drive BACKEND-NAME in ENV.
ENV, when non-nil, is the workspace's `:active-env'.  Every registered
frontend supports the sole surviving environment, so today only the
BACKEND axis can rule a pair out; the ENV check is what a containerized
environment would be gated on.
Returns t when the combination is valid."
  (let ((fe (agent-repl-frontend-get frontend-name)))
    (unless (memq backend-name (agent-repl-frontend-supported-backends fe))
      (user-error "agent-repl: the %s frontend cannot drive the %s backend (supported: %s)"
                  frontend-name backend-name
                  (mapconcat #'symbol-name
                             (agent-repl-frontend-supported-backends fe) ", ")))
    (when (and env (not (memq env (agent-repl-frontend-supported-envs fe))))
      (user-error "agent-repl: the %s frontend cannot run a %s session (supported: %s)"
                  frontend-name env
                  (mapconcat #'symbol-name
                             (agent-repl-frontend-supported-envs fe) ", ")))
    t))

(defun agent-repl--frontend-validate-for-ws (frontend-name ws)
  "Signal `user-error' unless FRONTEND-NAME can present workspace WS.
The ws-shaped form of `agent-repl--frontend-validate-pair': validates
both capability axes (WS's backend and its `:active-env')."
  (agent-repl--frontend-validate-pair frontend-name
                                      (agent-repl--ws-backend-name ws)
                                      (agent-repl--ws-get ws :active-env)))

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

(defun agent-repl--frontend-dispatch-show (ws)
  "Make WS's already-running session visible through its frontend."
  (funcall (agent-repl-frontend-show-fn (agent-repl--ws-frontend ws)) ws))

(defun agent-repl--frontend-dispatch-hide (ws)
  "Put WS's view away through its frontend, leaving the session running.
Pure view teardown.  The `:repl-state' bookkeeping that goes with a close
is NOT a frontend concern — it belongs to the close commands
\(`agent-repl--on-simple-close' and `agent-repl--on-close'), which own it
once for every frontend rather than leaving each frontend to remember it.
Leaving that to the frontends is exactly how the gui came to close without
ever marking itself `:inactive' or `:hidden', which in turn left hide-mode
unable to sweep it."
  (funcall (agent-repl-frontend-hide-fn (agent-repl--ws-frontend ws)) ws))

(defun agent-repl--frontend-boot-session (ws &optional project-dir-hint active-env-hint)
  "Start WS's agent session under WS's own frontend, WITHOUT showing it.

The single boot door for every path that brings a workspace into
existence or back from disk — worktree creation
\(`agent-repl--setup-worktree-session') and snapshot / project restore
\(`agent-repl--establish-workspace') — so all of them agree on which
frontend a workspace is born under, instead of each hard-wiring its
own boot path and stranding the gui default.

Order matters: the environment is hydrated (PROJECT-DIR-HINT and
ACTIVE-ENV-HINT are `agent-repl--initialize-ws-env' hints) BEFORE the
booting frontend is picked, because `:active-env' is one of the two
axes the frontend resolves against, so a workspace must have declared
its environment before a frontend is chosen for it.

No-op when WS's frontend already has a live session — the restore path
re-establishes workspaces that may already be running.  The
already-running check runs BEFORE the hydration, because
`agent-repl--initialize-ws-env' must never run against a live session
\(it would clobber the instantiation structs' session ids).  A running
workspace always carries the `:active-env' its own boot hydrated, so
the pre-hydration resolution below sees the same frontend the running
session is on."
  (let ((running (agent-repl--ws-frontend ws)))
    (if (funcall (agent-repl-frontend-running-p-fn running) ws)
        (agent-repl--log ws "boot-session: ws=%s already running on %s — skipping"
                         ws (agent-repl-frontend-name running))
      (agent-repl--initialize-ws-env ws project-dir-hint active-env-hint)
      (let ((fe (agent-repl--ws-frontend ws)))
        (agent-repl--log ws "boot-session: ws=%s booting on %s merged=%s merge-completed-at=%s"
                         ws (agent-repl-frontend-name fe)
                         (or (eq (agent-repl--ws-get ws :repl-state) :merged)
                             (eq (agent-repl--ws-get ws :merge-completed) t))
                         (agent-repl--ws-get ws :merge-completed-at))
        (funcall (agent-repl-frontend-boot-fn fe) ws project-dir-hint active-env-hint)))))

(provide 'frontends)

;;; frontends.el ends here
