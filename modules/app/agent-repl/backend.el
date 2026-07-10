;;; app/agent-repl/backend.el -*- lexical-binding: t; -*-
;;
;; Pluggable agent-CLI backend registry.
;;
;; agent-repl drives an interactive coding-agent CLI inside a vterm
;; panel.  Historically that CLI was hardwired to Claude Code; this file
;; is the seam that makes the CLI pluggable.  A backend bundles every
;; CLI-specific decision behind a small struct:
;;
;;   - how to build the interactive start command for a workspace
;;     (binary, resume/fork flags, permission flags, config-dir env)
;;
;; Further slots (headless one-shot runner, transcript locator, hook
;; registration) are added as the corresponding subsystems are threaded
;; through the seam.
;;
;; The claude backend is defined here and registered at load time; its
;; implementation functions live in session.el (they predate the seam).
;; A codex backend follows the same shape.
;;
;; Selection: `agent-repl-default-backend' names the default; a
;; workspace may override it via the `:backend' workspace property
;; (set at workspace creation).

(require 'cl-lib)

;;;; ---- Struct -------------------------------------------------------------

(cl-defstruct (agent-repl-backend
               (:constructor agent-repl-backend-create)
               (:copier nil))
  "One agent-CLI backend.

NAME is the identifying symbol (e.g. `claude', `codex').
BINARY is the CLI executable name (e.g. \"claude\").
START-CMD-FN builds the interactive start command: called with one
plist argument carrying `:session-id', `:fork-session-id',
`:project-dir' and `:model' (any of which may be nil except
`:project-dir'), it returns the full shell command string used to
launch the agent inside the workspace vterm."
  name
  binary
  start-cmd-fn)

;;;; ---- Registry -----------------------------------------------------------

(defvar agent-repl--backends (make-hash-table :test #'eq)
  "Registry of known backends, keyed by `agent-repl-backend-name' symbol.")

(defun agent-repl-register-backend (backend)
  "Register BACKEND in `agent-repl--backends', replacing any same-named one.
Signals an error when BACKEND is not an `agent-repl-backend' struct or
when any required slot is missing — a partially-defined backend is a
bug, not a configuration to cope with."
  (unless (agent-repl-backend-p backend)
    (error "agent-repl-register-backend: not a backend struct: %S" backend))
  (dolist (slot '(name binary start-cmd-fn))
    (unless (funcall (intern (format "agent-repl-backend-%s" slot)) backend)
      (error "agent-repl-register-backend: backend %S is missing slot %s"
             (agent-repl-backend-name backend) slot)))
  (puthash (agent-repl-backend-name backend) backend agent-repl--backends))

(defun agent-repl-backend-get (name)
  "Return the registered backend named NAME (a symbol).
Signals an error when no such backend is registered — callers must
never silently fall back to a different CLI."
  (or (gethash name agent-repl--backends)
      (error "agent-repl-backend-get: no backend registered under `%s'" name)))

;;;; ---- Selection ----------------------------------------------------------

(defcustom agent-repl-default-backend 'claude
  "Name of the backend used for workspaces without a `:backend' override."
  :type 'symbol
  :group 'agent-repl)

(defun agent-repl--ws-backend-name (ws)
  "Return the backend name symbol for workspace WS.
The workspace's `:backend' property wins; otherwise
`agent-repl-default-backend'."
  (or (agent-repl--ws-get ws :backend) agent-repl-default-backend))

(defun agent-repl--ws-backend (ws)
  "Return the resolved backend struct for workspace WS.
Signals via `agent-repl-backend-get' when the named backend is not
registered."
  (agent-repl-backend-get (agent-repl--ws-backend-name ws)))

;;;; ---- Claude backend -----------------------------------------------------

;; Implementation functions live in session.el; symbols resolve at call
;; time, so load order between backend.el and session.el is free.

(agent-repl-register-backend
 (agent-repl-backend-create
  :name 'claude
  :binary "claude"
  :start-cmd-fn #'agent-repl--claude-start-cmd))

(provide 'agent-repl-backend)
;;; backend.el ends here
