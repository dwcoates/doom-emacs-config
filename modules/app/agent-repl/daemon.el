;;; daemon.el --- Lazy, build-if-stale launch of the claude-repld frontend daemon -*- lexical-binding: t; -*-

;;; Commentary:

;; Makes the agent-repl web frontend self-initialize with no by-hand
;; setup.  Two responsibilities, both driven from a single idempotent
;; entry point (`agent-repl--ensure-frontend-daemon'):
;;
;;   1. Build-if-stale.  Delegates to `bin/build-frontend.sh', which
;;      rebuilds the shim, webapp, and daemon artifacts only when their
;;      sources are newer than the built output (see that script's
;;      header).  Fresh artifacts cost nothing.
;;   2. Launch-once.  Starts a single shared `claude-repld' process via
;;      `make-process', tracked in `agent-repl--frontend-daemon-process'
;;      so repeated calls are no-ops while it is live.
;;
;; The entry point is meant to be called LAZILY -- on the first session
;; open rather than eagerly at `emacs-startup-hook' -- so a user who
;; never touches the web frontend never pays its build or launch cost.
;; It is idempotent and cheap on the hot path (a live-process check),
;; so the session-open path may call it unconditionally.
;;
;; Everything no-ops under `noninteractive' (batch/test) and inside the
;; agent sandbox, mirroring `install.el's auto-install guard.

;;; Code:

(require 'cl-lib)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--in-sandbox-p "agent-repl-install" ())

;;;; ---- Paths ------------------------------------------------------------

(defconst agent-repl--frontend-root
  (let ((module-dir (file-name-directory (or load-file-name
                                              buffer-file-name
                                              default-directory))))
    (file-name-as-directory (expand-file-name "." module-dir)))
  "Absolute path to the `modules/app/agent-repl/' directory.
Anchors the frontend build script and artifact locations.")

(defconst agent-repl--frontend-build-script
  (expand-file-name "bin/build-frontend.sh" agent-repl--frontend-root)
  "Absolute path to the build-if-stale orchestrator script.")

(defconst agent-repl--frontend-shim-entry
  (expand-file-name "shim/dist/main.js" agent-repl--frontend-root)
  "Built shim entrypoint handed to `claude-repld' via --shim.")

(defconst agent-repl--frontend-webapp-dir
  (expand-file-name "webapp/dist" agent-repl--frontend-root)
  "Built webapp static directory handed to `claude-repld' via --webapp.")

(defconst agent-repl--frontend-daemon-bin
  (expand-file-name "daemon/bin/claude-repld" agent-repl--frontend-root)
  "Built `claude-repld' binary produced by the build script.")

(defconst agent-repl--frontend-repo-root
  (expand-file-name "../../../" agent-repl--frontend-root)
  "Checkout containing this module (`modules/app/agent-repl/' is three deep).
Handed to `claude-repld' via -remediation-dir: it is the tree the
\"session gone\" analyst diagnoses and opens its resilience workspace
against.")

;;;; ---- Customization ----------------------------------------------------

(defcustom agent-repl-frontend-auto-start t
  "When non-nil, opening a session lazily ensures the frontend daemon.
The ensure step builds any stale artifact and launches `claude-repld'
if it is not already running.  Set to nil to require the user to run
`agent-repl-frontend-daemon-ensure' by hand."
  :type 'boolean
  :group 'agent-repl)

(defcustom agent-repl-frontend-daemon-addr "127.0.0.1:8787"
  "Listen address passed to `claude-repld' via its --addr flag."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-frontend-build-shell "bash"
  "Shell interpreter used to invoke the frontend build script."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-frontend-node-bin "node"
  "Node binary `claude-repld' uses to run the shim (its --node flag)."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-frontend-remediate-lost-sessions t
  "When non-nil, `claude-repld' remediates a session it has lost.
A frontend whose session has vanished from the daemon shows \"session
gone\", and the daemon answers by dispatching a headless Claude analyst
against `agent-repl--frontend-repo-root': it diagnoses the termination
and opens a workspace that makes the system resilient to that failure
class.  Set to nil to leave the daemon reporting the loss and nothing
more (the -remediation-dir flag is then omitted)."
  :type 'boolean
  :group 'agent-repl)

(defcustom agent-repl-frontend-remediation-permission-mode "bypassPermissions"
  "Permission mode the lost-session analyst runs under.
The analyst is headless, so nobody is at the keyboard to answer a
permission prompt: under the CLI default every tool call it makes is
auto-denied, and it can then only narrate a diagnosis into the daemon
log.  Reading the logs, running git, and driving the workspace-creation
skill all require it to be ungated, which is what the default here buys.
Set to nil to hand the analyst no --permission-mode at all."
  :type '(choice (const :tag "Ungated (can actually remediate)" "bypassPermissions")
                 (const :tag "Edits only" "acceptEdits")
                 (const :tag "CLI default (report-only)" nil)
                 string)
  :group 'agent-repl)

;;;; ---- State ------------------------------------------------------------

(defvar agent-repl--frontend-daemon-process nil
  "The live `claude-repld' process object, or nil when none is running.")

(defconst agent-repl--frontend-daemon-buffer "*claude-repld*"
  "Buffer capturing the daemon process's stdout/stderr.")

(defconst agent-repl--frontend-build-buffer "*agent-repl-build-frontend*"
  "Buffer capturing build-script output.")

;;;; ---- Predicates -------------------------------------------------------

(defun agent-repl--frontend-daemon-live-p ()
  "Return non-nil when the tracked `claude-repld' process is running."
  (and agent-repl--frontend-daemon-process
       (process-live-p agent-repl--frontend-daemon-process)))

(defun agent-repl--frontend-init-inhibited-p ()
  "Return non-nil when automatic frontend init must not run.
No-op under batch (tests) and inside the agent sandbox, matching the
`install.el' auto-install guard."
  (or noninteractive
      (agent-repl--in-sandbox-p)))

;;;; ---- Build-if-stale ---------------------------------------------------

(defun agent-repl--frontend-run-build-script (args)
  "External-boundary wrapper: run the build shell with ARGS, return exit code.
ARGS is the full argument list following the shell interpreter (script
path plus optional flags).  Output is captured into
`agent-repl--frontend-build-buffer'.  Body does nothing but invoke the
external process so tests mock it via `cl-letf'; registered in
`agent-repl--external-boundary-functions'."
  (apply #'call-process ;; ALLOW-EXTERNAL-BOUNDARY
         agent-repl-frontend-build-shell nil
         agent-repl--frontend-build-buffer nil
         args))

(defun agent-repl--frontend-build-if-stale (&optional force)
  "Run the build-if-stale script, building only out-of-date artifacts.
With FORCE non-nil, pass --force so every artifact is rebuilt.  Signals
an error (surfacing the script's output buffer) when the script exits
non-zero, so a build failure is never swallowed."
  (unless (file-exists-p agent-repl--frontend-build-script)
    (error "agent-repl: frontend build script not found: %s"
           agent-repl--frontend-build-script))
  (with-current-buffer (get-buffer-create agent-repl--frontend-build-buffer)
    (erase-buffer))
  (let* ((args (append (list agent-repl--frontend-build-script)
                       (when force '("--force"))))
         (exit-code (agent-repl--frontend-run-build-script args)))
    (agent-repl--log nil "frontend build-if-stale exited %S" exit-code)
    (unless (eq exit-code 0)
      (display-buffer agent-repl--frontend-build-buffer)
      (error "agent-repl: frontend build failed (exit %s) — see %s"
             exit-code agent-repl--frontend-build-buffer))
    exit-code))

;;;; ---- Launch -----------------------------------------------------------

(defun agent-repl--frontend-daemon-command ()
  "Return the argv list used to launch `claude-repld'.
When the system `claude' binary resolves, it is handed to the daemon
via -claude-bin so SDK sessions drive the SAME CLI version as vterm
sessions (and accept its permission modes, e.g. `auto', which the
SDK-bundled cli.js predates).  The same binary drives the headless
\"session gone\" analyst the daemon dispatches from -remediation-dir."
  (append
   (list agent-repl--frontend-daemon-bin
         "-addr"   agent-repl-frontend-daemon-addr
         "-node"   agent-repl-frontend-node-bin
         "-shim"   agent-repl--frontend-shim-entry
         "-webapp" agent-repl--frontend-webapp-dir)
   (when agent-repl-frontend-remediate-lost-sessions
     (append
      (list "-remediation-dir" agent-repl--frontend-repo-root)
      (when agent-repl-frontend-remediation-permission-mode
        (list "-remediation-permission-mode"
              agent-repl-frontend-remediation-permission-mode))))
   (when-let ((claude (executable-find "claude")))
     (list "-claude-bin" claude))))

(defun agent-repl--frontend-spawn-daemon ()
  "External-boundary wrapper: spawn `claude-repld' via `make-process'.
Body does nothing but invoke `make-process' with the daemon argv and
sentinel, returning the live process.  Tests mock it via `cl-letf';
registered in `agent-repl--external-boundary-functions'."
  (make-process ;; ALLOW-EXTERNAL-BOUNDARY
   :name "claude-repld"
   :buffer agent-repl--frontend-daemon-buffer
   :command (agent-repl--frontend-daemon-command)
   :noquery t
   :sentinel #'agent-repl--frontend-daemon-sentinel))

(defun agent-repl--frontend-start-daemon ()
  "Start the `claude-repld' process and track it, returning the process.
Assumes the artifacts are already built; call
`agent-repl--frontend-build-if-stale' first."
  (unless (file-exists-p agent-repl--frontend-daemon-bin)
    (error "agent-repl: daemon binary missing after build: %s"
           agent-repl--frontend-daemon-bin))
  (let ((proc (agent-repl--frontend-spawn-daemon)))
    (setq agent-repl--frontend-daemon-process proc)
    (agent-repl--log nil "claude-repld started (pid %s) on %s"
                      (process-id proc) agent-repl-frontend-daemon-addr)
    proc))

(defun agent-repl--frontend-daemon-sentinel (proc event)
  "Clear the tracked process when PROC dies; EVENT is the status change."
  (unless (process-live-p proc)
    (when (eq proc agent-repl--frontend-daemon-process)
      (setq agent-repl--frontend-daemon-process nil))
    (agent-repl--log nil "claude-repld exited: %s" (string-trim event))))

;;;; ---- Entry point ------------------------------------------------------

(defun agent-repl--ensure-frontend-daemon (&optional force)
  "Ensure the frontend daemon is built and running; return its process.
Idempotent: returns the live process immediately when one exists (unless
FORCE).  Otherwise builds any stale artifact and launches `claude-repld'.
Returns nil without acting when `agent-repl-frontend-auto-start' is nil
or automatic init is inhibited (batch/sandbox)."
  (cond
   ((not agent-repl-frontend-auto-start) nil)
   ((agent-repl--frontend-init-inhibited-p) nil)
   ((and (not force) (agent-repl--frontend-daemon-live-p))
    agent-repl--frontend-daemon-process)
   (t
    (when (and force (agent-repl--frontend-daemon-live-p))
      (agent-repl--frontend-stop-daemon))
    (agent-repl--frontend-build-if-stale force)
    (agent-repl--frontend-start-daemon))))

(defun agent-repl--frontend-stop-daemon ()
  "Kill the tracked `claude-repld' process if it is running."
  (when (agent-repl--frontend-daemon-live-p)
    (delete-process agent-repl--frontend-daemon-process))
  (setq agent-repl--frontend-daemon-process nil))

;;;; ---- Interactive commands ---------------------------------------------

;;;###autoload
(defun agent-repl-frontend-daemon-ensure ()
  "Interactively build-if-stale and start the frontend daemon.
Bypasses `agent-repl-frontend-auto-start' and the batch/sandbox guard
so a user can force initialization on demand."
  (interactive)
  (let ((agent-repl-frontend-auto-start t))
    (cl-letf (((symbol-function 'agent-repl--frontend-init-inhibited-p)
               (lambda () nil)))
      (let ((proc (agent-repl--ensure-frontend-daemon)))
        (message "claude-repld running (pid %s) on %s"
                 (and proc (process-id proc))
                 agent-repl-frontend-daemon-addr)))))

;;;###autoload
(defun agent-repl-frontend-daemon-stop ()
  "Stop the running frontend daemon."
  (interactive)
  (agent-repl--frontend-stop-daemon)
  (message "claude-repld stopped."))

;;;###autoload
(defun agent-repl-frontend-daemon-restart ()
  "Rebuild any stale artifact and restart the frontend daemon."
  (interactive)
  (let ((agent-repl-frontend-auto-start t))
    (cl-letf (((symbol-function 'agent-repl--frontend-init-inhibited-p)
               (lambda () nil)))
      (agent-repl--ensure-frontend-daemon t)
      (message "claude-repld restarted on %s"
               agent-repl-frontend-daemon-addr))))

(provide 'daemon)

;;; daemon.el ends here
