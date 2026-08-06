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
;; Everything no-ops under `noninteractive' (batch/test), mirroring
;; `install.el's auto-install guard.

;;; Code:

(require 'cl-lib)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--log-verbose "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--error "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--doctor-log "agent-repl-doctor" (fmt &rest args))
(declare-function agent-repl--frontend-turn-active-sessions "agent-repl-frontend-client" ())
(declare-function agent-repl-runtime-restart "services" (&optional stop-shims))
(declare-function agent-repl-runtime-restart-dispatch "services" (&optional stop-shims timeout))
(declare-function agent-repl--uds-connected-p "frontend-uds" ())
(declare-function agent-repl-uds-probe-async "frontend-uds" (path on-open on-failure))
(declare-function agent-repl--uds-run-timer "frontend-uds" (seconds function &rest args))
(declare-function agent-repl-uds-connect "frontend-uds" (&optional path readiness-p))
(declare-function agent-repl--uds-send-command "frontend-uds" (field payload &optional workspace process &rest keys))
(declare-function agent-repl--frontend-daemon-view-binary-mtime-seconds "frontend-state" ())
(declare-function agent-repl-frontend-shutdown-schedule "frontend-state" ())
(declare-function agent-repl-frontend-scheduled-shutdown-id "frontend-state" ())

;; Forward declaration: this defcustom lives in session.el, which loads
;; AFTER this file.  Declared here so byte-compilation doesn't warn about a
;; free variable in `agent-repl--frontend-accounts-flag'; it is always bound
;; by the time that function runs (daemon launch).  Mirrors install.el.
(defvar agent-repl-multi-repo-config-dir)

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

(defconst agent-repl--frontend-deploy-script
  (expand-file-name "bin/deploy-all.sh" agent-repl--frontend-root)
  "Absolute path to the whole-stack deploy orchestrator.
Supersedes `agent-repl--frontend-build-script' on the boot path: that one
covers the shim bundle, the webapp, and the daemon, and MISSES the two
things that bit a live deploy — protobuf regeneration (whose generated Go
lives outside `daemon/', so the daemon\='s own staleness check cannot see it)
and the launchd-managed shim-store and sidecar.")

(defconst agent-repl--frontend-shim-entry
  (expand-file-name "agent-shim/claude/shim/dist/main.js"
                    agent-repl--frontend-root)
  "Built shim entrypoint handed to `claude-repld' via --shim.")

(defconst agent-repl--frontend-webapp-dir
  (expand-file-name "webapp/dist" agent-repl--frontend-root)
  "Built webapp static directory handed to `claude-repld' via --webapp.")

(defconst agent-repl--frontend-daemon-bin
  (expand-file-name "daemon/bin/claude-repld" agent-repl--frontend-root)
  "Built `claude-repld' binary produced by the build script.")

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

(defcustom agent-repl-frontend-stop-grace-seconds 3.0
  "Seconds to wait for `claude-repld' to exit after SIGTERM.
The daemon's TERM handler drains its sessions and flushes the session
registry; only when the process outlives this window does
`agent-repl--frontend-stop-daemon' fall back to `delete-process'
\(SIGKILL).  The registry is write-through crash-safe, so the fallback
loses nothing durable — the grace window just lets sessions drain
cleanly."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-frontend-foreign-stop-grace-seconds 5.0
  "Seconds to wait for an ADOPTED daemon to exit after a `ShutdownCmd'.
The staleness bounce uses this only for a daemon this Emacs does not
track (no local process handle): it asks the daemon to shut down over the
UDS, then polls the socket until it frees before spawning a fresh one, so
the replacement never bind-fails next to a still-listening daemon.  A
daemon that outlives this window is left in place (manual restart)."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-frontend-widget-assets-dir
  (or (getenv "AGENT_REPL_WIDGET_ASSETS") "")
  "Directory of embeddable-widget assets `claude-repld' serves in place.
Names a widget dist directory (e.g. an explanation-engine checkout's
`apps/cee-web-widget/dist') that the daemon mounts read-only at
/widget-assets/ — nothing is ever copied into this repo.  The webapp
probes that mount to enable chess-game bubbles.

When empty this is NOT the end of the story: the daemon-launch argv is
then filled by AUTO-DISCOVERY (see
`agent-repl--frontend-discover-widget-assets-dir'), which looks for a
real `cee-web-widget/dist' under
`agent-repl-frontend-widget-assets-search-root'.  A non-empty value here
is an explicit override that always wins over discovery.  The daemon also
honors $AGENT_REPL_WIDGET_ASSETS from its own inherited environment."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-frontend-widget-assets-search-root
  "~/workspace/ChessCom"
  "Root under which the widget-assets dir is auto-discovered.
When `agent-repl-frontend-widget-assets-dir' is empty, discovery looks
for an `apps/cee-web-widget/dist' (holding the `chess-widget.js' the
webapp imports) under an explanation-engine checkout below this root.
Empty or nil disables auto-discovery, leaving only the explicit dir."
  :type 'string
  :group 'agent-repl)

(defun agent-repl--frontend-widget-assets-candidates ()
  "Return candidate `cee-web-widget/dist' dirs under the search root, best first.
The canonical explanation-engine checkout is tried first, then its
worktrees under `explanation-engine-worktrees', then any other direct
sibling checkout below `agent-repl-frontend-widget-assets-search-root'.
Only the path shapes are assembled here; the caller checks existence."
  (let ((root (and agent-repl-frontend-widget-assets-search-root
                   (not (string-empty-p agent-repl-frontend-widget-assets-search-root))
                   (expand-file-name agent-repl-frontend-widget-assets-search-root)))
        (rel "apps/cee-web-widget/dist"))
    (if (and root (file-directory-p root))
        (let* ((worktrees (expand-file-name "explanation-engine-worktrees" root))
               (candidates
                (append
                 (list (expand-file-name (concat "explanation-engine/" rel) root))
                 (when (file-directory-p worktrees)
                   (mapcar (lambda (d) (expand-file-name rel d))
                           (directory-files worktrees t "\\`[^.]" t)))
                 (mapcar (lambda (d) (expand-file-name rel d))
                         (directory-files root t "\\`[^.]" t)))))
          (agent-repl--widget-doctor-log
           "widget-assets candidates: root=%s worktrees-dir=%s worktrees-exists=%s count=%s candidates=%S"
           root worktrees (file-directory-p worktrees)
           (length candidates) candidates)
          candidates)
      (agent-repl--widget-doctor-log
       "widget-assets candidates unavailable: configured-root=%S expanded-root=%S root-exists=%s"
       agent-repl-frontend-widget-assets-search-root root
       (and root (file-directory-p root)))
      nil)))

(defun agent-repl--frontend-discover-widget-assets-dir ()
  "Resolve the widget-assets dir the daemon should serve, or nil.
An explicit `agent-repl-frontend-widget-assets-dir' always wins and is
returned verbatim (expanded).  When it is empty, auto-discover the first
candidate (see `agent-repl--frontend-widget-assets-candidates') that
actually holds `chess-widget.js', so a stale or empty dist is skipped."
  (if (not (string-empty-p agent-repl-frontend-widget-assets-dir))
      (let ((dir (expand-file-name agent-repl-frontend-widget-assets-dir)))
        (agent-repl--widget-doctor-log
         "widget-assets resolution: source=explicit configured=%S resolved=%s widget-exists=%s"
         agent-repl-frontend-widget-assets-dir dir
         (file-exists-p (expand-file-name "chess-widget.js" dir)))
        dir)
    (let* ((candidates (agent-repl--frontend-widget-assets-candidates))
           (dir (seq-find (lambda (candidate)
                            (file-exists-p
                             (expand-file-name "chess-widget.js" candidate)))
                          candidates)))
      (agent-repl--widget-doctor-log
       "widget-assets resolution: source=discovery candidate-count=%s resolved=%S"
       (length candidates) dir)
      dir)))

(defun agent-repl--widget-doctor-log (fmt &rest args)
  "Record a widget-doctor event described by FMT and ARGS.
Use the core logger during normal module operation.  Standalone `doom
doctor' loads `doctor.el' before this file and supplies its bootstrap-safe
`agent-repl--doctor-log' boundary before core exists.  Signal loudly if
neither boundary is available: silently dropping diagnostics would hide a
broken load contract."
  (cond
   ((fboundp 'agent-repl--log)
    (apply #'agent-repl--log nil fmt args))
   ((fboundp 'agent-repl--doctor-log)
    (apply #'agent-repl--doctor-log fmt args))
   (t
    (error "agent-repl: widget doctor logging boundary unavailable"))))

(defun agent-repl--widget-doctor-issues ()
  "Return (LEVEL . MESSAGE) issues for the chess-widget capability.
Warns when no widget-assets dir resolves (the capability is off, so a
chess-game bubble would render nothing) or when the resolved dir lacks
the `chess-widget.js' the webapp imports.  Aggregated by `doctor.el'
alongside the install and codex checks."
  (let ((dir (agent-repl--frontend-discover-widget-assets-dir)))
    (cond
     ((null dir)
      (agent-repl--widget-doctor-log
       "widget doctor: result=missing configured-dir=%S search-root=%S"
       agent-repl-frontend-widget-assets-dir
       agent-repl-frontend-widget-assets-search-root)
      (list (cons 'warn
                  (format (concat "chess-widget capability OFF: no widget-assets dir resolves"
                                  " — set agent-repl-frontend-widget-assets-dir or put a"
                                  " cee-web-widget/dist under %s, then"
                                  " M-x agent-repl-frontend-daemon-restart")
                          (or agent-repl-frontend-widget-assets-search-root
                              "your explanation-engine checkout")))))
     ((not (file-exists-p (expand-file-name "chess-widget.js" dir)))
      (agent-repl--widget-doctor-log
       "widget doctor: result=invalid dir=%s widget-exists=nil" dir)
      (list (cons 'warn
                  (format (concat "chess-widget dir %s lacks chess-widget.js"
                                  " — point agent-repl-frontend-widget-assets-dir at a real"
                                  " cee-web-widget/dist, then"
                                  " M-x agent-repl-frontend-daemon-restart")
                          dir))))
     (t
      (agent-repl--widget-doctor-log
       "widget doctor: result=ready dir=%s widget-exists=t" dir)
      nil))))

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
No-op under batch (tests), matching the `install.el' auto-install
guard."
  noninteractive)

(defun agent-repl--frontend-probe-daemon-async (on-open on-failure)
  "Resolve frontend UDS liveness without blocking the main thread.
ON-OPEN and ON-FAILURE run once after the callback-owned probe settles."
  (agent-repl--log nil "frontend daemon probe: start socket=%s" agent-repl-uds-socket-path)
  (agent-repl-uds-probe-async
   agent-repl-uds-socket-path
   (lambda ()
     (agent-repl--log nil "frontend daemon probe: complete socket=%s outcome=open"
                      agent-repl-uds-socket-path)
     (funcall on-open))
   (lambda (detail)
     (agent-repl--log nil "frontend daemon probe: complete socket=%s outcome=absent detail=%S"
                      agent-repl-uds-socket-path detail)
     (funcall on-failure detail))))

(defun agent-repl--frontend-await-async (predicate timeout interval on-ready on-timeout context)
  "Run PREDICATE through timers and call a continuation without blocking.
CONTEXT and every terminal value are persisted for lifecycle diagnosis."
  (let ((deadline (+ (float-time) timeout)))
    (cl-labels ((step ()
                  (let ((value (funcall predicate)))
                    (cond
                     ((not value)
                      (agent-repl--log nil "frontend await: complete context=%s outcome=ready value=%S" context value)
                      (funcall on-ready))
                     ((>= (float-time) deadline)
                      (agent-repl--log nil "frontend await: timeout context=%s timeout=%s value=%S" context timeout value)
                      (funcall on-timeout value))
                     (t
                      (agent-repl--log-verbose nil "frontend await: pending context=%s deadline=%S value=%S" context deadline value)
                      (agent-repl--uds-run-timer interval #'step))))))
      (agent-repl--log nil "frontend await: start context=%s timeout=%s interval=%s" context timeout interval)
      (step))))

(defun agent-repl--frontend-await-socket-absence-async (timeout on-absent on-timeout context)
  "Await frontend socket removal through callback probes and timers only."
  (let ((deadline (+ (float-time) timeout)))
    (cl-labels ((attempt ()
                  (agent-repl--frontend-probe-daemon-async
                   (lambda ()
                     (if (>= (float-time) deadline)
                         (progn
                           (agent-repl--log nil "frontend socket await: timeout context=%s timeout=%s" context timeout)
                           (funcall on-timeout))
                       (agent-repl--uds-run-timer 0.1 #'attempt)))
                   (lambda (detail)
                     (agent-repl--log nil "frontend socket await: complete context=%s outcome=absent detail=%S" context detail)
                     (funcall on-absent)))))
      (agent-repl--log nil "frontend socket await: start context=%s timeout=%s" context timeout)
      (attempt))))

;;;; ---- Build-if-stale ---------------------------------------------------

(defun agent-repl--frontend-run-build-script (args callback)
  "External-boundary wrapper: run the build shell with ARGS asynchronously.
ARGS is the full argument list following the shell interpreter (script
path plus optional flags).  Output is captured into
`agent-repl--frontend-build-buffer'.  CALLBACK receives the integer exit
code once the process terminates.

THERE IS NO BLOCKING SPELLING.  This wrapper used to be a `call-process'
and was one of the sites that froze the interactive editor for the whole
of a runtime restart (a `fresh, skipping' build run still cost over a
second of held input).  The continuation is a required argument rather
than an optional one precisely so no future build step can be written
that blocks the main thread.

Body does nothing but invoke the external process so tests mock it via
`cl-letf'; registered in `agent-repl--external-boundary-functions'."
  (make-process ;; ALLOW-EXTERNAL-BOUNDARY
   :name "agent-repl-frontend-build"
   :command (cons agent-repl-frontend-build-shell args)
   :connection-type 'pipe
   :noquery t
   :buffer (get-buffer-create agent-repl--frontend-build-buffer)
   :sentinel (agent-repl--exit-code-sentinel callback)))

(defun agent-repl--frontend-build-report (label exit-code on-success on-failure)
  "Settle an asynchronous build of LABEL from its EXIT-CODE.
Copies the captured subprocess output into the persistent agent-repl log,
then hands a zero EXIT-CODE to ON-SUCCESS and any other to ON-FAILURE
with the same diagnostic the blocking spelling used to signal.  Shared by
every build-shell caller so the log record and the failure text cannot
drift between them."
  (let ((output
         (with-current-buffer (get-buffer-create agent-repl--frontend-build-buffer)
           (string-trim-right (buffer-string)))))
    (agent-repl--log nil "frontend build: %s exit=%S output=%s"
                     label exit-code
                     (if (string-empty-p output) "<empty>" output))
    (if (eq exit-code 0)
        (funcall on-success exit-code)
      (display-buffer agent-repl--frontend-build-buffer)
      (funcall on-failure
               (format "agent-repl: %s failed (exit %s) — see %s"
                       label exit-code agent-repl--frontend-build-buffer)))))

(defun agent-repl--frontend-build-targets-if-stale
    (targets force on-success on-failure)
  "Build stale TARGETS through the shared artifact orchestrator.
TARGETS is a list of build-frontend target strings, or nil for its normal
shim/webapp/daemon set.  With FORCE non-nil, every selected artifact is
rebuilt.  ON-SUCCESS receives the zero exit code; ON-FAILURE receives the
diagnostic string.  A missing build script is an invariant violation, not
a build failure, so it signals through the canonical logging helper
rather than reaching ON-FAILURE."
  (unless (and (functionp on-success) (functionp on-failure))
    (error "agent-repl: frontend build requires callable continuations"))
  (agent-repl--log nil "frontend build-if-stale: requested targets=%S force=%s script=%s"
                   (or targets 'default) (if force "t" "nil")
                   agent-repl--frontend-build-script)
  (unless (file-exists-p agent-repl--frontend-build-script)
    (agent-repl--log nil "frontend build-if-stale: script missing path=%s"
                     agent-repl--frontend-build-script)
    (error "agent-repl: frontend build script not found: %s"
           agent-repl--frontend-build-script))
  (with-current-buffer (get-buffer-create agent-repl--frontend-build-buffer)
    (erase-buffer))
  (let ((args (append (list agent-repl--frontend-build-script)
                      (when force '("--force"))
                      targets)))
    (agent-repl--frontend-run-build-script
     args
     (lambda (exit-code)
       (agent-repl--frontend-build-report
        (format "build-if-stale targets=%S force=%s"
                (or targets 'default) (if force "t" "nil"))
        exit-code on-success on-failure))))
  :pending)

(defun agent-repl--frontend-build-if-stale (force on-success on-failure)
  "Build stale shim, webapp, and daemon artifacts.
With FORCE non-nil, rebuild all three.  ON-SUCCESS and ON-FAILURE carry
the same meaning as in `agent-repl--frontend-build-targets-if-stale'.

Covers the three build-frontend targets ONLY.  The boot path wants the
whole stack and calls `agent-repl--frontend-deploy-stack' instead; this
stays the narrow build for callers that own the rest of the deploy
themselves (notably `agent-repl-runtime-restart', which kickstarts the
services in elisp and would otherwise do it twice)."
  (agent-repl--frontend-build-targets-if-stale nil force on-success on-failure))

(defun agent-repl--frontend-deploy-stack (force on-success on-failure)
  "Build and deploy the WHOLE agent-repl stack; fail loudly on failure.
ON-SUCCESS receives the zero exit code once the script terminates;
ON-FAILURE receives the diagnostic string.  Asynchronous for the same
reason the narrow build is: this runs on the boot path, and a blocking
spelling holds the editor's input for the whole of a stack deploy.

Runs `bin/deploy-all.sh --no-daemon-bounce': protobuf regeneration, the
shim/webapp/daemon build, a forced daemon rebuild (proto codegen lands
outside `daemon/', where its staleness check cannot see it), and the
shim-store and sidecar binaries with a launchd kickstart for whichever of
them is not already running its installed build.

`--no-daemon-bounce' is not an optimization.  The script\='s last step
restarts the daemon by evaluating a form in Emacs over emacsclient, so a
call made FROM Emacs would re-enter the very session that is mid-boot.
The caller starts the daemon directly once this returns, which is what
that step would have been asking for anyway.

WHY THIS IS ON THE BOOT PATH.  It used to be `build-frontend.sh', which
left two gaps: a proto change reached neither the regenerated Go nor a
daemon rebuilt against it, and the two launchd services were only ever
deployed by the interactive `agent-repl-runtime-restart'.  A wire-format
change could therefore leave a new Emacs talking to a daemon built before
it, which fails every command rather than degrading."
  (unless (and (functionp on-success) (functionp on-failure))
    (error "agent-repl: stack deploy requires callable continuations"))
  (agent-repl--log nil "frontend deploy-stack: requested force=%s script=%s"
                   (if force "t" "nil") agent-repl--frontend-deploy-script)
  (unless (file-exists-p agent-repl--frontend-deploy-script)
    (agent-repl--log nil "frontend deploy-stack: script missing path=%s"
                     agent-repl--frontend-deploy-script)
    (error "agent-repl: deploy script not found: %s"
           agent-repl--frontend-deploy-script))
  (with-current-buffer (get-buffer-create agent-repl--frontend-build-buffer)
    (erase-buffer))
  (let ((args (append (list agent-repl--frontend-deploy-script "--no-daemon-bounce")
                      (when force (list "--force")))))
    (agent-repl--frontend-run-build-script
     args
     (lambda (exit-code)
       (agent-repl--frontend-build-report
        (format "deploy-stack force=%s" (if force "t" "nil"))
        exit-code on-success on-failure))))
  :pending)

;;;; ---- Launch -----------------------------------------------------------

(defun agent-repl--frontend-daemon-command ()
  "Return the argv list used to launch `claude-repld'.
When the system `claude' binary resolves, it is handed to the daemon
via -claude-bin so SDK sessions drive the SAME CLI version as vterm
sessions.  The user upgrades `claude' independently of the shim's
lockfile, so the system binary can lead the SDK's bundled one; since
SDK 0.2.113 that bundle is a per-platform NATIVE Claude Code binary
(0.3.220 ships 2.1.220), not the old JS `cli.js'.

The canonical account roster rides in via -accounts (see
`agent-repl--frontend-accounts-flag'): the daemon serves it at
GET /accounts for the webapp's account menu, and gates the switch
endpoint to exactly these roots."
  (append
   (list agent-repl--frontend-daemon-bin
         "-addr"   agent-repl-frontend-daemon-addr
         "-node"   agent-repl-frontend-node-bin
         "-shim"   agent-repl--frontend-shim-entry
         "-webapp" agent-repl--frontend-webapp-dir
         "-accounts" (agent-repl--frontend-accounts-flag))
   (when-let ((widget (agent-repl--frontend-discover-widget-assets-dir)))
     (list "-widget-assets" widget))
   (when-let ((claude (executable-find "claude")))
     (list "-claude-bin" claude))))

(defun agent-repl--frontend-accounts-flag ()
  "Return the -accounts flag value naming the canonical account roster.
Two roots, mirroring `agent-repl--compute-config-dir''s two answers:
`personal' is the CLI's own default ~/.claude (the empty dir, exactly
how sessions created without a config_dir record it), and `work' is
`agent-repl-multi-repo-config-dir' expanded (the dodge@chess.com
account).  Keeping the flag derived from the SAME variable the resolver
uses is what makes the roster's dirs equal (string-equal) to the dirs
sessions actually run under, which the daemon's switch endpoint
compares against."
  (format "personal=,work=%s"
          (expand-file-name agent-repl-multi-repo-config-dir)))

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

(defun agent-repl--frontend-artifact-exists-p (path)
  "External-boundary wrapper: return non-nil when artifact PATH exists.
Body does nothing but query PATH.  Tests mock it via `cl-letf';
registered in `agent-repl--external-boundary-functions'."
  (file-exists-p path)) ; ALLOW-EXTERNAL-BOUNDARY

(defun agent-repl--frontend-start-daemon ()
  "Start the `claude-repld' process and track it, returning the process.
Assumes the artifacts are already built; call
`agent-repl--frontend-build-if-stale' first."
  (let ((daemon-exists
         (agent-repl--frontend-artifact-exists-p
          agent-repl--frontend-daemon-bin))
        (shim-exists
         (agent-repl--frontend-artifact-exists-p
          agent-repl--frontend-shim-entry)))
    (agent-repl--log
     nil
     (concat "claude-repld launch preflight: daemon-bin=%s daemon-exists=%s "
             "shim-entry=%s shim-exists=%s webapp-dir=%s")
     agent-repl--frontend-daemon-bin daemon-exists
     agent-repl--frontend-shim-entry shim-exists
     agent-repl--frontend-webapp-dir)
    (unless daemon-exists
      (agent-repl--error
       nil "daemon binary missing after build: %s"
       agent-repl--frontend-daemon-bin))
    (unless shim-exists
      (agent-repl--error
       nil "Claude shim entrypoint missing after build: %s"
       agent-repl--frontend-shim-entry))
    (let ((proc (agent-repl--frontend-spawn-daemon)))
      (setq agent-repl--frontend-daemon-process proc)
      (agent-repl--log nil "claude-repld started (pid %s) on %s"
                        (process-id proc) agent-repl-frontend-daemon-addr)
      proc)))

(defun agent-repl--frontend-daemon-sentinel (proc event)
  "Clear the tracked process when PROC dies; EVENT is the status change.

Surfaces the death rather than only logging it (F4).  The daemon\='s own
exit is the one failure with no card anywhere — the process that composes
every other failure card is the one that just died, so it cannot report
this about itself.  Emacs supervises the process, so Emacs classifies it,
under the reserved `client.\=' prefix."
  (let ((live (process-live-p proc))
        (tracked (eq proc agent-repl--frontend-daemon-process))
        (trimmed-event (string-trim event)))
    (if live
        (agent-repl--log-verbose nil
                                  "claude-repld sentinel: process-live=t tracked=%s event=%s"
                                  tracked trimmed-event)
      (when tracked
        (setq agent-repl--frontend-daemon-process nil))
      (agent-repl--log nil
                       "claude-repld exited: tracked=%s event=%s"
                       tracked trimmed-event)
      (agent-repl-failure-surface
       nil
       (agent-repl-failure-local
        "client.daemon_exited"
        "the agent-repl daemon exited"
        trimmed-event)))))

;;;; ---- Entry point ------------------------------------------------------

(defun agent-repl--frontend-daemon-responsive-async (on-open on-absent)
  "Asynchronously report whether a daemon owns the frontend UDS socket.
The callback API is the only daemon lifecycle liveness surface."
  (agent-repl--frontend-probe-daemon-async on-open on-absent))

;;;; ---- Staleness detection ----------------------------------------------

(defun agent-repl--frontend-daemon-binary-disk-mtime ()
  "Return the on-disk `claude-repld' binary's mtime as integer Unix seconds.
Nil when the binary is absent (nothing built yet).  Integer seconds match
the resolution the daemon reports (`daemon_binary_mtime') and the build
script's own mtime-based staleness rule."
  (when (file-exists-p agent-repl--frontend-daemon-bin)
    (time-convert (file-attribute-modification-time
                   (file-attributes agent-repl--frontend-daemon-bin))
                  'integer)))

(defun agent-repl--frontend-running-daemon-binary-mtime ()
  "Return the launched-binary mtime the running daemon reports, or nil.
Reads `daemon_binary_mtime_ms' off the pushed `DaemonView'
\(`agent-repl--frontend-daemon-view-binary-mtime-seconds'), so it resolves
for a daemon this Emacs tracks AND one adopted from another Emacs — both
push on the same UDS, and neither exposes a local process-start-time this
Emacs could otherwise inspect.

Gated on a LIVE link: a stored view outlives the connection that
delivered it, and after a bounce it describes the PREVIOUS instance until
the reconnect snapshot lands, so a disconnected link reads nil (unknown)
exactly as the unreachable-daemon branch of the old HTTP probe did.  Nil
too when no view has been pushed or its mtime is absent/non-positive, so a
daemon that cannot name its own binary is never judged stale on a guess."
  (when (agent-repl--uds-connected-p)
    (agent-repl--frontend-daemon-view-binary-mtime-seconds)))

(defun agent-repl--frontend-daemon-stale-p ()
  "Return non-nil when the on-disk binary is newer than the running daemon's.
Compares the on-disk binary's mtime against the launched-binary mtime the
running/adopted daemon reports.  A STRICT `>' is deliberate: the common
no-rebuild case has the two equal (the daemon launched from the very file
still on disk), and a `>=' comparison would then flag every daemon stale
and bounce it in a loop.  Nil (fresh) whenever either mtime is unknown, so
nothing is ever bounced on a guess."
  (let ((disk (agent-repl--frontend-daemon-binary-disk-mtime))
        (running (agent-repl--frontend-running-daemon-binary-mtime)))
    (let ((stale (and disk running (> disk running))))
      (agent-repl--log nil
                       "frontend daemon staleness: disk-mtime=%S running-mtime=%S stale=%s"
                       disk running (if stale "t" "nil"))
      stale)))

(defun agent-repl--frontend-request-foreign-shutdown (&optional stop-shims)
  "Ask the daemon on the frontend UDS to shut down (`ShutdownCmd').
STOP-SHIMS sets `ShutdownCmd.stop_shims', asking the daemon to SIGTERM its
session shims on the way out.  Nil -- the default -- PRESERVES them: a shim
outlives its daemon, redials this same socket and is parked by the
replacement's listener, so preserving is what makes a bounce cheap.
The S9 replacement for the deleted `POST /shutdown' call: the graceful
teardown the daemon ran for that route is now reachable as a
`FrontendCommand' arm, so this needs no HTTP.  Dials first when the link
is down — a foreign daemon owns the SAME socket, which is exactly why this
reaches a daemon this Emacs never spawned.

Tracks the command so a rejected ack is surfaced loudly rather than read
as success.  Signals (`agent-repl--uds-send-command's loud `user-error')
when there is no connection to send on; the caller logs that and lets the
asynchronous socket probe determine the exit outcome."
  (let ((connected (agent-repl--uds-connected-p)))
    (agent-repl--log nil "foreign daemon shutdown: uds-connected=%s addr=%s"
                     connected agent-repl-frontend-daemon-addr)
    (unless connected
    (agent-repl-uds-connect))
    (let ((req (agent-repl--uds-send-command
                "shutdown" (and stop-shims (list :stopShims t)))))
      (agent-repl--log nil
                       "foreign daemon shutdown: command accepted request-id=%S stop-shims=%s"
                       req (if stop-shims "t" "nil"))
      req)))

;;;; ---- Scheduled shutdown (the drain lease) ----------------------------
;;
;; `ShutdownCmd' demands the bounce NOW: whatever turn was in flight when the
;; user typed it dies with the daemon.  `ScheduleShutdownCmd' hands the daemon
;; the decision of WHEN instead — it takes a daemon-global drain lease, blocks
;; NEW turns at the prompt queue, and runs the same shutdown the moment every
;; workspace goes quiet.  The immediate path stays exactly as it was; this is
;; a second door, not a replacement.
;;
;; Emacs builds NO reconnection machinery for it.  The executed shutdown is
;; indistinguishable from any other daemon exit, so the existing reattach
;; sweep (`agent-repl--frontend-reattach-check', frontend-client.el) brings the
;; link back on the replacement exactly as it does after a manual bounce.

(defun agent-repl--frontend-request-scheduled-shutdown (cause &optional stop-shims)
  "Ask the daemon to SCHEDULE a shutdown (`ScheduleShutdownCmd').
CAUSE is free display text carried on the broadcast lease and the daemon's
durable log; STOP-SHIMS sets `stop_shims' with the same meaning it has on
`ShutdownCmd' (nil, the default, PRESERVES shims for reattach).  Dials
first when the link is down, for the same reason the immediate shutdown
does: the daemon that owns the socket may be one this Emacs never spawned.

Refuses loudly, before any send, when a schedule is ALREADY live in the
recorded lease.  The proto makes a second schedule a nack rather than a
silent replace so two deploy flows cannot merge their intents, and
refusing here names the live schedule instead of making the user read a
daemon ack to find out.  An empty CAUSE is refused the same way: the
lease's only human-readable field must not be blank.

Tracks the command so a rejected ack surfaces loudly.  Returns the
`request_id'."
  (unless (and (stringp cause) (not (string-empty-p (string-trim cause))))
    (agent-repl--log nil "scheduled shutdown: REFUSING blank cause=%S" cause)
    (user-error "agent-repl: a scheduled shutdown needs a cause"))
  (let ((live (agent-repl-frontend-scheduled-shutdown-id)))
    (when live
      (agent-repl--log nil
                       "scheduled shutdown: REFUSING — schedule-id=%s already holds the lease; cancel it first"
                       live)
      (user-error "agent-repl: shutdown %s is already scheduled; cancel it first"
                  live)))
  (let ((connected (agent-repl--uds-connected-p)))
    (agent-repl--log nil
                     "scheduled shutdown: uds-connected=%s addr=%s stop-shims=%s cause=%S"
                     connected agent-repl-frontend-daemon-addr
                     (if stop-shims "t" "nil") cause)
    (unless connected
      (agent-repl-uds-connect))
    (let ((req (agent-repl--uds-send-command
                "scheduleShutdown"
                (append (list :cause cause)
                        (when stop-shims (list :stopShims t))))))
      (agent-repl--log nil
                       "scheduled shutdown: command accepted request-id=%S"
                       req)
      req)))

(defun agent-repl--frontend-request-cancel-scheduled-shutdown ()
  "Cancel the live scheduled shutdown (`CancelScheduledShutdownCmd').
The schedule id comes from the last decoded `ShutdownScheduleView'; the
proto requires it to match the live schedule so a cancel aimed at an old
schedule can never kill a newer one.

With NO recorded schedule this is a loud `user-error', never a no-op: the
nil case covers both `idle' and a lease never received, and sending a
guessed or empty id would earn a daemon nack the user would have to go
read the log to find.  Returns the `request_id'."
  (let ((schedule-id (agent-repl-frontend-scheduled-shutdown-id)))
    (unless schedule-id
      (agent-repl--log nil
                       "cancel scheduled shutdown: REFUSING — no live schedule recorded lease=%S"
                       (agent-repl-frontend-shutdown-schedule))
      (user-error "agent-repl: no scheduled shutdown to cancel"))
    (let ((connected (agent-repl--uds-connected-p)))
      (agent-repl--log nil
                       "cancel scheduled shutdown: uds-connected=%s addr=%s schedule-id=%s"
                       connected agent-repl-frontend-daemon-addr schedule-id)
      (unless connected
        (agent-repl-uds-connect))
      (let ((req (agent-repl--uds-send-command
                  "cancelScheduledShutdown" (list :scheduleId schedule-id))))
        (agent-repl--log nil
                         "cancel scheduled shutdown: command accepted request-id=%S schedule-id=%s"
                         req schedule-id)
        req))))

(defun agent-repl--frontend-bounce-foreign-daemon (&optional stop-shims on-complete)
  "Bounce an ADOPTED daemon this Emacs does not track, via `ShutdownCmd'.
STOP-SHIMS rides the command; see
`agent-repl--frontend-request-foreign-shutdown'.
Asks the foreign daemon to exit gracefully (it runs the same shutdown path
SIGTERM triggers), waits for its listener to free the socket, then starts a
fresh daemon from the already-rebuilt on-disk binary.  Returns `:pending'
immediately; ON-COMPLETE receives the replacement only after socket release.

The shutdown command errors benignly if the daemon drops the connection as
it tears down, so a transport error on the send is logged and ignored —
the asynchronous socket probe is the real exit signal.  A daemon that never frees the
socket within `agent-repl-frontend-foreign-stop-grace-seconds' fails loudly
and is left in place; spawning next to it would only bind-fail."
  (agent-repl--log nil
                   "foreign daemon bounce: begin addr=%s grace-seconds=%s"
                   agent-repl-frontend-daemon-addr
                   agent-repl-frontend-foreign-stop-grace-seconds)
  (condition-case err
      (agent-repl--frontend-request-foreign-shutdown stop-shims)
    (error
     (agent-repl--log nil "startup: foreign daemon shutdown request errored (%s) — probing the socket asynchronously"
                       (error-message-string err))))
  (agent-repl--frontend-await-socket-absence-async
   agent-repl-frontend-foreign-stop-grace-seconds
   (lambda ()
     (agent-repl--log nil "foreign daemon bounce: socket released addr=%s; starting replacement"
                      agent-repl-frontend-daemon-addr)
     (let ((started (agent-repl--frontend-start-daemon)))
       (when on-complete (funcall on-complete started))))
   (lambda ()
     (agent-repl--error nil "adopted daemon on %s ignored shutdown within %ss; replacement aborted"
                        agent-repl-frontend-daemon-addr
                        agent-repl-frontend-foreign-stop-grace-seconds))
   "foreign-daemon-shutdown")
  :pending)

(defun agent-repl--frontend-runtime-bounce-preflight-async (callback)
  "Resolve and validate the daemon state before any runtime mutation.
CALLBACK receives a validated state before any lifecycle mutation."
  (if (agent-repl--frontend-daemon-live-p)
      (progn
        (agent-repl--log nil "runtime-bounce preflight: state=tracked addr=%s"
                         agent-repl-frontend-daemon-addr)
        (funcall callback :tracked))
    (agent-repl--frontend-daemon-responsive-async
     (lambda ()
       (agent-repl--log nil "runtime-bounce preflight: state=responsive addr=%s"
                        agent-repl-frontend-daemon-addr)
       (funcall callback :responsive))
     (lambda (_detail)
       (let ((owner (agent-repl--frontend-listener-owner)))
         (cond
          ((and owner (agent-repl--frontend-our-daemon-command-p (cdr owner)))
           (let ((state (cons :incompatible owner)))
             (agent-repl--log nil "runtime-bounce preflight: state=%S addr=%s" state agent-repl-frontend-daemon-addr)
             (funcall callback state)))
          (owner
           (agent-repl--error nil "daemon address %s is held by unrelated process pid=%s command=%s"
                              agent-repl-frontend-daemon-addr (car owner) (cdr owner)))
          (t
           (agent-repl--log nil "runtime-bounce preflight: state=absent addr=%s" agent-repl-frontend-daemon-addr)
           (funcall callback :absent))))))))

(defun agent-repl--frontend-bounce-after-build (&optional preflight stop-shims on-complete)
  "Bounce the current daemon after all required artifacts are built.
STOP-SHIMS asks the outgoing daemon to stop its session shims rather than
leave them running for the replacement to reattach to; see
`agent-repl--frontend-stop-daemon'.
Starts a daemon when none exists, gracefully replaces a tracked or adopted
daemon, and handles an incompatible pre-UDS generation by terminating only
the verified claude-repld listener.  The caller must reject active turns
before invoking this state-changing operation.  PREFLIGHT is the validated
result of `agent-repl--frontend-runtime-bounce-preflight-async'.  The
caller supplies PREFLIGHT and ON-COMPLETE receives the started daemon after
any asynchronous stop has settled."
  (let ((state preflight))
    (unless state
      (error "agent-repl: runtime bounce requires asynchronous preflight state"))
    (agent-repl--log nil
                     "runtime-bounce: applying preflight=%S addr=%s"
                     state agent-repl-frontend-daemon-addr)
    (cond
     ((eq state :tracked)
      (agent-repl--frontend-stop-daemon
       nil stop-shims
       (lambda ()
         (let ((started (agent-repl--frontend-start-daemon)))
           (when on-complete (funcall on-complete started)))))
      :pending)
     ((eq state :responsive)
      (agent-repl--frontend-bounce-foreign-daemon stop-shims on-complete))
     ((and (consp state) (eq (car state) :incompatible))
      (agent-repl--frontend-terminate-incompatible-daemon
       (cadr state)
       (lambda ()
         (let ((started (agent-repl--frontend-start-daemon)))
           (when on-complete (funcall on-complete started))))))
     ((eq state :absent)
      (let ((started (agent-repl--frontend-start-daemon)))
        (when on-complete (funcall on-complete started))
        started))
     (t
      (agent-repl--error nil
                         "invalid daemon runtime-bounce preflight state: %S"
                         state)))))

;;;; ---- Incompatible-daemon replacement (first boot after a cutover) ------

(defcustom agent-repl-frontend-incompatible-stop-grace-seconds 8.0
  "Seconds to wait for an INCOMPATIBLE daemon to exit after SIGTERM.
An incompatible daemon predates this generation's protocol, so it cannot
be asked to leave over the UDS (it serves none) and cannot be reached by
`ShutdownCmd'.  It is signalled instead, and only SIGKILLed if it
outlives this window."
  :type 'number
  :group 'agent-repl)

(defconst agent-repl--frontend-listener-probe-program "lsof"
  "Program used to find the process holding the daemon's TCP port.")

(defun agent-repl--frontend-daemon-port ()
  "Return the port component of `agent-repl-frontend-daemon-addr' as a string.
Nil when the address carries no port, which makes every port-based
detection below a no-op rather than a guess."
  (let ((port (when (string-match "\\`.*:\\([0-9]+\\)\\'" agent-repl-frontend-daemon-addr)
                (match-string 1 agent-repl-frontend-daemon-addr))))
    (agent-repl--log-verbose nil "daemon port resolution: addr=%s port=%S"
                             agent-repl-frontend-daemon-addr port)
    port))

(defun agent-repl--frontend-run-listener-probe (port)
  "External-boundary wrapper: return `lsof' output naming PORT's listener.
Returns the raw stdout string, or nil when the probe cannot run.  Body
does nothing but invoke the external process so tests mock it via
`cl-letf'; registered in `agent-repl--external-boundary-functions'."
  (with-temp-buffer
    (let ((code (call-process ;; ALLOW-EXTERNAL-BOUNDARY
                 agent-repl--frontend-listener-probe-program nil t nil
                 "-nP" "-a" "-iTCP:" "-sTCP:LISTEN" "-Fpc"
                 (concat "-i" "TCP:" port))))
      ;; lsof exits 1 when nothing matches, which is a legitimate "no
      ;; listener" answer rather than a failure.
      (when (memq code '(0 1))
        (buffer-string)))))

(defun agent-repl--frontend-parse-listener (output)
  "Parse `lsof -F' OUTPUT into (PID . COMMAND), or nil when nothing listens.
`-F' emits one field per line, each prefixed by its field character: `p'
for the pid, `c' for the command name.  Only the FIRST listener is
returned — the port has a single owner, and a second would mean something
stranger than a stale daemon."
  (when (stringp output)
    (let (pid command)
      (dolist (line (split-string output "\n" t))
        (cond
         ((and (null pid) (string-prefix-p "p" line))
          (setq pid (string-to-number (substring line 1))))
         ((and pid (null command) (string-prefix-p "c" line))
          (setq command (substring line 1)))))
      (when (and pid (> pid 0))
        (cons pid (or command ""))))))

(defun agent-repl--frontend-listener-owner ()
  "Return (PID . COMMAND) for the process listening on the daemon's port.
Nil when the port is free, unparseable, or has no resolvable port."
  (let* ((port (agent-repl--frontend-daemon-port))
         (output (and port (agent-repl--frontend-run-listener-probe port)))
         (owner (and port (agent-repl--frontend-parse-listener output))))
    (agent-repl--log-verbose nil
                             "daemon listener probe: addr=%s port=%S output-present=%s owner=%S"
                             agent-repl-frontend-daemon-addr port (not (null output)) owner)
    owner))

(defun agent-repl--frontend-our-daemon-command-p (command)
  "Return non-nil when COMMAND names THIS module's daemon executable.
The whole safety of the replacement below rests on this predicate: it is
what keeps the terminate step from touching a process that merely happens
to hold the port.  The match is on the daemon binary's own basename, so
an unrelated program on the port is left strictly alone and reported."
  (and (stringp command)
       (equal command (file-name-nondirectory agent-repl--frontend-daemon-bin))))

(defun agent-repl--frontend-incompatible-daemon-async (callback)
  "Return (PID . COMMAND) of a running daemon this generation cannot talk to.
This is the FIRST-BOOT-AFTER-CUTOVER condition: a `claude-repld' from a
previous generation is still holding the daemon's port, but serves no
frontend UDS socket, so nothing here can reach it — and a fresh daemon
spawned next to it would only bind-fail and die.

All three conditions must hold, and each rules out a different innocent
case:
  - something is listening on the daemon's port (else there is nothing
    to replace);
  - NO frontend UDS socket is being served (else the daemon is a current
    one and is adopted normally, never killed);
  - the listener is OUR daemon binary (else it is someone else's program
    and is none of our business).

CALLBACK receives the owner or nil.  Foreign listeners remain untouched."
  (agent-repl--frontend-daemon-responsive-async
   (lambda ()
     (agent-repl--log nil "incompatible daemon check: addr=%s responsive=t; no replacement needed"
                      agent-repl-frontend-daemon-addr)
     (funcall callback nil))
   (lambda (_detail)
     (let ((owner (agent-repl--frontend-listener-owner)))
        (cond
         ((null owner)
          (agent-repl--log nil
                           "incompatible daemon check: addr=%s responsive=nil owner=nil"
                           agent-repl-frontend-daemon-addr)
          (funcall callback nil))
         ((agent-repl--frontend-our-daemon-command-p (cdr owner))
          (agent-repl--log nil
                           "incompatible daemon check: addr=%s responsive=nil owner=%S ours=t"
                           agent-repl-frontend-daemon-addr owner)
          (funcall callback owner))
         (t
          (agent-repl--log nil
                           "startup: %s is held by pid %s (%s), which is NOT our daemon — leaving it alone"
                           agent-repl-frontend-daemon-addr (car owner) (cdr owner))
          (funcall callback nil)))))))

(defun agent-repl--frontend-terminate-incompatible-daemon (pid &optional on-stopped)
  "Terminate the incompatible daemon PID, gracefully first.
SIGTERM so it runs its own shutdown path, then SIGKILL only if it
outlives `agent-repl-frontend-incompatible-stop-grace-seconds'.  Returns
`:pending' immediately and invokes ON-STOPPED only after the port is free.

There is no in-flight-turn guard here, unlike the ordinary stop: this
daemon serves no UDS, so its turns are already unreachable from this
Emacs — there is no live conversation to protect, only a process holding
a port nothing can use."
  (agent-repl--log nil "startup: terminating incompatible daemon pid=%s on %s"
                   pid agent-repl-frontend-daemon-addr)
  (unless on-stopped
    (error "agent-repl: incompatible termination requires completion callback"))
  (agent-repl--signal-process pid 'TERM)
  (agent-repl--frontend-await-async
   #'agent-repl--frontend-listener-owner agent-repl-frontend-incompatible-stop-grace-seconds 0.1
   (lambda ()
     (agent-repl--log nil "incompatible daemon termination: pid=%s outcome=term-stopped" pid)
     (funcall on-stopped))
   (lambda (owner)
     (agent-repl--log nil "startup: incompatible daemon pid=%s ignored SIGTERM owner=%S; sending KILL" pid owner)
     (agent-repl--signal-process pid 'KILL)
     (agent-repl--frontend-await-async
      #'agent-repl--frontend-listener-owner agent-repl-frontend-incompatible-stop-grace-seconds 0.1
      (lambda ()
        (agent-repl--log nil "incompatible daemon termination: pid=%s outcome=kill-stopped" pid)
        (funcall on-stopped))
      (lambda (still-owner)
        (agent-repl--error nil "incompatible daemon pid=%s survived SIGKILL owner=%S; replacement aborted" pid still-owner))
      "incompatible-daemon-kill"))
   "incompatible-daemon-term")
  :pending)

(defun agent-repl--ensure-frontend-daemon (&optional force)
  "Ensure frontend daemon startup has been requested; return its state.
Idempotent: returns the live process immediately when one exists (unless
FORCE).  A daemon already answering on the port that this Emacs does
NOT track is ADOPTED asynchronously: spawning next to it would only
bind-fail and die — the orphan-daemon failure mode.  Otherwise builds
any stale artifact and launches `claude-repld'.  FORCE skips adoption:
an explicit restart wants a fresh process (a foreign daemon cannot be
stopped from here — only its owner can).  Returns nil without acting
when `agent-repl-frontend-auto-start' is nil or automatic init is
inhibited (batch).  The post-snapshot startup coordinator in
services.el owns the once-per-Emacs full-runtime bounce; this function
remains the cheap idempotent session-open ensure.  Every probe or lifecycle
transition returns `:pending' immediately and completes from its callback."
  (let ((inhibited (agent-repl--frontend-init-inhibited-p)))
    (agent-repl--log nil
                     "ensure-frontend-daemon: force=%s auto-start=%s init-inhibited=%s tracked-live=%s"
                     (if force "t" "nil") agent-repl-frontend-auto-start inhibited
                     (agent-repl--frontend-daemon-live-p))
    (cond
   ((not agent-repl-frontend-auto-start)
    (agent-repl--log nil "ensure-frontend-daemon: skipped reason=auto-start-disabled")
    nil)
   (inhibited
    (agent-repl--log nil "ensure-frontend-daemon: skipped reason=init-inhibited")
    nil)
   (t
    (cond
     ((and (not force) (agent-repl--frontend-daemon-live-p))
      (agent-repl--log nil "ensure-frontend-daemon: reusing tracked pid=%S"
                       (process-id agent-repl--frontend-daemon-process))
      agent-repl--frontend-daemon-process)
     (t
      (cl-labels ((build-and-launch ()
                    (agent-repl--log nil "ensure-frontend-daemon: build-and-launch force=%s"
                                     (if force "t" "nil"))
                    ;; The stack deploy no longer blocks, so the launch that
                    ;; used to follow it in straight-line order is now its
                    ;; success continuation.  A failed deploy must never reach
                    ;; the launch: starting the daemon on a half-built stack is
                    ;; exactly the wire-format mismatch the deploy exists to
                    ;; prevent, so the failure arm signals through the module's
                    ;; canonical helper instead.
                    (agent-repl--frontend-deploy-stack
                     force
                     (lambda (_exit-code) (agent-repl--frontend-start-daemon))
                     (lambda (detail)
                       (agent-repl--error
                        nil
                        "ensure-frontend-daemon: stack deploy FAILED force=%s detail=%s"
                        (if force "t" "nil") detail)))))
        (if (and force (agent-repl--frontend-daemon-live-p))
            (progn
              (agent-repl--log nil "ensure-frontend-daemon: force=t; asynchronously stopping tracked daemon before rebuild")
              (agent-repl--frontend-stop-daemon t nil #'build-and-launch))
          (agent-repl--frontend-daemon-responsive-async
           (lambda ()
             (if force
                 (build-and-launch)
               (agent-repl--log nil "ensure-frontend-daemon: adopting foreign daemon on %s"
                                agent-repl-frontend-daemon-addr)))
           (lambda (detail)
             (agent-repl--log nil "ensure-frontend-daemon: no daemon probe-detail=%S; building" detail)
             (build-and-launch))))
        :pending)))))))

(defun agent-repl--frontend-stop-daemon (&optional force stop-shims on-stopped)
  "Stop the tracked `claude-repld' process, gracefully first.
STOP-SHIMS asks the daemon to SIGTERM its session shims on the way out.
A SIGNAL CANNOT CARRY A MODE, so that request has to travel as a
`ShutdownCmd' over the UDS link before any signal is sent; the daemon
serves the first shutdown request it receives and ignores the rest, so
the TERM below stays the fallback rather than a competing request.  Nil --
the default -- PRESERVES the shims: they redial this same socket and are
parked by the replacement daemon\='s listener, which is what makes a bounce
a reattach instead of a rebuild.
Refuses while any daemon session reports an in-flight turn — stopping
then would kill a live conversation mid-generation (the repeated
daemon-bounce incidents) — unless FORCE is non-nil.  An unreachable
daemon has nothing to protect, so the turn probe treats it as idle.

The stop itself signals SIGTERM so the daemon runs its shutdown path
\(draining its sessions and flushing the session registry), then checks
the process through timers until `agent-repl-frontend-stop-grace-seconds'
elapses before issuing `delete-process' (SIGKILL).  The old
delete-process-first behavior SIGKILLed the daemon on every routine
restart, which is exactly the restart class the session registry exists
to survive — the registry makes that survivable either way, and the
graceful window additionally lets shims exit cleanly instead of by
inherited-pipe EOF."
  (let ((live (agent-repl--frontend-daemon-live-p)))
    (agent-repl--log nil "frontend stop: requested force=%s tracked-live=%s"
                     (if force "t" "nil") live)
    (if (not live)
        (progn
          (agent-repl--log nil "frontend stop: no tracked live daemon; clearing process slot")
          (setq agent-repl--frontend-daemon-process nil)
          (when on-stopped (funcall on-stopped)))
      (unless force
        (when-let ((busy (agent-repl--frontend-turn-active-sessions)))
          (agent-repl--log nil "frontend stop: refused force=nil active-sessions=%S" busy)
          (error "agent-repl: refusing daemon stop — turn in flight in %s; retry when idle or pass FORCE"
                 busy)))
    (when stop-shims
      (condition-case err
          (agent-repl--frontend-request-foreign-shutdown t)
        (error
         ;; The link can be down while the tracked process is alive. The TERM
         ;; below still stops the daemon; it simply cannot carry the mode, so
         ;; the shims are preserved. Loud, because the caller asked for the
         ;; opposite and is entitled to know it did not happen.
         (agent-repl--log nil
                          "frontend stop: stop-shims request FAILED (%s) — the daemon will preserve its shims"
                          (error-message-string err)))))
    (let ((proc agent-repl--frontend-daemon-process))
      (agent-repl--log nil "frontend stop: sending TERM pid=%S grace-seconds=%s stop-shims=%s"
                       (process-id proc) agent-repl-frontend-stop-grace-seconds
                       (if stop-shims "t" "nil"))
      (signal-process proc 'TERM)
      (agent-repl--frontend-await-async
       (lambda () (process-live-p proc)) agent-repl-frontend-stop-grace-seconds 0.05
       (lambda ()
         (setq agent-repl--frontend-daemon-process nil)
         (agent-repl--log nil "frontend stop: graceful exit observed pid=%S" (process-id proc))
         (when on-stopped (funcall on-stopped)))
       (lambda (_live)
         (agent-repl--log nil "claude-repld ignored SIGTERM for %ss; issuing delete-process pid=%S"
                          agent-repl-frontend-stop-grace-seconds (process-id proc))
         (delete-process proc)
         (setq agent-repl--frontend-daemon-process nil)
         (when on-stopped (funcall on-stopped)))
       "tracked-daemon-stop")))
  :pending))

;;;; ---- Interactive commands ---------------------------------------------

;;;###autoload
(defun agent-repl-frontend-daemon-ensure ()
  "Interactively build-if-stale and start the frontend daemon.
Bypasses `agent-repl-frontend-auto-start' and the batch guard
so a user can force initialization on demand."
  (interactive)
  (let ((agent-repl-frontend-auto-start t))
    (cl-letf (((symbol-function 'agent-repl--frontend-init-inhibited-p)
               (lambda () nil)))
      (let ((state (agent-repl--ensure-frontend-daemon)))
        (message "claude-repld startup %s on %s"
                 (if (eq state :pending) "requested" "already available")
                 agent-repl-frontend-daemon-addr)))))

;;;###autoload
(defun agent-repl-frontend-daemon-stop (&optional force)
  "Stop the running frontend daemon.
Refuses while a turn is in flight; with prefix arg FORCE, stops anyway.
Completion is reported only after the asynchronous stop continuation runs."
  (interactive "P")
  (let ((forced (and force t)))
    (agent-repl--log nil "frontend stop command: requested force=%s"
                     (if forced "t" "nil"))
    (agent-repl--frontend-stop-daemon
     forced nil
     (lambda ()
       (agent-repl--log nil "frontend stop command: completed force=%s"
                        (if forced "t" "nil"))
       (message "claude-repld stopped.")))))

;;;###autoload
(defun agent-repl-frontend-daemon-restart (&optional stop-shims)
  "Restart the complete runtime through the canonical coordinator.
This compatibility command now includes stale builds plus launchd-managed
store/sidecar bounces, and immediately rebinds open panels.

It no longer replaces every session shim.  A shim outlives its daemon by
design -- it redials the daemon socket and is parked by the replacement\='s
listener -- so the default bounce PRESERVES them and the new daemon
reattaches instead of rebuilding, leaving every conversation running.

STOP-SHIMS (the interactive prefix argument) restores the old behavior for
the one caller that needs it: a deploy that changed the shim BUNDLE, whose
survivors would otherwise keep running the previous build\=' code."
  (interactive "P")
  (agent-repl-runtime-restart (and stop-shims t)))

(defun agent-repl-frontend-daemon-restart-dispatch (&optional stop-shims timeout)
  "Dispatch the runtime restart and return its request identity at once.
STOP-SHIMS and TIMEOUT are forwarded to
`agent-repl-runtime-restart-dispatch'.  This is the deployment-only
companion to the interactive `agent-repl-frontend-daemon-restart'.

It RETURNS IMMEDIATELY.  A deployment caller reaching this over
`emacsclient --eval' drives the editor the user is sitting in, so the
terminal result travels through the completion artifact rather than
through the editor's main loop; see the commentary above
`agent-repl-runtime-restart-dispatch'."
  (agent-repl--log nil
                   "frontend restart-dispatch: requested stop-shims=%s timeout=%S root=%s"
                   (if stop-shims "t" "nil") timeout agent-repl--frontend-root)
  (agent-repl-runtime-restart-dispatch (and stop-shims t) timeout))

;;;###autoload
(defun agent-repl-frontend-daemon-restart-scheduled (reason &optional stop-shims)
  "SCHEDULE the daemon restart instead of demanding it now.
Takes the daemon-global drain lease: no new turn starts anywhere, and the
daemon executes the bounce itself the moment every workspace is quiet.
REASON is folded into the lease's broadcast cause, which is what the
webapp's drain banner shows every other client while they wait.

This is the restart to reach for when work is in flight.  The immediate
`agent-repl-frontend-daemon-restart' is unchanged and still the right call
when nothing is running or the daemon must go NOW.

STOP-SHIMS (the interactive prefix argument) rides the schedule with the
same meaning it has on the immediate path, and is fixed HERE rather than
at drain time because it is a property of what was rebuilt.

Refuses loudly when a schedule already exists or REASON is blank; cancel
with `agent-repl-frontend-daemon-cancel-scheduled-restart'."
  (interactive (list (read-string "Scheduled restart reason: ")
                     current-prefix-arg))
  (unless (and (stringp reason) (not (string-empty-p (string-trim reason))))
    (agent-repl--log nil "scheduled restart command: REFUSING blank reason=%S" reason)
    (user-error "agent-repl: a scheduled restart needs a reason"))
  (let ((cause (format "scheduled restart from Emacs (%s)" (string-trim reason))))
    (agent-repl--log nil
                     "scheduled restart command: invoked interactive=%s stop-shims=%s cause=%S"
                     (if (called-interactively-p 'interactive) "t" "nil")
                     (if stop-shims "t" "nil") cause)
    (let ((req (agent-repl--frontend-request-scheduled-shutdown
                cause (and stop-shims t))))
      (message "agent-repl: restart scheduled; the daemon bounces when every workspace drains")
      req)))

;;;###autoload
(defun agent-repl-frontend-daemon-cancel-scheduled-restart ()
  "Cancel the scheduled daemon restart and release the drain lease.
Uses the schedule id from the last pushed lease view.  With no schedule
recorded this fails loudly rather than reporting a cancel that never
happened."
  (interactive)
  (agent-repl--log nil
                   "cancel scheduled restart command: invoked interactive=%s"
                   (if (called-interactively-p 'interactive) "t" "nil"))
  (let ((req (agent-repl--frontend-request-cancel-scheduled-shutdown)))
    (message "agent-repl: scheduled restart cancelled")
    req))

(provide 'daemon)

;;; daemon.el ends here
