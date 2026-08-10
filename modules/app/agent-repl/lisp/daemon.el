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
(declare-function agent-repl--info "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--warn "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--log-verbose "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--error "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--backend-phase "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--backend-output-tail "agent-repl-core" (output &optional lines))
(declare-function agent-repl--logfile-path "agent-repl-core" ())
(declare-function agent-repl--global-state-file "agent-repl-core" (relative))
(declare-function agent-repl--doctor-log "agent-repl-doctor" (fmt &rest args))
(declare-function agent-repl--frontend-turn-active-sessions "agent-repl-frontend-client" ())
(declare-function agent-repl-runtime-restart "services" (&optional stop-shims initiator))
(declare-function agent-repl-runtime-restart-await "services" (&optional stop-shims timeout initiator))
(declare-function agent-repl--uds-connected-p "frontend-uds" ())
(declare-function agent-repl-uds-probe-async "frontend-uds" (path on-open on-failure))
(declare-function agent-repl--uds-run-timer "frontend-uds" (seconds function &rest args))
(declare-function agent-repl-uds-connect "frontend-uds" (&optional path readiness-p))
(declare-function agent-repl--uds-send-command "frontend-uds" (field payload &optional workspace process &rest keys))
(declare-function agent-repl--frontend-daemon-view-binary-mtime-seconds "frontend-state" ())
(declare-function agent-repl-frontend-shutdown-schedule "frontend-state" ())
(declare-function agent-repl-frontend-scheduled-shutdown-id "frontend-state" ())
(declare-function agent-repl-failure-local "failure" (type message &optional detail))
(declare-function agent-repl-failure-surface "failure" (workspace failure &optional verb))

;; Forward declaration: the connection lifecycle hook lives in frontend-uds.el,
;; which loads AFTER this file.  `add-hook' copes with an unbound symbol; this
;; only keeps byte-compilation from reporting a free variable.
(defvar agent-repl-uds-connected-functions)

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
    (file-name-as-directory (expand-file-name ".." module-dir)))
  "Absolute path to the `modules/app/agent-repl/' directory.
Anchors the frontend build script and artifact locations.
This file lives in `lisp/', one level below that root, hence the `..'.")

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

(defcustom agent-repl-frontend-stop-grace-seconds 30.0
  "Seconds to wait for `claude-repld' to exit after SIGTERM.
The daemon's TERM handler drains its sessions and flushes the session
registry; only when the process outlives this window does
`agent-repl--frontend-stop-daemon' fall back to `delete-process'
\(SIGKILL).  The registry is write-through crash-safe, so the fallback
loses nothing durable — the grace window just lets sessions drain
cleanly.

THIS BUDGET IS SIZED FROM THE DRAIN IT WAITS ON, not from how long a
bounce feels.  `Server.ShutdownAll' first JOINS the in-flight idle sweep
\(`<-s.sweeperDone'), then, in stop-shims mode, hibernates every
non-terminal session one at a time.  At production roster sizes that is
seconds of work, and the previous 3.0 could not cover it: two consecutive
`deploy-all.sh --force' runs sent TERM, watched the daemon still running
3.0s later, and SIGKILLed it — which is exactly the drain-skipping exit
implicated in the orphaned-merge incident, since a killed daemon
reconstructs no merges and releases no leases.  The escalation to SIGKILL
is deliberately KEPT: a daemon that ignores TERM for this long is wedged,
and leaving it holding the port would block its own replacement."
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

(defconst agent-repl--frontend-daemon-buffer-max-chars (* 512 1024)
  "Characters of daemon output `*claude-repld*' retains before trimming.

The buffer is a CRASH TAIL, never an archive.  Everything structured the
daemon prints it has already written durably to
`agent-repl--frontend-daemon-log-path', so an unbounded buffer is
duplication that only grows: across one long-lived daemon it reached
tens of megabytes, and at exit that string was formatted whole into a
log line and split line-by-line, freezing Emacs for over ten minutes.

512KB is far more than any panic, goroutine dump or flag error needs and
small enough that consuming the whole of it stays cheap.")

(defvar agent-repl--frontend-daemon-line-accumulator ""
  "Trailing PARTIAL line of daemon output, held until its newline arrives.

`make-process' chunks a pipe wherever the kernel happened to split it,
so a record straddling two chunks reaches the filter as two fragments.
Without this the mirror logged both halves as separate lines and the
record was destroyed — observed on a real daemon panic whose goroutine
dump landed spliced through unrelated records and could not be read.
The capture BUFFER never had this problem (it inserts raw chunks); only
the log mirror did.")

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

(defun agent-repl--frontend-build-label (targets)
  "Return the terse phrase naming TARGETS in a minibuffer phase line.
Nil TARGETS is the script's own default set, which is what the user sees
on every ordinary bounce, so it is named rather than printed as `nil'."
  (if targets
      (string-join targets "/")
    "shim/webapp/daemon"))

(defun agent-repl--frontend-build-assert-script ()
  "Signal unless the shared build script is present on disk.
Every build run — blocking or asynchronous — starts here, so a missing
script is one failure with one message rather than one per caller."
  (unless (file-exists-p agent-repl--frontend-build-script)
    (agent-repl--log nil "frontend build: script missing path=%s"
                     agent-repl--frontend-build-script)
    (error "agent-repl: frontend build script not found: %s"
           agent-repl--frontend-build-script)))

(defun agent-repl--frontend-build-args (targets force)
  "Return the argv following the shell for a build of TARGETS.
The script path, `--force' when FORCE is non-nil, then TARGETS — the one
place that shape is spelled out, so the blocking and asynchronous runs
cannot invoke the script differently."
  (append (list agent-repl--frontend-build-script)
          (when force '("--force"))
          targets))

(defun agent-repl--frontend-build-reset-capture ()
  "Empty the build capture buffer so a run's output is only its own."
  (with-current-buffer (get-buffer-create agent-repl--frontend-build-buffer)
    (erase-buffer)))

(defun agent-repl--frontend-build-captured-output ()
  "Return the build capture buffer's contents, trailing whitespace trimmed."
  (with-current-buffer (get-buffer-create agent-repl--frontend-build-buffer)
    (string-trim-right (buffer-string))))

(defun agent-repl--frontend-build-log-outcome (targets force exit-code output)
  "Copy a settled build's TARGETS, FORCE, EXIT-CODE and OUTPUT to the log.
The full capture (stdout AND stderr, which the build process merges into
the destination buffer) reaches the durable record BEFORE success or
failure is decided, so a build that fails is never the only evidence that
it ran."
  (agent-repl--log nil
                   "frontend build-if-stale targets=%S force=%s exit=%S output=%s"
                   (or targets 'default) (if force "t" "nil") exit-code
                   (if (string-empty-p output) "<empty>" output)))

(defun agent-repl--frontend-run-report-failure (phase-subject error-subject exit-code output)
  "Surface a failed script run and return its failure detail string.
PHASE-SUBJECT names the run in the minibuffer phase line, ERROR-SUBJECT
in the returned message; EXIT-CODE and OUTPUT are the run's own.  Raises
the phase line, pops the capture buffer, and hands back the message.
The blocking callers signal with it; the asynchronous one, which has no
stack to signal on, warns with it and passes it to its failure
continuation."
  (agent-repl--backend-phase
   nil "%s FAILED (exit %s): %s — full output in %s"
   phase-subject exit-code (agent-repl--backend-output-tail output)
   (agent-repl--logfile-path))
  (display-buffer agent-repl--frontend-build-buffer)
  (format "agent-repl: %s failed (exit %s) — see %s"
          error-subject exit-code agent-repl--frontend-build-buffer))

(defun agent-repl--frontend-run-report-success (phrase started)
  "Raise the phase line reading PHRASE for a run that began at STARTED."
  (agent-repl--backend-phase nil "%s (%.1fs)" phrase
                             (- (float-time) started)))

(defun agent-repl--frontend-build-report-failure (label exit-code output)
  "Surface a failed LABEL build (EXIT-CODE, OUTPUT) and return its detail."
  (agent-repl--frontend-run-report-failure
   (format "%s build" label) "frontend build" exit-code output))

(defun agent-repl--frontend-build-report-success (label started)
  "Raise the phase line for a LABEL build that began at STARTED."
  (agent-repl--frontend-run-report-success (format "%s built" label) started))

(defun agent-repl--frontend-build-targets-if-stale (targets &optional force)
  "Build stale TARGETS through the shared artifact orchestrator.
TARGETS is a list of build-frontend target strings, or nil for its normal
shim/webapp/daemon set.  With FORCE non-nil, every selected artifact is
rebuilt.  The complete captured subprocess output is copied into the
persistent agent-repl log before success or failure is decided.

BLOCKS until the build settles.  Callers that must not stall the main
thread use `agent-repl--frontend-build-targets-async' instead."
  (agent-repl--log nil "frontend build-if-stale: requested targets=%S force=%s script=%s"
                   (or targets 'default) (if force "t" "nil")
                   agent-repl--frontend-build-script)
  (agent-repl--frontend-build-assert-script)
  (agent-repl--frontend-build-reset-capture)
  (let* ((label (agent-repl--frontend-build-label targets))
         (started (float-time))
         (args (agent-repl--frontend-build-args targets force)))
    (agent-repl--backend-phase nil "rebuilding %s if stale…" label)
    (let* ((exit-code (agent-repl--frontend-run-build-script args))
           (output (agent-repl--frontend-build-captured-output)))
      (agent-repl--frontend-build-log-outcome targets force exit-code output)
      (unless (eq exit-code 0)
        (error "%s" (agent-repl--frontend-build-report-failure
                     label exit-code output)))
      (agent-repl--frontend-build-report-success label started)
      exit-code)))

(defun agent-repl--frontend-build-if-stale (&optional force)
  "Build stale shim, webapp, and daemon artifacts.
With FORCE non-nil, rebuild all three.  Signals loudly on failure.

Covers the three build-frontend targets ONLY.  The boot path wants the
whole stack and calls `agent-repl--frontend-deploy-stack' instead; this
stays the narrow build for callers that own the rest of the deploy
themselves (notably `agent-repl-runtime-restart', which kickstarts the
services in elisp and would otherwise do it twice)."
  (agent-repl--frontend-build-targets-if-stale nil force))

;;;; ---- Script runs, asynchronously --------------------------------------
;;
;; The blocking runs above freeze Emacs for the whole of an npm build or a
;; whole-stack deploy.  That is wrong everywhere it is reachable from a
;; user action: an interactive verb that only wants a REDEPLOY afterwards
;; (`agent-repl-restart-session' rebuilding the webapp while the shim
;; restart flies in parallel), and — the expensive one — the lazy daemon
;; ensure on the panel-open path, where a `call-process' stalls the main
;; thread long enough to starve the 1Hz heartbeat and the update chain and
;; to burn the daemon's own command deadlines while nothing can read the
;; socket.
;;
;; ONE QUEUE FOR EVERY SCRIPT, NOT ONE PER SCRIPT.  The build script and
;; the deploy script write the SAME artifacts and share the SAME capture
;; buffer, so two of them running at once would interleave their output
;; and race their outputs onto disk.  Routing both through this single
;; queue makes that overlap unrepresentable rather than unlikely.
;;
;; SINGLE-FLIGHT, NEVER STACKED.  At most one script process exists at a
;; time.  A request arriving while one is in flight is QUEUED behind it
;; rather than joined: the in-flight run may have started before the edit
;; that prompted the new request, so its result cannot be handed back as
;; if it covered that edit.  Two pending requests with identical argv
;; coalesce into one queued run, so N presses of a verb cost at most one
;; extra run.

(cl-defstruct (agent-repl--frontend-run-request
               (:constructor agent-repl--make-frontend-run-request)
               (:copier nil))
  "One asynchronous script run: what to run, how to report it, who waits.

ARGS is the full argument list following the shell interpreter, and is
the run's IDENTITY: two requests with `equal' ARGS ask for the same work
and coalesce.

PHASE-START, SUCCESS-PHRASE, FAILURE-PHASE-SUBJECT and
FAILURE-ERROR-SUBJECT are the already-resolved strings this run reports
itself with, so the queue never has to know which script it is driving.
LOG-OUTCOME is a function of (EXIT-CODE OUTPUT) writing the run's durable
record before success or failure is decided.

CALLBACKS is a list of (ON-SUCCESS . ON-FAILURE) conses, oldest first;
either half may be nil.  STARTED is the `float-time' the process was
spawned, filled in at spawn rather than at request time so a queued
request reports its own duration."
  args phase-start success-phrase failure-phase-subject failure-error-subject
  log-outcome callbacks started)

(defvar agent-repl--frontend-async-run-process nil
  "The live asynchronous script process, or nil when none is running.")

(defvar agent-repl--frontend-async-run-request nil
  "The `agent-repl--frontend-run-request' currently in flight, or nil.")

(defvar agent-repl--frontend-async-run-queue nil
  "Requests waiting for the in-flight asynchronous script run to settle.
Oldest first.  Drained one at a time, so the invariant that exactly one
script process exists holds across the whole queue.")

(defun agent-repl--frontend-async-run-in-flight-p ()
  "Return non-nil while an asynchronous script process is running."
  (and agent-repl--frontend-async-run-process
       (process-live-p agent-repl--frontend-async-run-process)))

(defun agent-repl--frontend-spawn-run-script (args)
  "External-boundary wrapper: spawn the build shell with ARGS asynchronously.
ARGS is the full argument list following the shell, so the SAME wrapper
spawns the build script and the deploy script.  Output is captured into
`agent-repl--frontend-build-buffer' and the exit is delivered to
`agent-repl--frontend-async-run-sentinel'.  Body does nothing but invoke
the external process so tests mock it via `cl-letf'; registered in
`agent-repl--external-boundary-functions'."
  (make-process ;; ALLOW-EXTERNAL-BOUNDARY
   :name "agent-repl-run-frontend-script"
   :buffer agent-repl--frontend-build-buffer
   :command (cons agent-repl-frontend-build-shell args)
   :noquery t
   :connection-type 'pipe
   :sentinel #'agent-repl--frontend-async-run-sentinel))

(defun agent-repl--frontend-async-run-start (request)
  "Spawn the script process for REQUEST and track it as the in-flight run."
  (agent-repl--frontend-build-reset-capture)
  (setf (agent-repl--frontend-run-request-started request) (float-time))
  (agent-repl--log nil "frontend run-async: start args=%S"
                   (agent-repl--frontend-run-request-args request))
  (agent-repl--backend-phase
   nil "%s" (agent-repl--frontend-run-request-phase-start request))
  (setq agent-repl--frontend-async-run-request request
        agent-repl--frontend-async-run-process
        (agent-repl--frontend-spawn-run-script
         (agent-repl--frontend-run-request-args request))))

(defun agent-repl--frontend-async-run-enqueue (request)
  "Park REQUEST behind the in-flight script run.
Returns `coalesced' when a queued request already asks for the identical
argv and absorbed REQUEST's callbacks, `queued' when a fresh entry was
added."
  (let* ((args (agent-repl--frontend-run-request-args request))
         (pending (cl-find-if
                   (lambda (r) (equal (agent-repl--frontend-run-request-args r) args))
                   agent-repl--frontend-async-run-queue)))
    (if pending
        (progn
          (setf (agent-repl--frontend-run-request-callbacks pending)
                (append (agent-repl--frontend-run-request-callbacks pending)
                        (agent-repl--frontend-run-request-callbacks request)))
          (agent-repl--log nil
                           "frontend run-async: run in flight; coalesced into the queued run args=%S"
                           args)
          'coalesced)
      (setq agent-repl--frontend-async-run-queue
            (append agent-repl--frontend-async-run-queue (list request)))
      (agent-repl--log nil
                       "frontend run-async: run in flight; queued behind it rather than stacking a second args=%S queue-depth=%d"
                       args (length agent-repl--frontend-async-run-queue))
      'queued)))

(defun agent-repl--frontend-async-run-drain ()
  "Start the next queued run, if any, now that the in-flight one settled."
  (let ((next (pop agent-repl--frontend-async-run-queue)))
    (when next
      (agent-repl--log nil "frontend run-async: dequeued args=%S remaining=%d"
                       (agent-repl--frontend-run-request-args next)
                       (length agent-repl--frontend-async-run-queue))
      (agent-repl--frontend-async-run-start next))))

(defun agent-repl--frontend-async-run-settle (exit-code)
  "Report the in-flight run's EXIT-CODE, run its callbacks, drain the queue.
Signals when no request is in flight: the sentinel only ever fires for a
process this module spawned and tracked, so a settle with nothing tracked
is a broken invariant rather than a condition to cope with.

The transfer of the in-flight request out of the module's state runs with
quit inhibited: a `C-g' landing between reading the request and clearing
the two variables would leave a settled process tracked as live and every
queued run parked behind it forever."
  (let ((request agent-repl--frontend-async-run-request))
    (unless request
      (error "agent-repl: asynchronous script run settled with no request in flight"))
    (let ((inhibit-quit t))
      (setq agent-repl--frontend-async-run-request nil
            agent-repl--frontend-async-run-process nil))
    (let ((callbacks (agent-repl--frontend-run-request-callbacks request))
          (output (agent-repl--frontend-build-captured-output)))
      (funcall (agent-repl--frontend-run-request-log-outcome request)
               exit-code output)
      ;; The queue is drained even when a continuation throws, so one bad
      ;; waiter cannot strand every run behind it.
      (unwind-protect
          (if (eq exit-code 0)
              (progn
                (agent-repl--frontend-run-report-success
                 (agent-repl--frontend-run-request-success-phrase request)
                 (agent-repl--frontend-run-request-started request))
                (dolist (cb callbacks)
                  (when (car cb) (funcall (car cb)))))
            (let ((detail (agent-repl--frontend-run-report-failure
                           (agent-repl--frontend-run-request-failure-phase-subject request)
                           (agent-repl--frontend-run-request-failure-error-subject request)
                           exit-code output)))
              (agent-repl--warn nil "frontend run-async: %s" detail)
              (dolist (cb callbacks)
                (when (cdr cb) (funcall (cdr cb) detail)))))
        (agent-repl--frontend-async-run-drain)))))

(defun agent-repl--frontend-async-run-sentinel (proc _event)
  "Deliver PROC's exit status to the asynchronous script settle path."
  (unless (process-live-p proc)
    (agent-repl--frontend-async-run-settle (process-exit-status proc))))

(defun agent-repl--frontend-run-script-async (request)
  "Run REQUEST's script in the background, without blocking the main thread.
Returns `started' when REQUEST spawned the process, `queued' when a run
was already in flight and REQUEST was parked behind it, or `coalesced'
when a queued request already asked for the identical argv and absorbed
REQUEST's callbacks.  A second process is never stacked on the first."
  (if (agent-repl--frontend-async-run-in-flight-p)
      (agent-repl--frontend-async-run-enqueue request)
    (agent-repl--frontend-async-run-start request)
    'started))

(defun agent-repl--frontend-build-targets-async (targets &optional force on-success on-failure)
  "Build stale TARGETS in the background, without blocking the main thread.
TARGETS is a list of build-frontend target strings, or nil for its normal
shim/webapp/daemon set.  With FORCE non-nil, every selected artifact is
rebuilt.  ON-SUCCESS is called with no arguments once the build exits
zero; ON-FAILURE is called with the failure detail string otherwise,
after the failure has already been logged, phase-lined and warned about.

Signals immediately when the build script is missing — that is the
caller's own broken installation, not an outcome to deliver to a
continuation.

Returns `started', `queued' or `coalesced' per
`agent-repl--frontend-run-script-async'."
  (agent-repl--frontend-build-assert-script)
  (let ((label (agent-repl--frontend-build-label targets)))
    (agent-repl--frontend-run-script-async
     (agent-repl--make-frontend-run-request
      :args (agent-repl--frontend-build-args targets force)
      :phase-start (format "rebuilding %s if stale…" label)
      :success-phrase (format "%s built" label)
      :failure-phase-subject (format "%s build" label)
      :failure-error-subject "frontend build"
      :log-outcome (lambda (exit-code output)
                     (agent-repl--frontend-build-log-outcome
                      targets force exit-code output))
      :callbacks (list (cons on-success on-failure))))))

(defun agent-repl--frontend-deploy-assert-script ()
  "Signal unless the whole-stack deploy script is present on disk."
  (unless (file-exists-p agent-repl--frontend-deploy-script)
    (agent-repl--log nil "frontend deploy-stack: script missing path=%s"
                     agent-repl--frontend-deploy-script)
    (error "agent-repl: deploy script not found: %s"
           agent-repl--frontend-deploy-script)))

(defun agent-repl--frontend-deploy-args (force)
  "Return the argv following the shell for a whole-stack deploy.

`--no-daemon-bounce' is not an optimization.  The script\='s last step
restarts the daemon by evaluating a form in Emacs over emacsclient, so a
call made FROM Emacs would re-enter the very session that is mid-boot.
The caller starts the daemon itself once the deploy settles, which is
what that step would have been asking for anyway."
  (append (list agent-repl--frontend-deploy-script "--no-daemon-bounce")
          (when force (list "--force"))))

(defun agent-repl--frontend-deploy-log-outcome (force exit-code output)
  "Copy a settled deploy's FORCE, EXIT-CODE and OUTPUT to the durable log."
  (agent-repl--log nil "frontend deploy-stack: force=%s exit=%S output=%s"
                   (if force "t" "nil") exit-code
                   (if (string-empty-p output) "<empty>" output)))

(defun agent-repl--frontend-deploy-stack-async (&optional force on-success on-failure)
  "Deploy the WHOLE agent-repl stack in the background, then continue.
Runs `bin/deploy-all.sh --no-daemon-bounce': protobuf regeneration, the
shim/webapp/daemon build, a forced daemon rebuild (proto codegen lands
outside `daemon/', where its staleness check cannot see it), and the
shim-store and sidecar binaries with a launchd kickstart for whichever of
them is not already running its installed build.

ON-SUCCESS is called with no arguments once the deploy exits zero;
ON-FAILURE is called with the failure detail string otherwise, after the
failure has already been logged, phase-lined and warned about.

THERE IS NO BLOCKING DEPLOY.  This runs on the lazy panel-open path, and
a `call-process' there held the main thread for the whole npm/Go build:
the 1Hz heartbeat and the workspace update chain stopped ticking, the UDS
socket went unread, and the daemon\='s own command deadlines expired
against an Emacs that could not answer.  Every one of those is a
consequence of WAITING rather than of the deploy itself, so the wait is
gone rather than shortened.

Signals immediately when the deploy script is missing — that is the
caller\='s own broken installation, not an outcome to deliver to a
continuation.

WHY THIS IS ON THE BOOT PATH.  It used to be `build-frontend.sh', which
left two gaps: a proto change reached neither the regenerated Go nor a
daemon rebuilt against it, and the two launchd services were only ever
deployed by the interactive `agent-repl-runtime-restart'.  A wire-format
change could therefore leave a new Emacs talking to a daemon built before
it, which fails every command rather than degrading.

Returns `started', `queued' or `coalesced' per
`agent-repl--frontend-run-script-async'."
  (agent-repl--log nil "frontend deploy-stack: requested force=%s script=%s"
                   (if force "t" "nil") agent-repl--frontend-deploy-script)
  (agent-repl--frontend-deploy-assert-script)
  (agent-repl--frontend-run-script-async
   (agent-repl--make-frontend-run-request
    :args (agent-repl--frontend-deploy-args force)
    :phase-start "deploying the backend stack…"
    :success-phrase "backend stack deployed"
    :failure-phase-subject "stack deploy"
    :failure-error-subject "stack deploy"
    :log-outcome (lambda (exit-code output)
                   (agent-repl--frontend-deploy-log-outcome force exit-code output))
    :callbacks (list (cons on-success on-failure)))))

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

(defun agent-repl--frontend-daemon-log-path ()
  "Return the daemon's own durable structured log, `claude-repld.log'.
The daemon writes it itself under the shared state root; every record
the capture buffer trims away is still there, which is what makes
trimming the buffer a loss of duplication rather than a loss of
evidence."
  (agent-repl--global-state-file "claude-repld.log"))

(defun agent-repl--frontend-daemon-trim-capture (proc)
  "Trim the current buffer to the newest daemon output, keeping PROC's mark sane.

Retains at most `agent-repl--frontend-daemon-buffer-max-chars', cut
FORWARD to a line boundary so the surviving region starts on a whole
line rather than mid-record."
  (let ((cap agent-repl--frontend-daemon-buffer-max-chars))
    (when (> (- (point-max) (point-min)) cap)
      ;; Undo would re-accumulate everything the cap just discarded, so the
      ;; buffer would stay bounded while its undo list did not.  A process
      ;; capture buffer is never edited by hand; there is nothing to undo.
      (unless (eq buffer-undo-list t)
        (setq buffer-undo-list t))
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (- (point-max) cap))
          (let ((cut (if (bolp) (point) (line-beginning-position 2))))
            ;; `line-beginning-position' 2 answers `point-max' when no newline
            ;; lies ahead — one line longer than the whole cap.  Keeping a
            ;; WHOLE line would then mean keeping nothing, so cut mid-line
            ;; instead: the newest output outranks the alignment.
            (when (>= cut (point-max))
              (setq cut (- (point-max) cap)))
            (delete-region (point-min) cut))))
      ;; `delete-region' relocates any marker inside the deleted span to its
      ;; start, so a process mark that had fallen behind the retained region
      ;; now sits at `point-min' instead of at the insertion point the next
      ;; chunk must extend.  Re-anchor it at the end of what survived.
      (when (processp proc)
        (let ((mark (process-mark proc)))
          (when (and (markerp mark)
                     (marker-position mark)
                     (< (marker-position mark) (point-max)))
            (set-marker mark (point-max))))))))

(defun agent-repl--frontend-daemon-filter (proc chunk)
  "Capture CHUNK from PROC into its buffer AND the durable structured log.

`make-process' with no `:stderr' merges the daemon's standard error into
its standard output, and before this filter existed BOTH landed only in
`*claude-repld*' — a buffer no runbook reads and nothing persists.  A
daemon that refuses to start prints the reason there and exits, so the
sentinel's bare exit event was the entire record of a failure that had
already explained itself.

The daemon owns `daemon.log' for its own structured records.  This
mirrors only what it wrote to a TERMINAL, at the quiet debug rung, so the
two never become duplicate narratives of one event.

Two things make that promise real, and neither is cosmetic — this filter
runs on the SAME event loop that must drain the frontend UDS, and the
daemon hard-disconnects a host client whose outbound queue it fills
faster than the client reads (`frontend: slow consumer', a 256-frame
bound).  At boot the mirror was doing ~130 log writes a second while the
UDS managed 14 frame dispatches in sixteen seconds; the daemon evicted
Emacs, and the sentinel reported the eviction as a broken connection.
So:

- Lines are reassembled across chunk boundaries via
  `agent-repl--frontend-daemon-line-accumulator' before anything is
  logged.  A record split by the kernel is one record, not two.
- A line the daemon ALREADY wrote to its own structured log
  (`runtime':\"daemon\") is not mirrored again — that is the duplicate
  narrative the docstring above forbids, and it was the largest single
  share of the volume.  Everything else is mirrored verbatim: the
  relayed `sidecar' and `webapp' records, whose ONLY durable home is
  this mirror, and every unstructured line — panics, goroutine dumps,
  flag errors, Go runtime output — which is the failure evidence this
  filter was built for.

The capture buffer is trimmed to
`agent-repl--frontend-daemon-buffer-max-chars' after every chunk, so a
daemon that runs for days cannot turn it into the tens-of-megabytes
string that froze Emacs at exit."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert chunk)
          (set-marker (process-mark proc) (point)))
        (when moving (goto-char (process-mark proc))))
      (agent-repl--frontend-daemon-trim-capture proc)))
  (let* ((pending (concat agent-repl--frontend-daemon-line-accumulator
                          (or chunk "")))
         (parts (split-string pending "\n"))
         ;; The final element is whatever followed the last newline: a
         ;; complete line only if the chunk ended on one, in which case
         ;; `split-string' leaves "" there.  Either way it is exactly the
         ;; carry-over, so taking it unconditionally is correct.
         (complete (butlast parts)))
    (setq agent-repl--frontend-daemon-line-accumulator (car (last parts)))
    (dolist (line complete)
      (let ((line (string-trim line)))
        (unless (or (string-empty-p line)
                    (agent-repl--frontend-daemon-own-record-p line))
          (agent-repl--log nil "claude-repld output: %s" line))))))

(defun agent-repl--frontend-daemon-own-record-p (line)
  "Return non-nil when LINE is a record the daemon already logged itself.

Its own structured records carry a leading `\"runtime\":\"daemon\"' and
land in `claude-repld.log' durably, so mirroring them is pure
duplication.

The match is ANCHORED at the head of the record rather than searched for
anywhere in it, because a relayed `sidecar' or `webapp' record may quote
the daemon's runtime tag inside its own message text, and skipping one of
those would destroy the only durable copy that exists.  Anchoring also
fixes the failure DIRECTION: if the daemon ever reorders its envelope
this stops matching and the line is mirrored again — redundant, never
absent."
  (string-match-p "\\`{\"timestamp\":\"[^\"]*\",\"runtime\":\"daemon\"" line))

(defun agent-repl--frontend-daemon-flush-partial-line ()
  "Mirror any held partial daemon line and clear the accumulator.

Called when no newline can still arrive for it — the daemon is gone.  The
held text is mirrored WITHOUT the own-record filter: a line the daemon
did not finish writing to stdout is a line it very likely did not finish
writing to its own log either, so the duplication argument that justifies
skipping complete daemon records does not hold for this one."
  (let ((partial (string-trim agent-repl--frontend-daemon-line-accumulator)))
    (setq agent-repl--frontend-daemon-line-accumulator "")
    (unless (string-empty-p partial)
      (agent-repl--log nil "claude-repld output: %s" partial))))

(defconst agent-repl--frontend-daemon-output-tail-chars 16384
  "Characters of the daemon capture `agent-repl--frontend-daemon-output' returns.

The capture buffer is already capped, but this bound does not depend on
that one: the buffer can predate the cap, and nothing else guarantees a
caller of the exit path never meets a buffer someone else filled.  A
panic with its goroutine dump fits comfortably; the complete record
lives in `agent-repl--frontend-daemon-log-path' either way.")

(defun agent-repl--frontend-daemon-output ()
  "Return the TAIL of the captured `claude-repld' terminal output, trimmed.

At most `agent-repl--frontend-daemon-output-tail-chars' characters, read
directly out of the buffer's final region rather than by materializing
the whole capture.  That distinction is the incident: taking
`buffer-string' of a multi-megabyte capture and handing it onward froze
Emacs for over ten minutes inside a sentinel, where quit is inhibited.

Empty string when the capture buffer was never created."
  (let ((buffer (get-buffer agent-repl--frontend-daemon-buffer)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (string-trim-right
           (buffer-substring-no-properties
            (max (point-min)
                 (- (point-max) agent-repl--frontend-daemon-output-tail-chars))
            (point-max))))
      "")))

(defun agent-repl--frontend-spawn-daemon ()
  "External-boundary wrapper: spawn `claude-repld' via `make-process'.
Body does nothing but invoke `make-process' with the daemon argv, filter
and sentinel, returning the live process.  Tests mock it via `cl-letf';
registered in `agent-repl--external-boundary-functions'."
  (make-process ;; ALLOW-EXTERNAL-BOUNDARY
   :name "claude-repld"
   :buffer agent-repl--frontend-daemon-buffer
   :command (agent-repl--frontend-daemon-command)
   :noquery t
   :filter #'agent-repl--frontend-daemon-filter
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
    (agent-repl--backend-phase nil "starting the daemon…")
    ;; A dead daemon's unterminated last line belongs to THAT daemon.  Carrying
    ;; it across a spawn would glue it onto the next daemon's first record and
    ;; corrupt both — and the last line before a death is exactly the line a
    ;; crash investigation reads.
    (setq agent-repl--frontend-daemon-line-accumulator "")
    (let ((proc (agent-repl--frontend-spawn-daemon)))
      (setq agent-repl--frontend-daemon-process proc)
      (agent-repl--log nil "claude-repld started (pid %s) on %s"
                        (process-id proc) agent-repl-frontend-daemon-addr)
      (agent-repl--backend-phase nil "daemon up (pid %s) on %s"
                                 (process-id proc)
                                 agent-repl-frontend-daemon-addr)
      proc)))

;;;; ---- The expected-restart window --------------------------------------
;;
;; A deploy restarts the daemon THROUGH EMACS: `bin/deploy-all.sh' calls
;; `agent-repl-frontend-daemon-restart-await' over emacsclient, and the
;; coordinator in services.el stops the daemon itself.  The exit that follows
;; is therefore the one Emacs just ordered — yet the sentinel classified it as
;; `client.daemon_exited', so every single deploy produced a WARNING plus a
;; transient failure card for a death Emacs had orchestrated.
;;
;; This window is what separates "Emacs ordered this exit" from "the daemon
;; died".  Three properties make it safe to suppress on:
;;
;;   1. It is armed only by the restart coordinator, immediately before the
;;      bounce, and only when a daemon is actually there to be stopped.
;;   2. It is BOUNDED.  A daemon that dies and STAYS dead surfaces exactly the
;;      card and the warn it surfaces today, when the window expires without a
;;      replacement.  The suppressed failure is retained, never dropped.
;;   3. It closes the moment the replacement daemon's link comes back.
;;
;; An exit with NO window armed is untouched: that path is the daemon's only
;; failure surfacing anywhere, because the process that composes every other
;; failure card is the one that just died.

(defcustom agent-repl-frontend-expected-restart-window-seconds 180.0
  "Seconds a deliberate restart may suppress the daemon-exit failure card.
Armed by the runtime restart coordinator just before it stops the daemon,
and closed as soon as the replacement daemon's UDS link opens.  A daemon
that never comes back inside this window has its suppressed
`client.daemon_exited' failure surfaced when the window expires, so the
suppression can delay that card but can never cancel it."
  :type 'number
  :group 'agent-repl)

(defvar agent-repl--frontend-expected-restart nil
  "State of the armed expected-restart window, or nil when none is armed.
A plist: `:initiator' (who ordered the restart), `:armed-at' (`float-time'),
`:timer' (the expiry timer), and — once an exit has been observed inside the
window — `:exit' (the withheld failure), `:event' and `:tail' (its echo
material).")

(defvar agent-repl--frontend-expected-restart-last-close nil
  "The last window CLOSED by its replacement daemon reconnecting, or nil.
A plist: `:initiator', `:armed-at' and `:closed-at' (`float-time').

Kept after the window itself is gone because work that was already in
flight when the daemon went away can still be finishing when the link
comes back, and that work is as much a consequence of the restart as the
exit was.  `agent-repl--frontend-expected-restart-covering-initiator'
attributes such work to this closed window.

Only a reconnect records one.  A window that EXPIRED describes a daemon
that never came back, which is a genuine outage rather than a bounce, so
expiry clears this and every downstream site returns to warning.")

(defun agent-repl--frontend-expected-restart-cancel-timer ()
  "Cancel the armed window's expiry timer, if it still holds one."
  (let ((timer (plist-get agent-repl--frontend-expected-restart :timer)))
    (when (timerp timer)
      (cancel-timer timer))))

(defun agent-repl--frontend-expected-restart-expire ()
  "Close the window on its bound, surfacing whatever it withheld.
A daemon that exited inside the window and never returned is a daemon that
is simply DOWN, so its `client.daemon_exited' failure is surfaced here
exactly as an unexpected exit surfaces it — same echo, same warn, same
card — only later.  A window that expires having observed no exit at all
closes quietly: there is nothing to report."
  (let ((state agent-repl--frontend-expected-restart))
    (if (null state)
        (agent-repl--log nil "expected-restart: expiry fired with no window armed")
      (agent-repl--frontend-expected-restart-cancel-timer)
      (setq agent-repl--frontend-expected-restart nil)
      ;; No replacement arrived, so nothing downstream gets the benefit of the
      ;; doubt any more: a prior bounce's grace must not soften the reporting
      ;; of an outage this window just proved.
      (setq agent-repl--frontend-expected-restart-last-close nil)
      (let ((initiator (plist-get state :initiator))
            (elapsed (- (float-time) (plist-get state :armed-at)))
            (exit (plist-get state :exit)))
        (if (null exit)
            (agent-repl--log nil
                             "expected-restart: window closed unused initiator=%s elapsed=%.3fs"
                             initiator elapsed)
          (agent-repl--warn nil
                            "expected-restart: window EXPIRED with no replacement daemon initiator=%s elapsed=%.3fs; surfacing the withheld exit"
                            initiator elapsed)
          (agent-repl--backend-phase nil "daemon exited (%s): %s — full output in %s"
                                     (plist-get state :event)
                                     (plist-get state :tail)
                                     (agent-repl--logfile-path))
          (agent-repl-failure-surface nil exit))))))

(defun agent-repl--frontend-arm-expected-restart (initiator)
  "Arm the expected-restart window on behalf of INITIATOR.
INITIATOR names the control-plane caller that ordered the restart and rides
every record the window produces; a blank one is refused, because a window
that cannot say who opened it is indistinguishable from one opened by
accident.

Re-arming REPLACES the window rather than nesting inside it, and carries any
already-withheld exit across: a second restart ordered before the first one's
replacement arrived is a fresh deliberate act with a fresh bound, but the exit
the first window withheld is still owed to the user if nothing ever comes back."
  (unless (and (stringp initiator) (not (string-empty-p (string-trim initiator))))
    (agent-repl--log nil "expected-restart: REFUSING blank initiator=%S" initiator)
    (error "agent-repl: an expected-restart window needs an initiator"))
  (let ((prior agent-repl--frontend-expected-restart))
    (agent-repl--frontend-expected-restart-cancel-timer)
    (setq agent-repl--frontend-expected-restart
          (list :initiator initiator
                :armed-at (float-time)
                :timer nil
                :exit (plist-get prior :exit)
                :event (plist-get prior :event)
                :tail (plist-get prior :tail)))
    (setq agent-repl--frontend-expected-restart
          (plist-put agent-repl--frontend-expected-restart
                     :timer
                     (agent-repl--uds-run-timer
                      agent-repl-frontend-expected-restart-window-seconds
                      #'agent-repl--frontend-expected-restart-expire)))
    (agent-repl--log nil
                     "expected-restart: ARMED initiator=%s window=%.1fs carried-exit=%s"
                     initiator agent-repl-frontend-expected-restart-window-seconds
                     (if (plist-get prior :exit) "t" "nil"))
    initiator))

(defun agent-repl--frontend-expected-restart-initiator ()
  "Return the initiator of the LIVE expected-restart window, or nil.

Nil once the window's bound has elapsed, WHETHER OR NOT its timer got to
run: an elisp hot-reload runs `agent-repl--cancel-all-timers', and a window
whose only bound was a cancelled timer would suppress daemon exits forever.
An elapsed window is expired here instead, which surfaces anything it had
withheld rather than dropping it."
  (let ((state agent-repl--frontend-expected-restart))
    (when state
      (if (>= (- (float-time) (plist-get state :armed-at))
              agent-repl-frontend-expected-restart-window-seconds)
          (progn
            (agent-repl--frontend-expected-restart-expire)
            nil)
        (plist-get state :initiator)))))

(defun agent-repl--frontend-expected-restart-covering-initiator (&optional as-of)
  "Return the initiator of the restart window covering AS-OF, or nil.

AS-OF is a `float-time' naming the moment whose classification is being
asked about; nil asks about NOW.  A live window covers now, and therefore
covers any AS-OF at all — something happening inside an open window is
inside it whatever moment it refers back to.

With no live window, the only other covering window is the last one a
replacement daemon CLOSED, and it covers exactly its own lifetime:
`[:armed-at, :closed-at]'.  That interval is the boundary, not a fixed
grace period, because it is the span during which the daemon was going
away or gone.  Work stamped inside it stalled BECAUSE of the restart;
work stamped before the window was armed predates the restart entirely,
and work stamped after the close had a live daemon to talk to.  Both of
those keep their unsoftened reporting."
  (let ((live (agent-repl--frontend-expected-restart-initiator)))
    (cond
     (live live)
     ((null as-of) nil)
     (t
      (let ((closed agent-repl--frontend-expected-restart-last-close))
        (when (and closed
                   (<= (plist-get closed :armed-at) as-of)
                   (<= as-of (plist-get closed :closed-at)))
          (plist-get closed :initiator)))))))

(defun agent-repl--frontend-expected-restart-withhold-exit (failure event tail)
  "Retain FAILURE, with its EVENT and TAIL echo material, in the armed window.
Withheld, never dropped: `agent-repl--frontend-expected-restart-expire'
surfaces it if no replacement daemon ever arrives."
  (unless agent-repl--frontend-expected-restart
    (agent-repl--log nil "expected-restart: REFUSING to withhold an exit with no window armed")
    (error "agent-repl: no expected-restart window is armed"))
  (setq agent-repl--frontend-expected-restart
        (plist-put agent-repl--frontend-expected-restart :exit failure))
  (setq agent-repl--frontend-expected-restart
        (plist-put agent-repl--frontend-expected-restart :event event))
  (setq agent-repl--frontend-expected-restart
        (plist-put agent-repl--frontend-expected-restart :tail tail))
  (agent-repl--log nil
                   "expected-restart: WITHHELD daemon exit initiator=%s event=%s"
                   (plist-get agent-repl--frontend-expected-restart :initiator)
                   event)
  failure)

(defun agent-repl--frontend-expected-restart-note-reconnect ()
  "Close the expected-restart window once the replacement daemon is reachable.

Only an OBSERVED exit closes it.  The restart coordinator dials the OUTGOING
daemon while it is still serving (its readiness preflight runs before the
bounce), so treating any connect as the replacement's arrival would disarm the
window before the exit it exists to classify ever happened."
  (let ((state agent-repl--frontend-expected-restart))
    (when state
      (if (plist-get state :exit)
          (progn
            (agent-repl--frontend-expected-restart-cancel-timer)
            (setq agent-repl--frontend-expected-restart nil)
            (setq agent-repl--frontend-expected-restart-last-close
                  (list :initiator (plist-get state :initiator)
                        :armed-at (plist-get state :armed-at)
                        :closed-at (float-time)))
            (agent-repl--info nil
                              "expected-restart: replacement daemon connected; window CLOSED initiator=%s elapsed=%.3fs"
                              (plist-get state :initiator)
                              (- (float-time) (plist-get state :armed-at))))
        (agent-repl--log nil
                         "expected-restart: link opened before any daemon exit; window stays armed initiator=%s"
                         (plist-get state :initiator))))))

(add-hook 'agent-repl-uds-connected-functions
          #'agent-repl--frontend-expected-restart-note-reconnect)

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
      ;; A daemon that dies mid-line leaves its last line unterminated, and
      ;; that line is precisely the one a crash investigation wants.  Nothing
      ;; will ever deliver its newline, so flush it here rather than let the
      ;; next spawn's reset discard it.
      (agent-repl--frontend-daemon-flush-partial-line)
      (let* ((output (agent-repl--frontend-daemon-output))
             (tail (agent-repl--backend-output-tail output)))
        ;; The exit EVENT alone ("exited abnormally with code 1") never says
        ;; why.  The reason is whatever the daemon printed on its way out, so
        ;; the capture TAIL rides the durable record and its shorter tail
        ;; rides both the failure card and the echo line.
        ;;
        ;; The tail, not the capture.  This record used to splice the whole
        ;; capture into one format call, which is how a buffer nobody bounded
        ;; became a multi-megabyte log line.  What the tail leaves out is not
        ;; lost: the daemon's own structured records are durable in
        ;; `agent-repl--frontend-daemon-log-path', which the record now names
        ;; so the reader can go there.
        (agent-repl--log nil
                         "claude-repld exited: tracked=%s event=%s output-tail=%s daemon-log=%s"
                         tracked trimmed-event
                         (if (string-empty-p output) "<empty>" output)
                         (agent-repl--frontend-daemon-log-path))
        (let ((failure (agent-repl-failure-local
                        "client.daemon_exited"
                        "the agent-repl daemon exited"
                        (format "%s: %s" trimmed-event tail)))
              (initiator (agent-repl--frontend-expected-restart-initiator)))
          (if initiator
              ;; The exit Emacs itself ordered.  It is a PHASE of the restart,
              ;; so it reads as one: the echo names the initiator and the
              ;; replacement it is waiting for, the record lands at info, and
              ;; no card opens.  The failure is withheld rather than
              ;; discarded — if no replacement ever arrives, the window's
              ;; expiry surfaces exactly this.
              (progn
                (agent-repl--info nil
                                  "claude-repld exited inside the expected-restart window: initiator=%s event=%s"
                                  initiator trimmed-event)
                (agent-repl--backend-phase
                 nil "daemon exited for the %s restart (%s); awaiting the replacement…"
                 initiator trimmed-event)
                (agent-repl--frontend-expected-restart-withhold-exit
                 failure trimmed-event tail))
            ;; Both paths, because the record is genuinely split: the
            ;; unstructured dying lines (panics, Go runtime output) were
            ;; mirrored into agent-repl's own log by the filter, while the
            ;; daemon's structured records only ever land in its own.
            (agent-repl--backend-phase nil "daemon exited (%s): %s — full output in %s and %s"
                                       trimmed-event tail
                                       (agent-repl--logfile-path)
                                       (agent-repl--frontend-daemon-log-path))
            (agent-repl-failure-surface nil failure)))))))

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
the asynchronous socket probe is the real exit signal.  A daemon that
never frees the socket within
`agent-repl-frontend-foreign-stop-grace-seconds' fails loudly and is left
in place; spawning next to it would only bind-fail."
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
Returns the raw stdout string, or nil when the probe cannot run.  Tests
mock it via `cl-letf'; registered in
`agent-repl--external-boundary-functions'.

Beyond the external call the body only RECORDS a probe that failed: the
exit code and the merged output are both lost the moment this returns
nil, and neither is reconstructible by the caller."
  (with-temp-buffer
    (let ((code (call-process ;; ALLOW-EXTERNAL-BOUNDARY
                 agent-repl--frontend-listener-probe-program nil t nil
                 "-nP" "-a" "-iTCP:" "-sTCP:LISTEN" "-Fpc"
                 (concat "-i" "TCP:" port))))
      ;; lsof exits 1 when nothing matches, which is a legitimate "no
      ;; listener" answer rather than a failure.
      (if (memq code '(0 1))
          (buffer-string)
        ;; Any OTHER exit is the probe itself failing, and its diagnosis is
        ;; on the merged stdout/stderr this buffer holds.  Returning nil
        ;; without recording it made a broken probe indistinguishable from a
        ;; free port.
        (let ((output (string-trim-right (buffer-string))))
          (agent-repl--log nil
                           "daemon listener probe: FAILED port=%s exit=%S output=%s"
                           port code
                           (if (string-empty-p output) "<empty>" output))
          nil)))))

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

(defconst agent-repl--frontend-daemon-not-started-detail
  "frontend daemon not started (auto-start disabled or init inhibited)"
  "The failure detail every caller reports when the daemon ensure declines.
`agent-repl--ensure-frontend-daemon' returns nil without acting when
auto-start is off or automatic init is inhibited, and each of its callers
has to say the same thing about it — so it is said once, here.")

(defun agent-repl--ensure-frontend-daemon (&optional force on-ensured on-failure)
  "Ensure frontend daemon startup has been requested; return its state.
Idempotent: returns the live process immediately when one exists (unless
FORCE).  A daemon already answering on the port that this Emacs does
NOT track is ADOPTED asynchronously: spawning next to it would only
bind-fail and die — the orphan-daemon failure mode.  Otherwise deploys
the stack asynchronously and launches `claude-repld'.  FORCE skips
adoption: an explicit restart wants a fresh process (a foreign daemon
cannot be stopped from here — only its owner can).  Returns nil without
acting when `agent-repl-frontend-auto-start' is nil or automatic init is
inhibited (batch).  The post-snapshot startup coordinator in
services.el owns the once-per-Emacs full-runtime bounce; this function
remains the cheap idempotent session-open ensure.  Every probe or lifecycle
transition returns `:pending' immediately and completes from its callback.

ON-ENSURED and ON-FAILURE are optional continuations, and exactly one of
them runs whenever either is supplied.  ON-ENSURED runs once the daemon
this call is responsible for is RUNNING — immediately for a reused or
adopted daemon, and after the asynchronous deploy and launch otherwise.
ON-FAILURE receives a detail string when the ensure declines
\(`agent-repl--frontend-daemon-not-started-detail') or when the deploy or
the launch fails.

WAITING FOR THE ENSURE IS THE CALLER\='S ONLY CORRECT MOVE.  Before the
continuations existed, callers read this function\='s return value as a
yes/no gate and went straight on to readiness polling — which meant the
poll budget started burning while the stack was still building, and a
first open on a cold or stale checkout could exhaust it against a daemon
that had not been launched yet.  Chaining on ON-ENSURED makes that
overlap impossible rather than unlikely."
  (let ((inhibited (agent-repl--frontend-init-inhibited-p))
        (on-ensured (or on-ensured #'ignore))
        (on-failure (or on-failure #'ignore)))
    (agent-repl--log nil
                     "ensure-frontend-daemon: force=%s auto-start=%s init-inhibited=%s tracked-live=%s"
                     (if force "t" "nil") agent-repl-frontend-auto-start inhibited
                     (agent-repl--frontend-daemon-live-p))
    (cond
   ((not agent-repl-frontend-auto-start)
    (agent-repl--log nil "ensure-frontend-daemon: skipped reason=auto-start-disabled")
    (funcall on-failure agent-repl--frontend-daemon-not-started-detail)
    nil)
   (inhibited
    (agent-repl--log nil "ensure-frontend-daemon: skipped reason=init-inhibited")
    (funcall on-failure agent-repl--frontend-daemon-not-started-detail)
    nil)
   (t
    (cond
     ((and (not force) (agent-repl--frontend-daemon-live-p))
      (agent-repl--log nil "ensure-frontend-daemon: reusing tracked pid=%S"
                       (process-id agent-repl--frontend-daemon-process))
      (funcall on-ensured)
      agent-repl--frontend-daemon-process)
     (t
      (cl-labels ((launch ()
                    ;; The launch runs from a process sentinel, where a
                    ;; signal has no stack to land on and would be
                    ;; swallowed as a filter error.  Its failure is routed
                    ;; to ON-FAILURE instead — surfaced by the caller's own
                    ;; failure path rather than lost.
                    (condition-case err
                        (progn (agent-repl--frontend-start-daemon)
                               (funcall on-ensured))
                      (error
                       (let ((detail (error-message-string err)))
                         (agent-repl--warn
                          nil "ensure-frontend-daemon: launch FAILED detail=%s" detail)
                         (funcall on-failure detail)))))
                  (build-and-launch ()
                    (agent-repl--log nil "ensure-frontend-daemon: build-and-launch force=%s"
                                     (if force "t" "nil"))
                    (agent-repl--frontend-deploy-stack-async
                     force #'launch
                     (lambda (detail)
                       (agent-repl--log nil
                                        "ensure-frontend-daemon: deploy FAILED detail=%s" detail)
                       (funcall on-failure detail)))))
        (if (and force (agent-repl--frontend-daemon-live-p))
            (progn
              (agent-repl--log nil "ensure-frontend-daemon: force=t; asynchronously stopping tracked daemon before rebuild")
              (agent-repl--frontend-stop-daemon t nil #'build-and-launch))
          (agent-repl--frontend-daemon-responsive-async
           (lambda ()
             (if force
                 (build-and-launch)
               ;; An adopted daemon is ALREADY the running daemon this
               ;; ensure was asked for, so the waiter proceeds now.
               (agent-repl--log nil "ensure-frontend-daemon: adopting foreign daemon on %s"
                                agent-repl-frontend-daemon-addr)
               (funcall on-ensured)))
           (lambda (detail)
             (agent-repl--log nil "ensure-frontend-daemon: no daemon probe-detail=%S; building" detail)
             (build-and-launch))))
        :pending)))))))

(defun agent-repl--frontend-after-daemon-ensured (on-ensured on-failure &optional force)
  "Run ON-ENSURED once the frontend daemon is running, ON-FAILURE otherwise.

The single door every caller that NEEDS a running daemon goes through, so
the declined-ensure detail string and the wait-for-the-launch discipline
are written once rather than re-derived at each call site.  Exactly one
continuation runs; ON-FAILURE receives a detail string.

Returns whatever `agent-repl--ensure-frontend-daemon' returned — nil when
the ensure declined, the tracked process when one was reused, `:pending'
while a deploy or launch is in flight."
  (unless (and (functionp on-ensured) (functionp on-failure))
    (error "agent-repl: daemon ensure requires callable continuations"))
  (agent-repl--ensure-frontend-daemon force on-ensured on-failure))

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
Refuses while any daemon session reports an in-flight turn ONLY when
STOP-SHIMS is requested, and unless FORCE is non-nil.  Stopping the shims
is what killed live conversations mid-generation in the repeated
daemon-bounce incidents: the process running the turn is the shim, so
ending it ends the turn.  A PRESERVING stop does not touch that process —
the turn keeps running, its events keep landing in the store, and the next
daemon replays them from its durable floor on reattach — so refusing one
protected nothing and blocked every daemon-only deploy behind whatever
happened to be thinking at the time.  An unreachable daemon has nothing to
protect, so the turn probe treats it as idle.

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
      (when (and stop-shims (not force))
        (when-let ((busy (agent-repl--frontend-turn-active-sessions)))
          (agent-repl--log nil
                           "frontend stop: refused force=nil stop-shims=t active-workspaces=%S"
                           busy)
          (error "agent-repl: refusing daemon stop — stop-shims would kill the turn in flight in %s; retry when idle or pass FORCE"
                 busy)))
    (when stop-shims
      (condition-case err
          (agent-repl--frontend-request-foreign-shutdown t)
        (error
         ;; The link can be down while the tracked process is alive. The TERM
         ;; below still stops the daemon; it simply cannot carry the mode, so
         ;; the shims are preserved. Loud, because the caller asked for the
         ;; opposite and is entitled to know it did not happen.
         (agent-repl--warn nil
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
  "Stop the running frontend daemon, PRESERVING its session shims.
The shims outlive the stop and reattach to whatever daemon comes next, so
a turn in flight survives it and this does not refuse for one; FORCE is
retained for the underlying stop\='s own guards.
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
an OPERATOR who explicitly wants every survivor replaced.  The DEPLOY no
longer asks for it, even when the shim bundle moved: the replacement
daemon rolls each stale shim at its own session\='s turn boundary, which
costs no conversation, whereas stopping them all at shutdown kills every
turn that happens to be running.  It is the one mode that refuses while a
turn is in flight."
  (interactive "P")
  (agent-repl-runtime-restart (and stop-shims t)))

(defun agent-repl-frontend-daemon-restart-await (&optional stop-shims timeout)
  "Restart the complete runtime and await its terminal deployment result.
STOP-SHIMS and TIMEOUT are forwarded to
`agent-repl-runtime-restart-await'.  This is the deployment-only companion to
the asynchronous interactive command `agent-repl-frontend-daemon-restart'."
  (agent-repl--log nil
                   "frontend restart-await: requested stop-shims=%s timeout=%S root=%s"
                   (if stop-shims "t" "nil") timeout agent-repl--frontend-root)
  ;; `bin/deploy-all.sh' reaches this over emacsclient, so the user never typed
  ;; a command and Emacs is about to block for the whole coordinated restart.
  ;; Naming the origin is what separates "the deploy is driving" from a frame
  ;; that has simply stopped responding.
  (agent-repl--backend-phase nil "deploy-driven restart requested (stop-shims=%s)"
                             (if stop-shims "t" "nil"))
  ;; Named as the initiator so the daemon exit this restart is about to cause
  ;; is recorded — and echoed — as the deploy's own doing rather than as the
  ;; daemon dying.  See `agent-repl--frontend-arm-expected-restart'.
  (agent-repl-runtime-restart-await (and stop-shims t) timeout
                                    "deploy (emacsclient)"))

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
