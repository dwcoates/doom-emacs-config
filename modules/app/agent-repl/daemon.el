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
(declare-function agent-repl--error "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--in-sandbox-p "agent-repl-install" ())
(declare-function agent-repl--frontend-turn-active-sessions "agent-repl-frontend-client" ())
(declare-function agent-repl--frontend-rebind-workspaces-after-restart "agent-repl-frontend-client" ())
(declare-function agent-repl--uds-socket-live-p "frontend-uds" (&optional path))
(declare-function agent-repl--uds-connected-p "frontend-uds" ())
(declare-function agent-repl-uds-connect "frontend-uds" (&optional path))
(declare-function agent-repl--uds-send-command "frontend-uds" (field payload &optional workspace process))
(declare-function agent-repl--uds-track-command "frontend-uds" (request-id field workspace &optional on-failure on-success))
(declare-function agent-repl--frontend-daemon-view-binary-mtime-seconds "frontend-state" ())

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
    (when (and root (file-directory-p root))
      (let ((worktrees (expand-file-name "explanation-engine-worktrees" root)))
        (append
         (list (expand-file-name (concat "explanation-engine/" rel) root))
         (when (file-directory-p worktrees)
           (mapcar (lambda (d) (expand-file-name rel d))
                   (directory-files worktrees t "\\`[^.]" t)))
         (mapcar (lambda (d) (expand-file-name rel d))
                 (directory-files root t "\\`[^.]" t)))))))

(defun agent-repl--frontend-discover-widget-assets-dir ()
  "Resolve the widget-assets dir the daemon should serve, or nil.
An explicit `agent-repl-frontend-widget-assets-dir' always wins and is
returned verbatim (expanded).  When it is empty, auto-discover the first
candidate (see `agent-repl--frontend-widget-assets-candidates') that
actually holds `chess-widget.js', so a stale or empty dist is skipped."
  (if (not (string-empty-p agent-repl-frontend-widget-assets-dir))
      (expand-file-name agent-repl-frontend-widget-assets-dir)
    (seq-find (lambda (dir)
                (file-exists-p (expand-file-name "chess-widget.js" dir)))
              (agent-repl--frontend-widget-assets-candidates))))

(defun agent-repl--widget-doctor-issues ()
  "Return (LEVEL . MESSAGE) issues for the chess-widget capability.
Warns when no widget-assets dir resolves (the capability is off, so a
chess-game bubble would render nothing) or when the resolved dir lacks
the `chess-widget.js' the webapp imports.  No-ops in the sandbox, where
the daemon and its assets are a host concern.  Aggregated by `doctor.el'
alongside the install and codex checks."
  (if (agent-repl--in-sandbox-p)
      nil
    (let ((dir (agent-repl--frontend-discover-widget-assets-dir)))
      (cond
       ((null dir)
        (list (cons 'warn
                    (format (concat "chess-widget capability OFF: no widget-assets dir resolves"
                                    " — set agent-repl-frontend-widget-assets-dir or put a"
                                    " cee-web-widget/dist under %s, then"
                                    " M-x agent-repl-frontend-daemon-restart")
                            (or agent-repl-frontend-widget-assets-search-root
                                "your explanation-engine checkout")))))
       ((not (file-exists-p (expand-file-name "chess-widget.js" dir)))
        (list (cons 'warn
                    (format (concat "chess-widget dir %s lacks chess-widget.js"
                                    " — point agent-repl-frontend-widget-assets-dir at a real"
                                    " cee-web-widget/dist, then"
                                    " M-x agent-repl-frontend-daemon-restart")
                            dir))))
       (t nil)))))

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

(defun agent-repl--frontend-poll-until (predicate timeout interval)
  "Block until PREDICATE clears or TIMEOUT elapses; return PREDICATE's last value.
Calls PREDICATE (a nullary function), sleeping INTERVAL seconds between
polls, until PREDICATE returns nil or the deadline passes.  Returns the
LAST value of PREDICATE, so a caller branches on whether the condition
cleared (nil) or the wait timed out (non-nil) — e.g. \"process still live
after the grace window\" or \"socket still bound after shutdown\".

Uses `sleep-for', NOT `sit-for': sit-for returns immediately on pending
input, which would collapse the wait window while the user types (the
same rationale as the readiness poll in frontend-client.el)."
  (let ((deadline (+ (float-time) timeout)))
    (while (and (funcall predicate) (< (float-time) deadline))
      (sleep-for interval))
    (funcall predicate)))

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
\"session gone\" analyst the daemon dispatches from -remediation-dir.

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
   (when agent-repl-frontend-remediate-lost-sessions
     (append
      (list "-remediation-dir" agent-repl--frontend-repo-root)
      (when agent-repl-frontend-remediation-permission-mode
        (list "-remediation-permission-mode"
              agent-repl-frontend-remediation-permission-mode))))
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
  "Clear the tracked process when PROC dies; EVENT is the status change."
  (unless (process-live-p proc)
    (when (eq proc agent-repl--frontend-daemon-process)
      (setq agent-repl--frontend-daemon-process nil))
    (agent-repl--log nil "claude-repld exited: %s" (string-trim event))))

;;;; ---- Entry point ------------------------------------------------------

(defun agent-repl--frontend-daemon-responsive-p ()
  "Return non-nil when a daemon is listening on the frontend UDS socket.
Detects a daemon this Emacs does not track — one bounced into place by
an agent, or surviving from a previous Emacs — because the socket path is
a fixed, single-owner rendezvous just as the HTTP port was.  An
unreachable socket counts as absent.

Re-sourced from the deleted `GET /sessions' probe onto
`agent-repl--uds-socket-live-p' (S8/S9 sentinel endgame): Emacs speaks no
HTTP to the daemon any more."
  (agent-repl--uds-socket-live-p))

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
    (and disk running (> disk running))))

(defvar agent-repl--frontend-startup-staleness-checked nil
  "Non-nil once this Emacs session ran its one-shot startup staleness bounce.
The bounce is folded into the FIRST non-force
`agent-repl--ensure-frontend-daemon' — the point the daemon is first
needed (Emacs open, when a restored or opened panel ensures it).  Keeping
it a per-session one-shot preserves the cheap live-process hot path every
later ensure relies on, and keeps the module LAZY: a user who never opens
a panel never ensures, so the check (and its build) never runs for them.")

(defun agent-repl--frontend-request-foreign-shutdown ()
  "Ask the daemon on the frontend UDS to shut down (`ShutdownCmd').
The S9 replacement for the deleted `POST /shutdown' call: the graceful
teardown the daemon ran for that route is now reachable as a
`FrontendCommand' arm, so this needs no HTTP.  Dials first when the link
is down — a foreign daemon owns the SAME socket, which is exactly why this
reaches a daemon this Emacs never spawned.

Tracks the command so a rejected ack is surfaced loudly rather than read
as success.  Signals (`agent-repl--uds-send-command's loud `user-error')
when there is no connection to send on; the caller logs that and falls
through to the liveness poll, which is the real exit signal."
  (unless (agent-repl--uds-connected-p)
    (agent-repl-uds-connect))
  (let ((req (agent-repl--uds-send-command "shutdown" nil)))
    (agent-repl--uds-track-command req "shutdown" nil)
    req))

(defun agent-repl--frontend-bounce-foreign-daemon ()
  "Bounce an ADOPTED daemon this Emacs does not track, via `ShutdownCmd'.
Asks the foreign daemon to exit gracefully (it runs the same shutdown path
SIGTERM triggers), waits for its listener to free the socket, then starts a
fresh daemon from the already-rebuilt on-disk binary.

The shutdown command errors benignly if the daemon drops the connection as
it tears down, so a transport error on the send is logged and ignored —
the liveness poll is the real exit signal.  A daemon that never frees the
socket within `agent-repl-frontend-foreign-stop-grace-seconds' is LEFT in
place \(spawning next to it would only bind-fail), with a note to run the
manual restart, so a wedged foreign daemon never costs a doomed spawn."
  (condition-case err
      (agent-repl--frontend-request-foreign-shutdown)
    (error
     (agent-repl--log nil "startup: foreign daemon shutdown request errored (%s) — polling the socket anyway"
                       (error-message-string err))))
  (if (agent-repl--frontend-poll-until
       #'agent-repl--frontend-daemon-responsive-p
       agent-repl-frontend-foreign-stop-grace-seconds 0.1)
      (agent-repl--log nil "startup: adopted daemon on %s ignored shutdown within %ss — leaving it in place; run M-x agent-repl-frontend-daemon-restart"
                        agent-repl-frontend-daemon-addr
                        agent-repl-frontend-foreign-stop-grace-seconds)
    (agent-repl--frontend-start-daemon)))

(defun agent-repl--frontend-bounce-if-stale ()
  "Rebuild-if-stale, then bounce the running daemon when the binary is stale.
Runs a build-if-stale pass FIRST so changed Go source is compiled onto
disk before the comparison, then bounces (fresh restart) when the on-disk
binary is newer than the running daemon's launched binary
\(`agent-repl--frontend-daemon-stale-p').

Acts only when a daemon is already up to compare against: with none
running, the launch branch of `agent-repl--ensure-frontend-daemon'
builds-if-stale before starting, so freshness is already guaranteed there
without a redundant build here.

Once stale, the branch taken keeps the bounce safe:
- an in-flight turn DEFERS the bounce rather than killing a live
  conversation mid-generation — the same refusal
  `agent-repl--frontend-stop-daemon' enforces, checked up front so a stop
  is never even attempted mid-turn, and it gates the foreign path too;
- a daemon THIS Emacs tracks is stopped (graceful SIGTERM) and respawned;
- an ADOPTED foreign daemon, which has no local handle to signal, is asked
  to exit over the UDS and replaced once it frees the socket
  (`agent-repl--frontend-bounce-foreign-daemon');
- a fresh daemon is left exactly as adopt/reuse would leave it (no
  restart), which is the whole point of gating the bounce on staleness."
  (when (or (agent-repl--frontend-daemon-live-p)
            (agent-repl--frontend-daemon-responsive-p))
    (agent-repl--frontend-build-if-stale nil)
    (when (agent-repl--frontend-daemon-stale-p)
      (cond
       ((agent-repl--frontend-turn-active-sessions)
        (agent-repl--log nil "startup: daemon binary stale but a turn is in flight — deferring bounce (will not kill mid-turn)"))
       ((agent-repl--frontend-daemon-live-p)
        (agent-repl--log nil "startup: daemon binary stale — bouncing tracked daemon to pick up the new build")
        (agent-repl--frontend-stop-daemon)
        (agent-repl--frontend-start-daemon))
       (t
        (agent-repl--log nil "startup: adopted daemon on %s is stale — bouncing it via ShutdownCmd"
                          agent-repl-frontend-daemon-addr)
        (agent-repl--frontend-bounce-foreign-daemon))))))

(defun agent-repl--frontend-maybe-bounce-stale-daemon-once ()
  "Run `agent-repl--frontend-bounce-if-stale' at most once per Emacs session.
The guard is set BEFORE the bounce runs, so even a build error leaves the
check a genuine one-shot rather than re-firing (and re-building) on every
subsequent panel open; the error itself still propagates to the caller,
never swallowed."
  (unless agent-repl--frontend-startup-staleness-checked
    (setq agent-repl--frontend-startup-staleness-checked t)
    (agent-repl--frontend-bounce-if-stale)))

(defun agent-repl--ensure-frontend-daemon (&optional force)
  "Ensure the frontend daemon is built and running; return its process.
Idempotent: returns the live process immediately when one exists (unless
FORCE).  A daemon already answering on the port that this Emacs does
NOT track is ADOPTED (returns t): spawning next to it would only
bind-fail and die — the orphan-daemon failure mode.  Otherwise builds
any stale artifact and launches `claude-repld'.  FORCE skips adoption:
an explicit restart wants a fresh process (a foreign daemon cannot be
stopped from here — only its owner can).  Returns nil without acting
when `agent-repl-frontend-auto-start' is nil or automatic init is
inhibited (batch/sandbox).

On the FIRST non-force call per Emacs session (the daemon first being
needed on Emacs open), a one-shot staleness bounce runs before the
adopt/reuse decision: it rebuilds any stale artifact and, when the
running daemon launched from an older binary than the one now on disk,
restarts it so the session picks up the new build.  A fresh daemon is
left untouched (today's adopt/reuse), an in-flight turn defers the
bounce, and an adopted daemon this Emacs does not track is asked to exit
over the UDS and replaced (see `agent-repl--frontend-bounce-if-stale')."
  (cond
   ((not agent-repl-frontend-auto-start) nil)
   ((agent-repl--frontend-init-inhibited-p) nil)
   (t
    (unless force
      (agent-repl--frontend-maybe-bounce-stale-daemon-once))
    (cond
     ((and (not force) (agent-repl--frontend-daemon-live-p))
      agent-repl--frontend-daemon-process)
     ((and (not force) (agent-repl--frontend-daemon-responsive-p))
      (agent-repl--log nil "ensure-frontend-daemon: adopting foreign daemon on %s"
                        agent-repl-frontend-daemon-addr)
      t)
     (t
      (when (and force (agent-repl--frontend-daemon-live-p))
        (agent-repl--frontend-stop-daemon))
      (agent-repl--frontend-build-if-stale force)
      (agent-repl--frontend-start-daemon))))))

(defun agent-repl--frontend-stop-daemon (&optional force)
  "Stop the tracked `claude-repld' process, gracefully first.
Refuses while any daemon session reports an in-flight turn — stopping
then would kill a live conversation mid-generation (the repeated
daemon-bounce incidents) — unless FORCE is non-nil.  An unreachable
daemon has nothing to protect, so the turn probe treats it as idle.

The stop itself signals SIGTERM so the daemon runs its shutdown path
\(draining its sessions and flushing the session registry), waits up to
`agent-repl-frontend-stop-grace-seconds' for the process to exit, and
only then falls back to `delete-process' (SIGKILL).  The old
delete-process-first behavior SIGKILLed the daemon on every routine
restart, which is exactly the restart class the session registry exists
to survive — the registry makes that survivable either way, and the
graceful window additionally lets shims exit cleanly instead of by
inherited-pipe EOF."
  (when (agent-repl--frontend-daemon-live-p)
    (unless force
      (when-let ((busy (agent-repl--frontend-turn-active-sessions)))
        (error "agent-repl: refusing daemon stop — turn in flight in %s; retry when idle or pass FORCE"
               busy)))
    (let ((proc agent-repl--frontend-daemon-process))
      (signal-process proc 'TERM)
      (when (agent-repl--frontend-poll-until
             (lambda () (process-live-p proc))
             agent-repl-frontend-stop-grace-seconds 0.05)
        (agent-repl--log nil "claude-repld ignored SIGTERM for %ss; falling back to delete-process"
                         agent-repl-frontend-stop-grace-seconds)
        (delete-process proc))))
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
        ;; PROC is t for an adopted foreign daemon (no process object).
        (message "claude-repld running (pid %s) on %s"
                 (if (processp proc) (process-id proc) "foreign/adopted")
                 agent-repl-frontend-daemon-addr)))))

;;;###autoload
(defun agent-repl-frontend-daemon-stop (&optional force)
  "Stop the running frontend daemon.
Refuses while a turn is in flight; with prefix arg FORCE, stops anyway."
  (interactive "P")
  (agent-repl--frontend-stop-daemon (and force t))
  (message "claude-repld stopped."))

;;;###autoload
(defun agent-repl-frontend-daemon-restart ()
  "Rebuild any stale artifact, restart the frontend daemon, and rebind panels.
After the daemon is force-bounced, every OPEN workspace is immediately
rebound to the new instance: its shim is bounced (a fresh daemon session
resuming the durable conversation) and its webview is remounted, so each
panel is good to go the moment this returns rather than after the next
reattach sweep."
  (interactive)
  (let ((agent-repl-frontend-auto-start t))
    (cl-letf (((symbol-function 'agent-repl--frontend-init-inhibited-p)
               (lambda () nil)))
      (agent-repl--ensure-frontend-daemon t)
      (let ((n (agent-repl--frontend-rebind-workspaces-after-restart)))
        (message "claude-repld restarted on %s; rebound %d open workspace%s"
                 agent-repl-frontend-daemon-addr n (if (= n 1) "" "s"))))))

(provide 'daemon)

;;; daemon.el ends here
