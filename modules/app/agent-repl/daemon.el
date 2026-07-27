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
(declare-function agent-repl--log-verbose "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--error "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--doctor-log "agent-repl-doctor" (fmt &rest args))
(declare-function agent-repl--in-sandbox-p "agent-repl-install" ())
(declare-function agent-repl--frontend-turn-active-sessions "agent-repl-frontend-client" ())
(declare-function agent-repl-runtime-restart "services" ())
(declare-function agent-repl--uds-socket-live-p "frontend-uds" (&optional path))
(declare-function agent-repl--uds-connected-p "frontend-uds" ())
(declare-function agent-repl-uds-connect "frontend-uds" (&optional path readiness-p))
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
Set to nil to hand the analyst no --permission-mode at all.

An ungated value here is inert on its own: the daemon REFUSES TO BOOT
under one unless `agent-repl-frontend-remediation-allow-ungated' also
consents.  See that variable for the tradeoff being consented to."
  :type '(choice (const :tag "Ungated (can actually remediate)" "bypassPermissions")
                 (const :tag "Edits only" "acceptEdits")
                 (const :tag "CLI default (report-only)" nil)
                 string)
  :group 'agent-repl)

(defcustom agent-repl-frontend-remediation-allow-ungated t
  "Consent to running the lost-session analyst with NO permission gate.
Becomes the daemon's `-allow-ungated-remediation', which it REQUIRES
whenever `agent-repl-frontend-remediation-permission-mode' is ungated
\(`bypassPermissions').  Without it such a configuration makes
`claude-repld' refuse to boot rather than run ungated by default.

THE TRADEOFF THIS ACKNOWLEDGES.  The analyst is a headless `claude -p'
with nobody at the keyboard, so under any GATED mode every tool call it
makes is auto-denied and it can only narrate a diagnosis: the feature
does nothing.  Ungated is therefore the only mode in which it works, and
what that costs is a `claude' running unattended against
`agent-repl--frontend-repo-root' -- your real doom checkout -- approving
its own tool calls, triggered by a lost session rather than by you.

Defaulting to t is that tradeoff accepted deliberately, not by omission.
Set to nil to withdraw the consent, and then also set
`agent-repl-frontend-remediation-permission-mode' to a gated mode (or
nil), or the daemon will refuse to start.  Every ungated analyst -- both
its configuration at daemon boot and each dispatch -- is recorded in the
daemon log, which is where an analyst with no session UI is visible."
  :type 'boolean
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
the `chess-widget.js' the webapp imports.  No-ops in the sandbox, where
the daemon and its assets are a host concern.  Aggregated by `doctor.el'
alongside the install and codex checks."
  (if (agent-repl--in-sandbox-p)
      (progn
        (agent-repl--widget-doctor-log
         "widget doctor: sandbox=t; skipping host asset checks")
        nil)
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
        nil)))))

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
  (agent-repl--log-verbose nil
                            "frontend poll: begin predicate=%S timeout=%s interval=%s"
                            predicate timeout interval)
  (let ((deadline (+ (float-time) timeout)))
    (while (and (funcall predicate) (< (float-time) deadline))
      (sleep-for interval))
    (let ((remaining (funcall predicate)))
      (agent-repl--log-verbose nil
                                "frontend poll: complete predicate=%S remaining=%S timeout=%s interval=%s"
                                predicate remaining timeout interval)
      remaining)))

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

(defun agent-repl--frontend-build-targets-if-stale (targets &optional force)
  "Build stale TARGETS through the shared artifact orchestrator.
TARGETS is a list of build-frontend target strings, or nil for its normal
shim/webapp/daemon set.  With FORCE non-nil, every selected artifact is
rebuilt.  The complete captured subprocess output is copied into the
persistent agent-repl log before success or failure is decided."
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
  (let* ((args (append (list agent-repl--frontend-build-script)
                       (when force '("--force"))
                       targets))
         (exit-code (agent-repl--frontend-run-build-script args))
         (output
          (with-current-buffer agent-repl--frontend-build-buffer
            (string-trim-right (buffer-string)))))
    (agent-repl--log nil
                     "frontend build-if-stale targets=%S force=%s exit=%S output=%s"
                     (or targets 'default) (if force "t" "nil") exit-code
                     (if (string-empty-p output) "<empty>" output))
    (unless (eq exit-code 0)
      (display-buffer agent-repl--frontend-build-buffer)
      (error "agent-repl: frontend build failed (exit %s) — see %s"
             exit-code agent-repl--frontend-build-buffer))
    exit-code))

(defun agent-repl--frontend-build-if-stale (&optional force)
  "Build stale shim, webapp, and daemon artifacts.
With FORCE non-nil, rebuild all three.  Signals loudly on failure."
  (agent-repl--frontend-build-targets-if-stale nil force))

;;;; ---- Launch -----------------------------------------------------------

(defun agent-repl--frontend-daemon-command ()
  "Return the argv list used to launch `claude-repld'.
When the system `claude' binary resolves, it is handed to the daemon
via -claude-bin so SDK sessions drive the SAME CLI version as vterm
sessions.  The user upgrades `claude' independently of the shim's
lockfile, so the system binary can lead the SDK's bundled one; since
SDK 0.2.113 that bundle is a per-platform NATIVE Claude Code binary
(0.3.220 ships 2.1.220), not the old JS `cli.js'.  The same binary
drives the headless \"session gone\" analyst the daemon dispatches
from -remediation-dir.

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
              agent-repl-frontend-remediation-permission-mode))
      ;; The standing consent, sent whenever it is held rather than only
      ;; when the configured mode happens to need it.  The daemon owns the
      ;; question of WHICH modes are ungated (`protocol.UngatedPermissionMode'),
      ;; so re-deciding it here would only add a second copy of that list to
      ;; drift out of sync -- and a drifted copy would withhold the consent
      ;; and stop the daemon booting.
      (when agent-repl-frontend-remediation-allow-ungated
        (list "-allow-ungated-remediation"))))
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
    (let ((stale (and disk running (> disk running))))
      (agent-repl--log nil
                       "frontend daemon staleness: disk-mtime=%S running-mtime=%S stale=%s"
                       disk running (if stale "t" "nil"))
      stale)))

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
  (let ((connected (agent-repl--uds-connected-p)))
    (agent-repl--log nil "foreign daemon shutdown: uds-connected=%s addr=%s"
                     connected agent-repl-frontend-daemon-addr)
    (unless connected
    (agent-repl-uds-connect))
    (let ((req (agent-repl--uds-send-command "shutdown" nil)))
      (agent-repl--uds-track-command req "shutdown" nil)
      (agent-repl--log nil "foreign daemon shutdown: command accepted request-id=%S" req)
      req)))

(defun agent-repl--frontend-bounce-foreign-daemon ()
  "Bounce an ADOPTED daemon this Emacs does not track, via `ShutdownCmd'.
Asks the foreign daemon to exit gracefully (it runs the same shutdown path
SIGTERM triggers), waits for its listener to free the socket, then starts a
fresh daemon from the already-rebuilt on-disk binary.

The shutdown command errors benignly if the daemon drops the connection as
it tears down, so a transport error on the send is logged and ignored —
the liveness poll is the real exit signal.  A daemon that never frees the
socket within `agent-repl-frontend-foreign-stop-grace-seconds' fails loudly
and is left in place; spawning next to it would only bind-fail."
  (agent-repl--log nil
                   "foreign daemon bounce: begin addr=%s grace-seconds=%s"
                   agent-repl-frontend-daemon-addr
                   agent-repl-frontend-foreign-stop-grace-seconds)
  (condition-case err
      (agent-repl--frontend-request-foreign-shutdown)
    (error
     (agent-repl--log nil "startup: foreign daemon shutdown request errored (%s) — polling the socket anyway"
                       (error-message-string err))))
  (let ((still-responsive
         (agent-repl--frontend-poll-until
          #'agent-repl--frontend-daemon-responsive-p
          agent-repl-frontend-foreign-stop-grace-seconds 0.1)))
    (agent-repl--log nil
                     "foreign daemon bounce: shutdown wait complete still-responsive=%S addr=%s"
                     still-responsive agent-repl-frontend-daemon-addr)
    (if still-responsive
      (agent-repl--error nil
                         "adopted daemon on %s ignored shutdown within %ss; replacement aborted"
                         agent-repl-frontend-daemon-addr
                         agent-repl-frontend-foreign-stop-grace-seconds)
      (agent-repl--frontend-start-daemon))))

(defun agent-repl--frontend-runtime-bounce-preflight ()
  "Resolve and validate the daemon state before any runtime mutation.
Returns `:tracked', `:responsive', `:absent', or
`(:incompatible PID . COMMAND)'.  A listener that is neither responsive
nor the verified claude-repld binary fails loudly before builds or service
bounces can change anything."
  (let* ((tracked (agent-repl--frontend-daemon-live-p))
         (responsive (agent-repl--frontend-daemon-responsive-p))
         (owner (and (not tracked) (not responsive)
                     (agent-repl--frontend-listener-owner)))
         (state
          (cond
           (tracked :tracked)
           (responsive :responsive)
           ((and owner
                 (agent-repl--frontend-our-daemon-command-p (cdr owner)))
            (cons :incompatible owner))
           (owner
            (agent-repl--error nil
                               "daemon address %s is held by unrelated process pid=%s command=%s"
                               agent-repl-frontend-daemon-addr
                               (car owner) (cdr owner)))
           (t :absent))))
    (agent-repl--log nil
                     "runtime-bounce preflight: state=%S addr=%s owner=%S"
                     state agent-repl-frontend-daemon-addr owner)
    state))

(defun agent-repl--frontend-bounce-after-build (&optional preflight)
  "Bounce the current daemon after all required artifacts are built.
Starts a daemon when none exists, gracefully replaces a tracked or adopted
daemon, and handles an incompatible pre-UDS generation by terminating only
the verified claude-repld listener.  The caller must reject active turns
before invoking this state-changing operation.  PREFLIGHT is the validated
result of `agent-repl--frontend-runtime-bounce-preflight'; when omitted,
this function resolves it before acting."
  (let ((state (or preflight
                   (agent-repl--frontend-runtime-bounce-preflight))))
    (agent-repl--log nil
                     "runtime-bounce: applying preflight=%S addr=%s"
                     state agent-repl-frontend-daemon-addr)
    (cond
     ((eq state :tracked)
      (agent-repl--frontend-stop-daemon)
      (agent-repl--frontend-start-daemon))
     ((eq state :responsive)
      (agent-repl--frontend-bounce-foreign-daemon))
     ((and (consp state) (eq (car state) :incompatible))
      (unless (agent-repl--frontend-terminate-incompatible-daemon
               (cadr state))
        (agent-repl--error nil
                           "incompatible daemon pid=%s survived termination; replacement aborted"
                           (cadr state)))
      (agent-repl--frontend-start-daemon))
     ((eq state :absent)
      (agent-repl--frontend-start-daemon))
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

(defun agent-repl--frontend-incompatible-daemon ()
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

Returns nil in every other case, including when the port is held by a
foreign program — that is reported by the caller, not acted on."
  (let ((responsive (agent-repl--frontend-daemon-responsive-p)))
    (if responsive
        (progn
          (agent-repl--log nil
                           "incompatible daemon check: addr=%s responsive=t; no replacement needed"
                           agent-repl-frontend-daemon-addr)
          nil)
      (let ((owner (agent-repl--frontend-listener-owner)))
        (cond
         ((null owner)
          (agent-repl--log nil
                           "incompatible daemon check: addr=%s responsive=nil owner=nil"
                           agent-repl-frontend-daemon-addr)
          nil)
         ((agent-repl--frontend-our-daemon-command-p (cdr owner))
          (agent-repl--log nil
                           "incompatible daemon check: addr=%s responsive=nil owner=%S ours=t"
                           agent-repl-frontend-daemon-addr owner)
          owner)
         (t
          (agent-repl--log nil
                           "startup: %s is held by pid %s (%s), which is NOT our daemon — leaving it alone"
                           agent-repl-frontend-daemon-addr (car owner) (cdr owner))
          nil))))))

(defun agent-repl--frontend-terminate-incompatible-daemon (pid)
  "Terminate the incompatible daemon PID, gracefully first.
SIGTERM so it runs its own shutdown path, then SIGKILL only if it
outlives `agent-repl-frontend-incompatible-stop-grace-seconds'.  Returns
non-nil when the port is free afterwards.

There is no in-flight-turn guard here, unlike the ordinary stop: this
daemon serves no UDS, so its turns are already unreachable from this
Emacs — there is no live conversation to protect, only a process holding
a port nothing can use."
  (agent-repl--log nil "startup: terminating incompatible daemon pid=%s on %s"
                   pid agent-repl-frontend-daemon-addr)
  (agent-repl--signal-process pid 'TERM)
  (let ((survived-term
         (agent-repl--frontend-poll-until
          #'agent-repl--frontend-listener-owner
          agent-repl-frontend-incompatible-stop-grace-seconds 0.1)))
    (agent-repl--log nil
                     "incompatible daemon termination: pid=%s TERM-survived=%S"
                     pid survived-term)
    (when survived-term
      (agent-repl--log nil "startup: incompatible daemon pid=%s ignored SIGTERM for %ss; sending KILL"
                       pid agent-repl-frontend-incompatible-stop-grace-seconds)
      (agent-repl--signal-process pid 'KILL)
      (let ((survived-kill
             (agent-repl--frontend-poll-until
              #'agent-repl--frontend-listener-owner
              agent-repl-frontend-incompatible-stop-grace-seconds 0.1)))
        (agent-repl--log nil
                         "incompatible daemon termination: pid=%s KILL-survived=%S"
                         pid survived-kill))))
  (let ((stopped (not (agent-repl--frontend-listener-owner))))
    (agent-repl--log nil "incompatible daemon termination: pid=%s stopped=%s"
                     pid stopped)
    stopped))

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
inhibited (batch/sandbox).  The post-snapshot startup coordinator in
services.el owns the once-per-Emacs full-runtime bounce; this function
remains the cheap idempotent session-open ensure."
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
     ((and (not force) (agent-repl--frontend-daemon-responsive-p))
      (agent-repl--log nil "ensure-frontend-daemon: adopting foreign daemon on %s"
                        agent-repl-frontend-daemon-addr)
      t)
     (t
      (when (and force (agent-repl--frontend-daemon-live-p))
        (agent-repl--log nil "ensure-frontend-daemon: force=t; stopping tracked daemon before rebuild")
        (agent-repl--frontend-stop-daemon))
      (agent-repl--log nil "ensure-frontend-daemon: build-and-launch force=%s"
                       (if force "t" "nil"))
      (agent-repl--frontend-build-if-stale force)
      (agent-repl--frontend-start-daemon)))))))

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
  (let ((live (agent-repl--frontend-daemon-live-p)))
    (agent-repl--log nil "frontend stop: requested force=%s tracked-live=%s"
                     (if force "t" "nil") live)
    (if (not live)
        (agent-repl--log nil "frontend stop: no tracked live daemon; clearing process slot")
      (unless force
        (when-let ((busy (agent-repl--frontend-turn-active-sessions)))
          (agent-repl--log nil "frontend stop: refused force=nil active-sessions=%S" busy)
          (error "agent-repl: refusing daemon stop — turn in flight in %s; retry when idle or pass FORCE"
                 busy)))
    (let ((proc agent-repl--frontend-daemon-process))
      (agent-repl--log nil "frontend stop: sending TERM pid=%S grace-seconds=%s"
                       (process-id proc) agent-repl-frontend-stop-grace-seconds)
      (signal-process proc 'TERM)
      (if (agent-repl--frontend-poll-until
           (lambda () (process-live-p proc))
           agent-repl-frontend-stop-grace-seconds 0.05)
          (progn
            (agent-repl--log nil "claude-repld ignored SIGTERM for %ss; falling back to delete-process"
                             agent-repl-frontend-stop-grace-seconds)
            (delete-process proc)
            (agent-repl--log nil "frontend stop: delete-process issued pid=%S" (process-id proc)))
        (agent-repl--log nil "frontend stop: graceful exit observed pid=%S" (process-id proc)))))
    (setq agent-repl--frontend-daemon-process nil)
    (agent-repl--log nil "frontend stop: complete tracked-process-cleared=t")))

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
  "Restart the complete runtime through the canonical coordinator.
This compatibility command now includes stale builds plus launchd-managed
store/sidecar bounces; restarting the daemon also replaces every session
shim and immediately rebinds open panels."
  (interactive)
  (agent-repl-runtime-restart))

(provide 'daemon)

;;; daemon.el ends here
