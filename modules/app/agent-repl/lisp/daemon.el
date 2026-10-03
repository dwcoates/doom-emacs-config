;;; daemon.el --- Emacs owns the daemon's COLD START, and only that -*- lexical-binding: t; -*-

;;; Commentary:

;; EMACS OWNS COLD START.  Auto-start on boot, stale-binary rebuild, the
;; foreign-daemon question, and surfacing a build failure.  EVERYTHING
;; AFTER BOOT is the daemon's own blue-green rollout, which daemon-link.el
;; rides as a client — this file has no restart machinery, no readiness
;; probing, no expected-restart bookkeeping and no process supervision
;; beyond the one spawn.
;;
;; THE COLD-START DECISION, in one sentence: a daemon that ANSWERS is
;; adopted and never killed.
;;
;;   daemon.addr absent            → build, start, wait for the file
;;   daemon.addr names a dead pid  → advertiser dead: retire it, build (no dial)
;;   daemon.addr present, answers  → ADOPT it, healthy or unhealthy alike
;;   daemon.addr present, silent   → a stale file; treat it as absent
;;
;; The dead-pid arm is what a legacy, pid-less advertisement cannot take: it
;; falls through to the dial, and a predecessor that died without withdrawing
;; then costs a refused connection and its transport records before the file
;; is retired.  An advertisement that carries a live-checkable pid is retired
;; before any dial.
;;
;; UNHEALTHY IS AN ANSWER.  `DaemonHealth' returns a typed verdict, never
;; a transport error, so an unhealthy daemon is a daemon — it is adopted
;; and its faults are surfaced, because starting a second daemon beside a
;; live one is strictly worse than running the one that is there.  Only a
;; TRANSPORT failure proves no daemon is listening.
;;
;; THE ARGV CARRIES THE ACCOUNT ROOTS, AND NOTHING ELSE.  The daemon
;; DETERMINES a workspace's vendor config dir from its path — the main
;; repo under `$MULTI_REPO_ROOT' gets the multi-repo root, everything else
;; the default — and its account resolver REFUSES TO BUILD without both
;; roots, so a daemon spawned without `--default-config-dir' and
;; `--multi-repo-config-dir' exits 2 before it ever serves.  Emacs states
;; both on the spawn, and refuses the launch loudly when either is empty
;; rather than letting the child die with only a log line.  `MULTI_REPO_ROOT'
;; is read by the daemon from the ENVIRONMENT, not from a flag, so it is
;; exported the same explicit way the state root is.  Every other path the
;; daemon needs (shim, webapp dist, prompts, vocab) it derives from its own
;; checkout, so none of them belongs here.
;;
;; The state root travels in `AGENT_REPL_STATE_DIR', which is exported
;; EXPLICITLY on the spawn so the child cannot inherit a different root than
;; the one Emacs resolved.  The listen address is the daemon's own choice,
;; published by writing `daemon.addr' — which is also the readiness signal
;; this file waits on, and a daemon that EXITS before publishing it ends the
;; wait immediately with the tail of its own run log.
;;
;; THE DAEMON IS A RESIDENT SERVICE, AND IT OUTLIVES THIS EMACS.  A
;; daemon spawned as a plain `make-process' child does not: Emacs's own
;; shutdown walks its process list and sends each child a SIGHUP
;; (`kill_buffer_processes'), so quitting the editor took the daemon with
;; it — and with it the whole adopt-on-restart path this file's triage is
;; written around.  Restarting Emacs always respawned, never adopted.
;;
;; So the spawn goes through a DETACHER, `agent-repl-daemon--spawn-argv':
;; a `/bin/sh' that sets SIGHUP to ignore, redirects the child's stdout
;; and stderr to `logs/daemon.stdio.log', and then EXECS the daemon.  Two
;; deaths are closed by that, and nothing else changes:
;;
;;   - the exit-time SIGHUP is ignored, because a disposition of SIG_IGN
;;     survives `exec' and Go leaves an initially-ignored signal ignored
;;     (the daemon asks for SIGINT/SIGTERM/SIGQUIT and no others);
;;   - the child's stdout no longer names a pipe whose read end dies with
;;     Emacs, so the first log line written after Emacs exits cannot take
;;     the daemon down with SIGPIPE.
;;
;; It is an `exec', so the pid Emacs holds IS the daemon's: the sentinel,
;; the boot wait's "it exited before publishing an address" branch and
;; `agent-repl-daemon--spawned-here-p' all keep working on the real
;; process.  Emacs's own children are already `setsid'-ed by
;; `make-process', so the daemon is in its own session either way.
;;
;; STOPPING IT IS STILL EMACS'S TO DO, and it is unchanged: the stop verb
;; is `UpdateShutdownSchedule{now}' over the link
;; (`agent-repl-frontend-daemon-stop'), which asks the daemon to flush and
;; exit itself, and the restart verb sequences that stop ahead of a fresh
;; ensure.  Neither ever signalled the process object, so neither loses
;; anything to the detach.  Only the IMPLICIT death-on-exit is gone.
;;
;; NO SLEEPS AND NO BLOCKING.  The boot wait is a timer poll whose tick is
;; a named function, and the build is an ASYNCHRONOUS `make-process' whose
;; sentinel carries the continuation.  Nothing in this file blocks the
;; main thread, because the frame must paint before any of it runs:
;; `agent-repl-daemon-schedule-ensure' defers the whole cold start onto an
;; idle timer, so the command loop reaches its first redisplay first.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl-popup-show "popup" (buffer))
(declare-function agent-repl--with-deferred-quit "core")
(declare-function agent-repl--deferred-quit-arm-audit "core" (context))
(declare-function agent-repl--deferred-quit-hand-off "core" (context))
(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--warn "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function agent-repl--fatal "core" (ws fmt &rest args))
(declare-function agent-repl--backend-phase "core" (ws fmt &rest args))
(declare-function agent-repl--phase-echo "core" (ws fmt &rest args))
(declare-function agent-repl--logfile-path "core" ())
(declare-function agent-repl--global-state-dir "core" ())
(declare-function agent-repl--global-state-file "core" (relative))

(declare-function agent-repl-connect-open "connect" (address))
(declare-function agent-repl-connect-close "connect" (conn))
(declare-function agent-repl-connect-read-daemon-addr "connect" ())
(declare-function agent-repl-connect-read-daemon-addr-pid "connect" ())
(declare-function agent-repl-connect-daemon-addr-file "connect" ())
(declare-function agent-repl-rpc-daemon-health "rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-update-shutdown-schedule "rpc" (conn request &rest keys))

(defvar agent-repl-roster-bringup-functions)

(declare-function agent-repl-link-connect "daemon-link" ())
(declare-function agent-repl-link-primary "daemon-link" ())
(declare-function agent-repl-link-up-p "daemon-link" ())
(declare-function agent-repl-link-successor "daemon-link" ())
(declare-function agent-repl-link-successor-pending-p "daemon-link" ())
(declare-function agent-repl-link-teardown "daemon-link" ())

;;;; ---- Paths ----

(defconst agent-repl--frontend-root
  (let ((module-dir (file-name-directory (or load-file-name
                                             buffer-file-name
                                             default-directory))))
    (file-name-as-directory (expand-file-name ".." module-dir)))
  "Absolute path to the `modules/app/agent-repl/' directory.
This file lives in `lisp/', one level below that root, hence the `..'.")

(defconst agent-repl--frontend-webapp-dir
  (expand-file-name "webapp/dist" agent-repl--frontend-root)
  "The built webapp's static directory.
The daemon serves these assets itself, on its own one origin; this
constant exists for the build-artifact identity frontend.el reads, not
for any flag — no webapp path rides the daemon's argv any more.")

(defconst agent-repl--frontend-daemon-buffer "*claude-repld*"
  "Buffer the detacher's own stdout and stderr are captured into.

THE DAEMON\='S OWN OUTPUT DOES NOT LAND HERE ANY MORE, and it must not: a
buffer is an Emacs pipe, and a pipe whose read end dies with Emacs is a
SIGPIPE waiting for the daemon\='s next line.  The detacher redirects the
exec\='d daemon to `agent-repl-daemon--stdio-log-path\=' instead, which is a
file that outlives the editor and is read back by
`agent-repl--frontend-stdio-log-tail\='.  What still arrives here is the
detacher\='s own failure to get that far — an unmakeable log directory, an
unexecutable binary.")

(defconst agent-repl-daemon-build-buffer "*agent-repl-build-frontend*"
  "Capture buffer for the build script; SHOWN to the user on a failure.")

(defun agent-repl-daemon--stdio-log-path ()
  "Absolute path the detached daemon\='s stdout and stderr are appended to.

Under the state root\='s `logs/', beside the daemon\='s own run log and
NEVER the same file: the run log is contract JSONL that readers parse a
line at a time, and raw stderr interleaved into it would break every one
of them."
  (agent-repl--global-state-file "logs/daemon.stdio.log"))

;;;; ---- Customization ----

(defcustom agent-repl-frontend-auto-start t
  "When non-nil, Emacs ensures a daemon at startup.
Set to nil to require `agent-repl-frontend-daemon-ensure' by hand."
  :type 'boolean
  :group 'agent-repl)

(defcustom agent-repl-daemon-build-script
  (expand-file-name "bin/build-frontend.sh" agent-repl--frontend-root)
  "The build-if-stale orchestrator run before a daemon is started.
It decides staleness itself; this file never second-guesses it."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-daemon-build-shell "bash"
  "Shell interpreter used to invoke `agent-repl-daemon-build-script'."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-daemon-build-status-display-seconds 4.0
  "Seconds the \"stack built in N.Ns\" mode-line status stays up.
A completed build is worth SAYING and not worth keeping: the segment
clears itself after this many seconds so the mode line goes back to
whatever else lives there."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-daemon-startup-idle-seconds 0
  "Idle seconds `agent-repl-daemon-schedule-ensure' waits before the ensure.
Zero is not \"immediately\": an idle timer runs only once the command loop
has gone idle, which is AFTER the first redisplay.  That is the whole
point — the frame paints before the stack build is even considered."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-daemon-command
  (list (expand-file-name "daemon/bin/claude-repld" agent-repl--frontend-root))
  "The BASE argv used to start the daemon: the binary and any overrides.
The two account-root flags the daemon requires are appended by
`agent-repl-daemon--argv\='; the state root travels in
`AGENT_REPL_STATE_DIR\=' and the daemon picks and publishes its own listen
address."
  :type '(repeat string)
  :group 'agent-repl)

(defcustom agent-repl-daemon-default-config-dir
  (expand-file-name (or (getenv "CLAUDE_CONFIG_DIR") "~/.claude"))
  "Vendor config root for every workspace OUTSIDE the multi-repo root.
Passed as `--default-config-dir\='.  REQUIRED BY THE DAEMON: its account
resolver refuses to build without it (\"Roots.Default is required\") and the
process exits 2 before it serves, so an empty value is refused HERE.
`CLAUDE_CONFIG_DIR\=' seeds the default because that is where this Emacs\='
own vendor config lives; the DAEMON never reads that variable — the flag is
the only channel."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-daemon-multi-repo-config-dir
  (expand-file-name "~/.claude-chesscom")
  "Vendor config root for workspaces UNDER `agent-repl-daemon-multi-repo-root\='.
Passed as `--multi-repo-config-dir\='.  REQUIRED BY THE DAEMON for the same
reason as `agent-repl-daemon-default-config-dir\=': the account is
DETERMINED by the workspace path, so both roots must have an answer."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-daemon-multi-repo-root
  (let ((root (getenv "MULTI_REPO_ROOT")))
    (and (stringp root) (not (string-empty-p root)) (expand-file-name root)))
  "The tree whose workspaces route to the multi-repo account, or nil.
Exported as `MULTI_REPO_ROOT\=' on the spawn, because the daemon reads it
from the ENVIRONMENT rather than from a flag.  Nil leaves the variable to
whatever the environment already carries, which the daemon reads as \"no
multi-repo root\": every workspace then routes to the default config dir.
It is NOT required — path-under-the-root is the only account rule, and a
root that was never named can contain nothing."
  :type '(choice (const :tag "None" nil) string)
  :group 'agent-repl)

(defcustom agent-repl-daemon-boot-timeout-seconds 30.0
  "Seconds to wait for a started daemon to publish `daemon.addr'.
A daemon that has not written its address by then has not booted; the
wait fails loudly rather than leaving Emacs pointed at nothing."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-daemon-boot-poll-interval-seconds 0.25
  "Seconds between polls of `daemon.addr' while a started daemon boots."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-daemon-health-timeout-seconds 3.0
  "Seconds the cold-start `DaemonHealth' probe waits for an answer.
Short by design: this call only decides whether ANYONE is listening, and
a daemon that cannot answer it in seconds is indistinguishable from an
address file left behind by a dead one."
  :type 'number
  :group 'agent-repl)

;;;; ---- State ----

(defvar agent-repl--frontend-daemon-process nil
  "The daemon process THIS Emacs spawned, or nil.
Nil is the ordinary state when the daemon was adopted: Emacs supervises
only what it started, and never kills what it did not.")

(defvar agent-repl-daemon--boot-process nil
  "The process whose boot the address wait is currently watching, or nil.
Kept apart from `agent-repl--frontend-daemon-process\=', which the sentinel
clears the moment the child dies: the boot wait needs the DEAD process in
hand to report why it never published an address.")

(defvar agent-repl-daemon-launch-failure nil
  "Detail of a refused or failed daemon LAUNCH, or nil.
Set when a required account root is empty, and when a spawned daemon exits
before it publishes `daemon.addr\='.  Drawn as
`agent-repl-daemon-mode-line-segment\=' and cleared by the next launch that
gets as far as a spawn.")

(defvar agent-repl-daemon-build-failure nil
  "The last build failure's detail string, or nil.
Drawn as `agent-repl-daemon-mode-line-segment' and cleared by the next
successful build.  There is NO automatic retry — the interactive
`agent-repl-frontend-daemon-ensure' is the retry.")

(defvar agent-repl-daemon-mode-line-segment nil
  "The `global-mode-string' segment reporting the stack build, or nil.
Three states, in precedence order: a standing failure, a build that is
running right now, and a build that just finished (shown for
`agent-repl-daemon-build-status-display-seconds' and then cleared).")
(put 'agent-repl-daemon-mode-line-segment 'risky-local-variable t)

(defvar agent-repl-daemon--build-state nil
  "`building', `built', or nil — what the status segment is reporting.")

(defvar agent-repl-daemon--lifecycle nil
  "Where the daemon bring-up has got to, or nil when nothing is in flight.

`starting' from the spawn until an address is published, `linking' from
the address until the primary link stands, then `adopted' or `ready' for
a moment before it clears.

THE STARTUP COST IS NOT ALLOWED TO BE INVISIBLE.  The segment used to
report the BUILD and nothing else, so everything between the spawn and
link-up -- on a real cold start ten seconds of it, spent waiting out a
stale address -- left the mode line saying nothing at all, which reads
exactly like an editor that has finished starting.")

(defvar agent-repl-daemon--lifecycle-timer nil
  "Timer that clears a settled `adopted'/`ready' lifecycle note, or nil.")

(defconst agent-repl-daemon--lifecycle-labels
  '((starting . "daemon: starting\u2026")
    (linking  . "daemon: linking\u2026")
    (adopted  . "daemon: adopted")
    (ready    . "daemon: ready"))
  "The mode-line text for each `agent-repl-daemon--lifecycle' state.")

(defconst agent-repl-daemon--lifecycle-echo-lines
  '((starting . "starting the daemon\u2026")
    (linking  . "linking to the daemon\u2026")
    (adopted  . "daemon adopted")
    (ready    . "daemon ready"))
  "The MINIBUFFER line each `agent-repl-daemon--lifecycle' state echoes.

THE MODE LINE IS NOT THE ONLY PLACE STARTUP IS REPORTED.  The segment
above says where bring-up has got to, but it says it in a corner of the
frame a user watching a cold start is not necessarily looking at, and it
says nothing at all once the note clears.  So every transition also
echoes exactly one short line, from `agent-repl-daemon--set-lifecycle' --
the single chokepoint the state moves through, which is what makes
\"exactly one echo per transition\" structural rather than a convention
each caller has to remember.")

(defconst agent-repl-daemon--lifecycle-settled-states '(adopted ready)
  "Lifecycle states that are an OUTCOME, shown briefly and then cleared.")

(defvar agent-repl-daemon--build-duration nil
  "Wall seconds the last successful build took, or nil.")

(defvar agent-repl-daemon--build-status-timer nil
  "Timer that clears the post-build status segment, or nil.")

(defvar agent-repl-daemon--build-in-flight nil
  "Non-nil while a build subprocess is running.
This is the coalescing guard: a second build request while one is under
way joins that build rather than starting a rival one.")

(defvar agent-repl-daemon--build-process nil
  "The running build process, or nil.  Kept for introspection only.")

(defvar agent-repl-daemon--build-continuations nil
  "Continuations waiting on the running build, oldest first.
Each is called with nil on success, or the failure detail string.")

(defvar agent-repl-daemon--build-started nil
  "`float-time' the running build started at, or nil.")

(defvar agent-repl-daemon--build-labels nil
  "`(RUNNING-LABEL . DONE-LABEL)' naming what the running build covers.")

(defvar agent-repl-daemon--build-target-names nil
  "The TARGETS argument the running build was asked for, for the log.")

(defvar agent-repl-daemon--startup-timer nil
  "The idle timer the startup hook scheduled the ensure on, or nil.")

(defvar agent-repl-daemon--boot-timer nil
  "The pending `daemon.addr' boot poll timer, or nil.")

(defvar agent-repl-daemon--boot-deadline nil
  "Epoch seconds after which the boot wait gives up, or nil.")

(defvar agent-repl-daemon--boot-continuation nil
  "The continuation the current boot wait will call, or nil.")

(defvar agent-repl-daemon--boot-rejected-address nil
  "An address this ensure ALREADY judged stale, which the boot wait refuses.
The file's appearance is the readiness signal, so a boot wait that starts
with a stale `daemon.addr' still on disk reads the DEAD address on its
first tick and calls the daemon booted milliseconds after the spawn.  That
happened on a real cold start: `stale-addr 127.0.0.1:58161' and
`booted 127.0.0.1:58161' three milliseconds apart, a link refused on the
spot, and ten seconds of nothing until the daemon we started published its
own address.  The stale file is removed before the build, and this is the
second half of the same guard: whatever is read back, the address already
proven dead is never mistaken for the new daemon's.")

(defvar agent-repl-daemon--own-address nil
  "The address the daemon THIS Emacs spawned published, or nil.
Kept so its exit can retire its own `daemon.addr' without ever retiring a
successor's: a file whose content has moved on belongs to another daemon.")

(defvar agent-repl-daemon--departure-timer nil
  "The pending poll waiting for a stopped daemon to remove `daemon.addr'.")

(defvar agent-repl-daemon--departure-deadline nil
  "`float-time' after which the departure wait gives up.")

(defvar agent-repl-daemon--departure-continuation nil
  "The function the departure wait calls once the daemon is gone.")

(defvar agent-repl-daemon--ensure-in-flight nil
  "Non-nil while an ensure is between its start and its outcome.
A second ensure while one is under way would spawn a second daemon,
which is the one thing this file exists to prevent.")

;;;; ---- External boundaries ----
;;
;; Every process this file starts goes through one of these.  Each is
;; registered in `agent-repl--external-boundary-functions', so a test that
;; reaches one unstubbed fails loudly rather than shelling out.

(defun agent-repl--frontend-run-build-script (args on-exit)
  "External-boundary wrapper: run the build shell with ARGS asynchronously.
ARGS is the argument list following the interpreter.  Output — stdout and
stderr alike — is captured into `agent-repl-daemon-build-buffer'.
ON-EXIT is called from the process sentinel with the integer exit code
once the process is no longer live.  Returns the process.

ASYNCHRONOUS BY CONTRACT.  Its predecessor was a `call-process', which
froze Emacs for the whole build — including before the very first
redisplay, so a startup after any touched source showed no frame at all
until the stack finished building."
  (make-process ;; ALLOW-EXTERNAL-BOUNDARY
   :name "agent-repl-build-frontend"
   :buffer (get-buffer-create agent-repl-daemon-build-buffer)
   :command (cons agent-repl-daemon-build-shell args)
   :connection-type 'pipe
   :noquery t
   :sentinel
   (lambda (process _event)
     (unless (process-live-p process)
       (funcall on-exit (process-exit-status process))))))

(defun agent-repl--frontend-file-mtime (path)
  "External-boundary wrapper: PATH's mtime as a float, or nil when absent."
  (let ((attributes (file-attributes path))) ;; ALLOW-EXTERNAL-BOUNDARY
    (and attributes (float-time (file-attribute-modification-time attributes)))))

(defun agent-repl--frontend-source-files (dir regexp)
  "External-boundary wrapper: DIR's source files matching REGEXP, recursively.
REGEXP nil means every file.  `node_modules', `dist' and `bin' are never
descended into: they hold artifacts, not sources, and an artifact newer
than itself would make every tree permanently stale."
  (when (file-directory-p dir) ;; ALLOW-EXTERNAL-BOUNDARY
    (directory-files-recursively
     dir (or regexp "") nil
     (lambda (subdir)
       (not (member (file-name-nondirectory (directory-file-name subdir))
                    '("node_modules" "dist" "bin")))))))

(defun agent-repl--frontend-spawn-daemon (argv environment)
  "External-boundary wrapper: spawn ARGV under ENVIRONMENT, return the process.
ENVIRONMENT is a complete `process-environment' value, which is how
`AGENT_REPL_STATE_DIR' is exported EXPLICITLY rather than inherited."
  (let ((process-environment environment))
    (make-process ;; ALLOW-EXTERNAL-BOUNDARY
     :name "claude-repld"
     :buffer agent-repl--frontend-daemon-buffer
     :command argv
     :noquery t
     :sentinel #'agent-repl-daemon--sentinel)))

(defun agent-repl--frontend-artifact-exists-p (path)
  "External-boundary wrapper: return non-nil when artifact PATH exists."
  (file-exists-p path)) ; ALLOW-EXTERNAL-BOUNDARY

(defun agent-repl--frontend-probe-boot-claim (binary state-dir)
  "External-boundary wrapper: ask BINARY whether STATE-DIR\='s boot claim is held.
Returns whatever `call-process\=' returns: the daemon\='s integer exit status,
or the signal that ended it.  The daemon answers
`agent-repl-daemon--claim-free-status\=' for a free claim,
`agent-repl-daemon--claim-held-status\=' for a held one, and anything else
for a claim it could not decide."
  (call-process binary nil nil nil ;; ALLOW-EXTERNAL-BOUNDARY
                "-probe-boot-claim" "-state-dir" state-dir))

(defun agent-repl--frontend-run-log-tail ()
  "External-boundary wrapper: the last non-blank line of `daemon.run.log\='.
Returns a DESCRIPTION when there is no line to return — an unreadable or
empty run log is itself the diagnosis, and answering nil would hand the
caller a blank where the reason belongs."
  (let ((path (agent-repl--global-state-file "logs/daemon.run.log")))
    (if (not (file-readable-p path)) ;; ALLOW-EXTERNAL-BOUNDARY
        (format "<no readable %s>" path)
      (with-temp-buffer
        (insert-file-contents path) ;; ALLOW-EXTERNAL-BOUNDARY
        (goto-char (point-max))
        (skip-chars-backward " \t\n\r")
        (let ((end (point)))
          (forward-line 0)
          (if (= (point) end)
              (format "<%s is empty>" path)
            (buffer-substring-no-properties (point) end)))))))

(defun agent-repl--frontend-stdio-log-tail ()
  "External-boundary wrapper: the last non-blank line of the daemon stdio log.

THE OTHER HALF OF THE DIAGNOSIS.  The run log is the daemon\='s own
contract JSONL, so a daemon that died before its logger stood up writes
nothing to it; whatever it did say went to stderr, and since the spawn is
detached that stderr is a FILE rather than the `*claude-repld*' buffer.
Answers a DESCRIPTION rather than nil for the same reason the run-log
tail does: an absent or empty file is itself part of the answer."
  (let ((path (agent-repl-daemon--stdio-log-path)))
    (if (not (file-readable-p path)) ;; ALLOW-EXTERNAL-BOUNDARY
        (format "<no readable %s>" path)
      (with-temp-buffer
        (insert-file-contents path) ;; ALLOW-EXTERNAL-BOUNDARY
        (goto-char (point-max))
        (skip-chars-backward " \t\n\r")
        (let ((end (point)))
          (forward-line 0)
          (if (= (point) end)
              (format "<%s is empty>" path)
            (buffer-substring-no-properties (point) end)))))))

;;;; ---- The mode-line segment ----

(defun agent-repl-daemon--workspace-bringup-label ()
  "Return \"workspaces: opening n/m\" while roster tabs are still painting.
Nil once every roster workspace's panel is painted, and nil before there
is a roster at all.

THE COUNT IS READ, NEVER KEPT.  open-progress.el owns the per-workspace
phases and `agent-repl-open-progress-opening-workspaces' is its answer to
\"which of these is not painted yet\"; a second tally maintained here
would be a second answer to the same question, and a progress display
whose two answers disagree is worse than none."
  (when (and (fboundp 'agent-repl-roster-tab-order)
             (fboundp 'agent-repl-open-progress-opening-workspaces))
    (let* ((tabs (agent-repl-roster-tab-order))
           (total (length tabs))
           (opening (seq-intersection
                     tabs (agent-repl-open-progress-opening-workspaces))))
      (when (and (> total 0) opening)
        (format "workspaces: opening %d/%d" (- total (length opening)) total)))))

(defun agent-repl-daemon--clear-lifecycle ()
  "Take a settled lifecycle note down and repaint."
  (setq agent-repl-daemon--lifecycle-timer nil
        agent-repl-daemon--lifecycle nil)
  (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
                    "elisp.daemon.lifecycle state=cleared")
  (agent-repl-daemon--refresh-segment))

(defun agent-repl-daemon--set-lifecycle (state)
  "Move the bring-up lifecycle to STATE, record it, and repaint.
A SETTLED state (`adopted', `ready') schedules its own clearing after
`agent-repl-daemon-build-status-display-seconds' -- an outcome is worth
saying and not worth keeping -- and any pending clear is cancelled first
so an earlier outcome's timer cannot wipe a later one's note."
  (when (timerp agent-repl-daemon--lifecycle-timer)
    (cancel-timer agent-repl-daemon--lifecycle-timer))
  (setq agent-repl-daemon--lifecycle-timer nil
        agent-repl-daemon--lifecycle state)
  (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
                    "elisp.daemon.lifecycle state=%s" state)
  (let ((line (cdr (assq state agent-repl-daemon--lifecycle-echo-lines))))
    (when line
      (agent-repl--phase-echo
       '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
       "%s" line)))
  (when (memq state agent-repl-daemon--lifecycle-settled-states)
    (setq agent-repl-daemon--lifecycle-timer
          (run-with-timer agent-repl-daemon-build-status-display-seconds nil
                          #'agent-repl-daemon--clear-lifecycle)))
  (agent-repl-daemon--refresh-segment)
  state)

(defun agent-repl-daemon-on-link-up (&optional _conn)
  "Settle the bring-up lifecycle once the primary link stands.
Registered on `agent-repl-link-up-functions'.  Which outcome it is turns
on the same liveness question the provenance line asks: a daemon this
Emacs started is `ready', one it attached to is `adopted'."
  (agent-repl-daemon--set-lifecycle
   (if (agent-repl-daemon--spawned-here-p) 'ready 'adopted)))

(defvar agent-repl-daemon--workspace-echo-done nil
  "`(DONE . TOTAL)\=' the last bring-up echo reported, or nil for none yet.

THE DEDUPE IS THE POINT.  Both feeds into this line repeat themselves:
`agent-repl-open-progress-change-functions\=' fires on every phase step of
every pending open -- several times per workspace -- and a reconcile can
re-report a count a previous pass already said.  An echo is issued only
when the PAIR moves, so the user reads one line per workspace and nothing
in between.")

(defun agent-repl-daemon--reset-workspace-echo ()
  "Forget the last bring-up echo so the next cold start starts from zero."
  (setq agent-repl-daemon--workspace-echo-done nil))

(defun agent-repl-daemon--echo-workspace-progress (done total)
  "Echo \"loading workspaces (DONE/TOTAL)\" unless that pair was just said.
Returns the pair when it echoed, nil when the dedupe or an empty TOTAL
suppressed it."
  (unless (or (null total) (zerop total)
              (equal (cons done total) agent-repl-daemon--workspace-echo-done))
    (setq agent-repl-daemon--workspace-echo-done (cons done total))
    (agent-repl--phase-echo
     '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
     "loading workspaces (%d/%d)\u2026" done total)
    agent-repl-daemon--workspace-echo-done))

(defun agent-repl-daemon--echo-workspaces-ready (ready)
  "Echo the bring-up\='s closing line naming READY workspaces, once.
Silent unless a progress line was actually issued first: a session with
no bring-up behind it has nothing to announce the end of."
  (when agent-repl-daemon--workspace-echo-done
    (agent-repl-daemon--reset-workspace-echo)
    (agent-repl--phase-echo
     '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
     "%d workspaces ready" ready)
    ready))

(defun agent-repl-daemon-on-roster-bringup (opened total finished)
  "Echo the startup\='s workspace bring-up as the roster opens tabs.

Registered on `agent-repl-roster-bringup-functions\='.  THIS IS THE PATH A
HIDDEN STARTUP TAKES: panels park until focus, so a cold start paints
nothing and the painted-count feed below never moves -- the roster
opening the tabs is the only thing that does.  OPENED of TOTAL is the
progress line; FINISHED closes the pass with the same ready line the
painted feed ends on."
  (if finished
      (agent-repl-daemon--echo-workspaces-ready opened)
    (agent-repl-daemon--echo-workspace-progress opened total)))

(defun agent-repl-daemon-on-open-progress-change ()
  "Repaint the segment and echo the workspace bring-up count when it moves.
Registered on `agent-repl-open-progress-change-functions', which is
open-progress.el's one publication of its own state.

Two lines can come out of here, both through `agent-repl--phase-echo' so
the record and the echo are one call and neither can arrive while the
user is typing: \"loading workspaces (n/m)\" each time another workspace
finishes painting, and \"N workspaces ready\" once the pending set empties
after having been non-empty.  A change that moves neither -- a phase step
inside a workspace still opening -- echoes nothing."
  (agent-repl-daemon--refresh-segment)
  (when (and (fboundp 'agent-repl-roster-tab-order)
             (fboundp 'agent-repl-open-progress-opening-workspaces))
    (let* ((tabs (agent-repl-roster-tab-order))
           (total (length tabs))
           (opening (seq-intersection
                     tabs (agent-repl-open-progress-opening-workspaces)))
           (done (- total (length opening))))
      (cond
       ((zerop total) nil)
       (opening (agent-repl-daemon--echo-workspace-progress done total))
       (t (agent-repl-daemon--echo-workspaces-ready total))))))

(defun agent-repl-daemon--refresh-segment ()
  "Recompute `agent-repl-daemon-mode-line-segment' from the bring-up state.
Precedence: a standing failure outranks a running build, which outranks
the daemon's own bring-up lifecycle, which outranks the short-lived
\"just built\" note, which outranks the workspace bring-up count."
  (setq agent-repl-daemon-mode-line-segment
        (cond
         (agent-repl-daemon-launch-failure "daemon: launch failed")
         (agent-repl-daemon-build-failure "daemon: build failed")
         ((eq agent-repl-daemon--build-state 'building)
          "building agent-repl stack\u2026")
         ((cdr (assq agent-repl-daemon--lifecycle
                     agent-repl-daemon--lifecycle-labels)))
         ((and (eq agent-repl-daemon--build-state 'built)
               agent-repl-daemon--build-duration)
          (format "agent-repl stack built in %.1fs"
                  agent-repl-daemon--build-duration))
         ((agent-repl-daemon--workspace-bringup-label))))
  (agent-repl--log '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.segment segment=%S"
                   agent-repl-daemon-mode-line-segment)
  (force-mode-line-update t)
  ;; THE TAB-BAR CAVEAT (status.el): the tab-bar caches its render by string
  ;; equality, so a segment change that the mode line picks up can still
  ;; leave a tab-bar-hosted `global-mode-string' showing the old text.  Only
  ;; reached when a tab-bar is actually up.
  (when (and (bound-and-true-p tab-bar-mode)
             (fboundp 'agent-repl--force-tab-bar-redraw))
    (agent-repl--force-tab-bar-redraw))
  agent-repl-daemon-mode-line-segment)

(defun agent-repl-daemon--clear-build-status ()
  "Take the post-build status note down and repaint."
  (setq agent-repl-daemon--build-status-timer nil
        agent-repl-daemon--build-state nil
        agent-repl-daemon--build-duration nil)
  (agent-repl--log '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.build-status-cleared")
  (agent-repl-daemon--refresh-segment))

(defun agent-repl-daemon--set-build-status (state duration)
  "Report STATE (`building', `built' or nil) with DURATION in the mode line.
A `built' status schedules its own clearing; any pending clear timer is
cancelled first so two builds in a row cannot have the earlier one's
timer wipe the later one's status."
  (when (timerp agent-repl-daemon--build-status-timer)
    (cancel-timer agent-repl-daemon--build-status-timer))
  (setq agent-repl-daemon--build-status-timer nil
        agent-repl-daemon--build-state state
        agent-repl-daemon--build-duration duration)
  (when (eq state 'built)
    (setq agent-repl-daemon--build-status-timer
          (run-with-timer agent-repl-daemon-build-status-display-seconds nil
                          #'agent-repl-daemon--clear-build-status)))
  (agent-repl-daemon--refresh-segment))

(defun agent-repl-daemon-install-segment ()
  "Add the build-status segment to `global-mode-string', once."
  (let ((current (if (listp global-mode-string)
                     global-mode-string
                   (list global-mode-string))))
    (if (memq 'agent-repl-daemon-mode-line-segment current)
        (agent-repl--log '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.segment-already-installed")
      (setq global-mode-string
            (append current (list 'agent-repl-daemon-mode-line-segment)))
      (agent-repl--log '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.segment-installed"))))

;;;; ---- The build ----

(defconst agent-repl-daemon--default-build-targets '("shim" "webapp" "daemon" "lock")
  "The targets `bin/build-frontend.sh' builds when given none.
Mirrors the script's own default set; the elisp pre-check has to know
which artifacts a default run would cover in order to decide whether one
is worth starting at all.")

(defconst agent-repl-daemon--go-source-regexp "\\(?:\\.go\\|go\\.mod\\|go\\.sum\\)\\'"
  "Which files under a Go module count as its sources.")

(defconst agent-repl-daemon--build-target-specs
  (let ((root agent-repl--frontend-root)
        (cache-bin (expand-file-name "~/.cache/agent-repl/bin")))
    (cl-flet ((at (relative) (expand-file-name relative root)))
      (list
       (list "shim"
             :artifact (at "agent-shim/claude/shim/dist/main.js")
             :sources (list (at "agent-shim/claude/shim/src"))
             :files (list (at "agent-shim/claude/shim/package.json")
                          (at "agent-shim/claude/shim/tsconfig.json")
                          (at "agent-shim/claude/shim/build.mjs")))
       (list "webapp"
             :artifact (at "webapp/dist/index.html")
             :sources (list (at "webapp/src"))
             :files (list (at "webapp/package.json")
                          (at "webapp/tsconfig.json")
                          (at "webapp/vite.config.ts")))
       (list "daemon"
             :artifact (at "daemon/bin/claude-repld")
             :sources (list (at "daemon"))
             :match agent-repl-daemon--go-source-regexp)
       (list "store"
             :artifact (expand-file-name "shim-store" cache-bin)
             :sources (list (at "agent-shim/shim-store") (at "agent-shim/wire")
                            (at "agent-shim/logging/go") (at "proto/gen/go"))
             :match agent-repl-daemon--go-source-regexp)
       (list "sidecar"
             :artifact (expand-file-name "shim-claude-sidecar" cache-bin)
             :sources (list (at "agent-shim/claude/shim-sidecar") (at "agent-shim/wire")
                            (at "agent-shim/logging/go") (at "proto/gen/go"))
             :match agent-repl-daemon--go-source-regexp)
       ;; The shim spawns this for every kernel claim and refuses to start a
       ;; session without it, which is why it is in the DEFAULT target set
       ;; while the two launchd services are not.  It reads no proto.
       (list "lock"
             :artifact (expand-file-name "shim-lock" cache-bin)
             :sources (list (at "agent-shim/shim-lock") (at "agent-shim/logging/go"))
             :match agent-repl-daemon--go-source-regexp))))
  "Per-target artifact and source set, mirroring `bin/build-frontend.sh'.

WHY A SECOND COPY OF THE STALENESS QUESTION.  The script owns staleness
and remains the authority INSIDE a build — it still re-decides every
target and skips the fresh ones.  This spec only answers the cheaper
question the script cannot answer without being started: is starting it
worth a subprocess at all?  On a fresh tree the script costs seconds of
`npm'/`go' startup to conclude nothing changed, and Emacs used to pay
that on every boot.")

(defun agent-repl-daemon--target-stale-p (spec)
  "Return non-nil when SPEC's artifact is missing or older than its sources."
  (let* ((plist (cdr spec))
         (artifact (plist-get plist :artifact))
         (artifact-mtime (agent-repl--frontend-file-mtime artifact)))
    (if (null artifact-mtime)
        t
      (let ((newest 0)
            (sources (append (plist-get plist :files)
                             (apply #'append
                                    (mapcar (lambda (dir)
                                              (agent-repl--frontend-source-files
                                               dir (plist-get plist :match)))
                                            (plist-get plist :sources))))))
        (dolist (file sources)
          (let ((mtime (agent-repl--frontend-file-mtime file)))
            (when (and mtime (> mtime newest))
              (setq newest mtime))))
        ;; `>=' and not `>': the script uses the same comparison, and a
        ;; source written in the same second as the artifact is a source the
        ;; build may not have seen.
        (>= newest artifact-mtime)))))

(defun agent-repl-daemon--stale-targets (targets)
  "Return the subset of TARGETS needing a build; nil for TARGETS's default set.
An UNKNOWN target name is reported stale: a target this spec cannot
reason about must never be silently skipped."
  (seq-filter
   (lambda (name)
     (let ((spec (assoc name agent-repl-daemon--build-target-specs)))
       (if (null spec)
           (progn
             (agent-repl--warn '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.build-target-unknown target=%S" name)
             t)
         (agent-repl-daemon--target-stale-p spec))))
   (or targets agent-repl-daemon--default-build-targets)))

(defun agent-repl-daemon--build-finished (exit-code)
  "Settle the running build with EXIT-CODE and answer every waiting caller."
  (let* ((duration (- (float-time) (or agent-repl-daemon--build-started (float-time))))
         (output (with-current-buffer (get-buffer-create agent-repl-daemon-build-buffer)
                   (string-trim-right (buffer-string))))
         (labels agent-repl-daemon--build-labels)
         (targets agent-repl-daemon--build-target-names)
         (continuations (nreverse agent-repl-daemon--build-continuations))
         (detail (unless (eq exit-code 0)
                   (format "build failed (exit %s) — see %s"
                           exit-code agent-repl-daemon-build-buffer))))
    (setq agent-repl-daemon--build-in-flight nil
          agent-repl-daemon--build-process nil
          agent-repl-daemon--build-continuations nil
          agent-repl-daemon--build-started nil
          agent-repl-daemon--build-labels nil
          agent-repl-daemon--build-target-names nil)
    (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.build script=%S targets=%S exit=%S output=%s"
                      agent-repl-daemon-build-script (or targets 'default) exit-code
                      (if (string-empty-p output) "<empty>" output))
    (if detail
        (agent-repl-daemon--set-build-status nil nil)
      (agent-repl--phase-echo
       '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
       "%s built (%.1fs)" (cdr labels) duration)
      (agent-repl-daemon--set-build-status 'built duration))
    (dolist (continuation continuations)
      (funcall continuation detail))))

(defun agent-repl-daemon--build (targets continuation)
  "Build TARGETS if stale, then call CONTINUATION with nil or a failure detail.
TARGETS is a list of build-script target names, or nil for the script's
own default set (the whole stack).

ASYNCHRONOUS.  CONTINUATION runs from the build process's sentinel, and
SYNCHRONOUSLY on the two paths that start no process at all: a missing
script, and a tree where nothing is stale.

COALESCING.  A request arriving while a build runs joins that build
instead of starting a second one; both continuations settle together.

A failure is SURFACED and not retried: the capture buffer is shown, the
mode-line segment is raised, and the interactive ensure is the retry."
  (let* ((running-label (if targets (string-join targets "/") "the stack"))
         (done-label (if targets (string-join targets "/") "stack")))
    (cond
     ((not (agent-repl--frontend-artifact-exists-p agent-repl-daemon-build-script))
      (let ((detail (format "build script not found: %s" agent-repl-daemon-build-script)))
        (agent-repl--error '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.build-script-missing script=%S"
                           agent-repl-daemon-build-script)
        (funcall continuation detail)))
     (agent-repl-daemon--build-in-flight
      (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.build-coalesced targets=%S"
                        (or targets 'default))
      (push continuation agent-repl-daemon--build-continuations))
     ((null (agent-repl-daemon--stale-targets targets))
      ;; Nothing moved, so nothing is spawned: the script's own check would
      ;; reach the same verdict, after paying a subprocess to do it.
      (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.build-skipped-fresh targets=%S"
                        (or targets 'default))
      (funcall continuation nil))
     (t
      (with-current-buffer (get-buffer-create agent-repl-daemon-build-buffer)
        (erase-buffer))
      (agent-repl--phase-echo
       '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
       "building %s\u2026" running-label)
      (setq agent-repl-daemon--build-in-flight t
            agent-repl-daemon--build-continuations (list continuation)
            agent-repl-daemon--build-started (float-time)
            agent-repl-daemon--build-labels (cons running-label done-label)
            agent-repl-daemon--build-target-names targets)
      (agent-repl-daemon--set-build-status 'building nil)
      ;; A signal out of the spawn must not leave the in-flight guard raised:
      ;; a stuck guard would coalesce every later build onto a process that
      ;; never existed.  The signal itself is re-raised, never swallowed.
      (let ((process (condition-case err
                         (agent-repl--frontend-run-build-script
                          (cons agent-repl-daemon-build-script targets)
                          #'agent-repl-daemon--build-finished)
                       (error
                        (setq agent-repl-daemon--build-in-flight nil
                              agent-repl-daemon--build-continuations nil)
                        (agent-repl-daemon--set-build-status nil nil)
                        (agent-repl--error '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.build-spawn-failed error=%S" err)
                        (signal (car err) (cdr err))))))
        ;; Only when the build is still running: a stub (or a process that
        ;; exited before this returned) may already have settled it.
        (when agent-repl-daemon--build-in-flight
          (setq agent-repl-daemon--build-process process)))))))

(defun agent-repl-daemon--report-build-failure (detail)
  "Surface build failure DETAIL: the buffer, a WARNING, an echo, the segment."
  (setq agent-repl-daemon-build-failure detail)
  (agent-repl--warn '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.build-failed detail=%s" detail)
  (agent-repl-popup-show (get-buffer-create agent-repl-daemon-build-buffer))
  ;; ONE CALL, NOT A LOG PLUS A BARE `message'.  The echo goes out through
  ;; the module's own logging function in echo mode, so the line the user
  ;; reads is the line the log file carries.  A FAILURE is deliberately not
  ;; behind the minibuffer guard `agent-repl--phase-echo' applies to
  ;; progress: a build that did not happen is worth interrupting for.
  (agent-repl--backend-phase
   '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
   "%s" detail)
  (agent-repl-daemon--refresh-segment))

(defun agent-repl-daemon--report-launch-failure (detail)
  "Surface launch failure DETAIL: an error line, an echo, the segment.
LOUD BY CONTRACT.  The failures this reports — a missing account root, a
daemon that exits before it publishes its address — used to show up only as
a status-2 log line followed by a thirty-second boot timeout, with every
verb afterwards failing on a nil connection."
  (setq agent-repl-daemon-launch-failure detail)
  (agent-repl--error '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.launch-failed detail=%s" detail)
  ;; Same one-call rule as the build failure above, and loud for the same
  ;; reason: a daemon that never launched is not progress chatter.
  (agent-repl--backend-phase
   '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
   "%s" detail)
  (agent-repl-daemon--refresh-segment))

(defun agent-repl-daemon--clear-launch-failure ()
  "Clear a recorded launch failure and take its mode-line segment down."
  (when agent-repl-daemon-launch-failure
    (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.launch-failure-cleared")
    (setq agent-repl-daemon-launch-failure nil)
    (agent-repl-daemon--refresh-segment)))

(defun agent-repl-daemon--clear-build-failure ()
  "Clear a recorded build failure and take its mode-line segment down."
  (when agent-repl-daemon-build-failure
    (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.build-failure-cleared")
    (setq agent-repl-daemon-build-failure nil)
    (agent-repl-daemon--refresh-segment)))

;;;; ---- The probe ----

(defun agent-repl-daemon--probe (address on-answer on-silence)
  "Ask `DaemonHealth' at ADDRESS over a FRESH connection.
ON-ANSWER receives the decoded verdict plist `(:arm :healthy | :unhealthy
:value V)', or nil when the daemon answered with an error arm — ANY
answer proves a daemon is there.  ON-SILENCE receives the transport
failure detail, which is the only evidence that nobody is listening.  The
probe connection is closed on both paths: it exists to ask one question."
  (let ((conn (agent-repl-connect-open address)))
    (cl-labels
        ((finish (fn arg)
           (agent-repl-connect-close conn)
           (funcall fn arg)))
      (agent-repl-rpc-daemon-health
       conn nil
       :timeout agent-repl-daemon-health-timeout-seconds
       :on-response
       (lambda (response)
         (pcase (plist-get response :arm)
           (:success (finish on-answer (plist-get response :value)))
           (:error
            ;; A daemon that REFUSES the health question still answered it:
            ;; something is serving on that address, so it is adopted.
            (agent-repl--warn '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.health-error address=%S error=%S"
                              address (plist-get response :value))
            (finish on-answer nil))
           (arm
            (agent-repl--error '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.health-unknown-arm address=%S arm=%S"
                               address arm)
            (finish on-answer nil))))
       :on-failure
       (lambda (detail) (finish on-silence detail))))))

(defun agent-repl-daemon-observe-identity (conn on-answer on-failure)
  "Ask CONN which daemon process serves it.
ON-ANSWER receives `agentrepl.v1.DaemonIdentity' as a plist, or nil when
the answering daemon predates that response field.  ON-FAILURE receives a
diagnostic when the question could not be answered.  The compatibility nil
is an explicit wire-version fact; callers deciding restart completion must
require the replacement to state a valid identity."
  (unless (and (functionp on-answer) (functionp on-failure))
    (agent-repl--fatal '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
                       "elisp.daemon.observe-identity needs callable continuations"))
  (if (null conn)
      (let ((detail "no daemon link is available"))
        (agent-repl--error '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
                           "elisp.daemon.identity-unavailable reason=no-link")
        (funcall on-failure detail))
    (agent-repl--log '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
                     "elisp.daemon.identity-request conn=%S" conn)
    (agent-repl-rpc-daemon-health
     conn nil
     :timeout agent-repl-daemon-health-timeout-seconds
     :on-response
     (lambda (response)
       (pcase (plist-get response :arm)
         (:success
          (let* ((verdict (plist-get response :value))
                 (identity (plist-get verdict :identity)))
            (if (and identity
                     (or (not (stringp (plist-get identity :instance-id)))
                         (string-empty-p (plist-get identity :instance-id))
                         (not (integerp (plist-get identity :pid)))
                         (<= (plist-get identity :pid) 0)))
                (let ((detail (format "daemon returned an invalid process identity: %S"
                                      identity)))
                  (agent-repl--error '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
                                     "elisp.daemon.identity-invalid identity=%S" identity)
                  (funcall on-failure detail))
              (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
                                "elisp.daemon.identity-observed instance=%S pid=%S build-sha=%S legacy=%s"
                                (plist-get identity :instance-id)
                                (plist-get identity :pid)
                                (plist-get identity :build-sha)
                                (if identity "nil" "t"))
              (funcall on-answer identity))))
         (:error
          (let ((detail (format "daemon refused the identity health query: %S"
                                (plist-get response :value))))
            (agent-repl--error '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
                               "elisp.daemon.identity-refused error=%S"
                               (plist-get response :value))
            (funcall on-failure detail)))
         (arm
          (let ((detail (format "daemon returned unknown health arm %S" arm)))
            (agent-repl--error '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
                               "elisp.daemon.identity-unknown-arm arm=%S" arm)
            (funcall on-failure detail)))))
     :on-failure
     (lambda (detail)
       (agent-repl--error '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
                          "elisp.daemon.identity-failed detail=%S" detail)
       (funcall on-failure (format "daemon identity query failed: %S" detail))))))

(defun agent-repl-daemon--render-health (address faults)
  "Write the adopted daemon at ADDRESS and its FAULTS into the health buffer."
  (with-current-buffer (get-buffer-create "*agent-repl-health*")
    (goto-char (point-max))
    (insert (format "\ndaemon %s: UNHEALTHY (%d fault(s))\n" address (length faults)))
    (dolist (fault faults)
      (insert (format "  - %s\n" (plist-get fault :detail))))))

(defun agent-repl-daemon--spawned-here-p ()
  "Return non-nil when the daemon THIS Emacs spawned is still running.
Nil is the ordinary answer: Emacs adopts whatever answers, and the one
process it started itself is the only daemon that is its own."
  (and agent-repl--frontend-daemon-process
       (process-live-p agent-repl--frontend-daemon-process)
       t))

(defun agent-repl-daemon--report-provenance (address)
  "Record whose daemon the adopted one at ADDRESS is.
A FOREIGN daemon — one this Emacs did not spawn — is the normal
cold-start outcome and not a fault, but which daemon a session attached
to is the first thing anyone reading the log needs, so it is STATED
rather than inferred from the absence of a start line."
  ;; BOTH bring-up paths pass through here with an address in hand -- the
  ;; adopt path from the probe verdict and the cold start from the boot
  ;; wait -- so this is the one place the lifecycle can learn that an
  ;; address exists without either path having to remember to say so.
  (agent-repl-daemon--set-lifecycle 'linking)
  (if (agent-repl-daemon--spawned-here-p)
      (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.own-adopted address=%S" address)
    (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.foreign-adopted address=%S" address)))

(defun agent-repl-daemon--report-verdict (address verdict)
  "Log the adopted daemon at ADDRESS and surface VERDICT's faults, if any."
  (agent-repl-daemon--report-provenance address)
  (pcase (plist-get verdict :arm)
    (:healthy
     (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.adopted address=%S health=healthy" address))
    (:unhealthy
     (let ((faults (plist-get (plist-get verdict :value) :faults)))
       (agent-repl--warn '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.adopted-unhealthy address=%S faults=%d"
                         address (length faults))
       (agent-repl-daemon--render-health address faults)))
    (_
     (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.adopted address=%S health=unstated" address))))

;;;; ---- The boot wait ----

(defun agent-repl-daemon--cancel-boot-wait ()
  "Cancel any pending boot poll and forget its continuation."
  (when (timerp agent-repl-daemon--boot-timer)
    (cancel-timer agent-repl-daemon--boot-timer))
  (setq agent-repl-daemon--boot-timer nil
        agent-repl-daemon--boot-deadline nil
        agent-repl-daemon--boot-process nil
        agent-repl-daemon--boot-rejected-address nil
        agent-repl-daemon--boot-continuation nil))

(defun agent-repl-daemon--await-address (on-ready &optional process rejected-address)
  "Poll `daemon.addr\=' until it appears, then call ON-READY with the address.
ON-READY receives nil when the boot timeout elapses first.  A TIMER poll,
never a sleep: the daemon writes its address atomically when it is ready
to serve, so the file\='s appearance IS readiness and nothing else has to
be probed.

PROCESS is the daemon THIS Emacs spawned, when there is one.  A process
that EXITS ends the wait at once, with the tail of its run log: a daemon
that died is never going to publish an address, and waiting out the full
timeout to say so buries the reason it died.

REJECTED-ADDRESS is an address this ensure already PROVED dead.  Reading
it back is never readiness; see `agent-repl-daemon--boot-rejected-address'."
  (agent-repl-daemon--cancel-boot-wait)
  (setq agent-repl-daemon--boot-deadline
        (+ (float-time) agent-repl-daemon-boot-timeout-seconds)
        agent-repl-daemon--boot-process process
        agent-repl-daemon--boot-rejected-address rejected-address
        agent-repl-daemon--boot-continuation on-ready)
  (agent-repl-daemon--boot-tick))

(defun agent-repl-daemon--boot-tick ()
  "One poll of the boot wait: the timer's whole body.
Named and argument-free so a test drives the wait by calling it, with no
dependence on the scheduler and no sleep anywhere."
  (setq agent-repl-daemon--boot-timer nil)
  (let* ((read (condition-case err
                   (agent-repl-connect-read-daemon-addr)
                 (error
                  (agent-repl--warn '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.boot-addr-unreadable error=%S" err)
                  nil)))
         (rejected (and read agent-repl-daemon--boot-rejected-address
                        (equal read agent-repl-daemon--boot-rejected-address)))
         (address (and (not rejected) read))
         (on-ready agent-repl-daemon--boot-continuation)
         (deadline (or agent-repl-daemon--boot-deadline 0)))
    (when rejected
      (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
                        "elisp.daemon.boot-addr-rejected address=%S reason=already-judged-stale"
                        read))
    (cond
     (address
      (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.booted address=%S" address)
      (agent-repl-daemon--cancel-boot-wait)
      (when on-ready (funcall on-ready address)))
     ((agent-repl-daemon--exited-p agent-repl-daemon--boot-process)
      (let ((status (process-exit-status agent-repl-daemon--boot-process))
            (tail (agent-repl--frontend-run-log-tail))
            ;; BOTH TAILS, because the spawn is detached: a daemon that
            ;; died before its own logger stood up wrote nothing to the run
            ;; log, and what it did say went to the stdio log the detacher
            ;; redirects into rather than to a buffer in this Emacs.
            (stdio (agent-repl--frontend-stdio-log-tail)))
        (agent-repl--error '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.boot-exited status=%S run-log=%s stdio-log=%s"
                           status tail stdio)
        (agent-repl-daemon--cancel-boot-wait)
        (agent-repl-daemon--report-launch-failure
         (format "the daemon exited (status %s) before it published its address: %s"
                 status tail)))
      (when on-ready (funcall on-ready nil)))
     ((>= (float-time) deadline)
      (agent-repl--error '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.boot-timeout seconds=%.1f file=%S"
                         agent-repl-daemon-boot-timeout-seconds
                         (agent-repl-connect-daemon-addr-file))
      (agent-repl-daemon--cancel-boot-wait)
      (when on-ready (funcall on-ready nil)))
     (t
      (agent-repl--log '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.boot-waiting remaining=%.3f"
                       (- deadline (float-time)))
      (setq agent-repl-daemon--boot-timer
            (run-with-timer agent-repl-daemon-boot-poll-interval-seconds nil
                            #'agent-repl-daemon--boot-tick))))))

(defun agent-repl-daemon--cancel-departure-wait ()
  "Cancel any pending departure poll and forget its continuation."
  (when (timerp agent-repl-daemon--departure-timer)
    (cancel-timer agent-repl-daemon--departure-timer))
  (setq agent-repl-daemon--departure-timer nil
        agent-repl-daemon--departure-deadline nil
        agent-repl-daemon--departure-continuation nil))

(defconst agent-repl-daemon--claim-free-status 0
  "The probe\='s exit status for a boot claim NOBODY holds.
Mirrors the daemon\='s own `exitSuccess\='.")

(defconst agent-repl-daemon--claim-held-status 3
  "The probe\='s exit status for a boot claim a live process holds.
Mirrors the daemon\='s own `exitClaimLost\='.")

(defun agent-repl-daemon--boot-claim-state ()
  "Return `free\=', `held\=' or `undecided\=' for this state root\='s boot claim.

THE BOOT CLAIM IS WHAT \"THE PREVIOUS DAEMON IS GONE\" MEANS.  The daemon
withdraws `daemon.addr\=' at the START of its shutdown so a client
forwarding during shutdown finds no address; the claim -- an exclusive
kernel lock on `daemon.lock\=' -- is released only when the process
actually ends.  So the address file going away says nothing about
whether a daemon is still there, and only the claim does.

AN UNDECIDABLE CLAIM IS NOT A FREE ONE.  A probe that could not run, or
that answered anything the daemon does not define, is reported as
`undecided\=' and WARNED about; a caller that treated it as departure
would be doing exactly what this function exists to prevent."
  (let* ((binary (car agent-repl-daemon-command))
         (state-dir (directory-file-name (agent-repl--global-state-dir)))
         (outcome (condition-case err
                      (cons :status
                            (agent-repl--frontend-probe-boot-claim binary state-dir))
                    (error (cons :error err))))
         (status (and (eq (car outcome) :status) (cdr outcome))))
    (cond
     ((eql status agent-repl-daemon--claim-free-status) 'free)
     ((eql status agent-repl-daemon--claim-held-status) 'held)
     (t
      (agent-repl--warn '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.claim-probe-undecided outcome=%S binary=%S state-dir=%S"
                        outcome binary state-dir)
      'undecided))))

(defun agent-repl-daemon--await-departure (on-gone)
  "Poll until the stopped daemon releases the BOOT CLAIM, then call ON-GONE.
ON-GONE receives non-nil when the claim came free and nil when the wait
timed out with it still held or undecidable.  A TIMER poll, never a
sleep, and the mirror image of `agent-repl-daemon--await-address\='.

IT IS THE CLAIM, NOT `daemon.addr\=', THAT SAYS THE DAEMON IS GONE.  The
address is withdrawn at the start of the daemon\='s shutdown and the claim
is held until its process ends, so a wait keyed on the address reported
departure into a window where the outgoing daemon was still holding the
claim -- and the replacement Emacs then spawned lost the claim to it and
exited, leaving no daemon at all (2026-09-12)."
  (agent-repl-daemon--cancel-departure-wait)
  (setq agent-repl-daemon--departure-deadline
        (+ (float-time) agent-repl-daemon-boot-timeout-seconds)
        agent-repl-daemon--departure-continuation on-gone)
  (agent-repl-daemon--departure-tick))

(defun agent-repl-daemon--departure-tick ()
  "One poll of the departure wait: the timer\='s whole body.
Named and argument-free so a test drives the wait by calling it, with no
dependence on the scheduler and no sleep anywhere.

The DECISION is the boot claim; the address is carried alongside it as
diagnosis only, so a timeout still names the address the departing
daemon left standing."
  (setq agent-repl-daemon--departure-timer nil)
  (let ((claim (agent-repl-daemon--boot-claim-state))
        (address (condition-case err
                     (agent-repl-connect-read-daemon-addr)
                   (error
                    (agent-repl--warn '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.departure-addr-unreadable error=%S" err)
                    nil)))
        (on-gone agent-repl-daemon--departure-continuation)
        (deadline (or agent-repl-daemon--departure-deadline 0)))
    (cond
     ((eq claim 'free)
      (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.departed address=%S" address)
      (agent-repl-daemon--cancel-departure-wait)
      (when on-gone (funcall on-gone t)))
     ((>= (float-time) deadline)
      (agent-repl--warn '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.departure-timeout seconds=%.1f claim=%S address=%S file=%S"
                        agent-repl-daemon-boot-timeout-seconds claim address
                        (agent-repl-connect-daemon-addr-file))
      (agent-repl-daemon--cancel-departure-wait)
      (when on-gone (funcall on-gone nil)))
     (t
      (agent-repl--log '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.departure-waiting claim=%S address=%S remaining=%.3f"
                       claim address (- deadline (float-time)))
      (setq agent-repl-daemon--departure-timer
            (run-with-timer agent-repl-daemon-boot-poll-interval-seconds nil
                            #'agent-repl-daemon--departure-tick))))))

;;;; ---- The start ----

(defun agent-repl-daemon--sentinel (proc event)
  "Record the daemon PROC's EVENT.  Emacs supervises, it does not restart.
Everything after boot is the daemon's own blue-green rollout, so a daemon
that exits is a fact to record — daemon-link.el's reconnect is what
notices and recovers.

Runs under `agent-repl--with-deferred-quit\', which this sentinel had
before the overhaul refactor dropped it.  A `C-g\' between retiring
`daemon.addr\' and forgetting the process leaves the address file naming
a dead daemon while Emacs still believes it owns a live one, which is the
stale address the next cold start reads back as readiness.  Atomicity is
the whole reason: Emacs already binds `inhibit-quit\' around sentinels,
so the guard adds the record of the deferral and the handing on of the
flag, not the protection itself."
  (unless (process-live-p proc)
    (agent-repl--with-deferred-quit "daemon-sentinel"
      (agent-repl-daemon--record-exit proc event))))

(defvar agent-repl-daemon--exit-requested nil
  "The daemon this editor has asked to shut down, or nil for none.
Set where the request is issued (`agent-repl-frontend-daemon-stop\'), which
every restart path goes through, and consumed by
`agent-repl-daemon--record-exit\'.

IT NAMES THE PROCESS, NOT MERELY THE FACT.  It holds the process object the
order was given to when this Emacs spawned that daemon, and t when there was
no process object to name (an adopted daemon).  Naming the addressee is what
keeps an order given to one daemon from excusing the departure of its
successor, and it has to be identity rather than a blanket clear on spawn:
`agent-repl-daemon--await-departure\' keys on the BOOT CLAIM, which the
kernel releases the instant the old process ends, while Emacs runs that
process\='s sentinel later.  So the restart\='s ensure spawns, and the
predecessor\='s sentinel only then fires.  A clear at the spawn wiped the
order before the exit it excused was ever recorded, and two orderly
restarts (ordered by the deploy script the daemon\='s own Deploy has since
replaced) were logged as unrequested exits at WARN (2026-09-13).  A t is
still cleared on spawn: it names nobody, so it could otherwise excuse the
new daemon.")

(defconst agent-repl-daemon--exit-log-format
  "elisp.daemon.exited status=%S event=%s requested=%s"
  "The one format every daemon exit is recorded with, whatever its level.
One format keeps every exit under a single operation name, so a harvest
groups a requested exit with an unrequested one and the LEVEL is what
separates them; `requested=' says which this was.")

(defun agent-repl-daemon--record-exit (proc event)
  "Record PROC's EVENT and release the daemon this Emacs spawned.
The quit-guarded critical section of `agent-repl-daemon--sentinel\',
named separately so the guard has a body to wrap and a test can drive the
recording without a process death.

THE LEVEL FOLLOWS WHO ASKED.  A daemon exit is not one fact:

  - AN EXIT THIS EDITOR ASKED FOR is the requested outcome arriving.  Every
    backend restart this editor orders goes through
    `agent-repl-frontend-daemon-stop\', and the exit that follows was the
    point of the exercise — recorded at INFO.  (A deploy is the daemon\='s
    own: its Deploy replaces a stale daemon by a blue-green handover, never
    through this stop.)  Warning about it put a `WARNING:\' line in
    *Messages* twice in one morning for two orderly restarts, which is the
    log crying wolf about its own instruction being obeyed.

  - AN EXIT NOBODY ASKED FOR is the daemon leaving on its own.  A clean
    status still means work this editor believed was being served has
    stopped, so it stays a WARN; a non-zero status is the daemon reporting
    its own failure, and that is an ERROR.

The request is CONSUMED here, so it excuses exactly the one exit it
ordered and the next unrequested departure is heard in full."
  (let* ((status (process-exit-status proc))
         (order agent-repl-daemon--exit-requested)
         (requested (or (eq order t) (eq order proc)))
         ;; A HANDOVER IS THE DAEMON'S OWN ANNOUNCED DEPARTURE.  A rolling
         ;; deploy's outgoing daemon announces its successor, transfers every
         ;; workspace and exits -- routinely BEFORE its stream's end promotes
         ;; the successor here.  A clean exit while a successor stands or is
         ;; pending is that planned departure, recorded at INFO; it never
         ;; consumes an order given for some other daemon.
         (handover (and (not requested) (eql status 0)
                        (or (agent-repl-link-successor)
                            (agent-repl-link-successor-pending-p))))
         (trimmed (string-trim (or event "")))
         (scope '(:agent-repl-central "the resident daemon lifecycle spans workspaces")))
    ;; CONSUMED ONLY BY ITS OWN ADDRESSEE.  An order standing for some other
    ;; daemon outlives this exit, so the departure it was given for is still
    ;; heard as requested when it arrives.
    (when requested (setq agent-repl-daemon--exit-requested nil))
    (cond
     (requested
      (agent-repl--info scope agent-repl-daemon--exit-log-format
                        status trimmed "t"))
     (handover
      (agent-repl--info scope agent-repl-daemon--exit-log-format
                        status trimmed "handover"))
     ((eql status 0)
      (agent-repl--warn scope agent-repl-daemon--exit-log-format
                        status trimmed "nil"))
     (t
      (agent-repl--error scope agent-repl-daemon--exit-log-format
                         status trimmed "nil"))))
  (when (eq proc agent-repl--frontend-daemon-process)
    (agent-repl-daemon--retire-own-addr)
    (setq agent-repl--frontend-daemon-process nil)))

(defun agent-repl-daemon--retire-own-addr ()
  "Remove `daemon.addr' when it still names the daemon THIS Emacs spawned.
A daemon that exits cleanly removes its own file; one that is killed, or
that dies, does not — and the file it leaves is the stale address the next
cold start reads back as readiness.  The content is checked against the
address our own daemon published, so a SUCCESSOR's file (a blue-green
rollout writes a different address into the same path) is never retired."
  (let ((own agent-repl-daemon--own-address))
    (when own
      (let ((current (condition-case nil
                         (agent-repl-connect-read-daemon-addr)
                       (error nil))))
        (cond
         ((null current)
          (agent-repl--log '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
                           "elisp.daemon.own-addr-already-gone address=%S" own))
         ((equal current own)
          (agent-repl-daemon--retire-stale-addr own "own-daemon-exited"))
         (t
          (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
                            "elisp.daemon.own-addr-kept own=%S current=%S reason=successor-published"
                            own current))))
      (setq agent-repl-daemon--own-address nil))))

(defun agent-repl-daemon--exited-p (proc)
  "Return non-nil when PROC is a real process that is no longer live.
A NON-PROCESS IS NOT AN EXIT: the spawn wrapper is stubbable, so anything
that is not a process object is a stand-in that has said nothing about
whether a daemon died."
  (and (processp proc) (not (process-live-p proc))))

(defconst agent-repl-daemon--required-config-flags
  '(("--default-config-dir" . agent-repl-daemon-default-config-dir)
    ("--multi-repo-config-dir" . agent-repl-daemon-multi-repo-config-dir))
  "The account-root flags the daemon REQUIRES, as (FLAG . VARIABLE).
Both are non-negotiable: the daemon\='s account resolver refuses to build
without either, and the process exits 2 before it serves.")

(defun agent-repl-daemon--config-value (symbol)
  "SYMBOL\='s config-root value, expanded, or nil when it is empty."
  (let ((value (symbol-value symbol)))
    (and (stringp value)
         (not (string-empty-p (string-trim value)))
         (expand-file-name (string-trim value)))))

(defun agent-repl-daemon--missing-config-flags ()
  "The required account-root flags whose value is empty, as strings."
  (delq nil
        (mapcar (lambda (entry)
                  (unless (agent-repl-daemon--config-value (cdr entry))
                    (format "%s (%s)" (car entry) (cdr entry))))
                agent-repl-daemon--required-config-flags)))

(defun agent-repl-daemon--argv ()
  "The full spawn argv: `agent-repl-daemon-command\=' plus the account roots.
Every path is expanded, because the daemon compares config roots against
resolved workspace paths and a `~\=' the shell never saw would never match."
  (append agent-repl-daemon-command
          (mapcan (lambda (entry)
                    (list (car entry) (agent-repl-daemon--config-value (cdr entry))))
                  agent-repl-daemon--required-config-flags)))

(defconst agent-repl-daemon--detach-shell "/bin/sh"
  "The shell that detaches the daemon from this Emacs\='s lifetime.
POSIX `sh\=' is the whole requirement — `trap\=', a redirection and `exec\='
— so the system shell is enough and no launcher of our own has to exist
on disk before a cold start can happen.")

(defconst agent-repl-daemon--detach-script
  "mkdir -p \"$1\" || exit 1; shift; trap '' HUP; exec \"$@\" >>\"$0\" 2>&1"
  "The detacher, run as `sh -c SCRIPT STDIO-LOG LOG-DIR DAEMON-ARGV...\='.

READ IT IN ORDER, because each clause closes one way the daemon used to
die with the editor:

  `mkdir -p \"$1\"\='  the state root\='s `logs/\=' need not exist yet on a
                    first ever cold start, and a redirection into a
                    missing directory would fail the spawn outright.  A
                    failure here EXITS 1, which the boot wait already
                    reports as \"exited before it published its address\".
  `trap \='\=' HUP\='    sets SIGHUP to SIG_IGN, and that disposition survives
                    the `exec\='.  Emacs SIGHUPs every child from
                    `kill-emacs\='; the daemon now outlives it.
  `exec \"$@\"\='      REPLACES the shell, so the pid Emacs holds is the
                    daemon\='s own — the sentinel, the boot wait\='s exit
                    branch and `agent-repl-daemon--spawned-here-p\=' all
                    keep watching the real process.
  `>>\"$0\" 2>&1\='   sends the daemon\='s output to a FILE.  Left on Emacs\='s
                    pipe, the first line written after Emacs exits would
                    reach a closed read end and kill the daemon with
                    SIGPIPE — a detach that only held until the daemon
                    next spoke.

The stdio log rides in `$0\=' and the log directory in `$1\=' rather than
being interpolated into the script text, so no path is ever re-parsed by
the shell.")

(defun agent-repl-daemon--spawn-argv ()
  "The argv Emacs actually spawns: the detacher, then the daemon\='s own argv.

`agent-repl-daemon--argv\=' remains the DAEMON\='S argv, unprefixed — it is
what the daemon sees in its own `argv\=' after the `exec\=', and every flag
assertion is about that list.  This is the launch wrapper around it."
  (let ((stdio (agent-repl-daemon--stdio-log-path)))
    (append (list agent-repl-daemon--detach-shell
                  "-c"
                  agent-repl-daemon--detach-script
                  stdio
                  (directory-file-name (file-name-directory stdio)))
            (agent-repl-daemon--argv))))

(defun agent-repl-daemon--environment ()
  "Return the spawn environment, with `AGENT_REPL_STATE_DIR' set EXPLICITLY.
ONE state root is the cross-system contract; a child that inherited a
different one would be a silent split-brain, so the value Emacs resolved
is stated on the spawn rather than assumed."
  (let* ((root (and (stringp agent-repl-daemon-multi-repo-root)
                    (not (string-empty-p agent-repl-daemon-multi-repo-root))
                    (expand-file-name agent-repl-daemon-multi-repo-root)))
         (base (cl-remove-if
                (lambda (entry)
                  (or (string-prefix-p "AGENT_REPL_STATE_DIR=" entry)
                      (and root (string-prefix-p "MULTI_REPO_ROOT=" entry))))
                process-environment))
         (env (cons (format "AGENT_REPL_STATE_DIR=%s"
                            (directory-file-name (agent-repl--global-state-dir)))
                    base)))
    ;; THE DAEMON READS THE ROOT FROM THE ENVIRONMENT, not from a flag, so
    ;; a configured root is STATED here the same way the state root is.  An
    ;; unconfigured one leaves the inherited value alone: nil is "Emacs has
    ;; no opinion", not "unset whatever the session had".
    (if root
        (cons (format "MULTI_REPO_ROOT=%s" (directory-file-name root)) env)
      env)))

(defun agent-repl-daemon--start ()
  "Spawn the daemon and return its process, or nil when the binary is absent."
  (let ((binary (car agent-repl-daemon-command))
        (missing (agent-repl-daemon--missing-config-flags)))
    (cond
     ((not (agent-repl--frontend-artifact-exists-p binary))
      (agent-repl--error '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.binary-missing binary=%S" binary)
      nil)
     ;; REFUSED HERE, LOUDLY.  Spawning without an account root buys a
     ;; status-2 exit, a thirty-second boot timeout, and every verb after it
     ;; failing on a nil connection — the launcher knows the value is missing
     ;; before it spends any of that.
     (missing
      (agent-repl-daemon--report-launch-failure
       (format "the daemon needs %s: set it before starting"
               (string-join missing ", ")))
      nil)
     (t
      (agent-repl-daemon--clear-launch-failure)
      ;; RECORDED HERE, ECHOED BY THE TRANSITION.  The spawn's own line is
      ;; kept for the log, but the minibuffer line for this phase is issued
      ;; once by `agent-repl-daemon--set-lifecycle' a few forms below, so
      ;; the user does not read "starting the daemon" twice in a row.
      (agent-repl--info
       '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
       "starting the daemon...")
      (let* ((argv (agent-repl-daemon--spawn-argv))
             (proc (agent-repl--frontend-spawn-daemon
                    argv (agent-repl-daemon--environment))))
        (setq agent-repl--frontend-daemon-process proc)
        ;; A NEW DAEMON INHERITS NO ORDERS.  An order that names no process
        ;; could otherwise excuse this daemon's unasked-for death, so it is
        ;; spent here.  One that NAMES its addressee is left standing: it can
        ;; only ever match that process, and the predecessor's sentinel
        ;; routinely fires after this spawn.
        (when (eq agent-repl-daemon--exit-requested t)
          (setq agent-repl-daemon--exit-requested nil))
        (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.started argv=%S state-dir=%S"
                          argv (agent-repl--global-state-dir))
        (agent-repl-daemon--set-lifecycle 'starting)
        proc)))))

;;;; ---- The entry point ----

(defun agent-repl-daemon--settle (on-ready outcome)
  "End the ensure with OUTCOME and hand it to ON-READY."
  (setq agent-repl-daemon--ensure-in-flight nil)
  (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.ensure-settled ready=%s"
                    (if outcome "t" "nil"))
  (when on-ready (funcall on-ready outcome))
  outcome)

(defun agent-repl-daemon--advertiser-alive-p (pid)
  "Return non-nil when PID names a process alive on this host.
`process-attributes' answers nil for a pid no process holds and never
signals, so a dead advertiser is detected WITHOUT a dial -- which is the
whole point: a dead advertiser is absent, not a transport fault."
  (and (integerp pid) (> pid 0) (process-attributes pid) t))

(defun agent-repl-daemon--retire-dead-advertiser-addr (address pid)
  "Remove the `daemon.addr' whose advertiser PID is no longer alive.
An advertisement whose advertiser is dead is ABSENT, not a transport
failure: it is retired at INFO and the boot proceeds straight to a spawn,
with NO dial -- so none of the connect/transport records a live-but-
unreachable daemon would earn ever fires on a plain cold start after a
predecessor died without withdrawing (crash, SIGKILL, or a restart path
that skipped withdraw)."
  (let ((file (agent-repl-connect-daemon-addr-file)))
    (when (file-exists-p file)
      (condition-case err
          (progn
            (delete-file file)
            (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
                              "elisp.daemon.addr-retired-dead-advertiser address=%S pid=%S file=%S"
                              address pid file))
        (error
         (agent-repl--error '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
                            "elisp.daemon.addr-retire-dead-advertiser-failed address=%S pid=%S file=%S error=%S"
                            address pid file err))))))

(defun agent-repl-daemon--retire-stale-addr (address reason)
  "Remove the `daemon.addr' file whose ADDRESS was just proven dead.
REASON names which proof retired it, for the record.
The file is the readiness signal, so leaving a dead one on disk while a
fresh daemon boots makes the boot wait read the corpse's address on its
first tick and declare the new daemon booted at an address nothing is
listening on.  Removing it is not a repair of someone else's state: the
address behind it refused a connection, and only the daemon that owns a
live one ever writes the file."
  (let ((file (agent-repl-connect-daemon-addr-file)))
    (when (file-exists-p file)
      (condition-case err
          (progn
            (delete-file file)
            (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
                              "elisp.daemon.stale-addr-removed address=%S reason=%s file=%S"
                              address reason file))
        (error
         (agent-repl--error '(:agent-repl-central "the resident daemon lifecycle spans workspaces")
                            "elisp.daemon.stale-addr-remove-failed address=%S reason=%s file=%S error=%S"
                            address reason file err))))))

(defun agent-repl-daemon--build-and-start (on-ready &optional rejected-address)
  "Build, start a daemon, wait for its address, and link to it.
ON-READY receives the connection, or nil.  The build is asynchronous, so
everything after it lives in the continuation.

REJECTED-ADDRESS is an address this ensure already proved dead; the boot
wait refuses to read it back as the new daemon's readiness."
  (agent-repl-daemon--build
   nil
   (lambda (failure)
     (if failure
         (progn
           (agent-repl-daemon--report-build-failure failure)
           (agent-repl-daemon--settle on-ready nil))
       (agent-repl-daemon--clear-build-failure)
       (let ((proc (agent-repl-daemon--start)))
        (if (null proc)
           (agent-repl-daemon--settle on-ready nil)
         (agent-repl-daemon--await-address
          (lambda (address)
           (if (null address)
               (agent-repl-daemon--settle on-ready nil)
             ;; STATE whose daemon this is on the own-spawn path too.  The
             ;; adopt path reports provenance from the probe verdict, but a
             ;; cold start never probes -- and a log that only ever carries
             ;; `foreign-adopted' would say the session attached to someone
             ;; else's daemon every single time it started its own.
             (agent-repl-daemon--report-provenance address)
              (setq agent-repl-daemon--own-address address)
              (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.linking address=%S" address)
              (agent-repl-daemon--settle on-ready (agent-repl-link-connect))))
          proc rejected-address)))))))

(defun agent-repl-daemon--begin (on-ready)
  "Take the cold-start decision for `agent-repl-daemon-ensure'.
Separated from the entry point so the in-flight flag's release covers the
WHOLE decision, signals included."
  (let ((address (condition-case err
                     (agent-repl-connect-read-daemon-addr)
                   (error
                    (agent-repl--warn '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.addr-unreadable error=%S" err)
                    nil))))
    (if (null address)
        (progn
          (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.addr-absent")
          (agent-repl-daemon--build-and-start on-ready))
      ;; AN ADVERTISEMENT CARRIES ITS ADVERTISER'S PID.  A pid that names no
      ;; live process is a dead advertiser, and a dead advertiser's address
      ;; is ABSENT -- retire it at INFO and go straight to a spawn, with NO
      ;; dial, so a predecessor that died without withdrawing costs the cold
      ;; start none of the dial-failed / transport-failure records a
      ;; live-but-unreachable daemon rightly earns.  A LEGACY file names no
      ;; pid (nil), and an ALIVE pid is a real daemon: both dial and probe.
      (let ((pid (condition-case err
                     (agent-repl-connect-read-daemon-addr-pid)
                   (error
                    (agent-repl--warn '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.addr-pid-unreadable error=%S" err)
                    nil))))
        (if (and pid (not (agent-repl-daemon--advertiser-alive-p pid)))
            (progn
              (agent-repl-daemon--retire-dead-advertiser-addr address pid)
              ;; The address travels with the boot wait so a file that somehow
              ;; survives the retire is never read back as the new daemon.
              (agent-repl-daemon--build-and-start on-ready address))
          (agent-repl-daemon--probe
           address
           (lambda (verdict)
             (agent-repl-daemon--report-verdict address verdict)
             (agent-repl-daemon--settle on-ready (agent-repl-link-connect)))
           (lambda (detail)
             (agent-repl--warn '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.stale-addr address=%S detail=%S"
                               address detail)
             ;; The corpse's file goes FIRST, before anything can mistake it for
             ;; the daemon about to be built, and the address travels with the
             ;; boot wait so a file that somehow survives still cannot.
             (agent-repl-daemon--retire-stale-addr address "probe-refused")
             (agent-repl-daemon--build-and-start on-ready address))))))))

(defun agent-repl-daemon-schedule-ensure ()
  "Schedule `agent-repl-daemon-ensure' on an idle timer and return the timer.
THE FRAME PAINTS FIRST.  This is what the startup hook registers, and the
whole reason it exists: `emacs-startup-hook' runs BEFORE Doom's UI init
and before the first redisplay, so anything it does synchronously — even
deciding whether a build is needed — happens while there is no frame on
screen.  An idle timer cannot run until the command loop is idle, which
is after that first redisplay."
  (setq agent-repl-daemon--startup-timer
        (run-with-idle-timer agent-repl-daemon-startup-idle-seconds nil
                             #'agent-repl-daemon-ensure))
  (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.ensure-scheduled idle=%s"
                    agent-repl-daemon-startup-idle-seconds)
  agent-repl-daemon--startup-timer)

(defun agent-repl-daemon-ensure (&optional on-ready)
  "Make sure a daemon is serving, adopting one wherever one answers.
ON-READY receives the primary connection, or nil when no daemon could be
brought up.  The whole decision, in order:

  - a link already stands            => nothing to do
  - `daemon.addr' absent             => build, start, wait, link
  - present and the daemon ANSWERS   => ADOPT it (healthy or not) and link
  - present and NOTHING answers      => a stale file; treat it as absent

Emacs NEVER kills a daemon that answers.  Idempotent, and refuses to run
twice at once: a concurrent ensure is exactly how a second daemon gets
spawned beside a live one."
  (cond
   ((agent-repl-link-up-p)
    (agent-repl--log '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.ensure-noop reason=link-up")
    (when on-ready (funcall on-ready (agent-repl-link-primary)))
    (agent-repl-link-primary))
   (agent-repl-daemon--ensure-in-flight
    (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.ensure-already-in-flight")
    nil)
   (t
    (setq agent-repl-daemon--ensure-in-flight t)
    ;; A SIGNAL out of the ensure must not leave the in-flight flag raised:
    ;; the flag is what refuses a second ensure, so one stuck at t disables
    ;; cold start for the rest of the session — silently, because every
    ;; later ensure would look like an ordinary concurrent one.  The signal
    ;; itself is re-raised, never swallowed.
    (condition-case err
        (agent-repl-daemon--begin on-ready)
      (error
       (setq agent-repl-daemon--ensure-in-flight nil)
       (agent-repl--error '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.ensure-failed error=%S" err)
       (signal (car err) (cdr err)))))))

;;;; ---- Interactive commands ----

(defun agent-repl-frontend-daemon-ensure ()
  "Ensure a daemon is serving; also the retry after a build failure."
  (interactive)
  (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.ensure-command")
  (agent-repl-daemon-ensure
   (lambda (conn)
     (message "agent-repl: %s"
              (if conn "daemon ready" "daemon NOT ready - see the log")))))

(defun agent-repl-frontend-daemon-stop (&optional on-done)
  "Ask the daemon to shut down now.  EMACS NEVER KILLS A DAEMON.
The stop is `UpdateShutdownSchedule{now}' with an operator reason naming
this editor — the daemon stops accepting work, flushes its in-flight
writes and exits itself, which is the only shutdown that strands
nothing.

ON-DONE, when given, receives `(:arm :accepted)' once the daemon accepts
the shutdown, or `(:arm :not-restarted :reason STRING)' on a refusal,
missing link, or transport failure; interactive callers leave it out."
  (interactive)
  (let ((conn (agent-repl-link-primary)))
    (if (null conn)
        (progn
          (agent-repl--warn '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.stop-skipped reason=no-link")
          (message "agent-repl: no daemon link to stop")
          (when on-done
            (funcall on-done '(:arm :not-restarted
                               :reason "no daemon link is available")))
          nil)
      (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.stop")
      ;; RECORDED AT THE REQUEST, not at its acceptance: the daemon may be
      ;; gone before its answer reaches us, and the exit that races the
      ;; acknowledgement is still the exit this editor asked for.
      (setq agent-repl-daemon--exit-requested
            (or agent-repl--frontend-daemon-process t))
      (agent-repl-rpc-update-shutdown-schedule
       conn (list :action
                  (list :arm :now
                        :value (list :reason (list :arm :operator
                                                   :value (list :note "emacs")))))
       :on-response
       (lambda (response)
         (pcase (plist-get response :arm)
           (:success
            (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.stop-accepted")
            (message "agent-repl: daemon shutting down")
            (when on-done (funcall on-done '(:arm :accepted))))
           (:error
            ;; A REFUSED shutdown is one nobody obeyed, so the request is
            ;; withdrawn and a later departure is heard as unrequested.
            (setq agent-repl-daemon--exit-requested nil)
            (let ((detail (format "daemon refused the immediate shutdown: %S"
                                  (plist-get response :value))))
              (agent-repl--error '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.stop-refused error=%S"
                                 (plist-get response :value))
              (when on-done
                (funcall on-done (list :arm :not-restarted :reason detail))))
            (message "agent-repl: daemon refused the shutdown"))
           (arm
            ;; An answer this editor cannot read is not an acceptance.
            (setq agent-repl-daemon--exit-requested nil)
            (agent-repl--error '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.stop-unknown-arm arm=%S" arm)
            (when on-done
              (funcall on-done
                       (list :arm :not-restarted
                             :reason (format "daemon returned unknown shutdown arm %S" arm)))))))
       :on-failure
       (lambda (detail)
         ;; THE REQUEST NEVER LANDED.  A daemon that goes away after a
         ;; shutdown this editor could not deliver went away on its own, and
         ;; is heard as such.
         (setq agent-repl-daemon--exit-requested nil)
         (agent-repl--error '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.stop-failed detail=%S" detail)
         (message "agent-repl: could not reach the daemon to stop it")
         (when on-done
           (funcall on-done
                    (list :arm :not-restarted
                          :reason (format "daemon stop request failed: %S" detail))))))
      t)))

(defun agent-repl-frontend-daemon-restart ()
  "Stop the daemon and ensure one again.
Emacs's own restart, distinct from the daemon's blue-green rollout: the
daemon is ASKED to exit, the link tears down, and the ensure brings a
fresh one up from a fresh build.

The three steps are SEQUENCED on the stop, not merely written in order.
The stop is an async rpc, so an ensure fired beside it runs while the
departing daemon is still answering -- and an ensure that finds a daemon
ADOPTS it, because Emacs never kills a daemon that answers.  The restart
would then re-adopt the daemon it had just asked to leave, no fresh build
would ever run, and the only visible trace would be a daemon exiting some
moments later with nothing left to reconnect to.  So: the stop's ack,
then the teardown, then the wait for the address to go away, and only
then the ensure.

With no link standing there is nothing to stop, and the restart is just
the ensure."
  (interactive)
  (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.restart")
  (if (null (agent-repl-link-primary))
      (progn
        (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.restart-nothing-to-stop reason=no-link")
        (agent-repl-daemon-ensure))
    (agent-repl-frontend-daemon-stop
     (lambda (outcome)
       (if (not (eq (plist-get outcome :arm) :accepted))
           ;; The daemon refused or never answered; it is still serving.
           ;; Ensuring here would adopt it and report a restart that did
           ;; not happen, so the refusal stands and the link is left alone.
           (progn
             (agent-repl--error '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.restart-abandoned reason=stop-not-accepted outcome=%S"
                                outcome)
             (message "agent-repl: not restarted: %s" (plist-get outcome :reason)))
         (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.restart-stop-accepted")
         (agent-repl-link-teardown)
         (agent-repl-daemon--await-departure
          (lambda (gone)
            (if gone
                (progn
                  (agent-repl--info '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.restart-ensure gone=t")
                  (agent-repl-daemon-ensure))
              (agent-repl--error '(:agent-repl-central "the resident daemon lifecycle spans workspaces") "elisp.daemon.restart-abandoned reason=departure-timeout gone=%S" gone)
              (message "agent-repl: not restarted: the accepted daemon stop never completed")))))))))

(add-hook 'agent-repl-link-no-daemon-functions #'agent-repl-daemon-ensure)
(add-hook 'agent-repl-link-up-functions #'agent-repl-daemon-on-link-up)
(add-hook 'agent-repl-open-progress-change-functions
          #'agent-repl-daemon-on-open-progress-change)
(add-hook 'agent-repl-roster-bringup-functions
          #'agent-repl-daemon-on-roster-bringup)
(agent-repl-daemon-install-segment)

(provide 'daemon)

;;; daemon.el ends here
