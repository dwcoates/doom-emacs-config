;;; services.el --- launchd-managed shim service lifecycle -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns the launchd integration boundary for shim-store and the Claude
;; transcript sidecar.  A coordinated runtime restart builds their installed
;; binaries only when stale, then uses launchctl kickstart so launchd remains
;; their process supervisor.  Store always comes up before sidecar.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--assert-main-thread "core" (what))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function agent-repl--frontend-all-turn-active-workspaces "frontend-client" ())
(declare-function agent-repl--frontend-artifact-exists-p "daemon" (path))
(declare-function agent-repl--frontend-bounce-after-build "daemon" (&optional preflight stop-shims on-complete))
(declare-function agent-repl--frontend-arm-expected-restart "daemon" (initiator))
(declare-function agent-repl--frontend-build-if-stale "daemon" (&optional force))
(declare-function agent-repl--frontend-build-targets-if-stale "daemon" (targets &optional force))
(declare-function agent-repl--frontend-init-inhibited-p "daemon" ())
(declare-function agent-repl--frontend-after-daemon-healthy "frontend-client" (on-success on-failure))
(declare-function agent-repl--frontend-after-ready "frontend-client" (on-ready on-failure &optional ws))
(declare-function agent-repl--frontend-rebind-workspaces-after-restart "frontend-client" (&optional on-success on-failure))
(declare-function agent-repl--frontend-runtime-bounce-preflight-async "daemon" (callback))
(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--warn "core" (ws fmt &rest args))
(declare-function agent-repl--log-verbose "core" (ws fmt &rest args))
(declare-function agent-repl--backend-phase "core" (ws fmt &rest args))
(declare-function agent-repl--backend-output-tail "core" (output &optional lines))
(declare-function agent-repl--logfile-path "core" ())
(declare-function agent-repl-uds-disconnect "frontend-uds" ())
(declare-function agent-repl--frontend-invalidate-daemon-view "frontend-state" (reason))

(defvar agent-repl-frontend-health-timeout)
(defvar agent-repl-frontend-ready-attempts)
(defvar agent-repl-uds-command-ack-deadline)

(defcustom agent-repl-shim-services-launchctl-program "launchctl"
  "Program used to inspect and kickstart the launchd-managed shim services."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-shim-store-ready-timeout 15.0
  "Seconds to wait for shim-store's socket after its launchd kickstart."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-runtime-restart-await-timeout 300.0
  "Seconds a synchronous deploy may await a coordinated runtime restart.
The ordinary interactive restart remains asynchronous.  This timeout exists
for deployment callers that must not report success before every restart,
health, and workspace-rebind continuation has completed."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-runtime-restart-health-timeout 60.0
  "Seconds a synchronous restart may await daemon health acknowledgement.
The replacement daemon can become ready before Emacs finishes applying its
startup snapshot.  At production roster sizes that snapshot work can delay
the correlated `daemonHealth' response beyond the ordinary command deadline.
This deployment-only budget is dynamically bound across the asynchronous
coordinator and its event pump; ordinary frontend commands retain their
shorter deadlines."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-runtime-restart-ready-attempts 150
  "Readiness attempts available to a synchronous runtime restart.
Each attempt uses the frontend readiness poll's fixed 0.2 second interval.
Large durable registries can make replacement startup exceed the ordinary
interactive budget; this deployment-only value remains bounded below the
terminal latch timeout and is restored when the latch returns."
  :type 'integer
  :group 'agent-repl)

(defconst agent-repl--shim-store-label "com.agentrepl.shim-store"
  "launchd label for shim-store.")

(defconst agent-repl--shim-sidecar-label
  "com.agentrepl.shim-claude-sidecar"
  "launchd label for the Claude transcript sidecar.")

(defconst agent-repl--shim-service-cache-bin
  (expand-file-name ".cache/agent-repl/bin/" "~")
  "Directory containing the binaries launchd executes.")

(defconst agent-repl--shim-store-binary
  (expand-file-name "shim-store" agent-repl--shim-service-cache-bin)
  "Installed shim-store binary.")

(defconst agent-repl--shim-sidecar-binary
  (expand-file-name "shim-claude-sidecar"
                    agent-repl--shim-service-cache-bin)
  "Installed Claude sidecar binary.")

(defconst agent-repl--shim-store-socket
  (expand-file-name ".cache/agent-repl/sock/store.sock" "~")
  "Unix socket shim-store recreates when launchd starts it.")

(defconst agent-repl--shim-services-buffer "*agent-repl-shim-services*"
  "Capture buffer for launchctl diagnostics.")

(defun agent-repl--shim-services-run-timer (seconds callback)
  "Integration boundary: run CALLBACK after SECONDS without blocking Emacs."
  (run-with-timer seconds nil callback))

(defun agent-repl--runtime-pump-events (seconds)
  "Integration boundary: process Emacs events for at most SECONDS."
  (accept-process-output nil seconds))

(defun agent-repl--launchctl-call (args)
  "External-boundary wrapper: run launchctl with ARGS and capture its output."
  (apply #'call-process ;; ALLOW-EXTERNAL-BOUNDARY
         agent-repl-shim-services-launchctl-program nil
         agent-repl--shim-services-buffer nil args))

(defun agent-repl--shim-service-file-sha256 (path)
  "External-boundary wrapper: return the SHA-256 digest of file PATH."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (secure-hash 'sha256 (current-buffer))))

(defun agent-repl--shim-service-write-stamp (path digest)
  "External-boundary wrapper: write DIGEST to deployment-stamp PATH."
  (with-temp-file path
    (insert digest "\n")))

(defun agent-repl--shim-store-socket-present-p ()
  "External-boundary wrapper: return non-nil when the store socket exists."
  (file-exists-p agent-repl--shim-store-socket))

(defun agent-repl--shim-services-output ()
  "Return captured launchctl output without trailing whitespace."
  (with-current-buffer (get-buffer-create agent-repl--shim-services-buffer)
    (string-trim-right (buffer-string))))

(defun agent-repl--shim-services-launchctl (verb label)
  "Run launchctl VERB for LABEL, logging all inputs and captured output."
  (with-current-buffer (get-buffer-create agent-repl--shim-services-buffer)
    (erase-buffer))
  (let* ((service (format "gui/%d/%s" (user-uid) label))
         (args
          (pcase verb
            ("print" (list "print" service))
            ("kickstart" (list "kickstart" "-k" service))
            (_
             (agent-repl--log nil
                              "shim-services launchctl: invalid verb=%S label=%s service=%s"
                              verb label service)
             (agent-repl--error nil
                                "invalid launchctl verb %S for service %s"
                                verb label))))
         (exit-code (agent-repl--launchctl-call args))
         (output (agent-repl--shim-services-output)))
    (agent-repl--log nil
                     "shim-services launchctl: verb=%s label=%s service=%s exit=%S output=%s"
                     verb label service exit-code
                     (if (string-empty-p output) "<empty>" output))
    (unless (eq exit-code 0)
      ;; `call-process' merged launchctl's stderr into the capture buffer, so
      ;; the refusal text is already in the durable record above.  The echo
      ;; line names the phase and carries only its tail.
      (agent-repl--backend-phase
       nil "launchctl %s FAILED for %s (exit %s): %s — full output in %s"
       verb label exit-code (agent-repl--backend-output-tail output)
       (agent-repl--logfile-path))
      (agent-repl--error nil
                         "launchd service %s failed `%s' (exit %s): %s"
                         label verb exit-code
                         (if (string-empty-p output) "<no output>" output)))
    t))

(defun agent-repl--shim-services-assert-launchd-loaded ()
  "Fail before mutation unless launchd owns both required service jobs."
  (agent-repl--shim-services-launchctl "print" agent-repl--shim-store-label)
  (agent-repl--shim-services-launchctl "print" agent-repl--shim-sidecar-label)
  (agent-repl--log nil
                   "shim-services preflight: launchd jobs loaded store=%s sidecar=%s"
                   agent-repl--shim-store-label agent-repl--shim-sidecar-label)
  t)

(defun agent-repl--shim-service-record-deployed (binary)
  "Record that launchd has started the installed BINARY."
  (let* ((digest (agent-repl--shim-service-file-sha256 binary))
         (stamp (expand-file-name
                 (format ".%s.deployed" (file-name-nondirectory binary))
                 agent-repl--shim-service-cache-bin)))
    (agent-repl--log nil
                     "shim-services deployed stamp: recording binary=%s stamp=%s sha256=%s"
                     binary stamp digest)
    (agent-repl--shim-service-write-stamp stamp digest)
    (agent-repl--log nil
                     "shim-services deployed stamp: binary=%s stamp=%s sha256=%s"
                     binary stamp digest)))

(defun agent-repl--shim-store-after-ready (on-success on-failure)
  "Poll for the newly kickstarted store socket without blocking Emacs."
  (unless (and (functionp on-success) (functionp on-failure))
    (error "agent-repl: shim-store readiness requires callable continuations"))
  (let* ((started-at (float-time))
         (deadline (+ started-at agent-repl-shim-store-ready-timeout))
         timer settled)
    (agent-repl--log nil
                     "shim-services store readiness: socket=%s timeout=%.1fs initial-ready=%s"
                     agent-repl--shim-store-socket
                     agent-repl-shim-store-ready-timeout
                     (if (agent-repl--shim-store-socket-present-p) "t" "nil"))
    (cl-labels
        ((finish (ok detail)
           (unless settled
             (setq settled t)
             (when (timerp timer) (cancel-timer timer))
             (agent-repl--log nil
                              "shim-services store readiness: outcome=%s socket=%s elapsed=%.3fs timeout=%.1fs detail=%S"
                              (if ok "ready" "timeout") agent-repl--shim-store-socket
                              (- (float-time) started-at)
                              agent-repl-shim-store-ready-timeout detail)
             (if ok (funcall on-success) (funcall on-failure detail))))
         (poll ()
           (let ((ready (agent-repl--shim-store-socket-present-p))
                 (now (float-time)))
             (agent-repl--log-verbose nil
                                      "shim-services store readiness poll: socket=%s ready=%s elapsed=%.3fs remaining=%.3fs"
                                      agent-repl--shim-store-socket
                                      (if ready "t" "nil") (- now started-at)
                                      (max 0.0 (- deadline now)))
             (cond
              (ready (finish t nil))
              ((>= now deadline)
               (finish nil (format "shim-store socket %s absent after %.1fs"
                                   agent-repl--shim-store-socket
                                   agent-repl-shim-store-ready-timeout)))
              (t (setq timer (agent-repl--shim-services-run-timer 0.1 #'poll)))))))
      (poll)
      :pending)))

(defun agent-repl--shim-services-build-and-bounce
    (preflight-complete on-success on-failure)
  "Build stale store/sidecar binaries and kickstart both launchd jobs.
The store is kickstarted and confirmed ready before the sidecar is touched.
Both deployed-binary stamps are written only after their corresponding
kickstart succeeds.  PREFLIGHT-COMPLETE means the coordinator already
validated both jobs before building any runtime artifact."
  (unless (and (functionp on-success) (functionp on-failure))
    (error "agent-repl: shim service bounce requires callable continuations"))
  (agent-repl--log nil
                   "shim-services build-and-bounce: beginning preflight-complete=%s store=%s sidecar=%s"
                   (if preflight-complete "t" "nil")
                   agent-repl--shim-store-binary
                   agent-repl--shim-sidecar-binary)
  (if preflight-complete
      (agent-repl--log nil
                       "shim-services build-and-bounce: using coordinator launchd preflight")
    (agent-repl--log nil
                     "shim-services build-and-bounce: validating launchd jobs before build")
    (agent-repl--shim-services-assert-launchd-loaded))
  (let ((build-result
         (agent-repl--frontend-build-targets-if-stale '("store" "sidecar"))))
    (agent-repl--log nil
                     "shim-services build-and-bounce: target build completed targets=%S result=%S"
                     '("store" "sidecar") build-result))
  (let ((store-present (agent-repl--frontend-artifact-exists-p
                        agent-repl--shim-store-binary))
        (sidecar-present (agent-repl--frontend-artifact-exists-p
                          agent-repl--shim-sidecar-binary)))
    (agent-repl--log nil
                     "shim-services build-and-bounce: artifacts checked store=%s present=%s sidecar=%s present=%s"
                     agent-repl--shim-store-binary (if store-present "t" "nil")
                     agent-repl--shim-sidecar-binary (if sidecar-present "t" "nil"))
    (unless (and store-present sidecar-present)
      (agent-repl--error nil
                         "shim service build completed without both binaries: store=%s present=%s sidecar=%s present=%s"
                         agent-repl--shim-store-binary (if store-present "t" "nil")
                         agent-repl--shim-sidecar-binary (if sidecar-present "t" "nil"))))
  (agent-repl--backend-phase nil "bouncing the store service…")
  (agent-repl--shim-services-launchctl "kickstart" agent-repl--shim-store-label)
  (agent-repl--shim-store-after-ready
   (lambda ()
     (condition-case err
         (progn
           (agent-repl--shim-service-record-deployed agent-repl--shim-store-binary)
           (agent-repl--backend-phase nil "store up; bouncing the sidecar service…")
           (agent-repl--shim-services-launchctl "kickstart" agent-repl--shim-sidecar-label)
           (agent-repl--shim-service-record-deployed agent-repl--shim-sidecar-binary)
           (agent-repl--log nil "shim-services bounce complete: store=%s sidecar=%s"
                            agent-repl--shim-store-label agent-repl--shim-sidecar-label)
           (agent-repl--backend-phase nil "store and sidecar services up")
           (funcall on-success))
       (error
        (let ((detail (error-message-string err)))
          (agent-repl--warn nil "shim-services bounce FAILED after store readiness: %s" detail)
          (agent-repl--backend-phase
           nil "sidecar service bounce FAILED: %s — full output in %s"
           detail (agent-repl--logfile-path))
          (funcall on-failure detail)))))
   (lambda (detail)
     (agent-repl--warn nil "shim-services bounce FAILED before sidecar: %s" detail)
     (agent-repl--backend-phase
      nil "store service never came up: %s — full output in %s"
      detail (agent-repl--logfile-path))
     (funcall on-failure detail)))
  :pending)

(defun agent-repl--runtime-retire-bounced-link ()
  "Retire the UDS link that belonged to the daemon a bounce just replaced.

A bounce DEFINITIONALLY ends the connection it was issued over: the old
daemon has already exited by the time the replacement is started.  Left
alone, the retired socket can still read as live for a beat while the
retained `DaemonView' still describes the daemon that is gone, so
`agent-repl--frontend-after-ready' returns `ready' against the corpse and
the next command's `process-send-string' fails with \"no longer connected
to pipe\" — a restart that SUCCEEDED reported as a rejected command.

Retiring the link here makes that unrepresentable rather than unlikely:
readiness after a bounce can only be satisfied by a fresh dial and the
REPLACEMENT daemon's own snapshot.  A replacement that never arrives still
exhausts the readiness budget and fails loudly."
  (agent-repl--log nil "runtime-prepare: retiring bounced daemon link before readiness")
  (agent-repl-uds-disconnect)
  (agent-repl--frontend-invalidate-daemon-view "runtime-bounce-retired-link"))

(defun agent-repl--frontend-expected-restart-initiator-name (rebind initiator)
  "Return the name the expected-restart window records for this bounce.
INITIATOR, when the caller supplied one, names the control-plane surface
that ordered the restart (a deploy over emacsclient, the interactive
command).  Absent that, the mode itself is the initiator: REBIND
distinguishes a restart from the once-per-Emacs startup bounce."
  (if (and (stringp initiator) (not (string-empty-p (string-trim initiator))))
      (string-trim initiator)
    (if rebind "runtime-restart" "runtime-startup")))

(defun agent-repl--runtime-prepare (rebind on-success on-failure &optional stop-shims initiator)
  "Asynchronously bounce dependencies, verify the daemon, and optionally REBIND.
ON-SUCCESS runs only after every requested stage completes.  ON-FAILURE
receives the first diagnostic and no later stage starts.

INITIATOR names the control-plane surface that ordered this bounce; it is
recorded on the expected-restart window this coordinator arms immediately
before it stops a daemon, so the exit Emacs itself ordered is classified as
a restart phase rather than as the daemon dying.  See
`agent-repl--frontend-arm-expected-restart'."
  (agent-repl--assert-main-thread "runtime-restart")
  (unless (and (functionp on-success) (functionp on-failure))
    (error "agent-repl: runtime preparation requires callable continuations"))
  (agent-repl--backend-phase nil "backend %s beginning…"
                             (if rebind "restart" "startup"))
  (let ((started (float-time)) settled)
    (cl-labels
        ((fail (detail)
           (unless settled
             (setq settled t)
             (agent-repl--log nil
                              "runtime-prepare: FAILED rebind=%s elapsed=%.3fs detail=%s"
                              (if rebind "t" "nil") (- (float-time) started) detail)
             (agent-repl--backend-phase
              nil "backend %s FAILED: %s — full output in %s"
              (if rebind "restart" "startup") detail
              (agent-repl--logfile-path))
             (funcall on-failure detail)))
         (complete (&optional rebound)
           (unless settled
             (setq settled t)
             (agent-repl--log nil
                              "runtime-prepare complete: mode=%s rebound=%S elapsed=%.3fs launchd-store=%s launchd-sidecar=%s"
                              (if rebind "restart" "startup") rebound
                              (- (float-time) started)
                              agent-repl--shim-store-label agent-repl--shim-sidecar-label)
             (agent-repl--backend-phase nil "backend %s complete (%.1fs)"
                                        (if rebind "restart" "startup")
                                        (- (float-time) started))
             (funcall on-success)))
         (after-daemon-bounce ()
           (agent-repl--frontend-after-ready
            (lambda ()
              (agent-repl--frontend-after-daemon-healthy
               (lambda ()
                 (if rebind
                     (agent-repl--frontend-rebind-workspaces-after-restart
                      #'complete #'fail)
                   (complete)))
               #'fail))
            #'fail))
         (bounce-runtime (daemon-state)
           (condition-case err
               (progn
                 (let ((busy (agent-repl--frontend-all-turn-active-workspaces)))
                   (agent-repl--log nil
                                    "runtime-restart preflight: daemon-state=%S busy=%S store=%s sidecar=%s"
                                    daemon-state busy agent-repl--shim-store-label
                                    agent-repl--shim-sidecar-label)
                   (when busy
                     (error "runtime restart refused: turn in flight in %s" busy)))
                 (agent-repl--shim-services-assert-launchd-loaded)
                 (let ((build-result (agent-repl--frontend-build-if-stale nil)))
                   (agent-repl--log nil
                                    "runtime-prepare: frontend build completed result=%S"
                                    build-result))
                 (agent-repl--shim-services-build-and-bounce
                  t
                  (lambda ()
                    ;; ARMED HERE, and no earlier: this is the last instant
                    ;; before a daemon is actually stopped, so nothing that
                    ;; fails during the preflight or the build can leave a
                    ;; window suppressing an unrelated daemon death.  A
                    ;; `:absent' state stops nothing, so it arms nothing.
                    (unless (eq daemon-state :absent)
                      (agent-repl--frontend-arm-expected-restart
                       (agent-repl--frontend-expected-restart-initiator-name
                        rebind initiator)))
                    (agent-repl--frontend-bounce-after-build
                     daemon-state stop-shims
                     (lambda (_started)
                      (agent-repl--runtime-retire-bounced-link)
                      (after-daemon-bounce))))
                  #'fail))
             (error (fail (error-message-string err)))))
         (after-preflight (daemon-state)
           (let ((daemon-present (memq daemon-state '(:tracked :responsive))))
             (agent-repl--log nil
                              "runtime-prepare: beginning rebind=%s daemon-state=%S daemon-present=%s"
                              (if rebind "t" "nil") daemon-state
                              (if daemon-present "t" "nil"))
             (if daemon-present
                 (agent-repl--frontend-after-ready
                  (lambda () (bounce-runtime daemon-state)) #'fail)
               (bounce-runtime daemon-state)))))
      (agent-repl--frontend-runtime-bounce-preflight-async #'after-preflight)
      :pending)))
(defun agent-repl-runtime-restart (&optional stop-shims initiator)
  "Rebuild, bounce, verify, then rebind the complete agent-repl runtime.
Refuses before any build or bounce when any daemon workspace reports an
active turn.

STOP-SHIMS (the interactive prefix argument) asks the outgoing daemon to
stop its session shims rather than leave them running for the replacement
to reattach to.  The default PRESERVES them; see
`agent-repl--runtime-prepare'.

INITIATOR, when supplied, names the surface that ordered this restart on the
expected-restart window; interactive callers leave it nil."
  (interactive "P")
  (agent-repl--log nil
                   "runtime-restart command: invoked interactive=%s stop-shims=%s initiator=%S"
                   (if (called-interactively-p 'interactive) "t" "nil")
                   (if stop-shims "t" "nil") initiator)
  (agent-repl--runtime-prepare
   t
   ;; `agent-repl--runtime-prepare' already echoes the terminal phase line
   ;; with its elapsed time; a second completion message here would only
   ;; overwrite it with less information.
   #'ignore
   (lambda (detail)
     (agent-repl--warn nil "runtime-restart command: FAILED detail=%s" detail))
   (and stop-shims t)
   initiator))

(defun agent-repl-runtime-restart-await (&optional stop-shims timeout initiator)
  "Restart the complete runtime and return only after terminal completion.
STOP-SHIMS has the same meaning as in `agent-repl-runtime-restart'.  TIMEOUT,
when non-nil, overrides `agent-repl-runtime-restart-await-timeout'.
INITIATOR names the ordering surface on the expected-restart window.

This synchronous surface is reserved for deployment orchestration.  It pumps
Emacs process output and timers while the canonical asynchronous coordinator
runs, then returns the exact string `runtime-restart-complete'.  A coordinator
failure or timeout is logged and signalled, so a caller cannot mistake the
initial `:pending' dispatch for a completed deployment."
  (let ((limit (or timeout agent-repl-runtime-restart-await-timeout)))
    (unless (and (numberp limit) (> limit 0))
      (agent-repl--error nil
                         "runtime-restart-await: invalid timeout=%S stop-shims=%s"
                         limit (if stop-shims "t" "nil")))
    (unless (and (numberp agent-repl-runtime-restart-health-timeout)
                 (> agent-repl-runtime-restart-health-timeout 0))
      (agent-repl--error
       nil
       "runtime-restart-await: invalid health-timeout=%S stop-shims=%s"
       agent-repl-runtime-restart-health-timeout (if stop-shims "t" "nil")))
    (unless (and (integerp agent-repl-runtime-restart-ready-attempts)
                 (> agent-repl-runtime-restart-ready-attempts 0))
      (agent-repl--error
       nil
       "runtime-restart-await: invalid ready-attempts=%S stop-shims=%s"
       agent-repl-runtime-restart-ready-attempts (if stop-shims "t" "nil")))
    (let* ((health-limit
            (min agent-repl-runtime-restart-health-timeout (* limit 0.8)))
           (ready-attempts
            (min agent-repl-runtime-restart-ready-attempts
                 (max 1 (floor (/ (* limit 0.8) 0.2)))))
           ;; These variables are special.  Their bindings remain active while
           ;; the synchronous event pump runs every asynchronous continuation,
           ;; but disappear when this deployment-only surface returns.
           (agent-repl-frontend-health-timeout health-limit)
           (agent-repl-frontend-ready-attempts ready-attempts)
           (agent-repl-uds-command-ack-deadline health-limit)
           (started (float-time))
           (deadline (+ started limit))
           (state :pending)
           failure)
      (agent-repl--log nil
                       "runtime-restart-await: beginning stop-shims=%s timeout=%.3fs health-timeout=%.3fs ready-attempts=%d"
                       (if stop-shims "t" "nil") limit health-limit ready-attempts)
      (agent-repl--runtime-prepare
       t
       (lambda () (setq state :complete))
       (lambda (detail)
         (setq failure detail
               state :failed))
       (and stop-shims t)
       initiator)
      (while (and (eq state :pending) (< (float-time) deadline))
        (agent-repl--runtime-pump-events 0.05))
      (pcase state
        (:complete
         (agent-repl--log nil
                          "runtime-restart-await: complete stop-shims=%s elapsed=%.3fs"
                          (if stop-shims "t" "nil") (- (float-time) started))
         "runtime-restart-complete")
        (:failed
         (agent-repl--error nil
                            "runtime-restart-await: FAILED stop-shims=%s elapsed=%.3fs detail=%s"
                            (if stop-shims "t" "nil")
                            (- (float-time) started) failure))
        (_
         (agent-repl--error nil
                            "runtime-restart-await: TIMEOUT stop-shims=%s timeout=%.3fs elapsed=%.3fs state=%S"
                            (if stop-shims "t" "nil") limit
                            (- (float-time) started) state))))))

(defun agent-repl--runtime-startup-prepare (on-success on-failure)
  "Prepare runtime services and daemon readiness before snapshot restoration.
Batch loads intentionally inhibit automatic backend startup; outside
that explicit no-runtime context, a failed readiness check signals
and leaves snapshot restoration entirely untouched."
  (unless (and (functionp on-success) (functionp on-failure))
    (error "agent-repl: runtime startup requires callable continuations"))
  (if (agent-repl--frontend-init-inhibited-p)
      (progn
        (agent-repl--log nil
                         "runtime-startup-prepare: inhibited noninteractive=%s"
                         noninteractive)
        (funcall on-success))
    (agent-repl--log nil "runtime-startup-prepare: beginning before snapshot restore")
    (agent-repl--runtime-prepare nil on-success on-failure)))

(provide 'services)

;;; services.el ends here
