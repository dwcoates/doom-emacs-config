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
(declare-function agent-repl--do-log "core" (ws fmt args &optional error-p))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function agent-repl--exit-code-sentinel "core" (callback))
(declare-function agent-repl--global-state-file "core" (relative))
(declare-function agent-repl--frontend-all-turn-active-session-ids "frontend-client" ())
(declare-function agent-repl--frontend-artifact-exists-p "daemon" (path))
(declare-function agent-repl--frontend-bounce-after-build "daemon" (&optional preflight stop-shims on-complete))
(declare-function agent-repl--frontend-build-if-stale "daemon" (force on-success on-failure))
(declare-function agent-repl--frontend-build-targets-if-stale "daemon" (targets force on-success on-failure))
(declare-function agent-repl--frontend-init-inhibited-p "daemon" ())
(declare-function agent-repl--frontend-after-daemon-healthy "frontend-client" (on-success on-failure))
(declare-function agent-repl--frontend-after-ready "frontend-client" (on-ready on-failure &optional ws))
(declare-function agent-repl--frontend-rebind-workspaces-after-restart "frontend-client" (&optional on-success on-failure))
(declare-function agent-repl--frontend-runtime-bounce-preflight-async "daemon" (callback))
(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--log-verbose "core" (ws fmt &rest args))

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

(defcustom agent-repl-runtime-restart-dispatch-timeout 300.0
  "Seconds a dispatched deployment restart may run before it is failed.
The ordinary interactive restart remains asynchronous and untimed.  This
budget bounds `agent-repl-runtime-restart-dispatch': a coordinator that
has not settled within it publishes a `failed' completion artifact, so a
deployment caller can never wait forever nor mistake a hung restart for a
finished one."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-runtime-restart-health-timeout 60.0
  "Seconds a dispatched deployment restart allows for daemon health.
The replacement daemon can become ready before Emacs finishes applying its
startup snapshot.  At production roster sizes that snapshot work can delay
the correlated `daemonHealth' response beyond the ordinary command deadline.
This deployment-only budget is raised for the duration of a dispatched
restart and restored when it settles; ordinary frontend commands retain
their shorter deadlines."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-runtime-restart-ready-attempts 150
  "Readiness attempts available to a dispatched deployment restart.
Each attempt uses the frontend readiness poll's fixed 0.2 second interval.
Large durable registries can make replacement startup exceed the ordinary
interactive budget; this deployment-only value remains bounded below the
dispatch timeout and is restored when the dispatch settles."
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

(defun agent-repl--launchctl-call (args callback)
  "External-boundary wrapper: run launchctl with ARGS asynchronously.
Output is captured into `agent-repl--shim-services-buffer'; CALLBACK
receives the integer exit code once the process terminates.

THERE IS NO BLOCKING SPELLING.  This wrapper used to be a `call-process',
and each of its two kickstarts held the editor's main thread for the
whole of a launchd service restart (measured at 5.01s and 4.01s, proven
by the main-thread heartbeat ticks that never fired across them).  The
continuation is a required argument rather than an optional one precisely
so no future restart step can be written that blocks."
  (make-process ;; ALLOW-EXTERNAL-BOUNDARY
   :name "agent-repl-launchctl"
   :command (cons agent-repl-shim-services-launchctl-program args)
   :connection-type 'pipe
   :noquery t
   :buffer (get-buffer-create agent-repl--shim-services-buffer)
   :sentinel (agent-repl--exit-code-sentinel callback)))

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

(defun agent-repl--shim-services-launchctl (verb label on-success on-failure)
  "Run launchctl VERB for LABEL, logging all inputs and captured output.
ON-SUCCESS is called with no arguments once launchctl exits zero;
ON-FAILURE receives the diagnostic string for any other exit.

An unknown VERB is an invariant violation rather than an expected runtime
failure — nothing outside this file chooses the verb — so it still
signals through the canonical logging helper instead of degrading into
ON-FAILURE."
  (unless (and (functionp on-success) (functionp on-failure))
    (error "agent-repl: launchctl requires callable continuations"))
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
                                verb label)))))
    (agent-repl--launchctl-call
     args
     (lambda (exit-code)
       (let ((output (agent-repl--shim-services-output)))
         (agent-repl--log nil
                          "shim-services launchctl: verb=%s label=%s service=%s exit=%S output=%s"
                          verb label service exit-code
                          (if (string-empty-p output) "<empty>" output))
         (if (eq exit-code 0)
             (funcall on-success)
           (funcall on-failure
                    (format "launchd service %s failed `%s' (exit %s): %s"
                            label verb exit-code
                            (if (string-empty-p output) "<no output>" output))))))))
  :pending)

(defun agent-repl--shim-services-assert-launchd-loaded (on-success on-failure)
  "Fail before mutation unless launchd owns both required service jobs.
The two `print' probes run in sequence through their continuations, so a
missing store job never lets the sidecar probe start."
  (unless (and (functionp on-success) (functionp on-failure))
    (error "agent-repl: launchd preflight requires callable continuations"))
  (agent-repl--shim-services-launchctl
   "print" agent-repl--shim-store-label
   (lambda ()
     (agent-repl--shim-services-launchctl
      "print" agent-repl--shim-sidecar-label
      (lambda ()
        (agent-repl--log nil
                         "shim-services preflight: launchd jobs loaded store=%s sidecar=%s"
                         agent-repl--shim-store-label agent-repl--shim-sidecar-label)
        (funcall on-success))
      on-failure))
   on-failure)
  :pending)

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
  (cl-labels
      ((fail (detail)
         (agent-repl--log nil "shim-services bounce FAILED: %s" detail)
         (message "agent-repl: shim service bounce failed: %s" detail)
         (funcall on-failure detail))
       (after-preflight ()
         (agent-repl--frontend-build-targets-if-stale
          '("store" "sidecar") nil
          (lambda (result)
            (agent-repl--log nil
                             "shim-services build-and-bounce: target build completed targets=%S result=%S"
                             '("store" "sidecar") result)
            (after-build))
          #'fail))
       (after-build ()
         ;; The build claiming success while an artifact is missing is an
         ;; invariant violation, not an expected build failure, so it keeps
         ;; signalling rather than routing through the failure continuation.
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
         (agent-repl--shim-services-launchctl
          "kickstart" agent-repl--shim-store-label
          (lambda ()
            (agent-repl--shim-store-after-ready #'after-store-ready #'fail))
          #'fail))
       (after-store-ready ()
         (condition-case err
             (progn
               (agent-repl--shim-service-record-deployed agent-repl--shim-store-binary)
               (agent-repl--shim-services-launchctl
                "kickstart" agent-repl--shim-sidecar-label
                #'after-sidecar-kickstart #'fail))
           (error (fail (error-message-string err)))))
       (after-sidecar-kickstart ()
         (condition-case err
             (progn
               (agent-repl--shim-service-record-deployed agent-repl--shim-sidecar-binary)
               (agent-repl--log nil "shim-services bounce complete: store=%s sidecar=%s"
                                agent-repl--shim-store-label agent-repl--shim-sidecar-label)
               (funcall on-success))
           (error (fail (error-message-string err))))))
    (if preflight-complete
        (progn
          (agent-repl--log nil
                           "shim-services build-and-bounce: using coordinator launchd preflight")
          (after-preflight))
      (agent-repl--log nil
                       "shim-services build-and-bounce: validating launchd jobs before build")
      (agent-repl--shim-services-assert-launchd-loaded #'after-preflight #'fail)))
  :pending)

(defun agent-repl--runtime-prepare (rebind on-success on-failure &optional stop-shims)
  "Asynchronously bounce dependencies, verify the daemon, and optionally REBIND.
ON-SUCCESS runs only after every requested stage completes.  ON-FAILURE
receives the first diagnostic and no later stage starts."
  (agent-repl--assert-main-thread "runtime-restart")
  (unless (and (functionp on-success) (functionp on-failure))
    (error "agent-repl: runtime preparation requires callable continuations"))
  (let ((started (float-time)) settled)
    (cl-labels
        ((fail (detail)
           (unless settled
             (setq settled t)
             (agent-repl--log nil
                              "runtime-prepare: FAILED rebind=%s elapsed=%.3fs detail=%s"
                              (if rebind "t" "nil") (- (float-time) started) detail)
             (message "agent-repl: runtime preparation failed: %s" detail)
             (funcall on-failure detail)))
         (complete (&optional rebound)
           (unless settled
             (setq settled t)
             (agent-repl--log nil
                              "runtime-prepare complete: mode=%s rebound=%S elapsed=%.3fs launchd-store=%s launchd-sidecar=%s"
                              (if rebind "restart" "startup") rebound
                              (- (float-time) started)
                              agent-repl--shim-store-label agent-repl--shim-sidecar-label)
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
         (after-services (daemon-state)
           (agent-repl--shim-services-build-and-bounce
            t
            (lambda ()
              (agent-repl--frontend-bounce-after-build
               daemon-state stop-shims
               (lambda (_started) (after-daemon-bounce))))
            #'fail))
         (bounce-runtime (daemon-state)
           ;; Every stage below is continuation-passing now that the launchd
           ;; and build boundaries are asynchronous.  The `condition-case'
           ;; stays for the synchronous parts (the turn probe and the
           ;; invariant assertions inside the stages); each asynchronous stage
           ;; routes its own failure into `fail' directly.
           (condition-case err
               (progn
                 (let ((busy (agent-repl--frontend-all-turn-active-session-ids)))
                   (agent-repl--log nil
                                    "runtime-restart preflight: daemon-state=%S busy=%S store=%s sidecar=%s"
                                    daemon-state busy agent-repl--shim-store-label
                                    agent-repl--shim-sidecar-label)
                   (when busy
                     (error "runtime restart refused: turn in flight in %s" busy)))
                 (agent-repl--shim-services-assert-launchd-loaded
                  (lambda ()
                    (agent-repl--frontend-build-if-stale
                     nil
                     (lambda (build-result)
                       (agent-repl--log nil
                                        "runtime-prepare: frontend build completed result=%S"
                                        build-result)
                       (after-services daemon-state))
                     #'fail))
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
(defun agent-repl-runtime-restart (&optional stop-shims)
  "Rebuild, bounce, verify, then rebind the complete agent-repl runtime.
Refuses before any build or bounce when any daemon workspace reports an
active turn.

STOP-SHIMS (the interactive prefix argument) asks the outgoing daemon to
stop its session shims rather than leave them running for the replacement
to reattach to.  The default PRESERVES them; see
`agent-repl--runtime-prepare'."
  (interactive "P")
  (agent-repl--log nil
                   "runtime-restart command: invoked interactive=%s stop-shims=%s"
                   (if (called-interactively-p 'interactive) "t" "nil")
                   (if stop-shims "t" "nil"))
  (agent-repl--runtime-prepare
   t
   (lambda () (message "agent-repl: runtime restart complete"))
   (lambda (detail)
     (agent-repl--log nil "runtime-restart command: FAILED detail=%s" detail))
   (and stop-shims t)))

;;;; ---- Deployment dispatch and its completion artifact -------------------
;;
;; WHY THE DEPLOYMENT SURFACE NO LONGER AWAITS IN EMACS.
;;
;; `bin/deploy-all.sh' drives the LIVE Emacs over `emacsclient --eval'.  The
;; previous spelling ran the asynchronous coordinator and then spun on
;; `accept-process-output', which services process output but never keyboard
;; input — so a deploy froze the editor the user was sitting in for the whole
;; restart.  Making the launchd and build boundaries asynchronous removes the
;; multi-second `call-process' stalls but NOT that freeze: the await loop held
;; input for the restart's entire duration by construction.
;;
;; So the eval no longer awaits anything.  It returns the request identity
;; immediately, the coordinator writes its terminal result to a completion
;; artifact from its own continuations, and the deploy script waits on THAT
;; file.  The wait moved out of Emacs's main loop and into the shell, which is
;; the only participant that has nothing else to do.
;;
;; The failure semantics are unchanged: a coordinator failure or a timeout is
;; recorded through the canonical logging helper AND published as a `failed'
;; artifact carrying the exact detail, so the deploy still fails loudly and no
;; caller can mistake a dispatch for a completed deployment.

(defconst agent-repl--runtime-restart-result-subdir "restart"
  "State-dir subdirectory holding runtime-restart completion artifacts.")

(defvar agent-repl--runtime-restart-request-counter 0
  "Monotonic counter making each dispatched restart request id unique.")

(defvar agent-repl--runtime-restart-dispatch nil
  "Plist describing the runtime restart currently dispatched, or nil.
Keys: `:request-id', `:started', `:timeout', `:timer', and the
`:saved-health' / `:saved-ready' / `:saved-ack' values of the deployment
budget variables this dispatch raised.  Non-nil means a dispatch is in
flight; a second dispatch while one is outstanding is an invariant
violation, since the two would fight over those same budget variables.")

(defun agent-repl--runtime-restart-result-path (request-id)
  "Return the completion-artifact path for REQUEST-ID."
  (agent-repl--global-state-file
   (format "%s/%s.result"
           agent-repl--runtime-restart-result-subdir request-id)))

(defun agent-repl--runtime-restart-write-result (path text)
  "External-boundary wrapper: publish TEXT as the completion artifact PATH.
Written to a sibling temp file and renamed into place, so a reader can
never observe a half-written record — the deploy script polls this file
and must see either nothing or a complete one."
  (make-directory (file-name-directory path) t) ;; ALLOW-EXTERNAL-BOUNDARY
  (let ((tmp (concat path ".partial")))
    (with-temp-file tmp (insert text))
    (rename-file tmp path t)))

(defun agent-repl--runtime-restart-publish (request-id status detail)
  "Publish STATUS and DETAIL for REQUEST-ID's completion artifact."
  (let ((path (agent-repl--runtime-restart-result-path request-id)))
    (agent-repl--log nil
                     "runtime-restart-dispatch: publishing request-id=%s status=%s path=%s detail=%s"
                     request-id status path (or detail "<none>"))
    (agent-repl--runtime-restart-write-result
     path
     (format "request-id=%s\nstatus=%s\ndetail=%s\n"
             request-id status (or detail "")))))

(defun agent-repl--runtime-restart-settle (request-id status detail)
  "Settle the dispatched restart REQUEST-ID with STATUS and DETAIL.
Cancels the timeout timer, restores the deployment budget variables this
dispatch raised, publishes the completion artifact, and — for a non
`complete' STATUS — records the failure through the canonical logging
helper.  Later continuations from the same dispatch find no state and are
dropped, so success and timeout cannot both settle it."
  (let ((state agent-repl--runtime-restart-dispatch))
    (if (not (and state (equal (plist-get state :request-id) request-id)))
        (agent-repl--log nil
                         "runtime-restart-dispatch: ignoring late %s for request-id=%s (current=%S)"
                         status request-id
                         (and state (plist-get state :request-id)))
      (setq agent-repl--runtime-restart-dispatch nil)
      (when (timerp (plist-get state :timer))
        (cancel-timer (plist-get state :timer)))
      (setq agent-repl-frontend-health-timeout (plist-get state :saved-health)
            agent-repl-frontend-ready-attempts (plist-get state :saved-ready)
            agent-repl-uds-command-ack-deadline (plist-get state :saved-ack))
      (let ((elapsed (- (float-time) (plist-get state :started))))
        (if (eq status :complete)
            (agent-repl--log nil
                             "runtime-restart-dispatch: complete request-id=%s elapsed=%.3fs"
                             request-id elapsed)
          (agent-repl--do-log
           nil
           "runtime-restart-dispatch: %s request-id=%s elapsed=%.3fs timeout=%.3fs detail=%s"
           (list (if (eq status :timeout) "TIMEOUT" "FAILED")
                 request-id elapsed (plist-get state :timeout) detail)
           nil)))
      (agent-repl--runtime-restart-publish
       request-id
       (if (eq status :complete) "complete" "failed")
       detail))))

(defun agent-repl-runtime-restart-dispatch (&optional stop-shims timeout)
  "Dispatch a coordinated runtime restart and return its request identity.
STOP-SHIMS has the same meaning as in `agent-repl-runtime-restart'.
TIMEOUT, when non-nil, overrides `agent-repl-runtime-restart-dispatch-timeout'.

RETURNS IMMEDIATELY with the string `runtime-restart-dispatched:<id>'.
The terminal result lands in the completion artifact at
`agent-repl--runtime-restart-result-path', which is written once with
`status=pending' before this returns and rewritten exactly once more with
`status=complete' or `status=failed' when the coordinator settles.  A
deployment caller waits on that artifact; nothing waits in Emacs.

This surface is reserved for deployment orchestration.  The ordinary
interactive restart is `agent-repl-runtime-restart'."
  (let ((limit (or timeout agent-repl-runtime-restart-dispatch-timeout)))
    (unless (and (numberp limit) (> limit 0))
      (agent-repl--error nil
                         "runtime-restart-dispatch: invalid timeout=%S stop-shims=%s"
                         limit (if stop-shims "t" "nil")))
    (unless (and (numberp agent-repl-runtime-restart-health-timeout)
                 (> agent-repl-runtime-restart-health-timeout 0))
      (agent-repl--error
       nil
       "runtime-restart-dispatch: invalid health-timeout=%S stop-shims=%s"
       agent-repl-runtime-restart-health-timeout (if stop-shims "t" "nil")))
    (unless (and (integerp agent-repl-runtime-restart-ready-attempts)
                 (> agent-repl-runtime-restart-ready-attempts 0))
      (agent-repl--error
       nil
       "runtime-restart-dispatch: invalid ready-attempts=%S stop-shims=%s"
       agent-repl-runtime-restart-ready-attempts (if stop-shims "t" "nil")))
    (when agent-repl--runtime-restart-dispatch
      (agent-repl--error
       nil
       "runtime-restart-dispatch: REFUSING concurrent dispatch outstanding=%s stop-shims=%s"
       (plist-get agent-repl--runtime-restart-dispatch :request-id)
       (if stop-shims "t" "nil")))
    (let* ((request-id (format "%d-%d" (emacs-pid)
                               (cl-incf agent-repl--runtime-restart-request-counter)))
           (health-limit
            (min agent-repl-runtime-restart-health-timeout (* limit 0.8)))
           (ready-attempts
            (min agent-repl-runtime-restart-ready-attempts
                 (max 1 (floor (/ (* limit 0.8) 0.2))))))
      ;; The deployment budget is raised GLOBALLY rather than through a `let':
      ;; every continuation now runs from a timer or a process sentinel, long
      ;; after any dynamic binding this function established would have
      ;; unwound.  `agent-repl--runtime-restart-settle' restores all three,
      ;; and the concurrent-dispatch refusal above is what makes that
      ;; save/restore pair a single owner rather than a race.
      (setq agent-repl--runtime-restart-dispatch
            (list :request-id request-id
                  :started (float-time)
                  :timeout limit
                  :timer nil
                  :saved-health agent-repl-frontend-health-timeout
                  :saved-ready agent-repl-frontend-ready-attempts
                  :saved-ack agent-repl-uds-command-ack-deadline))
      (setq agent-repl-frontend-health-timeout health-limit
            agent-repl-frontend-ready-attempts ready-attempts
            agent-repl-uds-command-ack-deadline health-limit)
      (agent-repl--log nil
                       "runtime-restart-dispatch: beginning request-id=%s stop-shims=%s timeout=%.3fs health-timeout=%.3fs ready-attempts=%d"
                       request-id (if stop-shims "t" "nil") limit health-limit
                       ready-attempts)
      (agent-repl--runtime-restart-publish request-id "pending" nil)
      ;; The timer is recorded only while THIS dispatch is still the current
      ;; one: a seam whose timer fires immediately has already settled and
      ;; cleared the state, and writing into it afterwards would resurrect a
      ;; dispatch that no longer exists.
      (let ((timer (agent-repl--shim-services-run-timer
                    limit
                    (lambda ()
                      (agent-repl--runtime-restart-settle
                       request-id :timeout
                       (format "runtime restart did not settle within %.3fs"
                               limit))))))
        (when (equal request-id
                     (plist-get agent-repl--runtime-restart-dispatch :request-id))
          (setq agent-repl--runtime-restart-dispatch
                (plist-put agent-repl--runtime-restart-dispatch :timer timer))))
      (agent-repl--runtime-prepare
       t
       (lambda () (agent-repl--runtime-restart-settle request-id :complete nil))
       (lambda (detail)
         (agent-repl--runtime-restart-settle request-id :failed detail))
       (and stop-shims t))
      (format "runtime-restart-dispatched:%s" request-id))))

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
