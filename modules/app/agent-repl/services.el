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
(declare-function agent-repl--frontend-all-turn-active-session-ids "frontend-client" ())
(declare-function agent-repl--frontend-artifact-exists-p "daemon" (path))
(declare-function agent-repl--frontend-bounce-after-build "daemon" (&optional preflight stop-shims on-complete))
(declare-function agent-repl--frontend-build-if-stale "daemon" (&optional force))
(declare-function agent-repl--frontend-build-targets-if-stale "daemon" (targets &optional force))
(declare-function agent-repl--frontend-init-inhibited-p "daemon" ())
(declare-function agent-repl--frontend-after-daemon-healthy "frontend-client" (on-success on-failure))
(declare-function agent-repl--frontend-after-ready "frontend-client" (on-ready on-failure &optional ws))
(declare-function agent-repl--frontend-rebind-workspaces-after-restart "frontend-client" (&optional on-success on-failure))
(declare-function agent-repl--frontend-runtime-bounce-preflight-async "daemon" (callback))
(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--log-verbose "core" (ws fmt &rest args))

(defcustom agent-repl-shim-services-launchctl-program "launchctl"
  "Program used to inspect and kickstart the launchd-managed shim services."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-shim-store-ready-timeout 15.0
  "Seconds to wait for shim-store's socket after its launchd kickstart."
  :type 'number
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
  (agent-repl--shim-services-launchctl "kickstart" agent-repl--shim-store-label)
  (agent-repl--shim-store-after-ready
   (lambda ()
     (condition-case err
         (progn
           (agent-repl--shim-service-record-deployed agent-repl--shim-store-binary)
           (agent-repl--shim-services-launchctl "kickstart" agent-repl--shim-sidecar-label)
           (agent-repl--shim-service-record-deployed agent-repl--shim-sidecar-binary)
           (agent-repl--log nil "shim-services bounce complete: store=%s sidecar=%s"
                            agent-repl--shim-store-label agent-repl--shim-sidecar-label)
           (funcall on-success))
       (error
        (let ((detail (error-message-string err)))
          (agent-repl--log nil "shim-services bounce FAILED after store readiness: %s" detail)
          (funcall on-failure detail)))))
   (lambda (detail)
     (agent-repl--log nil "shim-services bounce FAILED before sidecar: %s" detail)
     (message "agent-repl: shim service bounce failed: %s" detail)
     (funcall on-failure detail)))
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
         (bounce-runtime (daemon-state)
           (condition-case err
               (progn
                 (let ((busy (agent-repl--frontend-all-turn-active-session-ids)))
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
                    (agent-repl--frontend-bounce-after-build
                     daemon-state stop-shims
                     (lambda (_started) (after-daemon-bounce))))
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

(defun agent-repl--runtime-startup-prepare (on-success on-failure)
  "Prepare runtime services and daemon readiness before snapshot restoration.
Batch and sandbox loads intentionally inhibit automatic backend startup;
outside those explicit no-runtime contexts, a failed readiness check signals
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
