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
(declare-function agent-repl--frontend-bounce-after-build "daemon" (&optional preflight))
(declare-function agent-repl--frontend-build-if-stale "daemon" (&optional force))
(declare-function agent-repl--frontend-build-targets-if-stale "daemon" (targets &optional force))
(declare-function agent-repl--frontend-init-inhibited-p "daemon" ())
(declare-function agent-repl--frontend-rebind-workspaces-after-restart "frontend-client" ())
(declare-function agent-repl--frontend-runtime-bounce-preflight "daemon" ())
(declare-function agent-repl--frontend-wait-daemon-healthy "frontend-client" ())
(declare-function agent-repl--frontend-wait-ready "frontend-client" ())
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

(defun agent-repl--shim-store-wait-ready ()
  "Wait for the newly kickstarted store socket, failing on timeout."
  (let* ((started-at (float-time))
         (deadline (+ started-at agent-repl-shim-store-ready-timeout))
         (ready (agent-repl--shim-store-socket-present-p)))
    (agent-repl--log nil
                     "shim-services store readiness: socket=%s timeout=%.1fs initial-ready=%s"
                     agent-repl--shim-store-socket
                     agent-repl-shim-store-ready-timeout
                     (if ready "t" "nil"))
    (while (and (not ready)
                (< (float-time) deadline))
      (sleep-for 0.1)
      (setq ready (agent-repl--shim-store-socket-present-p))
      ;; This poll runs up to ten times per second while a service is starting.
      (agent-repl--log-verbose nil
                               "shim-services store readiness poll: socket=%s ready=%s elapsed=%.3fs remaining=%.3fs"
                               agent-repl--shim-store-socket
                               (if ready "t" "nil")
                               (- (float-time) started-at)
                               (max 0.0 (- deadline (float-time)))))
    (unless ready
      (agent-repl--log nil
                       "shim-services store readiness: timeout socket=%s elapsed=%.3fs timeout=%.1fs"
                       agent-repl--shim-store-socket
                       (- (float-time) started-at)
                       agent-repl-shim-store-ready-timeout)
      (agent-repl--error nil
                         "shim-store socket %s absent after %.1fs"
                         agent-repl--shim-store-socket
                         agent-repl-shim-store-ready-timeout))
    (agent-repl--log nil
                     "shim-services store ready: socket=%s elapsed=%.3fs timeout=%.1fs"
                     agent-repl--shim-store-socket
                     (- (float-time) started-at)
                     agent-repl-shim-store-ready-timeout)
    t))

(defun agent-repl--shim-services-build-and-bounce (&optional preflight-complete)
  "Build stale store/sidecar binaries and kickstart both launchd jobs.
The store is kickstarted and confirmed ready before the sidecar is touched.
Both deployed-binary stamps are written only after their corresponding
kickstart succeeds.  PREFLIGHT-COMPLETE means the coordinator already
validated both jobs before building any runtime artifact."
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
  (agent-repl--shim-services-launchctl "kickstart"
                                       agent-repl--shim-store-label)
  (agent-repl--shim-store-wait-ready)
  (agent-repl--shim-service-record-deployed agent-repl--shim-store-binary)
  (agent-repl--shim-services-launchctl "kickstart"
                                       agent-repl--shim-sidecar-label)
  (agent-repl--shim-service-record-deployed agent-repl--shim-sidecar-binary)
  (agent-repl--log nil
                   "shim-services bounce complete: store=%s sidecar=%s"
                   agent-repl--shim-store-label agent-repl--shim-sidecar-label)
  t)

(defun agent-repl--runtime-prepare (rebind)
  "Bounce backend dependencies, require daemon health, and optionally REBIND.
REBIND is non-nil only for an explicit runtime restart with workspaces
already loaded.  Startup passes nil so the backend becomes healthy before
the snapshot reader can establish, restore, or render any workspace."
  (agent-repl--assert-main-thread "runtime-restart")
  (let* ((daemon-state (agent-repl--frontend-runtime-bounce-preflight))
         (daemon-present (memq daemon-state '(:tracked :responsive))))
    (agent-repl--log nil
                     "runtime-prepare: beginning rebind=%s daemon-state=%S daemon-present=%s"
                     (if rebind "t" "nil") daemon-state
                     (if daemon-present "t" "nil"))
    ;; Pump the authoritative snapshot before checking active turns.  This
    ;; includes states for workspace paths not yet restored into Emacs.
    (if daemon-present
        (progn
          (agent-repl--log nil
                           "runtime-prepare: waiting for pre-existing daemon snapshot before turn preflight")
          (agent-repl--frontend-wait-ready))
      (agent-repl--log nil
                       "runtime-prepare: daemon absent; skipping pre-bounce snapshot wait"))
    (let ((busy (agent-repl--frontend-all-turn-active-session-ids)))
      (agent-repl--log nil
                       "runtime-restart preflight: daemon-state=%S daemon-present=%s busy=%S store=%s sidecar=%s"
                       daemon-state (if daemon-present "t" "nil") busy
                       agent-repl--shim-store-label
                       agent-repl--shim-sidecar-label)
      (when busy
        (agent-repl--error nil
                           "runtime restart refused: turn in flight in %s"
                           busy)))
    ;; Validate both launchd jobs before any build changes installed files.
    (agent-repl--shim-services-assert-launchd-loaded)
    (let ((frontend-build-result (agent-repl--frontend-build-if-stale nil)))
      (agent-repl--log nil
                       "runtime-prepare: frontend build completed result=%S"
                       frontend-build-result))
    (agent-repl--shim-services-build-and-bounce t)
    (agent-repl--frontend-bounce-after-build daemon-state)
    ;; The new daemon can accept a UDS connection before it can service
    ;; requests.  Require both the link/snapshot readiness and the daemon's
    ;; correlated initialization readiness before a caller can continue.
    (agent-repl--frontend-wait-ready)
    (agent-repl--frontend-wait-daemon-healthy)
    (if rebind
        (let ((rebound (agent-repl--frontend-rebind-workspaces-after-restart)))
          (agent-repl--log nil
                           "runtime-prepare complete: mode=restart rebound=%d launchd-store=%s launchd-sidecar=%s"
                           rebound agent-repl--shim-store-label
                           agent-repl--shim-sidecar-label)
          rebound)
      (agent-repl--log nil
                       "runtime-prepare complete: mode=startup launchd-store=%s launchd-sidecar=%s"
                       agent-repl--shim-store-label
                       agent-repl--shim-sidecar-label)
      t)))

(defun agent-repl-runtime-restart ()
  "Rebuild, bounce, verify, then rebind the complete agent-repl runtime.
Refuses before any build or bounce when any daemon workspace reports an
active turn."
  (interactive)
  (agent-repl--log nil
                   "runtime-restart command: invoked interactive=%s"
                   (if (called-interactively-p 'interactive) "t" "nil"))
  (let ((rebound (agent-repl--runtime-prepare t)))
    (when (called-interactively-p 'interactive)
      (agent-repl--log nil
                       "runtime-restart command: presenting interactive completion rebound=%d"
                       rebound)
      (message "agent-repl runtime restarted; rebound %d workspace%s"
               rebound (if (= rebound 1) "" "s")))
    rebound))

(defun agent-repl--runtime-startup-prepare ()
  "Prepare runtime services and daemon readiness before snapshot restoration.
Batch and sandbox loads intentionally inhibit automatic backend startup;
outside those explicit no-runtime contexts, a failed readiness check signals
and leaves snapshot restoration entirely untouched."
  (if (agent-repl--frontend-init-inhibited-p)
      (agent-repl--log nil
                       "runtime-startup-prepare: inhibited noninteractive=%s"
                       noninteractive)
    (agent-repl--log nil "runtime-startup-prepare: beginning before snapshot restore")
    (agent-repl--runtime-prepare nil)))

(provide 'services)

;;; services.el ends here
