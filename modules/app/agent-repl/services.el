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
(declare-function agent-repl--frontend-wait-ready "frontend-client" ())
(declare-function agent-repl--log "core" (ws fmt &rest args))

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
    (agent-repl--shim-service-write-stamp stamp digest)
    (agent-repl--log nil
                     "shim-services deployed stamp: binary=%s stamp=%s sha256=%s"
                     binary stamp digest)))

(defun agent-repl--shim-store-wait-ready ()
  "Wait for the newly kickstarted store socket, failing on timeout."
  (let ((deadline (+ (float-time) agent-repl-shim-store-ready-timeout))
        (ready (agent-repl--shim-store-socket-present-p)))
    (while (and (not ready)
                (< (float-time) deadline))
      (sleep-for 0.1)
      (setq ready (agent-repl--shim-store-socket-present-p)))
    (unless ready
      (agent-repl--error nil
                         "shim-store socket %s absent after %.1fs"
                         agent-repl--shim-store-socket
                         agent-repl-shim-store-ready-timeout))
    (agent-repl--log nil
                     "shim-services store ready: socket=%s timeout=%.1fs"
                     agent-repl--shim-store-socket
                     agent-repl-shim-store-ready-timeout)
    t))

(defun agent-repl--shim-services-build-and-bounce (&optional preflight-complete)
  "Build stale store/sidecar binaries and kickstart both launchd jobs.
The store is kickstarted and confirmed ready before the sidecar is touched.
Both deployed-binary stamps are written only after their corresponding
kickstart succeeds.  PREFLIGHT-COMPLETE means the coordinator already
validated both jobs before building any runtime artifact."
  (unless preflight-complete
    (agent-repl--shim-services-assert-launchd-loaded))
  (agent-repl--log nil
                   "shim-services build-and-bounce: preflight-complete=%s"
                   (if preflight-complete "t" "nil"))
  (agent-repl--frontend-build-targets-if-stale '("store" "sidecar"))
  (unless (and (agent-repl--frontend-artifact-exists-p
                agent-repl--shim-store-binary)
               (agent-repl--frontend-artifact-exists-p
                agent-repl--shim-sidecar-binary))
    (agent-repl--error nil
                       "shim service build completed without both binaries: store=%s sidecar=%s"
                       agent-repl--shim-store-binary
                       agent-repl--shim-sidecar-binary))
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

(defvar agent-repl--runtime-startup-bounce-scheduled nil
  "Non-nil once this Emacs scheduled its post-restore runtime bounce.")

(defvar agent-repl--runtime-startup-bounce-completed nil
  "Non-nil once this Emacs completed its post-restore runtime bounce.")

(defun agent-repl-runtime-restart ()
  "Rebuild stale artifacts and bounce the complete agent-repl runtime.
launchd retains ownership of store and sidecar.  The daemon restart
terminates its old session shims, then the normal rebind path resumes each
durable conversation with a fresh shim.  Refuses before any build or bounce
when any daemon workspace reports an active turn."
  (interactive)
  (agent-repl--assert-main-thread "runtime-restart")
  (let* ((daemon-state (agent-repl--frontend-runtime-bounce-preflight))
         (daemon-present (memq daemon-state '(:tracked :responsive))))
    ;; Pump the authoritative snapshot before checking active turns.  This
    ;; includes states for workspace paths not yet restored into Emacs.
    (when daemon-present
      (agent-repl--frontend-wait-ready))
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
    (agent-repl--frontend-build-if-stale nil)
    (agent-repl--shim-services-build-and-bounce t)
    (agent-repl--frontend-bounce-after-build daemon-state)
    (let ((rebound (agent-repl--frontend-rebind-workspaces-after-restart)))
      (setq agent-repl--runtime-startup-bounce-completed t)
      (agent-repl--log nil
                       "runtime-restart complete: rebound=%d launchd-store=%s launchd-sidecar=%s"
                       rebound agent-repl--shim-store-label
                       agent-repl--shim-sidecar-label)
      (when (called-interactively-p 'interactive)
        (message "agent-repl runtime restarted; rebound %d workspace%s"
                 rebound (if (= rebound 1) "" "s")))
      rebound)))

(defun agent-repl--run-runtime-startup-bounce ()
  "Run the coordinated runtime bounce scheduled after snapshot restoration."
  (if (agent-repl--frontend-init-inhibited-p)
      (agent-repl--log nil
                       "runtime-startup-bounce: inhibited noninteractive=%s"
                       noninteractive)
    (agent-repl--log nil "runtime-startup-bounce: beginning")
    (agent-repl-runtime-restart)))

(defun agent-repl--schedule-runtime-startup-bounce ()
  "Schedule one coordinated runtime bounce after startup restoration."
  (unless (or agent-repl--runtime-startup-bounce-scheduled
              agent-repl--runtime-startup-bounce-completed)
    (setq agent-repl--runtime-startup-bounce-scheduled t)
    (agent-repl--log nil
                     "runtime-startup-bounce: scheduled after snapshot restore")
    ;; Let the snapshot loader unwind fully before a restart rebinds its
    ;; sessions and webviews.
    (run-at-time 0 nil #'agent-repl--run-runtime-startup-bounce)))

(provide 'services)

;;; services.el ends here
