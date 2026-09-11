;;; services.el --- launchd-managed store and sidecar lifecycle -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns the launchd integration boundary for shim-store and the Claude
;; transcript sidecar, and the coordinated runtime bounce that rebuilds
;; the stack, restarts those two services, and hands the daemon back to
;; daemon.el.  Store always comes up before sidecar.
;;
;; THE DEPLOYED-FINGERPRINT STAMP (`.<name>.deployed', the same file
;; bin/lib-deploy-stamp.sh owns) is the SOLE kickstart authority here: a
;; deploy that already bounced a service and stamped it must not bounce it
;; a second time when it reaches this path through the runtime restart,
;; because every extra store bounce is another outage every live shim's
;; producer connection has to survive.
;;
;; WHAT THIS FILE NO LONGER DOES.  The daemon's readiness, its socket, its
;; expected-restart window, its retained view and its workspace rebinding
;; were all machinery of the old UDS transport and are gone: the daemon
;; publishes `daemon.addr' when it is ready to serve, daemon-link.el
;; reconnects on its own, and every workspace re-registers on link-up
;; because registration is idempotent by dir.  The only readiness this
;; file still waits on is the STORE's own socket, which is genuinely its
;; business: the sidecar must not come up against a store that is not
;; serving.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--assert-main-thread "core" (what))
(declare-function agent-repl--fatal "core" (ws fmt &rest args))
(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--log-verbose "core" (ws fmt &rest args))
(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--warn "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function agent-repl--backend-phase "core" (ws fmt &rest args))
(declare-function agent-repl--backend-output-tail "core" (output &optional lines))
(declare-function agent-repl--logfile-path "core" ())
(declare-function agent-repl--make-latch "core" ())
(declare-function agent-repl--latch-claim "core" (latch))
(declare-function agent-repl--latch-set-timer "core" (latch key timer))

(declare-function agent-repl--frontend-artifact-exists-p "daemon" (path))
(declare-function agent-repl-daemon--build "daemon" (targets continuation))
(declare-function agent-repl-daemon-ensure "daemon" (&optional on-ready))
(declare-function agent-repl-frontend-daemon-stop "daemon" (&optional on-done))
(declare-function agent-repl-link-teardown "daemon-link" ())

(defcustom agent-repl-shim-services-launchctl-program "launchctl"
  "Program used to drive launchd."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-shim-store-ready-timeout 15.0
  "Seconds to wait for the store's socket after a kickstart.
The sidecar dials that socket, so it must not be started against a store
that is not serving yet."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-runtime-restart-await-timeout 300.0
  "Seconds `agent-repl-runtime-restart-await' waits for the whole bounce."
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
  "The store's own socket, recreated when launchd starts it.
The one readiness signal this file still waits on — the store's, not the
daemon's.")

(defconst agent-repl--shim-services-buffer "*agent-repl-shim-services*"
  "Capture buffer for launchctl diagnostics.")

(defconst agent-repl--shim-service-build-targets '("store" "sidecar")
  "Build-script targets covering the two launchd-managed services.")

;;;; ---- External boundaries ----

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

;;;; ---- launchd ----

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
             (agent-repl--error '(:agent-repl-central "host service management spans workspaces")
                                "elisp.services.launchctl-invalid-verb verb=%S label=%s"
                                verb label)
             (agent-repl--fatal '(:agent-repl-central "host service management spans workspaces")
                                "invalid launchctl verb %S for service %s"
                                verb label))))
         (exit-code (agent-repl--launchctl-call args))
         (output (agent-repl--shim-services-output)))
    (agent-repl--log '(:agent-repl-central "host service management spans workspaces")
                     "elisp.services.launchctl verb=%s label=%s service=%s exit=%S output=%s"
                     verb label service exit-code
                     (if (string-empty-p output) "<empty>" output))
    (unless (eq exit-code 0)
      ;; `call-process' merged launchctl's stderr into the capture buffer, so
      ;; the refusal text is already in the durable record above.
      (agent-repl--backend-phase
       '(:agent-repl-central "host service management spans workspaces")
       "launchctl %s FAILED for %s (exit %s): %s — full output in %s"
       verb label exit-code (agent-repl--backend-output-tail output)
       (agent-repl--logfile-path))
      (agent-repl--fatal '(:agent-repl-central "host service management spans workspaces")
                         "launchd service %s failed `%s' (exit %s): %s"
                         label verb exit-code
                         (if (string-empty-p output) "<no output>" output)))
    t))

(defun agent-repl--shim-services-assert-launchd-loaded ()
  "Fail before mutation unless launchd owns both required service jobs."
  (agent-repl--shim-services-launchctl "print" agent-repl--shim-store-label)
  (agent-repl--shim-services-launchctl "print" agent-repl--shim-sidecar-label)
  (agent-repl--log '(:agent-repl-central "host service management spans workspaces")
                   "elisp.services.preflight store=%s sidecar=%s"
                   agent-repl--shim-store-label agent-repl--shim-sidecar-label)
  t)

;;;; ---- The deployed-fingerprint stamp ----

(defun agent-repl--shim-service-deployed-stamp (binary)
  "Return the deployed-fingerprint stamp path belonging to BINARY."
  (expand-file-name (format ".%s.deployed" (file-name-nondirectory binary))
                    agent-repl--shim-service-cache-bin))

(defun agent-repl--shim-service-read-stamp (path)
  "External-boundary wrapper: return the digest recorded at PATH, or nil."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents-literally path)
      (let ((value (string-trim (buffer-string))))
        (unless (string-empty-p value) value)))))

(defun agent-repl--shim-service-needs-bounce-p (binary)
  "Return non-nil when the running service is not executing installed BINARY.

The stamp is written at kickstart time, so a stamp equal to the installed
binary's digest means the live process is already serving that image and a
second kickstart would only widen the service outage.  A missing binary or
a missing/empty stamp is never \"in sync\" — there is nothing to be in sync
with, so bounce."
  (let* ((stamp (agent-repl--shim-service-deployed-stamp binary))
         (recorded (agent-repl--shim-service-read-stamp stamp))
         (stale (cond
                 ((not (agent-repl--frontend-artifact-exists-p binary)) t)
                 ((null recorded) t)
                 (t (not (equal recorded
                                (agent-repl--shim-service-file-sha256 binary)))))))
    (agent-repl--log '(:agent-repl-central "host service management spans workspaces")
                     "elisp.services.stamp binary=%s stamp=%s recorded=%s stale=%s"
                     binary stamp (or recorded "<absent>") (if stale "t" "nil"))
    stale))

(defun agent-repl--shim-service-record-deployed (binary)
  "Record that launchd has started the installed BINARY."
  (let* ((digest (agent-repl--shim-service-file-sha256 binary))
         (stamp (agent-repl--shim-service-deployed-stamp binary)))
    (agent-repl--log '(:agent-repl-central "host service management spans workspaces")
                     "elisp.services.stamp-recorded binary=%s stamp=%s sha256=%s"
                     binary stamp digest)
    (agent-repl--shim-service-write-stamp stamp digest)))

;;;; ---- Store readiness ----

(defun agent-repl--shim-store-after-ready (on-success on-failure)
  "Poll for the kickstarted store socket without blocking Emacs.
A TIMER poll, never a sleep."
  (unless (and (functionp on-success) (functionp on-failure))
    (agent-repl--fatal '(:agent-repl-central "host service management spans workspaces") "elisp.services.store-readiness needs callable continuations"))
  (let* ((started-at (float-time))
         (deadline (+ started-at agent-repl-shim-store-ready-timeout))
         (latch (agent-repl--make-latch)))
    (agent-repl--log '(:agent-repl-central "host service management spans workspaces")
                     "elisp.services.store-readiness socket=%s timeout=%.1f initial-ready=%s"
                     agent-repl--shim-store-socket
                     agent-repl-shim-store-ready-timeout
                     (if (agent-repl--shim-store-socket-present-p) "t" "nil"))
    (cl-labels
        ((finish (ok detail)
           (when (agent-repl--latch-claim latch)
             (if ok
                 (agent-repl--info '(:agent-repl-central "host service management spans workspaces")
                                   "elisp.services.store-ready socket=%s elapsed=%.3f"
                                   agent-repl--shim-store-socket
                                   (- (float-time) started-at))
               (agent-repl--error '(:agent-repl-central "host service management spans workspaces")
                                  "elisp.services.store-not-ready socket=%s elapsed=%.3f detail=%S"
                                  agent-repl--shim-store-socket
                                  (- (float-time) started-at) detail))
             (if ok (funcall on-success) (funcall on-failure detail))))
         (poll ()
           (let ((ready (agent-repl--shim-store-socket-present-p))
                 (now (float-time)))
             (agent-repl--log-verbose '(:agent-repl-central "host service management spans workspaces")
                                      "elisp.services.store-readiness-poll ready=%s remaining=%.3f"
                                      (if ready "t" "nil") (max 0.0 (- deadline now)))
             (cond
              (ready (finish t nil))
              ((>= now deadline)
               (finish nil (format "shim-store socket %s absent after %.1fs"
                                   agent-repl--shim-store-socket
                                   agent-repl-shim-store-ready-timeout)))
              (t (agent-repl--latch-set-timer
                  latch 'poll
                  (agent-repl--shim-services-run-timer 0.1 #'poll)))))))
      (poll)
      :pending)))

;;;; ---- The store/sidecar bounce ----

(cl-defun agent-repl--shim-services-build-and-bounce
    (preflight-complete on-success on-failure)
  "Build stale store/sidecar binaries and kickstart both launchd jobs.
The store is kickstarted and confirmed ready before the sidecar is
touched, and both stamps are written only after their kickstart
succeeded.  PREFLIGHT-COMPLETE means the coordinator already validated
both jobs before building any runtime artifact."
  (unless (and (functionp on-success) (functionp on-failure))
    (agent-repl--fatal '(:agent-repl-central "host service management spans workspaces") "elisp.services.bounce needs callable continuations"))
  (agent-repl--info '(:agent-repl-central "host service management spans workspaces")
                    "elisp.services.bounce-begin preflight-complete=%s store=%s sidecar=%s"
                    (if preflight-complete "t" "nil")
                    agent-repl--shim-store-binary
                    agent-repl--shim-sidecar-binary)
  (if preflight-complete
      (agent-repl--log '(:agent-repl-central "host service management spans workspaces") "elisp.services.bounce-preflight-inherited")
    (agent-repl--log '(:agent-repl-central "host service management spans workspaces") "elisp.services.bounce-preflight-own")
    (agent-repl--shim-services-assert-launchd-loaded))
  ;; The build is ASYNCHRONOUS, so everything downstream of it lives in
  ;; `agent-repl--shim-services-after-build', which the continuation calls.
  (agent-repl-daemon--build
   agent-repl--shim-service-build-targets
   (lambda (build-failure)
     (if build-failure
         (progn
           (agent-repl--error '(:agent-repl-central "host service management spans workspaces") "elisp.services.build-failed detail=%s" build-failure)
           (funcall on-failure build-failure))
       ;; A signal raised inside a sentinel-driven continuation has no
       ;; caller left to catch it, so it is converted to the failure
       ;; channel here rather than escaping into the timer machinery.
       (condition-case err
           (agent-repl--shim-services-after-build on-success on-failure)
         (error (funcall on-failure (error-message-string err)))))))
  :pending)

(cl-defun agent-repl--shim-services-after-build (on-success on-failure)
  "Kickstart the store and sidecar launchd jobs once their build has landed.
Split out of `agent-repl--shim-services-build-and-bounce' only because
the build in front of it is asynchronous; the sequencing is unchanged."
  (let ((store-present (agent-repl--frontend-artifact-exists-p
                        agent-repl--shim-store-binary))
        (sidecar-present (agent-repl--frontend-artifact-exists-p
                          agent-repl--shim-sidecar-binary)))
    (agent-repl--log '(:agent-repl-central "host service management spans workspaces")
                     "elisp.services.artifacts store-present=%s sidecar-present=%s"
                     (if store-present "t" "nil") (if sidecar-present "t" "nil"))
    (unless (and store-present sidecar-present)
      (agent-repl--fatal '(:agent-repl-central "host service management spans workspaces")
                         "shim service build completed without both binaries: store=%s present=%s sidecar=%s present=%s"
                         agent-repl--shim-store-binary (if store-present "t" "nil")
                         agent-repl--shim-sidecar-binary (if sidecar-present "t" "nil"))))
  (let ((store-stale (agent-repl--shim-service-needs-bounce-p
                      agent-repl--shim-store-binary))
        (sidecar-stale (agent-repl--shim-service-needs-bounce-p
                        agent-repl--shim-sidecar-binary)))
    (cl-labels
        ((after-store-ready (store-bounced)
           (condition-case err
               (progn
                 (when store-bounced
                   (agent-repl--shim-service-record-deployed
                    agent-repl--shim-store-binary))
                 ;; A store bounce always bounces the sidecar too, stale or
                 ;; not: the sidecar's link recovery is connection-scoped, and
                 ;; a fresh pair is the known-good state after a store restart.
                 (if (or store-bounced sidecar-stale)
                     (progn
                       (agent-repl--backend-phase
                        '(:agent-repl-central "host service management spans workspaces")
                        "store up; bouncing the sidecar service…")
                       (agent-repl--shim-services-launchctl
                        "kickstart" agent-repl--shim-sidecar-label)
                       (agent-repl--shim-service-record-deployed
                        agent-repl--shim-sidecar-binary))
                   (agent-repl--log '(:agent-repl-central "host service management spans workspaces")
                                    "elisp.services.sidecar-kickstart-skipped label=%s"
                                    agent-repl--shim-sidecar-label))
                 (agent-repl--info '(:agent-repl-central "host service management spans workspaces") "elisp.services.bounce-complete store=%s sidecar=%s"
                                   agent-repl--shim-store-label
                                   agent-repl--shim-sidecar-label)
                 (agent-repl--backend-phase
                  '(:agent-repl-central "host service management spans workspaces")
                  "store and sidecar services up")
                 (funcall on-success))
             (error
              (let ((detail (error-message-string err)))
                (agent-repl--error '(:agent-repl-central "host service management spans workspaces") "elisp.services.sidecar-bounce-failed detail=%s" detail)
                (agent-repl--backend-phase
                 '(:agent-repl-central "host service management spans workspaces")
                 "sidecar service bounce FAILED: %s — full output in %s"
                 detail (agent-repl--logfile-path))
                (funcall on-failure detail)))))
         (store-failed (detail)
           (agent-repl--error '(:agent-repl-central "host service management spans workspaces") "elisp.services.store-bounce-failed detail=%s" detail)
           (agent-repl--backend-phase
            '(:agent-repl-central "host service management spans workspaces")
            "store service never came up: %s — full output in %s"
            detail (agent-repl--logfile-path))
           (funcall on-failure detail)))
      (if store-stale
          (progn
            (agent-repl--backend-phase
             '(:agent-repl-central "host service management spans workspaces")
             "bouncing the store service…")
            (agent-repl--shim-services-launchctl "kickstart" agent-repl--shim-store-label))
        (agent-repl--log '(:agent-repl-central "host service management spans workspaces") "elisp.services.store-kickstart-skipped label=%s"
                         agent-repl--shim-store-label))
      ;; Readiness is awaited on BOTH paths.  A skipped kickstart still has to
      ;; prove the store is serving before the sidecar is touched.
      (agent-repl--shim-store-after-ready
       (lambda () (after-store-ready store-stale))
       #'store-failed)))
  :pending)

;;;; ---- The coordinated runtime bounce ----

(cl-defun agent-repl--runtime-prepare (on-success on-failure)
  "Rebuild the stack, bounce store and sidecar, then replace the daemon.
The order is the dependency order and nothing else: launchd preflight,
whole-stack build, store then sidecar, then the daemon is ASKED to exit
and a fresh one is ensured.  ON-SUCCESS runs only after every stage
completes; ON-FAILURE receives the first diagnostic and no later stage
starts.

There is no readiness wait on the daemon here and no workspace rebinding:
the daemon publishes `daemon.addr' when it is serving, and every
workspace re-registers on link-up because registration is idempotent by
dir."
  (agent-repl--assert-main-thread "runtime-restart")
  (unless (and (functionp on-success) (functionp on-failure))
    (agent-repl--fatal '(:agent-repl-central "host service management spans workspaces") "elisp.services.runtime-prepare needs callable continuations"))
  (agent-repl--backend-phase
   '(:agent-repl-central "host service management spans workspaces")
   "backend restart beginning…")
  (let ((started (float-time))
        (settled nil))
    (cl-labels
        ((fail (detail)
           (unless settled
             (setq settled t)
             (agent-repl--error '(:agent-repl-central "host service management spans workspaces") "elisp.services.runtime-failed elapsed=%.3f detail=%s"
                                (- (float-time) started) detail)
             (agent-repl--backend-phase
              '(:agent-repl-central "host service management spans workspaces")
              "backend restart FAILED: %s — full output in %s"
              detail (agent-repl--logfile-path))
             (funcall on-failure detail)))
         (complete ()
           (unless settled
             (setq settled t)
             (agent-repl--info '(:agent-repl-central "host service management spans workspaces") "elisp.services.runtime-complete elapsed=%.3f"
                               (- (float-time) started))
             (agent-repl--backend-phase
              '(:agent-repl-central "host service management spans workspaces")
              "backend restart complete (%.1fs)"
                                       (- (float-time) started))
             (funcall on-success)))
         (replace-daemon ()
           ;; The stop is a REQUEST — Emacs never kills a daemon — and the
           ;; ensure is what brings the replacement up.  A daemon that is
           ;; not there to stop is not an error: the ensure covers it.
           (agent-repl-frontend-daemon-stop
            (lambda (stopped)
              (agent-repl--info '(:agent-repl-central "host service management spans workspaces") "elisp.services.daemon-stopped accepted=%s"
                                (if stopped "t" "nil"))
              (agent-repl-link-teardown)
              (agent-repl-daemon-ensure
               (lambda (conn)
                 (if conn
                     (complete)
                   (fail "the replacement daemon never came up"))))))))
      (condition-case err
          (progn
            (agent-repl--shim-services-assert-launchd-loaded)
            (agent-repl-daemon--build
             nil
             (lambda (build-failure)
               (if build-failure
                   (fail build-failure)
                 ;; Same reason as above: the outer `condition-case' has
                 ;; already returned by the time this continuation runs.
                 (condition-case err
                     (agent-repl--shim-services-build-and-bounce
                      t #'replace-daemon #'fail)
                   (error (fail (error-message-string err))))))))
        (error (fail (error-message-string err))))
      :pending)))

(defun agent-repl-runtime-restart ()
  "Rebuild and bounce the whole agent-repl runtime.
Build script, then the store and sidecar services, then the daemon: stop
it (a request, never a kill) and ensure a fresh one."
  (interactive)
  (agent-repl--info '(:agent-repl-central "host service management spans workspaces") "elisp.services.runtime-restart-command interactive=%s"
                    (if (called-interactively-p 'interactive) "t" "nil"))
  (agent-repl--runtime-prepare
   ;; `agent-repl--runtime-prepare' already echoes the completion phase line
   ;; with its elapsed time; a second message would only overwrite it.
   #'ignore
   (lambda (detail)
     (agent-repl--warn '(:agent-repl-central "host service management spans workspaces") "elisp.services.runtime-restart-failed detail=%s" detail))))

(defun agent-repl-runtime-restart-await (&optional timeout)
  "Restart the runtime and return only after terminal completion.
TIMEOUT overrides `agent-repl-runtime-restart-await-timeout'.  Reserved
for deployment orchestration reaching Emacs over emacsclient: it pumps
process output and timers while the asynchronous coordinator runs, then
returns the exact string `runtime-restart-complete'.  A failure or a
timeout is logged and SIGNALLED, so a caller cannot mistake the initial
dispatch for a completed deployment."
  (let ((limit (or timeout agent-repl-runtime-restart-await-timeout)))
    (unless (and (numberp limit) (> limit 0))
      (agent-repl--fatal '(:agent-repl-central "host service management spans workspaces") "elisp.services.runtime-await-invalid-timeout timeout=%S" limit))
    (let ((started (float-time))
          (deadline (+ (float-time) limit))
          (state :pending)
          (failure nil))
      (agent-repl--info '(:agent-repl-central "host service management spans workspaces") "elisp.services.runtime-await-begin timeout=%.3f" limit)
      (agent-repl--runtime-prepare
       (lambda () (setq state :complete))
       (lambda (detail) (setq failure detail state :failed)))
      (while (and (eq state :pending) (< (float-time) deadline))
        (agent-repl--runtime-pump-events 0.05))
      (pcase state
        (:complete
         (agent-repl--info '(:agent-repl-central "host service management spans workspaces") "elisp.services.runtime-await-complete elapsed=%.3f"
                           (- (float-time) started))
         "runtime-restart-complete")
        (:failed
         (agent-repl--fatal '(:agent-repl-central "host service management spans workspaces") "elisp.services.runtime-await-failed elapsed=%.3f detail=%s"
                            (- (float-time) started) failure))
        (_
         (agent-repl--fatal '(:agent-repl-central "host service management spans workspaces") "elisp.services.runtime-await-timeout timeout=%.3f elapsed=%.3f"
                            limit (- (float-time) started)))))))

(provide 'services)

;;; services.el ends here
