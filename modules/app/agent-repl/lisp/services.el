;;; services.el --- the coordinated runtime restart -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns the coordinated runtime restart: rebuild the stack and the services'
;; binaries, ask the daemon to stop, and bring a fresh one up through
;; daemon.el.
;;
;; THIS FILE DRIVES NO SERVICE MANAGER.  The store, the sidecar and zettel
;; are kept running by the platform's service manager (launchd on macOS,
;; systemd --user on Linux), and the DAEMON is the one program that
;; restarts them: at every boot it restarts whichever service is not
;; running the installed build, the store before any shim starts
;; (daemon/internal/deploy/services.go, `Restarter.EnsureCurrent').  So a
;; restart here only has to install the new binaries and replace the
;; daemon; the replacement's boot does the rest, in the one recorded safe
;; order, on either platform.  Emacs once ran launchctl itself and kept
;; its own deployed-fingerprint stamps to decide what to bounce: a second
;; copy of the daemon's logic, and one that could not run on Linux.
;;
;; A SERVICE THE DAEMON COULD NOT RESTART is the daemon's to report: its
;; boot records it at ERROR and each shim raises its own store fault on
;; every surface.  Completion here is the daemon's NEW process identity
;; and nothing else.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--assert-main-thread "core" (what))
(declare-function agent-repl--fatal "core" (ws fmt &rest args))
(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--warn "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function agent-repl--backend-phase "core" (ws fmt &rest args))
(declare-function agent-repl--logfile-path "core" ())

(declare-function agent-repl--frontend-artifact-exists-p "daemon" (path))
(declare-function agent-repl-daemon--build "daemon" (targets continuation))
(declare-function agent-repl-daemon--await-departure "daemon" (on-gone))
(declare-function agent-repl-daemon-ensure "daemon" (&optional on-ready))
(declare-function agent-repl-daemon-observe-identity "daemon" (conn on-answer on-failure))
(declare-function agent-repl-frontend-daemon-stop "daemon" (&optional on-done))
(declare-function agent-repl-link-primary "daemon-link" ())
(declare-function agent-repl-link-teardown "daemon-link" ())

(defconst agent-repl--shim-service-cache-bin
  (expand-file-name ".cache/agent-repl/bin/" "~")
  "Directory containing the binaries the service manager executes.")

(defconst agent-repl--shim-store-binary
  (expand-file-name "shim-store" agent-repl--shim-service-cache-bin)
  "Installed shim-store binary.")

(defconst agent-repl--shim-sidecar-binary
  (expand-file-name "shim-claude-sidecar"
                    agent-repl--shim-service-cache-bin)
  "Installed Claude sidecar binary.")

(defconst agent-repl--shim-service-build-targets '("store" "sidecar" "zettel")
  "Build-script targets covering the services the service manager runs.
They are outside the build script's default set, so the restart names
them.  The daemon's own boot restarts each one whose running build is
not the installed one (daemon/internal/deploy/services.go).")

;;;; ---- The services' build ----

(cl-defun agent-repl--shim-services-build (on-success on-failure)
  "Build the store, sidecar and zettel binaries, restarting none of them.
ON-SUCCESS runs once the build landed and both the store's and the
sidecar's binaries are installed; ON-FAILURE receives the build's
diagnostic.  A build that reports success without both binaries is a
broken build, reported as a failure and never as a restart to go on with.

The running services are left as they are: the daemon that boots next
restarts whichever one is not running what was just installed."
  (unless (and (functionp on-success) (functionp on-failure))
    (agent-repl--fatal '(:agent-repl-central "host service management spans workspaces") "elisp.services.build needs callable continuations"))
  (agent-repl--info '(:agent-repl-central "host service management spans workspaces")
                    "elisp.services.build-begin targets=%S store=%s sidecar=%s"
                    agent-repl--shim-service-build-targets
                    agent-repl--shim-store-binary
                    agent-repl--shim-sidecar-binary)
  ;; The build is ASYNCHRONOUS: the continuation runs from its sentinel.
  (agent-repl-daemon--build
   agent-repl--shim-service-build-targets
   (lambda (build-failure)
     (if build-failure
         (progn
           (agent-repl--error '(:agent-repl-central "host service management spans workspaces") "elisp.services.build-failed detail=%s" build-failure)
           (funcall on-failure build-failure))
       (let ((store-present (agent-repl--frontend-artifact-exists-p
                             agent-repl--shim-store-binary))
             (sidecar-present (agent-repl--frontend-artifact-exists-p
                               agent-repl--shim-sidecar-binary)))
         (agent-repl--log '(:agent-repl-central "host service management spans workspaces")
                          "elisp.services.artifacts store-present=%s sidecar-present=%s"
                          (if store-present "t" "nil") (if sidecar-present "t" "nil"))
         (if (and store-present sidecar-present)
             (funcall on-success)
           (let ((detail (format "shim service build completed without both binaries: store=%s present=%s sidecar=%s present=%s"
                                 agent-repl--shim-store-binary (if store-present "t" "nil")
                                 agent-repl--shim-sidecar-binary (if sidecar-present "t" "nil"))))
             (agent-repl--error '(:agent-repl-central "host service management spans workspaces")
                                "elisp.services.build-artifacts-missing detail=%s" detail)
             (funcall on-failure detail)))))))
  :pending)

;;;; ---- The coordinated runtime restart ----

(cl-defun agent-repl--runtime-prepare (on-success on-failure)
  "Rebuild the stack and the services, then replace the daemon.
The order is the dependency order and nothing else: the whole-stack
build, the services' build, then the daemon is ASKED to exit and a fresh
one is ensured, whose boot restarts every service not running what was
just installed.  ON-SUCCESS runs only after every stage completes;
ON-FAILURE receives the first diagnostic and no later stage starts.

Daemon completion requires a health answer carrying a NEW immutable
process identity.  A link cycle, address-file cycle, connection object,
or accepted shutdown request is not completion."
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
         (complete (before after)
           (unless settled
             (setq settled t)
             (agent-repl--info '(:agent-repl-central "host service management spans workspaces")
                               "elisp.services.runtime-complete elapsed=%.3f old-instance=%S old-pid=%S new-instance=%S new-pid=%S new-build-sha=%S"
                               (- (float-time) started)
                               (plist-get before :instance-id)
                               (plist-get before :pid)
                               (plist-get after :instance-id)
                               (plist-get after :pid)
                               (plist-get after :build-sha))
             (agent-repl--backend-phase
              '(:agent-repl-central "host service management spans workspaces")
              "backend restart complete (%.1fs)"
                                       (- (float-time) started))
             (funcall on-success)))
         (replacement-observed (before after)
           (cond
            ((null after)
             (fail "not restarted: the replacement daemon omitted its process identity"))
            ((and before
                  (equal (plist-get before :instance-id)
                         (plist-get after :instance-id)))
             (fail (format "not restarted: daemon instance %s still serves after the link cycle (pid %s, build %s)"
                           (plist-get after :instance-id)
                           (plist-get after :pid)
                           (plist-get after :build-sha))))
            (t
             (complete before after))))
         (replace-daemon (before)
           ;; The stop is a REQUEST — Emacs never kills a daemon.  Every
           ;; terminal outcome is inspected before the link is torn down.
           (agent-repl-frontend-daemon-stop
            (lambda (outcome)
              (agent-repl--info '(:agent-repl-central "host service management spans workspaces")
                                "elisp.services.daemon-stop-outcome arm=%S reason=%S before=%S"
                                (plist-get outcome :arm)
                                (plist-get outcome :reason) before)
              (pcase (plist-get outcome :arm)
                (:accepted
                 (agent-repl-link-teardown)
                 (agent-repl-daemon--await-departure
                  (lambda (gone)
                    (if (not gone)
                        (fail "not restarted: the daemon accepted shutdown but did not depart")
                      (agent-repl-daemon-ensure
                       (lambda (conn)
                         (if (null conn)
                             (fail "not restarted: the replacement daemon never came up")
                           (agent-repl-daemon-observe-identity
                            conn
                            (lambda (after)
                              (replacement-observed before after))
                            (lambda (detail)
                              (fail (format "not restarted: replacement identity was not observed: %s"
                                            detail)))))))))))
                (:not-restarted
                 (fail (format "not restarted: %s" (plist-get outcome :reason))))
                (arm
                 (fail (format "not restarted: unknown daemon stop outcome %S"
                               arm)))))))
         (build-then-replace (before)
           (condition-case err
               (agent-repl-daemon--build
                nil
                (lambda (build-failure)
                  (if build-failure
                      (fail build-failure)
                    ;; A signal raised inside a sentinel-driven continuation
                    ;; has no caller left to catch it: the outer
                    ;; `condition-case' has already returned.
                    (condition-case err
                        (agent-repl--shim-services-build
                         (lambda () (replace-daemon before)) #'fail)
                      (error (fail (error-message-string err)))))))
             (error (fail (error-message-string err)))))
         (before-observed (before)
           (agent-repl--info '(:agent-repl-central "host service management spans workspaces")
                             "elisp.services.runtime-current-identity instance=%S pid=%S build-sha=%S legacy=%s"
                             (plist-get before :instance-id)
                             (plist-get before :pid)
                             (plist-get before :build-sha)
                             (if before "nil" "t"))
           (build-then-replace before)))
      ;; Resolve the daemon link and its identity before mutating any runtime
      ;; state. A missing link is the exact production defect that used to be
      ;; ignored and turned into a false completion.
      (agent-repl-daemon-observe-identity
       (agent-repl-link-primary)
       #'before-observed
       (lambda (detail)
         (fail (format "not restarted: current daemon identity was not observed: %s"
                       detail))))
      :pending)))

(defun agent-repl-runtime-restart ()
  "Rebuild and restart the whole agent-repl runtime.
Build script, then the services' binaries, then the daemon: stop it (a
request, never a kill) and ensure a fresh one, whose boot restarts the
services onto what was built."
  (interactive)
  (agent-repl--info '(:agent-repl-central "host service management spans workspaces") "elisp.services.runtime-restart-command interactive=%s"
                    (if (called-interactively-p 'interactive) "t" "nil"))
  (agent-repl--runtime-prepare
   ;; `agent-repl--runtime-prepare' already echoes the completion phase line
   ;; with its elapsed time; a second message would only overwrite it.
   #'ignore
   (lambda (detail)
     (agent-repl--warn '(:agent-repl-central "host service management spans workspaces") "elisp.services.runtime-restart-failed detail=%s" detail))))

(provide 'services)

;;; services.el ends here
