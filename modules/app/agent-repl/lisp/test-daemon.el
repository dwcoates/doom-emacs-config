;;; test-daemon.el --- ERT tests for agent-repl daemon.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-daemon.el -f ert-run-tests-batch-and-exit
;;
;; NOTHING IS BUILT AND NOTHING IS SPAWNED.  The three external
;; boundaries — the build script, the daemon spawn, and the artifact
;; existence probe — are registered in
;; `agent-repl--external-boundary-functions', so the harness's guard
;; already fails any test that reaches one unstubbed; every test below
;; stubs all three.  The `DaemonHealth' probe is stubbed at the rpc layer
;; and answers SYNCHRONOUSLY, which is what makes the three-way cold-start
;; triage (answers healthy / answers unhealthy / answers nothing)
;; deterministic.
;;
;; The boot wait is a timer whose tick is a named function; the tests call
;; that function rather than waiting for a scheduler.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Harness ----

(defvar agent-repl-test-daemon--address nil
  "What the stubbed `daemon.addr' read answers, or nil for absent.")

(defvar agent-repl-test-daemon--build-exit 0
  "Exit code the stubbed build script answers.")

(defvar agent-repl-test-daemon--build-runs nil
  "Build-script invocations, newest first: the argv following the shell.")

(defvar agent-repl-test-daemon--build-defer nil
  "When non-nil the stubbed build does NOT settle; the test settles it.")

(defvar agent-repl-test-daemon--build-exits nil
  "Pending build sentinels, newest first: the ON-EXIT the stub captured.")

(defvar agent-repl-test-daemon--mtimes nil
  "Alist of PATH to mtime the stubbed mtime probe answers; absent means missing.")

(defvar agent-repl-test-daemon--source-files nil
  "Alist of DIR to the file list the stubbed source scan answers.")

(defvar agent-repl-test-daemon--idle-timers nil
  "Captured `run-with-idle-timer' calls, newest first: `(SECONDS . FUNCTION)'.")

(defvar agent-repl-test-daemon--spawns nil
  "Daemon spawns, newest first: `(ARGV . ENVIRONMENT)'.")

(defvar agent-repl-test-daemon--existing-paths nil
  "Paths the stubbed artifact probe reports as present; t means every path.")

(defvar agent-repl-test-daemon--health-answer nil
  "What the stubbed DaemonHealth answers; see `agent-repl-test-daemon--answer'.")

(defvar agent-repl-test-daemon--shutdown-answer nil
  "What the stubbed UpdateShutdownSchedule answers.")

(defvar agent-repl-test-daemon--shutdown-requests nil
  "UpdateShutdownSchedule requests, newest first.")

(defvar agent-repl-test-daemon--link-connect-calls 0
  "How many times the stubbed `agent-repl-link-connect' ran.")

(defvar agent-repl-test-daemon--link-conn nil
  "What the stubbed `agent-repl-link-connect' answers.")

(defvar agent-repl-test-daemon--link-up nil
  "What the stubbed `agent-repl-link-up-p' answers.")

(defvar agent-repl-test-daemon--timers nil
  "Captured `run-with-timer' calls, newest first: `(SECONDS . FUNCTION)'.")

(defvar agent-repl-test-daemon--logs nil
  "Captured `(LEVEL . TEXT)' log entries, newest first.")

(defvar agent-repl-test-daemon--displayed nil
  "Buffers handed to `display-buffer', newest first.")

(defun agent-repl-test-daemon--logged-p (level substring)
  "Return non-nil when a LEVEL entry containing SUBSTRING was recorded."
  (seq-some (lambda (entry)
              (and (eq (car entry) level)
                   (string-search substring (cdr entry))))
            agent-repl-test-daemon--logs))

(defun agent-repl-test-daemon--answer (answer on-response on-failure)
  "Deliver ANSWER through ON-RESPONSE or ON-FAILURE, synchronously."
  (pcase (car answer)
    (:response (when on-response (funcall on-response (cadr answer))))
    (:failure (when on-failure (funcall on-failure (cadr answer))))))

(defmacro agent-repl-test-daemon--with-harness (&rest body)
  "Run BODY with daemon.el's whole world faked and its state reset."
  (declare (indent 0))
  `(let ((agent-repl-test-daemon--address nil)
         (agent-repl-test-daemon--build-exit 0)
         (agent-repl-test-daemon--build-runs nil)
         (agent-repl-test-daemon--build-defer nil)
         (agent-repl-test-daemon--build-exits nil)
         (agent-repl-test-daemon--mtimes nil)
         (agent-repl-test-daemon--source-files nil)
         (agent-repl-test-daemon--idle-timers nil)
         (agent-repl-test-daemon--spawns nil)
         (agent-repl-test-daemon--existing-paths t)
         (agent-repl-test-daemon--health-answer
          (list :response (list :arm :success
                                :value (list :arm :healthy :value nil))))
         (agent-repl-test-daemon--shutdown-answer
          (list :response (list :arm :success :value nil)))
         (agent-repl-test-daemon--shutdown-requests nil)
         (agent-repl-test-daemon--link-connect-calls 0)
         (agent-repl-test-daemon--link-conn 'the-connection)
         (agent-repl-test-daemon--link-up nil)
         (agent-repl-test-daemon--timers nil)
         (agent-repl-test-daemon--logs nil)
         (agent-repl-test-daemon--displayed nil)
         (agent-repl--frontend-daemon-process nil)
         (agent-repl-daemon-build-failure nil)
         (agent-repl-daemon-mode-line-segment nil)
         (agent-repl-daemon--build-state nil)
         (agent-repl-daemon--build-duration nil)
         (agent-repl-daemon--build-status-timer nil)
         (agent-repl-daemon--build-in-flight nil)
         (agent-repl-daemon--build-process nil)
         (agent-repl-daemon--build-continuations nil)
         (agent-repl-daemon--build-started nil)
         (agent-repl-daemon--build-labels nil)
         (agent-repl-daemon--build-target-names nil)
         (agent-repl-daemon--startup-timer nil)
         (agent-repl-daemon--boot-timer nil)
         (agent-repl-daemon--boot-deadline nil)
         (agent-repl-daemon--boot-continuation nil)
         (agent-repl-daemon--ensure-in-flight nil))
     (cl-letf (((symbol-function 'agent-repl-connect-read-daemon-addr)
                (lambda () agent-repl-test-daemon--address))
               ((symbol-function 'agent-repl--frontend-run-build-script)
                (lambda (args on-exit)
                  (push args agent-repl-test-daemon--build-runs)
                  (push on-exit agent-repl-test-daemon--build-exits)
                  ;; The default is to settle SYNCHRONOUSLY, which keeps the
                  ;; cold-start scenarios deterministic; a test that cares
                  ;; about the window while a build runs defers instead.
                  (unless agent-repl-test-daemon--build-defer
                    (funcall on-exit agent-repl-test-daemon--build-exit))
                  'the-build-process))
               ((symbol-function 'agent-repl--frontend-file-mtime)
                (lambda (path) (cdr (assoc path agent-repl-test-daemon--mtimes))))
               ((symbol-function 'agent-repl--frontend-source-files)
                (lambda (dir _regexp)
                  (cdr (assoc dir agent-repl-test-daemon--source-files))))
               ((symbol-function 'run-with-idle-timer)
                (lambda (seconds _repeat function &rest _args)
                  (push (cons seconds function) agent-repl-test-daemon--idle-timers)
                  (timer-create)))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda (argv environment)
                  (push (cons argv environment) agent-repl-test-daemon--spawns)
                  'the-daemon-process))
               ((symbol-function 'agent-repl--frontend-artifact-exists-p)
                (lambda (path)
                  (if (eq agent-repl-test-daemon--existing-paths t)
                      t
                    (and (member path agent-repl-test-daemon--existing-paths) t))))
               ((symbol-function 'agent-repl-rpc-daemon-health)
                (lambda (_conn _request &rest keys)
                  (agent-repl-test-daemon--answer agent-repl-test-daemon--health-answer
                                                  (plist-get keys :on-response)
                                                  (plist-get keys :on-failure))))
               ((symbol-function 'agent-repl-rpc-update-shutdown-schedule)
                (lambda (_conn request &rest keys)
                  (push request agent-repl-test-daemon--shutdown-requests)
                  (agent-repl-test-daemon--answer agent-repl-test-daemon--shutdown-answer
                                                  (plist-get keys :on-response)
                                                  (plist-get keys :on-failure))))
               ((symbol-function 'agent-repl-link-connect)
                (lambda ()
                  (setq agent-repl-test-daemon--link-connect-calls
                        (1+ agent-repl-test-daemon--link-connect-calls))
                  agent-repl-test-daemon--link-conn))
               ((symbol-function 'agent-repl-link-up-p)
                (lambda () agent-repl-test-daemon--link-up))
               ((symbol-function 'agent-repl-link-primary)
                (lambda () agent-repl-test-daemon--link-conn))
               ((symbol-function 'agent-repl-link-teardown) (lambda () nil))
               ((symbol-function 'run-with-timer)
                (lambda (seconds _repeat function &rest _args)
                  (push (cons seconds function) agent-repl-test-daemon--timers)
                  (timer-create)))
               ((symbol-function 'display-buffer)
                (lambda (buffer &rest _)
                  (push buffer agent-repl-test-daemon--displayed)
                  nil))
               ((symbol-function 'message) (lambda (&rest _) nil))
               ((symbol-function 'agent-repl--backend-phase) (lambda (&rest _) nil))
               ((symbol-function 'agent-repl--log)
                (lambda (_ws fmt &rest args)
                  (push (cons :log (apply #'format fmt args)) agent-repl-test-daemon--logs)))
               ((symbol-function 'agent-repl--info)
                (lambda (_ws fmt &rest args)
                  (push (cons :info (apply #'format fmt args)) agent-repl-test-daemon--logs)))
               ((symbol-function 'agent-repl--warn)
                (lambda (_ws fmt &rest args)
                  (push (cons :warn (apply #'format fmt args)) agent-repl-test-daemon--logs)))
               ((symbol-function 'agent-repl--error)
                (lambda (_ws fmt &rest args)
                  (push (cons :error (apply #'format fmt args)) agent-repl-test-daemon--logs))))
       ,@body)))

;;;; ---- daemon.addr absent: build, start, wait ----

(ert-deftest agent-repl-test-daemon-absent-address-runs-the-build-script ()
  "No daemon means build first; the script owns staleness itself."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil)
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (equal (car agent-repl-test-daemon--build-runs)
                   (list agent-repl-daemon-build-script)))))

(ert-deftest agent-repl-test-daemon-command-defaults-to-the-module-binary ()
  "The base argv defaults to the module's own `daemon/bin/claude-repld'.
A wrong default path would point cold start at nothing.  The account-root
flags are NOT part of it: `agent-repl-daemon--argv' appends them, so the
binary stays overridable on its own."
  ;; Arrange / Act: the default, independent of any buffer-local override.
  (let ((default (default-value 'agent-repl-daemon-command)))
    ;; Assert
    (should (equal default
                   (list (expand-file-name "daemon/bin/claude-repld"
                                           agent-repl--frontend-root))))))

(ert-deftest agent-repl-test-daemon-argv-carries-the-default-config-dir ()
  "The argv states `--default-config-dir', expanded.
The daemon's account resolver refuses to build without it and the process
exits 2 before it serves, so an argv missing it never boots at all."
  ;; Arrange
  (let ((agent-repl-daemon-command '("/bin/claude-repld"))
        (agent-repl-daemon-default-config-dir "~/.claude")
        (agent-repl-daemon-multi-repo-config-dir "~/.claude-chesscom"))
    ;; Act
    (let ((argv (agent-repl-daemon--argv)))
      ;; Assert
      (should (equal (cadr (member "--default-config-dir" argv))
                     (expand-file-name "~/.claude"))))))

(ert-deftest agent-repl-test-daemon-argv-carries-the-multi-repo-config-dir ()
  "The argv states `--multi-repo-config-dir', expanded.
The resolver requires BOTH roots: the account is determined by the
workspace's path, so every path must have an answer."
  ;; Arrange
  (let ((agent-repl-daemon-command '("/bin/claude-repld"))
        (agent-repl-daemon-default-config-dir "~/.claude")
        (agent-repl-daemon-multi-repo-config-dir "~/.claude-chesscom"))
    ;; Act
    (let ((argv (agent-repl-daemon--argv)))
      ;; Assert
      (should (equal (cadr (member "--multi-repo-config-dir" argv))
                     (expand-file-name "~/.claude-chesscom"))))))

(ert-deftest agent-repl-test-daemon-argv-keeps-the-binary-first ()
  "The base argv leads: the flags are APPENDED, never interleaved."
  ;; Arrange
  (let ((agent-repl-daemon-command '("/bin/claude-repld"))
        (agent-repl-daemon-default-config-dir "/a")
        (agent-repl-daemon-multi-repo-config-dir "/b"))
    ;; Act
    (let ((argv (agent-repl-daemon--argv)))
      ;; Assert
      (should (equal (car argv) "/bin/claude-repld")))))

(ert-deftest agent-repl-test-daemon-default-config-dir-defaults-to-claude ()
  "The default account root is the vendor's own `~/.claude'."
  ;; Arrange / Act
  (let ((default (default-value 'agent-repl-daemon-default-config-dir)))
    ;; Assert
    (should (equal default
                   (expand-file-name (or (getenv "CLAUDE_CONFIG_DIR") "~/.claude"))))))

(ert-deftest agent-repl-test-daemon-multi-repo-config-dir-defaults-to-chesscom ()
  "The multi-repo account root is `~/.claude-chesscom' by default."
  ;; Arrange / Act
  (let ((default (default-value 'agent-repl-daemon-multi-repo-config-dir)))
    ;; Assert
    (should (equal default (expand-file-name "~/.claude-chesscom")))))

(ert-deftest agent-repl-test-daemon-environment-states-the-multi-repo-root ()
  "A configured multi-repo root is EXPORTED: the daemon reads it from env.
There is no flag for it, so a root Emacs knows about and does not state is
a root the daemon has never heard of."
  ;; Arrange
  (let ((agent-repl-daemon-multi-repo-root "~/workspace"))
    ;; Act
    (let ((env (agent-repl-daemon--environment)))
      ;; Assert
      (should (member (format "MULTI_REPO_ROOT=%s"
                              (directory-file-name (expand-file-name "~/workspace")))
                      env)))))

(ert-deftest agent-repl-test-daemon-environment-states-one-multi-repo-root ()
  "The stated root REPLACES an inherited one: two would be a split account rule."
  ;; Arrange
  (let* ((agent-repl-daemon-multi-repo-root "/roots/mine")
         (process-environment (cons "MULTI_REPO_ROOT=/roots/stale"
                                    process-environment)))
    ;; Act
    (let ((env (agent-repl-daemon--environment)))
      ;; Assert
      (should (equal (seq-filter (lambda (entry)
                                   (string-prefix-p "MULTI_REPO_ROOT=" entry))
                                 env)
                     (list "MULTI_REPO_ROOT=/roots/mine"))))))

(ert-deftest agent-repl-test-daemon-environment-leaves-an-unset-root-alone ()
  "Nil is \"Emacs has no opinion\", never \"unset what the session carried\"."
  ;; Arrange
  (let* ((agent-repl-daemon-multi-repo-root nil)
         (process-environment (cons "MULTI_REPO_ROOT=/roots/inherited"
                                    process-environment)))
    ;; Act
    (let ((env (agent-repl-daemon--environment)))
      ;; Assert
      (should (member "MULTI_REPO_ROOT=/roots/inherited" env)))))

(ert-deftest agent-repl-test-daemon-empty-default-config-dir-refuses-the-launch ()
  "An empty required root REFUSES the spawn instead of buying a status-2 exit."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil)
    (let ((agent-repl-daemon-default-config-dir ""))
      ;; Act
      (agent-repl-daemon-ensure)
      ;; Assert
      (should (null agent-repl-test-daemon--spawns)))))

(ert-deftest agent-repl-test-daemon-refused-launch-names-the-missing-flag ()
  "The refusal SAYS which flag has no value, loudly."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil)
    (let ((agent-repl-daemon-multi-repo-config-dir "  "))
      ;; Act
      (agent-repl-daemon-ensure)
      ;; Assert
      (should (string-match-p "--multi-repo-config-dir"
                              (or agent-repl-daemon-launch-failure ""))))))

(ert-deftest agent-repl-test-daemon-refused-launch-raises-the-segment ()
  "A refused launch is drawn in the mode line, not only logged."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil)
    (let ((agent-repl-daemon-default-config-dir ""))
      ;; Act
      (agent-repl-daemon-ensure)
      ;; Assert
      (should (equal agent-repl-daemon-mode-line-segment "daemon: launch failed")))))

(ert-deftest agent-repl-test-daemon-early-exit-ends-the-boot-wait ()
  "A daemon that EXITED will never publish an address; the wait ends at once."
  ;; Arrange
  (let ((agent-repl-daemon--boot-timer nil)
        (agent-repl-daemon--boot-deadline nil)
        (agent-repl-daemon--boot-process nil)
        (agent-repl-daemon--boot-continuation nil)
        (outcomes nil))
    (cl-letf (((symbol-function 'agent-repl-connect-read-daemon-addr) (lambda () nil))
              ((symbol-function 'agent-repl-daemon--exited-p) (lambda (proc) (eq proc 'dead)))
              ((symbol-function 'process-exit-status) (lambda (_proc) 2))
              ((symbol-function 'agent-repl--frontend-run-log-tail) (lambda () "boom")))
      ;; Act
      (agent-repl-daemon--await-address (lambda (address) (push address outcomes)) 'dead)
      ;; Assert
      (should (equal outcomes (list nil))))))

(ert-deftest agent-repl-test-daemon-early-exit-surfaces-the-run-log-tail ()
  "The failure carries the run log's last line: WHY it died is the diagnosis."
  ;; Arrange
  (let ((agent-repl-daemon--boot-timer nil)
        (agent-repl-daemon--boot-deadline nil)
        (agent-repl-daemon--boot-process nil)
        (agent-repl-daemon--boot-continuation nil)
        (agent-repl-daemon-launch-failure nil))
    (cl-letf (((symbol-function 'agent-repl-connect-read-daemon-addr) (lambda () nil))
              ((symbol-function 'agent-repl-daemon--exited-p) (lambda (proc) (eq proc 'dead)))
              ((symbol-function 'process-exit-status) (lambda (_proc) 2))
              ((symbol-function 'agent-repl--frontend-run-log-tail)
               (lambda () "Roots.Default is required")))
      ;; Act
      (agent-repl-daemon--await-address #'ignore 'dead)
      ;; Assert
      (should (string-match-p "Roots.Default is required"
                              (or agent-repl-daemon-launch-failure ""))))))

(ert-deftest agent-repl-test-daemon-live-process-keeps-the-boot-wait-polling ()
  "A LIVE daemon that has not published yet is still booting, not dead."
  ;; Arrange
  (let ((agent-repl-daemon--boot-timer nil)
        (agent-repl-daemon--boot-deadline nil)
        (agent-repl-daemon--boot-process nil)
        (agent-repl-daemon--boot-continuation nil)
        (agent-repl-daemon-launch-failure nil)
        (settled 'none))
    (cl-letf (((symbol-function 'agent-repl-connect-read-daemon-addr) (lambda () nil))
              ((symbol-function 'agent-repl-daemon--exited-p) (lambda (_proc) nil))
              ((symbol-function 'run-with-timer) (lambda (&rest _) (timer-create))))
      ;; Act
      (agent-repl-daemon--await-address (lambda (address) (setq settled address)) 'alive)
      ;; Assert
      (should (eq settled 'none)))))

(ert-deftest agent-repl-test-daemon-exited-p-ignores-a-non-process ()
  "A stubbed spawn is not an exit: only a real dead process ends the wait."
  ;; Arrange / Act / Assert
  (should-not (agent-repl-daemon--exited-p 'the-daemon-process)))

(ert-deftest agent-repl-test-daemon-absent-address-starts-the-daemon ()
  "After a clean build the daemon is started with the account-root argv."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil)
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (equal (car (car agent-repl-test-daemon--spawns))
                   (agent-repl-daemon--argv)))))

(ert-deftest agent-repl-test-daemon-start-exports-the-state-root-explicitly ()
  "ONE state root is the cross-system contract, and it is STATED, not inherited."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil)
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (member (format "AGENT_REPL_STATE_DIR=%s"
                            (directory-file-name (agent-repl--global-state-dir)))
                    (cdr (car agent-repl-test-daemon--spawns))))))

(ert-deftest agent-repl-test-daemon-start-exports-exactly-one-state-root ()
  "An inherited second value would be a silent split-brain."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil)
    (let ((process-environment (cons "AGENT_REPL_STATE_DIR=/somewhere/stale"
                                     process-environment)))
      ;; Act
      (agent-repl-daemon-ensure))
    ;; Assert
    (should (= 1 (seq-count (lambda (entry)
                              (string-prefix-p "AGENT_REPL_STATE_DIR=" entry))
                            (cdr (car agent-repl-test-daemon--spawns)))))))

(ert-deftest agent-repl-test-daemon-start-waits-for-the-address ()
  "The daemon's published address IS its readiness; nothing else is probed."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil)
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (eq (cdr (car agent-repl-test-daemon--timers))
                #'agent-repl-daemon--boot-tick))))

(ert-deftest agent-repl-test-daemon-boot-tick-links-once-the-address-appears ()
  "An address that appears is what the link is stood on."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil)
    (agent-repl-daemon-ensure)
    (setq agent-repl-test-daemon--address "127.0.0.1:9001")
    ;; Act
    (agent-repl-daemon--boot-tick)
    ;; Assert
    (should (= agent-repl-test-daemon--link-connect-calls 1))))

(ert-deftest agent-repl-test-daemon-cold-start-logs-own-adopted ()
  "A daemon THIS Emacs cold-started is stated as its OWN before linking.
The adopt path reports provenance from the probe verdict; a cold start
never probes, so without this the own-spawn path publishes no provenance
at all and the log cannot say whose daemon the session attached to."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil)
    (agent-repl-daemon-ensure)
    (setq agent-repl-test-daemon--address "127.0.0.1:9001"
          agent-repl--frontend-daemon-process 'the-daemon-process)
    (cl-letf (((symbol-function 'process-live-p)
               (lambda (object) (eq object 'the-daemon-process))))
      ;; Act
      (agent-repl-daemon--boot-tick)
      ;; Assert
      (should (agent-repl-test-daemon--logged-p :info "elisp.daemon.own-adopted")))))

(ert-deftest agent-repl-test-daemon-cold-start-whose-daemon-died-logs-foreign-adopted ()
  "A cold start whose own child is GONE by the time an address appears is foreign.
The address was published by something this Emacs no longer owns, so the
provenance line must not claim it -- the same liveness question the adopt
path asks."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil)
    (agent-repl-daemon-ensure)
    (setq agent-repl-test-daemon--address "127.0.0.1:9001"
          agent-repl--frontend-daemon-process nil)
    ;; Act
    (agent-repl-daemon--boot-tick)
    ;; Assert
    (should (agent-repl-test-daemon--logged-p :info "elisp.daemon.foreign-adopted"))))

(ert-deftest agent-repl-test-daemon-boot-timeout-gives-up-loudly ()
  "A daemon that never publishes an address has not booted."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil)
    (agent-repl-daemon-ensure)
    (setq agent-repl-daemon--boot-deadline (- (float-time) 1))
    ;; Act
    (agent-repl-daemon--boot-tick)
    ;; Assert
    (should (agent-repl-test-daemon--logged-p :error "elisp.daemon.boot-timeout"))))

(ert-deftest agent-repl-test-daemon-boot-timeout-settles-the-continuation-with-nil ()
  "A caller must never be left waiting on a callback that will not come."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (let ((outcome :unset))
      (setq agent-repl-test-daemon--address nil)
      (agent-repl-daemon-ensure (lambda (conn) (setq outcome conn)))
      (setq agent-repl-daemon--boot-deadline (- (float-time) 1))
      ;; Act
      (agent-repl-daemon--boot-tick)
      ;; Assert
      (should (null outcome)))))

(ert-deftest agent-repl-test-daemon-missing-binary-starts-nothing ()
  "A build that produced no binary is not a daemon to start."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil
          agent-repl-test-daemon--existing-paths (list agent-repl-daemon-build-script))
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (null agent-repl-test-daemon--spawns))))

;;;; ---- The build failure path ----

(ert-deftest agent-repl-test-daemon-build-failure-starts-nothing ()
  "A failed build must never be followed by a start of a stale binary."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil
          agent-repl-test-daemon--build-exit 2)
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (null agent-repl-test-daemon--spawns))))

(ert-deftest agent-repl-test-daemon-build-failure-shows-the-capture-buffer ()
  "The build's own output is what diagnoses it, so it is put in front of the user."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil
          agent-repl-test-daemon--build-exit 2)
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (member agent-repl-daemon-build-buffer agent-repl-test-daemon--displayed))))

(ert-deftest agent-repl-test-daemon-build-failure-warns ()
  "A build failure is a WARNING on the durable record."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil
          agent-repl-test-daemon--build-exit 2)
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (agent-repl-test-daemon--logged-p :warn "elisp.daemon.build-failed"))))

(ert-deftest agent-repl-test-daemon-build-failure-raises-the-mode-line-segment ()
  "The standing failure is visible without reading a log."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil
          agent-repl-test-daemon--build-exit 2)
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (equal agent-repl-daemon-mode-line-segment "daemon: build failed"))))

(ert-deftest agent-repl-test-daemon-build-failure-is-not-retried ()
  "There is NO automatic retry; the interactive ensure is the retry."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil
          agent-repl-test-daemon--build-exit 2)
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (= (length agent-repl-test-daemon--build-runs) 1))))

(ert-deftest agent-repl-test-daemon-successful-build-clears-a-standing-failure ()
  "A build that works takes the failure segment down.
The segment then carries the short-lived \"built in N.Ns\" note, which is
the same segment reporting the new state — what must be gone is the
FAILURE, and it is gone from both the state and the text."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil
          agent-repl-daemon-build-failure "an earlier failure"
          agent-repl-daemon-mode-line-segment "daemon: build failed")
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (null agent-repl-daemon-build-failure))
    (should-not (equal agent-repl-daemon-mode-line-segment "daemon: build failed"))))

(ert-deftest agent-repl-test-daemon-missing-build-script-is-a-failure ()
  "An absent script is the installation being broken, not a run that failed."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil
          agent-repl-test-daemon--existing-paths nil)
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (agent-repl-test-daemon--logged-p
             :error "elisp.daemon.build-script-missing"))))

;;;; ---- An answering daemon is ADOPTED ----

(ert-deftest agent-repl-test-daemon-healthy-answer-is-adopted ()
  "A daemon that answers is adopted; nothing is built and nothing is started."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address "127.0.0.1:9001")
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (null agent-repl-test-daemon--spawns))))

(ert-deftest agent-repl-test-daemon-healthy-answer-builds-nothing ()
  "Adoption costs no build: the running daemon is the one that serves."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address "127.0.0.1:9001")
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (null agent-repl-test-daemon--build-runs))))

(ert-deftest agent-repl-test-daemon-healthy-answer-links ()
  "Adoption means standing the link on the daemon that is already there."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address "127.0.0.1:9001")
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (= agent-repl-test-daemon--link-connect-calls 1))))

(ert-deftest agent-repl-test-daemon-foreign-adoption-is-logged-at-info ()
  "Adopting a daemon this Emacs did not spawn is STATED, not inferred."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address "127.0.0.1:9001"
          agent-repl--frontend-daemon-process nil)
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (agent-repl-test-daemon--logged-p :info "elisp.daemon.foreign-adopted"))))

(ert-deftest agent-repl-test-daemon-own-daemon-adoption-is-not-called-foreign ()
  "A daemon THIS Emacs spawned is its own, whatever the address file says.
`process-live-p' is the liveness question, and the harness's spawn stub
answers with a symbol rather than a real process — so the answer is
stubbed too, which is the only external thing about this branch."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address "127.0.0.1:9001"
          agent-repl--frontend-daemon-process 'the-daemon-process)
    (cl-letf (((symbol-function 'process-live-p)
               (lambda (object) (eq object 'the-daemon-process))))
      ;; Act
      (agent-repl-daemon-ensure)
      ;; Assert
      (should-not (agent-repl-test-daemon--logged-p
                   :info "elisp.daemon.foreign-adopted")))))

(ert-deftest agent-repl-test-daemon-ensure-failure-releases-the-in-flight-flag ()
  "A signal out of the ensure must not leave cold start refusing forever."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil)
    (cl-letf (((symbol-function 'agent-repl--frontend-artifact-exists-p)
               (lambda (_path) (error "the build boundary exploded"))))
      ;; Act
      (should-error (agent-repl-daemon-ensure))
      ;; Assert
      (should-not agent-repl-daemon--ensure-in-flight))))

(ert-deftest agent-repl-test-daemon-ensure-failure-is-recorded-at-error ()
  "The signal is re-raised, but it is on the record before it leaves."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil)
    (cl-letf (((symbol-function 'agent-repl--frontend-artifact-exists-p)
               (lambda (_path) (error "the build boundary exploded"))))
      ;; Act
      (should-error (agent-repl-daemon-ensure))
      ;; Assert
      (should (agent-repl-test-daemon--logged-p :error "elisp.daemon.ensure-failed")))))

(ert-deftest agent-repl-test-daemon-unhealthy-answer-is-still-adopted ()
  "UNHEALTHY IS AN ANSWER: a daemon that is there is never replaced blindly."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address "127.0.0.1:9001"
          agent-repl-test-daemon--health-answer
          (list :response (list :arm :success
                                :value (list :arm :unhealthy
                                             :value (list :faults
                                                          (list (list :detail "store unreachable")))))))
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (null agent-repl-test-daemon--spawns))))

(ert-deftest agent-repl-test-daemon-unhealthy-answer-warns ()
  "An adopted daemon's faults are surfaced, not swallowed."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address "127.0.0.1:9001"
          agent-repl-test-daemon--health-answer
          (list :response (list :arm :success
                                :value (list :arm :unhealthy
                                             :value (list :faults
                                                          (list (list :detail "store unreachable")))))))
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (agent-repl-test-daemon--logged-p :warn "elisp.daemon.adopted-unhealthy"))))

(ert-deftest agent-repl-test-daemon-unhealthy-faults-reach-the-health-buffer ()
  "Each fault's dynamic detail is what a doctor report reads."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address "127.0.0.1:9001"
          agent-repl-test-daemon--health-answer
          (list :response (list :arm :success
                                :value (list :arm :unhealthy
                                             :value (list :faults
                                                          (list (list :detail "store unreachable")))))))
    (with-current-buffer (get-buffer-create "*agent-repl-health*") (erase-buffer))
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (string-search "store unreachable"
                           (with-current-buffer "*agent-repl-health*" (buffer-string))))))

(ert-deftest agent-repl-test-daemon-health-error-arm-still-counts-as-an-answer ()
  "A daemon that REFUSES the health question still proved it is listening."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address "127.0.0.1:9001"
          agent-repl-test-daemon--health-answer
          (list :response (list :arm :error :value nil)))
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (null agent-repl-test-daemon--spawns))))

;;;; ---- A stale address file ----

(ert-deftest agent-repl-test-daemon-transport-failure-is-a-stale-address ()
  "Only a TRANSPORT failure proves nobody is listening."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address "127.0.0.1:9001"
          agent-repl-test-daemon--health-answer
          (list :failure (list :kind :transport :message "connection refused")))
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (agent-repl-test-daemon--logged-p :warn "elisp.daemon.stale-addr"))))

(ert-deftest agent-repl-test-daemon-stale-address-is-treated-as-absent ()
  "A file left behind by a dead daemon leads to a build and a start."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address "127.0.0.1:9001"
          agent-repl-test-daemon--health-answer
          (list :failure (list :kind :transport :message "connection refused")))
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should agent-repl-test-daemon--spawns)))

;;;; ---- Idempotence ----

(ert-deftest agent-repl-test-daemon-ensure-with-a-standing-link-does-nothing ()
  "A link that already stands needs no daemon brought up."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--link-up t)
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (null agent-repl-test-daemon--build-runs))))

(ert-deftest agent-repl-test-daemon-ensure-with-a-standing-link-answers-the-connection ()
  "The continuation still receives the connection that is already there."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (let ((outcome :unset))
      (setq agent-repl-test-daemon--link-up t)
      ;; Act
      (agent-repl-daemon-ensure (lambda (conn) (setq outcome conn)))
      ;; Assert
      (should (eq outcome 'the-connection)))))

(ert-deftest agent-repl-test-daemon-concurrent-ensure-spawns-no-second-daemon ()
  "A concurrent ensure is exactly how a second daemon lands beside a live one."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil
          agent-repl-daemon--ensure-in-flight t)
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (null agent-repl-test-daemon--spawns))))

;;;; ---- Stop and restart ----

(ert-deftest agent-repl-test-daemon-stop-asks-for-an-immediate-shutdown ()
  "EMACS NEVER KILLS A DAEMON: the stop is UpdateShutdownSchedule{now}."
  (agent-repl-test-daemon--with-harness
    ;; Arrange / Act
    (agent-repl-frontend-daemon-stop)
    ;; Assert
    (should (eq (plist-get (plist-get (car agent-repl-test-daemon--shutdown-requests)
                                      :action)
                           :arm)
                :now))))

(ert-deftest agent-repl-test-daemon-stop-names-emacs-as-the-operator ()
  "The drain reason is typed, and its operator note names this editor."
  (agent-repl-test-daemon--with-harness
    ;; Arrange / Act
    (agent-repl-frontend-daemon-stop)
    ;; Assert
    (let* ((action (plist-get (car agent-repl-test-daemon--shutdown-requests) :action))
           (reason (plist-get (plist-get action :value) :reason)))
      (should (equal reason '(:arm :operator :value (:note "emacs")))))))

(ert-deftest agent-repl-test-daemon-stop-without-a-link-sends-nothing ()
  "There is no daemon to ask when no link stands."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--link-conn nil)
    ;; Act
    (agent-repl-frontend-daemon-stop)
    ;; Assert
    (should (null agent-repl-test-daemon--shutdown-requests))))

(ert-deftest agent-repl-test-daemon-stop-refusal-is-logged-at-error ()
  "A refused shutdown is surfaced, never swallowed."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--shutdown-answer
          (list :response (list :arm :error :value nil)))
    ;; Act
    (agent-repl-frontend-daemon-stop)
    ;; Assert
    (should (agent-repl-test-daemon--logged-p :error "elisp.daemon.stop-refused"))))

(ert-deftest agent-repl-test-daemon-stop-reports-acceptance-to-its-continuation ()
  "A caller that must sequence on the stop learns whether it landed."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (let ((outcome :unset))
      ;; Act
      (agent-repl-frontend-daemon-stop (lambda (value) (setq outcome value)))
      ;; Assert
      (should (equal outcome '(:arm :accepted))))))

(ert-deftest agent-repl-test-daemon-stop-transport-failure-reports-the-reason ()
  "A stop that could not be delivered did not happen."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (let ((outcome :unset))
      (setq agent-repl-test-daemon--shutdown-answer
            (list :failure (list :kind :transport :message "no route")))
      ;; Act
      (agent-repl-frontend-daemon-stop (lambda (value) (setq outcome value)))
      ;; Assert
      (should (eq (plist-get outcome :arm) :not-restarted))
      (should (string-match-p "no route" (plist-get outcome :reason))))))

(ert-deftest agent-repl-test-daemon-observe-identity-returns-the-serving-process ()
  "DaemonHealth is the identity boundary used by restart coordination."
  (agent-repl-test-daemon--with-harness
    ;; Arrange.
    (let ((identity nil))
      (setq agent-repl-test-daemon--health-answer
            '(:response
              (:arm :success
               :value (:arm :healthy :value nil
                       :identity (:instance-id "daemon-2" :pid 4242
                                  :build-sha "abc123")))))
      ;; Act.
      (agent-repl-daemon-observe-identity
       'the-connection (lambda (value) (setq identity value)) #'ignore)
      ;; Assert.
      (should (equal identity '(:instance-id "daemon-2" :pid 4242
                                             :build-sha "abc123"))))))

(ert-deftest agent-repl-test-daemon-observe-identity-rejects-an-invalid-pid ()
  "A malformed identity cannot certify a restarted daemon."
  (agent-repl-test-daemon--with-harness
    ;; Arrange.
    (let ((failure nil))
      (setq agent-repl-test-daemon--health-answer
            '(:response
              (:arm :success
               :value (:arm :healthy :value nil
                       :identity (:instance-id "daemon-2" :pid 0
                                  :build-sha "abc123")))))
      ;; Act.
      (agent-repl-daemon-observe-identity
       'the-connection #'ignore (lambda (detail) (setq failure detail)))
      ;; Assert.
      (should (string-match-p "invalid process identity" failure)))))

(ert-deftest agent-repl-test-daemon-restart-stops-then-ensures ()
  "Restart is the stop request followed by a fresh ensure."
  (agent-repl-test-daemon--with-harness
    ;; Arrange / Act
    (agent-repl-frontend-daemon-restart)
    ;; Assert
    (should (and agent-repl-test-daemon--shutdown-requests
                 agent-repl-test-daemon--build-runs))))

(ert-deftest agent-repl-test-daemon-restart-does-not-ensure-while-the-daemon-is-still-there ()
  "The ensure half waits: a daemon that still answers would be RE-ADOPTED."
  (agent-repl-test-daemon--with-harness
    ;; Arrange: the departing daemon has not removed its address yet.
    (setq agent-repl-test-daemon--address "127.0.0.1:9999")
    ;; Act
    (agent-repl-frontend-daemon-restart)
    ;; Assert
    (should (null agent-repl-test-daemon--build-runs))))

(ert-deftest agent-repl-test-daemon-restart-ensures-once-the-address-goes-away ()
  "The address's disappearance IS the departure, and it releases the ensure."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address "127.0.0.1:9999")
    (agent-repl-frontend-daemon-restart)
    ;; Act: the daemon exits and removes `daemon.addr'.
    (setq agent-repl-test-daemon--address nil)
    (agent-repl-daemon--departure-tick)
    ;; Assert
    (should agent-repl-test-daemon--build-runs)))

(ert-deftest agent-repl-test-daemon-restart-abandoned-when-the-stop-is-refused ()
  "A refused stop leaves the daemon serving, so no ensure may adopt it."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--shutdown-answer
          (list :response (list :arm :error :value nil)))
    ;; Act
    (agent-repl-frontend-daemon-restart)
    ;; Assert
    (should (agent-repl-test-daemon--logged-p :error "elisp.daemon.restart-abandoned"))))

(ert-deftest agent-repl-test-daemon-restart-refused-stop-runs-no-build ()
  "The other half of an abandoned restart: nothing is built or started."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--shutdown-answer
          (list :response (list :arm :error :value nil)))
    ;; Act
    (agent-repl-frontend-daemon-restart)
    ;; Assert
    (should (null agent-repl-test-daemon--build-runs))))

(ert-deftest agent-repl-test-daemon-restart-without-a-link-just-ensures ()
  "With no link standing there is nothing to stop; the restart is the ensure."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--link-conn nil)
    ;; Act
    (agent-repl-frontend-daemon-restart)
    ;; Assert
    (should (and (null agent-repl-test-daemon--shutdown-requests)
                 agent-repl-test-daemon--build-runs))))

(ert-deftest agent-repl-test-daemon-departure-timeout-is-surfaced ()
  "An address that outlives the wait is a WARNING, never a silent pass."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address "127.0.0.1:9999")
    (agent-repl-frontend-daemon-restart)
    ;; Act: the deadline has passed with the address still published.
    (setq agent-repl-daemon--departure-deadline (- (float-time) 1))
    (agent-repl-daemon--departure-tick)
    ;; Assert
    (should (agent-repl-test-daemon--logged-p :warn "elisp.daemon.departure-timeout"))))

(ert-deftest agent-repl-test-daemon-departure-timeout-does-not-ensure ()
  "A timed-out departure aborts instead of re-adopting the same daemon."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address "127.0.0.1:9999")
    (agent-repl-frontend-daemon-restart)
    ;; Act
    (setq agent-repl-daemon--departure-deadline (- (float-time) 1))
    (agent-repl-daemon--departure-tick)
    ;; Assert
    (should (= agent-repl-test-daemon--link-connect-calls 0))
    (should (agent-repl-test-daemon--logged-p
             :error "elisp.daemon.restart-abandoned"))))

;;;; ---- The sentinel ----

(ert-deftest agent-repl-test-daemon-sentinel-forgets-the-dead-process ()
  "Emacs supervises only what it started, and a corpse is not that."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
              ((symbol-function 'process-exit-status) (lambda (_p) 1)))
      (setq agent-repl--frontend-daemon-process 'the-daemon-process)
      ;; Act
      (agent-repl-daemon--sentinel 'the-daemon-process "exited abnormally")
      ;; Assert
      (should (null agent-repl--frontend-daemon-process)))))

(ert-deftest agent-repl-test-daemon-sentinel-restarts-nothing ()
  "Everything after boot is the daemon's own blue-green; Emacs does not restart it."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
              ((symbol-function 'process-exit-status) (lambda (_p) 1)))
      ;; Act
      (agent-repl-daemon--sentinel 'the-daemon-process "exited abnormally")
      ;; Assert
      (should (null agent-repl-test-daemon--spawns)))))

;;;; ---- The build's target selection ----

(defun agent-repl-test-daemon--artifact (target)
  "Return TARGET's artifact path from the build spec."
  (plist-get (cdr (assoc target agent-repl-daemon--build-target-specs)) :artifact))

(defun agent-repl-test-daemon--mark-fresh (&rest targets)
  "Give each of TARGETS an artifact newer than every source the scan reports."
  (dolist (target (or targets agent-repl-daemon--default-build-targets))
    (push (cons (agent-repl-test-daemon--artifact target) 100.0)
          agent-repl-test-daemon--mtimes)))

(ert-deftest agent-repl-test-daemon-build-passes-its-targets-to-the-script ()
  "A targeted build names its targets; the script still owns staleness."
  (agent-repl-test-daemon--with-harness
    ;; Arrange / Act
    (agent-repl-daemon--build '("store" "sidecar") #'ignore)
    ;; Assert
    (should (equal (car agent-repl-test-daemon--build-runs)
                   (list agent-repl-daemon-build-script "store" "sidecar")))))

(ert-deftest agent-repl-test-daemon-build-answers-nil-on-success ()
  "Nil IS success; a detail string IS the failure."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (let ((answers nil))
      ;; Act
      (agent-repl-daemon--build nil (lambda (detail) (push detail answers)))
      ;; Assert
      (should (equal answers '(nil))))))

(ert-deftest agent-repl-test-daemon-build-answers-a-detail-on-failure ()
  "The failure detail names the exit code and where the output is."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--build-exit 3)
    (let ((answers nil))
      ;; Act
      (agent-repl-daemon--build nil (lambda (detail) (push detail answers)))
      ;; Assert
      (should (string-search "exit 3" (car answers))))))

;;;; ---- The elisp staleness pre-check ----

(ert-deftest agent-repl-test-daemon-fresh-tree-spawns-no-build ()
  "Nothing stale means no subprocess at all: the script's own startup is the cost."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (agent-repl-test-daemon--mark-fresh)
    ;; Act
    (agent-repl-daemon--build nil #'ignore)
    ;; Assert
    (should (null agent-repl-test-daemon--build-runs))))

(ert-deftest agent-repl-test-daemon-fresh-tree-still-answers-success ()
  "A skipped build is a SUCCEEDED build as far as the caller is concerned."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (agent-repl-test-daemon--mark-fresh)
    (let ((answers 'unset))
      ;; Act
      (agent-repl-daemon--build nil (lambda (detail) (setq answers detail)))
      ;; Assert
      (should (null answers)))))

(ert-deftest agent-repl-test-daemon-missing-artifact-is-stale ()
  "An artifact that is not there cannot be fresh."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (agent-repl-test-daemon--mark-fresh "shim" "webapp" "lock")
    ;; Act / Assert: `daemon' has no artifact mtime, so it alone is stale.
    (should (equal (agent-repl-daemon--stale-targets nil) '("daemon")))))

(ert-deftest agent-repl-test-daemon-newer-source-is-stale ()
  "A source newer than the artifact is exactly what a rebuild is for."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (agent-repl-test-daemon--mark-fresh)
    (let ((spec (cdr (assoc "webapp" agent-repl-daemon--build-target-specs))))
      (push (cons (car (plist-get spec :sources)) '("/webapp/src/app.ts"))
            agent-repl-test-daemon--source-files)
      (push (cons "/webapp/src/app.ts" 200.0) agent-repl-test-daemon--mtimes))
    ;; Act / Assert
    (should (equal (agent-repl-daemon--stale-targets nil) '("webapp")))))

(ert-deftest agent-repl-test-daemon-stale-tree-spawns-exactly-one-build ()
  "A stale tree spawns the script ONCE, whatever the stale count."
  (agent-repl-test-daemon--with-harness
    ;; Arrange / Act
    (agent-repl-daemon--build nil #'ignore)
    ;; Assert
    (should (= (length agent-repl-test-daemon--build-runs) 1))))

(ert-deftest agent-repl-test-daemon-unknown-target-is-treated-as-stale ()
  "A target the spec cannot reason about is never silently skipped."
  (agent-repl-test-daemon--with-harness
    ;; Arrange / Act / Assert
    (should (equal (agent-repl-daemon--stale-targets '("no-such-target"))
                   '("no-such-target")))))

;;;; ---- Concurrent builds coalesce ----

(ert-deftest agent-repl-test-daemon-second-build-request-spawns-nothing ()
  "A request arriving mid-build joins it; two scripts at once would race."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--build-defer t)
    (agent-repl-daemon--build nil #'ignore)
    ;; Act
    (agent-repl-daemon--build nil #'ignore)
    ;; Assert
    (should (= (length agent-repl-test-daemon--build-runs) 1))))

(ert-deftest agent-repl-test-daemon-coalesced-requests-both-settle ()
  "Both callers are answered by the one build they share."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--build-defer t)
    (let ((answers nil))
      (agent-repl-daemon--build nil (lambda (d) (push (cons :first d) answers)))
      (agent-repl-daemon--build nil (lambda (d) (push (cons :second d) answers)))
      ;; Act
      (funcall (car agent-repl-test-daemon--build-exits) 0)
      ;; Assert
      (should (equal answers '((:second . nil) (:first . nil)))))))

;;;; ---- The mode-line status segment ----

(ert-deftest agent-repl-test-daemon-segment-says-building-while-the-build-runs ()
  "The user is told WHY the editor is busy, in the mode line and not the echo area."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--build-defer t)
    ;; Act
    (agent-repl-daemon--build nil #'ignore)
    ;; Assert
    (should (equal agent-repl-daemon-mode-line-segment
                   "building agent-repl stack…"))))

(ert-deftest agent-repl-test-daemon-segment-reports-the-build-duration ()
  "A finished build says how long it took, for a short while."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--build-defer t)
    (agent-repl-daemon--build nil #'ignore)
    (setq agent-repl-daemon--build-started (- (float-time) 2.0))
    ;; Act
    (funcall (car agent-repl-test-daemon--build-exits) 0)
    ;; Assert
    (should (equal agent-repl-daemon-mode-line-segment
                   "agent-repl stack built in 2.0s"))))

(ert-deftest agent-repl-test-daemon-segment-failure-outranks-the-duration ()
  "A failed build reports the failure, never a duration."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil
          agent-repl-test-daemon--build-exit 2)
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (equal agent-repl-daemon-mode-line-segment "daemon: build failed"))))

(ert-deftest agent-repl-test-daemon-built-status-schedules-its-own-clearing ()
  "The \"built\" note is a note, not a permanent fixture."
  (agent-repl-test-daemon--with-harness
    ;; Arrange / Act
    (agent-repl-daemon--build nil #'ignore)
    ;; Assert
    (should (eq (cdr (car agent-repl-test-daemon--timers))
                #'agent-repl-daemon--clear-build-status))))

(ert-deftest agent-repl-test-daemon-clearing-the-status-empties-the-segment ()
  "Once the window passes the mode line goes back to whatever else lives there."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (agent-repl-daemon--build nil #'ignore)
    ;; Act
    (agent-repl-daemon--clear-build-status)
    ;; Assert
    (should (null agent-repl-daemon-mode-line-segment))))

(ert-deftest agent-repl-test-daemon-a-second-build-cancels-the-pending-clear ()
  "The earlier build's clear timer must not wipe the later build's status."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (agent-repl-daemon--build nil #'ignore)
    (let ((cancelled nil))
      (cl-letf (((symbol-function 'cancel-timer)
                 (lambda (timer) (push timer cancelled))))
        ;; Act
        (agent-repl-daemon--set-build-status 'building nil))
      ;; Assert
      (should (= (length cancelled) 1)))))

;;;; ---- Startup scheduling ----

(ert-deftest agent-repl-test-daemon-startup-schedules-the-ensure-on-an-idle-timer ()
  "THE FRAME PAINTS FIRST: cold start is armed, never run, from the startup hook."
  (agent-repl-test-daemon--with-harness
    ;; Arrange / Act
    (agent-repl-daemon-schedule-ensure)
    ;; Assert
    (should (equal (car agent-repl-test-daemon--idle-timers)
                   (cons agent-repl-daemon-startup-idle-seconds
                         #'agent-repl-daemon-ensure)))))

(ert-deftest agent-repl-test-daemon-startup-scheduling-runs-no-build ()
  "Scheduling costs nothing: nothing is probed and nothing is spawned."
  (agent-repl-test-daemon--with-harness
    ;; Arrange / Act
    (agent-repl-daemon-schedule-ensure)
    ;; Assert
    (should (null agent-repl-test-daemon--build-runs))))

(provide 'test-daemon)

;;; test-daemon.el ends here
