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

(defvar agent-repl-test-daemon--boot-claim 'held
  "What the stubbed boot-claim probe reports.
`free\=', `held\=' or `undecided\=' name the daemon\='s three exit statuses; an
integer is that exit status verbatim, and `error\=' makes the probe itself
signal, which is a binary that could not be run at all.  The default is
`held\=', because the harness\='s default world has a daemon serving.")

(defvar agent-repl-test-daemon--boot-claim-probes nil
  "Boot-claim probes, newest first: `(BINARY . STATE-DIR)\='.")

(defun agent-repl-test-daemon--claim-status ()
  "The exit status the stubbed boot-claim probe answers."
  (pcase agent-repl-test-daemon--boot-claim
    ('free agent-repl-daemon--claim-free-status)
    ('held agent-repl-daemon--claim-held-status)
    ('undecided 2)
    ((and (pred integerp) status) status)
    (other (error "Unknown stubbed boot-claim answer %S" other))))

(defvar agent-repl-test-daemon--health-answer nil
  "What the stubbed DaemonHealth answers; see `agent-repl-test-daemon--answer'.")

(defvar agent-repl-test-daemon--shutdown-answer nil
  "What the stubbed UpdateShutdownSchedule answers.")

(defvar agent-repl-test-daemon--shutdown-requests nil
  "UpdateShutdownSchedule requests, newest first.")

(defvar agent-repl-test-daemon--link-connect-calls 0
  "How many times the stubbed `agent-repl-link-connect' ran.")

(defvar agent-repl-test-daemon--health-calls 0
  "How many times the stubbed `DaemonHealth' probe ran.
A dead-advertiser cold start dials nothing, so this stays zero -- the
proxy for `no dial' the transport records would otherwise prove.")

(defvar agent-repl-test-daemon--link-conn nil
  "What the stubbed `agent-repl-link-connect' answers.")

(defvar agent-repl-test-daemon--link-up nil
  "What the stubbed `agent-repl-link-up-p' answers.")

(defvar agent-repl-test-daemon--timers nil
  "Captured `run-with-timer' calls, newest first: `(SECONDS . FUNCTION)'.")

(defvar agent-repl-test-daemon--logs nil
  "Captured `(LEVEL . TEXT)' log entries, newest first.")

(defvar agent-repl-test-daemon--addr-file nil
  "The throwaway path the stubbed `daemon.addr' resolver answers.
The removal paths touch a REAL file, so every scenario gets its own
under the temp root rather than the session's state dir.")

(defvar agent-repl-test-daemon--addr-file-contents nil
  "What the throwaway `daemon.addr' holds, or nil when the file is absent.")

(defvar agent-repl-test-daemon--addr-pid nil
  "The pid the stubbed `daemon.addr' pid reader answers, or nil for legacy.")

(defvar agent-repl-test-daemon--alive-pids nil
  "Pids the stubbed `process-attributes' reports as live.
The cold-start triage checks the advertiser's liveness before any dial, so
a scenario names which pids are alive rather than depending on the host.")

(defun agent-repl-test-daemon--write-addr-file (address)
  "Write ADDRESS into the scenario's throwaway `daemon.addr'."
  (setq agent-repl-test-daemon--addr-file-contents address)
  (with-temp-file agent-repl-test-daemon--addr-file (insert address "\n")))

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
         (agent-repl-test-daemon--boot-claim 'held)
         (agent-repl-test-daemon--boot-claim-probes nil)
         (agent-repl-test-daemon--health-answer
          (list :response (list :arm :success
                                :value (list :arm :healthy :value nil))))
         (agent-repl-test-daemon--shutdown-answer
          (list :response (list :arm :success :value nil)))
         (agent-repl-test-daemon--shutdown-requests nil)
         (agent-repl-test-daemon--link-connect-calls 0)
         (agent-repl-test-daemon--health-calls 0)
         (agent-repl-test-daemon--link-conn 'the-connection)
         (agent-repl-test-daemon--link-up nil)
         (agent-repl-test-daemon--timers nil)
         (agent-repl-test-daemon--logs nil)
         (agent-repl--frontend-daemon-process nil)
         (agent-repl-daemon--exit-requested nil)
         (agent-repl-daemon-build-failure nil)
         ;; A scenario that boots into a timeout or a refused launch SETS
         ;; this, and without the reset it stood for the rest of the run --
         ;; "daemon: launch failed" outranks every other arm, so the next
         ;; scenario's segment assertion read the previous one's failure.
         (agent-repl-daemon-launch-failure nil)
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
         (agent-repl-daemon--boot-rejected-address nil)
         (agent-repl-daemon--own-address nil)
         (agent-repl-daemon--lifecycle nil)
         (agent-repl-daemon--lifecycle-timer nil)
         (agent-repl-daemon--workspace-echo-done nil)
         ;; The segment reads the roster's order and the pending-open set,
         ;; so both are reset per scenario: a tab order left standing by an
         ;; earlier test would put a workspace bring-up note in this one's
         ;; mode line.
         (agent-repl-roster--tab-order nil)
         (agent-repl--open-progress (make-hash-table :test 'equal))
         (agent-repl-test-daemon--addr-file
          (make-temp-file "agent-repl-test-daemon-addr-"))
         (agent-repl-test-daemon--addr-file-contents nil)
         (agent-repl-test-daemon--addr-pid nil)
         (agent-repl-test-daemon--alive-pids nil)
         (agent-repl-daemon--ensure-in-flight nil))
     (cl-letf (((symbol-function 'agent-repl-connect-read-daemon-addr)
                (lambda () agent-repl-test-daemon--address))
               ((symbol-function 'agent-repl-connect-read-daemon-addr-pid)
                (lambda () agent-repl-test-daemon--addr-pid))
               ((symbol-function 'process-attributes)
                (lambda (pid)
                  (when (memq pid agent-repl-test-daemon--alive-pids)
                    (list (cons 'state "R")))))
               ((symbol-function 'agent-repl-connect-daemon-addr-file)
                (lambda () agent-repl-test-daemon--addr-file))
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
               ((symbol-function 'agent-repl--frontend-probe-boot-claim)
                (lambda (binary state-dir)
                  (push (cons binary state-dir)
                        agent-repl-test-daemon--boot-claim-probes)
                  (if (eq agent-repl-test-daemon--boot-claim 'error)
                      (error "the boot-claim probe could not run")
                    (agent-repl-test-daemon--claim-status))))
               ((symbol-function 'agent-repl--frontend-artifact-exists-p)
                (lambda (path)
                  (if (eq agent-repl-test-daemon--existing-paths t)
                      t
                    (and (member path agent-repl-test-daemon--existing-paths) t))))
               ((symbol-function 'agent-repl-rpc-daemon-health)
                (lambda (_conn _request &rest keys)
                  (setq agent-repl-test-daemon--health-calls
                        (1+ agent-repl-test-daemon--health-calls))
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
               ((symbol-function 'message) (lambda (&rest _) nil))
               ;; `agent-repl--backend-phase' and `agent-repl--phase-echo' are
               ;; NOT stubbed: they are how the startup phases reach the echo
               ;; area, and the scenarios below assert exactly that.  Both
               ;; end at `agent-repl--emit-message', whose `message' is
               ;; stubbed above, so nothing is printed either way.
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
       (unwind-protect
           (agent-repl-test--recording-popups ,@body)
         (when (and agent-repl-test-daemon--addr-file
                    (file-exists-p agent-repl-test-daemon--addr-file))
           (delete-file agent-repl-test-daemon--addr-file))))))

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
              ((symbol-function 'agent-repl--frontend-run-log-tail) (lambda () "boom"))
              ((symbol-function 'agent-repl--frontend-stdio-log-tail) (lambda () "boom")))
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
               (lambda () "Roots.Default is required"))
              ((symbol-function 'agent-repl--frontend-stdio-log-tail)
               (lambda () "exit status 2")))
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
  "After a clean build the daemon is started with the detaching spawn argv.
The daemon\='s OWN argv is `agent-repl-daemon--argv\=' and is asserted on its
own below; what reaches `make-process\=' is that list behind the detacher,
because a daemon that dies with this Emacs is not a resident service."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil)
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (equal (car (car agent-repl-test-daemon--spawns))
                   (agent-repl-daemon--spawn-argv)))))

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
    (should (member agent-repl-daemon-build-buffer
                    (mapcar #'buffer-name agent-repl-test--popups-shown)))))

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

(ert-deftest agent-repl-test-daemon-stale-address-file-is-removed-before-the-build ()
  "The corpse's `daemon.addr' goes before a fresh daemon is built."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (agent-repl-test-daemon--write-addr-file "127.0.0.1:9001")
    (setq agent-repl-test-daemon--address "127.0.0.1:9001"
          agent-repl-test-daemon--health-answer
          (list :failure (list :kind :transport :message "connection refused")))
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should-not (file-exists-p agent-repl-test-daemon--addr-file))))

(ert-deftest agent-repl-test-daemon-stale-address-removal-is-recorded ()
  "Removing another process's leftover file is stated, never silent."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (agent-repl-test-daemon--write-addr-file "127.0.0.1:9001")
    (setq agent-repl-test-daemon--address "127.0.0.1:9001"
          agent-repl-test-daemon--health-answer
          (list :failure (list :kind :transport :message "connection refused")))
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (agent-repl-test-daemon--logged-p
             :info "elisp.daemon.stale-addr-removed"))))

(ert-deftest agent-repl-test-daemon-boot-wait-refuses-the-address-it-judged-stale ()
  "A `daemon.addr' still holding the dead address is not the new daemon booting.
A cold start read the dead 127.0.0.1:58161 back three milliseconds after
the spawn, called the daemon booted, and linked to a refused port."
  (agent-repl-test-daemon--with-harness
    ;; Arrange: the probe refuses, and the file survives the removal.
    (setq agent-repl-test-daemon--address "127.0.0.1:9001"
          agent-repl-test-daemon--health-answer
          (list :failure (list :kind :transport :message "connection refused")))
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert: the ensure did not settle on the dead address.
    (should (= agent-repl-test-daemon--link-connect-calls 0))))

(ert-deftest agent-repl-test-daemon-boot-wait-records-the-refused-address ()
  "The refusal is stated so a log reader sees WHY boot kept waiting."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address "127.0.0.1:9001"
          agent-repl-test-daemon--health-answer
          (list :failure (list :kind :transport :message "connection refused")))
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (agent-repl-test-daemon--logged-p
             :info "elisp.daemon.boot-addr-rejected"))))

(ert-deftest agent-repl-test-daemon-boot-wait-accepts-the-new-daemons-address ()
  "A DIFFERENT address is the daemon we started, and it is accepted at once."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address "127.0.0.1:9001"
          agent-repl-test-daemon--health-answer
          (list :failure (list :kind :transport :message "connection refused")))
    (agent-repl-daemon-ensure)
    (setq agent-repl-test-daemon--address "127.0.0.1:9002")
    ;; Act
    (agent-repl-daemon--boot-tick)
    ;; Assert
    (should (= agent-repl-test-daemon--link-connect-calls 1))))

;;;; ---- An advertisement whose advertiser is dead ----

(ert-deftest agent-repl-test-daemon-dead-advertiser-addr-is-retired-at-info ()
  "A pid that names no live process retires the address at INFO."
  (agent-repl-test-daemon--with-harness
    ;; Arrange: an advertisement whose pid is not among the live pids.
    (agent-repl-test-daemon--write-addr-file "127.0.0.1:9001")
    (setq agent-repl-test-daemon--address "127.0.0.1:9001"
          agent-repl-test-daemon--addr-pid 4242
          agent-repl-test-daemon--alive-pids nil)
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (agent-repl-test-daemon--logged-p
             :info "elisp.daemon.addr-retired-dead-advertiser"))))

(ert-deftest agent-repl-test-daemon-dead-advertiser-is-not-dialled ()
  "A dead advertiser's address is absent, so the probe never runs."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (agent-repl-test-daemon--write-addr-file "127.0.0.1:9001")
    (setq agent-repl-test-daemon--address "127.0.0.1:9001"
          agent-repl-test-daemon--addr-pid 4242
          agent-repl-test-daemon--alive-pids nil)
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert: no dial means the health probe was never asked.
    (should (= agent-repl-test-daemon--health-calls 0))))

(ert-deftest agent-repl-test-daemon-dead-advertiser-records-no-transport-warn ()
  "A dead advertiser earns none of the stale-addr transport records."
  (agent-repl-test-daemon--with-harness
    ;; Arrange: a transport-failing health answer would fire the stale-addr
    ;; warn IF the probe were taken, so its absence proves the probe was not.
    (agent-repl-test-daemon--write-addr-file "127.0.0.1:9001")
    (setq agent-repl-test-daemon--address "127.0.0.1:9001"
          agent-repl-test-daemon--addr-pid 4242
          agent-repl-test-daemon--alive-pids nil
          agent-repl-test-daemon--health-answer
          (list :failure (list :kind :transport :message "connection refused")))
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should-not (agent-repl-test-daemon--logged-p :warn "elisp.daemon.stale-addr"))))

(ert-deftest agent-repl-test-daemon-dead-advertiser-file-is-retired ()
  "The dead advertiser's `daemon.addr' is removed before the build."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (agent-repl-test-daemon--write-addr-file "127.0.0.1:9001")
    (setq agent-repl-test-daemon--address "127.0.0.1:9001"
          agent-repl-test-daemon--addr-pid 4242
          agent-repl-test-daemon--alive-pids nil)
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should-not (file-exists-p agent-repl-test-daemon--addr-file))))

(ert-deftest agent-repl-test-daemon-dead-advertiser-spawns-a-fresh-daemon ()
  "A dead advertiser leads straight to a build and a start."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (agent-repl-test-daemon--write-addr-file "127.0.0.1:9001")
    (setq agent-repl-test-daemon--address "127.0.0.1:9001"
          agent-repl-test-daemon--addr-pid 4242
          agent-repl-test-daemon--alive-pids nil)
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should agent-repl-test-daemon--spawns)))

(ert-deftest agent-repl-test-daemon-alive-advertiser-that-is-unreachable-still-warns ()
  "A LIVE pid whose daemon refuses the connection is a real transport fault."
  (agent-repl-test-daemon--with-harness
    ;; Arrange: the advertiser is alive, but its daemon answers nothing.
    (agent-repl-test-daemon--write-addr-file "127.0.0.1:9001")
    (setq agent-repl-test-daemon--address "127.0.0.1:9001"
          agent-repl-test-daemon--addr-pid 4242
          agent-repl-test-daemon--alive-pids '(4242)
          agent-repl-test-daemon--health-answer
          (list :failure (list :kind :transport :message "connection refused")))
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (agent-repl-test-daemon--logged-p :warn "elisp.daemon.stale-addr"))))

(ert-deftest agent-repl-test-daemon-alive-advertiser-that-answers-is-adopted ()
  "A LIVE pid whose daemon answers is dialled and adopted, never respawned."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (agent-repl-test-daemon--write-addr-file "127.0.0.1:9001")
    (setq agent-repl-test-daemon--address "127.0.0.1:9001"
          agent-repl-test-daemon--addr-pid 4242
          agent-repl-test-daemon--alive-pids '(4242))
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert: it dialled, and it did not spawn.
    (should (= agent-repl-test-daemon--health-calls 1))
    (should (null agent-repl-test-daemon--spawns))))

(ert-deftest agent-repl-test-daemon-pid-less-legacy-advertisement-is-dialled ()
  "A LEGACY file names no pid, so the cold start falls through to the dial."
  (agent-repl-test-daemon--with-harness
    ;; Arrange: no advertised pid, and a daemon that refuses the connection.
    (agent-repl-test-daemon--write-addr-file "127.0.0.1:9001")
    (setq agent-repl-test-daemon--address "127.0.0.1:9001"
          agent-repl-test-daemon--addr-pid nil
          agent-repl-test-daemon--health-answer
          (list :failure (list :kind :transport :message "connection refused")))
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert: the legacy path dials and then records the stale address.
    (should (= agent-repl-test-daemon--health-calls 1))
    (should (agent-repl-test-daemon--logged-p :warn "elisp.daemon.stale-addr"))))

(ert-deftest agent-repl-test-daemon-advertiser-alive-p-reports-a-live-process ()
  "`process-attributes' answering for a pid means the advertiser is alive."
  (agent-repl-test-daemon--with-harness
    (cl-letf (((symbol-function 'process-attributes)
               (lambda (_pid) (list (cons 'state "R")))))
      ;; Arrange, Act, Assert.
      (should (agent-repl-daemon--advertiser-alive-p 4242)))))

(ert-deftest agent-repl-test-daemon-advertiser-alive-p-reports-a-dead-process ()
  "`process-attributes' answering nil means the advertiser is gone."
  (agent-repl-test-daemon--with-harness
    (cl-letf (((symbol-function 'process-attributes) (lambda (_pid) nil)))
      ;; Arrange, Act, Assert.
      (should-not (agent-repl-daemon--advertiser-alive-p 4242)))))

(ert-deftest agent-repl-test-daemon-advertiser-alive-p-rejects-a-nonpositive-pid ()
  "A non-positive pid is no advertiser at all, never probed for liveness."
  (agent-repl-test-daemon--with-harness
    (cl-letf (((symbol-function 'process-attributes)
               (lambda (_pid) (error "process-attributes must not be reached"))))
      ;; Arrange, Act, Assert.
      (should-not (agent-repl-daemon--advertiser-alive-p 0)))))

(ert-deftest agent-repl-test-daemon-own-daemons-exit-removes-its-address-file ()
  "A daemon THIS Emacs spawned leaves no address behind when it dies."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil)
    (agent-repl-daemon-ensure)
    (setq agent-repl-test-daemon--address "127.0.0.1:9001")
    (agent-repl-daemon--boot-tick)
    (agent-repl-test-daemon--write-addr-file "127.0.0.1:9001")
    (setq agent-repl--frontend-daemon-process 'the-daemon-process)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_object) nil))
              ((symbol-function 'process-exit-status) (lambda (_proc) 1)))
      ;; Act
      (agent-repl-daemon--sentinel 'the-daemon-process "killed\n"))
    ;; Assert
    (should-not (file-exists-p agent-repl-test-daemon--addr-file))))

(ert-deftest agent-repl-test-daemon-own-daemons-exit-keeps-a-successors-address ()
  "A file that has moved on belongs to another daemon and is left alone."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil)
    (agent-repl-daemon-ensure)
    (setq agent-repl-test-daemon--address "127.0.0.1:9001")
    (agent-repl-daemon--boot-tick)
    (agent-repl-test-daemon--write-addr-file "127.0.0.1:9002")
    (setq agent-repl-test-daemon--address "127.0.0.1:9002"
          agent-repl--frontend-daemon-process 'the-daemon-process)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_object) nil))
              ((symbol-function 'process-exit-status) (lambda (_proc) 0)))
      ;; Act
      (agent-repl-daemon--sentinel 'the-daemon-process "finished\n"))
    ;; Assert
    (should (file-exists-p agent-repl-test-daemon--addr-file))))


;;;; ---- The level of an exit follows who asked for it ----

(ert-deftest agent-repl-test-daemon-a-requested-exit-is-recorded-at-info ()
  "The daemon obeying a shutdown this editor asked for is not a warning."
  (agent-repl-test-daemon--with-harness
    ;; Arrange: the editor asks, and the daemon accepts.
    (agent-repl-frontend-daemon-stop)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_object) nil))
              ((symbol-function 'process-exit-status) (lambda (_proc) 0)))
      ;; Act
      (agent-repl-daemon--sentinel 'the-daemon-process "finished\n"))
    ;; Assert
    (should (agent-repl-test-daemon--logged-p
             :info "elisp.daemon.exited status=0 event=finished requested=t"))))

(ert-deftest agent-repl-test-daemon-a-requested-exit-raises-no-warning ()
  "The requested exit is recorded ONCE, and not also at WARN."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (agent-repl-frontend-daemon-stop)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_object) nil))
              ((symbol-function 'process-exit-status) (lambda (_proc) 0)))
      ;; Act
      (agent-repl-daemon--sentinel 'the-daemon-process "finished\n"))
    ;; Assert
    (should-not (agent-repl-test-daemon--logged-p :warn "elisp.daemon.exited"))))

(ert-deftest agent-repl-test-daemon-an-unrequested-clean-exit-stays-a-warning ()
  "A daemon leaving on its own stopped work this editor believed was served."
  (agent-repl-test-daemon--with-harness
    ;; Arrange: nothing asked.
    (cl-letf (((symbol-function 'process-live-p) (lambda (_object) nil))
              ((symbol-function 'process-exit-status) (lambda (_proc) 0)))
      ;; Act
      (agent-repl-daemon--sentinel 'the-daemon-process "finished\n"))
    ;; Assert
    (should (agent-repl-test-daemon--logged-p
             :warn "elisp.daemon.exited status=0 event=finished requested=nil"))))

(ert-deftest agent-repl-test-daemon-a-clean-exit-during-a-handover-is-info ()
  "The outgoing daemon of a rolling deploy exits on its own announcement: INFO."
  (agent-repl-test-daemon--with-harness
    ;; Arrange: a successor stands.
    (cl-letf (((symbol-function 'process-live-p) (lambda (_object) nil))
              ((symbol-function 'process-exit-status) (lambda (_proc) 0))
              ((symbol-function 'agent-repl-link-successor) (lambda () 'the-successor))
              ((symbol-function 'agent-repl-link-successor-pending-p) (lambda () nil)))
      ;; Act
      (agent-repl-daemon--sentinel 'the-daemon-process "finished\n"))
    ;; Assert
    (should (agent-repl-test-daemon--logged-p
             :info "elisp.daemon.exited status=0 event=finished requested=handover"))))

(ert-deftest agent-repl-test-daemon-a-clean-exit-with-a-successor-pending-is-info ()
  "A successor dialed but not yet accepted is still an announced handover."
  (agent-repl-test-daemon--with-harness
    ;; Arrange: a successor is pending.
    (cl-letf (((symbol-function 'process-live-p) (lambda (_object) nil))
              ((symbol-function 'process-exit-status) (lambda (_proc) 0))
              ((symbol-function 'agent-repl-link-successor) (lambda () nil))
              ((symbol-function 'agent-repl-link-successor-pending-p) (lambda () t)))
      ;; Act
      (agent-repl-daemon--sentinel 'the-daemon-process "finished\n"))
    ;; Assert
    (should-not (agent-repl-test-daemon--logged-p :warn "elisp.daemon.exited"))))

(ert-deftest agent-repl-test-daemon-a-failed-exit-during-a-handover-is-still-an-error ()
  "A non-zero exit is the daemon's own failure even mid-handover."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (cl-letf (((symbol-function 'process-live-p) (lambda (_object) nil))
              ((symbol-function 'process-exit-status) (lambda (_proc) 2))
              ((symbol-function 'agent-repl-link-successor) (lambda () 'the-successor))
              ((symbol-function 'agent-repl-link-successor-pending-p) (lambda () nil)))
      ;; Act
      (agent-repl-daemon--sentinel 'the-daemon-process "exited abnormally with code 2\n"))
    ;; Assert
    (should (agent-repl-test-daemon--logged-p :error "elisp.daemon.exited status=2"))))

(ert-deftest agent-repl-test-daemon-an-unrequested-failed-exit-is-an-error ()
  "A non-zero status is the daemon reporting its own failure."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (cl-letf (((symbol-function 'process-live-p) (lambda (_object) nil))
              ((symbol-function 'process-exit-status) (lambda (_proc) 2)))
      ;; Act
      (agent-repl-daemon--sentinel 'the-daemon-process "exited abnormally\n"))
    ;; Assert
    (should (agent-repl-test-daemon--logged-p
             :error "elisp.daemon.exited status=2 event=exited abnormally requested=nil"))))

(ert-deftest agent-repl-test-daemon-a-request-excuses-only-the-exit-it-ordered ()
  "The order is CONSUMED, so the next departure is heard in full."
  (agent-repl-test-daemon--with-harness
    ;; Arrange: one requested exit has already been recorded.
    (agent-repl-frontend-daemon-stop)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_object) nil))
              ((symbol-function 'process-exit-status) (lambda (_proc) 0)))
      (agent-repl-daemon--sentinel 'the-daemon-process "finished\n")
      ;; Act: a second daemon goes away with nothing having asked.
      (agent-repl-daemon--sentinel 'the-daemon-process "finished\n"))
    ;; Assert
    (should (agent-repl-test-daemon--logged-p
             :warn "elisp.daemon.exited status=0 event=finished requested=nil"))))

(ert-deftest agent-repl-test-daemon-a-refused-stop-withdraws-the-request ()
  "A shutdown the daemon refused excuses no later departure."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--shutdown-answer
          (list :response (list :arm :error :value nil)))
    (agent-repl-frontend-daemon-stop)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_object) nil))
              ((symbol-function 'process-exit-status) (lambda (_proc) 0)))
      ;; Act
      (agent-repl-daemon--sentinel 'the-daemon-process "finished\n"))
    ;; Assert
    (should (agent-repl-test-daemon--logged-p
             :warn "elisp.daemon.exited status=0 event=finished requested=nil"))))

(ert-deftest agent-repl-test-daemon-an-undelivered-stop-withdraws-the-request ()
  "A shutdown that never reached the daemon excuses no later departure."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--shutdown-answer
          (list :failure (list :detail "the link went away")))
    (agent-repl-frontend-daemon-stop)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_object) nil))
              ((symbol-function 'process-exit-status) (lambda (_proc) 0)))
      ;; Act
      (agent-repl-daemon--sentinel 'the-daemon-process "finished\n"))
    ;; Assert
    (should (agent-repl-test-daemon--logged-p
             :warn "elisp.daemon.exited status=0 event=finished requested=nil"))))

(ert-deftest agent-repl-test-daemon-a-new-daemon-inherits-no-shutdown-order ()
  "An order given to a predecessor does not excuse its successor's death."
  (agent-repl-test-daemon--with-harness
    ;; Arrange: an order stands, and a fresh daemon is then spawned.
    (setq agent-repl-daemon--exit-requested t
          agent-repl-test-daemon--address nil)
    (agent-repl-daemon-ensure)
    ;; Act / Assert
    (should-not agent-repl-daemon--exit-requested)))

(ert-deftest agent-repl-test-daemon-a-restarts-exit-is-recorded-at-info ()
  "THE 2026-09-13 DEPLOY.  The restart's ensure spawns the successor before
the predecessor's sentinel runs -- the departure wait keys on the boot
claim, which the kernel frees the instant the process ends -- so the order
has to survive the spawn to excuse the exit it was given for."
  (agent-repl-test-daemon--with-harness
    ;; Arrange: a daemon this Emacs spawned, and a claim already free.
    (setq agent-repl--frontend-daemon-process 'the-predecessor-process
          agent-repl-test-daemon--boot-claim 'free)
    (cl-letf (((symbol-function 'agent-repl--frontend-spawn-daemon)
               (lambda (argv environment)
                 (push (cons argv environment) agent-repl-test-daemon--spawns)
                 'the-successor-process))
              ((symbol-function 'process-live-p) (lambda (_object) nil))
              ((symbol-function 'process-exit-status) (lambda (_proc) 0)))
      (agent-repl-frontend-daemon-restart)
      ;; Act: the predecessor's sentinel fires after the successor is up.
      (agent-repl-daemon--sentinel 'the-predecessor-process "finished\n"))
    ;; Assert: the successor really was spawned first, so the order survived it.
    (should agent-repl-test-daemon--spawns)
    (should (agent-repl-test-daemon--logged-p
             :info "elisp.daemon.exited status=0 event=finished requested=t"))))

(ert-deftest agent-repl-test-daemon-a-refused-restart-withdraws-the-order ()
  "A restart the daemon refused is no order, and its later exit is unasked."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl--frontend-daemon-process 'the-predecessor-process
          agent-repl-test-daemon--shutdown-answer
          (list :response (list :arm :error :value nil)))
    (agent-repl-frontend-daemon-restart)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_object) nil))
              ((symbol-function 'process-exit-status) (lambda (_proc) 0)))
      ;; Act
      (agent-repl-daemon--sentinel 'the-predecessor-process "finished\n"))
    ;; Assert
    (should (agent-repl-test-daemon--logged-p
             :warn "elisp.daemon.exited status=0 event=finished requested=nil"))))

(ert-deftest agent-repl-test-daemon-a-foreign-daemons-exit-removes-no-address-file ()
  "Emacs retires only the address of the daemon it started itself."
  (agent-repl-test-daemon--with-harness
    ;; Arrange: no own address was ever published.
    (agent-repl-test-daemon--write-addr-file "127.0.0.1:9001")
    (setq agent-repl-test-daemon--address "127.0.0.1:9001"
          agent-repl--frontend-daemon-process 'the-daemon-process)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_object) nil))
              ((symbol-function 'process-exit-status) (lambda (_proc) 1)))
      ;; Act
      (agent-repl-daemon--sentinel 'the-daemon-process "killed\n"))
    ;; Assert
    (should (file-exists-p agent-repl-test-daemon--addr-file))))

;;;; ---- The sentinel's quit deferral ----
;;
;; The recording is a critical section: a `C-g' between retiring
;; `daemon.addr' and forgetting the process leaves the address file naming a
;; dead daemon that the next cold start reads back as readiness.

(ert-deftest agent-repl-test-daemon-sentinel-defers-a-quit-that-lands-in-it ()
  "A C-g arriving inside the exit recording is held for the command loop."
  (agent-repl-test-daemon--with-harness
    (cl-letf (((symbol-function 'process-live-p) (lambda (_object) nil))
              ((symbol-function 'agent-repl-daemon--record-exit)
               (lambda (&rest _) (setq quit-flag t))))
      ;; Act / Assert
      (should (agent-repl-test--quit-deferred-p
                (agent-repl-daemon--sentinel 'the-daemon-process "killed\n"))))))

(ert-deftest agent-repl-test-daemon-sentinel-records-the-exit-despite-a-pending-quit ()
  "A quit already requested does not stop the address file being retired."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil)
    (agent-repl-daemon-ensure)
    (setq agent-repl-test-daemon--address "127.0.0.1:9001")
    (agent-repl-daemon--boot-tick)
    (agent-repl-test-daemon--write-addr-file "127.0.0.1:9001")
    (setq agent-repl--frontend-daemon-process 'the-daemon-process)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_object) nil))
              ((symbol-function 'process-exit-status) (lambda (_proc) 1)))
      ;; Act
      (agent-repl-test--with-pending-quit
        (agent-repl-daemon--sentinel 'the-daemon-process "killed\n")))
    ;; Assert
    (should-not (file-exists-p agent-repl-test-daemon--addr-file))))

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
    ;; Arrange: the daemon's process is already gone, so its claim is free.
    (setq agent-repl-test-daemon--boot-claim 'free)
    ;; Act
    (agent-repl-frontend-daemon-restart)
    ;; Assert
    (should (and agent-repl-test-daemon--shutdown-requests
                 agent-repl-test-daemon--build-runs))))

(ert-deftest agent-repl-test-daemon-restart-does-not-ensure-while-the-daemon-is-still-there ()
  "The ensure half waits: a daemon that still answers would be RE-ADOPTED."
  (agent-repl-test-daemon--with-harness
    ;; Arrange: the departing daemon still holds the boot claim.
    (setq agent-repl-test-daemon--address "127.0.0.1:9999"
          agent-repl-test-daemon--boot-claim 'held)
    ;; Act
    (agent-repl-frontend-daemon-restart)
    ;; Assert
    (should (null agent-repl-test-daemon--build-runs))))

(ert-deftest agent-repl-test-daemon-restart-ensures-once-the-claim-is-released ()
  "The BOOT CLAIM's release is the departure, and it releases the ensure."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address "127.0.0.1:9999")
    (agent-repl-frontend-daemon-restart)
    ;; Act: the daemon's process ends, which frees the claim.
    (setq agent-repl-test-daemon--boot-claim 'free
          agent-repl-test-daemon--address nil)
    (agent-repl-daemon--departure-tick)
    ;; Assert
    (should agent-repl-test-daemon--build-runs)))

(ert-deftest agent-repl-test-daemon-restart-waits-while-the-claim-outlives-the-address ()
  "THE 2026-09-12 RESTART.  The daemon withdraws `daemon.addr' at the start
of its shutdown and holds the boot claim until its process ends; a
replacement spawned into that window loses the claim and exits, and the
restart destroys the daemon instead of replacing it."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address "127.0.0.1:9999")
    (agent-repl-frontend-daemon-restart)
    ;; Act: the address is gone but the outgoing daemon is still holding on.
    (setq agent-repl-test-daemon--address nil
          agent-repl-test-daemon--boot-claim 'held)
    (agent-repl-daemon--departure-tick)
    ;; Assert
    (should (null agent-repl-test-daemon--build-runs))))

(ert-deftest agent-repl-test-daemon-departure-probes-the-state-root-claim ()
  "The claim asked about is THIS Emacs's state root, named on the probe."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (agent-repl-frontend-daemon-restart)
    ;; Act
    (agent-repl-daemon--departure-tick)
    ;; Assert
    (should (equal (cdar agent-repl-test-daemon--boot-claim-probes)
                   (directory-file-name (agent-repl--global-state-dir))))))

(ert-deftest agent-repl-test-daemon-departure-does-not-fire-on-an-undecidable-claim ()
  "A claim that could not be decided is NOT a free one."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (agent-repl-frontend-daemon-restart)
    ;; Act
    (setq agent-repl-test-daemon--boot-claim 'undecided)
    (agent-repl-daemon--departure-tick)
    ;; Assert
    (should (null agent-repl-test-daemon--build-runs))))

(ert-deftest agent-repl-test-daemon-undecidable-claim-is-warned-about ()
  "The undecidable claim is surfaced, never silently waited out."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (agent-repl-frontend-daemon-restart)
    ;; Act
    (setq agent-repl-test-daemon--boot-claim 'undecided)
    (agent-repl-daemon--departure-tick)
    ;; Assert
    (should (agent-repl-test-daemon--logged-p
             :warn "elisp.daemon.claim-probe-undecided"))))

(ert-deftest agent-repl-test-daemon-unrunnable-claim-probe-is-warned-about ()
  "A probe that could not run at all is the same answer: undecided, warned."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (agent-repl-frontend-daemon-restart)
    ;; Act
    (setq agent-repl-test-daemon--boot-claim 'error)
    (agent-repl-daemon--departure-tick)
    ;; Assert
    (should (agent-repl-test-daemon--logged-p
             :warn "elisp.daemon.claim-probe-undecided"))))

(ert-deftest agent-repl-test-daemon-departure-fires-on-a-free-claim-with-a-stale-address ()
  "A daemon that died without withdrawing has still departed: the claim is
free, and the stale address it left behind is what the boot overwrites."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address "127.0.0.1:9999")
    (agent-repl-frontend-daemon-restart)
    ;; Act
    (setq agent-repl-test-daemon--boot-claim 'free)
    (agent-repl-daemon--departure-tick)
    ;; Assert: the departure fired, and what the ensure then makes of the
    ;; stale address is the boot's business, not the departure wait's.
    (should (agent-repl-test-daemon--logged-p
             :info "elisp.daemon.restart-ensure"))))

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

;;;; ---- The bring-up lifecycle in the mode line ----
;;
;; Everything between the spawn and link-up used to leave the segment empty,
;; which reads exactly like an editor that has finished starting.  On the
;; cold start these tests come from, that silence covered ten seconds.

(ert-deftest agent-repl-test-daemon-a-spawn-says-the-daemon-is-starting ()
  "From the spawn until an address exists the mode line says so."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil)
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (equal agent-repl-daemon-mode-line-segment "daemon: starting…"))))

(ert-deftest agent-repl-test-daemon-a-published-address-says-linking ()
  "An address exists but the link does not yet: that is its own state."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil)
    (agent-repl-daemon-ensure)
    (setq agent-repl-test-daemon--address "127.0.0.1:9001")
    ;; Act
    (agent-repl-daemon--boot-tick)
    ;; Assert
    (should (equal agent-repl-daemon-mode-line-segment "daemon: linking…"))))

(ert-deftest agent-repl-test-daemon-link-up-on-an-own-daemon-says-ready ()
  "A daemon this Emacs started and linked to is ready."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl--frontend-daemon-process 'the-daemon-process)
    (cl-letf (((symbol-function 'process-live-p)
               (lambda (object) (eq object 'the-daemon-process))))
      ;; Act
      (agent-repl-daemon-on-link-up 'the-connection))
    ;; Assert
    (should (equal agent-repl-daemon-mode-line-segment "daemon: ready"))))

(ert-deftest agent-repl-test-daemon-link-up-on-a-foreign-daemon-says-adopted ()
  "A daemon this Emacs merely attached to is adopted, not started."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl--frontend-daemon-process nil)
    ;; Act
    (agent-repl-daemon-on-link-up 'the-connection)
    ;; Assert
    (should (equal agent-repl-daemon-mode-line-segment "daemon: adopted"))))

(ert-deftest agent-repl-test-daemon-a-settled-lifecycle-clears-itself ()
  "An outcome is worth saying and not worth keeping."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (agent-repl-daemon--set-lifecycle 'ready)
    ;; Act
    (agent-repl-daemon--clear-lifecycle)
    ;; Assert
    (should (null agent-repl-daemon-mode-line-segment))))

(ert-deftest agent-repl-test-daemon-a-settled-lifecycle-schedules-its-own-clear ()
  "`ready' takes itself down after the status display window."
  (agent-repl-test-daemon--with-harness
    ;; Act
    (agent-repl-daemon--set-lifecycle 'ready)
    ;; Assert
    (should (equal (car (car agent-repl-test-daemon--timers))
                   agent-repl-daemon-build-status-display-seconds))))

(ert-deftest agent-repl-test-daemon-an-in-flight-lifecycle-schedules-no-clear ()
  "`starting' is not an outcome, so nothing takes it down but the next state."
  (agent-repl-test-daemon--with-harness
    ;; Act
    (agent-repl-daemon--set-lifecycle 'starting)
    ;; Assert
    (should (null agent-repl-daemon--lifecycle-timer))))

(ert-deftest agent-repl-test-daemon-every-lifecycle-transition-is-recorded ()
  "A transition nobody can read afterwards is not a transition."
  (agent-repl-test-daemon--with-harness
    ;; Act
    (agent-repl-daemon--set-lifecycle 'linking)
    ;; Assert
    (should (agent-repl-test-daemon--logged-p
             :info "elisp.daemon.lifecycle state=linking"))))

(ert-deftest agent-repl-test-daemon-a-build-failure-outranks-the-lifecycle ()
  "A standing failure is what the user has to act on, whatever else is up."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (agent-repl-daemon--set-lifecycle 'starting)
    ;; Act
    (setq agent-repl-daemon-build-failure "exit 2")
    (agent-repl-daemon--refresh-segment)
    ;; Assert
    (should (equal agent-repl-daemon-mode-line-segment "daemon: build failed"))))

(ert-deftest agent-repl-test-daemon-a-running-build-outranks-the-lifecycle ()
  "The build is the longest leg, so it keeps the segment while it runs."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (agent-repl-daemon--set-lifecycle 'starting)
    ;; Act
    (agent-repl-daemon--set-build-status 'building nil)
    ;; Assert
    (should (equal agent-repl-daemon-mode-line-segment
                   "building agent-repl stack…"))))

(ert-deftest agent-repl-test-daemon-the-lifecycle-outranks-the-just-built-note ()
  "\"built in 1.0s\" must not hide the bring-up that is still going."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (agent-repl-daemon--set-build-status 'built 1.0)
    ;; Act
    (agent-repl-daemon--set-lifecycle 'linking)
    ;; Assert
    (should (equal agent-repl-daemon-mode-line-segment "daemon: linking…"))))

;;;; ---- The workspace bring-up count ----

(ert-deftest agent-repl-test-daemon-opening-workspaces-are-counted-in-the-segment ()
  "While roster tabs are still painting the mode line says how many are up."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-roster--tab-order '("ws-1" "ws-2" "ws-3"))
    (puthash "ws-1" (list :phase :requested) agent-repl--open-progress)
    ;; Act
    (agent-repl-daemon--refresh-segment)
    ;; Assert
    (should (equal agent-repl-daemon-mode-line-segment
                   "workspaces: opening 2/3"))))

(ert-deftest agent-repl-test-daemon-a-painted-workspace-leaves-the-count ()
  "A workspace whose webview finished loading is painted, not opening."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-roster--tab-order '("ws-1" "ws-2"))
    (puthash "ws-1" (list :phase :loaded) agent-repl--open-progress)
    ;; Act
    (agent-repl-daemon--refresh-segment)
    ;; Assert
    (should (null agent-repl-daemon-mode-line-segment))))

(ert-deftest agent-repl-test-daemon-a-workspace-with-no-tab-is-not-counted ()
  "The denominator is the ROSTER's tabs, so an open elsewhere is not ours."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-roster--tab-order '("ws-1"))
    (puthash "ws-other" (list :phase :requested) agent-repl--open-progress)
    ;; Act
    (agent-repl-daemon--refresh-segment)
    ;; Assert
    (should (null agent-repl-daemon-mode-line-segment))))

(ert-deftest agent-repl-test-daemon-an-open-progress-change-repaints-the-segment ()
  "open-progress publishing a change is what moves the count."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-roster--tab-order '("ws-1" "ws-2"))
    (puthash "ws-1" (list :phase :requested) agent-repl--open-progress)
    (agent-repl-daemon--refresh-segment)
    (remhash "ws-1" agent-repl--open-progress)
    ;; Act
    (agent-repl-daemon-on-open-progress-change)
    ;; Assert
    (should (null agent-repl-daemon-mode-line-segment))))

(ert-deftest agent-repl-test-daemon-the-lifecycle-outranks-the-workspace-count ()
  "The daemon has to be up before its workspaces mean anything."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-roster--tab-order '("ws-1" "ws-2"))
    (puthash "ws-1" (list :phase :requested) agent-repl--open-progress)
    ;; Act
    (agent-repl-daemon--set-lifecycle 'linking)
    ;; Assert
    (should (equal agent-repl-daemon-mode-line-segment "daemon: linking…"))))

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


;;;; ---- The bring-up phases in the MINIBUFFER ----
;;
;; The mode line says where bring-up has got to in a corner of the frame
;; and then clears itself; these assert the other half, the short line the
;; user actually reads while a cold start runs.  Every echo is produced by
;; the module's own logging function in echo mode, so the capture point is
;; `agent-repl--emit-message' -- the one chokepoint separating agent-repl's
;; quiet sink from its loud one -- and a line captured with ECHO nil is a
;; line that was RECORDED but never reached the echo area.

(defvar agent-repl-test-daemon--echoes nil
  "Echo-area lines captured during a scenario, oldest first.")

(defmacro agent-repl-test-daemon--capturing-echoes (&rest body)
  "Run BODY with every loud `agent-repl--emit-message' line captured."
  (declare (indent 0))
  `(let ((agent-repl-test-daemon--echoes nil))
     (cl-letf (((symbol-function 'agent-repl--emit-message)
                (lambda (text &optional echo)
                  (when echo
                    (setq agent-repl-test-daemon--echoes
                          (append agent-repl-test-daemon--echoes (list text))))
                  text)))
       ,@body)))

(ert-deftest agent-repl-test-daemon-the-starting-phase-echoes-once ()
  "The spawn says so in the minibuffer, and says it exactly once."
  (agent-repl-test-daemon--with-harness
    (agent-repl-test-daemon--capturing-echoes
      ;; Act
      (agent-repl-daemon--set-lifecycle 'starting)
      ;; Assert
      (should (equal agent-repl-test-daemon--echoes
                     '("agent-repl: starting the daemon…"))))))

(ert-deftest agent-repl-test-daemon-the-linking-phase-echoes-once ()
  "The wait between an address and a standing link is the invisible leg."
  (agent-repl-test-daemon--with-harness
    (agent-repl-test-daemon--capturing-echoes
      ;; Act
      (agent-repl-daemon--set-lifecycle 'linking)
      ;; Assert
      (should (equal agent-repl-test-daemon--echoes
                     '("agent-repl: connecting to the daemon…"))))))

(ert-deftest agent-repl-test-daemon-the-ready-outcome-echoes-once ()
  "A daemon this Emacs started reaching link-up is `ready'."
  (agent-repl-test-daemon--with-harness
    (agent-repl-test-daemon--capturing-echoes
      ;; Act
      (agent-repl-daemon--set-lifecycle 'ready)
      ;; Assert
      (should (equal agent-repl-test-daemon--echoes
                     '("agent-repl: connected to the daemon."))))))

(ert-deftest agent-repl-test-daemon-the-adopted-outcome-echoes-once ()
  "A daemon this Emacs attached to is `adopted', and says which it was."
  (agent-repl-test-daemon--with-harness
    (agent-repl-test-daemon--capturing-echoes
      ;; Act
      (agent-repl-daemon--set-lifecycle 'adopted)
      ;; Assert
      (should (equal agent-repl-test-daemon--echoes
                     '("agent-repl: connected to the daemon."))))))

(ert-deftest agent-repl-test-daemon-a-phase-while-the-minibuffer-is-busy-is-not-echoed ()
  "The echo area is the user's prompt; progress must not type over it."
  (agent-repl-test-daemon--with-harness
    (agent-repl-test-daemon--capturing-echoes
      (cl-letf (((symbol-function 'minibuffer-depth) (lambda () 1)))
        ;; Act
        (agent-repl-daemon--set-lifecycle 'linking))
      ;; Assert
      (should (null agent-repl-test-daemon--echoes)))))

(ert-deftest agent-repl-test-daemon-a-phase-while-the-minibuffer-is-busy-is-still-recorded ()
  "Suppressed is not dropped: the transition is still in the log."
  (agent-repl-test-daemon--with-harness
    (agent-repl-test-daemon--capturing-echoes
      (cl-letf (((symbol-function 'minibuffer-depth) (lambda () 1)))
        ;; Act
        (agent-repl-daemon--set-lifecycle 'linking))
      ;; Assert
      (should (agent-repl-test-daemon--logged-p
               :info "elisp.daemon.lifecycle state=linking")))))

(ert-deftest agent-repl-test-daemon-the-spawn-echoes-the-starting-phase-once ()
  "The spawn records its own line, but the TRANSITION owns the echo."
  (agent-repl-test-daemon--with-harness
    (agent-repl-test-daemon--capturing-echoes
      ;; Act
      (agent-repl-daemon--start)
      ;; Assert
      (should (equal (seq-filter
                      (lambda (line)
                        (string-match-p "starting the daemon" line))
                      agent-repl-test-daemon--echoes)
                     '("agent-repl: starting the daemon…"))))))

(ert-deftest agent-repl-test-daemon-a-running-build-echoes-once ()
  "The build is the leg that blocks the frame, so it announces itself."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--existing-paths t
          agent-repl-test-daemon--build-defer t)
    (agent-repl-test-daemon--capturing-echoes
      ;; Act
      (agent-repl-daemon--build nil #'ignore)
      ;; Assert
      (should (equal agent-repl-test-daemon--echoes
                     '("agent-repl: building the daemon…"))))))

(ert-deftest agent-repl-test-daemon-a-finished-build-echoes-its-outcome ()
  "A build that finished is worth one line and not worth keeping."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--existing-paths t)
    (agent-repl-test-daemon--capturing-echoes
      ;; Act
      (agent-repl-daemon--build nil #'ignore)
      ;; Assert
      (should (member "agent-repl: daemon built." agent-repl-test-daemon--echoes)))))

(ert-deftest agent-repl-test-daemon-adopting-a-running-daemon-says-it-was-found ()
  "A daemon this Emacs did not start is FOUND, and says so before connecting."
  (agent-repl-test-daemon--with-harness
    (agent-repl-test-daemon--capturing-echoes
      ;; Act
      (agent-repl-daemon--report-provenance "127.0.0.1:9")
      ;; Assert
      (should (equal agent-repl-test-daemon--echoes
                     '("agent-repl: found a running daemon."
                       "agent-repl: connecting to the daemon…"))))))

(ert-deftest agent-repl-test-daemon-a-build-failure-echoes-through-the-log-function ()
  "A failure the user must act on reaches the echo area as a LOGGED line."
  (agent-repl-test-daemon--with-harness
    (agent-repl-test-daemon--capturing-echoes
      ;; Act
      (agent-repl-daemon--report-build-failure "build failed (exit 2)")
      ;; Assert
      (should (equal agent-repl-test-daemon--echoes
                     '("agent-repl: build failed (exit 2)"))))))

(ert-deftest agent-repl-test-daemon-a-launch-failure-echoes-through-the-log-function ()
  "Same one-call rule for the daemon that never launched at all."
  (agent-repl-test-daemon--with-harness
    (agent-repl-test-daemon--capturing-echoes
      ;; Act
      (agent-repl-daemon--report-launch-failure "the daemon needs a state root")
      ;; Assert
      (should (equal agent-repl-test-daemon--echoes
                     '("agent-repl: the daemon needs a state root"))))))

(ert-deftest agent-repl-test-daemon-a-failure-echoes-even-while-the-minibuffer-is-busy ()
  "The guard covers PROGRESS, never a failure: this one is worth interrupting."
  (agent-repl-test-daemon--with-harness
    (agent-repl-test-daemon--capturing-echoes
      (cl-letf (((symbol-function 'minibuffer-depth) (lambda () 1)))
        ;; Act
        (agent-repl-daemon--report-launch-failure "no state root"))
      ;; Assert
      (should (equal agent-repl-test-daemon--echoes
                     '("agent-repl: no state root"))))))

;;;; ---- The open-progress feed moves the segment, never a line ----

(ert-deftest agent-repl-test-daemon-an-open-progress-change-echoes-nothing ()
  "The startup's workspace lines are startup.el's; this feed only repaints."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-roster--tab-order '("ws-1" "ws-2"))
    (puthash "ws-1" (list :phase :loaded) agent-repl--open-progress)
    (agent-repl-test-daemon--capturing-echoes
      ;; Act
      (agent-repl-daemon-on-open-progress-change)
      ;; Assert
      (should (null agent-repl-test-daemon--echoes)))))

;;;; ---- The spawn is DETACHED: the daemon outlives this Emacs ----

(ert-deftest agent-repl-test-daemon-spawn-argv-leads-with-the-detaching-shell ()
  "What Emacs spawns is the detacher, never the daemon binary directly.
A plain `make-process' child takes Emacs's exit-time SIGHUP and dies with
the editor, which is what made the adopt-on-restart path unreachable."
  ;; Arrange
  (let ((agent-repl-daemon-command '("/bin/claude-repld"))
        (agent-repl-daemon-default-config-dir "/a")
        (agent-repl-daemon-multi-repo-config-dir "/b"))
    ;; Act
    (let ((argv (agent-repl-daemon--spawn-argv)))
      ;; Assert
      (should (equal (car argv) agent-repl-daemon--detach-shell)))))

(ert-deftest agent-repl-test-daemon-spawn-argv-ignores-sighup ()
  "SIGHUP is set to ignore BEFORE the exec, so Emacs's exit cannot end it.
An ignored disposition survives `exec', and the daemon asks the kernel
for SIGINT/SIGTERM/SIGQUIT and no others, so it stays ignored."
  ;; Arrange
  (let ((agent-repl-daemon-command '("/bin/claude-repld"))
        (agent-repl-daemon-default-config-dir "/a")
        (agent-repl-daemon-multi-repo-config-dir "/b"))
    ;; Act
    (let ((argv (agent-repl-daemon--spawn-argv)))
      ;; Assert
      (should (string-match-p "trap '' HUP" (nth 2 argv))))))

(ert-deftest agent-repl-test-daemon-spawn-argv-execs-the-daemon ()
  "The detacher EXECS, so the pid Emacs holds is the daemon's own.
Without the exec the process object would name a shell that exits at
once, and the boot wait would call every cold start a dead daemon."
  ;; Arrange
  (let ((agent-repl-daemon-command '("/bin/claude-repld"))
        (agent-repl-daemon-default-config-dir "/a")
        (agent-repl-daemon-multi-repo-config-dir "/b"))
    ;; Act
    (let ((argv (agent-repl-daemon--spawn-argv)))
      ;; Assert
      (should (string-match-p "exec \"\\$@\"" (nth 2 argv))))))

(ert-deftest agent-repl-test-daemon-spawn-argv-carries-the-daemon-argv-verbatim ()
  "The daemon's own argv rides behind the detacher, unchanged.
It is what the daemon reads as its `argv' after the exec, so every flag
the account resolver requires has to survive the wrapping."
  ;; Arrange
  (let ((agent-repl-daemon-command '("/bin/claude-repld"))
        (agent-repl-daemon-default-config-dir "/a")
        (agent-repl-daemon-multi-repo-config-dir "/b"))
    ;; Act
    (let* ((argv (agent-repl-daemon--spawn-argv))
           (daemon-argv (agent-repl-daemon--argv)))
      ;; Assert
      (should (equal (last argv (length daemon-argv)) daemon-argv)))))

(ert-deftest agent-repl-test-daemon-spawn-argv-redirects-stdio-to-a-file ()
  "The daemon's output goes to a FILE, not to a pipe that dies with Emacs.
Left on Emacs's pipe, the daemon's first line after the editor exits
would meet a closed read end and take it down with SIGPIPE — a detach
that only held until the daemon next spoke."
  ;; Arrange
  (let ((agent-repl-daemon-command '("/bin/claude-repld"))
        (agent-repl-daemon-default-config-dir "/a")
        (agent-repl-daemon-multi-repo-config-dir "/b"))
    ;; Act
    (let ((argv (agent-repl-daemon--spawn-argv)))
      ;; Assert
      (should (equal (nth 3 argv) (agent-repl-daemon--stdio-log-path))))))

(ert-deftest agent-repl-test-daemon-spawn-argv-makes-the-log-directory ()
  "A first-ever cold start has no `logs/' yet, and a redirection into a
missing directory would fail the spawn outright."
  ;; Arrange
  (let ((agent-repl-daemon-command '("/bin/claude-repld"))
        (agent-repl-daemon-default-config-dir "/a")
        (agent-repl-daemon-multi-repo-config-dir "/b"))
    ;; Act
    (let ((argv (agent-repl-daemon--spawn-argv)))
      ;; Assert
      (should (equal (nth 4 argv)
                     (directory-file-name
                      (file-name-directory (agent-repl-daemon--stdio-log-path))))))))

(ert-deftest agent-repl-test-daemon-the-stdio-log-is-not-the-run-log ()
  "Raw stderr never lands in the contract JSONL every reader parses."
  ;; Arrange / Act / Assert
  (should-not (equal (agent-repl-daemon--stdio-log-path)
                     (agent-repl--global-state-file "logs/daemon.run.log"))))

(ert-deftest agent-repl-test-daemon-the-detached-spawn-is-really-detached ()
  "END TO END over the real shell: the wrapper survives a SIGHUP and execs.
Drives `agent-repl-daemon--spawn-argv' against a stub `daemon' that
records its own pid and sleeps, then SIGHUPs that pid — the detacher's
whole promise is that the process is still there afterwards."
  ;; Arrange
  (let* ((dir (make-temp-file "agent-repl-test-detach-" t))
         (stub (expand-file-name "stub.sh" dir))
         (pidfile (expand-file-name "pid" dir))
         (agent-repl-daemon-command (list stub))
         (agent-repl-daemon-default-config-dir "/a")
         (agent-repl-daemon-multi-repo-config-dir "/b"))
    (unwind-protect
        (progn
          ;; THE PID LANDS BY RENAME, never by a redirect into the watched
          ;; name: `>' creates the file before it writes, so the wait below
          ;; could see it existing and read it EMPTY (pid 0) under load.
          (with-temp-file stub
            (insert "#!/bin/sh\necho $$ > " pidfile ".tmp && mv " pidfile ".tmp " pidfile
                    "\nsleep 30\n"))
          (set-file-modes stub #o755)
          ;; Act
          (let ((proc (make-process :name "agent-repl-test-detach"
                                    :command (agent-repl-daemon--spawn-argv)
                                    :noquery t)))
            (unwind-protect
                (progn
                  (while (not (file-exists-p pidfile))
                    (accept-process-output nil 0.002))
                  ;; The pid the stub reported must be the process Emacs
                  ;; holds: that is the `exec', not a shell in between.
                  (let ((pid (string-to-number
                              (with-temp-buffer
                                (insert-file-contents pidfile)
                                (string-trim (buffer-string))))))
                    (should (equal pid (process-id proc)))
                    ;; Act: the signal Emacs sends every child on exit.
                    (signal-process pid 'SIGHUP)
                    ;; Assert: still there.
                    (accept-process-output nil 0.05)
                    (should (process-attributes pid))))
              (when (process-live-p proc)
                (signal-process (process-id proc) 'SIGKILL)))))
      (delete-directory dir t))))

(ert-deftest agent-repl-test-daemon-the-deliberate-stop-still-stops-it ()
  "Detaching costs the stop verb nothing: it was never a signal.
`agent-repl-frontend-daemon-stop' asks the daemon to shut itself down
over the link, so the one thing that ends a resident daemon on purpose
works exactly as it did."
  (agent-repl-test-daemon--with-harness
    ;; Arrange / Act
    (agent-repl-frontend-daemon-stop)
    ;; Assert
    (should (equal (plist-get (plist-get (car agent-repl-test-daemon--shutdown-requests)
                                         :action)
                              :arm)
                   :now))))

(ert-deftest agent-repl-test-daemon-the-deliberate-stop-signals-no-process ()
  "The stop NEVER kills the process object, before or after the detach.
A kill would strand the daemon's in-flight writes, which is exactly what
the shutdown schedule exists to avoid."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (let ((signalled nil))
      (cl-letf (((symbol-function 'signal-process)
                 (lambda (&rest args) (push args signalled) nil))
                ((symbol-function 'delete-process)
                 (lambda (&rest args) (push args signalled) nil)))
        ;; Act
        (agent-repl-frontend-daemon-stop))
      ;; Assert
      (should (null signalled)))))

(provide 'test-daemon)

;;; test-daemon.el ends here
