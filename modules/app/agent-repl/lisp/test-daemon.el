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
         (agent-repl-daemon--boot-timer nil)
         (agent-repl-daemon--boot-deadline nil)
         (agent-repl-daemon--boot-continuation nil)
         (agent-repl-daemon--ensure-in-flight nil))
     (cl-letf (((symbol-function 'agent-repl-connect-read-daemon-addr)
                (lambda () agent-repl-test-daemon--address))
               ((symbol-function 'agent-repl--frontend-run-build-script)
                (lambda (args)
                  (push args agent-repl-test-daemon--build-runs)
                  agent-repl-test-daemon--build-exit))
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
  "The default argv is the module's own `daemon/bin/claude-repld', alone.
fanout \u00a711: \"the default the module's `daemon/bin/claude-repld', no argv\"
\u2014 a wrong default path would point cold start at nothing, and a default
argument would hand the daemon input it is not supposed to need."
  ;; Arrange / Act: the default, independent of any buffer-local override.
  (let ((default (default-value 'agent-repl-daemon-command)))
    ;; Assert
    (should (equal default
                   (list (expand-file-name "daemon/bin/claude-repld"
                                           agent-repl--frontend-root))))))

(ert-deftest agent-repl-test-daemon-absent-address-starts-the-daemon ()
  "After a clean build the daemon is started, with NO argv of its own."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil)
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (equal (car (car agent-repl-test-daemon--spawns))
                   agent-repl-daemon-command))))

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
  "A build that works takes the failure segment down."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address nil
          agent-repl-daemon-build-failure "an earlier failure"
          agent-repl-daemon-mode-line-segment "daemon: build failed")
    ;; Act
    (agent-repl-daemon-ensure)
    ;; Assert
    (should (null agent-repl-daemon-mode-line-segment))))

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
    (let ((accepted :unset))
      ;; Act
      (agent-repl-frontend-daemon-stop (lambda (ok) (setq accepted ok)))
      ;; Assert
      (should (eq accepted t)))))

(ert-deftest agent-repl-test-daemon-stop-transport-failure-reports-nil ()
  "A stop that could not be delivered did not happen."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (let ((accepted :unset))
      (setq agent-repl-test-daemon--shutdown-answer
            (list :failure (list :kind :transport :message "no route")))
      ;; Act
      (agent-repl-frontend-daemon-stop (lambda (ok) (setq accepted ok)))
      ;; Assert
      (should (null accepted)))))

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

(ert-deftest agent-repl-test-daemon-departure-timeout-still-ensures ()
  "A timed-out wait ensures anyway: the link is down and no daemon is worse."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--address "127.0.0.1:9999")
    (agent-repl-frontend-daemon-restart)
    ;; Act
    (setq agent-repl-daemon--departure-deadline (- (float-time) 1))
    (agent-repl-daemon--departure-tick)
    ;; Assert
    (should (> agent-repl-test-daemon--link-connect-calls 0))))

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

(ert-deftest agent-repl-test-daemon-build-passes-its-targets-to-the-script ()
  "A targeted build names its targets; the script still owns staleness."
  (agent-repl-test-daemon--with-harness
    ;; Arrange / Act
    (agent-repl-daemon--build '("store" "sidecar"))
    ;; Assert
    (should (equal (car agent-repl-test-daemon--build-runs)
                   (list agent-repl-daemon-build-script "store" "sidecar")))))

(ert-deftest agent-repl-test-daemon-build-answers-nil-on-success ()
  "Nil IS success; a detail string IS the failure."
  (agent-repl-test-daemon--with-harness
    ;; Arrange / Act / Assert
    (should (null (agent-repl-daemon--build)))))

(ert-deftest agent-repl-test-daemon-build-answers-a-detail-on-failure ()
  "The failure detail names the exit code and where the output is."
  (agent-repl-test-daemon--with-harness
    ;; Arrange
    (setq agent-repl-test-daemon--build-exit 3)
    ;; Act
    (let ((detail (agent-repl-daemon--build)))
      ;; Assert
      (should (string-search "exit 3" detail)))))

(provide 'test-daemon)

;;; test-daemon.el ends here
