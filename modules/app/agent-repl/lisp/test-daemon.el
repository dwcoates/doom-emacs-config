;;; test-daemon.el --- ERT tests for daemon.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the lazy, build-if-stale frontend daemon launcher.
;;
;; The three external-boundary wrappers
;; (`agent-repl--frontend-run-build-script' and
;; `agent-repl--frontend-artifact-exists-p' and
;; `agent-repl--frontend-spawn-daemon') are shadowed via `cl-letf' in
;; every test that reaches them, so no real subprocess is ever spawned.
;; A `agent-repl-test--fake-daemon' struct stands in for the process
;; object, and the process primitives the module calls (`process-live-p',
;; `process-id', `delete-process') are shadowed to route through it.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-daemon.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; ---- Shared widget-dist helpers -------------------------------------------

(defun agent-repl-test--make-widget-dist (dir)
  "Create DIR as a cee-web-widget dist holding a stub `chess-widget.js'."
  (make-directory dir t)
  (with-temp-file (expand-file-name "chess-widget.js" dir)
    (insert "export default 0")))

(defmacro agent-repl-test--with-temp-root (var &rest body)
  "Bind VAR to a fresh temp directory for BODY, deleting it afterward."
  (declare (indent 1))
  `(let ((,var (make-temp-file "agent-repl-widget-" t)))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

;;;; ---- Fake daemon process --------------------------------------------------

(cl-defstruct agent-repl-test--fake-daemon
  live pid
  ;; term-behavior: `exit' (the default) makes the fake die on SIGTERM,
  ;; modeling a daemon whose graceful-shutdown path works; `ignore'
  ;; models a hung daemon that outlives the grace window.
  (term-behavior 'exit)
  ;; signals: every signal delivered via `signal-process', newest first.
  signals
  ;; deleted: non-nil once `delete-process' was invoked on the fake.
  deleted)

(defun agent-repl-test--make-live-daemon (&optional pid)
  "Return a fake daemon process that reports itself live, PID default 4242."
  (make-agent-repl-test--fake-daemon :live t :pid (or pid 4242)))

(defmacro agent-repl-test--with-daemon-env (&rest body)
  "Run BODY with daemon process primitives and the init guard shadowed.
`agent-repl--frontend-init-inhibited-p' returns nil so the real ensure
path runs under batch; `process-live-p'/`process-id'/`signal-process'/
`delete-process' route through the fake-daemon struct.  SIGTERM kills
the fake when its `term-behavior' is `exit' (the default).

`agent-repl--frontend-turn-active-sessions' is stubbed IDLE, which makes
this env hermetic: the real probe HTTP-GETs whatever `claude-repld' is
running on the developer's machine, so an unstubbed daemon test silently
keys on the developer's own live sessions and fails whenever one of them
happens to be mid-turn.  Tests wanting a busy probe re-stub it with their
own `cl-letf', which shadows this one for the extent of that form.

`agent-repl--frontend-run-listener-probe' is stubbed to report NO
listener for the same hermeticity reason: the real probe runs `lsof'
against whatever holds port 8787 on the developer's machine.

`agent-repl--frontend-run-daemon-pgrep' is stubbed to report NO daemon
process for the same reason again: the real one runs `pgrep' and would
otherwise adopt whatever `claude-repld' the developer has running.  The
spawn-tracking state (`agent-repl--frontend-daemon-spawn-time' and the
parked-ensure queue) is bound fresh so one test's spawn cannot suppress
the next test's."
  `(let ((agent-repl-frontend-auto-start t)
         (agent-repl--frontend-daemon-process nil)
         (agent-repl--frontend-daemon-spawn-time nil)
         (agent-repl--frontend-spawn-waiters nil)
         (agent-repl--frontend-adopted-daemon-pid nil))
     (cl-letf (((symbol-function 'agent-repl--frontend-init-inhibited-p)
                (lambda () nil))
               ((symbol-function 'agent-repl--frontend-artifact-exists-p)
                (lambda (_path) t))
               ((symbol-function 'agent-repl--frontend-turn-active-sessions)
                (lambda () nil))
               ((symbol-function 'agent-repl--frontend-run-listener-probe)
                (lambda (&rest _) nil))
               ((symbol-function 'agent-repl--frontend-run-daemon-pgrep)
                (lambda (&rest _) ""))
               ;; The exit path reads the daemon's terminal-output sink off
               ;; disk; an empty capture is the neutral fixture for tests
               ;; that are not about the capture at all.
               ((symbol-function 'agent-repl--frontend-read-daemon-output-sink)
                (lambda (&rest _) ""))
               ((symbol-function 'process-live-p)
                (lambda (p) (and (agent-repl-test--fake-daemon-p p)
                                 (agent-repl-test--fake-daemon-live p))))
               ((symbol-function 'process-id)
                (lambda (p) (and (agent-repl-test--fake-daemon-p p)
                                 (agent-repl-test--fake-daemon-pid p))))
               ((symbol-function 'signal-process)
                (lambda (p sig)
                  (when (agent-repl-test--fake-daemon-p p)
                    (push sig (agent-repl-test--fake-daemon-signals p))
                    (when (and (eq sig 'TERM)
                               (eq (agent-repl-test--fake-daemon-term-behavior p)
                                   'exit))
                      (setf (agent-repl-test--fake-daemon-live p) nil)))))
               ((symbol-function 'delete-process)
                (lambda (p) (when (agent-repl-test--fake-daemon-p p)
                              (setf (agent-repl-test--fake-daemon-deleted p) t)
                              (setf (agent-repl-test--fake-daemon-live p) nil)))))
       ,@body)))

;;;; ---- build-if-stale: argument shaping ------------------------------------

(ert-deftest agent-repl-test-daemon-build-omits-force-by-default ()
  "Without FORCE, the build script argv carries only the script path."
  ;; Arrange
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--frontend-run-build-script)
               (lambda (args) (setq captured args) 0)))
      ;; Act
      (agent-repl--frontend-build-if-stale nil)
      ;; Assert
      (should (equal captured (list agent-repl--frontend-build-script))))))

(ert-deftest agent-repl-test-daemon-build-passes-force-flag ()
  "With FORCE, the build script argv appends --force."
  ;; Arrange
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--frontend-run-build-script)
               (lambda (args) (setq captured args) 0)))
      ;; Act
      (agent-repl--frontend-build-if-stale t)
      ;; Assert
      (should (equal captured
                     (list agent-repl--frontend-build-script "--force"))))))

(ert-deftest agent-repl-test-daemon-build-targets-selects-store-and-sidecar ()
  "Targeted stale builds pass both launchd service names to the shared script."
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--frontend-run-build-script)
               (lambda (args) (setq captured args) 0)))
      (agent-repl--frontend-build-targets-if-stale '("store" "sidecar"))
      (should (equal captured
                     (list agent-repl--frontend-build-script
                           "store" "sidecar"))))))

;;;; ---- build argv/capture helpers ------------------------------------------
;;
;; The blocking and asynchronous build runs share one argv shape, one capture
;; buffer and one failure report.  These cover the shared helpers directly, so
;; a hand-rolled second spelling of any of them fails here rather than drifting
;; silently against the script.

(ert-deftest agent-repl-test-daemon-build-args-shape-script-force-targets ()
  "The shared argv builder emits script, then --force, then the targets."
  ;; Act
  (let ((args (agent-repl--frontend-build-args '("webapp") t)))
    ;; Assert
    (should (equal args (list agent-repl--frontend-build-script
                              "--force" "webapp")))))

(ert-deftest agent-repl-test-daemon-build-args-back-the-blocking-run ()
  "The blocking run invokes the script with exactly the shared argv.
Asserts the call site SHARES the extracted shape rather than spelling a
near-identical argv of its own."
  ;; Arrange
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--frontend-run-build-script)
               (lambda (args) (setq captured args) 0)))
      ;; Act
      (agent-repl--frontend-build-targets-if-stale '("webapp") t)
      ;; Assert
      (should (equal captured (agent-repl--frontend-build-args '("webapp") t))))))

(ert-deftest agent-repl-test-daemon-build-capture-resets-between-runs ()
  "Resetting the capture leaves a run's buffer holding only its own output."
  ;; Arrange
  (with-current-buffer (get-buffer-create agent-repl--frontend-build-buffer)
    (erase-buffer)
    (insert "output of the previous run\n"))
  ;; Act
  (agent-repl--frontend-build-reset-capture)
  ;; Assert
  (should (equal (agent-repl--frontend-build-captured-output) "")))

(ert-deftest agent-repl-test-daemon-build-capture-trims-trailing-newline ()
  "The captured output is returned with trailing whitespace trimmed."
  ;; Arrange
  (agent-repl--frontend-build-reset-capture)
  (with-current-buffer agent-repl--frontend-build-buffer
    (insert "built ok\n\n"))
  ;; Act / Assert
  (should (equal (agent-repl--frontend-build-captured-output) "built ok")))

(ert-deftest agent-repl-test-daemon-build-failure-report-names-the-exit-code ()
  "The shared failure report hands back a detail naming the exit code."
  ;; Arrange
  (cl-letf (((symbol-function 'display-buffer) #'ignore))
    ;; Act
    (let ((detail (agent-repl--frontend-build-report-failure "webapp" 3 "boom")))
      ;; Assert
      (should (string-match-p "exit 3" detail)))))

(ert-deftest agent-repl-test-daemon-run-report-failure-names-both-subjects ()
  "The shared run reporter phrases the phase line and the detail separately.
The build run and the stack deploy name themselves differently in each
place, so both subjects are the reporter's arguments."
  ;; Arrange
  (let (phase)
    (cl-letf (((symbol-function 'display-buffer) #'ignore)
              ((symbol-function 'agent-repl--backend-phase)
               (lambda (_ws fmt &rest args) (setq phase (apply #'format fmt args)))))
      ;; Act
      (let ((detail (agent-repl--frontend-run-report-failure
                     "stack deploy" "stack deploy" 1 "boom")))
        ;; Assert
        (should (string-prefix-p "stack deploy FAILED (exit 1)" phase))
        (should (string-prefix-p "agent-repl: stack deploy failed (exit 1)" detail))))))

(ert-deftest agent-repl-test-daemon-build-missing-script-check-is-shared ()
  "The script-presence assertion signals for an absent script.
Both build runs gate on this one check, so neither can start a process
against a script that is not there."
  ;; Arrange — a genuinely absent path (no `file-exists-p' shadow, which
  ;; would trip a native-comp trampoline warning; see test-sentinel.el).
  (let ((agent-repl--frontend-build-script "/agent-repl-nonexistent/build.sh"))
    ;; Act / Assert
    (should-error (agent-repl--frontend-build-assert-script))))

;;;; ---- build-if-stale: the asynchronous run --------------------------------
;;
;; The spawn wrapper is shadowed to hand back a fake process, so no build ever
;; runs; the sentinel is driven by hand with the exit code under test.

(cl-defstruct agent-repl-test--fake-run-proc
  (live t) (exit 0))

(defvar agent-repl-test--async-run-spawned nil
  "Argv of every faked async build spawn, oldest first.")

(defvar agent-repl-test--async-run-proc nil
  "The fake process the most recent faked async build spawn returned.")

(defmacro agent-repl-test--with-async-run (&rest body)
  "Run BODY with the async build boundary faked and its state reset.
`agent-repl-test--async-run-spawned' collects the argv of every spawn,
newest last, and `agent-repl-test--async-run-proc' holds the fake the
most recent spawn returned."
  `(let ((agent-repl--frontend-async-run-process nil)
         (agent-repl--frontend-async-run-request nil)
         (agent-repl--frontend-async-run-queue nil)
         (agent-repl-test--async-run-spawned nil)
         (agent-repl-test--async-run-proc nil))
     (cl-letf (((symbol-function 'agent-repl--frontend-spawn-run-script)
                (lambda (args)
                  (setq agent-repl-test--async-run-spawned
                        (append agent-repl-test--async-run-spawned (list args)))
                  (setq agent-repl-test--async-run-proc
                        (make-agent-repl-test--fake-run-proc))))
               ((symbol-function 'process-live-p)
                (lambda (p) (if (agent-repl-test--fake-run-proc-p p)
                                (agent-repl-test--fake-run-proc-live p)
                              nil)))
               ((symbol-function 'process-exit-status)
                (lambda (p) (agent-repl-test--fake-run-proc-exit p)))
               ((symbol-function 'display-buffer) #'ignore))
       ,@body)))

(defun agent-repl-test--settle-async-run (exit)
  "Exit the tracked fake build process with EXIT, driving the real sentinel."
  (let ((proc agent-repl--frontend-async-run-process))
    (setf (agent-repl-test--fake-run-proc-live proc) nil)
    (setf (agent-repl-test--fake-run-proc-exit proc) exit)
    (agent-repl--frontend-async-run-sentinel proc "finished\n")))

;;;; ---- deploy-stack: the boot path's whole-stack deploy --------------------
;;
;; The boot path used to run build-frontend.sh, which covers the shim, the
;; webapp and the daemon and MISSES protobuf regeneration and the two
;; launchd services. A wire-format change could therefore leave a new Emacs
;; talking to a daemon built before it.

(ert-deftest agent-repl-test-daemon-deploy-failure-report-is-shared ()
  "The stack deploy's failure goes through the shared run reporter.
A hand-rolled second failure path at the deploy site fails here rather
than drifting from the build's."
  ;; Arrange
  (agent-repl-test--with-async-run
   (let (reported)
     (cl-letf (((symbol-function 'agent-repl--warn) (lambda (&rest _) nil))
               ((symbol-function 'agent-repl--frontend-run-report-failure)
                (lambda (phase-subject &rest _) (setq reported phase-subject) "detail")))
       ;; Act
       (agent-repl--frontend-deploy-stack-async nil)
       (agent-repl-test--settle-async-run 1)
       ;; Assert
       (should (equal reported "stack deploy"))))))

(ert-deftest agent-repl-test-daemon-deploy-stack-runs-the-deploy-script ()
  "deploy-stack runs bin/deploy-all.sh, not the narrower build script."
  ;; Arrange
  (agent-repl-test--with-async-run
   ;; Act
   (agent-repl--frontend-deploy-stack-async nil)
   ;; Assert
   (should (equal (car (car agent-repl-test--async-run-spawned))
                  agent-repl--frontend-deploy-script))))

(ert-deftest agent-repl-test-daemon-deploy-stack-suppresses-the-daemon-bounce ()
  "deploy-stack always passes --no-daemon-bounce.
The script's last step restarts the daemon by evaluating a form in Emacs
over emacsclient.  A call made FROM Emacs would re-enter the session that
is mid-boot, and the caller starts the daemon directly anyway."
  ;; Arrange
  (agent-repl-test--with-async-run
   ;; Act
   (agent-repl--frontend-deploy-stack-async nil)
   ;; Assert
   (should (member "--no-daemon-bounce" (car agent-repl-test--async-run-spawned)))))

(ert-deftest agent-repl-test-daemon-deploy-stack-omits-force-by-default ()
  "Without FORCE the deploy argv carries no --force."
  (agent-repl-test--with-async-run
   (agent-repl--frontend-deploy-stack-async nil)
   (should-not (member "--force" (car agent-repl-test--async-run-spawned)))))

(ert-deftest agent-repl-test-daemon-deploy-stack-passes-force-flag ()
  "With FORCE the deploy argv appends --force."
  (agent-repl-test--with-async-run
   (agent-repl--frontend-deploy-stack-async t)
   (should (member "--force" (car agent-repl-test--async-run-spawned)))))

(ert-deftest agent-repl-test-daemon-deploy-stack-surfaces-a-failed-deploy ()
  "A non-zero deploy exit reports the failure to its continuation.
The launch that would otherwise run against stale code is what the
continuation gates, so the failure has to REACH it rather than signal
into a process sentinel that would swallow it."
  (agent-repl-test--with-async-run
   (let (detail)
     (cl-letf (((symbol-function 'agent-repl--warn) (lambda (&rest _) nil)))
       (agent-repl--frontend-deploy-stack-async nil nil (lambda (d) (setq detail d)))
       (agent-repl-test--settle-async-run 1))
     (should (string-match-p "exit 1" (or detail ""))))))

(ert-deftest agent-repl-test-daemon-deploy-stack-never-blocks-the-main-thread ()
  "The whole-stack deploy spawns; it never runs the blocking script wrapper.
A `call-process' here held the main thread for the whole build, which is
what starved the 1Hz heartbeat and the workspace update chain and burned
the daemon's command deadlines against an Emacs that could not answer."
  (agent-repl-test--with-async-run
   (let (blocked)
     (cl-letf (((symbol-function 'agent-repl--frontend-run-build-script)
                (lambda (_args) (setq blocked t) 0)))
       ;; Act
       (agent-repl--frontend-deploy-stack-async nil)
       ;; Assert
       (should-not blocked)
       (should (equal (length agent-repl-test--async-run-spawned) 1))))))

(ert-deftest agent-repl-test-daemon-deploy-stack-errors-on-missing-script ()
  "A missing deploy script signals rather than reporting a failed deploy."
  (agent-repl-test--with-async-run
   (let ((agent-repl--frontend-deploy-script "/agent-repl-nonexistent/deploy.sh"))
     (should-error (agent-repl--frontend-deploy-stack-async nil))
     (should-not agent-repl-test--async-run-spawned))))

(ert-deftest agent-repl-test-daemon-deploy-shares-the-build-queue ()
  "A deploy and a build never run as two overlapping processes.
Both write the same artifacts and share one capture buffer, so the second
request queues behind the first instead of racing it."
  (agent-repl-test--with-async-run
   ;; Arrange
   (agent-repl--frontend-deploy-stack-async nil)
   ;; Act
   (let ((outcome (agent-repl--frontend-build-targets-async '("webapp"))))
     ;; Assert
     (should (eq outcome 'queued))
     (should (equal (length agent-repl-test--async-run-spawned) 1)))))

;;;; ---- build-if-stale: failure surfacing -----------------------------------

(ert-deftest agent-repl-test-daemon-build-errors-on-missing-script ()
  "A missing build script signals an error rather than silently passing."
  ;; Arrange — point at a genuinely absent path (no `file-exists-p' shadow,
  ;; which would trip a native-comp trampoline warning; see test-sentinel.el).
  (let ((agent-repl--frontend-build-script "/agent-repl-nonexistent/build.sh"))
    ;; Act / Assert
    (should-error (agent-repl--frontend-build-if-stale nil))))

(ert-deftest agent-repl-test-daemon-build-errors-on-nonzero-exit ()
  "A non-zero build exit signals an error, never swallowed."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-run-build-script)
             (lambda (_args) 1))
            ((symbol-function 'display-buffer) #'ignore))
    ;; Act / Assert
    (should-error (agent-repl--frontend-build-if-stale nil))))

(ert-deftest agent-repl-test-daemon-build-returns-zero-on-success ()
  "A zero build exit returns 0."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-run-build-script)
             (lambda (_args) 0)))
    ;; Act / Assert
    (should (eq 0 (agent-repl--frontend-build-if-stale nil)))))

(ert-deftest agent-repl-test-daemon-async-build-spawns-the-shared-argv ()
  "The asynchronous run spawns the script with the shared argv shape."
  (agent-repl-test--with-async-run
   ;; Act
   (let ((outcome (agent-repl--frontend-build-targets-async '("webapp"))))
     ;; Assert
     (should (eq outcome 'started))
     (should (equal agent-repl-test--async-run-spawned
                    (list (agent-repl--frontend-build-args '("webapp") nil)))))))

(ert-deftest agent-repl-test-daemon-async-build-runs-success-callback ()
  "A zero exit runs the success continuation."
  (agent-repl-test--with-async-run
   ;; Arrange
   (let (won)
     (agent-repl--frontend-build-targets-async
      '("webapp") nil (lambda () (setq won t)) nil)
     ;; Act
     (agent-repl-test--settle-async-run 0)
     ;; Assert
     (should won))))

(ert-deftest agent-repl-test-daemon-async-build-runs-failure-callback ()
  "A non-zero exit runs the failure continuation with the failure detail."
  (agent-repl-test--with-async-run
   ;; Arrange
   (let (detail)
     (cl-letf (((symbol-function 'agent-repl--warn) (lambda (&rest _) nil)))
       (agent-repl--frontend-build-targets-async
        '("webapp") nil nil (lambda (d) (setq detail d)))
       ;; Act
       (agent-repl-test--settle-async-run 2))
     ;; Assert
     (should (string-match-p "exit 2" (or detail ""))))))

(ert-deftest agent-repl-test-daemon-async-build-failure-skips-success-callback ()
  "A non-zero exit never runs the success continuation."
  (agent-repl-test--with-async-run
   ;; Arrange
   (let (won)
     (cl-letf (((symbol-function 'agent-repl--warn) (lambda (&rest _) nil)))
       (agent-repl--frontend-build-targets-async
        '("webapp") nil (lambda () (setq won t)) nil)
       ;; Act
       (agent-repl-test--settle-async-run 1))
     ;; Assert
     (should-not won))))

(ert-deftest agent-repl-test-daemon-async-build-does-not-stack-a-second-process ()
  "A request arriving mid-build queues instead of spawning a second build."
  (agent-repl-test--with-async-run
   ;; Arrange
   (agent-repl--frontend-build-targets-async '("webapp"))
   ;; Act
   (let ((outcome (agent-repl--frontend-build-targets-async '("shim"))))
     ;; Assert — still exactly one spawn.
     (should (eq outcome 'queued))
     (should (equal (length agent-repl-test--async-run-spawned) 1)))))

(ert-deftest agent-repl-test-daemon-async-build-coalesces-identical-requests ()
  "Two identical requests behind one build share a single queued run."
  (agent-repl-test--with-async-run
   ;; Arrange
   (agent-repl--frontend-build-targets-async '("webapp"))
   (agent-repl--frontend-build-targets-async '("webapp"))
   ;; Act
   (let ((outcome (agent-repl--frontend-build-targets-async '("webapp"))))
     ;; Assert
     (should (eq outcome 'coalesced))
     (should (equal (length agent-repl--frontend-async-run-queue) 1)))))

(ert-deftest agent-repl-test-daemon-async-build-runs-the-queued-run-after ()
  "The queued build starts once the in-flight one settles."
  (agent-repl-test--with-async-run
   ;; Arrange
   (agent-repl--frontend-build-targets-async '("webapp"))
   (agent-repl--frontend-build-targets-async '("shim"))
   ;; Act
   (agent-repl-test--settle-async-run 0)
   ;; Assert
   (should (equal agent-repl-test--async-run-spawned
                  (list (agent-repl--frontend-build-args '("webapp") nil)
                        (agent-repl--frontend-build-args '("shim") nil))))))

(ert-deftest agent-repl-test-daemon-async-build-coalesced-waiters-all-run ()
  "Every waiter coalesced onto a queued run gets its continuation."
  (agent-repl-test--with-async-run
   ;; Arrange
   (let ((won 0))
     (agent-repl--frontend-build-targets-async '("webapp"))
     (agent-repl--frontend-build-targets-async
      '("shim") nil (lambda () (setq won (1+ won))) nil)
     (agent-repl--frontend-build-targets-async
      '("shim") nil (lambda () (setq won (1+ won))) nil)
     ;; Act — settle the in-flight run, then the queued one it starts.
     (agent-repl-test--settle-async-run 0)
     (agent-repl-test--settle-async-run 0)
     ;; Assert
     (should (equal won 2)))))

(ert-deftest agent-repl-test-daemon-async-build-drains-past-a-throwing-waiter ()
  "A continuation that throws does not strand the queued build behind it."
  (agent-repl-test--with-async-run
   ;; Arrange
   (agent-repl--frontend-build-targets-async
    '("webapp") nil (lambda () (error "waiter blew up")) nil)
   (agent-repl--frontend-build-targets-async '("shim"))
   ;; Act
   (should-error (agent-repl-test--settle-async-run 0))
   ;; Assert — the queued run was started anyway.
   (should (equal (length agent-repl-test--async-run-spawned) 2))))

(ert-deftest agent-repl-test-daemon-async-build-errors-on-missing-script ()
  "A missing build script signals rather than reporting a failed build."
  (agent-repl-test--with-async-run
   ;; Arrange
   (let ((agent-repl--frontend-build-script "/agent-repl-nonexistent/build.sh"))
     ;; Act / Assert
     (should-error (agent-repl--frontend-build-targets-async '("webapp")))
     (should-not agent-repl-test--async-run-spawned))))

(ert-deftest agent-repl-test-daemon-async-build-settle-without-a-request-signals ()
  "Settling with nothing in flight is a broken invariant, not a no-op."
  (agent-repl-test--with-async-run
   ;; Act / Assert
   (should-error (agent-repl--frontend-async-run-settle 0))))

;;;; ---- timer-backed lifecycle waiting --------------------------------------

(ert-deftest agent-repl-test-daemon-await-calls-ready-without-scheduling ()
  "A clear lifecycle condition completes synchronously without a timer."
  (let (ready scheduled)
    (cl-letf (((symbol-function 'agent-repl--uds-run-timer)
               (lambda (&rest _) (setq scheduled t))))
      (agent-repl--frontend-await-async (lambda () nil) 10 0.01
                                         (lambda () (setq ready t)) #'ignore "test")
      (should ready)
      (should-not scheduled))))

(ert-deftest agent-repl-test-daemon-await-timeout-calls-timeout ()
  "A lifecycle deadline reports the remaining value without blocking."
  (let (timed-out)
    (agent-repl--frontend-await-async (lambda () 'still-live) 0 0.01
                                       #'ignore (lambda (value) (setq timed-out value)) "test")
    (should (eq timed-out 'still-live))))

(ert-deftest agent-repl-test-daemon-await-pending-schedules-one-timer ()
  "A pending lifecycle condition returns immediately after scheduling one retry."
  (let (timer)
    (cl-letf (((symbol-function 'agent-repl--uds-run-timer)
               (lambda (&rest args) (setq timer args) :timer)))
      (should (eq :timer (agent-repl--frontend-await-async (lambda () t) 10 0.25 #'ignore #'ignore "test")))
      (should (equal (car timer) 0.25)))))

;;;; ---- staleness: on-disk binary mtime -------------------------------------

(ert-deftest agent-repl-test-daemon-disk-mtime-reads-integer-seconds ()
  "The on-disk mtime reader returns the binary's mtime as integer seconds."
  ;; Arrange — a temp file stamped to a known Unix second.
  (let ((tmp (make-temp-file "agent-repl-daemon-bin")))
    (unwind-protect
        (progn
          (set-file-times tmp 1700000000)
          (let ((agent-repl--frontend-daemon-bin tmp))
            ;; Act / Assert
            (should (equal 1700000000
                           (agent-repl--frontend-daemon-binary-disk-mtime)))))
      (delete-file tmp))))

(ert-deftest agent-repl-test-daemon-disk-mtime-nil-when-binary-absent ()
  "The on-disk mtime reader is nil when the binary does not exist."
  ;; Arrange — an absent path (no `file-exists-p' shadow).
  (let ((agent-repl--frontend-daemon-bin "/agent-repl-nonexistent/claude-repld"))
    ;; Act / Assert
    (should (null (agent-repl--frontend-daemon-binary-disk-mtime)))))

;;;; ---- staleness: running daemon reported mtime ----------------------------

(defmacro agent-repl-test--with-daemon-view (connected view &rest body)
  "Run BODY with the UDS link CONNECTED (a boolean) and VIEW stored.
VIEW is the `DaemonView' plist the daemon last pushed (nil = none yet)."
  (declare (indent 2))
  `(let ((agent-repl--frontend-last-daemon-view ,view))
     (cl-letf (((symbol-function 'agent-repl--uds-connected-p)
                (lambda () ,connected)))
       ,@body)))

(ert-deftest agent-repl-test-daemon-running-mtime-reads-pushed-daemon-view ()
  "The running-mtime reader converts the pushed `daemonBinaryMtimeMs' to seconds."
  ;; Arrange — protojson encodes the int64 field as a STRING.
  (agent-repl-test--with-daemon-view t '(:daemonBinaryMtimeMs "1700000000000")
    ;; Act / Assert
    (should (equal 1700000000
                   (agent-repl--frontend-running-daemon-binary-mtime)))))

(ert-deftest agent-repl-test-daemon-running-mtime-accepts-a-numeric-mtime ()
  "A numerically-decoded mtime is converted to seconds just the same."
  ;; Arrange
  (agent-repl-test--with-daemon-view t '(:daemonBinaryMtimeMs 1700000000000)
    ;; Act / Assert
    (should (equal 1700000000
                   (agent-repl--frontend-running-daemon-binary-mtime)))))

(ert-deftest agent-repl-test-daemon-running-mtime-nil-when-link-is-down ()
  "A DEAD link yields nil: the stored view may describe a daemon that is gone."
  ;; Arrange
  (agent-repl-test--with-daemon-view nil '(:daemonBinaryMtimeMs "1700000000000")
    ;; Act / Assert
    (should (null (agent-repl--frontend-running-daemon-binary-mtime)))))

(ert-deftest agent-repl-test-daemon-running-mtime-nil-when-no-view-pushed ()
  "No `DaemonView' yet yields nil, never a guess."
  ;; Arrange
  (agent-repl-test--with-daemon-view t nil
    ;; Act / Assert
    (should (null (agent-repl--frontend-running-daemon-binary-mtime)))))

(ert-deftest agent-repl-test-daemon-running-mtime-nil-when-field-absent ()
  "A daemon predating the field (no `daemonBinaryMtimeMs') yields nil."
  ;; Arrange
  (agent-repl-test--with-daemon-view t '(:bootId "b_abc")
    ;; Act / Assert
    (should (null (agent-repl--frontend-running-daemon-binary-mtime)))))

(ert-deftest agent-repl-test-daemon-running-mtime-nil-when-nonpositive ()
  "A zero mtime (the daemon's boot-time self-stat failed) yields nil."
  ;; Arrange
  (agent-repl-test--with-daemon-view t '(:daemonBinaryMtimeMs "0")
    ;; Act / Assert
    (should (null (agent-repl--frontend-running-daemon-binary-mtime)))))

(ert-deftest agent-repl-test-daemon-running-mtime-nil-when-unparsable ()
  "A non-numeric mtime string yields nil rather than a coerced zero."
  ;; Arrange
  (agent-repl-test--with-daemon-view t '(:daemonBinaryMtimeMs "not-a-number")
    ;; Act / Assert
    (should (null (agent-repl--frontend-running-daemon-binary-mtime)))))

;;;; ---- staleness: the comparison predicate ---------------------------------

(ert-deftest agent-repl-test-daemon-stale-p-true-when-disk-newer ()
  "STALE when the on-disk binary's mtime exceeds the running daemon's."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-daemon-binary-disk-mtime)
             (lambda () 200))
            ((symbol-function 'agent-repl--frontend-running-daemon-binary-mtime)
             (lambda () 100)))
    ;; Act / Assert
    (should (agent-repl--frontend-daemon-stale-p))))

(ert-deftest agent-repl-test-daemon-stale-p-nil-when-equal ()
  "NOT stale when the mtimes are equal — the no-rebuild steady state."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-daemon-binary-disk-mtime)
             (lambda () 100))
            ((symbol-function 'agent-repl--frontend-running-daemon-binary-mtime)
             (lambda () 100)))
    ;; Act / Assert
    (should-not (agent-repl--frontend-daemon-stale-p))))

(ert-deftest agent-repl-test-daemon-stale-p-nil-when-disk-older ()
  "NOT stale when the on-disk binary predates the running daemon's."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-daemon-binary-disk-mtime)
             (lambda () 50))
            ((symbol-function 'agent-repl--frontend-running-daemon-binary-mtime)
             (lambda () 100)))
    ;; Act / Assert
    (should-not (agent-repl--frontend-daemon-stale-p))))

(ert-deftest agent-repl-test-daemon-stale-p-nil-when-running-unknown ()
  "NOT stale when the running daemon reports no mtime — never bounce on a guess."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-daemon-binary-disk-mtime)
             (lambda () 200))
            ((symbol-function 'agent-repl--frontend-running-daemon-binary-mtime)
             (lambda () nil)))
    ;; Act / Assert
    (should-not (agent-repl--frontend-daemon-stale-p))))

;;;; ---- ensure: gating ------------------------------------------------------

(ert-deftest agent-repl-test-daemon-ensure-nil-when-auto-start-disabled ()
  "Ensure returns nil and does nothing when auto-start is off."
  ;; Arrange
  (let ((agent-repl-frontend-auto-start nil)
        (agent-repl--frontend-daemon-process nil))
    ;; Act / Assert
    (should (null (agent-repl--ensure-frontend-daemon)))))

(ert-deftest agent-repl-test-daemon-ensure-nil-when-inhibited ()
  "Ensure returns nil under the batch inhibit guard."
  ;; Arrange
  (let ((agent-repl-frontend-auto-start t)
        (agent-repl--frontend-daemon-process nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-init-inhibited-p)
               (lambda () t)))
      ;; Act / Assert
      (should (null (agent-repl--ensure-frontend-daemon))))))

;;;; ---- ensure: idempotence and launch --------------------------------------

(ert-deftest agent-repl-test-daemon-ensure-idempotent-when-live ()
  "Ensure returns the existing live process without building or spawning."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((existing (agent-repl-test--make-live-daemon))
         (built nil) (spawned nil))
     (setq agent-repl--frontend-daemon-process existing)
     (cl-letf (((symbol-function 'agent-repl--frontend-deploy-stack)
                (lambda (&optional _f) (setq built t) 0))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () (setq spawned t) (agent-repl-test--make-live-daemon))))
       ;; Act
       (let ((result (agent-repl--ensure-frontend-daemon)))
         ;; Assert
         (should (eq result existing))
         (should-not built)
         (should-not spawned))))))

(ert-deftest agent-repl-test-daemon-ensure-builds-then-spawns-when-probe-fails ()
  "Ensure builds-if-stale then spawns when no daemon is running."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((built nil)
         (fresh (agent-repl-test--make-live-daemon 777)))
     (cl-letf (((symbol-function 'agent-repl--frontend-daemon-responsive-async)
                (lambda (_open absent) (funcall absent 'no-listener)))
               ((symbol-function 'agent-repl--frontend-deploy-stack-async)
                (lambda (&optional _f on-success _on-failure)
                  (setq built t) (when on-success (funcall on-success)) 'started))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () fresh)))
       ;; Act
       (let ((result (agent-repl--ensure-frontend-daemon)))
         ;; Assert
         (should built)
         (should (eq result :pending))
         (should (eq agent-repl--frontend-daemon-process fresh)))))))

(ert-deftest agent-repl-test-daemon-ensure-force-restarts-live ()
  "Ensure with FORCE stops the live daemon, rebuilds, and respawns."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let* ((old (agent-repl-test--make-live-daemon 1))
          (new (agent-repl-test--make-live-daemon 2)))
     (setq agent-repl--frontend-daemon-process old)
     (cl-letf (((symbol-function 'agent-repl--frontend-turn-active-sessions)
                (lambda () nil))
               ((symbol-function 'agent-repl--frontend-deploy-stack-async)
                (lambda (&optional _f on-success _on-failure)
                  (when on-success (funcall on-success)) 'started))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () new)))
       ;; Act
       (let ((result (agent-repl--ensure-frontend-daemon t)))
         ;; Assert
         (should (eq result :pending))
         (should-not (agent-repl-test--fake-daemon-live old)))))))

;;;; ---- startup staleness bounce (one-shot) ---------------------------------

;;;; ---- Daemon liveness probe (UDS) -----------------------------------------

(ert-deftest agent-repl-test-daemon-responsive-async-delegates-to-uds-probe ()
  "The asynchronous liveness probe forwards open completion without a sync socket check."
  (let (path open)
    (cl-letf (((symbol-function 'agent-repl-uds-probe-async)
               (lambda (socket on-open _failure) (setq path socket) (funcall on-open))))
      (agent-repl--frontend-daemon-responsive-async (lambda () (setq open t)) #'ignore)
      (should open)
      (should (equal path agent-repl-uds-socket-path)))))

(ert-deftest agent-repl-test-daemon-responsive-async-forwards-probe-error ()
  "An absent listener reaches the failure continuation with probe detail."
  (let (detail)
    (cl-letf (((symbol-function 'agent-repl-uds-probe-async)
               (lambda (_socket _open failure) (funcall failure 'refused))))
      (agent-repl--frontend-daemon-responsive-async #'ignore (lambda (value) (setq detail value)))
      (should (eq detail 'refused)))))

;;;; ---- Foreign-daemon shutdown (UDS `ShutdownCmd') -------------------------

(ert-deftest agent-repl-test-daemon-foreign-shutdown-sends-the-shutdown-command ()
  "The foreign-daemon shutdown is a `shutdown' FrontendCommand, not an HTTP POST."
  ;; Arrange
  (let (sent)
    (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () t))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (field payload &rest _) (setq sent (list field payload)) "req-1")))
      ;; Act
      (agent-repl--frontend-request-foreign-shutdown)
      ;; Assert — empty ShutdownCmd message (nil payload encodes as `{}').
      (should (equal sent '("shutdown" nil))))))


(ert-deftest agent-repl-test-daemon-foreign-shutdown-dials-when-disconnected ()
  "A down link is dialed first — the foreign daemon owns the same socket."
  ;; Arrange
  (let ((dials 0))
    (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () nil))
              ((symbol-function 'agent-repl-uds-connect)
               (lambda (&optional _p) (cl-incf dials) nil))
              ((symbol-function 'agent-repl--uds-send-command) (lambda (&rest _) "req-1")))
      ;; Act
      (agent-repl--frontend-request-foreign-shutdown)
      ;; Assert
      (should (= dials 1)))))

(ert-deftest agent-repl-test-daemon-foreign-shutdown-propagates-a-send-failure ()
  "No connection to send on signals — never a silent no-op the caller reads as sent."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () nil))
            ((symbol-function 'agent-repl-uds-connect) (lambda (&optional _p) nil))
            ((symbol-function 'agent-repl--uds-send-command)
             (lambda (&rest _) (user-error "agent-repl UDS: not connected"))))
    ;; Act / Assert
    (should-error (agent-repl--frontend-request-foreign-shutdown))))

(ert-deftest agent-repl-test-daemon-foreign-bounce-starts-only-after-socket-release ()
  "Foreign replacement waits for the socket-absence completion callback."
  (let (events completion)
    (cl-letf (((symbol-function 'agent-repl--frontend-request-foreign-shutdown)
               (lambda (&rest _) (setq events (append events '(shutdown)))))
              ((symbol-function 'agent-repl--frontend-await-socket-absence-async)
               (lambda (_timeout absent _timeout-callback _context)
                 (setq events (append events '(await))) (setq completion absent) :timer))
              ((symbol-function 'agent-repl--frontend-start-daemon)
               (lambda () (setq events (append events '(start))) 'started)))
      (should (eq :pending (agent-repl--frontend-bounce-foreign-daemon nil (lambda (value) (setq events (append events (list value)))))))
      (should (equal events '(shutdown await)))
      (funcall completion)
      (should (equal events '(shutdown await start started))))))

(ert-deftest agent-repl-test-daemon-foreign-bounce-timeout-never-starts-replacement ()
  "Foreign socket timeout reports failure without starting beside a live listener."
  (let (timeout-called started)
    (cl-letf (((symbol-function 'agent-repl--frontend-request-foreign-shutdown) (lambda (&rest _) t))
              ((symbol-function 'agent-repl--frontend-await-socket-absence-async)
               (lambda (_timeout _absent timeout _context) (setq timeout-called timeout) :timer))
              ((symbol-function 'agent-repl--frontend-start-daemon) (lambda () (setq started t))))
      (agent-repl--frontend-bounce-foreign-daemon)
      (should-error (funcall timeout-called))
      (should-not started))))

;;;; ---- ensure: the spawn-in-flight guard -----------------------------------

(ert-deftest agent-repl-test-daemon-ensure-suppresses-spawn-within-boot-grace ()
  "An ensure arriving while a spawn is still booting starts nothing."
  ;; Arrange — a spawn started one second ago, still inside its grace.
  (agent-repl-test--with-daemon-env
   (let ((agent-repl-frontend-spawn-boot-grace-seconds 30.0)
         (built nil) (spawned nil))
     (setq agent-repl--frontend-daemon-spawn-time (- (float-time) 1.0))
     (cl-letf (((symbol-function 'agent-repl--frontend-daemon-responsive-async)
                (lambda (_open absent) (funcall absent 'no-listener)))
               ((symbol-function 'agent-repl--frontend-deploy-stack-async)
                (lambda (&optional _f on-success _on-failure)
                  (setq built t) (when on-success (funcall on-success)) 'started))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () (setq spawned t) (agent-repl-test--make-live-daemon))))
       ;; Act
       (let ((result (agent-repl--ensure-frontend-daemon)))
         ;; Assert
         (should (eq result :pending))
         (should-not built)
         (should-not spawned))))))

(ert-deftest agent-repl-test-daemon-ensure-spawns-after-boot-grace-expires ()
  "A spawn older than the boot grace no longer suppresses a new one."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((agent-repl-frontend-spawn-boot-grace-seconds 30.0)
         (fresh (agent-repl-test--make-live-daemon 501))
         (spawned nil))
     (setq agent-repl--frontend-daemon-spawn-time (- (float-time) 31.0))
     (cl-letf (((symbol-function 'agent-repl--frontend-daemon-responsive-async)
                (lambda (_open absent) (funcall absent 'no-listener)))
               ((symbol-function 'agent-repl--frontend-deploy-stack-async)
                (lambda (&optional _f on-success _on-failure)
                  (when on-success (funcall on-success)) 'started))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () (setq spawned t) fresh)))
       ;; Act
       (agent-repl--ensure-frontend-daemon)
       ;; Assert
       (should spawned)))))

(ert-deftest agent-repl-test-daemon-ensure-spawns-when-tracked-spawn-died ()
  "A spawn that died inside its grace releases it, so a replacement starts."
  ;; Arrange — young spawn, but its process is already dead.
  (agent-repl-test--with-daemon-env
   (let ((agent-repl-frontend-spawn-boot-grace-seconds 30.0)
         (dead (make-agent-repl-test--fake-daemon :live nil :pid 601))
         (fresh (agent-repl-test--make-live-daemon 602))
         (spawned nil))
     (setq agent-repl--frontend-daemon-process dead)
     (setq agent-repl--frontend-daemon-spawn-time (- (float-time) 1.0))
     (cl-letf (((symbol-function 'agent-repl--frontend-daemon-responsive-async)
                (lambda (_open absent) (funcall absent 'no-listener)))
               ((symbol-function 'agent-repl--frontend-deploy-stack-async)
                (lambda (&optional _f on-success _on-failure)
                  (when on-success (funcall on-success)) 'started))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () (setq spawned t) fresh)))
       ;; Act
       (agent-repl--ensure-frontend-daemon)
       ;; Assert
       (should spawned)))))

(ert-deftest agent-repl-test-daemon-ensure-parked-waiter-runs-on-spawn-success ()
  "A suppressed ensure's ON-ENSURED runs when the in-flight spawn succeeds."
  ;; Arrange — park a waiter, then let a later spawn settle it.
  (agent-repl-test--with-daemon-env
   (let ((agent-repl-frontend-spawn-boot-grace-seconds 30.0)
         (ensured nil))
     (setq agent-repl--frontend-daemon-spawn-time (- (float-time) 1.0))
     (agent-repl--ensure-frontend-daemon nil (lambda () (setq ensured t)) #'ignore)
     (should-not ensured)
     ;; Act — the spawn everyone waited on completes.
     (agent-repl--frontend-settle-spawn-waiters nil)
     ;; Assert
     (should ensured))))

(ert-deftest agent-repl-test-daemon-ensure-parked-waiter-fails-on-spawn-failure ()
  "A suppressed ensure's ON-FAILURE receives the failed spawn's detail."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((agent-repl-frontend-spawn-boot-grace-seconds 30.0)
         (detail nil))
     (setq agent-repl--frontend-daemon-spawn-time (- (float-time) 1.0))
     (agent-repl--ensure-frontend-daemon nil #'ignore (lambda (d) (setq detail d)))
     ;; Act
     (agent-repl--frontend-settle-spawn-waiters "deploy exploded")
     ;; Assert
     (should (equal detail "deploy exploded")))))

(ert-deftest agent-repl-test-daemon-ensure-force-ignores-the-boot-grace ()
  "FORCE is an explicit restart and is never suppressed by a spawn in flight."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((agent-repl-frontend-spawn-boot-grace-seconds 30.0)
         (fresh (agent-repl-test--make-live-daemon 701))
         (spawned nil))
     (setq agent-repl--frontend-daemon-spawn-time (- (float-time) 1.0))
     (cl-letf (((symbol-function 'agent-repl--frontend-daemon-responsive-async)
                (lambda (_open absent) (funcall absent 'no-listener)))
               ((symbol-function 'agent-repl--frontend-deploy-stack-async)
                (lambda (&optional _f on-success _on-failure)
                  (when on-success (funcall on-success)) 'started))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () (setq spawned t) fresh)))
       ;; Act
       (agent-repl--ensure-frontend-daemon t)
       ;; Assert
       (should spawned)))))

(ert-deftest agent-repl-test-daemon-ensure-failed-deploy-reopens-spawning ()
  "A deploy failure ends the boot grace so the next ensure may retry at once."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((agent-repl-frontend-spawn-boot-grace-seconds 30.0))
     (cl-letf (((symbol-function 'agent-repl--frontend-daemon-responsive-async)
                (lambda (_open absent) (funcall absent 'no-listener)))
               ((symbol-function 'agent-repl--frontend-deploy-stack-async)
                (lambda (&optional _f _on-success on-failure)
                  (when on-failure (funcall on-failure "build failed")) 'started)))
       ;; Act
       (agent-repl--ensure-frontend-daemon nil #'ignore #'ignore)
       ;; Assert
       (should-not (agent-repl--frontend-spawn-in-flight-p))))))

;;;; ---- ensure: adoption of an untracked live daemon ------------------------

(ert-deftest agent-repl-test-daemon-ensure-adopts-untracked-live-daemon ()
  "A live `claude-repld' this Emacs did not spawn is adopted, not competed with."
  ;; Arrange — nothing answering on the socket yet, but the process exists.
  (agent-repl-test--with-daemon-env
   (let ((built nil) (spawned nil))
     (cl-letf (((symbol-function 'agent-repl--frontend-daemon-responsive-async)
                (lambda (_open absent) (funcall absent 'no-listener)))
               ((symbol-function 'agent-repl--frontend-run-daemon-pgrep)
                (lambda (&rest _) "31337\n"))
               ((symbol-function 'agent-repl--frontend-deploy-stack-async)
                (lambda (&optional _f on-success _on-failure)
                  (setq built t) (when on-success (funcall on-success)) 'started))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () (setq spawned t) (agent-repl-test--make-live-daemon))))
       ;; Act
       (agent-repl--ensure-frontend-daemon)
       ;; Assert
       (should (eql agent-repl--frontend-adopted-daemon-pid 31337))
       (should-not built)
       (should-not spawned)))))

(ert-deftest agent-repl-test-daemon-ensure-adoption-runs-on-ensured ()
  "Adopting an untracked live daemon settles the caller through ON-ENSURED."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((ensured nil))
     (cl-letf (((symbol-function 'agent-repl--frontend-daemon-responsive-async)
                (lambda (_open absent) (funcall absent 'no-listener)))
               ((symbol-function 'agent-repl--frontend-run-daemon-pgrep)
                (lambda (&rest _) "31337\n")))
       ;; Act
       (agent-repl--ensure-frontend-daemon nil (lambda () (setq ensured t)) #'ignore)
       ;; Assert
       (should ensured)))))

(ert-deftest agent-repl-test-daemon-ensure-force-does-not-adopt-untracked-daemon ()
  "FORCE wants a fresh process, so an untracked live daemon is not adopted."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((spawned nil)
         (fresh (agent-repl-test--make-live-daemon 801)))
     (cl-letf (((symbol-function 'agent-repl--frontend-daemon-responsive-async)
                (lambda (_open absent) (funcall absent 'no-listener)))
               ((symbol-function 'agent-repl--frontend-run-daemon-pgrep)
                (lambda (&rest _) "31337\n"))
               ((symbol-function 'agent-repl--frontend-deploy-stack-async)
                (lambda (&optional _f on-success _on-failure)
                  (when on-success (funcall on-success)) 'started))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () (setq spawned t) fresh)))
       ;; Act
       (agent-repl--ensure-frontend-daemon t)
       ;; Assert
       (should spawned)))))

(ert-deftest agent-repl-test-daemon-untracked-pid-excludes-the-tracked-process ()
  "The pid this Emacs already tracks is never reported as an untracked daemon."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (setq agent-repl--frontend-daemon-process (agent-repl-test--make-live-daemon 4242))
   (cl-letf (((symbol-function 'agent-repl--frontend-run-daemon-pgrep)
              (lambda (&rest _) "4242\n")))
     ;; Act / Assert
     (should-not (agent-repl--frontend-untracked-daemon-pid)))))

(ert-deftest agent-repl-test-daemon-untracked-pid-nil-when-pgrep-fails ()
  "A pgrep that could not run reports no daemon rather than a guess."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (cl-letf (((symbol-function 'agent-repl--frontend-run-daemon-pgrep)
              (lambda (&rest _) nil)))
     ;; Act / Assert
     (should-not (agent-repl--frontend-untracked-daemon-pid)))))

(ert-deftest agent-repl-test-daemon-forget-process-clears-the-boot-grace ()
  "Dropping the tracked process drops the grace that belonged to its spawn."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (setq agent-repl--frontend-daemon-process (agent-repl-test--make-live-daemon 901))
   (setq agent-repl--frontend-daemon-spawn-time (float-time))
   ;; Act
   (agent-repl--frontend-forget-daemon-process)
   ;; Assert
   (should-not (agent-repl--frontend-spawn-in-flight-p))))

;;;; ---- Foreign-daemon adoption + stop guard ---------------------------------

(ert-deftest agent-repl-test-daemon-ensure-adopts-foreign-daemon ()
  "A daemon answering on the socket that this Emacs does not track is adopted."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((built nil) (spawned nil))
     (cl-letf (((symbol-function 'agent-repl--frontend-daemon-responsive-async)
                (lambda (open _absent) (funcall open)))
               ((symbol-function 'agent-repl--frontend-deploy-stack-async)
                (lambda (&optional _f on-success _on-failure)
                  (setq built t) (when on-success (funcall on-success)) 'started))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () (setq spawned t) (agent-repl-test--make-live-daemon))))
       ;; Act
       (let ((result (agent-repl--ensure-frontend-daemon)))
         ;; Assert — adopted (non-nil, no process object), nothing spawned.
         (should (eq result :pending))
         (should-not built)
         (should-not spawned))))))

(ert-deftest agent-repl-test-daemon-ensure-force-skips-adoption ()
  "FORCE ignores a responsive foreign daemon and builds/spawns fresh."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((built nil)
         (fresh (agent-repl-test--make-live-daemon 9)))
     (cl-letf (((symbol-function 'agent-repl--frontend-daemon-responsive-async)
                (lambda (open _absent) (funcall open)))
               ((symbol-function 'agent-repl--frontend-deploy-stack-async)
                (lambda (&optional _f on-success _on-failure)
                  (setq built t) (when on-success (funcall on-success)) 'started))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () fresh)))
       ;; Act
       (let ((result (agent-repl--ensure-frontend-daemon t)))
         ;; Assert
         (should built)
         (should (eq result :pending)))))))

(ert-deftest agent-repl-test-daemon-stop-refuses-stop-shims-during-turn ()
  "A STOP-SHIMS stop is refused while any daemon session has a turn in flight.
Stopping the shims kills the process running the turn; that is the harm the
guard exists for, and the mode it is now scoped to."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((live (agent-repl-test--make-live-daemon)))
     (setq agent-repl--frontend-daemon-process live)
     (cl-letf (((symbol-function 'agent-repl--frontend-turn-active-sessions)
                (lambda () '("s_busy"))))
       ;; Act / Assert
       (let ((err (should-error (agent-repl--frontend-stop-daemon nil t))))
         (should (string-match-p "turn in flight" (error-message-string err))))
       ;; The daemon survives the refusal.
       (should (agent-repl-test--fake-daemon-live live))
       (should (eq agent-repl--frontend-daemon-process live))))))

(ert-deftest agent-repl-test-daemon-preserving-stop-proceeds-during-turn ()
  "A shim-PRESERVING stop is not refused for a turn in flight.
The shim outlives the stop and keeps serving its turn, so the conversation
survives and a daemon-only deploy is never blocked behind it."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((live (agent-repl-test--make-live-daemon))
         (signalled nil))
     (setq agent-repl--frontend-daemon-process live)
     (cl-letf (((symbol-function 'agent-repl--frontend-turn-active-sessions)
                (lambda () '("s_busy")))
               ((symbol-function 'signal-process)
                (lambda (_proc _sig) (setq signalled t)))
               ((symbol-function 'agent-repl--frontend-await-async)
                (lambda (&rest _) :pending)))
       ;; Act
       (should (eq :pending (agent-repl--frontend-stop-daemon)))
       ;; Assert
       (should signalled)))))

(ert-deftest agent-repl-test-daemon-stop-force-bypasses-turn-guard ()
  "FORCE stops the daemon without consulting the turn probe."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((live (agent-repl-test--make-live-daemon))
         (probed nil))
     (setq agent-repl--frontend-daemon-process live)
     (cl-letf (((symbol-function 'agent-repl--frontend-turn-active-sessions)
                (lambda () (setq probed t) '("s_busy"))))
       ;; Act
       (agent-repl--frontend-stop-daemon t)
       ;; Assert
       (should-not probed)
       (should-not (agent-repl-test--fake-daemon-live live))
       (should-not agent-repl--frontend-daemon-process)))))

(ert-deftest agent-repl-test-daemon-stop-proceeds-when-idle ()
  "Stopping proceeds normally when no turn is in flight."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((live (agent-repl-test--make-live-daemon)))
     (setq agent-repl--frontend-daemon-process live)
     (cl-letf (((symbol-function 'agent-repl--frontend-turn-active-sessions)
                (lambda () nil)))
       ;; Act
       (agent-repl--frontend-stop-daemon)
       ;; Assert
       (should-not (agent-repl-test--fake-daemon-live live))
       (should-not agent-repl--frontend-daemon-process)))))

;;;; ---- start-daemon: guard --------------------------------------------------

(ert-deftest agent-repl-test-daemon-shim-entry-follows-relocated-layout ()
  "The launch path names the Claude shim's relocated artifact exactly."
  (should
   (equal agent-repl--frontend-shim-entry
          (expand-file-name "agent-shim/claude/shim/dist/main.js"
                            agent-repl--frontend-root))))

(ert-deftest agent-repl-test-daemon-start-errors-when-binary-missing ()
  "Starting errors before process creation when the daemon binary is absent."
  (let ((spawned nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-artifact-exists-p)
               (lambda (_path) nil))
              ((symbol-function 'agent-repl--frontend-spawn-daemon)
               (lambda () (setq spawned t))))
      (should-error (agent-repl--frontend-start-daemon))
      (should-not spawned))))

(ert-deftest agent-repl-test-daemon-start-errors-when-shim-missing ()
  "Starting errors before process creation when the relocated shim is absent."
  (let ((spawned nil)
        (agent-repl--frontend-daemon-bin "/artifacts/claude-repld")
        (agent-repl--frontend-shim-entry "/artifacts/claude/shim/main.js"))
    (cl-letf (((symbol-function 'agent-repl--frontend-artifact-exists-p)
               (lambda (path)
                 (equal path agent-repl--frontend-daemon-bin)))
              ((symbol-function 'agent-repl--frontend-spawn-daemon)
               (lambda () (setq spawned t))))
      (let ((err (should-error (agent-repl--frontend-start-daemon))))
        (should (string-match-p
                 (regexp-quote agent-repl--frontend-shim-entry)
                 (error-message-string err))))
      (should-not spawned))))

;;;; ---- daemon-command shape ------------------------------------------------

(ert-deftest agent-repl-test-daemon-command-carries-flags ()
  "The daemon argv includes -addr, -shim, and -webapp with their values."
  ;; Arrange
  (let ((agent-repl-frontend-daemon-addr "127.0.0.1:9999"))
    ;; Act
    (let ((cmd (agent-repl--frontend-daemon-command)))
      ;; Assert
      (should (member "-addr" cmd))
      (should (member "127.0.0.1:9999" cmd))
      (should
       (equal (cadr (member "-shim" cmd))
              (expand-file-name "agent-shim/claude/shim/dist/main.js"
                                agent-repl--frontend-root)))
      (should (member agent-repl--frontend-webapp-dir
                      (cdr (member "-webapp" cmd)))))))

(ert-deftest agent-repl-test-daemon-command-carries-accounts-roster ()
  "The daemon argv carries the canonical account roster via -accounts."
  ;; Arrange
  (let ((agent-repl-multi-repo-config-dir "~/.claude-chesscom"))
    ;; Act
    (let ((cmd (agent-repl--frontend-daemon-command)))
      ;; Assert
      (should (member (format "personal=,work=%s"
                              (expand-file-name "~/.claude-chesscom"))
                      (cdr (member "-accounts" cmd)))))))

(ert-deftest agent-repl-test-daemon-accounts-flag-expands-the-work-root ()
  "The -accounts value names the CLI default as personal and the expanded
multi-repo config dir as work, matching how sessions record the dirs."
  ;; Arrange
  (let ((agent-repl-multi-repo-config-dir "~/.claude-chesscom"))
    ;; Act / Assert
    (should (equal (agent-repl--frontend-accounts-flag)
                   (format "personal=,work=%s"
                           (expand-file-name "~/.claude-chesscom"))))))

(ert-deftest agent-repl-test-daemon-command-carries-widget-assets-dir ()
  "A configured widget-assets dir rides to the daemon expanded."
  ;; Arrange
  (let ((agent-repl-frontend-widget-assets-dir "~/ee/apps/cee-web-widget/dist"))
    ;; Act
    (let ((cmd (agent-repl--frontend-daemon-command)))
      ;; Assert
      (should (member (expand-file-name "~/ee/apps/cee-web-widget/dist")
                      (cdr (member "-widget-assets" cmd)))))))

(ert-deftest agent-repl-test-daemon-command-omits-widget-assets-when-empty ()
  "With no explicit dir and nothing discoverable, the flag stays off the argv."
  ;; Arrange — empty dir and discovery disabled (nil search root).
  (let ((agent-repl-frontend-widget-assets-dir "")
        (agent-repl-frontend-widget-assets-search-root nil))
    ;; Act
    (let ((cmd (agent-repl--frontend-daemon-command)))
      ;; Assert
      (should-not (member "-widget-assets" cmd)))))

(ert-deftest agent-repl-test-daemon-command-carries-discovered-widget-assets ()
  "An empty dir lets auto-discovery put a found dist on the argv."
  ;; Arrange — no explicit dir, but a discoverable dist under the search root.
  (agent-repl-test--with-temp-root root
    (let* ((agent-repl-frontend-widget-assets-dir "")
           (agent-repl-frontend-widget-assets-search-root root)
           (dist (expand-file-name "explanation-engine/apps/cee-web-widget/dist" root)))
      (agent-repl-test--make-widget-dist dist)
      ;; Act
      (let ((cmd (agent-repl--frontend-daemon-command)))
        ;; Assert
        (should (member dist (cdr (member "-widget-assets" cmd))))))))

;;;; ---- sentinel and lifecycle ----------------------------------------------

(ert-deftest agent-repl-test-daemon-sentinel-clears-on-death ()
  "The sentinel nils the tracked process once it is no longer live."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((proc (make-agent-repl-test--fake-daemon :live nil :pid 5)))
     (setq agent-repl--frontend-daemon-process proc)
     ;; Act
     (agent-repl--frontend-daemon-sentinel proc "exited abnormally\n")
     ;; Assert
     (should (null agent-repl--frontend-daemon-process)))))

(ert-deftest agent-repl-test-daemon-live-p-nil-without-process ()
  "The live predicate is nil when no process is tracked."
  ;; Arrange
  (let ((agent-repl--frontend-daemon-process nil))
    ;; Act / Assert
    (should-not (agent-repl--frontend-daemon-live-p))))

(ert-deftest agent-repl-test-daemon-env-turn-probe-is-idle-by-default ()
  "The daemon test env reports no in-flight turns without a per-test stub.
Regression guard: the unstubbed probe queries the developer's live
`claude-repld' over HTTP, which made every unstubbed daemon test fail
whenever a real session happened to be mid-turn."
  ;; Arrange / Act / Assert
  (agent-repl-test--with-daemon-env
   (should-not (agent-repl--frontend-turn-active-sessions))))

;; Master's `stop-deletes-and-clears' is deliberately superseded here: the
;; stop path no longer SIGKILLs first, so its assertion (delete-process ran)
;; would now encode the very bug the registry exists to survive.  Its two
;; guarantees live on split across the three tests below (TERM is sent, the
;; tracker is cleared) plus the SIGKILL fallback for a hung daemon.
(ert-deftest agent-repl-test-daemon-stop-signals-term-first ()
  "Stopping delivers SIGTERM so the daemon runs its graceful shutdown."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((proc (agent-repl-test--make-live-daemon)))
     (setq agent-repl--frontend-daemon-process proc)
     (cl-letf (((symbol-function 'agent-repl--frontend-turn-active-sessions)
                (lambda () nil)))
       ;; Act
       (agent-repl--frontend-stop-daemon))
     ;; Assert
     (should (equal (agent-repl-test--fake-daemon-signals proc) '(TERM))))))

(ert-deftest agent-repl-test-daemon-stop-graceful-exit-skips-kill ()
  "A daemon that exits on TERM within the grace window is never SIGKILLed."
  ;; Arrange — term-behavior `exit' (the default) dies on TERM.
  (agent-repl-test--with-daemon-env
   (let ((proc (agent-repl-test--make-live-daemon)))
     (setq agent-repl--frontend-daemon-process proc)
     ;; Act
     (agent-repl--frontend-stop-daemon)
     ;; Assert
     (should-not (agent-repl-test--fake-daemon-deleted proc))
     (should-not (agent-repl-test--fake-daemon-live proc)))))

(ert-deftest agent-repl-test-daemon-stop-falls-back-to-kill ()
  "A daemon that outlives the grace window falls back to `delete-process'."
  ;; Arrange — a hung daemon and a zero grace window (no real waiting).
  (agent-repl-test--with-daemon-env
   (let ((proc (agent-repl-test--make-live-daemon))
         (agent-repl-frontend-stop-grace-seconds 0))
     (setf (agent-repl-test--fake-daemon-term-behavior proc) 'ignore)
     (setq agent-repl--frontend-daemon-process proc)
     ;; Act
     (agent-repl--frontend-stop-daemon)
     ;; Assert
     (should (agent-repl-test--fake-daemon-deleted proc))
     (should-not (agent-repl-test--fake-daemon-live proc)))))

(ert-deftest agent-repl-test-daemon-stop-clears-tracker ()
  "Stopping clears the tracked process variable."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((proc (agent-repl-test--make-live-daemon)))
     (setq agent-repl--frontend-daemon-process proc)
     ;; Act
     (agent-repl--frontend-stop-daemon)
     ;; Assert
     (should (null agent-repl--frontend-daemon-process)))))

(ert-deftest agent-repl-test-daemon-stop-command-reports-only-after-completion ()
  "The interactive stop command never claims completion while shutdown is pending."
  (let (on-stopped messages)
    (cl-letf (((symbol-function 'agent-repl--frontend-stop-daemon)
               (lambda (_force _stop-shims callback)
                 (setq on-stopped callback)
                 :pending))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (should (eq (agent-repl-frontend-daemon-stop) :pending))
      (should (functionp on-stopped))
      (should-not messages)
      (funcall on-stopped)
      (should (equal messages '("claude-repld stopped."))))))

;;;; ---- restart command: canonical runtime delegation ----------------------

(ert-deftest agent-repl-test-daemon-restart-delegates-to-runtime-coordinator ()
  "The legacy daemon command has one implementation: the full runtime restart."
  (let (called)
    (cl-letf (((symbol-function 'agent-repl-runtime-restart)
               (lambda (&optional _stop-shims) (setq called t) 3)))
      (should (= 3 (agent-repl-frontend-daemon-restart)))
      (should called))))

(ert-deftest agent-repl-test-daemon-restart-preserves-shims-by-default ()
  "A plain restart asks the coordinator to LEAVE the session shims running."
  (let (arg)
    (cl-letf (((symbol-function 'agent-repl-runtime-restart)
               (lambda (&optional stop-shims) (setq arg (list stop-shims)) 0)))
      (agent-repl-frontend-daemon-restart)
      (should (equal arg (list nil))))))

(ert-deftest agent-repl-test-daemon-restart-forwards-stop-shims ()
  "The prefix argument reaches the coordinator as the stop-shims mode."
  (let (arg)
    (cl-letf (((symbol-function 'agent-repl-runtime-restart)
               (lambda (&optional stop-shims) (setq arg (list stop-shims)) 0)))
      (agent-repl-frontend-daemon-restart '(4))
      (should (equal arg (list t))))))

(ert-deftest agent-repl-test-daemon-restart-await-delegates-terminal-coordinator ()
  "The deployment restart surface forwards mode and timeout exactly once."
  (let (args)
    (cl-letf (((symbol-function 'agent-repl-runtime-restart-await)
               (lambda (&optional stop-shims timeout _initiator)
                 (setq args (list stop-shims timeout))
                 "runtime-restart-complete")))
      (should (equal "runtime-restart-complete"
                     (agent-repl-frontend-daemon-restart-await '(4) 17.0)))
      (should (equal args '(t 17.0))))))

(ert-deftest agent-repl-test-daemon-restart-await-names-the-deploy-as-initiator ()
  "The deploy names itself, so the exit it causes is recorded as its doing."
  ;; Arrange
  (let (initiator)
    (cl-letf (((symbol-function 'agent-repl-runtime-restart-await)
               (lambda (&optional _stop-shims _timeout who)
                 (setq initiator who)
                 "runtime-restart-complete")))
      ;; Act
      (agent-repl-frontend-daemon-restart-await)
      ;; Assert
      (should (equal initiator "deploy (emacsclient)")))))

(ert-deftest agent-repl-test-foreign-shutdown-omits-stop-shims-by-default ()
  "`ShutdownCmd' carries no payload unless stop-shims was asked for."
  (let (sent)
    (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () t))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (field payload &rest _) (setq sent (list field payload)) "req-1")))
      (agent-repl--frontend-request-foreign-shutdown)
      (should (equal sent (list "shutdown" nil))))))

(ert-deftest agent-repl-test-foreign-shutdown-sets-stop-shims ()
  "A stop-shims request sets `ShutdownCmd.stop_shims' on the wire."
  (let (sent)
    (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () t))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (field payload &rest _) (setq sent (list field payload)) "req-1")))
      (agent-repl--frontend-request-foreign-shutdown t)
      (should (equal sent (list "shutdown" (list :stopShims t)))))))

;;;; ---- Scheduled shutdown (the drain lease) --------------------------------

(defmacro agent-repl-test--with-lease (lease-id &rest body)
  "Run BODY with the recorded drain lease reporting LEASE-ID and a stub link.
LEASE-ID is the live schedule id, or nil for \"no schedule known\".  The
lease readers live in frontend-state.el, which this batch file does not
load, so they are stubbed here rather than reached."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-repl-frontend-scheduled-shutdown-id)
              (lambda () ,lease-id))
             ((symbol-function 'agent-repl-frontend-shutdown-schedule)
              (lambda () (when ,lease-id (list :state :draining :scheduleId ,lease-id))))
             ((symbol-function 'agent-repl--uds-connected-p) (lambda () t)))
     ,@body))

(ert-deftest agent-repl-test-scheduled-shutdown-sends-the-schedule-command ()
  "Scheduling emits `scheduleShutdown' carrying its cause."
  ;; Arrange
  (let (sent)
    (agent-repl-test--with-lease nil
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (lambda (field payload &rest _) (setq sent (list field payload)) "req-1")))
        ;; Act
        (agent-repl--frontend-request-scheduled-shutdown "manual restart")
        ;; Assert
        (should (equal sent '("scheduleShutdown" (:cause "manual restart"))))))))

(ert-deftest agent-repl-test-scheduled-shutdown-omits-stop-shims-by-default ()
  "The default schedule PRESERVES shims, exactly as the immediate path does."
  ;; Arrange
  (let (sent)
    (agent-repl-test--with-lease nil
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (lambda (_field payload &rest _) (setq sent payload) "req-1")))
        ;; Act
        (agent-repl--frontend-request-scheduled-shutdown "manual restart")
        ;; Assert
        (should-not (plist-member sent :stopShims))))))

(ert-deftest agent-repl-test-scheduled-shutdown-sets-stop-shims ()
  "A stop-shims schedule fixes `stop_shims' at schedule time."
  ;; Arrange
  (let (sent)
    (agent-repl-test--with-lease nil
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (lambda (_field payload &rest _) (setq sent payload) "req-1")))
        ;; Act
        (agent-repl--frontend-request-scheduled-shutdown "bundle changed" t)
        ;; Assert
        (should (equal sent '(:cause "bundle changed" :stopShims t)))))))


(ert-deftest agent-repl-test-scheduled-shutdown-dials-when-disconnected ()
  "A down link is dialed first — a foreign daemon owns the same socket."
  ;; Arrange
  (let ((dials 0))
    (agent-repl-test--with-lease nil
      (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () nil))
                ((symbol-function 'agent-repl-uds-connect)
                 (lambda (&optional _p) (cl-incf dials) nil))
                ((symbol-function 'agent-repl--uds-send-command) (lambda (&rest _) "req-1")))
        ;; Act
        (agent-repl--frontend-request-scheduled-shutdown "manual restart")
        ;; Assert
        (should (= dials 1))))))

(ert-deftest agent-repl-test-scheduled-shutdown-refuses-a-blank-cause ()
  "A blank cause is refused: the lease's only readable field must say something."
  ;; Arrange
  (agent-repl-test--with-lease nil
    (cl-letf (((symbol-function 'agent-repl--uds-send-command)
               (lambda (&rest _) (error "must not send"))))
      ;; Act / Assert
      (should-error (agent-repl--frontend-request-scheduled-shutdown "   ")
                    :type 'user-error))))

(ert-deftest agent-repl-test-scheduled-shutdown-refuses-a-second-schedule ()
  "Scheduling over a live lease is refused, never a silent replace."
  ;; Arrange
  (agent-repl-test--with-lease "sch-live"
    (cl-letf (((symbol-function 'agent-repl--uds-send-command)
               (lambda (&rest _) (error "must not send"))))
      ;; Act / Assert
      (should-error (agent-repl--frontend-request-scheduled-shutdown "second")
                    :type 'user-error))))

(ert-deftest agent-repl-test-cancel-scheduled-shutdown-sends-the-live-id ()
  "The cancel names the live schedule, so it can never kill a newer one."
  ;; Arrange
  (let (sent)
    (agent-repl-test--with-lease "sch-live"
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (lambda (field payload &rest _) (setq sent (list field payload)) "req-4")))
        ;; Act
        (agent-repl--frontend-request-cancel-scheduled-shutdown)
        ;; Assert
        (should (equal sent '("cancelScheduledShutdown" (:scheduleId "sch-live"))))))))


(ert-deftest agent-repl-test-cancel-without-a-schedule-errors-loudly ()
  "A cancel with no known schedule is a loud refusal, never a silent no-op."
  ;; Arrange
  (agent-repl-test--with-lease nil
    (cl-letf (((symbol-function 'agent-repl--uds-send-command)
               (lambda (&rest _) (error "must not send"))))
      ;; Act / Assert
      (should-error (agent-repl--frontend-request-cancel-scheduled-shutdown)
                    :type 'user-error))))

(ert-deftest agent-repl-test-cancel-without-a-schedule-logs-the-refusal ()
  "The refused cancel is instrumented, not merely signalled."
  ;; Arrange
  (let (logged)
    (agent-repl-test--with-lease nil
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        ;; Act
        (ignore-errors (agent-repl--frontend-request-cancel-scheduled-shutdown))
        ;; Assert
        (should (seq-find (lambda (m) (string-match-p "REFUSING — no live schedule" m))
                          logged))))))

(ert-deftest agent-repl-test-scheduled-restart-composes-its-cause ()
  "The interactive scheduled restart folds the reason into a named cause."
  ;; Arrange
  (let (cause)
    (cl-letf (((symbol-function 'agent-repl--frontend-request-scheduled-shutdown)
               (lambda (c &optional _stop-shims) (setq cause c) "req-6"))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      ;; Act
      (agent-repl-frontend-daemon-restart-scheduled "daemon rebuilt")
      ;; Assert
      (should (equal cause "scheduled restart from Emacs (daemon rebuilt)")))))

(ert-deftest agent-repl-test-scheduled-restart-forwards-stop-shims ()
  "The prefix argument reaches the schedule as the stop-shims mode."
  ;; Arrange
  (let (arg)
    (cl-letf (((symbol-function 'agent-repl--frontend-request-scheduled-shutdown)
               (lambda (_cause &optional stop-shims) (setq arg (list stop-shims)) "req-6"))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      ;; Act
      (agent-repl-frontend-daemon-restart-scheduled "bundle changed" '(4))
      ;; Assert
      (should (equal arg (list t))))))

(ert-deftest agent-repl-test-scheduled-restart-refuses-a-blank-reason ()
  "A blank reason is refused before any command is composed."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-request-scheduled-shutdown)
             (lambda (&rest _) (error "must not send"))))
    ;; Act / Assert
    (should-error (agent-repl-frontend-daemon-restart-scheduled "  ")
                  :type 'user-error)))

(ert-deftest agent-repl-test-scheduled-restart-leaves-the-immediate-path-alone ()
  "The immediate restart still bounces now — the schedule is a second door."
  ;; Arrange
  (let (arg)
    (cl-letf (((symbol-function 'agent-repl-runtime-restart)
               (lambda (&optional stop-shims) (setq arg (list stop-shims)) 0))
              ((symbol-function 'agent-repl--frontend-request-scheduled-shutdown)
               (lambda (&rest _) (error "the immediate path must not schedule"))))
      ;; Act
      (agent-repl-frontend-daemon-restart)
      ;; Assert
      (should (equal arg (list nil))))))

(ert-deftest agent-repl-test-cancel-scheduled-restart-command-delegates ()
  "The interactive cancel routes through the one sender that owns the id."
  ;; Arrange
  (let (called)
    (cl-letf (((symbol-function 'agent-repl--frontend-request-cancel-scheduled-shutdown)
               (lambda () (setq called t) "req-7"))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      ;; Act
      (agent-repl-frontend-daemon-cancel-scheduled-restart)
      ;; Assert
      (should called))))

;;;; ---- Widget-assets auto-discovery -----------------------------------------

(ert-deftest agent-repl-test--widget-override-wins ()
  "An explicit widget-assets dir is returned verbatim, ahead of discovery."
  (agent-repl-test--with-temp-root root
    (let ((agent-repl-frontend-widget-assets-dir (expand-file-name "explicit" root))
          (agent-repl-frontend-widget-assets-search-root root))
      ;; A real discoverable dist is present, yet the override still wins.
      (agent-repl-test--make-widget-dist
       (expand-file-name "explanation-engine/apps/cee-web-widget/dist" root))
      (should (equal (agent-repl--frontend-discover-widget-assets-dir)
                     (expand-file-name "explicit" root))))))

(ert-deftest agent-repl-test--widget-discovers-canonical-checkout ()
  "An empty dir auto-discovers the canonical explanation-engine dist."
  (agent-repl-test--with-temp-root root
    (let* ((agent-repl-frontend-widget-assets-dir "")
           (agent-repl-frontend-widget-assets-search-root root)
           (dist (expand-file-name "explanation-engine/apps/cee-web-widget/dist" root)))
      (agent-repl-test--make-widget-dist dist)
      (should (equal (agent-repl--frontend-discover-widget-assets-dir) dist)))))

(ert-deftest agent-repl-test--widget-discovers-worktree-dist ()
  "With no canonical dist, discovery finds one under explanation-engine-worktrees."
  (agent-repl-test--with-temp-root root
    (let* ((agent-repl-frontend-widget-assets-dir "")
           (agent-repl-frontend-widget-assets-search-root root)
           (dist (expand-file-name
                  "explanation-engine-worktrees/wt1/apps/cee-web-widget/dist" root)))
      (agent-repl-test--make-widget-dist dist)
      (should (equal (agent-repl--frontend-discover-widget-assets-dir) dist)))))

(ert-deftest agent-repl-test--widget-skips-dist-without-bundle ()
  "A dist dir lacking chess-widget.js is not selected."
  (agent-repl-test--with-temp-root root
    (let ((agent-repl-frontend-widget-assets-dir "")
          (agent-repl-frontend-widget-assets-search-root root))
      (make-directory
       (expand-file-name "explanation-engine/apps/cee-web-widget/dist" root) t)
      (should-not (agent-repl--frontend-discover-widget-assets-dir)))))

(ert-deftest agent-repl-test--widget-discovery-nil-when-absent ()
  "Discovery returns nil when no dist exists under the search root."
  (agent-repl-test--with-temp-root root
    (let ((agent-repl-frontend-widget-assets-dir "")
          (agent-repl-frontend-widget-assets-search-root root))
      (should-not (agent-repl--frontend-discover-widget-assets-dir)))))

;;;; ---- Widget-assets doctor check -------------------------------------------

(ert-deftest agent-repl-test--widget-doctor-warns-when-off ()
  "With nothing discoverable, the doctor warns the capability is off."
  (let* ((agent-repl-frontend-widget-assets-dir "")
         (agent-repl-frontend-widget-assets-search-root nil)
         (issues (agent-repl--widget-doctor-issues)))
    (should (= 1 (length issues)))
    (should (eq 'warn (caar issues)))
    (should (string-match-p "capability OFF" (cdar issues)))))

(ert-deftest agent-repl-test--widget-doctor-clean-when-bundle-present ()
  "A discoverable dist holding chess-widget.js yields no doctor issue."
  (agent-repl-test--with-temp-root root
    (let ((agent-repl-frontend-widget-assets-dir "")
          (agent-repl-frontend-widget-assets-search-root root))
      (agent-repl-test--make-widget-dist
       (expand-file-name "explanation-engine/apps/cee-web-widget/dist" root))
      (should-not (agent-repl--widget-doctor-issues)))))

(ert-deftest agent-repl-test--widget-doctor-warns-when-dir-lacks-bundle ()
  "An explicit dir without chess-widget.js warns about the missing bundle."
  (agent-repl-test--with-temp-root root
    ;; Override points at a real dir that lacks the mount bundle.
    (let ((agent-repl-frontend-widget-assets-dir root))
      (let ((issues (agent-repl--widget-doctor-issues)))
        (should (= 1 (length issues)))
        (should (eq 'warn (caar issues)))
        (should (string-match-p "lacks chess-widget.js" (cdar issues)))))))

;;;; ---- Incompatible-daemon detection -------------------------------------

(defmacro agent-repl-test--with-daemon-boot (&rest body)
  "Run BODY with every incompatible-listener external boundary stubbed inert.
Each stub is overridden per test; the defaults make an unmocked path
obvious (no listener, no build, no spawn) rather than reaching the host."
  (declare (indent 0))
  `(let ((agent-repl-frontend-daemon-addr "127.0.0.1:8787")
         (agent-repl-frontend-incompatible-stop-grace-seconds 0.05)
         (agent-repl--frontend-daemon-process nil))
     (cl-letf (((symbol-function 'agent-repl--frontend-run-listener-probe)
                (lambda (&rest _) nil))
               ((symbol-function 'agent-repl--frontend-deploy-stack)
                (lambda (&rest _) 0))
               ((symbol-function 'agent-repl--frontend-start-daemon)
                (lambda (&rest _) 'started))
               ((symbol-function 'agent-repl--signal-process)
                (lambda (&rest _) t))
               ((symbol-function 'agent-repl-uds-probe-async)
                (lambda (_path _open failure) (funcall failure 'absent))))
       ,@body)))

(defun agent-repl-test--incompatible-daemon-result ()
  "Return the immediate result delivered by the incompatible-daemon callback."
  (let (result)
    (agent-repl--frontend-incompatible-daemon-async (lambda (value) (setq result value)))
    result))

;; --- port parsing -------------------------------------------------------

(ert-deftest agent-repl-test-daemon-port-parsed-from-addr ()
  "The port is read off the configured listen address."
  ;; Arrange
  (let ((agent-repl-frontend-daemon-addr "127.0.0.1:8787"))
    ;; Act / Assert
    (should (equal (agent-repl--frontend-daemon-port) "8787"))))

(ert-deftest agent-repl-test-daemon-port-nil-without-one ()
  "An address with no port yields nil, making port detection a no-op."
  ;; Arrange
  (let ((agent-repl-frontend-daemon-addr "/var/run/sock"))
    ;; Act / Assert
    (should-not (agent-repl--frontend-daemon-port))))

;; --- lsof parsing -------------------------------------------------------

(ert-deftest agent-repl-test-parse-listener-reads-pid-and-command ()
  "A `lsof -F' record yields its pid and command name."
  ;; Arrange / Act
  (let ((got (agent-repl--frontend-parse-listener "p4242\ncclaude-repld\n")))
    ;; Assert
    (should (equal got '(4242 . "claude-repld")))))

(ert-deftest agent-repl-test-parse-listener-empty-output-is-nil ()
  "No listener means nil, not a zero pid."
  ;; Arrange / Act / Assert
  (should-not (agent-repl--frontend-parse-listener "")))

(ert-deftest agent-repl-test-parse-listener-nil-output-is-nil ()
  "A probe that could not run yields nil rather than erroring."
  ;; Arrange / Act / Assert
  (should-not (agent-repl--frontend-parse-listener nil)))

(ert-deftest agent-repl-test-parse-listener-takes-the-first-record ()
  "Only the first listener is reported; the port has one owner."
  ;; Arrange / Act
  (let ((got (agent-repl--frontend-parse-listener
              "p1\ncclaude-repld\np2\ncsomething-else\n")))
    ;; Assert
    (should (equal (car got) 1))))

;; --- the surgical predicate ---------------------------------------------

(ert-deftest agent-repl-test-our-daemon-command-matches-the-binary ()
  "The daemon binary's own basename is recognized."
  ;; Arrange / Act / Assert
  (should (agent-repl--frontend-our-daemon-command-p
           (file-name-nondirectory agent-repl--frontend-daemon-bin))))

(ert-deftest agent-repl-test-our-daemon-command-rejects-a-stranger ()
  "An unrelated program on the port is NOT ours."
  ;; Arrange / Act / Assert
  (should-not (agent-repl--frontend-our-daemon-command-p "node")))

(ert-deftest agent-repl-test-our-daemon-command-rejects-nil ()
  "A missing command name is not a match (never signal on a guess)."
  ;; Arrange / Act / Assert
  (should-not (agent-repl--frontend-our-daemon-command-p nil)))

;; --- detection ----------------------------------------------------------

(ert-deftest agent-repl-test-incompatible-daemon-detected ()
  "A daemon holding the port while serving no UDS is incompatible."
  ;; Arrange
  (agent-repl-test--with-daemon-boot
    (cl-letf (((symbol-function 'agent-repl--frontend-run-listener-probe)
               (lambda (&rest _)
                 (format "p999\nc%s\n"
                         (file-name-nondirectory agent-repl--frontend-daemon-bin)))))
      ;; Act / Assert
      (should (equal (agent-repl-test--incompatible-daemon-result) '(999 . "claude-repld"))))))

(ert-deftest agent-repl-test-current-daemon-is-not-incompatible ()
  "A daemon serving the frontend UDS is current and must never be killed."
  ;; Arrange
  (agent-repl-test--with-daemon-boot
    (cl-letf (((symbol-function 'agent-repl-uds-probe-async)
               (lambda (_path open _failure) (funcall open)))
              ((symbol-function 'agent-repl--frontend-run-listener-probe)
               (lambda (&rest _) "p999\ncclaude-repld\n")))
      ;; Act / Assert
      (should-not (agent-repl-test--incompatible-daemon-result)))))

(ert-deftest agent-repl-test-free-port-is-not-incompatible ()
  "Nothing listening means nothing to replace."
  ;; Arrange
  (agent-repl-test--with-daemon-boot
    ;; Act / Assert
    (should-not (agent-repl-test--incompatible-daemon-result))))

(ert-deftest agent-repl-test-foreign-listener-is-not-incompatible ()
  "A program that is not our daemon is left strictly alone."
  ;; Arrange
  (agent-repl-test--with-daemon-boot
    (cl-letf (((symbol-function 'agent-repl--frontend-run-listener-probe)
               (lambda (&rest _) "p999\ncsome-web-server\n")))
      ;; Act / Assert
      (should-not (agent-repl-test--incompatible-daemon-result)))))

(ert-deftest agent-repl-test-runtime-bounce-preflight-async-rejects-unrelated-listener ()
  "An unrelated port owner signals from the probe failure callback."
  (cl-letf (((symbol-function 'agent-repl--frontend-daemon-live-p)
             (lambda () nil))
            ((symbol-function 'agent-repl--frontend-daemon-responsive-async)
             (lambda (_open absent) (funcall absent 'absent)))
            ((symbol-function 'agent-repl--frontend-listener-owner)
             (lambda () '(987 . "other-server"))))
    (should-error (agent-repl--frontend-runtime-bounce-preflight-async #'ignore))))

(ert-deftest agent-repl-test-runtime-bounce-preflight-async-identifies-incompatible-daemon ()
  "A verified pre-UDS listener is reported through the completion callback."
  (let (state)
  (cl-letf (((symbol-function 'agent-repl--frontend-daemon-live-p)
             (lambda () nil))
            ((symbol-function 'agent-repl--frontend-daemon-responsive-async)
             (lambda (_open absent) (funcall absent 'absent)))
            ((symbol-function 'agent-repl--frontend-listener-owner)
             (lambda () '(987 . "claude-repld"))))
    (agent-repl--frontend-runtime-bounce-preflight-async (lambda (value) (setq state value)))
    (should (equal state '(:incompatible 987 . "claude-repld"))))))

;;;; ---- Backend-initiation output: capture, record, echo --------------------
;;
;; A build that fails silently and a daemon that prints a refusal to stderr
;; used to look identical from outside: `*agent-repl-build-frontend*' and
;; `*claude-repld*' held the evidence and nothing persisted or showed either.

(defmacro agent-repl-test--with-phase-echo (var &rest body)
  "Run BODY with VAR bound to a collector of every echoed phase line.
VAR accumulates newest-first, so BODY asserts with `cl-some' rather than
by position."
  (declare (indent 1))
  `(let (,var)
     (cl-letf (((symbol-function 'agent-repl--emit-message)
                (lambda (text &optional _echo) (push text ,var)))
               ((symbol-function 'agent-repl--persist-log-record) #'ignore))
       ,@body)))

(defun agent-repl-test--phase-line-p (lines &rest fragments)
  "Return non-nil when one line in LINES contains every one of FRAGMENTS."
  (cl-some (lambda (line)
             (cl-every (lambda (fragment) (string-match-p (regexp-quote fragment) line))
                       fragments))
           lines))

(defun agent-repl-test--seed-build-output (text)
  "Return a build-script stub that writes TEXT into the capture buffer."
  (lambda (_args)
    (with-current-buffer (get-buffer-create agent-repl--frontend-build-buffer)
      (insert text))
    3))

(ert-deftest agent-repl-test-daemon-build-failure-records-captured-output ()
  "A failing build's captured stderr lands in the durable structured record."
  ;; Arrange
  (let (records)
    (cl-letf (((symbol-function 'agent-repl--frontend-run-build-script)
               (agent-repl-test--seed-build-output "go: cannot find module\n"))
              ((symbol-function 'display-buffer) #'ignore)
              ((symbol-function 'agent-repl--emit-message) #'ignore)
              ((symbol-function 'agent-repl--persist-log-record)
               (lambda (_ws _level _verbosity fmt args)
                 (push (apply #'format fmt args) records))))
      ;; Act
      (should-error (agent-repl--frontend-build-if-stale nil))
      ;; Assert
      (should (cl-some (lambda (line)
                         (string-match-p "go: cannot find module" line))
                       records)))))

(ert-deftest agent-repl-test-daemon-build-failure-echoes-the-phase-and-log ()
  "A failing build names its phase and points at the log file."
  ;; Arrange
  (agent-repl-test--with-phase-echo lines
    (cl-letf (((symbol-function 'agent-repl--frontend-run-build-script)
               (agent-repl-test--seed-build-output "go: cannot find module\n"))
              ((symbol-function 'display-buffer) #'ignore))
      ;; Act
      (should-error (agent-repl--frontend-build-if-stale nil)))
    ;; Assert
    (should (agent-repl-test--phase-line-p
             lines "shim/webapp/daemon build FAILED" "exit 3"
             "go: cannot find module" (agent-repl--logfile-path)))))

(ert-deftest agent-repl-test-daemon-build-failure-preserves-the-exit-status ()
  "The signalled error still carries the subprocess exit status."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-run-build-script)
             (agent-repl-test--seed-build-output "boom\n"))
            ((symbol-function 'display-buffer) #'ignore)
            ((symbol-function 'agent-repl--emit-message) #'ignore)
            ((symbol-function 'agent-repl--persist-log-record) #'ignore))
    ;; Act
    (let ((detail (should-error (agent-repl--frontend-build-if-stale nil))))
      ;; Assert
      (should (string-match-p "exit 3" (cadr detail))))))

(ert-deftest agent-repl-test-daemon-build-success-echoes-a-completion-phase ()
  "A successful build reports that it finished, not only that it started."
  ;; Arrange
  (agent-repl-test--with-phase-echo lines
    (cl-letf (((symbol-function 'agent-repl--frontend-run-build-script)
               (lambda (_args) 0)))
      ;; Act
      (agent-repl--frontend-build-if-stale nil))
    ;; Assert
    (should (agent-repl-test--phase-line-p
             lines "agent-repl: shim/webapp/daemon built"))))

(ert-deftest agent-repl-test-daemon-deploy-stack-failure-echoes-its-phase ()
  "A failed whole-stack deploy names itself rather than the narrower build."
  ;; Arrange
  (agent-repl-test--with-phase-echo lines
    (agent-repl-test--with-async-run
     (cl-letf (((symbol-function 'agent-repl--warn) (lambda (&rest _) nil)))
       ;; Act
       (agent-repl--frontend-deploy-stack-async nil)
       (with-current-buffer (get-buffer-create agent-repl--frontend-build-buffer)
         (insert "protoc: not found\n"))
       (agent-repl-test--settle-async-run 3)))
    ;; Assert
    (should (agent-repl-test--phase-line-p
             lines "stack deploy FAILED" "protoc: not found"))))

(ert-deftest agent-repl-test-daemon-deploy-restart-echoes-its-origin ()
  "A restart driven over emacsclient names itself before Emacs blocks."
  ;; Arrange
  (agent-repl-test--with-phase-echo lines
    (cl-letf (((symbol-function 'agent-repl-runtime-restart-await)
               (lambda (&rest _) "runtime-restart-complete")))
      ;; Act
      (agent-repl-frontend-daemon-restart-await))
    ;; Assert
    (should (agent-repl-test--phase-line-p lines "deploy-driven restart requested"))))

(ert-deftest agent-repl-test-daemon-filter-records-process-output ()
  "Daemon stdout/stderr reaches the structured log, not only its buffer."
  ;; Arrange
  (let (records)
    (cl-letf (((symbol-function 'agent-repl--persist-log-record)
               (lambda (_ws _level _verbosity fmt args)
                 (push (apply #'format fmt args) records))))
      ;; Act — a nil process buffer exercises the log path alone.
      (cl-letf (((symbol-function 'process-buffer) (lambda (_proc) nil)))
        (agent-repl--frontend-daemon-filter 'proc "listen tcp: address in use\n"))
      ;; Assert
      (should (equal records
                     '("claude-repld output: listen tcp: address in use"))))))

(defmacro agent-repl-test--with-daemon-mirror (records &rest body)
  "Run BODY with the daemon log mirror accumulating into RECORDS, in order.
RECORDS is in scope for BODY's assertions.  The capture buffer is taken
out of the picture so the log path is exercised alone, and the line
accumulator starts empty."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,records nil)
         (agent-repl--frontend-daemon-line-accumulator ""))
     (cl-letf (((symbol-function 'agent-repl--persist-log-record)
                (lambda (_ws _level _verbosity fmt args)
                  (setq ,records
                        (append ,records (list (apply #'format fmt args))))))
               ((symbol-function 'process-buffer) (lambda (_proc) nil)))
       ,@body)))

(ert-deftest agent-repl-test-daemon-filter-holds-a-partial-line ()
  "A chunk ending mid-line logs nothing until its newline arrives."
  ;; Arrange
  (agent-repl-test--with-daemon-mirror records
    ;; Act
    (agent-repl--frontend-daemon-filter 'proc "panic: turn accounting names")
    ;; Assert
    (should-not records)))

(ert-deftest agent-repl-test-daemon-filter-rejoins-a-split-line ()
  "A record split across two chunks is mirrored as ONE line, not two fragments."
  ;; Arrange
  (agent-repl-test--with-daemon-mirror records
    ;; Act
    (agent-repl--frontend-daemon-filter 'proc "panic: turn accounting")
    (agent-repl--frontend-daemon-filter 'proc " names unknown turn\n")
    ;; Assert
    (should (equal records
                   '("claude-repld output: panic: turn accounting names unknown turn")))))

(ert-deftest agent-repl-test-daemon-filter-skips-the-daemons-own-record ()
  "A record the daemon already wrote to claude-repld.log is not mirrored again."
  ;; Arrange
  (agent-repl-test--with-daemon-mirror records
    ;; Act
    (agent-repl--frontend-daemon-filter
     'proc "{\"timestamp\":\"2026-08-07T15:53:38-04:00\",\"runtime\":\"daemon\",\"message\":\"x\"}\n")
    ;; Assert
    (should-not records)))

(ert-deftest agent-repl-test-daemon-filter-mirrors-a-relayed-sidecar-record ()
  "A sidecar record has no other durable home, so the mirror must keep it."
  ;; Arrange
  (agent-repl-test--with-daemon-mirror records
    ;; Act
    (agent-repl--frontend-daemon-filter
     'proc "{\"timestamp\":\"2026-08-07T15:53:38-04:00\",\"runtime\":\"sidecar\",\"message\":\"x\"}\n")
    ;; Assert
    (should (= (length records) 1))))

(ert-deftest agent-repl-test-daemon-filter-mirrors-a-relayed-webapp-record ()
  "A webapp record has no other durable home either."
  ;; Arrange
  (agent-repl-test--with-daemon-mirror records
    ;; Act
    (agent-repl--frontend-daemon-filter
     'proc "{\"timestamp\":\"2026-08-07T15:53:38-04:00\",\"runtime\":\"webapp\",\"message\":\"x\"}\n")
    ;; Assert
    (should (= (length records) 1))))

(ert-deftest agent-repl-test-daemon-filter-mirrors-a-quoted-daemon-tag ()
  "A relayed record QUOTING the daemon's runtime tag is still mirrored."
  ;; Arrange — the anchor is what keeps this from being read as a daemon record.
  (agent-repl-test--with-daemon-mirror records
    ;; Act
    (agent-repl--frontend-daemon-filter
     'proc (concat "{\"timestamp\":\"2026-08-07T15:53:38-04:00\",\"runtime\":\"sidecar\","
                   "\"message\":\"saw \\\"runtime\\\":\\\"daemon\\\" upstream\"}\n"))
    ;; Assert
    (should (= (length records) 1))))

(ert-deftest agent-repl-test-daemon-filter-drops-a-blank-line ()
  "Blank separators between records are not mirrored as empty log lines."
  ;; Arrange
  (agent-repl-test--with-daemon-mirror records
    ;; Act
    (agent-repl--frontend-daemon-filter 'proc "\n   \n")
    ;; Assert
    (should-not records)))

(ert-deftest agent-repl-test-daemon-flush-partial-line-surfaces-a-dying-line ()
  "A daemon that dies mid-line still gets its last line into the log."
  ;; Arrange
  (agent-repl-test--with-daemon-mirror records
    (agent-repl--frontend-daemon-filter 'proc "fatal error: out of memory")
    ;; Act
    (agent-repl--frontend-daemon-flush-partial-line)
    ;; Assert
    (should (equal records '("claude-repld output: fatal error: out of memory")))))

(ert-deftest agent-repl-test-daemon-flush-partial-line-keeps-a-dying-daemon-record ()
  "The dying line is flushed even when it LOOKS like the daemon's own record.
Unfinished on stdout means very likely unfinished in its own log too, so the
duplication argument that skips complete daemon records does not apply."
  ;; Arrange
  (agent-repl-test--with-daemon-mirror records
    (agent-repl--frontend-daemon-filter
     'proc "{\"timestamp\":\"2026-08-07T15:53:38-04:00\",\"runtime\":\"daemon\",\"messa")
    ;; Act
    (agent-repl--frontend-daemon-flush-partial-line)
    ;; Assert
    (should (= (length records) 1))))

(ert-deftest agent-repl-test-daemon-flush-partial-line-is-idempotent ()
  "A second flush with nothing held logs nothing."
  ;; Arrange
  (agent-repl-test--with-daemon-mirror records
    (agent-repl--frontend-daemon-filter 'proc "fatal error\n")
    ;; Act
    (agent-repl--frontend-daemon-flush-partial-line)
    ;; Assert
    (should (= (length records) 1))))

(ert-deftest agent-repl-test-daemon-filter-preserves-buffer-capture ()
  "Routing output to the log must not cost the capture buffer its content."
  ;; Arrange
  (let ((buffer (generate-new-buffer " *agent-repl-test-daemon-out*")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--persist-log-record) #'ignore)
                  ((symbol-function 'process-buffer) (lambda (_proc) buffer))
                  ((symbol-function 'process-mark)
                   (lambda (_proc) (with-current-buffer buffer (point-max-marker)))))
          ;; Act
          (agent-repl--frontend-daemon-filter 'proc "refusing to start\n")
          ;; Assert
          (should (equal (with-current-buffer buffer (buffer-string))
                         "refusing to start\n")))
      (kill-buffer buffer))))

;;;; ---- the bounded capture buffer -------------------------------------------

(defmacro agent-repl-test--with-daemon-capture (buffer &rest body)
  "Run BODY with BUFFER bound to a fresh capture buffer wired to `proc'.
The log mirror is silenced and the line accumulator starts empty, so
only the buffer side of the filter is under test."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,buffer (generate-new-buffer " *agent-repl-test-daemon-capture*"))
         (agent-repl--frontend-daemon-line-accumulator ""))
     (unwind-protect
         (cl-letf (((symbol-function 'agent-repl--persist-log-record) #'ignore)
                   ((symbol-function 'process-buffer) (lambda (_proc) ,buffer))
                   ((symbol-function 'process-mark)
                    (lambda (_proc)
                      (with-current-buffer ,buffer (point-max-marker)))))
           ,@body)
       (kill-buffer ,buffer))))

(defun agent-repl-test--daemon-feed-past-cap (chunk)
  "Feed CHUNK to the daemon filter until well past the capture cap."
  (dotimes (_ (1+ (/ (* 3 agent-repl--frontend-daemon-buffer-max-chars)
                     (length chunk))))
    (agent-repl--frontend-daemon-filter 'proc chunk)))

(ert-deftest agent-repl-test-daemon-filter-caps-the-capture-buffer ()
  "Output past the cap is trimmed away instead of accumulating forever."
  ;; Arrange
  (agent-repl-test--with-daemon-capture buffer
    ;; Act — three times the cap, delivered as whole lines.
    (agent-repl-test--daemon-feed-past-cap (concat (make-string 8000 ?x) "\n"))
    ;; Assert
    (should (<= (buffer-size buffer)
                agent-repl--frontend-daemon-buffer-max-chars))))

(ert-deftest agent-repl-test-daemon-filter-trim-starts-on-a-whole-line ()
  "The retained region begins at a line boundary, never mid-record."
  ;; Arrange
  (agent-repl-test--with-daemon-capture buffer
    ;; Act
    (agent-repl-test--daemon-feed-past-cap (concat (make-string 8000 ?x) "\n"))
    ;; Assert — a fragment would be shorter than a whole record.
    (should (equal (with-current-buffer buffer
                     (save-excursion
                       (goto-char (point-min))
                       (buffer-substring-no-properties
                        (point-min) (line-end-position))))
                   (make-string 8000 ?x)))))

(ert-deftest agent-repl-test-daemon-filter-trim-keeps-the-newest-output ()
  "Trimming discards the OLDEST output, so the dying words always survive."
  ;; Arrange
  (agent-repl-test--with-daemon-capture buffer
    (agent-repl-test--daemon-feed-past-cap (concat (make-string 8000 ?x) "\n"))
    ;; Act
    (agent-repl--frontend-daemon-filter 'proc "panic: the last words\n")
    ;; Assert
    (should (string-suffix-p "panic: the last words\n"
                             (with-current-buffer buffer (buffer-string))))))

(ert-deftest agent-repl-test-daemon-filter-trim-keeps-a-single-overlong-line ()
  "A line longer than the whole cap is cut mid-line rather than erased."
  ;; Arrange — no newline anywhere, so no line boundary exists to cut at.
  (agent-repl-test--with-daemon-capture buffer
    ;; Act
    (agent-repl-test--daemon-feed-past-cap (make-string 100000 ?y))
    ;; Assert
    (should (= (buffer-size buffer)
               agent-repl--frontend-daemon-buffer-max-chars))))

(ert-deftest agent-repl-test-daemon-trim-reanchors-a-stranded-process-mark ()
  "A process mark left inside the trimmed span is moved back to the tail."
  ;; Arrange
  (let ((buffer (generate-new-buffer " *agent-repl-test-daemon-mark*")))
    (unwind-protect
        (with-current-buffer buffer
          (insert (make-string (* 2 agent-repl--frontend-daemon-buffer-max-chars) ?z))
          (let ((mark (copy-marker (point-min))))
            (cl-letf (((symbol-function 'processp) (lambda (_proc) t))
                      ((symbol-function 'process-mark) (lambda (_proc) mark)))
              ;; Act
              (agent-repl--frontend-daemon-trim-capture 'proc)
              ;; Assert
              (should (= (marker-position mark) (point-max))))))
      (kill-buffer buffer))))

(ert-deftest agent-repl-test-daemon-trim-leaves-a-small-capture-alone ()
  "A capture inside the cap keeps every byte, trimming nothing."
  ;; Arrange
  (agent-repl-test--with-daemon-capture buffer
    ;; Act
    (agent-repl--frontend-daemon-filter 'proc "refusing to start\n")
    ;; Assert
    (should (equal (with-current-buffer buffer (buffer-string))
                   "refusing to start\n"))))

;;;; ---- the bounded at-exit consumption --------------------------------------

(defmacro agent-repl-test--with-daemon-capture-content (content &rest body)
  "Run BODY with the real capture buffer holding CONTENT.
Echoing is ON for the extent: the in-Emacs capture buffer only carries
the daemon's output when the filter that fills it is attached."
  (declare (indent 1))
  `(let ((agent-repl-daemon-echo-output t))
     (with-current-buffer (get-buffer-create agent-repl--frontend-daemon-buffer)
       (erase-buffer)
       (insert ,content))
     (unwind-protect (progn ,@body)
       (with-current-buffer agent-repl--frontend-daemon-buffer (erase-buffer)))))

(ert-deftest agent-repl-test-daemon-output-bounds-a-huge-capture ()
  "The at-exit read never materializes more than its tail bound."
  ;; Arrange — larger than the buffer cap, as a pre-cap buffer could be.
  (agent-repl-test--with-daemon-capture-content
      (concat (make-string (* 4 1024 1024) ?x) "\n")
    ;; Act
    (let ((output (agent-repl--frontend-daemon-output)))
      ;; Assert
      (should (<= (length output)
                  agent-repl--frontend-daemon-output-tail-chars)))))

(ert-deftest agent-repl-test-daemon-output-keeps-the-final-lines ()
  "The bounded read returns the END of the capture, which is the evidence."
  ;; Arrange
  (agent-repl-test--with-daemon-capture-content
      (concat (make-string (* 2 1024 1024) ?x) "\npanic: nil map write\n")
    ;; Act
    (let ((output (agent-repl--frontend-daemon-output)))
      ;; Assert
      (should (string-suffix-p "panic: nil map write" output)))))

(ert-deftest agent-repl-test-daemon-output-is-empty-without-a-capture-buffer ()
  "A capture buffer that never existed reads as an empty capture."
  ;; Arrange
  (let ((agent-repl-daemon-echo-output t))
    (when (get-buffer agent-repl--frontend-daemon-buffer)
      (kill-buffer agent-repl--frontend-daemon-buffer))
    ;; Act / Assert
    (should (equal (agent-repl--frontend-daemon-output) ""))))

;;;; ---- the file-backed output sink -----------------------------------------

(ert-deftest agent-repl-test-daemon-spawn-command-redirects-into-the-sink ()
  "With echoing off the spawn argv redirects both streams into the sink file."
  ;; Arrange
  (let ((agent-repl-daemon-echo-output nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-daemon-command)
               (lambda () (list "/bin/claude-repld" "-addr" "127.0.0.1:8787")))
              ((symbol-function 'agent-repl--frontend-daemon-output-sink-path)
               (lambda () "/state/claude-repld-output.log")))
      ;; Act
      (let ((cmd (agent-repl--frontend-daemon-spawn-command)))
        ;; Assert
        (should (member "exec \"$@\" >>/state/claude-repld-output.log 2>&1"
                        cmd))))))

(ert-deftest agent-repl-test-daemon-spawn-command-keeps-the-daemon-argv-intact ()
  "The redirect wrapper passes the daemon argv through as parameters."
  ;; Arrange
  (let ((agent-repl-daemon-echo-output nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-daemon-command)
               (lambda () (list "/bin/claude-repld" "-addr" "127.0.0.1:8787")))
              ((symbol-function 'agent-repl--frontend-daemon-output-sink-path)
               (lambda () "/state/claude-repld-output.log")))
      ;; Act
      (let ((cmd (agent-repl--frontend-daemon-spawn-command)))
        ;; Assert
        (should (equal (last cmd 3)
                       (list "/bin/claude-repld" "-addr" "127.0.0.1:8787")))))))

(ert-deftest agent-repl-test-daemon-spawn-command-is-bare-argv-when-echoing ()
  "The escape hatch launches the daemon directly, with no shell wrapper."
  ;; Arrange
  (let ((agent-repl-daemon-echo-output t))
    (cl-letf (((symbol-function 'agent-repl--frontend-daemon-command)
               (lambda () (list "/bin/claude-repld" "-addr" "127.0.0.1:8787"))))
      ;; Act
      (let ((cmd (agent-repl--frontend-daemon-spawn-command)))
        ;; Assert
        (should (equal cmd (list "/bin/claude-repld" "-addr" "127.0.0.1:8787")))))))

(ert-deftest agent-repl-test-daemon-spawn-attaches-no-filter-by-default ()
  "The firehose never enters elisp: no filter is attached by default."
  ;; Arrange
  (let ((agent-repl-daemon-echo-output nil))
    ;; Act / Assert
    (should (null (agent-repl--frontend-daemon-spawn-filter)))))

(ert-deftest agent-repl-test-daemon-spawn-attaches-the-filter-when-echoing ()
  "The escape hatch restores the mirroring filter."
  ;; Arrange
  (let ((agent-repl-daemon-echo-output t))
    ;; Act / Assert
    (should (eq (agent-repl--frontend-daemon-spawn-filter)
                #'agent-repl--frontend-daemon-filter))))

(ert-deftest agent-repl-test-daemon-spawn-attaches-no-buffer-by-default ()
  "No capture buffer is attached when the stream is redirected at the kernel."
  ;; Arrange
  (let ((agent-repl-daemon-echo-output nil))
    ;; Act / Assert
    (should (null (agent-repl--frontend-daemon-spawn-buffer)))))

(ert-deftest agent-repl-test-daemon-spawn-attaches-the-buffer-when-echoing ()
  "The escape hatch restores the in-Emacs capture buffer."
  ;; Arrange
  (let ((agent-repl-daemon-echo-output t))
    ;; Act / Assert
    (should (equal (agent-repl--frontend-daemon-spawn-buffer)
                   agent-repl--frontend-daemon-buffer))))

(ert-deftest agent-repl-test-daemon-output-reads-the-sink-by-default ()
  "The at-exit capture comes from the sink file when echoing is off."
  ;; Arrange
  (let ((agent-repl-daemon-echo-output nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-read-daemon-output-sink)
               (lambda (_path _chars) "panic: nil map write\n")))
      ;; Act / Assert
      (should (equal (agent-repl--frontend-daemon-output)
                     "panic: nil map write")))))

(ert-deftest agent-repl-test-daemon-output-bounds-the-sink-read ()
  "The sink read is bounded by the same tail cap the buffer read uses."
  ;; Arrange
  (let ((agent-repl-daemon-echo-output nil)
        (requested nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-read-daemon-output-sink)
               (lambda (_path chars) (setq requested chars) "")))
      ;; Act
      (agent-repl--frontend-daemon-output)
      ;; Assert
      (should (equal requested agent-repl--frontend-daemon-output-tail-chars)))))

(ert-deftest agent-repl-test-daemon-output-reads-the-sink-path ()
  "The at-exit capture reads the sink the spawn redirects into."
  ;; Arrange
  (let ((agent-repl-daemon-echo-output nil)
        (requested nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-read-daemon-output-sink)
               (lambda (path _chars) (setq requested path) "")))
      ;; Act
      (agent-repl--frontend-daemon-output)
      ;; Assert
      (should (equal requested (agent-repl--frontend-daemon-output-sink-path))))))

(ert-deftest agent-repl-test-daemon-exit-record-carries-the-sink-tail ()
  "A goroutine dump in the sink file rides the exit record."
  ;; Arrange
  (let ((agent-repl-daemon-echo-output nil)
        records)
    (cl-letf (((symbol-function 'agent-repl--frontend-read-daemon-output-sink)
               (lambda (_path _chars) "SIGQUIT: quit\ngoroutine 1 [running]:\n"))
              ((symbol-function 'agent-repl--persist-log-record)
               (lambda (_ws _level _verbosity fmt args)
                 (push (apply #'format fmt args) records)))
              ((symbol-function 'agent-repl--emit-message) #'ignore)
              ((symbol-function 'process-live-p) (lambda (_proc) nil))
              ((symbol-function 'agent-repl-failure-surface) #'ignore))
      ;; Act
      (agent-repl--frontend-daemon-sentinel 'proc "exited abnormally with code 2\n"))
    ;; Assert
    (should (seq-find (lambda (r)
                        (and (string-prefix-p "claude-repld exited:" r)
                             (string-match-p "goroutine 1 \\[running\\]:" r)))
                      records))))

(ert-deftest agent-repl-test-daemon-terminal-output-path-is-the-sink-by-default ()
  "The exit phase points readers at the sink file when echoing is off."
  ;; Arrange
  (let ((agent-repl-daemon-echo-output nil))
    ;; Act / Assert
    (should (equal (agent-repl--frontend-daemon-terminal-output-path)
                   (agent-repl--frontend-daemon-output-sink-path)))))

(ert-deftest agent-repl-test-daemon-terminal-output-path-is-the-log-when-echoing ()
  "The exit phase points readers at agent-repl's log when echoing mirrors there."
  ;; Arrange
  (let ((agent-repl-daemon-echo-output t))
    ;; Act / Assert
    (should (equal (agent-repl--frontend-daemon-terminal-output-path)
                   (agent-repl--logfile-path)))))

(ert-deftest agent-repl-test-daemon-exit-record-omits-the-full-capture ()
  "A multi-megabyte capture never reaches the exit log record whole."
  ;; Arrange
  (let (records)
    (agent-repl-test--with-daemon-capture-content
        (concat (make-string (* 3 1024 1024) ?x) "\npanic: nil map write\n")
      (cl-letf (((symbol-function 'agent-repl--persist-log-record)
                 (lambda (_ws _level _verbosity fmt args)
                   (push (apply #'format fmt args) records)))
                ((symbol-function 'agent-repl--emit-message) #'ignore)
                ((symbol-function 'process-live-p) (lambda (_proc) nil))
                ((symbol-function 'agent-repl-failure-surface) #'ignore))
        ;; Act
        (agent-repl--frontend-daemon-sentinel 'proc "exited abnormally with code 2\n")))
    ;; Assert
    (let ((exit (car (seq-filter
                      (lambda (r) (string-prefix-p "claude-repld exited:" r))
                      records))))
      (should exit)
      (should (< (length exit) (* 64 1024))))))

(ert-deftest agent-repl-test-daemon-exit-record-carries-the-output-tail ()
  "The bounded tail still reaches the exit record, so the exit stays loud."
  ;; Arrange
  (let (records)
    (agent-repl-test--with-daemon-capture-content
        (concat (make-string (* 3 1024 1024) ?x) "\npanic: nil map write\n")
      (cl-letf (((symbol-function 'agent-repl--persist-log-record)
                 (lambda (_ws _level _verbosity fmt args)
                   (push (apply #'format fmt args) records)))
                ((symbol-function 'agent-repl--emit-message) #'ignore)
                ((symbol-function 'process-live-p) (lambda (_proc) nil))
                ((symbol-function 'agent-repl-failure-surface) #'ignore))
        ;; Act
        (agent-repl--frontend-daemon-sentinel 'proc "exited abnormally with code 2\n")))
    ;; Assert
    (should (seq-find (lambda (r)
                        (and (string-prefix-p "claude-repld exited:" r)
                             (string-match-p "panic: nil map write" r)))
                      records))))

(ert-deftest agent-repl-test-daemon-exit-record-names-the-durable-log ()
  "What the tail leaves out is reachable: the record names claude-repld.log."
  ;; Arrange
  (let (records)
    (agent-repl-test--with-daemon-capture-content "panic: nil map write\n"
      (cl-letf (((symbol-function 'agent-repl--persist-log-record)
                 (lambda (_ws _level _verbosity fmt args)
                   (push (apply #'format fmt args) records)))
                ((symbol-function 'agent-repl--emit-message) #'ignore)
                ((symbol-function 'process-live-p) (lambda (_proc) nil))
                ((symbol-function 'agent-repl-failure-surface) #'ignore))
        ;; Act
        (agent-repl--frontend-daemon-sentinel 'proc "exited abnormally with code 2\n")))
    ;; Assert
    (should (seq-find (lambda (r)
                        (and (string-prefix-p "claude-repld exited:" r)
                             (string-match-p
                              (regexp-quote (agent-repl--frontend-daemon-log-path)) r)))
                      records))))

(ert-deftest agent-repl-test-daemon-log-path-sits-under-the-state-root ()
  "The durable daemon log is named where the daemon actually writes it."
  ;; Arrange / Act
  (let ((path (agent-repl--frontend-daemon-log-path)))
    ;; Assert
    (should (equal path (agent-repl--global-state-file "claude-repld.log")))))

(ert-deftest agent-repl-test-daemon-exit-carries-the-captured-output ()
  "The daemon's dying words ride its failure card, not only its exit event."
  ;; Arrange
  (let ((agent-repl-daemon-echo-output nil)
        surfaced)
    (cl-letf (((symbol-function 'agent-repl--frontend-read-daemon-output-sink)
               (lambda (_path _chars) "claude-repld: -accounts is malformed\n"))
              ((symbol-function 'agent-repl--persist-log-record) #'ignore)
              ((symbol-function 'agent-repl--emit-message) #'ignore)
              ((symbol-function 'process-live-p) (lambda (_proc) nil))
              ((symbol-function 'agent-repl-failure-surface)
               (lambda (_ws failure) (setq surfaced failure))))
      ;; Act
      (agent-repl--frontend-daemon-sentinel 'proc "exited abnormally with code 1\n")
      ;; Assert
      (should (string-match-p "-accounts is malformed" (format "%S" surfaced))))))

;;;; ---- the expected-restart window -----------------------------------------

(defmacro agent-repl-test--with-expected-restart (records &rest body)
  "Run BODY over a clean expected-restart window, collecting log RECORDS.
Each collected record is (LEVEL FMT ARGS).  The window's expiry timer is
captured rather than armed and the echo area is silenced, so nothing here
escapes the test."
  (declare (indent 1))
  `(let ((agent-repl--frontend-expected-restart nil)
         (agent-repl--frontend-expected-restart-last-close nil)
         (agent-repl-frontend-expected-restart-window-seconds 180.0)
         (,records nil))
     (with-current-buffer (get-buffer-create agent-repl--frontend-daemon-buffer)
       (erase-buffer))
     (cl-letf (((symbol-function 'agent-repl--frontend-read-daemon-output-sink)
                (lambda (&rest _) ""))
               ((symbol-function 'agent-repl--uds-run-timer)
                (lambda (_delay _fn) 'agent-repl-test--fake-timer))
               ((symbol-function 'agent-repl--emit-message) #'ignore)
               ((symbol-function 'agent-repl--persist-log-record)
                (lambda (_ws level _verbosity fmt args)
                  (push (list level fmt args) ,records))))
       ,@body)))

(defun agent-repl-test--records-matching (records level substring)
  "Return the RECORDS at LEVEL whose format string contains SUBSTRING."
  (cl-remove-if-not
   (lambda (record)
     (and (equal (nth 0 record) level)
          (stringp (nth 1 record))
          (string-match-p (regexp-quote substring) (nth 1 record))))
   records))

(ert-deftest agent-repl-test-daemon-expected-restart-exit-opens-no-card ()
  "An exit inside an armed window opens no failure card."
  ;; Arrange
  (agent-repl-test--with-expected-restart records
    (let (surfaced)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
                ((symbol-function 'agent-repl-failure-surface)
                 (lambda (_ws failure) (setq surfaced failure))))
        (agent-repl--frontend-arm-expected-restart "deploy (emacsclient)")
        ;; Act
        (agent-repl--frontend-daemon-sentinel 'proc "killed: 9\n")
        ;; Assert
        (should-not surfaced))
      (ignore records))))

(ert-deftest agent-repl-test-daemon-expected-restart-exit-emits-no-warn ()
  "An exit inside an armed window records no warning about the daemon."
  ;; Arrange
  (agent-repl-test--with-expected-restart records
    (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
              ((symbol-function 'agent-repl-failure-surface) #'ignore))
      (agent-repl--frontend-arm-expected-restart "deploy (emacsclient)")
      ;; Act
      (agent-repl--frontend-daemon-sentinel 'proc "killed: 9\n"))
    ;; Assert
    (should-not (agent-repl-test--records-matching records "warn" ""))))

(ert-deftest agent-repl-test-daemon-expected-restart-exit-records-the-initiator ()
  "The suppressed exit is recorded at info, naming who ordered the restart."
  ;; Arrange
  (agent-repl-test--with-expected-restart records
    (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
              ((symbol-function 'agent-repl-failure-surface) #'ignore))
      (agent-repl--frontend-arm-expected-restart "deploy (emacsclient)")
      ;; Act
      (agent-repl--frontend-daemon-sentinel 'proc "killed: 9\n"))
    ;; Assert
    (let ((info (agent-repl-test--records-matching
                 records "info" "exited inside the expected-restart window")))
      (should (= (length info) 1))
      (should (member "deploy (emacsclient)" (nth 2 (car info)))))))

(ert-deftest agent-repl-test-daemon-expected-restart-expiry-surfaces-the-withheld-exit ()
  "A window that expires with no replacement surfaces the exit it withheld."
  ;; Arrange
  (agent-repl-test--with-expected-restart records
    (let (surfaced)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
                ((symbol-function 'agent-repl-failure-surface)
                 (lambda (_ws failure) (setq surfaced failure))))
        (agent-repl--frontend-arm-expected-restart "deploy (emacsclient)")
        (agent-repl--frontend-daemon-sentinel 'proc "killed: 9\n")
        ;; Act
        (agent-repl--frontend-expected-restart-expire)
        ;; Assert
        (should (equal (plist-get surfaced :type) "client.daemon_exited")))
      (ignore records))))

(ert-deftest agent-repl-test-daemon-expected-restart-expiry-warns ()
  "The expiry that surfaces a withheld exit also warns about the window."
  ;; Arrange
  (agent-repl-test--with-expected-restart records
    (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
              ((symbol-function 'agent-repl-failure-surface) #'ignore))
      (agent-repl--frontend-arm-expected-restart "deploy (emacsclient)")
      (agent-repl--frontend-daemon-sentinel 'proc "killed: 9\n")
      ;; Act
      (agent-repl--frontend-expected-restart-expire))
    ;; Assert
    (should (agent-repl-test--records-matching
             records "warn" "window EXPIRED with no replacement daemon"))))

(ert-deftest agent-repl-test-daemon-exit-without-a-window-still-opens-a-card ()
  "With no window armed the daemon exit surfaces exactly as it always has."
  ;; Arrange
  (agent-repl-test--with-expected-restart records
    (let (surfaced)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
                ((symbol-function 'agent-repl-failure-surface)
                 (lambda (_ws failure) (setq surfaced failure))))
        ;; Act
        (agent-repl--frontend-daemon-sentinel 'proc "killed: 9\n")
        ;; Assert
        (should (equal (plist-get surfaced :type) "client.daemon_exited")))
      (ignore records))))

(ert-deftest agent-repl-test-daemon-expected-restart-closes-on-reconnect ()
  "The replacement daemon's link closes the window and drops the withheld exit."
  ;; Arrange
  (agent-repl-test--with-expected-restart records
    (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
              ((symbol-function 'agent-repl-failure-surface) #'ignore))
      (agent-repl--frontend-arm-expected-restart "deploy (emacsclient)")
      (agent-repl--frontend-daemon-sentinel 'proc "killed: 9\n")
      ;; Act
      (agent-repl--frontend-expected-restart-note-reconnect)
      ;; Assert
      (should-not agent-repl--frontend-expected-restart))
    (ignore records)))

(ert-deftest agent-repl-test-daemon-covering-initiator-covers-any-moment-while-live ()
  "A LIVE window covers whatever moment it is asked about."
  ;; Arrange
  (agent-repl-test--with-expected-restart records
    (agent-repl--frontend-arm-expected-restart "deploy (emacsclient)")
    ;; Act / Assert
    (should (equal (agent-repl--frontend-expected-restart-covering-initiator
                    (- (float-time) 3600.0))
                   "deploy (emacsclient)"))
    (ignore records)))

(ert-deftest agent-repl-test-daemon-covering-initiator-needs-a-moment-once-closed ()
  "With no live window, `nil' AS-OF asks about NOW, which no closed window covers."
  ;; Arrange
  (agent-repl-test--with-expected-restart records
    (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
              ((symbol-function 'agent-repl-failure-surface) #'ignore))
      (agent-repl--frontend-arm-expected-restart "deploy (emacsclient)")
      (agent-repl--frontend-daemon-sentinel 'proc "killed: 9\n")
      (agent-repl--frontend-expected-restart-note-reconnect))
    ;; Act / Assert
    (should-not (agent-repl--frontend-expected-restart-covering-initiator))
    (ignore records)))

(ert-deftest agent-repl-test-daemon-covering-initiator-covers-a-closed-windows-span ()
  "A reconnect-closed window still covers the moments inside its own lifetime."
  ;; Arrange
  (agent-repl-test--with-expected-restart records
    (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
              ((symbol-function 'agent-repl-failure-surface) #'ignore))
      (agent-repl--frontend-arm-expected-restart "deploy (emacsclient)")
      (setq agent-repl--frontend-expected-restart
            (plist-put agent-repl--frontend-expected-restart
                       :armed-at (- (float-time) 20.0)))
      (agent-repl--frontend-daemon-sentinel 'proc "killed: 9\n")
      (agent-repl--frontend-expected-restart-note-reconnect))
    ;; Act / Assert
    (should (equal (agent-repl--frontend-expected-restart-covering-initiator
                    (- (float-time) 10.0))
                   "deploy (emacsclient)"))
    (ignore records)))

(ert-deftest agent-repl-test-daemon-covering-initiator-excludes-what-predates-the-window ()
  "A moment before the window was armed is outside it, so nothing covers it."
  ;; Arrange
  (agent-repl-test--with-expected-restart records
    (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
              ((symbol-function 'agent-repl-failure-surface) #'ignore))
      (agent-repl--frontend-arm-expected-restart "deploy (emacsclient)")
      (agent-repl--frontend-daemon-sentinel 'proc "killed: 9\n")
      (agent-repl--frontend-expected-restart-note-reconnect))
    ;; Act / Assert
    (should-not (agent-repl--frontend-expected-restart-covering-initiator
                 (- (float-time) 10.0)))
    (ignore records)))

(ert-deftest agent-repl-test-daemon-expiry-leaves-no-covering-window-behind ()
  "A window that EXPIRED describes an outage, so it grants nothing any grace."
  ;; Arrange
  (agent-repl-test--with-expected-restart records
    (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
              ((symbol-function 'agent-repl-failure-surface) #'ignore))
      (agent-repl--frontend-arm-expected-restart "deploy (emacsclient)")
      (setq agent-repl--frontend-expected-restart
            (plist-put agent-repl--frontend-expected-restart
                       :armed-at (- (float-time) 20.0)))
      (agent-repl--frontend-daemon-sentinel 'proc "killed: 9\n")
      ;; Act
      (agent-repl--frontend-expected-restart-expire))
    ;; Assert
    (should-not (agent-repl--frontend-expected-restart-covering-initiator
                 (- (float-time) 10.0)))
    (ignore records)))

(ert-deftest agent-repl-test-daemon-expected-restart-survives-a-preflight-connect ()
  "A link that opens before any exit leaves the window armed.
The coordinator dials the OUTGOING daemon during its readiness preflight,
so that connect must not be mistaken for the replacement's arrival."
  ;; Arrange
  (agent-repl-test--with-expected-restart records
    (agent-repl--frontend-arm-expected-restart "deploy (emacsclient)")
    ;; Act
    (agent-repl--frontend-expected-restart-note-reconnect)
    ;; Assert
    (should (equal (plist-get agent-repl--frontend-expected-restart :initiator)
                   "deploy (emacsclient)"))
    (ignore records)))

(ert-deftest agent-repl-test-daemon-expected-restart-refuses-a-blank-initiator ()
  "A window that cannot name who opened it is refused rather than armed."
  ;; Arrange / Act / Assert
  (agent-repl-test--with-expected-restart records
    (should-error (agent-repl--frontend-arm-expected-restart "   "))
    (ignore records)))

(ert-deftest agent-repl-test-daemon-expected-restart-elapsed-window-stops-suppressing ()
  "An elapsed window suppresses nothing even if its timer never fired.
A hot-reload cancels every module timer, so elapsed time — not the timer —
is what bounds the suppression."
  ;; Arrange
  (agent-repl-test--with-expected-restart records
    (let (surfaced)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
                ((symbol-function 'agent-repl-failure-surface)
                 (lambda (_ws failure) (setq surfaced failure))))
        (agent-repl--frontend-arm-expected-restart "deploy (emacsclient)")
        (setq agent-repl--frontend-expected-restart
              (plist-put agent-repl--frontend-expected-restart :armed-at
                         (- (float-time)
                            agent-repl-frontend-expected-restart-window-seconds
                            1.0)))
        ;; Act
        (agent-repl--frontend-daemon-sentinel 'proc "killed: 9\n")
        ;; Assert
        (should (equal (plist-get surfaced :type) "client.daemon_exited")))
      (ignore records))))

(ert-deftest agent-repl-test-daemon-stop-grace-covers-the-daemon-drain ()
  "The SIGTERM grace must outlast the drain it exists to allow.
Two consecutive deploys SIGKILLed the daemon at the old 3.0s budget, which
skips the drain that reconstructs merges and releases leases."
  ;; Arrange / Act / Assert
  (should (>= agent-repl-frontend-stop-grace-seconds 20.0)))

(provide 'test-daemon)

;;; test-daemon.el ends here

;;;; ---- ensure: the launch waits for the deploy -----------------------------
;;
;; The deploy no longer blocks, so "the daemon is running" is a fact that
;; arrives later.  These pin that the launch and the caller's continuation
;; both wait for it, and that a failed deploy never launches against stale
;; code.

(ert-deftest agent-repl-test-daemon-ensure-launches-only-after-the-deploy ()
  "The spawn happens on the deploy's success continuation, not beside it."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let (deploy-done spawned
         (fresh (agent-repl-test--make-live-daemon 5)))
     (cl-letf (((symbol-function 'agent-repl--frontend-daemon-responsive-async)
                (lambda (_open absent) (funcall absent 'no-listener)))
               ((symbol-function 'agent-repl--frontend-deploy-stack-async)
                (lambda (&optional _f on-success _on-failure)
                  (setq deploy-done on-success) 'started))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () (setq spawned t) fresh)))
       ;; Act
       (agent-repl--ensure-frontend-daemon)
       ;; Assert — nothing is launched while the deploy is still running.
       (should-not spawned)
       (funcall deploy-done)
       (should spawned)))))

(ert-deftest agent-repl-test-daemon-ensure-runs-its-continuation-after-launch ()
  "ON-ENSURED runs once the daemon this ensure is responsible for is up."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let (ensured (fresh (agent-repl-test--make-live-daemon 6)))
     (cl-letf (((symbol-function 'agent-repl--frontend-daemon-responsive-async)
                (lambda (_open absent) (funcall absent 'no-listener)))
               ((symbol-function 'agent-repl--frontend-deploy-stack-async)
                (lambda (&optional _f on-success _on-failure)
                  (funcall on-success) 'started))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () fresh)))
       ;; Act
       (agent-repl--ensure-frontend-daemon nil (lambda () (setq ensured t)) #'ignore)
       ;; Assert
       (should ensured)))))

(ert-deftest agent-repl-test-daemon-ensure-failed-deploy-never-launches ()
  "A failed deploy reports to ON-FAILURE and spawns nothing."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let (detail spawned)
     (cl-letf (((symbol-function 'agent-repl--frontend-daemon-responsive-async)
                (lambda (_open absent) (funcall absent 'no-listener)))
               ((symbol-function 'agent-repl--frontend-deploy-stack-async)
                (lambda (&optional _f _on-success on-failure)
                  (funcall on-failure "stack deploy failed (exit 1)") 'started))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () (setq spawned t) nil)))
       ;; Act
       (agent-repl--ensure-frontend-daemon
        nil #'ignore (lambda (d) (setq detail d)))
       ;; Assert
       (should-not spawned)
       (should (string-match-p "exit 1" detail))))))

(ert-deftest agent-repl-test-daemon-ensure-reports-a-failed-launch ()
  "A launch that signals reaches ON-FAILURE rather than dying in the sentinel."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let (detail)
     (cl-letf (((symbol-function 'agent-repl--warn) (lambda (&rest _) nil))
               ((symbol-function 'agent-repl--frontend-daemon-responsive-async)
                (lambda (_open absent) (funcall absent 'no-listener)))
               ((symbol-function 'agent-repl--frontend-deploy-stack-async)
                (lambda (&optional _f on-success _on-failure)
                  (funcall on-success) 'started))
               ((symbol-function 'agent-repl--frontend-start-daemon)
                (lambda () (error "daemon binary missing after build"))))
       ;; Act
       (agent-repl--ensure-frontend-daemon
        nil #'ignore (lambda (d) (setq detail d)))
       ;; Assert
       (should (string-match-p "daemon binary missing" detail))))))

(ert-deftest agent-repl-test-daemon-ensure-adoption-runs-the-continuation ()
  "An adopted foreign daemon IS the running daemon, so the waiter proceeds."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let (ensured deployed)
     (cl-letf (((symbol-function 'agent-repl--frontend-daemon-responsive-async)
                (lambda (open _absent) (funcall open)))
               ((symbol-function 'agent-repl--frontend-deploy-stack-async)
                (lambda (&rest _) (setq deployed t) 'started)))
       ;; Act
       (agent-repl--ensure-frontend-daemon nil (lambda () (setq ensured t)) #'ignore)
       ;; Assert
       (should ensured)
       (should-not deployed)))))

(ert-deftest agent-repl-test-daemon-ensure-declined-reports-the-shared-detail ()
  "An ensure declined by the auto-start gate reports the one shared detail."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((agent-repl-frontend-auto-start nil) detail)
     ;; Act
     (should-not (agent-repl--ensure-frontend-daemon
                  nil #'ignore (lambda (d) (setq detail d))))
     ;; Assert
     (should (equal detail agent-repl--frontend-daemon-not-started-detail)))))

(ert-deftest agent-repl-test-daemon-after-ensured-requires-continuations ()
  "The canonical ensure door refuses non-callable continuations."
  (should-error (agent-repl--frontend-after-daemon-ensured nil nil)))

;;;; ---- the script-presence assertion is one check --------------------------

(ert-deftest agent-repl-test-daemon-assert-script-signals-for-an-absent-path ()
  "The shared assertion signals, naming its subject."
  (let ((err (should-error
              (agent-repl--frontend-assert-script
               "/agent-repl-nonexistent/x.sh" "stack deploy"))))
    (should (string-match-p "stack deploy" (error-message-string err)))))

(ert-deftest agent-repl-test-daemon-assert-script-passes-for-a-present-path ()
  "A script that exists passes the assertion silently."
  (should-not (agent-repl--frontend-assert-script
               agent-repl--frontend-build-script "frontend build")))

(ert-deftest agent-repl-test-daemon-build-assertion-uses-the-shared-check ()
  "The build gate delegates to the shared assertion rather than its own copy."
  (let (asked)
    (cl-letf (((symbol-function 'agent-repl--frontend-assert-script)
               (lambda (path _subject) (setq asked path))))
      (agent-repl--frontend-build-assert-script)
      (should (equal asked agent-repl--frontend-build-script)))))

(ert-deftest agent-repl-test-daemon-deploy-assertion-uses-the-shared-check ()
  "The deploy gate delegates to the shared assertion rather than its own copy."
  (let (asked)
    (cl-letf (((symbol-function 'agent-repl--frontend-assert-script)
               (lambda (path _subject) (setq asked path))))
      (agent-repl--frontend-deploy-assert-script)
      (should (equal asked agent-repl--frontend-deploy-script)))))

;;;; ---- filter/sentinel: quit deferral ----------------------------------

(ert-deftest agent-repl-test-daemon-filter-defers-a-quit-to-the-command-loop ()
  "A C-g during a mirror leaves the quit armed rather than losing it."
  ;; Arrange
  (agent-repl-test--with-daemon-mirror records
    (cl-letf (((symbol-function 'agent-repl--frontend-daemon-capture)
               (lambda (&rest _) (setq quit-flag t))))
      ;; Act / Assert
      (should (agent-repl-test--quit-deferred-p
                (agent-repl--frontend-daemon-filter 'proc "panic: boom\n"))))))

(ert-deftest agent-repl-test-daemon-filter-completes-the-line-assembly-under-a-quit ()
  "A quit mid-mirror never strands a half-assembled line in the accumulator."
  ;; Arrange
  (agent-repl-test--with-daemon-mirror records
    ;; Act — the C-g lands while the chunk is being consumed.
    (agent-repl-test--with-pending-quit
      (agent-repl--frontend-daemon-filter 'proc "panic: boom\n"))
    ;; Assert
    (should (member "claude-repld output: panic: boom" records))
    (should (equal agent-repl--frontend-daemon-line-accumulator ""))))

(ert-deftest agent-repl-test-daemon-sentinel-defers-a-quit-to-the-command-loop ()
  "A C-g during the daemon's exit report is deferred, not taken mid-report."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-daemon-report-exit)
             (lambda (&rest _) (setq quit-flag t))))
    ;; Act / Assert
    (should (agent-repl-test--quit-deferred-p
              (agent-repl--frontend-daemon-sentinel 'proc "exited abnormally\n")))))

(ert-deftest agent-repl-test-async-run-sentinel-defers-a-quit-to-the-command-loop ()
  "A C-g while an async build settles cannot strand the single-flight slot."
  ;; Arrange
  (cl-letf (((symbol-function 'process-live-p) (lambda (_proc) nil))
            ((symbol-function 'process-exit-status) (lambda (_proc) 0))
            ((symbol-function 'agent-repl--frontend-async-run-settle)
             (lambda (&rest _) (setq quit-flag t))))
    ;; Act / Assert
    (should (agent-repl-test--quit-deferred-p
              (agent-repl--frontend-async-run-sentinel 'proc "finished\n")))))
