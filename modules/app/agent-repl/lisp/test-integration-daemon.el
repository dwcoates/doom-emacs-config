;;; test-integration-daemon.el --- Integration: daemon.el cold start -*- lexical-binding: t; -*-

;;; Commentary:

;; Scenario 14 of elisp-fanout.md §14: EMACS OWNS COLD START of the daemon,
;; and nothing after boot — everything later is the daemon's own blue-green.
;;
;; The four cases the ruling names:
;;   - no daemon.addr        → build, then start, then the link comes up;
;;   - a stale daemon.addr   → treated as ABSENT (an address nothing answers);
;;   - a build failure       → surfaced in a buffer with a WARNING, NO start;
;;   - a daemon that ANSWERS → adopted, with no build and no start, healthy or
;;     unhealthy alike.  Emacs NEVER kills a daemon that answers.
;;
;; The build and daemon commands are stub SHELL SCRIPTS written into the test's
;; own temp dir: cold start's contract is "run this script, then wait for
;; daemon.addr", and a stub that publishes an address exercises exactly that
;; without pulling the real daemon (another system) into the run.  Reaching
;; those boundaries for real is what `agent-repl-itest--with-cold-start' is
;; for; it also reaps the poll timer, the spawned daemon and the link a cold
;; start leaves behind, so no scenario inherits another's in-flight boot.

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "test-integration-helpers.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;; Production names this suite depends on (daemon.el, §11; daemon-link.el, §6).
(declare-function agent-repl-daemon-ensure "daemon")
(declare-function agent-repl-frontend-daemon-ensure "daemon")
(declare-function agent-repl-link-up-p "daemon-link")
(declare-function agent-repl-link-primary "daemon-link")
(defvar agent-repl-daemon-build-script)
(defvar agent-repl-daemon-command)
(defvar agent-repl-daemon-boot-timeout-seconds)
(defvar agent-repl-daemon-mode-line-segment)
(defvar agent-repl-link-up-functions)
(defvar agent-repl-link-no-daemon-functions)
(defvar agent-repl-link-reconnect-interval-seconds)

;;;; ---- Stub scripts ----

(defun agent-repl-itest-daemon--write-script (path body)
  "Write an executable shell script at PATH whose body is BODY."
  (with-temp-file path
    (insert "#!/bin/sh\n" body "\n"))
  (set-file-modes path #o755)
  path)

(defmacro agent-repl-itest-daemon--with-stubs (dir &rest body)
  "Run BODY with DIR bound to a fresh temp dir holding the stub scripts.
The stubs record their invocations by appending to files in DIR, which
is how a test proves a script ran — or, just as importantly, did not."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,dir (file-name-as-directory (make-temp-file "agent-repl-itest-boot-" t))))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-directory ,dir t)))))

(defun agent-repl-itest-daemon--ran-p (dir name)
  "Return non-nil when the stub NAME recorded a run in DIR."
  (file-exists-p (expand-file-name name dir)))

(defun agent-repl-itest-daemon--line-count (path)
  "Return the number of lines PATH holds, or 0 when it does not exist yet.
A stub script appends one line per invocation, so this is how a scenario
counts exactly how many times it ran."
  (if (not (file-exists-p path))
      0
    (with-temp-buffer
      (insert-file-contents path)
      (count-lines (point-min) (point-max)))))

;;;; ---- An answering daemon is adopted ----

(ert-deftest agent-repl-itest-daemon-answering-daemon-is-adopted ()
  "A daemon that answers DaemonHealth is ADOPTED: no build, no start.
Ruled at kickoff: Emacs adopts any daemon that answers and only builds
and starts one when daemon.addr is absent or answers nothing."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--with-cold-start
      (agent-repl-itest-daemon--with-stubs boot-dir
        (let* ((build (agent-repl-itest-daemon--write-script
                       (expand-file-name "build.sh" boot-dir)
                       (format "touch %sbuild-ran" boot-dir)))
               (start (agent-repl-itest-daemon--write-script
                       (expand-file-name "start.sh" boot-dir)
                       (format "touch %sstart-ran" boot-dir)))
               (agent-repl-daemon-build-script build)
               (agent-repl-daemon-command (list start))
               (agent-repl-link-up-functions nil)
               (agent-repl-link-no-daemon-functions nil))
          ;; Act.
          (agent-repl-daemon-ensure)
          (agent-repl-itest--await-call daemon "DaemonHealth")
          ;; Assert.
          (should-not (agent-repl-itest-daemon--ran-p boot-dir "build-ran"))
          (should-not (agent-repl-itest-daemon--ran-p boot-dir "start-ran"))
          ;; "adopt it ... `agent-repl-link-connect'" — adoption means the
          ;; link actually comes up, not merely that the probe was asked.
          (agent-repl-itest--wait-until (lambda () (agent-repl-link-up-p))
                                        nil "the link to come up")
          (should (agent-repl-link-up-p))
          ;; Adopting means standing the ONE `WatchDaemon' stream this link
          ;; holds, on the daemon that answered — not just answering the
          ;; probe and going no further.
          (agent-repl-itest--await-subscriber daemon "daemon")
          (should (agent-repl-itest--subscribers daemon "daemon")))))))

(ert-deftest agent-repl-itest-daemon-unhealthy-daemon-is-still-adopted ()
  "An UNHEALTHY daemon is still a daemon: adopted, never killed.
UNHEALTHY IS AN ANSWER; only a transport failure means there is no
daemon there."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let* ((fault '((detail . "store socket unreachable")
                    (wsmReadOnly . ())))
           (response `((success . ((unhealthy . ((faults . [,fault]))))))))
      (agent-repl-itest--script daemon "DaemonHealth" response))
    (agent-repl-itest--with-cold-start
      (agent-repl-itest-daemon--with-stubs boot-dir
        (let* ((start (agent-repl-itest-daemon--write-script
                       (expand-file-name "start.sh" boot-dir)
                       (format "touch %sstart-ran" boot-dir)))
               (agent-repl-daemon-command (list start))
               (agent-repl-link-up-functions nil)
               (agent-repl-link-no-daemon-functions nil))
          ;; Act.
          (agent-repl-daemon-ensure)
          (agent-repl-itest--await-call daemon "DaemonHealth")
          ;; Assert.
          (should-not (agent-repl-itest-daemon--ran-p boot-dir "start-ran"))
          ;; "adopt it ... `agent-repl-link-connect'" — UNHEALTHY IS AN
          ;; ANSWER, so it is adopted the same way a healthy one is: link up,
          ;; `WatchDaemon' standing.
          (agent-repl-itest--wait-until (lambda () (agent-repl-link-up-p))
                                        nil "the link to come up")
          (should (agent-repl-link-up-p))
          (agent-repl-itest--await-subscriber daemon "daemon")
          (should (agent-repl-itest--subscribers daemon "daemon")))))))

(ert-deftest agent-repl-itest-daemon-unhealthy-faults-reach-the-health-buffer ()
  "An adopted-but-unhealthy daemon's faults are surfaced, not swallowed.
The faults carry dynamic detail strings; the user must be able to see
what is wrong with the daemon they just adopted."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let* ((fault '((detail . "store socket unreachable")
                    (wsmReadOnly . ())))
           (response `((success . ((unhealthy . ((faults . [,fault]))))))))
      (agent-repl-itest--script daemon "DaemonHealth" response))
    (agent-repl-itest--with-cold-start
      (let ((agent-repl-link-up-functions nil)
            (agent-repl-link-no-daemon-functions nil))
        ;; Act.
        (agent-repl-daemon-ensure)
        (agent-repl-itest--await-call daemon "DaemonHealth")
        ;; Assert.
        (agent-repl-itest--wait-until
         (lambda () (get-buffer "*agent-repl-health*"))
         nil "the health buffer")
        (with-current-buffer "*agent-repl-health*"
          (should (string-match-p "store socket unreachable" (buffer-string))))
        ;; "unhealthy faults go to `*agent-repl-health*' as a WARNING" — the
        ;; buffer alone is not a warning; the production log must carry the
        ;; unhealthy adoption at `warn' level too.
        (agent-repl-itest--await-log daemon "elisp.daemon.adopted-unhealthy" "warn")
        (should (agent-repl-itest--logged-p daemon "elisp.daemon.adopted-unhealthy"
                                            "warn"))))))

(ert-deftest agent-repl-itest-daemon-foreign-daemon-adoption-is-logged ()
  "Adopting a daemon this Emacs did not spawn is logged at INFO.
It is the normal path, not a fault — but it is worth being able to see in
the log which daemon a session attached to."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--with-cold-start
      (let ((agent-repl-link-up-functions nil)
            (agent-repl-link-no-daemon-functions nil))
        ;; Act.
        (agent-repl-daemon-ensure)
        (agent-repl-itest--await-call daemon "DaemonHealth")
        ;; Assert.
        (agent-repl-itest--await-log daemon "elisp.daemon.foreign-adopted" "info")
        (should (agent-repl-itest--logged-p daemon "elisp.daemon.foreign-adopted"
                                            "info"))))))

;;;; ---- No daemon.addr: build, then start ----

(ert-deftest agent-repl-itest-daemon-absent-addr-runs-the-build-script ()
  "With no daemon.addr, cold start runs the build script first.
The build script does its own build-if-stale work; Emacs only invokes
it."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--stop-daemon daemon t)
    (agent-repl-itest--with-cold-start
      (agent-repl-itest-daemon--with-stubs boot-dir
        (let* ((build (agent-repl-itest-daemon--write-script
                       (expand-file-name "build.sh" boot-dir)
                       (format "touch %sbuild-ran" boot-dir)))
               (start (agent-repl-itest-daemon--write-script
                       (expand-file-name "start.sh" boot-dir)
                       (format "touch %sstart-ran" boot-dir)))
               (agent-repl-daemon-build-script build)
               (agent-repl-daemon-command (list start))
               (agent-repl-daemon-boot-timeout-seconds 2)
               (agent-repl-link-up-functions nil)
               (agent-repl-link-no-daemon-functions nil))
          ;; Act.
          (agent-repl-daemon-ensure)
          ;; Assert.
          (agent-repl-itest--wait-until
           (lambda () (agent-repl-itest-daemon--ran-p boot-dir "build-ran"))
           nil "the build script to run")
          (should (agent-repl-itest-daemon--ran-p boot-dir "build-ran")))))))

(ert-deftest agent-repl-itest-daemon-absent-addr-starts-the-daemon-command ()
  "After a successful build, cold start runs the daemon command.
`daemon/bin/claude-repld' with NO required argv (E3): the state root
travels in the environment, never on the command line."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--stop-daemon daemon t)
    (agent-repl-itest--with-cold-start
      (agent-repl-itest-daemon--with-stubs boot-dir
        (let* ((build (agent-repl-itest-daemon--write-script
                       (expand-file-name "build.sh" boot-dir) "exit 0"))
               (start (agent-repl-itest-daemon--write-script
                       (expand-file-name "start.sh" boot-dir)
                       (format "touch %sstart-ran" boot-dir)))
               (agent-repl-daemon-build-script build)
               (agent-repl-daemon-command (list start))
               (agent-repl-daemon-boot-timeout-seconds 2)
               (agent-repl-link-up-functions nil)
               (agent-repl-link-no-daemon-functions nil))
          ;; Act.
          (agent-repl-daemon-ensure)
          ;; Assert.
          (agent-repl-itest--wait-until
           (lambda () (agent-repl-itest-daemon--ran-p boot-dir "start-ran"))
           nil "the daemon command to run")
          (should (agent-repl-itest-daemon--ran-p boot-dir "start-ran")))))))

(ert-deftest agent-repl-itest-daemon-start-exports-the-state-dir ()
  "The daemon command is started with AGENT_REPL_STATE_DIR exported.
There is ONE state root and it travels in the environment; a daemon that
inherited a different one would publish its address where nobody looks."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((state-dir (agent-repl-itest-daemon-state-dir daemon)))
      (agent-repl-itest--stop-daemon daemon t)
      (agent-repl-itest--with-cold-start
        (agent-repl-itest-daemon--with-stubs boot-dir
          (let* ((build (agent-repl-itest-daemon--write-script
                         (expand-file-name "build.sh" boot-dir) "exit 0"))
                 (start (agent-repl-itest-daemon--write-script
                         (expand-file-name "start.sh" boot-dir)
                         (format "printf '%%s' \"$AGENT_REPL_STATE_DIR\" > %sstate-dir"
                                 boot-dir)))
                 (agent-repl-daemon-build-script build)
                 (agent-repl-daemon-command (list start))
                 (agent-repl-daemon-boot-timeout-seconds 2)
                 (agent-repl-link-up-functions nil)
                 (agent-repl-link-no-daemon-functions nil))
            ;; Act.
            (agent-repl-daemon-ensure)
            ;; Assert.
            (agent-repl-itest--wait-until
             (lambda () (agent-repl-itest-daemon--ran-p boot-dir "state-dir"))
             nil "the daemon command to record its state dir")
            (with-temp-buffer
              (insert-file-contents (expand-file-name "state-dir" boot-dir))
              (should (equal (file-name-as-directory
                              (expand-file-name (string-trim (buffer-string))))
                             (file-name-as-directory
                              (expand-file-name state-dir)))))))))))

(ert-deftest agent-repl-itest-daemon-links-up-once-the-addr-appears ()
  "Cold start finishes by connecting, once the started daemon publishes.
Waiting is done with a timer against a deadline, never a sleep — the
whole point is that Emacs stays responsive while the daemon boots."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((binary (agent-repl-itest--ensure-binary)))
      (agent-repl-itest--stop-daemon daemon t)
      (agent-repl-itest--with-cold-start
        (agent-repl-itest-daemon--with-stubs boot-dir
          (let* ((build (agent-repl-itest-daemon--write-script
                         (expand-file-name "build.sh" boot-dir) "exit 0"))
                 ;; The stub daemon IS the fake: `exec' replaces the stub, so
                 ;; the process Emacs spawned and supervises is the fake
                 ;; itself, and it publishes daemon.addr into the one state
                 ;; root exactly as the real daemon does.
                 (start (agent-repl-itest-daemon--write-script
                         (expand-file-name "start.sh" boot-dir)
                         (format "exec %s" (shell-quote-argument binary))))
                 (agent-repl-daemon-build-script build)
                 (agent-repl-daemon-command (list start))
                 (agent-repl-daemon-boot-timeout-seconds 10)
                 (agent-repl-link-reconnect-interval-seconds 0.05)
                 (agent-repl-link-up-functions nil)
                 (agent-repl-link-no-daemon-functions nil))
            ;; Act.
            (agent-repl-daemon-ensure)
            ;; Assert.
            (agent-repl-itest--wait-until (lambda () (agent-repl-link-up-p))
                                          nil "the link to come up")
            (should (agent-repl-link-up-p))
            ;; "INFO `elisp.daemon.foreign-adopted' when this Emacs did NOT
            ;; spawn it" — the negative: THIS Emacs spawned this daemon
            ;; itself (cold start's build-and-start path), so it must never
            ;; be logged as foreign.
            (should-not (agent-repl-itest--logged-p daemon "elisp.daemon.foreign-adopted"))))))))

;;;; ---- A stale address ----

(ert-deftest agent-repl-itest-daemon-stale-addr-is-treated-as-absent ()
  "An address nothing answers is a STALE FILE: treated as absent, warned.
A transport failure against daemon.addr proves there is no daemon, which
is a different fact from an unhealthy one."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((addr-file (agent-repl-itest--addr-file
                      (agent-repl-itest-daemon-state-dir daemon))))
      (agent-repl-itest--stop-daemon daemon t)
      ;; A leftover address pointing at a port nothing listens on.
      (with-temp-file addr-file (insert "127.0.0.1:1\n"))
      (agent-repl-itest--with-cold-start
        (agent-repl-itest-daemon--with-stubs boot-dir
          (let* ((build (agent-repl-itest-daemon--write-script
                         (expand-file-name "build.sh" boot-dir)
                         (format "touch %sbuild-ran" boot-dir)))
                 (start (agent-repl-itest-daemon--write-script
                         (expand-file-name "start.sh" boot-dir)
                         (format "touch %sstart-ran" boot-dir)))
                 (agent-repl-daemon-build-script build)
                 (agent-repl-daemon-command (list start))
                 (agent-repl-daemon-boot-timeout-seconds 2)
                 (agent-repl-link-up-functions nil)
                 (agent-repl-link-no-daemon-functions nil))
            ;; Act.
            (agent-repl-daemon-ensure)
            ;; Assert: the stale file is warned about and the build proceeds.
            (agent-repl-itest--wait-until
             (lambda () (agent-repl-itest--logged-p daemon "elisp.daemon.stale-addr"
                                                    "warn"))
             nil "the stale-addr warning")
            (agent-repl-itest--wait-until
             (lambda () (agent-repl-itest-daemon--ran-p boot-dir "build-ran"))
             nil "the build script to run")
            (should (agent-repl-itest-daemon--ran-p boot-dir "build-ran"))
            ;; "treated as absent" — absent means the FULL absent sequence,
            ;; build AND start, not merely the build half of it.
            (agent-repl-itest--wait-until
             (lambda () (agent-repl-itest-daemon--ran-p boot-dir "start-ran"))
             nil "the daemon command to run")
            (should (agent-repl-itest-daemon--ran-p boot-dir "start-ran"))))))))

;;;; ---- A boot that never publishes daemon.addr ----

(ert-deftest agent-repl-itest-daemon-boot-timeout-surfaces-with-no-link ()
  "A started daemon that never publishes daemon.addr times out, not hangs.
If the boot wait kept polling forever, this test itself would fail on
the harness's own deadline instead of on the small boot timeout the
scenario sets — proving both that the timeout is surfaced and that
nothing here can hang past it."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--stop-daemon daemon t)
    (agent-repl-itest--with-cold-start
      (agent-repl-itest-daemon--with-stubs boot-dir
        (let* ((build (agent-repl-itest-daemon--write-script
                       (expand-file-name "build.sh" boot-dir) "exit 0"))
               ;; Starts and idles WITHOUT ever writing daemon.addr.
               (start (agent-repl-itest-daemon--write-script
                       (expand-file-name "start.sh" boot-dir) "sleep 30"))
               (agent-repl-daemon-build-script build)
               (agent-repl-daemon-command (list start))
               ;; "wait for daemon.addr up to
               ;; `agent-repl-daemon-boot-timeout-seconds' (30)" — rebound
               ;; small (well under the harness's 15s default wait) so a
               ;; scenario that pins the timeout stays fast.
               (agent-repl-daemon-boot-timeout-seconds 1.0)
               (agent-repl-link-up-functions nil)
               (agent-repl-link-no-daemon-functions nil))
          ;; Act.
          (agent-repl-daemon-ensure)
          ;; Assert: the timeout is surfaced in the production log ...
          (agent-repl-itest--wait-until
           (lambda () (agent-repl-itest--logged-p daemon "elisp.daemon.boot-timeout"))
           5 "the boot timeout to be logged")
          (should (agent-repl-itest--logged-p daemon "elisp.daemon.boot-timeout"))
          ;; ... and no link ever comes up from a daemon that never published.
          (should-not (agent-repl-link-up-p)))))))

;;;; ---- The default daemon command and no argv ----

(defconst agent-repl-itest-daemon--module-root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory (or load-file-name
                                                   buffer-file-name
                                                   default-directory))))
  "Absolute path to the `modules/app/agent-repl/' directory.
Captured AT LOAD TIME, the way daemon.el captures its own root: inside an
ERT body `load-file-name' is already nil, so deriving it there would ask
the test to know the path the module derives for itself.")

(ert-deftest agent-repl-itest-daemon-command-default-and-no-argv ()
  "`agent-repl-daemon-command' defaults to the module's own binary,
with NO required argv: the state root travels in the environment only.
A wrong default binary path would silently point cold start at nothing;
an argv that grew a flag would give the spawned daemon input it is not
supposed to need."
  ;; Arrange: the default value, independent of any override.  This suite
  ;; file lives in the same `lisp/' directory as daemon.el, so the module
  ;; root is derived the same way daemon.el derives it, without depending
  ;; on the harness's own notion of that path.
  (let* ((expected (expand-file-name "daemon/bin/claude-repld"
                                     agent-repl-itest-daemon--module-root)))
    ;; Assert: "the default the module's `daemon/bin/claude-repld', no argv".
    (should (equal (default-value 'agent-repl-daemon-command) (list expected))))
  ;; Arrange: a cold start whose start stub records its own argument count.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--stop-daemon daemon t)
    (agent-repl-itest--with-cold-start
      (agent-repl-itest-daemon--with-stubs boot-dir
        (let* ((build (agent-repl-itest-daemon--write-script
                       (expand-file-name "build.sh" boot-dir) "exit 0"))
               (start (agent-repl-itest-daemon--write-script
                       (expand-file-name "start.sh" boot-dir)
                       (format "printf '%%s' \"$#\" > %sargc" boot-dir)))
               (agent-repl-daemon-build-script build)
               (agent-repl-daemon-command (list start))
               (agent-repl-daemon-boot-timeout-seconds 2)
               (agent-repl-link-up-functions nil)
               (agent-repl-link-no-daemon-functions nil))
          ;; Act.
          (agent-repl-daemon-ensure)
          ;; Assert: the spawn passes NO arguments.
          (agent-repl-itest--wait-until
           (lambda () (agent-repl-itest-daemon--ran-p boot-dir "argc"))
           nil "the daemon command to record its argument count")
          (with-temp-buffer
            (insert-file-contents (expand-file-name "argc" boot-dir))
            (should (equal (string-trim (buffer-string)) "0"))))))))

;;;; ---- A build failure ----

(ert-deftest agent-repl-itest-daemon-build-failure-does-not-start-a-daemon ()
  "A failed build stops cold start: NO daemon command runs.
Starting a daemon from a build that failed would run a stale or broken
binary and call it success."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--stop-daemon daemon t)
    (agent-repl-itest--with-cold-start
      (agent-repl-itest-daemon--with-stubs boot-dir
        (let* ((build (agent-repl-itest-daemon--write-script
                       (expand-file-name "build.sh" boot-dir)
                       "echo 'compile error: undefined symbol' >&2\nexit 1"))
               (start (agent-repl-itest-daemon--write-script
                       (expand-file-name "start.sh" boot-dir)
                       (format "touch %sstart-ran" boot-dir)))
               (agent-repl-daemon-build-script build)
               (agent-repl-daemon-command (list start))
               (agent-repl-daemon-boot-timeout-seconds 2)
               (agent-repl-link-up-functions nil)
               (agent-repl-link-no-daemon-functions nil))
          ;; Act.
          (agent-repl-daemon-ensure)
          ;; Assert.
          (agent-repl-itest--wait-until
           (lambda () (agent-repl-itest--logged-p daemon "elisp.daemon.build-failed"
                                                  "warn"))
           nil "the build-failure warning")
          (should-not (agent-repl-itest-daemon--ran-p boot-dir "start-ran")))))))

(ert-deftest agent-repl-itest-daemon-build-failure-surfaces-the-output ()
  "A failed build's output is put in a buffer the user can read.
The failure is surfaced, never swallowed: there is no automatic retry, so
the message is the whole recovery path."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--stop-daemon daemon t)
    (agent-repl-itest--with-cold-start
      (agent-repl-itest-daemon--with-stubs boot-dir
        (let* ((build (agent-repl-itest-daemon--write-script
                       (expand-file-name "build.sh" boot-dir)
                       "echo 'compile error: undefined symbol' >&2\nexit 1"))
               (start (agent-repl-itest-daemon--write-script
                       (expand-file-name "start.sh" boot-dir) "exit 0"))
               (agent-repl-daemon-build-script build)
               (agent-repl-daemon-command (list start))
               (agent-repl-daemon-boot-timeout-seconds 2)
               (agent-repl-link-up-functions nil)
               (agent-repl-link-no-daemon-functions nil))
          ;; Act.
          (agent-repl-daemon-ensure)
          ;; Assert.
          (agent-repl-itest--wait-until
           (lambda () (get-buffer "*agent-repl-build-frontend*"))
           nil "the build-output buffer")
          (with-current-buffer "*agent-repl-build-frontend*"
            (should (string-match-p "compile error" (buffer-string)))))))))

(ert-deftest agent-repl-itest-daemon-build-failure-segment-and-interactive-retry ()
  "The mode-line names a failed build; only the INTERACTIVE ensure retries.
The failed build must run exactly once from cold start's own ensure —
any more and the \"no automatic retry\" ruling is broken; any less and
`agent-repl-frontend-daemon-ensure' would not actually be a retry."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--stop-daemon daemon t)
    (agent-repl-itest--with-cold-start
      (agent-repl-itest-daemon--with-stubs boot-dir
        (let* ((counter (expand-file-name "build-count" boot-dir))
               (build (agent-repl-itest-daemon--write-script
                       (expand-file-name "build.sh" boot-dir)
                       (format "echo run >> %s\nexit 1" counter)))
               (start (agent-repl-itest-daemon--write-script
                       (expand-file-name "start.sh" boot-dir)
                       (format "touch %sstart-ran" boot-dir)))
               (agent-repl-daemon-build-script build)
               (agent-repl-daemon-command (list start))
               (agent-repl-daemon-boot-timeout-seconds 2)
               (agent-repl-link-up-functions nil)
               (agent-repl-link-no-daemon-functions nil))
          ;; Act: the automatic cold start.
          (agent-repl-daemon-ensure)
          (agent-repl-itest--wait-until
           (lambda () (file-exists-p counter))
           nil "the build script to run")
          ;; Assert: "the mode-line segment \"daemon: build failed\"" —
          ;; the exact text.
          (should (equal agent-repl-daemon-mode-line-segment "daemon: build failed"))
          ;; Assert: "no automatic retry" — exactly one run so far.
          (should (= 1 (agent-repl-itest-daemon--line-count counter)))
          ;; Act: "`agent-repl-frontend-daemon-ensure' (interactive) retries".
          (agent-repl-frontend-daemon-ensure)
          ;; Assert: the retry ran the build a SECOND time.
          (agent-repl-itest--wait-until
           (lambda () (= 2 (agent-repl-itest-daemon--line-count counter)))
           nil "the interactive retry to run the build a second time")
          (should (= 2 (agent-repl-itest-daemon--line-count counter))))))))

(provide 'test-integration-daemon)

;;; test-integration-daemon.el ends here
