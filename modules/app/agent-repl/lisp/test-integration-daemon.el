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
           4 "the boot timeout to be logged")
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
          ;; The capture buffer now exists from the moment the build process
          ;; is spawned, so its EXISTENCE no longer means the output landed;
          ;; the output itself is what this waits on.
          (agent-repl-itest--wait-until
           (lambda ()
             (let ((buffer (get-buffer "*agent-repl-build-frontend*")))
               (and buffer
                    (with-current-buffer buffer
                      (string-match-p "compile error" (buffer-string))))))
           nil "the build output to be captured")
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

;;;; ---- Audit-2 additions (R-SUITE-2) ----
;;
;; Findings 39-42 of docs/overhaul/reports/elisp-suite-audit-2.md.  Kept in
;; their own section so they merge cleanly beside concurrent edits above.

(declare-function agent-repl-frontend-daemon-restart "daemon")
(declare-function agent-repl-link-connect "daemon-link")
(declare-function agent-repl-itest--script "test-integration-helpers")
(defvar agent-repl-daemon-build-failure)

;; audit-2 #39
(ert-deftest agent-repl-itest-daemon-health-error-arm-is-still-adopted ()
  "A daemon that REFUSES the health question still answered it: adopt it.
elisp.md: \"Emacs adopts any daemon that answers DaemonHealth\".
`DaemonHealthError' declares no arms at all, so `{}' is the legal whole
of a refusal — something is serving on that address, which is the only
question cold start is asking, so the refusal is a WARNING beside the
adoption rather than a reason to build and start a second daemon."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "DaemonHealth" '((error . ())))
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
          ;; Assert: the refusal is surfaced ...
          (agent-repl-itest--await-log daemon "elisp.daemon.health-error" "warn")
          ;; ... nothing was built or started ...
          (should-not (agent-repl-itest-daemon--ran-p boot-dir "build-ran"))
          (should-not (agent-repl-itest-daemon--ran-p boot-dir "start-ran"))
          ;; ... and the daemon that refused is nonetheless the link's.
          (agent-repl-itest--wait-until (lambda () (agent-repl-link-up-p))
                                        nil "the link to come up")
          (should (agent-repl-link-up-p))
          (agent-repl-itest--await-subscriber daemon "daemon")
          (should (agent-repl-itest--subscribers daemon "daemon")))))))

;; audit-2 #40
(ert-deftest agent-repl-itest-daemon-boot-timeout-is-logged-at-error ()
  "The boot timeout is logged at ERROR, not at some unstated level.
fanout §11 names the surfaced timeout beside the build failure's
\"WARNING, `message'\"; a timeout recorded at `debug' would be invisible
to the remediation loop that reads these runs by level."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--stop-daemon daemon t)
    (agent-repl-itest--with-cold-start
      (agent-repl-itest-daemon--with-stubs boot-dir
        (let* ((build (agent-repl-itest-daemon--write-script
                       (expand-file-name "build.sh" boot-dir) "exit 0"))
               (start (agent-repl-itest-daemon--write-script
                       (expand-file-name "start.sh" boot-dir) "sleep 30"))
               (agent-repl-daemon-build-script build)
               (agent-repl-daemon-command (list start))
               (agent-repl-daemon-boot-timeout-seconds 1.0)
               (agent-repl-link-up-functions nil)
               (agent-repl-link-no-daemon-functions nil))
          ;; Act.
          (agent-repl-daemon-ensure)
          ;; Assert.
          (agent-repl-itest--wait-until
           (lambda () (agent-repl-itest--logged-p daemon "elisp.daemon.boot-timeout"
                                                  "error"))
           4 "the boot timeout to be logged at error")
          (should (agent-repl-itest--logged-p daemon "elisp.daemon.boot-timeout"
                                              "error")))))))

;; audit-2 #40
(ert-deftest agent-repl-itest-daemon-boot-timeout-surfaces-a-message-and-no-build-segment ()
  "A boot timeout is told to the user, and is NOT reported as a build failure.
fanout §11: the timeout is surfaced.  The mode-line segment belongs to
the BUILD state, and this build succeeded — a timeout that painted
\"daemon: build failed\" would send the user to fix a build that is fine."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--stop-daemon daemon t)
    (agent-repl-itest--with-cold-start
      (agent-repl-itest-daemon--with-stubs boot-dir
        (let* ((build (agent-repl-itest-daemon--write-script
                       (expand-file-name "build.sh" boot-dir) "exit 0"))
               (start (agent-repl-itest-daemon--write-script
                       (expand-file-name "start.sh" boot-dir) "sleep 30"))
               (agent-repl-daemon-build-script build)
               (agent-repl-daemon-command (list start))
               (agent-repl-daemon-boot-timeout-seconds 1.0)
               (agent-repl-link-up-functions nil)
               (agent-repl-link-no-daemon-functions nil)
               (messages nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (if args (apply #'format fmt args) fmt) messages)
                       nil)))
            ;; Act: the INTERACTIVE ensure, which is what a user reaches.
            (agent-repl-frontend-daemon-ensure)
            ;; Assert: the timeout reaches the echo area.
            (agent-repl-itest--wait-until
             (lambda () (seq-some (lambda (m) (string-match-p "NOT ready" m)) messages))
             4 "the boot timeout to be surfaced to the user"))
          ;; Assert: and the build state is untouched.  The segment may be
          ;; carrying the short-lived "stack built in N.Ns" note from the
          ;; build that SUCCEEDED — what it must never say is that the build
          ;; failed, which is the misdirection this scenario exists to catch.
          (should (null agent-repl-daemon-build-failure))
          (should-not (equal agent-repl-daemon-mode-line-segment
                             "daemon: build failed")))))))

;; audit-2 #41
(ert-deftest agent-repl-itest-daemon-restart-asks-the-daemon-to-stop-itself ()
  "`agent-repl-frontend-daemon-restart' STOPS the daemon by asking it to exit.
fanout §11; daemon.el: \"EMACS NEVER KILLS A DAEMON.\"  The stop is
`UpdateShutdownSchedule{now}' with an operator reason naming this editor,
which is the only shutdown that strands nothing."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--with-cold-start
      (agent-repl-itest-daemon--with-stubs boot-dir
        (let* ((build (agent-repl-itest-daemon--write-script
                       (expand-file-name "build.sh" boot-dir) "exit 0"))
               (start (agent-repl-itest-daemon--write-script
                       (expand-file-name "start.sh" boot-dir) "exit 0"))
               (agent-repl-daemon-build-script build)
               (agent-repl-daemon-command (list start))
               (agent-repl-daemon-boot-timeout-seconds 1.0)
               (agent-repl-link-up-functions nil)
               (agent-repl-link-no-daemon-functions nil))
          ;; The link must be standing: the stop goes out on the primary.
          (agent-repl-daemon-ensure)
          (agent-repl-itest--wait-until (lambda () (agent-repl-link-up-p))
                                        nil "the link to come up")
          ;; Act.
          (agent-repl-frontend-daemon-restart)
          (agent-repl-itest--await-call daemon "UpdateShutdownSchedule")
          ;; Assert.
          (let ((body (car (agent-repl-itest--call-bodies
                            daemon "UpdateShutdownSchedule"))))
            (should (assq 'now body))
            (should (equal (agent-repl-itest--body-field
                            body 'now 'reason 'operator 'note)
                           "emacs"))))))))

;; audit-2 #41
(ert-deftest agent-repl-itest-daemon-restart-then-ensures-a-fresh-daemon ()
  "A restart brings a FRESH daemon up once the old one is gone.
fanout §11: the restart is \"stop then ensure\" — \"the daemon is ASKED to
exit, the link tears down, and the ensure brings a fresh one up from a
fresh build.\"  An ensure that ran while the departing daemon still
answered would simply re-adopt the daemon it just asked to leave, and no
fresh build would ever happen."
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
               (agent-repl-daemon-boot-timeout-seconds 2.0)
               (agent-repl-link-up-functions nil)
               (agent-repl-link-no-daemon-functions nil))
          ;; The link must be standing: the stop goes out on the primary,
          ;; and the restart sequences everything else on its ack.
          (agent-repl-daemon-ensure)
          (agent-repl-itest--wait-until (lambda () (agent-repl-link-up-p))
                                        nil "the link to come up")
          ;; Act.
          (agent-repl-frontend-daemon-restart)
          (agent-repl-itest--await-call daemon "UpdateShutdownSchedule")
          ;; The daemon does what it was asked: it exits and removes
          ;; `daemon.addr', which is the state a fresh ensure must see.
          ;; The fake honours the ask on its own control endpoint -- it has
          ;; no shutdown scheduler of its own to run down.
          (agent-repl-itest--exit daemon)
          (agent-repl-itest--wait-until
           (lambda () (null (agent-repl-itest--read-addr-file
                             (agent-repl-itest-daemon-state-dir daemon))))
           nil "the stopped daemon to remove daemon.addr")
          ;; Assert: the ensure half of the restart built and started one.
          (agent-repl-itest--wait-until
           (lambda () (agent-repl-itest-daemon--ran-p boot-dir "build-ran"))
           3 "the restart's own ensure to run the build script")
          (should (agent-repl-itest-daemon--ran-p boot-dir "build-ran"))
          (agent-repl-itest--wait-until
           (lambda () (agent-repl-itest-daemon--ran-p boot-dir "start-ran"))
           3 "the restart's own ensure to start a daemon")
          (should (agent-repl-itest-daemon--ran-p boot-dir "start-ran")))))))

;; audit-2 #42
(ert-deftest agent-repl-itest-daemon-no-daemon-hook-is-globally-registered ()
  "Cold start hangs off `agent-repl-link-no-daemon-functions' at LOAD time.
daemon.el registers `agent-repl-daemon-ensure' there, and every other
cold-start test scratch-binds the hook to nil and calls the ensure
directly — so the registration itself is unpinned everywhere else.  Here
`agent-repl-link-connect' is the ONLY thing called: the absence of
`daemon.addr' has to reach cold start through production's own wiring."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--stop-daemon daemon t)
    (agent-repl-itest--with-cold-start
      (agent-repl-itest-daemon--with-stubs boot-dir
        (let* ((build (agent-repl-itest-daemon--write-script
                       (expand-file-name "build.sh" boot-dir)
                       (format "touch %sbuild-ran\nexit 0" boot-dir)))
               (start (agent-repl-itest-daemon--write-script
                       (expand-file-name "start.sh" boot-dir)
                       (format "touch %sstart-ran" boot-dir)))
               (agent-repl-daemon-build-script build)
               (agent-repl-daemon-command (list start))
               (agent-repl-daemon-boot-timeout-seconds 1.0))
          ;; Assert the wiring exists at all, then drive it.
          (should (memq #'agent-repl-daemon-ensure
                        (default-value 'agent-repl-link-no-daemon-functions)))
          ;; Act: NO ensure call anywhere here.
          (agent-repl-link-connect)
          ;; Assert.
          (agent-repl-itest--wait-until
           (lambda () (agent-repl-itest-daemon--ran-p boot-dir "build-ran"))
           5 "the no-daemon hook to reach cold start's build script")
          (should (agent-repl-itest-daemon--ran-p boot-dir "build-ran")))))))

;;;; ---- Audit-3 additions (R-SUITE-3) ----
;;
;; Findings 12-17 of docs/overhaul/reports/elisp-suite-audit-3.md.  Kept in
;; their own section so they merge cleanly beside concurrent edits above.

(defvar agent-repl-connect-unary-timeout-seconds)

;; audit-3 #12
(ert-deftest agent-repl-itest-daemon-restart-awaits-departure-before-ensuring ()
  "The restart's departure wait is a GATE, not a race with the ack.
R-RED-MISC: \"stop ack -> teardown -> await daemon.addr removal -> ensure\".
The fake is kept alive well past the stop's ack, so a bounded window with
it still answering must show no re-probe and no ensure at all -- only once
it actually exits does the build-and-start half of the restart run."
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
               (agent-repl-daemon-boot-timeout-seconds 5.0)
               (agent-repl-link-up-functions nil)
               (agent-repl-link-no-daemon-functions nil))
          (agent-repl-daemon-ensure)
          (agent-repl-itest--wait-until (lambda () (agent-repl-link-up-p))
                                        nil "the link to come up")
          (let ((health-calls-before (length (agent-repl-itest--calls daemon "DaemonHealth"))))
            ;; Act: restart, but the fake is left running past its ack.
            (agent-repl-frontend-daemon-restart)
            (agent-repl-itest--await-call daemon "UpdateShutdownSchedule")
            (agent-repl-itest--await-log daemon "elisp.daemon.departure-waiting")
            ;; Assert: a bounded window with the fake still alive.  Not a
            ;; wait-until (there is no positive condition to wait for) -- an
            ;; explicit deadline loop through `accept-process-output', never
            ;; a sleep, so the daemon's own I/O keeps being served while the
            ;; negative is held open.
            (let ((deadline (+ (float-time) 1.0)))
              (while (< (float-time) deadline)
                (accept-process-output nil 0.05)))
            (should (= health-calls-before (length (agent-repl-itest--calls daemon "DaemonHealth"))))
            (should-not (agent-repl-itest-daemon--ran-p boot-dir "build-ran"))
            (should-not (agent-repl-itest-daemon--ran-p boot-dir "start-ran"))
            ;; Act: only now does the fake actually leave.
            (agent-repl-itest--exit daemon)
            (agent-repl-itest--wait-until
             (lambda () (null (agent-repl-itest--read-addr-file
                               (agent-repl-itest-daemon-state-dir daemon))))
             nil "the stopped daemon to remove daemon.addr")
            ;; Assert: the ensure half proceeds once the address is gone.
            (agent-repl-itest--wait-until
             (lambda () (agent-repl-itest-daemon--ran-p boot-dir "build-ran"))
             3 "the restart's own ensure to run the build script")
            (should (agent-repl-itest-daemon--ran-p boot-dir "build-ran"))
            (agent-repl-itest--wait-until
             (lambda () (agent-repl-itest-daemon--ran-p boot-dir "start-ran"))
             3 "the restart's own ensure to start a daemon")
            (should (agent-repl-itest-daemon--ran-p boot-dir "start-ran"))))))))

;; audit-3 #12
(ert-deftest agent-repl-itest-daemon-restart-departure-timeout-adopts-the-still-live-daemon ()
  "A departure wait that times out still leaves the daemon ADOPTED.
`agent-repl-daemon-boot-timeout-seconds' doubles as the departure-wait
deadline (cross-suite note); a daemon that never removes `daemon.addr'
inside it is not gone, and the ensure that follows finds it still
answering -- so it is adopted again, never built or started a second
time."
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
               (agent-repl-daemon-boot-timeout-seconds 0.5)
               (agent-repl-link-up-functions nil)
               (agent-repl-link-no-daemon-functions nil))
          (agent-repl-daemon-ensure)
          (agent-repl-itest--wait-until (lambda () (agent-repl-link-up-p))
                                        nil "the link to come up")
          ;; Act: restart, but the fake is NEVER asked to exit -- it answers
          ;; the stop and simply keeps running, exactly like a departure
          ;; that never actually completes.
          (agent-repl-frontend-daemon-restart)
          (agent-repl-itest--await-call daemon "UpdateShutdownSchedule")
          ;; Assert: the wait gives up ...
          (agent-repl-itest--await-log daemon "elisp.daemon.departure-timeout" "warn")
          ;; ... and the ensure that follows ADOPTS the still-live daemon:
          ;; no build, no start, and the link stands again.
          (agent-repl-itest--wait-until (lambda () (agent-repl-link-up-p))
                                        nil "the link to come back up")
          (should (agent-repl-link-up-p))
          (should-not (agent-repl-itest-daemon--ran-p boot-dir "build-ran"))
          (should-not (agent-repl-itest-daemon--ran-p boot-dir "start-ran")))))))

;; audit-3 #13
(ert-deftest agent-repl-itest-daemon-restart-abandoned-on-stop-refusal ()
  "A `nothingScheduled' refusal of the stop abandons the restart outright.
daemon.el: \"the refusal stands and the link is left alone\" -- ensuring
here would adopt the daemon it just asked to leave and report a restart
that never happened."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "UpdateShutdownSchedule"
                              '((error . ((nothingScheduled . ())))))
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
               (agent-repl-daemon-boot-timeout-seconds 2.0)
               (agent-repl-link-up-functions nil)
               (agent-repl-link-no-daemon-functions nil)
               (messages nil))
          (agent-repl-daemon-ensure)
          (agent-repl-itest--wait-until (lambda () (agent-repl-link-up-p))
                                        nil "the link to come up")
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (if args (apply #'format fmt args) fmt) messages)
                       nil)))
            ;; Act.
            (agent-repl-frontend-daemon-restart)
            (agent-repl-itest--await-call daemon "UpdateShutdownSchedule")
            ;; Assert: the refusal and the abandonment are both on the record.
            (agent-repl-itest--await-log daemon "elisp.daemon.stop-refused" "error")
            (agent-repl-itest--await-log daemon "elisp.daemon.restart-abandoned" "error")
            (agent-repl-itest--wait-until
             (lambda () (seq-some (lambda (m) (string-match-p "not restarting" m)) messages))
             3 "the abandonment to be told to the user"))
          ;; Assert: nothing was built or started, the link never came down,
          ;; and the fake is still there to prove nothing killed it.
          (should-not (agent-repl-itest-daemon--ran-p boot-dir "build-ran"))
          (should-not (agent-repl-itest-daemon--ran-p boot-dir "start-ran"))
          (should (agent-repl-link-up-p))
          (should (process-live-p (agent-repl-itest-daemon-process daemon))))))))

;; audit-3 #13
(ert-deftest agent-repl-itest-daemon-restart-abandoned-on-stop-transport-failure ()
  "An UNREACHABLE stop (a transport failure, not a refused arm) also abandons.
daemon.el treats a transport failure on the stop exactly like a refused
arm: the link is left alone rather than risk re-adopting a daemon that
never actually heard the ask."
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
               (agent-repl-daemon-boot-timeout-seconds 2.0)
               (agent-repl-link-up-functions nil)
               (agent-repl-link-no-daemon-functions nil))
          (agent-repl-daemon-ensure)
          (agent-repl-itest--wait-until (lambda () (agent-repl-link-up-p))
                                        nil "the link to come up")
          ;; Arrange: hold the stop's ANSWER open, and shrink the unary
          ;; timeout so the client gives up on it -- a transport failure,
          ;; never a daemon-authored arm.
          (agent-repl-itest--gate daemon "UpdateShutdownSchedule")
          (unwind-protect
              (let ((agent-repl-connect-unary-timeout-seconds 0.3))
                ;; Act.
                (agent-repl-frontend-daemon-restart)
                (agent-repl-itest--await-call daemon "UpdateShutdownSchedule")
                ;; Assert: the transport failure, not a refusal, is what is
                ;; logged, and the restart still abandons.
                (agent-repl-itest--await-log daemon "elisp.daemon.stop-failed" "error")
                (agent-repl-itest--await-log daemon "elisp.daemon.restart-abandoned" "error"))
            (agent-repl-itest--release-gate daemon "UpdateShutdownSchedule"))
          ;; Assert: nothing was built or started and the link is untouched.
          (should-not (agent-repl-itest-daemon--ran-p boot-dir "build-ran"))
          (should-not (agent-repl-itest-daemon--ran-p boot-dir "start-ran"))
          (should (agent-repl-link-up-p)))))))

;; audit-3 #14
(ert-deftest agent-repl-itest-daemon-restart-with-no-link-is-just-the-ensure ()
  "With no link standing, a restart is nothing but the ensure.
fanout §11; daemon.el: \"nothing to stop\".  No `daemon.addr', no link ->
the restart never sends `UpdateShutdownSchedule' at all, and its whole
effect is cold start's own build-and-start-and-link sequence."
  ;; Arrange: no daemon.addr and no link.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((binary (agent-repl-itest--ensure-binary))
          (state-dir (agent-repl-itest-daemon-state-dir daemon)))
      (agent-repl-itest--stop-daemon daemon t)
      (agent-repl-itest--with-cold-start
        (agent-repl-itest-daemon--with-stubs boot-dir
          (let* ((build (agent-repl-itest-daemon--write-script
                         (expand-file-name "build.sh" boot-dir)
                         (format "touch %sbuild-ran" boot-dir)))
                 (start (agent-repl-itest-daemon--write-script
                         (expand-file-name "start.sh" boot-dir)
                         (format "exec %s" (shell-quote-argument binary))))
                 (agent-repl-daemon-build-script build)
                 (agent-repl-daemon-command (list start))
                 (agent-repl-daemon-boot-timeout-seconds 10)
                 (agent-repl-link-reconnect-interval-seconds 0.05)
                 (agent-repl-link-up-functions nil)
                 (agent-repl-link-no-daemon-functions nil))
            (should-not (agent-repl-link-up-p))
            ;; Act.
            (agent-repl-frontend-daemon-restart)
            ;; Assert.
            (agent-repl-itest--await-log daemon "elisp.daemon.restart-nothing-to-stop" "info")
            (agent-repl-itest--wait-until
             (lambda () (agent-repl-itest-daemon--ran-p boot-dir "build-ran"))
             nil "the ensure's build script to run")
            (should (agent-repl-itest-daemon--ran-p boot-dir "build-ran"))
            (agent-repl-itest--wait-until (lambda () (agent-repl-link-up-p))
                                          nil "the link to come up")
            (should (agent-repl-link-up-p))
            ;; Assert: `UpdateShutdownSchedule' was never sent to the new
            ;; daemon -- there was never anything standing to stop.
            (let* ((address (agent-repl-itest--read-addr-file state-dir))
                   (live (agent-repl-itest--make-daemon :address address)))
              (should (null (agent-repl-itest--calls live "UpdateShutdownSchedule"))))))))))

;; audit-3 #14
(ert-deftest agent-repl-itest-daemon-stop-with-no-link-is-skipped-not-signalled ()
  "`agent-repl-frontend-daemon-stop' with no link stands never signals.
daemon.el: with `agent-repl-link-primary' nil there is nothing to stop --
a WARNING and a message are the whole reaction, and the callback still
runs (with nil) so a caller is never left hanging on one that will not
come."
  ;; Arrange: a fake stands but nothing ever links to it.
  (agent-repl-itest--with-fake-daemon daemon
    (should (null (agent-repl-link-primary)))
    (let (messages (outcome :unset))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (if args (apply #'format fmt args) fmt) messages)
                   nil)))
        ;; Act.  A raw call, with no `condition-case': an unhandled signal
        ;; here would fail this test on its own, which is the whole proof
        ;; of "no signal".
        (agent-repl-frontend-daemon-stop (lambda (accepted) (setq outcome accepted)))
        ;; Assert: the callback ran, with nil -- refused, not abandoned.
        (should (eq outcome nil)))
      (should (seq-some (lambda (m) (string-match-p "no daemon link to stop" m)) messages)))
    (should (agent-repl-itest--logged-p daemon "elisp.daemon.stop-skipped" "warn"))))

;; audit-3 #15
(ert-deftest agent-repl-itest-daemon-concurrent-ensure-is-single-flight ()
  "A second `agent-repl-daemon-ensure' while one is in flight is a no-op.
daemon.el `ensure-already-in-flight': a concurrent ensure is exactly how a
second daemon gets spawned beside a live one, so the daemon command must
run exactly once and only one `WatchDaemon' subscriber ever stands."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((binary (agent-repl-itest--ensure-binary))
          (state-dir (agent-repl-itest-daemon-state-dir daemon)))
      (agent-repl-itest--stop-daemon daemon t)
      (agent-repl-itest--with-cold-start
        (agent-repl-itest-daemon--with-stubs boot-dir
          (let* ((build (agent-repl-itest-daemon--write-script
                         (expand-file-name "build.sh" boot-dir) "exit 0"))
                 (counter (expand-file-name "start-count" boot-dir))
                 (start (agent-repl-itest-daemon--write-script
                         (expand-file-name "start.sh" boot-dir)
                         (format "echo run >> %s\nexec %s"
                                 counter (shell-quote-argument binary))))
                 (agent-repl-daemon-build-script build)
                 (agent-repl-daemon-command (list start))
                 (agent-repl-daemon-boot-timeout-seconds 10)
                 (agent-repl-link-reconnect-interval-seconds 0.05)
                 (agent-repl-link-up-functions nil)
                 (agent-repl-link-no-daemon-functions nil))
            ;; Act: two back-to-back ensures, before the first has settled.
            (agent-repl-daemon-ensure)
            (agent-repl-daemon-ensure)
            ;; Assert: the second was refused as already in flight.
            (agent-repl-itest--await-log daemon "elisp.daemon.ensure-already-in-flight" "info")
            ;; Assert: the daemon command ran exactly once, and exactly one
            ;; `WatchDaemon' subscriber stands on the one daemon that came up.
            (agent-repl-itest--wait-until (lambda () (agent-repl-link-up-p))
                                          nil "the link to come up")
            (should (= 1 (agent-repl-itest-daemon--line-count counter)))
            (let* ((address (agent-repl-itest--read-addr-file state-dir))
                   (live (agent-repl-itest--make-daemon :address address)))
              (agent-repl-itest--await-subscriber live "daemon")
              (should (= 1 (length (agent-repl-itest--subscribers live "daemon")))))))))))

;; audit-3 #16
(ert-deftest agent-repl-itest-daemon-unhealthy-faults-all-kinds-reach-the-health-buffer ()
  "Every `DaemonFault' KIND, not just `wsmReadOnly', reaches the health buffer.
endpoint_daemon_health.proto declares six typed kinds; whichever one a
daemon carries, adoption still happens (unhealthy is an answer) and the
fault's dynamic detail still renders -- the kind classifies the fault, it
does not gate whether the user gets to see it."
  (dolist (case
           (list (cons "adoption-window-expired"
                       '(adoptionWindowExpired
                         . ((workspace . ((id . "ws-1") (dir . "/tmp/ws-1"))))))
                 (cons "log-sink-poisoned"
                       '(logSinkPoisoned . ((sink . "emacs.jsonl"))))
                 (cons "deploy-script-failed"
                       '(deployScriptFailed . ((detail . "deploy script exited 1"))))
                 (cons "successor-spawn-failed"
                       '(successorSpawnFailed . ((detail . "spawn refused: address in use"))))
                 (cons "prompts-dir-missing"
                       '(promptsDirMissing . ((path . "/does/not/exist/prompts"))))
                 (cons "wsm-read-only" '(wsmReadOnly . ()))))
    (let* ((label (car case))
           (kind-cell (cdr case))
           (detail (format "audit-3-16 fault detail for %s" label))
           (fault (list (cons 'detail detail) kind-cell))
           (response `((success . ((unhealthy . ((faults . [,fault]))))))))
      ;; Arrange: a fresh fake and cold start per kind.
      (agent-repl-itest--with-fake-daemon daemon
        (agent-repl-itest--script daemon "DaemonHealth" response)
        (agent-repl-itest--with-cold-start
          (let ((agent-repl-link-up-functions nil)
                (agent-repl-link-no-daemon-functions nil))
            ;; Act.
            (agent-repl-daemon-ensure)
            (agent-repl-itest--await-call daemon "DaemonHealth")
            ;; Assert: adopted regardless of kind.
            (agent-repl-itest--wait-until
             (lambda () (agent-repl-link-up-p))
             nil (format "the link to come up for %s" label))
            (should (agent-repl-link-up-p))
            ;; Assert: this kind's own detail rendered.
            (agent-repl-itest--wait-until
             (lambda () (get-buffer "*agent-repl-health*"))
             nil (format "the health buffer for %s" label))
            (with-current-buffer "*agent-repl-health*"
              (should (string-match-p (regexp-quote detail) (buffer-string))))))))))

;; audit-3 #17
(ert-deftest agent-repl-itest-daemon-own-adoption-is-logged-at-info ()
  "The positive of `elisp.daemon.own-adopted': logged at INFO when THIS
Emacs spawned the daemon it adopts.  Ledger: \"logs own-/foreign-adopted\";
`agent-repl-itest-daemon-links-up-once-the-addr-appears' only pins the
negative (never `foreign-adopted'), leaving the positive claim unpinned."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((binary (agent-repl-itest--ensure-binary)))
      (agent-repl-itest--stop-daemon daemon t)
      (agent-repl-itest--with-cold-start
        (agent-repl-itest-daemon--with-stubs boot-dir
          (let* ((build (agent-repl-itest-daemon--write-script
                         (expand-file-name "build.sh" boot-dir) "exit 0"))
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
            (agent-repl-itest--await-log daemon "elisp.daemon.own-adopted" "info")
            (should (agent-repl-itest--logged-p daemon "elisp.daemon.own-adopted" "info"))))))))

(provide 'test-integration-daemon)

;;; test-integration-daemon.el ends here
