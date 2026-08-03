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
against whatever holds port 8787 on the developer's machine."
  `(let ((agent-repl-frontend-auto-start t)
         (agent-repl--frontend-daemon-process nil))
     (cl-letf (((symbol-function 'agent-repl--frontend-init-inhibited-p)
                (lambda () nil))
               ((symbol-function 'agent-repl--frontend-artifact-exists-p)
                (lambda (_path) t))
               ((symbol-function 'agent-repl--frontend-turn-active-sessions)
                (lambda () nil))
               ((symbol-function 'agent-repl--frontend-run-listener-probe)
                (lambda (&rest _) nil))
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

;;;; ---- deploy-stack: the boot path's whole-stack deploy --------------------
;;
;; The boot path used to run build-frontend.sh, which covers the shim, the
;; webapp and the daemon and MISSES protobuf regeneration and the two
;; launchd services. A wire-format change could therefore leave a new Emacs
;; talking to a daemon built before it.

(ert-deftest agent-repl-test-daemon-deploy-stack-runs-the-deploy-script ()
  "deploy-stack runs bin/deploy-all.sh, not the narrower build script."
  ;; Arrange
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--frontend-run-build-script)
               (lambda (args) (setq captured args) 0)))
      ;; Act
      (agent-repl--frontend-deploy-stack nil)
      ;; Assert
      (should (equal (car captured) agent-repl--frontend-deploy-script)))))

(ert-deftest agent-repl-test-daemon-deploy-stack-suppresses-the-daemon-bounce ()
  "deploy-stack always passes --no-daemon-bounce.
The script's last step restarts the daemon by evaluating a form in Emacs
over emacsclient.  A call made FROM Emacs would re-enter the session that
is mid-boot, and the caller starts the daemon directly anyway."
  ;; Arrange
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--frontend-run-build-script)
               (lambda (args) (setq captured args) 0)))
      ;; Act
      (agent-repl--frontend-deploy-stack nil)
      ;; Assert
      (should (member "--no-daemon-bounce" captured)))))

(ert-deftest agent-repl-test-daemon-deploy-stack-omits-force-by-default ()
  "Without FORCE the deploy argv carries no --force."
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--frontend-run-build-script)
               (lambda (args) (setq captured args) 0)))
      (agent-repl--frontend-deploy-stack nil)
      (should-not (member "--force" captured)))))

(ert-deftest agent-repl-test-daemon-deploy-stack-passes-force-flag ()
  "With FORCE the deploy argv appends --force."
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--frontend-run-build-script)
               (lambda (args) (setq captured args) 0)))
      (agent-repl--frontend-deploy-stack t)
      (should (member "--force" captured)))))

(ert-deftest agent-repl-test-daemon-deploy-stack-surfaces-a-failed-deploy ()
  "A non-zero deploy exit signals rather than launching against stale code."
  (cl-letf (((symbol-function 'agent-repl--frontend-run-build-script)
             (lambda (_args) 1)))
    (should-error (agent-repl--frontend-deploy-stack nil))))

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
               ((symbol-function 'agent-repl--frontend-deploy-stack)
                (lambda (&optional _f) (setq built t) 0))
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
               ((symbol-function 'agent-repl--frontend-deploy-stack)
                (lambda (&optional _f) 0))
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
               (lambda (field payload &rest _) (setq sent (list field payload)) "req-1"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (request-id &rest _) request-id)))
      ;; Act
      (agent-repl--frontend-request-foreign-shutdown)
      ;; Assert — empty ShutdownCmd message (nil payload encodes as `{}').
      (should (equal sent '("shutdown" nil))))))

(ert-deftest agent-repl-test-daemon-foreign-shutdown-tracks-its-ack ()
  "The shutdown command is tracked so a rejected ack surfaces loudly."
  ;; Arrange
  (let (tracked)
    (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () t))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (&rest _) "req-7"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (request-id field &rest _) (setq tracked (list request-id field))
                 request-id)))
      ;; Act
      (agent-repl--frontend-request-foreign-shutdown)
      ;; Assert
      (should (equal tracked '("req-7" "shutdown"))))))

(ert-deftest agent-repl-test-daemon-foreign-shutdown-dials-when-disconnected ()
  "A down link is dialed first — the foreign daemon owns the same socket."
  ;; Arrange
  (let ((dials 0))
    (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () nil))
              ((symbol-function 'agent-repl-uds-connect)
               (lambda (&optional _p) (cl-incf dials) nil))
              ((symbol-function 'agent-repl--uds-send-command) (lambda (&rest _) "req-1"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (request-id &rest _) request-id)))
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

;;;; ---- Foreign-daemon adoption + stop guard ---------------------------------

(ert-deftest agent-repl-test-daemon-ensure-adopts-foreign-daemon ()
  "A daemon answering on the socket that this Emacs does not track is adopted."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((built nil) (spawned nil))
     (cl-letf (((symbol-function 'agent-repl--frontend-daemon-responsive-async)
                (lambda (open _absent) (funcall open)))
               ((symbol-function 'agent-repl--frontend-deploy-stack)
                (lambda (&optional _f) (setq built t) 0))
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
               ((symbol-function 'agent-repl--frontend-deploy-stack)
                (lambda (&optional _f) (setq built t) 0))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () fresh)))
       ;; Act
       (let ((result (agent-repl--ensure-frontend-daemon t)))
         ;; Assert
         (should built)
         (should (eq result :pending)))))))

(ert-deftest agent-repl-test-daemon-stop-refuses-during-turn ()
  "Stopping is refused while any daemon session has a turn in flight."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((live (agent-repl-test--make-live-daemon)))
     (setq agent-repl--frontend-daemon-process live)
     (cl-letf (((symbol-function 'agent-repl--frontend-turn-active-sessions)
                (lambda () '("s_busy"))))
       ;; Act / Assert
       (let ((err (should-error (agent-repl--frontend-stop-daemon))))
         (should (string-match-p "turn in flight" (error-message-string err))))
       ;; The daemon survives the refusal.
       (should (agent-repl-test--fake-daemon-live live))
       (should (eq agent-repl--frontend-daemon-process live))))))

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

(ert-deftest agent-repl-test-daemon-command-carries-remediation-dir ()
  "The daemon argv nominates the checkout the lost-session analyst works in."
  ;; Arrange
  (let ((agent-repl-frontend-remediate-lost-sessions t))
    ;; Act
    (let ((cmd (agent-repl--frontend-daemon-command)))
      ;; Assert
      (should (member agent-repl--frontend-repo-root
                      (cdr (member "-remediation-dir" cmd)))))))

(ert-deftest agent-repl-test-daemon-command-carries-remediation-permission-mode ()
  "The headless analyst is handed the configured permission mode."
  ;; Arrange
  (let ((agent-repl-frontend-remediate-lost-sessions t)
        (agent-repl-frontend-remediation-permission-mode "bypassPermissions"))
    ;; Act
    (let ((cmd (agent-repl--frontend-daemon-command)))
      ;; Assert
      (should (member "bypassPermissions"
                      (cdr (member "-remediation-permission-mode" cmd)))))))

(ert-deftest agent-repl-test-daemon-command-omits-permission-mode-when-nil ()
  "A nil permission mode hands the analyst no --permission-mode at all."
  ;; Arrange
  (let ((agent-repl-frontend-remediate-lost-sessions t)
        (agent-repl-frontend-remediation-permission-mode nil))
    ;; Act
    (let ((cmd (agent-repl--frontend-daemon-command)))
      ;; Assert
      (should (member "-remediation-dir" cmd))
      (should-not (member "-remediation-permission-mode" cmd)))))

(ert-deftest agent-repl-test-daemon-command-carries-the-ungated-analyst-consent ()
  "The held consent reaches the daemon, which refuses to boot ungated without it."
  ;; Arrange
  (let ((agent-repl-frontend-remediate-lost-sessions t)
        (agent-repl-frontend-remediation-allow-ungated t))
    ;; Act
    (let ((cmd (agent-repl--frontend-daemon-command)))
      ;; Assert
      (should (member "-allow-ungated-remediation" cmd)))))

(ert-deftest agent-repl-test-daemon-command-withholds-a-revoked-ungated-consent ()
  "Withdrawing the consent withholds the flag rather than quietly re-granting it."
  ;; Arrange
  (let ((agent-repl-frontend-remediate-lost-sessions t)
        (agent-repl-frontend-remediation-allow-ungated nil))
    ;; Act
    (let ((cmd (agent-repl--frontend-daemon-command)))
      ;; Assert
      (should-not (member "-allow-ungated-remediation" cmd)))))

(ert-deftest agent-repl-test-daemon-command-omits-ungated-consent-when-remediation-off ()
  "No remediation means no analyst, so there is nothing to consent to."
  ;; Arrange
  (let ((agent-repl-frontend-remediate-lost-sessions nil)
        (agent-repl-frontend-remediation-allow-ungated t))
    ;; Act
    (let ((cmd (agent-repl--frontend-daemon-command)))
      ;; Assert
      (should-not (member "-allow-ungated-remediation" cmd)))))

(ert-deftest agent-repl-test-daemon-remediation-consent-defaults-to-acknowledged ()
  "The shipped default accepts the ungated tradeoff deliberately, not by omission."
  ;; Arrange / Act / Assert — a gated analyst is auto-denied on every tool
  ;; call, so the ungated mode is the only one in which remediation works.
  (should (eq (default-value 'agent-repl-frontend-remediation-allow-ungated) t)))

(ert-deftest agent-repl-test-daemon-command-omits-remediation-when-disabled ()
  "Disabling remediation drops -remediation-dir, which disables it daemon-side."
  ;; Arrange
  (let ((agent-repl-frontend-remediate-lost-sessions nil))
    ;; Act
    (let ((cmd (agent-repl--frontend-daemon-command)))
      ;; Assert
      (should-not (member "-remediation-dir" cmd)))))

(ert-deftest agent-repl-test-daemon-repo-root-contains-the-module ()
  "The remediation checkout is the tree this module lives in."
  ;; Arrange / Act
  (let ((root agent-repl--frontend-repo-root))
    ;; Assert
    (should (string-prefix-p (expand-file-name root)
                             (expand-file-name agent-repl--frontend-root)))
    (should (file-directory-p (expand-file-name "modules/app/agent-repl" root)))))

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

(ert-deftest agent-repl-test-foreign-shutdown-omits-stop-shims-by-default ()
  "`ShutdownCmd' carries no payload unless stop-shims was asked for."
  (let (sent)
    (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () t))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (field payload &rest _) (setq sent (list field payload)) "req-1"))
              ((symbol-function 'agent-repl--uds-track-command) (lambda (&rest _) nil)))
      (agent-repl--frontend-request-foreign-shutdown)
      (should (equal sent (list "shutdown" nil))))))

(ert-deftest agent-repl-test-foreign-shutdown-sets-stop-shims ()
  "A stop-shims request sets `ShutdownCmd.stop_shims' on the wire."
  (let (sent)
    (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () t))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (field payload &rest _) (setq sent (list field payload)) "req-1"))
              ((symbol-function 'agent-repl--uds-track-command) (lambda (&rest _) nil)))
      (agent-repl--frontend-request-foreign-shutdown t)
      (should (equal sent (list "shutdown" (list :stopShims t)))))))

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

(provide 'test-daemon)

;;; test-daemon.el ends here
