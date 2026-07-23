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

The startup staleness one-shot is bound ALREADY-RUN
\(`agent-repl--frontend-startup-staleness-checked' t), so this env models
the STEADY-STATE ensure (the cheap live-process hot path).  Tests
exercising the one-shot itself re-bind it nil in their own `let'."
  `(let ((agent-repl-frontend-auto-start t)
         (agent-repl--frontend-daemon-process nil)
         (agent-repl--frontend-startup-staleness-checked t))
     (cl-letf (((symbol-function 'agent-repl--frontend-init-inhibited-p)
                (lambda () nil))
               ((symbol-function 'agent-repl--frontend-artifact-exists-p)
                (lambda (_path) t))
               ((symbol-function 'agent-repl--frontend-turn-active-sessions)
                (lambda () nil))
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

;;;; ---- poll-until helper ---------------------------------------------------

(ert-deftest agent-repl-test-daemon-poll-until-returns-nil-when-condition-clears ()
  "The poll returns nil (and stops early) once the predicate clears."
  ;; Arrange — a predicate that flips to nil on its second call.
  (let ((calls 0))
    (cl-letf (((symbol-function 'sleep-for) #'ignore))
      ;; Act
      (let ((result (agent-repl--frontend-poll-until
                     (lambda () (setq calls (1+ calls)) (< calls 2))
                     10 0.01)))
        ;; Assert — cleared condition yields nil.
        (should (null result))))))

(ert-deftest agent-repl-test-daemon-poll-until-returns-truthy-on-timeout ()
  "The poll returns the predicate's truthy value when the deadline passes first."
  ;; Arrange — a predicate that never clears, and a zeroed timeout.
  (cl-letf (((symbol-function 'sleep-for) #'ignore))
    ;; Act / Assert
    (should (eq t (agent-repl--frontend-poll-until (lambda () t) 0 0.01)))))

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

(ert-deftest agent-repl-test-daemon-running-mtime-reads-reported-field ()
  "The running-mtime reader returns the daemon's `daemon_binary_mtime'."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-api)
             (lambda (&rest _) '((daemon_binary_mtime . 1700000000)))))
    ;; Act / Assert
    (should (equal 1700000000
                   (agent-repl--frontend-running-daemon-binary-mtime)))))

(ert-deftest agent-repl-test-daemon-running-mtime-nil-when-unreachable ()
  "An unreachable daemon (the GET errors) yields nil, never a guess."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-api)
             (lambda (&rest _) (error "connection refused"))))
    ;; Act / Assert
    (should (null (agent-repl--frontend-running-daemon-binary-mtime)))))

(ert-deftest agent-repl-test-daemon-running-mtime-nil-when-field-absent ()
  "A daemon predating the field (no `daemon_binary_mtime') yields nil."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-api)
             (lambda (&rest _) '((boot_id . "b_abc")))))
    ;; Act / Assert
    (should (null (agent-repl--frontend-running-daemon-binary-mtime)))))

(ert-deftest agent-repl-test-daemon-running-mtime-nil-when-nonpositive ()
  "A zero mtime (the daemon's boot-time self-stat failed) yields nil."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-api)
             (lambda (&rest _) '((daemon_binary_mtime . 0)))))
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
  "Ensure returns nil under the batch/sandbox inhibit guard."
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
     (cl-letf (((symbol-function 'agent-repl--frontend-build-if-stale)
                (lambda (&optional _f) (setq built t) 0))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () (setq spawned t) (agent-repl-test--make-live-daemon))))
       ;; Act
       (let ((result (agent-repl--ensure-frontend-daemon)))
         ;; Assert
         (should (eq result existing))
         (should-not built)
         (should-not spawned))))))

(ert-deftest agent-repl-test-daemon-ensure-builds-then-spawns-when-none ()
  "Ensure builds-if-stale then spawns when no daemon is running."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((built nil)
         (fresh (agent-repl-test--make-live-daemon 777)))
     (cl-letf (((symbol-function 'agent-repl--frontend-daemon-port-responsive-p)
                (lambda () nil))
               ((symbol-function 'agent-repl--frontend-build-if-stale)
                (lambda (&optional _f) (setq built t) 0))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () fresh)))
       ;; Act
       (let ((result (agent-repl--ensure-frontend-daemon)))
         ;; Assert
         (should built)
         (should (eq result fresh))
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
               ((symbol-function 'agent-repl--frontend-build-if-stale)
                (lambda (&optional _f) 0))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () new)))
       ;; Act
       (let ((result (agent-repl--ensure-frontend-daemon t)))
         ;; Assert
         (should (eq result new))
         (should-not (agent-repl-test--fake-daemon-live old)))))))

;;;; ---- startup staleness bounce (one-shot) ---------------------------------

(ert-deftest agent-repl-test-daemon-startup-bounces-stale-tracked-daemon ()
  "On Emacs open, a stale daemon THIS Emacs tracks is stopped and respawned."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((old (agent-repl-test--make-live-daemon 1))
         (new (agent-repl-test--make-live-daemon 2))
         (built nil)
         (agent-repl--frontend-startup-staleness-checked nil))
     (setq agent-repl--frontend-daemon-process old)
     (cl-letf (((symbol-function 'agent-repl--frontend-daemon-stale-p)
                (lambda () t))
               ((symbol-function 'agent-repl--frontend-build-if-stale)
                (lambda (&optional _f) (setq built t) 0))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () new)))
       ;; Act
       (let ((result (agent-repl--ensure-frontend-daemon)))
         ;; Assert — rebuilt, old stopped, fresh process tracked and returned.
         (should built)
         (should-not (agent-repl-test--fake-daemon-live old))
         (should (eq new agent-repl--frontend-daemon-process))
         (should (eq new result)))))))

(ert-deftest agent-repl-test-daemon-startup-leaves-fresh-tracked-daemon ()
  "A daemon that is not stale is reused with no restart on Emacs open."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((old (agent-repl-test--make-live-daemon 1))
         (spawned nil)
         (agent-repl--frontend-startup-staleness-checked nil))
     (setq agent-repl--frontend-daemon-process old)
     (cl-letf (((symbol-function 'agent-repl--frontend-daemon-stale-p)
                (lambda () nil))
               ((symbol-function 'agent-repl--frontend-build-if-stale)
                (lambda (&optional _f) 0))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () (setq spawned t) (agent-repl-test--make-live-daemon))))
       ;; Act
       (let ((result (agent-repl--ensure-frontend-daemon)))
         ;; Assert — same process, still live, nothing spawned.
         (should (eq old result))
         (should (agent-repl-test--fake-daemon-live old))
         (should-not spawned))))))

(ert-deftest agent-repl-test-daemon-startup-defers-bounce-during-turn ()
  "A stale daemon is NOT bounced while a turn is in flight on Emacs open."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((old (agent-repl-test--make-live-daemon 1))
         (spawned nil)
         (agent-repl--frontend-startup-staleness-checked nil))
     (setq agent-repl--frontend-daemon-process old)
     (cl-letf (((symbol-function 'agent-repl--frontend-daemon-stale-p)
                (lambda () t))
               ((symbol-function 'agent-repl--frontend-turn-active-sessions)
                (lambda () '("s_busy")))
               ((symbol-function 'agent-repl--frontend-build-if-stale)
                (lambda (&optional _f) 0))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () (setq spawned t) (agent-repl-test--make-live-daemon))))
       ;; Act
       (let ((result (agent-repl--ensure-frontend-daemon)))
         ;; Assert — the live conversation survives untouched, no SIGTERM sent.
         (should (eq old result))
         (should (agent-repl-test--fake-daemon-live old))
         (should-not spawned)
         (should-not (agent-repl-test--fake-daemon-signals old)))))))

(ert-deftest agent-repl-test-daemon-startup-bounces-stale-foreign-daemon ()
  "A stale ADOPTED daemon is shut down over HTTP, then replaced once the port frees."
  ;; Arrange — no tracked process, so any running daemon is foreign/adopted.
  (agent-repl-test--with-daemon-env
   (let ((foreign-alive t)
         (shutdown-called nil)
         (spawned nil)
         (agent-repl--frontend-startup-staleness-checked nil))
     (cl-letf (((symbol-function 'agent-repl--frontend-daemon-port-responsive-p)
                (lambda () foreign-alive))
               ((symbol-function 'agent-repl--frontend-daemon-stale-p)
                (lambda () t))
               ((symbol-function 'agent-repl--frontend-request-foreign-shutdown)
                ;; The daemon exits: the port stops responding after the ask.
                (lambda () (setq shutdown-called t foreign-alive nil)))
               ((symbol-function 'agent-repl--frontend-build-if-stale)
                (lambda (&optional _f) 0))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () (setq spawned t) (agent-repl-test--make-live-daemon))))
       ;; Act
       (agent-repl--ensure-frontend-daemon)
       ;; Assert — asked to shut down, then a fresh daemon spawned in its place.
       (should shutdown-called)
       (should spawned)))))

(ert-deftest agent-repl-test-daemon-startup-leaves-wedged-foreign-daemon ()
  "A foreign daemon that ignores POST /shutdown is left in place, never spawned over."
  ;; Arrange — the port stays responsive past the (zeroed) grace window.
  (agent-repl-test--with-daemon-env
   (let ((shutdown-called nil)
         (spawned nil)
         (agent-repl--frontend-startup-staleness-checked nil)
         (agent-repl-frontend-foreign-stop-grace-seconds 0))
     (cl-letf (((symbol-function 'agent-repl--frontend-daemon-port-responsive-p)
                (lambda () t))
               ((symbol-function 'agent-repl--frontend-daemon-stale-p)
                (lambda () t))
               ((symbol-function 'agent-repl--frontend-request-foreign-shutdown)
                (lambda () (setq shutdown-called t)))
               ((symbol-function 'agent-repl--frontend-build-if-stale)
                (lambda (&optional _f) 0))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () (setq spawned t) (agent-repl-test--make-live-daemon))))
       ;; Act
       (let ((result (agent-repl--ensure-frontend-daemon)))
         ;; Assert — asked to exit, but never spawned next to the wedged daemon.
         (should shutdown-called)
         (should-not spawned)
         ;; It stays adopted for this session.
         (should (eq t result)))))))

(ert-deftest agent-repl-test-daemon-startup-bounce-noop-when-inhibited ()
  "The staleness bounce never runs under the batch/sandbox inhibit guard."
  ;; Arrange
  (let ((agent-repl-frontend-auto-start t)
        (agent-repl--frontend-daemon-process nil)
        (agent-repl--frontend-startup-staleness-checked nil)
        (checked nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-init-inhibited-p)
               (lambda () t))
              ((symbol-function 'agent-repl--frontend-bounce-if-stale)
               (lambda () (setq checked t))))
      ;; Act
      (let ((result (agent-repl--ensure-frontend-daemon)))
        ;; Assert — inhibit short-circuits before the one-shot even fires.
        (should (null result))
        (should-not checked)
        (should-not agent-repl--frontend-startup-staleness-checked)))))

(ert-deftest agent-repl-test-daemon-startup-staleness-check-runs-once ()
  "The staleness bounce fires on the first ensure only, not on later ones."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((old (agent-repl-test--make-live-daemon 1))
         (bounce-calls 0)
         (agent-repl--frontend-startup-staleness-checked nil))
     (setq agent-repl--frontend-daemon-process old)
     (cl-letf (((symbol-function 'agent-repl--frontend-bounce-if-stale)
                (lambda () (cl-incf bounce-calls))))
       ;; Act — two ensures in one session.
       (agent-repl--ensure-frontend-daemon)
       (agent-repl--ensure-frontend-daemon)
       ;; Assert — the one-shot fired exactly once.
       (should (= 1 bounce-calls))))))

;;;; ---- Foreign-daemon adoption + stop guard ---------------------------------

(ert-deftest agent-repl-test-daemon-ensure-adopts-foreign-daemon ()
  "A daemon answering on the port that this Emacs does not track is adopted."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((built nil) (spawned nil))
     (cl-letf (((symbol-function 'agent-repl--frontend-daemon-port-responsive-p)
                (lambda () t))
               ((symbol-function 'agent-repl--frontend-build-if-stale)
                (lambda (&optional _f) (setq built t) 0))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () (setq spawned t) (agent-repl-test--make-live-daemon))))
       ;; Act
       (let ((result (agent-repl--ensure-frontend-daemon)))
         ;; Assert — adopted (non-nil, no process object), nothing spawned.
         (should (eq result t))
         (should-not built)
         (should-not spawned))))))

(ert-deftest agent-repl-test-daemon-ensure-force-skips-adoption ()
  "FORCE ignores a responsive foreign daemon and builds/spawns fresh."
  ;; Arrange
  (agent-repl-test--with-daemon-env
   (let ((built nil)
         (fresh (agent-repl-test--make-live-daemon 9)))
     (cl-letf (((symbol-function 'agent-repl--frontend-daemon-port-responsive-p)
                (lambda () t))
               ((symbol-function 'agent-repl--frontend-build-if-stale)
                (lambda (&optional _f) (setq built t) 0))
               ((symbol-function 'agent-repl--frontend-spawn-daemon)
                (lambda () fresh)))
       ;; Act
       (let ((result (agent-repl--ensure-frontend-daemon t)))
         ;; Assert
         (should built)
         (should (eq result fresh)))))))

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
          (expand-file-name "agent-shim/claude-shim/dist/main.js"
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
        (agent-repl--frontend-shim-entry "/artifacts/claude-shim/main.js"))
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
              (expand-file-name "agent-shim/claude-shim/dist/main.js"
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

;;;; ---- restart command: rebinds open workspaces ----------------------------

(ert-deftest agent-repl-test-daemon-restart-rebinds-after-force-restart ()
  "Restart force-bounces the daemon FIRST, then rebinds the open workspaces.
Order is contractual: the workspaces can only rebind onto a daemon that has
already been restarted."
  ;; Arrange
  (let ((calls nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-init-inhibited-p)
               (lambda () nil))
              ((symbol-function 'agent-repl--ensure-frontend-daemon)
               (lambda (&optional force) (push (cons 'ensure force) calls) t))
              ((symbol-function 'agent-repl--frontend-rebind-workspaces-after-restart)
               (lambda () (push 'rebind calls) 0)))
      ;; Act
      (agent-repl-frontend-daemon-restart)
      ;; Assert
      (should (equal (reverse calls) '((ensure . t) rebind))))))

(ert-deftest agent-repl-test-daemon-restart-reports-rebound-count ()
  "Restart's confirmation message reports how many workspaces were rebound."
  ;; Arrange
  (let ((reported nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-init-inhibited-p)
               (lambda () nil))
              ((symbol-function 'agent-repl--ensure-frontend-daemon)
               (lambda (&optional _force) t))
              ((symbol-function 'agent-repl--frontend-rebind-workspaces-after-restart)
               (lambda () 3))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq reported (apply #'format fmt args)))))
      ;; Act
      (agent-repl-frontend-daemon-restart)
      ;; Assert
      (should (string-match-p "rebound 3 open workspaces" reported)))))

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

(ert-deftest agent-repl-test--widget-doctor-nil-in-sandbox ()
  "In the sandbox the widget doctor check is a no-op."
  (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () t)))
    (should-not (agent-repl--widget-doctor-issues))))

(ert-deftest agent-repl-test--widget-doctor-warns-when-off ()
  "With nothing discoverable, the doctor warns the capability is off."
  (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil)))
    (let* ((agent-repl-frontend-widget-assets-dir "")
           (agent-repl-frontend-widget-assets-search-root nil)
           (issues (agent-repl--widget-doctor-issues)))
      (should (= 1 (length issues)))
      (should (eq 'warn (caar issues)))
      (should (string-match-p "capability OFF" (cdar issues))))))

(ert-deftest agent-repl-test--widget-doctor-clean-when-bundle-present ()
  "A discoverable dist holding chess-widget.js yields no doctor issue."
  (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil)))
    (agent-repl-test--with-temp-root root
      (let ((agent-repl-frontend-widget-assets-dir "")
            (agent-repl-frontend-widget-assets-search-root root))
        (agent-repl-test--make-widget-dist
         (expand-file-name "explanation-engine/apps/cee-web-widget/dist" root))
        (should-not (agent-repl--widget-doctor-issues))))))

(ert-deftest agent-repl-test--widget-doctor-warns-when-dir-lacks-bundle ()
  "An explicit dir without chess-widget.js warns about the missing bundle."
  (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil)))
    (agent-repl-test--with-temp-root root
      ;; Override points at a real dir that lacks the mount bundle.
      (let ((agent-repl-frontend-widget-assets-dir root))
        (let ((issues (agent-repl--widget-doctor-issues)))
          (should (= 1 (length issues)))
          (should (eq 'warn (caar issues)))
          (should (string-match-p "lacks chess-widget.js" (cdar issues))))))))

(provide 'test-daemon)

;;; test-daemon.el ends here
