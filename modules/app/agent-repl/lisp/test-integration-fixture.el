;;; test-integration-fixture.el --- The shared fake daemon's isolation guarantee -*- lexical-binding: t; -*-

;;; Commentary:

;; ONE fake daemon serves a whole batch Emacs process, and every scenario is
;; handed it by `agent-repl-itest--with-fake-daemon'.  A process boot costs
;; seconds and the integration suites run hundreds of scenarios, so the saving
;; is the point — but the saving is only allowed to exist because the reset is
;; TOTAL.  This suite is what says so.
;;
;; Every scenario here is self-contained: it runs two consecutive fixtures in
;; one test and asserts across them, so the guarantee is pinned without any
;; dependence on the order ERT happens to run tests in.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

(load (expand-file-name "test-integration-helpers.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(declare-function agent-repl-connect-open "connect")
(declare-function agent-repl-connect-close "connect")
(declare-function agent-repl-connect-unary-sync "connect")
(declare-function agent-repl-connect-read-daemon-addr "connect")

(defun agent-repl-itest-fixture--register (daemon)
  "Issue one real `RegisterWorkspace' at DAEMON, so a call is on its record."
  (let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
    (unwind-protect
        (agent-repl-connect-unary-sync
         conn "RegisterWorkspace" (json-serialize '((dir . "/tmp/itest-fixture-ws"))))
      (agent-repl-connect-close conn))))

;;;; ---- One process, many scenarios ----

(ert-deftest agent-repl-itest-fixture-consecutive-scenarios-share-one-process ()
  "Two scenarios in a row get the SAME daemon process.
That is the whole saving: a scenario pays a reset, not a process boot."
  ;; Arrange.
  (let (first-pid)
    (agent-repl-itest--with-fake-daemon daemon
      (setq first-pid (process-id (agent-repl-itest-daemon-process daemon))))
    ;; Act / Assert.
    (agent-repl-itest--with-fake-daemon daemon
      (should (equal (process-id (agent-repl-itest-daemon-process daemon)) first-pid)))))

;;;; ---- What the reset must throw away ----

(ert-deftest agent-repl-itest-fixture-recorded-calls-do-not-cross-scenarios ()
  "A scenario sees NO call the previous scenario made.
`agent-repl-itest--calls' is how nearly every assertion in the suite reads
what Emacs sent; a call surviving into the next scenario would satisfy
that assertion with another scenario's evidence."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-fixture--register daemon)
    (should (agent-repl-itest--calls daemon "RegisterWorkspace")))
  ;; Act / Assert.
  (agent-repl-itest--with-fake-daemon daemon
    (should-not (agent-repl-itest--calls daemon "RegisterWorkspace"))))

(ert-deftest agent-repl-itest-fixture-scripted-answers-do-not-cross-scenarios ()
  "A scenario answers from the DEFAULT synthesis, never a previous script."
  ;; Arrange: script the error arm the default synthesis never produces.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "RegisterWorkspace" '((error . ()))))
  ;; Act.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((answer (agent-repl-itest-fixture--register daemon)))
      ;; Assert.
      (should (alist-get 'success answer))
      (should-not (alist-get 'error answer)))))

(ert-deftest agent-repl-itest-fixture-armed-gates-do-not-cross-scenarios ()
  "A gate armed in one scenario is disarmed for the next.
Releasing a gate nobody armed is a loud control-plane refusal, which is
exactly what a still-armed gate would NOT produce."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--gate daemon "RegisterWorkspace"))
  ;; Act / Assert.
  (agent-repl-itest--with-fake-daemon daemon
    (should-error (agent-repl-itest--release-gate daemon "RegisterWorkspace"))))

(ert-deftest agent-repl-itest-fixture-state-root-does-not-cross-scenarios ()
  "A file a scenario left in the shared state root is gone for the next.
Production writes into the state root, and a scenario reading another
scenario's leftovers there is the leak a fresh process used to prevent."
  ;; Arrange.
  (let (litter)
    (agent-repl-itest--with-fake-daemon daemon
      (setq litter (expand-file-name "itest-fixture-litter"
                                     (agent-repl-itest-daemon-state-dir daemon)))
      (with-temp-file litter (insert "left behind\n"))
      (should (file-exists-p litter)))
    ;; Act / Assert.
    (agent-repl-itest--with-fake-daemon daemon
      (ignore daemon)
      (should-not (file-exists-p litter)))))

;;;; ---- The address a scenario is handed always names the live daemon ----

(ert-deftest agent-repl-itest-fixture-a-stopped-daemon-is-replaced ()
  "A scenario that STOPS the shared daemon still leaves the next one served.
Cold start's absent-address cases stop it deliberately; the fixture treats
a dead process as an absent one and respawns into the same state root."
  ;; Arrange.
  (let (stopped-pid)
    (agent-repl-itest--with-fake-daemon daemon
      (setq stopped-pid (process-id (agent-repl-itest-daemon-process daemon)))
      (agent-repl-itest--stop-daemon daemon t))
    ;; Act / Assert.
    (agent-repl-itest--with-fake-daemon daemon
      (should (process-live-p (agent-repl-itest-daemon-process daemon)))
      (should-not (equal (process-id (agent-repl-itest-daemon-process daemon))
                         stopped-pid))
      (should (equal (agent-repl-connect-read-daemon-addr)
                     (agent-repl-itest-daemon-address daemon))))))

(ert-deftest agent-repl-itest-fixture-a-hijacked-addr-file-is-republished ()
  "`daemon.addr' names the shared daemon even after a scenario overwrote it.
A cold-start scenario points the file at a stub daemon of its own; left
there, production's discovery in the next scenario would dial a process
that no longer exists."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (with-temp-file (agent-repl-itest--addr-file
                     (agent-repl-itest-daemon-state-dir daemon))
      (insert "127.0.0.1:1\n")))
  ;; Act / Assert.
  (agent-repl-itest--with-fake-daemon daemon
    (should (equal (agent-repl-connect-read-daemon-addr)
                   (agent-repl-itest-daemon-address daemon)))))

(ert-deftest agent-repl-itest-fixture-a-handover-leaves-the-primary-discoverable ()
  "After a staged handover, `daemon.addr' names the daemon still running.
The successor's orderly exit removes the file both daemons share, so the
primary re-publishes its own address on the way out of the handover."
  ;; Arrange / Act.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--with-second-daemon daemon successor
      (should-not (equal (agent-repl-itest-daemon-address successor)
                         (agent-repl-itest-daemon-address daemon)))))
  ;; Assert.
  (agent-repl-itest--with-fake-daemon daemon
    (should (equal (agent-repl-connect-read-daemon-addr)
                   (agent-repl-itest-daemon-address daemon)))))

(provide 'test-integration-fixture)
;;; test-integration-fixture.el ends here
