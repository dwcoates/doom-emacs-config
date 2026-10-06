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
;;
;; The harness's own CONTROL-PLANE CLIENT is pinned here too, for the same
;; reason: every scenario in every integration suite reaches the fake through
;; it, so a response it decodes wrongly would misreport what the fake saw
;; rather than fail.

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

;;;; ---- The control-plane client ----
;;
;; `agent-repl-itest--control' speaks HTTP/1.1 over a native loopback socket
;; instead of spawning `curl'.  These pin the response decoding, which `curl'
;; used to do on the harness's behalf.

(ert-deftest agent-repl-itest-fixture-control-reads-a-content-length-body ()
  "A response framed by `Content-Length' decodes to its status and payload."
  ;; Arrange.
  (let ((raw (concat "HTTP/1.1 200 OK\r\n"
                     "Content-Type: application/json\r\n"
                     "Content-Length: 9\r\n\r\n"
                     "{\"ok\":1}\n")))
    ;; Act.
    (let ((answer (agent-repl-itest--http-read-body raw "/_fake/probe")))
      ;; Assert.
      (should (equal (car answer) 200))
      (should (equal (string-trim (cdr answer)) "{\"ok\":1}")))))

(ert-deftest agent-repl-itest-fixture-control-reads-a-chunked-body ()
  "A response framed by `Transfer-Encoding: chunked' is de-chunked.
Go picks the framing by how the handler wrote its answer, so the client
cannot assume one."
  ;; Arrange.
  (let ((raw (concat "HTTP/1.1 200 OK\r\n"
                     "Transfer-Encoding: chunked\r\n\r\n"
                     "4\r\n{\"ok\r\n"
                     "4\r\n\":1}\r\n"
                     "0\r\n\r\n")))
    ;; Act.
    (let ((answer (agent-repl-itest--http-read-body raw "/_fake/probe")))
      ;; Assert.
      (should (equal (cdr answer) "{\"ok\":1}")))))

(ert-deftest agent-repl-itest-fixture-control-carries-a-non-200-status ()
  "A refusal's status reaches the caller rather than being read as success.
Several scenarios assert on a deliberate 400."
  ;; Arrange.
  (let ((raw "HTTP/1.1 400 Bad Request\r\nContent-Length: 0\r\n\r\n"))
    ;; Act / Assert.
    (should (equal (car (agent-repl-itest--http-read-body raw "/_fake/probe")) 400))))

(ert-deftest agent-repl-itest-fixture-control-refuses-a-headerless-answer ()
  "An answer with no complete header block signals instead of decoding.
A truncated read that silently produced `nil' would look to every caller
like a fake that answered nothing."
  ;; Arrange.
  (let ((raw "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n"))
    ;; Act / Assert.
    (should-error (agent-repl-itest--http-read-body raw "/_fake/probe"))))

(ert-deftest agent-repl-itest-fixture-control-round-trips-against-the-fake ()
  "A real control call reaches the fake and its parsed body comes back.
`/_fake/reset' answers the counts it cleared, so a decoded `calls' key is
the fake's own answer and not an echo of the request."
  ;; Arrange / Act.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-fixture--register daemon)
    (let ((answer (agent-repl-itest--control daemon "/_fake/reset" "{}")))
      ;; Assert.
      (should (equal (car answer) 200))
      (should (equal (alist-get 'calls (cdr answer)) 1)))))

(ert-deftest agent-repl-itest-fixture-control-round-trips-a-refusal ()
  "A control call the fake refuses comes back as its status, not an error."
  ;; Arrange / Act.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((answer (agent-repl-itest--control daemon "/_fake/reset" "{\"nope\":true}")))
      ;; Assert.
      (should (equal (car answer) 400)))))

;;;; ---- The prebuilt fake ----

(ert-deftest agent-repl-itest-fixture-prebuilt-unset-defers-to-the-build ()
  "No prebuilt path answers nil, so `agent-repl-itest--ensure-binary' builds."
  ;; Arrange.
  (let ((agent-repl-itest--binary nil))
    ;; Act / Assert.
    (should-not (agent-repl-itest--prebuilt-binary nil))
    (should-not (agent-repl-itest--prebuilt-binary ""))
    (should-not agent-repl-itest--binary)))

(ert-deftest agent-repl-itest-fixture-prebuilt-executable-is-adopted ()
  "An executable prebuilt path becomes the process's fake daemon."
  ;; Arrange.
  (let ((agent-repl-itest--binary nil)
        (path (make-temp-file "agent-repl-itest-prebuilt-")))
    (unwind-protect
        (progn
          (set-file-modes path #o755)
          ;; Act.
          (let ((answer (agent-repl-itest--prebuilt-binary path)))
            ;; Assert.
            (should (equal answer path))
            (should (equal agent-repl-itest--binary path))))
      (delete-file path))))

(ert-deftest agent-repl-itest-fixture-prebuilt-non-executable-signals ()
  "A prebuilt path that names no executable fails loudly and adopts nothing."
  ;; Arrange.
  (let ((agent-repl-itest--binary nil)
        (path (make-temp-file "agent-repl-itest-prebuilt-")))
    (unwind-protect
        (progn
          (set-file-modes path #o644)
          ;; Act.
          (let ((err (should-error (agent-repl-itest--prebuilt-binary path))))
            ;; Assert.
            (should (string-match-p "not an executable fake daemon" (cadr err)))
            (should-not agent-repl-itest--binary)))
      (delete-file path))))

;;;; ---- A failed cleanup is surfaced, never swallowed ----

(defun agent-repl-itest-fixture--refuse-removal (&rest args)
  "Stand in for `delete-directory', refusing ARGS' directory."
  (signal 'file-error (list "Removing directory" "Permission denied" (car args))))

(ert-deftest agent-repl-itest-fixture-remove-tree-of-an-absent-dir-is-a-no-op ()
  "An absent directory is already removed, so removing it again is quiet."
  ;; Arrange
  (let ((dir (expand-file-name "never-made" temporary-file-directory)))
    ;; Act / Assert
    (should-not (agent-repl-itest--remove-tree dir))))

(ert-deftest agent-repl-itest-fixture-remove-tree-signals-a-failed-removal ()
  "A directory that cannot be removed signals rather than being ignored."
  ;; Arrange
  (let ((dir (make-temp-file "remove-tree-test-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'delete-directory)
                   #'agent-repl-itest-fixture--refuse-removal))
          ;; Act / Assert
          (should-error (agent-repl-itest--remove-tree dir) :type 'file-error))
      (delete-directory dir t))))

(ert-deftest agent-repl-itest-fixture-fixture-root-sweep-signals-a-failed-removal ()
  "The per-scenario fixture-root sweep fails the scenario it cannot clean for."
  ;; Arrange
  (let ((agent-repl-itest--fixture-root (make-temp-file "fixture-root-test-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'delete-directory)
                   #'agent-repl-itest-fixture--refuse-removal))
          ;; Act / Assert
          (should-error (agent-repl-itest--sweep-fixture-root) :type 'file-error))
      (delete-directory agent-repl-itest--fixture-root t))))

(ert-deftest agent-repl-itest-fixture-state-dir-sweep-signals-a-failed-removal ()
  "The per-scenario state-root sweep fails on a leftover it cannot remove."
  ;; Arrange
  (let* ((root (make-temp-file "state-dir-test-" t))
         (daemon (agent-repl-itest--make-daemon :state-dir root)))
    (make-directory (expand-file-name "leftover" root))
    (unwind-protect
        (cl-letf (((symbol-function 'delete-directory)
                   #'agent-repl-itest-fixture--refuse-removal))
          ;; Act / Assert
          (should-error (agent-repl-itest--sweep-state-dir daemon) :type 'file-error))
      (delete-directory root t))))

(ert-deftest agent-repl-itest-fixture-exit-time-fixture-root-failure-is-printed ()
  "A fixture root that cannot be removed at exit is reported on stderr."
  ;; Arrange
  (let ((agent-repl-itest--fixture-root (make-temp-file "fixture-root-test-" t))
        (printed ""))
    (unwind-protect
        (cl-letf (((symbol-function 'delete-directory)
                   #'agent-repl-itest-fixture--refuse-removal)
                  ((symbol-function 'princ)
                   (lambda (object &optional _stream) (setq printed (concat printed object)))))
          ;; Act
          (agent-repl-itest--delete-fixture-root))
      (delete-directory agent-repl-itest--fixture-root t))
    ;; Assert
    (should (string-match-p "ERROR: could not remove the fixture root" printed))))

(ert-deftest agent-repl-itest-fixture-exit-time-fixture-root-removal-removes-it ()
  "The exit-time removal deletes the fixture root and what is under it."
  ;; Arrange
  (let ((agent-repl-itest--fixture-root (make-temp-file "fixture-root-test-" t)))
    (make-directory (expand-file-name "left/behind" agent-repl-itest--fixture-root) t)
    ;; Act
    (agent-repl-itest--delete-fixture-root)
    ;; Assert
    (should-not (file-exists-p agent-repl-itest--fixture-root))))

(provide 'test-integration-fixture)
;;; test-integration-fixture.el ends here
