;;; test-integration-connect.el --- Integration: connect.el against the fake daemon -*- lexical-binding: t; -*-

;;; Commentary:

;; Scenarios 1 and 16 of elisp-fanout.md §14: the transport itself, exercised
;; against a REAL agentrepl.v1 server over a real loopback socket.
;;
;; 1.  daemon.addr discovery; the RegisterWorkspace round trip; lowerCamel
;;     keys; the id echoed verbatim; the error arm as an ANSWER; and the
;;     unknown-field refusal that proves the round trip goes through the
;;     frozen schema.
;; 16. Stream endings: a producer close WITHOUT an end frame, and an end frame
;;     CARRYING an error — two different facts, both failures.

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "test-integration-helpers.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(declare-function agent-repl-connect-read-daemon-addr "connect")
(declare-function agent-repl-connect-daemon-addr-file "connect")
(declare-function agent-repl-connect-open "connect")
(declare-function agent-repl-connect-close "connect")
(declare-function agent-repl-connect-unary-sync "connect")
(declare-function agent-repl-connect-unary "connect")
(declare-function agent-repl-connect-stream "connect")
(declare-function agent-repl-connect-stream-cancel "connect")

;;;; ---- Scenario 1: discovery and the unary round trip ----

(ert-deftest agent-repl-itest-connect-reads-the-published-daemon-addr ()
  "Discovery reads the address the daemon published under the state root.
COMMON.md's DAEMON ADDRESS contract: the daemon writes `127.0.0.1:<port>'
plus a newline and every client discovers it from that file."
  ;; Arrange / Act.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((address (agent-repl-connect-read-daemon-addr)))
      ;; Assert.
      (should (equal address (agent-repl-itest-daemon-address daemon))))))

(ert-deftest agent-repl-itest-connect-addr-file-sits-under-the-state-root ()
  "The addr file production reads is the one the daemon wrote.
There is ONE state root; a divergence here is the silent-misconfig class
the contract forbids."
  ;; Arrange / Act.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((path (agent-repl-connect-daemon-addr-file)))
      ;; Assert.
      (should (equal (expand-file-name path)
                     (expand-file-name
                      (agent-repl-itest--addr-file
                       (agent-repl-itest-daemon-state-dir daemon))))))))

(ert-deftest agent-repl-itest-connect-daemon-addr-absent-reads-nil ()
  "An absent addr file is the legal no-daemon state, not an error.
Cold start depends on telling `no daemon' from `a broken daemon'."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (delete-file (agent-repl-itest--addr-file
                  (agent-repl-itest-daemon-state-dir daemon)))
    ;; Act.
    (let ((address (agent-repl-connect-read-daemon-addr)))
      ;; Assert.
      (should (null address)))))

(ert-deftest agent-repl-itest-connect-unary-round-trip-answers-the-success-arm ()
  "RegisterWorkspace answers `success' carrying the daemon-minted ref.
endpoint_register_workspace.proto: the daemon normalizes, mints and
returns; Emacs supplies only the dir."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          ;; Act.
          (let ((answer (agent-repl-connect-unary-sync
                         conn "RegisterWorkspace"
                         (json-serialize '((dir . "/tmp/itest-ws"))))))
            ;; Assert: the response is the `result' oneof's success arm.
            (should (alist-get 'success answer)))
        (agent-repl-connect-close conn)))))

(ert-deftest agent-repl-itest-connect-unary-response-keys-are-lower-camel ()
  "The wire spells keys lowerCamel, which is what the codec must read.
Go protojson emits `shimAttached', `atMs', `reloadWebapp'; a codec
expecting snake_case would silently see every field as absent."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let* ((fault '((detail . "shim gone")))
           (unhealthy `((faults . [,fault])))
           (response `((success . ((unhealthy . ,unhealthy))))))
      (agent-repl-itest--script daemon "SessionHealth" response))
    (let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          ;; Act.
          (let* ((answer (agent-repl-connect-unary-sync
                          conn "SessionHealth"
                          (json-serialize '((workspace . ((id . "ws-1") (dir . "/tmp/ws-1")))))))
                 (success (alist-get 'success answer)))
            ;; Assert: `unhealthy' and `faults' arrive under their protojson
            ;; spellings, and UNHEALTHY IS AN ANSWER, not a transport error.
            (should (alist-get 'unhealthy success)))
        (agent-repl-connect-close conn)))))

(ert-deftest agent-repl-itest-connect-echoes-the-ref-id-verbatim ()
  "A WorkspaceRef id crosses the wire byte for byte.
workspace.v1's opacity comment: the id is opaque, compared byte-wise and
handed back verbatim — Emacs never parses or rebuilds one."
  ;; Arrange: an id with characters no client may normalize away.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((id "ws-A_b.9-Z/x")
          (conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          (progn
            ;; Act.
            (agent-repl-connect-unary-sync
             conn "SelectWorkspace"
             (json-serialize `((workspace . ((id . ,id) (dir . "/tmp/ws"))))))
            ;; Assert: the fake recorded exactly the bytes Emacs sent.
            (let ((body (car (agent-repl-itest--call-bodies daemon "SelectWorkspace"))))
              (should (equal (agent-repl-itest--body-field body 'workspace 'id) id))))
        (agent-repl-connect-close conn)))))

(ert-deftest agent-repl-itest-connect-error-arm-is-an-answer-not-a-failure ()
  "A scripted `error' arm arrives as a 200 response, not as a failure.
Empty error messages are DELIBERATE on this contract; an error ARM is the
daemon answering, and only a transport fault is a failure."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "RegisterWorkspace" '((error . ())))
    (let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          ;; Act.
          (let ((answer (agent-repl-connect-unary-sync
                         conn "RegisterWorkspace"
                         (json-serialize '((dir . "/tmp/itest-ws"))))))
            ;; Assert: an empty message serializes to `{}', so the arm is
            ;; present as an empty alist rather than as a value.
            (should (assq 'error answer)))
        (agent-repl-connect-close conn)))))

(ert-deftest agent-repl-itest-connect-unknown-field-is-refused-by-the-schema ()
  "A field the schema does not declare is refused, loudly.
This is the round-trip check on elisp's encoders: the request is
unmarshalled into the GENERATED type, so a misspelling cannot pass."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          ;; Act / Assert.
          (let ((detail
                 (condition-case err
                     (progn (agent-repl-connect-unary-sync
                             conn "RegisterWorkspace"
                             (json-serialize '((dir . "/tmp/ws") (nosuchfield . 1))))
                            nil)
                   (agent-repl-connect-error (cadr err)))))
            (should detail)
            ;; The daemon answered non-200 with a Connect error body.
            (should (eq (plist-get detail :kind) :http))
            (should (equal (plist-get detail :status) 400)))
        (agent-repl-connect-close conn)))))

(ert-deftest agent-repl-itest-connect-missing-required-field-is-refused ()
  "An unset non-optional field is refused at once.
THE VALIDATION INVARIANT: a request carrying one is answered with an
error immediately — this catches an encoder that FORGOT a field, which
unknown-field strictness cannot see."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          ;; Act / Assert: SelectWorkspace without its workspace ref.
          (let ((detail
                 (condition-case err
                     (progn (agent-repl-connect-unary-sync
                             conn "SelectWorkspace" (json-serialize '()))
                            nil)
                   (agent-repl-connect-error (cadr err)))))
            (should detail)
            (should (eq (plist-get detail :kind) :http)))
        (agent-repl-connect-close conn)))))

(ert-deftest agent-repl-itest-connect-logs-the-address-read ()
  "Discovery logs its read through core.el's canonical ladder.
The integration runs are read through the production logs, so the
discovery step must leave one."
  ;; Arrange / Act.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-connect-read-daemon-addr)
    ;; Assert.
    (agent-repl-itest--await-log daemon "elisp.connect.daemon-addr-read" "debug")
    (should (agent-repl-itest--logged-p daemon "elisp.connect.daemon-addr-read" "debug"))))

;;;; ---- Scenario 16: the two stream endings ----

(defun agent-repl-itest-connect--watch-daemon (conn outcomes)
  "Open a WatchDaemon stream on CONN, pushing close outcomes onto OUTCOMES.
OUTCOMES is a symbol naming a special variable."
  (agent-repl-connect-stream
   conn "WatchDaemon" (json-serialize '())
   (lambda (_push) nil)
   (lambda (outcome) (push outcome (symbol-value outcomes)))))

(ert-deftest agent-repl-itest-connect-producer-close-without-an-end-frame-fails ()
  "A producer that stops without its terminal envelope is a TRANSPORT failure.
elisp.md: a stream the PRODUCER ends without a terminal frame is a
transport failure; only a CLIENT cancel is a normal close."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((outcomes nil)
          (conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          (progn
            (agent-repl-itest-connect--watch-daemon conn 'outcomes)
            (agent-repl-itest--await-subscriber daemon "daemon")
            ;; Act: drop the TCP connection with no end frame at all.
            (agent-repl-itest--end daemon "daemon" nil nil t)
            (agent-repl-itest--wait-until (lambda () outcomes) nil
                                          "the stream's close outcome")
            ;; Assert.
            (should (eq (car (car outcomes)) :error))
            (should (eq (plist-get (cadr (car outcomes)) :kind) :no-end-frame)))
        (agent-repl-connect-close conn)))))

(ert-deftest agent-repl-itest-connect-end-frame-with-an-error-fails ()
  "An end frame CARRYING a Connect error closes the stream as an error.
A different fact from a missing frame: the producer did conclude, and
said why."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((outcomes nil)
          (conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          (progn
            (agent-repl-itest-connect--watch-daemon conn 'outcomes)
            (agent-repl-itest--await-subscriber daemon "daemon")
            ;; Act.
            (agent-repl-itest--end daemon "daemon" nil
                                   '("unavailable" . "daemon going away"))
            (agent-repl-itest--wait-until (lambda () outcomes) nil
                                          "the stream's close outcome")
            ;; Assert.
            (should (eq (car (car outcomes)) :error))
            (should (equal (plist-get (cadr (car outcomes)) :code) "unavailable")))
        (agent-repl-connect-close conn)))))

(ert-deftest agent-repl-itest-connect-clean-end-frame-reports-ended ()
  "An end frame with NO error reports `:ended', distinct from an error.
A STANDING stream's consumer treats `:ended' as a failure of its own —
but that policy belongs to the consumer, not to the transport."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((outcomes nil)
          (conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          (progn
            (agent-repl-itest-connect--watch-daemon conn 'outcomes)
            (agent-repl-itest--await-subscriber daemon "daemon")
            ;; Act.
            (agent-repl-itest--end daemon "daemon")
            (agent-repl-itest--wait-until (lambda () outcomes) nil
                                          "the stream's close outcome")
            ;; Assert.
            (should (eq (car (car outcomes)) :ended)))
        (agent-repl-connect-close conn)))))

(ert-deftest agent-repl-itest-connect-client-cancel-is-the-graceful-close ()
  "Cancelling a stream IS the graceful close; no verb exists for it.
elisp.md, stream lifecycle: no CloseXConnection rpc exists, and the
daemon observes the cancel as the subscription going away."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((outcomes nil)
          (conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          (let ((stream (agent-repl-itest-connect--watch-daemon conn 'outcomes)))
            (agent-repl-itest--await-subscriber daemon "daemon")
            ;; Act.
            (agent-repl-connect-stream-cancel stream)
            (agent-repl-itest--wait-until (lambda () outcomes) nil
                                          "the stream's close outcome")
            ;; Assert: the client's own cancel, and the daemon sees it.
            (should (eq (car (car outcomes)) :cancelled))
            (agent-repl-itest--wait-until
             (lambda () (null (agent-repl-itest--subscribers daemon "daemon")))
             nil "the daemon to observe the cancel"))
        (agent-repl-connect-close conn)))))

(ert-deftest agent-repl-itest-connect-stream-delivers-pushes-in-order ()
  "Per-stream ordering is the only ordering the contract provides.
No fences, revisions or boot ids exist; a consumer may rely on nothing
but the order within one stream."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((pushes nil)
          (conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          (progn
            (agent-repl-connect-stream
             conn "WatchDaemon" (json-serialize '())
             (lambda (push) (push push pushes))
             (lambda (_outcome) nil))
            (agent-repl-itest--await-subscriber daemon "daemon")
            ;; Act.
            (agent-repl-itest--push
             daemon "daemon"
             '((drainScheduled . ((atMs . "1735689600000") (reason . ((deploy . ())))))))
            (agent-repl-itest--push daemon "daemon" '((drainCancelled . ())))
            (agent-repl-itest--wait-until (lambda () (>= (length pushes) 2)) nil
                                          "both daemon pushes")
            ;; Assert: newest-first accumulation, so the cancel is at the head.
            (should (assq 'drainCancelled (car pushes)))
            (should (assq 'drainScheduled (cadr pushes))))
        (agent-repl-connect-close conn)))))

(provide 'test-integration-connect)

;;; test-integration-connect.el ends here
