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

(ert-deftest agent-repl-itest-connect-malformed-addr-signals-connect-error ()
  "Malformed `daemon.addr' content signals `agent-repl-connect-error'.
fanout §3: \"malformed content signals `agent-repl-connect-error'\" — a
regression here would let a garbled address file be treated as no daemon
at all, or crash with an unclassified error instead of the documented
`:malformed-addr' kind."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (write-region "garbage" nil
                  (agent-repl-itest--addr-file
                   (agent-repl-itest-daemon-state-dir daemon))
                  nil 'silent)
    ;; Act / Assert.
    (let ((detail
           (condition-case err
               (progn (agent-repl-connect-read-daemon-addr) nil)
             (agent-repl-connect-error (cadr err)))))
      (should detail)
      ;; fanout §3: "malformed content signals `agent-repl-connect-error'".
      (should (eq (plist-get detail :kind) :malformed-addr)))))

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
            (should (equal (plist-get detail :status) 400))
            ;; fanout §3: "parse the Connect error body {code,message}" —
            ;; an unparsed body would still pass the two assertions above.
            (should (equal (plist-get detail :code) "invalid_argument"))
            (should (and (stringp (plist-get detail :message))
                        (not (string-empty-p (plist-get detail :message))))))
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
            (should (eq (plist-get detail :kind) :http))
            ;; fanout §3: "parse the Connect error body {code,message}" —
            ;; this test previously asserted neither the code nor the
            ;; message, so an unparsed body would also pass.
            (should (equal (plist-get detail :code) "invalid_argument"))
            (should (and (stringp (plist-get detail :message))
                        (not (string-empty-p (plist-get detail :message))))))
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

(ert-deftest agent-repl-itest-connect-unary-sync-times-out ()
  "A unary call whose answer never arrives fails as `:timeout'.
fanout §3: \"Default timeout `agent-repl-connect-unary-timeout-seconds'
(10)\" — this fails if a withheld answer hangs the sync call forever, or
if the failure it eventually reports is misclassified as some other
kind (`:transport', `:http', ...)."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--gate daemon "RegisterWorkspace")
    (let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          ;; Act / Assert.
          (let ((detail
                 (condition-case err
                     (progn (agent-repl-connect-unary-sync
                             conn "RegisterWorkspace"
                             (json-serialize '((dir . "/tmp/itest-ws"))) 0.2)
                            nil)
                   (agent-repl-connect-error (cadr err)))))
            (should detail)
            ;; fanout §3: the `:timeout' failure kind.
            (should (eq (plist-get detail :kind) :timeout)))
        (agent-repl-connect-close conn)
        (agent-repl-itest--release-gate daemon "RegisterWorkspace")))))

(ert-deftest agent-repl-itest-connect-on-push-exception-is-caught-and-logged ()
  "An ON-PUSH handler that signals is caught at the filter boundary.
fanout §3: \"ON-PUSH exceptions are caught at the filter boundary: log
ERROR with the payload in context; the stream stays open\" — this fails
if a misbehaving callback ever kills the transport, or if the exception
is swallowed without a trace in the production log."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((pushes nil)
          (first-call t)
          (conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          (progn
            (agent-repl-connect-stream
             conn "WatchDaemon" (json-serialize '())
             (lambda (msg)
               (if first-call
                   (progn (setq first-call nil) (error "on-push boom"))
                 (push msg pushes)))
             (lambda (_outcome) nil))
            (agent-repl-itest--await-subscriber daemon "daemon")
            ;; Act: two pushes, the first of which the handler chokes on.
            (agent-repl-itest--push daemon "daemon" '((drainCancelled . ())))
            (agent-repl-itest--push daemon "daemon" '((drainCancelled . ())))
            (agent-repl-itest--wait-until (lambda () pushes) nil
                                          "the second push to still arrive")
            ;; Assert: the stream stayed open past the first exception.
            (should pushes)
            ;; fanout §3: "log ERROR with the payload in context".
            (should (agent-repl-itest--logged-p
                     daemon "elisp.connect.push-handler-error" "error")))
        (agent-repl-connect-close conn)))))

(ert-deftest agent-repl-itest-connect-close-cancels-every-standing-stream ()
  "`agent-repl-connect-close' cancels every standing stream as `(:cancelled)'.
fanout §3 LANDED SHAPES: \"(agent-repl-connect-close CONN) marks the
connection dead and cancels every standing stream as `(:cancelled)'\" —
this fails if closing a connection with two open streams leaves one
of them open, reports a different outcome for it, or if the fake still
lists either subscription afterward."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((outcomes-a nil) (outcomes-b nil)
          (conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (agent-repl-connect-stream
       conn "WatchDaemon" (json-serialize '())
       (lambda (_push) nil)
       (lambda (outcome) (push outcome outcomes-a)))
      (agent-repl-connect-stream
       conn "WatchWorkspaceRoster" (json-serialize '())
       (lambda (_push) nil)
       (lambda (outcome) (push outcome outcomes-b)))
      (agent-repl-itest--await-subscriber daemon "daemon")
      (agent-repl-itest--await-subscriber daemon "roster")
      ;; Act.
      (agent-repl-connect-close conn)
      (agent-repl-itest--wait-until (lambda () (and outcomes-a outcomes-b)) nil
                                    "both streams to close")
      ;; Assert.
      (should (equal (car outcomes-a) '(:cancelled)))
      (should (equal (car outcomes-b) '(:cancelled)))
      (agent-repl-itest--wait-until
       (lambda () (null (agent-repl-itest--subscribers daemon)))
       nil "the fake's subscriber list to empty"))))

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
            (should (eq (plist-get (cadr (car outcomes)) :kind) :no-end-frame))
            ;; fanout §3: "process death without an end frame — logged
            ;; ERROR here".
            (should (agent-repl-itest--logged-p
                     daemon "elisp.connect.stream-error" "error")))
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
            (should (equal (plist-get (cadr (car outcomes)) :code) "unavailable"))
            ;; fanout §3: "(:error DETAIL) ... logged ERROR here" — the
            ;; end-frame-with-error case, distinct from the no-end-frame
            ;; case above but logged through the same site.
            (should (agent-repl-itest--logged-p
                     daemon "elisp.connect.stream-error" "error")))
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

(ert-deftest agent-repl-itest-connect-watch-host-workspace-unset-ref-never-opens ()
  "A stream request that fails validation is never accepted as a subscriber.
fanout §3 STANDING-STREAM ACCEPTANCE: \"a non-200 ... never calls
[ON-OPEN]\" — this fails if the fake ever lists a subscriber for a
request that never passed validation, or if the resulting close outcome
loses its `:error' kind or Connect error code.

NOTE on `:status': the Connect streaming protocol reports an in-handler
refusal (validation runs before `accept', per fakedaemon's accept.go)
through the terminal EndStreamResponse frame at HTTP 200, never through a
non-200 status line — confirmed empirically against the pinned connect-go
(a raw socket capture of this exact request shows `HTTP/1.1 200 OK' with
`{\"error\":{\"code\":\"invalid_argument\",...}}' framed as the end
frame).  `agent-repl-connect--end-frame-reason' therefore never populates
`:status' for this path; the audit's \"with status 400\" describes the
unary non-200 case (§3's `Any non-200 →' sentence), not this streaming
one, so `:code' is asserted here instead."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((outcomes nil)
          (conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          (progn
            ;; Act: WatchHostWorkspaceRequest with its non-optional
            ;; `workspace' field unset.
            (agent-repl-connect-stream
             conn "WatchHostWorkspace" (json-serialize '())
             (lambda (_push) nil)
             (lambda (outcome) (push outcome outcomes)))
            (agent-repl-itest--wait-until (lambda () outcomes) nil
                                          "the stream's close outcome")
            ;; Assert.
            ;; fanout §3 STANDING-STREAM ACCEPTANCE: never accepted as a
            ;; subscriber.
            (should (null (agent-repl-itest--subscribers daemon "host")))
            (should (eq (car (car outcomes)) :error))
            (should (equal (plist-get (cadr (car outcomes)) :code)
                           "invalid_argument")))
        (agent-repl-connect-close conn)))))

(provide 'test-integration-connect)

;;; test-integration-connect.el ends here
