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

(defconst agent-repl-itest-connect--watch-daemon-body
  (json-serialize '((emacs . ((elispBuild . "itest-elisp-build")))))
  "The WatchDaemon body these transport scenarios send.
The request names its client and an Emacs client states its elisp build:
the daemon (and the fake) refuse one that does not, and these scenarios
are about the transport, not that refusal.")

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
             conn "WatchDaemon" agent-repl-itest-connect--watch-daemon-body
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
       conn "WatchDaemon" agent-repl-itest-connect--watch-daemon-body
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

(defun agent-repl-itest-connect--watch-daemon (conn on-close)
  "Open a WatchDaemon stream on CONN, handing close outcomes to ON-CLOSE.
ON-CLOSE is a FUNCTION, not a symbol: this file is lexically bound, so a
`symbol-value' indirection would miss the caller's `let' binding entirely
and every close assertion would wait on a variable nothing ever wrote."
  (agent-repl-connect-stream
   conn "WatchDaemon" agent-repl-itest-connect--watch-daemon-body
   (lambda (_push) nil)
   on-close))

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
            (agent-repl-itest-connect--watch-daemon
             conn (lambda (outcome) (push outcome outcomes)))
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
            (agent-repl-itest-connect--watch-daemon
             conn (lambda (outcome) (push outcome outcomes)))
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
            (agent-repl-itest-connect--watch-daemon
             conn (lambda (outcome) (push outcome outcomes)))
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
          (let ((stream (agent-repl-itest-connect--watch-daemon
             conn (lambda (outcome) (push outcome outcomes)))))
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
             conn "WatchDaemon" agent-repl-itest-connect--watch-daemon-body
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

;;;; ---- Audit-2 additions (R-SUITE-2) ----
;;
;; Findings 43-45 of docs/overhaul/reports/elisp-suite-audit-2.md.  Kept in
;; their own section so they merge cleanly beside concurrent edits above.

;; audit-2 #43
(ert-deftest agent-repl-itest-connect-unary-request-carries-the-connect-headers ()
  "A unary request carries `Content-Type: application/json' and the version.
fanout §3 fixes the headers, not only the body: a client that sent the
wrong content type — or dropped `Connect-Protocol-Version: 1' — spoke a
protocol the daemon is not obliged to answer, and neither the parsed body
nor the verbatim raw body can see the difference."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          (progn
            ;; Act.
            (agent-repl-connect-unary-sync
             conn "RegisterWorkspace" (json-serialize '((dir . "/tmp/itest-ws"))))
            ;; Assert.
            (should (equal (agent-repl-itest--call-header
                            daemon "RegisterWorkspace" "Content-Type")
                           "application/json"))
            (should (equal (agent-repl-itest--call-header
                            daemon "RegisterWorkspace" "Connect-Protocol-Version")
                           "1")))
        (agent-repl-connect-close conn)))))

;; audit-2 #43
(ert-deftest agent-repl-itest-connect-stream-request-carries-the-streaming-content-type ()
  "A stream request carries `Content-Type: application/connect+json'.
fanout §3: the streaming content type is a DIFFERENT one from the unary
call's, because the body is a framed envelope sequence rather than a bare
JSON object.  Sending the unary type on a stream is a protocol error the
body alone cannot reveal."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          (progn
            ;; Act.
            (agent-repl-itest-connect--watch-daemon conn (lambda (_outcome) nil))
            (agent-repl-itest--await-call daemon "WatchDaemon")
            ;; Assert.
            (should (equal (agent-repl-itest--call-header
                            daemon "WatchDaemon" "Content-Type")
                           "application/connect+json"))
            (should (equal (agent-repl-itest--call-header
                            daemon "WatchDaemon" "Connect-Protocol-Version")
                           "1")))
        (agent-repl-connect-close conn)))))

;; audit-2 #44
(ert-deftest agent-repl-itest-connect-webapp-only-stream-is-never-accepted ()
  "A stream this fake does not mock is refused, never accepted as a subscriber.
fakedaemon's README: \"Every other `agentrepl.v1' stream belongs to the
webapp; the fake answers those `unimplemented' so a wrong caller fails
loudly instead of hanging.\"  A wrong caller must therefore see a CLOSE
carrying that code and leave no subscription behind.

NOTE on ON-OPEN, for the same reason the unset-ref test records: the
pinned connect-go reports an in-handler refusal through the terminal
`EndStreamResponse' frame at HTTP 200, so the header block does parse as
200 on this path and the acceptance callback is not what distinguishes
it.  The Connect error CODE and the absent subscriber are."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((outcomes nil)
          (conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          (progn
            ;; Act: a webapp stream, opened from Emacs's transport.
            (agent-repl-connect-stream
             conn "WatchFeed" (json-serialize '())
             (lambda (_push) nil)
             (lambda (outcome) (push outcome outcomes)))
            (agent-repl-itest--wait-until (lambda () outcomes) nil
                                          "the stream's close outcome")
            ;; Assert.
            (should (eq (car (car outcomes)) :error))
            (should (equal (plist-get (cadr (car outcomes)) :code) "unimplemented"))
            (should (null (agent-repl-itest--subscribers daemon))))
        (agent-repl-connect-close conn)))))

;; audit-2 #45
(ert-deftest agent-repl-itest-connect-timed-out-unary-is-never-retried ()
  "A unary call that timed out is NOT retried when the daemon answers late.
fanout §3: \"Never retried here.\"  A retry at the transport would turn
one submission into two turns on a daemon whose answer was merely slow —
which is exactly why every retry decision lives above this layer, with
the idempotency key that makes it safe."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--gate daemon "RegisterWorkspace")
    (let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          (progn
            ;; Act: the answer is withheld past the call's own timeout.
            (should-error
             (agent-repl-connect-unary-sync
              conn "RegisterWorkspace" (json-serialize '((dir . "/tmp/itest-ws"))) 0.2)
             :type 'agent-repl-connect-error)
            ;; The daemon finally answers every held call.
            (agent-repl-itest--release-gate daemon "RegisterWorkspace")
            ;; Assert: exactly ONE request ever reached the daemon.
            (should (equal 1 (length (agent-repl-itest--calls
                                      daemon "RegisterWorkspace")))))
        (agent-repl-connect-close conn)))))

;;;; ---- Audit-3 additions (R-SUITE-3) ----
;;
;; Findings 1-3 of docs/overhaul/reports/elisp-suite-audit-3.md.  Kept in
;; their own section so they merge cleanly beside concurrent edits above.

;; audit-3 #1
(ert-deftest agent-repl-itest-connect-unary-on-a-closed-connection-never-reaches-the-wire ()
  "A unary call on a CLOSED connection fails loudly and sends nothing.
elisp.md \"unary calls fail loudly\"; fanout §3: `agent-repl-connect-close'
marks the connection dead (connect.el `--check-alive').  This fails if a
call on a dead connection is quietly dropped, if it is misclassified as
some kind other than `:transport', or -- the fact only the fake can see --
if the request reaches the daemon anyway."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (agent-repl-connect-close conn)
      ;; Act.
      (let ((detail
             (condition-case err
                 (progn (agent-repl-connect-unary-sync
                         conn "RegisterWorkspace"
                         (json-serialize '((dir . "/tmp/itest-ws"))))
                        nil)
               (agent-repl-connect-error (cadr err)))))
        ;; Assert.
        (should detail)
        (should (eq (plist-get detail :kind) :transport))
        ;; The wire was never touched.
        (should (null (agent-repl-itest--calls daemon "RegisterWorkspace")))
        (should (agent-repl-itest--logged-p
                 daemon "elisp.connect.call-on-closed-connection" "error"))))))

;; audit-3 #1
(ert-deftest agent-repl-itest-connect-stream-on-a-closed-connection-never-subscribes ()
  "Opening a stream on a CLOSED connection fails loudly and subscribes nobody.
The same `--check-alive' guard fronts `agent-repl-connect-stream', so a
dead connection can never stand a subscription -- a stream that slipped
past it would be a subscriber production believes it does not have."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (agent-repl-connect-close conn)
      ;; Act.
      (let ((detail
             (condition-case err
                 (progn (agent-repl-connect-stream
                         conn "WatchDaemon" agent-repl-itest-connect--watch-daemon-body
                         (lambda (_push) nil)
                         (lambda (_outcome) nil))
                        nil)
               (agent-repl-connect-error (cadr err)))))
        ;; Assert.
        (should detail)
        (should (eq (plist-get detail :kind) :transport))
        (should (null (agent-repl-itest--calls daemon "WatchDaemon")))
        (should (null (agent-repl-itest--subscribers daemon "daemon")))))))

;; audit-3 #2
(ert-deftest agent-repl-itest-connect-on-open-runs-once-before-the-first-push ()
  "ON-OPEN fires exactly once, and BEFORE any frame, on an accepted stream.
fanout §3 STANDING-STREAM ACCEPTANCE: the acceptance instant is the one
the consumer re-subscribes on, so it must precede the first push.  R-ACCEPT
pinned this in unit tests only; against a real accepted stream this fails
if ON-OPEN runs late (after the snapshot frame the daemon replays), runs
more than once, or never runs at all."
  ;; Arrange: a standing snapshot, so a frame is waiting the moment the
  ;; subscription is accepted.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--push
     daemon "daemon"
     '((drainScheduled . ((atMs . "1735689600000") (reason . ((deploy . ()))))))
     nil t)
    (let ((events nil)
          (conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          (progn
            ;; Act.
            (agent-repl-connect-stream
             conn "WatchDaemon" agent-repl-itest-connect--watch-daemon-body
             (lambda (_push) (push :push events))
             (lambda (_outcome) nil)
             (lambda () (push :open events)))
            (agent-repl-itest--wait-until
             (lambda () (memq :push events)) nil
             "the replayed drainScheduled snapshot to reach ON-PUSH")
            ;; Assert: newest-first, so the tail is the acceptance instant.
            (should (equal (reverse events) '(:open :push)))
            (should (equal 1 (seq-count (lambda (e) (eq e :open)) events))))
        (agent-repl-connect-close conn)))))

;; audit-3 #2
(ert-deftest agent-repl-itest-connect-on-open-runs-before-an-unset-ref-refusal ()
  "A stream refused at VALIDATION is refused AFTER acceptance, not instead of it.
fanout §3 STANDING-STREAM ACCEPTANCE says ON-OPEN fires on the HTTP 200
header block and that \"a non-200 ... never calls\" it.  A Connect
SERVER-STREAMING refusal is not a non-200: connect-go reports an
in-handler error through the terminal `EndStreamResponse' frame at HTTP
200 (see the NOTE on
`agent-repl-itest-connect-watch-host-workspace-unset-ref-never-opens',
which captured this off the socket).  So acceptance DOES happen here and
ON-OPEN runs exactly once; what marks the refusal is the CLOSE outcome's
Connect code and the absent subscriber.  This is the seam daemon-link
keys \"subscribed\" on, so it is pinned rather than left to be
rediscovered: ON-OPEN alone never proves a subscription stands."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((opened 0) (outcomes nil)
          (conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          (progn
            ;; Act: WatchHostWorkspaceRequest with its non-optional
            ;; `workspace' field unset.
            (agent-repl-connect-stream
             conn "WatchHostWorkspace" (json-serialize '())
             (lambda (_push) nil)
             (lambda (outcome) (push outcome outcomes))
             (lambda () (setq opened (1+ opened))))
            (agent-repl-itest--wait-until
             (lambda () outcomes) nil
             "the refused WatchHostWorkspace stream's close outcome")
            ;; Assert: refused with the validation code and nothing
            ;; subscribed -- while acceptance itself happened, exactly once.
            (should (eq (car (car outcomes)) :error))
            (should (equal (plist-get (cadr (car outcomes)) :code)
                           "invalid_argument"))
            (should (null (agent-repl-itest--subscribers daemon)))
            (should (equal 1 opened)))
        (agent-repl-connect-close conn)))))

;; audit-3 #2
(ert-deftest agent-repl-itest-connect-on-open-runs-before-an-unimplemented-refusal ()
  "A stream answered `unimplemented' is likewise refused AFTER acceptance.
The sibling of the validation refusal: a DIFFERENT refusal reason, the
same acceptance contract -- HTTP 200, one ON-OPEN, then an error end
frame and no subscriber left behind."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((opened 0) (outcomes nil)
          (conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          (progn
            ;; Act: a webapp-only stream.
            (agent-repl-connect-stream
             conn "WatchFeed" (json-serialize '())
             (lambda (_push) nil)
             (lambda (outcome) (push outcome outcomes))
             (lambda () (setq opened (1+ opened))))
            (agent-repl-itest--wait-until
             (lambda () outcomes) nil
             "the unimplemented WatchFeed stream's close outcome")
            ;; Assert.
            (should (equal (plist-get (cadr (car outcomes)) :code) "unimplemented"))
            (should (null (agent-repl-itest--subscribers daemon)))
            (should (equal 1 opened)))
        (agent-repl-connect-close conn)))))

;; audit-3 #3
(ert-deftest agent-repl-itest-connect-unary-request-negotiates-no-compression ()
  "A unary request offers and carries NO content encoding.
fanout §3: \"No compression negotiated.\"  An encoding the transport
quietly started advertising would let the daemon answer a compressed body
the reader does not decompress -- a failure that shows up as a parse error
far from its cause.  Only the recorded HEADERS can see this; neither the
parsed body nor the raw body can."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          (progn
            ;; Act.
            (agent-repl-connect-unary-sync
             conn "RegisterWorkspace" (json-serialize '((dir . "/tmp/itest-ws"))))
            ;; Assert.
            (dolist (header '("Connect-Accept-Encoding" "Connect-Content-Encoding"
                              "Accept-Encoding" "Content-Encoding"))
              (should (null (agent-repl-itest--call-header
                             daemon "RegisterWorkspace" header)))))
        (agent-repl-connect-close conn)))))

;; audit-3 #3
(ert-deftest agent-repl-itest-connect-stream-request-negotiates-no-compression ()
  "A stream request offers and carries no content encoding either.
The streaming half of fanout §3's \"No compression negotiated\": the
framed envelope sequence has its OWN encoding headers, so the unary
assertion does not cover it."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          (progn
            ;; Act.
            (agent-repl-itest-connect--watch-daemon conn (lambda (_outcome) nil))
            (agent-repl-itest--await-call daemon "WatchDaemon")
            ;; Assert.
            (dolist (header '("Connect-Accept-Encoding" "Connect-Content-Encoding"
                              "Accept-Encoding" "Content-Encoding"))
              (should (null (agent-repl-itest--call-header
                             daemon "WatchDaemon" header)))))
        (agent-repl-connect-close conn)))))

(provide 'test-integration-connect)

;;; test-integration-connect.el ends here
