;;; test-failure.el --- Tests for failure.el -*- lexical-binding: t; -*-

;;; Commentary:

;; The classified-failure vocabulary (F4): the namespace partition, the
;; closed local type set, wire adoption, and the single surfacing point.
;; One edge case per test (AAA).

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "test-helpers.el" dir) nil t))

;;;; ---- Namespace partition ---------------------------------------------

(ert-deftest agent-repl-test-failure-local-types-carry-the-client-prefix ()
  "Every LOCAL failure type is in the frontend's reserved namespace.
A bare type minted here would mean Emacs re-classified something the
daemon already decided."
  ;; Act / Assert
  (dolist (type agent-repl-failure-local-types)
    (should (agent-repl-failure-client-type-p type))))

(ert-deftest agent-repl-test-failure-daemon-type-is-not-a-client-type ()
  "A daemon-owned type is recognized as NOT the frontend's.
The other direction of the partition: a `client.'-prefixed type on the
wire would mean a frontend's failure was laundered through the daemon."
  ;; Act / Assert
  (should-not (agent-repl-failure-client-type-p "shim.rejected")))

(ert-deftest agent-repl-test-failure-client-type-p-rejects-a-non-string ()
  "A non-string is not a client type (and does not error)."
  ;; Act / Assert
  (should-not (agent-repl-failure-client-type-p nil)))

(ert-deftest agent-repl-test-failure-client-type-p-logs-its-branch-inputs ()
  "Client-type classification records its value and both predicate outcomes."
  ;; Arrange
  (let (logs)
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (ws fmt &rest args)
                 (push (list ws (apply #'format fmt args)) logs))))
      ;; Act
      (agent-repl-failure-client-type-p "client.daemon_unreachable")
      (agent-repl-failure-client-type-p "shim.rejected")
      ;; Assert
      (should (= (length logs) 2))
      (should (cl-every (lambda (entry) (null (car entry))) logs))
      (should (string-match-p "client-type-p=nil" (cadr (car logs))))
      (should (string-match-p "client-type-p=t" (cadr (cadr logs)))))))

(ert-deftest agent-repl-test-failure-text-renders-typed-resume-transcript-action ()
  "Typed transcript evidence tells the user what to restore before retrying."
  (let ((failure (agent-repl-failure-from-wire
                  '(:message "resume failed"
                    :terminal ()
                    :kind
                    (:sessionResumeFailed
                     (:detail
                      (:claudeSessionId "c_authoritative" :cwd "/repo"
                       :automaticRestore ()
                       :transcriptUnavailable (:searchedPaths ("/cfg/a.jsonl")))))))))
    (should (equal (agent-repl-failure-text failure "ws1")
                   "Resume restoration for Claude session c_authoritative in /repo cannot continue: transcript unavailable at /cfg/a.jsonl. Restore that transcript, then retry."))))

(ert-deftest agent-repl-test-failure-text-renders-typed-resume-identity-action ()
  "Typed identity evidence rejects a replacement conversation actionably."
  (let ((failure (agent-repl-failure-from-wire
                  '(:message "resume failed"
                    :terminal ()
                    :kind
                    (:sessionResumeFailed
                     (:detail
                      (:claudeSessionId "c_authoritative" :cwd "/repo"
                       :create ()
                       :identityMismatch (:replacementClaudeSessionId "c_other"))))))))
    (should (string-match-p "recovery proposed Claude session c_other"
                            (agent-repl-failure-text failure "ws1")))))

(ert-deftest agent-repl-test-failure-text-renders-typed-resume-bring-up-restore ()
  "Typed bring-up-failure evidence renders the cause during a restoration."
  (let ((failure (agent-repl-failure-from-wire
                  '(:message "resume failed"
                    :terminal ()
                    :kind
                    (:sessionResumeFailed
                     (:detail
                      (:claudeSessionId "c_authoritative" :cwd "/repo"
                       :automaticRestore ()
                       :bringUpFailure (:cause "sdk query never became driveable"))))))))
    (should (equal (agent-repl-failure-text failure "ws1")
                   "Resume restoration for Claude session c_authoritative in /repo could not bring the session up: sdk query never became driveable."))))

(ert-deftest agent-repl-test-failure-text-renders-typed-resume-bring-up-create ()
  "Typed bring-up-failure evidence renders the cause during a creation."
  (let ((failure (agent-repl-failure-from-wire
                  '(:message "resume failed"
                    :terminal ()
                    :kind
                    (:sessionResumeFailed
                     (:detail
                      (:claudeSessionId "c_authoritative" :cwd "/repo"
                       :create ()
                       :bringUpFailure (:cause "process exited before init"))))))))
    (should (equal (agent-repl-failure-text failure "ws1")
                   "Resume creation for Claude session c_authoritative in /repo could not bring the session up: process exited before init."))))

(ert-deftest agent-repl-test-failure-text-renders-typed-resume-query-termination-iterator ()
  "Typed query-termination evidence renders an iterator failure's cause."
  (let ((failure (agent-repl-failure-from-wire
                  '(:message "resume failed"
                    :terminal ()
                    :kind
                    (:sessionResumeFailed
                     (:detail
                      (:claudeSessionId "c_authoritative" :cwd "/repo"
                       :automaticRestore ()
                       :queryTermination
                       (:iteratorFailure (:cause "stream reset by peer")))))))))
    (should (equal (agent-repl-failure-text failure "ws1")
                   "Resume restoration for Claude session c_authoritative in /repo could not continue: the resumed query terminated (the SDK iterator threw: stream reset by peer)."))))

(ert-deftest agent-repl-test-failure-text-renders-typed-resume-query-termination-eof ()
  "Typed query-termination evidence renders an unexpected-EOF termination
during a creation attempt."
  (let ((failure (agent-repl-failure-from-wire
                  '(:message "resume failed"
                    :terminal ()
                    :kind
                    (:sessionResumeFailed
                     (:detail
                      (:claudeSessionId "c_authoritative" :cwd "/repo"
                       :create ()
                       :queryTermination
                       (:unexpectedEof ()))))))))
    (should (equal (agent-repl-failure-text failure "ws1")
                   "Resume creation for Claude session c_authoritative in /repo could not continue: the resumed query terminated (the SDK iterator ended without an intentional shutdown)."))))

(ert-deftest agent-repl-test-failure-text-typed-resume-query-termination-zero-reasons-is-malformed ()
  "A query-termination cause with no recognized reason arm is malformed."
  (let ((failure (agent-repl-failure-from-wire
                  '(:message "resume failed"
                    :terminal ()
                    :kind
                    (:sessionResumeFailed
                     (:detail
                      (:claudeSessionId "c_authoritative" :cwd "/repo"
                       :automaticRestore ()
                       :queryTermination (:vendorSessionId "c_vendor"))))))))
    (should-error (agent-repl-failure-text failure "ws1"))))

(ert-deftest agent-repl-test-failure-text-typed-resume-two-causes-is-malformed ()
  "Two causes present at once is malformed, never a silent pick-one."
  (let ((failure (agent-repl-failure-from-wire
                  '(:message "resume failed"
                    :terminal ()
                    :kind
                    (:sessionResumeFailed
                     (:detail
                      (:claudeSessionId "c_authoritative" :cwd "/repo"
                       :automaticRestore ()
                       :transcriptUnavailable (:searchedPaths ("/cfg/a.jsonl"))
                       :bringUpFailure (:cause "sdk query never became driveable"))))))))
    (should-error (agent-repl-failure-text failure "ws1"))))

(ert-deftest agent-repl-test-failure-text-typed-resume-zero-causes-is-malformed ()
  "No recognized cause at all is malformed, never flattened to generic text."
  (let ((failure (agent-repl-failure-from-wire
                  '(:message "resume failed"
                    :terminal ()
                    :kind
                    (:sessionResumeFailed
                     (:detail
                      (:claudeSessionId "c_authoritative" :cwd "/repo"
                       :automaticRestore ())))))))
    (should-error (agent-repl-failure-text failure "ws1"))))

;;;; ---- Class decoding --------------------------------------------------

(ert-deftest agent-repl-test-failure-class-internal ()
  "ERROR_CLASS_INTERNAL decodes to `:internal'."
  ;; Act / Assert
  (should (eq (agent-repl-failure-class "ERROR_CLASS_INTERNAL") :internal)))

(ert-deftest agent-repl-test-failure-class-api ()
  "ERROR_CLASS_API decodes to `:api'."
  ;; Act / Assert
  (should (eq (agent-repl-failure-class "ERROR_CLASS_API") :api)))

(ert-deftest agent-repl-test-failure-class-unspecified-errors ()
  "An UNSPECIFIED class fails loudly rather than defaulting.
The class decides the failure's color, so a default would paint a failure
some arbitrary color — quietly, and possibly contradicting the workspace
colored beside it."
  ;; Act / Assert
  (should-error (agent-repl-failure-class "ERROR_CLASS_UNSPECIFIED")))

(ert-deftest agent-repl-test-failure-class-unknown-errors ()
  "A class outside the vocabulary fails loudly."
  ;; Act / Assert
  (should-error (agent-repl-failure-class "ERROR_CLASS_SOMETHING_NEW")))

(ert-deftest agent-repl-test-failure-class-logs-the-accepted-and-rejected-branches ()
  "Class validation logs both the selected class and a bad wire value."
  ;; Arrange
  (let (logs)
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (ws fmt &rest args)
                 (push (list ws (apply #'format fmt args)) logs))))
      ;; Act
      (agent-repl-failure-class "ERROR_CLASS_API")
      (should-error (agent-repl-failure-class "ERROR_CLASS_UNKNOWN"))
      ;; Assert
      (should (string-match-p "REJECTED unrecognized-error-class=\\\"ERROR_CLASS_UNKNOWN\\\""
                       (cadr (car logs))))
      (should (string-match-p "class=:api" (cadr (cadr logs)))))))

(ert-deftest agent-repl-test-failure-every-class-decodes ()
  "Every class keyword has a wire name that decodes back to it."
  ;; Act / Assert
  (dolist (class agent-repl-failure-classes)
    (should (rassq class agent-repl-failure-class-wire))))

;;;; ---- The FailureKind partition ---------------------------------------
;;
;; The three arm lists restate the `FailureKind' oneof, so they are asserted
;; against the checked-in generated bindings rather than against a fourth
;; hand-written copy: an arm added to the proto and forgotten here reaches
;; runtime as a failure with no side and therefore no color.

(defun agent-repl-test--failure-kind-arms ()
  "Return the protojson names of every `FailureKind' oneof arm."
  (agent-repl-test--generated-oneof-arms
   "agentshim/frontend/v1/errors.pb.go" "FailureKind"))

(ert-deftest agent-repl-test-failure-kind-partition-covers-every-wire-arm ()
  "Every `FailureKind' arm the proto declares has a side declared here."
  ;; Arrange
  (let ((declared (append agent-repl-failure-machinery-kinds
                          agent-repl-failure-vendor-kinds
                          agent-repl-failure-client-kinds)))
    ;; Act
    (let ((missing (cl-remove-if (lambda (arm) (member arm declared))
                                 (agent-repl-test--failure-kind-arms))))
      ;; Assert
      (should (null missing)))))

(ert-deftest agent-repl-test-failure-kind-partition-adds-no-unknown-arm ()
  "No side lists an arm the proto does not declare."
  ;; Arrange
  (let ((arms (agent-repl-test--failure-kind-arms))
        (declared (append agent-repl-failure-machinery-kinds
                          agent-repl-failure-vendor-kinds
                          agent-repl-failure-client-kinds)))
    ;; Act
    (let ((extra (cl-remove-if (lambda (arm) (member arm arms)) declared)))
      ;; Assert
      (should (null extra)))))

(ert-deftest agent-repl-test-failure-kind-partition-assigns-one-side-per-arm ()
  "An arm belongs to exactly one side; two sides would be two colors."
  ;; Arrange
  (let ((declared (append agent-repl-failure-machinery-kinds
                          agent-repl-failure-vendor-kinds
                          agent-repl-failure-client-kinds)))
    ;; Act / Assert
    (should (= (length declared) (length (delete-dups (copy-sequence declared)))))))

(ert-deftest agent-repl-test-failure-kind-class-machinery-is-internal ()
  "A machinery arm classifies `:internal'."
  ;; Act / Assert
  (should (eq (agent-repl-failure-kind-class "shimRejected") :internal)))

(ert-deftest agent-repl-test-failure-kind-class-vendor-is-api ()
  "A vendor arm classifies `:api'."
  ;; Act / Assert
  (should (eq (agent-repl-failure-kind-class "apiRateLimit") :api)))

(ert-deftest agent-repl-test-failure-kind-class-client-local-is-internal ()
  "A frontend-minted arm classifies `:internal': a frontend sees only its own
machinery fail, never the account."
  ;; Act / Assert
  (should (eq (agent-repl-failure-kind-class "daemonUnreachable") :internal)))

(ert-deftest agent-repl-test-failure-kind-class-unknown-arm-errors ()
  "An arm outside the partition fails loudly rather than taking a side."
  ;; Act / Assert
  (should-error (agent-repl-failure-kind-class "somethingNew")))

(ert-deftest agent-repl-test-failure-kind-arm-reads-the-set-arm ()
  "The set arm's protojson name is recovered from a decoded kind."
  ;; Act / Assert
  (should (equal (car (agent-repl-failure-kind-arm '(:shimRejected (:reason "no"))))
                 "shimRejected")))

(ert-deftest agent-repl-test-failure-kind-arm-reads-the-arm-payload ()
  "The arm's own typed payload rides out beside its name."
  ;; Act / Assert
  (should (equal (cdr (agent-repl-failure-kind-arm '(:shimRejected (:reason "no"))))
                 '(:reason "no"))))

(ert-deftest agent-repl-test-failure-kind-arm-unset-errors ()
  "An unset kind is a failure nobody named, never a default one."
  ;; Act / Assert
  (should-error (agent-repl-failure-kind-arm nil)))

(ert-deftest agent-repl-test-failure-kind-arm-two-arms-errors ()
  "Two set arms is two failures claiming one card, never a pick-the-first."
  ;; Act / Assert
  (should-error (agent-repl-failure-kind-arm '(:shimRejected () :apiRateLimit ()))))

;;;; ---- Wire adoption ---------------------------------------------------

(ert-deftest agent-repl-test-failure-from-wire-reads-the-class-off-the-kind ()
  "The card's side comes from which kind arm is set, not from a class field."
  ;; Arrange / Act
  (let ((f (agent-repl-failure-from-wire
            '(:kind (:apiRateLimit ()) :message "rate limited" :terminal ()))))
    ;; Assert
    (should (eq (plist-get f :class) :api))))

(ert-deftest agent-repl-test-failure-from-wire-adopts-the-kind-as-the-type ()
  "The kind arm's name IS the failure's type now that error_type left the wire."
  ;; Arrange / Act
  (let ((f (agent-repl-failure-from-wire
            '(:kind (:apiRateLimit ()) :message "rate limited" :terminal ()))))
    ;; Assert
    (should (equal (plist-get f :type) "apiRateLimit"))))

(ert-deftest agent-repl-test-failure-from-wire-adopts-the-sentence ()
  "The daemon-composed sentence rides through verbatim."
  ;; Arrange / Act
  (let ((f (agent-repl-failure-from-wire
            '(:kind (:apiRateLimit ()) :message "rate limited" :terminal ()))))
    ;; Assert
    (should (equal (plist-get f :message) "rate limited"))))

(ert-deftest agent-repl-test-failure-from-wire-defaults-absent-fields ()
  "Absent optional fields become empty rather than nil.
protojson omits a zero/empty field, so an absent detail is the common
case, not a malformed frame."
  ;; Arrange / Act
  (let ((f (agent-repl-failure-from-wire
            '(:kind (:shimRejected ()) :message "rejected" :open ()))))
    ;; Assert
    (should (equal (plist-get f :detail) ""))
    (should (equal (plist-get f :resolved-at) 0))))

(ert-deftest agent-repl-test-failure-from-wire-an-open-card-is-not-resolved ()
  "The `open' lifecycle arm reads as unresolved."
  ;; Arrange / Act
  (let ((f (agent-repl-failure-from-wire
            '(:kind (:shimDegraded ()) :message "no traffic" :open ()))))
    ;; Assert
    (should-not (plist-get f :resolved))))

(ert-deftest agent-repl-test-failure-from-wire-a-terminal-card-is-not-resolved ()
  "A `terminal' card has no closing edge, so it is not a resolution."
  ;; Arrange / Act
  (let ((f (agent-repl-failure-from-wire
            '(:kind (:apiRefusal ()) :message "refused" :terminal ()))))
    ;; Assert
    (should-not (plist-get f :resolved))))

(ert-deftest agent-repl-test-failure-from-wire-a-resolved-card-is-resolved ()
  "The `resolved' ARM is what makes a card settled."
  ;; Arrange / Act
  (let ((f (agent-repl-failure-from-wire
            '(:kind (:shimDegraded ()) :message "no traffic"
              :resolved (:resolvedAtMs 1700000000000)))))
    ;; Assert
    (should (plist-get f :resolved))))

(ert-deftest agent-repl-test-failure-from-wire-carries-the-resolution-instant ()
  "A resolved window's stamp rides through."
  ;; Arrange / Act
  (let ((f (agent-repl-failure-from-wire
            '(:kind (:shimDegraded ()) :message "no traffic"
              :resolved (:resolvedAtMs 1700000000000)))))
    ;; Assert
    (should (equal (plist-get f :resolved-at) 1700000000000))))

(ert-deftest agent-repl-test-failure-from-wire-resolved-at-instant-zero-is-resolved ()
  "A card settled at instant zero is settled: the ARM is the verdict.
Reading the number as the question is exactly the conflation the
lifecycle oneof removed."
  ;; Arrange / Act
  (let ((f (agent-repl-failure-from-wire
            '(:kind (:shimDegraded ()) :message "no traffic" :resolved ()))))
    ;; Assert
    (should (plist-get f :resolved))))

(ert-deftest agent-repl-test-failure-from-wire-coerces-a-string-resolution ()
  "A protojson STRING resolution instant is adopted as a NUMBER.
protojson encodes int64 as a JSON string, so every daemon-sent resolved
failure arrives with `resolvedAtMs' as a string of digits."
  ;; Arrange / Act
  (let ((f (agent-repl-failure-from-wire
            '(:kind (:sessionSuperseded ()) :message "superseded"
              :resolved (:resolvedAtMs "1786127506030")))))
    ;; Assert
    (should (equal (plist-get f :resolved-at) 1786127506030))))

(ert-deftest agent-repl-test-failure-from-wire-rejects-an-unreadable-resolution ()
  "A present but unreadable resolution instant signals rather than defaulting.
Defaulting it to 0 would misreport when a settled failure ended."
  ;; Arrange / Act / Assert
  (should-error
   (agent-repl-failure-from-wire
    '(:kind (:sessionSuperseded ()) :message "superseded"
      :resolved (:resolvedAtMs "later")))))

(ert-deftest agent-repl-test-failure-from-wire-adopts-the-item-uuid-from-the-caller ()
  "The card's feed address is passed in: a card carries no uuid of its own."
  ;; Arrange / Act
  (let ((f (agent-repl-failure-from-wire
            '(:kind (:shimRejected ()) :message "rejected" :terminal ())
            "item-17")))
    ;; Assert
    (should (equal (plist-get f :item-uuid) "item-17"))))

(ert-deftest agent-repl-test-failure-from-wire-defaults-an-absent-item-uuid ()
  "A card carried outside the feed was never filed, so its address is empty."
  ;; Arrange / Act
  (let ((f (agent-repl-failure-from-wire
            '(:kind (:shimRejected ()) :message "rejected" :terminal ()))))
    ;; Assert
    (should (equal (plist-get f :item-uuid) ""))))

(ert-deftest agent-repl-test-failure-from-wire-reads-the-resume-evidence-off-the-arm ()
  "The typed resume evidence now rides its own kind arm's `detail'."
  ;; Arrange / Act
  (let ((f (agent-repl-failure-from-wire
            '(:kind (:sessionResumeFailed (:detail (:cwd "/repo")))
              :message "resume failed" :terminal ()))))
    ;; Assert
    (should (equal (plist-get f :session-resume) '(:cwd "/repo")))))

(ert-deftest agent-repl-test-failure-from-wire-unset-kind-errors ()
  "A card with no kind names no failure and takes no side."
  ;; Act / Assert
  (should-error (agent-repl-failure-from-wire '(:message "something" :open ()))))

(ert-deftest agent-repl-test-failure-surface-is-silent-for-a-string-resolution ()
  "A wire failure resolved with a STRING instant is logged, not echoed."
  ;; Arrange
  (let ((failure (agent-repl-failure-from-wire
                  '(:kind (:sessionSuperseded ())
                    :message "superseded"
                    :resolved (:resolvedAtMs "1786127506030"))))
        echoed)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
      ;; Act
      (let ((result (agent-repl-failure-surface "ws" failure)))
        ;; Assert
        (should (null result))
        (should (null echoed))))))

(ert-deftest agent-repl-test-failure-from-wire-logs-all-adopted-fields ()
  "Wire adoption logs every value that reaches the normalized failure."
  ;; Arrange
  (let (logs)
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (ws fmt &rest args)
                 (push (list ws (apply #'format fmt args)) logs))))
      ;; Act
      (agent-repl-failure-from-wire
       '(:kind (:shimRejected ()) :message "rejected" :detail "request nacked"
         :resolved (:resolvedAtMs 17))
       "failure-17")
      ;; Assert — the adoption line is pushed before the class lookup's own.
      (should (string-match-p "kind=\\\"shimRejected\\\"" (cadr (cadr logs))))
      (should (string-match-p "resolved-at=17" (cadr (cadr logs))))
      (should (string-match-p "item-uuid=\\\"failure-17\\\"" (cadr (cadr logs)))))))

;;;; ---- CommandAck adoption ---------------------------------------------

(ert-deftest agent-repl-test-failure-from-ack-reads-the-class-off-the-kind ()
  "A refusal's side comes from the bare kind the ack carries."
  ;; Arrange / Act
  (let ((f (agent-repl-failure-from-ack
            '(:requestId "r1" :failure (:apiRateLimit ()) :error "rate limited"))))
    ;; Assert
    (should (eq (plist-get f :class) :api))))

(ert-deftest agent-repl-test-failure-from-ack-prefers-the-daemon-text ()
  "The daemon's own refusal text is the sentence when it supplied one."
  ;; Arrange / Act
  (let ((f (agent-repl-failure-from-ack
            '(:requestId "r1" :failure (:shimRejected ())
              :error "shimclient: request nacked"))))
    ;; Assert
    (should (equal (plist-get f :message) "shimclient: request nacked"))))

(ert-deftest agent-repl-test-failure-from-ack-names-the-kind-without-text ()
  "With no refusal text the kind arm names what was refused.
An ack carries no daemon-composed sentence, so naming the kind is the
honest account; an empty echo would be a silent refusal."
  ;; Arrange / Act
  (let ((f (agent-repl-failure-from-ack
            '(:requestId "r1" :failure (:workspaceNotLive ())))))
    ;; Assert
    (should (equal (plist-get f :message) "workspaceNotLive"))))

(ert-deftest agent-repl-test-failure-from-ack-adopts-the-card-address ()
  "`failure_card' is the address of the card the refusal was filed under."
  ;; Arrange / Act
  (let ((f (agent-repl-failure-from-ack
            '(:requestId "r1" :failure (:shimRejected ())
              :failureCard (:cardUuid "item-9")))))
    ;; Assert
    (should (equal (plist-get f :item-uuid) "item-9"))))

(ert-deftest agent-repl-test-failure-from-ack-is-never-resolved ()
  "A refusal is a decision, not a window: nothing closes it."
  ;; Arrange / Act
  (let ((f (agent-repl-failure-from-ack
            '(:requestId "r1" :failure (:shimRejected ())))))
    ;; Assert
    (should-not (plist-get f :resolved))))

(ert-deftest agent-repl-test-failure-from-ack-without-a-kind-is-nil ()
  "An unclassified refusal returns nil so the caller keeps its raw-text path.
Handing back a failure with no side would be Emacs classifying a refusal
the daemon declined to classify."
  ;; Act / Assert
  (should-not (agent-repl-failure-from-ack '(:requestId "r1" :error "boom"))))

;;;; ---- Local construction ----------------------------------------------

(ert-deftest agent-repl-test-failure-local-is-internal-class ()
  "A locally-classified failure is always INTERNAL.
Nothing Emacs can observe implicates the account, so an `:api' local
failure would be Emacs guessing at something only the daemon can see."
  ;; Act
  (let ((f (agent-repl-failure-local "client.daemon_unreachable" "gone")))
    ;; Assert
    (should (eq (plist-get f :class) :internal))))

(ert-deftest agent-repl-test-failure-local-accepts-command-unacked ()
  "An unanswered outbound command is a locally-classifiable failure.
The daemon cannot report that it never answered Emacs, so the ack-aging
verdict has to be minted at this end."
  ;; Act
  (let ((f (agent-repl-failure-local "client.command_unacked" "never acked")))
    ;; Assert
    (should (equal (plist-get f :type) "client.command_unacked"))))

(ert-deftest agent-repl-test-failure-local-rejects-an-unlisted-type ()
  "A type outside the closed local set fails loudly.
That is what keeps the local vocabulary closed rather than accumulating a
type per call site."
  ;; Act / Assert
  (should-error (agent-repl-failure-local "client.something_new" "x")))

(ert-deftest agent-repl-test-failure-local-rejects-a-daemon-type ()
  "A DAEMON-owned type cannot be minted locally."
  ;; Act / Assert
  (should-error (agent-repl-failure-local "shim.rejected" "x")))

(ert-deftest agent-repl-test-failure-local-logs-validation-and-creation ()
  "Local construction records its inputs and the closed-vocabulary outcome."
  ;; Arrange
  (let (logs)
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (ws fmt &rest args)
                 (push (list ws (apply #'format fmt args)) logs))))
      ;; Act
      (agent-repl-failure-local "client.daemon_unreachable" "gone" "dial failed")
      (should-error (agent-repl-failure-local "client.unknown" "gone"))
      ;; Assert
      (should (string-match-p "REJECTED unlisted-type=\\\"client.unknown\\\""
                       (cadr (car logs))))
      (should (string-match-p "CREATED class=:internal type=\\\"client.daemon_unreachable\\\""
                       (cadr (caddr logs)))))))

;;;; ---- Surfacing -------------------------------------------------------

(ert-deftest agent-repl-test-failure-text-leads-with-the-prose ()
  "The human sentence leads and the raw account follows."
  ;; Arrange / Act
  (let ((text (agent-repl-failure-text
               '(:message "the agent process rejected the request"
                 :detail "shimclient: request nacked"))))
    ;; Assert
    (should (string-prefix-p "the agent process rejected the request" text))))

(ert-deftest agent-repl-test-failure-text-keeps-the-detail-as-evidence ()
  "The raw account survives beside the prose rather than replacing it."
  ;; Arrange / Act
  (let ((text (agent-repl-failure-text
               '(:message "rejected" :detail "shimclient: request nacked"))))
    ;; Assert
    (should (string-match-p "shimclient: request nacked" text))))

(ert-deftest agent-repl-test-failure-text-omits-an-empty-detail ()
  "A failure with no raw account reads as the sentence alone."
  ;; Arrange / Act
  (let ((text (agent-repl-failure-text '(:message "rejected" :detail ""))))
    ;; Assert
    (should (equal text "rejected"))))

(ert-deftest agent-repl-test-failure-text-logs-detail-branch-and-rendered-text ()
  "Text construction logs whether detail entered the human account."
  ;; Arrange
  (let (logs)
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (ws fmt &rest args)
                 (push (list ws (apply #'format fmt args)) logs))))
      ;; Act
      (agent-repl-failure-text '(:type "shim.rejected" :message "rejected"
                                 :detail "request nacked"))
      (agent-repl-failure-text '(:type "shim.rejected" :message "rejected"
                                 :detail ""))
      ;; Assert
      (should (string-match-p "detail-included=nil" (cadr (car logs))))
      (should (string-match-p "detail-included=t" (cadr (cadr logs)))))))

(ert-deftest agent-repl-test-failure-surface-echoes-an-open-failure ()
  "An unresolved failure reaches the echo area."
  ;; Arrange
  (let (echoed)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
      ;; Act
      (agent-repl-failure-surface nil '(:class :internal :type "shim.rejected"
                                        :message "rejected" :detail "" :resolved-at 0))
      ;; Assert
      (should (string-match-p "rejected" echoed)))))

(ert-deftest agent-repl-test-failure-surface-does-not-echo-a-resolution ()
  "A RESOLVED failure is logged and not echoed.
The echo area is for what is happening now; announcing the end of
something the user may never have seen the start of is noise."
  ;; Arrange
  (let (echoed)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
      ;; Act
      (agent-repl-failure-surface nil '(:class :internal :type "shimDegraded"
                                        :message "no traffic" :detail ""
                                        :resolved t
                                        :resolved-at 1700000000000))
      ;; Assert
      (should (null echoed)))))

(ert-deftest agent-repl-test-failure-surface-returns-the-echoed-text ()
  "Surfacing answers what it echoed, so a caller can assert on it."
  ;; Arrange
  (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
    ;; Act / Assert
    (should (equal (agent-repl-failure-surface
                   nil '(:class :internal :type "shim.rejected"
                          :message "rejected" :detail "" :resolved-at 0))
                   "rejected"))))

(ert-deftest agent-repl-test-failure-surface-logs-open-and-resolved-context ()
  "Surface logs preserve the workspace and lifecycle fields on both branches."
  ;; Arrange
  (let (logs)
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (ws fmt &rest args)
                 (push (list ws (apply #'format fmt args)) logs)))
              ;; The OPEN branch rides the warn rung (a surfaced failure is a
              ;; UX regression); capture both rungs so this test keeps
              ;; asserting the CONTEXT rather than the severity.
              ((symbol-function 'agent-repl--warn)
               (lambda (ws fmt &rest args)
                 (push (list ws (apply #'format fmt args)) logs)))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      ;; Act
      (agent-repl-failure-surface
       "ws-a" '(:class :internal :type "shimRejected" :message "rejected"
                :detail "" :resolved nil :resolved-at 0 :item-uuid "open-1"))
      (agent-repl-failure-surface
       "ws-a" '(:class :internal :type "shimRejected" :message "rejected"
                :detail "" :resolved t :resolved-at 17 :item-uuid "resolved-17"))
      ;; Assert
      (should (string-match-p "RESOLVED.*resolved-at=17.*item-uuid=\\\"resolved-17\\\""
                       (cadr (car logs))))
      (should (string-match-p "OPEN.*resolved-at=0.*item-uuid=\\\"open-1\\\""
                       (cadr (caddr logs))))
      (should (cl-every (lambda (entry) (equal (car entry) "ws-a")) logs)))))

(ert-deftest agent-repl-test-failure-surface-open-records-at-the-warn-rung ()
  "An OPEN failure is recorded at `warn', not below it.
A surfaced failure card IS a user-visible regression, so it must clear the
warning sweeps and the remediation loops' warning gate rather than hiding
on the debug rung."
  ;; Arrange
  (let (levels)
    (cl-letf (((symbol-function 'agent-repl--persist-log-record)
               (lambda (_ws level _verbosity fmt _args)
                 (when (string-match-p "failure OPEN" fmt)
                   (push level levels))))
              ((symbol-function 'agent-repl--emit-message) (lambda (&rest _) nil))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      ;; Act
      (agent-repl-failure-surface
       "ws-a" '(:class :internal :type "shim.rejected" :message "rejected"
                :detail "" :resolved-at 0 :item-uuid "open-1"))
      ;; Assert
      (should (equal levels '("warn"))))))

(provide 'test-failure)
;;; test-failure.el ends here
