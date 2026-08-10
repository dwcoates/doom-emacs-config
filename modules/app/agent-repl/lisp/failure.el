;;; failure.el --- The classified-failure vocabulary -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs's end of the ONE render-bound error vocabulary (F4).
;;
;; Every failure that reaches a human eye is a `frontend.v1.FailureCardView'
;; produced by exactly one classifier, and its color comes from exactly one
;; table.  Two rules follow, and this file is where they live:
;;
;; 1. A fact is classified by the process that is FIRST to hold both the
;;    failure and its cause, exactly once, and every process downstream
;;    renders the result without re-inspecting it.  The daemon holds every
;;    conversation-plane fact, so anything arriving on `frontend.v1' is
;;    render-only here, forever.  What the daemon cannot report is its own
;;    unreachability and Emacs's own subprocess/worktree operations — those,
;;    and only those, are classified locally.
;;
;; 2. Failure TYPES are NAMESPACED by owner, so a violation is detectable by
;;    string inspection rather than by review.  Daemon-owned types are the
;;    `FailureKind' oneof's own arm names, unprefixed (`shimRejected',
;;    `apiRateLimit'); everything minted here carries the reserved `client.'
;;    prefix.
;;
;; WHAT THE COMPONENT RESHAPE CHANGED.  The wire used to carry a class enum
;; plus a free type string on one `SystemFailureItem'.  It now carries a
;; `FailureKind' — a closed oneof with one arm per failure — on a
;; `FailureCardView' that also holds the sentence, the evidence and a
;; lifecycle oneof.  The CLASS did not become unknowable, it became implicit
;; in the arm: each arm belongs to exactly one side of the vocabulary, so the
;; side is read off the arm here (`agent-repl-failure-kind-class') instead of
;; off a second field that could disagree with it.  The two class keywords and
;; the color contract they answer to are unchanged.
;;
;; Before this, a refused command reached the echo area as raw Go text
;; (`err.Error()' verbatim), a degraded component reached it as a
;; component/reason pair the daemon had already classified, and a session
;; death reached it as nothing at all — `death_reason' rode the wire with
;; TWO producers and ZERO readers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--frontend-int64 "frontend-state" (raw))

;;;; ---- Namespace partition ---------------------------------------------

(defconst agent-repl-failure-client-prefix "client."
  "The `Error.type' namespace reserved for FRONTEND-classified failures.

The daemon must never emit a type carrying it, and Emacs must never emit
one without it.  That makes a provenance violation a string check rather
than a review question: a bare type minted here would mean Emacs
re-classified something the daemon already decided, and a prefixed one on
the wire would mean a frontend's failure was laundered through the
daemon.")

(defconst agent-repl-failure-local-types
  '("client.daemon_unreachable"
    "client.daemon_exited"
    "client.command_unacked"
    "client.worktree_failed"
    "client.merge_failed")
  "The closed vocabulary of failures Emacs classifies for ITSELF.

Each names something no other process can observe: the UDS dial Emacs
performs, the daemon process Emacs supervises, the acknowledgements Emacs
is still waiting on for commands only Emacs sent, and the git/worktree
operations Emacs runs.  Nothing on the `frontend.v1' conversation plane
appears here — that plane arrives already classified.

`client.command_unacked' is the aging verdict on an outbound
`FrontendCommand': the daemon cannot report that it never answered, so
the deadline that declares the command lost is necessarily this end's
own (see `agent-repl--uds-command-deadline-expired').")

(defun agent-repl-failure-client-type-p (type)
  "Return non-nil when TYPE belongs to the FRONTEND namespace."
  (let ((client-type-p
         (and (stringp type)
              (string-prefix-p agent-repl-failure-client-prefix type))))
    (agent-repl--log nil
                     "failure-client-type-p: type=%S stringp=%S client-type-p=%S"
                     type (stringp type) client-type-p)
    client-type-p))

;;;; ---- Classes ---------------------------------------------------------

(defconst agent-repl-failure-classes '(:internal :api)
  "The two SIDES of the failure vocabulary, as render keywords.

SEMANTIC, never chromatic: the class says what KIND of thing failed and
each frontend decides what that looks like.  `:internal' is agent-repl's
own machinery (shim down, daemon unreachable, store outage, a refused
command); `:api' is the SDK or the vendor refusing or concluding the work.")

(defconst agent-repl-failure-class-wire
  '(("ERROR_CLASS_INTERNAL" . :internal)
    ("ERROR_CLASS_API"      . :api))
  "Map each COLOR-CONTRACT class name to its keyword.

The names are the rows of `error_classes' in proto/vocab/render-colors.json,
which is what binds a class to one of the six colors across Go, TypeScript
and here.  The `ErrorClass' ENUM that used to carry these names on the wire
is retired — a card now states its side by which `FailureKind' arm is set —
but the assignment did not move, so this stays the join between a failure's
side and its color and `test-render-colors.el' still asserts it row for row.

`ERROR_CLASS_UNSPECIFIED' is deliberately absent.  The class decides the
failure's color, so accepting an unset one would mean painting a failure
some default color — quietly, and in a way that could contradict the
workspace colored beside it.  See `agent-repl-failure-class'.")

(defun agent-repl-failure-class (name)
  "Return the class keyword for color-contract class NAME.

Signals on anything outside `agent-repl-failure-class-wire' — there is no
fallback class (AGENTS.md No-Silent-Fallbacks), because a guessed class is
a mis-colored failure rather than a missing one."
  (let ((class (cdr (assoc name agent-repl-failure-class-wire))))
    (if class
        (progn
          (agent-repl--log nil
                           "failure-class: error-class=%S class=%S"
                           name class)
          class)
      (agent-repl--log nil
                       "failure-class: REJECTED unrecognized-error-class=%S"
                       name)
      (error "agent-repl failure: unrecognized error_class %S" name))))

;;;; ---- The FailureKind arms --------------------------------------------
;;
;; The wire names a failure by WHICH ARM of the `FailureKind' oneof is set.
;; Each arm belongs to exactly one side of the vocabulary, and the proto says
;; so arm by arm ("Resolves the workspace BLUE" / "PURPLE"), so the side is a
;; property of the arm rather than a second field beside it.  These three
;; lists are that partition, spelled in protojson (lowerCamelCase) exactly as
;; the arm arrives.
;;
;; They are the one place Emacs restates the oneof, so `test-failure.el'
;; asserts them against the checked-in generated bindings: an arm added to the
;; proto and forgotten here fails the suite instead of arriving at runtime as
;; a failure with no side and therefore no color.

(defconst agent-repl-failure-machinery-kinds
  '("shimNotConnected" "shimRejected" "shimAckTimeout" "shimVersionMismatch"
    "shimSeqRegression" "shimDegraded" "shimStoreWriteRejected"
    "queryTermination" "shimNotSpawned" "shimHandshakeIncomplete"
    "shimUnhealthy" "sessionNotEstablished" "workspaceNotLive"
    "sessionDeleted" "sessionSuperseded" "reconnectSuperseded"
    "sessionShimDied" "sessionStartFailed" "sessionResumeFailed"
    "conversationUnresumable" "resumeModeRetired" "sessionEndedUnclassified"
    "historyRepullInFlight" "historyReplayTruncated" "interruptUndelivered"
    "queueEntryUnwired" "queueEntryKeepAliveHeld"
    "queueEntryUninterruptibleTurn" "sessionHibernated" "turnUndriven"
    "keepAliveWindowUnclosed" "keepAliveWindowInverted" "compactionColdRead"
    "clientLogIdentityStale" "promptRefusedByMergeState"
    "internalUnclassified")
  "The `FailureKind' arms naming agent-repl's OWN machinery failing.
Nothing about the account is implicated and no amount of waiting or
re-authenticating helps, so they classify `:internal' and the shared color
table resolves them blue.")

(defconst agent-repl-failure-vendor-kinds
  '("apiAuthenticationFailed" "apiBillingError" "apiRateLimit"
    "apiInvalidRequest" "apiServerError" "apiOverloaded"
    "apiOauthOrgNotAllowed" "apiModelNotFound" "apiNetworkDown"
    "apiRequestFailed" "apiUnknown" "apiMaxOutputTokens" "apiMaxTurns"
    "apiMaxBudget" "apiExecutionError" "apiRefusal" "apiTurnFailed")
  "The `FailureKind' arms naming the SDK or the vendor refusing the work.
Releasing one needs a human or the vendor, never a retry, so they classify
`:api' and the shared color table resolves them purple.")

(defconst agent-repl-failure-client-kinds
  '("daemonUnreachable" "workspaceGone" "bootFailed" "controlPlaneFailed"
    "frameUndecodable" "staleBundle" "commandUnsent"
    "commandRejectionUnclassified")
  "The `FailureKind' arms a FRONTEND mints for itself; the daemon never sets one.

They classify `:internal' with the machinery arms: a frontend can only ever
observe its own machinery failing, never the account.

Emacs does not currently mint any of them — its local vocabulary is
`agent-repl-failure-local-types', which never leaves this process — but the
arms are listed because Emacs can RECEIVE one: the daemon rebroadcasts a
webapp-authored card, and a card whose arm this end could not place would
have no side and therefore no color.")

(defun agent-repl-failure-kind-class (arm)
  "Return the class keyword for `FailureKind' ARM (a protojson arm name).

Signals on an arm outside the three partition lists, for the same reason
`agent-repl-failure-class' signals on an unrecognized class: a failure whose
side cannot be read is a failure that would be painted some default color."
  (let ((class (cond
                ((member arm agent-repl-failure-vendor-kinds) :api)
                ((or (member arm agent-repl-failure-machinery-kinds)
                     (member arm agent-repl-failure-client-kinds))
                 :internal))))
    (if class
        (progn
          (agent-repl--log nil "failure-kind-class: arm=%S class=%S" arm class)
          class)
      (agent-repl--log nil "failure-kind-class: REJECTED unrecognized-arm=%S" arm)
      (error "agent-repl failure: unrecognized FailureKind arm %S" arm))))

(defun agent-repl-failure-kind-arm (kind)
  "Return the single set arm of decoded `FailureKind' KIND, as a plist pair.

The value is a cons of the protojson arm NAME (a string) and the arm's own
decoded payload.  protojson emits only the set arm, so a well-formed kind
decodes to a one-key plist.

Signals when KIND is absent, empty, or carries more than one arm.  A kind is
the whole classification, so an unset one is a failure nobody named and a
double-set one is two failures claiming to be the same card; neither may be
resolved by picking whichever arm happens to come first."
  (unless (and kind (listp kind) (keywordp (car kind)) (null (cddr kind)))
    (agent-repl--log nil
                     "failure-kind-arm: MALFORMED arm-count=%S — no fallback"
                     (and (listp kind) (/ (length kind) 2)))
    (error "agent-repl failure: FailureKind must carry exactly one arm"))
  (cons (substring (symbol-name (car kind)) 1) (plist-get kind (car kind))))

;;;; ---- Construction ----------------------------------------------------

(defun agent-repl-failure--resolved-at (card)
  "Return decoded `FailureCardView' CARD's resolution instant as a NUMBER.

The instant lives on the `resolved' arm of the card's `lifecycle' oneof
\(`FailureCardResolved.resolved_at_ms').  An `open' or `terminal' card has
no instant and reads 0.

protojson encodes int64 as a JSON STRING, so a daemon-sent instant arrives
here as \"1786127506030\" while a hand-built test frame carries
1786127506030.  Both are the same fact, so the coercion belongs at this
decode boundary rather than at each crash site.

Absence inside a `resolved' arm is 0 — protojson omits a zero-valued int64,
and a card resolved at instant zero is representable.  Which is exactly why
the ARM, never this number, is what `agent-repl-failure--resolved-p'
answers.  A PRESENT but uncoercible value is not defaulted away: it signals,
because a resolution instant that cannot be read would misreport when a
settled failure ended."
  (let ((raw (plist-get (plist-get card :resolved) :resolvedAtMs)))
    (if (null raw)
        0
      (or (agent-repl--frontend-int64 raw)
          (error "agent-repl failure: unreadable resolved_at_ms %S" raw)))))

(defun agent-repl-failure--resolved-p (card)
  "Return non-nil when decoded `FailureCardView' CARD holds the `resolved' arm.

The ARM is the question, never the instant: a card resolved at instant zero
is representable, and reading the number as the verdict is the conflation
the lifecycle oneof removed."
  (and (plist-member card :resolved) t))

(defun agent-repl-failure-from-wire (card &optional item-uuid)
  "Normalize a decoded `FailureCardView' CARD plist into a failure plist.

ITEM-UUID is the `ConversationItem.uuid' the card was filed under, when the
caller has one.  It is NOT on the card — a card carried outside the feed
\(a `SessionView.death') was never filed at all — so it is passed in rather
than guessed at.

The result is the plist every surfacing site here consumes: `:class'
`:type' `:message' `:detail' `:resolved' `:resolved-at' `:item-uuid' and
`:session-resume'.

This is an ADOPTION, not a derivation.  Nothing is re-decided: the kind, the
sentence and the evidence are the daemon's verdict, and the class is read
off the kind's own arm rather than re-classified here."
  (let* ((arm (agent-repl-failure-kind-arm (plist-get card :kind)))
         (type (car arm))
         (message (or (plist-get card :message) ""))
         (detail (or (plist-get card :detail) ""))
         (resolved (agent-repl-failure--resolved-p card))
         (resolved-at (agent-repl-failure--resolved-at card))
         (item-uuid (or item-uuid ""))
         ;; The typed resume evidence moved onto its own kind arm: it used to
         ;; be a oneof member beside the class on the failure item, and it is
         ;; now `FailureSessionResumeFailed.detail'.
         (session-resume (and (equal type "sessionResumeFailed")
                              (plist-get (cdr arm) :detail))))
    (agent-repl--log nil
                     "failure-from-wire: adopting kind=%S message=%S detail=%S resolved=%S resolved-at=%S item-uuid=%S session-resume=%S"
                     type message detail resolved resolved-at item-uuid
                     (and session-resume t))
    (list :class (agent-repl-failure-kind-class type)
          :type type
          :message message
          :detail detail
          :resolved resolved
          :resolved-at resolved-at
          :item-uuid item-uuid
          :session-resume session-resume)))

(defun agent-repl-failure-from-ack (ack)
  "Normalize a refused `CommandAck' ACK plist into a failure plist, or nil.

A refusal carries the classification WITHOUT the card: `failure' is a bare
`FailureKind', `failure_card' is the address of the card the daemon filed
the refusal under (when it filed one), and the legacy `error' string is the
raw Go text this end used to print verbatim.

Returns nil when the ack carries no classified kind, so the caller can keep
its own unclassified-refusal path rather than being handed a failure with
no side.

The kind names the failure and the ack has no daemon-composed sentence of
its own, so the arm name IS the message when the daemon supplied no text.
That is naming what was refused, not composing prose for it."
  (when-let ((kind (plist-get ack :failure)))
    (let* ((arm (agent-repl-failure-kind-arm kind))
           (type (car arm))
           (err (plist-get ack :error))
           (message (if (and (stringp err) (not (string-empty-p err))) err type))
           (item-uuid (or (plist-get (plist-get ack :failureCard) :cardUuid) "")))
      (agent-repl--log nil
                       "failure-from-ack: adopting kind=%S error-present=%s card-uuid=%S"
                       type
                       (if (and (stringp err) (not (string-empty-p err))) "yes" "no")
                       item-uuid)
      (list :class (agent-repl-failure-kind-class type)
            :type type
            :message message
            ;; WHETHER `:message' IS PROSE OR EVIDENCE.  The wire card path
            ;; carries a sentence the daemon composed for a human; this path
            ;; may carry the raw `err.Error()' chain instead, and the echo area
            ;; must translate the second rather than print it.
            :message-raw (and (stringp err) (not (string-empty-p err)) t)
            :detail ""
            ;; A refusal is a decision, not a window: nothing closes it.
            :resolved nil
            :resolved-at 0
            :item-uuid item-uuid
            :session-resume nil))))

(defun agent-repl-failure--session-resume-text (resume workspace)
  "Return actionable prose for typed `SessionResumeFailure' RESUME.
WORKSPACE is threaded to canonical logging.  The oneof cause is closed: a
typed resume failure without exactly one recognized cause is malformed and
signals rather than being flattened into generic failure text."
  (let* ((claude-id (plist-get resume :claudeSessionId))
         (cwd (plist-get resume :cwd))
         (automatic-restore (plist-member resume :automaticRestore))
         (create (plist-member resume :create))
         (transcript (plist-get resume :transcriptUnavailable))
         (mismatch (plist-get resume :identityMismatch))
         (bring-up-failure (plist-get resume :bringUpFailure))
         (query-termination (plist-get resume :queryTermination))
         (causes (cl-remove-if-not #'identity
                                   (list (and transcript :transcript-unavailable)
                                         (and mismatch :identity-mismatch)
                                         (and bring-up-failure :bring-up-failure)
                                         (and query-termination :query-termination)))))
    (unless (and (stringp claude-id) (not (string-empty-p claude-id))
                 (stringp cwd) (not (string-empty-p cwd))
                 (= (length causes) 1)
                 (= (+ (if automatic-restore 1 0) (if create 1 0)) 1))
      (agent-repl--log workspace
                       "failure-session-resume-text: MALFORMED claude-id=%S cwd=%S causes=%S automatic-restore=%S create=%S resume=%S"
                       claude-id cwd causes automatic-restore create resume)
      (error "agent-repl failure: malformed SessionResumeFailure"))
    (pcase (car causes)
      (:transcript-unavailable
       (let ((searched-paths (plist-get transcript :searchedPaths)))
         (unless (listp searched-paths)
           (agent-repl--log workspace
                            "failure-session-resume-text: MALFORMED transcript searched-paths=%S claude-id=%S cwd=%S"
                            searched-paths claude-id cwd)
           (error "agent-repl failure: SessionResumeFailure transcript paths missing"))
         (let ((text (format "Resume %s for Claude session %s in %s cannot continue: transcript unavailable at %s. Restore that transcript, then retry."
                             (if automatic-restore "restoration" "creation")
                             claude-id cwd
                             (if searched-paths
                                 (mapconcat #'identity searched-paths ", ")
                               "the configured transcript locations"))))
           (agent-repl--log workspace
                            "failure-session-resume-text: cause=transcript-unavailable claude-id=%S cwd=%S searched-paths=%S text=%S"
                            claude-id cwd searched-paths text)
           text)))
      (:identity-mismatch
       (let ((replacement (plist-get mismatch :replacementClaudeSessionId)))
         (unless (stringp replacement)
           (agent-repl--log workspace
                            "failure-session-resume-text: MALFORMED identity replacement=%S claude-id=%S cwd=%S"
                            replacement claude-id cwd)
           (error "agent-repl failure: SessionResumeFailure replacement id missing"))
         (let ((text (format "Resume %s for Claude session %s in %s was refused because recovery proposed %s. Restore the authoritative conversation, then retry."
                             (if automatic-restore "restoration" "creation")
                             claude-id cwd
                             (if (string-empty-p replacement)
                                 "a fresh conversation"
                               (format "Claude session %s" replacement)))))
           (agent-repl--log workspace
                            "failure-session-resume-text: cause=identity-mismatch claude-id=%S cwd=%S replacement=%S text=%S"
                            claude-id cwd replacement text)
           text)))
      (:bring-up-failure
       (let ((cause (plist-get bring-up-failure :cause)))
         (unless (and (stringp cause) (not (string-empty-p cause)))
           (agent-repl--log workspace
                            "failure-session-resume-text: MALFORMED bring-up-failure cause=%S claude-id=%S cwd=%S"
                            cause claude-id cwd)
           (error "agent-repl failure: SessionResumeFailure bring-up cause missing"))
         (let ((text (format "Resume %s for Claude session %s in %s could not bring the session up: %s."
                             (if automatic-restore "restoration" "creation")
                             claude-id cwd cause)))
           (agent-repl--log workspace
                            "failure-session-resume-text: cause=bring-up-failure claude-id=%S cwd=%S cause=%S text=%S"
                            claude-id cwd cause text)
           text)))
      (:query-termination
       (let ((reason (agent-repl-failure--query-termination-reason-text
                      query-termination workspace)))
         (let ((text (format "Resume %s for Claude session %s in %s could not continue: the resumed query terminated (%s)."
                             (if automatic-restore "restoration" "creation")
                             claude-id cwd reason)))
           (agent-repl--log workspace
                            "failure-session-resume-text: cause=query-termination claude-id=%S cwd=%S reason=%S text=%S"
                            claude-id cwd reason text)
           text))))))

(defun agent-repl-failure--query-termination-reason-text (termination workspace)
  "Return the human account of the `reason' oneof in QueryTerminationFailure
TERMINATION.  WORKSPACE is threaded to canonical logging.  Like the outer
cause oneof, this nested one is closed: anything other than exactly one
recognized reason is malformed."
  (let* ((unexpected-eof (plist-member termination :unexpectedEof))
         (iterator-failure (plist-get termination :iteratorFailure))
         (startup-failure (plist-get termination :startupFailure))
         (reasons (cl-remove-if-not #'identity
                                    (list (and unexpected-eof :unexpected-eof)
                                          (and iterator-failure :iterator-failure)
                                          (and startup-failure :startup-failure)))))
    (unless (= (length reasons) 1)
      (agent-repl--log workspace
                       "failure-query-termination-reason: MALFORMED reasons=%S termination=%S"
                       reasons termination)
      (error "agent-repl failure: malformed QueryTerminationFailure reason"))
    (pcase (car reasons)
      (:unexpected-eof
       "the SDK iterator ended without an intentional shutdown")
      (:iterator-failure
       (let ((cause (plist-get iterator-failure :cause)))
         (unless (and (stringp cause) (not (string-empty-p cause)))
           (agent-repl--log workspace
                            "failure-query-termination-reason: MALFORMED iterator-failure cause=%S"
                            cause)
           (error "agent-repl failure: QueryTerminationFailure iterator cause missing"))
         (format "the SDK iterator threw: %s" cause)))
      (:startup-failure
       (let ((cause (plist-get startup-failure :cause)))
         (unless (and (stringp cause) (not (string-empty-p cause)))
           (agent-repl--log workspace
                            "failure-query-termination-reason: MALFORMED startup-failure cause=%S"
                            cause)
           (error "agent-repl failure: QueryTerminationFailure startup cause missing"))
         (format "query initialization failed: %s" cause))))))

(defun agent-repl-failure-local (type message &optional detail)
  "Build a LOCALLY-classified failure of TYPE with MESSAGE and DETAIL.

Always `:internal' class: everything Emacs can legitimately classify is
agent-repl's own machinery failing.  Nothing about the account is ever
implicated by a dial that would not connect or a worktree command that
returned non-zero, so an `:api' local failure would be Emacs guessing at
something only the daemon can see.

Signals when TYPE is outside `agent-repl-failure-local-types', which is
what keeps the local vocabulary closed rather than accumulating a type per
call site."
  (let ((allowed (member type agent-repl-failure-local-types)))
    (agent-repl--log nil
                     "failure-local: requested type=%S message=%S detail=%S allowed=%S"
                     type message detail (and allowed t))
    (unless allowed
      (agent-repl--log nil
                       "failure-local: REJECTED unlisted-type=%S"
                       type)
      (error "agent-repl failure: %S is not a local failure type" type))
    (let ((failure (list :class :internal
                         :type type
                         :message message
                         :detail (or detail "")
                         :resolved nil
                         :resolved-at 0
                         :item-uuid "")))
      (agent-repl--log nil
                       "failure-local: CREATED class=%S type=%S message=%S detail=%S"
                       (plist-get failure :class)
                       (plist-get failure :type)
                       (plist-get failure :message)
                       (plist-get failure :detail))
      failure)))

;;;; ---- Surfacing -------------------------------------------------------

(defun agent-repl-failure-text (failure &optional workspace)
  "Return the one-line human account of FAILURE for the echo area.

WORKSPACE carries log metadata when the caller already knows the failure's
workspace.  Direct consumers that only format a failure leave it nil.

The prose leads and the raw account follows in parens, rather than the
raw account replacing the prose.  The two are separate fields precisely so
a reader gets the sentence and a debugger still gets the evidence."
  (let ((message (plist-get failure :message))
        (detail (plist-get failure :detail))
        (session-resume (plist-get failure :session-resume)))
    (let ((text
           (cond
            (session-resume
             (agent-repl-failure--session-resume-text session-resume workspace))
            ((and (stringp detail) (not (string-empty-p detail)))
             (format "%s (%s)" message detail))
            (t message))))
      (agent-repl--log workspace
                       "failure-text: type=%S message=%S detail=%S detail-included=%S text=%S"
                       (plist-get failure :type)
                       message detail
                       (and (stringp detail) (not (string-empty-p detail)))
                       text)
      text)))

(defun agent-repl-failure--echo-copy (failure text verb)
  "Return the ONE user-facing sentence FAILURE may put in the echo area.

TEXT is `agent-repl-failure-text''s full account.  That account JOINS the
prose and the raw evidence, which makes it the right thing to log and the
wrong thing to echo, so this narrows it.

VERB names the command that earned the failure, for the cases where copy
has to be composed rather than adopted.

Three sources, in order of how ready they are to be read:

  1. a typed resume failure composes its own actionable prose and never
     admits the raw account into it — TEXT already IS the sentence;
  2. a wire card carries a daemon-composed sentence in `:message' — the
     daemon wrote it for a human, so it is used verbatim;
  3. a legacy ack whose `:message' is the raw `err.Error()' chain
     \(`:message-raw'), which is translated, never printed."
  (let ((message (plist-get failure :message)))
    (cond
     ((plist-get failure :session-resume) text)
     ((plist-get failure :message-raw)
      (agent-repl--user-copy-for-error message verb))
     ((and (stringp message) (not (string-empty-p message))) message)
     (t (agent-repl--user-copy-for-error nil verb)))))

(defun agent-repl-failure-surface (workspace failure &optional verb)
  "Log FAILURE for WORKSPACE and echo it.

VERB names the command the failure answers (\"prompt\", \"hibernate\"),
used only when copy has to be composed because the failure carried none.

The single surfacing point for a classified failure, so the log line and
the echo can never describe it differently.  A RESOLVED failure (one whose
window closed) is logged and NOT echoed: the echo area is for what is
happening now, and announcing the end of something the user may never have
seen the start of is noise.

Returns the text echoed, or nil when the failure was resolved.

Resolution is read from `:resolved', the lifecycle ARM the card carried,
never from `:resolved-at' being positive.  A card settled at instant zero is
representable on the wire, and treating the number as the verdict would
re-announce a closed window as a live alarm."
  (let ((text (agent-repl-failure-text failure workspace))
        (resolved (plist-get failure :resolved)))
    (if resolved
        (progn
          (agent-repl--log workspace
                           "failure RESOLVED class=%s type=%s resolved-at=%S item-uuid=%S: %s"
                           (plist-get failure :class)
                           (plist-get failure :type)
                           (plist-get failure :resolved-at)
                           (plist-get failure :item-uuid)
                           text)
          nil)
      (agent-repl--warn workspace
                       "failure OPEN class=%s type=%s resolved-at=%S item-uuid=%S: %s"
                       (plist-get failure :class)
                       (plist-get failure :type)
                       (plist-get failure :resolved-at)
                       (plist-get failure :item-uuid)
                       text)
      ;; THE ECHO IS THE DAEMON'S OWN PROSE, AND ONLY THAT.  A classified
      ;; failure arrives display-ready: `:message' is a sentence the daemon
      ;; composed for a human, so it is passed through verbatim rather than
      ;; re-translated.  `:detail' is the raw account behind it and belongs on
      ;; the log line, not in the echo area — `agent-repl-failure-text' still
      ;; joins the two for every consumer that wants the whole account.
      (agent-repl--user-message workspace "%s"
                                (list (agent-repl-failure--echo-copy failure text verb))
                                :detail text)
      text)))

(provide 'failure)
;;; failure.el ends here
