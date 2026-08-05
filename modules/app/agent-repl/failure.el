;;; failure.el --- The classified-failure vocabulary -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs's end of the ONE render-bound error vocabulary (F4).
;;
;; Every failure that reaches a human eye is a `frontend.v1.SystemFailureItem'
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
;; 2. `Error.type' values are NAMESPACED by owner, so a violation is
;;    detectable by string inspection rather than by review.  Daemon-owned
;;    types are unprefixed (`shim.rejected', `api.rate_limit'); everything
;;    minted here carries the reserved `client.' prefix.
;;
;; Before this, a refused command reached the echo area as raw Go text
;; (`err.Error()' verbatim), a degraded component reached it as a
;; component/reason pair the daemon had already classified, and a session
;; death reached it as nothing at all — `death_reason' rode the wire with
;; TWO producers and ZERO readers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

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
  "The `ErrorClass' vocabulary, as render keywords.

SEMANTIC, never chromatic: the class says what KIND of thing failed and
each frontend decides what that looks like.  `:internal' is agent-repl's
own machinery (shim down, daemon unreachable, store outage, a refused
command); `:api' is the SDK or the vendor refusing or concluding the work.")

(defconst agent-repl-failure-class-wire
  '(("ERROR_CLASS_INTERNAL" . :internal)
    ("ERROR_CLASS_API"      . :api))
  "Map each `ErrorClass' enum NAME (protojson string) to its keyword.

`ERROR_CLASS_UNSPECIFIED' is deliberately absent.  The class decides the
failure's color, so accepting an unset one would mean painting a failure
some default color — quietly, and in a way that could contradict the
workspace colored beside it.  See `agent-repl-failure-class'.")

(defun agent-repl-failure-class (name)
  "Return the class keyword for `ErrorClass' enum NAME.

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

;;;; ---- Construction ----------------------------------------------------

(defun agent-repl-failure-from-wire (item)
  "Normalize a decoded `SystemFailureItem' ITEM plist into a failure plist.

ITEM is the protojson-decoded arm; the result is the plist every surfacing
site here consumes: `:class' `:type' `:message' `:detail' `:resolved-at'
`:item-uuid' and `:session-resume'.

This is an ADOPTION, not a derivation.  Nothing is re-decided: the class,
the type and the prose are the daemon's verdict."
  (let* ((error-class (plist-get item :errorClass))
         (type (or (plist-get item :errorType) ""))
         (message (or (plist-get item :message) ""))
         (detail (or (plist-get item :sourceDetail) ""))
         (resolved-at (or (plist-get item :resolvedAtMs) 0))
         (item-uuid (or (plist-get item :itemUuid) ""))
         (session-resume (plist-get item :sessionResume)))
    (agent-repl--log nil
                     "failure-from-wire: adopting error-class=%S type=%S message=%S detail=%S resolved-at=%S item-uuid=%S session-resume=%S"
                     error-class type message detail resolved-at item-uuid
                     (and session-resume t))
    (list :class (agent-repl-failure-class error-class)
          :type type
          :message message
          :detail detail
          :resolved-at resolved-at
          :item-uuid item-uuid
          :session-resume session-resume)))

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
         (causes (cl-remove-if-not #'identity
                                   (list (and transcript :transcript-unavailable)
                                         (and mismatch :identity-mismatch)
                                         (and bring-up-failure :bring-up-failure)))))
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
           text))))))

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

(defun agent-repl-failure-surface (workspace failure)
  "Log FAILURE for WORKSPACE and echo it.

The single surfacing point for a classified failure, so the log line and
the echo can never describe it differently.  A RESOLVED failure (one whose
window closed) is logged and NOT echoed: the echo area is for what is
happening now, and announcing the end of something the user may never have
seen the start of is noise.

Returns the text echoed, or nil when the failure was resolved."
  (let ((text (agent-repl-failure-text failure workspace))
        (resolved (> (or (plist-get failure :resolved-at) 0) 0)))
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
      (agent-repl--log workspace
                       "failure OPEN class=%s type=%s resolved-at=%S item-uuid=%S: %s"
                       (plist-get failure :class)
                       (plist-get failure :type)
                       (plist-get failure :resolved-at)
                       (plist-get failure :item-uuid)
                       text)
      (message "agent-repl: %s" text)
      text)))

(provide 'failure)
;;; failure.el ends here
