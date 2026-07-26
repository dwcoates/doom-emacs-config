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
    "client.worktree_failed"
    "client.merge_failed")
  "The closed vocabulary of failures Emacs classifies for ITSELF.

Each names something no other process can observe: the UDS dial Emacs
performs, the daemon process Emacs supervises, and the git/worktree
operations Emacs runs.  Nothing on the `frontend.v1' conversation plane
appears here — that plane arrives already classified.")

(defun agent-repl-failure-client-type-p (type)
  "Return non-nil when TYPE belongs to the FRONTEND namespace."
  (and (stringp type)
       (string-prefix-p agent-repl-failure-client-prefix type)))

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
  (or (cdr (assoc name agent-repl-failure-class-wire))
      (error "agent-repl failure: unrecognized error_class %S" name)))

;;;; ---- Construction ----------------------------------------------------

(defun agent-repl-failure-from-wire (item)
  "Normalize a decoded `SystemFailureItem' ITEM plist into a failure plist.

ITEM is the protojson-decoded arm; the result is the plist every surfacing
site here consumes: `:class' `:type' `:message' `:detail' `:resolved-at'
`:item-uuid'.

This is an ADOPTION, not a derivation.  Nothing is re-decided: the class,
the type and the prose are the daemon's verdict."
  (list :class (agent-repl-failure-class (plist-get item :errorClass))
        :type (or (plist-get item :errorType) "")
        :message (or (plist-get item :message) "")
        :detail (or (plist-get item :sourceDetail) "")
        :resolved-at (or (plist-get item :resolvedAtMs) 0)
        :item-uuid (or (plist-get item :itemUuid) "")))

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
  (unless (member type agent-repl-failure-local-types)
    (error "agent-repl failure: %S is not a local failure type" type))
  (list :class :internal
        :type type
        :message message
        :detail (or detail "")
        :resolved-at 0
        :item-uuid ""))

;;;; ---- Surfacing -------------------------------------------------------

(defun agent-repl-failure-text (failure)
  "Return the one-line human account of FAILURE for the echo area.

The prose leads and the raw account follows in parens, rather than the
raw account replacing the prose.  The two are separate fields precisely so
a reader gets the sentence and a debugger still gets the evidence."
  (let ((message (plist-get failure :message))
        (detail (plist-get failure :detail)))
    (cond
     ((and (stringp detail) (not (string-empty-p detail)))
      (format "%s (%s)" message detail))
     (t message))))

(defun agent-repl-failure-surface (workspace failure)
  "Log FAILURE for WORKSPACE and echo it.

The single surfacing point for a classified failure, so the log line and
the echo can never describe it differently.  A RESOLVED failure (one whose
window closed) is logged and NOT echoed: the echo area is for what is
happening now, and announcing the end of something the user may never have
seen the start of is noise.

Returns the text echoed, or nil when the failure was resolved."
  (let ((text (agent-repl-failure-text failure))
        (resolved (> (or (plist-get failure :resolved-at) 0) 0)))
    (if resolved
        (progn
          (agent-repl--log workspace
                           "failure RESOLVED class=%s type=%s: %s"
                           (plist-get failure :class)
                           (plist-get failure :type)
                           text)
          nil)
      (agent-repl--log workspace
                       "failure class=%s type=%s: %s"
                       (plist-get failure :class)
                       (plist-get failure :type)
                       text)
      (message "agent-repl: %s" text)
      text)))

(provide 'failure)
;;; failure.el ends here
