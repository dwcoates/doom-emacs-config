;;; wire-verbs.el --- protojson codec for the agentrepl.v1 verbs -*- lexical-binding: t; -*-

;;; Commentary:

;; The protojson CODEC for the agentrepl.v1 WORKSPACE VERBS and DAEMON-ADMIN
;; verbs Emacs calls: CreateWorkspace, RegisterRepository, OpenWorkspace,
;; CloseWorkspace, KillWorkspace, NukeWorkspace, MergeWorkspace,
;; RestartWorkspace, SetWorkspacePriority, SubmitPrompt,
;; UpdateShutdownSchedule, Deploy, UpdateMergeQueue, DaemonHealth and
;; SessionHealth.
;;
;; SCOPE.  This file owns exactly the messages declared in those endpoint
;; protos.  The shared leaf vocabularies — WorkspaceRef, RepositoryRef,
;; UserSaid, PromptOrigin, DrainReason, WorkspacePriority, TurnId — live in
;; `wire-common.el' and are reached through the fixed names declared below;
;; they are never re-implemented here.
;;
;; PROTO -> CODE MAPPING.  One BASE function per message, where that
;; message's validation lives exactly once, plus one dedicated function per
;; NON-PRIMITIVE USE SITE (a message-typed field or a oneof arm) that
;; delegates to the child's base.  Primitives get no wrapper.  Where a use
;; site's derived name would COLLIDE with the child's own base name (e.g. a
;; message `Foo' reached through a `foo' arm of its own parent, where both
;; spell `agent-repl-wire-{en,de}code-foo'), the base IS the use-site
;; function: the delegation would be the identity, and two definitions of one
;; name are not possible.
;;
;; WIRE SHAPE (binding, docs/overhaul/elisp-fanout.md §2).  Keys are
;; protojson lowerCamel symbols.  Bools are `t' / `:false'.  int64 is emitted
;; as an integer (Go accepts numbers) and accepted from callers as an integer
;; or a decimal string.  A set EMPTY MESSAGE is `nil', which `json-serialize'
;; writes as `{}' and `json-parse-string' reads back as `nil'.  Repeated
;; fields are lists on the elisp side.
;;
;; ELISP SHAPE.  Messages are plists with kebab-case keywords.  A oneof is
;; `(:arm KEYWORD :value V)', where V is the decoded arm message and `nil'
;; for an empty arm.  An OPTIONAL EMPTY MESSAGE — where presence itself is
;; the fact (CreateWorkspaceFork, CreateWorkspaceUngatedConsent) — is `t'
;; when present and nil when absent, because `nil' cannot express "set" for
;; a message with no fields.
;;
;; VALIDATION INVARIANT.  An unset non-optional field, an unset oneof, two
;; arms of one oneof, an unknown field and an unknown arm are all contract
;; breaches: each logs at ERROR and signals `agent-repl-wire-error' with data
;; (MESSAGE FIELD REASON).  Requests are built only from complete values, so
;; an incomplete request errors BEFORE it can be sent.
;;
;; Empty `<Rpc>Error' messages are deliberate: refusal arms are derived from
;; the daemon's real refusal sites as they are written.  A future arm
;; therefore arrives here as an UNKNOWN KEY and is refused loudly — by
;; design, so the arm is threaded through deliberately.

;;; Code:

(require 'cl-lib)

;; wire-common.el (concurrent sibling module) owns the shared leaf codecs and
;; the `agent-repl-wire-error' definition.
(declare-function agent-repl-wire--fail "agent-repl-wire-common" (message field reason))
(declare-function agent-repl-wire--decoded "agent-repl-wire-common" (message-name value))
(declare-function agent-repl-wire--object "agent-repl-wire-common" (message-name value))
(declare-function agent-repl-wire--check-keys "agent-repl-wire-common" (message-name object allowed))
(declare-function agent-repl-wire--decode-oneof "agent-repl-wire-common"
                  (message-name oneof object arms &optional unset-legal))
(declare-function agent-repl-wire--encode-empty "agent-repl-wire-common" (message-name value))
(declare-function agent-repl-wire--decode-bool "agent-repl-wire-common" (message-name field object))
(declare-function agent-repl-wire--decode-int64 "agent-repl-wire-common" (message-name field object))
(declare-function agent-repl-wire--decode-uint32 "agent-repl-wire-common" (message-name field object))
(declare-function agent-repl-wire--decode-optional-string "agent-repl-wire-common" (message-name field object))
(declare-function agent-repl-wire--decode-optional-message "agent-repl-wire-common" (message-name field object decoder))
(declare-function agent-repl-wire--decode-message "agent-repl-wire-common" (message-name field object decoder))
(declare-function agent-repl-wire-decode-lock-holder-failure "agent-repl-wire-common" (value))
(declare-function agent-repl-wire--decode-double "agent-repl-wire-common" (message-name field object))
(declare-function agent-repl-wire-encode-workspace-ref "agent-repl-wire-common" (ref))
(declare-function agent-repl-wire-decode-workspace-ref "agent-repl-wire-common" (json))
(declare-function agent-repl-wire-encode-repository-ref "agent-repl-wire-common" (ref))
(declare-function agent-repl-wire-decode-repository-ref "agent-repl-wire-common" (json))
(declare-function agent-repl-wire-encode-user-said "agent-repl-wire-common" (said))
(declare-function agent-repl-wire-decode-user-said "agent-repl-wire-common" (value))
(declare-function agent-repl-wire-encode-prompt-origin "agent-repl-wire-common" (origin))
(declare-function agent-repl-wire-encode-drain-reason "agent-repl-wire-common" (reason))
(declare-function agent-repl-wire-encode-workspace-priority "agent-repl-wire-common" (priority))
(declare-function agent-repl-wire-decode-turn-id "agent-repl-wire-common" (json))
(declare-function agent-repl-wire-encode-turn-id "agent-repl-wire-common" (turn))
(declare-function agent-repl-wire-encode-feed-id "agent-repl-wire-common" (feedid))
(declare-function agent-repl-wire-decode-feed-id "agent-repl-wire-common" (json))
(declare-function agent-repl-wire-decode-session-fault-shim-start-failed "agent-repl-wire-common" (json))
(declare-function agent-repl-wire-decode-session-fault-shim-died "agent-repl-wire-common" (json))
(declare-function agent-repl-wire-decode-session-fault-link-severed "agent-repl-wire-common" (json))
(declare-function agent-repl-wire-decode-session-fault-resume-failed "agent-repl-wire-common" (json))
(declare-function agent-repl-wire-decode-session-fault-bounce-died "agent-repl-wire-common" (json))
(declare-function agent-repl-wire-decode-session-fault-bounce-unknown "agent-repl-wire-common" (json))
(declare-function agent-repl-wire-decode-session-fault-classifier-failed "agent-repl-wire-common" (json))
(declare-function agent-repl-wire-decode-session-fault-shim-reported "agent-repl-wire-common" (json))
(declare-function agent-repl-wire-decode-session-fault-conversation-abandoned "agent-repl-wire-common" (json))
(declare-function agent-repl-wire-decode-session-fault-session-absent "agent-repl-wire-common" (json))
(declare-function agent-repl-wire-decode-session-fault-watch-open-refused "agent-repl-wire-common" (json))
(declare-function agent-repl-wire-decode-session-fault-daemon-state-unreadable "agent-repl-wire-common" (json))
(declare-function agent-repl-wire-decode-session-fault-adoption-window-expired "agent-repl-wire-common" (json))
(declare-function agent-repl-wire-decode-session-fault-final-answer-unresolved "agent-repl-wire-common" (json))

;; core.el's canonical logging ladder.
(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--error "agent-repl-core" (ws fmt &rest args))


;;;; ---- Shared primitives ----------------------------------------------

(defun agent-repl-wire-verbs--fail (message field reason)
  "Log a contract breach at ERROR and signal `agent-repl-wire-error'.
MESSAGE names the protobuf message, FIELD the offending field or oneof,
REASON the breach.  Delegates to `agent-repl-wire--fail', which is the ONE
place in the codec that turns a breach into the typed signal; this file
keeps its own name only because every call site inside it reads better
that way."
  (agent-repl-wire--fail message field reason))

(defun agent-repl-wire-verbs--object (message json)
  "Return JSON when it is a decoded protojson object for MESSAGE, else fail.
`json-parse-string' with `:object-type alist' yields nil for `{}' and an
alist of (SYMBOL . VALUE) otherwise; anything else at a message position
is a contract breach."
  (if (or (null json)
          (and (consp json) (consp (car json)) (symbolp (caar json))))
      json
    (agent-repl-wire-verbs--fail message "-" "not a protojson object")))

(defun agent-repl-wire-verbs--check-keys (message json allowed)
  "Signal unless every key of JSON is in ALLOWED, for MESSAGE.
This is the unknown-field refusal: the same strictness a generated client
enforces, so a field the daemon adds without threading it here is loud
rather than silently dropped."
  (agent-repl-wire-verbs--object message json)
  (dolist (cell json)
    (unless (memq (car cell) allowed)
      (agent-repl-wire-verbs--fail message (symbol-name (car cell)) "unknown field")))
  json)

(defun agent-repl-wire-verbs--decode-empty (message json)
  "Decode an EMPTY protobuf MESSAGE from JSON, returning nil.
Any field at all is unknown, so this is the whole validation."
  (agent-repl-wire-verbs--check-keys message json nil)
  (agent-repl--log '(:agent-repl-context "a codec call outside a request has no workspace") "elisp.wire.verbs-decode-empty message=%s" message)
  nil)

(defun agent-repl-wire-verbs--decode-oneof (message field json arms)
  "Decode the oneof FIELD of MESSAGE from JSON into (:arm KEYWORD :value V).
ARMS is a list of (WIRE-SYMBOL KEYWORD DECODER).  Exactly one arm must be
set: none is an unset oneof, more than one is a malformed message, and
both are contract breaches."
  (let (found)
    (dolist (arm arms)
      (let ((cell (assq (car arm) json)))
        (when cell (push (cons arm cell) found))))
    (cond
     ((null found)
      (agent-repl-wire-verbs--fail message field "oneof is unset"))
     ((cdr found)
      (agent-repl-wire-verbs--fail message field "two oneof arms are set"))
     (t
      (let* ((hit (car found))
             (arm (car hit))
             (cell (cdr hit)))
        (agent-repl--log '(:agent-repl-context "a codec call outside a request has no workspace") "elisp.wire.verbs-decode-oneof message=%s field=%s arm=%s"
                          message field (nth 1 arm))
        (list :arm (nth 1 arm)
              :value (funcall (nth 2 arm) (cdr cell))))))))

(defun agent-repl-wire-verbs--decode-result (message json success error)
  "Decode MESSAGE's standard `result' oneof from JSON.
SUCCESS and ERROR are the two arms' use-site decoders."
  (agent-repl-wire-verbs--check-keys message json '(success error))
  (agent-repl-wire-verbs--decode-oneof
   message "result" json
   (list (list 'success :success success)
         (list 'error :error error))))

(defun agent-repl-wire-verbs--decode-accepting-result (message json success error accepted)
  "Decode MESSAGE's `result' oneof from JSON, with its option-B `accepted' arm.
A verb whose request carries an op id is answered `accepted' in place of
`success' (CreateWorkspace, KillWorkspace, NukeWorkspace): three arms, not
the standard two.  SUCCESS, ERROR and ACCEPTED are the arms' decoders."
  (agent-repl-wire-verbs--check-keys message json '(success error accepted))
  (agent-repl-wire-verbs--decode-oneof
   message "result" json
   (list (list 'success :success success)
         (list 'error :error error)
         (list 'accepted :accepted accepted))))

(defun agent-repl-wire-verbs--append-op-id (out request)
  "Return OUT, an encoded request alist, with REQUEST's optional op id last.
Every verb that carries a client-minted op id carries it the same way:
absent, the request is sent without it; present, it rides as `opId'."
  (if (plist-get request :op-id)
      (append out (list (cons 'opId (plist-get request :op-id))))
    out))

(defun agent-repl-wire-verbs--decode-string (message field json)
  "Decode the non-optional string FIELD of MESSAGE out of JSON.
protojson omits default-valued scalars, so an absent field is the proto3
default (the empty string); a present non-string is a breach."
  (let ((cell (assq field json)))
    (cond
     ((null cell) "")
     ((stringp (cdr cell)) (cdr cell))
     (t (agent-repl-wire-verbs--fail message (symbol-name field) "not a string")))))

(defun agent-repl-wire-verbs--decode-repeated (message field json decoder)
  "Decode the repeated message FIELD of MESSAGE out of JSON with DECODER.
An absent field is the empty list, protojson's spelling of `no elements'."
  (let ((cell (assq field json)))
    (cond
     ((null cell) nil)
     ((listp (cdr cell)) (mapcar decoder (cdr cell)))
     (t (agent-repl-wire-verbs--fail message (symbol-name field) "not an array")))))

(defun agent-repl-wire-verbs--decode-repeated-string (message field json)
  "Decode the repeated string FIELD of MESSAGE out of JSON as a list.
An absent field is the empty list, protojson's spelling of `no elements';
a non-string element is a contract breach."
  (let ((cell (assq field json)))
    (cond
     ((null cell) nil)
     ((listp (cdr cell))
      (dolist (element (cdr cell))
        (unless (stringp element)
          (agent-repl-wire-verbs--fail message (symbol-name field) "not a string")))
      (append (cdr cell) nil))
     (t (agent-repl-wire-verbs--fail message (symbol-name field) "not an array")))))

(defun agent-repl-wire-verbs--require (message field value)
  "Return VALUE, or fail because MESSAGE's non-optional FIELD is unset."
  (or value
      (agent-repl-wire-verbs--fail message field "required field is unset")))

(defun agent-repl-wire-verbs--decode-required-message (message field json decoder)
  "Decode MESSAGE's REQUIRED message FIELD (a symbol) out of JSON with DECODER.
An absent or null field is a contract breach."
  (let ((cell (assq field json)))
    (unless (and cell (not (eq (cdr cell) :null)))
      (agent-repl-wire-verbs--fail message (symbol-name field) "required field is unset"))
    (funcall decoder (cdr cell))))

(defun agent-repl-wire-verbs--decode-optional-message (message field json decoder)
  "Decode MESSAGE's OPTIONAL message FIELD (a symbol) out of JSON with DECODER.
Returns nil when the field is absent: presence is the fact.  MESSAGE names
the owner for the decoder's own diagnostics."
  (ignore message)
  (let ((cell (assq field json)))
    (when (and cell (not (eq (cdr cell) :null)))
      (funcall decoder (cdr cell)))))

(defun agent-repl-wire-verbs--require-string (message field value)
  "Return VALUE as MESSAGE's required non-blank string FIELD, or fail."
  (cond
   ((not (stringp value))
    (agent-repl-wire-verbs--fail message field "required field is unset"))
   ((string-empty-p value)
    (agent-repl-wire-verbs--fail message field "required string is empty"))
   (t value)))

(defun agent-repl-wire-verbs--decode-required-string (message field json)
  "Decode MESSAGE's REQUIRED string FIELD out of JSON; empty is a breach."
  (agent-repl-wire-verbs--require-string
   message (symbol-name field)
   (agent-repl-wire-verbs--decode-string message field json)))

(defun agent-repl-wire-verbs--encode-bool (value)
  "Encode elisp VALUE as a protojson bool, spelled explicitly."
  (if value t :false))

(defun agent-repl-wire-verbs--encode-int64 (message field value)
  "Encode VALUE as MESSAGE's int64 FIELD.
An integer rides the wire as a number, which Go's protojson accepts; a
decimal string is accepted from the caller (protojson's own int64
spelling) and normalized to that integer."
  (cond
   ((integerp value) value)
   ((and (stringp value) (string-match-p "\\`-?[0-9]+\\'" value))
    (string-to-number value))
   (t (agent-repl-wire-verbs--fail message field "not an int64"))))

(defun agent-repl-wire-verbs--encode-oneof (message field value arms)
  "Encode the oneof FIELD of MESSAGE from VALUE into a single protojson cell.
VALUE is (:arm KEYWORD :value V); ARMS is a list of (KEYWORD WIRE-SYMBOL
ENCODER).  An unset oneof and an unrecognized arm are contract breaches."
  (unless (and (consp value) (plist-member value :arm))
    (agent-repl-wire-verbs--fail message field "oneof is unset"))
  (let* ((keyword (plist-get value :arm))
         (arm (assq keyword arms)))
    (unless arm
      (agent-repl-wire-verbs--fail message field "unknown oneof arm"))
    (agent-repl--log '(:agent-repl-context "a codec call outside a request has no workspace") "elisp.wire.verbs-encode-oneof message=%s field=%s arm=%s"
                      message field keyword)
    (cons (nth 1 arm) (funcall (nth 2 arm) (plist-get value :value)))))

;;;; ---- CreateWorkspace: encode ----------------------------------------

(defun agent-repl-wire-encode-create-workspace-fork (_present)
  "Encode CreateWorkspaceFork.  Empty: presence IS the fork fact."
  nil)

(defun agent-repl-wire-encode-create-workspace-ungated-consent (_present)
  "Encode CreateWorkspaceUngatedConsent.  Empty: presence IS the consent."
  nil)

(defun agent-repl-wire-encode-create-workspace-one-shot-prompt (said)
  "Encode CreateWorkspaceOneShot's `prompt' use site from SAID."
  (agent-repl-wire-encode-user-said said))

(defun agent-repl-wire-encode-create-workspace-one-shot (value)
  "Encode CreateWorkspaceOneShot from plist VALUE.
VALUE is (:prompt SAID).  A one-shot IS its prompt, so the prompt is
required and it is the whole form: there is no finish choice, because what
happens on completion is the REPOSITORY\='s own directive, which the daemon
appends to the commission and the agent carries out."
  (let ((message "CreateWorkspaceOneShot"))
    (list (cons 'prompt
                (agent-repl-wire-encode-create-workspace-one-shot-prompt
                 (agent-repl-wire-verbs--require message "prompt" (plist-get value :prompt)))))))

(defun agent-repl-wire-encode-create-workspace-merge-actions-before-ws-merge (said)
  "Encode CreateWorkspaceMergeActions's `before_ws_merge' use site from SAID."
  (agent-repl-wire-encode-user-said said))

(defun agent-repl-wire-encode-create-workspace-merge-actions-postprocessing-prompt (said)
  "Encode CreateWorkspaceMergeActions's `postprocessing_prompt' use site from
SAID."
  (agent-repl-wire-encode-user-said said))

(defun agent-repl-wire-encode-create-workspace-merge-actions (value)
  "Encode CreateWorkspaceMergeActions from plist VALUE.
Both actions are optional; absence is the absence of a configured action,
never an empty UserSaid."
  (let (out)
    (when (plist-get value :before-ws-merge)
      (push (cons 'beforeWsMerge
                  (agent-repl-wire-encode-create-workspace-merge-actions-before-ws-merge
                   (plist-get value :before-ws-merge)))
            out))
    (when (plist-get value :postprocessing-prompt)
      (push (cons 'postprocessingPrompt
                  (agent-repl-wire-encode-create-workspace-merge-actions-postprocessing-prompt
                   (plist-get value :postprocessing-prompt)))
            out))
    (nreverse out)))

(defun agent-repl-wire-encode-create-workspace-standard-initial-prompt (said)
  "Encode CreateWorkspaceStandard's `initial_prompt' use site from SAID."
  (agent-repl-wire-encode-user-said said))

(defun agent-repl-wire-encode-create-workspace-standard-merge-actions (value)
  "Encode CreateWorkspaceStandard's `merge_actions' use site from VALUE."
  (agent-repl-wire-encode-create-workspace-merge-actions value))

(defun agent-repl-wire-encode-create-workspace-standard (value)
  "Encode CreateWorkspaceStandard from plist VALUE.
Every field is optional: an absent initial prompt is an empty workspace,
an absent base ref is the repo's default resolution, an absent name means
the daemon mints one, and absent merge actions mean none are configured."
  (let (out)
    (when (plist-get value :initial-prompt)
      (push (cons 'initialPrompt
                  (agent-repl-wire-encode-create-workspace-standard-initial-prompt
                   (plist-get value :initial-prompt)))
            out))
    (when (plist-get value :base-ref)
      (push (cons 'baseRef (plist-get value :base-ref)) out))
    (when (plist-get value :name)
      (push (cons 'name (plist-get value :name)) out))
    (when (plist-get value :merge-actions)
      (push (cons 'mergeActions
                  (agent-repl-wire-encode-create-workspace-standard-merge-actions
                   (plist-get value :merge-actions)))
            out))
    (nreverse out)))

(defun agent-repl-wire-encode-create-workspace-parent-workspace (ref)
  "Encode CreateWorkspaceParent's `workspace' use site from REF."
  (agent-repl-wire-encode-workspace-ref ref))

(defun agent-repl-wire-encode-create-workspace-parent-fork (present)
  "Encode CreateWorkspaceParent's `fork' use site from PRESENT."
  (agent-repl-wire-encode-create-workspace-fork present))

(defun agent-repl-wire-encode-create-workspace-parent (value)
  "Encode CreateWorkspaceParent from plist VALUE.
VALUE is (:workspace REF :fork PRESENT-FLAG).  The parent workspace is
required; the fork flag lives HERE by construction, which is exactly why a
fork without a parent is unrepresentable."
  (let ((out (list (cons 'workspace
                         (agent-repl-wire-encode-create-workspace-parent-workspace
                          (agent-repl-wire-verbs--require
                           "CreateWorkspaceParent" "workspace"
                           (plist-get value :workspace)))))))
    (when (plist-get value :fork)
      (setq out (append out (list (cons 'fork
                                        (agent-repl-wire-encode-create-workspace-parent-fork
                                         (plist-get value :fork)))))))
    out))

(defun agent-repl-wire-encode-create-workspace-request-repository (ref)
  "Encode CreateWorkspaceRequest's `repository' use site from REF."
  (agent-repl-wire-encode-repository-ref ref))

(defun agent-repl-wire-encode-create-workspace-request-standard (value)
  "Encode CreateWorkspaceRequest's `standard' form arm from VALUE."
  (agent-repl-wire-encode-create-workspace-standard value))

(defun agent-repl-wire-encode-create-workspace-request-one-shot (value)
  "Encode CreateWorkspaceRequest's `one_shot' form arm from VALUE."
  (agent-repl-wire-encode-create-workspace-one-shot value))

(defun agent-repl-wire-encode-create-workspace-request-parent (value)
  "Encode CreateWorkspaceRequest's `parent' use site from VALUE."
  (agent-repl-wire-encode-create-workspace-parent value))

(defun agent-repl-wire-encode-create-workspace-request-priority (priority)
  "Encode CreateWorkspaceRequest's `priority' use site from PRIORITY."
  (agent-repl-wire-encode-workspace-priority priority))

(defun agent-repl-wire-encode-create-workspace-request-allow-ungated (present)
  "Encode CreateWorkspaceRequest's `allow_ungated' use site from PRESENT."
  (agent-repl-wire-encode-create-workspace-ungated-consent present))

(defun agent-repl-wire-encode-create-workspace-request (request)
  "Encode CreateWorkspaceRequest from plist REQUEST.
REQUEST is (:repository REPO-REF :form ONEOF :parent PARENT :model STRING
:priority PRIORITY :allow-ungated FLAG).  The repository and the form arm
are required — the arm IS the creation form, so a request without one is
refused before it can be sent.  Everything else is optional and absent
means what the proto says absence means: the daemon's default model, an
unprioritized workspace, a top-level workspace, no ungated consent."
  (let ((message "CreateWorkspaceRequest")
        out)
    (push (cons 'repository
                (agent-repl-wire-encode-create-workspace-request-repository
                 (agent-repl-wire-verbs--require message "repository"
                                                  (plist-get request :repository))))
          out)
    (push (agent-repl-wire-verbs--encode-oneof
           message "form" (plist-get request :form)
           (list (list :standard 'standard
                       #'agent-repl-wire-encode-create-workspace-request-standard)
                 (list :one-shot 'oneShot
                       #'agent-repl-wire-encode-create-workspace-request-one-shot)))
          out)
    (when (plist-get request :parent)
      (push (cons 'parent (agent-repl-wire-encode-create-workspace-request-parent
                           (plist-get request :parent)))
            out))
    (when (plist-get request :model)
      (push (cons 'model (plist-get request :model)) out))
    (when (plist-get request :priority)
      (push (cons 'priority (agent-repl-wire-encode-create-workspace-request-priority
                             (plist-get request :priority)))
            out))
    (when (plist-get request :allow-ungated)
      (push (cons 'allowUngated
                  (agent-repl-wire-encode-create-workspace-request-allow-ungated
                   (plist-get request :allow-ungated)))
            out))
    (agent-repl--log '(:agent-repl-context "a codec call outside a request has no workspace") "elisp.wire.verbs-encode-create-workspace-request form=%s"
                      (plist-get (plist-get request :form) :arm))
    ;; THE OP ID OPTS INTO OPTION B. Present, the daemon acks at once and pushes
    ;; this create's progress on WatchDaemon keyed on it; absent, the create is
    ;; the legacy synchronous form.
    (agent-repl-wire-verbs--append-op-id (nreverse out) request)))


;;;; ---- CreateWorkspace: decode ----------------------------------------

(defun agent-repl-wire-decode-create-workspace-success-workspace (json)
  "Decode CreateWorkspaceSuccess's `workspace' use site from JSON."
  (agent-repl-wire-decode-workspace-ref json))

(defun agent-repl-wire-decode-create-workspace-success (json)
  "Decode CreateWorkspaceSuccess from JSON into (:workspace REF).
`workspace' is non-optional: a success without the minted identity is a
contract breach, not an empty success."
  (let ((message "CreateWorkspaceSuccess"))
    (agent-repl-wire-verbs--check-keys message json '(workspace))
    (list :workspace
          (agent-repl-wire-decode-create-workspace-success-workspace
           (agent-repl-wire-verbs--require message "workspace" (cdr (assq 'workspace json)))))))

(defun agent-repl-wire-decode-create-workspace-ungated-without-consent (json)
  "Decode CreateWorkspaceUngatedWithoutConsent from JSON.  Empty: An ungated
permission mode was asked for without the explicit consent."
  (agent-repl-wire-verbs--decode-empty "CreateWorkspaceUngatedWithoutConsent" json))

(defun agent-repl-wire-decode-create-workspace-no-slug (json)
  "Decode CreateWorkspaceNoSlug from JSON.  Empty: No slug could be derived for
the workspace's branch and dir."
  (agent-repl-wire-verbs--decode-empty "CreateWorkspaceNoSlug" json))

(defun agent-repl-wire-decode-create-workspace-fork-parent-has-no-conversation (json)
  "Decode CreateWorkspaceForkParentHasNoConversation from JSON.  Empty: A fork
was asked for from a parent that has no conversation to fork."
  (agent-repl-wire-verbs--decode-empty "CreateWorkspaceForkParentHasNoConversation" json))

(defun agent-repl-wire-decode-create-workspace-brief-missing (json)
  "Decode CreateWorkspaceBriefMissing from JSON into a plist (`:name').
The named brief file is absent."
  (let ((message "CreateWorkspaceBriefMissing"))
    (agent-repl-wire-verbs--check-keys message json '(name))
    (list :name (agent-repl-wire-verbs--decode-string
                       message 'name json))))

(defun agent-repl-wire-decode-create-workspace-unknown-repository (json)
  "Decode CreateWorkspaceUnknownRepository from JSON.  Empty: The repository
the form names is not one the daemon knows."
  (agent-repl-wire-verbs--decode-empty "CreateWorkspaceUnknownRepository" json))

(defun agent-repl-wire-decode-create-workspace-unknown-parent (json)
  "Decode CreateWorkspaceUnknownParent from JSON.  Empty: The parent workspace
is not in the daemon's registry."
  (agent-repl-wire-verbs--decode-empty "CreateWorkspaceUnknownParent" json))

(defun agent-repl-wire-decode-create-workspace-base-ref-unresolved (json)
  "Decode CreateWorkspaceBaseRefUnresolved from JSON into a plist (`:ref').
The base ref does not resolve in the repository."
  (let ((message "CreateWorkspaceBaseRefUnresolved"))
    (agent-repl-wire-verbs--check-keys message json '(ref))
    (list :ref (agent-repl-wire-verbs--decode-string
                       message 'ref json))))

(defun agent-repl-wire-decode-create-workspace-worktree-creation-failed (json)
  "Decode CreateWorkspaceWorktreeCreationFailed from JSON into a plist
(`:detail').
Creating the worktree failed."
  (let ((message "CreateWorkspaceWorktreeCreationFailed"))
    (agent-repl-wire-verbs--check-keys message json '(detail))
    (list :detail (agent-repl-wire-verbs--decode-string
                       message 'detail json))))

(defun agent-repl-wire-decode-create-workspace-spawn-failed (json)
  "Decode CreateWorkspaceSpawnFailed from JSON into a plist (`:detail\').
The created workspace's bring-up could not start a shim.  The SAME daemon
refusal `OpenWorkspaceSpawnFailed\' carries, and deliberately the same
shape: a create and an open raise it from one site."
  (let ((message "CreateWorkspaceSpawnFailed"))
    (agent-repl-wire-verbs--check-keys message json '(detail))
    (list :detail (agent-repl-wire-verbs--decode-string
                       message 'detail json))))

(defun agent-repl-wire-decode-create-workspace-one-shot-policy-missing (json)
  "Decode CreateWorkspaceOneShotPolicyMissing from JSON into a plist
(`:repository-root' `:policy-dir' `:missing-files').
A one-shot was asked for in a repository that states no one-shot policy
of its own.  A repository declares its policy in `.agent-repl/prompts\='
at its main checkout root; the daemon\='s own corpus is the policy of
exactly one repository and is never a fallback for another."
  (let ((message "CreateWorkspaceOneShotPolicyMissing"))
    (agent-repl-wire-verbs--check-keys message json '(repositoryRoot policyDir missingFiles))
    (list :repository-root (agent-repl-wire-verbs--decode-string
                       message 'repositoryRoot json)
          :policy-dir (agent-repl-wire-verbs--decode-string
                       message 'policyDir json)
          :missing-files (agent-repl-wire-verbs--decode-repeated-string
                       message 'missingFiles json))))

(defun agent-repl-wire-decode-create-workspace-naming-failed (json)
  "Decode CreateWorkspaceNamingFailed from JSON into a plist
(`:model\=' `:cause\=' `:attempts\=' `:answer\=').
EVERY DYNAMICALLY CREATED WORKSPACE IS NAMED BY THE MODEL, and this
create supplied no name and the naming call could not mint one.  There
is no word-truncation fallback, so the create is refused: `answer\=' is
the last thing the model said, when it said anything at all."
  (let ((message "CreateWorkspaceNamingFailed"))
    (agent-repl-wire-verbs--check-keys message json '(model cause attempts answer))
    (list :model (agent-repl-wire-verbs--decode-string
                       message 'model json)
          :cause (agent-repl-wire-verbs--decode-string
                       message 'cause json)
          :attempts (agent-repl-wire--decode-uint32 message 'attempts json)
          :answer (agent-repl-wire-verbs--decode-string
                       message 'answer json))))

(defun agent-repl-wire-decode-create-workspace-error-ungated-without-consent (json)
  "Decode CreateWorkspaceError's `ungated_without_consent' cause arm from JSON."
  (agent-repl-wire-decode-create-workspace-ungated-without-consent json))

(defun agent-repl-wire-decode-create-workspace-error-no-slug (json)
  "Decode CreateWorkspaceError's `no_slug' cause arm from JSON."
  (agent-repl-wire-decode-create-workspace-no-slug json))

(defun agent-repl-wire-decode-create-workspace-error-fork-parent-has-no-conversation (json)
  "Decode CreateWorkspaceError's `fork_parent_has_no_conversation' cause arm
from JSON."
  (agent-repl-wire-decode-create-workspace-fork-parent-has-no-conversation json))

(defun agent-repl-wire-decode-create-workspace-error-brief-missing (json)
  "Decode CreateWorkspaceError's `brief_missing' cause arm from JSON."
  (agent-repl-wire-decode-create-workspace-brief-missing json))

(defun agent-repl-wire-decode-create-workspace-error-unknown-repository (json)
  "Decode CreateWorkspaceError's `unknown_repository' cause arm from JSON."
  (agent-repl-wire-decode-create-workspace-unknown-repository json))

(defun agent-repl-wire-decode-create-workspace-error-unknown-parent (json)
  "Decode CreateWorkspaceError's `unknown_parent' cause arm from JSON."
  (agent-repl-wire-decode-create-workspace-unknown-parent json))

(defun agent-repl-wire-decode-create-workspace-error-base-ref-unresolved (json)
  "Decode CreateWorkspaceError's `base_ref_unresolved' cause arm from JSON."
  (agent-repl-wire-decode-create-workspace-base-ref-unresolved json))

(defun agent-repl-wire-decode-create-workspace-error-worktree-creation-failed (json)
  "Decode CreateWorkspaceError's `worktree_creation_failed' cause arm from
JSON."
  (agent-repl-wire-decode-create-workspace-worktree-creation-failed json))

(defun agent-repl-wire-decode-create-workspace-error-spawn-failed (json)
  "Decode CreateWorkspaceError's `spawn_failed' cause arm from JSON."
  (agent-repl-wire-decode-create-workspace-spawn-failed json))

(defun agent-repl-wire-decode-create-workspace-error-one-shot-policy-missing (json)
  "Decode CreateWorkspaceError's `one_shot_policy_missing' cause arm from JSON."
  (agent-repl-wire-decode-create-workspace-one-shot-policy-missing json))

(defun agent-repl-wire-decode-create-workspace-error-naming-failed (json)
  "Decode CreateWorkspaceError's `naming_failed' cause arm from JSON."
  (agent-repl-wire-decode-create-workspace-naming-failed json))

(defun agent-repl-wire-decode-create-workspace-error (json)
  "Decode CreateWorkspaceError from JSON into (:cause (:arm ARM :value V)).
THE ARM IS THE REFUSAL, so an unset cause is a contract breach and an
arm this codec does not know is refused as an unknown field."
  (let ((message "CreateWorkspaceError"))
    (agent-repl-wire-verbs--check-keys message json '(ungatedWithoutConsent noSlug forkParentHasNoConversation briefMissing unknownRepository unknownParent baseRefUnresolved worktreeCreationFailed spawnFailed oneShotPolicyMissing namingFailed))
    (list :cause
          (agent-repl-wire-verbs--decode-oneof
           message "cause" json
           (list (list 'ungatedWithoutConsent :ungated-without-consent #'agent-repl-wire-decode-create-workspace-error-ungated-without-consent)
         (list 'noSlug :no-slug #'agent-repl-wire-decode-create-workspace-error-no-slug)
         (list 'forkParentHasNoConversation :fork-parent-has-no-conversation #'agent-repl-wire-decode-create-workspace-error-fork-parent-has-no-conversation)
         (list 'briefMissing :brief-missing #'agent-repl-wire-decode-create-workspace-error-brief-missing)
         (list 'unknownRepository :unknown-repository #'agent-repl-wire-decode-create-workspace-error-unknown-repository)
         (list 'unknownParent :unknown-parent #'agent-repl-wire-decode-create-workspace-error-unknown-parent)
         (list 'baseRefUnresolved :base-ref-unresolved #'agent-repl-wire-decode-create-workspace-error-base-ref-unresolved)
         (list 'worktreeCreationFailed :worktree-creation-failed #'agent-repl-wire-decode-create-workspace-error-worktree-creation-failed)
         (list 'spawnFailed :spawn-failed #'agent-repl-wire-decode-create-workspace-error-spawn-failed)
         (list 'oneShotPolicyMissing :one-shot-policy-missing #'agent-repl-wire-decode-create-workspace-error-one-shot-policy-missing)
         (list 'namingFailed :naming-failed #'agent-repl-wire-decode-create-workspace-error-naming-failed))))))

(defun agent-repl-wire-decode-create-workspace-response-success (json)
  "Decode CreateWorkspaceResponse's `success' arm from JSON."
  (agent-repl-wire-decode-create-workspace-success json))

(defun agent-repl-wire-decode-create-workspace-response-error (json)
  "Decode CreateWorkspaceResponse's `error' arm from JSON."
  (agent-repl-wire-decode-create-workspace-error json))

(defun agent-repl-wire-decode-create-workspace-accepted (json)
  "Decode CreateWorkspaceAccepted from JSON into (:op-id ID).
The option-B ack: the create was accepted and detached to the background,
so the real outcome arrives on the WatchDaemon progress channel, not here."
  (list :op-id
        (agent-repl-wire-verbs--decode-string "CreateWorkspaceAccepted" 'opId json)))

(defun agent-repl-wire-decode-create-workspace-response-accepted (json)
  "Decode CreateWorkspaceResponse's `accepted' arm from JSON."
  (agent-repl-wire-decode-create-workspace-accepted json))

(defun agent-repl-wire-decode-create-workspace-response (json)
  "Decode CreateWorkspaceResponse from JSON into (:arm ARM :value V).
THREE arms, not the standard two: `accepted' is the option-B ack answered
when the request carried an op_id, in place of `success'/`error'."
  (agent-repl-wire-verbs--decode-accepting-result
   "CreateWorkspaceResponse" json
   #'agent-repl-wire-decode-create-workspace-response-success
   #'agent-repl-wire-decode-create-workspace-response-error
   #'agent-repl-wire-decode-create-workspace-response-accepted))


;;;; ---- OpenWorkspace --------------------------------------------------

(defun agent-repl-wire-encode-open-workspace-request-workspace (ref)
  "Encode OpenWorkspaceRequest's `workspace' use site from REF."
  (agent-repl-wire-encode-workspace-ref ref))

(defun agent-repl-wire-encode-open-workspace-request (request)
  "Encode OpenWorkspaceRequest from plist REQUEST (:workspace REF :op-id ID).
The OP ID IS OPTIONAL and changes nothing about how the rpc answers:
present, the daemon also pushes this open\='s stages on WatchDaemon keyed
on it; absent, the open reports only its terminal answer."
  (agent-repl--log '(:agent-repl-context "a codec call outside a request has no workspace") "elisp.wire.verbs-encode-open-workspace-request")
  (let ((out (list (cons 'workspace
                         (agent-repl-wire-encode-open-workspace-request-workspace
                          (agent-repl-wire-verbs--require "OpenWorkspaceRequest" "workspace"
                                                          (plist-get request :workspace)))))))
    (agent-repl-wire-verbs--append-op-id out request)))

(defun agent-repl-wire-decode-open-workspace-success (json)
  "Decode OpenWorkspaceSuccess from JSON.  Empty: the effects ride the streams."
  (agent-repl-wire-verbs--decode-empty "OpenWorkspaceSuccess" json))

(defun agent-repl-wire-decode-open-workspace-unknown-workspace (json)
  "Decode OpenWorkspaceUnknownWorkspace from JSON.  Empty: The workspace id is
not in the daemon's registry."
  (agent-repl-wire-verbs--decode-empty "OpenWorkspaceUnknownWorkspace" json))

(defun agent-repl-wire-decode-open-workspace-workspace-ref-mismatch (json)
  "Decode OpenWorkspaceWorkspaceRefMismatch from JSON into a plist (`:registry-
dir').
The echoed dir disagrees with the registry's dir for this id."
  (let ((message "OpenWorkspaceWorkspaceRefMismatch"))
    (agent-repl-wire-verbs--check-keys message json '(registryDir))
    (list :registry-dir (agent-repl-wire-verbs--decode-string
                       message 'registryDir json))))

(defun agent-repl-wire-decode-open-workspace-transferring-away (json)
  "Decode OpenWorkspaceTransferringAway from JSON into a plist (`:address').
This daemon released the workspace to a successor; dial `address'."
  (let ((message "OpenWorkspaceTransferringAway"))
    (agent-repl-wire-verbs--check-keys message json '(address))
    (list :address (agent-repl-wire-verbs--decode-string
                       message 'address json))))

(defun agent-repl-wire-decode-open-workspace-not-yet-adopted (json)
  "Decode OpenWorkspaceNotYetAdopted from JSON.  Empty: A joining daemon has
not finished adopting this workspace yet."
  (agent-repl-wire-verbs--decode-empty "OpenWorkspaceNotYetAdopted" json))

(defun agent-repl-wire-decode-open-workspace-session-deleted (json)
  "Decode OpenWorkspaceSessionDeleted from JSON.  Empty: The workspace's
session has been deleted and cannot be reopened."
  (agent-repl-wire-verbs--decode-empty "OpenWorkspaceSessionDeleted" json))

(defun agent-repl-wire-decode-open-workspace-transcript-missing (json)
  "Decode OpenWorkspaceTranscriptMissing from JSON into a plist (`:vendor-
session-id' `:searched-paths').
The vendor transcript to resume from is nowhere on disk."
  (let ((message "OpenWorkspaceTranscriptMissing"))
    (agent-repl-wire-verbs--check-keys message json '(vendorSessionId searchedPaths))
    (list :vendor-session-id (agent-repl-wire-verbs--decode-string
                       message 'vendorSessionId json)
          :searched-paths (agent-repl-wire-verbs--decode-repeated-string
                       message 'searchedPaths json))))

(defun agent-repl-wire-decode-open-workspace-spawn-failed (json)
  "Decode OpenWorkspaceSpawnFailed from JSON into a plist (`:detail').
Spawning the session's shim failed."
  (let ((message "OpenWorkspaceSpawnFailed"))
    (agent-repl-wire-verbs--check-keys message json '(detail))
    (list :detail (agent-repl-wire-verbs--decode-string
                       message 'detail json))))

(defun agent-repl-wire-decode-open-workspace-vendor-start-failed (json)
  "Decode OpenWorkspaceVendorStartFailed from JSON into a plist (`:detail\').
The shim came up but the VENDOR failed to start the session."
  (let ((message "OpenWorkspaceVendorStartFailed"))
    (agent-repl-wire-verbs--check-keys message json '(detail))
    (list :detail (agent-repl-wire-verbs--decode-string
                       message 'detail json))))

(defun agent-repl-wire-decode-open-workspace-lock-holder-unavailable-failure (json)
  "Decode OpenWorkspaceLockHolderUnavailable's `failure' field from JSON."
  (agent-repl-wire-decode-lock-holder-failure json))

(defun agent-repl-wire-decode-open-workspace-lock-holder-unavailable (json)
  "Decode OpenWorkspaceLockHolderUnavailable from JSON into a plist
\(`:failure'), a decoded `conversation.v1.LockHolderFailure'.
The session's shim's own kernel-lock holder failed: a broken lock helper,
never an ownership conflict."
  (let ((message "OpenWorkspaceLockHolderUnavailable"))
    (agent-repl-wire-verbs--check-keys message json '(failure))
    (list :failure (agent-repl-wire--decode-message
                    message 'failure json
                    #'agent-repl-wire-decode-open-workspace-lock-holder-unavailable-failure))))

(defun agent-repl-wire-decode-open-workspace-error-unknown-workspace (json)
  "Decode OpenWorkspaceError's `unknown_workspace' cause arm from JSON."
  (agent-repl-wire-decode-open-workspace-unknown-workspace json))

(defun agent-repl-wire-decode-open-workspace-error-workspace-ref-mismatch (json)
  "Decode OpenWorkspaceError's `workspace_ref_mismatch' cause arm from JSON."
  (agent-repl-wire-decode-open-workspace-workspace-ref-mismatch json))

(defun agent-repl-wire-decode-open-workspace-error-transferring-away (json)
  "Decode OpenWorkspaceError's `transferring_away' cause arm from JSON."
  (agent-repl-wire-decode-open-workspace-transferring-away json))

(defun agent-repl-wire-decode-open-workspace-error-not-yet-adopted (json)
  "Decode OpenWorkspaceError's `not_yet_adopted' cause arm from JSON."
  (agent-repl-wire-decode-open-workspace-not-yet-adopted json))

(defun agent-repl-wire-decode-open-workspace-error-session-deleted (json)
  "Decode OpenWorkspaceError's `session_deleted' cause arm from JSON."
  (agent-repl-wire-decode-open-workspace-session-deleted json))

(defun agent-repl-wire-decode-open-workspace-error-transcript-missing (json)
  "Decode OpenWorkspaceError's `transcript_missing' cause arm from JSON."
  (agent-repl-wire-decode-open-workspace-transcript-missing json))

(defun agent-repl-wire-decode-open-workspace-error-spawn-failed (json)
  "Decode OpenWorkspaceError's `spawn_failed' cause arm from JSON."
  (agent-repl-wire-decode-open-workspace-spawn-failed json))

(defun agent-repl-wire-decode-open-workspace-error-vendor-start-failed (json)
  "Decode OpenWorkspaceError's `vendor_start_failed\' cause arm from JSON."
  (agent-repl-wire-decode-open-workspace-vendor-start-failed json))

(defun agent-repl-wire-decode-open-workspace-error-lock-holder-unavailable (json)
  "Decode OpenWorkspaceError's `lock_holder_unavailable' cause arm from JSON."
  (agent-repl-wire-decode-open-workspace-lock-holder-unavailable json))

(defun agent-repl-wire-decode-open-workspace-error (json)
  "Decode OpenWorkspaceError from JSON into (:cause (:arm ARM :value V)).
THE ARM IS THE REFUSAL, so an unset cause is a contract breach and an
arm this codec does not know is refused as an unknown field."
  (let ((message "OpenWorkspaceError"))
    (agent-repl-wire-verbs--check-keys message json '(unknownWorkspace workspaceRefMismatch transferringAway notYetAdopted sessionDeleted transcriptMissing spawnFailed vendorStartFailed lockHolderUnavailable))
    (list :cause
          (agent-repl-wire-verbs--decode-oneof
           message "cause" json
           (list (list 'unknownWorkspace :unknown-workspace #'agent-repl-wire-decode-open-workspace-error-unknown-workspace)
         (list 'workspaceRefMismatch :workspace-ref-mismatch #'agent-repl-wire-decode-open-workspace-error-workspace-ref-mismatch)
         (list 'transferringAway :transferring-away #'agent-repl-wire-decode-open-workspace-error-transferring-away)
         (list 'notYetAdopted :not-yet-adopted #'agent-repl-wire-decode-open-workspace-error-not-yet-adopted)
         (list 'sessionDeleted :session-deleted #'agent-repl-wire-decode-open-workspace-error-session-deleted)
         (list 'transcriptMissing :transcript-missing #'agent-repl-wire-decode-open-workspace-error-transcript-missing)
         (list 'spawnFailed :spawn-failed #'agent-repl-wire-decode-open-workspace-error-spawn-failed)
         (list 'vendorStartFailed :vendor-start-failed #'agent-repl-wire-decode-open-workspace-error-vendor-start-failed)
         (list 'lockHolderUnavailable :lock-holder-unavailable #'agent-repl-wire-decode-open-workspace-error-lock-holder-unavailable))))))

(defun agent-repl-wire-decode-open-workspace-response-success (json)
  "Decode OpenWorkspaceResponse's `success' arm from JSON."
  (agent-repl-wire-decode-open-workspace-success json))

(defun agent-repl-wire-decode-open-workspace-response-error (json)
  "Decode OpenWorkspaceResponse's `error' arm from JSON."
  (agent-repl-wire-decode-open-workspace-error json))

(defun agent-repl-wire-decode-open-workspace-response (json)
  "Decode OpenWorkspaceResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-result
   "OpenWorkspaceResponse" json
   #'agent-repl-wire-decode-open-workspace-response-success
   #'agent-repl-wire-decode-open-workspace-response-error))


;;;; ---- ListWorkspaceTranscripts --------------------------------------

(defun agent-repl-wire-encode-list-workspace-transcripts-request-workspace (ref)
  "Encode ListWorkspaceTranscriptsRequest's `workspace' use site from REF."
  (agent-repl-wire-encode-workspace-ref ref))

(defun agent-repl-wire-encode-list-workspace-transcripts-request (request)
  "Encode ListWorkspaceTranscriptsRequest from plist REQUEST (:workspace REF)."
  (agent-repl--log '(:agent-repl-context "a codec call outside a request has no workspace") "elisp.wire.verbs-encode-list-workspace-transcripts-request")
  (list (cons 'workspace
              (agent-repl-wire-encode-list-workspace-transcripts-request-workspace
               (agent-repl-wire-verbs--require "ListWorkspaceTranscriptsRequest" "workspace"
                                               (plist-get request :workspace))))))

(defun agent-repl-wire-decode-workspace-transcript-last-model (json)
  "Decode WorkspaceTranscript's `last_model' use site from JSON.
`conversation.v1.AgentModel' carries the model id and nothing else."
  (let ((message "AgentModel"))
    (agent-repl-wire-verbs--check-keys message json '(name))
    (list :name (agent-repl-wire-verbs--decode-string message 'name json))))

(defun agent-repl-wire-decode-workspace-transcript-current (json)
  "Decode WorkspaceTranscriptCurrent from JSON.  Empty: presence IS the fact —
this is the conversation the workspace runs NOW."
  (agent-repl-wire-verbs--decode-empty "WorkspaceTranscriptCurrent" json)
  t)

(defun agent-repl-wire-decode-workspace-transcript-cleared (json)
  "Decode WorkspaceTranscriptCleared from JSON into a plist (`:at-ms').
The conversation's most recent boundary is a context clear, so it resumes
empty rather than at `context_tokens'."
  (let ((message "WorkspaceTranscriptCleared"))
    (agent-repl-wire-verbs--check-keys message json '(atMs))
    (list :at-ms (agent-repl-wire--decode-int64 message 'atMs json))))

(defun agent-repl-wire-decode-workspace-transcript-active (json)
  "Decode WorkspaceTranscriptActive from JSON into a plist (`:at-ms').
Something is writing to the transcript right now; a bind is refused on it."
  (let ((message "WorkspaceTranscriptActive"))
    (agent-repl-wire-verbs--check-keys message json '(atMs))
    (list :at-ms (agent-repl-wire--decode-int64 message 'atMs json))))

(defun agent-repl-wire-decode-workspace-transcript-held (json)
  "Decode WorkspaceTranscriptHeld from JSON into a plist (`:workspace').
ANOTHER workspace in the daemon's registry is bound to this conversation,
and the arm NAMES it so a client can say which."
  (let ((message "WorkspaceTranscriptHeld"))
    (agent-repl-wire-verbs--check-keys message json '(workspace))
    (list :workspace (agent-repl-wire-decode-workspace-ref
                      (cdr (assq 'workspace json))))))

(defun agent-repl-wire-decode-workspace-transcript (json)
  "Decode one WorkspaceTranscript from JSON into a plist.
Keys: `:vendor-session-id' `:last-request-at-ms' `:context-tokens'
`:last-model' `:opening' `:prompts' `:current' `:cleared' `:active'
`:held'.

EVERY OPTIONAL FIELD DECODES TO nil WHEN THE TRANSCRIPT STATED NOTHING,
never to a zero: a chooser that cannot tell an absence from a zero ranks
the conversation nobody could read as the smallest one."
  (let ((message "WorkspaceTranscript"))
    (agent-repl-wire-verbs--check-keys
     message json
     '(vendorSessionId lastRequestAtMs contextTokens lastModel opening prompts
       current cleared active held))
    (list :vendor-session-id (agent-repl-wire-verbs--decode-string message 'vendorSessionId json)
          :last-request-at-ms (when (assq 'lastRequestAtMs json)
                                (agent-repl-wire--decode-int64 message 'lastRequestAtMs json))
          :context-tokens (when (assq 'contextTokens json)
                            (agent-repl-wire--decode-int64 message 'contextTokens json))
          :last-model (agent-repl-wire--decode-optional-message
                       message 'lastModel json
                       #'agent-repl-wire-decode-workspace-transcript-last-model)
          :opening (agent-repl-wire--decode-optional-string message 'opening json)
          :prompts (agent-repl-wire--decode-uint32 message 'prompts json)
          :current (agent-repl-wire--decode-optional-message
                    message 'current json
                    #'agent-repl-wire-decode-workspace-transcript-current)
          :cleared (agent-repl-wire--decode-optional-message
                    message 'cleared json
                    #'agent-repl-wire-decode-workspace-transcript-cleared)
          :active (agent-repl-wire--decode-optional-message
                   message 'active json
                   #'agent-repl-wire-decode-workspace-transcript-active)
          :held (agent-repl-wire--decode-optional-message
                 message 'held json
                 #'agent-repl-wire-decode-workspace-transcript-held))))

(defun agent-repl-wire-decode-list-workspace-transcripts-success (json)
  "Decode ListWorkspaceTranscriptsSuccess from JSON into (:transcripts LIST).
An EMPTY list is a success: a directory with no conversations is an answer."
  (let ((message "ListWorkspaceTranscriptsSuccess"))
    (agent-repl-wire-verbs--check-keys message json '(transcripts))
    (list :transcripts (agent-repl-wire-verbs--decode-repeated
                        message 'transcripts json
                        #'agent-repl-wire-decode-workspace-transcript))))

(defun agent-repl-wire-decode-list-workspace-transcripts-unknown-workspace (json)
  "Decode ListWorkspaceTranscriptsUnknownWorkspace from JSON.  Empty."
  (agent-repl-wire-verbs--decode-empty "ListWorkspaceTranscriptsUnknownWorkspace" json))

(defun agent-repl-wire-decode-list-workspace-transcripts-workspace-ref-mismatch (json)
  "Decode ListWorkspaceTranscriptsWorkspaceRefMismatch from JSON (`:registry-dir')."
  (let ((message "ListWorkspaceTranscriptsWorkspaceRefMismatch"))
    (agent-repl-wire-verbs--check-keys message json '(registryDir))
    (list :registry-dir (agent-repl-wire-verbs--decode-string message 'registryDir json))))

(defun agent-repl-wire-decode-list-workspace-transcripts-transferring-away (json)
  "Decode ListWorkspaceTranscriptsTransferringAway from JSON (`:address')."
  (let ((message "ListWorkspaceTranscriptsTransferringAway"))
    (agent-repl-wire-verbs--check-keys message json '(address))
    (list :address (agent-repl-wire-verbs--decode-string message 'address json))))

(defun agent-repl-wire-decode-list-workspace-transcripts-no-session (json)
  "Decode ListWorkspaceTranscriptsNoSession from JSON.  Empty: no live shim,
and the shim is what reads the transcripts."
  (agent-repl-wire-verbs--decode-empty "ListWorkspaceTranscriptsNoSession" json))

(defun agent-repl-wire-decode-list-workspace-transcripts-unreadable (json)
  "Decode ListWorkspaceTranscriptsUnreadable from JSON.
Into a plist (`:searched-path\' `:detail\')."
  (let ((message "ListWorkspaceTranscriptsUnreadable"))
    (agent-repl-wire-verbs--check-keys message json '(searchedPath detail))
    (list :searched-path (agent-repl-wire-verbs--decode-string message 'searchedPath json)
          :detail (agent-repl-wire-verbs--decode-string message 'detail json))))

(defun agent-repl-wire-decode-list-workspace-transcripts-error-unknown-workspace (json)
  "Decode ListWorkspaceTranscriptsError's `unknown_workspace' cause arm from JSON."
  (agent-repl-wire-decode-list-workspace-transcripts-unknown-workspace json))

(defun agent-repl-wire-decode-list-workspace-transcripts-error-workspace-ref-mismatch (json)
  "Decode ListWorkspaceTranscriptsError's `workspace_ref_mismatch' arm from JSON."
  (agent-repl-wire-decode-list-workspace-transcripts-workspace-ref-mismatch json))

(defun agent-repl-wire-decode-list-workspace-transcripts-error-transferring-away (json)
  "Decode ListWorkspaceTranscriptsError's `transferring_away' arm from JSON."
  (agent-repl-wire-decode-list-workspace-transcripts-transferring-away json))

(defun agent-repl-wire-decode-list-workspace-transcripts-error-no-session (json)
  "Decode ListWorkspaceTranscriptsError's `no_session' arm from JSON."
  (agent-repl-wire-decode-list-workspace-transcripts-no-session json))

(defun agent-repl-wire-decode-list-workspace-transcripts-error-unreadable (json)
  "Decode ListWorkspaceTranscriptsError's `unreadable' arm from JSON."
  (agent-repl-wire-decode-list-workspace-transcripts-unreadable json))

(defun agent-repl-wire-decode-list-workspace-transcripts-error (json)
  "Decode ListWorkspaceTranscriptsError from JSON.
Into (:cause (:arm ARM :value V))."
  (let ((message "ListWorkspaceTranscriptsError"))
    (agent-repl-wire-verbs--check-keys
     message json '(unknownWorkspace workspaceRefMismatch transferringAway noSession unreadable))
    (list :cause
          (agent-repl-wire-verbs--decode-oneof
           message "cause" json
           (list (list 'unknownWorkspace :unknown-workspace #'agent-repl-wire-decode-list-workspace-transcripts-error-unknown-workspace)
                 (list 'workspaceRefMismatch :workspace-ref-mismatch #'agent-repl-wire-decode-list-workspace-transcripts-error-workspace-ref-mismatch)
                 (list 'transferringAway :transferring-away #'agent-repl-wire-decode-list-workspace-transcripts-error-transferring-away)
                 (list 'noSession :no-session #'agent-repl-wire-decode-list-workspace-transcripts-error-no-session)
                 (list 'unreadable :unreadable #'agent-repl-wire-decode-list-workspace-transcripts-error-unreadable))))))

(defun agent-repl-wire-decode-list-workspace-transcripts-response-success (json)
  "Decode ListWorkspaceTranscriptsResponse's `success' arm from JSON."
  (agent-repl-wire-decode-list-workspace-transcripts-success json))

(defun agent-repl-wire-decode-list-workspace-transcripts-response-error (json)
  "Decode ListWorkspaceTranscriptsResponse's `error' arm from JSON."
  (agent-repl-wire-decode-list-workspace-transcripts-error json))

(defun agent-repl-wire-decode-list-workspace-transcripts-response (json)
  "Decode ListWorkspaceTranscriptsResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-result
   "ListWorkspaceTranscriptsResponse" json
   #'agent-repl-wire-decode-list-workspace-transcripts-response-success
   #'agent-repl-wire-decode-list-workspace-transcripts-response-error))


;;;; ---- BindWorkspaceSession -------------------------------------------

(defun agent-repl-wire-encode-bind-workspace-session-request-workspace (ref)
  "Encode BindWorkspaceSessionRequest's `workspace' use site from REF."
  (agent-repl-wire-encode-workspace-ref ref))

(defun agent-repl-wire-encode-bind-workspace-session-request (request)
  "Encode BindWorkspaceSessionRequest from plist REQUEST.
REQUEST is (:workspace REF :vendor-session-id ID :op-id OP).  The vendor
session id is an ECHO of a value ListWorkspaceTranscripts served: a client
may not invent one, so a blank id is refused here rather than sent.  The
OP ID IS OPTIONAL: present, the daemon also pushes the bind's stages on
WatchDaemon keyed on it; absent, the bind emits no stages at all."
  (agent-repl--log '(:agent-repl-context "a codec call outside a request has no workspace") "elisp.wire.verbs-encode-bind-workspace-session-request")
  (let ((out (list (cons 'workspace
                         (agent-repl-wire-encode-bind-workspace-session-request-workspace
                          (agent-repl-wire-verbs--require "BindWorkspaceSessionRequest" "workspace"
                                                          (plist-get request :workspace))))
                   (cons 'vendorSessionId
                         (agent-repl-wire-verbs--require-string
                          "BindWorkspaceSessionRequest" "vendor_session_id"
                          (plist-get request :vendor-session-id))))))
    (agent-repl-wire-verbs--append-op-id out request)))

(defun agent-repl-wire-decode-bind-workspace-session-success (json)
  "Decode BindWorkspaceSessionSuccess from JSON.  Empty: every visible effect
— the new feed, the topbar, the roster row — arrives on the streams."
  (agent-repl-wire-verbs--decode-empty "BindWorkspaceSessionSuccess" json))

(defun agent-repl-wire-decode-bind-workspace-session-unknown-workspace (json)
  "Decode BindWorkspaceSessionUnknownWorkspace from JSON.  Empty."
  (agent-repl-wire-verbs--decode-empty "BindWorkspaceSessionUnknownWorkspace" json))

(defun agent-repl-wire-decode-bind-workspace-session-workspace-ref-mismatch (json)
  "Decode BindWorkspaceSessionWorkspaceRefMismatch from JSON (`:registry-dir')."
  (let ((message "BindWorkspaceSessionWorkspaceRefMismatch"))
    (agent-repl-wire-verbs--check-keys message json '(registryDir))
    (list :registry-dir (agent-repl-wire-verbs--decode-string message 'registryDir json))))

(defun agent-repl-wire-decode-bind-workspace-session-transferring-away (json)
  "Decode BindWorkspaceSessionTransferringAway from JSON (`:address')."
  (let ((message "BindWorkspaceSessionTransferringAway"))
    (agent-repl-wire-verbs--check-keys message json '(address))
    (list :address (agent-repl-wire-verbs--decode-string message 'address json))))

(defun agent-repl-wire-decode-bind-workspace-session-unknown-transcript (json)
  "Decode BindWorkspaceSessionUnknownTranscript from JSON (`:vendor-session-id')."
  (let ((message "BindWorkspaceSessionUnknownTranscript"))
    (agent-repl-wire-verbs--check-keys message json '(vendorSessionId))
    (list :vendor-session-id (agent-repl-wire-verbs--decode-string message 'vendorSessionId json))))

(defun agent-repl-wire-decode-bind-workspace-session-already-bound (json)
  "Decode BindWorkspaceSessionAlreadyBound from JSON.  Empty: the workspace
already runs that conversation and nothing was changed."
  (agent-repl-wire-verbs--decode-empty "BindWorkspaceSessionAlreadyBound" json))

(defun agent-repl-wire-decode-bind-workspace-session-transcript-active (json)
  "Decode BindWorkspaceSessionTranscriptActive from JSON (`:at-ms')."
  (let ((message "BindWorkspaceSessionTranscriptActive"))
    (agent-repl-wire-verbs--check-keys message json '(atMs))
    (list :at-ms (agent-repl-wire--decode-int64 message 'atMs json))))

(defun agent-repl-wire-decode-bind-workspace-session-transcript-held (json)
  "Decode BindWorkspaceSessionTranscriptHeld from JSON (`:workspace')."
  (let ((message "BindWorkspaceSessionTranscriptHeld"))
    (agent-repl-wire-verbs--check-keys message json '(workspace))
    (list :workspace (agent-repl-wire-decode-workspace-ref
                      (cdr (assq 'workspace json))))))

(defun agent-repl-wire-decode-bind-workspace-session-turn-in-flight (json)
  "Decode BindWorkspaceSessionTurnInFlight from JSON.  Empty: a bind is not an
interrupt."
  (agent-repl-wire-verbs--decode-empty "BindWorkspaceSessionTurnInFlight" json))

(defun agent-repl-wire-decode-bind-workspace-session-stop-failed (json)
  "Decode BindWorkspaceSessionStopFailed from JSON (`:detail')."
  (let ((message "BindWorkspaceSessionStopFailed"))
    (agent-repl-wire-verbs--check-keys message json '(detail))
    (list :detail (agent-repl-wire-verbs--decode-string message 'detail json))))

(defun agent-repl-wire-decode-bind-workspace-session-start-failed (json)
  "Decode BindWorkspaceSessionStartFailed from JSON (`:detail').
THE BINDING STANDS: the record names the conversation the user chose, and
the ordinary open path is what brings it up next."
  (let ((message "BindWorkspaceSessionStartFailed"))
    (agent-repl-wire-verbs--check-keys message json '(detail))
    (list :detail (agent-repl-wire-verbs--decode-string message 'detail json))))

(defun agent-repl-wire-decode-bind-workspace-session-error-unknown-workspace (json)
  "Decode BindWorkspaceSessionError's `unknown_workspace' cause arm from JSON."
  (agent-repl-wire-decode-bind-workspace-session-unknown-workspace json))

(defun agent-repl-wire-decode-bind-workspace-session-error-workspace-ref-mismatch (json)
  "Decode BindWorkspaceSessionError's `workspace_ref_mismatch' arm from JSON."
  (agent-repl-wire-decode-bind-workspace-session-workspace-ref-mismatch json))

(defun agent-repl-wire-decode-bind-workspace-session-error-transferring-away (json)
  "Decode BindWorkspaceSessionError's `transferring_away' arm from JSON."
  (agent-repl-wire-decode-bind-workspace-session-transferring-away json))

(defun agent-repl-wire-decode-bind-workspace-session-error-unknown-transcript (json)
  "Decode BindWorkspaceSessionError's `unknown_transcript' arm from JSON."
  (agent-repl-wire-decode-bind-workspace-session-unknown-transcript json))

(defun agent-repl-wire-decode-bind-workspace-session-error-already-bound (json)
  "Decode BindWorkspaceSessionError's `already_bound' arm from JSON."
  (agent-repl-wire-decode-bind-workspace-session-already-bound json))

(defun agent-repl-wire-decode-bind-workspace-session-error-transcript-active (json)
  "Decode BindWorkspaceSessionError's `transcript_active' arm from JSON."
  (agent-repl-wire-decode-bind-workspace-session-transcript-active json))

(defun agent-repl-wire-decode-bind-workspace-session-error-transcript-held (json)
  "Decode BindWorkspaceSessionError's `transcript_held' arm from JSON."
  (agent-repl-wire-decode-bind-workspace-session-transcript-held json))

(defun agent-repl-wire-decode-bind-workspace-session-error-turn-in-flight (json)
  "Decode BindWorkspaceSessionError's `turn_in_flight' arm from JSON."
  (agent-repl-wire-decode-bind-workspace-session-turn-in-flight json))

(defun agent-repl-wire-decode-bind-workspace-session-error-stop-failed (json)
  "Decode BindWorkspaceSessionError's `stop_failed' arm from JSON."
  (agent-repl-wire-decode-bind-workspace-session-stop-failed json))

(defun agent-repl-wire-decode-bind-workspace-session-error-start-failed (json)
  "Decode BindWorkspaceSessionError's `start_failed' arm from JSON."
  (agent-repl-wire-decode-bind-workspace-session-start-failed json))

(defun agent-repl-wire-decode-bind-workspace-session-error (json)
  "Decode BindWorkspaceSessionError from JSON into (:cause (:arm ARM :value V))."
  (let ((message "BindWorkspaceSessionError"))
    (agent-repl-wire-verbs--check-keys
     message json
     '(unknownWorkspace workspaceRefMismatch transferringAway unknownTranscript
       alreadyBound transcriptActive transcriptHeld turnInFlight stopFailed startFailed))
    (list :cause
          (agent-repl-wire-verbs--decode-oneof
           message "cause" json
           (list (list 'unknownWorkspace :unknown-workspace #'agent-repl-wire-decode-bind-workspace-session-error-unknown-workspace)
                 (list 'workspaceRefMismatch :workspace-ref-mismatch #'agent-repl-wire-decode-bind-workspace-session-error-workspace-ref-mismatch)
                 (list 'transferringAway :transferring-away #'agent-repl-wire-decode-bind-workspace-session-error-transferring-away)
                 (list 'unknownTranscript :unknown-transcript #'agent-repl-wire-decode-bind-workspace-session-error-unknown-transcript)
                 (list 'alreadyBound :already-bound #'agent-repl-wire-decode-bind-workspace-session-error-already-bound)
                 (list 'transcriptActive :transcript-active #'agent-repl-wire-decode-bind-workspace-session-error-transcript-active)
                 (list 'transcriptHeld :transcript-held #'agent-repl-wire-decode-bind-workspace-session-error-transcript-held)
                 (list 'turnInFlight :turn-in-flight #'agent-repl-wire-decode-bind-workspace-session-error-turn-in-flight)
                 (list 'stopFailed :stop-failed #'agent-repl-wire-decode-bind-workspace-session-error-stop-failed)
                 (list 'startFailed :start-failed #'agent-repl-wire-decode-bind-workspace-session-error-start-failed))))))

(defun agent-repl-wire-decode-bind-workspace-session-response-success (json)
  "Decode BindWorkspaceSessionResponse's `success' arm from JSON."
  (agent-repl-wire-decode-bind-workspace-session-success json))

(defun agent-repl-wire-decode-bind-workspace-session-response-error (json)
  "Decode BindWorkspaceSessionResponse's `error' arm from JSON."
  (agent-repl-wire-decode-bind-workspace-session-error json))

(defun agent-repl-wire-decode-bind-workspace-session-response (json)
  "Decode BindWorkspaceSessionResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-result
   "BindWorkspaceSessionResponse" json
   #'agent-repl-wire-decode-bind-workspace-session-response-success
   #'agent-repl-wire-decode-bind-workspace-session-response-error))


;;;; ---- CloseWorkspace -------------------------------------------------

(defun agent-repl-wire-encode-close-workspace-request-workspace (ref)
  "Encode CloseWorkspaceRequest's `workspace' use site from REF."
  (agent-repl-wire-encode-workspace-ref ref))

(defun agent-repl-wire-encode-close-workspace-request (request)
  "Encode CloseWorkspaceRequest from plist REQUEST (:workspace REF)."
  (agent-repl--log '(:agent-repl-context "a codec call outside a request has no workspace") "elisp.wire.verbs-encode-close-workspace-request")
  (list (cons 'workspace
              (agent-repl-wire-encode-close-workspace-request-workspace
               (agent-repl-wire-verbs--require "CloseWorkspaceRequest" "workspace"
                                                (plist-get request :workspace))))))

(defun agent-repl-wire-decode-close-workspace-success (json)
  "Decode CloseWorkspaceSuccess from JSON.  Empty: quiet, so the close
happened."
  (agent-repl-wire-verbs--decode-empty "CloseWorkspaceSuccess" json))

(defun agent-repl-wire-decode-close-workspace-blocked (json)
  "Decode CloseWorkspaceBlocked from JSON into a plist.
The plist is (`:turn-in-flight' `:live-work' `:held-prompts'
`:merge-queued' `:summary') -- the daemon's own close-blocked evidence,
with `summary' its composed sentence over the other four.  A caller with
no footer in front of it can say WHY the close was refused; the footer
itself stays the place the reasons are read."
  (let ((message "CloseWorkspaceBlocked"))
    (agent-repl-wire-verbs--check-keys
     message json '(turnInFlight liveWork heldPrompts mergeQueued summary))
    (list :turn-in-flight (agent-repl-wire--decode-bool message 'turnInFlight json)
          :live-work (agent-repl-wire--decode-uint32 message 'liveWork json)
          :held-prompts (agent-repl-wire--decode-uint32 message 'heldPrompts json)
          :merge-queued (agent-repl-wire--decode-bool message 'mergeQueued json)
          :summary (agent-repl-wire-verbs--decode-string message 'summary json))))

(defun agent-repl-wire-decode-close-workspace-unknown-workspace (json)
  "Decode CloseWorkspaceUnknownWorkspace from JSON.  Empty: The workspace id is
not in the daemon's registry."
  (agent-repl-wire-verbs--decode-empty "CloseWorkspaceUnknownWorkspace" json))

(defun agent-repl-wire-decode-close-workspace-workspace-ref-mismatch (json)
  "Decode CloseWorkspaceWorkspaceRefMismatch from JSON into a plist
(`:registry-dir').
The echoed dir disagrees with the registry's dir for this id."
  (let ((message "CloseWorkspaceWorkspaceRefMismatch"))
    (agent-repl-wire-verbs--check-keys message json '(registryDir))
    (list :registry-dir (agent-repl-wire-verbs--decode-string
                       message 'registryDir json))))

(defun agent-repl-wire-decode-close-workspace-transferring-away (json)
  "Decode CloseWorkspaceTransferringAway from JSON into a plist (`:address').
This daemon released the workspace to a successor; dial `address'."
  (let ((message "CloseWorkspaceTransferringAway"))
    (agent-repl-wire-verbs--check-keys message json '(address))
    (list :address (agent-repl-wire-verbs--decode-string
                       message 'address json))))

(defun agent-repl-wire-decode-close-workspace-not-yet-adopted (json)
  "Decode CloseWorkspaceNotYetAdopted from JSON.  Empty: A joining daemon has
not finished adopting this workspace yet."
  (agent-repl-wire-verbs--decode-empty "CloseWorkspaceNotYetAdopted" json))

(defun agent-repl-wire-decode-close-workspace-error-blocked (json)
  "Decode CloseWorkspaceError's `blocked' cause arm from JSON."
  (agent-repl-wire-decode-close-workspace-blocked json))

(defun agent-repl-wire-decode-close-workspace-error-unknown-workspace (json)
  "Decode CloseWorkspaceError's `unknown_workspace' cause arm from JSON."
  (agent-repl-wire-decode-close-workspace-unknown-workspace json))

(defun agent-repl-wire-decode-close-workspace-error-workspace-ref-mismatch (json)
  "Decode CloseWorkspaceError's `workspace_ref_mismatch' cause arm from JSON."
  (agent-repl-wire-decode-close-workspace-workspace-ref-mismatch json))

(defun agent-repl-wire-decode-close-workspace-error-transferring-away (json)
  "Decode CloseWorkspaceError's `transferring_away' cause arm from JSON."
  (agent-repl-wire-decode-close-workspace-transferring-away json))

(defun agent-repl-wire-decode-close-workspace-error-not-yet-adopted (json)
  "Decode CloseWorkspaceError's `not_yet_adopted' cause arm from JSON."
  (agent-repl-wire-decode-close-workspace-not-yet-adopted json))

(defun agent-repl-wire-decode-close-workspace-error (json)
  "Decode CloseWorkspaceError from JSON into (:cause (:arm ARM :value V)).
THE ARM IS THE REFUSAL, so an unset cause is a contract breach and an
arm this codec does not know is refused as an unknown field."
  (let ((message "CloseWorkspaceError"))
    (agent-repl-wire-verbs--check-keys message json '(blocked unknownWorkspace workspaceRefMismatch transferringAway notYetAdopted))
    (list :cause
          (agent-repl-wire-verbs--decode-oneof
           message "cause" json
           (list (list 'blocked :blocked #'agent-repl-wire-decode-close-workspace-error-blocked)
         (list 'unknownWorkspace :unknown-workspace #'agent-repl-wire-decode-close-workspace-error-unknown-workspace)
         (list 'workspaceRefMismatch :workspace-ref-mismatch #'agent-repl-wire-decode-close-workspace-error-workspace-ref-mismatch)
         (list 'transferringAway :transferring-away #'agent-repl-wire-decode-close-workspace-error-transferring-away)
         (list 'notYetAdopted :not-yet-adopted #'agent-repl-wire-decode-close-workspace-error-not-yet-adopted))))))

(defun agent-repl-wire-decode-close-workspace-response-success (json)
  "Decode CloseWorkspaceResponse's `success' arm from JSON."
  (agent-repl-wire-decode-close-workspace-success json))

(defun agent-repl-wire-decode-close-workspace-response-error (json)
  "Decode CloseWorkspaceResponse's `error' arm from JSON."
  (agent-repl-wire-decode-close-workspace-error json))

(defun agent-repl-wire-decode-close-workspace-response (json)
  "Decode CloseWorkspaceResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-result
   "CloseWorkspaceResponse" json
   #'agent-repl-wire-decode-close-workspace-response-success
   #'agent-repl-wire-decode-close-workspace-response-error))


;;;; ---- KillWorkspace --------------------------------------------------

(defun agent-repl-wire-encode-kill-workspace-request-workspace (ref)
  "Encode KillWorkspaceRequest's `workspace' use site from REF."
  (agent-repl-wire-encode-workspace-ref ref))

(defun agent-repl-wire-encode-kill-workspace-request (request)
  "Encode KillWorkspaceRequest from plist REQUEST (:workspace REF :op-id ID).
THE OP ID OPTS INTO THE IMMEDIATE ACK: present, the daemon answers
`accepted' once the workspace is closed and pushes the teardown\='s end on
WatchDaemon keyed on it; absent, the rpc answers once the teardown is done."
  (agent-repl--log '(:agent-repl-context "a codec call outside a request has no workspace") "elisp.wire.verbs-encode-kill-workspace-request")
  (agent-repl-wire-verbs--append-op-id
   (list (cons 'workspace
               (agent-repl-wire-encode-kill-workspace-request-workspace
                (agent-repl-wire-verbs--require "KillWorkspaceRequest" "workspace"
                                                 (plist-get request :workspace)))))
   request))

(defun agent-repl-wire-decode-kill-workspace-success (json)
  "Decode KillWorkspaceSuccess from JSON.  Empty: the effects ride the streams."
  (agent-repl-wire-verbs--decode-empty "KillWorkspaceSuccess" json))

(defun agent-repl-wire-decode-kill-workspace-unknown-workspace (json)
  "Decode KillWorkspaceUnknownWorkspace from JSON.  Empty: The workspace id is
not in the daemon's registry."
  (agent-repl-wire-verbs--decode-empty "KillWorkspaceUnknownWorkspace" json))

(defun agent-repl-wire-decode-kill-workspace-workspace-ref-mismatch (json)
  "Decode KillWorkspaceWorkspaceRefMismatch from JSON into a plist (`:registry-
dir').
The echoed dir disagrees with the registry's dir for this id."
  (let ((message "KillWorkspaceWorkspaceRefMismatch"))
    (agent-repl-wire-verbs--check-keys message json '(registryDir))
    (list :registry-dir (agent-repl-wire-verbs--decode-string
                       message 'registryDir json))))

(defun agent-repl-wire-decode-kill-workspace-transferring-away (json)
  "Decode KillWorkspaceTransferringAway from JSON into a plist (`:address').
This daemon released the workspace to a successor; dial `address'."
  (let ((message "KillWorkspaceTransferringAway"))
    (agent-repl-wire-verbs--check-keys message json '(address))
    (list :address (agent-repl-wire-verbs--decode-string
                       message 'address json))))

(defun agent-repl-wire-decode-kill-workspace-not-yet-adopted (json)
  "Decode KillWorkspaceNotYetAdopted from JSON.  Empty: A joining daemon has
not finished adopting this workspace yet."
  (agent-repl-wire-verbs--decode-empty "KillWorkspaceNotYetAdopted" json))

(defun agent-repl-wire-decode-kill-workspace-error-unknown-workspace (json)
  "Decode KillWorkspaceError's `unknown_workspace' cause arm from JSON."
  (agent-repl-wire-decode-kill-workspace-unknown-workspace json))

(defun agent-repl-wire-decode-kill-workspace-error-workspace-ref-mismatch (json)
  "Decode KillWorkspaceError's `workspace_ref_mismatch' cause arm from JSON."
  (agent-repl-wire-decode-kill-workspace-workspace-ref-mismatch json))

(defun agent-repl-wire-decode-kill-workspace-error-transferring-away (json)
  "Decode KillWorkspaceError's `transferring_away' cause arm from JSON."
  (agent-repl-wire-decode-kill-workspace-transferring-away json))

(defun agent-repl-wire-decode-kill-workspace-error-not-yet-adopted (json)
  "Decode KillWorkspaceError's `not_yet_adopted' cause arm from JSON."
  (agent-repl-wire-decode-kill-workspace-not-yet-adopted json))

(defun agent-repl-wire-decode-kill-workspace-error (json)
  "Decode KillWorkspaceError from JSON into (:cause (:arm ARM :value V)).
THE ARM IS THE REFUSAL, so an unset cause is a contract breach and an
arm this codec does not know is refused as an unknown field."
  (let ((message "KillWorkspaceError"))
    (agent-repl-wire-verbs--check-keys message json '(unknownWorkspace workspaceRefMismatch transferringAway notYetAdopted))
    (list :cause
          (agent-repl-wire-verbs--decode-oneof
           message "cause" json
           (list (list 'unknownWorkspace :unknown-workspace #'agent-repl-wire-decode-kill-workspace-error-unknown-workspace)
         (list 'workspaceRefMismatch :workspace-ref-mismatch #'agent-repl-wire-decode-kill-workspace-error-workspace-ref-mismatch)
         (list 'transferringAway :transferring-away #'agent-repl-wire-decode-kill-workspace-error-transferring-away)
         (list 'notYetAdopted :not-yet-adopted #'agent-repl-wire-decode-kill-workspace-error-not-yet-adopted))))))

(defun agent-repl-wire-decode-kill-workspace-response-success (json)
  "Decode KillWorkspaceResponse's `success' arm from JSON."
  (agent-repl-wire-decode-kill-workspace-success json))

(defun agent-repl-wire-decode-kill-workspace-response-error (json)
  "Decode KillWorkspaceResponse's `error' arm from JSON."
  (agent-repl-wire-decode-kill-workspace-error json))

(defun agent-repl-wire-decode-kill-workspace-accepted (json)
  "Decode KillWorkspaceAccepted from JSON into (:op-id ID).
The immediate ack: the workspace is closed and its teardown detached; the
teardown\='s end arrives on the WatchDaemon progress channel."
  (list :op-id
        (agent-repl-wire-verbs--decode-string "KillWorkspaceAccepted" 'opId json)))

(defun agent-repl-wire-decode-kill-workspace-response-accepted (json)
  "Decode KillWorkspaceResponse's `accepted' arm from JSON."
  (agent-repl-wire-decode-kill-workspace-accepted json))

(defun agent-repl-wire-decode-kill-workspace-response (json)
  "Decode KillWorkspaceResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-accepting-result
   "KillWorkspaceResponse" json
   #'agent-repl-wire-decode-kill-workspace-response-success
   #'agent-repl-wire-decode-kill-workspace-response-error
   #'agent-repl-wire-decode-kill-workspace-response-accepted))


;;;; ---- NukeWorkspace --------------------------------------------------

(defun agent-repl-wire-encode-nuke-workspace-request-workspace (ref)
  "Encode NukeWorkspaceRequest's `workspace' use site from REF."
  (agent-repl-wire-encode-workspace-ref ref))

(defun agent-repl-wire-encode-nuke-workspace-request (request)
  "Encode NukeWorkspaceRequest from plist REQUEST (:workspace REF :op-id ID).
THE OP ID OPTS INTO THE IMMEDIATE ACK: present, the daemon answers
`accepted' once the workspace is closed and pushes the teardown\='s end on
WatchDaemon keyed on it; absent, the rpc answers once the teardown is done."
  (agent-repl--log '(:agent-repl-context "a codec call outside a request has no workspace") "elisp.wire.verbs-encode-nuke-workspace-request")
  (agent-repl-wire-verbs--append-op-id
   (list (cons 'workspace
               (agent-repl-wire-encode-nuke-workspace-request-workspace
                (agent-repl-wire-verbs--require "NukeWorkspaceRequest" "workspace"
                                                 (plist-get request :workspace)))))
   request))

(defun agent-repl-wire-decode-nuke-workspace-success (json)
  "Decode NukeWorkspaceSuccess from JSON.  Empty: the effects ride the streams."
  (agent-repl-wire-verbs--decode-empty "NukeWorkspaceSuccess" json))

(defun agent-repl-wire-decode-nuke-workspace-unknown-workspace (json)
  "Decode NukeWorkspaceUnknownWorkspace from JSON.  Empty: The workspace id is
not in the daemon's registry."
  (agent-repl-wire-verbs--decode-empty "NukeWorkspaceUnknownWorkspace" json))

(defun agent-repl-wire-decode-nuke-workspace-workspace-ref-mismatch (json)
  "Decode NukeWorkspaceWorkspaceRefMismatch from JSON into a plist (`:registry-
dir').
The echoed dir disagrees with the registry's dir for this id."
  (let ((message "NukeWorkspaceWorkspaceRefMismatch"))
    (agent-repl-wire-verbs--check-keys message json '(registryDir))
    (list :registry-dir (agent-repl-wire-verbs--decode-string
                       message 'registryDir json))))

(defun agent-repl-wire-decode-nuke-workspace-transferring-away (json)
  "Decode NukeWorkspaceTransferringAway from JSON into a plist (`:address').
This daemon released the workspace to a successor; dial `address'."
  (let ((message "NukeWorkspaceTransferringAway"))
    (agent-repl-wire-verbs--check-keys message json '(address))
    (list :address (agent-repl-wire-verbs--decode-string
                       message 'address json))))

(defun agent-repl-wire-decode-nuke-workspace-not-yet-adopted (json)
  "Decode NukeWorkspaceNotYetAdopted from JSON.  Empty: A joining daemon has
not finished adopting this workspace yet."
  (agent-repl-wire-verbs--decode-empty "NukeWorkspaceNotYetAdopted" json))

(defun agent-repl-wire-decode-nuke-workspace-git-failed (json)
  "Decode NukeWorkspaceGitFailed from JSON into a plist (`:detail').
A git operation failed mid-delete."
  (let ((message "NukeWorkspaceGitFailed"))
    (agent-repl-wire-verbs--check-keys message json '(detail))
    (list :detail (agent-repl-wire-verbs--decode-string
                       message 'detail json))))

(defun agent-repl-wire-decode-nuke-workspace-error-unknown-workspace (json)
  "Decode NukeWorkspaceError's `unknown_workspace' cause arm from JSON."
  (agent-repl-wire-decode-nuke-workspace-unknown-workspace json))

(defun agent-repl-wire-decode-nuke-workspace-error-workspace-ref-mismatch (json)
  "Decode NukeWorkspaceError's `workspace_ref_mismatch' cause arm from JSON."
  (agent-repl-wire-decode-nuke-workspace-workspace-ref-mismatch json))

(defun agent-repl-wire-decode-nuke-workspace-error-transferring-away (json)
  "Decode NukeWorkspaceError's `transferring_away' cause arm from JSON."
  (agent-repl-wire-decode-nuke-workspace-transferring-away json))

(defun agent-repl-wire-decode-nuke-workspace-error-not-yet-adopted (json)
  "Decode NukeWorkspaceError's `not_yet_adopted' cause arm from JSON."
  (agent-repl-wire-decode-nuke-workspace-not-yet-adopted json))

(defun agent-repl-wire-decode-nuke-workspace-error-git-failed (json)
  "Decode NukeWorkspaceError's `git_failed' cause arm from JSON."
  (agent-repl-wire-decode-nuke-workspace-git-failed json))

(defun agent-repl-wire-decode-nuke-workspace-error (json)
  "Decode NukeWorkspaceError from JSON into (:cause (:arm ARM :value V)).
THE ARM IS THE REFUSAL, so an unset cause is a contract breach and an
arm this codec does not know is refused as an unknown field."
  (let ((message "NukeWorkspaceError"))
    (agent-repl-wire-verbs--check-keys message json '(unknownWorkspace workspaceRefMismatch transferringAway notYetAdopted gitFailed))
    (list :cause
          (agent-repl-wire-verbs--decode-oneof
           message "cause" json
           (list (list 'unknownWorkspace :unknown-workspace #'agent-repl-wire-decode-nuke-workspace-error-unknown-workspace)
         (list 'workspaceRefMismatch :workspace-ref-mismatch #'agent-repl-wire-decode-nuke-workspace-error-workspace-ref-mismatch)
         (list 'transferringAway :transferring-away #'agent-repl-wire-decode-nuke-workspace-error-transferring-away)
         (list 'notYetAdopted :not-yet-adopted #'agent-repl-wire-decode-nuke-workspace-error-not-yet-adopted)
         (list 'gitFailed :git-failed #'agent-repl-wire-decode-nuke-workspace-error-git-failed))))))

(defun agent-repl-wire-decode-nuke-workspace-response-success (json)
  "Decode NukeWorkspaceResponse's `success' arm from JSON."
  (agent-repl-wire-decode-nuke-workspace-success json))

(defun agent-repl-wire-decode-nuke-workspace-response-error (json)
  "Decode NukeWorkspaceResponse's `error' arm from JSON."
  (agent-repl-wire-decode-nuke-workspace-error json))

(defun agent-repl-wire-decode-nuke-workspace-accepted (json)
  "Decode NukeWorkspaceAccepted from JSON into (:op-id ID).
The immediate ack: the workspace is closed and its teardown detached; the
teardown\='s end arrives on the WatchDaemon progress channel."
  (list :op-id
        (agent-repl-wire-verbs--decode-string "NukeWorkspaceAccepted" 'opId json)))

(defun agent-repl-wire-decode-nuke-workspace-response-accepted (json)
  "Decode NukeWorkspaceResponse's `accepted' arm from JSON."
  (agent-repl-wire-decode-nuke-workspace-accepted json))

(defun agent-repl-wire-decode-nuke-workspace-response (json)
  "Decode NukeWorkspaceResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-accepting-result
   "NukeWorkspaceResponse" json
   #'agent-repl-wire-decode-nuke-workspace-response-success
   #'agent-repl-wire-decode-nuke-workspace-response-error
   #'agent-repl-wire-decode-nuke-workspace-response-accepted))


;;;; ---- MergeWorkspace -------------------------------------------------

(defun agent-repl-wire-encode-merge-workspace-request-workspace (ref)
  "Encode MergeWorkspaceRequest's `workspace' use site from REF."
  (agent-repl-wire-encode-workspace-ref ref))

(defun agent-repl-wire-encode-merge-workspace-source-own-branch (value)
  "Encode MergeWorkspaceSourceOwnBranch from plist VALUE (:keep-open BOOL).
`keep_open' is spelled EXPLICITLY on the wire even when false: whether the
requesting workspace closes once its branch lands is the request\='s own
statement, never an omitted default."
  (list (cons 'keepOpen
              (agent-repl-wire-verbs--encode-bool (plist-get value :keep-open)))))

(defun agent-repl-wire-encode-merge-workspace-source-workspace (value)
  "Encode MergeWorkspaceSourceWorkspace from plist VALUE (:ref REF).
The other workspace's ref is required: a workspace source naming no
workspace names nothing to merge."
  (list (cons 'ref
              (agent-repl-wire-encode-workspace-ref
               (agent-repl-wire-verbs--require "MergeWorkspaceSourceWorkspace" "ref"
                                                (plist-get value :ref))))))

(defun agent-repl-wire-encode-merge-workspace-source-branch (value)
  "Encode MergeWorkspaceSourceBranch from plist VALUE (:name STRING).
The branch name is required and non-blank, spelled as git spells it."
  (list (cons 'name
              (agent-repl-wire-verbs--require-string
               "MergeWorkspaceSourceBranch" "name" (plist-get value :name)))))

(defun agent-repl-wire-encode-merge-workspace-source-merged-upstream (value)
  "Encode MergeWorkspaceSourceMergedUpstream from VALUE.  Empty: the
branch and the repository are the requester\='s own."
  (agent-repl-wire--encode-empty "MergeWorkspaceSourceMergedUpstream" value))

(defun agent-repl-wire-encode-merge-workspace-source (value)
  "Encode MergeWorkspaceSource from the oneof plist VALUE (:arm ARM :value V).
THE ARM IS THE SOURCE: `:own-branch' (:keep-open BOOL), `:workspace'
\(:ref REF), `:branch' (:name STRING) or `:merged-upstream' (nil).  An
unset oneof and an unknown arm are both refused."
  (list (agent-repl-wire-verbs--encode-oneof
         "MergeWorkspaceSource" "source" value
         '((:own-branch ownBranch
                        agent-repl-wire-encode-merge-workspace-source-own-branch)
           (:workspace workspace
                       agent-repl-wire-encode-merge-workspace-source-workspace)
           (:branch branch
                    agent-repl-wire-encode-merge-workspace-source-branch)
           (:merged-upstream mergedUpstream
                             agent-repl-wire-encode-merge-workspace-source-merged-upstream)))))

(defun agent-repl-wire-encode-merge-workspace-request-source (value)
  "Encode MergeWorkspaceRequest's `source' use site from VALUE."
  (agent-repl-wire-encode-merge-workspace-source value))

(defun agent-repl-wire-encode-merge-workspace-request (request)
  "Encode MergeWorkspaceRequest from plist REQUEST (:workspace REF :source S).
`workspace' is the REQUESTING workspace, the one the merge runs in;
`source' is WHAT it merges (`agent-repl-wire-encode-merge-workspace-source').
Both are required."
  (agent-repl--log '(:agent-repl-context "a codec call outside a request has no workspace") "elisp.wire.verbs-encode-merge-workspace-request source=%S"
                   (plist-get (plist-get request :source) :arm))
  (list (cons 'workspace
              (agent-repl-wire-encode-merge-workspace-request-workspace
               (agent-repl-wire-verbs--require "MergeWorkspaceRequest" "workspace"
                                                (plist-get request :workspace))))
        (cons 'source
              (agent-repl-wire-encode-merge-workspace-request-source
               (agent-repl-wire-verbs--require "MergeWorkspaceRequest" "source"
                                                (plist-get request :source))))))

(defun agent-repl-wire-decode-merge-workspace-success (json)
  "Decode MergeWorkspaceSuccess from JSON.  Empty: success means ENQUEUED."
  (agent-repl-wire-verbs--decode-empty "MergeWorkspaceSuccess" json))

(defun agent-repl-wire-decode-merge-workspace-unknown-workspace (json)
  "Decode MergeWorkspaceUnknownWorkspace from JSON.  Empty: The workspace id is
not in the daemon's registry."
  (agent-repl-wire-verbs--decode-empty "MergeWorkspaceUnknownWorkspace" json))

(defun agent-repl-wire-decode-merge-workspace-workspace-ref-mismatch (json)
  "Decode MergeWorkspaceWorkspaceRefMismatch from JSON into a plist
(`:registry-dir').
The echoed dir disagrees with the registry's dir for this id."
  (let ((message "MergeWorkspaceWorkspaceRefMismatch"))
    (agent-repl-wire-verbs--check-keys message json '(registryDir))
    (list :registry-dir (agent-repl-wire-verbs--decode-string
                       message 'registryDir json))))

(defun agent-repl-wire-decode-merge-workspace-transferring-away (json)
  "Decode MergeWorkspaceTransferringAway from JSON into a plist (`:address').
This daemon released the workspace to a successor; dial `address'."
  (let ((message "MergeWorkspaceTransferringAway"))
    (agent-repl-wire-verbs--check-keys message json '(address))
    (list :address (agent-repl-wire-verbs--decode-string
                       message 'address json))))

(defun agent-repl-wire-decode-merge-workspace-not-yet-adopted (json)
  "Decode MergeWorkspaceNotYetAdopted from JSON.  Empty: A joining daemon has
not finished adopting this workspace yet."
  (agent-repl-wire-verbs--decode-empty "MergeWorkspaceNotYetAdopted" json))

(defun agent-repl-wire-decode-merge-workspace-no-layout-facts (json)
  "Decode MergeWorkspaceNoLayoutFacts from JSON.  Empty: The daemon holds no
layout facts for this workspace to merge with."
  (agent-repl-wire-verbs--decode-empty "MergeWorkspaceNoLayoutFacts" json))

(defun agent-repl-wire-decode-merge-workspace-session-deleted (json)
  "Decode MergeWorkspaceSessionDeleted from JSON.  Empty: The workspace's
session has been deleted."
  (agent-repl-wire-verbs--decode-empty "MergeWorkspaceSessionDeleted" json))

(defun agent-repl-wire-decode-merge-workspace-already-queued (json)
  "Decode MergeWorkspaceAlreadyQueued from JSON.  Empty: This workspace's merge
is already in the queue."
  (agent-repl-wire-verbs--decode-empty "MergeWorkspaceAlreadyQueued" json))

(defun agent-repl-wire-decode-merge-workspace-already-merging (json)
  "Decode MergeWorkspaceAlreadyMerging from JSON.  Empty: This workspace's
merge is already in flight."
  (agent-repl-wire-verbs--decode-empty "MergeWorkspaceAlreadyMerging" json))

(defun agent-repl-wire-decode-merge-workspace-unknown-source-workspace (json)
  "Decode MergeWorkspaceUnknownSourceWorkspace from JSON.  Empty: the source
workspace is not open, is in another repository, or is the requester itself."
  (agent-repl-wire-verbs--decode-empty "MergeWorkspaceUnknownSourceWorkspace" json))

(defun agent-repl-wire-decode-merge-workspace-unknown-branch (json)
  "Decode MergeWorkspaceUnknownBranch from JSON.  Empty: the named branch does
not exist in the requester's repository."
  (agent-repl-wire-verbs--decode-empty "MergeWorkspaceUnknownBranch" json))

(defun agent-repl-wire-decode-merge-workspace-error-unknown-workspace (json)
  "Decode MergeWorkspaceError's `unknown_workspace' cause arm from JSON."
  (agent-repl-wire-decode-merge-workspace-unknown-workspace json))

(defun agent-repl-wire-decode-merge-workspace-error-workspace-ref-mismatch (json)
  "Decode MergeWorkspaceError's `workspace_ref_mismatch' cause arm from JSON."
  (agent-repl-wire-decode-merge-workspace-workspace-ref-mismatch json))

(defun agent-repl-wire-decode-merge-workspace-error-transferring-away (json)
  "Decode MergeWorkspaceError's `transferring_away' cause arm from JSON."
  (agent-repl-wire-decode-merge-workspace-transferring-away json))

(defun agent-repl-wire-decode-merge-workspace-error-not-yet-adopted (json)
  "Decode MergeWorkspaceError's `not_yet_adopted' cause arm from JSON."
  (agent-repl-wire-decode-merge-workspace-not-yet-adopted json))

(defun agent-repl-wire-decode-merge-workspace-error-no-layout-facts (json)
  "Decode MergeWorkspaceError's `no_layout_facts' cause arm from JSON."
  (agent-repl-wire-decode-merge-workspace-no-layout-facts json))

(defun agent-repl-wire-decode-merge-workspace-error-session-deleted (json)
  "Decode MergeWorkspaceError's `session_deleted' cause arm from JSON."
  (agent-repl-wire-decode-merge-workspace-session-deleted json))

(defun agent-repl-wire-decode-merge-workspace-error-already-queued (json)
  "Decode MergeWorkspaceError's `already_queued' cause arm from JSON."
  (agent-repl-wire-decode-merge-workspace-already-queued json))

(defun agent-repl-wire-decode-merge-workspace-error-already-merging (json)
  "Decode MergeWorkspaceError's `already_merging' cause arm from JSON."
  (agent-repl-wire-decode-merge-workspace-already-merging json))

(defun agent-repl-wire-decode-merge-workspace-error-unknown-source-workspace (json)
  "Decode MergeWorkspaceError's `unknown_source_workspace' cause arm from JSON."
  (agent-repl-wire-decode-merge-workspace-unknown-source-workspace json))

(defun agent-repl-wire-decode-merge-workspace-error-unknown-branch (json)
  "Decode MergeWorkspaceError's `unknown_branch' cause arm from JSON."
  (agent-repl-wire-decode-merge-workspace-unknown-branch json))

(defun agent-repl-wire-decode-merge-workspace-error (json)
  "Decode MergeWorkspaceError from JSON into (:cause (:arm ARM :value V)).
THE ARM IS THE REFUSAL, so an unset cause is a contract breach and an
arm this codec does not know is refused as an unknown field."
  (let ((message "MergeWorkspaceError"))
    (agent-repl-wire-verbs--check-keys message json '(unknownWorkspace workspaceRefMismatch transferringAway notYetAdopted noLayoutFacts sessionDeleted alreadyQueued alreadyMerging unknownSourceWorkspace unknownBranch))
    (list :cause
          (agent-repl-wire-verbs--decode-oneof
           message "cause" json
           (list (list 'unknownWorkspace :unknown-workspace #'agent-repl-wire-decode-merge-workspace-error-unknown-workspace)
         (list 'workspaceRefMismatch :workspace-ref-mismatch #'agent-repl-wire-decode-merge-workspace-error-workspace-ref-mismatch)
         (list 'transferringAway :transferring-away #'agent-repl-wire-decode-merge-workspace-error-transferring-away)
         (list 'notYetAdopted :not-yet-adopted #'agent-repl-wire-decode-merge-workspace-error-not-yet-adopted)
         (list 'noLayoutFacts :no-layout-facts #'agent-repl-wire-decode-merge-workspace-error-no-layout-facts)
         (list 'sessionDeleted :session-deleted #'agent-repl-wire-decode-merge-workspace-error-session-deleted)
         (list 'alreadyQueued :already-queued #'agent-repl-wire-decode-merge-workspace-error-already-queued)
         (list 'alreadyMerging :already-merging #'agent-repl-wire-decode-merge-workspace-error-already-merging)
         (list 'unknownSourceWorkspace :unknown-source-workspace #'agent-repl-wire-decode-merge-workspace-error-unknown-source-workspace)
         (list 'unknownBranch :unknown-branch #'agent-repl-wire-decode-merge-workspace-error-unknown-branch))))))

(defun agent-repl-wire-decode-merge-workspace-response-success (json)
  "Decode MergeWorkspaceResponse's `success' arm from JSON."
  (agent-repl-wire-decode-merge-workspace-success json))

(defun agent-repl-wire-decode-merge-workspace-response-error (json)
  "Decode MergeWorkspaceResponse's `error' arm from JSON."
  (agent-repl-wire-decode-merge-workspace-error json))

(defun agent-repl-wire-decode-merge-workspace-response (json)
  "Decode MergeWorkspaceResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-result
   "MergeWorkspaceResponse" json
   #'agent-repl-wire-decode-merge-workspace-response-success
   #'agent-repl-wire-decode-merge-workspace-response-error))


;;;; ---- RestartWorkspace -----------------------------------------------

(defun agent-repl-wire-encode-restart-workspace-request-workspace (ref)
  "Encode RestartWorkspaceRequest's `workspace' use site from REF."
  (agent-repl-wire-encode-workspace-ref ref))

(defun agent-repl-wire-encode-restart-workspace-request (request)
  "Encode RestartWorkspaceRequest from plist REQUEST (:workspace REF :force
BOOL).
`force' is spelled EXPLICITLY on the wire even when false: a forced
restart interrupts live work, so the request states the mode rather than
leaning on an omitted default."
  (agent-repl--log '(:agent-repl-context "a codec call outside a request has no workspace") "elisp.wire.verbs-encode-restart-workspace-request force=%S"
                    (and (plist-get request :force) t))
  (list (cons 'workspace
              (agent-repl-wire-encode-restart-workspace-request-workspace
               (agent-repl-wire-verbs--require "RestartWorkspaceRequest" "workspace"
                                                (plist-get request :workspace))))
        (cons 'force (agent-repl-wire-verbs--encode-bool (plist-get request :force)))))

(defun agent-repl-wire-decode-restart-workspace-success (json)
  "Decode RestartWorkspaceSuccess from JSON.  Empty: the restart is accepted."
  (agent-repl-wire-verbs--decode-empty "RestartWorkspaceSuccess" json))

(defun agent-repl-wire-decode-restart-workspace-unknown-workspace (json)
  "Decode RestartWorkspaceUnknownWorkspace from JSON.  Empty: The workspace id
is not in the daemon's registry."
  (agent-repl-wire-verbs--decode-empty "RestartWorkspaceUnknownWorkspace" json))

(defun agent-repl-wire-decode-restart-workspace-workspace-ref-mismatch (json)
  "Decode RestartWorkspaceWorkspaceRefMismatch from JSON into a plist
(`:registry-dir').
The echoed dir disagrees with the registry's dir for this id."
  (let ((message "RestartWorkspaceWorkspaceRefMismatch"))
    (agent-repl-wire-verbs--check-keys message json '(registryDir))
    (list :registry-dir (agent-repl-wire-verbs--decode-string
                       message 'registryDir json))))

(defun agent-repl-wire-decode-restart-workspace-transferring-away (json)
  "Decode RestartWorkspaceTransferringAway from JSON into a plist (`:address').
This daemon released the workspace to a successor; dial `address'."
  (let ((message "RestartWorkspaceTransferringAway"))
    (agent-repl-wire-verbs--check-keys message json '(address))
    (list :address (agent-repl-wire-verbs--decode-string
                       message 'address json))))

(defun agent-repl-wire-decode-restart-workspace-not-yet-adopted (json)
  "Decode RestartWorkspaceNotYetAdopted from JSON.  Empty: A joining daemon has
not finished adopting this workspace yet."
  (agent-repl-wire-verbs--decode-empty "RestartWorkspaceNotYetAdopted" json))

(defun agent-repl-wire-decode-restart-workspace-no-session (json)
  "Decode RestartWorkspaceNoSession from JSON.  Empty: The workspace has no
session to restart."
  (agent-repl-wire-verbs--decode-empty "RestartWorkspaceNoSession" json))

(defun agent-repl-wire-decode-restart-workspace-error-unknown-workspace (json)
  "Decode RestartWorkspaceError's `unknown_workspace' cause arm from JSON."
  (agent-repl-wire-decode-restart-workspace-unknown-workspace json))

(defun agent-repl-wire-decode-restart-workspace-error-workspace-ref-mismatch (json)
  "Decode RestartWorkspaceError's `workspace_ref_mismatch' cause arm from JSON."
  (agent-repl-wire-decode-restart-workspace-workspace-ref-mismatch json))

(defun agent-repl-wire-decode-restart-workspace-error-transferring-away (json)
  "Decode RestartWorkspaceError's `transferring_away' cause arm from JSON."
  (agent-repl-wire-decode-restart-workspace-transferring-away json))

(defun agent-repl-wire-decode-restart-workspace-error-not-yet-adopted (json)
  "Decode RestartWorkspaceError's `not_yet_adopted' cause arm from JSON."
  (agent-repl-wire-decode-restart-workspace-not-yet-adopted json))

(defun agent-repl-wire-decode-restart-workspace-error-no-session (json)
  "Decode RestartWorkspaceError's `no_session' cause arm from JSON."
  (agent-repl-wire-decode-restart-workspace-no-session json))

(defun agent-repl-wire-decode-restart-workspace-error (json)
  "Decode RestartWorkspaceError from JSON into (:cause (:arm ARM :value V)).
THE ARM IS THE REFUSAL, so an unset cause is a contract breach and an
arm this codec does not know is refused as an unknown field."
  (let ((message "RestartWorkspaceError"))
    (agent-repl-wire-verbs--check-keys message json '(unknownWorkspace workspaceRefMismatch transferringAway notYetAdopted noSession))
    (list :cause
          (agent-repl-wire-verbs--decode-oneof
           message "cause" json
           (list (list 'unknownWorkspace :unknown-workspace #'agent-repl-wire-decode-restart-workspace-error-unknown-workspace)
         (list 'workspaceRefMismatch :workspace-ref-mismatch #'agent-repl-wire-decode-restart-workspace-error-workspace-ref-mismatch)
         (list 'transferringAway :transferring-away #'agent-repl-wire-decode-restart-workspace-error-transferring-away)
         (list 'notYetAdopted :not-yet-adopted #'agent-repl-wire-decode-restart-workspace-error-not-yet-adopted)
         (list 'noSession :no-session #'agent-repl-wire-decode-restart-workspace-error-no-session))))))

(defun agent-repl-wire-decode-restart-workspace-response-success (json)
  "Decode RestartWorkspaceResponse's `success' arm from JSON."
  (agent-repl-wire-decode-restart-workspace-success json))

(defun agent-repl-wire-decode-restart-workspace-response-error (json)
  "Decode RestartWorkspaceResponse's `error' arm from JSON."
  (agent-repl-wire-decode-restart-workspace-error json))

(defun agent-repl-wire-decode-restart-workspace-response (json)
  "Decode RestartWorkspaceResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-result
   "RestartWorkspaceResponse" json
   #'agent-repl-wire-decode-restart-workspace-response-success
   #'agent-repl-wire-decode-restart-workspace-response-error))


;;;; ---- SetWorkspacePriority -------------------------------------------

(defun agent-repl-wire-encode-set-workspace-priority-request-workspace (ref)
  "Encode SetWorkspacePriorityRequest's `workspace' use site from REF."
  (agent-repl-wire-encode-workspace-ref ref))

(defun agent-repl-wire-encode-set-workspace-priority-request-priority (priority)
  "Encode SetWorkspacePriorityRequest's `priority' use site from PRIORITY."
  (agent-repl-wire-encode-workspace-priority priority))

(defun agent-repl-wire-encode-set-workspace-priority-request (request)
  "Encode SetWorkspacePriorityRequest from plist REQUEST.
REQUEST is (:workspace REF :priority PRIORITY-OR-NIL).  An ABSENT priority
IS the clear — presence, never a sentinel level — so a nil priority omits
the field entirely."
  (let ((out (list (cons 'workspace
                         (agent-repl-wire-encode-set-workspace-priority-request-workspace
                          (agent-repl-wire-verbs--require
                           "SetWorkspacePriorityRequest" "workspace"
                           (plist-get request :workspace)))))))
    (agent-repl--log '(:agent-repl-context "a codec call outside a request has no workspace") "elisp.wire.verbs-encode-set-workspace-priority-request cleared=%S"
                      (null (plist-get request :priority)))
    (if (plist-get request :priority)
        (append out (list (cons 'priority
                                (agent-repl-wire-encode-set-workspace-priority-request-priority
                                 (plist-get request :priority)))))
      out)))

(defun agent-repl-wire-decode-set-workspace-priority-success (json)
  "Decode SetWorkspacePrioritySuccess from JSON.
Empty: the roster push carries the new state."
  (agent-repl-wire-verbs--decode-empty "SetWorkspacePrioritySuccess" json))

(defun agent-repl-wire-decode-set-workspace-priority-unknown-workspace (json)
  "Decode SetWorkspacePriorityUnknownWorkspace from JSON.  Empty: The workspace
id is not in the daemon's registry."
  (agent-repl-wire-verbs--decode-empty "SetWorkspacePriorityUnknownWorkspace" json))

(defun agent-repl-wire-decode-set-workspace-priority-workspace-ref-mismatch (json)
  "Decode SetWorkspacePriorityWorkspaceRefMismatch from JSON into a plist
(`:registry-dir').
The echoed dir disagrees with the registry's dir for this id."
  (let ((message "SetWorkspacePriorityWorkspaceRefMismatch"))
    (agent-repl-wire-verbs--check-keys message json '(registryDir))
    (list :registry-dir (agent-repl-wire-verbs--decode-string
                       message 'registryDir json))))

(defun agent-repl-wire-decode-set-workspace-priority-transferring-away (json)
  "Decode SetWorkspacePriorityTransferringAway from JSON into a plist
(`:address').
This daemon released the workspace to a successor; dial `address'."
  (let ((message "SetWorkspacePriorityTransferringAway"))
    (agent-repl-wire-verbs--check-keys message json '(address))
    (list :address (agent-repl-wire-verbs--decode-string
                       message 'address json))))

(defun agent-repl-wire-decode-set-workspace-priority-not-yet-adopted (json)
  "Decode SetWorkspacePriorityNotYetAdopted from JSON.  Empty: A joining daemon
has not finished adopting this workspace yet."
  (agent-repl-wire-verbs--decode-empty "SetWorkspacePriorityNotYetAdopted" json))

(defun agent-repl-wire-decode-set-workspace-priority-error-unknown-workspace (json)
  "Decode SetWorkspacePriorityError's `unknown_workspace' cause arm from JSON."
  (agent-repl-wire-decode-set-workspace-priority-unknown-workspace json))

(defun agent-repl-wire-decode-set-workspace-priority-error-workspace-ref-mismatch (json)
  "Decode SetWorkspacePriorityError's `workspace_ref_mismatch' cause arm from
JSON."
  (agent-repl-wire-decode-set-workspace-priority-workspace-ref-mismatch json))

(defun agent-repl-wire-decode-set-workspace-priority-error-transferring-away (json)
  "Decode SetWorkspacePriorityError's `transferring_away' cause arm from JSON."
  (agent-repl-wire-decode-set-workspace-priority-transferring-away json))

(defun agent-repl-wire-decode-set-workspace-priority-error-not-yet-adopted (json)
  "Decode SetWorkspacePriorityError's `not_yet_adopted' cause arm from JSON."
  (agent-repl-wire-decode-set-workspace-priority-not-yet-adopted json))

(defun agent-repl-wire-decode-set-workspace-priority-error (json)
  "Decode SetWorkspacePriorityError from JSON into (:cause (:arm ARM :value V)).
THE ARM IS THE REFUSAL, so an unset cause is a contract breach and an
arm this codec does not know is refused as an unknown field."
  (let ((message "SetWorkspacePriorityError"))
    (agent-repl-wire-verbs--check-keys message json '(unknownWorkspace workspaceRefMismatch transferringAway notYetAdopted))
    (list :cause
          (agent-repl-wire-verbs--decode-oneof
           message "cause" json
           (list (list 'unknownWorkspace :unknown-workspace #'agent-repl-wire-decode-set-workspace-priority-error-unknown-workspace)
         (list 'workspaceRefMismatch :workspace-ref-mismatch #'agent-repl-wire-decode-set-workspace-priority-error-workspace-ref-mismatch)
         (list 'transferringAway :transferring-away #'agent-repl-wire-decode-set-workspace-priority-error-transferring-away)
         (list 'notYetAdopted :not-yet-adopted #'agent-repl-wire-decode-set-workspace-priority-error-not-yet-adopted))))))

(defun agent-repl-wire-decode-set-workspace-priority-response-success (json)
  "Decode SetWorkspacePriorityResponse's `success' arm from JSON."
  (agent-repl-wire-decode-set-workspace-priority-success json))

(defun agent-repl-wire-decode-set-workspace-priority-response-error (json)
  "Decode SetWorkspacePriorityResponse's `error' arm from JSON."
  (agent-repl-wire-decode-set-workspace-priority-error json))

(defun agent-repl-wire-decode-set-workspace-priority-response (json)
  "Decode SetWorkspacePriorityResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-result
   "SetWorkspacePriorityResponse" json
   #'agent-repl-wire-decode-set-workspace-priority-response-success
   #'agent-repl-wire-decode-set-workspace-priority-response-error))


;;;; ---- RegisterRepository ---------------------------------------------
;;
;; A REPOSITORY WITH NO WORKSPACE.  The request carries ANY path inside the
;; repository -- a file as readily as a directory, because "pick a file" is
;; the gesture the command is built on -- and the daemon resolves and mints
;; the identity from it.  `already_known' is an ANSWER rather than a
;; refusal: re-registering succeeds, and the caller says which of the two it
;; was.

(defun agent-repl-wire-encode-register-repository-request (request)
  "Encode RegisterRepositoryRequest from plist REQUEST (:path PATH).
A PATH, not an identity: the daemon resolves the repository from it and
mints the identity, which the response returns."
  (agent-repl--log '(:agent-repl-context "a codec call outside a request has no workspace") "elisp.wire.verbs-encode-register-repository-request")
  (list (cons 'path
              (agent-repl-wire-verbs--require-string
               "RegisterRepositoryRequest" "path" (plist-get request :path)))))

(defun agent-repl-wire-decode-register-repository-success-repository (json)
  "Decode RegisterRepositorySuccess's `repository' use site from JSON."
  (agent-repl-wire-decode-repository-ref json))

(defun agent-repl-wire-decode-register-repository-success-workspace (json)
  "Decode RegisterRepositorySuccess\='s `workspace\=' use site from JSON."
  (agent-repl-wire-decode-workspace-ref json))

(defun agent-repl-wire-decode-register-repository-success (json)
  "Decode RegisterRepositorySuccess from JSON.
The plist is (:repository REF :already-known BOOL :workspace REF
:workspace-already-known BOOL).  BOTH refs are REQUIRED on a success: the
endpoint registers the repository\='s main worktree as a workspace through
the same registration `RegisterWorkspace\=' runs (owner ruling,
2026-09-14), and never answers without one.  The two already-known bools
are INDEPENDENT -- a repository minted by an earlier RegisterWorkspace is
already known while its workspace is too, and a repository registered
before that ruling landed is already known while its workspace is fresh."
  (let ((message "RegisterRepositorySuccess"))
    (agent-repl-wire-verbs--check-keys
     message json '(repository alreadyKnown workspace workspaceAlreadyKnown))
    (list :repository (agent-repl-wire-decode-register-repository-success-repository
                       (agent-repl-wire-verbs--require
                        message "repository" (cdr (assq 'repository json))))
          :already-known (agent-repl-wire--decode-bool message 'alreadyKnown json)
          :workspace (agent-repl-wire-decode-register-repository-success-workspace
                      (agent-repl-wire-verbs--require
                       message "workspace" (cdr (assq 'workspace json))))
          :workspace-already-known
          (agent-repl-wire--decode-bool message 'workspaceAlreadyKnown json))))

(defun agent-repl-wire-decode-register-repository-not-in-a-repository (json)
  "Decode RegisterRepositoryNotInARepository from JSON.  Empty: the path is
readable but lies inside no git work tree."
  (agent-repl-wire-verbs--decode-empty "RegisterRepositoryNotInARepository" json))

(defun agent-repl-wire-decode-register-repository-unreadable-path (json)
  "Decode RegisterRepositoryUnreadablePath from JSON.  Empty: the path cannot
be read at all."
  (agent-repl-wire-verbs--decode-empty "RegisterRepositoryUnreadablePath" json))

(defun agent-repl-wire-decode-register-repository-error-not-in-a-repository (json)
  "Decode RegisterRepositoryError's `not_in_a_repository' cause arm from JSON."
  (agent-repl-wire-decode-register-repository-not-in-a-repository json))

(defun agent-repl-wire-decode-register-repository-error-unreadable-path (json)
  "Decode RegisterRepositoryError's `unreadable_path' cause arm from JSON."
  (agent-repl-wire-decode-register-repository-unreadable-path json))

(defun agent-repl-wire-decode-register-repository-error (json)
  "Decode RegisterRepositoryError from JSON into (:cause (:arm ARM :value V)).
THE ARM IS THE REFUSAL, so an unset cause is a contract breach and an
arm this codec does not know is refused as an unknown field."
  (let ((message "RegisterRepositoryError"))
    (agent-repl-wire-verbs--check-keys message json '(notInARepository unreadablePath))
    (list :cause
          (agent-repl-wire-verbs--decode-oneof
           message "cause" json
           (list (list 'notInARepository :not-in-a-repository
                       #'agent-repl-wire-decode-register-repository-error-not-in-a-repository)
                 (list 'unreadablePath :unreadable-path
                       #'agent-repl-wire-decode-register-repository-error-unreadable-path))))))

(defun agent-repl-wire-decode-register-repository-response-success (json)
  "Decode RegisterRepositoryResponse's `success' arm from JSON."
  (agent-repl-wire-decode-register-repository-success json))

(defun agent-repl-wire-decode-register-repository-response-error (json)
  "Decode RegisterRepositoryResponse's `error' arm from JSON."
  (agent-repl-wire-decode-register-repository-error json))

(defun agent-repl-wire-decode-register-repository-response (json)
  "Decode RegisterRepositoryResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-result
   "RegisterRepositoryResponse" json
   #'agent-repl-wire-decode-register-repository-response-success
   #'agent-repl-wire-decode-register-repository-response-error))


;;;; ---- SubmitPrompt: encode -------------------------------------------

(defun agent-repl-wire-encode-submit-prompt-request-said (said)
  "Encode SubmitPromptRequest's `said' use site from SAID."
  (agent-repl-wire-encode-user-said said))

(defun agent-repl-wire-encode-submit-prompt-request-origin (origin)
  "Encode SubmitPromptRequest's `origin' use site from ORIGIN."
  (agent-repl-wire-encode-prompt-origin origin))

(defun agent-repl-wire-encode-submit-prompt-request-workspace (ref)
  "Encode SubmitPromptRequest's `workspace' use site from REF."
  (agent-repl-wire-encode-workspace-ref ref))

(defconst agent-repl-wire-submit-prompt-deliveries
  '((:deferred . "SUBMIT_PROMPT_DELIVERY_DEFERRED"))
  "The SubmitPromptDelivery vocabulary Emacs sends, keyword to wire name.
UNSPECIFIED is deliberately ABSENT: the ordinary delivery is the field's
ABSENCE, and a request carrying the zero value is refused as
InvalidArgument, so it has no elisp spelling to reach for by accident.")

(defun agent-repl-wire-encode-submit-prompt-delivery (value)
  "Encode the SubmitPromptDelivery keyword VALUE as its protojson enum name.
The one spelling of a delivery Emacs writes: the request encoder and the
held-prompt ingress's entry (`held-ingress.el') both read it, so the two
can never name a deferral differently.  The vocabulary is closed; an
unknown keyword is refused before anything is built."
  (let ((name (cdr (assq value agent-repl-wire-submit-prompt-deliveries))))
    (unless name
      (agent-repl-wire-verbs--fail "SubmitPromptRequest" "delivery" "unknown delivery"))
    name))

(defun agent-repl-wire-encode-submit-prompt-request (request)
  "Encode SubmitPromptRequest from plist REQUEST.
REQUEST is (:workspace REF :said SAID :idempotency-key STRING :origin
KEYWORD) plus the OPTIONAL :delivery KEYWORD
\(`agent-repl-wire-submit-prompt-deliveries'; absent is the ordinary
delivery, so it is appended only when set).  The first
four are required: the workspace names WHICH workspace the submission
belongs to and is echoed verbatim like every other per-workspace request
(it is required even though `feed' is not, because the root feed has no id
of its own); an empty idempotency key defeats the duplicate refusal that
makes a retry safe; and the origin is REQUIRED and never UNSPECIFIED
because a stored turn must trace back to the exact send site.  `feed' is
never set — Emacs composes into the workspace's root feed only, so the
absent field IS that fact.

No reply target rides the request: the daemon holds the feed's selection
(SelectFeedRow) and applies its own when the prompt is sent."
  (let ((message "SubmitPromptRequest")
        (delivery (plist-get request :delivery)))
    (agent-repl--log '(:agent-repl-context "a codec call outside a request has no workspace") "elisp.wire.verbs-encode-submit-prompt-request origin=%s delivery=%S"
                      (plist-get request :origin) delivery)
    (append
     (list (cons 'workspace
                 (agent-repl-wire-encode-submit-prompt-request-workspace
                  (agent-repl-wire-verbs--require message "workspace"
                                                   (plist-get request :workspace))))
           (cons 'said (agent-repl-wire-encode-submit-prompt-request-said
                        (agent-repl-wire-verbs--require message "said"
                                                         (plist-get request :said))))
           (cons 'idempotencyKey
                 (agent-repl-wire-verbs--require-string message "idempotency_key"
                                                         (plist-get request :idempotency-key)))
           (cons 'origin (agent-repl-wire-encode-submit-prompt-request-origin
                          (agent-repl-wire-verbs--require message "origin"
                                                           (plist-get request :origin)))))
     ;; OPTIONAL field: its absence is the ordinary delivery.
     (when delivery
       (list (cons 'delivery (agent-repl-wire-encode-submit-prompt-delivery delivery)))))))


;;;; ---- SubmitPrompt: decode -------------------------------------------

(defun agent-repl-wire-decode-submit-prompt-turn-turn (json)
  "Decode SubmitPromptTurn's `turn' use site from JSON."
  (agent-repl-wire-decode-turn-id json))

(defun agent-repl-wire-decode-submit-prompt-turn (json)
  "Decode SubmitPromptTurn from JSON into (:turn TURN-ID).
The minted turn is what the client matches against FeedRow.turn, so it is
non-optional: a turn arm without it is a contract breach."
  (let ((message "SubmitPromptTurn"))
    (agent-repl-wire-verbs--check-keys message json '(turn))
    (list :turn (agent-repl-wire-decode-submit-prompt-turn-turn
                 (agent-repl-wire-verbs--require message "turn" (cdr (assq 'turn json)))))))

(defun agent-repl-wire-verbs--decode-panel-payload (json)
  "Return the panel arm's payload JSON verbatim.
The panels are frontend.v1 views the WEBAPP draws; Emacs learns only WHICH
command was recognized, so the payload is kept as the raw decoded alist
rather than modeled here."
  json)

(defun agent-repl-wire-decode-submit-prompt-command-panel (json)
  "Decode SubmitPromptCommandPanel from JSON into (:arm ARM :value RAW).
ARM is the recognized command; RAW is the panel payload verbatim.  Tags 2
and 3 are RETIRED in the proto, so `cost' and `usage' are not arms and
arrive here as unknown fields."
  (let ((message "SubmitPromptCommandPanel")
        (panels '(status todos agents mcp context help)))
    (agent-repl-wire-verbs--check-keys message json panels)
    (agent-repl-wire-verbs--decode-oneof
     message "panel" json
     (mapcar (lambda (name)
               (list name (intern (concat ":" (symbol-name name)))
                     #'agent-repl-wire-verbs--decode-panel-payload))
             panels))))

(defun agent-repl-wire-decode-submit-prompt-command-refused (json)
  "Decode SubmitPromptCommandRefused from JSON into (:command STRING)."
  (let ((message "SubmitPromptCommandRefused"))
    (agent-repl-wire-verbs--check-keys message json '(command))
    (list :command (agent-repl-wire-verbs--decode-string message 'command json))))

(defun agent-repl-wire-decode-submit-prompt-command-acted (json)
  "Decode SubmitPromptCommandActed from JSON.  Empty: a session-acting command
was recognized and queued as an act that mints no turn; the visible effect
arrives on the component streams."
  (agent-repl-wire-verbs--decode-empty "SubmitPromptCommandActed" json))

(defun agent-repl-wire-decode-submit-prompt-success-turn (json)
  "Decode SubmitPromptSuccess's `turn' outcome arm from JSON."
  (agent-repl-wire-decode-submit-prompt-turn json))

(defun agent-repl-wire-decode-submit-prompt-success-command-panel (json)
  "Decode SubmitPromptSuccess's `command_panel' outcome arm from JSON."
  (agent-repl-wire-decode-submit-prompt-command-panel json))

(defun agent-repl-wire-decode-submit-prompt-success-command-refused (json)
  "Decode SubmitPromptSuccess's `command_refused' outcome arm from JSON."
  (agent-repl-wire-decode-submit-prompt-command-refused json))

(defun agent-repl-wire-decode-submit-prompt-success-command-acted (json)
  "Decode SubmitPromptSuccess's `command_acted' outcome arm from JSON."
  (agent-repl-wire-decode-submit-prompt-command-acted json))

(defun agent-repl-wire-decode-submit-prompt-success (json)
  "Decode SubmitPromptSuccess from JSON into (:arm ARM :value V).
All four arms are ANSWERS: a minted turn, a resolved panel, a
recognized-but-unsupported command, or a session-acting command the daemon
acted on.  Only the turn arm means there is anything to await."
  (let ((message "SubmitPromptSuccess"))
    (agent-repl-wire-verbs--check-keys message json '(turn commandPanel commandRefused commandActed))
    (agent-repl-wire-verbs--decode-oneof
     message "outcome" json
     (list (list 'turn :turn #'agent-repl-wire-decode-submit-prompt-success-turn)
           (list 'commandPanel :command-panel
                 #'agent-repl-wire-decode-submit-prompt-success-command-panel)
           (list 'commandRefused :command-refused
                 #'agent-repl-wire-decode-submit-prompt-success-command-refused)
           (list 'commandActed :command-acted
                 #'agent-repl-wire-decode-submit-prompt-success-command-acted)))))

(defun agent-repl-wire-decode-submit-prompt-refused-merging (json)
  "Decode SubmitPromptRefusedMerging from JSON.  Empty: a merge is in flight
and the prompt arrived after it began."
  (agent-repl-wire-verbs--decode-empty "SubmitPromptRefusedMerging" json))

(defun agent-repl-wire-decode-submit-prompt-unknown-workspace (json)
  "Decode SubmitPromptUnknownWorkspace from JSON.  Empty: The workspace id is
not in the daemon's registry."
  (agent-repl-wire-verbs--decode-empty "SubmitPromptUnknownWorkspace" json))

(defun agent-repl-wire-decode-submit-prompt-workspace-ref-mismatch (json)
  "Decode SubmitPromptWorkspaceRefMismatch from JSON into a plist (`:registry-
dir').
The echoed dir disagrees with the registry's dir for this id."
  (let ((message "SubmitPromptWorkspaceRefMismatch"))
    (agent-repl-wire-verbs--check-keys message json '(registryDir))
    (list :registry-dir (agent-repl-wire-verbs--decode-string
                       message 'registryDir json))))

(defun agent-repl-wire-decode-submit-prompt-transferring-away (json)
  "Decode SubmitPromptTransferringAway from JSON into a plist (`:address').
This daemon released the workspace to a successor; dial `address'."
  (let ((message "SubmitPromptTransferringAway"))
    (agent-repl-wire-verbs--check-keys message json '(address))
    (list :address (agent-repl-wire-verbs--decode-string
                       message 'address json))))

(defun agent-repl-wire-decode-submit-prompt-not-yet-adopted (json)
  "Decode SubmitPromptNotYetAdopted from JSON.  Empty: A joining daemon has not
finished adopting this workspace yet."
  (agent-repl-wire-verbs--decode-empty "SubmitPromptNotYetAdopted" json))

(defun agent-repl-wire-decode-submit-prompt-feed-not-in-workspace (json)
  "Decode SubmitPromptFeedNotInWorkspace from JSON.  Empty: The FeedId decodes
to another workspace."
  (agent-repl-wire-verbs--decode-empty "SubmitPromptFeedNotInWorkspace" json))

(defun agent-repl-wire-decode-submit-prompt-feed-undecodable (json)
  "Decode SubmitPromptFeedUndecodable from JSON.  Empty: The FeedId does not
decode."
  (agent-repl-wire-verbs--decode-empty "SubmitPromptFeedUndecodable" json))

(defun agent-repl-wire-decode-submit-prompt-no-session (json)
  "Decode SubmitPromptNoSession from JSON.  Empty: The workspace has no session
to submit to."
  (agent-repl-wire-verbs--decode-empty "SubmitPromptNoSession" json))

(defun agent-repl-wire-decode-submit-prompt-duplicate-submission (json)
  "Decode SubmitPromptDuplicateSubmission from JSON.  Empty: the client-minted
`idempotency_key' was already accepted for this workspace, so the earlier
submission stands and nothing is submitted twice."
  (agent-repl-wire-verbs--decode-empty "SubmitPromptDuplicateSubmission" json))

(defun agent-repl-wire-decode-submit-prompt-error-merging (json)
  "Decode SubmitPromptError's `merging' reason arm from JSON."
  (agent-repl-wire-decode-submit-prompt-refused-merging json))

(defun agent-repl-wire-decode-submit-prompt-error-unknown-workspace (json)
  "Decode SubmitPromptError's `unknown_workspace' reason arm from JSON."
  (agent-repl-wire-decode-submit-prompt-unknown-workspace json))

(defun agent-repl-wire-decode-submit-prompt-error-workspace-ref-mismatch (json)
  "Decode SubmitPromptError's `workspace_ref_mismatch' reason arm from JSON."
  (agent-repl-wire-decode-submit-prompt-workspace-ref-mismatch json))

(defun agent-repl-wire-decode-submit-prompt-error-transferring-away (json)
  "Decode SubmitPromptError's `transferring_away' reason arm from JSON."
  (agent-repl-wire-decode-submit-prompt-transferring-away json))

(defun agent-repl-wire-decode-submit-prompt-error-not-yet-adopted (json)
  "Decode SubmitPromptError's `not_yet_adopted' reason arm from JSON."
  (agent-repl-wire-decode-submit-prompt-not-yet-adopted json))

(defun agent-repl-wire-decode-submit-prompt-error-feed-not-in-workspace (json)
  "Decode SubmitPromptError's `feed_not_in_workspace' reason arm from JSON."
  (agent-repl-wire-decode-submit-prompt-feed-not-in-workspace json))

(defun agent-repl-wire-decode-submit-prompt-error-feed-undecodable (json)
  "Decode SubmitPromptError's `feed_undecodable' reason arm from JSON."
  (agent-repl-wire-decode-submit-prompt-feed-undecodable json))

(defun agent-repl-wire-decode-submit-prompt-error-no-session (json)
  "Decode SubmitPromptError's `no_session' reason arm from JSON."
  (agent-repl-wire-decode-submit-prompt-no-session json))

(defun agent-repl-wire-decode-submit-prompt-cold-gate (json)
  "Decode SubmitPromptColdGate from JSON into (`:detail').
The workspace's session is PARKED AT ITS COLD GATE: a session exists and
is serving, and it takes no prompt until the gate is answered in the
panel.  `detail' is the gate's own account of what was refused cold --
the same sentence the gate card and the footer carry -- and is never
switched on."
  (let ((message "SubmitPromptColdGate"))
    (agent-repl-wire-verbs--check-keys message json '(detail))
    (list :detail (agent-repl-wire-verbs--decode-string message 'detail json))))

(defun agent-repl-wire-decode-submit-prompt-error-cold-gate (json)
  "Decode SubmitPromptError's `cold_gate' reason arm from JSON."
  (agent-repl-wire-decode-submit-prompt-cold-gate json))

(defun agent-repl-wire-decode-submit-prompt-model-not-in-catalog (json)
  "Decode SubmitPromptModelNotInCatalog from JSON into (`:detail').
A `/model' act named a model the session's catalog does not hold; the act
was not applied.  `detail' is the shim's own sentence naming the model,
and is never switched on."
  (let ((message "SubmitPromptModelNotInCatalog"))
    (agent-repl-wire-verbs--check-keys message json '(detail))
    (list :detail (agent-repl-wire-verbs--decode-string message 'detail json))))

(defun agent-repl-wire-decode-submit-prompt-error-model-not-in-catalog (json)
  "Decode SubmitPromptError's `model_not_in_catalog' reason arm from JSON."
  (agent-repl-wire-decode-submit-prompt-model-not-in-catalog json))

(defun agent-repl-wire-decode-submit-prompt-model-refused (json)
  "Decode SubmitPromptModelRefused from JSON into (`:detail').
The vendor refused the model change a `/model' act asked for.  `detail'
is the vendor's own sentence, never switched on."
  (let ((message "SubmitPromptModelRefused"))
    (agent-repl-wire-verbs--check-keys message json '(detail))
    (list :detail (agent-repl-wire-verbs--decode-string message 'detail json))))

(defun agent-repl-wire-decode-submit-prompt-error-model-refused (json)
  "Decode SubmitPromptError's `model_refused' reason arm from JSON."
  (agent-repl-wire-decode-submit-prompt-model-refused json))

(defun agent-repl-wire-decode-submit-prompt-error-duplicate-submission (json)
  "Decode SubmitPromptError's `duplicate_submission' reason arm from JSON."
  (agent-repl-wire-decode-submit-prompt-duplicate-submission json))

(defun agent-repl-wire-decode-submit-prompt-bubble-not-deliverable (json)
  "Decode SubmitPromptBubbleNotDeliverable from JSON.  Empty: the vendor
offers no route to this agent kind."
  (agent-repl-wire-verbs--decode-empty "SubmitPromptBubbleNotDeliverable" json))

(defun agent-repl-wire-decode-submit-prompt-bubble-agent-busy (json)
  "Decode SubmitPromptBubbleAgentBusy from JSON.  Empty: the addressed
subagent's own turn is already running."
  (agent-repl-wire-verbs--decode-empty "SubmitPromptBubbleAgentBusy" json))

(defun agent-repl-wire-decode-submit-prompt-bubble-refused-not-deliverable (json)
  "Decode SubmitPromptBubbleRefused's `not_deliverable' kind arm from JSON."
  (agent-repl-wire-decode-submit-prompt-bubble-not-deliverable json))

(defun agent-repl-wire-decode-submit-prompt-bubble-refused-agent-busy (json)
  "Decode SubmitPromptBubbleRefused's `agent_busy' kind arm from JSON."
  (agent-repl-wire-decode-submit-prompt-bubble-agent-busy json))

(defun agent-repl-wire-decode-submit-prompt-bubble-refused (json)
  "Decode SubmitPromptBubbleRefused from JSON into (`:detail' `:kind').
The SHIM refused a bubble-addressed prompt and the daemon relays that
refusal BY KIND -- the daemon never judges a subagent's turn itself.  The
kind oneof is REQUIRED: the arm is the refusal, so an unset one is a
contract breach.  `detail' is the shim's own account, for a human and for
logs, and is never switched on."
  (let ((message "SubmitPromptBubbleRefused"))
    (agent-repl-wire-verbs--check-keys message json '(detail notDeliverable agentBusy))
    (list :detail (agent-repl-wire-verbs--decode-string message 'detail json)
          :kind
          (agent-repl-wire-verbs--decode-oneof
           message "kind" json
           (list (list 'notDeliverable :not-deliverable
                       #'agent-repl-wire-decode-submit-prompt-bubble-refused-not-deliverable)
                 (list 'agentBusy :agent-busy
                       #'agent-repl-wire-decode-submit-prompt-bubble-refused-agent-busy))))))

(defun agent-repl-wire-decode-submit-prompt-error-bubble-refused (json)
  "Decode SubmitPromptError's `bubble_refused' reason arm from JSON."
  (agent-repl-wire-decode-submit-prompt-bubble-refused json))

(defun agent-repl-wire-decode-submit-prompt-error (json)
  "Decode SubmitPromptError from JSON into (:reason (:arm ARM :value V)).
THE ARM IS THE REFUSAL, so an unset reason is a contract breach and an
arm this codec does not know is refused as an unknown field."
  (let ((message "SubmitPromptError"))
    (agent-repl-wire-verbs--check-keys message json '(merging unknownWorkspace workspaceRefMismatch transferringAway notYetAdopted feedNotInWorkspace feedUndecodable noSession duplicateSubmission bubbleRefused coldGate modelNotInCatalog modelRefused))
    (list :reason
          (agent-repl-wire-verbs--decode-oneof
           message "reason" json
           (list (list 'merging :merging #'agent-repl-wire-decode-submit-prompt-error-merging)
         (list 'unknownWorkspace :unknown-workspace #'agent-repl-wire-decode-submit-prompt-error-unknown-workspace)
         (list 'workspaceRefMismatch :workspace-ref-mismatch #'agent-repl-wire-decode-submit-prompt-error-workspace-ref-mismatch)
         (list 'transferringAway :transferring-away #'agent-repl-wire-decode-submit-prompt-error-transferring-away)
         (list 'notYetAdopted :not-yet-adopted #'agent-repl-wire-decode-submit-prompt-error-not-yet-adopted)
         (list 'feedNotInWorkspace :feed-not-in-workspace #'agent-repl-wire-decode-submit-prompt-error-feed-not-in-workspace)
         (list 'feedUndecodable :feed-undecodable #'agent-repl-wire-decode-submit-prompt-error-feed-undecodable)
         (list 'noSession :no-session #'agent-repl-wire-decode-submit-prompt-error-no-session)
         (list 'duplicateSubmission :duplicate-submission #'agent-repl-wire-decode-submit-prompt-error-duplicate-submission)
         (list 'bubbleRefused :bubble-refused #'agent-repl-wire-decode-submit-prompt-error-bubble-refused)
         (list 'coldGate :cold-gate #'agent-repl-wire-decode-submit-prompt-error-cold-gate)
         (list 'modelNotInCatalog :model-not-in-catalog #'agent-repl-wire-decode-submit-prompt-error-model-not-in-catalog)
         (list 'modelRefused :model-refused #'agent-repl-wire-decode-submit-prompt-error-model-refused))))))

(defun agent-repl-wire-decode-submit-prompt-response-success (json)
  "Decode SubmitPromptResponse's `success' arm from JSON."
  (agent-repl-wire-decode-submit-prompt-success json))

(defun agent-repl-wire-decode-submit-prompt-response-error (json)
  "Decode SubmitPromptResponse's `error' arm from JSON."
  (agent-repl-wire-decode-submit-prompt-error json))

(defun agent-repl-wire-decode-submit-prompt-response (json)
  "Decode SubmitPromptResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-result
   "SubmitPromptResponse" json
   #'agent-repl-wire-decode-submit-prompt-response-success
   #'agent-repl-wire-decode-submit-prompt-response-error))


;;;; ---- UpdateShutdownSchedule -----------------------------------------

(defun agent-repl-wire-encode-update-shutdown-schedule-schedule-reason (reason)
  "Encode UpdateShutdownScheduleSchedule's `reason' use site from REASON."
  (agent-repl-wire-encode-drain-reason reason))

(defun agent-repl-wire-encode-update-shutdown-schedule-schedule (value)
  "Encode UpdateShutdownScheduleSchedule from plist VALUE (:at-ms N :reason R).
The reason is REQUIRED: every client's drain banner names it, and the
drain_scheduled push carries it verbatim."
  (let ((message "UpdateShutdownScheduleSchedule"))
    (list (cons 'atMs (agent-repl-wire-verbs--encode-int64
                       message "at_ms"
                       (agent-repl-wire-verbs--require message "at_ms"
                                                        (plist-get value :at-ms))))
          (cons 'reason (agent-repl-wire-encode-update-shutdown-schedule-schedule-reason
                         (agent-repl-wire-verbs--require message "reason"
                                                          (plist-get value :reason)))))))

(defun agent-repl-wire-encode-update-shutdown-schedule-cancel (_value)
  "Encode UpdateShutdownScheduleCancel.  Empty: the arm is the whole action."
  nil)

(defun agent-repl-wire-encode-update-shutdown-schedule-now-reason (reason)
  "Encode UpdateShutdownScheduleNow's `reason' use site from REASON."
  (agent-repl-wire-encode-drain-reason reason))

(defun agent-repl-wire-encode-update-shutdown-schedule-now (value)
  "Encode UpdateShutdownScheduleNow from plist VALUE (:reason R).
The reason is REQUIRED: it rides the shutdown announcement's immediate
cause."
  (let ((message "UpdateShutdownScheduleNow"))
    (list (cons 'reason (agent-repl-wire-encode-update-shutdown-schedule-now-reason
                         (agent-repl-wire-verbs--require message "reason"
                                                          (plist-get value :reason)))))))

(defun agent-repl-wire-encode-update-shutdown-schedule-request-schedule (value)
  "Encode UpdateShutdownScheduleRequest's `schedule' action arm from VALUE."
  (agent-repl-wire-encode-update-shutdown-schedule-schedule value))

(defun agent-repl-wire-encode-update-shutdown-schedule-request-cancel (value)
  "Encode UpdateShutdownScheduleRequest's `cancel' action arm from VALUE."
  (agent-repl-wire-encode-update-shutdown-schedule-cancel value))

(defun agent-repl-wire-encode-update-shutdown-schedule-request-now (value)
  "Encode UpdateShutdownScheduleRequest's `now' action arm from VALUE."
  (agent-repl-wire-encode-update-shutdown-schedule-now value))

(defun agent-repl-wire-encode-update-shutdown-schedule-request (request)
  "Encode UpdateShutdownScheduleRequest from plist REQUEST (:action ONEOF)."
  (list (agent-repl-wire-verbs--encode-oneof
         "UpdateShutdownScheduleRequest" "action" (plist-get request :action)
         (list (list :schedule 'schedule
                     #'agent-repl-wire-encode-update-shutdown-schedule-request-schedule)
               (list :cancel 'cancel
                     #'agent-repl-wire-encode-update-shutdown-schedule-request-cancel)
               (list :now 'now
                     #'agent-repl-wire-encode-update-shutdown-schedule-request-now)))))

(defun agent-repl-wire-decode-update-shutdown-schedule-success (json)
  "Decode UpdateShutdownScheduleSuccess from JSON.  Empty: the action is armed."
  (agent-repl-wire-verbs--decode-empty "UpdateShutdownScheduleSuccess" json))

(defun agent-repl-wire-decode-update-shutdown-schedule-nothing-scheduled (json)
  "Decode UpdateShutdownScheduleNothingScheduled from JSON.  Empty: Nothing is
scheduled to cancel."
  (agent-repl-wire-verbs--decode-empty "UpdateShutdownScheduleNothingScheduled" json))

(defun agent-repl-wire-decode-update-shutdown-schedule-error-nothing-scheduled (json)
  "Decode UpdateShutdownScheduleError's `nothing_scheduled' cause arm from
JSON."
  (agent-repl-wire-decode-update-shutdown-schedule-nothing-scheduled json))

(defun agent-repl-wire-decode-update-shutdown-schedule-error (json)
  "Decode UpdateShutdownScheduleError from JSON into (:cause (:arm ARM :value
V)).
THE ARM IS THE REFUSAL, so an unset cause is a contract breach and an
arm this codec does not know is refused as an unknown field."
  (let ((message "UpdateShutdownScheduleError"))
    (agent-repl-wire-verbs--check-keys message json '(nothingScheduled))
    (list :cause
          (agent-repl-wire-verbs--decode-oneof
           message "cause" json
           (list (list 'nothingScheduled :nothing-scheduled #'agent-repl-wire-decode-update-shutdown-schedule-error-nothing-scheduled))))))

(defun agent-repl-wire-decode-update-shutdown-schedule-response-success (json)
  "Decode UpdateShutdownScheduleResponse's `success' arm from JSON."
  (agent-repl-wire-decode-update-shutdown-schedule-success json))

(defun agent-repl-wire-decode-update-shutdown-schedule-response-error (json)
  "Decode UpdateShutdownScheduleResponse's `error' arm from JSON."
  (agent-repl-wire-decode-update-shutdown-schedule-error json))

(defun agent-repl-wire-decode-update-shutdown-schedule-response (json)
  "Decode UpdateShutdownScheduleResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-result
   "UpdateShutdownScheduleResponse" json
   #'agent-repl-wire-decode-update-shutdown-schedule-response-success
   #'agent-repl-wire-decode-update-shutdown-schedule-response-error))


;;;; ---- UpdatePersistentWifiMode ---------------------------------------

(defun agent-repl-wire-verbs--decode-string-fields (message json fields)
  "Decode MESSAGE out of JSON as a message whose every field is a string.
FIELDS is a list of (WIRE-SYMBOL KEYWORD).  Returns a plist of KEYWORD to
the decoded string, after refusing any key not among FIELDS."
  (agent-repl-wire-verbs--check-keys message json (mapcar #'car fields))
  (let (out)
    (dolist (field fields)
      (setq out (plist-put out (nth 1 field)
                           (agent-repl-wire-verbs--decode-string message (nth 0 field) json))))
    out))

(defun agent-repl-wire-decode-persistent-wifi-joined (json)
  "Decode PersistentWifiJoined from JSON into (:network-name NAME).
NAME is nil when macOS withholds the network's name."
  (let ((object (agent-repl-wire--object "PersistentWifiJoined" json)))
    (agent-repl-wire--check-keys "PersistentWifiJoined" object '(networkName))
    (list :network-name (agent-repl-wire--decode-optional-string
                         "PersistentWifiJoined" 'networkName object))))

(defun agent-repl-wire-decode-persistent-wifi-not-joined (json)
  "Decode the empty PersistentWifiNotJoined from JSON."
  (agent-repl-wire-verbs--decode-empty "PersistentWifiNotJoined" json))

(defun agent-repl-wire-decode-persistent-wifi-mode-on (json)
  "Decode the empty PersistentWifiModeOn from JSON."
  (agent-repl-wire-verbs--decode-empty "PersistentWifiModeOn" json))

(defun agent-repl-wire-decode-persistent-wifi-mode-off (json)
  "Decode the empty PersistentWifiModeOff from JSON."
  (agent-repl-wire-verbs--decode-empty "PersistentWifiModeOff" json))

(defun agent-repl-wire-decode-persistent-wifi-state (json)
  "Decode PersistentWifiState from JSON into (:wifi ONEOF :mode ONEOF).
Each ONEOF is (:arm ARM :value V), or nil when the daemon could not read
that fact -- the two facts are independent, and an unassigned one is the
daemon saying it does not know, never a contract breach.  Shared by the
WatchDaemon push and UpdatePersistentWifiMode's success."
  (let ((message "PersistentWifiState")
        (object (agent-repl-wire--object "PersistentWifiState" json)))
    (agent-repl-wire--check-keys message object '(joined notJoined on off))
    (agent-repl-wire--decoded
     message
     (list :wifi (agent-repl-wire--decode-oneof
                  message 'wifi object
                  '((joined :joined agent-repl-wire-decode-persistent-wifi-joined)
                    (notJoined :not-joined agent-repl-wire-decode-persistent-wifi-not-joined))
                  t)
           :mode (agent-repl-wire--decode-oneof
                  message 'mode object
                  '((on :on agent-repl-wire-decode-persistent-wifi-mode-on)
                    (off :off agent-repl-wire-decode-persistent-wifi-mode-off))
                  t)))))

(defun agent-repl-wire-encode-update-persistent-wifi-mode-request (request)
  "Encode UpdatePersistentWifiModeRequest from plist REQUEST (:action ONEOF).
ONEOF is `(:arm :on)', `(:arm :off)' or `(:arm :toggle)'; every arm is
empty, so the arm is the whole action."
  (list (agent-repl-wire-verbs--encode-oneof
         "UpdatePersistentWifiModeRequest" "action" (plist-get request :action)
         (list (list :on 'on #'ignore)
               (list :off 'off #'ignore)
               (list :toggle 'toggle #'ignore)))))

(defconst agent-repl-wire-persistent-wifi-hotspot-arms
  '((joined :joined ((networkName :network-name)))
    (alreadyJoined :already-joined ((networkName :network-name)))
    (left :left ((networkName :network-name)))
    (notOnHotspot :not-on-hotspot nil)
    (networkUnreadable :network-unreadable nil)
    (noWifiInterface :no-wifi-interface nil)
    (failed :failed ((networkName :network-name) (detail :detail))))
  "UpdatePersistentWifiModeHotspot's outcome arms: (WIRE KEYWORD FIELDS).
Every arm's message is all strings, so FIELDS is its whole schema.")

(defconst agent-repl-wire-persistent-wifi-display-arms
  '((dimmed :dimmed nil)
    (restored :restored nil)
    (toolMissing :tool-missing ((toolPath :tool-path)))
    (failed :failed ((detail :detail))))
  "UpdatePersistentWifiModeDisplay's outcome arms: (WIRE KEYWORD FIELDS).")

(defconst agent-repl-wire-persistent-wifi-error-arms
  '((powerSettingsRefused :power-settings-refused ((detail :detail)))
    (modeUnreadable :mode-unreadable ((detail :detail))))
  "UpdatePersistentWifiModeError's cause arms: (WIRE KEYWORD FIELDS).")

(defun agent-repl-wire-verbs--decode-string-arms (message field json arms)
  "Decode MESSAGE's required oneof FIELD from JSON over string-only ARMS.
ARMS is a list of (WIRE KEYWORD FIELDS) as in
`agent-repl-wire-persistent-wifi-hotspot-arms'; each arm decodes with
`agent-repl-wire-verbs--decode-string-fields' named WIRE."
  (agent-repl-wire-verbs--check-keys message json (mapcar #'car arms))
  (agent-repl-wire-verbs--decode-oneof
   message field json
   (mapcar (lambda (arm)
             (let ((fields (nth 2 arm))
                   (arm-name (format "%s.%s" message (nth 0 arm))))
               (list (nth 0 arm) (nth 1 arm)
                     (lambda (value)
                       (agent-repl-wire-verbs--decode-string-fields arm-name value fields)))))
           arms)))

(defun agent-repl-wire-decode-update-persistent-wifi-mode-success (json)
  "Decode UpdatePersistentWifiModeSuccess from JSON.
Returns (:state STATE :hotspot ONEOF :display ONEOF): the standing re-read
after the change, and how the hotspot and display steps went."
  (let ((message "UpdatePersistentWifiModeSuccess"))
    (agent-repl-wire-verbs--check-keys message json '(state hotspot display))
    ;; Presence, not value: protojson spells a message with nothing set as
    ;; `{}', which parses to nil, and a standing with both facts unread is
    ;; exactly that.
    (list :state (agent-repl-wire--decode-message
                  message 'state json #'agent-repl-wire-decode-persistent-wifi-state)
          :hotspot (agent-repl-wire--decode-message
                    message 'hotspot json
                    (lambda (value)
                      (agent-repl-wire-verbs--decode-string-arms
                       "UpdatePersistentWifiModeHotspot" "outcome" value
                       agent-repl-wire-persistent-wifi-hotspot-arms)))
          :display (agent-repl-wire--decode-message
                    message 'display json
                    (lambda (value)
                      (agent-repl-wire-verbs--decode-string-arms
                       "UpdatePersistentWifiModeDisplay" "outcome" value
                       agent-repl-wire-persistent-wifi-display-arms))))))

(defun agent-repl-wire-decode-update-persistent-wifi-mode-error (json)
  "Decode UpdatePersistentWifiModeError from JSON into (:cause ONEOF)."
  (list :cause (agent-repl-wire-verbs--decode-string-arms
                "UpdatePersistentWifiModeError" "cause" json
                agent-repl-wire-persistent-wifi-error-arms)))

(defun agent-repl-wire-decode-update-persistent-wifi-mode-response (json)
  "Decode UpdatePersistentWifiModeResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-result
   "UpdatePersistentWifiModeResponse" json
   #'agent-repl-wire-decode-update-persistent-wifi-mode-success
   #'agent-repl-wire-decode-update-persistent-wifi-mode-error))


;;;; ---- Deploy -------------------------------------------------------

(defun agent-repl-wire-encode-deploy-request (request)
  "Encode DeployRequest from plist REQUEST (:force BOOL).
FORCED is spelled explicitly either way: an unforced deploy ends no turn,
and a forced one does not wait for in-flight work."
  (list (cons 'force (agent-repl-wire-verbs--encode-bool (plist-get request :force)))))

(defconst agent-repl-wire-deploy-components
  '(("DEPLOY_COMPONENT_DAEMON" . :daemon)
    ("DEPLOY_COMPONENT_SHIM" . :shim)
    ("DEPLOY_COMPONENT_WEBAPP" . :webapp)
    ("DEPLOY_COMPONENT_STORE" . :store)
    ("DEPLOY_COMPONENT_SIDECAR" . :sidecar)
    ("DEPLOY_COMPONENT_ELISP" . :elisp))
  "The DeployComponent vocabulary, wire name to keyword.
UNSPECIFIED is deliberately ABSENT: it is never sent, and an outcome
carrying it is malformed.")

(defun agent-repl-wire-decode-deploy-component (message field json)
  "Decode MESSAGE's REQUIRED DeployComponent FIELD out of JSON as a keyword.
Absent (protojson's spelling of UNSPECIFIED), UNSPECIFIED itself and an
unknown name are all contract breaches."
  (let* ((raw (agent-repl-wire-verbs--decode-string message field json))
         (keyword (cdr (assoc raw agent-repl-wire-deploy-components))))
    (cond
     ((member raw '("" "DEPLOY_COMPONENT_UNSPECIFIED"))
      (agent-repl-wire-verbs--fail message (symbol-name field) "required field is unset"))
     ((null keyword)
      (agent-repl-wire-verbs--fail message (symbol-name field)
                                   (format "unknown enum value %S" raw)))
     (t keyword))))

(defun agent-repl-wire-decode-deploy-up-to-date (json)
  "Decode DeployUpToDate from JSON.  Empty: presence is the fact."
  (agent-repl-wire-verbs--decode-empty "DeployUpToDate" json))

(defun agent-repl-wire-decode-deploy-service-restarted (json)
  "Decode DeployServiceRestarted from JSON.  Empty: presence is the fact."
  (agent-repl-wire-verbs--decode-empty "DeployServiceRestarted" json))

(defun agent-repl-wire-decode-deploy-handing-over (json)
  "Decode DeployHandingOver from JSON into (:workspaces N :busy N :forced B)."
  (let ((message "DeployHandingOver"))
    (agent-repl-wire-verbs--check-keys message json '(workspaces busy forced))
    (list :workspaces (agent-repl-wire--decode-uint32 message 'workspaces json)
          :busy (agent-repl-wire--decode-uint32 message 'busy json)
          :forced (agent-repl-wire--decode-bool message 'forced json))))

(defun agent-repl-wire-decode-deploy-restarting (json)
  "Decode DeployRestarting from JSON.
Returns (:running-state-layout N :fresh-state-layout N :workspaces N
:busy N :forced B): a stop-then-start restart because the fresh build
writes a different state layout than the running one."
  (let ((message "DeployRestarting"))
    (agent-repl-wire-verbs--check-keys
     message json '(runningStateLayout freshStateLayout workspaces busy forced))
    (list :running-state-layout (agent-repl-wire--decode-uint32 message 'runningStateLayout json)
          :fresh-state-layout (agent-repl-wire--decode-uint32 message 'freshStateLayout json)
          :workspaces (agent-repl-wire--decode-uint32 message 'workspaces json)
          :busy (agent-repl-wire--decode-uint32 message 'busy json)
          :forced (agent-repl-wire--decode-bool message 'forced json))))

(defun agent-repl-wire-decode-deploy-bounced-now (json)
  "Decode DeployBouncedNow from JSON into (:forced B)."
  (let ((message "DeployBouncedNow"))
    (agent-repl-wire-verbs--check-keys message json '(forced))
    (list :forced (agent-repl-wire--decode-bool message 'forced json))))

(defun agent-repl-wire-decode-deploy-bounce-registered (json)
  "Decode DeployBounceRegistered from JSON.
Returns (:turn-in-flight B :detached-work N)."
  (let ((message "DeployBounceRegistered"))
    (agent-repl-wire-verbs--check-keys message json '(turnInFlight detachedWork))
    (list :turn-in-flight (agent-repl-wire--decode-bool message 'turnInFlight json)
          :detached-work (agent-repl-wire--decode-uint32 message 'detachedWork json))))

(defun agent-repl-wire-decode-deploy-shim-bounce-bounced-now (json)
  "Decode DeployShimBounce's `bounced_now' when arm from JSON."
  (agent-repl-wire-decode-deploy-bounced-now json))

(defun agent-repl-wire-decode-deploy-shim-bounce-registered (json)
  "Decode DeployShimBounce's `registered' when arm from JSON."
  (agent-repl-wire-decode-deploy-bounce-registered json))

(defun agent-repl-wire-decode-deploy-shim-bounce (json)
  "Decode DeployShimBounce from JSON.
Returns (:workspace ID :when (:arm ARM :value V)).  The workspace is
REQUIRED, and THE ARM IS WHEN, so an unset one is a breach."
  (let ((message "DeployShimBounce"))
    (agent-repl-wire-verbs--check-keys message json '(workspace bouncedNow registered))
    (list :workspace (agent-repl-wire-verbs--decode-required-string message 'workspace json)
          :when (agent-repl-wire-verbs--decode-oneof
                 message "when" json
                 (list (list 'bouncedNow :bounced-now
                             #'agent-repl-wire-decode-deploy-shim-bounce-bounced-now)
                       (list 'registered :registered
                             #'agent-repl-wire-decode-deploy-shim-bounce-registered))))))

(defun agent-repl-wire-decode-deploy-shim-bounces-bounces (json)
  "Decode DeployShimBounces' `bounces' element from JSON."
  (agent-repl-wire-decode-deploy-shim-bounce json))

(defun agent-repl-wire-decode-deploy-shim-bounces (json)
  "Decode DeployShimBounces from JSON into (:bounces LIST)."
  (let ((message "DeployShimBounces"))
    (agent-repl-wire-verbs--check-keys message json '(bounces))
    (list :bounces (agent-repl-wire-verbs--decode-repeated
                    message 'bounces json
                    #'agent-repl-wire-decode-deploy-shim-bounces-bounces))))

(defun agent-repl-wire-decode-deploy-reload-pushed (json)
  "Decode DeployReloadPushed from JSON into (:recipients N)."
  (let ((message "DeployReloadPushed"))
    (agent-repl-wire-verbs--check-keys message json '(recipients))
    (list :recipients (agent-repl-wire--decode-uint32 message 'recipients json))))

(defun agent-repl-wire-decode-deploy-deferred-to-successor (json)
  "Decode DeployDeferredToSuccessor from JSON.  Empty: presence is the fact."
  (agent-repl-wire-verbs--decode-empty "DeployDeferredToSuccessor" json))

(defun agent-repl-wire-decode-deploy-component-outcome-up-to-date (json)
  "Decode DeployComponentOutcome's `up_to_date' arm from JSON."
  (agent-repl-wire-decode-deploy-up-to-date json))

(defun agent-repl-wire-decode-deploy-component-outcome-restarted (json)
  "Decode DeployComponentOutcome's `restarted' arm from JSON."
  (agent-repl-wire-decode-deploy-service-restarted json))

(defun agent-repl-wire-decode-deploy-component-outcome-handing-over (json)
  "Decode DeployComponentOutcome's `handing_over' arm from JSON."
  (agent-repl-wire-decode-deploy-handing-over json))

(defun agent-repl-wire-decode-deploy-component-outcome-restarting (json)
  "Decode DeployComponentOutcome's `restarting' arm from JSON."
  (agent-repl-wire-decode-deploy-restarting json))

(defun agent-repl-wire-decode-deploy-component-outcome-shims (json)
  "Decode DeployComponentOutcome's `shims' arm from JSON."
  (agent-repl-wire-decode-deploy-shim-bounces json))

(defun agent-repl-wire-decode-deploy-component-outcome-reload-pushed (json)
  "Decode DeployComponentOutcome's `reload_pushed' arm from JSON."
  (agent-repl-wire-decode-deploy-reload-pushed json))

(defun agent-repl-wire-decode-deploy-component-outcome-deferred-to-successor (json)
  "Decode DeployComponentOutcome's `deferred_to_successor' arm from JSON."
  (agent-repl-wire-decode-deploy-deferred-to-successor json))

(defun agent-repl-wire-decode-deploy-component-outcome (json)
  "Decode DeployComponentOutcome from JSON.
Returns (:component KEYWORD :build HASH :outcome (:arm ARM :value V)).  The
component and the build are REQUIRED, and THE ARM IS THE DECISION, so an
unset one is a contract breach."
  (let ((message "DeployComponentOutcome"))
    (agent-repl-wire-verbs--check-keys
     message json
     '(component build upToDate restarted handingOver shims reloadPushed deferredToSuccessor
       restarting))
    (list :component (agent-repl-wire-decode-deploy-component message 'component json)
          :build (agent-repl-wire-verbs--decode-required-string message 'build json)
          :outcome
          (agent-repl-wire-verbs--decode-oneof
           message "outcome" json
           (list (list 'upToDate :up-to-date
                       #'agent-repl-wire-decode-deploy-component-outcome-up-to-date)
                 (list 'restarted :restarted
                       #'agent-repl-wire-decode-deploy-component-outcome-restarted)
                 (list 'handingOver :handing-over
                       #'agent-repl-wire-decode-deploy-component-outcome-handing-over)
                 (list 'shims :shims
                       #'agent-repl-wire-decode-deploy-component-outcome-shims)
                 (list 'reloadPushed :reload-pushed
                       #'agent-repl-wire-decode-deploy-component-outcome-reload-pushed)
                 (list 'deferredToSuccessor :deferred-to-successor
                       #'agent-repl-wire-decode-deploy-component-outcome-deferred-to-successor)
                 (list 'restarting :restarting
                       #'agent-repl-wire-decode-deploy-component-outcome-restarting))))))

(defun agent-repl-wire-decode-deploy-success-components (json)
  "Decode DeploySuccess' `components' element from JSON."
  (agent-repl-wire-decode-deploy-component-outcome json))

(defun agent-repl-wire-decode-deploy-success (json)
  "Decode DeploySuccess from JSON into (:components LIST), in build order."
  (let ((message "DeploySuccess"))
    (agent-repl-wire-verbs--check-keys message json '(components))
    (list :components (agent-repl-wire-verbs--decode-repeated
                       message 'components json
                       #'agent-repl-wire-decode-deploy-success-components))))

(defun agent-repl-wire-decode-deploy-build-failed (json)
  "Decode DeployBuildFailed from JSON into (:step S :detail D :log L).
The step and the detail are REQUIRED; the log path may be empty."
  (let ((message "DeployBuildFailed"))
    (agent-repl-wire-verbs--check-keys message json '(step detail log))
    (list :step (agent-repl-wire-verbs--decode-required-string message 'step json)
          :detail (agent-repl-wire-verbs--decode-required-string message 'detail json)
          :log (agent-repl-wire-verbs--decode-string message 'log json))))

(defun agent-repl-wire-decode-deploy-already-deploying (json)
  "Decode DeployAlreadyDeploying from JSON.  Empty: a deploy is running."
  (agent-repl-wire-verbs--decode-empty "DeployAlreadyDeploying" json))

(defun agent-repl-wire-decode-deploy-already-rolling-out (json)
  "Decode DeployAlreadyRollingOut from JSON into (:waiting-on IDS)."
  (let ((message "DeployAlreadyRollingOut"))
    (agent-repl-wire-verbs--check-keys message json '(waitingOn))
    (list :waiting-on
          (agent-repl-wire-verbs--decode-repeated-string message 'waitingOn json))))

(defun agent-repl-wire-decode-deploy-joining (json)
  "Decode DeployJoining from JSON.  Empty: the daemon is a joining successor."
  (agent-repl-wire-verbs--decode-empty "DeployJoining" json))

(defun agent-repl-wire-decode-deploy-service-restart-failed (json)
  "Decode DeployServiceRestartFailed from JSON into (:component K :detail D).
Both are REQUIRED."
  (let ((message "DeployServiceRestartFailed"))
    (agent-repl-wire-verbs--check-keys message json '(component detail))
    (list :component (agent-repl-wire-decode-deploy-component message 'component json)
          :detail (agent-repl-wire-verbs--decode-required-string message 'detail json))))

(defun agent-repl-wire-decode-deploy-install-failed (json)
  "Decode DeployInstallFailed from JSON into (:component K :detail D).
Both are REQUIRED."
  (let ((message "DeployInstallFailed"))
    (agent-repl-wire-verbs--check-keys message json '(component detail))
    (list :component (agent-repl-wire-decode-deploy-component message 'component json)
          :detail (agent-repl-wire-verbs--decode-required-string message 'detail json))))

(defun agent-repl-wire-decode-deploy-error-build-failed (json)
  "Decode DeployError's `build_failed' cause arm from JSON."
  (agent-repl-wire-decode-deploy-build-failed json))

(defun agent-repl-wire-decode-deploy-error-already-deploying (json)
  "Decode DeployError's `already_deploying' cause arm from JSON."
  (agent-repl-wire-decode-deploy-already-deploying json))

(defun agent-repl-wire-decode-deploy-error-already-rolling-out (json)
  "Decode DeployError's `already_rolling_out' cause arm from JSON."
  (agent-repl-wire-decode-deploy-already-rolling-out json))

(defun agent-repl-wire-decode-deploy-error-joining (json)
  "Decode DeployError's `joining' cause arm from JSON."
  (agent-repl-wire-decode-deploy-joining json))

(defun agent-repl-wire-decode-deploy-error-service-restart-failed (json)
  "Decode DeployError's `service_restart_failed' cause arm from JSON."
  (agent-repl-wire-decode-deploy-service-restart-failed json))

(defun agent-repl-wire-decode-deploy-error-install-failed (json)
  "Decode DeployError's `install_failed' cause arm from JSON."
  (agent-repl-wire-decode-deploy-install-failed json))

(defun agent-repl-wire-decode-deploy-error (json)
  "Decode DeployError from JSON into (:cause (:arm ARM :value V)).
THE ARM IS THE REFUSAL, so an unset cause is a contract breach and an
arm this codec does not know is refused as an unknown field."
  (let ((message "DeployError"))
    (agent-repl-wire-verbs--check-keys
     message json
     '(buildFailed alreadyDeploying alreadyRollingOut joining serviceRestartFailed installFailed))
    (list :cause
          (agent-repl-wire-verbs--decode-oneof
           message "cause" json
           (list (list 'buildFailed :build-failed
                       #'agent-repl-wire-decode-deploy-error-build-failed)
                 (list 'alreadyDeploying :already-deploying
                       #'agent-repl-wire-decode-deploy-error-already-deploying)
                 (list 'alreadyRollingOut :already-rolling-out
                       #'agent-repl-wire-decode-deploy-error-already-rolling-out)
                 (list 'joining :joining
                       #'agent-repl-wire-decode-deploy-error-joining)
                 (list 'serviceRestartFailed :service-restart-failed
                       #'agent-repl-wire-decode-deploy-error-service-restart-failed)
                 (list 'installFailed :install-failed
                       #'agent-repl-wire-decode-deploy-error-install-failed))))))

(defun agent-repl-wire-decode-deploy-response-success (json)
  "Decode DeployResponse's `success' arm from JSON."
  (agent-repl-wire-decode-deploy-success json))

(defun agent-repl-wire-decode-deploy-response-error (json)
  "Decode DeployResponse's `error' arm from JSON."
  (agent-repl-wire-decode-deploy-error json))

(defun agent-repl-wire-decode-deploy-response (json)
  "Decode DeployResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-result
   "DeployResponse" json
   #'agent-repl-wire-decode-deploy-response-success
   #'agent-repl-wire-decode-deploy-response-error))


;;;; ---- UpdateMergeQueue -----------------------------------------------

(defun agent-repl-wire-encode-update-merge-queue-pause-repository (ref)
  "Encode UpdateMergeQueuePause's `repository' use site from REF."
  (agent-repl-wire-encode-repository-ref ref))

(defun agent-repl-wire-encode-update-merge-queue-pause (value)
  "Encode UpdateMergeQueuePause from plist VALUE (:repository REF).
`repository' is OPTIONAL and its ABSENCE IS THE DAEMON-WIDE SWITCH: unset
means every repository that has a queue, so an absent ref is omitted from
the encoding rather than sent as an empty one."
  (let ((ref (plist-get value :repository)))
    (when ref
      (list (cons 'repository
                  (agent-repl-wire-encode-update-merge-queue-pause-repository ref))))))

(defun agent-repl-wire-encode-update-merge-queue-resume-repository (ref)
  "Encode UpdateMergeQueueResume's `repository' use site from REF."
  (agent-repl-wire-encode-repository-ref ref))

(defun agent-repl-wire-encode-update-merge-queue-resume (value)
  "Encode UpdateMergeQueueResume from plist VALUE (:repository REF).
`repository' is OPTIONAL and its absence is the daemon-wide switch, the
same as on the pause arm."
  (let ((ref (plist-get value :repository)))
    (when ref
      (list (cons 'repository
                  (agent-repl-wire-encode-update-merge-queue-resume-repository ref))))))

(defun agent-repl-wire-encode-update-merge-queue-evict-workspace (ref)
  "Encode UpdateMergeQueueEvict's `workspace' use site from REF."
  (agent-repl-wire-encode-workspace-ref ref))

(defun agent-repl-wire-encode-update-merge-queue-evict (value)
  "Encode UpdateMergeQueueEvict from plist VALUE (:workspace REF)."
  (list (cons 'workspace
              (agent-repl-wire-encode-update-merge-queue-evict-workspace
               (agent-repl-wire-verbs--require "UpdateMergeQueueEvict" "workspace"
                                                (plist-get value :workspace))))))

(defun agent-repl-wire-encode-update-merge-queue-request-pause (value)
  "Encode UpdateMergeQueueRequest's `pause' action arm from VALUE."
  (agent-repl-wire-encode-update-merge-queue-pause value))

(defun agent-repl-wire-encode-update-merge-queue-request-resume (value)
  "Encode UpdateMergeQueueRequest's `resume' action arm from VALUE."
  (agent-repl-wire-encode-update-merge-queue-resume value))

(defun agent-repl-wire-encode-update-merge-queue-request-evict (value)
  "Encode UpdateMergeQueueRequest's `evict' action arm from VALUE."
  (agent-repl-wire-encode-update-merge-queue-evict value))

(defun agent-repl-wire-encode-update-merge-queue-request (request)
  "Encode UpdateMergeQueueRequest from plist REQUEST (:action ONEOF)."
  (list (agent-repl-wire-verbs--encode-oneof
         "UpdateMergeQueueRequest" "action" (plist-get request :action)
         (list (list :pause 'pause #'agent-repl-wire-encode-update-merge-queue-request-pause)
               (list :resume 'resume #'agent-repl-wire-encode-update-merge-queue-request-resume)
               (list :evict 'evict #'agent-repl-wire-encode-update-merge-queue-request-evict)))))

(defun agent-repl-wire-decode-update-merge-queue-success (json)
  "Decode UpdateMergeQueueSuccess from JSON.  Empty: the bubbles reflect it."
  (agent-repl-wire-verbs--decode-empty "UpdateMergeQueueSuccess" json))

(defun agent-repl-wire-decode-update-merge-queue-unknown-workspace (json)
  "Decode UpdateMergeQueueUnknownWorkspace from JSON.  Empty: The workspace id
is not in the daemon's registry."
  (agent-repl-wire-verbs--decode-empty "UpdateMergeQueueUnknownWorkspace" json))

(defun agent-repl-wire-decode-update-merge-queue-workspace-ref-mismatch (json)
  "Decode UpdateMergeQueueWorkspaceRefMismatch from JSON into a plist
(`:registry-dir').
The echoed dir disagrees with the registry's dir for this id."
  (let ((message "UpdateMergeQueueWorkspaceRefMismatch"))
    (agent-repl-wire-verbs--check-keys message json '(registryDir))
    (list :registry-dir (agent-repl-wire-verbs--decode-string
                       message 'registryDir json))))

(defun agent-repl-wire-decode-update-merge-queue-transferring-away (json)
  "Decode UpdateMergeQueueTransferringAway from JSON into a plist (`:address').
This daemon released the workspace to a successor; dial `address'."
  (let ((message "UpdateMergeQueueTransferringAway"))
    (agent-repl-wire-verbs--check-keys message json '(address))
    (list :address (agent-repl-wire-verbs--decode-string
                       message 'address json))))

(defun agent-repl-wire-decode-update-merge-queue-not-yet-adopted (json)
  "Decode UpdateMergeQueueNotYetAdopted from JSON.  Empty: A joining daemon has
not finished adopting this workspace yet."
  (agent-repl-wire-verbs--decode-empty "UpdateMergeQueueNotYetAdopted" json))

(defun agent-repl-wire-decode-update-merge-queue-already-paused (json)
  "Decode UpdateMergeQueueAlreadyPaused from JSON.  Empty: The queue is already
paused."
  (agent-repl-wire-verbs--decode-empty "UpdateMergeQueueAlreadyPaused" json))

(defun agent-repl-wire-decode-update-merge-queue-not-paused (json)
  "Decode UpdateMergeQueueNotPaused from JSON.  Empty: The queue is not paused."
  (agent-repl-wire-verbs--decode-empty "UpdateMergeQueueNotPaused" json))

(defun agent-repl-wire-decode-update-merge-queue-no-such-queued-merge (json)
  "Decode UpdateMergeQueueNoSuchQueuedMerge from JSON.  Empty: No queued merge
for that workspace."
  (agent-repl-wire-verbs--decode-empty "UpdateMergeQueueNoSuchQueuedMerge" json))

(defun agent-repl-wire-decode-update-merge-queue-unknown-repository (json)
  "Decode UpdateMergeQueueUnknownRepository from JSON.  Empty: `repository'
named a RepositoryRef the daemon's registry does not hold."
  (agent-repl-wire-verbs--decode-empty "UpdateMergeQueueUnknownRepository" json))

(defun agent-repl-wire-decode-update-merge-queue-error-unknown-workspace (json)
  "Decode UpdateMergeQueueError's `unknown_workspace' cause arm from JSON."
  (agent-repl-wire-decode-update-merge-queue-unknown-workspace json))

(defun agent-repl-wire-decode-update-merge-queue-error-workspace-ref-mismatch (json)
  "Decode UpdateMergeQueueError's `workspace_ref_mismatch' cause arm from JSON."
  (agent-repl-wire-decode-update-merge-queue-workspace-ref-mismatch json))

(defun agent-repl-wire-decode-update-merge-queue-error-transferring-away (json)
  "Decode UpdateMergeQueueError's `transferring_away' cause arm from JSON."
  (agent-repl-wire-decode-update-merge-queue-transferring-away json))

(defun agent-repl-wire-decode-update-merge-queue-error-not-yet-adopted (json)
  "Decode UpdateMergeQueueError's `not_yet_adopted' cause arm from JSON."
  (agent-repl-wire-decode-update-merge-queue-not-yet-adopted json))

(defun agent-repl-wire-decode-update-merge-queue-error-already-paused (json)
  "Decode UpdateMergeQueueError's `already_paused' cause arm from JSON."
  (agent-repl-wire-decode-update-merge-queue-already-paused json))

(defun agent-repl-wire-decode-update-merge-queue-error-not-paused (json)
  "Decode UpdateMergeQueueError's `not_paused' cause arm from JSON."
  (agent-repl-wire-decode-update-merge-queue-not-paused json))

(defun agent-repl-wire-decode-update-merge-queue-error-no-such-queued-merge (json)
  "Decode UpdateMergeQueueError's `no_such_queued_merge' cause arm from JSON."
  (agent-repl-wire-decode-update-merge-queue-no-such-queued-merge json))

(defun agent-repl-wire-decode-update-merge-queue-error-unknown-repository (json)
  "Decode UpdateMergeQueueError's `unknown_repository' cause arm from JSON."
  (agent-repl-wire-decode-update-merge-queue-unknown-repository json))

(defun agent-repl-wire-decode-update-merge-queue-error (json)
  "Decode UpdateMergeQueueError from JSON into (:cause (:arm ARM :value V)).
THE ARM IS THE REFUSAL, so an unset cause is a contract breach and an
arm this codec does not know is refused as an unknown field."
  (let ((message "UpdateMergeQueueError"))
    (agent-repl-wire-verbs--check-keys message json '(unknownWorkspace workspaceRefMismatch transferringAway notYetAdopted alreadyPaused notPaused noSuchQueuedMerge unknownRepository))
    (list :cause
          (agent-repl-wire-verbs--decode-oneof
           message "cause" json
           (list (list 'unknownWorkspace :unknown-workspace #'agent-repl-wire-decode-update-merge-queue-error-unknown-workspace)
         (list 'workspaceRefMismatch :workspace-ref-mismatch #'agent-repl-wire-decode-update-merge-queue-error-workspace-ref-mismatch)
         (list 'transferringAway :transferring-away #'agent-repl-wire-decode-update-merge-queue-error-transferring-away)
         (list 'notYetAdopted :not-yet-adopted #'agent-repl-wire-decode-update-merge-queue-error-not-yet-adopted)
         (list 'alreadyPaused :already-paused #'agent-repl-wire-decode-update-merge-queue-error-already-paused)
         (list 'notPaused :not-paused #'agent-repl-wire-decode-update-merge-queue-error-not-paused)
         (list 'noSuchQueuedMerge :no-such-queued-merge #'agent-repl-wire-decode-update-merge-queue-error-no-such-queued-merge)
         (list 'unknownRepository :unknown-repository #'agent-repl-wire-decode-update-merge-queue-error-unknown-repository))))))

(defun agent-repl-wire-decode-update-merge-queue-response-success (json)
  "Decode UpdateMergeQueueResponse's `success' arm from JSON."
  (agent-repl-wire-decode-update-merge-queue-success json))

(defun agent-repl-wire-decode-update-merge-queue-response-error (json)
  "Decode UpdateMergeQueueResponse's `error' arm from JSON."
  (agent-repl-wire-decode-update-merge-queue-error json))

(defun agent-repl-wire-decode-update-merge-queue-response (json)
  "Decode UpdateMergeQueueResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-result
   "UpdateMergeQueueResponse" json
   #'agent-repl-wire-decode-update-merge-queue-response-success
   #'agent-repl-wire-decode-update-merge-queue-response-error))


;;;; ---- DaemonHealth ---------------------------------------------------

(defun agent-repl-wire-encode-daemon-health-request (&optional _request)
  "Encode DaemonHealthRequest.  Nothing to ask beyond \"you?\"."
  (agent-repl--log '(:agent-repl-context "a codec call outside a request has no workspace") "elisp.wire.verbs-encode-daemon-health-request")
  nil)

(defun agent-repl-wire-decode-daemon-fault-adoption-window-expired-workspace (json)
  "Decode DaemonFaultAdoptionWindowExpired's `workspace' use site from JSON."
  (agent-repl-wire-decode-workspace-ref json))

(defun agent-repl-wire-decode-daemon-fault-adoption-window-expired (json)
  "Decode DaemonFaultAdoptionWindowExpired from JSON into (:workspace REF).
The workspace whose adoption window expired."
  (let ((message "DaemonFaultAdoptionWindowExpired"))
    (agent-repl-wire-verbs--check-keys message json '(workspace))
    (list :workspace (agent-repl-wire-decode-daemon-fault-adoption-window-expired-workspace
                      (agent-repl-wire-verbs--require
                       message "workspace" (alist-get 'workspace json))))))

(defun agent-repl-wire-decode-daemon-fault-log-sink-poisoned (json)
  "Decode DaemonFaultLogSinkPoisoned from JSON into (:sink).
Which sink."
  (let ((message "DaemonFaultLogSinkPoisoned"))
    (agent-repl-wire-verbs--check-keys message json '(sink))
    (list :sink (agent-repl-wire-verbs--decode-string message 'sink json))))

(defun agent-repl-wire-decode-daemon-fault-successor-spawn-failed (json)
  "Decode DaemonFaultSuccessorSpawnFailed from JSON into (:detail).
The spawn's own account of the failure."
  (let ((message "DaemonFaultSuccessorSpawnFailed"))
    (agent-repl-wire-verbs--check-keys message json '(detail))
    (list :detail (agent-repl-wire-verbs--decode-string message 'detail json))))

(defun agent-repl-wire-decode-daemon-fault-prompts-dir-missing (json)
  "Decode DaemonFaultPromptsDirMissing from JSON into (:path).
The path that is not there."
  (let ((message "DaemonFaultPromptsDirMissing"))
    (agent-repl-wire-verbs--check-keys message json '(path))
    (list :path (agent-repl-wire-verbs--decode-string message 'path json))))

(defun agent-repl-wire-decode-daemon-fault-wsm-read-only (json)
  "Decode DaemonFaultWsmReadOnly from JSON.  Empty: the arm is the whole fact."
  (agent-repl-wire-verbs--decode-empty "DaemonFaultWsmReadOnly" json))

(defun agent-repl-wire-decode-daemon-fault-daemon-state-unreadable (json)
  "Decode DaemonFaultDaemonStateUnreadable from JSON into (:cause).
The self-check's OWN fault: the state client would not answer, so the
daemon's standing faults could not be read at all.  It is the one fault the
daemon can always detect about itself, and it says THE ANSWER IS INCOMPLETE
rather than naming a condition the daemon is in.  Before this arm existed the
self-check was the one site that put a fault on the wire with the `kind'
oneof unset, which every consumer reads as a contract breach."
  (let ((message "DaemonFaultDaemonStateUnreadable"))
    (agent-repl-wire-verbs--check-keys message json '(cause))
    (list :cause (agent-repl-wire-verbs--decode-string message 'cause json))))

(defun agent-repl-wire-decode-daemon-fault-deploy-failed-build (json)
  "Decode DaemonFaultDeployFailed's `build' step arm from JSON as a
`DeployBuildFailed'."
  (agent-repl-wire-decode-deploy-build-failed json))

(defun agent-repl-wire-decode-daemon-fault-deploy-failed-install (json)
  "Decode DaemonFaultDeployFailed's `install' step arm from JSON as a
`DeployInstallFailed'."
  (agent-repl-wire-decode-deploy-install-failed json))

(defun agent-repl-wire-decode-daemon-fault-deploy-failed-restart-services (json)
  "Decode DaemonFaultDeployFailed's `restart_services' step arm from JSON as
a `DeployServiceRestartFailed'."
  (agent-repl-wire-decode-deploy-service-restart-failed json))

(defun agent-repl-wire-decode-deploy-rollback-failed (json)
  "Decode DeployRollbackFailed from JSON into (:component K :detail D).
A failed deploy's rollback did not restore the previous build.  Both are
REQUIRED."
  (let ((message "DeployRollbackFailed"))
    (agent-repl-wire-verbs--check-keys message json '(component detail))
    (list :component (agent-repl-wire-decode-deploy-component message 'component json)
          :detail (agent-repl-wire-verbs--decode-required-string message 'detail json))))

(defun agent-repl-wire-decode-daemon-fault-deploy-failed-rollback (json)
  "Decode DaemonFaultDeployFailed's `rollback' step arm from JSON as a
`DeployRollbackFailed'."
  (agent-repl-wire-decode-deploy-rollback-failed json))

(defun agent-repl-wire-decode-daemon-fault-deploy-failed (json)
  "Decode DaemonFaultDeployFailed from JSON into (:step (:arm ARM :value V)).
THE ARM IS THE STEP THAT FAILED, carrying the very refusal the Deploy rpc
answered its caller with, or the failed rollback, so an unset step is a
contract breach."
  (let ((message "DaemonFaultDeployFailed"))
    (agent-repl-wire-verbs--check-keys message json '(build install restartServices rollback))
    (list :step
          (agent-repl-wire-verbs--decode-oneof
           message "step" json
           (list (list 'build :build
                       #'agent-repl-wire-decode-daemon-fault-deploy-failed-build)
                 (list 'install :install
                       #'agent-repl-wire-decode-daemon-fault-deploy-failed-install)
                 (list 'restartServices :restart-services
                       #'agent-repl-wire-decode-daemon-fault-deploy-failed-restart-services)
                 (list 'rollback :rollback
                       #'agent-repl-wire-decode-daemon-fault-deploy-failed-rollback))))))

(defun agent-repl-wire-decode-daemon-fault-kind-adoption-window-expired (json)
  "Decode DaemonFault's `adoption_window_expired' kind arm from JSON as a
`DaemonFaultAdoptionWindowExpired'."
  (agent-repl-wire-decode-daemon-fault-adoption-window-expired json))

(defun agent-repl-wire-decode-daemon-fault-kind-log-sink-poisoned (json)
  "Decode DaemonFault's `log_sink_poisoned' kind arm from JSON as a
`DaemonFaultLogSinkPoisoned'."
  (agent-repl-wire-decode-daemon-fault-log-sink-poisoned json))

(defun agent-repl-wire-decode-daemon-fault-kind-successor-spawn-failed (json)
  "Decode DaemonFault's `successor_spawn_failed' kind arm from JSON as a
`DaemonFaultSuccessorSpawnFailed'."
  (agent-repl-wire-decode-daemon-fault-successor-spawn-failed json))

(defun agent-repl-wire-decode-daemon-fault-kind-prompts-dir-missing (json)
  "Decode DaemonFault's `prompts_dir_missing' kind arm from JSON as a
`DaemonFaultPromptsDirMissing'."
  (agent-repl-wire-decode-daemon-fault-prompts-dir-missing json))

(defun agent-repl-wire-decode-daemon-fault-kind-wsm-read-only (json)
  "Decode DaemonFault's `wsm_read_only' kind arm from JSON as a
`DaemonFaultWsmReadOnly'."
  (agent-repl-wire-decode-daemon-fault-wsm-read-only json))

(defun agent-repl-wire-decode-daemon-fault-kind-daemon-state-unreadable (json)
  "Decode DaemonFault's `daemon_state_unreadable' kind arm from JSON as a
`DaemonFaultDaemonStateUnreadable'."
  (agent-repl-wire-decode-daemon-fault-daemon-state-unreadable json))

(defun agent-repl-wire-decode-daemon-fault-kind-deploy-failed (json)
  "Decode DaemonFault's `deploy_failed' kind arm from JSON as a
`DaemonFaultDeployFailed'."
  (agent-repl-wire-decode-daemon-fault-deploy-failed json))

(defun agent-repl-wire-decode-daemon-fault-kind (json)
  "Decode DaemonFault's `kind' oneof from JSON into (:arm ARM :value V).
THE KIND IS A TYPED ARM: `detail' carries only what prose must, so a
fault with no kind is a contract breach."
  (agent-repl-wire-verbs--decode-oneof
   "DaemonFault" "kind" json
   (list (list 'adoptionWindowExpired :adoption-window-expired #'agent-repl-wire-decode-daemon-fault-kind-adoption-window-expired)
                 (list 'logSinkPoisoned :log-sink-poisoned #'agent-repl-wire-decode-daemon-fault-kind-log-sink-poisoned)
                 (list 'successorSpawnFailed :successor-spawn-failed #'agent-repl-wire-decode-daemon-fault-kind-successor-spawn-failed)
                 (list 'promptsDirMissing :prompts-dir-missing #'agent-repl-wire-decode-daemon-fault-kind-prompts-dir-missing)
                 (list 'wsmReadOnly :wsm-read-only #'agent-repl-wire-decode-daemon-fault-kind-wsm-read-only)
                 (list 'daemonStateUnreadable :daemon-state-unreadable #'agent-repl-wire-decode-daemon-fault-kind-daemon-state-unreadable)
                 (list 'deployFailed :deploy-failed #'agent-repl-wire-decode-daemon-fault-kind-deploy-failed))))

(defun agent-repl-wire-decode-daemon-fault (json)
  "Decode DaemonFault from JSON into (:detail STRING :kind ONEOF)."
  (let ((message "DaemonFault"))
    (agent-repl-wire-verbs--check-keys message json '(detail adoptionWindowExpired logSinkPoisoned successorSpawnFailed promptsDirMissing wsmReadOnly daemonStateUnreadable deployFailed))
    (list :detail (agent-repl-wire-verbs--decode-string message 'detail json)
          :kind (agent-repl-wire-decode-daemon-fault-kind json))))

(defun agent-repl-wire-decode-daemon-unhealthy-faults (json)
  "Decode one element of DaemonUnhealthy's repeated `faults' use site from
JSON."
  (agent-repl-wire-decode-daemon-fault json))

(defun agent-repl-wire-decode-daemon-unhealthy (json)
  "Decode DaemonUnhealthy from JSON into (:faults LIST)."
  (let ((message "DaemonUnhealthy"))
    (agent-repl-wire-verbs--check-keys message json '(faults))
    (list :faults (agent-repl-wire-verbs--decode-repeated
                   message 'faults json
                   #'agent-repl-wire-decode-daemon-unhealthy-faults))))

(defun agent-repl-wire-decode-daemon-healthy (json)
  "Decode DaemonHealthy from JSON.  Empty: the arm is the whole verdict."
  (agent-repl-wire-verbs--decode-empty "DaemonHealthy" json))

(defun agent-repl-wire-decode-daemon-identity (json)
  "Decode DaemonIdentity into its immutable process and build fields."
  (let ((message "DaemonIdentity"))
    (agent-repl-wire-verbs--check-keys message json '(instanceId pid buildSha))
    (list :instance-id (agent-repl-wire-verbs--decode-string message 'instanceId json)
          :pid (agent-repl-wire--decode-int64 message 'pid json)
          :build-sha (agent-repl-wire-verbs--decode-string message 'buildSha json))))

(defun agent-repl-wire-decode-daemon-health-success-healthy (json)
  "Decode DaemonHealthSuccess's `healthy' verdict arm from JSON."
  (agent-repl-wire-decode-daemon-healthy json))

(defun agent-repl-wire-decode-daemon-health-success-unhealthy (json)
  "Decode DaemonHealthSuccess's `unhealthy' verdict arm from JSON."
  (agent-repl-wire-decode-daemon-unhealthy json))

(defun agent-repl-wire-decode-daemon-health-success (json)
  "Decode DaemonHealthSuccess with its verdict and process identity.
UNHEALTHY IS AN ANSWER: it arrives inside success, never as an error.
An absent identity decodes as nil for the one-version transition from a
daemon built before `agentrepl.v1.DaemonIdentity' existed; restart
coordination refuses to declare a replacement until the new daemon states
one."
  (let ((message "DaemonHealthSuccess"))
    (agent-repl-wire-verbs--check-keys message json '(healthy unhealthy identity))
    (let ((verdict
           (agent-repl-wire-verbs--decode-oneof
            message "health" json
            (list (list 'healthy :healthy #'agent-repl-wire-decode-daemon-health-success-healthy)
                  (list 'unhealthy :unhealthy
                        #'agent-repl-wire-decode-daemon-health-success-unhealthy))))
          (identity (alist-get 'identity json)))
      (append verdict
              (list :identity
                    (and identity
                         (agent-repl-wire-decode-daemon-identity identity)))))))

(defun agent-repl-wire-decode-daemon-health-error (json)
  "Decode DaemonHealthError from JSON.
Empty until its arms are derived; error means the question could not be
ANSWERED at all."
  (agent-repl-wire-verbs--decode-empty "DaemonHealthError" json))

(defun agent-repl-wire-decode-daemon-health-response-success (json)
  "Decode DaemonHealthResponse's `success' arm from JSON."
  (agent-repl-wire-decode-daemon-health-success json))

(defun agent-repl-wire-decode-daemon-health-response-error (json)
  "Decode DaemonHealthResponse's `error' arm from JSON."
  (agent-repl-wire-decode-daemon-health-error json))

(defun agent-repl-wire-decode-daemon-health-response (json)
  "Decode DaemonHealthResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-result
   "DaemonHealthResponse" json
   #'agent-repl-wire-decode-daemon-health-response-success
   #'agent-repl-wire-decode-daemon-health-response-error))


;;;; ---- SessionHealth --------------------------------------------------

(defun agent-repl-wire-encode-session-health-request-workspace (ref)
  "Encode SessionHealthRequest's `workspace' use site from REF."
  (agent-repl-wire-encode-workspace-ref ref))

(defun agent-repl-wire-encode-session-health-request (request)
  "Encode SessionHealthRequest from plist REQUEST (:workspace REF)."
  (agent-repl--log '(:agent-repl-context "a codec call outside a request has no workspace") "elisp.wire.verbs-encode-session-health-request")
  (list (cons 'workspace
              (agent-repl-wire-encode-session-health-request-workspace
               (agent-repl-wire-verbs--require "SessionHealthRequest" "workspace"
                                                (plist-get request :workspace))))))

(defun agent-repl-wire-decode-session-fault-kind-shim-start-failed (json)
  "Decode SessionFault's `shim_start_failed' kind arm from JSON as a
`SessionFaultShimStartFailed'."
  (agent-repl-wire-decode-session-fault-shim-start-failed json))

(defun agent-repl-wire-decode-session-fault-kind-shim-died (json)
  "Decode SessionFault's `shim_died' kind arm from JSON as a
`SessionFaultShimDied'."
  (agent-repl-wire-decode-session-fault-shim-died json))

(defun agent-repl-wire-decode-session-fault-kind-link-severed (json)
  "Decode SessionFault's `link_severed' kind arm from JSON as a
`SessionFaultLinkSevered'."
  (agent-repl-wire-decode-session-fault-link-severed json))

(defun agent-repl-wire-decode-session-fault-kind-resume-failed (json)
  "Decode SessionFault's `resume_failed' kind arm from JSON as a
`SessionFaultResumeFailed'."
  (agent-repl-wire-decode-session-fault-resume-failed json))

(defun agent-repl-wire-decode-session-fault-kind-bounce-died (json)
  "Decode SessionFault's `bounce_died' kind arm from JSON as a
`SessionFaultBounceDied'."
  (agent-repl-wire-decode-session-fault-bounce-died json))

(defun agent-repl-wire-decode-session-fault-kind-bounce-unknown (json)
  "Decode SessionFault's `bounce_unknown' kind arm from JSON as a
`SessionFaultBounceUnknown'."
  (agent-repl-wire-decode-session-fault-bounce-unknown json))

(defun agent-repl-wire-decode-session-fault-kind-classifier-failed (json)
  "Decode SessionFault's `classifier_failed' kind arm from JSON as a
`SessionFaultClassifierFailed'."
  (agent-repl-wire-decode-session-fault-classifier-failed json))

(defun agent-repl-wire-decode-session-fault-kind-shim-reported (json)
  "Decode SessionFault's `shim_reported' kind arm from JSON as a
`SessionFaultShimReported'."
  (agent-repl-wire-decode-session-fault-shim-reported json))

(defun agent-repl-wire-decode-session-fault-kind-conversation-abandoned (json)
  "Decode SessionFault's `conversation_abandoned' kind arm from JSON as a
`SessionFaultConversationAbandoned'."
  (agent-repl-wire-decode-session-fault-conversation-abandoned json))

(defun agent-repl-wire-decode-session-fault-kind-session-absent (json)
  "Decode SessionFault's `session_absent' kind arm from JSON as a
`SessionFaultSessionAbsent'."
  (agent-repl-wire-decode-session-fault-session-absent json))

(defun agent-repl-wire-decode-session-fault-kind-watch-open-refused (json)
  "Decode SessionFault's `watch_open_refused' kind arm from JSON as a
`SessionFaultWatchOpenRefused'."
  (agent-repl-wire-decode-session-fault-watch-open-refused json))

(defun agent-repl-wire-decode-session-fault-kind-daemon-state-unreadable (json)
  "Decode SessionFault's `daemon_state_unreadable' kind arm from JSON as a
`SessionFaultDaemonStateUnreadable'."
  (agent-repl-wire-decode-session-fault-daemon-state-unreadable json))

(defun agent-repl-wire-decode-session-fault-kind-adoption-window-expired (json)
  "Decode SessionFault's `adoption_window_expired' kind arm from JSON as a
`SessionFaultAdoptionWindowExpired'."
  (agent-repl-wire-decode-session-fault-adoption-window-expired json))

(defun agent-repl-wire-decode-session-fault-kind-final-answer-unresolved (json)
  "Decode SessionFault's `final_answer_unresolved' kind arm from JSON as a
`SessionFaultFinalAnswerUnresolved'."
  (agent-repl-wire-decode-session-fault-final-answer-unresolved json))

(defun agent-repl-wire-decode-session-fault-kind (json)
  "Decode SessionFault's `kind' oneof from JSON into (:arm ARM :value V).
THE ARM IS THE FAULT CLASS: `detail' supplements it and never replaces
it, so a fault with no kind is a contract breach."
  (agent-repl-wire-verbs--decode-oneof
   "SessionFault" "kind" json
   (list (list 'shimStartFailed :shim-start-failed #'agent-repl-wire-decode-session-fault-kind-shim-start-failed)
                 (list 'shimDied :shim-died #'agent-repl-wire-decode-session-fault-kind-shim-died)
                 (list 'linkSevered :link-severed #'agent-repl-wire-decode-session-fault-kind-link-severed)
                 (list 'resumeFailed :resume-failed #'agent-repl-wire-decode-session-fault-kind-resume-failed)
                 (list 'bounceDied :bounce-died #'agent-repl-wire-decode-session-fault-kind-bounce-died)
                 (list 'bounceUnknown :bounce-unknown #'agent-repl-wire-decode-session-fault-kind-bounce-unknown)
                 (list 'classifierFailed :classifier-failed #'agent-repl-wire-decode-session-fault-kind-classifier-failed)
                 (list 'shimReported :shim-reported #'agent-repl-wire-decode-session-fault-kind-shim-reported)
                 (list 'conversationAbandoned :conversation-abandoned #'agent-repl-wire-decode-session-fault-kind-conversation-abandoned)
                 (list 'sessionAbsent :session-absent #'agent-repl-wire-decode-session-fault-kind-session-absent)
                 (list 'watchOpenRefused :watch-open-refused #'agent-repl-wire-decode-session-fault-kind-watch-open-refused)
                 (list 'daemonStateUnreadable :daemon-state-unreadable #'agent-repl-wire-decode-session-fault-kind-daemon-state-unreadable)
                 (list 'adoptionWindowExpired :adoption-window-expired #'agent-repl-wire-decode-session-fault-kind-adoption-window-expired)
                 (list 'finalAnswerUnresolved :final-answer-unresolved #'agent-repl-wire-decode-session-fault-kind-final-answer-unresolved))))

(defun agent-repl-wire-decode-session-fault (json)
  "Decode SessionFault from JSON into (:detail STRING :kind ONEOF).
Deliberately NOT DaemonFault: a session's fault classes are the session
controller's own vocabulary — the same fourteen the host stream's HostFault
carries, decoded through the same shared arm messages."
  (let ((message "SessionFault"))
    (agent-repl-wire-verbs--check-keys message json '(detail shimStartFailed shimDied linkSevered resumeFailed bounceDied bounceUnknown classifierFailed shimReported conversationAbandoned sessionAbsent watchOpenRefused daemonStateUnreadable adoptionWindowExpired finalAnswerUnresolved))
    (list :detail (agent-repl-wire-verbs--decode-string message 'detail json)
          :kind (agent-repl-wire-decode-session-fault-kind json))))

(defun agent-repl-wire-decode-session-unhealthy-faults (json)
  "Decode one element of SessionUnhealthy's repeated `faults' use site from
JSON."
  (agent-repl-wire-decode-session-fault json))

(defun agent-repl-wire-decode-session-unhealthy (json)
  "Decode SessionUnhealthy from JSON into (:faults LIST)."
  (let ((message "SessionUnhealthy"))
    (agent-repl-wire-verbs--check-keys message json '(faults))
    (list :faults (agent-repl-wire-verbs--decode-repeated
                   message 'faults json
                   #'agent-repl-wire-decode-session-unhealthy-faults))))

(defun agent-repl-wire-decode-session-healthy (json)
  "Decode SessionHealthy from JSON.  Empty: the arm is the whole verdict."
  (agent-repl-wire-verbs--decode-empty "SessionHealthy" json))

(defun agent-repl-wire-decode-session-health-success-healthy (json)
  "Decode SessionHealthSuccess's `healthy' verdict arm from JSON."
  (agent-repl-wire-decode-session-healthy json))

(defun agent-repl-wire-decode-session-health-success-unhealthy (json)
  "Decode SessionHealthSuccess's `unhealthy' verdict arm from JSON."
  (agent-repl-wire-decode-session-unhealthy json))

(defun agent-repl-wire-decode-session-health-success (json)
  "Decode SessionHealthSuccess from JSON into (:arm ARM :value V).
UNHEALTHY IS AN ANSWER: it arrives inside success, never as an error."
  (let ((message "SessionHealthSuccess"))
    (agent-repl-wire-verbs--check-keys message json '(healthy unhealthy))
    (agent-repl-wire-verbs--decode-oneof
     message "health" json
     (list (list 'healthy :healthy #'agent-repl-wire-decode-session-health-success-healthy)
           (list 'unhealthy :unhealthy
                 #'agent-repl-wire-decode-session-health-success-unhealthy)))))

(defun agent-repl-wire-decode-session-health-unknown-workspace (json)
  "Decode SessionHealthUnknownWorkspace from JSON.  Empty: The workspace id is
not in the daemon's registry."
  (agent-repl-wire-verbs--decode-empty "SessionHealthUnknownWorkspace" json))

(defun agent-repl-wire-decode-session-health-workspace-ref-mismatch (json)
  "Decode SessionHealthWorkspaceRefMismatch from JSON into a plist (`:registry-
dir').
The echoed dir disagrees with the registry's dir for this id."
  (let ((message "SessionHealthWorkspaceRefMismatch"))
    (agent-repl-wire-verbs--check-keys message json '(registryDir))
    (list :registry-dir (agent-repl-wire-verbs--decode-string
                       message 'registryDir json))))

(defun agent-repl-wire-decode-session-health-transferring-away (json)
  "Decode SessionHealthTransferringAway from JSON into a plist (`:address').
This daemon released the workspace to a successor; dial `address'."
  (let ((message "SessionHealthTransferringAway"))
    (agent-repl-wire-verbs--check-keys message json '(address))
    (list :address (agent-repl-wire-verbs--decode-string
                       message 'address json))))

(defun agent-repl-wire-decode-session-health-not-yet-adopted (json)
  "Decode SessionHealthNotYetAdopted from JSON.  Empty: A joining daemon has
not finished adopting this workspace yet."
  (agent-repl-wire-verbs--decode-empty "SessionHealthNotYetAdopted" json))

(defun agent-repl-wire-decode-session-health-error-unknown-workspace (json)
  "Decode SessionHealthError's `unknown_workspace' cause arm from JSON."
  (agent-repl-wire-decode-session-health-unknown-workspace json))

(defun agent-repl-wire-decode-session-health-error-workspace-ref-mismatch (json)
  "Decode SessionHealthError's `workspace_ref_mismatch' cause arm from JSON."
  (agent-repl-wire-decode-session-health-workspace-ref-mismatch json))

(defun agent-repl-wire-decode-session-health-error-transferring-away (json)
  "Decode SessionHealthError's `transferring_away' cause arm from JSON."
  (agent-repl-wire-decode-session-health-transferring-away json))

(defun agent-repl-wire-decode-session-health-error-not-yet-adopted (json)
  "Decode SessionHealthError's `not_yet_adopted' cause arm from JSON."
  (agent-repl-wire-decode-session-health-not-yet-adopted json))

(defun agent-repl-wire-decode-session-health-error (json)
  "Decode SessionHealthError from JSON into (:cause (:arm ARM :value V)).
THE ARM IS THE REFUSAL, so an unset cause is a contract breach and an
arm this codec does not know is refused as an unknown field."
  (let ((message "SessionHealthError"))
    (agent-repl-wire-verbs--check-keys message json '(unknownWorkspace workspaceRefMismatch transferringAway notYetAdopted))
    (list :cause
          (agent-repl-wire-verbs--decode-oneof
           message "cause" json
           (list (list 'unknownWorkspace :unknown-workspace #'agent-repl-wire-decode-session-health-error-unknown-workspace)
         (list 'workspaceRefMismatch :workspace-ref-mismatch #'agent-repl-wire-decode-session-health-error-workspace-ref-mismatch)
         (list 'transferringAway :transferring-away #'agent-repl-wire-decode-session-health-error-transferring-away)
         (list 'notYetAdopted :not-yet-adopted #'agent-repl-wire-decode-session-health-error-not-yet-adopted))))))

(defun agent-repl-wire-decode-session-health-response-success (json)
  "Decode SessionHealthResponse's `success' arm from JSON."
  (agent-repl-wire-decode-session-health-success json))

(defun agent-repl-wire-decode-session-health-response-error (json)
  "Decode SessionHealthResponse's `error' arm from JSON."
  (agent-repl-wire-decode-session-health-error json))

(defun agent-repl-wire-decode-session-health-response (json)
  "Decode SessionHealthResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-result
   "SessionHealthResponse" json
   #'agent-repl-wire-decode-session-health-response-success
   #'agent-repl-wire-decode-session-health-response-error))


;;;; ---- Interrupt ------------------------------------------------------
;;
;; Interrupt is a FEED verb whose ARM IS THE TARGET.  Emacs authors only the
;; two targets it can name from what it holds: `turn' (the running vendor
;; query) and `all_agents' (the fan-wide stop).  The `detached' target names
;; a bubble by `frontend.v1.FeedId', vocabulary Emacs has no feed to build,
;; so no encoder is offered for it and an attempt to send it is refused as an
;; unknown oneof arm rather than an ill-formed FeedId reaching the wire.

(defun agent-repl-wire-encode-interrupt-turn (_present)
  "Encode InterruptTurn.  Empty: the arm's presence IS the target."
  nil)

(defun agent-repl-wire-encode-interrupt-all-agents (_present)
  "Encode InterruptAllAgents.  Empty: the arm's presence IS the target."
  nil)

(defun agent-repl-wire-encode-interrupt-request-workspace (ref)
  "Encode InterruptRequest's `workspace' use site from REF."
  (agent-repl-wire-encode-workspace-ref ref))

(defun agent-repl-wire-encode-interrupt-request-target (value)
  "Encode InterruptRequest's `target' oneof from VALUE.
VALUE is (:arm KEYWORD :value V).  Only `turn' and `all-agents' can be
authored here; `detached' is refused as an unknown arm because Emacs holds
no feed from which to build its FeedId."
  (agent-repl-wire-verbs--encode-oneof
   "InterruptRequest" "target" value
   (list (list :turn 'turn #'agent-repl-wire-encode-interrupt-turn)
         (list :all-agents 'allAgents #'agent-repl-wire-encode-interrupt-all-agents))))

(defun agent-repl-wire-encode-interrupt-request (request)
  "Encode InterruptRequest from plist REQUEST.
REQUEST is (:workspace REF :target (:arm KEYWORD :value V) :confirm-agents
BOOL).  `confirm_agents' is spelled EXPLICITLY even when false, mirroring
`force' on RestartWorkspace and the footer: the challenge answer is a fact
the request states, never an omitted default."
  (agent-repl--log '(:agent-repl-context "a codec call outside a request has no workspace") "elisp.wire.verbs-encode-interrupt-request confirm=%S"
                    (and (plist-get request :confirm-agents) t))
  (list (cons 'workspace
              (agent-repl-wire-encode-interrupt-request-workspace
               (agent-repl-wire-verbs--require "InterruptRequest" "workspace"
                                                (plist-get request :workspace))))
        (agent-repl-wire-encode-interrupt-request-target
         (agent-repl-wire-verbs--require "InterruptRequest" "target"
                                          (plist-get request :target)))
        (cons 'confirmAgents
              (agent-repl-wire-verbs--encode-bool (plist-get request :confirm-agents)))))

(defun agent-repl-wire-decode-interrupt-interrupted-turn (json)
  "Decode InterruptedTurn from JSON.  Empty: the turn was interrupted."
  (agent-repl-wire-verbs--decode-empty "InterruptedTurn" json))

(defun agent-repl-wire-decode-interrupt-interrupted-detached (json)
  "Decode InterruptedDetached from JSON into a plist (`:count').
COUNT is how many detached agents the stop reached."
  (let ((message "InterruptedDetached"))
    (agent-repl-wire-verbs--check-keys message json '(count))
    (list :count (agent-repl-wire--decode-int64 message 'count json))))

(defun agent-repl-wire-decode-interrupt-nothing-running (json)
  "Decode InterruptNothingRunning from JSON.  Empty: nothing was running -- an
ANSWER (a stop that found the session already quiet), never a failure."
  (agent-repl-wire-verbs--decode-empty "InterruptNothingRunning" json))

(defun agent-repl-wire-decode-interrupt-success (json)
  "Decode InterruptSuccess from JSON into (:arm ARM :value V).
THE ARM IS WHAT THE STOP DID, so each outcome is its own arm; `nothing_running'
is one of them, not a failure smuggled into success."
  (let ((message "InterruptSuccess"))
    (agent-repl-wire-verbs--check-keys
     message json '(interruptedTurn interruptedDetached nothingRunning))
    (agent-repl-wire-verbs--decode-oneof
     message "outcome" json
     (list (list 'interruptedTurn :interrupted-turn
                 #'agent-repl-wire-decode-interrupt-interrupted-turn)
           (list 'interruptedDetached :interrupted-detached
                 #'agent-repl-wire-decode-interrupt-interrupted-detached)
           (list 'nothingRunning :nothing-running
                 #'agent-repl-wire-decode-interrupt-nothing-running)))))

(defun agent-repl-wire-decode-interrupt-confirm-required (json)
  "Decode InterruptConfirmRequired from JSON into a plist (`:live-agent-count').
LIVE-AGENT-COUNT is how many live detached agents a turn stop would also end."
  (let ((message "InterruptConfirmRequired"))
    (agent-repl-wire-verbs--check-keys message json '(liveAgentCount))
    (list :live-agent-count
          (agent-repl-wire--decode-int64 message 'liveAgentCount json))))

(defun agent-repl-wire-decode-interrupt-unknown-workspace (json)
  "Decode InterruptUnknownWorkspace from JSON.  Empty: the workspace id is not
in the daemon's registry."
  (agent-repl-wire-verbs--decode-empty "InterruptUnknownWorkspace" json))

(defun agent-repl-wire-decode-interrupt-workspace-ref-mismatch (json)
  "Decode InterruptWorkspaceRefMismatch from JSON into a plist (`:registry-dir').
The echoed dir disagrees with the registry's dir for this id."
  (let ((message "InterruptWorkspaceRefMismatch"))
    (agent-repl-wire-verbs--check-keys message json '(registryDir))
    (list :registry-dir
          (agent-repl-wire-verbs--decode-string message 'registryDir json))))

(defun agent-repl-wire-decode-interrupt-transferring-away (json)
  "Decode InterruptTransferringAway from JSON into a plist (`:address').
This daemon released the workspace to a successor; dial `address'."
  (let ((message "InterruptTransferringAway"))
    (agent-repl-wire-verbs--check-keys message json '(address))
    (list :address
          (agent-repl-wire-verbs--decode-string message 'address json))))

(defun agent-repl-wire-decode-interrupt-not-yet-adopted (json)
  "Decode InterruptNotYetAdopted from JSON.  Empty: a joining daemon has not
finished adopting this workspace yet."
  (agent-repl-wire-verbs--decode-empty "InterruptNotYetAdopted" json))

(defun agent-repl-wire-decode-interrupt-not-detached-work (json)
  "Decode InterruptNotDetachedWork from JSON.  Empty: the FeedId names no
detached item."
  (agent-repl-wire-verbs--decode-empty "InterruptNotDetachedWork" json))

(defun agent-repl-wire-decode-interrupt-no-session (json)
  "Decode InterruptNoSession from JSON.  Empty: the workspace has no session to
interrupt."
  (agent-repl-wire-verbs--decode-empty "InterruptNoSession" json))

(defun agent-repl-wire-decode-interrupt-shim-refused (json)
  "Decode InterruptShimRefused from JSON into a plist (`:detail').
DETAIL is the shim's own account of the refusal."
  (let ((message "InterruptShimRefused"))
    (agent-repl-wire-verbs--check-keys message json '(detail))
    (list :detail
          (agent-repl-wire-verbs--decode-string message 'detail json))))

(defun agent-repl-wire-decode-interrupt-error (json)
  "Decode InterruptError from JSON into (:cause (:arm ARM :value V)).
THE ARM IS THE REFUSAL, so an unset kind is a contract breach and an arm
this codec does not know is refused as an unknown field.  The oneof is
spelled `:cause' so `verbs.el's' arm-generic refusal handling reads it
with the same accessor every other verb's error uses."
  (let ((message "InterruptError"))
    (agent-repl-wire-verbs--check-keys
     message json '(confirmRequired unknownWorkspace workspaceRefMismatch
                    transferringAway notYetAdopted notDetachedWork noSession
                    shimRefused))
    (list :cause
          (agent-repl-wire-verbs--decode-oneof
           message "kind" json
           (list (list 'confirmRequired :confirm-required
                       #'agent-repl-wire-decode-interrupt-confirm-required)
                 (list 'unknownWorkspace :unknown-workspace
                       #'agent-repl-wire-decode-interrupt-unknown-workspace)
                 (list 'workspaceRefMismatch :workspace-ref-mismatch
                       #'agent-repl-wire-decode-interrupt-workspace-ref-mismatch)
                 (list 'transferringAway :transferring-away
                       #'agent-repl-wire-decode-interrupt-transferring-away)
                 (list 'notYetAdopted :not-yet-adopted
                       #'agent-repl-wire-decode-interrupt-not-yet-adopted)
                 (list 'notDetachedWork :not-detached-work
                       #'agent-repl-wire-decode-interrupt-not-detached-work)
                 (list 'noSession :no-session
                       #'agent-repl-wire-decode-interrupt-no-session)
                 (list 'shimRefused :shim-refused
                       #'agent-repl-wire-decode-interrupt-shim-refused))))))

(defun agent-repl-wire-decode-interrupt-response (json)
  "Decode InterruptResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-result
   "InterruptResponse" json
   #'agent-repl-wire-decode-interrupt-success
   #'agent-repl-wire-decode-interrupt-error))

;;;; ---- SelectFeedRow --------------------------------------------------
;;
;; Moving the feed's selection.  THE ARM IS THE MOVE on the request: Emacs
;; steps through the final responses (`C-p' / `C-n') or through the prompts a
;; rollback can reach (`C-S-p' / `C-S-n'), or clears the selection (escape
;; twice).  The daemon owns the ordered rows and the selection, computes where
;; a step lands, pushes the result to the webapp and to Emacs's host watch,
;; and acks it here.  THE ARM IS THE OUTCOME on the success.  See
;; endpoint_select_feed_row.proto.  The webapp's own `left_view' move is
;; never sent from Emacs, so it has no elisp spelling.

(defconst agent-repl-wire-select-feed-row-directions
  '((:older . "SELECT_FEED_ROW_DIRECTION_OLDER")
    (:newer . "SELECT_FEED_ROW_DIRECTION_NEWER"))
  "The SelectFeedRowDirection vocabulary Emacs sends, keyword to wire name.
UNSPECIFIED is deliberately ABSENT: a request carrying it is refused as
InvalidArgument, so the zero value has no elisp spelling to reach for by
accident.")

(defun agent-repl-wire-encode-select-feed-row-direction (value)
  "Encode the SelectFeedRowDirection keyword VALUE as its protojson enum name.
The vocabulary is closed; an unknown keyword — `:unspecified' in
particular — is refused before the request is built."
  (let ((name (cdr (assq value agent-repl-wire-select-feed-row-directions))))
    (unless name
      (agent-repl-wire-verbs--fail "SelectFeedRowStep" "direction" "unknown direction"))
    name))

(defun agent-repl-wire-encode-select-feed-row-step (value)
  "Encode SelectFeedRowStep from plist VALUE (:direction K).
The direction is REQUIRED: a step names which way it moves."
  (list (cons 'direction
              (agent-repl-wire-encode-select-feed-row-direction
               (agent-repl-wire-verbs--require "SelectFeedRowStep" "direction"
                                                (plist-get value :direction))))))

(defun agent-repl-wire-encode-select-feed-row-clear (_value)
  "Encode the empty SelectFeedRowClear.  Presence IS the dismissal."
  nil)

(defun agent-repl-wire-encode-select-feed-row-request-workspace (ref)
  "Encode SelectFeedRowRequest's `workspace' use site from REF."
  (agent-repl-wire-encode-workspace-ref ref))

(defun agent-repl-wire-encode-select-feed-row-request-move (value)
  "Encode SelectFeedRowRequest's `move' oneof from VALUE.
VALUE is (:arm KEYWORD :value V), KEYWORD one of `:response' and `:prompt'
\(V a step plist) and `:clear' (V nil)."
  (agent-repl-wire-verbs--encode-oneof
   "SelectFeedRowRequest" "move" value
   (list (list :response 'response #'agent-repl-wire-encode-select-feed-row-step)
         (list :prompt 'prompt #'agent-repl-wire-encode-select-feed-row-step)
         (list :clear 'clear #'agent-repl-wire-encode-select-feed-row-clear))))

(defun agent-repl-wire-encode-select-feed-row-request (request)
  "Encode SelectFeedRowRequest from plist REQUEST (:workspace REF :move MOVE).
Both are required: the workspace is the daemon-minted echo token naming
WHICH workspace's selection to change, and the move says what to do to
it.  An incomplete request errors here rather than reaching the wire."
  (let ((message "SelectFeedRowRequest"))
    (list (cons 'workspace
                (agent-repl-wire-encode-select-feed-row-request-workspace
                 (agent-repl-wire-verbs--require message "workspace"
                                                  (plist-get request :workspace))))
          (agent-repl-wire-encode-select-feed-row-request-move
           (agent-repl-wire-verbs--require message "move" (plist-get request :move))))))

(defun agent-repl-wire-decode-feed-selection-row (message json)
  "Decode the selected-row MESSAGE from JSON into (:row FEEDID).
MESSAGE is `FeedSelectionResponse', `FeedSelectionPrompt' or
`FeedSelectionBubble'.  The row is REQUIRED: a selection that names no
row is a contract breach."
  (agent-repl-wire-verbs--check-keys message json '(row))
  (list :row (agent-repl-wire-verbs--decode-required-message
              message 'row json #'agent-repl-wire-decode-feed-id)))

(defun agent-repl-wire-decode-feed-selection-none (json)
  "Decode FeedSelectionNone from JSON into (:viewport (:arm ARM :value nil)).
ARM is `:return-to-tail' or `:stay'; exactly one is set."
  (let ((message "FeedSelectionNone"))
    (agent-repl-wire-verbs--check-keys message json '(returnToTail stay))
    (list :viewport
          (agent-repl-wire-verbs--decode-oneof
           message "viewport" json
           (list (list 'returnToTail :return-to-tail
                       (lambda (v) (agent-repl-wire-verbs--decode-empty
                                    "FeedSelectionNoneReturnToTail" v)))
                 (list 'stay :stay
                       (lambda (v) (agent-repl-wire-verbs--decode-empty
                                    "FeedSelectionNoneStay" v))))))))

(defun agent-repl-wire-decode-feed-selection (json)
  "Decode frontend.v1.FeedSelection from JSON into (:arm ARM :value V).
ARM is `:none' (V the none plist), `:response', `:prompt' or `:bubble'
\(V (:row ID))."
  (let ((message "FeedSelection"))
    (agent-repl-wire-verbs--check-keys message json '(none response prompt bubble))
    (agent-repl-wire-verbs--decode-oneof
     message "selection" json
     (list (list 'none :none #'agent-repl-wire-decode-feed-selection-none)
           (list 'response :response
                 (lambda (v) (agent-repl-wire-decode-feed-selection-row
                              "FeedSelectionResponse" v)))
           (list 'prompt :prompt
                 (lambda (v) (agent-repl-wire-decode-feed-selection-row
                              "FeedSelectionPrompt" v)))
           (list 'bubble :bubble
                 (lambda (v) (agent-repl-wire-decode-feed-selection-row
                              "FeedSelectionBubble" v)))))))

(defun agent-repl-wire-decode-select-feed-row-success-selected (json)
  "Decode SelectFeedRowSuccessSelected from JSON into (:selection SEL).
The selection is REQUIRED: a selected outcome names what is selected."
  (let ((message "SelectFeedRowSuccessSelected"))
    (agent-repl-wire-verbs--check-keys message json '(selection))
    (list :selection (agent-repl-wire-verbs--decode-required-message
                      message 'selection json #'agent-repl-wire-decode-feed-selection))))

(defun agent-repl-wire-decode-select-feed-row-success (json)
  "Decode SelectFeedRowSuccess from JSON into (:outcome (:arm ARM :value V)).
ARM is `:selected' (V (:selection SEL)), `:nothing-selectable' or `:none'
\(V nil).  THE ARM IS THE OUTCOME, so an unset outcome is a contract breach."
  (let ((message "SelectFeedRowSuccess"))
    (agent-repl-wire-verbs--check-keys message json '(selected nothingSelectable none))
    (list :outcome
          (agent-repl-wire-verbs--decode-oneof
           message "outcome" json
           (list (list 'selected :selected
                       #'agent-repl-wire-decode-select-feed-row-success-selected)
                 (list 'nothingSelectable :nothing-selectable
                       (lambda (v) (agent-repl-wire-verbs--decode-empty
                                    "SelectFeedRowSuccessNothingSelectable" v)))
                 (list 'none :none
                       (lambda (v) (agent-repl-wire-verbs--decode-empty
                                    "SelectFeedRowSuccessNone" v))))))))

(defun agent-repl-wire-decode-select-feed-row-error (json)
  "Decode SelectFeedRowError from JSON into (:cause (:arm ARM :value V)).
THE ARM IS THE REFUSAL, so an unset cause is a contract breach and an arm
this codec does not know is refused as an unknown field."
  (let ((message "SelectFeedRowError"))
    (agent-repl-wire-verbs--check-keys
     message json '(unknownWorkspace workspaceRefMismatch transferringAway notYetAdopted
                    notSelectable))
    (list :cause
          (agent-repl-wire-verbs--decode-oneof
           message "cause" json
           (append (agent-repl-wire-verbs--handover-arms "SelectFeedRow")
                   (list (list 'notSelectable :not-selectable
                               #'agent-repl-wire-decode-select-feed-row-not-selectable)))))))

(defun agent-repl-wire-decode-select-feed-row-not-selectable (json)
  "Decode SelectFeedRowNotSelectable from JSON into (:row FEEDID).
The clicked row the daemon does not deem selectable, echoed.  Emacs never
sends a click, but its decoder knows every arm the contract has."
  (let ((message "SelectFeedRowNotSelectable"))
    (agent-repl-wire-verbs--check-keys message json '(row))
    (list :row (agent-repl-wire-verbs--decode-required-message
                message 'row json #'agent-repl-wire-decode-feed-id))))

(defun agent-repl-wire-decode-select-feed-row-response (json)
  "Decode SelectFeedRowResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-result
   "SelectFeedRowResponse" json
   #'agent-repl-wire-decode-select-feed-row-success
   #'agent-repl-wire-decode-select-feed-row-error))

;;;; ---- Rollback: the shared token ------------------------------------
;;
;; See rollback.proto.  The token is OPAQUE: decoded from a PlanRollback
;; plan and handed back to RollBack unchanged, never built by Emacs.

(defun agent-repl-wire-decode-rollback-token (json)
  "Decode RollbackToken from JSON into (:value STRING).
The value is REQUIRED: a plan without a token cannot be confirmed."
  (let ((message "RollbackToken"))
    (agent-repl-wire-verbs--check-keys message json '(value))
    (list :value (agent-repl-wire-verbs--decode-required-string message 'value json))))

(defun agent-repl-wire-encode-rollback-token (token)
  "Encode RollbackToken from TOKEN, the plist a plan decoded, verbatim."
  (list (cons 'value (agent-repl-wire-verbs--require-string
                      "RollbackToken" "value" (plist-get token :value)))))

;;;; ---- PlanRollback ---------------------------------------------------
;;
;; Say what a rollback would do, for the user to confirm.  THE ARM IS THE
;; FILES CHOICE on the request and THE ARM IS THE OUTCOME on the success.
;; See endpoint_plan_rollback.proto.

(defun agent-repl-wire-encode-plan-rollback-empty-files (_value)
  "Encode the empty PlanRollbackKeepFiles / PlanRollbackRestoreFiles arm."
  nil)

(defun agent-repl-wire-encode-plan-rollback-request-files (value)
  "Encode PlanRollbackRequest's `files' oneof from VALUE.
VALUE is (:arm KEYWORD :value nil), KEYWORD `:keep-files' or
`:restore-files'."
  (agent-repl-wire-verbs--encode-oneof
   "PlanRollbackRequest" "files" value
   (list (list :keep-files 'keepFiles #'agent-repl-wire-encode-plan-rollback-empty-files)
         (list :restore-files 'restoreFiles #'agent-repl-wire-encode-plan-rollback-empty-files))))

(defun agent-repl-wire-encode-plan-rollback-request (request)
  "Encode PlanRollbackRequest from plist REQUEST (:workspace REF :files FILES).
Both are required; an incomplete request errors here rather than reaching
the wire."
  (let ((message "PlanRollbackRequest"))
    (list (cons 'workspace
                (agent-repl-wire-encode-workspace-ref
                 (agent-repl-wire-verbs--require message "workspace"
                                                  (plist-get request :workspace))))
          (agent-repl-wire-encode-plan-rollback-request-files
           (agent-repl-wire-verbs--require message "files" (plist-get request :files))))))

(defun agent-repl-wire-decode-rollback-plan-target (json)
  "Decode RollbackPlanTarget from JSON into a plist.
\(:chosen ARM :excerpt STRING :prompts-dropped N), ARM `:selected' or
`:latest'; exactly one is set."
  (let ((message "RollbackPlanTarget"))
    (agent-repl-wire-verbs--check-keys message json '(selected latest excerpt promptsDropped))
    (list :chosen (plist-get
                   (agent-repl-wire-verbs--decode-oneof
                    message "chosen" json
                    (list (list 'selected :selected
                                (lambda (v) (agent-repl-wire-verbs--decode-empty
                                             "RollbackPlanTargetSelected" v)))
                          (list 'latest :latest
                                (lambda (v) (agent-repl-wire-verbs--decode-empty
                                             "RollbackPlanTargetLatest" v)))))
                   :arm)
          :excerpt (agent-repl-wire-verbs--decode-string message 'excerpt json)
          :prompts-dropped (agent-repl-wire--decode-uint32 message 'promptsDropped json))))

(defun agent-repl-wire-decode-rollback-plan-cancel-detached (json)
  "Decode RollbackPlanCancelDetached from JSON into (:items N)."
  (let ((message "RollbackPlanCancelDetached"))
    (agent-repl-wire-verbs--check-keys message json '(items))
    (list :items (agent-repl-wire--decode-uint32 message 'items json))))

(defun agent-repl-wire-decode-rollback-plan-files-restored (json)
  "Decode RollbackPlanFilesRestored from JSON into (:cancel-detached C-or-nil)."
  (let ((message "RollbackPlanFilesRestored"))
    (agent-repl-wire-verbs--check-keys message json '(cancelDetached))
    (list :cancel-detached
          (agent-repl-wire-verbs--decode-optional-message
           message 'cancelDetached json
           #'agent-repl-wire-decode-rollback-plan-cancel-detached))))

(defun agent-repl-wire-decode-rollback-plan-files (json)
  "Decode RollbackPlanFiles from JSON into (:arm ARM :value V).
ARM is `:kept' (V nil) or `:restored' (V the restored plist)."
  (let ((message "RollbackPlanFiles"))
    (agent-repl-wire-verbs--check-keys message json '(kept restored))
    (agent-repl-wire-verbs--decode-oneof
     message "files" json
     (list (list 'kept :kept
                 (lambda (v) (agent-repl-wire-verbs--decode-empty "RollbackPlanFilesKept" v)))
           (list 'restored :restored #'agent-repl-wire-decode-rollback-plan-files-restored)))))

(defun agent-repl-wire-decode-rollback-plan-drop-queued (json)
  "Decode RollbackPlanDropQueued from JSON into (:prompts N)."
  (let ((message "RollbackPlanDropQueued"))
    (agent-repl-wire-verbs--check-keys message json '(prompts))
    (list :prompts (agent-repl-wire--decode-uint32 message 'prompts json))))

(defun agent-repl-wire-decode-rollback-plan (json)
  "Decode RollbackPlan from JSON into a plist.
\(:token TOKEN :target TARGET :files FILES :interrupt BOOL :drop-queued D).
TOKEN, TARGET and FILES are required.  `interrupt' is an OPTIONAL EMPTY
message, so its PRESENCE is the fact: t when set, nil when unset.
`drop_queued' is nil when unset."
  (let ((message "RollbackPlan"))
    (agent-repl-wire-verbs--check-keys
     message json '(token target files interrupt dropQueued))
    (list :token (agent-repl-wire-verbs--decode-required-message
                  message 'token json #'agent-repl-wire-decode-rollback-token)
          :target (agent-repl-wire-verbs--decode-required-message
                   message 'target json #'agent-repl-wire-decode-rollback-plan-target)
          :files (agent-repl-wire-verbs--decode-required-message
                  message 'files json #'agent-repl-wire-decode-rollback-plan-files)
          :interrupt (and (agent-repl-wire-verbs--decode-optional-message
                           message 'interrupt json
                           (lambda (v)
                             (agent-repl-wire-verbs--decode-empty "RollbackPlanInterrupt" v)
                             t))
                          t)
          :drop-queued (agent-repl-wire-verbs--decode-optional-message
                        message 'dropQueued json
                        #'agent-repl-wire-decode-rollback-plan-drop-queued))))

(defun agent-repl-wire-decode-plan-rollback-success (json)
  "Decode PlanRollbackSuccess from JSON into (:outcome (:arm ARM :value V)).
ARM is `:plan' (V the plan plist) or `:nothing-to-roll-back' (V nil)."
  (let ((message "PlanRollbackSuccess"))
    (agent-repl-wire-verbs--check-keys message json '(plan nothingToRollBack))
    (list :outcome
          (agent-repl-wire-verbs--decode-oneof
           message "outcome" json
           (list (list 'plan :plan #'agent-repl-wire-decode-rollback-plan)
                 (list 'nothingToRollBack :nothing-to-roll-back
                       (lambda (v) (agent-repl-wire-verbs--decode-empty
                                    "PlanRollbackNothingToRollBack" v))))))))

(defun agent-repl-wire-verbs--decode-registry-dir (message json)
  "Decode the workspace-ref-mismatch MESSAGE from JSON into (:registry-dir)."
  (agent-repl-wire-verbs--check-keys message json '(registryDir))
  (list :registry-dir (agent-repl-wire-verbs--decode-string message 'registryDir json)))

(defun agent-repl-wire-verbs--decode-address (message json)
  "Decode the transferring-away MESSAGE from JSON into (:address)."
  (agent-repl-wire-verbs--check-keys message json '(address))
  (list :address (agent-repl-wire-verbs--decode-string message 'address json)))

(defun agent-repl-wire-verbs--decode-vendor-message (message json)
  "Decode the vendor-refusal MESSAGE from JSON into (:vendor-message)."
  (agent-repl-wire-verbs--check-keys message json '(vendorMessage))
  (list :vendor-message (agent-repl-wire-verbs--decode-string message 'vendorMessage json)))

(defun agent-repl-wire-verbs--empty-arm (message)
  "Return a decoder for the empty refusal arm MESSAGE."
  (lambda (json) (agent-repl-wire-verbs--decode-empty message json)))

(defun agent-repl-wire-verbs--handover-arms (prefix)
  "Return the four cross-cutting refusal arms, their messages named PREFIX*.
`unknown_workspace', `workspace_ref_mismatch', `transferring_away' and
`not_yet_adopted', as `agent-repl-wire-verbs--decode-oneof' arms."
  (list (list 'unknownWorkspace :unknown-workspace
              (agent-repl-wire-verbs--empty-arm (concat prefix "UnknownWorkspace")))
        (list 'workspaceRefMismatch :workspace-ref-mismatch
              (lambda (json) (agent-repl-wire-verbs--decode-registry-dir
                              (concat prefix "WorkspaceRefMismatch") json)))
        (list 'transferringAway :transferring-away
              (lambda (json) (agent-repl-wire-verbs--decode-address
                              (concat prefix "TransferringAway") json)))
        (list 'notYetAdopted :not-yet-adopted
              (agent-repl-wire-verbs--empty-arm (concat prefix "NotYetAdopted")))))

(defun agent-repl-wire-decode-plan-rollback-error (json)
  "Decode PlanRollbackError from JSON into (:cause (:arm ARM :value V))."
  (let ((message "PlanRollbackError"))
    (agent-repl-wire-verbs--check-keys
     message json '(unknownWorkspace workspaceRefMismatch transferringAway notYetAdopted))
    (list :cause
          (agent-repl-wire-verbs--decode-oneof
           message "cause" json (agent-repl-wire-verbs--handover-arms "PlanRollback")))))

(defun agent-repl-wire-decode-plan-rollback-response (json)
  "Decode PlanRollbackResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-result
   "PlanRollbackResponse" json
   #'agent-repl-wire-decode-plan-rollback-success
   #'agent-repl-wire-decode-plan-rollback-error))

;;;; ---- RollBack -------------------------------------------------------
;;
;; Perform a confirmed plan: the token goes back exactly as PlanRollback
;; served it.  See endpoint_roll_back.proto.

(defun agent-repl-wire-encode-roll-back-request (request)
  "Encode RollBackRequest from plist REQUEST (:workspace REF :token TOKEN).
Both are required.  TOKEN is the plan's decoded token, echoed verbatim."
  (let ((message "RollBackRequest"))
    (list (cons 'workspace
                (agent-repl-wire-encode-workspace-ref
                 (agent-repl-wire-verbs--require message "workspace"
                                                  (plist-get request :workspace))))
          (cons 'token
                (agent-repl-wire-encode-rollback-token
                 (agent-repl-wire-verbs--require message "token"
                                                  (plist-get request :token)))))))

(defun agent-repl-wire-decode-roll-back-files-restored (json)
  "Decode RollBackFilesRestored from JSON into (:files N)."
  (let ((message "RollBackFilesRestored"))
    (agent-repl-wire-verbs--check-keys message json '(files))
    (list :files (agent-repl-wire--decode-uint32 message 'files json))))

(defun agent-repl-wire-decode-roll-back-success (json)
  "Decode RollBackSuccess from JSON into (:prompt SAID :files-restored F-or-nil).
The prompt is REQUIRED: it is what the composer holds again."
  (let ((message "RollBackSuccess"))
    (agent-repl-wire-verbs--check-keys message json '(prompt filesRestored))
    (list :prompt (agent-repl-wire-verbs--decode-required-message
                   message 'prompt json #'agent-repl-wire-decode-user-said)
          :files-restored (agent-repl-wire-verbs--decode-optional-message
                           message 'filesRestored json
                           #'agent-repl-wire-decode-roll-back-files-restored))))

(defconst agent-repl-wire-roll-back-error-own-arms
  '((planStale :plan-stale "RollBackPlanStale")
    (noSession :no-session "RollBackNoSession")
    (promptNotRecorded :prompt-not-recorded "RollBackPromptNotRecorded")
    (firstPrompt :first-prompt "RollBackFirstPrompt")
    (unseenPrompt :unseen-prompt "RollBackUnseenPrompt"))
  "RollBackError's own EMPTY cause arms: wire key, keyword, message name.")

(defun agent-repl-wire-decode-roll-back-error (json)
  "Decode RollBackError from JSON into (:cause (:arm ARM :value V)).
THE ARM IS WHY.  `vendor_refused' and `files_not_restorable' carry the
vendor's message as (:vendor-message STRING)."
  (let ((message "RollBackError"))
    (agent-repl-wire-verbs--check-keys
     message json '(unknownWorkspace workspaceRefMismatch transferringAway notYetAdopted
                    planStale noSession promptNotRecorded firstPrompt unseenPrompt
                    vendorRefused filesNotRestorable))
    (list :cause
          (agent-repl-wire-verbs--decode-oneof
           message "cause" json
           (append
            (agent-repl-wire-verbs--handover-arms "RollBack")
            (mapcar (lambda (arm)
                      (list (nth 0 arm) (nth 1 arm)
                            (agent-repl-wire-verbs--empty-arm (nth 2 arm))))
                    agent-repl-wire-roll-back-error-own-arms)
            (list (list 'vendorRefused :vendor-refused
                        (lambda (v) (agent-repl-wire-verbs--decode-vendor-message
                                     "RollBackVendorRefused" v)))
                  (list 'filesNotRestorable :files-not-restorable
                        (lambda (v) (agent-repl-wire-verbs--decode-vendor-message
                                     "RollBackFilesNotRestorable" v)))))))))

(defun agent-repl-wire-decode-roll-back-response (json)
  "Decode RollBackResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-result
   "RollBackResponse" json
   #'agent-repl-wire-decode-roll-back-success
   #'agent-repl-wire-decode-roll-back-error))

;;;; ---- EditHeldPrompt -------------------------------------------------
;;
;; Editing a held prompt.  The webapp's tray card BEGINS an edit; this
;; composer COMMITS (the new content, whole) or CANCELS it.  THE ARM IS THE
;; STEP on the request and THE ARM IS THE OUTCOME on the response.  See
;; endpoint_edit_held_prompt.proto.

(defun agent-repl-wire-encode-edit-held-prompt-request-workspace (ref)
  "Encode EditHeldPromptRequest's `workspace' use site from REF."
  (agent-repl-wire-encode-workspace-ref ref))

(defun agent-repl-wire-encode-edit-held-prompt-request-turn (turn)
  "Encode EditHeldPromptRequest's `turn' use site from TURN.
TURN is the decoded TurnId plist the host view's edit carried, echoed."
  (agent-repl-wire-encode-turn-id turn))

(defun agent-repl-wire-encode-edit-held-prompt-commit-said (said)
  "Encode EditHeldPromptCommit's `said' use site from SAID."
  (agent-repl-wire-encode-user-said said))

(defun agent-repl-wire-encode-edit-held-prompt-commit (value)
  "Encode EditHeldPromptCommit from plist VALUE (:said SAID).
The content is REQUIRED: a commit replaces the held prompt's content whole."
  (list (cons 'said
              (agent-repl-wire-encode-edit-held-prompt-commit-said
               (agent-repl-wire-verbs--require "EditHeldPromptCommit" "said"
                                                (plist-get value :said))))))

(defun agent-repl-wire-encode-edit-held-prompt-empty-step (_value)
  "Encode the empty EditHeldPromptBegin / EditHeldPromptCancel step."
  nil)

(defun agent-repl-wire-encode-edit-held-prompt-request-action (value)
  "Encode EditHeldPromptRequest's `action' oneof from VALUE.
VALUE is (:arm KEYWORD :value V), KEYWORD one of `:begin', `:commit' and
`:cancel'."
  (agent-repl-wire-verbs--encode-oneof
   "EditHeldPromptRequest" "action" value
   (list (list :begin 'begin #'agent-repl-wire-encode-edit-held-prompt-empty-step)
         (list :commit 'commit #'agent-repl-wire-encode-edit-held-prompt-commit)
         (list :cancel 'cancel #'agent-repl-wire-encode-edit-held-prompt-empty-step))))

(defun agent-repl-wire-encode-edit-held-prompt-request (request)
  "Encode EditHeldPromptRequest from plist REQUEST (:workspace :turn :action).
All three are required; an incomplete request errors here rather than
reaching the wire."
  (let ((message "EditHeldPromptRequest"))
    (agent-repl--log '(:agent-repl-context "a codec call outside a request has no workspace")
                     "elisp.wire.verbs-encode-edit-held-prompt-request step=%S"
                     (plist-get (plist-get request :action) :arm))
    (list (cons 'workspace
                (agent-repl-wire-encode-edit-held-prompt-request-workspace
                 (agent-repl-wire-verbs--require message "workspace" (plist-get request :workspace))))
          (cons 'turn
                (agent-repl-wire-encode-edit-held-prompt-request-turn
                 (agent-repl-wire-verbs--require message "turn" (plist-get request :turn))))
          (agent-repl-wire-encode-edit-held-prompt-request-action
           (agent-repl-wire-verbs--require message "action" (plist-get request :action))))))

(defun agent-repl-wire-decode-edit-held-prompt-success (json)
  "Decode EditHeldPromptSuccess from JSON.  Empty: the step was taken."
  (agent-repl-wire-verbs--decode-empty "EditHeldPromptSuccess" json))

(defun agent-repl-wire-decode-edit-held-prompt-workspace-ref-mismatch (json)
  "Decode EditHeldPromptWorkspaceRefMismatch from JSON into (:registry-dir)."
  (let ((message "EditHeldPromptWorkspaceRefMismatch"))
    (agent-repl-wire-verbs--check-keys message json '(registryDir))
    (list :registry-dir (agent-repl-wire-verbs--decode-string message 'registryDir json))))

(defun agent-repl-wire-decode-edit-held-prompt-transferring-away (json)
  "Decode EditHeldPromptTransferringAway from JSON into (:address)."
  (let ((message "EditHeldPromptTransferringAway"))
    (agent-repl-wire-verbs--check-keys message json '(address))
    (list :address (agent-repl-wire-verbs--decode-string message 'address json))))

(defun agent-repl-wire-decode-edit-held-prompt-being-edited-editing-turn (json)
  "Decode EditHeldPromptBeingEdited's `editing_turn' use site from JSON."
  (agent-repl-wire-decode-turn-id json))

(defun agent-repl-wire-decode-edit-held-prompt-being-edited (json)
  "Decode EditHeldPromptBeingEdited from JSON into (:editing-turn TURN)."
  (let ((message "EditHeldPromptBeingEdited"))
    (agent-repl-wire-verbs--check-keys message json '(editingTurn))
    (list :editing-turn
          (agent-repl-wire--decode-message
           message 'editingTurn json
           #'agent-repl-wire-decode-edit-held-prompt-being-edited-editing-turn))))

(defun agent-repl-wire-decode-edit-held-prompt-empty-cause (message)
  "Return a decoder for the empty refusal arm MESSAGE."
  (lambda (json) (agent-repl-wire-verbs--decode-empty message json)))

(defun agent-repl-wire-decode-edit-held-prompt-error (json)
  "Decode EditHeldPromptError from JSON into (:cause (:arm ARM :value V)).
THE ARM IS THE REFUSAL, so an unset cause is a contract breach."
  (let ((message "EditHeldPromptError"))
    (agent-repl-wire-verbs--check-keys
     message json '(unknownWorkspace workspaceRefMismatch transferringAway notYetAdopted
                    noSuchHold notHeld alreadyDelivered beingEdited notEditing noEditor
                    beingDelivered))
    (list :cause
          (agent-repl-wire-verbs--decode-oneof
           message "cause" json
           (list (list 'unknownWorkspace :unknown-workspace
                       (agent-repl-wire-decode-edit-held-prompt-empty-cause "EditHeldPromptUnknownWorkspace"))
                 (list 'workspaceRefMismatch :workspace-ref-mismatch
                       #'agent-repl-wire-decode-edit-held-prompt-workspace-ref-mismatch)
                 (list 'transferringAway :transferring-away
                       #'agent-repl-wire-decode-edit-held-prompt-transferring-away)
                 (list 'notYetAdopted :not-yet-adopted
                       (agent-repl-wire-decode-edit-held-prompt-empty-cause "EditHeldPromptNotYetAdopted"))
                 (list 'noSuchHold :no-such-hold
                       (agent-repl-wire-decode-edit-held-prompt-empty-cause "EditHeldPromptNoSuchHold"))
                 (list 'notHeld :not-held
                       (agent-repl-wire-decode-edit-held-prompt-empty-cause "EditHeldPromptNotHeld"))
                 (list 'alreadyDelivered :already-delivered
                       (agent-repl-wire-decode-edit-held-prompt-empty-cause "EditHeldPromptAlreadyDelivered"))
                 (list 'beingEdited :being-edited
                       #'agent-repl-wire-decode-edit-held-prompt-being-edited)
                 (list 'notEditing :not-editing
                       (agent-repl-wire-decode-edit-held-prompt-empty-cause "EditHeldPromptNotEditing"))
                 (list 'noEditor :no-editor
                       (agent-repl-wire-decode-edit-held-prompt-empty-cause "EditHeldPromptNoEditor"))
                 ;; The prompt's delivery call is in flight: neither held nor
                 ;; delivered yet, and back in the tray if the call fails.
                 (list 'beingDelivered :being-delivered
                       (agent-repl-wire-decode-edit-held-prompt-empty-cause "EditHeldPromptBeingDelivered")))))))

(defun agent-repl-wire-decode-edit-held-prompt-response (json)
  "Decode EditHeldPromptResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-result
   "EditHeldPromptResponse" json
   #'agent-repl-wire-decode-edit-held-prompt-success
   #'agent-repl-wire-decode-edit-held-prompt-error))

;; AdjustFeedTextScale — the feed text zoom nudge. The request is a bare
;; DIRECTION (the scale is daemon-global, so there is no workspace ref); the
;; response is a bare `scale' double (no result oneof, because the preference
;; has no per-workspace ownership to refuse on). See
;; endpoint_adjust_feed_text_scale.proto.

(defconst agent-repl-wire-adjust-feed-text-scale-directions
  '((:increase . "ADJUST_FEED_TEXT_SCALE_DIRECTION_INCREASE")
    (:decrease . "ADJUST_FEED_TEXT_SCALE_DIRECTION_DECREASE"))
  "Map an AdjustFeedTextScaleDirection keyword to its protojson enum name.
`:unspecified' is deliberately ABSENT: it is never a legitimate nudge, so
a request carrying it is refused before it reaches the wire.")

(defun agent-repl-wire-encode-adjust-feed-text-scale-direction (value)
  "Encode the AdjustFeedTextScaleDirection keyword VALUE as its enum name.
The vocabulary is closed; an unknown keyword — `:unspecified' in
particular — is refused rather than sent."
  (let ((name (cdr (assq value agent-repl-wire-adjust-feed-text-scale-directions))))
    (unless name
      (agent-repl-wire-verbs--fail "AdjustFeedTextScaleRequest" "direction" "unknown direction"))
    name))

(defun agent-repl-wire-encode-adjust-feed-text-scale-request (request)
  "Encode AdjustFeedTextScaleRequest from plist REQUEST (:direction K).
The direction is required; an incomplete request errors here rather than
reaching the wire."
  (let ((message "AdjustFeedTextScaleRequest"))
    (list (cons 'direction
                (agent-repl-wire-encode-adjust-feed-text-scale-direction
                 (agent-repl-wire-verbs--require message "direction"
                                                 (plist-get request :direction)))))))

(defun agent-repl-wire-decode-adjust-feed-text-scale-response (json)
  "Decode AdjustFeedTextScaleResponse from JSON into (:scale FLOAT).
There is no result oneof to unwrap: the response is the clamped scale now
in force, which the caller may echo."
  (list :scale (agent-repl-wire--decode-double "AdjustFeedTextScaleResponse" 'scale json)))

(provide 'agent-repl-wire-verbs)

;;; wire-verbs.el ends here
