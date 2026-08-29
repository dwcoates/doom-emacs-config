;;; wire-verbs.el --- protojson codec for the agentrepl.v1 verbs -*- lexical-binding: t; -*-

;;; Commentary:

;; The protojson CODEC for the agentrepl.v1 WORKSPACE VERBS and DAEMON-ADMIN
;; verbs Emacs calls: CreateWorkspace, OpenWorkspace, CloseWorkspace,
;; KillWorkspace, NukeWorkspace, MergeWorkspace, RestartWorkspace,
;; SetWorkspacePriority, SubmitPrompt, UpdateShutdownSchedule,
;; UpdateMergeQueue, DaemonHealth and SessionHealth.
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
;; site's derived name would COLLIDE with the child's own base name (e.g.
;; CreateWorkspaceOneShot's `self_merge' arm and the message
;; CreateWorkspaceOneShotSelfMerge both spell
;; `agent-repl-wire-{en,de}code-create-workspace-one-shot-self-merge'), the
;; base IS the use-site function: the delegation would be the identity, and
;; two definitions of one name are not possible.
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
(declare-function agent-repl-wire-encode-workspace-ref "agent-repl-wire-common" (ref))
(declare-function agent-repl-wire-decode-workspace-ref "agent-repl-wire-common" (json))
(declare-function agent-repl-wire-encode-repository-ref "agent-repl-wire-common" (ref))
(declare-function agent-repl-wire-encode-user-said "agent-repl-wire-common" (said))
(declare-function agent-repl-wire-encode-prompt-origin "agent-repl-wire-common" (origin))
(declare-function agent-repl-wire-encode-drain-reason "agent-repl-wire-common" (reason))
(declare-function agent-repl-wire-encode-workspace-priority "agent-repl-wire-common" (priority))
(declare-function agent-repl-wire-decode-turn-id "agent-repl-wire-common" (json))

;; core.el's canonical logging ladder.
(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--error "agent-repl-core" (ws fmt &rest args))


;;;; ---- Shared primitives ----------------------------------------------

(defun agent-repl-wire-verbs--fail (message field reason)
  "Log a contract breach at ERROR and signal `agent-repl-wire-error'.
MESSAGE names the protobuf message, FIELD the offending field or oneof,
REASON the breach.  `agent-repl--error' both persists the record at ERROR
level and signals a plain `error'; the signal is swallowed here so the
TYPED `agent-repl-wire-error' — the one every wire consumer catches — is
what actually leaves this function, with the ERROR log already written."
  (condition-case nil
      (agent-repl--error nil "elisp.wire.verbs-contract-breach message=%s field=%s reason=%s"
                          message field reason)
    (error nil))
  (signal 'agent-repl-wire-error (list message field reason)))

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
  (agent-repl--log nil "elisp.wire.verbs-decode-empty message=%s" message)
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
        (agent-repl--log nil "elisp.wire.verbs-decode-oneof message=%s field=%s arm=%s"
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

(defun agent-repl-wire-verbs--require (message field value)
  "Return VALUE, or fail because MESSAGE's non-optional FIELD is unset."
  (or value
      (agent-repl-wire-verbs--fail message field "required field is unset")))

(defun agent-repl-wire-verbs--require-string (message field value)
  "Return VALUE as MESSAGE's required non-blank string FIELD, or fail."
  (cond
   ((not (stringp value))
    (agent-repl-wire-verbs--fail message field "required field is unset"))
   ((string-empty-p value)
    (agent-repl-wire-verbs--fail message field "required string is empty"))
   (t value)))

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
    (agent-repl--log nil "elisp.wire.verbs-encode-oneof message=%s field=%s arm=%s"
                      message field keyword)
    (cons (nth 1 arm) (funcall (nth 2 arm) (plist-get value :value)))))

(defun agent-repl-wire-verbs--encode-empty (_value)
  "Encode a set EMPTY message: nil, which serializes as `{}'."
  nil)


;;;; ---- CreateWorkspace: encode ----------------------------------------

(defun agent-repl-wire-encode-create-workspace-fork (_present)
  "Encode CreateWorkspaceFork.  Empty: presence IS the fork fact."
  nil)

(defun agent-repl-wire-encode-create-workspace-ungated-consent (_present)
  "Encode CreateWorkspaceUngatedConsent.  Empty: presence IS the consent."
  nil)

(defun agent-repl-wire-encode-create-workspace-one-shot-self-merge (_value)
  "Encode CreateWorkspaceOneShotSelfMerge.  Empty: the arm is the whole fact.
Also the `self_merge' arm's use-site encoder — the derived use-site name
is this name, so the base serves both roles."
  nil)

(defun agent-repl-wire-encode-create-workspace-one-shot-open-pr (value)
  "Encode CreateWorkspaceOneShotOpenPr from plist VALUE.
VALUE is (:self-certified BOOL :add-to-merge-queue BOOL); both are plain
proto3 bools with a `false' default and are spelled explicitly.  Also the
`open_pr' arm's use-site encoder, whose derived name is this name."
  (list (cons 'selfCertified
              (agent-repl-wire-verbs--encode-bool (plist-get value :self-certified)))
        (cons 'addToMergeQueue
              (agent-repl-wire-verbs--encode-bool (plist-get value :add-to-merge-queue)))))

(defun agent-repl-wire-encode-create-workspace-one-shot-prompt (said)
  "Encode CreateWorkspaceOneShot's `prompt' use site from SAID."
  (agent-repl-wire-encode-user-said said))

(defun agent-repl-wire-encode-create-workspace-one-shot (value)
  "Encode CreateWorkspaceOneShot from plist VALUE.
VALUE is (:prompt SAID :finish ONEOF).  A one-shot IS its prompt, so the
prompt is required; the finish oneof is the finish action and an unset
oneof is a breach."
  (let ((message "CreateWorkspaceOneShot"))
    (list (cons 'prompt
                (agent-repl-wire-encode-create-workspace-one-shot-prompt
                 (agent-repl-wire-verbs--require message "prompt" (plist-get value :prompt))))
          (agent-repl-wire-verbs--encode-oneof
           message "finish" (plist-get value :finish)
           (list (list :self-merge 'selfMerge
                       #'agent-repl-wire-encode-create-workspace-one-shot-self-merge)
                 (list :open-pr 'openPr
                       #'agent-repl-wire-encode-create-workspace-one-shot-open-pr))))))

(defun agent-repl-wire-encode-create-workspace-merge-actions-before-ws-merge (said)
  "Encode CreateWorkspaceMergeActions's `before_ws_merge' use site from SAID."
  (agent-repl-wire-encode-user-said said))

(defun agent-repl-wire-encode-create-workspace-merge-actions-postprocessing-prompt (said)
  "Encode CreateWorkspaceMergeActions's `postprocessing_prompt' use site from SAID."
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
    (agent-repl--log nil "elisp.wire.verbs-encode-create-workspace-request form=%s"
                      (plist-get (plist-get request :form) :arm))
    (nreverse out)))


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

(defun agent-repl-wire-decode-create-workspace-error (json)
  "Decode CreateWorkspaceError from JSON.  Empty until its arms are derived."
  (agent-repl-wire-verbs--decode-empty "CreateWorkspaceError" json))

(defun agent-repl-wire-decode-create-workspace-response-success (json)
  "Decode CreateWorkspaceResponse's `success' arm from JSON."
  (agent-repl-wire-decode-create-workspace-success json))

(defun agent-repl-wire-decode-create-workspace-response-error (json)
  "Decode CreateWorkspaceResponse's `error' arm from JSON."
  (agent-repl-wire-decode-create-workspace-error json))

(defun agent-repl-wire-decode-create-workspace-response (json)
  "Decode CreateWorkspaceResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-result
   "CreateWorkspaceResponse" json
   #'agent-repl-wire-decode-create-workspace-response-success
   #'agent-repl-wire-decode-create-workspace-response-error))


(provide 'agent-repl-wire-verbs)

;;; wire-verbs.el ends here
