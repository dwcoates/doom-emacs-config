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
(declare-function agent-repl-wire--fail "agent-repl-wire-common" (message field reason))
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


;;;; ---- OpenWorkspace --------------------------------------------------

(defun agent-repl-wire-encode-open-workspace-request-workspace (ref)
  "Encode OpenWorkspaceRequest's `workspace' use site from REF."
  (agent-repl-wire-encode-workspace-ref ref))

(defun agent-repl-wire-encode-open-workspace-request (request)
  "Encode OpenWorkspaceRequest from plist REQUEST (:workspace REF)."
  (agent-repl--log nil "elisp.wire.verbs-encode-open-workspace-request")
  (list (cons 'workspace
              (agent-repl-wire-encode-open-workspace-request-workspace
               (agent-repl-wire-verbs--require "OpenWorkspaceRequest" "workspace"
                                                (plist-get request :workspace))))))

(defun agent-repl-wire-decode-open-workspace-success (json)
  "Decode OpenWorkspaceSuccess from JSON.  Empty: the effects ride the streams."
  (agent-repl-wire-verbs--decode-empty "OpenWorkspaceSuccess" json))

(defun agent-repl-wire-decode-open-workspace-error (json)
  "Decode OpenWorkspaceError from JSON.  Empty until its arms are derived."
  (agent-repl-wire-verbs--decode-empty "OpenWorkspaceError" json))

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


;;;; ---- CloseWorkspace -------------------------------------------------

(defun agent-repl-wire-encode-close-workspace-request-workspace (ref)
  "Encode CloseWorkspaceRequest's `workspace' use site from REF."
  (agent-repl-wire-encode-workspace-ref ref))

(defun agent-repl-wire-encode-close-workspace-request (request)
  "Encode CloseWorkspaceRequest from plist REQUEST (:workspace REF)."
  (agent-repl--log nil "elisp.wire.verbs-encode-close-workspace-request")
  (list (cons 'workspace
              (agent-repl-wire-encode-close-workspace-request-workspace
               (agent-repl-wire-verbs--require "CloseWorkspaceRequest" "workspace"
                                                (plist-get request :workspace))))))

(defun agent-repl-wire-decode-close-workspace-success (json)
  "Decode CloseWorkspaceSuccess from JSON.  Empty: quiet, so the close happened."
  (agent-repl-wire-verbs--decode-empty "CloseWorkspaceSuccess" json))

(defun agent-repl-wire-decode-close-workspace-blocked (json)
  "Decode CloseWorkspaceBlocked from JSON.
Empty on purpose: the reasons are pushed on the footer stream, so this
refusal never restates them."
  (agent-repl-wire-verbs--decode-empty "CloseWorkspaceBlocked" json))

(defun agent-repl-wire-decode-close-workspace-error-blocked (json)
  "Decode CloseWorkspaceError's `blocked' cause arm from JSON."
  (agent-repl-wire-decode-close-workspace-blocked json))

(defun agent-repl-wire-decode-close-workspace-error (json)
  "Decode CloseWorkspaceError from JSON into (:cause (:arm ARM :value V)).
The cause oneof is the refusal, so an unset cause is a contract breach."
  (let ((message "CloseWorkspaceError"))
    (agent-repl-wire-verbs--check-keys message json '(blocked))
    (list :cause
          (agent-repl-wire-verbs--decode-oneof
           message "cause" json
           (list (list 'blocked :blocked
                       #'agent-repl-wire-decode-close-workspace-error-blocked))))))

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
  "Encode KillWorkspaceRequest from plist REQUEST (:workspace REF)."
  (agent-repl--log nil "elisp.wire.verbs-encode-kill-workspace-request")
  (list (cons 'workspace
              (agent-repl-wire-encode-kill-workspace-request-workspace
               (agent-repl-wire-verbs--require "KillWorkspaceRequest" "workspace"
                                                (plist-get request :workspace))))))

(defun agent-repl-wire-decode-kill-workspace-success (json)
  "Decode KillWorkspaceSuccess from JSON.  Empty: the effects ride the streams."
  (agent-repl-wire-verbs--decode-empty "KillWorkspaceSuccess" json))

(defun agent-repl-wire-decode-kill-workspace-error (json)
  "Decode KillWorkspaceError from JSON.  Empty until its arms are derived."
  (agent-repl-wire-verbs--decode-empty "KillWorkspaceError" json))

(defun agent-repl-wire-decode-kill-workspace-response-success (json)
  "Decode KillWorkspaceResponse's `success' arm from JSON."
  (agent-repl-wire-decode-kill-workspace-success json))

(defun agent-repl-wire-decode-kill-workspace-response-error (json)
  "Decode KillWorkspaceResponse's `error' arm from JSON."
  (agent-repl-wire-decode-kill-workspace-error json))

(defun agent-repl-wire-decode-kill-workspace-response (json)
  "Decode KillWorkspaceResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-result
   "KillWorkspaceResponse" json
   #'agent-repl-wire-decode-kill-workspace-response-success
   #'agent-repl-wire-decode-kill-workspace-response-error))


;;;; ---- NukeWorkspace --------------------------------------------------

(defun agent-repl-wire-encode-nuke-workspace-request-workspace (ref)
  "Encode NukeWorkspaceRequest's `workspace' use site from REF."
  (agent-repl-wire-encode-workspace-ref ref))

(defun agent-repl-wire-encode-nuke-workspace-request (request)
  "Encode NukeWorkspaceRequest from plist REQUEST (:workspace REF)."
  (agent-repl--log nil "elisp.wire.verbs-encode-nuke-workspace-request")
  (list (cons 'workspace
              (agent-repl-wire-encode-nuke-workspace-request-workspace
               (agent-repl-wire-verbs--require "NukeWorkspaceRequest" "workspace"
                                                (plist-get request :workspace))))))

(defun agent-repl-wire-decode-nuke-workspace-success (json)
  "Decode NukeWorkspaceSuccess from JSON.  Empty: the effects ride the streams."
  (agent-repl-wire-verbs--decode-empty "NukeWorkspaceSuccess" json))

(defun agent-repl-wire-decode-nuke-workspace-error (json)
  "Decode NukeWorkspaceError from JSON.  Empty until its arms are derived."
  (agent-repl-wire-verbs--decode-empty "NukeWorkspaceError" json))

(defun agent-repl-wire-decode-nuke-workspace-response-success (json)
  "Decode NukeWorkspaceResponse's `success' arm from JSON."
  (agent-repl-wire-decode-nuke-workspace-success json))

(defun agent-repl-wire-decode-nuke-workspace-response-error (json)
  "Decode NukeWorkspaceResponse's `error' arm from JSON."
  (agent-repl-wire-decode-nuke-workspace-error json))

(defun agent-repl-wire-decode-nuke-workspace-response (json)
  "Decode NukeWorkspaceResponse from JSON into (:arm ARM :value V)."
  (agent-repl-wire-verbs--decode-result
   "NukeWorkspaceResponse" json
   #'agent-repl-wire-decode-nuke-workspace-response-success
   #'agent-repl-wire-decode-nuke-workspace-response-error))


;;;; ---- MergeWorkspace -------------------------------------------------

(defun agent-repl-wire-encode-merge-workspace-request-workspace (ref)
  "Encode MergeWorkspaceRequest's `workspace' use site from REF."
  (agent-repl-wire-encode-workspace-ref ref))

(defun agent-repl-wire-encode-merge-workspace-request (request)
  "Encode MergeWorkspaceRequest from plist REQUEST (:workspace REF)."
  (agent-repl--log nil "elisp.wire.verbs-encode-merge-workspace-request")
  (list (cons 'workspace
              (agent-repl-wire-encode-merge-workspace-request-workspace
               (agent-repl-wire-verbs--require "MergeWorkspaceRequest" "workspace"
                                                (plist-get request :workspace))))))

(defun agent-repl-wire-decode-merge-workspace-success (json)
  "Decode MergeWorkspaceSuccess from JSON.  Empty: success means ENQUEUED."
  (agent-repl-wire-verbs--decode-empty "MergeWorkspaceSuccess" json))

(defun agent-repl-wire-decode-merge-workspace-error (json)
  "Decode MergeWorkspaceError from JSON.  Empty until its arms are derived."
  (agent-repl-wire-verbs--decode-empty "MergeWorkspaceError" json))

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
  "Encode RestartWorkspaceRequest from plist REQUEST (:workspace REF :force BOOL).
`force' is spelled EXPLICITLY on the wire even when false: a forced
restart interrupts live work, so the request states the mode rather than
leaning on an omitted default."
  (agent-repl--log nil "elisp.wire.verbs-encode-restart-workspace-request force=%S"
                    (and (plist-get request :force) t))
  (list (cons 'workspace
              (agent-repl-wire-encode-restart-workspace-request-workspace
               (agent-repl-wire-verbs--require "RestartWorkspaceRequest" "workspace"
                                                (plist-get request :workspace))))
        (cons 'force (agent-repl-wire-verbs--encode-bool (plist-get request :force)))))

(defun agent-repl-wire-decode-restart-workspace-success (json)
  "Decode RestartWorkspaceSuccess from JSON.  Empty: the restart is accepted."
  (agent-repl-wire-verbs--decode-empty "RestartWorkspaceSuccess" json))

(defun agent-repl-wire-decode-restart-workspace-error (json)
  "Decode RestartWorkspaceError from JSON.  Empty until its arms are derived."
  (agent-repl-wire-verbs--decode-empty "RestartWorkspaceError" json))

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
    (agent-repl--log nil "elisp.wire.verbs-encode-set-workspace-priority-request cleared=%S"
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

(defun agent-repl-wire-decode-set-workspace-priority-error (json)
  "Decode SetWorkspacePriorityError from JSON.  Empty until its arms are derived."
  (agent-repl-wire-verbs--decode-empty "SetWorkspacePriorityError" json))

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

(defun agent-repl-wire-encode-submit-prompt-request (request)
  "Encode SubmitPromptRequest from plist REQUEST.
REQUEST is (:workspace REF :said SAID :idempotency-key STRING :origin
KEYWORD).  All four are required: the workspace names WHICH workspace the
submission belongs to and is echoed verbatim like every other
per-workspace request (it is required even though `feed' is not, because
the root feed has no id of its own); an empty idempotency key defeats the
duplicate refusal that makes a retry safe; and the origin is REQUIRED and
never UNSPECIFIED because a stored turn must trace back to the exact send
site.  `feed' is never set — Emacs composes into the workspace's root feed
only, so the absent field IS that fact."
  (let ((message "SubmitPromptRequest"))
    (agent-repl--log nil "elisp.wire.verbs-encode-submit-prompt-request origin=%s"
                      (plist-get request :origin))
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
                                                          (plist-get request :origin)))))))


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

(defun agent-repl-wire-decode-submit-prompt-success-turn (json)
  "Decode SubmitPromptSuccess's `turn' outcome arm from JSON."
  (agent-repl-wire-decode-submit-prompt-turn json))

(defun agent-repl-wire-decode-submit-prompt-success-command-panel (json)
  "Decode SubmitPromptSuccess's `command_panel' outcome arm from JSON."
  (agent-repl-wire-decode-submit-prompt-command-panel json))

(defun agent-repl-wire-decode-submit-prompt-success-command-refused (json)
  "Decode SubmitPromptSuccess's `command_refused' outcome arm from JSON."
  (agent-repl-wire-decode-submit-prompt-command-refused json))

(defun agent-repl-wire-decode-submit-prompt-success (json)
  "Decode SubmitPromptSuccess from JSON into (:arm ARM :value V).
All three arms are ANSWERS: a minted turn, a resolved panel, or a
recognized-but-unsupported command.  Only the turn arm means there is
anything to await."
  (let ((message "SubmitPromptSuccess"))
    (agent-repl-wire-verbs--check-keys message json '(turn commandPanel commandRefused))
    (agent-repl-wire-verbs--decode-oneof
     message "outcome" json
     (list (list 'turn :turn #'agent-repl-wire-decode-submit-prompt-success-turn)
           (list 'commandPanel :command-panel
                 #'agent-repl-wire-decode-submit-prompt-success-command-panel)
           (list 'commandRefused :command-refused
                 #'agent-repl-wire-decode-submit-prompt-success-command-refused)))))

(defun agent-repl-wire-decode-submit-prompt-refused-merging (json)
  "Decode SubmitPromptRefusedMerging from JSON.
Empty: the set arm IS the whole assertion — the footer and the merge
bubble already show which merge."
  (agent-repl-wire-verbs--decode-empty "SubmitPromptRefusedMerging" json))

(defun agent-repl-wire-decode-submit-prompt-error-merging (json)
  "Decode SubmitPromptError's `merging' reason arm from JSON."
  (agent-repl-wire-decode-submit-prompt-refused-merging json))

(defun agent-repl-wire-decode-submit-prompt-error (json)
  "Decode SubmitPromptError from JSON into (:reason (:arm ARM :value V))."
  (let ((message "SubmitPromptError"))
    (agent-repl-wire-verbs--check-keys message json '(merging))
    (list :reason
          (agent-repl-wire-verbs--decode-oneof
           message "reason" json
           (list (list 'merging :merging
                       #'agent-repl-wire-decode-submit-prompt-error-merging))))))

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

(defun agent-repl-wire-decode-update-shutdown-schedule-error (json)
  "Decode UpdateShutdownScheduleError from JSON.  Empty until its arms are derived."
  (agent-repl-wire-verbs--decode-empty "UpdateShutdownScheduleError" json))

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


;;;; ---- UpdateMergeQueue -----------------------------------------------

(defun agent-repl-wire-encode-update-merge-queue-pause (_value)
  "Encode UpdateMergeQueuePause.  Empty: the arm is the whole action."
  nil)

(defun agent-repl-wire-encode-update-merge-queue-resume (_value)
  "Encode UpdateMergeQueueResume.  Empty: the arm is the whole action."
  nil)

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

(defun agent-repl-wire-decode-update-merge-queue-error (json)
  "Decode UpdateMergeQueueError from JSON.  Empty until its arms are derived."
  (agent-repl-wire-verbs--decode-empty "UpdateMergeQueueError" json))

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
  (agent-repl--log nil "elisp.wire.verbs-encode-daemon-health-request")
  nil)

(defun agent-repl-wire-decode-daemon-fault (json)
  "Decode DaemonFault from JSON into (:detail STRING).
The kind oneof is not declared yet — it lands with its first derived arms
— so `detail' is the whole message today."
  (let ((message "DaemonFault"))
    (agent-repl-wire-verbs--check-keys message json '(detail))
    (list :detail (agent-repl-wire-verbs--decode-string message 'detail json))))

(defun agent-repl-wire-decode-daemon-unhealthy-faults (json)
  "Decode one element of DaemonUnhealthy's repeated `faults' use site from JSON."
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

(defun agent-repl-wire-decode-daemon-health-success-healthy (json)
  "Decode DaemonHealthSuccess's `healthy' verdict arm from JSON."
  (agent-repl-wire-decode-daemon-healthy json))

(defun agent-repl-wire-decode-daemon-health-success-unhealthy (json)
  "Decode DaemonHealthSuccess's `unhealthy' verdict arm from JSON."
  (agent-repl-wire-decode-daemon-unhealthy json))

(defun agent-repl-wire-decode-daemon-health-success (json)
  "Decode DaemonHealthSuccess from JSON into (:arm ARM :value V).
UNHEALTHY IS AN ANSWER: it arrives inside success, never as an error."
  (let ((message "DaemonHealthSuccess"))
    (agent-repl-wire-verbs--check-keys message json '(healthy unhealthy))
    (agent-repl-wire-verbs--decode-oneof
     message "health" json
     (list (list 'healthy :healthy #'agent-repl-wire-decode-daemon-health-success-healthy)
           (list 'unhealthy :unhealthy
                 #'agent-repl-wire-decode-daemon-health-success-unhealthy)))))

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
  (agent-repl--log nil "elisp.wire.verbs-encode-session-health-request")
  (list (cons 'workspace
              (agent-repl-wire-encode-session-health-request-workspace
               (agent-repl-wire-verbs--require "SessionHealthRequest" "workspace"
                                                (plist-get request :workspace))))))

(defun agent-repl-wire-decode-session-fault (json)
  "Decode SessionFault from JSON into (:detail STRING).
Deliberately NOT DaemonFault: a session's fault classes are the session
controller's own vocabulary."
  (let ((message "SessionFault"))
    (agent-repl-wire-verbs--check-keys message json '(detail))
    (list :detail (agent-repl-wire-verbs--decode-string message 'detail json))))

(defun agent-repl-wire-decode-session-unhealthy-faults (json)
  "Decode one element of SessionUnhealthy's repeated `faults' use site from JSON."
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

(defun agent-repl-wire-decode-session-health-error (json)
  "Decode SessionHealthError from JSON.
Empty until its arms are derived; error means the question could not be
ANSWERED (an unknown workspace), never that the session is unhealthy."
  (agent-repl-wire-verbs--decode-empty "SessionHealthError" json))

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

(provide 'agent-repl-wire-verbs)

;;; wire-verbs.el ends here
