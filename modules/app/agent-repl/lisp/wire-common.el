;;; wire-common.el --- Shared protojson codec vocabulary -*- lexical-binding: t; -*-

;;; Commentary:

;; THE PROTOJSON CODEC, SHARED LEAF.  Emacs speaks the same schema as the
;; webapp does, in JSON: Connect serves binary or protojson per client, and
;; elisp takes the JSON codec.  This file carries the pieces every other
;; `wire-*.el' needs — the error, the shared decode/encode primitives, and
;; the leaf vocabularies (workspace.v1 identities, the conversation.v1 user
;; message Emacs produces, PromptOrigin, DrainReason, WorkspacePriority,
;; TurnId).
;;
;; THE PROTOJSON SHAPE THIS CODE ASSUMES (Go's protojson, which is what the
;; daemon emits and accepts):
;;   - object keys are lowerCamelCase (`atMs', `shimAttached');
;;   - int64 is EMITTED as a decimal string and ACCEPTED as string or number;
;;   - uint32 is a number;
;;   - default-valued scalars are OMITTED (absent bool = false, absent string
;;     = "", absent number = 0);
;;   - an unset message field is omitted; a SET but empty message is `{}';
;;   - enums travel as their string names; repeated fields as arrays.
;;
;; THE ELISP SHAPE (fixed by the fanout spec, §2):
;;   - a decoded message is a plist with kebab-case keyword keys (`:at-ms',
;;     `:shim-attached');
;;   - a decoded oneof is `(:arm KEYWORD :value V)', where V is the decoded
;;     arm message and nil for an empty arm;
;;   - a repeated field is a list; an absent optional field is nil;
;;   - an empty message is nil, which `json-serialize' renders as `{}'.
;;
;; VALIDATION LIVES ONCE PER MESSAGE.  Every message has ONE base
;; decode/encode function that validates it; every non-primitive USE SITE (a
;; message-typed field, a oneof arm) has its own dedicated function
;; delegating to the child's base.  Where the use-site name computed from
;; `<parent-message-kebab>-<field-kebab>' is IDENTICAL to the child's base
;; name — which the proto's own naming makes the common case, e.g. RosterRow's
;; `when' field holding a `RosterRowWhen' — the base function IS the use-site
;; function, and no synonym is defined.
;;
;; Primitives get no wrappers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--error "core")
(declare-function agent-repl--log "core")

;;;; ---- The contract-breach error ----

(define-error 'agent-repl-wire-error
  "agent-repl wire contract breach")

(defun agent-repl-wire--fail (message-name field reason)
  "Log an ERROR and signal `agent-repl-wire-error' for MESSAGE-NAME.
FIELD names the offending field (or oneof); REASON says what is wrong.
The signalled data is `(MESSAGE-NAME FIELD REASON)'.

THE ONE PLACE a wire contract breach becomes a signal.  Every codec file
routes here (`wire-verbs.el' keeps a same-named thin wrapper for
readability at its own call sites), so the breach path is a single
function rather than a shape re-derived per file.

The two acts are separate and both happen, in order: `agent-repl--error'
RECORDS the breach on the durable sink at ERROR level, and the `signal'
then ABORTS the caller with the TYPED error every wire consumer catches —
carrying which message, which field, and why.  There is no
`condition-case' anywhere in the codec: `agent-repl--error' is a pure
logging rung that never signals, so nothing here can swallow anything."
  (agent-repl--error '(:agent-repl-context "a codec call outside a request has no workspace") "elisp.wire.contract-breach message=%s field=%s reason=%s"
                     message-name field reason)
  (signal 'agent-repl-wire-error (list message-name field reason)))

(defun agent-repl-wire--decoded (message-name value)
  "Log a successful decode of MESSAGE-NAME at debug and return VALUE."
  (agent-repl--log '(:agent-repl-context "a codec call outside a request has no workspace") "elisp.wire.decoded message=%s" message-name)
  value)

(defun agent-repl-wire--encoded (message-name value)
  "Log a successful encode of MESSAGE-NAME at debug and return VALUE."
  (agent-repl--log '(:agent-repl-context "a codec call outside a request has no workspace") "elisp.wire.encoded message=%s" message-name)
  value)

;;;; ---- Shared decode primitives ----

(defun agent-repl-wire--object (message-name value)
  "Return VALUE as a protojson object alist for MESSAGE-NAME.
`json-parse-string' with `:object-type' `alist' yields nil for `{}', so an
empty object and an empty message are the same value here."
  (cond
   ((null value) nil)
   ((eq value :null) nil)
   ((and (consp value) (consp (car value)) (symbolp (caar value))) value)
   (t (agent-repl-wire--fail message-name '- "expected a JSON object"))))

(defun agent-repl-wire--raw (object key)
  "Return OBJECT's KEY value, or nil when absent or JSON null."
  (let ((cell (assq key object)))
    (when (and cell (not (eq (cdr cell) :null)))
      (cdr cell))))

(defun agent-repl-wire--present-p (object key)
  "Return non-nil when OBJECT carries KEY with a non-null value."
  (let ((cell (assq key object)))
    (and cell (not (eq (cdr cell) :null)))))

(defun agent-repl-wire--check-keys (message-name object allowed)
  "Refuse any key of OBJECT that is not in ALLOWED, for MESSAGE-NAME.
The same strictness a generated client has: an unknown field is a schema
the consumer does not hold, and drawing from it would be a guess."
  (dolist (cell object)
    (unless (memq (car cell) allowed)
      (agent-repl-wire--fail message-name (car cell) "unknown field")))
  object)

(defun agent-repl-wire--decode-empty (message-name value)
  "Decode VALUE as the empty message MESSAGE-NAME, returning nil."
  (agent-repl-wire--check-keys
   message-name (agent-repl-wire--object message-name value) nil)
  nil)

(defun agent-repl-wire--decode-string (message-name field object)
  "Decode OBJECT's non-optional string FIELD of MESSAGE-NAME.
An absent field is the proto3 default, which protojson omits."
  (let ((raw (agent-repl-wire--raw object field)))
    (cond ((null raw) "")
          ((stringp raw) raw)
          (t (agent-repl-wire--fail message-name field "expected a string")))))

(defun agent-repl-wire--decode-optional-string (message-name field object)
  "Decode OBJECT's optional string FIELD of MESSAGE-NAME, nil when absent."
  (let ((raw (agent-repl-wire--raw object field)))
    (cond ((null raw) nil)
          ((stringp raw) raw)
          (t (agent-repl-wire--fail message-name field "expected a string")))))

(defun agent-repl-wire--decode-bool (message-name field object)
  "Decode OBJECT's non-optional bool FIELD of MESSAGE-NAME as t or nil."
  (let ((cell (assq field object)))
    (cond ((null cell) nil)
          ((eq (cdr cell) t) t)
          ((memq (cdr cell) '(:false :null)) nil)
          (t (agent-repl-wire--fail message-name field "expected a boolean")))))

(defun agent-repl-wire--parse-integer (message-name field raw)
  "Return RAW as an integer for MESSAGE-NAME's FIELD.
protojson EMITS int64 as a decimal string and ACCEPTS either spelling, so
both are honored here."
  (cond
   ((integerp raw) raw)
   ((and (stringp raw) (string-match-p "\\`-?[0-9]+\\'" raw))
    (string-to-number raw))
   (t (agent-repl-wire--fail message-name field "expected an integer"))))

(defun agent-repl-wire--decode-int64 (message-name field object)
  "Decode OBJECT's non-optional int64 FIELD of MESSAGE-NAME."
  (let ((raw (agent-repl-wire--raw object field)))
    (if (null raw) 0 (agent-repl-wire--parse-integer message-name field raw))))

(defun agent-repl-wire--decode-int32 (message-name field object)
  "Decode OBJECT's non-optional int32 FIELD of MESSAGE-NAME.
protojson spells int32 as a number, but the same decimal-string spelling
int64 uses is accepted rather than refused; an absent field is the proto3
default 0, which protojson omits."
  (let ((raw (agent-repl-wire--raw object field)))
    (if (null raw) 0 (agent-repl-wire--parse-integer message-name field raw))))

(defun agent-repl-wire--decode-uint32 (message-name field object)
  "Decode OBJECT's non-optional uint32 FIELD of MESSAGE-NAME.
An absent field is the proto3 default 0, which protojson omits; a
negative value is a contract breach for an unsigned field."
  (let ((raw (agent-repl-wire--raw object field)))
    (if (null raw)
        0
      (let ((n (agent-repl-wire--parse-integer message-name field raw)))
        (when (< n 0)
          (agent-repl-wire--fail message-name field "expected a non-negative integer"))
        n))))

(defun agent-repl-wire--decode-uint64 (message-name field object)
  "Decode OBJECT's non-optional uint64 FIELD of MESSAGE-NAME.
protojson EMITS a 64-bit integer as a decimal string; an absent field is
the proto3 default 0, and a negative value is a breach for an unsigned
field."
  (let ((raw (agent-repl-wire--raw object field)))
    (if (null raw)
        0
      (let ((n (agent-repl-wire--parse-integer message-name field raw)))
        (when (< n 0)
          (agent-repl-wire--fail message-name field "expected a non-negative integer"))
        n))))

(defun agent-repl-wire--decode-optional-uint32 (message-name field object)
  "Decode OBJECT's optional uint32 FIELD of MESSAGE-NAME, nil when absent."
  (let ((raw (agent-repl-wire--raw object field)))
    (when raw
      (let ((n (agent-repl-wire--parse-integer message-name field raw)))
        (when (< n 0)
          (agent-repl-wire--fail message-name field "expected a non-negative integer"))
        n))))

(defun agent-repl-wire--decode-double (message-name field object)
  "Decode OBJECT's non-optional double FIELD of MESSAGE-NAME as a float.
protojson spells a double as a JSON number, but also ACCEPTS the decimal
string spelling, so both are honored here; an absent field is the proto3
default 0.0, which protojson omits.  A non-finite spelling
\(\"NaN\"/\"Infinity\") is a contract breach for a scale field and is
refused rather than silently coerced."
  (let ((raw (agent-repl-wire--raw object field)))
    (cond
     ((null raw) 0.0)
     ((numberp raw) (float raw))
     ((and (stringp raw) (string-match-p "\\`-?[0-9]+\\(\\.[0-9]+\\)?\\([eE][-+]?[0-9]+\\)?\\'" raw))
      (float (string-to-number raw)))
     (t (agent-repl-wire--fail message-name field "expected a double")))))

(defun agent-repl-wire--decode-message (message-name field object decoder)
  "Decode OBJECT's REQUIRED message FIELD of MESSAGE-NAME with DECODER.
An absent non-optional message field is a contract breach: the validation
invariant says a push carrying one makes the consumer raise, loudly."
  (unless (agent-repl-wire--present-p object field)
    (agent-repl-wire--fail message-name field "required message field is absent"))
  (funcall decoder (agent-repl-wire--raw object field)))

(defun agent-repl-wire--decode-optional-message (message-name field object decoder)
  "Decode OBJECT's OPTIONAL message FIELD of MESSAGE-NAME with DECODER.
Returns nil when the field is absent — presence is the fact."
  (ignore message-name)
  (when (agent-repl-wire--present-p object field)
    (funcall decoder (agent-repl-wire--raw object field))))

(defun agent-repl-wire--decode-repeated (message-name field object decoder)
  "Decode OBJECT's repeated message FIELD of MESSAGE-NAME with DECODER."
  (let ((raw (agent-repl-wire--raw object field)))
    (cond ((null raw) nil)
          ((listp raw) (mapcar decoder raw))
          (t (agent-repl-wire--fail message-name field "expected an array")))))

(defun agent-repl-wire--decode-oneof (message-name oneof object arms &optional unset-legal)
  "Decode the ONEOF of MESSAGE-NAME from OBJECT.
ARMS is a list of (WIRE-KEY ARM-KEYWORD DECODER).  Exactly one arm must be
set; zero is an error unless UNSET-LEGAL, and two is always an error.  An
unknown arm never reaches here — it is refused as an unknown field by the
message's own key check, which is the same breach seen from the key side."
  (let (set)
    (dolist (arm arms)
      (when (agent-repl-wire--present-p object (nth 0 arm))
        (push arm set)))
    (cond
     ((null set)
      (if unset-legal
          nil
        (agent-repl-wire--fail message-name oneof "oneof is unset")))
     ((cdr set)
      (agent-repl-wire--fail message-name oneof "oneof has more than one arm set"))
     (t
      (let ((arm (car set)))
        (list :arm (nth 1 arm)
              :value (funcall (nth 2 arm)
                              (agent-repl-wire--raw object (nth 0 arm)))))))))

;;;; ---- Shared encode primitives ----

(defun agent-repl-wire--encode-string (message-name field value)
  "Return VALUE as an encodable string for MESSAGE-NAME's FIELD."
  (cond ((stringp value) value)
        (t (agent-repl-wire--fail message-name field "expected a string"))))

(defun agent-repl-wire--encode-repeated (message-name field values encoder)
  "Return VALUES encoded with ENCODER as a vector, for MESSAGE-NAME's FIELD."
  (unless (listp values)
    (agent-repl-wire--fail message-name field "expected a list"))
  (vconcat (mapcar encoder values)))

(defun agent-repl-wire--encode-oneof (message-name oneof value arms)
  "Encode the ONEOF VALUE of MESSAGE-NAME into a one-cell alist.
VALUE is `(:arm KEYWORD :value V)'.  ARMS is a list of (ARM-KEYWORD
WIRE-KEY ENCODER).  An unset oneof and an unknown arm are both refused: a
request is built only from complete values."
  (unless (and (listp value) (plist-member value :arm))
    (agent-repl-wire--fail message-name oneof "oneof is unset"))
  (let* ((keyword (plist-get value :arm))
         (arm (assq keyword arms)))
    (unless arm
      (agent-repl-wire--fail message-name oneof "unknown oneof arm"))
    (list (cons (nth 1 arm) (funcall (nth 2 arm) (plist-get value :value))))))

(defun agent-repl-wire--encode-empty (message-name value)
  "Encode VALUE as the empty message MESSAGE-NAME: nil, which is `{}'."
  (when value
    (agent-repl-wire--fail message-name '- "expected an empty message"))
  nil)

;;;; ---- workspace.v1.WorkspaceRef ----

(defun agent-repl-wire-decode-workspace-ref (value)
  "Decode VALUE as a `workspace.v1.WorkspaceRef' plist `(:id :dir)'."
  (let ((object (agent-repl-wire--object "WorkspaceRef" value)))
    (agent-repl-wire--check-keys "WorkspaceRef" object '(id dir))
    (agent-repl-wire--decoded
     "WorkspaceRef"
     (list :id (agent-repl-wire--decode-string "WorkspaceRef" 'id object)
           :dir (agent-repl-wire--decode-string "WorkspaceRef" 'dir object)))))

(defun agent-repl-wire-encode-workspace-ref (value)
  "Encode the WorkspaceRef plist VALUE as a protojson alist.
The ref is a daemon-minted echo token: both halves travel back verbatim."
  (agent-repl-wire--encoded
   "WorkspaceRef"
   (list (cons 'id (agent-repl-wire--encode-string
                    "WorkspaceRef" 'id (plist-get value :id)))
         (cons 'dir (agent-repl-wire--encode-string
                     "WorkspaceRef" 'dir (plist-get value :dir))))))

;;;; ---- workspace.v1.RepositoryRef ----

(defun agent-repl-wire-decode-repository-ref (value)
  "Decode VALUE as a `workspace.v1.RepositoryRef' plist `(:id :dir)'."
  (let ((object (agent-repl-wire--object "RepositoryRef" value)))
    (agent-repl-wire--check-keys "RepositoryRef" object '(id dir))
    (agent-repl-wire--decoded
     "RepositoryRef"
     (list :id (agent-repl-wire--decode-string "RepositoryRef" 'id object)
           :dir (agent-repl-wire--decode-string "RepositoryRef" 'dir object)))))

(defun agent-repl-wire-encode-repository-ref (value)
  "Encode the RepositoryRef plist VALUE as a protojson alist."
  (agent-repl-wire--encoded
   "RepositoryRef"
   (list (cons 'id (agent-repl-wire--encode-string
                    "RepositoryRef" 'id (plist-get value :id)))
         (cons 'dir (agent-repl-wire--encode-string
                     "RepositoryRef" 'dir (plist-get value :dir))))))

;;;; ---- conversation.v1.TurnId ----

(defun agent-repl-wire-decode-turn-id (value)
  "Decode VALUE as a `conversation.v1.TurnId' plist `(:value)'."
  (let ((object (agent-repl-wire--object "TurnId" value)))
    (agent-repl-wire--check-keys "TurnId" object '(value))
    (agent-repl-wire--decoded
     "TurnId"
     (list :value (agent-repl-wire--decode-string "TurnId" 'value object)))))

(defun agent-repl-wire-encode-turn-id (value)
  "Encode the TurnId plist VALUE `(:value)' as a protojson alist.
The id is an ECHO TOKEN: the daemon-minted value travels back verbatim,
never one Emacs builds itself."
  (agent-repl-wire--encoded
   "TurnId"
   (list (cons 'value (agent-repl-wire--encode-string
                       "TurnId" 'value (plist-get value :value))))))

;;;; ---- frontend.v1.FeedId ----

(defun agent-repl-wire-decode-feed-id (value)
  "Decode VALUE as a `frontend.v1.FeedId' plist `(:value)'.
The daemon-minted opaque row identity: clients echo it (navigation,
parenting, a footer jump target, the reply-to-a-past-response cursor) and
never parse it, so the codec keeps the value verbatim."
  (let ((object (agent-repl-wire--object "FeedId" value)))
    (agent-repl-wire--check-keys "FeedId" object '(value))
    (agent-repl-wire--decoded
     "FeedId"
     (list :value (agent-repl-wire--decode-string "FeedId" 'value object)))))

(defun agent-repl-wire-encode-feed-id (value)
  "Encode the FeedId plist VALUE `(:value)' as a protojson alist.
The id is an ECHO TOKEN: the opaque value the daemon minted travels back
verbatim, never one Emacs builds itself.  A non-string value is refused
here rather than sent for the daemon to reject."
  (agent-repl-wire--encoded
   "FeedId"
   (list (cons 'value (agent-repl-wire--encode-string
                       "FeedId" 'value (plist-get value :value))))))

;;;; ---- conversation.v1 user content ----
;;
;; Emacs PRODUCES a user message, and consumes exactly one: the held prompt
;; the host view hands back while it is being edited
;; (`HostHeldPromptEdit.said'), which is put into the composer.  The feed is
;; the webview's surface, so that is the decode half's only reader.

(defun agent-repl-wire-encode-text-block (value)
  "Encode the TextBlock plist VALUE `(:text)' as a protojson alist."
  (agent-repl-wire--encoded
   "TextBlock"
   (list (cons 'text (agent-repl-wire--encode-string
                      "TextBlock" 'text (plist-get value :text))))))

(defun agent-repl-wire-encode-image-block-path (value)
  "Encode the ImageBlockPath plist VALUE `(:path)' as a protojson alist."
  (agent-repl-wire--encoded
   "ImageBlockPath"
   (list (cons 'path (agent-repl-wire--encode-string
                      "ImageBlockPath" 'path (plist-get value :path))))))

(defun agent-repl-wire-encode-image-block-url (value)
  "Encode the ImageBlockUrl plist VALUE `(:url)' as a protojson alist."
  (agent-repl-wire--encoded
   "ImageBlockUrl"
   (list (cons 'url (agent-repl-wire--encode-string
                     "ImageBlockUrl" 'url (plist-get value :url))))))

(defun agent-repl-wire-encode-image-block-location (value)
  "Encode ImageBlock's `location' oneof VALUE.
WHICH kind of reference an image is travels by ARM, never sniffed from a
string, so the arm is required."
  (agent-repl-wire--encode-oneof
   "ImageBlock" 'location value
   ;; `path' and `url' each name a message whose own base encoder carries
   ;; the identical use-site name, so the base IS the use site.
   '((:path path agent-repl-wire-encode-image-block-path)
     (:url url agent-repl-wire-encode-image-block-url))))

(defun agent-repl-wire-encode-image-block (value)
  "Encode the ImageBlock plist VALUE `(:location :media-type)'."
  (agent-repl-wire--encoded
   "ImageBlock"
   (append
    (agent-repl-wire-encode-image-block-location (plist-get value :location))
    (list (cons 'mediaType (agent-repl-wire--encode-string
                            "ImageBlock" 'media_type
                            (plist-get value :media-type)))))))

(defun agent-repl-wire-encode-user-content-block-text (value)
  "Encode a UserContentBlock `text' arm VALUE as a TextBlock."
  (agent-repl-wire-encode-text-block value))

(defun agent-repl-wire-encode-user-content-block-image (value)
  "Encode a UserContentBlock `image' arm VALUE as an ImageBlock."
  (agent-repl-wire-encode-image-block value))

(defun agent-repl-wire-encode-user-content-block-block (value)
  "Encode UserContentBlock's `block' oneof VALUE.
The `unsupported' arm has no encoder ON PURPOSE: it is not a fallback, and
Emacs — which knows exactly what it composed — can never legitimately
produce one.  Naming it is therefore an unknown arm."
  (agent-repl-wire--encode-oneof
   "UserContentBlock" 'block value
   '((:text text agent-repl-wire-encode-user-content-block-text)
     (:image image agent-repl-wire-encode-user-content-block-image))))

(defun agent-repl-wire-encode-user-content-block (value)
  "Encode the UserContentBlock plist VALUE `(:arm :value)'."
  (agent-repl-wire--encoded
   "UserContentBlock"
   (agent-repl-wire-encode-user-content-block-block value)))

(defun agent-repl-wire-encode-user-content-blocks (values)
  "Encode UserContent's repeated `blocks' field VALUES."
  (agent-repl-wire--encode-repeated
   "UserContent" 'blocks values #'agent-repl-wire-encode-user-content-block))

(defun agent-repl-wire-encode-user-content (value)
  "Encode the UserContent plist VALUE `(:blocks)'.
Blocks travel in the order the person composed them, which is the order
they are shown."
  (agent-repl-wire--encoded
   "UserContent"
   (list (cons 'blocks (agent-repl-wire-encode-user-content-blocks
                        (plist-get value :blocks))))))

(defun agent-repl-wire-encode-user-said-content (value)
  "Encode UserSaid's `content' field VALUE as a UserContent."
  (agent-repl-wire-encode-user-content value))

(defun agent-repl-wire-encode-user-said (value)
  "Encode the UserSaid plist VALUE `(:content)'.
`content' is not optional: what a person said is the whole message."
  (unless (plist-member value :content)
    (agent-repl-wire--fail "UserSaid" 'content "required message field is absent"))
  (agent-repl-wire--encoded
   "UserSaid"
   (list (cons 'content (agent-repl-wire-encode-user-said-content
                         (plist-get value :content))))))

;;;; ---- conversation.v1 user content (DECODE, for a held-prompt edit) ----

(defun agent-repl-wire-decode-text-block (value)
  "Decode VALUE as a `TextBlock' plist `(:text)'."
  (let ((object (agent-repl-wire--object "TextBlock" value)))
    (agent-repl-wire--check-keys "TextBlock" object '(text))
    (agent-repl-wire--decoded
     "TextBlock"
     (list :text (agent-repl-wire--decode-string "TextBlock" 'text object)))))

(defun agent-repl-wire-decode-image-block-path (value)
  "Decode VALUE as an `ImageBlockPath' plist `(:path)'."
  (let ((object (agent-repl-wire--object "ImageBlockPath" value)))
    (agent-repl-wire--check-keys "ImageBlockPath" object '(path))
    (agent-repl-wire--decoded
     "ImageBlockPath"
     (list :path (agent-repl-wire--decode-string "ImageBlockPath" 'path object)))))

(defun agent-repl-wire-decode-image-block-url (value)
  "Decode VALUE as an `ImageBlockUrl' plist `(:url)'."
  (let ((object (agent-repl-wire--object "ImageBlockUrl" value)))
    (agent-repl-wire--check-keys "ImageBlockUrl" object '(url))
    (agent-repl-wire--decoded
     "ImageBlockUrl"
     (list :url (agent-repl-wire--decode-string "ImageBlockUrl" 'url object)))))

(defun agent-repl-wire-decode-image-block-location (object)
  "Decode `ImageBlock''s `location' oneof from OBJECT.
WHICH kind of reference is stated by arm, so an unset location is a breach."
  (agent-repl-wire--decode-oneof
   "ImageBlock" 'location object
   '((path :path agent-repl-wire-decode-image-block-path)
     (url :url agent-repl-wire-decode-image-block-url))))

(defun agent-repl-wire-decode-image-block (value)
  "Decode VALUE as an `ImageBlock' plist `(:location :media-type)'."
  (let ((object (agent-repl-wire--object "ImageBlock" value)))
    (agent-repl-wire--check-keys "ImageBlock" object '(path url mediaType))
    (agent-repl-wire--decoded
     "ImageBlock"
     (list :location (agent-repl-wire-decode-image-block-location object)
           :media-type (agent-repl-wire--decode-string "ImageBlock" 'mediaType object)))))

(defun agent-repl-wire-decode-unsupported-block (value)
  "Decode VALUE as an `UnsupportedBlock' plist `(:kind :raw)'.
RAW is the untyped Struct, kept verbatim: nothing is drawn from it."
  (let ((object (agent-repl-wire--object "UnsupportedBlock" value)))
    (agent-repl-wire--check-keys "UnsupportedBlock" object '(kind raw))
    (agent-repl-wire--decoded
     "UnsupportedBlock"
     (list :kind (agent-repl-wire--decode-string "UnsupportedBlock" 'kind object)
           :raw (agent-repl-wire--raw object 'raw)))))

(defun agent-repl-wire-decode-user-content-block (value)
  "Decode VALUE as a `UserContentBlock', the oneof `(:arm :value)'."
  (let ((object (agent-repl-wire--object "UserContentBlock" value)))
    (agent-repl-wire--check-keys "UserContentBlock" object '(text image unsupported))
    (agent-repl-wire--decoded
     "UserContentBlock"
     (agent-repl-wire--decode-oneof
      "UserContentBlock" 'block object
      '((text :text agent-repl-wire-decode-text-block)
        (image :image agent-repl-wire-decode-image-block)
        (unsupported :unsupported agent-repl-wire-decode-unsupported-block))))))

(defun agent-repl-wire-decode-user-content (value)
  "Decode VALUE as a `UserContent' plist `(:blocks)', blocks in order."
  (let ((object (agent-repl-wire--object "UserContent" value)))
    (agent-repl-wire--check-keys "UserContent" object '(blocks))
    (agent-repl-wire--decoded
     "UserContent"
     (list :blocks (agent-repl-wire--decode-repeated
                    "UserContent" 'blocks object
                    #'agent-repl-wire-decode-user-content-block)))))

(defun agent-repl-wire-decode-user-said-content (value)
  "Decode UserSaid's `content' field VALUE as a UserContent."
  (agent-repl-wire-decode-user-content value))

(defun agent-repl-wire-decode-user-said (value)
  "Decode VALUE as a `UserSaid' plist `(:content)'.
`content' is REQUIRED: what a person said is the whole message."
  (let ((object (agent-repl-wire--object "UserSaid" value)))
    (agent-repl-wire--check-keys "UserSaid" object '(content))
    (agent-repl-wire--decoded
     "UserSaid"
     (list :content (agent-repl-wire--decode-message
                     "UserSaid" 'content object
                     #'agent-repl-wire-decode-user-said-content)))))

;;;; ---- conversation.v1.PromptOrigin (ENCODE only) ----

(defconst agent-repl-wire-prompt-origins
  '((:user-sent . "PROMPT_ORIGIN_USER_SENT")
    (:user-sent-and-hide . "PROMPT_ORIGIN_USER_SENT_AND_HIDE")
    (:user-sent-with-metaprompt . "PROMPT_ORIGIN_USER_SENT_WITH_METAPROMPT")
    (:user-sent-with-postfix . "PROMPT_ORIGIN_USER_SENT_WITH_POSTFIX")
    (:user-sent-with-prefix . "PROMPT_ORIGIN_USER_SENT_WITH_PREFIX")
    (:metaprompt-read . "PROMPT_ORIGIN_METAPROMPT_READ")
    (:command-diff-analysis . "PROMPT_ORIGIN_COMMAND_DIFF_ANALYSIS")
    (:command-explain-context . "PROMPT_ORIGIN_COMMAND_EXPLAIN_CONTEXT")
    (:command-explain-prompt . "PROMPT_ORIGIN_COMMAND_EXPLAIN_PROMPT")
    (:command-update-pr . "PROMPT_ORIGIN_COMMAND_UPDATE_PR")
    (:command-rebase . "PROMPT_ORIGIN_COMMAND_REBASE")
    (:command-create-or-update-pr . "PROMPT_ORIGIN_COMMAND_CREATE_OR_UPDATE_PR")
    (:panel-selection . "PROMPT_ORIGIN_PANEL_SELECTION")
    (:deferred-prompt . "PROMPT_ORIGIN_DEFERRED_PROMPT")
    (:legacy-host-prompt . "PROMPT_ORIGIN_LEGACY_HOST_PROMPT")
    (:gns-sockets-close . "PROMPT_ORIGIN_GNS_SOCKETS_CLOSE")
    (:legacy-host-eval-result . "PROMPT_ORIGIN_LEGACY_HOST_EVAL_RESULT")
    (:explain-config . "PROMPT_ORIGIN_EXPLAIN_CONFIG")
    (:webapp-user-sent . "PROMPT_ORIGIN_WEBAPP_USER_SENT")
    (:webapp-card-action . "PROMPT_ORIGIN_WEBAPP_CARD_ACTION")
    (:workspace-created . "PROMPT_ORIGIN_WORKSPACE_CREATED")
    (:merge-conflict-repair . "PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR")
    (:merge-test-repair . "PROMPT_ORIGIN_MERGE_TEST_REPAIR")
    (:merge-before-action . "PROMPT_ORIGIN_MERGE_BEFORE_ACTION")
    (:merge-after-action . "PROMPT_ORIGIN_MERGE_AFTER_ACTION")
    (:merge-displaced-turn-resume . "PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME")
    (:resume-after-restart . "PROMPT_ORIGIN_RESUME_AFTER_RESTART"))
  "The whole `conversation.v1.PromptOrigin' vocabulary, keyword to wire name.
UNSPECIFIED is deliberately ABSENT: every send site must choose a real
value, so the zero value has no elisp spelling to reach for by accident.
A test pins this table against the checked-in Go bindings, so an enum
value landed in the proto without a keyword here fails loudly.")

(defun agent-repl-wire-encode-prompt-origin (value)
  "Encode the PromptOrigin keyword VALUE as its protojson enum name.
The vocabulary is CLOSED and durable — a stored turn is traced back to the
exact editor situation that caused it — so an unknown keyword, and
`:unspecified' in particular, is refused before the request is built."
  (let ((name (cdr (assq value agent-repl-wire-prompt-origins))))
    (unless name
      (agent-repl-wire--fail "PromptOrigin" 'origin "unknown prompt origin"))
    (agent-repl-wire--encoded "PromptOrigin" name)))

;;;; ---- agentrepl.v1.DrainReason ----

(defun agent-repl-wire-decode-drain-reason-deploy (value)
  "Decode VALUE as the empty message `DrainReasonDeploy'."
  (agent-repl-wire--decode-empty "DrainReasonDeploy" value))

(defun agent-repl-wire-decode-drain-reason-maintenance (value)
  "Decode VALUE as the empty message `DrainReasonMaintenance'."
  (agent-repl-wire--decode-empty "DrainReasonMaintenance" value))

(defun agent-repl-wire-decode-drain-reason-operator (value)
  "Decode VALUE as `DrainReasonOperator', a plist `(:note)'."
  (let ((object (agent-repl-wire--object "DrainReasonOperator" value)))
    (agent-repl-wire--check-keys "DrainReasonOperator" object '(note))
    (agent-repl-wire--decoded
     "DrainReasonOperator"
     (list :note (agent-repl-wire--decode-string
                  "DrainReasonOperator" 'note object)))))

(defun agent-repl-wire-decode-drain-reason-kind (value)
  "Decode DrainReason's `kind' oneof from VALUE.
THE ARM IS THE REASON — never a bare string; the detail lives inside the
one arm that owns it."
  (let ((object (agent-repl-wire--object "DrainReason" value)))
    (agent-repl-wire--check-keys "DrainReason" object '(deploy maintenance operator))
    (agent-repl-wire--decode-oneof
     "DrainReason" 'kind object
     '((deploy :deploy agent-repl-wire-decode-drain-reason-deploy)
       (maintenance :maintenance agent-repl-wire-decode-drain-reason-maintenance)
       (operator :operator agent-repl-wire-decode-drain-reason-operator)))))

(defun agent-repl-wire-decode-drain-reason (value)
  "Decode VALUE as a `DrainReason' oneof plist `(:arm :value)'."
  (agent-repl-wire--decoded
   "DrainReason" (agent-repl-wire-decode-drain-reason-kind value)))

(defun agent-repl-wire-encode-drain-reason-deploy (value)
  "Encode the empty message `DrainReasonDeploy' from VALUE."
  (agent-repl-wire--encode-empty "DrainReasonDeploy" value))

(defun agent-repl-wire-encode-drain-reason-maintenance (value)
  "Encode the empty message `DrainReasonMaintenance' from VALUE."
  (agent-repl-wire--encode-empty "DrainReasonMaintenance" value))

(defun agent-repl-wire-encode-drain-reason-operator (value)
  "Encode the DrainReasonOperator plist VALUE `(:note)'.
The note is REQUIRED non-blank; a blank note is refused at the request
rather than sent for the daemon to reject."
  (let ((note (agent-repl-wire--encode-string
               "DrainReasonOperator" 'note (plist-get value :note))))
    (when (string-blank-p note)
      (agent-repl-wire--fail "DrainReasonOperator" 'note "operator note is blank"))
    (agent-repl-wire--encoded "DrainReasonOperator" (list (cons 'note note)))))

(defun agent-repl-wire-encode-drain-reason-kind (value)
  "Encode DrainReason's `kind' oneof VALUE."
  (agent-repl-wire--encode-oneof
   "DrainReason" 'kind value
   '((:deploy deploy agent-repl-wire-encode-drain-reason-deploy)
     (:maintenance maintenance agent-repl-wire-encode-drain-reason-maintenance)
     (:operator operator agent-repl-wire-encode-drain-reason-operator))))

(defun agent-repl-wire-encode-drain-reason (value)
  "Encode the DrainReason oneof plist VALUE `(:arm :value)'."
  (agent-repl-wire--encoded
   "DrainReason" (agent-repl-wire-encode-drain-reason-kind value)))

;;;; ---- agentrepl.v1.WorkspacePriority (ENCODE only) ----

(defun agent-repl-wire-encode-workspace-priority-p05 (value)
  "Encode the empty message `WorkspacePriorityP05' from VALUE."
  (agent-repl-wire--encode-empty "WorkspacePriorityP05" value))

(defun agent-repl-wire-encode-workspace-priority-p1 (value)
  "Encode the empty message `WorkspacePriorityP1' from VALUE."
  (agent-repl-wire--encode-empty "WorkspacePriorityP1" value))

(defun agent-repl-wire-encode-workspace-priority-p2 (value)
  "Encode the empty message `WorkspacePriorityP2' from VALUE."
  (agent-repl-wire--encode-empty "WorkspacePriorityP2" value))

(defun agent-repl-wire-encode-workspace-priority-p3 (value)
  "Encode the empty message `WorkspacePriorityP3' from VALUE."
  (agent-repl-wire--encode-empty "WorkspacePriorityP3" value))

(defun agent-repl-wire-encode-workspace-priority-level (value)
  "Encode WorkspacePriority's `level' oneof VALUE — highest arm first."
  (agent-repl-wire--encode-oneof
   "WorkspacePriority" 'level value
   '((:p05 p05 agent-repl-wire-encode-workspace-priority-p05)
     (:p1 p1 agent-repl-wire-encode-workspace-priority-p1)
     (:p2 p2 agent-repl-wire-encode-workspace-priority-p2)
     (:p3 p3 agent-repl-wire-encode-workspace-priority-p3))))

(defun agent-repl-wire-encode-workspace-priority (value)
  "Encode the WorkspacePriority oneof plist VALUE `(:arm :value)'."
  (agent-repl-wire--encoded
   "WorkspacePriority" (agent-repl-wire-encode-workspace-priority-level value)))

;;;; ---- agentrepl.v1 SessionFault arm messages (shared leaf) ----
;;
;; The thirteen fault classes the session controller mints.  They are declared
;; once in the proto and carried by TWO parents — `SessionFault' on the
;; SessionHealth response (wire-verbs.el) and `HostFault' on the host stream
;; (wire-host.el) — because a session's fault classes do not change with the
;; stream that reports them.  Validation lives ONCE per message, so the base
;; decoders live here, in the leaf both files already lean on; each parent
;; keeps its own use-site wrappers and its own `kind' oneof decoder.

(defun agent-repl-wire-decode-session-fault-shim-start-failed (value)
  "Decode VALUE as `SessionFaultShimStartFailed', a plist (`:exit-code'
`:stderr-tail')."
  (let ((object (agent-repl-wire--object "SessionFaultShimStartFailed" value)))
    (agent-repl-wire--check-keys "SessionFaultShimStartFailed" object '(exitCode stderrTail))
    (agent-repl-wire--decoded
     "SessionFaultShimStartFailed"
     (list :exit-code (agent-repl-wire--decode-int32
                    "SessionFaultShimStartFailed" 'exitCode object)
           :stderr-tail (agent-repl-wire--decode-string
                    "SessionFaultShimStartFailed" 'stderrTail object)))))

(defun agent-repl-wire-decode-session-fault-shim-died (value)
  "Decode VALUE as `SessionFaultShimDied', a plist (`:exit-code')."
  (let ((object (agent-repl-wire--object "SessionFaultShimDied" value)))
    (agent-repl-wire--check-keys "SessionFaultShimDied" object '(exitCode))
    (agent-repl-wire--decoded
     "SessionFaultShimDied"
     (list :exit-code (agent-repl-wire--decode-int32
                    "SessionFaultShimDied" 'exitCode object)))))

(defun agent-repl-wire-decode-session-fault-link-severed (value)
  "Decode VALUE as the empty message `SessionFaultLinkSevered'."
  (agent-repl-wire--decode-empty "SessionFaultLinkSevered" value))

(defun agent-repl-wire-decode-session-fault-resume-failed (value)
  "Decode VALUE as `SessionFaultResumeFailed', a plist (`:cause')."
  (let ((object (agent-repl-wire--object "SessionFaultResumeFailed" value)))
    (agent-repl-wire--check-keys "SessionFaultResumeFailed" object '(cause))
    (agent-repl-wire--decoded
     "SessionFaultResumeFailed"
     (list :cause (agent-repl-wire--decode-string
                    "SessionFaultResumeFailed" 'cause object)))))

(defun agent-repl-wire-decode-session-fault-bounce-died (value)
  "Decode VALUE as the empty message `SessionFaultBounceDied'."
  (agent-repl-wire--decode-empty "SessionFaultBounceDied" value))

(defun agent-repl-wire-decode-session-fault-bounce-unknown (value)
  "Decode VALUE as the empty message `SessionFaultBounceUnknown'."
  (agent-repl-wire--decode-empty "SessionFaultBounceUnknown" value))

(defun agent-repl-wire-decode-session-fault-classifier-failed (value)
  "Decode VALUE as `SessionFaultClassifierFailed', a plist (`:detail')."
  (let ((object (agent-repl-wire--object "SessionFaultClassifierFailed" value)))
    (agent-repl-wire--check-keys "SessionFaultClassifierFailed" object '(detail))
    (agent-repl-wire--decoded
     "SessionFaultClassifierFailed"
     (list :detail (agent-repl-wire--decode-string
                    "SessionFaultClassifierFailed" 'detail object)))))

(defun agent-repl-wire-decode-session-fault-shim-reported (value)
  "Decode VALUE as `SessionFaultShimReported', a plist (`:component' `:kind')."
  (let ((object (agent-repl-wire--object "SessionFaultShimReported" value)))
    (agent-repl-wire--check-keys "SessionFaultShimReported" object '(component kind))
    (agent-repl-wire--decoded
     "SessionFaultShimReported"
     (list :component (agent-repl-wire--decode-string
                    "SessionFaultShimReported" 'component object)
           :kind (agent-repl-wire--decode-string
                    "SessionFaultShimReported" 'kind object)))))

(defun agent-repl-wire-decode-session-fault-conversation-abandoned (value)
  "Decode VALUE as `SessionFaultConversationAbandoned', a plist
(`:vendor-session-id').
A recorded conversation whose transcript was gone at bring-up: the session
came up FRESH and the old vendor session id was left behind.  NOT a failure
to serve — the workspace has a live session — it is the record of what was
abandoned, which is why it is its own arm and not a resume failure."
  (let ((object (agent-repl-wire--object "SessionFaultConversationAbandoned" value)))
    (agent-repl-wire--check-keys "SessionFaultConversationAbandoned" object '(vendorSessionId))
    (agent-repl-wire--decoded
     "SessionFaultConversationAbandoned"
     (list :vendor-session-id (agent-repl-wire--decode-string
                    "SessionFaultConversationAbandoned" 'vendorSessionId object)))))

(defun agent-repl-wire-decode-session-fault-session-absent (value)
  "Decode VALUE as the empty message `SessionFaultSessionAbsent'.
The LIVENESS PROBE'S OWN observation: this workspace has no live session at
all.  Nothing RAISED it — no shim reported it and no controller opened it, so
it is never a recorded fault — it is what the probe answers when there is
nothing there to answer for itself.  Empty: the arm is the whole fact."
  (agent-repl-wire--decode-empty "SessionFaultSessionAbsent" value))

(defun agent-repl-wire-decode-session-fault-watch-open-refused (value)
  "Decode VALUE as `SessionFaultWatchOpenRefused', a plist
(`:operation' `:handle').
A shim watch OPEN the shim REFUSED for a handle nothing announced: the
daemon and the shim disagree about what exists.  NOT a severed link — the
shim answered the open, so the hop is serving and a redial would change
nothing, which is why it is its own arm beside `link_severed'."
  (let ((object (agent-repl-wire--object "SessionFaultWatchOpenRefused" value)))
    (agent-repl-wire--check-keys "SessionFaultWatchOpenRefused" object '(operation handle))
    (agent-repl-wire--decoded
     "SessionFaultWatchOpenRefused"
     (list :operation (agent-repl-wire--decode-string
                    "SessionFaultWatchOpenRefused" 'operation object)
           :handle (agent-repl-wire--decode-string
                    "SessionFaultWatchOpenRefused" 'handle object)))))

(defun agent-repl-wire-decode-session-fault-daemon-state-unreadable (value)
  "Decode VALUE as `SessionFaultDaemonStateUnreadable', a plist (`:cause').
The health reporter's own fault: the daemon's state client would not answer,
so the workspace's recorded faults could not be read at all.  It says THE
ANSWER IS INCOMPLETE, not that the session is broken — every other arm here
is a condition of the session, and this one is a condition of the reporting."
  (let ((object (agent-repl-wire--object "SessionFaultDaemonStateUnreadable" value)))
    (agent-repl-wire--check-keys "SessionFaultDaemonStateUnreadable" object '(cause))
    (agent-repl-wire--decoded
     "SessionFaultDaemonStateUnreadable"
     (list :cause (agent-repl-wire--decode-string
                    "SessionFaultDaemonStateUnreadable" 'cause object)))))

(defun agent-repl-wire-decode-session-fault-adoption-window-expired (value)
  "Decode VALUE as `SessionFaultAdoptionWindowExpired', a plist
(`:adoption-window').
A handover whose adoption window ran out with this workspace unclaimed: the
successor never took it.  DaemonFault spells the DAEMON-scoped arm; this is
the WORKSPACE's own, because the rollout controller records the expiry
against the workspace it was handing over and the daemon-health filter
\(workspace-bound faults are SessionHealth's answer) passes it here."
  (let ((object (agent-repl-wire--object "SessionFaultAdoptionWindowExpired" value)))
    (agent-repl-wire--check-keys "SessionFaultAdoptionWindowExpired" object '(adoptionWindow))
    (agent-repl-wire--decoded
     "SessionFaultAdoptionWindowExpired"
     (list :adoption-window (agent-repl-wire--decode-string
                    "SessionFaultAdoptionWindowExpired" 'adoptionWindow object)))))

(defun agent-repl-wire-decode-session-fault-final-answer-unresolved (value)
  "Decode VALUE as `SessionFaultFinalAnswerUnresolved', a plist
(`:turn' `:unit' `:why').
A turn that concluded with NO GREEN ANSWER STANDING: the terminal named no
answering response while the turn drew prose, the answer it named resolves to
no drawn row, or an open response went silent.  NOT a failure to serve — the
session is healthy and the prose is on screen — so it never escalates the
footer's status; `why' is the substatus, and it is carried HERE rather than in
the status cell for exactly that reason."
  (let ((object (agent-repl-wire--object "SessionFaultFinalAnswerUnresolved" value)))
    (agent-repl-wire--check-keys "SessionFaultFinalAnswerUnresolved" object '(turn unit why))
    (agent-repl-wire--decoded
     "SessionFaultFinalAnswerUnresolved"
     (list :turn (agent-repl-wire--decode-string
                    "SessionFaultFinalAnswerUnresolved" 'turn object)
           :unit (agent-repl-wire--decode-string
                    "SessionFaultFinalAnswerUnresolved" 'unit object)
           :why (agent-repl-wire--decode-string
                    "SessionFaultFinalAnswerUnresolved" 'why object)))))

(provide 'wire-common)

;;; wire-common.el ends here
