;;; test-wire-common.el --- ERT tests for agent-repl wire-common.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-wire-common.el -f ert-run-tests-batch-and-exit
;;
;; The JSON fixtures are written BY HAND from the .proto files in Go's
;; protojson shape (lowerCamel keys, int64 as a decimal string, omitted
;; defaults, `{}' for a set-but-empty message), so a fixture that stops
;; matching the daemon is a fixture bug the reader can see, never a
;; generator's opinion echoed back at itself.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Local harness ----

(defmacro agent-repl-test-wire-common--quiet (&rest body)
  "Run BODY with the logging ladder stubbed out.
`agent-repl--error' is the ERROR rung the codec logs a breach through, and
it signals an `error' of its own; the codec catches that so the TYPED wire
error survives, and these tests stub it so a breach case neither writes to
the durable sink nor depends on that catch."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'agent-repl--error) (lambda (&rest _) nil))
             ((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
     ,@body))

(defun agent-repl-test-wire-common--parse (json)
  "Parse JSON exactly as the codec's callers do."
  (json-parse-string json :object-type 'alist :array-type 'list
                     :null-object :null :false-object :false))

(defun agent-repl-test-wire-common--breach (thunk)
  "Return the `agent-repl-wire-error' data THUNK signals, or nil."
  (agent-repl-test-wire-common--quiet
    (condition-case err
        (progn (funcall thunk) nil)
      (agent-repl-wire-error (cdr err)))))

(defun agent-repl-test-wire-common--decode (decoder json)
  "Decode JSON with DECODER, quietly."
  (agent-repl-test-wire-common--quiet
    (funcall decoder (agent-repl-test-wire-common--parse json))))

(defun agent-repl-test-wire-common--reparse (alist)
  "Serialize ALIST and parse it back, the way the daemon would see it."
  (agent-repl-test-wire-common--parse (json-serialize alist)))

;;;; ---- WorkspaceRef ----

(ert-deftest agent-repl-test-wire-common-workspace-ref-decodes-both-halves ()
  "A WorkspaceRef decodes its opaque id and its display dir."
  (let ((decoded (agent-repl-test-wire-common--decode
                  #'agent-repl-wire-decode-workspace-ref
                  "{\"id\":\"ws-7\",\"dir\":\"/w/fix\"}")))
    (should (equal decoded '(:id "ws-7" :dir "/w/fix")))))

(ert-deftest agent-repl-test-wire-common-workspace-ref-omitted-dir-is-the-default ()
  "protojson omits a default-valued scalar, so an absent dir decodes to \"\"."
  (let ((decoded (agent-repl-test-wire-common--decode
                  #'agent-repl-wire-decode-workspace-ref "{\"id\":\"ws-7\"}")))
    (should (equal (plist-get decoded :dir) ""))))

(ert-deftest agent-repl-test-wire-common-workspace-ref-refuses-an-unknown-field ()
  "An unknown field on WorkspaceRef is a schema the consumer does not hold."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-decode-workspace-ref
                     (agent-repl-test-wire-common--parse
                      "{\"id\":\"ws-7\",\"path\":\"/w/fix\"}"))))
                 '("WorkspaceRef" path "unknown field"))))

(ert-deftest agent-repl-test-wire-common-workspace-ref-encodes-verbatim ()
  "The ref is an echo token: both halves travel back unchanged."
  (should (equal (agent-repl-test-wire-common--quiet
                   (agent-repl-wire-encode-workspace-ref '(:id "ws-7" :dir "/w/fix")))
                 '((id . "ws-7") (dir . "/w/fix")))))

(ert-deftest agent-repl-test-wire-common-workspace-ref-round-trips ()
  "Encoding a ref and decoding the serialized form returns the same plist."
  (let* ((ref '(:id "ws-7" :dir "/w/fix"))
         (json (agent-repl-test-wire-common--quiet
                 (json-serialize (agent-repl-wire-encode-workspace-ref ref)))))
    (should (equal (agent-repl-test-wire-common--decode
                    #'agent-repl-wire-decode-workspace-ref json)
                   ref))))

(ert-deftest agent-repl-test-wire-common-workspace-ref-refuses-a-non-string-id ()
  "A non-string id is refused rather than coerced."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-decode-workspace-ref
                     (agent-repl-test-wire-common--parse "{\"id\":7}"))))
                 '("WorkspaceRef" id "expected a string"))))

;;;; ---- RepositoryRef ----

(ert-deftest agent-repl-test-wire-common-repository-ref-decodes-both-halves ()
  "A RepositoryRef decodes with the same minting logic as a WorkspaceRef."
  (should (equal (agent-repl-test-wire-common--decode
                  #'agent-repl-wire-decode-repository-ref
                  "{\"id\":\"repo-1\",\"dir\":\"/src/doom\"}")
                 '(:id "repo-1" :dir "/src/doom"))))

(ert-deftest agent-repl-test-wire-common-repository-ref-refuses-an-unknown-field ()
  "An unknown field on RepositoryRef is refused at that message."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-decode-repository-ref
                     (agent-repl-test-wire-common--parse "{\"name\":\"doom\"}"))))
                 '("RepositoryRef" name "unknown field"))))

(ert-deftest agent-repl-test-wire-common-repository-ref-round-trips ()
  "A RepositoryRef survives encode, serialize, parse and decode."
  (let* ((ref '(:id "repo-1" :dir "/src/doom"))
         (json (agent-repl-test-wire-common--quiet
                 (json-serialize (agent-repl-wire-encode-repository-ref ref)))))
    (should (equal (agent-repl-test-wire-common--decode
                    #'agent-repl-wire-decode-repository-ref json)
                   ref))))

;;;; ---- TurnId ----

(ert-deftest agent-repl-test-wire-common-turn-id-decodes-its-opaque-token ()
  "A TurnId decodes to its opaque value, never parsed further."
  (should (equal (agent-repl-test-wire-common--decode
                  #'agent-repl-wire-decode-turn-id "{\"value\":\"turn-42\"}")
                 '(:value "turn-42"))))

(ert-deftest agent-repl-test-wire-common-turn-id-refuses-an-unknown-field ()
  "An unknown field on TurnId is refused."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-decode-turn-id
                     (agent-repl-test-wire-common--parse "{\"id\":\"turn-42\"}"))))
                 '("TurnId" id "unknown field"))))

;;;; ---- Shared scalar primitives ----

(ert-deftest agent-repl-test-wire-common-int64-accepts-the-emitted-string ()
  "protojson EMITS int64 as a decimal string; the decoder takes it."
  (let ((object (agent-repl-test-wire-common--parse "{\"atMs\":\"1756400000000\"}")))
    (should (equal (agent-repl-wire--decode-int64 "M" 'atMs object) 1756400000000))))

(ert-deftest agent-repl-test-wire-common-int64-accepts-a-number ()
  "protojson ACCEPTS a number for int64, and so does this decoder."
  (let ((object (agent-repl-test-wire-common--parse "{\"atMs\":1756400000000}")))
    (should (equal (agent-repl-wire--decode-int64 "M" 'atMs object) 1756400000000))))

(ert-deftest agent-repl-test-wire-common-int64-absent-is-the-proto3-default ()
  "An omitted non-optional int64 is 0, the default protojson elides."
  (should (equal (agent-repl-wire--decode-int64 "M" 'atMs nil) 0)))

(ert-deftest agent-repl-test-wire-common-int64-refuses-a-non-decimal-string ()
  "A string that is not a decimal integer is a breach, not a zero."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire--decode-int64
                     "M" 'atMs (agent-repl-test-wire-common--parse "{\"atMs\":\"soon\"}"))))
                 '("M" atMs "expected an integer"))))

(ert-deftest agent-repl-test-wire-common-int64-accepts-a-negative-string ()
  "A negative instant is still an int64 and decodes as one."
  (let ((object (agent-repl-test-wire-common--parse "{\"atMs\":\"-5\"}")))
    (should (equal (agent-repl-wire--decode-int64 "M" 'atMs object) -5))))

(ert-deftest agent-repl-test-wire-common-optional-uint32-absent-is-nil ()
  "An absent optional uint32 is nil — presence, never a sentinel."
  (should (equal (agent-repl-wire--decode-optional-uint32 "M" 'line nil) nil)))

(ert-deftest agent-repl-test-wire-common-optional-uint32-present-is-a-number ()
  "protojson carries uint32 as a number, which decodes verbatim."
  (let ((object (agent-repl-test-wire-common--parse "{\"line\":41}")))
    (should (equal (agent-repl-wire--decode-optional-uint32 "M" 'line object) 41))))

(ert-deftest agent-repl-test-wire-common-optional-uint32-refuses-a-negative ()
  "A negative value cannot be a uint32 and is refused."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire--decode-optional-uint32
                     "M" 'line (agent-repl-test-wire-common--parse "{\"line\":-1}"))))
                 '("M" line "expected a non-negative integer"))))

(ert-deftest agent-repl-test-wire-common-bool-absent-is-false ()
  "An omitted bool is false: protojson never emits the default."
  (should (equal (agent-repl-wire--decode-bool "M" 'shimAttached nil) nil)))

(ert-deftest agent-repl-test-wire-common-bool-false-object-is-false ()
  "An explicit false parses to `:false' and decodes to nil."
  (let ((object (agent-repl-test-wire-common--parse "{\"shimAttached\":false}")))
    (should (equal (agent-repl-wire--decode-bool "M" 'shimAttached object) nil))))

(ert-deftest agent-repl-test-wire-common-bool-true-is-t ()
  "An explicit true decodes to t."
  (let ((object (agent-repl-test-wire-common--parse "{\"shimAttached\":true}")))
    (should (equal (agent-repl-wire--decode-bool "M" 'shimAttached object) t))))

(ert-deftest agent-repl-test-wire-common-bool-refuses-a-string ()
  "A string where a bool belongs is a breach."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire--decode-bool
                     "M" 'shimAttached
                     (agent-repl-test-wire-common--parse "{\"shimAttached\":\"yes\"}"))))
                 '("M" shimAttached "expected a boolean"))))

(ert-deftest agent-repl-test-wire-common-optional-string-absent-is-nil ()
  "An absent optional string is nil, distinct from a present empty one."
  (should (equal (agent-repl-wire--decode-optional-string "M" 'slug nil) nil)))

(ert-deftest agent-repl-test-wire-common-optional-string-present-empty-is-empty ()
  "A PRESENT empty optional string decodes to \"\", not to nil."
  (let ((object (agent-repl-test-wire-common--parse "{\"slug\":\"\"}")))
    (should (equal (agent-repl-wire--decode-optional-string "M" 'slug object) ""))))

(ert-deftest agent-repl-test-wire-common-object-refuses-a-scalar ()
  "A scalar where a message belongs is refused before any field is read."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda () (agent-repl-wire--object "M" "nope")))
                 '("M" - "expected a JSON object"))))

(ert-deftest agent-repl-test-wire-common-required-message-absence-is-a-breach ()
  "An absent non-optional message field raises loudly, per the invariant."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire--decode-message "M" 'naming nil #'ignore)))
                 '("M" naming "required message field is absent"))))

(ert-deftest agent-repl-test-wire-common-repeated-absent-is-the-empty-list ()
  "An omitted repeated field is the empty list, never a breach."
  (should (equal (agent-repl-wire--decode-repeated "M" 'faults nil #'ignore) nil)))

(ert-deftest agent-repl-test-wire-common-repeated-populated-decodes-each-element ()
  "Each element of a populated repeated field goes through the decoder."
  (let ((object (agent-repl-test-wire-common--parse
                 "{\"rows\":[{\"value\":\"a\"},{\"value\":\"b\"}]}")))
    (should (equal (agent-repl-wire--decode-repeated
                    "M" 'rows object #'agent-repl-wire-decode-turn-id)
                   '((:value "a") (:value "b"))))))

;;;; ---- The oneof primitive ----

(defconst agent-repl-test-wire-common--oneof-arms
  '((deploy :deploy agent-repl-wire-decode-drain-reason-deploy)
    (operator :operator agent-repl-wire-decode-drain-reason-operator))
  "A two-arm table for the oneof primitive's own tests.")

(ert-deftest agent-repl-test-wire-common-oneof-decodes-the-one-set-arm ()
  "Exactly one set arm decodes to `(:arm KEYWORD :value V)'."
  (let ((object (agent-repl-test-wire-common--parse "{\"operator\":{\"note\":\"rollout\"}}")))
    (should (equal (agent-repl-test-wire-common--quiet
                     (agent-repl-wire--decode-oneof
                      "M" 'kind object agent-repl-test-wire-common--oneof-arms))
                   '(:arm :operator :value (:note "rollout"))))))

(ert-deftest agent-repl-test-wire-common-oneof-unset-is-a-breach-by-default ()
  "An unset oneof is an error by default: a state with no arm is no state."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire--decode-oneof
                     "M" 'kind nil agent-repl-test-wire-common--oneof-arms)))
                 '("M" kind "oneof is unset"))))

(ert-deftest agent-repl-test-wire-common-oneof-unset-is-nil-where-legal ()
  "Where the proto says unset is legal, an unset oneof decodes to nil."
  (should (equal (agent-repl-test-wire-common--quiet
                   (agent-repl-wire--decode-oneof
                    "M" 'kind nil agent-repl-test-wire-common--oneof-arms t))
                 nil)))

(ert-deftest agent-repl-test-wire-common-oneof-two-arms-is-a-breach ()
  "Two arms set is the same breach as none, seen from the other side."
  (let ((object (agent-repl-test-wire-common--parse
                 "{\"deploy\":{},\"operator\":{\"note\":\"n\"}}")))
    (should (equal (agent-repl-test-wire-common--breach
                    (lambda ()
                      (agent-repl-wire--decode-oneof
                       "M" 'kind object agent-repl-test-wire-common--oneof-arms)))
                   '("M" kind "oneof has more than one arm set")))))

;;;; ---- PromptOrigin ----

(ert-deftest agent-repl-test-wire-common-prompt-origin-encodes-the-enum-name ()
  "The kebab keyword encodes to the enum's protojson string name."
  (should (equal (agent-repl-test-wire-common--quiet
                   (agent-repl-wire-encode-prompt-origin :user-sent))
                 "PROMPT_ORIGIN_USER_SENT")))

(ert-deftest agent-repl-test-wire-common-prompt-origin-encodes-every-value ()
  "Every keyword in the vocabulary encodes to a PROMPT_ORIGIN_ name."
  (dolist (case agent-repl-wire-prompt-origins)
    (should (equal (agent-repl-test-wire-common--quiet
                     (agent-repl-wire-encode-prompt-origin (car case)))
                   (cdr case)))))

(ert-deftest agent-repl-test-wire-common-prompt-origin-refuses-unspecified ()
  "UNSPECIFIED has no keyword, so `:unspecified' is refused before send."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda () (agent-repl-wire-encode-prompt-origin :unspecified)))
                 '("PromptOrigin" origin "unknown prompt origin"))))

(ert-deftest agent-repl-test-wire-common-prompt-origin-refuses-an-unknown-keyword ()
  "A keyword outside the closed vocabulary is refused, never guessed at."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda () (agent-repl-wire-encode-prompt-origin :invented)))
                 '("PromptOrigin" origin "unknown prompt origin"))))

(ert-deftest agent-repl-test-wire-common-prompt-origin-table-matches-the-bindings ()
  "The keyword table covers the declared enum exactly, minus UNSPECIFIED.
Read from the checked-in Go bindings, so a value landed in the proto
without a keyword here fails this test instead of failing a send."
  (let ((declared (sort (delete "PROMPT_ORIGIN_UNSPECIFIED"
                                (agent-repl-test--generated-enum-names
                                 "conversation/v1/prompt_origin.pb.go" "PROMPT_ORIGIN_"))
                        #'string<))
        (spelled (sort (mapcar #'cdr agent-repl-wire-prompt-origins) #'string<)))
    (should (equal spelled declared))))

;;;; ---- The user message Emacs produces ----

(ert-deftest agent-repl-test-wire-common-text-block-encodes-its-text ()
  "A TextBlock carries the text verbatim and declares no format."
  (should (equal (agent-repl-test-wire-common--quiet
                   (agent-repl-wire-encode-text-block '(:text "ship it")))
                 '((text . "ship it")))))

(ert-deftest agent-repl-test-wire-common-image-block-encodes-the-path-arm ()
  "A pasted image travels as ImageBlock{path} plus its media type."
  (should (equal (agent-repl-test-wire-common--quiet
                   (agent-repl-wire-encode-image-block
                    '(:location (:arm :path :value (:path "/tmp/a.png"))
                      :media-type "image/png")))
                 '((path . ((path . "/tmp/a.png"))) (mediaType . "image/png")))))

(ert-deftest agent-repl-test-wire-common-image-block-encodes-the-url-arm ()
  "WHICH kind of reference an image is travels by arm, not by sniffing."
  (should (equal (agent-repl-test-wire-common--quiet
                   (agent-repl-wire-encode-image-block
                    '(:location (:arm :url :value (:url "https://x/a.png"))
                      :media-type "image/png")))
                 '((url . ((url . "https://x/a.png"))) (mediaType . "image/png")))))

(ert-deftest agent-repl-test-wire-common-image-block-refuses-an-unset-location ()
  "An ImageBlock with no location arm is an incomplete request."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-encode-image-block '(:media-type "image/png"))))
                 '("ImageBlock" location "oneof is unset"))))

(ert-deftest agent-repl-test-wire-common-user-content-block-refuses-unsupported ()
  "`unsupported' is not a fallback, so Emacs cannot name it as an arm."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-encode-user-content-block
                     '(:arm :unsupported :value nil))))
                 '("UserContentBlock" block "unknown oneof arm"))))

(ert-deftest agent-repl-test-wire-common-user-content-empty-blocks-is-an-array ()
  "An empty repeated field serializes as `[]', never as an omitted key."
  (should (equal (agent-repl-test-wire-common--quiet
                   (json-serialize (agent-repl-wire-encode-user-content '(:blocks nil))))
                 "{\"blocks\":[]}")))

(ert-deftest agent-repl-test-wire-common-user-said-refuses-absent-content ()
  "What a person said IS the message, so absent content is refused."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda () (agent-repl-wire-encode-user-said nil)))
                 '("UserSaid" content "required message field is absent"))))

(ert-deftest agent-repl-test-wire-common-user-said-serializes-in-composed-order ()
  "Blocks serialize in the order the person composed them."
  (should (equal (agent-repl-test-wire-common--quiet
                   (json-serialize
                    (agent-repl-wire-encode-user-said
                     '(:content
                       (:blocks ((:arm :text :value (:text "look at this"))
                                 (:arm :image
                                  :value (:location (:arm :path
                                                     :value (:path "/tmp/a.png"))
                                          :media-type "image/png"))))))))
                 (concat "{\"content\":{\"blocks\":["
                         "{\"text\":{\"text\":\"look at this\"}},"
                         "{\"image\":{\"path\":{\"path\":\"/tmp/a.png\"},"
                         "\"mediaType\":\"image/png\"}}]}}"))))

;;;; ---- DrainReason ----

(ert-deftest agent-repl-test-wire-common-drain-reason-decodes-every-arm ()
  "Every declared DrainReason arm decodes to its keyword."
  (dolist (case '(("{\"deploy\":{}}" (:arm :deploy :value nil))
                  ("{\"maintenance\":{}}" (:arm :maintenance :value nil))
                  ("{\"operator\":{\"note\":\"paging\"}}"
                   (:arm :operator :value (:note "paging")))))
    (should (equal (agent-repl-test-wire-common--decode
                    #'agent-repl-wire-decode-drain-reason (nth 0 case))
                   (nth 1 case)))))

(ert-deftest agent-repl-test-wire-common-drain-reason-unset-is-a-breach ()
  "A DrainReason with no arm is not a reason at all."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-decode-drain-reason
                     (agent-repl-test-wire-common--parse "{}"))))
                 '("DrainReason" kind "oneof is unset"))))

(ert-deftest agent-repl-test-wire-common-drain-reason-two-arms-is-a-breach ()
  "Two DrainReason arms set is refused loudly."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-decode-drain-reason
                     (agent-repl-test-wire-common--parse
                      "{\"deploy\":{},\"maintenance\":{}}"))))
                 '("DrainReason" kind "oneof has more than one arm set"))))

(ert-deftest agent-repl-test-wire-common-drain-reason-refuses-an-unknown-arm ()
  "An arm this build does not know is refused as an unknown field."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-decode-drain-reason
                     (agent-repl-test-wire-common--parse "{\"rehearsal\":{}}"))))
                 '("DrainReason" rehearsal "unknown field"))))

(ert-deftest agent-repl-test-wire-common-drain-reason-encodes-an-empty-arm ()
  "An empty arm encodes as `{}': the arm being set is the whole assertion."
  (should (equal (agent-repl-test-wire-common--quiet
                   (json-serialize
                    (agent-repl-wire-encode-drain-reason '(:arm :deploy :value nil))))
                 "{\"deploy\":{}}")))

(ert-deftest agent-repl-test-wire-common-drain-reason-refuses-a-blank-note ()
  "The operator note is REQUIRED non-blank and is refused at the request."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-encode-drain-reason
                     '(:arm :operator :value (:note "   ")))))
                 '("DrainReasonOperator" note "operator note is blank"))))

(ert-deftest agent-repl-test-wire-common-drain-reason-refuses-an-unknown-arm-on-encode ()
  "An unknown arm keyword never reaches the wire."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-encode-drain-reason '(:arm :rehearsal :value nil))))
                 '("DrainReason" kind "unknown oneof arm"))))

(ert-deftest agent-repl-test-wire-common-drain-reason-round-trips-every-arm ()
  "Each arm survives encode, serialize, parse and decode unchanged."
  (dolist (value '((:arm :deploy :value nil)
                   (:arm :maintenance :value nil)
                   (:arm :operator :value (:note "paging"))))
    (let ((json (agent-repl-test-wire-common--quiet
                  (json-serialize (agent-repl-wire-encode-drain-reason value)))))
      (should (equal (agent-repl-test-wire-common--decode
                      #'agent-repl-wire-decode-drain-reason json)
                     value)))))

;;;; ---- WorkspacePriority ----

(ert-deftest agent-repl-test-wire-common-workspace-priority-encodes-every-level ()
  "Every priority level encodes to its own empty arm."
  (dolist (case '((:p05 "{\"p05\":{}}")
                  (:p1 "{\"p1\":{}}")
                  (:p2 "{\"p2\":{}}")
                  (:p3 "{\"p3\":{}}")))
    (should (equal (agent-repl-test-wire-common--quiet
                     (json-serialize
                      (agent-repl-wire-encode-workspace-priority
                       (list :arm (nth 0 case) :value nil))))
                   (nth 1 case)))))

(ert-deftest agent-repl-test-wire-common-workspace-priority-refuses-an-unset-level ()
  "A priority with no level is not a priority."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda () (agent-repl-wire-encode-workspace-priority nil)))
                 '("WorkspacePriority" level "oneof is unset"))))

(ert-deftest agent-repl-test-wire-common-workspace-priority-refuses-an-unknown-level ()
  "A level outside the declared vocabulary is refused before send."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-encode-workspace-priority
                     '(:arm :p4 :value nil))))
                 '("WorkspacePriority" level "unknown oneof arm"))))

(ert-deftest agent-repl-test-wire-common-empty-arm-refuses-a-payload ()
  "An empty message given a payload is a caller bug, refused at the encode."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-encode-workspace-priority
                     '(:arm :p1 :value (:level 1)))))
                 '("WorkspacePriorityP1" - "expected an empty message"))))

(provide 'test-wire-common)

;;; test-wire-common.el ends here
