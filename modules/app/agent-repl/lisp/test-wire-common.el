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
`agent-repl--error' is the pure ERROR rung the codec RECORDS a breach
through; the typed `agent-repl-wire-error' these tests assert comes from
the codec's own `signal' afterwards.  Stubbed here only so a breach case
does not write to the durable sink."
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

;;;; ---- FeedId ----

(ert-deftest agent-repl-test-wire-common-feed-id-decodes-its-opaque-token ()
  "A FeedId decodes to its opaque value, echoed and never parsed."
  (should (equal (agent-repl-test-wire-common--decode
                  #'agent-repl-wire-decode-feed-id "{\"value\":\"feed-9\"}")
                 '(:value "feed-9"))))

(ert-deftest agent-repl-test-wire-common-feed-id-refuses-an-unknown-field ()
  "An unknown field on FeedId is a schema the consumer does not hold."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-decode-feed-id
                     (agent-repl-test-wire-common--parse "{\"id\":\"feed-9\"}"))))
                 '("FeedId" id "unknown field"))))

(ert-deftest agent-repl-test-wire-common-feed-id-encodes-verbatim ()
  "The id is an echo token: its opaque value travels back unchanged."
  (should (equal (agent-repl-test-wire-common--quiet
                   (agent-repl-wire-encode-feed-id '(:value "feed-9")))
                 '((value . "feed-9")))))

(ert-deftest agent-repl-test-wire-common-feed-id-round-trips ()
  "A FeedId survives encode, serialize, parse and decode unchanged."
  (let* ((feedid '(:value "feed-9"))
         (json (agent-repl-test-wire-common--quiet
                 (json-serialize (agent-repl-wire-encode-feed-id feedid)))))
    (should (equal (agent-repl-test-wire-common--decode
                    #'agent-repl-wire-decode-feed-id json)
                   feedid))))

(ert-deftest agent-repl-test-wire-common-feed-id-refuses-a-non-string-value ()
  "A non-string echo value is refused rather than sent for the daemon to reject."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-encode-feed-id '(:value 9))))
                 '("FeedId" value "expected a string"))))

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

(ert-deftest agent-repl-test-wire-common-uint32-absent-is-zero ()
  "An absent non-optional uint32 is the proto3 default protojson omits."
  (should (equal (agent-repl-wire--decode-uint32 "M" 'liveWork nil) 0)))

(ert-deftest agent-repl-test-wire-common-uint32-present-is-a-number ()
  "protojson carries uint32 as a number, which decodes verbatim."
  (let ((object (agent-repl-test-wire-common--parse "{\"liveWork\":3}")))
    (should (equal (agent-repl-wire--decode-uint32 "M" 'liveWork object) 3))))

(ert-deftest agent-repl-test-wire-common-uint32-refuses-a-negative ()
  "A negative value cannot be a uint32 and is refused."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire--decode-uint32
                     "M" 'liveWork (agent-repl-test-wire-common--parse "{\"liveWork\":-1}"))))
                 '("M" liveWork "expected a non-negative integer"))))

(ert-deftest agent-repl-test-wire-common-uint32-refuses-a-non-integer ()
  "A non-integer in a uint32 field is a contract breach."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire--decode-uint32
                     "M" 'liveWork (agent-repl-test-wire-common--parse "{\"liveWork\":\"lots\"}"))))
                 '("M" liveWork "expected an integer"))))

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

(ert-deftest agent-repl-test-wire-common-prompt-origin-refuses-a-shim-only-origin ()
  "A shim-only origin has no keyword, so `:vendor-started' is refused before send."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda () (agent-repl-wire-encode-prompt-origin :vendor-started)))
                 '("PromptOrigin" origin "unknown prompt origin"))))

(ert-deftest agent-repl-test-wire-common-prompt-origin-table-matches-the-bindings ()
  "The keyword table covers the declared enum exactly, minus UNSPECIFIED
and the shim-only values. Read from the checked-in Go bindings, so a value
landed in the proto without a keyword here fails this test instead of
failing a send."
  (let ((declared (sort (seq-difference
                         (delete "PROMPT_ORIGIN_UNSPECIFIED"
                                 (agent-repl-test--generated-enum-names
                                  "conversation/v1/prompt_origin.pb.go" "PROMPT_ORIGIN_"))
                         agent-repl-wire-shim-only-prompt-origins)
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


;;;; ---- Shared SessionFault arm messages (landing 4) ---------------------
;;
;; The base decoders live here because TWO parents carry them — SessionFault
;; on the SessionHealth response and HostFault on the host stream — and
;; validation lives once per message.

(ert-deftest agent-repl-test-wire-common-int32-absent-is-the-proto3-default ()
  "An omitted int32 is 0, which protojson omits rather than spells."
  (should (equal (agent-repl-test-wire-common--decode
                  #'agent-repl-wire-decode-session-fault-shim-died "{}")
                 '(:exit-code 0))))

(ert-deftest agent-repl-test-wire-common-int32-accepts-a-decimal-string ()
  "int32 is accepted in protojson's int64 string spelling as readily as a number."
  (should (equal (agent-repl-test-wire-common--decode
                  #'agent-repl-wire-decode-session-fault-shim-died
                  "{\"exitCode\":\"-1\"}")
                 '(:exit-code -1))))

(ert-deftest agent-repl-test-wire-common-int32-refuses-a-non-integer ()
  "A non-integer at an int32 position is a contract breach."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-decode-session-fault-shim-died
                     (agent-repl-test-wire-common--parse "{\"exitCode\":\"nine\"}"))))
                 '("SessionFaultShimDied" exitCode "expected an integer"))))

(ert-deftest agent-repl-test-wire-common-fault-string-absent-is-the-proto3-default ()
  "An omitted string inside a fault arm is the empty string, not a breach."
  (should (equal (plist-get
                  (agent-repl-test-wire-common--decode
                   #'agent-repl-wire-decode-session-fault-shim-start-failed "{}")
                  :stderr-tail)
                 "")))

(ert-deftest agent-repl-test-wire-common-session-fault-shim-start-failed-decodes ()
  "The shim-start-failed class carries the exit code and the stderr tail."
  (should (equal (agent-repl-test-wire-common--decode
                  #'agent-repl-wire-decode-session-fault-shim-start-failed
                  "{\"exitCode\":3,\"stderrTail\":\"panic\"}")
                 '(:exit-code 3 :stderr-tail "panic"))))

(ert-deftest agent-repl-test-wire-common-session-fault-shim-reported-decodes ()
  "A relayed shim-side fault names the component and the shim's own kind."
  (should (equal (agent-repl-test-wire-common--decode
                  #'agent-repl-wire-decode-session-fault-shim-reported
                  "{\"component\":\"stdout\",\"kind\":\"parse\"}")
                 '(:component "stdout" :kind "parse"))))

(ert-deftest agent-repl-test-wire-common-session-fault-refuses-an-unknown-field ()
  "An unknown key inside a fault arm is refused, never dropped."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-decode-session-fault-resume-failed
                     (agent-repl-test-wire-common--parse "{\"why\":\"x\"}"))))
                 '("SessionFaultResumeFailed" why "unknown field"))))

(ert-deftest agent-repl-test-wire-common-session-fault-empty-arm-decodes-to-nil ()
  "An empty fault class decodes to nil: the arm is the whole assertion."
  (should (equal (agent-repl-test-wire-common--decode
                  #'agent-repl-wire-decode-session-fault-link-severed "{}")
                 nil)))

(ert-deftest agent-repl-test-wire-common-session-fault-conversation-abandoned-decodes ()
  "The abandoned-conversation class names the vendor session id left behind."
  (should (equal (agent-repl-test-wire-common--decode
                  #'agent-repl-wire-decode-session-fault-conversation-abandoned
                  "{\"vendorSessionId\":\"vs-9\"}")
                 '(:vendor-session-id "vs-9"))))

(ert-deftest agent-repl-test-wire-common-session-fault-session-absent-decodes ()
  "The session-absent class is empty: the arm is the whole fact."
  (should (equal (agent-repl-test-wire-common--decode
                  #'agent-repl-wire-decode-session-fault-session-absent "{}")
                 nil)))

(ert-deftest agent-repl-test-wire-common-session-fault-watch-open-refused-decodes ()
  "The refused-open class names the operation and the handle it named."
  (should (equal (agent-repl-test-wire-common--decode
                  #'agent-repl-wire-decode-session-fault-watch-open-refused
                  "{\"operation\":\"WatchTranscript\",\"handle\":\"h-3\"}")
                 '(:operation "WatchTranscript" :handle "h-3"))))

(ert-deftest agent-repl-test-wire-common-session-fault-daemon-state-unreadable-decodes ()
  "The unreadable-state class carries the state client's own account."
  (should (equal (agent-repl-test-wire-common--decode
                  #'agent-repl-wire-decode-session-fault-daemon-state-unreadable
                  "{\"cause\":\"store closed\"}")
                 '(:cause "store closed"))))

(ert-deftest agent-repl-test-wire-common-session-fault-adoption-window-expired-decodes ()
  "The expired-window class carries the window as the controller rendered it."
  (should (equal (agent-repl-test-wire-common--decode
                  #'agent-repl-wire-decode-session-fault-adoption-window-expired
                  "{\"adoptionWindow\":\"30s\"}")
                 '(:adoption-window "30s"))))

(ert-deftest agent-repl-test-wire-common-session-fault-final-answer-unresolved-decodes ()
  "The unresolved-final-answer class carries the turn, the unit and the `why'
that tells its three cases apart."
  (should (equal (agent-repl-test-wire-common--decode
                  #'agent-repl-wire-decode-session-fault-final-answer-unresolved
                  "{\"turn\":\"turn-7\",\"unit\":\"msg_01:0\",\"why\":\"no_answer_named\"}")
                 '(:turn "turn-7" :unit "msg_01:0" :why "no_answer_named"))))

(ert-deftest agent-repl-test-wire-common-session-fault-final-answer-unresolved-refuses-a-field ()
  "A field the unresolved-final-answer class does not declare is refused."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-decode-session-fault-final-answer-unresolved
                     (agent-repl-test-wire-common--parse "{\"turn\":\"t\",\"unit\":\"u\",\"why\":\"w\",\"row\":\"r\"}"))))
                 '("SessionFaultFinalAnswerUnresolved" row "unknown field"))))

(ert-deftest agent-repl-test-wire-common-session-fault-session-absent-refuses-a-field ()
  "A field inside the empty session-absent class is refused, never dropped."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-decode-session-fault-session-absent
                     (agent-repl-test-wire-common--parse "{\"why\":\"x\"}"))))
                 '("SessionFaultSessionAbsent" why "unknown field"))))

(ert-deftest agent-repl-test-wire-common-double-present-is-a-number ()
  "protojson carries a double as a JSON number, decoded as a float."
  (let ((object (agent-repl-test-wire-common--parse "{\"scale\":1.5}")))
    (should (equal (agent-repl-wire--decode-double "M" 'scale object) 1.5))))

(ert-deftest agent-repl-test-wire-common-double-accepts-a-decimal-string ()
  "protojson also accepts the decimal-string spelling of a double."
  (let ((object (agent-repl-test-wire-common--parse "{\"scale\":\"1.5\"}")))
    (should (equal (agent-repl-wire--decode-double "M" 'scale object) 1.5))))

(ert-deftest agent-repl-test-wire-common-double-absent-is-zero ()
  "An absent non-optional double is the proto3 default protojson omits."
  (should (equal (agent-repl-wire--decode-double "M" 'scale nil) 0.0)))

(ert-deftest agent-repl-test-wire-common-double-refuses-a-non-number ()
  "A non-numeric spelling in a double field is a contract breach."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire--decode-double
                     "M" 'scale (agent-repl-test-wire-common--parse "{\"scale\":\"NaN\"}"))))
                 '("M" scale "expected a double"))))

;;;; ---- TurnId encode, uint64, and UserSaid decode (a held-prompt edit) ----

(ert-deftest agent-repl-test-wire-common-turn-id-encodes-its-echo-token ()
  "A TurnId encodes its opaque value verbatim."
  (should (equal (agent-repl-test-wire-common--quiet
                   (agent-repl-wire-encode-turn-id '(:value "turn-42")))
                 '((value . "turn-42")))))

(ert-deftest agent-repl-test-wire-common-turn-id-encode-refuses-a-non-string ()
  "A TurnId with no string value is refused before the wire."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda () (agent-repl-wire-encode-turn-id '(:value 7))))
                 '("TurnId" value "expected a string"))))

(ert-deftest agent-repl-test-wire-common-uint64-reads-the-decimal-string ()
  "protojson emits a uint64 as a decimal string."
  (should (equal (agent-repl-test-wire-common--quiet
                   (agent-repl-wire--decode-uint64
                    "M" 'edit (agent-repl-test-wire-common--parse "{\"edit\":\"12\"}")))
                 12)))

(ert-deftest agent-repl-test-wire-common-uint64-absent-is-zero ()
  "An absent uint64 is the proto3 default."
  (should (equal (agent-repl-test-wire-common--quiet
                   (agent-repl-wire--decode-uint64 "M" 'edit nil))
                 0)))

(ert-deftest agent-repl-test-wire-common-uint64-refuses-a-negative ()
  "A negative value is a breach for an unsigned field."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire--decode-uint64
                     "M" 'edit (agent-repl-test-wire-common--parse "{\"edit\":\"-1\"}"))))
                 '("M" edit "expected a non-negative integer"))))

(ert-deftest agent-repl-test-wire-common-user-said-decodes-text-and-a-path-image ()
  "A UserSaid decodes its blocks in order, words and a path image."
  (should (equal (agent-repl-test-wire-common--decode
                  #'agent-repl-wire-decode-user-said
                  "{\"content\":{\"blocks\":[{\"text\":{\"text\":\"hi\"}},{\"image\":{\"path\":{\"path\":\"/i.png\"},\"mediaType\":\"image/png\"}}]}}")
                 '(:content (:blocks ((:arm :text :value (:text "hi"))
                                      (:arm :image
                                       :value (:location (:arm :path :value (:path "/i.png"))
                                               :media-type "image/png"))))))))

(ert-deftest agent-repl-test-wire-common-user-said-decodes-a-url-image ()
  "An image by URL decodes under its own arm."
  (should (equal (plist-get
                  (car (plist-get (plist-get (agent-repl-test-wire-common--decode
                                              #'agent-repl-wire-decode-user-said
                                              "{\"content\":{\"blocks\":[{\"image\":{\"url\":{\"url\":\"https://x/i.png\"}}}]}}")
                                             :content)
                                  :blocks))
                  :value)
                 '(:location (:arm :url :value (:url "https://x/i.png")) :media-type ""))))

(ert-deftest agent-repl-test-wire-common-user-said-decodes-an-unsupported-block ()
  "An unsupported block keeps its kind and its raw payload."
  (should (equal (car (plist-get (plist-get (agent-repl-test-wire-common--decode
                                             #'agent-repl-wire-decode-user-said
                                             "{\"content\":{\"blocks\":[{\"unsupported\":{\"kind\":\"doc\"}}]}}")
                                            :content)
                                 :blocks))
                 '(:arm :unsupported :value (:kind "doc" :raw nil)))))

(ert-deftest agent-repl-test-wire-common-user-said-without-content-is-a-breach ()
  "`content' is REQUIRED on a UserSaid."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-decode-user-said (agent-repl-test-wire-common--parse "{}"))))
                 '("UserSaid" content "required message field is absent"))))

(ert-deftest agent-repl-test-wire-common-user-content-block-unset-is-a-breach ()
  "A UserContentBlock with no arm set is a breach."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-decode-user-content-block (agent-repl-test-wire-common--parse "{}"))))
                 '("UserContentBlock" block "oneof is unset"))))

(ert-deftest agent-repl-test-wire-common-image-block-refuses-an-unknown-field ()
  "An unknown field on an ImageBlock is refused."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-decode-image-block
                     (agent-repl-test-wire-common--parse "{\"path\":{\"path\":\"/i\"},\"bytes\":\"x\"}"))))
                 '("ImageBlock" bytes "unknown field"))))

;;;; ---- conversation.v1.LockHolderFailure ----

(ert-deftest agent-repl-test-wire-common-lock-holder-failure-decodes-a-spawn-failure ()
  "A spawn failure carries the operating system's account."
  (should (equal (agent-repl-test-wire-common--decode
                  #'agent-repl-wire-decode-lock-holder-failure
                  "{\"binary\":\"/b/shim-lock\",\"spawnFailed\":{\"osError\":\"spawn ENOENT\"}}")
                 '(:binary "/b/shim-lock" :how (:arm :spawn-failed :value (:os-error "spawn ENOENT"))))))

(ert-deftest agent-repl-test-wire-common-lock-holder-failure-decodes-an-exit ()
  "An exit carries its code and the holder's stderr."
  (should (equal (agent-repl-test-wire-common--decode
                  #'agent-repl-wire-decode-lock-holder-failure
                  "{\"binary\":\"/b/shim-lock\",\"exited\":{\"code\":1,\"stderr\":\"EACCES\"}}")
                 '(:binary "/b/shim-lock" :how (:arm :exited :value (:code 1 :stderr "EACCES"))))))

(ert-deftest agent-repl-test-wire-common-lock-holder-failure-decodes-a-signal ()
  "A signal death carries the signal's name."
  (should (equal (agent-repl-test-wire-common--decode
                  #'agent-repl-wire-decode-lock-holder-failure
                  "{\"binary\":\"/b/shim-lock\",\"signaled\":{\"signal\":\"SIGSEGV\"}}")
                 '(:binary "/b/shim-lock" :how (:arm :signaled :value (:signal "SIGSEGV" :stderr ""))))))

(ert-deftest agent-repl-test-wire-common-lock-holder-failure-decodes-a-wrong-line ()
  "A wrong answer carries the line the holder wrote."
  (should (equal (agent-repl-test-wire-common--decode
                  #'agent-repl-wire-decode-lock-holder-failure
                  "{\"binary\":\"/b/shim-lock\",\"misanswered\":{\"line\":\"ok\"}}")
                 '(:binary "/b/shim-lock" :how (:arm :misanswered :value (:line "ok"))))))

(ert-deftest agent-repl-test-wire-common-lock-holder-failure-decodes-silence ()
  "Silence carries the bound the shim waited."
  (should (equal (agent-repl-test-wire-common--decode
                  #'agent-repl-wire-decode-lock-holder-failure
                  "{\"binary\":\"/b/shim-lock\",\"silent\":{\"timeoutMs\":5000}}")
                 '(:binary "/b/shim-lock" :how (:arm :silent :value (:timeout-ms 5000))))))

(ert-deftest agent-repl-test-wire-common-lock-holder-failure-refuses-an-unset-how ()
  "A failure that says nothing of how the holder failed is a breach."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-decode-lock-holder-failure
                     (agent-repl-test-wire-common--parse "{\"binary\":\"/b/shim-lock\"}"))))
                 '("LockHolderFailure" "how" "oneof is unset"))))

(ert-deftest agent-repl-test-wire-common-lock-holder-failure-refuses-a-field ()
  "A field LockHolderFailure does not declare is refused."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-decode-lock-holder-failure
                     (agent-repl-test-wire-common--parse "{\"binary\":\"b\",\"osError\":\"x\"}"))))
                 '("LockHolderFailure" osError "unknown field"))))

;;;; ---- DaemonStreamEnding ----

(ert-deftest agent-repl-test-wire-common-daemon-stream-ending-decodes-to-nil ()
  "The planned ending is an empty message: its presence is the whole fact."
  (should (null (agent-repl-test-wire-common--decode
                 #'agent-repl-wire-decode-daemon-stream-ending "{}"))))

(ert-deftest agent-repl-test-wire-common-daemon-stream-ending-refuses-a-field ()
  "A field on the ending is a schema this consumer does not hold."
  (should (equal (agent-repl-test-wire-common--breach
                  (lambda ()
                    (agent-repl-wire-decode-daemon-stream-ending
                     (agent-repl-test-wire-common--parse "{\"address\":\"x\"}"))))
                 '("DaemonStreamEnding" address "unknown field"))))

(provide 'test-wire-common)

;;; test-wire-common.el ends here
