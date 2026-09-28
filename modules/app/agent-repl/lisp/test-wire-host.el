;;; test-wire-host.el --- ERT tests for agent-repl wire-host.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-wire-host.el -f ert-run-tests-batch-and-exit
;;
;; Fixtures are hand-written from the .proto files in Go's protojson shape.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Local harness ----

(defmacro agent-repl-test-wire-host--quiet (&rest body)
  "Run BODY with the logging ladder stubbed out."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'agent-repl--error) (lambda (&rest _) nil))
             ((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
     ,@body))

(defun agent-repl-test-wire-host--parse (json)
  "Parse JSON exactly as the codec's callers do."
  (json-parse-string json :object-type 'alist :array-type 'list
                     :null-object :null :false-object :false))

(defun agent-repl-test-wire-host--decode (decoder json)
  "Decode JSON with DECODER, quietly."
  (agent-repl-test-wire-host--quiet
    (funcall decoder (agent-repl-test-wire-host--parse json))))

(defun agent-repl-test-wire-host--breach (decoder json)
  "Return the `agent-repl-wire-error' data decoding JSON with DECODER raises."
  (agent-repl-test-wire-host--quiet
    (condition-case err
        (progn (funcall decoder (agent-repl-test-wire-host--parse json)) nil)
      (agent-repl-wire-error (cdr err)))))

(defconst agent-repl-test-wire-host--live-json
  (concat "{\"existing\":{\"id\":{\"value\":\"sess-1\"},"
          "\"live\":{\"generation\":{\"value\":\"gen-3\"},"
          "\"shimAttached\":true,"
          "\"claude\":{\"sessionId\":\"vendor-9\",\"configDir\":\"/home/me/.claude\"},"
          "\"backfill\":{\"done\":{}},"
          "\"open\":{},"
          "\"faults\":[{\"detail\":\"sidecar lag\",\"openedAtMs\":\"1756400000000\","
          "\"linkSevered\":{}}]}},"
          "\"naming\":{\"slug\":\"fix-flaky\",\"title\":\"Fix the flaky reconnect\"}}")
  "A fully populated HostWorkspace on the live standing.")

;;;; ---- RegisterWorkspace ----

(ert-deftest agent-repl-test-wire-host-register-request-carries-the-path ()
  "RegisterWorkspace sends a PATH; the daemon mints the identity from it."
  (should (equal (agent-repl-test-wire-host--quiet
                   (json-serialize
                    (agent-repl-wire-encode-register-workspace-request
                     '(:dir "~/w/fix"))))
                 "{\"dir\":\"~/w/fix\"}")))

(ert-deftest agent-repl-test-wire-host-register-success-carries-the-minted-ref ()
  "The success arm carries the daemon-minted WorkspaceRef, whole."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-register-workspace-response
                  "{\"success\":{\"workspace\":{\"id\":\"ws-7\",\"dir\":\"/w/fix\"}}}")
                 '(:arm :success :value (:workspace (:id "ws-7" :dir "/w/fix"))))))

(ert-deftest agent-repl-test-wire-host-register-error-arm-carries-its-cause ()
  "The error arm carries the refusal the daemon named."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-register-workspace-response
                  "{\"error\":{\"notAWorktree\":{}}}")
                 '(:arm :error
                   :value (:cause (:arm :not-a-worktree :value nil))))))

(ert-deftest agent-repl-test-wire-host-register-unset-result-is-a-breach ()
  "A response with no outcome arm is a contract breach."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-register-workspace-response "{}")
                 '("RegisterWorkspaceResponse" result "oneof is unset"))))

(ert-deftest agent-repl-test-wire-host-register-two-results-is-a-breach ()
  "A response setting both outcome arms is refused."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-register-workspace-response
                  "{\"success\":{\"workspace\":{\"id\":\"a\"}},\"error\":{}}")
                 '("RegisterWorkspaceResponse" result "oneof has more than one arm set"))))

(ert-deftest agent-repl-test-wire-host-register-success-without-workspace-is-a-breach ()
  "The success arm's workspace is not optional."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-register-workspace-response
                  "{\"success\":{}}")
                 '("RegisterWorkspaceSuccess" workspace
                   "required message field is absent"))))

(ert-deftest agent-repl-test-wire-host-register-refuses-an-unknown-field ()
  "An unknown key on the response is refused at the response's own level."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-register-workspace-response
                  "{\"pending\":{}}")
                 '("RegisterWorkspaceResponse" pending "unknown field"))))

;;;; ---- SelectWorkspace ----

(ert-deftest agent-repl-test-wire-host-select-request-echoes-the-ref ()
  "SelectWorkspace echoes the daemon-minted ref, never a path it built."
  (should (equal (agent-repl-test-wire-host--quiet
                   (json-serialize
                    (agent-repl-wire-encode-select-workspace-request
                     '(:workspace (:id "ws-7" :dir "/w/fix")))))
                 "{\"workspace\":{\"id\":\"ws-7\",\"dir\":\"/w/fix\"}}")))

(ert-deftest agent-repl-test-wire-host-select-request-without-a-ref-is-refused ()
  "An incomplete request errors before send, never on the wire."
  (should (equal (agent-repl-test-wire-host--quiet
                   (condition-case err
                       (progn (agent-repl-wire-encode-select-workspace-request nil) nil)
                     (agent-repl-wire-error (cdr err))))
                 '("SelectWorkspaceRequest" workspace
                   "required message field is absent"))))

(ert-deftest agent-repl-test-wire-host-select-success-is-empty ()
  "Selection succeeded; the roster stream carries the new `current'."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-select-workspace-response "{\"success\":{}}")
                 '(:arm :success :value nil))))

(ert-deftest agent-repl-test-wire-host-select-error-arm-decodes ()
  "The error arm decodes to its keyword with the cause the daemon named."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-select-workspace-response
                  "{\"error\":{\"unknownWorkspace\":{}}}")
                 '(:arm :error
                   :value (:cause (:arm :unknown-workspace :value nil))))))

(ert-deftest agent-repl-test-wire-host-select-unset-result-is-a-breach ()
  "A SelectWorkspace response with no arm is a breach."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-select-workspace-response "{}")
                 '("SelectWorkspaceResponse" result "oneof is unset"))))

;;;; ---- MarkWorkspaceViewed ----

(ert-deftest agent-repl-test-wire-host-mark-viewed-request-echoes-the-ref ()
  "MarkWorkspaceViewed echoes the daemon-minted ref, never a path it built."
  (should (equal (agent-repl-test-wire-host--quiet
                   (json-serialize
                    (agent-repl-wire-encode-mark-workspace-viewed-request
                     '(:workspace (:id "ws-7" :dir "/w/fix")))))
                 "{\"workspace\":{\"id\":\"ws-7\",\"dir\":\"/w/fix\"}}")))

(ert-deftest agent-repl-test-wire-host-mark-viewed-request-without-a-ref-is-refused ()
  "An incomplete request errors before send, never on the wire."
  (should (equal (agent-repl-test-wire-host--quiet
                   (condition-case err
                       (progn (agent-repl-wire-encode-mark-workspace-viewed-request nil) nil)
                     (agent-repl-wire-error (cdr err))))
                 '("MarkWorkspaceViewedRequest" workspace
                   "required message field is absent"))))

(ert-deftest agent-repl-test-wire-host-mark-viewed-success-is-empty ()
  "Marked; the roster stream carries the row's viewed marker."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-mark-workspace-viewed-response "{\"success\":{}}")
                 '(:arm :success :value nil))))

(ert-deftest agent-repl-test-wire-host-mark-viewed-error-arm-decodes ()
  "The error arm decodes to its keyword with the cause the daemon named."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-mark-workspace-viewed-response
                  "{\"error\":{\"unknownWorkspace\":{}}}")
                 '(:arm :error
                   :value (:cause (:arm :unknown-workspace :value nil))))))

(ert-deftest agent-repl-test-wire-host-mark-viewed-unset-result-is-a-breach ()
  "A MarkWorkspaceViewed response with no arm is a breach."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-mark-workspace-viewed-response "{}")
                 '("MarkWorkspaceViewedResponse" result "oneof is unset"))))

;;;; ---- AdoptHostWorkspace ----

(ert-deftest agent-repl-test-wire-host-adopt-request-echoes-the-ref ()
  "The adopt verb identifies the participant; the request carries only the ref."
  (should (equal (agent-repl-test-wire-host--quiet
                   (json-serialize
                    (agent-repl-wire-encode-adopt-host-workspace-request
                     '(:workspace (:id "ws-7" :dir "/w/fix")))))
                 "{\"workspace\":{\"id\":\"ws-7\",\"dir\":\"/w/fix\"}}")))

(ert-deftest agent-repl-test-wire-host-adopt-success-is-empty ()
  "Adoption is COMPLETE: re-subscribe the workspace's streams here now."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-adopt-host-workspace-response
                  "{\"success\":{}}")
                 '(:arm :success :value nil))))

(ert-deftest agent-repl-test-wire-host-adopt-error-arm-decodes ()
  "The adopt error arm decodes to its keyword with its cause."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-adopt-host-workspace-response
                  "{\"error\":{\"noTransferAnnounced\":{}}}")
                 '(:arm :error
                   :value (:cause (:arm :no-transfer-announced :value nil))))))

(ert-deftest agent-repl-test-wire-host-adopt-unset-result-is-a-breach ()
  "An adopt response with no arm is a breach."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-adopt-host-workspace-response "{}")
                 '("AdoptHostWorkspaceResponse" result "oneof is unset"))))

;;;; ---- WatchHostWorkspace: the request and the push oneof ----

(ert-deftest agent-repl-test-wire-host-watch-request-echoes-the-ref ()
  "One subscription per open workspace, addressed by the minted ref."
  (should (equal (agent-repl-test-wire-host--quiet
                   (json-serialize
                    (agent-repl-wire-encode-watch-host-workspace-request
                     '(:workspace (:id "ws-7" :dir "/w/fix")))))
                 "{\"workspace\":{\"id\":\"ws-7\",\"dir\":\"/w/fix\"}}")))

(ert-deftest agent-repl-test-wire-host-push-decodes-every-arm ()
  "Every declared push arm decodes to its own keyword."
  (dolist (case (list (list (concat "{\"host\":" agent-repl-test-wire-host--live-json "}")
                            :host)
                      (list (concat "{\"notification\":{\"text\":\"hi\",\"atMs\":\"1\","
                                    "\"kind\":{\"agentAddressed\":{}}}}")
                            :notification)
                      (list "{\"transferred\":{}}" :transferred)
                      (list "{\"reloadWebapp\":{}}" :reload-webapp)
                      (list "{\"openInEditor\":{\"path\":\"/w/a.el\"}}" :open-in-editor)))
    (should (equal (plist-get (agent-repl-test-wire-host--decode
                               #'agent-repl-wire-decode-watch-host-workspace-response
                               (nth 0 case))
                              :arm)
                   (nth 1 case)))))

(ert-deftest agent-repl-test-wire-host-push-unset-is-a-breach ()
  "A push carrying no arm is a contract breach, not an empty update."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-watch-host-workspace-response "{}")
                 '("WatchHostWorkspaceResponse" push "oneof is unset"))))

(ert-deftest agent-repl-test-wire-host-push-two-arms-is-a-breach ()
  "Two push arms set is refused."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-watch-host-workspace-response
                  "{\"transferred\":{},\"reloadWebapp\":{}}")
                 '("WatchHostWorkspaceResponse" push "oneof has more than one arm set"))))

(ert-deftest agent-repl-test-wire-host-push-unknown-arm-is-refused ()
  "An arm this build does not carry is refused as an unknown field."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-watch-host-workspace-response
                  "{\"restartWebview\":{}}")
                 '("WatchHostWorkspaceResponse" restartWebview "unknown field"))))

;;;; ---- HostOpenInEditor ----

(ert-deftest agent-repl-test-wire-host-open-in-editor-carries-the-line ()
  "A relayed click with a line decodes both halves."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-host-open-in-editor
                  "{\"path\":\"/w/a.el\",\"line\":41}")
                 '(:path "/w/a.el" :line 41))))

(ert-deftest agent-repl-test-wire-host-open-in-editor-absent-line-is-nil ()
  "UNSET line means the file's top, or a directory — nil, not 0."
  (should (equal (plist-get (agent-repl-test-wire-host--decode
                             #'agent-repl-wire-decode-host-open-in-editor
                             "{\"path\":\"/w/src\"}")
                            :line)
                 nil)))

(ert-deftest agent-repl-test-wire-host-open-in-editor-refuses-an-unknown-field ()
  "An unknown key on the relayed click is refused."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-host-open-in-editor
                  "{\"path\":\"/w/a.el\",\"column\":3}")
                 '("HostOpenInEditor" column "unknown field"))))

;;;; ---- The notification push ----

(ert-deftest agent-repl-test-wire-host-notification-decodes-agent-addressed ()
  "The agent addressed the user: text, instant, and the kind arm."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-host-workspace-notification
                  (concat "{\"text\":\"ready for review\",\"atMs\":\"1756400000000\","
                          "\"kind\":{\"agentAddressed\":{}}}"))
                 '(:text "ready for review" :at-ms 1756400000000
                   :kind (:arm :agent-addressed :value nil)))))

(ert-deftest agent-repl-test-wire-host-notification-decodes-permission-requested ()
  "A permission ask names the gated tool for the banner line."
  (should (equal (plist-get (agent-repl-test-wire-host--decode
                             #'agent-repl-wire-decode-host-workspace-notification
                             (concat "{\"text\":\"allow Bash?\",\"atMs\":1,"
                                     "\"kind\":{\"permissionRequested\":"
                                     "{\"toolName\":\"Bash\"}}}"))
                            :kind)
                 '(:arm :permission-requested :value (:tool-name "Bash")))))

(ert-deftest agent-repl-test-wire-host-notification-decodes-question-asked ()
  "A question ask carries the first question's chip label as its header."
  (should (equal (plist-get (agent-repl-test-wire-host--decode
                             #'agent-repl-wire-decode-host-workspace-notification
                             (concat "{\"text\":\"which approach?\",\"atMs\":1,"
                                     "\"kind\":{\"questionAsked\":"
                                     "{\"header\":\"Which approach?\"}}}"))
                            :kind)
                 '(:arm :question-asked :value (:header "Which approach?")))))

(ert-deftest agent-repl-test-wire-host-notification-question-asked-omitted-header-is-empty ()
  "An omitted `header' is protojson's proto3 default, exactly as `tool_name' is."
  (should (equal (plist-get (agent-repl-test-wire-host--decode
                             #'agent-repl-wire-decode-host-workspace-notification
                             "{\"text\":\"x\",\"atMs\":1,\"kind\":{\"questionAsked\":{}}}")
                            :kind)
                 '(:arm :question-asked :value (:header "")))))

(ert-deftest agent-repl-test-wire-host-notification-accepts-a-numeric-instant ()
  "protojson accepts a number for int64, and an instant is an int64."
  (should (equal (plist-get (agent-repl-test-wire-host--decode
                             #'agent-repl-wire-decode-host-workspace-notification
                             (concat "{\"text\":\"x\",\"atMs\":1756400000000,"
                                     "\"kind\":{\"agentAddressed\":{}}}"))
                            :at-ms)
                 1756400000000)))

(ert-deftest agent-repl-test-wire-host-notification-without-a-kind-is-a-breach ()
  "The kind is not optional: the arm is the programmatic semantics."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-host-workspace-notification
                  "{\"text\":\"x\",\"atMs\":\"1\"}")
                 '("HostWorkspaceNotification" kind "required message field is absent"))))

(ert-deftest agent-repl-test-wire-host-notification-kind-unset-is-a-breach ()
  "A kind message with no arm is a breach."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-host-workspace-notification
                  "{\"text\":\"x\",\"atMs\":\"1\",\"kind\":{}}")
                 '("HostNotificationKind" kind "oneof is unset"))))

(ert-deftest agent-repl-test-wire-host-question-asked-without-a-header-defaults ()
  "`header' is a non-optional proto3 string: protojson omits the default."
  (should (equal (plist-get (agent-repl-test-wire-host--decode
                             #'agent-repl-wire-decode-host-workspace-notification
                             (concat "{\"text\":\"x\",\"atMs\":1,"
                                     "\"kind\":{\"questionAsked\":{}}}"))
                            :kind)
                 '(:arm :question-asked :value (:header "")))))

(ert-deftest agent-repl-test-wire-host-question-asked-unknown-field-is-a-breach ()
  "An unmodeled field inside the arm is refused, not ignored."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-host-workspace-notification
                  (concat "{\"text\":\"x\",\"atMs\":1,"
                          "\"kind\":{\"questionAsked\":{\"count\":2}}}"))
                 '("HostNotificationQuestionAsked" count "unknown field"))))

(ert-deftest agent-repl-test-wire-host-notification-kind-arms-match-the-bindings ()
  "The kind decoder's arm set is exactly what the frozen schema declares.
A future arm added to the proto fails this loudly rather than reaching the
unknown-arm refusal at runtime."
  (let ((declared (sort (agent-repl-test--generated-oneof-arms
                         "agentrepl/v1/endpoint_watch_host_workspace.pb.go"
                         "HostNotificationKind")
                        #'string<))
        (spelled (sort (list "agentAddressed" "permissionRequested" "questionAsked")
                       #'string<)))
    (should (equal spelled declared))))

(ert-deftest agent-repl-test-wire-host-notification-kind-unknown-arm-is-refused ()
  "An unmodeled notification kind is refused rather than silently dropped."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-host-workspace-notification
                  "{\"text\":\"x\",\"atMs\":\"1\",\"kind\":{\"budgetExceeded\":{}}}")
                 '("HostNotificationKind" budgetExceeded "unknown field"))))

;;;; ---- HostWorkspace: the session axis ----

(ert-deftest agent-repl-test-wire-host-workspace-decodes-the-none-session ()
  "Registered, but no session was ever created for it."
  (should (equal (plist-get (agent-repl-test-wire-host--decode
                             #'agent-repl-wire-decode-host-workspace
                             "{\"none\":{},\"naming\":{}}")
                            :session)
                 '(:arm :none :value nil))))

(ert-deftest agent-repl-test-wire-host-workspace-decodes-the-live-standing ()
  "A fully populated live workspace decodes whole, tree preserved."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-host-workspace
                  agent-repl-test-wire-host--live-json)
                 '(:session
                   (:arm :existing
                    :value (:id (:value "sess-1")
                            :standing
                            (:arm :live
                             :value (:generation (:value "gen-3")
                                     :shim-attached t
                                     :vendor-info (:arm :claude
                                                   :value (:session-id "vendor-9"
                                                           :config-dir "/home/me/.claude"))
                                     :backfill (:arm :done :value nil)
                                     :composer (:arm :open :value nil)
                                     :faults ((:detail "sidecar lag"
                                               :opened-at-ms 1756400000000
                                               :kind (:arm :link-severed
                                                      :value nil)))))))
                   :naming (:slug "fix-flaky" :title "Fix the flaky reconnect")
                   :held-prompt-edit nil))))

(ert-deftest agent-repl-test-wire-host-workspace-decodes-the-terminal-standing ()
  "A terminal session states whether reopening can rehydrate it."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-host-session-existing
                  "{\"id\":{\"value\":\"s\"},\"terminal\":{\"rehydratable\":true}}")
                 '(:id (:value "s")
                   :standing (:arm :terminal :value (:rehydratable t))))))

(ert-deftest agent-repl-test-wire-host-workspace-unset-session-is-a-breach ()
  "A workspace with no session arm is a breach, not a default."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-host-workspace "{\"naming\":{}}")
                 '("HostWorkspace" session "oneof is unset"))))

(ert-deftest agent-repl-test-wire-host-workspace-without-naming-is-a-breach ()
  "`naming' is a REQUIRED message: a buffer needs a name in every standing."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-host-workspace "{\"none\":{}}")
                 '("HostWorkspace" naming "required message field is absent"))))

(ert-deftest agent-repl-test-wire-host-naming-undecided-halves-are-nil ()
  "Both naming fields are OPTIONAL and unset until derived."
  (should (equal (plist-get (agent-repl-test-wire-host--decode
                             #'agent-repl-wire-decode-host-workspace
                             "{\"none\":{},\"naming\":{}}")
                            :naming)
                 '(:slug nil :title nil))))

(ert-deftest agent-repl-test-wire-host-naming-present-empty-slug-is-empty ()
  "A PRESENT empty slug is \"\", distinct from an absent one."
  (should (equal (plist-get (agent-repl-test-wire-host--decode
                             #'agent-repl-wire-decode-host-workspace-naming
                             "{\"slug\":\"\"}")
                            :slug)
                 "")))

(ert-deftest agent-repl-test-wire-host-workspace-decodes-the-standing-held-prompt-edit ()
  "A standing held-prompt edit decodes whole: its turn, its content, its id."
  (should (equal (plist-get (agent-repl-test-wire-host--decode
                             #'agent-repl-wire-decode-host-workspace
                             "{\"none\":{},\"naming\":{},\"heldPromptEdit\":{\"turn\":{\"value\":\"t-1\"},\"said\":{\"content\":{\"blocks\":[{\"text\":{\"text\":\"fix it\"}}]}},\"edit\":\"3\"}}")
                            :held-prompt-edit)
                 '(:turn (:value "t-1")
                   :said (:content (:blocks ((:arm :text :value (:text "fix it")))))
                   :edit 3))))

(ert-deftest agent-repl-test-wire-host-workspace-without-an-edit-carries-none ()
  "An absent held_prompt_edit is the fact that no edit stands."
  (should (null (plist-get (agent-repl-test-wire-host--decode
                            #'agent-repl-wire-decode-host-workspace
                            "{\"none\":{},\"naming\":{}}")
                           :held-prompt-edit))))

(ert-deftest agent-repl-test-wire-host-held-prompt-edit-without-a-turn-is-a-breach ()
  "The edit's turn is REQUIRED: a commit and a cancel echo it."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-host-held-prompt-edit
                  "{\"said\":{\"content\":{}},\"edit\":\"1\"}")
                 '("HostHeldPromptEdit" turn "required message field is absent"))))

(ert-deftest agent-repl-test-wire-host-held-prompt-edit-without-content-is-a-breach ()
  "The edit's content is REQUIRED: it is what the composer is filled with."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-host-held-prompt-edit
                  "{\"turn\":{\"value\":\"t\"},\"edit\":\"1\"}")
                 '("HostHeldPromptEdit" said "required message field is absent"))))

(ert-deftest agent-repl-test-wire-host-held-prompt-edit-refuses-an-unknown-field ()
  "An unknown key on HostHeldPromptEdit is refused."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-host-held-prompt-edit
                  "{\"turn\":{\"value\":\"t\"},\"said\":{\"content\":{}},\"owner\":\"x\"}")
                 '("HostHeldPromptEdit" owner "unknown field"))))

(ert-deftest agent-repl-test-wire-host-workspace-refuses-an-unknown-field ()
  "An unknown key on HostWorkspace is refused at that level."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-host-workspace
                  "{\"none\":{},\"naming\":{},\"hibernated\":{}}")
                 '("HostWorkspace" hibernated "unknown field"))))

(ert-deftest agent-repl-test-wire-host-existing-without-an-id-is-a-breach ()
  "The session identity is hoisted over the standing and is required."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-host-session-existing "{\"terminal\":{}}")
                 '("HostSessionExisting" id "required message field is absent"))))

(ert-deftest agent-repl-test-wire-host-existing-without-a-standing-is-a-breach ()
  "A session that exists always has a standing."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-host-session-existing
                  "{\"id\":{\"value\":\"s\"}}")
                 '("HostSessionExisting" standing "oneof is unset"))))

;;;; ---- HostSessionLive ----

(defconst agent-repl-test-wire-host--minimal-live
  "{\"generation\":{\"value\":\"g\"},\"backfill\":{\"none\":{}},%s}"
  "A live session with only its required parts, plus a %s slot for a gate.")

(defun agent-repl-test-wire-host--live (extra)
  "Return a minimal HostSessionLive JSON carrying EXTRA."
  (format agent-repl-test-wire-host--minimal-live extra))

(ert-deftest agent-repl-test-wire-host-live-decodes-every-composer-arm ()
  "Every composer gate arm decodes to its keyword; the arm IS the gate."
  (dolist (case '(("\"open\":{}" :open)
                  ("\"merging\":{}" :merging)
                  ("\"draining\":{}" :draining)
                  ("\"restarting\":{}" :restarting)
                  ("\"mergeParked\":{}" :merge-parked)))
    (should (equal (plist-get (agent-repl-test-wire-host--decode
                               #'agent-repl-wire-decode-host-session-live
                               (agent-repl-test-wire-host--live (nth 0 case)))
                              :composer)
                   (list :arm (nth 1 case) :value nil)))))

(ert-deftest agent-repl-test-wire-host-live-composer-arms-match-the-bindings ()
  "The decoder's arm set is exactly what the frozen schema declares.
Read from the checked-in Go bindings, which carry HostSessionLive's
composer and vendor_info arms together."
  (let ((declared (sort (agent-repl-test--generated-oneof-arms
                         "agentrepl/v1/endpoint_watch_host_workspace.pb.go"
                         "HostSessionLive")
                        #'string<))
        (spelled (sort (list "open" "merging" "draining" "restarting" "mergeParked"
                             "claude")
                       #'string<)))
    (should (equal spelled declared))))

(ert-deftest agent-repl-test-wire-host-live-unset-composer-is-a-breach ()
  "A live session with no gate arm is a breach."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-host-session-live
                  "{\"generation\":{\"value\":\"g\"},\"backfill\":{\"none\":{}}}")
                 '("HostSessionLive" composer "oneof is unset"))))

(ert-deftest agent-repl-test-wire-host-live-two-composer-arms-is-a-breach ()
  "Two gates set is the same breach as none."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-host-session-live
                  (agent-repl-test-wire-host--live "\"open\":{},\"merging\":{}"))
                 '("HostSessionLive" composer "oneof has more than one arm set"))))

(ert-deftest agent-repl-test-wire-host-live-unset-vendor-info-is-legal ()
  "The vendor oneof stays UNSET while no vendor conversation exists yet."
  (should (equal (plist-get (agent-repl-test-wire-host--decode
                             #'agent-repl-wire-decode-host-session-live
                             (agent-repl-test-wire-host--live "\"open\":{}"))
                            :vendor-info)
                 nil)))

(ert-deftest agent-repl-test-wire-host-live-vendor-claude-names-the-account ()
  "The claude arm names the conversation and the account it runs against."
  (should (equal (plist-get (agent-repl-test-wire-host--decode
                             #'agent-repl-wire-decode-host-session-live
                             (agent-repl-test-wire-host--live
                              (concat "\"open\":{},\"claude\":{\"sessionId\":\"v\","
                                      "\"configDir\":\"/c\"}")))
                            :vendor-info)
                 '(:arm :claude :value (:session-id "v" :config-dir "/c")))))

(ert-deftest agent-repl-test-wire-host-live-without-a-generation-is-a-breach ()
  "The controller generation is required: fault windows scope to it."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-host-session-live
                  "{\"backfill\":{\"none\":{}},\"open\":{}}")
                 '("HostSessionLive" generation "required message field is absent"))))

(ert-deftest agent-repl-test-wire-host-live-without-backfill-is-a-breach ()
  "The never-blue signal is required on a live session."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-host-session-live
                  "{\"generation\":{\"value\":\"g\"},\"open\":{}}")
                 '("HostSessionLive" backfill "required message field is absent"))))

(ert-deftest agent-repl-test-wire-host-live-omitted-shim-attached-is-false ()
  "protojson omits the default, so an absent shimAttached is false."
  (should (equal (plist-get (agent-repl-test-wire-host--decode
                             #'agent-repl-wire-decode-host-session-live
                             (agent-repl-test-wire-host--live "\"open\":{}"))
                            :shim-attached)
                 nil)))

(ert-deftest agent-repl-test-wire-host-live-empty-faults-is-the-empty-list ()
  "No standing faults decodes to the empty list, never to a breach."
  (should (equal (plist-get (agent-repl-test-wire-host--decode
                             #'agent-repl-wire-decode-host-session-live
                             (agent-repl-test-wire-host--live "\"open\":{},\"faults\":[]"))
                            :faults)
                 nil)))

(ert-deftest agent-repl-test-wire-host-live-faults-decode-each-window ()
  "Each standing fault decodes its account and the instant it opened."
  (should (equal (plist-get (agent-repl-test-wire-host--decode
                             #'agent-repl-wire-decode-host-session-live
                             (agent-repl-test-wire-host--live
                              (concat "\"open\":{},\"faults\":["
                                      "{\"detail\":\"a\",\"openedAtMs\":\"7\",\"bounceDied\":{}},"
                                      "{\"detail\":\"b\",\"openedAtMs\":9,\"bounceUnknown\":{}}]")))
                            :faults)
                 '((:detail "a" :opened-at-ms 7
                    :kind (:arm :bounce-died :value nil))
                   (:detail "b" :opened-at-ms 9
                    :kind (:arm :bounce-unknown :value nil))))))

(ert-deftest agent-repl-test-wire-host-live-refuses-an-unknown-field ()
  "An unknown key on the live standing is refused."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-host-session-live
                  (agent-repl-test-wire-host--live "\"open\":{},\"hibernated\":true"))
                 '("HostSessionLive" hibernated "unknown field"))))

;;;; ---- HostBackfill ----

(ert-deftest agent-repl-test-wire-host-backfill-decodes-every-arm ()
  "Every backfill state arm decodes to its keyword."
  (dolist (case '(("{\"none\":{}}" (:arm :none :value nil))
                  ("{\"pending\":{}}" (:arm :pending :value nil))
                  ("{\"done\":{}}" (:arm :done :value nil))
                  ("{\"failed\":{\"detail\":\"parse error\"}}"
                   (:arm :failed :value (:detail "parse error")))))
    (should (equal (agent-repl-test-wire-host--decode
                    #'agent-repl-wire-decode-host-backfill (nth 0 case))
                   (nth 1 case)))))

(ert-deftest agent-repl-test-wire-host-backfill-unset-is-a-breach ()
  "A backfill message with no state arm is a breach."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-host-backfill "{}")
                 '("HostBackfill" state "oneof is unset"))))

(ert-deftest agent-repl-test-wire-host-backfill-unknown-arm-is-refused ()
  "An unmodeled backfill state is refused as an unknown field."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-host-backfill "{\"partial\":{}}")
                 '("HostBackfill" partial "unknown field"))))

;;;; ---- WatchDaemon ----

(defun agent-repl-test-wire-host--encode-breach (encoder value)
  "Return the `agent-repl-wire-error' data encoding VALUE with ENCODER raises."
  (agent-repl-test-wire-host--quiet
    (condition-case err
        (progn (funcall encoder value) nil)
      (agent-repl-wire-error (cdr err)))))

(ert-deftest agent-repl-test-wire-host-watch-daemon-request-names-emacs-and-its-build ()
  "Emacs connects as the `emacs' client, stating the elisp build it loaded."
  (should (equal (agent-repl-test-wire-host--quiet
                   (json-serialize (agent-repl-wire-encode-watch-daemon-request
                                    '(:client (:arm :emacs :value (:elisp-build "abc123"))))))
                 "{\"emacs\":{\"elispBuild\":\"abc123\"}}")))

(ert-deftest agent-repl-test-wire-host-watch-daemon-request-without-a-client-is-refused ()
  "The client arm is REQUIRED: a request naming none never leaves Emacs."
  (should (equal (agent-repl-test-wire-host--encode-breach
                  #'agent-repl-wire-encode-watch-daemon-request nil)
                 '("WatchDaemonRequest" client "oneof is unset"))))

(ert-deftest agent-repl-test-wire-host-watch-daemon-request-refuses-the-webview-arm ()
  "Emacs never connects as a webview, so the codec has no spelling for it."
  (should (equal (agent-repl-test-wire-host--encode-breach
                  #'agent-repl-wire-encode-watch-daemon-request
                  '(:client (:arm :webview :value nil)))
                 '("WatchDaemonRequest" client "unknown oneof arm"))))

(ert-deftest agent-repl-test-wire-host-watch-daemon-emacs-empty-build-is-refused ()
  "An EMPTY elisp build is refused before anything is sent."
  (should (equal (agent-repl-test-wire-host--encode-breach
                  #'agent-repl-wire-encode-watch-daemon-emacs '(:elisp-build ""))
                 '("WatchDaemonEmacs" elispBuild "required string is empty"))))

(ert-deftest agent-repl-test-wire-host-watch-daemon-emacs-unset-build-is-refused ()
  "An UNSET elisp build is refused before anything is sent."
  (should (equal (agent-repl-test-wire-host--encode-breach
                  #'agent-repl-wire-encode-watch-daemon-emacs nil)
                 '("WatchDaemonEmacs" elispBuild "required field is unset"))))

(ert-deftest agent-repl-test-wire-host-watch-daemon-request-arms-pinned ()
  "The request's client arms are the frozen schema's; Emacs spells `emacs' alone."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_watch_daemon.pb.go" "WatchDaemonRequest")
                       #'string<)
                 '("emacs" "webview"))))

(ert-deftest agent-repl-test-wire-host-daemon-push-arms-pinned ()
  "The daemon stream's push arms are exactly what the frozen schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_watch_daemon.pb.go" "WatchDaemonResponse")
                       #'string<)
                 (sort (list "shutdownAnnounced" "drainScheduled" "drainCancelled"
                             "mutationProgress" "reloadElisp" "ending" "faultsStanding")
                       #'string<))))

(defconst agent-repl-test-wire-host--standing-fault-json
  (concat "{\"faultId\":\"f-1\",\"line\":\"deploy failed: build webapp: tsc\","
          "\"fault\":{\"detail\":\"the deploy failed\",\"deployFailed\":{\"build\":"
          "{\"step\":\"webapp\",\"detail\":\"tsc\",\"log\":\"/s/build.log\"}}},"
          "\"openedAtMs\":\"1756400000000\"}")
  "One standing loud fault: a failed deploy's build.")

(ert-deftest agent-repl-test-wire-host-faults-standing-decodes-each-fault ()
  "The standing loud faults decode to their id, line, typed fault and instant."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-watch-daemon-response
                  (concat "{\"faultsStanding\":{\"faults\":["
                          agent-repl-test-wire-host--standing-fault-json "]}}"))
                 '(:arm :faults-standing
                   :value (:faults
                           ((:fault-id "f-1"
                             :line "deploy failed: build webapp: tsc"
                             :fault (:detail "the deploy failed"
                                     :kind (:arm :deploy-failed
                                            :value (:step (:arm :build
                                                           :value (:step "webapp" :detail "tsc"
                                                                   :log "/s/build.log")))))
                             :opened-at-ms 1756400000000)))))))

(ert-deftest agent-repl-test-wire-host-faults-standing-empty-is-none-standing ()
  "An empty standing set decodes to no faults: the last one closed."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-watch-daemon-response "{\"faultsStanding\":{}}")
                 '(:arm :faults-standing :value (:faults nil)))))

(ert-deftest agent-repl-test-wire-host-standing-fault-without-an-id-is-refused ()
  "A standing fault without an id is a breach: the id is what a client surfaces a fault once by."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-daemon-standing-fault
                  "{\"line\":\"x\",\"fault\":{}}")
                 '("DaemonStandingFault" faultId "required string is empty"))))

(ert-deftest agent-repl-test-wire-host-standing-fault-without-a-line-is-refused ()
  "A standing fault without a line is a breach: the line is the one sentence a client shows."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-daemon-standing-fault
                  "{\"faultId\":\"f-1\",\"fault\":{}}")
                 '("DaemonStandingFault" line "required string is empty"))))

(ert-deftest agent-repl-test-wire-host-standing-fault-without-its-fault-is-refused ()
  "A standing fault without its fault is a breach: the typed fault is required."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-daemon-standing-fault
                  "{\"faultId\":\"f-1\",\"line\":\"x\"}")
                 '("DaemonStandingFault" fault "required message field is absent"))))

(ert-deftest agent-repl-test-wire-host-standing-fault-with-an-unknown-field-is-refused ()
  "A standing fault with an unknown field is a breach: a field the message does not declare is refused."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-daemon-standing-fault
                  "{\"faultId\":\"f-1\",\"line\":\"x\",\"fault\":{},\"extra\":1}")
                 '("DaemonStandingFault" extra "unknown field"))))

(ert-deftest agent-repl-test-wire-host-daemon-ending-decodes-to-its-arm ()
  "The daemon stream's planned ending decodes to the `:ending' arm."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-watch-daemon-response "{\"ending\":{}}")
                 '(:arm :ending :value nil))))

(ert-deftest agent-repl-test-wire-host-host-push-arms-pinned ()
  "The host stream's push arms are exactly what the frozen schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_watch_host_workspace.pb.go"
                        "WatchHostWorkspaceResponse")
                       #'string<)
                 (sort (list "host" "notification" "transferred" "reloadWebapp"
                             "openInEditor" "ending")
                       #'string<))))

(ert-deftest agent-repl-test-wire-host-host-ending-decodes-to-its-arm ()
  "The host stream's planned ending decodes to the `:ending' arm."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-watch-host-workspace-response "{\"ending\":{}}")
                 '(:arm :ending :value nil))))

(ert-deftest agent-repl-test-wire-host-reload-elisp-decodes-its-root-and-build ()
  "A deploy's reload push carries the root to load from and the build it is."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-watch-daemon-response
                  "{\"reloadElisp\":{\"moduleRoot\":\"/r/agent-repl/\",\"build\":\"b1\"}}")
                 '(:arm :reload-elisp
                   :value (:module-root "/r/agent-repl/" :build "b1")))))

(ert-deftest agent-repl-test-wire-host-reload-elisp-without-a-root-is-a-breach ()
  "The root is REQUIRED: without it Emacs cannot check the reload is its own."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-watch-daemon-response
                  "{\"reloadElisp\":{\"build\":\"b1\"}}")
                 '("DaemonReloadElisp" moduleRoot "required string is empty"))))

(ert-deftest agent-repl-test-wire-host-reload-elisp-without-a-build-is-a-breach ()
  "The build is REQUIRED: it is what Emacs reports from then on."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-watch-daemon-response
                  "{\"reloadElisp\":{\"moduleRoot\":\"/r/\"}}")
                 '("DaemonReloadElisp" build "required string is empty"))))

(ert-deftest agent-repl-test-wire-host-reload-elisp-unknown-field-is-refused ()
  "A field the reload does not declare is refused, never guessed at."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-watch-daemon-response
                  "{\"reloadElisp\":{\"moduleRoot\":\"/r/\",\"build\":\"b\",\"partial\":true}}")
                 '("DaemonReloadElisp" partial "unknown field"))))

(ert-deftest agent-repl-test-wire-host-shutdown-announced-carries-the-successor ()
  "A handover announcement carries the address Emacs dual-attaches to."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-watch-daemon-response
                  (concat "{\"shutdownAnnounced\":{\"address\":\"127.0.0.1:5051\","
                          "\"cause\":{\"selfMergeRollout\":{}},"
                          "\"expectedOutageMs\":\"4000\","
                          "\"mintedAtMs\":\"1756400000000\"}}"))
                 '(:arm :shutdown-announced
                   :value (:address "127.0.0.1:5051"
                           :cause (:arm :self-merge-rollout :value nil)
                           :expected-outage-ms 4000
                           :minted-at-ms 1756400000000)))))

(ert-deftest agent-repl-test-wire-host-shutdown-announced-without-address-is-a-bounce ()
  "UNSET address is a PLAIN BOUNCE: nil, never an empty-string sentinel."
  (should (equal (plist-get (plist-get
                             (agent-repl-test-wire-host--decode
                              #'agent-repl-wire-decode-watch-daemon-response
                              (concat "{\"shutdownAnnounced\":{"
                                      "\"cause\":{\"selfMergeRollout\":{}},"
                                      "\"expectedOutageMs\":\"4000\","
                                      "\"mintedAtMs\":\"1\"}}"))
                             :value)
                            :address)
                 nil)))

(ert-deftest agent-repl-test-wire-host-shutdown-cause-scheduled-drain-nests-its-reason ()
  "A scheduled drain's cause carries the typed drain reason."
  (should (equal (plist-get (plist-get
                             (agent-repl-test-wire-host--decode
                              #'agent-repl-wire-decode-watch-daemon-response
                              (concat "{\"shutdownAnnounced\":{\"cause\":{"
                                      "\"scheduledDrain\":{\"reason\":{\"deploy\":{}}}},"
                                      "\"expectedOutageMs\":\"1\",\"mintedAtMs\":\"1\"}}"))
                             :value)
                            :cause)
                 '(:arm :scheduled-drain
                   :value (:reason (:arm :deploy :value nil))))))

(ert-deftest agent-repl-test-wire-host-shutdown-cause-immediate-nests-its-reason ()
  "An immediate operator shutdown carries the operator's note."
  (should (equal (plist-get (plist-get
                             (agent-repl-test-wire-host--decode
                              #'agent-repl-wire-decode-watch-daemon-response
                              (concat "{\"shutdownAnnounced\":{\"cause\":{"
                                      "\"immediate\":{\"reason\":{\"operator\":"
                                      "{\"note\":\"hotfix\"}}}},"
                                      "\"expectedOutageMs\":\"1\",\"mintedAtMs\":\"1\"}}"))
                             :value)
                            :cause)
                 '(:arm :immediate
                   :value (:reason (:arm :operator :value (:note "hotfix")))))))

(ert-deftest agent-repl-test-wire-host-shutdown-without-a-cause-is-a-breach ()
  "The cause is not optional: an unexplained retirement is a breach."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-watch-daemon-response
                  (concat "{\"shutdownAnnounced\":{\"expectedOutageMs\":\"1\","
                          "\"mintedAtMs\":\"1\"}}"))
                 '("DaemonShutdownAnnounced" cause "required message field is absent"))))

(ert-deftest agent-repl-test-wire-host-shutdown-cause-unset-is-a-breach ()
  "A cause message with no arm is a breach."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-watch-daemon-response
                  (concat "{\"shutdownAnnounced\":{\"cause\":{},"
                          "\"expectedOutageMs\":\"1\",\"mintedAtMs\":\"1\"}}"))
                 '("DaemonShutdownCause" kind "oneof is unset"))))

(ert-deftest agent-repl-test-wire-host-drain-scheduled-carries-its-instant ()
  "The standing drain schedule ships its deadline instant and its reason."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-watch-daemon-response
                  (concat "{\"drainScheduled\":{\"atMs\":\"1756400000000\","
                          "\"reason\":{\"maintenance\":{}}}}"))
                 '(:arm :drain-scheduled
                   :value (:at-ms 1756400000000
                           :reason (:arm :maintenance :value nil))))))

(ert-deftest agent-repl-test-wire-host-drain-scheduled-without-a-reason-is-a-breach ()
  "Every client's banner names the reason, so it is required."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-watch-daemon-response
                  "{\"drainScheduled\":{\"atMs\":\"1\"}}")
                 '("DaemonDrainScheduled" reason "required message field is absent"))))

(ert-deftest agent-repl-test-wire-host-drain-cancelled-is-presence-alone ()
  "Presence is the fact: the schedule was cancelled."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-watch-daemon-response
                  "{\"drainCancelled\":{}}")
                 '(:arm :drain-cancelled :value nil))))

(ert-deftest agent-repl-test-wire-host-daemon-push-unset-is-a-breach ()
  "A daemon push carrying no arm is a breach."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-watch-daemon-response "{}")
                 '("WatchDaemonResponse" push "oneof is unset"))))

(ert-deftest agent-repl-test-wire-host-daemon-push-unknown-arm-is-refused ()
  "An unmodeled daemon push arm is refused as an unknown field."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-watch-daemon-response
                  "{\"configReloaded\":{}}")
                 '("WatchDaemonResponse" configReloaded "unknown field"))))

;;;; ---- <Rpc>Error cause arms (landing 4) --------------------------------
;;
;; One test per arm the proto declares, plus the unset and unknown refusals
;; and the pin against the checked-in Go bindings — the PROTO is the arm
;; list, and the pin is what makes a landed arm the codec has not been taught
;; fail loudly instead of decoding as an unknown key.

(ert-deftest agent-repl-test-wire-host-register-error-not-a-worktree-arm ()
  "RegisterWorkspaceError's `not_a_worktree' arm decodes with everything it
carries."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-register-workspace-error
                  "{\"notAWorktree\":{}}")
                 '(:cause (:arm :not-a-worktree :value nil)))))

(ert-deftest agent-repl-test-wire-host-register-error-unset-cause-is-a-breach ()
  "RegisterWorkspaceError with no arm set says nothing actionable, so it is a
breach."
  (should (equal (agent-repl-test-wire-host--breach #'agent-repl-wire-decode-register-workspace-error "{}")
                 '("RegisterWorkspaceError" cause "oneof is unset"))))

(ert-deftest agent-repl-test-wire-host-register-error-unknown-arm-is-a-breach ()
  "An arm RegisterWorkspaceError does not declare here is refused, never
guessed at."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-register-workspace-error "{\"noSuchArm\":{}}")
                 '("RegisterWorkspaceError" noSuchArm "unknown field"))))

(ert-deftest agent-repl-test-wire-host-register-error-arms-pinned ()
  "RegisterWorkspaceError's arm set is exactly what the frozen schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_register_workspace.pb.go" "RegisterWorkspaceError")
                       #'string<)
                 (sort (list "notAWorktree")
                       #'string<))))

(ert-deftest agent-repl-test-wire-host-select-error-unknown-workspace-arm ()
  "SelectWorkspaceError's `unknown_workspace' arm decodes with everything it
carries."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-select-workspace-error
                  "{\"unknownWorkspace\":{}}")
                 '(:cause (:arm :unknown-workspace :value nil)))))

(ert-deftest agent-repl-test-wire-host-select-error-workspace-ref-mismatch-arm ()
  "SelectWorkspaceError's `workspace_ref_mismatch' arm decodes with everything
it carries."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-select-workspace-error
                  "{\"workspaceRefMismatch\":{\"registryDir\":\"/w/registry\"}}")
                 '(:cause (:arm :workspace-ref-mismatch :value (:registry-dir "/w/registry"))))))

(ert-deftest agent-repl-test-wire-host-select-error-transferring-away-arm ()
  "SelectWorkspaceError's `transferring_away' arm decodes with everything it
carries."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-select-workspace-error
                  "{\"transferringAway\":{\"address\":\"127.0.0.1:9999\"}}")
                 '(:cause (:arm :transferring-away :value (:address "127.0.0.1:9999"))))))

(ert-deftest agent-repl-test-wire-host-select-error-not-yet-adopted-arm ()
  "SelectWorkspaceError's `not_yet_adopted' arm decodes with everything it
carries."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-select-workspace-error
                  "{\"notYetAdopted\":{}}")
                 '(:cause (:arm :not-yet-adopted :value nil)))))

(ert-deftest agent-repl-test-wire-host-select-error-unset-cause-is-a-breach ()
  "SelectWorkspaceError with no arm set says nothing actionable, so it is a
breach."
  (should (equal (agent-repl-test-wire-host--breach #'agent-repl-wire-decode-select-workspace-error "{}")
                 '("SelectWorkspaceError" cause "oneof is unset"))))

(ert-deftest agent-repl-test-wire-host-select-error-unknown-arm-is-a-breach ()
  "An arm SelectWorkspaceError does not declare here is refused, never guessed
at."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-select-workspace-error "{\"noSuchArm\":{}}")
                 '("SelectWorkspaceError" noSuchArm "unknown field"))))

(ert-deftest agent-repl-test-wire-host-select-error-arms-pinned ()
  "SelectWorkspaceError's arm set is exactly what the frozen schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_select_workspace.pb.go" "SelectWorkspaceError")
                       #'string<)
                 (sort (list "unknownWorkspace" "workspaceRefMismatch" "transferringAway" "notYetAdopted")
                       #'string<))))

(ert-deftest agent-repl-test-wire-host-adopt-error-unknown-workspace-arm ()
  "AdoptHostWorkspaceError's `unknown_workspace' arm decodes with everything it
carries."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-adopt-host-workspace-error
                  "{\"unknownWorkspace\":{}}")
                 '(:cause (:arm :unknown-workspace :value nil)))))

(ert-deftest agent-repl-test-wire-host-adopt-error-workspace-ref-mismatch-arm ()
  "AdoptHostWorkspaceError's `workspace_ref_mismatch' arm decodes with
everything it carries."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-adopt-host-workspace-error
                  "{\"workspaceRefMismatch\":{\"registryDir\":\"/w/registry\"}}")
                 '(:cause (:arm :workspace-ref-mismatch :value (:registry-dir "/w/registry"))))))

(ert-deftest agent-repl-test-wire-host-adopt-error-transferring-away-arm ()
  "AdoptHostWorkspaceError's `transferring_away' arm decodes with everything it
carries."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-adopt-host-workspace-error
                  "{\"transferringAway\":{\"address\":\"127.0.0.1:9999\"}}")
                 '(:cause (:arm :transferring-away :value (:address "127.0.0.1:9999"))))))

(ert-deftest agent-repl-test-wire-host-adopt-error-not-yet-adopted-arm ()
  "AdoptHostWorkspaceError's `not_yet_adopted' arm decodes with everything it
carries."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-adopt-host-workspace-error
                  "{\"notYetAdopted\":{}}")
                 '(:cause (:arm :not-yet-adopted :value nil)))))

(ert-deftest agent-repl-test-wire-host-adopt-error-no-transfer-announced-arm ()
  "AdoptHostWorkspaceError's `no_transfer_announced' arm decodes with
everything it carries."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-adopt-host-workspace-error
                  "{\"noTransferAnnounced\":{}}")
                 '(:cause (:arm :no-transfer-announced :value nil)))))

(ert-deftest agent-repl-test-wire-host-adopt-error-participant-not-expected-arm ()
  "AdoptHostWorkspaceError's `participant_not_expected' arm decodes with
everything it carries."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-adopt-host-workspace-error
                  "{\"participantNotExpected\":{}}")
                 '(:cause (:arm :participant-not-expected :value nil)))))

(ert-deftest agent-repl-test-wire-host-adopt-error-unset-cause-is-a-breach ()
  "AdoptHostWorkspaceError with no arm set says nothing actionable, so it is a
breach."
  (should (equal (agent-repl-test-wire-host--breach #'agent-repl-wire-decode-adopt-host-workspace-error "{}")
                 '("AdoptHostWorkspaceError" cause "oneof is unset"))))

(ert-deftest agent-repl-test-wire-host-adopt-error-unknown-arm-is-a-breach ()
  "An arm AdoptHostWorkspaceError does not declare here is refused, never
guessed at."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-adopt-host-workspace-error "{\"noSuchArm\":{}}")
                 '("AdoptHostWorkspaceError" noSuchArm "unknown field"))))

(ert-deftest agent-repl-test-wire-host-adopt-error-arms-pinned ()
  "AdoptHostWorkspaceError's arm set is exactly what the frozen schema
declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_adopt_host_workspace.pb.go" "AdoptHostWorkspaceError")
                       #'string<)
                 (sort (list "unknownWorkspace" "workspaceRefMismatch" "transferringAway" "notYetAdopted" "noTransferAnnounced" "participantNotExpected")
                       #'string<))))

;;;; ---- HostFault kinds (landing 4) --------------------------------------
;;
;; The eight session-controller fault classes, shared verbatim with
;; SessionHealth's SessionFault: the stream reporting a fault never changes
;; its class.

(ert-deftest agent-repl-test-wire-host-fault-shim-start-failed-kind ()
  "HostFault's `shim_start_failed' kind decodes with everything it carries."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-host-fault
                  "{\"detail\":\"d\",\"openedAtMs\":\"5\",\"shimStartFailed\":{\"exitCode\":3,\"stderrTail\":\"panic\"}}")
                 '(:detail "d" :opened-at-ms 5
                   :kind (:arm :shim-start-failed :value (:exit-code 3 :stderr-tail "panic"))))))

(ert-deftest agent-repl-test-wire-host-fault-shim-died-kind ()
  "HostFault's `shim_died' kind decodes with everything it carries."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-host-fault
                  "{\"detail\":\"d\",\"openedAtMs\":\"5\",\"shimDied\":{\"exitCode\":9}}")
                 '(:detail "d" :opened-at-ms 5
                   :kind (:arm :shim-died :value (:exit-code 9))))))

(ert-deftest agent-repl-test-wire-host-fault-link-severed-kind ()
  "HostFault's `link_severed' kind decodes with everything it carries."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-host-fault
                  "{\"detail\":\"d\",\"openedAtMs\":\"5\",\"linkSevered\":{}}")
                 '(:detail "d" :opened-at-ms 5
                   :kind (:arm :link-severed :value nil)))))

(ert-deftest agent-repl-test-wire-host-fault-resume-failed-kind ()
  "HostFault's `resume_failed' kind decodes with everything it carries."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-host-fault
                  "{\"detail\":\"d\",\"openedAtMs\":\"5\",\"resumeFailed\":{\"cause\":\"no transcript\"}}")
                 '(:detail "d" :opened-at-ms 5
                   :kind (:arm :resume-failed :value (:cause "no transcript"))))))

(ert-deftest agent-repl-test-wire-host-fault-bounce-died-kind ()
  "HostFault's `bounce_died' kind decodes with everything it carries."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-host-fault
                  "{\"detail\":\"d\",\"openedAtMs\":\"5\",\"bounceDied\":{}}")
                 '(:detail "d" :opened-at-ms 5
                   :kind (:arm :bounce-died :value nil)))))

(ert-deftest agent-repl-test-wire-host-fault-bounce-unknown-kind ()
  "HostFault's `bounce_unknown' kind decodes with everything it carries."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-host-fault
                  "{\"detail\":\"d\",\"openedAtMs\":\"5\",\"bounceUnknown\":{}}")
                 '(:detail "d" :opened-at-ms 5
                   :kind (:arm :bounce-unknown :value nil)))))

(ert-deftest agent-repl-test-wire-host-fault-classifier-failed-kind ()
  "HostFault's `classifier_failed' kind decodes with everything it carries."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-host-fault
                  "{\"detail\":\"d\",\"openedAtMs\":\"5\",\"classifierFailed\":{\"detail\":\"regex blew up\"}}")
                 '(:detail "d" :opened-at-ms 5
                   :kind (:arm :classifier-failed :value (:detail "regex blew up"))))))

(ert-deftest agent-repl-test-wire-host-fault-shim-reported-kind ()
  "HostFault's `shim_reported' kind decodes with everything it carries."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-host-fault
                  "{\"detail\":\"d\",\"openedAtMs\":\"5\",\"shimReported\":{\"component\":\"stdout\",\"kind\":\"parse\"}}")
                 '(:detail "d" :opened-at-ms 5
                   :kind (:arm :shim-reported :value (:component "stdout" :kind "parse"))))))

(ert-deftest agent-repl-test-wire-host-fault-conversation-abandoned-kind ()
  "HostFault's `conversation_abandoned' kind decodes with everything it
carries."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-host-fault
                  "{\"detail\":\"d\",\"openedAtMs\":\"5\",\"conversationAbandoned\":{\"vendorSessionId\":\"vs-9\"}}")
                 '(:detail "d" :opened-at-ms 5
                   :kind (:arm :conversation-abandoned :value (:vendor-session-id "vs-9"))))))

(ert-deftest agent-repl-test-wire-host-fault-session-absent-kind ()
  "HostFault's `session_absent' kind decodes with everything it carries."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-host-fault
                  "{\"detail\":\"d\",\"openedAtMs\":\"5\",\"sessionAbsent\":{}}")
                 '(:detail "d" :opened-at-ms 5
                   :kind (:arm :session-absent :value nil)))))

(ert-deftest agent-repl-test-wire-host-fault-watch-open-refused-kind ()
  "HostFault's `watch_open_refused' kind decodes with everything it carries."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-host-fault
                  "{\"detail\":\"d\",\"openedAtMs\":\"5\",\"watchOpenRefused\":{\"operation\":\"WatchTranscript\",\"handle\":\"h-3\"}}")
                 '(:detail "d" :opened-at-ms 5
                   :kind (:arm :watch-open-refused :value (:operation "WatchTranscript" :handle "h-3"))))))

(ert-deftest agent-repl-test-wire-host-fault-daemon-state-unreadable-kind ()
  "HostFault's `daemon_state_unreadable' kind decodes with everything it
carries."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-host-fault
                  "{\"detail\":\"d\",\"openedAtMs\":\"5\",\"daemonStateUnreadable\":{\"cause\":\"store closed\"}}")
                 '(:detail "d" :opened-at-ms 5
                   :kind (:arm :daemon-state-unreadable :value (:cause "store closed"))))))

(ert-deftest agent-repl-test-wire-host-fault-adoption-window-expired-kind ()
  "HostFault's `adoption_window_expired' kind decodes with everything it
carries."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-host-fault
                  "{\"detail\":\"d\",\"openedAtMs\":\"5\",\"adoptionWindowExpired\":{\"adoptionWindow\":\"30s\"}}")
                 '(:detail "d" :opened-at-ms 5
                   :kind (:arm :adoption-window-expired :value (:adoption-window "30s"))))))

(ert-deftest agent-repl-test-wire-host-fault-final-answer-unresolved-kind ()
  "HostFault's `final_answer_unresolved' kind decodes with everything it
carries: the turn, the unit, and the `why' that IS this kind's substatus."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-host-fault
                  "{\"detail\":\"d\",\"openedAtMs\":\"5\",\"finalAnswerUnresolved\":{\"turn\":\"turn-7\",\"unit\":\"msg_01:0\",\"why\":\"stalled\"}}")
                 '(:detail "d" :opened-at-ms 5
                   :kind (:arm :final-answer-unresolved
                          :value (:turn "turn-7" :unit "msg_01:0" :why "stalled"))))))

(ert-deftest agent-repl-test-wire-host-fault-unset-kind-is-a-breach ()
  "A fault with no kind is a breach: `detail' supplements the class, never
replaces it."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-host-fault
                  "{\"detail\":\"prose\",\"openedAtMs\":\"5\"}")
                 '("HostFault" kind "oneof is unset"))))

(ert-deftest agent-repl-test-wire-host-fault-unknown-kind-is-a-breach ()
  "A HostFault kind this codec does not know is refused, never dropped."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-host-fault "{\"shimDead\":{}}")
                 '("HostFault" shimDead "unknown field"))))

(ert-deftest agent-repl-test-wire-host-fault-kind-arms-pinned ()
  "HostFault's kind oneof has exactly the fourteen arms decoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_watch_host_workspace.pb.go"
                        "HostFault")
                       #'string<)
                 (sort (list "shimStartFailed" "shimDied" "linkSevered" "resumeFailed" "bounceDied" "bounceUnknown" "classifierFailed" "shimReported" "conversationAbandoned" "sessionAbsent" "watchOpenRefused" "daemonStateUnreadable" "adoptionWindowExpired" "finalAnswerUnresolved")
                       #'string<))))


;;;; ---- Workspace-mutation progress on WatchDaemon --------------------

(defun agent-repl-test-wire-host--create-stage-push (stage-json)
  "Return a WatchDaemonResponse JSON carrying create entered_stage STAGE-JSON."
  (concat "{\"mutationProgress\":{\"opId\":\"op-1\",\"create\":{"
          "\"enteredStage\":" stage-json "}}}"))

(ert-deftest agent-repl-test-wire-host-mutation-progress-stage ()
  "A create stage push decodes to the op id and the stage keyword."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-watch-daemon-response
                  (agent-repl-test-wire-host--create-stage-push "{\"derivingName\":{}}"))
                 '(:arm :mutation-progress
                   :value (:op-id "op-1"
                           :event (:arm :create
                                   :value (:arm :entered-stage :value :deriving-name)))))))

(ert-deftest agent-repl-test-wire-host-create-stage-deriving-name ()
  "The deriving_name arm decodes to `:deriving-name'."
  (should (eq (agent-repl-test-wire-host--decode
               #'agent-repl-wire-decode-workspace-create-stage "{\"derivingName\":{}}")
              :deriving-name)))

(ert-deftest agent-repl-test-wire-host-create-stage-creating-worktree ()
  "The creating_worktree arm decodes to `:creating-worktree'."
  (should (eq (agent-repl-test-wire-host--decode
               #'agent-repl-wire-decode-workspace-create-stage "{\"creatingWorktree\":{}}")
              :creating-worktree)))

(ert-deftest agent-repl-test-wire-host-create-stage-starting-session ()
  "The starting_session arm decodes to `:starting-session'."
  (should (eq (agent-repl-test-wire-host--decode
               #'agent-repl-wire-decode-workspace-create-stage "{\"startingSession\":{}}")
              :starting-session)))

(ert-deftest agent-repl-test-wire-host-create-stage-unset-is-a-breach ()
  "An entered_stage with no arm set is refused, not read as some stage."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-workspace-create-stage "{}")
                 '("WorkspaceCreateStage" stage "oneof is unset"))))

(ert-deftest agent-repl-test-wire-host-create-progress-retired-stage-field-is-a-breach ()
  "A push on the retired field-1 `stage' enum is refused, never misread."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-workspace-create-progress
                  "{\"stage\":\"WORKSPACE_CREATE_STAGE_DERIVING_NAME\"}")
                 '("WorkspaceCreateProgress" stage "unknown field"))))

(ert-deftest agent-repl-test-wire-host-mutation-progress-succeeded ()
  "A succeeded push decodes to the minted ref and the workspace name."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-watch-daemon-response
                  (concat "{\"mutationProgress\":{\"opId\":\"op-2\",\"create\":{"
                          "\"succeeded\":{\"workspace\":{\"id\":\"w\",\"dir\":\"/d\"},"
                          "\"name\":\"minted\"}}}}"))
                 '(:arm :mutation-progress
                   :value (:op-id "op-2"
                           :event (:arm :create
                                   :value (:arm :succeeded
                                           :value (:workspace (:id "w" :dir "/d")
                                                   :name "minted"))))))))

(ert-deftest agent-repl-test-wire-host-mutation-progress-failed-internal ()
  "A failed push with an internal error decodes to the internal sentence."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-watch-daemon-response
                  (concat "{\"mutationProgress\":{\"opId\":\"op-3\",\"create\":{"
                          "\"failed\":{\"internal\":\"materialize worktree: boom\"}}}}"))
                 '(:arm :mutation-progress
                   :value (:op-id "op-3"
                           :event (:arm :create
                                   :value (:arm :failed
                                           :value (:arm :internal
                                                   :value "materialize worktree: boom"))))))))

(ert-deftest agent-repl-test-wire-host-mutation-progress-failed-refusal ()
  "A failed push with a typed refusal decodes to the CreateWorkspaceError arm."
  (let* ((decoded (agent-repl-test-wire-host--decode
                   #'agent-repl-wire-decode-watch-daemon-response
                   (concat "{\"mutationProgress\":{\"opId\":\"op-4\",\"create\":{"
                           "\"failed\":{\"refusal\":{\"namingFailed\":{\"model\":\"haiku\","
                           "\"cause\":\"timeout\",\"attempts\":2,\"answer\":\"\"}}}}}}")))
         (step (plist-get (plist-get (plist-get decoded :value) :event) :value))
         (cause (plist-get step :value))
         (error-val (plist-get cause :value)))
    (should (eq (plist-get step :arm) :failed))
    (should (eq (plist-get cause :arm) :refusal))
    (should (eq (plist-get (plist-get error-val :cause) :arm) :naming-failed))))

(ert-deftest agent-repl-test-wire-host-mutation-progress-unknown-stage-is-a-breach ()
  "An unknown create-stage arm is refused as an unknown field, not guessed at."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-watch-daemon-response
                  (agent-repl-test-wire-host--create-stage-push "{\"teleporting\":{}}"))
                 '("WorkspaceCreateStage" teleporting "unknown field"))))

(ert-deftest agent-repl-test-wire-host-mutation-progress-unknown-stage-is-logged ()
  "An unknown create-stage arm is recorded at ERROR before it is refused."
  (let (logged)
    (cl-letf (((symbol-function 'agent-repl--error)
               (lambda (_scope fmt &rest args) (push (apply #'format fmt args) logged))))
      (condition-case nil
          (agent-repl-wire-decode-workspace-create-stage
           (agent-repl-test-wire-host--parse "{\"teleporting\":{}}"))
        (agent-repl-wire-error nil)))
    (should (equal logged
                   '("elisp.wire.contract-breach message=WorkspaceCreateStage field=teleporting reason=unknown field")))))


(defun agent-repl-test-wire-host--open-stage-push (op-id stage-json)
  "Return a WatchDaemonResponse JSON carrying OP-ID's open entered_stage STAGE-JSON."
  (concat "{\"mutationProgress\":{\"opId\":\"" op-id "\",\"open\":{"
          "\"enteredStage\":" stage-json "}}}"))

(ert-deftest agent-repl-test-wire-host-mutation-progress-open-stage ()
  "An open stage push decodes to the op id and the stage keyword."
  (should (equal (agent-repl-test-wire-host--decode
                  #'agent-repl-wire-decode-watch-daemon-response
                  (agent-repl-test-wire-host--open-stage-push
                   "op-6" "{\"startingSession\":{}}"))
                 '(:arm :mutation-progress
                   :value (:op-id "op-6"
                           :event (:arm :open
                                   :value (:stage :starting-session)))))))

(ert-deftest agent-repl-test-wire-host-open-stage-checking-worktree ()
  "The checking_worktree arm decodes to `:checking-worktree'."
  (should (eq (agent-repl-test-wire-host--decode
               #'agent-repl-wire-decode-workspace-open-stage "{\"checkingWorktree\":{}}")
              :checking-worktree)))

(ert-deftest agent-repl-test-wire-host-open-stage-starting-session ()
  "The starting_session arm decodes to `:starting-session'."
  (should (eq (agent-repl-test-wire-host--decode
               #'agent-repl-wire-decode-workspace-open-stage "{\"startingSession\":{}}")
              :starting-session)))

(ert-deftest agent-repl-test-wire-host-open-stage-reviving ()
  "The conditional reviving arm decodes to `:reviving'."
  (should (eq (agent-repl-test-wire-host--decode
               #'agent-repl-wire-decode-workspace-open-stage "{\"reviving\":{}}")
              :reviving)))

(ert-deftest agent-repl-test-wire-host-open-stage-clearing-closed ()
  "The conditional clearing_closed arm decodes to `:clearing-closed'."
  (should (eq (agent-repl-test-wire-host--decode
               #'agent-repl-wire-decode-workspace-open-stage "{\"clearingClosed\":{}}")
              :clearing-closed)))

(ert-deftest agent-repl-test-wire-host-open-stage-checking-build ()
  "The checking_build arm decodes to `:checking-build'."
  (should (eq (agent-repl-test-wire-host--decode
               #'agent-repl-wire-decode-workspace-open-stage "{\"checkingBuild\":{}}")
              :checking-build)))

(ert-deftest agent-repl-test-wire-host-open-stage-unset-is-a-breach ()
  "An open entered_stage with no arm set is refused, not read as some stage."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-workspace-open-stage "{}")
                 '("WorkspaceOpenStage" stage "oneof is unset"))))

(ert-deftest agent-repl-test-wire-host-open-progress-absent-stage-is-a-breach ()
  "An open progress push with no entered_stage is refused, not read as some stage."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-workspace-open-progress "{}")
                 '("WorkspaceOpenProgress" enteredStage "required message field is absent"))))

(ert-deftest agent-repl-test-wire-host-open-progress-retired-stage-field-is-a-breach ()
  "A push on the retired field-1 open `stage' enum is refused, never misread."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-workspace-open-progress
                  "{\"stage\":\"WORKSPACE_OPEN_STAGE_STARTING_SESSION\"}")
                 '("WorkspaceOpenProgress" stage "unknown field"))))

(ert-deftest agent-repl-test-wire-host-mutation-progress-unknown-open-stage-is-a-breach ()
  "An unknown open-stage arm is refused as an unknown field, not guessed at."
  (should (equal (agent-repl-test-wire-host--breach
                  #'agent-repl-wire-decode-watch-daemon-response
                  (agent-repl-test-wire-host--open-stage-push "op-8" "{\"teleporting\":{}}"))
                 '("WorkspaceOpenStage" teleporting "unknown field"))))

(ert-deftest agent-repl-test-wire-host-mutation-progress-unknown-open-stage-is-logged ()
  "An unknown open-stage arm is recorded at ERROR before it is refused."
  (let (logged)
    (cl-letf (((symbol-function 'agent-repl--error)
               (lambda (_scope fmt &rest args) (push (apply #'format fmt args) logged))))
      (condition-case nil
          (agent-repl-wire-decode-workspace-open-stage
           (agent-repl-test-wire-host--parse "{\"teleporting\":{}}"))
        (agent-repl-wire-error nil)))
    (should (equal logged
                   '("elisp.wire.contract-breach message=WorkspaceOpenStage field=teleporting reason=unknown field")))))


(provide 'test-wire-host)

;;; test-wire-host.el ends here
