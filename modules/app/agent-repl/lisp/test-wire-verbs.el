;;; test-wire-verbs.el --- ERT tests for agent-repl wire-verbs.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs -batch -Q -l ert -l test-wire-verbs.el -f ert-run-tests-batch-and-exit
;;
;; Or interactively:
;;   M-x load-file RET test-wire-verbs.el RET
;;   M-x ert RET t RET
;;
;; The wire-common.el leaf codecs are STUBBED with deterministic fakes so
;; this suite asserts wire-verbs.el's own composition, validation and arm
;; dispatch rather than a sibling module's encoding.  The oneof arm lists are
;; PINNED against the checked-in Go bindings, so an arm added to a proto
;; without being threaded through this codec fails here.

;;; Code:

(require 'cl-lib)

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;; wire-common.el owns this definition; define it when the sibling module is
;; not present so this suite runs standalone.
(unless (get 'agent-repl-wire-error 'error-conditions)
  (define-error 'agent-repl-wire-error "agent-repl wire contract breach"))


;;;; ---- Fixtures --------------------------------------------------------

(defun agent-repl-test-wire-verbs--parse (json)
  "Parse JSON exactly as the transport does."
  (json-parse-string json :object-type 'alist :array-type 'list
                     :null-object :null :false-object :false))

(defmacro agent-repl-test-wire-verbs--with-common (&rest body)
  "Run BODY with wire-common.el's leaf codecs bound to deterministic fakes.
Also silences core.el's logging ladder.  `agent-repl--error' is a pure
logging rung that never signals, so the stub is a no-op: the typed
`agent-repl-wire-error' this suite asserts comes from the codec's own
`signal', never from the logger."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'agent-repl-wire-encode-workspace-ref)
              (lambda (ref) (list (cons 'id (plist-get ref :id))
                                  (cons 'dir (plist-get ref :dir)))))
             ((symbol-function 'agent-repl-wire-decode-workspace-ref)
              (lambda (json) (list :id (cdr (assq 'id json))
                                   :dir (cdr (assq 'dir json)))))
             ((symbol-function 'agent-repl-wire-encode-repository-ref)
              (lambda (ref) (list (cons 'id (plist-get ref :id))
                                  (cons 'dir (plist-get ref :dir)))))
             ((symbol-function 'agent-repl-wire-decode-repository-ref)
              (lambda (json) (list :id (cdr (assq 'id json))
                                   :dir (cdr (assq 'dir json)))))
             ((symbol-function 'agent-repl-wire--decode-bool)
              (lambda (_message field json)
                (eq (cdr (assq field json)) t)))
             ((symbol-function 'agent-repl-wire-encode-user-said)
              (lambda (said) (list (cons 'said (plist-get said :text)))))
             ((symbol-function 'agent-repl-wire-encode-prompt-origin)
              (lambda (origin) (format "ORIGIN:%s" origin)))
             ((symbol-function 'agent-repl-wire-encode-drain-reason)
              (lambda (reason) (list (cons 'reason (format "%s" (plist-get reason :arm))))))
             ((symbol-function 'agent-repl-wire-encode-workspace-priority)
              (lambda (priority) (list (cons 'level (format "%s" (plist-get priority :arm))))))
             ((symbol-function 'agent-repl-wire-decode-turn-id)
              (lambda (json) (list :value (cdr (assq 'value json)))))
             ((symbol-function 'agent-repl-wire-encode-feed-id)
              (lambda (feedid) (list (cons 'value (plist-get feedid :value)))))
             ((symbol-function 'agent-repl-wire-decode-feed-id)
              (lambda (json) (list :value (cdr (assq 'value json)))))
             ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
             ((symbol-function 'agent-repl--error) (lambda (&rest _) nil)))
     ,@body))

(defconst agent-repl-test-wire-verbs--ref '(:id "ws-1" :dir "/w/one")
  "A decoded WorkspaceRef, echoed verbatim as the contract requires.")

(defconst agent-repl-test-wire-verbs--repo '(:id "repo-1" :dir "/r/one")
  "A decoded RepositoryRef, as the roster's repo sections supply it.")

(defconst agent-repl-test-wire-verbs--simple-verbs
  '(("OpenWorkspace"
     agent-repl-wire-encode-open-workspace-request
     agent-repl-wire-decode-open-workspace-response)
    ("KillWorkspace"
     agent-repl-wire-encode-kill-workspace-request
     agent-repl-wire-decode-kill-workspace-response)
    ("NukeWorkspace"
     agent-repl-wire-encode-nuke-workspace-request
     agent-repl-wire-decode-nuke-workspace-response)
    ("FinishWorkspace"
     agent-repl-wire-encode-finish-workspace-request
     agent-repl-wire-decode-finish-workspace-response))
  "The verbs whose result arms are both empty.
All but FinishWorkspace also take a {workspace}-only request; FinishWorkspace
names its requester and placement as well, so the request tests read
`agent-repl-test-wire-verbs--simple-request-verbs'.")

(defconst agent-repl-test-wire-verbs--simple-request-verbs
  (cl-remove "FinishWorkspace" agent-repl-test-wire-verbs--simple-verbs
             :key #'car :test #'equal)
  "The simple verbs whose request is {workspace} and nothing else.")

(defconst agent-repl-test-wire-verbs--result-oneofs
  ;; A third element, when present, is the arm set this codec decodes instead of
  ;; the usual two: CreateWorkspaceResponse carries the option-B `accepted' ack
  ;; beside success and error.
  ;; CreateWorkspaceResponse's `async_merge_accepted' answers only an async
  ;; merge, which is an agent's request and never the editor's, and its
  ;; `waiting_for_network' answers a synchronous create held for the network.
  '(("agentrepl/v1/endpoint_create_workspace.pb.go" "CreateWorkspaceResponse"
     ("accepted" "asyncMergeAccepted" "error" "success" "waitingForNetwork"))
    ("agentrepl/v1/endpoint_open_workspace.pb.go" "OpenWorkspaceResponse")
    ("agentrepl/v1/endpoint_list_workspace_transcripts.pb.go" "ListWorkspaceTranscriptsResponse")
    ("agentrepl/v1/endpoint_bind_workspace_session.pb.go" "BindWorkspaceSessionResponse")
    ("agentrepl/v1/endpoint_close_workspace.pb.go" "CloseWorkspaceResponse")
    ("agentrepl/v1/endpoint_kill_workspace.pb.go" "KillWorkspaceResponse"
     ("accepted" "error" "success"))
    ("agentrepl/v1/endpoint_nuke_workspace.pb.go" "NukeWorkspaceResponse"
     ("accepted" "error" "success"))
    ("agentrepl/v1/endpoint_finish_workspace.pb.go" "FinishWorkspaceResponse")
    ("agentrepl/v1/endpoint_restart_workspace.pb.go" "RestartWorkspaceResponse")
    ("agentrepl/v1/endpoint_set_workspace_priority.pb.go" "SetWorkspacePriorityResponse")
    ("agentrepl/v1/endpoint_submit_prompt.pb.go" "SubmitPromptResponse")
    ("agentrepl/v1/endpoint_update_shutdown_schedule.pb.go" "UpdateShutdownScheduleResponse")
    ("agentrepl/v1/endpoint_update_merge_queue.pb.go" "UpdateMergeQueueResponse")
    ("agentrepl/v1/endpoint_daemon_health.pb.go" "DaemonHealthResponse")
    ("agentrepl/v1/endpoint_session_health.pb.go" "SessionHealthResponse")
    ("agentrepl/v1/endpoint_register_repository.pb.go" "RegisterRepositoryResponse"))
  "Every response whose result oneof this codec decodes, with its binding.")


;;;; ---- CreateWorkspaceRequest: the standard form ----------------------

(defun agent-repl-test-wire-verbs--encode-create (request)
  "Encode the CreateWorkspaceRequest REQUEST with its required facts filled.
A test about one fact names only that fact: an absent requester is the
user, an absent context origin is fresh, and a form value without a
relationship is a peer."
  (let* ((form (plist-get request :form))
         (value (plist-get form :value)))
    (when (and form (not (plist-member value :relationship)))
      (setq request (plist-put (copy-sequence request) :form
                               (list :arm (plist-get form :arm)
                                     :value (append value (list :relationship '(:arm :peer)))))))
    (agent-repl-wire-encode-create-workspace-request
     (append request
             (unless (plist-member request :requester) (list :requester '(:arm :user)))
             (unless (plist-member request :context-origin) (list :context-origin '(:arm :fresh)))))))

(ert-deftest agent-repl-test-wire-verbs-create-standard-minimal ()
  "A standard create with no optional facts carries its repository, its
relationship, its requester and its context origin, and nothing else."
  (agent-repl-test-wire-verbs--with-common
    (let* ((request (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard :value nil)))
           (encoded (agent-repl-test-wire-verbs--encode-create request)))
      (should (equal (json-serialize encoded)
                     (concat "{\"repository\":{\"id\":\"repo-1\",\"dir\":\"/r/one\"},"
                             "\"standard\":{\"relationship\":{\"peer\":{}}},"
                             "\"requester\":{\"user\":{}},"
                             "\"contextOrigin\":{\"fresh\":{}}}"))))))

(ert-deftest agent-repl-test-wire-verbs-create-standard-initial-prompt ()
  "A standard create's initial prompt encodes through the UserSaid codec."
  (agent-repl-test-wire-verbs--with-common
    (let* ((request (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard
                                  :value (:initial-prompt (:text "go")))))
           (encoded (agent-repl-test-wire-verbs--encode-create request)))
      (should (equal (cdr (assq 'initialPrompt (cdr (assq 'standard encoded))))
                     '((said . "go")))))))

(ert-deftest agent-repl-test-wire-verbs-create-standard-name ()
  "A caller-supplied workspace name rides the standard form."
  (agent-repl-test-wire-verbs--with-common
    (let* ((request (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard :value (:name "wire-b"))))
           (encoded (agent-repl-test-wire-verbs--encode-create request)))
      (should (equal (cdr (assq 'name (cdr (assq 'standard encoded)))) "wire-b")))))

(ert-deftest agent-repl-test-wire-verbs-create-standard-absent-optionals ()
  "Absent standard-form optionals are OMITTED, never spelled as sentinels:
the required relationship is all the form carries."
  (agent-repl-test-wire-verbs--with-common
    (let* ((request (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard :value nil)))
           (standard (cdr (assq 'standard
                                (agent-repl-test-wire-verbs--encode-create request)))))
      (should (equal standard '((relationship . ((peer)))))))))

;;;; ---- CreateWorkspaceRequest: the one-shot form ----------------------

(ert-deftest agent-repl-test-wire-verbs-create-one-shot-is-its-prompt-alone ()
  "A one-shot encodes its prompt and NOTHING else: there is no finish arm."
  (agent-repl-test-wire-verbs--with-common
    (let* ((request (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :one-shot
                                  :value (:prompt (:text "ship it")))))
           (one-shot (cdr (assq 'oneShot
                                (agent-repl-test-wire-verbs--encode-create request)))))
      (should (equal (json-serialize one-shot)
                     "{\"prompt\":{\"said\":\"ship it\"},\"relationship\":{\"peer\":{}}}")))))

(ert-deftest agent-repl-test-wire-verbs-create-one-shot-carries-its-context-workspace ()
  "A one-shot's context workspace encodes inside the one-shot arm, beside the prompt."
  (agent-repl-test-wire-verbs--with-common
    (let* ((request (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :one-shot
                                  :value (:prompt (:text "fix its feed")
                                          :context (:id "ws-2" :dir "/w/two")))))
           (one-shot (cdr (assq 'oneShot
                                (agent-repl-test-wire-verbs--encode-create request)))))
      (should (equal (json-serialize one-shot)
                     (concat "{\"prompt\":{\"said\":\"fix its feed\"},\"relationship\":{\"peer\":{}},"
                             "\"context\":{\"workspace\":{\"id\":\"ws-2\",\"dir\":\"/w/two\"}}}"))))))

(ert-deftest agent-repl-test-wire-verbs-create-one-shot-without-prompt-refused ()
  "A one-shot IS its prompt: a one-shot without one never reaches the wire."
  (agent-repl-test-wire-verbs--with-common
    (should-error
     (agent-repl-test-wire-verbs--encode-create
      (list :repository agent-repl-test-wire-verbs--repo
            :form '(:arm :one-shot :value nil)))
     :type 'agent-repl-wire-error)))


;;;; ---- CreateWorkspaceRequest: shared facts and refusals --------------

(ert-deftest agent-repl-test-wire-verbs-create-without-form-refused ()
  "The form arm IS the creation form, so a request without one is refused."
  (agent-repl-test-wire-verbs--with-common
    (should-error
     (agent-repl-test-wire-verbs--encode-create
      (list :repository agent-repl-test-wire-verbs--repo))
     :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-create-without-repository-refused ()
  "A create with no repository is incomplete and errors before send."
  (agent-repl-test-wire-verbs--with-common
    (should-error
     (agent-repl-test-wire-verbs--encode-create '(:form (:arm :standard :value nil)))
     :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-create-unknown-form-arm-refused ()
  "An arm keyword the form oneof does not declare is refused."
  (agent-repl-test-wire-verbs--with-common
    (should-error
     (agent-repl-test-wire-verbs--encode-create
      (list :repository agent-repl-test-wire-verbs--repo
            :form '(:arm :rehydrate :value nil)))
     :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-create-standard-without-relationship-refused ()
  "Nothing is implied: a standard form without a relationship never reaches the wire."
  (agent-repl-test-wire-verbs--with-common
    (should-error
     (agent-repl-wire-encode-create-workspace-request
      (list :repository agent-repl-test-wire-verbs--repo
            :form '(:arm :standard :value nil)
            :requester '(:arm :user) :context-origin '(:arm :fresh)))
     :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-create-one-shot-without-relationship-refused ()
  "Nothing is implied: a one-shot form without a relationship never reaches the wire."
  (agent-repl-test-wire-verbs--with-common
    (should-error
     (agent-repl-wire-encode-create-workspace-request
      (list :repository agent-repl-test-wire-verbs--repo
            :form '(:arm :one-shot :value (:prompt (:text "go")))
            :requester '(:arm :user) :context-origin '(:arm :fresh)))
     :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-create-without-requester-refused ()
  "Nothing is implied: a create without a requester never reaches the wire."
  (agent-repl-test-wire-verbs--with-common
    (should-error
     (agent-repl-wire-encode-create-workspace-request
      (list :repository agent-repl-test-wire-verbs--repo
            :form '(:arm :standard :value (:relationship (:arm :peer)))
            :context-origin '(:arm :fresh)))
     :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-create-without-context-origin-refused ()
  "Nothing is implied: a create without a context origin never reaches the wire."
  (agent-repl-test-wire-verbs--with-common
    (should-error
     (agent-repl-wire-encode-create-workspace-request
      (list :repository agent-repl-test-wire-verbs--repo
            :form '(:arm :standard :value (:relationship (:arm :peer)))
            :requester '(:arm :user)))
     :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-create-child-relationship ()
  "A child relationship names its parent inside the form."
  (agent-repl-test-wire-verbs--with-common
    (let* ((request (list :repository agent-repl-test-wire-verbs--repo
                          :form (list :arm :standard
                                      :value (list :relationship
                                                   (list :arm :child
                                                         :value (list :parent agent-repl-test-wire-verbs--ref))))))
           (standard (cdr (assq 'standard (agent-repl-test-wire-verbs--encode-create request)))))
      (should (equal (json-serialize standard)
                     "{\"relationship\":{\"child\":{\"parent\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"}}}}")))))

(ert-deftest agent-repl-test-wire-verbs-create-standard-owner-notice ()
  "A standard create's owner notice rides the form as `ownerNotice'."
  (agent-repl-test-wire-verbs--with-common
    (let* ((request (list :repository agent-repl-test-wire-verbs--repo
                          :form (list :arm :standard
                                      :value (list :relationship
                                                   (list :arm :child
                                                         :value (list :parent agent-repl-test-wire-verbs--ref))
                                                   :owner-notice "coming your way"))))
           (standard (cdr (assq 'standard (agent-repl-test-wire-verbs--encode-create request)))))
      (should (equal (cdr (assq 'ownerNotice standard)) '((text . "coming your way")))))))

(ert-deftest agent-repl-test-wire-verbs-create-child-without-parent-refused ()
  "A child relationship IS its parent's, so one without a parent never reaches the wire."
  (agent-repl-test-wire-verbs--with-common
    (should-error
     (agent-repl-test-wire-verbs--encode-create
      (list :repository agent-repl-test-wire-verbs--repo
            :form '(:arm :standard :value (:relationship (:arm :child :value nil)))))
     :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-create-context-origin-fork ()
  "A forked context names the workspace whose conversation is copied."
  (agent-repl-test-wire-verbs--with-common
    (let ((encoded (agent-repl-test-wire-verbs--encode-create
                    (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard :value nil)
                          :context-origin (list :arm :fork :value agent-repl-test-wire-verbs--ref)))))
      (should (equal (json-serialize (cdr (assq 'contextOrigin encoded)))
                     "{\"fork\":{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"}}}")))))

(ert-deftest agent-repl-test-wire-verbs-create-context-origin-fork-without-workspace-refused ()
  "A fork IS its source, so one without a workspace never reaches the wire."
  (agent-repl-test-wire-verbs--with-common
    (should-error
     (agent-repl-test-wire-verbs--encode-create
      (list :repository agent-repl-test-wire-verbs--repo
            :form '(:arm :standard :value nil)
            :context-origin '(:arm :fork :value nil)))
     :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-create-requester-workspace ()
  "A workspace requester names the asking workspace."
  (agent-repl-test-wire-verbs--with-common
    (let ((encoded (agent-repl-test-wire-verbs--encode-create
                    (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard :value nil)
                          :requester (list :arm :workspace :value agent-repl-test-wire-verbs--ref)))))
      (should (equal (json-serialize (cdr (assq 'requester encoded)))
                     "{\"workspace\":{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"}}}")))))

(ert-deftest agent-repl-test-wire-verbs-create-model-present ()
  "A chosen session model rides the request."
  (agent-repl-test-wire-verbs--with-common
    (let ((encoded (agent-repl-test-wire-verbs--encode-create
                    (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard :value nil)
                          :model "opus"))))
      (should (equal (cdr (assq 'model encoded)) "opus")))))

(ert-deftest agent-repl-test-wire-verbs-create-model-absent-omitted ()
  "An absent model omits the key: the daemon's default, not an empty string."
  (agent-repl-test-wire-verbs--with-common
    (let ((encoded (agent-repl-test-wire-verbs--encode-create
                    (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard :value nil)))))
      (should-not (assq 'model encoded)))))

(ert-deftest agent-repl-test-wire-verbs-create-priority-present ()
  "A creation-time priority delegates to the WorkspacePriority codec."
  (agent-repl-test-wire-verbs--with-common
    (let ((encoded (agent-repl-test-wire-verbs--encode-create
                    (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard :value nil)
                          :priority '(:arm :p1 :value nil)))))
      (should (equal (cdr (assq 'priority encoded)) '((level . ":p1")))))))

(ert-deftest agent-repl-test-wire-verbs-create-priority-absent-omitted ()
  "An absent priority omits the key: unprioritized, never a sentinel level."
  (agent-repl-test-wire-verbs--with-common
    (let ((encoded (agent-repl-test-wire-verbs--encode-create
                    (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard :value nil)))))
      (should-not (assq 'priority encoded)))))

(ert-deftest agent-repl-test-wire-verbs-create-allow-ungated-present ()
  "Presence IS the ungated consent, so it rides as the empty message."
  (agent-repl-test-wire-verbs--with-common
    (let ((encoded (agent-repl-test-wire-verbs--encode-create
                    (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard :value nil)
                          :allow-ungated t))))
      (should (equal (json-serialize (list (assq 'allowUngated encoded)))
                     "{\"allowUngated\":{}}")))))

(ert-deftest agent-repl-test-wire-verbs-create-allow-ungated-absent-omitted ()
  "Absent consent omits the key entirely: no consent was given."
  (agent-repl-test-wire-verbs--with-common
    (let ((encoded (agent-repl-test-wire-verbs--encode-create
                    (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard :value nil)))))
      (should-not (assq 'allowUngated encoded)))))


;;;; ---- CreateWorkspaceResponse ----------------------------------------

(ert-deftest agent-repl-test-wire-verbs-create-response-success ()
  "A create success carries the minted workspace identity."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"success\":{\"workspace\":{\"id\":\"ws-9\",\"dir\":\"/w/nine\"}}}"))
                   '(:arm :success :value (:workspace (:id "ws-9" :dir "/w/nine")))))))

(ert-deftest agent-repl-test-wire-verbs-create-response-error-without-cause ()
  "A CreateWorkspaceError with no cause arm is a contract breach.
THE ARM IS THE REFUSAL, so a bare error says nothing the caller can act on."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-create-workspace-response
                   (agent-repl-test-wire-verbs--parse "{\"error\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-create-response-success-without-workspace ()
  "A success missing the non-optional workspace is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-create-workspace-response
                   (agent-repl-test-wire-verbs--parse "{\"success\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-create-response-future-error-arm ()
  "A future CreateWorkspaceError arm arrives as an unknown key and is loud."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-create-workspace-response
                   (agent-repl-test-wire-verbs--parse "{\"error\":{\"unknownRepo\":{}}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-create-response-unknown-field ()
  "An unknown field on the response is refused, never silently dropped."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-create-workspace-response
                   (agent-repl-test-wire-verbs--parse "{\"success\":{},\"pending\":{}}"))
                  :type 'agent-repl-wire-error)))


(ert-deftest agent-repl-test-wire-verbs-create-response-accepted-own-create ()
  "An ack for a create of its own carries the op id and no joined create."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-response
                    (agent-repl-test-wire-verbs--parse "{\"accepted\":{\"opId\":\"op-7\"}}"))
                   '(:arm :accepted :value (:op-id "op-7" :joined nil))))))

(ert-deftest agent-repl-test-wire-verbs-create-response-accepted-joined ()
  "An ack that joined an identical pending create names that create."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"accepted\":{\"opId\":\"op-8\",\"joined\":{\"create\":{\"id\":\"create-1\"}}}}"))
                   '(:arm :accepted :value (:op-id "op-8" :joined (:create (:id "create-1"))))))))

(ert-deftest agent-repl-test-wire-verbs-create-response-waiting-for-network ()
  "The synchronous answer for a create held for the network names it."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"waitingForNetwork\":{\"create\":{\"id\":\"create-2\"}}}"))
                   '(:arm :waiting-for-network :value (:create (:id "create-2")))))))

(ert-deftest agent-repl-test-wire-verbs-create-waiting-without-a-create-is-a-breach ()
  "A held create with no pending ref says nothing a caller can follow."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-create-workspace-response
                   (agent-repl-test-wire-verbs--parse "{\"waitingForNetwork\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-pending-create-ref-without-an-id-is-a-breach ()
  "A pending ref with no id is no identity at all."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-pending-create-ref
                   (agent-repl-test-wire-verbs--parse "{}"))
                  :type 'agent-repl-wire-error)))


;;;; ---- The {workspace}-request / empty-result verbs -------------------

(ert-deftest agent-repl-test-wire-verbs-simple-request-echoes-ref ()
  "Each simple verb's request echoes the WorkspaceRef verbatim."
  (agent-repl-test-wire-verbs--with-common
    (dolist (verb agent-repl-test-wire-verbs--simple-request-verbs)
      (should (equal (funcall (nth 1 verb)
                              (list :workspace agent-repl-test-wire-verbs--ref))
                     '((workspace . ((id . "ws-1") (dir . "/w/one")))))))))

(ert-deftest agent-repl-test-wire-verbs-simple-request-without-workspace ()
  "Each simple verb refuses a request with no workspace."
  (agent-repl-test-wire-verbs--with-common
    (dolist (verb agent-repl-test-wire-verbs--simple-request-verbs)
      (should-error (funcall (nth 1 verb) nil) :type 'agent-repl-wire-error))))

(ert-deftest agent-repl-test-wire-verbs-simple-response-success ()
  "Each simple verb's empty success decodes to the success arm."
  (agent-repl-test-wire-verbs--with-common
    (dolist (verb agent-repl-test-wire-verbs--simple-verbs)
      (should (equal (funcall (nth 2 verb)
                              (agent-repl-test-wire-verbs--parse "{\"success\":{}}"))
                     '(:arm :success :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-simple-response-error ()
  "Each simple verb's error decodes to the error arm carrying its cause."
  (agent-repl-test-wire-verbs--with-common
    (dolist (verb agent-repl-test-wire-verbs--simple-verbs)
      (should (equal (funcall (nth 2 verb)
                              (agent-repl-test-wire-verbs--parse
                               "{\"error\":{\"unknownWorkspace\":{}}}"))
                     '(:arm :error
                       :value (:cause (:arm :unknown-workspace :value nil))))))))

(ert-deftest agent-repl-test-wire-verbs-simple-response-error-without-cause ()
  "Each simple verb refuses an error with no cause arm."
  (agent-repl-test-wire-verbs--with-common
    (dolist (verb agent-repl-test-wire-verbs--simple-verbs)
      (should-error (funcall (nth 2 verb)
                             (agent-repl-test-wire-verbs--parse "{\"error\":{}}"))
                    :type 'agent-repl-wire-error))))

(ert-deftest agent-repl-test-wire-verbs-simple-response-unset-oneof ()
  "A response with no result arm set is a contract breach for every verb."
  (agent-repl-test-wire-verbs--with-common
    (dolist (verb agent-repl-test-wire-verbs--simple-verbs)
      (should-error (funcall (nth 2 verb) (agent-repl-test-wire-verbs--parse "{}"))
                    :type 'agent-repl-wire-error))))

(ert-deftest agent-repl-test-wire-verbs-simple-response-two-arms ()
  "A response with both result arms set is a contract breach for every verb."
  (agent-repl-test-wire-verbs--with-common
    (dolist (verb agent-repl-test-wire-verbs--simple-verbs)
      (should-error (funcall (nth 2 verb)
                             (agent-repl-test-wire-verbs--parse
                              "{\"success\":{},\"error\":{}}"))
                    :type 'agent-repl-wire-error))))

(ert-deftest agent-repl-test-wire-verbs-simple-response-unknown-field ()
  "A response field outside the result oneof is refused for every verb."
  (agent-repl-test-wire-verbs--with-common
    (dolist (verb agent-repl-test-wire-verbs--simple-verbs)
      (should-error (funcall (nth 2 verb)
                             (agent-repl-test-wire-verbs--parse "{\"queued\":{}}"))
                    :type 'agent-repl-wire-error))))


;;;; ---- CloseWorkspace --------------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-close-request-echoes-ref ()
  "CloseWorkspaceRequest echoes the WorkspaceRef verbatim."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-encode-close-workspace-request
                    (list :workspace agent-repl-test-wire-verbs--ref :requester '(:arm :user)))
                   '((workspace . ((id . "ws-1") (dir . "/w/one")))
                     (requester . ((user))))))))

(ert-deftest agent-repl-test-wire-verbs-close-request-without-requester-refused ()
  "A close naming no requester is refused before the send: nothing is implied."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (cdr (should-error
                         (agent-repl-wire-encode-close-workspace-request
                          (list :workspace agent-repl-test-wire-verbs--ref))
                         :type 'agent-repl-wire-error))
                   '("CloseWorkspaceRequest" "requester" "required field is unset")))))

(ert-deftest agent-repl-test-wire-verbs-close-response-blocked ()
  "A blocked close decodes to the cause arm; the reasons ride the footer."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-close-workspace-response
                    (agent-repl-test-wire-verbs--parse "{\"error\":{\"blocked\":{}}}"))
                   (list :arm :error :value
                         (list :cause (list :arm :blocked :value
                                            '(:turn-in-flight nil :live-work 0 :held-prompts 0 :merge-queued nil :summary ""))))))))

(ert-deftest agent-repl-test-wire-verbs-close-response-error-without-cause ()
  "A close error with no cause arm set is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-close-workspace-response
                   (agent-repl-test-wire-verbs--parse "{\"error\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-close-response-unknown-cause ()
  "A future close-refusal arm arrives as an unknown key and is loud."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-close-workspace-response
                   (agent-repl-test-wire-verbs--parse "{\"error\":{\"heldPrompts\":{}}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-close-response-success ()
  "A done close decodes to the success arm's `closed' outcome."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-close-workspace-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{\"closed\":{}}}"))
                   '(:arm :success :value (:outcome (:arm :closed :value nil)))))))

(ert-deftest agent-repl-test-wire-verbs-close-response-success-without-outcome ()
  "A close success naming no outcome is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-close-workspace-response
                   (agent-repl-test-wire-verbs--parse "{\"success\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-close-response-closes-when-turn-ends-is-a-breach ()
  "Only a workspace closing itself is told its close waits for its turn, so
that outcome answering the editor\='s close is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-close-workspace-response
                   (agent-repl-test-wire-verbs--parse "{\"success\":{\"closesWhenTurnEnds\":{}}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-close-success-outcome-arms-pinned ()
  "CloseWorkspaceSuccess's outcome is `closed\', decoded here, and
`closes_when_turn_ends\', which answers only a workspace closing itself."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_close_workspace.pb.go" "CloseWorkspaceSuccess")
                       #'string<)
                 '("closed" "closesWhenTurnEnds"))))


;;;; ---- RestartWorkspace ------------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-restart-request-carries-only-the-workspace ()
  "A restart request is the workspace alone: no force field, ever."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-restart-workspace-request
                     (list :workspace agent-repl-test-wire-verbs--ref)))
                   "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"}}"))))

(ert-deftest agent-repl-test-wire-verbs-restart-without-workspace ()
  "A restart with no workspace is incomplete and errors before send."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-restart-workspace-request '())
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-restart-response-success ()
  "An accepted restart decodes to the empty success arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-restart-workspace-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{}}"))
                   '(:arm :success :value nil)))))


;;;; ---- SetWorkspacePriority --------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-set-priority-present ()
  "A set priority delegates to the WorkspacePriority codec."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-encode-set-workspace-priority-request
                    (list :workspace agent-repl-test-wire-verbs--ref
                          :priority '(:arm :p2 :value nil)))
                   '((workspace . ((id . "ws-1") (dir . "/w/one")))
                     (priority . ((level . ":p2"))))))))

(ert-deftest agent-repl-test-wire-verbs-set-priority-absent-clears ()
  "An absent priority OMITS the field, which is how a priority is cleared."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-encode-set-workspace-priority-request
                    (list :workspace agent-repl-test-wire-verbs--ref))
                   '((workspace . ((id . "ws-1") (dir . "/w/one"))))))))

(ert-deftest agent-repl-test-wire-verbs-set-priority-without-workspace ()
  "A priority change with no workspace is incomplete and errors before send."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-set-workspace-priority-request
                   '(:priority (:arm :p1 :value nil)))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-set-priority-response-error ()
  "A refused priority change decodes to the error arm carrying its cause."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-set-workspace-priority-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"error\":{\"notYetAdopted\":{}}}"))
                   '(:arm :error
                     :value (:cause (:arm :not-yet-adopted :value nil)))))))


;;;; ---- SetWorkspaceHidden ----------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-validate-name-request-carries-the-name ()
  "A name check carries the name as the user typed it."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-encode-validate-workspace-name-request '(:name "fix-login"))
                   '((name . "fix-login"))))))

(ert-deftest agent-repl-test-wire-verbs-validate-name-request-without-name ()
  "A name check with no name is incomplete and errors before send."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-validate-workspace-name-request '(:name ""))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-validate-name-legal-verdict ()
  "A legal verdict decodes as the empty `legal' arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-validate-workspace-name-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{\"legal\":{}}}"))
                   '(:arm :success :value (:verdict (:arm :legal :value nil)))))))

(ert-deftest agent-repl-test-wire-verbs-validate-name-invalid-verdict ()
  "An invalid verdict decodes as the create's own `invalid_name' shape."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-validate-workspace-name-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"success\":{\"invalid\":{\"name\":\"Fix It\",\"component\":\"Fix It\",\"slug\":{}}}}"))
                   '(:arm :success
                     :value (:verdict (:arm :invalid
                                       :value (:name "Fix It" :component "Fix It" :rule :slug))))))))

(ert-deftest agent-repl-test-wire-verbs-validate-name-error-arm ()
  "The daemon's error arm decodes as an empty error."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-validate-workspace-name-response
                    (agent-repl-test-wire-verbs--parse "{\"error\":{}}"))
                   '(:arm :error :value nil)))))

(ert-deftest agent-repl-test-wire-verbs-validate-name-unset-verdict-is-a-breach ()
  "A success with no verdict set is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-validate-workspace-name-response
                   (agent-repl-test-wire-verbs--parse "{\"success\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-set-hidden-hide ()
  "A hide carries the workspace and the empty `hide' arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-encode-set-workspace-hidden-request
                    (list :workspace agent-repl-test-wire-verbs--ref
                          :visibility '(:arm :hide :value nil)))
                   '((workspace . ((id . "ws-1") (dir . "/w/one")))
                     (hide))))))

(ert-deftest agent-repl-test-wire-verbs-set-hidden-show ()
  "A show carries the workspace and the empty `show' arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-encode-set-workspace-hidden-request
                    (list :workspace agent-repl-test-wire-verbs--ref
                          :visibility '(:arm :show :value nil)))
                   '((workspace . ((id . "ws-1") (dir . "/w/one")))
                     (show))))))

(ert-deftest agent-repl-test-wire-verbs-set-hidden-without-visibility ()
  "A hidden change with no visibility arm is incomplete and errors before send."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-set-workspace-hidden-request
                   (list :workspace agent-repl-test-wire-verbs--ref))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-set-hidden-without-workspace ()
  "A hidden change with no workspace is incomplete and errors before send."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-set-workspace-hidden-request
                   '(:visibility (:arm :hide :value nil)))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-set-hidden-response-success ()
  "An accepted hidden change decodes to the empty success arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-set-workspace-hidden-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{}}"))
                   '(:arm :success :value nil)))))

(ert-deftest agent-repl-test-wire-verbs-set-hidden-response-error ()
  "A refused hidden change decodes to the error arm carrying its cause."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-set-workspace-hidden-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"error\":{\"workspaceRefMismatch\":{\"registryDir\":\"/w/registry\"}}}"))
                   '(:arm :error
                     :value (:cause (:arm :workspace-ref-mismatch :value (:registry-dir "/w/registry"))))))))

(ert-deftest agent-repl-test-wire-verbs-set-hidden-error-refuses-an-unknown-arm ()
  "An arm this codec does not know is refused as an unknown field."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-set-workspace-hidden-error
                   (agent-repl-test-wire-verbs--parse "{\"stillHidden\":{}}"))
                  :type 'agent-repl-wire-error)))

;;;; ---- UpdateSidebarView ----------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-update-sidebar-view-folds-a-section ()
  "A fold carries its section's empty arm and the fold's."
  (agent-repl-test-wire-verbs--with-common
    (dolist (case '((:hidden hidden :collapse collapse)
                    (:open open :expand expand)
                    (:recently-merged recentlyMerged :collapse collapse)))
      (pcase-let ((`(,section ,wire-section ,fold ,wire-fold) case))
        (should (equal (agent-repl-wire-encode-update-sidebar-view-request
                        (list :change (list :arm :fold-section
                                            :value (list :section (list :arm section)
                                                         :fold (list :arm fold)))))
                       (list (list 'foldSection (list wire-section) (list wire-fold)))))))))

(ert-deftest agent-repl-test-wire-verbs-update-sidebar-view-refuses-what-it-cannot-send ()
  "An unset change, an unset section or fold, and a repository fold are refused before send."
  (agent-repl-test-wire-verbs--with-common
    (dolist (request '(nil
                       (:change (:arm :fold-section :value (:fold (:arm :collapse))))
                       (:change (:arm :fold-section :value (:section (:arm :hidden))))
                       (:change (:arm :fold-section :value (:section (:arm :repository) :fold (:arm :collapse))))))
      (should-error (agent-repl-wire-encode-update-sidebar-view-request request)
                    :type 'agent-repl-wire-error))))

(ert-deftest agent-repl-test-wire-verbs-update-sidebar-view-response-success ()
  "An accepted fold decodes to the empty success arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-update-sidebar-view-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{}}"))
                   '(:arm :success :value nil)))))

(ert-deftest agent-repl-test-wire-verbs-update-sidebar-view-response-error ()
  "Each refusal decodes to its own empty cause arm."
  (agent-repl-test-wire-verbs--with-common
    (dolist (case '(("unknownRepository" . :unknown-repository)
                    ("unknownSubsection" . :unknown-subsection)
                    ("holdsCurrent" . :holds-current)))
      (should (equal (agent-repl-wire-decode-update-sidebar-view-response
                      (agent-repl-test-wire-verbs--parse (format "{\"error\":{\"%s\":{}}}" (car case))))
                     (list :arm :error :value (list :cause (list :arm (cdr case) :value nil))))))))

(ert-deftest agent-repl-test-wire-verbs-update-sidebar-view-error-refuses-an-unknown-arm ()
  "A refusal arm this codec does not know is refused as an unknown field."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-update-sidebar-view-error
                   (agent-repl-test-wire-verbs--parse "{\"stillFolded\":{}}"))
                  :type 'agent-repl-wire-error)))

;;;; ---- RetryVendorError -----------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-retry-vendor-error-request ()
  "The retry asked for now carries the workspace and nothing else."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-encode-retry-vendor-error-request
                    (list :workspace agent-repl-test-wire-verbs--ref))
                   '((workspace . ((id . "ws-1") (dir . "/w/one"))))))))

(ert-deftest agent-repl-test-wire-verbs-retry-vendor-error-without-workspace ()
  "A retry naming no workspace is incomplete and errors before send."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-retry-vendor-error-request nil)
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-retry-vendor-error-retried ()
  "A made retry decodes to the empty `retried' outcome."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-retry-vendor-error-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{\"retried\":{}}}"))
                   '(:arm :success :value (:outcome (:arm :retried :value nil)))))))

(ert-deftest agent-repl-test-wire-verbs-retry-vendor-error-nothing-scheduled ()
  "No retry scheduled decodes to the empty `nothing_scheduled' outcome."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-retry-vendor-error-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{\"nothingScheduled\":{}}}"))
                   '(:arm :success :value (:outcome (:arm :nothing-scheduled :value nil)))))))

(ert-deftest agent-repl-test-wire-verbs-retry-vendor-error-busy ()
  "A busy answer carries the shim's detail."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-retry-vendor-error-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"success\":{\"busy\":{\"detail\":\"a turn is in flight\"}}}"))
                   '(:arm :success :value (:outcome (:arm :busy :value (:detail "a turn is in flight"))))))))

(ert-deftest agent-repl-test-wire-verbs-retry-vendor-error-ended ()
  "An ended answer carries the shim's detail."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-retry-vendor-error-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"success\":{\"ended\":{\"detail\":\"standing down\"}}}"))
                   '(:arm :success :value (:outcome (:arm :ended :value (:detail "standing down"))))))))

(ert-deftest agent-repl-test-wire-verbs-retry-vendor-error-no-session ()
  "A workspace with no session decodes to the `no_session' refusal."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-retry-vendor-error-response
                    (agent-repl-test-wire-verbs--parse "{\"error\":{\"noSession\":{}}}"))
                   '(:arm :error :value (:cause (:arm :no-session :value nil)))))))

(ert-deftest agent-repl-test-wire-verbs-retry-vendor-error-ownership-refusal ()
  "A cross-cutting ownership refusal decodes with its fields."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-retry-vendor-error-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"error\":{\"transferringAway\":{\"address\":\"127.0.0.1:9\"}}}"))
                   '(:arm :error :value (:cause (:arm :transferring-away :value (:address "127.0.0.1:9"))))))))

(ert-deftest agent-repl-test-wire-verbs-retry-vendor-error-refuses-an-unknown-outcome ()
  "An outcome this codec does not know is refused as an unknown field."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-retry-vendor-error-success
                   (agent-repl-test-wire-verbs--parse "{\"postponed\":{}}"))
                  :type 'agent-repl-wire-error)))

;;;; ---- SwapWorkspaces ---------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-swap-request ()
  "A swap request carries both refs through the WorkspaceRef codec."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-encode-swap-workspaces-request
                    (list :workspace agent-repl-test-wire-verbs--ref
                          :target '(:id "ws-2" :dir "/w/two")))
                   '((workspace . ((id . "ws-1") (dir . "/w/one")))
                     (target . ((id . "ws-2") (dir . "/w/two"))))))))

(ert-deftest agent-repl-test-wire-verbs-swap-without-target ()
  "A swap with no target is incomplete and errors before send."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-swap-workspaces-request
                   (list :workspace agent-repl-test-wire-verbs--ref))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-swap-without-workspace ()
  "A swap with no workspace is incomplete and errors before send."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-swap-workspaces-request
                   '(:other (:id "ws-2" :dir "/w/two")))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-swap-response-success ()
  "A made swap decodes to the empty success arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-swap-workspaces-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{}}"))
                   '(:arm :success :value nil)))))

(ert-deftest agent-repl-test-wire-verbs-swap-error-unknown-workspace-arm ()
  "SwapWorkspacesError's `unknown_workspace' arm decodes with the ref it
names."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-swap-workspaces-error
                    (agent-repl-test-wire-verbs--parse "{\"unknownWorkspace\":{\"workspace\":{\"id\":\"ws-2\",\"dir\":\"/w/two\"}}}"))
                   '(:cause (:arm :unknown-workspace :value (:workspace (:id "ws-2" :dir "/w/two"))))))))

(ert-deftest agent-repl-test-wire-verbs-swap-error-workspace-ref-mismatch-arm ()
  "SwapWorkspacesError's `workspace_ref_mismatch' arm decodes with the ref
and the registry dir."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-swap-workspaces-error
                    (agent-repl-test-wire-verbs--parse "{\"workspaceRefMismatch\":{\"workspace\":{\"id\":\"ws-2\",\"dir\":\"/w/stale\"},\"registryDir\":\"/w/two\"}}"))
                   '(:cause (:arm :workspace-ref-mismatch
                             :value (:workspace (:id "ws-2" :dir "/w/stale") :registry-dir "/w/two")))))))

(ert-deftest agent-repl-test-wire-verbs-swap-error-transferring-away-arm ()
  "SwapWorkspacesError's `transferring_away' arm decodes with its address."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-swap-workspaces-error
                    (agent-repl-test-wire-verbs--parse "{\"transferringAway\":{\"address\":\"127.0.0.1:9999\"}}"))
                   '(:cause (:arm :transferring-away :value (:address "127.0.0.1:9999")))))))

(ert-deftest agent-repl-test-wire-verbs-swap-error-not-yet-adopted-arm ()
  "SwapWorkspacesError's `not_yet_adopted' arm decodes empty."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-swap-workspaces-error
                    (agent-repl-test-wire-verbs--parse "{\"notYetAdopted\":{}}"))
                   '(:cause (:arm :not-yet-adopted :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-swap-error-unset-cause-is-a-breach ()
  "SwapWorkspacesError with no arm set is a breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-swap-workspaces-error (agent-repl-test-wire-verbs--parse "{}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-swap-error-unknown-arm-is-a-breach ()
  "An arm SwapWorkspacesError does not declare is refused."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-swap-workspaces-error
                   (agent-repl-test-wire-verbs--parse "{\"noSuchArm\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-swap-error-arms-pinned ()
  "SwapWorkspacesError's arm set is exactly what the frozen schema
declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_swap_workspaces.pb.go" "SwapWorkspacesError")
                       #'string<)
                 (sort (list "unknownWorkspace" "workspaceRefMismatch" "transferringAway" "notYetAdopted" "otherRepository" "notSiblings")
                       #'string<))))

(ert-deftest agent-repl-test-wire-verbs-swap-error-other-repository-arm ()
  "SwapWorkspacesError's `other_repository' arm decodes empty."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-swap-workspaces-error
                    (agent-repl-test-wire-verbs--parse "{\"otherRepository\":{}}"))
                   '(:cause (:arm :other-repository :value nil))))))


;;;; ---- MoveWorkspace ---------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-move-request ()
  "A move request carries both refs through the WorkspaceRef codec."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-encode-move-workspace-request
                    (list :workspace agent-repl-test-wire-verbs--ref
                          :target '(:id "ws-2" :dir "/w/two")))
                   '((workspace . ((id . "ws-1") (dir . "/w/one")))
                     (target . ((id . "ws-2") (dir . "/w/two"))))))))

(ert-deftest agent-repl-test-wire-verbs-move-without-target ()
  "A move with no target is incomplete and errors before send."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-move-workspace-request
                   (list :workspace agent-repl-test-wire-verbs--ref))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-move-without-workspace ()
  "A move with no workspace is incomplete and errors before send."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-move-workspace-request
                   '(:other (:id "ws-2" :dir "/w/two")))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-move-response-success ()
  "A made move decodes to the empty success arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-move-workspace-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{}}"))
                   '(:arm :success :value nil)))))

(ert-deftest agent-repl-test-wire-verbs-move-error-unknown-workspace-arm ()
  "MoveWorkspaceError's `unknown_workspace' arm decodes with the ref it
names."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-move-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"unknownWorkspace\":{\"workspace\":{\"id\":\"ws-2\",\"dir\":\"/w/two\"}}}"))
                   '(:cause (:arm :unknown-workspace :value (:workspace (:id "ws-2" :dir "/w/two"))))))))

(ert-deftest agent-repl-test-wire-verbs-move-error-workspace-ref-mismatch-arm ()
  "MoveWorkspaceError's `workspace_ref_mismatch' arm decodes with the ref
and the registry dir."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-move-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"workspaceRefMismatch\":{\"workspace\":{\"id\":\"ws-2\",\"dir\":\"/w/stale\"},\"registryDir\":\"/w/two\"}}"))
                   '(:cause (:arm :workspace-ref-mismatch
                             :value (:workspace (:id "ws-2" :dir "/w/stale") :registry-dir "/w/two")))))))

(ert-deftest agent-repl-test-wire-verbs-move-error-transferring-away-arm ()
  "MoveWorkspaceError's `transferring_away' arm decodes with its address."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-move-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"transferringAway\":{\"address\":\"127.0.0.1:9999\"}}"))
                   '(:cause (:arm :transferring-away :value (:address "127.0.0.1:9999")))))))

(ert-deftest agent-repl-test-wire-verbs-move-error-not-yet-adopted-arm ()
  "MoveWorkspaceError's `not_yet_adopted' arm decodes empty."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-move-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"notYetAdopted\":{}}"))
                   '(:cause (:arm :not-yet-adopted :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-move-error-other-repository-arm ()
  "MoveWorkspaceError's `other_repository' arm decodes empty."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-move-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"otherRepository\":{}}"))
                   '(:cause (:arm :other-repository :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-move-error-not-siblings-arm ()
  "MoveWorkspaceError's `not_siblings' arm decodes empty."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-move-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"notSiblings\":{}}"))
                   '(:cause (:arm :not-siblings :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-move-error-unset-cause-is-a-breach ()
  "MoveWorkspaceError with no arm set is a breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-move-workspace-error (agent-repl-test-wire-verbs--parse "{}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-move-error-unknown-arm-is-a-breach ()
  "An arm MoveWorkspaceError does not declare is refused."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-move-workspace-error
                   (agent-repl-test-wire-verbs--parse "{\"noSuchArm\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-move-error-arms-pinned ()
  "MoveWorkspaceError's arm set is exactly what the frozen schema
declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_move_workspace.pb.go" "MoveWorkspaceError")
                       #'string<)
                 (sort (list "unknownWorkspace" "workspaceRefMismatch" "transferringAway" "notYetAdopted" "otherRepository" "notSiblings")
                       #'string<))))


;;;; ---- SubmitPromptRequest ---------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-submit-request-shape ()
  "A submission carries the workspace, said, the idempotency key and the
origin."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-encode-submit-prompt-request
                    (list :workspace agent-repl-test-wire-verbs--ref
                          :said '(:text "hi") :idempotency-key "k-1"
                          :origin :user-sent))
                   '((workspace . ((id . "ws-1") (dir . "/w/one")))
                     (said . ((said . "hi")))
                     (idempotencyKey . "k-1")
                     (origin . "ORIGIN::user-sent"))))))

(ert-deftest agent-repl-test-wire-verbs-submit-request-echoes-the-workspace ()
  "The workspace ref rides every Emacs submit, echoed verbatim.
Landing 2: the root feed has no id of its own, so the workspace — not
`feed' — is what names WHICH workspace the submission belongs to."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (cdr (assq 'workspace
                              (agent-repl-wire-encode-submit-prompt-request
                               (list :workspace agent-repl-test-wire-verbs--ref
                                     :said '(:text "hi") :idempotency-key "k-1"
                                     :origin :user-sent))))
                   '((id . "ws-1") (dir . "/w/one"))))))

(ert-deftest agent-repl-test-wire-verbs-submit-missing-workspace-refused ()
  "The workspace is REQUIRED, so a submission without one never reaches the
wire."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-submit-prompt-request
                   '(:said (:text "hi") :idempotency-key "k-1" :origin :user-sent))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-submit-request-omits-feed ()
  "Emacs never addresses a subagent feed, so `feed' is never spelled."
  (agent-repl-test-wire-verbs--with-common
    (should-not (assq 'feed (agent-repl-wire-encode-submit-prompt-request
                             (list :workspace agent-repl-test-wire-verbs--ref
                                   :said '(:text "hi") :idempotency-key "k-1"
                                   :origin :user-sent))))))

(ert-deftest agent-repl-test-wire-verbs-submit-empty-idempotency-key-refused ()
  "An empty idempotency key defeats duplicate refusal and is refused here."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-submit-prompt-request
                   (list :workspace agent-repl-test-wire-verbs--ref
                         :said '(:text "hi") :idempotency-key "" :origin :user-sent))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-submit-missing-idempotency-key-refused ()
  "An absent idempotency key is refused before the submission can be sent."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-submit-prompt-request
                   (list :workspace agent-repl-test-wire-verbs--ref
                         :said '(:text "hi") :origin :user-sent))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-submit-missing-origin-refused ()
  "The origin is REQUIRED, so a submission without one never reaches the wire."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-submit-prompt-request
                   (list :workspace agent-repl-test-wire-verbs--ref
                         :said '(:text "hi") :idempotency-key "k-1"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-submit-missing-said-refused ()
  "A submission with nothing said is incomplete and errors before send."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-submit-prompt-request
                   (list :workspace agent-repl-test-wire-verbs--ref
                         :idempotency-key "k-1" :origin :user-sent))
                  :type 'agent-repl-wire-error)))


(ert-deftest agent-repl-test-wire-verbs-submit-names-no-reply-target ()
  "A submit spells no reply target: the daemon applies its own selection."
  (agent-repl-test-wire-verbs--with-common
    (should-not (assq 'referenceResponseFeedid
                      (agent-repl-wire-encode-submit-prompt-request
                       (list :workspace agent-repl-test-wire-verbs--ref
                             :said '(:text "hi") :idempotency-key "k-1"
                             :origin :user-sent))))))


(ert-deftest agent-repl-test-wire-verbs-submit-omits-delivery-when-absent ()
  "An ordinary prompt spells no `delivery': absence is the ordinary delivery."
  (agent-repl-test-wire-verbs--with-common
    (should-not (assq 'delivery
                      (agent-repl-wire-encode-submit-prompt-request
                       (list :workspace agent-repl-test-wire-verbs--ref
                             :said '(:text "hi") :idempotency-key "k-1"
                             :origin :user-sent))))))

(ert-deftest agent-repl-test-wire-verbs-submit-carries-a-deferred-delivery ()
  "A deferred submit spells its delivery by the generated enum name."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (cdr (assq 'delivery
                              (agent-repl-wire-encode-submit-prompt-request
                               (list :workspace agent-repl-test-wire-verbs--ref
                                     :said '(:text "hi") :idempotency-key "k-1"
                                     :origin :deferred-prompt :delivery :deferred))))
                   "SUBMIT_PROMPT_DELIVERY_DEFERRED"))))

(ert-deftest agent-repl-test-wire-verbs-submit-omits-the-submit-instant-when-absent ()
  "A retry spells no submit instant: its wait is not the prompt's echo."
  (agent-repl-test-wire-verbs--with-common
    (should-not (assq 'submittedAtUnixMs
                      (agent-repl-wire-encode-submit-prompt-request
                       (list :workspace agent-repl-test-wire-verbs--ref
                             :said '(:text "hi") :idempotency-key "k-1"
                             :origin :user-sent))))))

(ert-deftest agent-repl-test-wire-verbs-submit-carries-the-submit-instant ()
  "A person's send spells its submit instant in Unix milliseconds."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (cdr (assq 'submittedAtUnixMs
                              (agent-repl-wire-encode-submit-prompt-request
                               (list :workspace agent-repl-test-wire-verbs--ref
                                     :said '(:text "hi") :idempotency-key "k-1"
                                     :origin :user-sent :submitted-at-unix-ms 1700000000123))))
                   1700000000123))))

(ert-deftest agent-repl-test-wire-verbs-submit-refuses-a-non-integer-submit-instant ()
  "A submit instant that is not an int64 is refused before the wire."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-submit-prompt-request
                   (list :workspace agent-repl-test-wire-verbs--ref
                         :said '(:text "hi") :idempotency-key "k-1"
                         :origin :user-sent :submitted-at-unix-ms 1.5))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-submit-delivery-refuses-unspecified ()
  "UNSPECIFIED has no elisp spelling, so an unknown keyword is refused."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-submit-prompt-delivery :unspecified)
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-submit-delivery-vocabulary-pinned ()
  "The delivery vocabulary is every generated enum name except UNSPECIFIED."
  (should (equal
           (sort (mapcar #'cdr agent-repl-wire-submit-prompt-deliveries) #'string<)
           (sort (remove "SUBMIT_PROMPT_DELIVERY_UNSPECIFIED"
                         (agent-repl-test--generated-enum-names
                          "agentrepl/v1/endpoint_submit_prompt.pb.go"
                          "SUBMIT_PROMPT_DELIVERY_"))
                 #'string<))))


;;;; ---- SubmitPromptResponse --------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-submit-response-turn ()
  "A minted turn decodes through the TurnId codec."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"success\":{\"turn\":{\"turn\":{\"value\":\"t-1\"}}}}"))
                   '(:arm :success :value (:arm :turn :value (:turn (:value "t-1"))))))))

(ert-deftest agent-repl-test-wire-verbs-submit-response-turn-without-turn-id ()
  "A turn arm without the minted TurnId is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-submit-prompt-response
                   (agent-repl-test-wire-verbs--parse "{\"success\":{\"turn\":{}}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-submit-response-command-panel-raw ()
  "A command panel decodes to its arm keyword, keeping the payload raw."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"success\":{\"commandPanel\":{\"status\":{\"model\":\"opus\"}}}}"))
                   '(:arm :success
                     :value (:arm :command-panel
                             :value (:arm :status :value ((model . "opus")))))))))

(ert-deftest agent-repl-test-wire-verbs-submit-response-command-panel-todos ()
  "Every declared panel arm decodes, not only the first."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"success\":{\"commandPanel\":{\"todos\":{}}}}"))
                   '(:arm :success
                     :value (:arm :command-panel :value (:arm :todos :value nil)))))))

(ert-deftest agent-repl-test-wire-verbs-submit-response-command-panel-retired-arm ()
  "The retired /cost and /usage tags are not arms and arrive as unknown keys."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-submit-prompt-response
                   (agent-repl-test-wire-verbs--parse
                    "{\"success\":{\"commandPanel\":{\"cost\":{}}}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-submit-response-command-refused ()
  "A recognized-but-unsupported command decodes with the command as typed."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"success\":{\"commandRefused\":{\"command\":\"/agents\"}}}"))
                   '(:arm :success
                     :value (:arm :command-refused :value (:command "/agents")))))))

(ert-deftest agent-repl-test-wire-verbs-submit-response-command-refused-default ()
  "protojson omits a default-valued scalar, so an absent command is the empty string."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"success\":{\"commandRefused\":{}}}"))
                   '(:arm :success
                     :value (:arm :command-refused :value (:command "")))))))

(ert-deftest agent-repl-test-wire-verbs-submit-response-outcome-unset ()
  "A success with no outcome arm set is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-submit-prompt-response
                   (agent-repl-test-wire-verbs--parse "{\"success\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-submit-response-outcome-two-arms ()
  "A success with two outcome arms set is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-submit-prompt-response
                   (agent-repl-test-wire-verbs--parse
                    "{\"success\":{\"turn\":{\"turn\":{\"value\":\"t\"}},\"commandRefused\":{}}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-submit-response-merging ()
  "A merge in flight refuses the submission through the reason oneof."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-response
                    (agent-repl-test-wire-verbs--parse "{\"error\":{\"merging\":{}}}"))
                   '(:arm :error :value (:reason (:arm :merging :value nil)))))))

(ert-deftest agent-repl-test-wire-verbs-submit-response-reason-unset ()
  "A submit error with no reason arm set is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-submit-prompt-response
                   (agent-repl-test-wire-verbs--parse "{\"error\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-submit-response-unknown-reason ()
  "A future submit-refusal arm arrives as an unknown key and is loud."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-submit-prompt-response
                   (agent-repl-test-wire-verbs--parse "{\"error\":{\"draining\":{}}}"))
                  :type 'agent-repl-wire-error)))


;;;; ---- Deploy -----------------------------------------------------------

(defun agent-repl-test-wire-verbs--deploy (json)
  "Decode the DeployResponse JSON text with the codec, quietly."
  (agent-repl-wire-decode-deploy-response (agent-repl-test-wire-verbs--parse json)))

(defun agent-repl-test-wire-verbs--deploy-breach (json)
  "Return the `agent-repl-wire-error' data decoding DeployResponse JSON raises."
  (condition-case err
      (progn (agent-repl-test-wire-verbs--deploy json) nil)
    (agent-repl-wire-error (cdr err))))

(defun agent-repl-test-wire-verbs--outcome-json (arm-json)
  "Return a DeployResponse success carrying one daemon outcome with ARM-JSON."
  (concat "{\"success\":{\"components\":[{\"component\":\"DEPLOY_COMPONENT_DAEMON\","
          "\"build\":\"h1\"," arm-json "}]}}"))

(ert-deftest agent-repl-test-wire-verbs-deploy-request-unforced ()
  "An unforced deploy spells `force' explicitly false."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize (agent-repl-wire-encode-deploy-request '(:force nil)))
                   "{\"force\":false}"))))

(ert-deftest agent-repl-test-wire-verbs-deploy-request-forced ()
  "A forced deploy carries `force' true."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize (agent-repl-wire-encode-deploy-request '(:force t)))
                   "{\"force\":true}"))))

(ert-deftest agent-repl-test-wire-verbs-deploy-decodes-every-outcome-arm ()
  "Every DeployComponentOutcome arm decodes to its keyword and its fields."
  (agent-repl-test-wire-verbs--with-common
    (dolist (case
             '(("\"upToDate\":{}" (:arm :up-to-date :value nil))
               ("\"restarted\":{}" (:arm :restarted :value nil))
               ("\"handingOver\":{\"workspaces\":3,\"busy\":1,\"forced\":true}"
                (:arm :handing-over :value (:workspaces 3 :busy 1 :forced t)))
               ("\"shims\":{\"bounces\":[{\"workspace\":\"ws-a\",\"bouncedNow\":{\"forced\":true}},{\"workspace\":\"ws-b\",\"registered\":{\"turnInFlight\":true,\"detachedWork\":2}}]}"
                (:arm :shims
                 :value (:bounces ((:workspace "ws-a" :when (:arm :bounced-now :value (:forced t)))
                                   (:workspace "ws-b"
                                    :when (:arm :registered
                                           :value (:turn-in-flight t :detached-work 2)))))))
               ("\"reloadPushed\":{\"recipients\":4}"
                (:arm :reload-pushed :value (:recipients 4)))
               ("\"deferredToSuccessor\":{}" (:arm :deferred-to-successor :value nil))
               ("\"restarting\":{\"runningStateLayout\":7,\"freshStateLayout\":8,\"workspaces\":3,\"busy\":1,\"forced\":true}"
                (:arm :restarting
                 :value (:running-state-layout 7 :fresh-state-layout 8
                         :workspaces 3 :busy 1 :forced t)))))
      (should (equal (list (car case)
                           (plist-get
                            (car (plist-get
                                  (plist-get (agent-repl-test-wire-verbs--deploy
                                              (agent-repl-test-wire-verbs--outcome-json (car case)))
                                             :value)
                                  :components))
                            :outcome))
                     (list (car case) (cadr case)))))))

(ert-deftest agent-repl-test-wire-verbs-deploy-decodes-component-and-build ()
  "An outcome names its component as a keyword and the fresh build's hash."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-test-wire-verbs--deploy
                    (concat "{\"success\":{\"components\":["
                            "{\"component\":\"DEPLOY_COMPONENT_ELISP\",\"build\":\"e1\",\"upToDate\":{}},"
                            "{\"component\":\"DEPLOY_COMPONENT_STORE\",\"build\":\"s1\",\"restarted\":{}}]}}"))
                   '(:arm :success
                     :value (:components
                             ((:component :elisp :build "e1" :outcome (:arm :up-to-date :value nil))
                              (:component :store :build "s1"
                               :outcome (:arm :restarted :value nil)))))))))

(ert-deftest agent-repl-test-wire-verbs-deploy-decodes-every-component ()
  "Every DeployComponent name decodes to its keyword."
  (agent-repl-test-wire-verbs--with-common
    (dolist (case '(("DEPLOY_COMPONENT_DAEMON" :daemon) ("DEPLOY_COMPONENT_SHIM" :shim)
                    ("DEPLOY_COMPONENT_WEBAPP" :webapp) ("DEPLOY_COMPONENT_STORE" :store)
                    ("DEPLOY_COMPONENT_SIDECAR" :sidecar) ("DEPLOY_COMPONENT_ELISP" :elisp)
                    ("DEPLOY_COMPONENT_ZETTEL" :zettel)))
      (should (equal (plist-get
                      (car (plist-get
                            (plist-get (agent-repl-test-wire-verbs--deploy
                                        (format "{\"success\":{\"components\":[{\"component\":%S,\"build\":\"h\",\"upToDate\":{}}]}}"
                                                (car case)))
                                       :value)
                            :components))
                      :component)
                     (cadr case))))))

(ert-deftest agent-repl-test-wire-verbs-deploy-component-vocabulary-pinned ()
  "The component vocabulary is exactly the schema's, UNSPECIFIED aside."
  (should (equal (sort (delete "DEPLOY_COMPONENT_UNSPECIFIED"
                               (agent-repl-test--generated-enum-names
                                "agentrepl/v1/endpoint_deploy.pb.go" "DEPLOY_COMPONENT_"))
                       #'string<)
                 (sort (mapcar #'car agent-repl-wire-deploy-components) #'string<))))

(ert-deftest agent-repl-test-wire-verbs-deploy-unset-component-is-a-breach ()
  "An outcome naming no component is malformed."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-test-wire-verbs--deploy-breach
                    "{\"success\":{\"components\":[{\"build\":\"h\",\"upToDate\":{}}]}}")
                   '("DeployComponentOutcome" "component" "required field is unset")))))

(ert-deftest agent-repl-test-wire-verbs-deploy-unspecified-component-is-a-breach ()
  "UNSPECIFIED is never sent, so an outcome carrying it is malformed."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-test-wire-verbs--deploy-breach
                    "{\"success\":{\"components\":[{\"component\":\"DEPLOY_COMPONENT_UNSPECIFIED\",\"build\":\"h\",\"upToDate\":{}}]}}")
                   '("DeployComponentOutcome" "component" "required field is unset")))))

(ert-deftest agent-repl-test-wire-verbs-deploy-empty-build-is-a-breach ()
  "Every outcome names the fresh build it was compared against."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-test-wire-verbs--deploy-breach
                    "{\"success\":{\"components\":[{\"component\":\"DEPLOY_COMPONENT_SHIM\",\"upToDate\":{}}]}}")
                   '("DeployComponentOutcome" "build" "required string is empty")))))

(ert-deftest agent-repl-test-wire-verbs-deploy-unset-decision-is-a-breach ()
  "THE ARM IS THE DECISION, so an outcome deciding nothing is malformed."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-test-wire-verbs--deploy-breach
                    "{\"success\":{\"components\":[{\"component\":\"DEPLOY_COMPONENT_SHIM\",\"build\":\"h\"}]}}")
                   '("DeployComponentOutcome" "outcome" "oneof is unset")))))

(ert-deftest agent-repl-test-wire-verbs-deploy-shim-bounce-without-a-workspace-is-a-breach ()
  "A shim bounce names the workspace it serves."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-test-wire-verbs--deploy-breach
                    (agent-repl-test-wire-verbs--outcome-json
                     "\"shims\":{\"bounces\":[{\"bouncedNow\":{}}]}"))
                   '("DeployShimBounce" "workspace" "required string is empty")))))

(ert-deftest agent-repl-test-wire-verbs-deploy-shim-bounce-without-a-when-is-a-breach ()
  "THE ARM IS WHEN, so a bounce with neither is malformed."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-test-wire-verbs--deploy-breach
                    (agent-repl-test-wire-verbs--outcome-json
                     "\"shims\":{\"bounces\":[{\"workspace\":\"ws-a\"}]}"))
                   '("DeployShimBounce" "when" "oneof is unset")))))

(ert-deftest agent-repl-test-wire-verbs-deploy-decodes-every-error-arm ()
  "Every DeployError arm decodes to its keyword and its fields."
  (agent-repl-test-wire-verbs--with-common
    (dolist (case
             '(("{\"buildFailed\":{\"step\":\"webapp\",\"detail\":\"tsc: 2 errors\",\"log\":\"/tmp/b.log\"}}"
                (:arm :build-failed :value (:step "webapp" :detail "tsc: 2 errors" :log "/tmp/b.log")))
               ("{\"alreadyDeploying\":{}}" (:arm :already-deploying :value nil))
               ("{\"alreadyRollingOut\":{\"waitingOn\":[\"ws-a\",\"ws-b\"]}}"
                (:arm :already-rolling-out :value (:waiting-on ("ws-a" "ws-b"))))
               ("{\"joining\":{}}" (:arm :joining :value nil))
               ("{\"serviceRestartFailed\":{\"component\":\"DEPLOY_COMPONENT_STORE\",\"detail\":\"exit 78\"}}"
                (:arm :service-restart-failed :value (:component :store :detail "exit 78")))
               ("{\"installFailed\":{\"component\":\"DEPLOY_COMPONENT_DAEMON\",\"detail\":\"EACCES\"}}"
                (:arm :install-failed :value (:component :daemon :detail "EACCES")))))
      (should (equal (list (car case)
                           (agent-repl-test-wire-verbs--deploy
                            (format "{\"error\":%s}" (car case))))
                     (list (car case)
                           (list :arm :error :value (list :cause (cadr case)))))))))

(ert-deftest agent-repl-test-wire-verbs-deploy-build-failed-log-may-be-empty ()
  "The archived log path is optional detail; an absent one decodes as empty."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-test-wire-verbs--deploy
                    "{\"error\":{\"buildFailed\":{\"step\":\"lock\",\"detail\":\"held\"}}}")
                   '(:arm :error
                     :value (:cause (:arm :build-failed
                                     :value (:step "lock" :detail "held" :log ""))))))))

(ert-deftest agent-repl-test-wire-verbs-deploy-build-failed-without-a-step-is-a-breach ()
  "A failed build names the step that failed."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-test-wire-verbs--deploy-breach
                    "{\"error\":{\"buildFailed\":{\"detail\":\"x\"}}}")
                   '("DeployBuildFailed" "step" "required string is empty")))))

(ert-deftest agent-repl-test-wire-verbs-deploy-build-failed-without-detail-is-a-breach ()
  "A failed build carries the step's own words."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-test-wire-verbs--deploy-breach
                    "{\"error\":{\"buildFailed\":{\"step\":\"shim\"}}}")
                   '("DeployBuildFailed" "detail" "required string is empty")))))

(ert-deftest agent-repl-test-wire-verbs-deploy-restart-failed-without-a-component-is-a-breach ()
  "A failed restart names the service that did not come back."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-test-wire-verbs--deploy-breach
                    "{\"error\":{\"serviceRestartFailed\":{\"detail\":\"x\"}}}")
                   '("DeployServiceRestartFailed" "component" "required field is unset")))))

(ert-deftest agent-repl-test-wire-verbs-deploy-install-failed-without-detail-is-a-breach ()
  "A failed install says why."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-test-wire-verbs--deploy-breach
                    "{\"error\":{\"installFailed\":{\"component\":\"DEPLOY_COMPONENT_SHIM\"}}}")
                   '("DeployInstallFailed" "detail" "required string is empty")))))

(ert-deftest agent-repl-test-wire-verbs-deploy-unset-cause-is-a-breach ()
  "THE ARM IS THE REFUSAL, so an error naming none is malformed."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-test-wire-verbs--deploy-breach "{\"error\":{}}")
                   '("DeployError" "cause" "oneof is unset")))))

(ert-deftest agent-repl-test-wire-verbs-deploy-unknown-cause-is-refused ()
  "An error arm this codec does not know is refused, never guessed at."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-test-wire-verbs--deploy-breach "{\"error\":{\"diskFull\":{}}}")
                   '("DeployError" "diskFull" "unknown field")))))

(ert-deftest agent-repl-test-wire-verbs-deploy-arms-pinned ()
  "Every Deploy oneof carries exactly the arms this codec decodes."
  (dolist (case '(("DeployComponentOutcome"
                   ("upToDate" "restarted" "handingOver" "shims" "reloadPushed"
                    "deferredToSuccessor" "restarting"))
                  ("DeployShimBounce" ("bouncedNow" "registered"))
                  ("DeployError"
                   ("buildFailed" "alreadyDeploying" "alreadyRollingOut" "joining"
                    "serviceRestartFailed" "installFailed"))
                  ("DeployResponse" ("success" "error"))))
    (should (equal (list (car case)
                         (sort (agent-repl-test--generated-oneof-arms
                                "agentrepl/v1/endpoint_deploy.pb.go" (car case))
                               #'string<))
                   (list (car case) (sort (copy-sequence (cadr case)) #'string<))))))

;;;; ---- UpdateShutdownSchedule ------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-schedule-at-ms-integer ()
  "An int64 instant supplied as an integer rides the wire as a number."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-update-shutdown-schedule-request
                     '(:action (:arm :schedule
                                :value (:at-ms 1756400000000
                                        :reason (:arm :deploy :value nil))))))
                   "{\"schedule\":{\"atMs\":1756400000000,\"reason\":{\"reason\":\":deploy\"}}}"))))

(ert-deftest agent-repl-test-wire-verbs-schedule-at-ms-decimal-string ()
  "protojson's own int64 spelling — a decimal string — is accepted and normalized."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (cdr (assq 'atMs
                              (cdr (assq 'schedule
                                         (agent-repl-wire-encode-update-shutdown-schedule-request
                                          '(:action (:arm :schedule
                                                     :value (:at-ms "1756400000000"
                                                             :reason (:arm :deploy :value nil)))))))))
                   1756400000000))))

(ert-deftest agent-repl-test-wire-verbs-schedule-at-ms-not-int64 ()
  "A non-int64 instant is a malformed request and errors before send."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-update-shutdown-schedule-request
                   '(:action (:arm :schedule
                              :value (:at-ms "soon" :reason (:arm :deploy :value nil)))))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-schedule-without-at-ms ()
  "A schedule with no instant is incomplete and errors before send."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-update-shutdown-schedule-request
                   '(:action (:arm :schedule :value (:reason (:arm :deploy :value nil)))))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-schedule-without-reason ()
  "The drain reason is REQUIRED on schedule: every client's banner names it."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-update-shutdown-schedule-request
                   '(:action (:arm :schedule :value (:at-ms 1))))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-schedule-cancel ()
  "Cancel is the whole action, so its arm is the empty message."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-update-shutdown-schedule-request
                     '(:action (:arm :cancel :value nil))))
                   "{\"cancel\":{}}"))))

(ert-deftest agent-repl-test-wire-verbs-schedule-now ()
  "An immediate exit carries its reason, which rides the announcement."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-update-shutdown-schedule-request
                     '(:action (:arm :now :value (:reason (:arm :operator :value nil))))))
                   "{\"now\":{\"reason\":{\"reason\":\":operator\"}}}"))))

(ert-deftest agent-repl-test-wire-verbs-schedule-now-without-reason ()
  "The drain reason is REQUIRED on now as well."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-update-shutdown-schedule-request
                   '(:action (:arm :now :value nil)))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-schedule-action-unset ()
  "The arm IS the action, so a request without one is refused."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-update-shutdown-schedule-request nil)
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-schedule-response-success ()
  "An armed schedule decodes to the empty success arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-update-shutdown-schedule-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{}}"))
                   '(:arm :success :value nil)))))


;;;; ---- UpdateMergeQueue ------------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-merge-queue-pause-names-its-repository ()
  "A pause that means ONE repository names it: the queue is per repository."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-update-merge-queue-request
                     (list :requester '(:arm :user) :action
                           (list :arm :pause
                                 :value (list :repository
                                              agent-repl-test-wire-verbs--repo)))))
                   (concat "{\"requester\":{\"user\":{}},\"pause\":{\"repository\":"
                           "{\"id\":\"repo-1\",\"dir\":\"/r/one\"}}}")))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-resume-names-its-repository ()
  "A resume scoped to one repository carries the same optional ref."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-update-merge-queue-request
                     (list :requester '(:arm :user) :action
                           (list :arm :resume
                                 :value (list :repository
                                              agent-repl-test-wire-verbs--repo)))))
                   (concat "{\"requester\":{\"user\":{}},\"resume\":{\"repository\":"
                           "{\"id\":\"repo-1\",\"dir\":\"/r/one\"}}}")))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-pause ()
  "Pause with no repository is the DAEMON-WIDE switch: the field is omitted,
never sent as an empty ref, because unset is what means every queue."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize (agent-repl-wire-encode-update-merge-queue-request
                                    '(:requester (:arm :user) :action (:arm :pause :value nil))))
                   "{\"requester\":{\"user\":{}},\"pause\":{}}"))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-resume ()
  "Resume with no repository is the daemon-wide switch, field omitted."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize (agent-repl-wire-encode-update-merge-queue-request
                                    '(:requester (:arm :user) :action (:arm :resume :value nil))))
                   "{\"requester\":{\"user\":{}},\"resume\":{}}"))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-evict ()
  "An eviction names whose merge to take off the queue."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-update-merge-queue-request
                     (list :requester '(:arm :user) :action (list :arm :evict
                                         :value (list :workspace
                                                      agent-repl-test-wire-verbs--ref)))))
                   "{\"requester\":{\"user\":{}},\"evict\":{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"}}}"))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-evict-without-workspace ()
  "An eviction with no workspace is incomplete and errors before send."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-update-merge-queue-request
                   '(:requester (:arm :user) :action (:arm :evict :value nil)))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-move-up ()
  "A move-up request names whose merge to move one place up its queue."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-update-merge-queue-request
                     (list :requester '(:arm :user) :action (list :arm :move-up
                                         :value (list :workspace
                                                      agent-repl-test-wire-verbs--ref)))))
                   "{\"requester\":{\"user\":{}},\"moveUp\":{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"}}}"))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-move-up-without-workspace ()
  "A move-up request with no workspace is incomplete and errors before send."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-update-merge-queue-request
                   '(:requester (:arm :user) :action (:arm :move-up :value nil)))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-move-down ()
  "A move-down request names whose merge to move one place down its queue."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-update-merge-queue-request
                     (list :requester '(:arm :user) :action (list :arm :move-down
                                         :value (list :workspace
                                                      agent-repl-test-wire-verbs--ref)))))
                   "{\"requester\":{\"user\":{}},\"moveDown\":{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"}}}"))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-move-down-without-workspace ()
  "A move-down request with no workspace is incomplete and errors before send."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-update-merge-queue-request
                   '(:requester (:arm :user) :action (:arm :move-down :value nil)))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-disable-racing ()
  "A disable of racing is daemon-wide, so its message is empty."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize (agent-repl-wire-encode-update-merge-queue-request
                                    '(:requester (:arm :user) :action (:arm :disable-racing :value nil))))
                   "{\"requester\":{\"user\":{}},\"disableRacing\":{}}"))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-enable-racing ()
  "An enable of racing is daemon-wide, so its message is empty."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize (agent-repl-wire-encode-update-merge-queue-request
                                    '(:requester (:arm :user) :action (:arm :enable-racing :value nil))))
                   "{\"requester\":{\"user\":{}},\"enableRacing\":{}}"))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-move-to-encodes-each-side ()
  "A move to a place names the merge to move and the merge it goes beside."
  (agent-repl-test-wire-verbs--with-common
    (dolist (case '((:ahead-of . "aheadOf") (:behind . "behind")))
      (should (equal (json-serialize
                      (agent-repl-wire-encode-update-merge-queue-request
                       (list :requester '(:arm :user)
                             :action (list :arm :move-to
                                           :value (list :workspace agent-repl-test-wire-verbs--ref
                                                        :place (list :arm (car case) :value agent-repl-test-wire-verbs--ref))))))
                     (format "{\"requester\":{\"user\":{}},\"moveTo\":{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},\"%s\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"}}}"
                             (cdr case)))))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-move-to-without-place ()
  "A move to a place with no place is incomplete and errors before send."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-update-merge-queue-request
                   (list :requester '(:arm :user)
                         :action (list :arm :move-to :value (list :workspace agent-repl-test-wire-verbs--ref))))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-move-to-without-workspace ()
  "A move to a place with no merge to move is incomplete and errors before send."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-update-merge-queue-request
                   (list :requester '(:arm :user)
                         :action (list :arm :move-to
                                       :value (list :place (list :arm :behind :value agent-repl-test-wire-verbs--ref)))))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-workspace-arm-encodes-the-ref ()
  "The shared workspace-arm encoder hands the required ref to its use site."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-verbs--encode-merge-queue-workspace-arm
                    "UpdateMergeQueueMoveUp" '(:workspace ref-1)
                    (lambda (ref) (list :encoded ref)))
                   '((workspace :encoded ref-1))))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-workspace-arm-requires-the-ref ()
  "The shared workspace-arm encoder refuses an arm with no workspace."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-verbs--encode-merge-queue-workspace-arm
                   "UpdateMergeQueueMoveUp" nil #'identity)
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-workspace-arms-share-one-encoder ()
  "Every UpdateMergeQueue arm naming one workspace encodes through the shared
helper, so a hand-rolled arm cannot drift from the others."
  (dolist (fn '(agent-repl-wire-encode-update-merge-queue-evict
                agent-repl-wire-encode-update-merge-queue-move-up
                agent-repl-wire-encode-update-merge-queue-move-down
                agent-repl-wire-encode-update-merge-queue-move-to))
    (should (string-match-p
             "agent-repl-wire-verbs--encode-merge-queue-workspace-arm"
             (prin1-to-string (symbol-function fn))))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-action-unset ()
  "The arm IS the action, so a request without one is refused."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-update-merge-queue-request nil)
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-response-error ()
  "A refused queue change decodes to the error arm carrying its cause."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-update-merge-queue-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"error\":{\"alreadyPaused\":{}}}"))
                   '(:arm :error
                     :value (:cause (:arm :already-paused :value nil)))))))


(ert-deftest agent-repl-test-wire-verbs-merge-queue-without-requester ()
  "A queue action that names no requester is incomplete and errors before send."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-update-merge-queue-request
                   '(:action (:arm :pause :value nil)))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-names-an-asking-workspace ()
  "A queue action a workspace asks for names that workspace."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-update-merge-queue-request
                     (list :requester (list :arm :workspace :value agent-repl-test-wire-verbs--ref)
                           :action '(:arm :pause :value nil))))
                   (concat "{\"requester\":{\"workspace\":{\"workspace\":"
                           "{\"id\":\"ws-1\",\"dir\":\"/w/one\"}}},\"pause\":{}}")))))

;;;; ---- DaemonHealth ----------------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-daemon-health-request-empty ()
  "There is nothing to ask beyond \"you?\", so the request is the empty
message."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize (agent-repl-wire-encode-daemon-health-request)) "{}"))))

(ert-deftest agent-repl-test-wire-verbs-daemon-health-healthy ()
  "A healthy verdict decodes to the healthy arm with no payload."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-daemon-health-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{\"healthy\":{}}}"))
                   '(:arm :success :value (:arm :healthy :value nil :identity nil))))))

(ert-deftest agent-repl-test-wire-verbs-daemon-health-process-identity ()
  "A health answer identifies the exact serving process and deployed build."
  (agent-repl-test-wire-verbs--with-common
    (should
     (equal
      (agent-repl-wire-decode-daemon-health-response
       (agent-repl-test-wire-verbs--parse
        "{\"success\":{\"healthy\":{},\"identity\":{\"instanceId\":\"daemon-2\",\"pid\":\"4242\",\"buildSha\":\"abc123\"}}}"))
      '(:arm :success
        :value (:arm :healthy :value nil
                :identity (:instance-id "daemon-2" :pid 4242
                           :build-sha "abc123")))))))

(ert-deftest agent-repl-test-wire-verbs-daemon-health-unhealthy-faults ()
  "UNHEALTHY IS AN ANSWER: the faults arrive inside success, each with its
detail."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-daemon-health-response
                    (agent-repl-test-wire-verbs--parse
                     (concat "{\"success\":{\"unhealthy\":{\"faults\":"
                             "[{\"detail\":\"store down\",\"wsmReadOnly\":{}},"
                             "{\"detail\":\"queue stuck\",\"logSinkPoisoned\":{\"sink\":\"emacs\"}}]}}}")))
                   '(:arm :success
                     :value (:arm :unhealthy
                             :value (:faults
                                     ((:detail "store down"
                                       :kind (:arm :wsm-read-only :value nil))
                                      (:detail "queue stuck"
                                       :kind (:arm :log-sink-poisoned
                                              :value (:sink "emacs")))))
                             :identity nil))))))

(ert-deftest agent-repl-test-wire-verbs-daemon-health-unhealthy-no-faults ()
  "An omitted repeated field is the empty list, protojson's `no elements'."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-daemon-health-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{\"unhealthy\":{}}}"))
                   '(:arm :success :value (:arm :unhealthy :value (:faults nil)
                                                :identity nil))))))

(ert-deftest agent-repl-test-wire-verbs-daemon-fault-detail-default ()
  "An omitted fault detail is the proto3 default, never a missing-field breach."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-daemon-fault
                    (agent-repl-test-wire-verbs--parse "{\"wsmReadOnly\":{}}"))
                   '(:detail "" :kind (:arm :wsm-read-only :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-daemon-fault-without-kind ()
  "A DaemonFault with no kind arm is a breach: detail never replaces the class."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-daemon-fault
                   (agent-repl-test-wire-verbs--parse "{\"detail\":\"prose\"}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-daemon-fault-unknown-field ()
  "A DaemonFault kind arm added upstream arrives as an unknown key and is loud."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-daemon-fault
                   (agent-repl-test-wire-verbs--parse "{\"storeUnreachable\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-daemon-health-verdict-unset ()
  "A health success with no verdict arm set is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-daemon-health-response
                   (agent-repl-test-wire-verbs--parse "{\"success\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-daemon-health-error ()
  "An unanswerable health question decodes to the empty error arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-daemon-health-response
                    (agent-repl-test-wire-verbs--parse "{\"error\":{}}"))
                   '(:arm :error :value nil)))))


;;;; ---- SessionHealth ---------------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-session-health-request ()
  "SessionHealthRequest echoes the WorkspaceRef verbatim."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-encode-session-health-request
                    (list :workspace agent-repl-test-wire-verbs--ref))
                   '((workspace . ((id . "ws-1") (dir . "/w/one"))))))))

(ert-deftest agent-repl-test-wire-verbs-session-health-request-without-workspace ()
  "A session-health pull with no workspace is incomplete and errors before
send."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-session-health-request nil)
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-session-health-healthy ()
  "A healthy session decodes to the healthy arm with no payload."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-session-health-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{\"healthy\":{}}}"))
                   '(:arm :success :value (:arm :healthy :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-session-health-unhealthy-faults ()
  "SessionFault carries the session controller's own dynamic detail."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-session-health-response
                    (agent-repl-test-wire-verbs--parse
                     (concat "{\"success\":{\"unhealthy\":{\"faults\":"
                             "[{\"detail\":\"shim gone\",\"shimDied\":{\"exitCode\":9}}]}}}")))
                   '(:arm :success
                     :value (:arm :unhealthy
                             :value (:faults ((:detail "shim gone"
                                               :kind (:arm :shim-died
                                                      :value (:exit-code 9)))))))))))

(ert-deftest agent-repl-test-wire-verbs-session-fault-without-kind ()
  "A SessionFault with no kind arm is a breach, as HostFault's is."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-session-fault
                   (agent-repl-test-wire-verbs--parse "{\"detail\":\"prose\"}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-session-fault-unknown-field ()
  "A SessionFault kind arm added upstream arrives as an unknown key and is
loud."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-session-fault
                   (agent-repl-test-wire-verbs--parse "{\"shimDead\":{}}"))
                  :type 'agent-repl-wire-error)))


;;;; ---- Logging -----------------------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-breach-logs-error ()
  "Every contract breach logs at ERROR before the typed signal leaves.
Recording the failure and aborting the caller are separate acts: the
logging rung returns normally, and the typed signal follows it."
  (agent-repl-test-wire-verbs--with-common
    (let (calls)
      (cl-letf (((symbol-function 'agent-repl--error)
                 (lambda (&rest args) (push args calls) nil)))
        (should-error (agent-repl-wire-encode-submit-prompt-request
                       (list :workspace agent-repl-test-wire-verbs--ref
                             :said '(:text "hi") :idempotency-key ""
                             :origin :user-sent))
                      :type 'agent-repl-wire-error))
      (should (= (length calls) 1)))))


;;;; ---- Arm lists pinned against the generated Go bindings --------------

(ert-deftest agent-repl-test-wire-verbs-result-arms-pinned ()
  "Every response's result oneof has exactly the arms this codec decodes."
  (dolist (entry agent-repl-test-wire-verbs--result-oneofs)
    (let ((want (or (nth 2 entry) '("error" "success"))))
      (should (equal (sort (agent-repl-test--generated-oneof-arms (nth 0 entry) (nth 1 entry))
                           #'string<)
                     (sort (copy-sequence want) #'string<))))))

(ert-deftest agent-repl-test-wire-verbs-create-form-arms-pinned ()
  "CreateWorkspaceRequest's form oneof is the two arms encoded here and the
async merge, which is an agent\='s request and never the editor\='s."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_create_workspace.pb.go"
                        "CreateWorkspaceRequest")
                       #'string<)
                 '("asyncMerge" "oneShot" "standard"))))

(ert-deftest agent-repl-test-wire-verbs-one-shot-has-no-oneof-at-all ()
  "CreateWorkspaceOneShot carries no oneof: the finish choice is retired, so
the form is its prompt and nothing else."
  (should (equal (agent-repl-test--generated-oneof-arms
                  "agentrepl/v1/endpoint_create_workspace.pb.go"
                  "CreateWorkspaceOneShot")
                 nil)))

(ert-deftest agent-repl-test-wire-verbs-close-cause-arms-pinned ()
  "CloseWorkspaceError's cause oneof has exactly the arms this codec decodes."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_close_workspace.pb.go"
                        "CloseWorkspaceError")
                       #'string<)
                 '("blocked" "hasOpenChildren" "notYetAdopted" "transferringAway"
                   "unknownWorkspace" "workspaceRefMismatch"))))

(ert-deftest agent-repl-test-wire-verbs-submit-outcome-arms-pinned ()
  "SubmitPromptSuccess's outcome oneof has exactly the six arms decoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_submit_prompt.pb.go"
                        "SubmitPromptSuccess")
                       #'string<)
                 '("askAnswered" "commandActed" "commandPanel" "commandRefused" "responseRevising" "turn"))))

(ert-deftest agent-repl-test-wire-verbs-submit-panel-arms-pinned ()
  "SubmitPromptCommandPanel's panel oneof has exactly the six arms decoded
here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_submit_prompt.pb.go"
                        "SubmitPromptCommandPanel")
                       #'string<)
                 '("agents" "context" "help" "mcp" "status" "todos"))))

(ert-deftest agent-repl-test-wire-verbs-submit-reason-arms-pinned ()
  "SubmitPromptError's reason oneof is the arms this codec decodes and
`slash_command_from_agent' and `unknown_sender', which refuse only an
agent\='s prompt and so never answer the editor\='s."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_submit_prompt.pb.go"
                        "SubmitPromptError")
                       #'string<)
                 '("askNotStanding" "bubbleRefused" "coldGate" "duplicateSubmission"
                   "feedNotInWorkspace" "feedUndecodable" "merging"
                   "modelNotInCatalog" "modelRefused" "noSession"
                   "notYetAdopted" "slashCommandFromAgent" "threadAwaitingFirstAnswer" "transferringAway"
                   "unknownSender" "unknownWorkspace" "workspaceRefMismatch"))))

(ert-deftest agent-repl-test-wire-verbs-shutdown-action-arms-pinned ()
  "UpdateShutdownScheduleRequest's action oneof has exactly the three arms
encoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_update_shutdown_schedule.pb.go"
                        "UpdateShutdownScheduleRequest")
                       #'string<)
                 '("cancel" "now" "schedule"))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-action-arms-pinned ()
  "UpdateMergeQueueRequest's action oneof has exactly the eight arms encoded
here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_update_merge_queue.pb.go"
                        "UpdateMergeQueueRequest")
                       #'string<)
                 '("disableRacing" "enableRacing" "evict" "moveDown" "moveTo" "moveUp" "pause" "resume"))))

(ert-deftest agent-repl-test-wire-verbs-daemon-health-arms-pinned ()
  "DaemonHealthSuccess's health oneof has exactly the two verdict arms decoded
here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_daemon_health.pb.go"
                        "DaemonHealthSuccess")
                       #'string<)
                 '("healthy" "unhealthy"))))

(ert-deftest agent-repl-test-wire-verbs-session-health-arms-pinned ()
  "SessionHealthSuccess's health oneof has exactly the two verdict arms decoded
here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_session_health.pb.go"
                        "SessionHealthSuccess")
                       #'string<)
                 '("healthy" "unhealthy"))))

;;;; ---- <Rpc>Error cause arms (landing 4) --------------------------------
;;
;; One test per arm the proto declares, plus the unset-oneof and unknown-arm
;; refusals, plus the per-rpc pin against the checked-in Go bindings.  The
;; PROTO is the arm list; the pins are what make a landed arm this codec has
;; not been taught fail loudly rather than silently decode as an unknown key.

(ert-deftest agent-repl-test-wire-verbs-create-error-ungated-without-consent-arm ()
  "CreateWorkspaceError's `ungated_without_consent' arm decodes with everything
it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"ungatedWithoutConsent\":{}}"))
                   '(:cause (:arm :ungated-without-consent :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-no-slug-arm ()
  "CreateWorkspaceError's `no_slug' arm decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"noSlug\":{}}"))
                   '(:cause (:arm :no-slug :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-brief-missing-arm ()
  "CreateWorkspaceError's `brief_missing' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"briefMissing\":{\"name\":\"nightly\"}}"))
                   '(:cause (:arm :brief-missing :value (:name "nightly")))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-unknown-repository-arm ()
  "CreateWorkspaceError's `unknown_repository' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"unknownRepository\":{}}"))
                   '(:cause (:arm :unknown-repository :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-worktree-creation-failed-arm ()
  "CreateWorkspaceError's `worktree_creation_failed' arm decodes with
everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"worktreeCreationFailed\":{\"detail\":\"fatal: exists\"}}"))
                   '(:cause (:arm :worktree-creation-failed :value (:detail "fatal: exists")))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-one-shot-policy-missing-arm ()
  "CreateWorkspaceError's `one_shot_policy_missing' arm decodes with everything
it carries: the repository, the directory it must state its policy in, and
the files that directory does not hold."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse
                     "{\"oneShotPolicyMissing\":{\"repositoryRoot\":\"/src/p\",\"policyDir\":\"/src/p/.agent-repl/prompts\",\"missingFiles\":[\"oneshot-completion-directive.md\"]}}"))
                   '(:cause (:arm :one-shot-policy-missing
                             :value (:repository-root "/src/p"
                                     :policy-dir "/src/p/.agent-repl/prompts"
                                     :missing-files ("oneshot-completion-directive.md"))))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-one-shot-policy-missing-empty-file-list ()
  "The arm's `missing_files' is a repeated field, so an omitted one decodes as
no files rather than as a breach."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (plist-get (plist-get (plist-get
                               (agent-repl-wire-decode-create-workspace-error
                                (agent-repl-test-wire-verbs--parse
                                 "{\"oneShotPolicyMissing\":{\"repositoryRoot\":\"/src/p\",\"policyDir\":\"/src/p/.agent-repl/prompts\"}}"))
                               :cause)
                              :value)
                              :missing-files)
                   nil))))

(ert-deftest agent-repl-test-wire-verbs-create-error-unset-cause-is-a-breach ()
  "CreateWorkspaceError with no arm set says nothing actionable, so it is a
breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-create-workspace-error (agent-repl-test-wire-verbs--parse "{}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-create-error-unknown-arm-is-a-breach ()
  "An arm CreateWorkspaceError does not declare here is refused, never guessed
at."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-create-workspace-error
                   (agent-repl-test-wire-verbs--parse "{\"noSuchArm\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-create-error-naming-failed-arm ()
  "CreateWorkspaceError's `naming_failed' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse
                     "{\"namingFailed\":{\"model\":\"haiku\",\"cause\":\"invalid_answer\",\"attempts\":2,\"answer\":\"Fix The Login\"}}"))
                   '(:cause (:arm :naming-failed
                                  :value (:model "haiku" :cause "invalid_answer"
                                          :attempts 2 :answer "Fix The Login")))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-naming-failed-with-no-answer ()
  "A naming call that never answered decodes with an empty `answer'."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (plist-get
                    (plist-get (plist-get (agent-repl-wire-decode-create-workspace-error
                                           (agent-repl-test-wire-verbs--parse
                                            "{\"namingFailed\":{\"model\":\"haiku\",\"cause\":\"timeout\",\"attempts\":2}}"))
                                          :cause)
                               :value)
                    :answer)
                   ""))))

(ert-deftest agent-repl-test-wire-verbs-create-error-inside-temporary-directory-arm ()
  "CreateWorkspaceError's `inside_temporary_directory' arm decodes with the dir
and the temporary root it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"insideTemporaryDirectory\":{\"dir\":\"/private/tmp/scratch\",\"temporaryRoot\":\"/private/tmp\"}}"))
                   '(:cause (:arm :inside-temporary-directory :value (:dir "/private/tmp/scratch" :temporary-root "/private/tmp")))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-arms-pinned ()
  "CreateWorkspaceError's arm set is exactly what the frozen schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_create_workspace.pb.go" "CreateWorkspaceError")
                       #'string<)
                 (sort (list "ungatedWithoutConsent" "noSlug" "briefMissing" "unknownRepository" "worktreeCreationFailed" "spawnFailed" "oneShotPolicyMissing" "namingFailed" "insideTemporaryDirectory" "unknownForkSource" "forkSourceHasNoConversation" "nameReserved" "invalidName" "unknownContextWorkspace" "unknownOwner" "ownerNoticeRequired" "ownerNoticeUnexpected" "oneShotOwnerLands" "unknownRequester" "asyncMergeUnknownBranch" "asyncMergeBranchOwned" "asyncMergeUnknownSubagent" "asyncMergeUnknownMaker" "asyncMergeContextNotMaker" "asyncMergeNameTaken" "asyncMergeUnknownCloseAfter" "opIdWithAsyncMerge" "versionIncrementRequired" "versionIncrementUnexpected" "asyncMergeRequesterQueued" "asyncMergeRequesterMerging" "asyncMergeRequesterSessionDeleted" "parentNotOpen" "originFetchFailed" "asyncMergeNoDeployConfig" "asyncMergeDeployConfigInvalid" "transferringAway" "standingDown" "placementBehindNotInLine" "unknownLandingTarget")
                       #'string<))))

(ert-deftest agent-repl-test-wire-verbs-create-error-unknown-fork-source-arm ()
  "CreateWorkspaceError's `unknown_fork_source' arm decodes the ref it names."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse
                     "{\"unknownForkSource\":{\"workspace\":{\"id\":\"ws-2\",\"dir\":\"/w/two\"}}}"))
                   '(:cause (:arm :unknown-fork-source
                             :value (:workspace (:id "ws-2" :dir "/w/two"))))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-unknown-context-workspace-arm ()
  "CreateWorkspaceError's `unknown_context_workspace' arm decodes the ref it names."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse
                     "{\"unknownContextWorkspace\":{\"workspace\":{\"id\":\"ws-2\",\"dir\":\"/w/two\"}}}"))
                   '(:cause (:arm :unknown-context-workspace
                             :value (:workspace (:id "ws-2" :dir "/w/two"))))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-fork-source-has-no-conversation-arm ()
  "CreateWorkspaceError's `fork_source_has_no_conversation' arm decodes the
ref it names."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse
                     "{\"forkSourceHasNoConversation\":{\"workspace\":{\"id\":\"ws-2\",\"dir\":\"/w/two\"}}}"))
                   '(:cause (:arm :fork-source-has-no-conversation
                             :value (:workspace (:id "ws-2" :dir "/w/two"))))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-unknown-owner-arm ()
  "CreateWorkspaceError's `unknown_owner' arm decodes the owner it names."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse
                     "{\"unknownOwner\":{\"owner\":{\"id\":\"ws-2\",\"dir\":\"/w/two\"}}}"))
                   '(:cause (:arm :unknown-owner :value (:owner (:id "ws-2" :dir "/w/two"))))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-owner-notice-required-arm ()
  "CreateWorkspaceError's `owner_notice_required' arm is empty."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"ownerNoticeRequired\":{}}"))
                   '(:cause (:arm :owner-notice-required :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-owner-notice-unexpected-arm ()
  "CreateWorkspaceError's `owner_notice_unexpected' arm is empty."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"ownerNoticeUnexpected\":{}}"))
                   '(:cause (:arm :owner-notice-unexpected :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-one-shot-owner-lands-arm ()
  "CreateWorkspaceError's `one_shot_owner_lands' arm decodes the owner that lands."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse
                     "{\"oneShotOwnerLands\":{\"owner\":{\"id\":\"ws-2\",\"dir\":\"/w/two\"}}}"))
                   '(:cause (:arm :one-shot-owner-lands :value (:owner (:id "ws-2" :dir "/w/two"))))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-unknown-requester-arm ()
  "CreateWorkspaceError's `unknown_requester' arm decodes the requester it names."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse
                     "{\"unknownRequester\":{\"workspace\":{\"id\":\"ws-2\",\"dir\":\"/w/two\"}}}"))
                   '(:cause (:arm :unknown-requester :value (:workspace (:id "ws-2" :dir "/w/two"))))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-async-merge-arm-is-a-breach ()
  "No editor create is an async merge, so an async-merge refusal is a breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-create-workspace-error
                   (agent-repl-test-wire-verbs--parse "{\"asyncMergeNameTaken\":{\"name\":\"x\"}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-create-error-invalid-name-slug-arm ()
  "CreateWorkspaceError's `invalid_name' arm decodes the name, its breaking
component and the `slug' rule."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse
                     "{\"invalidName\":{\"name\":\"Fix It\",\"component\":\"Fix It\",\"slug\":{}}}"))
                   '(:cause (:arm :invalid-name
                             :value (:name "Fix It" :component "Fix It" :rule :slug)))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-invalid-name-prefix-arm ()
  "The `prefix' rule decodes, and an omitted component is the empty string
protojson spells it as."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse
                     "{\"invalidName\":{\"name\":\"ABC//fix\",\"prefix\":{}}}"))
                   '(:cause (:arm :invalid-name
                             :value (:name "ABC//fix" :component "" :rule :prefix)))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-invalid-name-unset-rule-is-a-breach ()
  "An `invalid_name' arm with no rule set is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-create-workspace-error
                   (agent-repl-test-wire-verbs--parse
                    "{\"invalidName\":{\"name\":\"x y\",\"component\":\"x y\"}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-invalid-name-rule-arms-pinned ()
  "CreateWorkspaceInvalidName's rule arms are exactly what the frozen schema
declares, so the codec's arm list cannot drift from it."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_create_workspace.pb.go" "CreateWorkspaceInvalidName")
                       #'string<)
                 (sort (mapcar (lambda (arm) (symbol-name (car arm)))
                               agent-repl-wire-verbs--invalid-name-rule-arms)
                       #'string<))))

(ert-deftest agent-repl-test-wire-verbs-create-error-transferring-away-arm ()
  "CreateWorkspaceError's `transferring_away' arm decodes the successor's address."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"transferringAway\":{\"address\":\"127.0.0.1:9\"}}"))
                   '(:cause (:arm :transferring-away :value (:address "127.0.0.1:9")))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-standing-down-arm ()
  "CreateWorkspaceError's `standing_down' arm decodes as the empty arm it is."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"standingDown\":{}}"))
                   '(:cause (:arm :standing-down :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-name-reserved-arm ()
  "CreateWorkspaceError's `name_reserved' arm decodes the branch the name resolves to."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"nameReserved\":{\"branch\":\"agent-1/fix\"}}"))
                   '(:cause (:arm :name-reserved :value (:branch "agent-1/fix")))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-spawn-failed-arm ()
  "CreateWorkspaceError's `spawn_failed' arm decodes with everything it
carries.  It is the SAME daemon refusal an open answers with, so a create
that could not start a shim states it in band rather than out of it."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse
                     "{\"spawnFailed\":{\"detail\":\"the shim would not come up\"}}"))
                   '(:cause (:arm :spawn-failed
                             :value (:detail "the shim would not come up")))))))

(ert-deftest agent-repl-test-wire-verbs-open-error-unknown-workspace-arm ()
  "OpenWorkspaceError's `unknown_workspace' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-open-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"unknownWorkspace\":{}}"))
                   '(:cause (:arm :unknown-workspace :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-open-error-workspace-ref-mismatch-arm ()
  "OpenWorkspaceError's `workspace_ref_mismatch' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-open-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"workspaceRefMismatch\":{\"registryDir\":\"/w/registry\"}}"))
                   '(:cause (:arm :workspace-ref-mismatch :value (:registry-dir "/w/registry")))))))

(ert-deftest agent-repl-test-wire-verbs-open-error-transferring-away-arm ()
  "OpenWorkspaceError's `transferring_away' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-open-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"transferringAway\":{\"address\":\"127.0.0.1:9999\"}}"))
                   '(:cause (:arm :transferring-away :value (:address "127.0.0.1:9999")))))))

(ert-deftest agent-repl-test-wire-verbs-open-error-not-yet-adopted-arm ()
  "OpenWorkspaceError's `not_yet_adopted' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-open-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"notYetAdopted\":{}}"))
                   '(:cause (:arm :not-yet-adopted :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-open-error-session-deleted-arm ()
  "OpenWorkspaceError's `session_deleted' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-open-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"sessionDeleted\":{}}"))
                   '(:cause (:arm :session-deleted :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-open-error-transcript-missing-arm ()
  "OpenWorkspaceError's `transcript_missing' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-open-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"transcriptMissing\":{\"vendorSessionId\":\"vendor-9\",\"searchedPaths\":[\"/a\",\"/b\"]}}"))
                   '(:cause (:arm :transcript-missing :value (:vendor-session-id "vendor-9" :searched-paths ("/a" "/b"))))))))

(ert-deftest agent-repl-test-wire-verbs-open-error-spawn-failed-arm ()
  "OpenWorkspaceError's `spawn_failed' arm decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-open-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"spawnFailed\":{\"detail\":\"exec format error\"}}"))
                   '(:cause (:arm :spawn-failed :value (:detail "exec format error")))))))

(ert-deftest agent-repl-test-wire-verbs-open-error-worktree-unrestorable-arm ()
  "OpenWorkspaceError's `worktree_unrestorable' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-open-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"worktreeUnrestorable\":{\"dir\":\"/w/gone\",\"branch\":\"b\",\"detail\":\"gone\"}}"))
                   '(:cause (:arm :worktree-unrestorable :value (:dir "/w/gone" :branch "b" :detail "gone")))))))

(ert-deftest agent-repl-test-wire-verbs-open-error-not-merged-arm ()
  "OpenWorkspaceError's `not_merged' arm decodes as its empty arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-open-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"notMerged\":{}}"))
                   '(:cause (:arm :not-merged :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-open-error-brief-missing-arm ()
  "OpenWorkspaceError's `brief_missing' arm decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-open-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"briefMissing\":{\"detail\":\"the brief is gone\"}}"))
                   '(:cause (:arm :brief-missing :value (:detail "the brief is gone")))))))

(ert-deftest agent-repl-test-wire-verbs-open-error-brief-missing-refuses-an-unknown-field ()
  "OpenWorkspaceBriefMissing refuses a field the schema does not declare."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-open-workspace-brief-missing
                   (agent-repl-test-wire-verbs--parse "{\"detail\":\"x\",\"bogus\":1}")))))

(ert-deftest agent-repl-test-wire-verbs-open-error-vendor-start-failed-arm ()
  "OpenWorkspaceError's `vendor_start_failed' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-open-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"vendorStartFailed\":{\"detail\":\"the sdk threw\"}}"))
                   '(:cause (:arm :vendor-start-failed :value (:detail "the sdk threw")))))))

(ert-deftest agent-repl-test-wire-verbs-open-error-vendor-start-failed-empty-detail ()
  "An omitted `detail' decodes as the empty string, never as a missing key."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-open-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"vendorStartFailed\":{}}"))
                   '(:cause (:arm :vendor-start-failed :value (:detail "")))))))

(ert-deftest agent-repl-test-wire-verbs-open-error-lock-holder-unavailable-arm ()
  "OpenWorkspaceError's `lock_holder_unavailable' arm decodes with everything
it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-open-workspace-error
                    (agent-repl-test-wire-verbs--parse
                     "{\"lockHolderUnavailable\":{\"failure\":{\"binary\":\"/b/shim-lock\",\"exited\":{\"code\":1}}}}"))
                   '(:cause (:arm :lock-holder-unavailable
                             :value (:failure (:binary "/b/shim-lock"
                                               :how (:arm :exited :value (:code 1 :stderr ""))))))))))

(ert-deftest agent-repl-test-wire-verbs-open-error-lock-holder-unavailable-without-failure-is-a-breach ()
  "A lock_holder_unavailable arm that says nothing of the failure is a breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-open-workspace-error
                   (agent-repl-test-wire-verbs--parse "{\"lockHolderUnavailable\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-open-error-lock-holder-unavailable-retired-field-is-a-breach ()
  "The retired `osError' field is unknown now, never silently dropped."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-open-workspace-error
                   (agent-repl-test-wire-verbs--parse "{\"lockHolderUnavailable\":{\"osError\":\"x\"}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-open-error-unset-cause-is-a-breach ()
  "OpenWorkspaceError with no arm set says nothing actionable, so it is a
breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-open-workspace-error (agent-repl-test-wire-verbs--parse "{}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-open-error-unknown-arm-is-a-breach ()
  "An arm OpenWorkspaceError does not declare here is refused, never guessed
at."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-open-workspace-error
                   (agent-repl-test-wire-verbs--parse "{\"noSuchArm\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-open-error-arms-pinned ()
  "OpenWorkspaceError's arm set is exactly what the frozen schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_open_workspace.pb.go" "OpenWorkspaceError")
                       #'string<)
                 (sort (list "unknownWorkspace" "workspaceRefMismatch" "transferringAway" "notYetAdopted" "sessionDeleted" "transcriptMissing" "spawnFailed" "vendorStartFailed" "lockHolderUnavailable" "worktreeUnrestorable" "notMerged" "briefMissing" "parentNotOpen")
                       #'string<))))

(ert-deftest agent-repl-test-wire-verbs-close-error-blocked-arm ()
  "CloseWorkspaceError's `blocked' arm decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-close-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"blocked\":{}}"))
                   '(:cause (:arm :blocked :value (:turn-in-flight nil :live-work 0 :held-prompts 0 :merge-queued nil :summary "")))))))

(ert-deftest agent-repl-test-wire-verbs-close-blocked-evidence ()
  "CloseWorkspaceBlocked decodes every evidence field it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-close-workspace-blocked
                    (agent-repl-test-wire-verbs--parse
                     "{\"turnInFlight\":true,\"liveWork\":2,\"heldPrompts\":1,\"mergeQueued\":true,\"summary\":\"a turn is running\"}"))
                   '(:turn-in-flight t :live-work 2 :held-prompts 1
                     :merge-queued t :summary "a turn is running")))))

(ert-deftest agent-repl-test-wire-verbs-close-blocked-defaults ()
  "CloseWorkspaceBlocked's omitted scalars are the proto3 defaults."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-close-workspace-blocked
                    (agent-repl-test-wire-verbs--parse "{}"))
                   '(:turn-in-flight nil :live-work 0 :held-prompts 0 :merge-queued nil :summary "")))))

(ert-deftest agent-repl-test-wire-verbs-close-blocked-negative-live-work ()
  "A negative uint32 in CloseWorkspaceBlocked is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-close-workspace-blocked
                   (agent-repl-test-wire-verbs--parse "{\"liveWork\":-1}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-close-blocked-unknown-field ()
  "An unknown field on CloseWorkspaceBlocked is refused, not dropped."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-close-workspace-blocked
                   (agent-repl-test-wire-verbs--parse "{\"nope\":1}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-close-error-unknown-workspace-arm ()
  "CloseWorkspaceError's `unknown_workspace' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-close-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"unknownWorkspace\":{}}"))
                   '(:cause (:arm :unknown-workspace :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-close-error-workspace-ref-mismatch-arm ()
  "CloseWorkspaceError's `workspace_ref_mismatch' arm decodes with everything
it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-close-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"workspaceRefMismatch\":{\"registryDir\":\"/w/registry\"}}"))
                   '(:cause (:arm :workspace-ref-mismatch :value (:registry-dir "/w/registry")))))))

(ert-deftest agent-repl-test-wire-verbs-close-error-transferring-away-arm ()
  "CloseWorkspaceError's `transferring_away' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-close-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"transferringAway\":{\"address\":\"127.0.0.1:9999\"}}"))
                   '(:cause (:arm :transferring-away :value (:address "127.0.0.1:9999")))))))

(ert-deftest agent-repl-test-wire-verbs-close-error-not-yet-adopted-arm ()
  "CloseWorkspaceError's `not_yet_adopted' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-close-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"notYetAdopted\":{}}"))
                   '(:cause (:arm :not-yet-adopted :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-close-error-unset-cause-is-a-breach ()
  "CloseWorkspaceError with no arm set says nothing actionable, so it is a
breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-close-workspace-error (agent-repl-test-wire-verbs--parse "{}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-close-error-unknown-arm-is-a-breach ()
  "An arm CloseWorkspaceError does not declare here is refused, never guessed
at."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-close-workspace-error
                   (agent-repl-test-wire-verbs--parse "{\"noSuchArm\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-close-error-arms-pinned ()
  "CloseWorkspaceError's arm set is exactly what the frozen schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_close_workspace.pb.go" "CloseWorkspaceError")
                       #'string<)
                 (sort (list "blocked" "unknownWorkspace" "workspaceRefMismatch" "transferringAway" "notYetAdopted" "hasOpenChildren")
                       #'string<))))

(ert-deftest agent-repl-test-wire-verbs-kill-error-unknown-workspace-arm ()
  "KillWorkspaceError's `unknown_workspace' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-kill-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"unknownWorkspace\":{}}"))
                   '(:cause (:arm :unknown-workspace :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-kill-error-workspace-ref-mismatch-arm ()
  "KillWorkspaceError's `workspace_ref_mismatch' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-kill-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"workspaceRefMismatch\":{\"registryDir\":\"/w/registry\"}}"))
                   '(:cause (:arm :workspace-ref-mismatch :value (:registry-dir "/w/registry")))))))

(ert-deftest agent-repl-test-wire-verbs-kill-error-transferring-away-arm ()
  "KillWorkspaceError's `transferring_away' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-kill-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"transferringAway\":{\"address\":\"127.0.0.1:9999\"}}"))
                   '(:cause (:arm :transferring-away :value (:address "127.0.0.1:9999")))))))

(ert-deftest agent-repl-test-wire-verbs-kill-error-not-yet-adopted-arm ()
  "KillWorkspaceError's `not_yet_adopted' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-kill-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"notYetAdopted\":{}}"))
                   '(:cause (:arm :not-yet-adopted :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-kill-error-unset-cause-is-a-breach ()
  "KillWorkspaceError with no arm set says nothing actionable, so it is a
breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-kill-workspace-error (agent-repl-test-wire-verbs--parse "{}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-kill-error-unknown-arm-is-a-breach ()
  "An arm KillWorkspaceError does not declare here is refused, never guessed
at."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-kill-workspace-error
                   (agent-repl-test-wire-verbs--parse "{\"noSuchArm\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-kill-error-arms-pinned ()
  "KillWorkspaceError's arm set is exactly what the frozen schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_kill_workspace.pb.go" "KillWorkspaceError")
                       #'string<)
                 (sort (list "unknownWorkspace" "workspaceRefMismatch" "transferringAway" "notYetAdopted" "hasOpenChildren")
                       #'string<))))

(ert-deftest agent-repl-test-wire-verbs-nuke-error-unknown-workspace-arm ()
  "NukeWorkspaceError's `unknown_workspace' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-nuke-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"unknownWorkspace\":{}}"))
                   '(:cause (:arm :unknown-workspace :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-nuke-error-workspace-ref-mismatch-arm ()
  "NukeWorkspaceError's `workspace_ref_mismatch' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-nuke-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"workspaceRefMismatch\":{\"registryDir\":\"/w/registry\"}}"))
                   '(:cause (:arm :workspace-ref-mismatch :value (:registry-dir "/w/registry")))))))

(ert-deftest agent-repl-test-wire-verbs-nuke-error-transferring-away-arm ()
  "NukeWorkspaceError's `transferring_away' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-nuke-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"transferringAway\":{\"address\":\"127.0.0.1:9999\"}}"))
                   '(:cause (:arm :transferring-away :value (:address "127.0.0.1:9999")))))))

(ert-deftest agent-repl-test-wire-verbs-nuke-error-not-yet-adopted-arm ()
  "NukeWorkspaceError's `not_yet_adopted' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-nuke-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"notYetAdopted\":{}}"))
                   '(:cause (:arm :not-yet-adopted :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-nuke-error-git-failed-arm ()
  "NukeWorkspaceError's `git_failed' arm decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-nuke-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"gitFailed\":{\"detail\":\"fatal: locked\"}}"))
                   '(:cause (:arm :git-failed :value (:detail "fatal: locked")))))))

(ert-deftest agent-repl-test-wire-verbs-nuke-error-unset-cause-is-a-breach ()
  "NukeWorkspaceError with no arm set says nothing actionable, so it is a
breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-nuke-workspace-error (agent-repl-test-wire-verbs--parse "{}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-nuke-error-unknown-arm-is-a-breach ()
  "An arm NukeWorkspaceError does not declare here is refused, never guessed
at."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-nuke-workspace-error
                   (agent-repl-test-wire-verbs--parse "{\"noSuchArm\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-nuke-error-arms-pinned ()
  "NukeWorkspaceError's arm set is exactly what the frozen schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_nuke_workspace.pb.go" "NukeWorkspaceError")
                       #'string<)
                 (sort (list "unknownWorkspace" "workspaceRefMismatch" "transferringAway" "notYetAdopted" "gitFailed" "hasOpenChildren"
                             "threadDeleteFailed")
                       #'string<))))

(ert-deftest agent-repl-test-wire-verbs-nuke-error-thread-delete-failed-arm ()
  "NukeWorkspaceError's `thread_delete_failed' arm decodes with its detail."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-nuke-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"threadDeleteFailed\":{\"detail\":\"zettel down\"}}"))
                   '(:cause (:arm :thread-delete-failed :value (:detail "zettel down")))))))

;;;; ---- FinishWorkspaceRequest ---------------------------------------

(defun agent-repl-test-wire-verbs--finish-request (&rest edits)
  "A user\='s back-of-queue FinishWorkspaceRequest for the fixture ref, with
EDITS (a plist) replacing its fields."
  (let ((request (list :workspace agent-repl-test-wire-verbs--ref
                       :requester '(:arm :user)
                       :placement '(:arm :back))))
    (while edits
      (setq request (plist-put request (pop edits) (pop edits))))
    request))

(ert-deftest agent-repl-test-wire-verbs-finish-request-encodes-its-three-fields ()
  "A finish request spells its workspace, its requester and its placement."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize (agent-repl-wire-encode-finish-workspace-request
                                    (agent-repl-test-wire-verbs--finish-request)))
                   (concat "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},"
                           "\"requester\":{\"user\":{}},"
                           "\"placement\":{\"back\":{}}}")))))

(ert-deftest agent-repl-test-wire-verbs-finish-request-front-placement ()
  "A front placement spells the empty `front' arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (cdr (assq 'placement
                              (agent-repl-wire-encode-finish-workspace-request
                               (agent-repl-test-wire-verbs--finish-request :placement '(:arm :front)))))
                   '((front))))))

(ert-deftest agent-repl-test-wire-verbs-finish-request-refuses-an-unset-field ()
  "A finish request naming no workspace, requester or placement is refused:
nothing is implied."
  (agent-repl-test-wire-verbs--with-common
    (dolist (field '(:workspace :requester :placement))
      (should (equal (cdr (should-error
                           (agent-repl-wire-encode-finish-workspace-request
                            (agent-repl-test-wire-verbs--finish-request field nil))
                           :type 'agent-repl-wire-error))
                     (list "FinishWorkspaceRequest" (substring (symbol-name field) 1)
                           "required field is unset"))))))

;;;; ---- FinishWorkspaceError -------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-finish-error-unknown-workspace-arm ()
  "FinishWorkspaceError's `unknown_workspace' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-finish-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"unknownWorkspace\":{}}"))
                   '(:cause (:arm :unknown-workspace :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-finish-error-workspace-ref-mismatch-arm ()
  "FinishWorkspaceError's `workspace_ref_mismatch' arm decodes with everything
it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-finish-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"workspaceRefMismatch\":{\"registryDir\":\"/w/registry\"}}"))
                   '(:cause (:arm :workspace-ref-mismatch :value (:registry-dir "/w/registry")))))))

(ert-deftest agent-repl-test-wire-verbs-finish-error-transferring-away-arm ()
  "FinishWorkspaceError's `transferring_away' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-finish-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"transferringAway\":{\"address\":\"127.0.0.1:9999\"}}"))
                   '(:cause (:arm :transferring-away :value (:address "127.0.0.1:9999")))))))

(ert-deftest agent-repl-test-wire-verbs-finish-error-not-yet-adopted-arm ()
  "FinishWorkspaceError's `not_yet_adopted' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-finish-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"notYetAdopted\":{}}"))
                   '(:cause (:arm :not-yet-adopted :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-finish-error-no-layout-facts-arm ()
  "FinishWorkspaceError's `no_layout_facts' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-finish-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"noLayoutFacts\":{}}"))
                   '(:cause (:arm :no-layout-facts :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-finish-error-session-deleted-arm ()
  "FinishWorkspaceError's `session_deleted' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-finish-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"sessionDeleted\":{}}"))
                   '(:cause (:arm :session-deleted :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-finish-error-already-queued-arm ()
  "FinishWorkspaceError's `already_queued' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-finish-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"alreadyQueued\":{}}"))
                   '(:cause (:arm :already-queued :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-finish-error-already-deploying-arm ()
  "FinishWorkspaceError's `already_deploying' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-finish-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"alreadyDeploying\":{}}"))
                   '(:cause (:arm :already-deploying :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-finish-error-no-deploy-config-arm ()
  "FinishWorkspaceError's `no_deploy_config' arm decodes the path looked for."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-finish-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"noDeployConfig\":{\"path\":\"/r/.agent-repl/deploy.yaml\"}}"))
                   '(:cause (:arm :no-deploy-config :value (:path "/r/.agent-repl/deploy.yaml")))))))

(ert-deftest agent-repl-test-wire-verbs-finish-error-deploy-config-invalid-arm ()
  "FinishWorkspaceError's `deploy_config_invalid' arm decodes its path and why."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-finish-workspace-error
                    (agent-repl-test-wire-verbs--parse
                     "{\"deployConfigInvalid\":{\"path\":\"/r/.agent-repl/deploy.yaml\",\"detail\":\"pre_deploy is required\"}}"))
                   '(:cause (:arm :deploy-config-invalid
                             :value (:path "/r/.agent-repl/deploy.yaml" :detail "pre_deploy is required")))))))

(ert-deftest agent-repl-test-wire-verbs-finish-error-owner-lands-arm ()
  "FinishWorkspaceError's `owner_lands' arm decodes the owner that lands."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-finish-workspace-error
                    (agent-repl-test-wire-verbs--parse
                     "{\"ownerLands\":{\"owner\":{\"id\":\"ws-2\",\"dir\":\"/w/two\"}}}"))
                   '(:cause (:arm :owner-lands :value (:owner (:id "ws-2" :dir "/w/two"))))))))

(ert-deftest agent-repl-test-wire-verbs-finish-error-requester-may-not-land-arm ()
  "FinishWorkspaceError's `requester_may_not_land' arm is empty."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-finish-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"requesterMayNotLand\":{}}"))
                   '(:cause (:arm :requester-may-not-land :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-finish-error-no-branch-checked-out-arm ()
  "FinishWorkspaceError's `no_branch_checked_out' arm decodes the dir it names."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-finish-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"noBranchCheckedOut\":{\"dir\":\"/w/one\"}}"))
                   '(:cause (:arm :no-branch-checked-out :value (:dir "/w/one")))))))

(ert-deftest agent-repl-test-wire-verbs-finish-error-version-increment-required-arm ()
  "FinishWorkspaceError's `version_increment_required' arm decodes."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-finish-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"versionIncrementRequired\":{}}"))
                   '(:cause (:arm :version-increment-required :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-finish-error-version-increment-unexpected-unversioned-repository ()
  "FinishWorkspaceError's `version_increment_unexpected' arm decodes its
unversioned-repository reason."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-finish-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"versionIncrementUnexpected\":{\"unversionedRepository\":{}}}"))
                   '(:cause (:arm :version-increment-unexpected
                             :value (:reason (:arm :unversioned-repository :value nil))))))))

(ert-deftest agent-repl-test-wire-verbs-finish-error-version-increment-unexpected-not-default-branch ()
  "FinishWorkspaceError's `version_increment_unexpected' arm decodes its
not-default-branch reason."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-finish-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"versionIncrementUnexpected\":{\"notDefaultBranch\":{}}}"))
                   '(:cause (:arm :version-increment-unexpected
                             :value (:reason (:arm :not-default-branch :value nil))))))))

(ert-deftest agent-repl-test-wire-verbs-finish-error-version-increment-unexpected-reason-arms-pinned ()
  "MergeWorkspaceVersionIncrementUnexpected's reason arm set is exactly what
the frozen schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/merge_version_increment.pb.go" "MergeWorkspaceVersionIncrementUnexpected")
                       #'string<)
                 (sort (list "unversionedRepository" "notDefaultBranch") #'string<))))

(ert-deftest agent-repl-test-wire-verbs-finish-error-unset-cause-is-a-breach ()
  "FinishWorkspaceError with no arm set says nothing actionable, so it is a
breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-finish-workspace-error (agent-repl-test-wire-verbs--parse "{}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-finish-error-unknown-arm-is-a-breach ()
  "An arm FinishWorkspaceError does not declare here is refused, never guessed
at."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-finish-workspace-error
                   (agent-repl-test-wire-verbs--parse "{\"noSuchArm\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-finish-error-arms-pinned ()
  "FinishWorkspaceError's arm set is exactly what the frozen schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_finish_workspace.pb.go" "FinishWorkspaceError")
                       #'string<)
                 (sort (list "unknownWorkspace" "workspaceRefMismatch" "transferringAway" "notYetAdopted" "noLayoutFacts" "sessionDeleted" "alreadyQueued" "alreadyDeploying" "versionIncrementRequired" "versionIncrementUnexpected" "ownerLands" "requesterMayNotLand" "noBranchCheckedOut" "hasOpenChildren" "noDeployConfig" "deployConfigInvalid" "placementBehindNotInLine")
                       #'string<))))

(ert-deftest agent-repl-test-wire-verbs-finish-error-placement-behind-not-in-line-arm ()
  "FinishWorkspaceError's `placement_behind_not_in_line' arm decodes the merge
the placement named."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-finish-workspace-error
                    (agent-repl-test-wire-verbs--parse
                     "{\"placementBehindNotInLine\":{\"behind\":\"two\"}}"))
                   '(:cause (:arm :placement-behind-not-in-line :value (:behind "two")))))))

(ert-deftest agent-repl-test-wire-verbs-restart-error-unknown-workspace-arm ()
  "RestartWorkspaceError's `unknown_workspace' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-restart-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"unknownWorkspace\":{}}"))
                   '(:cause (:arm :unknown-workspace :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-restart-error-workspace-ref-mismatch-arm ()
  "RestartWorkspaceError's `workspace_ref_mismatch' arm decodes with everything
it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-restart-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"workspaceRefMismatch\":{\"registryDir\":\"/w/registry\"}}"))
                   '(:cause (:arm :workspace-ref-mismatch :value (:registry-dir "/w/registry")))))))

(ert-deftest agent-repl-test-wire-verbs-restart-error-transferring-away-arm ()
  "RestartWorkspaceError's `transferring_away' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-restart-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"transferringAway\":{\"address\":\"127.0.0.1:9999\"}}"))
                   '(:cause (:arm :transferring-away :value (:address "127.0.0.1:9999")))))))

(ert-deftest agent-repl-test-wire-verbs-restart-error-not-yet-adopted-arm ()
  "RestartWorkspaceError's `not_yet_adopted' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-restart-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"notYetAdopted\":{}}"))
                   '(:cause (:arm :not-yet-adopted :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-restart-error-no-session-arm-is-retired ()
  "The retired `no_session' arm is refused: every restart is immediate, so a
workspace with no session is restarted by bringing one up."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-restart-workspace-error
                   (agent-repl-test-wire-verbs--parse "{\"noSession\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-restart-error-unset-cause-is-a-breach ()
  "RestartWorkspaceError with no arm set says nothing actionable, so it is a
breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-restart-workspace-error (agent-repl-test-wire-verbs--parse "{}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-restart-error-unknown-arm-is-a-breach ()
  "An arm RestartWorkspaceError does not declare here is refused, never guessed
at."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-restart-workspace-error
                   (agent-repl-test-wire-verbs--parse "{\"noSuchArm\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-restart-error-arms-pinned ()
  "RestartWorkspaceError's arm set is exactly what the frozen schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_restart_workspace.pb.go" "RestartWorkspaceError")
                       #'string<)
                 (sort (list "unknownWorkspace" "workspaceRefMismatch" "transferringAway" "notYetAdopted" "relaunchFailed")
                       #'string<))))

(ert-deftest agent-repl-test-wire-verbs-restart-error-relaunch-failed-arm ()
  "RestartWorkspaceError's `relaunch_failed' arm decodes with its detail."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-restart-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"relaunchFailed\":{\"detail\":\"the shim died\"}}"))
                   '(:cause (:arm :relaunch-failed :value (:detail "the shim died")))))))

(ert-deftest agent-repl-test-wire-verbs-set-priority-error-unknown-workspace-arm ()
  "SetWorkspacePriorityError's `unknown_workspace' arm decodes with everything
it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-set-workspace-priority-error
                    (agent-repl-test-wire-verbs--parse "{\"unknownWorkspace\":{}}"))
                   '(:cause (:arm :unknown-workspace :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-set-priority-error-workspace-ref-mismatch-arm ()
  "SetWorkspacePriorityError's `workspace_ref_mismatch' arm decodes with
everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-set-workspace-priority-error
                    (agent-repl-test-wire-verbs--parse "{\"workspaceRefMismatch\":{\"registryDir\":\"/w/registry\"}}"))
                   '(:cause (:arm :workspace-ref-mismatch :value (:registry-dir "/w/registry")))))))

(ert-deftest agent-repl-test-wire-verbs-set-priority-error-transferring-away-arm ()
  "SetWorkspacePriorityError's `transferring_away' arm decodes with everything
it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-set-workspace-priority-error
                    (agent-repl-test-wire-verbs--parse "{\"transferringAway\":{\"address\":\"127.0.0.1:9999\"}}"))
                   '(:cause (:arm :transferring-away :value (:address "127.0.0.1:9999")))))))

(ert-deftest agent-repl-test-wire-verbs-set-priority-error-not-yet-adopted-arm ()
  "SetWorkspacePriorityError's `not_yet_adopted' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-set-workspace-priority-error
                    (agent-repl-test-wire-verbs--parse "{\"notYetAdopted\":{}}"))
                   '(:cause (:arm :not-yet-adopted :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-set-priority-error-unset-cause-is-a-breach ()
  "SetWorkspacePriorityError with no arm set says nothing actionable, so it is
a breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-set-workspace-priority-error (agent-repl-test-wire-verbs--parse "{}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-set-priority-error-unknown-arm-is-a-breach ()
  "An arm SetWorkspacePriorityError does not declare here is refused, never
guessed at."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-set-workspace-priority-error
                   (agent-repl-test-wire-verbs--parse "{\"noSuchArm\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-set-priority-error-arms-pinned ()
  "SetWorkspacePriorityError's arm set is exactly what the frozen schema
declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_set_workspace_priority.pb.go" "SetWorkspacePriorityError")
                       #'string<)
                 (sort (list "unknownWorkspace" "workspaceRefMismatch" "transferringAway" "notYetAdopted")
                       #'string<))))

(ert-deftest agent-repl-test-wire-verbs-submit-error-merging-arm ()
  "SubmitPromptError's `merging' arm decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-error
                    (agent-repl-test-wire-verbs--parse "{\"merging\":{}}"))
                   '(:reason (:arm :merging :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-submit-error-unknown-workspace-arm ()
  "SubmitPromptError's `unknown_workspace' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-error
                    (agent-repl-test-wire-verbs--parse "{\"unknownWorkspace\":{}}"))
                   '(:reason (:arm :unknown-workspace :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-submit-error-workspace-ref-mismatch-arm ()
  "SubmitPromptError's `workspace_ref_mismatch' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-error
                    (agent-repl-test-wire-verbs--parse "{\"workspaceRefMismatch\":{\"registryDir\":\"/w/registry\"}}"))
                   '(:reason (:arm :workspace-ref-mismatch :value (:registry-dir "/w/registry")))))))

(ert-deftest agent-repl-test-wire-verbs-submit-error-transferring-away-arm ()
  "SubmitPromptError's `transferring_away' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-error
                    (agent-repl-test-wire-verbs--parse "{\"transferringAway\":{\"address\":\"127.0.0.1:9999\"}}"))
                   '(:reason (:arm :transferring-away :value (:address "127.0.0.1:9999")))))))

(ert-deftest agent-repl-test-wire-verbs-submit-error-not-yet-adopted-arm ()
  "SubmitPromptError's `not_yet_adopted' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-error
                    (agent-repl-test-wire-verbs--parse "{\"notYetAdopted\":{}}"))
                   '(:reason (:arm :not-yet-adopted :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-submit-success-command-acted-arm ()
  "SubmitPromptSuccess's `command_acted' arm decodes as the answer it is:
empty, with nothing to await."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-success
                    (agent-repl-test-wire-verbs--parse "{\"commandActed\":{}}"))
                   '(:arm :command-acted :value nil)))))

(ert-deftest agent-repl-test-wire-verbs-submit-success-ask-answered-arm ()
  "SubmitPromptSuccess's `ask_answered' arm decodes as the answer it is:
empty, with no turn to await."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-success
                    (agent-repl-test-wire-verbs--parse "{\"askAnswered\":{}}"))
                   '(:arm :ask-answered :value nil)))))

(ert-deftest agent-repl-test-wire-verbs-submit-success-response-revising-arm ()
  "SubmitPromptSuccess's `response_revising' arm decodes the revised
response's row."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-success
                    (agent-repl-test-wire-verbs--parse
                     "{\"responseRevising\":{\"row\":{\"value\":\"row-1\"}}}"))
                   '(:arm :response-revising :value (:row (:value "row-1")))))))

(ert-deftest agent-repl-test-wire-verbs-submit-success-response-revising-needs-row ()
  "A `response_revising' arm that names no row is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-submit-prompt-success
                   (agent-repl-test-wire-verbs--parse "{\"responseRevising\":{}}")))))

(ert-deftest agent-repl-test-wire-verbs-submit-error-ask-not-standing-arm ()
  "SubmitPromptError's `ask_not_standing' arm decodes empty."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-error
                    (agent-repl-test-wire-verbs--parse "{\"askNotStanding\":{}}"))
                   '(:reason (:arm :ask-not-standing :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-error-unknown-repository-arm ()
  "UpdateMergeQueueError's `unknown_repository' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-update-merge-queue-error
                    (agent-repl-test-wire-verbs--parse "{\"unknownRepository\":{}}"))
                   '(:cause (:arm :unknown-repository :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-submit-error-feed-not-in-workspace-arm ()
  "SubmitPromptError's `feed_not_in_workspace' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-error
                    (agent-repl-test-wire-verbs--parse "{\"feedNotInWorkspace\":{}}"))
                   '(:reason (:arm :feed-not-in-workspace :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-submit-error-feed-undecodable-arm ()
  "SubmitPromptError's `feed_undecodable' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-error
                    (agent-repl-test-wire-verbs--parse "{\"feedUndecodable\":{}}"))
                   '(:reason (:arm :feed-undecodable :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-submit-error-turn-already-open-is-retired ()
  "`turn_already_open' was retired at landing 6, so it is an unknown arm now
and is refused loudly rather than decoded."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-submit-prompt-error
                   (agent-repl-test-wire-verbs--parse "{\"turnAlreadyOpen\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-submit-error-duplicate-submission-arm ()
  "SubmitPromptError's `duplicate_submission' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-error
                    (agent-repl-test-wire-verbs--parse "{\"duplicateSubmission\":{}}"))
                   '(:reason (:arm :duplicate-submission :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-submit-error-no-session-arm ()
  "SubmitPromptError's `no_session' arm decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-error
                    (agent-repl-test-wire-verbs--parse "{\"noSession\":{}}"))
                   '(:reason (:arm :no-session :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-submit-error-unset-reason-is-a-breach ()
  "SubmitPromptError with no arm set says nothing actionable, so it is a
breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-submit-prompt-error (agent-repl-test-wire-verbs--parse "{}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-submit-error-unknown-arm-is-a-breach ()
  "An arm SubmitPromptError does not declare here is refused, never guessed at."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-submit-prompt-error
                   (agent-repl-test-wire-verbs--parse "{\"noSuchArm\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-submit-error-bubble-refused-arm ()
  "SubmitPromptError's `bubble_refused' arm decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-error
                    (agent-repl-test-wire-verbs--parse
                     "{\"bubbleRefused\":{\"detail\":\"no route\",\"notDeliverable\":{}}}"))
                   '(:reason (:arm :bubble-refused
                              :value (:detail "no route"
                                      :kind (:arm :not-deliverable :value nil))))))))

(ert-deftest agent-repl-test-wire-verbs-submit-bubble-refused-agent-busy-kind ()
  "SubmitPromptBubbleRefused's `agent_busy' kind decodes to its keyword."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-bubble-refused
                    (agent-repl-test-wire-verbs--parse
                     "{\"detail\":\"busy\",\"agentBusy\":{}}"))
                   '(:detail "busy" :kind (:arm :agent-busy :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-submit-bubble-refused-detail-default ()
  "SubmitPromptBubbleRefused's omitted detail is the empty string."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (plist-get (agent-repl-wire-decode-submit-prompt-bubble-refused
                               (agent-repl-test-wire-verbs--parse "{\"agentBusy\":{}}"))
                              :detail)
                   ""))))

(ert-deftest agent-repl-test-wire-verbs-submit-bubble-refused-kind-unset ()
  "SubmitPromptBubbleRefused with no kind arm set is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-submit-prompt-bubble-refused
                   (agent-repl-test-wire-verbs--parse "{\"detail\":\"x\"}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-submit-bubble-refused-two-kinds ()
  "SubmitPromptBubbleRefused with two kind arms set is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-submit-prompt-bubble-refused
                   (agent-repl-test-wire-verbs--parse
                    "{\"notDeliverable\":{},\"agentBusy\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-submit-bubble-refused-unknown-field ()
  "An unknown field on SubmitPromptBubbleRefused is refused, not dropped."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-submit-prompt-bubble-refused
                   (agent-repl-test-wire-verbs--parse "{\"agentBusy\":{},\"nope\":1}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-submit-error-cold-gate-arm ()
  "SubmitPromptError's `cold_gate' arm decodes with the gate's own account."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-error
                    (agent-repl-test-wire-verbs--parse
                     "{\"coldGate\":{\"detail\":\"context cold\"}}"))
                   '(:reason (:arm :cold-gate :value (:detail "context cold")))))))

(ert-deftest agent-repl-test-wire-verbs-submit-error-model-not-in-catalog-arm ()
  "SubmitPromptError's `model_not_in_catalog' arm decodes with the shim's sentence."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-error
                    (agent-repl-test-wire-verbs--parse
                     "{\"modelNotInCatalog\":{\"detail\":\"opus is not in the catalog\"}}"))
                   '(:reason (:arm :model-not-in-catalog
                              :value (:detail "opus is not in the catalog")))))))

(ert-deftest agent-repl-test-wire-verbs-submit-error-model-refused-arm ()
  "SubmitPromptError's `model_refused' arm decodes with the vendor's sentence."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-error
                    (agent-repl-test-wire-verbs--parse
                     "{\"modelRefused\":{\"detail\":\"refused\"}}"))
                   '(:reason (:arm :model-refused :value (:detail "refused")))))))

(ert-deftest agent-repl-test-wire-verbs-submit-cold-gate-detail-default ()
  "SubmitPromptColdGate's omitted detail is the empty string."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (plist-get (agent-repl-wire-decode-submit-prompt-cold-gate
                               (agent-repl-test-wire-verbs--parse "{}"))
                              :detail)
                   ""))))

(ert-deftest agent-repl-test-wire-verbs-submit-cold-gate-unknown-field ()
  "An unknown field on SubmitPromptColdGate is refused, not dropped."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-submit-prompt-cold-gate
                   (agent-repl-test-wire-verbs--parse "{\"detail\":\"x\",\"nope\":1}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-submit-bubble-kind-arms-pinned ()
  "SubmitPromptBubbleRefused's kind oneof has exactly the arms this codec
decodes."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_submit_prompt.pb.go"
                        "SubmitPromptBubbleRefused")
                       #'string<)
                 '("agentBusy" "notDeliverable"))))

(ert-deftest agent-repl-test-wire-verbs-submit-error-arms-pinned ()
  "SubmitPromptError's arm set is exactly what the frozen schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_submit_prompt.pb.go" "SubmitPromptError")
                       #'string<)
                 (sort (list "merging" "unknownWorkspace" "workspaceRefMismatch" "transferringAway" "notYetAdopted" "feedNotInWorkspace" "feedUndecodable" "noSession" "duplicateSubmission" "bubbleRefused" "coldGate" "modelNotInCatalog" "modelRefused" "askNotStanding" "slashCommandFromAgent" "unknownSender" "threadAwaitingFirstAnswer")
                       #'string<))))

(ert-deftest agent-repl-test-wire-verbs-shutdown-error-nothing-scheduled-arm ()
  "UpdateShutdownScheduleError's `nothing_scheduled' arm decodes with
everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-update-shutdown-schedule-error
                    (agent-repl-test-wire-verbs--parse "{\"nothingScheduled\":{}}"))
                   '(:cause (:arm :nothing-scheduled :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-shutdown-error-unset-cause-is-a-breach ()
  "UpdateShutdownScheduleError with no arm set says nothing actionable, so it
is a breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-update-shutdown-schedule-error (agent-repl-test-wire-verbs--parse "{}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-shutdown-error-unknown-arm-is-a-breach ()
  "An arm UpdateShutdownScheduleError does not declare here is refused, never
guessed at."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-update-shutdown-schedule-error
                   (agent-repl-test-wire-verbs--parse "{\"noSuchArm\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-shutdown-error-arms-pinned ()
  "UpdateShutdownScheduleError's arm set is exactly what the frozen schema
declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_update_shutdown_schedule.pb.go" "UpdateShutdownScheduleError")
                       #'string<)
                 (sort (list "nothingScheduled")
                       #'string<))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-error-unknown-workspace-arm ()
  "UpdateMergeQueueError's `unknown_workspace' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-update-merge-queue-error
                    (agent-repl-test-wire-verbs--parse "{\"unknownWorkspace\":{}}"))
                   '(:cause (:arm :unknown-workspace :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-error-workspace-ref-mismatch-arm ()
  "UpdateMergeQueueError's `workspace_ref_mismatch' arm decodes with everything
it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-update-merge-queue-error
                    (agent-repl-test-wire-verbs--parse "{\"workspaceRefMismatch\":{\"registryDir\":\"/w/registry\"}}"))
                   '(:cause (:arm :workspace-ref-mismatch :value (:registry-dir "/w/registry")))))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-error-transferring-away-arm ()
  "UpdateMergeQueueError's `transferring_away' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-update-merge-queue-error
                    (agent-repl-test-wire-verbs--parse "{\"transferringAway\":{\"address\":\"127.0.0.1:9999\"}}"))
                   '(:cause (:arm :transferring-away :value (:address "127.0.0.1:9999")))))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-error-not-yet-adopted-arm ()
  "UpdateMergeQueueError's `not_yet_adopted' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-update-merge-queue-error
                    (agent-repl-test-wire-verbs--parse "{\"notYetAdopted\":{}}"))
                   '(:cause (:arm :not-yet-adopted :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-error-already-paused-arm ()
  "UpdateMergeQueueError's `already_paused' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-update-merge-queue-error
                    (agent-repl-test-wire-verbs--parse "{\"alreadyPaused\":{}}"))
                   '(:cause (:arm :already-paused :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-error-not-paused-arm ()
  "UpdateMergeQueueError's `not_paused' arm decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-update-merge-queue-error
                    (agent-repl-test-wire-verbs--parse "{\"notPaused\":{}}"))
                   '(:cause (:arm :not-paused :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-error-no-such-queued-merge-arm ()
  "UpdateMergeQueueError's `no_such_queued_merge' arm decodes with everything
it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-update-merge-queue-error
                    (agent-repl-test-wire-verbs--parse "{\"noSuchQueuedMerge\":{}}"))
                   '(:cause (:arm :no-such-queued-merge :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-error-racing-already-disabled-arm ()
  "UpdateMergeQueueError's `racing_already_disabled' arm decodes with
everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-update-merge-queue-error
                    (agent-repl-test-wire-verbs--parse "{\"racingAlreadyDisabled\":{}}"))
                   '(:cause (:arm :racing-already-disabled :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-error-racing-not-disabled-arm ()
  "UpdateMergeQueueError's `racing_not_disabled' arm decodes with everything
it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-update-merge-queue-error
                    (agent-repl-test-wire-verbs--parse "{\"racingNotDisabled\":{}}"))
                   '(:cause (:arm :racing-not-disabled :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-error-unset-cause-is-a-breach ()
  "UpdateMergeQueueError with no arm set says nothing actionable, so it is a
breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-update-merge-queue-error (agent-repl-test-wire-verbs--parse "{}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-error-unknown-arm-is-a-breach ()
  "An arm UpdateMergeQueueError does not declare here is refused, never guessed
at."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-update-merge-queue-error
                   (agent-repl-test-wire-verbs--parse "{\"noSuchArm\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-error-arms-pinned ()
  "UpdateMergeQueueError's arm set is exactly what the frozen schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_update_merge_queue.pb.go" "UpdateMergeQueueError")
                       #'string<)
                 (sort (list "unknownWorkspace" "workspaceRefMismatch" "transferringAway" "notYetAdopted" "alreadyPaused" "notPaused" "noSuchQueuedMerge" "unknownRepository" "racingAlreadyDisabled" "racingNotDisabled")
                       #'string<))))

(ert-deftest agent-repl-test-wire-verbs-session-health-error-unknown-workspace-arm ()
  "SessionHealthError's `unknown_workspace' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-session-health-error
                    (agent-repl-test-wire-verbs--parse "{\"unknownWorkspace\":{}}"))
                   '(:cause (:arm :unknown-workspace :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-session-health-error-workspace-ref-mismatch-arm ()
  "SessionHealthError's `workspace_ref_mismatch' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-session-health-error
                    (agent-repl-test-wire-verbs--parse "{\"workspaceRefMismatch\":{\"registryDir\":\"/w/registry\"}}"))
                   '(:cause (:arm :workspace-ref-mismatch :value (:registry-dir "/w/registry")))))))

(ert-deftest agent-repl-test-wire-verbs-session-health-error-transferring-away-arm ()
  "SessionHealthError's `transferring_away' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-session-health-error
                    (agent-repl-test-wire-verbs--parse "{\"transferringAway\":{\"address\":\"127.0.0.1:9999\"}}"))
                   '(:cause (:arm :transferring-away :value (:address "127.0.0.1:9999")))))))

(ert-deftest agent-repl-test-wire-verbs-session-health-error-not-yet-adopted-arm ()
  "SessionHealthError's `not_yet_adopted' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-session-health-error
                    (agent-repl-test-wire-verbs--parse "{\"notYetAdopted\":{}}"))
                   '(:cause (:arm :not-yet-adopted :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-session-health-error-unset-cause-is-a-breach ()
  "SessionHealthError with no arm set says nothing actionable, so it is a
breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-session-health-error (agent-repl-test-wire-verbs--parse "{}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-session-health-error-unknown-arm-is-a-breach ()
  "An arm SessionHealthError does not declare here is refused, never guessed
at."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-session-health-error
                   (agent-repl-test-wire-verbs--parse "{\"noSuchArm\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-session-health-error-arms-pinned ()
  "SessionHealthError's arm set is exactly what the frozen schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_session_health.pb.go" "SessionHealthError")
                       #'string<)
                 (sort (list "unknownWorkspace" "workspaceRefMismatch" "transferringAway" "notYetAdopted")
                       #'string<))))

;;;; ---- SessionFault kinds (landing 4) -----------------------------------

(ert-deftest agent-repl-test-wire-verbs-session-fault-shim-start-failed-kind ()
  "SessionFault's `shim_start_failed' kind decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-session-fault
                    (agent-repl-test-wire-verbs--parse "{\"shimStartFailed\":{\"exitCode\":3,\"stderrTail\":\"panic\"}}"))
                   '(:detail "" :kind (:arm :shim-start-failed :value (:exit-code 3 :stderr-tail "panic")))))))

(ert-deftest agent-repl-test-wire-verbs-session-fault-vendor-start-retrying-kind ()
  "SessionFault's `vendor_start_retrying' kind decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-session-fault
                    (agent-repl-test-wire-verbs--parse "{\"vendorStartRetrying\":{\"failedAttempts\":3,\"cause\":\"timed out\",\"failingSinceMs\":\"1000\"}}"))
                   '(:detail "" :kind (:arm :vendor-start-retrying
                                       :value (:failed-attempts 3 :cause "timed out" :failing-since-ms 1000)))))))

(ert-deftest agent-repl-test-wire-verbs-session-fault-vendor-start-rejected-kind ()
  "SessionFault's `vendor_start_rejected' kind decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-session-fault
                    (agent-repl-test-wire-verbs--parse "{\"vendorStartRejected\":{\"cause\":\"auth rejected\"}}"))
                   '(:detail "" :kind (:arm :vendor-start-rejected :value (:cause "auth rejected")))))))

(ert-deftest agent-repl-test-wire-verbs-session-fault-vendor-start-failed-kind ()
  "SessionFault's `vendor_start_failed' kind decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-session-fault
                    (agent-repl-test-wire-verbs--parse "{\"vendorStartFailed\":{\"failedAttempts\":90,\"lastCause\":\"overloaded\",\"failingSinceMs\":\"2000\"}}"))
                   '(:detail "" :kind (:arm :vendor-start-failed
                                       :value (:failed-attempts 90 :last-cause "overloaded" :failing-since-ms 2000)))))))

(ert-deftest agent-repl-test-wire-verbs-session-fault-shim-died-kind ()
  "SessionFault's `shim_died' kind decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-session-fault
                    (agent-repl-test-wire-verbs--parse "{\"shimDied\":{\"exitCode\":9}}"))
                   '(:detail "" :kind (:arm :shim-died :value (:exit-code 9)))))))

(ert-deftest agent-repl-test-wire-verbs-session-fault-link-severed-kind ()
  "SessionFault's `link_severed' kind decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-session-fault
                    (agent-repl-test-wire-verbs--parse "{\"linkSevered\":{}}"))
                   '(:detail "" :kind (:arm :link-severed :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-session-fault-resume-failed-kind ()
  "SessionFault's `resume_failed' kind decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-session-fault
                    (agent-repl-test-wire-verbs--parse "{\"resumeFailed\":{\"cause\":\"no transcript\"}}"))
                   '(:detail "" :kind (:arm :resume-failed :value (:cause "no transcript")))))))

(ert-deftest agent-repl-test-wire-verbs-session-fault-bounce-died-kind ()
  "SessionFault's `bounce_died' kind decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-session-fault
                    (agent-repl-test-wire-verbs--parse "{\"bounceDied\":{}}"))
                   '(:detail "" :kind (:arm :bounce-died :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-session-fault-bounce-unknown-kind ()
  "SessionFault's `bounce_unknown' kind decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-session-fault
                    (agent-repl-test-wire-verbs--parse "{\"bounceUnknown\":{}}"))
                   '(:detail "" :kind (:arm :bounce-unknown :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-session-fault-classifier-failed-kind ()
  "SessionFault's `classifier_failed' kind decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-session-fault
                    (agent-repl-test-wire-verbs--parse "{\"classifierFailed\":{\"detail\":\"regex blew up\"}}"))
                   '(:detail "" :kind (:arm :classifier-failed :value (:detail "regex blew up")))))))

(ert-deftest agent-repl-test-wire-verbs-session-fault-shim-reported-kind ()
  "SessionFault's `shim_reported' kind decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-session-fault
                    (agent-repl-test-wire-verbs--parse "{\"shimReported\":{\"component\":\"stdout\",\"kind\":\"parse\"}}"))
                   '(:detail "" :kind (:arm :shim-reported :value (:component "stdout" :kind "parse")))))))

(ert-deftest agent-repl-test-wire-verbs-session-fault-conversation-abandoned-kind ()
  "SessionFault's `conversation_abandoned' kind decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-session-fault
                    (agent-repl-test-wire-verbs--parse "{\"conversationAbandoned\":{\"vendorSessionId\":\"vs-9\"}}"))
                   '(:detail "" :kind (:arm :conversation-abandoned :value (:vendor-session-id "vs-9")))))))

(ert-deftest agent-repl-test-wire-verbs-session-fault-session-absent-kind ()
  "SessionFault's `session_absent' kind decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-session-fault
                    (agent-repl-test-wire-verbs--parse "{\"sessionAbsent\":{}}"))
                   '(:detail "" :kind (:arm :session-absent :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-session-fault-watch-open-refused-kind ()
  "SessionFault's `watch_open_refused' kind decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-session-fault
                    (agent-repl-test-wire-verbs--parse "{\"watchOpenRefused\":{\"operation\":\"WatchTranscript\",\"handle\":\"h-3\"}}"))
                   '(:detail "" :kind (:arm :watch-open-refused :value (:operation "WatchTranscript" :handle "h-3")))))))

(ert-deftest agent-repl-test-wire-verbs-session-fault-daemon-state-unreadable-kind ()
  "SessionFault's `daemon_state_unreadable' kind decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-session-fault
                    (agent-repl-test-wire-verbs--parse "{\"daemonStateUnreadable\":{\"cause\":\"store closed\"}}"))
                   '(:detail "" :kind (:arm :daemon-state-unreadable :value (:cause "store closed")))))))

(ert-deftest agent-repl-test-wire-verbs-session-fault-adoption-window-expired-kind ()
  "SessionFault's `adoption_window_expired' kind decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-session-fault
                    (agent-repl-test-wire-verbs--parse "{\"adoptionWindowExpired\":{\"adoptionWindow\":\"30s\"}}"))
                   '(:detail "" :kind (:arm :adoption-window-expired :value (:adoption-window "30s")))))))

(ert-deftest agent-repl-test-wire-verbs-session-fault-final-answer-unresolved-kind ()
  "SessionFault's `final_answer_unresolved' kind decodes with everything it
carries: the turn, the unit, and the `why' that IS this kind's substatus."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-session-fault
                    (agent-repl-test-wire-verbs--parse "{\"finalAnswerUnresolved\":{\"turn\":\"turn-7\",\"unit\":\"msg_01:0\",\"why\":\"answer_row_unresolved\"}}"))
                   '(:detail "" :kind (:arm :final-answer-unresolved
                                       :value (:turn "turn-7" :unit "msg_01:0" :why "answer_row_unresolved")))))))

(ert-deftest agent-repl-test-wire-verbs-session-fault-unknown-kind-is-a-breach ()
  "A SessionFault kind this codec does not know is refused, never dropped."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-session-fault
                   (agent-repl-test-wire-verbs--parse "{\"shimDead\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-session-fault-kind-arms-pinned ()
  "SessionFault's kind oneof has exactly the seventeen arms decoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_session_health.pb.go" "SessionFault")
                       #'string<)
                 (sort (list "shimStartFailed" "shimDied" "linkSevered" "resumeFailed" "bounceDied" "bounceUnknown" "classifierFailed" "shimReported" "conversationAbandoned" "sessionAbsent" "watchOpenRefused" "daemonStateUnreadable" "adoptionWindowExpired" "finalAnswerUnresolved" "vendorStartRetrying" "vendorStartRejected" "vendorStartFailed")
                       #'string<))))

;;;; ---- DaemonFault kinds (landing 4) ------------------------------------

(ert-deftest agent-repl-test-wire-verbs-daemon-fault-adoption-window-expired-kind ()
  "DaemonFault's `adoption_window_expired' kind names the workspace whose
window closed."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-daemon-fault
                    (agent-repl-test-wire-verbs--parse
                     "{\"adoptionWindowExpired\":{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"}}}"))
                   '(:detail ""
                     :kind (:arm :adoption-window-expired
                            :value (:workspace (:id "ws-1" :dir "/w/one"))))))))

(ert-deftest agent-repl-test-wire-verbs-daemon-fault-adoption-window-expired-without-workspace ()
  "The expired-window arm without its workspace is a required-field breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-daemon-fault
                   (agent-repl-test-wire-verbs--parse "{\"adoptionWindowExpired\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-daemon-fault-log-sink-poisoned-kind ()
  "DaemonFault's `log_sink_poisoned' kind decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-daemon-fault
                    (agent-repl-test-wire-verbs--parse "{\"logSinkPoisoned\":{\"sink\":\"emacs\"}}"))
                   '(:detail "" :kind (:arm :log-sink-poisoned :value (:sink "emacs")))))))

(ert-deftest agent-repl-test-wire-verbs-daemon-fault-successor-spawn-failed-kind ()
  "DaemonFault's `successor_spawn_failed' kind decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-daemon-fault
                    (agent-repl-test-wire-verbs--parse "{\"successorSpawnFailed\":{\"detail\":\"no port\"}}"))
                   '(:detail "" :kind (:arm :successor-spawn-failed :value (:detail "no port")))))))

(ert-deftest agent-repl-test-wire-verbs-daemon-fault-prompts-dir-missing-kind ()
  "DaemonFault's `prompts_dir_missing' kind decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-daemon-fault
                    (agent-repl-test-wire-verbs--parse "{\"promptsDirMissing\":{\"path\":\"/p\"}}"))
                   '(:detail "" :kind (:arm :prompts-dir-missing :value (:path "/p")))))))

(ert-deftest agent-repl-test-wire-verbs-daemon-fault-wsm-read-only-kind ()
  "DaemonFault's `wsm_read_only' kind decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-daemon-fault
                    (agent-repl-test-wire-verbs--parse "{\"wsmReadOnly\":{}}"))
                   '(:detail "" :kind (:arm :wsm-read-only :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-daemon-fault-daemon-state-unreadable-kind ()
  "DaemonFault's `daemon_state_unreadable' kind decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-daemon-fault
                    (agent-repl-test-wire-verbs--parse "{\"daemonStateUnreadable\":{\"cause\":\"state client refused\"}}"))
                   '(:detail "" :kind (:arm :daemon-state-unreadable :value (:cause "state client refused")))))))

(ert-deftest agent-repl-test-wire-verbs-daemon-fault-deploy-failed-kind ()
  "DaemonFault's `deploy_failed' kind decodes each failed step with the very
refusal the Deploy rpc answers for it."
  (agent-repl-test-wire-verbs--with-common
    (dolist (case
             '(("{\"build\":{\"step\":\"webapp\",\"detail\":\"tsc\",\"log\":\"/l\"}}"
                (:arm :build :value (:step "webapp" :detail "tsc" :log "/l")))
               ("{\"install\":{\"component\":\"DEPLOY_COMPONENT_DAEMON\",\"detail\":\"EACCES\"}}"
                (:arm :install :value (:component :daemon :detail "EACCES")))
               ("{\"restartServices\":{\"component\":\"DEPLOY_COMPONENT_STORE\",\"detail\":\"exit 78\"}}"
                (:arm :restart-services :value (:component :store :detail "exit 78")))
               ("{\"rollback\":{\"component\":\"DEPLOY_COMPONENT_DAEMON\",\"detail\":\"EROFS\"}}"
                (:arm :rollback :value (:component :daemon :detail "EROFS")))))
      (should (equal (list (car case)
                           (agent-repl-wire-decode-daemon-fault
                            (agent-repl-test-wire-verbs--parse
                             (format "{\"deployFailed\":%s}" (car case)))))
                     (list (car case)
                           (list :detail ""
                                 :kind (list :arm :deploy-failed
                                             :value (list :step (cadr case))))))))))

(ert-deftest agent-repl-test-wire-verbs-daemon-fault-deploy-failed-without-a-step-is-a-breach ()
  "A failed deploy names the step that failed; an unset step is refused."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-daemon-fault
                   (agent-repl-test-wire-verbs--parse "{\"deployFailed\":{}}"))
                  :type 'agent-repl-wire-error)))

(defun agent-repl-test-wire-verbs--daemon-fault-breach (json)
  "Return the `agent-repl-wire-error' data decoding DaemonFault JSON raises."
  (condition-case err
      (progn (agent-repl-wire-decode-daemon-fault
              (agent-repl-test-wire-verbs--parse json))
             nil)
    (agent-repl-wire-error (cdr err))))

(ert-deftest agent-repl-test-wire-verbs-daemon-fault-rollback-failed-without-a-component-is-a-breach ()
  "A failed rollback names the component it could not restore."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-test-wire-verbs--daemon-fault-breach
                    "{\"deployFailed\":{\"rollback\":{\"detail\":\"x\"}}}")
                   '("DeployRollbackFailed" "component" "required field is unset")))))

(ert-deftest agent-repl-test-wire-verbs-daemon-fault-rollback-failed-without-detail-is-a-breach ()
  "A failed rollback says why."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-test-wire-verbs--daemon-fault-breach
                    "{\"deployFailed\":{\"rollback\":{\"component\":\"DEPLOY_COMPONENT_STORE\"}}}")
                   '("DeployRollbackFailed" "detail" "required string is empty")))))

(ert-deftest agent-repl-test-wire-verbs-daemon-fault-deploy-failed-step-arms-pinned ()
  "DaemonFaultDeployFailed's step oneof has exactly the four arms decoded
here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_daemon_health.pb.go" "DaemonFaultDeployFailed")
                       #'string<)
                 (sort (list "build" "install" "restartServices" "rollback") #'string<))))

(ert-deftest agent-repl-test-wire-verbs-daemon-fault-kind-arms-pinned ()
  "DaemonFault's kind oneof has exactly the seven arms decoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_daemon_health.pb.go" "DaemonFault")
                       #'string<)
                 (sort (list "adoptionWindowExpired" "logSinkPoisoned" "successorSpawnFailed" "promptsDirMissing" "wsmReadOnly" "daemonStateUnreadable" "deployFailed")
                       #'string<))))

;;;; ---- Interrupt -------------------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-interrupt-turn-request ()
  "An InterruptRequest for the turn target names the turn arm and spells
`confirm_agents' false."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-interrupt-request
                     (list :workspace agent-repl-test-wire-verbs--ref
                           :target '(:arm :turn :value nil)
                           :confirm-agents nil)))
                   "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},\"turn\":{},\"confirmAgents\":false}"))))

(ert-deftest agent-repl-test-wire-verbs-interrupt-turn-roll-back-unanswered-request ()
  "A turn stop asking to roll an unanswered prompt back carries the presence message."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-interrupt-request
                     (list :workspace agent-repl-test-wire-verbs--ref
                           :target '(:arm :turn :value (:roll-back-unanswered t))
                           :confirm-agents nil)))
                   "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},\"turn\":{\"rollBackUnanswered\":{}},\"confirmAgents\":false}"))))

(ert-deftest agent-repl-test-wire-verbs-interrupt-confirm-agents-true ()
  "A re-sent turn stop states `confirm_agents' true."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-interrupt-request
                     (list :workspace agent-repl-test-wire-verbs--ref
                           :target '(:arm :turn :value nil)
                           :confirm-agents t)))
                   "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},\"turn\":{},\"confirmAgents\":true}"))))

(ert-deftest agent-repl-test-wire-verbs-interrupt-all-agents-request ()
  "An InterruptRequest for the fan-wide target names the allAgents arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-interrupt-request
                     (list :workspace agent-repl-test-wire-verbs--ref
                           :target '(:arm :all-agents :value nil)
                           :confirm-agents nil)))
                   "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},\"allAgents\":{},\"confirmAgents\":false}"))))

(ert-deftest agent-repl-test-wire-verbs-interrupt-request-requires-workspace ()
  "An InterruptRequest with no workspace is a required-field breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-interrupt-request
                   (list :target '(:arm :turn :value nil)))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-interrupt-request-requires-target ()
  "An InterruptRequest with no target arm is a required-field breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-interrupt-request
                   (list :workspace agent-repl-test-wire-verbs--ref))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-interrupt-detached-target-refused ()
  "The detached target has no encoder here -- Emacs holds no FeedId to build --
so authoring it is refused rather than sent malformed."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-interrupt-request
                   (list :workspace agent-repl-test-wire-verbs--ref
                         :target '(:arm :detached :value nil)))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-interrupt-merge-tab-target-refused ()
  "The merge-tab target has no encoder here -- Emacs holds no FeedId to build --
so authoring it is refused rather than sent malformed."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-interrupt-request
                   (list :workspace agent-repl-test-wire-verbs--ref
                         :target '(:arm :merge-tab :value nil)))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-interrupt-response-interrupted-turn ()
  "An interrupted turn decodes to the interrupted_turn success arm, with no
rollback refusal when none was attempted."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-interrupt-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{\"interruptedTurn\":{}}}"))
                   '(:arm :success :value (:arm :interrupted-turn :value (:not-rolled-back nil)))))))

(ert-deftest agent-repl-test-wire-verbs-interrupt-response-interrupted-turn-not-rolled-back ()
  "An interrupted turn whose rollback was refused carries the RollBackError cause."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-interrupt-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"success\":{\"interruptedTurn\":{\"notRolledBack\":{\"cause\":{\"firstPrompt\":{}}}}}}"))
                   '(:arm :success :value
                     (:arm :interrupted-turn
                      :value (:not-rolled-back (:cause (:arm :first-prompt :value nil)))))))))

(ert-deftest agent-repl-test-wire-verbs-interrupt-response-not-rolled-back-without-cause-refused ()
  "A rollback refusal naming no cause is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-interrupt-response
                   (agent-repl-test-wire-verbs--parse
                    "{\"success\":{\"interruptedTurn\":{\"notRolledBack\":{}}}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-interrupt-response-rolled-back-unanswered ()
  "A stop that rolled its unanswered prompt back carries the prompt's words."
  (agent-repl-test-wire-verbs--with-common
    (let ((decoded (agent-repl-wire-decode-interrupt-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"success\":{\"rolledBackUnanswered\":{\"prompt\":{\"content\":{\"blocks\":[{\"text\":{\"text\":\"fix it\"}}]}}}}}"))))
      (should (eq (plist-get (plist-get decoded :value) :arm) :rolled-back-unanswered))
      (should (plist-get (plist-get (plist-get decoded :value) :value) :prompt)))))

(ert-deftest agent-repl-test-wire-verbs-interrupt-response-rolled-back-unanswered-without-prompt-refused ()
  "A rolled-back answer with no prompt leaves the composer nothing to hold: a breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-interrupt-response
                   (agent-repl-test-wire-verbs--parse "{\"success\":{\"rolledBackUnanswered\":{}}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-interrupt-response-nothing-running ()
  "A stop that found the session quiet decodes to the nothing_running SUCCESS
arm, never an error."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-interrupt-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{\"nothingRunning\":{}}}"))
                   '(:arm :success :value (:arm :nothing-running :value (:rollback nil)))))))

(ert-deftest agent-repl-test-wire-verbs-interrupt-response-merge-queue-offered ()
  "A stop that raised the merge-queue question decodes to its own SUCCESS arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-interrupt-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{\"mergeQueueOffered\":{}}}"))
                   '(:arm :success :value (:arm :merge-queue-offered :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-interrupt-nothing-running-immediate-offer ()
  "An immediate rollback offer carries the plan's token."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-interrupt-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"success\":{\"nothingRunning\":{\"rollback\":{\"immediate\":{\"token\":{\"value\":\"tok-9\"}}}}}}"))
                   '(:arm :success :value
                     (:arm :nothing-running
                      :value (:rollback (:arm :immediate :value (:token (:value "tok-9"))))))))))

(ert-deftest agent-repl-test-wire-verbs-interrupt-nothing-running-choose-offer ()
  "A choose offer is the empty arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-interrupt-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"success\":{\"nothingRunning\":{\"rollback\":{\"choose\":{}}}}}"))
                   '(:arm :success :value
                     (:arm :nothing-running :value (:rollback (:arm :choose :value nil))))))))

(ert-deftest agent-repl-test-wire-verbs-interrupt-nothing-running-immediate-without-token-refused ()
  "An immediate offer with no token cannot be performed: a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-interrupt-response
                   (agent-repl-test-wire-verbs--parse
                    "{\"success\":{\"nothingRunning\":{\"rollback\":{\"immediate\":{}}}}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-interrupt-rollback-offer-arms-pinned ()
  "InterruptNothingRunningRollback's offer oneof has exactly the arms decoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_interrupt.pb.go" "InterruptNothingRunningRollback")
                       #'string<)
                 '("choose" "immediate"))))

(ert-deftest agent-repl-test-wire-verbs-interrupt-response-interrupted-detached ()
  "The interrupted_detached arm carries the count the stop reached."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-interrupt-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{\"interruptedDetached\":{\"count\":3}}}"))
                   '(:arm :success :value (:arm :interrupted-detached :value (:count 3)))))))

(ert-deftest agent-repl-test-wire-verbs-interrupt-success-unset-outcome ()
  "A success with no outcome arm set is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-interrupt-response
                   (agent-repl-test-wire-verbs--parse "{\"success\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-interrupt-error-confirm-required ()
  "The confirm_required challenge decodes with its live-agent count."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-interrupt-response
                    (agent-repl-test-wire-verbs--parse "{\"error\":{\"confirmRequired\":{\"liveAgentCount\":2}}}"))
                   '(:arm :error :value (:cause (:arm :confirm-required :value (:live-agent-count 2))))))))

(ert-deftest agent-repl-test-wire-verbs-interrupt-error-no-session ()
  "A workspace with no session decodes to the no_session refusal arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-interrupt-response
                    (agent-repl-test-wire-verbs--parse "{\"error\":{\"noSession\":{}}}"))
                   '(:arm :error :value (:cause (:arm :no-session :value nil)))))))

(ert-deftest agent-repl-test-wire-verbs-interrupt-error-shim-refused ()
  "A relayed shim refusal decodes with the shim's own detail."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-interrupt-response
                    (agent-repl-test-wire-verbs--parse "{\"error\":{\"shimRefused\":{\"detail\":\"already idle\"}}}"))
                   '(:arm :error :value (:cause (:arm :shim-refused :value (:detail "already idle"))))))))

(ert-deftest agent-repl-test-wire-verbs-interrupt-error-unknown-workspace ()
  "An unknown workspace decodes to the unknown_workspace refusal arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-interrupt-response
                    (agent-repl-test-wire-verbs--parse "{\"error\":{\"unknownWorkspace\":{}}}"))
                   '(:arm :error :value (:cause (:arm :unknown-workspace :value nil)))))))

(ert-deftest agent-repl-test-wire-verbs-interrupt-error-ref-mismatch ()
  "A ref mismatch decodes with the registry's own dir."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-interrupt-response
                    (agent-repl-test-wire-verbs--parse "{\"error\":{\"workspaceRefMismatch\":{\"registryDir\":\"/w/real\"}}}"))
                   '(:arm :error :value (:cause (:arm :workspace-ref-mismatch :value (:registry-dir "/w/real"))))))))

(ert-deftest agent-repl-test-wire-verbs-interrupt-error-transferring-away ()
  "A handover decodes to transferring_away with the successor address."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-interrupt-response
                    (agent-repl-test-wire-verbs--parse "{\"error\":{\"transferringAway\":{\"address\":\"127.0.0.1:9\"}}}"))
                   '(:arm :error :value (:cause (:arm :transferring-away :value (:address "127.0.0.1:9"))))))))

(ert-deftest agent-repl-test-wire-verbs-interrupt-error-unset-kind ()
  "An error with no kind arm set is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-interrupt-response
                   (agent-repl-test-wire-verbs--parse "{\"error\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-interrupt-error-unknown-kind ()
  "A future interrupt-refusal arm arrives as an unknown key and is loud."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-interrupt-response
                   (agent-repl-test-wire-verbs--parse "{\"error\":{\"somethingNew\":{}}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-interrupt-target-arms-pinned ()
  "InterruptRequest's target oneof has exactly the arms the proto declares;
Emacs encodes two of them and refuses the FeedId-bearing `detached' and
`merge_tab'."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_interrupt.pb.go" "InterruptRequest")
                       #'string<)
                 (sort (list "turn" "detached" "allAgents" "mergeTab") #'string<))))

(ert-deftest agent-repl-test-wire-verbs-interrupt-success-arms-pinned ()
  "InterruptSuccess's outcome oneof has exactly the five arms decoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_interrupt.pb.go" "InterruptSuccess")
                       #'string<)
                 (sort (list "interruptedTurn" "interruptedDetached" "nothingRunning"
                             "mergeQueueOffered" "rolledBackUnanswered")
                       #'string<))))

(ert-deftest agent-repl-test-wire-verbs-interrupt-error-arms-pinned ()
  "InterruptError's kind oneof has exactly the arms decoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_interrupt.pb.go" "InterruptError")
                       #'string<)
                 (sort (list "confirmRequired" "unknownWorkspace" "workspaceRefMismatch"
                             "transferringAway" "notYetAdopted" "notDetachedWork"
                             "noSession" "shimRefused" "notMergeTab")
                       #'string<))))

(ert-deftest agent-repl-test-wire-verbs-interrupt-error-not-merge-tab-arm ()
  "InterruptError's `not_merge_tab' arm decodes as its empty arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-interrupt-error
                    (agent-repl-test-wire-verbs--parse "{\"notMergeTab\":{}}"))
                   '(:cause (:arm :not-merge-tab :value nil))))))

;;;; ---- SelectFeedRowRequest -------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-direction-older ()
  "The OLDER direction encodes to its protojson enum name."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-encode-select-feed-row-direction :older)
                   "SELECT_FEED_ROW_DIRECTION_OLDER"))))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-direction-newer ()
  "The NEWER direction encodes to its protojson enum name."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-encode-select-feed-row-direction :newer)
                   "SELECT_FEED_ROW_DIRECTION_NEWER"))))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-direction-refuses-unspecified ()
  "UNSPECIFIED has no elisp spelling, so an unknown keyword is refused."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-select-feed-row-direction :unspecified)
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-direction-vocabulary-pinned ()
  "The direction vocabulary is every generated enum name except UNSPECIFIED."
  (should (equal
           (sort (mapcar #'cdr agent-repl-wire-select-feed-row-directions) #'string<)
           (sort (remove "SELECT_FEED_ROW_DIRECTION_UNSPECIFIED"
                         (agent-repl-test--generated-enum-names
                          "agentrepl/v1/endpoint_select_feed_row.pb.go"
                          "SELECT_FEED_ROW_DIRECTION_"))
                 #'string<))))

(defconst agent-repl-test-wire-verbs--composer-image
  '(:location (:arm :path :value (:path "/w/clip.png")) :media-type "image/png")
  "An ImageBlock the composer holds.")

(ert-deftest agent-repl-test-wire-verbs-open-feed-image-composer-shape ()
  "An open from the composer carries the echoed ref and the composer's ImageBlock."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-open-feed-image-request
                     (list :workspace agent-repl-test-wire-verbs--ref
                           :image (list :arm :composer
                                        :value (list :image agent-repl-test-wire-verbs--composer-image)))))
                   "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},\"composer\":{\"image\":{\"path\":{\"path\":\"/w/clip.png\"},\"mediaType\":\"image/png\"}}}"))))

(ert-deftest agent-repl-test-wire-verbs-open-feed-image-missing-image-refused ()
  "The image is REQUIRED, so an open without one never reaches the wire."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-open-feed-image-request
                   (list :workspace agent-repl-test-wire-verbs--ref))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-open-feed-image-missing-workspace-refused ()
  "The workspace is REQUIRED, so an open without one never reaches the wire."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-open-feed-image-request
                   (list :image (list :arm :composer
                                      :value (list :image agent-repl-test-wire-verbs--composer-image))))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-open-feed-image-drawn-arm-has-no-spelling ()
  "The webapp's `drawn' arm is never sent from Emacs, so it is refused."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-open-feed-image-request
                   (list :workspace agent-repl-test-wire-verbs--ref
                         :image '(:arm :drawn :value (:image (:src "/x" :alt "")))))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-open-feed-image-success-decodes ()
  "The empty success decodes as the success arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (plist-get (agent-repl-wire-decode-open-feed-image-response
                               (json-parse-string "{\"success\":{}}" :object-type 'alist))
                              :arm)
                   :success))))

(ert-deftest agent-repl-test-wire-verbs-open-feed-image-refusals-decode ()
  "Each refusal arm decodes with its own fields."
  (agent-repl-test-wire-verbs--with-common
    (dolist (case '(("{\"error\":{\"unservable\":{\"detail\":\"not absolute\"}}}"
                     :unservable (:detail "not absolute"))
                    ("{\"error\":{\"notDrawn\":{\"src\":\"/x\"}}}"
                     :not-drawn (:src "/x"))
                    ("{\"error\":{\"notYetAdopted\":{}}}"
                     :not-yet-adopted nil)))
      (let ((cause (plist-get (plist-get (agent-repl-wire-decode-open-feed-image-response
                                          (json-parse-string (nth 0 case) :object-type 'alist))
                                         :value)
                              :cause)))
        (should (equal (list (plist-get cause :arm) (plist-get cause :value))
                       (list (nth 1 case) (nth 2 case))))))))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-turn-boundary-step-shape ()
  "A turn-boundary step carries the echoed ref and the step's direction."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-select-feed-row-request
                     (list :workspace agent-repl-test-wire-verbs--ref
                           :move '(:arm :turn-boundary :value (:direction :older)))))
                   "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},\"turnBoundary\":{\"direction\":\"SELECT_FEED_ROW_DIRECTION_OLDER\"}}"))))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-retired-step-arms-have-no-spelling ()
  "The retired `response' and `prompt' step arms are refused before the wire."
  (agent-repl-test-wire-verbs--with-common
    (dolist (arm '(:response :prompt))
      (should-error (agent-repl-wire-encode-select-feed-row-request
                     (list :workspace agent-repl-test-wire-verbs--ref
                           :move (list :arm arm :value '(:direction :older))))
                    :type 'agent-repl-wire-error))))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-clear-shape ()
  "A clear carries the empty clear arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-select-feed-row-request
                     (list :workspace agent-repl-test-wire-verbs--ref
                           :move '(:arm :clear :value nil))))
                   "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},\"clear\":{}}"))))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-missing-workspace-refused ()
  "The workspace is REQUIRED, so a move without one never reaches the wire."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-select-feed-row-request
                   '(:move (:arm :clear :value nil)))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-missing-move-refused ()
  "The move is REQUIRED, so a request without one never reaches the wire."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-select-feed-row-request
                   (list :workspace agent-repl-test-wire-verbs--ref))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-step-missing-direction-refused ()
  "A step without a direction never reaches the wire."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-select-feed-row-request
                   (list :workspace agent-repl-test-wire-verbs--ref
                         :move '(:arm :turn-boundary :value nil)))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-left-view-has-no-spelling ()
  "The webapp's `left_view' move is never sent from Emacs, so it is refused."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-select-feed-row-request
                   (list :workspace agent-repl-test-wire-verbs--ref
                         :move '(:arm :left-view :value (:row (:value "r1")))))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-move-arms-pinned ()
  "SelectFeedRowRequest's move oneof has exactly the arms the schema declares.
Emacs spells two of them; `leftView', `bubble' (a click), `answer' (a
click on a linked label), `entry' (a click on a prompt's entry) and
`branch' (a click on a response's branch) are the webapp's alone."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_select_feed_row.pb.go"
                        "SelectFeedRowRequest")
                       #'string<)
                 '("answer" "branch" "bubble" "clear" "entry" "leftView" "turnBoundary"))))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-answer-and-entry-have-no-spelling ()
  "The webapp's label and entry clicks are never sent from Emacs, so each is refused."
  (agent-repl-test-wire-verbs--with-common
    (dolist (arm '(:answer :entry))
      (should-error (agent-repl-wire-encode-select-feed-row-request
                     (list :workspace agent-repl-test-wire-verbs--ref
                           :move (list :arm arm :value '(:row (:value "r1") :label "3.1"))))
                    :type 'agent-repl-wire-error))))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-bubble-has-no-spelling ()
  "The webapp's click move is never sent from Emacs, so it is refused."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-select-feed-row-request
                   (list :workspace agent-repl-test-wire-verbs--ref
                         :move '(:arm :bubble :value (:row (:value "r1")))))
                  :type 'agent-repl-wire-error)))

;;;; ---- SelectFeedRowResponse ------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-success-selected-response ()
  "A step that lands on a final response acks the selection naming it."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-select-feed-row-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"success\":{\"selected\":{\"selection\":{\"response\":{\"row\":{\"value\":\"feed-9\"},\"viewport\":{\"center\":{}}}}}}}"))
                   '(:arm :success
                     :value (:outcome (:arm :selected
                                       :value (:selection (:arm :response
                                                           :value (:row (:value "feed-9")
                                                                   :viewport (:viewport (:arm :center :value nil))))))))))))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-success-selected-prompt ()
  "A step that lands on a prompt acks the selection naming it."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (plist-get
                    (plist-get
                     (plist-get
                      (plist-get
                       (plist-get
                        (agent-repl-wire-decode-select-feed-row-response
                         (agent-repl-test-wire-verbs--parse
                          "{\"success\":{\"selected\":{\"selection\":{\"prompt\":{\"row\":{\"value\":\"p-1\"},\"viewport\":{\"center\":{}}}}}}}"))
                        :value)
                       :outcome)
                      :value)
                     :selection)
                    :arm)
                   :prompt))))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-success-nothing-selectable ()
  "A step that finds nothing of its kind decodes as an empty arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-select-feed-row-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"success\":{\"nothingSelectable\":{}}}"))
                   '(:arm :success :value (:outcome (:arm :nothing-selectable :value nil)))))))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-success-none ()
  "A clear decodes as the empty `none' outcome."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-select-feed-row-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{\"none\":{}}}"))
                   '(:arm :success :value (:outcome (:arm :none :value nil)))))))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-success-unset-outcome-refused ()
  "THE ARM IS THE OUTCOME: a success naming none is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-select-feed-row-response
                   (agent-repl-test-wire-verbs--parse "{\"success\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-success-refuses-unknown-field ()
  "An unknown field on the success is a schema the consumer does not hold."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-select-feed-row-response
                   (agent-repl-test-wire-verbs--parse "{\"success\":{\"moved\":{}}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-selected-without-selection-refused ()
  "A selected outcome naming no selection is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-select-feed-row-response
                   (agent-repl-test-wire-verbs--parse "{\"success\":{\"selected\":{}}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-selected-row-without-id-refused ()
  "A selected row naming no row is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-select-feed-row-response
                   (agent-repl-test-wire-verbs--parse
                    "{\"success\":{\"selected\":{\"selection\":{\"response\":{}}}}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-success-arms-pinned ()
  "SelectFeedRowSuccess's outcome oneof has exactly the arms decoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_select_feed_row.pb.go"
                        "SelectFeedRowSuccess")
                       #'string<)
                 '("none" "nothingSelectable" "selected"))))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-error-unknown-workspace ()
  "The unknown-workspace refusal decodes as an empty arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-select-feed-row-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"error\":{\"unknownWorkspace\":{}}}"))
                   '(:arm :error :value (:cause (:arm :unknown-workspace :value nil)))))))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-error-workspace-ref-mismatch ()
  "The ref-mismatch refusal carries the registry's dir for this id."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-select-feed-row-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"error\":{\"workspaceRefMismatch\":{\"registryDir\":\"/w/real\"}}}"))
                   '(:arm :error
                     :value (:cause (:arm :workspace-ref-mismatch
                                     :value (:registry-dir "/w/real"))))))))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-error-transferring-away ()
  "The transferring-away refusal carries the successor's address."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-select-feed-row-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"error\":{\"transferringAway\":{\"address\":\"127.0.0.1:9\"}}}"))
                   '(:arm :error
                     :value (:cause (:arm :transferring-away
                                     :value (:address "127.0.0.1:9"))))))))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-error-not-yet-adopted ()
  "The not-yet-adopted refusal decodes as an empty arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-select-feed-row-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"error\":{\"notYetAdopted\":{}}}"))
                   '(:arm :error :value (:cause (:arm :not-yet-adopted :value nil)))))))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-error-cause-arms-pinned ()
  "SelectFeedRowError's cause oneof has exactly the arms this codec decodes."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_select_feed_row.pb.go"
                        "SelectFeedRowError")
                       #'string<)
                 '("notSelectable" "notYetAdopted" "transferringAway"
                   "unknownWorkspace" "workspaceRefMismatch"))))

(ert-deftest agent-repl-test-wire-verbs-select-feed-row-error-not-selectable ()
  "The not-selectable refusal decodes with the row it echoes."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-select-feed-row-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"error\":{\"notSelectable\":{\"row\":{\"value\":\"r1\"}}}}"))
                   '(:arm :error :value (:cause (:arm :not-selectable :value (:row (:value "r1")))))))))

;;;; ---- frontend.v1.FeedSelection --------------------------------------

(ert-deftest agent-repl-test-wire-verbs-feed-selection-none-return-to-tail ()
  "Nothing selected, back to the tail, decodes to its viewport arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-feed-selection
                    (agent-repl-test-wire-verbs--parse "{\"none\":{\"returnToTail\":{}}}"))
                   '(:arm :none :value (:viewport (:arm :return-to-tail :value nil)))))))

(ert-deftest agent-repl-test-wire-verbs-feed-selection-none-stay ()
  "Nothing selected, staying put, decodes to its viewport arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-feed-selection
                    (agent-repl-test-wire-verbs--parse "{\"none\":{\"stay\":{}}}"))
                   '(:arm :none :value (:viewport (:arm :stay :value nil)))))))

(ert-deftest agent-repl-test-wire-verbs-feed-selection-none-without-viewport-refused ()
  "Nothing selected with no viewport arm is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-feed-selection
                   (agent-repl-test-wire-verbs--parse "{\"none\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-feed-selection-unset-refused ()
  "A selection with no arm set is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-feed-selection
                   (agent-repl-test-wire-verbs--parse "{}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-feed-selection-arms-pinned ()
  "FeedSelection's selection oneof has exactly the arms decoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "frontend/v1/feed.pb.go" "FeedSelection")
                       #'string<)
                 '("bubble" "none" "prompt" "response"))))

(ert-deftest agent-repl-test-wire-verbs-feed-selection-bubble-decodes-its-row ()
  "A selected bubble decodes to the `:bubble' arm with its row and viewport."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-feed-selection
                    (agent-repl-test-wire-verbs--parse
                     "{\"bubble\":{\"row\":{\"value\":\"r1\"},\"viewport\":{\"hold\":{}}}}"))
                   '(:arm :bubble :value (:row (:value "r1")
                                          :viewport (:viewport (:arm :hold :value nil))))))))

(ert-deftest agent-repl-test-wire-verbs-feed-selection-response-decodes-center ()
  "A response a step selected decodes its `:center' viewport."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-feed-selection
                    (agent-repl-test-wire-verbs--parse
                     "{\"response\":{\"row\":{\"value\":\"r1\"},\"viewport\":{\"center\":{}}}}"))
                   '(:arm :response :value (:row (:value "r1")
                                            :viewport (:viewport (:arm :center :value nil))))))))

(ert-deftest agent-repl-test-wire-verbs-feed-selection-row-without-viewport-refused ()
  "A selected row with no viewport is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-feed-selection
                   (agent-repl-test-wire-verbs--parse "{\"prompt\":{\"row\":{\"value\":\"r1\"}}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-feed-selection-row-viewport-unset-refused ()
  "A row viewport with no arm set is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-feed-selection
                   (agent-repl-test-wire-verbs--parse
                    "{\"prompt\":{\"row\":{\"value\":\"r1\"},\"viewport\":{}}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-feed-selection-none-hold ()
  "Nothing selected after a click on the selected bubble decodes to `:hold'."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-feed-selection
                    (agent-repl-test-wire-verbs--parse "{\"none\":{\"hold\":{}}}"))
                   '(:arm :none :value (:viewport (:arm :hold :value nil)))))))

(ert-deftest agent-repl-test-wire-verbs-feed-selection-none-arms-pinned ()
  "FeedSelectionNone's viewport oneof has exactly the arms decoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "frontend/v1/feed.pb.go" "FeedSelectionNone")
                       #'string<)
                 '("hold" "returnToTail" "stay"))))

(ert-deftest agent-repl-test-wire-verbs-feed-selection-row-viewport-arms-pinned ()
  "FeedSelectionRowViewport's viewport oneof has exactly the arms decoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "frontend/v1/feed.pb.go" "FeedSelectionRowViewport")
                       #'string<)
                 '("center" "hold" "reveal" "revealNode"))))

(ert-deftest agent-repl-test-wire-verbs-feed-selection-row-viewport-reveal-names-its-entry ()
  "A selected prompt a linked label revealed decodes `:reveal' with its entry label."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-feed-selection-row-viewport
                    (agent-repl-test-wire-verbs--parse "{\"reveal\":{\"entryLabel\":\"3.1\"}}"))
                   '(:viewport (:arm :reveal :value (:entry-label "3.1")))))))

;;;; ---- PlanRollback -----------------------------------------------------

(defun agent-repl-test-wire-verbs--plan-response (plan-json)
  "Decode a PlanRollback success carrying PLAN-JSON as its plan."
  (agent-repl-wire-decode-plan-rollback-response
   (agent-repl-test-wire-verbs--parse
    (concat "{\"success\":{\"plan\":" plan-json "}}"))))

(defun agent-repl-test-wire-verbs--plan (plan-json)
  "Return the decoded plan plist out of PLAN-JSON."
  (plist-get (plist-get (plist-get (agent-repl-test-wire-verbs--plan-response plan-json)
                                   :value)
                        :outcome)
             :value))

(defconst agent-repl-test-wire-verbs--minimal-plan
  "{\"token\":{\"value\":\"tok-1\"},\"target\":{\"latest\":{},\"excerpt\":\"fix it\",\"promptsDropped\":1},\"files\":{\"kept\":{}}}"
  "A plan with nothing optional set.")

(ert-deftest agent-repl-test-wire-verbs-plan-rollback-keep-files-request-shape ()
  "Keeping files rides the empty `keepFiles' arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-plan-rollback-request
                     (list :workspace agent-repl-test-wire-verbs--ref
                           :files '(:arm :keep-files :value nil))))
                   "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},\"keepFiles\":{}}"))))

(ert-deftest agent-repl-test-wire-verbs-plan-rollback-restore-files-request-shape ()
  "Restoring files rides the empty `restoreFiles' arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-plan-rollback-request
                     (list :workspace agent-repl-test-wire-verbs--ref
                           :files '(:arm :restore-files :value nil))))
                   "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},\"restoreFiles\":{}}"))))

(ert-deftest agent-repl-test-wire-verbs-plan-rollback-named-prompt-request-shape ()
  "A named prompt rides `prompt', echoed as ListRollbackPrompts served it."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-plan-rollback-request
                     (list :workspace agent-repl-test-wire-verbs--ref
                           :files '(:arm :keep-files :value nil)
                           :prompt '(:value "row|p1"))))
                   "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},\"keepFiles\":{},\"prompt\":{\"value\":\"row|p1\"}}"))))

(ert-deftest agent-repl-test-wire-verbs-plan-rollback-named-target ()
  "A named target decodes to `:named'."
  (agent-repl-test-wire-verbs--with-common
    (should (eq (plist-get (plist-get (agent-repl-test-wire-verbs--plan
                                       "{\"token\":{\"value\":\"t\"},\"target\":{\"named\":{},\"excerpt\":\"x\",\"promptsDropped\":2},\"files\":{\"kept\":{}}}")
                                      :target)
                           :chosen)
                :named))))

(ert-deftest agent-repl-test-wire-verbs-plan-rollback-target-arms-pinned ()
  "RollbackPlanTarget's chosen oneof has exactly the arms decoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_plan_rollback.pb.go" "RollbackPlanTarget")
                       #'string<)
                 '("latest" "named" "selected"))))

(ert-deftest agent-repl-test-wire-verbs-plan-rollback-missing-files-refused ()
  "The files choice is REQUIRED."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-plan-rollback-request
                   (list :workspace agent-repl-test-wire-verbs--ref))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-plan-rollback-files-arms-pinned ()
  "PlanRollbackRequest's files oneof has exactly the arms encoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_plan_rollback.pb.go" "PlanRollbackRequest")
                       #'string<)
                 '("keepFiles" "restoreFiles"))))

(ert-deftest agent-repl-test-wire-verbs-plan-rollback-nothing-to-roll-back ()
  "Nothing reachable decodes as the empty outcome arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-plan-rollback-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{\"nothingToRollBack\":{}}}"))
                   '(:arm :success :value (:outcome (:arm :nothing-to-roll-back :value nil)))))))

(ert-deftest agent-repl-test-wire-verbs-plan-rollback-minimal-plan ()
  "A plan with nothing optional decodes every field, the optionals nil."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-test-wire-verbs--plan agent-repl-test-wire-verbs--minimal-plan)
                   '(:token (:value "tok-1")
                     :target (:chosen :latest :excerpt "fix it" :prompts-dropped 1)
                     :files (:arm :kept :value nil)
                     :interrupt nil
                     :drop-queued nil)))))

(ert-deftest agent-repl-test-wire-verbs-plan-rollback-selected-target ()
  "A selected target decodes to `:selected'."
  (agent-repl-test-wire-verbs--with-common
    (should (eq (plist-get (plist-get (agent-repl-test-wire-verbs--plan
                                       "{\"token\":{\"value\":\"t\"},\"target\":{\"selected\":{},\"excerpt\":\"x\",\"promptsDropped\":3},\"files\":{\"kept\":{}}}")
                                      :target)
                           :chosen)
                :selected))))

(ert-deftest agent-repl-test-wire-verbs-plan-rollback-interrupt-presence-is-t ()
  "The empty `interrupt' message decodes to t when present."
  (agent-repl-test-wire-verbs--with-common
    (should (eq (plist-get (agent-repl-test-wire-verbs--plan
                            "{\"token\":{\"value\":\"t\"},\"target\":{\"latest\":{}},\"files\":{\"kept\":{}},\"interrupt\":{}}")
                           :interrupt)
                t))))

(ert-deftest agent-repl-test-wire-verbs-plan-rollback-drop-queued-count ()
  "Queued prompts to drop carry their count."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (plist-get (agent-repl-test-wire-verbs--plan
                               "{\"token\":{\"value\":\"t\"},\"target\":{\"latest\":{}},\"files\":{\"kept\":{}},\"dropQueued\":{\"prompts\":2}}")
                              :drop-queued)
                   '(:prompts 2)))))

(ert-deftest agent-repl-test-wire-verbs-plan-rollback-restored-with-cancel-detached ()
  "Restoring files carries the detached work it stops."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (plist-get (agent-repl-test-wire-verbs--plan
                               "{\"token\":{\"value\":\"t\"},\"target\":{\"latest\":{}},\"files\":{\"restored\":{\"cancelDetached\":{\"items\":4}}}}")
                              :files)
                   '(:arm :restored :value (:cancel-detached (:items 4)))))))

(ert-deftest agent-repl-test-wire-verbs-plan-rollback-restored-without-cancel-detached ()
  "Restoring files with no detached work leaves cancel-detached nil."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (plist-get (agent-repl-test-wire-verbs--plan
                               "{\"token\":{\"value\":\"t\"},\"target\":{\"latest\":{}},\"files\":{\"restored\":{}}}")
                              :files)
                   '(:arm :restored :value (:cancel-detached nil))))))

(ert-deftest agent-repl-test-wire-verbs-plan-rollback-missing-token-refused ()
  "A plan without a token cannot be confirmed: a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-test-wire-verbs--plan
                   "{\"target\":{\"latest\":{}},\"files\":{\"kept\":{}}}")
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-plan-rollback-empty-token-refused ()
  "An empty token value is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-test-wire-verbs--plan
                   "{\"token\":{},\"target\":{\"latest\":{}},\"files\":{\"kept\":{}}}")
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-plan-rollback-target-unset-chosen-refused ()
  "A target that says neither selected nor latest is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-test-wire-verbs--plan
                   "{\"token\":{\"value\":\"t\"},\"target\":{\"excerpt\":\"x\"},\"files\":{\"kept\":{}}}")
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-plan-rollback-error-arms ()
  "Every PlanRollback refusal arm decodes to its keyword and payload."
  (agent-repl-test-wire-verbs--with-common
    (dolist (case '(("{\"unknownWorkspace\":{}}" (:arm :unknown-workspace :value nil))
                    ("{\"workspaceRefMismatch\":{\"registryDir\":\"/w/real\"}}"
                     (:arm :workspace-ref-mismatch :value (:registry-dir "/w/real")))
                    ("{\"transferringAway\":{\"address\":\"127.0.0.1:9\"}}"
                     (:arm :transferring-away :value (:address "127.0.0.1:9")))
                    ("{\"notYetAdopted\":{}}" (:arm :not-yet-adopted :value nil))
                    ("{\"promptNotReachable\":{\"prompt\":{\"value\":\"row|p1\"}}}"
                     (:arm :prompt-not-reachable :value (:prompt (:value "row|p1"))))))
      (should (equal (agent-repl-wire-decode-plan-rollback-response
                      (agent-repl-test-wire-verbs--parse
                       (concat "{\"error\":" (car case) "}")))
                     (list :arm :error :value (list :cause (cadr case))))))))

(ert-deftest agent-repl-test-wire-verbs-plan-rollback-error-arms-pinned ()
  "PlanRollbackError's cause oneof has exactly the arms decoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_plan_rollback.pb.go" "PlanRollbackError")
                       #'string<)
                 '("notYetAdopted" "promptNotReachable" "transferringAway" "unknownWorkspace"
                   "workspaceRefMismatch"))))

(ert-deftest agent-repl-test-wire-verbs-plan-rollback-outcome-arms-pinned ()
  "PlanRollbackSuccess's outcome oneof has exactly the arms decoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_plan_rollback.pb.go" "PlanRollbackSuccess")
                       #'string<)
                 '("nothingToRollBack" "plan"))))

;;;; ---- ListRollbackPrompts ----------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-list-rollback-prompts-request-shape ()
  "The request is the workspace ref alone."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-list-rollback-prompts-request
                     (list :workspace agent-repl-test-wire-verbs--ref)))
                   "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"}}"))))

(ert-deftest agent-repl-test-wire-verbs-list-rollback-prompts-missing-workspace-refused ()
  "The workspace ref is REQUIRED."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-list-rollback-prompts-request nil)
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-list-rollback-prompts-entries ()
  "Entries decode in order, each figure nil when unset and a number when set."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-list-rollback-prompts-response
                    (agent-repl-test-wire-verbs--parse
                     (concat "{\"success\":{\"prompts\":["
                             "{\"prompt\":{\"value\":\"p2\"},\"excerpt\":\"newer\",\"sentAtMs\":\"1700000000000\",\"contextTokens\":\"42000\"},"
                             "{\"prompt\":{\"value\":\"p1\"},\"excerpt\":\"older\"}]}}")))
                   '(:arm :success :value
                     (:prompts ((:prompt (:value "p2") :excerpt "newer"
                                 :sent-at-ms 1700000000000 :context-tokens 42000)
                                (:prompt (:value "p1") :excerpt "older"
                                 :sent-at-ms nil :context-tokens nil))))))))

(ert-deftest agent-repl-test-wire-verbs-list-rollback-prompts-empty ()
  "No reachable prompt is an empty success."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-list-rollback-prompts-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{}}"))
                   '(:arm :success :value (:prompts nil))))))

(ert-deftest agent-repl-test-wire-verbs-list-rollback-prompts-entry-without-prompt-refused ()
  "An entry with no row cannot be picked: a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-list-rollback-prompts-response
                   (agent-repl-test-wire-verbs--parse
                    "{\"success\":{\"prompts\":[{\"excerpt\":\"x\"}]}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-list-rollback-prompts-error-arms ()
  "Every ListRollbackPrompts refusal arm decodes to its keyword and payload."
  (agent-repl-test-wire-verbs--with-common
    (dolist (case '(("{\"unknownWorkspace\":{}}" (:arm :unknown-workspace :value nil))
                    ("{\"workspaceRefMismatch\":{\"registryDir\":\"/w/real\"}}"
                     (:arm :workspace-ref-mismatch :value (:registry-dir "/w/real")))
                    ("{\"transferringAway\":{\"address\":\"127.0.0.1:9\"}}"
                     (:arm :transferring-away :value (:address "127.0.0.1:9")))
                    ("{\"notYetAdopted\":{}}" (:arm :not-yet-adopted :value nil))))
      (should (equal (agent-repl-wire-decode-list-rollback-prompts-response
                      (agent-repl-test-wire-verbs--parse
                       (concat "{\"error\":" (car case) "}")))
                     (list :arm :error :value (list :cause (cadr case))))))))

(ert-deftest agent-repl-test-wire-verbs-list-rollback-prompts-error-arms-pinned ()
  "ListRollbackPromptsError's cause oneof has exactly the arms decoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_list_rollback_prompts.pb.go" "ListRollbackPromptsError")
                       #'string<)
                 '("notYetAdopted" "transferringAway" "unknownWorkspace" "workspaceRefMismatch"))))

;;;; ---- RollBack ---------------------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-roll-back-request-echoes-the-token ()
  "The plan's token goes back verbatim beside the workspace ref."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-roll-back-request
                     (list :workspace agent-repl-test-wire-verbs--ref
                           :token (plist-get (agent-repl-test-wire-verbs--plan
                                              agent-repl-test-wire-verbs--minimal-plan)
                                             :token))))
                   "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},\"token\":{\"value\":\"tok-1\"}}"))))

(defun agent-repl-test-wire-verbs--roll-back-with (edit)
  "Encode a RollBack request resending EDIT, as JSON."
  (json-serialize
   (agent-repl-wire-encode-roll-back-request
    (list :workspace agent-repl-test-wire-verbs--ref
          :token (plist-get (agent-repl-test-wire-verbs--plan agent-repl-test-wire-verbs--minimal-plan)
                            :token)
          :resend (list :idempotency-key "k-1" :origin :user-sent :edit edit)))))

(ert-deftest agent-repl-test-wire-verbs-roll-back-resend-whole-shape ()
  "A whole resend carries the composer's prompt."
  (agent-repl-test-wire-verbs--with-common
    (should (string-match-p
             (regexp-quote "\"whole\":{\"said\":")
             (agent-repl-test-wire-verbs--roll-back-with
              (list :arm :whole :value (list :said agent-repl-test-wire-verbs--edit-said)))))))

(ert-deftest agent-repl-test-wire-verbs-roll-back-without-a-resend-sends-none ()
  "A plain rollback carries no resend field."
  (agent-repl-test-wire-verbs--with-common
    (should-not (string-match-p
                 "resend"
                 (json-serialize
                  (agent-repl-wire-encode-roll-back-request
                   (list :workspace agent-repl-test-wire-verbs--ref
                         :token (plist-get (agent-repl-test-wire-verbs--plan
                                            agent-repl-test-wire-verbs--minimal-plan)
                                           :token))))))))

(ert-deftest agent-repl-test-wire-verbs-roll-back-resend-unknown-edit-refused ()
  "An edit arm the schema does not declare is refused before anything is sent."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-test-wire-verbs--roll-back-with '(:arm :sideways :value nil))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-roll-back-success-carries-the-resend ()
  "A success after a resend carries the resend's submission, decoded as SubmitPrompt's."
  (agent-repl-test-wire-verbs--with-common
    (let ((value (plist-get (agent-repl-wire-decode-roll-back-response
                             (agent-repl-test-wire-verbs--parse
                              "{\"success\":{\"prompt\":{\"content\":{\"blocks\":[]}},\"resend\":{\"error\":{\"noSession\":{}}}}}"))
                            :value)))
      (should (equal (plist-get (plist-get value :resend) :arm) :error)))))

(ert-deftest agent-repl-test-wire-verbs-roll-back-edit-arms-pinned ()
  "RollBackResend's edit oneof has exactly the arms encoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_roll_back.pb.go" "RollBackResend")
                       #'string<)
                 '("whole"))))

(ert-deftest agent-repl-test-wire-verbs-roll-back-missing-token-refused ()
  "The token is REQUIRED."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-roll-back-request
                   (list :workspace agent-repl-test-wire-verbs--ref))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-roll-back-empty-token-refused ()
  "An empty token is never sent."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-roll-back-request
                   (list :workspace agent-repl-test-wire-verbs--ref :token '(:value "")))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-roll-back-success-carries-the-prompt ()
  "The rolled-back prompt decodes through the UserSaid codec."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-roll-back-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"success\":{\"prompt\":{\"content\":{\"blocks\":[{\"text\":{\"text\":\"hi\"}},{\"image\":{\"path\":{\"path\":\"/i.png\"},\"mediaType\":\"image/png\"}}]}}}}"))
                   '(:arm :success
                     :value (:prompt (:content (:blocks ((:arm :text :value (:text "hi"))
                                                         (:arm :image
                                                          :value (:location (:arm :path :value (:path "/i.png"))
                                                                  :media-type "image/png")))))
                             :files-restored nil :resend nil))))))

(ert-deftest agent-repl-test-wire-verbs-roll-back-success-files-restored ()
  "A restore carries how many files it changed back."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (plist-get (plist-get (agent-repl-wire-decode-roll-back-response
                                          (agent-repl-test-wire-verbs--parse
                                           "{\"success\":{\"prompt\":{\"content\":{}},\"filesRestored\":{\"files\":3}}}"))
                                         :value)
                              :files-restored)
                   '(:files 3)))))

(ert-deftest agent-repl-test-wire-verbs-roll-back-success-without-prompt-refused ()
  "The prompt is REQUIRED on a success."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-roll-back-response
                   (agent-repl-test-wire-verbs--parse "{\"success\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-roll-back-error-arms ()
  "Every RollBack refusal arm decodes to its keyword and payload."
  (agent-repl-test-wire-verbs--with-common
    (dolist (case '(("{\"unknownWorkspace\":{}}" (:arm :unknown-workspace :value nil))
                    ("{\"workspaceRefMismatch\":{\"registryDir\":\"/w/real\"}}"
                     (:arm :workspace-ref-mismatch :value (:registry-dir "/w/real")))
                    ("{\"transferringAway\":{\"address\":\"127.0.0.1:9\"}}"
                     (:arm :transferring-away :value (:address "127.0.0.1:9")))
                    ("{\"notYetAdopted\":{}}" (:arm :not-yet-adopted :value nil))
                    ("{\"planStale\":{}}" (:arm :plan-stale :value nil))
                    ("{\"noSession\":{}}" (:arm :no-session :value nil))
                    ("{\"promptNotRecorded\":{}}" (:arm :prompt-not-recorded :value nil))
                    ("{\"firstPrompt\":{}}" (:arm :first-prompt :value nil))
                    ("{\"unseenPrompt\":{}}" (:arm :unseen-prompt :value nil))
                    ("{\"filesNotRestorable\":{\"vendorMessage\":\"gone\"}}"
                     (:arm :files-not-restorable :value (:vendor-message "gone")))))
      (should (equal (agent-repl-wire-decode-roll-back-response
                      (agent-repl-test-wire-verbs--parse
                       (concat "{\"error\":" (car case) "}")))
                     (list :arm :error :value (list :cause (cadr case))))))))

(ert-deftest agent-repl-test-wire-verbs-roll-back-error-unset-refused ()
  "THE ARM IS WHY: an error naming no cause is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-roll-back-response
                   (agent-repl-test-wire-verbs--parse "{\"error\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-roll-back-error-arms-pinned ()
  "RollBackError's cause oneof has exactly the arms decoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/rollback.pb.go" "RollBackError")
                       #'string<)
                 (sort (list "unknownWorkspace" "workspaceRefMismatch" "transferringAway"
                             "notYetAdopted" "planStale" "noSession" "promptNotRecorded"
                             "firstPrompt" "unseenPrompt" "filesNotRestorable")
                       #'string<))))

;;;; ---- EditHeldPrompt ------------------------------------------------------

(defconst agent-repl-test-wire-verbs--edit-said '(:text "fixed")
  "A held prompt's new content, in the shape this suite's fake UserSaid
encoder reads: the common codec owns the real encoding.")

(ert-deftest agent-repl-test-wire-verbs-edit-held-prompt-commit-request-shape ()
  "A commit carries the echoed ref, the echoed turn and the new content."
  (agent-repl-test-wire-verbs--with-common
    (let ((encoded (agent-repl-wire-encode-edit-held-prompt-request
                    (list :workspace agent-repl-test-wire-verbs--ref
                          :turn '(:value "t-1")
                          :action (list :arm :commit
                                        :value (list :said agent-repl-test-wire-verbs--edit-said))))))
      (should (equal (json-serialize encoded)
                     "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},\"turn\":{\"value\":\"t-1\"},\"commit\":{\"said\":{\"said\":\"fixed\"}}}")))))

(ert-deftest agent-repl-test-wire-verbs-edit-held-prompt-cancel-request-shape ()
  "A cancel carries the empty cancel step."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-edit-held-prompt-request
                     (list :workspace agent-repl-test-wire-verbs--ref
                           :turn '(:value "t-1")
                           :action '(:arm :cancel :value nil))))
                   "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},\"turn\":{\"value\":\"t-1\"},\"cancel\":{}}"))))

(ert-deftest agent-repl-test-wire-verbs-edit-held-prompt-begin-request-shape ()
  "A begin carries the empty begin step."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-edit-held-prompt-request
                     (list :workspace agent-repl-test-wire-verbs--ref
                           :turn '(:value "t-1")
                           :action '(:arm :begin :value nil))))
                   "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},\"turn\":{\"value\":\"t-1\"},\"begin\":{}}"))))

(ert-deftest agent-repl-test-wire-verbs-edit-held-prompt-missing-turn-refused ()
  "The turn is REQUIRED: it names which held prompt."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-edit-held-prompt-request
                   (list :workspace agent-repl-test-wire-verbs--ref
                         :action '(:arm :cancel :value nil)))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-edit-held-prompt-commit-without-content-refused ()
  "A commit replaces the content whole, so it carries one."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-edit-held-prompt-request
                   (list :workspace agent-repl-test-wire-verbs--ref
                         :turn '(:value "t-1")
                         :action '(:arm :commit :value nil)))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-edit-held-prompt-unknown-step-refused ()
  "The step vocabulary is closed."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-edit-held-prompt-request
                   (list :workspace agent-repl-test-wire-verbs--ref
                         :turn '(:value "t-1")
                         :action '(:arm :release :value nil)))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-edit-held-prompt-success ()
  "The success is empty: the new state arrives on the streams."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-edit-held-prompt-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{}}"))
                   '(:arm :success :value nil)))))

(ert-deftest agent-repl-test-wire-verbs-edit-held-prompt-empty-refusal-arms ()
  "Every empty refusal arm decodes by its own keyword (one table, one contract)."
  (agent-repl-test-wire-verbs--with-common
    (dolist (case '(("unknownWorkspace" . :unknown-workspace)
                    ("notYetAdopted" . :not-yet-adopted)
                    ("noSuchHold" . :no-such-hold)
                    ("notHeld" . :not-held)
                    ("alreadyDelivered" . :already-delivered)
                    ("notEditing" . :not-editing)
                    ("noEditor" . :no-editor)
                    ("beingDelivered" . :being-delivered)))
      (should (equal (agent-repl-wire-decode-edit-held-prompt-response
                      (agent-repl-test-wire-verbs--parse
                       (format "{\"error\":{\"%s\":{}}}" (car case))))
                     (list :arm :error :value (list :cause (list :arm (cdr case) :value nil))))))))

(ert-deftest agent-repl-test-wire-verbs-edit-held-prompt-being-edited-requires-the-turn ()
  "A being-edited refusal with no editing turn is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-edit-held-prompt-being-edited
                   (agent-repl-test-wire-verbs--parse "{}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-edit-held-prompt-being-edited-names-the-turn ()
  "The being-edited refusal names the prompt the standing edit is on."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-edit-held-prompt-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"error\":{\"beingEdited\":{\"editingTurn\":{\"value\":\"t-0\"}}}}"))
                   '(:arm :error :value (:cause (:arm :being-edited
                                                 :value (:editing-turn (:value "t-0")))))))))

(ert-deftest agent-repl-test-wire-verbs-edit-held-prompt-being-edited-without-a-turn-refused ()
  "The being-edited refusal's turn is REQUIRED."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-edit-held-prompt-response
                   (agent-repl-test-wire-verbs--parse "{\"error\":{\"beingEdited\":{}}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-edit-held-prompt-transferring-away ()
  "The transferring-away refusal carries the successor's address."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-edit-held-prompt-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"error\":{\"transferringAway\":{\"address\":\"127.0.0.1:9\"}}}"))
                   '(:arm :error :value (:cause (:arm :transferring-away
                                                 :value (:address "127.0.0.1:9"))))))))

(ert-deftest agent-repl-test-wire-verbs-edit-held-prompt-unknown-refusal-refused ()
  "A refusal arm this codec does not hold is refused, never defaulted."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-edit-held-prompt-response
                   (agent-repl-test-wire-verbs--parse "{\"error\":{\"lockedOut\":{}}}"))
                  :type 'agent-repl-wire-error)))


;;;; ---- ReportLatencies ----------------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-report-latencies-request-shape ()
  "The editor's startup milestones ride `editorStartup', each fully spelled."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-encode-report-latencies-request
                    (list :editor-startup
                          (list (list :milestone :daemon-linked :daemon :spawned :seconds 2)
                                (list :milestone :all-tabs :daemon :running :seconds 6.5))))
                   '((editorStartup . [((milestone . "EDITOR_STARTUP_MILESTONE_DAEMON_LINKED")
                                        (spawned)
                                        (secondsSinceProcessStart . 2.0))
                                       ((milestone . "EDITOR_STARTUP_MILESTONE_ALL_TABS")
                                        (running)
                                        (secondsSinceProcessStart . 6.5))])
                     (editorLatencies . []))))))

(ert-deftest agent-repl-test-wire-verbs-report-latencies-request-carries-editor-latencies ()
  "The editor's own latencies ride `editorLatencies', each fully spelled."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-encode-report-latencies-request
                    (list :editor-latencies
                          (list (list :kind :stall :seconds 0.25)
                                (list :kind :elisp-reload :seconds 1)
                                (list :kind :input-wait :seconds 0.4))))
                   '((editorStartup . [])
                     (editorLatencies . [((kind . "EDITOR_LATENCY_KIND_STALL") (seconds . 0.25))
                                         ((kind . "EDITOR_LATENCY_KIND_ELISP_RELOAD") (seconds . 1.0))
                                         ((kind . "EDITOR_LATENCY_KIND_INPUT_WAIT") (seconds . 0.4))]))))))

(ert-deftest agent-repl-test-wire-verbs-report-latencies-refuses-a-malformed-editor-latency ()
  "An unknown kind or a bad time never reaches the wire."
  (agent-repl-test-wire-verbs--with-common
    (dolist (latency (list (list :kind :unspecified :seconds 1)
                           (list :kind :stall :seconds -1)
                           (list :kind :stall :seconds 0.0e+NaN)
                           (list :kind :stall :seconds 1.0e+INF)
                           (list :kind :stall)))
      (should-error (agent-repl-wire-encode-report-latencies-request
                     (list :editor-latencies (list latency)))
                    :type 'agent-repl-wire-error))))

(ert-deftest agent-repl-test-wire-verbs-editor-latency-kinds-pinned ()
  "The editor latency vocabulary is every generated enum name except UNSPECIFIED."
  (should (equal
           (sort (mapcar #'cdr agent-repl-wire-editor-latency-kinds) #'string<)
           (sort (remove "EDITOR_LATENCY_KIND_UNSPECIFIED"
                         (agent-repl-test--generated-enum-names
                          "agentrepl/v1/endpoint_report_latencies.pb.go"
                          "EDITOR_LATENCY_KIND_"))
                 #'string<))))

(ert-deftest agent-repl-test-wire-verbs-report-latencies-request-serializes-empty-arms ()
  "An empty daemon arm serializes as an empty object, as protojson expects."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-report-latencies-request
                     (list :editor-startup (list (list :milestone :init :daemon :spawned :seconds 1)))))
                   "{\"editorStartup\":[{\"milestone\":\"EDITOR_STARTUP_MILESTONE_INIT\",\"spawned\":{},\"secondsSinceProcessStart\":1.0}],\"editorLatencies\":[]}"))))

(ert-deftest agent-repl-test-wire-verbs-report-latencies-refuses-a-malformed-milestone ()
  "An unknown milestone or daemon arm, or a bad time, never reaches the wire."
  (agent-repl-test-wire-verbs--with-common
    (dolist (milestone (list (list :milestone :unspecified :daemon :spawned :seconds 1)
                             (list :milestone :init :daemon :unknown :seconds 1)
                             (list :milestone :init :daemon :spawned :seconds -1)
                             (list :milestone :init :daemon :spawned :seconds 0.0e+NaN)
                             (list :milestone :init :daemon :spawned :seconds 1.0e+INF)
                             (list :milestone :init :daemon :spawned)))
      (should-error (agent-repl-wire-encode-report-latencies-request
                     (list :editor-startup (list milestone)))
                    :type 'agent-repl-wire-error))))

(ert-deftest agent-repl-test-wire-verbs-editor-startup-milestones-pinned ()
  "The milestone vocabulary is every generated enum name except UNSPECIFIED."
  (should (equal
           (sort (mapcar #'cdr agent-repl-wire-editor-startup-milestones) #'string<)
           (sort (remove "EDITOR_STARTUP_MILESTONE_UNSPECIFIED"
                         (agent-repl-test--generated-enum-names
                          "agentrepl/v1/endpoint_report_latencies.pb.go"
                          "EDITOR_STARTUP_MILESTONE_"))
                 #'string<))))

(ert-deftest agent-repl-test-wire-verbs-editor-startup-daemon-arms-pinned ()
  "The daemon arms are exactly the generated oneof's."
  (should (equal (sort (mapcar (lambda (c) (symbol-name (cdr c))) agent-repl-wire-editor-startup-daemons) #'string<)
                 (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_report_latencies.pb.go" "ReportedEditorStartup")
                       #'string<))))

(ert-deftest agent-repl-test-wire-verbs-report-latencies-response-arms ()
  "Every success arm decodes, and the refusal names its mismatch."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-report-latencies-response
                    '((success . ((recorded . ((recorded . 3)))))))
                   '(:arm :success :value (:arm :recorded :value (:recorded 3)))))
    (should (equal (agent-repl-wire-decode-report-latencies-response
                    '((success . ((recorded . nil)))))
                   '(:arm :success :value (:arm :recorded :value (:recorded 0)))))
    (should (equal (agent-repl-wire-decode-report-latencies-response
                    '((success . ((partlyRecorded . ((recorded . 1) (droppedUnknownWorkspace . 2)))))))
                   '(:arm :success :value (:arm :partly-recorded :value (:recorded 1 :dropped 2)))))
    (should (equal (agent-repl-wire-decode-report-latencies-response
                    '((success . ((metricsOff . nil)))))
                   '(:arm :success :value (:arm :metrics-off :value nil))))
    (should (equal (agent-repl-wire-decode-report-latencies-response
                    '((error . ((workspaceRefMismatch . ((workspaceId . "w") (reportedDir . "/a") (registryDir . "/b")))))))
                   '(:arm :error :value (:arm :workspace-ref-mismatch
                                         :value (:workspace-id "w" :reported-dir "/a" :registry-dir "/b")))))))

(ert-deftest agent-repl-test-wire-verbs-report-latencies-response-refuses-unknown-fields ()
  "A field the codec does not know is loud rather than dropped."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-report-latencies-response
                   '((success . ((recorded . ((recorded . 1) (extra . 2)))))))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-feed-selection-row-viewport-reveal-node-names-its-node ()
  "A selected response a prompt's entry revealed decodes `:reveal-node' with its node's label."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-feed-selection-row-viewport
                    (agent-repl-test-wire-verbs--parse "{\"revealNode\":{\"label\":\"3.1\"}}"))
                   '(:viewport (:arm :reveal-node :value (:label "3.1")))))))

(ert-deftest agent-repl-test-wire-verbs-feed-selection-row-viewport-reveal-node-refuses-a-bad-message ()
  "A revealed node with an empty label, or a field it does not declare, is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (dolist (json '("{\"revealNode\":{}}" "{\"revealNode\":{\"label\":\"3.1\",\"extra\":1}}"))
      (should-error (agent-repl-wire-decode-feed-selection-row-viewport
                     (agent-repl-test-wire-verbs--parse json))
                    :type 'agent-repl-wire-error))))

(provide 'test-wire-verbs)

;;; test-wire-verbs.el ends here

;;;; ---- RegisterRepository ----------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-register-repository-request-carries-the-path ()
  "The request is ONE field: any path inside the repository."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-encode-register-repository-request '(:path "/r/one/README.md"))
                   '((path . "/r/one/README.md"))))))

(ert-deftest agent-repl-test-wire-verbs-register-repository-request-refuses-a-blank-path ()
  "A blank path is not a path: the request is refused before it can be sent."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-register-repository-request '(:path ""))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-register-repository-request-refuses-an-unset-path ()
  "An unset path is a contract breach, never an empty string on the wire."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-register-repository-request nil)
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-register-repository-success-carries-the-ref ()
  "The success carries the daemon-minted RepositoryRef."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-register-repository-success
                    (agent-repl-test-wire-verbs--parse
                     "{\"repository\":{\"id\":\"repo-1\",\"dir\":\"/r/one\"},\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/r/one\"}}"))
                   '(:repository (:id "repo-1" :dir "/r/one") :already-known nil
                     :workspace (:id "ws-1" :dir "/r/one")
                     :workspace-already-known nil)))))

(ert-deftest agent-repl-test-wire-verbs-register-repository-success-carries-already-known ()
  "`already_known' is an ANSWER, so it decodes rather than being inferred."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-register-repository-success
                    (agent-repl-test-wire-verbs--parse
                     "{\"repository\":{\"id\":\"repo-1\",\"dir\":\"/r/one\"},\"alreadyKnown\":true,\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/r/one\"}}"))
                   '(:repository (:id "repo-1" :dir "/r/one") :already-known t
                     :workspace (:id "ws-1" :dir "/r/one")
                     :workspace-already-known nil)))))

(ert-deftest agent-repl-test-wire-verbs-register-repository-success-carries-the-workspace-ref ()
  "The success carries the main worktree the same call registered as a workspace.
It is what `SPC p p\=' switches to, so a success without it would say the
registration happened while naming nothing the user can act on."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (plist-get (agent-repl-wire-decode-register-repository-success
                               (agent-repl-test-wire-verbs--parse
                                "{\"repository\":{\"id\":\"repo-1\",\"dir\":\"/r/one\"},\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/r/one\"}}"))
                              :workspace)
                   '(:id "ws-1" :dir "/r/one")))))

(ert-deftest agent-repl-test-wire-verbs-register-repository-success-carries-workspace-already-known ()
  "`workspace_already_known' decodes on its own: it is independent of the
repository's own `already_known', and the ack reports the two apart."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-register-repository-success
                    (agent-repl-test-wire-verbs--parse
                     "{\"repository\":{\"id\":\"repo-1\",\"dir\":\"/r/one\"},\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/r/one\"},\"workspaceAlreadyKnown\":true}"))
                   '(:repository (:id "repo-1" :dir "/r/one") :already-known nil
                     :workspace (:id "ws-1" :dir "/r/one")
                     :workspace-already-known t)))))

(ert-deftest agent-repl-test-wire-verbs-register-repository-success-without-a-ref-is-a-breach ()
  "A success naming no repository says nothing the caller can use."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-register-repository-success
                   (agent-repl-test-wire-verbs--parse "{\"alreadyKnown\":true,\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/r/one\"}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-register-repository-success-without-a-workspace-is-a-breach ()
  "A success naming no workspace is a breach for the same reason: the endpoint
always registers the main worktree, so an answer without it is not one."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-register-repository-success
                   (agent-repl-test-wire-verbs--parse
                    "{\"repository\":{\"id\":\"repo-1\",\"dir\":\"/r/one\"}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-register-repository-error-not-in-a-repository-arm ()
  "RegisterRepositoryError's `not_in_a_repository' arm decodes as an empty arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-register-repository-error
                    (agent-repl-test-wire-verbs--parse "{\"notInARepository\":{}}"))
                   '(:cause (:arm :not-in-a-repository :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-register-repository-error-unreadable-path-arm ()
  "RegisterRepositoryError's `unreadable_path' arm decodes as an empty arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-register-repository-error
                    (agent-repl-test-wire-verbs--parse "{\"unreadablePath\":{}}"))
                   '(:cause (:arm :unreadable-path :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-register-repository-error-inside-temporary-directory-arm ()
  "RegisterRepositoryError's `inside_temporary_directory' arm decodes with the
dir and the temporary root it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-register-repository-error
                    (agent-repl-test-wire-verbs--parse "{\"insideTemporaryDirectory\":{\"dir\":\"/private/tmp/scratch\",\"temporaryRoot\":\"/private/tmp\"}}"))
                   '(:cause (:arm :inside-temporary-directory :value (:dir "/private/tmp/scratch" :temporary-root "/private/tmp")))))))

(ert-deftest agent-repl-test-wire-verbs-register-repository-error-unset-cause-is-a-breach ()
  "An error with no arm set says nothing actionable, so it is a breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-register-repository-error
                   (agent-repl-test-wire-verbs--parse "{}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-register-repository-error-unknown-arm-is-a-breach ()
  "An arm this codec does not declare is refused, never guessed at."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-register-repository-error
                   (agent-repl-test-wire-verbs--parse "{\"noSuchArm\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-register-repository-response-success-arm ()
  "The response's `success' arm decodes through the shared result oneof."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-register-repository-response
                    (agent-repl-test-wire-verbs--parse
                     "{\"success\":{\"repository\":{\"id\":\"repo-1\",\"dir\":\"/r/one\"},\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/r/one\"}}}"))
                   '(:arm :success
                     :value (:repository (:id "repo-1" :dir "/r/one") :already-known nil
                             :workspace (:id "ws-1" :dir "/r/one")
                             :workspace-already-known nil))))))

(ert-deftest agent-repl-test-wire-verbs-register-repository-response-error-arm ()
  "The response's `error' arm decodes through the shared result oneof."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-register-repository-response
                    (agent-repl-test-wire-verbs--parse "{\"error\":{\"unreadablePath\":{}}}"))
                   '(:arm :error :value (:cause (:arm :unreadable-path :value nil)))))))

(ert-deftest agent-repl-test-wire-verbs-register-repository-cause-arms-pinned ()
  "RegisterRepositoryError's cause oneof has exactly the arms this codec decodes."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_register_repository.pb.go"
                        "RegisterRepositoryError")
                       #'string<)
                 '("insideTemporaryDirectory" "notInARepository" "unreadablePath"))))

;;;; ---- AdjustFeedTextScale ---------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-adjust-feed-text-scale-direction-increase ()
  "The increase keyword encodes to its protojson enum name."
  (should (equal (agent-repl-wire-encode-adjust-feed-text-scale-direction :increase)
                 "ADJUST_FEED_TEXT_SCALE_DIRECTION_INCREASE")))

(ert-deftest agent-repl-test-wire-verbs-adjust-feed-text-scale-direction-decrease ()
  "The decrease keyword encodes to its protojson enum name."
  (should (equal (agent-repl-wire-encode-adjust-feed-text-scale-direction :decrease)
                 "ADJUST_FEED_TEXT_SCALE_DIRECTION_DECREASE")))

(ert-deftest agent-repl-test-wire-verbs-adjust-feed-text-scale-direction-refuses-unspecified ()
  "UNSPECIFIED is never a legitimate nudge, so it is refused before the wire."
  (should-error (agent-repl-wire-encode-adjust-feed-text-scale-direction :unspecified)
                :type 'agent-repl-wire-error))

(ert-deftest agent-repl-test-wire-verbs-adjust-feed-text-scale-vocabulary-pinned ()
  "The direction vocabulary is every generated enum name except UNSPECIFIED."
  (should (equal
           (sort (mapcar #'cdr agent-repl-wire-adjust-feed-text-scale-directions) #'string<)
           (sort (remove "ADJUST_FEED_TEXT_SCALE_DIRECTION_UNSPECIFIED"
                         (agent-repl-test--generated-enum-names
                          "agentrepl/v1/endpoint_adjust_feed_text_scale.pb.go"
                          "ADJUST_FEED_TEXT_SCALE_DIRECTION_"))
                 #'string<))))

(ert-deftest agent-repl-test-wire-verbs-adjust-feed-text-scale-request-shape ()
  "A nudge carries only the chosen direction (the scale is daemon-global)."
  (should (equal (agent-repl-wire-encode-adjust-feed-text-scale-request '(:direction :increase))
                 '((direction . "ADJUST_FEED_TEXT_SCALE_DIRECTION_INCREASE")))))

(ert-deftest agent-repl-test-wire-verbs-adjust-feed-text-scale-missing-direction-refused ()
  "The direction is REQUIRED, so a nudge without one never reaches the wire."
  (should-error (agent-repl-wire-encode-adjust-feed-text-scale-request '())
                :type 'agent-repl-wire-error))

(ert-deftest agent-repl-test-wire-verbs-adjust-feed-text-scale-response-scale ()
  "The response decodes the clamped scale now in force as a float."
  (should (equal (agent-repl-wire-decode-adjust-feed-text-scale-response
                  (agent-repl-test-wire-verbs--parse "{\"scale\":1.02}"))
                 '(:scale 1.02))))

;;;; ---- The shared op id and accepted-result helpers --------------------

(ert-deftest agent-repl-test-wire-verbs-append-op-id-leaves-a-request-without-one-alone ()
  "A request that minted no op id is returned as encoded."
  (should (equal (agent-repl-wire-verbs--append-op-id '((workspace . 1)) nil)
                 '((workspace . 1)))))

(ert-deftest agent-repl-test-wire-verbs-append-op-id-puts-the-op-id-last ()
  "A minted op id rides as `opId', after every other field."
  (should (equal (agent-repl-wire-verbs--append-op-id '((workspace . 1)) '(:op-id "op-1"))
                 '((workspace . 1) (opId . "op-1")))))

(ert-deftest agent-repl-test-wire-verbs-accepting-result-decodes-each-arm ()
  "The three-arm result decodes success, error and accepted by their decoders."
  (dolist (case '((success :success) (error :error) (accepted :accepted)))
    (should (equal (agent-repl-wire-verbs--decode-accepting-result
                    "M" (list (cons (car case) '((x . 1))))
                    (lambda (_) 'decoded-success)
                    (lambda (_) 'decoded-error)
                    (lambda (_) 'decoded-accepted))
                   (list :arm (cadr case)
                         :value (intern (format "decoded-%s" (car case))))))))

(ert-deftest agent-repl-test-wire-verbs-accepting-result-refuses-an-unknown-arm ()
  "A field outside the three arms is a contract breach."
  (should-error (agent-repl-wire-verbs--decode-accepting-result
                 "M" '((other . 1)) #'ignore #'ignore #'ignore)))

(defun agent-repl-test-wire-verbs--occurrences (needle haystack)
  "Count the non-overlapping occurrences of NEEDLE in HAYSTACK."
  (let ((count 0) (start 0))
    (while (setq start (string-search needle haystack start))
      (setq count (1+ count) start (+ start (length needle))))
    count))

(ert-deftest agent-repl-test-wire-verbs-no-codec-hand-rolls-the-op-id-or-accepted-result ()
  "Every op id and every accepted result goes through the shared helpers.
A hand-rolled site is exactly the one that drifts: it spells `opId' or the
three arms its own way."
  (let* ((file (file-name-with-extension
                (symbol-file 'agent-repl-wire-verbs--append-op-id 'defun) "el"))
         (source (with-temp-buffer (insert-file-contents file) (buffer-string))))
    (should (= (agent-repl-test-wire-verbs--occurrences "(cons 'opId" source) 1))
    (should (= (agent-repl-test-wire-verbs--occurrences "'(success error accepted)" source) 1))))

;;;; ---- The lone-`detail' decoder ----------------------------------------

(ert-deftest agent-repl-test-wire-verbs-decode-detail-carries-the-sentence ()
  "A message carrying only `detail' decodes to (:detail SENTENCE)."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-verbs--decode-detail
                    "Anything" (agent-repl-test-wire-verbs--parse "{\"detail\":\"it broke\"}"))
                   '(:detail "it broke")))))

(ert-deftest agent-repl-test-wire-verbs-decode-detail-refuses-another-field ()
  "A field beside `detail' is refused as unknown, never dropped."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-verbs--decode-detail
                   "Anything" (agent-repl-test-wire-verbs--parse "{\"detail\":\"x\",\"cause\":\"y\"}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-no-codec-hand-rolls-a-lone-detail ()
  "Every message carrying only `detail' decodes through the one helper.
A hand-rolled site is a second spelling of the same shape, which drifts."
  (let* ((file (file-name-with-extension
                (symbol-file 'agent-repl-wire-verbs--decode-detail 'defun) "el"))
         (source (with-temp-buffer (insert-file-contents file) (buffer-string))))
    (should (= (agent-repl-test-wire-verbs--occurrences "'(detail))" source) 1))))

;;;; ---- KillWorkspace / NukeWorkspace: the immediate ack --------------

(ert-deftest agent-repl-test-wire-verbs-kill-and-nuke-requests-carry-the-op-id ()
  "An op id rides a kill's and a nuke's request, opting into the immediate ack."
  (agent-repl-test-wire-verbs--with-common
   (dolist (encode '(agent-repl-wire-encode-kill-workspace-request
                     agent-repl-wire-encode-nuke-workspace-request))
     (should (equal (cdr (assq 'opId (funcall encode
                                              (list :workspace agent-repl-test-wire-verbs--ref
                                                    :op-id "op-7"))))
                    "op-7")))))

(ert-deftest agent-repl-test-wire-verbs-kill-and-nuke-decode-the-accepted-arm ()
  "The immediate ack decodes to the accepted arm, echoing the op id."
  (dolist (decode '(agent-repl-wire-decode-kill-workspace-response
                    agent-repl-wire-decode-nuke-workspace-response))
    (should (equal (funcall decode (agent-repl-test-wire-verbs--parse
                                    "{\"accepted\":{\"opId\":\"op-7\"}}"))
                   '(:arm :accepted :value (:op-id "op-7"))))))

;;;; ---- OpenWorkspace: the optional correlation id ----------------------

(ert-deftest agent-repl-test-wire-verbs-open-workspace-request-omits-an-absent-op-id ()
  "An open that minted no op id sends the bare {workspace} it always did."
  (agent-repl-test-wire-verbs--with-common
   (should (equal (agent-repl-wire-encode-open-workspace-request
                   (list :workspace agent-repl-test-wire-verbs--ref))
                  '((workspace . ((id . "ws-1") (dir . "/w/one"))))))))

(ert-deftest agent-repl-test-wire-verbs-open-workspace-request-carries-the-op-id ()
  "An op id rides the request, which is what opts the open into stage pushes."
  (agent-repl-test-wire-verbs--with-common
   (should (equal (cdr (assq 'opId
                             (agent-repl-wire-encode-open-workspace-request
                              (list :workspace agent-repl-test-wire-verbs--ref
                                    :op-id "op-42"))))
                  "op-42"))))


;;;; ---- ListWorkspaceTranscripts ---------------------------------------

(ert-deftest agent-repl-test-wire-verbs-list-transcripts-request-is-the-bare-workspace ()
  "The shim resolves the directory from its own identity, so the request names
nothing but the workspace."
  (agent-repl-test-wire-verbs--with-common
   (should (equal (agent-repl-wire-encode-list-workspace-transcripts-request
                   (list :workspace agent-repl-test-wire-verbs--ref))
                  '((workspace . ((id . "ws-1") (dir . "/w/one"))))))))

(ert-deftest agent-repl-test-wire-verbs-list-transcripts-request-refuses-no-workspace ()
  "A request built without the workspace it names is refused before it is sent."
  (agent-repl-test-wire-verbs--with-common
   (should-error (agent-repl-wire-encode-list-workspace-transcripts-request nil)
                 :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-transcript-decodes-its-stated-figures ()
  "Every figure the transcript stated reaches the chooser."
  (agent-repl-test-wire-verbs--with-common
   (let ((transcript (agent-repl-wire-decode-workspace-transcript
                      (agent-repl-test-wire-verbs--parse
                       "{\"vendorSessionId\":\"a\",\"lastRequestAtMs\":\"17\",\"contextTokens\":\"4242\",\"opening\":\"hi\",\"prompts\":3}"))))
     (should (equal (list (plist-get transcript :vendor-session-id)
                          (plist-get transcript :last-request-at-ms)
                          (plist-get transcript :context-tokens)
                          (plist-get transcript :opening)
                          (plist-get transcript :prompts))
                    (list "a" 17 4242 "hi" 3))))))

(ert-deftest agent-repl-test-wire-verbs-transcript-leaves-an-unstated-size-nil ()
  "A zero that cannot be told from an absence ranks the unread conversation
cheapest, so an absent `context_tokens' decodes to nil rather than 0."
  (agent-repl-test-wire-verbs--with-common
   (should-not (plist-get (agent-repl-wire-decode-workspace-transcript
                           (agent-repl-test-wire-verbs--parse "{\"vendorSessionId\":\"a\"}"))
                          :context-tokens))))

(ert-deftest agent-repl-test-wire-verbs-transcript-leaves-an-unstated-instant-nil ()
  "An absent `last_request_at_ms' decodes to nil, never to the epoch."
  (agent-repl-test-wire-verbs--with-common
   (should-not (plist-get (agent-repl-wire-decode-workspace-transcript
                           (agent-repl-test-wire-verbs--parse "{\"vendorSessionId\":\"a\"}"))
                          :last-request-at-ms))))

(ert-deftest agent-repl-test-wire-verbs-transcript-leaves-an-unstated-opening-nil ()
  "A transcript holding no user prompt states no opening."
  (agent-repl-test-wire-verbs--with-common
   (should-not (plist-get (agent-repl-wire-decode-workspace-transcript
                           (agent-repl-test-wire-verbs--parse "{\"vendorSessionId\":\"a\"}"))
                          :opening))))

(ert-deftest agent-repl-test-wire-verbs-transcript-decodes-the-current-marker ()
  "Presence IS the fact: the empty message decodes to t, not to nil."
  (agent-repl-test-wire-verbs--with-common
   (should (eq (plist-get (agent-repl-wire-decode-workspace-transcript
                           (agent-repl-test-wire-verbs--parse
                            "{\"vendorSessionId\":\"a\",\"current\":{}}"))
                          :current)
               t))))

(ert-deftest agent-repl-test-wire-verbs-transcript-decodes-the-cleared-instant ()
  "A cleared conversation carries when it was cleared."
  (agent-repl-test-wire-verbs--with-common
   (should (equal (plist-get (agent-repl-wire-decode-workspace-transcript
                              (agent-repl-test-wire-verbs--parse
                               "{\"vendorSessionId\":\"a\",\"cleared\":{\"atMs\":\"5\"}}"))
                             :cleared)
                  '(:at-ms 5)))))

(ert-deftest agent-repl-test-wire-verbs-transcript-decodes-the-active-instant ()
  "An active transcript carries when it was last appended to."
  (agent-repl-test-wire-verbs--with-common
   (should (equal (plist-get (agent-repl-wire-decode-workspace-transcript
                              (agent-repl-test-wire-verbs--parse
                               "{\"vendorSessionId\":\"a\",\"active\":{\"atMs\":\"9\"}}"))
                             :active)
                  '(:at-ms 9)))))

(ert-deftest agent-repl-test-wire-verbs-transcript-decodes-the-holding-workspace ()
  "The held marker NAMES the workspace rather than saying only that one holds it."
  (agent-repl-test-wire-verbs--with-common
   (should (equal (plist-get (plist-get (agent-repl-wire-decode-workspace-transcript
                                         (agent-repl-test-wire-verbs--parse
                                          "{\"vendorSessionId\":\"a\",\"held\":{\"workspace\":{\"id\":\"ws-2\",\"dir\":\"/w/two\"}}}"))
                                        :held)
                             :workspace)
                  '(:id "ws-2" :dir "/w/two")))))

(ert-deftest agent-repl-test-wire-verbs-transcript-decodes-the-last-model ()
  "The model that answered the last request reaches the chooser by name."
  (agent-repl-test-wire-verbs--with-common
   (should (equal (plist-get (agent-repl-wire-decode-workspace-transcript
                              (agent-repl-test-wire-verbs--parse
                               "{\"vendorSessionId\":\"a\",\"lastModel\":{\"name\":\"claude-opus-5\"}}"))
                             :last-model)
                  '(:name "claude-opus-5")))))

(ert-deftest agent-repl-test-wire-verbs-transcript-unknown-field-is-a-breach ()
  "A field the daemon adds without being threaded here is loud, not dropped."
  (agent-repl-test-wire-verbs--with-common
   (should-error (agent-repl-wire-decode-workspace-transcript
                  (agent-repl-test-wire-verbs--parse "{\"noSuchField\":1}"))
                 :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-list-transcripts-empty-success ()
  "An EMPTY list is a success: a directory with no conversations is an answer."
  (agent-repl-test-wire-verbs--with-common
   (should (equal (agent-repl-wire-decode-list-workspace-transcripts-success
                   (agent-repl-test-wire-verbs--parse "{}"))
                  '(:transcripts nil)))))

(ert-deftest agent-repl-test-wire-verbs-list-transcripts-error-unknown-arm-is-a-breach ()
  "An arm this codec does not know is refused, never guessed at."
  (agent-repl-test-wire-verbs--with-common
   (should-error (agent-repl-wire-decode-list-workspace-transcripts-error
                  (agent-repl-test-wire-verbs--parse "{\"noSuchArm\":{}}"))
                 :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-list-transcripts-error-unreadable-carries-its-evidence ()
  "The arm carries the path and the read's own account, not only a sentence."
  (agent-repl-test-wire-verbs--with-common
   (should (equal (agent-repl-wire-decode-list-workspace-transcripts-error
                   (agent-repl-test-wire-verbs--parse
                    "{\"unreadable\":{\"searchedPath\":\"/p\",\"detail\":\"EACCES\"}}"))
                  '(:cause (:arm :unreadable :value (:searched-path "/p" :detail "EACCES")))))))

(ert-deftest agent-repl-test-wire-verbs-list-transcripts-error-arms-pinned ()
  "ListWorkspaceTranscriptsError's arm set is exactly what the schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_list_workspace_transcripts.pb.go"
                        "ListWorkspaceTranscriptsError")
                       #'string<)
                 (sort (list "unknownWorkspace" "workspaceRefMismatch" "transferringAway"
                             "noSession" "unreadable")
                       #'string<))))


;;;; ---- BindWorkspaceSession -------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-bind-request-carries-the-echoed-id ()
  "The id is an ECHO of a served value and rides the request verbatim."
  (agent-repl-test-wire-verbs--with-common
   (should (equal (cdr (assq 'vendorSessionId
                             (agent-repl-wire-encode-bind-workspace-session-request
                              (list :workspace agent-repl-test-wire-verbs--ref
                                    :vendor-session-id "conv-1"))))
                  "conv-1"))))

(ert-deftest agent-repl-test-wire-verbs-bind-request-refuses-a-blank-id ()
  "A client may not invent an id, and it certainly may not invent nothing."
  (agent-repl-test-wire-verbs--with-common
   (should-error (agent-repl-wire-encode-bind-workspace-session-request
                  (list :workspace agent-repl-test-wire-verbs--ref :vendor-session-id ""))
                 :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-bind-request-omits-an-absent-op-id ()
  "A bind with no op id emits no stages, so it sends no op id."
  (agent-repl-test-wire-verbs--with-common
   (should-not (assq 'opId (agent-repl-wire-encode-bind-workspace-session-request
                            (list :workspace agent-repl-test-wire-verbs--ref
                                  :vendor-session-id "conv-1"))))))

(ert-deftest agent-repl-test-wire-verbs-bind-request-carries-the-op-id ()
  "An op id rides the request, which is what opts the bind into stage pushes."
  (agent-repl-test-wire-verbs--with-common
   (should (equal (cdr (assq 'opId (agent-repl-wire-encode-bind-workspace-session-request
                                    (list :workspace agent-repl-test-wire-verbs--ref
                                          :vendor-session-id "conv-1"
                                          :op-id "op-42"))))
                  "op-42"))))

(ert-deftest agent-repl-test-wire-verbs-bind-error-unknown-transcript-names-the-id ()
  "The arm echoes the id that was asked for, so the client can name it."
  (agent-repl-test-wire-verbs--with-common
   (should (equal (agent-repl-wire-decode-bind-workspace-session-error
                   (agent-repl-test-wire-verbs--parse
                    "{\"unknownTranscript\":{\"vendorSessionId\":\"invented\"}}"))
                  '(:cause (:arm :unknown-transcript :value (:vendor-session-id "invented")))))))

(ert-deftest agent-repl-test-wire-verbs-bind-error-transcript-active-carries-the-instant ()
  "Arrange, Act, Assert."
  (agent-repl-test-wire-verbs--with-common
   (should (equal (agent-repl-wire-decode-bind-workspace-session-error
                   (agent-repl-test-wire-verbs--parse "{\"transcriptActive\":{\"atMs\":\"7\"}}"))
                  '(:cause (:arm :transcript-active :value (:at-ms 7)))))))

(ert-deftest agent-repl-test-wire-verbs-bind-error-transcript-held-names-the-holder ()
  "Arrange, Act, Assert."
  (agent-repl-test-wire-verbs--with-common
   (should (equal (agent-repl-wire-decode-bind-workspace-session-error
                   (agent-repl-test-wire-verbs--parse
                    "{\"transcriptHeld\":{\"workspace\":{\"id\":\"ws-2\",\"dir\":\"/w/two\"}}}"))
                  '(:cause (:arm :transcript-held
                            :value (:workspace (:id "ws-2" :dir "/w/two"))))))))

(ert-deftest agent-repl-test-wire-verbs-bind-error-start-failed-carries-its-detail ()
  "THE BINDING STANDS, and the arm says why the session did not come up."
  (agent-repl-test-wire-verbs--with-common
   (should (equal (agent-repl-wire-decode-bind-workspace-session-error
                   (agent-repl-test-wire-verbs--parse "{\"startFailed\":{\"detail\":\"nope\"}}"))
                  '(:cause (:arm :start-failed :value (:detail "nope")))))))

(ert-deftest agent-repl-test-wire-verbs-bind-error-already-bound-is-empty ()
  "Arrange, Act, Assert: nothing was changed, so the arm carries nothing."
  (agent-repl-test-wire-verbs--with-common
   (should (equal (agent-repl-wire-decode-bind-workspace-session-error
                   (agent-repl-test-wire-verbs--parse "{\"alreadyBound\":{}}"))
                  '(:cause (:arm :already-bound :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-bind-error-unknown-arm-is-a-breach ()
  "An arm this codec does not know is refused, never guessed at."
  (agent-repl-test-wire-verbs--with-common
   (should-error (agent-repl-wire-decode-bind-workspace-session-error
                  (agent-repl-test-wire-verbs--parse "{\"noSuchArm\":{}}"))
                 :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-bind-error-arms-pinned ()
  "BindWorkspaceSessionError's arm set is exactly what the schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_bind_workspace_session.pb.go"
                        "BindWorkspaceSessionError")
                       #'string<)
                 (sort (list "unknownWorkspace" "workspaceRefMismatch" "transferringAway"
                             "unknownTranscript" "alreadyBound" "transcriptActive"
                             "transcriptHeld" "turnInFlight" "stopFailed" "startFailed")
                       #'string<))))

;;;; ---- Quiet decoding, shared by the verbs below -----------------------

(defun agent-repl-test-wire-verbs--decode-quietly (decoder json)
  "Decode JSON with DECODER, quietly.
THE ONE quiet decode the verb suites below share; a suite decoding by hand
instead diverges from it unseen."
  (agent-repl-test-wire-verbs--with-common
    (funcall decoder (agent-repl-test-wire-verbs--parse json))))

(defun agent-repl-test-wire-verbs--decode-breach (decoder json)
  "Return the `agent-repl-wire-error' data decoding JSON with DECODER raises."
  (condition-case err
      (progn (agent-repl-test-wire-verbs--decode-quietly decoder json) nil)
    (agent-repl-wire-error (cdr err))))

;;;; ---- ReportComposerEntries ------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-report-composer-entries-names-the-entry-at-point ()
  "A report carries the ref, the labels in order and the entry point is on."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-report-composer-entries-request
                     (list :workspace agent-repl-test-wire-verbs--ref :labels '("3.1" "3.2")
                           :at-point '(:arm :entry :value (:label "3.2")))))
                   "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},\"labels\":[\"3.1\",\"3.2\"],\"edits\":0,\"atPoint\":{\"entry\":{\"label\":\"3.2\"}}}"))))

(ert-deftest agent-repl-test-wire-verbs-report-composer-entries-carries-the-edit-count ()
  "A report carries the composer's edit count, 0 when none is given."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (alist-get 'edits
                              (agent-repl-wire-encode-report-composer-entries-request
                               (list :workspace agent-repl-test-wire-verbs--ref :labels nil :at-point '(:arm :none) :edits 5)))
                   5))
    (should (equal (alist-get 'edits
                              (agent-repl-wire-encode-report-composer-entries-request
                               (list :workspace agent-repl-test-wire-verbs--ref :labels nil :at-point '(:arm :none))))
                   0))))

(ert-deftest agent-repl-test-wire-verbs-report-composer-entries-with-point-on-no-entry ()
  "Point on no entry is the empty `none' arm, and no labels an empty array."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-report-composer-entries-request
                     (list :workspace agent-repl-test-wire-verbs--ref :labels nil :at-point '(:arm :none))))
                   "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},\"labels\":[],\"edits\":0,\"atPoint\":{\"none\":{}}}"))))

(ert-deftest agent-repl-test-wire-verbs-report-composer-entries-refuses-a-missing-point ()
  "Which entry point is on is REQUIRED, so a report without it never reaches the wire."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-report-composer-entries-request
                   (list :workspace agent-repl-test-wire-verbs--ref :labels '("3.1")))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-report-composer-entries-success-decodes ()
  "The empty success decodes as the success arm."
  (should (equal (plist-get (agent-repl-test-wire-verbs--decode-quietly
                             #'agent-repl-wire-decode-report-composer-entries-response "{\"success\":{}}")
                            :arm)
                 :success)))

(ert-deftest agent-repl-test-wire-verbs-report-composer-entries-refusal-decodes ()
  "A handover refusal decodes as its arm."
  (should (equal (plist-get (plist-get (agent-repl-test-wire-verbs--decode-quietly
                                        #'agent-repl-wire-decode-report-composer-entries-response
                                        "{\"error\":{\"unknownWorkspace\":{}}}")
                                       :value)
                            :cause)
                 '(:arm :unknown-workspace :value nil))))

;;;; ---- FindEntryNeighbor ----------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-find-entry-neighbor-names-the-line-and-the-way ()
  "A request carries the ref, the entry line's label and which way it opens."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-find-entry-neighbor-request
                     (list :workspace agent-repl-test-wire-verbs--ref :label "3.1" :direction '(:arm :previous))))
                   "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},\"label\":\"3.1\",\"direction\":{\"previous\":{}}}"))))

(ert-deftest agent-repl-test-wire-verbs-find-entry-neighbor-encodes-every-way ()
  "Each way the composer asks for is its own arm of the direction."
  (agent-repl-test-wire-verbs--with-common
    (dolist (case '((:next "next") (:previous "previous") (:following "following")))
      (should (equal (json-serialize
                      (cdr (assq 'direction
                                 (agent-repl-wire-encode-find-entry-neighbor-request
                                  (list :workspace agent-repl-test-wire-verbs--ref :label "3.1"
                                        :direction (list :arm (car case)))))))
                     (format "{\"%s\":{}}" (cadr case)))))))

(ert-deftest agent-repl-test-wire-verbs-entry-neighbor-direction-arms-pinned ()
  "EntryNeighborDirection's way oneof has exactly the arms encoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_find_entry_neighbor.pb.go" "EntryNeighborDirection")
                       #'string<)
                 '("following" "next" "previous"))))

(ert-deftest agent-repl-test-wire-verbs-find-entry-neighbor-refuses-a-missing-direction ()
  "The direction is REQUIRED."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-find-entry-neighbor-request
                   (list :workspace agent-repl-test-wire-verbs--ref :label "3.1"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-find-entry-neighbor-found-decodes ()
  "A found neighbor decodes with its label."
  (should (equal (plist-get (plist-get (agent-repl-test-wire-verbs--decode-quietly
                                        #'agent-repl-wire-decode-find-entry-neighbor-response
                                        "{\"success\":{\"found\":{\"label\":\"3.2\"}}}")
                                       :value)
                            :neighbor)
                 '(:arm :found :value (:label "3.2")))))

(ert-deftest agent-repl-test-wire-verbs-find-entry-neighbor-none-decodes ()
  "No neighbor is an answer, the `none' arm."
  (should (equal (plist-get (plist-get (agent-repl-test-wire-verbs--decode-quietly
                                        #'agent-repl-wire-decode-find-entry-neighbor-response
                                        "{\"success\":{\"none\":{}}}")
                                       :value)
                            :neighbor)
                 '(:arm :none :value nil))))

(ert-deftest agent-repl-test-wire-verbs-find-entry-neighbor-refuses-a-found-without-its-label ()
  "A found neighbor with no label is a contract breach."
  (should (agent-repl-test-wire-verbs--decode-breach
           #'agent-repl-wire-decode-find-entry-neighbor-response "{\"success\":{\"found\":{}}}")))

;;;; ---- SendHiddenKeepalives -------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-hidden-keepalives-request-is-empty ()
  "The request carries nothing: the call is the request."
  (should (equal (json-serialize
                  (or (agent-repl-test-wire-verbs--with-common
                        (agent-repl-wire-encode-send-hidden-keepalives-request))
                      (make-hash-table)))
                 "{}")))

(ert-deftest agent-repl-test-wire-verbs-hidden-keepalives-success-decodes-each-outcome ()
  "A success decodes every workspace's ref and outcome arm, in order."
  (should (equal (agent-repl-test-wire-verbs--decode-quietly
                  #'agent-repl-wire-decode-send-hidden-keepalives-response
                  (concat "{\"success\":{\"workspaces\":["
                          "{\"workspace\":{\"id\":\"a\",\"dir\":\"/w/a\"},\"sent\":{}},"
                          "{\"workspace\":{\"id\":\"b\",\"dir\":\"/w/b\"},\"notRunning\":{}},"
                          "{\"workspace\":{\"id\":\"c\",\"dir\":\"/w/c\"},\"failed\":{\"detail\":\"gone\"}},"
                          "{\"workspace\":{\"id\":\"d\",\"dir\":\"/w/d\"},\"skipped\":{\"busy\":{\"detail\":\"a turn\"}}},"
                          "{\"workspace\":{\"id\":\"e\",\"dir\":\"/w/e\"},\"skipped\":{\"detachedWorkLive\":{\"liveItems\":2}}},"
                          "{\"workspace\":{\"id\":\"f\",\"dir\":\"/w/f\"},\"skipped\":{\"cacheLapsed\":{\"lapsedAtMs\":\"1800000000000\"}}}"
                          "]}}"))
                 '(:arm :success
                   :value (:workspaces
                           ((:workspace (:id "a" :dir "/w/a") :outcome (:arm :sent :value nil))
                            (:workspace (:id "b" :dir "/w/b") :outcome (:arm :not-running :value nil))
                            (:workspace (:id "c" :dir "/w/c") :outcome (:arm :failed :value (:detail "gone")))
                            (:workspace (:id "d" :dir "/w/d")
                             :outcome (:arm :skipped :value (:reason (:arm :busy :value (:detail "a turn")))))
                            (:workspace (:id "e" :dir "/w/e")
                             :outcome (:arm :skipped :value (:reason (:arm :detached-work-live :value (:live-items 2)))))
                            (:workspace (:id "f" :dir "/w/f")
                             :outcome (:arm :skipped :value (:reason (:arm :cache-lapsed
                                                                     :value (:lapsed-at-ms 1800000000000)))))))))))

(ert-deftest agent-repl-test-wire-verbs-hidden-keepalives-empty-section-decodes-empty ()
  "An empty hidden section is protojson's `{}' success: no workspaces."
  (should (equal (agent-repl-test-wire-verbs--decode-quietly
                  #'agent-repl-wire-decode-send-hidden-keepalives-response
                  "{\"success\":{}}")
                 '(:arm :success :value (:workspaces nil)))))

(ert-deftest agent-repl-test-wire-verbs-hidden-keepalives-workspace-without-outcome-refused ()
  "A workspace entry naming no outcome is a breach."
  (should (equal (agent-repl-test-wire-verbs--decode-breach
                  #'agent-repl-wire-decode-send-hidden-keepalives-workspace
                  "{\"workspace\":{\"id\":\"a\",\"dir\":\"/w/a\"}}")
                 '("SendHiddenKeepalivesWorkspace" "outcome" "oneof is unset"))))

(ert-deftest agent-repl-test-wire-verbs-hidden-keepalives-unknown-field-refused ()
  "A field the codec does not hold is refused, not dropped."
  (should (equal (agent-repl-test-wire-verbs--decode-breach
                  #'agent-repl-wire-decode-send-hidden-keepalives-workspace
                  "{\"workspace\":{\"id\":\"a\",\"dir\":\"/w/a\"},\"sent\":{},\"extra\":1}")
                 '("SendHiddenKeepalivesWorkspace" "extra" "unknown field"))))

(ert-deftest agent-repl-test-wire-verbs-hidden-keepalives-error-decodes-its-cause ()
  "An unreadable registry decodes its cause and the read's words."
  (should (equal (agent-repl-test-wire-verbs--decode-quietly
                  #'agent-repl-wire-decode-send-hidden-keepalives-response
                  "{\"error\":{\"registryUnreadable\":{\"detail\":\"database is locked\"}}}")
                 '(:arm :error
                   :value (:cause (:arm :registry-unreadable
                                   :value (:detail "database is locked")))))))

;;;; ---- UpdatePersistentWifiMode ----------------------------------------

(ert-deftest agent-repl-test-wire-verbs-persistent-wifi-state-joined-with-a-name ()
  "A joined network with its name and the mode on decode arm for arm."
  (should (equal (agent-repl-test-wire-verbs--decode-quietly
                  #'agent-repl-wire-decode-persistent-wifi-state
                  "{\"joined\":{\"networkName\":\"Home\"},\"on\":{}}")
                 '(:wifi (:arm :joined :value (:network-name "Home"))
                   :mode (:arm :on :value nil)))))

(ert-deftest agent-repl-test-wire-verbs-persistent-wifi-state-withheld-name-is-nil ()
  "A joined network whose name macOS withholds decodes with no name."
  (should (equal (agent-repl-test-wire-verbs--decode-quietly
                  #'agent-repl-wire-decode-persistent-wifi-state
                  "{\"joined\":{},\"off\":{}}")
                 '(:wifi (:arm :joined :value (:network-name nil))
                   :mode (:arm :off :value nil)))))

(ert-deftest agent-repl-test-wire-verbs-persistent-wifi-state-unread-facts-are-nil ()
  "Facts the daemon could not read are unassigned oneofs, never a breach."
  (should (equal (agent-repl-test-wire-verbs--decode-quietly
                  #'agent-repl-wire-decode-persistent-wifi-state "{}")
                 '(:wifi nil :mode nil))))

(ert-deftest agent-repl-test-wire-verbs-persistent-wifi-state-unknown-field-refused ()
  "A field the codec does not hold is refused, not dropped."
  (should (equal (agent-repl-test-wire-verbs--decode-breach
                  #'agent-repl-wire-decode-persistent-wifi-state "{\"radio\":{}}")
                 '("PersistentWifiState" radio "unknown field"))))

(ert-deftest agent-repl-test-wire-verbs-persistent-wifi-request-encodes-each-action ()
  "Each action arm encodes as the arm alone."
  (dolist (case '((:on . "{\"on\":{}}") (:off . "{\"off\":{}}") (:toggle . "{\"toggle\":{}}")))
    (should (equal (json-serialize
                    (agent-repl-test-wire-verbs--with-common
                      (agent-repl-wire-encode-update-persistent-wifi-mode-request
                       (list :action (list :arm (car case))))))
                   (cdr case)))))

(ert-deftest agent-repl-test-wire-verbs-persistent-wifi-request-without-action-refused ()
  "A request naming no action errors before send."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-update-persistent-wifi-mode-request '(:action nil))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-persistent-wifi-success-decodes-every-step ()
  "A success decodes the re-read standing and every step's outcome."
  (should (equal (agent-repl-test-wire-verbs--decode-quietly
                  #'agent-repl-wire-decode-update-persistent-wifi-mode-response
                  (concat "{\"success\":{\"state\":{\"joined\":{},\"on\":{}},"
                          "\"hotspot\":{\"failed\":{\"networkName\":\"Phone\",\"detail\":\"not visible\"}},"
                          "\"energyMode\":{\"failed\":{\"detail\":\"refused\"}},"
                          "\"display\":{\"toolMissing\":{\"toolPath\":\"/b/mac-brightness\"}}}}"))
                 '(:arm :success
                   :value (:state (:wifi (:arm :joined :value (:network-name nil))
                                   :mode (:arm :on :value nil))
                           :hotspot (:arm :failed :value (:network-name "Phone" :detail "not visible"))
                           :energy-mode (:arm :failed :value (:detail "refused"))
                           :display (:arm :tool-missing :value (:tool-path "/b/mac-brightness")))))))

(ert-deftest agent-repl-test-wire-verbs-persistent-wifi-success-with-an-unread-state ()
  "A success whose standing could not be re-read carries the empty state."
  (should (equal (plist-get (plist-get (agent-repl-test-wire-verbs--decode-quietly
                                        #'agent-repl-wire-decode-update-persistent-wifi-mode-response
                                        (concat "{\"success\":{\"state\":{},"
                                                "\"hotspot\":{\"notOnHotspot\":{}},"
                                                "\"energyMode\":{\"highPower\":{}},"
                                                "\"display\":{\"restored\":{}}}}"))
                                       :value)
                            :state)
                 '(:wifi nil :mode nil))))

(ert-deftest agent-repl-test-wire-verbs-persistent-wifi-success-without-hotspot-refused ()
  "A success missing its required hotspot outcome is a breach."
  (should (equal (agent-repl-test-wire-verbs--decode-breach
                  #'agent-repl-wire-decode-update-persistent-wifi-mode-response
                  "{\"success\":{\"state\":{},\"display\":{\"dimmed\":{}}}}")
                 '("UpdatePersistentWifiModeSuccess" hotspot "required message field is absent"))))

(ert-deftest agent-repl-test-wire-verbs-persistent-wifi-success-without-energy-mode-refused ()
  "A success missing its required energy mode outcome is a breach."
  (should (equal (agent-repl-test-wire-verbs--decode-breach
                  #'agent-repl-wire-decode-update-persistent-wifi-mode-response
                  "{\"success\":{\"state\":{},\"hotspot\":{\"notOnHotspot\":{}},\"display\":{\"dimmed\":{}}}}")
                 '("UpdatePersistentWifiModeSuccess" energyMode "required message field is absent"))))

(ert-deftest agent-repl-test-wire-verbs-persistent-wifi-error-decodes-its-cause ()
  "A refused power step decodes its cause and the refusal's words."
  (should (equal (agent-repl-test-wire-verbs--decode-quietly
                  #'agent-repl-wire-decode-update-persistent-wifi-mode-response
                  "{\"error\":{\"powerSettingsRefused\":{\"detail\":\"sudo: a password is required\"}}}")
                 '(:arm :error
                   :value (:cause (:arm :power-settings-refused
                                   :value (:detail "sudo: a password is required")))))))

(ert-deftest agent-repl-test-wire-verbs-persistent-wifi-arm-with-an-unknown-field-refused ()
  "An outcome arm carrying a field its table does not name is refused."
  (should (equal (agent-repl-test-wire-verbs--decode-breach
                  #'agent-repl-wire-decode-update-persistent-wifi-mode-error
                  "{\"modeUnreadable\":{\"detail\":\"x\",\"code\":1}}")
                 '("UpdatePersistentWifiModeError.modeUnreadable" "code" "unknown field"))))

(ert-deftest agent-repl-test-wire-verbs-decode-step-outcome-reads-the-set-arm ()
  "A step's outcome decodes to its arm and string fields."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-verbs--decode-step-outcome
                    "M" 'step (agent-repl-test-wire-verbs--parse "{\"step\":{\"failed\":{\"detail\":\"x\"}}}")
                    "S" '((ok :ok nil) (failed :failed ((detail :detail)))))
                   '(:arm :failed :value (:detail "x"))))))

(ert-deftest agent-repl-test-wire-verbs-decode-step-outcome-refuses-an-absent-step ()
  "An absent step is a breach naming the parent message and field."
  (should (equal (agent-repl-test-wire-verbs--decode-breach
                  (lambda (json)
                    (agent-repl-wire-verbs--decode-step-outcome
                     "M" 'step json "S" '((ok :ok nil))))
                  "{}")
                 '("M" step "required message field is absent"))))

(ert-deftest agent-repl-test-wire-verbs-decode-step-outcome-refuses-an-unset-outcome ()
  "A step whose outcome sets no arm is a breach naming the step message."
  (should (equal (car (agent-repl-test-wire-verbs--decode-breach
                       (lambda (json)
                         (agent-repl-wire-verbs--decode-step-outcome
                          "M" 'step json "S" '((ok :ok nil))))
                       "{\"step\":{\"other\":{}}}"))
                 "S")))

(ert-deftest agent-repl-test-wire-verbs-persistent-wifi-success-reads-steps-through-one-helper ()
  "Every step field of the success decoder reads through the shared helper."
  (let ((calls nil))
    (cl-letf (((symbol-function 'agent-repl-wire-verbs--decode-step-outcome)
               (lambda (_message field &rest _) (push field calls) nil))
              ((symbol-function 'agent-repl-wire--decode-message) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl-wire-verbs--check-keys) #'ignore))
      (agent-repl-wire-decode-update-persistent-wifi-mode-success nil))
    (should (equal (sort (mapcar #'symbol-name calls) #'string<)
                   '("display" "energyMode" "hotspot")))))

(ert-deftest agent-repl-test-wire-verbs-decode-string-fields-reads-every-named-string ()
  "The all-strings decoder answers each named field, absent ones as empty."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-verbs--decode-string-fields
                    "M" (agent-repl-test-wire-verbs--parse "{\"a\":\"x\"}")
                    '((a :a) (b :b)))
                   '(:a "x" :b "")))))

(ert-deftest agent-repl-test-wire-verbs-persistent-wifi-arm-tables-pinned ()
  "Each outcome table names exactly the arms the frozen schema declares."
  (dolist (case `(("UpdatePersistentWifiModeHotspot" . ,agent-repl-wire-persistent-wifi-hotspot-arms)
                  ("UpdatePersistentWifiModeDisplay" . ,agent-repl-wire-persistent-wifi-display-arms)
                  ("UpdatePersistentWifiModeEnergyMode" . ,agent-repl-wire-persistent-wifi-energy-mode-arms)
                  ("UpdatePersistentWifiModeError" . ,agent-repl-wire-persistent-wifi-error-arms)))
    (should (equal (sort (agent-repl-test--generated-oneof-arms
                          "agentrepl/v1/endpoint_update_persistent_wifi_mode.pb.go" (car case))
                         #'string<)
                   (sort (mapcar (lambda (arm) (symbol-name (car arm))) (cdr case)) #'string<)))))

(ert-deftest agent-repl-test-wire-verbs-persistent-wifi-action-arms-pinned ()
  "UpdatePersistentWifiModeRequest's action oneof has exactly the arms encoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_update_persistent_wifi_mode.pb.go"
                        "UpdatePersistentWifiModeRequest")
                       #'string<)
                 '("off" "on" "toggle"))))

;;;; ---- UpdateQuestionDraft ----

(ert-deftest agent-repl-test-wire-verbs-update-question-draft-pick-shape ()
  "A pick echoes the card's row, the shown question and the option's label."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-update-question-draft-request
                     (list :workspace agent-repl-test-wire-verbs--ref
                           :question '(:value "row-q")
                           :edit '(:arm :pick :value (:question-text "Which auth?" :label "OAuth")))))
                   "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},\"question\":{\"value\":\"row-q\"},\"pick\":{\"questionText\":\"Which auth?\",\"label\":\"OAuth\"}}"))))

(ert-deftest agent-repl-test-wire-verbs-update-question-draft-confirm-shape ()
  "A confirm echoes the card's row and the shown question."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-update-question-draft-request
                     (list :workspace agent-repl-test-wire-verbs--ref
                           :question '(:value "row-q")
                           :edit '(:arm :confirm :value (:question-text "Which suites?")))))
                   "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},\"question\":{\"value\":\"row-q\"},\"confirm\":{\"questionText\":\"Which suites?\"}}"))))

(ert-deftest agent-repl-test-wire-verbs-update-question-draft-incomplete-requests-refused ()
  "Every field is REQUIRED, and the tab's `show' has no Emacs spelling."
  (agent-repl-test-wire-verbs--with-common
    (dolist (request (list (list :question '(:value "q") :edit '(:arm :confirm :value (:question-text "q?")))
                           (list :workspace agent-repl-test-wire-verbs--ref
                                 :edit '(:arm :confirm :value (:question-text "q?")))
                           (list :workspace agent-repl-test-wire-verbs--ref :question '(:value "q"))
                           (list :workspace agent-repl-test-wire-verbs--ref :question '(:value "q")
                                 :edit '(:arm :pick :value (:question-text "q?")))
                           (list :workspace agent-repl-test-wire-verbs--ref :question '(:value "q")
                                 :edit '(:arm :show :value (:question-text "q?")))))
      (should-error (agent-repl-wire-encode-update-question-draft-request request)
                    :type 'agent-repl-wire-error))))

(ert-deftest agent-repl-test-wire-verbs-update-question-draft-edit-arms-pinned ()
  "The request's edit oneof is exactly what the schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_update_question_draft.pb.go"
                        "UpdateQuestionDraftRequest")
                       #'string<)
                 '("confirm" "pick" "show"))))

(ert-deftest agent-repl-test-wire-verbs-update-question-draft-success-decodes ()
  "The success is empty: the card's new state is the feed's push."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-update-question-draft-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{}}"))
                   '(:arm :success :value nil)))))

(ert-deftest agent-repl-test-wire-verbs-update-question-draft-refusals-decode ()
  "Each refusal decodes as its arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-update-question-draft-response
                    (agent-repl-test-wire-verbs--parse "{\"error\":{\"askNotStanding\":{}}}"))
                   '(:arm :error :value (:cause (:arm :ask-not-standing :value nil)))))
    (should (equal (agent-repl-wire-decode-update-question-draft-response
                    (agent-repl-test-wire-verbs--parse "{\"error\":{\"unservedValue\":{\"text\":\"OAuth\"}}}"))
                   '(:arm :error :value (:cause (:arm :unserved-value :value (:text "OAuth"))))))
    (should (equal (agent-repl-wire-decode-update-question-draft-response
                    (agent-repl-test-wire-verbs--parse "{\"error\":{\"noSession\":{}}}"))
                   '(:arm :error :value (:cause (:arm :no-session :value nil)))))))

(ert-deftest agent-repl-test-wire-verbs-update-question-draft-error-arms-pinned ()
  "The error's cause oneof is exactly what the schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_update_question_draft.pb.go"
                        "UpdateQuestionDraftError")
                       #'string<)
                 '("askNotStanding" "noSession" "notYetAdopted" "transferringAway"
                   "unknownWorkspace" "unservedValue" "workspaceRefMismatch"))))

;;;; ---- AnswerPermission ----

(ert-deftest agent-repl-test-wire-verbs-answer-permission-request-shapes ()
  "Each button is its own empty arm beside the echoed row."
  (agent-repl-test-wire-verbs--with-common
    (dolist (case '((:allow-once . "allowOnce") (:allow-standing . "allowStanding") (:deny . "deny")))
      (should (equal (json-serialize
                      (agent-repl-wire-encode-answer-permission-request
                       (list :workspace agent-repl-test-wire-verbs--ref
                             :permission '(:value "row-p")
                             :answer (car case))))
                     (format "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},\"permission\":{\"value\":\"row-p\"},\"%s\":{}}"
                             (cdr case)))))))

(ert-deftest agent-repl-test-wire-verbs-answer-permission-incomplete-requests-refused ()
  "Every field is REQUIRED, and an unknown button is refused before the wire."
  (agent-repl-test-wire-verbs--with-common
    (dolist (request (list (list :permission '(:value "p") :answer :deny)
                           (list :workspace agent-repl-test-wire-verbs--ref :answer :deny)
                           (list :workspace agent-repl-test-wire-verbs--ref :permission '(:value "p"))
                           (list :workspace agent-repl-test-wire-verbs--ref :permission '(:value "p")
                                 :answer :maybe)))
      (should-error (agent-repl-wire-encode-answer-permission-request request)
                    :type 'agent-repl-wire-error))))

(ert-deftest agent-repl-test-wire-verbs-answer-permission-answer-arms-pinned ()
  "The request's answer oneof is exactly what the schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_answer_permission.pb.go"
                        "AnswerPermissionRequest")
                       #'string<)
                 '("allowOnce" "allowStanding" "deny"))))

(ert-deftest agent-repl-test-wire-verbs-answer-permission-responses-decode ()
  "The success is empty and each refusal decodes as its arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-answer-permission-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{}}"))
                   '(:arm :success :value nil)))
    (dolist (case '(("askNotStanding" . :ask-not-standing)
                    ("noStandingOffer" . :no-standing-offer)
                    ("noSession" . :no-session)))
      (should (equal (agent-repl-wire-decode-answer-permission-response
                      (agent-repl-test-wire-verbs--parse
                       (format "{\"error\":{\"%s\":{}}}" (car case))))
                     (list :arm :error :value (list :cause (list :arm (cdr case) :value nil))))))))

(ert-deftest agent-repl-test-wire-verbs-answer-permission-error-arms-pinned ()
  "The error's cause oneof is exactly what the schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_answer_permission.pb.go"
                        "AnswerPermissionError")
                       #'string<)
                 '("askNotStanding" "noSession" "noStandingOffer" "notYetAdopted"
                   "transferringAway" "unknownWorkspace" "workspaceRefMismatch"))))

;;;; ---- The family refusals (agentrepl.v1 family.proto) ----

(defconst agent-repl-test-wire-verbs--open-children-json
  "{\"hasOpenChildren\":{\"parent\":{\"id\":\"p\",\"dir\":\"/w/parent\"},\"children\":[{\"id\":\"c\",\"dir\":\"/w/child\"}]}}"
  "A `has_open_children' cause naming one parent and one child.")

(defconst agent-repl-test-wire-verbs--open-children-decoded
  '(:cause (:arm :has-open-children
            :value (:parent (:id "p" :dir "/w/parent") :children ((:id "c" :dir "/w/child")) :landing-into nil)))
  "What `agent-repl-test-wire-verbs--open-children-json' decodes to.")

(ert-deftest agent-repl-test-wire-verbs-every-has-open-children-arm-decodes-alike ()
  "Close, kill, nuke, finish and hide decode `has_open_children' with its parent and children."
  (agent-repl-test-wire-verbs--with-common
    (dolist (decoder '(agent-repl-wire-decode-close-workspace-error
                       agent-repl-wire-decode-kill-workspace-error
                       agent-repl-wire-decode-nuke-workspace-error
                       agent-repl-wire-decode-finish-workspace-error
                       agent-repl-wire-decode-set-workspace-hidden-error))
      (should (equal (funcall decoder (agent-repl-test-wire-verbs--parse agent-repl-test-wire-verbs--open-children-json))
                     agent-repl-test-wire-verbs--open-children-decoded)))))

(ert-deftest agent-repl-test-wire-verbs-has-open-children-naming-nobody-is-a-breach ()
  "The contract says the children and the workspaces landing into it are never both empty."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-close-workspace-error
                   (agent-repl-test-wire-verbs--parse
                    "{\"hasOpenChildren\":{\"parent\":{\"id\":\"p\",\"dir\":\"/w/p\"},\"children\":[],\"landingInto\":[]}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-has-open-children-naming-only-workspaces-landing-into-it ()
  "A workspace held open only by workspaces landing into it decodes them as `:landing-into'."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-close-workspace-error
                    (agent-repl-test-wire-verbs--parse
                     "{\"hasOpenChildren\":{\"parent\":{\"id\":\"p\",\"dir\":\"/w/p\"},\"landingInto\":[{\"id\":\"l\",\"dir\":\"/w/lander\"}]}}"))
                   '(:cause (:arm :has-open-children
                             :value (:parent (:id "p" :dir "/w/p") :children nil
                                     :landing-into ((:id "l" :dir "/w/lander")))))))))

(ert-deftest agent-repl-test-wire-verbs-every-parent-not-open-arm-decodes-each-state ()
  "Open, create and show decode `parent_not_open' with its parent and state."
  (agent-repl-test-wire-verbs--with-common
    (dolist (decoder '(agent-repl-wire-decode-open-workspace-error
                       agent-repl-wire-decode-create-workspace-error
                       agent-repl-wire-decode-set-workspace-hidden-error))
      (dolist (state '(("hidden" . :hidden) ("closed" . :closed) ("closing" . :closing)))
        (should (equal (funcall decoder (agent-repl-test-wire-verbs--parse
                                         (format "{\"parentNotOpen\":{\"parent\":{\"id\":\"p\",\"dir\":\"/w/p\"},\"%s\":{}}}"
                                                 (car state))))
                       (list :cause (list :arm :parent-not-open
                                          :value (list :parent '(:id "p" :dir "/w/p")
                                                       :state (list :arm (cdr state) :value nil))))))))))

(ert-deftest agent-repl-test-wire-verbs-parent-not-open-without-a-state-is-a-breach ()
  "The parent's state is never unset."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-open-workspace-error
                   (agent-repl-test-wire-verbs--parse "{\"parentNotOpen\":{\"parent\":{\"id\":\"p\",\"dir\":\"/w/p\"}}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-swap-error-not-siblings-arm ()
  "SwapWorkspacesError's `not_siblings' arm decodes, carrying nothing."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-swap-workspaces-error
                    (agent-repl-test-wire-verbs--parse "{\"notSiblings\":{}}"))
                   '(:cause (:arm :not-siblings :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-set-hidden-error-arms-pinned ()
  "SetWorkspaceHiddenError's arm set is exactly what the frozen schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_set_workspace_hidden.pb.go" "SetWorkspaceHiddenError")
                       #'string<)
                 (sort (list "unknownWorkspace" "workspaceRefMismatch" "transferringAway" "notYetAdopted"
                             "hasOpenChildren" "parentNotOpen")
                       #'string<))))

;;;; ---- The one-ref refusal decoder ------------------------------------

(ert-deftest agent-repl-test-wire-verbs-decode-ref-arm-keys-the-field ()
  "A one-ref arm decodes to its field's keyword over the ref."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-verbs--decode-ref-arm
                    "M" 'owner (agent-repl-test-wire-verbs--parse
                                "{\"owner\":{\"id\":\"ws-2\",\"dir\":\"/w/two\"}}"))
                   '(:owner (:id "ws-2" :dir "/w/two"))))))

(ert-deftest agent-repl-test-wire-verbs-decode-ref-arm-refuses-another-field ()
  "A one-ref arm carrying any other field is a contract breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-verbs--decode-ref-arm
                   "M" 'owner (agent-repl-test-wire-verbs--parse "{\"workspace\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-one-ref-arms-share-the-decoder ()
  "Every refusal arm naming one workspace decodes through the one helper."
  (dolist (fn '(agent-repl-wire-decode-create-workspace-unknown-fork-source
                agent-repl-wire-decode-create-workspace-unknown-context-workspace
                agent-repl-wire-decode-create-workspace-fork-source-has-no-conversation
                agent-repl-wire-decode-create-workspace-error-unknown-owner
                agent-repl-wire-decode-create-workspace-error-one-shot-owner-lands
                agent-repl-wire-decode-create-workspace-error-unknown-requester
                agent-repl-wire-decode-finish-workspace-error-owner-lands))
    (should (string-match-p "agent-repl-wire-verbs--decode-ref-arm"
                            (prin1-to-string (symbol-function fn))))))

(ert-deftest agent-repl-test-wire-verbs-empty-arm-decodes-an-empty-message ()
  "An empty-arm decoder answers nil for the empty JSON object."
  (agent-repl-test-wire-verbs--with-common
    (should (null (funcall (agent-repl-wire-verbs--empty-arm "SomeEmptyArm") nil)))))

(ert-deftest agent-repl-test-wire-verbs-empty-arm-refuses-a-field ()
  "An empty-arm decoder refuses any field as unknown, naming its message."
  (agent-repl-test-wire-verbs--with-common
    (should-error (funcall (agent-repl-wire-verbs--empty-arm "SomeEmptyArm") '((stray . "x")))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-empty-arms-share-one-decoder ()
  "Every empty arm in the verbs codec is decoded through
`agent-repl-wire-verbs--empty-arm', so no site hand-rolls a lambda around
`agent-repl-wire-verbs--decode-empty' that could drift from it."
  (let ((source (with-temp-buffer
                  (insert-file-contents
                   (concat (file-name-sans-extension
                            (symbol-file 'agent-repl-wire-verbs--empty-arm 'defun))
                           ".el"))
                  (buffer-string))))
    (should-not (string-match-p
                 "(lambda (\\([a-z]+\\))[ \t\n]+(agent-repl-wire-verbs--decode-empty[ \t\n]+\"[A-Za-z]+\"[ \t\n]+\\1))"
                 source))))

(ert-deftest agent-repl-test-wire-verbs-submit-error-thread-awaiting-first-answer-arm ()
  "SubmitPromptError's `thread_awaiting_first_answer' arm decodes as its keyword."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-error
                    (agent-repl-test-wire-verbs--parse "{\"threadAwaitingFirstAnswer\":{}}"))
                   '(:reason (:arm :thread-awaiting-first-answer :value nil))))))

;;;; ---- UpdateHeldPrompt ------------------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-update-held-prompt-release-request-shape ()
  "A release carries the echoed ref, the echoed turn and the empty release arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-update-held-prompt-request
                     (list :workspace agent-repl-test-wire-verbs--ref
                           :turn '(:value "t-1")
                           :action '(:arm :release :value nil))))
                   "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},\"turn\":{\"value\":\"t-1\"},\"release\":{}}"))))

(ert-deftest agent-repl-test-wire-verbs-update-held-prompt-missing-turn-refused ()
  "The turn is REQUIRED: it names which held prompt."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-update-held-prompt-request
                   (list :workspace agent-repl-test-wire-verbs--ref
                         :action '(:arm :release :value nil)))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-update-held-prompt-unknown-action-refused ()
  "The action vocabulary is closed."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-update-held-prompt-request
                   (list :workspace agent-repl-test-wire-verbs--ref
                         :turn '(:value "t-1")
                         :action '(:arm :accept :value nil)))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-update-held-prompt-success-decodes ()
  "An empty success decodes as the success arm."
  (should (eq (plist-get (agent-repl-wire-decode-update-held-prompt-response
                          (agent-repl-test-wire-verbs--parse "{\"success\":{}}"))
                         :arm)
              :success)))

(ert-deftest agent-repl-test-wire-verbs-update-held-prompt-refusal-arms-decode ()
  "Each refusal arm decodes as its keyword."
  (dolist (case '(("noSuchHold" . :no-such-hold)
                  ("alreadyDelivered" . :already-delivered)
                  ("releaseRefused" . :release-refused)
                  ("beingDelivered" . :being-delivered)))
    (should (equal (agent-repl-wire-decode-update-held-prompt-error
                    (agent-repl-test-wire-verbs--parse (format "{\"%s\":{}}" (car case))))
                   (list :cause (list :arm (cdr case) :value nil))))))
