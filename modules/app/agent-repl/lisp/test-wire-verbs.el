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
Also silences core.el's logging ladder: `agent-repl--error' signals after
persisting, and the suite asserts the SIGNAL, not the log file."
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
             ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
             ((symbol-function 'agent-repl--error)
              (lambda (&rest _) (error "stubbed agent-repl--error"))))
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
    ("MergeWorkspace"
     agent-repl-wire-encode-merge-workspace-request
     agent-repl-wire-decode-merge-workspace-response))
  "The verbs whose request is {workspace} and whose result arms are both empty.")

(defconst agent-repl-test-wire-verbs--result-oneofs
  '(("agentrepl/v1/endpoint_create_workspace.pb.go" "CreateWorkspaceResponse")
    ("agentrepl/v1/endpoint_open_workspace.pb.go" "OpenWorkspaceResponse")
    ("agentrepl/v1/endpoint_close_workspace.pb.go" "CloseWorkspaceResponse")
    ("agentrepl/v1/endpoint_kill_workspace.pb.go" "KillWorkspaceResponse")
    ("agentrepl/v1/endpoint_nuke_workspace.pb.go" "NukeWorkspaceResponse")
    ("agentrepl/v1/endpoint_merge_workspace.pb.go" "MergeWorkspaceResponse")
    ("agentrepl/v1/endpoint_restart_workspace.pb.go" "RestartWorkspaceResponse")
    ("agentrepl/v1/endpoint_set_workspace_priority.pb.go" "SetWorkspacePriorityResponse")
    ("agentrepl/v1/endpoint_submit_prompt.pb.go" "SubmitPromptResponse")
    ("agentrepl/v1/endpoint_update_shutdown_schedule.pb.go" "UpdateShutdownScheduleResponse")
    ("agentrepl/v1/endpoint_update_merge_queue.pb.go" "UpdateMergeQueueResponse")
    ("agentrepl/v1/endpoint_daemon_health.pb.go" "DaemonHealthResponse")
    ("agentrepl/v1/endpoint_session_health.pb.go" "SessionHealthResponse"))
  "Every response whose result oneof this codec decodes, with its binding.")


;;;; ---- CreateWorkspaceRequest: the standard form ----------------------

(ert-deftest agent-repl-test-wire-verbs-create-standard-minimal ()
  "A standard create with no optional facts carries repository and an empty form."
  (agent-repl-test-wire-verbs--with-common
    (let* ((request (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard :value nil)))
           (encoded (agent-repl-wire-encode-create-workspace-request request)))
      (should (equal (json-serialize encoded)
                     "{\"repository\":{\"id\":\"repo-1\",\"dir\":\"/r/one\"},\"standard\":{}}")))))

(ert-deftest agent-repl-test-wire-verbs-create-standard-initial-prompt ()
  "A standard create's initial prompt encodes through the UserSaid codec."
  (agent-repl-test-wire-verbs--with-common
    (let* ((request (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard
                                  :value (:initial-prompt (:text "go")))))
           (encoded (agent-repl-wire-encode-create-workspace-request request)))
      (should (equal (cdr (assq 'initialPrompt (cdr (assq 'standard encoded))))
                     '((said . "go")))))))

(ert-deftest agent-repl-test-wire-verbs-create-standard-base-ref ()
  "A standard create's base ref rides as the lowerCamel `baseRef' key."
  (agent-repl-test-wire-verbs--with-common
    (let* ((request (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard :value (:base-ref "origin/main"))))
           (encoded (agent-repl-wire-encode-create-workspace-request request)))
      (should (equal (cdr (assq 'baseRef (cdr (assq 'standard encoded)))) "origin/main")))))

(ert-deftest agent-repl-test-wire-verbs-create-standard-name ()
  "A caller-supplied workspace name rides the standard form."
  (agent-repl-test-wire-verbs--with-common
    (let* ((request (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard :value (:name "wire-b"))))
           (encoded (agent-repl-wire-encode-create-workspace-request request)))
      (should (equal (cdr (assq 'name (cdr (assq 'standard encoded)))) "wire-b")))))

(ert-deftest agent-repl-test-wire-verbs-create-standard-absent-optionals ()
  "Absent standard-form optionals are OMITTED, never spelled as sentinels."
  (agent-repl-test-wire-verbs--with-common
    (let* ((request (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard :value nil)))
           (standard (cdr (assq 'standard
                                (agent-repl-wire-encode-create-workspace-request request)))))
      (should (null standard)))))

(ert-deftest agent-repl-test-wire-verbs-create-merge-actions-before ()
  "A configured pre-merge action rides as `beforeWsMerge'."
  (agent-repl-test-wire-verbs--with-common
    (let* ((request (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard
                                  :value (:merge-actions
                                          (:before-ws-merge (:text "lint"))))))
           (actions (cdr (assq 'mergeActions
                               (cdr (assq 'standard
                                          (agent-repl-wire-encode-create-workspace-request
                                           request)))))))
      (should (equal actions '((beforeWsMerge . ((said . "lint")))))))))

(ert-deftest agent-repl-test-wire-verbs-create-merge-actions-postprocessing ()
  "A configured post-merge action rides as `postprocessingPrompt'."
  (agent-repl-test-wire-verbs--with-common
    (let* ((request (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard
                                  :value (:merge-actions
                                          (:postprocessing-prompt (:text "tidy"))))))
           (actions (cdr (assq 'mergeActions
                               (cdr (assq 'standard
                                          (agent-repl-wire-encode-create-workspace-request
                                           request)))))))
      (should (equal actions '((postprocessingPrompt . ((said . "tidy")))))))))


;;;; ---- CreateWorkspaceRequest: the one-shot form ----------------------

(ert-deftest agent-repl-test-wire-verbs-create-one-shot-self-merge ()
  "A one-shot finishing by self-merge encodes an empty `selfMerge' arm."
  (agent-repl-test-wire-verbs--with-common
    (let* ((request (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :one-shot
                                  :value (:prompt (:text "ship it")
                                          :finish (:arm :self-merge :value nil)))))
           (one-shot (cdr (assq 'oneShot
                                (agent-repl-wire-encode-create-workspace-request request)))))
      (should (equal (json-serialize one-shot)
                     "{\"prompt\":{\"said\":\"ship it\"},\"selfMerge\":{}}")))))

(ert-deftest agent-repl-test-wire-verbs-create-one-shot-open-pr-flags ()
  "A one-shot's open-pr flags ride as `selfCertified' and `addToMergeQueue'."
  (agent-repl-test-wire-verbs--with-common
    (let* ((request (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :one-shot
                                  :value (:prompt (:text "ship it")
                                          :finish (:arm :open-pr
                                                   :value (:self-certified t
                                                           :add-to-merge-queue t))))))
           (open-pr (cdr (assq 'openPr
                               (cdr (assq 'oneShot
                                          (agent-repl-wire-encode-create-workspace-request
                                           request)))))))
      (should (equal open-pr '((selfCertified . t) (addToMergeQueue . t)))))))

(ert-deftest agent-repl-test-wire-verbs-create-one-shot-open-pr-false-explicit ()
  "Unset open-pr flags are spelled explicitly false, never left ambiguous."
  (agent-repl-test-wire-verbs--with-common
    (let* ((request (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :one-shot
                                  :value (:prompt (:text "ship it")
                                          :finish (:arm :open-pr :value nil)))))
           (open-pr (cdr (assq 'openPr
                               (cdr (assq 'oneShot
                                          (agent-repl-wire-encode-create-workspace-request
                                           request)))))))
      (should (equal (json-serialize open-pr)
                     "{\"selfCertified\":false,\"addToMergeQueue\":false}")))))

(ert-deftest agent-repl-test-wire-verbs-create-one-shot-without-prompt-refused ()
  "A one-shot IS its prompt: a one-shot without one never reaches the wire."
  (agent-repl-test-wire-verbs--with-common
    (should-error
     (agent-repl-wire-encode-create-workspace-request
      (list :repository agent-repl-test-wire-verbs--repo
            :form '(:arm :one-shot :value (:finish (:arm :self-merge :value nil)))))
     :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-create-one-shot-without-finish-refused ()
  "The finish arm IS the finish action, so an unset finish oneof is refused."
  (agent-repl-test-wire-verbs--with-common
    (should-error
     (agent-repl-wire-encode-create-workspace-request
      (list :repository agent-repl-test-wire-verbs--repo
            :form '(:arm :one-shot :value (:prompt (:text "ship it")))))
     :type 'agent-repl-wire-error)))


;;;; ---- CreateWorkspaceRequest: shared facts and refusals --------------

(ert-deftest agent-repl-test-wire-verbs-create-without-form-refused ()
  "The form arm IS the creation form, so a request without one is refused."
  (agent-repl-test-wire-verbs--with-common
    (should-error
     (agent-repl-wire-encode-create-workspace-request
      (list :repository agent-repl-test-wire-verbs--repo))
     :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-create-without-repository-refused ()
  "A create with no repository is incomplete and errors before send."
  (agent-repl-test-wire-verbs--with-common
    (should-error
     (agent-repl-wire-encode-create-workspace-request '(:form (:arm :standard :value nil)))
     :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-create-unknown-form-arm-refused ()
  "An arm keyword the form oneof does not declare is refused."
  (agent-repl-test-wire-verbs--with-common
    (should-error
     (agent-repl-wire-encode-create-workspace-request
      (list :repository agent-repl-test-wire-verbs--repo
            :form '(:arm :rehydrate :value nil)))
     :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-create-parent-without-fork ()
  "A parent without a fork nests the child but starts a fresh conversation."
  (agent-repl-test-wire-verbs--with-common
    (let* ((request (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard :value nil)
                          :parent (list :workspace agent-repl-test-wire-verbs--ref)))
           (parent (cdr (assq 'parent
                              (agent-repl-wire-encode-create-workspace-request request)))))
      (should (equal (json-serialize parent)
                     "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"}}")))))

(ert-deftest agent-repl-test-wire-verbs-create-parent-with-fork ()
  "Presence IS the fork fact, so a set fork is the empty message."
  (agent-repl-test-wire-verbs--with-common
    (let* ((request (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard :value nil)
                          :parent (list :workspace agent-repl-test-wire-verbs--ref
                                        :fork t)))
           (parent (cdr (assq 'parent
                              (agent-repl-wire-encode-create-workspace-request request)))))
      (should (equal (json-serialize parent)
                     (concat "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},"
                             "\"fork\":{}}"))))))

(ert-deftest agent-repl-test-wire-verbs-create-parent-without-workspace-refused ()
  "A parent block with no parent workspace is incomplete and refused."
  (agent-repl-test-wire-verbs--with-common
    (should-error
     (agent-repl-wire-encode-create-workspace-request
      (list :repository agent-repl-test-wire-verbs--repo
            :form '(:arm :standard :value nil)
            :parent '(:fork t)))
     :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-create-model-present ()
  "A chosen session model rides the request."
  (agent-repl-test-wire-verbs--with-common
    (let ((encoded (agent-repl-wire-encode-create-workspace-request
                    (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard :value nil)
                          :model "opus"))))
      (should (equal (cdr (assq 'model encoded)) "opus")))))

(ert-deftest agent-repl-test-wire-verbs-create-model-absent-omitted ()
  "An absent model omits the key: the daemon's default, not an empty string."
  (agent-repl-test-wire-verbs--with-common
    (let ((encoded (agent-repl-wire-encode-create-workspace-request
                    (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard :value nil)))))
      (should-not (assq 'model encoded)))))

(ert-deftest agent-repl-test-wire-verbs-create-priority-present ()
  "A creation-time priority delegates to the WorkspacePriority codec."
  (agent-repl-test-wire-verbs--with-common
    (let ((encoded (agent-repl-wire-encode-create-workspace-request
                    (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard :value nil)
                          :priority '(:arm :p1 :value nil)))))
      (should (equal (cdr (assq 'priority encoded)) '((level . ":p1")))))))

(ert-deftest agent-repl-test-wire-verbs-create-priority-absent-omitted ()
  "An absent priority omits the key: unprioritized, never a sentinel level."
  (agent-repl-test-wire-verbs--with-common
    (let ((encoded (agent-repl-wire-encode-create-workspace-request
                    (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard :value nil)))))
      (should-not (assq 'priority encoded)))))

(ert-deftest agent-repl-test-wire-verbs-create-allow-ungated-present ()
  "Presence IS the ungated consent, so it rides as the empty message."
  (agent-repl-test-wire-verbs--with-common
    (let ((encoded (agent-repl-wire-encode-create-workspace-request
                    (list :repository agent-repl-test-wire-verbs--repo
                          :form '(:arm :standard :value nil)
                          :allow-ungated t))))
      (should (equal (json-serialize (list (assq 'allowUngated encoded)))
                     "{\"allowUngated\":{}}")))))

(ert-deftest agent-repl-test-wire-verbs-create-allow-ungated-absent-omitted ()
  "Absent consent omits the key entirely: no consent was given."
  (agent-repl-test-wire-verbs--with-common
    (let ((encoded (agent-repl-wire-encode-create-workspace-request
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

(ert-deftest agent-repl-test-wire-verbs-create-response-error-empty ()
  "The empty CreateWorkspaceError decodes to the arm with a nil value."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-response
                    (agent-repl-test-wire-verbs--parse "{\"error\":{}}"))
                   '(:arm :error :value nil)))))

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


;;;; ---- Arm lists pinned against the generated Go bindings --------------

(ert-deftest agent-repl-test-wire-verbs-create-form-arms-pinned ()
  "CreateWorkspaceRequest's form oneof has exactly the two arms encoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_create_workspace.pb.go"
                        "CreateWorkspaceRequest")
                       #'string<)
                 '("oneShot" "standard"))))

(ert-deftest agent-repl-test-wire-verbs-one-shot-finish-arms-pinned ()
  "CreateWorkspaceOneShot's finish oneof has exactly the two arms encoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_create_workspace.pb.go"
                        "CreateWorkspaceOneShot")
                       #'string<)
                 '("openPr" "selfMerge"))))

(provide 'test-wire-verbs)

;;; test-wire-verbs.el ends here
