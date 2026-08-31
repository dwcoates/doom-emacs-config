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
  "A standard create with no optional facts carries repository and an empty
form."
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


;;;; ---- The {workspace}-request / empty-result verbs -------------------

(ert-deftest agent-repl-test-wire-verbs-simple-request-echoes-ref ()
  "Each simple verb's request echoes the WorkspaceRef verbatim."
  (agent-repl-test-wire-verbs--with-common
    (dolist (verb agent-repl-test-wire-verbs--simple-verbs)
      (should (equal (funcall (nth 1 verb)
                              (list :workspace agent-repl-test-wire-verbs--ref))
                     '((workspace . ((id . "ws-1") (dir . "/w/one")))))))))

(ert-deftest agent-repl-test-wire-verbs-simple-request-without-workspace ()
  "Each simple verb refuses a request with no workspace."
  (agent-repl-test-wire-verbs--with-common
    (dolist (verb agent-repl-test-wire-verbs--simple-verbs)
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
                    (list :workspace agent-repl-test-wire-verbs--ref))
                   '((workspace . ((id . "ws-1") (dir . "/w/one"))))))))

(ert-deftest agent-repl-test-wire-verbs-close-response-blocked ()
  "A blocked close decodes to the cause arm; the reasons ride the footer."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-close-workspace-response
                    (agent-repl-test-wire-verbs--parse "{\"error\":{\"blocked\":{}}}"))
                   '(:arm :error :value (:cause (:arm :blocked :value nil)))))))

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
  "A quiet close decodes to the empty success arm."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-close-workspace-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{}}"))
                   '(:arm :success :value nil)))))


;;;; ---- RestartWorkspace ------------------------------------------------

(ert-deftest agent-repl-test-wire-verbs-restart-force-true ()
  "A forced restart states force explicitly on the wire."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-restart-workspace-request
                     (list :workspace agent-repl-test-wire-verbs--ref :force t)))
                   "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},\"force\":true}"))))

(ert-deftest agent-repl-test-wire-verbs-restart-force-false-explicit ()
  "A graceful restart still SPELLS force, rather than omitting the default."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-restart-workspace-request
                     (list :workspace agent-repl-test-wire-verbs--ref)))
                   "{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"},\"force\":false}"))))

(ert-deftest agent-repl-test-wire-verbs-restart-without-workspace ()
  "A restart with no workspace is incomplete and errors before send."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-restart-workspace-request '(:force t))
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
                     (list :action
                           (list :arm :pause
                                 :value (list :repository
                                              agent-repl-test-wire-verbs--repo)))))
                   (concat "{\"pause\":{\"repository\":"
                           "{\"id\":\"repo-1\",\"dir\":\"/r/one\"}}}")))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-resume-names-its-repository ()
  "A resume scoped to one repository carries the same optional ref."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-update-merge-queue-request
                     (list :action
                           (list :arm :resume
                                 :value (list :repository
                                              agent-repl-test-wire-verbs--repo)))))
                   (concat "{\"resume\":{\"repository\":"
                           "{\"id\":\"repo-1\",\"dir\":\"/r/one\"}}}")))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-pause ()
  "Pause with no repository is the DAEMON-WIDE switch: the field is omitted,
never sent as an empty ref, because unset is what means every queue."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize (agent-repl-wire-encode-update-merge-queue-request
                                    '(:action (:arm :pause :value nil))))
                   "{\"pause\":{}}"))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-resume ()
  "Resume with no repository is the daemon-wide switch, field omitted."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize (agent-repl-wire-encode-update-merge-queue-request
                                    '(:action (:arm :resume :value nil))))
                   "{\"resume\":{}}"))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-evict ()
  "An eviction names whose merge to take off the queue."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (json-serialize
                    (agent-repl-wire-encode-update-merge-queue-request
                     (list :action (list :arm :evict
                                         :value (list :workspace
                                                      agent-repl-test-wire-verbs--ref)))))
                   "{\"evict\":{\"workspace\":{\"id\":\"ws-1\",\"dir\":\"/w/one\"}}}"))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-evict-without-workspace ()
  "An eviction with no workspace is incomplete and errors before send."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-encode-update-merge-queue-request
                   '(:action (:arm :evict :value nil)))
                  :type 'agent-repl-wire-error)))

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
                   '(:arm :success :value (:arm :healthy :value nil))))))

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
                                              :value (:sink "emacs")))))))))))

(ert-deftest agent-repl-test-wire-verbs-daemon-health-unhealthy-no-faults ()
  "An omitted repeated field is the empty list, protojson's `no elements'."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-daemon-health-response
                    (agent-repl-test-wire-verbs--parse "{\"success\":{\"unhealthy\":{}}}"))
                   '(:arm :success :value (:arm :unhealthy :value (:faults nil)))))))

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
  "Every response's result oneof has exactly the two arms this codec decodes."
  (dolist (entry agent-repl-test-wire-verbs--result-oneofs)
    (should (equal (sort (agent-repl-test--generated-oneof-arms (nth 0 entry) (nth 1 entry))
                         #'string<)
                   '("error" "success")))))

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

(ert-deftest agent-repl-test-wire-verbs-close-cause-arms-pinned ()
  "CloseWorkspaceError's cause oneof has exactly the arms this codec decodes."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_close_workspace.pb.go"
                        "CloseWorkspaceError")
                       #'string<)
                 '("blocked" "notYetAdopted" "transferringAway"
                   "unknownWorkspace" "workspaceRefMismatch"))))

(ert-deftest agent-repl-test-wire-verbs-submit-outcome-arms-pinned ()
  "SubmitPromptSuccess's outcome oneof has exactly the three arms decoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_submit_prompt.pb.go"
                        "SubmitPromptSuccess")
                       #'string<)
                 '("commandPanel" "commandRefused" "turn"))))

(ert-deftest agent-repl-test-wire-verbs-submit-panel-arms-pinned ()
  "SubmitPromptCommandPanel's panel oneof has exactly the six arms decoded
here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_submit_prompt.pb.go"
                        "SubmitPromptCommandPanel")
                       #'string<)
                 '("agents" "context" "help" "mcp" "status" "todos"))))

(ert-deftest agent-repl-test-wire-verbs-submit-reason-arms-pinned ()
  "SubmitPromptError's reason oneof has exactly the arms this codec decodes."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_submit_prompt.pb.go"
                        "SubmitPromptError")
                       #'string<)
                 '("feedNotInWorkspace" "feedUndecodable" "merging" "noSession"
                   "notYetAdopted" "transferringAway" "turnAlreadyOpen"
                   "unknownWorkspace" "workspaceRefMismatch"))))

(ert-deftest agent-repl-test-wire-verbs-shutdown-action-arms-pinned ()
  "UpdateShutdownScheduleRequest's action oneof has exactly the three arms
encoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_update_shutdown_schedule.pb.go"
                        "UpdateShutdownScheduleRequest")
                       #'string<)
                 '("cancel" "now" "schedule"))))

(ert-deftest agent-repl-test-wire-verbs-merge-queue-action-arms-pinned ()
  "UpdateMergeQueueRequest's action oneof has exactly the three arms encoded
here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_update_merge_queue.pb.go"
                        "UpdateMergeQueueRequest")
                       #'string<)
                 '("evict" "pause" "resume"))))

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

(ert-deftest agent-repl-test-wire-verbs-create-error-finish-required-arm ()
  "CreateWorkspaceError's `finish_required' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"finishRequired\":{}}"))
                   '(:cause (:arm :finish-required :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-finish-not-one-shot-arm ()
  "CreateWorkspaceError's `finish_not_one_shot' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"finishNotOneShot\":{}}"))
                   '(:cause (:arm :finish-not-one-shot :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-fork-parent-has-no-conversation-arm ()
  "CreateWorkspaceError's `fork_parent_has_no_conversation' arm decodes with
everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"forkParentHasNoConversation\":{}}"))
                   '(:cause (:arm :fork-parent-has-no-conversation :value nil))))))

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

(ert-deftest agent-repl-test-wire-verbs-create-error-unknown-parent-arm ()
  "CreateWorkspaceError's `unknown_parent' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"unknownParent\":{}}"))
                   '(:cause (:arm :unknown-parent :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-base-ref-unresolved-arm ()
  "CreateWorkspaceError's `base_ref_unresolved' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"baseRefUnresolved\":{\"ref\":\"origin/main\"}}"))
                   '(:cause (:arm :base-ref-unresolved :value (:ref "origin/main")))))))

(ert-deftest agent-repl-test-wire-verbs-create-error-worktree-creation-failed-arm ()
  "CreateWorkspaceError's `worktree_creation_failed' arm decodes with
everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-create-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"worktreeCreationFailed\":{\"detail\":\"fatal: exists\"}}"))
                   '(:cause (:arm :worktree-creation-failed :value (:detail "fatal: exists")))))))

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

(ert-deftest agent-repl-test-wire-verbs-create-error-arms-pinned ()
  "CreateWorkspaceError's arm set is exactly what the frozen schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_create_workspace.pb.go" "CreateWorkspaceError")
                       #'string<)
                 (sort (list "ungatedWithoutConsent" "noSlug" "finishRequired" "finishNotOneShot" "forkParentHasNoConversation" "briefMissing" "unknownRepository" "unknownParent" "baseRefUnresolved" "worktreeCreationFailed")
                       #'string<))))

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
                 (sort (list "unknownWorkspace" "workspaceRefMismatch" "transferringAway" "notYetAdopted" "sessionDeleted" "transcriptMissing" "spawnFailed")
                       #'string<))))

(ert-deftest agent-repl-test-wire-verbs-close-error-blocked-arm ()
  "CloseWorkspaceError's `blocked' arm decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-close-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"blocked\":{}}"))
                   '(:cause (:arm :blocked :value nil))))))

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
                 (sort (list "blocked" "unknownWorkspace" "workspaceRefMismatch" "transferringAway" "notYetAdopted")
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
                 (sort (list "unknownWorkspace" "workspaceRefMismatch" "transferringAway" "notYetAdopted")
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
                 (sort (list "unknownWorkspace" "workspaceRefMismatch" "transferringAway" "notYetAdopted" "gitFailed")
                       #'string<))))

(ert-deftest agent-repl-test-wire-verbs-merge-error-unknown-workspace-arm ()
  "MergeWorkspaceError's `unknown_workspace' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-merge-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"unknownWorkspace\":{}}"))
                   '(:cause (:arm :unknown-workspace :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-merge-error-workspace-ref-mismatch-arm ()
  "MergeWorkspaceError's `workspace_ref_mismatch' arm decodes with everything
it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-merge-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"workspaceRefMismatch\":{\"registryDir\":\"/w/registry\"}}"))
                   '(:cause (:arm :workspace-ref-mismatch :value (:registry-dir "/w/registry")))))))

(ert-deftest agent-repl-test-wire-verbs-merge-error-transferring-away-arm ()
  "MergeWorkspaceError's `transferring_away' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-merge-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"transferringAway\":{\"address\":\"127.0.0.1:9999\"}}"))
                   '(:cause (:arm :transferring-away :value (:address "127.0.0.1:9999")))))))

(ert-deftest agent-repl-test-wire-verbs-merge-error-not-yet-adopted-arm ()
  "MergeWorkspaceError's `not_yet_adopted' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-merge-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"notYetAdopted\":{}}"))
                   '(:cause (:arm :not-yet-adopted :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-merge-error-no-layout-facts-arm ()
  "MergeWorkspaceError's `no_layout_facts' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-merge-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"noLayoutFacts\":{}}"))
                   '(:cause (:arm :no-layout-facts :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-merge-error-session-deleted-arm ()
  "MergeWorkspaceError's `session_deleted' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-merge-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"sessionDeleted\":{}}"))
                   '(:cause (:arm :session-deleted :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-merge-error-already-queued-arm ()
  "MergeWorkspaceError's `already_queued' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-merge-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"alreadyQueued\":{}}"))
                   '(:cause (:arm :already-queued :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-merge-error-already-merging-arm ()
  "MergeWorkspaceError's `already_merging' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-merge-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"alreadyMerging\":{}}"))
                   '(:cause (:arm :already-merging :value nil))))))

(ert-deftest agent-repl-test-wire-verbs-merge-error-unset-cause-is-a-breach ()
  "MergeWorkspaceError with no arm set says nothing actionable, so it is a
breach."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-merge-workspace-error (agent-repl-test-wire-verbs--parse "{}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-merge-error-unknown-arm-is-a-breach ()
  "An arm MergeWorkspaceError does not declare here is refused, never guessed
at."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-merge-workspace-error
                   (agent-repl-test-wire-verbs--parse "{\"noSuchArm\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-merge-error-arms-pinned ()
  "MergeWorkspaceError's arm set is exactly what the frozen schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_merge_workspace.pb.go" "MergeWorkspaceError")
                       #'string<)
                 (sort (list "unknownWorkspace" "workspaceRefMismatch" "transferringAway" "notYetAdopted" "noLayoutFacts" "sessionDeleted" "alreadyQueued" "alreadyMerging")
                       #'string<))))

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

(ert-deftest agent-repl-test-wire-verbs-restart-error-no-session-arm ()
  "RestartWorkspaceError's `no_session' arm decodes with everything it carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-restart-workspace-error
                    (agent-repl-test-wire-verbs--parse "{\"noSession\":{}}"))
                   '(:cause (:arm :no-session :value nil))))))

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
                 (sort (list "unknownWorkspace" "workspaceRefMismatch" "transferringAway" "notYetAdopted" "noSession")
                       #'string<))))

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

(ert-deftest agent-repl-test-wire-verbs-submit-error-turn-already-open-arm ()
  "SubmitPromptError's `turn_already_open' arm decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-submit-prompt-error
                    (agent-repl-test-wire-verbs--parse "{\"turnAlreadyOpen\":{}}"))
                   '(:reason (:arm :turn-already-open :value nil))))))

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

(ert-deftest agent-repl-test-wire-verbs-submit-error-arms-pinned ()
  "SubmitPromptError's arm set is exactly what the frozen schema declares."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_submit_prompt.pb.go" "SubmitPromptError")
                       #'string<)
                 (sort (list "merging" "unknownWorkspace" "workspaceRefMismatch" "transferringAway" "notYetAdopted" "feedNotInWorkspace" "feedUndecodable" "turnAlreadyOpen" "noSession")
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
                 (sort (list "unknownWorkspace" "workspaceRefMismatch" "transferringAway" "notYetAdopted" "alreadyPaused" "notPaused" "noSuchQueuedMerge")
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

(ert-deftest agent-repl-test-wire-verbs-session-fault-unknown-kind-is-a-breach ()
  "A SessionFault kind this codec does not know is refused, never dropped."
  (agent-repl-test-wire-verbs--with-common
    (should-error (agent-repl-wire-decode-session-fault
                   (agent-repl-test-wire-verbs--parse "{\"shimDead\":{}}"))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-verbs-session-fault-kind-arms-pinned ()
  "SessionFault's kind oneof has exactly the eight arms decoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_session_health.pb.go" "SessionFault")
                       #'string<)
                 (sort (list "shimStartFailed" "shimDied" "linkSevered" "resumeFailed" "bounceDied" "bounceUnknown" "classifierFailed" "shimReported")
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

(ert-deftest agent-repl-test-wire-verbs-daemon-fault-deploy-script-failed-kind ()
  "DaemonFault's `deploy_script_failed' kind decodes with everything it
carries."
  (agent-repl-test-wire-verbs--with-common
    (should (equal (agent-repl-wire-decode-daemon-fault
                    (agent-repl-test-wire-verbs--parse "{\"deployScriptFailed\":{\"detail\":\"exit 1\"}}"))
                   '(:detail "" :kind (:arm :deploy-script-failed :value (:detail "exit 1")))))))

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

(ert-deftest agent-repl-test-wire-verbs-daemon-fault-kind-arms-pinned ()
  "DaemonFault's kind oneof has exactly the six arms decoded here."
  (should (equal (sort (agent-repl-test--generated-oneof-arms
                        "agentrepl/v1/endpoint_daemon_health.pb.go" "DaemonFault")
                       #'string<)
                 (sort (list "adoptionWindowExpired" "logSinkPoisoned" "deployScriptFailed" "successorSpawnFailed" "promptsDirMissing" "wsmReadOnly")
                       #'string<))))

(provide 'test-wire-verbs)

;;; test-wire-verbs.el ends here
