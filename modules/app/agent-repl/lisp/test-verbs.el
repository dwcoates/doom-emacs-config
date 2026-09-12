;;; test-verbs.el --- ERT tests for agent-repl verbs.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-verbs.el -f ert-run-tests-batch-and-exit
;;
;; Every rpc is stubbed with a function that RECORDS its request and invokes
;; the caller's callback SYNCHRONOUSLY, so each of the three answer shapes --
;; a success arm, a daemon-authored error arm, and a transport failure -- is
;; exercised deterministically with no process and no daemon anywhere.
;;
;; The tab teardown (`agent-repl--kill-one-workspace') is recorded rather
;; than run: it is workspace.el's persp-mode boundary, and what these tests
;; assert is WHETHER a verb tore the tab down, not what teardown does.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Fixtures ----

(defvar agent-repl-test-verbs--sent nil
  "Requests the stubbed rpcs received, oldest first, as (OP . REQUEST).")

(defvar agent-repl-test-verbs--torn-down nil
  "Workspaces whose tab teardown was invoked.")

(defvar agent-repl-test-verbs--messages nil
  "Strings passed to `message' during a test.")

(defvar agent-repl-test-verbs--handover nil
  "Refusal arms handed to host.el's handover path, as (WS ARM).")

(defvar agent-repl-test-verbs--selected nil
  "Project roots handed to `agent-repl-switch-to-project', oldest first.
The selection is commands.el's persp/projectile boundary, so it is
RECORDED here rather than run -- exactly as the tab teardown is.")

(defun agent-repl-test-verbs--ref (&optional id dir)
  "Return a decoded `WorkspaceRef' plist, echoed verbatim by production."
  (list :id (or id "ws-id-1") :dir (or dir "/tmp/agent-repl-test/ws-1")))

(defun agent-repl-test-verbs--created (&optional ref)
  "Return the answers alist for a create that SUCCEEDED, minting REF.
`CreateWorkspaceSuccess' is one of the few answers that is not empty --
\"Callers learn the identity from the response\" -- so a create test that
cares what the verb does with the minted identity has to script it."
  (list (cons :create
              (list :response
                    (list :arm :success
                          :value (list :workspace (or ref (agent-repl-test-verbs--ref))))))))

(defun agent-repl-test-verbs--tab-arrives (id name)
  "Deliver the roster arrival that gives the minted ID a tab called NAME.
A minted ref is the daemon's ANSWER and the workspace itself reaches
Emacs on the roster stream, so the landing waits for the tab
\(`agent-repl-verbs--pending-landing-fire').  A create test that expects
to STAND on what it made therefore has to let that tab arrive."
  (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id)
             (lambda (want) (and (equal want id) name))))
    (agent-repl-verbs--pending-landing-fire)))

(defun agent-repl-test-verbs--repo-ref (&optional id dir)
  "Return a decoded `RepositoryRef' plist."
  (list :id (or id "repo-id-1") :dir (or dir "/tmp/agent-repl-test/repo")))

(defun agent-repl-test-verbs--repo-section ()
  "Return the fixture roster's single repository section.
The DYNAMIC creation modes derive their repository from the section the
current workspace's row sits in, so a create test hands them this."
  (car (agent-repl-verbs--repo-sections (agent-repl-test-verbs--roster nil))))

(defun agent-repl-test-verbs--row (&rest overrides)
  "Return a decoded `RosterRow' with OVERRIDES applied at the top level."
  (let ((row (list :workspace (list :workspace (agent-repl-test-verbs--ref))
                   :attention nil
                   :priority nil
                   :name (list :text "row-one")
                   :status (list :arm :ready :value nil)
                   :current (list :current nil)
                   :children nil
                   :when nil
                   :detail (list :branch nil :parent-branch nil :summary nil)
                   :closed (list :closed nil))))
    (while overrides
      (setq row (plist-put row (pop overrides) (pop overrides))))
    row))

(defun agent-repl-test-verbs--roster (rows &optional merged-rows sections)
  "Return a decoded `WorkspaceRoster' carrying ROWS in one repo section."
  (list :repository
        (list :sections
              (or sections
                  (list (list :key (list :repository (agent-repl-test-verbs--repo-ref))
                              :header (list :label (list :text "repo-one"))
                              :rows (list :rows rows)))))
        :task (list :sections nil)
        :recently-merged (list :header (list :label (list :text "Recently Merged"))
                               :rows (list :rows merged-rows))
        :current nil))

(defmacro agent-repl-test-verbs--with (answers &rest body)
  "Run BODY with every verb rpc stubbed to answer from ANSWERS.
ANSWERS is an alist of (OP . ANSWER) where OP is the rpc's method keyword
and ANSWER is either `(:response PLIST)' -- delivered to `:on-response' --
or `(:failure PLIST)', delivered to `:on-failure'.  An op with no entry
answers a bare success, which is what almost every verb's success is."
  (declare (indent 1))
  `(let ((agent-repl-test-verbs--sent nil)
         (agent-repl-verbs--pending-landing nil)
         (agent-repl-test-verbs--torn-down nil)
         (agent-repl-test-verbs--messages nil)
         (agent-repl-test-verbs--handover nil)
         (agent-repl-test-verbs--selected nil)
         (agent-repl-test-verbs--restart-holds nil)
         (agent-repl-test-verbs--answers ,answers))
     (cl-letf* (((symbol-function 'agent-repl-host-ref)
                 (lambda (_ws) (agent-repl-test-verbs--ref)))
                ((symbol-function 'agent-repl--ws-require-known)
                 (lambda (_ws _context) nil))
                ((symbol-function 'agent-repl-host-conn) (lambda (_ws) 'test-conn))
                ((symbol-function 'agent-repl-host-faults) (lambda (_ws) nil))
                ((symbol-function 'agent-repl-host-handle-refusal)
                 (lambda (ws arm) (push (list ws arm) agent-repl-test-verbs--handover)))
                ((symbol-function 'agent-repl-host-take-restart-hold)
                 (lambda (ws) (push ws agent-repl-test-verbs--restart-holds)))
                ((symbol-function 'agent-repl-host-release-restart-hold)
                 (lambda (ws _reason)
                   (setq agent-repl-test-verbs--restart-holds
                         (delete ws agent-repl-test-verbs--restart-holds))))
                ((symbol-function 'agent-repl-link-primary) (lambda () 'test-conn))
                ((symbol-function 'agent-repl--ws-current-name) (lambda () "ws-one"))
                ;; The default registry is one real workspace, "ws-one", which
                ;; is also the current perspective: that is what makes the
                ;; command wrappers act on it without a picker.
                ((symbol-function 'agent-repl--live-ws-names) (lambda () '("ws-one")))
                ((symbol-function 'agent-repl--ws-get)
                 (lambda (ws key)
                   (and (eq key :project-dir) (equal ws "ws-one") "/tmp/agent-repl-test/ws-1")))
                ((symbol-function 'agent-repl--ws-log-routable-p)
                 (lambda (ws)
                   (and (stringp ws) (not (member ws '("main" "none"))))))
                ((symbol-function 'agent-repl--workspace-log-identity)
                 (lambda (ws)
                   (list :project-dir (format "/tmp/agent-repl-test/%s" ws)
                         :workspace-id (format "id-%s" ws))))
                ((symbol-function 'agent-repl--pseudo-workspace-name-p)
                 (lambda (ws) (member ws '("main" "none"))))
                ((symbol-function 'agent-repl--kill-one-workspace)
                 (lambda (ws &optional _p) (push ws agent-repl-test-verbs--torn-down)))
                ((symbol-function 'agent-repl-switch-to-project)
                 (lambda (&optional project)
                   (push project agent-repl-test-verbs--selected)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (if args (apply #'format fmt args) fmt)
                         agent-repl-test-verbs--messages)
                   nil))
                ((symbol-function 'display-buffer) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl-rpc-close-workspace)
                 (agent-repl-test-verbs--stub :close))
                ((symbol-function 'agent-repl-rpc-kill-workspace)
                 (agent-repl-test-verbs--stub :kill))
                ((symbol-function 'agent-repl-rpc-nuke-workspace)
                 (agent-repl-test-verbs--stub :nuke))
                ((symbol-function 'agent-repl-rpc-open-workspace)
                 (agent-repl-test-verbs--stub :open))
                ((symbol-function 'agent-repl-rpc-merge-workspace)
                 (agent-repl-test-verbs--stub :merge))
                ((symbol-function 'agent-repl-rpc-restart-workspace)
                 (agent-repl-test-verbs--stub :restart))
                ((symbol-function 'agent-repl-rpc-create-workspace)
                 (agent-repl-test-verbs--stub :create))
                ((symbol-function 'agent-repl-rpc-set-workspace-priority)
                 (agent-repl-test-verbs--stub :set-priority))
                ((symbol-function 'agent-repl-rpc-update-shutdown-schedule)
                 (agent-repl-test-verbs--stub :shutdown-schedule))
                ((symbol-function 'agent-repl-rpc-update-merge-queue)
                 (agent-repl-test-verbs--stub :merge-queue))
                ((symbol-function 'agent-repl-rpc-daemon-health)
                 (agent-repl-test-verbs--stub :daemon-health))
                ((symbol-function 'agent-repl-rpc-session-health)
                 (agent-repl-test-verbs--stub :session-health)))
       ,@body)))

(defvar agent-repl-test-verbs--restart-holds nil
  "Workspaces whose composer a forced restart closed, bound by `--with'.")

(defvar agent-repl-test-verbs--answers nil
  "The scripted answers for the rpc stubs, bound by the `--with' macro.")

(ert-deftest agent-repl-test-verbs-read-prompt-without-a-workspace-uses-no-composer ()
  "A creation prompt outside agent-repl does not read an unroutable composer."
  ;; Arrange.
  (let (read-workspace logged-workspace)
    (cl-letf (((symbol-function 'agent-repl--ws-current-log-name) (lambda () nil))
              ((symbol-function 'agent-repl--read-input-buffer)
               (lambda (ws) (setq read-workspace ws)))
              ((symbol-function 'agent-repl--log)
               (lambda (ws &rest _args) (setq logged-workspace ws)))
              ((symbol-function 'read-string)
               (lambda (_prompt initial &rest _) initial)))
      ;; Act.
      (agent-repl-verbs--read-prompt "Commission: "))
    ;; Assert.
    (should-not read-workspace)
    (should (agent-repl--central-log-scope-reason logged-workspace))))

(defun agent-repl-test-verbs--stub (op)
  "Return an rpc stub for OP that records its request and answers it."
  (lambda (_conn request &rest keys)
    (push (cons op request) agent-repl-test-verbs--sent)
    (let* ((scripted (cdr (assq op agent-repl-test-verbs--answers)))
           (failure (plist-get scripted :failure))
           (response (or (plist-get scripted :response)
                         (list :arm :success :value nil))))
      (if failure
          (funcall (plist-get keys :on-failure) failure)
        (funcall (plist-get keys :on-response) response)))))

(defun agent-repl-test-verbs--request (op)
  "Return the request recorded for OP, or nil."
  (cdr (assq op (reverse agent-repl-test-verbs--sent))))

(defun agent-repl-test-verbs--messaged-p (needle)
  "Return non-nil when any recorded `message' contained NEEDLE."
  (cl-find-if (lambda (m) (string-match-p (regexp-quote needle) m))
              agent-repl-test-verbs--messages))

(ert-deftest agent-repl-test-verbs-send-correlates-the-request-and-response ()
  "A verb's outbound boundary and synchronous answer share one request id."
  ;; Arrange
  (let (seen)
    (cl-letf (((symbol-function 'agent-repl--next-log-request-id)
               (lambda () "request-1"))
              ((symbol-function 'agent-repl--emit-log-record)
               (lambda (_ws _level _verbosity fmt _args &rest _)
                 (push (list fmt
                             agent-repl--log-context-workspace
                             agent-repl--log-context-request-id)
                       seen))))
      ;; Act
      (agent-repl-verbs--send
       (lambda (_conn _request &rest keys)
         (funcall (plist-get keys :on-response)
                  (list :arm :success :value nil)))
       'test-conn nil :ws "ws-one" :op "close")
      ;; Assert
      (should (equal (nreverse seen)
                     '(("elisp.verbs.send op=%s ws=%s" "ws-one" "request-1")
                       ("elisp.verbs.ack op=%s ws=%s outcome=success"
                        "ws-one" "request-1")))))))

;;;; ---- Request shapes: one test per verb ----

(ert-deftest agent-repl-verbs-close-echoes-the-ref ()
  "CloseWorkspace carries the daemon-minted ref verbatim."
  ;; Arrange / Act
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-close "ws-one")
    ;; Assert
    (should (equal (agent-repl-test-verbs--request :close)
                   (list :workspace (agent-repl-test-verbs--ref))))))

(ert-deftest agent-repl-verbs-kill-echoes-the-ref ()
  "KillWorkspace carries the ref and nothing else."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-kill "ws-one")
    (should (equal (agent-repl-test-verbs--request :kill)
                   (list :workspace (agent-repl-test-verbs--ref))))))

(ert-deftest agent-repl-verbs-nuke-echoes-the-ref ()
  "NukeWorkspace carries the ref and nothing else."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-nuke "ws-one")
    (should (equal (agent-repl-test-verbs--request :nuke)
                   (list :workspace (agent-repl-test-verbs--ref))))))

(ert-deftest agent-repl-verbs-merge-echoes-the-ref ()
  "MergeWorkspace carries the ref and nothing else."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-merge "ws-one")
    (should (equal (agent-repl-test-verbs--request :merge)
                   (list :workspace (agent-repl-test-verbs--ref))))))

(ert-deftest agent-repl-verbs-open-echoes-the-given-ref ()
  "OpenWorkspace carries the closed row's own ref, not the current one's."
  (let ((closed-ref (agent-repl-test-verbs--ref "closed-id" "/tmp/closed")))
    (agent-repl-test-verbs--with nil
      (agent-repl-verb-open closed-ref)
      (should (equal (agent-repl-test-verbs--request :open)
                     (list :workspace closed-ref))))))

(ert-deftest agent-repl-verbs-restart-states-force-false ()
  "A graceful restart states `force' explicitly rather than omitting it."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-restart "ws-one" nil)
    (should (equal (plist-get (agent-repl-test-verbs--request :restart) :force) nil))))

(ert-deftest agent-repl-verbs-restart-force-closes-the-composer-on-the-send ()
  "A forced restart's SEND is what closes the composer, not the later push."
  (agent-repl-test-verbs--with nil
    ;; Arrange / Act
    (agent-repl-verb-restart "ws-one" t)
    ;; Assert
    (should (equal agent-repl-test-verbs--restart-holds '("ws-one")))))

(ert-deftest agent-repl-verbs-restart-graceful-leaves-the-composer-open ()
  "A graceful restart is SCHEDULED, so it takes no hold on the composer."
  (agent-repl-test-verbs--with nil
    ;; Arrange / Act
    (agent-repl-verb-restart "ws-one" nil)
    ;; Assert
    (should (null agent-repl-test-verbs--restart-holds))))

(ert-deftest agent-repl-verbs-restart-refused-gives-the-composer-back ()
  "A refused forced restart bounces nothing, so it holds nothing shut."
  (agent-repl-test-verbs--with
      '((:restart . (:response (:arm :error
                                :value (:arm :no-session :value nil)))))
    ;; Arrange / Act
    (agent-repl-verb-restart "ws-one" t)
    ;; Assert
    (should (null agent-repl-test-verbs--restart-holds))))

(ert-deftest agent-repl-verbs-restart-force-sets-force-true ()
  "A forced restart sets `force'."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-restart "ws-one" t)
    (should (eq (plist-get (agent-repl-test-verbs--request :restart) :force) t))))

(ert-deftest agent-repl-verbs-set-priority-carries-the-level ()
  "SetWorkspacePriority carries the level arm when one is given."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-set-priority "ws-one" :p1)
    (should (equal (plist-get (agent-repl-test-verbs--request :set-priority) :priority)
                   (list :arm :p1 :value nil)))))

(ert-deftest agent-repl-verbs-set-priority-clear-omits-the-field ()
  "CLEARING a priority is the ABSENCE of the field, never a sentinel level."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-set-priority "ws-one" nil)
    (should-not (plist-get (agent-repl-test-verbs--request :set-priority) :priority))))

;;;; ---- Ack handling: one test per outcome ----

(ert-deftest agent-repl-verbs-close-success-tears-the-tab-down ()
  "A quiet close proceeds with the view teardown."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-close "ws-one")
    (should (equal agent-repl-test-verbs--torn-down '("ws-one")))))

(ert-deftest agent-repl-verbs-close-blocked-leaves-the-tab ()
  "A BLOCKED close draws no dialog and leaves the tab in place."
  (agent-repl-test-verbs--with
      '((:close . (:response (:arm :error :value (:cause (:arm :blocked :value nil))))))
    (agent-repl-verb-close "ws-one")
    (should-not agent-repl-test-verbs--torn-down)))

(ert-deftest agent-repl-verbs-close-blocked-names-the-footer ()
  "The blocked message sends the user to the footer, which carries the reasons."
  (agent-repl-test-verbs--with
      '((:close . (:response (:arm :error :value (:cause (:arm :blocked :value nil))))))
    (agent-repl-verb-close "ws-one")
    (should (agent-repl-test-verbs--messaged-p
             "close blocked -- see the workspace footer"))))

(ert-deftest agent-repl-verbs-close-blocked-echoes-the-summary ()
  "The daemon's own composed sentence is echoed verbatim when it sent one."
  (agent-repl-test-verbs--with
      '((:close . (:response (:arm :error
                              :value (:cause (:arm :blocked
                                              :value (:turn-in-flight t :live-work 0
                                                      :held-prompts 0 :merge-queued nil
                                                      :summary "a turn is running")))))))
    (agent-repl-verb-close "ws-one")
    (should (agent-repl-test-verbs--messaged-p "close blocked -- a turn is running"))))

(ert-deftest agent-repl-verbs-close-blocked-empty-summary-names-the-footer ()
  "With no composed sentence the footer stays the place the reasons are read."
  (agent-repl-test-verbs--with
      '((:close . (:response (:arm :error
                              :value (:cause (:arm :blocked
                                              :value (:turn-in-flight nil :live-work 2
                                                      :held-prompts 0 :merge-queued nil
                                                      :summary "")))))))
    (agent-repl-verb-close "ws-one")
    (should (agent-repl-test-verbs--messaged-p
             "close blocked -- see the workspace footer"))))

(ert-deftest agent-repl-verbs-close-blocked-with-evidence-leaves-the-tab ()
  "Evidence on the refusal does not make it any less a refusal."
  (agent-repl-test-verbs--with
      '((:close . (:response (:arm :error
                              :value (:cause (:arm :blocked
                                              :value (:turn-in-flight t :live-work 1
                                                      :held-prompts 3 :merge-queued t
                                                      :summary "busy")))))))
    (agent-repl-verb-close "ws-one")
    (should-not agent-repl-test-verbs--torn-down)))

(ert-deftest agent-repl-verbs-close-transport-failure-leaves-the-tab ()
  "Nobody answering is not permission to tear the tab down."
  (agent-repl-test-verbs--with '((:close . (:failure (:kind :transport))))
    (agent-repl-verb-close "ws-one")
    (should-not agent-repl-test-verbs--torn-down)))

(ert-deftest agent-repl-verbs-kill-success-tears-the-tab-down ()
  "A kill's success proceeds with the view teardown."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-kill "ws-one")
    (should (equal agent-repl-test-verbs--torn-down '("ws-one")))))

(ert-deftest agent-repl-verbs-nuke-success-tears-the-tab-down ()
  "A nuke's success proceeds with the view teardown."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-nuke "ws-one")
    (should (equal agent-repl-test-verbs--torn-down '("ws-one")))))

(ert-deftest agent-repl-verbs-merge-success-says-enqueued-only ()
  "Merge success means ENQUEUED; Emacs holds no further merge state."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-merge "ws-one")
    (should (agent-repl-test-verbs--messaged-p "merge enqueued"))
    (should-not agent-repl-test-verbs--torn-down)))

(ert-deftest agent-repl-verbs-error-arm-does-not-tear-the-tab-down ()
  "A daemon-authored refusal changes no editor state."
  (agent-repl-test-verbs--with
      '((:kill . (:response (:arm :error
                             :value (:cause (:arm :unknown-workspace :value nil))))))
    (agent-repl-verb-kill "ws-one")
    (should-not agent-repl-test-verbs--torn-down)))

;;;; ---- Arm-generic refusal handling ----
;;
;; Every `<Rpc>Error' spells its refusal as a `cause' oneof whose ARM IS THE
;; REASON, so the handling is generic over the arm rather than a table this
;; file would have to keep in step with the contract.

(ert-deftest agent-repl-verbs-refusal-names-the-arm ()
  "A refusal draws the arm keyword, whatever arm the daemon chose."
  (agent-repl-test-verbs--with
      '((:merge . (:response (:arm :error
                              :value (:cause (:arm :already-queued :value nil))))))
    (agent-repl-verb-merge "ws-one")
    (should (agent-repl-test-verbs--messaged-p "merge refused: already-queued"))))

(ert-deftest agent-repl-verbs-refusal-draws-an-unmodelled-arm-too ()
  "An arm this file has never heard of still reaches the user by name.
The point of arm-generic handling: a new refusal arm works the day the
daemon starts sending it, with no table to update here."
  (agent-repl-test-verbs--with
      '((:restart . (:response (:arm :error
                                :value (:cause (:arm :some-future-arm :value nil))))))
    (agent-repl-verb-restart "ws-one" nil)
    (should (agent-repl-test-verbs--messaged-p "restart refused: some-future-arm"))))

(ert-deftest agent-repl-verbs-refusal-carries-the-arms-own-fields ()
  "Fields living inside the arm are drawn beside it."
  (agent-repl-test-verbs--with
      '((:kill . (:response (:arm :error
                             :value (:cause (:arm :workspace-ref-mismatch
                                             :value (:registry-dir "/tmp/real")))))))
    (agent-repl-verb-kill "ws-one")
    (should (agent-repl-test-verbs--messaged-p "/tmp/real"))))

(ert-deftest agent-repl-verbs-open-vendor-start-failure-names-the-arm ()
  "An open refused because the VENDOR would not start names that arm."
  (agent-repl-test-verbs--with
      '((:open . (:response (:arm :error
                            :value (:cause (:arm :vendor-start-failed
                                            :value (:detail "the sdk threw")))))))
    (agent-repl-verb-open (agent-repl-test-verbs--ref "closed-id" "/tmp/closed"))
    (should (agent-repl-test-verbs--messaged-p "open refused: vendor-start-failed"))))

(ert-deftest agent-repl-verbs-open-vendor-start-failure-echoes-the-detail ()
  "The shim's own account is the user's only lead, so it is drawn too."
  (agent-repl-test-verbs--with
      '((:open . (:response (:arm :error
                            :value (:cause (:arm :vendor-start-failed
                                            :value (:detail "the sdk threw")))))))
    (agent-repl-verb-open (agent-repl-test-verbs--ref "closed-id" "/tmp/closed"))
    (should (agent-repl-test-verbs--messaged-p "the sdk threw"))))

(ert-deftest agent-repl-verbs-empty-arm-draws-no-empty-fields ()
  "An empty arm is its own whole assertion and renders no trailing payload."
  (agent-repl-test-verbs--with
      '((:merge . (:response (:arm :error
                              :value (:cause (:arm :already-merging :value nil))))))
    (agent-repl-verb-merge "ws-one")
    (should (agent-repl-test-verbs--messaged-p "merge refused: already-merging"))
    (should-not (agent-repl-test-verbs--messaged-p "already-merging ("))))

(ert-deftest agent-repl-verbs-transferring-away-goes-to-the-handover ()
  "`transferring_away' is the handover's ordering, not a user-facing failure."
  (agent-repl-test-verbs--with
      '((:merge . (:response (:arm :error
                              :value (:cause (:arm :transferring-away
                                              :value (:address "127.0.0.1:9999")))))))
    (agent-repl-verb-merge "ws-one")
    (should (equal (length agent-repl-test-verbs--handover) 1))
    (should (equal (car (car agent-repl-test-verbs--handover)) "ws-one"))
    (should (equal (plist-get (nth 1 (car agent-repl-test-verbs--handover)) :value)
                   '(:address "127.0.0.1:9999")))))

(ert-deftest agent-repl-verbs-transferring-away-draws-nothing ()
  "The rollout it belongs to is supposed to be invisible, so it is silent."
  (agent-repl-test-verbs--with
      '((:merge . (:response (:arm :error
                              :value (:cause (:arm :transferring-away
                                              :value (:address "127.0.0.1:9999")))))))
    (agent-repl-verb-merge "ws-one")
    (should-not (agent-repl-test-verbs--messaged-p "refused"))))

(ert-deftest agent-repl-verbs-not-yet-adopted-goes-to-the-handover ()
  "`not_yet_adopted' is the successor's own too-early refusal, also silent."
  (agent-repl-test-verbs--with
      '((:kill . (:response (:arm :error
                             :value (:cause (:arm :not-yet-adopted :value nil))))))
    (agent-repl-verb-kill "ws-one")
    (should (equal (length agent-repl-test-verbs--handover) 1))
    (should-not (agent-repl-test-verbs--messaged-p "refused"))))

(ert-deftest agent-repl-verbs-handover-refusal-changes-no-editor-state ()
  "A handover refusal is not a close: the tab stays where it is."
  (agent-repl-test-verbs--with
      '((:close . (:response (:arm :error
                              :value (:cause (:arm :not-yet-adopted :value nil))))))
    (agent-repl-verb-close "ws-one")
    (should-not agent-repl-test-verbs--torn-down)))

(ert-deftest agent-repl-verbs-close-non-blocked-arm-falls-through ()
  "Close claims only `blocked'; every other arm gets the generic report."
  (agent-repl-test-verbs--with
      '((:close . (:response (:arm :error
                              :value (:cause (:arm :unknown-workspace :value nil))))))
    (agent-repl-verb-close "ws-one")
    (should (agent-repl-test-verbs--messaged-p "close refused: unknown-workspace"))
    (should-not agent-repl-test-verbs--torn-down)))

(ert-deftest agent-repl-verbs-close-blocked-still-draws-the-footer-message ()
  "Close's one claimed arm keeps its prescribed treatment."
  (agent-repl-test-verbs--with
      '((:close . (:response (:arm :error
                              :value (:cause (:arm :blocked :value nil))))))
    (agent-repl-verb-close "ws-one")
    (should (agent-repl-test-verbs--messaged-p
             "close blocked -- see the workspace footer"))
    (should-not (agent-repl-test-verbs--messaged-p "close refused"))))

(ert-deftest agent-repl-verbs-create-refusal-names-its-arm ()
  "A create refusal reports the daemon's own reason, e.g. an unknown repo."
  (agent-repl-test-verbs--with
      '((:create . (:response (:arm :error
                               :value (:cause (:arm :unknown-repository :value nil))))))
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard)
    (should (agent-repl-test-verbs--messaged-p "create refused: unknown-repository"))))

(ert-deftest agent-repl-verbs-missing-ref-refuses-before-sending ()
  "A workspace with no daemon identity cannot be addressed at all."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (_ws) nil)))
      (should-error (agent-repl-verb-close "ws-one") :type 'user-error)
      (should-not agent-repl-test-verbs--sent))))

;;;; ---- Interactive command behavior ----

(ert-deftest agent-repl-verbs-nuke-command-confirms-before-sending ()
  "The one data-destroying verb asks first."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_p) t)))
      (agent-repl-nuke-workspace)
      (should (agent-repl-test-verbs--request :nuke)))))

(ert-deftest agent-repl-verbs-nuke-command-declined-sends-nothing ()
  "Declining the nuke confirmation sends no request."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_p) nil)))
      (agent-repl-nuke-workspace)
      (should-not agent-repl-test-verbs--sent))))

(ert-deftest agent-repl-verbs-restart-command-prefix-arg-forces ()
  "A prefix argument makes `agent-repl-restart-workspace' forced."
  (agent-repl-test-verbs--with nil
    (agent-repl-restart-workspace t)
    (should (eq (plist-get (agent-repl-test-verbs--request :restart) :force) t))))

(ert-deftest agent-repl-verbs-open-command-offers-only-closed-rows ()
  "The open picker's candidates are exactly the roster's CLOSED rows."
  (let* ((open-row (agent-repl-test-verbs--row :name (list :text "open-row")))
         (closed-row (agent-repl-test-verbs--row
                      :workspace (list :workspace (agent-repl-test-verbs--ref "cid" "/tmp/c"))
                      :name (list :text "closed-row")
                      :closed (list :closed t)))
         (agent-repl-roster-view (agent-repl-test-verbs--roster (list open-row closed-row)))
         (offered nil))
    (agent-repl-test-verbs--with nil
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p candidates &rest _) (setq offered candidates) "closed-row")))
        (agent-repl-open-workspace)
        (should (equal offered '("closed-row")))
        (should (equal (agent-repl-test-verbs--request :open)
                       (list :workspace (agent-repl-test-verbs--ref "cid" "/tmp/c"))))))))

(ert-deftest agent-repl-verbs-open-command-refuses-with-no-closed-rows ()
  "With nothing closed there is nothing to open, and that is a refusal."
  (let ((agent-repl-roster-view
         (agent-repl-test-verbs--roster (list (agent-repl-test-verbs--row)))))
    (agent-repl-test-verbs--with nil
      (should-error (agent-repl-open-workspace) :type 'user-error))))

(ert-deftest agent-repl-verbs-closed-rows-walks-children ()
  "A closed CHILD row is reachable: the walk is depth-first over children."
  (let* ((child (agent-repl-test-verbs--row
                 :workspace (list :workspace (agent-repl-test-verbs--ref "kid" "/tmp/kid"))
                 :name (list :text "kid") :closed (list :closed t)))
         (parent (agent-repl-test-verbs--row :children (list child)))
         (roster (agent-repl-test-verbs--roster (list parent))))
    (should (equal (mapcar #'agent-repl-verbs--row-name
                           (agent-repl-verbs--closed-rows roster))
                   '("kid")))))

(ert-deftest agent-repl-verbs-closed-rows-includes-recently-merged ()
  "The recently-merged section is walked after the repo sections."
  (let* ((merged (agent-repl-test-verbs--row
                  :workspace (list :workspace (agent-repl-test-verbs--ref "m" "/tmp/m"))
                  :name (list :text "merged-one") :closed (list :closed t)))
         (roster (agent-repl-test-verbs--roster nil (list merged))))
    (should (equal (mapcar #'agent-repl-verbs--row-name
                           (agent-repl-verbs--closed-rows roster))
                   '("merged-one")))))

;;;; ---- Create: the standard form ----

(ert-deftest agent-repl-verbs-create-standard-sends-the-repository-and-form ()
  "A standard create names its repository and sets the standard form arm."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard :name "n")
    (let ((request (agent-repl-test-verbs--request :create)))
      (should (equal (plist-get request :repository) (agent-repl-test-verbs--repo-ref)))
      (should (eq (plist-get (plist-get request :form) :arm) :standard)))))

(ert-deftest agent-repl-verbs-create-omits-absent-facts ()
  "Absent creation facts are OMITTED: absence is what the proto reads."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard)
    (let ((request (agent-repl-test-verbs--request :create)))
      (should-not (plist-get request :parent))
      (should-not (plist-get request :model))
      (should-not (plist-get request :priority))
      (should-not (plist-get request :allow-ungated)))))

(ert-deftest agent-repl-verbs-create-command-sends-no-name-and-no-base-ref ()
  "A DYNAMIC create asks for neither a name nor a base ref, and sends neither.
The daemon mints the name, and an absent base ref IS the repo's main
branch -- so a `read-string' here would be a question the ruling removed."
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "do a thing"))
              ((symbol-function 'read-string)
               (lambda (&rest _) (error "a dynamic create asks nothing but its prompt"))))
      (agent-repl-create-workspace nil)
      (let ((standard (plist-get (plist-get (agent-repl-test-verbs--request :create) :form)
                                 :value)))
        (should-not (plist-get standard :name))
        (should-not (plist-get standard :base-ref))))))

(ert-deftest agent-repl-verbs-create-command-prefix-arg-sets-the-parent ()
  "A prefix argument makes the new workspace a CHILD of the current one."
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "do a thing"))
              ((symbol-function 'read-string) (lambda (&rest _) "")))
      (agent-repl-create-workspace t)
      (should (equal (plist-get (agent-repl-test-verbs--request :create) :parent)
                     (list :workspace (agent-repl-test-verbs--ref)))))))

(ert-deftest agent-repl-verbs-fork-command-sets-fork-inside-the-parent ()
  "A fork lives INSIDE the parent: a fork without a parent is unrepresentable."
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "fork it")))
      (agent-repl-fork-workspace)
      (should (eq (plist-get (plist-get (agent-repl-test-verbs--request :create) :parent)
                             :fork)
                  t)))))

;;;; ---- Create: the four modes and the questions each one asks ----

(ert-deftest agent-repl-verbs-create-command-asks-no-repository ()
  "A DYNAMIC create never asks for a repository (owner ruling, 2026-09-12)."
  ;; Arrange.
  (let ((agent-repl-roster-view
         (agent-repl-test-verbs--roster (list (agent-repl-test-verbs--row)))))
    (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
      (cl-letf (((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "do a thing"))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) (error "a dynamic create must not ask for a repository"))))
        ;; Act.
        (agent-repl-create-workspace nil)
        ;; Assert.
        (should (equal (plist-get (agent-repl-test-verbs--request :create) :repository)
                       (agent-repl-test-verbs--repo-ref)))))))

(ert-deftest agent-repl-verbs-create-command-uses-the-current-workspaces-section ()
  "The derived repository is the section the CURRENT workspace's row sits in."
  ;; Arrange: two sections, the current workspace's row in the second.
  (let* ((other (list :key (list :repository (agent-repl-test-verbs--repo-ref
                                              "other-id" "/tmp/other"))
                      :header (list :label (list :text "other-repo"))
                      :rows (list :rows nil)))
         (mine (list :key (list :repository (agent-repl-test-verbs--repo-ref))
                     :header (list :label (list :text "repo-one"))
                     :rows (list :rows (list (agent-repl-test-verbs--row)))))
         (agent-repl-roster-view
          (agent-repl-test-verbs--roster nil nil (list other mine))))
    (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
      (cl-letf (((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "do a thing")))
        ;; Act.
        (agent-repl-create-workspace nil)
        ;; Assert.
        (should (equal (plist-get (agent-repl-test-verbs--request :create) :repository)
                       (agent-repl-test-verbs--repo-ref)))))))

(ert-deftest agent-repl-verbs-create-command-without-a-current-workspace-refuses ()
  "With no current workspace a dynamic create has nothing to derive from."
  ;; Arrange.
  (let ((agent-repl-roster-view
         (agent-repl-test-verbs--roster (list (agent-repl-test-verbs--row)))))
    (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
      (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (_ws) nil)))
        ;; Act / Assert.
        (should-error (agent-repl-create-workspace nil) :type 'user-error)
        (should-not agent-repl-test-verbs--sent)))))

(ert-deftest agent-repl-verbs-fork-command-asks-no-repository ()
  "A fork is a DYNAMIC mode too: prompt only, repository derived."
  ;; Arrange.
  (let ((agent-repl-roster-view
         (agent-repl-test-verbs--roster (list (agent-repl-test-verbs--row)))))
    (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
      (cl-letf (((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "fork it"))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) (error "a fork must not ask for a repository"))))
        ;; Act.
        (agent-repl-fork-workspace)
        ;; Assert.
        (should (equal (plist-get (agent-repl-test-verbs--request :create) :repository)
                       (agent-repl-test-verbs--repo-ref)))))))

(ert-deftest agent-repl-verbs-oneshot-self-merge-asks-no-repository ()
  "The self-merge one-shot derives its repository instead of asking."
  ;; Arrange.
  (let ((agent-repl-roster-view
         (agent-repl-test-verbs--roster (list (agent-repl-test-verbs--row)))))
    (agent-repl-test-verbs--with nil
      (cl-letf (((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "commission"))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) (error "a one-shot must not ask for a repository"))))
        ;; Act.
        (agent-repl-create-oneshot-self-merge nil)
        ;; Assert.
        (should (equal (plist-get (agent-repl-test-verbs--request :create) :repository)
                       (agent-repl-test-verbs--repo-ref)))))))

(ert-deftest agent-repl-verbs-oneshot-open-pr-asks-no-repository ()
  "The queued-PR one-shot derives its repository instead of asking."
  ;; Arrange.
  (let ((agent-repl-roster-view
         (agent-repl-test-verbs--roster (list (agent-repl-test-verbs--row)))))
    (agent-repl-test-verbs--with nil
      (cl-letf (((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "commission"))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) (error "a one-shot must not ask for a repository"))))
        ;; Act.
        (agent-repl-create-oneshot-open-pr nil)
        ;; Assert.
        (should (equal (plist-get (agent-repl-test-verbs--request :create) :repository)
                       (agent-repl-test-verbs--repo-ref)))))))

(ert-deftest agent-repl-verbs-oneshot-open-pr-reviewed-asks-no-repository ()
  "The review-demanding one-shot derives its repository instead of asking."
  ;; Arrange.
  (let ((agent-repl-roster-view
         (agent-repl-test-verbs--roster (list (agent-repl-test-verbs--row)))))
    (agent-repl-test-verbs--with nil
      (cl-letf (((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "commission"))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) (error "a one-shot must not ask for a repository"))))
        ;; Act.
        (agent-repl-create-oneshot-open-pr-reviewed nil)
        ;; Assert.
        (should (equal (plist-get (agent-repl-test-verbs--request :create) :repository)
                       (agent-repl-test-verbs--repo-ref)))))))

(ert-deftest agent-repl-verbs-static-create-sends-its-name ()
  "The STATIC create asks a repository and a REQUIRED name, and sends both."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    (cl-letf (((symbol-function 'agent-repl-verbs--read-repository)
               (lambda () (agent-repl-test-verbs--repo-ref)))
              ((symbol-function 'read-string) (lambda (&rest _) "named-one")))
      ;; Act.
      (agent-repl-create-workspace-static nil)
      ;; Assert.
      (let ((request (agent-repl-test-verbs--request :create)))
        (should (equal (plist-get request :repository) (agent-repl-test-verbs--repo-ref)))
        (should (equal (plist-get (plist-get (plist-get request :form) :value) :name)
                       "named-one"))))))

(ert-deftest agent-repl-verbs-static-create-sends-no-initial-prompt ()
  "The STATIC create sends NO prompt: the workspace comes up idle."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    (cl-letf (((symbol-function 'agent-repl-verbs--read-repository)
               (lambda () (agent-repl-test-verbs--repo-ref)))
              ((symbol-function 'agent-repl-verbs--read-prompt)
               (lambda (_p) (error "a static create asks for no prompt")))
              ((symbol-function 'read-string) (lambda (&rest _) "named-one")))
      ;; Act.
      (agent-repl-create-workspace-static nil)
      ;; Assert.
      (should-not (plist-get (plist-get (plist-get (agent-repl-test-verbs--request :create)
                                                   :form)
                                        :value)
                             :initial-prompt)))))

(ert-deftest agent-repl-verbs-static-create-refuses-a-blank-name ()
  "The static create's name is REQUIRED: a blank one is refused before send."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    (cl-letf (((symbol-function 'agent-repl-verbs--read-repository)
               (lambda () (agent-repl-test-verbs--repo-ref)))
              ((symbol-function 'read-string) (lambda (&rest _) "   ")))
      ;; Act / Assert.
      (should-error (agent-repl-create-workspace-static nil) :type 'user-error)
      (should-not agent-repl-test-verbs--sent))))

(ert-deftest agent-repl-verbs-static-create-prefix-arg-sets-the-parent ()
  "The static create takes the same CHILD prefix the dynamic one does."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    (cl-letf (((symbol-function 'agent-repl-verbs--read-repository)
               (lambda () (agent-repl-test-verbs--repo-ref)))
              ((symbol-function 'read-string) (lambda (&rest _) "named-one")))
      ;; Act.
      (agent-repl-create-workspace-static t)
      ;; Assert.
      (should (equal (plist-get (agent-repl-test-verbs--request :create) :parent)
                     (list :workspace (agent-repl-test-verbs--ref)))))))

;;;; ---- Create: standing on what was just created ----

(ert-deftest agent-repl-verbs-create-command-selects-the-created-workspace ()
  "`SPC TAB n' stands on the workspace it just made.
Creating one is a statement about where the user intends to work next, so
the create selects it the same way registering a directory does."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created
                                (agent-repl-test-verbs--ref "new-id" "/tmp/agent-repl-test/new"))
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "do a thing"))
              ((symbol-function 'read-string) (lambda (&rest _) "")))
      ;; Act.
      (agent-repl-create-workspace nil)
      (agent-repl-test-verbs--tab-arrives "new-id" "new-ws")
      ;; Assert.
      (should (equal agent-repl-test-verbs--selected
                     '("/tmp/agent-repl-test/new"))))))

(ert-deftest agent-repl-verbs-fork-command-selects-the-created-workspace ()
  "`SPC TAB f' stands on the fork: you forked in order to work in the fork."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created
                                (agent-repl-test-verbs--ref "fork-id" "/tmp/agent-repl-test/fork"))
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "fork it")))
      ;; Act.
      (agent-repl-fork-workspace)
      (agent-repl-test-verbs--tab-arrives "fork-id" "fork-ws")
      ;; Assert.
      (should (equal agent-repl-test-verbs--selected
                     '("/tmp/agent-repl-test/fork"))))))

(ert-deftest agent-repl-verbs-create-lands-on-the-panel-not-magit ()
  "The workspace `SPC TAB n\' just made comes up on ITS OWN PANEL.
The recorded selection runs the real landing policy, so a magit status
opened over the new workspace\'s panel shows up here as a magit call."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created
                                (agent-repl-test-verbs--ref "new-id" "/tmp/agent-repl-test/new"))
    (let (magit-dirs)
      (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
                 (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
                ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "do a thing"))
                ((symbol-function 'read-string) (lambda (&rest _) ""))
                ((symbol-function 'agent-repl--ws-name-for-dir)
                 (lambda (dir) (and (equal dir "/tmp/agent-repl-test/new") "new-ws")))
                ((symbol-function 'doom-real-buffer-list) (lambda (&optional _b) nil))
                ((symbol-function 'agent-repl--magit-status-same-window)
                 (lambda (dir) (push dir magit-dirs)))
                ((symbol-function 'agent-repl-switch-to-project)
                 (lambda (dir)
                   (push dir agent-repl-test-verbs--selected)
                   (agent-repl--ws-switch-project-display dir))))
        ;; Act.
        (agent-repl-create-workspace nil)
        (agent-repl-test-verbs--tab-arrives "new-id" "new-ws")
        ;; Assert.
        (should agent-repl-test-verbs--selected)
        (should-not magit-dirs)))))

(ert-deftest agent-repl-verbs-fork-lands-on-the-panel-not-magit ()
  "A fork comes up on its own panel too: the fork is where you meant to work."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created
                                (agent-repl-test-verbs--ref "fork-id" "/tmp/agent-repl-test/fork"))
    (let (magit-dirs)
      (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
                 (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
                ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "fork it"))
                ((symbol-function 'agent-repl--ws-name-for-dir)
                 (lambda (dir) (and (equal dir "/tmp/agent-repl-test/fork") "fork-ws")))
                ((symbol-function 'doom-real-buffer-list) (lambda (&optional _b) nil))
                ((symbol-function 'agent-repl--magit-status-same-window)
                 (lambda (dir) (push dir magit-dirs)))
                ((symbol-function 'agent-repl-switch-to-project)
                 (lambda (dir)
                   (push dir agent-repl-test-verbs--selected)
                   (agent-repl--ws-switch-project-display dir))))
        ;; Act.
        (agent-repl-fork-workspace)
        (agent-repl-test-verbs--tab-arrives "fork-id" "fork-ws")
        ;; Assert.
        (should agent-repl-test-verbs--selected)
        (should-not magit-dirs)))))

(ert-deftest agent-repl-verbs-create-does-not-land-before-the-tab-arrives ()
  "A minted ref whose tab has not reached the roster moves the user NOWHERE.
Standing on a directory that is not a workspace yet arms no panels and
lands the user on Doom's empty-project fallback, so the landing waits."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created
                                (agent-repl-test-verbs--ref "new-id" "/tmp/agent-repl-test/new"))
    (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil)))
      ;; Act.
      (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard :select t)
      ;; Assert.
      (should-not agent-repl-test-verbs--selected))))

(ert-deftest agent-repl-verbs-a-second-mint-supersedes-a-waiting-landing ()
  "The user stands in ONE place, so a newer mint replaces one still waiting."
  ;; Arrange.
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil)))
      (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "first" "/tmp/a"))
      ;; Act.
      (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "second" "/tmp/b"))
      ;; Assert.
      (should (equal (plist-get agent-repl-verbs--pending-landing :id) "second")))))

(ert-deftest agent-repl-verbs-create-without-select-stands-still ()
  "A create that did not ask to be selected moves the user NOWHERE.
`select' is off by default because a one-shot is fire-and-forget and must
not steal the user's place."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created)
    ;; Act.
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard)
    ;; Assert.
    (should-not agent-repl-test-verbs--selected)))

(ert-deftest agent-repl-verbs-create-select-without-a-dir-is-reported ()
  "A success whose ref carries no dir is REPORTED, never silently skipped.
The decoder already refuses a success without the ref, so a missing dir is
a contract breach and the user is owed the reason nothing came up."
  ;; Arrange.
  (agent-repl-test-verbs--with (agent-repl-test-verbs--created (list :id "no-dir" :dir ""))
    ;; Act.
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard :select t)
    ;; Assert.
    (should-not agent-repl-test-verbs--selected)
    (should (agent-repl-test-verbs--messaged-p "no directory to switch to"))))

;;;; ---- Create: the one-shot form and both finish arms ----

(ert-deftest agent-repl-verbs-oneshot-self-merge-sets-that-finish-arm ()
  "The self-merge one-shot finishes through the ordinary merge engine."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "commission")))
      (agent-repl-create-oneshot-self-merge nil)
      (let ((one-shot (plist-get (plist-get (agent-repl-test-verbs--request :create) :form)
                                 :value)))
        (should (eq (plist-get (plist-get one-shot :finish) :arm) :self-merge))))))

(ert-deftest agent-repl-verbs-oneshot-open-pr-sets-both-flags-true ()
  "The queued one-shot PR is self-certified and added to the merge queue."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "commission")))
      (agent-repl-create-oneshot-open-pr nil)
      (let* ((one-shot (plist-get (plist-get (agent-repl-test-verbs--request :create) :form)
                                  :value))
             (finish (plist-get one-shot :finish)))
        (should (eq (plist-get finish :arm) :open-pr))
        (should (eq (plist-get (plist-get finish :value) :self-certified) t))
        (should (eq (plist-get (plist-get finish :value) :add-to-merge-queue) t))))))

(ert-deftest agent-repl-verbs-oneshot-open-pr-reviewed-states-both-flags-false ()
  "The review-demanding PR states both flags FALSE rather than omitting them."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "commission")))
      (agent-repl-create-oneshot-open-pr-reviewed nil)
      (let ((finish (plist-get (plist-get (plist-get (agent-repl-test-verbs--request :create)
                                                     :form)
                                          :value)
                               :finish)))
        (should-not (plist-get (plist-get finish :value) :self-certified))
        (should-not (plist-get (plist-get finish :value) :add-to-merge-queue))))))

(ert-deftest agent-repl-verbs-oneshot-carries-its-prompt ()
  "A one-shot IS its prompt, so the prompt travels as a UserSaid text block."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "commission")))
      (agent-repl-create-oneshot-self-merge nil)
      (let ((one-shot (plist-get (plist-get (agent-repl-test-verbs--request :create) :form)
                                 :value)))
        (should (equal (plist-get one-shot :prompt)
                       (list :content
                             (list :blocks
                                   (list (list :arm :text
                                               :value (list :text "commission")))))))))))

(ert-deftest agent-repl-verbs-oneshot-refuses-a-blank-commission ()
  "A one-shot with no prompt is refused before a request is built."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "   ")))
      (should-error (agent-repl-create-oneshot-self-merge nil) :type 'user-error)
      (should-not agent-repl-test-verbs--sent))))

(ert-deftest agent-repl-verbs-oneshot-prefix-arg-picks-a-model ()
  "A prefix argument threads the chosen model onto the shared facts."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl-verbs--section-of-ws)
               (lambda (&rest _) (agent-repl-test-verbs--repo-section)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "commission"))
              ((symbol-function 'agent-repl-verbs--read-model) (lambda () "haiku")))
      (agent-repl-create-oneshot-self-merge t)
      (should (equal (plist-get (agent-repl-test-verbs--request :create) :model) "haiku")))))

;;;; ---- Repository selection ----

(ert-deftest agent-repl-verbs-read-repository-defaults-to-the-current-section ()
  "The repository defaults to the section the current workspace's row sits in."
  (let* ((row (agent-repl-test-verbs--row))
         (agent-repl-roster-view (agent-repl-test-verbs--roster (list row)))
         (seen-default nil))
    (agent-repl-test-verbs--with nil
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p _c &optional _pr _rm _ii _h default)
                   (setq seen-default default) "repo-one")))
        (should (equal (agent-repl-verbs--read-repository)
                       (agent-repl-test-verbs--repo-ref)))
        (should (equal seen-default "repo-one"))))))

(ert-deftest agent-repl-verbs-read-repository-refuses-with-no-sections ()
  "With no repositories on the roster there is nothing to create in.
The roster is built inline rather than through the fixture: the fixture
supplies a default section precisely so the other tests have one, and
this test needs the genuinely sectionless roster."
  (let ((agent-repl-roster-view
         (list :repository (list :sections nil)
               :task (list :sections nil)
               :recently-merged (list :header (list :label (list :text "Recently Merged"))
                                      :rows (list :rows nil))
               :current nil)))
    (agent-repl-test-verbs--with nil
      (should-error (agent-repl-verbs--read-repository) :type 'user-error))))

;;;; ---- Shutdown schedule ----

(ert-deftest agent-repl-verbs-shutdown-schedule-carries-at-ms-and-reason ()
  "A scheduled drain carries its deadline instant and its typed reason."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-shutdown-schedule
     (list :arm :schedule :at-ms 1700000000000 :reason (list :arm :deploy)))
    (let ((action (plist-get (agent-repl-test-verbs--request :shutdown-schedule) :action)))
      (should (eq (plist-get action :arm) :schedule))
      (should (equal (plist-get (plist-get action :value) :at-ms) 1700000000000))
      (should (eq (plist-get (plist-get (plist-get action :value) :reason) :arm) :deploy)))))

(ert-deftest agent-repl-verbs-shutdown-cancel-carries-an-empty-arm ()
  "Cancel is the whole assertion: the arm carries no payload."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-shutdown-schedule (list :arm :cancel))
    (should (equal (plist-get (agent-repl-test-verbs--request :shutdown-schedule) :action)
                   (list :arm :cancel :value nil)))))

(ert-deftest agent-repl-verbs-shutdown-schedule-command-requires-a-reason ()
  "The schedule command always reads a reason: the proto requires one."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "maintenance")))
      (agent-repl-daemon-shutdown-schedule 5)
      (let* ((action (plist-get (agent-repl-test-verbs--request :shutdown-schedule) :action))
             (reason (plist-get (plist-get action :value) :reason)))
        (should (eq (plist-get reason :arm) :maintenance))))))

(ert-deftest agent-repl-verbs-operator-reason-refuses-a-blank-note ()
  "An operator reason's note is REQUIRED non-blank, refused before send."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "operator"))
              ((symbol-function 'read-string) (lambda (&rest _) "   ")))
      (should-error (agent-repl-daemon-shutdown-schedule 5) :type 'user-error)
      (should-not agent-repl-test-verbs--sent))))

(ert-deftest agent-repl-verbs-operator-reason-carries-its-note ()
  "A non-blank operator note travels inside the arm that owns it."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "operator"))
              ((symbol-function 'read-string) (lambda (&rest _) "rolling the daemon")))
      (agent-repl-daemon-shutdown-now)
      (let* ((action (plist-get (agent-repl-test-verbs--request :shutdown-schedule) :action))
             (reason (plist-get (plist-get action :value) :reason)))
        (should (equal (plist-get (plist-get reason :value) :note) "rolling the daemon"))))))

;;;; ---- Merge queue ----

(defmacro agent-repl-test-verbs--with-repository (repository &rest body)
  "Run BODY with the roster answering REPOSITORY for any workspace id."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-repl-roster-repository-of)
              (lambda (&rest _) ,repository)))
     ,@body))

(ert-deftest agent-repl-verbs-merge-queue-pause-names-the-current-repository ()
  "THE QUEUE IS PER REPOSITORY, so a plain pause names the one the current
workspace's roster section is keyed by."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-repository (agent-repl-test-verbs--repo-ref)
      (agent-repl-merge-queue-pause)
      (should (equal (plist-get (agent-repl-test-verbs--request :merge-queue) :action)
                     (list :arm :pause
                           :value (list :repository (agent-repl-test-verbs--repo-ref))))))))

(ert-deftest agent-repl-verbs-merge-queue-pause-daemon-wide-omits-the-repository ()
  "A prefix argument means every repository that has a queue: UNSET."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-repository
        (error "a daemon-wide pause must not consult the roster")
      (agent-repl-merge-queue-pause t)
      (should (equal (plist-get (agent-repl-test-verbs--request :merge-queue) :action)
                     (list :arm :pause :value (list :repository nil)))))))

(ert-deftest agent-repl-verbs-merge-queue-pause-without-a-repository-refuses ()
  "No section for the workspace means no scope to send; pausing every
repository instead would exceed what the caller asked for."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-repository nil
      (should-error (agent-repl-merge-queue-pause) :type 'user-error))))

(ert-deftest agent-repl-verbs-merge-queue-resume-names-the-current-repository ()
  "Resume carries the same per-repository scope as pause."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-repository (agent-repl-test-verbs--repo-ref)
      (agent-repl-merge-queue-resume)
      (should (equal (plist-get (agent-repl-test-verbs--request :merge-queue) :action)
                     (list :arm :resume
                           :value (list :repository (agent-repl-test-verbs--repo-ref))))))))

(ert-deftest agent-repl-verbs-merge-queue-resume-daemon-wide-omits-the-repository ()
  "A prefix argument resumes every repository that has a queue."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-repository
        (error "a daemon-wide resume must not consult the roster")
      (agent-repl-merge-queue-resume t)
      (should (equal (plist-get (agent-repl-test-verbs--request :merge-queue) :action)
                     (list :arm :resume :value (list :repository nil)))))))

(ert-deftest agent-repl-verbs-merge-queue-unknown-repository-names-the-repository ()
  "The arm is EMPTY, so the repository the user is told about is the one this
pause sent."
  (agent-repl-test-verbs--with
      '((:merge-queue . (:response (:arm :error
                                    :value (:cause (:arm :unknown-repository :value nil))))))
    (agent-repl-test-verbs--with-repository (agent-repl-test-verbs--repo-ref)
      (agent-repl-merge-queue-pause)
      (should (agent-repl-test-verbs--messaged-p
               (format "merge-queue refused: the daemon's registry does not hold repository %S"
                       (agent-repl-test-verbs--repo-ref)))))))

(ert-deftest agent-repl-verbs-merge-queue-unknown-repository-logs-the-repository ()
  "The dynamic values go in the log context, never baked into the slug."
  (let ((records nil))
    (agent-repl-test-verbs--with
        '((:merge-queue . (:response (:arm :error
                                      :value (:cause (:arm :unknown-repository :value nil))))))
      (agent-repl-test-verbs--with-repository (agent-repl-test-verbs--repo-ref)
        (cl-letf (((symbol-function 'agent-repl--warn)
                   (lambda (_ws fmt &rest args) (push (apply #'format fmt args) records))))
          (agent-repl-merge-queue-pause))))
    (should (cl-find-if
             (lambda (record)
               (string-prefix-p "elisp.verbs.merge-queue-unknown-repository action=:pause"
                                record))
             records))))

(ert-deftest agent-repl-verbs-merge-queue-other-arms-still-report-generically ()
  "Claiming one arm must not swallow the rest: another arm reports as always."
  (agent-repl-test-verbs--with
      '((:merge-queue . (:response (:arm :error
                                    :value (:cause (:arm :already-paused :value nil))))))
    (agent-repl-test-verbs--with-repository (agent-repl-test-verbs--repo-ref)
      (agent-repl-merge-queue-pause)
      (should (agent-repl-test-verbs--messaged-p "merge-queue refused: already-paused")))))

(ert-deftest agent-repl-verbs-merge-queue-evict-names-the-workspace ()
  "Evict takes ONE workspace's merge off the queue, named by its ref."
  (agent-repl-test-verbs--with nil
    (agent-repl-merge-queue-evict)
    (let ((action (plist-get (agent-repl-test-verbs--request :merge-queue) :action)))
      (should (eq (plist-get action :arm) :evict))
      (should (equal (plist-get (plist-get action :value) :workspace)
                     (agent-repl-test-verbs--ref))))))

;;;; ---- Health ----

(defun agent-repl-test-verbs--health-text ()
  "Return the health buffer's contents."
  (with-current-buffer (get-buffer-create agent-repl-verbs-health-buffer)
    (buffer-string)))

(ert-deftest agent-repl-verbs-daemon-health-healthy-says-so ()
  "A healthy daemon renders the verdict and no faults."
  (agent-repl-test-verbs--with
      '((:daemon-health . (:response (:arm :success
                                      :value (:arm :healthy :value nil)))))
    (with-current-buffer (get-buffer-create agent-repl-verbs-health-buffer) (erase-buffer))
    (agent-repl-daemon-health)
    (should (string-match-p "daemon: HEALTHY" (agent-repl-test-verbs--health-text)))))

(ert-deftest agent-repl-verbs-daemon-health-unhealthy-prints-each-fault ()
  "UNHEALTHY IS AN ANSWER: its faults' details are printed verbatim."
  (agent-repl-test-verbs--with
      '((:daemon-health
         . (:response (:arm :success
                       :value (:arm :unhealthy
                               :value (:faults ((:detail "shim adoption stalled"
                                                 :kind (:arm :adoption-window-expired
                                                        :value nil))
                                                (:detail "store socket absent"
                                                 :kind (:arm :log-sink-poisoned
                                                        :value nil)))))))))
    (with-current-buffer (get-buffer-create agent-repl-verbs-health-buffer) (erase-buffer))
    (agent-repl-daemon-health)
    (let ((text (agent-repl-test-verbs--health-text)))
      (should (string-match-p "daemon: UNHEALTHY (2 fault(s))" text))
      (should (string-match-p "shim adoption stalled" text))
      (should (string-match-p "store socket absent" text)))))

(ert-deftest agent-repl-verbs-session-health-includes-standing-host-faults ()
  "The host stream's STANDING faults are printed beside the pulled ones."
  (agent-repl-test-verbs--with
      '((:session-health . (:response (:arm :success
                                       :value (:arm :healthy :value nil)))))
    (cl-letf (((symbol-function 'agent-repl-host-faults)
               (lambda (_ws) (list (list :detail "generation fault window open"
                                         :kind (list :arm :link-severed :value nil))))))
      (with-current-buffer (get-buffer-create agent-repl-verbs-health-buffer) (erase-buffer))
      (agent-repl-session-health "ws-one")
      (let ((text (agent-repl-test-verbs--health-text)))
        (should (string-match-p "session ws-one: HEALTHY" text))
        (should (string-match-p "standing host faults (1)" text))
        (should (string-match-p "generation fault window open" text))))))

(ert-deftest agent-repl-verbs-daemon-fault-line-names-its-kind ()
  "A DaemonFault renders its KIND beside the detail, never the detail alone."
  (agent-repl-test-verbs--with
      '((:daemon-health
         . (:response (:arm :success
                       :value (:arm :unhealthy
                               :value (:faults ((:detail "prompts dir gone"
                                                 :kind (:arm :prompts-dir-missing
                                                        :value nil)))))))))
    (with-current-buffer (get-buffer-create agent-repl-verbs-health-buffer) (erase-buffer))
    (agent-repl-daemon-health)
    (should (string-match-p "prompts-dir-missing: prompts dir gone"
                            (agent-repl-test-verbs--health-text)))))

(ert-deftest agent-repl-verbs-session-fault-line-names-its-kind ()
  "A SessionFault renders its KIND beside the detail, through the same formatter."
  (agent-repl-test-verbs--with
      '((:session-health
         . (:response (:arm :success
                       :value (:arm :unhealthy
                               :value (:faults ((:detail "shim exited 2"
                                                 :kind (:arm :shim-start-failed
                                                        :value nil)))))))))
    (cl-letf (((symbol-function 'agent-repl-host-faults) (lambda (_ws) nil)))
      (with-current-buffer (get-buffer-create agent-repl-verbs-health-buffer) (erase-buffer))
      (agent-repl-session-health "ws-one")
      (should (string-match-p "shim-start-failed: shim exited 2"
                              (agent-repl-test-verbs--health-text))))))

(ert-deftest agent-repl-verbs-standing-host-fault-line-names-its-kind ()
  "A standing HostFault renders its KIND beside the detail (audit-3 #29)."
  (agent-repl-test-verbs--with
      '((:session-health . (:response (:arm :success
                                       :value (:arm :healthy :value nil)))))
    (cl-letf (((symbol-function 'agent-repl-host-faults)
               (lambda (_ws) (list (list :detail "store socket unreachable"
                                         :kind (list :arm :bounce-unknown :value nil))))))
      (with-current-buffer (get-buffer-create agent-repl-verbs-health-buffer) (erase-buffer))
      (agent-repl-session-health "ws-one")
      (should (string-match-p "bounce-unknown: store socket unreachable"
                              (agent-repl-test-verbs--health-text))))))

(ert-deftest agent-repl-verbs-session-health-echoes-the-ref ()
  "SessionHealth is a per-workspace pull and carries the ref."
  (agent-repl-test-verbs--with
      '((:session-health . (:response (:arm :success
                                       :value (:arm :healthy :value nil)))))
    (agent-repl-session-health "ws-one")
    (should (equal (agent-repl-test-verbs--request :session-health)
                   (list :workspace (agent-repl-test-verbs--ref))))))

;;;; ---- Priority reading ----

(ert-deftest agent-repl-verbs-read-priority-clear-label-answers-nil ()
  "The clear entry answers nil, which is how absence is spelled."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) agent-repl-verbs-priority-clear-label)))
    (should-not (agent-repl-verbs--read-priority))))

(ert-deftest agent-repl-verbs-read-priority-label-answers-its-arm ()
  "A level label answers that level's arm."
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "P0.5")))
    (should (equal (agent-repl-verbs--read-priority) :p05))))

;;;; ---- Flat arms to codec oneofs ----

(ert-deftest agent-repl-verbs-arm-wraps-a-flat-arms-fields ()
  "A flat arm's own fields become the oneof's VALUE plist."
  (should (equal (agent-repl-verbs--arm (list :arm :evict :workspace "ref"))
                 (list :arm :evict :value (list :workspace "ref")))))

(ert-deftest agent-repl-verbs-arm-gives-an-empty-arm-a-nil-value ()
  "An arm carrying nothing gets a nil VALUE: being set is its whole assertion."
  (should (equal (agent-repl-verbs--arm (list :arm :pause))
                 (list :arm :pause :value nil))))

(ert-deftest agent-repl-verbs-arm-of-nothing-is-nothing ()
  "No arm at all translates to nil rather than an unset-arm oneof."
  (should-not (agent-repl-verbs--arm nil)))

(ert-deftest agent-repl-verbs-level-arm-builds-the-priority-oneof ()
  "A bare level keyword becomes the level arm; the levels carry nothing."
  (should (equal (agent-repl-verbs--level-arm :p2) (list :arm :p2 :value nil))))

(ert-deftest agent-repl-verbs-level-arm-of-nil-clears ()
  "No level is the ABSENCE of the field, never a sentinel arm."
  (should-not (agent-repl-verbs--level-arm nil)))

(ert-deftest agent-repl-verbs-shutdown-action-translates-the-nested-reason ()
  "The `DrainReason' nested in a flat action is translated on the way past."
  (should (equal (agent-repl-verbs--shutdown-action
                  (list :arm :now :reason (list :arm :operator :note "n")))
                 (list :arm :now
                       :value (list :reason (list :arm :operator
                                                  :value (list :note "n")))))))

;;;; ---- Creation forms built at the verb boundary ----

(ert-deftest agent-repl-verbs-create-standard-wraps-the-prompt-as-user-said ()
  "A standard form's initial prompt rides as `UserSaid', not as bare text."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard
                            :initial-prompt "fix the flake")
    (let* ((form (plist-get (agent-repl-test-verbs--request :create) :form))
           (blocks (plist-get (plist-get (plist-get (plist-get form :value)
                                                    :initial-prompt)
                                         :content)
                              :blocks)))
      (should (equal (plist-get (plist-get (car blocks) :value) :text) "fix the flake")))))

(ert-deftest agent-repl-verbs-create-one-shot-self-merge-is-an-empty-arm ()
  "The `self_merge' finish arm carries nothing: the arm is the whole fact."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :one-shot
                            :prompt "land it" :finish :self-merge)
    (let ((form (plist-get (agent-repl-test-verbs--request :create) :form)))
      (should (equal (plist-get (plist-get form :value) :finish)
                     (list :arm :self-merge :value nil))))))

(ert-deftest agent-repl-verbs-create-one-shot-open-pr-states-false-explicitly ()
  "`open_pr's two bools are plain bools: false is a VALUE, never an absence."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :one-shot
                            :prompt "land it" :finish :open-pr
                            :self-certified nil :add-to-merge-queue nil)
    (let* ((form (plist-get (agent-repl-test-verbs--request :create) :form))
           (finish (plist-get (plist-get form :value) :finish)))
      (should (equal (plist-get finish :value)
                     (list :self-certified nil :add-to-merge-queue nil))))))

(ert-deftest agent-repl-verbs-create-standard-wraps-a-merge-action-as-user-said ()
  "A standard form's pre-merge action rides as `UserSaid', not as bare text."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard
                            :merge-actions (list :before-ws-merge "run the linter"))
    (let* ((form (plist-get (agent-repl-test-verbs--request :create) :form))
           (blocks (plist-get (plist-get (plist-get (plist-get (plist-get form :value)
                                                              :merge-actions)
                                                    :before-ws-merge)
                                         :content)
                              :blocks)))
      (should (equal (plist-get (plist-get (car blocks) :value) :text) "run the linter")))))

(ert-deftest agent-repl-verbs-create-standard-wraps-a-postprocessing-prompt-as-user-said ()
  "A standard form's postprocessing prompt rides as `UserSaid' too."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard
                            :merge-actions (list :postprocessing-prompt "tidy up"))
    (let* ((form (plist-get (agent-repl-test-verbs--request :create) :form))
           (blocks (plist-get (plist-get (plist-get (plist-get (plist-get form :value)
                                                              :merge-actions)
                                                    :postprocessing-prompt)
                                         :content)
                              :blocks)))
      (should (equal (plist-get (plist-get (car blocks) :value) :text) "tidy up")))))

(ert-deftest agent-repl-verbs-create-standard-without-merge-actions-omits-them ()
  "No merge actions is the ABSENCE of the message, never an empty one."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard)
    (let ((form (plist-get (agent-repl-test-verbs--request :create) :form)))
      (should-not (plist-get (plist-get form :value) :merge-actions)))))

(ert-deftest agent-repl-verbs-create-fork-without-a-parent-is-refused ()
  "A fork without a parent is unrepresentable, so it is refused, not encoded."
  (agent-repl-test-verbs--with nil
    ;; Act / Assert
    (should-error (agent-repl-verb-create (agent-repl-test-verbs--repo-ref)
                                          :standard :fork t)
                  :type 'user-error)))

(ert-deftest agent-repl-verbs-create-fork-without-a-parent-sends-nothing ()
  "The refusal is BEFORE the send: silently dropping the fork would create
a plain workspace the caller never asked for."
  (agent-repl-test-verbs--with nil
    ;; Act
    (ignore-errors
      (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard :fork t))
    ;; Assert
    (should-not agent-repl-test-verbs--sent)))

(ert-deftest agent-repl-verbs-create-fork-rides-inside-the-parent ()
  "FORK lives INSIDE the parent: a fork without a parent is unrepresentable."
  (agent-repl-test-verbs--with nil
    (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :standard
                            :parent (agent-repl-test-verbs--ref) :fork t)
    (should (equal (plist-get (agent-repl-test-verbs--request :create) :parent)
                   (list :workspace (agent-repl-test-verbs--ref) :fork t)))))

(ert-deftest agent-repl-verbs-create-unknown-form-refuses ()
  "A form arm the verb does not know is refused before anything is sent."
  (agent-repl-test-verbs--with nil
    (should-error (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :bogus)
                  :type 'user-error)))

(ert-deftest agent-repl-verbs-create-unknown-finish-refuses ()
  "A finish arm the verb does not know is refused before anything is sent."
  (agent-repl-test-verbs--with nil
    (should-error (agent-repl-verb-create (agent-repl-test-verbs--repo-ref) :one-shot
                                          :prompt "p" :finish :bogus)
                  :type 'user-error)))

;;;; ---- Resolution and logging ----

(ert-deftest agent-repl-verbs-conn-stands-the-link-when-none-is-up ()
  "A daemon-admin verb with no standing link STANDS one rather than refusing."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl-link-primary) (lambda () nil))
              ((symbol-function 'agent-repl-link-connect) (lambda () 'stood-conn)))
      (should (eq (agent-repl-verbs--conn) 'stood-conn)))))

(ert-deftest agent-repl-verbs-conn-refuses-when-the-link-cannot-be-stood ()
  "No daemon anywhere is a refusal, not a request sent into nothing."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl-link-primary) (lambda () nil))
              ((symbol-function 'agent-repl-link-connect) (lambda () nil)))
      (should-error (agent-repl-verbs--conn) :type 'user-error))))

(ert-deftest agent-repl-verbs-refusal-slug-names-the-verb ()
  "A refusal's operation slug is `elisp.verbs.<op>-refused'."
  (let ((formats nil))
    (agent-repl-test-verbs--with
        '((:merge . (:response (:arm :error
                                :value (:cause (:arm :already-queued :value nil))))))
      (cl-letf (((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest _args) (push fmt formats))))
        (agent-repl-verb-merge "ws-one")))
    (should (cl-find-if (lambda (fmt) (string-prefix-p "elisp.verbs.merge-refused" fmt))
                        formats))))

;;;; ---- Current-perspective target resolution ----

(defmacro agent-repl-test-verbs--with-registry (names dirs current &rest body)
  "Run BODY with NAMES live, DIRS as their `:project-dir's and CURRENT active.
NAMES that are absent from DIRS own no directory, which is exactly how a
persp-mode perspective such as \"main\" appears in the registry."
  (declare (indent 3))
  `(cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () ,names))
             ((symbol-function 'agent-repl--ws-get)
              (lambda (ws key)
                (and (eq key :project-dir) (cdr (assoc ws ,dirs)))))
             ((symbol-function 'agent-repl--pseudo-workspace-name-p)
              (lambda (ws) (member ws '("main" "none"))))
             ((symbol-function 'agent-repl--ws-current-name) (lambda () ,current)))
     ,@body))

(ert-deftest agent-repl-verbs-close-with-an-empty-registry-refuses ()
  "No registered workspace at all: the picker's refusal, and no wire call."
  ;; Arrange
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-registry nil nil "main"
      ;; Act / Assert
      (should (equal (should-error (agent-repl-close-workspace) :type 'user-error)
                     '(user-error "No agent-repl workspaces registered")))
      (should (null agent-repl-test-verbs--sent)))))

(ert-deftest agent-repl-verbs-close-with-only-pseudo-workspaces-refuses ()
  "A registry holding only persp-mode's own perspectives is an EMPTY registry."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-registry '("main" "none") nil "main"
      (should (equal (should-error (agent-repl-close-workspace) :type 'user-error)
                     '(user-error "No agent-repl workspaces registered"))))))

(ert-deftest agent-repl-verbs-close-from-a-pseudo-perspective-picks ()
  "Current is \"main\" but real workspaces exist: the user PICKS one."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-registry '("main" "ws-one") '(("ws-one" . "/tmp/ws-one")) "main"
      (cl-letf (((symbol-function 'agent-repl--read-known-workspace)
                 (lambda (_prompt) "ws-one")))
        (agent-repl-close-workspace))
      (should (equal (agent-repl-test-verbs--request :close)
                     (list :workspace (agent-repl-test-verbs--ref)))))))

(ert-deftest agent-repl-verbs-close-from-a-registered-workspace-acts-on-it ()
  "A registered current workspace is acted on directly, with no picker."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-registry '("ws-one") '(("ws-one" . "/tmp/ws-one")) "ws-one"
      (cl-letf (((symbol-function 'agent-repl--read-known-workspace)
                 (lambda (_prompt) (error "the picker must not run"))))
        (agent-repl-close-workspace))
      (should (agent-repl-test-verbs--request :close)))))

(ert-deftest agent-repl-verbs-close-without-a-ref-keeps-the-identity-refusal ()
  "A REAL registered workspace with no daemon ref still refuses on identity."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-registry '("ws-one") '(("ws-one" . "/tmp/ws-one")) "ws-one"
      (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (_ws) nil)))
        (should (equal (should-error (agent-repl-close-workspace) :type 'user-error)
                       '(user-error "agent-repl: workspace ws-one has no daemon identity yet")))))))

(ert-deftest agent-repl-verbs-kill-with-an-empty-registry-refuses ()
  "The kill verb defaults to the current perspective too, and refuses alike."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-registry nil nil "main"
      (should (equal (should-error (agent-repl-kill-workspace) :type 'user-error)
                     '(user-error "No agent-repl workspaces registered")))
      (should (null agent-repl-test-verbs--sent)))))

(ert-deftest agent-repl-verbs-restart-with-an-empty-registry-refuses ()
  "So does restart: same default, same refusal, nothing sent."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-registry nil nil "main"
      (should (equal (should-error (agent-repl-restart-workspace) :type 'user-error)
                     '(user-error "No agent-repl workspaces registered")))
      (should (null agent-repl-test-verbs--sent)))))

(ert-deftest agent-repl-verbs-merge-with-an-empty-registry-refuses ()
  "And merge, which shares the same current-perspective default."
  (agent-repl-test-verbs--with nil
    (agent-repl-test-verbs--with-registry nil nil "main"
      (should (equal (should-error (agent-repl-merge-workspace) :type 'user-error)
                     '(user-error "No agent-repl workspaces registered")))
      (should (null agent-repl-test-verbs--sent)))))


;;;; ---- Model selection ----

(ert-deftest agent-repl-verbs-read-model-seeds-the-configured-model ()
  "The picker opens on `agent-repl-interactive-model' as its initial input."
  (let ((agent-repl-interactive-model "sonnet")
        (seen-initial :unset))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p _c &optional _pr _rm initial &rest _)
                 (setq seen-initial initial) initial)))
      (should (equal (agent-repl-verbs--read-model) "sonnet"))
      (should (equal seen-initial "sonnet")))))

(ert-deftest agent-repl-verbs-read-model-seeds-nothing-when-unset ()
  "A nil setting seeds no initial input, so the prompt opens blank."
  (let ((agent-repl-interactive-model nil)
        (seen-initial :unset))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p _c &optional _pr _rm initial &rest _)
                 (setq seen-initial initial) "")))
      (should-not (agent-repl-verbs--read-model))
      (should-not seen-initial))))

(ert-deftest agent-repl-verbs-read-model-seeds-todays-default ()
  "Unconfigured, the picker opens on the defcustom's shipped default."
  (let ((seen-initial :unset))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p _c &optional _pr _rm initial &rest _)
                 (setq seen-initial initial) initial)))
      (agent-repl-verbs--read-model)
      (should (equal seen-initial
                     (eval (car (get 'agent-repl-interactive-model
                                     'standard-value))
                           t))))))

(ert-deftest agent-repl-verbs-read-model-honors-an-erased-seed ()
  "Erasing the seed back to blank still asks the daemon to choose."
  (let ((agent-repl-interactive-model "opus"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "   ")))
      (should-not (agent-repl-verbs--read-model)))))

(provide 'test-verbs)

;;; test-verbs.el ends here

(ert-deftest agent-repl-verbs-a-landing-still-waiting-for-its-tab-is-recorded ()
  "A landing that did not fire says what it is still waiting for.
Nothing here polls or times out, so for as long as the tab did not come
the log went silent between `pending-landing-registered' and nothing at
all -- which is what three minutes of waiting on a register whose row
came back CLOSED left behind."
  ;; Arrange.
  (agent-repl-test-verbs--with nil
    (let (logs)
      (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil)))
        (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "waiting" "/tmp/w"))
        (cl-letf (((symbol-function 'agent-repl--log)
                   (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs))))
          ;; Act.
          (agent-repl-verbs--pending-landing-fire)))
      ;; Assert.
      (should (seq-some (lambda (text)
                          (string-search "elisp.verbs.pending-landing-waiting id=waiting" text))
                        logs)))))

(ert-deftest agent-repl-verbs-a-landing-that-fires-records-no-wait ()
  "The wait record names a landing that is STILL pending, never one that landed."
  ;; Arrange.
  (agent-repl-test-verbs--with nil
    (let (logs)
      (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil)))
        (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "arrives" "/tmp/w")))
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs)))
                ((symbol-function 'agent-repl-switch-to-project) #'ignore))
        ;; Act.
        (agent-repl-test-verbs--tab-arrives "arrives" "arrived-ws"))
      ;; Assert.
      (should-not (seq-some (lambda (text)
                              (string-search "elisp.verbs.pending-landing-waiting" text))
                            logs)))))

(ert-deftest agent-repl-verbs-a-waiting-landing-is-not-cleared-by-the-record ()
  "Recording the wait must not consume the landing: the tab is still coming."
  ;; Arrange.
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil)))
      (agent-repl-verbs-select-minted (agent-repl-test-verbs--ref "kept" "/tmp/w"))
      ;; Act.
      (agent-repl-verbs--pending-landing-fire))
    ;; Assert.
    (should (equal (plist-get agent-repl-verbs--pending-landing :id) "kept"))))
