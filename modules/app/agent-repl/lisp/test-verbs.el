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

(defun agent-repl-test-verbs--ref (&optional id dir)
  "Return a decoded `WorkspaceRef' plist, echoed verbatim by production."
  (list :id (or id "ws-id-1") :dir (or dir "/tmp/agent-repl-test/ws-1")))

(defun agent-repl-test-verbs--repo-ref (&optional id dir)
  "Return a decoded `RepositoryRef' plist."
  (list :id (or id "repo-id-1") :dir (or dir "/tmp/agent-repl-test/repo")))

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
         (agent-repl-test-verbs--torn-down nil)
         (agent-repl-test-verbs--messages nil)
         (agent-repl-test-verbs--handover nil)
         (agent-repl-test-verbs--answers ,answers))
     (cl-letf* (((symbol-function 'agent-repl-host-ref)
                 (lambda (_ws) (agent-repl-test-verbs--ref)))
                ((symbol-function 'agent-repl-host-conn) (lambda (_ws) 'test-conn))
                ((symbol-function 'agent-repl-host-faults) (lambda (_ws) nil))
                ((symbol-function 'agent-repl-host-handle-refusal)
                 (lambda (ws arm) (push (list ws arm) agent-repl-test-verbs--handover)))
                ((symbol-function 'agent-repl-link-primary) (lambda () 'test-conn))
                ((symbol-function 'agent-repl--ws-current-name) (lambda () "ws-one"))
                ((symbol-function 'agent-repl--kill-one-workspace)
                 (lambda (ws &optional _p) (push ws agent-repl-test-verbs--torn-down)))
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

(defvar agent-repl-test-verbs--answers nil
  "The scripted answers for the rpc stubs, bound by the `--with' macro.")

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

(ert-deftest agent-repl-verbs-create-command-blank-name-is-absence ()
  "A blank name is ABSENCE -- the daemon mints one -- never an empty string."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl-verbs--read-repository)
               (lambda () (agent-repl-test-verbs--repo-ref)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "do a thing"))
              ((symbol-function 'read-string) (lambda (&rest _) "")))
      (agent-repl-create-workspace nil)
      (let ((standard (plist-get (plist-get (agent-repl-test-verbs--request :create) :form)
                                 :value)))
        (should-not (plist-get standard :name))
        (should-not (plist-get standard :base-ref))))))

(ert-deftest agent-repl-verbs-create-command-prefix-arg-sets-the-parent ()
  "A prefix argument makes the new workspace a CHILD of the current one."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl-verbs--read-repository)
               (lambda () (agent-repl-test-verbs--repo-ref)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "do a thing"))
              ((symbol-function 'read-string) (lambda (&rest _) "")))
      (agent-repl-create-workspace t)
      (should (equal (plist-get (agent-repl-test-verbs--request :create) :parent)
                     (list :workspace (agent-repl-test-verbs--ref)))))))

(ert-deftest agent-repl-verbs-fork-command-sets-fork-inside-the-parent ()
  "A fork lives INSIDE the parent: a fork without a parent is unrepresentable."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl-verbs--read-repository)
               (lambda () (agent-repl-test-verbs--repo-ref)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "fork it")))
      (agent-repl-fork-workspace)
      (should (eq (plist-get (plist-get (agent-repl-test-verbs--request :create) :parent)
                             :fork)
                  t)))))

;;;; ---- Create: the one-shot form and both finish arms ----

(ert-deftest agent-repl-verbs-oneshot-self-merge-sets-that-finish-arm ()
  "The self-merge one-shot finishes through the ordinary merge engine."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl-verbs--read-repository)
               (lambda () (agent-repl-test-verbs--repo-ref)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "commission")))
      (agent-repl-create-oneshot-self-merge nil)
      (let ((one-shot (plist-get (plist-get (agent-repl-test-verbs--request :create) :form)
                                 :value)))
        (should (eq (plist-get (plist-get one-shot :finish) :arm) :self-merge))))))

(ert-deftest agent-repl-verbs-oneshot-open-pr-sets-both-flags-true ()
  "The queued one-shot PR is self-certified and added to the merge queue."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl-verbs--read-repository)
               (lambda () (agent-repl-test-verbs--repo-ref)))
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
    (cl-letf (((symbol-function 'agent-repl-verbs--read-repository)
               (lambda () (agent-repl-test-verbs--repo-ref)))
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
    (cl-letf (((symbol-function 'agent-repl-verbs--read-repository)
               (lambda () (agent-repl-test-verbs--repo-ref)))
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
    (cl-letf (((symbol-function 'agent-repl-verbs--read-repository)
               (lambda () (agent-repl-test-verbs--repo-ref)))
              ((symbol-function 'agent-repl-verbs--read-prompt) (lambda (_p) "   ")))
      (should-error (agent-repl-create-oneshot-self-merge nil) :type 'user-error)
      (should-not agent-repl-test-verbs--sent))))

(ert-deftest agent-repl-verbs-oneshot-prefix-arg-picks-a-model ()
  "A prefix argument threads the chosen model onto the shared facts."
  (agent-repl-test-verbs--with nil
    (cl-letf (((symbol-function 'agent-repl-verbs--read-repository)
               (lambda () (agent-repl-test-verbs--repo-ref)))
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
                               :value (:faults ((:detail "shim adoption stalled")
                                                (:detail "store socket absent"))))))))
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
               (lambda (_ws) (list (list :detail "generation fault window open")))))
      (with-current-buffer (get-buffer-create agent-repl-verbs-health-buffer) (erase-buffer))
      (agent-repl-session-health "ws-one")
      (let ((text (agent-repl-test-verbs--health-text)))
        (should (string-match-p "session ws-one: HEALTHY" text))
        (should (string-match-p "standing host faults (1)" text))
        (should (string-match-p "generation fault window open" text))))))

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

(provide 'test-verbs)

;;; test-verbs.el ends here
