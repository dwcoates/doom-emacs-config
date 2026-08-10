;;; test-frontend-state.el --- ERT tests for frontend-state.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Pure-elisp tests for the frontend.v1 state application layer:
;; RenderState -> render keyword mapping, WorkspaceState application into
;; the workspace.el pushed-state key, StateSnapshot resync, and
;; DegradedNotice surfacing.  No processes, no network — the frame plists
;; are constructed directly (the transport's decode is covered in
;; test-frontend-uds.el).  `message' is shadowed where echo-area output is
;; asserted.
;;
;; One edge case per test, AAA structure.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-frontend-state.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;; Transport before state: frontend-state.el registers its handlers into
;; frontend-uds.el at load, so load the transport first.
(load (expand-file-name "frontend-uds.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)
(defconst agent-repl-test--frontend-state-file
  (expand-file-name "frontend-state.el"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Absolute frontend-state module path captured while this test file loads.")
(load agent-repl-test--frontend-state-file nil t)

(require 'cl-lib)

;;;; ---- RenderState -> keyword: one test per enum value -----------------

(defmacro agent-repl-test--deftest-state-map (name state keyword)
  "Define a mapping test NAME asserting STATE maps to render KEYWORD."
  `(ert-deftest ,name ()
     ,(format "RenderState %s maps to %s." state keyword)
     (should (eq (agent-repl--frontend-state->keyword ,state) ,keyword))))

(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-init "RENDER_STATE_INIT" :init)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-idle "RENDER_STATE_IDLE" :idle)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-idle-async "RENDER_STATE_IDLE_ASYNC" :idle-async)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-thinking "RENDER_STATE_THINKING" :thinking)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-permission "RENDER_STATE_PERMISSION" :permission)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-done "RENDER_STATE_DONE" :done)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-stop-failed "RENDER_STATE_STOP_FAILED" :vendor-blocked)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-ready "RENDER_STATE_READY" :ready)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-vendor-blocked "RENDER_STATE_VENDOR_BLOCKED" :vendor-blocked)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-merge-enqueuing "RENDER_STATE_MERGE_ENQUEUING" :merge-enqueuing)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-merging "RENDER_STATE_MERGING" :merging)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-merge-queued "RENDER_STATE_MERGE_QUEUED" :merge-queued)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-merge-conflict "RENDER_STATE_MERGE_CONFLICT" :merge-conflict)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-merge-failed "RENDER_STATE_MERGE_FAILED" :merge-failed)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-merged "RENDER_STATE_MERGED" :merged)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-dead "RENDER_STATE_DEAD" :dead)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-degraded "RENDER_STATE_DEGRADED" :degraded)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-severed "RENDER_STATE_SEVERED" :severed)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-hibernated "RENDER_STATE_HIBERNATED" :hibernated)

(ert-deftest agent-repl-test-state-map-severed-and-hibernated-are-distinct ()
  "The two halves of the old RENDER_STATE_DORMANT decode to DIFFERENT keywords.
A single state used to mean both \"we put this session to sleep on
purpose\" and \"the backend substrate is broken\", so the idle sweeper
reclaiming memory from an untouched workspace was indistinguishable from
a dead shim.  Collapsing them back onto one keyword here would restore
that exactly, whatever the color tables said."
  ;; Act / Assert
  (should-not (eq (agent-repl--frontend-state->keyword "RENDER_STATE_SEVERED")
                  (agent-repl--frontend-state->keyword "RENDER_STATE_HIBERNATED"))))

(ert-deftest agent-repl-test-state-map-unspecified-errors ()
  "RENDER_STATE_UNSPECIFIED is unresolved and errors (no fallback keyword)."
  ;; Act / Assert
  (should-error (agent-repl--frontend-state->keyword "RENDER_STATE_UNSPECIFIED")))

(ert-deftest agent-repl-test-state-map-unknown-errors ()
  "An unknown RenderState string errors (no fallback keyword)."
  ;; Act / Assert
  (should-error (agent-repl--frontend-state->keyword "RENDER_STATE_WAT")))

(ert-deftest agent-repl-test-state-map-nil-errors ()
  "A nil state (protojson omits a default enum) errors, never defaults."
  ;; Act / Assert
  (should-error (agent-repl--frontend-state->keyword nil)))

;;;; ---- WorkspaceState application --------------------------------------

(defun agent-repl-test--register-ws (name &optional dir)
  "Register live workspace NAME owning DIR, so a pushed frame resolves to it.
The daemon names workspaces by CWD; `agent-repl--frontend-ws-name' maps that
back to the persp NAME, and a name with no live workspace behind it is
dropped rather than stub-created."
  (agent-repl--ws-put name :project-dir (or dir (concat "/tmp/" name))))

(defun agent-repl-test--complete-workspace-state (state)
  "Return test STATE with the mandatory composite daemon verdict."
  (append state
          '(:sessionId "s_test"
            :connectivity "SESSION_CONNECTIVITY_OPERATIONAL"
            :status "SESSION_STATUS_READY"
            :controllerGenerationId "g_test"
            :activeFaults nil)))

(defun agent-repl-test--apply-workspace-state (state)
  "Apply test STATE after adding mandatory composite fields."
  (agent-repl--frontend-apply-workspace-state
   (agent-repl-test--complete-workspace-state state)))

(defun agent-repl-test--apply-snapshot (snapshot)
  "Apply test SNAPSHOT after completing every workspace state."
  (let ((copy (copy-sequence snapshot)))
    (setq copy
          (plist-put
           copy :workspaces
           (mapcar #'agent-repl-test--complete-workspace-state
                   (plist-get snapshot :workspaces))))
    (agent-repl--frontend-apply-snapshot copy)))

(ert-deftest agent-repl-test-composite-state-stores-both-authoritative-facts ()
  "Connectivity and status remain separately queryable in Emacs."
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl--frontend-apply-workspace-state
     '(:workspace "ws1" :sessionId "s1"
       :state "RENDER_STATE_DEGRADED"
       :connectivity "SESSION_CONNECTIVITY_DEGRADED"
       :status "SESSION_STATUS_THINKING"
       :controllerGenerationId "g1"
       :activeFaults ((:component "shim-store-client"
                       :faultType "store-link"
                       :impact "connectivity"
                       :causeKind "store_subscription_lost"
                       :openedAtMs "42"))))
    (should (eq (agent-repl--ws-get "ws1" :pushed-session-connectivity)
                :degraded))
    (should (eq (agent-repl--ws-get "ws1" :pushed-session-status)
                :thinking))
    (let ((meta (agent-repl--ws-get "ws1" :pushed-render-state-meta)))
      (should (equal (plist-get meta :controller-generation-id) "g1"))
      (should (= (length (plist-get meta :active-faults)) 1)))))

(ert-deftest agent-repl-test-composite-state-rejects-missing-connectivity ()
  "A WorkspaceState without connectivity fails before mutating workspace state."
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (should-error
     (agent-repl--frontend-apply-workspace-state
      '(:workspace "ws1" :sessionId "s1"
        :state "RENDER_STATE_READY"
        :status "SESSION_STATUS_READY"
        :controllerGenerationId "g1")))
    (should-not (agent-repl--ws-get "ws1" :pushed-render-state))))

(ert-deftest agent-repl-test-composite-state-rejects-incomplete-controller-identity ()
  "Current connectivity without a generation fails before mutation."
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (should-error
     (agent-repl--frontend-apply-workspace-state
      '(:workspace "ws1" :sessionId "s1"
        :state "RENDER_STATE_READY"
        :connectivity "SESSION_CONNECTIVITY_OPERATIONAL"
        :status "SESSION_STATUS_READY")))
    (should-not (agent-repl--ws-get "ws1" :pushed-render-state))))

(ert-deftest agent-repl-test-composite-state-rejects-malformed-runtime-fault ()
  "A RuntimeFault without typed impact fails before mutation."
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (should-error
     (agent-repl--frontend-apply-workspace-state
      '(:workspace "ws1" :sessionId "s1"
        :state "RENDER_STATE_DEGRADED"
        :connectivity "SESSION_CONNECTIVITY_DEGRADED"
        :status "SESSION_STATUS_READY"
        :controllerGenerationId "g1"
        :activeFaults ((:component "shim-store-client"
                        :faultType "store-link")))))
    (should-not (agent-repl--ws-get "ws1" :pushed-render-state))))

(ert-deftest agent-repl-test-apply-workspace-state-stores-keyword ()
  "Applying a WorkspaceState stores the mapped keyword under :pushed-render-state."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (agent-repl-test--register-ws "ws1")
    ;; Act
    (agent-repl-test--apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_THINKING"))
    ;; Assert
    (should (eq (agent-repl--ws-get "ws1" :pushed-render-state) :thinking))))

(ert-deftest agent-repl-test-apply-workspace-state-returns-keyword ()
  "Applying a WorkspaceState returns the mapped keyword."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    ;; Act / Assert
    (should (eq (agent-repl-test--apply-workspace-state
                '(:workspace "ws1" :state "RENDER_STATE_DONE"))
               :done))))

(ert-deftest agent-repl-test-apply-workspace-state-overwrites ()
  "A later WorkspaceState overwrites the earlier pushed state (daemon is truth)."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (agent-repl-test--apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_THINKING"))
    ;; Act
    (agent-repl-test--apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_IDLE"))
    ;; Assert
    (should (eq (agent-repl--ws-get "ws1" :pushed-render-state) :idle))))

(ert-deftest agent-repl-test-apply-workspace-state-stores-inputs ()
  "The resolution inputs are stored under :pushed-render-state-meta."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (agent-repl-test--register-ws "ws1")
    ;; Act
    (agent-repl-test--apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_IDLE_ASYNC"
       :turnActive t :liveTaskCount "3"
       :causeKind "task_started" :causeSeq "42"))
    ;; Assert
    (let ((meta (agent-repl--ws-get "ws1" :pushed-render-state-meta)))
      (should (eq (plist-get meta :turn-active) t))
      (should (equal (plist-get meta :live-task-count) "3"))
      (should (equal (plist-get meta :cause-kind) "task_started"))
      (should (equal (plist-get meta :cause-seq) "42")))))

;;;; ---- The durable merge instant ---------------------------------------

(ert-deftest agent-repl-test-apply-workspace-state-retains-merged-at ()
  "`mergedAtMs' lands on :merge-completed-at as epoch SECONDS.
protojson encodes the int64 as a JSON string, which is the shape the
daemon actually puts on the wire."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    ;; Act
    (agent-repl-test--apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_MERGED" :mergedAtMs "1700000000500"))
    ;; Assert
    (should (= (agent-repl--ws-get "ws1" :merge-completed-at) 1700000000.5))))

(ert-deftest agent-repl-test-apply-workspace-state-retains-numeric-merged-at ()
  "A numeric `mergedAtMs' is retained exactly as a stringified one is."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    ;; Act
    (agent-repl-test--apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_MERGED" :mergedAtMs 1700000000500))
    ;; Assert
    (should (= (agent-repl--ws-get "ws1" :merge-completed-at) 1700000000.5))))

(ert-deftest agent-repl-test-apply-workspace-state-merged-at-survives-hibernation ()
  "The post-merge `:hibernated' frame keeps carrying the merge instant.
This is the frame sequence that made a merged workspace vanish from the
sidebar: the merge lands, then the daemon hibernates the session."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_MERGED" :mergedAtMs "1700000000000"))
    ;; Act
    (agent-repl--frontend-apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_HIBERNATED"
       :connectivity "SESSION_CONNECTIVITY_HIBERNATED"
       :status "SESSION_STATUS_READY"
       :mergedAtMs "1700000000000"))
    ;; Assert
    (should (eq (agent-repl--ws-get "ws1" :pushed-render-state) :hibernated))
    (should (= (agent-repl--ws-get "ws1" :merge-completed-at) 1700000000.0))))

(ert-deftest agent-repl-test-apply-workspace-state-absent-merged-at-keeps-known ()
  "An unmerged frame leaves an already-known merge instant alone.
protojson omits a zero int64, so \"not merged\" and \"field absent\" share
a wire shape; clearing on it would erase a restored merge."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl--ws-put "ws1" :merge-completed-at 1700000000.0)
    ;; Act
    (agent-repl-test--apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_READY"))
    ;; Assert
    (should (= (agent-repl--ws-get "ws1" :merge-completed-at) 1700000000.0))))

(ert-deftest agent-repl-test-apply-workspace-state-zero-merged-at-retains-nothing ()
  "A zero `mergedAtMs' records no merge (0 is the never-merged value)."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    ;; Act
    (agent-repl-test--apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_READY" :mergedAtMs "0"))
    ;; Assert
    (should-not (agent-repl--ws-get "ws1" :merge-completed-at))))

(ert-deftest agent-repl-test-apply-workspace-state-unparsable-merged-at-retains-nothing ()
  "A non-numeric `mergedAtMs' records no merge rather than a guessed 0."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    ;; Act
    (agent-repl-test--apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_READY" :mergedAtMs "later"))
    ;; Assert
    (should-not (agent-repl--ws-get "ws1" :merge-completed-at))))

(ert-deftest agent-repl-test-apply-workspace-state-merged-at-supersedes-loudly ()
  "A daemon instant that disagrees with the known one wins, with a log."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl--ws-put "ws1" :merge-completed-at 1600000000.0)
    (let (logged)
      ;; Act
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        (agent-repl-test--apply-workspace-state
         '(:workspace "ws1" :state "RENDER_STATE_MERGED" :mergedAtMs "1700000000000")))
      ;; Assert
      (should (= (agent-repl--ws-get "ws1" :merge-completed-at) 1700000000.0))
      (should (cl-find-if (lambda (line) (string-match-p "SUPERSEDES" line)) logged)))))

(ert-deftest agent-repl-test-workspace-state-rejects-pre-materialization-frame ()
  "An unowned WorkspaceState signals and logs identity before any host mutation."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--frontend-workspace-state-views (make-hash-table :test 'equal))
          logged)
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (ws fmt &rest args)
                   (push (list ws (apply #'format fmt args)) logged))))
        (should-error
         (agent-repl-test--apply-workspace-state
          '(:workspace "/pending/new" :sessionId "s_pending"
            :state "RENDER_STATE_MERGED" :mergedAtMs "1700000000000")))
        (let ((entry (car logged)))
          (should (null (car entry)))
          (should (string-match-p "REJECTED pre-materialization" (cadr entry)))
          (should (string-match-p "frame=WorkspaceState" (cadr entry)))
          (should (string-match-p "job-id=unannounced" (cadr entry)))
          (should (string-match-p "path=\\\"/pending/new\\\"" (cadr entry)))
          ;; A HOST frame still names itself by its session id, which is what
          ;; reaches the rejection as its identity.
          (should (string-match-p "identity=\\\"s_pending\\\"" (cadr entry))))
        (should (zerop (hash-table-count agent-repl--frontend-workspace-state-views)))
        (should-not (agent-repl--ws-known-p "/pending/new"))))))

(ert-deftest agent-repl-test-apply-workspace-state-missing-workspace-errors ()
  "A WorkspaceState with no workspace fails loudly (invariant violation)."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    ;; Act / Assert
    (should-error (agent-repl-test--apply-workspace-state
                   '(:state "RENDER_STATE_IDLE")))))

(ert-deftest agent-repl-test-apply-workspace-state-blank-workspace-errors ()
  "A WorkspaceState with a blank workspace fails loudly."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    ;; Act / Assert
    (should-error (agent-repl-test--apply-workspace-state
                   '(:workspace "" :state "RENDER_STATE_IDLE")))))

(ert-deftest agent-repl-test-apply-workspace-state-bad-state-errors ()
  "A WorkspaceState carrying an unmappable state fails loudly."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    ;; Act / Assert
    (should-error (agent-repl-test--apply-workspace-state
                   '(:workspace "ws1" :state "RENDER_STATE_UNSPECIFIED")))))

;;;; ---- MergeStatus: the phase IS the oneof arm -------------------------

(defmacro agent-repl-test--deftest-merge-phase (name arm keyword)
  "Define a test asserting `MergeStatus' oneof ARM derives phase KEYWORD.
NAME is the ert test-name suffix.  One arm per test — the derivation is
the whole contract, so a broken arm must name itself in the failure."
  `(ert-deftest ,(intern (format "agent-repl-test-merge-phase-%s" name)) ()
     ,(format "MergeStatus arm `%s' derives the phase %s." arm keyword)
     ;; Act / Assert
     (should (eq (plist-get (agent-repl--frontend-parse-merge-status
                             (list :runId "r1" ,arm nil) nil)
                            :phase)
                 ,keyword))))

(agent-repl-test--deftest-merge-phase enqueued :enqueued :enqueued)
(agent-repl-test--deftest-merge-phase before-action :beforeAction :before-action)
(agent-repl-test--deftest-merge-phase cherry-picking :cherryPicking :cherry-picking)
(agent-repl-test--deftest-merge-phase testing :testing :testing)
(agent-repl-test--deftest-merge-phase conflict :conflict :conflict)
(agent-repl-test--deftest-merge-phase after-action :afterAction :after-action)
(agent-repl-test--deftest-merge-phase merged :merged :merged)
(agent-repl-test--deftest-merge-phase failed :failed :failed)

(ert-deftest agent-repl-test-dequeue-offer-absent-is-nil ()
  "No `mergeDequeueOffer' on the frame decodes to nil, never an invented one.
The daemon clears the field to take the card down, so absence must not be
narrated as a question that is still standing."
  ;; Act / Assert
  (should-not (agent-repl--frontend-parse-merge-dequeue-offer nil nil)))

(ert-deftest agent-repl-test-dequeue-offer-decodes-the-waiting-standing ()
  "A waiting offer carries the standing keyword and its queue figures."
  ;; Arrange / Act
  (let ((parsed (agent-repl--frontend-parse-merge-dequeue-offer
                 '(:offerId "offer-1" :runId "run-7"
                   :waiting (:ahead "2" :position "3" :depth "5"))
                 nil)))
    ;; Assert
    (should (eq (plist-get parsed :standing) :waiting))
    (should (equal (list (plist-get parsed :ahead)
                         (plist-get parsed :position)
                         (plist-get parsed :depth))
                   '(2 3 5)))))

(ert-deftest agent-repl-test-dequeue-offer-decodes-the-running-standing ()
  "A running offer carries the standing keyword and NO queue figures.
The card that shows the run's stage is the webapp's; Emacs already holds
that run's status on `:pushed-merge-status', so a second copy of it here
would be a second thing to keep in step."
  ;; Arrange / Act
  (let ((parsed (agent-repl--frontend-parse-merge-dequeue-offer
                 '(:offerId "offer-1" :runId "run-7" :running nil) nil)))
    ;; Assert
    (should (eq (plist-get parsed :standing) :running))
    (should-not (plist-member parsed :position))))

(ert-deftest agent-repl-test-dequeue-offer-keeps-the-offer-id ()
  "The offer id rides every decoded offer — it is what an answer names."
  ;; Act / Assert
  (should (equal (plist-get (agent-repl--frontend-parse-merge-dequeue-offer
                             '(:offerId "offer-9" :runId "run-7" :running nil) nil)
                            :offer-id)
                 "offer-9")))

(ert-deftest agent-repl-test-dequeue-offer-no-arm-errors ()
  "An offer with no standing arm is malformed and fails loudly."
  ;; Act / Assert
  (should-error (agent-repl--frontend-parse-merge-dequeue-offer
                 '(:offerId "offer-1" :runId "run-7") nil)))

(ert-deftest agent-repl-test-dequeue-offer-two-arms-error ()
  "An offer with two standing arms is malformed and fails loudly."
  ;; Act / Assert
  (should-error (agent-repl--frontend-parse-merge-dequeue-offer
                 '(:offerId "offer-1" :runId "run-7"
                   :waiting (:ahead "1" :position "2" :depth "2") :running nil)
                 nil)))

(ert-deftest agent-repl-test-merge-status-absent-is-nil ()
  "No `mergeStatus' on the frame decodes to nil, never an invented phase."
  ;; Act / Assert
  (should-not (agent-repl--frontend-parse-merge-status nil nil)))

(ert-deftest agent-repl-test-merge-status-no-arm-errors ()
  "A MergeStatus with no oneof arm is malformed and fails loudly."
  ;; Act / Assert
  (should-error (agent-repl--frontend-parse-merge-status
                 '(:runId "r1" :updatedAtMs "5") nil)))

(ert-deftest agent-repl-test-merge-status-two-arms-error ()
  "A MergeStatus with two oneof arms is malformed and fails loudly."
  ;; Act / Assert
  (should-error (agent-repl--frontend-parse-merge-status
                 '(:runId "r1" :testing nil :conflict nil) nil)))

(ert-deftest agent-repl-test-merge-status-unknown-arm-field-errors ()
  "An arm field Emacs was never taught fails loudly (no silent drop)."
  ;; Act / Assert
  (should-error (agent-repl--frontend-parse-merge-status
                 '(:runId "r1" :testing (:someNewField "x")) nil)))

(ert-deftest agent-repl-test-merge-status-keeps-the-run-id ()
  "The run id rides every decoded status."
  ;; Act / Assert
  (should (equal (plist-get (agent-repl--frontend-parse-merge-status
                             '(:runId "run-7" :merged (:commitsTotal 3)) nil)
                            :run-id)
                 "run-7")))

(ert-deftest agent-repl-test-merge-status-parses-int64-instants ()
  "protojson int64 instants arrive as strings and decode to numbers."
  ;; Arrange / Act
  (let ((parsed (agent-repl--frontend-parse-merge-status
                 '(:runId "r1" :phaseStartedAtMs "1700000000000"
                   :updatedAtMs "1700000000500" :testing nil)
                 nil)))
    ;; Assert
    (should (equal (plist-get parsed :phase-started-at-ms) 1700000000000))
    (should (equal (plist-get parsed :updated-at-ms) 1700000000500))))

(ert-deftest agent-repl-test-merge-status-flattens-queue-position ()
  "The enqueued arm's position and depth land as flat plist keys."
  ;; Arrange / Act
  (let ((parsed (agent-repl--frontend-parse-merge-status
                 '(:runId "r1" :enqueued (:position 2 :depth 5)) nil)))
    ;; Assert
    (should (equal (plist-get parsed :position) 2))
    (should (equal (plist-get parsed :depth) 5))))

(ert-deftest agent-repl-test-merge-status-flattens-pick-progress ()
  "The cherry-picking arm's counts and current commit land as flat keys."
  ;; Arrange / Act
  (let ((parsed (agent-repl--frontend-parse-merge-status
                 '(:runId "r1" :cherryPicking
                   (:commitsTotal 4 :commitsLanded 2
                    :currentSha "abc1234" :currentSubject "fix the thing"))
                 nil)))
    ;; Assert
    (should (equal (plist-get parsed :commits-landed) 2))
    (should (equal (plist-get parsed :commits-total) 4))
    (should (equal (plist-get parsed :current-sha) "abc1234"))
    (should (equal (plist-get parsed :current-subject) "fix the thing"))))

(ert-deftest agent-repl-test-merge-status-flattens-the-conflicted-commit ()
  "The conflict arm names the commit that conflicted."
  ;; Act / Assert
  (should (equal (plist-get (agent-repl--frontend-parse-merge-status
                             '(:runId "r1" :conflict
                               (:conflictedSha "def5678"
                                :conflictedSubject "rename the widget"))
                             nil)
                            :conflicted-subject)
                 "rename the widget")))

(ert-deftest agent-repl-test-merge-status-flattens-the-failure-cause ()
  "The failed arm carries the daemon's cause verbatim."
  ;; Act / Assert
  (should (equal (plist-get (agent-repl--frontend-parse-merge-status
                             '(:runId "r1" :failed (:cause "lease unavailable"))
                             nil)
                            :cause)
                 "lease unavailable")))

(ert-deftest agent-repl-test-merge-status-flattens-the-failure-record ()
  "The failed arm's own JSON record lands VERBATIM as `:failed-json'.
The daemon serialized it with proto3's JSON mapping, so Emacs carries the
string it was handed rather than re-deriving a record from the sibling
fields beside it."
  ;; Act / Assert
  (should (equal (plist-get (agent-repl--frontend-parse-merge-status
                             '(:runId "r1" :failed
                               (:cause "lease unavailable"
                                :failedJson "{\"cause\":\"lease unavailable\"}"))
                             nil)
                            :failed-json)
                 "{\"cause\":\"lease unavailable\"}")))

(ert-deftest agent-repl-test-merge-status-flattens-the-after-action-error ()
  "A merged run reports an after-action that failed."
  ;; Act / Assert
  (should (equal (plist-get (agent-repl--frontend-parse-merge-status
                             '(:runId "r1" :merged
                               (:commitsTotal 3 :afterActionError "tests failed"))
                             nil)
                            :after-action-error)
                 "tests failed")))

(ert-deftest agent-repl-test-merge-status-flattens-the-action-prompt ()
  "A before-action arm carries the configured prompt for display."
  ;; Act / Assert
  (should (equal (plist-get (agent-repl--frontend-parse-merge-status
                             '(:runId "r1" :beforeAction (:prompt "bump the version"))
                             nil)
                            :prompt)
                 "bump the version")))

(ert-deftest agent-repl-test-apply-workspace-state-stores-merge-status ()
  "An applied frame's `mergeStatus' lands on `:pushed-merge-status'."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    ;; Act
    (agent-repl-test--apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_MERGING"
       :mergeStatus (:runId "r1" :cherryPicking
                     (:commitsTotal 2 :commitsLanded 1))))
    ;; Assert
    (should (eq (plist-get (agent-repl--ws-get "ws1" :pushed-merge-status) :phase)
                :cherry-picking))))

(ert-deftest agent-repl-test-apply-workspace-state-clears-a-gone-merge-status ()
  "A later frame without `mergeStatus' clears the retained one."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_MERGING"
       :mergeStatus (:runId "r1" :cherryPicking (:commitsTotal 2))))
    ;; Act
    (agent-repl-test--apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_READY"))
    ;; Assert
    (should-not (agent-repl--ws-get "ws1" :pushed-merge-status))))

(ert-deftest agent-repl-test-apply-workspace-state-rejects-a-malformed-merge-status ()
  "A malformed `mergeStatus' rejects the whole frame before any mutation."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    ;; Act / Assert
    (should-error (agent-repl-test--apply-workspace-state
                   '(:workspace "ws1" :state "RENDER_STATE_MERGING"
                     :mergeStatus (:runId "r1"))))
    (should-not (agent-repl--ws-get "ws1" :pushed-render-state))))

(ert-deftest agent-repl-test-apply-workspace-state-drops-the-retired-flat-merge-phase ()
  "REWRITTEN: this asserted the pre-cutover flat `mergePhase' landed in the
meta plist.  The field is RETIRED from the wire contract, so what has to hold
now is the opposite -- the meta plist carries no merge phase at all, and the
merge inputs are the pushed render state plus `:pushed-merge-status'."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    ;; Act
    (agent-repl-test--apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_MERGING"
       :mergeStatus (:runId "r1" :cherryPicking (:commitsTotal 2))))
    ;; Assert
    (should-not (plist-get (agent-repl--ws-get "ws1" :pushed-render-state-meta)
                           :merge-phase))
    (should (eq (agent-repl--ws-get "ws1" :pushed-render-state) :merging))
    (should (equal (plist-get (agent-repl--ws-get "ws1" :pushed-merge-status)
                              :run-id)
                   "r1"))))

;;;; ---- StateSnapshot resync --------------------------------------------

(ert-deftest agent-repl-test-apply-snapshot-applies-every-workspace ()
  "A StateSnapshot applies the pushed state for every WorkspaceState in it."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    ;; Act
    (agent-repl-test--apply-snapshot
     '(:workspaces ((:workspace "a" :state "RENDER_STATE_THINKING")
                    (:workspace "b" :state "RENDER_STATE_MERGED"))))
    ;; Assert
    (should (eq (agent-repl--ws-get "a" :pushed-render-state) :thinking))
    (should (eq (agent-repl--ws-get "b" :pushed-render-state) :merged))))

(ert-deftest agent-repl-test-apply-snapshot-returns-count ()
  "A StateSnapshot returns the count of workspace states applied."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    ;; Act / Assert
    (should (= (agent-repl-test--apply-snapshot
                '(:workspaces ((:workspace "a" :state "RENDER_STATE_IDLE")
                               (:workspace "b" :state "RENDER_STATE_IDLE"))))
               2))))

(ert-deftest agent-repl-test-apply-snapshot-empty-is-zero ()
  "An empty StateSnapshot applies nothing and returns 0."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    ;; Act / Assert
    (should (= (agent-repl-test--apply-snapshot '(:workspaces nil)) 0))))

(ert-deftest agent-repl-test-apply-snapshot-runs-the-snapshot-applied-hook ()
  "Applying a snapshot fires the reconnect edge every recovery hangs off."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let* ((ran 0)
           (agent-repl-uds-snapshot-applied-functions
            (list (lambda () (cl-incf ran)))))
      ;; Act
      (agent-repl-test--apply-snapshot '(:workspaces nil))
      ;; Assert
      (should (= ran 1)))))

(ert-deftest agent-repl-test-apply-snapshot-runs-the-hook-after-a-failed-item ()
  "A partial resync is still a live link, so the recovery edge still fires.
Withholding it would leave the outage notices standing over a daemon that
is demonstrably answering."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let* ((ran 0)
           (agent-repl-uds-snapshot-applied-functions
            (list (lambda () (cl-incf ran)))))
      (cl-letf (((symbol-function 'agent-repl--frontend-apply-session-view)
                 (lambda (_v) (error "undecodable session view"))))
        ;; Act
        (agent-repl-test--apply-snapshot
         '(:workspaces nil :sessions ((:sessionId "s_1" :workspace "/w")))))
      ;; Assert
      (should (= ran 1)))))

(ert-deftest agent-repl-test-apply-snapshot-applies-sessions-and-tolerates-catalogs ()
  "A snapshot applies workspaces AND rebuilds the SessionView store from
`:sessions'; the `:catalogs' array (no handler here) does not break it."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (clrhash agent-repl--frontend-session-views)
    ;; Act — sessions carry ids now; catalogs present but unhandled here.
    (agent-repl-test--apply-snapshot
     '(:workspaces ((:workspace "a" :state "RENDER_STATE_IDLE"))
       :sessions ((:sessionId "s_a" :workspace "a" :model "haiku"))
       :catalogs ((:workspace "a" :tasks nil))))
    ;; Assert — the workspace state applied AND the session view is stored.
    (should (eq (agent-repl--ws-get "a" :pushed-render-state) :idle))
    (should (equal (plist-get (agent-repl--frontend-session-view "a") :model) "haiku"))))

(ert-deftest agent-repl-test-apply-snapshot-rebuilds-session-store ()
  "A snapshot REBUILDS the roster wholesale, dropping stale entries a bounced
daemon no longer knows."
  ;; Arrange — a stale view lingers before the snapshot.
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (clrhash agent-repl--frontend-session-views)
    (agent-repl--frontend-store-session-view '(:sessionId "s_stale" :workspace "old"))
    ;; Act
    (agent-repl-test--apply-snapshot
     '(:workspaces nil :sessions ((:sessionId "s_new" :workspace "a"))))
    ;; Assert — the stale entry is gone, only the snapshot's roster remains.
    (should-not (agent-repl--frontend-session-view "old"))
    (should (agent-repl--frontend-session-view "a"))))

(ert-deftest agent-repl-test-snapshot-replaces-workspace-state-safety-store ()
  "A reconnect snapshot drops raw states the daemon no longer reports."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--frontend-workspace-state-views
           (make-hash-table :test 'equal)))
      (puthash "/stale" '(:workspace "/stale")
               agent-repl--frontend-workspace-state-views)
      (agent-repl-test--apply-snapshot
       '(:workspaces ((:workspace "/current"
                       :sessionId "s_1"
                       :state "RENDER_STATE_IDLE"))))
      (should-not (gethash "/stale"
                           agent-repl--frontend-workspace-state-views))
      (should (gethash "/current"
                       agent-repl--frontend-workspace-state-views)))))

(ert-deftest agent-repl-test-apply-snapshot-applies-daemon-view ()
  "A snapshot's `:daemon' member routes into the boot-id note (give-up reset)."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (let ((noted nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-note-boot-id)
                 (lambda (boot-id) (setq noted boot-id))))
        ;; Act
        (agent-repl-test--apply-snapshot
         '(:workspaces nil :daemon (:bootId "b_1" :protocolVersion "1")))
        ;; Assert
        (should (equal noted "b_1"))))))

;;;; ---- StateSnapshot resync: per-item failure containment ---------------
;;
;; The host-action executor acknowledges a handler failure to the daemon and
;; then deliberately re-signals it (that contract lives in
;; workspace-create-client.el and is unchanged).  Inside a snapshot that
;; signal used to abort the whole resync, so a single retained action for a
;; dead directory left the DaemonView unapplied and every later readiness
;; read failed forever.

(ert-deftest agent-repl-test-apply-snapshot-failing-host-action-still-applies-daemon-view ()
  "A host action whose handler signals does not cost the snapshot its DaemonView."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl--frontend-last-daemon-view nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-note-boot-id) #'ignore)
                ((symbol-function 'agent-repl--workspace-create-handle-host-action)
                 (lambda (_item) (error "no live workspace owns that dir")))
                ((symbol-function 'message) #'ignore))
        ;; Act
        (agent-repl-test--apply-snapshot
         '(:workspaces nil
           :hostActions ((:actionId "act-1"))
           :daemon (:bootId "b_1" :protocolVersion "1")))
        ;; Assert
        (should (equal (plist-get agent-repl--frontend-last-daemon-view :bootId)
                       "b_1"))))))

(ert-deftest agent-repl-test-apply-snapshot-failing-host-action-still-applies-workspaces ()
  "A failing host action leaves the workspace states applied and the count returned."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "a")
    (cl-letf (((symbol-function 'agent-repl--workspace-create-handle-host-action)
               (lambda (_item) (error "no live workspace owns that dir")))
              ((symbol-function 'message) #'ignore))
      ;; Act
      (let ((count (agent-repl-test--apply-snapshot
                    '(:workspaces ((:workspace "a" :state "RENDER_STATE_IDLE"))
                      :hostActions ((:actionId "act-1"))))))
        ;; Assert
        (should (= count 1))
        (should (eq (agent-repl--ws-get "a" :pushed-render-state) :idle))))))

(ert-deftest agent-repl-test-apply-snapshot-item-failure-is-logged ()
  "A contained item failure names the item and its error in the log."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let (logged)
      (cl-letf (((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged)))
                ((symbol-function 'agent-repl--workspace-create-handle-host-action)
                 (lambda (_item) (error "no live workspace owns that dir")))
                ((symbol-function 'message) #'ignore))
        ;; Act
        (agent-repl-test--apply-snapshot
         '(:workspaces nil :hostActions ((:actionId "act-1")))))
      ;; Assert
      (should (cl-find-if
               (lambda (line)
                 (and (string-match-p "host-action item FAILED" line)
                      (string-match-p "act-1" line)
                      (string-match-p "no live workspace owns that dir" line)))
               logged)))))

(ert-deftest agent-repl-test-apply-snapshot-item-failure-is-messaged ()
  "A contained item failure is also surfaced to the user, never only logged."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let (echoed)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) echoed)))
                ((symbol-function 'agent-repl--workspace-create-handle-host-action)
                 (lambda (_item) (error "no live workspace owns that dir"))))
        ;; Act
        (agent-repl-test--apply-snapshot
         '(:workspaces nil :hostActions ((:actionId "act-1")))))
      ;; Assert
      (should (cl-find-if
               (lambda (line) (string-match-p "1 item(s) FAILED during snapshot resync" line))
               echoed)))))

(ert-deftest agent-repl-test-apply-snapshot-failing-workspace-state-still-applies-the-rest ()
  "One rejected WorkspaceState does not stop the remaining ones from applying."
  ;; Arrange — the first state omits the mandatory connectivity verdict, so
  ;; its application signals; the second is complete.
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (cl-letf (((symbol-function 'message) #'ignore))
      ;; Act
      (agent-repl--frontend-apply-snapshot
       (list :workspaces
             (list '(:workspace "a" :state "RENDER_STATE_IDLE" :sessionId "s_a")
                   (agent-repl-test--complete-workspace-state
                    '(:workspace "b" :state "RENDER_STATE_THINKING")))))
      ;; Assert
      (should-not (agent-repl--ws-get "a" :pushed-render-state))
      (should (eq (agent-repl--ws-get "b" :pushed-render-state) :thinking)))))

;;;; ---- SessionView store + handler -------------------------------------

(ert-deftest agent-repl-test-apply-session-view-stores-by-workspace ()
  "The sessionView handler upserts the view into the store, keyed by workspace."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (clrhash agent-repl--frontend-session-views)
    ;; Act
    (agent-repl--frontend-apply-session-view
     '(:sessionId "s_1" :workspace "/w" :claudeSessionId "cli-1"))
    ;; Assert
    (should (equal (plist-get (agent-repl--frontend-session-view "/w") :claudeSessionId) "cli-1"))))

(ert-deftest agent-repl-test-store-session-view-missing-workspace-errors ()
  "A SessionView with no workspace fails loudly (No-Silent-Fallbacks).
The workspace is the key the store is indexed by, so a view without one
cannot be filed at all."
  ;; Arrange / Act / Assert
  (should-error (agent-repl--frontend-store-session-view '(:sessionId "s_1"))))

(ert-deftest agent-repl-test-store-session-view-keeps-the-live-session ()
  "A superseded predecessor never displaces the live session it lost to.
The daemon re-pushes every terminal view on each snapshot, in no
guaranteed order, so a predecessor arriving after its successor must not
retire a session that is running."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (clrhash agent-repl--frontend-session-views)
    (agent-repl--frontend-store-session-view '(:sessionId "s_live" :workspace "/w"))
    ;; Act — the retired predecessor lands afterwards.
    (agent-repl--frontend-store-session-view
     '(:sessionId "s_dead" :workspace "/w" :terminal t))
    ;; Assert
    (should (equal (plist-get (agent-repl--frontend-session-view "/w") :sessionId) "s_live"))
    (should (agent-repl--frontend-workspace-session-live-p "/w"))))

(ert-deftest agent-repl-test-store-session-view-records-its-own-death ()
  "A session's own terminal view replaces the live one it reports on.
Only a DIFFERENT session's terminal view is a superseded predecessor."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (clrhash agent-repl--frontend-session-views)
    (agent-repl--frontend-store-session-view '(:sessionId "s_1" :workspace "/w"))
    ;; Act
    (agent-repl--frontend-store-session-view
     '(:sessionId "s_1" :workspace "/w" :terminal t))
    ;; Assert
    (should-not (agent-repl--frontend-workspace-session-live-p "/w"))))

(ert-deftest agent-repl-test-store-session-view-successor-replaces-predecessor ()
  "A live successor replaces whatever the workspace held before it."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (clrhash agent-repl--frontend-session-views)
    (agent-repl--frontend-store-session-view '(:sessionId "s_dead" :workspace "/w" :terminal t))
    ;; Act
    (agent-repl--frontend-store-session-view '(:sessionId "s_live" :workspace "/w"))
    ;; Assert — one entry per workspace, and it is the current session.
    (should (equal (plist-get (agent-repl--frontend-session-view "/w") :sessionId) "s_live"))
    (should (= (hash-table-count agent-repl--frontend-session-views) 1))))

(ert-deftest agent-repl-test-session-view-nil-for-unknown-workspace ()
  "The store reports nothing for a workspace no view has been pushed for."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (clrhash agent-repl--frontend-session-views)
    ;; Act / Assert
    (should (null (agent-repl--frontend-session-view "/nope")))
    (should-not (agent-repl--frontend-workspace-session-live-p "/nope"))))

(ert-deftest agent-repl-test-apply-daemon-view-notes-boot-id ()
  "The daemonView handler routes its bootId into `--frontend-note-boot-id'."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (let ((noted nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-note-boot-id)
                 (lambda (boot-id) (setq noted boot-id))))
        ;; Act
        (agent-repl--frontend-apply-daemon-view '(:bootId "b_9" :protocolVersion "1"))
        ;; Assert
        (should (equal noted "b_9"))))))

(ert-deftest agent-repl-test-apply-daemon-view-stores-the-view ()
  "The daemonView handler stores the view for the readiness/staleness reads."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (let ((agent-repl--frontend-last-daemon-view nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-note-boot-id) #'ignore))
        ;; Act
        (agent-repl--frontend-apply-daemon-view '(:bootId "b_9" :daemonVersion "v2"))
        ;; Assert
        (should (equal (plist-get (agent-repl--frontend-daemon-view) :daemonVersion)
                       "v2"))))))

(ert-deftest agent-repl-test-daemon-view-nil-before-any-frame ()
  "The daemon-view accessor is nil until the first `DaemonView' is pushed."
  ;; Arrange
  (let ((agent-repl--frontend-last-daemon-view nil))
    ;; Act / Assert
    (should (null (agent-repl--frontend-daemon-view)))))

(ert-deftest agent-repl-test-apply-snapshot-stores-the-daemon-view ()
  "A StateSnapshot's `:daemon' arm lands in the daemon-view store."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (let ((agent-repl--frontend-last-daemon-view nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-note-boot-id) #'ignore))
        ;; Act
        (agent-repl-test--apply-snapshot '(:daemon (:bootId "b_1")))
        ;; Assert
        (should (equal (plist-get (agent-repl--frontend-daemon-view) :bootId) "b_1"))))))

;;;; ---- DaemonView binary mtime (staleness source) ----------------------

(ert-deftest agent-repl-test-daemon-view-mtime-parses-protojson-string ()
  "An int64 mtime arrives as a protojson STRING and converts to seconds."
  ;; Arrange
  (let ((agent-repl--frontend-last-daemon-view '(:daemonBinaryMtimeMs "1700000000000")))
    ;; Act / Assert
    (should (equal 1700000000
                   (agent-repl--frontend-daemon-view-binary-mtime-seconds)))))

(ert-deftest agent-repl-test-daemon-view-mtime-accepts-a-number ()
  "A numerically-decoded mtime converts to seconds just the same."
  ;; Arrange
  (let ((agent-repl--frontend-last-daemon-view '(:daemonBinaryMtimeMs 1700000000000)))
    ;; Act / Assert
    (should (equal 1700000000
                   (agent-repl--frontend-daemon-view-binary-mtime-seconds)))))

(ert-deftest agent-repl-test-daemon-view-mtime-nil-without-a-view ()
  "No pushed view yields nil, never a guessed mtime."
  ;; Arrange
  (let ((agent-repl--frontend-last-daemon-view nil))
    ;; Act / Assert
    (should (null (agent-repl--frontend-daemon-view-binary-mtime-seconds)))))

(ert-deftest agent-repl-test-daemon-view-mtime-nil-when-field-absent ()
  "A view predating the mtime field yields nil."
  ;; Arrange
  (let ((agent-repl--frontend-last-daemon-view '(:bootId "b_1")))
    ;; Act / Assert
    (should (null (agent-repl--frontend-daemon-view-binary-mtime-seconds)))))

(ert-deftest agent-repl-test-daemon-view-mtime-nil-when-nonpositive ()
  "A zero mtime (the daemon's boot-time self-stat failed) yields nil."
  ;; Arrange
  (let ((agent-repl--frontend-last-daemon-view '(:daemonBinaryMtimeMs "0")))
    ;; Act / Assert
    (should (null (agent-repl--frontend-daemon-view-binary-mtime-seconds)))))

(ert-deftest agent-repl-test-daemon-view-mtime-nil-when-unparsable ()
  "A non-numeric mtime yields nil rather than a coerced zero."
  ;; Arrange
  (let ((agent-repl--frontend-last-daemon-view '(:daemonBinaryMtimeMs "garbage")))
    ;; Act / Assert
    (should (null (agent-repl--frontend-daemon-view-binary-mtime-seconds)))))

;;;; ---- SessionInit store + handler (slash-menu source) -----------------

(ert-deftest agent-repl-test-apply-session-init-stores-by-workspace ()
  "The sessionInit handler stores its SystemInit keyed by workspace."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (clrhash agent-repl--frontend-session-inits)
    ;; Act
    (agent-repl--frontend-apply-session-init
     '(:fence "f_1" :workspace "/w" :init (:slashCommands ("commit" "review"))))
    ;; Assert
    (should (equal (plist-get (agent-repl--frontend-session-init "/w") :slashCommands)
                   '("commit" "review")))))

(ert-deftest agent-repl-test-store-session-init-missing-workspace-errors ()
  "A SessionInitView with no workspace fails loudly (No-Silent-Fallbacks)."
  ;; Arrange / Act / Assert
  (should-error (agent-repl--frontend-store-session-init
                 '(:fence "f_1" :init (:slashCommands ())))))

(ert-deftest agent-repl-test-session-init-nil-for-unknown ()
  "The session-init accessor returns nil for a session with no pushed init."
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (clrhash agent-repl--frontend-session-inits)
    (should (null (agent-repl--frontend-session-init "/nowhere")))))

(ert-deftest agent-repl-test-apply-snapshot-rebuilds-session-inits ()
  "A StateSnapshot rebuilds the session-init roster from its :inits list."
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (clrhash agent-repl--frontend-session-inits)
    ;; Arrange — a stale init that the snapshot must drop.
    (agent-repl--frontend-store-session-init
     '(:workspace "/stale" :init (:slashCommands ("old"))))
    ;; Act
    (agent-repl-test--apply-snapshot
     '(:inits ((:workspace "/new" :init (:slashCommands ("new"))))))
    ;; Assert — stale gone, new present.
    (should-not (agent-repl--frontend-session-init "/stale"))
    (should (equal (plist-get (agent-repl--frontend-session-init "/new") :slashCommands)
                   '("new")))))

(ert-deftest agent-repl-test-state-registers-session-init-handler ()
  "The sessionInit oneof arm is wired to its handler."
  (should (eq (cdr (assoc "sessionInit" agent-repl--uds-frame-handlers))
              #'agent-repl--frontend-apply-session-init)))

(ert-deftest agent-repl-test-state-registers-session-view-handler ()
  "The sessionView oneof arm is wired to its handler."
  (should (eq (cdr (assoc "sessionView" agent-repl--uds-frame-handlers))
              #'agent-repl--frontend-apply-session-view)))

(ert-deftest agent-repl-test-state-registers-daemon-view-handler ()
  "The daemonView oneof arm is wired to its handler."
  (should (eq (cdr (assoc "daemonView" agent-repl--uds-frame-handlers))
              #'agent-repl--frontend-apply-daemon-view)))

;;;; ---- SessionView.death: the reader it never had (F4) ----------------
;;
;; These replace the five DegradedNotice tests that stood here. The banner
;; is retired: degradation is a self-resolving failure card on the
;; conversation plane plus a move on the SSM's legacy impairment projection, so there is no
;; echo-area handler left to test.

(ert-deftest agent-repl-test-session-death-surfaces ()
  "A terminal SessionView's classified death reaches the echo area."
  ;; Arrange
  (agent-repl-test--with-clean-state
   (clrhash agent-repl--frontend-surfaced-deaths)
   (let (echoed)
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
       ;; Act
       (agent-repl--frontend-apply-session-view
        '(:sessionId "s1" :workspace "/w" :terminal t
          :death (:kind (:sessionShimDied ())
                  :message "the agent process exited"
                  :terminal ())))
       ;; Assert
       (should (string-match-p "the agent process exited" echoed))))))

(ert-deftest agent-repl-test-session-death-surfaces-only-once ()
  "A re-pushed terminal SessionView does not re-announce the death."
  ;; Arrange — a terminal view is re-pushed on every snapshot, so without the
  ;; latch one honest report becomes recurring noise.
  (agent-repl-test--with-clean-state
   (clrhash agent-repl--frontend-surfaced-deaths)
   (let ((count 0)
         (view '(:sessionId "s1" :workspace "/w" :terminal t
                 :death (:kind (:sessionShimDied ())
                         :message "the agent process exited"
                         :terminal ()))))
     ;; Only echo-area announcements count.  The quiet log rung the failure
     ;; record rides (`agent-repl--warn') also goes through `message', with
     ;; `inhibit-message' bound — that is the durable record, not an
     ;; announcement, so it must not read as re-announcing the death.
     (cl-letf (((symbol-function 'message)
                (lambda (&rest _) (unless inhibit-message (setq count (1+ count))))))
       ;; Act
       (agent-repl--frontend-apply-session-view view)
       (agent-repl--frontend-apply-session-view view)
       ;; Assert
       (should (equal count 1))))))

(ert-deftest agent-repl-test-live-session-surfaces-no-death ()
  "A living session announces nothing."
  ;; Arrange
  (agent-repl-test--with-clean-state
   (clrhash agent-repl--frontend-surfaced-deaths)
   (let (echoed)
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
       ;; Act
       (agent-repl--frontend-apply-session-view '(:sessionId "s1" :workspace "/w"))
       ;; Assert
       (should (null echoed))))))

(ert-deftest agent-repl-test-session-death-echoes-the-daemon-prose ()
  "The death's own sentence is what the user reads."
  ;; Arrange
  (agent-repl-test--with-clean-state
   (clrhash agent-repl--frontend-surfaced-deaths)
   (let (echoed)
     (cl-letf (((symbol-function 'agent-repl--warn) #'ignore)
               ((symbol-function 'agent-repl--emit-message)
                (lambda (text &optional _echo) (setq echoed text))))
       ;; Act
       (agent-repl--frontend-apply-session-view
        '(:sessionId "s1" :workspace "/w" :terminal t
          :death (:kind (:sessionEndedUnclassified ())
                  :message "the session ended"
                  :detail "some ancient reason"
                  :terminal ())))
       ;; Assert
       (should (equal echoed "agent-repl: the session ended"))))))

(ert-deftest agent-repl-test-session-death-keeps-the-raw-reason-off-the-echo ()
  "The death's source detail is evidence, and evidence does not reach the echo."
  ;; Arrange
  (agent-repl-test--with-clean-state
   (clrhash agent-repl--frontend-surfaced-deaths)
   (let (echoed)
     (cl-letf (((symbol-function 'agent-repl--warn) #'ignore)
               ((symbol-function 'agent-repl--emit-message)
                (lambda (text &optional _echo) (setq echoed text))))
       ;; Act
       (agent-repl--frontend-apply-session-view
        '(:sessionId "s1" :workspace "/w" :terminal t
          :death (:kind (:sessionEndedUnclassified ())
                  :message "the session ended"
                  :detail "some ancient reason"
                  :terminal ())))
       ;; Assert
       (should-not (string-match-p "some ancient reason" echoed))))))

(ert-deftest agent-repl-test-session-death-files-the-raw-reason ()
  "The evidence the echo dropped still reaches the canonical log."
  ;; Arrange
  (agent-repl-test--with-clean-state
   (clrhash agent-repl--frontend-surfaced-deaths)
   (let (logged)
     (cl-letf (((symbol-function 'agent-repl--warn) #'ignore)
               ((symbol-function 'agent-repl--emit-message) #'ignore)
               ((symbol-function 'agent-repl--log)
                (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
       ;; Act
       (agent-repl--frontend-apply-session-view
        '(:sessionId "s1" :workspace "/w" :terminal t
          :death (:kind (:sessionEndedUnclassified ())
                  :message "the session ended"
                  :detail "some ancient reason"
                  :terminal ())))
       ;; Assert
       (should (cl-find-if (lambda (line) (string-match-p "some ancient reason" line))
                           logged))))))

(ert-deftest agent-repl-test-open-supersede-death-surfaces ()
  "A supersede whose successor is not yet up still announces itself."
  ;; Arrange — the daemon resolves a supersede only once the replacement
  ;; session reaches operational, so an unresolved one is a handover the user
  ;; just triggered and must still be reported.
  (agent-repl-test--with-clean-state
   (clrhash agent-repl--frontend-surfaced-deaths)
   (let (echoed)
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
       ;; Act
       (agent-repl--frontend-apply-session-view
        '(:sessionId "s1" :workspace "/w" :terminal t
          :death (:kind (:sessionSuperseded ())
                  :message "a new Claude session was started for this workspace"
                  :open ())))
       ;; Assert
       (should (string-match-p "a new Claude session was started" echoed))))))

(ert-deftest agent-repl-test-resolved-supersede-death-is-silent ()
  "A supersede the daemon has resolved re-presents nothing on restore."
  ;; Arrange — this is the boot burst the resolution exists to end: historical
  ;; supersedes replayed in every snapshot, each one an open blue card about a
  ;; handover completed days earlier.
  (agent-repl-test--with-clean-state
   (clrhash agent-repl--frontend-surfaced-deaths)
   (let (echoed)
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
       ;; Act
       (agent-repl--frontend-apply-session-view
        '(:sessionId "s1" :workspace "/w" :terminal t
          :death (:kind (:sessionSuperseded ())
                  :message "a new Claude session was started for this workspace"
                  :resolved (:resolvedAtMs 1700000000000))))
       ;; Assert
       (should (null echoed))))))

(ert-deftest agent-repl-test-resolved-supersede-death-is-silent-with-string-instant ()
  "A protojson STRING resolution instant applies and stays silent.
protojson encodes int64 as a JSON string, so the daemon's own frames
carry `resolvedAtMs' as \"1786127506030\" — the shape that made every
session-view item fail to apply at boot."
  ;; Arrange
  (agent-repl-test--with-clean-state
   (clrhash agent-repl--frontend-surfaced-deaths)
   (let (echoed)
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
       ;; Act
       (agent-repl--frontend-apply-session-view
        '(:sessionId "s1" :workspace "/w" :terminal t
          :death (:kind (:sessionSuperseded ())
                  :message "a new Claude session was started for this workspace"
                  :resolved (:resolvedAtMs "1786127506030"))))
       ;; Assert
       (should (null echoed))))))

(ert-deftest agent-repl-test-open-delete-death-surfaces ()
  "A deletion the daemon left open still announces itself."
  ;; Arrange — a record written before the delete resolved its own death.
  (agent-repl-test--with-clean-state
   (clrhash agent-repl--frontend-surfaced-deaths)
   (let (echoed)
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
       ;; Act
       (agent-repl--frontend-apply-session-view
        '(:sessionId "s1" :workspace "/w" :terminal t
          :death (:kind (:sessionDeleted ())
                  :message "the session was deleted"
                  :open ())))
       ;; Assert
       (should (string-match-p "the session was deleted" echoed))))))

(ert-deftest agent-repl-test-resolved-delete-death-is-silent ()
  "A deletion the daemon resolved re-presents nothing.
This is the loop the resolution exists to end: a workspace whose session
was deleted was handed the same open card on every snapshot, forever,
while its successor session was serving turns."
  ;; Arrange
  (agent-repl-test--with-clean-state
   (clrhash agent-repl--frontend-surfaced-deaths)
   (let (echoed)
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
       ;; Act
       (agent-repl--frontend-apply-session-view
        '(:sessionId "s1" :workspace "/w" :terminal t
          :death (:kind (:sessionDeleted ())
                  :message "the session was deleted"
                  :resolved (:resolvedAtMs 1700000000000))))
       ;; Assert
       (should (null echoed))))))

(ert-deftest agent-repl-test-two-deaths-on-one-workspace-each-surface-once ()
  "Two terminal records on one cwd do not evict each other's latch.
A workspace has as many terminal records as it has had sessions and the
daemon pushes a SessionView for every one, so a single workspace-wide
slot held whichever death arrived last and re-announced BOTH on every
snapshot."
  ;; Arrange
  (agent-repl-test--with-clean-state
   (clrhash agent-repl--frontend-surfaced-deaths)
   (let ((count 0)
         (deleted '(:sessionId "s1" :workspace "/w" :terminal t
                    :death (:kind (:sessionDeleted ())
                            :message "the session was deleted"
                            :open ())))
         (died '(:sessionId "s2" :workspace "/w" :terminal t
                 :death (:kind (:sessionShimDied ())
                         :message "the agent process exited"
                         :open ()))))
     (cl-letf (((symbol-function 'message)
                (lambda (&rest _) (unless inhibit-message (setq count (1+ count))))))
       ;; Act — two snapshots of the same roster.
       (agent-repl--frontend-apply-session-view deleted)
       (agent-repl--frontend-apply-session-view died)
       (agent-repl--frontend-apply-session-view deleted)
       (agent-repl--frontend-apply-session-view died)
       ;; Assert — one announcement per death, not one per snapshot.
       (should (equal count 2))))))

(ert-deftest agent-repl-test-session-death-without-a-session-id-signals ()
  "A death with no session id has no identity to latch and is refused."
  ;; Arrange — the latch is per session, so an anonymous death would
  ;; re-announce on every snapshot: the very defect the latch exists for.
  (agent-repl-test--with-clean-state
   (agent-repl-test--with-log-sink-on
    (clrhash agent-repl--frontend-surfaced-deaths)
    ;; Act / Assert
    (should-error
     (agent-repl--frontend-surface-session-death
      "/w" '(:workspace "/w" :terminal t
             :death (:kind (:sessionDeleted ())
                     :message "the session was deleted"
                     :open ())))))))

;;;; ---- Handler registration wiring -------------------------------------

(ert-deftest agent-repl-test-state-load-does-not-dial-before-lazy-daemon ()
  "Loading state handlers does not race the lazy daemon's startup owner."
  ;; Arrange
  (let ((dials 0))
    (cl-letf (((symbol-function 'agent-repl-uds-connect)
               (lambda (&rest _) (cl-incf dials))))
      ;; Act
      (load agent-repl-test--frontend-state-file nil t))
    ;; Assert
    (should (= dials 0))))

(ert-deftest agent-repl-test-state-registers-workspace-state-handler ()
  "Loading frontend-state.el registers the workspaceState handler."
  (should (eq (cdr (assoc "workspaceState" agent-repl--uds-frame-handlers))
              #'agent-repl--frontend-apply-workspace-state)))

(ert-deftest agent-repl-test-state-registers-snapshot-handler ()
  "Loading frontend-state.el registers the snapshot handler."
  (should (eq (cdr (assoc "snapshot" agent-repl--uds-frame-handlers))
              #'agent-repl--frontend-apply-snapshot)))

(ert-deftest agent-repl-test-state-degraded-notice-arm-stays-retired ()
  "The degradedNotice arm is RETIRED (step 11): no handler, and no longer a
known or ignored frame field at all (the wire arm itself is gone —
reserved 8/\"degraded_notice\" in frontend.proto), so a push from a daemon
old enough to still send it is now the loud unknown-field signal rather
than a settled no-op."
  (should-not (assoc "degradedNotice" agent-repl--uds-frame-handlers))
  (should-not (member "degradedNotice" agent-repl--uds-known-frame-fields))
  (should-not (member "degradedNotice" agent-repl--uds-ignored-frame-fields)))

(ert-deftest agent-repl-test-state-dispatch-end-to-end ()
  "A decoded workspaceState frame dispatched through the transport applies state."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    ;; Act — decode + dispatch via the real registered handler
    (agent-repl--uds-dispatch-frame
     (agent-repl--uds-decode-frame
      "{\"workspaceState\":{\"workspace\":\"ws1\",\"sessionId\":\"s1\",\"state\":\"RENDER_STATE_PERMISSION\",\"connectivity\":\"SESSION_CONNECTIVITY_OPERATIONAL\",\"status\":\"SESSION_STATUS_PERMISSION\",\"controllerGenerationId\":\"g1\"}}"))
    ;; Assert
    (should (eq (agent-repl--ws-get "ws1" :pushed-render-state) :permission))))

;;;; ---- State-transition hook -------------------------------------------

(ert-deftest agent-repl-test-state-transition-hook-fires-on-push ()
  "Applying a WorkspaceState runs the state-transition hook with (ws new prev)."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    ;; `let*': the hook-list init form closes over `captured', which must be
    ;; bound FIRST (a plain `let' evaluates all inits in the outer scope, so
    ;; the closure would capture an empty lexical env and never see `captured').
    (let* (captured
           (agent-repl-ws-state-transition-functions
            (list (lambda (ws new prev) (setq captured (list ws new prev))))))
      ;; Act
      (agent-repl-test--apply-workspace-state
       '(:workspace "ws1" :state "RENDER_STATE_MERGING"))
      ;; Assert
      (should (equal captured '("ws1" :merging nil))))))

(ert-deftest agent-repl-test-state-transition-hook-previous-keyword ()
  "The hook receives the prior pushed keyword as PREVIOUS on a later push."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (let* (captured
           (agent-repl-ws-state-transition-functions
            (list (lambda (ws new prev) (setq captured (list ws new prev))))))
      (agent-repl-test--apply-workspace-state
       '(:workspace "ws1" :state "RENDER_STATE_MERGING"))
      ;; Act — second push transitions merging -> merge-conflict
      (agent-repl-test--apply-workspace-state
       '(:workspace "ws1" :state "RENDER_STATE_MERGE_CONFLICT"))
      ;; Assert
      (should (equal captured '("ws1" :merge-conflict :merging))))))

(ert-deftest agent-repl-test-state-transition-hook-subscriber-error-isolated ()
  "A signaling subscriber is caught + logged; state application still succeeds."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (let ((agent-repl-ws-state-transition-functions
           (list (lambda (&rest _) (error "boom")))))
      ;; Act
      (agent-repl-test--apply-workspace-state
       '(:workspace "ws1" :state "RENDER_STATE_MERGED"))
      ;; Assert — the pushed state landed despite the broken subscriber
      (should (eq (agent-repl--ws-get "ws1" :pushed-render-state) :merged)))))

;;;; ---- Session-ready latch ---------------------------------------------

(ert-deftest agent-repl-test-latch-fires-agent-ready-on-first-push ()
  "The FIRST pushed WorkspaceState arms the :agent-ready latch bit."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (let (calls)
      (cl-letf (((symbol-function 'agent-repl--latch-and-maybe-fire-loaded)
                 (lambda (ws key &optional _m) (push (list ws key) calls))))
        ;; Act
        (agent-repl-test--apply-workspace-state
         '(:workspace "ws1" :state "RENDER_STATE_INIT"))
        ;; Assert
        (should (equal calls '(("ws1" :agent-ready))))))))

(ert-deftest agent-repl-test-latch-fires-only-once-per-workspace ()
  "A second pushed state for the same workspace does not re-arm the latch."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (let (calls)
      (cl-letf (((symbol-function 'agent-repl--latch-and-maybe-fire-loaded)
                 (lambda (ws key &optional _m) (push (list ws key) calls))))
        (agent-repl-test--apply-workspace-state
         '(:workspace "ws1" :state "RENDER_STATE_INIT"))
        ;; Act — second push
        (agent-repl-test--apply-workspace-state
         '(:workspace "ws1" :state "RENDER_STATE_IDLE"))
        ;; Assert — latch fired exactly once
        (should (= (length calls) 1))))))

;;;; ---- Tests: wire CWDs never reach the workspace log sink ----

;; `agent-repl-test--with-log-sink-on' lives in test-helpers.el: every suite
;; that asserts a path cannot violate the log routing invariant needs it,
;; because the batch harness disables the sink and the ladder then skips
;; identity resolution altogether.

(ert-deftest agent-repl-test-apply-session-view-with-unowned-cwd-does-not-signal ()
  "A SessionView naming a CWD no live workspace owns must not abort the filter.
Frame handlers run inside the connection's process filter, and a wire CWD can
never index the workspace hash, so routing one into the logging ladder made
the daemon's own snapshot replay kill the filter.  The sink must be ENABLED
for this to mean anything: with it off the ladder never resolves an identity
at all, which is why the existing frame tests could not see this."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-log-sink-on
      (clrhash agent-repl--frontend-session-views)
      ;; Act / Assert
      (should (agent-repl--frontend-apply-session-view
               '(:sessionId "s_unowned" :workspace "/nowhere/unowned"))))))

(ert-deftest agent-repl-test-surface-session-death-with-unowned-cwd-does-not-signal ()
  "The no-death branch runs for every replayed frame and must stay silent.
It is the chattiest path in the handler, so it is the one a wire CWD reaches
first."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-log-sink-on
      ;; Act / Assert
      (should-not (agent-repl--frontend-surface-session-death
                   "s_unowned" '(:workspace "/nowhere/unowned"))))))

(ert-deftest agent-repl-test-apply-session-init-with-unowned-cwd-does-not-signal ()
  "The sessionInit handler carries the same wire CWD field as sessionView."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-log-sink-on
      (clrhash agent-repl--frontend-session-inits)
      ;; Act / Assert
      (should (agent-repl--frontend-apply-session-init
               '(:fence "f_init_unowned" :workspace "/nowhere/unowned"
                 :init (:slashCommands nil :skills nil)))))))

(provide 'test-frontend-state)

;;; test-frontend-state.el ends here

;;;; ---- The inbound workspace key ---------------------------------------
;;
;; Every daemon frame names its workspace by the session CWD; Emacs keys
;; workspaces by persp NAME.  Feeding the path straight to `agent-repl--ws-put'
;; does not fail — it STUB-CREATES an entry under the path, so the pushed state
;; lands somewhere the tab-bar never reads and every workspace sits at its
;; disconnected colour while the session works normally.

(ert-deftest agent-repl-test-inbound-frame-resolves-cwd-to-workspace-name ()
  "A frame naming the session CWD applies to the workspace that owns it."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "doom" :project-dir "/Users/x/.config/doom")
    ;; Act — the daemon always names the workspace by cwd.
    (agent-repl-test--apply-workspace-state
     '(:workspace "/Users/x/.config/doom" :state "RENDER_STATE_THINKING"))
    ;; Assert — the state reached the NAME the renderer reads.
    (should (eq (agent-repl--ws-get "doom" :pushed-render-state) :thinking))))

(ert-deftest agent-repl-test-inbound-frame-does-not-stub-create-under-the-path ()
  "Resolving must not leave a path-keyed stub behind."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "doom" :project-dir "/Users/x/.config/doom")
    ;; Act
    (agent-repl-test--apply-workspace-state
     '(:workspace "/Users/x/.config/doom" :state "RENDER_STATE_DONE"))
    ;; Assert — the path is not a workspace.
    (should-not (agent-repl--ws-get "/Users/x/.config/doom" :pushed-render-state))))

(ert-deftest agent-repl-test-tombstoned-path-stays-out-of-the-live-resolver ()
  "A tombstoned path is invisible to shared live-only resolver callers."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "closed" :project-dir "/Users/x/closed")
    (agent-repl--ws-put "closed" :nuked-at (current-time))
    (should-not (agent-repl--frontend-ws-name "/Users/x/closed"))))

(ert-deftest agent-repl-test-tombstoned-workspace-state-retains-without-runtime-mutation ()
  "A tombstoned WorkspaceState is retained without writing runtime keys."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--frontend-workspace-state-views (make-hash-table :test 'equal)))
      (agent-repl--ws-put "closed" :project-dir "/Users/x/closed")
      (agent-repl--ws-put "closed" :nuked-at (current-time))
      (should-not
       (agent-repl-test--apply-workspace-state
        '(:workspace "/Users/x/closed" :state "RENDER_STATE_HIBERNATED")))
      (should (gethash "/Users/x/closed" agent-repl--frontend-workspace-state-views))
      (should-not (agent-repl--ws-get "closed" :pushed-render-state)))))

(ert-deftest agent-repl-test-live-frame-for-an-unowned-cwd-rejects ()
  "A live frame for an unowned cwd signals and never creates a workspace."
  ;; Arrange
  (agent-repl-test--with-clean-state
    ;; Act — Emacs has nothing open for this path.
    (should-error
     (agent-repl-test--apply-workspace-state
      '(:workspace "/Users/x/not-open" :state "RENDER_STATE_THINKING")))
    ;; Assert — no entry invented to hold it.
    (should-not (agent-repl--ws-known-p "/Users/x/not-open"))))

(ert-deftest agent-repl-test-merge-failed-for-a-closed-workspace-resurrects-it ()
  "A merge_failed push for a torn-down workspace re-establishes its tab."
  ;; Arrange — an on-disk worktree Emacs has no live workspace for, and an
  ;; establish stub that registers the dir the way the real one does.
  (agent-repl-test--with-clean-state
    (let ((dir (make-temp-file "agent-repl-resurrect" t))
          established reordered)
      (unwind-protect
          (let ((ws (file-name-nondirectory (directory-file-name dir))))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                       (lambda (w d)
                         (setq established (list w d))
                         (agent-repl--ws-put w :project-dir d)))
                      ((symbol-function 'agent-repl--reorder-workspace-to-front)
                       (lambda (w) (setq reordered w))))
              ;; Act
              (agent-repl-test--apply-workspace-state
               (list :workspace dir :state "RENDER_STATE_MERGE_FAILED"))
              ;; Assert — resurrected, fronted, and the frame re-applied.
              (should (equal established (list ws dir)))
              (should (equal reordered ws))
              (should (eq (agent-repl--ws-get ws :pushed-render-state)
                          :merge-failed))))
        (delete-directory dir t)))))

(ert-deftest agent-repl-test-merge-failed-resurrection-flags-merge-failed ()
  "The resurrected workspace carries the :merge-failed badge flag."
  (agent-repl-test--with-clean-state
    (let ((dir (make-temp-file "agent-repl-resurrect" t)))
      (unwind-protect
          (let ((ws (file-name-nondirectory (directory-file-name dir))))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                       (lambda (w d) (agent-repl--ws-put w :project-dir d)))
                      ((symbol-function 'agent-repl--reorder-workspace-to-front)
                       (lambda (_w) nil)))
              ;; Act
              (agent-repl-test--apply-workspace-state
               (list :workspace dir :state "RENDER_STATE_MERGE_FAILED"))
              ;; Assert
              (should (eq (agent-repl--ws-get ws :merge-failed) t))))
        (delete-directory dir t)))))

(ert-deftest agent-repl-test-merge-failed-resurrection-skips-a-missing-worktree ()
  "A merge_failed for a worktree gone from disk stays retained-only."
  (agent-repl-test--with-clean-state
    (let (established)
      (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                 (lambda (&rest args) (setq established args))))
        ;; Act
        (agent-repl-test--apply-workspace-state
         '(:workspace "/nonexistent/agent-repl-gone"
           :state "RENDER_STATE_MERGE_FAILED"))
        ;; Assert — nothing established, nothing stub-created.
        (should-not established)
        (should-not (agent-repl--ws-known-p "agent-repl-gone"))))))

(ert-deftest agent-repl-test-merge-failed-resurrection-bounds-the-recursion ()
  "An establish that fails to register the dir does not loop the re-apply."
  (agent-repl-test--with-clean-state
    (let ((dir (make-temp-file "agent-repl-resurrect" t))
          (establish-calls 0))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                     (lambda (&rest _) (cl-incf establish-calls)))
                    ((symbol-function 'agent-repl--reorder-workspace-to-front)
                     (lambda (_w) nil)))
            ;; Act — the stub registers nothing, so ownership never appears.
            (agent-repl-test--apply-workspace-state
             (list :workspace dir :state "RENDER_STATE_MERGE_FAILED"))
            ;; Assert — exactly one establish attempt, no infinite re-apply.
            (should (= establish-calls 1)))
        (delete-directory dir t)))))

(ert-deftest agent-repl-test-merge-failed-for-a-tab-less-owned-workspace-resurrects-it ()
  "A merge_failed for a data-only (owned, no tab) workspace re-opens its tab."
  ;; Arrange — a registered workspace whose persp tab is gone (the entry a
  ;; completed merge leaves behind), with the tab predicate answering closed.
  (agent-repl-test--with-clean-state
    (let ((dir (make-temp-file "agent-repl-dataonly" t))
          established reordered)
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir dir)
            (cl-letf (((symbol-function 'agent-repl--ws-open-p)
                       (lambda (_w) nil))
                      ((symbol-function 'agent-repl--establish-workspace)
                       (lambda (w d) (setq established (list w d))))
                      ((symbol-function 'agent-repl--reorder-workspace-to-front)
                       (lambda (w) (setq reordered w))))
              ;; Act
              (agent-repl-test--apply-workspace-state
               '(:workspace "ws1" :state "RENDER_STATE_MERGE_FAILED"))
              ;; Assert — promoted back to a real, leftmost tab.
              (should (equal established (list "ws1" dir)))
              (should (equal reordered "ws1"))
              (should (eq (agent-repl--ws-get "ws1" :merge-failed) t))))
        (delete-directory dir t)))))

(ert-deftest agent-repl-test-merge-failed-with-an-open-tab-establishes-nothing ()
  "A merge_failed for a workspace whose tab is open re-establishes nothing."
  (agent-repl-test--with-clean-state
    (let (established)
      (agent-repl-test--register-ws "ws1")
      (cl-letf (((symbol-function 'agent-repl--ws-open-p)
                 (lambda (_w) t))
                ((symbol-function 'agent-repl--establish-workspace)
                 (lambda (&rest args) (setq established args))))
        ;; Act
        (agent-repl-test--apply-workspace-state
         '(:workspace "ws1" :state "RENDER_STATE_MERGE_FAILED"))
        ;; Assert
        (should-not established)))))

(ert-deftest agent-repl-test-non-failure-states-resurrect-no-tab ()
  "Non-merge-failed transitions never trigger the tab resurrection."
  (agent-repl-test--with-clean-state
    (let (established)
      (agent-repl-test--register-ws "ws1")
      (cl-letf (((symbol-function 'agent-repl--ws-open-p)
                 (lambda (_w) nil))
                ((symbol-function 'agent-repl--establish-workspace)
                 (lambda (&rest args) (setq established args))))
        ;; Act
        (agent-repl-test--apply-workspace-state
         '(:workspace "ws1" :state "RENDER_STATE_READY"))
        ;; Assert
        (should-not established)))))

(ert-deftest agent-repl-test-tab-resurrection-skips-a-missing-worktree-dir ()
  "A tab-less merge_failed whose worktree is gone from disk is left alone."
  (agent-repl-test--with-clean-state
    (let (established)
      (agent-repl--ws-put "ws1" :project-dir "/nonexistent/agent-repl-gone")
      (cl-letf (((symbol-function 'agent-repl--ws-open-p)
                 (lambda (_w) nil))
                ((symbol-function 'agent-repl--establish-workspace)
                 (lambda (&rest args) (setq established args))))
        ;; Act
        (agent-repl-test--apply-workspace-state
         '(:workspace "ws1" :state "RENDER_STATE_MERGE_FAILED"))
        ;; Assert
        (should-not established)))))

(ert-deftest agent-repl-test-tab-resurrection-is-subscribed-to-the-transition-hook ()
  "The tab resurrector is registered on the state-transition hook."
  (should (memq #'agent-repl--merge-resurrect-on-failure
                agent-repl-ws-state-transition-functions)))

(ert-deftest agent-repl-test-non-merge-live-state-for-an-unowned-workspace-rejects ()
  "Only merge_failed recovers a closed workspace; other live frames reject."
  (agent-repl-test--with-clean-state
    (let (established)
      (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                 (lambda (&rest args) (setq established args))))
        ;; Act — a benign state for an unowned cwd.
        (should-error
         (agent-repl-test--apply-workspace-state
          '(:workspace "/Users/x/not-open" :state "RENDER_STATE_MERGED")))
        ;; Assert
        (should-not established)))))

(ert-deftest agent-repl-test-inbound-frame-accepts-a-workspace-name ()
  "A frame already naming a known workspace still applies."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "doom" :project-dir "/Users/x/.config/doom")
    ;; Act
    (agent-repl-test--apply-workspace-state
     '(:workspace "doom" :state "RENDER_STATE_IDLE"))
    ;; Assert
    (should (eq (agent-repl--ws-get "doom" :pushed-render-state) :idle))))

;;;; ---- The two context cuts --------------------------------------------

(ert-deftest agent-repl-test-apply-workspace-state-decodes-clearing ()
  "RENDER_STATE_CLEARING decodes to the :clearing keyword."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    ;; Act
    (agent-repl-test--apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_CLEARING"))
    ;; Assert
    (should (eq (agent-repl--ws-get "ws1" :pushed-render-state) :clearing))))

(ert-deftest agent-repl-test-apply-workspace-state-decodes-compacting ()
  "RENDER_STATE_COMPACTING decodes to the :compacting keyword."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    ;; Act
    (agent-repl-test--apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_COMPACTING"))
    ;; Assert
    (should (eq (agent-repl--ws-get "ws1" :pushed-render-state) :compacting))))

;;;; ---- ShutdownScheduleView: the recorded drain lease ------------------

(defmacro agent-repl-test--with-clean-lease (&rest body)
  "Run BODY with the recorded drain lease scratch-bound to unknown."
  (declare (indent 0))
  `(let ((agent-repl--frontend-shutdown-schedule nil))
     ,@body))

(ert-deftest agent-repl-test-shutdown-schedule-unknown-until-a-view-arrives ()
  "No pushed lease reads as unknown (nil), which is not the same as idle."
  ;; Arrange / Act / Assert
  (agent-repl-test--with-clean-lease
    (should-not (agent-repl-frontend-shutdown-schedule))))

(ert-deftest agent-repl-test-shutdown-schedule-records-idle ()
  "An `idle' view records the idle state from an EMPTY decoded message."
  ;; Arrange
  (agent-repl-test--with-clean-lease
    ;; Act — `{\"idle\":{}}' decodes to a present key with a nil value.
    (agent-repl--frontend-apply-shutdown-schedule '(:idle nil))
    ;; Assert
    (should (eq (plist-get (agent-repl-frontend-shutdown-schedule) :state)
                :idle))))

(ert-deftest agent-repl-test-shutdown-schedule-records-draining-id ()
  "A `draining' view records the schedule id the cancel command needs."
  ;; Arrange
  (agent-repl-test--with-clean-lease
    ;; Act
    (agent-repl--frontend-apply-shutdown-schedule
     '(:draining (:scheduleId "sch-1" :scheduledAtMs 5 :cause "merge"
                  :stopShims t :holds ((:workspace "/w" :sessionId "s1")))))
    ;; Assert
    (should (equal (agent-repl-frontend-scheduled-shutdown-id) "sch-1"))))

(ert-deftest agent-repl-test-shutdown-schedule-records-draining-cause ()
  "The draining arm's display cause is retained verbatim."
  ;; Arrange
  (agent-repl-test--with-clean-lease
    ;; Act
    (agent-repl--frontend-apply-shutdown-schedule
     '(:draining (:scheduleId "sch-1" :cause "merge of ws rebuilt the daemon")))
    ;; Assert
    (should (equal (plist-get (agent-repl-frontend-shutdown-schedule) :cause)
                   "merge of ws rebuilt the daemon"))))

(ert-deftest agent-repl-test-shutdown-schedule-records-draining-holds ()
  "The holds list is retained so the log can count what the drain waits on."
  ;; Arrange
  (agent-repl-test--with-clean-lease
    ;; Act
    (agent-repl--frontend-apply-shutdown-schedule
     '(:draining (:scheduleId "sch-1"
                  :holds ((:workspace "/a" :sessionId "s1"
                           :turn (:turnId "t1"))
                          (:workspace "/b" :sessionId "s2"
                           :tasks (:count 3))))))
    ;; Assert
    (should (= (length (plist-get (agent-repl-frontend-shutdown-schedule) :holds))
               2))))

(ert-deftest agent-repl-test-shutdown-schedule-transitions-idle-to-draining ()
  "A schedule taken after idle replaces the recorded state."
  ;; Arrange
  (agent-repl-test--with-clean-lease
    (agent-repl--frontend-apply-shutdown-schedule '(:idle nil))
    ;; Act
    (agent-repl--frontend-apply-shutdown-schedule
     '(:draining (:scheduleId "sch-2")))
    ;; Assert
    (should (equal (agent-repl-frontend-scheduled-shutdown-id) "sch-2"))))

(ert-deftest agent-repl-test-shutdown-schedule-transitions-draining-to-idle ()
  "A cancelled schedule clears the recorded id — idle is a real broadcast value."
  ;; Arrange
  (agent-repl-test--with-clean-lease
    (agent-repl--frontend-apply-shutdown-schedule
     '(:draining (:scheduleId "sch-3")))
    ;; Act
    (agent-repl--frontend-apply-shutdown-schedule '(:idle nil))
    ;; Assert
    (should-not (agent-repl-frontend-scheduled-shutdown-id))))

(ert-deftest agent-repl-test-shutdown-schedule-logs-the-transition ()
  "Every lease edge is instrumented, naming both the old and new state."
  ;; Arrange
  (agent-repl-test--with-clean-lease
    (let (logged)
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        ;; Act
        (agent-repl--frontend-apply-shutdown-schedule
         '(:draining (:scheduleId "sch-4" :cause "c")))
        ;; Assert
        (should (seq-find
                 (lambda (m)
                   (string-match-p
                    "frontend-shutdown-schedule: unknown -> draining id=sch-4" m))
                 logged))))))

(ert-deftest agent-repl-test-shutdown-schedule-idle-reports-no-schedule-id ()
  "An idle lease names no schedule: nothing to cancel, and no invented id."
  ;; Arrange
  (agent-repl-test--with-clean-lease
    (agent-repl--frontend-apply-shutdown-schedule '(:idle nil))
    ;; Act / Assert
    (should-not (agent-repl-frontend-scheduled-shutdown-id))))

(ert-deftest agent-repl-test-shutdown-schedule-no-arm-errors ()
  "A view setting neither oneof arm fails loudly, never defaulting to idle."
  ;; Arrange
  (agent-repl-test--with-clean-lease
    ;; Act / Assert
    (should-error (agent-repl--frontend-apply-shutdown-schedule nil))))

(ert-deftest agent-repl-test-shutdown-schedule-both-arms-error ()
  "A view setting BOTH oneof arms fails loudly — the wire allows exactly one."
  ;; Arrange
  (agent-repl-test--with-clean-lease
    ;; Act / Assert
    (should-error (agent-repl--frontend-apply-shutdown-schedule
                   '(:idle nil :draining (:scheduleId "sch-5"))))))

(ert-deftest agent-repl-test-shutdown-schedule-draining-without-id-errors ()
  "A draining arm with no schedule id fails loudly — it could never be cancelled."
  ;; Arrange
  (agent-repl-test--with-clean-lease
    ;; Act / Assert
    (should-error (agent-repl--frontend-apply-shutdown-schedule
                   '(:draining (:cause "no id"))))))

(ert-deftest agent-repl-test-shutdown-schedule-malformed-view-leaves-the-record ()
  "A rejected view must not overwrite the lease it failed to describe."
  ;; Arrange
  (agent-repl-test--with-clean-lease
    (agent-repl--frontend-apply-shutdown-schedule '(:draining (:scheduleId "sch-6")))
    ;; Act
    (ignore-errors (agent-repl--frontend-apply-shutdown-schedule nil))
    ;; Assert
    (should (equal (agent-repl-frontend-scheduled-shutdown-id) "sch-6"))))

(ert-deftest agent-repl-test-snapshot-applies-the-shutdown-schedule ()
  "The connect snapshot's lease lands without waiting for an edge frame."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-clean-lease
      ;; Act
      (agent-repl-test--apply-snapshot
       '(:workspaces nil
         :shutdownSchedule (:draining (:scheduleId "sch-7" :cause "boot mid-drain"))))
      ;; Assert
      (should (equal (agent-repl-frontend-scheduled-shutdown-id) "sch-7")))))

(ert-deftest agent-repl-test-snapshot-without-a-shutdown-schedule-is-not-an-error ()
  "A daemon too old to carry the field leaves the lease honestly unknown."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-clean-lease
      ;; Act
      (agent-repl-test--apply-snapshot '(:workspaces nil))
      ;; Assert
      (should-not (agent-repl-frontend-shutdown-schedule)))))

(ert-deftest agent-repl-test-snapshot-with-a-malformed-lease-still-resyncs ()
  "A malformed lease is contained: the rest of the reconnect still lands."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-clean-lease
      (agent-repl-test--register-ws "ws1")
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
        ;; Act — a present-but-empty view sets no arm.
        (agent-repl-test--apply-snapshot
         '(:workspaces ((:workspace "ws1" :state "RENDER_STATE_IDLE"))
           :shutdownSchedule nil))
        ;; Assert
        (should (eq (agent-repl--ws-get "ws1" :pushed-render-state) :idle))))))

(ert-deftest agent-repl-test-snapshot-with-a-malformed-lease-counts-a-failure ()
  "The contained lease failure is surfaced to the user, never swallowed."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-clean-lease
      (let (messages)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
          ;; Act
          (agent-repl-test--apply-snapshot '(:workspaces nil :shutdownSchedule nil))
          ;; Assert
          (should (seq-find (lambda (m) (string-match-p "FAILED during snapshot resync" m))
                            messages)))))))

(ert-deftest agent-repl-test-snapshot-resync-failure-echoes-user-copy ()
  "The resync failure the user reads is a sentence, and it points at the log."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-clean-lease
      (let (echoed)
        (cl-letf (((symbol-function 'agent-repl--warn) #'ignore)
                  ((symbol-function 'agent-repl--emit-message)
                   (lambda (text &optional _echo) (setq echoed text))))
          ;; Act
          (agent-repl-test--apply-snapshot '(:workspaces nil :shutdownSchedule nil))
          ;; Assert
          (should (equal echoed
                         (concat "agent-repl: 1 item(s) failed to resync — see the "
                                 "agent-repl log for detail"))))))))

(ert-deftest agent-repl-test-snapshot-tolerates-a-queue-entry-shutdown-hold ()
  "A snapshot whose queues carry a lease hold applies without choking."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-clean-lease
      (agent-repl-test--register-ws "ws1")
      ;; Act
      (agent-repl-test--apply-snapshot
       '(:workspaces ((:workspace "ws1" :state "RENDER_STATE_IDLE"))
         :queues ((:workspace "ws1" :fence "f1"
                   :entries ((:id "q1" :text "held"
                              :pending ()
                              :shutdown (:scheduleId "sch-8")))))))
      ;; Assert
      (should (eq (agent-repl--ws-get "ws1" :pushed-render-state) :idle)))))

(ert-deftest agent-repl-test-apply-snapshot-item-failure-records-at-the-warn-rung ()
  "A CONTAINED per-item apply failure is recorded at `warn', not below it.
The item's state never applied, so the containment is a UX regression the
warning sweeps must see — containment is about not aborting the resync,
never about lowering the severity."
  ;; Arrange
  (let (levels)
    (cl-letf (((symbol-function 'agent-repl--persist-log-record)
               (lambda (_ws level _verbosity fmt _args)
                 (when (string-match-p "item FAILED" fmt)
                   (push level levels))))
              ((symbol-function 'agent-repl--emit-message) (lambda (&rest _) nil)))
      ;; Act
      (agent-repl--frontend-apply-snapshot-items
       "session" '((:sessionId "s1")) '(:sessionId)
       (lambda (_item) (error "apply blew up")))
      ;; Assert
      (should (equal levels '("warn"))))))

(ert-deftest agent-repl-test-apply-snapshot-item-failure-stays-contained ()
  "The warn rung does not change the containment: the resync still counts on."
  ;; Arrange / Act
  (let ((failures
         (cl-letf (((symbol-function 'agent-repl--persist-log-record)
                    (lambda (&rest _) nil))
                   ((symbol-function 'agent-repl--emit-message) (lambda (&rest _) nil)))
           (agent-repl--frontend-apply-snapshot-items
            "session" '((:sessionId "s1") (:sessionId "s2")) '(:sessionId)
            (lambda (_item) (error "apply blew up"))))))
    ;; Assert
    (should (= failures 2))))

(ert-deftest agent-repl-test-shutdown-schedule-handler-is-registered ()
  "The lease arm is wired to its handler, not left as unfinished wiring."
  ;; Act / Assert
  (should (eq (cdr (assoc "shutdownSchedule" agent-repl--uds-frame-handlers))
              #'agent-repl--frontend-apply-shutdown-schedule)))
