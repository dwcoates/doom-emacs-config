;;; test-mutation-progress.el --- ERT tests for mutation-progress.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-mutation-progress.el -f ert-run-tests-batch-and-exit
;;
;; The correlation seat matches a decoded WorkspaceMutationProgress push to the
;; command that minted its op id, dispatches its step to the registered
;; callbacks, and drops a terminal op.  These tests drive it with decoded
;; plists — the wire codec is covered by test-wire-host.el — and reset the
;; pending table so each case starts clean.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

(defun agent-repl-test-mp--reset ()
  "Empty the pending-ops table so a test starts with no registrations."
  (clrhash agent-repl-mutation-progress--pending))

(defun agent-repl-test-mp--progress (op-id step-arm step-value)
  "Build a decoded WorkspaceMutationProgress for OP-ID's create STEP-ARM."
  (list :op-id op-id
        :event (list :arm :create
                     :value (list :arm step-arm :value step-value))))

(ert-deftest agent-repl-test-mp-stage-dispatches-to-on-stage ()
  "A stage step calls the op's on-stage callback with the stage keyword."
  ;; Arrange.
  (agent-repl-test-mp--reset)
  (let (got)
    (agent-repl-mutation-progress-register "op-1" :on-stage (lambda (s) (setq got s)))
    ;; Act.
    (agent-repl-mutation-progress-handle
     (agent-repl-test-mp--progress "op-1" :entered-stage :deriving-name))
    ;; Assert.
    (should (eq got :deriving-name))))

(ert-deftest agent-repl-test-mp-succeeded-dispatches-and-forgets ()
  "A succeeded step calls on-succeeded with the value and drops the op."
  ;; Arrange.
  (agent-repl-test-mp--reset)
  (let (got)
    (agent-repl-mutation-progress-register "op-2" :on-succeeded (lambda (v) (setq got v)))
    ;; Act.
    (agent-repl-mutation-progress-handle
     (agent-repl-test-mp--progress
      "op-2" :succeeded (list :workspace '(:id "w" :dir "/d") :name "minted")))
    ;; Assert.
    (should (equal (plist-get got :name) "minted"))
    (should-not (gethash "op-2" agent-repl-mutation-progress--pending))))

(ert-deftest agent-repl-test-mp-failed-dispatches-arm-and-detail-and-forgets ()
  "A failed step calls on-failed with the failure arm and detail, then forgets."
  ;; Arrange.
  (agent-repl-test-mp--reset)
  (let (got-arm got-detail)
    (agent-repl-mutation-progress-register
     "op-3" :on-failed (lambda (arm detail) (setq got-arm arm got-detail detail)))
    ;; Act.
    (agent-repl-mutation-progress-handle
     (agent-repl-test-mp--progress "op-3" :failed (list :arm :internal :value "boom")))
    ;; Assert.
    (should (eq got-arm :internal))
    (should (equal got-detail "boom"))
    (should-not (gethash "op-3" agent-repl-mutation-progress--pending))))

(ert-deftest agent-repl-test-mp-unknown-op-is-dropped-quietly ()
  "A progress event for an op this Emacs never registered is dropped, not erred."
  ;; Arrange: nothing registered.
  (agent-repl-test-mp--reset)
  ;; Act / Assert: handling does not signal, and registers nothing.
  (agent-repl-mutation-progress-handle
   (agent-repl-test-mp--progress "op-x" :entered-stage :deriving-name))
  (should-not (gethash "op-x" agent-repl-mutation-progress--pending)))

(ert-deftest agent-repl-test-mp-new-op-id-is-unique ()
  "Two minted op ids differ, so two concurrent creates never collide."
  ;; Act.
  (let ((a (agent-repl-mutation-progress-new-op-id))
        (b (agent-repl-mutation-progress-new-op-id)))
    ;; Assert.
    (should-not (equal a b))))

;;;; ---- The open arm -----------------------------------------------------

(defun agent-repl-test-mp--open-progress (op-id stage)
  "Build a decoded WorkspaceMutationProgress for OP-ID's open STAGE."
  (list :op-id op-id :event (list :arm :open :value (list :stage stage))))

(ert-deftest agent-repl-test-mp-open-stage-dispatches-to-on-stage ()
  "An open stage calls the op's on-stage callback with the stage keyword."
  ;; Arrange.
  (agent-repl-test-mp--reset)
  (let (got)
    (agent-repl-mutation-progress-register "op-o1" :on-stage (lambda (s) (setq got s)))
    ;; Act.
    (agent-repl-mutation-progress-handle
     (agent-repl-test-mp--open-progress "op-o1" :starting-session))
    ;; Assert.
    (should (eq got :starting-session))))

(ert-deftest agent-repl-test-mp-open-stage-keeps-the-op-registered ()
  "An open has no terminal step on this channel, so a stage never drops it."
  ;; Arrange.
  (agent-repl-test-mp--reset)
  (agent-repl-mutation-progress-register "op-o2" :on-stage #'ignore)
  ;; Act.
  (agent-repl-mutation-progress-handle
   (agent-repl-test-mp--open-progress "op-o2" :checking-build))
  ;; Assert.
  (should (gethash "op-o2" agent-repl-mutation-progress--pending)))

(ert-deftest agent-repl-test-mp-open-without-a-stage-is-reported ()
  "An open event carrying no stage is a contract breach, not a silent drop."
  ;; Arrange.
  (agent-repl-test-mp--reset)
  (let (errors)
    (agent-repl-mutation-progress-register "op-o3" :on-stage #'ignore)
    (cl-letf (((symbol-function 'agent-repl--error)
               (lambda (_ws fmt &rest _args) (push fmt errors))))
      ;; Act.
      (agent-repl-mutation-progress-handle
       (agent-repl-test-mp--open-progress "op-o3" nil)))
    ;; Assert.
    (should errors)))

;;;; ---- The one reporting function ---------------------------------------

(defun agent-repl-test-mp--echoed (thunk)
  "Return the sentences `agent-repl--backend-phase' echoed while THUNK ran."
  (let (said)
    (cl-letf (((symbol-function 'agent-repl--backend-phase)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) said))))
      (funcall thunk))
    (nreverse said)))

(ert-deftest agent-repl-test-mp-report-echoes-a-phase-with-no-detail ()
  "A template with no hole is echoed as written."
  ;; Arrange / Act.
  (let ((said (agent-repl-test-mp--echoed
               (lambda () (agent-repl-workspace-progress-report :open :starting-session)))))
    ;; Assert.
    (should (equal said '("starting the workspace's session…")))))

(ert-deftest agent-repl-test-mp-report-fills-the-template-hole ()
  "A template's hole is filled by the caller's detail."
  ;; Arrange / Act.
  (let ((said (agent-repl-test-mp--echoed
               (lambda ()
                 (agent-repl-workspace-progress-report
                  :create :completed "fix-the-login-test")))))
    ;; Assert.
    (should (equal said '("workspace created: fix-the-login-test")))))

(ert-deftest agent-repl-test-mp-report-passes-details-as-format-arguments ()
  "The TEMPLATE reaches the log unchanged, so no runtime value can become
part of an operation name."
  ;; Arrange.
  (let (templates)
    (cl-letf (((symbol-function 'agent-repl--backend-phase)
               (lambda (_ws fmt &rest _args) (push fmt templates))))
      ;; Act.
      (agent-repl-workspace-progress-report :create :completed "minted"))
    ;; Assert.
    (should (equal templates '("workspace created: %s")))))

(ert-deftest agent-repl-test-mp-report-records-a-failure-at-error ()
  "A failed phase is in the durable record at the level a failure deserves."
  ;; Arrange.
  (let (errors)
    (cl-letf (((symbol-function 'agent-repl--backend-phase) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--error)
               (lambda (_ws fmt &rest _args) (push fmt errors))))
      ;; Act.
      (agent-repl-workspace-progress-report :create :failed "the worktree add failed"))
    ;; Assert.
    (should errors)))

(ert-deftest agent-repl-test-mp-report-refuses-an-unknown-phase ()
  "A phase with no template is reported as the caller bug it is, not invented."
  ;; Arrange.
  (let (errors said)
    (cl-letf (((symbol-function 'agent-repl--backend-phase)
               (lambda (_ws fmt &rest _args) (push fmt said)))
              ((symbol-function 'agent-repl--error)
               (lambda (_ws fmt &rest _args) (push fmt errors))))
      ;; Act / Assert.
      (should-not (agent-repl-workspace-progress-report :open :teleporting))
      (should errors)
      (should-not said))))

(ert-deftest agent-repl-test-mp-report-refuses-an-unknown-kind ()
  "A kind with no phases is refused for the same reason an unknown phase is."
  ;; Arrange.
  (cl-letf (((symbol-function 'agent-repl--backend-phase) (lambda (&rest _) nil))
            ((symbol-function 'agent-repl--error) (lambda (&rest _) nil)))
    ;; Act / Assert.
    (should-not (agent-repl-workspace-progress-report :teleport :requested))))

(ert-deftest agent-repl-test-mp-every-kind-states-the-three-universal-phases ()
  "Every way of making or restoring a workspace can say it asked, it landed,
and it failed -- that is what makes the vocabulary one vocabulary."
  ;; Arrange / Act / Assert.
  (dolist (kind '(:create :open :register :register-repository))
    (dolist (phase '(:requested :completed :failed))
      (should (alist-get phase (alist-get kind agent-repl-workspace-progress-phases))))))

(ert-deftest agent-repl-test-mp-open-states-every-daemon-stage ()
  "Every stage the daemon's WorkspaceOpenStage enum can push has a sentence.
A stage with none would reach the user as the caller bug report instead of
as progress."
  ;; Arrange / Act / Assert.
  (dolist (stage '(:checking-worktree :starting-session :reviving
                   :clearing-closed :checking-build))
    (should (alist-get stage (alist-get :open agent-repl-workspace-progress-phases)))))

(ert-deftest agent-repl-test-mp-create-states-every-daemon-stage ()
  "Every stage the daemon's WorkspaceCreateStage enum can push has a sentence."
  ;; Arrange / Act / Assert.
  (dolist (stage '(:deriving-name :creating-worktree :starting-session))
    (should (alist-get stage (alist-get :create agent-repl-workspace-progress-phases)))))

(ert-deftest agent-repl-test-mp-create-states-the-daemon-s-acceptance ()
  "A create's option-B ack has a sentence, so the whole sequence -- accepted,
the daemon's stages, created or FAILED -- reaches the minibuffer."
  ;; Arrange / Act.
  (let ((said (agent-repl-test-mp--echoed
               (lambda () (agent-repl-workspace-progress-report :create :accepted)))))
    ;; Assert.
    (should (equal said '("the daemon accepted the workspace create…")))))

(ert-deftest agent-repl-test-mp-create-echoes-starting-the-session ()
  "A create's starting_session stage reaches the minibuffer in its own words."
  ;; Arrange / Act.
  (let ((said (agent-repl-test-mp--echoed
               (lambda () (agent-repl-workspace-progress-report :create :starting-session)))))
    ;; Assert.
    (should (equal said '("starting the workspace's session…")))))

(provide 'test-mutation-progress)

;;; test-mutation-progress.el ends here
