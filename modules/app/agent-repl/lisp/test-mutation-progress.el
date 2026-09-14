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
     (agent-repl-test-mp--progress "op-1" :stage :deriving-name))
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
   (agent-repl-test-mp--progress "op-x" :stage :deriving-name))
  (should-not (gethash "op-x" agent-repl-mutation-progress--pending)))

(ert-deftest agent-repl-test-mp-new-op-id-is-unique ()
  "Two minted op ids differ, so two concurrent creates never collide."
  ;; Act.
  (let ((a (agent-repl-mutation-progress-new-op-id))
        (b (agent-repl-mutation-progress-new-op-id)))
    ;; Assert.
    (should-not (equal a b))))

(provide 'test-mutation-progress)

;;; test-mutation-progress.el ends here
