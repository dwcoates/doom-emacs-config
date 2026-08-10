;;; test-open-fence.el --- Tests for open-fence.el -*- lexical-binding: t; -*-

;;; Commentary:

;; The client-side terminal-open fence: which pushed failure cards stop the
;; automatic re-open loop, which ones deliberately do not, and the two
;; explicit ways back out.  One edge case per test (AAA).

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "test-helpers.el" dir) nil t))

(defun agent-repl-test--fence-card (kind-arm &optional lifecycle-arm message)
  "Return a `FailureCardView' plist of KIND-ARM under LIFECYCLE-ARM.
LIFECYCLE-ARM defaults to `:terminal'; MESSAGE defaults to a sentence."
  (list :kind (list (or kind-arm :sessionResumeFailed) '())
        :message (or message "the conversation could not be reached")
        (or lifecycle-arm :terminal) '()))

(defun agent-repl-test--fence-delta (ws card)
  "Return a `ConversationDelta' plist for WS carrying CARD."
  (list :workspace ws :items (list (list :uuid "start_failed:s1" :failureCard card))))

(defmacro agent-repl-test--with-fenceable-ws (ws &rest body)
  "Run BODY with WS registered as a live workspace."
  (declare (indent 1))
  `(agent-repl-test--with-clean-state
     (agent-repl--ws-put ,ws :project-dir "/tmp/agent-repl-test-open-fence")
     ,@body))

;;;; ---- Reading the card ------------------------------------------------

(ert-deftest agent-repl-test-open-fence-terminal-continuity-card-fences ()
  "A TERMINAL card of a continuity kind is what the fence is for."
  ;; Arrange
  (let ((card (agent-repl-test--fence-card :sessionResumeFailed)))
    ;; Act / Assert
    (should (agent-repl--open-fence-card-terminal-p card))))

(ert-deftest agent-repl-test-open-fence-open-continuity-card-does-not-fence ()
  "An OPEN continuity failure is the retryable case the ensure ladder owns."
  ;; Arrange
  (let ((card (agent-repl-test--fence-card :sessionResumeFailed :open)))
    ;; Act / Assert
    (should-not (agent-repl--open-fence-card-terminal-p card))))

(ert-deftest agent-repl-test-open-fence-terminal-other-kind-does-not-fence ()
  "A terminal card of a NON-continuity kind says nothing about opening."
  ;; Arrange
  (let ((card (agent-repl-test--fence-card :apiRefusal)))
    ;; Act / Assert
    (should-not (agent-repl--open-fence-card-terminal-p card))))

(ert-deftest agent-repl-test-open-fence-unresumable-conversation-kind-fences ()
  "The second continuity kind fences on exactly the same terms as the first."
  ;; Arrange
  (let ((card (agent-repl-test--fence-card :conversationUnresumable)))
    ;; Act / Assert
    (should (agent-repl--open-fence-card-terminal-p card))))

(ert-deftest agent-repl-test-open-fence-card-with-no-kind-does-not-fence ()
  "A card carrying no kind arm is malformed, and malformed never fences."
  ;; Arrange
  (let ((card (list :message "no kind" :terminal '())))
    ;; Act / Assert
    (should-not (agent-repl--open-fence-card-terminal-p card))))

;;;; ---- Applying the delta ----------------------------------------------

(ert-deftest agent-repl-test-open-fence-delta-fences-the-workspace ()
  "A delta carrying a terminal continuity card fences its workspace."
  (agent-repl-test--with-fenceable-ws "alpha-ws"
    ;; Arrange
    (let ((delta (agent-repl-test--fence-delta
                  "alpha-ws" (agent-repl-test--fence-card :sessionResumeFailed))))
      ;; Act
      (agent-repl--open-fence-note-delta delta)
      ;; Assert
      (should (agent-repl--open-fence-active-p "alpha-ws")))))

(ert-deftest agent-repl-test-open-fence-delta-records-the-card-sentence ()
  "The fence cites the card's own sentence, so the skip can say why."
  (agent-repl-test--with-fenceable-ws "alpha-ws"
    ;; Arrange
    (let ((delta (agent-repl-test--fence-delta
                  "alpha-ws"
                  (agent-repl-test--fence-card :sessionResumeFailed :terminal
                                               "transcript is gone"))))
      ;; Act
      (agent-repl--open-fence-note-delta delta)
      ;; Assert
      (should (equal (agent-repl--open-fence-detail "alpha-ws") "transcript is gone")))))

(ert-deftest agent-repl-test-open-fence-delta-with-no-card-leaves-ws-unfenced ()
  "The overwhelmingly common delta carries no failure card and changes nothing."
  (agent-repl-test--with-fenceable-ws "alpha-ws"
    ;; Arrange
    (let ((delta (list :workspace "alpha-ws"
                       :items (list (list :uuid "u1" :permission '())))))
      ;; Act
      (agent-repl--open-fence-note-delta delta)
      ;; Assert
      (should-not (agent-repl--open-fence-active-p "alpha-ws")))))

(ert-deftest agent-repl-test-open-fence-open-card-leaves-ws-unfenced ()
  "An open bring-up failure must keep its retries; only terminal stops them."
  (agent-repl-test--with-fenceable-ws "alpha-ws"
    ;; Arrange
    (let ((delta (agent-repl-test--fence-delta
                  "alpha-ws" (agent-repl-test--fence-card :sessionResumeFailed :open))))
      ;; Act
      (agent-repl--open-fence-note-delta delta)
      ;; Assert
      (should-not (agent-repl--open-fence-active-p "alpha-ws")))))

(ert-deftest agent-repl-test-open-fence-unresolvable-workspace-fences-nothing ()
  "A terminal failure for a workspace this end does not know fences nothing."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((delta (agent-repl-test--fence-delta
                  "/nowhere/at/all" (agent-repl-test--fence-card :sessionResumeFailed))))
      ;; Act / Assert
      (should (= 0 (agent-repl--open-fence-note-delta delta))))))

(ert-deftest agent-repl-test-open-fence-mark-is-idempotent ()
  "The daemon re-publishes its standing card; the fence is established once."
  (agent-repl-test--with-fenceable-ws "alpha-ws"
    ;; Arrange
    (agent-repl--open-fence-mark "alpha-ws" "first")
    ;; Act / Assert
    (should-not (agent-repl--open-fence-mark "alpha-ws" "second"))))

(ert-deftest agent-repl-test-open-fence-mark-keeps-the-first-detail ()
  "A re-observation must not rewrite the account of the fence that stands."
  (agent-repl-test--with-fenceable-ws "alpha-ws"
    ;; Arrange
    (agent-repl--open-fence-mark "alpha-ws" "first")
    ;; Act
    (agent-repl--open-fence-mark "alpha-ws" "second")
    ;; Assert
    (should (equal (agent-repl--open-fence-detail "alpha-ws") "first"))))

;;;; ---- The ways out ----------------------------------------------------

(ert-deftest agent-repl-test-open-fence-clear-releases-the-workspace ()
  "Clearing entitles the ensure ladder to ask the daemon again."
  (agent-repl-test--with-fenceable-ws "alpha-ws"
    ;; Arrange
    (agent-repl--open-fence-mark "alpha-ws" "gone")
    ;; Act
    (agent-repl--open-fence-clear "alpha-ws" "test")
    ;; Assert
    (should-not (agent-repl--open-fence-active-p "alpha-ws"))))

(ert-deftest agent-repl-test-open-fence-clear-drops-the-stale-detail ()
  "A cleared fence must not keep citing the failure it no longer asserts."
  (agent-repl-test--with-fenceable-ws "alpha-ws"
    ;; Arrange
    (agent-repl--open-fence-mark "alpha-ws" "gone")
    ;; Act
    (agent-repl--open-fence-clear "alpha-ws" "test")
    ;; Assert
    (should-not (agent-repl--open-fence-detail "alpha-ws"))))

(ert-deftest agent-repl-test-open-fence-clear-of-an-unfenced-ws-is-a-no-op ()
  "Clearing a workspace that was never fenced reports that it changed nothing."
  (agent-repl-test--with-fenceable-ws "alpha-ws"
    ;; Arrange / Act / Assert
    (should-not (agent-repl--open-fence-clear "alpha-ws" "test"))))

;;;; ---- The loop it stops -----------------------------------------------

(ert-deftest agent-repl-test-open-fence-skips-the-automatic-open ()
  "A fenced workspace no longer sends `openWorkspace' from the ensure ladder."
  (agent-repl-test--with-fenceable-ws "alpha-ws"
    ;; Arrange
    (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () t)))
      (agent-repl--open-fence-mark "alpha-ws" "transcript is gone")
      ;; Act
      (let ((skip (agent-repl--frontend-ensure-skip-reason "alpha-ws")))
        ;; Assert
        (should (string-match-p "terminal open failure" skip))))))

(ert-deftest agent-repl-test-open-fence-unfenced-ws-still-opens ()
  "The skip is the fence's alone: an unfenced workspace still sends its open."
  (agent-repl-test--with-fenceable-ws "alpha-ws"
    ;; Arrange
    (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () t)))
      ;; Act / Assert
      (should-not (agent-repl--frontend-ensure-skip-reason "alpha-ws")))))

;;; test-open-fence.el ends here
