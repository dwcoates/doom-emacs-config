;;; test-open-progress.el --- Tests for open-progress.el -*- lexical-binding: t; -*-

;;; Commentary:

;; The workspace-open placeholder: raised inside the keypress, advanced by
;; stages that ARRIVE, escalated when nothing arrives, and resolved on every
;; path.  One edge case per test (AAA).

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "test-helpers.el" dir) nil t))

(defmacro agent-repl-test--with-open-progress (&rest body)
  "Run BODY over a private pending-open registry with display stubbed.
`agent-repl--open-progress-show' reaches the webview's own host-window
resolution, which is frame layout this suite is not about; every test
here asserts the placeholder's CONTENT and registry, so the window
placement is recorded rather than performed."
  (declare (indent 0))
  `(agent-repl-test--with-clean-state
     (cl-letf (((symbol-function 'agent-repl--open-progress-show)
                (lambda (_ws buf) buf)))
       ,@body)))

(defun agent-repl-test--open-progress-text (ws)
  "Return the text of WS's placeholder buffer, or nil when it has none."
  (when-let* ((entry (agent-repl--open-progress-entry ws))
              (buf (plist-get entry :buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf (substring-no-properties (buffer-string))))))

;;;; ---- Raising the placeholder -----------------------------------------

(ert-deftest agent-repl-test-open-progress-start-names-the-workspace ()
  "The placeholder raised by an open names the workspace being opened."
  ;; Arrange / Act
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    ;; Assert
    (should (string-match-p "Opening alpha-ws"
                            (agent-repl-test--open-progress-text "alpha-ws")))))

(ert-deftest agent-repl-test-open-progress-start-is-synchronous ()
  "The placeholder buffer is live the instant `-start' returns."
  ;; Arrange / Act
  (agent-repl-test--with-open-progress
    (let ((buf (agent-repl--open-progress-start "alpha-ws")))
      ;; Assert
      (should (buffer-live-p buf)))))

(ert-deftest agent-repl-test-open-progress-start-marks-workspace-active ()
  "A raised placeholder makes its workspace report a pending open."
  ;; Arrange / Act
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    ;; Assert
    (should (agent-repl--open-progress-active-p "alpha-ws"))))

(ert-deftest agent-repl-test-open-progress-inactive-workspace-is-not-active ()
  "A workspace with no open in flight reports no pending open."
  ;; Arrange / Act / Assert
  (agent-repl-test--with-open-progress
    (should-not (agent-repl--open-progress-active-p "alpha-ws"))))

;;;; ---- Double invocation -----------------------------------------------

(ert-deftest agent-repl-test-open-progress-second-start-returns-nil ()
  "A second start while an open is pending refuses, so no open is dispatched."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    ;; Act / Assert
    (should-not (agent-repl--open-progress-start "alpha-ws"))))

(ert-deftest agent-repl-test-open-progress-second-start-keeps-one-buffer ()
  "A second start reuses the standing placeholder rather than stacking one."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (let ((first (agent-repl--open-progress-start "alpha-ws")))
      ;; Act
      (agent-repl--open-progress-start "alpha-ws")
      ;; Assert
      (should (eq first (plist-get (agent-repl--open-progress-entry "alpha-ws")
                                   :buffer))))))

(ert-deftest agent-repl-test-open-progress-second-start-does-not-reset-phase ()
  "A second start leaves the phase the first open has already reached."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    (agent-repl--open-progress-note "alpha-ws" :acked)
    ;; Act
    (agent-repl--open-progress-start "alpha-ws")
    ;; Assert
    (should (eq :acked (plist-get (agent-repl--open-progress-entry "alpha-ws")
                                  :phase)))))

;;;; ---- Phase advances --------------------------------------------------

(ert-deftest agent-repl-test-open-progress-ack-advances-the-phase ()
  "An arriving acknowledgement moves the placeholder to `:acked'."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    ;; Act
    (agent-repl--open-progress-note "alpha-ws" :acked)
    ;; Assert
    (should (eq :acked (plist-get (agent-repl--open-progress-entry "alpha-ws")
                                  :phase)))))

(ert-deftest agent-repl-test-open-progress-ack-marks-earlier-stages-cleared ()
  "The stage the open has passed renders as cleared, not as current."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    ;; Act
    (agent-repl--open-progress-note "alpha-ws" :acked)
    ;; Assert
    (should (string-match-p
             "✓ Asking the daemon"
             (agent-repl-test--open-progress-text "alpha-ws")))))

(ert-deftest agent-repl-test-open-progress-ack-marks-its-own-stage-current ()
  "The stage just reached renders as the current one."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    ;; Act
    (agent-repl--open-progress-note "alpha-ws" :acked)
    ;; Assert
    (should (string-match-p
             "▸ Acknowledged; bringing the session up"
             (agent-repl-test--open-progress-text "alpha-ws")))))

(ert-deftest agent-repl-test-open-progress-note-refuses-a-regression ()
  "A stage report arriving late cannot walk the ladder backwards."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    (agent-repl--open-progress-note "alpha-ws" :rendering)
    ;; Act
    (agent-repl--open-progress-note "alpha-ws" :daemon-ready)
    ;; Assert
    (should (eq :rendering (plist-get (agent-repl--open-progress-entry "alpha-ws")
                                      :phase)))))

(ert-deftest agent-repl-test-open-progress-note-refuses-an-unknown-phase ()
  "A phase that is not on the ladder moves nothing."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    ;; Act / Assert
    (should-not (agent-repl--open-progress-note "alpha-ws" :not-a-stage))))

(ert-deftest agent-repl-test-open-progress-note-is-silent-without-a-placeholder ()
  "A background open raises no placeholder, so stage reports do nothing."
  ;; Arrange / Act / Assert
  (agent-repl-test--with-open-progress
    (should-not (agent-repl--open-progress-note "alpha-ws" :acked))))

;;;; ---- Failure ---------------------------------------------------------

(ert-deftest agent-repl-test-open-progress-nack-shows-the-cause ()
  "A refused open replaces the ladder with the daemon's stated cause."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    ;; Act
    (agent-repl--open-progress-fail "alpha-ws" "command rejected: no such worktree")
    ;; Assert
    (should (string-match-p "command rejected: no such worktree"
                            (agent-repl-test--open-progress-text "alpha-ws")))))

(ert-deftest agent-repl-test-open-progress-nack-leaves-the-placeholder-standing ()
  "A failed open keeps its report on the frame instead of vanishing."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    ;; Act
    (agent-repl--open-progress-fail "alpha-ws" "command rejected")
    ;; Assert
    (should (agent-repl--open-progress-active-p "alpha-ws"))))

(ert-deftest agent-repl-test-open-progress-nack-freezes-later-stage-reports ()
  "A stage report arriving after a failure cannot overwrite the cause."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    (agent-repl--open-progress-fail "alpha-ws" "command rejected")
    ;; Act
    (agent-repl--open-progress-note "alpha-ws" :rendering)
    ;; Assert
    (should (eq :failed (plist-get (agent-repl--open-progress-entry "alpha-ws")
                                   :phase)))))

;;;; ---- Escalation ------------------------------------------------------

(ert-deftest agent-repl-test-open-progress-escalation-names-the-timeout ()
  "A deadline that passes escalates the placeholder to a visible warning."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    ;; Act
    (agent-repl--open-progress-escalate "alpha-ws")
    ;; Assert
    (should (string-match-p "Still opening alpha-ws"
                            (agent-repl-test--open-progress-text "alpha-ws")))))

(ert-deftest agent-repl-test-open-progress-escalation-names-the-last-stage ()
  "The escalation says which stage the open was still sitting on."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    (agent-repl--open-progress-note "alpha-ws" :opening)
    ;; Act
    (agent-repl--open-progress-escalate "alpha-ws")
    ;; Assert
    (should (string-match-p "openWorkspace sent"
                            (agent-repl-test--open-progress-text "alpha-ws")))))

(ert-deftest agent-repl-test-open-progress-escalation-suggests-a-remedy ()
  "The escalation tells the user what to try, not only what stalled."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    ;; Act
    (agent-repl--open-progress-escalate "alpha-ws")
    ;; Assert
    (should (string-match-p "agent-repl-frontend-daemon-restart"
                            (agent-repl-test--open-progress-text "alpha-ws")))))

(ert-deftest agent-repl-test-open-progress-escalation-spares-a-settled-failure ()
  "An escalation firing after a failure leaves the stated cause alone."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    (agent-repl--open-progress-fail "alpha-ws" "command rejected")
    ;; Act
    (agent-repl--open-progress-escalate "alpha-ws")
    ;; Assert
    (should (eq :failed (plist-get (agent-repl--open-progress-entry "alpha-ws")
                                   :phase)))))

(ert-deftest agent-repl-test-open-progress-escalation-is-silent-after-success ()
  "An escalation firing after teardown raises no placeholder of its own."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    (agent-repl--open-progress-finish "alpha-ws")
    ;; Act
    (agent-repl--open-progress-escalate "alpha-ws")
    ;; Assert
    (should-not (agent-repl--open-progress-active-p "alpha-ws"))))

;;;; ---- Success teardown ------------------------------------------------

(ert-deftest agent-repl-test-open-progress-finish-kills-the-placeholder ()
  "A mounted view leaves no placeholder buffer behind."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (let ((buf (agent-repl--open-progress-start "alpha-ws")))
      ;; Act
      (agent-repl--open-progress-finish "alpha-ws")
      ;; Assert
      (should-not (buffer-live-p buf)))))

(ert-deftest agent-repl-test-open-progress-finish-clears-the-registry ()
  "A finished open stops reporting a pending open."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    ;; Act
    (agent-repl--open-progress-finish "alpha-ws")
    ;; Assert
    (should-not (agent-repl--open-progress-active-p "alpha-ws"))))

(ert-deftest agent-repl-test-open-progress-finish-cancels-the-escalation ()
  "A finished open disarms its escalation timer."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    (let ((timer (plist-get (agent-repl--open-progress-entry "alpha-ws") :timer)))
      ;; Act
      (agent-repl--open-progress-finish "alpha-ws")
      ;; Assert
      (should-not (memq timer timer-list)))))

(ert-deftest agent-repl-test-open-progress-finish-without-a-placeholder-is-nil ()
  "Finishing an open that raised no placeholder tears nothing down."
  ;; Arrange / Act / Assert
  (agent-repl-test--with-open-progress
    (should-not (agent-repl--open-progress-finish "alpha-ws"))))

(ert-deftest agent-repl-test-open-progress-workspaces-are-independent ()
  "Finishing one workspace's open leaves another workspace's placeholder up."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    (agent-repl--open-progress-start "beta-ws")
    ;; Act
    (agent-repl--open-progress-finish "alpha-ws")
    ;; Assert
    (should (agent-repl--open-progress-active-p "beta-ws"))))

;;;; ---- Pushed-state subscription ---------------------------------------

(ert-deftest agent-repl-test-open-progress-pushed-init-reports-bring-up ()
  "A pushed `:init' moves the placeholder onto the bring-up stage."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    ;; Act
    (agent-repl--open-progress-react-to-pushed-state "alpha-ws" :init nil)
    ;; Assert
    (should (eq :acked (plist-get (agent-repl--open-progress-entry "alpha-ws")
                                  :phase)))))

(ert-deftest agent-repl-test-open-progress-pushed-live-state-reports-backfill ()
  "A live session whose transcript has not landed reports the backfill."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    (cl-letf (((symbol-function 'agent-repl--frontend-ws-command-key)
               (lambda (_ws) "/tmp/alpha"))
              ((symbol-function 'agent-repl--frontend-backfill-settled-p)
               (lambda (_key) nil)))
      ;; Act
      (agent-repl--open-progress-react-to-pushed-state "alpha-ws" :idle :init))
    ;; Assert
    (should (eq :backfilling (plist-get (agent-repl--open-progress-entry "alpha-ws")
                                        :phase)))))

(ert-deftest agent-repl-test-open-progress-pushed-live-settled-reports-rendering ()
  "A live session with its transcript in hand reports the render stage."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    (cl-letf (((symbol-function 'agent-repl--frontend-ws-command-key)
               (lambda (_ws) "/tmp/alpha"))
              ((symbol-function 'agent-repl--frontend-backfill-settled-p)
               (lambda (_key) t)))
      ;; Act
      (agent-repl--open-progress-react-to-pushed-state "alpha-ws" :idle :init))
    ;; Assert
    (should (eq :rendering (plist-get (agent-repl--open-progress-entry "alpha-ws")
                                      :phase)))))

(ert-deftest agent-repl-test-open-progress-pushed-severed-fails-the-open ()
  "A pushed `:severed' resolves the pending open as failed."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    ;; Act
    (agent-repl--open-progress-react-to-pushed-state "alpha-ws" :severed :init)
    ;; Assert
    (should (eq :failed (plist-get (agent-repl--open-progress-entry "alpha-ws")
                                   :phase)))))

(ert-deftest agent-repl-test-open-progress-pushed-hibernated-holds-the-ladder ()
  "A pushed `:hibernated' is what the open is about to wake, so it moves nothing."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    ;; Act
    (agent-repl--open-progress-react-to-pushed-state "alpha-ws" :hibernated nil)
    ;; Assert
    (should (eq :dispatched (plist-get (agent-repl--open-progress-entry "alpha-ws")
                                       :phase)))))

(ert-deftest agent-repl-test-open-progress-pushed-state-ignores-idle-workspaces ()
  "A workspace with no pending open raises no placeholder from a pushed state."
  ;; Arrange / Act
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-react-to-pushed-state "alpha-ws" :init nil)
    ;; Assert
    (should-not (agent-repl--open-progress-active-p "alpha-ws"))))

(ert-deftest agent-repl-test-open-progress-is-subscribed-to-pushed-state ()
  "The reactor is registered on the pushed-state hook, so phases arrive at all."
  ;; Arrange / Act / Assert
  (should (memq #'agent-repl--open-progress-react-to-pushed-state
                agent-repl-ws-state-transition-functions)))

;;;; ---- Teardown when the workspace itself goes away ----------------------

(ert-deftest agent-repl-test-open-progress-abandon-cancels-the-escalation ()
  "Closing a workspace mid-open disarms its escalation timer.
Regression: the timer outlived the workspace and later emitted a record
against a name the registry no longer resolved."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    (let ((timer (plist-get (agent-repl--open-progress-entry "alpha-ws") :timer)))
      ;; Act
      (agent-repl--open-progress-abandon "alpha-ws")
      ;; Assert
      (should-not (memq timer timer-list)))))

(ert-deftest agent-repl-test-open-progress-abandon-drops-the-entry ()
  "An abandoned placeholder leaves no registry entry behind."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    ;; Act
    (agent-repl--open-progress-abandon "alpha-ws")
    ;; Assert
    (should-not (agent-repl--open-progress-active-p "alpha-ws"))))

(ert-deftest agent-repl-test-open-progress-abandon-kills-the-buffer ()
  "An abandoned placeholder leaves no standing buffer: there is no workspace
left for the user to read a verdict about."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    (let ((buf (plist-get (agent-repl--open-progress-entry "alpha-ws") :buffer)))
      ;; Act
      (agent-repl--open-progress-abandon "alpha-ws")
      ;; Assert
      (should-not (buffer-live-p buf)))))

(ert-deftest agent-repl-test-open-progress-abandon-without-a-placeholder-is-nil ()
  "A workspace closed with no open in flight tears nothing down."
  ;; Arrange / Act / Assert
  (agent-repl-test--with-open-progress
    (should-not (agent-repl--open-progress-abandon "alpha-ws"))))

(ert-deftest agent-repl-test-open-progress-abandon-leaves-peers-alone ()
  "Closing one workspace must not tear down another workspace's placeholder."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    (agent-repl--open-progress-start "beta-ws")
    ;; Act
    (agent-repl--open-progress-abandon "alpha-ws")
    ;; Assert
    (should (agent-repl--open-progress-active-p "beta-ws"))))

(ert-deftest agent-repl-test-open-progress-subscribes-to-ws-del ()
  "The teardown is wired to workspace deletion, not merely available."
  ;; Arrange / Act / Assert
  (should (memq #'agent-repl--open-progress-abandon agent-repl-ws-del-hook)))

(provide 'test-open-progress)
;;; test-open-progress.el ends here
