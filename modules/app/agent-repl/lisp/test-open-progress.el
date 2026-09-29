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
             "▸ The daemon answered"
             (agent-repl-test--open-progress-text "alpha-ws")))))

(ert-deftest agent-repl-test-open-progress-note-refuses-a-regression ()
  "A stage report arriving late cannot walk the ladder backwards."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    (agent-repl--open-progress-note "alpha-ws" :host-state)
    ;; Act
    (agent-repl--open-progress-note "alpha-ws" :acked)
    ;; Assert
    (should (eq :host-state (plist-get (agent-repl--open-progress-entry "alpha-ws")
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

(ert-deftest agent-repl-test-open-progress-escalation-names-the-first-missing-stage ()
  "The diagnosis names what has NOT happened — that is what is wrong."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    (agent-repl--open-progress-note "alpha-ws" :acked)
    ;; Act
    (agent-repl--open-progress-escalate "alpha-ws")
    ;; Assert
    (should (string-match-p "stalled waiting for: The workspace's host state arrived"
                            (agent-repl-test--open-progress-text "alpha-ws")))))

(ert-deftest agent-repl-test-open-progress-escalation-also-names-the-stage-reached ()
  "How far the open got is reported beside what it is waiting for."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    (agent-repl--open-progress-note "alpha-ws" :acked)
    ;; Act
    (agent-repl--open-progress-escalate "alpha-ws")
    ;; Assert
    (should (string-match-p "reached: The daemon answered"
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

;;;; ---- The host stream and the webview ------------------------------

(ert-deftest agent-repl-test-open-progress-a-host-push-advances-the-ladder ()
  "The FIRST host push for the workspace is the `:host-state' stage."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    (agent-repl--open-progress-note "alpha-ws" :acked)
    ;; Act
    (agent-repl--open-progress-note-host-state "alpha-ws" nil)
    ;; Assert
    (should (eq :host-state (plist-get (agent-repl--open-progress-entry "alpha-ws")
                                       :phase)))))

(ert-deftest agent-repl-test-open-progress-a-later-host-push-moves-nothing ()
  "The stage is \"a host state ARRIVED\", not \"the newest one\"."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    (agent-repl--open-progress-note "alpha-ws" :acked)
    (agent-repl--open-progress-note-host-state "alpha-ws" nil)
    (agent-repl-open-progress-note-loaded "alpha-ws")
    ;; Act
    (agent-repl--open-progress-note-host-state "alpha-ws" nil)
    ;; Assert
    (should (eq :loaded (plist-get (agent-repl--open-progress-entry "alpha-ws")
                                   :phase)))))

(ert-deftest agent-repl-test-open-progress-the-webviews-load-completes-the-ladder ()
  "`:loaded' comes from the xwidget's own load-finished event."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    (agent-repl--open-progress-note "alpha-ws" :host-state)
    ;; Act
    (agent-repl-open-progress-note-loaded "alpha-ws")
    ;; Assert
    (should (eq :loaded (plist-get (agent-repl--open-progress-entry "alpha-ws")
                                   :phase)))))

(ert-deftest agent-repl-test-open-progress-a-host-push-for-an-idle-workspace-is-ignored ()
  "A workspace with no open in flight raises no placeholder to advance."
  ;; Arrange
  (agent-repl-test--with-open-progress
    ;; Act
    (agent-repl--open-progress-note-host-state "alpha-ws" nil)
    ;; Assert
    (should-not (agent-repl--open-progress-active-p "alpha-ws"))))

(ert-deftest agent-repl-test-open-progress-is-subscribed-to-the-host-stream ()
  "The ladder rides the host stream's own push hook, and polls nothing."
  ;; Act / Assert
  (should (memq #'agent-repl--open-progress-note-host-state
                agent-repl-host-update-functions)))


;;;; ---- Publishing the pending-open set ---------------------------------
;;
;; The daemon's mode-line segment reports the cold start's workspace
;; bring-up by READING this registry.  These pin the reader and its
;; notification, because a second tally kept elsewhere is how a progress
;; display starts disagreeing with itself.

(ert-deftest agent-repl-test-open-progress-a-pending-open-is-reported-as-opening ()
  "A workspace whose open is in flight has not been painted yet."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    ;; Act / Assert
    (should (equal '("alpha-ws") (agent-repl-open-progress-opening-workspaces)))))

(ert-deftest agent-repl-test-open-progress-a-loaded-open-is-not-reported-as-opening ()
  "`:loaded' is the webview's own load-finished event -- the panel is up."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    (agent-repl--open-progress-note "alpha-ws" :host-state)
    (agent-repl-open-progress-note-loaded "alpha-ws")
    ;; Act / Assert
    (should (null (agent-repl-open-progress-opening-workspaces)))))

(ert-deftest agent-repl-test-open-progress-a-failed-open-is-not-reported-as-opening ()
  "A standing failure is a resolved open, not work still in progress."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    (agent-repl--open-progress-fail "alpha-ws" "the daemon refused")
    ;; Act / Assert
    (should (null (agent-repl-open-progress-opening-workspaces)))))

(ert-deftest agent-repl-test-open-progress-a-finished-open-is-not-reported-as-opening ()
  "A torn-down placeholder holds no entry, so it is absent by construction."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    (agent-repl--open-progress-finish "alpha-ws")
    ;; Act / Assert
    (should (null (agent-repl-open-progress-opening-workspaces)))))

(ert-deftest agent-repl-test-open-progress-a-started-open-publishes-the-change ()
  "Raising a placeholder tells the readers the answer moved."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (let* ((runs 0)
           (agent-repl-open-progress-change-functions
            (list (lambda () (setq runs (1+ runs))))))
      ;; Act
      (agent-repl--open-progress-start "alpha-ws")
      ;; Assert
      (should (= runs 1)))))

(ert-deftest agent-repl-test-open-progress-an-advance-publishes-the-change ()
  "A phase advance can be the moment a workspace becomes painted."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    (let* ((runs 0)
           (agent-repl-open-progress-change-functions
            (list (lambda () (setq runs (1+ runs))))))
      ;; Act
      (agent-repl--open-progress-note "alpha-ws" :host-state)
      ;; Assert
      (should (= runs 1)))))

(ert-deftest agent-repl-test-open-progress-a-finish-publishes-the-change ()
  "Tearing the placeholder down is the commonest way the answer moves."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (agent-repl--open-progress-start "alpha-ws")
    (let* ((runs 0)
           (agent-repl-open-progress-change-functions
            (list (lambda () (setq runs (1+ runs))))))
      ;; Act
      (agent-repl--open-progress-finish "alpha-ws")
      ;; Assert
      (should (= runs 1)))))

(ert-deftest agent-repl-test-open-progress-a-signalling-change-handler-is-contained ()
  "A display handler that breaks must not take down the open it reports on."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (let ((agent-repl-open-progress-change-functions
           (list (lambda () (error "the mode line blew up")))))
      ;; Act / Assert
      (should (agent-repl--open-progress-start "alpha-ws")))))


;;;; ---- What the published change says in the minibuffer ----------------
;;
;; The count itself is open-progress's answer; the daemon's handler is the
;; reader that turns it into a line the user reads.  These drive the REAL
;; handler off the REAL publication, so the two cannot drift.

(defmacro agent-repl-test--open-progress-capturing-echoes (&rest body)
  "Run BODY with loud `agent-repl--emit-message' lines collected in `echoes'."
  (declare (indent 0))
  `(let ((echoes nil))
     (cl-letf (((symbol-function 'agent-repl--emit-message)
                (lambda (text &optional echo)
                  (when echo (setq echoes (append echoes (list text))))
                  text))
               ((symbol-function 'agent-repl-daemon--refresh-segment)
                (lambda () nil)))
       ,@body)))

(ert-deftest agent-repl-test-open-progress-a-published-change-echoes-the-count ()
  "An open that advances is what puts the bring-up count in the minibuffer."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (let ((agent-repl-roster--tab-order '("alpha-ws" "beta-ws"))
          (agent-repl-daemon--workspace-echo-done nil)
          (agent-repl-open-progress-change-functions
           (list #'agent-repl-daemon-on-open-progress-change)))
      (agent-repl-test--open-progress-capturing-echoes
        ;; Act
        (agent-repl--open-progress-start "alpha-ws")
        ;; Assert
        (should (equal echoes '("agent-repl: loading workspaces (1/2)…")))))))

(ert-deftest agent-repl-test-open-progress-a-published-change-is-quiet-in-the-minibuffer ()
  "An open advancing while the user is typing must not take their prompt."
  ;; Arrange
  (agent-repl-test--with-open-progress
    (let ((agent-repl-roster--tab-order '("alpha-ws" "beta-ws"))
          (agent-repl-daemon--workspace-echo-done nil)
          (agent-repl-open-progress-change-functions
           (list #'agent-repl-daemon-on-open-progress-change)))
      (agent-repl-test--open-progress-capturing-echoes
        (cl-letf (((symbol-function 'minibuffer-depth) (lambda () 1)))
          ;; Act
          (agent-repl--open-progress-start "alpha-ws"))
        ;; Assert
        (should (null echoes))))))

(provide 'test-open-progress)
;;; test-open-progress.el ends here

;;;; ---- The layout under the placeholder ----

(ert-deftest agent-repl-test-open-progress-show-saves-the-layout-before-covering-it ()
  "The placeholder saves the frame it covers before it covers it.
It is the first part of the mount to take the frame, so the saved
pre-panel layout must show the user's buffer, never the placeholder."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((work (get-buffer-create "*placeholder-work1*"))
          (buf  (get-buffer-create " *agent-opening-save1*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--frontend-main-area-window)
                     (lambda () (selected-window))))
            (delete-other-windows)
            (set-window-buffer (selected-window) work)
            ;; Act
            (agent-repl--open-progress-show "test-ws" buf)
            ;; Assert
            (let ((saved (agent-repl--ws-get "test-ws" :fullscreen-config)))
              (should (window-configuration-p saved))
              (save-window-excursion
                (set-window-configuration saved)
                (should (eq (window-buffer (selected-window)) work)))))
        (kill-buffer work)
        (kill-buffer buf)
        (delete-other-windows)))))
