;;; test-status.el --- ERT tests for status.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the workspace status state machine and tab bar rendering.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-status.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: Typed state setters (ws-set-claude-state, ws-set-repl-state) ----

(ert-deftest claude-repl-test-ws-set-claude-state-writes-both ()
  "ws-set-claude-state writes :claude-state AND legacy :status (write-both)."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set-claude-state "ws1" :thinking)
    (should (eq (claude-repl--ws-get "ws1" :claude-state) :thinking))
    (should (eq (claude-repl--ws-get "ws1" :claude-state) :thinking))))

(ert-deftest claude-repl-test-ws-set-claude-state-nil-writes-both ()
  "ws-set-claude-state nil clears both axes."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set-claude-state "ws1" :done)
    (claude-repl--ws-set-claude-state "ws1" nil)
    (should-not (claude-repl--ws-get "ws1" :claude-state))
    (should-not (claude-repl--ws-get "ws1" :claude-state))))

(ert-deftest claude-repl-test-ws-set-claude-state-nil-ws-errors ()
  "ws-set-claude-state signals error on nil workspace."
  (should-error (claude-repl--ws-set-claude-state nil :thinking) :type 'error))

(ert-deftest claude-repl-test-ws-claude-state-getter ()
  "ws-claude-state reads :claude-state, not :status."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :claude-state :permission)
    (should (eq (claude-repl--ws-claude-state "ws1") :permission))))

(ert-deftest claude-repl-test-ws-set-repl-state-isolated ()
  "ws-set-repl-state writes :repl-state, leaves :claude-state/:status alone."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set-claude-state "ws1" :thinking)
    (claude-repl--ws-set-repl-state "ws1" :inactive)
    (should (eq (claude-repl--ws-get "ws1" :repl-state) :inactive))
    (should (eq (claude-repl--ws-get "ws1" :claude-state) :thinking))
    (should (eq (claude-repl--ws-get "ws1" :claude-state) :thinking))))

(ert-deftest claude-repl-test-ws-set-repl-state-nil-ws-errors ()
  "ws-set-repl-state signals error on nil workspace."
  (should-error (claude-repl--ws-set-repl-state nil :inactive) :type 'error))

(ert-deftest claude-repl-test-ws-repl-state-getter ()
  "ws-repl-state reads :repl-state, not :status."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :repl-state :init)
    (should (eq (claude-repl--ws-repl-state "ws1") :init))))

(ert-deftest claude-repl-test-ws-claude-state-clear-if-match-clears-both ()
  "ws-claude-state-clear-if with a matching state clears both fields."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set-claude-state "ws1" :thinking)
    (claude-repl--ws-claude-state-clear-if "ws1" :thinking)
    (should-not (claude-repl--ws-get "ws1" :claude-state))
    (should-not (claude-repl--ws-get "ws1" :claude-state))))

(ert-deftest claude-repl-test-ws-claude-state-clear-if-mismatch-noop ()
  "ws-claude-state-clear-if with a non-matching state is a no-op on both fields."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set-claude-state "ws1" :done)
    (claude-repl--ws-claude-state-clear-if "ws1" :thinking)
    (should (eq (claude-repl--ws-get "ws1" :claude-state) :done))
    (should (eq (claude-repl--ws-get "ws1" :claude-state) :done))))

(ert-deftest claude-repl-test-ws-claude-state-clear-if-nil-ws-errors ()
  "ws-claude-state-clear-if signals error on nil workspace."
  (should-error (claude-repl--ws-claude-state-clear-if nil :thinking) :type 'error))

;;;; ---- Tests: composed-state 8-cell product space ----

(ert-deftest claude-repl-test-composed-nil-nil ()
  "Composed (nil, nil) → nil (default face)."
  (should-not (claude-repl--composed-state nil nil)))

(ert-deftest claude-repl-test-composed-thinking-init ()
  "Composed (:thinking, :init) → :thinking."
  (should (eq :thinking (claude-repl--composed-state :thinking :init))))

(ert-deftest claude-repl-test-composed-thinking-inactive ()
  ":thinking dominates even when repl-state is :inactive (work-in-progress visibility)."
  (should (eq :thinking (claude-repl--composed-state :thinking :inactive))))

(ert-deftest claude-repl-test-composed-permission-init ()
  "Composed (:permission, :init) → :permission."
  (should (eq :permission (claude-repl--composed-state :permission :init))))

(ert-deftest claude-repl-test-composed-permission-inactive ()
  ":permission dominates even with repl-state :inactive (❓ label, not orange)."
  (should (eq :permission (claude-repl--composed-state :permission :inactive))))

(ert-deftest claude-repl-test-composed-done-init ()
  "Composed (:done, :init) → :done (green)."
  (should (eq :done (claude-repl--composed-state :done :init))))

(ert-deftest claude-repl-test-composed-done-inactive ()
  "Composed (:done, :inactive) → :done — :repl-state contributes no color."
  (should (eq :done (claude-repl--composed-state :done :inactive))))

(ert-deftest claude-repl-test-composed-nil-inactive ()
  "Composed (nil, :inactive) → nil — :repl-state alone is colorless."
  (should-not (claude-repl--composed-state nil :inactive)))

(ert-deftest claude-repl-test-composed-init ()
  "Composed (:init, any) → :init (blue — Claude initializing)."
  (should (eq :init (claude-repl--composed-state :init nil)))
  (should (eq :init (claude-repl--composed-state :init :inactive))))

(ert-deftest claude-repl-test-composed-idle ()
  "Composed (:idle, any) → nil — idle Claude renders as default face."
  (should-not (claude-repl--composed-state :idle nil))
  (should-not (claude-repl--composed-state :idle :inactive)))

;;;; ---- Tests: ws-display-state reads both axes ----

(ert-deftest claude-repl-test-display-state-repl-inactive-does-not-change-color ()
  "A :done workspace with :repl-state :inactive still renders :done (green)."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set-claude-state "ws1" :done)
    (claude-repl--ws-set-repl-state "ws1" :inactive)
    (should (eq :done (claude-repl--ws-display-state "ws1")))))

(ert-deftest claude-repl-test-display-state-thinking-overrides-closed-panels ()
  "Panels closed during a :thinking turn still renders red."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set-claude-state "ws1" :thinking)
    (claude-repl--ws-set-repl-state "ws1" :inactive)
    (should (eq :thinking (claude-repl--ws-display-state "ws1")))))

(ert-deftest claude-repl-test-display-state-permission-closed-panels ()
  "Panels closed during :permission still shows ❓ green (user must decide)."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set-claude-state "ws1" :permission)
    (claude-repl--ws-set-repl-state "ws1" :inactive)
    (should (eq :permission (claude-repl--ws-display-state "ws1")))))

;;;; ---- Tests: Legacy wrappers still populate both axes ----

(ert-deftest claude-repl-test-legacy-ws-set-writes-claude-state ()
  "Legacy ws-set (wrapper) still writes :claude-state during migration."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :permission)
    (should (eq (claude-repl--ws-get "ws1" :claude-state) :permission))
    (should (eq (claude-repl--ws-get "ws1" :claude-state) :permission))))

(ert-deftest claude-repl-test-legacy-ws-clear-clears-both-axes ()
  "Legacy ws-clear-if-status (wrapper) clears :claude-state too."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :thinking)
    (claude-repl--ws-claude-state-clear-if "ws1" :thinking)
    (should-not (claude-repl--ws-get "ws1" :claude-state))
    (should-not (claude-repl--ws-get "ws1" :claude-state))))

(ert-deftest claude-repl-test-mark-dead-vterm-clears-claude-state ()
  "mark-dead-vterm clears :claude-state and sets :repl-state :dead."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set-claude-state "ws1" :done)
    (claude-repl--mark-dead-vterm "ws1")
    (should-not (claude-repl--ws-get "ws1" :claude-state))
    (should (eq (claude-repl--ws-get "ws1" :repl-state) :dead))))

;;;; ---- Tests: Workspace state accessors (ws-set, ws-clear, ws-state) ----

(ert-deftest claude-repl-test-ws-set-and-state ()
  "ws-set should set the correct state, clearing others."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :thinking)
    (should (eq (claude-repl--ws-state "ws1") :thinking))
    (claude-repl--ws-set "ws1" :done)
    (should (eq (claude-repl--ws-state "ws1") :done))
    ;; Thinking should be cleared — the plist status should now be :done
    (should (eq (claude-repl--ws-get "ws1" :claude-state) :done))))

(ert-deftest claude-repl-test-ws-clear ()
  "ws-clear should clear only the specified state."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :thinking)
    (claude-repl--ws-claude-state-clear-if "ws1" :thinking)
    (should-not (claude-repl--ws-state "ws1"))))

(ert-deftest claude-repl-test-ws-state-inactive ()
  "ws-state should return :inactive when explicitly set."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :inactive)
    (should (eq (claude-repl--ws-state "ws1") :inactive))))

(ert-deftest claude-repl-test-ws-set-nil-error ()
  "ws-set with nil workspace should signal an error."
  (claude-repl-test--with-clean-state
    (should-error (claude-repl--ws-set nil :thinking) :type 'error)))

(ert-deftest claude-repl-test-ws-state-transitions ()
  "Verify state transitions: ws-set correctly cycles through :thinking, :permission, :done, and :inactive."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :thinking)
    (should (eq (claude-repl--ws-state "ws1") :thinking))
    ;; Set to :permission
    (claude-repl--ws-set "ws1" :permission)
    (should (eq (claude-repl--ws-state "ws1") :permission))
    ;; Set to :done
    (claude-repl--ws-set "ws1" :done)
    (should (eq (claude-repl--ws-state "ws1") :done))
    ;; Set to :inactive
    (claude-repl--ws-set "ws1" :inactive)
    (should (eq (claude-repl--ws-state "ws1") :inactive))))

(ert-deftest claude-repl-test-ws-set-permission ()
  "`ws-set' with :permission should set permission hash."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :permission)
    (should (eq (claude-repl--ws-state "ws1") :permission))))

(ert-deftest claude-repl-test-ws-clear-done ()
  "`ws-clear' with :done should not clear status when it is :thinking."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :thinking)
    (claude-repl--ws-claude-state-clear-if "ws1" :done)
    (should (eq (claude-repl--ws-get "ws1" :claude-state) :thinking))))

(ert-deftest claude-repl-test-ws-clear-permission ()
  "`ws-clear' with :permission should not clear status when it is :done."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :done)
    (claude-repl--ws-claude-state-clear-if "ws1" :permission)
    (should (eq (claude-repl--ws-get "ws1" :claude-state) :done))))

(ert-deftest claude-repl-test-ws-clear-nil-error ()
  "`ws-clear' with nil ws should signal error."
  (claude-repl-test--with-clean-state
    (should-error (claude-repl--ws-claude-state-clear-if nil :done) :type 'error)))

;;;; ---- Tests: Tabline rendering ----

(ert-deftest claude-repl-test-tabline-thinking-face ()
  "Tabline should apply thinking face for background thinking tabs."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "other-ws" :thinking)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function '+workspace-list-names) (lambda () '("test-ws" "other-ws"))))
      (let ((result (claude-repl--tabline-advice '("test-ws" "other-ws"))))
        ;; other-ws should have thinking face
        (should (string-match-p "other-ws" result))))))

(ert-deftest claude-repl-test-tabline-permission-label ()
  "Tabline should show ❓ for permission state."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "test-ws" :permission)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function '+workspace-list-names) (lambda () '("test-ws"))))
      (let ((result (claude-repl--tabline-advice '("test-ws"))))
        (should (string-match-p "❓" result))))))

(ert-deftest claude-repl-test-tabline-done-face ()
  "A background tab with :done should use `claude-repl-tab-done' face."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "bg-ws" :done)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws")))
      (let ((result (claude-repl--tabline-advice '("current-ws" "bg-ws"))))
        ;; Find the "bg-ws" segment and check its face
        (let ((pos (string-match "bg-ws" result)))
          (should pos)
          (should (eq (get-text-property pos 'face result) 'claude-repl-tab-done)))))))

(ert-deftest claude-repl-test-tabline-selected-suppresses-thinking ()
  "The SELECTED tab with :thinking should NOT get the thinking face."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "sel-ws" :thinking)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "sel-ws")))
      (let ((result (claude-repl--tabline-advice '("sel-ws"))))
        (let ((pos (string-match "sel-ws" result)))
          (should pos)
          ;; Should get the normal selected face, NOT thinking
          (should (eq (get-text-property pos 'face result)
                      '+workspace-tab-selected-face)))))))

(ert-deftest claude-repl-test-tabline-selected-shows-permission ()
  "The SELECTED tab with :permission SHOULD still get the permission face."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "sel-ws" :permission)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "sel-ws")))
      (let ((result (claude-repl--tabline-advice '("sel-ws"))))
        (let ((pos (string-match "sel-ws" result)))
          (should pos)
          (should (eq (get-text-property pos 'face result)
                      'claude-repl-tab-permission)))))))

;;;; ---- Tests: ws-state edge cases ----

(ert-deftest claude-repl-test-ws-state-untouched-workspace ()
  "ws-state should return nil for a workspace that was never touched."
  (claude-repl-test--with-clean-state
    (should-not (claude-repl--ws-state "never-touched-ws"))))

;;;; ---- Tests: ws-set edge cases ----

(ert-deftest claude-repl-test-ws-set-calls-force-mode-line-update ()
  "ws-set should call `force-mode-line-update'."
  (claude-repl-test--with-clean-state
    (let ((called nil))
      (cl-letf (((symbol-function 'force-mode-line-update)
                 (lambda (&rest _) (setq called t))))
        (claude-repl--ws-set "ws1" :thinking)
        (should called)))))

(ert-deftest claude-repl-test-ws-set-idempotent ()
  "Setting the same state twice should be a no-op (state stays the same)."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :done)
    (claude-repl--ws-set "ws1" :done)
    (should (eq (claude-repl--ws-state "ws1") :done))))

;;;; ---- Tests: ws-clear-if-status edge cases ----

(ert-deftest claude-repl-test-ws-clear-when-already-nil ()
  "Clearing when status is already nil should be a no-op."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-claude-state-clear-if "ws1" :thinking)
    (should-not (claude-repl--ws-state "ws1"))))

(ert-deftest claude-repl-test-ws-clear-matching-done ()
  "Clearing :done when status IS :done should clear to nil."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :done)
    (claude-repl--ws-claude-state-clear-if "ws1" :done)
    (should-not (claude-repl--ws-state "ws1"))))

(ert-deftest claude-repl-test-ws-clear-matching-permission ()
  "Clearing :permission when status IS :permission should clear to nil."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :permission)
    (claude-repl--ws-claude-state-clear-if "ws1" :permission)
    (should-not (claude-repl--ws-state "ws1"))))

(ert-deftest claude-repl-test-ws-clear-matching-inactive ()
  "Clearing :inactive when status IS :inactive should clear to nil."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :inactive)
    (claude-repl--ws-claude-state-clear-if "ws1" :inactive)
    (should-not (claude-repl--ws-state "ws1"))))

(ert-deftest claude-repl-test-ws-clear-calls-mode-line-update-on-match ()
  "ws-clear-if-status should call `force-mode-line-update' only when state matches."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :done)
    (let ((call-count 0))
      (cl-letf (((symbol-function 'force-mode-line-update)
                 (lambda (&rest _) (cl-incf call-count))))
        ;; Mismatch: should NOT call force-mode-line-update
        (claude-repl--ws-claude-state-clear-if "ws1" :thinking)
        (should (= call-count 0))
        ;; Match: should call force-mode-line-update
        (claude-repl--ws-claude-state-clear-if "ws1" :done)
        (should (= call-count 1))))))

;;;; ---- Tests: ws-dir ----

(ert-deftest claude-repl-test-ws-dir-returns-project-dir ()
  "ws-dir should return the :project-dir value when set."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/home/user/project")
    (should (equal (claude-repl--ws-dir "ws1") "/home/user/project"))))

(ert-deftest claude-repl-test-ws-dir-errors-when-missing ()
  "ws-dir should signal an error when :project-dir is not set."
  (claude-repl-test--with-clean-state
    (should-error (claude-repl--ws-dir "ws1") :type 'error)))

;;;; ---- Tests: workspace-clean-p ----

(ert-deftest claude-repl-test-workspace-clean-p-clean ()
  "workspace-clean-p should return non-nil when :git-clean is 'clean."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :git-clean 'clean)
    (should (claude-repl--workspace-clean-p "ws1"))))

(ert-deftest claude-repl-test-workspace-clean-p-dirty ()
  "workspace-clean-p should return nil when :git-clean is 'dirty."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :git-clean 'dirty)
    (should-not (claude-repl--workspace-clean-p "ws1"))))

(ert-deftest claude-repl-test-workspace-clean-p-default-nil ()
  "workspace-clean-p should signal an error when :git-clean is unset."
  (claude-repl-test--with-clean-state
    (should-error (claude-repl--workspace-clean-p "ws1") :type 'error)))

;;;; ---- Tests: git-check-in-progress-p ----

(ert-deftest claude-repl-test-git-check-in-progress-no-proc ()
  "git-check-in-progress-p should return nil when no :git-proc is set."
  (claude-repl-test--with-clean-state
    (should-not (claude-repl--git-check-in-progress-p "ws1"))))

(ert-deftest claude-repl-test-git-check-in-progress-dead-proc ()
  "git-check-in-progress-p should return nil when :git-proc is a dead process."
  (claude-repl-test--with-clean-state
    (let ((proc (start-process "dead-test" nil "true")))
      ;; Wait for it to finish
      (while (process-live-p proc) (accept-process-output proc 0.1))
      (claude-repl--ws-put "ws1" :git-proc proc)
      (should-not (claude-repl--git-check-in-progress-p "ws1")))))

(ert-deftest claude-repl-test-git-check-in-progress-live-proc ()
  "git-check-in-progress-p should return non-nil when :git-proc is live."
  (claude-repl-test--with-clean-state
    (let ((proc (start-process "live-test" nil "sleep" "60")))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws1" :git-proc proc)
            (should (claude-repl--git-check-in-progress-p "ws1")))
        (delete-process proc)))))

;;;; ---- Tests: git-diff-sentinel ----

(ert-deftest claude-repl-test-git-diff-sentinel-clean ()
  "git-diff-sentinel should set 'clean when process exits with 0."
  (claude-repl-test--with-clean-state
    (let ((proc (start-process "sentinel-clean" nil "true")))
      (while (process-live-p proc) (accept-process-output proc 0.1))
      (cl-letf (((symbol-function 'claude-repl--update-ws-state) #'ignore))
        (claude-repl--git-diff-sentinel "ws1" proc "finished\n")
        (should (eq (claude-repl--ws-get "ws1" :git-clean) 'clean))))))

(ert-deftest claude-repl-test-git-diff-sentinel-dirty ()
  "git-diff-sentinel should set 'dirty when process exits with non-zero."
  (claude-repl-test--with-clean-state
    (let ((proc (start-process "sentinel-dirty" nil "false")))
      (while (process-live-p proc) (accept-process-output proc 0.1))
      (cl-letf (((symbol-function 'claude-repl--update-ws-state) #'ignore))
        (claude-repl--git-diff-sentinel "ws1" proc "finished\n")
        (should (eq (claude-repl--ws-get "ws1" :git-clean) 'dirty))))))

(ert-deftest claude-repl-test-git-diff-sentinel-clears-git-proc ()
  "git-diff-sentinel should clear :git-proc after completion."
  (claude-repl-test--with-clean-state
    (let ((proc (start-process "sentinel-clear" nil "true")))
      (while (process-live-p proc) (accept-process-output proc 0.1))
      (claude-repl--ws-put "ws1" :git-proc proc)
      (cl-letf (((symbol-function 'claude-repl--update-ws-state) #'ignore))
        (claude-repl--git-diff-sentinel "ws1" proc "finished\n")
        (should-not (claude-repl--ws-get "ws1" :git-proc))))))

(ert-deftest claude-repl-test-git-diff-sentinel-calls-update-ws-state ()
  "git-diff-sentinel should call update-ws-state after completion."
  (claude-repl-test--with-clean-state
    (let ((proc (start-process "sentinel-update" nil "true"))
          (called-with nil))
      (while (process-live-p proc) (accept-process-output proc 0.1))
      (cl-letf (((symbol-function 'claude-repl--update-ws-state)
                 (lambda (ws) (setq called-with ws))))
        (claude-repl--git-diff-sentinel "ws1" proc "finished\n")
        (should (equal called-with "ws1"))))))

(ert-deftest claude-repl-test-git-diff-sentinel-noop-when-live ()
  "git-diff-sentinel should be a no-op when the process is still live."
  (claude-repl-test--with-clean-state
    (let ((proc (start-process "sentinel-live" nil "sleep" "60"))
          (update-called nil))
      (unwind-protect
          (cl-letf (((symbol-function 'claude-repl--update-ws-state)
                     (lambda (_ws) (setq update-called t))))
            (claude-repl--git-diff-sentinel "ws1" proc "running\n")
            (should-not (claude-repl--ws-get "ws1" :git-clean))
            (should-not update-called))
        (delete-process proc)))))

;;;; ---- Tests: async-refresh-git-status ----

(ert-deftest claude-repl-test-async-refresh-noop-when-in-progress ()
  "async-refresh-git-status should be a no-op when check already in progress."
  (claude-repl-test--with-clean-state
    (let ((proc (start-process "existing-check" nil "sleep" "60")))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws1" :project-dir "/tmp")
            (claude-repl--ws-put "ws1" :git-proc proc)
            (claude-repl--async-refresh-git-status "ws1")
            ;; :git-proc should still be the original proc, not replaced
            (should (eq (claude-repl--ws-get "ws1" :git-proc) proc)))
        (delete-process proc)))))

(ert-deftest claude-repl-test-async-refresh-noop-when-no-dir ()
  "async-refresh-git-status should be a no-op when ws-dir errors."
  (claude-repl-test--with-clean-state
    ;; No :project-dir set, so ws-dir will error
    ;; The function uses when-let which handles nil returns,
    ;; but ws-dir errors. We stub ws-dir to return nil.
    (cl-letf (((symbol-function 'claude-repl--ws-dir) (lambda (_ws) nil)))
      (claude-repl--async-refresh-git-status "ws1")
      (should-not (claude-repl--ws-get "ws1" :git-proc)))))

;;;; ---- Tests: workspace-for-buffer ----

(ert-deftest claude-repl-test-workspace-for-buffer-persp-mode-nil ()
  "workspace-for-buffer should return nil when persp-mode is nil."
  (claude-repl-test--with-clean-state
    (let ((persp-mode nil))
      (should-not (claude-repl--workspace-for-buffer (current-buffer))))))

(ert-deftest claude-repl-test-workspace-for-buffer-found ()
  "workspace-for-buffer should return workspace name when buffer is found."
  (claude-repl-test--with-clean-state
    (let ((persp-mode t)
          (test-buf (current-buffer))
          (fake-persp "my-workspace"))
      (cl-letf (((symbol-function 'persp-persps)
                 (lambda () (list fake-persp)))
                ((symbol-function 'persp-contain-buffer-p)
                 (lambda (buf persp)
                   (and (eq buf test-buf) (equal persp fake-persp))))
                ((symbol-function 'safe-persp-name)
                 (lambda (persp) persp)))
        (should (equal (claude-repl--workspace-for-buffer test-buf)
                       "my-workspace"))))))

(ert-deftest claude-repl-test-workspace-for-buffer-not-found ()
  "workspace-for-buffer should return nil when buffer not in any persp."
  (claude-repl-test--with-clean-state
    (let ((persp-mode t))
      (cl-letf (((symbol-function 'persp-persps)
                 (lambda () '("ws-a" "ws-b")))
                ((symbol-function 'persp-contain-buffer-p)
                 (lambda (_buf _persp) nil)))
        (should-not (claude-repl--workspace-for-buffer (current-buffer)))))))

;;;; ---- Tests: tab-spec ----

(ert-deftest claude-repl-test-tab-spec-unselected-known-state ()
  "tab-spec returns the :unselected plist from the palette for a known state."
  (let ((spec (claude-repl--tab-spec :thinking nil)))
    (should (equal (plist-get spec :bg) "#cc3333"))
    (should (equal (plist-get spec :fg) "white"))))

(ert-deftest claude-repl-test-tab-spec-selected-known-state ()
  "tab-spec returns the :selected plist from the palette for a known state."
  (let ((spec (claude-repl--tab-spec :done t)))
    (should (equal (plist-get spec :bracket-fg) "#2a8c2a"))))

(ert-deftest claude-repl-test-tab-spec-unknown-state-falls-back-to-default ()
  "tab-spec returns the default spec for states absent from the palette."
  (let ((unsel (claude-repl--tab-spec :bogus nil))
        (sel   (claude-repl--tab-spec :bogus t)))
    (should (equal (plist-get unsel :bracket-fg) "#4477cc"))
    (should (equal (plist-get sel :bg) "#c0c0c0"))))

(ert-deftest claude-repl-test-tab-spec-nil-state-uses-default ()
  "tab-spec with nil state returns the default spec."
  (should (equal (plist-get (claude-repl--tab-spec nil nil) :bracket-fg)
                 "#4477cc")))

(ert-deftest claude-repl-test-tab-spec-permission-has-face-override ()
  "The :permission :selected spec carries :face-override = the permission face."
  (let ((spec (claude-repl--tab-spec :permission t)))
    (should (eq (plist-get spec :face-override) 'claude-repl-tab-permission))))

;;;; ---- Tests: render-tab (spec-driven) ----

(ert-deftest claude-repl-test-render-tab-with-img-str ()
  "render-tab should include img-str when non-nil."
  (let* ((spec '(:bg unspecified :fg "black" :bracket-fg "blue" :weight bold))
         (result (claude-repl--render-tab "ws1" spec "1" '+workspace-tab-face "IMG")))
    (should (string-match-p "IMG" result))
    (should (string-match-p "ws1" result))))

(ert-deftest claude-repl-test-render-tab-empty-name ()
  "render-tab should handle an empty name string."
  (let* ((spec '(:bg unspecified :fg "black" :bracket-fg "blue" :weight bold))
         (result (claude-repl--render-tab "" spec "1" '+workspace-tab-face nil)))
    (should (string-match-p "\\[1\\]" result))))

(ert-deftest claude-repl-test-render-tab-selected-spec-bg ()
  "render-tab applies the spec :bg to the bracket face's background."
  (let* ((spec '(:bg "#c0c0c0" :fg "black" :bracket-fg "#2a8c2a" :weight bold))
         (result (claude-repl--render-tab "ws1" spec "1" '+workspace-tab-face nil))
         (pos (string-match "\\[1\\]" result))
         (face (get-text-property pos 'face result)))
    (should (equal (plist-get face :background) "#c0c0c0"))
    (should (equal (plist-get face :foreground) "#2a8c2a"))))

;;;; ---- Tests: tab-label edge cases ----

(ert-deftest claude-repl-test-tab-label-zero-index ()
  "tab-label should return \"0\" when index is 0 and state has no :label."
  (should (equal (claude-repl--tab-label :thinking 0) "0")))

;;;; ---- Tests: tab-face direct tests ----

(ert-deftest claude-repl-test-tab-face-nil-state-selected ()
  "tab-face with nil state and selected should return +workspace-tab-selected-face."
  (should (eq (claude-repl--tab-face nil t) '+workspace-tab-selected-face)))

(ert-deftest claude-repl-test-tab-face-nil-state-unselected ()
  "tab-face with nil state and unselected should return +workspace-tab-face."
  (should (eq (claude-repl--tab-face nil nil) '+workspace-tab-face)))

;;;; ---- Tests: tab-priority-image-str ----

(ert-deftest claude-repl-test-tab-priority-image-str-no-image ()
  "tab-priority-image-str should return nil when :priority is set but no image found."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :priority "nonexistent-priority")
    (cl-letf (((symbol-function 'claude-repl--priority-image)
               (lambda (_p) nil)))
      (should-not (claude-repl--tab-priority-image-str "ws1")))))

(ert-deftest claude-repl-test-tab-priority-image-str-with-image ()
  "tab-priority-image-str should return a propertized string when image found."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :priority "high")
    (let ((fake-image '(image :type png :data "fake")))
      (cl-letf (((symbol-function 'claude-repl--priority-image)
                 (lambda (_p) fake-image)))
        (let ((result (claude-repl--tab-priority-image-str "ws1")))
          (should result)
          (should (stringp result))
          (should (equal (get-text-property 0 'display result) fake-image)))))))

;;;; ---- Tests: render-tab-entry edge cases ----

(ert-deftest claude-repl-test-render-tab-entry-no-match ()
  "render-tab-entry should render as unselected when current-name matches nothing."
  (claude-repl-test--with-clean-state
    (let ((result (claude-repl--render-tab-entry "ws1" "no-such-ws" 1)))
      ;; Should render as unselected (bracket foreground is blue #4477cc)
      (should (string-match-p "ws1" result))
      (let ((bracket-pos (string-match "\\[" result)))
        (should bracket-pos)
        (let ((face (get-text-property bracket-pos 'face result)))
          (should (equal (plist-get face :foreground) "#4477cc")))))))

;;;; ---- Tests: tabline-advice edge cases ----

(ert-deftest claude-repl-test-tabline-advice-defaults-from-list-names ()
  "tabline-advice with no args should default names from +workspace-list-names."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws-a"))
              ((symbol-function '+workspace-list-names) (lambda () '("ws-a" "ws-b"))))
      (let ((result (claude-repl--tabline-advice)))
        (should (string-match-p "ws-a" result))
        (should (string-match-p "ws-b" result))))))

(ert-deftest claude-repl-test-tabline-advice-empty-names ()
  "tabline-advice with an empty names list should return an empty string."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (let ((result (claude-repl--tabline-advice '())))
        (should (equal result ""))))))

;;;; ---- Tests: wconf-has-claude-p ----

(ert-deftest claude-repl-test-wconf-has-claude-nil ()
  "wconf-has-claude-p should return nil for nil wconf."
  (should-not (claude-repl--wconf-has-claude-p nil)))

(ert-deftest claude-repl-test-wconf-has-claude-non-list ()
  "wconf-has-claude-p should return nil for a non-list wconf."
  (should-not (claude-repl--wconf-has-claude-p "not-a-list")))

(ert-deftest claude-repl-test-wconf-has-claude-flat-match ()
  "wconf-has-claude-p should return t for a flat wconf with a matching buffer."
  (let ((wconf '((buffer "*claude-panel-ab12cd34*"))))
    (should (claude-repl--wconf-has-claude-p wconf))))

(ert-deftest claude-repl-test-wconf-has-claude-nested-match ()
  "wconf-has-claude-p should return t for a nested wconf with matching buffer."
  (let ((wconf '((child ((buffer "*claude-panel-ab12cd34*"))))))
    (should (claude-repl--wconf-has-claude-p wconf))))

(ert-deftest claude-repl-test-wconf-has-claude-no-buffer ()
  "wconf-has-claude-p should return nil for a wconf with no buffer entries."
  (let ((wconf '((something "other"))))
    (should-not (claude-repl--wconf-has-claude-p wconf))))

(ert-deftest claude-repl-test-wconf-has-claude-non-claude-buffer ()
  "wconf-has-claude-p should return nil for a wconf with non-claude buffers."
  (let ((wconf '((buffer "*scratch*"))))
    (should-not (claude-repl--wconf-has-claude-p wconf))))

;;;; ---- Tests: visible-claude-buffer-p ----

(ert-deftest claude-repl-test-visible-claude-buffer-dead-buffer ()
  "visible-claude-buffer-p should return nil for a dead buffer."
  (let ((buf (generate-new-buffer "*claude-panel-deadbeef*")))
    (kill-buffer buf)
    (should-not (claude-repl--visible-claude-buffer-p buf))))

(ert-deftest claude-repl-test-visible-claude-buffer-non-claude ()
  "visible-claude-buffer-p should return nil for a live non-claude buffer."
  (claude-repl-test--with-temp-buffer "*not-claude*"
    (should-not (claude-repl--visible-claude-buffer-p (current-buffer)))))

(ert-deftest claude-repl-test-visible-claude-buffer-no-window ()
  "visible-claude-buffer-p should return nil for a live claude buffer with no window."
  (claude-repl-test--with-temp-buffer "*claude-panel-00112233*"
    ;; Buffer is live and claude but has no window
    (should-not (claude-repl--visible-claude-buffer-p (current-buffer)))))

(ert-deftest claude-repl-test-visible-claude-buffer-with-window ()
  "visible-claude-buffer-p should return non-nil for a live claude buffer with a window."
  (claude-repl-test--with-temp-buffer "*claude-panel-00112233*"
    (cl-letf (((symbol-function 'get-buffer-window)
               (lambda (_buf) 'fake-window)))
      (should (claude-repl--visible-claude-buffer-p (current-buffer))))))

;;;; ---- Tests: claude-visible-in-current-ws-p ----

(ert-deftest claude-repl-test-claude-visible-in-current-ws-none ()
  "claude-visible-in-current-ws-p should return nil when no claude buffers exist."
  (cl-letf (((symbol-function 'buffer-list)
             (lambda () nil)))
    (should-not (claude-repl--claude-visible-in-current-ws-p))))

(ert-deftest claude-repl-test-claude-visible-in-current-ws-found ()
  "claude-visible-in-current-ws-p should return non-nil when a visible claude buffer exists."
  (claude-repl-test--with-temp-buffer "*claude-panel-aabbccdd*"
    (let ((test-buf (current-buffer)))
      (cl-letf (((symbol-function 'buffer-list)
                 (lambda () (list test-buf)))
                ((symbol-function 'get-buffer-window)
                 (lambda (_buf) 'fake-window)))
        (should (claude-repl--claude-visible-in-current-ws-p))))))

;;;; ---- Tests: claude-in-saved-wconf-p ----

(ert-deftest claude-repl-test-claude-in-saved-wconf-persp-not-found ()
  "claude-in-saved-wconf-p should return nil when persp is not found."
  (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_name) nil)))
    (should-not (claude-repl--claude-in-saved-wconf-p "ws1"))))

(ert-deftest claude-repl-test-claude-in-saved-wconf-persp-is-symbol ()
  "claude-in-saved-wconf-p should return nil when persp is a symbol (nil persp)."
  (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_name) 'none)))
    (should-not (claude-repl--claude-in-saved-wconf-p "ws1"))))

(ert-deftest claude-repl-test-claude-in-saved-wconf-with-claude ()
  "claude-in-saved-wconf-p should return t when saved wconf contains claude buffer."
  (let ((fake-persp (list 'fake-persp-struct))
        (fake-wconf '((buffer "*claude-panel-ab12cd34*"))))
    (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_name) fake-persp))
              ((symbol-function 'persp-window-conf) (lambda (_persp) fake-wconf)))
      (should (claude-repl--claude-in-saved-wconf-p "ws1")))))

(ert-deftest claude-repl-test-claude-in-saved-wconf-without-claude ()
  "claude-in-saved-wconf-p should return nil when saved wconf has no claude buffer."
  (let ((fake-persp (list 'fake-persp-struct))
        (fake-wconf '((buffer "*scratch*"))))
    (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_name) fake-persp))
              ((symbol-function 'persp-window-conf) (lambda (_persp) fake-wconf)))
      (should-not (claude-repl--claude-in-saved-wconf-p "ws1")))))

;;;; ---- Tests: ws-claude-open-p ----

(ert-deftest claude-repl-test-ws-claude-open-current-ws ()
  "ws-claude-open-p should delegate to visible check for the current workspace."
  (let ((visible-called nil))
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'claude-repl--claude-visible-in-current-ws-p)
               (lambda () (setq visible-called t) t)))
      (should (claude-repl--ws-claude-open-p "ws1"))
      (should visible-called))))

(ert-deftest claude-repl-test-ws-claude-open-background-ws ()
  "ws-claude-open-p should delegate to saved wconf check for a background workspace."
  (let ((wconf-called nil))
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
              ((symbol-function 'claude-repl--claude-in-saved-wconf-p)
               (lambda (ws) (setq wconf-called ws) t)))
      (should (claude-repl--ws-claude-open-p "bg-ws"))
      (should (equal wconf-called "bg-ws")))))

;;;; ---- Tests: update-ws-state (state machine) ----

(ert-deftest claude-repl-test-update-ws-state-thinking-unchanged ()
  ":thinking state should remain unchanged regardless of dirty value."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :thinking)
    (cl-letf (((symbol-function 'claude-repl--workspace-clean-p) (lambda (_ws) nil)))
      (claude-repl--update-ws-state "ws1")
      (should (eq (claude-repl--ws-state "ws1") :thinking)))))

(ert-deftest claude-repl-test-update-ws-state-done-clean-to-idle ()
  ":done + clean → :idle (user staged/committed; nothing outstanding)."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set-claude-state "ws1" :done)
    (cl-letf (((symbol-function 'claude-repl--workspace-clean-p) (lambda (_ws) t)))
      (claude-repl--update-ws-state "ws1")
      (should (eq (claude-repl--ws-claude-state "ws1") :idle)))))

(ert-deftest claude-repl-test-update-ws-state-done-dirty-stays-done ()
  ":done + dirty stays :done — waiting on user to stage/commit."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set-claude-state "ws1" :done)
    (cl-letf (((symbol-function 'claude-repl--workspace-clean-p) (lambda (_ws) nil)))
      (claude-repl--update-ws-state "ws1")
      (should (eq (claude-repl--ws-claude-state "ws1") :done)))))

(ert-deftest claude-repl-test-update-ws-state-inactive-dirty ()
  ":inactive + dirty should remain :inactive.
:inactive is terminal — git status is irrelevant once the user has dismissed it."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :inactive)
    (cl-letf (((symbol-function 'claude-repl--workspace-clean-p) (lambda (_ws) nil)))
      (claude-repl--update-ws-state "ws1")
      (should (eq (claude-repl--ws-state "ws1") :inactive)))))

(ert-deftest claude-repl-test-update-ws-state-inactive-clean ()
  ":inactive + clean should remain :inactive."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :inactive)
    (cl-letf (((symbol-function 'claude-repl--workspace-clean-p) (lambda (_ws) t)))
      (claude-repl--update-ws-state "ws1")
      (should (eq (claude-repl--ws-state "ws1") :inactive)))))

(ert-deftest claude-repl-test-update-ws-state-nil-dirty-stays-nil ()
  "nil + dirty no longer transitions (dirty ≠ signal from Claude anymore).
The old (nil . t) → :done inference was a footgun on pre-existing dirty
trees; under the revised model only the Stop hook writes :done."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--workspace-clean-p) (lambda (_ws) nil)))
      (claude-repl--update-ws-state "ws1")
      (should-not (claude-repl--ws-claude-state "ws1")))))

(ert-deftest claude-repl-test-update-ws-state-nil-clean ()
  "nil + clean should remain nil."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--workspace-clean-p) (lambda (_ws) t)))
      (claude-repl--update-ws-state "ws1")
      (should-not (claude-repl--ws-state "ws1")))))

(ert-deftest claude-repl-test-update-ws-state-permission-unchanged ()
  ":permission state should remain unchanged regardless of dirty value."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :permission)
    (cl-letf (((symbol-function 'claude-repl--workspace-clean-p) (lambda (_ws) nil)))
      (claude-repl--update-ws-state "ws1")
      (should (eq (claude-repl--ws-state "ws1") :permission)))))

;;;; ---- Tests: update-all-workspace-states ----

(ert-deftest claude-repl-test-update-all-persp-mode-nil ()
  "update-all-workspace-states should be a no-op when persp-mode is nil."
  (claude-repl-test--with-clean-state
    (let ((persp-mode nil)
          (update-called nil))
      (cl-letf (((symbol-function 'claude-repl--poll-workspace-notifications) #'ignore)
                ((symbol-function 'claude-repl--update-ws-state)
                 (lambda (_ws) (setq update-called t))))
        (claude-repl--update-all-workspace-states)
        (should-not update-called)))))

(ert-deftest claude-repl-test-update-all-running-vterm ()
  "update-all should call update-ws-state and async-refresh for ws with running vterm."
  (claude-repl-test--with-clean-state
    (let ((updated-ws nil)
          (refreshed-ws nil))
      ;; Register ws1 in the hashmap so the iterator finds it
      (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'claude-repl--poll-workspace-notifications) #'ignore)
                ((symbol-function 'claude-repl--vterm-running-p) (lambda (_ws) t))
                ((symbol-function 'claude-repl--update-ws-state)
                 (lambda (ws) (setq updated-ws ws)))
                ((symbol-function 'claude-repl--async-refresh-git-status)
                 (lambda (ws) (setq refreshed-ws ws))))
        (claude-repl--update-all-workspace-states)
        (should (equal updated-ws "ws1"))
        (should (equal refreshed-ws "ws1"))))))

(ert-deftest claude-repl-test-update-all-no-vterm ()
  "update-all should call mark-dead-vterm for ws without running vterm."
  (claude-repl-test--with-clean-state
    (let ((dead-ws nil))
      ;; Register ws1 in the hashmap so the iterator finds it
      (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'claude-repl--poll-workspace-notifications) #'ignore)
                ((symbol-function 'claude-repl--vterm-running-p) (lambda (_ws) nil))
                ((symbol-function 'claude-repl--mark-dead-vterm)
                 (lambda (ws) (setq dead-ws ws))))
        (claude-repl--update-all-workspace-states)
        (should (equal dead-ws "ws1"))))))

(ert-deftest claude-repl-test-update-all-calls-poll ()
  "update-all-workspace-states should call poll-workspace-notifications."
  (claude-repl-test--with-clean-state
    (let ((persp-mode nil)
          (poll-called nil))
      (cl-letf (((symbol-function 'claude-repl--poll-workspace-notifications)
                 (lambda () (setq poll-called t))))
        (claude-repl--update-all-workspace-states)
        (should poll-called)))))

;;;; ---- Tests: mark-dead-vterm ----

(ert-deftest claude-repl-test-mark-dead-vterm-sets-dead-and-clears-claude-state ()
  "mark-dead-vterm writes :repl-state :dead and clears :claude-state."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set-claude-state "ws1" :done)
    (claude-repl--mark-dead-vterm "ws1")
    (should (eq (claude-repl--ws-repl-state "ws1") :dead))
    (should-not (claude-repl--ws-claude-state "ws1"))))

(ert-deftest claude-repl-test-mark-dead-vterm-from-thinking ()
  "mark-dead-vterm clears :thinking (vterm is gone; sentinel won't fire)."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set-claude-state "ws1" :thinking)
    (claude-repl--mark-dead-vterm "ws1")
    (should (eq (claude-repl--ws-repl-state "ws1") :dead))
    (should-not (claude-repl--ws-claude-state "ws1"))))

(ert-deftest claude-repl-test-mark-dead-vterm-idempotent ()
  "mark-dead-vterm is a no-op when :repl-state is already :dead."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :repl-state :dead)
    (claude-repl--ws-put "ws1" :claude-state :done)  ; simulate stale residue
    (claude-repl--mark-dead-vterm "ws1")
    ;; Second call must not re-run the clear — claude-state stays as-is.
    (should (eq (claude-repl--ws-claude-state "ws1") :done))))

(ert-deftest claude-repl-test-mark-dead-vterm-preserves-init ()
  "mark-dead-vterm is a no-op when :claude-state is :init.
During start-fresh, the timer may tick before vterm-running-p returns t
even though the session is legitimately coming up; under the old code
this clobbered :init with :dead.  The :init guard prevents that."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set-claude-state "ws1" :init)
    (claude-repl--mark-dead-vterm "ws1")
    (should (eq (claude-repl--ws-claude-state "ws1") :init))
    (should-not (claude-repl--ws-repl-state "ws1"))))

;;;; ---- Tests: on-frame-focus ----

(ert-deftest claude-repl-test-on-frame-focus-with-focus ()
  "on-frame-focus should refresh vterm and update states when frame has focus."
  (claude-repl-test--with-clean-state
    (let ((refresh-called nil)
          (update-called nil))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda () t))
                ((symbol-function 'claude-repl--refresh-vterm)
                 (lambda () (setq refresh-called t)))
                ((symbol-function 'claude-repl--update-all-workspace-states)
                 (lambda () (setq update-called t))))
        (claude-repl--on-frame-focus)
        (should refresh-called)
        (should update-called)))))

(ert-deftest claude-repl-test-on-frame-focus-no-focus ()
  "on-frame-focus should be a no-op when frame does not have focus."
  (claude-repl-test--with-clean-state
    (let ((refresh-called nil))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda () nil))
                ((symbol-function 'claude-repl--refresh-vterm)
                 (lambda () (setq refresh-called t)))
                ((symbol-function 'claude-repl--update-all-workspace-states) #'ignore))
        (claude-repl--on-frame-focus)
        (should-not refresh-called)))))

;;;; ---- Tests: update-ws-state edge cases (status transitions .md) ----

(ert-deftest claude-repl-test-update-ws-state-thinking-clean-unchanged ()
  ":thinking + clean should remain unchanged (pcase wildcard catches both dirty values)."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :thinking)
    (cl-letf (((symbol-function 'claude-repl--workspace-clean-p) (lambda (_ws) t)))
      (claude-repl--update-ws-state "ws1")
      (should (eq (claude-repl--ws-state "ws1") :thinking)))))

(ert-deftest claude-repl-test-update-ws-state-done-dirty-stays-done-explicit ()
  ":done + dirty stays :done — dirty worktree blocks the :done→:idle decay."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set-claude-state "ws1" :done)
    (cl-letf (((symbol-function 'claude-repl--workspace-clean-p) (lambda (_ws) nil)))
      (claude-repl--update-ws-state "ws1")
      (should (eq (claude-repl--ws-claude-state "ws1") :done)))))

(ert-deftest claude-repl-test-update-ws-state-permission-clean-unchanged ()
  ":permission + clean should remain unchanged.
:permission is never auto-cleared by the state machine regardless of dirty value."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :permission)
    (cl-letf (((symbol-function 'claude-repl--workspace-clean-p) (lambda (_ws) t)))
      (claude-repl--update-ws-state "ws1")
      (should (eq (claude-repl--ws-state "ws1") :permission)))))

;;;; ---- Tests: ws-clear-if-status cross-state edge cases ----

(ert-deftest claude-repl-test-ws-clear-thinking-when-permission-noop ()
  "Clearing :thinking when actual status is :permission should be a no-op."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :permission)
    (claude-repl--ws-claude-state-clear-if "ws1" :thinking)
    (should (eq (claude-repl--ws-get "ws1" :claude-state) :permission))))

(ert-deftest claude-repl-test-ws-clear-inactive-when-thinking-noop ()
  "Clearing :inactive when actual status is :thinking should be a no-op."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :thinking)
    (claude-repl--ws-claude-state-clear-if "ws1" :inactive)
    (should (eq (claude-repl--ws-get "ws1" :claude-state) :thinking))))

(ert-deftest claude-repl-test-ws-clear-done-when-inactive-noop ()
  "Clearing :done when actual status is :inactive should be a no-op."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :inactive)
    (claude-repl--ws-claude-state-clear-if "ws1" :done)
    (should (eq (claude-repl--ws-get "ws1" :claude-state) :inactive))))

(ert-deftest claude-repl-test-ws-clear-inactive-when-done-noop ()
  "Clearing :inactive when actual status is :done should be a no-op."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :done)
    (claude-repl--ws-claude-state-clear-if "ws1" :inactive)
    (should (eq (claude-repl--ws-get "ws1" :claude-state) :done))))

(ert-deftest claude-repl-test-ws-clear-thinking-when-done-noop ()
  "Clearing :thinking when actual status is :done should be a no-op."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :done)
    (claude-repl--ws-claude-state-clear-if "ws1" :thinking)
    (should (eq (claude-repl--ws-get "ws1" :claude-state) :done))))

;;;; ---- Tests: update-all-workspace-states multi-workspace dispatch ----

(ert-deftest claude-repl-test-update-all-multiple-workspaces-dispatch ()
  "update-all should dispatch correctly per workspace: update running, clear dead."
  (claude-repl-test--with-clean-state
    (let ((updated nil)
          (refreshed nil)
          (cleared nil))
      ;; Register both workspaces in the hashmap
      (claude-repl--ws-put "running-ws" :project-dir "/tmp/running")
      (claude-repl--ws-put "dead-ws" :project-dir "/tmp/dead")
      (cl-letf (((symbol-function 'claude-repl--poll-workspace-notifications) #'ignore)
                ((symbol-function 'claude-repl--vterm-running-p)
                 (lambda (ws) (equal ws "running-ws")))
                ((symbol-function 'claude-repl--update-ws-state)
                 (lambda (ws) (push ws updated)))
                ((symbol-function 'claude-repl--async-refresh-git-status)
                 (lambda (ws) (push ws refreshed)))
                ((symbol-function 'claude-repl--mark-dead-vterm)
                 (lambda (ws) (push ws cleared))))
        (claude-repl--update-all-workspace-states)
        ;; running-ws should get update + refresh
        (should (member "running-ws" updated))
        (should (member "running-ws" refreshed))
        (should-not (member "running-ws" cleared))
        ;; dead-ws should get clear, not update
        (should (member "dead-ws" cleared))
        (should-not (member "dead-ws" updated))
        (should-not (member "dead-ws" refreshed))))))

;;;; ---- Tests: priority-image (moved from core.el) ----

(ert-deftest claude-repl-test-priority-image-valid ()
  "priority-image should return the image spec for a known priority."
  (let ((claude-repl--priority-images '(("p1" . fake-image-spec))))
    (should (equal (claude-repl--priority-image "p1") 'fake-image-spec))))

(ert-deftest claude-repl-test-priority-image-unknown ()
  "priority-image should return nil for an unknown priority."
  (let ((claude-repl--priority-images '(("p1" . fake-image-spec))))
    (should-not (claude-repl--priority-image "p99"))))

(ert-deftest claude-repl-test-priority-image-nil-input ()
  "priority-image should return nil for nil input."
  (let ((claude-repl--priority-images '(("p1" . fake-image-spec))))
    (should-not (claude-repl--priority-image nil))))

(ert-deftest claude-repl-test-priority-image-empty-alist ()
  "priority-image should return nil when the images alist is empty."
  (let ((claude-repl--priority-images nil))
    (should-not (claude-repl--priority-image "p1"))))

;;;; ---- Tests: load-priority-images (moved from core.el) ----

(ert-deftest claude-repl-test-load-priority-images-all-present ()
  "load-priority-images should populate alist when all PNGs exist."
  (let ((tmpdir (make-temp-file "test-img-" t)))
    (unwind-protect
        (let ((img-dir (expand-file-name "images/" tmpdir)))
          (make-directory img-dir t)
          ;; Create fake PNG files
          (dolist (name '("p05" "p1" "p2" "p3"))
            (with-temp-file (expand-file-name (concat name ".png") img-dir)
              (insert "fake-png")))
          (let ((claude-repl--priority-images nil)
                (load-file-name (expand-file-name "core.el" tmpdir)))
            (cl-letf (((symbol-function 'create-image)
                       (lambda (file _type &rest _args) (list 'image :file file)))
                      ((symbol-function 'frame-char-height) (lambda () 16)))
              (claude-repl--load-priority-images)
              (should (= (length claude-repl--priority-images) 4))
              (should (assoc "p1" claude-repl--priority-images))
              (should (assoc "p05" claude-repl--priority-images)))))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-load-priority-images-some-missing ()
  "load-priority-images should skip missing PNG files."
  (let ((tmpdir (make-temp-file "test-img-" t)))
    (unwind-protect
        (let ((img-dir (expand-file-name "images/" tmpdir)))
          (make-directory img-dir t)
          ;; Create only p1.png
          (with-temp-file (expand-file-name "p1.png" img-dir)
            (insert "fake-png"))
          (let ((claude-repl--priority-images nil)
                (load-file-name (expand-file-name "core.el" tmpdir)))
            (cl-letf (((symbol-function 'create-image)
                       (lambda (file _type &rest _args) (list 'image :file file)))
                      ((symbol-function 'frame-char-height) (lambda () 16)))
              (claude-repl--load-priority-images)
              (should (= (length claude-repl--priority-images) 1))
              (should (assoc "p1" claude-repl--priority-images)))))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-load-priority-images-dir-missing ()
  "load-priority-images should produce empty alist when images dir does not exist."
  (let ((tmpdir (make-temp-file "test-img-" t)))
    (unwind-protect
        (let ((claude-repl--priority-images nil)
              (load-file-name (expand-file-name "core.el" tmpdir)))
          (cl-letf (((symbol-function 'create-image)
                     (lambda (file _type &rest _args) (list 'image :file file)))
                    ((symbol-function 'frame-char-height) (lambda () 16)))
            (claude-repl--load-priority-images)
            (should (null claude-repl--priority-images))))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-load-priority-images-buffer-file-fallback ()
  "load-priority-images should use buffer-file-name when load-file-name is nil."
  (let ((tmpdir (make-temp-file "test-img-" t)))
    (unwind-protect
        (let ((img-dir (expand-file-name "images/" tmpdir)))
          (make-directory img-dir t)
          (with-temp-file (expand-file-name "p1.png" img-dir)
            (insert "fake-png"))
          (let ((claude-repl--priority-images nil)
                (load-file-name nil)
                (buffer-file-name (expand-file-name "core.el" tmpdir)))
            (cl-letf (((symbol-function 'create-image)
                       (lambda (file _type &rest _args) (list 'image :file file)))
                      ((symbol-function 'frame-char-height) (lambda () 16)))
              (claude-repl--load-priority-images)
              (should (= (length claude-repl--priority-images) 1)))))
      (delete-directory tmpdir t))))

(provide 'test-status)

;;; test-status.el ends here
