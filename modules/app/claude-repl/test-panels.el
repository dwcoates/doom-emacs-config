;;; test-panels.el --- ERT tests for panels.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for panel/window management and entry point functions.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-panels.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: Panel visibility predicates ----

(ert-deftest claude-repl-test-panels-input-visible-p-with-visible-buffer ()
  "input-visible-p returns non-nil when the input buffer is in a window."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*test-input*"
      (claude-repl--ws-put "test-ws" :input-buffer (current-buffer))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'get-buffer-window)
                 (lambda (_buf &rest _) (selected-window))))
        ;; Mock get-buffer-window: batch mode has no real display
        (should (claude-repl--input-visible-p))))))

(ert-deftest claude-repl-test-panels-input-visible-p-no-buffer ()
  "input-visible-p returns nil when no input buffer is set."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
      (should-not (claude-repl--input-visible-p)))))

(ert-deftest claude-repl-test-panels-input-visible-p-dead-buffer ()
  "input-visible-p returns nil when the input buffer has been killed."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*test-dead-input*")))
      (claude-repl--ws-put "test-ws" :input-buffer buf)
      (kill-buffer buf)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
        (should-not (claude-repl--input-visible-p))))))

(ert-deftest claude-repl-test-panels-vterm-visible-p-no-buffer ()
  "vterm-visible-p returns nil when no vterm buffer is set."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
      (should-not (claude-repl--vterm-visible-p)))))

(ert-deftest claude-repl-test-panels-panels-visible-p-both-nil ()
  "panels-visible-p returns nil when neither panel exists."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
      (should-not (claude-repl--panels-visible-p)))))

;;;; ---- Tests: Safe buffer name ----

(ert-deftest claude-repl-test-panels-safe-buffer-name-nil ()
  "safe-buffer-name returns nil for nil input."
  (should-not (claude-repl--safe-buffer-name nil)))

(ert-deftest claude-repl-test-panels-safe-buffer-name-live-buffer ()
  "safe-buffer-name returns the name for a live buffer."
  (claude-repl-test--with-temp-buffer "*safe-name-test*"
    (should (equal (claude-repl--safe-buffer-name (current-buffer))
                   "*safe-name-test*"))))

;;;; ---- Tests: Extract panel hex ----

(ert-deftest claude-repl-test-panels-extract-hex-from-vterm ()
  "extract-panel-hex returns hex from a vterm buffer name."
  (should (equal (claude-repl--extract-panel-hex "*claude-abcd1234*")
                 "abcd1234")))

(ert-deftest claude-repl-test-panels-extract-hex-from-input ()
  "extract-panel-hex returns hex from an input buffer name."
  (should (equal (claude-repl--extract-panel-hex "*claude-input-abcd1234*")
                 "abcd1234")))

(ert-deftest claude-repl-test-panels-extract-hex-non-claude ()
  "extract-panel-hex returns nil for non-Claude buffer names."
  (should-not (claude-repl--extract-panel-hex "*scratch*"))
  (should-not (claude-repl--extract-panel-hex "*Messages*"))
  (should-not (claude-repl--extract-panel-hex "config.el")))

;;;; ---- Tests: Partner buffer name ----

(ert-deftest claude-repl-test-panels-partner-of-vterm ()
  "partner-buffer-name of a vterm buffer is the input buffer."
  (should (equal (claude-repl--partner-buffer-name "*claude-abcd1234*" "abcd1234")
                 "*claude-input-abcd1234*")))

(ert-deftest claude-repl-test-panels-partner-of-input ()
  "partner-buffer-name of an input buffer is the vterm buffer."
  (should (equal (claude-repl--partner-buffer-name "*claude-input-abcd1234*" "abcd1234")
                 "*claude-abcd1234*")))

;;;; ---- Tests: Orphaned panel detection (migrated) ----

(ert-deftest claude-repl-test-panels-orphaned-vterm-p ()
  "A vterm buffer whose input partner is not visible is orphaned."
  (claude-repl-test--with-clean-state
    ;; Mock: not one-window-p, no partner window visible
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window) (lambda (_buf) nil))
              ((symbol-function 'get-buffer) (lambda (_name) nil)))
      ;; Vterm with no visible input partner is orphaned
      (should (claude-repl--orphaned-panel-p "*claude-abcd1234*"))
      ;; Non-Claude buffers are never orphaned
      (should-not (claude-repl--orphaned-panel-p "*some-other*")))))

(ert-deftest claude-repl-test-panels-orphaned-input-p ()
  "An input buffer whose vterm partner is not visible is orphaned (no loading placeholder)."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window) (lambda (_buf) nil))
              ((symbol-function 'get-buffer) (lambda (_name) nil)))
      ;; Input with no visible vterm partner and no loading placeholder is orphaned
      (should (claude-repl--orphaned-panel-p "*claude-input-abcd1234*"))
      ;; Non-Claude buffers are never orphaned
      (should-not (claude-repl--orphaned-panel-p "*scratch*")))))

(ert-deftest claude-repl-test-panels-orphaned-vterm-one-window ()
  "When one-window-p returns t, no panel is considered orphaned."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () t)))
      (should-not (claude-repl--orphaned-panel-p "*claude-abcd1234*")))))

(ert-deftest claude-repl-test-panels-orphaned-input-with-loading ()
  "When loading placeholder buffer exists, input panel is not orphaned."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window) (lambda (_buf) nil))
              ((symbol-function 'get-buffer) (lambda (name)
                                               (when (equal name " *claude-loading*")
                                                 'fake-buffer))))
      (should-not (claude-repl--orphaned-panel-p "*claude-input-abcd1234*")))))

(ert-deftest claude-repl-test-panels-orphaned-vterm-partner-visible ()
  "A vterm buffer whose input partner IS visible is not orphaned."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window)
               (lambda (buf)
                 ;; The input partner window is visible
                 (when (equal buf "*claude-input-abcd1234*")
                   'fake-window))))
      (should-not (claude-repl--orphaned-panel-p "*claude-abcd1234*")))))

(ert-deftest claude-repl-test-panels-orphaned-input-partner-visible ()
  "An input buffer whose vterm partner IS visible is not orphaned."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window)
               (lambda (buf)
                 ;; The vterm partner window is visible
                 (when (equal buf "*claude-abcd1234*")
                   'fake-window))))
      (should-not (claude-repl--orphaned-panel-p "*claude-input-abcd1234*")))))

;;;; ---- Tests: Docstring accuracy (migrated) ----

(ert-deftest claude-repl-test-panels-show-panels-docstring ()
  "show-panels docstring should mention 60% and 15%."
  (let ((doc (documentation 'claude-repl--show-panels)))
    (should (string-match-p "60%" doc))
    (should (string-match-p "15%" doc))))

;;;; ---- Tests: mark-viewed ----

(ert-deftest claude-repl-test-panels-mark-viewed-inactive-to-done ()
  "mark-viewed on :inactive workspace transitions to :done WITHOUT setting :viewed.
This prevents the oscillation where update-ws-state immediately sees
:done+:viewed+panels-closed and transitions back to :inactive."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "test-ws" :inactive)
    (claude-repl--mark-viewed "test-ws")
    (should (eq (claude-repl--ws-get "test-ws" :status) :done))
    (should-not (claude-repl--ws-get "test-ws" :viewed))))

(ert-deftest claude-repl-test-panels-mark-viewed-done-sets-viewed ()
  "mark-viewed on :done workspace sets :viewed without changing status."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "test-ws" :done)
    (claude-repl--mark-viewed "test-ws")
    (should (eq (claude-repl--ws-get "test-ws" :status) :done))
    (should (claude-repl--ws-get "test-ws" :viewed))))

(ert-deftest claude-repl-test-panels-mark-viewed-thinking-noop ()
  "mark-viewed on :thinking workspace does nothing."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "test-ws" :thinking)
    (claude-repl--mark-viewed "test-ws")
    (should (eq (claude-repl--ws-get "test-ws" :status) :thinking))
    (should-not (claude-repl--ws-get "test-ws" :viewed))))

(ert-deftest claude-repl-test-panels-mark-viewed-nil-status-noop ()
  "mark-viewed on a workspace with nil status does nothing."
  (claude-repl-test--with-clean-state
    (claude-repl--mark-viewed "test-ws")
    (should-not (claude-repl--ws-get "test-ws" :status))
    (should-not (claude-repl--ws-get "test-ws" :viewed))))

(ert-deftest claude-repl-test-panels-mark-viewed-permission-noop ()
  "mark-viewed on :permission workspace does nothing."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "test-ws" :permission)
    (claude-repl--mark-viewed "test-ws")
    (should (eq (claude-repl--ws-get "test-ws" :status) :permission))
    (should-not (claude-repl--ws-get "test-ws" :viewed))))

;;;; ---- Tests: drain-pending-show-panels ----

(ert-deftest claude-repl-test-panels-drain-pending-when-set ()
  "drain-pending-show-panels calls claude-repl and clears the flag."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :pending-show-panels t)
    (let ((called nil))
      (cl-letf (((symbol-function 'claude-repl) (lambda () (setq called t))))
        (claude-repl--drain-pending-show-panels "test-ws")
        (should called)
        (should-not (claude-repl--ws-get "test-ws" :pending-show-panels))))))

(ert-deftest claude-repl-test-panels-drain-pending-when-not-set ()
  "drain-pending-show-panels does nothing when flag is nil."
  (claude-repl-test--with-clean-state
    (let ((called nil))
      (cl-letf (((symbol-function 'claude-repl) (lambda () (setq called t))))
        (claude-repl--drain-pending-show-panels "test-ws")
        (should-not called)))))

;;;; ---- Tests: close-buffer-window ----

(ert-deftest claude-repl-test-panels-close-buffer-window-no-window ()
  "close-buffer-window silently does nothing when buffer has no window."
  (claude-repl-test--with-temp-buffer "*close-test*"
    ;; Buffer exists but is not displayed in any window (beyond selected)
    ;; This should not error
    (claude-repl--close-buffer-window (get-buffer "*not-a-buffer*"))))

;;;; ---- Tests: close-buffer-windows ----

(ert-deftest claude-repl-test-panels-close-buffer-windows-nil-args ()
  "close-buffer-windows handles nil buffers gracefully."
  (claude-repl-test--with-clean-state
    ;; Should not error with nil arguments
    (claude-repl--close-buffer-windows nil nil)))

(ert-deftest claude-repl-test-panels-close-buffer-windows-dead-buffer ()
  "close-buffer-windows skips dead buffers."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*dead-buf-test*")))
      (kill-buffer buf)
      ;; Should not error with a dead buffer
      (claude-repl--close-buffer-windows buf))))

;;;; ---- Tests: configure-vterm-window ----

(ert-deftest claude-repl-test-panels-configure-vterm-window ()
  "configure-vterm-window sets dedicated, no-other-window, and size-fixed."
  (let ((win (selected-window)))
    (unwind-protect
        (progn
          (claude-repl--configure-vterm-window win)
          (should (window-dedicated-p win))
          (should (window-parameter win 'no-other-window))
          (should (eq (window-parameter win 'window-size-fixed) 'width)))
      ;; Clean up window parameters
      (set-window-dedicated-p win nil)
      (set-window-parameter win 'no-other-window nil)
      (set-window-parameter win 'window-size-fixed nil))))

;;;; ---- Tests: resolve-vterm-buffer ----

(ert-deftest claude-repl-test-panels-resolve-vterm-buffer-from-non-vterm ()
  "resolve-vterm-buffer looks up workspace vterm when not in vterm-mode."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*test-vterm-resolve*"
      (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
        (should (eq (claude-repl--resolve-vterm-buffer) (current-buffer)))))))

(ert-deftest claude-repl-test-panels-resolve-vterm-buffer-no-workspace ()
  "resolve-vterm-buffer returns nil when no workspace is active."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
      (should-not (claude-repl--resolve-vterm-buffer)))))

;;;; ---- Tests: kill-placeholder ----

(ert-deftest claude-repl-test-panels-kill-placeholder-when-exists ()
  "kill-placeholder kills the loading placeholder buffer."
  (claude-repl-test--with-clean-state
    (get-buffer-create " *claude-loading*")
    (should (get-buffer " *claude-loading*"))
    (claude-repl--kill-placeholder)
    (should-not (get-buffer " *claude-loading*"))))

(ert-deftest claude-repl-test-panels-kill-placeholder-when-absent ()
  "kill-placeholder does nothing when no placeholder exists."
  (claude-repl-test--with-clean-state
    ;; Ensure no placeholder exists
    (when-let ((buf (get-buffer " *claude-loading*")))
      (kill-buffer buf))
    ;; Should not error
    (claude-repl--kill-placeholder)))

;;;; ---- Tests: sigkill-if-alive ----

(ert-deftest claude-repl-test-panels-sigkill-if-alive-dead-process ()
  "sigkill-if-alive does nothing for a dead (nil) process."
  ;; Should not error when process is nil / not live
  (claude-repl--sigkill-if-alive nil))

;;;; ---- Tests: non-claude-panel-window-p ----

(ert-deftest claude-repl-test-panels-non-claude-panel-window-p ()
  "non-claude-panel-window-p returns t for non-Claude windows."
  (let ((win (selected-window)))
    ;; The selected window should be showing *scratch* or similar
    (should (claude-repl--non-claude-panel-window-p win))))

;;;; ---- Tests: on-close (single close audit point) ----

(ert-deftest claude-repl-test-panels-on-close-marks-hidden ()
  "on-close sets :panels-hidden on the current workspace."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'claude-repl--hide-panels) (lambda () nil)))
      (claude-repl--on-close)
      (should (claude-repl--ws-get "test-ws" :panels-hidden)))))

(ert-deftest claude-repl-test-panels-on-close-calls-hide-panels ()
  "on-close invokes hide-panels."
  (claude-repl-test--with-clean-state
    (let ((hide-called nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--hide-panels)
                 (lambda () (setq hide-called t))))
        (claude-repl--on-close)
        (should hide-called)))))

(ert-deftest claude-repl-test-panels-on-close-with-explicit-ws ()
  "on-close accepts an explicit WS argument."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ignored"))
              ((symbol-function 'claude-repl--hide-panels) (lambda () nil)))
      (claude-repl--on-close "specific-ws")
      (should (claude-repl--ws-get "specific-ws" :panels-hidden))
      (should-not (claude-repl--ws-get "ignored" :panels-hidden)))))

(ert-deftest claude-repl-test-panels-on-close-nil-ws-still-hides ()
  "on-close with nil workspace hides panels but skips bookkeeping."
  (claude-repl-test--with-clean-state
    (let ((hide-called nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
                ((symbol-function 'claude-repl--hide-panels)
                 (lambda () (setq hide-called t))))
        (claude-repl--on-close)
        (should hide-called)))))

;;;; ---- Tests: hide-and-preserve-status ----

(ert-deftest claude-repl-test-panels-hide-and-preserve-marks-hidden ()
  "hide-and-preserve-status sets :panels-hidden flag via on-close."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'claude-repl--hide-panels) (lambda () nil)))
      (claude-repl--hide-and-preserve-status)
      (should (claude-repl--ws-get "test-ws" :panels-hidden)))))

(ert-deftest claude-repl-test-panels-hide-and-preserve-no-workspace-errors ()
  "hide-and-preserve-status errors when no workspace is active."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
      (should-error (claude-repl--hide-and-preserve-status)))))

(ert-deftest claude-repl-test-panels-hide-and-preserve-routes-through-on-close ()
  "hide-and-preserve-status delegates to on-close."
  (claude-repl-test--with-clean-state
    (let ((on-close-ws nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--on-close)
                 (lambda (&optional ws) (setq on-close-ws ws))))
        (claude-repl--hide-and-preserve-status)
        (should (equal on-close-ws "test-ws"))))))

;;;; ---- Tests: show-hidden-panels ----

(ert-deftest claude-repl-test-panels-show-hidden-clears-flag ()
  "show-hidden-panels clears :panels-hidden and calls show-existing-panels."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :panels-hidden t)
    (claude-repl--ws-set "test-ws" :inactive)
    (let ((show-called nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--show-existing-panels)
                 (lambda () (setq show-called t))))
        (claude-repl--show-hidden-panels)
        (should show-called)
        (should-not (claude-repl--ws-get "test-ws" :panels-hidden))
        ;; :inactive should have been re-activated to :done via mark-viewed
        (should (eq (claude-repl--ws-get "test-ws" :status) :done))))))

;;;; ---- Tests: deferred macro ----

(ert-deftest claude-repl-test-panels-deferred-debounces ()
  "The deferred macro cancels a pending timer and schedules a new one."
  (claude-repl-test--with-clean-state
    (let ((test-timer nil)
          (call-count 0))
      (let ((debounced (claude-repl--deferred test-timer
                         (lambda () (cl-incf call-count)))))
        ;; Call twice rapidly; the first timer should be cancelled
        (funcall debounced)
        (should test-timer)
        (let ((first-timer test-timer))
          (funcall debounced)
          ;; Timer variable should have been replaced
          (should test-timer)
          ;; The first timer should have been cancelled
          (when first-timer
            (cancel-timer first-timer)))
        ;; Clean up
        (when test-timer
          (cancel-timer test-timer))))))

;;;; ---- Tests: Entry point (claude-repl) dispatch ----

(ert-deftest claude-repl-test-panels-entry-point-not-running-starts-fresh ()
  "claude-repl starts fresh when nothing is running."
  (claude-repl-test--with-clean-state
    (let ((started nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--vterm-running-p) (lambda () nil))
                ((symbol-function 'claude-repl--session-starting-p) (lambda () nil))
                ((symbol-function 'claude-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'claude-repl--start-fresh) (lambda () (setq started t))))
        (claude-repl)
        (should started)))))

(ert-deftest claude-repl-test-panels-entry-point-session-starting-shows-message ()
  "claude-repl shows loading message when session is starting."
  (claude-repl-test--with-clean-state
    (let ((messages nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--vterm-running-p) (lambda () t))
                ((symbol-function 'claude-repl--session-starting-p) (lambda () t))
                ((symbol-function 'claude-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'message) (lambda (fmt &rest _) (push fmt messages))))
        (claude-repl)
        (should (cl-some (lambda (m) (string-match-p "loading" m)) messages))))))

(ert-deftest claude-repl-test-panels-entry-point-visible-hides ()
  "claude-repl hides panels when they are visible."
  (claude-repl-test--with-clean-state
    (let ((hidden nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--vterm-running-p) (lambda () t))
                ((symbol-function 'claude-repl--session-starting-p) (lambda () nil))
                ((symbol-function 'claude-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'claude-repl--hide-and-preserve-status)
                 (lambda () (setq hidden t))))
        (claude-repl)
        (should hidden)))))

(ert-deftest claude-repl-test-panels-entry-point-hidden-shows ()
  "claude-repl shows hidden panels when running but not visible."
  (claude-repl-test--with-clean-state
    (let ((shown nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--vterm-running-p) (lambda () t))
                ((symbol-function 'claude-repl--session-starting-p) (lambda () nil))
                ((symbol-function 'claude-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'claude-repl--show-hidden-panels)
                 (lambda () (setq shown t))))
        (claude-repl)
        (should shown)))))

(ert-deftest claude-repl-test-panels-entry-point-selection-sends ()
  "claude-repl sends selected text to Claude when region is active."
  (claude-repl-test--with-clean-state
    (let ((sent-text nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--vterm-running-p) (lambda () t))
                ((symbol-function 'claude-repl--session-starting-p) (lambda () nil))
                ((symbol-function 'claude-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'region-beginning) (lambda () 1))
                ((symbol-function 'region-end) (lambda () 12))
                ((symbol-function 'buffer-substring-no-properties)
                 (lambda (_beg _end) "hello world"))
                ((symbol-function 'deactivate-mark) (lambda () nil))
                ((symbol-function 'claude-repl--send-to-claude)
                 (lambda (text) (setq sent-text text))))
        (claude-repl)
        (should (equal sent-text "hello world"))))))

;;;; ---- Tests: validate-env-switch ----

(ert-deftest claude-repl-test-panels-validate-env-switch-no-worktree ()
  "validate-env-switch errors when not a worktree workspace."
  (claude-repl-test--with-clean-state
    (should-error
     (claude-repl--validate-env-switch "test-ws" :sandbox nil "session-123")
     :type 'user-error)))

(ert-deftest claude-repl-test-panels-validate-env-switch-no-session-id ()
  "validate-env-switch errors when no session ID is available."
  (claude-repl-test--with-clean-state
    (should-error
     (claude-repl--validate-env-switch "test-ws" :sandbox t nil)
     :type 'user-error)))

(ert-deftest claude-repl-test-panels-validate-env-switch-thinking ()
  "validate-env-switch errors when Claude is thinking."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :thinking t)
    (should-error
     (claude-repl--validate-env-switch "test-ws" :sandbox t "session-123")
     :type 'user-error)))

(ert-deftest claude-repl-test-panels-validate-env-switch-no-sandbox-config ()
  "validate-env-switch errors when switching to sandbox with no config."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--resolve-sandbox-config) (lambda (_) nil))
              ((symbol-function 'claude-repl--git-root) (lambda (_) "/tmp")))
      (should-error
       (claude-repl--validate-env-switch "test-ws" :sandbox t "session-123")
       :type 'user-error))))

(ert-deftest claude-repl-test-panels-validate-env-switch-bare-metal-ok ()
  "validate-env-switch succeeds for bare-metal switch with valid args."
  (claude-repl-test--with-clean-state
    ;; Should not error
    (claude-repl--validate-env-switch "test-ws" :bare-metal t "session-123")))

;;;; ---- Tests: seed-new-env-session ----

(ert-deftest claude-repl-test-panels-seed-new-env-creates-inst ()
  "seed-new-env-session creates an instantiation and copies session-id."
  (claude-repl-test--with-clean-state
    (claude-repl--seed-new-env-session "test-ws" :sandbox "sess-abc")
    (let ((inst (claude-repl--ws-get "test-ws" :sandbox)))
      (should inst)
      (should (equal (claude-repl-instantiation-session-id inst) "sess-abc")))))

(ert-deftest claude-repl-test-panels-seed-new-env-does-not-overwrite ()
  "seed-new-env-session does not overwrite an existing session-id."
  (claude-repl-test--with-clean-state
    (let ((existing (make-claude-repl-instantiation :session-id "existing-id")))
      (claude-repl--ws-put "test-ws" :sandbox existing)
      (claude-repl--seed-new-env-session "test-ws" :sandbox "new-id")
      (let ((inst (claude-repl--ws-get "test-ws" :sandbox)))
        (should (equal (claude-repl-instantiation-session-id inst) "existing-id"))))))

;;;; ---- Tests: kill-vterm-process ----

(ert-deftest claude-repl-test-panels-kill-vterm-process-nil ()
  "kill-vterm-process does nothing for nil buffer."
  ;; Should not error
  (claude-repl--kill-vterm-process nil))

(ert-deftest claude-repl-test-panels-kill-vterm-process-dead-buffer ()
  "kill-vterm-process does nothing for a dead buffer."
  (let ((buf (get-buffer-create "*kill-proc-test*")))
    (kill-buffer buf)
    ;; Should not error
    (claude-repl--kill-vterm-process buf)))

;;;; ---- Tests: delete-non-panel-windows ----

(ert-deftest claude-repl-test-panels-delete-non-panel-windows-preserves-panels ()
  "delete-non-panel-windows does not delete windows showing panel buffers."
  ;; In batch mode we only have one window; just verify it doesn't error
  ;; when called with buffers that are not displayed
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*fake-vterm*"
      (claude-repl-test--with-temp-buffer "*fake-input*"
        ;; Should not error even when buffers aren't displayed
        (claude-repl--delete-non-panel-windows
         (get-buffer "*fake-vterm*")
         (get-buffer "*fake-input*"))))))

;;;; ---- Tests: ws-buffer-visible-p with live but undisplayed buffer ----

(ert-deftest claude-repl-test-panels-ws-buffer-visible-p-live-not-displayed ()
  "ws-buffer-visible-p returns nil when the buffer is live but not in any window."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*undisplayed-test*")))
      (unwind-protect
          (progn
            (claude-repl--ws-put "test-ws" :input-buffer buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
              ;; Display a different buffer in the selected window
              (switch-to-buffer (get-buffer-create "*other-buf*"))
              (should-not (claude-repl--ws-buffer-visible-p :input-buffer))))
        (when (buffer-live-p buf) (kill-buffer buf))
        (when (get-buffer "*other-buf*") (kill-buffer "*other-buf*"))))))

;;;; ---- Tests: vterm-visible-p with visible and dead buffer ----

(ert-deftest claude-repl-test-panels-vterm-visible-p-with-visible-buffer ()
  "vterm-visible-p returns non-nil when the vterm buffer is displayed in a window."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*test-vterm*"
      (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'get-buffer-window)
                 (lambda (_buf &rest _) (selected-window))))
        ;; Mock get-buffer-window: batch mode has no real display
        (should (claude-repl--vterm-visible-p))))))

(ert-deftest claude-repl-test-panels-vterm-visible-p-dead-buffer ()
  "vterm-visible-p returns nil when the vterm buffer has been killed."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*test-dead-vterm*")))
      (claude-repl--ws-put "test-ws" :vterm-buffer buf)
      (kill-buffer buf)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
        (should-not (claude-repl--vterm-visible-p))))))

;;;; ---- Tests: panels-visible-p multi-window cases ----

(ert-deftest claude-repl-test-panels-panels-visible-p-only-input ()
  "panels-visible-p returns nil when only input panel is visible."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*test-input-only*"
      (claude-repl--ws-put "test-ws" :input-buffer (current-buffer))
      ;; No vterm buffer set
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
        (should-not (claude-repl--panels-visible-p))))))

(ert-deftest claude-repl-test-panels-panels-visible-p-only-vterm ()
  "panels-visible-p returns nil when only vterm panel is visible."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*test-vterm-only*"
      (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
      ;; No input buffer set
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
        (should-not (claude-repl--panels-visible-p))))))

(ert-deftest claude-repl-test-panels-panels-visible-p-both-visible ()
  "panels-visible-p returns t when both panels are displayed in windows."
  (claude-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*test-both-vterm*"))
          (input-buf (get-buffer-create "*test-both-input*"))
          (new-win nil))
      (unwind-protect
          (progn
            (claude-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
            (claude-repl--ws-put "test-ws" :input-buffer input-buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
              ;; Show vterm in current window
              (switch-to-buffer vterm-buf)
              ;; Split and show input in new window
              (setq new-win (split-window))
              (set-window-buffer new-win input-buf)
              (should (claude-repl--panels-visible-p))))
        (when (and new-win (window-live-p new-win))
          (delete-window new-win))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

;;;; ---- Tests: safe-buffer-name dead buffer ----

(ert-deftest claude-repl-test-panels-safe-buffer-name-dead-buffer ()
  "safe-buffer-name returns nil for a killed buffer."
  (let ((buf (get-buffer-create "*dead-safe-name*")))
    (kill-buffer buf)
    ;; buffer-name on a dead buffer returns nil in Emacs
    (should-not (claude-repl--safe-buffer-name buf))))

;;;; ---- Tests: close-buffer-window edge cases ----

(ert-deftest claude-repl-test-panels-close-buffer-window-nil ()
  "close-buffer-window does nothing when passed nil."
  ;; when-let guards nil -- should not error
  (claude-repl--close-buffer-window nil))

(ert-deftest claude-repl-test-panels-close-buffer-window-successful-delete ()
  "close-buffer-window deletes the window displaying the buffer."
  (let ((buf (get-buffer-create "*close-win-test*"))
        (new-win nil))
    (unwind-protect
        (progn
          (setq new-win (split-window))
          (set-window-buffer new-win buf)
          (should (get-buffer-window buf))
          (claude-repl--close-buffer-window buf)
          (should-not (get-buffer-window buf)))
      (when (and new-win (window-live-p new-win))
        (ignore-errors (delete-window new-win)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-test-panels-close-buffer-window-last-window ()
  "close-buffer-window handles error when trying to delete the last window."
  ;; In batch mode, the selected window is the only window.
  ;; Display the buffer in the only window, then try to close it.
  ;; ignore-errors in the implementation should prevent error.
  (claude-repl-test--with-temp-buffer "*last-win-test*"
    (switch-to-buffer (current-buffer))
    ;; This should not error -- ignore-errors catches the "last window" error
    (claude-repl--close-buffer-window (current-buffer))))

;;;; ---- Tests: close-buffer-windows edge cases ----

(ert-deftest claude-repl-test-panels-close-buffer-windows-mix-live-and-nil ()
  "close-buffer-windows handles a mix of live and nil buffers."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*mix-live*"
      ;; Pass a mix of live buffer and nil -- should not error
      (claude-repl--close-buffer-windows (current-buffer) nil))))

(ert-deftest claude-repl-test-panels-close-buffer-windows-visible-window ()
  "close-buffer-windows closes a window displaying a buffer."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*close-wins-visible*"))
          (new-win nil))
      (unwind-protect
          (progn
            (setq new-win (split-window))
            (set-window-buffer new-win buf)
            (should (get-buffer-window buf))
            (claude-repl--close-buffer-windows buf)
            (should-not (get-buffer-window buf)))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;;;; ---- Tests: show-panels edge cases ----

(ert-deftest claude-repl-test-panels-show-panels-normal-operation ()
  "show-panels splits windows and displays vterm and input buffers."
  (claude-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*show-panels-vterm*"))
          (input-buf (get-buffer-create "*show-panels-input*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'claude-repl--refresh-vterm) (lambda () nil))
                    ((symbol-function 'claude-repl--update-all-workspace-states) (lambda () nil)))
            (claude-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
            (claude-repl--ws-put "test-ws" :input-buffer input-buf)
            (delete-other-windows)
            (claude-repl--show-panels)
            ;; Both buffers should now be visible in windows
            (should (get-buffer-window vterm-buf))
            (should (get-buffer-window input-buf)))
        ;; Clean up
        (delete-other-windows)
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

;;;; ---- Tests: focus-input-panel edge cases ----

(ert-deftest claude-repl-test-panels-focus-input-panel-nil-buffer ()
  "focus-input-panel is a no-op when input buffer is nil."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
      ;; No input buffer set -- when-let guards, should return nil
      (should-not (claude-repl--focus-input-panel)))))

(ert-deftest claude-repl-test-panels-focus-input-panel-no-window ()
  "focus-input-panel is a no-op when input buffer exists but has no window."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*focus-no-win*")))
      (unwind-protect
          (progn
            (claude-repl--ws-put "test-ws" :input-buffer buf)
            ;; Display a different buffer in the selected window
            (switch-to-buffer (get-buffer-create "*other*"))
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
              ;; Input buffer exists but is not in any window
              (should-not (claude-repl--focus-input-panel))))
        (when (buffer-live-p buf) (kill-buffer buf))
        (when (get-buffer "*other*") (kill-buffer "*other*"))))))

(ert-deftest claude-repl-test-panels-focus-input-panel-with-window ()
  "focus-input-panel selects the window displaying the input buffer."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*focus-input-win*"))
          (new-win nil))
      (unwind-protect
          (progn
            (claude-repl--ws-put "test-ws" :input-buffer buf)
            (setq new-win (split-window))
            (set-window-buffer new-win buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                      ((symbol-function 'evil-insert-state) (lambda () nil)))
              (claude-repl--focus-input-panel)
              (should (eq (window-buffer (selected-window)) buf))))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;;;; ---- Tests: show-panels-and-focus ----

(ert-deftest claude-repl-test-panels-show-panels-and-focus-delegates ()
  "show-panels-and-focus calls show-panels and focus-input-panel."
  (claude-repl-test--with-clean-state
    (let ((show-called nil)
          (focus-called nil))
      (cl-letf (((symbol-function 'claude-repl--show-panels)
                 (lambda () (setq show-called t)))
                ((symbol-function 'claude-repl--focus-input-panel)
                 (lambda () (setq focus-called t))))
        (claude-repl--show-panels-and-focus)
        (should show-called)
        (should focus-called)))))

;;;; ---- Tests: vterm-redraw with nil vterm--term ----

(ert-deftest claude-repl-test-panels-vterm-redraw-nil-term ()
  "vterm-redraw is a no-op when vterm--term is nil."
  (let ((vterm--term nil)
        (redraw-called nil))
    (cl-letf (((symbol-function 'vterm--redraw)
               (lambda (&rest _) (setq redraw-called t))))
      (claude-repl--vterm-redraw)
      ;; vterm--term is nil, so vterm--redraw should not be called
      (should-not redraw-called))))

;;;; ---- Tests: fix-vterm-scroll edge cases ----

(ert-deftest claude-repl-test-panels-fix-vterm-scroll-no-window ()
  "fix-vterm-scroll is a no-op when the buffer has no window."
  (claude-repl-test--with-temp-buffer "*no-vterm-win*"
    ;; Display a different buffer so our buffer has no window
    (let ((buf (current-buffer)))
      (switch-to-buffer (get-buffer-create "*other-scroll*"))
      (unwind-protect
          ;; Should not error -- the when guard skips the body
          (claude-repl--fix-vterm-scroll buf)
        (when (get-buffer "*other-scroll*") (kill-buffer "*other-scroll*"))))))

(ert-deftest claude-repl-test-panels-fix-vterm-scroll-same-window ()
  "fix-vterm-scroll is a no-op when vterm window is the selected window."
  (claude-repl-test--with-temp-buffer "*same-vterm-win*"
    ;; The buffer is displayed in the selected window
    ;; vterm-win eq orig-win, so the when body is skipped
    (claude-repl--fix-vterm-scroll (current-buffer))))

;;;; ---- Tests: resolve-vterm-buffer current buffer is vterm-mode ----

(ert-deftest claude-repl-test-panels-resolve-vterm-buffer-is-vterm-mode ()
  "resolve-vterm-buffer returns the current buffer when it is in vterm-mode."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*test-vterm-mode*"
      ;; Simulate vterm-mode by setting major-mode directly
      (let ((major-mode 'vterm-mode))
        (should (eq (claude-repl--resolve-vterm-buffer) (current-buffer)))))))

;;;; ---- Tests: refresh-vterm edge cases ----

(ert-deftest claude-repl-test-panels-refresh-vterm-resolve-nil ()
  "refresh-vterm is a no-op when resolve-vterm-buffer returns nil."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--resolve-vterm-buffer) (lambda () nil)))
      ;; Should not error
      (should-not (claude-repl--refresh-vterm)))))

(ert-deftest claude-repl-test-panels-refresh-vterm-dead-buffer ()
  "refresh-vterm is a no-op when the resolved buffer is dead."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*dead-refresh*")))
      (kill-buffer buf)
      (cl-letf (((symbol-function 'claude-repl--resolve-vterm-buffer) (lambda () buf)))
        ;; buffer-live-p check prevents action
        (should-not (claude-repl--refresh-vterm))))))

(ert-deftest claude-repl-test-panels-refresh-vterm-not-vterm-mode ()
  "refresh-vterm is a no-op when the resolved buffer is not in vterm-mode."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*not-vterm-mode*"
      (let ((buf (current-buffer)))
        (cl-letf (((symbol-function 'claude-repl--resolve-vterm-buffer) (lambda () buf))
                  ((symbol-function 'claude-repl--do-refresh)
                   (lambda () (error "should not be called"))))
          ;; Buffer is live but not in vterm-mode, so do-refresh is skipped
          (claude-repl--refresh-vterm))))))

;;;; ---- Tests: on-workspace-switch ws nil ----

(ert-deftest claude-repl-test-panels-on-workspace-switch-nil-ws ()
  "on-workspace-switch does not error when workspace is nil."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
              ((symbol-function 'claude-repl--update-all-workspace-states) (lambda () nil))
              ((symbol-function 'claude-repl--refresh-vterm) (lambda () nil))
              ((symbol-function 'claude-repl--reset-vterm-cursors) (lambda () nil))
              ((symbol-function 'claude-repl--drain-pending-show-panels) (lambda (_ws) nil)))
      ;; Should not error -- the when guard skips mark-viewed
      (claude-repl--on-workspace-switch))))

;;;; ---- Tests: non-claude-panel-window-p with Claude buffers ----

(ert-deftest claude-repl-test-panels-non-claude-panel-window-p-vterm-buffer ()
  "non-claude-panel-window-p returns nil for a window showing a Claude vterm buffer."
  (let ((buf (get-buffer-create "*claude-abcd1234*")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (should-not (claude-repl--non-claude-panel-window-p (selected-window))))
      (switch-to-buffer "*scratch*")
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-test-panels-non-claude-panel-window-p-input-buffer ()
  "non-claude-panel-window-p returns nil for a window showing a Claude input buffer."
  (let ((buf (get-buffer-create "*claude-input-abcd1234*")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (should-not (claude-repl--non-claude-panel-window-p (selected-window))))
      (switch-to-buffer "*scratch*")
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;; ---- Tests: redirect-from-claude-before-save ----

(ert-deftest claude-repl-test-panels-redirect-non-claude-noop ()
  "redirect-from-claude-before-save is a no-op when selected window is non-Claude."
  (claude-repl-test--with-clean-state
    ;; Selected window shows a regular buffer -- the outer when clause fails
    (let ((orig-win (selected-window)))
      (claude-repl--redirect-from-claude-before-save)
      ;; Selected window should not change
      (should (eq (selected-window) orig-win)))))

;;;; ---- Tests: hide-panels edge cases ----

(ert-deftest claude-repl-test-panels-hide-panels-both-exist ()
  "hide-panels calls close-buffer-windows with both buffers."
  (claude-repl-test--with-clean-state
    (let ((closed-bufs nil))
      (claude-repl-test--with-temp-buffer "*hide-vterm*"
        (let ((vterm-buf (current-buffer)))
          (claude-repl-test--with-temp-buffer "*hide-input*"
            (let ((input-buf (current-buffer)))
              (claude-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
              (claude-repl--ws-put "test-ws" :input-buffer input-buf)
              (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                        ((symbol-function 'claude-repl--close-buffer-windows)
                         (lambda (&rest bufs) (setq closed-bufs bufs))))
                (claude-repl--hide-panels)
                (should (equal closed-bufs (list input-buf vterm-buf)))))))))))

(ert-deftest claude-repl-test-panels-hide-panels-neither-exists ()
  "hide-panels does not error when neither buffer exists."
  (claude-repl-test--with-clean-state
    (let ((closed-bufs nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--close-buffer-windows)
                 (lambda (&rest bufs) (setq closed-bufs bufs))))
        (claude-repl--hide-panels)
        ;; Both should be nil
        (should (equal closed-bufs (list nil nil)))))))

;;;; ---- Tests: sync-panels no orphans ----

(ert-deftest claude-repl-test-panels-sync-panels-no-orphans ()
  "sync-panels does not delete any windows when there are no orphans."
  (claude-repl-test--with-clean-state
    (let ((deleted nil))
      (cl-letf (((symbol-function 'claude-repl--orphaned-panel-p) (lambda (_) nil))
                ((symbol-function 'delete-window) (lambda (w) (push w deleted))))
        (claude-repl--sync-panels)
        (should-not deleted)))))

;;;; ---- Tests: on-window-change sync-panels error ----

(ert-deftest claude-repl-test-panels-on-window-change-sync-error ()
  "on-window-change catches errors from sync-panels via condition-case."
  (claude-repl-test--with-clean-state
    (let ((overlay-called nil)
          (cursors-called nil))
      (cl-letf (((symbol-function 'claude-repl--sync-panels)
                 (lambda () (error "sync failed")))
                ((symbol-function 'claude-repl--update-hide-overlay)
                 (lambda () (setq overlay-called t)))
                ((symbol-function 'claude-repl--reset-vterm-cursors)
                 (lambda () (setq cursors-called t))))
        ;; Should not error despite sync-panels throwing
        (claude-repl--on-window-change)
        ;; Subsequent steps should still execute
        (should overlay-called)
        (should cursors-called)))))

;;;; ---- Tests: bounce-from-vterm ----

(ert-deftest claude-repl-test-panels-bounce-from-vterm-normal-window ()
  "bounce-from-vterm is a no-op when selected window lacks no-other-window param."
  (claude-repl-test--with-clean-state
    (let ((orig-win (selected-window)))
      ;; Ensure the window does not have no-other-window
      (set-window-parameter orig-win 'no-other-window nil)
      (claude-repl--bounce-from-vterm nil)
      ;; Should remain on the same window
      (should (eq (selected-window) orig-win)))))

(ert-deftest claude-repl-test-panels-bounce-from-vterm-keyboard-redirects ()
  "bounce-from-vterm redirects to input window on keyboard event."
  (claude-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*bounce-input*"))
          (new-win nil))
      (unwind-protect
          (progn
            (claude-repl--ws-put "test-ws" :input-buffer input-buf)
            (setq new-win (split-window))
            (set-window-buffer new-win input-buf)
            ;; Set no-other-window on selected window to simulate vterm window
            (set-window-parameter (selected-window) 'no-other-window t)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
              ;; Simulate keyboard event (not mouse)
              (let ((last-input-event ?a))
                (claude-repl--bounce-from-vterm nil)
                ;; Should have redirected to the input window
                (should (eq (window-buffer (selected-window)) input-buf)))))
        ;; Clean up
        (set-window-parameter (selected-window) 'no-other-window nil)
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

;;;; ---- Tests: kill-stale-vterm ----

(ert-deftest claude-repl-test-panels-kill-stale-vterm-no-buffer ()
  "kill-stale-vterm is a no-op when no buffer with the expected name exists."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--buffer-name) (lambda () "*nonexistent-stale*")))
      ;; Should not error
      (claude-repl--kill-stale-vterm))))

(ert-deftest claude-repl-test-panels-kill-stale-vterm-stale ()
  "kill-stale-vterm kills a buffer that exists without a live process."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*stale-vterm-test*")))
      (cl-letf (((symbol-function 'claude-repl--buffer-name) (lambda () "*stale-vterm-test*")))
        (should (get-buffer "*stale-vterm-test*"))
        (claude-repl--kill-stale-vterm)
        (should-not (get-buffer "*stale-vterm-test*"))))))

(ert-deftest claude-repl-test-panels-kill-stale-vterm-has-process ()
  "kill-stale-vterm preserves a buffer that has a live process."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*process-vterm-test*")))
      (unwind-protect
          (cl-letf (((symbol-function 'claude-repl--buffer-name) (lambda () "*process-vterm-test*"))
                    ((symbol-function 'get-buffer-process) (lambda (_buf) 'fake-process)))
            (claude-repl--kill-stale-vterm)
            ;; Buffer should still exist
            (should (get-buffer "*process-vterm-test*")))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;;;; ---- Tests: kill-vterm-process live buffer without process ----

(ert-deftest claude-repl-test-panels-kill-vterm-process-live-no-process ()
  "kill-vterm-process kills a live buffer that has no process."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*kill-no-proc*")))
      (claude-repl--kill-vterm-process buf)
      ;; Buffer should have been killed
      (should-not (buffer-live-p buf)))))

;;;; ---- Tests: teardown-session-state ----

(ert-deftest claude-repl-test-panels-teardown-session-state ()
  "teardown-session-state clears overlay, timers, and workspace buffer refs."
  (claude-repl-test--with-clean-state
    (let ((overlay-disabled nil)
          (state-saved nil))
      (claude-repl-test--with-temp-buffer "*teardown-vterm*"
        (let ((vterm-buf (current-buffer)))
          (claude-repl-test--with-temp-buffer "*teardown-input*"
            (let ((input-buf (current-buffer)))
              (claude-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
              (claude-repl--ws-put "test-ws" :input-buffer input-buf)
              (claude-repl--ws-put "test-ws" :active-env :bare-metal)
              (claude-repl--ws-put "test-ws" :bare-metal
                                   (make-claude-repl-instantiation :start-cmd "claude" :session-id "sess-1"))
              (cl-letf (((symbol-function 'claude-repl--disable-hide-overlay)
                         (lambda () (setq overlay-disabled t)))
                        ((symbol-function 'claude-repl--state-save)
                         (lambda (_ws) (setq state-saved t)))
                        ((symbol-function 'force-mode-line-update) (lambda (&rest _) nil)))
                (claude-repl--teardown-session-state "test-ws")
                (should overlay-disabled)
                (should state-saved)
                ;; Buffer refs should be cleared
                (should-not (claude-repl--ws-get "test-ws" :vterm-buffer))
                (should-not (claude-repl--ws-get "test-ws" :input-buffer))
                ;; Instantiation should have start-cmd cleared and had-session set
                (let ((inst (claude-repl--ws-get "test-ws" :bare-metal)))
                  (should-not (claude-repl-instantiation-start-cmd inst))
                  (should (claude-repl-instantiation-had-session inst)))))))))))

;;;; ---- Tests: destroy-session-buffers input dead ----

(ert-deftest claude-repl-test-panels-destroy-session-buffers-input-dead ()
  "destroy-session-buffers handles a dead input buffer gracefully."
  (claude-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*destroy-vterm*"))
          (input-buf (get-buffer-create "*destroy-input-dead*")))
      (kill-buffer input-buf)
      (cl-letf (((symbol-function 'claude-repl--close-buffer-windows) (lambda (&rest _) nil))
                ((symbol-function 'claude-repl--kill-placeholder) (lambda () nil))
                ((symbol-function 'claude-repl--kill-vterm-process) (lambda (_) nil)))
        ;; Should not error with dead input buffer
        (claude-repl--destroy-session-buffers vterm-buf input-buf)))))

;;;; ---- Tests: seed-new-env-session existing inst without session-id ----

(ert-deftest claude-repl-test-panels-seed-new-env-existing-no-session-id ()
  "seed-new-env-session seeds an existing instantiation that has no session-id."
  (claude-repl-test--with-clean-state
    (let ((existing (make-claude-repl-instantiation)))
      ;; existing has nil session-id
      (claude-repl--ws-put "test-ws" :sandbox existing)
      (claude-repl--seed-new-env-session "test-ws" :sandbox "new-sess-id")
      (let ((inst (claude-repl--ws-get "test-ws" :sandbox)))
        ;; Should have been seeded since there was no existing session-id
        (should (equal (claude-repl-instantiation-session-id inst) "new-sess-id"))))))

;;;; ---- Tests: show-existing-panels no workspace ----

(ert-deftest claude-repl-test-panels-show-existing-panels-no-workspace ()
  "show-existing-panels errors when no workspace is active."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
              ((symbol-function 'claude-repl--refresh-vterm) (lambda () nil)))
      (should-error (claude-repl--show-existing-panels)))))

;;;; ---- Tests: start-fresh no workspace ----

(ert-deftest claude-repl-test-panels-start-fresh-no-workspace ()
  "start-fresh errors when no workspace is active."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
      (should-error (claude-repl--start-fresh)))))

;;;; ---- Tests: schedule-sigkill ----

(ert-deftest claude-repl-test-panels-schedule-sigkill-schedules-timer ()
  "schedule-sigkill schedules a timer to call sigkill-if-alive."
  (claude-repl-test--with-clean-state
    (let ((scheduled-fn nil)
          (scheduled-delay nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (delay _repeat fn &rest _args)
                   (setq scheduled-delay delay)
                   (setq scheduled-fn fn))))
        (claude-repl--schedule-sigkill 'fake-proc)
        (should (= scheduled-delay 0.5))
        (should (eq scheduled-fn #'claude-repl--sigkill-if-alive))))))

;;;; ---- Tests: sigkill-if-alive with live process ----

(ert-deftest claude-repl-test-panels-sigkill-if-alive-live-process ()
  "sigkill-if-alive sends SIGKILL to a live process."
  (let ((signaled nil))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_proc) t))
              ((symbol-function 'signal-process)
               (lambda (proc sig) (setq signaled (list proc sig)))))
      (claude-repl--sigkill-if-alive 'fake-proc)
      (should (equal signaled '(fake-proc SIGKILL))))))

;;;; ---- Tests: do-refresh ----

(ert-deftest claude-repl-test-panels-do-refresh-calls-redraw ()
  "do-refresh calls vterm-redraw and redisplay."
  (let ((redraw-called nil)
        (redisplay-called nil)
        (vterm--term 'fake-term))
    (cl-letf (((symbol-function 'claude-repl--vterm-redraw)
               (lambda () (setq redraw-called t)))
              ((symbol-function 'redisplay)
               (lambda (&rest _) (setq redisplay-called t))))
      (claude-repl--do-refresh)
      (should redraw-called)
      (should redisplay-called))))

;;;; ---- Tests: vterm-redraw with set term ----

(ert-deftest claude-repl-test-panels-vterm-redraw-with-term ()
  "vterm-redraw calls vterm--redraw when vterm--term is set."
  (let ((vterm--term 'fake-term)
        (redraw-arg nil))
    (cl-letf (((symbol-function 'vterm--redraw)
               (lambda (term) (setq redraw-arg term))))
      (claude-repl--vterm-redraw)
      (should (eq redraw-arg 'fake-term)))))

;;;; ---- Tests: fix-vterm-scroll with different window ----

(ert-deftest claude-repl-test-panels-fix-vterm-scroll-different-window ()
  "fix-vterm-scroll briefly selects the vterm window and restores original."
  (let ((buf (get-buffer-create "*scroll-diff-win*"))
        (new-win nil))
    (unwind-protect
        (progn
          (setq new-win (split-window))
          (set-window-buffer new-win buf)
          (let ((orig-win (selected-window)))
            ;; Ensure we are NOT in the vterm window
            (should-not (eq new-win orig-win))
            (claude-repl--fix-vterm-scroll buf)
            ;; Should return to original window
            (should (eq (selected-window) orig-win))))
      (when (and new-win (window-live-p new-win))
        (ignore-errors (delete-window new-win)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;; ---- Tests: claude-repl-kill no workspace ----

(ert-deftest claude-repl-test-panels-kill-no-workspace ()
  "claude-repl-kill errors when no active workspace."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
      (should-error (claude-repl-kill)))))

;;;; ---- Tests: redirect-from-claude-before-save with Claude window ----

(ert-deftest claude-repl-test-panels-redirect-claude-to-other-window ()
  "redirect-from-claude-before-save selects a non-Claude window when current is Claude."
  (claude-repl-test--with-clean-state
    (let ((claude-buf (get-buffer-create "*claude-abcd1234*"))
          (regular-buf (get-buffer-create "*regular-buf*"))
          (new-win nil))
      (unwind-protect
          (progn
            (switch-to-buffer claude-buf)
            (setq new-win (split-window))
            (set-window-buffer new-win regular-buf)
            (claude-repl--redirect-from-claude-before-save)
            ;; Should have redirected to the window showing regular-buf
            (should (eq (window-buffer (selected-window)) regular-buf)))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (switch-to-buffer "*scratch*")
        (when (buffer-live-p claude-buf) (kill-buffer claude-buf))
        (when (buffer-live-p regular-buf) (kill-buffer regular-buf))))))

;;;; ---- Tests: redirect-from-claude-before-save fullscreen case ----

(ert-deftest claude-repl-test-panels-redirect-claude-only-window ()
  "redirect-from-claude-before-save skips redirect when Claude is the only window."
  (claude-repl-test--with-clean-state
    (let ((claude-buf (get-buffer-create "*claude-abcd1234*")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer claude-buf)
            ;; Only one window shows Claude -- cl-find-if returns nil since
            ;; the only window is also a Claude window
            (claude-repl--redirect-from-claude-before-save)
            ;; Should still be on the same Claude buffer (no redirect target)
            (should (eq (window-buffer (selected-window)) claude-buf)))
        (switch-to-buffer "*scratch*")
        (when (buffer-live-p claude-buf) (kill-buffer claude-buf))))))

;;;; ---- Tests: workspace switch and panel show edge cases (status transitions .md) ----

(ert-deftest claude-repl-test-panels-on-workspace-switch-marks-viewed-for-done ()
  "on-workspace-switch should set :viewed=t when switching to a :done workspace."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :done)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'claude-repl--update-all-workspace-states) #'ignore)
              ((symbol-function 'claude-repl--refresh-vterm) #'ignore)
              ((symbol-function 'claude-repl--reset-vterm-cursors) #'ignore)
              ((symbol-function 'claude-repl--drain-pending-show-panels) #'ignore))
      (claude-repl--on-workspace-switch)
      (should (eq (claude-repl--ws-get "ws1" :viewed) t)))))

(ert-deftest claude-repl-test-panels-on-workspace-switch-reactivates-inactive ()
  "on-workspace-switch should transition :inactive→:done WITHOUT setting :viewed.
:viewed is deferred to the next switch so update-ws-state doesn't immediately
cycle the workspace back to :inactive."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :inactive)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'claude-repl--update-all-workspace-states) #'ignore)
              ((symbol-function 'claude-repl--refresh-vterm) #'ignore)
              ((symbol-function 'claude-repl--reset-vterm-cursors) #'ignore)
              ((symbol-function 'claude-repl--drain-pending-show-panels) #'ignore))
      (claude-repl--on-workspace-switch)
      (should (eq (claude-repl--ws-state "ws1") :done))
      (should-not (claude-repl--ws-get "ws1" :viewed)))))

(ert-deftest claude-repl-test-panels-show-hidden-reactivates-inactive ()
  "show-hidden-panels should transition :inactive→:done WITHOUT setting :viewed.
:viewed is deferred so update-ws-state doesn't immediately cycle back to :inactive."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :inactive)
    (claude-repl--ws-put "ws1" :panels-hidden t)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'claude-repl--show-existing-panels) #'ignore))
      (claude-repl--show-hidden-panels)
      (should (eq (claude-repl--ws-state "ws1") :done))
      (should-not (claude-repl--ws-get "ws1" :viewed))
      (should-not (claude-repl--ws-get "ws1" :panels-hidden)))))

;;; test-panels.el ends here
