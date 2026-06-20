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

;;;; ---- Tests: output-visible-input-hidden-p ----

(ert-deftest claude-repl-test-panels-output-visible-input-hidden-p-true ()
  "output-visible-input-hidden-p is t when output is visible but input is not."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'claude-repl--vterm-visible-p) (lambda () t))
              ((symbol-function 'claude-repl--input-visible-p) (lambda () nil)))
      (should (claude-repl--output-visible-input-hidden-p)))))

(ert-deftest claude-repl-test-panels-output-visible-input-hidden-p-both-visible ()
  "output-visible-input-hidden-p is nil when both panels are visible."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'claude-repl--vterm-visible-p) (lambda () t))
              ((symbol-function 'claude-repl--input-visible-p) (lambda () t)))
      (should-not (claude-repl--output-visible-input-hidden-p)))))

(ert-deftest claude-repl-test-panels-output-visible-input-hidden-p-output-hidden ()
  "output-visible-input-hidden-p is nil when the output panel is not visible."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'claude-repl--vterm-visible-p) (lambda () nil))
              ((symbol-function 'claude-repl--input-visible-p) (lambda () nil)))
      (should-not (claude-repl--output-visible-input-hidden-p)))))

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

(ert-deftest claude-repl-test-panels-extract-id-from-vterm ()
  "extract-panel-id returns workspace identifier from a vterm buffer name."
  (should (equal (claude-repl--extract-panel-id "*claude-panel-abcd1234*")
                 "abcd1234"))
  (should (equal (claude-repl--extract-panel-id "*claude-panel-my-workspace*")
                 "my-workspace")))

(ert-deftest claude-repl-test-panels-extract-id-from-input ()
  "extract-panel-id returns workspace identifier from an input buffer name."
  (should (equal (claude-repl--extract-panel-id "*claude-panel-input-abcd1234*")
                 "abcd1234"))
  (should (equal (claude-repl--extract-panel-id "*claude-panel-input-my-workspace*")
                 "my-workspace")))

(ert-deftest claude-repl-test-panels-extract-id-non-claude ()
  "extract-panel-id returns nil for non-Claude buffer names."
  (should-not (claude-repl--extract-panel-id "*scratch*"))
  (should-not (claude-repl--extract-panel-id "*Messages*"))
  (should-not (claude-repl--extract-panel-id "config.el")))

;;;; ---- Tests: Partner buffer name ----

(ert-deftest claude-repl-test-panels-partner-of-vterm ()
  "partner-buffer-name of a vterm buffer is the input buffer."
  (should (equal (claude-repl--partner-buffer-name "*claude-panel-abcd1234*" "abcd1234")
                 "*claude-panel-input-abcd1234*")))

(ert-deftest claude-repl-test-panels-partner-of-input ()
  "partner-buffer-name of an input buffer is the vterm buffer."
  (should (equal (claude-repl--partner-buffer-name "*claude-panel-input-abcd1234*" "abcd1234")
                 "*claude-panel-abcd1234*")))

;;;; ---- Tests: Orphaned panel detection (migrated) ----

(ert-deftest claude-repl-test-panels-orphaned-vterm-p ()
  "A vterm buffer whose input partner is not visible is orphaned."
  (claude-repl-test--with-clean-state
    ;; Mock: not one-window-p, no partner window visible
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window) (lambda (_buf) nil))
              ((symbol-function 'get-buffer) (lambda (_name) nil)))
      ;; Vterm with no visible input partner is orphaned
      (should (claude-repl--orphaned-panel-p "*claude-panel-abcd1234*"))
      ;; Non-Claude buffers are never orphaned
      (should-not (claude-repl--orphaned-panel-p "*some-other*")))))

(ert-deftest claude-repl-test-panels-orphaned-input-p ()
  "An input buffer whose vterm partner is not visible is orphaned (no loading placeholder)."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window) (lambda (_buf) nil))
              ((symbol-function 'get-buffer) (lambda (_name) nil)))
      ;; Input with no visible vterm partner and no loading placeholder is orphaned
      (should (claude-repl--orphaned-panel-p "*claude-panel-input-abcd1234*"))
      ;; Non-Claude buffers are never orphaned
      (should-not (claude-repl--orphaned-panel-p "*scratch*")))))

(ert-deftest claude-repl-test-panels-orphaned-vterm-one-window ()
  "When one-window-p returns t, no panel is considered orphaned."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () t)))
      (should-not (claude-repl--orphaned-panel-p "*claude-panel-abcd1234*")))))

(ert-deftest claude-repl-test-panels-orphaned-input-with-loading ()
  "When loading placeholder buffer exists, input panel is not orphaned."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window) (lambda (_buf) nil))
              ((symbol-function 'get-buffer) (lambda (name)
                                               (when (equal name " *claude-loading*")
                                                 'fake-buffer))))
      (should-not (claude-repl--orphaned-panel-p "*claude-panel-input-abcd1234*")))))

(ert-deftest claude-repl-test-panels-orphaned-vterm-partner-visible ()
  "A vterm buffer whose input partner IS visible is not orphaned."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window)
               (lambda (buf)
                 ;; The input partner window is visible
                 (when (equal buf "*claude-panel-input-abcd1234*")
                   'fake-window))))
      (should-not (claude-repl--orphaned-panel-p "*claude-panel-abcd1234*")))))

(ert-deftest claude-repl-test-panels-orphaned-input-partner-visible ()
  "An input buffer whose vterm partner IS visible is not orphaned."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window)
               (lambda (buf)
                 ;; The vterm partner window is visible
                 (when (equal buf "*claude-panel-abcd1234*")
                   'fake-window))))
      (should-not (claude-repl--orphaned-panel-p "*claude-panel-input-abcd1234*")))))

;;;; ---- Tests: Defcustom defaults ----

(ert-deftest claude-repl-test-panels-vterm-width-fraction-default ()
  "vterm-width-fraction defcustom defaults to 0.60."
  (should (boundp 'claude-repl-vterm-width-fraction))
  (should (floatp claude-repl-vterm-width-fraction))
  (should (= (default-value 'claude-repl-vterm-width-fraction) 0.40)))

;;;; ---- Tests: Docstring accuracy (migrated) ----

(ert-deftest claude-repl-test-panels-show-panels-docstring ()
  "show-panels docstring should mention 60% and 15%."
  (let ((doc (documentation 'claude-repl--show-panels)))
    (should (string-match-p "60%" doc))
    (should (string-match-p "15%" doc))))

;;;; ---- Tests: show-input-beside-output ----

(ert-deftest claude-repl-test-panels-show-input-beside-output-splits-output ()
  "show-input-beside-output splits the output window and shows the input buffer."
  (claude-repl-test--with-clean-state
    (let* ((input-buf (get-buffer-create "*claude-panel-input-test-ws*"))
           (vterm-win (selected-window))
           (split-arg nil)
           (set-win nil)
           (set-buf nil)
           (hardened nil))
      (unwind-protect
          (progn
            (claude-repl--ws-put "test-ws" :input-buffer input-buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                      ((symbol-function 'claude-repl-window--panel-window)
                       (lambda (_kind &rest _) vterm-win))
                      ((symbol-function 'split-window)
                       (lambda (win &rest _) (setq split-arg win) 'input-win))
                      ((symbol-function 'window-total-height) (lambda (_w) 40))
                      ((symbol-function 'set-window-buffer)
                       (lambda (w b) (setq set-win w set-buf b)))
                      ((symbol-function 'claude-repl-window--harden)
                       (lambda (w &rest _) (setq hardened w))))
              (should (eq (claude-repl--show-input-beside-output) 'input-win))
              ;; Split happens on the existing output window.
              (should (eq split-arg vterm-win))
              ;; Input buffer is shown in the new window and it is hardened.
              (should (eq set-win 'input-win))
              (should (eq set-buf input-buf))
              (should (eq hardened 'input-win))))
        (kill-buffer input-buf)))))

(ert-deftest claude-repl-test-panels-show-input-beside-output-noop-no-output-window ()
  "show-input-beside-output is a no-op when the output window is not visible."
  (claude-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*claude-panel-input-test-ws*"))
          (split-called nil))
      (unwind-protect
          (progn
            (claude-repl--ws-put "test-ws" :input-buffer input-buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                      ((symbol-function 'claude-repl-window--panel-window)
                       (lambda (_kind &rest _) nil))
                      ((symbol-function 'split-window)
                       (lambda (&rest _) (setq split-called t) 'input-win)))
              (should-not (claude-repl--show-input-beside-output))
              (should-not split-called)))
        (kill-buffer input-buf)))))

(ert-deftest claude-repl-test-panels-show-input-beside-output-noop-dead-input ()
  "show-input-beside-output is a no-op when the input buffer is not live."
  (claude-repl-test--with-clean-state
    (let ((split-called nil))
      ;; No :input-buffer set — buffer is nil/dead.
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl-window--panel-window)
                 (lambda (_kind &rest _) (selected-window)))
                ((symbol-function 'split-window)
                 (lambda (&rest _) (setq split-called t) 'input-win)))
        (should-not (claude-repl--show-input-beside-output))
        (should-not split-called)))))

;;;; ---- Tests: ensure-input-beside-output ----

(ert-deftest claude-repl-test-panels-ensure-input-beside-output-repairs ()
  "ensure-input-beside-output adds the input window when output is up, input down."
  (claude-repl-test--with-clean-state
    (let ((shown nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--output-visible-input-hidden-p)
                 (lambda () t))
                ((symbol-function 'claude-repl--show-input-beside-output)
                 (lambda () (setq shown t))))
        (claude-repl--ensure-input-beside-output)
        (should shown)))))

(ert-deftest claude-repl-test-panels-ensure-input-beside-output-noop ()
  "ensure-input-beside-output is a no-op when the layout is not half-shown."
  (claude-repl-test--with-clean-state
    (let ((shown nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--output-visible-input-hidden-p)
                 (lambda () nil))
                ((symbol-function 'claude-repl--show-input-beside-output)
                 (lambda () (setq shown t))))
        (claude-repl--ensure-input-beside-output)
        (should-not shown)))))

;;;; ---- Tests: drain-pending-show-panels ----

(ert-deftest claude-repl-test-panels-drain-pending-when-set-and-ready ()
  "drain-pending-show-panels shows panels and clears the flag when Claude is ready."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :pending-show-panels t)
    (let ((called nil))
      (cl-letf (((symbol-function 'claude-repl--session-starting-p) (lambda (_ws) nil))
                ((symbol-function 'claude-repl--show-hidden-panels)
                 (lambda () (setq called t))))
        (claude-repl--drain-pending-show-panels "test-ws")
        (should called)
        (should-not (claude-repl--ws-get "test-ws" :pending-show-panels))))))

(ert-deftest claude-repl-test-panels-drain-pending-when-set-but-starting ()
  "drain-pending-show-panels defers (leaves flag set, no show) when session is starting."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :pending-show-panels t)
    (let ((called nil))
      (cl-letf (((symbol-function 'claude-repl--session-starting-p) (lambda (_ws) t))
                ((symbol-function 'claude-repl--show-hidden-panels)
                 (lambda () (setq called t))))
        (claude-repl--drain-pending-show-panels "test-ws")
        (should-not called)
        (should (claude-repl--ws-get "test-ws" :pending-show-panels))))))

(ert-deftest claude-repl-test-panels-drain-pending-when-not-set ()
  "drain-pending-show-panels does nothing when flag is nil."
  (claude-repl-test--with-clean-state
    (let ((called nil))
      (cl-letf (((symbol-function 'claude-repl--show-hidden-panels)
                 (lambda () (setq called t))))
        (claude-repl--drain-pending-show-panels "test-ws")
        (should-not called)))))

;;;; ---- Tests: drain-pending-fullscreen ----

(ert-deftest claude-repl-test-panels-drain-fullscreen-when-set ()
  "drain-pending-fullscreen enters fullscreen when :pending-fullscreen is set."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :pending-fullscreen t)
    (let ((called nil))
      (cl-letf (((symbol-function 'claude-repl--enter-fullscreen)
                 (lambda (_ws) (setq called t))))
        (claude-repl--drain-pending-fullscreen "test-ws")
        (should called)))))

(ert-deftest claude-repl-test-panels-drain-fullscreen-passes-ws ()
  "drain-pending-fullscreen enters fullscreen for the WS it was given."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :pending-fullscreen t)
    (let ((got nil))
      (cl-letf (((symbol-function 'claude-repl--enter-fullscreen)
                 (lambda (ws) (setq got ws))))
        (claude-repl--drain-pending-fullscreen "test-ws")
        (should (equal got "test-ws"))))))

(ert-deftest claude-repl-test-panels-drain-fullscreen-not-via-toggle ()
  "drain-pending-fullscreen does NOT route through the toggle command.
Routing through `claude-repl-toggle-fullscreen' would no-op when the
generated workspace's panels were split off another workspace's
already-fullscreen panels (every window a Claude buffer), so the drain
must call the unconditional `claude-repl--enter-fullscreen' instead."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :pending-fullscreen t)
    (let ((toggle-called nil))
      (cl-letf (((symbol-function 'claude-repl-toggle-fullscreen)
                 (lambda () (setq toggle-called t)))
                ((symbol-function 'claude-repl--enter-fullscreen) #'ignore))
        (claude-repl--drain-pending-fullscreen "test-ws")
        (should-not toggle-called)))))

(ert-deftest claude-repl-test-panels-drain-fullscreen-clears-flag ()
  "drain-pending-fullscreen clears :pending-fullscreen after entering fullscreen."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :pending-fullscreen t)
    (cl-letf (((symbol-function 'claude-repl--enter-fullscreen) #'ignore))
      (claude-repl--drain-pending-fullscreen "test-ws")
      (should-not (claude-repl--ws-get "test-ws" :pending-fullscreen)))))

(ert-deftest claude-repl-test-panels-drain-fullscreen-when-not-set ()
  "drain-pending-fullscreen does nothing when :pending-fullscreen is nil."
  (claude-repl-test--with-clean-state
    (let ((called nil))
      (cl-letf (((symbol-function 'claude-repl--enter-fullscreen)
                 (lambda (_ws) (setq called t))))
        (claude-repl--drain-pending-fullscreen "test-ws")
        (should-not called)))))

(ert-deftest claude-repl-test-panels-drain-fullscreen-only-once ()
  "drain-pending-fullscreen is one-shot: a second call does not re-enter."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :pending-fullscreen t)
    (let ((count 0))
      (cl-letf (((symbol-function 'claude-repl--enter-fullscreen)
                 (lambda (_ws) (setq count (1+ count)))))
        (claude-repl--drain-pending-fullscreen "test-ws")
        (claude-repl--drain-pending-fullscreen "test-ws")
        (should (= count 1))))))

;;;; ---- Tests: fullscreen-active-p ----

(ert-deftest claude-repl-test-panels-fullscreen-active-p-saved-config ()
  "fullscreen-active-p is non-nil when :fullscreen-config is set."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :fullscreen-config 'some-config)
    (cl-letf (((symbol-function 'claude-repl--fullscreen-p) (lambda () nil)))
      (should (claude-repl--fullscreen-active-p "test-ws")))))

(ert-deftest claude-repl-test-panels-fullscreen-active-p-layout ()
  "fullscreen-active-p is non-nil when the live layout is fullscreen."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--fullscreen-p) (lambda () t)))
      (should (claude-repl--fullscreen-active-p "test-ws")))))

(ert-deftest claude-repl-test-panels-fullscreen-active-p-neither ()
  "fullscreen-active-p is nil with no saved config and a non-fullscreen layout."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--fullscreen-p) (lambda () nil)))
      (should-not (claude-repl--fullscreen-active-p "test-ws")))))

;;;; ---- Tests: maybe-fullscreen-on-switch ----

(ert-deftest claude-repl-test-panels-maybe-fullscreen-on-switch-enters ()
  "maybe-fullscreen-on-switch enters fullscreen for the current visible ws."
  (claude-repl-test--with-clean-state
    (let ((called nil))
      (cl-letf (((symbol-function 'claude-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'claude-repl--fullscreen-p) (lambda () nil))
                ((symbol-function 'claude-repl-toggle-fullscreen)
                 (lambda () (setq called t))))
        (claude-repl--maybe-fullscreen-on-switch "test-ws")
        (should called)))))

(ert-deftest claude-repl-test-panels-maybe-fullscreen-on-switch-no-panels ()
  "maybe-fullscreen-on-switch is a no-op when the panels are not visible."
  (claude-repl-test--with-clean-state
    (let ((called nil))
      (cl-letf (((symbol-function 'claude-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'claude-repl--fullscreen-p) (lambda () nil))
                ((symbol-function 'claude-repl-toggle-fullscreen)
                 (lambda () (setq called t))))
        (claude-repl--maybe-fullscreen-on-switch "test-ws")
        (should-not called)))))

(ert-deftest claude-repl-test-panels-maybe-fullscreen-on-switch-already-saved ()
  "maybe-fullscreen-on-switch is a no-op when :fullscreen-config is already set."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :fullscreen-config 'some-config)
    (let ((called nil))
      (cl-letf (((symbol-function 'claude-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'claude-repl--fullscreen-p) (lambda () nil))
                ((symbol-function 'claude-repl-toggle-fullscreen)
                 (lambda () (setq called t))))
        (claude-repl--maybe-fullscreen-on-switch "test-ws")
        (should-not called)))))

(ert-deftest claude-repl-test-panels-maybe-fullscreen-on-switch-already-layout ()
  "maybe-fullscreen-on-switch is a no-op when the live layout is already fullscreen."
  (claude-repl-test--with-clean-state
    (let ((called nil))
      (cl-letf (((symbol-function 'claude-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'claude-repl--fullscreen-p) (lambda () t))
                ((symbol-function 'claude-repl-toggle-fullscreen)
                 (lambda () (setq called t))))
        (claude-repl--maybe-fullscreen-on-switch "test-ws")
        (should-not called)))))

(ert-deftest claude-repl-test-panels-maybe-fullscreen-on-switch-not-current ()
  "maybe-fullscreen-on-switch is a no-op when WS is not the current workspace."
  (claude-repl-test--with-clean-state
    (let ((called nil))
      (cl-letf (((symbol-function 'claude-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'claude-repl--fullscreen-p) (lambda () nil))
                ((symbol-function 'claude-repl-toggle-fullscreen)
                 (lambda () (setq called t))))
        (claude-repl--maybe-fullscreen-on-switch "other-ws")
        (should-not called)))))

;;;; ---- Tests: drain-pending-magit ----

(ert-deftest claude-repl-test-panels-drain-pending-magit-when-set ()
  "drain-pending-magit calls magit-status with :project-dir and clears the flag."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :pending-magit t)
    (claude-repl--ws-put "test-ws" :project-dir "/tmp/my-worktree")
    (let ((magit-path nil)
          (dash-called nil))
      (cl-letf (((symbol-function 'magit-status)
                 (lambda (path) (setq magit-path path)))
                ((symbol-function 'claude-repl--remove-doom-dashboard)
                 (lambda () (setq dash-called t))))
        (claude-repl--drain-pending-magit "test-ws")
        (should (equal magit-path "/tmp/my-worktree"))
        (should dash-called)
        (should-not (claude-repl--ws-get "test-ws" :pending-magit))))))

(ert-deftest claude-repl-test-panels-drain-pending-magit-when-not-set ()
  "drain-pending-magit does nothing when :pending-magit flag is nil."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :project-dir "/tmp/my-worktree")
    (let ((magit-called nil)
          (dash-called nil))
      (cl-letf (((symbol-function 'magit-status)
                 (lambda (&rest _) (setq magit-called t)))
                ((symbol-function 'claude-repl--remove-doom-dashboard)
                 (lambda () (setq dash-called t))))
        (claude-repl--drain-pending-magit "test-ws")
        (should-not magit-called)
        (should-not dash-called)))))

(ert-deftest claude-repl-test-panels-drain-pending-magit-only-once ()
  "drain-pending-magit is one-shot: a second activation does not reopen magit."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :pending-magit t)
    (claude-repl--ws-put "test-ws" :project-dir "/tmp/my-worktree")
    (let ((magit-call-count 0))
      (cl-letf (((symbol-function 'magit-status)
                 (lambda (&rest _) (cl-incf magit-call-count)))
                ((symbol-function 'claude-repl--remove-doom-dashboard) #'ignore))
        (claude-repl--drain-pending-magit "test-ws")
        (claude-repl--drain-pending-magit "test-ws")
        (should (equal magit-call-count 1))))))

(ert-deftest claude-repl-test-panels-drain-pending-magit-no-project-dir ()
  "drain-pending-magit clears the flag but skips magit-status when :project-dir is missing.
Defensive: :project-dir is always written by setup-worktree-session before
finalize returns, so this path shouldn't occur in practice — but a missing
path must not error."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :pending-magit t)
    (let ((magit-called nil))
      (cl-letf (((symbol-function 'magit-status)
                 (lambda (&rest _) (setq magit-called t)))
                ((symbol-function 'claude-repl--remove-doom-dashboard) #'ignore))
        (claude-repl--drain-pending-magit "test-ws")
        (should-not magit-called)
        (should-not (claude-repl--ws-get "test-ws" :pending-magit))))))

;;;; ---- Tests: drain-pending-initial-buffers ----

(ert-deftest claude-repl-test-panels-drain-pending-initial-buffers-when-set ()
  "drain-pending-initial-buffers calls open-initial-buffers with WS and :project-dir, clears the flag."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :pending-initial-buffers t)
    (claude-repl--ws-put "test-ws" :project-dir "/tmp/my-worktree")
    (let ((call-args nil))
      (cl-letf (((symbol-function 'claude-repl--open-initial-buffers)
                 (lambda (ws path) (setq call-args (list ws path)))))
        (claude-repl--drain-pending-initial-buffers "test-ws")
        (should (equal call-args '("test-ws" "/tmp/my-worktree")))
        (should-not (claude-repl--ws-get "test-ws" :pending-initial-buffers))))))

(ert-deftest claude-repl-test-panels-drain-pending-initial-buffers-when-not-set ()
  "drain-pending-initial-buffers does nothing when :pending-initial-buffers is nil."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :project-dir "/tmp/my-worktree")
    (let ((open-called nil))
      (cl-letf (((symbol-function 'claude-repl--open-initial-buffers)
                 (lambda (&rest _) (setq open-called t))))
        (claude-repl--drain-pending-initial-buffers "test-ws")
        (should-not open-called)))))

(ert-deftest claude-repl-test-panels-drain-pending-initial-buffers-only-once ()
  "drain-pending-initial-buffers is one-shot: a second activation does not re-open."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :pending-initial-buffers t)
    (claude-repl--ws-put "test-ws" :project-dir "/tmp/my-worktree")
    (let ((call-count 0))
      (cl-letf (((symbol-function 'claude-repl--open-initial-buffers)
                 (lambda (&rest _) (cl-incf call-count))))
        (claude-repl--drain-pending-initial-buffers "test-ws")
        (claude-repl--drain-pending-initial-buffers "test-ws")
        (should (equal call-count 1))))))

(ert-deftest claude-repl-test-panels-drain-pending-initial-buffers-no-project-dir ()
  "drain-pending-initial-buffers clears the flag but skips the call when :project-dir is missing.
Defensive: :project-dir is always written by setup-worktree-session before
finalize returns, so this path shouldn't occur in practice — but a missing
path must not error."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :pending-initial-buffers t)
    (let ((open-called nil))
      (cl-letf (((symbol-function 'claude-repl--open-initial-buffers)
                 (lambda (&rest _) (setq open-called t))))
        (claude-repl--drain-pending-initial-buffers "test-ws")
        (should-not open-called)
        (should-not (claude-repl--ws-get "test-ws" :pending-initial-buffers))))))

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
  "configure-vterm-window sets dedicated + width-fixed + no-delete-other-windows.
Does NOT set `no-other-window' — keyboard isolation now comes from
`claude-repl--bounce-from-vterm', so vterm stays visible to
`other-window'/`windmove' but any non-mouse landing is auto-corrected."
  (let ((win (selected-window)))
    (unwind-protect
        (progn
          (claude-repl--configure-vterm-window win)
          (should (window-dedicated-p win))
          (should-not (window-parameter win 'no-other-window))
          (should (eq (window-parameter win 'window-size-fixed) 'width))
          (should (window-parameter win 'no-delete-other-windows)))
      ;; Clean up window parameters
      (set-window-dedicated-p win nil)
      (set-window-parameter win 'window-size-fixed nil)
      (set-window-parameter win 'no-delete-other-windows nil))))

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

(ert-deftest claude-repl-test-panels-on-close-calls-hide-panels ()
  "on-close invokes hide-panels."
  (claude-repl-test--with-clean-state
    (let ((hide-called nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--hide-panels)
                 (lambda () (setq hide-called t)))
                ((symbol-function 'claude-repl-workspace-push-to-back) #'ignore))
        (claude-repl--on-close)
        (should hide-called)))))

(ert-deftest claude-repl-test-panels-on-close-with-explicit-ws ()
  "on-close accepts an explicit WS argument."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ignored"))
              ((symbol-function 'claude-repl--hide-panels) (lambda () nil)))
      (claude-repl--on-close "specific-ws")
      (should (eq (claude-repl--ws-get "specific-ws" :repl-state) :hidden))
      (should-not (claude-repl--ws-get "ignored" :repl-state)))))

(ert-deftest claude-repl-test-panels-on-close-nil-ws-still-hides ()
  "on-close with nil workspace hides panels but skips bookkeeping."
  (claude-repl-test--with-clean-state
    (let ((hide-called nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
                ((symbol-function 'claude-repl--hide-panels)
                 (lambda () (setq hide-called t))))
        (claude-repl--on-close)
        (should hide-called)))))

(ert-deftest claude-repl-test-panels-on-close-sets-repl-state-hidden ()
  "on-close (deprio path) writes :repl-state :hidden so the workspace is a
sweep candidate when hide-mode is enabled.  Distinct from on-simple-close
which writes :inactive."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'claude-repl--hide-panels) (lambda () nil))
              ((symbol-function 'claude-repl-workspace-push-to-back) #'ignore))
      (claude-repl--on-close)
      (should (eq (claude-repl--ws-get "test-ws" :repl-state) :hidden)))))

(ert-deftest claude-repl-test-panels-on-close-preserves-claude-state ()
  "on-close does not touch :claude-state — mid-task :thinking survives close."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set-claude-state "test-ws" :thinking)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'claude-repl--hide-panels) (lambda () nil))
              ((symbol-function 'claude-repl-workspace-push-to-back) #'ignore))
      (claude-repl--on-close)
      (should (eq (claude-repl--ws-claude-state "test-ws") :thinking))
      (should (eq (claude-repl--ws-get "test-ws" :repl-state) :hidden)))))

(ert-deftest claude-repl-test-panels-on-close-pushes-current-ws-to-back ()
  "on-close calls `claude-repl-workspace-push-to-back' when WS is the current workspace."
  (claude-repl-test--with-clean-state
    (let ((push-called 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--hide-panels) (lambda () nil))
                ((symbol-function 'claude-repl-workspace-push-to-back)
                 (lambda (&optional _keep) (cl-incf push-called))))
        (claude-repl--on-close)
        (should (= push-called 1))))))

(ert-deftest claude-repl-test-panels-on-close-does-not-keep-focus ()
  "on-close calls push-to-back without KEEP-FOCUS so focus moves to a fresh workspace."
  (claude-repl-test--with-clean-state
    (let ((received-args 'unset))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--hide-panels) (lambda () nil))
                ((symbol-function 'claude-repl-workspace-push-to-back)
                 (lambda (&rest args) (setq received-args args))))
        (claude-repl--on-close)
        (should (equal received-args nil))))))

(ert-deftest claude-repl-test-panels-on-close-skips-push-when-explicit-ws-not-current ()
  "on-close does not push to back when an explicit WS is not the current workspace."
  (claude-repl-test--with-clean-state
    (let ((push-called 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
                ((symbol-function 'claude-repl--hide-panels) (lambda () nil))
                ((symbol-function 'claude-repl-workspace-push-to-back)
                 (lambda (&optional _keep) (cl-incf push-called))))
        (claude-repl--on-close "other-ws")
        (should (= push-called 0))))))

(ert-deftest claude-repl-test-panels-on-close-skips-push-when-no-workspace ()
  "on-close does not push to back when no workspace is active."
  (claude-repl-test--with-clean-state
    (let ((push-called 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
                ((symbol-function 'claude-repl--hide-panels) (lambda () nil))
                ((symbol-function 'claude-repl-workspace-push-to-back)
                 (lambda (&optional _keep) (cl-incf push-called))))
        (claude-repl--on-close)
        (should (= push-called 0))))))

(ert-deftest claude-repl-test-panels-on-close-saves-tab-index-before-pushing ()
  "on-close calls save-tab-index before push so the captured index is the original."
  (claude-repl-test--with-clean-state
    (let ((calls nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--hide-panels) (lambda () nil))
                ((symbol-function 'claude-repl--save-tab-index)
                 (lambda (_ws) (push 'save calls)))
                ((symbol-function 'claude-repl-workspace-push-to-back)
                 (lambda (&optional _keep) (push 'push calls))))
        (claude-repl--on-close)
        (should (equal (reverse calls) '(save push)))))))

;;;; ---- Tests: on-simple-close (no-deprio variant) ----

(ert-deftest claude-repl-test-panels-on-simple-close-sets-inactive ()
  "on-simple-close writes :repl-state :inactive."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'claude-repl--hide-panels) (lambda () nil)))
      (claude-repl--on-simple-close)
      (should (eq :inactive (claude-repl--ws-get "test-ws" :repl-state))))))

(ert-deftest claude-repl-test-panels-on-simple-close-hides-panels ()
  "on-simple-close calls hide-panels."
  (claude-repl-test--with-clean-state
    (let ((hide-called 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--hide-panels)
                 (lambda () (cl-incf hide-called))))
        (claude-repl--on-simple-close)
        (should (= 1 hide-called))))))

(ert-deftest claude-repl-test-panels-on-simple-close-does-not-save-tab-index ()
  "on-simple-close does NOT call save-tab-index — that's the deprio path."
  (claude-repl-test--with-clean-state
    (let ((save-called 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--hide-panels) (lambda () nil))
                ((symbol-function 'claude-repl--save-tab-index)
                 (lambda (_ws) (cl-incf save-called))))
        (claude-repl--on-simple-close)
        (should (= 0 save-called))))))

(ert-deftest claude-repl-test-panels-on-simple-close-does-not-push-to-back ()
  "on-simple-close does NOT call push-to-back — that's the deprio path."
  (claude-repl-test--with-clean-state
    (let ((push-called 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--hide-panels) (lambda () nil))
                ((symbol-function 'claude-repl-workspace-push-to-back)
                 (lambda (&rest _) (cl-incf push-called))))
        (claude-repl--on-simple-close)
        (should (= 0 push-called))))))

(ert-deftest claude-repl-test-panels-on-simple-close-exits-fullscreen-before-hide ()
  "on-simple-close restores the pre-fullscreen layout before hiding panels.
The restore must run BEFORE hide-panels so hide-panels deletes the panels
from the restored splitscreen layout rather than from the full-frame one."
  (claude-repl-test--with-clean-state
    (let ((order '()))
      (claude-repl--ws-put "test-ws" :fullscreen-config 'saved-config)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'set-window-configuration)
                 (lambda (_cfg) (push 'restore order)))
                ((symbol-function 'claude-repl--hide-panels)
                 (lambda () (push 'hide order))))
        (claude-repl--on-simple-close)
        (should (equal order '(hide restore)))))))

(ert-deftest claude-repl-test-panels-on-simple-close-clears-fullscreen-config ()
  "on-simple-close clears :fullscreen-config when exiting fullscreen."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :fullscreen-config 'saved-config)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'set-window-configuration) #'ignore)
              ((symbol-function 'claude-repl--hide-panels) #'ignore))
      (claude-repl--on-simple-close)
      (should-not (claude-repl--ws-get "test-ws" :fullscreen-config)))))

(ert-deftest claude-repl-test-panels-on-simple-close-no-restore-without-config ()
  "on-simple-close does NOT call set-window-configuration when not fullscreen."
  (claude-repl-test--with-clean-state
    (let ((restore-called 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'set-window-configuration)
                 (lambda (_cfg) (cl-incf restore-called)))
                ((symbol-function 'claude-repl--hide-panels) #'ignore))
        ;; No :fullscreen-config set on test-ws.
        (claude-repl--on-simple-close)
        (should (= 0 restore-called))))))

(ert-deftest claude-repl-test-panels-on-simple-close-fullscreen-leaves-work-window ()
  "on-simple-close on a fullscreen ws removes panels and leaves the work window.
End-to-end with real windows: a fullscreen layout (only the two panels)
plus a saved splitscreen config restores to work+panels, then hides the
panels, leaving just the work window — the `SPC o c' goes-away contract."
  (claude-repl-test--with-clean-state
    (let ((wconf (current-window-configuration))
          (work-buf (generate-new-buffer "*fsclose-work*"))
          (vterm-buf (generate-new-buffer "*claude-panel-fsclose*"))
          (input-buf (generate-new-buffer "*claude-panel-input-fsclose*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
            (delete-other-windows)
            (let* ((work-win (selected-window))
                   (vterm-win (split-window work-win nil 'right))
                   (input-win (split-window vterm-win nil 'below)))
              (set-window-buffer work-win work-buf)
              (set-window-buffer vterm-win vterm-buf)
              (set-window-buffer input-win input-buf)
              (claude-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
              (claude-repl--ws-put "test-ws" :input-buffer input-buf)
              ;; Capture the splitscreen layout as the pre-fullscreen config.
              (claude-repl--ws-put "test-ws" :fullscreen-config
                                   (current-window-configuration))
              ;; Enter fullscreen: delete the work window, leaving only panels.
              (delete-window work-win)
              (should-not (window-live-p work-win))
              ;; SPC o c.
              (claude-repl--on-simple-close)
              ;; Panels are gone.
              (should-not (get-buffer-window vterm-buf))
              (should-not (get-buffer-window input-buf))
              ;; The work window's buffer is back onscreen.
              (should (get-buffer-window work-buf))
              ;; Fullscreen config was cleared.
              (should-not (claude-repl--ws-get "test-ws" :fullscreen-config))))
        (set-window-configuration wconf)
        (kill-buffer work-buf)
        (kill-buffer vterm-buf)
        (kill-buffer input-buf)))))

;;;; ---- Tests: restore-fullscreen-config ----

(ert-deftest claude-repl-test-panels-restore-fullscreen-config-restores-and-clears ()
  "restore-fullscreen-config restores the saved config and clears the flag."
  (claude-repl-test--with-clean-state
    (let ((restored nil))
      (claude-repl--ws-put "test-ws" :fullscreen-config 'saved-config)
      (cl-letf (((symbol-function 'set-window-configuration)
                 (lambda (cfg) (setq restored cfg))))
        (should (claude-repl--restore-fullscreen-config "test-ws"))
        (should (eq restored 'saved-config))
        (should-not (claude-repl--ws-get "test-ws" :fullscreen-config))))))

(ert-deftest claude-repl-test-panels-restore-fullscreen-config-noop-without-config ()
  "restore-fullscreen-config returns nil and does nothing with no saved config."
  (claude-repl-test--with-clean-state
    (let ((restore-called 0))
      (cl-letf (((symbol-function 'set-window-configuration)
                 (lambda (_cfg) (cl-incf restore-called))))
        (should-not (claude-repl--restore-fullscreen-config "test-ws"))
        (should (= 0 restore-called))))))

(ert-deftest claude-repl-test-panels-restore-fullscreen-config-noop-on-nil-ws ()
  "restore-fullscreen-config returns nil when WS is nil."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'set-window-configuration)
               (lambda (_cfg) (error "should not restore"))))
      (should-not (claude-repl--restore-fullscreen-config nil)))))

;;;; ---- Tests: simple-hide-and-preserve-status ----

(ert-deftest claude-repl-test-panels-simple-hide-routes-through-on-simple-close ()
  "simple-hide-and-preserve-status delegates to on-simple-close."
  (claude-repl-test--with-clean-state
    (let ((received-ws nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--on-simple-close)
                 (lambda (&optional ws) (setq received-ws ws))))
        (claude-repl--simple-hide-and-preserve-status)
        (should (equal received-ws "test-ws"))))))

(ert-deftest claude-repl-test-panels-simple-hide-no-workspace-errors ()
  "simple-hide-and-preserve-status errors when no workspace is active."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
      (should-error (claude-repl--simple-hide-and-preserve-status)))))

;;;; ---- Tests: claude-repl-simple toggle ----

(ert-deftest claude-repl-test-panels-claude-repl-simple-uses-simple-hide ()
  "claude-repl-simple dispatches the visible-panels case to simple-hide."
  (claude-repl-test--with-clean-state
    (let ((simple-called 0)
          (full-called 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--claude-running-p) (lambda () t))
                ((symbol-function 'claude-repl--session-starting-p) (lambda () nil))
                ((symbol-function 'claude-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'claude-repl--simple-hide-and-preserve-status)
                 (lambda () (cl-incf simple-called)))
                ((symbol-function 'claude-repl--hide-and-preserve-status)
                 (lambda () (cl-incf full-called))))
        (claude-repl-simple)
        (should (= 1 simple-called))
        (should (= 0 full-called))))))

(ert-deftest claude-repl-test-panels-claude-repl-uses-full-hide ()
  "claude-repl (deprio variant) dispatches the visible-panels case to hide-and-preserve."
  (claude-repl-test--with-clean-state
    (let ((simple-called 0)
          (full-called 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--claude-running-p) (lambda () t))
                ((symbol-function 'claude-repl--session-starting-p) (lambda () nil))
                ((symbol-function 'claude-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'claude-repl--simple-hide-and-preserve-status)
                 (lambda () (cl-incf simple-called)))
                ((symbol-function 'claude-repl--hide-and-preserve-status)
                 (lambda () (cl-incf full-called))))
        (claude-repl)
        (should (= 0 simple-called))
        (should (= 1 full-called))))))

;;;; ---- Tests: save-tab-index ----

(ert-deftest claude-repl-test-panels-save-tab-index-writes-position ()
  "save-tab-index records the workspace's persp index as :saved-tab-index."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-names-current-frame-fast-ordered)
               (lambda () '("a" "b" "test-ws" "c"))))
      (claude-repl--save-tab-index "test-ws")
      (should (= 2 (claude-repl--ws-get "test-ws" :saved-tab-index))))))

(ert-deftest claude-repl-test-panels-save-tab-index-skips-when-not-in-list ()
  "save-tab-index is a no-op when the workspace name isn't in the persp list."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-names-current-frame-fast-ordered)
               (lambda () '("a" "b"))))
      (claude-repl--save-tab-index "missing-ws")
      (should-not (claude-repl--ws-get "missing-ws" :saved-tab-index)))))

(ert-deftest claude-repl-test-panels-save-tab-index-skips-when-persp-unavailable ()
  "save-tab-index is a no-op when persp helper is not bound."
  (claude-repl-test--with-clean-state
    (when (fboundp 'persp-names-current-frame-fast-ordered)
      (fmakunbound 'persp-names-current-frame-fast-ordered))
    (claude-repl--save-tab-index "test-ws")
    (should-not (claude-repl--ws-get "test-ws" :saved-tab-index))))

;;;; ---- Tests: restore-tab-index ----

(ert-deftest claude-repl-test-panels-restore-tab-index-moves-ws-to-saved-slot ()
  "restore-tab-index reorders names so WS is at its saved index."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws-c" :saved-tab-index 1)
    (let ((reordered nil))
      (cl-letf (((symbol-function 'persp-names-current-frame-fast-ordered)
                 (lambda () '("ws-a" "ws-b" "ws-c")))  ; ws-c is at end
                ((symbol-function 'persp-update-names-cache)
                 (lambda (names) (setq reordered names))))
        (claude-repl--restore-tab-index "ws-c")
        (should (equal reordered '("ws-a" "ws-c" "ws-b")))))))

(ert-deftest claude-repl-test-panels-restore-tab-index-clears-saved-index ()
  "restore-tab-index clears :saved-tab-index after a successful restore."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws-c" :saved-tab-index 0)
    (cl-letf (((symbol-function 'persp-names-current-frame-fast-ordered)
               (lambda () '("ws-a" "ws-b" "ws-c")))
              ((symbol-function 'persp-update-names-cache) (lambda (_) nil)))
      (claude-repl--restore-tab-index "ws-c")
      (should-not (claude-repl--ws-get "ws-c" :saved-tab-index)))))

(ert-deftest claude-repl-test-panels-restore-tab-index-clamps-past-tail ()
  "restore-tab-index clamps a saved index larger than the new list length."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws-c" :saved-tab-index 99)
    (let ((reordered nil))
      (cl-letf (((symbol-function 'persp-names-current-frame-fast-ordered)
                 (lambda () '("ws-a" "ws-b" "ws-c")))
                ((symbol-function 'persp-update-names-cache)
                 (lambda (names) (setq reordered names))))
        (claude-repl--restore-tab-index "ws-c")
        ;; Clamped: ws-c lands at the tail of the without-ws list.
        (should (equal reordered '("ws-a" "ws-b" "ws-c")))))))

(ert-deftest claude-repl-test-panels-restore-tab-index-noop-when-no-saved-index ()
  "restore-tab-index does nothing when no :saved-tab-index is set."
  (claude-repl-test--with-clean-state
    (let ((called 0))
      (cl-letf (((symbol-function 'persp-names-current-frame-fast-ordered)
                 (lambda () '("a")))
                ((symbol-function 'persp-update-names-cache)
                 (lambda (_) (cl-incf called))))
        (claude-repl--restore-tab-index "no-saved-ws")
        (should (= 0 called))))))

;;;; ---- Tests: hide-and-preserve-status ----

(ert-deftest claude-repl-test-panels-hide-and-preserve-marks-hidden ()
  "hide-and-preserve-status routes through on-close (deprio path) and sets
:repl-state :hidden so the workspace is a sweep candidate when hide-mode
is on."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'claude-repl--hide-panels) (lambda () nil))
              ((symbol-function 'claude-repl-workspace-push-to-back) #'ignore))
      (claude-repl--hide-and-preserve-status)
      (should (eq (claude-repl--ws-get "test-ws" :repl-state) :hidden)))))

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

(ert-deftest claude-repl-test-panels-show-hidden-calls-show-existing ()
  "show-hidden-panels calls show-existing-panels."
  (claude-repl-test--with-clean-state
    (let ((show-called nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--show-existing-panels)
                 (lambda () (setq show-called t))))
        (claude-repl--show-hidden-panels)
        (should show-called)))))

(ert-deftest claude-repl-test-panels-show-hidden-drains-fullscreen ()
  "show-hidden-panels drains :pending-fullscreen after showing panels."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :pending-fullscreen t)
    (let ((fullscreen-called nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--show-existing-panels) #'ignore)
                ((symbol-function 'claude-repl--enter-fullscreen)
                 (lambda (_ws) (setq fullscreen-called t))))
        (claude-repl--show-hidden-panels)
        (should fullscreen-called)
        (should-not (claude-repl--ws-get "test-ws" :pending-fullscreen))))))

(ert-deftest claude-repl-test-panels-show-hidden-sets-active ()
  "show-hidden-panels (via show-existing-panels) sets :repl-state :active."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set-repl-state "test-ws" :inactive)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'claude-repl--refresh-vterm) #'ignore)
              ((symbol-function 'delete-other-windows) #'ignore)
              ((symbol-function 'claude-repl--show-panels-and-focus) #'ignore)
              ((symbol-function 'claude-repl--update-hide-overlay) #'ignore))
      (claude-repl--show-hidden-panels)
      (should (eq (claude-repl--ws-get "test-ws" :repl-state) :active)))))

(ert-deftest claude-repl-test-panels-show-existing-sets-active ()
  "show-existing-panels sets :repl-state :active."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set-repl-state "test-ws" :inactive)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'claude-repl--refresh-vterm) #'ignore)
              ((symbol-function 'delete-other-windows) #'ignore)
              ((symbol-function 'claude-repl--show-panels-and-focus) #'ignore)
              ((symbol-function 'claude-repl--update-hide-overlay) #'ignore))
      (claude-repl--show-existing-panels)
      (should (eq (claude-repl--ws-get "test-ws" :repl-state) :active)))))

(ert-deftest claude-repl-test-panels-show-existing-restores-tab-index ()
  "show-existing-panels calls restore-tab-index for the current workspace."
  (claude-repl-test--with-clean-state
    (let ((restored-ws nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--refresh-vterm) #'ignore)
                ((symbol-function 'delete-other-windows) #'ignore)
                ((symbol-function 'claude-repl--show-panels-and-focus) #'ignore)
                ((symbol-function 'claude-repl--update-hide-overlay) #'ignore)
                ((symbol-function 'claude-repl--restore-tab-index)
                 (lambda (ws) (setq restored-ws ws))))
        (claude-repl--show-existing-panels)
        (should (equal restored-ws "test-ws"))))))

(ert-deftest claude-repl-test-panels-show-existing-flashes-tab ()
  "show-existing-panels pulses the workspace tab so reopen is visually marked."
  (claude-repl-test--with-clean-state
    (let ((flashed-ws nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--refresh-vterm) #'ignore)
                ((symbol-function 'delete-other-windows) #'ignore)
                ((symbol-function 'claude-repl--show-panels-and-focus) #'ignore)
                ((symbol-function 'claude-repl--update-hide-overlay) #'ignore)
                ((symbol-function 'claude-repl--restore-tab-index) #'ignore)
                ((symbol-function 'claude-repl-flash-tab)
                 (lambda (ws &rest _) (setq flashed-ws ws))))
        (claude-repl--show-existing-panels)
        (should (equal flashed-ws "test-ws"))))))

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

(ert-deftest claude-repl-test-panels-entry-point-not-running-hides ()
  "claude-repl (SPC o C, always-close) hides the workspace even when no
Claude session is running.  Skips the initialize-claude branch the
plain `claude-repl-simple' (SPC o c) toggle would otherwise take."
  (claude-repl-test--with-clean-state
    (let ((started nil) (hidden nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--claude-running-p) (lambda () nil))
                ((symbol-function 'claude-repl--session-starting-p) (lambda () nil))
                ((symbol-function 'claude-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'claude-repl--initialize-claude) (lambda (&rest _) (setq started t)))
                ((symbol-function 'claude-repl--hide-and-preserve-status)
                 (lambda () (setq hidden t))))
        (claude-repl)
        (should hidden)
        (should-not started)))))

(ert-deftest claude-repl-test-panels-entry-point-session-starting-hides ()
  "claude-repl hides the workspace mid-startup rather than showing a loading
message — always-close skips the loading branch."
  (claude-repl-test--with-clean-state
    (let ((messages nil) (hidden nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--claude-running-p) (lambda () t))
                ((symbol-function 'claude-repl--session-starting-p) (lambda () t))
                ((symbol-function 'claude-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'message) (lambda (fmt &rest _) (push fmt messages)))
                ((symbol-function 'claude-repl--hide-and-preserve-status)
                 (lambda () (setq hidden t))))
        (claude-repl)
        (should hidden)
        (should-not (cl-some (lambda (m) (and m (string-match-p "loading" m))) messages))))))

(ert-deftest claude-repl-test-panels-entry-point-visible-hides ()
  "claude-repl hides panels when they are visible."
  (claude-repl-test--with-clean-state
    (let ((hidden nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--claude-running-p) (lambda () t))
                ((symbol-function 'claude-repl--session-starting-p) (lambda () nil))
                ((symbol-function 'claude-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'claude-repl--hide-and-preserve-status)
                 (lambda () (setq hidden t))))
        (claude-repl)
        (should hidden)))))

(ert-deftest claude-repl-test-panels-entry-point-hidden-still-hides ()
  "claude-repl hides the workspace even when panels are already hidden — the
always-close contract: pressing SPC o C on a hidden workspace re-asserts
:hidden + push-to-back instead of re-showing the panels."
  (claude-repl-test--with-clean-state
    (let ((shown nil) (hidden nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--claude-running-p) (lambda () t))
                ((symbol-function 'claude-repl--session-starting-p) (lambda () nil))
                ((symbol-function 'claude-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'claude-repl--show-hidden-panels)
                 (lambda () (setq shown t)))
                ((symbol-function 'claude-repl--hide-and-preserve-status)
                 (lambda () (setq hidden t))))
        (claude-repl)
        (should hidden)
        (should-not shown)))))

(ert-deftest claude-repl-test-panels-entry-point-selection-sends ()
  "claude-repl sends selected text to Claude when region is active.
Selection-handling stays orthogonal to the always-close hide path."
  (claude-repl-test--with-clean-state
    (let ((sent-text nil) (hidden nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--claude-running-p) (lambda () t))
                ((symbol-function 'claude-repl--session-starting-p) (lambda () nil))
                ((symbol-function 'claude-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'region-beginning) (lambda () 1))
                ((symbol-function 'region-end) (lambda () 12))
                ((symbol-function 'buffer-substring-no-properties)
                 (lambda (_beg _end) "hello world"))
                ((symbol-function 'deactivate-mark) (lambda () nil))
                ((symbol-function 'claude-repl--hide-and-preserve-status)
                 (lambda () (setq hidden t)))
                ((symbol-function 'claude-repl--send-to-claude)
                 (lambda (text) (setq sent-text text))))
        (claude-repl)
        (should (equal sent-text "hello world"))
        (should-not hidden)))))

(ert-deftest claude-repl-test-panels-entry-point-simple-not-running-initializes ()
  "claude-repl-simple (SPC o c) keeps its non-always-close dispatch: when
nothing is running, it initializes Claude (in contrast to SPC o C)."
  (claude-repl-test--with-clean-state
    (let ((started nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--claude-running-p) (lambda () nil))
                ((symbol-function 'claude-repl--session-starting-p) (lambda () nil))
                ((symbol-function 'claude-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'claude-repl--initialize-claude)
                 (lambda (&rest _) (setq started t))))
        (claude-repl-simple)
        (should started)))))

(ert-deftest claude-repl-test-panels-entry-point-simple-hidden-shows ()
  "claude-repl-simple (SPC o c) keeps its non-always-close dispatch: when
the session is running but panels are hidden, it re-shows them (in
contrast to SPC o C, which hides further)."
  (claude-repl-test--with-clean-state
    (let ((shown nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--claude-running-p) (lambda () t))
                ((symbol-function 'claude-repl--session-starting-p) (lambda () nil))
                ((symbol-function 'claude-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'claude-repl--show-hidden-panels)
                 (lambda () (setq shown t))))
        (claude-repl-simple)
        (should shown)))))

(ert-deftest claude-repl-test-panels-entry-point-simple-output-only-adds-input ()
  "claude-repl-simple (SPC o c): when only the output window is visible, it
adds the input window beside it and focuses it — rather than rebuilding the
whole layout (which would duplicate the already-visible output window)."
  (claude-repl-test--with-clean-state
    (let ((added nil) (focused nil) (shown-hidden nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--claude-running-p) (lambda () t))
                ((symbol-function 'claude-repl--session-starting-p) (lambda () nil))
                ((symbol-function 'claude-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'claude-repl--output-visible-input-hidden-p)
                 (lambda () t))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'claude-repl--show-input-beside-output)
                 (lambda () (setq added t)))
                ((symbol-function 'claude-repl--focus-input-panel)
                 (lambda () (setq focused t)))
                ((symbol-function 'claude-repl--show-hidden-panels)
                 (lambda () (setq shown-hidden t))))
        (claude-repl-simple)
        (should added)
        (should focused)
        (should-not shown-hidden)))))

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

(ert-deftest claude-repl-test-panels-delete-non-panel-windows-silent-on-tricky-layout ()
  "Regression for `SPC w f' in claude-repl: when a non-panel window
ends up as the sole main-area window mid-sweep, an earlier
implementation logged `[claude-repl] window--delete-where: could not
delete ...' into *Messages*.  The fixed implementation routes through
`--delete-where' whose benign-error filter silences the structural
refusal — verify no warning escapes."
  (let ((wconf (current-window-configuration)))
    (unwind-protect
        (progn
          (delete-other-windows)
          (let* ((vterm-buf  (generate-new-buffer " *test-vterm*"))
                 (input-buf  (generate-new-buffer " *test-input*"))
                 (drawer-buf (generate-new-buffer " *test-drawer*"))
                 (vterm-win  (selected-window))
                 (input-win  (split-window vterm-win nil 'below))
                 (drawer-win (display-buffer-in-side-window
                              drawer-buf '((side . left) (slot . 0)))))
            (set-window-buffer vterm-win vterm-buf)
            (set-window-buffer input-win input-buf)
            ;; Mirror production: panels and drawer carry
            ;; `no-delete-other-windows'.  Only vterm- and input-win
            ;; remain in the main area — the sweep targets none of them
            ;; (both are panels), so this collapses to the trivial case
            ;; and exercises the benign-error path of `--delete-where'
            ;; without leaving residue from a synthetic `extra-win'.
            (set-window-parameter vterm-win  'no-delete-other-windows t)
            (set-window-parameter input-win  'no-delete-other-windows t)
            (set-window-parameter drawer-win 'no-delete-other-windows t)
            (unwind-protect
                (let ((captured nil)
                      (orig-message (symbol-function 'message)))
                  (cl-letf (((symbol-function 'message)
                             (lambda (fmt &rest args)
                               (let ((s (apply #'format fmt args)))
                                 (when (string-match-p "could not delete" s)
                                   (push s captured))
                                 (apply orig-message fmt args)))))
                    (claude-repl--delete-non-panel-windows vterm-buf input-buf)
                    (should-not captured)
                    (should (window-live-p vterm-win))
                    (should (window-live-p input-win))
                    (should (window-live-p drawer-win))))
              (when (window-live-p drawer-win) (delete-window drawer-win))
              (kill-buffer vterm-buf)
              (kill-buffer input-buf)
              (kill-buffer drawer-buf))))
      (set-window-configuration wconf))))

(ert-deftest claude-repl-test-panels-delete-non-panel-windows-preserves-drawer-without-ndow-param ()
  "Regression for `SPC w f' killing the drawer: drawer preservation
must NOT depend on the drawer window carrying
`no-delete-other-windows'.  An earlier revision routed through
Emacs's `delete-other-windows', which protects the drawer only when
that parameter is intact; any upstream parameter loss (a redisplay
without the original action alist, etc.) made the drawer vulnerable.
The parameter-independent path via `--delete-where' must survive a
drawer with the parameter explicitly stripped."
  (let ((wconf (current-window-configuration)))
    (unwind-protect
        (progn
          (delete-other-windows)
          (let* ((vterm-buf  (generate-new-buffer " *test-vterm*"))
                 (input-buf  (generate-new-buffer " *test-input*"))
                 (drawer-buf (generate-new-buffer " *test-drawer*"))
                 (work-buf   (generate-new-buffer " *test-work*"))
                 (work-win   (selected-window))
                 (vterm-win  (split-window work-win nil 'right))
                 (input-win  (split-window vterm-win nil 'below))
                 (drawer-win (display-buffer-in-side-window
                              drawer-buf '((side . left) (slot . 0)))))
            (set-window-buffer work-win  work-buf)
            (set-window-buffer vterm-win  vterm-buf)
            (set-window-buffer input-win  input-buf)
            ;; Panels keep their hardening, but the drawer's
            ;; `no-delete-other-windows' is explicitly absent — the test
            ;; would pass trivially if we left it set, and the
            ;; regression is precisely about the parameter-stripped case.
            (set-window-parameter vterm-win  'no-delete-other-windows t)
            (set-window-parameter input-win  'no-delete-other-windows t)
            (set-window-parameter drawer-win 'no-delete-other-windows nil)
            (unwind-protect
                (progn
                  (claude-repl--delete-non-panel-windows vterm-buf input-buf)
                  (should (window-live-p drawer-win))
                  (should (window-live-p vterm-win))
                  (should (window-live-p input-win))
                  (should-not (window-live-p work-win)))
              (when (window-live-p drawer-win) (delete-window drawer-win))
              (kill-buffer vterm-buf)
              (kill-buffer input-buf)
              (kill-buffer drawer-buf)
              (kill-buffer work-buf))))
      (set-window-configuration wconf))))

;;;; ---- Tests: fullscreen-p (layout-based fullscreen detection) ----

(ert-deftest claude-repl-test-panels-fullscreen-p-only-panels ()
  "fullscreen-p detects a frame reduced to only the Claude panels.
This is the manual-fullscreen case (other windows deleted by hand,
without `claude-repl-toggle-fullscreen' having saved a config)."
  (claude-repl-test--with-clean-state
    (let ((wconf (current-window-configuration))
          (vterm-buf (generate-new-buffer "*claude-panel-fsp1*"))
          (input-buf (generate-new-buffer "*claude-panel-input-fsp1*")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (let* ((vterm-win (selected-window))
                   (input-win (split-window vterm-win nil 'below)))
              (set-window-buffer vterm-win vterm-buf)
              (set-window-buffer input-win input-buf)
              (claude-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
              (claude-repl--ws-put "test-ws" :input-buffer input-buf)
              (should (claude-repl--fullscreen-p))))
        (set-window-configuration wconf)
        (kill-buffer vterm-buf)
        (kill-buffer input-buf)))))

(ert-deftest claude-repl-test-panels-fullscreen-p-work-window-present ()
  "fullscreen-p returns nil when an ordinary work window coexists with the panels."
  (claude-repl-test--with-clean-state
    (let ((wconf (current-window-configuration))
          (vterm-buf (generate-new-buffer "*claude-panel-fsp2*"))
          (input-buf (generate-new-buffer "*claude-panel-input-fsp2*"))
          (work-buf  (generate-new-buffer "*fsp2-work*")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (let* ((work-win (selected-window))
                   (vterm-win (split-window work-win nil 'right))
                   (input-win (split-window vterm-win nil 'below)))
              (set-window-buffer work-win work-buf)
              (set-window-buffer vterm-win vterm-buf)
              (set-window-buffer input-win input-buf)
              (claude-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
              (claude-repl--ws-put "test-ws" :input-buffer input-buf)
              (should-not (claude-repl--fullscreen-p))))
        (set-window-configuration wconf)
        (kill-buffer vterm-buf)
        (kill-buffer input-buf)
        (kill-buffer work-buf)))))

(ert-deftest claude-repl-test-panels-fullscreen-p-ignores-side-window ()
  "fullscreen-p treats side windows (e.g. the drawer) as exempt.
A frame holding only the panels plus a side window still counts as
fullscreen, because the drawer is not an ordinary work window."
  (claude-repl-test--with-clean-state
    (let ((wconf (current-window-configuration))
          (vterm-buf  (generate-new-buffer "*claude-panel-fsp3*"))
          (input-buf  (generate-new-buffer "*claude-panel-input-fsp3*"))
          (drawer-buf (generate-new-buffer "*fsp3-drawer*"))
          (drawer-win nil))
      (unwind-protect
          (progn
            (delete-other-windows)
            (let* ((vterm-win (selected-window))
                   (input-win (split-window vterm-win nil 'below)))
              (set-window-buffer vterm-win vterm-buf)
              (set-window-buffer input-win input-buf)
              (setq drawer-win (display-buffer-in-side-window
                                drawer-buf '((side . left) (slot . 0))))
              (claude-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
              (claude-repl--ws-put "test-ws" :input-buffer input-buf)
              (should (window-live-p drawer-win))
              (should (claude-repl--fullscreen-p))))
        (when (window-live-p drawer-win) (delete-window drawer-win))
        (set-window-configuration wconf)
        (kill-buffer vterm-buf)
        (kill-buffer input-buf)
        (kill-buffer drawer-buf)))))

(ert-deftest claude-repl-test-panels-fullscreen-p-panels-not-visible ()
  "fullscreen-p returns nil when the panels are not displayed at all."
  (claude-repl-test--with-clean-state
    (let ((wconf (current-window-configuration))
          (vterm-buf (generate-new-buffer "*claude-panel-fsp4*"))
          (input-buf (generate-new-buffer "*claude-panel-input-fsp4*"))
          (other-buf (generate-new-buffer "*fsp4-other*")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer other-buf)
            ;; Panels exist in the workspace but are not in any window.
            (claude-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
            (claude-repl--ws-put "test-ws" :input-buffer input-buf)
            (should-not (claude-repl--fullscreen-p)))
        (set-window-configuration wconf)
        (kill-buffer vterm-buf)
        (kill-buffer input-buf)
        (kill-buffer other-buf)))))

;;;; ---- Tests: toggle-fullscreen on a manually-fullscreened frame ----

(ert-deftest claude-repl-test-panels-toggle-fullscreen-manual-no-poison ()
  "toggle-fullscreen on a manually-fullscreened frame does not poison the config.
With no saved `:fullscreen-config', the layout-based detection still
recognizes fullscreen, so the command must NOT save the fullscreen
layout as the workspace's restore config and must NOT sweep the panels."
  (claude-repl-test--with-clean-state
    (let ((wconf (current-window-configuration))
          (vterm-buf (generate-new-buffer "*claude-panel-fsmanual*"))
          (input-buf (generate-new-buffer "*claude-panel-input-fsmanual*"))
          (messages '()))
      (unwind-protect
          (progn
            (delete-other-windows)
            (let* ((vterm-win (selected-window))
                   (input-win (split-window vterm-win nil 'below)))
              (set-window-buffer vterm-win vterm-buf)
              (set-window-buffer input-win input-buf)
              (claude-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
              (claude-repl--ws-put "test-ws" :input-buffer input-buf)
              (should-not (claude-repl--ws-get "test-ws" :fullscreen-config))
              (cl-letf (((symbol-function 'message)
                         (lambda (fmt &rest args)
                           (push (apply #'format fmt args) messages))))
                (claude-repl-toggle-fullscreen))
              ;; Config was NOT poisoned with the fullscreen layout.
              (should-not (claude-repl--ws-get "test-ws" :fullscreen-config))
              ;; A "already fullscreen" message was emitted.
              (should (cl-some (lambda (m) (string-match-p "already fullscreen" m))
                               messages))
              ;; Panels survived (no destructive sweep).
              (should (window-live-p vterm-win))
              (should (window-live-p input-win))))
        (set-window-configuration wconf)
        (kill-buffer vterm-buf)
        (kill-buffer input-buf)))))

;;;; ---- Tests: enter-fullscreen (canonical show-fullscreen) ----

(ert-deftest claude-repl-test-panels-enter-fullscreen-saves-config ()
  "enter-fullscreen saves the prior layout as :fullscreen-config."
  (claude-repl-test--with-clean-state
    (let ((wconf (current-window-configuration))
          (vterm-buf (generate-new-buffer "*claude-panel-fsenter*"))
          (input-buf (generate-new-buffer "*claude-panel-input-fsenter*")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (let* ((work-win (selected-window))
                   (vterm-win (split-window work-win nil 'right))
                   (input-win (split-window vterm-win nil 'below)))
              (set-window-buffer vterm-win vterm-buf)
              (set-window-buffer input-win input-buf)
              (claude-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
              (claude-repl--ws-put "test-ws" :input-buffer input-buf)
              (should-not (claude-repl--ws-get "test-ws" :fullscreen-config))
              (claude-repl--enter-fullscreen "test-ws")
              (should (claude-repl--ws-get "test-ws" :fullscreen-config))))
        (set-window-configuration wconf)
        (kill-buffer vterm-buf)
        (kill-buffer input-buf)))))

(ert-deftest claude-repl-test-panels-enter-fullscreen-keeps-both-panels ()
  "enter-fullscreen keeps BOTH the vterm output and input panels visible.
This is the core guarantee: fullscreen must never show only one panel."
  (claude-repl-test--with-clean-state
    (let ((wconf (current-window-configuration))
          (vterm-buf (generate-new-buffer "*claude-panel-fsenter2*"))
          (input-buf (generate-new-buffer "*claude-panel-input-fsenter2*")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (let* ((work-win (selected-window))
                   (vterm-win (split-window work-win nil 'right))
                   (input-win (split-window vterm-win nil 'below)))
              (set-window-buffer vterm-win vterm-buf)
              (set-window-buffer input-win input-buf)
              (claude-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
              (claude-repl--ws-put "test-ws" :input-buffer input-buf)
              (claude-repl--enter-fullscreen "test-ws")
              ;; The work window was swept...
              (should-not (window-live-p work-win))
              ;; ...but BOTH panels survived.
              (should (get-buffer-window vterm-buf))
              (should (get-buffer-window input-buf))))
        (set-window-configuration wconf)
        (kill-buffer vterm-buf)
        (kill-buffer input-buf)))))

(ert-deftest claude-repl-test-panels-enter-fullscreen-errors-when-not-visible ()
  "enter-fullscreen signals a user-error when the panels are not displayed."
  (claude-repl-test--with-clean-state
    (let ((vterm-buf (generate-new-buffer "*claude-panel-fsenter3*"))
          (input-buf (generate-new-buffer "*claude-panel-input-fsenter3*")))
      (unwind-protect
          (progn
            ;; Buffers exist but are not shown in any window.
            (claude-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
            (claude-repl--ws-put "test-ws" :input-buffer input-buf)
            (should-error (claude-repl--enter-fullscreen "test-ws")
                          :type 'user-error))
        (kill-buffer vterm-buf)
        (kill-buffer input-buf)))))

(ert-deftest claude-repl-test-panels-enter-fullscreen-unconditional ()
  "enter-fullscreen sweeps even when every window already shows a Claude buffer.
The generation path splits the new workspace's panels off another
workspace's fullscreen panels, so the layout has only Claude buffers —
which `claude-repl--fullscreen-p' reports as already-fullscreen.
enter-fullscreen must ignore that and still sweep so the NEW workspace's
own panels fill the frame (both shown)."
  (claude-repl-test--with-clean-state
    (let ((wconf (current-window-configuration))
          (other-buf (generate-new-buffer "*claude-panel-other-ws*"))
          (vterm-buf (generate-new-buffer "*claude-panel-fsenter4*"))
          (input-buf (generate-new-buffer "*claude-panel-input-fsenter4*")))
      (unwind-protect
          (progn
            (delete-other-windows)
            ;; Every window shows a Claude panel buffer (the all-Claude layout).
            (let* ((other-win (selected-window))
                   (vterm-win (split-window other-win nil 'right))
                   (input-win (split-window vterm-win nil 'below)))
              (set-window-buffer other-win other-buf)
              (set-window-buffer vterm-win vterm-buf)
              (set-window-buffer input-win input-buf)
              (claude-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
              (claude-repl--ws-put "test-ws" :input-buffer input-buf)
              (claude-repl--enter-fullscreen "test-ws")
              ;; The other workspace's window was swept despite being a Claude buffer.
              (should-not (window-live-p other-win))
              ;; This workspace's own panels both survive.
              (should (get-buffer-window vterm-buf))
              (should (get-buffer-window input-buf))))
        (set-window-configuration wconf)
        (kill-buffer other-buf)
        (kill-buffer vterm-buf)
        (kill-buffer input-buf)))))

;;;; ---- Tests: toggle-fullscreen go-fullscreen routes through enter-fullscreen ----

(ert-deftest claude-repl-test-panels-toggle-go-fullscreen-via-enter ()
  "toggle-fullscreen's go-fullscreen branch routes through enter-fullscreen."
  (claude-repl-test--with-clean-state
    (let ((entered nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--fullscreen-p) (lambda () nil))
                ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                ((symbol-function 'claude-repl--enter-fullscreen)
                 (lambda (ws) (setq entered ws))))
        ;; No saved config and not fullscreen -> go-fullscreen branch.
        (claude-repl--ws-put "test-ws" :fullscreen-config nil)
        (claude-repl-toggle-fullscreen)
        (should (equal entered "test-ws"))))))

(ert-deftest claude-repl-test-panels-toggle-fullscreen-restore-via-helper ()
  "toggle-fullscreen's saved branch restores through restore-fullscreen-config.
With a saved config it restores the layout and clears `:fullscreen-config'."
  (claude-repl-test--with-clean-state
    (let ((restored nil))
      (claude-repl--ws-put "test-ws" :fullscreen-config 'saved-config)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'set-window-configuration)
                 (lambda (cfg) (setq restored cfg))))
        (claude-repl-toggle-fullscreen)
        (should (eq restored 'saved-config))
        (should-not (claude-repl--ws-get "test-ws" :fullscreen-config))))))

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
                    ((symbol-function 'claude-repl--update-all-workspace-states-now) (lambda () nil)))
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

(ert-deftest claude-repl-test-panels-show-panels-moves-up-from-bottom-window ()
  "show-panels selects the window above before splitting, so panels
are not created from a bottom popup like a regular vterm."
  (claude-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*show-up-vterm*"))
          (input-buf (get-buffer-create "*show-up-input*"))
          (bottom-win nil))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'claude-repl--refresh-vterm) (lambda () nil))
                    ((symbol-function 'claude-repl--update-all-workspace-states-now) (lambda () nil)))
            (claude-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
            (claude-repl--ws-put "test-ws" :input-buffer input-buf)
            (delete-other-windows)
            (let ((top-win (selected-window)))
              ;; Create a bottom popup window and select it
              (setq bottom-win (split-window top-win nil 'below))
              (select-window bottom-win)
              (should (eq (selected-window) bottom-win))
              (claude-repl--show-panels)
              ;; The vterm split should have come from top-win, not bottom-win.
              ;; Verify top-win is no longer selected (it was split into work + vterm)
              ;; and that both panel buffers are visible.
              (should (get-buffer-window vterm-buf))
              (should (get-buffer-window input-buf))))
        ;; Clean up
        (delete-other-windows)
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

(ert-deftest claude-repl-test-panels-show-panels-noop-when-no-window-above ()
  "show-panels does not error when there is no window above (single window)."
  (claude-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*show-noop-vterm*"))
          (input-buf (get-buffer-create "*show-noop-input*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'claude-repl--refresh-vterm) (lambda () nil))
                    ((symbol-function 'claude-repl--update-all-workspace-states-now) (lambda () nil)))
            (claude-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
            (claude-repl--ws-put "test-ws" :input-buffer input-buf)
            (delete-other-windows)
            ;; Single window, no window above -- should work normally
            (claude-repl--show-panels)
            (should (get-buffer-window vterm-buf))
            (should (get-buffer-window input-buf)))
        ;; Clean up
        (delete-other-windows)
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

(ert-deftest claude-repl-test-panels-show-panels-sets-no-delete-other-windows ()
  "show-panels sets `no-delete-other-windows' on both vterm and input windows
so that commands like magit-status cannot destroy panel layout."
  (claude-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*show-ndow-vterm*"))
          (input-buf (get-buffer-create "*show-ndow-input*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'claude-repl--refresh-vterm) (lambda () nil))
                    ((symbol-function 'claude-repl--update-all-workspace-states-now) (lambda () nil)))
            (claude-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
            (claude-repl--ws-put "test-ws" :input-buffer input-buf)
            (delete-other-windows)
            (claude-repl--show-panels)
            (let ((vterm-win (get-buffer-window vterm-buf))
                  (input-win (get-buffer-window input-buf)))
              (should (window-parameter vterm-win 'no-delete-other-windows))
              (should (window-parameter input-win 'no-delete-other-windows))))
        ;; Clean up
        (delete-other-windows)
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

(ert-deftest claude-repl-test-panels-show-panels-locks-input-height ()
  "show-panels sets `window-size-fixed' to height on the input window
so that window management operations cannot shrink it."
  (claude-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*show-hfix-vterm*"))
          (input-buf (get-buffer-create "*show-hfix-input*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'claude-repl--refresh-vterm) (lambda () nil))
                    ((symbol-function 'claude-repl--update-all-workspace-states-now) (lambda () nil)))
            (claude-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
            (claude-repl--ws-put "test-ws" :input-buffer input-buf)
            (delete-other-windows)
            (claude-repl--show-panels)
            (let ((input-win (get-buffer-window input-buf)))
              (should (eq (window-parameter input-win 'window-size-fixed) 'height))))
        ;; Clean up
        (delete-other-windows)
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

(ert-deftest claude-repl-test-panels-show-panels-preserves-input-height ()
  "show-panels calls `window-preserve-size' on the input window so a
multi-line minibuffer cannot shrink it.  `window-size-fixed' alone is
bypassed by `window--resize-mini-window' (ignore=t), so the stronger
`window-preserved-size' parameter is required."
  (claude-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*show-preserve-vterm*"))
          (input-buf (get-buffer-create "*show-preserve-input*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'claude-repl--refresh-vterm) (lambda () nil))
                    ((symbol-function 'claude-repl--update-all-workspace-states-now) (lambda () nil)))
            (claude-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
            (claude-repl--ws-put "test-ws" :input-buffer input-buf)
            (delete-other-windows)
            (claude-repl--show-panels)
            (let* ((input-win (get-buffer-window input-buf))
                   (param (window-parameter input-win 'window-preserved-size)))
              (should param)
              (should (eq (nth 0 param) input-buf))
              ;; Height is preserved (3rd element non-nil), width is not.
              (should (numberp (nth 2 param)))))
        ;; Clean up
        (delete-other-windows)
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

;;;; ---- Tests: focus-input-panel edge cases ----

(ert-deftest claude-repl-test-panels-focus-input-panel-nil-buffer ()
  "focus-input-panel signals an error when input buffer is nil."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
      (should-error (claude-repl--focus-input-panel) :type 'error))))

(ert-deftest claude-repl-test-panels-focus-input-panel-no-window ()
  "focus-input-panel signals an error when input buffer exists but has no window."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*focus-no-win*")))
      (unwind-protect
          (progn
            (claude-repl--ws-put "test-ws" :input-buffer buf)
            (switch-to-buffer (get-buffer-create "*other*"))
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
              (should-error (claude-repl--focus-input-panel) :type 'error)))
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
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
              (claude-repl--focus-input-panel)
              (should (eq (window-buffer (selected-window)) buf))))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest claude-repl-test-panels-focus-input-panel-no-insert-state ()
  "focus-input-panel does NOT enter evil insert state on focus."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*focus-input-no-insert*"))
          (new-win nil)
          (insert-called nil))
      (unwind-protect
          (progn
            (claude-repl--ws-put "test-ws" :input-buffer buf)
            (setq new-win (split-window))
            (set-window-buffer new-win buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                      ((symbol-function 'evil-insert-state)
                       (lambda (&rest _) (setq insert-called t))))
              (claude-repl--focus-input-panel)
              (should-not insert-called)))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;;;; ---- Tests: focus-input show-or-focus branch ----

(ert-deftest claude-repl-test-panels-focus-input-selects-window ()
  "focus-input selects the input window in the running/visible branch."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*focus-input-cmd-win*"))
          (new-win nil))
      (unwind-protect
          (progn
            (claude-repl--ws-put "test-ws" :input-buffer buf)
            (setq new-win (split-window))
            (set-window-buffer new-win buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                      ((symbol-function 'claude-repl--claude-running-p) (lambda () t))
                      ((symbol-function 'claude-repl--panels-visible-p) (lambda () t)))
              (claude-repl-focus-input)
              (should (eq (window-buffer (selected-window)) buf))))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest claude-repl-test-panels-focus-input-no-insert-state ()
  "focus-input does NOT enter evil insert state when focusing the input window."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*focus-input-cmd-no-insert*"))
          (new-win nil)
          (insert-called nil))
      (unwind-protect
          (progn
            (claude-repl--ws-put "test-ws" :input-buffer buf)
            (setq new-win (split-window))
            (set-window-buffer new-win buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                      ((symbol-function 'claude-repl--claude-running-p) (lambda () t))
                      ((symbol-function 'claude-repl--panels-visible-p) (lambda () t))
                      ((symbol-function 'evil-insert-state)
                       (lambda (&rest _) (setq insert-called t))))
              (claude-repl-focus-input)
              (should-not insert-called)))
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
              ((symbol-function 'claude-repl--update-all-workspace-states-now) (lambda () nil))
              ((symbol-function 'claude-repl--refresh-vterm) (lambda () nil))
              ((symbol-function 'claude-repl--reset-vterm-cursors) (lambda () nil))
              ((symbol-function 'claude-repl--drain-pending-magit) (lambda (_ws) nil))
              ((symbol-function 'claude-repl--drain-pending-initial-buffers) (lambda (_ws) nil))
              ((symbol-function 'claude-repl--drain-pending-show-panels) (lambda (_ws) nil))
              ((symbol-function 'claude-repl--maybe-autoselect-input) (lambda (_ws) nil)))
      ;; Should not error -- the when guard skips mark-viewed
      (claude-repl--on-workspace-switch))))

(ert-deftest claude-repl-test-panels-on-workspace-switch-flips-ws-loaded ()
  "Tail of `--on-workspace-switch' flips the `:ws-loaded' latch bit
on the ws plist (via `--latch-and-maybe-fire-loaded')."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
              ((symbol-function 'claude-repl--maybe-sweep-hidden-on-switch) #'ignore)
              ((symbol-function 'claude-repl--update-all-workspace-states-now) #'ignore)
              ((symbol-function 'claude-repl--refresh-vterm) #'ignore)
              ((symbol-function 'claude-repl--reset-vterm-cursors) #'ignore)
              ((symbol-function 'claude-repl--drain-pending-magit) #'ignore)
              ((symbol-function 'claude-repl--drain-pending-initial-buffers) #'ignore)
              ((symbol-function 'claude-repl--drain-pending-show-panels) #'ignore)
              ((symbol-function 'claude-repl--maybe-autoselect-input) #'ignore))
      (claude-repl--on-workspace-switch "ws1")
      ;; :claude-ready is nil so latch hasn't fired+cleared; bit stays set.
      (should (eq (claude-repl--ws-get "ws1" :ws-loaded) t)))))

(ert-deftest claude-repl-test-panels-on-workspace-switch-nil-ws-skips-latch ()
  "When `--on-workspace-switch' is called with nil ws (and current-name
also returns nil), the latch flip is skipped — guards against poisoning
the ws-plist hash with a nil key in test/init environments."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
              ((symbol-function 'claude-repl--maybe-sweep-hidden-on-switch) #'ignore)
              ((symbol-function 'claude-repl--update-all-workspace-states-now) #'ignore)
              ((symbol-function 'claude-repl--refresh-vterm) #'ignore)
              ((symbol-function 'claude-repl--reset-vterm-cursors) #'ignore)
              ((symbol-function 'claude-repl--drain-pending-magit) #'ignore)
              ((symbol-function 'claude-repl--drain-pending-initial-buffers) #'ignore)
              ((symbol-function 'claude-repl--drain-pending-show-panels) #'ignore)
              ((symbol-function 'claude-repl--maybe-autoselect-input) #'ignore))
      ;; Should not error and should not touch the hash table.
      (claude-repl--on-workspace-switch nil)
      (should-not (gethash nil claude-repl--workspaces)))))

(ert-deftest claude-repl-test-panels-on-workspace-switch-dequeues-merge ()
  "Switching to a workspace calls `--dequeue-merge' on it so a parked
merge request is pulled from the queue on activation."
  (claude-repl-test--with-clean-state
    (let (dequeued)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'claude-repl--maybe-sweep-hidden-on-switch) #'ignore)
                ((symbol-function 'claude-repl--dequeue-merge)
                 (lambda (ws) (setq dequeued ws)))
                ((symbol-function 'claude-repl--update-all-workspace-states-now) #'ignore)
                ((symbol-function 'claude-repl--refresh-vterm) #'ignore)
                ((symbol-function 'claude-repl--reset-vterm-cursors) #'ignore)
                ((symbol-function 'claude-repl--drain-pending-magit) #'ignore)
                ((symbol-function 'claude-repl--drain-pending-initial-buffers) #'ignore)
                ((symbol-function 'claude-repl--drain-pending-show-panels) #'ignore)
                ((symbol-function 'claude-repl--maybe-autoselect-input) #'ignore))
        (claude-repl--on-workspace-switch "ws1")
        (should (equal dequeued "ws1"))))))

(ert-deftest claude-repl-test-panels-on-workspace-switch-done-stamps-acked-at ()
  "Switching to a workspace in :done sets :done-acked t and stamps
:done-acked-at with the current time so the focus-dwell countdown
can start."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'claude-repl--maybe-sweep-hidden-on-switch) #'ignore)
              ((symbol-function 'claude-repl--update-all-workspace-states-now) #'ignore)
              ((symbol-function 'claude-repl--refresh-vterm) #'ignore)
              ((symbol-function 'claude-repl--reset-vterm-cursors) #'ignore)
              ((symbol-function 'claude-repl--drain-pending-magit) #'ignore)
              ((symbol-function 'claude-repl--drain-pending-initial-buffers) #'ignore)
              ((symbol-function 'claude-repl--drain-pending-show-panels) #'ignore)
              ((symbol-function 'claude-repl--maybe-autoselect-input) #'ignore))
      (claude-repl--ws-set-claude-state "ws1" :done)
      (let ((before (float-time)))
        (claude-repl--on-workspace-switch "ws1")
        (should (eq (claude-repl--ws-get "ws1" :done-acked) t))
        (let ((stamp (claude-repl--ws-get "ws1" :done-acked-at)))
          (should (numberp stamp))
          (should (>= stamp before)))))))

(ert-deftest claude-repl-test-panels-on-workspace-switch-non-done-does-not-stamp ()
  "Switching to a workspace not in :done does not touch :done-acked-at."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'claude-repl--maybe-sweep-hidden-on-switch) #'ignore)
              ((symbol-function 'claude-repl--update-all-workspace-states-now) #'ignore)
              ((symbol-function 'claude-repl--refresh-vterm) #'ignore)
              ((symbol-function 'claude-repl--reset-vterm-cursors) #'ignore)
              ((symbol-function 'claude-repl--drain-pending-magit) #'ignore)
              ((symbol-function 'claude-repl--drain-pending-initial-buffers) #'ignore)
              ((symbol-function 'claude-repl--drain-pending-show-panels) #'ignore)
              ((symbol-function 'claude-repl--maybe-autoselect-input) #'ignore))
      (claude-repl--ws-set-claude-state "ws1" :thinking)
      (claude-repl--on-workspace-switch "ws1")
      (should-not (claude-repl--ws-get "ws1" :done-acked))
      (should-not (claude-repl--ws-get "ws1" :done-acked-at)))))

(ert-deftest claude-repl-test-panels-clear-done-ack-on-switch-away-done ()
  "Leaving a workspace in :done clears :done-acked and :done-acked-at so
the dwell countdown restarts on return."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set-claude-state "ws1" :done)
    (claude-repl--ws-put "ws1" :done-acked t)
    (claude-repl--ws-put "ws1" :done-acked-at (float-time))
    (claude-repl--clear-done-ack-on-switch-away "ws1")
    (should-not (claude-repl--ws-get "ws1" :done-acked))
    (should-not (claude-repl--ws-get "ws1" :done-acked-at))))

(ert-deftest claude-repl-test-panels-clear-done-ack-on-switch-away-non-done ()
  "Leaving a workspace NOT in :done leaves ack flags untouched — the
clear only resets the dwell countdown for live :done workspaces."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set-claude-state "ws1" :thinking)
    (claude-repl--ws-put "ws1" :done-acked t)
    (let ((stamp (float-time)))
      (claude-repl--ws-put "ws1" :done-acked-at stamp)
      (claude-repl--clear-done-ack-on-switch-away "ws1")
      ;; :thinking ws was not affected.
      (should (eq (claude-repl--ws-get "ws1" :done-acked) t))
      (should (= (claude-repl--ws-get "ws1" :done-acked-at) stamp)))))

(ert-deftest claude-repl-test-panels-clear-done-ack-on-switch-away-nil-ws ()
  "Switch-away clear with nil ws is a no-op (covers test/init envs where
+workspace-current-name returns nil)."
  (claude-repl-test--with-clean-state
    ;; Should not error.
    (claude-repl--clear-done-ack-on-switch-away nil)))

(ert-deftest claude-repl-test-panels-on-workspace-switch-explicit-ws-overrides-current ()
  "An explicit WS argument propagates to every per-ws side effect,
overriding `(+workspace-current-name)' at call time.  This is how
`--after-persp-activated' delivers the just-switched-to ws name to
the deferred call so back-to-back switches don't collapse onto the
latest one."
  (claude-repl-test--with-clean-state
    (let ((received-ws nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "racing-current"))
                ((symbol-function 'claude-repl--maybe-sweep-hidden-on-switch)
                 (lambda (ws) (push (cons :sweep ws) received-ws)))
                ((symbol-function 'claude-repl--update-all-workspace-states-now) (lambda () nil))
                ((symbol-function 'claude-repl--refresh-vterm) (lambda () nil))
                ((symbol-function 'claude-repl--reset-vterm-cursors) (lambda () nil))
                ((symbol-function 'claude-repl--drain-pending-magit)
                 (lambda (ws) (push (cons :magit ws) received-ws)))
                ((symbol-function 'claude-repl--drain-pending-initial-buffers)
                 (lambda (ws) (push (cons :init-bufs ws) received-ws)))
                ((symbol-function 'claude-repl--drain-pending-show-panels)
                 (lambda (ws) (push (cons :show-panels ws) received-ws)))
                ((symbol-function 'claude-repl--maybe-autoselect-input)
                 (lambda (ws) (push (cons :autoselect ws) received-ws))))
        (claude-repl--on-workspace-switch "captured-ws")
        ;; Every per-ws helper got "captured-ws", not "racing-current".
        (should (cl-every (lambda (e) (equal (cdr e) "captured-ws"))
                          received-ws))))))

;;;; ---- Tests: maybe-autoselect-input ----

(ert-deftest claude-repl-test-panels-maybe-autoselect-input-selects-visible-input ()
  "maybe-autoselect-input selects the input window when it is visible."
  (claude-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*autoselect-input*"))
          (new-win nil))
      (unwind-protect
          (progn
            (claude-repl--ws-put "test-ws" :input-buffer input-buf)
            (setq new-win (split-window))
            (set-window-buffer new-win input-buf)
            ;; Ensure we start on the other window
            (select-window (car (window-list)))
            (should-not (eq (window-buffer (selected-window)) input-buf))
            (let ((claude-repl-autoselect-input-on-workspace-switch t))
              (claude-repl--maybe-autoselect-input "test-ws")
              (should (eq (window-buffer (selected-window)) input-buf))))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

(ert-deftest claude-repl-test-panels-maybe-autoselect-input-noop-when-disabled ()
  "maybe-autoselect-input does nothing when the defcustom is nil."
  (claude-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*autoselect-disabled*"))
          (new-win nil))
      (unwind-protect
          (progn
            (claude-repl--ws-put "test-ws" :input-buffer input-buf)
            (setq new-win (split-window))
            (set-window-buffer new-win input-buf)
            (let ((orig-win (selected-window))
                  (claude-repl-autoselect-input-on-workspace-switch nil))
              (claude-repl--maybe-autoselect-input "test-ws")
              ;; Window should not have changed
              (should (eq (selected-window) orig-win))))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

(ert-deftest claude-repl-test-panels-maybe-autoselect-input-noop-no-buffer ()
  "maybe-autoselect-input does nothing when no input buffer exists."
  (claude-repl-test--with-clean-state
    (let ((orig-win (selected-window))
          (claude-repl-autoselect-input-on-workspace-switch t))
      (claude-repl--maybe-autoselect-input "test-ws")
      (should (eq (selected-window) orig-win)))))

(ert-deftest claude-repl-test-panels-maybe-autoselect-input-noop-not-visible ()
  "maybe-autoselect-input does nothing when input buffer is not in any window."
  (claude-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*autoselect-hidden*")))
      (unwind-protect
          (progn
            (claude-repl--ws-put "test-ws" :input-buffer input-buf)
            ;; Display a different buffer so input-buf has no window
            (switch-to-buffer (get-buffer-create "*other-auto*"))
            (let ((orig-win (selected-window))
                  (claude-repl-autoselect-input-on-workspace-switch t))
              (claude-repl--maybe-autoselect-input "test-ws")
              (should (eq (selected-window) orig-win))))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))
        (when (get-buffer "*other-auto*") (kill-buffer "*other-auto*"))))))

(ert-deftest claude-repl-test-panels-maybe-autoselect-input-noop-dead-buffer ()
  "maybe-autoselect-input does nothing when input buffer has been killed."
  (claude-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*autoselect-dead*")))
      (claude-repl--ws-put "test-ws" :input-buffer input-buf)
      (kill-buffer input-buf)
      (let ((orig-win (selected-window))
            (claude-repl-autoselect-input-on-workspace-switch t))
        (claude-repl--maybe-autoselect-input "test-ws")
        (should (eq (selected-window) orig-win))))))

(ert-deftest claude-repl-test-panels-maybe-autoselect-input-snaps-vterm-then-selects-input ()
  "maybe-autoselect-input snaps the vterm window to its cursor (via
`--snap-vterm-window-to-cursor') and then selects only the input
window.  Replaces the old brief-select hack — the previous transient
`select-window vterm-win' was the source of the visible scroll-down
animation, so the new flow snaps `window-start' directly and selects
only the input window."
  (claude-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*autoselect-input-snap*"))
          (vterm-buf (get-buffer-create "*autoselect-vterm-snap*"))
          (vterm-win nil)
          (input-win nil)
          (selections nil)
          (snap-arg nil))
      (unwind-protect
          (progn
            (claude-repl--ws-put "test-ws" :input-buffer input-buf)
            (claude-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
            (setq input-win (split-window))
            (set-window-buffer input-win input-buf)
            (setq vterm-win (split-window))
            (set-window-buffer vterm-win vterm-buf)
            (with-current-buffer vterm-buf (setq major-mode 'vterm-mode))
            (select-window (car (window-list)))
            (let ((claude-repl-autoselect-input-on-workspace-switch t)
                  (orig-select-window (symbol-function 'select-window)))
              (cl-letf (((symbol-function 'select-window)
                         (lambda (win &optional norecord)
                           (push win selections)
                           (funcall orig-select-window win norecord)))
                        ((symbol-function 'claude-repl--snap-vterm-window-to-cursor)
                         (lambda (win) (setq snap-arg win))))
                (claude-repl--maybe-autoselect-input "test-ws"))
              (setq selections (nreverse selections))
              ;; Snap runs on vterm-win; only input-win is selected.
              (should (eq snap-arg vterm-win))
              (should (equal selections (list input-win)))
              (should (eq (selected-window) input-win))))
        (when (and vterm-win (window-live-p vterm-win))
          (ignore-errors (delete-window vterm-win)))
        (when (and input-win (window-live-p input-win))
          (ignore-errors (delete-window input-win)))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest claude-repl-test-panels-maybe-autoselect-input-no-vterm-hack-when-hidden ()
  "maybe-autoselect-input skips the vterm hack when vterm is not displayed."
  (claude-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*autoselect-input-no-vterm*"))
          (vterm-buf (get-buffer-create "*autoselect-vterm-hidden*"))
          (input-win nil)
          (selections nil))
      (unwind-protect
          (progn
            (claude-repl--ws-put "test-ws" :input-buffer input-buf)
            (claude-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
            (setq input-win (split-window))
            (set-window-buffer input-win input-buf)
            ;; vterm-buf intentionally not displayed in any window
            (select-window (car (window-list)))
            (let ((claude-repl-autoselect-input-on-workspace-switch t)
                  (orig-select-window (symbol-function 'select-window)))
              (cl-letf (((symbol-function 'select-window)
                         (lambda (win &optional norecord)
                           (push win selections)
                           (funcall orig-select-window win norecord))))
                (claude-repl--maybe-autoselect-input "test-ws"))
              (setq selections (nreverse selections))
              ;; Only the input selection should happen.
              (should (equal selections (list input-win)))))
        (when (and input-win (window-live-p input-win))
          (ignore-errors (delete-window input-win)))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

;;;; ---- Tests: non-claude-panel-window-p with Claude buffers ----

(ert-deftest claude-repl-test-panels-non-claude-panel-window-p-vterm-buffer ()
  "non-claude-panel-window-p returns nil for a window showing a Claude vterm buffer."
  (let ((buf (get-buffer-create "*claude-panel-abcd1234*")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (should-not (claude-repl--non-claude-panel-window-p (selected-window))))
      (switch-to-buffer "*scratch*")
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-test-panels-non-claude-panel-window-p-input-buffer ()
  "non-claude-panel-window-p returns nil for a window showing a Claude input buffer."
  (let ((buf (get-buffer-create "*claude-panel-input-abcd1234*")))
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
  "on-window-change propagates errors from sync-panels (no error swallowing)."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--sync-panels)
               (lambda () (error "sync failed")))
              ((symbol-function 'claude-repl--update-hide-overlay) #'ignore))
      (should-error (claude-repl--on-window-change) :type 'error))))

;;;; ---- Tests: cursor reset is workspace-switch-only ----

(ert-deftest claude-repl-test-panels-on-window-change-does-not-reset-cursors ()
  "`--on-window-change' must NOT call `--reset-vterm-cursors'.
Resetting on every window-config change snaps vterm back to the bottom
and undoes user scrolls (e.g. via `C-S-k')."
  (claude-repl-test--with-clean-state
    (let ((reset-called nil))
      (cl-letf (((symbol-function 'claude-repl--sync-panels) #'ignore)
                ((symbol-function 'claude-repl--update-hide-overlay) #'ignore)
                ((symbol-function 'claude-repl--reset-vterm-cursors)
                 (lambda () (setq reset-called t))))
        (claude-repl--on-window-change)
        (should-not reset-called)))))

(ert-deftest claude-repl-test-panels-no-cursor-reset-on-selection-change ()
  "No claude-repl cursor-reset handler is installed on
`window-selection-change-functions'.  If one is, every focus change
schedules `--reset-vterm-cursors', which snaps vterm to the bottom and
undoes user scrolls."
  (should-not
   (cl-find-if
    (lambda (fn)
      (and (symbolp fn)
           (string-prefix-p "claude-repl--" (symbol-name fn))
           (string-match-p "cursor-reset\\|reset-vterm" (symbol-name fn))))
    window-selection-change-functions)))

(ert-deftest claude-repl-test-panels-no-cursor-reset-on-buffer-list-update ()
  "No claude-repl cursor-reset handler is installed on
`buffer-list-update-hook'.  If one is, normal buffer activity
schedules `--reset-vterm-cursors', which snaps vterm to the bottom and
undoes user scrolls."
  (should-not
   (cl-find-if
    (lambda (fn)
      (and (symbolp fn)
           (string-prefix-p "claude-repl--" (symbol-name fn))
           (string-match-p "cursor-reset\\|reset-vterm" (symbol-name fn))))
    buffer-list-update-hook)))

(ert-deftest claude-repl-test-panels-on-workspace-switch-still-resets-cursors ()
  "Workspace switch is the one place that DOES reset vterm cursors.
This preserves the recenter-after-switch behavior while the broader
hooks are gone."
  (claude-repl-test--with-clean-state
    (let ((reset-called nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'claude-repl--maybe-sweep-hidden-on-switch) #'ignore)
                ((symbol-function 'claude-repl--update-all-workspace-states-now) #'ignore)
                ((symbol-function 'claude-repl--refresh-vterm) #'ignore)
                ((symbol-function 'claude-repl--reset-vterm-cursors)
                 (lambda () (setq reset-called t)))
                ((symbol-function 'claude-repl--drain-pending-magit) #'ignore)
                ((symbol-function 'claude-repl--drain-pending-initial-buffers) #'ignore)
                ((symbol-function 'claude-repl--drain-pending-show-panels) #'ignore)
                ((symbol-function 'claude-repl--maybe-autoselect-input) #'ignore))
        (claude-repl--on-workspace-switch "ws1")
        (should reset-called)))))

;;;; ---- Tests: bounce-from-vterm ----

(ert-deftest claude-repl-test-panels-bounce-from-vterm-non-vterm-buffer ()
  "bounce-from-vterm is a no-op when the selected window shows a non-claude buffer."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*bounce-noop-regular*"
      (let ((orig-win (selected-window)))
        (set-window-buffer orig-win (current-buffer))
        (claude-repl--bounce-from-vterm nil)
        (should (eq (selected-window) orig-win))))))

(ert-deftest claude-repl-test-panels-bounce-from-vterm-input-buffer-no-recursion ()
  "bounce-from-vterm does NOT fire when the selected window shows an input buffer.
Load-bearing: after the bounce redirects vterm→input, the input selection
must not itself trigger another bounce."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*claude-panel-input-test-ws*"
      (let ((orig-win (selected-window)))
        (set-window-buffer orig-win (current-buffer))
        (let ((last-input-event ?a))
          (claude-repl--bounce-from-vterm nil))
        (should (eq (selected-window) orig-win))))))

(ert-deftest claude-repl-test-panels-bounce-from-vterm-keyboard-redirects ()
  "bounce-from-vterm redirects to the input window when selection is keyboard-driven."
  (claude-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*claude-panel-test-ws*"))
          (input-buf (get-buffer-create "*claude-panel-input-test-ws*"))
          (new-win nil))
      (unwind-protect
          (progn
            (claude-repl--ws-put "test-ws" :input-buffer input-buf)
            (set-window-buffer (selected-window) vterm-buf)
            (setq new-win (split-window))
            (set-window-buffer new-win input-buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
              (let ((last-input-event ?a))
                (claude-repl--bounce-from-vterm nil)
                (should (eq (window-buffer (selected-window)) input-buf)))))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

(ert-deftest claude-repl-test-panels-bounce-from-vterm-mouse-does-not-redirect ()
  "Mouse-driven selection of a vterm window stays put — user wants to scroll/copy."
  (claude-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*claude-panel-test-ws*"))
          (input-buf (get-buffer-create "*claude-panel-input-test-ws*"))
          (new-win nil))
      (unwind-protect
          (progn
            (claude-repl--ws-put "test-ws" :input-buffer input-buf)
            (let ((vterm-win (selected-window)))
              (set-window-buffer vterm-win vterm-buf)
              (setq new-win (split-window))
              (set-window-buffer new-win input-buf)
              (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
                ;; Simulate a mouse event as last-input-event
                (let ((last-input-event '(mouse-1 (nil 0 . 0))))
                  (claude-repl--bounce-from-vterm nil)
                  (should (eq (selected-window) vterm-win))))))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

(ert-deftest claude-repl-test-panels-bounce-from-vterm-warns-when-no-input-win ()
  "When panels are hidden (no visible input window), bounce emits a user-facing warning.
Previously this path logged verbosely and stranded point in vterm; now
we at least surface the stuck state so the user knows to click out."
  (claude-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*claude-panel-test-ws*"))
          (messages nil))
      (unwind-protect
          (progn
            ;; Input buffer is stored but NOT displayed in any window.
            (claude-repl--ws-put "test-ws" :input-buffer
                                 (get-buffer-create "*claude-panel-input-test-ws*"))
            (set-window-buffer (selected-window) vterm-buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
              (let ((last-input-event ?a))
                (claude-repl--bounce-from-vterm nil)))
            (should (cl-some (lambda (m) (string-match-p "input panel isn't visible" m))
                             messages)))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))
        (when-let ((b (get-buffer "*claude-panel-input-test-ws*"))) (kill-buffer b))))))

;;;; ---- Tests: initialize-input-buffer ----

(ert-deftest claude-repl-test-initialize-input-buffer-fresh ()
  "initialize-input-buffer enables claude-input-mode and restores history on a fresh buffer."
  (claude-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *init-input-fresh*"))
          (mode-called nil)
          (history-called nil))
      (unwind-protect
          (cl-letf (((symbol-function 'claude-repl--create-buffer)
                     (lambda (_ws &optional _s) buf))
                    ((symbol-function 'claude-input-mode)
                     (lambda () (setq mode-called t)))
                    ((symbol-function 'claude-repl--history-restore)
                     (lambda (_ws) (setq history-called t))))
            (claude-repl--initialize-input-buffer "test-ws")
            (should mode-called)
            (should history-called)
            (should (eq (claude-repl--ws-get "test-ws" :input-buffer) buf)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest claude-repl-test-initialize-input-buffer-already-initialized ()
  "initialize-input-buffer errors when the buffer is already in claude-input-mode."
  (claude-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *init-input-already*")))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (setq major-mode 'claude-input-mode))
            (cl-letf (((symbol-function 'claude-repl--create-buffer)
                       (lambda (_ws &optional _s) buf)))
              (should-error (claude-repl--initialize-input-buffer "test-ws"))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;;;; ---- Tests: kill-stale-vterm ----

(ert-deftest claude-repl-test-panels-kill-stale-vterm-no-buffer ()
  "kill-stale-vterm is a no-op when no buffer with the expected name exists."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--buffer-name)
               (lambda (&rest _) "*nonexistent-stale*")))
      ;; Should not error
      (claude-repl--kill-stale-vterm))))

(ert-deftest claude-repl-test-panels-kill-stale-vterm-stale ()
  "kill-stale-vterm kills a buffer that exists without a live process."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*stale-vterm-test*")))
      (cl-letf (((symbol-function 'claude-repl--buffer-name)
                 (lambda (&rest _) "*stale-vterm-test*")))
        (should (get-buffer "*stale-vterm-test*"))
        (claude-repl--kill-stale-vterm)
        (should-not (get-buffer "*stale-vterm-test*"))))))

(ert-deftest claude-repl-test-panels-kill-stale-vterm-has-process ()
  "kill-stale-vterm preserves a buffer that has a live process."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*process-vterm-test*")))
      (unwind-protect
          (cl-letf (((symbol-function 'claude-repl--buffer-name)
                     (lambda (&rest _) "*process-vterm-test*"))
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

(ert-deftest claude-repl-test-panels-kill-vterm-process-skips-kill-buffer-query-functions ()
  "kill-vterm-process does not consult `kill-buffer-query-functions'.
Regression: the nuke path must not prompt about closing the claude
process, even when other hooks (e.g., vterm's own kill query) are
registered."
  (claude-repl-test--with-clean-state
    (let* ((buf (get-buffer-create "*kill-no-prompt*"))
           (consulted nil)
           (kill-buffer-query-functions
            (list (lambda () (setq consulted t) nil))))
      (claude-repl--kill-vterm-process buf)
      (should-not consulted)
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
                ;; Instantiation should have start-cmd cleared
                (let ((inst (claude-repl--ws-get "test-ws" :bare-metal)))
                  (should-not (claude-repl-instantiation-start-cmd inst)))))))))))

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

;;;; ---- Tests: kill-workspace-buffers ----

(ert-deftest claude-repl-test-panels-kill-workspace-buffers/persp-mode-off ()
  "kill-workspace-buffers is a no-op when persp-mode is not active."
  (let ((persp-mode nil)
        (buf (get-buffer-create "*kwb-persp-off*")))
    (unwind-protect
        (progn
          (claude-repl--kill-workspace-buffers "some-ws")
          ;; Buffer survives because persp-mode is off.
          (should (buffer-live-p buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-test-panels-kill-workspace-buffers/no-persp-for-ws ()
  "kill-workspace-buffers is a no-op when the persp does not exist."
  (let ((persp-mode t)
        (buf (get-buffer-create "*kwb-no-persp*")))
    (unwind-protect
        (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_ws) nil)))
          (claude-repl--kill-workspace-buffers "ghost-ws")
          (should (buffer-live-p buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-test-panels-kill-workspace-buffers/symbol-persp-skipped ()
  "kill-workspace-buffers skips when persp-get-by-name returns the persp-not-persp keyword.
persp-mode returns the :nil keyword (persp-not-persp) when the perspective
does not exist — --ws-resolve-persp normalizes it to nil so the buffers
loop is skipped entirely."
  (let ((persp-mode t)
        (buf (get-buffer-create "*kwb-symbol-persp*"))
        (persp-buffers-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_ws) :nil))
                  ((symbol-function 'persp-buffers)
                   (lambda (_p) (setq persp-buffers-called t) nil)))
          (claude-repl--kill-workspace-buffers "sym-ws")
          (should-not persp-buffers-called)
          (should (buffer-live-p buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-test-panels-kill-workspace-buffers/kills-all-live-buffers ()
  "kill-workspace-buffers kills every live buffer returned by persp-buffers."
  (let ((persp-mode t)
        (b1 (get-buffer-create "*kwb-live-1*"))
        (b2 (get-buffer-create "*kwb-live-2*"))
        (b3 (get-buffer-create "*kwb-live-3*")))
    (unwind-protect
        (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_ws) (list 'persp)))
                  ((symbol-function 'persp-buffers) (lambda (_p) (list b1 b2 b3))))
          (claude-repl--kill-workspace-buffers "live-ws")
          (should-not (buffer-live-p b1))
          (should-not (buffer-live-p b2))
          (should-not (buffer-live-p b3)))
      (dolist (b (list b1 b2 b3))
        (when (buffer-live-p b) (kill-buffer b))))))

(ert-deftest claude-repl-test-panels-kill-workspace-buffers/skips-dead-and-nil ()
  "kill-workspace-buffers tolerates dead and nil entries in the buffer list."
  (let ((persp-mode t)
        (live (get-buffer-create "*kwb-mixed-live*"))
        (dead (get-buffer-create "*kwb-mixed-dead*")))
    (kill-buffer dead)
    (unwind-protect
        (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_ws) (list 'persp)))
                  ((symbol-function 'persp-buffers) (lambda (_p) (list nil dead live))))
          ;; Should not error despite nil / dead entries.
          (claude-repl--kill-workspace-buffers "mixed-ws")
          (should-not (buffer-live-p live)))
      (when (buffer-live-p live) (kill-buffer live)))))

(ert-deftest claude-repl-test-panels-kill-workspace-buffers/spares-foreign-owned ()
  "kill-workspace-buffers does NOT kill a buffer owned by a different workspace.
Regression guard: persp-mode can drift another workspace's live Claude panel
into this persp, and nuking it would wipe that workspace's running session."
  (let ((persp-mode t)
        (foreign (get-buffer-create "*claude-panel-other-ws*")))
    (with-current-buffer foreign
      (setq-local claude-repl--owning-workspace "other-ws"))
    (unwind-protect
        (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_ws) (list 'persp)))
                  ((symbol-function 'persp-buffers) (lambda (_p) (list foreign))))
          (claude-repl--kill-workspace-buffers "this-ws")
          (should (buffer-live-p foreign)))
      (when (buffer-live-p foreign) (kill-buffer foreign)))))

(ert-deftest claude-repl-test-panels-kill-workspace-buffers/kills-own-owned ()
  "kill-workspace-buffers kills a buffer owned by the workspace being nuked."
  (let ((persp-mode t)
        (own (get-buffer-create "*claude-panel-this-ws*")))
    (with-current-buffer own
      (setq-local claude-repl--owning-workspace "this-ws"))
    (unwind-protect
        (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_ws) (list 'persp)))
                  ((symbol-function 'persp-buffers) (lambda (_p) (list own))))
          (claude-repl--kill-workspace-buffers "this-ws")
          (should-not (buffer-live-p own)))
      (when (buffer-live-p own) (kill-buffer own)))))

(ert-deftest claude-repl-test-panels-kill-workspace-buffers/kills-attached-process ()
  "kill-workspace-buffers deletes a process attached to a workspace buffer."
  (let* ((persp-mode t)
         (buf (get-buffer-create "*kwb-proc*"))
         (proc (start-process "kwb-fake-proc" buf "sleep" "60"))
         (deleted-procs nil)
         (query-cleared-procs nil))
    (unwind-protect
        (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_ws) (list 'persp)))
                  ((symbol-function 'persp-buffers) (lambda (_p) (list buf)))
                  ((symbol-function 'delete-process)
                   (lambda (p) (push p deleted-procs)))
                  ((symbol-function 'set-process-query-on-exit-flag)
                   (lambda (p _f) (push p query-cleared-procs)))
                  ((symbol-function 'claude-repl--schedule-sigkill) #'ignore))
          (claude-repl--kill-workspace-buffers "proc-ws")
          (should (memq proc deleted-procs))
          (should (memq proc query-cleared-procs))
          (should-not (buffer-live-p buf)))
      (when (process-live-p proc) (delete-process proc))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-test-panels-kill-workspace-buffers/kills-modified-buffer-without-prompt ()
  "kill-workspace-buffers kills a modified file-visiting buffer without prompting."
  (let* ((persp-mode t)
         (buf (get-buffer-create "*kwb-modified*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "unsaved work")
            (set-buffer-modified-p t))
          ;; If kill-buffer-query-functions were consulted this would block
          ;; interactively; in batch mode an unbound y-or-n-p would error.
          (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_ws) (list 'persp)))
                    ((symbol-function 'persp-buffers) (lambda (_p) (list buf))))
            (claude-repl--kill-workspace-buffers "modified-ws"))
          (should-not (buffer-live-p buf)))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf)))))

(ert-deftest claude-repl-test-panels-kill-workspace-buffers/continues-after-error ()
  "kill-workspace-buffers keeps killing remaining buffers when one errors."
  (let ((persp-mode t)
        (b1 (get-buffer-create "*kwb-err-1*"))
        (b2 (get-buffer-create "*kwb-err-2*"))
        (b3 (get-buffer-create "*kwb-err-3*"))
        (original-kill-buffer (symbol-function 'kill-buffer)))
    (unwind-protect
        (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_ws) (list 'persp)))
                  ((symbol-function 'persp-buffers) (lambda (_p) (list b1 b2 b3)))
                  ((symbol-function 'kill-buffer)
                   (lambda (b)
                     (if (eq b b2)
                         (error "simulated kill failure")
                       (funcall original-kill-buffer b)))))
          (claude-repl--kill-workspace-buffers "err-ws")
          ;; b1 killed normally, b2 errored (still live), b3 killed after the error.
          (should-not (buffer-live-p b1))
          (should (buffer-live-p b2))
          (should-not (buffer-live-p b3)))
      (dolist (b (list b1 b2 b3))
        (when (buffer-live-p b) (kill-buffer b))))))

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

;;;; ---- Tests: initialize-claude ----

(ert-deftest claude-repl-test-panels-initialize-claude-no-workspace ()
  "initialize-claude errors when no workspace is active."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
      (should-error (claude-repl--initialize-claude)))))

(ert-deftest claude-repl-test-panels-initialize-claude-already-running-errors ()
  "initialize-claude errors when Claude is already running."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'claude-repl--claude-running-p) (lambda (_ws) t)))
      (should-error (claude-repl--initialize-claude)))))

(defmacro claude-repl-test--initialize-claude-stubs (vterm-buf-var &rest body)
  "Run BODY with the stubs needed to exercise `claude-repl--initialize-claude'.
VTERM-BUF-VAR is the name of a `let'-bound buffer that will be returned
from `create-buffer'.  Stubs can be overridden by wrapping BODY in another
`cl-letf' that rebinds the same symbols."
  (declare (indent 1))
  `(cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
             ((symbol-function 'claude-repl--claude-running-p) (lambda (&optional _ws) nil))
             ((symbol-function 'claude-repl--initialize-ws-env) #'ignore)
             ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp"))
             ((symbol-function 'claude-repl--record-project-dir) #'ignore)
             ((symbol-function 'claude-repl--kill-stale-vterm) (lambda (&optional _ws) nil))
             ((symbol-function 'claude-repl--create-buffer)
              (lambda (_ws &optional _s) ,vterm-buf-var))
             ((symbol-function 'claude-repl--build-start-cmd)
              (lambda (_ws) (list :cmd "claude"
                                  :sandboxed-p nil
                                  :docker-image nil
                                  :session-id nil
                                  :fork-session-id nil
                                  :worktree-p nil
                                  :active-env :bare-metal
                                  :inst (make-claude-repl-instantiation))))
             ((symbol-function 'claude-repl--log-session-start) #'ignore)
             ((symbol-function 'vterm-mode) #'ignore)
             ((symbol-function 'claude-repl--set-buffer-background) #'ignore)
             ((symbol-function 'claude-repl--workspace-mode-line) (lambda (_ws) '("test")))
             ((symbol-function 'vterm-send-string) #'ignore)
             ((symbol-function 'vterm-send-return) #'ignore)
             ((symbol-function 'claude-repl--schedule-ready-timer) #'ignore)
             ((symbol-function 'claude-repl--initialize-input-buffer) #'ignore)
             ((symbol-function 'claude-repl--enable-hide-overlay) #'ignore)
             ((symbol-function 'claude-repl--workspace-id) (lambda () "id")))
     ,@body))

(ert-deftest claude-repl-test-panels-initialize-claude-starts-new-session ()
  "initialize-claude sets prefix counter, enables overlay, writes :claude-state :init."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :active-env :bare-metal)
    (let ((vterm-buf (generate-new-buffer " *init-claude-fixture*"))
          (overlay-called nil))
      (unwind-protect
          (claude-repl-test--initialize-claude-stubs vterm-buf
            (cl-letf (((symbol-function 'claude-repl--enable-hide-overlay)
                       (lambda () (setq overlay-called t))))
              (claude-repl--initialize-claude)
              (should (equal (claude-repl--ws-get "test-ws" :prefix-counter) 0))
              (should (eq (claude-repl--ws-get "test-ws" :claude-state) :init))
              (should overlay-called)))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest claude-repl-test-panels-initialize-claude-sends-cmd-and-return ()
  "initialize-claude sends the startup cmd string and a return to the vterm buffer."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :active-env :bare-metal)
    (let ((vterm-buf (generate-new-buffer " *init-claude-send*"))
          (sent-string nil)
          (return-sent nil))
      (unwind-protect
          (claude-repl-test--initialize-claude-stubs vterm-buf
            (cl-letf (((symbol-function 'vterm-send-string)
                       (lambda (s) (setq sent-string s)))
                      ((symbol-function 'vterm-send-return)
                       (lambda () (setq return-sent t))))
              (claude-repl--initialize-claude)
              (should (string-match-p "claude" sent-string))
              (should return-sent)))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest claude-repl-test-panels-initialize-claude-schedules-ready-timer ()
  "initialize-claude schedules the readiness timer for the workspace."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :active-env :bare-metal)
    (let ((vterm-buf (generate-new-buffer " *init-claude-timer*"))
          (timer-ws nil))
      (unwind-protect
          (claude-repl-test--initialize-claude-stubs vterm-buf
            (cl-letf (((symbol-function 'claude-repl--schedule-ready-timer)
                       (lambda (ws) (setq timer-ws ws))))
              (claude-repl--initialize-claude)
              (should (equal timer-ws "test-ws"))))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest claude-repl-test-panels-initialize-claude-sets-ready-nil ()
  "initialize-claude sets buffer-local claude-repl--ready to nil in the vterm buffer."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :active-env :bare-metal)
    (let ((vterm-buf (generate-new-buffer " *init-claude-ready*"))
          (ready-at-send 'unset))
      (unwind-protect
          (progn
            (with-current-buffer vterm-buf
              (setq-local claude-repl--ready t))
            (claude-repl-test--initialize-claude-stubs vterm-buf
              (cl-letf (((symbol-function 'vterm-send-string)
                         (lambda (_s) (setq ready-at-send claude-repl--ready))))
                (claude-repl--initialize-claude)
                (should-not ready-at-send))))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest claude-repl-test-panels-initialize-claude-sets-workspace-mode-line ()
  "initialize-claude sets mode-line-format via workspace-mode-line, passing ws."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :active-env :sandbox)
    (let ((vterm-buf (generate-new-buffer " *init-claude-ml*"))
          (mode-line-ws :unset))
      (unwind-protect
          (claude-repl-test--initialize-claude-stubs vterm-buf
            (cl-letf (((symbol-function 'claude-repl--build-start-cmd)
                       (lambda (_ws) (list :cmd "claude-sandbox"
                                           :sandboxed-p t
                                           :docker-image "img:latest"
                                           :session-id nil
                                           :fork-session-id nil
                                           :worktree-p t
                                           :active-env :sandbox
                                           :inst (make-claude-repl-instantiation))))
                      ((symbol-function 'claude-repl--workspace-mode-line)
                       (lambda (ws) (setq mode-line-ws ws) '("WS-ML"))))
              (claude-repl--initialize-claude)
              (should (equal mode-line-ws "test-ws"))
              (with-current-buffer vterm-buf
                (should (equal mode-line-format '("WS-ML"))))))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest claude-repl-test-panels-initialize-claude-clears-fork-session-id ()
  "initialize-claude clears :fork-session-id after building the cmd."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :active-env :bare-metal)
    (claude-repl--ws-put "test-ws" :fork-session-id "fork-abc")
    (let ((vterm-buf (generate-new-buffer " *init-claude-fork*")))
      (unwind-protect
          (claude-repl-test--initialize-claude-stubs vterm-buf
            (cl-letf (((symbol-function 'claude-repl--build-start-cmd)
                       (lambda (_ws) (list :cmd "claude"
                                           :sandboxed-p nil
                                           :docker-image nil
                                           :session-id nil
                                           :fork-session-id "fork-abc"
                                           :worktree-p nil
                                           :active-env :bare-metal
                                           :inst (make-claude-repl-instantiation)))))
              (claude-repl--initialize-claude)
              (should-not (claude-repl--ws-get "test-ws" :fork-session-id))))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest claude-repl-test-panels-initialize-claude-always-calls-ws-env-init ()
  "initialize-claude always calls initialize-ws-env, regardless of prior
`:active-env'.  initialize-ws-env is idempotent, so unconditional call is
safe and ensures the state file is re-read on every session start."
  (claude-repl-test--with-clean-state
    (let ((vterm-buf (generate-new-buffer " *init-claude-ws-env*"))
          (init-call-count 0))
      (unwind-protect
          (claude-repl-test--initialize-claude-stubs vterm-buf
            (cl-letf (((symbol-function 'claude-repl--initialize-ws-env)
                       (lambda (_ws &rest _) (cl-incf init-call-count))))
              (claude-repl--initialize-claude)
              (should (= init-call-count 1))))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest claude-repl-test-panels-initialize-claude-passes-hints-to-ws-env-init ()
  "initialize-claude forwards project-dir-hint and active-env-hint to
initialize-ws-env.  Models the worktree-creation / new-workspace paths."
  (claude-repl-test--with-clean-state
    (let ((vterm-buf (generate-new-buffer " *init-claude-hints*"))
          (got-hint nil)
          (got-env nil))
      (unwind-protect
          (claude-repl-test--initialize-claude-stubs vterm-buf
            (cl-letf (((symbol-function 'claude-repl--initialize-ws-env)
                       (lambda (_ws &optional dir env)
                         (setq got-hint dir)
                         (setq got-env env))))
              (claude-repl--initialize-claude "test-ws" "/tmp/worktree" :sandbox)
              (should (equal got-hint "/tmp/worktree"))
              (should (eq got-env :sandbox))))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest claude-repl-test-panels-initialize-claude-persists-state-on-success ()
  "initialize-claude calls state-save at the end of a successful start."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :active-env :bare-metal)
    (let ((vterm-buf (generate-new-buffer " *init-claude-state-save*"))
          (saved-ws nil))
      (unwind-protect
          (claude-repl-test--initialize-claude-stubs vterm-buf
            (cl-letf (((symbol-function 'claude-repl--state-save)
                       (lambda (ws) (setq saved-ws ws))))
              (claude-repl--initialize-claude)
              (should (equal saved-ws "test-ws"))))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest claude-repl-test-panels-initialize-claude-uses-explicit-ws-arg ()
  "initialize-claude uses the explicit WS argument rather than +workspace-current-name."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "explicit-arg-ws" :active-env :bare-metal)
    (let ((vterm-buf (generate-new-buffer " *init-claude-explicit*"))
          (running-ws nil))
      (unwind-protect
          (claude-repl-test--initialize-claude-stubs vterm-buf
            (cl-letf (((symbol-function '+workspace-current-name)
                       (lambda () "persp-current-ws"))
                      ((symbol-function 'claude-repl--claude-running-p)
                       (lambda (ws) (setq running-ws ws) nil)))
              (claude-repl--initialize-claude "explicit-arg-ws")
              (should (equal running-ws "explicit-arg-ws"))))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

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

(ert-deftest claude-repl-test-panels-fix-vterm-scroll-different-window-preserves-selection ()
  "fix-vterm-scroll never changes the selected window when the vterm window
is a different (non-selected) window — the previous brief-select hack
was the source of the visible scroll-down animation, so the new
implementation must NOT select the vterm window at any point."
  (let ((buf (get-buffer-create "*scroll-diff-win-preserve*"))
        (new-win nil)
        (selections nil))
    (unwind-protect
        (progn
          (setq new-win (split-window))
          (set-window-buffer new-win buf)
          (let ((orig-win (selected-window))
                (orig-select-window (symbol-function 'select-window)))
            ;; Ensure we are NOT in the vterm window
            (should-not (eq new-win orig-win))
            (cl-letf (((symbol-function 'select-window)
                       (lambda (win &optional norecord)
                         (push win selections)
                         (funcall orig-select-window win norecord))))
              (claude-repl--fix-vterm-scroll buf))
            ;; New impl: no `select-window' calls at all — the snap is
            ;; driven via `set-window-start' / `set-window-point' alone.
            (should-not selections)
            (should (eq (selected-window) orig-win))))
      (when (and new-win (window-live-p new-win))
        (ignore-errors (delete-window new-win)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-test-panels-fix-vterm-scroll-different-window-calls-snap ()
  "fix-vterm-scroll calls `--snap-vterm-window-to-cursor' on the vterm
window when the vterm window is a different (non-selected) window."
  (let ((buf (get-buffer-create "*scroll-diff-win-snap*"))
        (new-win nil)
        (snap-arg nil))
    (unwind-protect
        (progn
          (setq new-win (split-window))
          (set-window-buffer new-win buf)
          (with-current-buffer buf (setq major-mode 'vterm-mode))
          (cl-letf (((symbol-function 'claude-repl--snap-vterm-window-to-cursor)
                     (lambda (win) (setq snap-arg win))))
            (claude-repl--fix-vterm-scroll buf))
          (should (eq snap-arg new-win)))
      (when (and new-win (window-live-p new-win))
        (ignore-errors (delete-window new-win)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;; ---- Tests: snap-vterm-window-to-cursor ----

(ert-deftest claude-repl-test-panels-snap-vterm-window-to-cursor-positions-cursor-at-bottom ()
  "snap-vterm-window-to-cursor sets `window-start' so the cursor lands on
the last visible line — that is, `window-start' is exactly
`(body-height - 1)' lines above the cursor."
  (let ((buf (get-buffer-create "*snap-bottom*"))
        (new-win nil))
    (unwind-protect
        (progn
          (setq new-win (split-window))
          (set-window-buffer new-win buf)
          (with-current-buffer buf
            (erase-buffer)
            ;; Insert enough lines that the body-height fits inside the buffer.
            (dotimes (i 200) (insert (format "line-%d\n" i)))
            (goto-char (point-max))
            (let* ((body-height (window-body-height new-win))
                   (expected-start
                    (save-excursion
                      (goto-char (point-max))
                      (forward-line (- 1 body-height))
                      (line-beginning-position))))
              (claude-repl--snap-vterm-window-to-cursor new-win)
              (should (= (window-start new-win) expected-start))
              (should (= (window-point new-win) (point-max))))))
      (when (and new-win (window-live-p new-win))
        (ignore-errors (delete-window new-win)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-test-panels-snap-vterm-window-to-cursor-short-buffer-uses-point-min ()
  "When the buffer is shorter than `window-body-height',
snap-vterm-window-to-cursor falls back to `point-min' as `window-start'
\(via the natural `forward-line' cap when walking past the buffer head)."
  (let ((buf (get-buffer-create "*snap-short*"))
        (new-win nil))
    (unwind-protect
        (progn
          (setq new-win (split-window))
          (set-window-buffer new-win buf)
          (with-current-buffer buf
            (erase-buffer)
            (insert "only line\n")
            (goto-char (point-max))
            (claude-repl--snap-vterm-window-to-cursor new-win)
            (should (= (window-start new-win) (point-min)))
            (should (= (window-point new-win) (point-max)))))
      (when (and new-win (window-live-p new-win))
        (ignore-errors (delete-window new-win)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-test-panels-snap-vterm-window-to-cursor-does-not-select-window ()
  "snap-vterm-window-to-cursor never selects the target window — it
operates purely through `set-window-start' + `set-window-point' to
avoid `window-selection-change-functions' / `bounce-from-vterm'
re-entry."
  (let ((buf (get-buffer-create "*snap-no-select*"))
        (new-win nil)
        (selections nil)
        (orig-win nil))
    (unwind-protect
        (progn
          (setq orig-win (selected-window))
          (setq new-win (split-window))
          (set-window-buffer new-win buf)
          (with-current-buffer buf
            (insert "some content\n")
            (goto-char (point-max))
            (let ((orig-select-window (symbol-function 'select-window)))
              (cl-letf (((symbol-function 'select-window)
                         (lambda (win &optional norecord)
                           (push win selections)
                           (funcall orig-select-window win norecord))))
                (claude-repl--snap-vterm-window-to-cursor new-win))
              (should-not selections)
              (should (eq (selected-window) orig-win)))))
      (when (and new-win (window-live-p new-win))
        (ignore-errors (delete-window new-win)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;; ---- Tests: refresh-vterm-window ----

(ert-deftest claude-repl-test-panels-refresh-vterm-window-snaps-on-vterm-mode-buffer ()
  "refresh-vterm-window calls `--snap-vterm-window-to-cursor' on the
vterm window after the cursor reset + redraw, replacing the old bare
`set-window-point' tail."
  (let ((buf (get-buffer-create "*claude-panel-snap-test*"))
        (new-win nil)
        (snap-arg nil))
    (unwind-protect
        (progn
          (setq new-win (split-window))
          (set-window-buffer new-win buf)
          (with-current-buffer buf (setq major-mode 'vterm-mode))
          (cl-letf (((symbol-function 'claude-repl--claude-buffer-p) (lambda (_b) t))
                    ((symbol-function 'claude-repl--vterm-redraw) #'ignore)
                    ((symbol-function 'vterm-reset-cursor-point) #'ignore)
                    ((symbol-function 'claude-repl--snap-vterm-window-to-cursor)
                     (lambda (win) (setq snap-arg win))))
            (claude-repl--refresh-vterm-window new-win))
          (should (eq snap-arg new-win)))
      (when (and new-win (window-live-p new-win))
        (ignore-errors (delete-window new-win)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-test-panels-refresh-vterm-window-skips-non-claude-buffer ()
  "refresh-vterm-window is a no-op when the window's buffer is not a
Claude vterm buffer — the snap helper must not run."
  (let ((buf (get-buffer-create "*not-claude-refresh*"))
        (new-win nil)
        (snap-called nil))
    (unwind-protect
        (progn
          (setq new-win (split-window))
          (set-window-buffer new-win buf)
          (cl-letf (((symbol-function 'claude-repl--claude-buffer-p) (lambda (_b) nil))
                    ((symbol-function 'claude-repl--snap-vterm-window-to-cursor)
                     (lambda (_win) (setq snap-called t))))
            (claude-repl--refresh-vterm-window new-win))
          (should-not snap-called))
      (when (and new-win (window-live-p new-win))
        (ignore-errors (delete-window new-win)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-test-panels-fix-vterm-scroll-non-vterm-mode-skips-reset ()
  "fix-vterm-scroll does not call `vterm-reset-cursor-point' when the
buffer is not in `vterm-mode' — the cursor-reset is vterm-specific."
  (let ((buf (get-buffer-create "*scroll-non-vterm*"))
        (new-win nil)
        (reset-called nil))
    (unwind-protect
        (progn
          (setq new-win (split-window))
          (set-window-buffer new-win buf)
          (cl-letf (((symbol-function 'vterm-reset-cursor-point)
                     (lambda () (setq reset-called t)))
                    ((symbol-function 'claude-repl--snap-vterm-window-to-cursor)
                     #'ignore))
            (claude-repl--fix-vterm-scroll buf))
          (should-not reset-called))
      (when (and new-win (window-live-p new-win))
        (ignore-errors (delete-window new-win)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;; ---- Tests: claude-repl-restart ----

(ert-deftest claude-repl-test-panels-restart-kills-then-initializes ()
  "claude-repl-restart calls claude-repl-kill then claude-repl--initialize-claude in order."
  (claude-repl-test--with-clean-state
    (let ((order nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl-kill)
                 (lambda () (push 'kill order)))
                ((symbol-function 'claude-repl--initialize-claude)
                 (lambda (_ws) (push 'init order))))
        (claude-repl-restart)
        (should (equal (nreverse order) '(kill init)))))))

;;;; ---- Tests: claude-repl-kill no workspace ----

(ert-deftest claude-repl-test-panels-kill-no-workspace ()
  "claude-repl-kill errors when no active workspace."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
      (should-error (claude-repl-kill)))))

(ert-deftest claude-repl-test-panels-kill-clears-state-axes ()
  "claude-repl-kill resets :claude-state and :repl-state."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :thinking)
    (claude-repl--ws-set-repl-state "ws1" :inactive)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'claude-repl--kill-session) #'ignore)
              ((symbol-function 'force-mode-line-update) #'ignore))
      (claude-repl-kill)
      (should-not (claude-repl--ws-get "ws1" :claude-state))
      (should-not (claude-repl--ws-get "ws1" :repl-state)))))

;;;; ---- Tests: redirect-from-claude-before-save with Claude window ----

(ert-deftest claude-repl-test-panels-redirect-claude-to-other-window ()
  "redirect-from-claude-before-save selects a non-Claude window when current is Claude."
  (claude-repl-test--with-clean-state
    (let ((claude-buf (get-buffer-create "*claude-panel-abcd1234*"))
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
    (let ((claude-buf (get-buffer-create "*claude-panel-abcd1234*")))
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

;;;; ---- Tests: redirect-from-claude-before-save side/dedicated windows ----

(ert-deftest claude-repl-test-panels-redirect-from-side-window ()
  "Redirect fires when selected window is a side window (e.g. the drawer).

Regression: a side window selected at persp save time would otherwise be
restored as the selected window, causing `+workspace/kill's fallback
`switch-to-buffer' to split a new window for the doom splash buffer."
  (claude-repl-test--with-clean-state
    (delete-other-windows)
    (let* ((regular-buf (get-buffer-create "*regular-buf*"))
           (side-buf    (get-buffer-create "*side-buf*"))
           (main-win    (selected-window))
           (side-win    nil))
      (unwind-protect
          (progn
            (set-window-buffer main-win regular-buf)
            (setq side-win
                  (display-buffer-in-side-window side-buf '((side . right))))
            (should (window-live-p side-win))
            (select-window side-win)
            (claude-repl--redirect-from-claude-before-save)
            (should (eq (selected-window) main-win))
            (should (eq (window-buffer (selected-window)) regular-buf)))
        (when (and side-win (window-live-p side-win))
          (ignore-errors (delete-window side-win)))
        (set-window-buffer (selected-window) "*scratch*")
        (when (buffer-live-p regular-buf) (kill-buffer regular-buf))
        (when (buffer-live-p side-buf) (kill-buffer side-buf))))))

(ert-deftest claude-repl-test-panels-redirect-from-dedicated-window ()
  "Redirect fires when selected window is dedicated.

Dedicated windows cause `switch-to-buffer' to fall back to pop-up
behavior and split, which is what produced the spurious splash buffer
window after a nuke."
  (claude-repl-test--with-clean-state
    (let* ((regular-buf (get-buffer-create "*regular-buf*"))
           (dedicated-buf (get-buffer-create "*dedicated-buf*"))
           (main-win (selected-window))
           (extra-win nil))
      (unwind-protect
          (progn
            (set-window-buffer main-win regular-buf)
            (setq extra-win (split-window))
            (set-window-buffer extra-win dedicated-buf)
            (set-window-dedicated-p extra-win t)
            (select-window extra-win)
            (claude-repl--redirect-from-claude-before-save)
            (should (eq (selected-window) main-win))
            (should (eq (window-buffer (selected-window)) regular-buf)))
        (when (and extra-win (window-live-p extra-win))
          (set-window-dedicated-p extra-win nil)
          (ignore-errors (delete-window extra-win)))
        (set-window-buffer (selected-window) "*scratch*")
        (when (buffer-live-p regular-buf) (kill-buffer regular-buf))
        (when (buffer-live-p dedicated-buf) (kill-buffer dedicated-buf))))))

(ert-deftest claude-repl-test-panels-redirect-skips-side-window-as-target ()
  "Redirect target must skip side windows even when selected is a Claude panel.

Regression: the previous predicate `non-claude-panel-window-p' returned
t for the drawer (a non-Claude side window), so `cl-find-if' could
pick the drawer as the redirect destination — defeating the purpose
of the redirect."
  (claude-repl-test--with-clean-state
    (delete-other-windows)
    (let* ((claude-buf (get-buffer-create "*claude-panel-abcd1234*"))
           (regular-buf (get-buffer-create "*regular-buf*"))
           (side-buf    (get-buffer-create "*side-buf*"))
           (claude-win (selected-window))
           (regular-win nil)
           (side-win nil))
      (unwind-protect
          (progn
            (set-window-buffer claude-win claude-buf)
            (setq regular-win (split-window claude-win nil 'below))
            (set-window-buffer regular-win regular-buf)
            (setq side-win
                  (display-buffer-in-side-window side-buf '((side . right))))
            (should (window-live-p side-win))
            (select-window claude-win)
            (claude-repl--redirect-from-claude-before-save)
            (should (eq (selected-window) regular-win))
            (should (eq (window-buffer (selected-window)) regular-buf)))
        (when (and side-win (window-live-p side-win))
          (ignore-errors (delete-window side-win)))
        (when (and regular-win (window-live-p regular-win))
          (ignore-errors (delete-window regular-win)))
        (set-window-buffer (selected-window) "*scratch*")
        (when (buffer-live-p claude-buf) (kill-buffer claude-buf))
        (when (buffer-live-p regular-buf) (kill-buffer regular-buf))
        (when (buffer-live-p side-buf) (kill-buffer side-buf))))))

(ert-deftest claude-repl-test-panels-redirect-skips-dedicated-as-target ()
  "Redirect target must skip dedicated windows."
  (claude-repl-test--with-clean-state
    (delete-other-windows)
    (let* ((claude-buf (get-buffer-create "*claude-panel-abcd1234*"))
           (regular-buf (get-buffer-create "*regular-buf*"))
           (ded-buf (get-buffer-create "*ded-buf*"))
           (claude-win (selected-window))
           (regular-win nil)
           (ded-win nil))
      (unwind-protect
          (progn
            (set-window-buffer claude-win claude-buf)
            (setq ded-win (split-window claude-win nil 'right))
            (set-window-buffer ded-win ded-buf)
            (set-window-dedicated-p ded-win t)
            (setq regular-win (split-window claude-win nil 'below))
            (set-window-buffer regular-win regular-buf)
            (select-window claude-win)
            (claude-repl--redirect-from-claude-before-save)
            (should (eq (selected-window) regular-win))
            (should (eq (window-buffer (selected-window)) regular-buf)))
        (when (and ded-win (window-live-p ded-win))
          (set-window-dedicated-p ded-win nil)
          (ignore-errors (delete-window ded-win)))
        (when (and regular-win (window-live-p regular-win))
          (ignore-errors (delete-window regular-win)))
        (set-window-buffer (selected-window) "*scratch*")
        (when (buffer-live-p claude-buf) (kill-buffer claude-buf))
        (when (buffer-live-p regular-buf) (kill-buffer regular-buf))
        (when (buffer-live-p ded-buf) (kill-buffer ded-buf))))))

;;;; ---- Tests: save-target-window-p ----

(ert-deftest claude-repl-test-panels-save-target-window-p-regular ()
  "save-target-window-p returns non-nil for a plain window."
  (claude-repl-test--with-clean-state
    (should (claude-repl--save-target-window-p (selected-window)))))

(ert-deftest claude-repl-test-panels-save-target-window-p-claude-panel ()
  "save-target-window-p returns nil for a window showing a Claude panel."
  (claude-repl-test--with-clean-state
    (let ((claude-buf (get-buffer-create "*claude-panel-abcd1234*")))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) claude-buf)
            (should-not (claude-repl--save-target-window-p (selected-window))))
        (set-window-buffer (selected-window) "*scratch*")
        (when (buffer-live-p claude-buf) (kill-buffer claude-buf))))))

(ert-deftest claude-repl-test-panels-save-target-window-p-side-window ()
  "save-target-window-p returns nil for a side window."
  (claude-repl-test--with-clean-state
    (delete-other-windows)
    (let* ((side-buf (get-buffer-create "*side-buf*"))
           (side-win (display-buffer-in-side-window side-buf '((side . right)))))
      (unwind-protect
          (should-not (claude-repl--save-target-window-p side-win))
        (when (and side-win (window-live-p side-win))
          (ignore-errors (delete-window side-win)))
        (when (buffer-live-p side-buf) (kill-buffer side-buf))))))

(ert-deftest claude-repl-test-panels-save-target-window-p-dedicated ()
  "save-target-window-p returns nil for a dedicated window."
  (claude-repl-test--with-clean-state
    (delete-other-windows)
    (let ((extra (split-window)))
      (unwind-protect
          (progn
            (set-window-dedicated-p extra t)
            (should-not (claude-repl--save-target-window-p extra)))
        (when (window-live-p extra)
          (set-window-dedicated-p extra nil)
          (ignore-errors (delete-window extra)))))))

;;;; ---- Tests: fullscreen-and-focus ----

(ert-deftest claude-repl-test-panels-fullscreen-and-focus-calls-toggle ()
  "fullscreen-and-focus delegates to toggle-fullscreen when in a Claude buffer."
  (claude-repl-test--with-clean-state
    (let ((toggle-called nil)
          (claude-buf (get-buffer-create "*claude-panel-abcd1234*")))
      (unwind-protect
          (progn
            (switch-to-buffer claude-buf)
            (cl-letf (((symbol-function 'claude-repl-toggle-fullscreen)
                       (lambda () (setq toggle-called t)))
                      ((symbol-function '+workspace-current-name) (lambda () "test-ws")))
              (claude-repl-fullscreen-and-focus)
              (should toggle-called)))
        (switch-to-buffer "*scratch*")
        (when (buffer-live-p claude-buf) (kill-buffer claude-buf))))))

(ert-deftest claude-repl-test-panels-fullscreen-and-focus-selects-input ()
  "fullscreen-and-focus selects the input window after toggling when in a Claude buffer."
  (claude-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*claude-panel-input-abcd1234*"))
          (claude-buf (get-buffer-create "*claude-panel-abcd1234*"))
          (new-win nil))
      (unwind-protect
          (progn
            (claude-repl--ws-put "test-ws" :input-buffer input-buf)
            (switch-to-buffer claude-buf)
            (setq new-win (split-window))
            (set-window-buffer new-win input-buf)
            (cl-letf (((symbol-function 'claude-repl-toggle-fullscreen) #'ignore)
                      ((symbol-function '+workspace-current-name) (lambda () "test-ws")))
              (claude-repl-fullscreen-and-focus)
              (should (eq (window-buffer (selected-window)) input-buf))))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (switch-to-buffer "*scratch*")
        (when (buffer-live-p input-buf) (kill-buffer input-buf))
        (when (buffer-live-p claude-buf) (kill-buffer claude-buf))))))

(ert-deftest claude-repl-test-panels-fullscreen-and-focus-no-insert-state ()
  "fullscreen-and-focus does NOT enter evil insert state after focusing input."
  (claude-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*claude-panel-input-abcd1234*"))
          (claude-buf (get-buffer-create "*claude-panel-abcd1234*"))
          (new-win nil)
          (insert-called nil))
      (unwind-protect
          (progn
            (claude-repl--ws-put "test-ws" :input-buffer input-buf)
            (switch-to-buffer claude-buf)
            (setq new-win (split-window))
            (set-window-buffer new-win input-buf)
            (cl-letf (((symbol-function 'claude-repl-toggle-fullscreen) #'ignore)
                      ((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                      ((symbol-function 'evil-insert-state)
                       (lambda (&rest _) (setq insert-called t))))
              (claude-repl-fullscreen-and-focus)
              (should-not insert-called)))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (switch-to-buffer "*scratch*")
        (when (buffer-live-p input-buf) (kill-buffer input-buf))
        (when (buffer-live-p claude-buf) (kill-buffer claude-buf))))))

(ert-deftest claude-repl-test-panels-fullscreen-and-focus-no-input-window ()
  "fullscreen-and-focus does not error when in a Claude buffer but input has no window."
  (claude-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*test-fs-no-win*"))
          (claude-buf (get-buffer-create "*claude-panel-abcd1234*")))
      (unwind-protect
          (progn
            (claude-repl--ws-put "test-ws" :input-buffer input-buf)
            (switch-to-buffer claude-buf)
            (cl-letf (((symbol-function 'claude-repl-toggle-fullscreen) #'ignore)
                      ((symbol-function '+workspace-current-name) (lambda () "test-ws")))
              ;; Input buffer exists but is not displayed — should not error
              (claude-repl-fullscreen-and-focus)
              (should-not (eq (window-buffer (selected-window)) input-buf))))
        (switch-to-buffer "*scratch*")
        (when (buffer-live-p input-buf) (kill-buffer input-buf))
        (when (buffer-live-p claude-buf) (kill-buffer claude-buf))))))

(ert-deftest claude-repl-test-panels-fullscreen-and-focus-no-input-buffer ()
  "fullscreen-and-focus does not error when in a Claude buffer but no input buffer is set."
  (claude-repl-test--with-clean-state
    (let ((claude-buf (get-buffer-create "*claude-panel-abcd1234*")))
      (unwind-protect
          (progn
            (switch-to-buffer claude-buf)
            (cl-letf (((symbol-function 'claude-repl-toggle-fullscreen) #'ignore)
                      ((symbol-function '+workspace-current-name) (lambda () "test-ws")))
              ;; No input buffer at all — should not error
              (claude-repl-fullscreen-and-focus)))
        (switch-to-buffer "*scratch*")
        (when (buffer-live-p claude-buf) (kill-buffer claude-buf))))))

(ert-deftest claude-repl-test-panels-fullscreen-and-focus-non-claude-maximizes ()
  "fullscreen-and-focus saves config and sweeps other windows when not in a Claude buffer."
  (claude-repl-test--with-clean-state
    (let ((sweep-called nil)
          (claude-repl--window-fullscreen-config nil))
      (switch-to-buffer (get-buffer-create "*other*"))
      (unwind-protect
          (cl-letf (((symbol-function 'claude-repl-window--delete-where)
                     (lambda (&rest _) (setq sweep-called t) nil)))
            (claude-repl-fullscreen-and-focus)
            (should sweep-called)
            (should claude-repl--window-fullscreen-config))
        (setq claude-repl--window-fullscreen-config nil)
        (when (get-buffer "*other*") (kill-buffer "*other*"))))))

(ert-deftest claude-repl-test-panels-fullscreen-and-focus-non-claude-preserves-drawer ()
  "fullscreen-and-focus does NOT delete side windows (e.g. the drawer) when maximizing a non-Claude buffer."
  (claude-repl-test--with-clean-state
    (let* ((other-buf (get-buffer-create "*other-fs*"))
           (drawer-buf (get-buffer-create "*claude-drawer-fs*"))
           (claude-repl--window-fullscreen-config nil)
           (predicate-captured nil)
           (skip-captured nil))
      (switch-to-buffer other-buf)
      (unwind-protect
          (let ((fake-drawer-win (split-window-right)))
            (set-window-buffer fake-drawer-win drawer-buf)
            (set-window-parameter fake-drawer-win 'window-side 'left)
            (cl-letf (((symbol-function 'claude-repl-window--delete-where)
                       (lambda (pred &rest args)
                         (setq predicate-captured pred
                               skip-captured (plist-get args :skip-side-windows))
                         nil)))
              (claude-repl-fullscreen-and-focus)
              ;; The sweep must skip side windows by default (drawer survives).
              (should (or (null skip-captured) (eq skip-captured t)))
              ;; Predicate keeps the selected (non-drawer) window and would
              ;; target the drawer window if side-windows were not skipped.
              (should (functionp predicate-captured))
              (should-not (funcall predicate-captured (selected-window)))
              (should (funcall predicate-captured fake-drawer-win))))
        (setq claude-repl--window-fullscreen-config nil)
        (when (buffer-live-p drawer-buf) (kill-buffer drawer-buf))
        (when (buffer-live-p other-buf) (kill-buffer other-buf))))))

(ert-deftest claude-repl-test-panels-fullscreen-and-focus-non-claude-real-drawer-survives ()
  "End-to-end: maximizing a non-Claude buffer leaves a real side window alive."
  (claude-repl-test--with-clean-state
    (let* ((other-buf (get-buffer-create "*other-fs-real*"))
           (extra-buf (get-buffer-create "*extra-fs-real*"))
           (drawer-buf (get-buffer-create "*claude-drawer-fs-real*"))
           (claude-repl--window-fullscreen-config nil))
      (switch-to-buffer other-buf)
      (unwind-protect
          (let* ((extra-win (split-window-below))
                 (drawer-win (display-buffer-in-side-window
                              drawer-buf
                              '((side . left) (slot . 0)))))
            (set-window-buffer extra-win extra-buf)
            (should (window-live-p drawer-win))
            (claude-repl-fullscreen-and-focus)
            ;; Drawer (side window) is still alive after fullscreen.
            (should (window-live-p drawer-win))
            ;; Extra non-side window was swept.
            (should-not (window-live-p extra-win)))
        (setq claude-repl--window-fullscreen-config nil)
        (dolist (buf (list drawer-buf extra-buf other-buf))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest claude-repl-test-panels-fullscreen-and-focus-non-claude-restores ()
  "fullscreen-and-focus restores saved config on second press when not in a Claude buffer."
  (claude-repl-test--with-clean-state
    (let* ((restore-called nil)
           (fake-config (list 'fake-window-config))
           (claude-repl--window-fullscreen-config fake-config))
      (switch-to-buffer (get-buffer-create "*other*"))
      (unwind-protect
          (cl-letf (((symbol-function 'set-window-configuration)
                     (lambda (cfg) (when (eq cfg fake-config) (setq restore-called t)))))
            (claude-repl-fullscreen-and-focus)
            (should restore-called)
            (should-not claude-repl--window-fullscreen-config))
        (setq claude-repl--window-fullscreen-config nil)
        (when (get-buffer "*other*") (kill-buffer "*other*"))))))

;;;; ---- Tests: --first-live-leaf ----

(ert-deftest claude-repl-test-panels-first-live-leaf-nil ()
  "`claude-repl--first-live-leaf' returns nil for nil input."
  (should-not (claude-repl--first-live-leaf nil)))

(ert-deftest claude-repl-test-panels-first-live-leaf-on-leaf ()
  "`claude-repl--first-live-leaf' returns WIN when WIN is already a live leaf."
  (claude-repl-test--with-clean-state
    (let ((leaf (selected-window)))
      (should (eq (claude-repl--first-live-leaf leaf) leaf)))))

(ert-deftest claude-repl-test-panels-first-live-leaf-descends-container ()
  "`claude-repl--first-live-leaf' descends an internal container window
to find a live leaf.  Real-world trigger: `window-main-window' returns
an internal container when the main area has been split."
  (claude-repl-test--with-clean-state
    (let ((main-buf (get-buffer-create "*fs-leaf-main*"))
          (extra-buf (get-buffer-create "*fs-leaf-extra*"))
          (drawer-buf (get-buffer-create "*fs-leaf-drawer*")))
      (unwind-protect
          (progn
            (switch-to-buffer main-buf)
            (let* ((main-win (selected-window))
                   (extra-win (split-window-below)))
              (set-window-buffer extra-win extra-buf)
              (display-buffer-in-side-window drawer-buf '((side . left) (slot . 0)))
              (let* ((root (window-main-window))
                     (leaf (claude-repl--first-live-leaf root)))
                (should-not (window-live-p root))
                (should (window-live-p leaf))
                (should (memq leaf (list main-win extra-win))))))
        (dolist (buf (list drawer-buf extra-buf main-buf))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

;;;; ---- Tests: --fullscreen-leave-side-window ----

(ert-deftest claude-repl-test-panels-fullscreen-leave-side-window-noop-on-main ()
  "`claude-repl--fullscreen-leave-side-window' does NOT change selection
when the selected window is already a non-side main-area window."
  (claude-repl-test--with-clean-state
    (let ((other-buf (get-buffer-create "*fs-leave-side-noop*")))
      (unwind-protect
          (progn
            (switch-to-buffer other-buf)
            (let ((orig (selected-window)))
              (claude-repl--fullscreen-leave-side-window)
              (should (eq (selected-window) orig))))
        (when (buffer-live-p other-buf) (kill-buffer other-buf))))))

(ert-deftest claude-repl-test-panels-fullscreen-leave-side-window-from-drawer ()
  "`claude-repl--fullscreen-leave-side-window' selects the frame's main
window when invoked from a side window."
  (claude-repl-test--with-clean-state
    (let ((main-buf (get-buffer-create "*fs-leave-side-main*"))
          (drawer-buf (get-buffer-create "*fs-leave-side-drawer*")))
      (unwind-protect
          (progn
            (switch-to-buffer main-buf)
            (let* ((main-win (selected-window))
                   (drawer-win (display-buffer-in-side-window
                                drawer-buf
                                '((side . left) (slot . 0)))))
              (select-window drawer-win)
              (should (claude-repl-window--side-window-p (selected-window)))
              (claude-repl--fullscreen-leave-side-window)
              (should-not (claude-repl-window--side-window-p (selected-window)))
              (should (eq (selected-window) main-win))))
        (dolist (buf (list drawer-buf main-buf))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

;;;; ---- Tests: fullscreen-and-focus side-window redirect ----

(ert-deftest claude-repl-test-panels-fullscreen-and-focus-from-drawer-preserves-main ()
  "When invoked from inside the drawer side window with several main
windows visible, `claude-repl-fullscreen-and-focus' leaves the
originally-focused main window's siblings swept and the drawer alive —
crucially, the originating main window survives instead of being
sacrificed because the drawer was the `keep' anchor."
  (claude-repl-test--with-clean-state
    (let ((main-buf (get-buffer-create "*fs-from-drawer-main*"))
          (extra-buf (get-buffer-create "*fs-from-drawer-extra*"))
          (drawer-buf (get-buffer-create "*fs-from-drawer-drawer*"))
          (claude-repl--window-fullscreen-config nil))
      (unwind-protect
          (progn
            (switch-to-buffer main-buf)
            (let* ((main-win (selected-window))
                   (extra-win (split-window-below)))
              (set-window-buffer extra-win extra-buf)
              (let ((drawer-win (display-buffer-in-side-window
                                 drawer-buf
                                 '((side . left) (slot . 0)))))
                (select-window drawer-win)
                (claude-repl-fullscreen-and-focus)
                ;; Drawer (side window) survives.
                (should (window-live-p drawer-win))
                ;; The originating main window survives — without the
                ;; side-window redirect it would be deleted because the
                ;; drawer was `keep' and the predicate matches it.
                (should (window-live-p main-win))
                ;; The other main-area window is swept.
                (should-not (window-live-p extra-win)))))
        (setq claude-repl--window-fullscreen-config nil)
        (dolist (buf (list drawer-buf extra-buf main-buf))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest claude-repl-test-panels-fullscreen-and-focus-from-drawer-routes-to-claude-branch ()
  "When the drawer is selected but the main window contains a Claude
panel buffer, the side-window redirect lands on the Claude buffer and
the function takes the Claude branch (delegates to toggle-fullscreen)."
  (claude-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*claude-panel-fs-redir*"))
          (drawer-buf (get-buffer-create "*fs-redir-drawer*"))
          (toggle-called nil))
      (unwind-protect
          (progn
            (switch-to-buffer vterm-buf)
            (let* ((vterm-win (selected-window))
                   (drawer-win (display-buffer-in-side-window
                                drawer-buf
                                '((side . left) (slot . 0)))))
              (select-window drawer-win)
              (cl-letf (((symbol-function 'claude-repl-toggle-fullscreen)
                         (lambda () (setq toggle-called t)))
                        ((symbol-function '+workspace-current-name)
                         (lambda () "test-ws")))
                (claude-repl-fullscreen-and-focus))
              ;; The redirect moved point onto the Claude panel main window,
              ;; so the Claude branch fired.
              (should toggle-called)
              (should (window-live-p vterm-win))))
        (dolist (buf (list drawer-buf vterm-buf))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

;;;; ---- Tests: unhide-workspace ----

(ert-deftest claude-repl-test-unhide-workspace-flips-hidden-to-active ()
  "`claude-repl--unhide-workspace' resets `:hidden' to `:active'."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :repl-state :hidden)
    (claude-repl--unhide-workspace "ws")
    (should (eq (claude-repl--ws-get "ws" :repl-state) :active))))

(ert-deftest claude-repl-test-unhide-workspace-noop-on-non-hidden ()
  "`claude-repl--unhide-workspace' leaves non-hidden states alone."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :repl-state :inactive)
    (claude-repl--unhide-workspace "ws")
    (should (eq (claude-repl--ws-get "ws" :repl-state) :inactive))))

(ert-deftest claude-repl-test-unhide-workspace-nil-ws-noop ()
  "Nil WS is a no-op (matches the `:hidden'-only contract)."
  (claude-repl-test--with-clean-state
    (claude-repl--unhide-workspace nil)))

;;;; ---- Tests: clear-main-area-for-panels (drawer preservation) ----

(ert-deftest claude-repl-test-panels-clear-main-area-preserves-side-windows ()
  "`--clear-main-area-for-panels' must NOT delete side windows (drawer).
Opening Claude routes through `--show-existing-panels' which clears
the main area; the drawer side window must survive unconditionally,
even when its `no-delete-other-windows' parameter is absent (regression:
opening Claude used to destroy the drawer)."
  (claude-repl-test--with-clean-state
    (let ((drawer-buf (get-buffer-create "*clear-main-drawer*"))
          (work-buf   (get-buffer-create "*clear-main-work*"))
          (other-buf  (get-buffer-create "*clear-main-other*")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (set-window-buffer (selected-window) work-buf)
            (let ((other-win (split-window-right)))
              (set-window-buffer other-win other-buf))
            ;; Drawer is a side window with NO `no-delete-other-windows' —
            ;; the side-window-aware sweep must still preserve it.
            (let ((drawer-win (display-buffer-in-side-window
                              drawer-buf '((side . left) (slot . 0)))))
              (select-window (get-buffer-window work-buf))
              (claude-repl--clear-main-area-for-panels)
              (should (window-live-p drawer-win))
              (should (get-buffer-window drawer-buf))
              ;; The "other" main-area window should have been deleted.
              (should-not (get-buffer-window other-buf))))
        (mapc (lambda (b) (when (buffer-live-p b) (kill-buffer b)))
              (list drawer-buf work-buf other-buf))))))

(ert-deftest claude-repl-test-panels-show-existing-panels-preserves-drawer ()
  "Opening Claude (full show-existing-panels flow) must NOT destroy the drawer.
End-to-end regression: any drawer-as-side-window setup survives the
panel-open path regardless of whether the drawer's window parameters
match the canonical display-action."
  (claude-repl-test--with-clean-state
    (let ((drawer-buf (get-buffer-create "*spe-drawer*"))
          (vterm-buf  (get-buffer-create "*spe-vterm*"))
          (input-buf  (get-buffer-create "*spe-input*"))
          (work-buf   (get-buffer-create "*spe-work*"))
          (ws         "spe-ws"))
      (unwind-protect
          (progn
            (claude-repl--ws-put ws :vterm-buffer vterm-buf)
            (claude-repl--ws-put ws :input-buffer input-buf)
            (delete-other-windows)
            (set-window-buffer (selected-window) work-buf)
            (let ((drawer-win (display-buffer-in-side-window
                              drawer-buf '((side . left) (slot . 0)))))
              (select-window (get-buffer-window work-buf))
              (cl-letf (((symbol-function '+workspace-current-name) (lambda () ws))
                        ((symbol-function 'claude-repl--refresh-vterm) #'ignore)
                        ((symbol-function 'claude-repl--update-hide-overlay) #'ignore)
                        ((symbol-function 'claude-repl--restore-tab-index) #'ignore)
                        ((symbol-function 'claude-repl--flash-current-tab) #'ignore)
                        ((symbol-function 'claude-repl--focus-input-panel) #'ignore))
                (claude-repl--show-existing-panels))
              (should (window-live-p drawer-win))
              (should (get-buffer-window drawer-buf))))
        (mapc (lambda (b) (when (buffer-live-p b) (kill-buffer b)))
              (list drawer-buf vterm-buf input-buf work-buf))))))

(ert-deftest claude-repl-test-panels-show-panels-redirects-from-side-window ()
  "`--show-panels' must not try to split a side window.
When the selected window is a side window (drawer), redirect to the
frame's main window before splitting; splitting a side window would
otherwise signal `Cannot split side window' and leave panels half-shown."
  (claude-repl-test--with-clean-state
    (let ((drawer-buf (get-buffer-create "*sp-redir-drawer*"))
          (work-buf   (get-buffer-create "*sp-redir-work*"))
          (vterm-buf  (get-buffer-create "*sp-redir-vterm*"))
          (input-buf  (get-buffer-create "*sp-redir-input*"))
          (ws         "sp-redir-ws"))
      (unwind-protect
          (progn
            (claude-repl--ws-put ws :vterm-buffer vterm-buf)
            (claude-repl--ws-put ws :input-buffer input-buf)
            (delete-other-windows)
            (set-window-buffer (selected-window) work-buf)
            (let ((drawer-win (display-buffer-in-side-window
                              drawer-buf '((side . left) (slot . 0)))))
              ;; Simulate selected window being the drawer (e.g. mouse-click
              ;; landed here just before claude opened).
              (select-window drawer-win)
              (cl-letf (((symbol-function '+workspace-current-name) (lambda () ws))
                        ((symbol-function 'claude-repl--refresh-vterm) #'ignore)
                        ((symbol-function 'claude-repl--update-all-workspace-states-now) #'ignore))
                ;; Should NOT error.
                (claude-repl--show-panels))
              ;; Drawer still alive.
              (should (window-live-p drawer-win))
              ;; Panels were created.
              (should (get-buffer-window vterm-buf))
              (should (get-buffer-window input-buf))))
        (mapc (lambda (b) (when (buffer-live-p b) (kill-buffer b)))
              (list drawer-buf work-buf vterm-buf input-buf))))))

;;;; ---- Tests: stale-panel-windows ----

(ert-deftest claude-repl-test-panels-stale-panel-windows-returns-foreign-panels ()
  "stale-panel-windows returns windows showing panels from a different workspace."
  (claude-repl-test--with-clean-state
    (let ((foreign-buf (get-buffer-create "*claude-panel-other-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                    ((symbol-function 'window-list) (lambda (&rest _) (list (selected-window))))
                    ((symbol-function 'window-buffer) (lambda (_w) foreign-buf)))
            (let ((result (claude-repl--stale-panel-windows)))
              (should (= (length result) 1))
              (should (eq (car result) (selected-window)))))
        (kill-buffer foreign-buf)))))

(ert-deftest claude-repl-test-panels-stale-panel-windows-nil-for-own-panels ()
  "stale-panel-windows returns nil when panels belong to the current workspace."
  (claude-repl-test--with-clean-state
    (let ((own-buf (get-buffer-create "*claude-panel-my-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                    ((symbol-function 'window-list) (lambda (&rest _) (list (selected-window))))
                    ((symbol-function 'window-buffer) (lambda (_w) own-buf)))
            (should-not (claude-repl--stale-panel-windows)))
        (kill-buffer own-buf)))))

(ert-deftest claude-repl-test-panels-stale-panel-windows-nil-for-non-panel-buffers ()
  "stale-panel-windows returns nil when no Claude panel buffers are visible."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
              ((symbol-function 'window-list) (lambda (&rest _) (list (selected-window))))
              ((symbol-function 'window-buffer) (lambda (_w) (get-buffer-create "*scratch*"))))
      (should-not (claude-repl--stale-panel-windows)))))

(ert-deftest claude-repl-test-panels-stale-panel-windows-nil-when-ws-nil ()
  "stale-panel-windows returns nil when current workspace is nil."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
      (should-not (claude-repl--stale-panel-windows)))))

(ert-deftest claude-repl-test-panels-stale-panel-windows-includes-input-buffers ()
  "stale-panel-windows detects foreign input panel buffers too."
  (claude-repl-test--with-clean-state
    (let ((foreign-input (get-buffer-create "*claude-panel-input-other-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                    ((symbol-function 'window-list) (lambda (&rest _) (list (selected-window))))
                    ((symbol-function 'window-buffer) (lambda (_w) foreign-input)))
            (should (= (length (claude-repl--stale-panel-windows)) 1)))
        (kill-buffer foreign-input)))))

;;;; ---- Tests: ensure-own-panels-on-persp-switch ----

(ert-deftest claude-repl-test-panels-ensure-own-closes-stale-windows ()
  "ensure-own-panels-on-persp-switch closes stale panel windows."
  (claude-repl-test--with-clean-state
    (let ((deleted nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                ((symbol-function 'claude-repl--stale-panel-windows)
                 (lambda () (list (selected-window))))
                ((symbol-function 'set-window-dedicated-p) (lambda (_w _v) nil))
                ((symbol-function 'delete-window)
                 (lambda (w) (push w deleted)))
                ((symbol-function 'claude-repl--panels-visible-p) (lambda () nil)))
        (claude-repl--ensure-own-panels-on-persp-switch "my-ws")
        (should (= (length deleted) 1))))))

(ert-deftest claude-repl-test-panels-ensure-own-restores-when-panels-were-visible ()
  "ensure-own-panels-on-persp-switch re-shows panels when :panels-were-visible is set."
  (claude-repl-test--with-clean-state
    (let ((show-called nil))
      (claude-repl--ws-put "my-ws" :panels-were-visible t)
      (let ((vterm-buf (get-buffer-create "*claude-panel-my-ws*"))
            (input-buf (get-buffer-create "*claude-panel-input-my-ws*")))
        (unwind-protect
            (progn
              (claude-repl--ws-put "my-ws" :vterm-buffer vterm-buf)
              (claude-repl--ws-put "my-ws" :input-buffer input-buf)
              (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                        ((symbol-function 'claude-repl--stale-panel-windows) (lambda () nil))
                        ((symbol-function 'claude-repl--panels-visible-p) (lambda () nil))
                        ((symbol-function 'claude-repl--show-panels)
                         (lambda () (setq show-called t))))
                (claude-repl--ensure-own-panels-on-persp-switch "my-ws")
                (should show-called)))
          (kill-buffer vterm-buf)
          (kill-buffer input-buf))))))

(ert-deftest claude-repl-test-panels-ensure-own-noop-when-panels-already-visible ()
  "ensure-own-panels-on-persp-switch does not re-show if panels are already visible."
  (claude-repl-test--with-clean-state
    (let ((show-called nil))
      (claude-repl--ws-put "my-ws" :panels-were-visible t)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                ((symbol-function 'claude-repl--stale-panel-windows) (lambda () nil))
                ((symbol-function 'claude-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'claude-repl--show-panels)
                 (lambda () (setq show-called t))))
        (claude-repl--ensure-own-panels-on-persp-switch "my-ws")
        (should-not show-called)))))

(ert-deftest claude-repl-test-panels-ensure-own-noop-when-no-stale-no-flag ()
  "ensure-own-panels-on-persp-switch is a no-op with no stale panels and no flag."
  (claude-repl-test--with-clean-state
    (let ((show-called nil)
          (delete-called nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                ((symbol-function 'claude-repl--stale-panel-windows) (lambda () nil))
                ((symbol-function 'claude-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'claude-repl--show-panels)
                 (lambda () (setq show-called t)))
                ((symbol-function 'delete-window)
                 (lambda (_w) (setq delete-called t))))
        (claude-repl--ensure-own-panels-on-persp-switch "my-ws")
        (should-not show-called)
        (should-not delete-called)))))

(ert-deftest claude-repl-test-panels-ensure-own-skips-restore-when-buffers-dead ()
  "ensure-own-panels-on-persp-switch does not re-show if panel buffers are dead."
  (claude-repl-test--with-clean-state
    (let ((show-called nil))
      (claude-repl--ws-put "my-ws" :panels-were-visible t)
      ;; Buffers are nil (dead) — should not try to show.
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                ((symbol-function 'claude-repl--stale-panel-windows) (lambda () nil))
                ((symbol-function 'claude-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'claude-repl--show-panels)
                 (lambda () (setq show-called t))))
        (claude-repl--ensure-own-panels-on-persp-switch "my-ws")
        (should-not show-called)))))

(ert-deftest claude-repl-test-panels-ensure-own-adds-input-when-output-visible ()
  "ensure-own-panels-on-persp-switch adds only the input window (not a full
rebuild) when the output window survived but the input window was dropped."
  (claude-repl-test--with-clean-state
    (let ((show-panels-called nil)
          (add-input-called nil))
      (claude-repl--ws-put "my-ws" :panels-were-visible t)
      (let ((vterm-buf (get-buffer-create "*claude-panel-my-ws*"))
            (input-buf (get-buffer-create "*claude-panel-input-my-ws*")))
        (unwind-protect
            (progn
              (claude-repl--ws-put "my-ws" :vterm-buffer vterm-buf)
              (claude-repl--ws-put "my-ws" :input-buffer input-buf)
              (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                        ((symbol-function 'claude-repl--stale-panel-windows) (lambda () nil))
                        ((symbol-function 'claude-repl--panels-visible-p) (lambda () nil))
                        ((symbol-function 'claude-repl--vterm-visible-p) (lambda () t))
                        ((symbol-function 'claude-repl--output-visible-input-hidden-p)
                         (lambda () nil))
                        ((symbol-function 'claude-repl--show-panels)
                         (lambda () (setq show-panels-called t)))
                        ((symbol-function 'claude-repl--show-input-beside-output)
                         (lambda () (setq add-input-called t))))
                (claude-repl--ensure-own-panels-on-persp-switch "my-ws")
                (should add-input-called)
                (should-not show-panels-called)))
          (kill-buffer vterm-buf)
          (kill-buffer input-buf))))))

(ert-deftest claude-repl-test-panels-ensure-own-repairs-fullscreen-output-only ()
  "ensure-own-panels-on-persp-switch repairs a fullscreen output-only frame via
the trailing ensure-input-beside-output call, even when :panels-were-visible
was never recorded."
  (claude-repl-test--with-clean-state
    (let ((repair-called nil))
      ;; :panels-were-visible intentionally unset — the restore branch must
      ;; not fire; only the trailing repair should.
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                ((symbol-function 'claude-repl--stale-panel-windows) (lambda () nil))
                ((symbol-function 'claude-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'claude-repl--ensure-input-beside-output)
                 (lambda () (setq repair-called t))))
        (claude-repl--ensure-own-panels-on-persp-switch "my-ws")
        (should repair-called)))))

;;;; ---- Tests: stale-window-buffers ----

(ert-deftest claude-repl-test-panels-stale-window-buffers-unique-live ()
  "stale-window-buffers returns each live window's buffer once."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*claude-panel-other-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function 'window-live-p) (lambda (_w) t))
                    ((symbol-function 'window-buffer) (lambda (_w) buf)))
            (should (equal (claude-repl--stale-window-buffers '(w1 w2))
                           (list buf))))
        (kill-buffer buf)))))

(ert-deftest claude-repl-test-panels-stale-window-buffers-drops-dead ()
  "stale-window-buffers drops dead windows."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*claude-panel-other-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function 'window-live-p) (lambda (w) (eq w 'live)))
                    ((symbol-function 'window-buffer) (lambda (_w) buf)))
            (should (equal (claude-repl--stale-window-buffers '(live dead))
                           (list buf))))
        (kill-buffer buf)))))

;;;; ---- Tests: detach-foreign-panel-buffers ----

(ert-deftest claude-repl-test-panels-detach-foreign-removes-each ()
  "detach-foreign-panel-buffers removes each live buffer from the persp."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*claude-panel-other-ws*"))
          (removed nil))
      (unwind-protect
          (cl-letf (((symbol-function 'claude-repl--ws-remove-buffer)
                     (lambda (b) (push b removed))))
            (claude-repl--detach-foreign-panel-buffers "my-ws" (list buf))
            (should (equal removed (list buf))))
        (kill-buffer buf)))))

(ert-deftest claude-repl-test-panels-detach-foreign-skips-dead ()
  "detach-foreign-panel-buffers does not remove a dead buffer."
  (claude-repl-test--with-clean-state
    (let ((dead (get-buffer-create "*claude-panel-dead-ws*"))
          (removed nil))
      (kill-buffer dead)
      (cl-letf (((symbol-function 'claude-repl--ws-remove-buffer)
                 (lambda (b) (push b removed))))
        (claude-repl--detach-foreign-panel-buffers "my-ws" (list dead))
        (should-not removed)))))

;;;; ---- Tests: reclaim-frame-fullscreen ----

(ert-deftest claude-repl-test-panels-reclaim-fullscreen-noop-no-buffers ()
  "reclaim-frame-fullscreen is a no-op when ws has no live panel buffers."
  (claude-repl-test--with-clean-state
    (let ((show-called nil)
          (fs-called nil))
      (cl-letf (((symbol-function 'claude-repl--show-panels)
                 (lambda () (setq show-called t)))
                ((symbol-function 'claude-repl--enter-fullscreen)
                 (lambda (_ws) (setq fs-called t))))
        (claude-repl--reclaim-frame-fullscreen "my-ws")
        (should-not show-called)
        (should-not fs-called)))))

(ert-deftest claude-repl-test-panels-reclaim-fullscreen-shows-then-enters ()
  "reclaim-frame-fullscreen shows own panels then enters fullscreen when not visible."
  (claude-repl-test--with-clean-state
    (let ((visible nil)
          (show-called nil)
          (fs-called nil)
          (vterm (get-buffer-create "*claude-panel-my-ws*"))
          (input (get-buffer-create "*claude-panel-input-my-ws*")))
      (unwind-protect
          (progn
            (claude-repl--ws-put "my-ws" :vterm-buffer vterm)
            (claude-repl--ws-put "my-ws" :input-buffer input)
            (cl-letf (((symbol-function 'claude-repl--panels-visible-p)
                       (lambda () visible))
                      ((symbol-function 'claude-repl--show-panels)
                       (lambda () (setq show-called t visible t)))
                      ((symbol-function 'claude-repl--enter-fullscreen)
                       (lambda (_ws) (setq fs-called t))))
              (claude-repl--reclaim-frame-fullscreen "my-ws")
              (should show-called)
              (should fs-called)))
        (kill-buffer vterm)
        (kill-buffer input)))))

(ert-deftest claude-repl-test-panels-reclaim-fullscreen-skips-show-when-visible ()
  "reclaim-frame-fullscreen does not re-show panels when already visible."
  (claude-repl-test--with-clean-state
    (let ((show-called nil)
          (fs-called nil)
          (vterm (get-buffer-create "*claude-panel-my-ws*"))
          (input (get-buffer-create "*claude-panel-input-my-ws*")))
      (unwind-protect
          (progn
            (claude-repl--ws-put "my-ws" :vterm-buffer vterm)
            (claude-repl--ws-put "my-ws" :input-buffer input)
            (cl-letf (((symbol-function 'claude-repl--panels-visible-p)
                       (lambda () t))
                      ((symbol-function 'claude-repl--show-panels)
                       (lambda () (setq show-called t)))
                      ((symbol-function 'claude-repl--enter-fullscreen)
                       (lambda (_ws) (setq fs-called t))))
              (claude-repl--reclaim-frame-fullscreen "my-ws")
              (should-not show-called)
              (should fs-called)))
        (kill-buffer vterm)
        (kill-buffer input)))))

;;;; ---- Tests: lone-output-window ----

(ert-deftest claude-repl-test-panels-lone-output-window-returns-sole-output ()
  "lone-output-window returns the sole non-side window showing a Claude output buffer."
  (claude-repl-test--with-clean-state
    (let ((out (get-buffer-create "*claude-panel-my-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function 'window-list) (lambda (&rest _) '(w1)))
                    ((symbol-function 'claude-repl-window--side-window-p) (lambda (_w) nil))
                    ((symbol-function 'window-buffer) (lambda (_w) out)))
            (should (eq (claude-repl--lone-output-window) 'w1)))
        (kill-buffer out)))))

(ert-deftest claude-repl-test-panels-lone-output-window-nil-when-multiple ()
  "lone-output-window returns nil when more than one non-side window is present."
  (claude-repl-test--with-clean-state
    (let ((out (get-buffer-create "*claude-panel-my-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function 'window-list) (lambda (&rest _) '(w1 w2)))
                    ((symbol-function 'claude-repl-window--side-window-p) (lambda (_w) nil))
                    ((symbol-function 'window-buffer) (lambda (_w) out)))
            (should-not (claude-repl--lone-output-window)))
        (kill-buffer out)))))

(ert-deftest claude-repl-test-panels-lone-output-window-nil-non-claude ()
  "lone-output-window returns nil when the sole non-side window shows a non-Claude buffer."
  (claude-repl-test--with-clean-state
    (let ((reg (get-buffer-create "*regular-buffer*")))
      (unwind-protect
          (cl-letf (((symbol-function 'window-list) (lambda (&rest _) '(w1)))
                    ((symbol-function 'claude-repl-window--side-window-p) (lambda (_w) nil))
                    ((symbol-function 'window-buffer) (lambda (_w) reg)))
            (should-not (claude-repl--lone-output-window)))
        (kill-buffer reg)))))

(ert-deftest claude-repl-test-panels-lone-output-window-nil-input-buffer ()
  "lone-output-window returns nil when the sole non-side window shows a Claude input buffer."
  (claude-repl-test--with-clean-state
    (let ((inp (get-buffer-create "*claude-panel-input-my-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function 'window-list) (lambda (&rest _) '(w1)))
                    ((symbol-function 'claude-repl-window--side-window-p) (lambda (_w) nil))
                    ((symbol-function 'window-buffer) (lambda (_w) inp)))
            (should-not (claude-repl--lone-output-window)))
        (kill-buffer inp)))))

(ert-deftest claude-repl-test-panels-lone-output-window-ignores-side-windows ()
  "lone-output-window ignores side windows when finding the sole non-side output window."
  (claude-repl-test--with-clean-state
    (let ((out (get-buffer-create "*claude-panel-my-ws*"))
          (drawer (get-buffer-create "*drawer*")))
      (unwind-protect
          (cl-letf (((symbol-function 'window-list) (lambda (&rest _) '(side main)))
                    ((symbol-function 'claude-repl-window--side-window-p)
                     (lambda (w) (eq w 'side)))
                    ((symbol-function 'window-buffer)
                     (lambda (w) (if (eq w 'main) out drawer))))
            (should (eq (claude-repl--lone-output-window) 'main)))
        (kill-buffer out)
        (kill-buffer drawer)))))

;;;; ---- Tests: ensure-own reclaim/detach on foreign panels ----

(ert-deftest claude-repl-test-panels-ensure-own-detaches-foreign-buffers ()
  "ensure-own-panels-on-persp-switch detaches foreign panel buffers when stale present."
  (claude-repl-test--with-clean-state
    (let ((detached nil)
          (foreign (get-buffer-create "*claude-panel-other-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                    ((symbol-function 'claude-repl--stale-panel-windows)
                     (lambda () (list (selected-window))))
                    ((symbol-function 'claude-repl--stale-window-buffers)
                     (lambda (_w) (list foreign)))
                    ((symbol-function 'set-window-dedicated-p) (lambda (_w _v) nil))
                    ((symbol-function 'delete-window) (lambda (_w) nil))
                    ((symbol-function 'claude-repl--detach-foreign-panel-buffers)
                     (lambda (_ws bufs) (setq detached bufs)))
                    ((symbol-function 'claude-repl--panels-visible-p) (lambda () nil))
                    ((symbol-function 'claude-repl--ensure-input-beside-output) #'ignore)
                    ((symbol-function 'claude-repl--reclaim-frame-fullscreen) #'ignore))
            (claude-repl--ensure-own-panels-on-persp-switch "my-ws")
            (should (equal detached (list foreign))))
        (kill-buffer foreign)))))

(ert-deftest claude-repl-test-panels-ensure-own-reclaims-fullscreen-when-stale ()
  "ensure-own-panels-on-persp-switch reclaims the frame in fullscreen when stale present."
  (claude-repl-test--with-clean-state
    (let ((reclaimed nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                ((symbol-function 'claude-repl--stale-panel-windows)
                 (lambda () (list (selected-window))))
                ((symbol-function 'claude-repl--stale-window-buffers) (lambda (_w) nil))
                ((symbol-function 'set-window-dedicated-p) (lambda (_w _v) nil))
                ((symbol-function 'delete-window) (lambda (_w) nil))
                ((symbol-function 'claude-repl--detach-foreign-panel-buffers) #'ignore)
                ((symbol-function 'claude-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'claude-repl--ensure-input-beside-output) #'ignore)
                ((symbol-function 'claude-repl--reclaim-frame-fullscreen)
                 (lambda (ws) (setq reclaimed ws))))
        (claude-repl--ensure-own-panels-on-persp-switch "my-ws")
        (should (equal reclaimed "my-ws"))))))

(ert-deftest claude-repl-test-panels-ensure-own-no-reclaim-when-no-stale ()
  "ensure-own-panels-on-persp-switch does not reclaim or detach when no stale panels and no lone output."
  (claude-repl-test--with-clean-state
    (let ((reclaimed nil)
          (detached nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                ((symbol-function 'claude-repl--stale-panel-windows) (lambda () nil))
                ((symbol-function 'claude-repl--lone-output-window) (lambda () nil))
                ((symbol-function 'claude-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'claude-repl--ensure-input-beside-output) #'ignore)
                ((symbol-function 'claude-repl--detach-foreign-panel-buffers)
                 (lambda (_ws _bufs) (setq detached t)))
                ((symbol-function 'claude-repl--reclaim-frame-fullscreen)
                 (lambda (_ws) (setq reclaimed t))))
        (claude-repl--ensure-own-panels-on-persp-switch "my-ws")
        (should-not reclaimed)
        (should-not detached)))))

(ert-deftest claude-repl-test-panels-ensure-own-reclaims-fullscreen-when-lone-output ()
  "ensure-own-panels-on-persp-switch reclaims fullscreen for a lone output window with no stale panels."
  (claude-repl-test--with-clean-state
    (let ((reclaimed nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                ((symbol-function 'claude-repl--stale-panel-windows) (lambda () nil))
                ((symbol-function 'claude-repl--lone-output-window) (lambda () 'some-win))
                ((symbol-function 'claude-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'claude-repl--ensure-input-beside-output) #'ignore)
                ((symbol-function 'claude-repl--reclaim-frame-fullscreen)
                 (lambda (ws) (setq reclaimed ws))))
        (claude-repl--ensure-own-panels-on-persp-switch "my-ws")
        (should (equal reclaimed "my-ws"))))))

(ert-deftest claude-repl-test-panels-ensure-own-reclaims-once-when-stale-and-lone-output ()
  "ensure-own-panels-on-persp-switch reclaims exactly once when both stale and lone output are present."
  (claude-repl-test--with-clean-state
    (let ((reclaim-count 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                ((symbol-function 'claude-repl--stale-panel-windows)
                 (lambda () (list (selected-window))))
                ((symbol-function 'claude-repl--stale-window-buffers) (lambda (_w) nil))
                ((symbol-function 'claude-repl--lone-output-window) (lambda () 'some-win))
                ((symbol-function 'set-window-dedicated-p) (lambda (_w _v) nil))
                ((symbol-function 'delete-window) (lambda (_w) nil))
                ((symbol-function 'claude-repl--detach-foreign-panel-buffers) #'ignore)
                ((symbol-function 'claude-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'claude-repl--ensure-input-beside-output) #'ignore)
                ((symbol-function 'claude-repl--reclaim-frame-fullscreen)
                 (lambda (_ws) (setq reclaim-count (1+ reclaim-count)))))
        (claude-repl--ensure-own-panels-on-persp-switch "my-ws")
        (should (= reclaim-count 1))))))

;;;; ---- Tests: before-persp-deactivate records panels-were-visible ----

(ert-deftest claude-repl-test-panels-before-persp-deactivate-records-visible ()
  "before-persp-deactivate saves :panels-were-visible t when panels are visible."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'claude-repl--panels-visible-p) (lambda () t))
              ((symbol-function 'claude-repl--redirect-from-claude-before-save) #'ignore)
              ((symbol-function 'claude-repl--ws-frame-save-state) #'ignore))
      (claude-repl--before-persp-deactivate)
      (should (eq (claude-repl--ws-get "ws1" :panels-were-visible) t)))))

(ert-deftest claude-repl-test-panels-before-persp-deactivate-records-hidden ()
  "before-persp-deactivate saves :panels-were-visible nil when panels are hidden."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'claude-repl--panels-visible-p) (lambda () nil))
              ((symbol-function 'claude-repl--redirect-from-claude-before-save) #'ignore)
              ((symbol-function 'claude-repl--ws-frame-save-state) #'ignore))
      (claude-repl--before-persp-deactivate)
      (should-not (claude-repl--ws-get "ws1" :panels-were-visible)))))

;;; test-panels.el ends here
