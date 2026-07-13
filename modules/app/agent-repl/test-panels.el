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

(ert-deftest agent-repl-test-panels-input-visible-p-with-visible-buffer ()
  "input-visible-p returns non-nil when the input buffer is in a window."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*test-input*"
      (agent-repl--ws-put "test-ws" :input-buffer (current-buffer))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'get-buffer-window)
                 (lambda (_buf &rest _) (selected-window))))
        ;; Mock get-buffer-window: batch mode has no real display
        (should (agent-repl--input-visible-p))))))

(ert-deftest agent-repl-test-panels-input-visible-p-no-buffer ()
  "input-visible-p returns nil when no input buffer is set."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
      (should-not (agent-repl--input-visible-p)))))

(ert-deftest agent-repl-test-panels-input-visible-p-dead-buffer ()
  "input-visible-p returns nil when the input buffer has been killed."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*test-dead-input*")))
      (agent-repl--ws-put "test-ws" :input-buffer buf)
      (kill-buffer buf)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
        (should-not (agent-repl--input-visible-p))))))

(ert-deftest agent-repl-test-panels-vterm-visible-p-no-buffer ()
  "vterm-visible-p returns nil when no vterm buffer is set."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
      (should-not (agent-repl--vterm-visible-p)))))

(ert-deftest agent-repl-test-panels-panels-visible-p-both-nil ()
  "panels-visible-p returns nil when neither panel exists."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
      (should-not (agent-repl--panels-visible-p)))))

;;;; ---- Tests: output-visible-input-hidden-p ----

(ert-deftest agent-repl-test-panels-output-visible-input-hidden-p-true ()
  "output-visible-input-hidden-p is t when output is visible but input is not."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'agent-repl--vterm-visible-p) (lambda () t))
              ((symbol-function 'agent-repl--input-visible-p) (lambda () nil)))
      (should (agent-repl--output-visible-input-hidden-p)))))

(ert-deftest agent-repl-test-panels-output-visible-input-hidden-p-both-visible ()
  "output-visible-input-hidden-p is nil when both panels are visible."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'agent-repl--vterm-visible-p) (lambda () t))
              ((symbol-function 'agent-repl--input-visible-p) (lambda () t)))
      (should-not (agent-repl--output-visible-input-hidden-p)))))

(ert-deftest agent-repl-test-panels-output-visible-input-hidden-p-output-hidden ()
  "output-visible-input-hidden-p is nil when the output panel is not visible."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'agent-repl--vterm-visible-p) (lambda () nil))
              ((symbol-function 'agent-repl--input-visible-p) (lambda () nil)))
      (should-not (agent-repl--output-visible-input-hidden-p)))))

;;;; ---- Tests: Safe buffer name ----

(ert-deftest agent-repl-test-panels-safe-buffer-name-nil ()
  "safe-buffer-name returns nil for nil input."
  (should-not (agent-repl--safe-buffer-name nil)))

(ert-deftest agent-repl-test-panels-safe-buffer-name-live-buffer ()
  "safe-buffer-name returns the name for a live buffer."
  (agent-repl-test--with-temp-buffer "*safe-name-test*"
    (should (equal (agent-repl--safe-buffer-name (current-buffer))
                   "*safe-name-test*"))))

;;;; ---- Tests: Extract panel hex ----

(ert-deftest agent-repl-test-panels-extract-id-from-vterm ()
  "extract-panel-id returns workspace identifier from a vterm buffer name."
  (should (equal (agent-repl--extract-panel-id "*agent-panel-abcd1234*")
                 "abcd1234"))
  (should (equal (agent-repl--extract-panel-id "*agent-panel-my-workspace*")
                 "my-workspace")))

(ert-deftest agent-repl-test-panels-extract-id-from-input ()
  "extract-panel-id returns workspace identifier from an input buffer name."
  (should (equal (agent-repl--extract-panel-id "*agent-panel-input-abcd1234*")
                 "abcd1234"))
  (should (equal (agent-repl--extract-panel-id "*agent-panel-input-my-workspace*")
                 "my-workspace")))

(ert-deftest agent-repl-test-panels-extract-id-non-agent ()
  "extract-panel-id returns nil for non-agent buffer names."
  (should-not (agent-repl--extract-panel-id "*scratch*"))
  (should-not (agent-repl--extract-panel-id "*Messages*"))
  (should-not (agent-repl--extract-panel-id "config.el")))

;;;; ---- Tests: Partner buffer name ----

(ert-deftest agent-repl-test-panels-partner-of-vterm ()
  "partner-buffer-name of a vterm buffer is the input buffer."
  (should (equal (agent-repl--partner-buffer-name "*agent-panel-abcd1234*" "abcd1234")
                 "*agent-panel-input-abcd1234*")))

(ert-deftest agent-repl-test-panels-partner-of-input ()
  "partner-buffer-name of an input buffer is the vterm buffer."
  (should (equal (agent-repl--partner-buffer-name "*agent-panel-input-abcd1234*" "abcd1234")
                 "*agent-panel-abcd1234*")))

;;;; ---- Tests: Orphaned panel detection (migrated) ----

(ert-deftest agent-repl-test-panels-orphaned-vterm-p ()
  "A vterm buffer whose input partner is not visible is orphaned."
  (agent-repl-test--with-clean-state
    ;; Mock: not one-window-p, no partner window visible
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window) (lambda (_buf) nil))
              ((symbol-function 'get-buffer) (lambda (_name) nil)))
      ;; Vterm with no visible input partner is orphaned
      (should (agent-repl--orphaned-panel-p "*agent-panel-abcd1234*"))
      ;; Non-agent buffers are never orphaned
      (should-not (agent-repl--orphaned-panel-p "*some-other*")))))

(ert-deftest agent-repl-test-panels-orphaned-input-p ()
  "An input buffer whose vterm partner is not visible is orphaned (no loading placeholder)."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window) (lambda (_buf) nil))
              ((symbol-function 'get-buffer) (lambda (_name) nil)))
      ;; Input with no visible vterm partner and no loading placeholder is orphaned
      (should (agent-repl--orphaned-panel-p "*agent-panel-input-abcd1234*"))
      ;; Non-agent buffers are never orphaned
      (should-not (agent-repl--orphaned-panel-p "*scratch*")))))

(ert-deftest agent-repl-test-panels-input-not-orphaned-under-webview ()
  "Hybrid UI: an input panel under the workspace's visible webview is a live pair."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window)
               (lambda (buf) (and (equal buf "*agent-frontend-abcd1234*") 'fake-window)))
              ((symbol-function 'get-buffer) (lambda (_name) nil)))
      ;; The input panel is protected by the visible webview...
      (should-not (agent-repl--orphaned-panel-p "*agent-panel-input-abcd1234*"))
      ;; ...but the webview does NOT protect a vterm panel.
      (should (agent-repl--orphaned-panel-p "*agent-panel-abcd1234*")))))

(ert-deftest agent-repl-test-panels-orphaned-vterm-one-window ()
  "When one-window-p returns t, no panel is considered orphaned."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () t)))
      (should-not (agent-repl--orphaned-panel-p "*agent-panel-abcd1234*")))))

(ert-deftest agent-repl-test-panels-orphaned-input-with-loading ()
  "When loading placeholder buffer exists, input panel is not orphaned."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window) (lambda (_buf) nil))
              ((symbol-function 'get-buffer) (lambda (name)
                                               (when (equal name " *agent-loading*")
                                                 'fake-buffer))))
      (should-not (agent-repl--orphaned-panel-p "*agent-panel-input-abcd1234*")))))

(ert-deftest agent-repl-test-panels-orphaned-vterm-partner-visible ()
  "A vterm buffer whose input partner IS visible is not orphaned."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window)
               (lambda (buf)
                 ;; The input partner window is visible
                 (when (equal buf "*agent-panel-input-abcd1234*")
                   'fake-window))))
      (should-not (agent-repl--orphaned-panel-p "*agent-panel-abcd1234*")))))

(ert-deftest agent-repl-test-panels-orphaned-input-partner-visible ()
  "An input buffer whose vterm partner IS visible is not orphaned."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window)
               (lambda (buf)
                 ;; The vterm partner window is visible
                 (when (equal buf "*agent-panel-abcd1234*")
                   'fake-window))))
      (should-not (agent-repl--orphaned-panel-p "*agent-panel-input-abcd1234*")))))

;;;; ---- Tests: Defcustom defaults ----

;;;; ---- Tests: Docstring accuracy (migrated) ----

(ert-deftest agent-repl-test-panels-show-panels-docstring ()
  "show-panels docstring should describe the fullscreen (frame-filling) layout."
  (let ((doc (documentation 'agent-repl--show-panels)))
    (should (string-match-p "fullscreen" doc))
    (should (string-match-p "fill" doc))))

;;;; ---- Tests: show-input-beside-output ----

(ert-deftest agent-repl-test-panels-show-input-beside-output-splits-output ()
  "show-input-beside-output splits the output window and shows the input buffer."
  (agent-repl-test--with-clean-state
    (let* ((input-buf (get-buffer-create "*agent-panel-input-test-ws*"))
           (vterm-win (selected-window))
           (split-arg nil)
           (set-win nil)
           (set-buf nil)
           (hardened nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                      ((symbol-function 'agent-repl-window--panel-window)
                       (lambda (_kind &rest _) vterm-win))
                      ((symbol-function 'split-window)
                       (lambda (win &rest _) (setq split-arg win) 'input-win))
                      ((symbol-function 'window-total-height) (lambda (_w) 40))
                      ((symbol-function 'set-window-buffer)
                       (lambda (w b) (setq set-win w set-buf b)))
                      ((symbol-function 'agent-repl-window--harden)
                       (lambda (w &rest _) (setq hardened w))))
              (should (eq (agent-repl--show-input-beside-output) 'input-win))
              ;; Split happens on the existing output window.
              (should (eq split-arg vterm-win))
              ;; Input buffer is shown in the new window and it is hardened.
              (should (eq set-win 'input-win))
              (should (eq set-buf input-buf))
              (should (eq hardened 'input-win))))
        (kill-buffer input-buf)))))

(ert-deftest agent-repl-test-panels-show-input-beside-output-noop-no-output-window ()
  "show-input-beside-output is a no-op when the output window is not visible."
  (agent-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*agent-panel-input-test-ws*"))
          (split-called nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                      ((symbol-function 'agent-repl-window--panel-window)
                       (lambda (_kind &rest _) nil))
                      ((symbol-function 'split-window)
                       (lambda (&rest _) (setq split-called t) 'input-win)))
              (should-not (agent-repl--show-input-beside-output))
              (should-not split-called)))
        (kill-buffer input-buf)))))

(ert-deftest agent-repl-test-panels-show-input-beside-output-noop-dead-input ()
  "show-input-beside-output is a no-op when the input buffer is not live."
  (agent-repl-test--with-clean-state
    (let ((split-called nil))
      ;; No :input-buffer set — buffer is nil/dead.
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl-window--panel-window)
                 (lambda (_kind &rest _) (selected-window)))
                ((symbol-function 'split-window)
                 (lambda (&rest _) (setq split-called t) 'input-win)))
        (should-not (agent-repl--show-input-beside-output))
        (should-not split-called)))))

;;;; ---- Tests: ensure-input-beside-output ----

(ert-deftest agent-repl-test-panels-ensure-input-beside-output-repairs ()
  "ensure-input-beside-output adds the input window when output is up, input down."
  (agent-repl-test--with-clean-state
    (let ((shown nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--output-visible-input-hidden-p)
                 (lambda () t))
                ((symbol-function 'agent-repl--show-input-beside-output)
                 (lambda () (setq shown t))))
        (agent-repl--ensure-input-beside-output)
        (should shown)))))

(ert-deftest agent-repl-test-panels-ensure-input-beside-output-noop ()
  "ensure-input-beside-output is a no-op when the layout is not half-shown."
  (agent-repl-test--with-clean-state
    (let ((shown nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--output-visible-input-hidden-p)
                 (lambda () nil))
                ((symbol-function 'agent-repl--show-input-beside-output)
                 (lambda () (setq shown t))))
        (agent-repl--ensure-input-beside-output)
        (should-not shown)))))

;;;; ---- Tests: drain-pending-show-panels ----

(ert-deftest agent-repl-test-panels-drain-pending-when-set-and-ready ()
  "drain-pending-show-panels shows panels and clears the flag when the agent is ready."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :frontend 'vterm)
    (agent-repl--ws-put "test-ws" :pending-show-panels t)
    (let ((called nil))
      (cl-letf (((symbol-function 'agent-repl--session-starting-p) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--show-hidden-panels)
                 (lambda () (setq called t))))
        (agent-repl--drain-pending-show-panels "test-ws")
        (should called)
        (should-not (agent-repl--ws-get "test-ws" :pending-show-panels))))))

(ert-deftest agent-repl-test-panels-drain-pending-shows-gui-frontend ()
  "drain-pending-show-panels shows a GUI workspace through the gui show capability."
  (agent-repl-test--with-clean-state
    ;; Arrange — a generated gui workspace: session booted headlessly, view
    ;; deferred to the first switch, which is where this drain runs.
    (agent-repl--ws-put "test-ws" :frontend 'gui)
    (agent-repl--ws-put "test-ws" :pending-show-panels t)
    (let ((shown nil)
          (vterm-panels nil))
      (cl-letf (((symbol-function 'agent-repl--session-starting-p) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--gui-show)
                 (lambda (ws) (setq shown ws)))
                ((symbol-function 'agent-repl--show-hidden-panels)
                 (lambda () (setq vterm-panels t))))
        ;; Act
        (agent-repl--drain-pending-show-panels "test-ws")
        ;; Assert
        (should (equal shown "test-ws"))
        (should-not vterm-panels)
        (should-not (agent-repl--ws-get "test-ws" :pending-show-panels))))))

(ert-deftest agent-repl-test-panels-drain-pending-when-set-but-starting ()
  "drain-pending-show-panels defers (leaves flag set, no show) when session is starting."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :frontend 'vterm)
    (agent-repl--ws-put "test-ws" :pending-show-panels t)
    (let ((called nil))
      (cl-letf (((symbol-function 'agent-repl--session-starting-p) (lambda (_ws) t))
                ((symbol-function 'agent-repl--show-hidden-panels)
                 (lambda () (setq called t))))
        (agent-repl--drain-pending-show-panels "test-ws")
        (should-not called)
        (should (agent-repl--ws-get "test-ws" :pending-show-panels))))))

(ert-deftest agent-repl-test-panels-drain-pending-when-not-set ()
  "drain-pending-show-panels does nothing when flag is nil."
  (agent-repl-test--with-clean-state
    (let ((called nil))
      (cl-letf (((symbol-function 'agent-repl--show-hidden-panels)
                 (lambda () (setq called t))))
        (agent-repl--drain-pending-show-panels "test-ws")
        (should-not called)))))

;;;; ---- Tests: drain-pending-magit ----

(ert-deftest agent-repl-test-panels-drain-pending-magit-when-set ()
  "drain-pending-magit calls magit-status with :project-dir and clears the flag."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :pending-magit t)
    (agent-repl--ws-put "test-ws" :project-dir "/tmp/my-worktree")
    (let ((magit-path nil)
          (dash-called nil))
      (cl-letf (((symbol-function 'magit-status)
                 (lambda (path) (setq magit-path path)))
                ((symbol-function 'agent-repl--remove-doom-dashboard)
                 (lambda () (setq dash-called t))))
        (agent-repl--drain-pending-magit "test-ws")
        (should (equal magit-path "/tmp/my-worktree"))
        (should dash-called)
        (should-not (agent-repl--ws-get "test-ws" :pending-magit))))))

(ert-deftest agent-repl-test-panels-drain-pending-magit-when-not-set ()
  "drain-pending-magit does nothing when :pending-magit flag is nil."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :project-dir "/tmp/my-worktree")
    (let ((magit-called nil)
          (dash-called nil))
      (cl-letf (((symbol-function 'magit-status)
                 (lambda (&rest _) (setq magit-called t)))
                ((symbol-function 'agent-repl--remove-doom-dashboard)
                 (lambda () (setq dash-called t))))
        (agent-repl--drain-pending-magit "test-ws")
        (should-not magit-called)
        (should-not dash-called)))))

(ert-deftest agent-repl-test-panels-drain-pending-magit-only-once ()
  "drain-pending-magit is one-shot: a second activation does not reopen magit."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :pending-magit t)
    (agent-repl--ws-put "test-ws" :project-dir "/tmp/my-worktree")
    (let ((magit-call-count 0))
      (cl-letf (((symbol-function 'magit-status)
                 (lambda (&rest _) (cl-incf magit-call-count)))
                ((symbol-function 'agent-repl--remove-doom-dashboard) #'ignore))
        (agent-repl--drain-pending-magit "test-ws")
        (agent-repl--drain-pending-magit "test-ws")
        (should (equal magit-call-count 1))))))

(ert-deftest agent-repl-test-panels-drain-pending-magit-windowless-when-panels-pending ()
  "drain-pending-magit leaves the window tree untouched when panels are pending.
The panels drain (which runs after this one) opens the fullscreen
layout as the sole main-area display, so the magit buffer must be
created without a window."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :pending-magit t)
    (agent-repl--ws-put "test-ws" :pending-show-panels t)
    (agent-repl--ws-put "test-ws" :project-dir "/tmp/my-worktree")
    (let ((windows-before (length (window-list))))
      (cl-letf (((symbol-function 'magit-status)
                 (lambda (_path) (split-window)))
                ((symbol-function 'agent-repl--remove-doom-dashboard) #'ignore))
        (agent-repl--drain-pending-magit "test-ws")
        (should (equal (length (window-list)) windows-before))))))

(ert-deftest agent-repl-test-panels-drain-pending-magit-displays-without-panels-pending ()
  "drain-pending-magit still displays magit when no panel show is pending.
The no-agent `SPC TAB n' path has no fullscreen panels coming, so magit
remains the workspace's visible main buffer."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :pending-magit t)
    (agent-repl--ws-put "test-ws" :project-dir "/tmp/my-worktree")
    (let ((windows-before (length (window-list))))
      (unwind-protect
          (cl-letf (((symbol-function 'magit-status)
                     (lambda (_path) (split-window)))
                    ((symbol-function 'agent-repl--remove-doom-dashboard) #'ignore))
            (agent-repl--drain-pending-magit "test-ws")
            (should (equal (length (window-list)) (1+ windows-before))))
        (delete-other-windows)))))

(ert-deftest agent-repl-test-panels-drain-pending-magit-no-project-dir ()
  "drain-pending-magit clears the flag but skips magit-status when :project-dir is missing.
Defensive: :project-dir is always written by setup-worktree-session before
finalize returns, so this path shouldn't occur in practice — but a missing
path must not error."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :pending-magit t)
    (let ((magit-called nil))
      (cl-letf (((symbol-function 'magit-status)
                 (lambda (&rest _) (setq magit-called t)))
                ((symbol-function 'agent-repl--remove-doom-dashboard) #'ignore))
        (agent-repl--drain-pending-magit "test-ws")
        (should-not magit-called)
        (should-not (agent-repl--ws-get "test-ws" :pending-magit))))))

;;;; ---- Tests: drain-pending-initial-buffers ----

(ert-deftest agent-repl-test-panels-drain-pending-initial-buffers-when-set ()
  "drain-pending-initial-buffers calls open-initial-buffers with WS and :project-dir, clears the flag."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :pending-initial-buffers t)
    (agent-repl--ws-put "test-ws" :project-dir "/tmp/my-worktree")
    (let ((call-args nil))
      (cl-letf (((symbol-function 'agent-repl--open-initial-buffers)
                 (lambda (ws path) (setq call-args (list ws path)))))
        (agent-repl--drain-pending-initial-buffers "test-ws")
        (should (equal call-args '("test-ws" "/tmp/my-worktree")))
        (should-not (agent-repl--ws-get "test-ws" :pending-initial-buffers))))))

(ert-deftest agent-repl-test-panels-drain-pending-initial-buffers-when-not-set ()
  "drain-pending-initial-buffers does nothing when :pending-initial-buffers is nil."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :project-dir "/tmp/my-worktree")
    (let ((open-called nil))
      (cl-letf (((symbol-function 'agent-repl--open-initial-buffers)
                 (lambda (&rest _) (setq open-called t))))
        (agent-repl--drain-pending-initial-buffers "test-ws")
        (should-not open-called)))))

(ert-deftest agent-repl-test-panels-drain-pending-initial-buffers-only-once ()
  "drain-pending-initial-buffers is one-shot: a second activation does not re-open."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :pending-initial-buffers t)
    (agent-repl--ws-put "test-ws" :project-dir "/tmp/my-worktree")
    (let ((call-count 0))
      (cl-letf (((symbol-function 'agent-repl--open-initial-buffers)
                 (lambda (&rest _) (cl-incf call-count))))
        (agent-repl--drain-pending-initial-buffers "test-ws")
        (agent-repl--drain-pending-initial-buffers "test-ws")
        (should (equal call-count 1))))))

(ert-deftest agent-repl-test-panels-drain-pending-initial-buffers-no-project-dir ()
  "drain-pending-initial-buffers clears the flag but skips the call when :project-dir is missing.
Defensive: :project-dir is always written by setup-worktree-session before
finalize returns, so this path shouldn't occur in practice — but a missing
path must not error."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :pending-initial-buffers t)
    (let ((open-called nil))
      (cl-letf (((symbol-function 'agent-repl--open-initial-buffers)
                 (lambda (&rest _) (setq open-called t))))
        (agent-repl--drain-pending-initial-buffers "test-ws")
        (should-not open-called)
        (should-not (agent-repl--ws-get "test-ws" :pending-initial-buffers))))))

;;;; ---- Tests: close-buffer-window ----

(ert-deftest agent-repl-test-panels-close-buffer-window-no-window ()
  "close-buffer-window silently does nothing when buffer has no window."
  (agent-repl-test--with-temp-buffer "*close-test*"
    ;; Buffer exists but is not displayed in any window (beyond selected)
    ;; This should not error
    (agent-repl--close-buffer-window (get-buffer "*not-a-buffer*"))))

;;;; ---- Tests: close-buffer-windows ----

(ert-deftest agent-repl-test-panels-close-buffer-windows-nil-args ()
  "close-buffer-windows handles nil buffers gracefully."
  (agent-repl-test--with-clean-state
    ;; Should not error with nil arguments
    (agent-repl--close-buffer-windows nil nil)))

(ert-deftest agent-repl-test-panels-close-buffer-windows-dead-buffer ()
  "close-buffer-windows skips dead buffers."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*dead-buf-test*")))
      (kill-buffer buf)
      ;; Should not error with a dead buffer
      (agent-repl--close-buffer-windows buf))))

;;;; ---- Tests: configure-vterm-window ----

(ert-deftest agent-repl-test-panels-configure-vterm-window ()
  "configure-vterm-window sets dedicated + width-fixed + no-delete-other-windows.
Does NOT set `no-other-window' — keyboard isolation now comes from
`agent-repl--bounce-from-vterm', so vterm stays visible to
`other-window'/`windmove' but any non-mouse landing is auto-corrected."
  (let ((win (selected-window)))
    (unwind-protect
        (progn
          (agent-repl--configure-vterm-window win)
          (should (window-dedicated-p win))
          (should-not (window-parameter win 'no-other-window))
          (should (eq (window-parameter win 'window-size-fixed) 'width))
          (should (window-parameter win 'no-delete-other-windows)))
      ;; Clean up window parameters
      (set-window-dedicated-p win nil)
      (set-window-parameter win 'window-size-fixed nil)
      (set-window-parameter win 'no-delete-other-windows nil))))

;;;; ---- Tests: resolve-vterm-buffer ----

(ert-deftest agent-repl-test-panels-resolve-vterm-buffer-from-non-vterm ()
  "resolve-vterm-buffer looks up workspace vterm when not in vterm-mode."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*test-vterm-resolve*"
      (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
        (should (eq (agent-repl--resolve-vterm-buffer) (current-buffer)))))))

(ert-deftest agent-repl-test-panels-resolve-vterm-buffer-no-workspace ()
  "resolve-vterm-buffer returns nil when no workspace is active."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
      (should-not (agent-repl--resolve-vterm-buffer)))))

;;;; ---- Tests: kill-placeholder ----

(ert-deftest agent-repl-test-panels-kill-placeholder-when-exists ()
  "kill-placeholder kills the loading placeholder buffer."
  (agent-repl-test--with-clean-state
    (get-buffer-create " *agent-loading*")
    (should (get-buffer " *agent-loading*"))
    (agent-repl--kill-placeholder)
    (should-not (get-buffer " *agent-loading*"))))

(ert-deftest agent-repl-test-panels-kill-placeholder-when-absent ()
  "kill-placeholder does nothing when no placeholder exists."
  (agent-repl-test--with-clean-state
    ;; Ensure no placeholder exists
    (when-let ((buf (get-buffer " *agent-loading*")))
      (kill-buffer buf))
    ;; Should not error
    (agent-repl--kill-placeholder)))

;;;; ---- Tests: sigkill-if-alive ----

(ert-deftest agent-repl-test-panels-sigkill-if-alive-dead-process ()
  "sigkill-if-alive does nothing for a dead (nil) process."
  ;; Should not error when process is nil / not live
  (agent-repl--sigkill-if-alive nil))

;;;; ---- Tests: non-agent-panel-window-p ----

(ert-deftest agent-repl-test-panels-non-agent-panel-window-p ()
  "non-agent-panel-window-p returns t for non-agent windows."
  (let ((win (selected-window)))
    ;; The selected window should be showing *scratch* or similar
    (should (agent-repl--non-agent-panel-window-p win))))

;;;; ---- Tests: on-close (single close audit point) ----

(ert-deftest agent-repl-test-panels-on-close-calls-hide-panels ()
  "on-close invokes hide-panels for a VTERM workspace.
The teardown is dispatched through the workspace's own frontend, so the
workspace has to say which one it is — an undeclared frontend resolves to
the gui default and tears down a webview instead."
  (agent-repl-test--with-clean-state
    (let ((hide-called nil))
      (agent-repl--ws-put "test-ws" :frontend 'vterm)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--hide-panels)
                 (lambda () (setq hide-called t)))
                ((symbol-function 'agent-repl-workspace-push-to-back) #'ignore))
        (agent-repl--on-close)
        (should hide-called)))))

;;;; ---- Tests: a gui close is a real close ----
;;
;; The bug these close: the gui branch of `--toggle' called the frontend's
;; hide capability directly, so a gui workspace put its view away with NONE
;; of the bookkeeping a close carries — no `:repl-state', so hide-mode could
;; never sweep it, and on `SPC o C' no deprio and no session kill either.

(ert-deftest agent-repl-test-panels-gui-simple-close-marks-inactive ()
  "`SPC o c' on a gui workspace records the close as `:inactive'."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :frontend 'gui)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'agent-repl--frontend-dispatch-hide) #'ignore))
      (agent-repl--on-simple-close)
      (should (eq :inactive (agent-repl--ws-get "test-ws" :repl-state))))))

(ert-deftest agent-repl-test-panels-gui-close-marks-hidden ()
  "`SPC o C' on a gui workspace records the close as `:hidden'.
Without this the workspace can never become a hide-mode sweep candidate."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :frontend 'gui)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'agent-repl--frontend-dispatch-hide) #'ignore)
              ((symbol-function 'agent-repl--save-tab-index) #'ignore)
              ((symbol-function 'agent-repl-workspace-push-to-back) #'ignore))
      (agent-repl--on-close)
      (should (eq :hidden (agent-repl--ws-get "test-ws" :repl-state))))))

(ert-deftest agent-repl-test-panels-gui-close-tears-down-the-webview ()
  "A gui close puts the WEBVIEW away, never the vterm panels it does not have."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :frontend 'gui)
    (let ((hidden-ws 'unset) (vterm-hide-called 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--frontend-dispatch-hide)
                 (lambda (ws) (setq hidden-ws ws)))
                ((symbol-function 'agent-repl--hide-panels)
                 (lambda () (cl-incf vterm-hide-called))))
        (agent-repl--on-simple-close)
        (should (equal hidden-ws "test-ws"))
        (should (= 0 vterm-hide-called))))))

(ert-deftest agent-repl-test-panels-vterm-close-still-tears-down-its-panels ()
  "A vterm close is unchanged: it never routes through the frontend hide."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :frontend 'vterm)
    (agent-repl--ws-put "test-ws" :fullscreen-config 'saved-config)
    (let ((dispatched 0) (hide-called 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'set-window-configuration) #'ignore)
                ((symbol-function 'agent-repl--frontend-dispatch-hide)
                 (lambda (_ws) (cl-incf dispatched)))
                ((symbol-function 'agent-repl--hide-panels)
                 (lambda () (cl-incf hide-called))))
        (agent-repl--on-simple-close)
        (should (= 0 dispatched))
        (should (= 1 hide-called))))))

(ert-deftest agent-repl-test-panels-on-close-restores-config-before-hide ()
  "on-close restores the pre-panel layout before hiding panels.
The restore must run BEFORE hide-panels so the frame-filling panels are
removed via the restored work layout rather than stranding a panel."
  (agent-repl-test--with-clean-state
    (let ((order '()))
      (agent-repl--ws-put "test-ws" :frontend 'vterm)
      (agent-repl--ws-put "test-ws" :fullscreen-config 'saved-config)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'set-window-configuration)
                 (lambda (_cfg) (push 'restore order)))
                ((symbol-function 'agent-repl--hide-panels)
                 (lambda () (push 'hide order)))
                ((symbol-function 'agent-repl-workspace-push-to-back) #'ignore))
        (agent-repl--on-close)
        (should (equal order '(hide restore)))
        (should-not (agent-repl--ws-get "test-ws" :fullscreen-config))))))

(ert-deftest agent-repl-test-panels-on-close-with-explicit-ws ()
  "on-close accepts an explicit WS argument."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ignored"))
              ((symbol-function 'agent-repl--hide-panels) (lambda () nil)))
      (agent-repl--on-close "specific-ws")
      (should (eq (agent-repl--ws-get "specific-ws" :repl-state) :hidden))
      (should-not (agent-repl--ws-get "ignored" :repl-state)))))

(ert-deftest agent-repl-test-panels-on-close-nil-ws-still-hides ()
  "on-close with nil workspace hides panels but skips bookkeeping."
  (agent-repl-test--with-clean-state
    (let ((hide-called nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
                ((symbol-function 'agent-repl--hide-panels)
                 (lambda () (setq hide-called t))))
        (agent-repl--on-close)
        (should hide-called)))))

(ert-deftest agent-repl-test-panels-on-close-sets-repl-state-hidden ()
  "on-close (deprio path) writes :repl-state :hidden so the workspace is a
sweep candidate when hide-mode is enabled.  Distinct from on-simple-close
which writes :inactive."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'agent-repl--hide-panels) (lambda () nil))
              ((symbol-function 'agent-repl-workspace-push-to-back) #'ignore))
      (agent-repl--on-close)
      (should (eq (agent-repl--ws-get "test-ws" :repl-state) :hidden)))))

(ert-deftest agent-repl-test-panels-on-close-preserves-agent-state ()
  "on-close does not touch :agent-state — mid-task :thinking survives close."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "test-ws" :thinking)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'agent-repl--hide-panels) (lambda () nil))
              ((symbol-function 'agent-repl-workspace-push-to-back) #'ignore))
      (agent-repl--on-close)
      (should (eq (agent-repl--ws-agent-state "test-ws") :thinking))
      (should (eq (agent-repl--ws-get "test-ws" :repl-state) :hidden)))))

(ert-deftest agent-repl-test-panels-on-close-pushes-current-ws-to-back ()
  "on-close calls `agent-repl-workspace-push-to-back' when WS is the current workspace."
  (agent-repl-test--with-clean-state
    (let ((push-called 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--hide-panels) (lambda () nil))
                ((symbol-function 'agent-repl-workspace-push-to-back)
                 (lambda (&optional _keep) (cl-incf push-called))))
        (agent-repl--on-close)
        (should (= push-called 1))))))

(ert-deftest agent-repl-test-panels-on-close-does-not-keep-focus ()
  "on-close calls push-to-back without KEEP-FOCUS so focus moves to a fresh workspace."
  (agent-repl-test--with-clean-state
    (let ((received-args 'unset))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--hide-panels) (lambda () nil))
                ((symbol-function 'agent-repl-workspace-push-to-back)
                 (lambda (&rest args) (setq received-args args))))
        (agent-repl--on-close)
        (should (equal received-args nil))))))

(ert-deftest agent-repl-test-panels-on-close-skips-push-when-explicit-ws-not-current ()
  "on-close does not push to back when an explicit WS is not the current workspace."
  (agent-repl-test--with-clean-state
    (let ((push-called 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
                ((symbol-function 'agent-repl--hide-panels) (lambda () nil))
                ((symbol-function 'agent-repl-workspace-push-to-back)
                 (lambda (&optional _keep) (cl-incf push-called))))
        (agent-repl--on-close "other-ws")
        (should (= push-called 0))))))

(ert-deftest agent-repl-test-panels-on-close-skips-push-when-no-workspace ()
  "on-close does not push to back when no workspace is active."
  (agent-repl-test--with-clean-state
    (let ((push-called 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
                ((symbol-function 'agent-repl--hide-panels) (lambda () nil))
                ((symbol-function 'agent-repl-workspace-push-to-back)
                 (lambda (&optional _keep) (cl-incf push-called))))
        (agent-repl--on-close)
        (should (= push-called 0))))))

(ert-deftest agent-repl-test-panels-on-close-saves-tab-index-before-pushing ()
  "on-close calls save-tab-index before push so the captured index is the original."
  (agent-repl-test--with-clean-state
    (let ((calls nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--hide-panels) (lambda () nil))
                ((symbol-function 'agent-repl--save-tab-index)
                 (lambda (_ws) (push 'save calls)))
                ((symbol-function 'agent-repl-workspace-push-to-back)
                 (lambda (&optional _keep) (push 'push calls))))
        (agent-repl--on-close)
        (should (equal (reverse calls) '(save push)))))))

;;;; ---- Tests: on-simple-close (no-deprio variant) ----

(ert-deftest agent-repl-test-panels-on-simple-close-sets-inactive ()
  "on-simple-close writes :repl-state :inactive."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'agent-repl--hide-panels) (lambda () nil)))
      (agent-repl--on-simple-close)
      (should (eq :inactive (agent-repl--ws-get "test-ws" :repl-state))))))

(ert-deftest agent-repl-test-panels-on-simple-close-hides-panels ()
  "on-simple-close calls hide-panels when a saved layout is restored.
The workspace declares `:frontend vterm': the teardown is dispatched
through the workspace's own frontend now, so a workspace that never says
which one it is resolves to the gui default and tears down a webview
instead — which is exactly the point of the dispatch."
  (agent-repl-test--with-clean-state
    (let ((hide-called 0))
      (agent-repl--ws-put "test-ws" :frontend 'vterm)
      (agent-repl--ws-put "test-ws" :fullscreen-config 'saved-config)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'set-window-configuration) #'ignore)
                ((symbol-function 'agent-repl--hide-panels)
                 (lambda () (cl-incf hide-called))))
        (agent-repl--on-simple-close)
        (should (= 1 hide-called))))))

(ert-deftest agent-repl-test-panels-on-simple-close-does-not-save-tab-index ()
  "on-simple-close does NOT call save-tab-index — that's the deprio path."
  (agent-repl-test--with-clean-state
    (let ((save-called 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--hide-panels) (lambda () nil))
                ((symbol-function 'agent-repl--save-tab-index)
                 (lambda (_ws) (cl-incf save-called))))
        (agent-repl--on-simple-close)
        (should (= 0 save-called))))))

(ert-deftest agent-repl-test-panels-on-simple-close-does-not-push-to-back ()
  "on-simple-close does NOT call push-to-back — that's the deprio path."
  (agent-repl-test--with-clean-state
    (let ((push-called 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--hide-panels) (lambda () nil))
                ((symbol-function 'agent-repl-workspace-push-to-back)
                 (lambda (&rest _) (cl-incf push-called))))
        (agent-repl--on-simple-close)
        (should (= 0 push-called))))))

(ert-deftest agent-repl-test-panels-on-simple-close-exits-fullscreen-before-hide ()
  "on-simple-close restores the pre-fullscreen layout before hiding panels.
The restore must run BEFORE hide-panels so hide-panels deletes the panels
from the restored splitscreen layout rather than from the full-frame one."
  (agent-repl-test--with-clean-state
    (let ((order '()))
      (agent-repl--ws-put "test-ws" :frontend 'vterm)
      (agent-repl--ws-put "test-ws" :fullscreen-config 'saved-config)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'set-window-configuration)
                 (lambda (_cfg) (push 'restore order)))
                ((symbol-function 'agent-repl--hide-panels)
                 (lambda () (push 'hide order))))
        (agent-repl--on-simple-close)
        (should (equal order '(hide restore)))))))

(ert-deftest agent-repl-test-panels-on-simple-close-clears-fullscreen-config ()
  "on-simple-close clears :fullscreen-config when exiting fullscreen."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :fullscreen-config 'saved-config)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'set-window-configuration) #'ignore)
              ((symbol-function 'agent-repl--hide-panels) #'ignore))
      (agent-repl--on-simple-close)
      (should-not (agent-repl--ws-get "test-ws" :fullscreen-config)))))

(ert-deftest agent-repl-test-panels-on-simple-close-no-restore-without-config ()
  "on-simple-close does NOT call set-window-configuration when not fullscreen."
  (agent-repl-test--with-clean-state
    (let ((restore-called 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'set-window-configuration)
                 (lambda (_cfg) (cl-incf restore-called)))
                ((symbol-function 'agent-repl--replace-panels-with-fallback) #'ignore))
        ;; No :fullscreen-config set on test-ws.
        (agent-repl--on-simple-close)
        (should (= 0 restore-called))))))

(ert-deftest agent-repl-test-panels-on-simple-close-no-config-routes-to-fallback ()
  "on-simple-close with no saved layout replaces panels with the fallback buffer.
Rather than `hide-panels' (which would strand the output window), the
no-`:fullscreen-config' branch routes to
`agent-repl--replace-panels-with-fallback'."
  (agent-repl-test--with-clean-state
    (let ((hide-called 0) (replace-ws 'unset))
      (agent-repl--ws-put "test-ws" :frontend 'vterm)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'set-window-configuration) #'ignore)
                ((symbol-function 'agent-repl--hide-panels)
                 (lambda () (cl-incf hide-called)))
                ((symbol-function 'agent-repl--replace-panels-with-fallback)
                 (lambda (ws) (setq replace-ws ws))))
        ;; No :fullscreen-config set on test-ws.
        (agent-repl--on-simple-close)
        (should (equal replace-ws "test-ws"))
        (should (= 0 hide-called))))))

(ert-deftest agent-repl-test-panels-on-simple-close-with-config-does-not-fallback ()
  "on-simple-close with a saved layout hides panels and does NOT use the fallback.
The restore-succeeded branch keeps the historical `hide-panels' behavior."
  (agent-repl-test--with-clean-state
    (let ((replace-called 0))
      (agent-repl--ws-put "test-ws" :fullscreen-config 'saved-config)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'set-window-configuration) #'ignore)
                ((symbol-function 'agent-repl--hide-panels) #'ignore)
                ((symbol-function 'agent-repl--replace-panels-with-fallback)
                 (lambda (_ws) (cl-incf replace-called))))
        (agent-repl--on-simple-close)
        (should (= 0 replace-called))))))

(ert-deftest agent-repl-test-panels-on-simple-close-fullscreen-leaves-work-window ()
  "on-simple-close on a fullscreen ws removes panels and leaves the work window.
End-to-end with real windows: a fullscreen layout (only the two panels)
plus a saved splitscreen config restores to work+panels, then hides the
panels, leaving just the work window — the `SPC o c' goes-away contract."
  (agent-repl-test--with-clean-state
    (let ((wconf (current-window-configuration))
          (work-buf (generate-new-buffer "*fsclose-work*"))
          (vterm-buf (generate-new-buffer "*agent-panel-fsclose*"))
          (input-buf (generate-new-buffer "*agent-panel-input-fsclose*")))
      (agent-repl--ws-put "test-ws" :frontend 'vterm)
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
            (delete-other-windows)
            (let* ((work-win (selected-window))
                   (vterm-win (split-window work-win nil 'right))
                   (input-win (split-window vterm-win nil 'below)))
              (set-window-buffer work-win work-buf)
              (set-window-buffer vterm-win vterm-buf)
              (set-window-buffer input-win input-buf)
              (agent-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
              (agent-repl--ws-put "test-ws" :input-buffer input-buf)
              ;; Capture the splitscreen layout as the pre-fullscreen config.
              (agent-repl--ws-put "test-ws" :fullscreen-config
                                   (current-window-configuration))
              ;; Enter fullscreen: delete the work window, leaving only panels.
              (delete-window work-win)
              (should-not (window-live-p work-win))
              ;; SPC o c.
              (agent-repl--on-simple-close)
              ;; Panels are gone.
              (should-not (get-buffer-window vterm-buf))
              (should-not (get-buffer-window input-buf))
              ;; The work window's buffer is back onscreen.
              (should (get-buffer-window work-buf))
              ;; Fullscreen config was cleared.
              (should-not (agent-repl--ws-get "test-ws" :fullscreen-config))))
        (set-window-configuration wconf)
        (kill-buffer work-buf)
        (kill-buffer vterm-buf)
        (kill-buffer input-buf)))))

;;;; ---- Tests: restore-fullscreen-config ----

(ert-deftest agent-repl-test-panels-restore-fullscreen-config-restores-and-clears ()
  "restore-fullscreen-config restores the saved config and clears the flag."
  (agent-repl-test--with-clean-state
    (let ((restored nil))
      (agent-repl--ws-put "test-ws" :fullscreen-config 'saved-config)
      (cl-letf (((symbol-function 'set-window-configuration)
                 (lambda (cfg) (setq restored cfg))))
        (should (agent-repl--restore-fullscreen-config "test-ws"))
        (should (eq restored 'saved-config))
        (should-not (agent-repl--ws-get "test-ws" :fullscreen-config))))))

(ert-deftest agent-repl-test-panels-restore-fullscreen-config-noop-without-config ()
  "restore-fullscreen-config returns nil and does nothing with no saved config."
  (agent-repl-test--with-clean-state
    (let ((restore-called 0))
      (cl-letf (((symbol-function 'set-window-configuration)
                 (lambda (_cfg) (cl-incf restore-called))))
        (should-not (agent-repl--restore-fullscreen-config "test-ws"))
        (should (= 0 restore-called))))))

(ert-deftest agent-repl-test-panels-restore-fullscreen-config-noop-on-nil-ws ()
  "restore-fullscreen-config returns nil when WS is nil."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'set-window-configuration)
               (lambda (_cfg) (error "should not restore"))))
      (should-not (agent-repl--restore-fullscreen-config nil)))))

;;;; ---- Tests: workspace-magit-status-buffer ----

(ert-deftest agent-repl-test-panels-workspace-magit-status-buffer-finds-match ()
  "workspace-magit-status-buffer returns a magit-status buffer whose dir matches."
  (agent-repl-test--with-clean-state
    (let ((magit-buf (generate-new-buffer "*magit-match*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :project-dir "/repo")
            (with-current-buffer magit-buf
              (setq major-mode 'magit-status-mode)
              (setq default-directory "/repo/"))
            (cl-letf (((symbol-function 'agent-repl--git-root) (lambda (_) "/repo"))
                      ((symbol-function 'agent-repl--path-canonical)
                       (lambda (p) (directory-file-name p))))
              (should (eq magit-buf
                          (agent-repl--workspace-magit-status-buffer "test-ws")))))
        (kill-buffer magit-buf)))))

(ert-deftest agent-repl-test-panels-workspace-magit-status-buffer-dir-mismatch-nil ()
  "workspace-magit-status-buffer returns nil when the only magit buffer's dir differs."
  (agent-repl-test--with-clean-state
    (let ((magit-buf (generate-new-buffer "*magit-other*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :project-dir "/repo")
            (with-current-buffer magit-buf
              (setq major-mode 'magit-status-mode)
              (setq default-directory "/other/"))
            (cl-letf (((symbol-function 'agent-repl--git-root) (lambda (_) "/repo"))
                      ((symbol-function 'agent-repl--path-canonical)
                       (lambda (p) (directory-file-name p))))
              (should-not (agent-repl--workspace-magit-status-buffer "test-ws"))))
        (kill-buffer magit-buf)))))

(ert-deftest agent-repl-test-panels-workspace-magit-status-buffer-non-magit-nil ()
  "workspace-magit-status-buffer ignores a matching-dir buffer that is not magit-status."
  (agent-repl-test--with-clean-state
    (let ((plain-buf (generate-new-buffer "*plain-repo*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :project-dir "/repo")
            (with-current-buffer plain-buf
              (setq major-mode 'fundamental-mode)
              (setq default-directory "/repo/"))
            (cl-letf (((symbol-function 'agent-repl--git-root) (lambda (_) "/repo"))
                      ((symbol-function 'agent-repl--path-canonical)
                       (lambda (p) (directory-file-name p))))
              (should-not (agent-repl--workspace-magit-status-buffer "test-ws"))))
        (kill-buffer plain-buf)))))

;;;; ---- Tests: panel-fallback-buffer ----

(ert-deftest agent-repl-test-panels-panel-fallback-buffer-prefers-magit ()
  "panel-fallback-buffer returns the workspace magit-status buffer when one exists."
  (agent-repl-test--with-clean-state
    (let ((magit-buf (generate-new-buffer "*magit-pref*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--workspace-magit-status-buffer)
                     (lambda (_ws) magit-buf)))
            (should (eq magit-buf (agent-repl--panel-fallback-buffer "test-ws"))))
        (kill-buffer magit-buf)))))

(ert-deftest agent-repl-test-panels-panel-fallback-buffer-falls-back-to-doom ()
  "panel-fallback-buffer returns the Doom splash when no magit-status buffer exists."
  (agent-repl-test--with-clean-state
    (let ((splash (generate-new-buffer "*splash-fb*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--workspace-magit-status-buffer)
                     (lambda (_ws) nil))
                    ((symbol-function 'doom-fallback-buffer) (lambda () splash)))
            (should (eq splash (agent-repl--panel-fallback-buffer "test-ws"))))
        (kill-buffer splash)))))

(ert-deftest agent-repl-test-panels-panel-fallback-buffer-errors-when-none ()
  "panel-fallback-buffer signals when neither a magit nor a Doom fallback exists."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--workspace-magit-status-buffer)
               (lambda (_ws) nil))
              ((symbol-function 'doom-fallback-buffer) (lambda () nil)))
      (should-error (agent-repl--panel-fallback-buffer "test-ws")))))

;;;; ---- Tests: replace-panels-with-fallback ----

(ert-deftest agent-repl-test-panels-replace-panels-with-fallback-swaps-output ()
  "replace-panels-with-fallback closes the input window and swaps the output
window's buffer to the fallback, leaving a single window on the fallback buffer."
  (agent-repl-test--with-clean-state
    (let ((wconf (current-window-configuration))
          (vterm-buf (generate-new-buffer "*agent-panel-repl-fb*"))
          (input-buf (generate-new-buffer "*agent-panel-input-repl-fb*"))
          (fallback-buf (generate-new-buffer "*repl-fb-fallback*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--panel-fallback-buffer)
                     (lambda (_ws) fallback-buf)))
            (agent-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (delete-other-windows)
            (let* ((vterm-win (selected-window))
                   (input-win (split-window vterm-win nil 'below)))
              (set-window-buffer vterm-win vterm-buf)
              (set-window-buffer input-win input-buf)
              (agent-repl--replace-panels-with-fallback "test-ws")
              ;; Input window is gone.
              (should-not (get-buffer-window input-buf))
              ;; The output window survives but now shows the fallback buffer.
              (should (get-buffer-window fallback-buf))
              (should-not (get-buffer-window vterm-buf))))
        (set-window-configuration wconf)
        (kill-buffer vterm-buf)
        (kill-buffer input-buf)
        (kill-buffer fallback-buf)))))

(ert-deftest agent-repl-test-panels-replace-panels-with-fallback-noop-no-buffers ()
  "replace-panels-with-fallback is a no-op when the workspace has no panel buffers."
  (agent-repl-test--with-clean-state
    (let ((fallback-called 0))
      (cl-letf (((symbol-function 'agent-repl--panel-fallback-buffer)
                 (lambda (_ws) (cl-incf fallback-called) nil)))
        ;; No :vterm-buffer / :input-buffer on test-ws.
        (agent-repl--replace-panels-with-fallback "test-ws")
        ;; Fallback buffer is never computed when there is no output window.
        (should (= 0 fallback-called))))))

;;;; ---- Tests: simple-hide-and-preserve-status ----

(ert-deftest agent-repl-test-panels-simple-hide-routes-through-on-simple-close ()
  "simple-hide-and-preserve-status delegates to on-simple-close."
  (agent-repl-test--with-clean-state
    (let ((received-ws nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--on-simple-close)
                 (lambda (&optional ws) (setq received-ws ws))))
        (agent-repl--simple-hide-and-preserve-status)
        (should (equal received-ws "test-ws"))))))

(ert-deftest agent-repl-test-panels-simple-hide-no-workspace-errors ()
  "simple-hide-and-preserve-status errors when no workspace is active."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
      (should-error (agent-repl--simple-hide-and-preserve-status)))))

;;;; ---- Tests: agent-repl-simple toggle ----

(ert-deftest agent-repl-test-panels-agent-repl-simple-uses-simple-hide ()
  "agent-repl-simple dispatches the visible-panels case to simple-hide."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((simple-called 0)
          (full-called 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--agent-running-p) (lambda () t))
                ((symbol-function 'agent-repl--session-starting-p) (lambda () nil))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'agent-repl--simple-hide-and-preserve-status)
                 (lambda () (cl-incf simple-called)))
                ((symbol-function 'agent-repl--hide-and-preserve-status)
                 (lambda () (cl-incf full-called))))
        (agent-repl-simple)
        (should (= 1 simple-called))
        (should (= 0 full-called))))))

(ert-deftest agent-repl-test-panels-agent-repl-uses-full-hide ()
  "agent-repl (deprio variant) dispatches the visible-panels case to hide-and-preserve."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((simple-called 0)
          (full-called 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--agent-running-p) (lambda () t))
                ((symbol-function 'agent-repl--session-starting-p) (lambda () nil))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'agent-repl--simple-hide-and-preserve-status)
                 (lambda () (cl-incf simple-called)))
                ((symbol-function 'agent-repl--hide-and-preserve-status)
                 (lambda () (cl-incf full-called))))
        (agent-repl)
        (should (= 0 simple-called))
        (should (= 1 full-called))))))

;;;; ---- Tests: save-tab-index ----

(ert-deftest agent-repl-test-panels-save-tab-index-writes-position ()
  "save-tab-index records the workspace's persp index as :saved-tab-index."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-names-current-frame-fast-ordered)
               (lambda () '("a" "b" "test-ws" "c"))))
      (agent-repl--save-tab-index "test-ws")
      (should (= 2 (agent-repl--ws-get "test-ws" :saved-tab-index))))))

(ert-deftest agent-repl-test-panels-save-tab-index-skips-when-not-in-list ()
  "save-tab-index is a no-op when the workspace name isn't in the persp list."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-names-current-frame-fast-ordered)
               (lambda () '("a" "b"))))
      (agent-repl--save-tab-index "missing-ws")
      (should-not (agent-repl--ws-get "missing-ws" :saved-tab-index)))))

(ert-deftest agent-repl-test-panels-save-tab-index-skips-when-persp-unavailable ()
  "save-tab-index is a no-op when persp helper is not bound."
  (agent-repl-test--with-clean-state
    (when (fboundp 'persp-names-current-frame-fast-ordered)
      (fmakunbound 'persp-names-current-frame-fast-ordered))
    (agent-repl--save-tab-index "test-ws")
    (should-not (agent-repl--ws-get "test-ws" :saved-tab-index))))

;;;; ---- Tests: restore-tab-index ----

(ert-deftest agent-repl-test-panels-restore-tab-index-moves-ws-to-saved-slot ()
  "restore-tab-index reorders names so WS is at its saved index."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws-c" :saved-tab-index 1)
    (let ((reordered nil))
      (cl-letf (((symbol-function 'persp-names-current-frame-fast-ordered)
                 (lambda () '("ws-a" "ws-b" "ws-c")))  ; ws-c is at end
                ((symbol-function 'persp-update-names-cache)
                 (lambda (names) (setq reordered names))))
        (agent-repl--restore-tab-index "ws-c")
        (should (equal reordered '("ws-a" "ws-c" "ws-b")))))))

(ert-deftest agent-repl-test-panels-restore-tab-index-clears-saved-index ()
  "restore-tab-index clears :saved-tab-index after a successful restore."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws-c" :saved-tab-index 0)
    (cl-letf (((symbol-function 'persp-names-current-frame-fast-ordered)
               (lambda () '("ws-a" "ws-b" "ws-c")))
              ((symbol-function 'persp-update-names-cache) (lambda (_) nil)))
      (agent-repl--restore-tab-index "ws-c")
      (should-not (agent-repl--ws-get "ws-c" :saved-tab-index)))))

(ert-deftest agent-repl-test-panels-restore-tab-index-clamps-past-tail ()
  "restore-tab-index clamps a saved index larger than the new list length."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws-c" :saved-tab-index 99)
    (let ((reordered nil))
      (cl-letf (((symbol-function 'persp-names-current-frame-fast-ordered)
                 (lambda () '("ws-a" "ws-b" "ws-c")))
                ((symbol-function 'persp-update-names-cache)
                 (lambda (names) (setq reordered names))))
        (agent-repl--restore-tab-index "ws-c")
        ;; Clamped: ws-c lands at the tail of the without-ws list.
        (should (equal reordered '("ws-a" "ws-b" "ws-c")))))))

(ert-deftest agent-repl-test-panels-restore-tab-index-noop-when-no-saved-index ()
  "restore-tab-index does nothing when no :saved-tab-index is set."
  (agent-repl-test--with-clean-state
    (let ((called 0))
      (cl-letf (((symbol-function 'persp-names-current-frame-fast-ordered)
                 (lambda () '("a")))
                ((symbol-function 'persp-update-names-cache)
                 (lambda (_) (cl-incf called))))
        (agent-repl--restore-tab-index "no-saved-ws")
        (should (= 0 called))))))

;;;; ---- Tests: hide-and-preserve-status ----

(ert-deftest agent-repl-test-panels-hide-and-preserve-marks-hidden ()
  "hide-and-preserve-status routes through on-close (deprio path) and sets
:repl-state :hidden so the workspace is a sweep candidate when hide-mode
is on."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :frontend 'vterm)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'agent-repl--hide-panels) (lambda () nil))
              ((symbol-function 'agent-repl-workspace-push-to-back) #'ignore)
              ;; SPC o C now also kills through the registry; resolve a
              ;; probe frontend so no real teardown runs (the struct
              ;; accessor itself resists cl-letf via its compiler macro).
              ((symbol-function 'agent-repl--ws-frontend)
               (lambda (_ws) (agent-repl-frontend-create
                              :name 'probe :open-fn #'ignore
                              :kill-fn #'ignore :send-fn #'ignore
                              :interrupt-fn #'ignore :running-p-fn #'ignore
                              :supported-backends '(claude)))))
      (agent-repl--hide-and-preserve-status)
      (should (eq (agent-repl--ws-get "test-ws" :repl-state) :hidden)))))

(ert-deftest agent-repl-test-panels-hide-and-preserve-no-workspace-errors ()
  "hide-and-preserve-status errors when no workspace is active."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
      (should-error (agent-repl--hide-and-preserve-status)))))

(ert-deftest agent-repl-test-panels-hide-and-preserve-routes-through-on-close ()
  "hide-and-preserve-status delegates to on-close."
  (agent-repl-test--with-clean-state
    (let ((on-close-ws nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--on-close)
                 (lambda (&optional ws) (setq on-close-ws ws)))
                ((symbol-function 'agent-repl--ws-frontend)
               (lambda (_ws) (agent-repl-frontend-create
                              :name 'probe :open-fn #'ignore
                              :kill-fn #'ignore :send-fn #'ignore
                              :interrupt-fn #'ignore :running-p-fn #'ignore
                              :supported-backends '(claude)))))
        (agent-repl--hide-and-preserve-status)
        (should (equal on-close-ws "test-ws"))))))

;;;; ---- Tests: show-hidden-panels ----

(ert-deftest agent-repl-test-panels-show-hidden-calls-show-existing ()
  "show-hidden-panels calls show-existing-panels."
  (agent-repl-test--with-clean-state
    (let ((show-called nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--show-existing-panels)
                 (lambda () (setq show-called t))))
        (agent-repl--show-hidden-panels)
        (should show-called)))))

(ert-deftest agent-repl-test-panels-show-hidden-sets-active ()
  "show-hidden-panels (via show-existing-panels) sets :repl-state :active."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-repl-state "test-ws" :inactive)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'agent-repl--refresh-vterm) #'ignore)
              ((symbol-function 'delete-other-windows) #'ignore)
              ((symbol-function 'agent-repl--show-panels-and-focus) #'ignore)
              ((symbol-function 'agent-repl--update-hide-overlay) #'ignore))
      (agent-repl--show-hidden-panels)
      (should (eq (agent-repl--ws-get "test-ws" :repl-state) :active)))))

(ert-deftest agent-repl-test-panels-show-existing-sets-active ()
  "show-existing-panels sets :repl-state :active."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-repl-state "test-ws" :inactive)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'agent-repl--refresh-vterm) #'ignore)
              ((symbol-function 'delete-other-windows) #'ignore)
              ((symbol-function 'agent-repl--show-panels-and-focus) #'ignore)
              ((symbol-function 'agent-repl--update-hide-overlay) #'ignore))
      (agent-repl--show-existing-panels)
      (should (eq (agent-repl--ws-get "test-ws" :repl-state) :active)))))

(ert-deftest agent-repl-test-panels-show-existing-restores-tab-index ()
  "show-existing-panels calls restore-tab-index for the current workspace."
  (agent-repl-test--with-clean-state
    (let ((restored-ws nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--refresh-vterm) #'ignore)
                ((symbol-function 'delete-other-windows) #'ignore)
                ((symbol-function 'agent-repl--show-panels-and-focus) #'ignore)
                ((symbol-function 'agent-repl--update-hide-overlay) #'ignore)
                ((symbol-function 'agent-repl--restore-tab-index)
                 (lambda (ws) (setq restored-ws ws))))
        (agent-repl--show-existing-panels)
        (should (equal restored-ws "test-ws"))))))

(ert-deftest agent-repl-test-panels-show-existing-flashes-tab ()
  "show-existing-panels pulses the workspace tab so reopen is visually marked."
  (agent-repl-test--with-clean-state
    (let ((flashed-ws nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--refresh-vterm) #'ignore)
                ((symbol-function 'delete-other-windows) #'ignore)
                ((symbol-function 'agent-repl--show-panels-and-focus) #'ignore)
                ((symbol-function 'agent-repl--update-hide-overlay) #'ignore)
                ((symbol-function 'agent-repl--restore-tab-index) #'ignore)
                ((symbol-function 'agent-repl-flash-tab)
                 (lambda (ws &rest _) (setq flashed-ws ws))))
        (agent-repl--show-existing-panels)
        (should (equal flashed-ws "test-ws"))))))

;;;; ---- Tests: deferred macro ----

(ert-deftest agent-repl-test-panels-deferred-debounces ()
  "The deferred macro cancels a pending timer and schedules a new one."
  (agent-repl-test--with-clean-state
    (let ((test-timer nil)
          (call-count 0))
      (let ((debounced (agent-repl--deferred test-timer
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

;;;; ---- Tests: Entry point (agent-repl) dispatch ----

(ert-deftest agent-repl-test-panels-entry-point-not-running-hides ()
  "agent-repl (SPC o C, always-close) hides the workspace even when no
agent session is running.  Skips the initialize-agent branch the
plain `agent-repl-simple' (SPC o c) toggle would otherwise take."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((started nil) (hidden nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--agent-running-p) (lambda () nil))
                ((symbol-function 'agent-repl--session-starting-p) (lambda () nil))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'agent-repl--initialize-agent) (lambda (&rest _) (setq started t)))
                ((symbol-function 'agent-repl--hide-and-preserve-status)
                 (lambda () (setq hidden t))))
        (agent-repl)
        (should hidden)
        (should-not started)))))

(ert-deftest agent-repl-test-panels-entry-point-session-starting-hides ()
  "agent-repl hides the workspace mid-startup rather than showing a loading
message — always-close skips the loading branch."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((messages nil) (hidden nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--agent-running-p) (lambda () t))
                ((symbol-function 'agent-repl--session-starting-p) (lambda () t))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'message) (lambda (fmt &rest _) (push fmt messages)))
                ((symbol-function 'agent-repl--hide-and-preserve-status)
                 (lambda () (setq hidden t))))
        (agent-repl)
        (should hidden)
        (should-not (cl-some (lambda (m) (and m (string-match-p "loading" m))) messages))))))

(ert-deftest agent-repl-test-panels-entry-point-visible-hides ()
  "agent-repl hides panels when they are visible."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((hidden nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--agent-running-p) (lambda () t))
                ((symbol-function 'agent-repl--session-starting-p) (lambda () nil))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'agent-repl--hide-and-preserve-status)
                 (lambda () (setq hidden t))))
        (agent-repl)
        (should hidden)))))

(ert-deftest agent-repl-test-panels-entry-point-hidden-still-hides ()
  "agent-repl hides the workspace even when panels are already hidden — the
always-close contract: pressing SPC o C on a hidden workspace re-asserts
:hidden + push-to-back instead of re-showing the panels."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((shown nil) (hidden nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--agent-running-p) (lambda () t))
                ((symbol-function 'agent-repl--session-starting-p) (lambda () nil))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'agent-repl--show-hidden-panels)
                 (lambda () (setq shown t)))
                ((symbol-function 'agent-repl--hide-and-preserve-status)
                 (lambda () (setq hidden t))))
        (agent-repl)
        (should hidden)
        (should-not shown)))))

(ert-deftest agent-repl-test-panels-entry-point-selection-sends ()
  "agent-repl sends selected text to the agent when region is active.
Selection-handling stays orthogonal to the always-close hide path."
  (agent-repl-test--with-clean-state
    (let ((sent-text nil) (hidden nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--agent-running-p) (lambda () t))
                ((symbol-function 'agent-repl--session-starting-p) (lambda () nil))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'region-beginning) (lambda () 1))
                ((symbol-function 'region-end) (lambda () 12))
                ((symbol-function 'buffer-substring-no-properties)
                 (lambda (_beg _end) "hello world"))
                ((symbol-function 'deactivate-mark) (lambda () nil))
                ((symbol-function 'agent-repl--hide-and-preserve-status)
                 (lambda () (setq hidden t)))
                ((symbol-function 'agent-repl--send-to-agent)
                 (lambda (text) (setq sent-text text))))
        (agent-repl)
        (should (equal sent-text "hello world"))
        (should-not hidden)))))

(ert-deftest agent-repl-test-panels-entry-point-simple-not-running-initializes ()
  "agent-repl-simple (SPC o c) keeps its non-always-close dispatch: when
nothing is running, it initializes the agent (in contrast to SPC o C)."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((started nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--agent-running-p) (lambda () nil))
                ((symbol-function 'agent-repl--session-starting-p) (lambda () nil))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'agent-repl--initialize-agent)
                 (lambda (&rest _) (setq started t))))
        (agent-repl-simple)
        (should started)))))

(ert-deftest agent-repl-test-panels-entry-point-simple-hidden-shows ()
  "agent-repl-simple (SPC o c) keeps its non-always-close dispatch: when
the session is running but panels are hidden, it re-shows them (in
contrast to SPC o C, which hides further)."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((shown nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--agent-running-p) (lambda () t))
                ((symbol-function 'agent-repl--session-starting-p) (lambda () nil))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'agent-repl--show-hidden-panels)
                 (lambda () (setq shown t))))
        (agent-repl-simple)
        (should shown)))))

(ert-deftest agent-repl-test-panels-entry-point-simple-output-only-adds-input ()
  "agent-repl-simple (SPC o c): when only the output window is visible, it
adds the input window beside it and focuses it — rather than rebuilding the
whole layout (which would duplicate the already-visible output window)."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((added nil) (focused nil) (shown-hidden nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--agent-running-p) (lambda () t))
                ((symbol-function 'agent-repl--session-starting-p) (lambda () nil))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'agent-repl--output-visible-input-hidden-p)
                 (lambda () t))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'agent-repl--show-input-beside-output)
                 (lambda () (setq added t)))
                ((symbol-function 'agent-repl--focus-input-panel)
                 (lambda () (setq focused t)))
                ((symbol-function 'agent-repl--show-hidden-panels)
                 (lambda () (setq shown-hidden t))))
        (agent-repl-simple)
        (should added)
        (should focused)
        (should-not shown-hidden)))))

;;;; ---- Tests: validate-env-switch ----
;;
;; Environment switching is vterm machinery, so every case below pins
;; the workspace to the vterm frontend: without the pin the workspace
;; rides `agent-repl-default-frontend' (the gui), and the gui guard
;; would be the thing that errors — passing these tests for a reason
;; that has nothing to do with the precondition each one is about.

(ert-deftest agent-repl-test-panels-validate-env-switch-no-worktree ()
  "validate-env-switch errors when not a worktree workspace."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :frontend 'vterm)
    (should-error
     (agent-repl--validate-env-switch "test-ws" :sandbox nil "session-123")
     :type 'user-error)))

(ert-deftest agent-repl-test-panels-validate-env-switch-no-session-id ()
  "validate-env-switch errors when no session ID is available."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :frontend 'vterm)
    (should-error
     (agent-repl--validate-env-switch "test-ws" :sandbox t nil)
     :type 'user-error)))

(ert-deftest agent-repl-test-panels-validate-env-switch-thinking ()
  "validate-env-switch errors when the agent is thinking."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :frontend 'vterm)
    (agent-repl--ws-put "test-ws" :thinking t)
    (should-error
     (agent-repl--validate-env-switch "test-ws" :sandbox t "session-123")
     :type 'user-error)))

(ert-deftest agent-repl-test-panels-validate-env-switch-no-sandbox-config ()
  "validate-env-switch errors when switching to sandbox with no config."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :frontend 'vterm)
    (cl-letf (((symbol-function 'agent-repl--resolve-sandbox-config) (lambda (_) nil))
              ((symbol-function 'agent-repl--git-root) (lambda (_) "/tmp")))
      (should-error
       (agent-repl--validate-env-switch "test-ws" :sandbox t "session-123")
       :type 'user-error))))

(ert-deftest agent-repl-test-panels-validate-env-switch-gui-frontend ()
  "validate-env-switch refuses a gui workspace: env switching is vterm-only."
  (agent-repl-test--with-clean-state
    ;; Arrange — a gui workspace that satisfies every OTHER precondition.
    (agent-repl--ws-put "test-ws" :frontend 'gui)
    ;; Act / Assert
    (should-error
     (agent-repl--validate-env-switch "test-ws" :bare-metal t "session-123")
     :type 'user-error)))

(ert-deftest agent-repl-test-panels-validate-env-switch-bare-metal-ok ()
  "validate-env-switch succeeds for bare-metal switch with valid args."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :frontend 'vterm)
    ;; Should not error
    (agent-repl--validate-env-switch "test-ws" :bare-metal t "session-123")))

;;;; ---- Tests: seed-new-env-session ----

(ert-deftest agent-repl-test-panels-seed-new-env-creates-inst ()
  "seed-new-env-session creates an instantiation and copies session-id."
  (agent-repl-test--with-clean-state
    (agent-repl--seed-new-env-session "test-ws" :sandbox "sess-abc")
    (let ((inst (agent-repl--ws-get "test-ws" :sandbox)))
      (should inst)
      (should (equal (agent-repl-instantiation-session-id inst) "sess-abc")))))

(ert-deftest agent-repl-test-panels-seed-new-env-does-not-overwrite ()
  "seed-new-env-session does not overwrite an existing session-id."
  (agent-repl-test--with-clean-state
    (let ((existing (make-agent-repl-instantiation :session-id "existing-id")))
      (agent-repl--ws-put "test-ws" :sandbox existing)
      (agent-repl--seed-new-env-session "test-ws" :sandbox "new-id")
      (let ((inst (agent-repl--ws-get "test-ws" :sandbox)))
        (should (equal (agent-repl-instantiation-session-id inst) "existing-id"))))))

;;;; ---- Tests: kill-vterm-process ----

(ert-deftest agent-repl-test-panels-kill-vterm-process-nil ()
  "kill-vterm-process does nothing for nil buffer."
  ;; Should not error
  (agent-repl--kill-vterm-process nil))

(ert-deftest agent-repl-test-panels-kill-vterm-process-dead-buffer ()
  "kill-vterm-process does nothing for a dead buffer."
  (let ((buf (get-buffer-create "*kill-proc-test*")))
    (kill-buffer buf)
    ;; Should not error
    (agent-repl--kill-vterm-process buf)))

;;;; ---- Tests: ws-buffer-visible-p with live but undisplayed buffer ----

(ert-deftest agent-repl-test-panels-ws-buffer-visible-p-live-not-displayed ()
  "ws-buffer-visible-p returns nil when the buffer is live but not in any window."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*undisplayed-test*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
              ;; Display a different buffer in the selected window
              (switch-to-buffer (get-buffer-create "*other-buf*"))
              (should-not (agent-repl--ws-buffer-visible-p :input-buffer))))
        (when (buffer-live-p buf) (kill-buffer buf))
        (when (get-buffer "*other-buf*") (kill-buffer "*other-buf*"))))))

;;;; ---- Tests: vterm-visible-p with visible and dead buffer ----

(ert-deftest agent-repl-test-panels-vterm-visible-p-with-visible-buffer ()
  "vterm-visible-p returns non-nil when the vterm buffer is displayed in a window."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*test-vterm*"
      (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'get-buffer-window)
                 (lambda (_buf &rest _) (selected-window))))
        ;; Mock get-buffer-window: batch mode has no real display
        (should (agent-repl--vterm-visible-p))))))

(ert-deftest agent-repl-test-panels-vterm-visible-p-dead-buffer ()
  "vterm-visible-p returns nil when the vterm buffer has been killed."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*test-dead-vterm*")))
      (agent-repl--ws-put "test-ws" :vterm-buffer buf)
      (kill-buffer buf)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
        (should-not (agent-repl--vterm-visible-p))))))

;;;; ---- Tests: panels-visible-p multi-window cases ----

(ert-deftest agent-repl-test-panels-panels-visible-p-only-input ()
  "panels-visible-p returns nil when only input panel is visible."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*test-input-only*"
      (agent-repl--ws-put "test-ws" :input-buffer (current-buffer))
      ;; No vterm buffer set
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
        (should-not (agent-repl--panels-visible-p))))))

(ert-deftest agent-repl-test-panels-panels-visible-p-only-vterm ()
  "panels-visible-p returns nil when only vterm panel is visible."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*test-vterm-only*"
      (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
      ;; No input buffer set
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
        (should-not (agent-repl--panels-visible-p))))))

(ert-deftest agent-repl-test-panels-panels-visible-p-both-visible ()
  "panels-visible-p returns t when both panels are displayed in windows."
  (agent-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*test-both-vterm*"))
          (input-buf (get-buffer-create "*test-both-input*"))
          (new-win nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
              ;; Show vterm in current window
              (switch-to-buffer vterm-buf)
              ;; Split and show input in new window
              (setq new-win (split-window))
              (set-window-buffer new-win input-buf)
              (should (agent-repl--panels-visible-p))))
        (when (and new-win (window-live-p new-win))
          (delete-window new-win))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

;;;; ---- Tests: safe-buffer-name dead buffer ----

(ert-deftest agent-repl-test-panels-safe-buffer-name-dead-buffer ()
  "safe-buffer-name returns nil for a killed buffer."
  (let ((buf (get-buffer-create "*dead-safe-name*")))
    (kill-buffer buf)
    ;; buffer-name on a dead buffer returns nil in Emacs
    (should-not (agent-repl--safe-buffer-name buf))))

;;;; ---- Tests: close-buffer-window edge cases ----

(ert-deftest agent-repl-test-panels-close-buffer-window-nil ()
  "close-buffer-window does nothing when passed nil."
  ;; when-let guards nil -- should not error
  (agent-repl--close-buffer-window nil))

(ert-deftest agent-repl-test-panels-close-buffer-window-successful-delete ()
  "close-buffer-window deletes the window displaying the buffer."
  (let ((buf (get-buffer-create "*close-win-test*"))
        (new-win nil))
    (unwind-protect
        (progn
          (setq new-win (split-window))
          (set-window-buffer new-win buf)
          (should (get-buffer-window buf))
          (agent-repl--close-buffer-window buf)
          (should-not (get-buffer-window buf)))
      (when (and new-win (window-live-p new-win))
        (ignore-errors (delete-window new-win)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-panels-close-buffer-window-last-window ()
  "close-buffer-window handles error when trying to delete the last window."
  ;; In batch mode, the selected window is the only window.
  ;; Display the buffer in the only window, then try to close it.
  ;; ignore-errors in the implementation should prevent error.
  (agent-repl-test--with-temp-buffer "*last-win-test*"
    (switch-to-buffer (current-buffer))
    ;; This should not error -- ignore-errors catches the "last window" error
    (agent-repl--close-buffer-window (current-buffer))))

;;;; ---- Tests: close-buffer-windows edge cases ----

(ert-deftest agent-repl-test-panels-close-buffer-windows-mix-live-and-nil ()
  "close-buffer-windows handles a mix of live and nil buffers."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*mix-live*"
      ;; Pass a mix of live buffer and nil -- should not error
      (agent-repl--close-buffer-windows (current-buffer) nil))))

(ert-deftest agent-repl-test-panels-close-buffer-windows-visible-window ()
  "close-buffer-windows closes a window displaying a buffer."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*close-wins-visible*"))
          (new-win nil))
      (unwind-protect
          (progn
            (setq new-win (split-window))
            (set-window-buffer new-win buf)
            (should (get-buffer-window buf))
            (agent-repl--close-buffer-windows buf)
            (should-not (get-buffer-window buf)))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;;;; ---- Tests: show-panels edge cases ----

(ert-deftest agent-repl-test-panels-show-panels-normal-operation ()
  "show-panels splits windows and displays vterm and input buffers."
  (agent-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*show-panels-vterm*"))
          (input-buf (get-buffer-create "*show-panels-input*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--refresh-vterm) (lambda () nil))
                    ((symbol-function 'agent-repl--update-all-workspace-states-now) (lambda () nil)))
            (agent-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (delete-other-windows)
            (agent-repl--show-panels)
            ;; Both buffers should now be visible in windows
            (should (get-buffer-window vterm-buf))
            (should (get-buffer-window input-buf)))
        ;; Clean up
        (delete-other-windows)
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

(ert-deftest agent-repl-test-panels-show-panels-fills-frame-clearing-work-windows ()
  "show-panels clears existing work windows so the panels fill the frame.
Fullscreen is the sole display format: after show-panels the only
non-side windows are the two agent panels — no work window remains."
  (agent-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*show-up-vterm*"))
          (input-buf (get-buffer-create "*show-up-input*"))
          (work-buf (get-buffer-create "*show-up-work*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--refresh-vterm) (lambda () nil))
                    ((symbol-function 'agent-repl--update-all-workspace-states-now) (lambda () nil)))
            (agent-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (delete-other-windows)
            ;; Create an extra work window alongside the starting one.
            (let ((extra (split-window (selected-window) nil 'below)))
              (set-window-buffer extra work-buf))
            (agent-repl--show-panels)
            ;; Both panels visible …
            (should (get-buffer-window vterm-buf))
            (should (get-buffer-window input-buf))
            ;; … and the work window is gone (panels fill the frame).
            (should-not (get-buffer-window work-buf))
            (should (= 2 (length (cl-remove-if #'agent-repl-window--side-window-p
                                               (window-list))))))
        ;; Clean up
        (delete-other-windows)
        (dolist (b (list vterm-buf input-buf work-buf))
          (when (buffer-live-p b) (kill-buffer b)))))))

(ert-deftest agent-repl-test-panels-show-panels-saves-pre-panel-config ()
  "show-panels saves the pre-panel window layout as :fullscreen-config.
The close path restores it so the work windows the panels covered come
back rather than the close stranding a panel onscreen."
  (agent-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*show-noop-vterm*"))
          (input-buf (get-buffer-create "*show-noop-input*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--refresh-vterm) (lambda () nil))
                    ((symbol-function 'agent-repl--update-all-workspace-states-now) (lambda () nil)))
            (agent-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (delete-other-windows)
            (should-not (agent-repl--ws-get "test-ws" :fullscreen-config))
            (agent-repl--show-panels)
            (should (agent-repl--ws-get "test-ws" :fullscreen-config))
            (should (get-buffer-window vterm-buf))
            (should (get-buffer-window input-buf)))
        ;; Clean up
        (delete-other-windows)
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

(ert-deftest agent-repl-test-panels-show-panels-does-not-overwrite-saved-config ()
  "show-panels does NOT overwrite an already-saved :fullscreen-config.
Re-show paths (workspace-switch reclaim, half-shown repair) call through
show-panels too and must not clobber the saved work layout."
  (agent-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*show-keep-vterm*"))
          (input-buf (get-buffer-create "*show-keep-input*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--refresh-vterm) (lambda () nil))
                    ((symbol-function 'agent-repl--update-all-workspace-states-now) (lambda () nil)))
            (agent-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (agent-repl--ws-put "test-ws" :fullscreen-config 'preexisting)
            (delete-other-windows)
            (agent-repl--show-panels)
            (should (eq 'preexisting
                        (agent-repl--ws-get "test-ws" :fullscreen-config))))
        ;; Clean up
        (delete-other-windows)
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

(ert-deftest agent-repl-test-panels-show-panels-sets-no-delete-other-windows ()
  "show-panels sets `no-delete-other-windows' on both vterm and input windows
so that commands like magit-status cannot destroy panel layout."
  (agent-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*show-ndow-vterm*"))
          (input-buf (get-buffer-create "*show-ndow-input*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--refresh-vterm) (lambda () nil))
                    ((symbol-function 'agent-repl--update-all-workspace-states-now) (lambda () nil)))
            (agent-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (delete-other-windows)
            (agent-repl--show-panels)
            (let ((vterm-win (get-buffer-window vterm-buf))
                  (input-win (get-buffer-window input-buf)))
              (should (window-parameter vterm-win 'no-delete-other-windows))
              (should (window-parameter input-win 'no-delete-other-windows))))
        ;; Clean up
        (delete-other-windows)
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

(ert-deftest agent-repl-test-panels-show-panels-locks-input-height ()
  "show-panels sets `window-size-fixed' to height on the input window
so that window management operations cannot shrink it."
  (agent-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*show-hfix-vterm*"))
          (input-buf (get-buffer-create "*show-hfix-input*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--refresh-vterm) (lambda () nil))
                    ((symbol-function 'agent-repl--update-all-workspace-states-now) (lambda () nil)))
            (agent-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (delete-other-windows)
            (agent-repl--show-panels)
            (let ((input-win (get-buffer-window input-buf)))
              (should (eq (window-parameter input-win 'window-size-fixed) 'height))))
        ;; Clean up
        (delete-other-windows)
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

(ert-deftest agent-repl-test-panels-show-panels-preserves-input-height ()
  "show-panels calls `window-preserve-size' on the input window so a
multi-line minibuffer cannot shrink it.  `window-size-fixed' alone is
bypassed by `window--resize-mini-window' (ignore=t), so the stronger
`window-preserved-size' parameter is required."
  (agent-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*show-preserve-vterm*"))
          (input-buf (get-buffer-create "*show-preserve-input*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--refresh-vterm) (lambda () nil))
                    ((symbol-function 'agent-repl--update-all-workspace-states-now) (lambda () nil)))
            (agent-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (delete-other-windows)
            (agent-repl--show-panels)
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

(ert-deftest agent-repl-test-panels-show-panels-no-output-dup-on-dead-input ()
  "show-panels must NOT duplicate the output window when the input buffer is
dead and the output window is already selected/visible.

Regression for the panel-corruption bug: the input window is split off
the output window and inherits the vterm buffer until reassigned, so a
dead input buffer used to strand the vterm in that adjacent window (the
duplicated-output corruption seen switching to a freshly generated
workspace with the drawer open)."
  (agent-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*agent-panel-test-ws*"))
          (input-buf (get-buffer-create "*agent-panel-input-test-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--refresh-vterm) (lambda () nil))
                    ((symbol-function 'agent-repl--update-all-workspace-states-now) (lambda () nil)))
            (agent-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (delete-other-windows)
            ;; Output already visible AND selected, so the split inherits it.
            (set-window-buffer (selected-window) vterm-buf)
            ;; Input buffer dies before the show.
            (kill-buffer input-buf)
            (agent-repl--show-panels)
            ;; Exactly one window shows the vterm buffer — not duplicated.
            (should (= 1 (length (get-buffer-window-list vterm-buf nil nil)))))
        (delete-other-windows)
        (dolist (b (list vterm-buf input-buf
                         (get-buffer "*agent-panel-input-test-ws*")))
          (when (buffer-live-p b) (kill-buffer b)))))))

;;;; ---- Tests: ensure-input-buffer ----

(ert-deftest agent-repl-test-panels-ensure-input-buffer-returns-live ()
  "ensure-input-buffer returns the recorded :input-buffer when it is live."
  (agent-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*agent-panel-input-test-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (should (eq (agent-repl--ensure-input-buffer "test-ws") input-buf)))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

(ert-deftest agent-repl-test-panels-ensure-input-buffer-adopts-named ()
  "ensure-input-buffer adopts the live canonically-named buffer when the
recorded :input-buffer is dead, and records it back on the plist."
  (agent-repl-test--with-clean-state
    (let ((dead (get-buffer-create "*stale-input*"))
          (named (get-buffer-create "*agent-panel-input-test-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
            (agent-repl--ws-put "test-ws" :input-buffer dead)
            (kill-buffer dead)
            (let ((result (agent-repl--ensure-input-buffer "test-ws")))
              (should (eq result named))
              (should (eq (agent-repl--ws-get "test-ws" :input-buffer) named))))
        (when (buffer-live-p named) (kill-buffer named))))))

(ert-deftest agent-repl-test-panels-ensure-input-buffer-recreates-when-absent ()
  "ensure-input-buffer creates a fresh, live input buffer when neither the
recorded :input-buffer nor the canonically-named buffer is live."
  (agent-repl-test--with-clean-state
    (unwind-protect
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
          (agent-repl--ws-put "test-ws" :input-buffer nil)
          (let ((result (agent-repl--ensure-input-buffer "test-ws")))
            (should (buffer-live-p result))
            (should (eq result (agent-repl--ws-get "test-ws" :input-buffer)))
            (should (string= (buffer-name result) "*agent-panel-input-test-ws*"))))
      (when-let ((b (get-buffer "*agent-panel-input-test-ws*")))
        (kill-buffer b)))))

;;;; ---- Tests: focus-input-panel edge cases ----

(ert-deftest agent-repl-test-panels-focus-input-panel-nil-buffer ()
  "focus-input-panel signals an error when input buffer is nil."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
      (should-error (agent-repl--focus-input-panel) :type 'error))))

(ert-deftest agent-repl-test-panels-focus-input-panel-no-window ()
  "focus-input-panel signals an error when input buffer exists but has no window."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*focus-no-win*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer buf)
            (switch-to-buffer (get-buffer-create "*other*"))
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
              (should-error (agent-repl--focus-input-panel) :type 'error)))
        (when (buffer-live-p buf) (kill-buffer buf))
        (when (get-buffer "*other*") (kill-buffer "*other*"))))))

(ert-deftest agent-repl-test-panels-focus-input-panel-with-window ()
  "focus-input-panel selects the window displaying the input buffer."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*focus-input-win*"))
          (new-win nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer buf)
            (setq new-win (split-window))
            (set-window-buffer new-win buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
              (agent-repl--focus-input-panel)
              (should (eq (window-buffer (selected-window)) buf))))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest agent-repl-test-panels-focus-input-panel-no-insert-state ()
  "focus-input-panel does NOT enter evil insert state on focus."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*focus-input-no-insert*"))
          (new-win nil)
          (insert-called nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer buf)
            (setq new-win (split-window))
            (set-window-buffer new-win buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                      ((symbol-function 'evil-insert-state)
                       (lambda (&rest _) (setq insert-called t))))
              (agent-repl--focus-input-panel)
              (should-not insert-called)))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;;;; ---- Tests: focus-input show-or-focus branch ----

(ert-deftest agent-repl-test-panels-focus-input-selects-window ()
  "focus-input selects the input window in the running/visible branch."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*focus-input-cmd-win*"))
          (new-win nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer buf)
            (setq new-win (split-window))
            (set-window-buffer new-win buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                      ((symbol-function 'agent-repl--agent-running-p) (lambda () t))
                      ((symbol-function 'agent-repl--panels-visible-p) (lambda () t)))
              (agent-repl-focus-input)
              (should (eq (window-buffer (selected-window)) buf))))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest agent-repl-test-panels-focus-input-no-insert-state ()
  "focus-input does NOT enter evil insert state when focusing the input window."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*focus-input-cmd-no-insert*"))
          (new-win nil)
          (insert-called nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer buf)
            (setq new-win (split-window))
            (set-window-buffer new-win buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                      ((symbol-function 'agent-repl--agent-running-p) (lambda () t))
                      ((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                      ((symbol-function 'evil-insert-state)
                       (lambda (&rest _) (setq insert-called t))))
              (agent-repl-focus-input)
              (should-not insert-called)))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;;;; ---- Tests: show-panels-and-focus ----

(ert-deftest agent-repl-test-panels-show-panels-and-focus-delegates ()
  "show-panels-and-focus calls show-panels and focus-input-panel."
  (agent-repl-test--with-clean-state
    (let ((show-called nil)
          (focus-called nil))
      (cl-letf (((symbol-function 'agent-repl--show-panels)
                 (lambda () (setq show-called t)))
                ((symbol-function 'agent-repl--focus-input-panel)
                 (lambda () (setq focus-called t))))
        (agent-repl--show-panels-and-focus)
        (should show-called)
        (should focus-called)))))

;;;; ---- Tests: vterm-redraw with nil vterm--term ----

(ert-deftest agent-repl-test-panels-vterm-redraw-nil-term ()
  "vterm-redraw is a no-op when vterm--term is nil."
  (let ((vterm--term nil)
        (redraw-called nil))
    (cl-letf (((symbol-function 'vterm--redraw)
               (lambda (&rest _) (setq redraw-called t))))
      (agent-repl--vterm-redraw)
      ;; vterm--term is nil, so vterm--redraw should not be called
      (should-not redraw-called))))

;;;; ---- Tests: fix-vterm-scroll edge cases ----

(ert-deftest agent-repl-test-panels-fix-vterm-scroll-no-window ()
  "fix-vterm-scroll is a no-op when the buffer has no window."
  (agent-repl-test--with-temp-buffer "*no-vterm-win*"
    ;; Display a different buffer so our buffer has no window
    (let ((buf (current-buffer)))
      (switch-to-buffer (get-buffer-create "*other-scroll*"))
      (unwind-protect
          ;; Should not error -- the when guard skips the body
          (agent-repl--fix-vterm-scroll buf)
        (when (get-buffer "*other-scroll*") (kill-buffer "*other-scroll*"))))))

(ert-deftest agent-repl-test-panels-fix-vterm-scroll-same-window ()
  "fix-vterm-scroll is a no-op when vterm window is the selected window."
  (agent-repl-test--with-temp-buffer "*same-vterm-win*"
    ;; The buffer is displayed in the selected window
    ;; vterm-win eq orig-win, so the when body is skipped
    (agent-repl--fix-vterm-scroll (current-buffer))))

;;;; ---- Tests: resolve-vterm-buffer current buffer is vterm-mode ----

(ert-deftest agent-repl-test-panels-resolve-vterm-buffer-is-vterm-mode ()
  "resolve-vterm-buffer returns the current buffer when it is in vterm-mode."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*test-vterm-mode*"
      ;; Simulate vterm-mode by setting major-mode directly
      (let ((major-mode 'vterm-mode))
        (should (eq (agent-repl--resolve-vterm-buffer) (current-buffer)))))))

;;;; ---- Tests: refresh-vterm edge cases ----

(ert-deftest agent-repl-test-panels-refresh-vterm-resolve-nil ()
  "refresh-vterm is a no-op when resolve-vterm-buffer returns nil."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--resolve-vterm-buffer) (lambda () nil)))
      ;; Should not error
      (should-not (agent-repl--refresh-vterm)))))

(ert-deftest agent-repl-test-panels-refresh-vterm-dead-buffer ()
  "refresh-vterm is a no-op when the resolved buffer is dead."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*dead-refresh*")))
      (kill-buffer buf)
      (cl-letf (((symbol-function 'agent-repl--resolve-vterm-buffer) (lambda () buf)))
        ;; buffer-live-p check prevents action
        (should-not (agent-repl--refresh-vterm))))))

(ert-deftest agent-repl-test-panels-refresh-vterm-not-vterm-mode ()
  "refresh-vterm is a no-op when the resolved buffer is not in vterm-mode."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*not-vterm-mode*"
      (let ((buf (current-buffer)))
        (cl-letf (((symbol-function 'agent-repl--resolve-vterm-buffer) (lambda () buf))
                  ((symbol-function 'agent-repl--do-refresh)
                   (lambda () (error "should not be called"))))
          ;; Buffer is live but not in vterm-mode, so do-refresh is skipped
          (agent-repl--refresh-vterm))))))

;;;; ---- Tests: on-workspace-switch ws nil ----

(ert-deftest agent-repl-test-panels-on-workspace-switch-nil-ws ()
  "on-workspace-switch does not error when workspace is nil."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
              ((symbol-function 'agent-repl--update-all-workspace-states-now) (lambda () nil))
              ((symbol-function 'agent-repl--refresh-vterm) (lambda () nil))
              ((symbol-function 'agent-repl--reset-vterm-cursors) (lambda () nil))
              ((symbol-function 'agent-repl--drain-pending-magit) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--drain-pending-initial-buffers) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--drain-pending-show-panels) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--maybe-autoselect-input) (lambda (_ws) nil)))
      ;; Should not error -- the when guard skips mark-viewed
      (agent-repl--on-workspace-switch))))

(ert-deftest agent-repl-test-panels-on-workspace-switch-flips-ws-loaded ()
  "Tail of `--on-workspace-switch' flips the `:ws-loaded' latch bit
on the ws plist (via `--latch-and-maybe-fire-loaded')."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
              ((symbol-function 'agent-repl--maybe-sweep-hidden-on-switch) #'ignore)
              ((symbol-function 'agent-repl--update-all-workspace-states-now) #'ignore)
              ((symbol-function 'agent-repl--refresh-vterm) #'ignore)
              ((symbol-function 'agent-repl--reset-vterm-cursors) #'ignore)
              ((symbol-function 'agent-repl--drain-pending-magit) #'ignore)
              ((symbol-function 'agent-repl--drain-pending-initial-buffers) #'ignore)
              ((symbol-function 'agent-repl--drain-pending-show-panels) #'ignore)
              ((symbol-function 'agent-repl--maybe-autoselect-input) #'ignore))
      (agent-repl--on-workspace-switch "ws1")
      ;; :agent-ready is nil so latch hasn't fired+cleared; bit stays set.
      (should (eq (agent-repl--ws-get "ws1" :ws-loaded) t)))))

(ert-deftest agent-repl-test-panels-on-workspace-switch-nil-ws-skips-latch ()
  "When `--on-workspace-switch' is called with nil ws (and current-name
also returns nil), the latch flip is skipped — guards against poisoning
the ws-plist hash with a nil key in test/init environments."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
              ((symbol-function 'agent-repl--maybe-sweep-hidden-on-switch) #'ignore)
              ((symbol-function 'agent-repl--update-all-workspace-states-now) #'ignore)
              ((symbol-function 'agent-repl--refresh-vterm) #'ignore)
              ((symbol-function 'agent-repl--reset-vterm-cursors) #'ignore)
              ((symbol-function 'agent-repl--drain-pending-magit) #'ignore)
              ((symbol-function 'agent-repl--drain-pending-initial-buffers) #'ignore)
              ((symbol-function 'agent-repl--drain-pending-show-panels) #'ignore)
              ((symbol-function 'agent-repl--maybe-autoselect-input) #'ignore))
      ;; Should not error and should not touch the hash table.
      (agent-repl--on-workspace-switch nil)
      (should-not (gethash nil agent-repl--workspaces)))))

(ert-deftest agent-repl-test-panels-on-workspace-switch-dequeues-merge ()
  "Switching to a workspace calls `--dequeue-merge' on it so a parked
merge request is pulled from the queue on activation."
  (agent-repl-test--with-clean-state
    (let (dequeued)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--maybe-sweep-hidden-on-switch) #'ignore)
                ((symbol-function 'agent-repl--dequeue-merge)
                 (lambda (ws) (setq dequeued ws)))
                ((symbol-function 'agent-repl--update-all-workspace-states-now) #'ignore)
                ((symbol-function 'agent-repl--refresh-vterm) #'ignore)
                ((symbol-function 'agent-repl--reset-vterm-cursors) #'ignore)
                ((symbol-function 'agent-repl--drain-pending-magit) #'ignore)
                ((symbol-function 'agent-repl--drain-pending-initial-buffers) #'ignore)
                ((symbol-function 'agent-repl--drain-pending-show-panels) #'ignore)
                ((symbol-function 'agent-repl--maybe-autoselect-input) #'ignore))
        (agent-repl--on-workspace-switch "ws1")
        (should (equal dequeued "ws1"))))))

(ert-deftest agent-repl-test-panels-on-workspace-switch-snaps-webview-to-tail ()
  "Switching to a workspace snaps its gui webview feed to the newest message,
the gui counterpart of the vterm window's snap to the cursor."
  (agent-repl-test--with-clean-state
    (let (snapped)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--maybe-sweep-hidden-on-switch) #'ignore)
                ((symbol-function 'agent-repl--dequeue-merge) #'ignore)
                ((symbol-function 'agent-repl--update-all-workspace-states-now) #'ignore)
                ((symbol-function 'agent-repl--refresh-vterm) #'ignore)
                ((symbol-function 'agent-repl--reset-vterm-cursors) #'ignore)
                ((symbol-function 'agent-repl--drain-pending-magit) #'ignore)
                ((symbol-function 'agent-repl--drain-pending-initial-buffers) #'ignore)
                ((symbol-function 'agent-repl--drain-pending-show-panels) #'ignore)
                ((symbol-function 'agent-repl--maybe-autoselect-input) #'ignore)
                ((symbol-function 'agent-repl--frontend-snap-webview-to-tail)
                 (lambda (ws) (setq snapped ws))))
        (agent-repl--on-workspace-switch "ws1")
        (should (equal snapped "ws1"))))))

(ert-deftest agent-repl-test-panels-on-workspace-switch-snaps-after-show-drain ()
  "The webview snap runs AFTER the pending-show drain, so a webview that
just became visible on the switch is snapped to its tail too."
  (agent-repl-test--with-clean-state
    (let (order)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--maybe-sweep-hidden-on-switch) #'ignore)
                ((symbol-function 'agent-repl--dequeue-merge) #'ignore)
                ((symbol-function 'agent-repl--update-all-workspace-states-now) #'ignore)
                ((symbol-function 'agent-repl--refresh-vterm) #'ignore)
                ((symbol-function 'agent-repl--reset-vterm-cursors) #'ignore)
                ((symbol-function 'agent-repl--drain-pending-magit) #'ignore)
                ((symbol-function 'agent-repl--drain-pending-initial-buffers) #'ignore)
                ((symbol-function 'agent-repl--drain-pending-show-panels)
                 (lambda (_ws) (push 'show order)))
                ((symbol-function 'agent-repl--maybe-autoselect-input) #'ignore)
                ((symbol-function 'agent-repl--frontend-snap-webview-to-tail)
                 (lambda (_ws) (push 'snap order))))
        (agent-repl--on-workspace-switch "ws1")
        (should (equal (nreverse order) '(show snap)))))))

(ert-deftest agent-repl-test-panels-on-workspace-switch-done-stamps-acked-at ()
  "Switching to a workspace in :done sets :done-acked t and stamps
:done-acked-at with the current time so the focus-dwell countdown
can start."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'agent-repl--maybe-sweep-hidden-on-switch) #'ignore)
              ((symbol-function 'agent-repl--update-all-workspace-states-now) #'ignore)
              ((symbol-function 'agent-repl--refresh-vterm) #'ignore)
              ((symbol-function 'agent-repl--reset-vterm-cursors) #'ignore)
              ((symbol-function 'agent-repl--drain-pending-magit) #'ignore)
              ((symbol-function 'agent-repl--drain-pending-initial-buffers) #'ignore)
              ((symbol-function 'agent-repl--drain-pending-show-panels) #'ignore)
              ((symbol-function 'agent-repl--maybe-autoselect-input) #'ignore))
      (agent-repl--ws-set-agent-state "ws1" :done)
      (let ((before (float-time)))
        (agent-repl--on-workspace-switch "ws1")
        (should (eq (agent-repl--ws-get "ws1" :done-acked) t))
        (let ((stamp (agent-repl--ws-get "ws1" :done-acked-at)))
          (should (numberp stamp))
          (should (>= stamp before)))))))

(ert-deftest agent-repl-test-panels-on-workspace-switch-non-done-does-not-stamp ()
  "Switching to a workspace not in :done does not touch :done-acked-at."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'agent-repl--maybe-sweep-hidden-on-switch) #'ignore)
              ((symbol-function 'agent-repl--update-all-workspace-states-now) #'ignore)
              ((symbol-function 'agent-repl--refresh-vterm) #'ignore)
              ((symbol-function 'agent-repl--reset-vterm-cursors) #'ignore)
              ((symbol-function 'agent-repl--drain-pending-magit) #'ignore)
              ((symbol-function 'agent-repl--drain-pending-initial-buffers) #'ignore)
              ((symbol-function 'agent-repl--drain-pending-show-panels) #'ignore)
              ((symbol-function 'agent-repl--maybe-autoselect-input) #'ignore))
      (agent-repl--ws-set-agent-state "ws1" :thinking)
      (agent-repl--on-workspace-switch "ws1")
      (should-not (agent-repl--ws-get "ws1" :done-acked))
      (should-not (agent-repl--ws-get "ws1" :done-acked-at)))))

(ert-deftest agent-repl-test-panels-clear-done-ack-on-switch-away-done ()
  "Leaving a workspace in :done clears :done-acked and :done-acked-at so
the dwell countdown restarts on return."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :done)
    (agent-repl--ws-put "ws1" :done-acked t)
    (agent-repl--ws-put "ws1" :done-acked-at (float-time))
    (agent-repl--clear-done-ack-on-switch-away "ws1")
    (should-not (agent-repl--ws-get "ws1" :done-acked))
    (should-not (agent-repl--ws-get "ws1" :done-acked-at))))

(ert-deftest agent-repl-test-panels-clear-done-ack-on-switch-away-non-done ()
  "Leaving a workspace NOT in :done leaves ack flags untouched — the
clear only resets the dwell countdown for live :done workspaces."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :thinking)
    (agent-repl--ws-put "ws1" :done-acked t)
    (let ((stamp (float-time)))
      (agent-repl--ws-put "ws1" :done-acked-at stamp)
      (agent-repl--clear-done-ack-on-switch-away "ws1")
      ;; :thinking ws was not affected.
      (should (eq (agent-repl--ws-get "ws1" :done-acked) t))
      (should (= (agent-repl--ws-get "ws1" :done-acked-at) stamp)))))

(ert-deftest agent-repl-test-panels-clear-done-ack-on-switch-away-nil-ws ()
  "Switch-away clear with nil ws is a no-op (covers test/init envs where
+workspace-current-name returns nil)."
  (agent-repl-test--with-clean-state
    ;; Should not error.
    (agent-repl--clear-done-ack-on-switch-away nil)))

(ert-deftest agent-repl-test-panels-on-workspace-switch-explicit-ws-overrides-current ()
  "An explicit WS argument propagates to every per-ws side effect,
overriding `(+workspace-current-name)' at call time.  This is how
`--after-persp-activated' delivers the just-switched-to ws name to
the deferred call so back-to-back switches don't collapse onto the
latest one."
  (agent-repl-test--with-clean-state
    (let ((received-ws nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "racing-current"))
                ((symbol-function 'agent-repl--maybe-sweep-hidden-on-switch)
                 (lambda (ws) (push (cons :sweep ws) received-ws)))
                ((symbol-function 'agent-repl--update-all-workspace-states-now) (lambda () nil))
                ((symbol-function 'agent-repl--refresh-vterm) (lambda () nil))
                ((symbol-function 'agent-repl--reset-vterm-cursors) (lambda () nil))
                ((symbol-function 'agent-repl--drain-pending-magit)
                 (lambda (ws) (push (cons :magit ws) received-ws)))
                ((symbol-function 'agent-repl--drain-pending-initial-buffers)
                 (lambda (ws) (push (cons :init-bufs ws) received-ws)))
                ((symbol-function 'agent-repl--drain-pending-show-panels)
                 (lambda (ws) (push (cons :show-panels ws) received-ws)))
                ((symbol-function 'agent-repl--maybe-autoselect-input)
                 (lambda (ws) (push (cons :autoselect ws) received-ws))))
        (agent-repl--on-workspace-switch "captured-ws")
        ;; Every per-ws helper got "captured-ws", not "racing-current".
        (should (cl-every (lambda (e) (equal (cdr e) "captured-ws"))
                          received-ws))))))

;;;; ---- Tests: maybe-autoselect-input ----

(ert-deftest agent-repl-test-panels-maybe-autoselect-input-selects-visible-input ()
  "maybe-autoselect-input selects the input window when it is visible."
  (agent-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*autoselect-input*"))
          (new-win nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (setq new-win (split-window))
            (set-window-buffer new-win input-buf)
            ;; Ensure we start on the other window
            (select-window (car (window-list)))
            (should-not (eq (window-buffer (selected-window)) input-buf))
            (let ((agent-repl-autoselect-input-on-workspace-switch t))
              (agent-repl--maybe-autoselect-input "test-ws")
              (should (eq (window-buffer (selected-window)) input-buf))))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

(ert-deftest agent-repl-test-panels-maybe-autoselect-input-noop-when-disabled ()
  "maybe-autoselect-input does nothing when the defcustom is nil."
  (agent-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*autoselect-disabled*"))
          (new-win nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (setq new-win (split-window))
            (set-window-buffer new-win input-buf)
            (let ((orig-win (selected-window))
                  (agent-repl-autoselect-input-on-workspace-switch nil))
              (agent-repl--maybe-autoselect-input "test-ws")
              ;; Window should not have changed
              (should (eq (selected-window) orig-win))))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

(ert-deftest agent-repl-test-panels-maybe-autoselect-input-noop-no-buffer ()
  "maybe-autoselect-input does nothing when no input buffer exists."
  (agent-repl-test--with-clean-state
    (let ((orig-win (selected-window))
          (agent-repl-autoselect-input-on-workspace-switch t))
      (agent-repl--maybe-autoselect-input "test-ws")
      (should (eq (selected-window) orig-win)))))

(ert-deftest agent-repl-test-panels-maybe-autoselect-input-noop-not-visible ()
  "maybe-autoselect-input does nothing when input buffer is not in any window."
  (agent-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*autoselect-hidden*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            ;; Display a different buffer so input-buf has no window
            (switch-to-buffer (get-buffer-create "*other-auto*"))
            (let ((orig-win (selected-window))
                  (agent-repl-autoselect-input-on-workspace-switch t))
              (agent-repl--maybe-autoselect-input "test-ws")
              (should (eq (selected-window) orig-win))))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))
        (when (get-buffer "*other-auto*") (kill-buffer "*other-auto*"))))))

(ert-deftest agent-repl-test-panels-maybe-autoselect-input-noop-dead-buffer ()
  "maybe-autoselect-input does nothing when input buffer has been killed."
  (agent-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*autoselect-dead*")))
      (agent-repl--ws-put "test-ws" :input-buffer input-buf)
      (kill-buffer input-buf)
      (let ((orig-win (selected-window))
            (agent-repl-autoselect-input-on-workspace-switch t))
        (agent-repl--maybe-autoselect-input "test-ws")
        (should (eq (selected-window) orig-win))))))

(ert-deftest agent-repl-test-panels-maybe-autoselect-input-snaps-vterm-then-selects-input ()
  "maybe-autoselect-input snaps the vterm window to its cursor (via
`--snap-vterm-window-to-cursor') and then selects only the input
window.  Replaces the old brief-select hack — the previous transient
`select-window vterm-win' was the source of the visible scroll-down
animation, so the new flow snaps `window-start' directly and selects
only the input window."
  (agent-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*autoselect-input-snap*"))
          (vterm-buf (get-buffer-create "*autoselect-vterm-snap*"))
          (vterm-win nil)
          (input-win nil)
          (selections nil)
          (snap-arg nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (agent-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
            (setq input-win (split-window))
            (set-window-buffer input-win input-buf)
            (setq vterm-win (split-window))
            (set-window-buffer vterm-win vterm-buf)
            (with-current-buffer vterm-buf (setq major-mode 'vterm-mode))
            (select-window (car (window-list)))
            (let ((agent-repl-autoselect-input-on-workspace-switch t)
                  (orig-select-window (symbol-function 'select-window)))
              (cl-letf (((symbol-function 'select-window)
                         (lambda (win &optional norecord)
                           (push win selections)
                           (funcall orig-select-window win norecord)))
                        ((symbol-function 'agent-repl--snap-vterm-window-to-cursor)
                         (lambda (win) (setq snap-arg win))))
                (agent-repl--maybe-autoselect-input "test-ws"))
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

(ert-deftest agent-repl-test-panels-maybe-autoselect-input-no-vterm-hack-when-hidden ()
  "maybe-autoselect-input skips the vterm hack when vterm is not displayed."
  (agent-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*autoselect-input-no-vterm*"))
          (vterm-buf (get-buffer-create "*autoselect-vterm-hidden*"))
          (input-win nil)
          (selections nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (agent-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
            (setq input-win (split-window))
            (set-window-buffer input-win input-buf)
            ;; vterm-buf intentionally not displayed in any window
            (select-window (car (window-list)))
            (let ((agent-repl-autoselect-input-on-workspace-switch t)
                  (orig-select-window (symbol-function 'select-window)))
              (cl-letf (((symbol-function 'select-window)
                         (lambda (win &optional norecord)
                           (push win selections)
                           (funcall orig-select-window win norecord))))
                (agent-repl--maybe-autoselect-input "test-ws"))
              (setq selections (nreverse selections))
              ;; Only the input selection should happen.
              (should (equal selections (list input-win)))))
        (when (and input-win (window-live-p input-win))
          (ignore-errors (delete-window input-win)))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

;;;; ---- Tests: non-agent-panel-window-p with agent buffers ----

(ert-deftest agent-repl-test-panels-non-agent-panel-window-p-vterm-buffer ()
  "non-agent-panel-window-p returns nil for a window showing an agent vterm buffer."
  (let ((buf (get-buffer-create "*agent-panel-abcd1234*")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (should-not (agent-repl--non-agent-panel-window-p (selected-window))))
      (switch-to-buffer "*scratch*")
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-panels-non-agent-panel-window-p-input-buffer ()
  "non-agent-panel-window-p returns nil for a window showing an agent input buffer."
  (let ((buf (get-buffer-create "*agent-panel-input-abcd1234*")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (should-not (agent-repl--non-agent-panel-window-p (selected-window))))
      (switch-to-buffer "*scratch*")
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;; ---- Tests: redirect-from-agent-before-save ----

(ert-deftest agent-repl-test-panels-redirect-non-agent-noop ()
  "redirect-from-agent-before-save is a no-op when selected window is non-agent."
  (agent-repl-test--with-clean-state
    ;; Selected window shows a regular buffer -- the outer when clause fails
    (let ((orig-win (selected-window)))
      (agent-repl--redirect-from-agent-before-save)
      ;; Selected window should not change
      (should (eq (selected-window) orig-win)))))

;;;; ---- Tests: hide-panels edge cases ----

(ert-deftest agent-repl-test-panels-hide-panels-both-exist ()
  "hide-panels calls close-buffer-windows with both buffers."
  (agent-repl-test--with-clean-state
    (let ((closed-bufs nil))
      (agent-repl-test--with-temp-buffer "*hide-vterm*"
        (let ((vterm-buf (current-buffer)))
          (agent-repl-test--with-temp-buffer "*hide-input*"
            (let ((input-buf (current-buffer)))
              (agent-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
              (agent-repl--ws-put "test-ws" :input-buffer input-buf)
              (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                        ((symbol-function 'agent-repl--close-buffer-windows)
                         (lambda (&rest bufs) (setq closed-bufs bufs))))
                (agent-repl--hide-panels)
                (should (equal closed-bufs (list input-buf vterm-buf)))))))))))

(ert-deftest agent-repl-test-panels-hide-panels-neither-exists ()
  "hide-panels does not error when neither buffer exists."
  (agent-repl-test--with-clean-state
    (let ((closed-bufs nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--close-buffer-windows)
                 (lambda (&rest bufs) (setq closed-bufs bufs))))
        (agent-repl--hide-panels)
        ;; Both should be nil
        (should (equal closed-bufs (list nil nil)))))))

;;;; ---- Tests: sync-panels no orphans ----

(ert-deftest agent-repl-test-panels-sync-panels-no-orphans ()
  "sync-panels does not delete any windows when there are no orphans."
  (agent-repl-test--with-clean-state
    (let ((deleted nil))
      (cl-letf (((symbol-function 'agent-repl--orphaned-panel-p) (lambda (_) nil))
                ((symbol-function 'delete-window) (lambda (w) (push w deleted))))
        (agent-repl--sync-panels)
        (should-not deleted)))))

;;;; ---- Tests: on-window-change sync-panels error ----

(ert-deftest agent-repl-test-panels-on-window-change-sync-error ()
  "on-window-change propagates errors from sync-panels (no error swallowing)."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--sync-panels)
               (lambda () (error "sync failed")))
              ((symbol-function 'agent-repl--update-hide-overlay) #'ignore))
      (should-error (agent-repl--on-window-change) :type 'error))))

;;;; ---- Tests: cursor reset is workspace-switch-only ----

(ert-deftest agent-repl-test-panels-on-window-change-does-not-reset-cursors ()
  "`--on-window-change' must NOT call `--reset-vterm-cursors'.
Resetting on every window-config change snaps vterm back to the bottom
and undoes user scrolls (e.g. via `C-S-k')."
  (agent-repl-test--with-clean-state
    (let ((reset-called nil))
      (cl-letf (((symbol-function 'agent-repl--sync-panels) #'ignore)
                ((symbol-function 'agent-repl--update-hide-overlay) #'ignore)
                ((symbol-function 'agent-repl--reset-vterm-cursors)
                 (lambda () (setq reset-called t))))
        (agent-repl--on-window-change)
        (should-not reset-called)))))

(ert-deftest agent-repl-test-panels-no-cursor-reset-on-selection-change ()
  "No agent-repl cursor-reset handler is installed on
`window-selection-change-functions'.  If one is, every focus change
schedules `--reset-vterm-cursors', which snaps vterm to the bottom and
undoes user scrolls."
  (should-not
   (cl-find-if
    (lambda (fn)
      (and (symbolp fn)
           (string-prefix-p "agent-repl--" (symbol-name fn))
           (string-match-p "cursor-reset\\|reset-vterm" (symbol-name fn))))
    window-selection-change-functions)))

(ert-deftest agent-repl-test-panels-no-cursor-reset-on-buffer-list-update ()
  "No agent-repl cursor-reset handler is installed on
`buffer-list-update-hook'.  If one is, normal buffer activity
schedules `--reset-vterm-cursors', which snaps vterm to the bottom and
undoes user scrolls."
  (should-not
   (cl-find-if
    (lambda (fn)
      (and (symbolp fn)
           (string-prefix-p "agent-repl--" (symbol-name fn))
           (string-match-p "cursor-reset\\|reset-vterm" (symbol-name fn))))
    buffer-list-update-hook)))

(ert-deftest agent-repl-test-panels-on-workspace-switch-still-resets-cursors ()
  "Workspace switch is the one place that DOES reset vterm cursors.
This preserves the recenter-after-switch behavior while the broader
hooks are gone."
  (agent-repl-test--with-clean-state
    (let ((reset-called nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--maybe-sweep-hidden-on-switch) #'ignore)
                ((symbol-function 'agent-repl--update-all-workspace-states-now) #'ignore)
                ((symbol-function 'agent-repl--refresh-vterm) #'ignore)
                ((symbol-function 'agent-repl--reset-vterm-cursors)
                 (lambda () (setq reset-called t)))
                ((symbol-function 'agent-repl--drain-pending-magit) #'ignore)
                ((symbol-function 'agent-repl--drain-pending-initial-buffers) #'ignore)
                ((symbol-function 'agent-repl--drain-pending-show-panels) #'ignore)
                ((symbol-function 'agent-repl--maybe-autoselect-input) #'ignore))
        (agent-repl--on-workspace-switch "ws1")
        (should reset-called)))))

;;;; ---- Tests: bounce-from-vterm ----

(ert-deftest agent-repl-test-panels-bounce-from-vterm-non-vterm-buffer ()
  "bounce-from-vterm is a no-op when the selected window shows a non-agent buffer."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*bounce-noop-regular*"
      (let ((orig-win (selected-window)))
        (set-window-buffer orig-win (current-buffer))
        (agent-repl--bounce-from-vterm nil)
        (should (eq (selected-window) orig-win))))))

(ert-deftest agent-repl-test-panels-bounce-from-vterm-input-buffer-no-recursion ()
  "bounce-from-vterm does NOT fire when the selected window shows an input buffer.
Load-bearing: after the bounce redirects vterm→input, the input selection
must not itself trigger another bounce."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-input-test-ws*"
      (let ((orig-win (selected-window)))
        (set-window-buffer orig-win (current-buffer))
        (let ((last-input-event ?a))
          (agent-repl--bounce-from-vterm nil))
        (should (eq (selected-window) orig-win))))))

(ert-deftest agent-repl-test-panels-bounce-from-vterm-keyboard-redirects ()
  "bounce-from-vterm redirects to the input window when selection is keyboard-driven."
  (agent-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*agent-panel-test-ws*"))
          (input-buf (get-buffer-create "*agent-panel-input-test-ws*"))
          (new-win nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (set-window-buffer (selected-window) vterm-buf)
            (setq new-win (split-window))
            (set-window-buffer new-win input-buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
              (let ((last-input-event ?a))
                (agent-repl--bounce-from-vterm nil)
                (should (eq (window-buffer (selected-window)) input-buf)))))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

(ert-deftest agent-repl-test-panels-bounce-from-vterm-mouse-does-not-redirect ()
  "Mouse-driven selection of a vterm window stays put — user wants to scroll/copy."
  (agent-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*agent-panel-test-ws*"))
          (input-buf (get-buffer-create "*agent-panel-input-test-ws*"))
          (new-win nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (let ((vterm-win (selected-window)))
              (set-window-buffer vterm-win vterm-buf)
              (setq new-win (split-window))
              (set-window-buffer new-win input-buf)
              (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
                ;; Simulate a mouse event as last-input-event
                (let ((last-input-event '(mouse-1 (nil 0 . 0))))
                  (agent-repl--bounce-from-vterm nil)
                  (should (eq (selected-window) vterm-win))))))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

(ert-deftest agent-repl-test-panels-bounce-from-vterm-warns-when-no-input-win ()
  "When panels are hidden (no visible input window), bounce emits a user-facing warning.
Previously this path logged verbosely and stranded point in vterm; now
we at least surface the stuck state so the user knows to click out."
  (agent-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*agent-panel-test-ws*"))
          (messages nil))
      (unwind-protect
          (progn
            ;; Input buffer is stored but NOT displayed in any window.
            (agent-repl--ws-put "test-ws" :input-buffer
                                 (get-buffer-create "*agent-panel-input-test-ws*"))
            (set-window-buffer (selected-window) vterm-buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
              (let ((last-input-event ?a))
                (agent-repl--bounce-from-vterm nil)))
            (should (cl-some (lambda (m) (string-match-p "input panel isn't visible" m))
                             messages)))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))
        (when-let ((b (get-buffer "*agent-panel-input-test-ws*"))) (kill-buffer b))))))

;;;; ---- Tests: initialize-input-buffer ----

(ert-deftest agent-repl-test-initialize-input-buffer-fresh ()
  "initialize-input-buffer enables agent-repl-input-mode and restores history on a fresh buffer."
  (agent-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *init-input-fresh*"))
          (mode-called nil)
          (history-called nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--create-buffer)
                     (lambda (_ws &optional _s) buf))
                    ((symbol-function 'agent-repl-input-mode)
                     (lambda () (setq mode-called t)))
                    ((symbol-function 'agent-repl--history-restore)
                     (lambda (_ws) (setq history-called t))))
            (agent-repl--initialize-input-buffer "test-ws")
            (should mode-called)
            (should history-called)
            (should (eq (agent-repl--ws-get "test-ws" :input-buffer) buf)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest agent-repl-test-initialize-input-buffer-already-initialized ()
  "initialize-input-buffer errors when the buffer is already in agent-repl-input-mode."
  (agent-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *init-input-already*")))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (setq major-mode 'agent-repl-input-mode))
            (cl-letf (((symbol-function 'agent-repl--create-buffer)
                       (lambda (_ws &optional _s) buf)))
              (should-error (agent-repl--initialize-input-buffer "test-ws"))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest agent-repl-test-panels-spc-o-C-kills-session ()
  "hide-and-preserve-status (SPC o C) kills through the frontend registry.
SPC o C means done-with-this-session; the plain SPC o c close only
puts the view away."
  (agent-repl-test--with-clean-state
    (let ((killed nil)
          (closed nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--on-close)
                 (lambda (_ws) (setq closed t)))
                ((symbol-function 'agent-repl--ws-frontend)
                 (lambda (_ws) (agent-repl-frontend-create
                                :name 'probe
                                :open-fn #'ignore
                                :kill-fn (lambda (_ws) (setq killed t))
                                :send-fn #'ignore
                                :interrupt-fn #'ignore
                                :running-p-fn #'ignore
                                :supported-backends '(claude)))))
        (agent-repl--hide-and-preserve-status)
        (should closed)
        (should killed)
        ;; The hide-mode sweep marker survives the kill's state reset.
        (should (eq (agent-repl--ws-get "ws1" :repl-state) :hidden))))))

(ert-deftest agent-repl-test-panels-on-close-never-kills ()
  "on-close itself must NOT kill: send-and-hide and the drawer close
hide sessions that keep running."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--ws-put "ws1" :frontend 'vterm)
    (let ((killed nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--hide-panels) (lambda () nil))
                ((symbol-function 'agent-repl--save-tab-index) #'ignore)
                ((symbol-function 'agent-repl-workspace-push-to-back) #'ignore)
                ((symbol-function 'agent-repl--ws-frontend)
                 (lambda (_ws) (agent-repl-frontend-create
                                :name 'probe :open-fn #'ignore
                                :kill-fn (lambda (_ws) (setq killed t))
                                :send-fn #'ignore :interrupt-fn #'ignore
                                :running-p-fn #'ignore
                                :supported-backends '(claude)))))
        (agent-repl--on-close "ws1")
        (should-not killed)))))

;;;; ---- Tests: kill-stale-vterm ----

(ert-deftest agent-repl-test-panels-kill-stale-vterm-no-buffer ()
  "kill-stale-vterm is a no-op when no buffer with the expected name exists."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--buffer-name)
               (lambda (&rest _) "*nonexistent-stale*")))
      ;; Should not error
      (agent-repl--kill-stale-vterm))))

(ert-deftest agent-repl-test-panels-kill-stale-vterm-stale ()
  "kill-stale-vterm kills a buffer that exists without a live process."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*stale-vterm-test*")))
      (cl-letf (((symbol-function 'agent-repl--buffer-name)
                 (lambda (&rest _) "*stale-vterm-test*")))
        (should (get-buffer "*stale-vterm-test*"))
        (agent-repl--kill-stale-vterm)
        (should-not (get-buffer "*stale-vterm-test*"))))))

(ert-deftest agent-repl-test-panels-kill-stale-vterm-zombie-process ()
  "kill-stale-vterm kills a live-process leftover through the queryless path.
Callers reach it only after the already-running guard passed, so a
live process here is a zombie from a failed teardown — preserving it
(the old behavior) made the next launch die on \"already initialized\"
behind an interactive kill prompt."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*process-vterm-test*"))
          (killed nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--buffer-name)
                     (lambda (&rest _) "*process-vterm-test*"))
                    ((symbol-function 'get-buffer-process) (lambda (_buf) 'fake-process))
                    ((symbol-function 'agent-repl--kill-vterm-process)
                     (lambda (b) (setq killed b))))
            (agent-repl--kill-stale-vterm)
            ;; Routed through the queryless process-kill wrapper.
            (should (eq killed buf)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;;;; ---- Tests: kill-vterm-process live buffer without process ----

(ert-deftest agent-repl-test-panels-kill-vterm-process-live-no-process ()
  "kill-vterm-process kills a live buffer that has no process."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*kill-no-proc*")))
      (agent-repl--kill-vterm-process buf)
      ;; Buffer should have been killed
      (should-not (buffer-live-p buf)))))

(ert-deftest agent-repl-test-panels-kill-vterm-process-skips-kill-buffer-query-functions ()
  "kill-vterm-process does not consult `kill-buffer-query-functions'.
Regression: the nuke path must not prompt about closing the agent
process, even when other hooks (e.g., vterm's own kill query) are
registered."
  (agent-repl-test--with-clean-state
    (let* ((buf (get-buffer-create "*kill-no-prompt*"))
           (consulted nil)
           (kill-buffer-query-functions
            (list (lambda () (setq consulted t) nil))))
      (agent-repl--kill-vterm-process buf)
      (should-not consulted)
      (should-not (buffer-live-p buf)))))

;;;; ---- Tests: teardown-session-state ----

(ert-deftest agent-repl-test-panels-teardown-session-state ()
  "teardown-session-state clears overlay, timers, and workspace buffer refs."
  (agent-repl-test--with-clean-state
    (let ((overlay-disabled nil)
          (state-saved nil))
      (agent-repl-test--with-temp-buffer "*teardown-vterm*"
        (let ((vterm-buf (current-buffer)))
          (agent-repl-test--with-temp-buffer "*teardown-input*"
            (let ((input-buf (current-buffer)))
              (agent-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
              (agent-repl--ws-put "test-ws" :input-buffer input-buf)
              (agent-repl--ws-put "test-ws" :active-env :bare-metal)
              (agent-repl--ws-put "test-ws" :bare-metal
                                   (make-agent-repl-instantiation :start-cmd "claude" :session-id "sess-1"))
              (cl-letf (((symbol-function 'agent-repl--disable-hide-overlay)
                         (lambda () (setq overlay-disabled t)))
                        ((symbol-function 'agent-repl--state-save)
                         (lambda (_ws) (setq state-saved t)))
                        ((symbol-function 'force-mode-line-update) (lambda (&rest _) nil)))
                (agent-repl--teardown-session-state "test-ws")
                (should overlay-disabled)
                (should state-saved)
                ;; Buffer refs should be cleared
                (should-not (agent-repl--ws-get "test-ws" :vterm-buffer))
                (should-not (agent-repl--ws-get "test-ws" :input-buffer))
                ;; Instantiation should have start-cmd cleared
                (let ((inst (agent-repl--ws-get "test-ws" :bare-metal)))
                  (should-not (agent-repl-instantiation-start-cmd inst)))))))))))

;;;; ---- Tests: destroy-session-buffers input dead ----

(ert-deftest agent-repl-test-panels-destroy-session-buffers-input-dead ()
  "destroy-session-buffers handles a dead input buffer gracefully."
  (agent-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*destroy-vterm*"))
          (input-buf (get-buffer-create "*destroy-input-dead*")))
      (kill-buffer input-buf)
      (cl-letf (((symbol-function 'agent-repl--close-buffer-windows) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--kill-placeholder) (lambda () nil))
                ((symbol-function 'agent-repl--kill-vterm-process) (lambda (_) nil)))
        ;; Should not error with dead input buffer
        (agent-repl--destroy-session-buffers vterm-buf input-buf)))))

;;;; ---- Tests: kill-workspace-buffers ----

(ert-deftest agent-repl-test-panels-kill-workspace-buffers/persp-mode-off ()
  "kill-workspace-buffers is a no-op when persp-mode is not active."
  (let ((persp-mode nil)
        (buf (get-buffer-create "*kwb-persp-off*")))
    (unwind-protect
        (progn
          (agent-repl--kill-workspace-buffers "some-ws")
          ;; Buffer survives because persp-mode is off.
          (should (buffer-live-p buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-panels-kill-workspace-buffers/no-persp-for-ws ()
  "kill-workspace-buffers is a no-op when the persp does not exist."
  (let ((persp-mode t)
        (buf (get-buffer-create "*kwb-no-persp*")))
    (unwind-protect
        (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_ws) nil)))
          (agent-repl--kill-workspace-buffers "ghost-ws")
          (should (buffer-live-p buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-panels-kill-workspace-buffers/symbol-persp-skipped ()
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
          (agent-repl--kill-workspace-buffers "sym-ws")
          (should-not persp-buffers-called)
          (should (buffer-live-p buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-panels-kill-workspace-buffers/kills-all-live-buffers ()
  "kill-workspace-buffers kills every live buffer returned by persp-buffers."
  (let ((persp-mode t)
        (b1 (get-buffer-create "*kwb-live-1*"))
        (b2 (get-buffer-create "*kwb-live-2*"))
        (b3 (get-buffer-create "*kwb-live-3*")))
    (unwind-protect
        (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_ws) (list 'persp)))
                  ((symbol-function 'persp-buffers) (lambda (_p) (list b1 b2 b3))))
          (agent-repl--kill-workspace-buffers "live-ws")
          (should-not (buffer-live-p b1))
          (should-not (buffer-live-p b2))
          (should-not (buffer-live-p b3)))
      (dolist (b (list b1 b2 b3))
        (when (buffer-live-p b) (kill-buffer b))))))

(ert-deftest agent-repl-test-panels-kill-workspace-buffers/skips-dead-and-nil ()
  "kill-workspace-buffers tolerates dead and nil entries in the buffer list."
  (let ((persp-mode t)
        (live (get-buffer-create "*kwb-mixed-live*"))
        (dead (get-buffer-create "*kwb-mixed-dead*")))
    (kill-buffer dead)
    (unwind-protect
        (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_ws) (list 'persp)))
                  ((symbol-function 'persp-buffers) (lambda (_p) (list nil dead live))))
          ;; Should not error despite nil / dead entries.
          (agent-repl--kill-workspace-buffers "mixed-ws")
          (should-not (buffer-live-p live)))
      (when (buffer-live-p live) (kill-buffer live)))))

(ert-deftest agent-repl-test-panels-kill-workspace-buffers/spares-foreign-owned ()
  "kill-workspace-buffers does NOT kill a buffer owned by a different workspace.
Regression guard: persp-mode can drift another workspace's live agent panel
into this persp, and nuking it would wipe that workspace's running session."
  (let ((persp-mode t)
        (foreign (get-buffer-create "*agent-panel-other-ws*")))
    (with-current-buffer foreign
      (setq-local agent-repl--owning-workspace "other-ws"))
    (unwind-protect
        (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_ws) (list 'persp)))
                  ((symbol-function 'persp-buffers) (lambda (_p) (list foreign))))
          (agent-repl--kill-workspace-buffers "this-ws")
          (should (buffer-live-p foreign)))
      (when (buffer-live-p foreign) (kill-buffer foreign)))))

(ert-deftest agent-repl-test-panels-kill-workspace-buffers/kills-own-owned ()
  "kill-workspace-buffers kills a buffer owned by the workspace being nuked."
  (let ((persp-mode t)
        (own (get-buffer-create "*agent-panel-this-ws*")))
    (with-current-buffer own
      (setq-local agent-repl--owning-workspace "this-ws"))
    (unwind-protect
        (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_ws) (list 'persp)))
                  ((symbol-function 'persp-buffers) (lambda (_p) (list own))))
          (agent-repl--kill-workspace-buffers "this-ws")
          (should-not (buffer-live-p own)))
      (when (buffer-live-p own) (kill-buffer own)))))

(ert-deftest agent-repl-test-panels-kill-workspace-buffers/kills-attached-process ()
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
                  ((symbol-function 'agent-repl--schedule-sigkill) #'ignore))
          (agent-repl--kill-workspace-buffers "proc-ws")
          (should (memq proc deleted-procs))
          (should (memq proc query-cleared-procs))
          (should-not (buffer-live-p buf)))
      (when (process-live-p proc) (delete-process proc))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-panels-kill-workspace-buffers/kills-modified-buffer-without-prompt ()
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
            (agent-repl--kill-workspace-buffers "modified-ws"))
          (should-not (buffer-live-p buf)))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-panels-kill-workspace-buffers/continues-after-error ()
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
          (agent-repl--kill-workspace-buffers "err-ws")
          ;; b1 killed normally, b2 errored (still live), b3 killed after the error.
          (should-not (buffer-live-p b1))
          (should (buffer-live-p b2))
          (should-not (buffer-live-p b3)))
      (dolist (b (list b1 b2 b3))
        (when (buffer-live-p b) (kill-buffer b))))))

;;;; ---- Tests: seed-new-env-session existing inst without session-id ----

(ert-deftest agent-repl-test-panels-seed-new-env-existing-no-session-id ()
  "seed-new-env-session seeds an existing instantiation that has no session-id."
  (agent-repl-test--with-clean-state
    (let ((existing (make-agent-repl-instantiation)))
      ;; existing has nil session-id
      (agent-repl--ws-put "test-ws" :sandbox existing)
      (agent-repl--seed-new-env-session "test-ws" :sandbox "new-sess-id")
      (let ((inst (agent-repl--ws-get "test-ws" :sandbox)))
        ;; Should have been seeded since there was no existing session-id
        (should (equal (agent-repl-instantiation-session-id inst) "new-sess-id"))))))

;;;; ---- Tests: show-existing-panels no workspace ----

(ert-deftest agent-repl-test-panels-show-existing-panels-no-workspace ()
  "show-existing-panels errors when no workspace is active."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
              ((symbol-function 'agent-repl--refresh-vterm) (lambda () nil)))
      (should-error (agent-repl--show-existing-panels)))))

;;;; ---- Tests: initialize-agent ----

(ert-deftest agent-repl-test-panels-initialize-agent-no-workspace ()
  "initialize-agent errors when no workspace is active."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
      (should-error (agent-repl--initialize-agent)))))

(ert-deftest agent-repl-test-panels-initialize-agent-already-running-errors ()
  "initialize-agent errors when the agent is already running."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'agent-repl--agent-running-p) (lambda (_ws) t)))
      (should-error (agent-repl--initialize-agent)))))

(defmacro agent-repl-test--initialize-agent-stubs (vterm-buf-var &rest body)
  "Run BODY with the stubs needed to exercise `agent-repl--initialize-agent'.
VTERM-BUF-VAR is the name of a `let'-bound buffer that will be returned
from `create-buffer'.  Stubs can be overridden by wrapping BODY in another
`cl-letf' that rebinds the same symbols."
  (declare (indent 1))
  `(cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
             ((symbol-function 'agent-repl--agent-running-p) (lambda (&optional _ws) nil))
             ((symbol-function 'agent-repl--initialize-ws-env) #'ignore)
             ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp"))
             ((symbol-function 'agent-repl--record-project-dir) #'ignore)
             ((symbol-function 'agent-repl--kill-stale-vterm) (lambda (&optional _ws) nil))
             ((symbol-function 'agent-repl--create-buffer)
              (lambda (_ws &optional _s) ,vterm-buf-var))
             ((symbol-function 'agent-repl--build-start-cmd)
              (lambda (_ws) (list :cmd "claude"
                                  :sandboxed-p nil
                                  :docker-image nil
                                  :session-id nil
                                  :fork-session-id nil
                                  :worktree-p nil
                                  :active-env :bare-metal
                                  :inst (make-agent-repl-instantiation))))
             ((symbol-function 'agent-repl--log-session-start) #'ignore)
             ((symbol-function 'vterm-mode) #'ignore)
             ((symbol-function 'agent-repl--set-buffer-background) #'ignore)
             ((symbol-function 'agent-repl--workspace-mode-line) (lambda (_ws) '("test")))
             ((symbol-function 'vterm-send-string) #'ignore)
             ((symbol-function 'vterm-send-return) #'ignore)
             ((symbol-function 'agent-repl--schedule-ready-timer) #'ignore)
             ((symbol-function 'agent-repl--initialize-input-buffer) #'ignore)
             ((symbol-function 'agent-repl--enable-hide-overlay) #'ignore)
             ;; Launch-time panel open (the 2026-07 gate change) is
             ;; neutralized here; the dedicated launch-opens-panels tests
             ;; rebind it to observe the call.
             ((symbol-function 'agent-repl--show-hidden-panels) #'ignore)
             ((symbol-function 'agent-repl--workspace-id) (lambda () "id")))
     ,@body))

(ert-deftest agent-repl-test-panels-initialize-agent-stamps-vterm-frontend ()
  "initialize-agent stamps :frontend 'vterm — the function IS the vterm boot.
A workspace created while `agent-repl-default-frontend' is gui (the
workspace-generation dispatch path) would otherwise resolve to the gui
branch of `agent-repl--on-session-start-event', which never marks the
vterm ready nor drains the dispatched initial prompt."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :active-env :bare-metal)
    (let ((agent-repl-default-frontend 'gui)
          (vterm-buf (generate-new-buffer " *init-agent-stamp*")))
      (unwind-protect
          (agent-repl-test--initialize-agent-stubs vterm-buf
            (agent-repl--initialize-agent "test-ws")
            (should (eq (agent-repl--ws-get "test-ws" :frontend) 'vterm))
            (should-not (agent-repl--ws-gui-frontend-p "test-ws")))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest agent-repl-test-panels-initialize-agent-opens-panels-at-launch ()
  "initialize-agent opens panels immediately for the current workspace.
Pins the 2026-07 gate change: blocking first-run screens (trust
dialogs, codex onboarding) fire no readiness hook, so panels must not
wait for ready."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :active-env :bare-metal)
    (let ((vterm-buf (generate-new-buffer " *init-agent-open*"))
          (shown nil))
      (unwind-protect
          (agent-repl-test--initialize-agent-stubs vterm-buf
            (cl-letf (((symbol-function 'agent-repl--show-hidden-panels)
                       (lambda () (setq shown t))))
              (agent-repl--initialize-agent "test-ws")
              (should shown)))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest agent-repl-test-panels-initialize-agent-background-boot-no-panels ()
  "initialize-agent does NOT open panels for a non-current workspace boot."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "bg-ws" :active-env :bare-metal)
    (let ((vterm-buf (generate-new-buffer " *init-agent-bg*"))
          (shown nil))
      (unwind-protect
          (agent-repl-test--initialize-agent-stubs vterm-buf
            (cl-letf (((symbol-function 'agent-repl--show-hidden-panels)
                       (lambda () (setq shown t))))
              (agent-repl--initialize-agent "bg-ws")
              (should-not shown)))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest agent-repl-test-panels-initialize-agent-hidden-pref-no-panels ()
  "initialize-agent honors a persisted :hidden repl-state (panels stay closed)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :active-env :bare-metal)
    (agent-repl--ws-put "test-ws" :repl-state :hidden)
    (let ((vterm-buf (generate-new-buffer " *init-agent-hidden*"))
          (shown nil))
      (unwind-protect
          (agent-repl-test--initialize-agent-stubs vterm-buf
            (cl-letf (((symbol-function 'agent-repl--show-hidden-panels)
                       (lambda () (setq shown t))))
              (agent-repl--initialize-agent "test-ws")
              (should-not shown)))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest agent-repl-test-panels-initialize-agent-starts-new-session ()
  "initialize-agent sets prefix counter, enables overlay, writes :agent-state :init."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :active-env :bare-metal)
    (let ((vterm-buf (generate-new-buffer " *init-agent-fixture*"))
          (overlay-called nil))
      (unwind-protect
          (agent-repl-test--initialize-agent-stubs vterm-buf
            (cl-letf (((symbol-function 'agent-repl--enable-hide-overlay)
                       (lambda () (setq overlay-called t))))
              (agent-repl--initialize-agent)
              (should (equal (agent-repl--ws-get "test-ws" :prefix-counter) 0))
              (should (eq (agent-repl--ws-get "test-ws" :agent-state) :init))
              (should overlay-called)))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest agent-repl-test-panels-initialize-agent-build-error-creates-no-buffer ()
  "A build-start-cmd abort (e.g. sandbox image not built) happens before any
buffer is created, so no orphan panel buffer is left behind."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :active-env :bare-metal)
    (let ((created nil))
      (agent-repl-test--initialize-agent-stubs nil
        (cl-letf (((symbol-function 'agent-repl--build-start-cmd)
                   (lambda (_ws) (user-error "Sandbox image not built")))
                  ((symbol-function 'agent-repl--create-buffer)
                   (lambda (_ws &optional _s) (setq created t) nil)))
          (should-error (agent-repl--initialize-agent) :type 'user-error)
          (should-not created)
          (should-not (agent-repl--ws-get "test-ws" :vterm-buffer)))))))

(ert-deftest agent-repl-test-panels-initialize-agent-launch-error-kills-orphan-buffer ()
  "A failure after the buffer is created kills the orphan buffer and clears
:vterm-buffer, so a failed start cannot leave a zombie workspace behind."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :active-env :bare-metal)
    (let ((vterm-buf (generate-new-buffer " *init-agent-orphan*")))
      (unwind-protect
          (agent-repl-test--initialize-agent-stubs vterm-buf
            (cl-letf (((symbol-function 'vterm-mode)
                       (lambda () (error "boom during launch"))))
              (should-error (agent-repl--initialize-agent))
              (should-not (buffer-live-p vterm-buf))
              (should-not (agent-repl--ws-get "test-ws" :vterm-buffer))))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest agent-repl-test-panels-initialize-agent-sends-cmd-and-return ()
  "initialize-agent sends the startup cmd string and a return to the vterm buffer."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :active-env :bare-metal)
    (let ((vterm-buf (generate-new-buffer " *init-agent-send*"))
          (sent-string nil)
          (return-sent nil))
      (unwind-protect
          (agent-repl-test--initialize-agent-stubs vterm-buf
            (cl-letf (((symbol-function 'vterm-send-string)
                       (lambda (s) (setq sent-string s)))
                      ((symbol-function 'vterm-send-return)
                       (lambda () (setq return-sent t))))
              (agent-repl--initialize-agent)
              (should (string-match-p "claude" sent-string))
              (should return-sent)))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest agent-repl-test-panels-initialize-agent-schedules-ready-timer ()
  "initialize-agent schedules the readiness timer for the workspace."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :active-env :bare-metal)
    (let ((vterm-buf (generate-new-buffer " *init-agent-timer*"))
          (timer-ws nil))
      (unwind-protect
          (agent-repl-test--initialize-agent-stubs vterm-buf
            (cl-letf (((symbol-function 'agent-repl--schedule-ready-timer)
                       (lambda (ws) (setq timer-ws ws))))
              (agent-repl--initialize-agent)
              (should (equal timer-ws "test-ws"))))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest agent-repl-test-panels-initialize-agent-sets-ready-nil ()
  "initialize-agent sets buffer-local agent-repl--ready to nil in the vterm buffer."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :active-env :bare-metal)
    (let ((vterm-buf (generate-new-buffer " *init-agent-ready*"))
          (ready-at-send 'unset))
      (unwind-protect
          (progn
            (with-current-buffer vterm-buf
              (setq-local agent-repl--ready t))
            (agent-repl-test--initialize-agent-stubs vterm-buf
              (cl-letf (((symbol-function 'vterm-send-string)
                         (lambda (_s) (setq ready-at-send agent-repl--ready))))
                (agent-repl--initialize-agent)
                (should-not ready-at-send))))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest agent-repl-test-panels-initialize-agent-sets-workspace-mode-line ()
  "initialize-agent sets mode-line-format via workspace-mode-line, passing ws."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :active-env :sandbox)
    (let ((vterm-buf (generate-new-buffer " *init-agent-ml*"))
          (mode-line-ws :unset))
      (unwind-protect
          (agent-repl-test--initialize-agent-stubs vterm-buf
            (cl-letf (((symbol-function 'agent-repl--build-start-cmd)
                       (lambda (_ws) (list :cmd "claude-sandbox"
                                           :sandboxed-p t
                                           :docker-image "img:latest"
                                           :session-id nil
                                           :fork-session-id nil
                                           :worktree-p t
                                           :active-env :sandbox
                                           :inst (make-agent-repl-instantiation))))
                      ((symbol-function 'agent-repl--workspace-mode-line)
                       (lambda (ws) (setq mode-line-ws ws) '("WS-ML"))))
              (agent-repl--initialize-agent)
              (should (equal mode-line-ws "test-ws"))
              (with-current-buffer vterm-buf
                (should (equal mode-line-format '("WS-ML"))))))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest agent-repl-test-panels-initialize-agent-clears-fork-session-id ()
  "initialize-agent clears :fork-session-id after building the cmd."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :active-env :bare-metal)
    (agent-repl--ws-put "test-ws" :fork-session-id "fork-abc")
    (let ((vterm-buf (generate-new-buffer " *init-agent-fork*")))
      (unwind-protect
          (agent-repl-test--initialize-agent-stubs vterm-buf
            (cl-letf (((symbol-function 'agent-repl--build-start-cmd)
                       (lambda (_ws) (list :cmd "claude"
                                           :sandboxed-p nil
                                           :docker-image nil
                                           :session-id nil
                                           :fork-session-id "fork-abc"
                                           :worktree-p nil
                                           :active-env :bare-metal
                                           :inst (make-agent-repl-instantiation)))))
              (agent-repl--initialize-agent)
              (should-not (agent-repl--ws-get "test-ws" :fork-session-id))))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest agent-repl-test-panels-initialize-agent-always-calls-ws-env-init ()
  "initialize-agent always calls initialize-ws-env, regardless of prior
`:active-env'.  initialize-ws-env is idempotent, so unconditional call is
safe and ensures the state file is re-read on every session start."
  (agent-repl-test--with-clean-state
    (let ((vterm-buf (generate-new-buffer " *init-agent-ws-env*"))
          (init-call-count 0))
      (unwind-protect
          (agent-repl-test--initialize-agent-stubs vterm-buf
            (cl-letf (((symbol-function 'agent-repl--initialize-ws-env)
                       (lambda (_ws &rest _) (cl-incf init-call-count))))
              (agent-repl--initialize-agent)
              (should (= init-call-count 1))))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest agent-repl-test-panels-initialize-agent-passes-hints-to-ws-env-init ()
  "initialize-agent forwards project-dir-hint and active-env-hint to
initialize-ws-env.  Models the worktree-creation / new-workspace paths."
  (agent-repl-test--with-clean-state
    (let ((vterm-buf (generate-new-buffer " *init-agent-hints*"))
          (got-hint nil)
          (got-env nil))
      (unwind-protect
          (agent-repl-test--initialize-agent-stubs vterm-buf
            (cl-letf (((symbol-function 'agent-repl--initialize-ws-env)
                       (lambda (_ws &optional dir env)
                         (setq got-hint dir)
                         (setq got-env env))))
              (agent-repl--initialize-agent "test-ws" "/tmp/worktree" :sandbox)
              (should (equal got-hint "/tmp/worktree"))
              (should (eq got-env :sandbox))))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest agent-repl-test-panels-initialize-agent-persists-state-on-success ()
  "initialize-agent calls state-save at the end of a successful start."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :active-env :bare-metal)
    (let ((vterm-buf (generate-new-buffer " *init-agent-state-save*"))
          (saved-ws nil))
      (unwind-protect
          (agent-repl-test--initialize-agent-stubs vterm-buf
            (cl-letf (((symbol-function 'agent-repl--state-save)
                       (lambda (ws) (setq saved-ws ws))))
              (agent-repl--initialize-agent)
              (should (equal saved-ws "test-ws"))))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest agent-repl-test-panels-initialize-agent-uses-explicit-ws-arg ()
  "initialize-agent uses the explicit WS argument rather than +workspace-current-name."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "explicit-arg-ws" :active-env :bare-metal)
    (let ((vterm-buf (generate-new-buffer " *init-agent-explicit*"))
          (running-ws nil))
      (unwind-protect
          (agent-repl-test--initialize-agent-stubs vterm-buf
            (cl-letf (((symbol-function '+workspace-current-name)
                       (lambda () "persp-current-ws"))
                      ((symbol-function 'agent-repl--agent-running-p)
                       (lambda (ws) (setq running-ws ws) nil)))
              (agent-repl--initialize-agent "explicit-arg-ws")
              (should (equal running-ws "explicit-arg-ws"))))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

;;;; ---- Tests: schedule-sigkill ----

(ert-deftest agent-repl-test-panels-schedule-sigkill-schedules-timer ()
  "schedule-sigkill schedules a timer to call sigkill-if-alive."
  (agent-repl-test--with-clean-state
    (let ((scheduled-fn nil)
          (scheduled-delay nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (delay _repeat fn &rest _args)
                   (setq scheduled-delay delay)
                   (setq scheduled-fn fn))))
        (agent-repl--schedule-sigkill 'fake-proc)
        (should (= scheduled-delay 0.5))
        (should (eq scheduled-fn #'agent-repl--sigkill-if-alive))))))

;;;; ---- Tests: sigkill-if-alive with live process ----

(ert-deftest agent-repl-test-panels-sigkill-if-alive-live-process ()
  "sigkill-if-alive sends SIGKILL to a live process."
  (let ((signaled nil))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_proc) t))
              ((symbol-function 'signal-process)
               (lambda (proc sig) (setq signaled (list proc sig)))))
      (agent-repl--sigkill-if-alive 'fake-proc)
      (should (equal signaled '(fake-proc SIGKILL))))))

;;;; ---- Tests: do-refresh ----

(ert-deftest agent-repl-test-panels-do-refresh-calls-redraw ()
  "do-refresh calls vterm-redraw and redisplay."
  (let ((redraw-called nil)
        (redisplay-called nil)
        (vterm--term 'fake-term))
    (cl-letf (((symbol-function 'agent-repl--vterm-redraw)
               (lambda () (setq redraw-called t)))
              ((symbol-function 'redisplay)
               (lambda (&rest _) (setq redisplay-called t))))
      (agent-repl--do-refresh)
      (should redraw-called)
      (should redisplay-called))))

;;;; ---- Tests: vterm-redraw with set term ----

(ert-deftest agent-repl-test-panels-vterm-redraw-with-term ()
  "vterm-redraw calls vterm--redraw when vterm--term is set."
  (let ((vterm--term 'fake-term)
        (redraw-arg nil))
    (cl-letf (((symbol-function 'vterm--redraw)
               (lambda (term) (setq redraw-arg term))))
      (agent-repl--vterm-redraw)
      (should (eq redraw-arg 'fake-term)))))

;;;; ---- Tests: fix-vterm-scroll with different window ----

(ert-deftest agent-repl-test-panels-fix-vterm-scroll-different-window-preserves-selection ()
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
              (agent-repl--fix-vterm-scroll buf))
            ;; New impl: no `select-window' calls at all — the snap is
            ;; driven via `set-window-start' / `set-window-point' alone.
            (should-not selections)
            (should (eq (selected-window) orig-win))))
      (when (and new-win (window-live-p new-win))
        (ignore-errors (delete-window new-win)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-panels-fix-vterm-scroll-different-window-calls-snap ()
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
          (cl-letf (((symbol-function 'agent-repl--snap-vterm-window-to-cursor)
                     (lambda (win) (setq snap-arg win))))
            (agent-repl--fix-vterm-scroll buf))
          (should (eq snap-arg new-win)))
      (when (and new-win (window-live-p new-win))
        (ignore-errors (delete-window new-win)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;; ---- Tests: snap-vterm-window-to-cursor ----

(ert-deftest agent-repl-test-panels-snap-vterm-window-to-cursor-positions-cursor-at-bottom ()
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
              (agent-repl--snap-vterm-window-to-cursor new-win)
              (should (= (window-start new-win) expected-start))
              (should (= (window-point new-win) (point-max))))))
      (when (and new-win (window-live-p new-win))
        (ignore-errors (delete-window new-win)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-panels-snap-vterm-window-to-cursor-short-buffer-uses-point-min ()
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
            (agent-repl--snap-vterm-window-to-cursor new-win)
            (should (= (window-start new-win) (point-min)))
            (should (= (window-point new-win) (point-max)))))
      (when (and new-win (window-live-p new-win))
        (ignore-errors (delete-window new-win)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-panels-snap-vterm-window-to-cursor-does-not-select-window ()
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
                (agent-repl--snap-vterm-window-to-cursor new-win))
              (should-not selections)
              (should (eq (selected-window) orig-win)))))
      (when (and new-win (window-live-p new-win))
        (ignore-errors (delete-window new-win)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;; ---- Tests: refresh-vterm-window ----

(ert-deftest agent-repl-test-panels-refresh-vterm-window-snaps-on-vterm-mode-buffer ()
  "refresh-vterm-window calls `--snap-vterm-window-to-cursor' on the
vterm window after the cursor reset + redraw, replacing the old bare
`set-window-point' tail."
  (let ((buf (get-buffer-create "*agent-panel-snap-test*"))
        (new-win nil)
        (snap-arg nil))
    (unwind-protect
        (progn
          (setq new-win (split-window))
          (set-window-buffer new-win buf)
          (with-current-buffer buf (setq major-mode 'vterm-mode))
          (cl-letf (((symbol-function 'agent-repl--agent-buffer-p) (lambda (_b) t))
                    ((symbol-function 'agent-repl--vterm-redraw) #'ignore)
                    ((symbol-function 'vterm-reset-cursor-point) #'ignore)
                    ((symbol-function 'agent-repl--snap-vterm-window-to-cursor)
                     (lambda (win) (setq snap-arg win))))
            (agent-repl--refresh-vterm-window new-win))
          (should (eq snap-arg new-win)))
      (when (and new-win (window-live-p new-win))
        (ignore-errors (delete-window new-win)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-panels-refresh-vterm-window-skips-non-agent-buffer ()
  "refresh-vterm-window is a no-op when the window's buffer is not an
agent vterm buffer — the snap helper must not run."
  (let ((buf (get-buffer-create "*not-agent-refresh*"))
        (new-win nil)
        (snap-called nil))
    (unwind-protect
        (progn
          (setq new-win (split-window))
          (set-window-buffer new-win buf)
          (cl-letf (((symbol-function 'agent-repl--agent-buffer-p) (lambda (_b) nil))
                    ((symbol-function 'agent-repl--snap-vterm-window-to-cursor)
                     (lambda (_win) (setq snap-called t))))
            (agent-repl--refresh-vterm-window new-win))
          (should-not snap-called))
      (when (and new-win (window-live-p new-win))
        (ignore-errors (delete-window new-win)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-panels-fix-vterm-scroll-non-vterm-mode-skips-reset ()
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
                    ((symbol-function 'agent-repl--snap-vterm-window-to-cursor)
                     #'ignore))
            (agent-repl--fix-vterm-scroll buf))
          (should-not reset-called))
      (when (and new-win (window-live-p new-win))
        (ignore-errors (delete-window new-win)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;; ---- Tests: agent-repl-restart ----

(ert-deftest agent-repl-test-panels-restart-kills-then-initializes ()
  "agent-repl-restart dispatches the vterm frontend's kill-then-initialize."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((order nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--vterm-kill)
                 (lambda (_ws) (push 'kill order)))
                ((symbol-function 'agent-repl--initialize-agent)
                 (lambda (&optional _ws &rest _) (push 'init order))))
        (agent-repl-restart)
        (should (equal (nreverse order) '(kill init)))))))

;;;; ---- Tests: agent-repl-kill no workspace ----

(ert-deftest agent-repl-test-panels-kill-no-workspace ()
  "agent-repl-kill errors when no active workspace."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
      (should-error (agent-repl-kill)))))

(ert-deftest agent-repl-test-panels-kill-clears-state-axes ()
  "agent-repl-kill resets :agent-state and :repl-state."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (agent-repl--ws-set "ws1" :thinking)
    (agent-repl--ws-set-repl-state "ws1" :inactive)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'agent-repl--kill-session) #'ignore)
              ((symbol-function 'force-mode-line-update) #'ignore))
      (agent-repl-kill)
      (should-not (agent-repl--ws-get "ws1" :agent-state))
      (should-not (agent-repl--ws-get "ws1" :repl-state)))))

;;;; ---- Tests: redirect-from-agent-before-save with agent window ----

(ert-deftest agent-repl-test-panels-redirect-claude-to-other-window ()
  "redirect-from-agent-before-save selects a non-agent window when current is the agent."
  (agent-repl-test--with-clean-state
    (let ((agent-buf (get-buffer-create "*agent-panel-abcd1234*"))
          (regular-buf (get-buffer-create "*regular-buf*"))
          (new-win nil))
      (unwind-protect
          (progn
            (switch-to-buffer agent-buf)
            (setq new-win (split-window))
            (set-window-buffer new-win regular-buf)
            (agent-repl--redirect-from-agent-before-save)
            ;; Should have redirected to the window showing regular-buf
            (should (eq (window-buffer (selected-window)) regular-buf)))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (switch-to-buffer "*scratch*")
        (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
        (when (buffer-live-p regular-buf) (kill-buffer regular-buf))))))

;;;; ---- Tests: redirect-from-agent-before-save fullscreen case ----

(ert-deftest agent-repl-test-panels-redirect-claude-only-window ()
  "redirect-from-agent-before-save skips redirect when the agent is the only window."
  (agent-repl-test--with-clean-state
    (let ((agent-buf (get-buffer-create "*agent-panel-abcd1234*")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer agent-buf)
            ;; Only one window shows the agent -- cl-find-if returns nil since
            ;; the only window is also an agent window
            (agent-repl--redirect-from-agent-before-save)
            ;; Should still be on the same agent buffer (no redirect target)
            (should (eq (window-buffer (selected-window)) agent-buf)))
        (switch-to-buffer "*scratch*")
        (when (buffer-live-p agent-buf) (kill-buffer agent-buf))))))

;;;; ---- Tests: redirect-from-agent-before-save side/dedicated windows ----

(ert-deftest agent-repl-test-panels-redirect-from-side-window ()
  "Redirect fires when selected window is a side window (e.g. the drawer).

Regression: a side window selected at persp save time would otherwise be
restored as the selected window, causing `+workspace/kill's fallback
`switch-to-buffer' to split a new window for the doom splash buffer."
  (agent-repl-test--with-clean-state
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
            (agent-repl--redirect-from-agent-before-save)
            (should (eq (selected-window) main-win))
            (should (eq (window-buffer (selected-window)) regular-buf)))
        (when (and side-win (window-live-p side-win))
          (ignore-errors (delete-window side-win)))
        (set-window-buffer (selected-window) "*scratch*")
        (when (buffer-live-p regular-buf) (kill-buffer regular-buf))
        (when (buffer-live-p side-buf) (kill-buffer side-buf))))))

(ert-deftest agent-repl-test-panels-redirect-from-dedicated-window ()
  "Redirect fires when selected window is dedicated.

Dedicated windows cause `switch-to-buffer' to fall back to pop-up
behavior and split, which is what produced the spurious splash buffer
window after a nuke."
  (agent-repl-test--with-clean-state
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
            (agent-repl--redirect-from-agent-before-save)
            (should (eq (selected-window) main-win))
            (should (eq (window-buffer (selected-window)) regular-buf)))
        (when (and extra-win (window-live-p extra-win))
          (set-window-dedicated-p extra-win nil)
          (ignore-errors (delete-window extra-win)))
        (set-window-buffer (selected-window) "*scratch*")
        (when (buffer-live-p regular-buf) (kill-buffer regular-buf))
        (when (buffer-live-p dedicated-buf) (kill-buffer dedicated-buf))))))

(ert-deftest agent-repl-test-panels-redirect-skips-side-window-as-target ()
  "Redirect target must skip side windows even when selected is an agent panel.

Regression: the previous predicate `non-agent-panel-window-p' returned
t for the drawer (a non-agent side window), so `cl-find-if' could
pick the drawer as the redirect destination — defeating the purpose
of the redirect."
  (agent-repl-test--with-clean-state
    (delete-other-windows)
    (let* ((agent-buf (get-buffer-create "*agent-panel-abcd1234*"))
           (regular-buf (get-buffer-create "*regular-buf*"))
           (side-buf    (get-buffer-create "*side-buf*"))
           (agent-win (selected-window))
           (regular-win nil)
           (side-win nil))
      (unwind-protect
          (progn
            (set-window-buffer agent-win agent-buf)
            (setq regular-win (split-window agent-win nil 'below))
            (set-window-buffer regular-win regular-buf)
            (setq side-win
                  (display-buffer-in-side-window side-buf '((side . right))))
            (should (window-live-p side-win))
            (select-window agent-win)
            (agent-repl--redirect-from-agent-before-save)
            (should (eq (selected-window) regular-win))
            (should (eq (window-buffer (selected-window)) regular-buf)))
        (when (and side-win (window-live-p side-win))
          (ignore-errors (delete-window side-win)))
        (when (and regular-win (window-live-p regular-win))
          (ignore-errors (delete-window regular-win)))
        (set-window-buffer (selected-window) "*scratch*")
        (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
        (when (buffer-live-p regular-buf) (kill-buffer regular-buf))
        (when (buffer-live-p side-buf) (kill-buffer side-buf))))))

(ert-deftest agent-repl-test-panels-redirect-skips-dedicated-as-target ()
  "Redirect target must skip dedicated windows."
  (agent-repl-test--with-clean-state
    (delete-other-windows)
    (let* ((agent-buf (get-buffer-create "*agent-panel-abcd1234*"))
           (regular-buf (get-buffer-create "*regular-buf*"))
           (ded-buf (get-buffer-create "*ded-buf*"))
           (agent-win (selected-window))
           (regular-win nil)
           (ded-win nil))
      (unwind-protect
          (progn
            (set-window-buffer agent-win agent-buf)
            (setq ded-win (split-window agent-win nil 'right))
            (set-window-buffer ded-win ded-buf)
            (set-window-dedicated-p ded-win t)
            (setq regular-win (split-window agent-win nil 'below))
            (set-window-buffer regular-win regular-buf)
            (select-window agent-win)
            (agent-repl--redirect-from-agent-before-save)
            (should (eq (selected-window) regular-win))
            (should (eq (window-buffer (selected-window)) regular-buf)))
        (when (and ded-win (window-live-p ded-win))
          (set-window-dedicated-p ded-win nil)
          (ignore-errors (delete-window ded-win)))
        (when (and regular-win (window-live-p regular-win))
          (ignore-errors (delete-window regular-win)))
        (set-window-buffer (selected-window) "*scratch*")
        (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
        (when (buffer-live-p regular-buf) (kill-buffer regular-buf))
        (when (buffer-live-p ded-buf) (kill-buffer ded-buf))))))

;;;; ---- Tests: save-target-window-p ----

(ert-deftest agent-repl-test-panels-save-target-window-p-regular ()
  "save-target-window-p returns non-nil for a plain window."
  (agent-repl-test--with-clean-state
    (should (agent-repl--save-target-window-p (selected-window)))))

(ert-deftest agent-repl-test-panels-save-target-window-p-agent-panel ()
  "save-target-window-p returns nil for a window showing an agent panel."
  (agent-repl-test--with-clean-state
    (let ((agent-buf (get-buffer-create "*agent-panel-abcd1234*")))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) agent-buf)
            (should-not (agent-repl--save-target-window-p (selected-window))))
        (set-window-buffer (selected-window) "*scratch*")
        (when (buffer-live-p agent-buf) (kill-buffer agent-buf))))))

(ert-deftest agent-repl-test-panels-save-target-window-p-side-window ()
  "save-target-window-p returns nil for a side window."
  (agent-repl-test--with-clean-state
    (delete-other-windows)
    (let* ((side-buf (get-buffer-create "*side-buf*"))
           (side-win (display-buffer-in-side-window side-buf '((side . right)))))
      (unwind-protect
          (should-not (agent-repl--save-target-window-p side-win))
        (when (and side-win (window-live-p side-win))
          (ignore-errors (delete-window side-win)))
        (when (buffer-live-p side-buf) (kill-buffer side-buf))))))

(ert-deftest agent-repl-test-panels-save-target-window-p-dedicated ()
  "save-target-window-p returns nil for a dedicated window."
  (agent-repl-test--with-clean-state
    (delete-other-windows)
    (let ((extra (split-window)))
      (unwind-protect
          (progn
            (set-window-dedicated-p extra t)
            (should-not (agent-repl--save-target-window-p extra)))
        (when (window-live-p extra)
          (set-window-dedicated-p extra nil)
          (ignore-errors (delete-window extra)))))))

;;;; ---- Tests: fullscreen-and-focus ----

(ert-deftest agent-repl-test-panels-fullscreen-and-focus-claude-branch-does-not-maximize ()
  "fullscreen-and-focus in an agent buffer only focuses input, never maximizes.
The agent panels already fill the frame (fullscreen is the sole
display format), so the agent branch must NOT touch the non-agent
maximize state (`agent-repl--window-fullscreen-config') nor sweep
windows."
  (agent-repl-test--with-clean-state
    (let ((sweep-called nil)
          (agent-repl--window-fullscreen-config nil)
          (agent-buf (get-buffer-create "*agent-panel-abcd1234*")))
      (unwind-protect
          (progn
            (switch-to-buffer agent-buf)
            (cl-letf (((symbol-function 'agent-repl-window--delete-where)
                       (lambda (&rest _) (setq sweep-called t) nil))
                      ((symbol-function '+workspace-current-name) (lambda () "test-ws")))
              (agent-repl-fullscreen-and-focus)
              (should-not sweep-called)
              (should-not agent-repl--window-fullscreen-config)))
        (switch-to-buffer "*scratch*")
        (when (buffer-live-p agent-buf) (kill-buffer agent-buf))))))

(ert-deftest agent-repl-test-panels-fullscreen-and-focus-selects-input ()
  "fullscreen-and-focus selects the input window after toggling when in an agent buffer."
  (agent-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*agent-panel-input-abcd1234*"))
          (agent-buf (get-buffer-create "*agent-panel-abcd1234*"))
          (new-win nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (switch-to-buffer agent-buf)
            (setq new-win (split-window))
            (set-window-buffer new-win input-buf)
            (cl-letf (
                      ((symbol-function '+workspace-current-name) (lambda () "test-ws")))
              (agent-repl-fullscreen-and-focus)
              (should (eq (window-buffer (selected-window)) input-buf))))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (switch-to-buffer "*scratch*")
        (when (buffer-live-p input-buf) (kill-buffer input-buf))
        (when (buffer-live-p agent-buf) (kill-buffer agent-buf))))))

(ert-deftest agent-repl-test-panels-fullscreen-and-focus-no-insert-state ()
  "fullscreen-and-focus does NOT enter evil insert state after focusing input."
  (agent-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*agent-panel-input-abcd1234*"))
          (agent-buf (get-buffer-create "*agent-panel-abcd1234*"))
          (new-win nil)
          (insert-called nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (switch-to-buffer agent-buf)
            (setq new-win (split-window))
            (set-window-buffer new-win input-buf)
            (cl-letf (
                      ((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                      ((symbol-function 'evil-insert-state)
                       (lambda (&rest _) (setq insert-called t))))
              (agent-repl-fullscreen-and-focus)
              (should-not insert-called)))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (switch-to-buffer "*scratch*")
        (when (buffer-live-p input-buf) (kill-buffer input-buf))
        (when (buffer-live-p agent-buf) (kill-buffer agent-buf))))))

(ert-deftest agent-repl-test-panels-fullscreen-and-focus-no-input-window ()
  "fullscreen-and-focus does not error when in an agent buffer but input has no window."
  (agent-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*test-fs-no-win*"))
          (agent-buf (get-buffer-create "*agent-panel-abcd1234*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (switch-to-buffer agent-buf)
            (cl-letf (
                      ((symbol-function '+workspace-current-name) (lambda () "test-ws")))
              ;; Input buffer exists but is not displayed — should not error
              (agent-repl-fullscreen-and-focus)
              (should-not (eq (window-buffer (selected-window)) input-buf))))
        (switch-to-buffer "*scratch*")
        (when (buffer-live-p input-buf) (kill-buffer input-buf))
        (when (buffer-live-p agent-buf) (kill-buffer agent-buf))))))

(ert-deftest agent-repl-test-panels-fullscreen-and-focus-no-input-buffer ()
  "fullscreen-and-focus does not error when in an agent buffer but no input buffer is set."
  (agent-repl-test--with-clean-state
    (let ((agent-buf (get-buffer-create "*agent-panel-abcd1234*")))
      (unwind-protect
          (progn
            (switch-to-buffer agent-buf)
            (cl-letf (
                      ((symbol-function '+workspace-current-name) (lambda () "test-ws")))
              ;; No input buffer at all — should not error
              (agent-repl-fullscreen-and-focus)))
        (switch-to-buffer "*scratch*")
        (when (buffer-live-p agent-buf) (kill-buffer agent-buf))))))

(ert-deftest agent-repl-test-panels-fullscreen-and-focus-non-agent-maximizes ()
  "fullscreen-and-focus saves config and sweeps other windows when not in an agent buffer."
  (agent-repl-test--with-clean-state
    (let ((sweep-called nil)
          (agent-repl--window-fullscreen-config nil))
      (switch-to-buffer (get-buffer-create "*other*"))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl-window--delete-where)
                     (lambda (&rest _) (setq sweep-called t) nil)))
            (agent-repl-fullscreen-and-focus)
            (should sweep-called)
            (should agent-repl--window-fullscreen-config))
        (setq agent-repl--window-fullscreen-config nil)
        (when (get-buffer "*other*") (kill-buffer "*other*"))))))

(ert-deftest agent-repl-test-panels-fullscreen-and-focus-non-agent-preserves-drawer ()
  "fullscreen-and-focus does NOT delete side windows (e.g. the drawer) when maximizing a non-agent buffer."
  (agent-repl-test--with-clean-state
    (let* ((other-buf (get-buffer-create "*other-fs*"))
           (drawer-buf (get-buffer-create "*agent-drawer-fs*"))
           (agent-repl--window-fullscreen-config nil)
           (predicate-captured nil)
           (skip-captured nil))
      (switch-to-buffer other-buf)
      (unwind-protect
          (let ((fake-drawer-win (split-window-right)))
            (set-window-buffer fake-drawer-win drawer-buf)
            (set-window-parameter fake-drawer-win 'window-side 'left)
            (cl-letf (((symbol-function 'agent-repl-window--delete-where)
                       (lambda (pred &rest args)
                         (setq predicate-captured pred
                               skip-captured (plist-get args :skip-side-windows))
                         nil)))
              (agent-repl-fullscreen-and-focus)
              ;; The sweep must skip side windows by default (drawer survives).
              (should (or (null skip-captured) (eq skip-captured t)))
              ;; Predicate keeps the selected (non-drawer) window and would
              ;; target the drawer window if side-windows were not skipped.
              (should (functionp predicate-captured))
              (should-not (funcall predicate-captured (selected-window)))
              (should (funcall predicate-captured fake-drawer-win))))
        (setq agent-repl--window-fullscreen-config nil)
        (when (buffer-live-p drawer-buf) (kill-buffer drawer-buf))
        (when (buffer-live-p other-buf) (kill-buffer other-buf))))))

(ert-deftest agent-repl-test-panels-fullscreen-and-focus-non-agent-real-drawer-survives ()
  "End-to-end: maximizing a non-agent buffer leaves a real side window alive."
  (agent-repl-test--with-clean-state
    (let* ((other-buf (get-buffer-create "*other-fs-real*"))
           (extra-buf (get-buffer-create "*extra-fs-real*"))
           (drawer-buf (get-buffer-create "*agent-drawer-fs-real*"))
           (agent-repl--window-fullscreen-config nil))
      (switch-to-buffer other-buf)
      (unwind-protect
          (let* ((extra-win (split-window-below))
                 (drawer-win (display-buffer-in-side-window
                              drawer-buf
                              '((side . left) (slot . 0)))))
            (set-window-buffer extra-win extra-buf)
            (should (window-live-p drawer-win))
            (agent-repl-fullscreen-and-focus)
            ;; Drawer (side window) is still alive after fullscreen.
            (should (window-live-p drawer-win))
            ;; Extra non-side window was swept.
            (should-not (window-live-p extra-win)))
        (setq agent-repl--window-fullscreen-config nil)
        (dolist (buf (list drawer-buf extra-buf other-buf))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest agent-repl-test-panels-fullscreen-and-focus-non-agent-restores ()
  "fullscreen-and-focus restores saved config on second press when not in an agent buffer."
  (agent-repl-test--with-clean-state
    (let* ((restore-called nil)
           (fake-config (list 'fake-window-config))
           (agent-repl--window-fullscreen-config fake-config))
      (switch-to-buffer (get-buffer-create "*other*"))
      (unwind-protect
          (cl-letf (((symbol-function 'set-window-configuration)
                     (lambda (cfg) (when (eq cfg fake-config) (setq restore-called t)))))
            (agent-repl-fullscreen-and-focus)
            (should restore-called)
            (should-not agent-repl--window-fullscreen-config))
        (setq agent-repl--window-fullscreen-config nil)
        (when (get-buffer "*other*") (kill-buffer "*other*"))))))

;;;; ---- Tests: --first-live-leaf ----

(ert-deftest agent-repl-test-panels-first-live-leaf-nil ()
  "`agent-repl--first-live-leaf' returns nil for nil input."
  (should-not (agent-repl--first-live-leaf nil)))

(ert-deftest agent-repl-test-panels-first-live-leaf-on-leaf ()
  "`agent-repl--first-live-leaf' returns WIN when WIN is already a live leaf."
  (agent-repl-test--with-clean-state
    (let ((leaf (selected-window)))
      (should (eq (agent-repl--first-live-leaf leaf) leaf)))))

(ert-deftest agent-repl-test-panels-first-live-leaf-descends-container ()
  "`agent-repl--first-live-leaf' descends an internal container window
to find a live leaf.  Real-world trigger: `window-main-window' returns
an internal container when the main area has been split."
  (agent-repl-test--with-clean-state
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
                     (leaf (agent-repl--first-live-leaf root)))
                (should-not (window-live-p root))
                (should (window-live-p leaf))
                (should (memq leaf (list main-win extra-win))))))
        (dolist (buf (list drawer-buf extra-buf main-buf))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

;;;; ---- Tests: --fullscreen-leave-side-window ----

(ert-deftest agent-repl-test-panels-fullscreen-leave-side-window-noop-on-main ()
  "`agent-repl--fullscreen-leave-side-window' does NOT change selection
when the selected window is already a non-side main-area window."
  (agent-repl-test--with-clean-state
    (let ((other-buf (get-buffer-create "*fs-leave-side-noop*")))
      (unwind-protect
          (progn
            (switch-to-buffer other-buf)
            (let ((orig (selected-window)))
              (agent-repl--fullscreen-leave-side-window)
              (should (eq (selected-window) orig))))
        (when (buffer-live-p other-buf) (kill-buffer other-buf))))))

(ert-deftest agent-repl-test-panels-fullscreen-leave-side-window-from-drawer ()
  "`agent-repl--fullscreen-leave-side-window' selects the frame's main
window when invoked from a side window."
  (agent-repl-test--with-clean-state
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
              (should (agent-repl-window--side-window-p (selected-window)))
              (agent-repl--fullscreen-leave-side-window)
              (should-not (agent-repl-window--side-window-p (selected-window)))
              (should (eq (selected-window) main-win))))
        (dolist (buf (list drawer-buf main-buf))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

;;;; ---- Tests: fullscreen-and-focus side-window redirect ----

(ert-deftest agent-repl-test-panels-fullscreen-and-focus-from-drawer-preserves-main ()
  "When invoked from inside the drawer side window with several main
windows visible, `agent-repl-fullscreen-and-focus' leaves the
originally-focused main window's siblings swept and the drawer alive —
crucially, the originating main window survives instead of being
sacrificed because the drawer was the `keep' anchor."
  (agent-repl-test--with-clean-state
    (let ((main-buf (get-buffer-create "*fs-from-drawer-main*"))
          (extra-buf (get-buffer-create "*fs-from-drawer-extra*"))
          (drawer-buf (get-buffer-create "*fs-from-drawer-drawer*"))
          (agent-repl--window-fullscreen-config nil))
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
                (agent-repl-fullscreen-and-focus)
                ;; Drawer (side window) survives.
                (should (window-live-p drawer-win))
                ;; The originating main window survives — without the
                ;; side-window redirect it would be deleted because the
                ;; drawer was `keep' and the predicate matches it.
                (should (window-live-p main-win))
                ;; The other main-area window is swept.
                (should-not (window-live-p extra-win)))))
        (setq agent-repl--window-fullscreen-config nil)
        (dolist (buf (list drawer-buf extra-buf main-buf))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest agent-repl-test-panels-fullscreen-and-focus-from-drawer-routes-to-claude-branch ()
  "When the drawer is selected but the main window contains an agent
panel buffer, the side-window redirect lands on the agent buffer and
the function takes the agent branch (focus input, no non-agent maximize)."
  (agent-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*agent-panel-fs-redir*"))
          (drawer-buf (get-buffer-create "*fs-redir-drawer*"))
          (agent-repl--window-fullscreen-config nil))
      (unwind-protect
          (progn
            (switch-to-buffer vterm-buf)
            (let* ((vterm-win (selected-window))
                   (drawer-win (display-buffer-in-side-window
                                drawer-buf
                                '((side . left) (slot . 0)))))
              (select-window drawer-win)
              (cl-letf (((symbol-function '+workspace-current-name)
                         (lambda () "test-ws")))
                (agent-repl-fullscreen-and-focus))
              ;; The redirect moved point onto the agent panel main window,
              ;; so the agent branch fired — the non-agent maximize branch
              ;; (which would save a window config) never ran.
              (should-not agent-repl--window-fullscreen-config)
              (should (window-live-p vterm-win))))
        (dolist (buf (list drawer-buf vterm-buf))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

;;;; ---- Tests: unhide-workspace ----

(ert-deftest agent-repl-test-unhide-workspace-flips-hidden-to-active ()
  "`agent-repl--unhide-workspace' resets `:hidden' to `:active'."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :repl-state :hidden)
    (agent-repl--unhide-workspace "ws")
    (should (eq (agent-repl--ws-get "ws" :repl-state) :active))))

(ert-deftest agent-repl-test-unhide-workspace-noop-on-non-hidden ()
  "`agent-repl--unhide-workspace' leaves non-hidden states alone."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :repl-state :inactive)
    (agent-repl--unhide-workspace "ws")
    (should (eq (agent-repl--ws-get "ws" :repl-state) :inactive))))

(ert-deftest agent-repl-test-unhide-workspace-nil-ws-noop ()
  "Nil WS is a no-op (matches the `:hidden'-only contract)."
  (agent-repl-test--with-clean-state
    (agent-repl--unhide-workspace nil)))

;;;; ---- Tests: clear-main-area-for-panels (drawer preservation) ----

(ert-deftest agent-repl-test-panels-clear-main-area-preserves-side-windows ()
  "`--clear-main-area-for-panels' must NOT delete side windows (drawer).
Opening the agent routes through `--show-existing-panels' which clears
the main area; the drawer side window must survive unconditionally,
even when its `no-delete-other-windows' parameter is absent (regression:
opening the agent used to destroy the drawer)."
  (agent-repl-test--with-clean-state
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
              (agent-repl--clear-main-area-for-panels)
              (should (window-live-p drawer-win))
              (should (get-buffer-window drawer-buf))
              ;; The "other" main-area window should have been deleted.
              (should-not (get-buffer-window other-buf))))
        (mapc (lambda (b) (when (buffer-live-p b) (kill-buffer b)))
              (list drawer-buf work-buf other-buf))))))

(ert-deftest agent-repl-test-panels-show-existing-panels-preserves-drawer ()
  "Opening the agent (full show-existing-panels flow) must NOT destroy the drawer.
End-to-end regression: any drawer-as-side-window setup survives the
panel-open path regardless of whether the drawer's window parameters
match the canonical display-action."
  (agent-repl-test--with-clean-state
    (let ((drawer-buf (get-buffer-create "*spe-drawer*"))
          (vterm-buf  (get-buffer-create "*spe-vterm*"))
          (input-buf  (get-buffer-create "*spe-input*"))
          (work-buf   (get-buffer-create "*spe-work*"))
          (ws         "spe-ws"))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :vterm-buffer vterm-buf)
            (agent-repl--ws-put ws :input-buffer input-buf)
            (delete-other-windows)
            (set-window-buffer (selected-window) work-buf)
            (let ((drawer-win (display-buffer-in-side-window
                              drawer-buf '((side . left) (slot . 0)))))
              (select-window (get-buffer-window work-buf))
              (cl-letf (((symbol-function '+workspace-current-name) (lambda () ws))
                        ((symbol-function 'agent-repl--refresh-vterm) #'ignore)
                        ((symbol-function 'agent-repl--update-hide-overlay) #'ignore)
                        ((symbol-function 'agent-repl--restore-tab-index) #'ignore)
                        ((symbol-function 'agent-repl--flash-current-tab) #'ignore)
                        ((symbol-function 'agent-repl--focus-input-panel) #'ignore))
                (agent-repl--show-existing-panels))
              (should (window-live-p drawer-win))
              (should (get-buffer-window drawer-buf))))
        (mapc (lambda (b) (when (buffer-live-p b) (kill-buffer b)))
              (list drawer-buf vterm-buf input-buf work-buf))))))

(ert-deftest agent-repl-test-panels-show-panels-redirects-from-side-window ()
  "`--show-panels' must not try to split a side window.
When the selected window is a side window (drawer), redirect to the
frame's main window before splitting; splitting a side window would
otherwise signal `Cannot split side window' and leave panels half-shown."
  (agent-repl-test--with-clean-state
    (let ((drawer-buf (get-buffer-create "*sp-redir-drawer*"))
          (work-buf   (get-buffer-create "*sp-redir-work*"))
          (vterm-buf  (get-buffer-create "*sp-redir-vterm*"))
          (input-buf  (get-buffer-create "*sp-redir-input*"))
          (ws         "sp-redir-ws"))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :vterm-buffer vterm-buf)
            (agent-repl--ws-put ws :input-buffer input-buf)
            (delete-other-windows)
            (set-window-buffer (selected-window) work-buf)
            (let ((drawer-win (display-buffer-in-side-window
                              drawer-buf '((side . left) (slot . 0)))))
              ;; Simulate selected window being the drawer (e.g. mouse-click
              ;; landed here just before the agent opened).
              (select-window drawer-win)
              (cl-letf (((symbol-function '+workspace-current-name) (lambda () ws))
                        ((symbol-function 'agent-repl--refresh-vterm) #'ignore)
                        ((symbol-function 'agent-repl--update-all-workspace-states-now) #'ignore))
                ;; Should NOT error.
                (agent-repl--show-panels))
              ;; Drawer still alive.
              (should (window-live-p drawer-win))
              ;; Panels were created.
              (should (get-buffer-window vterm-buf))
              (should (get-buffer-window input-buf))))
        (mapc (lambda (b) (when (buffer-live-p b) (kill-buffer b)))
              (list drawer-buf work-buf vterm-buf input-buf))))))

;;;; ---- Tests: stale-panel-windows ----

(ert-deftest agent-repl-test-panels-stale-panel-windows-returns-foreign-panels ()
  "stale-panel-windows returns windows showing panels from a different workspace."
  (agent-repl-test--with-clean-state
    (let ((foreign-buf (get-buffer-create "*agent-panel-other-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                    ((symbol-function 'window-list) (lambda (&rest _) (list (selected-window))))
                    ((symbol-function 'window-buffer) (lambda (_w) foreign-buf)))
            (let ((result (agent-repl--stale-panel-windows)))
              (should (= (length result) 1))
              (should (eq (car result) (selected-window)))))
        (kill-buffer foreign-buf)))))

(ert-deftest agent-repl-test-panels-stale-panel-windows-nil-for-own-panels ()
  "stale-panel-windows returns nil when panels belong to the current workspace."
  (agent-repl-test--with-clean-state
    (let ((own-buf (get-buffer-create "*agent-panel-my-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                    ((symbol-function 'window-list) (lambda (&rest _) (list (selected-window))))
                    ((symbol-function 'window-buffer) (lambda (_w) own-buf)))
            (should-not (agent-repl--stale-panel-windows)))
        (kill-buffer own-buf)))))

(ert-deftest agent-repl-test-panels-stale-panel-windows-nil-for-non-panel-buffers ()
  "stale-panel-windows returns nil when no agent panel buffers are visible."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
              ((symbol-function 'window-list) (lambda (&rest _) (list (selected-window))))
              ((symbol-function 'window-buffer) (lambda (_w) (get-buffer-create "*scratch*"))))
      (should-not (agent-repl--stale-panel-windows)))))

(ert-deftest agent-repl-test-panels-stale-panel-windows-nil-when-ws-nil ()
  "stale-panel-windows returns nil when current workspace is nil."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
      (should-not (agent-repl--stale-panel-windows)))))

(ert-deftest agent-repl-test-panels-stale-panel-windows-includes-input-buffers ()
  "stale-panel-windows detects foreign input panel buffers too."
  (agent-repl-test--with-clean-state
    (let ((foreign-input (get-buffer-create "*agent-panel-input-other-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                    ((symbol-function 'window-list) (lambda (&rest _) (list (selected-window))))
                    ((symbol-function 'window-buffer) (lambda (_w) foreign-input)))
            (should (= (length (agent-repl--stale-panel-windows)) 1)))
        (kill-buffer foreign-input)))))

;;;; ---- Tests: ensure-own-panels-on-persp-switch ----

(ert-deftest agent-repl-test-panels-ensure-own-closes-stale-windows ()
  "ensure-own-panels-on-persp-switch deletes a deletable stale panel window."
  (agent-repl-test--with-clean-state
    (let ((deleted nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                ((symbol-function 'agent-repl--stale-panel-windows)
                 (lambda () (list (selected-window))))
                ((symbol-function 'set-window-parameter) (lambda (_w _p _v) nil))
                ((symbol-function 'set-window-dedicated-p) (lambda (_w _v) nil))
                ;; Deletable window — safe-delete-window must call delete-window.
                ((symbol-function 'window-deletable-p) (lambda (_w) t))
                ((symbol-function 'delete-window)
                 (lambda (w) (push w deleted)))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil)))
        (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
        (should (= (length deleted) 1))))))

(ert-deftest agent-repl-test-panels-ensure-own-neutralizes-sole-stale-window ()
  "ensure-own-panels-on-persp-switch swaps an undeletable sole stale window to
the fallback buffer instead of erroring, then still reclaims the frame."
  (agent-repl-test--with-clean-state
    (let ((deleted nil)
          (swapped nil)
          (reclaimed nil)
          (fallback (get-buffer-create "*agent-test-fallback*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                    ((symbol-function 'agent-repl--stale-panel-windows)
                     (lambda () (list (selected-window))))
                    ((symbol-function 'set-window-parameter) (lambda (_w _p _v) nil))
                    ((symbol-function 'set-window-dedicated-p) (lambda (_w _v) nil))
                    ;; Sole window — delete-window would signal; safe-delete must
                    ;; swap to fallback rather than delete.
                    ((symbol-function 'window-deletable-p) (lambda (_w) nil))
                    ((symbol-function 'doom-fallback-buffer) (lambda () fallback))
                    ((symbol-function 'delete-window)
                     (lambda (w) (push w deleted)))
                    ((symbol-function 'set-window-buffer)
                     (lambda (_w b) (push b swapped)))
                    ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                    ((symbol-function 'agent-repl--reclaim-frame-fullscreen)
                     (lambda (_ws) (setq reclaimed t))))
            (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
            ;; The sole window is neutralized to fallback, not deleted ...
            (should-not deleted)
            (should (equal swapped (list fallback)))
            ;; ... and the handler still reaches the reclaim step (the
            ;; regression guard: a sole-window delete error must not abort it).
            (should reclaimed))
        (kill-buffer fallback)))))

(ert-deftest agent-repl-test-panels-ensure-own-restores-when-panels-were-visible ()
  "ensure-own-panels-on-persp-switch re-shows panels when :panels-were-visible is set."
  (agent-repl-test--with-clean-state
    (let ((show-called nil))
      (agent-repl--ws-put "my-ws" :panels-were-visible t)
      (let ((vterm-buf (get-buffer-create "*agent-panel-my-ws*"))
            (input-buf (get-buffer-create "*agent-panel-input-my-ws*")))
        (unwind-protect
            (progn
              (agent-repl--ws-put "my-ws" :vterm-buffer vterm-buf)
              (agent-repl--ws-put "my-ws" :input-buffer input-buf)
              (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                        ((symbol-function 'agent-repl--stale-panel-windows) (lambda () nil))
                        ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                        ((symbol-function 'agent-repl--show-panels)
                         (lambda () (setq show-called t))))
                (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
                (should show-called)))
          (kill-buffer vterm-buf)
          (kill-buffer input-buf))))))

(ert-deftest agent-repl-test-panels-ensure-own-noop-when-panels-already-visible ()
  "ensure-own-panels-on-persp-switch does not re-show if panels are already visible."
  (agent-repl-test--with-clean-state
    (let ((show-called nil))
      (agent-repl--ws-put "my-ws" :panels-were-visible t)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                ((symbol-function 'agent-repl--stale-panel-windows) (lambda () nil))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'agent-repl--show-panels)
                 (lambda () (setq show-called t))))
        (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
        (should-not show-called)))))

(ert-deftest agent-repl-test-panels-ensure-own-noop-when-no-stale-no-flag ()
  "ensure-own-panels-on-persp-switch is a no-op with no stale panels and no flag."
  (agent-repl-test--with-clean-state
    (let ((show-called nil)
          (delete-called nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                ((symbol-function 'agent-repl--stale-panel-windows) (lambda () nil))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'agent-repl--show-panels)
                 (lambda () (setq show-called t)))
                ((symbol-function 'delete-window)
                 (lambda (_w) (setq delete-called t))))
        (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
        (should-not show-called)
        (should-not delete-called)))))

(ert-deftest agent-repl-test-panels-ensure-own-skips-restore-when-buffers-dead ()
  "ensure-own-panels-on-persp-switch does not re-show if panel buffers are dead."
  (agent-repl-test--with-clean-state
    (let ((show-called nil))
      (agent-repl--ws-put "my-ws" :panels-were-visible t)
      ;; Buffers are nil (dead) — should not try to show.
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                ((symbol-function 'agent-repl--stale-panel-windows) (lambda () nil))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'agent-repl--show-panels)
                 (lambda () (setq show-called t))))
        (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
        (should-not show-called)))))

(ert-deftest agent-repl-test-panels-ensure-own-adds-input-when-output-visible ()
  "ensure-own-panels-on-persp-switch adds only the input window (not a full
rebuild) when the output window survived but the input window was dropped."
  (agent-repl-test--with-clean-state
    (let ((show-panels-called nil)
          (add-input-called nil))
      (agent-repl--ws-put "my-ws" :panels-were-visible t)
      (let ((vterm-buf (get-buffer-create "*agent-panel-my-ws*"))
            (input-buf (get-buffer-create "*agent-panel-input-my-ws*")))
        (unwind-protect
            (progn
              (agent-repl--ws-put "my-ws" :vterm-buffer vterm-buf)
              (agent-repl--ws-put "my-ws" :input-buffer input-buf)
              (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                        ((symbol-function 'agent-repl--stale-panel-windows) (lambda () nil))
                        ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                        ((symbol-function 'agent-repl--vterm-visible-p) (lambda () t))
                        ((symbol-function 'agent-repl--output-visible-input-hidden-p)
                         (lambda () nil))
                        ((symbol-function 'agent-repl--show-panels)
                         (lambda () (setq show-panels-called t)))
                        ((symbol-function 'agent-repl--show-input-beside-output)
                         (lambda () (setq add-input-called t))))
                (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
                (should add-input-called)
                (should-not show-panels-called)))
          (kill-buffer vterm-buf)
          (kill-buffer input-buf))))))

(ert-deftest agent-repl-test-panels-ensure-own-repairs-fullscreen-output-only ()
  "ensure-own-panels-on-persp-switch repairs a fullscreen output-only frame via
the trailing ensure-input-beside-output call, even when :panels-were-visible
was never recorded."
  (agent-repl-test--with-clean-state
    (let ((repair-called nil))
      ;; :panels-were-visible intentionally unset — the restore branch must
      ;; not fire; only the trailing repair should.
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                ((symbol-function 'agent-repl--stale-panel-windows) (lambda () nil))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'agent-repl--ensure-input-beside-output)
                 (lambda () (setq repair-called t))))
        (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
        (should repair-called)))))

;;;; ---- Tests: stale-window-buffers ----

(ert-deftest agent-repl-test-panels-stale-window-buffers-unique-live ()
  "stale-window-buffers returns each live window's buffer once."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*agent-panel-other-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function 'window-live-p) (lambda (_w) t))
                    ((symbol-function 'window-buffer) (lambda (_w) buf)))
            (should (equal (agent-repl--stale-window-buffers '(w1 w2))
                           (list buf))))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-panels-stale-window-buffers-drops-dead ()
  "stale-window-buffers drops dead windows."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*agent-panel-other-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function 'window-live-p) (lambda (w) (eq w 'live)))
                    ((symbol-function 'window-buffer) (lambda (_w) buf)))
            (should (equal (agent-repl--stale-window-buffers '(live dead))
                           (list buf))))
        (kill-buffer buf)))))

;;;; ---- Tests: detach-foreign-panel-buffers ----

(ert-deftest agent-repl-test-panels-detach-foreign-removes-each ()
  "detach-foreign-panel-buffers removes each live buffer from the persp."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*agent-panel-other-ws*"))
          (removed nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--ws-remove-buffer)
                     (lambda (b) (push b removed))))
            (agent-repl--detach-foreign-panel-buffers "my-ws" (list buf))
            (should (equal removed (list buf))))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-panels-detach-foreign-skips-dead ()
  "detach-foreign-panel-buffers does not remove a dead buffer."
  (agent-repl-test--with-clean-state
    (let ((dead (get-buffer-create "*agent-panel-dead-ws*"))
          (removed nil))
      (kill-buffer dead)
      (cl-letf (((symbol-function 'agent-repl--ws-remove-buffer)
                 (lambda (b) (push b removed))))
        (agent-repl--detach-foreign-panel-buffers "my-ws" (list dead))
        (should-not removed)))))

;;;; ---- Tests: safe-delete-window ----

(ert-deftest agent-repl-test-panels-safe-delete-window-deletes-deletable ()
  "safe-delete-window calls delete-window for a deletable ordinary window."
  (agent-repl-test--with-clean-state
    (let ((deleted nil)
          (swapped nil))
      (cl-letf (((symbol-function 'window-live-p) (lambda (_w) t))
                ((symbol-function 'set-window-parameter) (lambda (_w _p _v) nil))
                ((symbol-function 'set-window-dedicated-p) (lambda (_w _v) nil))
                ((symbol-function 'window-deletable-p) (lambda (_w) t))
                ((symbol-function 'window-buffer) (lambda (_w) nil))
                ((symbol-function 'delete-window) (lambda (w) (push w deleted)))
                ((symbol-function 'set-window-buffer) (lambda (_w b) (push b swapped))))
        (agent-repl--safe-delete-window 'win)
        (should (equal deleted (list 'win)))
        (should-not swapped)))))

(ert-deftest agent-repl-test-panels-safe-delete-window-swaps-sole-to-fallback ()
  "safe-delete-window swaps an undeletable sole window to the fallback buffer."
  (agent-repl-test--with-clean-state
    (let ((deleted nil)
          (swapped nil)
          (fallback (get-buffer-create "*agent-test-fallback*")))
      (unwind-protect
          (cl-letf (((symbol-function 'window-live-p) (lambda (_w) t))
                    ((symbol-function 'set-window-parameter) (lambda (_w _p _v) nil))
                    ((symbol-function 'set-window-dedicated-p) (lambda (_w _v) nil))
                    ((symbol-function 'window-deletable-p) (lambda (_w) nil))
                    ((symbol-function 'window-buffer) (lambda (_w) nil))
                    ((symbol-function 'doom-fallback-buffer) (lambda () fallback))
                    ((symbol-function 'delete-window) (lambda (w) (push w deleted)))
                    ((symbol-function 'set-window-buffer) (lambda (_w b) (push b swapped))))
            (agent-repl--safe-delete-window 'win)
            (should-not deleted)
            (should (equal swapped (list fallback))))
        (kill-buffer fallback)))))

(ert-deftest agent-repl-test-panels-safe-delete-window-honors-explicit-fallback ()
  "safe-delete-window swaps to the explicitly passed FALLBACK over the default."
  (agent-repl-test--with-clean-state
    (let ((swapped nil)
          (explicit (get-buffer-create "*agent-test-explicit*"))
          (default (get-buffer-create "*agent-test-default*")))
      (unwind-protect
          (cl-letf (((symbol-function 'window-live-p) (lambda (_w) t))
                    ((symbol-function 'set-window-parameter) (lambda (_w _p _v) nil))
                    ((symbol-function 'set-window-dedicated-p) (lambda (_w _v) nil))
                    ((symbol-function 'window-deletable-p) (lambda (_w) nil))
                    ((symbol-function 'window-buffer) (lambda (_w) nil))
                    ((symbol-function 'doom-fallback-buffer) (lambda () default))
                    ((symbol-function 'set-window-buffer) (lambda (_w b) (push b swapped))))
            (agent-repl--safe-delete-window 'win explicit)
            (should (equal swapped (list explicit))))
        (kill-buffer explicit)
        (kill-buffer default)))))

(ert-deftest agent-repl-test-panels-safe-delete-window-errors-without-fallback ()
  "safe-delete-window signals when a window is undeletable and no fallback exists."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'window-live-p) (lambda (_w) t))
              ((symbol-function 'set-window-parameter) (lambda (_w _p _v) nil))
              ((symbol-function 'set-window-dedicated-p) (lambda (_w _v) nil))
              ((symbol-function 'window-deletable-p) (lambda (_w) nil))
              ((symbol-function 'window-buffer) (lambda (_w) nil))
              ;; No fallback buffer available — must fail loudly, not no-op.
              ((symbol-function 'doom-fallback-buffer) (lambda () nil))
              ((symbol-function 'set-window-buffer) (lambda (_w _b) nil)))
      (should-error (agent-repl--safe-delete-window 'win)))))

(ert-deftest agent-repl-test-panels-safe-delete-window-noop-dead-window ()
  "safe-delete-window is a no-op on a dead window."
  (agent-repl-test--with-clean-state
    (let ((touched nil))
      (cl-letf (((symbol-function 'window-live-p) (lambda (_w) nil))
                ((symbol-function 'set-window-dedicated-p)
                 (lambda (_w _v) (setq touched t)))
                ((symbol-function 'delete-window) (lambda (_w) (setq touched t)))
                ((symbol-function 'set-window-buffer) (lambda (_w _b) (setq touched t))))
        (agent-repl--safe-delete-window 'dead)
        (should-not touched)))))

;;;; ---- Tests: reclaim-frame-fullscreen ----

(ert-deftest agent-repl-test-panels-reclaim-fullscreen-noop-no-buffers ()
  "reclaim-frame-fullscreen is a no-op when ws has no live panel buffers."
  (agent-repl-test--with-clean-state
    (let ((show-called nil))
      (cl-letf (((symbol-function 'agent-repl--show-panels)
                 (lambda () (setq show-called t))))
        (agent-repl--reclaim-frame-fullscreen "my-ws")
        (should-not show-called)))))

(ert-deftest agent-repl-test-panels-reclaim-fullscreen-gui-shows-webview ()
  "reclaim-frame-fullscreen reclaims a gui workspace through its frontend.
The vterm buffer-liveness path must not run — a gui workspace has no
vterm buffer, and the old vterm-only check silently skipped the
reclaim, stranding foreign leftovers in the frame."
  (agent-repl-test--with-clean-state
    (let ((dispatched nil)
          (webview (get-buffer-create "*agent-frontend-my-ws*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "my-ws" :frontend 'gui)
            (agent-repl--ws-put "my-ws" :frontend-buffer webview)
            (cl-letf (((symbol-function 'agent-repl--frontend-dispatch-show)
                       (lambda (ws) (setq dispatched ws)))
                      ((symbol-function 'agent-repl--show-panels)
                       (lambda () (error "vterm path must not run for a gui ws"))))
              (agent-repl--reclaim-frame-fullscreen "my-ws")
              (should (equal dispatched "my-ws"))))
        (kill-buffer webview)))))

(ert-deftest agent-repl-test-panels-reclaim-fullscreen-gui-dead-webview-skips ()
  "reclaim-frame-fullscreen skips a gui workspace whose webview is dead.
No view exists to reclaim the frame with, so the layout is left as-is
rather than booting a session as a side effect."
  (agent-repl-test--with-clean-state
    (let ((dispatched nil)
          (show-called nil))
      (agent-repl--ws-put "my-ws" :frontend 'gui)
      (cl-letf (((symbol-function 'agent-repl--frontend-dispatch-show)
                 (lambda (_ws) (setq dispatched t)))
                ((symbol-function 'agent-repl--show-panels)
                 (lambda () (setq show-called t))))
        (agent-repl--reclaim-frame-fullscreen "my-ws")
        (should-not dispatched)
        (should-not show-called)))))

(ert-deftest agent-repl-test-panels-reclaim-fullscreen-shows-panels ()
  "reclaim-frame-fullscreen shows own panels via show-panels when buffers live.
show-panels itself clears the main area and lays the panels out filling
the frame (fullscreen is the sole display format), so reclaim no longer
needs a separate enter-fullscreen step."
  (agent-repl-test--with-clean-state
    (let ((show-called nil)
          (vterm (get-buffer-create "*agent-panel-my-ws*"))
          (input (get-buffer-create "*agent-panel-input-my-ws*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "my-ws" :vterm-buffer vterm)
            (agent-repl--ws-put "my-ws" :input-buffer input)
            (cl-letf (((symbol-function 'agent-repl--show-panels)
                       (lambda () (setq show-called t))))
              (agent-repl--reclaim-frame-fullscreen "my-ws")
              (should show-called)))
        (kill-buffer vterm)
        (kill-buffer input)))))

;;;; ---- Tests: lone-output-window ----

(ert-deftest agent-repl-test-panels-lone-output-window-returns-sole-output ()
  "lone-output-window returns the sole non-side window showing an agent output buffer."
  (agent-repl-test--with-clean-state
    (let ((out (get-buffer-create "*agent-panel-my-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function 'window-list) (lambda (&rest _) '(w1)))
                    ((symbol-function 'agent-repl-window--side-window-p) (lambda (_w) nil))
                    ((symbol-function 'window-buffer) (lambda (_w) out)))
            (should (eq (agent-repl--lone-output-window) 'w1)))
        (kill-buffer out)))))

(ert-deftest agent-repl-test-panels-lone-output-window-nil-when-multiple ()
  "lone-output-window returns nil when more than one non-side window is present."
  (agent-repl-test--with-clean-state
    (let ((out (get-buffer-create "*agent-panel-my-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function 'window-list) (lambda (&rest _) '(w1 w2)))
                    ((symbol-function 'agent-repl-window--side-window-p) (lambda (_w) nil))
                    ((symbol-function 'window-buffer) (lambda (_w) out)))
            (should-not (agent-repl--lone-output-window)))
        (kill-buffer out)))))

(ert-deftest agent-repl-test-panels-lone-output-window-nil-non-agent ()
  "lone-output-window returns nil when the sole non-side window shows a non-agent buffer."
  (agent-repl-test--with-clean-state
    (let ((reg (get-buffer-create "*regular-buffer*")))
      (unwind-protect
          (cl-letf (((symbol-function 'window-list) (lambda (&rest _) '(w1)))
                    ((symbol-function 'agent-repl-window--side-window-p) (lambda (_w) nil))
                    ((symbol-function 'window-buffer) (lambda (_w) reg)))
            (should-not (agent-repl--lone-output-window)))
        (kill-buffer reg)))))

(ert-deftest agent-repl-test-panels-lone-output-window-nil-input-buffer ()
  "lone-output-window returns nil when the sole non-side window shows an agent input buffer."
  (agent-repl-test--with-clean-state
    (let ((inp (get-buffer-create "*agent-panel-input-my-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function 'window-list) (lambda (&rest _) '(w1)))
                    ((symbol-function 'agent-repl-window--side-window-p) (lambda (_w) nil))
                    ((symbol-function 'window-buffer) (lambda (_w) inp)))
            (should-not (agent-repl--lone-output-window)))
        (kill-buffer inp)))))

(ert-deftest agent-repl-test-panels-lone-output-window-ignores-side-windows ()
  "lone-output-window ignores side windows when finding the sole non-side output window."
  (agent-repl-test--with-clean-state
    (let ((out (get-buffer-create "*agent-panel-my-ws*"))
          (drawer (get-buffer-create "*drawer*")))
      (unwind-protect
          (cl-letf (((symbol-function 'window-list) (lambda (&rest _) '(side main)))
                    ((symbol-function 'agent-repl-window--side-window-p)
                     (lambda (w) (eq w 'side)))
                    ((symbol-function 'window-buffer)
                     (lambda (w) (if (eq w 'main) out drawer))))
            (should (eq (agent-repl--lone-output-window) 'main)))
        (kill-buffer out)
        (kill-buffer drawer)))))

;;;; ---- Tests: ensure-own reclaim/detach on foreign panels ----

(ert-deftest agent-repl-test-panels-ensure-own-detaches-foreign-buffers ()
  "ensure-own-panels-on-persp-switch detaches foreign panel buffers when stale present."
  (agent-repl-test--with-clean-state
    (let ((detached nil)
          (foreign (get-buffer-create "*agent-panel-other-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                    ((symbol-function 'agent-repl--stale-panel-windows)
                     (lambda () (list (selected-window))))
                    ((symbol-function 'agent-repl--stale-window-buffers)
                     (lambda (_w) (list foreign)))
                    ((symbol-function 'set-window-dedicated-p) (lambda (_w _v) nil))
                    ((symbol-function 'delete-window) (lambda (_w) nil))
                    ((symbol-function 'agent-repl--detach-foreign-panel-buffers)
                     (lambda (_ws bufs) (setq detached bufs)))
                    ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                    ((symbol-function 'agent-repl--ensure-input-beside-output) #'ignore)
                    ((symbol-function 'agent-repl--reclaim-frame-fullscreen) #'ignore))
            (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
            (should (equal detached (list foreign))))
        (kill-buffer foreign)))))

(ert-deftest agent-repl-test-panels-ensure-own-reclaims-fullscreen-when-stale ()
  "ensure-own-panels-on-persp-switch reclaims the frame in fullscreen when stale present."
  (agent-repl-test--with-clean-state
    (let ((reclaimed nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                ((symbol-function 'agent-repl--stale-panel-windows)
                 (lambda () (list (selected-window))))
                ((symbol-function 'agent-repl--stale-window-buffers) (lambda (_w) nil))
                ((symbol-function 'set-window-dedicated-p) (lambda (_w _v) nil))
                ((symbol-function 'delete-window) (lambda (_w) nil))
                ((symbol-function 'agent-repl--detach-foreign-panel-buffers) #'ignore)
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'agent-repl--ensure-input-beside-output) #'ignore)
                ((symbol-function 'agent-repl--reclaim-frame-fullscreen)
                 (lambda (ws) (setq reclaimed ws))))
        (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
        (should (equal reclaimed "my-ws"))))))

(ert-deftest agent-repl-test-panels-ensure-own-no-reclaim-when-no-stale ()
  "ensure-own-panels-on-persp-switch does not reclaim or detach when no stale panels and no lone output."
  (agent-repl-test--with-clean-state
    (let ((reclaimed nil)
          (detached nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                ((symbol-function 'agent-repl--stale-panel-windows) (lambda () nil))
                ((symbol-function 'agent-repl--lone-output-window) (lambda () nil))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'agent-repl--ensure-input-beside-output) #'ignore)
                ((symbol-function 'agent-repl--detach-foreign-panel-buffers)
                 (lambda (_ws _bufs) (setq detached t)))
                ((symbol-function 'agent-repl--reclaim-frame-fullscreen)
                 (lambda (_ws) (setq reclaimed t))))
        (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
        (should-not reclaimed)
        (should-not detached)))))

(ert-deftest agent-repl-test-panels-ensure-own-reclaims-fullscreen-when-lone-output ()
  "ensure-own-panels-on-persp-switch reclaims fullscreen for a lone output window with no stale panels."
  (agent-repl-test--with-clean-state
    (let ((reclaimed nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                ((symbol-function 'agent-repl--stale-panel-windows) (lambda () nil))
                ((symbol-function 'agent-repl--lone-output-window) (lambda () 'some-win))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'agent-repl--ensure-input-beside-output) #'ignore)
                ((symbol-function 'agent-repl--reclaim-frame-fullscreen)
                 (lambda (ws) (setq reclaimed ws))))
        (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
        (should (equal reclaimed "my-ws"))))))

(ert-deftest agent-repl-test-panels-ensure-own-reclaims-once-when-stale-and-lone-output ()
  "ensure-own-panels-on-persp-switch reclaims exactly once when both stale and lone output are present."
  (agent-repl-test--with-clean-state
    (let ((reclaim-count 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                ((symbol-function 'agent-repl--stale-panel-windows)
                 (lambda () (list (selected-window))))
                ((symbol-function 'agent-repl--stale-window-buffers) (lambda (_w) nil))
                ((symbol-function 'agent-repl--lone-output-window) (lambda () 'some-win))
                ((symbol-function 'set-window-dedicated-p) (lambda (_w _v) nil))
                ((symbol-function 'delete-window) (lambda (_w) nil))
                ((symbol-function 'agent-repl--detach-foreign-panel-buffers) #'ignore)
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'agent-repl--ensure-input-beside-output) #'ignore)
                ((symbol-function 'agent-repl--reclaim-frame-fullscreen)
                 (lambda (_ws) (setq reclaim-count (1+ reclaim-count)))))
        (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
        (should (= reclaim-count 1))))))

;;;; ---- Tests: before-persp-deactivate records panels-were-visible ----

(ert-deftest agent-repl-test-panels-before-persp-deactivate-records-visible ()
  "before-persp-deactivate saves :panels-were-visible t when panels are visible."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
              ((symbol-function 'agent-repl--redirect-from-agent-before-save) #'ignore)
              ((symbol-function 'agent-repl--ws-frame-save-state) #'ignore))
      (agent-repl--before-persp-deactivate)
      (should (eq (agent-repl--ws-get "ws1" :panels-were-visible) t)))))

(ert-deftest agent-repl-test-panels-before-persp-deactivate-records-hidden ()
  "before-persp-deactivate saves :panels-were-visible nil when panels are hidden."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
              ((symbol-function 'agent-repl--redirect-from-agent-before-save) #'ignore)
              ((symbol-function 'agent-repl--ws-frame-save-state) #'ignore))
      (agent-repl--before-persp-deactivate)
      (should-not (agent-repl--ws-get "ws1" :panels-were-visible)))))

;;;; ---- Tests: vterm frontend registration ----

(ert-deftest agent-repl-test-panels-vterm-declares-both-environments ()
  "The vterm frontend declares both environments: it is the only sandbox-capable one."
  ;; Act / Assert
  (should (equal (agent-repl-frontend-supported-envs (agent-repl-frontend-get 'vterm))
                 '(:bare-metal :sandbox))))

(ert-deftest agent-repl-test-panels-vterm-boot-threads-the-creation-hints ()
  "The vterm boot capability passes the project-dir and env hints to initialize-agent."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((got nil))
      (cl-letf (((symbol-function 'agent-repl--initialize-agent)
                 (lambda (ws &optional dir env) (setq got (list ws dir env)))))
        ;; Act
        (funcall (agent-repl-frontend-boot-fn (agent-repl-frontend-get 'vterm))
                 "ws1" "/tmp/wt" :sandbox)
        ;; Assert
        (should (equal got '("ws1" "/tmp/wt" :sandbox)))))))

;;; test-panels.el ends here
