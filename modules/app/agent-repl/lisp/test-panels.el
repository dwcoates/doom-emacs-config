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

;; persp-mode's hook, bound dynamically by the snapshot-count tests that
;; drive its deactivation protocol; persp-mode itself is not loaded here.
(defvar persp-before-deactivate-functions)

;;;; ---- Tests: Input panel height ----

(ert-deftest agent-repl-test-panels-input-height-fraction-default ()
  "The composer defaults to 18.4% of the frame's main area, leaving the webview the rest."
  (should (= (default-value 'agent-repl-input-height-fraction) 0.184)))

(ert-deftest agent-repl-test-panels-input-height-line-offset-default ()
  "The composer defaults to one line shorter than the fraction alone."
  (should (= (default-value 'agent-repl-input-height-line-offset) -1)))

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

(ert-deftest agent-repl-test-panels-view-visible-p-no-buffer ()
  "view-visible-p returns nil when no frontend (webview) buffer is set."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
      (should-not (agent-repl--view-visible-p)))))

(ert-deftest agent-repl-test-panels-panels-visible-p-both-nil ()
  "panels-visible-p returns nil when neither panel exists."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
      (should-not (agent-repl--panels-visible-p)))))

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

(ert-deftest agent-repl-test-panels-extract-id-from-frontend-buffer ()
  "extract-panel-id returns workspace identifier from a frontend webview buffer name."
  (should (equal (agent-repl--extract-panel-id "*agent-frontend-abcd1234*")
                 "abcd1234"))
  (should (equal (agent-repl--extract-panel-id "*agent-frontend-my-workspace*")
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

(ert-deftest agent-repl-test-panels-extract-id-from-titled-input ()
  "The identity segment ends at the space that introduces the display title."
  (should (equal (agent-repl--extract-panel-id
                  "*agent-panel-input-my-workspace Refactor the codec*")
                 "my-workspace")))

(ert-deftest agent-repl-test-panels-titled-input-buffer-is-an-agent-panel ()
  "A titled composer is still an agent panel to the name predicates."
  (should (agent-repl--agent-panel-buffer-p
           (get-buffer-create "*agent-panel-input-my-workspace Refactor the codec*"))))

(ert-deftest agent-repl-test-panels-partner-of-frontend-finds-the-titled-input ()
  "The partner input buffer's name is READ off the live buffer, not rebuilt."
  (agent-repl-test--with-temp-buffer "*agent-panel-input-abcd1234 A title*"
    (should (equal (agent-repl--partner-buffer-name "*agent-frontend-abcd1234*" "abcd1234")
                   "*agent-panel-input-abcd1234 A title*"))))

;;;; ---- Tests: Partner buffer name ----

(ert-deftest agent-repl-test-panels-partner-of-frontend-buffer ()
  "partner-buffer-name of a frontend webview buffer is the input buffer."
  (should (equal (agent-repl--partner-buffer-name "*agent-frontend-abcd1234*" "abcd1234")
                 "*agent-panel-input-abcd1234*")))

(ert-deftest agent-repl-test-panels-partner-of-input ()
  "partner-buffer-name of an input buffer is the frontend webview buffer."
  (should (equal (agent-repl--partner-buffer-name "*agent-panel-input-abcd1234*" "abcd1234")
                 "*agent-frontend-abcd1234*")))

;;;; ---- Tests: Orphaned panel detection (migrated) ----

(ert-deftest agent-repl-test-panels-orphaned-frontend-buffer-p ()
  "A frontend (webview) buffer whose input partner is not visible is orphaned."
  (agent-repl-test--with-clean-state
    ;; Mock: not one-window-p, no partner window visible
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window) (lambda (_buf) nil))
              ((symbol-function 'get-buffer) (lambda (_name) nil)))
      ;; Frontend buffer with no visible input partner is orphaned
      (should (agent-repl--orphaned-panel-p "*agent-frontend-abcd1234*"))
      ;; Non-agent buffers are never orphaned
      (should-not (agent-repl--orphaned-panel-p "*some-other*")))))

(ert-deftest agent-repl-test-panels-orphaned-input-p ()
  "An input buffer whose frontend partner is not visible is orphaned (no loading placeholder)."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window) (lambda (_buf) nil))
              ((symbol-function 'get-buffer) (lambda (_name) nil)))
      ;; Input with no visible frontend partner and no loading placeholder is orphaned
      (should (agent-repl--orphaned-panel-p "*agent-panel-input-abcd1234*"))
      ;; Non-agent buffers are never orphaned
      (should-not (agent-repl--orphaned-panel-p "*scratch*")))))

(ert-deftest agent-repl-test-panels-orphaned-frontend-buffer-one-window ()
  "When one-window-p returns t, no panel is considered orphaned."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () t)))
      (should-not (agent-repl--orphaned-panel-p "*agent-frontend-abcd1234*")))))

(ert-deftest agent-repl-test-panels-orphaned-input-with-loading ()
  "When loading placeholder buffer exists, input panel is not orphaned."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window) (lambda (_buf) nil))
              ((symbol-function 'get-buffer) (lambda (name)
                                               (when (equal name " *agent-loading*")
                                                 'fake-buffer))))
      (should-not (agent-repl--orphaned-panel-p "*agent-panel-input-abcd1234*")))))

(ert-deftest agent-repl-test-panels-orphaned-frontend-buffer-partner-visible ()
  "A frontend buffer whose input partner IS visible is not orphaned."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window)
               (lambda (buf)
                 ;; The input partner window is visible
                 (when (equal buf "*agent-panel-input-abcd1234*")
                   'fake-window))))
      (should-not (agent-repl--orphaned-panel-p "*agent-frontend-abcd1234*")))))

(ert-deftest agent-repl-test-panels-orphaned-input-partner-visible ()
  "An input buffer whose frontend (webview) partner IS visible is not orphaned."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window)
               (lambda (buf)
                 ;; The frontend partner window is visible
                 (when (equal buf "*agent-frontend-abcd1234*")
                   'fake-window)))
              ((symbol-function 'get-buffer) (lambda (_name) nil)))
      (should-not (agent-repl--orphaned-panel-p "*agent-panel-input-abcd1234*")))))

;;;; ---- Tests: own-panel-p ----

(ert-deftest agent-repl-test-panels-own-panel-p-current-ws ()
  "own-panel-p is non-nil for a panel buffer of the current workspace."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "cur")))
      (should (agent-repl--own-panel-p "*agent-panel-input-cur*"))
      (should (agent-repl--own-panel-p "*agent-frontend-cur*")))))

(ert-deftest agent-repl-test-panels-own-panel-p-other-ws ()
  "own-panel-p is nil for a panel buffer of a different workspace."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "cur")))
      (should-not (agent-repl--own-panel-p "*agent-panel-input-other*")))))

(ert-deftest agent-repl-test-panels-own-panel-p-non-panel ()
  "own-panel-p is nil for a non-panel buffer name."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "cur")))
      (should-not (agent-repl--own-panel-p "*scratch*")))))

(ert-deftest agent-repl-test-panels-own-panel-p-sanitized-name ()
  "own-panel-p sanitizes the current name before matching the embedded id."
  ;; Panel buffer names embed the sanitized ws name (\"my ws\" -> \"my_ws\"),
  ;; so a raw current name carrying unsafe chars must still match.
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my ws")))
      (should (agent-repl--own-panel-p "*agent-panel-input-my_ws*")))))

;;;; ---- Tests: sweepable-panel-p ----

(ert-deftest agent-repl-test-panels-sweepable-current-ws-not-sweepable ()
  "sweepable-panel-p is nil for the current workspace's own orphaned panel."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "cur"))
              ((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window) (lambda (_buf) nil))
              ((symbol-function 'get-buffer) (lambda (_name) nil)))
      ;; orphaned-panel-p reports this input panel orphaned (no visible
      ;; partner), but it belongs to the current ws, so the sweep must
      ;; leave it alone.
      (should (agent-repl--orphaned-panel-p "*agent-panel-input-cur*"))
      (should-not (agent-repl--sweepable-panel-p "*agent-panel-input-cur*")))))

(ert-deftest agent-repl-test-panels-sweepable-other-ws-sweepable ()
  "sweepable-panel-p is non-nil for another workspace's orphaned panel."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "cur"))
              ((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window) (lambda (_buf) nil))
              ((symbol-function 'get-buffer) (lambda (_name) nil)))
      (should (agent-repl--sweepable-panel-p "*agent-panel-input-other*")))))

(ert-deftest agent-repl-test-panels-sweepable-non-orphan-not-sweepable ()
  "sweepable-panel-p is nil when the panel is not orphaned."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "cur"))
              ((symbol-function 'one-window-p) (lambda () nil))
              ;; Partner webview window visible -> input panel not orphaned.
              ((symbol-function 'get-buffer-window)
               (lambda (buf) (when (equal buf "*agent-frontend-other*") 'fake-window)))
              ((symbol-function 'get-buffer) (lambda (_name) nil)))
      (should-not (agent-repl--sweepable-panel-p "*agent-panel-input-other*")))))

;;;; ---- Tests: sync-panels ----

(defun agent-repl-test--run-sync-panels (current-ws window-buffers)
  "Drive `agent-repl--sync-panels' over a mocked window layout.
CURRENT-WS is returned by `+workspace-current-name'.  WINDOW-BUFFERS is
an alist of (WINDOW-SYMBOL . BUFFER-NAME).  Builds the layout with real,
in-memory Emacs windows so native-compiled window primitives are never
rebound through `cl-letf' (which can trigger trampoline compilation before
the test subject runs).  Returns the labels of windows actually deleted."
  (let ((created '()))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((first t)
                (window-labels '()))
            (dolist (cell window-buffers)
              (let* ((name (cdr cell))
                     (existing (get-buffer name))
                     (buf (get-buffer-create name))
                     (win (if first
                              (selected-window)
                            (split-window (selected-window)))))
                (unless existing
                  (push buf created))
                (setq first nil)
                (set-window-buffer win buf)
                (push (cons win (car cell)) window-labels)))
            (cl-letf (((symbol-function '+workspace-current-name)
                       (lambda () current-ws)))
              (agent-repl--sync-panels)
              (cl-loop for (win . label) in window-labels
                       unless (window-live-p win)
                       collect label))))
      (dolist (buf created)
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest agent-repl-test-panels-sync-keeps-current-input-when-webview-absent ()
  "sync-panels does NOT sweep the current ws input panel mid-split.
Models the regression with the input window present before its webview
partner is observable, so the current workspace's input looks orphaned
but must survive."
  (agent-repl-test--with-clean-state
    (should-not
     (agent-repl-test--run-sync-panels
      "cur"
      '((win-input . "*agent-panel-input-cur*")
        (win-control . "*agent-sync-control-cur*"))))))

(ert-deftest agent-repl-test-panels-sync-sweeps-orphaned-other-ws-panel ()
  "sync-panels sweeps a genuinely orphaned panel from another workspace."
  (agent-repl-test--with-clean-state
    (should (equal
             (agent-repl-test--run-sync-panels
              "cur"
              '((win-keep . "*agent-sync-control-other*")
                ;; A foreign frontend has no input partner.  Using the
                ;; frontend side keeps the predicate independent of the
                ;; global loading-placeholder buffer used by other tests.
                (win-other . "*agent-frontend-other*")))
             '(win-other)))))

(ert-deftest agent-repl-test-panels-sync-leaves-settled-layout-intact ()
  "sync-panels leaves a settled current ws two-window layout untouched."
  (agent-repl-test--with-clean-state
    (should-not
     (agent-repl-test--run-sync-panels
      "cur"
      '((win-input . "*agent-panel-input-cur*")
        (win-webview . "*agent-frontend-cur*"))))))

;;;; ---- Tests: on-window-change ----

(ert-deftest agent-repl-test-panels-on-window-change-reconciles-after-sync ()
  "on-window-change sweeps other workspaces' orphans first, then
reconciles the current workspace's own layout."
  (agent-repl-test--with-clean-state
    (let ((calls '()))
      (cl-letf (((symbol-function 'agent-repl--sync-panels)
                 (lambda () (push 'sync calls)))
                ((symbol-function 'agent-repl-window--ensure-layout)
                 (lambda () (push 'ensure calls))))
        (agent-repl--on-window-change))
      (should (equal (nreverse calls) '(sync ensure))))))

(ert-deftest agent-repl-test-panels-on-window-change-defers-while-minibuffer-active ()
  "on-window-change skips layout reconciliation while a minibuffer is
active (e.g. the `SPC p p' picker): reconciling then would churn windows
under the open picker and sweep the undeletable minibuffer window.  The
minibuffer's eventual close re-fires the hook to reconcile the settled
layout."
  (agent-repl-test--with-clean-state
    (let ((calls '()))
      (cl-letf (((symbol-function 'active-minibuffer-window)
                 (lambda () 'a-minibuffer-window))
                ((symbol-function 'agent-repl--sync-panels)
                 (lambda () (push 'sync calls)))
                ((symbol-function 'agent-repl-window--ensure-layout)
                 (lambda () (push 'ensure calls))))
        (agent-repl--on-window-change))
      (should-not calls))))

(ert-deftest agent-repl-test-panels-on-window-change-binds-central-scope-without-workspace ()
  "A frame-wide window callback gives every nested record a central scope."
  (agent-repl-test--with-clean-state
    ;; Arrange.
    (let ((captured 'unset))
      (cl-letf (((symbol-function 'agent-repl--ws-current-log-name) (lambda () nil))
                ((symbol-function 'agent-repl--with-log-context)
                 (lambda (workspace _request-id function)
                   (setq captured workspace)
                   (funcall function)))
                ((symbol-function 'agent-repl--sync-panels) #'ignore)
                ((symbol-function 'agent-repl-window--ensure-layout) #'ignore))
        ;; Act.
        (agent-repl--on-window-change))
      ;; Assert.
      (should (eq captured agent-repl--global-log-scope)))))

;;;; ---- Tests: Defcustom defaults ----

;;;; ---- Tests: drain-pending-show-panels ----

(ert-deftest agent-repl-test-panels-drain-pending-when-set-and-ready ()
  "drain-pending-show-panels dispatches through WS's frontend and clears the flag."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :pending-show-panels t)
    (let ((shown nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-dispatch-show)
                 (lambda (ws) (setq shown ws))))
        (agent-repl--drain-pending-show-panels "test-ws")
        (should (equal shown "test-ws"))
        (should-not (agent-repl--ws-get "test-ws" :pending-show-panels))))))

(ert-deftest agent-repl-test-panels-drain-pending-shows-gui-frontend ()
  "drain-pending-show-panels shows a GUI workspace through the gui show capability."
  (agent-repl-test--with-clean-state
    ;; Arrange — a generated gui workspace: session booted headlessly, view
    ;; deferred to the first switch, which is where this drain runs.
    (agent-repl--ws-put "test-ws" :frontend 'gui)
    (agent-repl--ws-put "test-ws" :pending-show-panels t)
    (let ((shown nil))
      (cl-letf (((symbol-function 'agent-repl--gui-show)
                 (lambda (ws) (setq shown ws))))
        ;; Act
        (agent-repl--drain-pending-show-panels "test-ws")
        ;; Assert
        (should (equal shown "test-ws"))
        (should-not (agent-repl--ws-get "test-ws" :pending-show-panels))))))

(ert-deftest agent-repl-test-panels-drain-pending-when-not-set ()
  "drain-pending-show-panels does nothing when flag is nil."
  (agent-repl-test--with-clean-state
    (let ((called nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-dispatch-show)
                 (lambda (_ws) (setq called t))))
        (agent-repl--drain-pending-show-panels "test-ws")
        (should-not called)))))

;;;; ---- Tests: drain-pending-magit ----

(ert-deftest agent-repl-test-panels-drain-pending-magit-when-set ()
  "drain-pending-magit opens magit on :project-dir and clears the flag."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :pending-magit t)
    (agent-repl--ws-put "test-ws" :project-dir "/tmp/my-worktree")
    (let ((magit-path nil)
          (dash-called nil))
      (cl-letf (((symbol-function 'magit-status-setup-buffer)
                 (lambda (path) (setq magit-path path)))
                ((symbol-function 'agent-repl--remove-doom-dashboard)
                 (lambda () (setq dash-called t))))
        (agent-repl--drain-pending-magit "test-ws")
        (should (equal magit-path "/tmp/my-worktree/"))
        (should dash-called)
        (should-not (agent-repl--ws-get "test-ws" :pending-magit))))))

(ert-deftest agent-repl-test-panels-drain-pending-magit-when-not-set ()
  "drain-pending-magit does nothing when :pending-magit flag is nil."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :project-dir "/tmp/my-worktree")
    (let ((magit-called nil)
          (dash-called nil))
      (cl-letf (((symbol-function 'magit-status-setup-buffer)
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
      (cl-letf (((symbol-function 'magit-status-setup-buffer)
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
      (cl-letf (((symbol-function 'magit-status-setup-buffer)
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
          (cl-letf (((symbol-function 'magit-status-setup-buffer)
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
      (cl-letf (((symbol-function 'magit-status-setup-buffer)
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

;;;; ---- Tests: sigkill-if-alive ----

(ert-deftest agent-repl-test-panels-sigkill-if-alive-dead-process ()
  "sigkill-if-alive does nothing for a dead (nil) process."
  ;; Should not error when process is nil / not live
  (agent-repl--sigkill-if-alive nil))

;;;; ---- Tests: non-agent-panel-window-p ----

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
;; of the bookkeeping a close carries — no `:repl-state', so the sidebar
;; still read it as live, and on `SPC o C' no deprio and no session kill
;; either.

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
  "on-close accepts an explicit WS argument and closes THAT workspace's view."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((closed nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ignored"))
                ((symbol-function 'agent-repl--hide-panels) (lambda () nil))
                ((symbol-function 'agent-repl--close-view)
                 (lambda (ws _teardown) (setq closed ws))))
        ;; Act
        (agent-repl--on-close "specific-ws")
        ;; Assert
        (should (equal closed "specific-ws"))))))

(ert-deftest agent-repl-test-panels-on-close-writes-no-lifecycle-state ()
  "Closing the panels is a LOCAL presentation act and says nothing about
the workspace's lifecycle, which is the roster's."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "other-ws"))
              ((symbol-function 'agent-repl--hide-panels) (lambda () nil))
              ((symbol-function 'agent-repl--close-view) #'ignore))
      ;; Act
      (agent-repl--on-close "test-ws")
      ;; Assert
      (should-not (agent-repl--ws-get "test-ws" :repl-state)))))

(ert-deftest agent-repl-test-panels-on-simple-close-writes-no-lifecycle-state ()
  "The plain close writes no state either."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'agent-repl--hide-panels) (lambda () nil))
              ((symbol-function 'agent-repl--frontend-dispatch-hide) #'ignore))
      ;; Act
      (agent-repl--on-simple-close "test-ws")
      ;; Assert
      (should-not (agent-repl--ws-get "test-ws" :repl-state)))))

(ert-deftest agent-repl-test-panels-on-close-nil-ws-still-hides ()
  "on-close with nil workspace hides panels but skips bookkeeping."
  (agent-repl-test--with-clean-state
    (let ((hide-called nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
                ((symbol-function 'agent-repl--hide-panels)
                 (lambda () (setq hide-called t))))
        (agent-repl--on-close)
        (should hide-called)))))

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
                 (lambda (_cfg) (cl-incf restore-called))))
        ;; No :fullscreen-config set on test-ws.
        (agent-repl--on-simple-close)
        (should (= 0 restore-called))))))

(ert-deftest agent-repl-test-panels-on-simple-close-fullscreen-leaves-work-window ()
  "on-simple-close on a fullscreen ws removes panels and leaves the work window.
End-to-end with real windows: a fullscreen layout (only the two panels)
plus a saved splitscreen config restores to work+panels, then hides the
panels, leaving just the work window — the `SPC o c' goes-away contract."
  (agent-repl-test--with-clean-state
    (let ((wconf (current-window-configuration))
          (work-buf (generate-new-buffer "*fsclose-work*"))
          (frontend-buf (generate-new-buffer "*agent-frontend-fsclose*"))
          (input-buf (generate-new-buffer "*agent-panel-input-fsclose*")))
      (agent-repl--ws-put "test-ws" :frontend 'vterm)
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
            (delete-other-windows)
            (let* ((work-win (selected-window))
                   (view-win (split-window work-win nil 'right))
                   (input-win (split-window view-win nil 'below)))
              (set-window-buffer work-win work-buf)
              (set-window-buffer view-win frontend-buf)
              (set-window-buffer input-win input-buf)
              (agent-repl--ws-put "test-ws" :frontend-buffer frontend-buf)
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
              (should-not (get-buffer-window frontend-buf))
              (should-not (get-buffer-window input-buf))
              ;; The work window's buffer is back onscreen.
              (should (get-buffer-window work-buf))
              ;; Fullscreen config was cleared.
              (should-not (agent-repl--ws-get "test-ws" :fullscreen-config))))
        (set-window-configuration wconf)
        (kill-buffer work-buf)
        (kill-buffer frontend-buf)
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

(ert-deftest agent-repl-test-panels-restore-fullscreen-config-drops-a-stale-config ()
  "A `delete-other-windows' over the panels leaves NO stale fullscreen config.
The composer is delete-protected, so the sweep takes the webview window
and leaves the composer standing alone.  The landing-era
`:fullscreen-config' recorded for that mount now describes a frame that
is gone, so the close drops it (returning nil, no restore) instead of
putting the landing back over whatever the user does next."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((view-buf  (get-buffer-create "*agent-frontend-stale01*"))
          (input-buf (get-buffer-create "*agent-panel-input-stale01*"))
          (restored  nil))
      (unwind-protect
          (progn
            (delete-other-windows)
            (set-window-buffer (selected-window) view-buf)
            (let ((input-win (split-window (selected-window) nil 'below)))
              (set-window-buffer input-win input-buf)
              (agent-repl-window--harden input-win :delete-protect t)
              (agent-repl--ws-put "test-ws" :frontend-buffer view-buf)
              (agent-repl--ws-put "test-ws" :input-buffer input-buf)
              (agent-repl--ws-put "test-ws" :fullscreen-config
                                  (current-window-configuration))
              (select-window input-win)
              ;; Act — the ordinary binding, pressed from the composer,
              ;; and then the user builds a window of their own on the
              ;; frame the half-torn mount left behind.
              (delete-other-windows)
              (should-not (get-buffer-window view-buf))
              (set-window-buffer (split-window (selected-window) nil 'above)
                                 (get-buffer-create "*stale-user-work*"))
              (cl-letf (((symbol-function 'set-window-configuration)
                         (lambda (cfg) (setq restored cfg))))
                ;; Assert
                (should-not (agent-repl--restore-fullscreen-config "test-ws"))
                (should-not restored)
                (should-not (agent-repl--ws-get "test-ws" :fullscreen-config)))))
        (mapc (lambda (b) (when (buffer-live-p b) (kill-buffer b)))
              (list view-buf input-buf (get-buffer "*stale-user-work*")))
        (delete-other-windows)))))

(ert-deftest agent-repl-test-panels-restore-fullscreen-config-keeps-a-covered-frame ()
  "A frame holding only the mount's REMAINS keeps its saved config.
The window-change reconciler repairs exactly this state
\(`agent-repl-window--ensure-layout'): the composer survived alone and
nothing else has taken the frame, so the layout underneath is still the
one the configuration was recorded from and the close must restore it."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((view-buf  (get-buffer-create "*agent-frontend-covered1*"))
          (input-buf (get-buffer-create "*agent-panel-input-covered1*"))
          (restored  nil))
      (unwind-protect
          (progn
            (delete-other-windows)
            (set-window-buffer (selected-window) view-buf)
            (let ((input-win (split-window (selected-window) nil 'below)))
              (set-window-buffer input-win input-buf)
              (agent-repl-window--harden input-win :delete-protect t)
              (agent-repl--ws-put "test-ws" :frontend-buffer view-buf)
              (agent-repl--ws-put "test-ws" :input-buffer input-buf)
              (agent-repl--ws-put "test-ws" :fullscreen-config 'saved-config)
              (select-window input-win)
              ;; Act — the composer is left alone, and nothing else
              ;; comes onto the frame.
              (delete-other-windows)
              (should-not (get-buffer-window view-buf))
              (cl-letf (((symbol-function 'set-window-configuration)
                         (lambda (cfg) (setq restored cfg))))
                ;; Assert
                (should (agent-repl--restore-fullscreen-config "test-ws"))
                (should (eq restored 'saved-config)))))
        (mapc (lambda (b) (when (buffer-live-p b) (kill-buffer b)))
              (list view-buf input-buf))
        (delete-other-windows)))))

(ert-deftest agent-repl-test-panels-delete-other-windows-spares-the-composer ()
  "The composer's delete protection is unchanged by the staleness rule.
`delete-other-windows' invoked from the webview window must still leave
the hardened composer window alive — that protection is what the
`:fullscreen-config' staleness rule reads, and it is not relaxed."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((view-buf  (get-buffer-create "*agent-frontend-protect1*"))
          (input-buf (get-buffer-create "*agent-panel-input-protect1*")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (set-window-buffer (selected-window) view-buf)
            (let ((input-win (split-window (selected-window) nil 'below)))
              (set-window-buffer input-win input-buf)
              (agent-repl-window--harden input-win :delete-protect t)
              (select-window (get-buffer-window view-buf))
              ;; Act
              (delete-other-windows)
              ;; Assert
              (should (window-live-p input-win))
              (should (eq (window-buffer input-win) input-buf))))
        (mapc (lambda (b) (when (buffer-live-p b) (kill-buffer b)))
              (list view-buf input-buf))
        (delete-other-windows)))))

(ert-deftest agent-repl-test-panels-restore-fullscreen-config-noop-on-nil-ws ()
  "restore-fullscreen-config returns nil when WS is nil."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'set-window-configuration)
               (lambda (_cfg) (error "should not restore"))))
      (should-not (agent-repl--restore-fullscreen-config nil)))))

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
  "agent-repl-simple dispatches the visible-view case to simple-hide.
The webview must be both live and displayed for `agent-repl--toggle' to
reach the close branch (no `:always-close', so the running/open branches
are the alternative if the view isn't visible)."
  (agent-repl-test--with-clean-state
    (let ((frontend-buf (generate-new-buffer "*agent-frontend-test-ws*"))
          (simple-called 0)
          (full-called 0))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :frontend-buffer frontend-buf)
            (switch-to-buffer frontend-buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                      ((symbol-function 'use-region-p) (lambda () nil))
                      ((symbol-function 'agent-repl--simple-hide-and-preserve-status)
                       (lambda () (cl-incf simple-called)))
                      ((symbol-function 'agent-repl--hide-and-preserve-status)
                       (lambda () (cl-incf full-called))))
              (agent-repl-simple)
              (should (= 1 simple-called))
              (should (= 0 full-called))))
        (kill-buffer frontend-buf)))))

(ert-deftest agent-repl-test-panels-agent-repl-uses-full-hide ()
  "agent-repl (deprio variant) dispatches the visible-view case to hide-and-preserve."
  (agent-repl-test--with-clean-state
    (let ((frontend-buf (generate-new-buffer "*agent-frontend-test-ws*"))
          (simple-called 0)
          (full-called 0))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :frontend-buffer frontend-buf)
            (switch-to-buffer frontend-buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                      ((symbol-function 'use-region-p) (lambda () nil))
                      ((symbol-function 'agent-repl--simple-hide-and-preserve-status)
                       (lambda () (cl-incf simple-called)))
                      ((symbol-function 'agent-repl--hide-and-preserve-status)
                       (lambda () (cl-incf full-called))))
              (agent-repl)
              (should (= 0 simple-called))
              (should (= 1 full-called))))
        (kill-buffer frontend-buf)))))

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

;;;; ---- Tests: hide-and-preserve-status ----

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
                              :kill-fn #'ignore :running-p-fn #'ignore
                              :supported-backends '(claude)))))
        (agent-repl--hide-and-preserve-status)
        (should (equal on-close-ws "test-ws"))))))

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

(ert-deftest agent-repl-test-panels-entry-point-always-close-hides-unconditionally ()
  "agent-repl (SPC o C, always-close) always routes straight to the close
path without ever consulting the frontend's running/open capabilities —
pressing it hides the workspace whether or not an agent session exists."
  (agent-repl-test--with-clean-state
    (let ((opened nil) (hidden nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'agent-repl--gui-open) (lambda (&rest _) (setq opened t)))
                ((symbol-function 'agent-repl--hide-and-preserve-status)
                 (lambda () (setq hidden t))))
        (agent-repl)
        (should hidden)
        (should-not opened)))))

(ert-deftest agent-repl-test-panels-entry-point-selection-sends ()
  "agent-repl sends selected text to the agent when region is active.
Selection-handling stays orthogonal to the always-close hide path."
  (agent-repl-test--with-clean-state
    (let ((sent-text nil) (hidden nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'region-beginning) (lambda () 1))
                ((symbol-function 'region-end) (lambda () 12))
                ((symbol-function 'buffer-substring-no-properties)
                 (lambda (_beg _end) "hello world"))
                ((symbol-function 'deactivate-mark) (lambda () nil))
                ((symbol-function 'agent-repl--hide-and-preserve-status)
                 (lambda () (setq hidden t)))
                ((symbol-function 'agent-repl--send-to-agent)
                 (lambda (text origin)
                   (should (equal origin :user-sent))
                   (setq sent-text text))))
        (agent-repl)
        (should (equal sent-text "hello world"))
        (should-not hidden)))))

(ert-deftest agent-repl-test-panels-selection-origin-is-in-the-closed-vocabulary ()
  "`agent-repl--send' refuses an origin Emacs never spells, so this site's must be one.
The old string named a `PromptOrigin' that does not exist, which would
have been fatal at the first region send."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((origin nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'region-beginning) (lambda () 1))
                ((symbol-function 'region-end) (lambda () 12))
                ((symbol-function 'buffer-substring-no-properties)
                 (lambda (_beg _end) "hello world"))
                ((symbol-function 'deactivate-mark) (lambda () nil))
                ((symbol-function 'agent-repl--send-to-agent)
                 (lambda (_text o) (setq origin o))))
        ;; Act
        (agent-repl))
      ;; Assert
      (should (memq origin agent-repl--input-origins)))))

(ert-deftest agent-repl-test-panels-entry-point-simple-not-running-initializes ()
  "agent-repl-simple (SPC o c) keeps its non-always-close dispatch: when
nothing is running, it opens the agent through the workspace's frontend
\(in contrast to SPC o C, which always hides)."
  (agent-repl-test--with-clean-state
    (let ((opened nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'agent-repl-host-ref)
                 (lambda (_ws) '(:id "ws-1" :dir "/w/1")))
                ((symbol-function 'agent-repl-host-stream) (lambda (_ws) t))
                ((symbol-function 'agent-repl--open-progress-show)
                 (lambda (_ws buf) buf))
                ((symbol-function 'agent-repl--ws-frontend)
                 (lambda (_ws)
                   (agent-repl-frontend-create
                    :name 'probe
                    :open-fn (lambda (ws) (setq opened ws))
                    :kill-fn #'ignore
                    :running-p-fn (lambda (_ws) nil)
                    :supported-backends '(claude)))))
        (agent-repl-simple)
        (should (equal opened "test-ws"))))))

(ert-deftest agent-repl-test-panels-entry-point-simple-hidden-shows ()
  "agent-repl-simple (SPC o c) keeps its non-always-close dispatch: when
the session is running but its view isn't visible, it shows the view
through the workspace's frontend (in contrast to SPC o C, which always
hides)."
  (agent-repl-test--with-clean-state
    (let ((shown nil))
      ;; No :frontend-buffer set on test-ws, so the view is not visible.
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'agent-repl--ws-frontend)
                 (lambda (_ws)
                   (agent-repl-frontend-create
                    :name 'probe
                    :open-fn #'ignore :kill-fn #'ignore
                    :running-p-fn (lambda (_ws) t)
                    :show-fn (lambda (ws) (setq shown ws))
                    :supported-backends '(claude)))))
        (agent-repl-simple)
        (should (equal shown "test-ws"))))))

;;;; ---- Tests: entry-point open feedback ----

(defmacro agent-repl-test--with-open-toggle (running-p &rest body)
  "Run BODY with `agent-repl-simple' dispatching into recorded probes.
RUNNING-P is the frontend's `running-p-fn' verdict.  The recorded
dispatches land in `opened' and `shown'; placeholder display is stubbed
because these tests are about the DISPATCH, not the frame layout.

Both capabilities answer `:pending', the contract the real gui frontend
keeps: establishment is asynchronous and its outcome — including the
placeholder's teardown — arrives from a continuation."
  (declare (indent 1))
  `(agent-repl-test--with-clean-state
     (let ((opened nil) (shown nil))
       (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                 ((symbol-function 'use-region-p) (lambda () nil))
                 ;; The open path ensures the workspace's host subscription
                 ;; before mounting; a ref is what makes one possible.
                 ((symbol-function 'agent-repl-host-ref)
                  (lambda (_ws) '(:id "ws-1" :dir "/w/1")))
                 ((symbol-function 'agent-repl-host-stream) (lambda (_ws) t))
                 ((symbol-function 'agent-repl--open-progress-show)
                  (lambda (_ws buf) buf))
                 ((symbol-function 'agent-repl--ws-frontend)
                  (lambda (_ws)
                    (agent-repl-frontend-create
                     :name 'probe
                     :open-fn (lambda (ws) (setq opened ws) :pending)
                     :kill-fn #'ignore
                     :running-p-fn (lambda (_ws) ,running-p)
                     :show-fn (lambda (ws) (setq shown ws) :pending)
                     :supported-backends '(claude)))))
         ,@body))))

(ert-deftest agent-repl-test-panels-synchronous-open-settles-the-placeholder ()
  "A frontend that opens inside the call leaves no placeholder to escalate."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'use-region-p) (lambda () nil))
              ((symbol-function 'agent-repl-host-ref)
               (lambda (_ws) '(:id "ws-1" :dir "/w/1")))
              ((symbol-function 'agent-repl-host-stream) (lambda (_ws) t))
              ((symbol-function 'agent-repl--open-progress-show)
               (lambda (_ws buf) buf))
              ((symbol-function 'agent-repl--ws-frontend)
               (lambda (_ws)
                 (agent-repl-frontend-create
                  :name 'probe
                  ;; No `:pending': this capability finished here.
                  :open-fn (lambda (_ws) t)
                  :kill-fn #'ignore
                  :running-p-fn (lambda (_ws) nil)
                  :show-fn #'ignore
                  :supported-backends '(claude)))))
      ;; Act
      (agent-repl-simple)
      ;; Assert
      (should-not (agent-repl--open-progress-active-p "test-ws")))))

(ert-deftest agent-repl-test-panels-open-raises-a-placeholder ()
  "SPC o c on a stopped workspace leaves a visible placeholder behind."
  ;; Arrange / Act
  (agent-repl-test--with-open-toggle nil
    (agent-repl-simple)
    ;; Assert
    (should (agent-repl--open-progress-active-p "test-ws"))))

(ert-deftest agent-repl-test-panels-show-raises-a-placeholder ()
  "SPC o c on a running-but-hidden workspace also marks the wait."
  ;; Arrange / Act
  (agent-repl-test--with-open-toggle t
    (agent-repl-simple)
    ;; Assert
    (should (agent-repl--open-progress-active-p "test-ws"))))

(ert-deftest agent-repl-test-panels-second-open-dispatches-nothing ()
  "A second SPC o c while an open is pending does not re-dispatch it."
  ;; Arrange
  (agent-repl-test--with-open-toggle nil
    (agent-repl-simple)
    (setq opened nil)
    ;; Act
    (agent-repl-simple)
    ;; Assert
    (should-not opened)))

(ert-deftest agent-repl-test-panels-second-open-does-not-stack-a-placeholder ()
  "A second SPC o c while an open is pending reuses the standing placeholder."
  ;; Arrange
  (agent-repl-test--with-open-toggle nil
    (agent-repl-simple)
    (let ((buf (plist-get (agent-repl--open-progress-entry "test-ws") :buffer)))
      ;; Act
      (agent-repl-simple)
      ;; Assert
      (should (eq buf (plist-get (agent-repl--open-progress-entry "test-ws")
                                 :buffer))))))

(ert-deftest agent-repl-test-panels-resolved-open-dispatches-again ()
  "Once the pending open has resolved, SPC o c dispatches a fresh one."
  ;; Arrange
  (agent-repl-test--with-open-toggle nil
    (agent-repl-simple)
    (agent-repl--open-progress-finish "test-ws")
    (setq opened nil)
    ;; Act
    (agent-repl-simple)
    ;; Assert
    (should (equal opened "test-ws"))))

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

;;;; ---- Tests: view-visible-p with visible and dead buffer ----

(ert-deftest agent-repl-test-panels-view-visible-p-with-visible-buffer ()
  "view-visible-p returns non-nil when the frontend buffer is displayed in a window."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*test-frontend*"
      (agent-repl--ws-put "test-ws" :frontend-buffer (current-buffer))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'get-buffer-window)
                 (lambda (_buf &rest _) (selected-window))))
        ;; Mock get-buffer-window: batch mode has no real display
        (should (agent-repl--view-visible-p))))))

(ert-deftest agent-repl-test-panels-view-visible-p-dead-buffer ()
  "view-visible-p returns nil when the frontend buffer has been killed."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*test-dead-frontend*")))
      (agent-repl--ws-put "test-ws" :frontend-buffer buf)
      (kill-buffer buf)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
        (should-not (agent-repl--view-visible-p))))))

;;;; ---- Tests: panels-visible-p multi-window cases ----

(ert-deftest agent-repl-test-panels-panels-visible-p-only-input ()
  "panels-visible-p returns nil when only the input panel is visible."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*test-input-only*"
      (let ((input-buf (current-buffer)))
        (agent-repl--ws-put "test-ws" :input-buffer input-buf)
        ;; No :frontend-buffer set
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'get-buffer-window)
                   (lambda (buf &rest _) (and (eq buf input-buf) (selected-window)))))
          (should-not (agent-repl--panels-visible-p)))))))

(ert-deftest agent-repl-test-panels-panels-visible-p-only-view ()
  "panels-visible-p returns nil when only the agent view is visible."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*test-view-only*"
      (let ((frontend-buf (current-buffer)))
        (agent-repl--ws-put "test-ws" :frontend-buffer frontend-buf)
        ;; No :input-buffer set
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'get-buffer-window)
                   (lambda (buf &rest _) (and (eq buf frontend-buf) (selected-window)))))
          (should-not (agent-repl--panels-visible-p)))))))

(ert-deftest agent-repl-test-panels-panels-visible-p-view-alone-under-a-gate ()
  "While a gate hides the input, the view alone is the whole layout."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*test-view-gated*"
      (let ((frontend-buf (current-buffer)))
        (agent-repl--ws-put "test-ws" :frontend-buffer frontend-buf)
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl-host-state)
                   (lambda (_ws) '(:gate (:arm :cold-gate :value nil))))
                  ((symbol-function 'get-buffer-window)
                   (lambda (buf &rest _) (and (eq buf frontend-buf) (selected-window)))))
          (should (agent-repl--panels-visible-p)))))))

(ert-deftest agent-repl-test-panels-panels-visible-p-both-visible ()
  "panels-visible-p returns t when both the input panel and the agent view
are displayed in windows."
  (agent-repl-test--with-clean-state
    (let ((frontend-buf (get-buffer-create "*test-both-frontend*"))
          (input-buf (get-buffer-create "*test-both-input*"))
          (new-win nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :frontend-buffer frontend-buf)
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
              ;; Show the agent view in current window
              (switch-to-buffer frontend-buf)
              ;; Split and show input in new window
              (setq new-win (split-window))
              (set-window-buffer new-win input-buf)
              (should (agent-repl--panels-visible-p))))
        (when (and new-win (window-live-p new-win))
          (delete-window new-win))
        (when (buffer-live-p frontend-buf) (kill-buffer frontend-buf))
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
          (agent-repl--ws-put "test-ws" :project-dir temporary-file-directory)
          (let ((result (agent-repl--ensure-input-buffer "test-ws")))
            (should (buffer-live-p result))
            (should (eq result (agent-repl--ws-get "test-ws" :input-buffer)))
            (should (string= (buffer-name result) "*agent-panel-input-test-ws*"))))
      (when-let ((b (get-buffer "*agent-panel-input-test-ws*")))
        (kill-buffer b)))))

(ert-deftest agent-repl-test-panels-ensure-input-buffer-aligns-default-directory ()
  "ensure-input-buffer realigns the input buffer's `default-directory' to
the workspace :project-dir, even when the buffer was created against a
foreign directory."
  (agent-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*agent-panel-input-test-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
            (with-current-buffer input-buf
              (setq default-directory "/some/foreign/repo/"))
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (agent-repl--ws-put "test-ws" :project-dir "/home/user/project")
            (let ((result (agent-repl--ensure-input-buffer "test-ws")))
              (should (equal (buffer-local-value 'default-directory result)
                             "/home/user/project/"))))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))


;;;; ---- Tests: focus-input jump-back branch ----

(ert-deftest agent-repl-test-panels-focus-input-jump-back-selects-webview ()
  "focus-input from the composer jumps back to the workspace's webview window."
  (agent-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*focus-input-jump-back-input*"))
          (webview-buf (get-buffer-create "*focus-input-jump-back-webview*"))
          (input-win nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (agent-repl--ws-put "test-ws" :frontend-buffer webview-buf)
            ;; The panels are STACKED: the webview above, the composer
            ;; below, which is the layout the jump-back has to move in.
            (delete-other-windows)
            (set-window-buffer (selected-window) webview-buf)
            (setq input-win (split-window nil nil 'below))
            (set-window-buffer input-win input-buf)
            (select-window input-win)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
              (with-current-buffer input-buf
                (agent-repl-focus-input))
              (should (eq (window-buffer (selected-window)) webview-buf))))
        (when (and input-win (window-live-p input-win))
          (ignore-errors (delete-window input-win)))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))
        (when (buffer-live-p webview-buf) (kill-buffer webview-buf))))))

(ert-deftest agent-repl-test-panels-focus-input-jump-back-without-webview-window ()
  "focus-input from the composer does not signal when no webview window exists."
  (agent-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*focus-input-jump-back-alone-input*"))
          (webview-buf (get-buffer-create "*focus-input-jump-back-alone-webview*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (agent-repl--ws-put "test-ws" :frontend-buffer webview-buf)
            ;; The composer alone on the frame is exactly the state that
            ;; used to raise "No window left from selected window".
            (delete-other-windows)
            (set-window-buffer (selected-window) input-buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
              (with-current-buffer input-buf
                (agent-repl-focus-input))
              ;; Nowhere to go, so the composer keeps the selection.
              (should (eq (window-buffer (selected-window)) input-buf))))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))
        (when (buffer-live-p webview-buf) (kill-buffer webview-buf))))))

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

;;;; ---- Tests: on-workspace-switch ws nil ----

(ert-deftest agent-repl-test-panels-on-workspace-switch-nil-ws ()
  "on-workspace-switch does not error when workspace is nil."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
              ((symbol-function 'agent-repl--update-all-workspace-states-now) (lambda () nil))
              ((symbol-function 'agent-repl--drain-pending-magit) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--drain-pending-initial-buffers) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--drain-pending-show-panels) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--maybe-autoselect-input) (lambda (_ws) nil)))
      ;; Should not error -- the when guard skips mark-viewed
      (agent-repl--on-workspace-switch))))

(ert-deftest agent-repl-test-panels-on-workspace-switch-stamps-no-ack ()
  "Switching to a :done workspace records no viewed-acknowledgment.
The stamp existed only to start the removed decay's dwell countdown."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :agent-state :done)
    (should (null (agent-repl--ws-get "ws1" :done-acked-at)))))

(ert-deftest agent-repl-test-panels-clear-done-ack-helper-is-gone ()
  "The switch-away dwell reset went with the decay it paced."
  (should-not (fboundp 'agent-repl--clear-done-ack-on-switch-away)))

(ert-deftest agent-repl-test-panels-on-workspace-switch-explicit-ws-overrides-current ()
  "An explicit WS argument propagates to every per-ws side effect,
overriding `(+workspace-current-name)' at call time.  This is how
`--after-persp-activated' delivers the just-switched-to ws name to
the deferred call so back-to-back switches don't collapse onto the
latest one."
  (agent-repl-test--with-clean-state
    (let ((received-ws nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "racing-current"))
                ((symbol-function 'agent-repl--update-all-workspace-states-now) (lambda () nil))
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

;;;; ---- Tests: after-persp-activated eager-open suppression ----

(ert-deftest agent-repl-test-panels-after-persp-activated-schedules-switch ()
  "after-persp-activated schedules --on-workspace-switch for the active ws
when no eager-open is in progress."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (let ((scheduled nil)
          (agent-repl--eager-open-in-progress nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'run-at-time)
                 (lambda (_secs _rep fn &rest args) (setq scheduled (cons fn args)))))
        (agent-repl--after-persp-activated)
        (should (eq (car scheduled) #'agent-repl--run-switch-activation))
        (should (equal (cdr scheduled)
                       (list "ws1" agent-repl--switch-activation-generation)))))))

(ert-deftest agent-repl-test-panels-after-persp-activated-suppressed-during-eager-open ()
  "after-persp-activated does NOT schedule --on-workspace-switch while
`agent-repl--eager-open-in-progress' is set: the eager-open transient
switch builds panels synchronously, and a deferred pass firing after
focus returns to the caller would reclaim the caller's frame with the
background workspace's panels."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (let ((scheduled nil)
          (agent-repl--eager-open-in-progress t))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _) (setq scheduled t))))
        (agent-repl--after-persp-activated)
        (should-not scheduled)))))

(ert-deftest agent-repl-test-panels-after-persp-activated-skips-foreign-perspective ()
  "A perspective agent-repl never registered (a stale leftover from a
prior session's saved perspective list, restored by persp-mode at boot)
does not schedule `--on-workspace-switch' at all.  This is the source
fix for the boot-time `unroutable log workspace' warning: the deferred
switch never fires for a name that will never own a sink, instead of
firing and then warning about the fact."
  (agent-repl-test--with-clean-state
    (let ((scheduled nil)
          (agent-repl--eager-open-in-progress nil))
      (cl-letf (((symbol-function '+workspace-current-name)
                 (lambda () "agent-repl-stale-restored-ws"))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _) (setq scheduled t))))
        (agent-repl--after-persp-activated)
        (should-not scheduled)))))

(ert-deftest agent-repl-test-panels-after-persp-activated-schedules-for-preregistering-ws ()
  "A workspace mid-creation (its own registration window still open) still
schedules the switch: it is not foreign, merely not committed yet."
  (agent-repl-test--with-clean-state
    (let ((scheduled nil)
          (agent-repl--eager-open-in-progress nil)
          (agent-repl--log-preregistration-workspace "being-created"))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "being-created"))
                ((symbol-function 'run-at-time)
                 (lambda (_secs _rep fn &rest args) (setq scheduled (cons fn args)))))
        (agent-repl--after-persp-activated)
        (should (eq (car scheduled) #'agent-repl--run-switch-activation))))))

;;;; ---- Tests: switch activation generations ----

(defmacro agent-repl-test--with-switch-activations (&rest body)
  "Run BODY with fresh switch-activation state and a recording timer queue.
`run-at-time' records `(FN . ARGS)' onto `scheduled' (newest first) and
answers a real but never-armed timer, so cancellation is observable;
`agent-repl--on-workspace-switch' records the workspaces it ran for onto
`ran'."
  (declare (indent 0) (debug t))
  `(let ((agent-repl--switch-activation-generation 0)
         (agent-repl--switch-activation-timer nil)
         (agent-repl--eager-open-in-progress nil)
         (scheduled nil)
         (cancelled nil)
         (ran nil))
     (cl-letf (((symbol-function 'run-at-time)
                (lambda (_secs _rep fn &rest args)
                  (push (cons fn args) scheduled)
                  (timer-create)))
               ((symbol-function 'cancel-timer)
                (lambda (timer) (push timer cancelled)))
               ((symbol-function 'agent-repl--on-workspace-switch)
                (lambda (&optional ws) (push ws ran))))
       ,@body)))

(defun agent-repl-test--fire-scheduled (entry)
  "Run the scheduled ENTRY `(FN . ARGS)' as its timer would."
  (apply (car entry) (cdr entry)))

(ert-deftest agent-repl-test-panels-switch-activation-newest-runs ()
  "The pass carrying the newest generation runs the switch for its workspace."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-switch-activations
      ;; Arrange.
      (agent-repl--schedule-switch-activation "ws-b")
      ;; Act.
      (agent-repl-test--fire-scheduled (car scheduled))
      ;; Assert.
      (should (equal ran '("ws-b"))))))

(ert-deftest agent-repl-test-panels-switch-activation-superseded-does-nothing ()
  "A pass a later activation superseded runs nothing, even if its timer fires.
This is the stale zero-delay activation that repainted and focused the
PRIOR workspace under CPU load: the A-to-B pass firing after the user
had already gone on to C."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-switch-activations
      ;; Arrange.
      (agent-repl--schedule-switch-activation "ws-b")
      (agent-repl--schedule-switch-activation "ws-c")
      (let ((stale (cadr scheduled)))
        ;; Act.
        (agent-repl-test--fire-scheduled stale))
      ;; Assert.
      (should (null ran)))))

(ert-deftest agent-repl-test-panels-switch-activation-superseded-is-logged ()
  "A superseded pass records its generation and the newest at INFO."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-switch-activations
      ;; Arrange.
      (let ((records nil))
        (cl-letf (((symbol-function 'agent-repl--info)
                   (lambda (_ws fmt &rest args) (push (apply #'format fmt args) records))))
          (agent-repl--schedule-switch-activation "ws-b")
          (agent-repl--schedule-switch-activation "ws-c")
          ;; Act.
          (agent-repl-test--fire-scheduled (cadr scheduled)))
        ;; Assert.
        (should (member "elisp.panels.switch-activation-superseded ws=ws-b generation=1 newest=2"
                        records))))))

(ert-deftest agent-repl-test-panels-switch-activation-winner-is-logged ()
  "The pass that runs records the generation that won at INFO."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-switch-activations
      ;; Arrange.
      (let ((records nil))
        (cl-letf (((symbol-function 'agent-repl--info)
                   (lambda (_ws fmt &rest args) (push (apply #'format fmt args) records))))
          (agent-repl--schedule-switch-activation "ws-b")
          (agent-repl--schedule-switch-activation "ws-c")
          ;; Act.
          (agent-repl-test--fire-scheduled (car scheduled)))
        ;; Assert.
        (should (member "elisp.panels.switch-activation-won ws=ws-c generation=2" records))))))

(ert-deftest agent-repl-test-panels-switch-activation-cancels-the-pending-timer ()
  "Scheduling a newer pass cancels the pending one's timer."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-switch-activations
      ;; Arrange.
      (agent-repl--schedule-switch-activation "ws-b")
      (let ((pending agent-repl--switch-activation-timer))
        ;; Act.
        (agent-repl--schedule-switch-activation "ws-c")
        ;; Assert.
        (should (equal cancelled (list pending)))))))

(ert-deftest agent-repl-test-panels-switch-activation-clears-timer-when-run ()
  "The winning pass forgets its timer, so nothing cancels a fired timer later."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-switch-activations
      ;; Arrange.
      (agent-repl--schedule-switch-activation "ws-b")
      ;; Act.
      (agent-repl-test--fire-scheduled (car scheduled))
      ;; Assert.
      (should (null agent-repl--switch-activation-timer)))))

(ert-deftest agent-repl-test-panels-switch-activation-foreign-activation-supersedes ()
  "Activating a foreign perspective supersedes the pending pass.
The user left the workspace that pass is for, so it must not run."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-switch-activations
      ;; Arrange.
      (agent-repl--ws-put "ws-b" :project-dir "/tmp/ws-b")
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws-b")))
        (agent-repl--after-persp-activated))
      (cl-letf (((symbol-function '+workspace-current-name)
                 (lambda () "agent-repl-stale-restored-ws")))
        (agent-repl--after-persp-activated))
      ;; Act.
      (agent-repl-test--fire-scheduled (car scheduled))
      ;; Assert.
      (should (null ran)))))

(ert-deftest agent-repl-test-panels-switch-activation-eager-open-does-not-supersede ()
  "An eager-open's transient activation leaves the pending pass the newest.
The eager-open switches back to the caller's workspace, which is the one
the pending pass is for, so superseding it would leave that workspace's
switch never run."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-switch-activations
      ;; Arrange.
      (agent-repl--ws-put "ws-b" :project-dir "/tmp/ws-b")
      (agent-repl--ws-put "ws-bg" :project-dir "/tmp/ws-bg")
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws-b")))
        (agent-repl--after-persp-activated))
      (let ((agent-repl--eager-open-in-progress t))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws-bg")))
          (agent-repl--after-persp-activated)))
      ;; Act.
      (agent-repl-test--fire-scheduled (car scheduled))
      ;; Assert.
      (should (equal ran '("ws-b"))))))

;;;; ---- Tests: foreign-perspective-p ----

(ert-deftest agent-repl-test-panels-foreign-perspective-p-true-for-unregistered-name ()
  "A name with no hash entry at all, not mid-creation, is foreign."
  (agent-repl-test--with-clean-state
    (should (agent-repl--foreign-perspective-p "never-seen-ws"))))

(ert-deftest agent-repl-test-panels-foreign-perspective-p-false-for-known-ws ()
  "A workspace with any hash entry — live or tombstoned — is not foreign."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "known-ws" :project-dir "/tmp/known-ws")
    (should-not (agent-repl--foreign-perspective-p "known-ws"))))

(ert-deftest agent-repl-test-panels-foreign-perspective-p-false-for-preregistering-ws ()
  "A workspace inside its own declared creation window is not foreign."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--log-preregistration-workspace "being-created"))
      (should-not (agent-repl--foreign-perspective-p "being-created")))))

(ert-deftest agent-repl-test-panels-foreign-perspective-p-false-for-pseudo-workspace ()
  "persp-mode's own placeholder perspectives are not foreign — they are
classified separately and never reach this predicate as an anomaly."
  (agent-repl-test--with-clean-state
    (let ((persp-nil-name "none"))
      (should-not (agent-repl--foreign-perspective-p "none")))))

(ert-deftest agent-repl-test-panels-foreign-perspective-p-false-for-nil ()
  "nil is not a perspective name at all."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--foreign-perspective-p nil))))

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

(defmacro agent-repl-test-panels--with-visible-input (input-hidden &rest body)
  "Run BODY with test-ws's input buffer shown in a second window.
INPUT-HIDDEN is what the gate predicate `agent-repl-input-hidden-p'
answers.  BODY runs with the other window selected; `input-buf' is bound."
  (declare (indent 1))
  `(agent-repl-test--with-clean-state
     (let ((input-buf (get-buffer-create "*autoselect-gated*"))
           (new-win nil))
       (unwind-protect
           (cl-letf (((symbol-function 'agent-repl-input-hidden-p)
                      (lambda (_ws) ,input-hidden)))
             (agent-repl--ws-put "test-ws" :input-buffer input-buf)
             (setq new-win (split-window))
             (set-window-buffer new-win input-buf)
             (select-window (car (window-list)))
             (let ((agent-repl-autoselect-input-on-workspace-switch t))
               ,@body))
         (when (and new-win (window-live-p new-win))
           (ignore-errors (delete-window new-win)))
         (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

(ert-deftest agent-repl-test-panels-maybe-autoselect-input-noop-when-a-gate-hides-the-input ()
  "A gate hiding the input window leaves the landing in the webview."
  (agent-repl-test-panels--with-visible-input t
    (let ((orig-win (selected-window)))
      ;; Act
      (agent-repl--maybe-autoselect-input "test-ws")
      ;; Assert
      (should (eq (selected-window) orig-win)))))

(ert-deftest agent-repl-test-panels-maybe-autoselect-input-selects-when-no-gate-hides-it ()
  "With the gate predicate answering nil the input window is selected."
  (agent-repl-test-panels--with-visible-input nil
    ;; Act
    (agent-repl--maybe-autoselect-input "test-ws")
    ;; Assert
    (should (eq (window-buffer (selected-window)) input-buf))))

(ert-deftest agent-repl-test-panels-maybe-autoselect-input-records-the-gate ()
  "The gated landing is recorded at INFO."
  (agent-repl-test-panels--with-visible-input t
    (let ((infos nil))
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) infos))))
        ;; Act
        (agent-repl--maybe-autoselect-input "test-ws"))
      ;; Assert
      (should (member "maybe-autoselect-input: ws=test-ws branch=input-hidden" infos)))))

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

;;;; ---- Tests: non-agent-panel-window-p with agent buffers ----

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
      (agent-repl-test--with-temp-buffer "*hide-frontend*"
        (let ((frontend-buf (current-buffer)))
          (agent-repl-test--with-temp-buffer "*hide-input*"
            (let ((input-buf (current-buffer)))
              (agent-repl--ws-put "test-ws" :frontend-buffer frontend-buf)
              (agent-repl--ws-put "test-ws" :input-buffer input-buf)
              (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                        ((symbol-function 'agent-repl--close-buffer-windows)
                         (lambda (&rest bufs) (setq closed-bufs bufs))))
                (agent-repl--hide-panels)
                (should (equal closed-bufs (list input-buf frontend-buf)))))))))))

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
            (agent-repl--ws-put "test-ws" :project-dir temporary-file-directory)
            (agent-repl--initialize-input-buffer "test-ws")
            (should mode-called)
            (should history-called)
            (should (eq (agent-repl--ws-get "test-ws" :input-buffer) buf)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest agent-repl-test-initialize-input-buffer-draws-the-held-prompt-waiting-line ()
  "A composer born after an Emacs restart re-reads the held-prompt ingress."
  (agent-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *init-input-waiting*"))
          (refreshed nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--create-buffer)
                     (lambda (_ws &optional _s) buf))
                    ((symbol-function 'agent-repl-input-mode) #'ignore)
                    ((symbol-function 'agent-repl--history-restore) #'ignore)
                    ((symbol-function 'agent-repl-held-ingress-refresh)
                     (lambda (ws) (push ws refreshed))))
            ;; Arrange
            (agent-repl--ws-put "test-ws" :project-dir temporary-file-directory)
            ;; Act
            (agent-repl--initialize-input-buffer "test-ws")
            ;; Assert
            (should (equal refreshed '("test-ws"))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest agent-repl-test-initialize-input-buffer-carries-an-already-pushed-title ()
  "A title the daemon pushed BEFORE the composer existed still names it.
`agent-repl-host--apply-naming' runs on a push, so a buffer born after
the last one would otherwise wear the bare canonical name forever."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-host--by-name (make-hash-table :test 'equal))
          (buf (generate-new-buffer "*agent-panel-input-test-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--create-buffer)
                     (lambda (_ws &optional _s) buf))
                    ((symbol-function 'agent-repl-input-mode) #'ignore)
                    ((symbol-function 'agent-repl--history-restore) #'ignore))
            ;; Arrange
            (agent-repl--ws-put "test-ws" :project-dir temporary-file-directory)
            (agent-repl-host--put "test-ws" :host (list :naming (list :title "the title")))
            ;; Act
            (agent-repl--initialize-input-buffer "test-ws")
            ;; Assert
            (should (equal (buffer-name buf) "*agent-panel-input-test-ws the title*")))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest agent-repl-test-initialize-input-buffer-without-a-title-is-canonical ()
  "No naming pushed yet is the ordinary early state, not a failure."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-host--by-name (make-hash-table :test 'equal))
          (buf (generate-new-buffer "*agent-panel-input-test-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--create-buffer)
                     (lambda (_ws &optional _s) buf))
                    ((symbol-function 'agent-repl-input-mode) #'ignore)
                    ((symbol-function 'agent-repl--history-restore) #'ignore))
            ;; Arrange
            (agent-repl--ws-put "test-ws" :project-dir temporary-file-directory)
            ;; Act
            (agent-repl--initialize-input-buffer "test-ws")
            ;; Assert
            (should (equal (buffer-name buf) "*agent-panel-input-test-ws*")))
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
              (agent-repl--ws-put "test-ws" :project-dir temporary-file-directory)
              (should-error (agent-repl--initialize-input-buffer "test-ws"))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest agent-repl-test-initialize-input-buffer-requires-project-dir-before-mutation ()
  "Input creation fails before it records a composer without :project-dir."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
      (should-error (agent-repl--initialize-input-buffer "test-ws") :type 'error)
      (should-not (agent-repl--ws-get "test-ws" :input-buffer))
      (should-not (get-buffer "*agent-panel-input-test-ws*")))))

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
                                :running-p-fn #'ignore
                                :supported-backends '(claude)))))
        (agent-repl--hide-and-preserve-status)
        (should closed)
        (should killed)))))

(ert-deftest agent-repl-test-panels-on-close-never-kills ()
  "on-close itself must NOT kill: send-and-hide hides sessions that
keep running."
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
                                :running-p-fn #'ignore
                                :supported-backends '(claude)))))
        (agent-repl--on-close "ws1")
        (should-not killed)))))

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

(ert-deftest agent-repl-test-panels-kill-workspace-buffers/spares-a-buffer-the-landing-persp-holds ()
  "An unowned buffer another live persp also holds -- the landing workspace's
among them -- survives the teardown."
  ;; Arrange
  (let ((persp-mode t)
        (shared (get-buffer-create "*kwb-shared*")))
    (unwind-protect
        (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_ws) (list 'persp)))
                  ((symbol-function 'persp-buffers) (lambda (_p) (list shared)))
                  ((symbol-function 'persp-other-persps-with-buffer-except-nil)
                   (lambda (&rest _) (list 'keeper-persp))))
          ;; Act
          (agent-repl--kill-workspace-buffers "live-ws")
          ;; Assert
          (should (buffer-live-p shared)))
      (when (buffer-live-p shared) (kill-buffer shared)))))

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
  "kill-workspace-buffers kills a buffer owned by the workspace being killed."
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
  "kill-workspace-buffers cleans up a mocked process attached to a buffer."
  (let* ((persp-mode t)
         (buf (get-buffer-create "*kwb-proc*"))
         (proc 'kwb-fake-proc)
         (deleted-procs nil)
         (query-flags nil)
         (scheduled-procs nil))
    (unwind-protect
        (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_ws) (list 'persp)))
                  ((symbol-function 'persp-buffers) (lambda (_p) (list buf)))
                  ((symbol-function 'get-buffer-process)
                   (lambda (candidate) (and (eq candidate buf) proc)))
                  ((symbol-function 'process-name)
                   (lambda (candidate)
                     (if (eq candidate proc)
                         "kwb-fake-proc"
                       (error "Unexpected process double: %S" candidate))))
                  ((symbol-function 'delete-process)
                   (lambda (p) (push p deleted-procs)))
                  ((symbol-function 'set-process-query-on-exit-flag)
                   (lambda (p flag) (push (cons p flag) query-flags)))
                  ((symbol-function 'agent-repl--schedule-sigkill)
                   (lambda (p) (push p scheduled-procs))))
          (agent-repl--kill-workspace-buffers "proc-ws")
          (should (equal deleted-procs (list proc)))
          (should (equal query-flags (list (cons proc nil))))
          (should (equal scheduled-procs (list proc)))
          (should-not (buffer-live-p buf)))
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

;;;; ---- Tests: agent-repl-restart ----

(ert-deftest agent-repl-test-panels-restart-dispatches-through-frontend ()
  "agent-repl-restart is frontend-blind: it dispatches through whatever
restart-fn the workspace's frontend registers."
  (agent-repl-test--with-clean-state
    (let ((restarted nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--ws-frontend)
                 (lambda (_ws)
                   (agent-repl-frontend-create
                    :name 'probe :open-fn #'ignore :kill-fn #'ignore
                    :running-p-fn #'ignore
                    :restart-fn (lambda (ws) (setq restarted ws))
                    :supported-backends '(claude)))))
        (agent-repl-restart)
        (should (equal restarted "test-ws"))))))

;;;; ---- Tests: agent-repl-kill no workspace ----

(ert-deftest agent-repl-test-panels-kill-no-workspace ()
  "agent-repl-kill errors when no active workspace."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
      (should-error (agent-repl-kill)))))

(ert-deftest agent-repl-test-panels-kill-dispatches-through-frontend ()
  "agent-repl-kill is frontend-blind: it dispatches through whatever kill-fn
the workspace's frontend registers, which is what actually resets the
state axes."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :frontend-buffer nil)
    (let ((killed-ws nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--ws-frontend)
                 (lambda (_ws)
                   (agent-repl-frontend-create
                    :name 'probe :open-fn #'ignore
                    :kill-fn (lambda (ws)
                               (setq killed-ws ws)
                               (agent-repl--ws-put ws :agent-state nil)
                               (agent-repl--ws-put ws :repl-state nil))
                    :running-p-fn #'ignore
                    :supported-backends '(claude)))))
        (agent-repl-kill)
        (should (equal killed-ws "ws1"))
        (should-not (agent-repl--ws-get "ws1" :agent-state))
        (should-not (agent-repl--ws-get "ws1" :repl-state))))))

;;;; ---- Tests: redirect-from-agent-before-save with agent window ----

(ert-deftest agent-repl-test-panels-redirect-claude-to-other-window ()
  "redirect-from-agent-before-save selects a non-agent window when current is the agent."
  (agent-repl-test--with-clean-state
    (let ((agent-buf (get-buffer-create "*agent-frontend-abcd1234*"))
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
    (let ((agent-buf (get-buffer-create "*agent-frontend-abcd1234*")))
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
  "Redirect fires when selected window is a side window.

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
window after a kill."
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
t for a non-agent side window, so `cl-find-if' could pick the side
window as the redirect destination — defeating the purpose of the
redirect."
  (agent-repl-test--with-clean-state
    (delete-other-windows)
    (let* ((agent-buf (get-buffer-create "*agent-frontend-abcd1234*"))
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
    (let* ((agent-buf (get-buffer-create "*agent-frontend-abcd1234*"))
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
    (let ((agent-buf (get-buffer-create "*agent-frontend-abcd1234*")))
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
          (agent-buf (get-buffer-create "*agent-frontend-abcd1234*")))
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

(ert-deftest agent-repl-test-panels-fullscreen-and-focus-toggles-a-work-window ()
  "The non-agent fullscreen toggle still maximizes and then restores.
Nothing about the `:fullscreen-config' staleness rule touches
`agent-repl--window-fullscreen-config', the toggle's OWN saved layout:
one press leaves a single work window, the next brings the other window
back."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((work-buf  (get-buffer-create "*fullscreen-toggle-work*"))
          (other-buf (get-buffer-create "*fullscreen-toggle-other*"))
          (agent-repl--window-fullscreen-config nil))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
            (delete-other-windows)
            (set-window-buffer (selected-window) work-buf)
            (set-window-buffer (split-window-right) other-buf)
            (select-window (get-buffer-window work-buf))
            ;; Act — maximize.
            (agent-repl-fullscreen-and-focus)
            ;; Assert — only the work window is left, and the layout is held.
            (should-not (get-buffer-window other-buf))
            (should (get-buffer-window work-buf))
            (should agent-repl--window-fullscreen-config)
            ;; Act — toggle back.
            (agent-repl-fullscreen-and-focus)
            ;; Assert — the second window returns and the hold is released.
            (should (get-buffer-window other-buf))
            (should (get-buffer-window work-buf))
            (should-not agent-repl--window-fullscreen-config))
        (mapc (lambda (b) (when (buffer-live-p b) (kill-buffer b)))
              (list work-buf other-buf))
        (delete-other-windows)))))

(ert-deftest agent-repl-test-panels-fullscreen-and-focus-selects-input ()
  "fullscreen-and-focus selects the input window after toggling when in an agent buffer."
  (agent-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*agent-panel-input-abcd1234*"))
          (agent-buf (get-buffer-create "*agent-frontend-abcd1234*"))
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
          (agent-buf (get-buffer-create "*agent-frontend-abcd1234*"))
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
          (agent-buf (get-buffer-create "*agent-frontend-abcd1234*")))
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
    (let ((agent-buf (get-buffer-create "*agent-frontend-abcd1234*")))
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

(ert-deftest agent-repl-test-panels-fullscreen-and-focus-non-agent-preserves-side-window ()
  "fullscreen-and-focus does NOT delete side windows when maximizing a non-agent buffer."
  (agent-repl-test--with-clean-state
    (let* ((other-buf (get-buffer-create "*other-fs*"))
           (side-buf (get-buffer-create "*agent-side-fs*"))
           (agent-repl--window-fullscreen-config nil)
           (predicate-captured nil)
           (skip-captured nil))
      (switch-to-buffer other-buf)
      (unwind-protect
          (let ((fake-side-win (split-window-right)))
            (set-window-buffer fake-side-win side-buf)
            (set-window-parameter fake-side-win 'window-side 'left)
            (cl-letf (((symbol-function 'agent-repl-window--delete-where)
                       (lambda (pred &rest args)
                         (setq predicate-captured pred
                               skip-captured (plist-get args :skip-side-windows))
                         nil)))
              (agent-repl-fullscreen-and-focus)
              ;; The sweep must skip side windows by default (side window survives).
              (should (or (null skip-captured) (eq skip-captured t)))
              ;; Predicate keeps the selected (non-side) window and would
              ;; target the side window if side-windows were not skipped.
              (should (functionp predicate-captured))
              (should-not (funcall predicate-captured (selected-window)))
              (should (funcall predicate-captured fake-side-win))))
        (setq agent-repl--window-fullscreen-config nil)
        (when (buffer-live-p side-buf) (kill-buffer side-buf))
        (when (buffer-live-p other-buf) (kill-buffer other-buf))))))

(ert-deftest agent-repl-test-panels-fullscreen-and-focus-non-agent-real-side-window-survives ()
  "End-to-end: maximizing a non-agent buffer leaves a real side window alive."
  (agent-repl-test--with-clean-state
    (let* ((other-buf (get-buffer-create "*other-fs-real*"))
           (extra-buf (get-buffer-create "*extra-fs-real*"))
           (side-buf (get-buffer-create "*agent-side-fs-real*"))
           (agent-repl--window-fullscreen-config nil))
      (switch-to-buffer other-buf)
      (unwind-protect
          (let* ((extra-win (split-window-below))
                 (side-win (display-buffer-in-side-window
                            side-buf
                            '((side . left) (slot . 0)))))
            (set-window-buffer extra-win extra-buf)
            (should (window-live-p side-win))
            (agent-repl-fullscreen-and-focus)
            ;; The side window is still alive after fullscreen.
            (should (window-live-p side-win))
            ;; Extra non-side window was swept.
            (should-not (window-live-p extra-win)))
        (setq agent-repl--window-fullscreen-config nil)
        (dolist (buf (list side-buf extra-buf other-buf))
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
          (side-buf (get-buffer-create "*fs-leaf-side*")))
      (unwind-protect
          (progn
            (switch-to-buffer main-buf)
            (let* ((main-win (selected-window))
                   (extra-win (split-window-below)))
              (set-window-buffer extra-win extra-buf)
              (display-buffer-in-side-window side-buf '((side . left) (slot . 0)))
              (let* ((root (window-main-window))
                     (leaf (agent-repl--first-live-leaf root)))
                (should-not (window-live-p root))
                (should (window-live-p leaf))
                (should (memq leaf (list main-win extra-win))))))
        (dolist (buf (list side-buf extra-buf main-buf))
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

(ert-deftest agent-repl-test-panels-fullscreen-leave-side-window-from-side-window ()
  "`agent-repl--fullscreen-leave-side-window' selects the frame's main
window when invoked from a side window."
  (agent-repl-test--with-clean-state
    (let ((main-buf (get-buffer-create "*fs-leave-side-main*"))
          (side-buf (get-buffer-create "*fs-leave-side-buf*")))
      (unwind-protect
          (progn
            (switch-to-buffer main-buf)
            (let* ((main-win (selected-window))
                   (side-win (display-buffer-in-side-window
                              side-buf
                              '((side . left) (slot . 0)))))
              (select-window side-win)
              (should (agent-repl-window--side-window-p (selected-window)))
              (agent-repl--fullscreen-leave-side-window)
              (should-not (agent-repl-window--side-window-p (selected-window)))
              (should (eq (selected-window) main-win))))
        (dolist (buf (list side-buf main-buf))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

;;;; ---- Tests: fullscreen-and-focus side-window redirect ----

(ert-deftest agent-repl-test-panels-fullscreen-and-focus-from-side-window-preserves-main ()
  "When invoked from inside a side window with several main windows
visible, `agent-repl-fullscreen-and-focus' leaves the
originally-focused main window's siblings swept and the side window
alive — crucially, the originating main window survives instead of
being sacrificed because the side window was the `keep' anchor."
  (agent-repl-test--with-clean-state
    (let ((main-buf (get-buffer-create "*fs-from-side-main*"))
          (extra-buf (get-buffer-create "*fs-from-side-extra*"))
          (side-buf (get-buffer-create "*fs-from-side-window*"))
          (agent-repl--window-fullscreen-config nil))
      (unwind-protect
          (progn
            (switch-to-buffer main-buf)
            (let* ((main-win (selected-window))
                   (extra-win (split-window-below)))
              (set-window-buffer extra-win extra-buf)
              (let ((side-win (display-buffer-in-side-window
                               side-buf
                               '((side . left) (slot . 0)))))
                (select-window side-win)
                (agent-repl-fullscreen-and-focus)
                ;; The side window survives.
                (should (window-live-p side-win))
                ;; The originating main window survives — without the
                ;; side-window redirect it would be deleted because the
                ;; side window was `keep' and the predicate matches it.
                (should (window-live-p main-win))
                ;; The other main-area window is swept.
                (should-not (window-live-p extra-win)))))
        (setq agent-repl--window-fullscreen-config nil)
        (dolist (buf (list side-buf extra-buf main-buf))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest agent-repl-test-panels-fullscreen-and-focus-from-side-window-routes-to-claude-branch ()
  "When a side window is selected but the main window contains an agent
panel buffer, the side-window redirect lands on the agent buffer and
the function takes the agent branch (focus input, no non-agent maximize)."
  (agent-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create "*agent-frontend-fs-redir*"))
          (side-buf (get-buffer-create "*fs-redir-side*"))
          (agent-repl--window-fullscreen-config nil))
      (unwind-protect
          (progn
            (switch-to-buffer vterm-buf)
            (let* ((vterm-win (selected-window))
                   (side-win (display-buffer-in-side-window
                              side-buf
                              '((side . left) (slot . 0)))))
              (select-window side-win)
              (cl-letf (((symbol-function '+workspace-current-name)
                         (lambda () "test-ws")))
                (agent-repl-fullscreen-and-focus))
              ;; The redirect moved point onto the agent panel main window,
              ;; so the agent branch fired — the non-agent maximize branch
              ;; (which would save a window config) never ran.
              (should-not agent-repl--window-fullscreen-config)
              (should (window-live-p vterm-win))))
        (dolist (buf (list side-buf vterm-buf))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

;;;; ---- Tests: clear-main-area-for-panels (side-window preservation) ----

(ert-deftest agent-repl-test-panels-clear-main-area-preserves-side-windows ()
  "`--clear-main-area-for-panels' must NOT delete side windows.
Opening the agent routes through `--show-existing-panels' which clears
the main area; a side window must survive unconditionally, even when
its `no-delete-other-windows' parameter is absent (regression: opening
the agent used to destroy side windows)."
  (agent-repl-test--with-clean-state
    (let ((side-buf  (get-buffer-create "*clear-main-side*"))
          (work-buf  (get-buffer-create "*clear-main-work*"))
          (other-buf (get-buffer-create "*clear-main-other*")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (set-window-buffer (selected-window) work-buf)
            (let ((other-win (split-window-right)))
              (set-window-buffer other-win other-buf))
            ;; The side window has NO `no-delete-other-windows' —
            ;; the side-window-aware sweep must still preserve it.
            (let ((side-win (display-buffer-in-side-window
                             side-buf '((side . left) (slot . 0)))))
              (select-window (get-buffer-window work-buf))
              (agent-repl--clear-main-area-for-panels)
              (should (window-live-p side-win))
              (should (get-buffer-window side-buf))
              ;; The "other" main-area window should have been deleted.
              (should-not (get-buffer-window other-buf))))
        (mapc (lambda (b) (when (buffer-live-p b) (kill-buffer b)))
              (list side-buf work-buf other-buf))))))

;;;; ---- Tests: stale-panel-windows ----

(ert-deftest agent-repl-test-panels-stale-panel-windows-returns-foreign-panels ()
  "stale-panel-windows returns windows showing panels from a different workspace."
  (agent-repl-test--with-clean-state
    (let ((foreign-buf (get-buffer-create "*agent-frontend-other-ws*")))
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
    (let ((own-buf (get-buffer-create "*agent-frontend-my-ws*")))
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
  "ensure-own-panels-on-persp-switch re-shows panels when :panels-were-visible
is set, dispatching through WS's own frontend (the webview + input layout)."
  (agent-repl-test--with-clean-state
    (let ((shown-ws nil))
      (agent-repl--ws-put "my-ws" :panels-were-visible t)
      (let ((frontend-buf (get-buffer-create "*agent-frontend-my-ws*"))
            (input-buf (get-buffer-create "*agent-panel-input-my-ws*")))
        (unwind-protect
            (progn
              (agent-repl--ws-put "my-ws" :frontend-buffer frontend-buf)
              (agent-repl--ws-put "my-ws" :input-buffer input-buf)
              (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                        ((symbol-function 'agent-repl--stale-panel-windows) (lambda () nil))
                        ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                        ((symbol-function 'agent-repl--frontend-dispatch-show)
                         (lambda (ws) (setq shown-ws ws))))
                (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
                (should (equal shown-ws "my-ws"))))
          (kill-buffer frontend-buf)
          (kill-buffer input-buf))))))

(ert-deftest agent-repl-test-panels-ensure-own-records-restore-decision-re-show ()
  "The panel-restore decision is recorded at INFO: decision=re-show reason=default-open-now-missing."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((infos nil)
          (frontend-buf (get-buffer-create "*agent-frontend-my-ws*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "my-ws" :frontend-buffer frontend-buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                      ((symbol-function 'agent-repl--stale-panel-windows) (lambda () nil))
                      ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                      ((symbol-function 'agent-repl--frontend-dispatch-show) #'ignore)
                      ((symbol-function 'agent-repl--info)
                       (lambda (_ws fmt &rest args) (push (apply #'format fmt args) infos))))
              ;; Act
              (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
              ;; Assert
              (should (member "elisp.panels.restore-decision: ws=my-ws decision=re-show reason=default-open-now-missing" infos))))
        (kill-buffer frontend-buf)))))

(ert-deftest agent-repl-test-panels-ensure-own-records-restore-decision-already-visible ()
  "The panel-restore decision is recorded at INFO: decision=no-show reason=already-visible."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((infos nil)
          (frontend-buf (get-buffer-create "*agent-frontend-my-ws*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "my-ws" :frontend-buffer frontend-buf)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                      ((symbol-function 'agent-repl--stale-panel-windows) (lambda () nil))
                      ((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                      ((symbol-function 'agent-repl--frontend-dispatch-show) #'ignore)
                      ((symbol-function 'agent-repl--info)
                       (lambda (_ws fmt &rest args) (push (apply #'format fmt args) infos))))
              ;; Act
              (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
              ;; Assert
              (should (member "elisp.panels.restore-decision: ws=my-ws decision=no-show reason=already-visible" infos))))
        (kill-buffer frontend-buf)))))

(ert-deftest agent-repl-test-panels-ensure-own-records-restore-decision-closed-by-user ()
  "The panel-restore decision is recorded at INFO: decision=no-show reason=closed-by-user."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((infos nil)
          (frontend-buf (get-buffer-create "*agent-frontend-my-ws*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "my-ws" :frontend-buffer frontend-buf)
            (agent-repl--ws-put "my-ws" :panels-closed-by-user t)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                      ((symbol-function 'agent-repl--stale-panel-windows) (lambda () nil))
                      ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                      ((symbol-function 'agent-repl--frontend-dispatch-show) #'ignore)
                      ((symbol-function 'agent-repl--info)
                       (lambda (_ws fmt &rest args) (push (apply #'format fmt args) infos))))
              ;; Act
              (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
              ;; Assert
              (should (member "elisp.panels.restore-decision: ws=my-ws decision=no-show reason=closed-by-user" infos))))
        (kill-buffer frontend-buf)))))

(ert-deftest agent-repl-test-panels-ensure-own-records-restore-decision-no-live-view ()
  "The panel-restore decision is recorded at INFO: decision=no-show reason=no-live-view."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((infos nil)
          (frontend-buf (get-buffer-create "*agent-frontend-my-ws*")))
      (unwind-protect
          (progn
            nil
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                      ((symbol-function 'agent-repl--stale-panel-windows) (lambda () nil))
                      ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                      ((symbol-function 'agent-repl--frontend-dispatch-show) #'ignore)
                      ((symbol-function 'agent-repl--info)
                       (lambda (_ws fmt &rest args) (push (apply #'format fmt args) infos))))
              ;; Act
              (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
              ;; Assert
              (should (member "elisp.panels.restore-decision: ws=my-ws decision=no-show reason=no-live-view" infos))))
        (kill-buffer frontend-buf)))))

(ert-deftest agent-repl-test-panels-ensure-own-noop-when-panels-already-visible ()
  "ensure-own-panels-on-persp-switch does not re-show if panels are already visible."
  (agent-repl-test--with-clean-state
    (let ((shown-ws nil))
      (agent-repl--ws-put "my-ws" :panels-were-visible t)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                ((symbol-function 'agent-repl--stale-panel-windows) (lambda () nil))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'agent-repl--frontend-dispatch-show)
                 (lambda (ws) (setq shown-ws ws))))
        (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
        (should-not shown-ws)))))

(ert-deftest agent-repl-test-panels-ensure-own-noop-when-no-stale-no-flag ()
  "ensure-own-panels-on-persp-switch is a no-op with no stale panels and no flag."
  (agent-repl-test--with-clean-state
    (let ((shown-ws nil)
          (delete-called nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                ((symbol-function 'agent-repl--stale-panel-windows) (lambda () nil))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'agent-repl--frontend-dispatch-show)
                 (lambda (ws) (setq shown-ws ws)))
                ((symbol-function 'delete-window)
                 (lambda (_w) (setq delete-called t))))
        (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
        (should-not shown-ws)
        (should-not delete-called)))))

(ert-deftest agent-repl-test-panels-ensure-own-skips-restore-when-buffers-dead ()
  "ensure-own-panels-on-persp-switch does not re-show if panel buffers are dead."
  (agent-repl-test--with-clean-state
    (let ((shown-ws nil))
      (agent-repl--ws-put "my-ws" :panels-were-visible t)
      ;; Buffers are nil (dead) — should not try to show.
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                ((symbol-function 'agent-repl--stale-panel-windows) (lambda () nil))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'agent-repl--frontend-dispatch-show)
                 (lambda (ws) (setq shown-ws ws))))
        (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
        (should-not shown-ws)))))

(ert-deftest agent-repl-test-panels-ensure-own-restores-when-input-dead ()
  "ensure-own-panels-on-persp-switch re-shows when the view buffer is
live even though the input buffer is dead — the mount recreates the
input buffer (`agent-repl--ensure-input-buffer'), so only view
liveness gates the restore."
  (agent-repl-test--with-clean-state
    (let ((shown-ws nil))
      (agent-repl--ws-put "my-ws" :panels-were-visible t)
      (let ((frontend-buf (get-buffer-create "*agent-frontend-my-ws*"))
            (input-buf (get-buffer-create "*agent-panel-input-my-ws*")))
        (unwind-protect
            (progn
              (agent-repl--ws-put "my-ws" :frontend-buffer frontend-buf)
              (agent-repl--ws-put "my-ws" :input-buffer input-buf)
              (kill-buffer input-buf)
              (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                        ((symbol-function 'agent-repl--stale-panel-windows) (lambda () nil))
                        ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                        ((symbol-function 'agent-repl--frontend-dispatch-show)
                         (lambda (ws) (setq shown-ws ws))))
                (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
                (should (equal shown-ws "my-ws"))))
          (kill-buffer frontend-buf)
          (when (buffer-live-p input-buf) (kill-buffer input-buf)))))))

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
                ((symbol-function 'window-minibuffer-p) (lambda (_w) nil))
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
                    ((symbol-function 'window-minibuffer-p) (lambda (_w) nil))
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
                    ((symbol-function 'window-minibuffer-p) (lambda (_w) nil))
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
    (let ((shown nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-dispatch-show)
                 (lambda (_ws) (setq shown t))))
        (agent-repl--reclaim-frame-fullscreen "my-ws")
        (should-not shown)))))

(ert-deftest agent-repl-test-panels-reclaim-fullscreen-gui-shows-webview ()
  "reclaim-frame-fullscreen reclaims a gui workspace through its frontend
when the webview buffer is live."
  (agent-repl-test--with-clean-state
    (let ((dispatched nil)
          (webview (get-buffer-create "*agent-frontend-my-ws*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "my-ws" :frontend 'gui)
            (agent-repl--ws-put "my-ws" :frontend-buffer webview)
            (cl-letf (((symbol-function 'agent-repl--frontend-dispatch-show)
                       (lambda (ws) (setq dispatched ws))))
              (agent-repl--reclaim-frame-fullscreen "my-ws")
              (should (equal dispatched "my-ws"))))
        (kill-buffer webview)))))

(ert-deftest agent-repl-test-panels-reclaim-fullscreen-gui-dead-webview-skips ()
  "reclaim-frame-fullscreen skips a gui workspace whose webview is dead.
No view exists to reclaim the frame with, so the layout is left as-is
rather than booting a session as a side effect."
  (agent-repl-test--with-clean-state
    (let ((dispatched nil))
      (agent-repl--ws-put "my-ws" :frontend 'gui)
      (cl-letf (((symbol-function 'agent-repl--frontend-dispatch-show)
                 (lambda (_ws) (setq dispatched t))))
        (agent-repl--reclaim-frame-fullscreen "my-ws")
        (should-not dispatched)))))

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
                ((symbol-function 'agent-repl--reclaim-frame-fullscreen)
                 (lambda (ws) (setq reclaimed ws))))
        (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
        (should (equal reclaimed "my-ws"))))))

(ert-deftest agent-repl-test-panels-ensure-own-no-reclaim-when-no-stale ()
  "ensure-own-panels-on-persp-switch does not reclaim or detach when no stale panels are present."
  (agent-repl-test--with-clean-state
    (let ((reclaimed nil)
          (detached nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                ((symbol-function 'agent-repl--stale-panel-windows) (lambda () nil))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'agent-repl--detach-foreign-panel-buffers)
                 (lambda (_ws _bufs) (setq detached t)))
                ((symbol-function 'agent-repl--reclaim-frame-fullscreen)
                 (lambda (_ws) (setq reclaimed t))))
        (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
        (should-not reclaimed)
        (should-not detached)))))

;;;; ---- Tests: before-persp-deactivate records panels-were-visible ----

(ert-deftest agent-repl-test-panels-before-persp-deactivate-records-visible ()
  "before-persp-deactivate saves :panels-were-visible t when panels are visible."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
              ((symbol-function 'agent-repl--redirect-from-agent-before-save) #'ignore))
      (agent-repl--before-persp-deactivate)
      (should (eq (agent-repl--ws-get "ws1" :panels-were-visible) t)))))

(ert-deftest agent-repl-test-panels-before-persp-deactivate-records-hidden ()
  "before-persp-deactivate saves :panels-were-visible nil when panels are hidden."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
              ((symbol-function 'agent-repl--redirect-from-agent-before-save) #'ignore))
      (agent-repl--before-persp-deactivate)
      (should-not (agent-repl--ws-get "ws1" :panels-were-visible)))))

(ert-deftest agent-repl-test-panels-before-persp-deactivate-binds-central-scope-without-workspace ()
  "A workspace-free perspective callback gives nested records central scope."
  (agent-repl-test--with-clean-state
    ;; Arrange.
    (let ((captured 'unset))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
                ((symbol-function 'agent-repl--ws-current-log-name) (lambda () nil))
                ((symbol-function 'agent-repl--with-log-context)
                 (lambda (workspace _request-id function)
                   (setq captured workspace)
                   (funcall function)))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'agent-repl--redirect-from-agent-before-save) #'ignore))
        ;; Act.
        (agent-repl--before-persp-deactivate))
      ;; Assert.
      (should (eq captured agent-repl--global-log-scope)))))

;;;; ---- Tests: one departing-frame snapshot per switch ----

(ert-deftest agent-repl-test-panels-before-persp-deactivate-takes-no-snapshot ()
  "The deactivate hook leaves the frame snapshot to persp-mode alone."
  (agent-repl-test--with-clean-state
    ;; Arrange.
    (let ((snapshots 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'agent-repl--redirect-from-agent-before-save) #'ignore)
                ((symbol-function 'persp-frame-save-state)
                 (lambda (&rest _) (setq snapshots (1+ snapshots)))))
        ;; Act.
        (agent-repl--before-persp-deactivate))
      ;; Assert.
      (should (= snapshots 0)))))

(ert-deftest agent-repl-test-panels-switch-snapshots-departing-frame-once ()
  "A switch snapshots the departing frame exactly once.
Drives persp-mode's frame deactivation protocol (`persp--deactivate'):
the before-deactivate hooks, this module's included, then persp-mode's
own `persp-frame-save-state'."
  (agent-repl-test--with-clean-state
    ;; Arrange.
    (let ((snapshots 0)
          (persp-before-deactivate-functions
           (list #'agent-repl--before-persp-deactivate)))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'agent-repl--redirect-from-agent-before-save) #'ignore)
                ((symbol-function 'persp-frame-save-state)
                 (lambda (&rest _) (setq snapshots (1+ snapshots)))))
        ;; Act.
        (run-hook-with-args 'persp-before-deactivate-functions 'frame)
        (persp-frame-save-state (selected-frame) nil))
      ;; Assert.
      (should (= snapshots 1)))))

(ert-deftest agent-repl-test-panels-before-persp-deactivate-redirects-before-the-snapshot ()
  "The redirect lands before persp-mode snapshots the frame.
The snapshot follows the hooks, so the redirect is what it records."
  (agent-repl-test--with-clean-state
    ;; Arrange.
    (let ((order nil)
          (persp-before-deactivate-functions
           (list #'agent-repl--before-persp-deactivate)))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'agent-repl--redirect-from-agent-before-save)
                 (lambda () (push :redirect order)))
                ((symbol-function 'persp-frame-save-state)
                 (lambda (&rest _) (push :snapshot order))))
        ;; Act.
        (run-hook-with-args 'persp-before-deactivate-functions 'frame)
        (persp-frame-save-state (selected-frame) nil))
      ;; Assert.
      (should (equal (nreverse order) '(:redirect :snapshot))))))

;;;; ---- Tests: before-persp-deactivate log routing ----

(ert-deftest agent-repl-test-panels-before-persp-deactivate-placeholder-logs-globally ()
  "Deactivating a persp PLACEHOLDER logs against the global sink.
The hook fires for persp-mode's own perspectives too (`persp-nil-name'
\"none\", Doom's initial \"main\").  Those are not agent-repl workspaces and
own no durable sink, so the record is global by design."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((logged 'no-record))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "none"))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'agent-repl--redirect-from-agent-before-save) #'ignore)
                ((symbol-function 'agent-repl--log)
                 (lambda (ws &rest _)
                   (when (eq logged 'no-record) (setq logged ws)))))
        ;; Act
        (agent-repl--before-persp-deactivate))
      ;; Assert
      (should (null logged)))))

(ert-deftest agent-repl-test-panels-before-persp-deactivate-routable-ws-keeps-attribution ()
  "A REAL workspace's deactivation record still routes to that workspace.
Screening the log name must not demote records that legitimately own a sink."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((project (make-temp-file "agent-repl-deactivate-route-" t))
          (logged 'no-record))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir project)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                      ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                      ((symbol-function 'agent-repl--redirect-from-agent-before-save) #'ignore)
                      ((symbol-function 'agent-repl--log)
                       (lambda (ws &rest _)
                         (when (eq logged 'no-record) (setq logged ws)))))
              ;; Act
              (agent-repl--before-persp-deactivate)))
        (delete-directory project t))
      ;; Assert
      (should (equal logged "ws1")))))

;;;; ---- Tests: on-workspace-switch log routing ----

(ert-deftest agent-repl-test-panels-on-workspace-switch-placeholder-logs-globally ()
  "Activating a persp PLACEHOLDER emits no workspace-attributed record.
`--after-persp-activated' hands this path whatever perspective persp-mode
activated, including its own \"none\" and Doom's initial \"main\".  Neither
owns a `:project-dir', so a record attributed to one is unroutable and
`--note-unroutable-log-workspace' warned about it once per boot."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((unroutable nil))
      (cl-letf* ((collect (lambda (ws &rest _)
                            (unless (or (null ws)
                                        (eq ws agent-repl--global-log-scope)
                                        (agent-repl--central-log-scope-reason ws)
                                        (agent-repl--context-log-scope-reason ws)
                                        (agent-repl--ws-log-routable-p ws))
                              (push ws unroutable))))
                 ((symbol-function '+workspace-current-name) (lambda () "main"))
                 ((symbol-function 'agent-repl--force-tab-bar-redraw) #'ignore)
                 ((symbol-function 'agent-repl--log) collect)
                 ((symbol-function 'agent-repl--log-verbose) collect)
                 ((symbol-function 'agent-repl--info) collect)
                 ((symbol-function 'agent-repl--warn) collect))
        ;; Act
        (agent-repl--on-workspace-switch "main"))
      ;; Assert
      (should-not unroutable))))

(ert-deftest agent-repl-test-panels-on-workspace-switch-placeholder-binds-central-scope ()
  "A pseudo-perspective switch gives every nested record a central scope."
  (agent-repl-test--with-clean-state
    ;; Arrange.
    (let ((captured 'unset))
      (cl-letf (((symbol-function 'agent-repl--with-log-context)
                 (lambda (workspace _request-id function)
                   (setq captured workspace)
                   (funcall function)))
                ((symbol-function 'agent-repl--force-tab-bar-redraw) #'ignore))
        ;; Act.
        (agent-repl--on-workspace-switch "main"))
      ;; Assert.
      (should (eq captured agent-repl--global-log-scope)))))

(ert-deftest agent-repl-test-panels-on-workspace-switch-routable-ws-keeps-attribution ()
  "Activating a REAL workspace still attributes its records to that workspace.
Screening the log name must not demote records that legitimately own a sink."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((project (make-temp-file "agent-repl-switch-route-" t))
          (logged 'no-record))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir project)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                      ((symbol-function 'agent-repl--force-tab-bar-redraw) #'ignore)
                      ((symbol-function 'agent-repl--log)
                       (lambda (ws &rest _)
                         (when (eq logged 'no-record) (setq logged ws)))))
              ;; Act
              (agent-repl--on-workspace-switch "ws1")))
        (delete-directory project t))
      ;; Assert
      (should (equal logged "ws1")))))

;;; test-panels.el ends here

;;;; ---- Tests: the panels entry ensures the host subscription ----

(ert-deftest agent-repl-test-panels-entry-subscribes-the-host-stream ()
  "Opening the panels is Emacs saying it is looking at this workspace, and
the host stream is what it looks at it through."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((subscribed nil))
      (cl-letf (((symbol-function 'agent-repl-host-ref)
                 (lambda (_ws) '(:id "ws-1" :dir "/w/1")))
                ((symbol-function 'agent-repl-host-stream) (lambda (_ws) nil))
                ((symbol-function 'agent-repl-host-conn) (lambda (_ws) 'conn))
                ((symbol-function 'agent-repl-host-subscribe)
                 (lambda (_conn ws _ref) (setq subscribed ws))))
        ;; Act
        (agent-repl--panels-ensure-host-subscription "test-ws")
        ;; Assert
        (should (equal subscribed "test-ws"))))))

(ert-deftest agent-repl-test-panels-entry-does-not-resubscribe ()
  "host.el keeps ONE subscription per open workspace; re-entering re-uses it."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((subscribed nil))
      (cl-letf (((symbol-function 'agent-repl-host-ref)
                 (lambda (_ws) '(:id "ws-1" :dir "/w/1")))
                ((symbol-function 'agent-repl-host-stream) (lambda (_ws) 'stream))
                ((symbol-function 'agent-repl-host-conn) (lambda (_ws) 'conn))
                ((symbol-function 'agent-repl-host-subscribe)
                 (lambda (_conn ws _ref) (setq subscribed ws))))
        ;; Act
        (agent-repl--panels-ensure-host-subscription "test-ws")
        ;; Assert
        (should (null subscribed))))))

(ert-deftest agent-repl-test-panels-entry-without-a-ref-subscribes-nothing ()
  "No ref means no identity to subscribe with — the roster brings one."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((subscribed nil))
      (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (_ws) nil))
                ((symbol-function 'agent-repl-host-subscribe)
                 (lambda (_conn ws _ref) (setq subscribed ws))))
        ;; Act
        (should (null (agent-repl--panels-ensure-host-subscription "test-ws")))
        ;; Assert
        (should (null subscribed))))))

(ert-deftest agent-repl-test-panels-entry-without-a-link-subscribes-nothing ()
  "No connection means no stream to open; the link-up edge will bring one."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((subscribed nil))
      (cl-letf (((symbol-function 'agent-repl-host-ref)
                 (lambda (_ws) '(:id "ws-1" :dir "/w/1")))
                ((symbol-function 'agent-repl-host-stream) (lambda (_ws) nil))
                ((symbol-function 'agent-repl-host-conn) (lambda (_ws) nil))
                ((symbol-function 'agent-repl-link-primary) (lambda () nil))
                ((symbol-function 'agent-repl-host-subscribe)
                 (lambda (_conn ws _ref) (setq subscribed ws))))
        ;; Act
        (should (null (agent-repl--panels-ensure-host-subscription "test-ws")))
        ;; Assert
        (should (null subscribed))))))

(ert-deftest agent-repl-test-panels-open-without-a-ref-shows-the-input-only ()
  "A workspace with no ref yet gets its composer, not a refusal: the user
asked for the panels, and the half that does not need an identity is
available now."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((opened nil) (input nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'agent-repl-host-ref) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--open-progress-show)
                 (lambda (_ws buf) buf))
                ((symbol-function 'agent-repl--ensure-input-buffer)
                 (lambda (ws) (setq input ws)))
                ((symbol-function 'agent-repl--ws-frontend)
                 (lambda (_ws)
                   (agent-repl-frontend-create
                    :name 'probe
                    :open-fn (lambda (ws) (setq opened ws) :pending)
                    :kill-fn #'ignore
                    :running-p-fn (lambda (_ws) nil)
                    :supported-backends '(claude)))))
        ;; Act
        (agent-repl-simple)
        ;; Assert
        (should (and (equal input "test-ws") (null opened)))))))

;;;; ---- Tests: workspace-push-to-back (the deprio shuffle) ----
;;
;; The bug these close: `SPC o C' called `agent-repl-workspace-push-to-back'
;; and nothing defined it — the shuffle existed only as a `cl-letf' stub in
;; this file, so every keypress signaled a void function.

(ert-deftest agent-repl-test-panels-push-to-back-is-a-command ()
  "push-to-back is interactively invokable."
  ;; Arrange / Act / Assert
  (should (commandp 'agent-repl-workspace-push-to-back)))

(ert-deftest agent-repl-test-panels-push-to-back-moves-ws-last ()
  "push-to-back puts the current workspace in the LAST roster tab slot."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((agent-repl-roster--tab-order '("a" "b" "c")))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "a"))
                ((symbol-function 'agent-repl--ws-update-names-cache) #'ignore)
                ((symbol-function 'agent-repl--force-tab-bar-redraw) #'ignore)
                ((symbol-function 'agent-repl--ws-switch) #'ignore))
        ;; Act
        (agent-repl-workspace-push-to-back)
        ;; Assert
        (should (equal agent-repl-roster--tab-order '("b" "c" "a")))))))

(ert-deftest agent-repl-test-panels-push-to-back-syncs-the-names-cache ()
  "push-to-back hands persp-mode the SAME order it wrote to the roster."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((agent-repl-roster--tab-order '("a" "b" "c"))
          (cached 'unset))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "a"))
                ((symbol-function 'agent-repl--ws-update-names-cache)
                 (lambda (names) (setq cached names)))
                ((symbol-function 'agent-repl--force-tab-bar-redraw) #'ignore)
                ((symbol-function 'agent-repl--ws-switch) #'ignore))
        ;; Act
        (agent-repl-workspace-push-to-back)
        ;; Assert
        (should (equal cached '("b" "c" "a")))))))

(ert-deftest agent-repl-test-panels-push-to-back-switches-to-the-vacated-slot ()
  "Without KEEP-FOCUS, focus lands on whoever now holds the vacated slot."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((agent-repl-roster--tab-order '("a" "b" "c"))
          (switched 'unset))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "a"))
                ((symbol-function 'agent-repl--ws-update-names-cache) #'ignore)
                ((symbol-function 'agent-repl--force-tab-bar-redraw) #'ignore)
                ((symbol-function 'agent-repl--ws-switch)
                 (lambda (ws &rest _) (setq switched ws))))
        ;; Act
        (agent-repl-workspace-push-to-back)
        ;; Assert
        (should (equal switched "b"))))))

(ert-deftest agent-repl-test-panels-push-to-back-keep-focus-stays-put ()
  "With KEEP-FOCUS the moved workspace keeps focus — no switch at all."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((agent-repl-roster--tab-order '("a" "b" "c"))
          (switch-calls 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "a"))
                ((symbol-function 'agent-repl--ws-update-names-cache) #'ignore)
                ((symbol-function 'agent-repl--force-tab-bar-redraw) #'ignore)
                ((symbol-function 'agent-repl--ws-switch)
                 (lambda (&rest _) (cl-incf switch-calls))))
        ;; Act
        (agent-repl-workspace-push-to-back t)
        ;; Assert
        (should (= 0 switch-calls))))))

(ert-deftest agent-repl-test-panels-push-to-back-last-slot-is-idempotent ()
  "A workspace already last stays last and keeps focus where it is."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((agent-repl-roster--tab-order '("a" "b" "c"))
          (switched 'unset))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "c"))
                ((symbol-function 'agent-repl--ws-update-names-cache) #'ignore)
                ((symbol-function 'agent-repl--force-tab-bar-redraw) #'ignore)
                ((symbol-function 'agent-repl--ws-switch)
                 (lambda (ws &rest _) (setq switched ws))))
        ;; Act
        (agent-repl-workspace-push-to-back)
        ;; Assert
        (should (equal agent-repl-roster--tab-order '("a" "b" "c")))))))

(ert-deftest agent-repl-test-panels-push-to-back-without-a-tab-is-a-no-op ()
  "A workspace holding no tab has no slot to vacate: the order is untouched."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((agent-repl-roster--tab-order '("a" "b"))
          (cache-calls 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "tabless"))
                ((symbol-function 'agent-repl--ws-update-names-cache)
                 (lambda (_names) (cl-incf cache-calls)))
                ((symbol-function 'agent-repl--force-tab-bar-redraw) #'ignore)
                ((symbol-function 'agent-repl--ws-switch) #'ignore))
        ;; Act
        (agent-repl-workspace-push-to-back)
        ;; Assert
        (should (= 0 cache-calls))))))

(ert-deftest agent-repl-test-panels-on-close-really-pushes-to-back ()
  "The deprio close reaches the REAL shuffle, not a stub: `SPC o C' end to end.
No `cl-letf' over push-to-back here — that stub is exactly what hid the
void function from this suite."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (agent-repl--ws-put "a" :frontend 'vterm)
    (let ((agent-repl-roster--tab-order '("a" "b")))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "a"))
                ((symbol-function 'agent-repl--hide-panels) #'ignore)
                ((symbol-function 'agent-repl--ws-update-names-cache) #'ignore)
                ((symbol-function 'agent-repl--force-tab-bar-redraw) #'ignore)
                ((symbol-function 'agent-repl--ws-switch) #'ignore))
        ;; Act
        (agent-repl--on-close)
        ;; Assert
        (should (equal agent-repl-roster--tab-order '("b" "a")))))))

;;;; ---- Tests: panels-any-visible-p ----

(ert-deftest agent-repl-test-panels-any-visible-input-only ()
  "panels-any-visible-p is non-nil when only the input panel is visible."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--input-visible-p) (lambda () t))
              ((symbol-function 'agent-repl--view-visible-p) (lambda () nil)))
      (should (agent-repl--panels-any-visible-p)))))

(ert-deftest agent-repl-test-panels-any-visible-view-only ()
  "panels-any-visible-p is non-nil when only the agent view is visible."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--input-visible-p) (lambda () nil))
              ((symbol-function 'agent-repl--view-visible-p) (lambda () t)))
      (should (agent-repl--panels-any-visible-p)))))

(ert-deftest agent-repl-test-panels-any-visible-neither ()
  "panels-any-visible-p is nil when no panel is visible."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--input-visible-p) (lambda () nil))
              ((symbol-function 'agent-repl--view-visible-p) (lambda () nil)))
      (should-not (agent-repl--panels-any-visible-p)))))

;;;; ---- Tests: single-press close (agent-repl--toggle) ----

(ert-deftest agent-repl-test-panels-toggle-closes-in-one-press-input-only ()
  "One `SPC o c' closes when only the input composer is visible.
Point-in-input / webview-window-absent is the case the old
webview-only check let fall through to a re-show, forcing a second
press."
  (agent-repl-test--with-clean-state
    (let ((close-calls 0)
          (show-checked nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "tw"))
                ((symbol-function 'agent-repl--ws-frontend) (lambda (_ws) nil))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'agent-repl--input-visible-p) (lambda () t))
                ((symbol-function 'agent-repl--view-visible-p) (lambda () nil))
                ((symbol-function 'agent-repl-frontend-running-p-fn)
                 (lambda (_fe) (lambda (_ws) (setq show-checked t) t))))
        (agent-repl--toggle (lambda () (cl-incf close-calls)))
        (should (= close-calls 1))
        (should-not show-checked)))))

(ert-deftest agent-repl-test-panels-toggle-closes-in-one-press-webview-visible ()
  "One `SPC o c' closes when the webview window is present."
  (agent-repl-test--with-clean-state
    (let ((close-calls 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "tw"))
                ((symbol-function 'agent-repl--ws-frontend) (lambda (_ws) nil))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'agent-repl--input-visible-p) (lambda () t))
                ((symbol-function 'agent-repl--view-visible-p) (lambda () t)))
        (agent-repl--toggle (lambda () (cl-incf close-calls)))
        (should (= close-calls 1))))))

(ert-deftest agent-repl-test-panels-toggle-does-not-close-when-hidden ()
  "With no panel visible the toggle opens rather than taking the close branch."
  (agent-repl-test--with-clean-state
    (let* ((close-calls 0)
           (opened nil)
           (fe (agent-repl-frontend-create
                :name 'gui
                :running-p-fn (lambda (_ws) nil)
                :open-fn (lambda (_ws) (setq opened t) :done))))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "tw"))
                ((symbol-function 'agent-repl--ws-frontend) (lambda (_ws) fe))
                ((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'agent-repl--input-visible-p) (lambda () nil))
                ((symbol-function 'agent-repl--view-visible-p) (lambda () nil))
                ((symbol-function 'agent-repl--open-progress-active-p)
                 (lambda (_ws) nil))
                ((symbol-function 'agent-repl--open-progress-start) #'ignore)
                ((symbol-function 'agent-repl--panels-ensure-host-subscription)
                 (lambda (_ws) t))
                ((symbol-function 'agent-repl--settle-placeholder)
                 (lambda (_ws outcome) outcome)))
        (agent-repl--toggle (lambda () (cl-incf close-calls)))
        (should (= close-calls 0))
        (should opened)))))

;;;; ---- Tests: the autoselect landing is DURABLE in the log ----

;; `maybe-autoselect-input' recorded on the debug rung, below the default
;; durable threshold (`info'), so the record that says "the SWITCH is what put
;; the cursor in the composer" never reached disk.  The cursor state alone
;; cannot say that: a composer that was already selected looks identical.

(defmacro agent-repl-test-panels--capturing-rungs (&rest body)
  "Run BODY capturing the `info' and `debug' logging rungs separately."
  (declare (indent 0))
  `(let ((agent-repl-test-panels--info nil)
         (agent-repl-test-panels--debug nil))
     (cl-letf (((symbol-function 'agent-repl--info)
                (lambda (_ws fmt &rest args)
                  (push (apply #'format fmt args) agent-repl-test-panels--info)))
               ((symbol-function 'agent-repl--log)
                (lambda (_ws fmt &rest args)
                  (push (apply #'format fmt args) agent-repl-test-panels--debug))))
       ,@body)))

(defvar agent-repl-test-panels--info nil
  "Messages the stubbed `info' rung received during a capture.")

(defvar agent-repl-test-panels--debug nil
  "Messages the stubbed debug rung received during a capture.")

(ert-deftest agent-repl-test-panels-autoselect-records-the-select-branch ()
  "Selecting the input window records `maybe-autoselect-input: ws=... branch=select'."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((input-buf (get-buffer-create "*autoselect-log-input*"))
          (new-win nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (setq new-win (split-window))
            (set-window-buffer new-win input-buf)
            (select-window (car (window-list)))
            ;; Act
            (agent-repl-test-panels--capturing-rungs
              (let ((agent-repl-autoselect-input-on-workspace-switch t))
                (agent-repl--maybe-autoselect-input "test-ws"))
              ;; Assert
              (should (cl-find-if
                       (lambda (m)
                         (string-prefix-p
                          "maybe-autoselect-input: ws=test-ws branch=select" m))
                       agent-repl-test-panels--info))))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

(ert-deftest agent-repl-test-panels-autoselect-select-branch-is-not-debug ()
  "The landing record must not sit below the default durable threshold."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((input-buf (get-buffer-create "*autoselect-log-rung*"))
          (new-win nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (setq new-win (split-window))
            (set-window-buffer new-win input-buf)
            (select-window (car (window-list)))
            ;; Act
            (agent-repl-test-panels--capturing-rungs
              (let ((agent-repl-autoselect-input-on-workspace-switch t))
                (agent-repl--maybe-autoselect-input "test-ws"))
              ;; Assert
              (should-not (cl-find-if
                           (lambda (m)
                             (string-prefix-p "maybe-autoselect-input: " m))
                           agent-repl-test-panels--debug))))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

(ert-deftest agent-repl-test-panels-autoselect-records-closed-panels ()
  "A switch to a workspace whose panels are CLOSED records `no-input-window'.
Closed panels are an ordinary outcome, not a failure: the switch happens,
the cursor stays where it is, and the record says why."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((input-buf (get-buffer-create "*autoselect-log-closed*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (switch-to-buffer (get-buffer-create "*autoselect-log-other*"))
            ;; Act
            (agent-repl-test-panels--capturing-rungs
              (let ((agent-repl-autoselect-input-on-workspace-switch t))
                (agent-repl--maybe-autoselect-input "test-ws"))
              ;; Assert
              (should (member "maybe-autoselect-input: ws=test-ws branch=no-input-window"
                              agent-repl-test-panels--info))))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))
        (when (get-buffer "*autoselect-log-other*")
          (kill-buffer "*autoselect-log-other*"))))))

(ert-deftest agent-repl-test-panels-autoselect-records-the-disabled-branch ()
  "With the defcustom off the record still says so, durably."
  (agent-repl-test--with-clean-state
    ;; Arrange / Act
    (agent-repl-test-panels--capturing-rungs
      (let ((agent-repl-autoselect-input-on-workspace-switch nil))
        (agent-repl--maybe-autoselect-input "test-ws"))
      ;; Assert
      (should (member "maybe-autoselect-input: ws=test-ws branch=disabled"
                      agent-repl-test-panels--info)))))

;;;; ---- Tests: input command-state on switch ----

(ert-deftest agent-repl-test-panels-autoselect-enters-command-state ()
  "maybe-autoselect-input puts evil in normal state after selecting input."
  (agent-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*autoselect-cmd-input*"))
          (new-win nil)
          (normal-called nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (setq new-win (split-window))
            (set-window-buffer new-win input-buf)
            (select-window (car (window-list)))
            (cl-letf (((symbol-function 'evil-normal-state)
                       (lambda (&rest _)
                         (setq normal-called
                               (eq (window-buffer (selected-window)) input-buf)))))
              (let ((agent-repl-autoselect-input-on-workspace-switch t))
                (agent-repl--maybe-autoselect-input "test-ws")
                (should (eq (window-buffer (selected-window)) input-buf))
                (should normal-called))))
        (when (and new-win (window-live-p new-win))
          (ignore-errors (delete-window new-win)))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))))))

(ert-deftest agent-repl-test-panels-autoselect-no-command-state-without-input-window ()
  "maybe-autoselect-input does not enter command state when input is not visible."
  (agent-repl-test--with-clean-state
    (let ((input-buf (get-buffer-create "*autoselect-cmd-hidden*"))
          (normal-called nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :input-buffer input-buf)
            (switch-to-buffer (get-buffer-create "*other-cmd*"))
            (cl-letf (((symbol-function 'evil-normal-state)
                       (lambda (&rest _) (setq normal-called t))))
              (let ((agent-repl-autoselect-input-on-workspace-switch t))
                (agent-repl--maybe-autoselect-input "test-ws")
                (should-not normal-called))))
        (when (buffer-live-p input-buf) (kill-buffer input-buf))
        (when (get-buffer "*other-cmd*") (kill-buffer "*other-cmd*"))))))

(ert-deftest agent-repl-test-panels-input-enter-command-state-noop-without-evil ()
  "input-enter-command-state is a no-op (no error) when evil is unavailable.
The batch -Q harness does not load evil, so `evil-normal-state' is not
`fboundp' and the helper must simply return without signalling."
  (skip-unless (not (fboundp 'evil-normal-state)))
  (agent-repl-test--with-clean-state
    ;; Should not error, and returns nil since the guard short-circuits.
    (should-not (agent-repl--input-enter-command-state))))

;;;; ---- Tests: panels-open default and explicit-close preference ----

(ert-deftest agent-repl-test-panels-open-preferred-when-no-preference ()
  "A workspace with no recorded preference defaults to panels-open."
  (agent-repl-test--with-clean-state
    (should (agent-repl--ws-panels-open-preferred-p "fresh-ws"))))

(ert-deftest agent-repl-test-panels-open-not-preferred-after-explicit-close ()
  "An explicit close is honored: panels-open is no longer preferred."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :panels-closed-by-user t)
    (should-not (agent-repl--ws-panels-open-preferred-p "ws"))))

(ert-deftest agent-repl-test-panels-note-closed-by-user-sets-flag ()
  "note-panels-closed-by-user records the explicit-close preference."
  (agent-repl-test--with-clean-state
    (agent-repl--note-panels-closed-by-user "ws")
    (should (agent-repl--ws-get "ws" :panels-closed-by-user))))

(ert-deftest agent-repl-test-panels-note-closed-by-user-nil-ws-noop ()
  "note-panels-closed-by-user is a no-op for a nil workspace."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--note-panels-closed-by-user nil))))

(ert-deftest agent-repl-test-panels-note-shown-clears-flag ()
  "note-panels-shown clears a recorded explicit-close preference."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :panels-closed-by-user t)
    (agent-repl--note-panels-shown "ws")
    (should-not (agent-repl--ws-get "ws" :panels-closed-by-user))))

(ert-deftest agent-repl-test-panels-ensure-own-shows-by-default-no-preference ()
  "ensure-own-panels-on-persp-switch shows panels by DEFAULT when no
preference is recorded (panels default to open), given a live view buffer."
  (agent-repl-test--with-clean-state
    (let ((shown-ws nil))
      (let ((frontend-buf (get-buffer-create "*agent-frontend-def-ws*"))
            (input-buf (get-buffer-create "*agent-panel-input-def-ws*")))
        (unwind-protect
            (progn
              (agent-repl--ws-put "def-ws" :frontend-buffer frontend-buf)
              (agent-repl--ws-put "def-ws" :input-buffer input-buf)
              ;; NB: :panels-were-visible is deliberately NOT set — a
              ;; never-stood-in workspace must still default open.
              (cl-letf (((symbol-function '+workspace-current-name) (lambda () "def-ws"))
                        ((symbol-function 'agent-repl--stale-panel-windows) (lambda () nil))
                        ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                        ((symbol-function 'agent-repl--frontend-dispatch-show)
                         (lambda (ws) (setq shown-ws ws))))
                (agent-repl--ensure-own-panels-on-persp-switch "def-ws")
                (should (equal shown-ws "def-ws"))))
          (kill-buffer frontend-buf)
          (kill-buffer input-buf))))))

(ert-deftest agent-repl-test-panels-ensure-own-honors-explicit-close ()
  "ensure-own-panels-on-persp-switch does NOT re-show when the user
explicitly closed the panels, even with a live view buffer."
  (agent-repl-test--with-clean-state
    (let ((shown-ws nil))
      (let ((frontend-buf (get-buffer-create "*agent-frontend-closed-ws*"))
            (input-buf (get-buffer-create "*agent-panel-input-closed-ws*")))
        (unwind-protect
            (progn
              (agent-repl--ws-put "closed-ws" :frontend-buffer frontend-buf)
              (agent-repl--ws-put "closed-ws" :input-buffer input-buf)
              (agent-repl--ws-put "closed-ws" :panels-closed-by-user t)
              (cl-letf (((symbol-function '+workspace-current-name) (lambda () "closed-ws"))
                        ((symbol-function 'agent-repl--stale-panel-windows) (lambda () nil))
                        ((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                        ((symbol-function 'agent-repl--frontend-dispatch-show)
                         (lambda (ws) (setq shown-ws ws))))
                (agent-repl--ensure-own-panels-on-persp-switch "closed-ws")
                (should-not shown-ws)))
          (kill-buffer frontend-buf)
          (kill-buffer input-buf))))))

(ert-deftest agent-repl-test-panels-on-simple-close-records-explicit-close ()
  "agent-repl--on-simple-close records the explicit-close preference."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws"))
              ((symbol-function 'agent-repl--close-view) #'ignore))
      (agent-repl--on-simple-close)
      (should (agent-repl--ws-get "ws" :panels-closed-by-user)))))

(ert-deftest agent-repl-test-panels-on-close-records-explicit-close ()
  "agent-repl--on-close records the explicit-close preference."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws"))
              ((symbol-function 'agent-repl--close-view) #'ignore)
              ((symbol-function 'agent-repl--save-tab-index) #'ignore)
              ((symbol-function 'agent-repl-workspace-push-to-back) #'ignore))
      (agent-repl--on-close)
      (should (agent-repl--ws-get "ws" :panels-closed-by-user)))))

;;;; ---- Tests: opening a workspace opens its panels (arrival) ----
;;
;; Owner ruling, 2026-09-13, item 6: opening a workspace by any means other
;; than switching to an already-open one opens its agent-repl panels
;; immediately, before it is switched to.

(defmacro agent-repl-test--with-arrival (&rest body)
  "Run BODY with the arrival opener's dispatches recorded.
`opened' and `shown' hold the workspace each frontend capability was
called with, `anchored' the workspace the background anchor ran in, and
`records' every INFO line the arrival wrote.  The workspace `arrived-ws'
is registered and live, and the arrival gate is ARMED — the startup
exemption has its own test."
  (declare (indent 0))
  `(agent-repl-test--with-clean-state
     (let ((opened nil) (shown nil) (anchored nil) (records nil)
           (agent-repl--panels-arrivals-armed t))
       (agent-repl--ws-put "arrived-ws" :project-dir "/w/arrived")
       (cl-letf (((symbol-function '+workspace-current-name) (lambda () "arrived-ws"))
                 ((symbol-function 'agent-repl--call-in-background-workspace)
                  (lambda (ws fn) (setq anchored ws) (funcall fn)))
                 ((symbol-function 'agent-repl--panels-any-visible-p) (lambda () nil))
                 ((symbol-function 'agent-repl-host-ref)
                  (lambda (_ws) '(:id "ws-1" :dir "/w/arrived")))
                 ((symbol-function 'agent-repl-host-stream) (lambda (_ws) t))
                 ((symbol-function 'agent-repl--open-progress-show)
                  (lambda (_ws buf) buf))
                 ((symbol-function 'agent-repl--info)
                  (lambda (_ws fmt &rest args) (push (apply #'format fmt args) records)))
                 ((symbol-function 'agent-repl--ws-frontend)
                  (lambda (_ws)
                    (agent-repl-frontend-create
                     :name 'probe
                     :open-fn (lambda (ws) (setq opened ws) :pending)
                     :kill-fn #'ignore
                     :running-p-fn (lambda (_ws) nil)
                     :show-fn (lambda (ws) (setq shown ws) :pending)
                     :supported-backends '(claude)))))
         ,@body))))

(ert-deftest agent-repl-test-panels-arrival-opens-the-panels ()
  "A workspace that arrives open dispatches its frontend's OPEN capability."
  ;; Arrange / Act
  (agent-repl-test--with-arrival
    (agent-repl--panels-open-on-arrival "arrived-ws" "ws-1")
    ;; Assert
    (should (equal opened "arrived-ws"))))

(ert-deftest agent-repl-test-panels-arrival-opens-inside-the-workspace ()
  "The open is anchored IN the arriving workspace, which is not current yet."
  ;; Arrange / Act
  (agent-repl-test--with-arrival
    (agent-repl--panels-open-on-arrival "arrived-ws" "ws-1")
    ;; Assert
    (should (equal anchored "arrived-ws"))))

(ert-deftest agent-repl-test-panels-arrival-records-the-verb-s-reason ()
  "The arrival record carries the reason the verb that asked for it recorded."
  ;; Arrange
  (agent-repl-test--with-arrival
    (agent-repl--panels-note-arrival-reason "ws-1" "forked")
    ;; Act
    (agent-repl--panels-open-on-arrival "arrived-ws" "ws-1")
    ;; Assert
    (should (member "elisp.panels.opened-on-arrival ws=arrived-ws reason=forked"
                    records))))

(ert-deftest agent-repl-test-panels-arrival-with-no-verb-reads-arrived ()
  "A daemon-side arrival nobody claimed is recorded as `arrived'."
  ;; Arrange / Act
  (agent-repl-test--with-arrival
    (agent-repl--panels-open-on-arrival "arrived-ws" "ws-1")
    ;; Assert
    (should (member "elisp.panels.opened-on-arrival ws=arrived-ws reason=arrived"
                    records))))

(ert-deftest agent-repl-test-panels-arrival-is-idempotent-when-already-open ()
  "An arrival whose panels are already on screen dispatches nothing."
  ;; Arrange
  (agent-repl-test--with-arrival
    (cl-letf (((symbol-function 'agent-repl--panels-any-visible-p) (lambda () t)))
      ;; Act
      (agent-repl--panels-open-on-arrival "arrived-ws" "ws-1")
      ;; Assert
      (should (and (null opened) (null shown))))))

(ert-deftest agent-repl-test-panels-startup-arrival-opens-nothing ()
  "A workspace already open when Emacs started is left to the startup path."
  ;; Arrange
  (agent-repl-test--with-arrival
    (let ((agent-repl--panels-arrivals-armed nil))
      ;; Act
      (agent-repl--panels-open-on-arrival "arrived-ws" "ws-1")
      ;; Assert
      (should-not opened))))

(ert-deftest agent-repl-test-panels-startup-arrival-a-verb-claimed-still-opens ()
  "A verb that claimed an arrival opens it even before the startup roster lands."
  ;; Arrange
  (agent-repl-test--with-arrival
    (let ((agent-repl--panels-arrivals-armed nil))
      (agent-repl--panels-note-arrival-reason "ws-1" "created")
      ;; Act
      (agent-repl--panels-open-on-arrival "arrived-ws" "ws-1")
      ;; Assert
      (should (equal opened "arrived-ws")))))

(ert-deftest agent-repl-test-panels-arrival-reason-is-consumed-once ()
  "A reason is spent by the arrival that reads it, never by a second one."
  ;; Arrange
  (agent-repl-test--with-arrival
    (agent-repl--panels-note-arrival-reason "ws-1" "registered")
    (agent-repl--panels-take-arrival-reason "ws-1")
    ;; Act / Assert
    (should (equal (agent-repl--panels-take-arrival-reason "ws-1") "arrived"))))

(ert-deftest agent-repl-test-panels-arrival-skips-a-dead-workspace ()
  "A workspace that is not live opens no panels."
  ;; Arrange
  (agent-repl-test--with-arrival
    ;; Act
    (agent-repl--panels-open-on-arrival "never-registered-ws" "ws-2")
    ;; Assert
    (should-not opened)))

;;;; ---- The one pre-panel layout save ----

(ert-deftest agent-repl-test-panels-own-buffers-include-the-open-placeholder ()
  "An open's placeholder is one of the workspace's own panel buffers.
It takes the main-area window the view mounts into, so a frame showing
it is covered by the mount, not by the user's work."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((placeholder (get-buffer-create " *agent-opening-own1*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--open-progress-placeholder)
                     (lambda (_ws) placeholder)))
            ;; Act / Assert
            (should (memq placeholder (agent-repl--panels-own-buffers "test-ws"))))
        (kill-buffer placeholder)))))

(ert-deftest agent-repl-test-panels-a-frame-showing-the-placeholder-is-covered ()
  "A frame holding only the placeholder is covered by the mount."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((placeholder (get-buffer-create " *agent-opening-cover1*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--open-progress-placeholder)
                     (lambda (_ws) placeholder)))
            (delete-other-windows)
            (set-window-buffer (selected-window) placeholder)
            ;; Act / Assert
            (should (agent-repl--panels-cover-frame-p "test-ws")))
        (kill-buffer placeholder)
        (delete-other-windows)))))

(ert-deftest agent-repl-test-panels-save-pre-panel-layout-saves-the-users-frame ()
  "The layout under a mount that has not yet taken the frame is saved."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((work (get-buffer-create "*pre-panel-work1*")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (set-window-buffer (selected-window) work)
            ;; Act
            (agent-repl--save-pre-panel-layout "test-ws" "test")
            ;; Assert
            (should (window-configuration-p
                     (agent-repl--ws-get "test-ws" :fullscreen-config))))
        (kill-buffer work)
        (delete-other-windows)))))

(ert-deftest agent-repl-test-panels-save-pre-panel-layout-keeps-it-under-the-mount ()
  "A save while the mount covers the frame keeps the layout already saved."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((placeholder (get-buffer-create " *agent-opening-keep1*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--open-progress-placeholder)
                     (lambda (_ws) placeholder)))
            (delete-other-windows)
            (set-window-buffer (selected-window) placeholder)
            (agent-repl--ws-put "test-ws" :fullscreen-config 'users-frame)
            ;; Act
            (agent-repl--save-pre-panel-layout "test-ws" "test")
            ;; Assert
            (should (eq (agent-repl--ws-get "test-ws" :fullscreen-config) 'users-frame)))
        (kill-buffer placeholder)
        (delete-other-windows)))))

(defconst agent-repl-test-panels--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The lisp/ directory the mount sources are read from.")

(ert-deftest agent-repl-test-panels-every-mount-saves-through-the-one-helper ()
  "No mount site records `:fullscreen-config' by hand.
The layout under the mount is saved by `agent-repl--save-pre-panel-layout'
alone; a site writing `current-window-configuration' into the key itself
would save whatever the mount had already put on the frame."
  (let ((dir agent-repl-test-panels--dir))
    (dolist (file '("frontend.el" "open-progress.el" "panels.el"))
      (with-temp-buffer
        (insert-file-contents (expand-file-name file dir))
        (goto-char (point-min))
        (let ((hits 0))
          (while (re-search-forward
                  ":fullscreen-config (current-window-configuration)" nil t)
            (setq hits (1+ hits)))
          (should (= hits (if (equal file "panels.el") 1 0))))))))

;;;; ---- A restore that awaited its view ----

(ert-deftest agent-repl-test-panels-a-restore-with-no-view-awaits-one ()
  "A restore that declined for want of a view is recorded as awaiting it."
  (let ((agent-repl--panels-restore-awaiting-view nil))
    (agent-repl--panels-note-restore-outcome "ws-a" 'no-live-view)
    (should (equal agent-repl--panels-restore-awaiting-view '("ws-a")))))

(ert-deftest agent-repl-test-panels-any-other-restore-outcome-awaits-nothing ()
  "A restore that decided anything else no longer awaits a view."
  (let ((agent-repl--panels-restore-awaiting-view (list "ws-a")))
    (agent-repl--panels-note-restore-outcome "ws-a" 'already-visible)
    (should-not agent-repl--panels-restore-awaiting-view)))

(ert-deftest agent-repl-test-panels-a-created-view-restores-the-current-workspace ()
  "The view the current workspace awaited runs its restore and input select.
MEASURED 2026-09-29: the first workspace at startup showed no panels until
the user switched away and back."
  (let ((agent-repl--panels-restore-awaiting-view (list "ws-a"))
        calls)
    (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws-a"))
              ((symbol-function 'agent-repl--ensure-own-panels-on-persp-switch)
               (lambda (ws) (push (list 'restore ws) calls)))
              ((symbol-function 'agent-repl--maybe-autoselect-input)
               (lambda (ws) (push (list 'select ws) calls))))
      (agent-repl--panels-on-view-created "ws-a")
      (should (equal (nreverse calls) '((restore "ws-a") (select "ws-a"))))
      (should-not agent-repl--panels-restore-awaiting-view))))

(ert-deftest agent-repl-test-panels-a-created-view-leaves-another-workspace-to-its-switch ()
  "A workspace the user has left restores on its next switch, not now."
  (let ((agent-repl--panels-restore-awaiting-view (list "ws-a"))
        restored)
    (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws-b"))
              ((symbol-function 'agent-repl--ensure-own-panels-on-persp-switch)
               (lambda (_ws) (setq restored t))))
      (agent-repl--panels-on-view-created "ws-a")
      (should-not restored)
      (should-not agent-repl--panels-restore-awaiting-view))))

(ert-deftest agent-repl-test-panels-a-created-view-nobody-awaited-restores-nothing ()
  "A view no restore awaited changes nothing about the panels."
  (let ((agent-repl--panels-restore-awaiting-view nil)
        restored)
    (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws-a"))
              ((symbol-function 'agent-repl--ensure-own-panels-on-persp-switch)
               (lambda (_ws) (setq restored t))))
      (agent-repl--panels-on-view-created "ws-a")
      (should-not restored))))

(ert-deftest agent-repl-test-panels-push-to-back-passes-over-a-hidden-tab ()
  "The vacated slot's occupant is the next DRAWN tab, never a hidden one."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((agent-repl-roster--tab-order '("a" "hidden" "c"))
          (agent-repl-roster--hidden-tabs '("hidden"))
          (switched 'unset))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "a"))
                ((symbol-function 'agent-repl--ws-update-names-cache) #'ignore)
                ((symbol-function 'agent-repl--force-tab-bar-redraw) #'ignore)
                ((symbol-function 'agent-repl--ws-switch)
                 (lambda (ws &rest _) (setq switched ws))))
        ;; Act
        (agent-repl-workspace-push-to-back)
        ;; Assert
        (should (equal switched "c"))))))

;;;; ---- Switch trigger capture -------------------------------------------

(ert-deftest agent-repl-test-panels-capture-switch-trigger-records-the-command ()
  ;; Arrange
  (let ((this-command 'agent-repl-switch-right)
        (agent-repl--switch-trigger nil))
    ;; Act
    (agent-repl--capture-switch-trigger)
    ;; Assert
    (should (eq (plist-get agent-repl--switch-trigger :command) 'agent-repl-switch-right))))

(ert-deftest agent-repl-test-panels-capture-switch-trigger-records-the-webview-window ()
  ;; Arrange
  (let ((buf (get-buffer-create "*agent-frontend-capture-test*"))
        (agent-repl--switch-trigger nil))
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) buf)
          ;; Act
          (agent-repl--capture-switch-trigger)
          ;; Assert
          (should (eq (plist-get agent-repl--switch-trigger :webview-window) (selected-window))))
      (kill-buffer buf))))

(ert-deftest agent-repl-test-panels-capture-switch-trigger-no-webview-window-is-nil ()
  ;; Arrange
  (let ((agent-repl--switch-trigger nil))
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      ;; Act
      (agent-repl--capture-switch-trigger)
      ;; Assert
      (should-not (plist-get agent-repl--switch-trigger :webview-window)))))

;;;; ---- Tests: the switch's webview record only for real workspaces ----

(ert-deftest agent-repl-test-panels-ensure-own-schedules-no-webview-record-for-a-placeholder ()
  "A placeholder perspective (no webview, no log sink) schedules no record."
  (agent-repl-test--with-clean-state
    (let ((scheduled nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "main"))
                ((symbol-function 'agent-repl--stale-panel-windows) (lambda () nil))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'agent-repl--ws-log-name) (lambda (_ws) nil))
                ((symbol-function 'agent-repl-window--panel-buffer) (lambda (&rest _) nil))
                ((symbol-function 'run-with-idle-timer)
                 (lambda (&rest _) (setq scheduled t))))
        ;; Act
        (agent-repl--ensure-own-panels-on-persp-switch "main")
        ;; Assert
        (should-not scheduled)))))

(ert-deftest agent-repl-test-panels-ensure-own-schedules-the-webview-record-for-a-workspace ()
  "A workspace with a live webview and a log sink schedules its record."
  (agent-repl-test--with-clean-state
    (let ((scheduled nil)
          (view (get-buffer-create "*agent-frontend-my-ws*")))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                    ((symbol-function 'agent-repl--stale-panel-windows) (lambda () nil))
                    ((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                    ((symbol-function 'agent-repl--ws-log-name) (lambda (ws) ws))
                    ((symbol-function 'agent-repl-window--panel-buffer)
                     (lambda (&rest _) view))
                    ((symbol-function 'run-with-idle-timer)
                     (lambda (&rest _) (setq scheduled t))))
            ;; Act
            (agent-repl--ensure-own-panels-on-persp-switch "my-ws")
            ;; Assert
            (should scheduled))
        (kill-buffer view)))))
