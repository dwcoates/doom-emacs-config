;;; test-frontend-state.el --- ERT tests for frontend-state.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Pure-elisp tests for the frontend.v1 state application layer:
;; RenderState -> render keyword mapping, WorkspaceState application into
;; the workspace.el pushed-state key, StateSnapshot resync, and
;; DegradedNotice surfacing.  No processes, no network — the frame plists
;; are constructed directly (the transport's decode is covered in
;; test-frontend-uds.el).  `message' is shadowed where echo-area output is
;; asserted.
;;
;; One edge case per test, AAA structure.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-frontend-state.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;; Transport before state: frontend-state.el registers its handlers into
;; frontend-uds.el at load, so load the transport first.
(load (expand-file-name "frontend-uds.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)
(load (expand-file-name "frontend-state.el" (file-name-directory
                                             (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; ---- RenderState -> keyword: one test per enum value -----------------

(defmacro agent-repl-test--deftest-state-map (name state keyword)
  "Define a mapping test NAME asserting STATE maps to render KEYWORD."
  `(ert-deftest ,name ()
     ,(format "RenderState %s maps to %s." state keyword)
     (should (eq (agent-repl--frontend-state->keyword ,state) ,keyword))))

(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-init "RENDER_STATE_INIT" :init)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-idle "RENDER_STATE_IDLE" :idle)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-idle-async "RENDER_STATE_IDLE_ASYNC" :idle-async)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-thinking "RENDER_STATE_THINKING" :thinking)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-permission "RENDER_STATE_PERMISSION" :permission)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-done "RENDER_STATE_DONE" :done)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-stop-failed "RENDER_STATE_STOP_FAILED" :stop-failed)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-merging "RENDER_STATE_MERGING" :merging)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-merge-queued "RENDER_STATE_MERGE_QUEUED" :merge-queued)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-merge-conflict "RENDER_STATE_MERGE_CONFLICT" :merge-conflict)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-merge-failed "RENDER_STATE_MERGE_FAILED" :merge-failed)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-merged "RENDER_STATE_MERGED" :merged)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-dead "RENDER_STATE_DEAD" :dead)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-degraded "RENDER_STATE_DEGRADED" :degraded)

(ert-deftest agent-repl-test-state-map-unspecified-errors ()
  "RENDER_STATE_UNSPECIFIED is unresolved and errors (no fallback keyword)."
  ;; Act / Assert
  (should-error (agent-repl--frontend-state->keyword "RENDER_STATE_UNSPECIFIED")))

(ert-deftest agent-repl-test-state-map-unknown-errors ()
  "An unknown RenderState string errors (no fallback keyword)."
  ;; Act / Assert
  (should-error (agent-repl--frontend-state->keyword "RENDER_STATE_WAT")))

(ert-deftest agent-repl-test-state-map-nil-errors ()
  "A nil state (protojson omits a default enum) errors, never defaults."
  ;; Act / Assert
  (should-error (agent-repl--frontend-state->keyword nil)))

;;;; ---- WorkspaceState application --------------------------------------

(ert-deftest agent-repl-test-apply-workspace-state-stores-keyword ()
  "Applying a WorkspaceState stores the mapped keyword under :pushed-render-state."
  ;; Arrange
  (agent-repl-test--with-clean-state
    ;; Act
    (agent-repl--frontend-apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_THINKING"))
    ;; Assert
    (should (eq (agent-repl--ws-get "ws1" :pushed-render-state) :thinking))))

(ert-deftest agent-repl-test-apply-workspace-state-returns-keyword ()
  "Applying a WorkspaceState returns the mapped keyword."
  ;; Arrange
  (agent-repl-test--with-clean-state
    ;; Act / Assert
    (should (eq (agent-repl--frontend-apply-workspace-state
                '(:workspace "ws1" :state "RENDER_STATE_DONE"))
               :done))))

(ert-deftest agent-repl-test-apply-workspace-state-overwrites ()
  "A later WorkspaceState overwrites the earlier pushed state (daemon is truth)."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--frontend-apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_THINKING"))
    ;; Act
    (agent-repl--frontend-apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_IDLE"))
    ;; Assert
    (should (eq (agent-repl--ws-get "ws1" :pushed-render-state) :idle))))

(ert-deftest agent-repl-test-apply-workspace-state-stores-inputs ()
  "The resolution inputs are stored under :pushed-render-state-meta."
  ;; Arrange
  (agent-repl-test--with-clean-state
    ;; Act
    (agent-repl--frontend-apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_IDLE_ASYNC"
       :turnActive t :liveTaskCount "3" :mergePhase "none"
       :causeKind "task_started" :causeSeq "42"))
    ;; Assert
    (let ((meta (agent-repl--ws-get "ws1" :pushed-render-state-meta)))
      (should (eq (plist-get meta :turn-active) t))
      (should (equal (plist-get meta :live-task-count) "3"))
      (should (equal (plist-get meta :cause-kind) "task_started"))
      (should (equal (plist-get meta :cause-seq) "42")))))

(ert-deftest agent-repl-test-apply-workspace-state-missing-workspace-errors ()
  "A WorkspaceState with no workspace fails loudly (invariant violation)."
  ;; Arrange
  (agent-repl-test--with-clean-state
    ;; Act / Assert
    (should-error (agent-repl--frontend-apply-workspace-state
                   '(:state "RENDER_STATE_IDLE")))))

(ert-deftest agent-repl-test-apply-workspace-state-blank-workspace-errors ()
  "A WorkspaceState with a blank workspace fails loudly."
  ;; Arrange
  (agent-repl-test--with-clean-state
    ;; Act / Assert
    (should-error (agent-repl--frontend-apply-workspace-state
                   '(:workspace "" :state "RENDER_STATE_IDLE")))))

(ert-deftest agent-repl-test-apply-workspace-state-bad-state-errors ()
  "A WorkspaceState carrying an unmappable state fails loudly."
  ;; Arrange
  (agent-repl-test--with-clean-state
    ;; Act / Assert
    (should-error (agent-repl--frontend-apply-workspace-state
                   '(:workspace "ws1" :state "RENDER_STATE_UNSPECIFIED")))))

;;;; ---- StateSnapshot resync --------------------------------------------

(ert-deftest agent-repl-test-apply-snapshot-applies-every-workspace ()
  "A StateSnapshot applies the pushed state for every WorkspaceState in it."
  ;; Arrange
  (agent-repl-test--with-clean-state
    ;; Act
    (agent-repl--frontend-apply-snapshot
     '(:workspaces ((:workspace "a" :state "RENDER_STATE_THINKING")
                    (:workspace "b" :state "RENDER_STATE_MERGED"))))
    ;; Assert
    (should (eq (agent-repl--ws-get "a" :pushed-render-state) :thinking))
    (should (eq (agent-repl--ws-get "b" :pushed-render-state) :merged))))

(ert-deftest agent-repl-test-apply-snapshot-returns-count ()
  "A StateSnapshot returns the count of workspace states applied."
  ;; Arrange
  (agent-repl-test--with-clean-state
    ;; Act / Assert
    (should (= (agent-repl--frontend-apply-snapshot
                '(:workspaces ((:workspace "a" :state "RENDER_STATE_IDLE")
                               (:workspace "b" :state "RENDER_STATE_IDLE"))))
               2))))

(ert-deftest agent-repl-test-apply-snapshot-empty-is-zero ()
  "An empty StateSnapshot applies nothing and returns 0."
  ;; Arrange
  (agent-repl-test--with-clean-state
    ;; Act / Assert
    (should (= (agent-repl--frontend-apply-snapshot '(:workspaces nil)) 0))))

(ert-deftest agent-repl-test-apply-snapshot-ignores-sessions-catalogs ()
  "Sessions/catalogs arrays do not break the snapshot (deferred to their handlers)."
  ;; Arrange
  (agent-repl-test--with-clean-state
    ;; Act — sessions/catalogs present but unhandled here
    (agent-repl--frontend-apply-snapshot
     '(:workspaces ((:workspace "a" :state "RENDER_STATE_IDLE"))
       :sessions ((:workspace "a" :model "haiku"))
       :catalogs ((:workspace "a" :tasks nil))))
    ;; Assert — the workspace state still applied cleanly
    (should (eq (agent-repl--ws-get "a" :pushed-render-state) :idle))))

;;;; ---- DegradedNotice surfacing ----------------------------------------

(ert-deftest agent-repl-test-degraded-notice-messages-echo-area ()
  "A DegradedNotice surfaces the component + reason in the echo area."
  ;; Arrange
  (let (echoed)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
      ;; Act
      (agent-repl--frontend-apply-degraded-notice
       '(:component "shim-store" :reason "socket closed"))
      ;; Assert
      (should (string-match-p "shim-store" echoed))
      (should (string-match-p "socket closed" echoed)))))

(ert-deftest agent-repl-test-degraded-notice-recovered-messages-recovery ()
  "A recovered DegradedNotice surfaces a recovery message."
  ;; Arrange
  (let (echoed)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
      ;; Act
      (agent-repl--frontend-apply-degraded-notice
       '(:component "shim-store" :reason "socket closed" :recovered t))
      ;; Assert
      (should (string-match-p "recovered" echoed)))))

(ert-deftest agent-repl-test-degraded-notice-returns-recovered-flag ()
  "The handler returns the :recovered flag."
  ;; Arrange
  (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
    ;; Act / Assert
    (should (eq (agent-repl--frontend-apply-degraded-notice
                 '(:component "x" :reason "y" :recovered t))
                t))))

(ert-deftest agent-repl-test-degraded-notice-missing-component-errors ()
  "A DegradedNotice with no component fails loudly (invariant violation)."
  ;; Act / Assert
  (should-error (agent-repl--frontend-apply-degraded-notice '(:reason "y"))))

;;;; ---- Handler registration wiring -------------------------------------

(ert-deftest agent-repl-test-state-registers-workspace-state-handler ()
  "Loading frontend-state.el registers the workspaceState handler."
  (should (eq (cdr (assoc "workspaceState" agent-repl--uds-frame-handlers))
              #'agent-repl--frontend-apply-workspace-state)))

(ert-deftest agent-repl-test-state-registers-snapshot-handler ()
  "Loading frontend-state.el registers the snapshot handler."
  (should (eq (cdr (assoc "snapshot" agent-repl--uds-frame-handlers))
              #'agent-repl--frontend-apply-snapshot)))

(ert-deftest agent-repl-test-state-registers-degraded-handler ()
  "Loading frontend-state.el registers the degradedNotice handler."
  (should (eq (cdr (assoc "degradedNotice" agent-repl--uds-frame-handlers))
              #'agent-repl--frontend-apply-degraded-notice)))

(ert-deftest agent-repl-test-state-dispatch-end-to-end ()
  "A decoded workspaceState frame dispatched through the transport applies state."
  ;; Arrange
  (agent-repl-test--with-clean-state
    ;; Act — decode + dispatch via the real registered handler
    (agent-repl--uds-dispatch-frame
     (agent-repl--uds-decode-frame
      "{\"workspaceState\":{\"workspace\":\"ws1\",\"state\":\"RENDER_STATE_PERMISSION\"}}"))
    ;; Assert
    (should (eq (agent-repl--ws-get "ws1" :pushed-render-state) :permission))))

;;;; ---- State-transition hook -------------------------------------------

(ert-deftest agent-repl-test-state-transition-hook-fires-on-push ()
  "Applying a WorkspaceState runs the state-transition hook with (ws new prev)."
  ;; Arrange
  (agent-repl-test--with-clean-state
    ;; `let*': the hook-list init form closes over `captured', which must be
    ;; bound FIRST (a plain `let' evaluates all inits in the outer scope, so
    ;; the closure would capture an empty lexical env and never see `captured').
    (let* (captured
           (agent-repl-ws-state-transition-functions
            (list (lambda (ws new prev) (setq captured (list ws new prev))))))
      ;; Act
      (agent-repl--frontend-apply-workspace-state
       '(:workspace "ws1" :state "RENDER_STATE_MERGING"))
      ;; Assert
      (should (equal captured '("ws1" :merging nil))))))

(ert-deftest agent-repl-test-state-transition-hook-previous-keyword ()
  "The hook receives the prior pushed keyword as PREVIOUS on a later push."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let* (captured
           (agent-repl-ws-state-transition-functions
            (list (lambda (ws new prev) (setq captured (list ws new prev))))))
      (agent-repl--frontend-apply-workspace-state
       '(:workspace "ws1" :state "RENDER_STATE_MERGING"))
      ;; Act — second push transitions merging -> merge-conflict
      (agent-repl--frontend-apply-workspace-state
       '(:workspace "ws1" :state "RENDER_STATE_MERGE_CONFLICT"))
      ;; Assert
      (should (equal captured '("ws1" :merge-conflict :merging))))))

(ert-deftest agent-repl-test-state-transition-hook-subscriber-error-isolated ()
  "A signaling subscriber is caught + logged; state application still succeeds."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl-ws-state-transition-functions
           (list (lambda (&rest _) (error "boom")))))
      ;; Act
      (agent-repl--frontend-apply-workspace-state
       '(:workspace "ws1" :state "RENDER_STATE_MERGED"))
      ;; Assert — the pushed state landed despite the broken subscriber
      (should (eq (agent-repl--ws-get "ws1" :pushed-render-state) :merged)))))

;;;; ---- Session-ready latch ---------------------------------------------

(ert-deftest agent-repl-test-latch-fires-agent-ready-on-first-push ()
  "The FIRST pushed WorkspaceState arms the :agent-ready latch bit."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let (calls)
      (cl-letf (((symbol-function 'agent-repl--latch-and-maybe-fire-loaded)
                 (lambda (ws key &optional _m) (push (list ws key) calls))))
        ;; Act
        (agent-repl--frontend-apply-workspace-state
         '(:workspace "ws1" :state "RENDER_STATE_INIT"))
        ;; Assert
        (should (equal calls '(("ws1" :agent-ready))))))))

(ert-deftest agent-repl-test-latch-fires-only-once-per-workspace ()
  "A second pushed state for the same workspace does not re-arm the latch."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let (calls)
      (cl-letf (((symbol-function 'agent-repl--latch-and-maybe-fire-loaded)
                 (lambda (ws key &optional _m) (push (list ws key) calls))))
        (agent-repl--frontend-apply-workspace-state
         '(:workspace "ws1" :state "RENDER_STATE_INIT"))
        ;; Act — second push
        (agent-repl--frontend-apply-workspace-state
         '(:workspace "ws1" :state "RENDER_STATE_IDLE"))
        ;; Assert — latch fired exactly once
        (should (= (length calls) 1))))))

(provide 'test-frontend-state)

;;; test-frontend-state.el ends here
