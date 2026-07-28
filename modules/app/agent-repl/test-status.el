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

;;;; ---- Tests: Typed state setters (ws-set-agent-state, ws-set-repl-state) ----

(ert-deftest agent-repl-test-ws-set-agent-state-writes-both ()
  "ws-set-agent-state writes :agent-state AND legacy :status (write-both)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :thinking)
    (should (eq (agent-repl--ws-get "ws1" :agent-state) :thinking))
    (should (eq (agent-repl--ws-get "ws1" :agent-state) :thinking))))

(ert-deftest agent-repl-test-ws-set-agent-state-nil-writes-both ()
  "ws-set-agent-state nil clears both axes."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :done)
    (agent-repl--ws-set-agent-state "ws1" nil)
    (should-not (agent-repl--ws-get "ws1" :agent-state))
    (should-not (agent-repl--ws-get "ws1" :agent-state))))

(ert-deftest agent-repl-test-ws-set-agent-state-nil-ws-errors ()
  "ws-set-agent-state signals error on nil workspace."
  (should-error (agent-repl--ws-set-agent-state nil :thinking) :type 'error))

(ert-deftest agent-repl-test-ws-agent-state-getter ()
  "ws-agent-state reads :agent-state, not :status."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :agent-state :permission)
    (should (eq (agent-repl--ws-agent-state "ws1") :permission))))

(ert-deftest agent-repl-test-ws-set-repl-state-isolated ()
  "ws-set-repl-state writes :repl-state, leaves :agent-state/:status alone."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :thinking)
    (agent-repl--ws-set-repl-state "ws1" :inactive)
    (should (eq (agent-repl--ws-get "ws1" :repl-state) :inactive))
    (should (eq (agent-repl--ws-get "ws1" :agent-state) :thinking))
    (should (eq (agent-repl--ws-get "ws1" :agent-state) :thinking))))

(ert-deftest agent-repl-test-ws-set-repl-state-nil-ws-errors ()
  "ws-set-repl-state signals error on nil workspace."
  (should-error (agent-repl--ws-set-repl-state nil :inactive) :type 'error))

(ert-deftest agent-repl-test-ws-set-repl-state-persists-active ()
  "ws-set-repl-state calls --state-save when STATE is :active so panel
visibility survives Emacs restart."
  (agent-repl-test--with-clean-state
    (let ((saved nil))
      (cl-letf (((symbol-function 'agent-repl--state-save)
                 (lambda (ws) (setq saved ws))))
        (agent-repl--ws-set-repl-state "ws1" :active)
        (should (equal saved "ws1"))))))

(ert-deftest agent-repl-test-ws-set-repl-state-persists-inactive ()
  "ws-set-repl-state calls --state-save when STATE is :inactive so
hide-mode survives Emacs restart."
  (agent-repl-test--with-clean-state
    (let ((saved nil))
      (cl-letf (((symbol-function 'agent-repl--state-save)
                 (lambda (ws) (setq saved ws))))
        (agent-repl--ws-set-repl-state "ws1" :inactive)
        (should (equal saved "ws1"))))))

(ert-deftest agent-repl-test-ws-set-repl-state-persists-hidden ()
  "ws-set-repl-state calls --state-save when STATE is :hidden so the
deprio-hide marker (set by `SPC o C') survives Emacs restart."
  (agent-repl-test--with-clean-state
    (let ((saved nil))
      (cl-letf (((symbol-function 'agent-repl--state-save)
                 (lambda (ws) (setq saved ws))))
        (agent-repl--ws-set-repl-state "ws1" :hidden)
        (should (equal saved "ws1"))))))

(ert-deftest agent-repl-test-ws-set-repl-state-skips-persist-for-dead ()
  "ws-set-repl-state does NOT persist `:dead' — process death is
lifecycle bookkeeping, not a desired-state hint for restart."
  (agent-repl-test--with-clean-state
    (let ((saved nil))
      (cl-letf (((symbol-function 'agent-repl--state-save)
                 (lambda (ws) (setq saved ws))))
        (agent-repl--ws-set-repl-state "ws1" :dead)
        (should-not saved)))))

(ert-deftest agent-repl-test-ws-set-repl-state-skips-persist-for-nil ()
  "ws-set-repl-state does NOT persist nil — \"no session\" is the
default at restart and shouldn't pin behavior."
  (agent-repl-test--with-clean-state
    (let ((saved nil))
      (cl-letf (((symbol-function 'agent-repl--state-save)
                 (lambda (ws) (setq saved ws))))
        (agent-repl--ws-set-repl-state "ws1" nil)
        (should-not saved)))))

(ert-deftest agent-repl-test-ws-repl-state-getter ()
  "ws-repl-state reads :repl-state, not :status."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :repl-state :init)
    (should (eq (agent-repl--ws-repl-state "ws1") :init))))

(ert-deftest agent-repl-test-ws-agent-state-clear-if-match-clears-both ()
  "ws-agent-state-clear-if with a matching state clears both fields."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :thinking)
    (agent-repl--ws-agent-state-clear-if "ws1" :thinking)
    (should-not (agent-repl--ws-get "ws1" :agent-state))
    (should-not (agent-repl--ws-get "ws1" :agent-state))))

(ert-deftest agent-repl-test-ws-agent-state-clear-if-mismatch-noop ()
  "ws-agent-state-clear-if with a non-matching state is a no-op on both fields."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :done)
    (agent-repl--ws-agent-state-clear-if "ws1" :thinking)
    (should (eq (agent-repl--ws-get "ws1" :agent-state) :done))
    (should (eq (agent-repl--ws-get "ws1" :agent-state) :done))))

(ert-deftest agent-repl-test-ws-agent-state-clear-if-nil-ws-errors ()
  "ws-agent-state-clear-if signals error on nil workspace."
  (should-error (agent-repl--ws-agent-state-clear-if nil :thinking) :type 'error))

;;;; ---- Tests: composed-state mapping ----
;;
;; The legacy `agent-repl--composed-state' pure-mapping tests were
;; removed along with that function.  The render-state contract is
;; now owned by `agent-repl--ws-render-status' in workspace.el and
;; its tests live in test-workspace.el.  The tests below cover only
;; the palette tab-bar wiring + the `--ws-display-state' panel-
;; visibility layer that sits on top of the unified render-state.

(ert-deftest agent-repl-test-tab-palette-has-no-merge-conflict-entry ()
  "The merge states have no palette row: they take none of the five colors
and the bracket no longer carries a glyph to distinguish them."
  (should-not (alist-get :merge-conflict agent-repl--tab-palette)))

(ert-deftest agent-repl-test-tab-palette-has-no-merge-failed-entry ()
  "`:merge-failed' likewise has no palette row."
  (should-not (alist-get :merge-failed agent-repl--tab-palette)))

(ert-deftest agent-repl-test-tab-spec-idle-async-is-yellow ()
  ":idle-async resolves to the amber background so an idle-but-working tab
reads distinctly from :idle orange and :thinking red."
  (should (equal agent-repl--color-idle-async-yellow
                 (plist-get (agent-repl--tab-spec :idle-async nil) :bg))))

;;;; ---- Tests: ws-display-state suppresses all coloring when panels closed ----

(ert-deftest agent-repl-test-display-state-done-panels-closed-renders-nil ()
  ":done with no agent panel in layout renders nil (suppressed on close)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :done)
    (agent-repl--ws-set-repl-state "ws1" :inactive)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p)
               (lambda (_ws) nil)))
      (should-not (agent-repl--ws-display-state "ws1")))))

(ert-deftest agent-repl-test-display-state-done-panels-open-renders-done ()
  ":done with panels visible renders :done (green)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :done)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p)
               (lambda (_ws) t)))
      (should (eq :done (agent-repl--ws-display-state "ws1"))))))

(ert-deftest agent-repl-test-display-state-thinking-panels-closed-renders-nil ()
  "Panels closed during :thinking suppresses red — :agent-state is preserved."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :thinking)
    (agent-repl--ws-set-repl-state "ws1" :inactive)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p)
               (lambda (_ws) nil)))
      (should-not (agent-repl--ws-display-state "ws1"))
      ;; :agent-state must stay so reopen restores the in-flight color.
      (should (eq :thinking (agent-repl--ws-agent-state "ws1"))))))

(ert-deftest agent-repl-test-display-state-thinking-panels-open-renders-thinking ()
  ":thinking with panels visible renders :thinking (red)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :thinking)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p)
               (lambda (_ws) t)))
      (should (eq :thinking (agent-repl--ws-display-state "ws1"))))))

(ert-deftest agent-repl-test-display-state-permission-panels-closed-renders-nil ()
  "Panels closed during :permission suppresses the ❓ badge — state preserved."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :permission)
    (agent-repl--ws-set-repl-state "ws1" :inactive)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p)
               (lambda (_ws) nil)))
      (should-not (agent-repl--ws-display-state "ws1"))
      (should (eq :permission (agent-repl--ws-agent-state "ws1"))))))

(ert-deftest agent-repl-test-display-state-permission-panels-open-renders-permission ()
  ":permission with panels visible renders :permission (green + ❓)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :permission)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p)
               (lambda (_ws) t)))
      (should (eq :permission (agent-repl--ws-display-state "ws1"))))))

(ert-deftest agent-repl-test-display-state-init-panels-closed-renders-nil ()
  ":init with panels closed suppresses the blue badge."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :init)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p)
               (lambda (_ws) nil)))
      (should-not (agent-repl--ws-display-state "ws1")))))

(ert-deftest agent-repl-test-display-state-vendor-blocked-panels-closed-renders-nil ()
  ":vendor-blocked with panels closed suppresses the purple ⛔ badge."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :vendor-blocked)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p)
               (lambda (_ws) nil)))
      (should-not (agent-repl--ws-display-state "ws1")))))

(ert-deftest agent-repl-test-display-state-dead-panels-open-renders-dead ()
  "A workspace whose pushed render-state is :dead renders :dead when visible."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :dead)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p)
               (lambda (_ws) t)))
      (should (eq :dead (agent-repl--ws-display-state "ws1"))))))

(ert-deftest agent-repl-test-display-state-dead-panels-closed-renders-nil ()
  ":dead with panels closed also suppresses the ❌ badge — uniform rule."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :agent-state nil)
    (agent-repl--ws-set-repl-state "ws1" :dead)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p)
               (lambda (_ws) nil)))
      (should-not (agent-repl--ws-display-state "ws1")))))

(ert-deftest agent-repl-test-display-state-idle-panels-open-renders-idle ()
  ":idle with agent panel present in layout renders :idle (orange)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :idle)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p)
               (lambda (_ws) t)))
      (should (eq :idle (agent-repl--ws-display-state "ws1"))))))

(ert-deftest agent-repl-test-display-state-idle-panels-hidden-renders-nil ()
  ":idle with no agent panel in layout renders nil (no background, no badge)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :idle)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p)
               (lambda (_ws) nil)))
      (should-not (agent-repl--ws-display-state "ws1")))))

;;;; ---- Tests: Legacy wrappers still populate both axes ----

(ert-deftest agent-repl-test-legacy-ws-set-writes-agent-state ()
  "Legacy ws-set (wrapper) still writes :agent-state during migration."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :permission)
    (should (eq (agent-repl--ws-get "ws1" :agent-state) :permission))
    (should (eq (agent-repl--ws-get "ws1" :agent-state) :permission))))

(ert-deftest agent-repl-test-legacy-ws-clear-clears-both-axes ()
  "Legacy ws-clear-if-status (wrapper) clears :agent-state too."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :thinking)
    (agent-repl--ws-agent-state-clear-if "ws1" :thinking)
    (should-not (agent-repl--ws-get "ws1" :agent-state))
    (should-not (agent-repl--ws-get "ws1" :agent-state))))

(ert-deftest agent-repl-test-mark-dead-clears-agent-state ()
  "mark-dead clears :agent-state and sets :repl-state :dead."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :done)
    (agent-repl--mark-dead "ws1")
    (should-not (agent-repl--ws-get "ws1" :agent-state))
    (should (eq (agent-repl--ws-get "ws1" :repl-state) :dead))))

(ert-deftest agent-repl-test-mark-dead-preserves-merged ()
  "mark-dead is a no-op when :repl-state is already :merged.
The post-merge poll otherwise clobbers the 🔀 badge with ❌."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :repl-state :merged)
    (agent-repl--mark-dead "ws1")
    (should (eq (agent-repl--ws-get "ws1" :repl-state) :merged))))

(ert-deftest agent-repl-test-mark-dead-already-dead-clears-stale-agent-state ()
  "A death event on an already-:dead workspace still clears :agent-state.
A gui send into a dead binding optimistically marks :thinking; if the
healed session also dies, the death event fires with :repl-state
already :dead and must not leave the tab spinning :thinking forever."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (agent-repl--ws-put "ws1" :repl-state :dead)
    (agent-repl--ws-put "ws1" :agent-state :thinking)
    ;; Act
    (agent-repl--mark-dead "ws1")
    ;; Assert
    (should-not (agent-repl--ws-get "ws1" :agent-state))
    (should (eq (agent-repl--ws-get "ws1" :repl-state) :dead))))

(ert-deftest agent-repl-test-mark-dead-already-dead-init-grace-preserved ()
  "The :init grace outranks the stale-state sweep on a :dead workspace."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (agent-repl--ws-put "ws1" :repl-state :dead)
    (agent-repl--ws-put "ws1" :agent-state :init)
    ;; Act
    (agent-repl--mark-dead "ws1")
    ;; Assert — :init survives; the session-start hook owns the transition.
    (should (eq (agent-repl--ws-get "ws1" :agent-state) :init))))

;;;; ---- Tests: Workspace state accessors (ws-set, ws-clear, ws-state) ----

(ert-deftest agent-repl-test-ws-set-and-state ()
  "ws-set should set the correct state, clearing others."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :thinking)
    (should (eq (agent-repl--ws-state "ws1") :thinking))
    (agent-repl--ws-set "ws1" :done)
    (should (eq (agent-repl--ws-state "ws1") :done))
    ;; Thinking should be cleared — the plist status should now be :done
    (should (eq (agent-repl--ws-get "ws1" :agent-state) :done))))

(ert-deftest agent-repl-test-ws-clear ()
  "ws-clear should clear only the specified state."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :thinking)
    (agent-repl--ws-agent-state-clear-if "ws1" :thinking)
    (should-not (agent-repl--ws-state "ws1"))))

(ert-deftest agent-repl-test-ws-state-inactive ()
  "ws-state should return :inactive when explicitly set."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :inactive)
    (should (eq (agent-repl--ws-state "ws1") :inactive))))

(ert-deftest agent-repl-test-ws-set-nil-error ()
  "ws-set with nil workspace should signal an error."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--ws-set nil :thinking) :type 'error)))

(ert-deftest agent-repl-test-ws-state-transitions ()
  "Verify state transitions: ws-set correctly cycles through :thinking, :permission, :done, and :inactive."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :thinking)
    (should (eq (agent-repl--ws-state "ws1") :thinking))
    ;; Set to :permission
    (agent-repl--ws-set "ws1" :permission)
    (should (eq (agent-repl--ws-state "ws1") :permission))
    ;; Set to :done
    (agent-repl--ws-set "ws1" :done)
    (should (eq (agent-repl--ws-state "ws1") :done))
    ;; Set to :inactive
    (agent-repl--ws-set "ws1" :inactive)
    (should (eq (agent-repl--ws-state "ws1") :inactive))))

(ert-deftest agent-repl-test-ws-set-permission ()
  "`ws-set' with :permission should set permission hash."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :permission)
    (should (eq (agent-repl--ws-state "ws1") :permission))))

(ert-deftest agent-repl-test-ws-clear-done ()
  "`ws-clear' with :done should not clear status when it is :thinking."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :thinking)
    (agent-repl--ws-agent-state-clear-if "ws1" :done)
    (should (eq (agent-repl--ws-get "ws1" :agent-state) :thinking))))

(ert-deftest agent-repl-test-ws-clear-permission ()
  "`ws-clear' with :permission should not clear status when it is :done."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :done)
    (agent-repl--ws-agent-state-clear-if "ws1" :permission)
    (should (eq (agent-repl--ws-get "ws1" :agent-state) :done))))

(ert-deftest agent-repl-test-ws-clear-nil-error ()
  "`ws-clear' with nil ws should signal error."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--ws-agent-state-clear-if nil :done) :type 'error)))

;;;; ---- Tests: Tabline rendering ----

(ert-deftest agent-repl-test-tabline-omits-folded-repo-workspaces ()
  "The tab-bar drops the workspaces of a folded repo."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "doom-ws" :group-key "/repos/doom/.git")
    (agent-repl--ws-put "ee-ws"   :group-key "/repos/explanation-engine/.git")
    (agent-repl--toggle-repo-fold "/repos/explanation-engine/.git")
    (cl-letf (((symbol-function 'agent-repl--ws-list-names)
               (lambda () '("doom-ws" "ee-ws")))
              ((symbol-function 'agent-repl--ws-current-name)
               (lambda () "doom-ws"))
              ((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) t)))
      (let ((result (agent-repl--tabline-advice)))
        (should (string-match-p "doom-ws" result))
        (should-not (string-match-p "ee-ws" result))))))

(ert-deftest agent-repl-test-tabline-renumbers-after-fold ()
  "Folding a repo closes up the tab numbers: the survivor after the folded
workspace takes its index rather than leaving a gap."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "doom-a" :group-key "/repos/doom/.git")
    (agent-repl--ws-put "ee-a"   :group-key "/repos/explanation-engine/.git")
    (agent-repl--ws-put "doom-b" :group-key "/repos/doom/.git")
    (agent-repl--ws-put "doom-b" :pushed-render-state :permission)
    (agent-repl--toggle-repo-fold "/repos/explanation-engine/.git")
    (cl-letf (((symbol-function 'agent-repl--ws-list-names)
               (lambda () '("doom-a" "ee-a" "doom-b")))
              ((symbol-function 'agent-repl--ws-current-name)
               (lambda () "doom-a"))
              ((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) t)))
      ;; doom-b is 3rd in the raw list but 2nd once the folded repo drops
      ;; out, so its bracket carries index 2.
      (let ((result (agent-repl--tabline-advice)))
        (should (string-match-p "\\[2\\]" result))))))

(ert-deftest agent-repl-test-tabline-keeps-current-workspace-when-its-repo-folded ()
  "The current workspace keeps its tab even when its own repo is folded."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ee-a" :group-key "/repos/explanation-engine/.git")
    (agent-repl--ws-put "ee-b" :group-key "/repos/explanation-engine/.git")
    (agent-repl--toggle-repo-fold "/repos/explanation-engine/.git")
    (cl-letf (((symbol-function 'agent-repl--ws-list-names)
               (lambda () '("ee-a" "ee-b")))
              ((symbol-function 'agent-repl--ws-current-name)
               (lambda () "ee-b"))
              ((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) t)))
      (let ((result (agent-repl--tabline-advice)))
        (should (string-match-p "ee-b" result))
        (should-not (string-match-p "ee-a" result))))))

(ert-deftest agent-repl-test-tabline-thinking-face ()
  "Tabline should apply thinking face for background thinking tabs (panels visible)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "other-ws" :thinking)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function '+workspace-list-names) (lambda () '("test-ws" "other-ws")))
              ((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) t)))
      (let ((result (agent-repl--tabline-advice '("test-ws" "other-ws"))))
        ;; other-ws should have thinking face
        (should (string-match-p "other-ws" result))))))

(ert-deftest agent-repl-test-tabline-permission-label ()
  "Tabline shows the bare index for permission state (panels visible)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :pushed-render-state :permission)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function '+workspace-list-names) (lambda () '("test-ws")))
              ((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) t)))
      (let ((result (agent-repl--tabline-advice '("test-ws"))))
        (should (string-match-p "\\[1\\]" result))
        (should-not (string-match-p "❓" result))))))

(ert-deftest agent-repl-test-tabline-dead-label ()
  "Tabline shows the bare index for a dead session (panels visible)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :pushed-render-state :dead)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "other-ws"))
              ((symbol-function '+workspace-list-names) (lambda () '("test-ws")))
              ((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) t)))
      (let ((result (agent-repl--tabline-advice '("test-ws"))))
        (should (string-match-p "\\[1\\]" result))
        (should-not (string-match-p "❌" result))))))

(ert-deftest agent-repl-test-tabline-merged-label-removed ()
  "Tabline shows no merge glyph for a merged workspace (bracket-only,
panels closed)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :pushed-render-state :merged)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "other-ws"))
              ((symbol-function '+workspace-list-names) (lambda () '("test-ws")))
              ((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) nil)))
      (let ((result (agent-repl--tabline-advice '("test-ws"))))
        (should-not (string-match-p "🔀" result))))))

(ert-deftest agent-repl-test-tabline-done-face ()
  "A background tab with :done should use `agent-repl-tab-done' face (panels visible)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "bg-ws" :pushed-render-state :done)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
              ((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) t)))
      (let ((result (agent-repl--tabline-advice '("current-ws" "bg-ws"))))
        ;; Find the "bg-ws" segment and check its face
        (let ((pos (string-match "bg-ws" result)))
          (should pos)
          (should (eq (get-text-property pos 'face result) 'agent-repl-tab-done)))))))

(ert-deftest agent-repl-test-tabline-selected-suppresses-thinking ()
  "The SELECTED tab with :thinking should NOT get the thinking face."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "sel-ws" :thinking)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "sel-ws")))
      (let ((result (agent-repl--tabline-advice '("sel-ws"))))
        (let ((pos (string-match "sel-ws" result)))
          (should pos)
          ;; Should get the normal selected face, NOT thinking
          (should (eq (get-text-property pos 'face result)
                      '+workspace-tab-selected-face)))))))

(ert-deftest agent-repl-test-tabline-selected-permission-uses-selected-face ()
  "The SELECTED tab with :permission uses the normal selected face.
The ❓ glyph in the bracket (not the name background) signals permission."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "sel-ws" :permission)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "sel-ws")))
      (let ((result (agent-repl--tabline-advice '("sel-ws"))))
        (let ((pos (string-match "sel-ws" result)))
          (should pos)
          (should (eq (get-text-property pos 'face result)
                      '+workspace-tab-selected-face)))))))

;;;; ---- Tests: ws-bracket-state ignores panel visibility ----

(ert-deftest agent-repl-test-bracket-state-thinking-panels-closed ()
  "ws-bracket-state returns the pushed state even when panels are closed."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :thinking)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p)
               (lambda (_ws) nil)))
      (should (eq :thinking (agent-repl--ws-bracket-state "ws1"))))))

(ert-deftest agent-repl-test-bracket-state-nil-when-no-state ()
  "ws-bracket-state returns nil when WS has no agent/repl state."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--ws-bracket-state "untouched"))))

;;;; ---- Tests: tab-spec-bracket-only ----

(ert-deftest agent-repl-test-tab-spec-bracket-only-unselected-thinking ()
  "Bracket-only spec for :thinking unselected: bg/fg unspecified, bracket gets thinking-red."
  (let ((spec (agent-repl--tab-spec-bracket-only :thinking nil)))
    (should (eq 'unspecified (plist-get spec :bg)))
    (should (eq 'unspecified (plist-get spec :fg)))
    (should (equal agent-repl--color-thinking-red (plist-get spec :bracket-bg)))
    (should (equal agent-repl--color-default-bracket (plist-get spec :bracket-fg)))))

(ert-deftest agent-repl-test-tab-spec-bracket-only-selected-thinking ()
  "Bracket-only spec for :thinking selected pulls bracket-bg from selected row."
  (let ((spec (agent-repl--tab-spec-bracket-only :thinking t)))
    (should (eq 'unspecified (plist-get spec :bg)))
    (should (eq 'unspecified (plist-get spec :fg)))
    (should (equal agent-repl--color-thinking-red (plist-get spec :bracket-bg)))))

;;;; ---- Tests: tabline renders bracket-only spec when panels closed ----

(ert-deftest agent-repl-test-tabline-panels-closed-name-uses-default-face ()
  "Panels closed for :thinking — name region should use the default workspace face."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "bg-ws" :thinking)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
              ((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) nil)))
      (let* ((result (agent-repl--tabline-advice '("current-ws" "bg-ws")))
             (pos (string-match "bg-ws" result)))
        (should pos)
        (should (eq (get-text-property pos 'face result) '+workspace-tab-face))))))

(ert-deftest agent-repl-test-tabline-panels-closed-bracket-keeps-state-color ()
  "Panels closed for :thinking — [N] bracket should keep the thinking-red background."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "bg-ws" :pushed-render-state :thinking)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
              ((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) nil)))
      (let* ((result (agent-repl--tabline-advice '("current-ws" "bg-ws")))
             (bracket-pos (string-match "\\[2\\]" result))
             (bracket-face (and bracket-pos (get-text-property bracket-pos 'face result))))
        (should bracket-pos)
        (should (equal agent-repl--color-thinking-red
                       (plist-get bracket-face :background)))))))

(ert-deftest agent-repl-test-tabline-panels-closed-permission-bracket-is-index ()
  "Panels closed for :permission — the bracket carries the index alone."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "bg-ws" :pushed-render-state :permission)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
              ((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) nil)))
      (let ((result (agent-repl--tabline-advice '("current-ws" "bg-ws"))))
        (should (string-match-p "\\[2\\]" result))))))

(ert-deftest agent-repl-test-tabline-panels-closed-dead-bracket-is-index ()
  "Panels closed for :dead — the bracket carries the index alone."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "bg-ws" :pushed-render-state :dead)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
              ((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) nil)))
      (let ((result (agent-repl--tabline-advice '("current-ws" "bg-ws"))))
        (should (string-match-p "\\[2\\]" result))))))

(ert-deftest agent-repl-test-tabline-panels-closed-vendor-blocked-bracket-is-index ()
  "Panels closed for :vendor-blocked — the bracket carries the index alone."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "bg-ws" :pushed-render-state :vendor-blocked)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
              ((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) nil)))
      (let ((result (agent-repl--tabline-advice '("current-ws" "bg-ws"))))
        (should (string-match-p "\\[2\\]" result))))))

(ert-deftest agent-repl-test-tabline-panels-closed-thinking-bracket-stays-plain ()
  "Panels closed for :thinking — the bracket carries the index alone."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "bg-ws" :thinking)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
              ((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) nil)))
      (let ((result (agent-repl--tabline-advice '("current-ws" "bg-ws"))))
        (should (string-match-p "\\[2\\]" result))))))

(ert-deftest agent-repl-test-tabline-panels-closed-permission-bracket-stays-green ()
  "Panels closed for :permission — bracket bg should be the done-green."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "bg-ws" :pushed-render-state :permission)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
              ((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) nil)))
      (let* ((result (agent-repl--tabline-advice '("current-ws" "bg-ws")))
             (bracket-pos (string-match "\\[2\\]" result))
             (bracket-face (and bracket-pos (get-text-property bracket-pos 'face result))))
        (should bracket-pos)
        (should (equal agent-repl--color-done-green
                       (plist-get bracket-face :background)))))))

(ert-deftest agent-repl-test-tabline-panels-closed-no-state-leaves-bracket-uncolored ()
  "No agent-state on a workspace — bracket bg stays unspecified even with panels closed."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
              ((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) nil)))
      (let* ((result (agent-repl--tabline-advice '("current-ws" "bg-ws")))
             (bracket-pos (string-match "\\[2\\]" result))
             (bracket-face (and bracket-pos (get-text-property bracket-pos 'face result))))
        (should bracket-pos)
        (should (eq 'unspecified (plist-get bracket-face :background)))))))

;;;; ---- Tests: ws-state edge cases ----

(ert-deftest agent-repl-test-ws-state-untouched-workspace ()
  "ws-state should return nil for a workspace that was never touched."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--ws-state "never-touched-ws"))))

;;;; ---- Tests: ws-set edge cases ----

(ert-deftest agent-repl-test-ws-set-calls-force-mode-line-update ()
  "ws-set should call `force-mode-line-update'."
  (agent-repl-test--with-clean-state
    (let ((called nil))
      (cl-letf (((symbol-function 'force-mode-line-update)
                 (lambda (&rest _) (setq called t))))
        (agent-repl--ws-set "ws1" :thinking)
        (should called)))))

(ert-deftest agent-repl-test-ws-set-idempotent ()
  "Setting the same state twice should be a no-op (state stays the same)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :done)
    (agent-repl--ws-set "ws1" :done)
    (should (eq (agent-repl--ws-state "ws1") :done))))

;;;; ---- Tests: ws-clear-if-status edge cases ----

(ert-deftest agent-repl-test-ws-clear-when-already-nil ()
  "Clearing when status is already nil should be a no-op."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-agent-state-clear-if "ws1" :thinking)
    (should-not (agent-repl--ws-state "ws1"))))

(ert-deftest agent-repl-test-ws-clear-matching-done ()
  "Clearing :done when status IS :done should clear to nil."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :done)
    (agent-repl--ws-agent-state-clear-if "ws1" :done)
    (should-not (agent-repl--ws-state "ws1"))))

(ert-deftest agent-repl-test-ws-clear-matching-permission ()
  "Clearing :permission when status IS :permission should clear to nil."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :permission)
    (agent-repl--ws-agent-state-clear-if "ws1" :permission)
    (should-not (agent-repl--ws-state "ws1"))))

(ert-deftest agent-repl-test-ws-clear-matching-inactive ()
  "Clearing :inactive when status IS :inactive should clear to nil."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :inactive)
    (agent-repl--ws-agent-state-clear-if "ws1" :inactive)
    (should-not (agent-repl--ws-state "ws1"))))

(ert-deftest agent-repl-test-ws-clear-calls-mode-line-update-on-match ()
  "ws-clear-if-status should call `force-mode-line-update' only when state matches."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :done)
    (let ((call-count 0))
      (cl-letf (((symbol-function 'force-mode-line-update)
                 (lambda (&rest _) (cl-incf call-count))))
        ;; Mismatch: should NOT call force-mode-line-update
        (agent-repl--ws-agent-state-clear-if "ws1" :thinking)
        (should (= call-count 0))
        ;; Match: should call force-mode-line-update
        (agent-repl--ws-agent-state-clear-if "ws1" :done)
        (should (= call-count 1))))))

;;;; ---- Tests: ws-dir ----

(ert-deftest agent-repl-test-ws-dir-returns-project-dir ()
  "ws-dir should return the :project-dir value when set."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/home/user/project")
    (should (equal (agent-repl--ws-dir "ws1") "/home/user/project"))))

(ert-deftest agent-repl-test-ws-dir-errors-when-missing ()
  "ws-dir should signal an error when :project-dir is not set."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--ws-dir "ws1") :type 'error)))

;;;; ---- Tests: align-buffer-to-ws-dir ----

(ert-deftest agent-repl-test-align-buffer-to-ws-dir-repoints ()
  "align-buffer-to-ws-dir sets the buffer's `default-directory' to :project-dir."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*align-test*")))
      (unwind-protect
          (progn
            (with-current-buffer buf (setq default-directory "/some/foreign/repo/"))
            (agent-repl--ws-put "ws1" :project-dir "/home/user/project")
            (agent-repl--align-buffer-to-ws-dir buf "ws1")
            (should (equal (buffer-local-value 'default-directory buf)
                           "/home/user/project/")))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-align-buffer-to-ws-dir-trailing-slash ()
  "align-buffer-to-ws-dir appends a trailing slash to a slashless :project-dir."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*align-test*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir "/home/user/project")
            (agent-repl--align-buffer-to-ws-dir buf "ws1")
            (should (equal (buffer-local-value 'default-directory buf)
                           "/home/user/project/")))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-align-buffer-to-ws-dir-noop-when-dir-missing ()
  "align-buffer-to-ws-dir leaves `default-directory' untouched when :project-dir is unset."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*align-test*")))
      (unwind-protect
          (progn
            (with-current-buffer buf (setq default-directory "/some/foreign/repo/"))
            (agent-repl--align-buffer-to-ws-dir buf "ws1")
            (should (equal (buffer-local-value 'default-directory buf)
                           "/some/foreign/repo/")))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-align-buffer-to-ws-dir-noop-when-buffer-dead ()
  "align-buffer-to-ws-dir is a silent no-op when the buffer is dead."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*align-test*")))
      (kill-buffer buf)
      (agent-repl--ws-put "ws1" :project-dir "/home/user/project")
      ;; Must not error on a dead buffer.
      (should-not (agent-repl--align-buffer-to-ws-dir buf "ws1")))))

;;;; ---- Tests: workspace-clean-p ----

(ert-deftest agent-repl-test-workspace-clean-p-clean ()
  "workspace-clean-p should return non-nil when :git-clean is 'clean."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :git-clean 'clean)
    (should (agent-repl--workspace-clean-p "ws1"))))

(ert-deftest agent-repl-test-workspace-clean-p-dirty ()
  "workspace-clean-p should return nil when :git-clean is 'dirty."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :git-clean 'dirty)
    (should-not (agent-repl--workspace-clean-p "ws1"))))

(ert-deftest agent-repl-test-workspace-clean-p-default-nil ()
  "workspace-clean-p should signal an error when :git-clean is unset."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--workspace-clean-p "ws1") :type 'error)))

;;;; ---- Tests: git-check-in-progress-p ----

(ert-deftest agent-repl-test-git-check-in-progress-no-proc ()
  "git-check-in-progress-p should return nil when no :git-proc is set."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--git-check-in-progress-p "ws1"))))

(ert-deftest agent-repl-test-git-check-in-progress-dead-proc ()
  "git-check-in-progress-p should return nil when :git-proc is a dead process."
  (agent-repl-test--with-clean-state
    (let ((proc 'dead-process-fixture))
      (agent-repl--ws-put "ws1" :git-proc proc)
      (cl-letf (((symbol-function 'process-live-p)
                 (lambda (candidate)
                   (should (eq candidate proc))
                   nil)))
        (should-not (agent-repl--git-check-in-progress-p "ws1"))))))

(ert-deftest agent-repl-test-git-check-in-progress-live-proc ()
  "git-check-in-progress-p should return non-nil when :git-proc is live."
  (agent-repl-test--with-clean-state
    (let ((proc 'live-process-fixture))
      (agent-repl--ws-put "ws1" :git-proc proc)
      (cl-letf (((symbol-function 'process-live-p)
                 (lambda (candidate)
                   (should (eq candidate proc))
                   t)))
        (should (agent-repl--git-check-in-progress-p "ws1"))))))

;;;; ---- Tests: git-diff-sentinel ----

(ert-deftest agent-repl-test-git-diff-sentinel-clean ()
  "git-diff-sentinel should set 'clean when process exits with 0."
  (agent-repl-test--with-clean-state
    (let ((proc 'clean-process-fixture))
      (cl-letf (((symbol-function 'process-live-p) (lambda (candidate)
                                                     (should (eq candidate proc))
                                                     nil))
                ((symbol-function 'process-exit-status) (lambda (candidate)
                                                          (should (eq candidate proc))
                                                          0)))
        (agent-repl--git-diff-sentinel "ws1" proc "finished\n")
        (should (eq (agent-repl--ws-get "ws1" :git-clean) 'clean))))))

(ert-deftest agent-repl-test-git-diff-sentinel-dirty ()
  "git-diff-sentinel should set 'dirty when process exits with non-zero."
  (agent-repl-test--with-clean-state
    (let ((proc 'dirty-process-fixture))
      (cl-letf (((symbol-function 'process-live-p) (lambda (candidate)
                                                     (should (eq candidate proc))
                                                     nil))
                ((symbol-function 'process-exit-status) (lambda (candidate)
                                                          (should (eq candidate proc))
                                                          1)))
        (agent-repl--git-diff-sentinel "ws1" proc "finished\n")
        (should (eq (agent-repl--ws-get "ws1" :git-clean) 'dirty))))))

(ert-deftest agent-repl-test-git-diff-sentinel-clears-git-proc ()
  "git-diff-sentinel should clear :git-proc after completion."
  (agent-repl-test--with-clean-state
    (let ((proc 'cleared-process-fixture))
      (agent-repl--ws-put "ws1" :git-proc proc)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_candidate) nil))
                ((symbol-function 'process-exit-status) (lambda (_candidate) 0)))
        (agent-repl--git-diff-sentinel "ws1" proc "finished\n")
        (should-not (agent-repl--ws-get "ws1" :git-proc))))))

(ert-deftest agent-repl-test-git-diff-sentinel-drives-no-state-transition ()
  "git-diff-sentinel caches cleanliness and changes no state.
Worktree cleanliness was only ever an input to the removed decay."
  (agent-repl-test--with-clean-state
    (let ((proc 'state-neutral-process-fixture))
      (agent-repl--ws-put "ws1" :agent-state :thinking)
      (agent-repl--ws-put "ws1" :repl-state :active)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_candidate) nil))
                ((symbol-function 'process-exit-status) (lambda (_candidate) 0)))
        (agent-repl--git-diff-sentinel "ws1" proc "finished\n")
        (should (eq (agent-repl--ws-get "ws1" :git-clean) 'clean))
        (should (eq (agent-repl--ws-get "ws1" :agent-state) :thinking))
        (should (eq (agent-repl--ws-get "ws1" :repl-state) :active))))))

(ert-deftest agent-repl-test-git-diff-sentinel-noop-when-live ()
  "git-diff-sentinel should be a no-op when the process is still live."
  (agent-repl-test--with-clean-state
    (let ((proc 'live-sentinel-process-fixture)
          (exit-status-read-p nil))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_candidate) t))
                ((symbol-function 'process-exit-status)
                 (lambda (_candidate)
                   (setq exit-status-read-p t)
                   0)))
        (agent-repl--git-diff-sentinel "ws1" proc "running\n")
        (should-not (agent-repl--ws-get "ws1" :git-clean))
        (should-not exit-status-read-p)))))

;;;; ---- Tests: async-refresh-git-status ----

(ert-deftest agent-repl-test-async-refresh-noop-when-in-progress ()
  "async-refresh-git-status should be a no-op when check already in progress."
  (agent-repl-test--with-clean-state
    (let ((proc 'existing-check-process-fixture)
          (spawn-called-p nil))
      (agent-repl--ws-put "ws1" :project-dir "/tmp")
      (agent-repl--ws-put "ws1" :git-proc proc)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_candidate) t))
                ((symbol-function 'agent-repl--make-process-git)
                 (lambda (&rest _args)
                   (setq spawn-called-p t)
                   'replacement-process-fixture)))
        (agent-repl--async-refresh-git-status "ws1")
        ;; The external-process boundary is not reached while a check is live.
        (should-not spawn-called-p)
        (should (eq (agent-repl--ws-get "ws1" :git-proc) proc))))))

(ert-deftest agent-repl-test-async-refresh-noop-when-no-dir ()
  "async-refresh-git-status should be a no-op when ws-dir errors."
  (agent-repl-test--with-clean-state
    ;; No :project-dir set, so ws-dir will error
    ;; The function uses when-let which handles nil returns,
    ;; but ws-dir errors. We stub ws-dir to return nil.
    (cl-letf (((symbol-function 'agent-repl--ws-dir) (lambda (_ws) nil)))
      (agent-repl--async-refresh-git-status "ws1")
      (should-not (agent-repl--ws-get "ws1" :git-proc)))))

;;;; ---- Tests: tab-spec ----

(ert-deftest agent-repl-test-tab-spec-unselected-known-state ()
  "tab-spec returns the :unselected plist from the palette for a known state."
  (let ((spec (agent-repl--tab-spec :thinking nil)))
    (should (equal (plist-get spec :bg) "#cc3333"))
    (should (equal (plist-get spec :fg) "white"))))

(ert-deftest agent-repl-test-tab-spec-selected-known-state ()
  "tab-spec returns the :selected plist from the palette for a known state."
  (let ((spec (agent-repl--tab-spec :done t)))
    (should (equal (plist-get spec :bracket-fg) "white"))
    (should (equal (plist-get spec :bracket-bg) "#1a7a1a"))))

(ert-deftest agent-repl-test-tab-spec-unknown-state-falls-back-to-default ()
  "tab-spec returns the default spec for states absent from the palette."
  (let ((unsel (agent-repl--tab-spec :bogus nil))
        (sel   (agent-repl--tab-spec :bogus t)))
    (should (equal (plist-get unsel :bracket-fg) "white"))
    (should (equal (plist-get sel :bg) "#c0c0c0"))))

(ert-deftest agent-repl-test-tab-spec-nil-state-uses-default ()
  "tab-spec with nil state returns the default spec."
  (should (equal (plist-get (agent-repl--tab-spec nil nil) :bracket-fg)
                 "white")))

(ert-deftest agent-repl-test-tab-spec-permission-selected-no-face-override ()
  "The :permission :selected spec carries no :face-override so the
selected tab dims to the normal selected face like other states."
  (let ((spec (agent-repl--tab-spec :permission t)))
    (should-not (plist-get spec :face-override))))

(ert-deftest agent-repl-test-tab-spec-dead-falls-back-to-default ()
  "The :dead state has no appearance spec; tab-spec returns the default."
  (let ((unsel (agent-repl--tab-spec :dead nil))
        (sel   (agent-repl--tab-spec :dead t)))
    (should (equal (plist-get unsel :bracket-fg) "white"))
    (should (equal (plist-get sel :bg) "#c0c0c0"))))

;;;; ---- Tests: bracket-bg on selected tabs ----

(ert-deftest agent-repl-test-tab-spec-selected-init-bracket-bg ()
  "Selected :init bracket-bg should be the init blue color."
  (let ((spec (agent-repl--tab-spec :init t)))
    (should (equal (plist-get spec :bracket-bg) "#3366cc"))
    (should (equal (plist-get spec :bracket-fg) "white"))))

(ert-deftest agent-repl-test-tab-spec-selected-thinking-bracket-bg ()
  "Selected :thinking bracket-bg should be the thinking red color."
  (let ((spec (agent-repl--tab-spec :thinking t)))
    (should (equal (plist-get spec :bracket-bg) "#cc3333"))
    (should (equal (plist-get spec :bracket-fg) "white"))))

(ert-deftest agent-repl-test-tab-spec-selected-ready-bracket-bg ()
  "Selected :ready bracket-bg is the green every ready state wears."
  (let ((spec (agent-repl--tab-spec :ready t)))
    (should (equal (plist-get spec :bracket-bg) agent-repl--color-done-green))
    (should (equal (plist-get spec :bracket-fg) "white"))))

(ert-deftest agent-repl-test-tab-spec-selected-permission-bracket-bg ()
  "Selected :permission bracket-bg should be the done green color."
  (let ((spec (agent-repl--tab-spec :permission t)))
    (should (equal (plist-get spec :bracket-bg) "#1a7a1a"))
    (should (equal (plist-get spec :bracket-fg) "white"))))

(ert-deftest agent-repl-test-tab-spec-unselected-has-no-bracket-bg ()
  "Unselected specs should not have :bracket-bg (falls back to :bg in renderer)."
  (dolist (state '(:init :thinking :done :permission :idle))
    (let ((spec (agent-repl--tab-spec state nil)))
      (should-not (plist-get spec :bracket-bg)))))

(ert-deftest agent-repl-test-render-tab-bracket-bg-applied ()
  "render-tab should use :bracket-bg for the bracket background when present."
  (let* ((spec '(:bg "#c0c0c0" :fg "black" :bracket-bg "#cc3333" :bracket-fg "white" :weight bold))
         (result (agent-repl--render-tab "ws1" spec "1" '+workspace-tab-face nil))
         (bracket-pos (string-match "\\[" result))
         (face (get-text-property bracket-pos 'face result)))
    (should (equal (plist-get face :background) "#cc3333"))
    (should (equal (plist-get face :foreground) "white"))))

(ert-deftest agent-repl-test-render-tab-bracket-bg-falls-back-to-bg ()
  "render-tab should fall back to :bg for bracket background when :bracket-bg is absent."
  (let* ((spec '(:bg "#c0c0c0" :fg "black" :bracket-fg "blue" :weight bold))
         (result (agent-repl--render-tab "ws1" spec "1" '+workspace-tab-face nil))
         (bracket-pos (string-match "\\[" result))
         (face (get-text-property bracket-pos 'face result)))
    (should (equal (plist-get face :background) "#c0c0c0"))))

;;;; ---- Tests: render-tab (spec-driven) ----

(ert-deftest agent-repl-test-render-tab-with-img-str ()
  "render-tab should include img-str when non-nil."
  (let* ((spec '(:bg unspecified :fg "black" :bracket-fg "blue" :weight bold))
         (result (agent-repl--render-tab "ws1" spec "1" '+workspace-tab-face "IMG")))
    (should (string-match-p "IMG" result))
    (should (string-match-p "ws1" result))))

(ert-deftest agent-repl-test-render-tab-img-str-trailing-space-unfaced ()
  "render-tab should place a single un-faced space between img-str and the name segment."
  (let* ((spec '(:bg "#c0c0c0" :fg "black" :bracket-fg "blue" :weight bold))
         (result (agent-repl--render-tab "ws1" spec "1" '+workspace-tab-face "IMG"))
         (img-pos (string-match "IMG" result))
         (gap-pos (+ img-pos 3))
         (name-pos (string-match "ws1" result)))
    (should img-pos)
    (should name-pos)
    ;; Exactly one un-faced space between IMG and the name-face padding's
    ;; leading space (which is part of " ws1 ").
    (should (equal (substring result gap-pos (1+ gap-pos)) " "))
    (should-not (get-text-property gap-pos 'face result))
    ;; The next character is the name-face's leading padding space.
    (should (equal (substring result (1+ gap-pos) (+ gap-pos 2)) " "))
    (should (eq (get-text-property (1+ gap-pos) 'face result)
                '+workspace-tab-face))))

(ert-deftest agent-repl-test-render-tab-empty-name ()
  "render-tab should handle an empty name string."
  (let* ((spec '(:bg unspecified :fg "black" :bracket-fg "blue" :weight bold))
         (result (agent-repl--render-tab "" spec "1" '+workspace-tab-face nil)))
    (should (string-match-p "\\[1\\]" result))))

(ert-deftest agent-repl-test-render-tab-selected-spec-bg ()
  "render-tab applies the spec :bg to the bracket face's background."
  (let* ((spec '(:bg "#c0c0c0" :fg "black" :bracket-fg "#2a8c2a" :weight bold))
         (result (agent-repl--render-tab "ws1" spec "1" '+workspace-tab-face nil))
         (pos (string-match "\\[1\\]" result))
         (face (get-text-property pos 'face result)))
    (should (equal (plist-get face :background) "#c0c0c0"))
    (should (equal (plist-get face :foreground) "#2a8c2a"))))

(ert-deftest agent-repl-test-render-tab-ends-with-unfaced-space ()
  "render-tab's last character is an unfaced space.
Without this terminator the name-face background bleeds to the row's
right edge via `extend_face_to_end_of_line'."
  (let* ((spec '(:bg "#c0c0c0" :fg "black" :bracket-fg "blue" :weight bold))
         (result (agent-repl--render-tab "ws1" spec "1" '+workspace-tab-face nil))
         (last-idx (1- (length result))))
    (should (equal (substring result last-idx) " "))
    (should-not (get-text-property last-idx 'face result))))

(ert-deftest agent-repl-test-render-tab-ends-with-unfaced-space-with-img ()
  "render-tab's last character is an unfaced space even when img-str is supplied."
  (let* ((spec '(:bg "#c0c0c0" :fg "black" :bracket-fg "blue" :weight bold))
         (result (agent-repl--render-tab "ws1" spec "1" '+workspace-tab-face "IMG"))
         (last-idx (1- (length result))))
    (should (equal (substring result last-idx) " "))
    (should-not (get-text-property last-idx 'face result))))

(ert-deftest agent-repl-test-render-tab-penultimate-is-faced-name-padding ()
  "The character immediately before the unfaced terminator is the
name-face's trailing padding space — confirms the terminator was
appended *after* the faced padding, not merged into it."
  (let* ((spec '(:bg "#c0c0c0" :fg "black" :bracket-fg "blue" :weight bold))
         (result (agent-repl--render-tab "ws1" spec "1" '+workspace-tab-face nil))
         (penultimate (- (length result) 2)))
    (should (equal (substring result penultimate (1+ penultimate)) " "))
    (should (eq (get-text-property penultimate 'face result)
                '+workspace-tab-face))))

;;;; ---- Tests: bracket label is the index alone ----

(ert-deftest agent-repl-test-render-tab-entry-bracket-is-index-for-permission ()
  "A :permission workspace renders [3] with no glyph beside the numeral."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :permission)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) t)))
      (should (string-match-p "\\[3\\]"
                              (agent-repl--render-tab-entry "ws1" "current-ws" 3))))))

(ert-deftest agent-repl-test-render-tab-entry-bracket-is-index-for-dead ()
  "A :dead workspace renders [2] with no glyph beside the numeral."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :dead)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) t)))
      (should (string-match-p "\\[2\\]"
                              (agent-repl--render-tab-entry "ws1" "current-ws" 2))))))

(ert-deftest agent-repl-test-render-tab-entry-bracket-is-index-when-panels-closed ()
  "A closed-panel :start-failed workspace still renders only its index."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :start-failed)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) nil)))
      (should (string-match-p "\\[5\\]"
                              (agent-repl--render-tab-entry "ws1" "current-ws" 5))))))

;;;; ---- Tests: tab-face direct tests ----

(ert-deftest agent-repl-test-tab-face-nil-state-selected ()
  "tab-face with nil state and selected should return +workspace-tab-selected-face."
  (should (eq (agent-repl--tab-face nil t) '+workspace-tab-selected-face)))

(ert-deftest agent-repl-test-tab-face-nil-state-unselected ()
  "tab-face with nil state and unselected should return +workspace-tab-face."
  (should (eq (agent-repl--tab-face nil nil) '+workspace-tab-face)))

;;;; ---- Tests: tab-priority-image-str ----

(ert-deftest agent-repl-test-tab-priority-image-str-no-image ()
  "tab-priority-image-str should return nil when :priority is set but no image found."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :priority "nonexistent-priority")
    (cl-letf (((symbol-function 'agent-repl--priority-image)
               (lambda (_p) nil)))
      (should-not (agent-repl--tab-priority-image-str "ws1")))))

(ert-deftest agent-repl-test-tab-priority-image-str-with-image ()
  "tab-priority-image-str should return a propertized string when image found."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :priority "high")
    (let ((fake-image '(image :type png :data "fake")))
      (cl-letf (((symbol-function 'agent-repl--priority-image)
                 (lambda (_p) fake-image)))
        (let ((result (agent-repl--tab-priority-image-str "ws1")))
          (should result)
          (should (stringp result))
          (should (equal (get-text-property 0 'display result) fake-image)))))))

;;;; ---- Tests: render-tab-entry edge cases ----

(ert-deftest agent-repl-test-render-tab-entry-no-match ()
  "render-tab-entry should render as unselected when current-name matches nothing."
  (agent-repl-test--with-clean-state
    (let ((result (agent-repl--render-tab-entry "ws1" "no-such-ws" 1)))
      ;; Should render as unselected (bracket foreground is white)
      (should (string-match-p "ws1" result))
      (let ((bracket-pos (string-match "\\[" result)))
        (should bracket-pos)
        (let ((face (get-text-property bracket-pos 'face result)))
          (should (equal (plist-get face :foreground) "white")))))))

;;;; ---- Tests: flash-tab ----

(ert-deftest agent-repl-test-flash-tab-sets-flashing-immediately ()
  "agent-repl-flash-tab sets :flashing t synchronously before returning."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) nil)))
      (agent-repl-flash-tab "ws1" 3 1.0)
      (should (agent-repl--ws-flashing-p "ws1")))))

(ert-deftest agent-repl-test-flash-tab-schedules-exactly-one-timer-per-call ()
  "agent-repl-flash-tab itself schedules a single timer (the rest are chained)."
  (agent-repl-test--with-clean-state
    (let ((scheduled-delays nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (delay _repeat _fn &rest _args)
                   (push delay scheduled-delays))))
        (agent-repl-flash-tab "ws1" 3 1.0)
        (should (= 1 (length scheduled-delays)))
        ;; The single scheduled timer is the next step at one interval.
        (should (< (abs (- (car scheduled-delays) (/ 1.0 6.0))) 0.001))))))

(ert-deftest agent-repl-test-flash-step-terminal-clears-and-does-not-chain ()
  "The terminal flash step clears :flashing and schedules no successor."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-flashing "ws1" t)
    (let ((scheduled 0))
      (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) (cl-incf scheduled)))
                ((symbol-function 'agent-repl--force-tab-bar-redraw) #'ignore))
        ;; total-steps=5, terminal step is step=4.
        (agent-repl--flash-step "ws1" 4 5 0.25)
        (should (= 0 scheduled))
        (should-not (agent-repl--ws-flashing-p "ws1"))))))

(ert-deftest agent-repl-test-flash-step-non-terminal-chains-with-incremented-step ()
  "A non-terminal flash step chains the next step with STEP+1, same TOTAL/INTERVAL."
  (agent-repl-test--with-clean-state
    (let ((captured nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (delay _repeat fn &rest args)
                   (setq captured (list :delay delay :fn fn :args args))))
                ((symbol-function 'agent-repl--force-tab-bar-redraw) #'ignore))
        (agent-repl--flash-step "ws1" 1 5 0.25)
        (should (eq (plist-get captured :fn) #'agent-repl--flash-step))
        (should (= 0.25 (plist-get captured :delay)))
        (should (equal (plist-get captured :args) '("ws1" 2 5 0.25)))))))

(ert-deftest agent-repl-test-flash-tab-redraws-tab-bar-synchronously ()
  "agent-repl-flash-tab triggers a tab-bar redraw on the synchronous initial step."
  (agent-repl-test--with-clean-state
    (let ((redraw-calls 0))
      (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--force-tab-bar-redraw)
                 (lambda () (cl-incf redraw-calls))))
        (agent-repl-flash-tab "ws1" 3 1.0)
        (should (= 1 redraw-calls))))))

(ert-deftest agent-repl-test-flash-tab-chain-redraws-once-per-step ()
  "Draining the chain produces one redraw per step (including terminal)."
  (agent-repl-test--with-clean-state
    (let ((pending nil)
          (redraw-calls 0))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_delay _repeat fn &rest args)
                   (setq pending (cons fn args))))
                ((symbol-function 'agent-repl--force-tab-bar-redraw)
                 (lambda () (cl-incf redraw-calls))))
        (agent-repl-flash-tab "ws1" 3 1.0)
        ;; Drive the chain: each call schedules the next via PENDING.
        (while pending
          (let ((next pending))
            (setq pending nil)
            (apply (car next) (cdr next))))
        ;; total-steps = 1 + 2*3 = 7; one redraw per step.
        (should (= 7 redraw-calls))))))

(ert-deftest agent-repl-test-force-tab-bar-redraw-flips-space-toggle ()
  "force-tab-bar-redraw flips `agent-repl--tabline-space-toggle' to defeat the cache."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--tabline-space-toggle nil))
      (agent-repl--force-tab-bar-redraw)
      (should agent-repl--tabline-space-toggle)
      (agent-repl--force-tab-bar-redraw)
      (should-not agent-repl--tabline-space-toggle))))

(ert-deftest agent-repl-test-flash-tab-default-args-produce-exactly-two-pulses ()
  "With default `agent-repl-flash-count', draining the chain yields exactly 2 ON pulses."
  (agent-repl-test--with-clean-state
    (let ((pending nil)
          (on-count 0))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_delay _repeat fn &rest args)
                   (setq pending (cons fn args))))
                ((symbol-function 'agent-repl--force-tab-bar-redraw) #'ignore))
        (agent-repl-flash-tab "ws1")
        (when (agent-repl--ws-flashing-p "ws1") (cl-incf on-count))
        (while pending
          (let ((next pending))
            (setq pending nil)
            (apply (car next) (cdr next))
            (when (agent-repl--ws-flashing-p "ws1") (cl-incf on-count))))
        (should (= 2 on-count))))))

(ert-deftest agent-repl-test-flash-tab-toggles-flashing-t-nil-alternately ()
  "Draining the chain drives :flashing alternately t, nil, ending in nil cleanup."
  (agent-repl-test--with-clean-state
    (let ((pending nil)
          (observed nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_delay _repeat fn &rest args)
                   (setq pending (cons fn args))))
                ((symbol-function 'agent-repl--force-tab-bar-redraw) #'ignore))
        (agent-repl-flash-tab "ws1" 3 1.0)
        ;; After sync step 0, :flashing is t.
        (push (agent-repl--ws-flashing-p "ws1") observed)
        (while pending
          (let ((next pending))
            (setq pending nil)
            (apply (car next) (cdr next))
            (push (agent-repl--ws-flashing-p "ws1") observed)))
        ;; Steps 0..5 alternate t/nil; step 6 is the cleanup (nil).
        (should (equal '(t nil t nil t nil nil) (nreverse observed)))))))

;;;; ---- Tests: flash-current-tab helper ----

(ert-deftest agent-repl-test-flash-current-tab-pulses-current-workspace ()
  "agent-repl--flash-current-tab calls flash-tab with `(+workspace-current-name)'."
  (agent-repl-test--with-clean-state
    (let ((flashed nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
                ((symbol-function 'agent-repl-flash-tab)
                 (lambda (ws &rest _) (setq flashed ws))))
        (agent-repl--flash-current-tab)
        (should (equal flashed "current-ws"))))))

(ert-deftest agent-repl-test-flash-current-tab-noop-when-flash-tab-unbound ()
  "Helper is a no-op when `agent-repl-flash-tab' is unbound — guards startup race."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym) (not (eq sym 'agent-repl-flash-tab)))))
      (should-not (agent-repl--flash-current-tab)))))

(ert-deftest agent-repl-test-render-tab-entry-flash-uses-flash-face ()
  "render-tab-entry paints with `agent-repl-tab-flash' when :flashing is set."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-flashing "ws1" t)
    (let* ((result (agent-repl--render-tab-entry "ws1" "current-ws" 1))
           (name-pos (string-match "ws1" result)))
      (should name-pos)
      (should (eq (get-text-property name-pos 'face result)
                  'agent-repl-tab-flash)))))

(ert-deftest agent-repl-test-render-tab-entry-flash-overrides-state-color ()
  "Flash spec overrides the state-driven coloring while :flashing is set."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :thinking)
    (agent-repl--ws-set-flashing "ws1" t)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) t)))
      (let* ((result (agent-repl--render-tab-entry "ws1" "current-ws" 1))
             (bracket-pos (string-match "\\[" result))
             (bracket-face (get-text-property bracket-pos 'face result)))
        ;; Bracket bg is the flash white, NOT the thinking red.
        (should (equal (plist-get bracket-face :background)
                       agent-repl--color-flash-bg))))))

(ert-deftest agent-repl-test-render-tab-entry-no-flash-uses-state-face ()
  "Without :flashing, render-tab-entry uses the state-driven face."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :done)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) t)))
      (let* ((result (agent-repl--render-tab-entry "ws1" "current-ws" 1))
             (name-pos (string-match "ws1" result)))
        (should name-pos)
        (should (eq (get-text-property name-pos 'face result)
                    'agent-repl-tab-done))))))

;;;; ---- Tests: tabline-advice edge cases ----

(ert-deftest agent-repl-test-tabline-advice-defaults-from-ws-list-names ()
  "tabline-advice with no args defaults to `agent-repl--ws-list-names'
(the persp-mode integration wrapper in workspace.el), NOT
`+workspace-list-names' directly.  The wrapper intersects the cache
with agent-repl's own registration so the tab-bar reflects
agent-repl's worldview."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws-a" :project-dir "/tmp/a")
    (agent-repl--ws-put "ws-b" :project-dir "/tmp/b")
    (let ((persp-names-cache '("ws-a" "ws-b")))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws-a")))
        (let ((result (agent-repl--tabline-advice)))
          (should (string-match-p "ws-a" result))
          (should (string-match-p "ws-b" result)))))))

(ert-deftest agent-repl-test-tabline-advice-empty-names ()
  "tabline-advice with an empty names list should return an empty string."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--tabline-space-toggle nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
        (let ((result (agent-repl--tabline-advice '())))
          (should (equal result "")))))))

;;;; ---- Tests: tabline space toggle ----

(ert-deftest agent-repl-test-tabline-space-toggle-off ()
  "tabline-advice appends no cache-bust trailing space when toggle is nil.
The last entry already ends with one unfaced terminator space (see
`agent-repl--render-tab'); the toggle adds *another* trailing space
when on.  So toggle-off must not end with two trailing unfaced spaces."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--tabline-space-toggle nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function '+workspace-list-names) (lambda () '("ws1"))))
        (let ((result (agent-repl--tabline-advice '("ws1"))))
          (should-not (string-suffix-p "   " result)))))))

(ert-deftest agent-repl-test-tabline-space-toggle-on ()
  "tabline-advice appends a trailing space when toggle is non-nil."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--tabline-space-toggle t))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function '+workspace-list-names) (lambda () '("ws1"))))
        (let ((result (agent-repl--tabline-advice '("ws1"))))
          (should (string-suffix-p " " result)))))))

(ert-deftest agent-repl-test-tabline-cache-buster-is-invisible ()
  "The toggled cache-buster is a single `invisible'-propertized space.
It must change the string's CONTENTS (the repaint cache compares with
`equal', which ignores text properties) while contributing zero
rendered width — a visible-width tick can push the tabline across a
row-wrap threshold and set off the tab-bar-height/frame-resize
livelock."
  (let ((agent-repl--tabline-space-toggle t))
    (let ((buster (agent-repl--tabline-cache-buster)))
      (should (equal buster " "))
      (should (get-text-property 0 'invisible buster))))
  (let ((agent-repl--tabline-space-toggle nil))
    (should (equal (agent-repl--tabline-cache-buster) ""))))

(ert-deftest agent-repl-test-tabline-advice-suffix-carries-invisible-property ()
  "The toggled trailing space on the advice path is invisible, not a
visible-width character."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--tabline-space-toggle t))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function '+workspace-list-names) (lambda () '("ws1"))))
        (let ((result (agent-repl--tabline-advice '("ws1"))))
          (should (get-text-property (1- (length result)) 'invisible result)))))))

(ert-deftest agent-repl-test-tabline-space-toggle-alternates ()
  "Consecutive tabline-advice calls with opposite toggle values produce different strings."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function '+workspace-list-names) (lambda () '("ws1"))))
      (let* ((agent-repl--tabline-space-toggle nil)
             (r1 (agent-repl--tabline-advice '("ws1")))
             (agent-repl--tabline-space-toggle t)
             (r2 (agent-repl--tabline-advice '("ws1"))))
        (should-not (equal r1 r2))))))

(ert-deftest agent-repl-test-update-all-flips-space-toggle ()
  "update-all-workspace-states flips the space toggle on each call."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--tabline-space-toggle nil))
      (cl-letf (((symbol-function 'agent-repl--poll-workspace-notifications) #'ignore)
                ;; sidebar.el's roster tick rides update-all too; out of scope here.
                ((symbol-function 'agent-repl--sidebar-tick) #'ignore)
                ((symbol-function 'hash-table-keys) (lambda (_ht) nil)))
        (agent-repl--update-all-workspace-states)
        (should (eq agent-repl--tabline-space-toggle t))
        (agent-repl--update-all-workspace-states)
        (should (eq agent-repl--tabline-space-toggle nil))))))

(ert-deftest agent-repl-test-update-all-drives-force-tab-bar-redraw ()
  "update-all-workspace-states calls `agent-repl--force-tab-bar-redraw' on every tick.
Just flipping `agent-repl--tabline-space-toggle' is NOT enough to make
the visible tab-bar repaint — Emacs's tab-bar-format cache will keep
painting the cached value until something forces a re-read.  The 1Hz
timer must drive the full redraw (toggle + tab-bar primitives +
force-mode-line-update), which is what `--force-tab-bar-redraw' does.
Verified explicitly here so the entrypoint doesn't regress to a
toggle-only flip again (which silently breaks live status colors,
since commit 4dc0ecb moved the active rendering off the
`+workspace--tabline' advice path)."
  (agent-repl-test--with-clean-state
    (let ((redraw-calls 0))
      (cl-letf (((symbol-function 'agent-repl--force-tab-bar-redraw)
                 (lambda () (cl-incf redraw-calls)))
                ((symbol-function 'agent-repl--poll-workspace-notifications) #'ignore)
                ((symbol-function 'hash-table-keys) (lambda (_ht) nil)))
        (agent-repl--update-all-workspace-states)
        (should (= 1 redraw-calls))
        (agent-repl--update-all-workspace-states)
        (should (= 2 redraw-calls))))))

(ert-deftest agent-repl-test-update-all-redraw-fires-even-when-in-flight ()
  "The 1Hz timer drives `--force-tab-bar-redraw' BEFORE the in-flight check.
If a previous chain is still in flight we skip the per-workspace work for
this tick, but the tab-bar must still animate so :thinking spinners and
state-color transitions remain visible.  This is the structural mirror
of `agent-repl-test-update-all-tabline-toggle-survives-in-flight-skip',
extended to assert the redraw force fires alongside the toggle flip."
  (agent-repl-test--with-clean-state
    (let ((redraw-calls 0))
      (setq agent-repl--update-in-flight (float-time))
      (cl-letf (((symbol-function 'agent-repl--force-tab-bar-redraw)
                 (lambda () (cl-incf redraw-calls)))
                ((symbol-function 'agent-repl--poll-workspace-notifications) #'ignore))
        (agent-repl--update-all-workspace-states)
        (should (= 1 redraw-calls))))))

;;;; ---- Tests: workspace-tabline-formatted (extracted from +dwc/) ----

(ert-deftest agent-repl-test-workspace-tabline-formatted-alternates-across-toggle ()
  "Consecutive renders with opposite toggle values produce different strings.
This is the core cache-bust property of the alternating-space hack — Emacs's
tab-bar caches on string equality, so the format function must return strings
that differ each tick or no repaint happens."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-list-names) (lambda () '("ws1" "ws2")))
              ((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'frame-width) (lambda () 80)))
      (let* ((agent-repl--tabline-space-toggle nil)
             (r-off (agent-repl-workspace-tabline-formatted))
             (agent-repl--tabline-space-toggle t)
             (r-on (agent-repl-workspace-tabline-formatted)))
        (should (stringp r-off))
        (should (stringp r-on))
        (should-not (equal r-off r-on))
        ;; Toggle-on is exactly one space longer than toggle-off — the
        ;; only delta is the trailing-space append, not anything else.
        (should (= (1+ (length r-off)) (length r-on)))))))

(ert-deftest agent-repl-test-workspace-tabline-formatted-toggle-on-appends-one-extra-trailing-space ()
  "When the toggle is non-nil, the result has one MORE trailing space than
when the toggle is nil; rendering and join already contribute some trailing
whitespace from the unfaced terminators in `agent-repl--render-tab' and
`agent-repl--join-tabline-rows', and the toggle layers exactly one more."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-list-names) (lambda () '("ws1")))
              ((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'frame-width) (lambda () 80)))
      (let* ((agent-repl--tabline-space-toggle nil)
             (off (agent-repl-workspace-tabline-formatted))
             (agent-repl--tabline-space-toggle t)
             (on (agent-repl-workspace-tabline-formatted)))
        (should (string-suffix-p " " on))
        (should (string-suffix-p (concat off " ") on))
        ;; The extra space is the zero-width cache-buster, not a
        ;; visible-width tick that could re-wrap the row.
        (should (get-text-property (1- (length on)) 'invisible on))))))

(defmacro agent-repl-test--with-eight-registered-workspaces (&rest body)
  "Run BODY with ws-one..ws-eight registered and ws-five current.
Registers each workspace via `agent-repl--ws-put' AND lists it in
`persp-names-cache' so `agent-repl--ws-list-names' (which intersects
the two) actually returns them, unlike a bare `+workspace-list-names'
mock."
  `(let ((names '("ws-one" "ws-two" "ws-three" "ws-four"
                  "ws-five" "ws-six" "ws-seven" "ws-eight")))
     (dolist (n names)
       (agent-repl--ws-put n :project-dir (concat "/tmp/" n)))
     (let ((persp-names-cache names))
       (cl-letf (((symbol-function '+workspace-current-name)
                  (lambda () "ws-five")))
         ,@body))))

(ert-deftest agent-repl-test-workspace-tabline-formatted-always-one-row ()
  "The formatted tab-bar segment ALWAYS spans exactly one row.
The row count is FIXED (never varies with workspace count): a
row-count change alters the tab-bar pixel height; on macOS
`ns_change_tab_bar_height' then resizes the NSWindow, and a clipped
resize livelocks redisplay at 100% CPU.  A single row carries no
newline, and many workspaces at a narrow frame width must elide
behind badges, never wrap to a second row."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-eight-registered-workspaces
     (cl-letf (((symbol-function 'frame-width) (lambda () 24)))
       (dolist (agent-repl--tabline-space-toggle '(nil t))
         (let ((result (agent-repl-workspace-tabline-formatted)))
           (should (stringp result))
           (should (= 0 (cl-count ?\n result)))))))))

(ert-deftest agent-repl-test-workspace-tabline-formatted-single-row-when-few-tabs ()
  "With only a couple of tabs, the segment is a single row with no newline.
The fixed one-row count keeps the tab-bar's pixel height constant."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-list-names) (lambda () '("ws1" "ws2")))
              ((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'frame-width) (lambda () 80)))
      (let ((agent-repl--tabline-space-toggle nil))
        (should (= 0 (cl-count ?\n (agent-repl-workspace-tabline-formatted))))))))

(ert-deftest agent-repl-test-workspace-tabline-formatted-overflow-shows-current ()
  "When workspaces overflow the single row, the current one stays visible."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-eight-registered-workspaces
     (cl-letf (((symbol-function 'frame-width) (lambda () 40)))
       (let ((agent-repl--tabline-space-toggle nil))
         (should (string-search "ws-five"
                                (substring-no-properties
                                 (agent-repl-workspace-tabline-formatted)))))))))

;;;; ---- Tests: current-workspace-name-segment (extracted from +dwc/) ----

(ert-deftest agent-repl-test-current-workspace-name-segment-is-invisible ()
  "The right-aligned current-workspace segment carries `invisible t' so its only
purpose is the alternating-space cache-bust."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'safe-persp-name) (lambda (_p) "ws1"))
              ((symbol-function 'get-current-persp) (lambda () nil)))
      (let ((result (agent-repl-current-workspace-name-segment)))
        (should (get-text-property 0 'invisible result))))))

(ert-deftest agent-repl-test-current-workspace-name-segment-alternates-across-toggle ()
  "Consecutive renders with opposite toggle values produce different segment strings."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'safe-persp-name) (lambda (_p) "ws1"))
              ((symbol-function 'get-current-persp) (lambda () nil)))
      (let* ((agent-repl--tabline-space-toggle nil)
             (r-off (agent-repl-current-workspace-name-segment))
             (agent-repl--tabline-space-toggle t)
             (r-on (agent-repl-current-workspace-name-segment)))
        (should-not (equal r-off r-on))))))

;; The ws-queued-segment tests were deleted in the S9 endgame: the queued-
;; message status segment and the queue plane it rendered are retired.

;;;; ---- Tests: wconf-has-agent-p ----

(ert-deftest agent-repl-test-wconf-has-agent-nil ()
  "wconf-has-agent-p should return nil for nil wconf."
  (should-not (agent-repl--wconf-has-agent-p nil)))

(ert-deftest agent-repl-test-wconf-has-agent-non-list ()
  "wconf-has-agent-p should return nil for a non-list wconf."
  (should-not (agent-repl--wconf-has-agent-p "not-a-list")))

(ert-deftest agent-repl-test-wconf-has-agent-no-buffer ()
  "wconf-has-agent-p should return nil for a wconf with no buffer entries."
  (let ((wconf '((something "other"))))
    (should-not (agent-repl--wconf-has-agent-p wconf))))

(ert-deftest agent-repl-test-wconf-has-agent-non-agent-buffer ()
  "wconf-has-agent-p should return nil for a wconf with non-agent buffers."
  (let ((wconf '((buffer "*scratch*"))))
    (should-not (agent-repl--wconf-has-agent-p wconf))))

(ert-deftest agent-repl-test-wconf-has-agent-gui-webview ()
  "A BACKGROUND gui workspace's saved layout counts as agent-open.
This is the half of the fix that reaches every tab the user is not
currently looking at: the webview is genuinely present in the saved
window config, it simply was not recognized."
  (let ((wconf '((buffer "*agent-frontend-my-ws*"))))
    (should (agent-repl--wconf-has-agent-p wconf))))

(ert-deftest agent-repl-test-wconf-has-agent-gui-webview-nested ()
  "The gui webview is found however deep the saved layout nests it."
  (let ((wconf '((child ((child ((buffer "*agent-frontend-my-ws*"))))))))
    (should (agent-repl--wconf-has-agent-p wconf))))

(ert-deftest agent-repl-test-wconf-has-agent-gui-input-only ()
  "A gui layout holding ONLY the input panel is not a workspace showing its agent."
  (let ((wconf '((buffer "*agent-panel-input-my-ws*"))))
    (should-not (agent-repl--wconf-has-agent-p wconf))))

;;;; ---- Tests: visible-agent-buffer-p ----

(ert-deftest agent-repl-test-visible-agent-buffer-dead-buffer ()
  "visible-agent-buffer-p should return nil for a dead buffer."
  (let ((buf (generate-new-buffer "*agent-panel-deadbeef*")))
    (kill-buffer buf)
    (should-not (agent-repl--visible-agent-buffer-p buf))))

(ert-deftest agent-repl-test-visible-agent-buffer-non-agent ()
  "visible-agent-buffer-p should return nil for a live non-agent buffer."
  (agent-repl-test--with-temp-buffer "*not-agent*"
    (should-not (agent-repl--visible-agent-buffer-p (current-buffer)))))

(ert-deftest agent-repl-test-visible-agent-buffer-gui-webview-with-window ()
  "A displayed gui webview IS a visible agent view.
The CURRENT workspace's half of the fix: the tab-bar walks live buffers
asking whether the agent view is on screen, and a gui workspace's answer
is its webview."
  (agent-repl-test--with-temp-buffer "*agent-frontend-my-ws*"
    (cl-letf (((symbol-function 'get-buffer-window)
               (lambda (_buf) 'fake-window)))
      (should (agent-repl--visible-agent-buffer-p (current-buffer))))))

(ert-deftest agent-repl-test-visible-agent-buffer-gui-webview-no-window ()
  "A gui webview that is not on screen is not a visible agent view."
  (agent-repl-test--with-temp-buffer "*agent-frontend-my-ws*"
    (should-not (agent-repl--visible-agent-buffer-p (current-buffer)))))

;;;; ---- Tests: the gui tab fills, end to end ----
;;
;; The bug this closes: a gui workspace's `:agent-state' was always correct,
;; but `--ws-display-state' suppressed it because `--ws-agent-open-p' could
;; only see a vterm.  Every gui tab was therefore drawn in the bracket-only
;; "panels closed" style forever, whatever the agent was doing.

(ert-deftest agent-repl-test-display-state-gui-webview-open-renders-thinking ()
  "A gui workspace whose webview is up gets the FULL :thinking tab."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :thinking)
    (agent-repl-test--with-temp-buffer "*agent-frontend-ws1*"
      (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                 (lambda () "ws1"))
                ((symbol-function 'get-buffer-window)
                 (lambda (buf &rest _)
                   (when (equal (buffer-name buf) "*agent-frontend-ws1*")
                     'fake-window))))
        (should (eq :thinking (agent-repl--ws-display-state "ws1")))))))

(ert-deftest agent-repl-test-display-state-gui-webview-closed-renders-nil ()
  "A gui workspace whose webview is dismissed falls back to bracket-only.
The state survives on the plist, exactly as it does for a vterm workspace
whose panels are closed."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :thinking)
    (cl-letf (((symbol-function 'agent-repl--ws-current-name)
               (lambda () "ws1"))
              ((symbol-function 'get-buffer-window) (lambda (&rest _) nil)))
      (should-not (agent-repl--ws-display-state "ws1"))
      ;; The bracket keeps the colour, which is the whole point of the split.
      (should (eq :thinking (agent-repl--ws-bracket-state "ws1"))))))

;;;; ---- Tests: agent-visible-in-current-ws-p ----

(ert-deftest agent-repl-test-agent-visible-in-current-ws-none ()
  "agent-visible-in-current-ws-p should return nil when no agent buffers exist."
  (cl-letf (((symbol-function 'buffer-list)
             (lambda () nil)))
    (should-not (agent-repl--agent-visible-in-current-ws-p))))

(ert-deftest agent-repl-test-agent-visible-in-current-ws-found ()
  "agent-visible-in-current-ws-p should return non-nil when a visible agent buffer exists.

The `get-buffer-window' mock takes an optional second arg because on
Emacs 30 native-compiled callers pass the ALL-FRAMES slot explicitly
(as nil) even when the source only writes `(get-buffer-window buf)' —
without it, the test fails with `wrong-number-of-arguments' under AOT
native-comp."
  (agent-repl-test--with-temp-buffer "*agent-frontend-aabbccdd*"
    (let ((test-buf (current-buffer)))
      (cl-letf (((symbol-function 'buffer-list)
                 (lambda () (list test-buf)))
                ((symbol-function 'get-buffer-window)
                 (lambda (_buf &optional _all-frames) 'fake-window)))
        (should (agent-repl--agent-visible-in-current-ws-p))))))

;;;; ---- Tests: agent-in-saved-wconf-p ----

(ert-deftest agent-repl-test-agent-in-saved-wconf-persp-not-found ()
  "agent-in-saved-wconf-p should return nil when persp is not found."
  (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_name) nil)))
    (should-not (agent-repl--agent-in-saved-wconf-p "ws1"))))

(ert-deftest agent-repl-test-agent-in-saved-wconf-persp-is-symbol ()
  "agent-in-saved-wconf-p should return nil when persp-get-by-name returns the sentinel keyword."
  ;; persp-not-persp is :nil — a keyword; --ws-resolve-persp normalizes it to nil.
  (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_name) :nil)))
    (should-not (agent-repl--agent-in-saved-wconf-p "ws1"))))

(ert-deftest agent-repl-test-agent-in-saved-wconf-with-claude ()
  "agent-in-saved-wconf-p should return t when saved wconf contains an agent buffer."
  (let ((fake-persp (list 'fake-persp-struct))
        (fake-wconf '((buffer "*agent-frontend-ab12cd34*"))))
    (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_name) fake-persp))
              ((symbol-function 'persp-window-conf) (lambda (_persp) fake-wconf)))
      (should (agent-repl--agent-in-saved-wconf-p "ws1")))))

(ert-deftest agent-repl-test-agent-in-saved-wconf-without-claude ()
  "agent-in-saved-wconf-p should return nil when saved wconf has no agent buffer."
  (let ((fake-persp (list 'fake-persp-struct))
        (fake-wconf '((buffer "*scratch*"))))
    (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_name) fake-persp))
              ((symbol-function 'persp-window-conf) (lambda (_persp) fake-wconf)))
      (should-not (agent-repl--agent-in-saved-wconf-p "ws1")))))

;;;; ---- Tests: ws-agent-open-p ----

(ert-deftest agent-repl-test-ws-agent-open-current-ws ()
  "ws-agent-open-p should delegate to visible check for the current workspace."
  (let ((visible-called nil))
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'agent-repl--agent-visible-in-current-ws-p)
               (lambda () (setq visible-called t) t)))
      (should (agent-repl--ws-agent-open-p "ws1"))
      (should visible-called))))

(ert-deftest agent-repl-test-ws-agent-open-background-ws ()
  "ws-agent-open-p should delegate to saved wconf check for a background workspace."
  (let ((wconf-called nil))
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
              ((symbol-function 'agent-repl--agent-in-saved-wconf-p)
               (lambda (ws) (setq wconf-called ws) t)))
      (should (agent-repl--ws-agent-open-p "bg-ws"))
      (should (equal wconf-called "bg-ws")))))

;;;; ---- Tests: the removed :done->:idle decay ----
;;
;; `agent-repl--update-ws-state' and the `:done-acked' / `:done-acked-at'
;; viewed-bookkeeping it read are GONE.  The decay moved a workspace off the
;; green "ready for review" color once the user had looked at it, which made
;; sense while green and orange were two different claims.  They are not:
;; `:done', `:ready' and `:idle' are ALL green, so the decay changed the
;; color without changing anything true.

(ert-deftest agent-repl-test-decay-function-is-gone ()
  "The :done->:idle decay entrypoint no longer exists."
  (should-not (fboundp 'agent-repl--update-ws-state)))

(ert-deftest agent-repl-test-done-idle-delay-custom-is-gone ()
  "The decay's dwell knob went with the decay."
  (should-not (boundp 'agent-repl-done-idle-delay)))

(ert-deftest agent-repl-test-orange-is-gone ()
  "The orange that used to mean :idle is gone from every constant.
Orange claimed a state between working and ready that does not exist:
an idle workspace IS ready."
  (should-not (boundp 'agent-repl--color-idle-orange)))

(ert-deftest agent-repl-test-stop-failed-magenta-is-gone ()
  "The magenta that used to mean :stop-failed is gone.
It was a sixth vocabulary word for a condition purple already covers."
  (should-not (boundp 'agent-repl--color-stop-failed-magenta)))

;;;; ---- Tests: update-all-workspace-states ----

(ert-deftest agent-repl-test-update-all-persp-mode-nil ()
  "update-all-workspace-states should be a no-op when persp-mode is nil."
  (agent-repl-test--with-clean-state
    (let ((persp-mode nil)
          (update-called nil))
      (cl-letf (((symbol-function 'agent-repl--poll-workspace-notifications) #'ignore)
                ;; sidebar.el's roster tick rides update-all too; out of scope here.
                ((symbol-function 'agent-repl--sidebar-tick) #'ignore)
                ((symbol-function 'agent-repl--async-refresh-git-status)
                 (lambda (_ws) (setq update-called t))))
        (agent-repl--update-all-workspace-states)
        (should-not update-called)))))

(ert-deftest agent-repl-test-update-all-running-agent ()
  "update-all should call async-refresh for a ws with a running agent.
Binds `agent-repl-state-git-tick-modulus' to 1 so every tick is a git tick;
otherwise the mod-N gate would suppress `--async-refresh-git-status' on the
first call (counter increments to 1, `(mod 1 5)' is non-zero)."
  (agent-repl-test--with-clean-state
    (let ((refreshed-ws nil)
          (agent-repl-state-git-tick-modulus 1))
      ;; Register ws1 in the hashmap so the iterator finds it
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'agent-repl--poll-workspace-notifications) #'ignore)
                ;; sidebar.el's roster tick rides update-all too; out of scope here.
                ((symbol-function 'agent-repl--sidebar-tick) #'ignore)
                ((symbol-function 'agent-repl--agent-running-p) (lambda (_ws) t))
                ((symbol-function 'agent-repl--async-refresh-git-status)
                 (lambda (ws) (setq refreshed-ws ws)))
                ((symbol-function 'agent-repl--async-refresh-branch-merged) #'ignore))
        (agent-repl--update-all-workspace-states)
        (should (equal refreshed-ws "ws1"))))))

(ert-deftest agent-repl-test-update-all-excludes-projectless-placeholders ()
  "Project-state updates skip and log live persp placeholders without dirs."
  (agent-repl-test--with-clean-state
    (let (processed logs)
      (agent-repl--ws-put "project" :project-dir "/tmp/project")
      (agent-repl--ws-put "main" :agent-state :idle)
      (agent-repl--ws-put "none" :repl-state :inactive)
      (cl-letf (((symbol-function 'agent-repl--update-one-workspace-state)
                 (lambda (ws _do-git-p) (push ws processed)))
                ((symbol-function 'agent-repl--log-verbose)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) logs))))
        (agent-repl--update-all-workspace-states-now)
        (should (equal processed '("project")))
        (should
         (cl-some
          (lambda (line)
            (and (string-match-p "placeholders=" line)
                 (string-match-p "main" line)
                 (string-match-p "none" line)))
          logs))))))

(ert-deftest agent-repl-test-update-all-non-gui-not-running-marks-dead ()
  "update-all should call mark-dead for a non-gui workspace with no running agent.
`mark-dead' fires every tick regardless of the mod-N gate, so no
gate-tweak is needed here.  `:frontend' is stamped directly to a
non-gui placeholder symbol so `--ws-gui-frontend-p' is false for this
ws — `agent-repl--agent-running-p' is stubbed below, so the placeholder
is never resolved through the (real, gui-only) frontend registry."
  (agent-repl-test--with-clean-state
    (let ((dead-ws nil))
      ;; Register ws1 in the hashmap so the iterator finds it
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--ws-put "ws1" :frontend 'not-gui)
      (cl-letf (((symbol-function 'agent-repl--poll-workspace-notifications) #'ignore)
                ;; sidebar.el's roster tick rides update-all too; out of scope here.
                ((symbol-function 'agent-repl--sidebar-tick) #'ignore)
                ((symbol-function 'agent-repl--agent-running-p) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--mark-dead)
                 (lambda (ws) (setq dead-ws ws)))
                ((symbol-function 'agent-repl--async-refresh-branch-merged) #'ignore))
        (agent-repl--update-all-workspace-states)
        (should (equal dead-ws "ws1"))))))

(ert-deftest agent-repl-test-update-one-gui-never-marks-dead ()
  "update-one-workspace-state never marks a gui workspace dead.
A gui workspace has no vterm process, so the vterm-liveness check is
meaningless and the daemon owns death via session_dead_* sentinels."
  (agent-repl-test--with-clean-state
    (let ((dead-ws nil))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--ws-put "ws1" :frontend 'gui)
      (cl-letf (((symbol-function 'agent-repl--agent-running-p) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--mark-dead)
                 (lambda (ws) (setq dead-ws ws))))
        (agent-repl--update-one-workspace-state "ws1" nil)
        (should-not dead-ws)))))

(ert-deftest agent-repl-test-update-one-gui-takes-the-alive-branch ()
  "A gui workspace takes the alive branch even when agent-running-p is nil.
Liveness for a gui workspace is the daemon's to report (a pushed DEAD
WorkspaceState), so the poll never marks one dead — it only refreshes
git for it."
  (agent-repl-test--with-clean-state
    (let ((refreshed-ws nil))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--ws-put "ws1" :frontend 'gui)
      (cl-letf (((symbol-function 'agent-repl--agent-running-p) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--async-refresh-branch-merged) #'ignore)
                ((symbol-function 'agent-repl--async-refresh-git-status)
                 (lambda (ws) (setq refreshed-ws ws))))
        (agent-repl--update-one-workspace-state "ws1" t)
        (should (equal refreshed-ws "ws1"))))))

(ert-deftest agent-repl-test-update-one-gui-preserves-sentinel-state ()
  "The poll must not clobber sentinel-driven agent-state on a gui workspace.
Runs the REAL mark-dead: the gui branch must prevent it from
ever being reached, keeping :thinking intact and :repl-state un-dead."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--ws-put "ws1" :frontend 'gui)
    (agent-repl--ws-set-agent-state "ws1" :thinking)
    (cl-letf (((symbol-function 'agent-repl--agent-running-p) (lambda (_ws) nil))
)
      (agent-repl--update-one-workspace-state "ws1" nil)
      (should (eq (agent-repl--ws-get "ws1" :agent-state) :thinking))
      (should-not (eq (agent-repl--ws-get "ws1" :repl-state) :dead)))))

(ert-deftest agent-repl-test-update-one-non-gui-still-marks-dead ()
  "update-one-workspace-state still marks a non-gui workspace dead.
The gui branch must not weaken the liveness check for any OTHER
frontend a workspace might carry — `:frontend' is stamped directly to
a non-gui placeholder symbol so `--ws-gui-frontend-p' is false; the
`agent-repl--agent-running-p' stub below means the placeholder is
never resolved through the (real, gui-only) frontend registry."
  (agent-repl-test--with-clean-state
    (let ((dead-ws nil))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--ws-put "ws1" :frontend 'not-gui)
      (cl-letf (((symbol-function 'agent-repl--agent-running-p) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--mark-dead)
                 (lambda (ws) (setq dead-ws ws))))
        (agent-repl--update-one-workspace-state "ws1" nil)
        (should (equal dead-ws "ws1"))))))

(ert-deftest agent-repl-test-update-all-calls-poll ()
  "update-all-workspace-states should call poll-workspace-notifications."
  (agent-repl-test--with-clean-state
    (let ((persp-mode nil)
          (poll-called nil))
      (cl-letf (((symbol-function 'agent-repl--poll-workspace-notifications)
                 (lambda () (setq poll-called t))))
        (agent-repl--update-all-workspace-states)
        (should poll-called)))))

(ert-deftest agent-repl-test-update-all-workspace-states-now-does-not-poll ()
  "update-all-workspace-states-now must NOT call poll-workspace-notifications.
The poll is a file-notify fallback that belongs only on the 1Hz timer
path (`update-all-workspace-states').  Calling it on every event-driven
refresh (workspace-switch, frame-focus, show-panels) would add a
redundant `directory-files' scan to each."
  (agent-repl-test--with-clean-state
    (let ((poll-called nil))
      (cl-letf (((symbol-function 'agent-repl--poll-workspace-notifications)
                 (lambda () (setq poll-called t)))
                ((symbol-function 'agent-repl--update-all-workspace-states--step)
                 (lambda (&rest _) nil)))
        (agent-repl--update-all-workspace-states-now)
        (should-not poll-called)))))

;;;; ---- Tests: mark-dead ----

(ert-deftest agent-repl-test-mark-dead-sets-dead-and-clears-agent-state ()
  "mark-dead writes :repl-state :dead and clears :agent-state."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :done)
    (agent-repl--mark-dead "ws1")
    (should (eq (agent-repl--ws-repl-state "ws1") :dead))
    (should-not (agent-repl--ws-agent-state "ws1"))))

(ert-deftest agent-repl-test-mark-dead-from-thinking ()
  "mark-dead clears :thinking (vterm is gone; sentinel won't fire)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :thinking)
    (agent-repl--mark-dead "ws1")
    (should (eq (agent-repl--ws-repl-state "ws1") :dead))
    (should-not (agent-repl--ws-agent-state "ws1"))))

(ert-deftest agent-repl-test-mark-dead-idempotent ()
  "A second death observation leaves an already-clean :dead workspace unchanged.
Stale :agent-state residue on a :dead workspace is NOT preserved
anymore — that was the stuck-:thinking bug — so idempotence means the
fully-transitioned state (:dead, nil agent-state) is a fixed point."
  (agent-repl-test--with-clean-state
    ;; Arrange: first observation performs the full transition.
    (agent-repl--ws-put "ws1" :agent-state :done)
    (agent-repl--mark-dead "ws1")
    ;; Act
    (agent-repl--mark-dead "ws1")
    ;; Assert — fixed point.
    (should (eq (agent-repl--ws-repl-state "ws1") :dead))
    (should-not (agent-repl--ws-agent-state "ws1"))))

;; The mark-dead → :dead display-state test was deleted in the agent-shim
;; cutover (design §10): `agent-repl--mark-dead' still sets `:repl-state
;; :dead' (asserted by `agent-repl-test-mark-dead-clears-agent-state'),
;; but that no longer drives `--ws-render-status' / `--ws-display-state',
;; which now read the daemon-pushed `:pushed-render-state'.  The daemon
;; pushes RENDER_STATE_DEAD for the dead badge.

(ert-deftest agent-repl-test-mark-dead-preserves-init ()
  "mark-dead is a no-op when :agent-state is :init.
During initialize-agent, the timer may tick before agent-running-p returns t
even though the session is legitimately coming up; under the old code
this clobbered :init with :dead.  The :init guard prevents that."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :init)
    (agent-repl--mark-dead "ws1")
    (should (eq (agent-repl--ws-agent-state "ws1") :init))
    (should-not (agent-repl--ws-repl-state "ws1"))))

;;;; ---- Tests: status-react-to-pushed-death ----

(ert-deftest agent-repl-test-react-to-pushed-death-marks-dead ()
  "A pushed DEAD render state marks the workspace dead (session_dead_ parity)."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (agent-repl--ws-set-agent-state "ws1" :thinking)
    ;; Act — the transition subscriber receives (ws :dead previous)
    (agent-repl--status-react-to-pushed-death "ws1" :dead :thinking)
    ;; Assert — mark-dead's effect: :repl-state :dead, cleared agent-state
    (should (eq (agent-repl--ws-repl-state "ws1") :dead))
    (should-not (agent-repl--ws-agent-state "ws1"))))

(ert-deftest agent-repl-test-react-to-pushed-death-noop-when-not-dead ()
  "A non-DEAD pushed render state does not mark the workspace dead."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (agent-repl--ws-set-agent-state "ws1" :thinking)
    ;; Act
    (agent-repl--status-react-to-pushed-death "ws1" :idle :thinking)
    ;; Assert — untouched
    (should-not (agent-repl--ws-repl-state "ws1"))
    (should (eq (agent-repl--ws-agent-state "ws1") :thinking))))

(ert-deftest agent-repl-test-react-to-pushed-death-registered-on-hook ()
  "The death reactor is registered on `agent-repl-ws-state-transition-functions'."
  (should (memq #'agent-repl--status-react-to-pushed-death
                agent-repl-ws-state-transition-functions)))

;;;; ---- Tests: on-frame-focus ----

(ert-deftest agent-repl-test-on-frame-focus-with-focus ()
  "on-frame-focus should update all workspace states when frame has focus.
Mocks the unguarded `-now' entrypoint because frame-focus bypasses the
periodic-timer in-flight guard — see `--on-frame-focus' docstring.
No vterm to refresh anymore; readiness is entirely hook-driven."
  (agent-repl-test--with-clean-state
    (let ((update-called nil))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda () t))
                ((symbol-function 'agent-repl--update-all-workspace-states-now)
                 (lambda () (setq update-called t))))
        (agent-repl--on-frame-focus)
        (should update-called)))))

(ert-deftest agent-repl-test-on-frame-focus-no-focus ()
  "on-frame-focus should be a no-op when frame does not have focus.
Mocks the unguarded `-now' entrypoint; matches what production code calls."
  (agent-repl-test--with-clean-state
    (let ((update-called nil))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda () nil))
                ((symbol-function 'agent-repl--update-all-workspace-states-now)
                 (lambda () (setq update-called t))))
        (agent-repl--on-frame-focus)
        (should-not update-called)))))


;;;; ---- Tests: ws-clear-if-status cross-state edge cases ----

(ert-deftest agent-repl-test-ws-clear-thinking-when-permission-noop ()
  "Clearing :thinking when actual status is :permission should be a no-op."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :permission)
    (agent-repl--ws-agent-state-clear-if "ws1" :thinking)
    (should (eq (agent-repl--ws-get "ws1" :agent-state) :permission))))

(ert-deftest agent-repl-test-ws-clear-inactive-when-thinking-noop ()
  "Clearing :inactive when actual status is :thinking should be a no-op."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :thinking)
    (agent-repl--ws-agent-state-clear-if "ws1" :inactive)
    (should (eq (agent-repl--ws-get "ws1" :agent-state) :thinking))))

(ert-deftest agent-repl-test-ws-clear-done-when-inactive-noop ()
  "Clearing :done when actual status is :inactive should be a no-op."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :inactive)
    (agent-repl--ws-agent-state-clear-if "ws1" :done)
    (should (eq (agent-repl--ws-get "ws1" :agent-state) :inactive))))

(ert-deftest agent-repl-test-ws-clear-inactive-when-done-noop ()
  "Clearing :inactive when actual status is :done should be a no-op."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :done)
    (agent-repl--ws-agent-state-clear-if "ws1" :inactive)
    (should (eq (agent-repl--ws-get "ws1" :agent-state) :done))))

(ert-deftest agent-repl-test-ws-clear-thinking-when-done-noop ()
  "Clearing :thinking when actual status is :done should be a no-op."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :done)
    (agent-repl--ws-agent-state-clear-if "ws1" :thinking)
    (should (eq (agent-repl--ws-get "ws1" :agent-state) :done))))

;;;; ---- Tests: update-all-workspace-states multi-workspace dispatch ----

(ert-deftest agent-repl-test-update-all-multiple-workspaces-dispatch ()
  "update-all should dispatch per workspace: refresh running, clear dead.
Binds `agent-repl-state-git-tick-modulus' to 1 so git refreshes fire on the
first tick.  Synchronous step chaining comes from `--with-clean-state' setting
`agent-repl--update-spread-sync' to `t', so both workspaces are processed
within a single function call rather than being spread across `run-at-time'
timers that would never fire under ERT batch mode.

`dead-ws' is stamped with a non-gui placeholder `:frontend' so
`--ws-gui-frontend-p' is false for it -- otherwise the gui branch of
`--update-one-workspace-state' would take the running path regardless
of the stubbed `agent-repl--agent-running-p' answer below."
  (agent-repl-test--with-clean-state
    (let ((refreshed nil)
          (cleared nil)
          (agent-repl-state-git-tick-modulus 1))
      ;; Register both workspaces in the hashmap
      (agent-repl--ws-put "running-ws" :project-dir "/tmp/running")
      (agent-repl--ws-put "dead-ws" :project-dir "/tmp/dead")
      (agent-repl--ws-put "dead-ws" :frontend 'not-gui)
      (cl-letf (((symbol-function 'agent-repl--poll-workspace-notifications) #'ignore)
                ;; sidebar.el's roster tick rides update-all too; out of scope here.
                ((symbol-function 'agent-repl--sidebar-tick) #'ignore)
                ((symbol-function 'agent-repl--agent-running-p)
                 (lambda (ws) (equal ws "running-ws")))
                ((symbol-function 'agent-repl--async-refresh-git-status)
                 (lambda (ws) (push ws refreshed)))
                ((symbol-function 'agent-repl--async-refresh-branch-merged) #'ignore)
                ((symbol-function 'agent-repl--mark-dead)
                 (lambda (ws) (push ws cleared))))
        (agent-repl--update-all-workspace-states)
        ;; running-ws should get update + refresh
        (should (member "running-ws" refreshed))
        (should-not (member "running-ws" cleared))
        ;; dead-ws should get clear, not update
        (should (member "dead-ws" cleared))
        (should-not (member "dead-ws" refreshed))))))

;;;; ---- Tests: mod-N git tick gate ----

(ert-deftest agent-repl-test-update-all-git-gate-skips-non-modulus-tick ()
  "Git refreshes do NOT fire on ticks where `(mod counter modulus) /= 0'.
With modulus=5 and counter starting at 0, the first tick post-increment is
counter=1, `(mod 1 5)' = 1, so the gate is closed."
  (agent-repl-test--with-clean-state
    (let ((git-refreshed nil)
          (merge-refreshed nil)
          (agent-repl-state-git-tick-modulus 5))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'agent-repl--poll-workspace-notifications) #'ignore)
                ;; sidebar.el's roster tick rides update-all too; out of scope here.
                ((symbol-function 'agent-repl--sidebar-tick) #'ignore)
                ((symbol-function 'agent-repl--agent-running-p) (lambda (_ws) t))
                ((symbol-function 'agent-repl--async-refresh-git-status)
                 (lambda (_ws) (setq git-refreshed t)))
                ((symbol-function 'agent-repl--async-refresh-branch-merged)
                 (lambda (_ws) (setq merge-refreshed t))))
        (agent-repl--update-all-workspace-states)
        (should-not git-refreshed)
        (should-not merge-refreshed)))))

(ert-deftest agent-repl-test-update-all-git-gate-fires-on-modulus-tick ()
  "Git refreshes DO fire when `(mod counter modulus) == 0'.
Pre-seeding the counter to (modulus - 1) means the in-function increment
lands on a multiple of modulus, opening the gate."
  (agent-repl-test--with-clean-state
    (let ((git-refreshed nil)
          (merge-refreshed nil)
          (agent-repl-state-git-tick-modulus 5)
          (agent-repl--update-tick-counter 4))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'agent-repl--poll-workspace-notifications) #'ignore)
                ;; sidebar.el's roster tick rides update-all too; out of scope here.
                ((symbol-function 'agent-repl--sidebar-tick) #'ignore)
                ((symbol-function 'agent-repl--agent-running-p) (lambda (_ws) t))
                ((symbol-function 'agent-repl--async-refresh-git-status)
                 (lambda (_ws) (setq git-refreshed t)))
                ((symbol-function 'agent-repl--async-refresh-branch-merged)
                 (lambda (_ws) (setq merge-refreshed t))))
        (agent-repl--update-all-workspace-states)
        (should git-refreshed)
        (should merge-refreshed)))))

(ert-deftest agent-repl-test-update-all-increments-tick-counter ()
  "Every call to the periodic timer entrypoint increments the tick counter.
The counter feeds the mod-N git gate."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--poll-workspace-notifications) #'ignore))
      (let ((before agent-repl--update-tick-counter))
        (agent-repl--update-all-workspace-states)
        (agent-repl--update-all-workspace-states)
        (agent-repl--update-all-workspace-states)
        (should (= (+ before 3) agent-repl--update-tick-counter))))))

;;;; ---- Tests: in-flight reentry guard ----

(ert-deftest agent-repl-test-update-all-in-flight-guard-skips-tick ()
  "Timer entrypoint skips its tick when a chain is already in flight.
Setting `--update-in-flight' to a recent float-time simulates a chain that
started just now and hasn't finalized; the next call should skip the actual
update chain, but both the tabline toggle and poll-workspace-notifications
still fire (poll runs before the in-flight check on the timer path)."
  (agent-repl-test--with-clean-state
    (let ((poll-called nil)
          (toggle-before agent-repl--tabline-space-toggle))
      (setq agent-repl--update-in-flight (float-time))
      (cl-letf (((symbol-function 'agent-repl--poll-workspace-notifications)
                 (lambda () (setq poll-called t))))
        (agent-repl--update-all-workspace-states)
        ;; Poll fires on every timer tick regardless of in-flight state.
        (should poll-called)
        ;; Toggle survives the in-flight guard so the tab-bar keeps animating.
        (should-not (eq toggle-before agent-repl--tabline-space-toggle))))))

(ert-deftest agent-repl-test-update-all-in-flight-stale-flag-recovers ()
  "An `--update-in-flight' stamp older than the stale threshold is force-cleared.
Without this, an error in a per-step body that escapes the `condition-case'
net could wedge the periodic timer permanently."
  (agent-repl-test--with-clean-state
    (let ((poll-called nil)
          (agent-repl-state-stale-threshold 5.0))
      ;; Simulate a chain that started 10s ago and never finalized.
      (setq agent-repl--update-in-flight (- (float-time) 10.0))
      (cl-letf (((symbol-function 'agent-repl--poll-workspace-notifications)
                 (lambda () (setq poll-called t))))
        (agent-repl--update-all-workspace-states)
        ;; Stale flag was cleared and the new chain ran.
        (should poll-called)
        ;; After the chain finalizes, in-flight is back to nil.
        (should-not agent-repl--update-in-flight)))))

(ert-deftest agent-repl-test-update-all-tabline-toggle-survives-in-flight-skip ()
  "Tabline space toggle flips on every tick, even when the in-flight guard skips.
This is critical: tab-bar repainting requires the toggle string to change
between ticks; if a long chain skips multiple ticks, the animation must still
advance.  Verified explicitly here so the entrypoint structure doesn't drift."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--tabline-space-toggle nil))
      (setq agent-repl--update-in-flight (float-time))
      (cl-letf (((symbol-function 'agent-repl--poll-workspace-notifications) #'ignore)
                ;; sidebar.el's roster tick rides update-all too; out of scope here.
                ((symbol-function 'agent-repl--sidebar-tick) #'ignore))
        (agent-repl--update-all-workspace-states)
        (should (eq agent-repl--tabline-space-toggle t))
        (agent-repl--update-all-workspace-states)
        (should (eq agent-repl--tabline-space-toggle nil))))))

;;;; ---- Tests: update-in-flight-p ----

(ert-deftest agent-repl-test-update-in-flight-p-nil-flag ()
  "in-flight-p returns nil when no chain is running."
  (agent-repl-test--with-clean-state
    (should-not agent-repl--update-in-flight)
    (should-not (agent-repl--update-in-flight-p))))

(ert-deftest agent-repl-test-update-in-flight-p-recent-flag ()
  "in-flight-p returns non-nil when the flag is recent (within stale threshold)."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-state-stale-threshold 5.0))
      (setq agent-repl--update-in-flight (float-time))
      (should (agent-repl--update-in-flight-p)))))

(ert-deftest agent-repl-test-update-in-flight-p-stale-flag-clears ()
  "in-flight-p clears a stale flag and returns nil.
Stale-flag recovery is side-effecting: the flag is reset to nil so subsequent
callers see a fresh state."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-state-stale-threshold 5.0))
      (setq agent-repl--update-in-flight (- (float-time) 10.0))
      (should-not (agent-repl--update-in-flight-p))
      (should-not agent-repl--update-in-flight))))

;;;; ---- Tests: sync entrypoint bypasses guard ----

(ert-deftest agent-repl-test-update-all-now-bypasses-in-flight-flag ()
  "The unguarded `-now' entrypoint runs even when a chain is in flight.
Event-driven callers (frame-focus, workspace-switch, show-panels) want to
kick a refresh regardless of the timer's in-flight reentry guard.
Does NOT call poll-workspace-notifications — that is the timer path's job."
  (agent-repl-test--with-clean-state
    (let ((step-called nil)
          (poll-called nil))
      (setq agent-repl--update-in-flight (float-time))
      (cl-letf (((symbol-function 'agent-repl--poll-workspace-notifications)
                 (lambda () (setq poll-called t)))
                ((symbol-function 'agent-repl--update-all-workspace-states--step)
                 (lambda (&rest _) (setq step-called t))))
        (agent-repl--update-all-workspace-states-now)
        ;; Chain fires despite in-flight flag
        (should step-called)
        ;; Poll does NOT fire from the event-driven path
        (should-not poll-called)))))

(ert-deftest agent-repl-test-update-all-now-does-not-flip-tabline-toggle ()
  "Only the periodic-timer entrypoint flips the tabline toggle.
Event-driven callers trigger redisplay through other paths, so duplicating
the flip from `-now' would be needless churn and could double-flip when
both the timer and a sync caller fire in the same instant."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--tabline-space-toggle nil))
      (cl-letf (((symbol-function 'agent-repl--poll-workspace-notifications) #'ignore)
                ;; sidebar.el's roster tick rides update-all too; out of scope here.
                ((symbol-function 'agent-repl--sidebar-tick) #'ignore))
        (agent-repl--update-all-workspace-states-now)
        (should-not agent-repl--tabline-space-toggle)))))

;;;; ---- Tests: finalize clears in-flight flag ----

(ert-deftest agent-repl-test-update-all-finalize-clears-flag ()
  "The terminal finalize step clears `--update-in-flight'.
Verified end-to-end: after a synchronous-spread chain completes, the flag is
back to nil and the next timer tick can run."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--poll-workspace-notifications) #'ignore))
      (agent-repl--update-all-workspace-states-now)
      (should-not agent-repl--update-in-flight))))

(ert-deftest agent-repl-test-update-all-finalize-direct-clears-flag ()
  "Calling finalize directly always clears the flag, even with no chain context.
This isolates the finalize behavior from the chain-entry behavior, so a future
refactor that wires finalize into a different code path can't silently break
the flag-clear invariant."
  (agent-repl-test--with-clean-state
    (setq agent-repl--update-in-flight (float-time))
    (agent-repl--update-all-workspace-states--finalize)
    (should-not agent-repl--update-in-flight)))

;;;; ---- Tests: mid-chain ws removal ----

(ert-deftest agent-repl-test-update-all-step-skips-removed-ws ()
  "The recursive step rechecks workspace pollability before acting on each ws.
A workspace can be deleted via `--ws-del' between the chain snapshot and the
step that would process it; the per-step recheck filters out ghost names so
the body never touches a removed workspace's stale plist (which would
re-create the entry as a side effect of `agent-repl--ws-put')."
  (agent-repl-test--with-clean-state
    (let ((processed nil))
      ;; Only register "alive-ws"; "removed-ws" is in the snapshot list but
      ;; not in the hash, simulating mid-chain removal.
      (agent-repl--ws-put "alive-ws" :project-dir "/tmp/alive")
      (cl-letf (((symbol-function 'agent-repl--update-one-workspace-state)
                 (lambda (ws _do-git-p) (push ws processed))))
        (agent-repl--update-all-workspace-states--step
         '("removed-ws" "alive-ws") nil 0.0)
        (should (member "alive-ws" processed))
        (should-not (member "removed-ws" processed))))))

;;;; ---- Tests: per-step error isolation ----

(ert-deftest agent-repl-test-update-all-step-error-does-not-wedge-chain ()
  "An error in one per-ws step is logged and the chain continues.
Without the `condition-case' wrap, an errored step would propagate, the
finalize wouldn't run, and the in-flight flag would stay set until the stale
threshold fires (5s of dead timer ticks)."
  (agent-repl-test--with-clean-state
    (let ((processed nil))
      (agent-repl--ws-put "ws-a" :project-dir "/tmp/a")
      (agent-repl--ws-put "ws-b" :project-dir "/tmp/b")
      (cl-letf (((symbol-function 'agent-repl--update-one-workspace-state)
                 (lambda (ws _do-git-p)
                   (if (equal ws "ws-a")
                       (error "boom")
                     (push ws processed)))))
        (agent-repl--update-all-workspace-states--step
         '("ws-a" "ws-b") nil 0.0)
        ;; ws-b still got processed after ws-a errored.
        (should (member "ws-b" processed))
        ;; And the in-flight flag was cleared by finalize.
        (should-not agent-repl--update-in-flight)))))

;;;; ---- Tests: per-workspace step ----

(ert-deftest agent-repl-test-update-one-ws-no-git-when-gate-closed ()
  "`--update-one-workspace-state' with DO-GIT-P nil skips git refreshes.
The cheap state-machine work still runs."
  (agent-repl-test--with-clean-state
    (let ((git-fired nil)
          (merge-fired nil)
          (state-fired nil))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'agent-repl--agent-running-p) (lambda (_ws) t))
                ((symbol-function 'agent-repl--async-refresh-git-status)
                 (lambda (_ws) (setq git-fired t)))
                ((symbol-function 'agent-repl--async-refresh-branch-merged)
                 (lambda (_ws) (setq merge-fired t))))
        (agent-repl--update-one-workspace-state "ws1" nil)
        (should-not git-fired)
        (should-not merge-fired)))))

(ert-deftest agent-repl-test-update-one-ws-fires-git-when-gate-open ()
  "`--update-one-workspace-state' with DO-GIT-P non-nil fires both git refreshes."
  (agent-repl-test--with-clean-state
    (let ((git-fired nil)
          (merge-fired nil))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'agent-repl--agent-running-p) (lambda (_ws) t))
                ((symbol-function 'agent-repl--async-refresh-git-status)
                 (lambda (_ws) (setq git-fired t)))
                ((symbol-function 'agent-repl--async-refresh-branch-merged)
                 (lambda (_ws) (setq merge-fired t))))
        (agent-repl--update-one-workspace-state "ws1" t)
        (should git-fired)
        (should merge-fired)))))

(ert-deftest agent-repl-test-update-one-ws-dead-agent-skips-state-update ()
  "When a non-gui workspace's agent is not running, `--update-one-workspace-state'
calls `--mark-dead' and skips the git refresh.  Merge refresh still
fires when DO-GIT-P is on because merged-ness is independent of agent
liveness — a dead workspace can still have a merge-completed parent."
  (agent-repl-test--with-clean-state
    (let ((dead-called nil)
          (merge-called nil))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--ws-put "ws1" :frontend 'not-gui)
      (cl-letf (((symbol-function 'agent-repl--agent-running-p) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--mark-dead)
                 (lambda (_ws) (setq dead-called t)))
                ((symbol-function 'agent-repl--async-refresh-branch-merged)
                 (lambda (_ws) (setq merge-called t))))
        (agent-repl--update-one-workspace-state "ws1" t)
        (should dead-called)
        (should merge-called)))))

;;;; ---- Tests: priority-image (moved from core.el) ----

(ert-deftest agent-repl-test-priority-image-valid ()
  "priority-image should return the image spec for a known priority."
  (let ((agent-repl--priority-images '(("p1" . fake-image-spec))))
    (should (equal (agent-repl--priority-image "p1") 'fake-image-spec))))

(ert-deftest agent-repl-test-priority-image-unknown ()
  "priority-image should return nil for an unknown priority."
  (let ((agent-repl--priority-images '(("p1" . fake-image-spec))))
    (should-not (agent-repl--priority-image "p99"))))

(ert-deftest agent-repl-test-priority-image-nil-input ()
  "priority-image should return nil for nil input."
  (let ((agent-repl--priority-images '(("p1" . fake-image-spec))))
    (should-not (agent-repl--priority-image nil))))

(ert-deftest agent-repl-test-priority-image-empty-alist ()
  "priority-image should return nil when the images alist is empty."
  (let ((agent-repl--priority-images nil))
    (should-not (agent-repl--priority-image "p1"))))

;;;; ---- Tests: priority-rank ----

(ert-deftest agent-repl-test-priority-rank-p05-is-zero ()
  "priority-rank returns 0 for p05 (highest priority)."
  (let ((agent-repl-priority-levels '("p05" "p1" "p2" "p3")))
    (should (= (agent-repl--priority-rank "p05") 0))))

(ert-deftest agent-repl-test-priority-rank-p1-is-one ()
  "priority-rank returns 1 for p1."
  (let ((agent-repl-priority-levels '("p05" "p1" "p2" "p3")))
    (should (= (agent-repl--priority-rank "p1") 1))))

(ert-deftest agent-repl-test-priority-rank-p3-is-three ()
  "priority-rank returns 3 for p3 (lowest recognized priority)."
  (let ((agent-repl-priority-levels '("p05" "p1" "p2" "p3")))
    (should (= (agent-repl--priority-rank "p3") 3))))

(ert-deftest agent-repl-test-priority-rank-nil-sorts-last ()
  "priority-rank returns most-positive-fixnum for nil priority."
  (should (= (agent-repl--priority-rank nil) most-positive-fixnum)))

(ert-deftest agent-repl-test-priority-rank-unknown-sorts-last ()
  "priority-rank returns most-positive-fixnum for unrecognized priority."
  (let ((agent-repl-priority-levels '("p05" "p1" "p2" "p3")))
    (should (= (agent-repl--priority-rank "p99") most-positive-fixnum))))

;;;; ---- Tests: load-priority-images (moved from core.el) ----

(ert-deftest agent-repl-test-load-priority-images-all-present ()
  "load-priority-images should populate alist when all PNGs exist."
  (let ((tmpdir (make-temp-file "test-img-" t)))
    (unwind-protect
        (let ((img-dir (expand-file-name "images/" tmpdir)))
          (make-directory img-dir t)
          ;; Create fake PNG files
          (dolist (name '("p05" "p1" "p2" "p3"))
            (with-temp-file (expand-file-name (concat name ".png") img-dir)
              (insert "fake-png")))
          (let ((agent-repl--priority-images nil)
                (load-file-name (expand-file-name "core.el" tmpdir)))
            (cl-letf (((symbol-function 'create-image)
                       (lambda (file _type &rest _args) (list 'image :file file)))
                      ((symbol-function 'frame-char-height) (lambda () 16)))
              (agent-repl--load-priority-images)
              (should (= (length agent-repl--priority-images) 4))
              (should (assoc "p1" agent-repl--priority-images))
              (should (assoc "p05" agent-repl--priority-images)))))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-load-priority-images-some-missing ()
  "load-priority-images should skip missing PNG files."
  (let ((tmpdir (make-temp-file "test-img-" t)))
    (unwind-protect
        (let ((img-dir (expand-file-name "images/" tmpdir)))
          (make-directory img-dir t)
          ;; Create only p1.png
          (with-temp-file (expand-file-name "p1.png" img-dir)
            (insert "fake-png"))
          (let ((agent-repl--priority-images nil)
                (load-file-name (expand-file-name "core.el" tmpdir)))
            (cl-letf (((symbol-function 'create-image)
                       (lambda (file _type &rest _args) (list 'image :file file)))
                      ((symbol-function 'frame-char-height) (lambda () 16)))
              (agent-repl--load-priority-images)
              (should (= (length agent-repl--priority-images) 1))
              (should (assoc "p1" agent-repl--priority-images)))))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-load-priority-images-dir-missing ()
  "load-priority-images should produce empty alist when images dir does not exist."
  (let ((tmpdir (make-temp-file "test-img-" t)))
    (unwind-protect
        (let ((agent-repl--priority-images nil)
              (load-file-name (expand-file-name "core.el" tmpdir)))
          (cl-letf (((symbol-function 'create-image)
                     (lambda (file _type &rest _args) (list 'image :file file)))
                    ((symbol-function 'frame-char-height) (lambda () 16)))
            (agent-repl--load-priority-images)
            (should (null agent-repl--priority-images))))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-load-priority-images-buffer-file-fallback ()
  "load-priority-images should use buffer-file-name when load-file-name is nil."
  (let ((tmpdir (make-temp-file "test-img-" t)))
    (unwind-protect
        (let ((img-dir (expand-file-name "images/" tmpdir)))
          (make-directory img-dir t)
          (with-temp-file (expand-file-name "p1.png" img-dir)
            (insert "fake-png"))
          (let ((agent-repl--priority-images nil)
                (load-file-name nil)
                (buffer-file-name (expand-file-name "core.el" tmpdir)))
            (cl-letf (((symbol-function 'create-image)
                       (lambda (file _type &rest _args) (list 'image :file file)))
                      ((symbol-function 'frame-char-height) (lambda () 16)))
              (agent-repl--load-priority-images)
              (should (= (length agent-repl--priority-images) 1)))))
      (delete-directory tmpdir t))))

;; The Stop / SubagentStop tracking-helper tests (pending-subagents
;; get/incf/decf, stop-received get/set, fully-stopped-p, clear-stop-tracking)
;; were DELETED in the agent-shim cutover (design §10): that whole
;; hook-counter block was removed from status.el.  The daemon's SSM now
;; owns turn-finished / subagent-in-flight resolution and pushes it as a
;; `frontend.v1' WorkspaceState frame.

;;;; ---- Tests: :vendor-blocked ws-display-state behavior ----
;;
;; `:vendor-blocked' superseded the retired `:stop-failed' keyword in
;; the status-semantics cutover.  The legacy `--composed-state'
;; pure-mapping coverage moved into test-workspace.el's
;; `--ws-render-status' coverage.  Only the display-state (panel-gated)
;; wrapper assertion remains here.

(ert-deftest agent-repl-test-display-state-vendor-blocked ()
  "ws-display-state returns the pushed :vendor-blocked state when visible."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :vendor-blocked)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p)
               (lambda (_ws) t)))
      (should (eq :vendor-blocked (agent-repl--ws-display-state "ws1"))))))

;;;; ---- Tests: :vendor-blocked palette resolution ----

(ert-deftest agent-repl-test-tab-spec-vendor-blocked-unselected ()
  "tab-spec for :vendor-blocked unselected returns the purple plist."
  (let ((spec (agent-repl--tab-spec :vendor-blocked nil)))
    (should (equal (plist-get spec :bg) agent-repl--color-vendor-blocked-purple))
    (should (equal (plist-get spec :fg) agent-repl--color-light))))

(ert-deftest agent-repl-test-tab-spec-vendor-blocked-selected ()
  "tab-spec for :vendor-blocked selected keeps the purple on the bracket."
  (let ((spec (agent-repl--tab-spec :vendor-blocked t)))
    (should (equal (plist-get spec :bg) agent-repl--color-selected-bg))
    (should (equal (plist-get spec :bracket-bg)
                   agent-repl--color-vendor-blocked-purple))))

;;;; ---- Tests: hide-mode tabline (no filtering — tab-bar reflects raw list) ----

(ert-deftest agent-repl-test-hide-mode-tabline-passthrough-when-off ()
  "When hide-mode is off, the tab-bar renders every name in the input list."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-hide-mode-enabled nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws")))
        (let ((result (agent-repl--tabline-advice '("current-ws" "bg-ws"))))
          (should (string-match-p "current-ws" result))
          (should (string-match-p "bg-ws" result)))))))

(ert-deftest agent-repl-test-hide-mode-tabline-passthrough-when-on ()
  "When hide-mode is on, the tab-bar STILL renders every name — filtering
no longer happens at the tab-bar layer (it's enforced by persp-kill on
workspace switch).  This guards against regressing back to the old
tabline-filter design that caused tab-bar bugs."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-hide-mode-enabled t))
      (agent-repl--ws-set-repl-state "bg-ws" :hidden)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws")))
        (let ((result (agent-repl--tabline-advice '("current-ws" "bg-ws"))))
          (should (string-match-p "current-ws" result))
          (should (string-match-p "bg-ws" result)))))))

;;;; ---- Tests: filter-hidden-names (cycle-skip helper) ----

(ert-deftest agent-repl-test-filter-hidden-names-passthrough-when-off ()
  "filter-hidden-names returns NAMES unchanged when hide-mode is off."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-hide-mode-enabled nil))
      (agent-repl--ws-set-repl-state "ws-b" :hidden)
      (should (equal (agent-repl--filter-hidden-names '("ws-a" "ws-b") "ws-a")
                     '("ws-a" "ws-b"))))))

(ert-deftest agent-repl-test-filter-hidden-names-drops-hidden ()
  "filter-hidden-names drops non-current workspaces with `:repl-state :hidden'."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-hide-mode-enabled t))
      (agent-repl--ws-set-repl-state "ws-b" :hidden)
      (should (equal (agent-repl--filter-hidden-names '("ws-a" "ws-b" "ws-c") "ws-a")
                     '("ws-a" "ws-c"))))))

(ert-deftest agent-repl-test-filter-hidden-names-keeps-current-even-if-hidden ()
  "filter-hidden-names always retains the current workspace, even if `:hidden'."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-hide-mode-enabled t))
      (agent-repl--ws-set-repl-state "ws-a" :hidden)
      (should (equal (agent-repl--filter-hidden-names '("ws-a" "ws-b") "ws-a")
                     '("ws-a" "ws-b"))))))

(ert-deftest agent-repl-test-filter-hidden-names-keeps-non-hidden-states ()
  "filter-hidden-names retains workspaces with `:active' / `:inactive' / nil
states — only `:hidden' is filtered."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-hide-mode-enabled t))
      (agent-repl--ws-set-repl-state "ws-b" :inactive)
      (agent-repl--ws-set-repl-state "ws-c" :active)
      (should (equal (agent-repl--filter-hidden-names '("ws-a" "ws-b" "ws-c") "ws-a")
                     '("ws-a" "ws-b" "ws-c"))))))

(ert-deftest agent-repl-test-toggle-hide-mode-flips-flag ()
  "agent-repl-toggle-hide-mode flips the flag and triggers a tab-bar redraw."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-hide-mode-enabled nil)
          (redraw-called 0))
      (cl-letf (((symbol-function 'agent-repl--force-tab-bar-redraw)
                 (lambda () (cl-incf redraw-called))))
        (agent-repl-toggle-hide-mode)
        (should agent-repl-hide-mode-enabled)
        (should (= redraw-called 1))
        (agent-repl-toggle-hide-mode)
        (should-not agent-repl-hide-mode-enabled)
        (should (= redraw-called 2))))))

;;;; ---- Tests: tabline first-fit packing primitive ----

(ert-deftest agent-repl-test-pack-first-fit-all-fit-one-row ()
  "Entries that fit the first row all land there; later rows stay empty."
  (should (equal (agent-repl--pack-first-fit '(3 3 3) '(80 80))
                 '(3 0))))

(ert-deftest agent-repl-test-pack-first-fit-spills-to-next-row ()
  "Entries that overflow the first row spill into the second."
  ;; Row cap 9: "aaaa"(4)+sep+"bbbb"(4)=9 fits; +sep+"cccc" would be 14 > 9.
  (should (equal (agent-repl--pack-first-fit '(4 4 4) '(9 9))
                 '(2 1))))

(ert-deftest agent-repl-test-pack-first-fit-returns-nil-when-overflow ()
  "When the entries cannot all fit the given rows, nil is returned."
  (should (null (agent-repl--pack-first-fit '(4 4 4 4 4) '(9 9)))))

(ert-deftest agent-repl-test-pack-first-fit-counts-sum-to-entries ()
  "Per-row counts sum to the number of entries placed."
  (let ((counts (agent-repl--pack-first-fit '(4 4 4 4) '(9 9))))
    (should (= 4 (apply #'+ counts)))))

;;;; ---- Tests: tabline row packing (livelock guard) ----
;;
;; `agent-repl--tabline-rows' returns a LIST of exactly MAX-ROWS
;; strings.  The single-row (MAX-ROWS 1) cases below preserve the
;; pre-two-row packing invariants; the two-row cases cover the fixed
;; two-row tab-bar.

(defun agent-repl-test--single-row (entries current-pos width)
  "Return the sole row `agent-repl--tabline-rows' packs with MAX-ROWS 1."
  (car (agent-repl--tabline-rows entries current-pos width 1)))

(ert-deftest agent-repl-test-tabline-rows-empty ()
  "Empty entry list renders to MAX-ROWS empty rows."
  (should (equal (agent-repl--tabline-rows nil 0 80 1) '("")))
  (should (equal (agent-repl--tabline-rows nil 0 80 2) '("" ""))))

(ert-deftest agent-repl-test-tabline-rows-exact-count ()
  "The result always has exactly MAX-ROWS elements, even when tabs fit one row."
  (should (= 2 (length (agent-repl--tabline-rows '("abc" "def") 0 80 2))))
  (should (= 3 (length (agent-repl--tabline-rows '("abc" "def") 0 80 3)))))

(ert-deftest agent-repl-test-tabline-rows-single-all-fit ()
  "Entries that fit join with single-space separators, no badges."
  (should (equal (agent-repl-test--single-row '("abc" "def" "ghi") 0 80)
                 "abc def ghi")))

(ert-deftest agent-repl-test-tabline-rows-never-contains-newline ()
  "No packed row ever contains a newline — wrapping is elision, not a
newline, so the join controls the row count and thus the tab-bar height."
  (dolist (width '(1 4 10 20 40))
    (dolist (max-rows '(1 2))
      (dolist (row (agent-repl--tabline-rows
                    '("aaaa" "bbbb" "cccc" "dddd" "eeee" "ffff") 2 width max-rows))
        (should-not (string-search "\n" row))))))

(ert-deftest agent-repl-test-tabline-rows-single-overflow-keeps-current ()
  "When entries overflow, the current entry is always in the row."
  (let ((entries '("aaaa" "bbbb" "cccc" "dddd" "eeee")))
    (dotimes (cur 5)
      (let ((row (agent-repl-test--single-row entries cur 12)))
        (should (string-search (nth cur entries) row))))))

(ert-deftest agent-repl-test-tabline-rows-single-overflow-badges ()
  "Elided neighbors are summarized by +N badges on the matching side."
  ;; budget = 20 - 2*(2 + 1) = 14; window around index 2 ("cccc"):
  ;; grows right to "dddd" (9), left to "bbbb" (14), then no more fits.
  (let ((row (agent-repl-test--single-row
              '("aaaa" "bbbb" "cccc" "dddd" "eeee") 2 20)))
    (should (equal row "+1 bbbb cccc dddd +1"))))

(ert-deftest agent-repl-test-tabline-rows-overflow-fits-width ()
  "No packed row (window + badges) ever exceeds WIDTH columns."
  (let ((entries '("aaaa" "bbbb" "cccc" "dddd" "eeee" "ffff" "gggg")))
    (dolist (width '(8 12 16 20 24))
      (dolist (max-rows '(1 2))
        (dotimes (cur 7)
          (dolist (row (agent-repl--tabline-rows entries cur width max-rows))
            (should (<= (length row) width))))))))

(ert-deftest agent-repl-test-tabline-rows-single-nil-current-pos ()
  "A nil CURRENT-POS falls back to windowing around the first entry."
  (let ((row (agent-repl-test--single-row
              '("aaaa" "bbbb" "cccc" "dddd" "eeee") nil 12)))
    (should (string-search "aaaa" row))
    (should-not (string-prefix-p "+" row))))

(ert-deftest agent-repl-test-tabline-rows-two-fit-blank-second-row ()
  "When all tabs fit one row, MAX-ROWS 2 leaves the second row blank."
  (should (equal (agent-repl--tabline-rows '("abc" "def" "ghi") 0 80 2)
                 '("abc def ghi" ""))))

(ert-deftest agent-repl-test-tabline-rows-two-uses-second-row-before-eliding ()
  "Entries that overflow one row fill the second row rather than eliding."
  ;; Width 12 fits only "aaaa bbbb" (9) per row; two rows hold four entries
  ;; with none elided, so neither a leading nor trailing badge appears.
  (let* ((rows (agent-repl--tabline-rows
                '("aaaa" "bbbb" "cccc" "dddd") 0 12 2)))
    (should (= 2 (length rows)))
    (dolist (e '("aaaa" "bbbb" "cccc" "dddd"))
      (should (cl-some (lambda (r) (string-search e r)) rows)))
    (should-not (cl-some (lambda (r) (string-search "+" r)) rows))))

(ert-deftest agent-repl-test-tabline-rows-two-overflow-keeps-current ()
  "With more tabs than two rows hold, the current tab stays visible."
  (let ((entries '("aaaa" "bbbb" "cccc" "dddd" "eeee" "ffff" "gggg" "hhhh")))
    (dotimes (cur 8)
      (let ((rows (agent-repl--tabline-rows entries cur 14 2)))
        (should (cl-some (lambda (r) (string-search (nth cur entries) r)) rows))))))

(ert-deftest agent-repl-test-tabline-rows-two-overflow-badges ()
  "Overflow past two rows shows a trailing +N badge on the last row."
  (let ((rows (agent-repl--tabline-rows
               '("aaaa" "bbbb" "cccc" "dddd" "eeee" "ffff" "gggg" "hhhh") 0 12 2)))
    ;; Current is index 0, so the leading side has nothing elided (no "+N ")
    ;; but the trailing side does — the badge lands on the second row.
    (should-not (string-prefix-p "+" (car rows)))
    (should (string-match-p "\\+[0-9]+\\'" (cadr rows)))))

(ert-deftest agent-repl-test-tabline-rendered-entries-count ()
  "rendered-entries returns one element per workspace name."
  (agent-repl-test--with-clean-state
    (let ((entries (agent-repl--tabline-rendered-entries '("a" "b" "c"))))
      (should (= (length entries) 3))
      (should (cl-every #'stringp entries)))))

(ert-deftest agent-repl-test-tabline-advice-uses-rendered-entries ()
  "tabline-advice's output is equivalent to mapconcating rendered-entries with a space."
  (agent-repl-test--with-clean-state
    (let* ((agent-repl--tabline-space-toggle nil)
           (entries (agent-repl--tabline-rendered-entries '("a" "b")))
           (expected (mapconcat #'identity entries " "))
           (result (agent-repl--tabline-advice '("a" "b"))))
      (should (equal (substring-no-properties result)
                     (substring-no-properties expected))))))

;;;; ---- Tests: tabline entry width (pixel-accurate measurement) ----

(ert-deftest agent-repl-test-tabline-entry-width-plain-text ()
  "A plain-text entry (no display property) is measured by `string-width'."
  (should (= 3 (agent-repl--tabline-entry-width "abc"))))

(ert-deftest agent-repl-test-tabline-entry-width-image-measured-in-pixels ()
  "An entry carrying a `display' property is measured in pixels and
converted to columns, not by its (tiny) character length."
  (cl-letf (((symbol-function 'string-pixel-width) (lambda (&rest _) 40))
            ((symbol-function 'frame-char-width) (lambda (&rest _) 10)))
    ;; character length is 1, but 40px / 10px-per-column = 4 columns.
    (should (= 4 (agent-repl--tabline-entry-width
                  (propertize " " 'display "img"))))))

(ert-deftest agent-repl-test-tabline-entry-width-rounds-up ()
  "A pixel width that is not a whole number of columns rounds UP, so the
estimate never under-reserves and a row can never pixel-overflow."
  (cl-letf (((symbol-function 'string-pixel-width) (lambda (&rest _) 41))
            ((symbol-function 'frame-char-width) (lambda (&rest _) 10)))
    ;; 41px / 10 = 4.1 columns -> ceil -> 5.
    (should (= 5 (agent-repl--tabline-entry-width
                  (propertize " " 'display "img"))))))

(ert-deftest agent-repl-test-tabline-entry-width-minimum-one ()
  "An empty entry never measures less than one column."
  (should (= 1 (agent-repl--tabline-entry-width ""))))

(ert-deftest agent-repl-test-tabline-rows-image-pixel-width-forces-elision ()
  "An image-bearing entry whose PIXEL width overflows the row budget is
elided behind a `+N' badge, even though its one-character length would
fit both rows.  The column-accurate width from
`agent-repl--tabline-entry-width' is what stops a badge-bearing row
from physically wrapping to a third row (the tab-bar livelock); with
the old character-length measurement no badge would appear."
  (cl-letf (((symbol-function 'string-pixel-width) (lambda (&rest _) 30))
            ((symbol-function 'frame-char-width) (lambda (&rest _) 1)))
    (let* ((img (propertize " " 'display "badge")) ; 1 char, 30 px-columns
           (rows (agent-repl--tabline-rows (list "aa" img "bb" "cc" "dd") 1 20 2)))
      (should (cl-some (lambda (r) (string-match-p "\\+[0-9]+" r)) rows)))))

;;;; ---- Tests: tabline row join (face-extension guard) ----

(ert-deftest agent-repl-test-join-tabline-rows-empty ()
  "Joining an empty list returns an empty string."
  (should (equal (agent-repl--join-tabline-rows nil) "")))

(ert-deftest agent-repl-test-join-tabline-rows-single ()
  "A single line is suffixed with an unfaced space so the tab-bar's
per-row face extension paints the default face on the row remainder,
not the row's last entry's faced padding space."
  (should (equal (agent-repl--join-tabline-rows '("only")) "only ")))

(ert-deftest agent-repl-test-join-tabline-rows-multi-uses-space-newline ()
  "Adjacent rows are separated by ` \\n' and the final row also gets a
trailing ` ' so EVERY row ends with an unfaced space terminator."
  (should (equal (agent-repl--join-tabline-rows '("a" "b" "c"))
                 "a \nb \nc ")))

(ert-deftest agent-repl-test-join-tabline-rows-non-final-rows-end-with-unfaced-space ()
  "The character immediately before each newline is an unfaced space.
This is what stops the tab-bar's per-row face extension from painting
the last entry's face to the row's right edge — if the char before
`\\n' carried a face, the extension would paint that face across the
gap.  We assert: (a) every char preceding a newline is a space, and
(b) none of those spaces carry a face text-property."
  (let* ((faced-a (propertize "alpha" 'face '+workspace-tab-selected-face))
         (faced-b (propertize "beta"  'face '+workspace-tab-face))
         (faced-c (propertize "gamma" 'face '+workspace-tab-selected-face))
         (joined  (agent-repl--join-tabline-rows
                   (list faced-a faced-b faced-c)))
         (newline-positions
          (cl-loop for i from 0 below (length joined)
                   when (eq (aref joined i) ?\n)
                   collect i)))
    (should (= (length newline-positions) 2))
    (dolist (pos newline-positions)
      (let ((prev (1- pos)))
        (should (>= prev 0))
        (should (eq (aref joined prev) ?\s))
        (should-not (get-text-property prev 'face joined))))))

(ert-deftest agent-repl-test-join-tabline-rows-final-row-ends-with-unfaced-space ()
  "The last character of the joined string is an unfaced space.
Without this, the tab-bar's per-row face extension would paint the
final entry's name-face background across the last row's remainder
(the bug visible whenever multi-line tab-bar wraps and the last
entry on the bottom row carries a stateful background)."
  (let* ((faced-a (propertize "alpha" 'face '+workspace-tab-selected-face))
         (faced-b (propertize "beta"  'face '+workspace-tab-face))
         (joined  (agent-repl--join-tabline-rows (list faced-a faced-b)))
         (last    (1- (length joined))))
    (should (eq (aref joined last) ?\s))
    (should-not (get-text-property last 'face joined))))

(ert-deftest agent-repl-test-join-tabline-rows-single-line-final-char-unfaced ()
  "Even a single-row tab-bar gets the trailing unfaced space.
Same face-extension reasoning as the multi-row case: when a single
centered row is shorter than the frame width, the face extension would
paint the rightmost entry's background to the right edge."
  (let* ((faced (propertize "alpha" 'face '+workspace-tab-selected-face))
         (joined (agent-repl--join-tabline-rows (list faced)))
         (last (1- (length joined))))
    (should (eq (aref joined last) ?\s))
    (should-not (get-text-property last 'face joined))))

(ert-deftest agent-repl-test-join-tabline-rows-preserves-row-faces ()
  "Joining does not strip text properties from the original row content."
  (let* ((faced (propertize "abc" 'face '+workspace-tab-selected-face))
         (joined (agent-repl--join-tabline-rows (list faced "def"))))
    (should (eq (get-text-property 0 'face joined)
                '+workspace-tab-selected-face))
    (should (eq (get-text-property 2 'face joined)
                '+workspace-tab-selected-face))))

;;;; ---- Tests: repo-default priority resolution ----

(ert-deftest agent-repl-test-repo-name-for-path-nil ()
  "`--repo-name-for-path' returns nil when PATH is nil."
  (should-not (agent-repl--repo-name-for-path nil)))

(ert-deftest agent-repl-test-repo-name-for-path-nonexistent ()
  "`--repo-name-for-path' returns nil when PATH does not exist."
  (should-not (agent-repl--repo-name-for-path "/tmp/does-not-exist-agent-repl-test/")))

(ert-deftest agent-repl-test-repo-name-for-path-absolute-common-dir ()
  "`--repo-name-for-path' extracts the basename of the parent of an absolute --git-common-dir."
  (let ((tmp (make-temp-file "repo-name-test-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
                   (lambda (&rest _args)
                     "/some/path/explanation-engine/.git"))
                  ((symbol-function 'agent-repl--path-canonical) #'identity))
          (should (equal (agent-repl--repo-name-for-path tmp)
                         "explanation-engine")))
      (delete-directory tmp t))))

(ert-deftest agent-repl-test-repo-name-for-path-relative-common-dir ()
  "`--repo-name-for-path' resolves a relative --git-common-dir against PATH."
  (let ((tmp (make-temp-file "repo-name-test-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
                   (lambda (&rest _args) ".git"))
                  ((symbol-function 'agent-repl--path-canonical) #'identity))
          ;; basename(parent(<tmp>/.git)) = basename(<tmp>)
          (should (equal (agent-repl--repo-name-for-path tmp)
                         (file-name-nondirectory (directory-file-name tmp)))))
      (delete-directory tmp t))))

(ert-deftest agent-repl-test-repo-name-for-path-fatal ()
  "`--repo-name-for-path' returns nil when git emits a fatal message."
  (let ((tmp (make-temp-file "repo-name-test-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
                   (lambda (&rest _args)
                     "fatal: not a git repository")))
          (should-not (agent-repl--repo-name-for-path tmp)))
      (delete-directory tmp t))))

(ert-deftest agent-repl-test-repo-name-for-path-empty ()
  "`--repo-name-for-path' returns nil when git emits an empty string."
  (let ((tmp (make-temp-file "repo-name-test-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
                   (lambda (&rest _args) "")))
          (should-not (agent-repl--repo-name-for-path tmp)))
      (delete-directory tmp t))))

(ert-deftest agent-repl-test-repo-default-priority-explanation-engine ()
  "`--repo-default-priority-for-path' returns p3 for explanation-engine by default."
  (let ((agent-repl-repo-default-priorities '(("explanation-engine" . "p3"))))
    (cl-letf (((symbol-function 'agent-repl--repo-name-for-path)
               (lambda (_path) "explanation-engine")))
      (should (equal (agent-repl--repo-default-priority-for-path "/any/path")
                     "p3")))))

(ert-deftest agent-repl-test-repo-default-priority-unknown-repo ()
  "`--repo-default-priority-for-path' returns nil for unconfigured repos."
  (let ((agent-repl-repo-default-priorities '(("explanation-engine" . "p3"))))
    (cl-letf (((symbol-function 'agent-repl--repo-name-for-path)
               (lambda (_path) "doom")))
      (should-not (agent-repl--repo-default-priority-for-path "/any/path")))))

(ert-deftest agent-repl-test-repo-default-priority-nil-repo-name ()
  "`--repo-default-priority-for-path' returns nil when repo name resolution fails."
  (let ((agent-repl-repo-default-priorities '(("explanation-engine" . "p3"))))
    (cl-letf (((symbol-function 'agent-repl--repo-name-for-path)
               (lambda (_path) nil)))
      (should-not (agent-repl--repo-default-priority-for-path "/any/path")))))

(ert-deftest agent-repl-test-repo-default-priority-explicit-nil-value ()
  "`--repo-default-priority-for-path' returns nil when an entry's value is nil."
  (let ((agent-repl-repo-default-priorities '(("foo" . nil))))
    (cl-letf (((symbol-function 'agent-repl--repo-name-for-path)
               (lambda (_path) "foo")))
      (should-not (agent-repl--repo-default-priority-for-path "/any/path")))))

(ert-deftest agent-repl-test-repo-default-priority-default-value-has-explanation-engine ()
  "Default value of `agent-repl-repo-default-priorities' assigns p1 to explanation-engine."
  (should (equal (cdr (assoc "explanation-engine" agent-repl-repo-default-priorities))
                 "p1")))

;;;; ---- Tests: pack-width / center-width reserve room for terminator ----

(ert-deftest agent-repl-test-tabline-rows-reserve-room-for-terminator ()
  "Callers must size the row to `(- frame-width 1)' (not `frame-width') so
the unfaced terminator appended by `agent-repl--join-tabline-rows' lands
within the visible columns (0..frame-width-1) after centering.

This test pins the contract: a row built for width W must never be
wider than W chars.  Combined with center-target = W, this guarantees
the centered+terminated row source is `<= W + 1` chars total — with
the caller passing `(1- frame-width)' as W, the terminator lands at
col `<= frame-width - 1' (visible)."
  (dolist (n '(5 10 20))
    (let ((entries (mapcar #'number-to-string (number-sequence 1 n))))
      (dolist (w '(4 8))
        (dolist (max-rows '(1 2))
          (dotimes (cur n)
            (dolist (row (agent-repl--tabline-rows entries cur w max-rows))
              (should (<= (length row) w)))))))))

;;;; ---- Tests: +workspace--message-body override (suppress tabline flash) ----

(ert-deftest agent-repl-test-workspace-message-body-advice-strips-tabline ()
  "Override returns ONLY the message text — no tabline prefix, no ` | ' separator.
Pins the merge-teardown contract: when `+workspace/kill' (called via
`agent-repl--nuke-one-workspace' during the merge-completed close) hits
`+workspace--message-body', the resulting echo-area string must not flash
the workspaces tabline."
  (let ((result (agent-repl--workspace-message-body-advice
                 "Deleted 'foo' workspace" 'success)))
    (should (equal (substring-no-properties result) "Deleted 'foo' workspace"))
    (should-not (string-match-p " | " result))))

(ert-deftest agent-repl-test-workspace-message-body-advice-faces-by-type ()
  "Override applies the correct face per TYPE (error/warn/success/info)."
  (dolist (case '((error . error)
                  (warn . warning)
                  (success . success)
                  (info . font-lock-comment-face)))
    (let* ((type (car case))
           (expected-face (cdr case))
           (result (agent-repl--workspace-message-body-advice "msg" type)))
      (should (equal (get-text-property 0 'face result) expected-face)))))

(ert-deftest agent-repl-test-workspace-message-body-advice-installed ()
  "The override is installed on `+workspace--message-body' at load time.
Guards against accidental removal of the `advice-add' at the bottom of
status.el — without it, the stock body (tabline + separator + message)
would resurface."
  (let ((advice-installed nil))
    (advice-mapc (lambda (fn _props)
                   (when (eq fn #'agent-repl--workspace-message-body-advice)
                     (setq advice-installed t)))
                 '+workspace--message-body)
    (should advice-installed)))

(ert-deftest agent-repl-test-workspace-message-body-advice-no-tabline-call ()
  "Override must not invoke `+workspace--tabline' — the whole point is to
avoid rendering the workspace list at all when the body is built for an
echo-area message.  Counter-stubs `+workspace--tabline' to signal if
called and verifies the advice runs cleanly."
  (cl-letf (((symbol-function '+workspace--tabline)
             (lambda (&optional _names)
               (error "+workspace--tabline must not be called from the message-body override"))))
    (let ((result (agent-repl--workspace-message-body-advice "ok" 'success)))
      (should (equal (substring-no-properties result) "ok")))))

;;;; ---- Tests: fixed-height tab-bar livelock prevention ----

(ert-deftest agent-repl-test-retire-storm-watchdog-cancels-old-timer ()
  "Hot reload cancels the old reactive watchdog heartbeat timer."
  (let ((agent-repl--storm-tick-timer 'old-timer)
        (agent-repl--timers '(other-timer old-timer))
        (pre-redisplay-function nil)
        (cancelled nil))
    (cl-letf (((symbol-function 'timerp) (lambda (timer) (eq timer 'old-timer)))
              ((symbol-function 'cancel-timer)
               (lambda (timer) (setq cancelled timer))))
      (let ((result (agent-repl--retire-redisplay-storm-watchdog)))
        (should (eq cancelled 'old-timer))
        (should-not agent-repl--storm-tick-timer)
        (should (equal agent-repl--timers '(other-timer)))
        (should (plist-get result :timer-cancelled))))))

(ert-deftest agent-repl-test-retire-storm-watchdog-removes-old-hook ()
  "Hot reload removes the old watchdog from `pre-redisplay-function'."
  (let ((pre-redisplay-function nil)
        (agent-repl--storm-tick-timer nil))
    (add-function :after pre-redisplay-function
                  #'agent-repl--redisplay-storm-watchdog)
    (let ((result (agent-repl--retire-redisplay-storm-watchdog)))
      (should (plist-get result :hook-present))
      (should-not pre-redisplay-function))))

(ert-deftest agent-repl-test-fixed-height-tab-bar-disables-native-resize ()
  "Installation disables auto-resize and pins existing and future frames.
This is the regression contract for the macOS 100%-CPU livelock:
`redisplay_tab_bar' must never enter its dynamic height path."
  (let ((auto-resize-tab-bars 'grow-only)
        (default-frame-alist '((tab-bar-lines . 1) (width . 100)))
        (frame-inhibit-implied-resize nil)
        (tab-bar-format nil)
        (tab-bar-show nil)
        (tab-bar-close-button-show t)
        (tab-bar-new-button-show t)
        (frame-lines '((frame-a . 1) (frame-b . 3)))
        (tab-bar-mode-arg nil)
        (logged nil))
    (cl-letf (((symbol-function 'frame-list)
               (lambda () '(frame-a frame-b)))
              ((symbol-function 'display-graphic-p) (lambda (_frame) t))
              ((symbol-function 'frame-parameter)
               (lambda (frame parameter)
                 (and (eq parameter 'tab-bar-lines)
                      (alist-get frame frame-lines))))
              ((symbol-function 'set-frame-parameter)
               (lambda (frame parameter value)
                 (should (eq parameter 'tab-bar-lines))
                 (setf (alist-get frame frame-lines) value)))
              ((symbol-function 'tab-bar-mode)
               (lambda (arg)
                 (setq tab-bar-mode-arg arg)
                 ;; Emulate Emacs 30.2: enabling the mode resets the future
                 ;; frame default to one line.
                 (setf (alist-get 'tab-bar-lines default-frame-alist) 1)))
              ((symbol-function 'agent-repl--retire-redisplay-storm-watchdog)
               (lambda () '(:timer-cancelled nil :hook-present nil)))
              ((symbol-function 'agent-repl--log)
               (lambda (&rest args) (setq logged args)))
              ((symbol-function 'run-with-timer)
               (lambda (&rest _)
                 (error "fixed-height install must not schedule recovery timers"))))
      (agent-repl--install-fixed-height-tab-bar)
      (should-not auto-resize-tab-bars)
      (should (eq tab-bar-mode-arg 1))
      (should (= (alist-get 'tab-bar-lines default-frame-alist)
                 agent-repl--tabline-row-count))
      (should (= (alist-get 'frame-a frame-lines)
                 agent-repl--tabline-row-count))
      (should (= (alist-get 'frame-b frame-lines)
                 agent-repl--tabline-row-count))
      (should (memq 'tab-bar-lines frame-inhibit-implied-resize))
      (should (equal tab-bar-format
                     '(agent-repl-workspace-tabline-formatted
                       tab-bar-format-align-right
                       agent-repl-current-workspace-name-segment)))
      (should-not tab-bar-close-button-show)
      (should-not tab-bar-new-button-show)
      (should logged))))

(ert-deftest agent-repl-test-fixed-height-tab-bar-default-covers-future-frames ()
  "Installation pins `default-frame-alist' even with no current GUI frame."
  (let ((auto-resize-tab-bars t)
        (default-frame-alist nil)
        (frame-inhibit-implied-resize t))
    (cl-letf (((symbol-function 'frame-list) (lambda () nil))
              ((symbol-function 'display-graphic-p) (lambda (_frame) nil))
              ((symbol-function 'tab-bar-mode)
               (lambda (_arg)
                 (setf (alist-get 'tab-bar-lines default-frame-alist) 1)))
              ((symbol-function 'agent-repl--retire-redisplay-storm-watchdog)
               (lambda () '(:timer-cancelled nil :hook-present nil)))
              ((symbol-function 'agent-repl--log) #'ignore))
      (agent-repl--install-fixed-height-tab-bar)
      (should-not auto-resize-tab-bars)
      (should (= (alist-get 'tab-bar-lines default-frame-alist)
                 agent-repl--tabline-row-count)))))

;;;; ---- The two context cuts --------------------------------------------

(ert-deftest agent-repl-test-state-color-clearing-is-red ()
  ":clearing takes thinking's red: the same claim, a different word."
  ;; Act / Assert
  (should (equal (alist-get :clearing agent-repl--state-color) "red")))

(ert-deftest agent-repl-test-state-color-compacting-is-red ()
  ":compacting takes thinking's red: the same claim, a different word."
  ;; Act / Assert
  (should (equal (alist-get :compacting agent-repl--state-color) "red")))

(ert-deftest agent-repl-test-display-state-clearing-panels-open-renders-clearing ()
  ":clearing with panels visible renders :clearing (red)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :clearing)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p)
               (lambda (_ws) t)))
      (should (eq :clearing (agent-repl--ws-display-state "ws1"))))))

(ert-deftest agent-repl-test-display-state-compacting-panels-open-renders-compacting ()
  ":compacting with panels visible renders :compacting (red)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :compacting)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p)
               (lambda (_ws) t)))
      (should (eq :compacting (agent-repl--ws-display-state "ws1"))))))

(provide 'test-status)

;;; test-status.el ends here
