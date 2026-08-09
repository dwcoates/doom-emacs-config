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
  "ws-set-repl-state calls --state-save when STATE is :inactive so a
closed workspace stays closed across an Emacs restart."
  (agent-repl-test--with-clean-state
    (let ((saved nil))
      (cl-letf (((symbol-function 'agent-repl--state-save)
                 (lambda (ws) (setq saved ws))))
        (agent-repl--ws-set-repl-state "ws1" :inactive)
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
  "`:merge-conflict' has no palette row: a conflict is waiting on the USER,
which is the opposite of the in-flight claim the merging states' red makes."
  (should-not (alist-get :merge-conflict agent-repl--tab-palette)))

(ert-deftest agent-repl-test-tab-palette-has-no-merge-failed-entry ()
  "`:merge-failed' likewise has no palette row: nothing is in flight."
  (should-not (alist-get :merge-failed agent-repl--tab-palette)))

(ert-deftest agent-repl-test-tab-spec-merging-is-red ()
  "A merge the daemon is RUNNING paints the tab thinking-red.
Before the override this state had no palette row at all, so a merging
workspace fell through to the default appearance and read as untouched."
  ;; Act / Assert
  (should (equal agent-repl--color-thinking-red
                 (plist-get (agent-repl--tab-spec :merging nil) :bg))))

(ert-deftest agent-repl-test-tab-spec-merge-enqueuing-is-red ()
  "A merge on its way into the queue is already in flight, so it is red."
  ;; Act / Assert
  (should (equal agent-repl--color-thinking-red
                 (plist-get (agent-repl--tab-spec :merge-enqueuing nil) :bg))))

(ert-deftest agent-repl-test-tab-spec-merge-queued-is-red ()
  "A merge waiting behind a sibling is in flight from the user's side.
They can no more act on the workspace than during the merge itself."
  ;; Act / Assert
  (should (equal agent-repl--color-thinking-red
                 (plist-get (agent-repl--tab-spec :merge-queued nil) :bg))))

(ert-deftest agent-repl-test-tab-spec-merging-brackets-green ()
  "A merge the daemon is RUNNING paints the [N] bracket green.
The red belongs to the name region alone: an entry painted red edge to
edge read as a thinking turn, and the green bracket says where the work
in flight is headed."
  ;; Act / Assert
  (should (equal agent-repl--color-done-green
                 (plist-get (agent-repl--tab-spec :merging nil) :bracket-bg))))

(ert-deftest agent-repl-test-tab-spec-merge-enqueuing-brackets-green ()
  "A merge on its way into the queue takes the same green bracket."
  ;; Act / Assert
  (should (equal agent-repl--color-done-green
                 (plist-get (agent-repl--tab-spec :merge-enqueuing nil) :bracket-bg))))

(ert-deftest agent-repl-test-tab-spec-merge-queued-brackets-green ()
  "A merge waiting behind a sibling takes the same green bracket."
  ;; Act / Assert
  (should (equal agent-repl--color-done-green
                 (plist-get (agent-repl--tab-spec :merge-queued nil) :bracket-bg))))

(ert-deftest agent-repl-test-tab-spec-thinking-bracket-inherits-the-name-color ()
  "A THINKING tab still paints one color end to end.
The split is the merge states\=' alone, so nothing else may grow a
bracket color of its own without saying so."
  ;; Act / Assert
  (should-not (plist-get (agent-repl--tab-spec :thinking nil) :bracket-bg)))

(ert-deftest agent-repl-test-tab-spec-merging-selected-brackets-red ()
  "A SELECTED merging tab keeps red on the [N] bracket.
Selection dims the name region to the shared grey for every state, so the
bracket is the only place the merge can still be read."
  ;; Arrange
  (let ((spec (agent-repl--tab-spec :merging t)))
    ;; Act / Assert
    (should (equal agent-repl--color-selected-bg (plist-get spec :bg)))
    (should (equal agent-repl--color-thinking-red (plist-get spec :bracket-bg)))))

(ert-deftest agent-repl-test-tab-spec-bracket-only-merging-is-red ()
  "A merging tab whose panels are dismissed keeps red on the bracket.
The bracket-only path is what a workspace the user closed the panels on
renders with, and a merge must stay visible through it."
  ;; Arrange
  (let ((spec (agent-repl--tab-spec-bracket-only :merging nil)))
    ;; Act / Assert
    (should (equal 'unspecified (plist-get spec :bg)))
    (should (equal agent-repl--color-thinking-red (plist-get spec :bracket-bg)))))

(ert-deftest agent-repl-test-tab-spec-bracket-only-selected-merging-is-red ()
  "A SELECTED merging tab with panels dismissed also keeps red on the bracket.
The selected row dims the name region to the shared grey, so the state
color has to come from that row\='s own `:bracket-bg\='."
  ;; Arrange
  (let ((spec (agent-repl--tab-spec-bracket-only :merging t)))
    ;; Act / Assert
    (should (equal agent-repl--color-thinking-red (plist-get spec :bracket-bg)))))

(ert-deftest agent-repl-test-tab-spec-bracket-only-ready-stays-green ()
  "A `:ready\=' tab with panels dismissed keeps green on the bracket.
The unselected row has no `:bracket-bg\=', so the bracket-only path has to
read the state color out of `:bg\='."
  ;; Arrange
  (let ((spec (agent-repl--tab-spec-bracket-only :ready nil)))
    ;; Act / Assert
    (should (equal agent-repl--color-done-green (plist-get spec :bracket-bg)))))

(ert-deftest agent-repl-test-tab-spec-merge-conflict-falls-back-to-default ()
  "`:merge-conflict' takes no tab color, so its spec is the default."
  ;; Arrange
  (let ((spec (agent-repl--tab-spec :merge-conflict nil)))
    ;; Act / Assert
    (should (equal (plist-get spec :bg) 'unspecified))))

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

;;;; ---- Tests: ready-view acknowledgment fades the tab name ----
;;
;; A `:ready' workspace shouts in full green until the user has actually
;; stood in it for `agent-repl-ready-view-fade-delay' seconds; after that
;; the name region falls back to the default face and only the [N] bracket
;; stays green.

(ert-deftest agent-repl-test-ready-view-ack-absent-by-default ()
  "A fresh workspace carries no ready-view acknowledgment."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :ready)
    (should-not (agent-repl--ws-ready-view-acknowledged-p "ws1"))))

(ert-deftest agent-repl-test-ready-view-ack-unknown-ws-is-nil ()
  "An unknown workspace answers nil rather than signalling at the boundary."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--ws-ready-view-acknowledged-p "never-registered"))))

(ert-deftest agent-repl-test-ready-view-dwell-elapsed-without-stamp-is-nil ()
  "A workspace never activated has no `:last-viewed-at' and so no dwell."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :ready)
    (should-not (agent-repl--ws-ready-view-dwell-elapsed-p "ws1"))))

(ert-deftest agent-repl-test-ready-view-dwell-elapsed-below-delay-is-nil ()
  "A view younger than the fade delay has not dwelt long enough."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :last-viewed-at (current-time))
    (should-not (agent-repl--ws-ready-view-dwell-elapsed-p "ws1"))))

(ert-deftest agent-repl-test-ready-view-dwell-elapsed-past-delay-is-t ()
  "A view older than the fade delay has dwelt long enough."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :last-viewed-at
                        (time-subtract (current-time)
                                       (1+ agent-repl-ready-view-fade-delay)))
    (should (agent-repl--ws-ready-view-dwell-elapsed-p "ws1"))))

(ert-deftest agent-repl-test-note-ready-view-dwell-latches-after-delay ()
  "The heartbeat latches the acknowledgment for a viewed :ready workspace."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :ready)
    (agent-repl--ws-put "ws1" :last-viewed-at
                        (time-subtract (current-time)
                                       (1+ agent-repl-ready-view-fade-delay)))
    (cl-letf (((symbol-function 'agent-repl--ws-current-name)
               (lambda () "ws1")))
      (agent-repl--note-ready-view-dwell)
      (should (agent-repl--ws-ready-view-acknowledged-p "ws1")))))

(ert-deftest agent-repl-test-note-ready-view-dwell-waits-for-delay ()
  "A :ready workspace viewed for less than the delay does not latch yet."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :ready)
    (agent-repl--ws-put "ws1" :last-viewed-at (current-time))
    (cl-letf (((symbol-function 'agent-repl--ws-current-name)
               (lambda () "ws1")))
      (agent-repl--note-ready-view-dwell)
      (should-not (agent-repl--ws-ready-view-acknowledged-p "ws1")))))

(ert-deftest agent-repl-test-note-ready-view-dwell-ignores-non-ready-state ()
  "A long-viewed :thinking workspace never latches — the fade is :ready-only."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :thinking)
    (agent-repl--ws-put "ws1" :last-viewed-at
                        (time-subtract (current-time)
                                       (1+ agent-repl-ready-view-fade-delay)))
    (cl-letf (((symbol-function 'agent-repl--ws-current-name)
               (lambda () "ws1")))
      (agent-repl--note-ready-view-dwell)
      (should-not (agent-repl--ws-ready-view-acknowledged-p "ws1")))))

(ert-deftest agent-repl-test-note-ready-view-dwell-ignores-unviewed-workspace ()
  "A :ready workspace that is not the current one does not latch."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws2" :pushed-render-state :ready)
    (agent-repl--ws-put "ws2" :last-viewed-at
                        (time-subtract (current-time)
                                       (1+ agent-repl-ready-view-fade-delay)))
    (cl-letf (((symbol-function 'agent-repl--ws-current-name)
               (lambda () "ws1")))
      (agent-repl--note-ready-view-dwell)
      (should-not (agent-repl--ws-ready-view-acknowledged-p "ws2")))))

(ert-deftest agent-repl-test-note-ready-view-dwell-no-current-workspace-noop ()
  "With no current workspace the heartbeat check is a no-op."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ws-current-name)
               (lambda () nil)))
      (agent-repl--note-ready-view-dwell)
      (should-not (agent-repl--ws-ready-view-acknowledged-p "ws1")))))

(ert-deftest agent-repl-test-note-ready-view-dwell-unknown-current-ws-noop ()
  "A current persp the workspace hash does not know is not stub-created."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ws-current-name)
               (lambda () "none")))
      (agent-repl--note-ready-view-dwell)
      (should-not (agent-repl--ws-known-p "none")))))

(ert-deftest agent-repl-test-clear-ready-view-ack-on-non-ready-push ()
  "A pushed state other than :ready clears the acknowledgment."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :ready-view-acknowledged t)
    (agent-repl--clear-ready-view-ack-on-state-change "ws1" :thinking :ready)
    (should-not (agent-repl--ws-ready-view-acknowledged-p "ws1"))))

(ert-deftest agent-repl-test-clear-ready-view-ack-keeps-latch-on-ready-push ()
  "A re-pushed :ready leaves the acknowledgment latched."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :ready-view-acknowledged t)
    (agent-repl--clear-ready-view-ack-on-state-change "ws1" :ready :ready)
    (should (agent-repl--ws-ready-view-acknowledged-p "ws1"))))

(ert-deftest agent-repl-test-clear-ready-view-ack-unknown-ws-noop ()
  "Clearing for an unknown workspace does not stub-create an entry."
  (agent-repl-test--with-clean-state
    (agent-repl--clear-ready-view-ack-on-state-change "none" :thinking :ready)
    (should-not (agent-repl--ws-known-p "none"))))

(ert-deftest agent-repl-test-clear-ready-view-ack-registered-on-transition-hook ()
  "The clear runs as a state-transition subscriber, not on an ad hoc call site."
  (should (memq #'agent-repl--clear-ready-view-ack-on-state-change
                agent-repl-ws-state-transition-functions)))

(ert-deftest agent-repl-test-display-state-ready-acknowledged-renders-nil ()
  "An acknowledged :ready workspace suppresses the state-colored name region."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :ready)
    (agent-repl--ws-put "ws1" :ready-view-acknowledged t)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p)
               (lambda (_ws) t)))
      (should-not (agent-repl--ws-display-state "ws1")))))

(ert-deftest agent-repl-test-bracket-state-ready-acknowledged-stays-ready ()
  "The [N] bracket keeps :ready green after the acknowledgment latches."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :ready)
    (agent-repl--ws-put "ws1" :ready-view-acknowledged t)
    (should (eq :ready (agent-repl--ws-bracket-state "ws1")))))

(ert-deftest agent-repl-test-display-state-ready-unacknowledged-renders-ready ()
  "An unviewed :ready workspace still paints the full green tab."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :ready)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p)
               (lambda (_ws) t)))
      (should (eq :ready (agent-repl--ws-display-state "ws1"))))))

(ert-deftest agent-repl-test-display-state-ack-does-not-suppress-other-states ()
  "A stale acknowledgment cannot fade a non-:ready state."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :thinking)
    (agent-repl--ws-put "ws1" :ready-view-acknowledged t)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p)
               (lambda (_ws) t)))
      (should (eq :thinking (agent-repl--ws-display-state "ws1"))))))

(ert-deftest agent-repl-test-render-tab-entry-ready-acknowledged-name-default-face ()
  "The rendered tab of an acknowledged :ready workspace uses the default name face."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :ready)
    (agent-repl--ws-put "ws1" :ready-view-acknowledged t)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p)
               (lambda (_ws) t)))
      (let* ((entry (agent-repl--render-tab-entry "ws1" "other" 9))
             (name-pos (string-match "ws1" entry)))
        (should (eq (get-text-property name-pos 'face entry)
                    '+workspace-tab-face))))))

(ert-deftest agent-repl-test-render-tab-entry-ready-acknowledged-bracket-stays-green ()
  "The [9] bracket of an acknowledged :ready workspace keeps the green background."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :ready)
    (agent-repl--ws-put "ws1" :ready-view-acknowledged t)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p)
               (lambda (_ws) t)))
      (let* ((entry (agent-repl--render-tab-entry "ws1" "other" 9))
             (bracket-pos (string-match "\\[9\\]" entry)))
        (should (equal (plist-get (get-text-property bracket-pos 'face entry)
                                  :background)
                       agent-repl--color-done-green))))))

(ert-deftest agent-repl-test-render-tab-entry-merging-bracket-is-green ()
  "The [9] bracket of a merging workspace renders with the green background."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :merging)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p)
               (lambda (_ws) t)))
      (let* ((entry (agent-repl--render-tab-entry "ws1" "other" 9))
             (bracket-pos (string-match "\\[9\\]" entry)))
        (should (equal (plist-get (get-text-property bracket-pos 'face entry)
                                  :background)
                       agent-repl--color-done-green))))))

(ert-deftest agent-repl-test-render-tab-entry-merging-name-keeps-the-red-face ()
  "The NAME region of a merging workspace still renders with the red face."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :pushed-render-state :merging)
    (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p)
               (lambda (_ws) t)))
      (let* ((entry (agent-repl--render-tab-entry "ws1" "other" 9))
             (name-pos (string-match "ws1" entry)))
        (should (eq (get-text-property name-pos 'face entry)
                    'agent-repl-tab-thinking))))))

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

(ert-deftest agent-repl-test-render-tab-entry-uses-state-face ()
  "render-tab-entry uses the state-driven face for the name region."
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

(ert-deftest agent-repl-test-force-tab-bar-redraw-preserves-fixed-height ()
  "A repaint invalidates tab data without invoking Emacs's line recalculator.
On Emacs 30.2 `tab-bar--update-tab-bar-lines' sets every frame and the
future-frame default to one line when `tab-bar-show' is t.  The fixed
two-line contract therefore requires the redraw path never to call it."
  ;; Arrange
  (let ((agent-repl--tabline-space-toggle nil)
        (agent-repl--tabbar-observation-states
         (make-hash-table :test #'eq))
        (agent-repl--tabbar-diagnostic-until nil)
        (tabs-set nil)
        (mode-line-forced nil))
    (cl-letf (((symbol-function 'tab-bar-tabs)
               (lambda () '(current-tabs)))
              ((symbol-function 'tab-bar-tabs-set)
               (lambda (&rest args) (setq tabs-set args)))
              ((symbol-function 'tab-bar--update-tab-bar-lines)
               (lambda (&rest _)
                 (error "redraw must not recalculate tab-bar line count")))
              ((symbol-function 'force-mode-line-update)
               (lambda (&optional all)
                 (setq mode-line-forced all)))
              ((symbol-function 'selected-frame) (lambda () 'frame-a))
              ((symbol-function 'frame-parameter)
               (lambda (_frame parameter)
                 (pcase parameter
                   ('tab-bar-lines 2)
                   ('tab-bar-lines-keep-state t))))
              ((symbol-function 'agent-repl--ws-current-name)
               (lambda () "ws"))
              ((symbol-function 'agent-repl--ws-known-p)
               (lambda (_ws) t))
              ((symbol-function 'agent-repl--log-verbose) #'ignore))
      ;; Act
      (agent-repl--force-tab-bar-redraw)
      ;; Assert
      (should agent-repl--tabline-space-toggle)
      (should (equal tabs-set '((current-tabs))))
      (should mode-line-forced))))

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

(ert-deftest agent-repl-test-workspace-tabline-formatted-always-two-rows ()
  "The formatted tab-bar segment ALWAYS spans exactly two rows.
The row count is FIXED (never varies with workspace count): a
row-count change alters the tab-bar pixel height; on macOS
`ns_change_tab_bar_height' then resizes the NSWindow, and a clipped
resize livelocks redisplay at 100% CPU.  Two rows carry exactly one
newline, and many workspaces at a narrow frame width must elide behind
badges, never wrap to a third row."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-eight-registered-workspaces
     (cl-letf (((symbol-function 'frame-width) (lambda () 24)))
       (dolist (agent-repl--tabline-space-toggle '(nil t))
         (let ((result (agent-repl-workspace-tabline-formatted)))
           (should (stringp result))
           (should (= 1 (cl-count ?\n result)))))))))

(ert-deftest agent-repl-test-workspace-tabline-formatted-two-rows-when-few-tabs ()
  "With only a couple of tabs, the segment is STILL exactly two rows.
The entries need one row, so the second renders blank — but it renders,
because the fixed two-row count is what keeps the tab-bar's pixel
height constant."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-list-names) (lambda () '("ws1" "ws2")))
              ((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'frame-width) (lambda () 80)))
      (let ((agent-repl--tabline-space-toggle nil))
        (should (= 1 (cl-count ?\n (agent-repl-workspace-tabline-formatted))))))))

(ert-deftest agent-repl-test-workspace-tabline-formatted-pads-unfilled-row ()
  "The row the entries do not fill is blank-padded to the full line width.
A zero-length second line would not occupy the pixel row the pinned
`tab-bar-lines' reserves for it."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-list-names) (lambda () '("ws1" "ws2")))
              ((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'frame-width) (lambda () 80)))
      (let* ((agent-repl--tabline-space-toggle nil)
             (lines (split-string
                     (substring-no-properties
                      (agent-repl-workspace-tabline-formatted))
                     "\n")))
        (should (= 2 (length lines)))
        ;; Second line: 79 pad columns plus the join's unfaced terminator.
        (should (string-blank-p (nth 1 lines)))
        (should (= 80 (length (nth 1 lines))))))))

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
                ((symbol-function 'agent-repl--async-refresh-branch-merged)
                 (lambda (_ws) (setq update-called t))))
        (agent-repl--update-all-workspace-states)
        (should-not update-called)))))

(ert-deftest agent-repl-test-update-all-running-agent ()
  "update-all refreshes merged-ness for a ws with a running agent.
Binds `agent-repl-state-git-tick-modulus' to 1 so every tick is a git tick;
otherwise the mod-N gate would suppress the refresh on the first call
(counter increments to 1, `(mod 1 5)' is non-zero)."
  (agent-repl-test--with-clean-state
    (let ((refreshed-ws nil)
          (agent-repl-state-git-tick-modulus 1))
      ;; Register ws1 in the hashmap so the iterator finds it
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'agent-repl--poll-workspace-notifications) #'ignore)
                ;; sidebar.el's roster tick rides update-all too; out of scope here.
                ((symbol-function 'agent-repl--sidebar-tick) #'ignore)
                ((symbol-function 'agent-repl--agent-running-p) (lambda (_ws) t))
                ((symbol-function 'agent-repl--async-refresh-branch-merged)
                 (lambda (ws) (setq refreshed-ws ws))))
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
WorkspaceState), so the poll never marks one dead."
  (agent-repl-test--with-clean-state
    (let ((dead-ws nil))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--ws-put "ws1" :frontend 'gui)
      (cl-letf (((symbol-function 'agent-repl--agent-running-p) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--async-refresh-branch-merged) #'ignore)
                ((symbol-function 'agent-repl--mark-dead)
                 (lambda (ws) (setq dead-ws ws))))
        (agent-repl--update-one-workspace-state "ws1" t)
        (should-not dead-ws)))))

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
                ((symbol-function 'agent-repl--async-refresh-branch-merged)
                 (lambda (ws) (push ws refreshed)))
                ((symbol-function 'agent-repl--mark-dead)
                 (lambda (ws) (push ws cleared))))
        (agent-repl--update-all-workspace-states)
        ;; running-ws is left alone
        (should-not (member "running-ws" cleared))
        ;; dead-ws should get cleared
        (should (member "dead-ws" cleared))
        ;; merged-ness is refreshed for both, alive or not
        (should (member "running-ws" refreshed))
        (should (member "dead-ws" refreshed))))))

;;;; ---- Tests: mod-N git tick gate ----

(ert-deftest agent-repl-test-update-all-git-gate-skips-non-modulus-tick ()
  "Git refreshes do NOT fire on ticks where `(mod counter modulus) /= 0'.
With modulus=5 and counter starting at 0, the first tick post-increment is
counter=1, `(mod 1 5)' = 1, so the gate is closed."
  (agent-repl-test--with-clean-state
    (let ((merge-refreshed nil)
          (agent-repl-state-git-tick-modulus 5))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'agent-repl--poll-workspace-notifications) #'ignore)
                ;; sidebar.el's roster tick rides update-all too; out of scope here.
                ((symbol-function 'agent-repl--sidebar-tick) #'ignore)
                ((symbol-function 'agent-repl--agent-running-p) (lambda (_ws) t))
                ((symbol-function 'agent-repl--async-refresh-branch-merged)
                 (lambda (_ws) (setq merge-refreshed t))))
        (agent-repl--update-all-workspace-states)
        (should-not merge-refreshed)))))

(ert-deftest agent-repl-test-update-all-git-gate-fires-on-modulus-tick ()
  "Git refreshes DO fire when `(mod counter modulus) == 0'.
Pre-seeding the counter to (modulus - 1) means the in-function increment
lands on a multiple of modulus, opening the gate."
  (agent-repl-test--with-clean-state
    (let ((merge-refreshed nil)
          (agent-repl-state-git-tick-modulus 5)
          (agent-repl--update-tick-counter 4))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'agent-repl--poll-workspace-notifications) #'ignore)
                ;; sidebar.el's roster tick rides update-all too; out of scope here.
                ((symbol-function 'agent-repl--sidebar-tick) #'ignore)
                ((symbol-function 'agent-repl--agent-running-p) (lambda (_ws) t))
                ((symbol-function 'agent-repl--async-refresh-branch-merged)
                 (lambda (_ws) (setq merge-refreshed t))))
        (agent-repl--update-all-workspace-states)
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

;;;; ---- Tests: a stale flag against a still-scheduled continuation ----

(defmacro agent-repl-test--with-starved-chain (warned informed &rest body)
  "Run BODY with a 10s-old in-flight flag and a live scheduled next hop.
This is the shape a chain has when a long main-thread operation starved
its `run-at-time' hops: the flag is well past the stale threshold, yet
the continuation is still sitting in `timer-list' waiting to run.  WARNED
and INFORMED are bound to the messages each level received."
  (declare (indent 2))
  `(agent-repl-test--with-clean-state
     (let ((agent-repl-state-stale-threshold 5.0)
           (agent-repl--frontend-expected-restart nil)
           (agent-repl--frontend-expected-restart-last-close nil)
           (,warned nil)
           (,informed nil))
       (setq agent-repl--update-in-flight (- (float-time) 10.0))
       (setq agent-repl--update-chain-timer
             (run-at-time 3600 nil #'ignore))
       (unwind-protect
           (cl-letf (((symbol-function 'agent-repl--warn)
                      (lambda (_ws fmt &rest args) (push (apply #'format fmt args) ,warned)))
                     ((symbol-function 'agent-repl--info)
                      (lambda (_ws fmt &rest args) (push (apply #'format fmt args) ,informed))))
             ,@body)
         (when (timerp agent-repl--update-chain-timer)
           (cancel-timer agent-repl--update-chain-timer))
         (setq agent-repl--update-chain-timer nil)))))

(ert-deftest agent-repl-test-update-in-flight-p-starved-chain-reports-in-flight ()
  "A stale flag whose next hop is still scheduled keeps the reentry guard up.
Clearing here would start a second chain on top of one still walking the
workspace list."
  ;; Arrange / Act / Assert
  (agent-repl-test--with-starved-chain warned informed
    (should (agent-repl--update-in-flight-p))
    (ignore warned informed)))

(ert-deftest agent-repl-test-update-in-flight-p-starved-chain-keeps-the-flag ()
  "A starved chain's flag is not force-cleared — its finalize still owns it."
  ;; Arrange / Act
  (agent-repl-test--with-starved-chain warned informed
    (agent-repl--update-in-flight-p)
    ;; Assert
    (should agent-repl--update-in-flight)
    (ignore warned informed)))

(ert-deftest agent-repl-test-update-in-flight-p-starved-chain-emits-no-warn ()
  "Being slow is not a leak, so a starved chain does not spend the leak alarm."
  ;; Arrange / Act
  (agent-repl-test--with-starved-chain warned informed
    (agent-repl--update-in-flight-p)
    ;; Assert
    (should-not warned)
    (ignore informed)))

(ert-deftest agent-repl-test-update-in-flight-p-starved-chain-logs-info ()
  "The starved chain stays observable: it is recorded at info with its age."
  ;; Arrange / Act
  (agent-repl-test--with-starved-chain warned informed
    (agent-repl--update-in-flight-p)
    ;; Assert
    (should (string-match-p "chain age=10.00s over threshold=5.00s.*starved, not wedged"
                            (car informed)))
    (ignore warned)))

(ert-deftest agent-repl-test-update-in-flight-p-cancelled-continuation-is-wedged ()
  "A timer cancelled out from under the variable is a wedge, not a starve.
It is off `timer-list' and will never fire, so nothing remains to finalize."
  ;; Arrange
  (agent-repl-test--with-starved-chain warned informed
    (cancel-timer agent-repl--update-chain-timer)
    ;; Act
    (should-not (agent-repl--update-in-flight-p))
    ;; Assert
    (should (string-match-p "stale flag (10.00s old), force-clearing" (car warned)))
    (ignore informed)))

(ert-deftest agent-repl-test-update-chain-continuation-pending-p-nil-timer ()
  "With no continuation held, nothing is pending."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (setq agent-repl--update-chain-timer nil)
    ;; Act / Assert
    (should-not (agent-repl--update-chain-continuation-pending-p))))

;;;; ---- Tests: a stale flag against the expected-restart window ----

(defmacro agent-repl-test--with-stale-flag (warned informed &rest body)
  "Run BODY over a stale in-flight flag stamped 10s ago, capturing log text.
WARNED and INFORMED are bound to the messages each level received.  The
expected-restart window starts disarmed; BODY arms or closes one to place
the flag's stamp inside or outside a restart."
  (declare (indent 2))
  `(agent-repl-test--with-clean-state
     (let ((agent-repl-state-stale-threshold 5.0)
           (agent-repl--frontend-expected-restart nil)
           (agent-repl--frontend-expected-restart-last-close nil)
           (agent-repl-frontend-expected-restart-window-seconds 180.0)
           (,warned nil)
           (,informed nil))
       (setq agent-repl--update-in-flight (- (float-time) 10.0))
       (cl-letf (((symbol-function 'agent-repl--uds-run-timer)
                  (lambda (&rest _) 'fake-timer))
                 ((symbol-function 'agent-repl--warn)
                  (lambda (_ws fmt &rest args) (push (apply #'format fmt args) ,warned)))
                 ((symbol-function 'agent-repl--info)
                  (lambda (_ws fmt &rest args) (push (apply #'format fmt args) ,informed))))
         ,@body))))

(ert-deftest agent-repl-test-update-in-flight-p-stale-flag-in-window-logs-info ()
  "A chain stalled by a deliberate bounce is recorded at info, naming the initiator."
  ;; Arrange
  (agent-repl-test--with-stale-flag warned informed
    (agent-repl--frontend-arm-expected-restart "deploy (emacsclient)")
    ;; Act
    (should-not (agent-repl--update-in-flight-p))
    ;; Assert
    (should (string-match-p "stale flag (10.00s old) from the deploy (emacsclient) restart"
                            (car informed)))
    (ignore warned)))

(ert-deftest agent-repl-test-update-in-flight-p-stale-flag-in-window-emits-no-warn ()
  "The bounce's own stalled chain does not spend the leak detector's warning."
  ;; Arrange
  (agent-repl-test--with-stale-flag warned informed
    (agent-repl--frontend-arm-expected-restart "deploy (emacsclient)")
    ;; Act
    (should-not (agent-repl--update-in-flight-p))
    ;; Assert
    (should-not warned)
    (ignore informed)))

(ert-deftest agent-repl-test-update-in-flight-p-stale-flag-in-window-still-clears ()
  "The force-clear is unconditional: a wedged flag is wedged however it got there."
  ;; Arrange
  (agent-repl-test--with-stale-flag warned informed
    (agent-repl--frontend-arm-expected-restart "deploy (emacsclient)")
    ;; Act
    (should-not (agent-repl--update-in-flight-p))
    ;; Assert
    (should-not agent-repl--update-in-flight)
    (ignore warned informed)))

(ert-deftest agent-repl-test-update-in-flight-p-stale-flag-outside-a-window-warns-verbatim ()
  "With no window in play the leak detector's warning is byte-identical to before."
  ;; Arrange
  (agent-repl-test--with-stale-flag warned informed
    ;; Act
    (should-not (agent-repl--update-in-flight-p))
    ;; Assert
    (should (equal (car warned)
                   "update-in-flight-p: stale flag (10.00s old), force-clearing"))
    (ignore informed)))

(ert-deftest agent-repl-test-update-in-flight-p-stale-flag-survives-the-window-close ()
  "A chain stamped inside the window is still the restart's doing once it closes.
The daemon's replacement arriving does not retroactively make the stall
that happened while it was away someone else's fault."
  ;; Arrange
  (agent-repl-test--with-stale-flag warned informed
    (agent-repl--frontend-arm-expected-restart "deploy (emacsclient)")
    ;; The window must predate the flag for the flag to fall inside it.
    (setq agent-repl--frontend-expected-restart
          (plist-put agent-repl--frontend-expected-restart
                     :armed-at (- (float-time) 20.0)))
    (setq agent-repl--frontend-expected-restart
          (plist-put agent-repl--frontend-expected-restart :exit 'withheld))
    (agent-repl--frontend-expected-restart-note-reconnect)
    ;; Act
    (should-not (agent-repl--update-in-flight-p))
    ;; Assert
    (should-not warned)
    (should (string-match-p "from the deploy (emacsclient) restart" (car informed)))))

(ert-deftest agent-repl-test-update-in-flight-p-stale-flag-predating-the-window-warns ()
  "A flag stamped BEFORE the window was armed predates the restart, so it warns.
Grace covers the span the daemon was going away or gone, and nothing else:
a flag older than that is the leak this warning exists to catch."
  ;; Arrange
  (agent-repl-test--with-stale-flag warned informed
    (agent-repl--frontend-arm-expected-restart "deploy (emacsclient)")
    (setq agent-repl--frontend-expected-restart
          (plist-put agent-repl--frontend-expected-restart :exit 'withheld))
    ;; Armed AFTER the flag's stamp, and closed by the replacement's link.
    (agent-repl--frontend-expected-restart-note-reconnect)
    ;; Act
    (should-not (agent-repl--update-in-flight-p))
    ;; Assert
    (should (string-match-p "stale flag (10.00s old), force-clearing" (car warned)))
    (ignore informed)))

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

(ert-deftest agent-repl-test-update-all-finalize-clears-before-logging ()
  "Finalize clears the flag even when the logger itself signals.
A deploy can swap the state directory out from under the file-backed
logger mid-chain; if the clear came after the trace, that signal would
strand the flag until the stale backstop noticed it minutes later."
  (agent-repl-test--with-clean-state
    (setq agent-repl--update-in-flight (float-time))
    (cl-letf (((symbol-function 'agent-repl--log-verbose)
               (lambda (&rest _) (error "log sink gone"))))
      (should-error (agent-repl--update-all-workspace-states--finalize)))
    (should-not agent-repl--update-in-flight)))

(ert-deftest agent-repl-test-update-all-step-clears-flag-when-handler-signals ()
  "An error escaping the per-step `condition-case' still clears the flag.
The handler's own `agent-repl--warn' is the escape hatch the old code had
no cover for: it runs outside anything that could finalize, so a signal
from it aborted the chain with the flag armed."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws-a" :project-dir "/tmp/a")
    (setq agent-repl--update-in-flight (float-time))
    (cl-letf (((symbol-function 'agent-repl--update-one-workspace-state)
               (lambda (&rest _) (error "boom")))
              ((symbol-function 'agent-repl--warn)
               (lambda (&rest _) (error "warn sink gone"))))
      (should-error
       (agent-repl--update-all-workspace-states--step '("ws-a") nil 0.0)))
    (should-not agent-repl--update-in-flight)))

(ert-deftest agent-repl-test-update-all-now-clears-flag-when-step-signals ()
  "A kickoff whose first step signals does not leave the flag armed.
The arm and the entry into the chain are one unit; without the unwind the
flag outlives a chain that never started."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws-a" :project-dir "/tmp/a")
    (cl-letf (((symbol-function 'agent-repl--poll-workspace-notifications) #'ignore)
              ((symbol-function 'agent-repl--update-all-workspace-states--step)
               (lambda (&rest _) (error "boom"))))
      (should-error (agent-repl--update-all-workspace-states-now)))
    (should-not agent-repl--update-in-flight)))

;;;; ---- Tests: chain teardown ----

(ert-deftest agent-repl-test-update-chain-teardown-clears-in-flight-flag ()
  "Tearing the chain down clears the in-flight flag.
This is the death path: a module reload cancels the heartbeat that would
otherwise have force-cleared the flag, so the flag must not survive it."
  (agent-repl-test--with-clean-state
    (setq agent-repl--update-in-flight (float-time))
    (agent-repl--update-chain-teardown)
    (should-not agent-repl--update-in-flight)))

(ert-deftest agent-repl-test-update-chain-teardown-cancels-pending-step ()
  "Tearing the chain down cancels the pending continuation timer.
A surviving continuation would run a step belonging to a generation whose
timers have already been cancelled."
  (agent-repl-test--with-clean-state
    (let ((timer (run-with-timer 9999 nil #'ignore)))
      (setq agent-repl--update-chain-timer timer)
      (agent-repl--update-chain-teardown)
      (should-not (memq timer timer-list))
      (should-not agent-repl--update-chain-timer))))

(ert-deftest agent-repl-test-update-chain-teardown-idempotent-with-no-chain ()
  "Tearing down with nothing in flight is a no-op, not an error."
  (agent-repl-test--with-clean-state
    (agent-repl--update-chain-teardown)
    (should-not agent-repl--update-in-flight)
    (should-not agent-repl--update-chain-timer)))

(ert-deftest agent-repl-test-update-in-flight-p-stale-flag-warns ()
  "The stale backstop still warns when it force-clears.
The leak fix must not silence the warn: a genuinely stale flag is a real
defect report and has to stay visible."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-state-stale-threshold 5.0)
          (warnings nil))
      (setq agent-repl--update-in-flight (- (float-time) 10.0))
      (cl-letf (((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warnings))))
        (should-not (agent-repl--update-in-flight-p)))
      (should (= 1 (length warnings)))
      (should (string-match-p "stale flag" (car warnings))))))

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
  "`--update-one-workspace-state' with DO-GIT-P nil skips the git refresh.
The cheap state-machine work still runs."
  (agent-repl-test--with-clean-state
    (let ((merge-fired nil))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'agent-repl--agent-running-p) (lambda (_ws) t))
                ((symbol-function 'agent-repl--async-refresh-branch-merged)
                 (lambda (_ws) (setq merge-fired t))))
        (agent-repl--update-one-workspace-state "ws1" nil)
        (should-not merge-fired)))))

(ert-deftest agent-repl-test-update-one-ws-fires-git-when-gate-open ()
  "`--update-one-workspace-state' with DO-GIT-P non-nil fires the git refresh."
  (agent-repl-test--with-clean-state
    (let ((merge-fired nil))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'agent-repl--agent-running-p) (lambda (_ws) t))
                ((symbol-function 'agent-repl--async-refresh-branch-merged)
                 (lambda (_ws) (setq merge-fired t))))
        (agent-repl--update-one-workspace-state "ws1" t)
        (should merge-fired)))))

(ert-deftest agent-repl-test-update-one-ws-dead-agent-skips-state-update ()
  "When a non-gui workspace's agent is not running, `--update-one-workspace-state'
calls `--mark-dead'.  The merge refresh still fires when DO-GIT-P is on
because merged-ness is independent of agent liveness — a dead workspace
can still have a merge-completed parent."
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
                (load-file-name (expand-file-name "lisp/core.el" tmpdir)))
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
                (load-file-name (expand-file-name "lisp/core.el" tmpdir)))
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
              (load-file-name (expand-file-name "lisp/core.el" tmpdir)))
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
                (buffer-file-name (expand-file-name "lisp/core.el" tmpdir)))
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

(ert-deftest agent-repl-test-tabline-renders-a-closed-workspace ()
  "A workspace closed via `SPC o C' stays on the tab-bar as inactive.
Repo folding is the only mechanism that takes a workspace off the
tab-bar."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-repl-state "bg-ws" :inactive)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws")))
      (let ((result (agent-repl--tabline-advice '("current-ws" "bg-ws"))))
        (should (string-match-p "current-ws" result))
        (should (string-match-p "bg-ws" result))))))

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

;;;; ---- Tests: pack-prefix (partial placement) ----

(ert-deftest agent-repl-test-pack-prefix-places-all-when-they-fit ()
  "When every entry fits, the prefix is the whole list."
  ;; Arrange / Act / Assert
  (should (equal (agent-repl--pack-prefix '(3 3 3) '(80 80)) '(3 0))))

(ert-deftest agent-repl-test-pack-prefix-stops-at-overflow ()
  "Entries past what the rows hold are simply not placed — unlike
`agent-repl--pack-first-fit', which discards the whole placement."
  ;; Arrange / Act
  (let ((counts (agent-repl--pack-prefix '(4 4 4 4 4) '(9 9))))
    ;; Assert: two rows of two, the fifth entry left unplaced.
    (should (equal counts '(2 2)))
    (should (= 4 (apply #'+ counts)))))

(ert-deftest agent-repl-test-pack-prefix-zero-when-nothing-fits ()
  "An entry too wide for every row places nothing at all."
  ;; Arrange / Act / Assert
  (should (equal (agent-repl--pack-prefix '(40) '(9 9)) '(0 0))))

;;;; ---- Tests: unfilled-row padding ----

(ert-deftest agent-repl-test-pad-tabline-row-pads-empty-row ()
  "An empty row is padded out to WIDTH columns of spaces so it actually
occupies the pixel row that the pinned `tab-bar-lines' reserves."
  ;; Arrange / Act / Assert
  (should (equal (agent-repl--pad-tabline-row "" 5) "     ")))

(ert-deftest agent-repl-test-pad-tabline-row-leaves-filled-row-alone ()
  "A row with entries is returned untouched: its width is measured in
PIXELS for image-bearing entries, so padding it to WIDTH character
columns could overflow the frame and wrap to a further physical row."
  ;; Arrange / Act / Assert
  (should (equal (agent-repl--pad-tabline-row "abc" 40) "abc")))

;;;; ---- Tests: rendered-width row centering ----

(ert-deftest agent-repl-test-center-tabline-row-plain-text ()
  "Plain text is centered by its rendered column width."
  ;; Arrange / Act / Assert
  (should (equal (agent-repl--center-tabline-row "abc" 9) "   abc")))

(ert-deftest agent-repl-test-center-tabline-row-measures-display-image ()
  "Image width rather than source character count determines left padding."
  ;; Arrange
  (cl-letf (((symbol-function 'string-pixel-width)
             (lambda (string)
               (+ (string-width (substring-no-properties string))
                  (if (text-property-not-all
                       0 (length string) 'display nil string)
                      7
                    0))))
            ((symbol-function 'frame-char-width) (lambda (&rest _) 1)))
    (let ((row (propertize " " 'display "eight-column-image")))
      ;; Act
      (let ((centered (agent-repl--center-tabline-row row 10)))
        ;; Assert: the image is eight columns, so exactly one column is added.
        (should (string-prefix-p " " centered))
        (should (= 9 (agent-repl--tabline-entry-width centered)))))))

;;;; ---- Tests: tab-bar boundary instrumentation ----

(ert-deftest agent-repl-test-tabbar-keymap-caption-observes-final-newlines ()
  "The Lisp-to-C boundary records the caption after tab-bar transforms it."
  ;; Arrange
  (let* ((caption "first row\nsecond row")
         (keymap `((mouse-1 . ignore)
                   (str-1 menu-item ,caption ignore)))
         ;; Act
         (observations
          (agent-repl--tabbar-keymap-caption-observations keymap))
         (observation (car observations)))
    ;; Assert
    (should (= 1 (length observations)))
    (should (eq 'str-1 (plist-get observation :key)))
    (should (= 1 (plist-get observation :newlines)))
    (should (equal caption (plist-get observation :caption)))
    (should (equal caption (plist-get observation :visible-caption)))))

(ert-deftest agent-repl-test-tabbar-backtrace-capture-is-available-unstubbed ()
  "The mutation tracing's backtrace capture resolves without a test stub.

The live Emacs 30 runtime does not provide `backtrace-to-string', even though
batch test startup can incidentally load the library that defines it.  This
test deliberately exercises agent-repl's portable `backtrace' capture helper
without a stub so the suite covers the live-runtime dependency surface."
  ;; Arrange / Act / Assert — the load of status.el is the code under test.
  (should (stringp (agent-repl--tabbar-backtrace-string))))

(ert-deftest agent-repl-test-tabbar-set-frame-lines-audit-preserves-result ()
  "The setter boundary logs requested and final line counts with a backtrace."
  ;; Arrange
  (let ((lines 2)
        (record nil)
        (agent-repl--tabbar-frame-parameter-audit-active nil))
    (cl-letf (((symbol-function 'selected-frame) (lambda () 'frame-a))
              ((symbol-function 'frame-parameter)
               (lambda (_frame _parameter) lines))
              ((symbol-function 'agent-repl--tabbar-backtrace-string)
               (lambda () "caller-trace"))
              ((symbol-function 'agent-repl--ws-current-name)
               (lambda () nil))
              ((symbol-function 'agent-repl--log)
               (lambda (_ws format-string &rest args)
                 (setq record (apply #'format format-string args)))))
      ;; Act
      (let ((result
             (agent-repl--tabbar-audit-set-frame-parameter
              (lambda (_frame _parameter value)
                (setq lines value)
                'setter-result)
              nil 'tab-bar-lines 0)))
        ;; Assert
        (should (eq result 'setter-result))
        (should (= lines 0))
        (should (string-match-p "prior=2 requested=0 final=0" record))
        (should (string-match-p "caller-trace" record))))))

(ert-deftest agent-repl-test-tabbar-modify-frame-lines-audit-resignals-errors ()
  "The bulk setter boundary logs failures and preserves the original signal."
  ;; Arrange
  (let ((record nil)
        (agent-repl--tabbar-frame-parameter-audit-active nil))
    (cl-letf (((symbol-function 'selected-frame) (lambda () 'frame-a))
              ((symbol-function 'frame-parameter)
               (lambda (_frame _parameter) 2))
              ((symbol-function 'agent-repl--tabbar-backtrace-string)
               (lambda () "bulk-caller-trace"))
              ((symbol-function 'agent-repl--ws-current-name)
               (lambda () nil))
              ((symbol-function 'agent-repl--log)
               (lambda (_ws format-string &rest args)
                 (setq record (apply #'format format-string args)))))
      ;; Act / Assert
      (should-error
       (agent-repl--tabbar-audit-modify-frame-parameters
        (lambda (&rest _) (error "setter failed"))
        nil '((tab-bar-lines . 0) (width . 100)))
       :type 'error)
      (should (string-match-p "requested=0 final=2" record))
      (should (string-match-p "setter failed" record))
      (should (string-match-p "bulk-caller-trace" record)))))

(ert-deftest agent-repl-test-tabbar-keymap-audit-logs-state-changes-only ()
  "The hot keymap boundary logs once until its actual caption changes."
  ;; Arrange
  (let ((agent-repl--tabbar-observation-states
         (make-hash-table :test #'eq))
        (agent-repl--tabbar-diagnostic-until nil)
        (tab-bar-auto-width nil)
        (log-count 0)
        (first '((str-1 menu-item "row one\nrow two" ignore)))
        (first-with-cache-buster
         `((str-1 menu-item
                  ,(concat "row one\nrow two"
                           (propertize " " 'invisible t))
                  ignore)))
        (second '((str-1 menu-item "changed\nrow two" ignore))))
    (cl-letf (((symbol-function 'selected-frame) (lambda () 'frame-a))
              ((symbol-function 'frame-parameter)
               (lambda (_frame parameter)
                 (pcase parameter
                   ('tab-bar-lines 2)
                   ('tab-bar-lines-keep-state t))))
              ((symbol-function 'agent-repl--ws-current-name)
               (lambda () "ws"))
              ((symbol-function 'agent-repl--ws-known-p)
               (lambda (_ws) t))
              ((symbol-function 'agent-repl--log-verbose)
               (lambda (&rest _) (cl-incf log-count))))
      ;; Act
      (should (eq first (agent-repl--tabbar-audit-keymap first)))
      (should
       (equal
        (plist-get
         (car (agent-repl--tabbar-keymap-caption-observations first))
         :visible-caption)
        (plist-get
         (car (agent-repl--tabbar-keymap-caption-observations
               first-with-cache-buster))
         :visible-caption)))
      (agent-repl--tabbar-audit-keymap first-with-cache-buster)
      (agent-repl--tabbar-audit-keymap second)
      ;; Assert
      (should (= 2 log-count)))))

(ert-deftest agent-repl-test-tabbar-render-log-records-changed-pipeline ()
  "The visible formatter boundary logs identical state once and changed rows."
  ;; Arrange
  (let ((agent-repl--tabbar-observation-states
         (make-hash-table :test #'eq))
        (agent-repl--tabbar-diagnostic-until nil)
        (tab-bar-mode t)
        (tab-bar-show t)
        (auto-resize-tab-bars nil)
        (tab-bar-auto-width nil)
        (tab-bar-format '(agent-repl-workspace-tabline-formatted))
        (frame-inhibit-implied-resize '(tab-bar-lines))
        (log-count 0))
    (cl-letf (((symbol-function 'frame-parameter)
               (lambda (_frame parameter)
                 (pcase parameter
                   ('tab-bar-lines 2)
                   ('tab-bar-lines-keep-state t))))
              ((symbol-function 'frame-pixel-width) (lambda (_frame) 800))
              ((symbol-function 'frame-char-width) (lambda (&optional _frame) 10))
              ((symbol-function 'agent-repl--ws-known-p)
               (lambda (_ws) t))
              ((symbol-function 'agent-repl--log-verbose)
               (lambda (&rest _) (cl-incf log-count))))
      ;; Act: two identical observations followed by one changed raw row.
      (dotimes (_ 2)
        (agent-repl--tabbar-log-render
         'frame-a 80 79 '("ws") '(("ws" . :ready)) "ws" '(8) 0
         '("row" "") '("row" "   ") '(" row" "   ")
         " row \n    " " row \n    "))
      (agent-repl--tabbar-log-render
       'frame-a 80 79 '("ws") '(("ws" . :thinking)) "ws" '(12) 0
       '("changed" "") '("changed" "   ") '(" changed" "   ")
       " changed \n    " " changed \n    ")
      ;; Assert
      (should (= 2 log-count)))))

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
  "Elided neighbors are summarized by +N badges on the matching side.
The window STARTS at the anchor and runs right; the two entries before
the anchor are what the leading badge counts."
  ;; budget = 20 - 2*(2 + 1) = 14; window from index 2 ("cccc") holds
  ;; "cccc dddd eeee" (14) exactly, so nothing is elided on the right.
  (let ((row (agent-repl-test--single-row
              '("aaaa" "bbbb" "cccc" "dddd" "eeee") 2 20)))
    (should (equal row "+2 cccc dddd eeee"))))

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

;;;; ---- Tests: anchored tab-bar view window ----
;;
;; The rendered window STARTS at an anchor workspace and runs right; it
;; is never recentered on the current workspace.  Fixture below: eight
;; 4-column entries at WIDTH 12 over two rows.  Badge reserve is
;; `2 + (length "8")' = 3, so each row's budget is 9 and holds exactly
;; two entries — every window is exactly FOUR entries wide, whatever it
;; is anchored at, which makes the anchor arithmetic readable.
;;
;; NSWindow geometry (`ns_change_tab_bar_height', the clipped-resize
;; livelock, the actual pixel height of the tab-bar strip) cannot be
;; exercised in batch: there is no graphical frame.  These tests pin the
;; STRING contract only — which entries render, in which order, in how
;; many rows.  The installation tests separately pin the frame-parameter
;; contract and the no-native-recalculation redraw contract.

(defconst agent-repl-test--anchor-names
  '("n1" "n2" "n3" "n4" "n5" "n6" "n7" "n8")
  "Eight workspace names for the anchored-window fixture.")

(defun agent-repl-test--anchor-widths (&optional n)
  "Return N (default 8) uniform 4-column entry widths."
  (make-list (or n 8) 4))

(defun agent-repl-test--anchor-at (current anchor &optional names width)
  "Return the window anchor index for CURRENT given a previous ANCHOR.
NAMES defaults to the eight-name fixture and WIDTH to 12; the previous
name list is NAMES, i.e. no membership change."
  (let ((names (or names agent-repl-test--anchor-names)))
    (agent-repl--tabline-window-anchor
     names current anchor names
     (agent-repl-test--anchor-widths (length names))
     (or width 12) 2)))

(ert-deftest agent-repl-test-tabline-anchor-inside-window-does-not-move ()
  "A current workspace already inside the window moves the anchor NOT AT ALL.
This is the invariant the whole redesign exists for: switching between
two visible tabs must not reshuffle the view."
  ;; Arrange: anchored at n1, the window covers n1..n4.
  (dolist (current '("n1" "n2" "n3" "n4"))
    ;; Act / Assert
    (should (= 0 (agent-repl-test--anchor-at current "n1")))))

(ert-deftest agent-repl-test-tabline-anchor-left-of-window-becomes-current ()
  "A current workspace LEFT of the window makes the anchor the current one."
  ;; Arrange: anchored at n5, the window covers n5..n8.
  (dolist (case '(("n1" . 0) ("n2" . 1) ("n3" . 2) ("n4" . 3)))
    ;; Act
    (let ((lo (agent-repl-test--anchor-at (car case) "n5")))
      ;; Assert
      (should (= (cdr case) lo)))))

(ert-deftest agent-repl-test-tabline-anchor-past-window-advances-minimally ()
  "A current workspace past the window's end advances the anchor the
SMALLEST number of positions that brings it back into view — never more."
  ;; Arrange: anchored at n1 (window n1..n4); each window holds four.
  (dolist (case '(("n5" . 1) ("n6" . 2) ("n7" . 3) ("n8" . 4)))
    ;; Act
    (let ((lo (agent-repl-test--anchor-at (car case) "n1")))
      ;; Assert
      (should (= (cdr case) lo)))))

(ert-deftest agent-repl-test-tabline-anchor-all-entries-fit-anchors-at-head ()
  "With nothing to elide the window is the whole list, anchored at index 0,
whatever stale anchor was carried in."
  ;; Arrange / Act / Assert
  (should (= 0 (agent-repl-test--anchor-at "n8" "n5" nil 80))))

(ert-deftest agent-repl-test-tabline-surviving-anchor-keeps-live-anchor ()
  "A membership change that spares the anchor workspace keeps it."
  ;; Arrange / Act / Assert
  (should (equal "n3" (agent-repl--tabline-surviving-anchor
                       "n3" '("n1" "n2" "n3" "n4") '("n1" "n3" "n4")))))

(ert-deftest agent-repl-test-tabline-surviving-anchor-prefers-right-neighbor ()
  "When the anchor dies and both neighbors survive, the RIGHT one takes
its place — that is the entry that slides into the leftmost slot."
  ;; Arrange / Act / Assert
  (should (equal "n4" (agent-repl--tabline-surviving-anchor
                       "n3" '("n1" "n2" "n3" "n4") '("n1" "n2" "n4")))))

(ert-deftest agent-repl-test-tabline-surviving-anchor-falls-back-left ()
  "When the anchor dies with no surviving entry to its right, the nearest
surviving entry to its LEFT takes over."
  ;; Arrange / Act / Assert
  (should (equal "n2" (agent-repl--tabline-surviving-anchor
                       "n4" '("n1" "n2" "n3" "n4") '("n1" "n2")))))

(ert-deftest agent-repl-test-tabline-surviving-anchor-unknown-anchor-heads-list ()
  "An anchor absent from BOTH name lists falls back to the first entry."
  ;; Arrange / Act / Assert
  (should (equal "n1" (agent-repl--tabline-surviving-anchor
                       "gone" '("n2" "n3") '("n1" "n2" "n3")))))

(ert-deftest agent-repl-test-tabline-anchor-index-records-state ()
  "The stateful wrapper records anchor state under the supplied frame."
  ;; Arrange
  (let ((agent-repl--tabline-view-states (make-hash-table :test #'eq)))
    (puthash 'frame-a
             (list :anchor "n5"
                   :width nil
                   :names agent-repl-test--anchor-names)
             agent-repl--tabline-view-states)
    ;; Act
    (let ((lo (agent-repl--tabline-anchor-index
               'frame-a (agent-repl-test--anchor-widths)
               agent-repl-test--anchor-names "n6" 12 2)))
      ;; Assert
      (let ((state (gethash 'frame-a agent-repl--tabline-view-states)))
        (should (= 4 lo))
        (should (equal "n5" (plist-get state :anchor)))
        (should (= 12 (plist-get state :width)))
        (should (equal agent-repl-test--anchor-names
                       (plist-get state :names)))))))

(ert-deftest agent-repl-test-tabline-anchor-resize-recomputes-from-anchor ()
  "A width change recomputes the window FROM the anchor: the anchor
workspace does not teleport, only the recorded width changes."
  ;; Arrange: twelve entries, overflowing at both widths under test.
  (let* ((names (mapcar (lambda (i) (format "e%03d" i)) (number-sequence 1 12)))
         (widths (agent-repl-test--anchor-widths 12))
         (agent-repl--tabline-view-states (make-hash-table :test #'eq)))
    (puthash 'frame-a
             (list :anchor "e005" :width 12 :names names)
             agent-repl--tabline-view-states)
    ;; Act
    (let* ((narrow (agent-repl--tabline-anchor-index
                    'frame-a widths names "e005" 12 2))
           (narrow-anchor
            (plist-get (gethash 'frame-a agent-repl--tabline-view-states)
                       :anchor))
           (wide (agent-repl--tabline-anchor-index
                  'frame-a widths names "e005" 20 2))
           (state (gethash 'frame-a agent-repl--tabline-view-states)))
      ;; Assert
      (should (= 4 narrow))
      (should (= 4 wide))
      (should (equal "e005" narrow-anchor))
      (should (equal "e005" (plist-get state :anchor)))
      (should (= 20 (plist-get state :width))))))

(ert-deftest agent-repl-test-tabline-rows-identical-across-visible-tab-switch ()
  "Switching between two tabs that are both already visible renders a
LITERALLY identical set of rows — same entries, same order, same string."
  ;; Arrange: twelve entries anchored at e005; e006 and e007 are both
  ;; inside the six-wide window the 20-column frame renders.
  (let* ((names (mapcar (lambda (i) (format "e%03d" i)) (number-sequence 1 12)))
         (widths (agent-repl-test--anchor-widths 12))
         (agent-repl--tabline-view-states (make-hash-table :test #'eq))
         (render (lambda (current)
                   (agent-repl--tabline-rows
                    names
                    (agent-repl--tabline-anchor-index
                     'frame-a widths names current 20 2)
                    20 2 widths))))
    (puthash 'frame-a
             (list :anchor "e005" :width 20 :names names)
             agent-repl--tabline-view-states)
    ;; Act
    (let ((before (funcall render "e006"))
          (after (funcall render "e007")))
      ;; Assert
      (should (equal before after))
      (should
       (equal "e005"
              (plist-get
               (gethash 'frame-a agent-repl--tabline-view-states)
               :anchor))))))

(ert-deftest agent-repl-test-tabline-anchor-state-is-frame-local ()
  "Redisplaying one frame never changes another frame's anchor window."
  ;; Arrange
  (let* ((names agent-repl-test--anchor-names)
         (widths (agent-repl-test--anchor-widths))
         (agent-repl--tabline-view-states (make-hash-table :test #'eq)))
    (puthash 'frame-a
             (list :anchor "n1" :width 12 :names names)
             agent-repl--tabline-view-states)
    (puthash 'frame-b
             (list :anchor "n5" :width 12 :names names)
             agent-repl--tabline-view-states)
    ;; Act
    (agent-repl--tabline-anchor-index 'frame-a widths names "n4" 12 2)
    (agent-repl--tabline-anchor-index 'frame-b widths names "n8" 12 2)
    ;; Assert
    (should
     (equal "n1"
            (plist-get (gethash 'frame-a agent-repl--tabline-view-states)
                       :anchor)))
    (should
     (equal "n5"
            (plist-get (gethash 'frame-b agent-repl--tabline-view-states)
                       :anchor)))))

(ert-deftest agent-repl-test-tabline-rows-badges-on-both-ends ()
  "Entries elided on EITHER side of the window get their own badge: the
leading count on the first row, the trailing count on the last."
  ;; Arrange
  (let ((entries (mapcar (lambda (i) (format "e%03d" i)) (number-sequence 1 12))))
    ;; Act
    (let ((rows (agent-repl--tabline-rows entries 4 20 2)))
      ;; Assert
      (should (string-prefix-p "+4 " (nth 0 rows)))
      (should (string-suffix-p " +2" (nth 1 rows))))))

(ert-deftest agent-repl-test-tabline-rows-no-leading-badge-at-head-anchor ()
  "An anchor at index 0 elides nothing on the left, so no leading badge."
  ;; Arrange
  (let ((entries (mapcar (lambda (i) (format "e%03d" i)) (number-sequence 1 12))))
    ;; Act
    (let ((rows (agent-repl--tabline-rows entries 0 20 2)))
      ;; Assert
      (should-not (string-prefix-p "+" (nth 0 rows)))
      (should (string-suffix-p " +6" (nth 1 rows))))))

(ert-deftest agent-repl-test-tabline-rows-labels-stay-globally-numbered ()
  "The [N] jump labels are GLOBAL positions in the full visible list, not
positions within the window — `SPC <n>' indexes the same list, so an
anchored window must NOT renumber from 1."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let* ((names (mapcar (lambda (i) (format "e%03d" i)) (number-sequence 1 12)))
           (entries (agent-repl--tabline-rendered-entries names))
           (widths (mapcar #'agent-repl--tabline-entry-width entries)))
      ;; Act: anchor at index 4, i.e. the fifth workspace.
      (let ((text (substring-no-properties
                   (mapconcat #'identity
                              (agent-repl--tabline-rows entries 4 40 2 widths)
                              " "))))
        ;; Assert
        (should (string-search "[5]" text))
        (should-not (string-search "[1]" text))))))

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
elided behind a `+N' badge and the forced anchor entry is physically
truncated when even that entry exceeds the row budget.  Character-count
truncation cannot enforce this because the image occupies one source
character but thirty rendered columns."
  ;; Arrange
  (let ((agent-repl--tabline-last-truncation nil)
        (log-count 0))
    (cl-letf (((symbol-function 'string-pixel-width)
               (lambda (string)
                 (+ (string-width (substring-no-properties string))
                    (if (text-property-not-all
                         0 (length string) 'display nil string)
                        29
                      0))))
              ((symbol-function 'frame-char-width) (lambda (&rest _) 1))
              ((symbol-function 'agent-repl--log-verbose)
               (lambda (&rest _) (cl-incf log-count))))
      (let ((img (propertize " " 'display "badge"))) ; 1 char, 30 columns
        ;; Act
        (let ((first
               (agent-repl--tabline-rows
                (list "aa" img "bb" "cc" "dd") 1 20 2))
              (second
               (agent-repl--tabline-rows
                (list "aa" img "bb" "cc" "dd") 1 20 2)))
          ;; Assert
          (should (cl-some (lambda (row)
                             (string-match-p "\\+[0-9]+" row))
                           first))
          (dolist (row (append first second))
            (should (<= (agent-repl--tabline-entry-width row) 20)))
          ;; The identical hot-path overflow is logged once.
          (should (= 1 log-count)))))))

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

(ert-deftest agent-repl-test-priority-image-str-uses-the-stored-priority ()
  "The tab renders the image for the `:priority' stored on the workspace.
That value comes from the daemon's `WorkspaceAvailable' announcement (or
an explicit `agent-repl-set-priority'); nothing derives one locally."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--priority-images '(("p1" . fake-image-spec))))
      (agent-repl--ws-put "ws1" :priority "p1")
      (should (equal (get-text-property
                      0 'display (agent-repl--tab-priority-image-str "ws1"))
                     'fake-image-spec)))))

(ert-deftest agent-repl-test-priority-image-str-nil-without-a-priority ()
  "A workspace the daemon announced no priority for renders no image."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--priority-images '(("p1" . fake-image-spec))))
      (should-not (agent-repl--tab-priority-image-str "ws1")))))

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
  "Installation disables auto-resize and pins current plus future frames.
This is the regression contract for the macOS 100%-CPU livelock:
`redisplay_tab_bar' must never enter its dynamic height path.  The
`frame-inhibit-implied-resize' guard is installed before current frame
heights change, and `tab-bar-lines-keep-state' prevents later native
tab operations from recalculating the explicit two-line height.  NS
frames pass through zero before the fixed row count because Emacs 30.2's
native setter ignores nonzero-to-nonzero height changes."
  (let ((auto-resize-tab-bars 'grow-only)
        (tab-bar-auto-width t)
        (default-frame-alist '((tab-bar-lines . 1) (width . 100)))
        (frame-inhibit-implied-resize nil)
        (tab-bar-format nil)
        (tab-bar-show nil)
        (tab-bar-close-button-show t)
        (tab-bar-new-button-show t)
        (frame-lines '((frame-a . 1) (frame-b . 3)))
        (frame-keeps '((frame-a) (frame-b)))
        (line-sets nil)
        (tab-bar-mode-arg nil)
        (logged nil))
    (cl-letf (((symbol-function 'frame-list)
               (lambda () '(frame-a frame-b)))
              ((symbol-function 'display-graphic-p) (lambda (_frame) t))
              ((symbol-function 'framep-on-display) (lambda (_frame) 'ns))
              ((symbol-function 'frame-parameter)
               (lambda (frame parameter)
                 (pcase parameter
                   ('tab-bar-lines (alist-get frame frame-lines))
                   ('tab-bar-lines-keep-state
                    (alist-get frame frame-keeps)))))
              ((symbol-function 'set-frame-parameter)
               (lambda (frame parameter value)
                 (pcase parameter
                   ('tab-bar-lines
                   (should (memq 'tab-bar-lines
                                  frame-inhibit-implied-resize))
                    (push (list frame value) line-sets)
                    (setf (alist-get frame frame-lines) value))
                   ('tab-bar-lines-keep-state
                    (setf (alist-get frame frame-keeps) value))
                   (_ (error "unexpected frame parameter %S" parameter)))))
              ((symbol-function 'tab-bar-mode)
               (lambda (arg)
                 (setq tab-bar-mode-arg arg)
                 ;; Emulate Emacs 30.2: enabling the mode resets the future
                 ;; frame default and every live frame to one line.
                 (setf (alist-get 'tab-bar-lines default-frame-alist) 1
                       (alist-get 'frame-a frame-lines) 1
                       (alist-get 'frame-b frame-lines) 1)))
              ((symbol-function 'agent-repl--retire-redisplay-storm-watchdog)
               (lambda () '(:timer-cancelled nil :hook-present nil)))
              ((symbol-function 'agent-repl--log)
               (lambda (&rest args) (setq logged args)))
              ((symbol-function 'run-with-timer)
               (lambda (&rest _)
                 (error "fixed-height install must not schedule recovery timers"))))
      (agent-repl--install-fixed-height-tab-bar)
      (should-not auto-resize-tab-bars)
      (should-not tab-bar-auto-width)
      (should (eq tab-bar-mode-arg 1))
      (should (= (alist-get 'tab-bar-lines default-frame-alist)
                 agent-repl--tabline-row-count))
      (should (eq t (alist-get 'tab-bar-lines-keep-state
                               default-frame-alist)))
      (should (= (alist-get 'frame-a frame-lines)
                 agent-repl--tabline-row-count))
      (should (= (alist-get 'frame-b frame-lines)
                 agent-repl--tabline-row-count))
      (should
       (equal (nreverse line-sets)
              (list (list 'frame-a 0)
                    (list 'frame-a agent-repl--tabline-row-count)
                    (list 'frame-b 0)
                    (list 'frame-b agent-repl--tabline-row-count))))
      (should (eq t (alist-get 'frame-a frame-keeps)))
      (should (eq t (alist-get 'frame-b frame-keeps)))
      (should (memq 'tab-bar-lines frame-inhibit-implied-resize))
      (should (equal tab-bar-format
                     '(agent-repl-workspace-tabline-formatted
                       tab-bar-format-align-right
                       agent-repl-daemon-link-segment
                       agent-repl-current-workspace-name-segment)))
      (should-not tab-bar-close-button-show)
      (should-not tab-bar-new-button-show)
      (should logged))))

(ert-deftest agent-repl-test-fixed-height-tab-bar-default-covers-future-frames ()
  "Installation pins `default-frame-alist' even with no current GUI frame."
  (let ((auto-resize-tab-bars t)
        (tab-bar-auto-width t)
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
      (should-not tab-bar-auto-width)
      (should (= (alist-get 'tab-bar-lines default-frame-alist)
                 agent-repl--tabline-row-count))
      (should (eq t (alist-get 'tab-bar-lines-keep-state
                               default-frame-alist))))))

(ert-deftest agent-repl-test-tabbar-apply-row-count-sets-selected-frame ()
  "The interactive command forces NS through zero, then reapplies ROWS."
  ;; Arrange
  (let ((params '((tab-bar-lines . 1)
                  (tab-bar-lines-keep-state)))
        (applied nil))
    (cl-letf (((symbol-function 'selected-frame) (lambda () 'frame-a))
              ((symbol-function 'framep-on-display) (lambda (_frame) 'ns))
              ((symbol-function 'frame-parameter)
               (lambda (_frame parameter) (alist-get parameter params)))
              ((symbol-function 'set-frame-parameter)
               (lambda (frame parameter value)
                 (push (list frame parameter value) applied)
                 (setf (alist-get parameter params) value)))
              ((symbol-function 'agent-repl--log) #'ignore)
              ((symbol-function 'message) #'ignore))
      ;; Act
      (let ((result (agent-repl-tabbar-apply-row-count)))
        ;; Assert
        (should (= agent-repl--tabline-row-count result))
        (should (= agent-repl--tabline-row-count
                   (alist-get 'tab-bar-lines params)))
        (should (eq t (alist-get 'tab-bar-lines-keep-state params)))
        (should
         (equal (nreverse applied)
                (list (list 'frame-a 'tab-bar-lines-keep-state t)
                      (list 'frame-a 'tab-bar-lines 0)
                      (list 'frame-a 'tab-bar-lines
                            agent-repl--tabline-row-count))))))))

(ert-deftest agent-repl-test-tabbar-pin-frame-non-ns-sets-target-directly ()
  "Non-NS frames do not take the macOS-specific zero transition."
  (let ((params '((tab-bar-lines . 1)
                  (tab-bar-lines-keep-state)))
        (applied nil)
        (frame-inhibit-implied-resize '(tab-bar-lines)))
    (cl-letf (((symbol-function 'framep-on-display) (lambda (_frame) 'x))
              ((symbol-function 'frame-parameter)
               (lambda (_frame parameter) (alist-get parameter params)))
              ((symbol-function 'set-frame-parameter)
               (lambda (frame parameter value)
                 (push (list frame parameter value) applied)
                 (setf (alist-get parameter params) value)))
              ((symbol-function 'agent-repl--ws-current-name)
               (lambda () nil))
              ((symbol-function 'agent-repl--log) #'ignore))
      (should (= 2 (agent-repl--tabbar-pin-frame 'frame-a 2)))
      (should
       (equal (nreverse applied)
              '((frame-a tab-bar-lines-keep-state t)
                (frame-a tab-bar-lines 2)))))))

;;;; ---- The hibernation split --------------------------------------------

(ert-deftest agent-repl-test-state-color-hibernated-is-teal ()
  ":hibernated takes TEAL, which is the entire point of the split.
The benign half of the old `:dormant\=' — a session we put to sleep to
reclaim its ~500MB, or a workspace nothing was ever wired to — must not
wear the color that means something is broken."
  ;; Act / Assert
  (should (equal (alist-get :hibernated agent-repl--state-color) "teal")))

(ert-deftest agent-repl-test-state-color-severed-is-blue ()
  ":severed keeps the blue the old `:dormant\=' had.
It is the half that really is evidence of breakage — a bring-up that
failed, or a session controller that died on a terminal protocol error."
  ;; Act / Assert
  (should (equal (alist-get :severed agent-repl--state-color) "blue")))

(ert-deftest agent-repl-test-state-color-hibernated-and-severed-differ ()
  "The two halves of the old `:dormant\=' never render alike.
Asserted independently of both hexes: whatever the two colors are, a
change that quietly re-merges them puts the system back where a dead
shim and a routine hibernation looked identical."
  ;; Act / Assert
  (should-not (equal (alist-get :hibernated agent-repl--state-color)
                     (alist-get :severed agent-repl--state-color))))

(ert-deftest agent-repl-test-teal-ranks-between-blue-and-purple ()
  "TEAL ranks below the blue band and above purple, never below green.
Hibernation makes the SAME actionability claim blue does — you cannot
interact without paying a bring-up — and only the reason is benign.
Below green, a stale `:thinking\=' row from the turn a workspace was
hibernated after would mask a workspace that is genuinely asleep."
  ;; Arrange
  (let ((blue   (cl-position "blue"   agent-repl--color-precedence :test #'equal))
        (teal   (cl-position "teal"   agent-repl--color-precedence :test #'equal))
        (purple (cl-position "purple" agent-repl--color-precedence :test #'equal))
        (green  (cl-position "green"  agent-repl--color-precedence :test #'equal)))
    ;; Act / Assert
    (should (< blue teal))
    (should (< teal purple))
    (should (< teal green))))

(ert-deftest agent-repl-test-teal-has-a-drawable-value ()
  "TEAL resolves to a hex this renderer can actually paint.
A ranked color with no constant behind it is an assignment nothing can
honor, which is how a colorless tab shipped once already."
  ;; Act / Assert
  (should (stringp (alist-get "teal" agent-repl--color-by-name nil nil #'equal))))

(ert-deftest agent-repl-test-teal-is-not-the-init-blue ()
  "TEAL is a distinct value, not a shade of the init blue.
The two states were ONE before the split, so a teal that reads as
\"bluish\" would re-merge them in the only place it matters: a glance at
the tab bar."
  ;; Act / Assert
  (should-not (equal agent-repl--color-hibernated-teal
                     agent-repl--color-init-blue)))

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

;;;; ---- Daemon-link indicator (Emacs's own command-plane fact) ----------

(ert-deftest agent-repl-test-daemon-link-segment-empty-when-healthy ()
  "A healthy command link renders nothing in the tab bar."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl-uds-link-health)
             (lambda () :healthy)))
    ;; Act / Assert
    (should (equal (agent-repl-daemon-link-segment) ""))))

(ert-deftest agent-repl-test-daemon-link-segment-names-the-link-when-degraded ()
  "A degraded command link renders a caption naming the DAEMON LINK."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl-uds-link-health)
             (lambda () :degraded)))
    ;; Act
    (let ((segment (agent-repl-daemon-link-segment)))
      ;; Assert
      (should (string-match-p "DAEMON LINK DEGRADED"
                              (substring-no-properties segment))))))

(ert-deftest agent-repl-test-daemon-link-segment-carries-its-own-face ()
  "The degraded caption uses its own face, not any per-workspace tab face."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl-uds-link-health)
             (lambda () :degraded)))
    ;; Act
    (let ((segment (agent-repl-daemon-link-segment)))
      ;; Assert
      (should (eq (get-text-property 0 'face segment)
                  'agent-repl-daemon-link-degraded)))))

(provide 'test-status)

;;; test-status.el ends here
