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

(ert-deftest agent-repl-test-ws-set-agent-state-nil-ws-errors ()
  "ws-set-agent-state signals error on nil workspace."
  (should-error (agent-repl--ws-set-agent-state nil :thinking) :type 'error))

(ert-deftest agent-repl-test-ws-set-repl-state-nil-ws-errors ()
  "ws-set-repl-state signals error on nil workspace."
  (should-error (agent-repl--ws-set-repl-state nil :inactive) :type 'error))

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

;;;; ---- Tests: the palette-row builder ----

(ert-deftest agent-repl-test-tab-palette-row-carries-the-face ()
  "The builder puts the caller\='s face on the row."
  ;; Arrange
  (let ((row (agent-repl--tab-palette-row 'agent-repl-tab-done "#123456" "white")))
    ;; Act / Assert
    (should (eq 'agent-repl-tab-done (plist-get row :face)))))

(ert-deftest agent-repl-test-tab-palette-row-unselected-paints-the-color ()
  "The unselected look takes the state color as its background."
  ;; Arrange
  (let ((row (agent-repl--tab-palette-row 'agent-repl-tab-done "#123456" "white")))
    ;; Act / Assert
    (should (equal "#123456" (plist-get (plist-get row :unselected) :bg)))))

(ert-deftest agent-repl-test-tab-palette-row-unselected-takes-the-given-foreground ()
  "The unselected foreground is the caller\='s, since no luminance rule
picks the right one for all six colors."
  ;; Arrange
  (let ((row (agent-repl--tab-palette-row 'agent-repl-tab-done "#123456" "black")))
    ;; Act / Assert
    (should (equal "black" (plist-get (plist-get row :unselected) :fg)))))

(ert-deftest agent-repl-test-tab-palette-row-unselected-bracket-numeral-is-the-default ()
  "Every unselected bracket numeral is `agent-repl--color-default-bracket\='."
  ;; Arrange
  (let ((row (agent-repl--tab-palette-row 'agent-repl-tab-done "#123456" "white")))
    ;; Act / Assert
    (should (equal agent-repl--color-default-bracket
                   (plist-get (plist-get row :unselected) :bracket-fg)))))

(ert-deftest agent-repl-test-tab-palette-row-unselected-has-no-bracket-bg ()
  "The unselected look carries NO `:bracket-bg\=', so the entry is one color.
The renderer falls back to `:bg\=' only when the key is absent, and a row
that set it would paint a two-color entry."
  ;; Arrange
  (let ((row (agent-repl--tab-palette-row 'agent-repl-tab-done "#123456" "white")))
    ;; Act / Assert
    (should-not (plist-member (plist-get row :unselected) :bracket-bg))))

(ert-deftest agent-repl-test-tab-palette-row-selected-brackets-the-state-color ()
  "The selected look puts the state color on the [N] bracket.
Selection dims the name to the shared grey, so the bracket there is the
only place the state color can still be read."
  ;; Arrange
  (let ((row (agent-repl--tab-palette-row 'agent-repl-tab-done "#123456" "white")))
    ;; Act / Assert
    (should (equal "#123456" (plist-get (plist-get row :selected) :bracket-bg)))))

(ert-deftest agent-repl-test-tab-palette-row-selected-takes-the-shared-grey ()
  "The selected look\='s background is the shared grey for every state."
  ;; Arrange
  (let ((row (agent-repl--tab-palette-row 'agent-repl-tab-done "#123456" "white")))
    ;; Act / Assert
    (should (equal agent-repl--color-selected-bg
                   (plist-get (plist-get row :selected) :bg)))))

(ert-deftest agent-repl-test-tab-palette-row-weight-is-the-shared-one ()
  "Both looks take `agent-repl--tab-weight\=', which no row has ever varied."
  ;; Arrange
  (let ((row (agent-repl--tab-palette-row 'agent-repl-tab-done "#123456" "white")))
    ;; Act / Assert
    (should (equal agent-repl--tab-weight
                   (plist-get (plist-get row :unselected) :weight)))
    (should (equal agent-repl--tab-weight
                   (plist-get (plist-get row :selected) :weight)))))

(ert-deftest agent-repl-test-tab-palette-every-row-has-the-builder-shape ()
  "Every palette row carries its state color on BOTH looks.
The selected bracket repeats the unselected background for all twenty
rows, which is the shape the builder guarantees and the thing a
hand-written row could silently drop."
  ;; Act / Assert
  (dolist (entry agent-repl--tab-palette)
    (let ((row (cdr entry)))
      (should (equal (plist-get (plist-get row :unselected) :bg)
                     (plist-get (plist-get row :selected) :bracket-bg))))))

(ert-deftest agent-repl-test-tab-palette-no-row-splits-its-entry ()
  "No palette row paints its [N] bracket differently from its name region.
An entry is one color end to end, and a second color inside one entry
would be a second vocabulary saying what the state color already says."
  ;; Act / Assert
  (dolist (entry agent-repl--tab-palette)
    (should-not (plist-member (plist-get (cdr entry) :unselected) :bracket-bg))))

(ert-deftest agent-repl-test-tab-palette-has-no-merge-conflict-entry ()
  "`:merge-conflict' has no palette row: a conflict is waiting on the USER,
which is the opposite of the in-flight claim the merging states' purple makes."
  (should-not (alist-get :merge-conflict agent-repl--tab-palette)))

(ert-deftest agent-repl-test-tab-palette-has-no-merge-failed-entry ()
  "`:merge-failed' likewise has no palette row: nothing is in flight."
  (should-not (alist-get :merge-failed agent-repl--tab-palette)))

(ert-deftest agent-repl-test-tab-spec-merging-is-purple ()
  "A merge the daemon is RUNNING paints the tab purple.
Purple is the tab bar\='s merge color: the work in flight is the SYSTEM\='s
rather than the agent\='s, and red would have claimed a turn was running."
  ;; Act / Assert
  (should (equal agent-repl--color-merging-purple
                 (plist-get (agent-repl--tab-spec :merging nil) :bg))))

(ert-deftest agent-repl-test-tab-spec-merge-enqueuing-is-purple ()
  "A merge on its way into the queue is already in flight, so it is purple."
  ;; Act / Assert
  (should (equal agent-repl--color-merging-purple
                 (plist-get (agent-repl--tab-spec :merge-enqueuing nil) :bg))))

(ert-deftest agent-repl-test-tab-spec-merge-queued-is-purple ()
  "A merge waiting behind a sibling is in flight from the user\='s side.
They can no more act on the workspace than during the merge itself."
  ;; Act / Assert
  (should (equal agent-repl--color-merging-purple
                 (plist-get (agent-repl--tab-spec :merge-queued nil) :bg))))

(ert-deftest agent-repl-test-tab-spec-vendor-blocked-is-blue ()
  "A vendor-blocked tab paints BLUE, not the purple every other surface gives it.
Purple is spent on the merge pipeline here, and a tab bar carries no
glyph to tell two purples apart."
  ;; Act / Assert
  (should (equal agent-repl--color-init-blue
                 (plist-get (agent-repl--tab-spec :vendor-blocked nil) :bg))))

(ert-deftest agent-repl-test-tab-spec-vendor-blocked-is-not-purple ()
  "Nothing but the merge states may paint purple on this surface.
The collision is the whole reason vendor-blocked moved, so it is pinned
directly rather than left implied by the blue assertion."
  ;; Act / Assert
  (should-not (equal agent-repl--color-merging-purple
                     (plist-get (agent-repl--tab-spec :vendor-blocked nil) :bg))))

(ert-deftest agent-repl-test-tab-spec-merging-bracket-inherits-the-name-color ()
  "A merging entry is ONE color end to end.
The [N] bracket takes no color of its own, so the tab reads as a single
purple region rather than a two-color entry."
  ;; Act / Assert
  (should-not (plist-get (agent-repl--tab-spec :merging nil) :bracket-bg)))

(ert-deftest agent-repl-test-tab-spec-merging-selected-brackets-purple ()
  "A SELECTED merging tab keeps purple on the [N] bracket.
Selection dims the name region to the shared grey for every state, so the
bracket is the only place the merge can still be read."
  ;; Arrange
  (let ((spec (agent-repl--tab-spec :merging t)))
    ;; Act / Assert
    (should (equal agent-repl--color-selected-bg (plist-get spec :bg)))
    (should (equal agent-repl--color-merging-purple (plist-get spec :bracket-bg)))))

(ert-deftest agent-repl-test-tab-spec-bracket-only-merging-is-purple ()
  "A merging tab whose panels are dismissed keeps purple on the bracket.
The bracket-only path is what a workspace the user closed the panels on
renders with, and a merge must stay visible through it."
  ;; Arrange
  (let ((spec (agent-repl--tab-spec-bracket-only :merging nil)))
    ;; Act / Assert
    (should (equal 'unspecified (plist-get spec :bg)))
    (should (equal agent-repl--color-merging-purple (plist-get spec :bracket-bg)))))

(ert-deftest agent-repl-test-tab-spec-bracket-only-vendor-blocked-is-blue ()
  "A vendor-blocked tab with panels dismissed keeps BLUE on the bracket.
The bracket is the only colored region left there, so it has to carry
the color this surface actually assigns the state."
  ;; Arrange
  (let ((spec (agent-repl--tab-spec-bracket-only :vendor-blocked nil)))
    ;; Act / Assert
    (should (equal agent-repl--color-init-blue (plist-get spec :bracket-bg)))))

(ert-deftest agent-repl-test-tab-spec-bracket-only-ready-stays-green ()
  "A `:ready\=' tab with panels dismissed keeps green on the bracket."
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

;;;; ---- Tests: Legacy wrappers still populate both axes ----

;;;; ---- Tests: Workspace state accessors (ws-set, ws-clear, ws-state) ----

(ert-deftest agent-repl-test-ws-set-nil-error ()
  "ws-set with nil workspace should signal an error."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--ws-set nil :thinking) :type 'error)))

(ert-deftest agent-repl-test-ws-clear-nil-error ()
  "`ws-clear' with nil ws should signal error."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--ws-agent-state-clear-if nil :done) :type 'error)))

;;;; ---- Tests: Tabline rendering ----

;;;; ---- Tests: ws-bracket-state ignores panel visibility ----

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

;;;; ---- Tests: ws-state edge cases ----

;;;; ---- Tests: ws-set edge cases ----

;;;; ---- Tests: ws-clear-if-status edge cases ----

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

;;;; ---- Tests: tabline-advice edge cases ----

(ert-deftest agent-repl-test-tabline-advice-empty-names ()
  "tabline-advice with an empty names list should return an empty string."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--tabline-space-toggle nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
        (let ((result (agent-repl--tabline-advice '())))
          (should (equal result "")))))))

;;;; ---- Tests: tabline space toggle ----

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

;;;; ---- Tests: mark-dead ----

;; The mark-dead → :dead display-state test was deleted in the agent-shim
;; cutover (design §10): `agent-repl--mark-dead' still sets `:repl-state
;; :dead' (asserted by `agent-repl-test-mark-dead-clears-agent-state'),
;; but that no longer drives `--ws-render-status' / `--ws-display-state',
;; which now read the daemon-pushed `:pushed-render-state'.  The daemon
;; pushes RENDER_STATE_DEAD for the dead badge.

;;;; ---- Tests: status-react-to-pushed-death ----

;;;; ---- Tests: on-frame-focus ----

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

;;;; ---- Tests: update-all-workspace-states multi-workspace dispatch ----

;;;; ---- Tests: mod-N git tick gate ----

;;;; ---- Tests: chain teardown ----

;;;; ---- Tests: mid-chain ws removal ----

;;;; ---- Tests: per-step error isolation ----

;;;; ---- Tests: per-workspace step ----

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

;;;; ---- Tests: :vendor-blocked palette resolution ----

(ert-deftest agent-repl-test-tab-spec-vendor-blocked-unselected ()
  "tab-spec for :vendor-blocked unselected returns the blue plist."
  (let ((spec (agent-repl--tab-spec :vendor-blocked nil)))
    (should (equal (plist-get spec :bg) agent-repl--color-init-blue))
    (should (equal (plist-get spec :fg) agent-repl--color-light))))

(ert-deftest agent-repl-test-tab-spec-vendor-blocked-selected ()
  "tab-spec for :vendor-blocked selected keeps the blue on the bracket."
  (let ((spec (agent-repl--tab-spec :vendor-blocked t)))
    (should (equal (plist-get spec :bg) agent-repl--color-selected-bg))
    (should (equal (plist-get spec :bracket-bg)
                   agent-repl--color-init-blue))))

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
`agent-repl--kill-one-workspace' during the merge-completed close) hits
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

;;;; ---- The two context cuts --------------------------------------------

;;;; ---- Daemon-link indicator (Emacs's own command-plane fact) ----------


;;;; ---- The roster is the state ----------------------------------------

(defmacro agent-repl-test-status--with-arm (ws arm &rest body)
  "Run BODY with WS's roster status arm answered as ARM."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'agent-repl-roster-status-for-ws)
              (lambda (name) (if (equal name ,ws) ,arm nil))))
     ,@body))

(ert-deftest agent-repl-test-status-tab-state-is-the-rows-arm ()
  "A workspace's tab state is its roster row's status arm."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-status--with-arm "alpha" :thinking
      ;; Act / Assert
      (should (eq (agent-repl-status-tab-state "alpha") :thinking)))))

(ert-deftest agent-repl-test-status-tab-state-is-nil-before-the-first-push ()
  "A workspace the roster has not spoken about is drawn uncoloured."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-status--with-arm "alpha" :ready
      ;; Act / Assert
      (should (null (agent-repl-status-tab-state "beta"))))))

;;;; ---- The five colour classes, arm by arm ----------------------------

(defun agent-repl-test-status--arms-taking (color)
  "Return every arm the tab bar paints COLOR."
  (mapcar #'car
          (cl-remove-if-not (lambda (row) (equal (cdr row) color))
                            agent-repl-status-tab-bar-color-table)))

(ert-deftest agent-repl-test-status-every-arm-has-a-tab-colour ()
  "All 23 arms answer with a colour: an unpainted dot is not a state."
  ;; Act / Assert
  (dolist (arm agent-repl-wire-roster-row-status-keywords)
    (should (stringp (agent-repl-status-tab-color arm)))))

(ert-deftest agent-repl-test-status-the-blue-band-is-the-compromised-route ()
  "Blue is every way the route to a working session is compromised —
`vendor_blocked' included, by this surface's declared override."
  ;; Act / Assert
  (should (equal (sort (agent-repl-test-status--arms-taking "blue") #'string<)
                 (sort (list :init :severed :dead :degraded :start-failed
                             :vendor-blocked)
                       #'string<))))

(ert-deftest agent-repl-test-status-purple-is-the-in-flight-merge ()
  "Purple is spent on the three merge arms with no verdict yet, and on
nothing else: a surface with no status word can carry one purple."
  ;; Act / Assert
  (should (equal (sort (agent-repl-test-status--arms-taking "purple") #'string<)
                 (sort (list :merge-enqueuing :merge-queued :merging) #'string<))))

(ert-deftest agent-repl-test-status-red-is-the-agent-holding-the-turn ()
  "Red is a turn in flight."
  ;; Act / Assert
  (should (equal (sort (agent-repl-test-status--arms-taking "red") #'string<)
                 (sort (list :submitting :thinking :clearing :compacting)
                       #'string<))))

(ert-deftest agent-repl-test-status-yellow-is-detached-work-alone ()
  "Yellow is the one state between a running turn and nothing running."
  ;; Act / Assert
  (should (equal (agent-repl-test-status--arms-taking "yellow") '(:idle-async))))

(ert-deftest agent-repl-test-status-green-is-the-session-yours-to-use ()
  "Green covers ready, done, interrupted and permission alike: a pending
permission means the agent is ready for the user."
  ;; Act / Assert
  (should (equal (sort (agent-repl-test-status--arms-taking "green") #'string<)
                 (sort (list :ready :done :interrupted :permission) #'string<))))

(ert-deftest agent-repl-test-status-none-is-a-real-answer ()
  "The settled merge arms and the two sessionless arms take no colour."
  ;; Act / Assert
  (should (equal (sort (agent-repl-test-status--arms-taking "none") #'string<)
                 (sort (list :none :inactive :merge-conflict :merge-failed :merged)
                       #'string<))))

(ert-deftest agent-repl-test-status-an-unknown-arm-paints-nothing ()
  "An arm this build does not know is a breach the codec already refused;
reaching the palette with one paints nothing rather than guessing."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--error) #'ignore))
    ;; Act / Assert
    (should (equal (agent-repl-status-tab-color :not-an-arm) "none"))))

;;;; ---- Glyphs ----------------------------------------------------------

(ert-deftest agent-repl-test-status-a-merge-arm-draws-its-glyph ()
  "The merge arms carry no lifecycle colour, so the glyph is their report."
  ;; Arrange
  (agent-repl-test--with-clean-state
    ;; Act / Assert
    (should (equal (agent-repl-status-tab-glyph "alpha" :merge-conflict)
                   (alist-get :merge-conflict agent-repl-status-merge-glyphs)))))

(ert-deftest agent-repl-test-status-an-inactive-row-draws-a-question-mark ()
  "A perspective-less workspace has no lifecycle a dot could report."
  ;; Arrange
  (agent-repl-test--with-clean-state
    ;; Act / Assert
    (should (equal (agent-repl-status-tab-glyph "alpha" :inactive)
                   agent-repl-status-inactive-glyph))))

(ert-deftest agent-repl-test-status-an-ordinary-arm-draws-no-glyph ()
  "A lifecycle arm reports itself with its colour."
  ;; Arrange
  (agent-repl-test--with-clean-state
    ;; Act / Assert
    (should (null (agent-repl-status-tab-glyph "alpha" :thinking)))))

(ert-deftest agent-repl-test-status-an-attention-marker-draws-a-glyph ()
  "An unseen notification is the marker, on any lifecycle arm."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl-status--marker-on (make-hash-table :test 'equal)))
      (puthash "alpha" t agent-repl-status--marker-on)
      ;; Act / Assert
      (should (equal (agent-repl-status-tab-glyph "alpha" :thinking)
                     agent-repl-status-attention-glyph)))))

;;;; ---- The blink cadence (frontend.v1 RosterRowAttention) --------------

(defmacro agent-repl-test-status--capturing-timers (var &rest body)
  "Run BODY recording every `run-with-timer' delay and thunk onto VAR."
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'run-with-timer)
                (lambda (delay _repeat fn &rest args)
                  (push (list delay fn args) ,var)
                  (timer-create)))
               ((symbol-function 'agent-repl--register-timer)
                (lambda (_key timer) timer))
               ((symbol-function 'agent-repl--cancel-timer-key) #'ignore))
       ,@body)))

(ert-deftest agent-repl-test-status-blink-schedules-five-steps ()
  "Two blinks then steady is five marker changes, not four and not six."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-status--capturing-timers steps
      ;; Act
      (agent-repl-status-blink-tab "alpha")
      ;; Assert
      (should (equal (length (nreverse steps)) 5)))))

(ert-deftest agent-repl-test-status-blink-uses-the-canonical-instants ()
  "ON at 0 ms, OFF at 500, ON at 1000, OFF at 1500, steady from 2000 —
exactly the cadence specified once on frontend.v1 RosterRowAttention."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-status--capturing-timers steps
      ;; Act
      (agent-repl-status-blink-tab "alpha")
      ;; Assert
      (should (equal (mapcar #'car (nreverse steps)) '(0.0 0.5 1.0 1.5 2.0))))))

(ert-deftest agent-repl-test-status-blink-alternates-the-marker ()
  "The marker goes on, off, on, off, then steady ON."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-status--capturing-timers steps
      ;; Act
      (agent-repl-status-blink-tab "alpha")
      ;; Assert
      (should (equal (mapcar (lambda (step) (cadr (nth 2 step))) (nreverse steps))
                     '(t nil t nil t))))))

(ert-deftest agent-repl-test-status-blink-targets-the-named-workspace ()
  "Every step is about the workspace the notification named."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-status--capturing-timers steps
      ;; Act
      (agent-repl-status-blink-tab "alpha")
      ;; Assert
      (should (cl-every (lambda (step) (equal (car (nth 2 step)) "alpha"))
                        (nreverse steps))))))

(ert-deftest agent-repl-test-status-blink-restarts-under-a-second-call ()
  "A second call while blinking RESTARTS the cadence: each step is armed
under a deterministic per-workspace key that replaces its predecessor."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((keys nil))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (&rest _) (timer-create)))
                ((symbol-function 'agent-repl--register-timer)
                 (lambda (key timer) (push key keys) timer)))
        (agent-repl-status-blink-tab "alpha")
        ;; Act
        (agent-repl-status-blink-tab "alpha"))
      ;; Assert — ten registrations over five distinct keys.
      (should (equal (length (delete-dups (copy-sequence keys))) 5)))))

(ert-deftest agent-repl-test-status-a-blink-step-draws-the-marker ()
  "The step's effect is the marker, and it repaints."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl-status--marker-on (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'agent-repl--force-tab-bar-redraw) #'ignore))
        ;; Act
        (agent-repl-status--set-marker "alpha" t)
        ;; Assert
        (should (agent-repl-status-attention-visible-p "alpha"))))))

(ert-deftest agent-repl-test-status-clearing-attention-removes-the-marker ()
  "The marker is cleared when it leaves the row — the daemon retracts it
on SelectWorkspace and re-pushes the roster."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl-status--marker-on (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'agent-repl--force-tab-bar-redraw) #'ignore)
                ((symbol-function 'agent-repl--cancel-timer-key) #'ignore))
        (puthash "alpha" t agent-repl-status--marker-on)
        ;; Act
        (agent-repl-status-clear-attention "alpha")
        ;; Assert
        (should-not (agent-repl-status-attention-visible-p "alpha"))))))

(ert-deftest agent-repl-test-status-clearing-attention-cancels-the-blink ()
  "A blink still in flight must not paint a marker already retracted."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((cancelled nil))
      (cl-letf (((symbol-function 'agent-repl--force-tab-bar-redraw) #'ignore)
                ((symbol-function 'agent-repl--cancel-timer-key)
                 (lambda (key) (push key cancelled))))
        ;; Act
        (agent-repl-status-clear-attention "alpha")
        ;; Assert
        (should (equal (length cancelled)
                       (length agent-repl-status-blink-schedule)))))))


;;;; ---- Following the roster's attention marker -------------------------

(defmacro agent-repl-test-status--syncing-attention (attention &rest body)
  "Run BODY with a one-row roster walk whose row's attention is ATTENTION."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-repl-roster-walk)
              (lambda (_roster) (list (list :row (list :attention ,attention)))))
             ((symbol-function 'agent-repl-roster-row-id) (lambda (_row) "id-alpha"))
             ((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) "alpha"))
             ((symbol-function 'agent-repl--force-tab-bar-redraw) #'ignore)
             ((symbol-function 'agent-repl--cancel-timer-key) #'ignore))
     ,@body))

(ert-deftest agent-repl-test-status-an-arriving-marker-blinks ()
  "A marker that ARRIVES runs the canonical cadence — the cadence IS
blink-then-steady, stated on frontend.v1 RosterRowAttention at the marker."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl-status--marker-on (make-hash-table :test 'equal))
          (blinked nil))
      (agent-repl-test-status--syncing-attention t
        (cl-letf (((symbol-function 'agent-repl-status-blink-tab)
                   (lambda (ws) (push ws blinked))))
          ;; Act
          (agent-repl-status-sync-attention 'roster)))
      ;; Assert
      (should (equal blinked '("alpha"))))))

(ert-deftest agent-repl-test-status-a-persisting-marker-does-not-re-blink ()
  "A marker still standing across a re-push is left alone: re-blinking
would blink at the daemon's push rate rather than the notification's."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl-status--marker-on (make-hash-table :test 'equal))
          (blinked nil))
      (puthash "alpha" t agent-repl-status--marker-on)
      (agent-repl-test-status--syncing-attention t
        (cl-letf (((symbol-function 'agent-repl-status-blink-tab)
                   (lambda (ws) (push ws blinked))))
          ;; Act
          (agent-repl-status-sync-attention 'roster)))
      ;; Assert
      (should-not blinked))))

(ert-deftest agent-repl-test-status-an-arriving-marker-is-visible-before-its-first-step ()
  "The marker is recorded BEFORE the cadence is armed, so a push landing
between the arming and the 0 ms step cannot restart the blink."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl-status--marker-on (make-hash-table :test 'equal)))
      (agent-repl-test-status--syncing-attention t
        (cl-letf (((symbol-function 'agent-repl-status-blink-tab) #'ignore))
          ;; Act
          (agent-repl-status-sync-attention 'roster)))
      ;; Assert
      (should (agent-repl-status-attention-visible-p "alpha")))))

(ert-deftest agent-repl-test-status-a-departed-marker-is-cleared ()
  "A row that arrives WITHOUT the marker retracts it."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl-status--marker-on (make-hash-table :test 'equal)))
      (puthash "alpha" t agent-repl-status--marker-on)
      (agent-repl-test-status--syncing-attention nil
        (cl-letf (((symbol-function 'agent-repl-status-blink-tab)
                   (lambda (&rest _) (error "a departed marker must not blink"))))
          ;; Act
          (agent-repl-status-sync-attention 'roster)))
      ;; Assert
      (should-not (agent-repl-status-attention-visible-p "alpha")))))

;;;; ---- The paint -------------------------------------------------------

(ert-deftest agent-repl-test-status-display-state-is-the-arm-with-panels-open ()
  "An open workspace paints its arm across the whole entry."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "alpha" :project-dir "/w/1")
    (agent-repl-test-status--with-arm "alpha" :thinking
      (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) t)))
        ;; Act / Assert
        (should (eq (agent-repl--ws-display-state "alpha") :thinking))))))

(ert-deftest agent-repl-test-status-display-state-is-nil-with-panels-dismissed ()
  "Panels dismissed is a LOCAL modifier: the entry falls back to the
bracket-only paint, which is the blessed treatment."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "alpha" :project-dir "/w/1")
    (agent-repl-test-status--with-arm "alpha" :thinking
      (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) nil)))
        ;; Act / Assert
        (should (null (agent-repl--ws-display-state "alpha")))))))

(ert-deftest agent-repl-test-status-display-state-is-nil-once-ready-is-acknowledged ()
  "The ready shout fades after the dwell: the other local modifier."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "alpha" :project-dir "/w/1")
    (agent-repl--ws-put "alpha" :ready-view-acknowledged t)
    (agent-repl-test-status--with-arm "alpha" :ready
      (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) t)))
        ;; Act / Assert
        (should (null (agent-repl--ws-display-state "alpha")))))))

(ert-deftest agent-repl-test-status-bracket-state-ignores-panel-visibility ()
  "The bracket keeps the arm's colour even with the panels dismissed."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "alpha" :project-dir "/w/1")
    (agent-repl-test-status--with-arm "alpha" :thinking
      (cl-letf (((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) nil)))
        ;; Act / Assert
        (should (eq (agent-repl--ws-bracket-state "alpha") :thinking))))))

(ert-deftest agent-repl-test-status-the-badge-run-carries-the-priority-label ()
  "The roster's priority badge label draws before the name."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl-roster-row-for-ws)
               (lambda (_ws) '(:priority (:label "P1"))))
              ((symbol-function 'agent-repl-roster-row-priority-label)
               (lambda (row) (plist-get (plist-get row :priority) :label))))
      ;; Act / Assert
      (should (equal (agent-repl--tab-badge-str "alpha" :thinking) "P1")))))

(ert-deftest agent-repl-test-status-the-badge-run-carries-the-glyph-after-the-label ()
  "Priority first, then the arm's glyph."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl-roster-row-for-ws)
               (lambda (_ws) '(:priority (:label "P1"))))
              ((symbol-function 'agent-repl-roster-row-priority-label)
               (lambda (row) (plist-get (plist-get row :priority) :label))))
      ;; Act / Assert
      (should (equal (agent-repl--tab-badge-str "alpha" :merging)
                     (concat "P1 " (alist-get :merging agent-repl-status-merge-glyphs)))))))

(ert-deftest agent-repl-test-status-an-unprioritized-plain-row-draws-no-badge ()
  "The ordinary case is name and bracket alone."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl-roster-row-for-ws) (lambda (_ws) nil)))
      ;; Act / Assert
      (should (null (agent-repl--tab-badge-str "alpha" :thinking))))))

;;;; ---- The heartbeat ---------------------------------------------------

(ert-deftest agent-repl-test-status-the-dwell-tick-repaints ()
  "The whole heartbeat: latch the dwell and repaint.  Nothing is polled."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((repainted 0))
      (cl-letf (((symbol-function 'agent-repl--note-ready-view-dwell) #'ignore)
                ((symbol-function 'agent-repl--force-tab-bar-redraw)
                 (lambda () (cl-incf repainted))))
        ;; Act
        (agent-repl--status-dwell-tick)
        ;; Assert
        (should (equal repainted 1))))))

(ert-deftest agent-repl-test-status-frame-focus-repaints-and-polls-nothing ()
  "Regaining focus draws what already arrived on the roster stream."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((repainted 0))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) t))
                ((symbol-function 'agent-repl--force-tab-bar-redraw)
                 (lambda () (cl-incf repainted))))
        ;; Act
        (agent-repl--on-frame-focus)
        ;; Assert
        (should (equal repainted 1))))))

(ert-deftest agent-repl-test-status-losing-focus-repaints-nothing ()
  "An unfocused frame has nothing to redraw for."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((repainted 0))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--force-tab-bar-redraw)
                 (lambda () (cl-incf repainted))))
        ;; Act
        (agent-repl--on-frame-focus)
        ;; Assert
        (should (equal repainted 0))))))

(provide 'test-status)

;;; test-status.el ends here
