;;; test-window.el --- ERT tests for window.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the centralized window-management helpers in window.el.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-window.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Helpers ----

(defmacro agent-repl-window-test--with-temp-frame (&rest body)
  "Run BODY in an isolated single-frame setup with a fresh frame root.
Splits the selected frame's root window to a known starting state
\(one window) before BODY, then restores the prior configuration."
  (declare (indent 0))
  `(let ((wconf (current-window-configuration)))
     (unwind-protect
         (progn (delete-other-windows) ,@body)
       (set-window-configuration wconf))))

;;;; ---- Side-window predicate ----

(ert-deftest agent-repl-window-test-side-window-p-nil-for-main-window ()
  "`--side-window-p' returns nil for a normal (non-side) window."
  (agent-repl-window-test--with-temp-frame
    (should-not (agent-repl-window--side-window-p (selected-window)))))

(ert-deftest agent-repl-window-test-side-window-p-t-for-side-window ()
  "`--side-window-p' returns non-nil for a `display-buffer-in-side-window'
created window — the parameter `window-side' must be the discriminator."
  (agent-repl-window-test--with-temp-frame
    (let* ((buf (generate-new-buffer " *test-side*"))
           (win (display-buffer-in-side-window
                 buf '((side . left) (slot . 0)))))
      (unwind-protect
          (progn
            (should (window-live-p win))
            (should (agent-repl-window--side-window-p win)))
        (when (window-live-p win) (delete-window win))
        (kill-buffer buf)))))

(ert-deftest agent-repl-window-test-side-window-p-nil-for-dead-window ()
  "`--side-window-p' is nil for a dead window — guards against calls
against a stale window reference after teardown."
  (agent-repl-window-test--with-temp-frame
    (let* ((buf (generate-new-buffer " *test-side-dead*"))
           (win (display-buffer-in-side-window
                 buf '((side . left) (slot . 0)))))
      (delete-window win)
      (kill-buffer buf)
      (should-not (agent-repl-window--side-window-p win)))))

;;;; ---- Panel finders ----

(ert-deftest agent-repl-window-test-panel-buffer-view-returns-ws-buffer ()
  "`--panel-buffer :view WS' returns the buffer stored under `:frontend-buffer'
in WS's workspace plist."
  (agent-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *test-panel-view*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :frontend-buffer buf)
            (should (eq (agent-repl-window--panel-buffer :view "ws") buf)))
        (kill-buffer buf)))))

(ert-deftest agent-repl-window-test-panel-buffer-input-returns-ws-buffer ()
  "`--panel-buffer :input WS' returns the buffer stored under `:input-buffer'
in WS's workspace plist."
  (agent-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *test-panel-input*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :input-buffer buf)
            (should (eq (agent-repl-window--panel-buffer :input "ws") buf)))
        (kill-buffer buf)))))

(ert-deftest agent-repl-window-test-panel-buffer-unknown-kind-errors ()
  "`--panel-buffer' on an unknown KIND signals an error — typos surface
at call time."
  (should-error (agent-repl-window--panel-buffer :bogus "ws")
                :type 'error))

(ert-deftest agent-repl-window-test-panel-window-returns-displaying-window ()
  "`--panel-window' returns the live window displaying the panel buffer."
  (agent-repl-test--with-clean-state
    (let* ((buf (generate-new-buffer " *test-panel-win-view*"))
           (wconf (current-window-configuration)))
      (unwind-protect
          (progn
            (delete-other-windows)
            (agent-repl--ws-put "ws" :frontend-buffer buf)
            (let ((win (display-buffer buf)))
              (should (eq (agent-repl-window--panel-window :view "ws") win))))
        (set-window-configuration wconf)
        (kill-buffer buf)))))

(ert-deftest agent-repl-window-test-panel-window-nil-when-buffer-not-shown ()
  "`--panel-window' returns nil when the panel buffer exists but is not
displayed in any window."
  (agent-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *test-panel-win-hidden*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :frontend-buffer buf)
            (should-not (agent-repl-window--panel-window :view "ws")))
        (kill-buffer buf)))))

(ert-deftest agent-repl-window-test-panel-window-nil-when-buffer-killed ()
  "`--panel-window' returns nil when the workspace plist holds a stale
buffer reference (killed buffer) — defensive for races against
teardown that nils plist entries after killing buffers."
  (agent-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *test-panel-win-killed*")))
      (agent-repl--ws-put "ws" :frontend-buffer buf)
      (kill-buffer buf)
      (should-not (agent-repl-window--panel-window :view "ws")))))

;;;; ---- Hardening ----

(ert-deftest agent-repl-window-test-harden-dedicate-sets-dedicated ()
  "`--harden :dedicate t' marks the window as dedicated."
  (agent-repl-window-test--with-temp-frame
    (let ((win (selected-window)))
      (set-window-dedicated-p win nil)
      (agent-repl-window--harden win :dedicate t)
      (should (window-dedicated-p win)))))

(ert-deftest agent-repl-window-test-harden-no-keys-is-noop ()
  "`--harden' with no recipe keys mutates nothing — invariants from
prior calls survive a recipe-free invocation."
  (agent-repl-window-test--with-temp-frame
    (let ((win (selected-window)))
      (set-window-dedicated-p win nil)
      (set-window-parameter win 'window-size-fixed nil)
      (set-window-parameter win 'no-delete-other-windows nil)
      (agent-repl-window--harden win)
      (should-not (window-dedicated-p win))
      (should-not (window-parameter win 'window-size-fixed))
      (should-not (window-parameter win 'no-delete-other-windows)))))

(ert-deftest agent-repl-window-test-harden-size-fix-sets-parameter ()
  "`--harden :size-fix VALUE' writes `window-size-fixed' to VALUE.
This is the window-parameter form (not the buffer-local variable) so
the lock is per-window even when the same buffer is displayed
elsewhere."
  (agent-repl-window-test--with-temp-frame
    (let ((win (selected-window)))
      (set-window-parameter win 'window-size-fixed nil)
      (agent-repl-window--harden win :size-fix 'width)
      (should (eq (window-parameter win 'window-size-fixed) 'width))
      (agent-repl-window--harden win :size-fix 'height)
      (should (eq (window-parameter win 'window-size-fixed) 'height)))))

(ert-deftest agent-repl-window-test-harden-delete-protect-sets-parameter ()
  "`--harden :delete-protect t' sets `no-delete-other-windows'."
  (agent-repl-window-test--with-temp-frame
    (let ((win (selected-window)))
      (set-window-parameter win 'no-delete-other-windows nil)
      (agent-repl-window--harden win :delete-protect t)
      (should (window-parameter win 'no-delete-other-windows)))))

(ert-deftest agent-repl-window-test-harden-no-other-window-sets-parameter ()
  "`--harden :no-other-window t' sets the `no-other-window' parameter
so keyboard `other-window' navigation skips this window."
  (agent-repl-window-test--with-temp-frame
    (let ((win (selected-window)))
      (set-window-parameter win 'no-other-window nil)
      (agent-repl-window--harden win :no-other-window t)
      (should (window-parameter win 'no-other-window)))))

(ert-deftest agent-repl-window-test-harden-fringes-integer-sets-both ()
  "`--harden :fringes N' sets both left and right fringes to N px."
  (agent-repl-window-test--with-temp-frame
    (let ((win (selected-window)))
      (agent-repl-window--harden win :fringes 0)
      (let ((f (window-fringes win)))
        ;; `window-fringes' returns (LEFT-WIDTH RIGHT-WIDTH OUTSIDE-MARGINS).
        (should (= (nth 0 f) 0))
        (should (= (nth 1 f) 0))))))

(ert-deftest agent-repl-window-test-harden-fringes-cons-passes-l-and-r ()
  "`--harden :fringes (L . R)' calls `set-window-fringes' with L and R as
the left/right widths.  Asserted via call interception because batch
frames may round/quantize the actual stored values."
  (agent-repl-window-test--with-temp-frame
    (let ((win (selected-window))
          (recorded nil))
      (cl-letf (((symbol-function 'set-window-fringes)
                 (lambda (w l r outside) (setq recorded (list w l r outside)))))
        (agent-repl-window--harden win :fringes (cons 3 7)))
      (should (equal recorded (list win 3 7 nil))))))

(ert-deftest agent-repl-window-test-harden-dead-window-no-error ()
  "`--harden' on a dead window is a silent no-op — defensive for callers
who race against a teardown."
  (agent-repl-window-test--with-temp-frame
    (let* ((main (selected-window))
           (extra (split-window main)))
      (delete-window extra)
      ;; Each branch should be guarded; no error.
      (agent-repl-window--harden
       extra :dedicate t :size-fix 'width :delete-protect t
       :no-other-window t :preserve-size 'height :fringes 0)
      (should-not (window-live-p extra)))))

;;;; ---- Subset deletion ----

(ert-deftest agent-repl-window-test-delete-where-deletes-matching ()
  "`--delete-where' deletes windows whose PREDICATE returns non-nil."
  (agent-repl-window-test--with-temp-frame
    (let* ((main (selected-window))
           (extra (split-window main)))
      ;; Predicate: every window EXCEPT the originally selected one.
      (agent-repl-window--delete-where
       (lambda (w) (not (eq w main))))
      (should (window-live-p main))
      (should-not (window-live-p extra)))))

(ert-deftest agent-repl-window-test-delete-where-skips-side-windows-by-default ()
  "`--delete-where' preserves side windows by default — this is the
side-window-aware default that stops layout-clearing commands from
trampling frame-level side-window UI elements.

Uses explicit `split-window' (not `display-buffer-pop-up-window'),
which is unreliable under `emacs -batch -Q' where window-sizing
heuristics may refuse to split."
  (agent-repl-window-test--with-temp-frame
    (let* ((main  (selected-window))
           (extra (split-window main))
           (side-buf (generate-new-buffer " *test-side*"))
           (side (display-buffer-in-side-window
                  side-buf '((side . left) (slot . 0)))))
      (unwind-protect
          (progn
            (should (window-live-p extra))
            (should (window-live-p side))
            ;; Use a deterministic predicate that targets both the
            ;; deletable main (`extra') AND the side window — the side
            ;; default must still preserve `side' even though the
            ;; predicate would otherwise match it.  An all-matching
            ;; predicate is iteration-order-dependent (one of the two
            ;; main windows would be undeletable as the last main) so
            ;; we target specifically.
            (agent-repl-window--delete-where
             (lambda (w) (or (eq w extra) (eq w side))))
            (should-not (window-live-p extra))
            (should (window-live-p side)))
        (when (window-live-p side) (delete-window side))
        (kill-buffer side-buf)))))

(ert-deftest agent-repl-window-test-delete-where-can-include-side-windows ()
  "`--delete-where' with `:skip-side-windows nil' DOES delete side
windows when the caller explicitly opts in (e.g. a fullscreen toggle
that genuinely wants a single-panel frame)."
  (agent-repl-window-test--with-temp-frame
    (let* ((side-buf (generate-new-buffer " *test-side-opt-in*"))
           (side (display-buffer-in-side-window
                  side-buf '((side . right) (slot . 0)))))
      (unwind-protect
          (progn
            (should (window-live-p side))
            (agent-repl-window--delete-where
             (lambda (w) (eq w side))
             :skip-side-windows nil)
            (should-not (window-live-p side)))
        (when (window-live-p side) (delete-window side))
        (kill-buffer side-buf)))))

(ert-deftest agent-repl-window-test-delete-where-never-deletes-minibuffer ()
  "`--delete-where' never deletes a minibuffer window, even when PREDICATE
matches it.  The minibuffer appears in `window-list' while a minibuffer is
active (as during the `SPC p p' picker) and `delete-window' always refuses
it — so the sweep must skip it categorically rather than attempt and warn."
  (agent-repl-window-test--with-temp-frame
    (let* ((main (selected-window))
           (extra (split-window main)))
      ;; Treat EXTRA as the minibuffer window; PREDICATE matches ONLY it.
      (cl-letf (((symbol-function 'window-minibuffer-p)
                 (lambda (&optional w) (eq w extra))))
        (let ((deleted (agent-repl-window--delete-where (lambda (w) (eq w extra)))))
          (should (window-live-p extra))   ; excluded, not deleted
          (should-not deleted))))))         ; nothing swept

(ert-deftest agent-repl-window-test-delete-where-returns-deleted-list ()
  "`--delete-where' returns the list of windows it actually deleted —
caller can verify the sweep was non-empty."
  (agent-repl-window-test--with-temp-frame
    (let* ((main (selected-window))
           (extra (split-window main))
           (deleted
            (agent-repl-window--delete-where
             (lambda (w) (eq w extra)))))
      (should (equal deleted (list extra)))
      (should-not (window-live-p extra)))))

(ert-deftest agent-repl-window-test-delete-where-empty-predicate-no-op ()
  "`--delete-where' with a never-matching predicate deletes nothing
and returns an empty list."
  (agent-repl-window-test--with-temp-frame
    (let* ((main (selected-window))
           (extra (split-window main))
           (deleted
            (agent-repl-window--delete-where (lambda (_w) nil))))
      (should (null deleted))
      (should (window-live-p main))
      (should (window-live-p extra)))))

;;;; ---- Delete by buffer ----

(ert-deftest agent-repl-window-test-delete-buffer-windows-deletes-window ()
  "`--delete-buffer-windows' deletes the window showing BUF."
  (agent-repl-window-test--with-temp-frame
    (let* ((buf  (generate-new-buffer " *test-target*"))
           (main (selected-window))
           (extra (split-window main)))
      (set-window-buffer extra buf)
      (unwind-protect
          (progn
            (agent-repl-window--delete-buffer-windows buf)
            (should-not (window-live-p extra))
            (should (window-live-p main)))
        (kill-buffer buf)))))

(ert-deftest agent-repl-window-test-delete-buffer-windows-deletes-side-window ()
  "`--delete-buffer-windows' deletes a side window targeting BUF —
targeting a specific buffer bypasses the side-window skip that
`--delete-where' applies by default."
  (agent-repl-window-test--with-temp-frame
    (let* ((buf  (generate-new-buffer " *test-side-target*"))
           (side (display-buffer-in-side-window
                  buf '((side . left) (slot . 0)))))
      (unwind-protect
          (progn
            (should (window-live-p side))
            (should (agent-repl-window--side-window-p side))
            (agent-repl-window--delete-buffer-windows buf)
            (should-not (window-live-p side)))
        (when (window-live-p side) (delete-window side))
        (kill-buffer buf)))))

(ert-deftest agent-repl-window-test-delete-buffer-windows-nil-buf-noop ()
  "`--delete-buffer-windows' with a nil buffer is a no-op."
  (agent-repl-window-test--with-temp-frame
    (let* ((main (selected-window))
           (extra (split-window main))
           (deleted (agent-repl-window--delete-buffer-windows nil)))
      (should (null deleted))
      (should (window-live-p main))
      (should (window-live-p extra)))))

(ert-deftest agent-repl-window-test-delete-buffer-windows-killed-buf-noop ()
  "`--delete-buffer-windows' on a killed buffer is a no-op (returns
nil) — defensive for callers passing stale buffer references."
  (agent-repl-window-test--with-temp-frame
    (let* ((buf (generate-new-buffer " *test-killed*"))
           (main (selected-window))
           (extra (split-window main)))
      (kill-buffer buf)
      (let ((deleted (agent-repl-window--delete-buffer-windows buf)))
        (should (null deleted))
        (should (window-live-p main))
        (should (window-live-p extra))))))

(ert-deftest agent-repl-window-test-delete-buffer-windows-returns-deleted ()
  "`--delete-buffer-windows' returns the list of windows it deleted."
  (agent-repl-window-test--with-temp-frame
    (let* ((buf  (generate-new-buffer " *test-return*"))
           (main (selected-window))
           (extra (split-window main)))
      (set-window-buffer extra buf)
      (unwind-protect
          (let ((deleted (agent-repl-window--delete-buffer-windows buf)))
            (should (equal deleted (list extra))))
        (kill-buffer buf)))))

;;;; ---- Benign-undeletable-error predicate ----

(ert-deftest agent-repl-window-test-benign-undeletable-main-window ()
  "Recognizes the main-window-of-frame error as benign."
  (should (agent-repl-window--benign-undeletable-error-p
           '(error "Attempt to delete main window of frame #<frame foo>"))))

(ert-deftest agent-repl-window-test-benign-undeletable-sole-side ()
  "Recognizes the sole-side-window error as benign."
  (should (agent-repl-window--benign-undeletable-error-p
           '(error "Attempt to delete sole side window of frame #<frame foo>"))))

(ert-deftest agent-repl-window-test-benign-undeletable-sole-ordinary ()
  "Recognizes the sole-ordinary-window error as benign."
  (should (agent-repl-window--benign-undeletable-error-p
           '(error "Attempt to delete sole ordinary window of frame #<frame foo>"))))

(ert-deftest agent-repl-window-test-benign-undeletable-unrelated-error ()
  "Unrelated errors are NOT classified as benign — they still surface."
  (should-not (agent-repl-window--benign-undeletable-error-p
               '(error "Some other failure"))))

(ert-deftest agent-repl-window-test-benign-undeletable-non-error-form ()
  "Non-error structures don't crash the predicate."
  (should-not (agent-repl-window--benign-undeletable-error-p nil))
  (should-not (agent-repl-window--benign-undeletable-error-p '(user-error "Nope"))))

;;;; ---- Restorable predicate (--panels-restorable-p) ----

(ert-deftest agent-repl-window-test-panels-restorable-p-live-view ()
  "A live view buffer makes the workspace's panels restorable."
  (agent-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *restorable-view*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :frontend-buffer buf)
            (should (agent-repl-window--panels-restorable-p "ws")))
        (kill-buffer buf)))))

(ert-deftest agent-repl-window-test-panels-restorable-p-nil-when-view-dead ()
  "A dead view buffer makes the workspace's panels non-restorable."
  (agent-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *restorable-view-dead*")))
      (agent-repl--ws-put "ws" :frontend-buffer buf)
      (kill-buffer buf)
      (should-not (agent-repl-window--panels-restorable-p "ws")))))

(ert-deftest agent-repl-window-test-panels-restorable-p-nil-when-view-missing ()
  "A workspace with no view buffer recorded is non-restorable."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl-window--panels-restorable-p "ws"))))

(ert-deftest agent-repl-window-test-panels-restorable-p-ignores-dead-input ()
  "A dead input buffer does not gate restorability — the mount path
recreates it (`agent-repl--ensure-input-buffer')."
  (agent-repl-test--with-clean-state
    (let ((view-buf (generate-new-buffer " *restorable-view-2*"))
          (input-buf (generate-new-buffer " *restorable-input*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :frontend-buffer view-buf)
            (agent-repl--ws-put "ws" :input-buffer input-buf)
            (kill-buffer input-buf)
            (should (agent-repl-window--panels-restorable-p "ws")))
        (when (buffer-live-p view-buf) (kill-buffer view-buf))))))

;;;; ---- Layout reconciliation (--ensure-layout) ----

(cl-defun agent-repl-window-test--run-ensure-layout
    (&key show-view show-input (current-ws "cur") kill-view eager
          in-progress setup after)
  "Drive `agent-repl-window--ensure-layout' over a real window layout.
Registers a view and an input buffer for workspace \"cur\", displays
the view buffer when SHOW-VIEW and the input buffer when SHOW-INPUT
\(in a split below when both), kills the view buffer when KILL-VIEW,
binds the eager-open / repair-in-progress guards to EAGER /
IN-PROGRESS, reports CURRENT-WS as the active workspace, and stubs the
frontend show dispatch.  SETUP, when non-nil, is a thunk run after the
layout is built (for fixtures like a side window or another
workspace's panel); AFTER, when non-nil, is a thunk run after the
reconcile while the layout is still live (for window-liveness
assertions the temp-frame restore would otherwise destroy).  Returns
the list of workspaces a repair was dispatched for."
  (let ((view-buf (generate-new-buffer " *elt-view*"))
        (input-buf (generate-new-buffer " *elt-input*"))
        (dispatched '()))
    (unwind-protect
        (agent-repl-test--with-clean-state
          (agent-repl-window-test--with-temp-frame
            (agent-repl--ws-put "cur" :frontend-buffer view-buf)
            (agent-repl--ws-put "cur" :input-buffer input-buf)
            (when show-view
              (set-window-buffer (selected-window) view-buf))
            (when show-input
              (set-window-buffer (if show-view
                                     (split-window (selected-window))
                                   (selected-window))
                                 input-buf))
            (when kill-view (kill-buffer view-buf))
            (when setup (funcall setup))
            (cl-letf (((symbol-function '+workspace-current-name)
                       (lambda () current-ws))
                      ((symbol-function 'agent-repl--frontend-dispatch-show)
                       (lambda (ws) (push ws dispatched))))
              (let ((agent-repl--eager-open-in-progress eager)
                    (agent-repl-window--ensure-layout-in-progress in-progress))
                (agent-repl-window--ensure-layout)))
            (when after (funcall after))
            (nreverse dispatched)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p input-buf) (kill-buffer input-buf)))))

(ert-deftest agent-repl-window-test-ensure-layout-restores-input-when-view-only ()
  "View window present without its input partner → a repair is
dispatched through the workspace's frontend (the input panel comes back)."
  (should (equal (agent-repl-window-test--run-ensure-layout :show-view t)
                 '("cur"))))

(ert-deftest agent-repl-window-test-ensure-layout-restores-view-when-input-only ()
  "Input window present without its view partner (view buffer live) → a
repair is dispatched through the workspace's frontend (the view comes back)."
  (should (equal (agent-repl-window-test--run-ensure-layout :show-input t)
                 '("cur"))))

(ert-deftest agent-repl-window-test-ensure-layout-noop-when-both-present ()
  "Both panel windows present → the layout already conforms and no
repair is dispatched."
  (should-not (agent-repl-window-test--run-ensure-layout
               :show-view t :show-input t)))

(ert-deftest agent-repl-window-test-ensure-layout-noop-when-neither-present ()
  "Neither panel window present (hidden/closed workspace) → no repair;
a deliberately hidden workspace is never resurrected."
  (should-not (agent-repl-window-test--run-ensure-layout)))

(ert-deftest agent-repl-window-test-ensure-layout-leaves-side-window-untouched ()
  "A side window beside a view-only drift is not disturbed: the
repair dispatches and the side window survives the reconcile."
  (let ((side-buf (generate-new-buffer " *elt-side*"))
        (side-win nil))
    (unwind-protect
        (should (equal
                 (agent-repl-window-test--run-ensure-layout
                  :show-view t
                  :setup (lambda ()
                           (setq side-win
                                 (display-buffer-in-side-window
                                  side-buf '((side . left) (slot . 0)))))
                  :after (lambda ()
                           (should (window-live-p side-win))))
                 '("cur")))
      (kill-buffer side-buf))))

(ert-deftest agent-repl-window-test-ensure-layout-ignores-other-workspace-panels ()
  "Another workspace's half-present panel never triggers a repair —
reconciliation is scoped to the current workspace's own pair."
  (let ((other-buf (generate-new-buffer " *elt-other-input*")))
    (unwind-protect
        (should-not
         (agent-repl-window-test--run-ensure-layout
          :setup (lambda ()
                   (agent-repl--ws-put "other" :input-buffer other-buf)
                   (set-window-buffer (selected-window) other-buf))))
      (kill-buffer other-buf))))

(ert-deftest agent-repl-window-test-ensure-layout-noop-without-workspace ()
  "No current workspace → no repair even when a drift is on the frame."
  (should-not (agent-repl-window-test--run-ensure-layout
               :show-view t :current-ws nil)))

(ert-deftest agent-repl-window-test-ensure-layout-noop-during-eager-open ()
  "An eager-open build in flight suppresses the reconcile — the mount
lays the panels out itself and a repair would fight it."
  (should-not (agent-repl-window-test--run-ensure-layout
               :show-view t :eager t)))

(ert-deftest agent-repl-window-test-ensure-layout-noop-when-repair-in-flight ()
  "A repair already dispatching suppresses a nested reconcile pass."
  (should-not (agent-repl-window-test--run-ensure-layout
               :show-view t :in-progress t)))

(ert-deftest agent-repl-window-test-ensure-layout-skips-dead-view-buffer ()
  "Input window present but the view buffer dead → no repair; remounting
a view would mean creating a webview/session from a window-change hook."
  (should-not (agent-repl-window-test--run-ensure-layout
               :show-input t :kill-view t)))

;;;; ---- delete-where: benign-error suppression ----

(ert-deftest agent-repl-window-test-delete-where-skips-main-window-quietly ()
  "Regression for `SPC w f' in agent-repl: when iteration reaches a
window that has collapsed to the sole main-area window, `delete-window'
errors with 'Attempt to delete main window of frame', and the old
code dumped that into *Messages* via `message'.  After the fix, the
sweep swallows the structural error and produces no `[agent-repl]
window--delete-where: could not delete' message."
  (agent-repl-window-test--with-temp-frame
    (let* ((side-buf (generate-new-buffer " *test-side*"))
           (main-buf (generate-new-buffer " *test-main*"))
           (side-win (display-buffer-in-side-window
                      side-buf '((side . left) (slot . 0)))))
      (set-window-buffer (selected-window) main-buf)
      (unwind-protect
          (let* ((captured nil)
                 (orig-message (symbol-function 'message)))
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (let ((s (apply #'format fmt args)))
                           (when (string-match-p "could not delete" s)
                             (push s captured))
                           (apply orig-message fmt args)))))
              ;; Sweep targets `main-buf' — it's the only main-area window
              ;; with the side window present, so `delete-window' refuses
              ;; it as the frame's main window.  We want the sweep to
              ;; recognize that and stay silent.
              (agent-repl-window--delete-where
               (lambda (win) (eq (window-buffer win) main-buf)))
              (should-not captured)))
        (when (window-live-p side-win) (delete-window side-win))
        (kill-buffer side-buf)
        (kill-buffer main-buf)))))

(ert-deftest agent-repl-window-test-delete-where-still-logs-unrelated-errors ()
  "Sanity: non-structural errors from `delete-window' still get logged.
Stubs `delete-window' to throw an unrelated error so we exercise the
non-benign branch without needing a window state that produces one
naturally."
  (agent-repl-window-test--with-temp-frame
    (let* ((other-buf (generate-new-buffer " *test-other*"))
           (other-win (split-window (selected-window))))
      (set-window-buffer other-win other-buf)
      (unwind-protect
          (let ((captured nil)
                (orig-message (symbol-function 'message)))
            (cl-letf (((symbol-function 'delete-window)
                       (lambda (&rest _) (error "Synthetic unrelated failure")))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (let ((s (apply #'format fmt args)))
                           (when (string-match-p "could not delete" s)
                             (push s captured))
                           (apply orig-message fmt args)))))
              (agent-repl-window--delete-where
               (lambda (win) (eq win other-win)))
              (should captured)))
        (when (window-live-p other-win)
          (ignore-errors (delete-window other-win)))
        (kill-buffer other-buf)))))

;;;; ---- Panel-finder log routing ----

(ert-deftest agent-repl-test-window-panel-window-placeholder-logs-globally ()
  "Asking about a persp PLACEHOLDER's panel logs against the global sink.
The workspace-switch path queries this finder for whatever perspective
persp-mode activated, and \"main\"/\"none\" own no durable log sink."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((logged 'no-record))
      (cl-letf (((symbol-function 'agent-repl--log-verbose)
                 (lambda (ws &rest _)
                   (when (eq logged 'no-record) (setq logged ws)))))
        ;; Act
        (agent-repl-window--panel-window :input "main"))
      ;; Assert
      (should (null logged)))))

(ert-deftest agent-repl-test-window-panel-window-routable-ws-keeps-attribution ()
  "Asking about a REAL workspace's panel keeps the record attributed to it."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((project (make-temp-file "agent-repl-panel-route-" t))
          (logged 'no-record))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir project)
            (cl-letf (((symbol-function 'agent-repl--log-verbose)
                       (lambda (ws &rest _)
                         (when (eq logged 'no-record) (setq logged ws)))))
              ;; Act
              (agent-repl-window--panel-window :input "ws1")))
        (delete-directory project t))
      ;; Assert
      (should (equal logged "ws1")))))

;;; test-window.el ends here

;;;; ---- Deletion, or its structural substitute ----

(ert-deftest agent-repl-window-test-delete-or-neutralize-deletes-a-deletable-window ()
  "A window that can be deleted is deleted, and reports `deleted'."
  (agent-repl-window-test--with-temp-frame
    (let* ((buf (generate-new-buffer " *test-neutralize-a*"))
           (main (selected-window))
           (extra (split-window main)))
      (set-window-buffer extra buf)
      (unwind-protect
          (progn
            ;; Act
            (should (eq (agent-repl-window--delete-or-neutralize extra) 'deleted))
            ;; Assert
            (should-not (window-live-p extra))
            (should (window-live-p main)))
        (kill-buffer buf)))))

(ert-deftest agent-repl-window-test-delete-or-neutralize-switches-the-sole-window ()
  "The frame's sole window is switched to the fallback, never deleted."
  (agent-repl-window-test--with-temp-frame
    (let ((buf (generate-new-buffer " *test-neutralize-sole*"))
          (fallback (generate-new-buffer " *test-neutralize-fallback*"))
          (sole (selected-window)))
      (unwind-protect
          (progn
            (set-window-buffer sole buf)
            ;; Act
            (should (eq (agent-repl-window--delete-or-neutralize sole fallback)
                        'neutralized))
            ;; Assert — the window survives and no longer shows the retired buffer.
            (should (window-live-p sole))
            (should (eq (window-buffer sole) fallback)))
        (kill-buffer buf)
        (kill-buffer fallback)))))

(ert-deftest agent-repl-window-test-delete-or-neutralize-undedicates-a-strong-dedication ()
  "A STRONGLY dedicated sole window is un-dedicated so the switch can land."
  (agent-repl-window-test--with-temp-frame
    (let ((buf (generate-new-buffer " *test-neutralize-dedicated*"))
          (fallback (generate-new-buffer " *test-neutralize-fallback2*"))
          (sole (selected-window)))
      (unwind-protect
          (progn
            (set-window-buffer sole buf)
            (set-window-dedicated-p sole t)
            ;; Act
            (agent-repl-window--delete-or-neutralize sole fallback)
            ;; Assert
            (should-not (window-dedicated-p sole))
            (should (eq (window-buffer sole) fallback)))
        (kill-buffer buf)
        (kill-buffer fallback)))))

(ert-deftest agent-repl-window-test-delete-or-neutralize-signals-without-a-fallback ()
  "An undeletable window with no fallback buffer fails loudly, never silently."
  (agent-repl-window-test--with-temp-frame
    (let ((buf (generate-new-buffer " *test-neutralize-nofallback*"))
          (sole (selected-window)))
      (unwind-protect
          (cl-letf (((symbol-function 'doom-fallback-buffer) (lambda () nil)))
            (set-window-buffer sole buf)
            ;; Act / Assert
            (should-error (agent-repl-window--delete-or-neutralize sole)
                          :type 'error))
        (kill-buffer buf)))))

(ert-deftest agent-repl-window-test-delete-buffer-windows-switches-the-sole-window ()
  "Closing a panel that owns the frame's only window switches it, never errors."
  (agent-repl-window-test--with-temp-frame
    (let ((buf (generate-new-buffer " *test-sole-panel*"))
          (fallback (generate-new-buffer " *test-sole-fallback*"))
          (sole (selected-window))
          (warned nil))
      (unwind-protect
          (cl-letf (((symbol-function 'doom-fallback-buffer) (lambda () fallback))
                    ((symbol-function 'agent-repl--warn)
                     (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warned))))
            (set-window-buffer sole buf)
            ;; Act
            (agent-repl-window--delete-buffer-windows buf)
            ;; Assert — no "Attempt to delete ... sole ordinary window" warning,
            ;; and the retired buffer is off screen all the same.
            (should (window-live-p sole))
            (should-not (eq (window-buffer sole) buf))
            (should-not warned))
        (kill-buffer buf)
        (kill-buffer fallback)))))

(ert-deftest agent-repl-window-test-delete-buffer-windows-omits-a-neutralized-window ()
  "The returned list names windows actually DELETED, not ones left standing."
  (agent-repl-window-test--with-temp-frame
    (let ((buf (generate-new-buffer " *test-sole-return*"))
          (fallback (generate-new-buffer " *test-sole-return-fallback*"))
          (sole (selected-window)))
      (unwind-protect
          (cl-letf (((symbol-function 'doom-fallback-buffer) (lambda () fallback)))
            (set-window-buffer sole buf)
            ;; Act / Assert
            (should-not (agent-repl-window--delete-buffer-windows buf)))
        (kill-buffer buf)
        (kill-buffer fallback)))))
