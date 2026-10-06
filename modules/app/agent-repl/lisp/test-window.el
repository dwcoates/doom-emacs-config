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

(ert-deftest agent-repl-window-test-panels-restorable-p-screens-placeholder-log ()
  "restorable-p emits no unroutable record for a persp placeholder ws.
The panels-open default now consults this for whatever perspective a
switch activated, including persp-mode built-ins that own no sink."
  (agent-repl-test--with-clean-state
    (let ((raw nil))
      (cl-letf (((symbol-function (quote agent-repl--log-verbose))
                 (lambda (ws &rest _)
                   (when (and (stringp ws)
                              (not (agent-repl--ws-log-routable-p ws))
                              (not (agent-repl--central-log-scope-reason ws))
                              (not (agent-repl--context-log-scope-reason ws)))
                     (push ws raw)))))
        (agent-repl-window--panels-restorable-p "main")
        (should-not raw)))))

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
          in-progress setup after gate)
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
assertions the temp-frame restore would otherwise destroy).  GATE, when
non-nil, is the gate the host view states for \"cur\".  Returns
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
                      ((symbol-function 'agent-repl-host-state)
                       (lambda (ws) (and gate (equal ws "cur") (list :gate gate))))
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

;;;; ---- The composer's fixed height ----
;;
;; The defect these cover: the composer used to be split out at a
;; fraction of whatever window the mount happened to be splitting, so
;; its height moved with the frame's state at that instant — different
;; per workspace, and different for one workspace across a remount.

(defmacro agent-repl-window-test--with-fresh-height-cache (&rest body)
  "Run BODY with the frame's cached composer height cleared and restored."
  (declare (indent 0))
  `(let ((saved (frame-parameter nil agent-repl-window--input-height-parameter)))
     (unwind-protect
         (progn
           (set-frame-parameter nil agent-repl-window--input-height-parameter nil)
           ,@body)
       (set-frame-parameter nil agent-repl-window--input-height-parameter saved))))

(defmacro agent-repl-window-test--capturing-debug (&rest body)
  "Run BODY capturing the debug rung into `agent-repl-window-test--debug'."
  (declare (indent 0))
  `(let ((agent-repl-window-test--debug nil))
     (cl-letf (((symbol-function 'agent-repl--log)
                (lambda (_ws fmt &rest args)
                  (push (apply #'format fmt args) agent-repl-window-test--debug))))
       ,@body)))

(defvar agent-repl-window-test--debug nil
  "Debug records the stubbed rung received during a capture.")

(ert-deftest agent-repl-window-test-input-height-is-the-declared-fraction-of-the-main-area ()
  "The composer height is the declared fraction of the main area plus the offset."
  (agent-repl-window-test--with-temp-frame
    (agent-repl-window-test--with-fresh-height-cache
      ;; Act
      (let ((lines (agent-repl-window--input-height)))
        ;; Assert
        (should (= lines
                   (+ (round (* agent-repl-input-height-fraction
                                (window-total-height (frame-root-window))))
                      agent-repl-input-height-line-offset)))))))

(ert-deftest agent-repl-window-test-input-height-default-is-one-line-under-the-fraction ()
  "By default the composer is exactly one line shorter than the fraction alone."
  (agent-repl-window-test--with-temp-frame
    (agent-repl-window-test--with-fresh-height-cache
      ;; Arrange
      (let ((unadjusted (let ((agent-repl-input-height-line-offset 0))
                          (agent-repl-window--input-height))))
        (set-frame-parameter nil agent-repl-window--input-height-parameter nil)
        ;; Act
        (let ((lines (agent-repl-window--input-height)))
          ;; Assert
          (should (= lines (1- unadjusted))))))))

(ert-deftest agent-repl-window-test-input-height-ignores-the-window-being-split ()
  "A smaller host window does not shrink the composer's height.
The old mount measured the window it was splitting, which is why a
workspace mounted while the frame carried something else came out short."
  (agent-repl-window-test--with-temp-frame
    (agent-repl-window-test--with-fresh-height-cache
      ;; Arrange — the full-frame answer, then a frame split in half.
      (let ((full (agent-repl-window--input-height)))
        (set-frame-parameter nil agent-repl-window--input-height-parameter nil)
        (split-window)
        ;; Act
        (let ((halved (agent-repl-window--input-height)))
          ;; Assert
          (should (= halved full)))))))

(ert-deftest agent-repl-window-test-input-height-reuses-the-frame-cache ()
  "A second ask returns the cached line count, not a fresh derivation."
  (agent-repl-window-test--with-temp-frame
    (agent-repl-window-test--with-fresh-height-cache
      ;; Arrange
      (let ((first (agent-repl-window--input-height)))
        ;; Act — a changed fraction must not move an already-derived frame.
        (let* ((agent-repl-input-height-fraction
                (* 2 agent-repl-input-height-fraction))
               (second (agent-repl-window--input-height)))
          ;; Assert
          (should (= second first)))))))

(ert-deftest agent-repl-window-test-input-height-rederives-on-a-frame-geometry-change ()
  "A frame whose text height changed re-derives instead of reusing the cache."
  (agent-repl-window-test--with-temp-frame
    (agent-repl-window-test--with-fresh-height-cache
      ;; Arrange — a cache recorded against a geometry this frame no longer has.
      (set-frame-parameter nil agent-repl-window--input-height-parameter
                           (cons (+ 1000 (frame-height)) 99))
      ;; Act
      (let ((lines (agent-repl-window--input-height)))
        ;; Assert
        (should (= lines
                   (+ (round (* agent-repl-input-height-fraction
                                (window-total-height (frame-root-window))))
                      agent-repl-input-height-line-offset)))))))

(ert-deftest agent-repl-window-test-input-height-floors-at-the-minimum ()
  "A fraction too small to leave a usable composer is floored, not honored."
  (agent-repl-window-test--with-temp-frame
    (agent-repl-window-test--with-fresh-height-cache
      ;; Arrange / Act
      (let* ((agent-repl-input-height-fraction 0.001)
             (lines (agent-repl-window--input-height)))
        ;; Assert
        (should (= lines agent-repl-window--input-height-minimum))))))

(ert-deftest agent-repl-window-test-input-height-records-the-derivation ()
  "The derivation is on the record: an unexplained height is a logging defect."
  (agent-repl-window-test--with-temp-frame
    (agent-repl-window-test--with-fresh-height-cache
      ;; Act
      (agent-repl-window-test--capturing-debug
        (agent-repl-window--input-height)
        ;; Assert
        (should (cl-find-if
                 (lambda (m)
                   (string-prefix-p "window--input-height: source=computed" m))
                 agent-repl-window-test--debug))))))

(ert-deftest agent-repl-window-test-input-height-records-a-cache-hit ()
  "A reused height says so, so a run can tell derivation from reuse."
  (agent-repl-window-test--with-temp-frame
    (agent-repl-window-test--with-fresh-height-cache
      ;; Arrange
      (agent-repl-window--input-height)
      ;; Act
      (agent-repl-window-test--capturing-debug
        (agent-repl-window--input-height)
        ;; Assert
        (should (cl-find-if
                 (lambda (m)
                   (string-prefix-p "window--input-height: source=frame-cache" m))
                 agent-repl-window-test--debug))))))

(ert-deftest agent-repl-window-test-apply-height-resizes-to-the-target ()
  "A window granted fewer lines than asked for is resized to the target."
  (agent-repl-window-test--with-temp-frame
    ;; Arrange
    (let ((win (split-window nil -4 'below)))
      ;; Act
      (agent-repl-window--apply-height win 6 nil)
      ;; Assert
      (should (= (window-total-height win) 6)))))

(ert-deftest agent-repl-window-test-apply-height-returns-the-real-height ()
  "The return value is the height the window HAS, never the one requested."
  (agent-repl-window-test--with-temp-frame
    ;; Arrange — a target the frame cannot possibly grant.
    (let ((win (split-window nil -4 'below)))
      ;; Act
      (let ((actual (agent-repl-window--apply-height win 10000 nil)))
        ;; Assert
        (should (= actual (window-total-height win)))
        (should (< actual 10000))))))

(ert-deftest agent-repl-window-test-apply-height-records-a-refused-resize ()
  "A resize the frame refuses is recorded with the error it refused with."
  (agent-repl-window-test--with-temp-frame
    ;; Arrange
    (let ((win (split-window nil -4 'below)))
      ;; Act
      (agent-repl-window-test--capturing-debug
        (agent-repl-window--apply-height win 10000 nil)
        ;; Assert
        (should (cl-find-if
                 (lambda (m)
                   (string-prefix-p "window--apply-height: outcome=refused" m))
                 agent-repl-window-test--debug))))))

(ert-deftest agent-repl-window-test-apply-height-errors-on-a-dead-window ()
  "A dead window is a caller bug and surfaces as one, not as a silent no-op."
  (agent-repl-window-test--with-temp-frame
    ;; Arrange
    (let ((win (split-window nil -4 'below)))
      (delete-window win)
      ;; Act / Assert
      (should-error (agent-repl-window--apply-height win 6 nil)))))

;;;; --- The gate hides the input window --------------------------------------

(ert-deftest agent-repl-window-test-a-standing-gate-hides-the-input ()
  "A gate in the host view hides the workspace's input window."
  (cl-letf (((symbol-function 'agent-repl-host-state)
             (lambda (_ws) '(:gate (:arm :cold-gate :value nil)))))
    ;; Act / Assert
    (should (agent-repl-input-hidden-p "ws"))))

(ert-deftest agent-repl-window-test-no-gate-hides-nothing ()
  "Without a gate the input window stands."
  (cl-letf (((symbol-function 'agent-repl-host-state) (lambda (_ws) '(:gate nil))))
    ;; Act / Assert
    (should-not (agent-repl-input-hidden-p "ws"))))

(ert-deftest agent-repl-window-test-an-unnamed-gate-hides-the-input ()
  "A gate this build has no arm for is still a gate."
  (cl-letf (((symbol-function 'agent-repl-host-state)
             (lambda (_ws) '(:gate (:arm nil :value nil)))))
    ;; Act / Assert
    (should (agent-repl-input-hidden-p "ws"))))

(ert-deftest agent-repl-window-test-a-gate-going-up-remounts-without-the-input ()
  "Both windows on screen when a gate stands: the layout is remounted, so
the input window goes and the webview grows into its space."
  (should (equal (agent-repl-window-test--run-ensure-layout
                  :show-view t :show-input t :gate '(:arm :cold-gate :value nil))
                 '("cur"))))

(ert-deftest agent-repl-window-test-the-view-alone-conforms-under-a-gate ()
  "The view alone is the whole layout while a gate stands: no repair."
  (should-not (agent-repl-window-test--run-ensure-layout
               :show-view t :gate '(:arm :cold-gate :value nil))))

(ert-deftest agent-repl-window-test-a-cleared-gate-restores-the-input ()
  "With the gate gone the lone view is half a pair, and the input returns."
  (should (equal (agent-repl-window-test--run-ensure-layout :show-view t)
                 '("cur"))))

(ert-deftest agent-repl-window-test-a-gate-never-resurrects-hidden-panels ()
  "A gate on a workspace whose panels are closed lays nothing out."
  (should-not (agent-repl-window-test--run-ensure-layout
               :gate '(:arm :cold-gate :value nil))))

(defmacro agent-repl-window-test--with-gate-edges (current &rest body)
  "Run BODY with CURRENT as the workspace on screen and the reconcile recorded.
`reconciled' counts the layout reconciles; `said' holds the INFO records;
`told' holds (WS . RECONCILES-SO-FAR) for each height told to a page."
  (declare (indent 1))
  `(agent-repl-test--with-clean-state
     (let ((agent-repl-window--gates-applied (make-hash-table :test 'equal))
           (reconciled 0)
           (said nil)
           (told nil))
       (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () ,current))
                 ((symbol-function 'agent-repl-window-tell-gate-dock)
                  (lambda (ws) (push (cons ws reconciled) told) 100))
                 ((symbol-function 'agent-repl-window--ensure-layout)
                  (lambda () (cl-incf reconciled)))
                 ((symbol-function 'agent-repl--info)
                  (lambda (_ws fmt &rest args) (push (apply #'format fmt args) said))))
         ,@body))))

(ert-deftest agent-repl-window-test-a-gate-standing-up-on-screen-reconciles ()
  "The gate standing on the workspace on screen re-lays it out at once."
  (agent-repl-window-test--with-gate-edges "a"
    ;; Act
    (agent-repl-window-on-host-update "a" '(:gate (:arm :cold-gate :value nil)))
    ;; Assert
    (should (= reconciled 1))
    (should (equal said '("elisp.gate.standing ws=a kind=:cold-gate")))))

(ert-deftest agent-repl-window-test-a-gate-clearing-on-screen-reconciles ()
  "The gate going away re-lays the workspace on screen out at once."
  (agent-repl-window-test--with-gate-edges "a"
    ;; Arrange
    (agent-repl-window-on-host-update "a" '(:gate (:arm :cold-gate :value nil)))
    (setq reconciled 0 said nil)
    ;; Act
    (agent-repl-window-on-host-update "a" '(:gate nil))
    ;; Assert
    (should (= reconciled 1))
    (should (equal said '("elisp.gate.cleared ws=a kind=:cold-gate")))))

(ert-deftest agent-repl-window-test-a-gate-on-another-workspace-waits-for-its-show ()
  "A gate edge on a workspace not on screen lays nothing out now; its
next show reads the gate (`agent-repl--frontend-display-webview')."
  (agent-repl-window-test--with-gate-edges "a"
    ;; Act
    (agent-repl-window-on-host-update "b" '(:gate (:arm :cold-gate :value nil)))
    ;; Assert
    (should (= reconciled 0))))

(ert-deftest agent-repl-window-test-a-host-update-that-moves-no-gate-changes-nothing ()
  "A host push whose gate did not move (any other row or fact) is no edge."
  (agent-repl-window-test--with-gate-edges "a"
    ;; Arrange
    (agent-repl-window-on-host-update "a" '(:gate (:arm :cold-gate :value nil)))
    (setq reconciled 0 said nil)
    ;; Act
    (agent-repl-window-on-host-update "a" '(:gate (:arm :cold-gate :value nil) :naming (:slug "x")))
    ;; Assert
    (should (= reconciled 0))
    (should (null said))))

(ert-deftest agent-repl-window-test-a-gate-standing-up-tells-the-page-the-input-height-first ()
  "The page learns the input's height before the input hides."
  (agent-repl-window-test--with-gate-edges "a"
    ;; Act
    (agent-repl-window-on-host-update "a" '(:gate (:arm :cold-gate :value nil)))
    ;; Assert
    (should (equal told '(("a" . 0))))))

(ert-deftest agent-repl-window-test-a-gate-clearing-tells-the-page-nothing ()
  "Restoring the input needs no height."
  (agent-repl-window-test--with-gate-edges "a"
    ;; Arrange
    (agent-repl-window-on-host-update "a" '(:gate (:arm :cold-gate :value nil)))
    (setq told nil)
    ;; Act
    (agent-repl-window-on-host-update "a" '(:gate nil))
    ;; Assert
    (should (null told))))

(ert-deftest agent-repl-window-test-the-dock-height-is-the-live-input-windows-pixels ()
  "A live input window's total pixel height is the height sent."
  (agent-repl-test--with-clean-state
    (let ((scripts nil))
      (cl-letf (((symbol-function 'agent-repl-window--panel-window)
                 (lambda (_kind &optional _ws _frame) (selected-window)))
                ((symbol-function 'window-pixel-height) (lambda (&optional _w) 137))
                ((symbol-function 'agent-repl-window--input-background) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--ws-get)
                 (lambda (_ws key) (and (eq key :frontend-buffer) (current-buffer))))
                ((symbol-function 'agent-repl--frontend-webview-read-script)
                 (lambda (_buf script _cb) (push script scripts) t)))
        ;; Act
        (agent-repl-window-tell-gate-dock "ws")
        ;; Assert
        (should (string-match-p "--gate-dock-height','137px'" (car scripts)))))))

(ert-deftest agent-repl-window-test-the-dock-height-without-an-input-window-is-its-mount-height ()
  "With no input window on screen, the height a mount gives it is sent."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl-window--panel-window)
               (lambda (_kind &optional _ws _frame) nil))
              ((symbol-function 'agent-repl-window--input-height) (lambda (&optional _f _ws) 9))
              ((symbol-function 'frame-char-height) (lambda (&optional _f) 20)))
      ;; Act / Assert
      (should (= 180 (agent-repl-window--gate-dock-pixels "ws"))))))

(ert-deftest agent-repl-window-test-the-dock-height-to-a-page-that-is-not-there-is-recorded ()
  "No webview to tell is recorded, never an error."
  (agent-repl-test--with-clean-state
    (let ((said nil))
      (cl-letf (((symbol-function 'agent-repl-window--gate-dock-pixels) (lambda (_ws) 10))
                ((symbol-function 'agent-repl-window--input-background) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--ws-get) (lambda (_ws _key) nil))
                ((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) said))))
        ;; Act
        (agent-repl-window-tell-gate-dock "ws")
        ;; Assert
        (should (equal said '("elisp.gate.dock-slot-skipped ws=ws reason=no-webview")))))))

(defmacro agent-repl-window-test--with-told-scripts (background &rest body)
  "Run BODY with the page's scripts collected in `scripts' and the INFO
records in `said', the input painting BACKGROUND (nil: no input buffer)."
  (declare (indent 1))
  `(agent-repl-test--with-clean-state
     (let ((scripts nil)
           (said nil))
       (cl-letf (((symbol-function 'agent-repl-window--gate-dock-pixels) (lambda (_ws) 137))
                 ((symbol-function 'agent-repl-window--input-background) (lambda (_ws) ,background))
                 ((symbol-function 'agent-repl--ws-get)
                  (lambda (_ws key) (and (eq key :frontend-buffer) (current-buffer))))
                 ((symbol-function 'agent-repl--frontend-webview-read-script)
                  (lambda (_buf script _cb) (push script scripts) t))
                 ((symbol-function 'agent-repl--info)
                  (lambda (_ws fmt &rest args) (push (apply #'format fmt args) said))))
         ,@body))))

(ert-deftest agent-repl-window-test-a-page-load-tells-the-input-background ()
  "The gate's color is told on its own, docked or not."
  (agent-repl-window-test--with-told-scripts "#14141a"
    ;; Act
    (agent-repl-window-tell-input-background "ws")
    ;; Assert
    (should (equal (list "(function(){document.documentElement.style.setProperty('--input-bg',\"#14141a\");return 'ok';})()")
                   scripts))))

(ert-deftest agent-repl-window-test-a-page-load-without-an-input-buffer-tells-nothing ()
  "No input buffer, no color to tell: nothing is sent."
  (agent-repl-window-test--with-told-scripts nil
    ;; Act
    (agent-repl-window-tell-input-background "ws")
    ;; Assert
    (should-not scripts)))

(ert-deftest agent-repl-window-test-a-page-load-without-a-webview-is-skipped ()
  "No live webview: the tell is skipped, and says so."
  (agent-repl-window-test--with-told-scripts "#14141a"
    ;; Arrange
    (let ((logged nil))
      (cl-letf (((symbol-function 'agent-repl--ws-get) (lambda (_ws _key) nil))
                ((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        ;; Act
        (agent-repl-window-tell-input-background "ws"))
      ;; Assert
      (should-not scripts)
      (should (cl-some (lambda (m) (string-match-p "input-bg-skipped.*no-webview" m)) logged)))))

(ert-deftest agent-repl-window-test-the-dock-slot-tells-the-input-background ()
  "The input's background rides the same script as its height."
  (agent-repl-window-test--with-told-scripts "#1d1f21"
    ;; Act
    (agent-repl-window-tell-gate-dock "ws")
    ;; Assert
    (should (string-match-p "--input-bg',\"#1d1f21\"" (car scripts)))))

(ert-deftest agent-repl-window-test-the-dock-slot-tells-the-height-in-the-same-script ()
  "One script carries both, so the page never holds one without the other."
  (agent-repl-window-test--with-told-scripts "#1d1f21"
    ;; Act
    (agent-repl-window-tell-gate-dock "ws")
    ;; Assert
    (should (= 1 (length scripts)))
    (should (string-match-p "--gate-dock-height','137px'" (car scripts)))))

(ert-deftest agent-repl-window-test-the-dock-slot-without-an-input-buffer-tells-no-background ()
  "No input buffer to read tells the height alone; the page keeps its own."
  (agent-repl-window-test--with-told-scripts nil
    ;; Act
    (agent-repl-window-tell-gate-dock "ws")
    ;; Assert
    (should-not (string-match-p "--input-bg" (car scripts)))))

(ert-deftest agent-repl-window-test-the-dock-slot-records-the-background-told ()
  "The told background is on the INFO record."
  (agent-repl-window-test--with-told-scripts "#1d1f21"
    ;; Act
    (agent-repl-window-tell-gate-dock "ws")
    ;; Assert
    (should (equal said '("elisp.gate.dock-slot ws=ws pixels=137 background=#1d1f21")))))

(ert-deftest agent-repl-window-test-the-dock-slot-records-an-untold-background ()
  "A background it could not read is recorded as untold."
  (agent-repl-window-test--with-told-scripts nil
    ;; Act
    (agent-repl-window-tell-gate-dock "ws")
    ;; Assert
    (should (equal said '("elisp.gate.dock-slot ws=ws pixels=137 background=untold")))))

(ert-deftest agent-repl-window-test-the-input-background-is-the-buffers-remapped-one ()
  "The composer's tint, remapped in the input buffer, is what the input paints."
  (agent-repl-test--with-clean-state
    (with-temp-buffer
      (let ((buf (current-buffer)))
        (face-remap-add-relative 'default :background "#14141a")
        (cl-letf (((symbol-function 'agent-repl--input-buffer) (lambda (_ws) buf))
                  ((symbol-function 'face-background) (lambda (&rest _) "#1d1f21")))
          ;; Act / Assert
          (with-temp-buffer
            (should (equal (agent-repl-window--input-background "ws") "#14141a"))))))))

(ert-deftest agent-repl-window-test-the-input-background-without-a-remap-is-the-frames ()
  "An input buffer that remaps nothing paints the frame's default background."
  (agent-repl-test--with-clean-state
    (with-temp-buffer
      (let ((buf (current-buffer)))
        (cl-letf (((symbol-function 'agent-repl--input-buffer) (lambda (_ws) buf))
                  ((symbol-function 'face-background) (lambda (&rest _) "#1d1f21")))
          ;; Act / Assert
          (should (equal (agent-repl-window--input-background "ws") "#1d1f21")))))))

(ert-deftest agent-repl-window-test-the-input-background-without-an-input-buffer-is-nil ()
  "No input buffer, no background to tell."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--input-buffer) (lambda (_ws) nil)))
      ;; Act / Assert
      (should-not (agent-repl-window--input-background "ws")))))

(ert-deftest agent-repl-window-test-the-newest-remap-background-wins ()
  "The highest-priority remapping is first, and it wins."
  ;; Act / Assert
  (should (equal (agent-repl-window--remapped-background
                  '((:background "#222222") (:background "#111111") default))
                 "#222222")))

(ert-deftest agent-repl-window-test-a-remap-that-sets-no-background-is-skipped ()
  "A spec that sets no background (or `default' itself) defers to the next."
  ;; Act / Assert
  (should (equal (agent-repl-window--remapped-background
                  '((:foreground "#ffffff") default (:background "#111111")))
                 "#111111")))

(ert-deftest agent-repl-window-test-a-single-plist-remap-is-read ()
  "A remapping that is one property list, not a list of specs, is read."
  ;; Act / Assert
  (should (equal (agent-repl-window--remapped-background '(:background "#111111"))
                 "#111111")))

(ert-deftest agent-repl-window-test-a-face-remap-counts-by-its-own-background ()
  "A face in the remapping paints its own background."
  (let ((face (make-symbol "agent-repl-window-test-face")))
    ;; Arrange
    (make-face face)
    (set-face-attribute face nil :background "#123456")
    ;; Act / Assert
    (should (equal (agent-repl-window--remapped-background (list face 'default))
                   "#123456"))))
