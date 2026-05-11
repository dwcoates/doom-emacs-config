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

(defmacro claude-repl-window-test--with-temp-frame (&rest body)
  "Run BODY in an isolated single-frame setup with a fresh frame root.
Splits the selected frame's root window to a known starting state
\(one window) before BODY, then restores the prior configuration."
  (declare (indent 0))
  `(let ((wconf (current-window-configuration)))
     (unwind-protect
         (progn (delete-other-windows) ,@body)
       (set-window-configuration wconf))))

;;;; ---- Side-window predicate ----

(ert-deftest claude-repl-window-test-side-window-p-nil-for-main-window ()
  "`--side-window-p' returns nil for a normal (non-side) window."
  (claude-repl-window-test--with-temp-frame
    (should-not (claude-repl-window--side-window-p (selected-window)))))

(ert-deftest claude-repl-window-test-side-window-p-t-for-side-window ()
  "`--side-window-p' returns non-nil for a `display-buffer-in-side-window'
created window — the parameter `window-side' must be the discriminator."
  (claude-repl-window-test--with-temp-frame
    (let* ((buf (generate-new-buffer " *test-side*"))
           (win (display-buffer-in-side-window
                 buf '((side . left) (slot . 0)))))
      (unwind-protect
          (progn
            (should (window-live-p win))
            (should (claude-repl-window--side-window-p win)))
        (when (window-live-p win) (delete-window win))
        (kill-buffer buf)))))

(ert-deftest claude-repl-window-test-side-window-p-nil-for-dead-window ()
  "`--side-window-p' is nil for a dead window — guards against calls
against a stale window reference after teardown."
  (claude-repl-window-test--with-temp-frame
    (let* ((buf (generate-new-buffer " *test-side-dead*"))
           (win (display-buffer-in-side-window
                 buf '((side . left) (slot . 0)))))
      (delete-window win)
      (kill-buffer buf)
      (should-not (claude-repl-window--side-window-p win)))))

;;;; ---- Subset deletion ----

(ert-deftest claude-repl-window-test-delete-where-deletes-matching ()
  "`--delete-where' deletes windows whose PREDICATE returns non-nil."
  (claude-repl-window-test--with-temp-frame
    (let* ((main (selected-window))
           (extra (split-window main)))
      ;; Predicate: every window EXCEPT the originally selected one.
      (claude-repl-window--delete-where
       (lambda (w) (not (eq w main))))
      (should (window-live-p main))
      (should-not (window-live-p extra)))))

(ert-deftest claude-repl-window-test-delete-where-skips-side-windows-by-default ()
  "`--delete-where' preserves side windows by default — this is the
side-window-aware default that stops layout-clearing commands from
trampling frame-level UI elements like the workspace drawer.

Uses explicit `split-window' (not `display-buffer-pop-up-window'),
which is unreliable under `emacs -batch -Q' where window-sizing
heuristics may refuse to split."
  (claude-repl-window-test--with-temp-frame
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
            (claude-repl-window--delete-where
             (lambda (w) (or (eq w extra) (eq w side))))
            (should-not (window-live-p extra))
            (should (window-live-p side)))
        (when (window-live-p side) (delete-window side))
        (kill-buffer side-buf)))))

(ert-deftest claude-repl-window-test-delete-where-can-include-side-windows ()
  "`--delete-where' with `:skip-side-windows nil' DOES delete side
windows when the caller explicitly opts in (e.g. a fullscreen toggle
that genuinely wants a single-panel frame)."
  (claude-repl-window-test--with-temp-frame
    (let* ((side-buf (generate-new-buffer " *test-side-opt-in*"))
           (side (display-buffer-in-side-window
                  side-buf '((side . right) (slot . 0)))))
      (unwind-protect
          (progn
            (should (window-live-p side))
            (claude-repl-window--delete-where
             (lambda (w) (eq w side))
             :skip-side-windows nil)
            (should-not (window-live-p side)))
        (when (window-live-p side) (delete-window side))
        (kill-buffer side-buf)))))

(ert-deftest claude-repl-window-test-delete-where-returns-deleted-list ()
  "`--delete-where' returns the list of windows it actually deleted —
caller can verify the sweep was non-empty."
  (claude-repl-window-test--with-temp-frame
    (let* ((main (selected-window))
           (extra (split-window main))
           (deleted
            (claude-repl-window--delete-where
             (lambda (w) (eq w extra)))))
      (should (equal deleted (list extra)))
      (should-not (window-live-p extra)))))

(ert-deftest claude-repl-window-test-delete-where-empty-predicate-no-op ()
  "`--delete-where' with a never-matching predicate deletes nothing
and returns an empty list."
  (claude-repl-window-test--with-temp-frame
    (let* ((main (selected-window))
           (extra (split-window main))
           (deleted
            (claude-repl-window--delete-where (lambda (_w) nil))))
      (should (null deleted))
      (should (window-live-p main))
      (should (window-live-p extra)))))

;;;; ---- Integration: delete-non-panel-windows regression ----

(ert-deftest claude-repl-window-test-delete-non-panel-preserves-side-windows ()
  "Regression: `claude-repl--delete-non-panel-windows' must preserve a
side window (e.g. the workspace drawer) — pre-consolidation this
function killed the drawer whenever the user opened magit via `SPC g g'.

Mirrors the production layout: one of the two main windows shows the
vterm panel buffer (so the call to `--delete-non-panel-windows' has
something to keep besides the side window), the other main shows an
unrelated buffer and must be deleted, and the side window must
survive."
  (claude-repl-window-test--with-temp-frame
    (let* ((vterm-buf  (generate-new-buffer " *test-vterm*"))
           (input-buf  (generate-new-buffer " *test-input*"))
           (drawer-buf (generate-new-buffer " *test-drawer*"))
           (other-buf  (generate-new-buffer " *test-other*"))
           (vterm-win  (selected-window))
           (other-win  (split-window vterm-win))
           (drawer-win (display-buffer-in-side-window
                        drawer-buf '((side . left) (slot . 0)))))
      ;; Make `vterm-win' actually display vterm-buf — that's what the
      ;; predicate looks for to keep it.
      (set-window-buffer vterm-win  vterm-buf)
      (set-window-buffer other-win  other-buf)
      (unwind-protect
          (progn
            (should (window-live-p drawer-win))
            (should (window-live-p other-win))
            (claude-repl--delete-non-panel-windows vterm-buf input-buf)
            ;; Non-panel non-side window is gone.
            (should-not (window-live-p other-win))
            ;; Panel window survives (predicate didn't match it).
            (should (window-live-p vterm-win))
            ;; Side window (drawer) survives.
            (should (window-live-p drawer-win)))
        (when (window-live-p drawer-win) (delete-window drawer-win))
        (kill-buffer vterm-buf)
        (kill-buffer input-buf)
        (kill-buffer drawer-buf)
        (kill-buffer other-buf)))))

;;; test-window.el ends here
