;;; test-sibling-popup.el --- ERT tests for sibling-popup.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `claude-repl-sibling-popup-display-fn' and its helpers.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-sibling-popup.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Helpers ----

(defmacro claude-repl-sibling-popup-test--with-temp-frame (&rest body)
  "Run BODY in a single-window starting state, restoring the prior config."
  (declare (indent 0))
  `(let ((wconf (current-window-configuration)))
     (unwind-protect
         (progn (delete-other-windows) ,@body)
       (set-window-configuration wconf))))

(defmacro claude-repl-sibling-popup-test--with-mocked-panels
    (vterm-buf input-buf &rest body)
  "Bind `claude-repl-window--panel-buffer' so it resolves panels to
VTERM-BUF (for `:vterm') and INPUT-BUF (for `:input').  Other kinds
\(e.g. `:drawer') resolve to nil.  Restores the original definition on
exit so test isolation is preserved."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'claude-repl-window--panel-buffer)
              (lambda (kind &optional _ws)
                (pcase kind
                  (:vterm ,vterm-buf)
                  (:input ,input-buf)
                  (_ nil)))))
     ,@body))

;;;; ---- target-window selection ----

(ert-deftest claude-repl-sibling-popup-test-target-nil-when-vterm-not-visible ()
  "Returns nil when the claude vterm buffer isn't shown on the frame.
The display fn must fall through to default popup behavior in that
case — without this guard a single editor window would be split below
itself by every `SPC o t', breaking the no-claude UX."
  (claude-repl-sibling-popup-test--with-temp-frame
    (let ((vterm-buf (generate-new-buffer " *test-cv*"))
          (input-buf (generate-new-buffer " *test-ci*")))
      (unwind-protect
          (claude-repl-sibling-popup-test--with-mocked-panels vterm-buf input-buf
            ;; vterm-buf exists but is not displayed in any window.
            (should-not (claude-repl-sibling-popup--target-window)))
        (kill-buffer vterm-buf)
        (kill-buffer input-buf)))))

(ert-deftest claude-repl-sibling-popup-test-target-nil-when-vterm-buf-missing ()
  "Returns nil when the panel-buffer lookup itself yields nil.
This covers the never-initialized-claude state — `--panel-buffer'
returns nil before any claude session is opened, and the display fn
must not crash on the nil."
  (claude-repl-sibling-popup-test--with-temp-frame
    (claude-repl-sibling-popup-test--with-mocked-panels nil nil
      (should-not (claude-repl-sibling-popup--target-window)))))

(ert-deftest claude-repl-sibling-popup-test-target-picks-leftmost-work-window ()
  "Picks the leftmost non-claude, non-side work window when vterm is shown.
Layout: [work] [claude-vterm] horizontally.  Target must be the work
window (left), NOT the claude vterm window (right), because the popup
is meant to drop below the work column only."
  (claude-repl-sibling-popup-test--with-temp-frame
    (let* ((work-buf  (generate-new-buffer " *test-work*"))
           (vterm-buf (generate-new-buffer " *test-cv*"))
           (input-buf (generate-new-buffer " *test-ci*")))
      (unwind-protect
          (let* ((work-win (selected-window))
                 (_ (set-window-buffer work-win work-buf))
                 (vterm-win (split-window work-win nil 'right)))
            (set-window-buffer vterm-win vterm-buf)
            (claude-repl-sibling-popup-test--with-mocked-panels vterm-buf input-buf
              (should (eq (claude-repl-sibling-popup--target-window) work-win))))
        (kill-buffer work-buf)
        (kill-buffer vterm-buf)
        (kill-buffer input-buf)))))

(ert-deftest claude-repl-sibling-popup-test-target-excludes-side-windows ()
  "Skips side windows (e.g. the drawer) even if they are the leftmost.
Side windows shouldn't be split — they're frame-level UI.  Layout: a
left-side drawer + a regular work window + claude vterm; target must
be the regular work window, not the drawer."
  (claude-repl-sibling-popup-test--with-temp-frame
    (let* ((drawer-buf (generate-new-buffer " *test-drawer*"))
           (work-buf   (generate-new-buffer " *test-work*"))
           (vterm-buf  (generate-new-buffer " *test-cv*"))
           (input-buf  (generate-new-buffer " *test-ci*")))
      (unwind-protect
          (let* ((work-win  (selected-window))
                 (_ (set-window-buffer work-win work-buf))
                 (vterm-win (split-window work-win nil 'right))
                 (_ (set-window-buffer vterm-win vterm-buf))
                 (drawer-win (display-buffer-in-side-window
                              drawer-buf '((side . left) (slot . 0)))))
            (should (window-live-p drawer-win))
            (claude-repl-sibling-popup-test--with-mocked-panels vterm-buf input-buf
              (let ((target (claude-repl-sibling-popup--target-window)))
                (should (eq target work-win))
                (should-not (claude-repl-window--side-window-p target)))))
        (kill-buffer drawer-buf)
        (kill-buffer work-buf)
        (kill-buffer vterm-buf)
        (kill-buffer input-buf)))))

(ert-deftest claude-repl-sibling-popup-test-target-excludes-claude-input-panel ()
  "Doesn't pick the claude input panel even when it's leftmost.
A degenerate frame where only claude buffers are shown (no editor
window) must still return nil — splitting below the input would
deform the claude column."
  (claude-repl-sibling-popup-test--with-temp-frame
    (let* ((vterm-buf (generate-new-buffer " *test-cv*"))
           (input-buf (generate-new-buffer " *test-ci*")))
      (unwind-protect
          (let* ((vterm-win (selected-window))
                 (_ (set-window-buffer vterm-win vterm-buf))
                 (input-win (split-window vterm-win nil 'below)))
            (set-window-buffer input-win input-buf)
            (claude-repl-sibling-popup-test--with-mocked-panels vterm-buf input-buf
              (should-not (claude-repl-sibling-popup--target-window))))
        (kill-buffer vterm-buf)
        (kill-buffer input-buf)))))

;;;; ---- height computation ----

(ert-deftest claude-repl-sibling-popup-test-height-honors-float-fraction ()
  "Float in `window-height' alist entry is treated as a fraction of target."
  (claude-repl-sibling-popup-test--with-temp-frame
    (let* ((win (selected-window))
           (h (window-total-height win))
           (computed (claude-repl-sibling-popup--popup-height
                      win '((window-height . 0.25)))))
      (should (= computed (max window-min-height (round (* 0.25 h))))))))

(ert-deftest claude-repl-sibling-popup-test-height-honors-integer-lines ()
  "Integer in `window-height' alist entry is treated as literal lines."
  (claude-repl-sibling-popup-test--with-temp-frame
    (should (= (claude-repl-sibling-popup--popup-height
                (selected-window) '((window-height . 7)))
               7))))

(ert-deftest claude-repl-sibling-popup-test-height-defaults-to-thirty-percent ()
  "Missing height keys fall back to 30% of target height."
  (claude-repl-sibling-popup-test--with-temp-frame
    (let* ((win (selected-window))
           (h (window-total-height win)))
      (should (= (claude-repl-sibling-popup--popup-height win nil)
                 (max window-min-height (round (* 0.3 h))))))))

(ert-deftest claude-repl-sibling-popup-test-height-floors-at-window-min-height ()
  "Tiny float fractions don't produce sub-min-height splits.
Without the floor a 0.01 fraction on a 30-line window would compute 0,
and `split-window' would signal `window-too-small'."
  (claude-repl-sibling-popup-test--with-temp-frame
    (should (>= (claude-repl-sibling-popup--popup-height
                 (selected-window) '((window-height . 0.001)))
                window-min-height))))

;;;; ---- display-fn integration ----

(ert-deftest claude-repl-sibling-popup-test-display-fn-splits-below-work-window ()
  "When claude vterm is visible, the display fn splits BELOW the work
window and places BUFFER there.  Layout invariants verified:
  • new popup window is a child of (or successor below) the work-win edge
  • popup buffer is the supplied buffer
  • claude vterm window is left untouched (still showing vterm-buf)"
  (claude-repl-sibling-popup-test--with-temp-frame
    (let* ((work-buf  (generate-new-buffer " *test-work*"))
           (vterm-buf (generate-new-buffer " *test-cv*"))
           (input-buf (generate-new-buffer " *test-ci*"))
           (popup-buf (generate-new-buffer " *test-popup*")))
      (unwind-protect
          (let* ((work-win (selected-window))
                 (_ (set-window-buffer work-win work-buf))
                 (vterm-win (split-window work-win nil 'right))
                 (_ (set-window-buffer vterm-win vterm-buf))
                 (work-bottom-before (cadddr (window-edges work-win))))
            (claude-repl-sibling-popup-test--with-mocked-panels vterm-buf input-buf
              (let ((new-win (claude-repl-sibling-popup-display-fn
                              popup-buf '((window-height . 0.3)))))
                (should (window-live-p new-win))
                (should (eq (window-buffer new-win) popup-buf))
                ;; Claude vterm window survives untouched.
                (should (window-live-p vterm-win))
                (should (eq (window-buffer vterm-win) vterm-buf))
                ;; New window sits below the original work-win region:
                ;; its top edge is <= the work-win's pre-split bottom.
                (should (<= (cadr (window-edges new-win)) work-bottom-before)))))
        (kill-buffer work-buf)
        (kill-buffer vterm-buf)
        (kill-buffer input-buf)
        (kill-buffer popup-buf)))))

(ert-deftest claude-repl-sibling-popup-test-display-fn-popup-width-matches-work-column ()
  "The popup width matches the work column (not the full frame).
This is the core promise of the feature: the popup must stop on the
right side at the claude window edge.  Verify the popup's right edge
equals the work window's right edge BEFORE the popup was opened (the
column width is preserved by `split-window' below)."
  (claude-repl-sibling-popup-test--with-temp-frame
    (let* ((work-buf  (generate-new-buffer " *test-work*"))
           (vterm-buf (generate-new-buffer " *test-cv*"))
           (input-buf (generate-new-buffer " *test-ci*"))
           (popup-buf (generate-new-buffer " *test-popup*")))
      (unwind-protect
          (let* ((work-win (selected-window))
                 (_ (set-window-buffer work-win work-buf))
                 (vterm-win (split-window work-win nil 'right))
                 (_ (set-window-buffer vterm-win vterm-buf))
                 ;; Capture work window's horizontal extent before split.
                 (work-left   (car (window-edges work-win)))
                 (work-right  (caddr (window-edges work-win)))
                 (frame-right (caddr (window-edges (frame-root-window)))))
            ;; Sanity: work window does NOT span the full frame width
            ;; (otherwise there's nothing to assert).
            (should (< work-right frame-right))
            (claude-repl-sibling-popup-test--with-mocked-panels vterm-buf input-buf
              (let ((new-win (claude-repl-sibling-popup-display-fn
                              popup-buf '((window-height . 0.3)))))
                (should (window-live-p new-win))
                (should (= (car (window-edges new-win)) work-left))
                (should (= (caddr (window-edges new-win)) work-right))
                ;; And — explicitly — it does NOT reach the frame's right edge.
                (should (< (caddr (window-edges new-win)) frame-right)))))
        (kill-buffer work-buf)
        (kill-buffer vterm-buf)
        (kill-buffer input-buf)
        (kill-buffer popup-buf)))))

(ert-deftest claude-repl-sibling-popup-test-display-fn-falls-back-when-claude-absent ()
  "With no claude panels visible, the display fn defers to the fallback.
We override the fallback symbol to observe that it's called with the
buffer and alist intact.  This guards the no-claude UX path."
  (claude-repl-sibling-popup-test--with-temp-frame
    (let ((popup-buf (generate-new-buffer " *test-popup*"))
          (calls 0))
      (unwind-protect
          (claude-repl-sibling-popup-test--with-mocked-panels nil nil
            (cl-letf (((symbol-function 'claude-repl-sibling-popup--fallback)
                       (lambda (buf alist)
                         (cl-incf calls)
                         (should (eq buf popup-buf))
                         (should (equal (cdr (assq 'window-height alist)) 0.3))
                         ;; Return a window-ish value so the action protocol is satisfied.
                         (selected-window))))
              (claude-repl-sibling-popup-display-fn
               popup-buf '((window-height . 0.3) (side . bottom)))
              (should (= calls 1))))
        (kill-buffer popup-buf)))))

(provide 'test-sibling-popup)
;;; test-sibling-popup.el ends here
