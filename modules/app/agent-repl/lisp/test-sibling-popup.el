;;; test-sibling-popup.el --- ERT tests for sibling-popup.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `agent-repl-sibling-popup-display-fn' and its helpers.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-sibling-popup.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Helpers ----

(defmacro agent-repl-sibling-popup-test--with-temp-frame (&rest body)
  "Run BODY in a single-window starting state, restoring the prior config."
  (declare (indent 0))
  `(let ((wconf (current-window-configuration)))
     (unwind-protect
         (progn (delete-other-windows) ,@body)
       (set-window-configuration wconf))))

(defmacro agent-repl-sibling-popup-test--with-mocked-panels
    (view-buf input-buf &rest body)
  "Bind `agent-repl-window--panel-buffer' so it resolves panels to
VIEW-BUF (for `:view') and INPUT-BUF (for `:input').  Other kinds
resolve to nil.  Restores the original definition on exit so test
isolation is preserved."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'agent-repl-window--panel-buffer)
              (lambda (kind &optional _ws)
                (pcase kind
                  (:view ,view-buf)
                  (:input ,input-buf)
                  (_ nil)))))
     ,@body))

;; The panels' layout: the sidebar, a left side window, beside the view over
;; the input.  WORK, when non-nil, adds an editor window left of the view.
(defmacro agent-repl-sibling-popup-test--with-panel-layout (work &rest body)
  "Run BODY with the sidebar beside the view over the input, all mocked.
Binds `side-win', `view-win', `input-win' and, when WORK is non-nil,
`work-win', plus `view-buf' and `input-buf'; kills every buffer on exit."
  (declare (indent 1))
  `(agent-repl-sibling-popup-test--with-temp-frame
     (let* ((side-buf  (generate-new-buffer " *test-sidebar*"))
            (work-buf  (generate-new-buffer " *test-work*"))
            (view-buf  (generate-new-buffer " *test-cv*"))
            (input-buf (generate-new-buffer " *test-ci*")))
       (unwind-protect
           (let* ((work-win (and ,work (selected-window)))
                  (view-win (if work-win
                                (split-window work-win nil 'right)
                              (selected-window)))
                  (input-win (split-window view-win nil 'below))
                  (side-win (display-buffer-in-side-window
                             side-buf '((side . left) (slot . -1)))))
             (when work-win (set-window-buffer work-win work-buf))
             (set-window-buffer view-win view-buf)
             (set-window-buffer input-win input-buf)
             (ignore work-win side-win)
             (agent-repl-sibling-popup-test--with-mocked-panels view-buf input-buf
               ,@body))
         (mapc #'kill-buffer (list side-buf work-buf view-buf input-buf))))))

;;;; ---- target-window selection ----

(ert-deftest agent-repl-sibling-popup-test-target-nil-when-view-not-visible ()
  "Returns nil when the agent view buffer isn't shown on the frame.
The display fn must fall through to default popup behavior in that
case — without this guard a single editor window would be split below
itself by every `SPC o t', breaking the no-agent UX."
  (agent-repl-sibling-popup-test--with-temp-frame
    (let ((view-buf (generate-new-buffer " *test-cv*"))
          (input-buf (generate-new-buffer " *test-ci*")))
      (unwind-protect
          (agent-repl-sibling-popup-test--with-mocked-panels view-buf input-buf
            ;; view-buf exists but is not displayed in any window.
            (should-not (agent-repl-sibling-popup--target-window)))
        (kill-buffer view-buf)
        (kill-buffer input-buf)))))

(ert-deftest agent-repl-sibling-popup-test-target-nil-when-view-buf-missing ()
  "Returns nil when the panel-buffer lookup itself yields nil.
This covers the never-initialized-agent state — `--panel-buffer'
returns nil before any agent session is opened, and the display fn
must not crash on the nil."
  (agent-repl-sibling-popup-test--with-temp-frame
    (agent-repl-sibling-popup-test--with-mocked-panels nil nil
      (should-not (agent-repl-sibling-popup--target-window)))))

(ert-deftest agent-repl-sibling-popup-test-target-is-agent-column ()
  "Targets the window holding the view over the input when both stand.
That column sits beside the sidebar, so a popup split below it never
runs under the sidebar."
  (agent-repl-sibling-popup-test--with-panel-layout nil
    (let ((target (agent-repl-sibling-popup--target-window)))
      (should (eq target (window-parent input-win)))
      (should (eq target (window-parent view-win))))))

(ert-deftest agent-repl-sibling-popup-test-target-is-not-sidebar ()
  "Never targets the sidebar's side window, though it is leftmost."
  (agent-repl-sibling-popup-test--with-panel-layout nil
    (should (window-live-p side-win))
    (should-not (eq (agent-repl-sibling-popup--target-window) side-win))))

(ert-deftest agent-repl-sibling-popup-test-target-ignores-work-window ()
  "Targets the agent column even with an editor window beside it.
The popup belongs under the composer, not under whatever window is
leftmost."
  (agent-repl-sibling-popup-test--with-panel-layout t
    (let ((target (agent-repl-sibling-popup--target-window)))
      (should (eq target (window-parent input-win)))
      (should-not (eq target work-win)))))

(ert-deftest agent-repl-sibling-popup-test-target-is-view-when-input-hidden ()
  "Targets the view window when a standing gate hides the input.
The view then stands alone beside the sidebar, so it is the column."
  (agent-repl-sibling-popup-test--with-temp-frame
    (let* ((side-buf  (generate-new-buffer " *test-sidebar*"))
           (view-buf  (generate-new-buffer " *test-cv*"))
           (input-buf (generate-new-buffer " *test-ci*")))
      (unwind-protect
          (let ((view-win (selected-window)))
            (set-window-buffer view-win view-buf)
            (display-buffer-in-side-window side-buf '((side . left) (slot . -1)))
            (agent-repl-sibling-popup-test--with-mocked-panels view-buf input-buf
              (should (eq (agent-repl-sibling-popup--target-window) view-win))))
        (mapc #'kill-buffer (list side-buf view-buf input-buf))))))

;;;; ---- height computation ----

(ert-deftest agent-repl-sibling-popup-test-height-honors-float-fraction ()
  "Float in `window-height' alist entry is treated as a fraction of target."
  (agent-repl-sibling-popup-test--with-temp-frame
    (let* ((win (selected-window))
           (h (window-total-height win))
           (computed (agent-repl-sibling-popup--popup-height
                      win '((window-height . 0.25)))))
      (should (= computed (max window-min-height (round (* 0.25 h))))))))

(ert-deftest agent-repl-sibling-popup-test-height-honors-integer-lines ()
  "Integer in `window-height' alist entry is treated as literal lines."
  (agent-repl-sibling-popup-test--with-temp-frame
    (should (= (agent-repl-sibling-popup--popup-height
                (selected-window) '((window-height . 7)))
               7))))

(ert-deftest agent-repl-sibling-popup-test-height-defaults-to-thirty-percent ()
  "Missing height keys fall back to 30% of target height."
  (agent-repl-sibling-popup-test--with-temp-frame
    (let* ((win (selected-window))
           (h (window-total-height win)))
      (should (= (agent-repl-sibling-popup--popup-height win nil)
                 (max window-min-height (round (* 0.3 h))))))))

(ert-deftest agent-repl-sibling-popup-test-height-floors-at-window-min-height ()
  "Tiny float fractions don't produce sub-min-height splits.
Without the floor a 0.01 fraction on a 30-line window would compute 0,
and `split-window' would signal `window-too-small'."
  (agent-repl-sibling-popup-test--with-temp-frame
    (should (>= (agent-repl-sibling-popup--popup-height
                 (selected-window) '((window-height . 0.001)))
                window-min-height))))

;;;; ---- display-fn integration ----

(ert-deftest agent-repl-sibling-popup-test-display-fn-splits-below-composer ()
  "When the agent view is visible, the popup opens below the composer.
It is a split of the agent column, not a side window; its top edge is
the composer's bottom edge after the split; and the view and input
windows keep their buffers."
  (agent-repl-sibling-popup-test--with-panel-layout nil
    (let* ((popup-buf (generate-new-buffer " *test-popup*"))
           (new-win (agent-repl-sibling-popup-display-fn
                     popup-buf '((window-height . 0.3)))))
      (unwind-protect
          (progn
            (should (window-live-p new-win))
            (should (eq (window-buffer new-win) popup-buf))
            ;; A split, not the frame-wide bottom side window of the fallback.
            (should-not (window-parameter new-win 'window-side))
            (should (eq (window-buffer view-win) view-buf))
            (should (eq (window-buffer input-win) input-buf))
            (should (= (cadr (window-edges new-win))
                       (cadddr (window-edges input-win)))))
        (kill-buffer popup-buf)))))

(ert-deftest agent-repl-sibling-popup-test-display-fn-popup-width-matches-composer ()
  "The popup spans the composer's columns and none of the sidebar's.
This is the core promise of the feature: the sidebar keeps the frame's
whole height while the popup is open."
  (agent-repl-sibling-popup-test--with-panel-layout nil
    (let* ((popup-buf (generate-new-buffer " *test-popup*"))
           (input-left (car (window-edges input-win)))
           (input-right (caddr (window-edges input-win)))
           (side-bottom-before (cadddr (window-edges side-win))))
      (unwind-protect
          (let ((new-win (agent-repl-sibling-popup-display-fn
                          popup-buf '((window-height . 0.3)))))
            (should (= (car (window-edges new-win)) input-left))
            (should (= (caddr (window-edges new-win)) input-right))
            (should (>= (car (window-edges new-win))
                        (caddr (window-edges side-win))))
            (should (= (cadddr (window-edges side-win)) side-bottom-before)))
        (kill-buffer popup-buf)))))

(ert-deftest agent-repl-sibling-popup-test-display-fn-falls-back-when-claude-absent ()
  "With no agent panels visible, the display fn defers to the fallback.
We override the fallback symbol to observe that it's called with the
buffer and alist intact.  This guards the no-agent UX path."
  (agent-repl-sibling-popup-test--with-temp-frame
    (let ((popup-buf (generate-new-buffer " *test-popup*"))
          (calls 0))
      (unwind-protect
          (agent-repl-sibling-popup-test--with-mocked-panels nil nil
            (cl-letf (((symbol-function 'agent-repl-sibling-popup--fallback)
                       (lambda (buf alist &optional _ws)
                         (cl-incf calls)
                         (should (eq buf popup-buf))
                         (should (equal (cdr (assq 'window-height alist)) 0.3))
                         ;; Return a window-ish value so the action protocol is satisfied.
                         (selected-window))))
              (agent-repl-sibling-popup-display-fn
               popup-buf '((window-height . 0.3) (side . bottom)))
              (should (= calls 1))))
        (kill-buffer popup-buf)))))

(ert-deftest agent-repl-sibling-popup-test-display-fn-threads-current-workspace ()
  "Passes the current workspace through target selection and display handling."
  (let ((popup-buf (generate-new-buffer " *test-popup*"))
        (ws "test-workspace")
        observed-target-ws
        observed-fallback-ws)
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                   (lambda () ws))
                  ((symbol-function 'agent-repl-sibling-popup--target-window)
                   (lambda (_frame target-ws)
                     (setq observed-target-ws target-ws)
                     nil))
                  ((symbol-function 'agent-repl-sibling-popup--fallback)
                   (lambda (_buffer _alist fallback-ws)
                     (setq observed-fallback-ws fallback-ws)
                     (selected-window))))
          (should (window-live-p
                   (agent-repl-sibling-popup-display-fn popup-buf nil)))
          (should (equal observed-target-ws ws))
          (should (equal observed-fallback-ws ws)))
      (kill-buffer popup-buf))))

(provide 'test-sibling-popup)
;;; test-sibling-popup.el ends here
