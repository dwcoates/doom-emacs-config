;;; test-overlay.el --- ERT tests for claude-repl overlay system -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for overlay.el — the hide-overlay system that conceals the
;; bottom lines of the Claude vterm buffer.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-overlay.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: claude-repl--hide-overlay-region ----

(ert-deftest claude-repl-test-overlay-region-empty-buffer ()
  "In an empty buffer (point-max = 1), `hide-overlay-region' returns nil."
  (claude-repl-test--with-temp-buffer " *test-region-empty*"
    (should-not (claude-repl--hide-overlay-region))))

(ert-deftest claude-repl-test-overlay-region-single-char ()
  "A buffer with a single character should return a valid region."
  (claude-repl-test--with-temp-buffer " *test-region-single*"
    (insert "x")
    (let ((region (claude-repl--hide-overlay-region)))
      (should region)
      (should (< (car region) (cdr region))))))

(ert-deftest claude-repl-test-overlay-region-fewer-lines-than-count ()
  "When the buffer has fewer lines than `hide-overlay-line-count', region covers from beginning."
  (claude-repl-test--with-temp-buffer " *test-region-few*"
    (insert "line1\nline2\n")
    (let ((region (claude-repl--hide-overlay-region)))
      (should region)
      ;; start should be at point-min since there are fewer lines than the count
      (should (= (car region) (point-min)))
      (should (= (cdr region) (point-max))))))

(ert-deftest claude-repl-test-overlay-region-exact-line-count ()
  "When the buffer has exactly `hide-overlay-line-count' lines, region covers them."
  (claude-repl-test--with-temp-buffer " *test-region-exact*"
    ;; 4 lines matching the constant
    (insert "line1\nline2\nline3\nline4\n")
    (let ((region (claude-repl--hide-overlay-region)))
      (should region)
      (should (= (car region) (point-min)))
      (should (= (cdr region) (point-max))))))

(ert-deftest claude-repl-test-overlay-region-more-lines-than-count ()
  "When the buffer has more lines than `hide-overlay-line-count', region starts after the visible lines."
  (claude-repl-test--with-temp-buffer " *test-region-more*"
    ;; 8 lines, hide-overlay-line-count is 4
    (insert "line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\n")
    (let ((region (claude-repl--hide-overlay-region)))
      (should region)
      ;; start should NOT be point-min since there are more lines
      (should (> (car region) (point-min)))
      (should (= (cdr region) (point-max))))))

;;;; ---- Tests: claude-repl--create-hide-overlay ----

(ert-deftest claude-repl-test-create-overlay-respects-flag ()
  "When `claude-repl-hide-input-box' is nil, `create-hide-overlay' should NOT create an overlay."
  (claude-repl-test--with-temp-buffer " *test-no-overlay*"
    (let ((claude-repl-hide-input-box nil))
      (insert "some content\nline two\nline three\nline four\nline five\n")
      (claude-repl--create-hide-overlay)
      (should-not claude-repl-hide-overlay))))

(ert-deftest claude-repl-test-create-overlay-creates-when-enabled ()
  "When `claude-repl-hide-input-box' is t and buffer has content, overlay is created."
  (claude-repl-test--with-temp-buffer " *test-yes-overlay*"
    (let ((claude-repl-hide-input-box t))
      (insert "line1\nline2\nline3\nline4\nline5\nline6\n")
      (claude-repl--create-hide-overlay)
      (should claude-repl-hide-overlay)
      (should (overlayp claude-repl-hide-overlay)))))

(ert-deftest claude-repl-test-create-overlay-empty-buffer ()
  "In an empty buffer, `create-hide-overlay' should not create an overlay even when enabled."
  (claude-repl-test--with-temp-buffer " *test-create-empty*"
    (let ((claude-repl-hide-input-box t))
      (claude-repl--create-hide-overlay)
      (should-not claude-repl-hide-overlay))))

(ert-deftest claude-repl-test-create-overlay-has-display-property ()
  "The created overlay should have a 'display property set to empty string."
  (claude-repl-test--with-temp-buffer " *test-overlay-display*"
    (let ((claude-repl-hide-input-box t))
      (insert "line1\nline2\nline3\nline4\nline5\n")
      (claude-repl--create-hide-overlay)
      (should claude-repl-hide-overlay)
      (should (equal (overlay-get claude-repl-hide-overlay 'display) "")))))

(ert-deftest claude-repl-test-create-overlay-has-evaporate-property ()
  "The created overlay should have the 'evaporate property set to t."
  (claude-repl-test--with-temp-buffer " *test-overlay-evaporate*"
    (let ((claude-repl-hide-input-box t))
      (insert "line1\nline2\nline3\nline4\nline5\n")
      (claude-repl--create-hide-overlay)
      (should claude-repl-hide-overlay)
      (should (overlay-get claude-repl-hide-overlay 'evaporate)))))

;;;; ---- Tests: claude-repl--delete-hide-overlay ----

(ert-deftest claude-repl-test-delete-overlay-noop-when-nil ()
  "When `claude-repl-hide-overlay' is nil, `delete-hide-overlay' should not error."
  (claude-repl-test--with-temp-buffer " *test-del-nil*"
    (setq-local claude-repl-hide-overlay nil)
    ;; Should not signal any error
    (claude-repl--delete-hide-overlay)
    (should-not claude-repl-hide-overlay)))

(ert-deftest claude-repl-test-delete-overlay-deletes-existing ()
  "When an overlay exists, `delete-hide-overlay' should remove it and nil the variable."
  (claude-repl-test--with-temp-buffer " *test-del-existing*"
    (let ((claude-repl-hide-input-box t))
      (insert "line1\nline2\nline3\nline4\nline5\n")
      (claude-repl--create-hide-overlay)
      (should claude-repl-hide-overlay)
      (let ((ov claude-repl-hide-overlay))
        (claude-repl--delete-hide-overlay)
        (should-not claude-repl-hide-overlay)
        ;; The overlay should no longer be attached to a buffer
        (should-not (overlay-buffer ov))))))

(ert-deftest claude-repl-test-delete-overlay-handles-already-deleted ()
  "If the overlay exists but its buffer is already killed, `delete-hide-overlay' should still nil the variable."
  (claude-repl-test--with-temp-buffer " *test-del-killed*"
    (let ((claude-repl-hide-input-box t))
      (insert "line1\nline2\nline3\nline4\nline5\n")
      (claude-repl--create-hide-overlay)
      (should claude-repl-hide-overlay)
      ;; Manually delete the overlay to simulate buffer-killed scenario
      (delete-overlay claude-repl-hide-overlay)
      ;; overlay-buffer returns nil for a deleted overlay
      (should-not (overlay-buffer claude-repl-hide-overlay))
      ;; delete-hide-overlay should handle this gracefully
      (claude-repl--delete-hide-overlay)
      (should-not claude-repl-hide-overlay))))

;;;; ---- Tests: claude-repl--resolve-overlay-target-buffer ----

(ert-deftest claude-repl-test-resolve-target-in-claude-buffer ()
  "In a claude buffer, `resolve-overlay-target-buffer' returns current buffer."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
      (should (eq (claude-repl--resolve-overlay-target-buffer) (current-buffer))))))

(ert-deftest claude-repl-test-resolve-target-non-claude-with-ws ()
  "In a non-claude buffer, resolves via workspace vterm-buffer."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*scratch-test*"
      (let ((fake-buf (get-buffer-create " *fake-vterm*")))
        (unwind-protect
            (cl-letf (((symbol-function '+workspace-current-name)
                       (lambda () "test-ws")))
              (claude-repl--ws-put "test-ws" :vterm-buffer fake-buf)
              (should (eq (claude-repl--resolve-overlay-target-buffer) fake-buf)))
          (kill-buffer fake-buf))))))

(ert-deftest claude-repl-test-resolve-target-non-claude-no-ws ()
  "In a non-claude buffer with no workspace, returns nil."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*scratch-test2*"
      (cl-letf (((symbol-function '+workspace-current-name)
                 (lambda () nil)))
        (should-not (claude-repl--resolve-overlay-target-buffer))))))

(ert-deftest claude-repl-test-resolve-target-non-claude-no-vterm ()
  "In a non-claude buffer where workspace has no vterm-buffer, returns nil."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*scratch-test3*"
      (cl-letf (((symbol-function '+workspace-current-name)
                 (lambda () "empty-ws")))
        ;; Workspace exists but has no :vterm-buffer
        (should-not (claude-repl--resolve-overlay-target-buffer))))))

;;;; ---- Tests: claude-repl--update-hide-overlay ----

(ert-deftest claude-repl-test-update-overlay-nil-buffer ()
  "When resolve returns nil, `update-hide-overlay' should be a no-op."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--resolve-overlay-target-buffer)
               (lambda () nil)))
      ;; Should not error
      (claude-repl--update-hide-overlay))))

(ert-deftest claude-repl-test-update-overlay-dead-buffer ()
  "When resolve returns a killed buffer, `update-hide-overlay' should be a no-op."
  (claude-repl-test--with-clean-state
    (let ((dead-buf (get-buffer-create " *dead-buf*")))
      (kill-buffer dead-buf)
      (cl-letf (((symbol-function 'claude-repl--resolve-overlay-target-buffer)
                 (lambda () dead-buf)))
        ;; Should not error
        (claude-repl--update-hide-overlay)))))

(ert-deftest claude-repl-test-update-overlay-recreates ()
  "update-hide-overlay deletes old overlay and creates a new one."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-update-recreate*"
      (let ((claude-repl-hide-input-box t))
        (insert "line1\nline2\nline3\nline4\nline5\nline6\n")
        (cl-letf (((symbol-function 'claude-repl--resolve-overlay-target-buffer)
                   (lambda () (current-buffer))))
          (claude-repl--update-hide-overlay)
          (let ((first-ov claude-repl-hide-overlay))
            (should first-ov)
            ;; Update again — should recreate
            (claude-repl--update-hide-overlay)
            (should claude-repl-hide-overlay)
            ;; Old overlay should be detached
            (should-not (overlay-buffer first-ov))))))))

;;;; ---- Tests: Overlay refcount (advice management) ----

(ert-deftest claude-repl-test-overlay-refcount ()
  "Overlay advice should be added once and removed only when refcount hits 0."
  (claude-repl-test--with-clean-state
    (let ((advice-added 0)
          (advice-removed 0))
      (cl-letf (((symbol-function 'advice-add)
                 (lambda (&rest _) (cl-incf advice-added)))
                ((symbol-function 'advice-remove)
                 (lambda (&rest _) (cl-incf advice-removed))))
        ;; Enable twice
        (claude-repl--enable-hide-overlay)
        (claude-repl--enable-hide-overlay)
        (should (= advice-added 1))
        (should (= claude-repl--hide-overlay-refcount 2))
        ;; Disable once — advice should NOT be removed
        (claude-repl--disable-hide-overlay)
        (should (= advice-removed 0))
        (should (= claude-repl--hide-overlay-refcount 1))
        ;; Disable again — now advice should be removed
        (claude-repl--disable-hide-overlay)
        (should (= advice-removed 1))
        (should (= claude-repl--hide-overlay-refcount 0))
        ;; Extra disable should clamp to 0
        (claude-repl--disable-hide-overlay)
        (should (= claude-repl--hide-overlay-refcount 0))))))

(ert-deftest claude-repl-test-enable-overlay-from-zero ()
  "Enabling from refcount 0 should add advice exactly once."
  (claude-repl-test--with-clean-state
    (let ((advice-added 0))
      (cl-letf (((symbol-function 'advice-add)
                 (lambda (&rest _) (cl-incf advice-added)))
                ((symbol-function 'advice-remove)
                 (lambda (&rest _) nil)))
        (claude-repl--enable-hide-overlay)
        (should (= advice-added 1))
        (should (= claude-repl--hide-overlay-refcount 1))))))

(ert-deftest claude-repl-test-enable-overlay-from-nonzero ()
  "Enabling from refcount > 0 should NOT add advice again."
  (claude-repl-test--with-clean-state
    (let ((advice-added 0))
      (cl-letf (((symbol-function 'advice-add)
                 (lambda (&rest _) (cl-incf advice-added)))
                ((symbol-function 'advice-remove)
                 (lambda (&rest _) nil)))
        (claude-repl--enable-hide-overlay)
        (claude-repl--enable-hide-overlay)
        (claude-repl--enable-hide-overlay)
        (should (= advice-added 1))
        (should (= claude-repl--hide-overlay-refcount 3))))))

(ert-deftest claude-repl-test-disable-overlay-clamps-at-zero ()
  "Disabling past zero should clamp refcount to 0, not go negative."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'advice-add)
               (lambda (&rest _) nil))
              ((symbol-function 'advice-remove)
               (lambda (&rest _) nil)))
      (claude-repl--disable-hide-overlay)
      (should (= claude-repl--hide-overlay-refcount 0))
      (claude-repl--disable-hide-overlay)
      (should (= claude-repl--hide-overlay-refcount 0)))))

(ert-deftest claude-repl-test-disable-overlay-cleans-up-overlay ()
  "Disabling always calls `delete-hide-overlay' to clean up the buffer-local overlay."
  (claude-repl-test--with-clean-state
    (let ((delete-called 0))
      (cl-letf (((symbol-function 'advice-add)
                 (lambda (&rest _) nil))
                ((symbol-function 'advice-remove)
                 (lambda (&rest _) nil))
                ((symbol-function 'claude-repl--delete-hide-overlay)
                 (lambda () (cl-incf delete-called))))
        (claude-repl--enable-hide-overlay)
        (claude-repl--disable-hide-overlay)
        (should (= delete-called 1))))))

;;;; ---- Tests: claude-repl--vterm-redraw-advice ----

(ert-deftest claude-repl-test-redraw-advice-reentrancy-guard ()
  "Reentrancy guard prevents recursive calls to `after-vterm-redraw'."
  (claude-repl-test--with-clean-state
    (let ((update-count 0))
      (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
        (cl-letf (((symbol-function 'claude-repl--update-hide-overlay)
                   (lambda () (cl-incf update-count))))
          ;; With reentrancy guard active, update-hide-overlay should NOT be called
          (let ((claude-repl--in-redraw-advice t))
            (claude-repl--vterm-redraw-advice)
            (should (= update-count 0)))
          ;; Without guard, in a claude buffer, update-hide-overlay IS called
          (let ((claude-repl--in-redraw-advice nil))
            (claude-repl--vterm-redraw-advice)
            (should (= update-count 1))))))))

(ert-deftest claude-repl-test-redraw-advice-skips-non-claude ()
  "In a non-claude buffer, `after-vterm-redraw' should not call `update-hide-overlay'."
  (claude-repl-test--with-clean-state
    (let ((update-count 0))
      (claude-repl-test--with-temp-buffer "*scratch*"
        (cl-letf (((symbol-function 'claude-repl--update-hide-overlay)
                   (lambda () (cl-incf update-count))))
          (let ((claude-repl--in-redraw-advice nil))
            (claude-repl--vterm-redraw-advice)
            (should (= update-count 0))))))))

(ert-deftest claude-repl-test-redraw-advice-accepts-args ()
  "The redraw advice function should accept and ignore any arguments."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*claude-test9999*"
      (cl-letf (((symbol-function 'claude-repl--update-hide-overlay)
                 (lambda () nil)))
        ;; Should not error when called with extra arguments
        (claude-repl--vterm-redraw-advice 'arg1 'arg2 'arg3)))))

(ert-deftest claude-repl-test-redraw-advice-sets-reentrancy-flag ()
  "While executing, the advice should set `claude-repl--in-redraw-advice' to t."
  (claude-repl-test--with-clean-state
    (let ((flag-during-update nil))
      (claude-repl-test--with-temp-buffer "*claude-flagcheck*"
        (cl-letf (((symbol-function 'claude-repl--update-hide-overlay)
                   (lambda ()
                     (setq flag-during-update claude-repl--in-redraw-advice))))
          (let ((claude-repl--in-redraw-advice nil))
            (claude-repl--vterm-redraw-advice)
            (should flag-during-update)
            ;; After returning, the outer binding should still be nil
            (should-not claude-repl--in-redraw-advice)))))))

;;;; ---- Tests: claude-repl-toggle-hide-input-box ----

(ert-deftest claude-repl-test-toggle-enables-when-disabled ()
  "Toggling when disabled should set `claude-repl-hide-input-box' to t."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-hide-input-box nil)
          (update-called nil))
      (cl-letf (((symbol-function 'claude-repl--update-hide-overlay)
                 (lambda () (setq update-called t))))
        (claude-repl-toggle-hide-input-box)
        (should claude-repl-hide-input-box)
        (should update-called)))))

(ert-deftest claude-repl-test-toggle-disables-when-enabled ()
  "Toggling when enabled should set `claude-repl-hide-input-box' to nil."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-hide-input-box t))
      (cl-letf (((symbol-function 'claude-repl--update-hide-overlay)
                 (lambda () nil)))
        (claude-repl-toggle-hide-input-box)
        (should-not claude-repl-hide-input-box)))))

(ert-deftest claude-repl-test-toggle-calls-update ()
  "Toggling should always call `update-hide-overlay'."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-hide-input-box nil)
          (update-count 0))
      (cl-letf (((symbol-function 'claude-repl--update-hide-overlay)
                 (lambda () (cl-incf update-count))))
        (claude-repl-toggle-hide-input-box)
        (claude-repl-toggle-hide-input-box)
        (should (= update-count 2))))))

;;;; ---- Tests: Constants and variables ----

(ert-deftest claude-repl-test-hide-overlay-line-count-is-positive ()
  "The line count constant should be a positive integer."
  (should (integerp claude-repl--hide-overlay-line-count))
  (should (> claude-repl--hide-overlay-line-count 0)))

(ert-deftest claude-repl-test-in-redraw-advice-declared ()
  "`claude-repl--in-redraw-advice' should be `boundp'."
  (should (boundp 'claude-repl--in-redraw-advice)))

;;;; ---- Tests: Remaining edge cases ----

(ert-deftest claude-repl-test-overlay-region-narrowed-buffer ()
  "In a narrowed buffer, `hide-overlay-region' respects narrowing boundaries."
  (claude-repl-test--with-temp-buffer " *test-region-narrowed*"
    (insert "line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\n")
    ;; Narrow to the last 4 lines (line5 through end)
    (save-excursion
      (goto-char (point-min))
      (forward-line 4)
      (narrow-to-region (point) (point-max)))
    (let ((region (claude-repl--hide-overlay-region)))
      (should region)
      ;; point-min in the narrowed region should not be 1
      (should (> (point-min) 1))
      ;; The region should lie within the narrowed bounds
      (should (>= (car region) (point-min)))
      (should (<= (cdr region) (point-max))))))

(ert-deftest claude-repl-test-create-overlay-front-advance ()
  "The created overlay should have front-advance set to t."
  (claude-repl-test--with-temp-buffer " *test-overlay-front-advance*"
    (let ((claude-repl-hide-input-box t)
          (captured-front-advance nil)
          (orig-make-overlay (symbol-function 'make-overlay)))
      (insert "line1\nline2\nline3\nline4\nline5\n")
      (cl-letf (((symbol-function 'make-overlay)
                 (lambda (beg end &optional buffer front-advance rear-advance)
                   (setq captured-front-advance front-advance)
                   (funcall orig-make-overlay beg end buffer front-advance rear-advance))))
        (claude-repl--create-hide-overlay)
        (should (eq captured-front-advance t))))))

(ert-deftest claude-repl-test-create-overlay-twice-leaks ()
  "Calling `create-hide-overlay' twice without deleting leaks the first overlay."
  (claude-repl-test--with-temp-buffer " *test-create-twice*"
    (let ((claude-repl-hide-input-box t))
      (insert "line1\nline2\nline3\nline4\nline5\n")
      (claude-repl--create-hide-overlay)
      (let ((first-ov claude-repl-hide-overlay))
        (should (overlayp first-ov))
        ;; Call again without deleting
        (claude-repl--create-hide-overlay)
        (let ((second-ov claude-repl-hide-overlay))
          (should (overlayp second-ov))
          ;; The two overlays are different objects
          (should-not (eq first-ov second-ov))
          ;; The first overlay is still attached (leaked)
          (should (overlay-buffer first-ov))
          ;; Clean up
          (delete-overlay first-ov)
          (delete-overlay second-ov))))))

(ert-deftest claude-repl-test-delete-overlay-non-overlay-value ()
  "When `claude-repl-hide-overlay' is non-nil but not an overlay, `delete-hide-overlay' signals an error."
  (claude-repl-test--with-temp-buffer " *test-del-non-overlay*"
    (setq-local claude-repl-hide-overlay "not-an-overlay")
    ;; overlay-buffer will error on a non-overlay argument
    (should-error (claude-repl--delete-hide-overlay) :type 'wrong-type-argument)))

(ert-deftest claude-repl-test-resolve-target-killed-vterm-buffer ()
  "When the workspace vterm-buffer is killed, resolve returns the killed buffer."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*scratch-killed-vterm*"
      (let ((killed-buf (get-buffer-create " *killed-vterm*")))
        (kill-buffer killed-buf)
        (cl-letf (((symbol-function '+workspace-current-name)
                   (lambda () "killed-ws")))
          (claude-repl--ws-put "killed-ws" :vterm-buffer killed-buf)
          (let ((result (claude-repl--resolve-overlay-target-buffer)))
            ;; Returns the killed buffer object (not nil)
            (should result)
            (should-not (buffer-live-p result))))))))

(ert-deftest claude-repl-test-toggle-is-interactive ()
  "`claude-repl-toggle-hide-input-box' should be an interactive command."
  (should (commandp #'claude-repl-toggle-hide-input-box)))

(ert-deftest claude-repl-test-toggle-displays-message ()
  "Toggling should display a message indicating the new state."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-hide-input-box nil)
          (captured-message nil))
      (cl-letf (((symbol-function 'claude-repl--update-hide-overlay)
                 (lambda () nil))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq captured-message (apply #'format fmt args)))))
        (claude-repl-toggle-hide-input-box)
        (should (stringp captured-message))
        (should (string-match-p "enabled" captured-message))))))

;;;; ---- Tests: Display helpers (moved from core.el) ----

(ert-deftest claude-repl-test-grey-format ()
  "Grey helper should return proper hex color."
  (should (equal (claude-repl--grey-hex 0) "#000000"))
  (should (equal (claude-repl--grey-hex 255) "#ffffff"))
  (should (equal (claude-repl--grey-hex 15) "#0f0f0f")))

(ert-deftest claude-repl-test-grey-hex-boundary-128 ()
  "grey-hex for 128 (middle grey) should return #808080."
  (should (equal (claude-repl--grey-hex 128) "#808080")))

(ert-deftest claude-repl-test-vterm-background-grey-value ()
  "`claude-repl--vterm-background-grey' should be 15."
  (should (= claude-repl--vterm-background-grey 15)))

;;;; ---- Tests: set-buffer-background ----

(ert-deftest claude-repl-test-set-buffer-background-sets-faces ()
  "set-buffer-background should remap default and fringe faces."
  (claude-repl-test--with-temp-buffer " *test-bg*"
    (let ((remapped-faces nil))
      (cl-letf (((symbol-function 'face-remap-add-relative)
                 (lambda (face &rest props)
                   (push (list face props) remapped-faces))))
        (claude-repl--set-buffer-background 30)
        ;; Should have remapped both default and fringe
        (should (= (length remapped-faces) 2))
        (should (assq 'default remapped-faces))
        (should (assq 'fringe remapped-faces))))))

(ert-deftest claude-repl-test-set-buffer-background-correct-hex ()
  "set-buffer-background should pass the correct hex color."
  (claude-repl-test--with-temp-buffer " *test-bg-hex*"
    (let ((hex-used nil))
      (cl-letf (((symbol-function 'face-remap-add-relative)
                 (lambda (_face &rest props)
                   (setq hex-used (plist-get props :background)))))
        (claude-repl--set-buffer-background 15)
        (should (equal hex-used "#0f0f0f"))))))

(ert-deftest claude-repl-test-set-buffer-background-different-levels ()
  "Different grey levels should produce different hex colors."
  (let ((hex-a nil) (hex-b nil))
    (claude-repl-test--with-temp-buffer " *test-bg-diff-a*"
      (cl-letf (((symbol-function 'face-remap-add-relative)
                 (lambda (_face &rest props)
                   (setq hex-a (plist-get props :background)))))
        (claude-repl--set-buffer-background 15)))
    (claude-repl-test--with-temp-buffer " *test-bg-diff-b*"
      (cl-letf (((symbol-function 'face-remap-add-relative)
                 (lambda (_face &rest props)
                   (setq hex-b (plist-get props :background)))))
        (claude-repl--set-buffer-background 30)))
    (should-not (equal hex-a hex-b))))

;;;; ---- Tests: default-background-request-p ----

(ert-deftest claude-repl-test-default-bg-request-index-neg1-no-props ()
  "default-background-request-p with index -1 and no special props should be non-nil."
  (should (claude-repl--default-background-request-p -1 nil)))

(ert-deftest claude-repl-test-default-bg-request-index-neg1-foreground ()
  "default-background-request-p with :foreground should return nil."
  (should-not (claude-repl--default-background-request-p -1 '(:foreground t))))

(ert-deftest claude-repl-test-default-bg-request-index-neg1-inverse-video ()
  "default-background-request-p with :inverse-video should return nil."
  (should-not (claude-repl--default-background-request-p -1 '(:inverse-video t))))

(ert-deftest claude-repl-test-default-bg-request-index-0 ()
  "default-background-request-p with index 0 should return nil."
  (should-not (claude-repl--default-background-request-p 0 nil)))

(ert-deftest claude-repl-test-default-bg-request-index-neg1-both ()
  "default-background-request-p with both :foreground and :inverse-video should return nil."
  (should-not (claude-repl--default-background-request-p -1 '(:foreground t :inverse-video t))))

(ert-deftest claude-repl-test-default-bg-request-index-neg1-empty-args ()
  "default-background-request-p with index -1 and empty args list should return non-nil."
  (should (claude-repl--default-background-request-p -1 '())))

;;;; ---- Tests: vterm-color-advice ----

(ert-deftest claude-repl-test-vterm-color-advice-claude-default-bg ()
  "vterm-color-advice in a claude buffer with default bg request should return dark hex."
  (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
    (let ((result (claude-repl--vterm-color-advice
                   (lambda (_idx &rest _args) "#ffffff")
                   -1)))
      (should (equal result (claude-repl--grey-hex claude-repl--vterm-background-grey))))))

(ert-deftest claude-repl-test-vterm-color-advice-claude-non-default ()
  "vterm-color-advice in a claude buffer with non-default request should pass through."
  (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
    (let ((result (claude-repl--vterm-color-advice
                   (lambda (_idx &rest _args) "#ff0000")
                   0)))
      (should (equal result "#ff0000")))))

(ert-deftest claude-repl-test-vterm-color-advice-non-claude ()
  "vterm-color-advice in a non-claude buffer should pass through regardless."
  (claude-repl-test--with-temp-buffer "*scratch*"
    (let ((result (claude-repl--vterm-color-advice
                   (lambda (_idx &rest _args) "#ff0000")
                   -1)))
      (should (equal result "#ff0000")))))

(ert-deftest claude-repl-test-vterm-color-advice-calls-original ()
  "vterm-color-advice should call the original function."
  (let ((original-called nil))
    (claude-repl-test--with-temp-buffer "*scratch*"
      (claude-repl--vterm-color-advice
       (lambda (_idx &rest _args)
         (setq original-called t)
         "#000000")
       0)
      (should original-called))))

(provide 'test-overlay)

;;; test-overlay.el ends here
