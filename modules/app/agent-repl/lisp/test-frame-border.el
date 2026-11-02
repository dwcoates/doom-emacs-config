;;; test-frame-border.el --- Tests for the one owner of a frame's border -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for frame-border.el -- a frame's internal border wears the ground
;; while the frame shows panels, an orange title bar's pinned color
;; otherwise, and no color of its own when neither holds.  The cases run
;; against the batch session's own frame, which holds frame parameters and
;; per-frame faces like any other.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(defconst agent-repl-test-frame-border--lisp-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The module's lisp directory, for the ownership scan below.")

(defmacro agent-repl-test-frame-border--with-frame (&rest body)
  "Run BODY against the batch frame, bound as `frame', holding neither state.
`logs' collects every line the canonical log helper receives.  Both
states and the border's color are cleared afterwards, so no case leaks
into another."
  (declare (indent 0))
  `(let ((frame (selected-frame))
         (logs nil)
         (agent-repl-input-background "#121212"))
     (cl-letf (((symbol-function 'agent-repl--log) (agent-repl-test--record-into logs)))
       (unwind-protect
           (progn ,@body)
         (set-frame-parameter frame agent-repl--frame-border-panels nil)
         (set-frame-parameter frame agent-repl--frame-border-pinned nil)
         (set-face-attribute 'internal-border frame :background 'unspecified)))))

(defun agent-repl-test-frame-border--color (frame)
  "Return FRAME's internal border color."
  (face-attribute 'internal-border :background frame))

;;;; ---- The color the two states decide ----

(ert-deftest agent-repl-test-frame-border-panels-wear-the-ground ()
  "A frame showing panels wears the ground round its edge."
  (agent-repl-test-frame-border--with-frame
    ;; Act
    (agent-repl-frame-border-set-panels frame t)
    ;; Assert
    (should (equal (agent-repl-test-frame-border--color frame) "#121212"))))

(ert-deftest agent-repl-test-frame-border-pinned-color-without-panels ()
  "A frame showing no panels, its title bar orange, keeps the color pinned before."
  (agent-repl-test-frame-border--with-frame
    ;; Act
    (agent-repl-frame-border-set-pinned frame "#1d1f21")
    ;; Assert
    (should (equal (agent-repl-test-frame-border--color frame) "#1d1f21"))))

(ert-deftest agent-repl-test-frame-border-panels-win-over-a-pinned-color ()
  "A frame showing panels wears the ground even while its title bar is orange."
  (agent-repl-test-frame-border--with-frame
    ;; Arrange
    (agent-repl-frame-border-set-pinned frame "#1d1f21")
    ;; Act
    (agent-repl-frame-border-set-panels frame t)
    ;; Assert
    (should (equal (agent-repl-test-frame-border--color frame) "#121212"))))

(ert-deftest agent-repl-test-frame-border-neither-state-has-no-color ()
  "A frame holding neither state has no border color of its own."
  (agent-repl-test-frame-border--with-frame
    ;; Arrange
    (agent-repl-frame-border-set-panels frame t)
    ;; Act
    (agent-repl-frame-border-set-panels frame nil)
    ;; Assert
    (should (eq (agent-repl-test-frame-border--color frame) 'unspecified))))

;;;; ---- No order of writes leaves it wrong ----

(ert-deftest agent-repl-test-frame-border-focus-gain-keeps-the-ground ()
  "A focus gain clearing the pinned color leaves a frame showing panels on the ground."
  (agent-repl-test-frame-border--with-frame
    ;; Arrange
    (agent-repl-frame-border-set-pinned frame "#1d1f21")
    (agent-repl-frame-border-set-panels frame t)
    ;; Act
    (agent-repl-frame-border-set-pinned frame nil)
    ;; Assert
    (should (equal (agent-repl-test-frame-border--color frame) "#121212"))))

(ert-deftest agent-repl-test-frame-border-panels-gone-return-the-pinned-color ()
  "Panels leaving a frame whose title bar is still orange return the pinned color."
  (agent-repl-test-frame-border--with-frame
    ;; Arrange
    (agent-repl-frame-border-set-panels frame t)
    (agent-repl-frame-border-set-pinned frame "#1d1f21")
    ;; Act
    (agent-repl-frame-border-set-panels frame nil)
    ;; Assert
    (should (equal (agent-repl-test-frame-border--color frame) "#1d1f21"))))

;;;; ---- Recording a state ----

(ert-deftest agent-repl-test-frame-border-set-panels-records-the-state ()
  "Whether the frame shows panels is recorded as t or nil."
  (agent-repl-test-frame-border--with-frame
    ;; Act
    (agent-repl-frame-border-set-panels frame 'some-window)
    ;; Assert
    (should (eq (frame-parameter frame agent-repl--frame-border-panels) t))))

(ert-deftest agent-repl-test-frame-border-unchanged-state-syncs-nothing ()
  "Recording the state a frame already holds sets no face and logs nothing."
  (agent-repl-test-frame-border--with-frame
    ;; Arrange
    (agent-repl-frame-border-set-panels frame t)
    (setq logs nil)
    ;; Act
    (agent-repl-frame-border-set-panels frame t)
    (agent-repl-frame-border-set-pinned frame nil)
    ;; Assert
    (should-not logs)))

(ert-deftest agent-repl-test-frame-border-sync-is-logged ()
  "A sync is recorded with the frame, the color and both states."
  (agent-repl-test-frame-border--with-frame
    ;; Act
    (agent-repl-frame-border-set-pinned frame "#1d1f21")
    ;; Assert
    (should (equal logs (list (format "elisp.frame-border.sync frame=%S color=\"#1d1f21\" panels=nil pinned=\"#1d1f21\""
                                      frame))))))

;;;; ---- A new ground, a theme, a reload ----

(ert-deftest agent-repl-test-frame-border-sync-all-follows-a-new-ground ()
  "Syncing every frame repaints a frame showing panels the current ground."
  (agent-repl-test-frame-border--with-frame
    ;; Arrange
    (agent-repl-frame-border-set-panels frame t)
    ;; Act
    (let ((agent-repl-input-background "#0a0a0a"))
      (agent-repl-frame-border-sync-all))
    ;; Assert
    (should (equal (agent-repl-test-frame-border--color frame) "#0a0a0a"))))

(ert-deftest agent-repl-test-frame-border-theme-load-restores-the-ground ()
  "A theme load that recalculated the border gives a frame showing panels the ground back."
  (agent-repl-test-frame-border--with-frame
    ;; Arrange
    (agent-repl-frame-border-set-panels frame t)
    (set-face-attribute 'internal-border frame :background "#abcdef")
    ;; Act
    (agent-repl--frame-border-on-theme 'some-theme)
    ;; Assert
    (should (equal (agent-repl-test-frame-border--color frame) "#121212"))))

(ert-deftest agent-repl-test-frame-border-hooks-are-installed ()
  "Every frame is synced on a new ground and on a theme load."
  ;; Act + Assert
  (should (memq #'agent-repl-frame-border-sync-all agent-repl-input-background-painters))
  (should (memq #'agent-repl--frame-border-on-theme enable-theme-functions)))

(ert-deftest agent-repl-test-frame-border-is-the-only-writer-of-the-border ()
  "No module but frame-border.el sets the `internal-border' face."
  ;; Arrange
  (let ((writers nil))
    ;; Act
    (dolist (file (directory-files agent-repl-test-frame-border--lisp-dir t "\\`[^.].*\\.el\\'"))
      (unless (or (string-prefix-p "test-" (file-name-nondirectory file))
                  (equal (file-name-nondirectory file) "frame-border.el"))
        (with-temp-buffer
          (insert-file-contents file)
          (when (re-search-forward "(set-face-attribute 'internal-border" nil t)
            (push (file-name-nondirectory file) writers)))))
    ;; Assert
    (should-not writers)))

(provide 'test-frame-border)
;;; test-frame-border.el ends here
