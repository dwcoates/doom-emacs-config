;;; test-titlebar.el --- Tests for agent-repl's unfocused title bar -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for titlebar.el -- the title bar of every Emacs window turns orange
;; while Emacs is unfocused, and back to the theme's color on a focus gain.
;; The paint runs against the batch session's own frame, which holds frame
;; parameters and per-frame faces like any other; only which frames count
;; and whether Emacs is focused are stubbed.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(defmacro agent-repl-test-titlebar--with-frame (focused &rest body)
  "Run BODY against the batch frame, bound as `frame', with Emacs FOCUSED.
`logs' collects every line the canonical log helper receives.  The
frame\='s paint is undone afterwards, so no case leaks into another."
  (declare (indent 1))
  `(let ((frame (selected-frame))
         (logs nil))
     (cl-letf (((symbol-function 'agent-repl--titlebar-frames) (lambda () (list frame)))
               ((symbol-function 'agent-repl--emacs-focused-p) (lambda (&optional _ws) ,focused))
               ((symbol-function 'agent-repl--log) (agent-repl-test--record-into logs)))
       (unwind-protect
           (progn ,@body)
         (agent-repl--titlebar-unpaint frame)))))

;;;; ---- An unfocused Emacs ----

(ert-deftest agent-repl-test-titlebar-unfocused-turns-the-frame-orange ()
  "An unfocused Emacs sets the frame\='s background, which the title bar shows, orange."
  (agent-repl-test-titlebar--with-frame nil
    ;; Act
    (agent-repl-titlebar-sync)
    ;; Assert
    (should (equal (frame-parameter frame 'background-color) agent-repl-titlebar-unfocused-color))))

(ert-deftest agent-repl-test-titlebar-unfocused-keeps-the-border-its-color ()
  "The internal border is pinned to the frame\='s color from before the paint."
  (agent-repl-test-titlebar--with-frame nil
    ;; Arrange
    (let ((before (face-background 'default frame)))
      ;; Act
      (agent-repl-titlebar-sync)
      ;; Assert
      (should (equal (face-attribute 'internal-border :background frame) before)))))

(ert-deftest agent-repl-test-titlebar-unfocused-marks-the-frame-painted ()
  "A painted frame says so in its frame parameter."
  (agent-repl-test-titlebar--with-frame nil
    ;; Act
    (agent-repl-titlebar-sync)
    ;; Assert
    (should (frame-parameter frame agent-repl--titlebar-painted))))

(ert-deftest agent-repl-test-titlebar-a-second-unfocused-sync-keeps-the-first-border ()
  "Painting a painted frame again leaves its border on the color from before the first paint."
  (agent-repl-test-titlebar--with-frame nil
    ;; Arrange
    (let ((before (face-background 'default frame)))
      (agent-repl-titlebar-sync)
      ;; Act
      (agent-repl-titlebar-sync)
      ;; Assert
      (should (equal (face-attribute 'internal-border :background frame) before)))))

(ert-deftest agent-repl-test-titlebar-paint-is-logged ()
  "A paint records the frame, its color from before and the orange."
  (agent-repl-test-titlebar--with-frame nil
    ;; Arrange
    (let ((before (face-background 'default frame)))
      ;; Act
      (agent-repl-titlebar-sync)
      ;; Assert
      (should (member (format "elisp.titlebar.paint frame=%S background=%s color=%s"
                              frame before agent-repl-titlebar-unfocused-color)
                      logs)))))

(ert-deftest agent-repl-test-titlebar-sync-is-logged ()
  "Every sync records the focus it read and how many frames it painted for."
  (agent-repl-test-titlebar--with-frame nil
    ;; Act
    (agent-repl-titlebar-sync)
    ;; Assert
    (should (member "elisp.titlebar.sync focused=nil frames=1" logs))))

;;;; ---- A focused Emacs ----

(ert-deftest agent-repl-test-titlebar-focus-gain-returns-the-theme-color ()
  "A focus gain recalculates the frame\='s background from the theme."
  (agent-repl-test-titlebar--with-frame nil
    ;; Arrange
    (let ((before (frame-parameter frame 'background-color)))
      (agent-repl-titlebar-sync)
      ;; Act
      (cl-letf (((symbol-function 'agent-repl--emacs-focused-p) (lambda (&optional _ws) t)))
        (agent-repl-titlebar-sync))
      ;; Assert
      (should (equal (frame-parameter frame 'background-color) before)))))

(ert-deftest agent-repl-test-titlebar-focus-gain-unpins-the-border ()
  "A focus gain leaves the internal border unspecified again."
  (agent-repl-test-titlebar--with-frame nil
    ;; Arrange
    (agent-repl-titlebar-sync)
    ;; Act
    (cl-letf (((symbol-function 'agent-repl--emacs-focused-p) (lambda (&optional _ws) t)))
      (agent-repl-titlebar-sync))
    ;; Assert
    (should (eq (face-attribute 'internal-border :background frame) 'unspecified))))

(ert-deftest agent-repl-test-titlebar-focus-gain-clears-the-mark ()
  "An unpainted frame no longer says it is painted."
  (agent-repl-test-titlebar--with-frame nil
    ;; Arrange
    (agent-repl-titlebar-sync)
    ;; Act
    (cl-letf (((symbol-function 'agent-repl--emacs-focused-p) (lambda (&optional _ws) t)))
      (agent-repl-titlebar-sync))
    ;; Assert
    (should-not (frame-parameter frame agent-repl--titlebar-painted))))

(ert-deftest agent-repl-test-titlebar-focus-gain-is-logged ()
  "An unpaint records the frame."
  (agent-repl-test-titlebar--with-frame nil
    ;; Arrange
    (agent-repl-titlebar-sync)
    ;; Act
    (cl-letf (((symbol-function 'agent-repl--emacs-focused-p) (lambda (&optional _ws) t)))
      (agent-repl-titlebar-sync))
    ;; Assert
    (should (member (format "elisp.titlebar.unpaint frame=%S" frame) logs))))

(ert-deftest agent-repl-test-titlebar-focused-leaves-an-unpainted-frame-alone ()
  "A focused sync of a frame never painted recalculates nothing."
  (agent-repl-test-titlebar--with-frame t
    ;; Arrange
    (let ((recalculated nil))
      (cl-letf (((symbol-function 'face-spec-recalc) (lambda (&rest args) (push args recalculated))))
        ;; Act
        (agent-repl-titlebar-sync))
      ;; Assert
      (should (null recalculated)))))

(ert-deftest agent-repl-test-titlebar-unknown-focus-counts-as-focused ()
  "A focus `agent-repl--emacs-focused-p' reports as `unknown' paints nothing."
  (agent-repl-test-titlebar--with-frame 'unknown
    ;; Act
    (agent-repl-titlebar-sync)
    ;; Assert
    (should-not (frame-parameter frame agent-repl--titlebar-painted))))

;;;; ---- A theme load ----

(ert-deftest agent-repl-test-titlebar-theme-load-repins-the-border-to-the-new-theme ()
  "A theme loaded while unfocused leaves the border on the new theme\='s color."
  (agent-repl-test-titlebar--with-frame nil
    ;; Arrange
    (agent-repl-titlebar-sync)
    (let ((recalc (symbol-function 'face-spec-recalc)))
      (cl-letf (((symbol-function 'face-spec-recalc)
                 (lambda (face frame)
                   (funcall recalc face frame)
                   ;; The new theme's default background.
                   (set-frame-parameter frame 'background-color "#222222"))))
        ;; Act
        (agent-repl--titlebar-on-theme 'new-theme)))
    ;; Assert
    (should (equal (face-attribute 'internal-border :background frame) "#222222"))))

(ert-deftest agent-repl-test-titlebar-theme-load-keeps-the-frame-orange ()
  "A theme loaded while unfocused leaves the title bar orange."
  (agent-repl-test-titlebar--with-frame nil
    ;; Arrange
    (agent-repl-titlebar-sync)
    ;; Act
    (agent-repl--titlebar-on-theme 'new-theme)
    ;; Assert
    (should (equal (frame-parameter frame 'background-color) agent-repl-titlebar-unfocused-color))))

;;;; ---- Which frames ----

(ert-deftest agent-repl-test-titlebar-frames-are-top-level-macos-frames ()
  "Only a top-level macOS frame has a title bar Emacs can color."
  ;; Arrange
  (let ((kinds '((top . ns) (child . ns) (tty . t) (x . x)))
        (parents '((child . top))))
    (cl-letf (((symbol-function 'frame-list) (lambda () (mapcar #'car kinds)))
              ((symbol-function 'framep) (lambda (frame) (alist-get frame kinds)))
              ((symbol-function 'frame-parameter)
               (lambda (frame parameter)
                 (and (eq parameter 'parent-frame) (alist-get frame parents)))))
      ;; Act / Assert
      (should (equal (agent-repl--titlebar-frames) '(top))))))

;;;; ---- Wiring ----

(ert-deftest agent-repl-test-titlebar-orange-is-the-render-colors-orange ()
  "The title bar\='s orange is the one the tab dot and the webapp draw."
  (should (equal agent-repl-titlebar-unfocused-color agent-repl--color-compaction-orange)))

(ert-deftest agent-repl-test-titlebar-rides-the-focus-settled-hook ()
  "The title bar follows every settled focus change."
  (should (memq #'agent-repl-titlebar-sync agent-repl-focus-settled-hook)))

(ert-deftest agent-repl-test-titlebar-paints-every-new-frame ()
  "A frame made while Emacs is unfocused is painted at once."
  (should (memq #'agent-repl-titlebar-sync after-make-frame-functions)))

(ert-deftest agent-repl-test-titlebar-repaints-on-every-theme-load ()
  "A theme load repaints the title bars."
  (should (memq #'agent-repl--titlebar-on-theme enable-theme-functions)))

;;; test-titlebar.el ends here
