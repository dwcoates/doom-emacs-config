;;; config/evil-search/test-evil-search-config.el -*- lexical-binding: t; -*-
;;
;; Tests for modules/config/evil-search/config.el.
;;
;; Run with the real evil (and its goto-chg dependency) on the load path:
;;   emacs -batch -Q -L <straight-build>/goto-chg -L <straight-build>/evil \
;;     -l ert -l test-evil-search-config.el -f ert-run-tests-batch-and-exit
;;
;; The current-match face reads agent-repl's purple from its status module.
;; That module needs the rest of agent-repl to load, so its color lookup is
;; stubbed here to record the color asked for, and `agent-repl-status' is
;; provided to fire the config's `with-eval-after-load'.  The lookup's own
;; value is covered by agent-repl's status tests.

(require 'ert)
(require 'cl-lib)

(defconst test-evil-search--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar test-evil-search--colors-asked nil
  "Each color `agent-repl-status--color-value' was asked for, newest first.")

(defconst test-evil-search--purple "#123456"
  "The value the stubbed status module gives every color.")

(defun agent-repl-status--color-value (color)
  "Record COLOR and return `test-evil-search--purple'."
  (push color test-evil-search--colors-asked)
  test-evil-search--purple)

(load (expand-file-name "config.el" test-evil-search--dir) nil t)
(require 'evil)
(provide 'agent-repl-status)

(defmacro test-evil-search--with-buffer (text &rest body)
  "Run BODY in a normal-state buffer holding TEXT, point at its start."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (goto-char (point-min))
     (evil-local-mode 1)
     (evil-normal-state)
     (let ((evil-ex-search-pattern nil)
           (evil-ex-search-match-beg nil)
           (evil-ex-search-match-end nil))
       (unwind-protect (progn ,@body)
         (evil-ex-delete-hl 'evil-ex-search)))))

(defun test-evil-search--start (string)
  "Run `evil-ex-start-search' forward for STRING as if typed at `/' and RET."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) string)))
    (evil-ex-start-search 'forward 1)))

(defun test-evil-search--current-bounds ()
  "Return the current-match overlay's (BEG . END), or nil when it is gone."
  (when (and (overlayp +dwc/evil-search-current-overlay)
             (overlay-buffer +dwc/evil-search-current-overlay))
    (cons (overlay-start +dwc/evil-search-current-overlay)
          (overlay-end +dwc/evil-search-current-overlay))))

(ert-deftest test-evil-search-ret-marks-the-landed-match ()
  "`RET' at the `/' prompt leaves the landed match marked."
  ;; Arrange
  (test-evil-search--with-buffer "foo bar foo baz foo"
    ;; Act
    (test-evil-search--start "foo")
    ;; Assert
    (should (equal (test-evil-search--current-bounds) '(9 . 12)))))

(ert-deftest test-evil-search-n-moves-the-mark-to-the-next-match ()
  "`n' moves the mark to the match it lands on."
  ;; Arrange
  (test-evil-search--with-buffer "foo bar foo baz foo"
    (test-evil-search--start "foo")
    ;; Act
    (evil-ex-search-next)
    ;; Assert
    (should (equal (test-evil-search--current-bounds) '(17 . 20)))))

(ert-deftest test-evil-search-N-moves-the-mark-to-the-previous-match ()
  "`N' moves the mark back to the match it lands on."
  ;; Arrange
  (test-evil-search--with-buffer "foo bar foo baz foo"
    (test-evil-search--start "foo")
    (evil-ex-search-next)
    ;; Act
    (evil-ex-search-previous)
    ;; Assert
    (should (equal (test-evil-search--current-bounds) '(9 . 12)))))

(ert-deftest test-evil-search-mark-is-one-overlay ()
  "Stepping through matches reuses one overlay rather than leaving a trail."
  ;; Arrange
  (test-evil-search--with-buffer "foo bar foo baz foo"
    (test-evil-search--start "foo")
    ;; Act
    (evil-ex-search-next)
    (evil-ex-search-next)
    ;; Assert
    (should (= 1 (cl-count-if (lambda (o) (eq (overlay-get o 'face) 'evil-ex-search))
                              (overlays-in (point-min) (point-max)))))))

(ert-deftest test-evil-search-mark-wears-the-search-face-above-evil ()
  "The mark wears `evil-ex-search' above Evil's own highlight overlays."
  ;; Arrange
  (test-evil-search--with-buffer "foo bar foo"
    ;; Act
    (test-evil-search--start "foo")
    ;; Assert
    (should (equal (list (overlay-get +dwc/evil-search-current-overlay 'face)
                         (overlay-get +dwc/evil-search-current-overlay 'priority))
                   '(evil-ex-search 1001)))))

(ert-deftest test-evil-search-nohighlight-drops-the-mark ()
  "`evil-ex-nohighlight' (Doom's ESC) drops the mark with the highlight."
  ;; Arrange
  (test-evil-search--with-buffer "foo bar foo"
    (test-evil-search--start "foo")
    ;; Act
    (evil-ex-nohighlight)
    ;; Assert
    (should (null (test-evil-search--current-bounds)))))

(ert-deftest test-evil-search-other-highlight-keeps-the-mark ()
  "Deleting a highlight other than the search's leaves the mark alone."
  ;; Arrange
  (test-evil-search--with-buffer "foo bar foo"
    (test-evil-search--start "foo")
    ;; Act
    (+dwc/evil-search-unmark-current 'evil-ex-substitute)
    ;; Assert
    (should (equal (test-evil-search--current-bounds) '(9 . 12)))))

(ert-deftest test-evil-search-failed-search-leaves-no-mark ()
  "A `/' search with no match signals and marks nothing."
  ;; Arrange
  (test-evil-search--with-buffer "foo bar foo"
    ;; Act
    (should-error (test-evil-search--start "qux") :type 'search-failed)
    ;; Assert
    (should (null (test-evil-search--current-bounds)))))

(ert-deftest test-evil-search-mark-without-match-bounds-signals ()
  "Marking with no recorded match bounds is a broken invariant and signals."
  ;; Arrange
  (test-evil-search--with-buffer "foo"
    ;; Act / Assert
    (should-error (+dwc/evil-search-mark-current) :type 'error)
    (should (null +dwc/evil-search-current-overlay))))

(ert-deftest test-evil-search-current-face-is-agent-repl-purple ()
  "The current match's background is the status module's purple."
  ;; Act / Assert
  (should (equal (list (face-attribute 'evil-ex-search :background)
                       test-evil-search--colors-asked)
                 (list test-evil-search--purple '("purple")))))

(ert-deftest test-evil-search-persistent-highlight-is-off ()
  "The all-matches highlight still goes away on `RET', as before."
  ;; Act / Assert
  (should (null evil-ex-search-persistent-highlight)))
