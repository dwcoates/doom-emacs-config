;;; config/personal-bindings/test-personal-bindings-config.el -*- lexical-binding: t; -*-
;;
;; Tests for modules/config/personal-bindings/config.el.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-personal-bindings-config.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; The config calls Doom's `map!' macro, which a bare `-Q' batch Emacs lacks,
;; so it is stubbed.  Evil is not loaded either: `evil-define-key*' is stubbed
;; to record its calls, and `(provide 'evil)' fires the config's
;; `with-eval-after-load' the way loading the real evil would.

(require 'ert)
(require 'cl-lib)

(unless (fboundp 'map!)
  (defmacro map! (&rest _) nil))

(defvar test-personal-bindings--evil-calls nil
  "Each `evil-define-key*' call as (STATE KEYMAP KEY DEF), newest first.")

(defun evil-define-key* (state keymap key def)
  "Record STATE, KEYMAP, KEY and DEF instead of binding them."
  (push (list state keymap key def) test-personal-bindings--evil-calls))

(load (expand-file-name "config.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest test-personal-bindings-minibuffer-maps-bind-c-k-to-kill ()
  "Every plain minibuffer map binds `C-k' to `kill-line'."
  ;; Arrange
  (let ((maps +dwc/minibuffer-kill-line-maps))
    ;; Act
    (let ((bindings (mapcar (lambda (map-symbol)
                              (lookup-key (symbol-value map-symbol) (kbd "C-k")))
                            maps)))
      ;; Assert
      (should (equal bindings (make-list (length maps) #'kill-line))))))

(ert-deftest test-personal-bindings-minibuffer-maps-cover-doom-plain-maps ()
  "The maps rebound are the plain maps Doom's evil bindings give `previous-line'."
  ;; Arrange
  (let ((doom-plain-maps '(minibuffer-local-map
                           minibuffer-local-ns-map
                           minibuffer-local-completion-map
                           minibuffer-local-must-match-map
                           minibuffer-local-isearch-map
                           read-expression-map)))
    ;; Act / Assert
    (should (equal +dwc/minibuffer-kill-line-maps doom-plain-maps))))

(ert-deftest test-personal-bindings-c-k-kills-to-end-of-line-in-minibuffer-map ()
  "`C-k' in a buffer using `minibuffer-local-map' kills from point to the end."
  ;; Arrange
  (with-temp-buffer
    (use-local-map minibuffer-local-map)
    (insert "keep this|drop this")
    (goto-char (point-min))
    (search-forward "|")
    (let ((kill-ring nil))
      ;; Act
      (call-interactively (key-binding (kbd "C-k")))
      ;; Assert
      (should (equal (buffer-string) "keep this|"))
      (should (equal (car kill-ring) "drop this")))))

(ert-deftest test-personal-bindings-evil-insert-state-binds-c-k-to-kill ()
  "Once evil loads, every plain minibuffer map's insert state binds `C-k'."
  ;; Arrange
  (setq test-personal-bindings--evil-calls nil)
  ;; Act
  (provide 'evil)
  ;; Assert
  (should (equal (reverse test-personal-bindings--evil-calls)
                 (mapcar (lambda (map-symbol)
                           (list 'insert (symbol-value map-symbol)
                                 (kbd "C-k") #'kill-line))
                         +dwc/minibuffer-kill-line-maps))))

(ert-deftest test-personal-bindings-non-keymap-fails-hard ()
  "A listed map that is not a keymap is an invariant violation, not skipped."
  ;; Arrange
  (defvar test-personal-bindings--not-a-keymap 'nope)
  (let ((+dwc/minibuffer-kill-line-maps '(test-personal-bindings--not-a-keymap)))
    ;; Act / Assert
    (should (equal (should-error (+dwc/bind-minibuffer-kill-line))
                   '(error "Minibuffer keymap test-personal-bindings--not-a-keymap is not a keymap")))))

(defmacro test-personal-bindings--with-vterm-toggle (shown &rest body)
  "Run BODY with the vterm toggle's collaborators stubbed.
The stubbed `+vterm/toggle' returns a probe buffer, displayed in the
selected window when SHOWN is non-nil.  BODY sees `calls', the stub calls
in order as (NAME . ARGS)."
  (declare (indent 1))
  `(let ((calls nil)
         (probe (generate-new-buffer " *probe-vterm*"))
         (original (window-buffer (selected-window))))
     (unwind-protect
         (cl-letf (((symbol-function '+vterm/toggle)
                    (lambda (arg)
                      (push (list '+vterm/toggle arg) calls)
                      (when ,shown (set-window-buffer (selected-window) probe))
                      probe))
                   ((symbol-function 'vterm-reset-cursor-point)
                    (lambda () (push (list 'vterm-reset-cursor-point (current-buffer)) calls)))
                   ((symbol-function 'evil-insert-state)
                    (lambda () (push (list 'evil-insert-state (current-buffer)) calls)))
                   ((symbol-function '+dwc/toggle-window-fullscreen)
                    (lambda () (push (list '+dwc/toggle-window-fullscreen) calls))))
           ,@body)
       (set-window-buffer (selected-window) original)
       (kill-buffer probe))))

(ert-deftest test-personal-bindings-vterm-toggle-shown-enters-insert-at-cursor ()
  "A toggle that shows the popup resets point to the cursor, then enters insert."
  ;; Arrange
  (test-personal-bindings--with-vterm-toggle t
    ;; Act
    (+dwc/vterm-toggle nil)
    ;; Assert
    (should (equal (reverse calls)
                   (list (list '+vterm/toggle nil)
                         (list 'vterm-reset-cursor-point probe)
                         (list 'evil-insert-state probe))))))

(ert-deftest test-personal-bindings-vterm-toggle-hidden-leaves-state-alone ()
  "A toggle that hides the popup neither moves point nor changes evil state."
  ;; Arrange
  (test-personal-bindings--with-vterm-toggle nil
    ;; Act
    (+dwc/vterm-toggle nil)
    ;; Assert
    (should (equal calls (list (list '+vterm/toggle nil))))))

(ert-deftest test-personal-bindings-vterm-toggle-forwards-prefix-arg ()
  "The prefix argument reaches `+vterm/toggle' unchanged."
  ;; Arrange
  (test-personal-bindings--with-vterm-toggle nil
    ;; Act
    (+dwc/vterm-toggle '(4))
    ;; Assert
    (should (equal calls (list (list '+vterm/toggle '(4)))))))

(ert-deftest test-personal-bindings-vterm-toggle-sole-vterm-window-unfullscreens ()
  "Toggling from a sole vterm window un-fullscreens it before toggling."
  ;; Arrange
  (test-personal-bindings--with-vterm-toggle nil
    (with-temp-buffer
      (rename-buffer "*doom:vterm-popup:probe*" t)
      (set-window-buffer (selected-window) (current-buffer))
      (delete-other-windows)
      ;; Act
      (+dwc/vterm-toggle nil))
    ;; Assert
    (should (equal (reverse calls)
                   (list (list '+dwc/toggle-window-fullscreen)
                         (list '+vterm/toggle nil))))))

(ert-deftest test-personal-bindings-vterm-toggle-other-buffer-keeps-layout ()
  "Toggling from a non-vterm buffer does not un-fullscreen anything."
  ;; Arrange
  (test-personal-bindings--with-vterm-toggle nil
    (with-temp-buffer
      (rename-buffer "*scratch-probe*" t)
      (set-window-buffer (selected-window) (current-buffer))
      (delete-other-windows)
      ;; Act
      (+dwc/vterm-toggle nil))
    ;; Assert
    (should (equal calls (list (list '+vterm/toggle nil))))))

(ert-deftest test-personal-bindings-vterm-toggle-returns-vterm-buffer ()
  "The toggle returns the vterm buffer `+vterm/toggle' returned."
  ;; Arrange
  (test-personal-bindings--with-vterm-toggle t
    ;; Act
    (let ((result (+dwc/vterm-toggle nil)))
      ;; Assert
      (should (eq result probe)))))
