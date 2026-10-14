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
