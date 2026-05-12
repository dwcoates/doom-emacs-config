;;; test-prevent-select.el --- ERT tests for prevent-select.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `prevent-select.el' — verifies that the skip predicate
;; identifies Claude panel buffers, and that the integration with
;; `switch-to-prev-buffer-skip' actually causes Emacs's interactive
;; cycling primitives to skip them.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-prevent-select.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Helpers ----

(defmacro claude-repl-prevent-select-test--with-buffers (bindings &rest body)
  "Create the buffers in BINDINGS, run BODY, kill them on cleanup.
BINDINGS is a list of (SYM NAME) pairs where NAME is the buffer name to
create with `get-buffer-create'.  Each SYM is bound to the buffer object
inside BODY."
  (declare (indent 1))
  (let ((let-clauses (mapcar (lambda (b)
                               `(,(car b) (get-buffer-create ,(cadr b))))
                             bindings))
        (syms (mapcar #'car bindings)))
    `(let ,let-clauses
       (unwind-protect
           (progn ,@body)
         ,@(mapcar (lambda (sym)
                     `(when (buffer-live-p ,sym) (kill-buffer ,sym)))
                   syms)))))

;;;; ---- Skip predicate ----

(ert-deftest claude-repl-prevent-select-test-skip-vterm-panel ()
  "Predicate returns non-nil for the *claude-panel-WS* vterm buffer."
  (claude-repl-prevent-select-test--with-buffers
      ((buf "*claude-panel-myws*"))
    (should (claude-repl--prev-buffer-skip-claude-panel nil buf nil))))

(ert-deftest claude-repl-prevent-select-test-skip-input-panel ()
  "Predicate returns non-nil for the *claude-panel-input-WS* input buffer."
  (claude-repl-prevent-select-test--with-buffers
      ((buf "*claude-panel-input-myws*"))
    (should (claude-repl--prev-buffer-skip-claude-panel nil buf nil))))

(ert-deftest claude-repl-prevent-select-test-do-not-skip-ordinary-buffer ()
  "Predicate returns nil for an ordinary user buffer."
  (claude-repl-prevent-select-test--with-buffers
      ((buf "*scratch-test-ordinary*"))
    (should-not (claude-repl--prev-buffer-skip-claude-panel nil buf nil))))

(ert-deftest claude-repl-prevent-select-test-do-not-skip-utility-buffer ()
  "Predicate returns nil for a non-panel claude-repl utility buffer.
The skip rule is scoped to panel buffers — utility scratch buffers
like *claude-repl-dump* should still be reachable via cycling."
  (claude-repl-prevent-select-test--with-buffers
      ((buf "*claude-repl-dump*"))
    (should-not (claude-repl--prev-buffer-skip-claude-panel nil buf nil))))

(ert-deftest claude-repl-prevent-select-test-dead-buffer-not-skipped ()
  "Predicate returns nil for a killed buffer — guards against acting on
a dead reference if Emacs ever hands one to the skip function."
  (let ((buf (get-buffer-create "*claude-panel-dead*")))
    (kill-buffer buf)
    (should-not (claude-repl--prev-buffer-skip-claude-panel nil buf nil))))

;;;; ---- Variable installation ----

(ert-deftest claude-repl-prevent-select-test-installed-on-skip-var ()
  "Loading the module installs the predicate on `switch-to-prev-buffer-skip'."
  (should (eq switch-to-prev-buffer-skip
              #'claude-repl--prev-buffer-skip-claude-panel)))

;;;; ---- Integration with switch-to-prev-buffer / kill-buffer ----

(ert-deftest claude-repl-prevent-select-test-kill-buffer-skips-claude-panel ()
  "Killing the buffer shown in a window picks a non-Claude replacement.
Seeds the window's prev-buffer history with a panel buffer above a
plain buffer, kills the current buffer, and asserts the window did not
land on the panel."
  (claude-repl-prevent-select-test--with-buffers
      ((victim "*prevent-select-victim*")
       (panel  "*claude-panel-skipme*")
       (plain  "*prevent-select-plain*"))
    (let ((wconf (current-window-configuration)))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer plain)
            (switch-to-buffer panel)
            (switch-to-buffer victim)
            (kill-buffer victim)
            (should-not (eq (window-buffer (selected-window)) panel)))
        (set-window-configuration wconf)))))

(ert-deftest claude-repl-prevent-select-test-previous-buffer-skips-claude-panel ()
  "`previous-buffer' walks past a panel buffer in the window history."
  (claude-repl-prevent-select-test--with-buffers
      ((plain "*prevent-select-prev-plain*")
       (panel "*claude-panel-prev-skipme*")
       (curr  "*prevent-select-prev-curr*"))
    (let ((wconf (current-window-configuration)))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer plain)
            (switch-to-buffer panel)
            (switch-to-buffer curr)
            (previous-buffer)
            (should-not (eq (window-buffer (selected-window)) panel)))
        (set-window-configuration wconf)))))

(provide 'test-prevent-select)

;;; test-prevent-select.el ends here
