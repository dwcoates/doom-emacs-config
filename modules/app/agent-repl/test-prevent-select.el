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

(defmacro agent-repl-prevent-select-test--with-buffers (bindings &rest body)
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

(ert-deftest agent-repl-prevent-select-test-skip-webview-panel ()
  "Predicate returns non-nil for the *agent-frontend-WS* webview buffer."
  (agent-repl-prevent-select-test--with-buffers
      ((buf "*agent-frontend-myws*"))
    (should (agent-repl--prev-buffer-skip-agent-panel nil buf nil))))

(ert-deftest agent-repl-prevent-select-test-skip-input-panel ()
  "Predicate returns non-nil for the *agent-panel-input-WS* input buffer."
  (agent-repl-prevent-select-test--with-buffers
      ((buf "*agent-panel-input-myws*"))
    (should (agent-repl--prev-buffer-skip-agent-panel nil buf nil))))

(ert-deftest agent-repl-prevent-select-test-do-not-skip-ordinary-buffer ()
  "Predicate returns nil for an ordinary user buffer."
  (agent-repl-prevent-select-test--with-buffers
      ((buf "*scratch-test-ordinary*"))
    (should-not (agent-repl--prev-buffer-skip-agent-panel nil buf nil))))

(ert-deftest agent-repl-prevent-select-test-do-not-skip-utility-buffer ()
  "Predicate returns nil for a non-panel agent-repl utility buffer.
The skip rule is scoped to panel buffers — utility scratch buffers
like *agent-repl-dump* should still be reachable via cycling."
  (agent-repl-prevent-select-test--with-buffers
      ((buf "*agent-repl-dump*"))
    (should-not (agent-repl--prev-buffer-skip-agent-panel nil buf nil))))

(ert-deftest agent-repl-prevent-select-test-dead-buffer-not-skipped ()
  "Predicate returns nil for a killed buffer — guards against acting on
a dead reference if Emacs ever hands one to the skip function.  The name
matches `agent-repl--input-buffer-re' so a live buffer of this name WOULD
be skipped — liveness alone must be what defeats it here."
  (let ((buf (get-buffer-create "*agent-panel-input-dead*")))
    (kill-buffer buf)
    (should-not (agent-repl--prev-buffer-skip-agent-panel nil buf nil))))

;;;; ---- Variable installation ----

(ert-deftest agent-repl-prevent-select-test-installed-on-skip-var ()
  "Loading the module installs the predicate on `switch-to-prev-buffer-skip'."
  (should (eq switch-to-prev-buffer-skip
              #'agent-repl--prev-buffer-skip-agent-panel)))

;;;; ---- Integration with switch-to-prev-buffer / kill-buffer ----

(ert-deftest agent-repl-prevent-select-test-kill-buffer-skips-agent-panel ()
  "Killing the buffer shown in a window picks a non-agent replacement.
Seeds the window's prev-buffer history with a panel buffer above a
plain buffer, kills the current buffer, and asserts the window did not
land on the panel."
  (agent-repl-prevent-select-test--with-buffers
      ((victim "*prevent-select-victim*")
       (panel  "*agent-frontend-skipme*")
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

(ert-deftest agent-repl-prevent-select-test-previous-buffer-skips-agent-panel ()
  "`previous-buffer' walks past a panel buffer in the window history."
  (agent-repl-prevent-select-test--with-buffers
      ((plain "*prevent-select-prev-plain*")
       (panel "*agent-frontend-prev-skipme*")
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
