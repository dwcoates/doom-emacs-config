;;; test-no-throw-on-input.el --- no module runs code under `while-no-input' -*- lexical-binding: t; -*-

;;; Commentary:

;; `while-no-input' binds `throw-on-input', and under it any input event
;; Emacs stores THROWS at the next quit check.  One such check sits inside
;; `kbd_buffer_store_event' itself (its `while-no-input-ignore-events'
;; lookup), and xwidget events are stored from inside GTK (and Cocoa)
;; signal callbacks.  The throw then unwinds through a live GSignal
;; emission, GLib's emission list keeps naming the dead stack frame, and the
;; next `notify' signal anywhere in the process segfaults walking it.  The
;; e2e sandbox's Emacs died that way in most merge gates from 2026-10-25,
;; when bytecode.el's compile slices ran under `while-no-input'.
;;
;; So no module may use it, nor bind `throw-on-input' by hand: this suite
;; reads every `lisp/*.el' source (tests aside) form by form and refuses
;; either symbol anywhere in code.  Comments and docstrings are not code, so
;; explaining the rule stays allowed.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-no-throw-on-input.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

(defconst agent-repl-test-nti--forbidden '(while-no-input throw-on-input)
  "Symbols whose presence in a module's code makes stored input throw.")

(defun agent-repl-test-nti--symbols-in (form)
  "Every forbidden symbol FORM contains, at any depth, in order of appearance."
  (let (found)
    (cl-labels ((walk (x)
                  (cond ((memq x agent-repl-test-nti--forbidden) (push x found))
                        ((consp x) (walk (car x)) (walk (cdr x)))
                        ((and (vectorp x) (not (stringp x))) (mapc #'walk x)))))
      (walk form))
    (nreverse found)))

(defun agent-repl-test-nti--uses-in-file (file)
  "Every forbidden symbol FILE's code uses, as (FILE . SYMBOL) pairs."
  (mapcan (lambda (form)
            (mapcar (lambda (sym) (cons (file-name-nondirectory file) sym))
                    (agent-repl-test-nti--symbols-in form)))
          (agent-repl-test--file-forms file)))

(ert-deftest agent-repl-test-nti-no-module-uses-while-no-input ()
  "No module's code names `while-no-input' or `throw-on-input'."
  ;; Arrange
  (let ((files (agent-repl-test--module-sources)))
    ;; Act
    (let ((uses (mapcan #'agent-repl-test-nti--uses-in-file files)))
      ;; Assert
      (should (> (length files) 10))
      (should (equal uses nil)))))

(ert-deftest agent-repl-test-nti-scan-finds-a-use-in-code ()
  "The scan reports a `while-no-input' nested in a definition's body."
  (agent-repl-test--with-temp-dir dir "agent-repl-nti"
    ;; Arrange
    (let ((file (expand-file-name "probe.el" dir)))
      (with-temp-file file
        (insert "(defun probe () (let ((x 1)) (while-no-input (sit-for x))))\n"
                "(defun probe-2 () (let ((throw-on-input 'tag)) nil))\n"))
      ;; Act
      (let ((uses (agent-repl-test-nti--uses-in-file file)))
        ;; Assert
        (should (equal uses '(("probe.el" . while-no-input)
                              ("probe.el" . throw-on-input))))))))

(ert-deftest agent-repl-test-nti-scan-ignores-comments-and-docstrings ()
  "A comment or docstring explaining the rule is not a use."
  (agent-repl-test--with-temp-dir dir "agent-repl-nti"
    ;; Arrange
    (let ((file (expand-file-name "probe.el" dir)))
      (with-temp-file file
        (insert ";; never `while-no-input' here\n"
                "(defun probe () \"Not under `while-no-input'.\" nil)\n"))
      ;; Act
      (let ((uses (agent-repl-test-nti--uses-in-file file)))
        ;; Assert
        (should (equal uses nil))))))

(provide 'test-no-throw-on-input)

;;; test-no-throw-on-input.el ends here
