;;; test-declarations.el --- every declare-function names something real -*- lexical-binding: t; -*-

;;; Commentary:

;; A `declare-function' is a PROMISE to the byte compiler: this symbol is
;; defined somewhere the compiler cannot see, so do not warn about the call.
;; The compiler takes the promise at face value and never checks it, which
;; makes a broken promise invisible to `bin/byte-compile.sh' — the gate that
;; would otherwise catch a call to a symbol nothing defines.
;;
;; That is not hypothetical here.  79faac1c7 deleted `frontend-client.el' and
;; left five declares behind naming its functions, three of them wired into
;; the gui frontend registry.  Each was a void function at the moment a user
;; reached it, and the whole module byte-compiled clean throughout.
;;
;; So this suite audits the promises.  It scans EVERY `lisp/*.el' file for
;; `declare-function' forms and, for those naming one of OUR OWN symbols,
;; requires the symbol to be defined once the module is loaded.  Declares for
;; third-party and built-in functions (`xwidget-get', `evil-define-key*',
;; `magit-toplevel') are out of scope on purpose: they are promises about
;; packages that legitimately are not in this tree, and a batch Emacs has no
;; standing to adjudicate them.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-declarations.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)
(require 'subr-x)
(require 'seq)

(defconst agent-repl-test-declarations--optional
  '(agent-repl--sidebar-push)
  "Our own symbols a source may declare WITHOUT this tree defining them.

Exactly one kind of entry belongs here: a declare for an OPTIONAL
integration whose module is not part of this tree, where every call site
guards on `fboundp\=' and has a defined behavior for the absent case.
`agent-repl--sidebar-push\=' is that: sidebar.el is not in `lisp/\=', and
`agent-repl--ws-repaint-sidebar\=' logs \"sidebar not loaded\" and returns
rather than calling it.

An entry here is a claim about a guard, so adding one means pointing at
the guard.  A declare that is merely stale is a defect, not an entry.")

(defconst agent-repl-test-declarations--own-prefixes '("agent-repl")
  "Prefixes marking a symbol this tree is responsible for defining.
Everything else a `declare-function' names belongs to Emacs itself or to a
package the module depends on, and is not this suite's business.")

(defun agent-repl-test-declarations--own-symbol-p (symbol)
  "Non-nil when SYMBOL is one this tree is responsible for defining."
  (let ((name (symbol-name symbol)))
    (cl-some (lambda (prefix) (string-prefix-p prefix name))
             agent-repl-test-declarations--own-prefixes)))

(defconst agent-repl-test-declarations--lisp-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The `lisp/' directory this suite lives in.
Captured at LOAD time: `load-file-name' is unbound by the time ERT runs a
test body, so resolving it there would answer nil and scan nothing.")

(defun agent-repl-test-declarations--files ()
  "Return every non-test elisp source in `lisp/'.

Suites are out of scope, and not for convenience: a suite declares its own
batch-only helpers (`agent-repl-itest--script' lives in
test-integration-helpers.el, which only the integration suites load), so
`fboundp' under THIS suite\='s load would call a perfectly-good declare
broken.  A source, by contrast, is loaded in full by test-helpers.el, which
is what makes `fboundp' the whole answer below."
  (seq-remove (lambda (file)
                (string-prefix-p "test-" (file-name-nondirectory file)))
              (directory-files agent-repl-test-declarations--lisp-dir t "\\.el\\'")))

(defun agent-repl-test-declarations--in-file (file)
  "Return `(SYMBOL . FILE)' for every `declare-function' form in FILE.
Reads FILE as data rather than matching text, so a symbol split across
lines or a form inside a comment-like string cannot fool the scan."
  (let ((found nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (consp form) (eq (car form) 'declare-function)
                         (symbolp (cadr form)))
                (push (cons (cadr form) (file-name-nondirectory file)) found))))
        (end-of-file nil)))
    (nreverse found)))

(defun agent-repl-test-declarations--all ()
  "Return `(SYMBOL . FILE)' for every `declare-function' in `lisp/'."
  (mapcan #'agent-repl-test-declarations--in-file
          (agent-repl-test-declarations--files)))

(ert-deftest agent-repl-test-declarations-scan-finds-the-declares ()
  "The scan reads real forms out of the tree, so an empty result is a bug.
Every other assertion here is a `should-not' over the scan's output: a
scan that silently found nothing would pass them all while auditing
nothing at all."
  ;; Act
  (let ((declares (agent-repl-test-declarations--all)))
    ;; Assert
    (should (> (length declares) 50))))

(ert-deftest agent-repl-test-declarations-name-defined-functions ()
  "No `declare-function' promises a symbol of ours that nothing defines.
The byte compiler takes every such promise on trust, so a declare left
behind by a deleted file compiles clean and dies at the call site.  The
module is fully loaded by `test-helpers.el', so `fboundp' is the whole
answer for our own symbols."
  ;; Arrange
  (let ((broken nil))
    ;; Act
    (pcase-dolist (`(,symbol . ,file) (agent-repl-test-declarations--all))
      (when (and (agent-repl-test-declarations--own-symbol-p symbol)
                 (not (memq symbol agent-repl-test-declarations--optional))
                 (not (fboundp symbol)))
        (push (format "%s (declared in %s)" symbol file) broken)))
    ;; Assert
    (should (equal (nreverse broken) nil))))

(ert-deftest agent-repl-test-declarations-exemptions-are-still-declared ()
  "Every exemption still names a declare that exists, so the list cannot rot.
An exemption outliving its `declare-function' is a standing permission for
a symbol nobody mentions any more — the audit would keep honoring it long
after the reason for it went away."
  ;; Arrange
  (let ((declared (mapcar #'car (agent-repl-test-declarations--all)))
        (stale nil))
    ;; Act
    (dolist (symbol agent-repl-test-declarations--optional)
      (unless (memq symbol declared)
        (push symbol stale)))
    ;; Assert
    (should (equal (nreverse stale) nil))))

(provide 'test-declarations)

;;; test-declarations.el ends here
