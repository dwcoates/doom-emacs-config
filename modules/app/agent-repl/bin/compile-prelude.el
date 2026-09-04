;;; compile-prelude.el --- byte-compile environment for agent-repl -*- lexical-binding: t; -*-

;;; Commentary:

;; Loaded by `bin/byte-compile.sh' before `batch-byte-compile'.
;;
;; The module's sources are written against Doom Emacs, where `map!',
;; `after!' and friends are MACROS.  Under `emacs -Q' they do not exist, so
;; the byte-compiler would read every `(map! ...)' form as a function call
;; and mis-report its keyword arguments as undefined functions.  This file
;; supplies the same compile-time macro shapes the test harness supplies at
;; load time (see `lisp/test-helpers.el'), so the compiler sees the forms
;; as the macro calls they are.
;;
;; It defines MACROS only — nothing here stands in for a function or a
;; variable a source file references.  Those are declared at their point of
;; use with `declare-function' / `defvar', so the warnings stay honest.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(unless (fboundp 'after!)
  (defmacro after! (_feature &rest body)
    "Compile-time stand-in for Doom's `after!': evaluate BODY inline."
    (declare (indent defun))
    `(progn ,@body)))

(unless (fboundp 'map!)
  (defmacro map! (&rest _args)
    "Compile-time stand-in for Doom's `map!': expand to nil."
    nil))

(unless (fboundp 'cmd!)
  (defmacro cmd! (&rest body)
    "Compile-time stand-in for Doom's `cmd!': wrap BODY in a command."
    `(lambda () (interactive) ,@body)))

(unless (fboundp 'modulep!)
  (defmacro modulep! (&rest _args)
    "Compile-time stand-in for Doom's `modulep!': expand to nil."
    nil))

(unless (fboundp 'load!)
  (defmacro load! (filename &optional path noerror)
    "Compile-time stand-in for Doom's `load!' over FILENAME, PATH, NOERROR."
    `(load (expand-file-name ,filename ,(or path 'default-directory))
           nil ,(if noerror noerror t))))

(unless (fboundp 'set-popup-rule!)
  (defmacro set-popup-rule! (&rest _args)
    "Compile-time stand-in for Doom's `set-popup-rule!': expand to nil."
    nil))

;; magit is not installable under `emacs -Q', and `magit-insert-section' is
;; a macro whose first argument is a section TYPE, not a call.  Without the
;; macro shape the compiler reads `(commit sha)' as a call to `commit'.
(unless (fboundp 'magit-insert-section)
  (defmacro magit-insert-section (_type &rest body)
    "Compile-time stand-in for magit's `magit-insert-section' over BODY."
    (declare (indent defun))
    `(progn ,@body)))

(provide 'compile-prelude)
;;; compile-prelude.el ends here
