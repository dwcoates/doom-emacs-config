;;; test-rules-scan.el --- Rule tests: every rule test lives in the rule suite -*- lexical-binding: t; -*-

;;; Commentary:

;; THE RULE TESTS LIVE IN ONE SUITE.  A rule test reads the module's elisp
;; sources from disk as data and holds them to a structural rule ("no file
;; calls X but Y", "every Z goes through one helper").  Every one of them
;; lives in a `test-rules-*.el' file, the `ert-rules' suite, which every
;; branch runs before its merge is admitted to the queue (lisp/AGENTS.md,
;; "Rule tests").  A rule test left in a module's suite runs only in the
;; merge gate, after the branch has queued, which is what this suite exists
;; to avoid.
;;
;; So this file scans every other `test-*.el' for an `ert-deftest' that
;; reads elisp source from disk, and fails naming each one.  The detector
;; reads the suites as data (`agent-repl-test--file-forms'), never as text,
;; so a comment or a docstring naming a source is never a read:
;;
;; - A FILE READER is a primitive that reads a file or lists a directory
;;   (`agent-repl-test-rules-scan--primitive-readers'), or any function a
;;   suite defines whose body calls a file reader.
;; - A SOURCE MARK is a string literal naming an elisp file (a whole string
;;   such as "status.el" or "lisp/core.el", with no space in it) or matching
;;   one (a regexp holding "\\.el"), the symbol `symbol-file', or a variable a
;;   suite defines whose value holds a source mark.
;; - A SOURCE READER is a definition whose body both calls a file reader and
;;   holds a source mark, or names a source reader.
;; - A test reads source when its body (its docstring aside) names a source
;;   reader, or both calls a file reader and holds a source mark.
;;
;; A test that reads an elisp file and is no rule (a binding observable only
;; in its source, a module tree the test writes itself) is named in
;; `agent-repl-test-rules-scan--allowed' with its reason; every entry there
;; must still be one the detector flags, so the list cannot rot.

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- The detector ----

(defconst agent-repl-test-rules-scan--primitive-readers
  '(insert-file-contents insert-file-contents-literally find-file-noselect find-file
    directory-files directory-files-recursively directory-files-and-attributes
    file-expand-wildcards)
  "The primitives that read a file or list a directory.")

(defconst agent-repl-test-rules-scan--function-definers
  '(defun defmacro defsubst cl-defun cl-defmacro define-inline)
  "The forms that define a function: their body follows an argument list.")

(defconst agent-repl-test-rules-scan--variable-definers
  '(defconst defvar defvar-local defcustom)
  "The forms that define a variable: their value is their one body form.")

(defun agent-repl-test-rules-scan--walk (fn x)
  "Call FN on X and on every cons and atom inside it, vectors included."
  (funcall fn x)
  (cond ((consp x)
         (while (consp x)
           (agent-repl-test-rules-scan--walk fn (car x))
           (setq x (cdr x)))
         (when x (agent-repl-test-rules-scan--walk fn x)))
        ((vectorp x)
         (mapc (lambda (y) (agent-repl-test-rules-scan--walk fn y)) x))))

(defun agent-repl-test-rules-scan--calls (body)
  "Every symbol BODY calls: the car of each list inside it."
  (let (found)
    (agent-repl-test-rules-scan--walk
     (lambda (x) (when (and (consp x) (symbolp (car x))) (push (car x) found)))
     body)
    found))

(defun agent-repl-test-rules-scan--symbols (body)
  "Every symbol BODY holds, called or not."
  (let (found)
    (agent-repl-test-rules-scan--walk (lambda (x) (when (symbolp x) (push x found))) body)
    found))

(defun agent-repl-test-rules-scan--source-literal-p (string)
  "Whether STRING names an elisp source: a path ending .el, or a regexp for one."
  (or (string-match-p "\\\\\\.el\\([^[:alnum:]]\\|\\'\\)" string)
      (string-match-p "\\`[^[:space:]]*\\.el\\'" string)))

(defun agent-repl-test-rules-scan--read (body)
  "What the detector reads off BODY: (CALLS SYMBOLS LITERAL).
CALLS and SYMBOLS are `agent-repl-test-rules-scan--calls' and `--symbols';
LITERAL is whether BODY holds a string naming an elisp source."
  (let (literal)
    (agent-repl-test-rules-scan--walk
     (lambda (x)
       (when (and (stringp x) (agent-repl-test-rules-scan--source-literal-p x))
         (setq literal t)))
     body)
    (list (delete-dups (agent-repl-test-rules-scan--calls body))
          (delete-dups (agent-repl-test-rules-scan--symbols body))
          literal)))

(defun agent-repl-test-rules-scan--body (form)
  "FORM's body as the detector reads it: no argument list, no docstring.
A variable definition's body is its value alone."
  (let ((head (car form)))
    (cond ((memq head agent-repl-test-rules-scan--variable-definers)
           (list (nth 2 form)))
          ((or (eq head 'ert-deftest)
               (memq head agent-repl-test-rules-scan--function-definers))
           (let ((rest (nthcdr 3 form)))
             (if (and (stringp (car rest)) (cdr rest)) (cdr rest) rest))))))

(defun agent-repl-test-rules-scan--definitions (forms)
  "Every definition among FORMS, as (NAME KIND CALLS SYMBOLS LITERAL).
KIND is `function' or `variable'; the rest is what
`agent-repl-test-rules-scan--read' reads off its body."
  (let (defs)
    (dolist (form forms (nreverse defs))
      (when (and (consp form) (symbolp (nth 1 form)) (nth 1 form))
        (let ((kind (cond ((memq (car form) agent-repl-test-rules-scan--function-definers) 'function)
                          ((memq (car form) agent-repl-test-rules-scan--variable-definers) 'variable))))
          (when kind
            (push (cons (nth 1 form)
                        (cons kind (agent-repl-test-rules-scan--read
                                    (agent-repl-test-rules-scan--body form))))
                  defs)))))))

(defun agent-repl-test-rules-scan--closure (defs seed test)
  "SEED grown to a fixpoint by every name of DEFS whose definition passes TEST.
TEST is called with a definition (NAME KIND CALLS SYMBOLS LITERAL) and the
set so far."
  (let ((set (copy-sequence seed)) (grew t))
    (while grew
      (setq grew nil)
      (dolist (def defs)
        (when (and (not (memq (car def) set)) (funcall test def set))
          (push (car def) set)
          (setq grew t))))
    set))

(defun agent-repl-test-rules-scan--reads-source-p (read readers marks sources)
  "Whether READ, a body's (CALLS SYMBOLS LITERAL), reads elisp source.
It names one of SOURCES, or calls one of READERS while holding a source
literal or naming one of MARKS."
  (cl-destructuring-bind (calls symbols literal) read
    (or (cl-intersection symbols sources)
        (and (cl-intersection calls readers)
             (or literal (cl-intersection symbols marks))))))

(defun agent-repl-test-rules-scan--offenders (files)
  "Every (FILE . TEST) of FILES whose `ert-deftest' reads elisp source.
FILES are suite paths; every definition of every one of them counts toward
what reads a file, whatever file the test is in."
  (let* ((forms (mapcar (lambda (file) (cons file (agent-repl-test--file-forms file))) files))
         (defs (mapcan (lambda (entry) (agent-repl-test-rules-scan--definitions (cdr entry))) forms))
         (readers (agent-repl-test-rules-scan--closure
                   defs agent-repl-test-rules-scan--primitive-readers
                   (lambda (def set)
                     (and (eq (nth 1 def) 'function) (cl-intersection (nth 2 def) set)))))
         (marks (agent-repl-test-rules-scan--closure
                 defs '(symbol-file)
                 (lambda (def set)
                   (and (eq (nth 1 def) 'variable)
                        (or (nth 4 def) (cl-intersection (nth 3 def) set))))))
         (sources (agent-repl-test-rules-scan--closure
                   defs nil
                   (lambda (def set)
                     (agent-repl-test-rules-scan--reads-source-p (nthcdr 2 def) readers marks set))))
         offenders)
    (dolist (entry forms (nreverse offenders))
      (dolist (form (cdr entry))
        (when (and (eq (car-safe form) 'ert-deftest)
                   (agent-repl-test-rules-scan--reads-source-p
                    (agent-repl-test-rules-scan--read (agent-repl-test-rules-scan--body form))
                    readers marks sources))
          (push (cons (file-name-nondirectory (car entry)) (nth 1 form)) offenders))))))

;;;; ---- What may read elisp source outside the rule suite ----

(defconst agent-repl-test-rules-scan--allowed
  '(("test-keybindings.el" nil
     "`map!' is a no-op stub under `emacs -Q', so a leader binding is observable only in keybindings.el's source: these are the bindings' own tests, not rules")
    ("test-magit.el" agent-repl-test-magit-status-main-worktree-bound-to-spc-g-G
     "the same: `SPC g G' is observable only in magit.el's source")
    ("test-history.el" agent-repl-test-write-sexp-file-leaves-no-temporary
     "lists the temporary directory the test writes; `state.el' is the data file under test")
    ("test-history.el" agent-repl-test-write-sexp-file-failed-write-keeps-previous-contents
     "the same temporary directory and data file")
    ("test-threads.el" agent-repl-test-threads-the-launch-file-opens-chats-and-closes
     "visits a copy of the launch data file; `symbol-file' only finds the directory it ships in")
    ("test-elisp-build.el" agent-repl-test-eb-vector-per-file-hashing
     "hashes a module tree the test writes into a temporary directory")
    ("test-integration-link.el" agent-repl-itest-link-reload-elisp-for-this-root-loads-the-set
     "loads a module tree the test writes into a temporary root")
    ("test-test-helpers.el" agent-repl-test-helpers-file-forms-reads-every-top-level-form
     "tests the shared reader itself on a file the test writes")
    ("test-test-helpers.el" agent-repl-test-helpers-file-forms-answers-nil-for-an-empty-file
     "tests the shared reader itself on a file the test writes")
    ("test-test-helpers.el" agent-repl-test-helpers-module-sources-leaves-out-every-suite
     "tests the shared source lister itself; it holds no source to a rule"))
  "Every test outside the rule suite allowed to read elisp source: (FILE TEST REASON).
A nil TEST allows every test of FILE.")

(defun agent-repl-test-rules-scan--allowed-p (offender)
  "Whether OFFENDER, a (FILE . TEST), is allowed to read source."
  (cl-some (lambda (entry)
             (and (equal (car entry) (car offender))
                  (or (null (nth 1 entry)) (eq (nth 1 entry) (cdr offender)))))
           agent-repl-test-rules-scan--allowed))

(defun agent-repl-test-rules-scan--suites ()
  "Every suite file of lisp/ the rule holds: `test-*.el', rule files aside."
  (cl-remove-if (lambda (file) (string-prefix-p "test-rules-" (file-name-nondirectory file)))
                (directory-files agent-repl-test--module-dir t "\\`test-.*\\.el\\'")))

;;;; ---- The rule ----

(ert-deftest agent-repl-test-rules-scan-no-suite-but-the-rules-reads-elisp-source ()
  "No `ert-deftest' outside `test-rules-*.el' reads elisp source, the allowed aside."
  ;; Arrange
  (let ((files (agent-repl-test-rules-scan--suites)))
    ;; Act
    (let ((offenders (cl-remove-if #'agent-repl-test-rules-scan--allowed-p
                                   (agent-repl-test-rules-scan--offenders files))))
      ;; Assert: each is a rule test in a module's suite; move it to the
      ;; `test-rules-*.el' file of its rule area.
      (should-not offenders))))

(ert-deftest agent-repl-test-rules-scan-every-allowed-test-still-reads-source ()
  "Every allowed entry names a file, and a test, the detector still flags."
  ;; Arrange
  (let ((offenders (agent-repl-test-rules-scan--offenders (agent-repl-test-rules-scan--suites))))
    ;; Act
    (let ((stale (cl-remove-if
                  (lambda (entry)
                    (cl-some (lambda (offender)
                               (and (equal (car entry) (car offender))
                                    (or (null (nth 1 entry)) (eq (nth 1 entry) (cdr offender)))))
                             offenders))
                  agent-repl-test-rules-scan--allowed)))
      ;; Assert
      (should-not stale))))

;;;; ---- The detector's own cases ----

(defun agent-repl-test-rules-scan--offenders-of (text)
  "The detector's offenders in a suite file holding TEXT."
  (agent-repl-test--with-temp-dir dir "rules-scan-"
    (let ((file (expand-file-name "test-fixture.el" dir)))
      (with-temp-file file (insert text))
      (agent-repl-test-rules-scan--offenders (list file)))))

(ert-deftest agent-repl-test-rules-scan-flags-a-test-reading-a-source ()
  "A test reading a named elisp source is flagged."
  ;; Arrange
  (let ((text "(ert-deftest fixture-reads ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name \"status.el\" dir))
    (should (search-forward \"x\" nil t))))"))
    ;; Act / Assert
    (should (equal (agent-repl-test-rules-scan--offenders-of text)
                   '(("test-fixture.el" . fixture-reads))))))

(ert-deftest agent-repl-test-rules-scan-flags-a-test-listing-the-sources ()
  "A test listing the sources by an elisp regexp is flagged."
  ;; Arrange
  (let ((text "(ert-deftest fixture-lists ()
  (should (directory-files dir t \"\\\\.el\\\\'\")))"))
    ;; Act / Assert
    (should (equal (agent-repl-test-rules-scan--offenders-of text)
                   '(("test-fixture.el" . fixture-lists))))))

(ert-deftest agent-repl-test-rules-scan-flags-a-test-reading-through-a-helper ()
  "A test reading source through a suite's own helper is flagged."
  ;; Arrange
  (let ((text "(defun fixture--text (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))
(defun fixture--source () (fixture--text (expand-file-name \"core.el\" dir)))
(ert-deftest fixture-through-helper ()
  (should (string-match-p \"x\" (fixture--source))))"))
    ;; Act / Assert
    (should (equal (agent-repl-test-rules-scan--offenders-of text)
                   '(("test-fixture.el" . fixture-through-helper))))))

(ert-deftest agent-repl-test-rules-scan-passes-a-test-reading-data ()
  "A test reading a data file, not elisp, is not flagged."
  ;; Arrange
  (let ((text "(ert-deftest fixture-reads-data ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name \"render-colors.json\" dir))
    (should (search-forward \"x\" nil t))))"))
    ;; Act / Assert
    (should-not (agent-repl-test-rules-scan--offenders-of text))))

(ert-deftest agent-repl-test-rules-scan-passes-a-source-named-in-prose ()
  "A source named in a docstring or a message is no read."
  ;; Arrange
  (let ((text "(ert-deftest fixture-prose ()
  \"Reads the log status.el writes.\"
  (with-temp-buffer
    (insert-file-contents log)
    (should (search-forward \"x\" nil t) \"host.el's record\")))"))
    ;; Act / Assert
    (should-not (agent-repl-test-rules-scan--offenders-of text))))

(provide 'test-rules-scan)

;;; test-rules-scan.el ends here
