;;; driver.el --- Run one chunk of the agent-repl ERT suite -*- lexical-binding: t; -*-

;;; Commentary:

;; The test runner (modules/app/agent-repl/testrun) splits the ERT suite into
;; chunks of test FILES and runs each chunk in its own batch Emacs, one core
;; each.  This is the chunk's entry point:
;;
;;   emacs -batch -Q -l ert -l testrun/ert/driver.el \
;;     --eval '(agent-repl-testrun-ert "LISP-DIR" (quote CHUNK) (quote ROSTER))'
;;
;; A fourth argument, NAMES, narrows the chunk to those tests (symbols) among
;; its files' own: a run that asks for single tests.  A name no chunk file
;; defines is an error, never a silently empty run.
;;
;; ROSTER is every file `lisp/test-agent-repl.el' loads, in its order, the
;; helpers included (they define tests too); CHUNK is
;; this process's share of it.  The driver loads the shared helpers and the
;; chunk's files in roster order, exactly as the aggregator would, then runs
;; each file's own tests and prints one line per file:
;;
;;   TESTRUN-ITEM <file> <seconds>
;;
;; which is that file's load time plus its tests' run time, the cost the
;; planner splits the next run by.
;;
;; A test is "a file's own" by `ert-test-file-name'.  A loaded test whose file
;; is not on the roster at all would run in NO chunk, which is the silent hole
;; the aggregator's exhaustive-list rule exists to prevent; it is an error.
;;
;; Exit status mirrors `ert-run-tests-batch-and-exit': 0 when every result was
;; expected, 1 on any unexpected result, 2 when the run itself broke.

;;; Code:

(require 'ert)

(defun agent-repl-testrun-ert--own-p (file test)
  "Whether TEST is defined in FILE (a bare file name)."
  (equal file (and (ert-test-file-name test)
                   (file-name-nondirectory (ert-test-file-name test)))))

(defun agent-repl-testrun-ert--own-tests (file &optional names)
  "Selector for the tests defined in FILE (a bare file name).
With NAMES, only those of them."
  `(satisfies ,(lambda (test)
                 (and (agent-repl-testrun-ert--own-p file test)
                      (or (null names) (memq (ert-test-name test) names))))))

(defun agent-repl-testrun-ert (dir chunk roster &optional names)
  "Run the ERT test files CHUNK from DIR, ordered and checked against ROSTER.
With NAMES, run only those tests of CHUNK's files."
  (or noninteractive
      (user-error "This function is only for use in batch mode"))
  (let ((eln-dir (and (featurep 'native-compile)
                      (make-temp-file "test-nativecomp-cache-" t))))
    (when eln-dir
      (startup-redirect-eln-cache eln-dir))
    (setq attempt-stack-overflow-recovery nil
          attempt-orderly-shutdown-on-fatal-signal nil)
    (unwind-protect
        (let ((ordered (seq-filter (lambda (f) (member f chunk)) roster))
              (load-seconds nil)
              (unexpected 0))
          ;; Inside the protected run, so a broken chunk exits 2 like any
          ;; other broken run.
          (dolist (file chunk)
            (unless (member file roster)
              (error "agent-repl-testrun: %s is not on the roster" file)))
          ;; The helpers load first in every chunk, as in the aggregator; they
          ;; are also a roster file, because they define tests of their own.
          (let ((start (float-time)))
            (load (expand-file-name "test-helpers.el" dir) nil t)
            (push (cons "test-helpers.el" (- (float-time) start)) load-seconds))
          (dolist (file ordered)
            (unless (equal file "test-helpers.el")
              (let ((start (float-time)))
                (load (expand-file-name file dir) nil t)
                (push (cons file (- (float-time) start)) load-seconds))))
          (dolist (test (ert-select-tests t t))
            (let* ((path (ert-test-file-name test))
                   (file (and path (file-name-nondirectory path))))
              (unless (member file roster)
                (error "agent-repl-testrun: test `%s' is defined in %s, which lisp/test-agent-repl.el does not load, so no chunk would run it"
                       (ert-test-name test) (or path "no file")))))
          (dolist (name names)
            (unless (and (ert-test-boundp name)
                         (seq-some (lambda (file)
                                     (agent-repl-testrun-ert--own-p file (ert-get-test name)))
                                   chunk))
              (error "agent-repl-testrun: no file of %s defines the test `%s'" chunk name)))
          (dolist (file ordered)
            (let* ((start (float-time))
                   (stats (ert-run-tests-batch (agent-repl-testrun-ert--own-tests file names))))
              (setq unexpected (+ unexpected (ert-stats-completed-unexpected stats)))
              (message "TESTRUN-ITEM %s %.6f" file
                       (+ (cdr (assoc file load-seconds)) (- (float-time) start)))))
          (when eln-dir
            (ignore-errors (delete-directory eln-dir t)))
          (kill-emacs (if (zerop unexpected) 0 1)))
      (unwind-protect
          (progn
            (message "Error running tests")
            (backtrace))
        (when eln-dir
          (ignore-errors (delete-directory eln-dir t)))
        (kill-emacs 2)))))

(provide 'agent-repl-testrun-ert)

;;; driver.el ends here
