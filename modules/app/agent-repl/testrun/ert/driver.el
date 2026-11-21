;;; driver.el --- Run one chunk of the agent-repl ERT suite -*- lexical-binding: t; -*-

;;; Commentary:

;; The test runner (modules/app/agent-repl/testrun) splits the ERT suite into
;; chunks of test FILES and runs each chunk in its own batch Emacs, one core
;; each.  This is the chunk's entry point:
;;
;;   emacs -batch -Q -l ert -l testrun/ert/driver.el \
;;     --eval '(agent-repl-testrun-ert "LISP-DIR" (quote CHUNK) (quote ROSTER))'
;;
;; A fourth argument, NAMES, narrows the chunk's files to single tests: an
;; alist of a chunk file to the test symbols of that file to run.  A file with
;; no entry runs whole.  Each file's names are checked against that file
;; alone, so a name its file does not define is an error, never a silently
;; empty run, and one file's names never narrow another (2026-11-13: two
;; `-ert FILE=NAMES' flags checked every name against each file).
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
;; planner splits the next run by.  As each test starts it prints
;;
;;   TESTRUN-TEST-START <unix seconds> <test>
;;
;; which places the test in its chunk's CPU over time, for its share of it
;; (testtimes.AttributeCPU).
;;
;; A PROFILING RUN (testrun.v1.TestRunRequest.profile) names the unit's
;; profiles directory in AGENT_REPL_TEST_PROFILE_DIR (testenv.ProfileDir).
;; The driver then samples the chunk's whole life, its loads included, with
;; the elisp CPU profiler, and writes two files there before it exits:
;;
;;   ert.cpu.eprofile   the profile itself (M-x profiler-find-profile)
;;   ert.cpu.top.tsv    the hottest functions, readable without Emacs:
;;                      samples, share of all samples, kind (total or
;;                      self), function
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

(defun agent-repl-testrun-ert--stamp-start (test)
  "Print the line saying TEST starts now (testtimes.StartLine)."
  (message "TESTRUN-TEST-START %.6f %s" (float-time) (ert-test-name test)))

(defconst agent-repl-testrun-ert--profile-env "AGENT_REPL_TEST_PROFILE_DIR"
  "The variable naming a profiling run's directory for this unit.")

(defconst agent-repl-testrun-ert--profile-top 80
  "How many functions each kind of `ert.cpu.top.tsv' lists.")

(defun agent-repl-testrun-ert--profile-dir ()
  "The profiles directory a profiling run gives this unit, or nil.
An empty value is no profiling run; a relative one is a broken run, which
signals rather than profiling into the working directory."
  (let ((dir (getenv agent-repl-testrun-ert--profile-env)))
    (cond ((or (null dir) (string-empty-p dir)) nil)
          ((not (file-name-absolute-p dir))
           (error "agent-repl-testrun: %s names %S, which is not absolute"
                  agent-repl-testrun-ert--profile-env dir))
          (t dir))))

(defun agent-repl-testrun-ert--profile-counts (log)
  "Return (TOTAL . SELF) sample counts per function from profiler LOG.
LOG maps each sampled backtrace (a vector, innermost frame first) to its
samples.  TOTAL counts a function once per backtrace it is anywhere on;
SELF counts only the innermost frame.  Both are hash tables keyed by the
function's printed name."
  (let ((total (make-hash-table :test 'equal))
        (self (make-hash-table :test 'equal)))
    (maphash
     (lambda (backtrace samples)
       (let ((seen nil) (innermost t))
         (seq-doseq (frame backtrace)
           (when frame
             (let ((name (if (symbolp frame) (symbol-name frame) (format "%S" frame))))
               (when innermost
                 (puthash name (+ samples (gethash name self 0)) self)
                 (setq innermost nil))
               (unless (member name seen)
                 (push name seen)
                 (puthash name (+ samples (gethash name total 0)) total)))))))
     log)
    (cons total self)))

(defun agent-repl-testrun-ert--profile-top-lines (counts kind all)
  "Return the hottest of COUNTS as `ert.cpu.top.tsv' lines labeled KIND.
ALL is every sample of the profile, which each share is of."
  (let (rows)
    (maphash (lambda (name samples) (push (cons name samples) rows)) counts)
    (setq rows (sort rows (lambda (a b) (> (cdr a) (cdr b)))))
    (mapcar (lambda (row)
              (format "%d\t%.1f%%\t%s\t%s" (cdr row)
                      (if (zerop all) 0.0 (/ (* 100.0 (cdr row)) all))
                      kind (car row)))
            (seq-take rows agent-repl-testrun-ert--profile-top))))

(defun agent-repl-testrun-ert--profile-write (profile dir)
  "Write PROFILE (`profiler-cpu-profile') and its hottest functions into DIR."
  (let* ((log (profiler-profile-log profile))
         (all 0))
    (maphash (lambda (_ samples) (setq all (+ all samples))) log)
    (profiler-write-profile profile (expand-file-name "ert.cpu.eprofile" dir))
    (let ((counts (agent-repl-testrun-ert--profile-counts log)))
      (with-temp-file (expand-file-name "ert.cpu.top.tsv" dir)
        (insert (format "# %d samples\n" all))
        (insert "samples\tshare\tkind\tfunction\n")
        (dolist (line (append (agent-repl-testrun-ert--profile-top-lines (car counts) "total" all)
                              (agent-repl-testrun-ert--profile-top-lines (cdr counts) "self" all)))
          (insert line "\n"))))))

(defun agent-repl-testrun-ert (dir chunk roster &optional names)
  "Run the ERT test files CHUNK from DIR, ordered and checked against ROSTER.
NAMES is an alist of a CHUNK file to the tests of it to run; a file with
no entry runs whole."
  (or noninteractive
      (user-error "This function is only for use in batch mode"))
  (let ((eln-dir (and (featurep 'native-compile)
                      (make-temp-file "test-nativecomp-cache-" t))))
    (when eln-dir
      (startup-redirect-eln-cache eln-dir))
    (setq attempt-stack-overflow-recovery nil
          attempt-orderly-shutdown-on-fatal-signal nil)
    (advice-add 'ert-run-test :before #'agent-repl-testrun-ert--stamp-start)
    (unwind-protect
        (let ((ordered (seq-filter (lambda (f) (member f chunk)) roster))
              (profile-dir (agent-repl-testrun-ert--profile-dir))
              (load-seconds nil)
              (unexpected 0))
          ;; Inside the protected run, so a broken chunk exits 2 like any
          ;; other broken run.
          (dolist (file chunk)
            (unless (member file roster)
              (error "agent-repl-testrun: %s is not on the roster" file)))
          ;; A profiling run samples everything from here on: the helpers'
          ;; and the chunk's loads, and every test.
          (when profile-dir
            (require 'profiler)
            (profiler-start 'cpu))
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
          (dolist (entry names)
            (unless (member (car entry) chunk)
              (error "agent-repl-testrun: tests are named for %s, which is not in the chunk %s" (car entry) chunk))
            (dolist (name (cdr entry))
              (unless (and (ert-test-boundp name)
                           (agent-repl-testrun-ert--own-p (car entry) (ert-get-test name)))
                (error "agent-repl-testrun: %s defines no test `%s'" (car entry) name))))
          (dolist (file ordered)
            (let* ((start (float-time))
                   (stats (ert-run-tests-batch (agent-repl-testrun-ert--own-tests file (cdr (assoc file names))))))
              (setq unexpected (+ unexpected (ert-stats-completed-unexpected stats)))
              (message "TESTRUN-ITEM %s %.6f" file
                       (+ (cdr (assoc file load-seconds)) (- (float-time) start)))))
          (when profile-dir
            ;; The log is taken when the profiler stops, so the profile is
            ;; read after the stop, never before (profiler.el
            ;; `profiler-stop').
            (profiler-stop)
            (agent-repl-testrun-ert--profile-write (profiler-cpu-profile) profile-dir))
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

(defun agent-repl-testrun-ert-list (dir files)
  "Print the tests each of FILES in DIR defines, running none.
A rerun's existence check (testrun's suites.Lister.ERTNames): the shared
helpers load first and then each of FILES, as a chunk loads them, and each
test a file defines prints one line,

  TESTRUN-NAME <file> <test>

Exit status is 0 once every file loaded, 2 when one could not."
  (or noninteractive
      (user-error "This function is only for use in batch mode"))
  (condition-case err
      (progn
        (load (expand-file-name "test-helpers.el" dir) nil t)
        (dolist (file files)
          (unless (equal file "test-helpers.el")
            (load (expand-file-name file dir) nil t)))
        (dolist (test (ert-select-tests t t))
          (let* ((path (ert-test-file-name test))
                 (file (and path (file-name-nondirectory path))))
            (when (member file files)
              (princ (format "TESTRUN-NAME %s %s\n" file (ert-test-name test))))))
        (kill-emacs 0))
    (error
     (message "agent-repl-testrun: listing %s failed: %S" files err)
     (kill-emacs 2))))

(provide 'agent-repl-testrun-ert)

;;; driver.el ends here
