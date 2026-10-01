;;; test-prompt-summary.el --- ERT tests for prompt-summary.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-prompt-summary.el -f ert-run-tests-batch-and-exit
;;
;; THIS FILE IS THE MODULE'S ONE REAL VENDOR EXEC SITE, and the first thing
;; these tests assert is that it REFUSES under
;; `AGENT_REPL_FORBID_VENDOR_CALLS' -- refuses, never fakes.  A fabricated
;; summary would be a fallback: the feature is a real vendor call or it is
;; nothing, and a made-up title displayed as a real one is worse than no
;; title.
;;
;; Because the suite normally runs WITH that variable set, every test that
;; needs the unguarded path clears it explicitly for its own body, and the
;; two process boundaries are stubbed so no subprocess is ever spawned
;; either way.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Fixtures ----

(defvar agent-repl-test-ps--started nil
  "Process-start invocations, as (WS CMD).")

(defvar agent-repl-test-ps--state nil
  "Workspace plist writes the stubbed `--ws-put' recorded.")

(defmacro agent-repl-test-ps--with (&rest body)
  "Run BODY with both process boundaries and the workspace store stubbed."
  (declare (indent 0))
  `(let ((agent-repl-test-ps--started nil)
         (agent-repl-test-ps--state nil)
         (agent-repl--prompt-summary-forbid-warned nil))
     (cl-letf (((symbol-function 'agent-repl--prompt-summary-process-start)
                (lambda (ws _out-buf cmd _sentinel)
                  (push (list ws cmd) agent-repl-test-ps--started)
                  'fake-process))
               ((symbol-function 'agent-repl--prompt-summary-process-send-input)
                (lambda (_proc _input) nil))
               ((symbol-function 'agent-repl--ws-put)
                (lambda (_ws key value) (push (cons key value) agent-repl-test-ps--state)))
               ((symbol-function 'agent-repl--ws-get) (lambda (_ws _key) nil))
               ((symbol-function 'agent-repl--state-save) (lambda (_ws) nil))
               ((symbol-function 'agent-repl--prompt-summary-redisplay) (lambda (_ws) nil))
               ((symbol-function 'agent-repl--prompt-summary-collect-context)
                (lambda (_ws) nil)))
       ,@body)))

(defmacro agent-repl-test-ps--unguarded (&rest body)
  "Run BODY with the vendor-call guard explicitly CLEARED for this process."
  (declare (indent 0))
  `(let ((process-environment
          (cons (concat agent-repl--prompt-summary-forbid-env "=")
                process-environment)))
     ;; `setenv' to nil removes the variable outright, which is what an
     ;; ordinary interactive session looks like.
     (setenv agent-repl--prompt-summary-forbid-env nil)
     ,@body))

;;;; ---- The vendor-call guard ----

(ert-deftest agent-repl-ps-refuses-under-the-forbid-variable ()
  "The one real vendor exec site refuses when vendor calls are forbidden."
  (agent-repl-test-ps--with
    (setenv agent-repl--prompt-summary-forbid-env "1")
    (should (agent-repl--prompt-summary-forbidden-p "ws-one"))))

(ert-deftest agent-repl-ps-kickoff-spawns-nothing-when-forbidden ()
  "A forbidden process spawns no summary: refusal, not a faked answer."
  (agent-repl-test-ps--with
    (setenv agent-repl--prompt-summary-forbid-env "1")
    (agent-repl--kickoff-prompt-summary "ws-one" "a reasonably long prompt to summarize")
    (should-not agent-repl-test-ps--started)))

(ert-deftest agent-repl-ps-forbidden-writes-no-pending-placeholder ()
  "A refusal leaves no pending state behind to display forever."
  (agent-repl-test-ps--with
    (setenv agent-repl--prompt-summary-forbid-env "1")
    (agent-repl--kickoff-prompt-summary "ws-one" "a reasonably long prompt to summarize")
    (should-not (assq :last-prompt-summary-pending agent-repl-test-ps--state))))

(defvar agent-repl-test-ps--info nil
  "Lines the stubbed info rung recorded, newest first.")

(defvar agent-repl-test-ps--warn nil
  "Lines the stubbed warn rung recorded, newest first.")

(defmacro agent-repl-test-ps--capturing-rungs (&rest body)
  "Run BODY with the info and warn rungs captured instead of logged."
  (declare (indent 0))
  `(let ((agent-repl-test-ps--info nil)
         (agent-repl-test-ps--warn nil))
     (cl-letf (((symbol-function 'agent-repl--info)
                (lambda (_ws fmt &rest args)
                  (push (apply #'format fmt args) agent-repl-test-ps--info)))
               ((symbol-function 'agent-repl--warn)
                (lambda (_ws fmt &rest args)
                  (push (apply #'format fmt args) agent-repl-test-ps--warn))))
       ,@body)))

(ert-deftest agent-repl-ps-refusal-is-recorded-at-info ()
  "The configured refusal is a record at INFO, naming the environment variable."
  (agent-repl-test-ps--with
    (agent-repl-test-ps--capturing-rungs
      (setenv agent-repl--prompt-summary-forbid-env "1")
      (should (agent-repl--prompt-summary-forbidden-p "ws-one"))
      (should (equal (list (format "elisp.prompt-summary.vendor-calls-forbidden env=%s -- prompt summaries are disabled for this process"
                                   agent-repl--prompt-summary-forbid-env))
                     agent-repl-test-ps--info)))))

(ert-deftest agent-repl-ps-refusal-uses-no-warn-rung ()
  "The expected configuration never reaches the warn rung."
  (agent-repl-test-ps--with
    (agent-repl-test-ps--capturing-rungs
      (setenv agent-repl--prompt-summary-forbid-env "1")
      (should (agent-repl--prompt-summary-forbidden-p "ws-one"))
      (should-not agent-repl-test-ps--warn))))

(ert-deftest agent-repl-ps-refusal-is-recorded-once ()
  "The refusal is a standing configuration fact, so it is recorded once."
  (agent-repl-test-ps--with
    (agent-repl-test-ps--capturing-rungs
      (setenv agent-repl--prompt-summary-forbid-env "1")
      (should (agent-repl--prompt-summary-forbidden-p "ws-one"))
      ;; Still forbidden on the second ask, and still only recorded once.
      (should (agent-repl--prompt-summary-forbidden-p "ws-one"))
      (should (= 1 (length agent-repl-test-ps--info))))))

(ert-deftest agent-repl-ps-not-forbidden-without-the-variable ()
  "With the variable unset the guard permits the call."
  (agent-repl-test-ps--with
    (agent-repl-test-ps--unguarded
      (should-not (agent-repl--prompt-summary-forbidden-p "ws-one")))))

;;;; ---- The headless command ----

(ert-deftest agent-repl-ps-command-uses-headless-print-mode ()
  "The summary runs headless: one prompt on stdin, one answer on stdout."
  (should (member "-p" (agent-repl--prompt-summary-command))))

(ert-deftest agent-repl-ps-command-names-the-configured-program ()
  "The program is the configured executable, not a hard-coded name."
  (let ((agent-repl-prompt-summary-program "some-vendor-cli"))
    (should (equal (car (agent-repl--prompt-summary-command)) "some-vendor-cli"))))

(ert-deftest agent-repl-ps-command-passes-the-configured-model ()
  "The summary model is passed through as `--model'."
  (let ((agent-repl-prompt-summary-model "haiku"))
    (let ((cmd (agent-repl--prompt-summary-command)))
      (should (equal (nth (1+ (cl-position "--model" cmd :test #'equal)) cmd) "haiku")))))

;;;; ---- The spawn path, with the guard cleared ----

(ert-deftest agent-repl-ps-kickoff-spawns-when-permitted ()
  "With vendor calls permitted a long enough prompt spawns the summary."
  (agent-repl-test-ps--with
    (agent-repl-test-ps--unguarded
      (agent-repl--kickoff-prompt-summary "ws-one" "a reasonably long prompt to summarize")
      (should (equal (length agent-repl-test-ps--started) 1)))))

(ert-deftest agent-repl-ps-kickoff-marks-pending-when-permitted ()
  "A spawned summary shows a pending placeholder until it resolves."
  (agent-repl-test-ps--with
    (agent-repl-test-ps--unguarded
      (agent-repl--kickoff-prompt-summary "ws-one" "a reasonably long prompt to summarize")
      (should (eq (cdr (assq :last-prompt-summary-pending agent-repl-test-ps--state)) t)))))

(ert-deftest agent-repl-ps-kickoff-skipped-when-disabled ()
  "The feature's own switch is honored before anything else."
  (agent-repl-test-ps--with
    (agent-repl-test-ps--unguarded
      (let ((agent-repl-prompt-summary-enabled nil))
        (agent-repl--kickoff-prompt-summary "ws-one" "a reasonably long prompt")
        (should-not agent-repl-test-ps--started)))))

(ert-deftest agent-repl-ps-kickoff-skipped-without-a-workspace ()
  "There is nowhere to write a summary without a workspace to write it to."
  (agent-repl-test-ps--with
    (agent-repl-test-ps--unguarded
      (agent-repl--kickoff-prompt-summary nil "a reasonably long prompt")
      (should-not agent-repl-test-ps--started))))

(ert-deftest agent-repl-ps-kickoff-skipped-for-trivial-input ()
  "A trivial prompt has no title worth a vendor call."
  (agent-repl-test-ps--with
    (agent-repl-test-ps--unguarded
      (agent-repl--kickoff-prompt-summary "ws-one" "/clear")
      (should-not agent-repl-test-ps--started))))

(ert-deftest agent-repl-ps-kickoff-skipped-for-a-non-string-prompt ()
  "A non-string prompt is a caller defect, refused rather than formatted."
  (agent-repl-test-ps--with
    (agent-repl-test-ps--unguarded
      (agent-repl--kickoff-prompt-summary "ws-one" 42)
      (should-not agent-repl-test-ps--started))))

(ert-deftest agent-repl-ps-spawn-runs-from-an-empty-private-directory ()
  "The summary runs from a directory of its own, never the shared temp root.
Its startup index walks the working directory, and the temp root's walk
cost up to seven cores per summary."
  (let* ((temporary-file-directory (file-name-as-directory (make-temp-file "ps-tmp" t)))
         (seen-dir nil))
    (unwind-protect
        (agent-repl-test-ps--with
          (agent-repl-test-ps--unguarded
            (cl-letf (((symbol-function 'agent-repl--prompt-summary-process-start)
                       (lambda (_ws _out-buf _cmd _sentinel)
                         (setq seen-dir default-directory)
                         'fake-process)))
              (agent-repl--prompt-summary-spawn "ws-one" "a reasonably long prompt to summarize"))))
      (delete-directory temporary-file-directory t))
    (should (equal seen-dir (file-name-as-directory
                             (expand-file-name "agent-repl-prompt-summary" temporary-file-directory))))
    (should-not (equal seen-dir temporary-file-directory))))

(ert-deftest agent-repl-ps-directory-is-created-when-missing ()
  "The private directory is made on demand, so a fresh temp root works."
  (let ((temporary-file-directory (file-name-as-directory (make-temp-file "ps-tmp" t))))
    (unwind-protect
        (let ((dir (agent-repl--prompt-summary-directory)))
          (should (file-directory-p dir))
          (should (equal (directory-files dir nil directory-files-no-dot-files-regexp) nil)))
      (delete-directory temporary-file-directory t))))

(ert-deftest agent-repl-ps-spawn-fails-loudly-when-its-directory-cannot-be-made ()
  "A private directory that cannot be made spawns nothing and says so."
  (let ((warned nil))
    (agent-repl-test-ps--with
      (agent-repl-test-ps--unguarded
        (cl-letf (((symbol-function 'make-directory)
                   (lambda (&rest _) (error "Permission denied")))
                  ((symbol-function 'agent-repl--warn)
                   (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warned))))
          (should-not (agent-repl--prompt-summary-spawn "ws-one" "a reasonably long prompt to summarize"))
          (should-not agent-repl-test-ps--started))))
    (should (cl-some (lambda (w) (string-match-p "prompt-summary: spawn failed.*Permission denied" w)) warned))))

;;;; ---- The boundaries are registered ----

(ert-deftest agent-repl-ps-process-start-is-a-registered-boundary ()
  "The spawn is a registered external boundary, so tests cannot reach a shell."
  (should (memq 'agent-repl--prompt-summary-process-start
                agent-repl--external-boundary-functions)))

(ert-deftest agent-repl-ps-process-send-input-is-a-registered-boundary ()
  "So is the stdin write paired with it."
  (should (memq 'agent-repl--prompt-summary-process-send-input
                agent-repl--external-boundary-functions)))

;;;; ---- Truncation and cleaning ----

(ert-deftest agent-repl-ps-truncate-keeps-a-single-line-summary ()
  "A single-line summary is returned unchanged."
  (should (equal (agent-repl--prompt-summary-truncate "short title") "short title")))

(ert-deftest agent-repl-ps-truncate-collapses-newlines ()
  "A multi-line summary collapses to one line: the mode line has one."
  (should (equal (agent-repl--prompt-summary-truncate "two\n  lines") "two lines")))

(ert-deftest agent-repl-ps-truncate-answers-empty-for-whitespace ()
  "An all-whitespace summary is no summary, and answers the empty string."
  (should (equal (agent-repl--prompt-summary-truncate "   \n ") "")))

(ert-deftest agent-repl-ps-skip-p-rejects-a-bare-digit ()
  "A bare numeral is an answer to a prompt, with no title to summarize."
  (should (agent-repl--prompt-summary-skip-p "2")))

(ert-deftest agent-repl-ps-skip-p-rejects-a-bare-yes ()
  "A one-character confirmation must not blow away the previous summary."
  (should (agent-repl--prompt-summary-skip-p "y")))

(ert-deftest agent-repl-ps-skip-p-rejects-an-argumentless-slash-command ()
  "A lone slash command carries no content to summarize."
  (should (agent-repl--prompt-summary-skip-p "/clear")))

(ert-deftest agent-repl-ps-skip-p-rejects-empty-input ()
  "Nothing typed is nothing to summarize."
  (should (agent-repl--prompt-summary-skip-p "   ")))

(ert-deftest agent-repl-ps-skip-p-accepts-a-real-prompt ()
  "A real prompt is worth summarizing."
  (should-not (agent-repl--prompt-summary-skip-p
               "please refactor the roster reconciliation to walk children")))

;;;; ---- Tests: attach-all manual recovery command ----
;;
;; `agent-repl-prompt-summary-attach-all' is deliberately not run at load (see
;; the note at the end of prompt-summary.el); it survives as the interactive
;; recovery command, so it has no elisp caller.  These pin it.

(ert-deftest agent-repl-ps-attach-all-attaches-to-a-live-vterm-buffer ()
  "A workspace with a live vterm buffer gets the segment attached."
  (agent-repl-test--with-clean-state
    (let ((attached nil))
      (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () '("ws1")))
                ((symbol-function 'agent-repl--ws-get)
                 (lambda (_ws _key) (current-buffer)))
                ((symbol-function 'agent-repl--prompt-summary-attach-to-mode-line)
                 (lambda (buf) (setq attached buf))))
        (agent-repl-prompt-summary-attach-all)
        (should (eq attached (current-buffer)))))))

(ert-deftest agent-repl-ps-attach-all-skips-a-workspace-without-a-vterm-buffer ()
  "No live vterm buffer means nothing is attached for that workspace."
  (agent-repl-test--with-clean-state
    (let ((attached nil))
      (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () '("ws1")))
                ((symbol-function 'agent-repl--ws-get) (lambda (_ws _key) nil))
                ((symbol-function 'agent-repl--prompt-summary-attach-to-mode-line)
                 (lambda (buf) (setq attached buf))))
        (agent-repl-prompt-summary-attach-all)
        (should-not attached)))))

;;;; ---- The sentinel's own buffer teardown ----

(ert-deftest agent-repl-test-ps-sentinel-tears-down-through-the-safe-kill ()
  "OUT-BUF is the process's OWN buffer, so it goes through the detaching kill.
A bare `kill-buffer' would delete the process from inside the kill and
re-enter this sentinel forever."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((buf (generate-new-buffer " *agent-repl-test-ps-out*"))
          (safe nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--kill-buffer-safely)
                     (lambda (b) (setq safe b)))
                    ((symbol-function 'process-exit-status) (lambda (_p) 0))
                    ((symbol-function 'process-status) (lambda (_p) 'exit))
                    ((symbol-function 'agent-repl--prompt-summary-apply) #'ignore))
            (with-current-buffer buf (insert "a summary"))
            ;; Act
            (funcall (agent-repl--prompt-summary-make-sentinel "ws" "raw" buf)
                     'fake-proc "finished\n")
            ;; Assert
            (should (eq safe buf)))
        (kill-buffer buf)))))

(provide 'test-prompt-summary)

;;; test-prompt-summary.el ends here
