;;; test-page-report.el --- ERT tests for page-report.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Every load and unload is told to the daemon with its reason, and every
;; host subscribe restates what the web view holds.  The harness records the
;; reports `agent-repl-host--send-subject-verb' would send.
;;
;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-page-report.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Harness ----

(defvar agent-repl-test-pr--sent nil
  "Every report sent, newest first, each (WS REPORT).")

(defvar agent-repl-test-pr--pages nil
  "Every page the harness's host holds, each (WS . WHY), WHY nil for none.")

(defvar agent-repl-test-pr--errors nil
  "Every ERROR record's text, newest first.")

(defvar agent-repl-test-pr--answers nil
  "Every call's answers not yet given, oldest first, each (ON-SUCCESS . ON-NOT-SENT).")

(defun agent-repl-test-pr--answer (how)
  "Answer the oldest call not yet answered: HOW `:success' or `:not-sent'."
  (let ((answer (pop agent-repl-test-pr--answers)))
    (funcall (if (eq how :success) (car answer) (cdr answer)))))

(defmacro agent-repl-test-pr--with (&rest body)
  "Run BODY with the sender and the host stubbed."
  (declare (indent 0))
  `(agent-repl-test--with-clean-state
     (let ((agent-repl-test-pr--sent nil)
           (agent-repl-test-pr--pages nil)
           (agent-repl-test-pr--errors nil)
           (agent-repl-test-pr--answers nil))
       (cl-letf (((symbol-function 'agent-repl-host-ref)
                  (lambda (ws) (and (not (equal ws "unreffed")) (list :id (concat "id-" ws)))))
                 ((symbol-function 'agent-repl-host--send-subject-verb)
                  (lambda (ws op rpc on-success on-not-sent &optional extra)
                    (should (equal op "page-state"))
                    (should (eq rpc #'agent-repl-rpc-report-page-state))
                    (push (list ws (plist-get extra :report)) agent-repl-test-pr--sent)
                    (setq agent-repl-test-pr--answers
                          (append agent-repl-test-pr--answers (list (cons on-success on-not-sent))))))
                 ((symbol-function 'agent-repl-webhost-has-page-p)
                  (lambda (ws) (and (assoc ws agent-repl-test-pr--pages) t)))
                 ((symbol-function 'agent-repl-webhost-page-reason)
                  (lambda (ws) (cdr (assoc ws agent-repl-test-pr--pages))))
                 ((symbol-function 'agent-repl--emit-error)
                  (agent-repl-test--record-into agent-repl-test-pr--errors)))
         ,@body))))

;;;; ---- Changes ----

(ert-deftest agent-repl-test-pr-a-load-is-reported-with-its-reason ()
  "A load sends the loaded arm with its reason."
  (agent-repl-test-pr--with
    ;; Act
    (agent-repl-page-report-loaded "alpha" 'allocated)
    ;; Assert
    (should (equal agent-repl-test-pr--sent '(("alpha" (:arm :loaded :value :allocated)))))))

(ert-deftest agent-repl-test-pr-an-unload-is-reported-with-its-reason ()
  "An unload sends the unloaded arm with its reason."
  (agent-repl-test-pr--with
    ;; Act
    (agent-repl-page-report-unloaded "alpha" 'deallocated)
    ;; Assert
    (should (equal agent-repl-test-pr--sent '(("alpha" (:arm :unloaded :value :deallocated)))))))

(ert-deftest agent-repl-test-pr-a-known-load-reason-passes-the-check ()
  "A reason a load path names passes the check, recording nothing."
  (agent-repl-test-pr--with
    ;; Act
    (agent-repl-page-report-check-load-reason "alpha" 'slot)
    ;; Assert
    (should-not agent-repl-test-pr--errors)
    (should-not agent-repl-test-pr--sent)))

(ert-deftest agent-repl-test-pr-an-unknown-load-reason-fails-the-check ()
  "The check records a reason no load path names at ERROR and signals."
  (agent-repl-test-pr--with
    ;; Act
    (should-error (agent-repl-page-report-check-load-reason "alpha" 'whim))
    ;; Assert
    (should (seq-some (lambda (text) (string-search "unknown-reason ws=alpha why=whim" text))
                      agent-repl-test-pr--errors))))

(ert-deftest agent-repl-test-pr-an-unknown-load-reason-errors-and-sends-nothing ()
  "A reason no load path names is a defect: recorded at ERROR and signalled."
  (agent-repl-test-pr--with
    ;; Act
    (should-error (agent-repl-page-report-loaded "alpha" 'whim))
    ;; Assert
    (should-not agent-repl-test-pr--sent)
    (should (seq-some (lambda (text) (string-search "unknown-reason ws=alpha why=whim" text))
                      agent-repl-test-pr--errors))))

(ert-deftest agent-repl-test-pr-an-unknown-unload-reason-errors-and-sends-nothing ()
  "A reason no unload path names is a defect: recorded at ERROR and signalled."
  (agent-repl-test-pr--with
    ;; Act
    (should-error (agent-repl-page-report-unloaded "alpha" 'whim))
    ;; Assert
    (should-not agent-repl-test-pr--sent)
    (should agent-repl-test-pr--errors)))

;;;; ---- One report in flight per workspace ----

(ert-deftest agent-repl-test-pr-a-report-waits-for-the-one-in-flight ()
  "A second report of a workspace is not sent while the first is unanswered.
2026-11-25 12:33: a load and the release's unload were in flight at once and
the load landed last, so the daemon counted a page no web view held."
  (agent-repl-test-pr--with
    ;; Arrange
    (agent-repl-page-report-loaded "alpha" 'allocated)
    ;; Act
    (agent-repl-page-report-unloaded "alpha" 'released)
    ;; Assert
    (should (equal agent-repl-test-pr--sent '(("alpha" (:arm :loaded :value :allocated)))))))

(ert-deftest agent-repl-test-pr-an-answered-report-sends-the-next-in-order ()
  "Once the report in flight is answered, the waiting reports go one at a time, oldest first."
  (agent-repl-test-pr--with
    ;; Arrange
    (agent-repl-page-report-loaded "alpha" 'allocated)
    (agent-repl-page-report-unloaded "alpha" 'released)
    (agent-repl-page-report-restate "alpha")
    ;; Act
    (agent-repl-test-pr--answer :success)
    ;; Assert
    (should (equal (reverse agent-repl-test-pr--sent)
                   '(("alpha" (:arm :loaded :value :allocated))
                     ("alpha" (:arm :unloaded :value :released)))))))

(ert-deftest agent-repl-test-pr-a-failed-report-still-sends-the-next ()
  "A report the daemon never took (a refusal, a transport failure) frees its workspace's line."
  (agent-repl-test-pr--with
    ;; Arrange
    (agent-repl-page-report-loaded "alpha" 'allocated)
    (agent-repl-page-report-unloaded "alpha" 'released)
    ;; Act
    (agent-repl-test-pr--answer :not-sent)
    ;; Assert
    (should (equal (car agent-repl-test-pr--sent) '("alpha" (:arm :unloaded :value :released))))))

(ert-deftest agent-repl-test-pr-the-last-answer-leaves-no-line ()
  "Once every report of a workspace is answered, no report stands in flight for it."
  (agent-repl-test-pr--with
    ;; Arrange
    (agent-repl-page-report-loaded "alpha" 'allocated)
    ;; Act
    (agent-repl-test-pr--answer :success)
    (agent-repl-page-report-unloaded "alpha" 'released)
    ;; Assert
    (should (equal (car agent-repl-test-pr--sent) '("alpha" (:arm :unloaded :value :released))))
    (agent-repl-test-pr--answer :success)
    (should (zerop (hash-table-count agent-repl-page-report--lines)))))

(ert-deftest agent-repl-test-pr-workspaces-do-not-wait-on-each-other ()
  "A report in flight holds back only its own workspace's reports."
  (agent-repl-test-pr--with
    ;; Arrange
    (agent-repl-page-report-loaded "alpha" 'allocated)
    ;; Act
    (agent-repl-page-report-loaded "beta" 'shown)
    ;; Assert
    (should (equal (car agent-repl-test-pr--sent) '("beta" (:arm :loaded :value :shown))))))

(ert-deftest agent-repl-test-pr-a-workspace-with-no-ref-is-sent-at-once-on-no-line ()
  "A workspace with no daemon ref goes straight to the sender, which skips it, and opens no line."
  (agent-repl-test-pr--with
    ;; Act
    (agent-repl-page-report-loaded "unreffed" 'allocated)
    (agent-repl-page-report-unloaded "unreffed" 'released)
    ;; Assert
    (should (equal (length agent-repl-test-pr--sent) 2))
    (should (zerop (hash-table-count agent-repl-page-report--lines)))))

;;;; ---- Restatements ----

(ert-deftest agent-repl-test-pr-a-held-page-is-restated-held-with-its-reason ()
  "A subscribe restates a held page with the reason the page holds."
  (agent-repl-test-pr--with
    ;; Arrange
    (push '("alpha" . slot) agent-repl-test-pr--pages)
    ;; Act
    (agent-repl-page-report-restate "alpha")
    ;; Assert
    (should (equal (car agent-repl-test-pr--sent) '("alpha" (:arm :held :value :slot))))))

(ert-deftest agent-repl-test-pr-a-page-held-before-reasons-is-restated-under-that-reason ()
  "A page converted with no load rule to explain it restates held, never an ERROR."
  (agent-repl-test-pr--with
    ;; Arrange
    (push '("alpha" . held-before-reasons) agent-repl-test-pr--pages)
    ;; Act
    (agent-repl-page-report-restate "alpha")
    ;; Assert
    (should (equal agent-repl-test-pr--sent '(("alpha" (:arm :held :value :held-before-reasons)))))
    (should-not agent-repl-test-pr--errors)))

(ert-deftest agent-repl-test-pr-a-workspace-with-no-page-is-restated-not-held ()
  "A subscribe of a workspace whose slot holds no page restates not-held."
  (agent-repl-test-pr--with
    ;; Act
    (agent-repl-page-report-restate "alpha")
    ;; Assert
    (should (equal agent-repl-test-pr--sent '(("alpha" (:arm :not-held :value nil)))))))

(ert-deftest agent-repl-test-pr-a-held-page-with-no-reason-is-an-error-and-restates-nothing ()
  "A page held that no load path loaded breaks the invariant: ERROR, nothing sent."
  (agent-repl-test-pr--with
    ;; Arrange
    (push '("alpha" . nil) agent-repl-test-pr--pages)
    ;; Act
    (agent-repl-page-report-restate "alpha")
    ;; Assert
    (should-not agent-repl-test-pr--sent)
    (should (seq-some (lambda (text) (string-search "held-without-reason ws=alpha" text))
                      agent-repl-test-pr--errors))))

;;;; ---- A stored reason the running build no longer names ----

(ert-deftest agent-repl-test-pr-a-page-held-under-a-retired-reason-is-restated-held-before-reasons ()
  "A page whose stored reason the running build no longer names restates `held-before-reasons'.
2026-11-25 12:33: a page held as `awake' before the rename to `allocated' was
restated under `awake', and the encoder signalled a contract breach inside the
host stream's open handler.  The web host's own table answers here, unstubbed."
  (agent-repl-test--with-clean-state
    (let ((sent nil)
          (errors nil))
      ;; Arrange
      (puthash "id-alpha" (list :url "http://ws-alpha.localhost:1/" :why 'retired-reason) agent-repl--webhost-pages)
      (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (ws) (list :id (concat "id-" ws))))
                ((symbol-function 'agent-repl-host--send-subject-verb)
                 (lambda (ws _op _rpc _on-success _on-not-sent &optional extra)
                   (push (list ws (plist-get extra :report)) sent)))
                ((symbol-function 'agent-repl--emit-error) (agent-repl-test--record-into errors)))
        ;; Act
        (agent-repl-page-report-restate "alpha"))
      ;; Assert
      (should (equal sent '(("alpha" (:arm :held :value :held-before-reasons)))))
      (should-not errors))))

;;; test-page-report.el ends here
