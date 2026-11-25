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

(defmacro agent-repl-test-pr--with (&rest body)
  "Run BODY with the sender and the host stubbed."
  (declare (indent 0))
  `(agent-repl-test--with-clean-state
     (let ((agent-repl-test-pr--sent nil)
           (agent-repl-test-pr--pages nil)
           (agent-repl-test-pr--errors nil))
       (cl-letf (((symbol-function 'agent-repl-host--send-subject-verb)
                  (lambda (ws op rpc _on-success _on-not-sent &optional extra)
                    (should (equal op "page-state"))
                    (should (eq rpc #'agent-repl-rpc-report-page-state))
                    (push (list ws (plist-get extra :report)) agent-repl-test-pr--sent)))
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

;;; test-page-report.el ends here
