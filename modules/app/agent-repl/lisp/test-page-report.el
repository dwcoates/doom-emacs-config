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
  "Workspaces whose page the harness's host holds.")

(defvar agent-repl-test-pr--errors nil
  "Every ERROR record's text, newest first.")

(defmacro agent-repl-test-pr--with (&rest body)
  "Run BODY with the sender and the host stubbed."
  (declare (indent 0))
  `(agent-repl-test--with-clean-state
     (let ((agent-repl-test-pr--sent nil)
           (agent-repl-test-pr--pages nil)
           (agent-repl-test-pr--errors nil)
           (agent-repl-page-report--reasons (make-hash-table :test #'equal)))
       (cl-letf (((symbol-function 'agent-repl-host--send-subject-verb)
                  (lambda (ws op rpc _on-success _on-not-sent &optional extra)
                    (should (equal op "page-state"))
                    (should (eq rpc #'agent-repl-rpc-report-page-state))
                    (push (list ws (plist-get extra :report)) agent-repl-test-pr--sent)))
                 ((symbol-function 'agent-repl-webhost-has-page-p)
                  (lambda (ws) (and (member ws agent-repl-test-pr--pages) t)))
                 ((symbol-function 'agent-repl--emit-error)
                  (agent-repl-test--record-into agent-repl-test-pr--errors)))
         ,@body))))

;;;; ---- Changes ----

(ert-deftest agent-repl-test-pr-a-load-is-reported-with-its-reason ()
  "A load sends the loaded arm and keeps its reason for a restatement."
  (agent-repl-test-pr--with
    ;; Act
    (agent-repl-page-report-loaded "alpha" 'awake)
    ;; Assert
    (should (equal agent-repl-test-pr--sent '(("alpha" (:arm :loaded :value :awake)))))
    (should (eq (gethash "alpha" agent-repl-page-report--reasons) 'awake))))

(ert-deftest agent-repl-test-pr-an-unload-is-reported-with-its-reason ()
  "An unload sends the unloaded arm and forgets the load reason."
  (agent-repl-test-pr--with
    ;; Arrange
    (agent-repl-page-report-loaded "alpha" 'shown)
    ;; Act
    (agent-repl-page-report-unloaded "alpha" 'dormant)
    ;; Assert
    (should (equal (car agent-repl-test-pr--sent) '("alpha" (:arm :unloaded :value :dormant))))
    (should-not (gethash "alpha" agent-repl-page-report--reasons))))

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
  "A subscribe restates a held page with the reason of its last load."
  (agent-repl-test-pr--with
    ;; Arrange
    (agent-repl-page-report-loaded "alpha" 'slot)
    (push "alpha" agent-repl-test-pr--pages)
    ;; Act
    (agent-repl-page-report-restate "alpha")
    ;; Assert
    (should (equal (car agent-repl-test-pr--sent) '("alpha" (:arm :held :value :slot))))))

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
    (push "alpha" agent-repl-test-pr--pages)
    ;; Act
    (agent-repl-page-report-restate "alpha")
    ;; Assert
    (should-not agent-repl-test-pr--sent)
    (should (seq-some (lambda (text) (string-search "held-without-reason ws=alpha" text))
                      agent-repl-test-pr--errors))))

;;; test-page-report.el ends here
