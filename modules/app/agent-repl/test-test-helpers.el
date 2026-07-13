;;; test-test-helpers.el --- ERT tests for test-helpers.el's batch-only contract -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the interactive-session safety gates in test-helpers.el.
;;
;; test-helpers.el is batch-only scaffolding; loaded into a live
;; interactive Emacs it must be an inert, loudly-announced no-op.  Each
;; test here simulates an interactive session by let-binding
;; `noninteractive' to nil and re-loading test-helpers.el (or invoking
;; the guard machinery directly), then asserts that exactly one
;; dangerous side effect stayed un-fired.
;;
;; The re-load is cheap in the interactive-simulated case because the
;; module reload (the expensive part) is itself one of the gated side
;; effects.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-test-helpers.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(defconst agent-repl-test-helpers--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing test-helpers.el, for re-load tests.")

(defun agent-repl-test-helpers--reload ()
  "Re-load test-helpers.el under the ambient dynamic environment."
  (load (expand-file-name "test-helpers.el" agent-repl-test-helpers--dir)
        nil t))

(defmacro agent-repl-test-helpers--with-interactive-reload (&rest body)
  "Re-load test-helpers.el with `noninteractive' bound to nil, then run BODY.
Silences the interactive-load `display-warning' so batch output stays
clean; BODY runs after the load with the same bindings still active."
  (declare (indent 0))
  `(let ((noninteractive nil))
     (cl-letf (((symbol-function 'display-warning) (lambda (&rest _args) nil)))
       (agent-repl-test-helpers--reload))
     ,@body))

;;;; ---- Interactive load is inert ----

(ert-deftest agent-repl-test-helpers-interactive-load-skips-guard-install ()
  "Interactive load must not fset boundary wrappers to guards."
  ;; Arrange: plant a marker impl on one registered wrapper.
  (let* ((sym 'agent-repl--frontend-http-request)
         (marker (lambda (&rest _args) 'marker-result))
         (saved (symbol-function sym)))
    (unwind-protect
        (progn
          (fset sym marker)
          ;; Act: interactive re-load with install state cleared.
          (let ((agent-repl-test--external-guards-installed nil)
                (agent-repl-test--external-original-functions nil))
            (agent-repl-test-helpers--with-interactive-reload))
          ;; Assert: the marker survived — no guard was installed.
          (should (eq (symbol-function sym) marker)))
      (fset sym saved))))

(ert-deftest agent-repl-test-helpers-interactive-load-preserves-log-to-file ()
  "Interactive load must not disable `agent-repl-log-to-file'."
  ;; Arrange / Act
  (let ((agent-repl-log-to-file t))
    (agent-repl-test-helpers--with-interactive-reload
      ;; Assert
      (should agent-repl-log-to-file))))

(ert-deftest agent-repl-test-helpers-interactive-load-preserves-state-dir-env ()
  "Interactive load must not redirect AGENT_REPL_STATE_DIR."
  ;; Arrange: a sentinel value in a let-bound copy of the environment.
  (let ((process-environment
         (cons "AGENT_REPL_STATE_DIR=/sentinel-state-dir" process-environment)))
    ;; Act
    (agent-repl-test-helpers--with-interactive-reload
      ;; Assert
      (should (equal (getenv "AGENT_REPL_STATE_DIR") "/sentinel-state-dir")))))

(ert-deftest agent-repl-test-helpers-interactive-load-adds-no-merge-advice ()
  "Interactive load must not advise `agent-repl--workspace-merge-async'."
  ;; Arrange: record every advice-add fired during the load.
  (let ((calls nil))
    (cl-letf (((symbol-function 'advice-add)
               (lambda (&rest args) (push args calls))))
      ;; Act
      (agent-repl-test-helpers--with-interactive-reload))
    ;; Assert
    (should-not (assq 'agent-repl--workspace-merge-async calls))))

(ert-deftest agent-repl-test-helpers-interactive-load-adds-no-defer-advice ()
  "Interactive load must not advise `agent-repl--defer-to-main-thread'."
  ;; Arrange
  (let ((calls nil))
    (cl-letf (((symbol-function 'advice-add)
               (lambda (&rest args) (push args calls))))
      ;; Act
      (agent-repl-test-helpers--with-interactive-reload))
    ;; Assert
    (should-not (assq 'agent-repl--defer-to-main-thread calls))))

(ert-deftest agent-repl-test-helpers-interactive-load-skips-module-reload ()
  "Interactive load must not re-load the production module (config.el)."
  ;; Arrange: any module file logging during the load proves a reload.
  (let ((log-calls nil))
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (&rest args) (push args log-calls))))
      ;; Act
      (agent-repl-test-helpers--with-interactive-reload))
    ;; Assert
    (should (null log-calls))))

(ert-deftest agent-repl-test-helpers-interactive-load-warns ()
  "Interactive load must announce itself via `display-warning'."
  ;; Arrange
  (let ((warnings nil)
        (noninteractive nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (type message &rest _) (push (cons type message) warnings))))
      ;; Act
      (agent-repl-test-helpers--reload))
    ;; Assert
    (should (assq 'agent-repl-test warnings))))

(ert-deftest agent-repl-test-helpers-interactive-load-skips-aaa-registration ()
  "Interactive load must not register the AAA sanity ert-deftest."
  ;; Arrange
  (let ((agent-repl-test--AAA-test-registered nil))
    ;; Act
    (agent-repl-test-helpers--with-interactive-reload
      ;; Assert: the registration flag was never flipped.
      (should-not agent-repl-test--AAA-test-registered))))

;;;; ---- Guard machinery behavior per session type ----

(ert-deftest agent-repl-test-helpers-install-guards-refuses-interactive ()
  "`agent-repl-test--install-external-guards' must refuse outside batch."
  ;; Arrange
  (let ((noninteractive nil)
        (agent-repl-test--external-guards-installed nil)
        (agent-repl-test--external-original-functions nil))
    ;; Act / Assert
    (should-error (agent-repl-test--install-external-guards))))

(ert-deftest agent-repl-test-helpers-guard-errors-in-batch ()
  "A guarded wrapper invoked unmocked in batch must signal, as always."
  ;; Arrange: ambient batch session; wrapper carries the installed guard.
  ;; Act / Assert
  (let ((err (should-error
              (agent-repl--frontend-http-request "GET" "http://x" nil))))
    (should (string-match-p "EXTERNAL BOUNDARY UNMOCKED"
                            (error-message-string err)))))

(ert-deftest agent-repl-test-helpers-guard-interactive-passthrough ()
  "A leaked guard invoked interactively must delegate to the original."
  ;; Arrange: fake captured original for one wrapper.
  (let* ((fake (lambda (&rest args) (cons 'fake-result args)))
         (noninteractive nil)
         (agent-repl-test--external-original-functions
          (list (cons 'agent-repl--frontend-http-request fake))))
    (cl-letf (((symbol-function 'display-warning) (lambda (&rest _args) nil)))
      ;; Act / Assert
      (should (equal (agent-repl--frontend-http-request "GET" "http://x" nil)
                     '(fake-result "GET" "http://x" nil))))))

(ert-deftest agent-repl-test-helpers-guard-interactive-passthrough-warns ()
  "The interactive passthrough must warn so the leak is visible."
  ;; Arrange
  (let* ((warnings nil)
         (noninteractive nil)
         (agent-repl-test--external-original-functions
          (list (cons 'agent-repl--frontend-http-request
                      (lambda (&rest _args) nil)))))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (type message &rest _) (push (cons type message) warnings))))
      ;; Act
      (agent-repl--frontend-http-request "GET" "http://x" nil))
    ;; Assert
    (let ((warning (assq 'agent-repl-test warnings)))
      (should warning)
      (should (string-match-p "agent-repl--frontend-http-request" (cdr warning))))))

(ert-deftest agent-repl-test-helpers-guard-interactive-missing-original-errors ()
  "A leaked guard with no captured original must still signal, not return nil."
  ;; Arrange
  (let ((noninteractive nil)
        (agent-repl-test--external-original-functions nil))
    ;; Act / Assert
    (should-error (agent-repl--frontend-http-request "GET" "http://x" nil))))

(ert-deftest agent-repl-test-helpers-reinstall-rearms-a-redefined-wrapper ()
  "Re-installing the guards re-arms a wrapper a production re-load re-`defun'-ed."
  ;; Arrange: simulate a production re-load putting the real impl back.
  (let ((guard (symbol-function 'agent-repl--frontend-http-request)))
    (unwind-protect
        (progn
          (fset 'agent-repl--frontend-http-request (lambda (&rest _args) 'real-impl))
          ;; Act
          (agent-repl-test--reinstall-external-guards)
          ;; Assert: the guard is back, so the boundary errors instead of running.
          (should-error (agent-repl--frontend-http-request "GET" "http://x" nil)))
      (fset 'agent-repl--frontend-http-request guard))))

(ert-deftest agent-repl-test-helpers-reinstall-keeps-captured-original-real ()
  "Re-installing leaves the captured original as the REAL impl, not a guard."
  ;; Arrange
  (let ((guard (symbol-function 'agent-repl--frontend-http-request))
        (before (cdr (assq 'agent-repl--frontend-http-request
                           agent-repl-test--external-original-functions))))
    (unwind-protect
        (progn
          ;; Act
          (agent-repl-test--reinstall-external-guards)
          ;; Assert
          (should (eq before
                      (cdr (assq 'agent-repl--frontend-http-request
                                 agent-repl-test--external-original-functions)))))
      (fset 'agent-repl--frontend-http-request guard))))

(provide 'test-test-helpers)

;;; test-test-helpers.el ends here
