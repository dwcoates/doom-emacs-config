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
  (let* ((sym 'agent-repl--launchctl-call)
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

(ert-deftest agent-repl-test-helpers-interactive-load-preserves-log-level-env ()
  "Interactive load must not override AGENT_REPL_LOG_LEVEL."
  ;; Arrange: a sentinel value in a let-bound copy of the environment.
  (let ((process-environment
         (cons "AGENT_REPL_LOG_LEVEL=warn" process-environment)))
    ;; Act
    (agent-repl-test-helpers--with-interactive-reload
      ;; Assert
      (should (equal (getenv "AGENT_REPL_LOG_LEVEL") "warn")))))

(ert-deftest agent-repl-test-helpers-batch-log-level-enables-debug-files ()
  "Batch module reloads must retain the harness's debug file threshold."
  ;; Arrange / Act: loading test-helpers.el established the batch environment.
  ;; Assert
  (should (equal (getenv "AGENT_REPL_LOG_LEVEL") "debug"))
  (should (eq (agent-repl--log-level-from-environment) 'debug)))

(ert-deftest agent-repl-test-helpers-batch-log-level-clock-holds-the-arm-instant ()
  "The batch window is read at the instant it was armed, however long the process runs."
  ;; Arrange: the harness armed the window to end 300s after its instant.
  (let ((until (string-to-number (getenv "AGENT_REPL_LOG_LEVEL_UNTIL"))))
    ;; Act / Assert
    (should (= (funcall agent-repl--log-level-clock) (- until 300)))))

(ert-deftest agent-repl-test-helpers-batch-global-log-is-process-isolated ()
  "Each batch process writes load-time records inside its PID-scoped state dir."
  (let ((state-dir (file-name-as-directory (getenv "AGENT_REPL_STATE_DIR"))))
    (should (string-prefix-p state-dir
                             (expand-file-name agent-repl-log-file-name)))
    (should (string-match-p (format "agent-repl-test-state-%d" (emacs-pid))
                            agent-repl-log-file-name))))

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

;;;; ---- Background-priority gate ----

(ert-deftest agent-repl-test-helpers-background-gate-refuses-unset-marker ()
  "A batch run with no background-priority marker is refused."
  ;; Arrange / Act / Assert
  (should-error (agent-repl-test--require-background-priority nil t)))

(ert-deftest agent-repl-test-helpers-background-gate-refuses-empty-marker ()
  "A batch run whose marker is the empty string is refused."
  ;; Arrange / Act / Assert
  (should-error (agent-repl-test--require-background-priority "" t)))

(ert-deftest agent-repl-test-helpers-background-gate-names-the-helper ()
  "The refusal tells the reader how to run the suite: through bin/background.sh."
  ;; Arrange / Act
  (let ((err (should-error (agent-repl-test--require-background-priority nil t))))
    ;; Assert
    (should (string-match-p "bin/background\\.sh" (error-message-string err)))))

(ert-deftest agent-repl-test-helpers-background-gate-admits-marked-batch-run ()
  "A batch run carrying the marker bin/background.sh exports is admitted."
  ;; Arrange / Act / Assert
  (should-not (agent-repl-test--require-background-priority "nice-19" t)))

(ert-deftest agent-repl-test-helpers-background-gate-ignores-interactive-load ()
  "An interactive load is already inert, so it is never refused."
  ;; Arrange / Act / Assert
  (should-not (agent-repl-test--require-background-priority nil nil)))

(ert-deftest agent-repl-test-helpers-background-gate-ran-for-this-run ()
  "This very batch run reached its tests only because it carried the marker."
  ;; Arrange / Act
  (let ((marker (getenv "AGENT_REPL_BACKGROUND_PRIORITY")))
    ;; Assert
    (should (and marker (not (string-empty-p marker))))))

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
              (agent-repl--launchctl-call "list"))))
    (should (string-match-p "EXTERNAL BOUNDARY UNMOCKED"
                            (error-message-string err)))))

(ert-deftest agent-repl-test-helpers-guard-interactive-passthrough ()
  "A leaked guard invoked interactively must delegate to the original."
  ;; Arrange: fake captured original for one wrapper.
  (let* ((fake (lambda (&rest args) (cons 'fake-result args)))
         (noninteractive nil)
         (agent-repl-test--external-original-functions
          (list (cons 'agent-repl--launchctl-call fake))))
    (cl-letf (((symbol-function 'display-warning) (lambda (&rest _args) nil)))
      ;; Act / Assert
      (should (equal (agent-repl--launchctl-call "list")
                     '(fake-result "list"))))))

(ert-deftest agent-repl-test-helpers-guard-interactive-passthrough-warns ()
  "The interactive passthrough must warn so the leak is visible."
  ;; Arrange
  (let* ((warnings nil)
         (noninteractive nil)
         (agent-repl-test--external-original-functions
          (list (cons 'agent-repl--launchctl-call
                      (lambda (&rest _args) nil)))))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (type message &rest _) (push (cons type message) warnings))))
      ;; Act
      (agent-repl--launchctl-call "list"))
    ;; Assert
    (let ((warning (assq 'agent-repl-test warnings)))
      (should warning)
      (should (string-match-p "agent-repl--launchctl-call" (cdr warning))))))

(ert-deftest agent-repl-test-helpers-guard-interactive-missing-original-errors ()
  "A leaked guard with no captured original must still signal, not return nil."
  ;; Arrange
  (let ((noninteractive nil)
        (agent-repl-test--external-original-functions nil))
    ;; Act / Assert
    (should-error (agent-repl--launchctl-call "list"))))

(ert-deftest agent-repl-test-helpers-reinstall-rearms-a-redefined-wrapper ()
  "Re-installing the guards re-arms a wrapper a production re-load re-`defun'-ed."
  ;; Arrange: simulate a production re-load putting the real impl back.
  (let ((guard (symbol-function 'agent-repl--launchctl-call)))
    (unwind-protect
        (progn
          (fset 'agent-repl--launchctl-call (lambda (&rest _args) 'real-impl))
          ;; Act
          (agent-repl-test--reinstall-external-guards)
          ;; Assert: the guard is back, so the boundary errors instead of running.
          (should-error (agent-repl--launchctl-call "list")))
      (fset 'agent-repl--launchctl-call guard))))

(ert-deftest agent-repl-test-helpers-reinstall-keeps-captured-original-real ()
  "Re-installing leaves the captured original as the REAL impl, not a guard."
  ;; Arrange
  (let ((guard (symbol-function 'agent-repl--launchctl-call))
        (before (cdr (assq 'agent-repl--launchctl-call
                           agent-repl-test--external-original-functions))))
    (unwind-protect
        (progn
          ;; Act
          (agent-repl-test--reinstall-external-guards)
          ;; Assert
          (should (eq before
                      (cdr (assq 'agent-repl--launchctl-call
                                 agent-repl-test--external-original-functions)))))
      (fset 'agent-repl--launchctl-call guard))))

;;;; ---- Generated protocol vocabulary readers ----

(ert-deftest agent-repl-test-helpers-generated-go-text-signals-when-absent ()
  "A missing generated binding signals: silently reading nothing would make
every vocabulary assertion built on it pass vacuously."
  ;; Act / Assert
  (should-error (agent-repl-test--generated-go-text "frontend/v1/nope.pb.go")))

(ert-deftest agent-repl-test-helpers-generated-oneof-arms-reads-a-json-name ()
  "The reader recovers a multi-word arm's lowerCamelCase protojson name."
  ;; Act
  (let ((arms (agent-repl-test--generated-oneof-arms
               "frontend/v1/sidebar.pb.go" "RosterRow")))
    ;; Assert
    (should (member "idleAsync" arms))))

(ert-deftest agent-repl-test-helpers-generated-oneof-arms-reads-a-bare-name ()
  "A single-word arm has no `json=' half, so its `name=' half must be read."
  ;; Act
  (let ((arms (agent-repl-test--generated-oneof-arms
               "frontend/v1/sidebar.pb.go" "RosterRow")))
    ;; Assert
    (should (member "submitting" arms))))

(ert-deftest agent-repl-test-helpers-generated-oneof-arms-are-message-scoped ()
  "Arms are read per message: a sibling message's arm is not reported here."
  ;; Act
  (let ((arms (agent-repl-test--generated-oneof-arms
               "frontend/v1/sidebar.pb.go" "RosterRowWhen")))
    ;; Assert
    (should (member "turnRunning" arms))
    (should-not (member "idleAsync" arms))))

(ert-deftest agent-repl-test-helpers-generated-enum-names-reads-a-value-name ()
  "The enum reader recovers a prefixed value name from the generated bindings."
  ;; Act
  ;; PromptOrigin moved into conversation/v1 so replay resolvers can read
  ;; it; the shim/v1 copy is gone, and this fixture names where it lives.
  (let ((names (agent-repl-test--generated-enum-names
                "conversation/v1/prompt_origin.pb.go" "PROMPT_ORIGIN_")))
    ;; Assert
    (should (member "PROMPT_ORIGIN_USER_SENT" names))))

;;;; ---- the quit-deferral test helpers -----------------------------------

(ert-deftest agent-repl-test-helpers-quit-deferred-p-reports-an-armed-quit ()
  "A quit requested inside the body is reported as still armed."
  ;; Act / Assert
  (should (agent-repl-test--quit-deferred-p
            (setq quit-flag t))))

(ert-deftest agent-repl-test-helpers-quit-deferred-p-reports-no-quit ()
  "A body that never requests a quit is reported as having deferred none."
  ;; Act / Assert
  (should-not (agent-repl-test--quit-deferred-p
                (ignore))))

(ert-deftest agent-repl-test-helpers-quit-deferred-p-runs-body-under-inhibit-quit ()
  "The body runs with quitting inhibited, which is what defers the quit."
  ;; Arrange
  (let (observed)
    ;; Act
    (agent-repl-test--quit-deferred-p (setq observed inhibit-quit))
    ;; Assert
    (should observed)))

(ert-deftest agent-repl-test-helpers-quit-deferred-p-disarms-before-returning ()
  "The captured quit is cleared, so it cannot land on the caller."
  ;; Act
  (agent-repl-test--quit-deferred-p (setq quit-flag t))
  ;; Assert -- reaching here at all means no quit was taken on the way out.
  (should-not quit-flag))

(ert-deftest agent-repl-test-helpers-quit-deferred-p-disarms-past-a-signal ()
  "A body that signals still leaves no armed quit behind for the next test."
  ;; Act
  (ignore-errors
    (agent-repl-test--quit-deferred-p
      (setq quit-flag t)
      (error "boom")))
  ;; Assert
  (should-not quit-flag))

(ert-deftest agent-repl-test-helpers-with-pending-quit-arms-before-the-body ()
  "The body observes the quit as already requested when it starts."
  ;; Arrange
  (let (observed)
    ;; Act
    (agent-repl-test--with-pending-quit (setq observed quit-flag))
    ;; Assert
    (should observed)))

(ert-deftest agent-repl-test-helpers-with-pending-quit-returns-the-body-value ()
  "The body's value is handed back, so the Act can be asserted on directly."
  ;; Act / Assert
  (should (equal (agent-repl-test--with-pending-quit 'settled) 'settled)))

(ert-deftest agent-repl-test-helpers-with-pending-quit-disarms-past-a-signal ()
  "A signalling body leaves no armed quit behind for the next test."
  ;; Act
  (ignore-errors
    (agent-repl-test--with-pending-quit (error "boom")))
  ;; Assert
  (should-not quit-flag))

(ert-deftest agent-repl-test-helpers-quit-deferral-sites-use-the-shared-macros ()
  "No suite hand-rolls the quit-deferral recipe beside the shared macros.
A hand-rolled site is the failure this asserts against: it is easy to
write one that forgets to disarm `quit-flag' inside the `inhibit-quit'
scope, and such a site passes while leaving a live quit for whichever
test runs next."
  ;; Arrange -- test-helpers.el is where the recipe is ALLOWED to appear.
  (let ((dir agent-repl-test--module-dir)
        (offenders nil))
    (dolist (file (directory-files dir t "\\`test-.*\\.el\\'"))
      (unless (member (file-name-nondirectory file)
                      '("test-helpers.el" "test-test-helpers.el"))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          ;; Act -- the recipe's own signature: binding `inhibit-quit' by hand
          ;; in a suite that also touches `quit-flag'.
          (when (and (save-excursion
                       (re-search-forward "(let ((inhibit-quit t))" nil t))
                     (save-excursion
                       (re-search-forward "\\_<quit-flag\\_>" nil t)))
            (push (file-name-nondirectory file) offenders)))))
    ;; Assert
    (should-not offenders)))

;;;; ---- the popup-recording test helper --------------------------------

(ert-deftest agent-repl-test-helpers-recording-popups-records-each-buffer ()
  "Every buffer handed to the popup inside the macro is recorded, newest first."
  ;; Arrange
  (with-temp-buffer
    (let ((first (current-buffer)))
      (with-temp-buffer
        (let ((second (current-buffer)))
          ;; Act
          (agent-repl-test--recording-popups
            (agent-repl-popup-show first)
            (agent-repl-popup-show second)
            ;; Assert
            (should (equal agent-repl-test--popups-shown (list second first)))))))))

(ert-deftest agent-repl-test-helpers-recording-popups-answers-a-window ()
  "The recorded popup answers the selected window, as a working popup does."
  (with-temp-buffer
    (agent-repl-test--recording-popups
      (should (eq (agent-repl-popup-show (current-buffer)) (selected-window))))))

(ert-deftest agent-repl-test-helpers-recording-popups-shows-the-buffer ()
  "The window the recorded popup answers shows the buffer it was handed."
  (let ((buffer (generate-new-buffer " *popup-shown*"))
        (before (window-buffer (selected-window))))
    (unwind-protect
        (agent-repl-test--recording-popups
          (should (eq (window-buffer (agent-repl-popup-show buffer)) buffer)))
      (set-window-buffer (selected-window) before)
      (kill-buffer buffer))))

(ert-deftest agent-repl-test-helpers-recording-popups-starts-empty ()
  "Each use starts with nothing recorded, whatever an earlier use saw."
  (with-temp-buffer
    (agent-repl-test--recording-popups (agent-repl-popup-show (current-buffer))))
  (agent-repl-test--recording-popups
    (should (null agent-repl-test--popups-shown))))

(ert-deftest agent-repl-test-helpers-popup-stubs-use-the-shared-macro ()
  "No suite but test-popup.el stubs `agent-repl-popup-show' by hand.
test-popup.el tests the popup module itself and records one level lower."
  ;; Arrange
  (let ((dir agent-repl-test--module-dir)
        (offenders nil))
    (dolist (file (directory-files dir t "\\`test-.*\\.el\\'"))
      (unless (member (file-name-nondirectory file)
                      '("test-helpers.el" "test-test-helpers.el" "test-popup.el"))
        (with-temp-buffer
          (insert-file-contents file)
          ;; Act
          (when (search-forward "(symbol-function 'agent-repl-popup-show)" nil t)
            (push (file-name-nondirectory file) offenders)))))
    ;; Assert
    (should-not offenders)))

;;;; ---- The batch temp root ----

(ert-deftest agent-repl-test-helpers-batch-temp-files-land-in-the-private-root ()
  "In batch, `temporary-file-directory' is this process's private root."
  (should (equal temporary-file-directory
                 (file-name-as-directory agent-repl-test--temp-root))))

(ert-deftest agent-repl-test-helpers-batch-subprocesses-inherit-the-private-root ()
  "In batch, TMPDIR names the same private root, for every child process."
  (should (equal (getenv "TMPDIR") temporary-file-directory)))

(ert-deftest agent-repl-test-helpers-batch-temp-root-is-removed-with-its-contents ()
  "The exit hook removes the root and whatever a test left in it."
  ;; Arrange
  (let* ((root (make-temp-file "temp-root-test-" t))
         (agent-repl-test--temp-root root))
    (make-directory (expand-file-name "left/behind" root) t)
    ;; Act
    (agent-repl-test--delete-temp-root)
    ;; Assert
    (should-not (file-exists-p root))))

(ert-deftest agent-repl-test-helpers-batch-temp-root-removal-failure-is-printed ()
  "A root that cannot be removed is reported on stderr, not swallowed."
  ;; Arrange
  (let* ((root (make-temp-file "temp-root-test-" t))
         (agent-repl-test--temp-root root)
         (printed ""))
    (unwind-protect
        (cl-letf (((symbol-function 'delete-directory)
                   (lambda (&rest _) (signal 'file-error (list "Removing directory" "Permission denied" root))))
                  ((symbol-function 'princ)
                   (lambda (object &optional _stream) (setq printed (concat printed object)))))
          ;; Act
          (agent-repl-test--delete-temp-root))
      (delete-directory root t))
    ;; Assert
    (should (string-match-p "ERROR: could not remove the batch temp root" printed))))

(ert-deftest agent-repl-test-helpers-batch-temp-root-removal-is-hooked-on-exit ()
  "The removal runs when Emacs exits."
  (should (memq #'agent-repl-test--delete-temp-root kill-emacs-hook)))

(ert-deftest agent-repl-test-helpers-batch-temp-root-survives-a-reload ()
  "Re-loading test-helpers.el keeps the one root rather than nesting another."
  ;; Arrange
  (let ((before temporary-file-directory))
    ;; Act
    (load (expand-file-name "test-helpers.el" agent-repl-test-helpers--dir) nil t)
    ;; Assert
    (should (equal temporary-file-directory before))))

;;;; ---- Reaping a subprocess with its children ----

(defun agent-repl-test-helpers--spawn-parent-of-sleep ()
  "Spawn a shell that backgrounds a `sleep' child; return (PROCESS . CHILD-PID)."
  (let* ((buffer (generate-new-buffer " *reap-test*"))
         (process (make-process :name "agent-repl-reap-test" :buffer buffer
                                :command (list "/bin/sh" "-c" "sleep 30 & echo $!; wait")
                                :connection-type 'pipe :noquery t)))
    (while (not (with-current-buffer buffer
                  (string-match-p "[0-9]+\n" (buffer-string))))
      (accept-process-output process 0.05))
    (cons process (string-to-number (with-current-buffer buffer (buffer-string))))))

(defun agent-repl-test-helpers--pid-gone-p (pid)
  "Return non-nil once PID is no longer a running process.
A zombie awaiting its reaper counts as gone: it holds no fds."
  (let ((attributes (process-attributes pid)))
    (or (null attributes) (equal (alist-get 'state attributes) "Z"))))

(ert-deftest agent-repl-test-helpers-reap-process-kills-the-child-too ()
  "Reaping a live shell takes its backgrounded child with it."
  ;; Arrange
  (let* ((spawned (agent-repl-test-helpers--spawn-parent-of-sleep))
         (process (car spawned))
         (child (cdr spawned))
         (deadline (+ (float-time) 2)))
    (unwind-protect
        (progn
          ;; Act
          (agent-repl-test--reap-process process)
          (while (and (not (agent-repl-test-helpers--pid-gone-p child))
                      (< (float-time) deadline))
            (accept-process-output nil 0.01))
          ;; Assert
          (should-not (process-live-p process))
          (should (agent-repl-test-helpers--pid-gone-p child)))
      (signal-process child 'SIGKILL)
      (kill-buffer (process-buffer process)))))

(ert-deftest agent-repl-test-helpers-reap-process-deletes-a-dead-process ()
  "A process that already exited is deleted without a signal."
  ;; Arrange
  (let ((process (make-process :name "agent-repl-reap-dead" :command '("/usr/bin/true")
                               :connection-type 'pipe :noquery t))
        (signalled nil))
    (while (process-live-p process) (accept-process-output process 0.05))
    (cl-letf (((symbol-function 'signal-process)
               (lambda (&rest args) (push args signalled) 0)))
      ;; Act
      (agent-repl-test--reap-process process))
    ;; Assert
    (should-not signalled)
    (should-not (memq process (process-list)))))

(ert-deftest agent-repl-test-helpers-reap-process-ignores-nil ()
  "No process at all is a no-op."
  ;; Act / Assert
  (should-not (agent-repl-test--reap-process nil)))

(ert-deftest agent-repl-test-helpers-subprocess-reaps-share-one-helper ()
  "Every subprocess reap in the shared harness goes through the group kill.
A site that hand-rolls `delete-process' on a subprocess orphans its children."
  ;; Arrange
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "test-integration-helpers.el" agent-repl-test-helpers--dir))
    ;; Act
    (let ((hand-rolled
           (cl-loop for name in '("agent-repl-daemon--build-process"
                                  "agent-repl--frontend-daemon-process")
                    when (progn (goto-char (point-min))
                                (search-forward (format "(delete-process %s)" name) nil t))
                    collect name))
          (helper (progn (goto-char (point-min))
                         (how-many (regexp-quote "(agent-repl-test--reap-process")))))
      ;; Assert
      (should-not hand-rolled)
      (should (= helper 3)))))

(provide 'test-test-helpers)

;;; test-test-helpers.el ends here

;;;; ---- The color contract's one path ----

(ert-deftest agent-repl-test-helpers-render-colors-path-names-the-checked-in-contract ()
  "`agent-repl-test--render-colors-path' names a readable render-colors.json."
  ;; Arrange / Act
  (let ((path agent-repl-test--render-colors-path))
    ;; Assert
    (should (equal (file-name-nondirectory path) "render-colors.json"))
    (should (file-readable-p path))))

(ert-deftest agent-repl-test-helpers-no-suite-spells-the-render-colors-path-itself ()
  "Every suite reads render-colors.json through the one helper path."
  ;; Arrange
  (let ((spelled nil))
    ;; Act
    (dolist (file (directory-files agent-repl-test--module-dir t "\\`test-.*\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (when (re-search-forward "(expand-file-name \"[./]*proto/vocab/render-colors\\.json\"" nil t)
          (unless (equal (file-name-nondirectory file) "test-helpers.el")
            (push (file-name-nondirectory file) spelled)))))
    ;; Assert
    (should-not spelled)))

(ert-deftest agent-repl-test-helpers-record-into-records-formatted-newest-first ()
  "A rung stubbed with the recorder keeps each record's text, newest first."
  ;; Arrange
  (let ((records nil))
    (cl-letf (((symbol-function 'agent-repl--info) (agent-repl-test--record-into records)))
      ;; Act
      (agent-repl--info "ws-1" "first n=%d" 1)
      (agent-repl--info nil "second s=%s" "x"))
    ;; Assert
    (should (equal records '("second s=x" "first n=1")))))

(ert-deftest agent-repl-test-helpers-fatal-into-records-then-signals ()
  "The fatal stand-in keeps the record's text, then signals it."
  ;; Arrange
  (let ((records nil))
    (cl-letf (((symbol-function 'agent-repl--fatal) (agent-repl-test--fatal-into records)))
      ;; Act
      (let ((err (should-error (agent-repl--fatal "ws-1" "broken n=%d" 1))))
        ;; Assert
        (should (equal (cadr err) "broken n=1"))))
    (should (equal records '("broken n=1")))))

(ert-deftest agent-repl-test-helpers-no-suite-hand-rolls-the-fatal-stand-in ()
  "Every suite stubs `agent-repl--fatal' with `agent-repl-test--fatal-into'.
test-spell.el records each rung with its level, a different shape, and
stubs every rung alike."
  ;; Arrange
  (let ((dir agent-repl-test-helpers--dir)
        (hand-rolled nil))
    ;; Act
    (dolist (file (directory-files dir t "\\`test-.*\\.el\\'"))
      (unless (member (file-name-nondirectory file) '("test-helpers.el" "test-test-helpers.el"))
        (with-temp-buffer
          (insert-file-contents file)
          (when (re-search-forward "(symbol-function 'agent-repl--fatal)[ \t\n]*(lambda (_" nil t)
            (push (file-name-nondirectory file) hand-rolled)))))
    ;; Assert
    (should-not hand-rolled)))

(ert-deftest agent-repl-test-helpers-no-suite-hand-rolls-the-recorder ()
  "Every suite records a logging rung through `agent-repl-test--record-into'."
  ;; Arrange
  (let ((dir agent-repl-test-helpers--dir)
        (hand-rolled nil))
    ;; Act
    (dolist (file (directory-files dir t "\\`test-.*\\.el\\'"))
      (unless (equal (file-name-nondirectory file) "test-helpers.el")
        (with-temp-buffer
          (insert-file-contents file)
          (when (re-search-forward
                 "(lambda (_[a-z]* fmt &rest args)[ \t\n]*(push (apply #'format fmt args) [^() \t\n]+))"
                 nil t)
            (push (file-name-nondirectory file) hand-rolled)))))
    ;; Assert
    (should-not hand-rolled)))
