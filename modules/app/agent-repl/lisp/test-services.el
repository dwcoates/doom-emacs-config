;;; test-services.el --- ERT tests for agent-repl services.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-services.el -f ert-run-tests-batch-and-exit
;;
;; NO build, NO service, NO daemon.  Every external boundary this file
;; reaches — the artifact probe and the build script — is registered in
;; `agent-repl--external-boundary-functions' and stubbed below; the
;; harness's guard fails any test that misses one.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Harness ----

(defvar agent-repl-test-services--build-failure nil
  "What the stubbed build answers: nil for success, a detail string else.")

(defvar agent-repl-test-services--artifacts-present t
  "What the stubbed artifact probe answers for every binary.")

(defvar agent-repl-test-services--builds nil
  "Build invocations, newest first: the TARGETS argument.")

(defvar agent-repl-test-services--daemon-stops 0
  "How many daemon stops were requested.")

(defvar agent-repl-test-services--daemon-stop-outcome '(:arm :accepted)
  "What the stubbed daemon stop reports to its continuation.")

(defvar agent-repl-test-services--daemon-departed t
  "What the stubbed daemon departure wait reports.")

(defvar agent-repl-test-services--daemon-current-identity nil
  "Identity observed before the stop.")

(defvar agent-repl-test-services--daemon-current-conn 'current-connection
  "The connection returned by the stubbed primary-link lookup.")

(defvar agent-repl-test-services--daemon-replacement-identity nil
  "Identity observed from the ensured replacement.")

(defvar agent-repl-test-services--daemon-ensures 0
  "How many daemon ensures were requested.")

(defvar agent-repl-test-services--daemon-conn 'the-connection
  "What the stubbed daemon ensure hands its continuation.")

(defvar agent-repl-test-services--teardowns 0
  "How many link teardowns ran.")

(defvar agent-repl-test-services--logs nil
  "Captured `(LEVEL . TEXT)' log entries, newest first.")

(defun agent-repl-test-services--logged-p (level substring)
  "Return non-nil when a LEVEL entry containing SUBSTRING was recorded."
  (seq-some (lambda (entry)
              (and (eq (car entry) level)
                   (string-search substring (cdr entry))))
            agent-repl-test-services--logs))

(defmacro agent-repl-test-services--with-harness (&rest body)
  "Run BODY with services.el's whole world faked and its state reset."
  (declare (indent 0))
  `(let ((agent-repl-test-services--build-failure nil)
         (agent-repl-test-services--artifacts-present t)
         (agent-repl-test-services--builds nil)
         (agent-repl-test-services--daemon-stops 0)
         (agent-repl-test-services--daemon-stop-outcome '(:arm :accepted))
         (agent-repl-test-services--daemon-departed t)
         (agent-repl-test-services--daemon-current-identity
          '(:instance-id "daemon-old" :pid 4101 :build-sha "old-build"))
         (agent-repl-test-services--daemon-current-conn 'current-connection)
         (agent-repl-test-services--daemon-replacement-identity
          '(:instance-id "daemon-new" :pid 4102 :build-sha "new-build"))
         (agent-repl-test-services--daemon-ensures 0)
         (agent-repl-test-services--daemon-conn 'the-connection)
         (agent-repl-test-services--teardowns 0)
         (agent-repl-test-services--logs nil))
     (cl-letf (((symbol-function 'agent-repl--frontend-artifact-exists-p)
                (lambda (_p) agent-repl-test-services--artifacts-present))
               ((symbol-function 'agent-repl-daemon--build)
                (lambda (targets continuation)
                  (push targets agent-repl-test-services--builds)
                  (funcall continuation agent-repl-test-services--build-failure)))
               ((symbol-function 'agent-repl-frontend-daemon-stop)
                (lambda (&optional on-done)
                  (setq agent-repl-test-services--daemon-stops
                        (1+ agent-repl-test-services--daemon-stops))
                  (when on-done
                    (funcall on-done agent-repl-test-services--daemon-stop-outcome))))
               ((symbol-function 'agent-repl-daemon--await-departure)
                (lambda (on-gone)
                  (funcall on-gone agent-repl-test-services--daemon-departed)))
               ((symbol-function 'agent-repl-daemon-ensure)
                (lambda (&optional on-ready)
                  (setq agent-repl-test-services--daemon-ensures
                        (1+ agent-repl-test-services--daemon-ensures))
                  (when on-ready (funcall on-ready agent-repl-test-services--daemon-conn))))
               ((symbol-function 'agent-repl-daemon-observe-identity)
                (lambda (conn on-answer on-failure)
                  (if (null conn)
                      (funcall on-failure "no daemon link is available")
                    (funcall on-answer
                             (if (eq conn 'current-connection)
                                 agent-repl-test-services--daemon-current-identity
                               agent-repl-test-services--daemon-replacement-identity)))))
               ((symbol-function 'agent-repl-link-primary)
                (lambda () agent-repl-test-services--daemon-current-conn))
               ((symbol-function 'agent-repl-link-teardown)
                (lambda ()
                  (setq agent-repl-test-services--teardowns
                        (1+ agent-repl-test-services--teardowns))))
               ((symbol-function 'agent-repl--assert-main-thread) (lambda (_what) nil))
               ((symbol-function 'agent-repl--backend-phase) (lambda (&rest _) nil))
               ((symbol-function 'message) (lambda (&rest _) nil))
               ((symbol-function 'agent-repl--log)
                (lambda (_ws fmt &rest args)
                  (push (cons :log (apply #'format fmt args))
                        agent-repl-test-services--logs)))
               ((symbol-function 'agent-repl--info)
                (lambda (_ws fmt &rest args)
                  (push (cons :info (apply #'format fmt args))
                        agent-repl-test-services--logs)))
               ((symbol-function 'agent-repl--warn)
                (lambda (_ws fmt &rest args)
                  (push (cons :warn (apply #'format fmt args))
                        agent-repl-test-services--logs)))
               ((symbol-function 'agent-repl--error)
                (lambda (_ws fmt &rest args)
                  (push (cons :error (apply #'format fmt args))
                        agent-repl-test-services--logs))))
       ,@body)))

;;;; ---- The services' build ----

(ert-deftest agent-repl-test-services-build-builds-only-the-service-targets ()
  "The services' build names the store, the sidecar and zettel, and nothing else."
  (agent-repl-test-services--with-harness
    ;; Arrange / Act
    (agent-repl--shim-services-build #'ignore #'ignore)
    ;; Assert
    (should (equal agent-repl-test-services--builds '(("store" "sidecar" "zettel"))))))

(ert-deftest agent-repl-test-services-build-succeeds-once-both-binaries-are-installed ()
  "A landed build with the store's and the sidecar's binaries is success."
  (agent-repl-test-services--with-harness
    ;; Arrange
    (let ((done nil) (failure :unset))
      ;; Act
      (agent-repl--shim-services-build (lambda () (setq done t))
                                       (lambda (detail) (setq failure detail)))
      ;; Assert
      (should done)
      (should (eq failure :unset)))))

(ert-deftest agent-repl-test-services-build-failure-reaches-the-continuation ()
  "The build's diagnostic is what the coordinator reports, logged at ERROR."
  (agent-repl-test-services--with-harness
    ;; Arrange
    (let ((done nil) (failure :unset))
      (setq agent-repl-test-services--build-failure "build failed (exit 2)")
      ;; Act
      (agent-repl--shim-services-build (lambda () (setq done t))
                                       (lambda (detail) (setq failure detail)))
      ;; Assert
      (should-not done)
      (should (equal failure "build failed (exit 2)"))
      (should (agent-repl-test-services--logged-p
               :error "elisp.services.build-failed detail=build failed (exit 2)")))))

(ert-deftest agent-repl-test-services-build-without-its-binaries-is-a-failure ()
  "A build that reports success and installed no binary is a broken build."
  (agent-repl-test-services--with-harness
    ;; Arrange
    (let ((done nil) (failure :unset))
      (setq agent-repl-test-services--artifacts-present nil)
      ;; Act
      (agent-repl--shim-services-build (lambda () (setq done t))
                                       (lambda (detail) (setq failure detail)))
      ;; Assert
      (should-not done)
      (should (string-match-p "completed without both binaries" failure))
      (should (agent-repl-test-services--logged-p
               :error "elisp.services.build-artifacts-missing")))))

(ert-deftest agent-repl-test-services-build-needs-callable-continuations ()
  "A continuation that is not a function is a caller's bug, refused at once."
  (agent-repl-test-services--with-harness
    ;; Arrange / Act / Assert
    (should-error (agent-repl--shim-services-build nil #'ignore))
    (should (null agent-repl-test-services--builds))))

(ert-deftest agent-repl-test-services-emacs-drives-no-service-manager ()
  "The daemon restarts the services; this file keeps no launchctl of its own."
  ;; Arrange / Act / Assert
  (dolist (retired '(agent-repl--launchctl-call
                     agent-repl--shim-services-launchctl
                     agent-repl--shim-services-assert-launchd-loaded
                     agent-repl--shim-services-build-and-bounce
                     agent-repl--shim-service-needs-bounce-p
                     agent-repl--shim-store-after-ready))
    (should-not (fboundp retired))))

;;;; ---- The coordinated runtime restart ----

(ert-deftest agent-repl-test-services-runtime-restart-builds-the-whole-stack ()
  "The daemon's own build comes before the services', and covers everything."
  (agent-repl-test-services--with-harness
    ;; Arrange / Act
    (agent-repl--runtime-prepare #'ignore #'ignore)
    ;; Assert
    (should (member nil agent-repl-test-services--builds))))

(ert-deftest agent-repl-test-services-runtime-restart-builds-the-services-after-the-stack ()
  "The services' binaries are installed before the daemon is replaced."
  (agent-repl-test-services--with-harness
    ;; Arrange / Act
    (agent-repl--runtime-prepare #'ignore #'ignore)
    ;; Assert: newest first, so the services' build came second.
    (should (equal agent-repl-test-services--builds
                   '(("store" "sidecar" "zettel") nil)))))

(ert-deftest agent-repl-test-services-runtime-restart-without-the-binaries-stops-nothing ()
  "A services build that installed nothing must not take the daemon down."
  (agent-repl-test-services--with-harness
    ;; Arrange
    (let ((failure :unset))
      (setq agent-repl-test-services--artifacts-present nil)
      ;; Act
      (agent-repl--runtime-prepare #'ignore (lambda (detail) (setq failure detail)))
      ;; Assert
      (should (string-match-p "completed without both binaries" failure))
      (should (= agent-repl-test-services--daemon-stops 0)))))

(ert-deftest agent-repl-test-services-runtime-restart-asks-the-daemon-to-stop ()
  "EMACS NEVER KILLS A DAEMON, not even during its own restart."
  (agent-repl-test-services--with-harness
    ;; Arrange / Act
    (agent-repl--runtime-prepare #'ignore #'ignore)
    ;; Assert
    (should (= agent-repl-test-services--daemon-stops 1))))

(ert-deftest agent-repl-test-services-runtime-restart-tears-the-link-down ()
  "The stopped daemon's link must not survive as a corpse to send over."
  (agent-repl-test-services--with-harness
    ;; Arrange / Act
    (agent-repl--runtime-prepare #'ignore #'ignore)
    ;; Assert
    (should (= agent-repl-test-services--teardowns 1))))

(ert-deftest agent-repl-test-services-runtime-restart-ensures-a-replacement ()
  "The ensure is what brings the fresh daemon up."
  (agent-repl-test-services--with-harness
    ;; Arrange / Act
    (agent-repl--runtime-prepare #'ignore #'ignore)
    ;; Assert
    (should (= agent-repl-test-services--daemon-ensures 1))))

(ert-deftest agent-repl-test-services-runtime-restart-completes ()
  "A newly identified daemon process is terminal restart completion."
  (agent-repl-test-services--with-harness
    ;; Arrange
    (let ((done nil))
      ;; Act
      (agent-repl--runtime-prepare (lambda () (setq done t)) #'ignore)
      ;; Assert
      (should done))))

(ert-deftest agent-repl-test-services-runtime-restart-rejects-the-same-instance-after-a-link-cycle ()
  "A down/up link cycle to one daemon process is not a completed restart."
  (agent-repl-test-services--with-harness
    ;; Arrange.
    (let ((done nil)
          (failure nil))
      (setq agent-repl-test-services--daemon-replacement-identity
            agent-repl-test-services--daemon-current-identity)
      ;; Act.
      (agent-repl--runtime-prepare (lambda () (setq done t))
                                   (lambda (detail) (setq failure detail)))
      ;; Assert.
      (should-not done)
      (should (string-match-p "not restarted: daemon instance daemon-old still serves"
                              failure)))))

(ert-deftest agent-repl-test-services-runtime-restart-surfaces-a-deferred-daemon-reason ()
  "A daemon-side deferral is a reasoned non-restart, never completion."
  (agent-repl-test-services--with-harness
    ;; Arrange.
    (let ((failure nil))
      (setq agent-repl-test-services--daemon-stop-outcome
            '(:arm :not-restarted
              :reason "shutdown deferred until workspace alpha is free"))
      ;; Act.
      (agent-repl--runtime-prepare #'ignore
                                   (lambda (detail) (setq failure detail)))
      ;; Assert.
      (should (equal failure
                     "not restarted: shutdown deferred until workspace alpha is free"))
      (should (= agent-repl-test-services--teardowns 0))
      (should (= agent-repl-test-services--daemon-ensures 0)))))

(ert-deftest agent-repl-test-services-runtime-restart-without-a-link-mutates-nothing ()
  "A missing daemon link fails before any build or service mutation begins."
  (agent-repl-test-services--with-harness
    ;; Arrange.
    (let ((failure nil))
      (setq agent-repl-test-services--daemon-current-conn nil)
      ;; Act.
      (agent-repl--runtime-prepare #'ignore
                                   (lambda (detail) (setq failure detail)))
      ;; Assert.
      (should (equal failure
                     "not restarted: current daemon identity was not observed: no daemon link is available"))
      (should (null agent-repl-test-services--builds))
      (should (= agent-repl-test-services--daemon-stops 0)))))

(ert-deftest agent-repl-test-services-runtime-restart-fails-without-a-replacement ()
  "A replacement that never comes up is a failed restart, not a quiet one."
  (agent-repl-test-services--with-harness
    ;; Arrange
    (let ((failure :unset))
      (setq agent-repl-test-services--daemon-conn nil)
      ;; Act
      (agent-repl--runtime-prepare #'ignore (lambda (detail) (setq failure detail)))
      ;; Assert
      (should (stringp failure)))))

(ert-deftest agent-repl-test-services-runtime-restart-build-failure-stops-nothing ()
  "A build that failed must not take the running daemon down with it."
  (agent-repl-test-services--with-harness
    ;; Arrange
    (setq agent-repl-test-services--build-failure "build failed (exit 2)")
    ;; Act
    (agent-repl--runtime-prepare #'ignore #'ignore)
    ;; Assert
    (should (= agent-repl-test-services--daemon-stops 0))))

(ert-deftest agent-repl-test-services-runtime-restart-command-reports-a-failure ()
  "The interactive restart records why it failed rather than dying silently."
  (agent-repl-test-services--with-harness
    ;; Arrange
    (setq agent-repl-test-services--build-failure "build failed (exit 2)")
    ;; Act
    (agent-repl-runtime-restart)
    ;; Assert
    (should (agent-repl-test-services--logged-p
             :warn "elisp.services.runtime-restart-failed"))))

(provide 'test-services)

;;; test-services.el ends here
