;;; test-services.el --- ERT tests for agent-repl services.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-services.el -f ert-run-tests-batch-and-exit
;;
;; NO launchctl, NO build, NO service.  Every external boundary this file
;; reaches — `agent-repl--launchctl-call', the two stamp wrappers, the
;; store-socket probe, the artifact probe, the build script — is
;; registered in `agent-repl--external-boundary-functions' and stubbed
;; below; the harness's guard fails any test that misses one.
;;
;; Readiness is a TIMER poll and the tests drive it by hand: the store's
;; socket probe answers from a variable a test flips between polls.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Harness ----

(defvar agent-repl-test-services--launchctl nil
  "launchctl invocations, newest first: the argv handed to the program.")

(defvar agent-repl-test-services--launchctl-exit 0
  "Exit code the stubbed launchctl answers.")

(defvar agent-repl-test-services--socket-present nil
  "What the stubbed store-socket probe answers.")

(defvar agent-repl-test-services--stamps nil
  "Stamp path → recorded digest, as the stubbed stamp reader sees it.")

(defvar agent-repl-test-services--written nil
  "Stamp writes, newest first: `(PATH . DIGEST)'.")

(defvar agent-repl-test-services--digests nil
  "Binary path → its stubbed SHA-256 digest.")

(defvar agent-repl-test-services--build-failure nil
  "What the stubbed build answers: nil for success, a detail string else.")

(defvar agent-repl-test-services--builds nil
  "Build invocations, newest first: the TARGETS argument.")

(defvar agent-repl-test-services--timers nil
  "Captured readiness timers, newest first: `(SECONDS . FUNCTION)'.")

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

(defun agent-repl-test-services--kickstarted-p (label)
  "Return non-nil when LABEL's service was kickstarted."
  (seq-some (lambda (args)
              (and (equal (car args) "kickstart")
                   (string-suffix-p label (car (last args)))))
            agent-repl-test-services--launchctl))

(defmacro agent-repl-test-services--with-harness (&rest body)
  "Run BODY with services.el's whole world faked and its state reset."
  (declare (indent 0))
  `(let ((agent-repl-test-services--launchctl nil)
         (agent-repl-test-services--launchctl-exit 0)
         (agent-repl-test-services--socket-present t)
         (agent-repl-test-services--stamps nil)
         (agent-repl-test-services--written nil)
         (agent-repl-test-services--digests nil)
         (agent-repl-test-services--build-failure nil)
         (agent-repl-test-services--builds nil)
         (agent-repl-test-services--timers nil)
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
     (cl-letf (((symbol-function 'agent-repl--launchctl-call)
                (lambda (args)
                  (push args agent-repl-test-services--launchctl)
                  agent-repl-test-services--launchctl-exit))
               ((symbol-function 'agent-repl--shim-store-socket-present-p)
                (lambda () agent-repl-test-services--socket-present))
               ((symbol-function 'agent-repl--shim-service-read-stamp)
                (lambda (path) (cdr (assoc path agent-repl-test-services--stamps))))
               ((symbol-function 'agent-repl--shim-service-write-stamp)
                (lambda (path digest)
                  (push (cons path digest) agent-repl-test-services--written)))
               ((symbol-function 'agent-repl--shim-service-file-sha256)
                (lambda (path)
                  (or (cdr (assoc path agent-repl-test-services--digests))
                      "sha-of-installed")))
               ((symbol-function 'agent-repl--frontend-artifact-exists-p) (lambda (_p) t))
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
               ((symbol-function 'agent-repl--shim-services-run-timer)
                (lambda (seconds callback)
                  (push (cons seconds callback) agent-repl-test-services--timers)
                  (timer-create)))
               ((symbol-function 'agent-repl--assert-main-thread) (lambda (_what) nil))
               ((symbol-function 'agent-repl--backend-phase) (lambda (&rest _) nil))
               ((symbol-function 'message) (lambda (&rest _) nil))
               ((symbol-function 'agent-repl--log)
                (lambda (_ws fmt &rest args)
                  (push (cons :log (apply #'format fmt args))
                        agent-repl-test-services--logs)))
               ((symbol-function 'agent-repl--log-verbose) (lambda (&rest _) nil))
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

(defun agent-repl-test-services--stamp-current (binary)
  "Record BINARY's stamp as matching its installed digest, so it is in sync."
  (push (cons (agent-repl--shim-service-deployed-stamp binary) "sha-of-installed")
        agent-repl-test-services--stamps))

;;;; ---- launchctl ----

(ert-deftest agent-repl-test-services-print-addresses-the-gui-domain ()
  "launchd jobs are addressed in the user's GUI domain."
  (agent-repl-test-services--with-harness
    ;; Arrange / Act
    (agent-repl--shim-services-launchctl "print" agent-repl--shim-store-label)
    ;; Assert
    (should (equal (car agent-repl-test-services--launchctl)
                   (list "print" (format "gui/%d/%s" (user-uid)
                                         agent-repl--shim-store-label))))))

(ert-deftest agent-repl-test-services-kickstart-forces-a-restart ()
  "`kickstart -k' is what actually replaces a running service."
  (agent-repl-test-services--with-harness
    ;; Arrange / Act
    (agent-repl--shim-services-launchctl "kickstart" agent-repl--shim-store-label)
    ;; Assert
    (should (equal (nth 1 (car agent-repl-test-services--launchctl)) "-k"))))

(ert-deftest agent-repl-test-services-unknown-verb-is-refused ()
  "An invalid verb is a programming error and never reaches launchd."
  (agent-repl-test-services--with-harness
    ;; Arrange / Act / Assert
    (should-error (agent-repl--shim-services-launchctl "obliterate"
                                                       agent-repl--shim-store-label))))

(ert-deftest agent-repl-test-services-nonzero-launchctl-exit-fails-loudly ()
  "A launchd refusal is fatal, never a shrug that leaves a stale service."
  (agent-repl-test-services--with-harness
    ;; Arrange
    (setq agent-repl-test-services--launchctl-exit 1)
    ;; Act / Assert
    (should-error (agent-repl--shim-services-launchctl "kickstart"
                                                       agent-repl--shim-store-label))))

(ert-deftest agent-repl-test-services-preflight-checks-both-jobs ()
  "Nothing is mutated until launchd is known to own BOTH jobs."
  (agent-repl-test-services--with-harness
    ;; Arrange / Act
    (agent-repl--shim-services-assert-launchd-loaded)
    ;; Assert
    (should (= (length agent-repl-test-services--launchctl) 2))))

;;;; ---- The deployed-fingerprint stamp ----

(ert-deftest agent-repl-test-services-missing-stamp-means-bounce ()
  "Nothing to be in sync with is never \"in sync\"."
  (agent-repl-test-services--with-harness
    ;; Arrange / Act / Assert
    (should (agent-repl--shim-service-needs-bounce-p agent-repl--shim-store-binary))))

(ert-deftest agent-repl-test-services-matching-stamp-means-no-bounce ()
  "A stamp equal to the installed digest means the live process serves it."
  (agent-repl-test-services--with-harness
    ;; Arrange
    (agent-repl-test-services--stamp-current agent-repl--shim-store-binary)
    ;; Act / Assert
    (should-not (agent-repl--shim-service-needs-bounce-p
                 agent-repl--shim-store-binary))))

(ert-deftest agent-repl-test-services-differing-stamp-means-bounce ()
  "A changed binary must reach the running service."
  (agent-repl-test-services--with-harness
    ;; Arrange
    (push (cons (agent-repl--shim-service-deployed-stamp
                 agent-repl--shim-store-binary)
                "sha-of-something-older")
          agent-repl-test-services--stamps)
    ;; Act / Assert
    (should (agent-repl--shim-service-needs-bounce-p agent-repl--shim-store-binary))))

(ert-deftest agent-repl-test-services-absent-binary-means-bounce ()
  "A binary that is not there cannot be the one being served."
  (agent-repl-test-services--with-harness
    ;; Arrange
    (agent-repl-test-services--stamp-current agent-repl--shim-store-binary)
    ;; Act / Assert
    (cl-letf (((symbol-function 'agent-repl--frontend-artifact-exists-p) (lambda (_p) nil)))
      (should (agent-repl--shim-service-needs-bounce-p agent-repl--shim-store-binary)))))

(ert-deftest agent-repl-test-services-recording-writes-the-installed-digest ()
  "The stamp records what launchd was actually started on."
  (agent-repl-test-services--with-harness
    ;; Arrange / Act
    (agent-repl--shim-service-record-deployed agent-repl--shim-store-binary)
    ;; Assert
    (should (equal (car agent-repl-test-services--written)
                   (cons (agent-repl--shim-service-deployed-stamp
                          agent-repl--shim-store-binary)
                         "sha-of-installed")))))

;;;; ---- Store readiness ----

(ert-deftest agent-repl-test-services-store-ready-immediately-succeeds ()
  "A store already serving needs no wait at all."
  (agent-repl-test-services--with-harness
    ;; Arrange
    (let ((ready nil))
      (setq agent-repl-test-services--socket-present t)
      ;; Act
      (agent-repl--shim-store-after-ready (lambda () (setq ready t)) #'ignore)
      ;; Assert
      (should ready))))

(ert-deftest agent-repl-test-services-store-not-yet-serving-polls ()
  "An absent socket schedules another poll instead of failing at once."
  (agent-repl-test-services--with-harness
    ;; Arrange
    (setq agent-repl-test-services--socket-present nil)
    ;; Act
    (agent-repl--shim-store-after-ready #'ignore #'ignore)
    ;; Assert
    (should agent-repl-test-services--timers)))

(ert-deftest agent-repl-test-services-store-poll-succeeds-when-the-socket-appears ()
  "The socket appearing between polls is what readiness means."
  (agent-repl-test-services--with-harness
    ;; Arrange
    (let ((ready nil))
      (setq agent-repl-test-services--socket-present nil)
      (agent-repl--shim-store-after-ready (lambda () (setq ready t)) #'ignore)
      (setq agent-repl-test-services--socket-present t)
      ;; Act
      (funcall (cdr (car agent-repl-test-services--timers)))
      ;; Assert
      (should ready))))

(ert-deftest agent-repl-test-services-store-readiness-timeout-fails ()
  "A store that never comes up fails loudly rather than starting the sidecar cold."
  (agent-repl-test-services--with-harness
    ;; Arrange
    (let ((failure :unset)
          (agent-repl-shim-store-ready-timeout 0.0))
      (setq agent-repl-test-services--socket-present nil)
      ;; Act
      (agent-repl--shim-store-after-ready #'ignore (lambda (detail) (setq failure detail)))
      ;; Assert
      (should (stringp failure)))))

;;;; ---- The store/sidecar bounce ----

(ert-deftest agent-repl-test-services-bounce-builds-only-its-two-targets ()
  "The bounce owns the store and the sidecar and nothing else."
  (agent-repl-test-services--with-harness
    ;; Arrange / Act
    (agent-repl--shim-services-build-and-bounce t #'ignore #'ignore)
    ;; Assert
    (should (equal (car agent-repl-test-services--builds) '("store" "sidecar")))))

(ert-deftest agent-repl-test-services-bounce-kickstarts-a-stale-store ()
  "A binary whose stamp does not match reaches the running service."
  (agent-repl-test-services--with-harness
    ;; Arrange / Act
    (agent-repl--shim-services-build-and-bounce t #'ignore #'ignore)
    ;; Assert
    (should (agent-repl-test-services--kickstarted-p agent-repl--shim-store-label))))

(ert-deftest agent-repl-test-services-bounce-skips-a-current-store ()
  "The stamp is the SOLE kickstart authority: a second bounce is pure outage."
  (agent-repl-test-services--with-harness
    ;; Arrange
    (agent-repl-test-services--stamp-current agent-repl--shim-store-binary)
    (agent-repl-test-services--stamp-current agent-repl--shim-sidecar-binary)
    ;; Act
    (agent-repl--shim-services-build-and-bounce t #'ignore #'ignore)
    ;; Assert
    (should-not (agent-repl-test-services--kickstarted-p agent-repl--shim-store-label))))

(ert-deftest agent-repl-test-services-a-store-bounce-always-bounces-the-sidecar ()
  "The sidecar's link recovery is connection-scoped: a fresh pair is known-good."
  (agent-repl-test-services--with-harness
    ;; Arrange
    (agent-repl-test-services--stamp-current agent-repl--shim-sidecar-binary)
    ;; Act
    (agent-repl--shim-services-build-and-bounce t #'ignore #'ignore)
    ;; Assert
    (should (agent-repl-test-services--kickstarted-p agent-repl--shim-sidecar-label))))

(ert-deftest agent-repl-test-services-bounce-waits-for-the-store-before-the-sidecar ()
  "The sidecar dials the store's socket; it must not come up against nothing."
  (agent-repl-test-services--with-harness
    ;; Arrange
    (setq agent-repl-test-services--socket-present nil)
    ;; Act
    (agent-repl--shim-services-build-and-bounce t #'ignore #'ignore)
    ;; Assert
    (should-not (agent-repl-test-services--kickstarted-p agent-repl--shim-sidecar-label))))

(ert-deftest agent-repl-test-services-bounce-stamps-the-store-after-its-kickstart ()
  "The stamp is written only once launchd actually started that image."
  (agent-repl-test-services--with-harness
    ;; Arrange / Act
    (agent-repl--shim-services-build-and-bounce t #'ignore #'ignore)
    ;; Assert
    (should (assoc (agent-repl--shim-service-deployed-stamp
                    agent-repl--shim-store-binary)
                   agent-repl-test-services--written))))

(ert-deftest agent-repl-test-services-bounce-build-failure-kickstarts-nothing ()
  "A failed build must never reach launchd."
  (agent-repl-test-services--with-harness
    ;; Arrange
    (setq agent-repl-test-services--build-failure "build failed (exit 2)")
    ;; Act
    (agent-repl--shim-services-build-and-bounce t #'ignore #'ignore)
    ;; Assert
    (should (null agent-repl-test-services--launchctl))))

(ert-deftest agent-repl-test-services-bounce-build-failure-reaches-the-continuation ()
  "The first diagnostic is what the coordinator reports, and no stage follows."
  (agent-repl-test-services--with-harness
    ;; Arrange
    (let ((failure :unset))
      (setq agent-repl-test-services--build-failure "build failed (exit 2)")
      ;; Act
      (agent-repl--shim-services-build-and-bounce t #'ignore
                                                  (lambda (detail) (setq failure detail)))
      ;; Assert
      (should (equal failure "build failed (exit 2)")))))

(ert-deftest agent-repl-test-services-bounce-runs-its-own-preflight-when-told-to ()
  "Without an inherited preflight the bounce validates both jobs itself."
  (agent-repl-test-services--with-harness
    ;; Arrange / Act
    (agent-repl--shim-services-build-and-bounce nil #'ignore #'ignore)
    ;; Assert
    (should (seq-some (lambda (args) (equal (car args) "print"))
                      agent-repl-test-services--launchctl))))

;;;; ---- The coordinated runtime bounce ----

(ert-deftest agent-repl-test-services-runtime-restart-builds-the-whole-stack ()
  "The daemon's own build comes before the services', and covers everything."
  (agent-repl-test-services--with-harness
    ;; Arrange / Act
    (agent-repl--runtime-prepare #'ignore #'ignore)
    ;; Assert
    (should (member nil agent-repl-test-services--builds))))

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

(ert-deftest agent-repl-test-services-runtime-restart-preflight-failure-builds-nothing ()
  "launchd owning both jobs is checked before any runtime artifact is built."
  (agent-repl-test-services--with-harness
    ;; Arrange
    (setq agent-repl-test-services--launchctl-exit 1)
    ;; Act
    (agent-repl--runtime-prepare #'ignore #'ignore)
    ;; Assert
    (should (null agent-repl-test-services--builds))))

(ert-deftest agent-repl-test-services-runtime-restart-preflight-failure-reports ()
  "A refused preflight reaches the failure continuation as its detail."
  (agent-repl-test-services--with-harness
    ;; Arrange
    (let ((failure :unset))
      (setq agent-repl-test-services--launchctl-exit 1)
      ;; Act
      (agent-repl--runtime-prepare #'ignore (lambda (detail) (setq failure detail)))
      ;; Assert
      (should (stringp failure)))))

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
