;;; test-services.el --- Tests for launchd runtime coordination -*- lexical-binding: t; -*-

;;; Code:

(load (expand-file-name "test-helpers.el"
                        (file-name-directory
                         (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

(ert-deftest agent-repl-services-test-store-readiness-is-timer-driven ()
  "Socket readiness returns before its owned timer reports success."
  (let ((checks 0) timer success failure)
    (cl-letf (((symbol-function 'agent-repl--shim-store-socket-present-p)
               (lambda () (setq checks (1+ checks)) (> checks 2)))
              ((symbol-function 'agent-repl--shim-services-run-timer)
               (lambda (_seconds callback) (setq timer callback) 'timer)))
      (should (eq :pending
                  (agent-repl--shim-store-after-ready
                   (lambda () (setq success t))
                   (lambda (detail) (setq failure detail)))))
      (should-not success)
      (funcall timer)
      (should success)
      (should-not failure))))

(ert-deftest agent-repl-services-test-build-and-bounce-orders-store-before-sidecar ()
  "The sidecar is untouched until asynchronous store readiness succeeds."
  (let (events store-success complete failure)
    (cl-letf (((symbol-function 'agent-repl--shim-services-assert-launchd-loaded)
               (lambda () (push 'preflight events)))
              ((symbol-function 'agent-repl--frontend-build-targets-if-stale)
               (lambda (&rest _) (push 'build events)))
              ((symbol-function 'agent-repl--frontend-artifact-exists-p) (lambda (_path) t))
              ((symbol-function 'agent-repl--shim-services-launchctl)
               (lambda (_verb label) (push label events)))
              ((symbol-function 'agent-repl--shim-store-after-ready)
               (lambda (ok _fail) (setq store-success ok) :pending))
              ((symbol-function 'agent-repl--shim-service-record-deployed)
               (lambda (path) (push path events))))
      (should (eq :pending
                  (agent-repl--shim-services-build-and-bounce
                   nil (lambda () (setq complete t))
                   (lambda (detail) (setq failure detail)))))
      (should-not (member agent-repl--shim-sidecar-label events))
      (funcall store-success)
      (should complete)
      (should-not failure)
      (should (< (cl-position agent-repl--shim-store-label (reverse events))
                 (cl-position agent-repl--shim-sidecar-label (reverse events)))))))

(ert-deftest agent-repl-services-test-runtime-refuses-active-turn-before-mutation ()
  "An active turn reaches failure before build or service mutation."
  (let (mutated failure)
    (cl-letf (((symbol-function 'agent-repl--frontend-runtime-bounce-preflight-async)
               (lambda (callback) (funcall callback :absent)))
              ((symbol-function 'agent-repl--frontend-all-turn-active-workspaces)
               (lambda () '("/w-busy")))
              ((symbol-function 'agent-repl--shim-services-assert-launchd-loaded)
               (lambda () (setq mutated t))))
      (agent-repl--runtime-prepare
       t (lambda () (error "unexpected success"))
       (lambda (detail) (setq failure detail)))
      (should (string-match-p "turn in flight" failure))
      (should-not mutated))))

(ert-deftest agent-repl-services-test-runtime-orders-async-stages ()
  "Runtime completion follows readiness, health, and rebind continuations."
  (let (events)
    (cl-letf (((symbol-function 'agent-repl--frontend-runtime-bounce-preflight-async)
               (lambda (callback) (push 'daemon-preflight events)
                 (funcall callback :absent)))
              ((symbol-function 'agent-repl--frontend-all-turn-active-session-ids)
               (lambda () nil))
              ((symbol-function 'agent-repl--shim-services-assert-launchd-loaded)
               (lambda () (push 'launchd-preflight events)))
              ((symbol-function 'agent-repl--frontend-build-if-stale)
               (lambda (&rest _) (push 'build events)))
              ((symbol-function 'agent-repl--shim-services-build-and-bounce)
               (lambda (_preflight ok _fail) (push 'services events)
                 (funcall ok) :pending))
              ((symbol-function 'agent-repl--frontend-bounce-after-build)
               (lambda (_state _stop on-complete) (push 'daemon-bounce events)
                 (funcall on-complete 'started)))
              ((symbol-function 'agent-repl--frontend-after-ready)
               (lambda (ok _fail &optional _ws) (push 'ready events)
                 (funcall ok) :pending))
              ((symbol-function 'agent-repl--frontend-after-daemon-healthy)
               (lambda (ok _fail) (push 'health events) (funcall ok) :pending))
              ((symbol-function 'agent-repl--frontend-rebind-workspaces-after-restart)
               (lambda (ok _fail) (push 'rebind events) (funcall ok 2) :pending)))
      (should (eq :pending
                  (agent-repl--runtime-prepare
                   t (lambda () (push 'complete events))
                   (lambda (detail) (error "unexpected failure: %s" detail)))))
      (should (equal (nreverse events)
                     '(daemon-preflight launchd-preflight build services
                       daemon-bounce ready health rebind complete))))))

(ert-deftest agent-repl-services-test-runtime-forwards-stop-shims ()
  "Explicit shim stopping reaches the daemon bounce unchanged."
  (let (seen)
    (cl-letf (((symbol-function 'agent-repl--frontend-runtime-bounce-preflight-async)
               (lambda (callback) (funcall callback :absent)))
              ((symbol-function 'agent-repl--frontend-all-turn-active-session-ids) (lambda () nil))
              ((symbol-function 'agent-repl--shim-services-assert-launchd-loaded) #'ignore)
              ((symbol-function 'agent-repl--frontend-build-if-stale) #'ignore)
              ((symbol-function 'agent-repl--shim-services-build-and-bounce)
               (lambda (_preflight ok _fail) (funcall ok)))
              ((symbol-function 'agent-repl--frontend-bounce-after-build)
               (lambda (_state stop on-complete) (setq seen stop)
                 (funcall on-complete 'started)))
              ((symbol-function 'agent-repl--frontend-after-ready)
               (lambda (ok _fail &optional _ws) (funcall ok)))
              ((symbol-function 'agent-repl--frontend-after-daemon-healthy)
               (lambda (ok _fail) (funcall ok)))
              ((symbol-function 'agent-repl--frontend-rebind-workspaces-after-restart)
               (lambda (ok _fail) (funcall ok 0))))
      (agent-repl--runtime-prepare t #'ignore #'error t)
      (should seen))))

(defmacro agent-repl-services-test--with-bounced-link (bindings &rest body)
  "Run BODY over a runtime restart whose bounce retires the pre-bounce link.
BINDINGS are extra `cl-letf' forms appended after the shared stubs, so a
test can override readiness without restating the whole coordinator."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-repl--frontend-runtime-bounce-preflight-async)
              (lambda (callback) (funcall callback :tracked)))
             ((symbol-function 'agent-repl--frontend-all-turn-active-workspaces)
              (lambda () nil))
             ((symbol-function 'agent-repl--shim-services-assert-launchd-loaded) #'ignore)
             ((symbol-function 'agent-repl--frontend-build-if-stale) #'ignore)
             ((symbol-function 'agent-repl--shim-services-build-and-bounce)
              (lambda (_preflight ok _fail) (funcall ok) :pending))
             ((symbol-function 'agent-repl--frontend-bounce-after-build)
              (lambda (_state _stop on-complete) (funcall on-complete 'started)))
             ((symbol-function 'agent-repl--frontend-after-daemon-healthy)
              (lambda (ok _fail) (funcall ok) :pending))
             ((symbol-function 'agent-repl--frontend-rebind-workspaces-after-restart)
              (lambda (ok _fail) (funcall ok 0) :pending))
             ,@bindings)
     ,@body))

(ert-deftest agent-repl-services-test-runtime-restart-await-retires-bounced-link ()
  "Post-bounce readiness can no longer be satisfied by the retired link."
  (let ((agent-repl--frontend-last-daemon-view '(:bootId "outgoing"))
        disconnected observed)
    (agent-repl-services-test--with-bounced-link
        (((symbol-function 'agent-repl-uds-disconnect)
          (lambda () (setq disconnected t)))
         ((symbol-function 'agent-repl--frontend-after-ready)
          (lambda (ok _fail &optional _ws)
            (push (list :view (agent-repl--frontend-daemon-view)
                        :disconnected disconnected)
                  observed)
            (funcall ok)
            :pending)))
      (should (equal "runtime-restart-complete"
                     (agent-repl-runtime-restart-await nil 1.0)))
      (should (equal (car observed) '(:view nil :disconnected t))))))

(ert-deftest agent-repl-services-test-runtime-restart-await-fails-when-replacement-never-ready ()
  "Retiring the bounced link never converts a missing replacement into success."
  (let ((agent-repl--frontend-last-daemon-view '(:bootId "outgoing"))
        (ready-calls 0))
    (agent-repl-services-test--with-bounced-link
        (((symbol-function 'agent-repl-uds-disconnect) #'ignore)
         ((symbol-function 'agent-repl--frontend-after-ready)
          (lambda (ok fail &optional _ws)
            (cl-incf ready-calls)
            (if (= ready-calls 1)
                (funcall ok)
              (funcall fail "daemon at /sock never became ready"))
            :pending)))
      (should-error (agent-repl-runtime-restart-await nil 1.0) :type 'error))))

(ert-deftest agent-repl-services-test-runtime-restart-await-requires-terminal-success ()
  "The deployment surface returns only after its success continuation runs."
  (let (finish pumps)
    (cl-letf (((symbol-function 'agent-repl--runtime-prepare)
               (lambda (_rebind on-success _on-failure &optional _stop-shims)
                 (setq finish on-success)
                 :pending))
              ((symbol-function 'agent-repl--runtime-pump-events)
               (lambda (_seconds)
                 (push 'pump pumps)
                 (funcall finish))))
      (should (equal "runtime-restart-complete"
                     (agent-repl-runtime-restart-await nil 1.0)))
      (should (equal pumps '(pump))))))

(ert-deftest agent-repl-services-test-runtime-restart-await-extends-health-budget-locally ()
  "The deployment latch extends health deadlines only inside its event pump."
  (let ((agent-repl-frontend-health-timeout 10.0)
        (agent-repl-frontend-ready-attempts 25)
        (agent-repl-uds-command-ack-deadline 10.0)
        seen-health seen-ready seen-ack)
    (cl-letf (((symbol-function 'agent-repl--runtime-prepare)
               (lambda (_rebind on-success _on-failure &optional _stop-shims)
                 (setq seen-health agent-repl-frontend-health-timeout
                       seen-ready agent-repl-frontend-ready-attempts
                       seen-ack agent-repl-uds-command-ack-deadline)
                 (funcall on-success)
                 :pending)))
      (should (equal "runtime-restart-complete"
                     (agent-repl-runtime-restart-await nil 300.0)))
      (should (= seen-health 60.0))
      (should (= seen-ready 150))
      (should (= seen-ack 60.0))
      (should (= agent-repl-frontend-health-timeout 10.0))
      (should (= agent-repl-frontend-ready-attempts 25))
      (should (= agent-repl-uds-command-ack-deadline 10.0)))))

(ert-deftest agent-repl-services-test-runtime-restart-await-bounds-health-before-terminal-timeout ()
  "A short terminal timeout still leaves an earlier health failure boundary."
  (let (seen-health seen-ready seen-ack)
    (cl-letf (((symbol-function 'agent-repl--runtime-prepare)
               (lambda (_rebind on-success _on-failure &optional _stop-shims)
                 (setq seen-health agent-repl-frontend-health-timeout
                       seen-ready agent-repl-frontend-ready-attempts
                       seen-ack agent-repl-uds-command-ack-deadline)
                 (funcall on-success)
                 :pending)))
      (should (equal "runtime-restart-complete"
                     (agent-repl-runtime-restart-await nil 1.0)))
      (should (= seen-health 0.8))
      (should (= seen-ready 4))
      (should (= seen-ack 0.8)))))

(ert-deftest agent-repl-services-test-runtime-restart-await-surfaces-failure ()
  "A coordinator failure is signalled instead of resembling completion."
  (cl-letf (((symbol-function 'agent-repl--runtime-prepare)
             (lambda (_rebind _on-success on-failure &optional _stop-shims)
               (funcall on-failure "daemon unhealthy")
               :pending)))
    (should-error (agent-repl-runtime-restart-await nil 1.0)
                  :type 'error)))

(ert-deftest agent-repl-services-test-runtime-restart-await-surfaces-timeout ()
  "A coordinator that never settles makes deployment fail loudly."
  (let ((times '(10.0 10.1 11.1 11.2)))
    (cl-letf (((symbol-function 'agent-repl--runtime-prepare)
               (lambda (&rest _) :pending))
              ((symbol-function 'float-time)
               (lambda (&optional _value) (pop times)))
              ((symbol-function 'agent-repl--runtime-pump-events) #'ignore))
      (should-error (agent-repl-runtime-restart-await nil 1.0)
                    :type 'error))))

;;;; ---- Backend-initiation output: record and echo --------------------------
;;
;; A launchd refusal and a failed coordinated restart both used to be visible
;; only to whoever went looking in the log afterwards.

(defmacro agent-repl-services-test--with-phase-echo (var &rest body)
  "Run BODY with VAR collecting every echoed backend phase line, newest first."
  (declare (indent 1))
  `(let (,var)
     (cl-letf (((symbol-function 'agent-repl--emit-message)
                (lambda (text &optional _echo) (push text ,var)))
               ((symbol-function 'agent-repl--persist-log-record) #'ignore))
       ,@body)))

(defun agent-repl-services-test--phase-line-p (lines &rest fragments)
  "Return non-nil when one line in LINES contains every one of FRAGMENTS."
  (cl-some (lambda (line)
             (cl-every (lambda (fragment)
                         (string-match-p (regexp-quote fragment) line))
                       fragments))
           lines))

(ert-deftest agent-repl-services-test-launchctl-failure-echoes-captured-output ()
  "A refused kickstart echoes the phase, the exit status, and its output tail."
  ;; Arrange
  (agent-repl-services-test--with-phase-echo lines
    (cl-letf (((symbol-function 'agent-repl--launchctl-call)
               (lambda (_args)
                 (with-current-buffer
                     (get-buffer-create agent-repl--shim-services-buffer)
                   (insert "Could not find service in domain\n"))
                 64)))
      ;; Act
      (should-error
       (agent-repl--shim-services-launchctl "kickstart"
                                            agent-repl--shim-store-label)))
    ;; Assert
    (should (agent-repl-services-test--phase-line-p
             lines "launchctl kickstart FAILED" "exit 64"
             "Could not find service in domain"))))

(ert-deftest agent-repl-services-test-launchctl-failure-still-signals ()
  "The echo line is additive: a nonzero launchctl exit still signals."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--emit-message) #'ignore)
            ((symbol-function 'agent-repl--persist-log-record) #'ignore)
            ((symbol-function 'agent-repl--launchctl-call) (lambda (_args) 64)))
    ;; Act
    (let ((detail (should-error
                   (agent-repl--shim-services-launchctl
                    "kickstart" agent-repl--shim-store-label))))
      ;; Assert
      (should (string-match-p "exit 64" (cadr detail))))))

(ert-deftest agent-repl-services-test-runtime-failure-echoes-the-phase ()
  "A refused restart names its phase and points at the log file."
  ;; Arrange
  (agent-repl-services-test--with-phase-echo lines
    (cl-letf (((symbol-function 'agent-repl--frontend-runtime-bounce-preflight-async)
               (lambda (callback) (funcall callback :absent)))
              ((symbol-function 'agent-repl--frontend-all-turn-active-workspaces)
               (lambda () '("/w-busy"))))
      ;; Act
      (agent-repl--runtime-prepare
       t (lambda () (error "unexpected success")) #'ignore))
    ;; Assert
    (should (agent-repl-services-test--phase-line-p
             lines "backend restart FAILED" "turn in flight"
             (agent-repl--logfile-path)))))

(ert-deftest agent-repl-services-test-runtime-success-echoes-completion ()
  "A completed startup reports that it finished, not only that it began."
  ;; Arrange
  (agent-repl-services-test--with-phase-echo lines
    (cl-letf (((symbol-function 'agent-repl--frontend-runtime-bounce-preflight-async)
               (lambda (callback) (funcall callback :absent)))
              ((symbol-function 'agent-repl--frontend-all-turn-active-workspaces)
               (lambda () nil))
              ((symbol-function 'agent-repl--shim-services-assert-launchd-loaded)
               #'ignore)
              ((symbol-function 'agent-repl--frontend-build-if-stale) #'ignore)
              ((symbol-function 'agent-repl--shim-services-build-and-bounce)
               (lambda (_preflight on-success _on-failure) (funcall on-success)))
              ((symbol-function 'agent-repl--frontend-bounce-after-build)
               (lambda (_state _stop on-complete) (funcall on-complete 'proc)))
              ((symbol-function 'agent-repl--runtime-retire-bounced-link) #'ignore)
              ((symbol-function 'agent-repl--frontend-after-ready)
               (lambda (on-ready _fail &optional _ws) (funcall on-ready)))
              ((symbol-function 'agent-repl--frontend-after-daemon-healthy)
               (lambda (on-success _fail) (funcall on-success))))
      ;; Act
      (agent-repl--runtime-prepare nil #'ignore (lambda (detail) (error "%s" detail))))
    ;; Assert
    (should (agent-repl-services-test--phase-line-p
             lines "backend startup complete"))))

(provide 'test-services)

;;; test-services.el ends here
