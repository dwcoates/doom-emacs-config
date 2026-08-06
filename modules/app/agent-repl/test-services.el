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
               (lambda (ok _fail) (push 'preflight events) (funcall ok)))
              ((symbol-function 'agent-repl--frontend-build-targets-if-stale)
               (lambda (_targets _force ok _fail) (push 'build events) (funcall ok 0)))
              ((symbol-function 'agent-repl--frontend-artifact-exists-p) (lambda (_path) t))
              ((symbol-function 'agent-repl--shim-services-launchctl)
               (lambda (_verb label ok _fail) (push label events) (funcall ok)))
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

(ert-deftest agent-repl-services-test-launchctl-reaches-failure-on-nonzero-exit ()
  "A nonzero launchctl exit reaches the failure continuation with its output."
  (let (detail succeeded)
    (cl-letf (((symbol-function 'agent-repl--launchctl-call)
               (lambda (_args callback)
                 (with-current-buffer
                     (get-buffer-create agent-repl--shim-services-buffer)
                   (insert "Could not find service"))
                 (funcall callback 3))))
      (agent-repl--shim-services-launchctl
       "kickstart" agent-repl--shim-store-label
       (lambda () (setq succeeded t))
       (lambda (d) (setq detail d)))
      (should-not succeeded)
      (should (string-match-p "exit 3" detail))
      (should (string-match-p "Could not find service" detail)))))

(ert-deftest agent-repl-services-test-launchd-preflight-stops-at-the-first-missing-job ()
  "A store job launchd does not own never lets the sidecar probe start."
  (let (probed detail)
    (cl-letf (((symbol-function 'agent-repl--shim-services-launchctl)
               (lambda (_verb label _ok fail)
                 (push label probed)
                 (funcall fail (format "launchd service %s failed" label)))))
      (agent-repl--shim-services-assert-launchd-loaded
       (lambda () (error "unexpected success"))
       (lambda (d) (setq detail d)))
      (should (equal probed (list agent-repl--shim-store-label)))
      (should (string-match-p agent-repl--shim-store-label detail)))))

(ert-deftest agent-repl-services-test-build-and-bounce-surfaces-a-failed-build ()
  "A failed target build reaches the bounce's failure continuation intact."
  (let (detail kicked)
    (cl-letf (((symbol-function 'agent-repl--frontend-build-targets-if-stale)
               (lambda (_targets _force _ok fail) (funcall fail "build failed (exit 2)")))
              ((symbol-function 'agent-repl--shim-services-launchctl)
               (lambda (&rest _) (setq kicked t)))
              ((symbol-function 'message) #'ignore))
      (agent-repl--shim-services-build-and-bounce
       t (lambda () (error "unexpected success"))
       (lambda (d) (setq detail d)))
      (should-not kicked)
      (should (equal detail "build failed (exit 2)")))))

(ert-deftest agent-repl-services-test-runtime-refuses-active-turn-before-mutation ()
  "An active turn reaches failure before build or service mutation."
  (let (mutated failure)
    (cl-letf (((symbol-function 'agent-repl--frontend-runtime-bounce-preflight-async)
               (lambda (callback) (funcall callback :absent)))
              ((symbol-function 'agent-repl--frontend-all-turn-active-session-ids)
               (lambda () '("s_busy")))
              ((symbol-function 'agent-repl--shim-services-assert-launchd-loaded)
               (lambda (&rest _) (setq mutated t))))
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
               (lambda (ok _fail) (push 'launchd-preflight events) (funcall ok)))
              ((symbol-function 'agent-repl--frontend-build-if-stale)
               (lambda (_force ok _fail) (push 'build events) (funcall ok 0)))
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
              ((symbol-function 'agent-repl--shim-services-assert-launchd-loaded)
               (lambda (ok _fail) (funcall ok)))
              ((symbol-function 'agent-repl--frontend-build-if-stale)
               (lambda (_force ok _fail) (funcall ok 0)))
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

(defmacro agent-repl-services-test--with-dispatch-seams (published &rest body)
  "Run BODY with the dispatch's artifact and timer seams captured.
PUBLISHED collects `(PATH . TEXT)' for every completion artifact written,
newest first, so no test touches the real state directory.  The timer seam
returns a live-looking token WITHOUT firing, which is what lets a test
drive the coordinator's own continuations and still assert the timeout
timer was cancelled."
  (declare (indent 1))
  `(let ((,published nil))
     (setq agent-repl--runtime-restart-dispatch nil)
     (cl-letf (((symbol-function 'agent-repl--runtime-restart-write-result)
                (lambda (path text) (push (cons path text) ,published)))
               ((symbol-function 'agent-repl--shim-services-run-timer)
                (lambda (_seconds _callback) nil)))
       ,@body)))

(defun agent-repl-services-test--artifact-status (entry)
  "Return the `status=' field of the published artifact ENTRY."
  (when (string-match "^status=\\(.*\\)$" (cdr entry))
    (match-string 1 (cdr entry))))

(ert-deftest agent-repl-services-test-dispatch-returns-the-request-identity ()
  "The deployment surface returns at once, naming the request it dispatched."
  (agent-repl-services-test--with-dispatch-seams published
    (cl-letf (((symbol-function 'agent-repl--runtime-prepare)
               (lambda (_rebind _on-success _on-failure &optional _stop-shims)
                 :pending)))
      (let ((result (agent-repl-runtime-restart-dispatch nil 300.0)))
        (should (string-prefix-p "runtime-restart-dispatched:" result))
        ;; Nothing terminal has happened yet: the only artifact is `pending'.
        (should (equal 1 (length published)))
        (should (equal "pending"
                       (agent-repl-services-test--artifact-status (car published))))))))

(ert-deftest agent-repl-services-test-dispatch-never-blocks-the-main-thread ()
  "The coordinator completes with every blocking primitive made fatal.
The restart's boundaries are `make-process' plus a sentinel now, so a
dispatch that reaches completion while `call-process',
`accept-process-output' and `sleep-for' all signal is direct evidence that
no step holds the editor."
  (agent-repl-services-test--with-dispatch-seams published
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _) (error "blocking primitive: call-process")))
              ((symbol-function 'accept-process-output)
               (lambda (&rest _) (error "blocking primitive: accept-process-output")))
              ((symbol-function 'sleep-for)
               (lambda (&rest _) (error "blocking primitive: sleep-for")))
              ((symbol-function 'agent-repl--frontend-runtime-bounce-preflight-async)
               (lambda (callback) (funcall callback :absent)))
              ((symbol-function 'agent-repl--frontend-all-turn-active-session-ids)
               (lambda () nil))
              ((symbol-function 'agent-repl--launchctl-call)
               (lambda (_args callback) (funcall callback 0)))
              ((symbol-function 'agent-repl--frontend-run-build-script)
               (lambda (_args callback) (funcall callback 0)))
              ((symbol-function 'agent-repl--frontend-artifact-exists-p)
               (lambda (_path) t))
              ((symbol-function 'agent-repl--shim-store-socket-present-p)
               (lambda () t))
              ((symbol-function 'agent-repl--shim-service-record-deployed) #'ignore)
              ((symbol-function 'agent-repl--frontend-bounce-after-build)
               (lambda (_state _stop on-complete) (funcall on-complete 'started)))
              ((symbol-function 'agent-repl--frontend-after-ready)
               (lambda (ok _fail &optional _ws) (funcall ok)))
              ((symbol-function 'agent-repl--frontend-after-daemon-healthy)
               (lambda (ok _fail) (funcall ok)))
              ((symbol-function 'agent-repl--frontend-rebind-workspaces-after-restart)
               (lambda (ok _fail) (funcall ok 2))))
      (should (string-prefix-p "runtime-restart-dispatched:"
                               (agent-repl-runtime-restart-dispatch nil 300.0)))
      (should (equal "complete"
                     (agent-repl-services-test--artifact-status (car published)))))))

(ert-deftest agent-repl-services-test-dispatch-carries-a-launchctl-failure-to-the-coordinator ()
  "A nonzero launchctl exit reaches the coordinator's failure continuation.
The exit code used to be a synchronous value; it is a sentinel callback
now, and the detail must survive the change."
  (agent-repl-services-test--with-dispatch-seams published
    (cl-letf (((symbol-function 'agent-repl--frontend-runtime-bounce-preflight-async)
               (lambda (callback) (funcall callback :absent)))
              ((symbol-function 'agent-repl--frontend-all-turn-active-session-ids)
               (lambda () nil))
              ((symbol-function 'agent-repl--launchctl-call)
               (lambda (_args callback) (funcall callback 113)))
              ((symbol-function 'message) #'ignore))
      (agent-repl-runtime-restart-dispatch nil 300.0)
      (let ((artifact (cdr (car published))))
        (should (string-match-p "^status=failed$" artifact))
        (should (string-match-p "exit 113" artifact))
        (should (string-match-p agent-repl--shim-store-label artifact))))))

(ert-deftest agent-repl-services-test-dispatch-carries-a-build-failure-to-the-coordinator ()
  "A nonzero build-script exit reaches the coordinator's failure continuation."
  (agent-repl-services-test--with-dispatch-seams published
    (cl-letf (((symbol-function 'agent-repl--frontend-runtime-bounce-preflight-async)
               (lambda (callback) (funcall callback :absent)))
              ((symbol-function 'agent-repl--frontend-all-turn-active-session-ids)
               (lambda () nil))
              ((symbol-function 'agent-repl--shim-services-assert-launchd-loaded)
               (lambda (ok _fail) (funcall ok)))
              ((symbol-function 'agent-repl--frontend-run-build-script)
               (lambda (_args callback) (funcall callback 7)))
              ((symbol-function 'display-buffer) #'ignore)
              ((symbol-function 'message) #'ignore))
      (agent-repl-runtime-restart-dispatch nil 300.0)
      (let ((artifact (cdr (car published))))
        (should (string-match-p "^status=failed$" artifact))
        (should (string-match-p "exit 7" artifact))))))

(ert-deftest agent-repl-services-test-dispatch-raises-the-health-budget-until-it-settles ()
  "The deployment budget is raised for the dispatch and restored on settle."
  (let ((agent-repl-frontend-health-timeout 10.0)
        (agent-repl-frontend-ready-attempts 25)
        (agent-repl-uds-command-ack-deadline 10.0)
        seen-health seen-ready seen-ack finish)
    (agent-repl-services-test--with-dispatch-seams published
      (cl-letf (((symbol-function 'agent-repl--runtime-prepare)
                 (lambda (_rebind on-success _on-failure &optional _stop-shims)
                   (setq seen-health agent-repl-frontend-health-timeout
                         seen-ready agent-repl-frontend-ready-attempts
                         seen-ack agent-repl-uds-command-ack-deadline
                         finish on-success)
                   :pending)))
        (agent-repl-runtime-restart-dispatch nil 300.0)
        (should (= seen-health 60.0))
        (should (= seen-ready 150))
        (should (= seen-ack 60.0))
        ;; Still raised while the coordinator runs across timers.
        (should (= agent-repl-frontend-health-timeout 60.0))
        (funcall finish)
        (should (= agent-repl-frontend-health-timeout 10.0))
        (should (= agent-repl-frontend-ready-attempts 25))
        (should (= agent-repl-uds-command-ack-deadline 10.0))
        (should (equal "complete"
                       (agent-repl-services-test--artifact-status (car published))))))))

(ert-deftest agent-repl-services-test-dispatch-bounds-health-before-the-terminal-timeout ()
  "A short dispatch timeout still leaves an earlier health failure boundary."
  (let (seen-health seen-ready seen-ack)
    (agent-repl-services-test--with-dispatch-seams _published
      (cl-letf (((symbol-function 'agent-repl--runtime-prepare)
                 (lambda (_rebind on-success _on-failure &optional _stop-shims)
                   (setq seen-health agent-repl-frontend-health-timeout
                         seen-ready agent-repl-frontend-ready-attempts
                         seen-ack agent-repl-uds-command-ack-deadline)
                   (funcall on-success)
                   :pending)))
        (agent-repl-runtime-restart-dispatch nil 1.0)
        (should (= seen-health 0.8))
        (should (= seen-ready 4))
        (should (= seen-ack 0.8))))))

(ert-deftest agent-repl-services-test-dispatch-publishes-a-coordinator-failure ()
  "A coordinator failure publishes `failed' with the detail intact."
  (agent-repl-services-test--with-dispatch-seams published
    (cl-letf (((symbol-function 'agent-repl--runtime-prepare)
               (lambda (_rebind _on-success on-failure &optional _stop-shims)
                 (funcall on-failure "daemon unhealthy")
                 :pending)))
      (agent-repl-runtime-restart-dispatch nil 1.0)
      (let ((artifact (cdr (car published))))
        (should (string-match-p "^status=failed$" artifact))
        (should (string-match-p "detail=daemon unhealthy" artifact))))))

(ert-deftest agent-repl-services-test-dispatch-publishes-a-timeout ()
  "A coordinator that never settles publishes `failed' from its timer."
  (let ((published nil) fire)
    (setq agent-repl--runtime-restart-dispatch nil)
    (cl-letf (((symbol-function 'agent-repl--runtime-restart-write-result)
               (lambda (path text) (push (cons path text) published)))
              ((symbol-function 'agent-repl--shim-services-run-timer)
               (lambda (_seconds callback) (setq fire callback) 'timer))
              ((symbol-function 'agent-repl--runtime-prepare)
               (lambda (&rest _) :pending)))
      (agent-repl-runtime-restart-dispatch nil 1.0)
      (should (equal "pending"
                     (agent-repl-services-test--artifact-status (car published))))
      (funcall fire)
      (let ((artifact (cdr (car published))))
        (should (string-match-p "^status=failed$" artifact))
        (should (string-match-p "did not settle within 1.000s" artifact))))))

(ert-deftest agent-repl-services-test-dispatch-refuses-a-concurrent-dispatch ()
  "A second dispatch while one is outstanding fails hard rather than racing.
Both would save and restore the same deployment budget variables, so the
second is an invariant violation rather than a queued request."
  (agent-repl-services-test--with-dispatch-seams _published
    (cl-letf (((symbol-function 'agent-repl--runtime-prepare)
               (lambda (&rest _) :pending)))
      (agent-repl-runtime-restart-dispatch nil 300.0)
      (should-error (agent-repl-runtime-restart-dispatch nil 300.0)
                    :type 'error))))

(ert-deftest agent-repl-services-test-dispatch-ignores-a-late-timeout-after-completion ()
  "A timeout arriving after completion cannot republish or restore twice."
  (let ((agent-repl-frontend-health-timeout 10.0)
        (published nil) fire finish)
    (setq agent-repl--runtime-restart-dispatch nil)
    (cl-letf (((symbol-function 'agent-repl--runtime-restart-write-result)
               (lambda (path text) (push (cons path text) published)))
              ((symbol-function 'agent-repl--shim-services-run-timer)
               (lambda (_seconds callback) (setq fire callback) 'timer))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'agent-repl--runtime-prepare)
               (lambda (_rebind on-success _on-failure &optional _stop-shims)
                 (setq finish on-success)
                 :pending)))
      (agent-repl-runtime-restart-dispatch nil 300.0)
      (funcall finish)
      (should (equal "complete"
                     (agent-repl-services-test--artifact-status (car published))))
      (let ((count (length published)))
        (funcall fire)
        (should (equal count (length published)))
        (should (= agent-repl-frontend-health-timeout 10.0))))))

(provide 'test-services)

;;; test-services.el ends here
