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
              ((symbol-function 'agent-repl--frontend-all-turn-active-session-ids)
               (lambda () '("s_busy")))
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

(provide 'test-services)

;;; test-services.el ends here
