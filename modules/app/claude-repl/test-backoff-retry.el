;;; test-backoff-retry.el --- Tests for backoff-retry.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the exponential-backoff retry logic defined in
;; backoff-retry.el and its wiring into:
;;   - claude-repl--on-stop-failure-event (sentinel.el)
;;   - claude-repl--mark-claude-done       (session.el)
;;   - claude-repl--mark-ws-thinking       (input.el)

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: delay formula ----

(ert-deftest claude-repl-test-backoff-retry-delay-attempt-1-is-base ()
  "Attempt 1 returns base * 60 seconds (1 minute by default)."
  (let ((claude-repl-backoff-retry-base-minutes 1)
        (claude-repl-backoff-retry-max-minutes  5))
    (should (= 60.0 (claude-repl--backoff-retry-delay-seconds 1)))))

(ert-deftest claude-repl-test-backoff-retry-delay-attempt-2-doubles ()
  "Attempt 2 doubles the base delay (2 minutes by default)."
  (let ((claude-repl-backoff-retry-base-minutes 1)
        (claude-repl-backoff-retry-max-minutes  5))
    (should (= 120.0 (claude-repl--backoff-retry-delay-seconds 2)))))

(ert-deftest claude-repl-test-backoff-retry-delay-attempt-3-quadruples ()
  "Attempt 3 quadruples the base delay (4 minutes by default)."
  (let ((claude-repl-backoff-retry-base-minutes 1)
        (claude-repl-backoff-retry-max-minutes  5))
    (should (= 240.0 (claude-repl--backoff-retry-delay-seconds 3)))))

(ert-deftest claude-repl-test-backoff-retry-delay-clamps-at-max ()
  "Attempt 4 clamps at max (5 minutes by default) — exponential would be 8."
  (let ((claude-repl-backoff-retry-base-minutes 1)
        (claude-repl-backoff-retry-max-minutes  5))
    (should (= 300.0 (claude-repl--backoff-retry-delay-seconds 4)))))

(ert-deftest claude-repl-test-backoff-retry-delay-last-attempt-is-max ()
  "Attempt 10 remains clamped at max (5 minutes)."
  (let ((claude-repl-backoff-retry-base-minutes 1)
        (claude-repl-backoff-retry-max-minutes  5))
    (should (= 300.0 (claude-repl--backoff-retry-delay-seconds 10)))))

(ert-deftest claude-repl-test-backoff-retry-delay-respects-custom-base ()
  "A custom base scales all delays before the cap kicks in."
  (let ((claude-repl-backoff-retry-base-minutes 2)
        (claude-repl-backoff-retry-max-minutes  10))
    (should (= 120.0 (claude-repl--backoff-retry-delay-seconds 1)))
    (should (= 240.0 (claude-repl--backoff-retry-delay-seconds 2)))
    (should (= 480.0 (claude-repl--backoff-retry-delay-seconds 3)))
    (should (= 600.0 (claude-repl--backoff-retry-delay-seconds 4)))))

;;;; ---- Tests: schedule ----

(ert-deftest claude-repl-test-backoff-retry-schedule-increments-count ()
  "Calling schedule increments `:backoff-retry-count' by 1."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) 'fake-timer)))
      (claude-repl--backoff-retry-schedule "ws1")
      (should (= 1 (claude-repl--ws-get "ws1" :backoff-retry-count)))
      (claude-repl--backoff-retry-schedule "ws1")
      (should (= 2 (claude-repl--ws-get "ws1" :backoff-retry-count))))))

(ert-deftest claude-repl-test-backoff-retry-schedule-stores-timer ()
  "Calling schedule stores the timer object in `:backoff-retry-timer'."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) 'fake-timer)))
      (claude-repl--backoff-retry-schedule "ws1")
      (should (eq 'fake-timer (claude-repl--ws-get "ws1" :backoff-retry-timer))))))

(ert-deftest claude-repl-test-backoff-retry-schedule-uses-correct-delay ()
  "Schedule passes the delay matching the new (incremented) attempt count."
  (claude-repl-test--with-clean-state
    (let ((captured-delay nil)
          (claude-repl-backoff-retry-base-minutes 1)
          (claude-repl-backoff-retry-max-minutes  5))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (delay _repeat _fn &rest _args)
                   (setq captured-delay delay)
                   'fake-timer)))
        (claude-repl--backoff-retry-schedule "ws1")
        (should (= 60.0 captured-delay))
        (claude-repl--backoff-retry-schedule "ws1")
        (should (= 120.0 captured-delay))
        (claude-repl--backoff-retry-schedule "ws1")
        (should (= 240.0 captured-delay))
        (claude-repl--backoff-retry-schedule "ws1")
        (should (= 300.0 captured-delay))))))

(ert-deftest claude-repl-test-backoff-retry-schedule-does-not-exceed-max ()
  "Once `:backoff-retry-count' reaches max-attempts, schedule is a no-op.
Neither the count is incremented further nor a new timer installed."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-backoff-retry-max-attempts 3)
          (run-at-time-calls 0))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _)
                   (cl-incf run-at-time-calls)
                   'fake-timer)))
        (claude-repl--backoff-retry-schedule "ws1") ;; 1
        (claude-repl--backoff-retry-schedule "ws1") ;; 2
        (claude-repl--backoff-retry-schedule "ws1") ;; 3
        (should (= 3 run-at-time-calls))
        (should (= 3 (claude-repl--ws-get "ws1" :backoff-retry-count)))
        ;; 4th call exhausts budget: no timer, no increment.
        (claude-repl--backoff-retry-schedule "ws1")
        (should (= 3 run-at-time-calls))
        (should (= 3 (claude-repl--ws-get "ws1" :backoff-retry-count)))
        (should-not (claude-repl--ws-get "ws1" :backoff-retry-timer))))))

(ert-deftest claude-repl-test-backoff-retry-schedule-cancels-prior-timer ()
  "Schedule defensively cancels any pre-existing pending timer first."
  (claude-repl-test--with-clean-state
    (let ((cancelled nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) 'new-timer))
                ((symbol-function 'timerp)
                 (lambda (obj) (eq obj 'old-timer)))
                ((symbol-function 'cancel-timer)
                 (lambda (timer)
                   (when (eq timer 'old-timer)
                     (setq cancelled t)))))
        (claude-repl--ws-put "ws1" :backoff-retry-timer 'old-timer)
        (claude-repl--backoff-retry-schedule "ws1")
        (should cancelled)
        (should (eq 'new-timer (claude-repl--ws-get "ws1" :backoff-retry-timer)))))))

;;;; ---- Tests: cancel-timer ----

(ert-deftest claude-repl-test-backoff-retry-cancel-timer-clears-slot ()
  "cancel-timer removes the pending timer object from the plist."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'timerp) (lambda (obj) (eq obj 'fake-timer)))
              ((symbol-function 'cancel-timer) (lambda (_) nil)))
      (claude-repl--ws-put "ws1" :backoff-retry-timer 'fake-timer)
      (claude-repl--backoff-retry-cancel-timer "ws1")
      (should-not (claude-repl--ws-get "ws1" :backoff-retry-timer)))))

(ert-deftest claude-repl-test-backoff-retry-cancel-timer-preserves-count ()
  "cancel-timer does NOT reset `:backoff-retry-count'."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'timerp) (lambda (obj) (eq obj 'fake-timer)))
              ((symbol-function 'cancel-timer) (lambda (_) nil)))
      (claude-repl--ws-put "ws1" :backoff-retry-count 3)
      (claude-repl--ws-put "ws1" :backoff-retry-timer 'fake-timer)
      (claude-repl--backoff-retry-cancel-timer "ws1")
      (should (= 3 (claude-repl--ws-get "ws1" :backoff-retry-count))))))

(ert-deftest claude-repl-test-backoff-retry-cancel-timer-no-pending-is-noop ()
  "cancel-timer is safe to call when no timer is pending."
  (claude-repl-test--with-clean-state
    (claude-repl--backoff-retry-cancel-timer "ws1")
    (should-not (claude-repl--ws-get "ws1" :backoff-retry-timer))))

;;;; ---- Tests: reset ----

(ert-deftest claude-repl-test-backoff-retry-reset-zeroes-count ()
  "reset sets `:backoff-retry-count' to 0."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :backoff-retry-count 7)
    (claude-repl--backoff-retry-reset "ws1")
    (should (= 0 (claude-repl--ws-get "ws1" :backoff-retry-count)))))

(ert-deftest claude-repl-test-backoff-retry-reset-cancels-pending-timer ()
  "reset cancels any pending timer in addition to zeroing the count."
  (claude-repl-test--with-clean-state
    (let ((cancelled nil))
      (cl-letf (((symbol-function 'timerp) (lambda (obj) (eq obj 'fake-timer)))
                ((symbol-function 'cancel-timer)
                 (lambda (_) (setq cancelled t))))
        (claude-repl--ws-put "ws1" :backoff-retry-count 2)
        (claude-repl--ws-put "ws1" :backoff-retry-timer 'fake-timer)
        (claude-repl--backoff-retry-reset "ws1")
        (should cancelled)
        (should-not (claude-repl--ws-get "ws1" :backoff-retry-timer))
        (should (= 0 (claude-repl--ws-get "ws1" :backoff-retry-count)))))))

;;;; ---- Tests: fire ----

(ert-deftest claude-repl-test-backoff-retry-fire-sends-configured-prompt ()
  "Fire sends `claude-repl-backoff-retry-prompt' via send-input-to-vterm
when the workspace is registered, vterm is live, and state is :stop-failed."
  (claude-repl-test--with-clean-state
    (let ((sent-input nil)
          (sent-buffer nil))
      (cl-letf (((symbol-function 'claude-repl--send-input-to-vterm)
                 (lambda (buf input &rest _)
                   (setq sent-buffer buf
                         sent-input  input))))
        (claude-repl-test--with-temp-buffer "fake-vterm"
          (puthash "ws1"
                   (list :project-dir "/tmp/x"
                         :vterm-buffer (current-buffer)
                         :claude-state :stop-failed)
                   claude-repl--workspaces)
          (let ((claude-repl-backoff-retry-prompt "try again"))
            (claude-repl--backoff-retry-fire "ws1"))
          (should (equal "try again" sent-input))
          (should (eq (current-buffer) sent-buffer)))))))

(ert-deftest claude-repl-test-backoff-retry-fire-clears-timer-slot ()
  "Fire clears `:backoff-retry-timer' (the timer that called us is consumed)."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--send-input-to-vterm)
               (lambda (&rest _) nil)))
      (claude-repl-test--with-temp-buffer "fake-vterm"
        (puthash "ws1"
                 (list :project-dir "/tmp/x"
                       :vterm-buffer (current-buffer)
                       :claude-state :stop-failed
                       :backoff-retry-timer 'fake-timer)
                 claude-repl--workspaces)
        (claude-repl--backoff-retry-fire "ws1")
        (should-not (claude-repl--ws-get "ws1" :backoff-retry-timer))))))

(ert-deftest claude-repl-test-backoff-retry-fire-aborts-when-state-not-stop-failed ()
  "Fire is a no-op (no vterm send) when state is no longer :stop-failed.
Handles user manual intervention between schedule and fire."
  (claude-repl-test--with-clean-state
    (let ((send-called nil))
      (cl-letf (((symbol-function 'claude-repl--send-input-to-vterm)
                 (lambda (&rest _) (setq send-called t))))
        (claude-repl-test--with-temp-buffer "fake-vterm"
          (puthash "ws1"
                   (list :project-dir "/tmp/x"
                         :vterm-buffer (current-buffer)
                         :claude-state :thinking)
                   claude-repl--workspaces)
          (claude-repl--backoff-retry-fire "ws1")
          (should-not send-called))))))

(ert-deftest claude-repl-test-backoff-retry-fire-aborts-when-vterm-dead ()
  "Fire is a no-op when the workspace's vterm buffer is null."
  (claude-repl-test--with-clean-state
    (let ((send-called nil))
      (cl-letf (((symbol-function 'claude-repl--send-input-to-vterm)
                 (lambda (&rest _) (setq send-called t))))
        (puthash "ws1"
                 (list :project-dir "/tmp/x"
                       :vterm-buffer nil
                       :claude-state :stop-failed)
                 claude-repl--workspaces)
        (claude-repl--backoff-retry-fire "ws1")
        (should-not send-called)))))

(ert-deftest claude-repl-test-backoff-retry-fire-aborts-when-ws-not-registered ()
  "Fire is a no-op when WS is no longer in `claude-repl--workspaces'."
  (claude-repl-test--with-clean-state
    (let ((send-called nil))
      (cl-letf (((symbol-function 'claude-repl--send-input-to-vterm)
                 (lambda (&rest _) (setq send-called t))))
        ;; No puthash — workspace is absent.
        (claude-repl--backoff-retry-fire "ws-ghost")
        (should-not send-called)))))

;;;; ---- Tests: integration with on-stop-failure-event ----

(ert-deftest claude-repl-test-on-stop-failure-event-schedules-retry ()
  "on-stop-failure-event arms a backoff retry on top of state transition."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) 'fake-timer)))
      (claude-repl--on-stop-failure-event "ws1" "/some/dir")
      (should (eq :stop-failed (claude-repl--ws-claude-state "ws1")))
      (should (= 1 (claude-repl--ws-get "ws1" :backoff-retry-count)))
      (should (eq 'fake-timer (claude-repl--ws-get "ws1" :backoff-retry-timer))))))

(ert-deftest claude-repl-test-on-stop-failure-event-consecutive-grows-count ()
  "Repeated stop-failure events escalate the count (consecutive failures)."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) 'fake-timer)))
      (claude-repl--on-stop-failure-event "ws1" "/some/dir")
      (claude-repl--on-stop-failure-event "ws1" "/some/dir")
      (claude-repl--on-stop-failure-event "ws1" "/some/dir")
      (should (= 3 (claude-repl--ws-get "ws1" :backoff-retry-count))))))

;;;; ---- Tests: integration with mark-claude-done ----

(ert-deftest claude-repl-test-mark-claude-done-resets-backoff-count ()
  "Reaching :done resets the consecutive-failure tally to 0."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--current-ws-p) (lambda (_) nil)))
      (puthash "ws1" (list :project-dir "/tmp/x") claude-repl--workspaces)
      (claude-repl--ws-put "ws1" :backoff-retry-count 4)
      (claude-repl--mark-claude-done "ws1")
      (should (= 0 (claude-repl--ws-get "ws1" :backoff-retry-count))))))

(ert-deftest claude-repl-test-mark-claude-done-cancels-pending-retry ()
  "Reaching :done cancels any pending backoff-retry timer."
  (claude-repl-test--with-clean-state
    (let ((cancelled nil))
      (cl-letf (((symbol-function 'claude-repl--current-ws-p) (lambda (_) nil))
                ((symbol-function 'timerp) (lambda (obj) (eq obj 'fake-timer)))
                ((symbol-function 'cancel-timer)
                 (lambda (_) (setq cancelled t))))
        (puthash "ws1" (list :project-dir "/tmp/x") claude-repl--workspaces)
        (claude-repl--ws-put "ws1" :backoff-retry-timer 'fake-timer)
        (claude-repl--mark-claude-done "ws1")
        (should cancelled)
        (should-not (claude-repl--ws-get "ws1" :backoff-retry-timer))))))

;;;; ---- Tests: integration with mark-ws-thinking ----

(ert-deftest claude-repl-test-mark-ws-thinking-cancels-pending-retry ()
  "Transitioning to :thinking cancels any pending backoff-retry timer.
Handles the case where a user manually prompts while a retry is armed
\(the retry must not fire later and clobber the user's intent)."
  (claude-repl-test--with-clean-state
    (let ((cancelled nil))
      (cl-letf (((symbol-function 'timerp) (lambda (obj) (eq obj 'fake-timer)))
                ((symbol-function 'cancel-timer)
                 (lambda (_) (setq cancelled t))))
        (claude-repl--ws-put "ws1" :backoff-retry-timer 'fake-timer)
        (claude-repl--mark-ws-thinking "ws1")
        (should cancelled)
        (should-not (claude-repl--ws-get "ws1" :backoff-retry-timer))))))

(ert-deftest claude-repl-test-mark-ws-thinking-preserves-backoff-count ()
  "Transitioning to :thinking leaves `:backoff-retry-count' intact.
Only :done resets the consecutive-failure tally."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :backoff-retry-count 5)
    (claude-repl--mark-ws-thinking "ws1")
    (should (= 5 (claude-repl--ws-get "ws1" :backoff-retry-count)))))

;;; test-backoff-retry.el ends here
