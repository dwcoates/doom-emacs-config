;;; backoff-retry.el --- Exponential backoff retry for :stop-failed -*- lexical-binding: t; -*-

;;; Commentary:

;; When the StopFailure hook fires (claude-state = :stop-failed, e.g. an
;; internal server / API error), this module schedules a "try again"
;; prompt after a backoff delay.  Repeated consecutive failures escalate
;; the delay exponentially up to a configurable cap; reaching :done
;; resets the counter; any non-:stop-failed transition cancels the
;; pending timer (so a manual user prompt doesn't get clobbered later).
;;
;; Per-workspace plist keys (owned here):
;;
;;   :backoff-retry-count — int, count of consecutive retries scheduled
;;                          since the last :done (0 = none pending/sent)
;;   :backoff-retry-timer — timer object for the pending retry, or nil
;;
;; Wiring (added at the relevant call sites in other modules):
;;
;;   - claude-repl--on-stop-failure-event → --backoff-retry-schedule
;;   - claude-repl--mark-claude-done       → --backoff-retry-reset
;;   - claude-repl--mark-ws-thinking       → --backoff-retry-cancel-timer

;;; Code:

(defcustom claude-repl-backoff-retry-prompt "try again"
  "Prompt sent to Claude on each backoff retry attempt."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-backoff-retry-max-attempts 10
  "Maximum number of consecutive backoff retries before giving up.
After this many retries have been scheduled and still result in
:stop-failed, the workspace is left in :stop-failed and no further
retries are attempted (the user must intervene)."
  :type 'integer
  :group 'claude-repl)

(defcustom claude-repl-backoff-retry-base-minutes 1
  "Minimum delay (in minutes) before the first backoff retry."
  :type 'number
  :group 'claude-repl)

(defcustom claude-repl-backoff-retry-max-minutes 5
  "Maximum delay (in minutes) between backoff retries.
Delays grow exponentially from `claude-repl-backoff-retry-base-minutes'
and clamp at this value."
  :type 'number
  :group 'claude-repl)

(defun claude-repl--backoff-retry-delay-seconds (attempt)
  "Return the delay (in seconds) before retry number ATTEMPT (1-indexed).
Computes `min(max, base * 2^(attempt-1))' minutes converted to seconds:
attempt 1 → base, attempt 2 → 2*base, attempt 3 → 4*base, etc.,
clamped at `claude-repl-backoff-retry-max-minutes'."
  (let* ((base   claude-repl-backoff-retry-base-minutes)
         (cap    claude-repl-backoff-retry-max-minutes)
         (factor (expt 2 (max 0 (1- attempt))))
         (mins   (min cap (* base factor))))
    (* 60.0 mins)))

(defun claude-repl--backoff-retry-cancel-timer (ws)
  "Cancel any pending backoff-retry timer for WS.
Leaves `:backoff-retry-count' intact — only the pending schedule is
cleared, not the historical tally that governs the next backoff delay."
  (let ((timer (claude-repl--ws-get ws :backoff-retry-timer)))
    (when (timerp timer)
      (claude-repl--log ws "backoff-retry: cancelling pending timer for ws=%s" ws)
      (cancel-timer timer))
    (claude-repl--ws-put ws :backoff-retry-timer nil)))

(defun claude-repl--backoff-retry-reset (ws)
  "Cancel WS's pending backoff-retry timer and reset its count to 0.
Called when WS reaches `:done' — i.e. Claude completed a turn
successfully, so the consecutive-failure tally should start fresh."
  (claude-repl--backoff-retry-cancel-timer ws)
  (let ((prev (or (claude-repl--ws-get ws :backoff-retry-count) 0)))
    (when (> prev 0)
      (claude-repl--log ws "backoff-retry: resetting count for ws=%s (was %d)" ws prev)))
  (claude-repl--ws-put ws :backoff-retry-count 0))

(defun claude-repl--backoff-retry-fire (ws)
  "Fire the pending retry for WS: send `claude-repl-backoff-retry-prompt'.
Guards: workspace must still be registered, vterm buffer must be live,
and `:claude-state' must still be `:stop-failed' (so a user manual
intervention between schedule and fire is honored even if the
cancel-timer call didn't beat the firing).  Always clears
`:backoff-retry-timer' since the timer that invoked us is now consumed."
  (let ((vterm-buf (claude-repl--ws-get ws :vterm-buffer))
        (state (claude-repl--ws-claude-state ws)))
    (claude-repl--ws-put ws :backoff-retry-timer nil)
    (cond
     ((not (gethash ws claude-repl--workspaces))
      (claude-repl--log ws "backoff-retry: fire aborted ws=%s — not registered" ws))
     ((not (eq state :stop-failed))
      (claude-repl--log ws "backoff-retry: fire aborted ws=%s — state=%s (no longer :stop-failed)" ws state))
     ((or (null vterm-buf) (not (buffer-live-p vterm-buf)))
      (claude-repl--log ws "backoff-retry: fire aborted ws=%s — vterm buffer null/dead" ws))
     (t
      (claude-repl--log ws "backoff-retry: firing retry for ws=%s (attempt=%d) prompt=%S"
                        ws
                        (or (claude-repl--ws-get ws :backoff-retry-count) 0)
                        claude-repl-backoff-retry-prompt)
      (claude-repl--send-input-to-vterm vterm-buf claude-repl-backoff-retry-prompt)))))

(defun claude-repl--backoff-retry-schedule (ws)
  "Schedule the next backoff retry for WS after a `:stop-failed' event.
Increments `:backoff-retry-count' and arms a one-shot timer that fires
`claude-repl--backoff-retry-fire' after the computed delay.  If the
next count would exceed `claude-repl-backoff-retry-max-attempts',
logs and leaves WS in `:stop-failed' permanently (timed out) —
no further retries until a successful `:done' resets the count.

Pre-cancels any pre-existing timer defensively (should be nil at this
point since a fresh `:stop-failed' implies the prior retry's timer
has already fired or been cancelled, but the defense keeps the
invariant simple)."
  (claude-repl--backoff-retry-cancel-timer ws)
  (let* ((prev (or (claude-repl--ws-get ws :backoff-retry-count) 0))
         (next (1+ prev)))
    (cond
     ((> next claude-repl-backoff-retry-max-attempts)
      (claude-repl--log ws "backoff-retry: ws=%s exhausted after %d attempts, giving up"
                        ws prev))
     (t
      (let ((delay (claude-repl--backoff-retry-delay-seconds next)))
        (claude-repl--ws-put ws :backoff-retry-count next)
        (claude-repl--log ws "backoff-retry: ws=%s scheduling attempt %d in %.0fs"
                          ws next delay)
        (claude-repl--ws-put ws :backoff-retry-timer
                             (run-at-time delay nil
                                          #'claude-repl--backoff-retry-fire
                                          ws)))))))

(provide 'claude-repl-backoff-retry)
;;; backoff-retry.el ends here
