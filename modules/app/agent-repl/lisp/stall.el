;;; stall.el --- the editor measures how long its command loop stalls -*- lexical-binding: t; -*-

;;; Commentary:

;; A STALL IS THE COMMAND LOOP NOT RUNNING when it should.  Every keystroke,
;; every redisplay and every process filter waits on the one loop, so a
;; stalled loop is the editor freezing in front of the person using it.
;;
;; THE SAMPLER IS A STEADY TIMER (`:stall-sampler', every
;; `agent-repl-stall-interval' seconds).  The loop runs a due timer as soon
;; as it is free, so the time between two runs of it, less the interval, is
;; how long the loop could not run it: the stall.  A stall at least
;; `agent-repl-stall-floor' long (the vocabulary's detection_floor_ms of
;; `editor_stall', proto/vocab/latencies.json) is reported through
;; latency-report.el; anything shorter is a healthy loop and sends nothing.
;; A stall that begins between two runs is measured from the earlier run's
;; due instant onward, so a measured stall is at most one interval short of
;; the real one, never longer.
;;
;; A GAP PAST `agent-repl-stall-unclassified' IS NOT A STALL.  The wall clock
;; keeps running while the host sleeps, and the timer runs again only when it
;; wakes; Emacs has no clock that stops with the host to tell the two apart.
;; No editor work holds the loop that long, so such a gap is recorded at INFO
;; and reported as nothing.
;;
;; The timer is owned here and armed through the timer registry
;; (`agent-repl--register-timer'), so a hot reload of this file replaces it
;; rather than adding a second, and the heartbeat assertion re-arms it when a
;; reload of core.el stranded it.

;;; Code:

(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--register-timer "core" (key timer))
(declare-function agent-repl-latency-report-editor "latency-report" (kind seconds))

(defconst agent-repl-stall--central
  '(:agent-repl-central "the editor's command loop spans every workspace")
  "The log scope of every stall record.")

(defconst agent-repl-stall-interval 0.1
  "Seconds between two runs of the stall sampler.")

(defconst agent-repl-stall-floor 0.05
  "The shortest stall reported, in seconds: `editor_stall''s detection floor.")

(defconst agent-repl-stall-unclassified 60.0
  "The longest gap measured as a stall, in seconds; a longer one is a host sleep.")

(defvar agent-repl-stall--last nil
  "The `float-time' of the sampler's previous run, or nil before its first.")

(defun agent-repl-stall--now ()
  "External-boundary wrapper: the wall clock, in float seconds."
  (float-time))

(defun agent-repl-stall-sample ()
  "One run of the sampler: measure the gap since the last run, report a stall."
  (let* ((now (agent-repl-stall--now))
         (last agent-repl-stall--last)
         (stall (and last (- now last agent-repl-stall-interval))))
    (setq agent-repl-stall--last now)
    (cond
     ((or (null stall) (< stall agent-repl-stall-floor)))
     ((> stall agent-repl-stall-unclassified)
      (agent-repl--info agent-repl-stall--central
                        "elisp.stall.unclassified gap=%.3f reason=longer-than-any-stall"
                        (+ stall agent-repl-stall-interval)))
     (t
      (agent-repl-latency-report-editor :stall stall)))))

(defun agent-repl--arm-stall-sampler ()
  "Arm the stall sampler under `:stall-sampler', replacing any held timer.
The first run after arming measures nothing: the gap before it is the
arming's, not the loop's."
  (setq agent-repl-stall--last nil)
  (agent-repl--register-timer
   :stall-sampler
   (run-at-time agent-repl-stall-interval agent-repl-stall-interval
                #'agent-repl-stall-sample)))

(agent-repl--arm-stall-sampler)

(provide 'stall)

;;; stall.el ends here
