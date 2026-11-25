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
;; latency-report.el with the work that held the loop longest meanwhile
;; (stall-culprit.el), and recorded at INFO by that work's name; anything
;; shorter is a healthy loop and sends nothing.
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
;; INPUT CAN WAIT WITHOUT A STALL.  Process filters and timers run between
;; the sampler's runs, so a burst of them (a deploy's handover pushes the
;; whole roster over a hundred times) keeps the loop busy without ever
;; holding it a whole interval: the sampler runs on time, yet keystrokes sit
;; unread until the burst ends.  So each run also asks whether input is
;; pending, and input found pending across at least two runs is reported as
;; an input wait (`editor_input_wait'), from the first run that found it to
;; the first that found none.
;;
;; The timer is owned here and armed through the timer registry
;; (`agent-repl--register-timer'), so a hot reload of this file replaces it
;; rather than adding a second, and the heartbeat assertion re-arms it when a
;; reload of core.el stranded it.
;;
;; BREAKING A STALL WITH SIGUSR2 MUST NOT LEAVE THE DEBUGGER ARMED.  SIGUSR2
;; is `debug-on-event', the documented way to break into a stalled editor,
;; and Emacs's signal handler answers it by setting `debug-on-quit' from C
;; and never clearing it.  Every later quit then enters the debugger: an
;; escape out of any minibuffer prompt (2026-10-26: escaping a one-shot's
;; prompt) lands in "Entering debugger...".  So `debug-on-quit' only ever
;; holds a value Lisp left it: a variable watcher records the value every
;; Lisp `set', `let' and unbinding leaves (a write from C fires no watcher),
;; and entering the debugger puts that value back when the live one
;; differs.  The signal's own debugger is the one entered, so the stall is
;; still broken; and nothing between the signal and that entry can bind
;; `debug-on-quit' and take the C value for its own, as a `keyboard-quit'
;; typed in the debugger would by a command's end.

;;; Code:

;; Every log call is a log-site macro (log-site.el), needed where this
;; file is expanded; config.el loads it first.
(require 'log-site)

(declare-function agent-repl--now "core" ())
(declare-function agent-repl--register-timer "core" (key timer))
(declare-function agent-repl-latency-report-editor "latency-report" (kind seconds))
(declare-function agent-repl-latency-report-stall "latency-report" (seconds culprit))
(declare-function agent-repl-stall-culprit-reset "stall-culprit" (now))
(declare-function agent-repl-stall-culprit-take "stall-culprit" (now stall))

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

(defvar agent-repl-stall--input nil
  "The input wait under way, `(SINCE . RUNS)', or nil when no input is pending.
SINCE is the `agent-repl--now' of the first run that found input pending,
RUNS how many runs in a row have found it.")

(defun agent-repl-stall--input-pending-p ()
  "Return non-nil when typed input is waiting to be read."
  (input-pending-p))

(defun agent-repl-stall--sample-input (now)
  "Track typed input waiting at NOW, reporting a wait once it is read.
A wait seen by one run alone is shorter than an interval and is no wait;
one seen by two runs or more is reported from its first run to NOW."
  (let ((wait agent-repl-stall--input))
    (cond
     ((agent-repl-stall--input-pending-p)
      (setq agent-repl-stall--input (cons (or (car wait) now) (1+ (or (cdr wait) 0)))))
     (wait
      (setq agent-repl-stall--input nil)
      (when (>= (cdr wait) 2)
        (agent-repl-latency-report-editor :input-wait (- now (car wait))))))))

(defun agent-repl-stall-sample ()
  "One run of the sampler: report a stall since the last run, then any input wait.
Every run closes the culprit window (stall-culprit.el) it measured over."
  (let* ((now (agent-repl--now))
         (last agent-repl-stall--last)
         (stall (and last (- now last agent-repl-stall-interval))))
    (setq agent-repl-stall--last now)
    (cond
     ((or (null stall) (< stall agent-repl-stall-floor))
      (agent-repl-stall-culprit-reset now))
     ((> stall agent-repl-stall-unclassified)
      ;; The host slept: whatever input was pending before it says nothing
      ;; about the loop.
      (setq agent-repl-stall--input nil)
      (agent-repl-stall-culprit-reset now)
      (agent-repl--info agent-repl-stall--central
                        "elisp.stall.unclassified gap={gap} reason=longer-than-any-stall"
                        :gap (format "%.3f" (+ stall agent-repl-stall-interval))))
     (t
      (agent-repl-stall--report now stall)))
    (agent-repl-stall--sample-input now)))

(defun agent-repl-stall--report (now stall)
  "Report a STALL seconds long ending at NOW, with the work that held the loop.
The record names the culprit by its own function or rpc method, whatever
its namespace; the daemon's metric folds a foreign function into `other'."
  (let* ((taken (agent-repl-stall-culprit-take now stall))
         (culprit (plist-get taken :culprit)))
    (agent-repl--info agent-repl-stall--central
                      "elisp.stall.culprit seconds={seconds} source={source} function={function} ran={ran} timed={timed} gc={gc}"
                      :seconds (format "%.3f" stall)
                      :source (symbol-name (plist-get culprit :source))
                      :function (format "%s" (or (plist-get culprit :function) (plist-get culprit :method) ""))
                      :ran (format "%.3f" (plist-get taken :ran))
                      :timed (format "%.3f" (plist-get taken :timed))
                      :gc (format "%.3f" (plist-get taken :gc)))
    (agent-repl-latency-report-stall stall culprit)))

(defun agent-repl--arm-stall-sampler ()
  "Arm the stall sampler under `:stall-sampler', replacing any held timer.
The first run after arming measures nothing: the gap before it is the
arming's, not the loop's."
  (setq agent-repl-stall--last nil
        agent-repl-stall--input nil)
  (agent-repl-stall-culprit-reset (agent-repl--now))
  (agent-repl--register-timer
   :stall-sampler
   (run-at-time agent-repl-stall-interval agent-repl-stall-interval
                #'agent-repl-stall-sample)))

(agent-repl--arm-stall-sampler)

;; `debug' is the debugger every quit and every signal enters (`debugger').
(declare-function debug "debug" (&rest args))

(defvar agent-repl-stall--debug-on-quit-lisp nil
  "The `debug-on-quit' Lisp last left, as its variable watcher saw it.
No configuration of this editor sets it, so it starts at Emacs's default,
nil: a value found at load is not trusted, since a SIGUSR2 before a hot
reload is exactly what leaves it wrong.")

(defun agent-repl-stall--note-debug-on-quit (_symbol value operation where)
  "Record VALUE as the `debug-on-quit' Lisp left by OPERATION.
The variable watcher of `debug-on-quit'.  A `set', a `let' and an `unlet'
each report the value they leave in place; WHERE is the buffer of a
buffer-local one, which leaves the global value as it was."
  (when (and (memq operation '(set let unlet)) (null where))
    (setq agent-repl-stall--debug-on-quit-lisp value)))

(defun agent-repl-stall--restore-debug-on-quit (&rest _)
  "Put back the `debug-on-quit' Lisp left when a write from C changed it.
Runs as the debugger is entered (before `debug'), and once at load."
  (unless (eq debug-on-quit agent-repl-stall--debug-on-quit-lisp)
    (agent-repl--info agent-repl-stall--central
                      "elisp.stall.debug-on-quit-restored from={from} to={to} reason=set-outside-lisp event={event}"
                      :from (prin1-to-string debug-on-quit) :to (prin1-to-string agent-repl-stall--debug-on-quit-lisp) :event (prin1-to-string debug-on-event))
    (setq debug-on-quit agent-repl-stall--debug-on-quit-lisp)))

(add-variable-watcher 'debug-on-quit #'agent-repl-stall--note-debug-on-quit)
(advice-add 'debug :before #'agent-repl-stall--restore-debug-on-quit)
(agent-repl-stall--restore-debug-on-quit)

(provide 'stall)

;;; stall.el ends here
